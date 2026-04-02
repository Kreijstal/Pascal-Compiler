#define _GNU_SOURCE
/*
 * KGPC - Kreijstal Gwinn Pascal Compiler (cparser integration)
 */

#include <assert.h>
#include <libgen.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif
#ifndef _WIN32
#include <unistd.h>
#else
#include <direct.h>
#endif

/* Portable setenv/unsetenv for Windows */
#ifdef _WIN32
static int setenv(const char *name, const char *value, int overwrite)
{
    (void)overwrite;
    return _putenv_s(name, value);
}
static int unsetenv(const char *name)
{
    return _putenv_s(name, "");
}
#endif

#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <time.h>
#include <sys/stat.h>
#include <errno.h>
#ifndef _WIN32
#include <malloc.h>
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#include "parser.h"
#include "combinators.h"
#include "pascal_parser.h"
#include "pascal_declaration.h"

#include "flags.h"
#include "unit_registry.h"
#include "Parser/ParseTree/tree.h"
#include "Parser/ParseTree/from_cparser.h"
#include "Parser/pascal_frontend.h"
#include "Parser/SemanticCheck/SemCheck.h"
#include "CodeGenerator/Intel_x86-64/codegen.h"
#include "stacktrace.h"
#include "unit_paths.h"
#include "arena.h"
#include "../common/file_lock.h"
#include "identifier_utils.h"
#include "compilation_context.h"

#ifdef _WIN32
#include <windows.h>
#include <io.h>

/* Cached getenv() — defined in SemCheck.c */
extern const char *kgpc_getenv(const char *name);
#ifndef R_OK
#define R_OK 4
#endif
#define access _access
#endif

extern ast_t *ast_nil;

/* Legacy globals referenced in other components */
Tree_t *parse_tree = NULL;
int num_args_alloced = 0;
int line_num = 1;
int col_num = 1;
char *file_to_parse = NULL;
char *preprocessed_source = NULL;
size_t preprocessed_length = 0;
char *preprocessed_path = NULL;
static UnitSearchPaths g_unit_paths;
static bool g_skip_stdlib = false;
static int g_requires_gmp = 0;
static int g_emit_link_args = 0;
static int g_ast_cache_explicit = 0;
static const char *g_codegen_cache_dir = NULL;
static char g_codegen_cache_obj_path[4096]; /* path to the cached .o used/created */
static char g_codegen_cache_asm_path[4096]; /* path to the cache .s (unit functions only) */
static int g_codegen_cache_hit = 0; /* 1 if cache hit, 0 if miss */
static int g_codegen_cache_forced_function_sections = 0;
static int g_codegen_cache_forced_skip_unit_codegen = 0;
static int g_saved_argc = 0;
static char **g_saved_argv = NULL;
static int g_batch_mode = 0;
static int g_batch_max_parallel = 0; /* 0 = auto (nproc) */

/* Pre-loaded unit store: units loaded in the batch-mode parent process.
 * After fork(), children inherit these via COW and skip parse_pascal_file().
 * In normal (non-batch) mode this store is empty. */
#define MAX_PRELOADED_UNITS 16
typedef struct
{
    char *normalized_name; /* lowercase unit name */
    Tree_t *tree;          /* the parsed + converted Tree_t */
    char *path;            /* original source path */
    char *pp_source;       /* preprocessed source (for error reporting) */
    size_t pp_length;
} PreloadedUnit;
static PreloadedUnit g_preloaded_units[MAX_PRELOADED_UNITS];
static int g_preloaded_unit_count = 0;

/* Look up a pre-loaded unit by normalized name.  Returns NULL if not found. */
static PreloadedUnit *find_preloaded_unit(const char *normalized_name)
{
    for (int i = 0; i < g_preloaded_unit_count; i++)
    {
        if (strcmp(g_preloaded_units[i].normalized_name, normalized_name) == 0)
            return &g_preloaded_units[i];
    }
    return NULL;
}

/* ---- Compilation context (Phase 2: central compilation state) ---- */
static CompilationContext g_comp_ctx;

/* Ensure program-defined subprograms are emitted even if reachability misses them */
static void mark_program_subs_used(Tree_t *program)
{
    if (program == NULL || program->type != TREE_PROGRAM_TYPE)
        return;
    ListNode_t *cur = program->tree_data.program_data.subprograms;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *sub = (Tree_t *)cur->cur;
            if (sub->type == TREE_SUBPROGRAM)
            {
                if (sub->tree_data.subprogram_data.statement_list != NULL &&
                    sub->tree_data.subprogram_data.defined_in_unit == 0)
                {
                    sub->tree_data.subprogram_data.is_used = 1;
                }
                if (sub->tree_data.subprogram_data.subprograms != NULL)
                {
                    Tree_t wrapper = {0};
                    wrapper.type = TREE_PROGRAM_TYPE;
                    wrapper.tree_data.program_data.subprograms = sub->tree_data.subprogram_data.subprograms;
                    mark_program_subs_used(&wrapper);
                }
            }
        }
        cur = cur->next;
    }
}

static void print_usage(const char *prog_name)
{
    FILE *out = stdout;
    fprintf(out, "Usage: %s <input.p> <output.s> [flags]\n", prog_name);
    fprintf(out, "  Compiles Pascal source to x86-64 assembly\n");
    fprintf(out, "  Flags:\n");
    fprintf(out, "    -h, --help            Show this help text and exit\n");
    fprintf(out, "    -O1, -O2              Enable optimizations\n");
    fprintf(out, "    -non-local            Enable non-local variable chasing (experimental)\n");
    fprintf(out, "    --target=windows      Generate assembly for the Windows x64 ABI\n");
    fprintf(out, "    --target=sysv         Generate assembly for the System V AMD64 ABI\n");
    fprintf(out, "    --dump-ast=<file>     Write the parsed AST to <file>\n");
    fprintf(out, "    --time-passes         Print timing information for major compiler stages\n");
    fprintf(out, "    --asm-debug           Annotate emitted assembly with semantic/codegen info\n");
    fprintf(out, "    --disable-dce         Emit unused subprograms (debugging)\n");
    fprintf(out, "    -I<path>              Add include path for preprocessor\n");
    fprintf(out, "    -Fu<path>             Add unit search path (FPC compatible)\n");
    fprintf(out, "    --no-vendor-units     Disable built-in KGPC vendor units\n");
    fprintf(out, "    --no-stdlib           Disable KGPC stdlib; load minimal prelude instead\n");
    fprintf(out, "    --pp-cache-dir=<dir>  Cache parsed unit ASTs to <dir> for faster re-compilation\n");
    fprintf(out, "    -D<symbol>[=<value>]  Define preprocessor symbol\n");
    fprintf(out, "    -Us                   Compile System unit (FPC compatible)\n");
    fprintf(out, "    -Sg                   Enable goto statements (FPC compatible)\n");
}

typedef enum
{
    SET_FLAGS_CONTINUE = 0,
    SET_FLAGS_HELP = 1
} SetFlagsResult;

static bool dump_ast_to_requested_path(Tree_t *tree)
{
    const char *path = dump_ast_path();
    if (path == NULL || tree == NULL)
        return true;

    FILE *fp = fopen(path, "w");
    if (fp == NULL)
    {
        fprintf(stderr, "ERROR: Failed to open AST dump file: %s\n", path);
        return false;
    }

    tree_print(tree, fp, 0);
    fclose(fp);
    fprintf(stderr, "AST written to %s\n", path);
    return true;
}

#ifdef _WIN32
static char* realpath(const char* path, char* resolved_path)
{
    DWORD len = GetFullPathNameA(path, PATH_MAX, resolved_path, NULL);
    if (len == 0 || len >= PATH_MAX)
        return NULL;
    return resolved_path;
}
#endif

static ssize_t get_executable_path(char *buffer, size_t size, const char *argv0)
{
#if defined(__linux__) || defined(__unix__) || defined(__APPLE__)
    ssize_t len = readlink("/proc/self/exe", buffer, size - 1);
    if (len >= 0)
    {
        buffer[len] = '\0';
        return len;
    }
#elif defined(_WIN32)
    DWORD len = GetModuleFileNameA(NULL, buffer, (DWORD)size);
    if (len > 0 && len < size)
        return (ssize_t)len;
#endif

    if (argv0 != NULL)
    {
        char resolved[PATH_MAX];
        if (realpath(argv0, resolved) != NULL)
        {
            strncpy(buffer, resolved, size);
            buffer[size - 1] = '\0';
            return (ssize_t)strlen(buffer);
        }
    }

    return -1;
}

static char *resolve_stdlib_path(const char *argv0)
{
    const char *env = kgpc_getenv("KGPC_STDLIB");
    if (env != NULL && access(env, R_OK) == 0)
    {
        char *dup = strdup(env);
        if (dup != NULL)
            return dup;
    }

    if (access("KGPC/Units/system.p", R_OK) == 0)
    {
        char *dup = strdup("KGPC/Units/system.p");
        if (dup != NULL)
            return dup;
    }

    const char *source_root = getenv("MESON_SOURCE_ROOT");
    if (source_root != NULL)
    {
        char candidate[PATH_MAX];
        int written = snprintf(candidate, sizeof(candidate), "%s/KGPC/Units/system.p", source_root);
        if (written > 0 && written < (int)sizeof(candidate) && access(candidate, R_OK) == 0)
        {
            char *dup = strdup(candidate);
            if (dup != NULL)
                return dup;
        }
    }

    char exe_path[PATH_MAX];
    ssize_t len = get_executable_path(exe_path, sizeof(exe_path), argv0);
    if (len > 0)
    {
        char exe_dir[PATH_MAX];
        strncpy(exe_dir, exe_path, sizeof(exe_dir));
        exe_dir[sizeof(exe_dir) - 1] = '\0';

        char *dir = dirname(exe_dir);
        if (dir != NULL)
        {
            const char *relative_candidates[] = {
                "../../KGPC/Units/system.p",
                "../KGPC/Units/system.p",
                "../Units/system.p",
            };

            for (size_t i = 0; i < sizeof(relative_candidates) / sizeof(relative_candidates[0]); ++i)
            {
                char candidate[PATH_MAX];
                int written = snprintf(candidate, sizeof(candidate), "%s/%s", dir, relative_candidates[i]);
                if (written > 0 && written < (int)sizeof(candidate) && access(candidate, R_OK) == 0)
                {
                    char *dup = strdup(candidate);
                    if (dup != NULL)
                        return dup;
                }
            }
        }
    }

    return NULL;
}

static char *resolve_prelude_path(const char *argv0)
{
    if (access("KGPC/Units/prelude.p", R_OK) == 0)
    {
        char *dup = strdup("KGPC/Units/prelude.p");
        if (dup != NULL)
            return dup;
    }

    const char *source_root = getenv("MESON_SOURCE_ROOT");
    if (source_root != NULL)
    {
        char candidate[PATH_MAX];
        int written = snprintf(candidate, sizeof(candidate), "%s/KGPC/Units/prelude.p", source_root);
        if (written > 0 && written < (int)sizeof(candidate) && access(candidate, R_OK) == 0)
        {
            char *dup = strdup(candidate);
            if (dup != NULL)
                return dup;
        }
    }

    char exe_path[PATH_MAX];
    ssize_t len = get_executable_path(exe_path, sizeof(exe_path), argv0);
    if (len > 0)
    {
        char exe_dir[PATH_MAX];
        strncpy(exe_dir, exe_path, sizeof(exe_dir));
        exe_dir[sizeof(exe_dir) - 1] = '\0';

        char *dir = dirname(exe_dir);
        if (dir != NULL)
        {
            const char *relative_candidates[] = {
                "../../KGPC/Units/prelude.p",
                "../KGPC/Units/prelude.p",
                "../Units/prelude.p",
            };

            for (size_t i = 0; i < sizeof(relative_candidates) / sizeof(relative_candidates[0]); ++i)
            {
                char candidate[PATH_MAX];
                int written = snprintf(candidate, sizeof(candidate), "%s/%s", dir, relative_candidates[i]);
                if (written > 0 && written < (int)sizeof(candidate) && access(candidate, R_OK) == 0)
                {
                    char *dup = strdup(candidate);
                    if (dup != NULL)
                        return dup;
                }
            }
        }
    }

    return NULL;
}

static double g_time_parse_stdlib = 0.0;
static double g_time_parse_user = 0.0;
static double g_time_parse_units = 0.0;
static double g_time_semantic = 0.0;
static double g_time_codegen = 0.0;
static unsigned g_count_parse_stdlib = 0;
static unsigned g_count_parse_user = 0;
static unsigned g_count_parse_units = 0;

/* Report current RSS if KGPC_DEBUG_RSS is set.  Uses /proc/self/status on
 * Linux and getrusage() elsewhere. */
static void report_rss(const char *label)
{
#ifndef _WIN32
    if (kgpc_getenv("KGPC_DEBUG_RSS") == NULL) return;
    long rss_kb = 0;
    FILE *f = fopen("/proc/self/status", "r");
    if (f) {
        char line[256];
        while (fgets(line, sizeof(line), f)) {
            if (strncmp(line, "VmRSS:", 6) == 0) {
                rss_kb = atol(line + 6);
                break;
            }
        }
        fclose(f);
    }
    /* mallinfo2() is a glibc extension; keep allocator stats optional. */
#ifdef __GLIBC__
    struct mallinfo2 mi = mallinfo2();
    size_t heap_mb = (mi.arena + mi.hblkhd) / (1024 * 1024);
    size_t used_mb = (mi.uordblks + mi.hblkhd) / (1024 * 1024);
    fprintf(stderr, "[RSS] %-50s %ld KB (%.1f MB)  heap=%zuMB used=%zuMB\n",
            label, rss_kb, rss_kb / 1024.0, heap_mb, used_mb);
#else
    fprintf(stderr, "[RSS] %-50s %ld KB (%.1f MB)\n",
            label, rss_kb, rss_kb / 1024.0);
#endif
#else
    (void)label;
#endif
}

static uint64_t fnv1a64_bytes(const unsigned char *data, size_t len)
{
    uint64_t hash = 1469598103934665603ULL;
    for (size_t i = 0; i < len; ++i)
    {
        hash ^= (uint64_t)data[i];
        hash *= 1099511628211ULL;
    }
    return hash;
}

static int ensure_dir_exists(const char *path)
{
    if (path == NULL || path[0] == '\0')
        return -1;

#ifdef _WIN32
    if (_mkdir(path) == 0 || errno == EEXIST)
        return 0;
#else
    if (mkdir(path, 0777) == 0 || errno == EEXIST)
        return 0;
#endif
    return -1;
}

static char *build_default_ast_cache_dir(const char *argv0)
{
    if (argv0 == NULL || argv0[0] == '\0')
        return NULL;

    char compiler_path[PATH_MAX];
#ifndef _WIN32
    if (realpath(argv0, compiler_path) == NULL)
        return NULL;
#else
    strncpy(compiler_path, argv0, sizeof(compiler_path) - 1);
    compiler_path[sizeof(compiler_path) - 1] = '\0';
#endif

    char *last_sep = strrchr(compiler_path, '/');
#ifdef _WIN32
    char *backslash = strrchr(compiler_path, '\\');
    if (backslash != NULL && (last_sep == NULL || backslash > last_sep))
        last_sep = backslash;
#endif
    if (last_sep == NULL)
        return NULL;
    *last_sep = '\0';

    struct stat st;
    memset(&st, 0, sizeof(st));
    if (stat(argv0, &st) != 0)
        memset(&st, 0, sizeof(st));

    char signature_input[PATH_MAX + 128];
    snprintf(signature_input, sizeof(signature_input), "%s|%lld|%lld",
             compiler_path,
             (long long)st.st_size,
             (long long)st.st_mtime);
    uint64_t sig = fnv1a64_bytes((const unsigned char *)signature_input, strlen(signature_input));

    size_t needed = strlen(compiler_path) + 1 + strlen("kgpc_ast_cache_") + 16 + 1;
    char *dir = (char *)malloc(needed);
    if (dir == NULL)
        return NULL;

    snprintf(dir, needed, "%s/kgpc_ast_cache_%016llx",
             compiler_path, (unsigned long long)sig);
    if (ensure_dir_exists(dir) != 0)
    {
        free(dir);
        return NULL;
    }

    return dir;
}

static int profile_pipeline_flag(void)
{
    static int initialized = 0;
    static int enabled = 0;

    if (!initialized)
    {
        const char *value = kgpc_getenv("KGPC_PROFILE_PIPELINE");
        enabled = (value != NULL && value[0] != '\0' && strcmp(value, "0") != 0);
        initialized = 1;
    }

    return enabled;
}

static double current_time_seconds(void)
{
#ifdef _WIN32
    LARGE_INTEGER freq;
    LARGE_INTEGER counter;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&counter);
    return (double)counter.QuadPart / (double)freq.QuadPart;
#else
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0)
        return ts.tv_sec + ts.tv_nsec / 1e9;
    return (double)clock() / CLOCKS_PER_SEC;
#endif
}

static void emit_profile_stage(const char *stage, double elapsed_seconds)
{
    if (!profile_pipeline_flag())
        return;
    fprintf(stderr, "[profile] %s: %.3fs\n", stage, elapsed_seconds);
}

static void emit_timing_summary(void)
{
    if (!time_passes_flag())
        return;

    fprintf(stderr, "[time] parse stdlib: %u run(s) in %.3fs\n",
            g_count_parse_stdlib, g_time_parse_stdlib);
    fprintf(stderr, "[time] parse user units: %u run(s) in %.3fs\n",
            g_count_parse_units, g_time_parse_units);
    fprintf(stderr, "[time] parse user program: %u run(s) in %.3fs\n",
            g_count_parse_user, g_time_parse_user);
    fprintf(stderr, "[time] semantic analysis: %.3fs\n", g_time_semantic);
    fprintf(stderr, "[time] code generation: %.3fs\n", g_time_codegen);
    fprintf(stderr, "[time] total (tracked stages): %.3fs\n",
            g_time_parse_stdlib + g_time_parse_units + g_time_parse_user +
            g_time_semantic + g_time_codegen);
}

static SetFlagsResult set_flags(char **optional_args, int count)
{
    int i = 0;
    while (count > 0)
    {
        const char *arg = optional_args[i];
        if (strcmp(arg, "-h") == 0 || strcmp(arg, "--help") == 0 || strcmp(arg, "help") == 0)
            return SET_FLAGS_HELP;
        if (strcmp(arg, "-non-local") == 0)
        {
            fprintf(stderr, "Non-local codegen support enabled\n");
            fprintf(stderr, "WARNING: Non-local is still in development and is very buggy!\n\n");
            set_nonlocal_flag();
        }
        else if (strcmp(arg, "-O1") == 0)
        {
            fprintf(stderr, "O1 optimizations enabled!\n\n");
            set_o1_flag();
        }
        else if (strcmp(arg, "-O2") == 0)
        {
            fprintf(stderr, "O2 optimizations enabled!\n\n");
            set_o2_flag();
        }
        else if (strcmp(arg, "-parse-only") == 0 || strcmp(arg, "--parse-only") == 0)
        {
            fprintf(stderr, "Parse-only mode enabled.\n\n");
            set_parse_only_flag();
        }
        else if (strcmp(arg, "--emit-link-args") == 0)
        {
            g_emit_link_args = 1;
        }
        else if (strcmp(arg, "--target-windows") == 0 || strcmp(arg, "-target-windows") == 0 || strcmp(arg, "--windows-abi") == 0)
        {
            fprintf(stderr, "Target ABI: Windows x64\n\n");
            fflush(stderr);
            set_target_windows_flag();
        }
        else if (strcmp(arg, "--target-sysv") == 0 || strcmp(arg, "-target-sysv") == 0 || strcmp(arg, "--sysv-abi") == 0)
        {
            fprintf(stderr, "Target ABI: System V AMD64\n\n");
            set_target_sysv_flag();
        }
        else if ((strcmp(arg, "--target") == 0 || strcmp(arg, "-target") == 0) && count > 1)
        {
            const char *value = optional_args[i + 1];
            if (strcasecmp(value, "windows") == 0 || strcasecmp(value, "win64") == 0)
            {
                fprintf(stderr, "Target ABI: Windows x64\n\n");
                fflush(stderr);
                set_target_windows_flag();
            }
            else if (strcasecmp(value, "sysv") == 0 || strcasecmp(value, "systemv") == 0 || strcasecmp(value, "linux") == 0)
            {
                fprintf(stderr, "Target ABI: System V AMD64\n\n");
                fflush(stderr);
                set_target_sysv_flag();
            }
            else
            {
                fprintf(stderr, "ERROR: Unknown target ABI '%s'\n", value);
                exit(1);
            }
            --count;
            ++i;
        }
        else if (strncmp(arg, "--target=", 9) == 0)
        {
            const char *value = arg + 9;
            if (strcasecmp(value, "windows") == 0 || strcasecmp(value, "win64") == 0)
            {
                fprintf(stderr, "Target ABI: Windows x64\n\n");
                fflush(stderr);
                set_target_windows_flag();
            }
            else if (strcasecmp(value, "sysv") == 0 || strcasecmp(value, "systemv") == 0 || strcasecmp(value, "linux") == 0)
            {
                fprintf(stderr, "Target ABI: System V AMD64\n\n");
                fflush(stderr);
                set_target_sysv_flag();
            }
            else
            {
                fprintf(stderr, "ERROR: Unknown target ABI '%s'\n", value);
                exit(1);
            }
        }
        else if ((strcmp(arg, "--dump-ast") == 0 || strcmp(arg, "-dump-ast") == 0) && count > 1)
        {
            const char *path = optional_args[i + 1];
            set_dump_ast_path(path);
            fprintf(stderr, "AST dump enabled: %s\n\n", path);
            --count;
            ++i;
        }
        else if (strncmp(arg, "--dump-ast=", 11) == 0)
        {
            const char *path = arg + 11;
            set_dump_ast_path(path);
            fprintf(stderr, "AST dump enabled: %s\n\n", path);
        }
        else if (strcmp(arg, "--time-passes") == 0)
        {
            fprintf(stderr, "Timing instrumentation enabled.\n\n");
            set_time_passes_flag();
        }
        else if (strcmp(arg, "--asm-debug") == 0 || strcmp(arg, "--asm-debug-comments") == 0)
        {
            fprintf(stderr, "Assembly debug comments enabled.\n\n");
            set_asm_debug_flag();
        }
        else if (strcmp(arg, "--disable-dce") == 0 || strcmp(arg, "--no-dce") == 0)
        {
            fprintf(stderr, "Dead-code elimination disabled (emit unused subprograms).\n\n");
            set_disable_dce_flag();
        }
        else if (strcmp(arg, "--function-sections") == 0)
        {
            set_function_sections_flag();
        }
        else if (strcmp(arg, "--skip-unit-codegen") == 0)
        {
            set_skip_unit_codegen_flag();
        }
        else if (strncmp(arg, "--codegen-cache-dir=", 20) == 0)
        {
            g_codegen_cache_dir = &arg[20];
        }
        else if (arg[0] == '-' && arg[1] == 'I' && arg[2] != '\0')
        {
            /* Include path: -I/path/to/include */
            pascal_frontend_add_include_path(&arg[2]);
        }
        else if (arg[0] == '-' && arg[1] == 'F' && arg[2] == 'u' && arg[3] != '\0')
        {
            /* Unit search path: -Fu/path/to/units (FPC compatible) */
            unit_search_paths_add_unit_path(&g_unit_paths, &arg[3]);
        }
        else if (strcmp(arg, "--no-vendor-units") == 0)
        {
            /* Disable built-in KGPC vendor units */
            unit_search_paths_disable_vendor(&g_unit_paths);
        }
        else if (strcmp(arg, "--no-stdlib") == 0 || strcmp(arg, "--no-prelude") == 0)
        {
            g_skip_stdlib = true;
        }
        else if (strncmp(arg, "--pp-cache-dir=", 15) == 0)
        {
            g_ast_cache_explicit = 1;
            pascal_frontend_set_ast_cache_dir(&arg[15]);
        }
        else if (strcmp(arg, "--batch") == 0)
        {
            g_batch_mode = 1;
        }
        else if (strncmp(arg, "--batch-max-parallel=", 20) == 0)
        {
            g_batch_max_parallel = atoi(&arg[20]);
            if (g_batch_max_parallel < 1)
                g_batch_max_parallel = 1;
        }
        else if (arg[0] == '-' && arg[1] == 'D' && arg[2] != '\0')
        {
            /* Define: -DSYMBOL or -DSYMBOL=VALUE */
            pascal_frontend_add_define(&arg[2]);
        }
        else if (strcmp(arg, "-Us") == 0)
        {
            /* FPC flag: compile System unit mode */
            set_compile_system_unit_flag();
            g_skip_stdlib = true; /* System unit is self-contained */
        }
        else if (strcmp(arg, "-Sg") == 0)
        {
            /* FPC flag: enable goto statements */
            set_goto_enabled_flag();
        }
        else if (arg[0] == '-' && arg[1] == 'S' && arg[2] != '\0')
        {
            /* Other FPC -S flags: silently ignore for compatibility */
            /* e.g., -Sew (warnings as errors), -Sd (Delphi mode), etc. */
        }
        else if (arg[0] == '-' && arg[1] == 'C' && arg[2] != '\0')
        {
            /* FPC -C flags: code generation options - silently ignore */
            /* e.g., -Cg (PIC code), etc. */
        }
        else if (arg[0] == '-' && arg[1] == 'F' && arg[2] != '\0')
        {
            /* Other FPC -F flags: silently ignore for compatibility */
            /* e.g., -FE, -Fl, etc. (already handled -Fu and -Fi) */
        }
        else if (arg[0] == '-' && arg[1] == 'd' && arg[2] != '\0')
        {
            /* FPC lowercase -d: define symbol (same as -D) */
            pascal_frontend_add_define(&arg[2]);
        }
        else if (arg[0] == '-')
        {
            fprintf(stderr, "ERROR: Unrecognized flag: %s\n", arg);
            exit(1);
        }
        /* else: positional argument (input/output file), skip silently */

        --count;
        ++i;
    }

    return SET_FLAGS_CONTINUE;
}

static void mark_stdlib_var_params(ListNode_t *subprograms)
{
    for (ListNode_t *node = subprograms; node != NULL; node = node->next)
    {
        if (node->type != LIST_TREE || node->cur == NULL)
            continue;

        Tree_t *sub = (Tree_t *)node->cur;
        if (sub->type != TREE_SUBPROGRAM)
            continue;

        if (sub->tree_data.subprogram_data.sub_type != TREE_SUBPROGRAM_PROC ||
            sub->tree_data.subprogram_data.id == NULL)
            continue;

        if (strcasecmp(sub->tree_data.subprogram_data.id, "read") != 0)
            continue;

        ListNode_t *params = sub->tree_data.subprogram_data.args_var;
        while (params != NULL)
        {
            if (params->type == LIST_TREE && params->cur != NULL)
            {
                Tree_t *param_decl = (Tree_t *)params->cur;
                if (param_decl->type == TREE_VAR_DECL)
                    param_decl->tree_data.var_decl_data.is_var_param = 1;
            }
            params = params->next;
        }
    }
}

static bool parse_pascal_file(const char *path, Tree_t **out_tree, bool convert_to_tree)
{
    Tree_t *tree = NULL;
    ParseError *error = NULL;
    bool success = pascal_parse_source(path, convert_to_tree, &tree, &error);
    if (!success)
    {
        pascal_print_parse_error(path, error);
        if (error != NULL)
            free_error(error);
    }

    if (out_tree != NULL)
    {
        if (success && convert_to_tree)
            *out_tree = tree;
        else
            *out_tree = NULL;
    }
    else if (tree != NULL && convert_to_tree)
    {
        destroy_tree(tree);
    }

    return success;
}

typedef struct
{
    char **names;
    size_t count;
    size_t capacity;
} UnitSet;

static void unit_set_init(UnitSet *set)
{
    set->names = NULL;
    set->count = 0;
    set->capacity = 0;
}

static void unit_set_destroy(UnitSet *set)
{
    if (set->names != NULL)
    {
        for (size_t i = 0; i < set->count; ++i)
            free(set->names[i]);
        free(set->names);
    }
    set->names = NULL;
    set->count = 0;
    set->capacity = 0;
}

static bool unit_set_contains(const UnitSet *set, const char *name)
{
    if (name == NULL)
        return true;

    for (size_t i = 0; i < set->count; ++i)
        if (strcmp(set->names[i], name) == 0)
            return true;

    return false;
}

static bool unit_set_add(UnitSet *set, char *name)
{
    if (name == NULL)
        return false;

    if (unit_set_contains(set, name))
    {
        free(name);
        return false;
    }

    if (set->count == set->capacity)
    {
        size_t new_capacity = set->capacity == 0 ? 4 : set->capacity * 2;
        char **new_names = (char **)realloc(set->names, new_capacity * sizeof(char *));
        if (new_names == NULL)
        {
            free(name);
            return false;
        }
        set->names = new_names;
        set->capacity = new_capacity;
    }

    set->names[set->count++] = name;
    return true;
}

static ListNode_t *get_prelude_subprograms(Tree_t *prelude)
{
    if (prelude == NULL)
        return NULL;
    if (prelude->type == TREE_PROGRAM_TYPE)
        return prelude->tree_data.program_data.subprograms;
    if (prelude->type == TREE_UNIT)
        return prelude->tree_data.unit_data.subprograms;
    return NULL;
}

static ListNode_t *get_prelude_type_decls(Tree_t *prelude)
{
    if (prelude == NULL)
        return NULL;
    if (prelude->type == TREE_PROGRAM_TYPE)
        return prelude->tree_data.program_data.type_declaration;
    if (prelude->type == TREE_UNIT)
        return prelude->tree_data.unit_data.interface_type_decls;
    return NULL;
}

static ListNode_t *get_prelude_const_decls(Tree_t *prelude)
{
    if (prelude == NULL)
        return NULL;
    if (prelude->type == TREE_PROGRAM_TYPE)
        return prelude->tree_data.program_data.const_declaration;
    if (prelude->type == TREE_UNIT)
        return prelude->tree_data.unit_data.interface_const_decls;
    return NULL;
}

static ListNode_t *get_prelude_var_decls(Tree_t *prelude)
{
    if (prelude == NULL)
        return NULL;
    if (prelude->type == TREE_PROGRAM_TYPE)
        return prelude->tree_data.program_data.var_declaration;
    if (prelude->type == TREE_UNIT)
        return prelude->tree_data.unit_data.interface_var_decls;
    return NULL;
}

static void clear_prelude_subprograms(Tree_t *prelude)
{
    if (prelude == NULL)
        return;
    if (prelude->type == TREE_PROGRAM_TYPE)
        prelude->tree_data.program_data.subprograms = NULL;
    else if (prelude->type == TREE_UNIT)
        prelude->tree_data.unit_data.subprograms = NULL;
}

static void clear_prelude_type_decls(Tree_t *prelude)
{
    if (prelude == NULL)
        return;
    if (prelude->type == TREE_PROGRAM_TYPE)
        prelude->tree_data.program_data.type_declaration = NULL;
    else if (prelude->type == TREE_UNIT)
        prelude->tree_data.unit_data.interface_type_decls = NULL;
}

static void clear_prelude_const_decls(Tree_t *prelude)
{
    if (prelude == NULL)
        return;
    if (prelude->type == TREE_PROGRAM_TYPE)
        prelude->tree_data.program_data.const_declaration = NULL;
    else if (prelude->type == TREE_UNIT)
        prelude->tree_data.unit_data.interface_const_decls = NULL;
}

static void clear_prelude_var_decls(Tree_t *prelude)
{
    if (prelude == NULL)
        return;
    if (prelude->type == TREE_PROGRAM_TYPE)
        prelude->tree_data.program_data.var_declaration = NULL;
    else if (prelude->type == TREE_UNIT)
        prelude->tree_data.unit_data.interface_var_decls = NULL;
}


static char *lowercase_copy(const char *name)
{
    return unit_search_paths_normalize_name(name);
}

static char *build_unit_path(const char *unit_name)
{
    return unit_search_paths_resolve(&g_unit_paths, unit_name);
}

static void mark_unit_subprograms(ListNode_t *sub_list, int unit_index, int is_public)
{
    ListNode_t *node = sub_list;
    while (node != NULL)
    {
        if (node->type == LIST_TREE && node->cur != NULL)
        {
            Tree_t *sub = (Tree_t *)node->cur;
            if (sub->type == TREE_SUBPROGRAM)
            {
                sub->tree_data.subprogram_data.defined_in_unit = 1;
                if (is_public)
                    sub->tree_data.subprogram_data.unit_is_public = 1;
                if (unit_index > 0 && sub->tree_data.subprogram_data.source_unit_index == 0)
                    sub->tree_data.subprogram_data.source_unit_index = unit_index;
                /* Also mark formal parameters so type resolution (e.g. TSize)
                 * picks the correct unit-specific type during overload matching,
                 * even before semcheck_subprogram processes this function. */
                ListNode_t *arg = sub->tree_data.subprogram_data.args_var;
                while (arg != NULL)
                {
                    if (arg->type == LIST_TREE && arg->cur != NULL)
                    {
                        Tree_t *arg_tree = (Tree_t *)arg->cur;
                        if (arg_tree->type == TREE_VAR_DECL)
                        {
                            arg_tree->tree_data.var_decl_data.defined_in_unit = 1;
                            if (unit_index > 0 && arg_tree->tree_data.var_decl_data.source_unit_index == 0)
                                arg_tree->tree_data.var_decl_data.source_unit_index = unit_index;
                        }
                        else if (arg_tree->type == TREE_ARR_DECL)
                        {
                            arg_tree->tree_data.arr_decl_data.defined_in_unit = 1;
                            if (unit_index > 0 && arg_tree->tree_data.arr_decl_data.source_unit_index == 0)
                                arg_tree->tree_data.arr_decl_data.source_unit_index = unit_index;
                        }
                    }
                    arg = arg->next;
                }
            }
        }
        node = node->next;
    }
}

static void mark_unit_type_decls(ListNode_t *type_list, int is_public, int unit_index)
{
    ListNode_t *node = type_list;
    while (node != NULL)
    {
        if (node->type == LIST_TREE && node->cur != NULL)
        {
            Tree_t *decl = (Tree_t *)node->cur;
            if (decl->type == TREE_TYPE_DECL)
            {
                decl->tree_data.type_decl_data.defined_in_unit = 1;
                decl->tree_data.type_decl_data.unit_is_public = is_public ? 1 : 0;
                if (unit_index > 0 && decl->tree_data.type_decl_data.source_unit_index == 0)
                    decl->tree_data.type_decl_data.source_unit_index = unit_index;
                /* Propagate unit index to the RecordType so field type lookups
                   can prefer the defining unit's types. */
                if (unit_index > 0 &&
                    decl->tree_data.type_decl_data.kind == TYPE_DECL_RECORD &&
                    decl->tree_data.type_decl_data.info.record != NULL &&
                    decl->tree_data.type_decl_data.info.record->source_unit_index == 0)
                    decl->tree_data.type_decl_data.info.record->source_unit_index = unit_index;
            }
        }
        node = node->next;
    }
}

static void mark_unit_const_decls(ListNode_t *const_list, int is_public, int unit_index)
{
    ListNode_t *node = const_list;
    while (node != NULL)
    {
        if (node->type == LIST_TREE && node->cur != NULL)
        {
            Tree_t *decl = (Tree_t *)node->cur;
            if (decl->type == TREE_CONST_DECL)
            {
                decl->tree_data.const_decl_data.defined_in_unit = 1;
                decl->tree_data.const_decl_data.unit_is_public = is_public ? 1 : 0;
                if (unit_index > 0 && decl->tree_data.const_decl_data.source_unit_index == 0)
                    decl->tree_data.const_decl_data.source_unit_index = unit_index;
            }
        }
        node = node->next;
    }
}

static void mark_unit_var_decls(ListNode_t *var_list, int is_public, int unit_index)
{
    ListNode_t *node = var_list;
    while (node != NULL)
    {
        if (node->type == LIST_TREE && node->cur != NULL)
        {
            Tree_t *decl = (Tree_t *)node->cur;
            if (decl->type == TREE_VAR_DECL)
            {
                decl->tree_data.var_decl_data.defined_in_unit = 1;
                decl->tree_data.var_decl_data.unit_is_public = is_public ? 1 : 0;
                if (unit_index > 0 && decl->tree_data.var_decl_data.source_unit_index == 0)
                    decl->tree_data.var_decl_data.source_unit_index = unit_index;
            }
            else if (decl->type == TREE_ARR_DECL)
            {
                decl->tree_data.arr_decl_data.defined_in_unit = 1;
                decl->tree_data.arr_decl_data.unit_is_public = is_public ? 1 : 0;
                if (unit_index > 0 && decl->tree_data.arr_decl_data.source_unit_index == 0)
                    decl->tree_data.arr_decl_data.source_unit_index = unit_index;
            }
            else
            {
                /* Other declaration types don't need marking */
            }
        }
        node = node->next;
    }
}

/* Helper macros to get declaration lists from either program or unit trees */
static ListNode_t **get_type_decl_list(Tree_t *target)
{
    assert(target != NULL);
    if (target->type == TREE_PROGRAM_TYPE)
        return &target->tree_data.program_data.type_declaration;
    else if (target->type == TREE_UNIT)
        return &target->tree_data.unit_data.interface_type_decls;
    else
        return NULL;
}

static ListNode_t **get_const_decl_list(Tree_t *target)
{
    assert(target != NULL);
    if (target->type == TREE_PROGRAM_TYPE)
        return &target->tree_data.program_data.const_declaration;
    else if (target->type == TREE_UNIT)
        return &target->tree_data.unit_data.interface_const_decls;
    else
        return NULL;
}

static ListNode_t **get_var_decl_list(Tree_t *target)
{
    assert(target != NULL);
    if (target->type == TREE_PROGRAM_TYPE)
        return &target->tree_data.program_data.var_declaration;
    else if (target->type == TREE_UNIT)
        return &target->tree_data.unit_data.interface_var_decls;
    else
        return NULL;
}

static ListNode_t **get_subprograms_list(Tree_t *target)
{
    assert(target != NULL);
    if (target->type == TREE_PROGRAM_TYPE)
        return &target->tree_data.program_data.subprograms;
    else if (target->type == TREE_UNIT)
        return &target->tree_data.unit_data.subprograms;
    else
        return NULL;
}

static int type_list_contains(ListNode_t *list, const char *type_id)
{
    ListNode_t *node = list;
    while (node != NULL)
    {
        if (node->type == LIST_TREE && node->cur != NULL)
        {
            Tree_t *decl = (Tree_t *)node->cur;
            if (decl->type == TREE_TYPE_DECL &&
                decl->tree_data.type_decl_data.id != NULL &&
                pascal_identifier_equals(decl->tree_data.type_decl_data.id, type_id))
            {
                return 1;
            }
        }
        node = node->next;
    }
    return 0;
}

static void debug_check_type_presence(Tree_t *target)
{
    const char *check_id = kgpc_getenv("KGPC_DEBUG_CHECK_TYPE");
    if (check_id == NULL || check_id[0] == '\0' || target == NULL)
        return;

    ListNode_t *interface_types = NULL;
    ListNode_t *implementation_types = NULL;
    if (target->type == TREE_PROGRAM_TYPE)
    {
        interface_types = target->tree_data.program_data.type_declaration;
    }
    else if (target->type == TREE_UNIT)
    {
        interface_types = target->tree_data.unit_data.interface_type_decls;
        implementation_types = target->tree_data.unit_data.implementation_type_decls;
    }

    int in_interface = type_list_contains(interface_types, check_id);
    int in_implementation = type_list_contains(implementation_types, check_id);
    fprintf(stderr, "[KGPC] type '%s' present: interface=%d implementation=%d target=%d\n",
        check_id, in_interface, in_implementation, target->type);
}

static ListNode_t *merge_unit_type_decls_before_locals(ListNode_t *head, ListNode_t *unit_types)
{
    if (unit_types == NULL)
        return head;
    if (head == NULL)
        return unit_types;

    ListNode_t *prev = NULL;
    ListNode_t *cur = head;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *decl = (Tree_t *)cur->cur;
            if (decl->type == TREE_TYPE_DECL &&
                !decl->tree_data.type_decl_data.defined_in_unit)
                break;
        }
        prev = cur;
        cur = cur->next;
    }

    if (prev == NULL)
        return ConcatList(unit_types, head);

    prev->next = ConcatList(unit_types, cur);
    return head;
}

/* Detach and return only public interface subprogram declarations from a unit subprogram list.
 * Non-public implementation subprogram nodes remain in *list for the caller to dispose with the unit tree. */
static ListNode_t *extract_public_unit_subprograms(ListNode_t **list)
{
    if (list == NULL || *list == NULL)
        return NULL;

    ListNode_t *public_head = NULL;
    ListNode_t **public_tail = &public_head;
    ListNode_t *prev = NULL;
    ListNode_t *cur = *list;
    while (cur != NULL)
    {
        ListNode_t *next = cur->next;
        int take = 0;
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *sub = (Tree_t *)cur->cur;
            if (sub->type == TREE_SUBPROGRAM &&
                sub->tree_data.subprogram_data.unit_is_public)
            {
                take = 1;
            }
        }

        if (take)
        {
            if (prev != NULL)
                prev->next = next;
            else
                *list = next;
            cur->next = NULL;
            *public_tail = cur;
            public_tail = &cur->next;
        }
        else
        {
            prev = cur;
        }
        cur = next;
    }
    return public_head;
}

/* Build combined declaration lists from loaded units into the target tree
 * (program or unit) so that semcheck can iterate a single list per
 * declaration kind.
 *
 * Semantic correctness does NOT depend on concatenation order:
 *   - Each declaration carries source_unit_index and defined_in_unit flags
 *   - semcheck pushes declarations into per-unit scopes via source_unit_index
 *   - Cross-unit references resolve through scope dependency edges
 *   - Imported vs local processing is driven by the defined_in_unit flag
 *
 * When the target is a program (TREE_PROGRAM_TYPE), both interface and
 * implementation declarations are merged.  When the target is a unit
 * (TREE_UNIT), only interface declarations and public subprograms are
 * merged (implementation details stay private to the imported unit).
 *
 * Called once after all units are loaded, before semcheck. */
static void build_combined_program_view(CompilationContext *comp_ctx)
{
    Tree_t *target = comp_ctx->program;
    if (target == NULL)
        return;

    int is_program = (target->type == TREE_PROGRAM_TYPE);
    int is_unit = (target->type == TREE_UNIT);
    if (!is_program && !is_unit)
        return;

    ListNode_t **type_list = get_type_decl_list(target);
    ListNode_t **const_list = get_const_decl_list(target);
    ListNode_t **var_list = get_var_decl_list(target);
    ListNode_t **sub_list = get_subprograms_list(target);

    for (int i = 0; i < comp_ctx->loaded_unit_count; ++i)
    {
        Tree_t *unit_tree = comp_ctx->loaded_units[i].unit_tree;
        if (unit_tree == NULL)
            continue;

        int unit_idx = comp_ctx->loaded_units[i].unit_idx;

        /* Types: interface (always) + implementation (programs only).
         * mark_unit_* calls are already done in load_unit() before
         * semcheck_unit_decls_only, so they are not repeated here. */
        if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL) {
            ListNode_t *dbg = unit_tree->tree_data.unit_data.interface_type_decls;
            while (dbg != NULL) {
                if (dbg->type == LIST_TREE) {
                    Tree_t *decl = (Tree_t *)dbg->cur;
                    if (decl != NULL && decl->type == TREE_TYPE_DECL &&
                        decl->tree_data.type_decl_data.id != NULL)
                    {
                        fprintf(stderr, "[KGPC] merging interface type %s from unit %s\n",
                                decl->tree_data.type_decl_data.id,
                                unit_tree->tree_data.unit_data.unit_id != NULL ?
                                    unit_tree->tree_data.unit_data.unit_id : "<unknown>");
                    }
                }
                dbg = dbg->next;
            }
        }
        *type_list = merge_unit_type_decls_before_locals(
                *type_list,
                unit_tree->tree_data.unit_data.interface_type_decls);
        unit_tree->tree_data.unit_data.interface_type_decls = NULL;

        if (is_program) {
            if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL) {
                ListNode_t *dbg = unit_tree->tree_data.unit_data.implementation_type_decls;
                while (dbg != NULL) {
                    if (dbg->type == LIST_TREE) {
                        Tree_t *decl = (Tree_t *)dbg->cur;
                        if (decl != NULL && decl->type == TREE_TYPE_DECL &&
                            decl->tree_data.type_decl_data.id != NULL)
                        {
                            fprintf(stderr, "[KGPC] merging impl type %s from unit %s\n",
                                    decl->tree_data.type_decl_data.id,
                                    unit_tree->tree_data.unit_data.unit_id != NULL ?
                                        unit_tree->tree_data.unit_data.unit_id : "<unknown>");
                        }
                    }
                    dbg = dbg->next;
                }
            }
            *type_list = merge_unit_type_decls_before_locals(
                    *type_list,
                    unit_tree->tree_data.unit_data.implementation_type_decls);
            unit_tree->tree_data.unit_data.implementation_type_decls = NULL;
        }

        /* Constants: interface (always) + implementation (programs only) */
        if (kgpc_getenv("KGPC_DEBUG_CONST") != NULL) {
            ListNode_t *dbg = unit_tree->tree_data.unit_data.interface_const_decls;
            while (dbg != NULL) {
                if (dbg->type == LIST_TREE) {
                    Tree_t *decl = (Tree_t *)dbg->cur;
                    if (decl != NULL && decl->type == TREE_CONST_DECL &&
                        decl->tree_data.const_decl_data.id != NULL)
                    {
                        fprintf(stderr, "[KGPC] merging interface const '%s' from unit '%s'\n",
                                decl->tree_data.const_decl_data.id,
                                unit_tree->tree_data.unit_data.unit_id != NULL ?
                                    unit_tree->tree_data.unit_data.unit_id : "<unknown>");
                    }
                }
                dbg = dbg->next;
            }
        }
        *const_list = ConcatList(*const_list,
                       unit_tree->tree_data.unit_data.interface_const_decls);
        unit_tree->tree_data.unit_data.interface_const_decls = NULL;

        if (is_program) {
            *const_list = ConcatList(*const_list,
                           unit_tree->tree_data.unit_data.implementation_const_decls);
            unit_tree->tree_data.unit_data.implementation_const_decls = NULL;
        }

        /* Variables: interface (always) + implementation (programs only) */
        *var_list = ConcatList(*var_list,
                       unit_tree->tree_data.unit_data.interface_var_decls);
        unit_tree->tree_data.unit_data.interface_var_decls = NULL;

        if (is_program) {
            *var_list = ConcatList(*var_list,
                           unit_tree->tree_data.unit_data.implementation_var_decls);
            unit_tree->tree_data.unit_data.implementation_var_decls = NULL;
        }

        /* Subprograms: programs get all; units get only public interface subs */
        if (is_program) {
            *sub_list = ConcatList(*sub_list,
                           unit_tree->tree_data.unit_data.subprograms);
            unit_tree->tree_data.unit_data.subprograms = NULL;
        } else {
            ListNode_t *public_subs = extract_public_unit_subprograms(
                    &unit_tree->tree_data.unit_data.subprograms);
            *sub_list = ConcatList(*sub_list, public_subs);
        }

        /* Init/final stay on the unit tree.  For programs, semcheck and
         * mark_used access them via compilation_context_get_active().
         * Codegen emits them from unit trees in dependency order. */
        if (unit_tree->tree_data.unit_data.finalization != NULL)
            unit_tree->tree_data.unit_data.finalization->source_unit_index = unit_idx;
    }
}

/* Helper: extract list nodes whose Tree_t has the given source_unit_index and
 * return them as a new list.  Nodes that don't match stay in the original list.
 * get_unit_index is a callback that reads source_unit_index from a Tree_t node. */
typedef int (*get_unit_index_fn)(const Tree_t *);

static int get_type_decl_unit_index(const Tree_t *t) {
    if (t == NULL || t->type != TREE_TYPE_DECL) return 0;
    return t->tree_data.type_decl_data.source_unit_index;
}
static int get_const_decl_unit_index(const Tree_t *t) {
    if (t == NULL || t->type != TREE_CONST_DECL) return 0;
    return t->tree_data.const_decl_data.source_unit_index;
}
static int get_var_decl_unit_index(const Tree_t *t) {
    if (t == NULL) return 0;
    if (t->type == TREE_VAR_DECL)
        return t->tree_data.var_decl_data.source_unit_index;
    if (t->type == TREE_ARR_DECL)
        return t->tree_data.arr_decl_data.source_unit_index;
    return 0;
}
static int get_subprogram_unit_index(const Tree_t *t) {
    if (t == NULL || t->type != TREE_SUBPROGRAM) return 0;
    return t->tree_data.subprogram_data.source_unit_index;
}

/* Extract nodes with source_unit_index == target_idx from *list_ptr,
 * returning them as a new list.  Modifies *list_ptr in place. */
static ListNode_t *extract_unit_nodes(ListNode_t **list_ptr, int target_idx,
                                       get_unit_index_fn get_idx)
{
    if (list_ptr == NULL || *list_ptr == NULL)
        return NULL;

    ListNode_t *extracted_head = NULL;
    ListNode_t **extracted_tail = &extracted_head;
    ListNode_t *prev = NULL;
    ListNode_t *cur = *list_ptr;

    while (cur != NULL) {
        ListNode_t *next = cur->next;
        int idx = 0;
        if (cur->type == LIST_TREE && cur->cur != NULL)
            idx = get_idx((const Tree_t *)cur->cur);

        if (idx == target_idx) {
            /* Remove from original list */
            if (prev != NULL)
                prev->next = next;
            else
                *list_ptr = next;
            /* Append to extracted list */
            cur->next = NULL;
            *extracted_tail = cur;
            extracted_tail = &cur->next;
        } else {
            prev = cur;
        }
        cur = next;
    }
    return extracted_head;
}

/* Restore unit declarations from the merged target tree back to unit records.
 * Called after semcheck (which needs the merged view) and before codegen
 * (which should iterate unit records directly).
 *
 * Works for both program and unit compilation targets.
 * Init/finalization are stored on LoadedUnit records (not inlined into the
 * program body) and emitted by codegen in dependency order. */
static void unbuild_combined_program_view(CompilationContext *comp_ctx)
{
    Tree_t *target = comp_ctx->program;
    if (target == NULL)
        return;
    if (target->type != TREE_PROGRAM_TYPE && target->type != TREE_UNIT)
        return;

    ListNode_t **type_list = get_type_decl_list(target);
    ListNode_t **const_list = get_const_decl_list(target);
    ListNode_t **var_list = get_var_decl_list(target);
    ListNode_t **sub_list = get_subprograms_list(target);

    for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
        LoadedUnit *lu = &comp_ctx->loaded_units[i];
        Tree_t *unit_tree = lu->unit_tree;
        if (unit_tree == NULL || unit_tree->type != TREE_UNIT)
            continue;

        int unit_idx = lu->unit_idx;

        /* Extract type declarations belonging to this unit.
         * build_combined_program_view merged interface (and for programs,
         * implementation) types into the target list.  We restore them to
         * interface_type_decls so codegen has a single list per unit. */
        ListNode_t *unit_types = extract_unit_nodes(
            type_list, unit_idx, get_type_decl_unit_index);
        unit_tree->tree_data.unit_data.interface_type_decls = unit_types;

        /* Extract const declarations */
        ListNode_t *unit_consts = extract_unit_nodes(
            const_list, unit_idx, get_const_decl_unit_index);
        unit_tree->tree_data.unit_data.interface_const_decls = unit_consts;

        /* Extract var declarations */
        ListNode_t *unit_vars = extract_unit_nodes(
            var_list, unit_idx, get_var_decl_unit_index);
        unit_tree->tree_data.unit_data.interface_var_decls = unit_vars;

        /* Extract subprograms */
        ListNode_t *unit_subs = extract_unit_nodes(
            sub_list, unit_idx, get_subprogram_unit_index);
        unit_tree->tree_data.unit_data.subprograms = unit_subs;
    }
}

static void load_units_from_list(CompilationContext *comp_ctx, ListNode_t *uses, UnitSet *visited);


static void load_prelude_uses(CompilationContext *comp_ctx, Tree_t *prelude, UnitSet *visited)
{
    if (prelude == NULL || visited == NULL)
        return;
    if (prelude->type == TREE_PROGRAM_TYPE)
    {
        load_units_from_list(comp_ctx, prelude->tree_data.program_data.uses_units, visited);
    }
    else if (prelude->type == TREE_UNIT)
    {
        load_units_from_list(comp_ctx, prelude->tree_data.unit_data.interface_uses, visited);
        load_units_from_list(comp_ctx, prelude->tree_data.unit_data.implementation_uses, visited);
    }
}

static void load_unit(CompilationContext *comp_ctx, const char *unit_name, UnitSet *visited)
{
    Tree_t *program = comp_ctx->program;
    SymTab_t *symtab = comp_ctx->symtab;
    if (unit_name == NULL || visited == NULL)
        return;

    char *normalized = lowercase_copy(unit_name);
    if (normalized == NULL)
        return;

    if (strcmp(normalized, "gmp") == 0)
        g_requires_gmp = 1;

    if (!unit_set_add(visited, normalized))
        return;

    char *path = build_unit_path(unit_name);
    if (path == NULL && normalized != NULL && strcmp(unit_name, normalized) != 0)
        path = build_unit_path(normalized);
    if (path == NULL)
        return;
    fprintf(stderr, "Loading unit %s from %s\n", unit_name, path);
    report_rss("before loading unit");

    Tree_t *unit_tree = NULL;
    double start_time = 0.0;
    bool track_time = time_passes_flag();
    double profile_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;

    /* Check if this unit was pre-loaded in a batch-mode parent process.
     * If so, use the inherited Tree_t directly (via fork COW) instead of
     * re-parsing from the cache, saving ~0.3 s for system.pp. */
    PreloadedUnit *preloaded = find_preloaded_unit(normalized);
    if (preloaded != NULL)
    {
        unit_tree = preloaded->tree;
        /* Register the preprocessed source for error reporting in semcheck.
         * The pp_source buffer is inherited from the parent via COW. */
        (void)semcheck_register_source_buffer(preloaded->path,
                                              preloaded->pp_source,
                                              preloaded->pp_length);
        /* preprocessed_source/preprocessed_length globals are set by the
         * registration call above; point them at the preloaded buffer so
         * that later save/restore in the caller (compile_single_program)
         * picks up the right data. */
        preprocessed_source = preloaded->pp_source;
        preprocessed_length = preloaded->pp_length;
        /* Clear the preloaded slot so we don't re-use it if the same unit is
         * somehow requested again (shouldn't happen due to visited set). */
        preloaded->tree = NULL;
    }
    else
    {
        if (track_time)
            start_time = current_time_seconds();
        bool ok = parse_pascal_file(path, &unit_tree, true);
        if (track_time)
        {
            g_time_parse_units += current_time_seconds() - start_time;
            ++g_count_parse_units;
        }
        if (!ok)
        {
            free(path);
            fprintf(stderr, "ERROR: Failed to load unit '%s'.\n", unit_name);
            exit(1);
        }
    }

    if (unit_tree == NULL)
    {
        fprintf(stderr, "ERROR: Failed to convert unit '%s' into legacy parse tree.\n", unit_name);
        exit(1);
    }

    if (unit_tree->type != TREE_UNIT)
    {
        fprintf(stderr, "ERROR: %s is not a Pascal unit.\n", unit_name);
        destroy_tree(unit_tree);
        exit(1);
    }
    if (profile_pipeline_flag())
    {
        char stage[512];
        snprintf(stage, sizeof(stage), "load unit %s", unit_name);
        emit_profile_stage(stage, current_time_seconds() - profile_start);
    }

    load_units_from_list(comp_ctx, unit_tree->tree_data.unit_data.interface_uses, visited);
    load_units_from_list(comp_ctx, unit_tree->tree_data.unit_data.implementation_uses, visited);

    /* {$mode objfpc} and {$mode delphi} implicitly require the ObjPas unit */
    if (!pascal_identifier_equals(unit_tree->tree_data.unit_data.unit_id, "objpas") &&
        pascal_frontend_is_objfpc_mode())
    {
        load_unit(comp_ctx, "objpas", visited);
        int has_objpas = 0;
        for (ListNode_t *cur = unit_tree->tree_data.unit_data.interface_uses;
             cur != NULL; cur = cur->next)
        {
            if (cur->type == LIST_STRING && cur->cur != NULL &&
                pascal_identifier_equals((const char *)cur->cur, "ObjPas"))
            {
                has_objpas = 1;
                break;
            }
        }
        if (!has_objpas)
        {
            ListNode_t *objpas_node = CreateListNode(strdup("ObjPas"), LIST_STRING);
            if (objpas_node != NULL)
            {
                objpas_node->next = unit_tree->tree_data.unit_data.interface_uses;
                unit_tree->tree_data.unit_data.interface_uses = objpas_node;
            }
        }
    }

    /* Record dependency edges: this unit depends on each unit it uses */
    {
        int this_idx = unit_registry_add(unit_tree->tree_data.unit_data.unit_id);
        ListNode_t *dep;
        for (dep = unit_tree->tree_data.unit_data.interface_uses; dep != NULL; dep = dep->next)
        {
            if (dep->type == LIST_STRING && dep->cur != NULL)
                unit_registry_add_iface_dep(this_idx, unit_registry_add((const char *)dep->cur));
        }
        for (dep = unit_tree->tree_data.unit_data.implementation_uses; dep != NULL; dep = dep->next)
        {
            if (dep->type == LIST_STRING && dep->cur != NULL)
                unit_registry_add_dep(this_idx, unit_registry_add((const char *)dep->cur));
        }
        /* Every unit implicitly depends on System (Input, Output, etc.) */
        unit_registry_add_iface_dep(this_idx, unit_registry_add("System"));
    }

    /* Mark unit declarations BEFORE semcheck so defined_in_unit is set.
     * These functions just set flags on AST nodes, they don't touch the symbol table. */
    {
        int unit_idx = unit_registry_add(unit_tree->tree_data.unit_data.unit_id);
        mark_unit_type_decls(unit_tree->tree_data.unit_data.interface_type_decls, 1, unit_idx);
        mark_unit_const_decls(unit_tree->tree_data.unit_data.interface_const_decls, 1, unit_idx);
        mark_unit_var_decls(unit_tree->tree_data.unit_data.interface_var_decls, 1, unit_idx);
        if (program->type == TREE_PROGRAM_TYPE)
        {
            mark_unit_type_decls(unit_tree->tree_data.unit_data.implementation_type_decls, 0, unit_idx);
            mark_unit_const_decls(unit_tree->tree_data.unit_data.implementation_const_decls, 0, unit_idx);
            mark_unit_var_decls(unit_tree->tree_data.unit_data.implementation_var_decls, 0, unit_idx);
            mark_unit_subprograms(unit_tree->tree_data.unit_data.subprograms, unit_idx, 0);
        }
        else
        {
            mark_unit_subprograms(unit_tree->tree_data.unit_data.subprograms, unit_idx, 0);
        }
    }

    /* Lightweight per-unit semantic check: predeclares types, enums,
     * subprogram signatures, and trivial constants into the unit's scope.
     * Full processing (type resolution, body checking) is deferred to
     * semcheck_program() on the merged tree. */
    if (symtab != NULL)
    {
        char *saved_file_to_parse = file_to_parse;
        file_to_parse = path;
        int saved_source_buf_idx = semcheck_register_source_buffer(
            path, preprocessed_source, preprocessed_length);
        (void)saved_source_buf_idx;
        semcheck_unit_decls_only(symtab, unit_tree);
        LeaveScope(symtab);
        file_to_parse = saved_file_to_parse;
    }

    /* Store the unit in the loaded-units array.  build_combined_program_view()
     * will merge declarations into the target tree before semcheck. */
    {
        int unit_idx = unit_registry_add(unit_tree->tree_data.unit_data.unit_id);
        compilation_context_add_unit(comp_ctx, unit_tree, unit_idx);
        /* Record source path for cache key computation */
        comp_ctx->loaded_units[comp_ctx->loaded_unit_count - 1].source_path = path;
        path = NULL; /* ownership transferred */
    }
}

static void load_units_from_list(CompilationContext *comp_ctx, ListNode_t *uses, UnitSet *visited)
{
    ListNode_t *cur = uses;
    while (cur != NULL)
    {
        if (cur->type == LIST_STRING && cur->cur != NULL)
            load_unit(comp_ctx, (const char *)cur->cur, visited);
        cur = cur->next;
    }
}

#ifndef _WIN32
/* ---- Batch compilation mode ----
 * In batch mode, the compiler reads (input, output) pairs from stdin and
 * compiles each one in a forked child process.  The parent pre-loads the
 * prelude and common units (System, ObjPas) so that children inherit them
 * via copy-on-write, eliminating the most expensive per-test overhead
 * (tree_from_pascal_ast for system.pp takes ~0.3 s per invocation). */

/* We need sys/wait.h for waitpid/WIFEXITED but it transitively includes
 * ucontext.h which defines REG_R8..REG_R15 that clash with our codegen
 * register enum.  Work around by providing our own minimal declarations. */
extern pid_t waitpid(pid_t pid, int *status, int options);
#ifndef WIFEXITED
#define WIFEXITED(s)   (((s) & 0x7f) == 0)
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(s) (((s) & 0xff00) >> 8)
#endif

static int get_nproc(void)
{
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    return (n > 0) ? (int)n : 1;
}

/* Compile a single file.  This is the common compilation path used by both
 * normal (single-file) and batch mode.  It re-uses loaded units already in
 * g_comp_ctx when available (batch mode) or loads them fresh (normal mode).
 *
 * Returns 0 on success, non-zero on failure. */
static int compile_single_program(
    const char *input_file, const char *output_file,
    Tree_t *prelude_tree, char *prelude_path,
    bool use_stdlib, bool convert_to_tree,
    arena_t *arena, char **argv);

/* Wait for at least one child to finish.  Returns the number of active
 * children remaining, or -1 on error. */
static int reap_children(pid_t *children, int *statuses, int nchildren)
{
    int status;
    pid_t pid = waitpid(-1, &status, 0);
    if (pid <= 0)
        return nchildren;
    for (int i = 0; i < nchildren; i++)
    {
        if (children[i] == pid)
        {
            statuses[i] = WIFEXITED(status) ? WEXITSTATUS(status) : 1;
            /* Compact array */
            children[i] = children[nchildren - 1];
            statuses[i] = statuses[nchildren - 1];
            return nchildren - 1;
        }
    }
    return nchildren;
}

static int batch_mode_main(int argc, char **argv)
{
    int max_parallel = g_batch_max_parallel > 0 ? g_batch_max_parallel : get_nproc();
    bool use_stdlib = !g_skip_stdlib;

    /* 1. Set up cache directory */
    if (!g_ast_cache_explicit)
    {
        char *default_cache_dir = build_default_ast_cache_dir(argv[0]);
        if (default_cache_dir != NULL)
        {
            pascal_frontend_set_ast_cache_dir(default_cache_dir);
            free(default_cache_dir);
        }
    }

    /* 2. Resolve and parse prelude */
    pascal_frontend_reset_objfpc_mode();
    char *prelude_path = NULL;
    if (use_stdlib)
        prelude_path = resolve_stdlib_path(argv[0]);
    else
        prelude_path = resolve_prelude_path(argv[0]);
    if (prelude_path == NULL)
    {
        fprintf(stderr, "Error: Unable to locate %s for batch mode.\n",
                use_stdlib ? "system.p" : "prelude.p");
        return 1;
    }
    set_stdlib_loaded_flag(1);

    if (use_stdlib)
        unit_search_paths_set_vendor(&g_unit_paths, prelude_path);
    else
        unit_search_paths_disable_vendor(&g_unit_paths);

    Tree_t *prelude_tree = NULL;
    bool parsed_prelude = parse_pascal_file(prelude_path, &prelude_tree, true);
    if (!parsed_prelude || prelude_tree == NULL)
    {
        fprintf(stderr, "Error: Failed to parse prelude for batch mode.\n");
        free(prelude_path);
        return 1;
    }

    /* 3. Pre-load common units.  We call parse_pascal_file() for each unit,
     * which loads from AST cache, converts to Tree_t, and returns the tree.
     * We save the preprocessed source (needed for error reporting) from
     * the global that parse_pascal_file populates.  After fork(), children
     * inherit these via COW and skip the expensive tree_from_pascal_ast()
     * step (~0.3 s for system.pp). */
    {
        /* Try both "System" and "system" — Linux filesystems are case-sensitive */
        char *system_path = build_unit_path("System");
        if (system_path == NULL)
            system_path = build_unit_path("system");
        if (system_path != NULL && g_preloaded_unit_count < MAX_PRELOADED_UNITS)
        {
            Tree_t *tree = NULL;
            if (parse_pascal_file(system_path, &tree, true) && tree != NULL)
            {
                PreloadedUnit *pu = &g_preloaded_units[g_preloaded_unit_count++];
                pu->normalized_name = strdup("system");
                pu->tree = tree;
                pu->path = strdup(system_path);
                /* Save the preprocessed source before it gets overwritten */
                pu->pp_source = NULL;
                pu->pp_length = 0;
                if (preprocessed_source != NULL && preprocessed_length > 0)
                {
                    pu->pp_source = (char *)malloc(preprocessed_length + 1);
                    if (pu->pp_source != NULL)
                    {
                        memcpy(pu->pp_source, preprocessed_source, preprocessed_length);
                        pu->pp_source[preprocessed_length] = '\0';
                        pu->pp_length = preprocessed_length;
                    }
                }
                fprintf(stderr, "[batch] Pre-loaded System unit\n");
            }
            free(system_path);
        }
    }

    /* Pre-load ObjPas too (used by all {$mode objfpc} programs) */
    {
        char *objpas_path = build_unit_path("objpas");
        if (objpas_path != NULL && g_preloaded_unit_count < MAX_PRELOADED_UNITS)
        {
            Tree_t *tree = NULL;
            if (parse_pascal_file(objpas_path, &tree, true) && tree != NULL)
            {
                PreloadedUnit *pu = &g_preloaded_units[g_preloaded_unit_count++];
                pu->normalized_name = strdup("objpas");
                pu->tree = tree;
                pu->path = strdup(objpas_path);
                pu->pp_source = NULL;
                pu->pp_length = 0;
                if (preprocessed_source != NULL && preprocessed_length > 0)
                {
                    pu->pp_source = (char *)malloc(preprocessed_length + 1);
                    if (pu->pp_source != NULL)
                    {
                        memcpy(pu->pp_source, preprocessed_source, preprocessed_length);
                        pu->pp_source[preprocessed_length] = '\0';
                        pu->pp_length = preprocessed_length;
                    }
                }
                fprintf(stderr, "[batch] Pre-loaded ObjPas unit\n");
            }
            free(objpas_path);
        }
    }

    /* 4. Read batch entries from stdin and fork children */
    pid_t *children = calloc((size_t)max_parallel, sizeof(pid_t));
    int *child_statuses = calloc((size_t)max_parallel, sizeof(int));
    int active = 0;
    int total_failed = 0;
    int total_count = 0;

    char line[PATH_MAX * 2 + 16];
    arena_t *arena = arena_create(1024 * 1024);
    arena_set_global(arena);

    while (fgets(line, (int)sizeof(line), stdin) != NULL)
    {
        /* Strip trailing newline */
        size_t len = strlen(line);
        while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r'))
            line[--len] = '\0';
        if (len == 0)
            continue;

        /* Parse "input_file output_file" */
        char batch_input[PATH_MAX], batch_output[PATH_MAX];
        if (sscanf(line, "%s %s", batch_input, batch_output) != 2)
        {
            fprintf(stderr, "[batch] Malformed line, skipping: %s\n", line);
            continue;
        }

        /* Wait for a slot if at max parallel */
        while (active >= max_parallel)
        {
            active = reap_children(children, child_statuses, active);
        }

        pid_t pid = fork();
        if (pid < 0)
        {
            perror("[batch] fork");
            total_failed++;
            continue;
        }

        if (pid == 0)
        {
            /* ---- Child process ---- */
            /* Create fresh arena for the child (don't share parent's) */
            arena_t *child_arena = arena_create(1024 * 1024);
            arena_set_global(child_arena);

            /* Reset per-compilation state */
            pascal_frontend_reset_objfpc_mode();
            unit_registry_reset();
            g_requires_gmp = 0;

            int result = compile_single_program(
                batch_input, batch_output,
                prelude_tree, prelude_path,
                use_stdlib, true,
                child_arena, argv);

            _exit(result);
        }

        /* ---- Parent process ---- */
        children[active] = pid;
        child_statuses[active] = -1;
        active++;
        total_count++;
    }

    /* Wait for remaining children */
    while (active > 0)
    {
        int status;
        pid_t pid = waitpid(-1, &status, 0);
        if (pid <= 0)
            break;
        int code = WIFEXITED(status) ? WEXITSTATUS(status) : 1;
        if (code != 0)
            total_failed++;
        for (int i = 0; i < active; i++)
        {
            if (children[i] == pid)
            {
                children[i] = children[active - 1];
                active--;
                break;
            }
        }
    }

    free(children);
    free(child_statuses);

    fprintf(stderr, "[batch] Compiled %d files (%d failed)\n", total_count, total_failed);

    /* Parent cleanup */
    if (prelude_tree != NULL)
        destroy_tree(prelude_tree);
    free(prelude_path);
    pascal_frontend_cleanup();
    unit_search_paths_destroy(&g_unit_paths);
    arena_destroy(arena);

    return total_failed > 0 ? 1 : 0;
}
#endif /* !_WIN32 */

static void emit_link_args(void); /* forward declaration */
static void codegen_cache_check(void);
static void codegen_cache_populate(const char *asm_file);
static void codegen_cache_clear_transient_flags(void);

/* Compile a single Pascal program.  Used by both normal mode and batch mode.
 * Returns 0 on success, non-zero on error. */
static int compile_single_program(
    const char *input_file, const char *output_file,
    Tree_t *prelude_tree, char *prelude_path,
    bool use_stdlib, bool convert_to_tree,
    arena_t *arena, char **argv)
{
    (void)argv;
    file_to_parse = (char *)input_file;
    unit_search_paths_set_user(&g_unit_paths, input_file);

    double pipeline_total_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;

    /* Parse user source */
    Tree_t *user_tree = NULL;
    double user_profile_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    from_cparser_enable_pending_specializations();
    bool parsed_user = parse_pascal_file(input_file, &user_tree, convert_to_tree);
    from_cparser_disable_pending_specializations();
    emit_profile_stage("parse user source", current_time_seconds() - user_profile_start);
    if (!parsed_user || user_tree == NULL)
    {
        if (user_tree != NULL)
            destroy_tree(user_tree);
        return 1;
    }

    char *saved_preprocessed_source = NULL;
    size_t saved_preprocessed_length = 0;
    char *saved_preprocessed_path = NULL;
    if (preprocessed_source != NULL && preprocessed_length > 0)
    {
        saved_preprocessed_source = (char *)malloc(preprocessed_length + 1);
        if (saved_preprocessed_source != NULL)
        {
            memcpy(saved_preprocessed_source, preprocessed_source, preprocessed_length);
            saved_preprocessed_source[preprocessed_length] = '\0';
            saved_preprocessed_length = preprocessed_length;
        }
        if (preprocessed_path != NULL)
            saved_preprocessed_path = strdup(preprocessed_path);
    }

    if (user_tree->type == TREE_UNIT)
    {
        fprintf(stderr, "[batch] Batch mode does not support unit compilation (%s)\n", input_file);
        destroy_tree(user_tree);
        free(saved_preprocessed_source);
        free(saved_preprocessed_path);
        return 1;
    }

    /* Normal program compilation */
    ListNode_t *prelude_subs = NULL;
    ListNode_t *user_subs = user_tree->tree_data.program_data.subprograms;
    UnitSet visited_units;
    unit_set_init(&visited_units);
    SymTab_t *early_symtab = semcheck_init_symtab();

    compilation_context_init(&g_comp_ctx);
    g_comp_ctx.program = user_tree;
    g_comp_ctx.symtab = early_symtab;
    ListNode_t *user_types = user_tree->tree_data.program_data.type_declaration;
    ListNode_t *user_consts = user_tree->tree_data.program_data.const_declaration;
    ListNode_t *user_vars = user_tree->tree_data.program_data.var_declaration;
    user_tree->tree_data.program_data.type_declaration = NULL;
    user_tree->tree_data.program_data.const_declaration = NULL;
    user_tree->tree_data.program_data.var_declaration = NULL;

    if (prelude_tree != NULL)
    {
        double prelude_merge_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        prelude_subs = get_prelude_subprograms(prelude_tree);
        if (prelude_subs != NULL)
            mark_stdlib_var_params(prelude_subs);
        if (prelude_subs != NULL)
        {
            mark_unit_subprograms(prelude_subs, unit_registry_add("System"), 1);
            ListNode_t *last = prelude_subs;
            while (last->next != NULL)
                last = last->next;
            last->next = user_subs;
            user_tree->tree_data.program_data.subprograms = prelude_subs;
            clear_prelude_subprograms(prelude_tree);
        }

        ListNode_t *prelude_types = get_prelude_type_decls(prelude_tree);
        if (prelude_types != NULL)
        {
            mark_unit_type_decls(prelude_types, 1, unit_registry_add("System"));
            user_tree->tree_data.program_data.type_declaration =
                ConcatList(prelude_types, user_tree->tree_data.program_data.type_declaration);
            clear_prelude_type_decls(prelude_tree);
        }

        ListNode_t *prelude_consts = get_prelude_const_decls(prelude_tree);
        if (prelude_consts != NULL)
        {
            mark_unit_const_decls(prelude_consts, 1, unit_registry_add("System"));
            user_tree->tree_data.program_data.const_declaration =
                ConcatList(prelude_consts, user_tree->tree_data.program_data.const_declaration);
            clear_prelude_const_decls(prelude_tree);
        }

        ListNode_t *prelude_vars = get_prelude_var_decls(prelude_tree);
        if (prelude_vars != NULL)
        {
            mark_unit_var_decls(prelude_vars, 1, unit_registry_add("System"));
            user_tree->tree_data.program_data.var_declaration =
                ConcatList(prelude_vars, user_tree->tree_data.program_data.var_declaration);
            clear_prelude_var_decls(prelude_tree);
        }

        {
            int sys_idx = unit_registry_add("System");
            semcheck_predeclare_program_into_unit_scope(early_symtab, user_tree, sys_idx);
        }

        load_prelude_uses(&g_comp_ctx, prelude_tree, &visited_units);
        emit_profile_stage("program: merge prelude and prelude uses", current_time_seconds() - prelude_merge_start);
    }
    if (!use_stdlib)
    {
        double system_load_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        load_unit(&g_comp_ctx, "System", &visited_units);
        emit_profile_stage("program: auto-load System", current_time_seconds() - system_load_start);
    }
    double uses_load_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    load_units_from_list(&g_comp_ctx, user_tree->tree_data.program_data.uses_units, &visited_units);
    emit_profile_stage("program: load uses units", current_time_seconds() - uses_load_start);

    if (pascal_frontend_is_objfpc_mode())
    {
        double objpas_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        load_unit(&g_comp_ctx, "objpas", &visited_units);
        ListNode_t *objpas_node = CreateListNode(strdup("ObjPas"), LIST_STRING);
        if (objpas_node != NULL)
        {
            objpas_node->next = user_tree->tree_data.program_data.uses_units;
            user_tree->tree_data.program_data.uses_units = objpas_node;
        }
        emit_profile_stage("program: auto-load ObjPas", current_time_seconds() - objpas_start);
    }

    if (saved_preprocessed_source != NULL)
    {
        if (preprocessed_source != NULL)
            free(preprocessed_source);
        preprocessed_source = saved_preprocessed_source;
        preprocessed_length = saved_preprocessed_length;
        saved_preprocessed_source = NULL;
    }
    if (saved_preprocessed_path != NULL)
    {
        if (preprocessed_path != NULL)
            free(preprocessed_path);
        preprocessed_path = saved_preprocessed_path;
        saved_preprocessed_path = NULL;
    }
    file_to_parse = (char *)input_file;
    semcheck_set_source_path(input_file);
    semcheck_set_source_buffer(preprocessed_source, preprocessed_length);
    /* Reset {$H-} flag before semantic analysis.  The flag is only meaningful
     * during per-file AST conversion (from_cparser.c); the merged AST already
     * has the correct return types (SHORTSTRING_TYPE where appropriate). */
    pascal_frontend_set_default_shortstring(false);

    debug_check_type_presence(user_tree);
    report_rss("after loading all units");
    double combined_view_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    build_combined_program_view(&g_comp_ctx);
    emit_profile_stage("program: build combined view from loaded units", current_time_seconds() - combined_view_start);
    report_rss("after build_combined_program_view");
    compilation_context_set_active(&g_comp_ctx);

    double merge_user_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    user_tree->tree_data.program_data.type_declaration =
        ConcatList(user_tree->tree_data.program_data.type_declaration, user_types);
    user_tree->tree_data.program_data.const_declaration =
        ConcatList(user_tree->tree_data.program_data.const_declaration, user_consts);
    user_tree->tree_data.program_data.var_declaration =
        ConcatList(user_tree->tree_data.program_data.var_declaration, user_vars);
    emit_profile_stage("program: merge user declarations", current_time_seconds() - merge_user_start);
    double generic_alias_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    resolve_pending_generic_aliases(user_tree);
    emit_profile_stage("program: resolve generic aliases", current_time_seconds() - generic_alias_start);
    from_cparser_resolve_deferred_arrays(user_tree);
    double generic_method_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    append_generic_method_clones(user_tree);
    emit_profile_stage("program: append generic method clones", current_time_seconds() - generic_method_start);
    double generic_sub_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    resolve_pending_generic_subprograms(user_tree);
    emit_profile_stage("program: resolve generic subprograms", current_time_seconds() - generic_sub_start);

    unit_set_destroy(&visited_units);

    int frontend_errors = from_cparser_get_error_count();

    int sem_result = 0;
    double sem_profile_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    SymTab_t *symtab = start_semcheck_with_symtab(early_symtab, user_tree, &sem_result);
    emit_profile_stage("program: semantic analysis", current_time_seconds() - sem_profile_start);
    report_rss("after semantic analysis");

    sem_result += frontend_errors;

    int exit_code = 0;

    if (sem_result <= 0)
    {
        /* Check codegen cache after semcheck (units are fully loaded) */
        codegen_cache_check();

        fprintf(stderr, "Generating code to file: %s\n", output_file);

        CodeGenContext ctx;
        memset(&ctx, 0, sizeof(ctx));
        ctx.output_file = fopen(output_file, "w");
        if (ctx.output_file == NULL)
        {
            fprintf(stderr, "ERROR: Failed to open output file: %s\n", output_file);
            DestroySymTab(symtab);
            compilation_context_destroy(&g_comp_ctx);
            destroy_tree(user_tree);
            return 1;
        }
        ctx.label_counter = 1;
        ctx.write_label_counter = 1;
        ctx.symtab = symtab;
        ctx.target_abi = current_target_abi();
        ctx.had_error = 0;
        ctx.loop_frames = NULL;
        ctx.loop_depth = 0;
        ctx.loop_capacity = 0;

        /* Open cache .s file for unit function sections on cache miss */
        if (codegen_cache_miss_flag() && g_codegen_cache_asm_path[0] != '\0')
        {
            ctx.cache_output = fopen(g_codegen_cache_asm_path, "w");
            if (ctx.cache_output == NULL)
                fprintf(stderr, "WARNING: cannot open cache asm file: %s\n", g_codegen_cache_asm_path);
        }

        extern void mark_used_functions(Tree_t *program, SymTab_t *symtab);
        double mark_used_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        mark_used_functions(user_tree, symtab);
        emit_profile_stage("program: mark used functions (pass 1)", current_time_seconds() - mark_used_start);
        double mark_program_subs_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        mark_program_subs_used(user_tree);
        emit_profile_stage("program: mark program subprograms", current_time_seconds() - mark_program_subs_start);
        double mark_used_second_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        mark_used_functions(user_tree, symtab);
        emit_profile_stage("program: mark used functions (pass 2)", current_time_seconds() - mark_used_second_start);

        double unbuild_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        unbuild_combined_program_view(&g_comp_ctx);
        emit_profile_stage("program: unbuild combined view for codegen", current_time_seconds() - unbuild_start);

        double codegen_profile_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        codegen(user_tree, input_file, &ctx, symtab, &g_comp_ctx);
        emit_profile_stage("program: code generation", current_time_seconds() - codegen_profile_start);
        report_rss("after code generation");
        int codegen_failed = codegen_had_error(&ctx);
        fclose(ctx.output_file);
        if (ctx.cache_output != NULL)
            fclose(ctx.cache_output);
        if (codegen_failed && !codegen_cache_miss_flag())
        {
            fprintf(stderr, "Code generation failed; removing incomplete output file.\n");
            remove(output_file);
            if (g_codegen_cache_obj_path[0] != '\0')
                file_lock_release(g_codegen_cache_obj_path);
            exit_code = 1;
        }
        else
        {
            if (codegen_failed)
                fprintf(stderr, "WARNING: codegen had errors; keeping output for cache (--gc-sections will discard broken sections).\n");
            /* On cache miss, assemble the unit functions .s into the cache .o */
            if (!codegen_failed)
                codegen_cache_populate(g_codegen_cache_asm_path);
            emit_link_args();
        }
        /* Clear transient cache-mode flags so they don't leak into later compilations. */
        codegen_cache_clear_transient_flags();
    }
    else
    {
        fprintf(stderr, "Semantic analysis failed with %d error(s).\n", sem_result);
        exit_code = sem_result;
        if (g_codegen_cache_obj_path[0] != '\0')
            file_lock_release(g_codegen_cache_obj_path);
        codegen_cache_clear_transient_flags();
    }

    DestroySymTab(symtab);
    compilation_context_destroy(&g_comp_ctx);
    destroy_tree(user_tree);

    emit_profile_stage("total pipeline", current_time_seconds() - pipeline_total_start);
    return exit_code;
}

static void hash_string(unsigned long *hash, const char *s)
{
    for (const char *p = s; *p; p++)
        *hash = *hash * 33 + (unsigned char)*p;
    *hash = *hash * 33 + '\0';
}

/* Compute a cache key from flags, loaded unit names, unit source mtimes, and compiler mtime.
 * The key deliberately excludes the input/output file paths (positional args)
 * since the cached unit functions are the same regardless of which program uses them. */
static void codegen_cache_compute_key(char *key_buf, size_t key_buf_size)
{
    unsigned long hash = 5381;

    /* Hash only flag arguments (starting with -), skipping positional args
     * (input file, output file) and --codegen-cache-dir.  The positional args
     * are per-test and don't affect unit codegen output. */
    for (int i = 1; i < g_saved_argc; i++)
    {
        const char *arg = g_saved_argv[i];
        if (arg[0] != '-')
            continue; /* skip positional args (input file, output file) */
        if (strncmp(arg, "--codegen-cache-dir=", 20) == 0)
            continue;
        if (strncmp(arg, "--pp-cache-dir=", 15) == 0)
            continue;
        hash_string(&hash, arg);
    }

    /* Hash sorted unit names */
    int count = unit_registry_count();
    const char **names = calloc((size_t)(count > 0 ? count : 1), sizeof(const char *));
    int n = 0;
    for (int i = 1; i <= count; i++)
    {
        const char *name = unit_registry_get(i);
        if (name != NULL)
            names[n++] = name;
    }
    for (int i = 1; i < n; i++)
    {
        const char *tmp = names[i];
        int j = i - 1;
        while (j >= 0 && strcasecmp(names[j], tmp) > 0)
        {
            names[j + 1] = names[j];
            j--;
        }
        names[j + 1] = tmp;
    }
    for (int i = 0; i < n; i++)
    {
        const char *p = names[i];
        for (; *p; p++)
        {
            unsigned char c = (unsigned char)*p;
            if (c >= 'A' && c <= 'Z') c = c - 'A' + 'a';
            hash = hash * 33 + c;
        }
        hash = hash * 33 + '\0';
    }
    free(names);

    /* Hash unit source file mtimes */
    CompilationContext *ctx = compilation_context_get_active();
    if (ctx != NULL)
    {
        for (int i = 0; i < ctx->loaded_unit_count; i++)
        {
            const char *path = ctx->loaded_units[i].source_path;
            if (path != NULL)
            {
                struct stat st;
                if (stat(path, &st) == 0)
                    hash = hash * 33 + (unsigned long)st.st_mtime;
            }
        }
    }

    /* Hash include file mtimes (covers {$i ...} dependencies) */
    if (ctx != NULL)
    {
        for (int i = 0; i < ctx->include_file_count; i++)
        {
            const char *ipath = ctx->include_files[i];
            if (ipath != NULL)
            {
                struct stat st;
                if (stat(ipath, &st) == 0)
                    hash = hash * 33 + (unsigned long)st.st_mtime;
            }
        }
    }

    /* Include compiler binary mtime */
    {
        char exe_path[PATH_MAX];
        ssize_t len = get_executable_path(exe_path, sizeof(exe_path), NULL);
        if (len > 0)
        {
            struct stat st;
            if (stat(exe_path, &st) == 0)
                hash = hash * 33 + (unsigned long)st.st_mtime;
        }
    }

    snprintf(key_buf, key_buf_size, "%016lx", hash);
}

/* Check if a codegen cache entry exists for the current unit set.
 * Cache hit: enables skip-unit-codegen + function-sections.
 * Cache miss: enables function-sections + codegen_cache_miss_flag. */
static void codegen_cache_check(void)
{
    if (g_codegen_cache_dir == NULL)
        return;

    g_codegen_cache_forced_function_sections = 0;
    g_codegen_cache_forced_skip_unit_codegen = 0;

    char key[32];
    codegen_cache_compute_key(key, sizeof(key));
    snprintf(g_codegen_cache_obj_path, sizeof(g_codegen_cache_obj_path),
             "%s/%s.o", g_codegen_cache_dir, key);
    snprintf(g_codegen_cache_asm_path, sizeof(g_codegen_cache_asm_path),
             "%s/%s.s", g_codegen_cache_dir, key);

    /* If the cache is missing, wait for any existing lock (another process might be building it). */
    if (access(g_codegen_cache_obj_path, R_OK) != 0)
    {
        if (file_lock_acquire(g_codegen_cache_obj_path, 300))
        {
            /* Lock acquired. If the cache now exists, use it. */
            if (access(g_codegen_cache_obj_path, R_OK) == 0)
            {
                file_lock_release(g_codegen_cache_obj_path);
            }
            /* If it still doesn't exist, we hold the lock and will populate it later. */
        }
    }

    if (access(g_codegen_cache_obj_path, R_OK) == 0)
    {
        fprintf(stderr, "Codegen cache hit: %s\n", g_codegen_cache_obj_path);
        if (!skip_unit_codegen_flag())
        {
            set_skip_unit_codegen_flag();
            g_codegen_cache_forced_skip_unit_codegen = 1;
        }
        if (!function_sections_flag())
        {
            set_function_sections_flag();
            g_codegen_cache_forced_function_sections = 1;
        }
        g_codegen_cache_hit = 1;
    }
    else
    {
        fprintf(stderr, "Codegen cache miss; will populate: %s\n", g_codegen_cache_obj_path);
        if (!function_sections_flag())
        {
            set_function_sections_flag();
            g_codegen_cache_forced_function_sections = 1;
        }
        set_codegen_cache_miss_flag();
        g_codegen_cache_hit = 0;
    }
}

static void codegen_cache_clear_transient_flags(void)
{
    clear_codegen_cache_miss_flag();
    if (g_codegen_cache_forced_skip_unit_codegen)
        clear_skip_unit_codegen_flag();
    if (g_codegen_cache_forced_function_sections)
        clear_function_sections_flag();
    g_codegen_cache_forced_skip_unit_codegen = 0;
    g_codegen_cache_forced_function_sections = 0;
}

/* Replace broken sections in cache .s with ud2 stubs.
 * Reads error line numbers from `as` stderr, finds which .section/.text.X
 * blocks contain those lines, and replaces their content with a ud2 stub.
 * Returns 1 if patching was done, 0 if nothing to patch. */
static int codegen_cache_patch_broken_sections(const char *cache_asm_file,
                                                const char *err_output)
{
    /* Parse error line numbers from as output like "file.s:123: Error: ..." */
    int error_lines[512];
    int num_errors = 0;
    const char *p = err_output;
    while (p != NULL && num_errors < 512)
    {
        const char *colon = strstr(p, ".s:");
        if (colon == NULL)
            break;
        colon += 3; /* skip ".s:" */
        int line = atoi(colon);
        if (line > 0)
            error_lines[num_errors++] = line;
        p = strchr(colon, '\n');
    }
    if (num_errors == 0)
        return 0;

    /* Read the .s file */
    FILE *f = fopen(cache_asm_file, "r");
    if (f == NULL)
        return 0;
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *content = malloc((size_t)fsize + 1);
    if (content == NULL) { fclose(f); return 0; }
    fsize = (long)fread(content, 1, (size_t)fsize, f);
    content[fsize] = '\0';
    fclose(f);

    /* Split into lines */
    int line_count = 1;
    for (long i = 0; i < fsize; i++)
        if (content[i] == '\n') line_count++;

    /* For each line, record its offset and find section boundaries */
    /* A section starts at ".section\t.text.NAME" and ends at the next section directive */
    /* Mark sections containing error lines */
    int *line_offsets = calloc((size_t)(line_count + 1), sizeof(int));
    if (line_offsets == NULL) { free(content); return 0; }
    int cur_line = 1;
    line_offsets[1] = 0;
    for (long i = 0; i < fsize; i++)
    {
        if (content[i] == '\n')
        {
            cur_line++;
            if (cur_line <= line_count)
                line_offsets[cur_line] = (int)(i + 1);
        }
    }

    /* Find section starts and mark broken ones */
    typedef struct { int start_line; int end_line; char name[256]; int broken; } Section;
    Section *sections = calloc((size_t)line_count, sizeof(Section));
    int num_sections = 0;
    cur_line = 1;
    for (long i = 0; i < fsize; )
    {
        /* Find end of this line */
        long eol = i;
        while (eol < fsize && content[eol] != '\n') eol++;

        if (strncmp(content + i, "\t.section\t.text.", 16) == 0)
        {
            /* Extract section name */
            const char *name_start = content + i + 16;
            const char *comma = strchr(name_start, ',');
            size_t name_len = comma ? (size_t)(comma - name_start) : (size_t)(eol - (long)(name_start - content));
            if (name_len >= 256) name_len = 255;

            if (num_sections > 0)
                sections[num_sections - 1].end_line = cur_line - 1;

            sections[num_sections].start_line = cur_line;
            memcpy(sections[num_sections].name, name_start, name_len);
            sections[num_sections].name[name_len] = '\0';
            sections[num_sections].broken = 0;
            num_sections++;
        }

        i = eol + 1;
        cur_line++;
    }
    if (num_sections > 0)
        sections[num_sections - 1].end_line = line_count;

    /* Mark sections that contain error lines */
    for (int e = 0; e < num_errors; e++)
    {
        for (int s = 0; s < num_sections; s++)
        {
            if (error_lines[e] >= sections[s].start_line &&
                error_lines[e] <= sections[s].end_line)
            {
                sections[s].broken = 1;
                break;
            }
        }
    }

    /* Rewrite the file, replacing broken sections with ud2 stubs */
    f = fopen(cache_asm_file, "w");
    if (f == NULL) { free(content); free(line_offsets); free(sections); return 0; }

    int patched = 0;
    cur_line = 1;
    for (long i = 0; i < fsize; )
    {
        long eol = i;
        while (eol < fsize && content[eol] != '\n') eol++;

        /* Check if this line starts a broken section */
        int in_broken = 0;
        int broken_idx = -1;
        for (int s = 0; s < num_sections; s++)
        {
            if (cur_line >= sections[s].start_line &&
                cur_line <= sections[s].end_line &&
                sections[s].broken)
            {
                in_broken = 1;
                broken_idx = s;
                break;
            }
        }

        if (in_broken && cur_line == sections[broken_idx].start_line)
        {
            /* Emit ud2 stub for this section */
            fprintf(f,
                "\t.section\t.text.%s,\"ax\",@progbits\n"
                "\t.globl\t%s\n"
                "%s:\n"
                "\tud2\n",
                sections[broken_idx].name,
                sections[broken_idx].name,
                sections[broken_idx].name);
            /* Skip all lines until end of this section */
            while (cur_line < sections[broken_idx].end_line)
            {
                i = eol + 1;
                eol = i;
                while (eol < fsize && content[eol] != '\n') eol++;
                cur_line++;
            }
            patched = 1;
        }
        else if (!in_broken)
        {
            fwrite(content + i, 1, (size_t)(eol - i), f);
            fputc('\n', f);
        }

        i = eol + 1;
        cur_line++;
    }

    fclose(f);
    free(content);
    free(line_offsets);
    free(sections);
    return patched;
}

/* After a cache-miss compile, assemble the unit-functions-only .s into the cache .o.
 * The cache_asm_file contains only unit subprogram .text.* sections (written by
 * codegen via ctx->cache_output).  No main, no vars, no init/finalization.
 * If assembly fails due to broken inline asm, those sections are replaced with
 * ud2 stubs and assembly is retried.  --gc-sections removes them at link time. */
static void codegen_cache_populate(const char *cache_asm_file)
{
    if (g_codegen_cache_dir == NULL || g_codegen_cache_hit)
        return;
    if (cache_asm_file == NULL || cache_asm_file[0] == '\0')
        return;

    char err_file[4200];
    snprintf(err_file, sizeof(err_file), "%s.err", cache_asm_file);

    char cmd[8500];
    snprintf(cmd, sizeof(cmd), "as '%s' -o '%s' 2>'%s'",
             cache_asm_file, g_codegen_cache_obj_path, err_file);
    int rc = system(cmd);

    if (rc != 0)
    {
        /* Read error output and try to patch broken sections */
        FILE *ef = fopen(err_file, "r");
        char err_buf[8192] = {0};
        if (ef != NULL)
        {
            size_t n = fread(err_buf, 1, sizeof(err_buf) - 1, ef);
            err_buf[n] = '\0';
            fclose(ef);
        }

        if (codegen_cache_patch_broken_sections(cache_asm_file, err_buf))
        {
            /* Retry assembly with patched file */
            snprintf(cmd, sizeof(cmd), "as '%s' -o '%s' 2>/dev/null",
                     cache_asm_file, g_codegen_cache_obj_path);
            rc = system(cmd);
        }

        if (rc != 0)
        {
            fprintf(stderr, "WARNING: codegen cache: as failed (rc=%d), cache not populated\n", rc);
            remove(g_codegen_cache_obj_path);
            g_codegen_cache_obj_path[0] = '\0';
            remove(err_file);
            return;
        }
    }

    remove(err_file);
    fprintf(stderr, "Codegen cache populated: %s\n", g_codegen_cache_obj_path);
    file_lock_release(g_codegen_cache_obj_path);
}

static void emit_link_args(void)
{
    if (!g_emit_link_args)
        return;

    char buffer[4096];
    size_t used = 0;

    /* On cache hit, include the cached .o and gc-sections.
     * On cache miss, the main .s has all unit functions (marked used for cache
     * completeness), so gc-sections is needed to strip unused ones with bad refs.
     * The cache .o is only populated for future runs, not linked this time. */
    if (g_codegen_cache_hit && g_codegen_cache_obj_path[0] != '\0')
    {
        used += (size_t)snprintf(buffer + used, sizeof(buffer) - used,
                                 " %s -Wl,--gc-sections",
                                 g_codegen_cache_obj_path);
    }
    else if (g_codegen_cache_dir != NULL)
    {
        /* Cache miss: --function-sections was forced, so gc-sections is needed
         * to remove unused unit functions that have unresolvable references. */
        used += (size_t)snprintf(buffer + used, sizeof(buffer) - used,
                                 " -Wl,--gc-sections");
    }

    if (!target_windows_flag())
    {
        used += (size_t)snprintf(buffer + used, sizeof(buffer) - used, " -lm");
    }

    /* Runtime requires pthread (thread manager in runtime_fpc_assign.c).
     * On MSYS2/Windows, winpthreads provides this. */
    used += (size_t)snprintf(buffer + used, sizeof(buffer) - used, " -lpthread");

    if (g_requires_gmp)
    {
        used += (size_t)snprintf(buffer + used, sizeof(buffer) - used, " -lgmp");
    }

    if (used > 0)
        fprintf(stderr, "KGPC_LINK_ARGS:%s\n", buffer);
    else
        fprintf(stderr, "KGPC_LINK_ARGS:\n");
}

int main(int argc, char **argv)
{
    install_stack_trace_handler();
    g_saved_argc = argc;
    g_saved_argv = argv;

        // Initialize global arena with 1MB blocks
    arena_t* arena = arena_create(1024 * 1024);
    arena_set_global(arena);

    /* Process ALL arguments through set_flags first, then extract positional args.
     * This allows flags like --no-stdlib to appear anywhere in the command line. */
    unit_search_paths_init(&g_unit_paths);
    if (argc > 1)
    {
        SetFlagsResult flag_result = set_flags(argv + 1, argc - 1);
        if (flag_result == SET_FLAGS_HELP)
        {
            print_usage(argv[0]);
            clear_dump_ast_path();
            unit_search_paths_destroy(&g_unit_paths);
            arena_destroy(arena);
            return 0;
        }
    }

    /* Record compiler binary mtime for AST cache invalidation */
    {
        char exe_path[PATH_MAX];
        ssize_t exe_len = get_executable_path(exe_path, sizeof(exe_path), argv[0]);
        if (exe_len > 0)
        {
            struct stat exe_st;
            if (stat(exe_path, &exe_st) == 0)
                pascal_frontend_set_compiler_mtime(exe_st.st_mtime);
        }
    }

#ifndef _WIN32
    /* Batch mode: compile multiple files from stdin, sharing pre-loaded units */
    if (g_batch_mode)
    {
        arena_destroy(arena);
        int result = batch_mode_main(argc, argv);
        return result;
    }
#endif

    /* Extract positional arguments (non-flag args) as input and output files */
    const char *input_file = NULL;
    const char *output_file = NULL;
    for (int i = 1; i < argc; ++i)
    {
        const char *a = argv[i];
        /* Skip flags and their consumed arguments */
        if (a[0] == '-')
        {
            /* Skip flags that consume the next argument */
            if ((strcmp(a, "--target") == 0 || strcmp(a, "-target") == 0 ||
                 strcmp(a, "--dump-ast") == 0 || strcmp(a, "-dump-ast") == 0 ||
                 strcmp(a, "--target-abi") == 0) && i + 1 < argc)
                ++i;
            continue;
        }
        if (input_file == NULL)
            input_file = a;
        else if (output_file == NULL)
            output_file = a;
    }

    if (input_file == NULL)
    {
        print_usage(argv[0]);
        clear_dump_ast_path();
        unit_search_paths_destroy(&g_unit_paths);
        return 1;
    }

    /* If no output file given, derive from input file (replace extension with .s) */
    char derived_output[PATH_MAX];
    if (output_file == NULL)
    {
        const char *dot = strrchr(input_file, '.');
        const char *slash = strrchr(input_file, '/');
        if (dot != NULL && (slash == NULL || dot > slash))
        {
            size_t base_len = (size_t)(dot - input_file);
            if (base_len + 3 < sizeof(derived_output))
            {
                memcpy(derived_output, input_file, base_len);
                memcpy(derived_output + base_len, ".s", 3);
                output_file = derived_output;
            }
        }
        if (output_file == NULL)
        {
            fprintf(stderr, "Error: Could not derive output file name from '%s'.\n", input_file);
            clear_dump_ast_path();
            unit_search_paths_destroy(&g_unit_paths);
            return 1;
        }
    }

    file_to_parse = (char *)input_file;  /* Set global for error reporting */
    unit_search_paths_set_user(&g_unit_paths, input_file);

    if (time_passes_flag())
        atexit(emit_timing_summary);

    pascal_frontend_reset_objfpc_mode();
    double pipeline_total_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;

    bool use_stdlib = !g_skip_stdlib;
    char *prelude_path = NULL;
    if (use_stdlib)
        prelude_path = resolve_stdlib_path(argv[0]);
    else
        prelude_path = resolve_prelude_path(argv[0]);
    if (prelude_path == NULL)
    {
        fprintf(stderr, "Error: Unable to locate %s. Set KGPC_STDLIB or run from the project root.\n",
            use_stdlib ? "system.p" : "prelude.p");
        clear_dump_ast_path();
        unit_search_paths_destroy(&g_unit_paths);
        arena_destroy(arena);
        return 1;
    }
    set_stdlib_loaded_flag(1);

    if (!g_ast_cache_explicit)
    {
        char *default_cache_dir = build_default_ast_cache_dir(argv[0]);
        if (default_cache_dir != NULL)
        {
            pascal_frontend_set_ast_cache_dir(default_cache_dir);
            free(default_cache_dir);
        }
    }

    bool parse_only = parse_only_flag();
    bool convert_to_tree = !parse_only || dump_ast_path() != NULL;

    Tree_t *prelude_tree = NULL;
    bool track_time = time_passes_flag();
    {
        double stdlib_start = track_time ? current_time_seconds() : 0.0;
        double stdlib_profile_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        bool parsed_stdlib = parse_pascal_file(prelude_path, &prelude_tree, convert_to_tree);
        if (track_time)
        {
            g_time_parse_stdlib += current_time_seconds() - stdlib_start;
            ++g_count_parse_stdlib;
        }
        emit_profile_stage(use_stdlib ? "parse stdlib/prelude" : "parse prelude", current_time_seconds() - stdlib_profile_start);
        if (!parsed_stdlib)
        {
            if (prelude_tree != NULL)
                destroy_tree(prelude_tree);
            free(prelude_path);
            clear_dump_ast_path();
            pascal_frontend_cleanup();
            unit_search_paths_destroy(&g_unit_paths);
            return 1;
        }

        if (use_stdlib)
            unit_search_paths_set_vendor(&g_unit_paths, prelude_path);
        else
            unit_search_paths_disable_vendor(&g_unit_paths);
    }

    Tree_t *user_tree = NULL;
    double user_start = track_time ? current_time_seconds() : 0.0;
    double user_profile_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    from_cparser_enable_pending_specializations();
    bool parsed_user = parse_pascal_file(input_file, &user_tree, convert_to_tree);
    from_cparser_disable_pending_specializations();
    if (track_time)
    {
        g_time_parse_user += current_time_seconds() - user_start;
        ++g_count_parse_user;
    }
    emit_profile_stage("parse user source", current_time_seconds() - user_profile_start);
    if (!parsed_user)
    {
        if (prelude_tree != NULL)
            destroy_tree(prelude_tree);
        if (user_tree != NULL)
            destroy_tree(user_tree);
        free(prelude_path);
        clear_dump_ast_path();
        pascal_frontend_cleanup();
        unit_search_paths_destroy(&g_unit_paths);
        return 1;
    }

    char *saved_preprocessed_source = NULL;
    size_t saved_preprocessed_length = 0;
    char *saved_preprocessed_path = NULL;
    if (preprocessed_source != NULL && preprocessed_length > 0)
    {
        saved_preprocessed_source = (char *)malloc(preprocessed_length + 1);
        if (saved_preprocessed_source != NULL)
        {
            memcpy(saved_preprocessed_source, preprocessed_source, preprocessed_length);
            saved_preprocessed_source[preprocessed_length] = '\0';
            saved_preprocessed_length = preprocessed_length;
        }
        if (preprocessed_path != NULL)
            saved_preprocessed_path = strdup(preprocessed_path);
    }

    if (!dump_ast_to_requested_path(user_tree))
    {
        if (prelude_tree != NULL)
            destroy_tree(prelude_tree);
        if (user_tree != NULL)
            destroy_tree(user_tree);
        free(prelude_path);
        clear_dump_ast_path();
        if (ast_nil != NULL)
        {
            free(ast_nil);
            ast_nil = NULL;
        }
        pascal_frontend_cleanup();
        unit_search_paths_destroy(&g_unit_paths);
        if (saved_preprocessed_source != NULL)
            free(saved_preprocessed_source);
        if (saved_preprocessed_path != NULL)
            free(saved_preprocessed_path);
        return 1;
    }

    if (parse_only)
    {
        FILE *out = fopen(output_file, "w");
        if (out == NULL)
        {
            fprintf(stderr, "ERROR: Failed to open output file: %s\n", output_file);
            if (prelude_tree != NULL)
                destroy_tree(prelude_tree);
            if (user_tree != NULL)
                destroy_tree(user_tree);
            free(prelude_path);
            clear_dump_ast_path();
            pascal_frontend_cleanup();
            unit_search_paths_destroy(&g_unit_paths);
            return 1;
        }
        fprintf(stderr, "Parse-only mode: skipping semantic analysis and code generation.\n");
        fprintf(out, "; parse-only mode: no code generated\n");
        fclose(out);
        if (prelude_tree != NULL)
            destroy_tree(prelude_tree);
        if (user_tree != NULL)
            destroy_tree(user_tree);
        free(prelude_path);
        if (ast_nil != NULL)
        {
            free(ast_nil);
            ast_nil = NULL;
        }
        clear_dump_ast_path();
        pascal_frontend_cleanup();
        unit_search_paths_destroy(&g_unit_paths);
        if (saved_preprocessed_source != NULL)
            free(saved_preprocessed_source);
        if (saved_preprocessed_path != NULL)
            free(saved_preprocessed_path);
        return 0;
    }

    if (user_tree == NULL || prelude_tree == NULL)
    {
        if (prelude_tree != NULL)
            destroy_tree(prelude_tree);
        if (user_tree != NULL)
            destroy_tree(user_tree);
        free(prelude_path);
        clear_dump_ast_path();
        pascal_frontend_cleanup();
        unit_search_paths_destroy(&g_unit_paths);
        return 1;
    }

    /* Handle unit compilation separately from program compilation */
    if (user_tree->type == TREE_UNIT)
    {
        fprintf(stderr, "Compiling unit: %s\n", user_tree->tree_data.unit_data.unit_id);
        
        if (prelude_tree != NULL)
        {
            /* For units, we still need to merge prelude for built-in functions and types */
            ListNode_t *prelude_subs = get_prelude_subprograms(prelude_tree);
            if (prelude_subs != NULL)
                mark_stdlib_var_params(prelude_subs);
            if (prelude_subs != NULL)
            {
                int sys_idx = unit_registry_add("System");
                mark_unit_subprograms(prelude_subs, sys_idx, 1);
                ListNode_t *last = prelude_subs;
                while (last->next != NULL)
                    last = last->next;
                last->next = user_tree->tree_data.unit_data.subprograms;
                user_tree->tree_data.unit_data.subprograms = prelude_subs;
                clear_prelude_subprograms(prelude_tree);
            }

            /* Skip merging prelude types/consts/vars when compiling the System unit itself,
             * since System defines its own core types and constants. */
            int is_system_unit = pascal_identifier_equals(user_tree->tree_data.unit_data.unit_id, "System");

            /* Merge prelude types into unit for FPC compatibility (SizeInt, PAnsiChar, etc.).
             * For System itself, keep prelude types unmarked so real System declarations
             * take precedence while still providing missing core aliases (e.g. Currency). */
            ListNode_t *prelude_types = get_prelude_type_decls(prelude_tree);
            if (prelude_types != NULL)
            {
                if (!is_system_unit)
                    mark_unit_type_decls(prelude_types, 1, unit_registry_add("System"));
                user_tree->tree_data.unit_data.interface_type_decls =
                    ConcatList(prelude_types, user_tree->tree_data.unit_data.interface_type_decls);
                clear_prelude_type_decls(prelude_tree);
            }

            /* Merge prelude constants into unit for FPC compatibility (fmClosed, fmInput, etc.) */
            ListNode_t *prelude_consts = get_prelude_const_decls(prelude_tree);
            if (prelude_consts != NULL && !is_system_unit)
            {
                mark_unit_const_decls(prelude_consts, 1, unit_registry_add("System"));  /* Mark as exported from unit */
                user_tree->tree_data.unit_data.interface_const_decls =
                    ConcatList(prelude_consts, user_tree->tree_data.unit_data.interface_const_decls);
                clear_prelude_const_decls(prelude_tree);
            }

            /* Merge prelude variables into unit */
            ListNode_t *prelude_vars = get_prelude_var_decls(prelude_tree);
            if (prelude_vars != NULL && !is_system_unit)
            {
                mark_unit_var_decls(prelude_vars, 1, unit_registry_add("System"));  /* Mark as exported from unit */
                user_tree->tree_data.unit_data.interface_var_decls =
                    ConcatList(prelude_vars, user_tree->tree_data.unit_data.interface_var_decls);
                clear_prelude_var_decls(prelude_tree);
            }
        }

        /* Load used units */
        UnitSet visited_units;
        unit_set_init(&visited_units);
        double unit_import_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;

        /* Set up compilation context for unit compilation.
         * symtab is NULL here -- unit compilation doesn't do per-unit
         * declaration pre-population during load. */
        CompilationContext unit_comp_ctx;
        compilation_context_init(&unit_comp_ctx);
        unit_comp_ctx.program = user_tree;
        unit_comp_ctx.symtab = NULL;

        /* Mark the current unit as visited so circular dependencies
         * (e.g. types.pp → Math → types.pp) don't re-import it. */
        if (user_tree->tree_data.unit_data.unit_id != NULL)
        {
            char *self_lower = lowercase_copy(user_tree->tree_data.unit_data.unit_id);
            if (self_lower != NULL)
                unit_set_add(&visited_units, self_lower);
        }

        /* NOTE: For FPC compatibility, system types (SizeInt, PAnsiChar, etc.)
         * are now included in the system prelude and available to all compilations.
         * Explicit system unit loading is not needed. */

        /* Load units from interface and implementation uses clauses */
        if (!use_stdlib &&
            !pascal_identifier_equals(user_tree->tree_data.unit_data.unit_id, "System"))
        {
            double system_load_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
            load_unit(&unit_comp_ctx, "System", &visited_units);
            emit_profile_stage("unit compile: auto-load System", current_time_seconds() - system_load_start);
        }
        double interface_uses_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        load_units_from_list(&unit_comp_ctx, user_tree->tree_data.unit_data.interface_uses, &visited_units);
        emit_profile_stage("unit compile: load interface uses", current_time_seconds() - interface_uses_start);
        double implementation_uses_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        load_units_from_list(&unit_comp_ctx, user_tree->tree_data.unit_data.implementation_uses, &visited_units);
        emit_profile_stage("unit compile: load implementation uses", current_time_seconds() - implementation_uses_start);

        /* If {$MODE objfpc} was detected, automatically load ObjPas unit.
         * This makes types like TEndian available without explicit 'uses objpas'.
         * Also add ObjPas to the uses list so that unit-qualified references work. */
        if (pascal_frontend_is_objfpc_mode())
        {
            double objpas_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
            load_unit(&unit_comp_ctx, "objpas", &visited_units);
            /* Add ObjPas to interface_uses for semcheck_is_unit_name to recognize it */
            ListNode_t *objpas_node = CreateListNode(strdup("ObjPas"), LIST_STRING);
            if (objpas_node != NULL)
            {
                objpas_node->next = user_tree->tree_data.unit_data.interface_uses;
                user_tree->tree_data.unit_data.interface_uses = objpas_node;
            }
            emit_profile_stage("unit compile: auto-load ObjPas", current_time_seconds() - objpas_start);
        }
        
        /* Register the target unit's own dependencies so that
         * unit_registry_is_dep() works for qualified type resolution
         * (e.g. baseunix.stat where baseunix is a direct dependency). */
        {
            int target_idx = unit_registry_add(user_tree->tree_data.unit_data.unit_id);
            ListNode_t *dep;
            for (dep = user_tree->tree_data.unit_data.interface_uses; dep != NULL; dep = dep->next)
            {
                if (dep->type == LIST_STRING && dep->cur != NULL)
                    unit_registry_add_iface_dep(target_idx, unit_registry_add((const char *)dep->cur));
            }
            for (dep = user_tree->tree_data.unit_data.implementation_uses; dep != NULL; dep = dep->next)
            {
                if (dep->type == LIST_STRING && dep->cur != NULL)
                    unit_registry_add_dep(target_idx, unit_registry_add((const char *)dep->cur));
            }
            unit_registry_add_iface_dep(target_idx, unit_registry_add("System"));
        }

        debug_check_type_presence(user_tree);
        emit_profile_stage("unit compile: total imports", current_time_seconds() - unit_import_start);
        unit_set_destroy(&visited_units);

        /* Build the combined view: merge imported unit declarations into
         * the target unit tree so semcheck can see them. */
        build_combined_program_view(&unit_comp_ctx);

        /* Flush deferred inline specializations and emit generic method clones
         * for unit compilation.  Unlike program compilation which calls
         * append_generic_method_clones() on ALL type declarations,
         * flush_deferred_inline_specializations() only emits clones for the
         * newly created inline specializations (avoiding duplicates with
         * method clones already present in imported units). */
        flush_deferred_inline_specializations(user_tree);

        if (saved_preprocessed_source != NULL)
        {
            if (preprocessed_source != NULL)
                free(preprocessed_source);
            preprocessed_source = saved_preprocessed_source;
            preprocessed_length = saved_preprocessed_length;
            saved_preprocessed_source = NULL;
            saved_preprocessed_length = 0;
        }
        if (saved_preprocessed_path != NULL)
        {
            if (preprocessed_path != NULL)
                free(preprocessed_path);
            preprocessed_path = saved_preprocessed_path;
            saved_preprocessed_path = NULL;
        }
        file_to_parse = (char *)input_file;
        semcheck_set_source_path(input_file);
        semcheck_set_source_buffer(preprocessed_source, preprocessed_length);
        pascal_frontend_set_default_shortstring(false);

        int sem_result = 0;
        double sem_start = track_time ? current_time_seconds() : 0.0;
        double sem_profile_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        SymTab_t *symtab = start_semcheck(user_tree, &sem_result);
        if (track_time)
            g_time_semantic += current_time_seconds() - sem_start;
        emit_profile_stage("unit compile: semantic analysis", current_time_seconds() - sem_profile_start);

        /* Restore imported declarations from the unit tree back to their
         * unit records so codegen only sees the target unit's own decls. */
        unbuild_combined_program_view(&unit_comp_ctx);
        compilation_context_destroy(&unit_comp_ctx);

        if (sem_result > 0)
        {
            fprintf(stderr, "Semantic check failed for unit.\n");
            DestroySymTab(symtab);
            if (prelude_tree != NULL)
                destroy_tree(prelude_tree);
            destroy_tree(user_tree);
            free(prelude_path);
            clear_dump_ast_path();
            pascal_frontend_cleanup();
            unit_search_paths_destroy(&g_unit_paths);
            if (saved_preprocessed_source != NULL)
                free(saved_preprocessed_source);
            if (saved_preprocessed_path != NULL)
                free(saved_preprocessed_path);
            return 1;
        }

        fprintf(stderr, "Generating code for unit to file: %s\n", output_file);
        
        CodeGenContext ctx;
        memset(&ctx, 0, sizeof(ctx));
        ctx.output_file = fopen(output_file, "w");
        if (ctx.output_file == NULL)
        {
            fprintf(stderr, "ERROR: Failed to open output file: %s\n", output_file);
            DestroySymTab(symtab);
            if (prelude_tree != NULL)
                destroy_tree(prelude_tree);
            destroy_tree(user_tree);
            free(prelude_path);
            clear_dump_ast_path();
            unit_search_paths_destroy(&g_unit_paths);
            return 1;
        }
        ctx.label_counter = 1;
        ctx.write_label_counter = 1;
        ctx.symtab = symtab;
        ctx.target_abi = current_target_abi();
        ctx.had_error = 0;
        ctx.loop_frames = NULL;
        ctx.loop_depth = 0;
        ctx.loop_capacity = 0;
        
        double codegen_start = track_time ? current_time_seconds() : 0.0;
        double codegen_profile_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        codegen_unit(user_tree, input_file, &ctx, symtab);
        if (track_time)
            g_time_codegen += current_time_seconds() - codegen_start;
        emit_profile_stage("unit compile: code generation", current_time_seconds() - codegen_profile_start);
        
        int codegen_failed = codegen_had_error(&ctx);
        fclose(ctx.output_file);
        
        if (codegen_failed)
        {
            fprintf(stderr, "ERROR: Code generation failed for unit.\n");
            DestroySymTab(symtab);
            if (prelude_tree != NULL)
                destroy_tree(prelude_tree);
            destroy_tree(user_tree);
            free(prelude_path);
            clear_dump_ast_path();
            pascal_frontend_cleanup();
            unit_search_paths_destroy(&g_unit_paths);
            if (saved_preprocessed_source != NULL)
                free(saved_preprocessed_source);
            if (saved_preprocessed_path != NULL)
                free(saved_preprocessed_path);
            return 1;
        }
        
        DestroySymTab(symtab);
        if (prelude_tree != NULL)
            destroy_tree(prelude_tree);
        destroy_tree(user_tree);
        free(prelude_path);
        if (ast_nil != NULL)
        {
            free(ast_nil);
            ast_nil = NULL;
        }
        clear_dump_ast_path();
        pascal_frontend_cleanup();
        unit_search_paths_destroy(&g_unit_paths);
        arena_destroy(arena);
        emit_profile_stage("total pipeline", current_time_seconds() - pipeline_total_start);
        return 0;
    }

    /* Normal program compilation continues below */
    ListNode_t *prelude_subs = NULL;
    ListNode_t *user_subs = user_tree->tree_data.program_data.subprograms;
    UnitSet visited_units;
    unit_set_init(&visited_units);
    /* Create symtab early for per-unit declaration pre-population during load */
    SymTab_t *early_symtab = semcheck_init_symtab();

    /* Initialise the compilation context for program compilation */
    compilation_context_init(&g_comp_ctx);
    g_comp_ctx.program = user_tree;
    g_comp_ctx.symtab = early_symtab;
    ListNode_t *user_types = user_tree->tree_data.program_data.type_declaration;
    ListNode_t *user_consts = user_tree->tree_data.program_data.const_declaration;
    ListNode_t *user_vars = user_tree->tree_data.program_data.var_declaration;
    user_tree->tree_data.program_data.type_declaration = NULL;
    user_tree->tree_data.program_data.const_declaration = NULL;
    user_tree->tree_data.program_data.var_declaration = NULL;
    if (prelude_tree != NULL)
    {
        double prelude_merge_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;

        /* System/prelude is parsed as TREE_PROGRAM_TYPE (not TREE_UNIT), so
         * its declarations are merged directly into the program tree below.
         * semcheck_program() processes them -- only declarations from units
         * loaded via load_unit() (marked fully checked) are skipped. */

        prelude_subs = get_prelude_subprograms(prelude_tree);
        if (prelude_subs != NULL)
            mark_stdlib_var_params(prelude_subs);
        if (prelude_subs != NULL)
        {
            /* Mark prelude (system.p) subprograms as library procedures so they don't
             * incorrectly get static links when merged into user programs */
            mark_unit_subprograms(prelude_subs, unit_registry_add("System"), 1);

            ListNode_t *last = prelude_subs;
            while (last->next != NULL)
                last = last->next;
            last->next = user_subs;
            user_tree->tree_data.program_data.subprograms = prelude_subs;
            clear_prelude_subprograms(prelude_tree);
        }

        /* Merge prelude types into program for FPC compatibility (SizeInt, PAnsiChar, etc.) */
        ListNode_t *prelude_types = get_prelude_type_decls(prelude_tree);
        if (prelude_types != NULL)
        {
            mark_unit_type_decls(prelude_types, 1, unit_registry_add("System"));
            user_tree->tree_data.program_data.type_declaration =
                ConcatList(prelude_types, user_tree->tree_data.program_data.type_declaration);
            clear_prelude_type_decls(prelude_tree);
        }

        /* Merge prelude constants into program */
        ListNode_t *prelude_consts = get_prelude_const_decls(prelude_tree);
        if (prelude_consts != NULL)
        {
            mark_unit_const_decls(prelude_consts, 1, unit_registry_add("System"));
            user_tree->tree_data.program_data.const_declaration =
                ConcatList(prelude_consts, user_tree->tree_data.program_data.const_declaration);
            clear_prelude_const_decls(prelude_tree);
        }

        /* Merge prelude variables into program */
        ListNode_t *prelude_vars = get_prelude_var_decls(prelude_tree);
        if (prelude_vars != NULL)
        {
            /* Mark prelude vars as coming from a unit, so they're recognized
             * as available for const expressions like Ord(DirectorySeparator) */
            mark_unit_var_decls(prelude_vars, 1, unit_registry_add("System"));
            user_tree->tree_data.program_data.var_declaration =
                ConcatList(prelude_vars, user_tree->tree_data.program_data.var_declaration);
            clear_prelude_var_decls(prelude_tree);
        }

        /* Predeclare System's types, enums, subprogram signatures, and trivial
         * constants into System's unit scope.  This must happen BEFORE loading
         * other units (which depend on System through scope deps). */
        {
            int sys_idx = unit_registry_add("System");
            semcheck_predeclare_program_into_unit_scope(early_symtab, user_tree, sys_idx);
        }

        load_prelude_uses(&g_comp_ctx, prelude_tree, &visited_units);
        emit_profile_stage("program: merge prelude and prelude uses", current_time_seconds() - prelude_merge_start);
    }
    if (!use_stdlib)
    {
        double system_load_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        load_unit(&g_comp_ctx, "System", &visited_units);
        emit_profile_stage("program: auto-load System", current_time_seconds() - system_load_start);
    }
    double uses_load_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    load_units_from_list(&g_comp_ctx, user_tree->tree_data.program_data.uses_units, &visited_units);
    emit_profile_stage("program: load uses units", current_time_seconds() - uses_load_start);

    /* If {$MODE objfpc} was detected, automatically load ObjPas unit.
     * This makes types like TEndian available without explicit 'uses objpas'.
     * Also add ObjPas to the uses list so that unit-qualified references
     * like ObjPas.TEndian work correctly. */
    if (pascal_frontend_is_objfpc_mode())
    {
        double objpas_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        load_unit(&g_comp_ctx, "objpas", &visited_units);
        /* Add ObjPas to uses list for semcheck_is_unit_name to recognize it */
        ListNode_t *objpas_node = CreateListNode(strdup("ObjPas"), LIST_STRING);
        if (objpas_node != NULL)
        {
            objpas_node->next = user_tree->tree_data.program_data.uses_units;
            user_tree->tree_data.program_data.uses_units = objpas_node;
        }
        emit_profile_stage("program: auto-load ObjPas", current_time_seconds() - objpas_start);
    }

    if (saved_preprocessed_source != NULL)
    {
        if (preprocessed_source != NULL)
            free(preprocessed_source);
        preprocessed_source = saved_preprocessed_source;
        preprocessed_length = saved_preprocessed_length;
        saved_preprocessed_source = NULL;
        saved_preprocessed_length = 0;
    }
    if (saved_preprocessed_path != NULL)
    {
        if (preprocessed_path != NULL)
            free(preprocessed_path);
        preprocessed_path = saved_preprocessed_path;
        saved_preprocessed_path = NULL;
    }
    file_to_parse = (char *)input_file;
    semcheck_set_source_path(input_file);
    semcheck_set_source_buffer(preprocessed_source, preprocessed_length);
    pascal_frontend_set_default_shortstring(false);

    debug_check_type_presence(user_tree);
    /* Build the combined program view from loaded unit records.
     * Loaded units are stored in the context during load_unit() and merged
     * here into the program tree so semcheck sees all declarations. */
    double combined_view_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    build_combined_program_view(&g_comp_ctx);
    emit_profile_stage("program: build combined view from loaded units", current_time_seconds() - combined_view_start);

    /* Make the context available to passes that don't receive it as a parameter */
    compilation_context_set_active(&g_comp_ctx);

    double merge_user_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    user_tree->tree_data.program_data.type_declaration =
        ConcatList(user_tree->tree_data.program_data.type_declaration, user_types);
    user_tree->tree_data.program_data.const_declaration =
        ConcatList(user_tree->tree_data.program_data.const_declaration, user_consts);
    user_tree->tree_data.program_data.var_declaration =
        ConcatList(user_tree->tree_data.program_data.var_declaration, user_vars);
    emit_profile_stage("program: merge user declarations", current_time_seconds() - merge_user_start);
    double generic_alias_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    resolve_pending_generic_aliases(user_tree);
    emit_profile_stage("program: resolve generic aliases", current_time_seconds() - generic_alias_start);
    from_cparser_resolve_deferred_arrays(user_tree);
    flush_deferred_inline_specializations(user_tree);
    double generic_method_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    append_generic_method_clones(user_tree);
    emit_profile_stage("program: append generic method clones", current_time_seconds() - generic_method_start);
    double generic_sub_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    resolve_pending_generic_subprograms(user_tree);
    emit_profile_stage("program: resolve generic subprograms", current_time_seconds() - generic_sub_start);

    unit_set_destroy(&visited_units);

    /* Check for frontend (parser/tree conversion) errors */
    int frontend_errors = from_cparser_get_error_count();

    int sem_result = 0;
    double sem_start = track_time ? current_time_seconds() : 0.0;
    double sem_profile_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
    SymTab_t *symtab = start_semcheck_with_symtab(early_symtab, user_tree, &sem_result);
    if (track_time)
        g_time_semantic += current_time_seconds() - sem_start;
    emit_profile_stage("program: semantic analysis", current_time_seconds() - sem_profile_start);

    /* Add frontend errors to semantic result */
    sem_result += frontend_errors;

    int exit_code = 0;

    if (sem_result <= 0)
    {
        /* Check codegen cache after semcheck (units are fully loaded) */
        codegen_cache_check();

        fprintf(stderr, "Generating code to file: %s\n", output_file);

        CodeGenContext ctx;
        memset(&ctx, 0, sizeof(ctx));
        ctx.output_file = fopen(output_file, "w");
        if (ctx.output_file == NULL)
        {
            fprintf(stderr, "ERROR: Failed to open output file: %s\n", output_file);
            DestroySymTab(symtab);
            if (prelude_tree != NULL)
                destroy_tree(prelude_tree);
            compilation_context_destroy(&g_comp_ctx);
            destroy_tree(user_tree);
            clear_dump_ast_path();
            unit_search_paths_destroy(&g_unit_paths);
            return 1;
        }
        ctx.label_counter = 1;
        ctx.write_label_counter = 1;
        ctx.symtab = symtab;
        ctx.target_abi = current_target_abi();
        ctx.had_error = 0;
        ctx.loop_frames = NULL;
        ctx.loop_depth = 0;
        ctx.loop_capacity = 0;

        /* Open cache .s file for unit function sections on cache miss */
        if (codegen_cache_miss_flag() && g_codegen_cache_asm_path[0] != '\0')
        {
            ctx.cache_output = fopen(g_codegen_cache_asm_path, "w");
            if (ctx.cache_output == NULL)
                fprintf(stderr, "WARNING: cannot open cache asm file: %s\n", g_codegen_cache_asm_path);
        }

        /* Mark which functions are actually used (dead code elimination) */
        extern void mark_used_functions(Tree_t *program, SymTab_t *symtab);
        double mark_used_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        mark_used_functions(user_tree, symtab);
        emit_profile_stage("program: mark used functions (pass 1)", current_time_seconds() - mark_used_start);
        double mark_program_subs_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        mark_program_subs_used(user_tree);
        emit_profile_stage("program: mark program subprograms", current_time_seconds() - mark_program_subs_start);
        /* Run mark_used again to discover functions called by newly-marked subprograms
         * (e.g., inherited methods in specialized generics) */
        double mark_used_second_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        mark_used_functions(user_tree, symtab);
        emit_profile_stage("program: mark used functions (pass 2)", current_time_seconds() - mark_used_second_start);

        /* Restore unit declarations from merged program lists back to unit
         * records so that codegen emits imported code from unit records. */
        double unbuild_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        unbuild_combined_program_view(&g_comp_ctx);
        emit_profile_stage("program: unbuild combined view for codegen", current_time_seconds() - unbuild_start);

        double codegen_start = track_time ? current_time_seconds() : 0.0;
        double codegen_profile_start = profile_pipeline_flag() ? current_time_seconds() : 0.0;
        codegen(user_tree, input_file, &ctx, symtab, &g_comp_ctx);
        if (track_time)
            g_time_codegen += current_time_seconds() - codegen_start;
        emit_profile_stage("program: code generation", current_time_seconds() - codegen_profile_start);
        int codegen_failed = codegen_had_error(&ctx);
        fclose(ctx.output_file);
        if (ctx.cache_output != NULL)
            fclose(ctx.cache_output);
        if (codegen_failed && !codegen_cache_miss_flag())
        {
            fprintf(stderr, "Code generation failed; removing incomplete output file.\n");
            remove(output_file);
            if (g_codegen_cache_obj_path[0] != '\0')
                file_lock_release(g_codegen_cache_obj_path);
            exit_code = 1;
        }
        else
        {
            if (codegen_failed)
                fprintf(stderr, "WARNING: codegen had errors; keeping output for cache (--gc-sections will discard broken sections).\n");
            /* On cache miss, assemble the unit functions .s into the cache .o */
            if (!codegen_failed)
                codegen_cache_populate(g_codegen_cache_asm_path);
            emit_link_args();
        }
        /* Clear transient cache-mode flags so they don't leak into later compilations. */
        codegen_cache_clear_transient_flags();
    }
    else
    {
        fprintf(stderr, "Semantic analysis failed with %d error(s).\n", sem_result);
        exit_code = sem_result;
        if (g_codegen_cache_obj_path[0] != '\0')
            file_lock_release(g_codegen_cache_obj_path);
        codegen_cache_clear_transient_flags();
    }

    DestroySymTab(symtab);
    if (prelude_tree != NULL)
        destroy_tree(prelude_tree);
    compilation_context_destroy(&g_comp_ctx);
    destroy_tree(user_tree);
    free(prelude_path);

    if (ast_nil != NULL)
    {
        free(ast_nil);
        ast_nil = NULL;
    }

    if (sem_result > 0)
    {
        clear_dump_ast_path();
        pascal_frontend_cleanup();
        unit_search_paths_destroy(&g_unit_paths);
        unit_registry_reset();
        arena_destroy(arena);
        return exit_code > 0 ? exit_code : 1;
    }

    clear_dump_ast_path();
    pascal_frontend_cleanup();
    unit_search_paths_destroy(&g_unit_paths);
    unit_registry_reset();
    arena_destroy(arena);
    emit_profile_stage("total pipeline", current_time_seconds() - pipeline_total_start);
    return exit_code;
}
