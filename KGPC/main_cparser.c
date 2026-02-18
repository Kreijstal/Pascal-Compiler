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
#endif
#include <ctype.h>
#include <stdbool.h>
#include <time.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#include "parser.h"
#include "combinators.h"
#include "pascal_parser.h"
#include "pascal_declaration.h"

#include "flags.h"
#include "Parser/ParseTree/tree.h"
#include "Parser/ParseTree/from_cparser.h"
#include "Parser/pascal_frontend.h"
#include "Parser/SemanticCheck/SemCheck.h"
#include "CodeGenerator/Intel_x86-64/codegen.h"
#include "stacktrace.h"
#include "unit_paths.h"
#include "arena.h"
#include "identifier_utils.h"

#ifdef _WIN32
#include <windows.h>
#include <io.h>
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
static int g_requires_pthread = 0;
static int g_requires_gmp = 0;
static int g_emit_link_args = 0;

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
    fprintf(stderr, "Usage: %s <input.p> <output.s> [flags]\n", prog_name);
    fprintf(stderr, "  Compiles Pascal source to x86-64 assembly\n");
    fprintf(stderr, "  Flags:\n");
    fprintf(stderr, "    -O1, -O2              Enable optimizations\n");
    fprintf(stderr, "    -non-local            Enable non-local variable chasing (experimental)\n");
    fprintf(stderr, "    --target=windows      Generate assembly for the Windows x64 ABI\n");
    fprintf(stderr, "    --target=sysv         Generate assembly for the System V AMD64 ABI\n");
    fprintf(stderr, "    --dump-ast=<file>     Write the parsed AST to <file>\n");
    fprintf(stderr, "    --time-passes         Print timing information for major compiler stages\n");
    fprintf(stderr, "    --asm-debug           Annotate emitted assembly with semantic/codegen info\n");
    fprintf(stderr, "    --disable-dce         Emit unused subprograms (debugging)\n");
    fprintf(stderr, "    -I<path>              Add include path for preprocessor\n");
    fprintf(stderr, "    -Fu<path>             Add unit search path (FPC compatible)\n");
    fprintf(stderr, "    --no-vendor-units     Disable built-in KGPC vendor units\n");
    fprintf(stderr, "    --no-stdlib           Disable KGPC stdlib; load minimal prelude instead\n");
    fprintf(stderr, "    -D<symbol>[=<value>]  Define preprocessor symbol\n");
    fprintf(stderr, "    -Us                   Compile System unit (FPC compatible)\n");
    fprintf(stderr, "    -Sg                   Enable goto statements (FPC compatible)\n");
}

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
    const char *env = getenv("KGPC_STDLIB");
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

static void set_flags(char **optional_args, int count)
{
    int i = 0;
    while (count > 0)
    {
        const char *arg = optional_args[i];
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
        else
        {
            fprintf(stderr, "ERROR: Unrecognized flag: %s\n", arg);
            exit(1);
        }

        --count;
        ++i;
    }
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

static void append_initialization_statement(Tree_t *program, struct Statement *init_stmt)
{
    if (program == NULL || init_stmt == NULL)
        return;

    struct Statement *body = program->tree_data.program_data.body_statement;
    if (body == NULL || body->type != STMT_COMPOUND_STATEMENT)
    {
        destroy_stmt(init_stmt);
        return;
    }

    if (init_stmt->type != STMT_COMPOUND_STATEMENT)
    {
        destroy_stmt(init_stmt);
        return;
    }

    ListNode_t *init_list = init_stmt->stmt_data.compound_statement;
    if (init_list == NULL)
    {
        destroy_stmt(init_stmt);
        return;
    }

    // Prepend initialization to the body (so it runs BEFORE the main program body)
    if (body->stmt_data.compound_statement == NULL)
        body->stmt_data.compound_statement = init_list;
    else
        body->stmt_data.compound_statement = ConcatList(init_list, body->stmt_data.compound_statement);

    init_stmt->stmt_data.compound_statement = NULL;
    destroy_stmt(init_stmt);
}

static void mark_unit_subprograms(ListNode_t *sub_list)
{
    ListNode_t *node = sub_list;
    while (node != NULL)
    {
        if (node->type == LIST_TREE && node->cur != NULL)
        {
            Tree_t *sub = (Tree_t *)node->cur;
            if (sub->type == TREE_SUBPROGRAM)
                sub->tree_data.subprogram_data.defined_in_unit = 1;
        }
        node = node->next;
    }
}

static void mark_unit_type_decls(ListNode_t *type_list, int is_public)
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
            }
        }
        node = node->next;
    }
}

static void mark_unit_const_decls(ListNode_t *const_list, int is_public)
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
            }
        }
        node = node->next;
    }
}

static void mark_unit_var_decls(ListNode_t *var_list, int is_public)
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
            }
            else if (decl->type == TREE_ARR_DECL)
            {
                decl->tree_data.arr_decl_data.defined_in_unit = 1;
                decl->tree_data.arr_decl_data.unit_is_public = is_public ? 1 : 0;
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

static ListNode_t **get_finalization_list(Tree_t *target)
{
    assert(target != NULL);
    if (target->type == TREE_PROGRAM_TYPE)
        return &target->tree_data.program_data.finalization_statements;
    /* Units don't accumulate finalization statements from other units */
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
    const char *check_id = getenv("KGPC_DEBUG_CHECK_TYPE");
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

/* Merges a loaded unit's declarations into the target tree.
 * The target can be either a TREE_PROGRAM_TYPE or TREE_UNIT. */
static void merge_unit_into_target(Tree_t *target, Tree_t *unit_tree)
{
    if (target == NULL || unit_tree == NULL)
        return;
    
    assert(target->type == TREE_PROGRAM_TYPE || target->type == TREE_UNIT);
    
    ListNode_t **type_list = get_type_decl_list(target);
    ListNode_t **const_list = get_const_decl_list(target);
    ListNode_t **var_list = get_var_decl_list(target);
    ListNode_t **sub_list = get_subprograms_list(target);
    ListNode_t **final_list = get_finalization_list(target);
    
    assert(type_list != NULL);
    assert(const_list != NULL);
    assert(var_list != NULL);
    assert(sub_list != NULL);

    mark_unit_type_decls(unit_tree->tree_data.unit_data.interface_type_decls, 1);
    if (getenv("KGPC_DEBUG_TFPG") != NULL) {
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
    /* Append imported types so dependencies stay ahead of later units. */
    *type_list = ConcatList(*type_list, unit_tree->tree_data.unit_data.interface_type_decls);
    unit_tree->tree_data.unit_data.interface_type_decls = NULL;

    mark_unit_const_decls(unit_tree->tree_data.unit_data.interface_const_decls, 1);
    if (getenv("KGPC_DEBUG_CONST") != NULL) {
        ListNode_t *dbg = unit_tree->tree_data.unit_data.interface_const_decls;
        while (dbg != NULL) {
            if (dbg->type == LIST_TREE) {
                Tree_t *decl = (Tree_t *)dbg->cur;
                if (decl != NULL && decl->type == TREE_CONST_DECL &&
                    decl->tree_data.const_decl_data.id != NULL)
                {
                    fprintf(stderr, "[KGPC] merging interface const '%s' from unit '%s' into target type %d\n",
                            decl->tree_data.const_decl_data.id,
                            unit_tree->tree_data.unit_data.unit_id != NULL ?
                                unit_tree->tree_data.unit_data.unit_id : "<unknown>",
                            target->type);
                }
            }
            dbg = dbg->next;
        }
    }
    /* Append imported constants so dependencies are available first. */
    *const_list = ConcatList(*const_list, unit_tree->tree_data.unit_data.interface_const_decls);
    unit_tree->tree_data.unit_data.interface_const_decls = NULL;

    mark_unit_var_decls(unit_tree->tree_data.unit_data.interface_var_decls, 1);
    *var_list = ConcatList(*var_list, unit_tree->tree_data.unit_data.interface_var_decls);
    unit_tree->tree_data.unit_data.interface_var_decls = NULL;

    mark_unit_type_decls(unit_tree->tree_data.unit_data.implementation_type_decls, 0);
    if (getenv("KGPC_DEBUG_TFPG") != NULL) {
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
    /* Append implementation types to keep dependencies ahead of targets. */
    *type_list = ConcatList(*type_list, unit_tree->tree_data.unit_data.implementation_type_decls);
    unit_tree->tree_data.unit_data.implementation_type_decls = NULL;

    mark_unit_const_decls(unit_tree->tree_data.unit_data.implementation_const_decls, 0);
    /* Append implementation constants to keep dependencies ahead of targets. */
    *const_list = ConcatList(*const_list, unit_tree->tree_data.unit_data.implementation_const_decls);
    unit_tree->tree_data.unit_data.implementation_const_decls = NULL;

    mark_unit_var_decls(unit_tree->tree_data.unit_data.implementation_var_decls, 0);
    *var_list = ConcatList(*var_list, unit_tree->tree_data.unit_data.implementation_var_decls);
    unit_tree->tree_data.unit_data.implementation_var_decls = NULL;

    mark_unit_subprograms(unit_tree->tree_data.unit_data.subprograms);
    *sub_list = ConcatList(*sub_list, unit_tree->tree_data.unit_data.subprograms);
    unit_tree->tree_data.unit_data.subprograms = NULL;

    /* Only programs accumulate initialization/finalization */
    if (target->type == TREE_PROGRAM_TYPE) {
        append_initialization_statement(target, unit_tree->tree_data.unit_data.initialization);
        unit_tree->tree_data.unit_data.initialization = NULL;

        // Prepend finalization to the list (for LIFO execution order)
        if (unit_tree->tree_data.unit_data.finalization != NULL && final_list != NULL) {
            ListNode_t *final_node = CreateListNode(unit_tree->tree_data.unit_data.finalization, LIST_STMT);
            if (final_node != NULL) {
                final_node->next = *final_list;
                *final_list = final_node;
            }
            unit_tree->tree_data.unit_data.finalization = NULL;
        }
    }
}

/* Legacy wrapper for backward compatibility */
static void merge_unit_into_program(Tree_t *program, Tree_t *unit_tree)
{
    merge_unit_into_target(program, unit_tree);
}

static void load_units_from_list(Tree_t *program, ListNode_t *uses, UnitSet *visited);

static void load_prelude_uses(Tree_t *program, Tree_t *prelude, UnitSet *visited)
{
    if (prelude == NULL || visited == NULL)
        return;
    if (prelude->type == TREE_PROGRAM_TYPE)
    {
        load_units_from_list(program, prelude->tree_data.program_data.uses_units, visited);
    }
    else if (prelude->type == TREE_UNIT)
    {
        load_units_from_list(program, prelude->tree_data.unit_data.interface_uses, visited);
        load_units_from_list(program, prelude->tree_data.unit_data.implementation_uses, visited);
    }
}

static void load_unit(Tree_t *program, const char *unit_name, UnitSet *visited)
{
    if (unit_name == NULL || visited == NULL)
        return;

    char *normalized = lowercase_copy(unit_name);
    if (normalized == NULL)
        return;

    if (strcmp(normalized, "cthreads") == 0)
        g_requires_pthread = 1;
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

    Tree_t *unit_tree = NULL;
    double start_time = 0.0;
    bool track_time = time_passes_flag();
    if (track_time)
        start_time = current_time_seconds();
    bool ok = parse_pascal_file(path, &unit_tree, true);
    if (track_time)
    {
        g_time_parse_units += current_time_seconds() - start_time;
        ++g_count_parse_units;
    }
    free(path);
    if (!ok)
    {
        fprintf(stderr, "ERROR: Failed to load unit '%s'.\n", unit_name);
        exit(1);
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

    load_units_from_list(program, unit_tree->tree_data.unit_data.interface_uses, visited);
    load_units_from_list(program, unit_tree->tree_data.unit_data.implementation_uses, visited);

    merge_unit_into_program(program, unit_tree);
    destroy_tree(unit_tree);
}

static void load_units_from_list(Tree_t *program, ListNode_t *uses, UnitSet *visited)
{
    ListNode_t *cur = uses;
    while (cur != NULL)
    {
        if (cur->type == LIST_STRING && cur->cur != NULL)
            load_unit(program, (const char *)cur->cur, visited);
        cur = cur->next;
    }
}

static void emit_link_args(void)
{
    if (!g_emit_link_args)
        return;

    char buffer[256];
    size_t used = 0;

    if (!target_windows_flag())
    {
        used += (size_t)snprintf(buffer + used, sizeof(buffer) - used, " -lm");
    }

    if (g_requires_pthread && !target_windows_flag())
    {
        used += (size_t)snprintf(buffer + used, sizeof(buffer) - used, " -lpthread");
    }

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

    // Initialize global arena with 1MB blocks
    arena_t* arena = arena_create(1024 * 1024);
    arena_set_global(arena);

    if (argc < 3)
    {
        print_usage(argv[0]);
        clear_dump_ast_path();
        return 1;
    }

    const char *input_file = argv[1];
    file_to_parse = (char *)input_file;  /* Set global for error reporting */
    unit_search_paths_init(&g_unit_paths);
    unit_search_paths_set_user(&g_unit_paths, input_file);
    const char *output_file = argv[2];

    int optional_count = argc - 3;
    if (optional_count > 0)
        set_flags(argv + 3, optional_count);

    if (time_passes_flag())
        atexit(emit_timing_summary);

    pascal_frontend_reset_objfpc_mode();

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

    bool parse_only = parse_only_flag();
    bool convert_to_tree = !parse_only || dump_ast_path() != NULL;

    Tree_t *prelude_tree = NULL;
    bool track_time = time_passes_flag();
    {
        double stdlib_start = track_time ? current_time_seconds() : 0.0;
        bool parsed_stdlib = parse_pascal_file(prelude_path, &prelude_tree, convert_to_tree);
        if (track_time)
        {
            g_time_parse_stdlib += current_time_seconds() - stdlib_start;
            ++g_count_parse_stdlib;
        }
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
    from_cparser_enable_pending_specializations();
    bool parsed_user = parse_pascal_file(input_file, &user_tree, convert_to_tree);
    from_cparser_disable_pending_specializations();
    if (track_time)
    {
        g_time_parse_user += current_time_seconds() - user_start;
        ++g_count_parse_user;
    }
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
                mark_unit_subprograms(prelude_subs);
                ListNode_t *last = prelude_subs;
                while (last->next != NULL)
                    last = last->next;
                last->next = user_tree->tree_data.unit_data.subprograms;
                user_tree->tree_data.unit_data.subprograms = prelude_subs;
                clear_prelude_subprograms(prelude_tree);
            }
            
            /* Merge prelude types into unit for FPC compatibility (SizeInt, PAnsiChar, etc.) */
            ListNode_t *prelude_types = get_prelude_type_decls(prelude_tree);
            if (prelude_types != NULL)
            {
                mark_unit_type_decls(prelude_types, 1);  /* Mark as exported from unit */
                user_tree->tree_data.unit_data.interface_type_decls =
                    ConcatList(prelude_types, user_tree->tree_data.unit_data.interface_type_decls);
                clear_prelude_type_decls(prelude_tree);
            }

            /* Skip merging prelude constants/vars when compiling the System unit itself,
             * since System defines its own DirectorySeparator, IsLibrary, etc. */
            int is_system_unit = pascal_identifier_equals(user_tree->tree_data.unit_data.unit_id, "System");

            /* Merge prelude constants into unit for FPC compatibility (fmClosed, fmInput, etc.) */
            ListNode_t *prelude_consts = get_prelude_const_decls(prelude_tree);
            if (prelude_consts != NULL && !is_system_unit)
            {
                mark_unit_const_decls(prelude_consts, 1);  /* Mark as exported from unit */
                user_tree->tree_data.unit_data.interface_const_decls =
                    ConcatList(prelude_consts, user_tree->tree_data.unit_data.interface_const_decls);
                clear_prelude_const_decls(prelude_tree);
            }

            /* Merge prelude variables into unit */
            ListNode_t *prelude_vars = get_prelude_var_decls(prelude_tree);
            if (prelude_vars != NULL && !is_system_unit)
            {
                mark_unit_var_decls(prelude_vars, 1);  /* Mark as exported from unit */
                user_tree->tree_data.unit_data.interface_var_decls =
                    ConcatList(prelude_vars, user_tree->tree_data.unit_data.interface_var_decls);
                clear_prelude_var_decls(prelude_tree);
            }
        }

        /* Load used units */
        UnitSet visited_units;
        unit_set_init(&visited_units);
        
        /* NOTE: For FPC compatibility, system types (SizeInt, PAnsiChar, etc.)
         * are now included in the system prelude and available to all compilations.
         * Explicit system unit loading is not needed. */
        
        /* Load units from interface and implementation uses clauses */
        if (!use_stdlib &&
            !pascal_identifier_equals(user_tree->tree_data.unit_data.unit_id, "System"))
        {
            load_unit(user_tree, "System", &visited_units);
        }
        load_units_from_list(user_tree, user_tree->tree_data.unit_data.interface_uses, &visited_units);
        load_units_from_list(user_tree, user_tree->tree_data.unit_data.implementation_uses, &visited_units);
        
        /* If {$MODE objfpc} was detected, automatically load ObjPas unit.
         * This makes types like TEndian available without explicit 'uses objpas'.
         * Also add ObjPas to the uses list so that unit-qualified references work. */
        if (pascal_frontend_is_objfpc_mode())
        {
            load_unit(user_tree, "objpas", &visited_units);
            /* Add ObjPas to interface_uses for semcheck_is_unit_name to recognize it */
            ListNode_t *objpas_node = CreateListNode(strdup("ObjPas"), LIST_STRING);
            if (objpas_node != NULL)
            {
                objpas_node->next = user_tree->tree_data.unit_data.interface_uses;
                user_tree->tree_data.unit_data.interface_uses = objpas_node;
            }
        }
        
        debug_check_type_presence(user_tree);
        unit_set_destroy(&visited_units);

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

        int sem_result = 0;
        double sem_start = track_time ? current_time_seconds() : 0.0;
        if (g_skip_stdlib)
            setenv("KGPC_SKIP_IMPORTED_IMPL_BODIES", "1", 1);
        else
            unsetenv("KGPC_SKIP_IMPORTED_IMPL_BODIES");
        SymTab_t *symtab = start_semcheck(user_tree, &sem_result);
        if (track_time)
            g_time_semantic += current_time_seconds() - sem_start;
        
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
        codegen_unit(user_tree, input_file, &ctx, symtab);
        if (track_time)
            g_time_codegen += current_time_seconds() - codegen_start;
        
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
        return 0;
    }

    /* Normal program compilation continues below */
    ListNode_t *prelude_subs = NULL;
    ListNode_t *user_subs = user_tree->tree_data.program_data.subprograms;
    UnitSet visited_units;
    unit_set_init(&visited_units);
    ListNode_t *user_types = user_tree->tree_data.program_data.type_declaration;
    ListNode_t *user_consts = user_tree->tree_data.program_data.const_declaration;
    ListNode_t *user_vars = user_tree->tree_data.program_data.var_declaration;
    user_tree->tree_data.program_data.type_declaration = NULL;
    user_tree->tree_data.program_data.const_declaration = NULL;
    user_tree->tree_data.program_data.var_declaration = NULL;
    if (prelude_tree != NULL)
    {
        prelude_subs = get_prelude_subprograms(prelude_tree);
        if (prelude_subs != NULL)
            mark_stdlib_var_params(prelude_subs);
        if (prelude_subs != NULL)
        {
            /* Mark prelude (system.p) subprograms as library procedures so they don't
             * incorrectly get static links when merged into user programs */
            mark_unit_subprograms(prelude_subs);
            
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
            user_tree->tree_data.program_data.type_declaration =
                ConcatList(prelude_types, user_tree->tree_data.program_data.type_declaration);
            clear_prelude_type_decls(prelude_tree);
        }

        /* Merge prelude constants into program */
        ListNode_t *prelude_consts = get_prelude_const_decls(prelude_tree);
        if (prelude_consts != NULL)
        {
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
            mark_unit_var_decls(prelude_vars, 1);
            user_tree->tree_data.program_data.var_declaration =
                ConcatList(prelude_vars, user_tree->tree_data.program_data.var_declaration);
            clear_prelude_var_decls(prelude_tree);
        }

        load_prelude_uses(user_tree, prelude_tree, &visited_units);
    }
    if (!use_stdlib)
        load_unit(user_tree, "System", &visited_units);
    load_units_from_list(user_tree, user_tree->tree_data.program_data.uses_units, &visited_units);
    
    /* If {$MODE objfpc} was detected, automatically load ObjPas unit.
     * This makes types like TEndian available without explicit 'uses objpas'.
     * Also add ObjPas to the uses list so that unit-qualified references
     * like ObjPas.TEndian work correctly. */
    if (pascal_frontend_is_objfpc_mode())
    {
        load_unit(user_tree, "objpas", &visited_units);
        /* Add ObjPas to uses list for semcheck_is_unit_name to recognize it */
        ListNode_t *objpas_node = CreateListNode(strdup("ObjPas"), LIST_STRING);
        if (objpas_node != NULL)
        {
            objpas_node->next = user_tree->tree_data.program_data.uses_units;
            user_tree->tree_data.program_data.uses_units = objpas_node;
        }
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
    
    debug_check_type_presence(user_tree);
    user_tree->tree_data.program_data.type_declaration =
        ConcatList(user_tree->tree_data.program_data.type_declaration, user_types);
    user_tree->tree_data.program_data.const_declaration =
        ConcatList(user_tree->tree_data.program_data.const_declaration, user_consts);
    user_tree->tree_data.program_data.var_declaration =
        ConcatList(user_tree->tree_data.program_data.var_declaration, user_vars);
    resolve_pending_generic_aliases(user_tree);
    append_generic_method_clones(user_tree);
    resolve_pending_generic_subprograms(user_tree);

    unit_set_destroy(&visited_units);

    /* Check for frontend (parser/tree conversion) errors */
    int frontend_errors = from_cparser_get_error_count();
    
    int sem_result = 0;
    double sem_start = track_time ? current_time_seconds() : 0.0;
    if (g_skip_stdlib)
        setenv("KGPC_SKIP_IMPORTED_IMPL_BODIES", "1", 1);
    else
        unsetenv("KGPC_SKIP_IMPORTED_IMPL_BODIES");
    SymTab_t *symtab = start_semcheck(user_tree, &sem_result);
    if (track_time)
        g_time_semantic += current_time_seconds() - sem_start;
    
    /* Add frontend errors to semantic result */
    sem_result += frontend_errors;
    
    int exit_code = 0;

    if (sem_result <= 0)
    {
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

        /* Mark which functions are actually used (dead code elimination) */
        extern void mark_used_functions(Tree_t *program, SymTab_t *symtab);
        mark_used_functions(user_tree, symtab);
        mark_program_subs_used(user_tree);
        /* Run mark_used again to discover functions called by newly-marked subprograms
         * (e.g., inherited methods in specialized generics) */
        mark_used_functions(user_tree, symtab);

        double codegen_start = track_time ? current_time_seconds() : 0.0;
        codegen(user_tree, input_file, &ctx, symtab);
        if (track_time)
            g_time_codegen += current_time_seconds() - codegen_start;
        int codegen_failed = codegen_had_error(&ctx);
        fclose(ctx.output_file);
        if (codegen_failed)
        {
            fprintf(stderr, "Code generation failed; removing incomplete output file.\n");
            remove(output_file);
            exit_code = 1;
        }
        else
        {
            emit_link_args();
        }
    }
    else
    {
        fprintf(stderr, "Semantic analysis failed with %d error(s).\n", sem_result);
        exit_code = sem_result;
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

    if (sem_result > 0)
    {
        clear_dump_ast_path();
        pascal_frontend_cleanup();
        unit_search_paths_destroy(&g_unit_paths);
        arena_destroy(arena);
        return exit_code > 0 ? exit_code : 1;
    }

    clear_dump_ast_path();
    pascal_frontend_cleanup();
    unit_search_paths_destroy(&g_unit_paths);
    arena_destroy(arena);
    return exit_code;
}
