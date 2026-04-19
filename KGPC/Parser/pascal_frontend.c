#include "pascal_frontend.h"

#include <ctype.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#ifndef _WIN32
#include <strings.h>
#endif
#if !defined(_WIN32) && defined(__GLIBC__)
#include <malloc.h>
#endif

#include "ErrVars.h"
#include "ParseTree/from_cparser.h"
#include "SemanticCheck/SemCheck.h"
#include "ast_cache.h"
#include "../string_intern.h"
#include "../compilation_context.h"
#include "../../common/file_time.h"

/* Global storage for user-defined preprocessor configuration */
#define MAX_USER_INCLUDE_PATHS 64
#define MAX_USER_DEFINES 128

static char *g_user_include_paths[MAX_USER_INCLUDE_PATHS];
static int g_user_include_path_count = 0;

static char *g_user_defines[MAX_USER_DEFINES];
static int g_user_define_count = 0;
static char *g_last_parse_path = NULL;

/* AST cache directory (NULL = disabled). When set, parsed ASTs for units
 * are cached to binary files in this directory. */
static char *g_ast_cache_dir = NULL;
static bool g_ast_cache_reads_enabled = true;

/* Modification time of the compiler binary. When set, cached ASTs older
 * than the binary are considered stale and re-parsed. */
static struct timespec g_compiler_mtime = { 0, 0 };
static bool g_compiler_mtime_known = false;

/* Flag set when {$MODE objfpc} is detected in the current parse.
 * Used to automatically inject ObjPas unit dependency. */
static bool g_objfpc_mode_detected = false;
static bool g_default_shortstring = false;

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

static bool ast_cache_is_fresh(const char *cache_path, const char *source_path)
{
    struct stat cache_st;
    struct stat source_st;

    if (cache_path == NULL || source_path == NULL)
        return false;
    if (stat(cache_path, &cache_st) != 0)
        return false;
    if (stat(source_path, &source_st) != 0)
        return false;

    /* Cache must be newer than the source file */
    if (cache_st.st_mtime < source_st.st_mtime)
        return false;

    /* Cache must be newer than the compiler binary (if known). Use
     * nanosecond precision so rebuilds and cache writes in the same second
     * cannot accidentally reuse stale ASTs. */
    if (g_compiler_mtime_known)
    {
        struct timespec cache_mtime = kgpc_stat_mtime(&cache_st);
        if (cache_mtime.tv_sec < g_compiler_mtime.tv_sec)
            return false;
        if (cache_mtime.tv_sec == g_compiler_mtime.tv_sec &&
            cache_mtime.tv_nsec < g_compiler_mtime.tv_nsec)
            return false;
    }

    return true;
}

/* Check if source buffer contains {$MODE objfpc} directive */
static bool detect_objfpc_mode(const char *buffer, size_t length)
{
    if (buffer == NULL || length == 0)
        return false;
    
    const char *pos = buffer;
    const char *end = buffer + length;
    
    while (pos < end)
    {
        /* Look for compiler directive start */
        if (*pos == '{' && (pos + 1) < end && pos[1] == '$')
        {
            pos += 2; /* Skip {$ */
            
            /* Skip whitespace */
            while (pos < end && (*pos == ' ' || *pos == '\t'))
                pos++;
            
            /* Check for MODE keyword (case-insensitive) */
            if ((pos + 4) < end && 
                (pos[0] == 'M' || pos[0] == 'm') &&
                (pos[1] == 'O' || pos[1] == 'o') &&
                (pos[2] == 'D' || pos[2] == 'd') &&
                (pos[3] == 'E' || pos[3] == 'e') &&
                (pos[4] == ' ' || pos[4] == '\t'))
            {
                pos += 5;
                
                /* Skip whitespace */
                while (pos < end && (*pos == ' ' || *pos == '\t'))
                    pos++;
                
                /* Check for OBJFPC (case-insensitive) */
                if ((pos + 6) <= end &&
                    (pos[0] == 'O' || pos[0] == 'o') &&
                    (pos[1] == 'B' || pos[1] == 'b') &&
                    (pos[2] == 'J' || pos[2] == 'j') &&
                    (pos[3] == 'F' || pos[3] == 'f') &&
                    (pos[4] == 'P' || pos[4] == 'p') &&
                    (pos[5] == 'C' || pos[5] == 'c'))
                {
                    return true;
                }
            }
            
            /* Skip to end of directive */
            while (pos < end && *pos != '}')
                pos++;
        }
        pos++;
    }
    
    return false;
}

/* Scan a buffer for {$H+} or {$H-} directives.
 * Returns 1 if found, 0 if not.  Updates *out_shortstring to match the
 * LAST occurrence: true for {$H-} (shortstring default), false for {$H+}. */
static int detect_shortstring_in_buffer(const char *buffer, size_t length, bool *value_out)
{
    if (buffer == NULL || length == 0 || value_out == NULL)
        return 0;

    const char *pos = buffer;
    const char *end = buffer + length;
    int found = 0;

    while (pos < end)
    {
        if (*pos == '{' && (pos + 1) < end && pos[1] == '$')
        {
            const char *dir_start = pos + 2;
            const char *dir_end = dir_start;
            while (dir_end < end && *dir_end != '}')
                dir_end++;

            for (const char *cur = dir_start; cur < dir_end; ++cur)
            {
                if (*cur == 'H' || *cur == 'h')
                {
                    const char *next = cur + 1;
                    while (next < dir_end && isspace((unsigned char)*next))
                        next++;
                    if (next < dir_end && (*next == '+' || *next == '-'))
                    {
                        *value_out = (*next == '-');
                        found = 1;
                    }
                }
            }

            pos = dir_end;
        }
        pos++;
    }

    return found;
}

/* Try to resolve an include file name relative to the source directory
 * and the registered include paths. Returns a malloc'd path or NULL. */
static char *resolve_include_for_h_detection(const char *source_path, const char *inc_name)
{
    /* Try relative to the source file's directory first. */
    if (source_path != NULL)
    {
        const char *last_sep = strrchr(source_path, '/');
        if (last_sep != NULL)
        {
            size_t dir_len = (size_t)(last_sep - source_path + 1);
            size_t name_len = strlen(inc_name);
            char *full = (char *)malloc(dir_len + name_len + 1);
            if (full != NULL)
            {
                memcpy(full, source_path, dir_len);
                memcpy(full + dir_len, inc_name, name_len + 1);
                FILE *f = fopen(full, "r");
                if (f != NULL)
                {
                    fclose(f);
                    return full;
                }
                free(full);
            }
        }
    }

    /* Try registered include paths. */
    for (int i = 0; i < g_user_include_path_count; ++i)
    {
        if (g_user_include_paths[i] == NULL)
            continue;
        size_t plen = strlen(g_user_include_paths[i]);
        size_t nlen = strlen(inc_name);
        char *full = (char *)malloc(plen + 1 + nlen + 1);
        if (full == NULL)
            continue;
        memcpy(full, g_user_include_paths[i], plen);
        full[plen] = '/';
        memcpy(full + plen + 1, inc_name, nlen + 1);
        FILE *f = fopen(full, "r");
        if (f != NULL)
        {
            fclose(f);
            return full;
        }
        free(full);
    }

    return NULL;
}

/* Detect {$H+/-} in a source buffer.  If not found directly, also follow
 * {$i <file>} includes one level deep to detect the directive in common
 * included files like fpcdefs.inc. */
static int detect_shortstring_default(const char *buffer, size_t length,
                                      bool *out_shortstring,
                                      const char *source_path)
{
    if (buffer == NULL || length == 0 || out_shortstring == NULL)
        return 0;

    bool value = false;
    if (detect_shortstring_in_buffer(buffer, length, &value))
    {
        *out_shortstring = value;
        return 1;
    }

    /* Not found directly — look for {$i <file>} includes and check them. */
    const char *pos = buffer;
    const char *end = buffer + length;

    while (pos < end)
    {
        if (*pos == '{' && (pos + 1) < end && pos[1] == '$')
        {
            const char *dir_start = pos + 2;
            const char *dir_end = dir_start;
            while (dir_end < end && *dir_end != '}')
                dir_end++;

            /* Check for {$i <filename>} or {$include <filename>} */
            size_t dir_len = (size_t)(dir_end - dir_start);
            if (dir_len >= 2 &&
                (dir_start[0] == 'i' || dir_start[0] == 'I') &&
                (dir_start[1] == ' ' || dir_start[1] == '\t'))
            {
                const char *fname_start = dir_start + 2;
                while (fname_start < dir_end && isspace((unsigned char)*fname_start))
                    fname_start++;
                /* Skip if it looks like {$I+} or {$I-} (I/O check toggle) */
                if (fname_start < dir_end && *fname_start != '+' && *fname_start != '-')
                {
                    const char *fname_end = fname_start;
                    while (fname_end < dir_end && !isspace((unsigned char)*fname_end))
                        fname_end++;
                    size_t fname_len = (size_t)(fname_end - fname_start);
                    if (fname_len > 0 && fname_len < 256)
                    {
                        char inc_name[256];
                        memcpy(inc_name, fname_start, fname_len);
                        inc_name[fname_len] = '\0';
                        /* Resolve and read the include file. */
                        char *inc_path = resolve_include_for_h_detection(source_path, inc_name);
                        if (inc_path != NULL)
                        {
                            FILE *f = fopen(inc_path, "r");
                            if (f != NULL)
                            {
                                fseek(f, 0, SEEK_END);
                                long sz = ftell(f);
                                fseek(f, 0, SEEK_SET);
                                if (sz > 0 && sz < 1024 * 1024)
                                {
                                    char *inc_buf = (char *)malloc((size_t)sz + 1);
                                    if (inc_buf != NULL)
                                    {
                                        size_t rd = fread(inc_buf, 1, (size_t)sz, f);
                                        inc_buf[rd] = '\0';
                                        bool inc_value = false;
                                        if (detect_shortstring_in_buffer(inc_buf, rd, &inc_value))
                                        {
                                            *out_shortstring = inc_value;
                                            free(inc_buf);
                                            fclose(f);
                                            free(inc_path);
                                            return 1;
                                        }
                                        free(inc_buf);
                                    }
                                }
                                fclose(f);
                            }
                            free(inc_path);
                        }
                    }
                }
            }

            pos = dir_end;
        }
        pos++;
    }

    return 0;
}

/* Getter for objfpc mode flag - used by main_cparser.c to inject ObjPas */
bool pascal_frontend_is_objfpc_mode(void)
{
    return g_objfpc_mode_detected;
}

/* Reset objfpc mode flag before parsing a new file */
void pascal_frontend_reset_objfpc_mode(void)
{
    g_objfpc_mode_detected = false;
    g_default_shortstring = false;
}

void pascal_frontend_set_objfpc_mode(void)
{
    g_objfpc_mode_detected = true;
}

bool pascal_frontend_default_shortstring(void)
{
    return g_default_shortstring;
}

void pascal_frontend_set_default_shortstring(bool value)
{
    g_default_shortstring = value;
}

void pascal_frontend_add_include_path(const char *path)
{
    if (path == NULL || g_user_include_path_count >= MAX_USER_INCLUDE_PATHS)
        return;
    g_user_include_paths[g_user_include_path_count++] = strdup(path);
}

void pascal_frontend_add_define(const char *define)
{
    if (define == NULL || g_user_define_count >= MAX_USER_DEFINES)
        return;
    g_user_defines[g_user_define_count++] = strdup(define);
}

void pascal_frontend_clear_user_config(void)
{
    for (int i = 0; i < g_user_include_path_count; ++i)
    {
        free(g_user_include_paths[i]);
        g_user_include_paths[i] = NULL;
    }
    g_user_include_path_count = 0;
    
    for (int i = 0; i < g_user_define_count; ++i)
    {
        free(g_user_defines[i]);
        g_user_defines[i] = NULL;
    }
    g_user_define_count = 0;
}

void pascal_frontend_set_ast_cache_dir(const char *dir)
{
    if (g_ast_cache_dir != NULL)
        free(g_ast_cache_dir);
    g_ast_cache_dir = (dir != NULL) ? strdup(dir) : NULL;
    if (g_ast_cache_dir != NULL && kgpc_mkdir(g_ast_cache_dir, 0775) != 0)
    {
        struct stat st;
        if (stat(g_ast_cache_dir, &st) != 0 || !S_ISDIR(st.st_mode))
        {
            fprintf(stderr, "Warning: AST cache directory '%s' is not usable; disabling AST cache.\n",
                g_ast_cache_dir);
            free(g_ast_cache_dir);
            g_ast_cache_dir = NULL;
        }
    }
}

void pascal_frontend_set_compiler_mtime(struct timespec mtime)
{
    g_compiler_mtime = mtime;
    g_compiler_mtime_known = true;
}

const char * const *pascal_frontend_get_include_paths(int *count)
{
    if (count != NULL)
        *count = g_user_include_path_count;
    return (const char * const *)g_user_include_paths;
}

const char *pascal_frontend_current_path(void)
{
    return g_last_parse_path;
}

#include "ParseTree/from_cparser.h"
#include "ParseTree/tree.h"
#include "ParseTree/generic_types.h"
#include "pascal_preprocessor.h"
#include "../flags.h"
#include "../../cparser/parser.h"
#include "../../cparser/examples/pascal_parser/pascal_peek.h"


/* Cached getenv() — defined in SemCheck.c */
extern const char *kgpc_getenv(const char *name);
extern ast_t *ast_nil;

static int find_conflict_marker_line(const char *buffer)
{
    int line = 1;
    int in_brace_comment = 0;
    int in_paren_comment = 0;

    if (buffer == NULL)
        return 0;

    for (const char *cursor = buffer; *cursor != '\0'; )
    {
        const char *line_start = cursor;
        const char *line_end = cursor;
        while (*line_end != '\0' && *line_end != '\n' && *line_end != '\r')
            ++line_end;

        const char *p = line_start;
        while (p < line_end)
        {
            if (in_brace_comment)
            {
                if (*p == '}')
                    in_brace_comment = 0;
                ++p;
                continue;
            }
            if (in_paren_comment)
            {
                if ((p + 1) < line_end && p[0] == '*' && p[1] == ')')
                {
                    in_paren_comment = 0;
                    p += 2;
                    continue;
                }
                ++p;
                continue;
            }

            if (*p == ' ' || *p == '\t')
            {
                ++p;
                continue;
            }
            if ((p + 1) < line_end && p[0] == '/' && p[1] == '/')
                break;
            if (*p == '{')
            {
                in_brace_comment = 1;
                ++p;
                continue;
            }
            if ((p + 1) < line_end && p[0] == '(' && p[1] == '*')
            {
                in_paren_comment = 1;
                p += 2;
                continue;
            }

            {
                size_t remaining = (size_t)(line_end - p);
                if ((remaining == 7 && strncmp(p, "=======", 7) == 0) ||
                    (remaining >= 7 &&
                     ((strncmp(p, "<<<<<<<", 7) == 0) ||
                      (strncmp(p, ">>>>>>>", 7) == 0)) &&
                     (remaining == 7 || p[7] == ' ' || p[7] == '\t')))
                {
                    return line;
                }
            }

            break;
        }

        cursor = line_end;
        if (*cursor == '\r' && cursor[1] == '\n')
            ++cursor;
        if (*cursor != '\0')
            ++cursor;
        ++line;
    }

    return 0;
}

static char *read_file(const char *path, size_t *out_len)
{
    FILE *fp = fopen(path, "rb");
    if (fp == NULL)
    {
        fprintf(stderr, "ERROR: Failed to open %s\n", path);
        return NULL;
    }

    if (fseek(fp, 0, SEEK_END) != 0)
    {
        fprintf(stderr, "ERROR: Failed to seek %s\n", path);
        fclose(fp);
        return NULL;
    }

    long len = ftell(fp);
    if (len < 0)
    {
        fprintf(stderr, "ERROR: Failed to determine size of %s\n", path);
        fclose(fp);
        return NULL;
    }

    if (fseek(fp, 0, SEEK_SET) != 0)
    {
        fprintf(stderr, "ERROR: Failed to rewind %s\n", path);
        fclose(fp);
        return NULL;
    }

    char *buffer = (char *)malloc((size_t)len + 1);
    if (buffer == NULL)
    {
        fprintf(stderr, "ERROR: Out of memory while reading %s\n", path);
        fclose(fp);
        return NULL;
    }

    size_t read_len = fread(buffer, 1, (size_t)len, fp);
    fclose(fp);
    if (read_len != (size_t)len)
    {
        fprintf(stderr, "ERROR: Failed to read %s\n", path);
        free(buffer);
        return NULL;
    }

    buffer[len] = '\0';

    {
        int conflict_line = find_conflict_marker_line(buffer);
        if (conflict_line > 0)
        {
            fprintf(stderr,
                "ERROR: Merge conflict marker found in %s at line %d\n",
                path, conflict_line);
            free(buffer);
            return NULL;
        }
    }

    if (out_len != NULL)
        *out_len = (size_t)len;

    return buffer;
}

static void set_preprocessed_context(const char *buffer, size_t length, const char *path)
{
    if (preprocessed_source != NULL)
    {
        free(preprocessed_source);
        preprocessed_source = NULL;
    }
    if (preprocessed_path != NULL)
    {
        free(preprocessed_path);
        preprocessed_path = NULL;
    }
    preprocessed_length = 0;

    if (buffer == NULL || length == 0)
        return;

    const unsigned char *ubytes = (const unsigned char *)buffer;
    if (length >= 3 && ubytes[0] == 0xEF && ubytes[1] == 0xBB && ubytes[2] == 0xBF)
    {
        buffer += 3;
        length -= 3;
    }

    preprocessed_source = (char *)malloc(length + 1);
    if (preprocessed_source == NULL)
        return;

    memcpy(preprocessed_source, buffer, length);
    preprocessed_source[length] = '\0';
    preprocessed_length = length;

    if (path != NULL)
        preprocessed_path = strdup(path);
    if (path != NULL)
        file_to_parse = (char *)path;
}

static const char *skip_utf8_bom(const char *cursor, const char *end)
{
    if (end - cursor >= 3 &&
        (unsigned char)cursor[0] == 0xEF &&
        (unsigned char)cursor[1] == 0xBB &&
        (unsigned char)cursor[2] == 0xBF)
    {
        return cursor + 3;
    }
    return cursor;
}

static const char *skip_whitespace_and_comments(const char *cursor, const char *end)
{
    while (cursor < end)
    {
        unsigned char ch = (unsigned char)*cursor;
        if (isspace(ch))
        {
            ++cursor;
            continue;
        }

        if (ch == '{')
        {
            ++cursor;
            while (cursor < end && *cursor != '}')
                ++cursor;
            if (cursor < end)
                ++cursor;
            continue;
        }

        if (ch == '(' && (cursor + 1) < end && cursor[1] == '*')
        {
            cursor += 2;
            while ((cursor + 1) < end && !(cursor[0] == '*' && cursor[1] == ')'))
                ++cursor;
            if ((cursor + 1) < end)
                cursor += 2;
            else
                cursor = end;
            continue;
        }

        if (ch == '/' && (cursor + 1) < end && cursor[1] == '/')
        {
            cursor += 2;
            while (cursor < end && *cursor != '\n')
                ++cursor;
            continue;
        }

        break;
    }

    return cursor;
}

static bool buffer_starts_with_unit(const char *buffer, size_t length)
{
    const char *cursor = buffer;
    const char *end = buffer + length;
    cursor = skip_utf8_bom(cursor, end);
    cursor = skip_whitespace_and_comments(cursor, end);

    const char *keyword = "unit";
    size_t keyword_len = strlen(keyword);
    if ((size_t)(end - cursor) < keyword_len)
        return false;

    if (strncasecmp(cursor, keyword, keyword_len) != 0)
        return false;

    const char *after = cursor + keyword_len;
    if (after < end && (isalnum((unsigned char)*after) || *after == '_'))
        return false;

    return true;
}

// Cache for initialized parsers to avoid expensive re-initialization
static combinator_t *cached_unit_parser = NULL;
static combinator_t *cached_program_parser = NULL;
static bool generic_registry_ready = false;

static void ensure_generic_registry(void)
{
    if (generic_registry_ready)
        return;
    generic_registry_init();
    generic_registry_ready = true;
}

static combinator_t *get_or_create_unit_parser(void)
{
    if (cached_unit_parser == NULL)
    {
        cached_unit_parser = new_combinator();
        init_pascal_unit_parser(&cached_unit_parser);
        parser_set_ephemeral_threshold();
    }
    return cached_unit_parser;
}

static combinator_t *get_or_create_program_parser(void)
{
    if (cached_program_parser == NULL)
    {
        cached_program_parser = new_combinator();
        init_pascal_complete_program_parser(&cached_program_parser);
        parser_set_ephemeral_threshold();
    }
    return cached_program_parser;
}

static ParseError *create_preprocessor_error(const char *path, const char *detail)
{
    ParseError *err = (ParseError *)calloc(1, sizeof(ParseError));
    if (err == NULL)
        return NULL;

    err->line = 0;
    err->col = 0;
    err->index = -1;

    const char *detail_text = detail != NULL ? detail : "unknown error";
    const char *template = path != NULL ? "Preprocessing failed for '%s': %s"
                                       : "Preprocessing failed: %s";

    int needed_len = 0;
    if (path != NULL)
        needed_len = snprintf(NULL, 0, template, path, detail_text);
    else
        needed_len = snprintf(NULL, 0, template, detail_text);
    if (needed_len < 0)
    {
        free(err);
        return NULL;
    }
    size_t needed = (size_t)needed_len + 1;
    char *message = (char *)malloc(needed);
    if (message == NULL)
    {
        free(err);
        return NULL;
    }

    if (path != NULL)
        snprintf(message, needed, template, path, detail_text);
    else
        snprintf(message, needed, template, detail_text);

    err->message = message;

    const char *stage = "preprocessor";
    err->parser_name = strdup(stage);
    if (err->parser_name == NULL)
    {
        free(message);
        free(err);
        return NULL;
    }

    err->unexpected = NULL;
    err->context = NULL;
    err->cause = NULL;
    err->partial_ast = NULL;

    return err;
}

static void report_preprocessor_error(ParseError **error_out, const char *path, const char *detail)
{
    if (error_out != NULL && *error_out == NULL)
    {
        ParseError *err = create_preprocessor_error(path, detail);
        if (err != NULL)
        {
            *error_out = err;
            return;
        }
    }

    const char *location = path != NULL ? path : "<buffer>";
    if (detail != NULL)
        fprintf(stderr, "Preprocessing failed for %s: %s\n", location, detail);
    else
        fprintf(stderr, "Preprocessing failed for %s\n", location);
}

void pascal_frontend_cleanup(void)
{
    /* Free the cached parser graphs. free_combinator_graph() uses a shared
     * visited set and ignores the cached flag, so shared subtrees between
     * the unit and program parsers are freed exactly once. */
    combinator_t *roots[2];
    size_t root_count = 0;
    if (cached_unit_parser != NULL)
        roots[root_count++] = cached_unit_parser;
    if (cached_program_parser != NULL)
        roots[root_count++] = cached_program_parser;
    if (root_count > 0)
        free_combinator_graph(roots, root_count);
    cached_unit_parser = NULL;
    cached_program_parser = NULL;
    parser_drain_free_list();
    if (generic_registry_ready)
    {
        generic_registry_cleanup();
        generic_registry_ready = false;
    }
    if (g_last_parse_path != NULL)
    {
        free(g_last_parse_path);
        g_last_parse_path = NULL;
    }
    if (preprocessed_source != NULL)
    {
        free(preprocessed_source);
        preprocessed_source = NULL;
    }
    if (preprocessed_path != NULL)
    {
        free(preprocessed_path);
        preprocessed_path = NULL;
    }
    preprocessed_length = 0;
    if (g_ast_cache_dir != NULL)
    {
        free(g_ast_cache_dir);
        g_ast_cache_dir = NULL;
    }
    g_ast_cache_reads_enabled = true;
    from_cparser_cleanup();
    string_intern_reset();
}

/* Compute the cache file path for a given source path.
 * The hash includes user-defined preprocessor symbols so that different
 * define sets (e.g. regular RTL tests vs pp.pas bootstrap) produce
 * distinct cache files.
 * Returns a malloc'd string or NULL if caching is disabled. */
static char *compute_ast_cache_path(const char *source_path)
{
    if (g_ast_cache_dir == NULL || source_path == NULL)
        return NULL;

    size_t dir_len = strlen(g_ast_cache_dir);
    uint64_t hash = fnv1a64_bytes((const unsigned char *)source_path, strlen(source_path));
    /* Mix in user-defined preprocessor symbols so different -D flags
     * produce different cache entries. */
    for (int i = 0; i < g_user_define_count; ++i)
    {
        if (g_user_defines[i] != NULL)
            hash = fnv1a64_bytes((const unsigned char *)g_user_defines[i],
                                 strlen(g_user_defines[i])) ^ hash;
    }
    if (g_compiler_mtime_known)
    {
        char compiler_stamp[64];
        snprintf(compiler_stamp, sizeof(compiler_stamp), "%lld.%09ld",
                 (long long)g_compiler_mtime.tv_sec,
                 (long)g_compiler_mtime.tv_nsec);
        hash = fnv1a64_bytes((const unsigned char *)compiler_stamp,
                             strlen(compiler_stamp)) ^ hash;
    }
    /* <dir>/<16-hex>.ast_cache\0 */
    size_t total = dir_len + 1 + 16 + 10 + 1;
    char *cache_path = (char *)malloc(total);
    if (cache_path == NULL)
        return NULL;
    snprintf(cache_path, total, "%s/%016llx.ast_cache",
        g_ast_cache_dir, (unsigned long long)hash);
    return cache_path;
}

static bool path_is_compiler_prelude(const char *path)
{
    if (path == NULL)
        return false;

    const char *base = strrchr(path, '/');
#ifdef _WIN32
    const char *backslash = strrchr(path, '\\');
    if (backslash != NULL && (base == NULL || backslash > base))
        base = backslash;
#endif
    base = (base != NULL) ? base + 1 : path;

    if (strcmp(base, "system.p") != 0 && strcmp(base, "prelude.p") != 0)
        return false;

    return strstr(path, "KGPC/Units/") != NULL || strstr(path, "KGPC\\Units\\") != NULL;
}

static bool source_path_is_cacheable(const char *path)
{
    if (path_is_compiler_prelude(path))
        return true;

    size_t length = 0;
    char *buffer = read_file(path, &length);
    if (buffer == NULL)
        return false;

    bool is_unit = buffer_starts_with_unit(buffer, length);
    free(buffer);
    return is_unit;
}

static void drain_parser_parse_pools(void)
{
    parser_drain_ast_free_list();
    parser_drain_error_free_list();
}

bool pascal_parse_source(const char *path, bool convert_to_tree, Tree_t **out_tree, ParseError **error_out)
{
    if (error_out != NULL)
        *error_out = NULL;

    /* Disable memoization — with ephemeral combinators getting unique IDs,
     * memo table operations (lookup/insert/clone) cost more than they save. */
    static bool memo_mode_set = false;
    if (!memo_mode_set) {
        parser_set_memo_mode(PARSER_MEMO_DISABLED);
        memo_mode_set = true;
    }

    ensure_generic_registry();

    if (g_last_parse_path != NULL)
    {
        free(g_last_parse_path);
        g_last_parse_path = NULL;
    }
    if (path != NULL)
        g_last_parse_path = strdup(path);

    bool cache_units_only = source_path_is_cacheable(path);

    /* --- AST cache: try loading a cached binary AST to skip preprocessing+parsing --- */
    if (cache_units_only && g_ast_cache_reads_enabled)
    {
        char *cache_path = compute_ast_cache_path(path);
        if (cache_path != NULL)
        {
            ast_t *cached_ast = NULL;
            char *cached_pp_buf = NULL;
            size_t cached_pp_len = 0;
            if (ast_cache_is_fresh(cache_path, path) &&
                ast_cache_load(cache_path, &cached_ast, &cached_pp_buf, &cached_pp_len))
            {
                free(cache_path);
                /* Set up preprocessed source context (needed for semcheck error reporting) */
                set_preprocessed_context(cached_pp_buf, cached_pp_len, path);
                int src_offset = semcheck_register_source_buffer(path, cached_pp_buf, cached_pp_len);

                /* Detect objfpc mode from cached preprocessed source */
                if (detect_objfpc_mode(cached_pp_buf, cached_pp_len))
                    g_objfpc_mode_detected = true;

                /* Detect {$H-} from original source (not cached preprocessed,
                 * since preprocessing strips directives).  This must happen
                 * before tree_from_pascal_ast so the return-type remapping
                 * in from_cparser.c sees the correct flag. */
                {
                    size_t raw_len = 0;
                    char *raw_buf = read_file(path, &raw_len);
                    if (raw_buf != NULL)
                    {
                        detect_shortstring_default(raw_buf, raw_len,
                                                   &g_default_shortstring, path);
                        free(raw_buf);
                    }
                }

                /* Convert the cached AST to Tree_t */
                Tree_t *tree = NULL;
                bool success = false;
                if (convert_to_tree)
                {
                    file_to_parse = (char *)path;
                    from_cparser_set_source_offset(src_offset);
                    tree = tree_from_pascal_ast(cached_ast);
                    if (tree != NULL)
                        success = true;
                    else
                        fprintf(stderr, "Error: Failed to convert cached AST for '%s'.\n", path);
                }
                else
                {
                    success = true;
                }
                free_ast(cached_ast);
                drain_parser_parse_pools();
                free(cached_pp_buf);
#if !defined(_WIN32) && defined(__GLIBC__)
                malloc_trim(0);
#endif

                if (out_tree != NULL)
                    *out_tree = success ? tree : NULL;
                else if (tree != NULL)
                    destroy_tree(tree);

                return success;
            }
            g_ast_cache_reads_enabled = false;
            free(cache_path);
            /* Cache miss — proceed with normal parsing, save at the end */
        }
    }

    size_t length = 0;
    char *buffer = read_file(path, &length);
    if (buffer == NULL)
        return false;

    /* Detect {$H+/-} directive on original source BEFORE preprocessing,
     * since the preprocessor strips directive comments. */
    detect_shortstring_default(buffer, length, &g_default_shortstring, path);

    PascalPreprocessor *preprocessor = pascal_preprocessor_create();
    if (preprocessor == NULL)
    {
        report_preprocessor_error(error_out, path, "unable to initialise preprocessor");
        free(buffer);
        return false;
    }

    /* Apply user-defined include paths */
    for (int i = 0; i < g_user_include_path_count; ++i)
    {
        if (g_user_include_paths[i] != NULL)
        {
            pascal_preprocessor_add_include_path(preprocessor, g_user_include_paths[i]);
        }
    }

    /* Apply user-defined symbols */
    for (int i = 0; i < g_user_define_count; ++i)
    {
        if (g_user_defines[i] != NULL)
        {
            pascal_preprocessor_define(preprocessor, g_user_defines[i]);
        }
    }

    // Define our own dialect symbol and FPC for Lazarus-compatible headers
    const char *default_symbols[] = { 
        "KGPC", "FPC", "FPC_FULLVERSION := 30200", "FPC_VERSION := 3", 
        "FPC_RELEASE := 2",
        "FPC_STACKALIGNMENT := 16",
        // FPC feature flags
        "FPC_HAS_FEATURE_SUPPORT",
        "FPC_HAS_FEATURE_DYNARRAYS", "FPC_HAS_FEATURE_ANSISTRINGS", 
        "FPC_HAS_FEATURE_WIDESTRINGS", "FPC_HAS_FEATURE_UNICODESTRINGS",
        "FPC_HAS_UNICODESTRING",
        "FPC_HAS_FEATURE_CLASSES",
        "FPC_HAS_FEATURE_OBJECTS", "FPC_HAS_FEATURE_EXCEPTIONS",
        "FPC_HAS_FEATURE_RTTI", "FPC_HAS_FEATURE_HEAP",
        "FPC_HAS_FEATURE_TEXTIO", "FPC_HAS_FEATURE_FILEIO",
        "FPC_HAS_FEATURE_CONSOLEIO", "FPC_HAS_FEATURE_RANDOM",
        "FPC_HAS_FEATURE_VARIANTS", "FPC_HAS_FEATURE_INITFINAL",
        "FPC_HAS_FEATURE_EXITCODE", "FPC_HAS_FEATURE_RESOURCES",
        "FPC_HAS_FEATURE_COMMANDARGS", "FPC_HAS_FEATURE_PROCESSES",
        "FPC_HAS_FEATURE_THREADING", "FPC_HAS_FEATURE_DYNLIBS",
        "FPC_HAS_FEATURE_OBJECTIVEC1", "FPC_HAS_FEATURE_STACKCHECK",
        // FPC floating-point type availability (KGPC maps Extended to Double)
        "FPC_HAS_TYPE_SINGLE", "FPC_HAS_TYPE_DOUBLE",
        // KGPC cannot evaluate FPU intrinsics (ln, round, etc.) in constant expressions,
        // so define FPUSOFT to prevent FPC RTL from using them in const initializers.
        "FPUSOFT",
        "FPC_WIDESTRING_EQUAL_UNICODESTRING",
        "FPC_ANSI_TEXTFILEREC",
        // Use the legacy (oldheap.inc) heap manager instead of the new heap.inc.
        // The new heap.inc uses complex pointer arithmetic with inline casts
        // (e.g. pCommonHeader(p - CommonHeaderSize)^.h, int32(h) - FixedFlag)
        // that our codegen doesn't yet handle correctly, causing heap corruption
        // and SIGSEGV in SysMemSize during finalization.
        "LEGACYHEAP"
    };
    for (size_t i = 0; i < sizeof(default_symbols) / sizeof(default_symbols[0]); ++i)
    {
        if (!pascal_preprocessor_define(preprocessor, default_symbols[i]))
        {
            char detail[128];
            snprintf(detail, sizeof(detail), "unable to define default symbol '%s'", default_symbols[i]);
            report_preprocessor_error(error_out, path, detail);
            pascal_preprocessor_free(preprocessor);
            free(buffer);
            return false;
        }
    }

    /* Define compile-time constants for {$if} expression evaluation.
     * These are available in {$if} but NOT expanded as text-replacement macros,
     * since they also appear as Pascal constant declarations in the FPC RTL
     * (e.g. maxExitCode = 255 in sysunixh.inc, heap constants in heap.inc). */
    struct { const char *name; const char *value; } const_symbols[] = {
        { "maxExitCode", "255" },
        /* Heap constants from FPCSource/rtl/inc/heap.inc — must match the
         * actual const declarations there.  These are used by {$IF}
         * expression evaluation in the preprocessor.  Keep in sync with
         * the corresponding block in preprocess_main.c. */
        { "FixedArenaOffsetShift", "5" },
        { "VarSizeQuant", "32" },
        { "FirstVarRangeP2", "10" },
        { "FirstVarStepP2", "5" },
        { "VarSizeClassesCount", "10" },
        { "MaxFixedHeaderAndPayload", "544" },
        { "MaxVarHeaderAndPayload", "1048096" },
        { "CommonHeaderSize", "4" },
        { "MinFixedHeaderAndPayload", "16" },
        { "MinSearchableVarHeaderAndPayload", "576" },
        { "FixedSizesCount", "16" },
        { "SizeIndexBits", "4" },
        { "SizeIndexMask", "15" },
        { "FixedBitPos", "4" },
        { "VarSizesPerClass", "32" },
        { "VarSizesCount", "320" },
        { "L0BinSize", "32" },
    };
    for (size_t i = 0; i < sizeof(const_symbols) / sizeof(const_symbols[0]); ++i)
    {
        if (!pascal_preprocessor_define_const(preprocessor, const_symbols[i].name, const_symbols[i].value))
        {
            char detail[128];
            snprintf(detail, sizeof(detail), "unable to define constant '%s'", const_symbols[i].name);
            report_preprocessor_error(error_out, path, detail);
            pascal_preprocessor_free(preprocessor);
            free(buffer);
            return false;
        }
    }

/* Define architecture-specific symbols based on the target platform */
#if INTPTR_MAX >= INT64_MAX
    const char *arch_symbol = "CPU64";
    /* On x86-64 architecture, define CPUX86_64 for FPC compatibility */
    #if defined(__x86_64__) || defined(_M_X64)
    if (!pascal_preprocessor_define(preprocessor, "CPUX86_64"))
    {
        report_preprocessor_error(error_out, path, "unable to define CPUX86_64 symbol");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    /* Define CPUINT64 for FPC (integer register size) */
    if (!pascal_preprocessor_define(preprocessor, "CPUINT64"))
    {
        report_preprocessor_error(error_out, path, "unable to define CPUINT64 symbol");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    /* Define x86_64 — FPC compiler sources use {$if defined(x86_64)} */
    if (!pascal_preprocessor_define(preprocessor, "x86_64"))
    {
        report_preprocessor_error(error_out, path, "unable to define x86_64 symbol");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    /* Define first_mm_imreg for FPC x86_64 paramgr.pas (CPU register constant from cpubase.pas) */
    if (!pascal_preprocessor_define_const(preprocessor, "first_mm_imreg", "32"))
    {
        report_preprocessor_error(error_out, path, "unable to define first_mm_imreg symbol");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    /* Define optimizer switch constants for {$IF x in supported_optimizerswitches} expressions.
     * cs_opt_use_load_modify_store is at ordinal 25 in the toptimizerswitch enum.
     * supported_optimizerswitches for x86_64 includes this (via genericlevel3optimizerswitches).
     * We represent sets as bitmasks: bit N = ordinal N is in set.
     */
    if (!pascal_preprocessor_define_const(preprocessor, "cs_opt_use_load_modify_store", "25"))
    {
        report_preprocessor_error(error_out, path, "unable to define cs_opt_use_load_modify_store");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    /* Bitmask with at least bit 25 set for supported_optimizerswitches */
    if (!pascal_preprocessor_define_const(preprocessor, "supported_optimizerswitches", "33554432"))
    {
        report_preprocessor_error(error_out, path, "unable to define supported_optimizerswitches");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    /* Define max_operands for x86 (4 operands max in x86 instructions) */
    if (!pascal_preprocessor_define_const(preprocessor, "max_operands", "4"))
    {
        report_preprocessor_error(error_out, path, "unable to define max_operands");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    #endif
#else
    const char *arch_symbol = "CPU32";
#endif
    if (!pascal_preprocessor_define(preprocessor, arch_symbol))
    {
        char detail[128];
        snprintf(detail, sizeof(detail), "unable to define default symbol '%s'", arch_symbol);
        report_preprocessor_error(error_out, path, detail);
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }

    /* Define platform-specific symbols */
#if defined(__linux__)
    if (!pascal_preprocessor_define(preprocessor, "LINUX"))
    {
        report_preprocessor_error(error_out, path, "unable to define LINUX symbol");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    if (!pascal_preprocessor_define(preprocessor, "UNIX"))
    {
        report_preprocessor_error(error_out, path, "unable to define UNIX symbol");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    if (!pascal_preprocessor_define(preprocessor, "HASUNIX"))
    {
        report_preprocessor_error(error_out, path, "unable to define HASUNIX symbol");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
#endif

    /* Define endianness - x86/x86_64 is little-endian */
#if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)
    if (!pascal_preprocessor_define(preprocessor, "ENDIAN_LITTLE"))
    {
        report_preprocessor_error(error_out, path, "unable to define ENDIAN_LITTLE symbol");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    if (!pascal_preprocessor_define(preprocessor, "FPC_LITTLE_ENDIAN"))
    {
        report_preprocessor_error(error_out, path, "unable to define FPC_LITTLE_ENDIAN symbol");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    if (!pascal_preprocessor_define(preprocessor, "FPC_USE_LIBC"))
    {
        report_preprocessor_error(error_out, path, "unable to define FPC_USE_LIBC symbol");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
#endif

    /* Define MSWINDOWS when targeting Windows (but not for Cygwin/MSYS which expose a POSIX API) */
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (target_windows_flag())
    {
        if (!pascal_preprocessor_define(preprocessor, "MSWINDOWS"))
        {
            report_preprocessor_error(error_out, path, "unable to define MSWINDOWS symbol");
            pascal_preprocessor_free(preprocessor);
            free(buffer);
            return false;
        }
    }
#else
    /* On Cygwin/MSYS and Unix, don't define MSWINDOWS */
    (void)target_windows_flag;  /* Suppress unused warning */
#endif

    char *preprocess_error = NULL;
    size_t preprocessed_length = 0;
    char *preprocessed_buffer = pascal_preprocess_buffer(preprocessor,
                                                         path,
                                                         buffer,
                                                         length,
                                                         &preprocessed_length,
                                                         &preprocess_error);
    /* Record included files in the compilation context for cache key. */
    {
        const char *const *inc_files = NULL;
        size_t inc_count = pascal_preprocessor_get_included_files(preprocessor, &inc_files);
        if (inc_count > 0) {
            CompilationContext *comp_ctx = compilation_context_get_active();
            if (comp_ctx != NULL)
                compilation_context_add_include_files(comp_ctx, inc_files, (int)inc_count);
        }
    }
    pascal_preprocessor_free(preprocessor);

    /* Temp debug: dump preprocessed output for system.pp */
    if (kgpc_getenv("KGPC_DUMP_PP") != NULL && path != NULL && strstr(path, "system.pp") != NULL) {
        FILE *dump = fopen("/tmp/system_pp_preprocessed.txt", "w");
        if (dump != NULL) {
            fwrite(preprocessed_buffer, 1, preprocessed_length, dump);
            fclose(dump);
            fprintf(stderr, "[PP] Dumped preprocessed system.pp to /tmp/system_pp_preprocessed.txt (%zu bytes)\n", preprocessed_length);
        }
    }

    if (preprocessed_buffer == NULL)
    {
        report_preprocessor_error(error_out, path, preprocess_error);
        free(preprocess_error);
        free(buffer);
        return false;
    }

    free(preprocess_error);
    free(buffer);
    buffer = preprocessed_buffer;
    length = preprocessed_length;

    if (length >= 3)
    {
        unsigned char *ubytes = (unsigned char *)buffer;
        if (ubytes[0] == 0xEF && ubytes[1] == 0xBB && ubytes[2] == 0xBF)
        {
            memmove(buffer, buffer + 3, length - 3);
            length -= 3;
            buffer[length] = '\0';
        }
    }
    set_preprocessed_context(buffer, length, path);
    int src_offset = semcheck_register_source_buffer(path, buffer, length);
    from_cparser_set_source_offset(src_offset);

    /* Debug: dump full preprocessed buffer */
    if (kgpc_getenv("KGPC_DUMP_PREPROC")) {
        char dumppath[256];
        const char *base = strrchr(path, '/');
        base = base ? base + 1 : path;
        snprintf(dumppath, sizeof(dumppath), "/tmp/preproc_%s.txt", base);
        FILE *dumpf = fopen(dumppath, "w");
        if (dumpf) {
            fwrite(buffer, 1, length, dumpf);
            fclose(dumpf);
        }
    }

    /* Detect {$MODE objfpc} in the preprocessed source.
     * If found, set flag so ObjPas unit can be auto-imported.
     * Note: {$H+/-} detection moved to before preprocessing since that directive
     * gets stripped during preprocessing. */
    if (detect_objfpc_mode(buffer, length))
        g_objfpc_mode_detected = true;

    const char *dump_path = kgpc_getenv("KGPC_DUMP_PREPROCESSED");
    if (dump_path != NULL && dump_path[0] != '\0')
    {
        /* Append file name to dump path to distinguish multiple parses */
        char dump_full[512];
        const char *basename = strrchr(path, '/');
        basename = basename ? basename + 1 : path;
        snprintf(dump_full, sizeof(dump_full), "%s_%s", dump_path, basename);
        FILE *dump = fopen(dump_full, "w");
        if (dump != NULL)
        {
            fwrite(buffer, 1, length, dump);
            fclose(dump);
        }
    }

    bool is_unit = buffer_starts_with_unit(buffer, length);
    combinator_t *parser = is_unit ? get_or_create_unit_parser() : get_or_create_program_parser();

    input_t *input = new_input();
    input->buffer = buffer;
    input->length = (int)length;

    if (ast_nil == NULL)
    {
        ast_nil = new_ast();
        ast_nil->typ = PASCAL_T_NONE;
    }

    file_to_parse = (char *)path;

    ParseResult result = parse(input, parser);

    if (kgpc_getenv("KGPC_DEBUG_TFPG_AST") != NULL && result.is_success && result.value.ast != NULL)
    {
        fprintf(stderr, "==== Raw cparser AST for %s ====\n", path);
        print_pascal_ast(result.value.ast);
        fprintf(stderr, "==== End AST ====\n");
    }
    Tree_t *tree = NULL;
    bool success = false;
    if (!result.is_success)
    {
        // Create context for the error before freeing input (performance optimization)
        if (result.value.error != NULL && input != NULL)
        {
            ensure_parse_error_contexts(result.value.error, input);
        }
        
        if (error_out != NULL)
            *error_out = result.value.error;
        else if (result.value.error != NULL)
            free_error(result.value.error);
    }
    else
    {
        int remaining = skip_pascal_layout_preview(input, input->start);
        if (remaining < input->length)
        {
            ParseError *err = (ParseError *)calloc(1, sizeof(ParseError));
            if (err != NULL)
            {
                err->line = input->line;
                err->col = input->col;
                err->index = remaining;
                err->message = strdup("Unexpected trailing input after program.");
                err->parser_name = strdup("pascal_frontend");
                err->committed = true;
                ensure_parse_error_contexts(err, input);
                if (error_out != NULL)
                    *error_out = err;
                else
                    free_error(err);
            }
            free_ast(result.value.ast);
            drain_parser_parse_pools();
            free_input(input);
            free(buffer);
            return false;
        }

        if (convert_to_tree)
        {
            /* Save unit ASTs only. Top-level programs should always reparse from source. */
            if (cache_units_only)
            {
                char *save_cache_path = compute_ast_cache_path(path);
                if (save_cache_path != NULL && result.value.ast != NULL)
                {
                    ast_cache_save(save_cache_path, result.value.ast, buffer, length);
                }
                free(save_cache_path);
            }

            tree = tree_from_pascal_ast(result.value.ast);
            if (tree == NULL)
            {
                fprintf(stderr, "Error: Failed to convert AST for '%s' to legacy parse tree.\n", path);
            }
            else
            {
                success = true;
            }
        }
        else
        {
            success = true;
        }

        free_ast(result.value.ast);
        drain_parser_parse_pools();
    }

    free(buffer);
    free_input(input);
    /* Drain AST/error free lists to return memory to the OS.
     * Without this, pooled nodes accumulate across hundreds of units
     * and inflate peak RSS by hundreds of MB for large compilations. */
    drain_parser_parse_pools();
#if !defined(_WIN32) && defined(__GLIBC__)
    /* Ask glibc to return freed heap pages to the OS.  Without this,
     * fragmentation causes peak RSS to stay high even after large
     * per-unit allocations are freed (e.g. parsing a 1 MB unit creates
     * millions of small malloc's; after free(), those pages remain
     * mapped). */
    malloc_trim(0);
#endif
    /* Don't reset file_to_parse to NULL - it's needed for semantic error reporting */
    /* file_to_parse = NULL; */
    // Don't free parser - it's cached for reuse

    if (!success && tree != NULL)
    {
        destroy_tree(tree);
        tree = NULL;
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

void pascal_print_parse_error(const char *path, const ParseError *err)
{
    if (err == NULL)
        return;

    const char *display_path = (err->source_filename != NULL) ? err->source_filename : path;
    fprintf(stderr, "Parse error in %s:\n", display_path);
    fprintf(stderr, "  Line %d, Column %d: %s\n",
            err->line, err->col,
            err->message ? err->message : "unknown error");
    if (err->unexpected)
        fprintf(stderr, "  Unexpected: %s\n", err->unexpected);
    if (err->context)
        fprintf(stderr, "  %s", err->context);
}
