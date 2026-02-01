#include "pascal_frontend.h"

#include <ctype.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <strings.h>
#endif

#include "ErrVars.h"

/* Global storage for user-defined preprocessor configuration */
#define MAX_USER_INCLUDE_PATHS 64
#define MAX_USER_DEFINES 128

static char *g_user_include_paths[MAX_USER_INCLUDE_PATHS];
static int g_user_include_path_count = 0;

static char *g_user_defines[MAX_USER_DEFINES];
static int g_user_define_count = 0;
static char *g_last_parse_path = NULL;

/* Flag set when {$MODE objfpc} is detected in the current parse.
 * Used to automatically inject ObjPas unit dependency. */
static bool g_objfpc_mode_detected = false;
static bool g_default_shortstring = false;

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

static int detect_shortstring_default(const char *buffer, size_t length, bool *out_shortstring)
{
    if (buffer == NULL || length == 0 || out_shortstring == NULL)
        return 0;

    const char *pos = buffer;
    const char *end = buffer + length;
    int found = 0;
    bool value = false;

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
                        value = (*next == '-');
                        found = 1;
                    }
                }
            }

            pos = dir_end;
        }
        pos++;
    }

    if (found)
        *out_shortstring = value;
    return found;
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

bool pascal_frontend_default_shortstring(void)
{
    return g_default_shortstring;
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

extern ast_t *ast_nil;

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
    }
    return cached_unit_parser;
}

static combinator_t *get_or_create_program_parser(void)
{
    if (cached_program_parser == NULL)
    {
        cached_program_parser = new_combinator();
        init_pascal_complete_program_parser(&cached_program_parser);
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
    if (cached_unit_parser != NULL)
    {
        free_combinator(cached_unit_parser);
        cached_unit_parser = NULL;
    }
    if (cached_program_parser != NULL)
    {
        free_combinator(cached_program_parser);
        cached_program_parser = NULL;
    }
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
}

bool pascal_parse_source(const char *path, bool convert_to_tree, Tree_t **out_tree, ParseError **error_out)
{
    if (error_out != NULL)
        *error_out = NULL;

    ensure_generic_registry();

    if (g_last_parse_path != NULL)
    {
        free(g_last_parse_path);
        g_last_parse_path = NULL;
    }
    if (path != NULL)
        g_last_parse_path = strdup(path);

    size_t length = 0;
    char *buffer = read_file(path, &length);
    if (buffer == NULL)
        return false;

    /* Detect {$H+/-} directive on original source BEFORE preprocessing,
     * since the preprocessor strips directive comments. */
    detect_shortstring_default(buffer, length, &g_default_shortstring);

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
        "FPC_RELEASE := 2", "maxExitCode := 255", 
        "FPC_STACKALIGNMENT := 16", "SizeIndexBits := 64", "FixedBitPos := 64", 
        "VarSizeQuant := 8", "FirstVarStepP2 := 3", "MaxVarHeaderAndPayload := 255", 
        "MaxFixedHeaderAndPayload := 255", "VarSizesCount := 32", 
        "MinSearchableVarHeaderAndPayload := 16", "MinFixedHeaderAndPayload := 16", 
        "CommonHeaderSize := 8",
        // FPC feature flags
        "FPC_HAS_FEATURE_DYNARRAYS", "FPC_HAS_FEATURE_ANSISTRINGS", 
        "FPC_HAS_FEATURE_WIDESTRINGS", "FPC_HAS_UNICODESTRING",
        "FPC_HAS_FEATURE_CLASSES",
        "FPC_HAS_FEATURE_OBJECTS", "FPC_HAS_FEATURE_EXCEPTIONS",
        "FPC_HAS_FEATURE_RTTI", "FPC_HAS_FEATURE_HEAP",
        "FPC_HAS_FEATURE_TEXTIO", "FPC_HAS_FEATURE_FILEIO",
        "FPC_HAS_FEATURE_CONSOLEIO", "FPC_HAS_FEATURE_RANDOM",
        "FPC_HAS_FEATURE_VARIANTS", "FPC_HAS_FEATURE_INITFINAL",
        "FPC_HAS_FEATURE_EXITCODE"
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
    /* Define first_mm_imreg for FPC x86_64 paramgr.pas (CPU register constant from cpubase.pas) */
    if (!pascal_preprocessor_define(preprocessor, "first_mm_imreg := 32"))
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
    if (!pascal_preprocessor_define(preprocessor, "cs_opt_use_load_modify_store := 25"))
    {
        report_preprocessor_error(error_out, path, "unable to define cs_opt_use_load_modify_store");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    /* Bitmask with at least bit 25 set for supported_optimizerswitches */
    if (!pascal_preprocessor_define(preprocessor, "supported_optimizerswitches := 33554432"))
    {
        report_preprocessor_error(error_out, path, "unable to define supported_optimizerswitches");
        pascal_preprocessor_free(preprocessor);
        free(buffer);
        return false;
    }
    /* Define max_operands for x86 (4 operands max in x86 instructions) */
    if (!pascal_preprocessor_define(preprocessor, "max_operands := 4"))
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
    pascal_preprocessor_free(preprocessor);

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
    set_preprocessed_context(buffer, length, path);

    /* Detect {$MODE objfpc} in the preprocessed source.
     * If found, set flag so ObjPas unit can be auto-imported.
     * Note: {$H+/-} detection moved to before preprocessing since that directive
     * gets stripped during preprocessing. */
    if (detect_objfpc_mode(buffer, length))
        g_objfpc_mode_detected = true;

    const char *dump_path = getenv("KGPC_DUMP_PREPROCESSED");
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
    if (getenv("KGPC_DEBUG_TFPG_AST") != NULL && result.is_success && result.value.ast != NULL &&
        path != NULL && (strstr(path, "fgl.p") != NULL || strstr(path, "FGL.p") != NULL))
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
            free_input(input);
            free(buffer);
            return false;
        }

        if (convert_to_tree)
        {
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
    }

    free(buffer);
    free_input(input);
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

    fprintf(stderr, "Parse error in %s:\n", path);
    fprintf(stderr, "  Line %d, Column %d: %s\n",
            err->line, err->col,
            err->message ? err->message : "unknown error");
    if (err->unexpected)
        fprintf(stderr, "  Unexpected: %s\n", err->unexpected);
    if (err->context)
        fprintf(stderr, "  %s", err->context);
}
