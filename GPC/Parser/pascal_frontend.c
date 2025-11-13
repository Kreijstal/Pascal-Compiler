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
#include "ParseTree/from_cparser.h"
#include "ParseTree/tree.h"
#include "pascal_preprocessor.h"
#include "../flags.h"
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
}

bool pascal_parse_source(const char *path, bool convert_to_tree, Tree_t **out_tree, ParseError **error_out)
{
    if (error_out != NULL)
        *error_out = NULL;

    size_t length = 0;
    char *buffer = read_file(path, &length);
    if (buffer == NULL)
        return false;

    PascalPreprocessor *preprocessor = pascal_preprocessor_create();
    if (preprocessor == NULL)
    {
        report_preprocessor_error(error_out, path, "unable to initialise preprocessor");
        free(buffer);
        return false;
    }

    // Define our own dialect symbol and FPC for Lazarus-compatible headers
    const char *default_symbols[] = { "GPC", "FPC" };
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

#if INTPTR_MAX >= INT64_MAX
    const char *arch_symbol = "CPU64";
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

    // Early semantic of dialect: reject C-style shift operators '<<' and '>>'
    // Scan preprocessed buffer while skipping strings and comments.
    {
        int line = 1, col = 1;
        const char* cur = buffer;
        const char* end = buffer + length;
        while (cur < end) {
            unsigned char ch = (unsigned char)*cur;
            if (ch == '\'' ) {
                // String literal with doubled quotes escaping
                ++cur; ++col;
                while (cur < end) {
                    if (*cur == '\'' && (cur + 1) < end && cur[1] == '\'') {
                        cur += 2; col += 2; // escaped quote
                        continue;
                    }
                    if (*cur == '\'') { ++cur; ++col; break; }
                    if (*cur == '\n') { ++line; col = 1; ++cur; }
                    else { ++cur; ++col; }
                }
                continue;
            }
            if (ch == '{') {
                ++cur; ++col;
                while (cur < end && *cur != '}') {
                    if (*cur == '\n') { ++line; col = 1; ++cur; }
                    else { ++cur; ++col; }
                }
                if (cur < end) { ++cur; ++col; }
                continue;
            }
            if (ch == '(' && (cur + 1) < end && cur[1] == '*') {
                cur += 2; col += 2;
                while ((cur + 1) < end && !(cur[0] == '*' && cur[1] == ')')) {
                    if (*cur == '\n') { ++line; col = 1; ++cur; }
                    else { ++cur; ++col; }
                }
                if ((cur + 1) < end) { cur += 2; col += 2; }
                else { cur = end; }
                continue;
            }
            if (ch == '/' && (cur + 1) < end && cur[1] == '/') {
                cur += 2; col += 2;
                while (cur < end && *cur != '\n') { ++cur; ++col; }
                continue;
            }
            // Detect forbidden tokens
            if ((ch == '<' && (cur + 1) < end && cur[1] == '<') ||
                (ch == '>' && (cur + 1) < end && cur[1] == '>')) {
                // Build a parse-style error so tests see "parse error" and "expected" in output.
                if (error_out != NULL && *error_out == NULL) {
                    ParseError *err = (ParseError *)calloc(1, sizeof(ParseError));
                    if (err != NULL) {
                        err->line = line;
                        err->col = col;
                        err->index = (int)(cur - buffer);
                        const char* msg = (ch == '<')
                            ? "Unexpected '<<'. Expected 'shl' for left shift."
                            : "Unexpected '>>'. Expected 'shr' for right shift.";
                        err->message = strdup(msg);
                        err->parser_name = strdup("pascal_frontend");
                        err->unexpected = NULL;
                        // Provide a short context slice for nicer printing
                        int ctx_start = err->index - 10; if (ctx_start < 0) ctx_start = 0;
                        int ctx_len = 0; while (ctx_start + ctx_len < (int)length && ctx_len < 40 && buffer[ctx_start + ctx_len] != '\n') ctx_len++;
                        char* ctx = (char*)malloc((size_t)ctx_len + 2);
                        if (ctx) {
                            memcpy(ctx, buffer + ctx_start, (size_t)ctx_len);
                            ctx[ctx_len] = '\n'; ctx[ctx_len + 1] = '\0';
                            err->context = ctx;
                        }
                        err->cause = NULL;
                        err->partial_ast = NULL;
                        err->committed = true;
                        *error_out = err;
                    }
                }
                free(buffer);
                return false;
            }
            if (*cur == '\n') { ++line; col = 1; ++cur; }
            else { ++cur; ++col; }
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
    free(input);
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
