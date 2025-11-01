#include "pascal_frontend.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <strings.h>
#endif

#include "ErrVars.h"
#include "ParseTree/from_cparser.h"
#include "ParseTree/tree.h"
#include "pascal_preprocessor.h"

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
    cached_unit_parser = NULL;
    cached_program_parser = NULL;
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

    const char *default_symbols[] = { "GPC" };
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
        if (error_out != NULL)
            *error_out = result.value.error;
        else if (result.value.error != NULL)
            free_error(result.value.error);
    }
    else
    {
        if (input->start < input->length)
        {
            fprintf(stderr,
                    "Warning: Parser did not consume entire input for %s (at position %d of %d)\n",
                    path, input->start, input->length);
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
    file_to_parse = NULL;
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
    if (err->context && err->context[0] != '\0')
    {
        fprintf(stderr, "  Context:\n");
        const char *cursor = err->context;
        while (*cursor != '\0')
        {
            const char *newline = strchr(cursor, '\n');
            if (newline != NULL)
            {
                fprintf(stderr, "    %.*s\n", (int)(newline - cursor), cursor);
                cursor = newline + 1;
            }
            else
            {
                fprintf(stderr, "    %s\n", cursor);
                break;
            }
        }
    }
}
