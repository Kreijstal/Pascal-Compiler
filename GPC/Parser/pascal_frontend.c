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

bool pascal_parse_source(const char *path, bool convert_to_tree, Tree_t **out_tree, ParseError **error_out)
{
    if (error_out != NULL)
        *error_out = NULL;

    size_t length = 0;
    char *buffer = read_file(path, &length);
    if (buffer == NULL)
        return false;

    combinator_t *parser = new_combinator();
    bool is_unit = buffer_starts_with_unit(buffer, length);
    if (is_unit)
        init_pascal_unit_parser(&parser);
    else
        init_pascal_complete_program_parser(&parser);

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
    free_combinator(parser);

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
}
