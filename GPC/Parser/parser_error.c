#include "parser_error.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ErrVars.h"

static void sanitize_token_text(const char *yytext, int yyleng, char *buffer, size_t buffer_size)
{
    if (buffer_size == 0)
        return;

    if (yytext == NULL || yyleng <= 0)
    {
        buffer[0] = '\0';
        return;
    }

    size_t copy_len = (size_t)yyleng;
    if (copy_len >= buffer_size)
        copy_len = buffer_size - 1;

    for (size_t i = 0; i < copy_len; ++i)
    {
        unsigned char ch = (unsigned char)yytext[i];
        buffer[i] = (char)(isprint(ch) ? ch : '?');
    }
    buffer[copy_len] = '\0';
}

static void trim_token(char *token)
{
    if (token == NULL)
        return;

    char *start = token;
    while (*start == ' ' || *start == '\t' || *start == ',')
        ++start;

    char *end = token + strlen(token);
    while (end > start && (end[-1] == ' ' || end[-1] == '\t' || end[-1] == ','))
        --end;

    size_t len = (size_t)(end - start);
    if (start != token)
        memmove(token, start, len);
    token[len] = '\0';
}

static void normalize_token_case(char *token)
{
    for (char *p = token; *p != '\0'; ++p)
        *p = (char)tolower((unsigned char)*p);
}

static const char *friendly_token_name(const char *token)
{
    if (token == NULL)
        return "";

    if (strcmp(token, "$end") == 0)
        return "end of file";
    if (strcmp(token, "identifier") == 0)
        return "identifier";

    return token;
}

static void extract_token_sections(const char *message, char *unexpected, size_t unexpected_size,
                                   char *expected, size_t expected_size)
{
    if (unexpected_size > 0)
        unexpected[0] = '\0';
    if (expected_size > 0)
        expected[0] = '\0';

    if (message == NULL)
        return;

    const char *unexpected_start = strstr(message, "unexpected ");
    if (unexpected_start != NULL)
    {
        unexpected_start += strlen("unexpected ");
        const char *unexpected_end = strchr(unexpected_start, ',');
        size_t len = unexpected_end != NULL ? (size_t)(unexpected_end - unexpected_start)
                                            : strlen(unexpected_start);
        if (len >= unexpected_size)
            len = unexpected_size - 1;
        strncpy(unexpected, unexpected_start, len);
        unexpected[len] = '\0';
        trim_token(unexpected);
        normalize_token_case(unexpected);
    }

    const char *expected_start = strstr(message, "expecting ");
    if (expected_start != NULL)
    {
        expected_start += strlen("expecting ");

        char raw_buffer[512];
        size_t len = strlen(expected_start);
        if (len >= sizeof(raw_buffer))
            len = sizeof(raw_buffer) - 1;
        strncpy(raw_buffer, expected_start, len);
        raw_buffer[len] = '\0';

        char formatted[512] = "";
        char *cursor = raw_buffer;
        int first = 1;
        while (*cursor != '\0')
        {
            char *next = strstr(cursor, " or ");
            size_t segment_len = next != NULL ? (size_t)(next - cursor) : strlen(cursor);
            char token_buffer[128];
            if (segment_len >= sizeof(token_buffer))
                segment_len = sizeof(token_buffer) - 1;
            strncpy(token_buffer, cursor, segment_len);
            token_buffer[segment_len] = '\0';

            trim_token(token_buffer);
            normalize_token_case(token_buffer);
            const char *friendly = friendly_token_name(token_buffer);

            if (!first)
                strncat(formatted, ", ", sizeof(formatted) - strlen(formatted) - 1);
            strncat(formatted, friendly, sizeof(formatted) - strlen(formatted) - 1);
            first = 0;

            if (next == NULL)
                break;
            cursor = next + 4;
        }

        strncpy(expected, formatted, expected_size - 1);
        expected[expected_size - 1] = '\0';
    }
}

static void print_context_line(const char *file_path, int error_line, int caret_column)
{
    if (file_path == NULL)
        return;

    FILE *fp = fopen(file_path, "r");
    if (fp == NULL)
        return;

    char buffer[512];
    int current_line = 1;
    while (current_line < error_line && fgets(buffer, sizeof(buffer), fp) != NULL)
        ++current_line;

    if (current_line == error_line && fgets(buffer, sizeof(buffer), fp) != NULL)
    {
        buffer[strcspn(buffer, "\r\n")] = '\0';
        fprintf(stderr, "  %s\n", buffer);
        fprintf(stderr, "  ");
        for (int i = 1; i < caret_column; ++i)
            fputc(' ', stderr);
        fprintf(stderr, "^\n");
    }

    fclose(fp);
}

void parser_error_report(const char *message, const char *yytext, int yyleng)
{
    char sanitized[128];
    sanitize_token_text(yytext, yyleng, sanitized, sizeof(sanitized));

    int caret_column = col_num;
    if (yyleng > 0 && col_num >= yyleng)
        caret_column = col_num - yyleng + 1;

    const char *file_path = (file_to_parse != NULL && *file_to_parse != '\0') ? file_to_parse : "<input>";

    fprintf(stderr, "Syntax error in %s at line %d, column %d\n", file_path, line_num, caret_column);

    char unexpected[128];
    char expected[256];
    extract_token_sections(message, unexpected, sizeof(unexpected), expected, sizeof(expected));

    if (unexpected[0] != '\0')
    {
        const char *friendly = friendly_token_name(unexpected);
        if (sanitized[0] != '\0')
            fprintf(stderr, "  Unexpected %s \"%s\"\n", friendly, sanitized);
        else
            fprintf(stderr, "  Unexpected %s\n", friendly);
    }
    else if (sanitized[0] != '\0')
    {
        fprintf(stderr, "  Problematic token: \"%s\"\n", sanitized);
    }

    if (expected[0] != '\0')
        fprintf(stderr, "  Expected: %s\n", expected);

    print_context_line(file_path, line_num, caret_column);

    if (message != NULL && strstr(message, "syntax error") == NULL)
        fprintf(stderr, "  Parser detail: %s\n", message);

    fputc('\n', stderr);
}
