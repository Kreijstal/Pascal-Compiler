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
        memcpy(raw_buffer, expected_start, len);
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

        size_t copy_len = strlen(formatted);
        if (copy_len >= expected_size)
            copy_len = expected_size - 1;
        memcpy(expected, formatted, copy_len);
        expected[copy_len] = '\0';
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

/* Print source code context around an error line */
void print_source_context(const char *file_path, int error_line, int error_col, int num_context_lines)
{
    if (file_path == NULL || error_line <= 0)
        return;

    FILE *fp = fopen(file_path, "r");
    if (fp == NULL)
        return;

    if (num_context_lines < 0)
        num_context_lines = 2; /* Default to 2 lines of context */
    
    if (error_col < 0)
        error_col = 0;

    char buffer[512];
    int current_line = 1;
    int start_line = error_line - num_context_lines;
    int end_line = error_line + num_context_lines;
    
    if (start_line < 1)
        start_line = 1;

    /* Skip to start_line */
    while (current_line < start_line && fgets(buffer, sizeof(buffer), fp) != NULL)
        ++current_line;

    /* Print context lines */
    while (current_line <= end_line && fgets(buffer, sizeof(buffer), fp) != NULL)
    {
        buffer[strcspn(buffer, "\r\n")] = '\0';
        
        if (current_line == error_line)
        {
            /* Highlight the error line */
            fprintf(stderr, "  > %4d | %s\n", current_line, buffer);
            fprintf(stderr, "         | ");
            
            /* Print caret at the error column */
            int col_to_print = error_col > 0 ? error_col : 1;
            for (int i = 1; i < col_to_print; ++i)
                fputc(' ', stderr);
            fprintf(stderr, "^\n");
        }
        else
        {
            fprintf(stderr, "    %4d | %s\n", current_line, buffer);
        }
        
        ++current_line;
    }

    fclose(fp);
}

/* Helper function to parse {#line N "file"} directive and extract the line number.
 * Returns 1 if a valid line directive was found, 0 otherwise.
 * If found, *out_line is set to the line number from the directive. */
static int parse_line_directive(const char *line_buf, size_t line_len, int *out_line)
{
    if (line_buf == NULL || line_len < 8 || out_line == NULL)
        return 0;
    
    /* Skip leading whitespace */
    size_t pos = 0;
    while (pos < line_len && (line_buf[pos] == ' ' || line_buf[pos] == '\t'))
        ++pos;
    
    /* Check for {#line */
    if (pos + 6 > line_len)
        return 0;
    if (line_buf[pos] != '{' || line_buf[pos+1] != '#')
        return 0;
    pos += 2;
    
    /* Skip optional whitespace after {# */
    while (pos < line_len && (line_buf[pos] == ' ' || line_buf[pos] == '\t'))
        ++pos;
    
    /* Check for "line" keyword (case insensitive) */
    if (pos + 4 > line_len)
        return 0;
    if ((line_buf[pos] != 'l' && line_buf[pos] != 'L') ||
        (line_buf[pos+1] != 'i' && line_buf[pos+1] != 'I') ||
        (line_buf[pos+2] != 'n' && line_buf[pos+2] != 'N') ||
        (line_buf[pos+3] != 'e' && line_buf[pos+3] != 'E'))
        return 0;
    pos += 4;
    
    /* Skip whitespace after "line" */
    while (pos < line_len && (line_buf[pos] == ' ' || line_buf[pos] == '\t'))
        ++pos;
    
    /* Parse the line number */
    if (pos >= line_len || !isdigit((unsigned char)line_buf[pos]))
        return 0;
    
    int line_num = 0;
    while (pos < line_len && isdigit((unsigned char)line_buf[pos]))
    {
        line_num = line_num * 10 + (line_buf[pos] - '0');
        ++pos;
    }
    
    *out_line = line_num;
    return 1;
}

int print_source_context_from_buffer(const char *buffer, size_t length,
                                     int error_line, int error_col,
                                     int num_context_lines)
{
    if (buffer == NULL || length == 0 || error_line <= 0)
        return 0;

    if (num_context_lines < 0)
        num_context_lines = 2;
    if (error_col < 0)
        error_col = 0;

    /* First pass: scan the buffer to find the physical line that corresponds
     * to the logical error_line, respecting {#line N} directives */
    int logical_line = 1;      /* Logical line number (from #line directives) */
    int physical_line = 1;     /* Actual line number in the buffer */
    size_t idx = 0;
    
    /* Track where the error line starts in the buffer */
    size_t error_physical_line = 0;
    int found_error_line = 0;
    
    /* We need to collect context lines, so we'll store line info */
    typedef struct {
        size_t start;
        size_t len;
        int logical_num;
        int is_directive;  /* 1 if this line is a #line directive */
    } LineInfo;
    
    /* Allocate space for line info - estimate max lines */
    size_t max_lines = 1000;
    LineInfo *lines = (LineInfo *)calloc(max_lines, sizeof(LineInfo));
    if (lines == NULL)
        return 0;
    
    size_t line_count = 0;
    
    while (idx < length)
    {
        size_t line_start = idx;
        while (idx < length && buffer[idx] != '\n')
            ++idx;
        size_t line_len = idx - line_start;
        
        /* Check if this line is a {#line N} directive */
        int directive_line = 0;
        int is_directive = parse_line_directive(buffer + line_start, line_len, &directive_line);
        
        if (is_directive)
        {
            /* Update logical line number for the NEXT line */
            logical_line = directive_line;
        }
        
        /* Store line info */
        if (line_count < max_lines)
        {
            lines[line_count].start = line_start;
            lines[line_count].len = line_len;
            lines[line_count].logical_num = is_directive ? -1 : logical_line;
            lines[line_count].is_directive = is_directive;
            ++line_count;
        }
        
        /* Check if this is the error line */
        if (!is_directive && logical_line == error_line && !found_error_line)
        {
            error_physical_line = line_count - 1;
            found_error_line = 1;
        }
        
        if (!is_directive)
            ++logical_line;
        
        if (idx < length && buffer[idx] == '\n')
            ++idx;
        ++physical_line;
    }
    
    if (!found_error_line)
    {
        free(lines);
        return 0;
    }
    
    /* Determine the range of physical lines to print */
    int start_physical = (int)error_physical_line - num_context_lines;
    int end_physical = (int)error_physical_line + num_context_lines;
    if (start_physical < 0)
        start_physical = 0;
    if (end_physical >= (int)line_count)
        end_physical = (int)line_count - 1;
    
    int printed_any = 0;
    for (int i = start_physical; i <= end_physical; ++i)
    {
        /* Skip #line directives in output */
        if (lines[i].is_directive)
            continue;
        
        char *line_buf = (char *)malloc(lines[i].len + 1);
        if (line_buf == NULL)
        {
            free(lines);
            return printed_any;
        }
        memcpy(line_buf, buffer + lines[i].start, lines[i].len);
        line_buf[lines[i].len] = '\0';
        
        int display_line = lines[i].logical_num;
        
        if ((size_t)i == error_physical_line)
        {
            fprintf(stderr, "  > %4d | %s\n", display_line, line_buf);
            fprintf(stderr, "         | ");
            int col_to_print = error_col > 0 ? error_col : 1;
            for (int j = 1; j < col_to_print; ++j)
                fputc(' ', stderr);
            fprintf(stderr, "^\n");
        }
        else
        {
            fprintf(stderr, "    %4d | %s\n", display_line, line_buf);
        }
        
        free(line_buf);
        printed_any = 1;
    }
    
    free(lines);
    return printed_any;
}
