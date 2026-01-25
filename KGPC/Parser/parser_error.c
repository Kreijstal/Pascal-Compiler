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

/* Helper to check if a line is a {#line N "file"} directive and extract the line number.
 * Returns the new line number if it's a directive, or -1 if not. */
/* Parse {#line N "filename"} directive and extract line number and optional filename */
static int parse_line_directive_with_file(const char *line, size_t len, char *filename_out, size_t filename_size)
{
    if (filename_out != NULL && filename_size > 0)
        filename_out[0] = '\0';
        
    if (len < 8) return -1;  /* Minimum: {#line 1} */
    if (line[0] != '{' || line[1] != '#') return -1;
    if (len < 7 || strncasecmp(line + 2, "line", 4) != 0) return -1;
    
    size_t pos = 6;
    /* Skip whitespace */
    while (pos < len && (line[pos] == ' ' || line[pos] == '\t'))
        ++pos;
    
    /* Parse line number */
    int line_num = 0;
    while (pos < len && line[pos] >= '0' && line[pos] <= '9') {
        line_num = line_num * 10 + (line[pos] - '0');
        ++pos;
    }
    
    /* Skip whitespace before optional filename */
    while (pos < len && (line[pos] == ' ' || line[pos] == '\t'))
        ++pos;
    
    /* Parse optional filename in quotes */
    if (pos < len && line[pos] == '"' && filename_out != NULL && filename_size > 0) {
        ++pos;  /* Skip opening quote */
        size_t fname_start = pos;
        while (pos < len && line[pos] != '"')
            ++pos;
        size_t fname_len = pos - fname_start;
        if (fname_len > 0 && fname_len < filename_size) {
            memcpy(filename_out, line + fname_start, fname_len);
            filename_out[fname_len] = '\0';
        }
    }
    
    return line_num > 0 ? line_num : -1;
}

static int parse_line_directive(const char *line, size_t len)
{
    return parse_line_directive_with_file(line, len, NULL, 0);
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

    int current_line = 1;
    int start_line = error_line - num_context_lines;
    int end_line = error_line + num_context_lines;
    if (start_line < 1)
        start_line = 1;

    size_t idx = 0;
    char current_file[512] = "";  /* Track current source file from {#line} directives */
    char error_file[512] = "";    /* File name for the error line */
    
    /* First pass: find the position where current_line matches start_line,
     * respecting {#line} directives that adjust the line number */
    while (current_line < start_line && idx < length)
    {
        /* Check for {#line N "file"} directive at start of line */
        size_t line_start = idx;
        while (idx < length && buffer[idx] != '\n')
            ++idx;
        size_t line_len = idx - line_start;
        
        char directive_file[512] = "";
        int directive_line = parse_line_directive_with_file(buffer + line_start, line_len, directive_file, sizeof(directive_file));
        if (directive_line >= 0) {
            /* This is a line directive - the NEXT line will be directive_line */
            if (idx < length && buffer[idx] == '\n')
                ++idx;
            current_line = directive_line;
            if (directive_file[0] != '\0') {
                strncpy(current_file, directive_file, sizeof(current_file) - 1);
                current_file[sizeof(current_file) - 1] = '\0';
            }
            continue;
        }
        
        if (idx < length && buffer[idx] == '\n')
            ++idx;
        ++current_line;
    }

    if (current_line < start_line)
        return 0;

    int printed_any = 0;
    int printed_file_header = 0;
    
    while (current_line <= end_line && idx < length)
    {
        size_t line_start = idx;
        while (idx < length && buffer[idx] != '\n')
            ++idx;
        size_t line_len = idx - line_start;
        
        /* Check for {#line N "file"} directive */
        char directive_file[512] = "";
        int directive_line = parse_line_directive_with_file(buffer + line_start, line_len, directive_file, sizeof(directive_file));
        if (directive_line >= 0) {
            /* Skip the directive line, don't print it */
            if (idx < length && buffer[idx] == '\n')
                ++idx;
            current_line = directive_line;
            if (directive_file[0] != '\0') {
                strncpy(current_file, directive_file, sizeof(current_file) - 1);
                current_file[sizeof(current_file) - 1] = '\0';
            }
            continue;
        }

        /* Remember the file for the error line */
        if (current_line == error_line && current_file[0] != '\0') {
            strncpy(error_file, current_file, sizeof(error_file) - 1);
            error_file[sizeof(error_file) - 1] = '\0';
        }

        /* Print file header if we have a file name and haven't printed it yet */
        if (!printed_file_header && current_file[0] != '\0') {
            fprintf(stderr, "  In %s:\n", current_file);
            printed_file_header = 1;
        }

        char *line_buf = (char *)malloc(line_len + 1);
        if (line_buf == NULL)
            return printed_any;
        memcpy(line_buf, buffer + line_start, line_len);
        line_buf[line_len] = '\0';

        if (current_line == error_line)
        {
            fprintf(stderr, "  > %4d | %s\n", current_line, line_buf);
            fprintf(stderr, "         | ");
            int col_to_print = error_col > 0 ? error_col : 1;
            for (int i = 1; i < col_to_print; ++i)
                fputc(' ', stderr);
            fprintf(stderr, "^\n");
        }
        else
        {
            fprintf(stderr, "    %4d | %s\n", current_line, line_buf);
        }

        free(line_buf);
        printed_any = 1;

        if (idx < length && buffer[idx] == '\n')
            ++idx;
        ++current_line;
    }

    return printed_any;
}

/* Print source context using byte offset for accurate positioning */
int print_source_context_at_offset(const char *buffer, size_t length,
                                   int source_offset, int error_line, int error_col,
                                   int num_context_lines)
{
    /* If no valid offset, fall back to line-based search */
    if (source_offset < 0 || (size_t)source_offset >= length)
        return print_source_context_from_buffer(buffer, length, error_line, error_col, num_context_lines);

    if (buffer == NULL || length == 0)
        return 0;

    if (num_context_lines < 0)
        num_context_lines = 2;
    if (error_col < 0)
        error_col = 0;

    /* Find the line number and file at source_offset by scanning backwards for #line directives */
    char current_file[512] = "";
    int current_line_at_offset = 1;  /* Default line number if no directive found */

    /* Scan backwards from source_offset to find the most recent #line directive */
    int scan_pos = source_offset;
    while (scan_pos > 0) {
        /* Find start of current line */
        int line_start = scan_pos;
        while (line_start > 0 && buffer[line_start - 1] != '\n')
            --line_start;

        /* Check if this line is a #line directive */
        size_t line_len = 0;
        int temp_pos = line_start;
        while ((size_t)temp_pos < length && buffer[temp_pos] != '\n') {
            ++temp_pos;
            ++line_len;
        }

        char directive_file[512] = "";
        int directive_line = parse_line_directive_with_file(buffer + line_start, line_len,
                                                            directive_file, sizeof(directive_file));
        if (directive_line >= 0) {
            /* Found a directive - count lines from here to source_offset */
            int lines_after_directive = 0;
            int pos = line_start;
            /* Skip the directive line itself */
            while ((size_t)pos < length && buffer[pos] != '\n')
                ++pos;
            if ((size_t)pos < length && buffer[pos] == '\n')
                ++pos;
            /* Now count newlines from after directive to source_offset */
            while (pos < source_offset) {
                if (buffer[pos] == '\n')
                    ++lines_after_directive;
                ++pos;
            }
            current_line_at_offset = directive_line + lines_after_directive;
            if (directive_file[0] != '\0') {
                strncpy(current_file, directive_file, sizeof(current_file) - 1);
                current_file[sizeof(current_file) - 1] = '\0';
            }
            break;
        }

        /* Move to previous line */
        if (line_start > 0)
            scan_pos = line_start - 1;
        else
            break;
    }

    /* Now find the start of the error line (the line containing source_offset) */
    int error_line_start = source_offset;
    while (error_line_start > 0 && buffer[error_line_start - 1] != '\n')
        --error_line_start;

    /* Calculate start and end line numbers for context */
    int actual_error_line = (error_line > 0) ? error_line : current_line_at_offset;
    int start_line = actual_error_line - num_context_lines;
    int end_line = actual_error_line + num_context_lines;
    if (start_line < 1)
        start_line = 1;

    /* Find the starting position for context display */
    /* Go back num_context_lines from error_line_start */
    int context_start = error_line_start;
    int lines_back = 0;
    while (context_start > 0 && lines_back < num_context_lines) {
        --context_start;
        if (buffer[context_start] == '\n')
            ++lines_back;
    }
    if (context_start > 0 && buffer[context_start] == '\n')
        ++context_start;  /* Skip the newline we landed on */

    /* Print file header */
    int printed_any = 0;
    if (current_file[0] != '\0') {
        fprintf(stderr, "  In %s:\n", current_file);
    }

    /* Print context lines */
    size_t idx = (size_t)context_start;
    int line_num = actual_error_line - lines_back;
    if (line_num < 1) line_num = 1;

    while (line_num <= end_line && idx < length) {
        size_t line_start_pos = idx;
        while (idx < length && buffer[idx] != '\n')
            ++idx;
        size_t line_len = idx - line_start_pos;

        /* Skip #line directive lines */
        char directive_file[512] = "";
        int directive_line = parse_line_directive_with_file(buffer + line_start_pos, line_len,
                                                            directive_file, sizeof(directive_file));
        if (directive_line >= 0) {
            if (idx < length && buffer[idx] == '\n')
                ++idx;
            line_num = directive_line;
            if (directive_file[0] != '\0') {
                strncpy(current_file, directive_file, sizeof(current_file) - 1);
                current_file[sizeof(current_file) - 1] = '\0';
            }
            continue;
        }

        char *line_buf = (char *)malloc(line_len + 1);
        if (line_buf == NULL)
            return printed_any;
        memcpy(line_buf, buffer + line_start_pos, line_len);
        line_buf[line_len] = '\0';

        if (line_num == actual_error_line) {
            fprintf(stderr, "  > %4d | %s\n", line_num, line_buf);
            fprintf(stderr, "         | ");
            int col_to_print = error_col > 0 ? error_col : 1;
            for (int i = 1; i < col_to_print; ++i)
                fputc(' ', stderr);
            fprintf(stderr, "^\n");
        } else {
            fprintf(stderr, "    %4d | %s\n", line_num, line_buf);
        }

        free(line_buf);
        printed_any = 1;

        if (idx < length && buffer[idx] == '\n')
            ++idx;
        ++line_num;
    }

    return printed_any;
}
