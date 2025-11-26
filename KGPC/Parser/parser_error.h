#ifndef KGPC_PARSER_ERROR_H
#define KGPC_PARSER_ERROR_H

#include <stddef.h>

void parser_error_report(const char *message, const char *yytext, int yyleng);

/* Print source code context around an error line
 * file_path: path to source file
 * error_line: line number where error occurred
 * error_col: column number where error occurred (0 if unknown)
 * num_context_lines: number of lines to show before and after the error line (default 2)
 */
void print_source_context(const char *file_path, int error_line, int error_col, int num_context_lines);

#endif
