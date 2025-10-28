#ifndef GPC_PARSER_ERROR_H
#define GPC_PARSER_ERROR_H

#include <stddef.h>

void parser_error_report(const char *message, const char *yytext, int yyleng);

#endif
