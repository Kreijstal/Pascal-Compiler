#ifndef PASCAL_FRONTEND_H
#define PASCAL_FRONTEND_H

#include <stdbool.h>

#include "parser.h"

struct Tree;
typedef struct Tree Tree_t;

bool pascal_parse_source(const char *path, bool convert_to_tree, Tree_t **out_tree, ParseError **error_out);
void pascal_print_parse_error(const char *path, const ParseError *err);
void pascal_frontend_cleanup(void);

#endif
