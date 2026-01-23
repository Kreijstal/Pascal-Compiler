#ifndef PASCAL_FRONTEND_H
#define PASCAL_FRONTEND_H

#include <stdbool.h>

#include "parser.h"

struct Tree;
typedef struct Tree Tree_t;

bool pascal_parse_source(const char *path, bool convert_to_tree, Tree_t **out_tree, ParseError **error_out);
void pascal_print_parse_error(const char *path, const ParseError *err);
void pascal_frontend_cleanup(void);

/* User-configurable preprocessor settings */
void pascal_frontend_add_include_path(const char *path);
void pascal_frontend_add_define(const char *define);
void pascal_frontend_clear_user_config(void);

/* Get the list of user-defined include paths for unit search */
const char * const *pascal_frontend_get_include_paths(int *count);
const char *pascal_frontend_current_path(void);

/* ObjFPC mode detection - used for automatic ObjPas import */
bool pascal_frontend_is_objfpc_mode(void);
void pascal_frontend_reset_objfpc_mode(void);
bool pascal_frontend_default_shortstring(void);

#endif
