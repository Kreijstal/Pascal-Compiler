#ifndef FROM_CPARSER_H
#define FROM_CPARSER_H

#include "parser.h"
#include "pascal_parser.h"
#include "tree.h"

Tree_t *tree_from_pascal_ast(ast_t *program_ast);

#endif /* FROM_CPARSER_H */
