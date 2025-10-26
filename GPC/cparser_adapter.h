/*
 * cparser_adapter.h
 * Adapter to convert cparser AST to GPC parse tree format
 */

#ifndef CPARSER_ADAPTER_H
#define CPARSER_ADAPTER_H

#include "parser.h"
#include "combinators.h"
#include "pascal_parser.h"
#include "Parser/ParseTree/tree.h"

// Convert cparser AST to GPC parse tree
Tree_t* cparser_ast_to_tree(ast_t* root);

#endif // CPARSER_ADAPTER_H
