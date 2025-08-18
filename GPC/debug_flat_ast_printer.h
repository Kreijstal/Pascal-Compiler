#ifndef DEBUG_FLAT_AST_PRINTER_H
#define DEBUG_FLAT_AST_PRINTER_H

#include "Parser/flat_ast.h"
#include <stdio.h>

void print_flat_ast(FlatNode *node, FILE *f);

#endif // DEBUG_FLAT_AST_PRINTER_H
