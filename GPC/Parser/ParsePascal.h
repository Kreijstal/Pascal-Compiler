/*
    Damon Gwinn
    Runs the pascal parser on the given file
*/

#ifndef PARSE_PASCAL_H
#define PARSE_PASCAL_H

/*#define DEBUG_BISON*/

#include <stdio.h>
#include <stdlib.h>
#include "ParseTree/tree.h"

// The root of the AST
extern FlatNode *parse_tree;

// Globals defined in ParsePascal.c, needed by lexer/parser
extern int line_num;
extern int col_num;
extern char *file_to_parse;

FlatNode *ParsePascalOnly(char *file);
FlatNode *ParsePascal(char *file);

#endif
