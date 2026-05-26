/*
    Damon Gwinn
    Runs the pascal parser on the given file
*/

#ifndef PARSE_PASCAL_H
#define PARSE_PASCAL_H

/*#define DEBUG_BISON*/

#include "ParseTree/tree.h"
#include <stdio.h>
#include <stdlib.h>

Tree_t *ParsePascalOnly(char *file);
Tree_t *ParsePascal(char *file);

#endif
