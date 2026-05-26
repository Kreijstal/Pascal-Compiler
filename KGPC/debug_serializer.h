#ifndef DEBUG_SERIALIZER_H
#define DEBUG_SERIALIZER_H

#include "Parser/ParseTree/tree.h"
#include <stdio.h>

void serialize_expression(FILE *fp, struct Expression *expr);

#endif
