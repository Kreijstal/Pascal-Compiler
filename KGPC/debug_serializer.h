#ifndef DEBUG_SERIALIZER_H
#define DEBUG_SERIALIZER_H

#include <stdio.h>
#include "Parser/ParseTree/tree.h"

void serialize_expression(FILE *fp, struct Expression *expr);

#endif
