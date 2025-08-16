#ifndef DEBUG_SERIALIZER_H
#define DEBUG_SERIALIZER_H

#include <stdio.h>
#include "Parser/ParseTree/tree.h"

void serialize_expression(FILE *log_file, struct Expression *expr);

#endif // DEBUG_SERIALIZER_H
