#ifndef DEBUG_DESERIALIZER_H
#define DEBUG_DESERIALIZER_H

#include "Parser/ParseTree/tree.h"
#include <stdio.h>

struct Expression *deserialize_expression(FILE *fp);

#endif
