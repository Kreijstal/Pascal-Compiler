#ifndef DEBUG_DESERIALIZER_H
#define DEBUG_DESERIALIZER_H

#include <stdio.h>
#include "Parser/ParseTree/tree.h"

struct Expression *deserialize_expression(FILE *fp);

#endif
