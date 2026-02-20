#ifndef PASCAL_STATEMENT_H
#define PASCAL_STATEMENT_H

#include "parser.h"
#include "combinators.h"

void init_pascal_statement_parser(combinator_t** p);
combinator_t* make_pascal_stmt_list_parser(combinator_t** stmt_parser);

#endif // PASCAL_STATEMENT_H
