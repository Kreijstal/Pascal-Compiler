#ifndef PASCAL_EXPRESSION_H
#define PASCAL_EXPRESSION_H

#include "parser.h"
#include "combinators.h"

typedef struct { tag_t tag; combinator_t** expr_parser; } set_args;

void init_pascal_expression_parser(combinator_t** p);
ParseResult parse_pascal_expression(input_t* input, combinator_t* parser);
combinator_t* pascal_identifier(tag_t tag);
combinator_t* pascal_expression_identifier(tag_t tag);
combinator_t* real_number(tag_t tag);
combinator_t* char_literal(tag_t tag);
combinator_t* pascal_string(tag_t tag);
combinator_t* set_constructor(tag_t tag, combinator_t** expr_parser);

#endif // PASCAL_EXPRESSION_H
