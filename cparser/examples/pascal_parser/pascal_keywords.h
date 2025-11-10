#ifndef PASCAL_KEYWORDS_H
#define PASCAL_KEYWORDS_H

#include <stdbool.h>
#include "parser.h"
#include "combinators.h"

typedef struct { char* str; } match_args;

extern const char* pascal_reserved_keywords[];
bool is_pascal_keyword(const char* str);
bool pascal_keyword_allowed_in_expression(const char* str);
combinator_t* keyword_ci(const char* str);
combinator_t* create_keyword_parser(const char* keyword_str, tag_t tag);

#endif // PASCAL_KEYWORDS_H
