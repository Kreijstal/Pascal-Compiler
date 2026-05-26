#ifndef PASCAL_STATEMENT_KEYWORDS_H
#define PASCAL_STATEMENT_KEYWORDS_H

#include "pascal_statement_keyword_types.h"
#include <stddef.h>

const struct statement_keyword_record *statement_keyword_lookup(const char *str,
                                                                size_t len);

#endif
