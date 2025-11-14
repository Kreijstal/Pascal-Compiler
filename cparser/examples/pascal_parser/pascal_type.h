#ifndef PASCAL_TYPE_H
#define PASCAL_TYPE_H

#include "parser.h"
#include "combinators.h"

combinator_t* range_type(tag_t tag);
combinator_t* array_type(tag_t tag);
combinator_t* record_type(tag_t tag);
combinator_t* enumerated_type(tag_t tag);
combinator_t* class_type(tag_t tag);
combinator_t* interface_type(tag_t tag);
combinator_t* type_name(tag_t tag);
combinator_t* pointer_type(tag_t tag);
combinator_t* set_type(tag_t tag);
combinator_t* file_type(tag_t tag);
combinator_t* procedure_type(tag_t tag);
combinator_t* function_type(tag_t tag);
combinator_t* reference_to_type(tag_t tag);
combinator_t* pascal_identifier_with_subscript(tag_t tag);

#endif // PASCAL_TYPE_H
