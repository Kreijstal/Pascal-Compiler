#ifndef PASCAL_DECLARATION_H
#define PASCAL_DECLARATION_H

#include "parser.h"
#include "combinators.h"

// Helper function to create parameter parser (reduces code duplication)
combinator_t* create_pascal_param_parser(combinator_t* type_parser);

void init_pascal_procedure_parser(combinator_t** p);
void init_pascal_method_implementation_parser(combinator_t** p);
void init_pascal_complete_program_parser(combinator_t** p);
void init_pascal_unit_parser(combinator_t** p);

#endif // PASCAL_DECLARATION_H
