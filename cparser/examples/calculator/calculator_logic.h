#ifndef CALCULATOR_LOGIC_H
#define CALCULATOR_LOGIC_H

#include "parser.h"

// --- Custom Tags for Calculator ---
typedef enum {
    CALC_T_NONE, CALC_T_INT, CALC_T_ADD, CALC_T_SUB, CALC_T_MUL, CALC_T_DIV, CALC_T_NEG
} calc_tag_t;

// --- Function Declarations ---
void init_calculator_parser(combinator_t** p);
bool eval(ast_t *ast, long *out);
void print_calculator_ast(ast_t* ast);
const char* calc_tag_to_string(tag_t tag);

#endif // CALCULATOR_LOGIC_H
