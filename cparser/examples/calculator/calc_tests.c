#include "acutest.h"
#include "parser.h"
#include "combinators.h"
#include "calculator_logic.h"
#include <stdio.h>

void test_calc_valid_expression(void) {
    combinator_t* p = new_combinator();
    init_calculator_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("1 + 2");
    input->length = 5;

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    long value = 0;
    TEST_ASSERT(eval(res.value.ast, &value));
    TEST_ASSERT(value == 3);

    free_ast(res.value.ast);
    free_combinator(p);
    free(input->buffer);
    free_input(input);
}

void test_calc_invalid_expression(void) {
    combinator_t* p = new_combinator();
    init_calculator_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("1 + * 2");
    input->length = 7;

    ParseResult res = parse(input, p);

    TEST_ASSERT(!res.is_success);
    TEST_ASSERT(res.value.error->partial_ast != NULL);
    TEST_ASSERT(res.value.error->partial_ast->typ == CALC_T_ADD);

    free_error(res.value.error);
    free_combinator(p);
    free(input->buffer);
    free_input(input);
}

TEST_LIST = {
    { "test_calc_valid_expression", test_calc_valid_expression },
    { "test_calc_invalid_expression", test_calc_invalid_expression },
    { NULL, NULL }
};
