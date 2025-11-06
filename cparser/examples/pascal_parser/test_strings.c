#include "../../acutest.h"
#include "../../parser.h"
#include "../../combinators.h" 
#include "pascal_parser.h"

void test_pascal_string_escapes(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p, NULL);

    // Test Pascal-style escaped single quotes
    input_t* input1 = new_input();
    input1->buffer = strdup("'it''s a string'");
    input1->length = strlen("'it''s a string'");

    ParseResult res1 = parse(input1, p);
    if (!res1.is_success) {
        printf("Parse failed: %s\n", res1.value.error->message);
        TEST_ASSERT(false);
        return;
    }
    TEST_ASSERT(res1.value.ast->typ == PASCAL_T_STRING);
    printf("Pascal single quote test: '%s' -> '%s'\n", input1->buffer, res1.value.ast->sym->name);
    printf("Expected: 'it's a string'\n");
    printf("Got:      '%s'\n", res1.value.ast->sym->name);
    TEST_ASSERT(strcmp(res1.value.ast->sym->name, "it's a string") == 0);

    free_ast(res1.value.ast);
    free(input1->buffer);
    free(input1);

    // Test double quoted string with escapes
    input_t* input2 = new_input();
    input2->buffer = strdup("\"hello\\nworld\"");
    input2->length = strlen("\"hello\\nworld\"");

    ParseResult res2 = parse(input2, p);
    TEST_ASSERT(res2.is_success);
    TEST_ASSERT(res2.value.ast->typ == PASCAL_T_STRING);
    printf("Double quote test: '%s' -> contains newline: %s\n", 
           input2->buffer, 
           strchr(res2.value.ast->sym->name, '\n') ? "yes" : "no");

    free_ast(res2.value.ast);
    free(input2->buffer);
    free(input2);

    free_combinator(p);
}

TEST_LIST = {
    { "test_pascal_string_escapes", test_pascal_string_escapes },
    { NULL, NULL }
};
