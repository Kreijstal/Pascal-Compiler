#include "acutest.h"
#include "parser.h"
#include "combinators.h"
#include "json_parser.h"
#include <stdio.h>

// --- Test Helpers ---

static void run_json_success_test(const char* text, void (*check_ast)(ast_t*), const char* name) {
    TEST_CASE(name);
    input_t* input = new_input();
    input->buffer = strdup(text);
    input->length = strlen(text);

    combinator_t* p = json_parser();
    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    if (!res.is_success) {
        printf("Failed to parse valid JSON: %s\n", text);
        if (res.value.error) free_error(res.value.error);
    } else {
        TEST_ASSERT(input->start == input->length);
        if (input->start != input->length) {
            printf("Parser did not consume entire input for: %s\n", text);
        }
        if (check_ast) {
            check_ast(res.value.ast);
        }
        free_ast(res.value.ast);
    }

    free_combinator(p);
    free(input->buffer);
    free(input);
}

static void run_json_fail_test(const char* text, const char* name) {
    TEST_CASE(name);
    input_t* input = new_input();
    input->buffer = strdup(text);
    input->length = strlen(text);

    // We need to check that EITHER the parser fails, OR it succeeds but doesn't consume the whole string.
    combinator_t* p = json_parser();
    ParseResult res = parse(input, p);

    bool failed_as_expected = !res.is_success || (input->start != input->length);
    TEST_ASSERT(failed_as_expected);
    if (!failed_as_expected) {
        printf("Parser incorrectly succeeded for: %s\n", text);
    }

    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        if(res.value.error) free_error(res.value.error);
    }

    free_combinator(p);
    free(input->buffer);
    free(input);
}

// --- AST Check Callbacks ---

void check_null(ast_t* ast) { TEST_ASSERT(ast->typ == JSON_T_NONE); }
void check_true(ast_t* ast) { TEST_ASSERT(ast->typ == JSON_T_INT && strcmp(ast->sym->name, "1") == 0); }
void check_false(ast_t* ast) { TEST_ASSERT(ast->typ == JSON_T_INT && strcmp(ast->sym->name, "0") == 0); }
void check_string(ast_t* ast) { TEST_ASSERT(ast->typ == JSON_T_STRING && strcmp(ast->sym->name, "hello world") == 0); }
void check_int(ast_t* ast) { TEST_ASSERT(ast->typ == JSON_T_INT && strcmp(ast->sym->name, "123") == 0); }
void check_float(ast_t* ast) { TEST_ASSERT(ast->typ == JSON_T_INT && strcmp(ast->sym->name, "-123.45") == 0); }
void check_sci(ast_t* ast) { TEST_ASSERT(ast->typ == JSON_T_INT && strcmp(ast->sym->name, "6.022e23") == 0); }
void check_empty_array(ast_t* ast) { TEST_ASSERT(ast->typ == JSON_T_SEQ && ast->child == ast_nil); }
void check_empty_object(ast_t* ast) { TEST_ASSERT(ast->typ == JSON_T_SEQ && ast->child == ast_nil); }

void check_simple_array(ast_t* ast) {
    TEST_ASSERT(ast->typ == JSON_T_SEQ);
    ast_t* child1 = ast->child;
    TEST_ASSERT(child1->typ == JSON_T_INT && strcmp(child1->sym->name, "1") == 0);
    ast_t* child2 = child1->next;
    TEST_ASSERT(child2->typ == JSON_T_STRING && strcmp(child2->sym->name, "two") == 0);
    ast_t* child3 = child2->next;
    TEST_ASSERT(child3->typ == JSON_T_INT && strcmp(child3->sym->name, "1") == 0);
}

void check_simple_object(ast_t* ast) {
    TEST_ASSERT(ast->typ == JSON_T_SEQ);
    ast_t* kv1 = ast->child;
    TEST_ASSERT(kv1->typ == JSON_T_ASSIGN);
    TEST_ASSERT(kv1->child->typ == JSON_T_STRING && strcmp(kv1->child->sym->name, "key") == 0);
    TEST_ASSERT(kv1->child->next->typ == JSON_T_STRING && strcmp(kv1->child->next->sym->name, "value") == 0);
    ast_t* kv2 = kv1->next;
    TEST_ASSERT(kv2->typ == JSON_T_ASSIGN);
    TEST_ASSERT(kv2->child->typ == JSON_T_STRING && strcmp(kv2->child->sym->name, "n") == 0);
    TEST_ASSERT(kv2->child->next->typ == JSON_T_INT && strcmp(kv2->child->next->sym->name, "123") == 0);
}


// --- Test Cases ---

void test_json_successes(void) {
    if (ast_nil == NULL) {
        ast_nil = new_ast();
        ast_nil->typ = JSON_T_NONE;
    }
    run_json_success_test("null", check_null, "null literal");
    run_json_success_test("true", check_true, "true literal");
    run_json_success_test("false", check_false, "false literal");
    run_json_success_test("\"hello world\"", check_string, "string literal");
    run_json_success_test("123", check_int, "integer literal");
    run_json_success_test("-123.45", check_float, "float literal");
    run_json_success_test("6.022e23", check_sci, "scientific literal");
    run_json_success_test("[]", check_empty_array, "empty array");
    run_json_success_test("{}", check_empty_object, "empty object");
    run_json_success_test("[1, \"two\", true]", check_simple_array, "simple array");
    run_json_success_test("{\"key\": \"value\", \"n\": 123}", check_simple_object, "simple object");
}

void test_json_failures(void) {
    if (ast_nil == NULL) {
        ast_nil = new_ast();
        ast_nil->typ = JSON_T_NONE;
    }
    run_json_fail_test("1.", "trailing decimal");
    run_json_fail_test("-", "lone minus");
    run_json_fail_test("1.2.3", "multiple decimals");
    run_json_fail_test("1e", "trailing e");
    run_json_fail_test("1e-", "trailing e-");
    run_json_fail_test("nul", "bad null");
    run_json_fail_test("flase", "bad false");
    run_json_fail_test("ture", "bad true");
    run_json_fail_test("\"hello", "unterminated string");
    run_json_fail_test("[1, 2, ]", "trailing comma in array");
    run_json_fail_test("[1 2]", "missing comma in array");
    run_json_fail_test("[1,", "hanging comma in array");
    run_json_fail_test("[", "unclosed array");
    run_json_fail_test("{\"a\": 1, }", "trailing comma in object");
    run_json_fail_test("{\"a\": 1 \"b\": 2}", "missing comma in object");
    run_json_fail_test("{\"a\": }", "missing value in object");
    run_json_fail_test("{\"a\"", "missing colon in object");
    run_json_fail_test("{", "unclosed object");
    run_json_fail_test("{\"a\": 1", "unclosed object with value");
}

TEST_LIST = {
    { "json_successes", test_json_successes },
    { "json_failures", test_json_failures },
    { NULL, NULL }
};
