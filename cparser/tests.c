#include "acutest.h"
#include "parser.h"
#include "combinators.h"
#include <stdio.h>

// Declare wrap_failure_with_ast function
ParseResult wrap_failure_with_ast(input_t* in, char* message, ParseResult original_result, ast_t* partial_ast);

// --- Custom Tags for Tests ---
typedef enum {
    TEST_T_NONE, TEST_T_INT, TEST_T_IDENT, TEST_T_ADD, TEST_T_SUB, TEST_T_MUL, TEST_T_DIV
} test_tag_t;

// Helper function to print AST with indentation
static void print_ast_indented(ast_t* ast, int depth) {
    if (ast == NULL) return;
    
    for (int i = 0; i < depth; i++) printf("  ");
    
    switch (ast->typ) {
        case TEST_T_NONE: printf("NONE"); break;
        case TEST_T_INT: printf("INT(%s)", ast->sym ? ast->sym->name : "NULL"); break;
        case TEST_T_IDENT: printf("IDENT(%s)", ast->sym ? ast->sym->name : "NULL"); break;
        case TEST_T_ADD: printf("ADD"); break;
        case TEST_T_SUB: printf("SUB"); break;
        case TEST_T_MUL: printf("MUL"); break;
        case TEST_T_DIV: printf("DIV"); break;
        default:
            fprintf(stderr, "FATAL: Unknown AST node type: %d in %s at %s:%d\n", ast->typ, __func__, __FILE__, __LINE__);
            abort();
    }
    printf("\n");
    
    if (ast->child) {
        print_ast_indented(ast->child, depth + 1);
    }
    if (ast->next) {
        print_ast_indented(ast->next, depth);
    }
}

// Helper function to print ParseError with partial AST
static void print_error_with_partial_ast(ParseError* error, int depth) {
    if (error == NULL) return;
    
    for (int i = 0; i < depth; i++) printf("  ");
    printf("Error at line %d, col %d: %s\n", error->line, error->col, error->message);
    
    if (error->partial_ast != NULL) {
        for (int i = 0; i < depth; i++) printf("  ");
        printf("Partial AST:\n");
        print_ast_indented(error->partial_ast, depth + 1);
    }
    
    if (error->cause != NULL) {
        for (int i = 0; i < depth; i++) printf("  ");
        printf("Caused by:\n");
        print_error_with_partial_ast(error->cause, depth + 1);
    }
}





void test_pnot_combinator(void) {
    input_t* input = new_input();
    input->buffer = strdup("hello");
    input->length = 5;
    combinator_t* p1 = pnot(match("world"));
    ParseResult res1 = parse(input, p1);
    TEST_ASSERT(res1.is_success);
    free_combinator(p1);
    combinator_t* p2 = pnot(match("hello"));
    ParseResult res2 = parse(input, p2);
    TEST_ASSERT(!res2.is_success);
    free_error(res2.value.error);
    free_combinator(p2);
    free(input->buffer);
    free(input);
}

void test_peek_combinator(void) {
    input_t* input = new_input();
    input->buffer = strdup("hello");
    input->length = 5;
    combinator_t* p1 = peek(match("hello"));
    ParseResult res1 = parse(input, p1);
    TEST_ASSERT(res1.is_success);
    free_ast(res1.value.ast);
    free_combinator(p1);
    combinator_t* p2 = peek(match("world"));
    ParseResult res2 = parse(input, p2);
    TEST_ASSERT(!res2.is_success);
    free_error(res2.value.error);
    free_combinator(p2);
    free(input->buffer);
    free(input);
}

void test_gseq_combinator(void) {
    input_t* input = new_input();
    input->buffer = strdup("helloworld");
    input->length = 10;
    combinator_t* p1 = gseq(new_combinator(), TEST_T_NONE, match("hello"), match("world"), NULL);
    ParseResult res1 = parse(input, p1);
    TEST_ASSERT(res1.is_success);
    free_ast(res1.value.ast);
    free_combinator(p1);
    input->start = 0;
    combinator_t* p2 = gseq(new_combinator(), TEST_T_NONE, match("hello"), match("goodbye"), NULL);
    ParseResult res2 = parse(input, p2);
    TEST_ASSERT(!res2.is_success);
    free_error(res2.value.error);
    free_combinator(p2);
    free(input->buffer);
    free(input);
}

void test_between_combinator(void) {
    input_t* input = new_input();
    input->buffer = strdup("(hello)");
    input->length = 7;
    combinator_t* p = between(match("("), match(")"), cident(TEST_T_IDENT));
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, "hello") == 0);
    free_ast(res.value.ast);
    free_combinator(p);
    free(input->buffer);
    free(input);
}

void test_sep_by_combinator(void) {
    input_t* input = new_input();
    input->buffer = strdup("a,b,c");
    input->length = 5;
    combinator_t* p = sep_by(cident(TEST_T_IDENT), match(","));
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    ast_t* ast = res.value.ast;
    TEST_ASSERT(strcmp(ast->sym->name, "a") == 0);
    ast = ast->next;
    TEST_ASSERT(strcmp(ast->sym->name, "b") == 0);
    ast = ast->next;
    TEST_ASSERT(strcmp(ast->sym->name, "c") == 0);
    free_ast(res.value.ast);
    free_combinator(p);
    free(input->buffer);
    free(input);
}

void test_sep_end_by_combinator(void) {
    input_t* input = new_input();
    input->buffer = strdup("a,b,c,");
    input->length = 6;
    combinator_t* p = sep_end_by(cident(TEST_T_IDENT), match(","));
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    ast_t* ast = res.value.ast;
    TEST_ASSERT(strcmp(ast->sym->name, "a") == 0);
    ast = ast->next;
    TEST_ASSERT(strcmp(ast->sym->name, "b") == 0);
    ast = ast->next;
    TEST_ASSERT(strcmp(ast->sym->name, "c") == 0);
    free_ast(res.value.ast);
    free_combinator(p);
    free(input->buffer);
    free(input);
}

static combinator_t* add_op() {
    return right(match("+"), succeed(ast1(TEST_T_ADD, ast_nil)));
}

void test_chainl1_combinator(void) {
    input_t* input = new_input();
    input->buffer = strdup("1+2+3");
    input->length = 5;

    combinator_t* p = chainl1(integer(TEST_T_INT), add_op());
    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    ast_t* ast = res.value.ast;
    TEST_ASSERT(ast->typ == TEST_T_ADD);
    TEST_ASSERT(ast->child->typ == TEST_T_ADD);
    TEST_ASSERT(strcmp(ast->child->next->sym->name, "3") == 0);

    free_ast(res.value.ast);
    free_combinator(p);
    free(input->buffer);
    free(input);
}

void test_any_char_combinator(void) {
    input_t* input = new_input();
    input->buffer = strdup("a");
    input->length = 1;

    combinator_t* p = any_char(TEST_T_NONE);
    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, "a") == 0);

    free_ast(res.value.ast);
    free_combinator(p);
    free(input->buffer);
    free(input);
}

static ast_t* to_uppercase(ast_t* ast) {
    for (int i = 0; ast->sym->name[i]; i++) {
        ast->sym->name[i] = toupper(ast->sym->name[i]);
    }
    return ast;
}

void test_map_combinator(void) {
    input_t* input = new_input();
    input->buffer = strdup("hello");
    input->length = 5;

    combinator_t* p = map(cident(TEST_T_IDENT), to_uppercase);
    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, "HELLO") == 0);

    free_ast(res.value.ast);
    free_combinator(p);
    free(input->buffer);
    free(input);
}

static ParseError* add_context_to_error(ParseError* err) {
    ParseError* new_err = (ParseError*)safe_malloc(sizeof(ParseError));
    new_err->line = err->line;
    new_err->col = err->col;
    new_err->message = strdup("In custom context");
    new_err->cause = err;
    new_err->partial_ast = NULL;
    return new_err;
}

void test_errmap_combinator(void) {
    input_t* input = new_input();
    input->buffer = strdup("world");
    input->length = 5;

    combinator_t* p = errmap(match("hello"), add_context_to_error);
    ParseResult res = parse(input, p);

    TEST_ASSERT(!res.is_success);
    TEST_ASSERT(strcmp(res.value.error->message, "In custom context") == 0);
    TEST_ASSERT(strstr(res.value.error->cause->message, "Expected 'hello'") != NULL);

    free_error(res.value.error);
    free_combinator(p);
    free(input->buffer);
    free(input);
}

static bool is_digit_predicate(char c) {
    return isdigit(c);
}

void test_satisfy_combinator(void) {
    input_t* input = new_input();
    input->buffer = strdup("1a");
    input->length = 2;

    combinator_t* p = satisfy(is_digit_predicate, TEST_T_NONE);
    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, "1") == 0);

    free_ast(res.value.ast);
    free_combinator(p);

    // Test failure
    p = satisfy(is_digit_predicate, TEST_T_NONE);
    res = parse(input, p);
    TEST_ASSERT(!res.is_success);
    free_error(res.value.error);
    free_combinator(p);

    free(input->buffer);
    free(input);
}

void test_partial_ast_functionality(void) {
    input_t* input = new_input();
    input->buffer = strdup("invalid input");
    input->length = strlen("invalid input");

    // Create a parser that will definitely fail
    combinator_t* p = match("expected_keyword");
    
    // Create a partial AST representing what would be successfully parsed
    ast_t* partial_ast = new_ast();
    partial_ast->typ = TEST_T_INT;
    partial_ast->sym = sym_lookup("42");
    partial_ast->child = NULL;
    partial_ast->next = NULL;

    // Parse to get a real failure
    ParseResult original_result = parse(input, p);
    TEST_ASSERT(!original_result.is_success);

    // Wrap the failure with partial AST
    ParseResult wrapped_result = wrap_failure_with_ast(input, "Parsing failed with partial result", original_result, partial_ast);
    TEST_ASSERT(!wrapped_result.is_success);
    
    // Verify the wrapped error contains the partial AST
    TEST_ASSERT(wrapped_result.value.error != NULL);
    TEST_ASSERT(wrapped_result.value.error->partial_ast != NULL);
    TEST_ASSERT(wrapped_result.value.error->partial_ast->typ == TEST_T_INT);
    TEST_ASSERT(strcmp(wrapped_result.value.error->partial_ast->sym->name, "42") == 0);
    
    // Verify the original error is preserved as cause
    TEST_ASSERT(wrapped_result.value.error->cause != NULL);
    TEST_ASSERT(strstr(wrapped_result.value.error->cause->message, "Expected 'expected_keyword'") != NULL);

    free_error(wrapped_result.value.error);
    free_combinator(p);
    free(input->buffer);
    free(input);
}

void test_expression_parser_partial_ast(void) {
    input_t* input = new_input();
    input->buffer = strdup("1 + 2");
    input->length = strlen("1 + 2");

    // Create a proper expression parser
    combinator_t* expr_parser = new_combinator();
    combinator_t* factor = multi(new_combinator(), TEST_T_NONE,
        integer(TEST_T_INT),
        cident(TEST_T_IDENT),
        NULL
    );
    expr(expr_parser, factor);
    expr_insert(expr_parser, 0, TEST_T_ADD, EXPR_INFIX, ASSOC_LEFT, match("+"));
    expr_altern(expr_parser, 0, TEST_T_SUB, match("-"));

    // Parse a valid expression first
    ParseResult valid_result = parse(input, expr_parser);
    TEST_ASSERT(valid_result.is_success);
    TEST_ASSERT(valid_result.value.ast != NULL);
    
    // Save the AST for partial result testing
    ast_t* valid_ast = valid_result.value.ast;
    
    // Reset input for error case
    free(input->buffer);
    input->buffer = strdup("invalid");
    input->length = strlen("invalid");
    input->start = 0;

    // Create a parser that will definitely fail
    combinator_t* failing_parser = match("expected_keyword");
    ParseResult failure_result = parse(input, failing_parser);
    TEST_ASSERT(!failure_result.is_success);

    // Wrap the failure with the valid AST as partial result
    ParseResult wrapped_result = wrap_failure_with_ast(input, "Expression parsing failed", failure_result, valid_ast);
    TEST_ASSERT(!wrapped_result.is_success);
    
    // Verify the wrapped error contains the partial AST
    TEST_ASSERT(wrapped_result.value.error != NULL);
    TEST_ASSERT(wrapped_result.value.error->partial_ast != NULL);
    
    // The expression parser stops at the first successful parse (the integer 1)
    // So the partial AST should be just the integer, not the full addition
    ast_t* partial_ast = wrapped_result.value.error->partial_ast;
    TEST_ASSERT(partial_ast->typ == TEST_T_INT);
    TEST_ASSERT(strcmp(partial_ast->sym->name, "1") == 0);

    free_error(wrapped_result.value.error);
    free_combinator(expr_parser);
    free_combinator(failing_parser);
    free(input->buffer);
    free(input);
}

void test_expression_parser_invalid_input(void) {
    input_t* input = new_input();
    input->buffer = strdup("1 + * 2");  // Invalid: operator after operator
    input->length = strlen("1 + * 2");

    // Create expression parser
    combinator_t* expr_parser = new_combinator();
    combinator_t* factor = multi(new_combinator(), TEST_T_NONE,
        integer(TEST_T_INT),
        cident(TEST_T_IDENT),
        NULL
    );
    expr(expr_parser, factor);
    expr_insert(expr_parser, 0, TEST_T_ADD, EXPR_INFIX, ASSOC_LEFT, match("+"));
    expr_altern(expr_parser, 0, TEST_T_SUB, match("-"));
    expr_insert(expr_parser, 1, TEST_T_MUL, EXPR_INFIX, ASSOC_LEFT, match("*"));
    expr_altern(expr_parser, 1, TEST_T_DIV, match("/"));

    // Parse invalid input - should succeed partially
    ParseResult result = parse(input, expr_parser);
    
    // Expression parser is permissive - it parses what it can and stops
    TEST_ASSERT(result.is_success);
    TEST_ASSERT(result.value.ast != NULL);
    TEST_ASSERT(result.value.ast->typ == TEST_T_INT);
    TEST_ASSERT(strcmp(result.value.ast->sym->name, "1") == 0);
    
    // Only part of input should be consumed
    TEST_ASSERT(input->start < input->length);

    free_ast(result.value.ast);
    free_combinator(expr_parser);
    free(input->buffer);
    free(input);
}

void test_expression_parser_behavior(void) {
    input_t* input = new_input();
    input->buffer = strdup("1 + * 2");  // Invalid: operator after operator
    input->length = strlen("1 + * 2");

    // Create expression parser
    combinator_t* expr_parser = new_combinator();
    combinator_t* factor = multi(new_combinator(), TEST_T_NONE,
        integer(TEST_T_INT),
        cident(TEST_T_IDENT),
        NULL
    );
    expr(expr_parser, factor);
    expr_insert(expr_parser, 0, TEST_T_ADD, EXPR_INFIX, ASSOC_LEFT, match("+"));
    expr_altern(expr_parser, 0, TEST_T_SUB, match("-"));
    expr_insert(expr_parser, 1, TEST_T_MUL, EXPR_INFIX, ASSOC_LEFT, match("*"));
    expr_altern(expr_parser, 1, TEST_T_DIV, match("/"));

    // Parse invalid input - should succeed partially
    ParseResult result = parse(input, expr_parser);
    
    // Expression parser is permissive - it parses what it can and stops
    TEST_ASSERT(result.is_success);
    TEST_ASSERT(result.value.ast != NULL);
    TEST_ASSERT(result.value.ast->typ == TEST_T_INT);
    TEST_ASSERT(strcmp(result.value.ast->sym->name, "1") == 0);
    
    // Only part of input should be consumed
    TEST_ASSERT(input->start < input->length);

    free_ast(result.value.ast);
    free_combinator(expr_parser);
    free(input->buffer);
    free(input);
}

TEST_LIST = {
    { "pnot_combinator", test_pnot_combinator },
    { "peek_combinator", test_peek_combinator },
    { "gseq_combinator", test_gseq_combinator },
    { "between_combinator", test_between_combinator },
    { "sep_by_combinator", test_sep_by_combinator },
    { "sep_end_by_combinator", test_sep_end_by_combinator },
    { "chainl1_combinator", test_chainl1_combinator },
    { "any_char_combinator", test_any_char_combinator },
    { "map_combinator", test_map_combinator },
    { "errmap_combinator", test_errmap_combinator },
    { "satisfy_combinator", test_satisfy_combinator },
    { "partial_ast_functionality", test_partial_ast_functionality },
    { "expression_parser_partial_ast", test_expression_parser_partial_ast },
    { "expression_parser_invalid_input", test_expression_parser_invalid_input },
    { "expression_parser_behavior", test_expression_parser_behavior },
    { NULL, NULL }
};
