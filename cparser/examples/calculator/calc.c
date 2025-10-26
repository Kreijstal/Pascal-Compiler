#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "parser.h"
#include "combinators.h"
#include "calculator_logic.h"

// --- Forward declarations for local functions ---
static void print_error_with_partial_ast(ParseError* error);
static void print_ast_indented(ast_t* ast, int depth);

// --- Error Printing with Partial AST ---
static void print_error_with_partial_ast(ParseError* error) {
    if (error == NULL) return;

    printf("Error at line %d, col %d: ", error->line, error->col);
    if (error->parser_name) {
        printf("In parser '%s': ", error->parser_name);
    }
    printf("%s\n", error->message);

    if (error->unexpected) {
        printf("Unexpected input: \"%s\"\n", error->unexpected);
    }

    if (error->partial_ast != NULL) {
        printf("Partial AST:\n");
        print_ast_indented(error->partial_ast, 1);
    }
}

static void print_ast_indented(ast_t* ast, int depth) {
    if (ast == NULL || ast == ast_nil) return;
    for (int i = 0; i < depth; i++) printf("  ");
    printf("(%s", calc_tag_to_string(ast->typ));
    if (ast->sym) printf(" %s", ast->sym->name);

    ast_t* child = ast->child;
    if (child) {
        printf("\n");
        print_ast_indented(child, depth + 1);
    }
    printf(")");

    if (ast->next) {
        printf("\n");
        print_ast_indented(ast->next, depth);
    }
}

// --- Visitor for counting nodes ---
void count_nodes_visitor(ast_t* node, void* context) {
    int* counter = (int*)context;
    (*counter)++;
}

// --- Main ---
int main(int argc, char *argv[]) {
    bool print_ast = false;
    bool count_nodes = false;
    char *expr_str = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--print-ast") == 0) {
            print_ast = true;
        } else if (strcmp(argv[i], "--count-nodes") == 0) {
            count_nodes = true;
        } else {
            expr_str = argv[i];
        }
    }

    if (expr_str == NULL) {
        fprintf(stderr, "Usage: %s [--print-ast] [--count-nodes] \"<expression>\"\n", argv[0]);
        return 1;
    }

    combinator_t *expr_parser = new_combinator();
    init_calculator_parser(&expr_parser);

    // Parsing
    input_t *in = new_input();
    in->buffer = expr_str;
    in->length = strlen(expr_str);
    ast_nil = new_ast();
    ast_nil->typ = CALC_T_NONE;
    ParseResult result = parse(in, expr_parser);

    // Output
    if (result.is_success) {
        if (in->start < in->length) {
            fprintf(stderr, "Error: Parser did not consume entire input. Trailing characters: '%s'\n", in->buffer + in->start);
            free_ast(result.value.ast);
            return 1;
        }
        if (print_ast) {
            print_calculator_ast(result.value.ast);
        }
        if (count_nodes) {
            int node_count = 0;
            parser_walk_ast(result.value.ast, count_nodes_visitor, &node_count);
            printf("AST contains %d nodes.\n", node_count);
        }
        long final_result = eval(result.value.ast);
        printf("%ld\n", final_result);
        free_ast(result.value.ast);
    } else {
        print_error_with_partial_ast(result.value.error);
        free_error(result.value.error);
        return 1;
    }

    // Cleanup
    free_combinator(expr_parser);
    free(in);
    free(ast_nil);
    return 0;
}