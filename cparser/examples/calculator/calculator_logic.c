#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "parser.h"
#include "combinators.h"
#include "calculator_logic.h"

// --- Helper Functions ---
static bool is_whitespace_char(char c) {
    return isspace((unsigned char)c);
}

static combinator_t* token(combinator_t* p) {
    combinator_t* ws = many(satisfy(is_whitespace_char, CALC_T_NONE));
    return right(ws, left(p, many(satisfy(is_whitespace_char, CALC_T_NONE))));
}

// --- Evaluation ---
// Return false on evaluation errors instead of aborting.
bool eval(ast_t *ast, long *out) {
    if (out == NULL) {
        fprintf(stderr, "Error: NULL output in %s at %s:%d\n", __func__, __FILE__, __LINE__);
        return false;
    }
    if (!ast) {
        fprintf(stderr, "Error: NULL AST node in %s at %s:%d\n", __func__, __FILE__, __LINE__);
        return false;
    }
    switch (ast->typ) {
        case CALC_T_INT:
            *out = atol(ast->sym->name);
            return true;
        case CALC_T_ADD: {
            long left = 0, right = 0;
            if (!eval(ast->child, &left) || !eval(ast->child->next, &right))
                return false;
            *out = left + right;
            return true;
        }
        case CALC_T_SUB: {
            long left = 0, right = 0;
            if (!eval(ast->child, &left) || !eval(ast->child->next, &right))
                return false;
            *out = left - right;
            return true;
        }
        case CALC_T_MUL: {
            long left = 0, right = 0;
            if (!eval(ast->child, &left) || !eval(ast->child->next, &right))
                return false;
            *out = left * right;
            return true;
        }
        case CALC_T_DIV: {
            long left = 0, right = 0;
            if (!eval(ast->child, &left) || !eval(ast->child->next, &right))
                return false;
            if (right == 0) {
                fprintf(stderr, "Error: Division by zero\n");
                return false;
            }
            *out = left / right;
            return true;
        }
        case CALC_T_NEG: {
            long value = 0;
            if (!eval(ast->child, &value))
                return false;
            *out = -value;
            return true;
        }
        default:
            fprintf(stderr, "Error: Unknown AST node type: %d in %s at %s:%d\n",
                ast->typ, __func__, __FILE__, __LINE__);
            return false;
    }
}

// --- AST Printing ---
const char* calc_tag_to_string(tag_t tag) {
    switch (tag) {
        case CALC_T_NONE: return "NONE";
        case CALC_T_INT: return "INT";
        case CALC_T_ADD: return "ADD";
        case CALC_T_SUB: return "SUB";
        case CALC_T_MUL: return "MUL";
        case CALC_T_DIV: return "DIV";
        case CALC_T_NEG: return "NEG";
        default: return "UNKNOWN";
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

void print_calculator_ast(ast_t* ast) {
    print_ast_indented(ast, 0);
    printf("\n");
}

// --- Parser Definition ---
void init_calculator_parser(combinator_t** p) {
    combinator_t *factor = expect(multi(new_combinator(), CALC_T_NONE,
        expect(token(integer(CALC_T_INT)), "Expected an integer"),
        between(
            expect(token(match("(")), "Expected '('"),
            expect(token(match(")")), "Expected ')'"),
            lazy(p)
        ),
        NULL
    ), "Expected a number or a parenthesized expression");
    expr(*p, factor);
    expr_insert(*p, 0, CALC_T_ADD, EXPR_INFIX, ASSOC_LEFT, expect(token(match("+")), "Expected '+' operator"));
    expr_altern(*p, 0, CALC_T_SUB, expect(token(match("-")), "Expected '-' operator"));
    expr_insert(*p, 1, CALC_T_MUL, EXPR_INFIX, ASSOC_LEFT, expect(token(match("*")), "Expected '*' operator"));
    expr_altern(*p, 1, CALC_T_DIV, expect(token(match("/")), "Expected '/' operator"));
    expr_insert(*p, 2, CALC_T_NEG, EXPR_PREFIX, ASSOC_NONE, expect(token(match("-")), "Expected '-' for negation"));
}
