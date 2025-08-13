#include "expr_gen.h"
#include <stdio.h>
#include "codegen.h"
#include "../../Parser/ParseTree/tree.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include <assert.h>
#include "register_types.h"

#include "expr_gen.h"
#include <stdio.h>
#include "codegen.h"
#include "../../Parser/ParseTree/tree.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include <assert.h>
#include "register_types.h"
#include "../../Parser/LexAndYacc/Grammar.tab.h"

/* Code generation for expressions */
ListNode_t *codegen_expr(struct Expression *expr, ListNode_t *inst_list, FILE *o_file)
{
    assert(expr != NULL);
    expr_node_t *expr_tree = NULL;

    fprintf(stderr, "DEBUG: Generating code for expression type %d\n", expr->type);

    switch(expr->type) {
        case EXPR_VAR_ID:
            fprintf(stderr, "DEBUG: Processing variable ID expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list);
            free_expr_tree(expr_tree);
            return inst_list;
        case EXPR_INUM:
            fprintf(stderr, "DEBUG: Processing integer constant expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list);
            free_expr_tree(expr_tree);
            return inst_list;
        case EXPR_RELOP:
            fprintf(stderr, "DEBUG: Processing relational operator expression\n");
            return codegen_simple_relop(expr, inst_list, o_file, NULL);
        case EXPR_ADDOP:
            fprintf(stderr, "DEBUG: Processing addop expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list);
            free_expr_tree(expr_tree);
            return inst_list;
        case EXPR_SIGN_TERM:
            fprintf(stderr, "DEBUG: Processing sign term expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list);
            free_expr_tree(expr_tree);
            return inst_list;
        default:
            fprintf(stderr, "ERROR: Unsupported expression type %d\n", expr->type);
            exit(1);
    }
}

/* Code generation for simple relops */
ListNode_t *codegen_simple_relop(struct Expression *expr, ListNode_t *inst_list,
                                FILE *o_file, int *relop_type)
{
    assert(expr != NULL);
    assert(expr->type == EXPR_RELOP);

    fprintf(stderr, "DEBUG: Generating simple relop\n");

    *relop_type = expr->expr_data.relop_data.type;
    inst_list = codegen_expr(expr->expr_data.relop_data.left, inst_list, o_file);

    Register_t *left_reg = pop_reg_stack(get_reg_stack());
    inst_list = codegen_expr(expr->expr_data.relop_data.right, inst_list, o_file);
    Register_t *right_reg = front_reg_stack(get_reg_stack());

    char buffer[100];
    snprintf(buffer, 100, "\tcmpl\t%s, %s\n", right_reg->bit_32, left_reg->bit_32);
    push_reg_stack(get_reg_stack(), left_reg);
    inst_list = add_inst(inst_list, buffer);

    fprintf(stderr, "DEBUG: Simple relop generated\n");
    return inst_list;
}
