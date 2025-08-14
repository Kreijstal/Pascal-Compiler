/*
    Damon Gwinn
    Code generation for expressions
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "codegen_expression.h"
#include "register_types.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/LexAndYacc/Grammar.tab.h"


/* Code generation for expressions */
ListNode_t *codegen_expr(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    assert(expr != NULL);
    expr_node_t *expr_tree = NULL;

    fprintf(stderr, "DEBUG: Generating code for expression type %d\n", expr->type);

    switch(expr->type) {
        case EXPR_VAR_ID:
            fprintf(stderr, "DEBUG: Processing variable ID expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list, ctx);
            free_expr_tree(expr_tree);
            return inst_list;
        case EXPR_INUM:
            fprintf(stderr, "DEBUG: Processing integer constant expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list, ctx);
            free_expr_tree(expr_tree);
            return inst_list;
        case EXPR_RELOP:
            fprintf(stderr, "DEBUG: Processing relational operator expression\n");
            return codegen_simple_relop(expr, inst_list, ctx, NULL);
        case EXPR_ADDOP:
            fprintf(stderr, "DEBUG: Processing addop expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list, ctx);
            free_expr_tree(expr_tree);
            return inst_list;
        case EXPR_SIGN_TERM:
            fprintf(stderr, "DEBUG: Processing sign term expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list, ctx);
            free_expr_tree(expr_tree);
            return inst_list;
        default:
            fprintf(stderr, "ERROR: Unsupported expression type %d\n", expr->type);
            exit(1);
    }
}


/* Code generation for simple relops */
ListNode_t *codegen_simple_relop(struct Expression *expr, ListNode_t *inst_list,
                                CodeGenContext *ctx, int *relop_type)
{
    assert(expr != NULL);
    assert(expr->type == EXPR_RELOP);

    fprintf(stderr, "DEBUG: Generating simple relop\n");

    *relop_type = expr->expr_data.relop_data.type;
    inst_list = codegen_expr(expr->expr_data.relop_data.left, inst_list, ctx);

    Register_t *left_reg = pop_reg_stack(get_reg_stack());
    inst_list = codegen_expr(expr->expr_data.relop_data.right, inst_list, ctx);
    Register_t *right_reg = front_reg_stack(get_reg_stack());

    char buffer[100];
    snprintf(buffer, 100, "\tcmpl\t%s, %s\n", right_reg->bit_32, left_reg->bit_32);
    push_reg_stack(get_reg_stack(), left_reg);
    inst_list = add_inst(inst_list, buffer);

    fprintf(stderr, "DEBUG: Simple relop generated\n");
    return inst_list;
}

/* Code generation for non-local variable access */
ListNode_t *codegen_get_nonlocal(ListNode_t *inst_list, char *var_id, int *offset)
{
    fprintf(stderr, "DEBUG: Generating non-local access for %s\n", var_id);

    char buffer[100];
    StackNode_t *var = find_label(var_id);

    if(var == NULL) {
        fprintf(stderr, "ERROR: Could not find non-local variable %s\n", var_id);
        exit(1);
    }

    *offset = var->offset;
    snprintf(buffer, 100, "\tmovq\t-8(%%rbp), %s\n", NON_LOCAL_REG_64);
    inst_list = add_inst(inst_list, buffer);

    fprintf(stderr, "DEBUG: Non-local access generated\n");
    return inst_list;
}

/* Code generation for passing arguments */
ListNode_t *codegen_pass_arguments(ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx)
{
    int arg_num;
    StackNode_t *stack_node;
    Register_t *top_reg;
    char buffer[50];
    char *arg_reg_char;
    expr_node_t *expr_tree;

    arg_num = 0;
    while(args != NULL)
    {
        arg_reg_char = get_arg_reg64_num(arg_num);
        if(arg_reg_char == NULL)
        {
            fprintf(stderr, "ERROR: Could not get arg register: %d\n", arg_num);
            exit(1);
        }

        expr_tree = build_expr_tree((struct Expression *)args->cur);
        inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list, ctx);
        free_expr_tree(expr_tree);

        top_reg = front_reg_stack(get_reg_stack());
        snprintf(buffer, 50, "\tmovq\t%s, %s\n", top_reg->bit_64, arg_reg_char);
        inst_list = add_inst(inst_list, buffer);

        args = args->next;
        ++arg_num;
    }

    return inst_list;
}

/* Helper for codegen_get_nonlocal */
ListNode_t * codegen_goto_prev_scope(ListNode_t *inst_list, StackScope_t *cur_scope, char *base)
{
    char buffer[50];

    snprintf(buffer, 50, "\tmovq\t(%s), %s\n", base, NON_LOCAL_REG_64);
    inst_list = add_inst(inst_list, buffer);

    return inst_list;
}
