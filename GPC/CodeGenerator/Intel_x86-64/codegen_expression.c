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
#include "Grammar.tab.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"


/* Code generation for expressions */
ListNode_t *codegen_expr(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(expr != NULL);
    assert(ctx != NULL);
    expr_node_t *expr_tree = NULL;
    Register_t *target_reg;

    fprintf(stderr, "DEBUG: Generating code for expression type %d\n", expr->type);

    switch(expr->type) {
        case EXPR_VAR_ID:
            fprintf(stderr, "DEBUG: Processing variable ID expression\n");
            expr_tree = build_expr_tree(expr);
            target_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
            free_reg(get_reg_stack(), target_reg);
            free_expr_tree(expr_tree);
            #ifdef DEBUG_CODEGEN
            fprintf(stderr, "DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_MULOP:
            fprintf(stderr, "DEBUG: Processing mulop expression\n");
            expr_tree = build_expr_tree(expr);
            target_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
            free_reg(get_reg_stack(), target_reg);
            free_expr_tree(expr_tree);
            #ifdef DEBUG_CODEGEN
            fprintf(stderr, "DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_INUM:
            fprintf(stderr, "DEBUG: Processing integer constant expression\n");
            expr_tree = build_expr_tree(expr);
            target_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
            free_reg(get_reg_stack(), target_reg);
            free_expr_tree(expr_tree);
            #ifdef DEBUG_CODEGEN
            fprintf(stderr, "DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_RELOP:
            fprintf(stderr, "DEBUG: Processing relational operator expression\n");
            #ifdef DEBUG_CODEGEN
            fprintf(stderr, "DEBUG: LEAVING %s\n", __func__);
            #endif
            return codegen_simple_relop(expr, inst_list, ctx, NULL);
        case EXPR_ADDOP:
            fprintf(stderr, "DEBUG: Processing addop expression\n");
            expr_tree = build_expr_tree(expr);
            target_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
            free_reg(get_reg_stack(), target_reg);
            free_expr_tree(expr_tree);
            #ifdef DEBUG_CODEGEN
            fprintf(stderr, "DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_SIGN_TERM:
            fprintf(stderr, "DEBUG: Processing sign term expression\n");
            expr_tree = build_expr_tree(expr);
            target_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
            free_reg(get_reg_stack(), target_reg);
            free_expr_tree(expr_tree);
            #ifdef DEBUG_CODEGEN
            fprintf(stderr, "DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        default:
            assert(0 && "Unsupported expression type");
            break;
    }
}


/* Code generation for simple relops */
ListNode_t *codegen_simple_relop(struct Expression *expr, ListNode_t *inst_list,
                                CodeGenContext *ctx, int *relop_type)
{
    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(expr != NULL);
    assert(expr->type == EXPR_RELOP);
    assert(inst_list != NULL);
    assert(ctx != NULL);

    fprintf(stderr, "DEBUG: Generating simple relop\n");

    *relop_type = expr->expr_data.relop_data.type;
    inst_list = codegen_expr(expr->expr_data.relop_data.left, inst_list, ctx);

    Register_t *left_reg = get_free_reg(get_reg_stack(), &inst_list);
    inst_list = codegen_expr(expr->expr_data.relop_data.right, inst_list, ctx);
    Register_t *right_reg = front_reg_stack(get_reg_stack());

    char buffer[100];
    snprintf(buffer, 100, "\tcmpl\t%s, %s\n", right_reg->bit_32, left_reg->bit_32);
    free_reg(get_reg_stack(), left_reg);
    inst_list = add_inst(inst_list, buffer);

    fprintf(stderr, "DEBUG: Simple relop generated\n");
    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for non-local variable access */
ListNode_t *codegen_get_nonlocal(ListNode_t *inst_list, char *var_id, int *offset)
{
    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "DEBUG: ENTERING %s\n", __func__);
    #endif
    fprintf(stderr, "DEBUG: Generating non-local access for %s\n", var_id);

    assert(inst_list != NULL);
    assert(var_id != NULL);
    assert(offset != NULL);

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
    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for passing arguments */
ListNode_t *codegen_pass_arguments(ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx, HashNode_t *proc_node)
{
    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "DEBUG: ENTERING %s\n", __func__);
    #endif
    int arg_num;
    StackNode_t *stack_node;
    Register_t *top_reg;
    char buffer[50];
    char *arg_reg_char;
    expr_node_t *expr_tree;

    assert(ctx != NULL);

    ListNode_t *formal_args = NULL;
    if(proc_node != NULL)
        formal_args = proc_node->args;

    arg_num = 0;
    while(args != NULL)
    {
        fprintf(stderr, "DEBUG: In codegen_pass_arguments loop, arg_num = %d\n", arg_num);
        struct Expression *arg_expr = (struct Expression *)args->cur;
        fprintf(stderr, "DEBUG: arg_expr at %p, type %d\n", arg_expr, arg_expr->type);

        arg_reg_char = get_arg_reg64_num(arg_num);
        if(arg_reg_char == NULL)
        {
            fprintf(stderr, "ERROR: Could not get arg register: %d\n", arg_num);
            exit(1);
        }

        Tree_t *formal_arg_decl = NULL;
        if(formal_args != NULL)
            formal_arg_decl = (Tree_t *)formal_args->cur;

        if(formal_arg_decl != NULL && formal_arg_decl->tree_data.var_decl_data.is_var_param)
        {
            // Pass by reference
            assert(arg_expr->type == EXPR_VAR_ID);
            StackNode_t *var_node = find_label(arg_expr->expr_data.id);
            snprintf(buffer, 50, "\tleaq\t-%d(%%rbp), %s\n", var_node->offset, arg_reg_char);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            // Pass by value
            expr_tree = build_expr_tree(arg_expr);
            top_reg = get_free_reg(get_reg_stack(), &inst_list);
            fprintf(stderr, "DEBUG: top_reg at %p\n", top_reg);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, top_reg);
            free_expr_tree(expr_tree);

            snprintf(buffer, 50, "\tmovq\t%s, %s\n", top_reg->bit_64, arg_reg_char);
            free_reg(get_reg_stack(), top_reg);
            inst_list = add_inst(inst_list, buffer);
        }

        args = args->next;
        if(formal_args != NULL)
            formal_args = formal_args->next;
        ++arg_num;
    }

    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Helper for codegen_get_nonlocal */
ListNode_t * codegen_goto_prev_scope(ListNode_t *inst_list, StackScope_t *cur_scope, char *base)
{
    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "DEBUG: ENTERING %s\n", __func__);
    #endif
    char buffer[50];

    assert(inst_list != NULL);
    assert(cur_scope != NULL);
    assert(base != NULL);

    snprintf(buffer, 50, "\tmovq\t(%s), %s\n", base, NON_LOCAL_REG_64);
    inst_list = add_inst(inst_list, buffer);

    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}
