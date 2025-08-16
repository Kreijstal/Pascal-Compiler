#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "expr_tree.h"
#include "../stackmng/stackmng.h"
#include "../register.h"
#include "../../../Parser/ParseTree/tree.h"

/* Local functions */
void print_expr_tree(expr_node_t *node, int cur_depth, int max_depth);
expr_node_t *build_expr_tree_from_term(term_t *term);
expr_node_t *build_expr_tree_from_factor(factor_t *factor);

expr_node_t *build_expr_tree(struct Expression *expr)
{
    expr_node_t *cur_node;
    cur_node = (expr_node_t *)malloc(sizeof(expr_node_t));
    cur_node->left = NULL;
    cur_node->right = NULL;
    cur_node->expr = expr;
    cur_node->reg = NULL;
    cur_node->label = NULL;
    cur_node->depth = -1;

    switch(expr->type)
    {
        case EXPR_ADDOP:
            cur_node->left = build_expr_tree(expr->expr_data.addop_data.left);
            cur_node->right = build_expr_tree(expr->expr_data.addop_data.right);
            break;

        case EXPR_MULOP:
            cur_node->left = build_expr_tree(expr->expr_data.mulop_data.left);
            cur_node->right = build_expr_tree(expr->expr_data.mulop_data.right);
            break;

        case EXPR_SIGN_TERM:
            cur_node->left = build_expr_tree(expr->expr_data.sign_term_data.expr);
            cur_node->right = NULL;
            break;

        default:
            cur_node->left = NULL;
            cur_node->right = NULL;
            break;
    }
    return cur_node;
}

void free_expr_tree(expr_node_t *node)
{
    if(node == NULL)
        return;

    free_expr_tree(node->left);
    free_expr_tree(node->right);
    free(node);
}

int expr_tree_is_leaf(expr_node_t *node)
{
    assert(node != NULL);
    if(node->left == NULL && node->right == NULL)
        return 1;
    else
        return 0;
}


void print_tree(expr_node_t *node, int cur_depth)
{
    int i;
    for(i=0; i<cur_depth; ++i)
    {
        printf("  ");
    }

    printf("Type: %d\n", node->expr->type);

    if(node->left != NULL)
        print_tree(node->left, cur_depth+1);
    if(node->right != NULL)
        print_tree(node->right, cur_depth+1);
}

ListNode_t *gencode_expr_tree(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *reg)
{
    assert(node != NULL);
    assert(node->expr != NULL);

    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "gencode_expr_tree: node->expr->type = %d\n", node->expr->type);
    #endif

    if(node->reg != NULL)
    {
        char buffer[50];
        snprintf(buffer, 50, "\tmovl\t%s, %s\n", node->reg->bit_32, reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        return inst_list;
    }

    if(expr_tree_is_leaf(node) == 1)
    {
        inst_list = gencode_leaf_var(node, inst_list, ctx, reg);
        node->reg = reg;
    }
    else if(node->left != NULL && node->right == NULL)
    {
        inst_list = gencode_expr_tree(node->left, inst_list, ctx, reg);
        inst_list = gencode_op(node, inst_list, reg, NULL);
    }
    else
    {
        int left_leaf = expr_tree_is_leaf(node->left);
        int right_leaf = expr_tree_is_leaf(node->right);
        Register_t *left_reg, *right_reg;

        if(left_leaf == 1 && right_leaf == 1)
        {
            left_reg = reg;
            right_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(node->left, inst_list, ctx, left_reg);
            inst_list = gencode_expr_tree(node->right, inst_list, ctx, right_reg);
            inst_list = gencode_op(node, inst_list, left_reg, right_reg);
            free_reg(get_reg_stack(), right_reg);
        }
        else if(left_leaf == 0 && right_leaf == 1)
        {
            left_reg = reg;
            inst_list = gencode_expr_tree(node->left, inst_list, ctx, left_reg);
            right_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(node->right, inst_list, ctx, right_reg);
            inst_list = gencode_op(node, inst_list, left_reg, right_reg);
            free_reg(get_reg_stack(), right_reg);
        }
        else if(left_leaf == 1 && right_leaf == 0)
        {
            right_reg = reg;
            inst_list = gencode_expr_tree(node->right, inst_list, ctx, right_reg);
            left_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(node->left, inst_list, ctx, left_reg);
            inst_list = gencode_op(node, inst_list, left_reg, right_reg);
            free_reg(get_reg_stack(), left_reg);
        }
        else
        {
            inst_list = gencode_expr_tree(node->right, inst_list, ctx, reg);
            Register_t *temp_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(node->left, inst_list, ctx, temp_reg);
            inst_list = gencode_op(node, inst_list, temp_reg, reg);
            free_reg(get_reg_stack(), temp_reg);
        }
    }
    return inst_list;
}
