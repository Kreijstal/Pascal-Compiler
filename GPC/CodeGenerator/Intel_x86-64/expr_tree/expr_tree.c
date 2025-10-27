/*
    Damon Gwinn
    Tree of simple expressions for the gencode algorithm
    TODO: Does not handle function calls or arrays
    TODO: Does not handle real numbers
    TODO: Does not handle panic case (not enough registers)
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "../codegen.h"
#include "expr_tree.h"
#include "../register_types.h"
#include "../codegen_expression.h"
#include "../stackmng/stackmng.h"
#include "../../../flags.h"
#include "../../../Parser/List/List.h"
#include "../../../Parser/ParseTree/tree.h"
#include "../../../Parser/ParseTree/tree_types.h"
#include "../../../Parser/ParseTree/type_tags.h"

/* Helper functions */
ListNode_t *gencode_sign_term(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case0(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case1(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case2(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case3(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_leaf_var(struct Expression *, ListNode_t *, CodeGenContext *, char *, int );
ListNode_t *gencode_op(struct Expression *expr, char *left, char *right,
    ListNode_t *inst_list);
ListNode_t *gencode_op_deprecated(struct Expression *expr, ListNode_t *inst_list,
    char *buffer, int buf_len);

ListNode_t *gencode_divide_const_no_optimize(char *left, char *right, ListNode_t *inst_list);
ListNode_t *gencode_divide_no_const(char *left, char *right, ListNode_t *inst_list);

/* Builds an expression tree out of an expression */
/* WARNING: Does not make deep copy of expression */
/* WARNING: Does not do relational expressions */
expr_node_t *build_expr_tree(struct Expression *expr)
{
    assert(expr != NULL);

    expr_node_t *new_node;

    new_node = (expr_node_t *)malloc(sizeof(expr_node_t));
    assert(new_node != NULL);
    new_node->expr = expr;
    new_node->reg = NULL;

    /* Building the tree */
    switch(expr->type)
    {
        case EXPR_ADDOP:
            new_node->left_expr = build_expr_tree(expr->expr_data.addop_data.left_expr);
            new_node->right_expr = build_expr_tree(expr->expr_data.addop_data.right_term);
            break;
        case EXPR_MULOP:
            new_node->left_expr = build_expr_tree(expr->expr_data.mulop_data.left_term);
            new_node->right_expr = build_expr_tree(expr->expr_data.mulop_data.right_factor);
            break;

        case EXPR_SIGN_TERM:
            new_node->left_expr = build_expr_tree(expr->expr_data.sign_term);
            new_node->right_expr = NULL;
            break;

        case EXPR_VAR_ID:
        case EXPR_ARRAY_ACCESS:
        case EXPR_INUM:
        case EXPR_FUNCTION_CALL:
        case EXPR_STRING:
            new_node->left_expr = NULL;
            new_node->right_expr = NULL;
            break;

        case EXPR_RELOP:
            new_node->left_expr = build_expr_tree(expr->expr_data.relop_data.left);
            assert(expr->expr_data.relop_data.right != NULL);
            new_node->right_expr = build_expr_tree(expr->expr_data.relop_data.right);
            break;

        default:
            assert(0 && "Unsupported expr_tree type");
            break;
    }

    /* Setting the labels */
    if(new_node->left_expr != NULL)
    {
        if (expr_tree_is_leaf(new_node->left_expr) == 1)
        {
            new_node->left_expr->label = 1;
        }
    }

    if(new_node->left_expr == NULL && new_node->right_expr == NULL)
    {
        new_node->label = 0;
    }
    else if(new_node->left_expr == NULL)
    {
        new_node->label = new_node->right_expr->label;
    }
    else if(new_node->right_expr == NULL)
    {
        new_node->label = new_node->left_expr->label;
    }
    else if(new_node->left_expr->label > new_node->right_expr->label)
    {
        new_node->label = new_node->left_expr->label;
    }
    else if(new_node->left_expr->label < new_node->right_expr->label)
    {
        new_node->label = new_node->right_expr->label;
    }
    else /* (new_node->left_expr->label == new_node->right_expr->label) */
    {
        new_node->label = new_node->left_expr->label + 1;
    }

    return new_node;
}

/* The famous gencode algorithm */
ListNode_t *gencode_expr_tree(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg)
{
    assert(node != NULL);
    assert(node->expr != NULL);
    assert(ctx != NULL);
    assert(target_reg != NULL);

    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "gencode_expr_tree: node->expr->type = %d\n", node->expr->type);
    #endif

    if(node->reg != NULL)
    {
        char buffer[50];
        snprintf(buffer, 50, "\tmovl\t%s, %s\n", node->reg->bit_32, target_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        return inst_list;
    }

    /*if(node->label > get_num_registers_free(get_reg_stack()))
    {
        fprintf(stderr, "ERROR: codegen more complex than number of registers is unsupported!\n");
        exit(1);
    }*/

    /* Handle special cases first */
    if(node->expr->type == EXPR_SIGN_TERM)
    {
        inst_list = gencode_sign_term(node, inst_list, ctx, target_reg);
    }
    /* CASE 0 */
    else if(expr_tree_is_leaf(node) == 1)
    {
        inst_list = gencode_case0(node, inst_list, ctx, target_reg);
        node->reg = target_reg;
    }
    /* CASE 1 */
    else if(expr_tree_is_leaf(node->right_expr))
    {
        inst_list = gencode_case1(node, inst_list, ctx, target_reg);
    }
    /* CASE 2 */
    else if(node->left_expr->label < node->right_expr->label)
    {
        inst_list = gencode_case2(node, inst_list, ctx, target_reg);
    }
    /* CASE 3 */
    else if(node->left_expr->label >= node->right_expr->label)
    {
        inst_list = gencode_case3(node, inst_list, ctx, target_reg);
    }
    else
    {
        assert(0 && "Unsupported case in codegen!");
    }

    return inst_list;
}

/* Gencode for modulus */
// left is right operand (B), right is left operand (A)
// calculates A mod B, stores result in A's location (right)
ListNode_t *gencode_modulus(char *left, char *right, ListNode_t *inst_list)
{
    StackNode_t *temp;
    char buffer[50];

    assert(left != NULL);
    assert(right != NULL);
    assert(inst_list != NULL);

    // Move dividend (A, right) to eax
    snprintf(buffer, 50, "\tmovl\t%s, %%eax\n", right);
    inst_list = add_inst(inst_list, buffer);

    // Sign extend eax to edx
    snprintf(buffer, 50, "\tcltd\n");
    inst_list = add_inst(inst_list, buffer);

    // If divisor (B, left) is a constant, move it to memory
    if(left[0] == '$')
    {
        temp = find_in_temp("TEMP_MOD");
        if(temp == NULL)
            temp = add_l_t("TEMP_MOD");
        snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", left, temp->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, 50, "\tidivl\t-%d(%%rbp)\n", temp->offset);
        inst_list = add_inst(inst_list, buffer);
    }
    else // Divisor is a register
    {
        snprintf(buffer, 50, "\tidivl\t%s\n", left);
        inst_list = add_inst(inst_list, buffer);
    }

    // Move remainder from edx to the target register (A's location, right)
    snprintf(buffer, 50, "\tmovl\t%%edx, %s\n", right);
    inst_list = add_inst(inst_list, buffer);

    return inst_list;
}

/* Checks if node is a leaf */
int expr_tree_is_leaf(expr_node_t *node)
{
    assert(node != NULL);

    if(node->left_expr == NULL && node->right_expr == NULL)
        return 1;

    return 0;
}

/* Prints an expression tree */
void print_expr_tree(expr_node_t *node, int num_indent, FILE *f)
{
    assert(node != NULL);
    assert(node->expr != NULL);
    assert(f != NULL);
    int i;

    for(i=0; i < num_indent; ++i)
        fprintf(f, "  ");

    fprintf(f, "[NODE:%d, L:%d]\n", node->expr->type, node->label);
    if(node->left_expr != NULL)
    {
        for(i=0; i < num_indent; ++i)
            fprintf(f, "  ");

        fprintf(f, "[LEFT]\n");
        print_expr_tree(node->left_expr, num_indent+1, f);
    }
    if(node->right_expr != NULL)
    {
        for(i=0; i < num_indent; ++i)
            fprintf(f, "  ");

        fprintf(f, "[RIGHT]\n");
        print_expr_tree(node->right_expr, num_indent+1, f);
    }
}

/* Frees an allocated expression tree */
void free_expr_tree(expr_node_t *node)
{
    if(node != NULL)
    {
        free_expr_tree(node->left_expr);
        free_expr_tree(node->right_expr);
        free(node);
    }
}

/* Special case for a sign term */
ListNode_t *gencode_sign_term(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg)
{
    assert(node != NULL);
    assert(node->expr != NULL);
    assert(node->expr->type == EXPR_SIGN_TERM);
    assert(inst_list != NULL);
    assert(ctx != NULL);
    assert(target_reg != NULL);

    char buffer[50];

    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

    snprintf(buffer, 50, "\tnegl\t%s\n", target_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    return inst_list;
}

/* node is a leaf */
ListNode_t *gencode_case0(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg)
{
    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "gencode_case0\n");
    #endif
    assert(node != NULL);
    assert(node->expr != NULL);
    assert(ctx != NULL);
    assert(target_reg != NULL);

    char buffer[50];
    char buf_leaf[30];
    struct Expression *expr;

    expr = node->expr;
    assert(target_reg != NULL);

    if (expr->type == EXPR_FUNCTION_CALL)
    {
        inst_list = codegen_pass_arguments(expr->expr_data.function_call_data.args_expr, inst_list, ctx, expr->expr_data.function_call_data.resolved_func);
        snprintf(buffer, 50, "\tcall\t%s\n", expr->expr_data.function_call_data.mangled_id);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, 50, "\tmovl\t%%eax, %s\n", target_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        return inst_list;
    }
    else if (expr->type == EXPR_ARRAY_ACCESS)
    {
        return codegen_array_access(expr, inst_list, ctx, target_reg);
    }
    else if (expr->type == EXPR_STRING)
    {
        char label[20];
        snprintf(label, 20, ".LC%d", ctx->write_label_counter++);
        char add_rodata[1024];
        snprintf(add_rodata, 1024, "\t.section\t.rodata\n%s:\n\t.string \"%s\"\n\t.text\n",
            label, expr->expr_data.string);
        inst_list = add_inst(inst_list, add_rodata);
        snprintf(buffer, 50, "\tleaq\t%s(%%rip), %s\n", label, target_reg->bit_64);
        return add_inst(inst_list, buffer);
    }

    inst_list = gencode_leaf_var(expr, inst_list, ctx, buf_leaf, 30);

#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: Loading value %s into register %s\n", buf_leaf, target_reg->bit_32);
#endif

    snprintf(buffer, 50, "\tmovl\t%s, %s\n", buf_leaf, target_reg->bit_32);

    return add_inst(inst_list, buffer);
}

/* right node is a leaf */
ListNode_t *gencode_case1(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg)
{
    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "gencode_case1\n");
    #endif
    assert(node != NULL);
    assert(node->expr != NULL);
    assert(node->right_expr != NULL);
    assert(node->right_expr->expr != NULL);
    assert(ctx != NULL);
    assert(target_reg != NULL);

    char name_buf[30];
    struct Expression *expr, *right_expr;

    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

    expr = node->expr;
    right_expr = node->right_expr->expr;
    assert(right_expr != NULL);
    inst_list = gencode_leaf_var(right_expr, inst_list, ctx, name_buf, 30);

    inst_list = gencode_op(expr, target_reg->bit_32, name_buf, inst_list);

    return inst_list;
}


ListNode_t *gencode_case2(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg)
{
    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "gencode_case2\n");
    #endif
    assert(node != NULL);
    assert(node->expr != NULL);
    assert(inst_list != NULL);
    assert(ctx != NULL);
    assert(target_reg != NULL);

    Register_t *temp_reg;

    temp_reg = get_free_reg(get_reg_stack(), &inst_list);
    if(temp_reg == NULL)
    {
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);

        StackNode_t *spill_loc = add_l_t("spill");
        char buffer[50];
        snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", target_reg->bit_32, spill_loc->offset);
        inst_list = add_inst(inst_list, buffer);

        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

        char spill_mem[30];
        snprintf(spill_mem, 30, "-%d(%%rbp)", spill_loc->offset);
        inst_list = gencode_op(node->expr, target_reg->bit_32, spill_mem, inst_list);
    }
    else
    {
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, temp_reg);
        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
        inst_list = gencode_op(node->expr, target_reg->bit_32, temp_reg->bit_32, inst_list);
        free_reg(get_reg_stack(), temp_reg);
    }

    return inst_list;
}

ListNode_t *gencode_case3(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg)
{
    #ifdef DEBUG_CODEGEN
    fprintf(stderr, "gencode_case3\n");
    #endif
    assert(node != NULL);
    assert(node->expr != NULL);
    assert(inst_list != NULL);
    assert(ctx != NULL);
    assert(target_reg != NULL);

    Register_t *temp_reg;

    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
    temp_reg = get_free_reg(get_reg_stack(), &inst_list);

    if(temp_reg == NULL)
    {
        StackNode_t *spill_loc = add_l_t("spill");
        char buffer[50];
        snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", target_reg->bit_32, spill_loc->offset);
        inst_list = add_inst(inst_list, buffer);

        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);

        char spill_mem[30];
        snprintf(spill_mem, 30, "-%d(%%rbp)", spill_loc->offset);
        inst_list = gencode_op(node->expr, target_reg->bit_32, spill_mem, inst_list);
    }
    else
    {
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, temp_reg);
        inst_list = gencode_op(node->expr, target_reg->bit_32, temp_reg->bit_32, inst_list);
        free_reg(get_reg_stack(), temp_reg);
    }

    return inst_list;
}

/* Returns the corresponding string and instructions for a leaf */
/* TODO: Only supports var_id and i_num */
ListNode_t *gencode_leaf_var(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, char *buffer, int buf_len)
{
    assert(expr != NULL);
    assert(buffer != NULL);

    StackNode_t *stack_node;
    int offset;

    switch(expr->type)
    {
        case EXPR_VAR_ID:
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: gencode_leaf_var: id = %s\n", expr->expr_data.id);
            #endif
            stack_node = find_label(expr->expr_data.id);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: gencode_leaf_var: stack_node = %p\n", stack_node);
            #endif

            if(stack_node != NULL)
            {
                snprintf(buffer, buf_len, "-%d(%%rbp)", stack_node->offset);
            }
            else if(nonlocal_flag() == 1)
            {
                inst_list = codegen_get_nonlocal(inst_list, expr->expr_data.id, &offset);
                snprintf(buffer, buf_len, "-%d(%s)", offset, current_non_local_reg64());
            }
            else
            {
                HashNode_t *node = NULL;
                if (ctx != NULL && ctx->symtab != NULL &&
                    FindIdent(&node, ctx->symtab, expr->expr_data.id) >= 0 &&
                    node != NULL && node->hash_type == HASHTYPE_CONST)
                {
                    snprintf(buffer, buf_len, "$%d", node->const_int_value);
                }
                else
                {
                    fprintf(stderr, "ERROR: Non-local codegen support disabled (buggy)!\n");
                    fprintf(stderr, "Enable with flag '-non-local' after required flags\n");
                    exit(1);
                }
            }

            break;

        case EXPR_INUM:
            snprintf(buffer, buf_len, "$%d", expr->expr_data.i_num);
            break;

        default:
            assert(0 && "Unsupported expr type in gencode!");
            break;
    }

    return inst_list;
}

/* TODO: Assumes eax and edx registers are free for division */
ListNode_t *gencode_op(struct Expression *expr, char *left, char *right,
    ListNode_t *inst_list)
{
    assert(expr != NULL);
    assert(left != NULL);
    assert(right != NULL);

    int type;
    char buffer[50];

    switch(expr->type)
    {
        case EXPR_ADDOP:
            type = expr->expr_data.addop_data.addop_type;
            switch(type)
            {
                case PLUS:
                    snprintf(buffer, 50, "\taddl\t%s, %s\n", right, left);
                    inst_list = add_inst(inst_list, buffer);
                    break;
                case MINUS:
                    snprintf(buffer, 50, "\tsubl\t%s, %s\n", right, left);
                    inst_list = add_inst(inst_list, buffer);
                    break;
                default:
                    assert(0 && "Bad addop type!");
                    break;
            }

            break;

        case EXPR_MULOP:
            type = expr->expr_data.mulop_data.mulop_type;
            if(type == STAR)
            {
                snprintf(buffer, 50, "\timull\t%s, %s\n", right, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == MOD)
            {
                snprintf(buffer, 50, "\tmovl\t%s, %%eax\n", left);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, 50, "\tcdq\n");
                inst_list = add_inst(inst_list, buffer);

                char reg[10];
                snprintf(reg, 10, "%%r10d");
                snprintf(buffer, 50, "\tmovl\t%s, %s\n", right, reg);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, 50, "\tidivl\t%s\n", reg);
                inst_list = add_inst(inst_list, buffer);

                snprintf(buffer, 50, "\tmovl\t%%edx, %s\n", left);
                inst_list = add_inst(inst_list, buffer);
            }
            /* NOTE: Division and modulus is a more special case */
            else if(type == SLASH || type == DIV)
            {
                #ifdef DEBUG_CODEGEN
                CODEGEN_DEBUG("DEBUG: gencode_op: left = %s, right = %s\n", left, right);
                #endif
                // left is the dividend, right is the divisor
                snprintf(buffer, 50, "\tpushq\t%%rax\n");
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, 50, "\tpushq\t%%rdx\n");
                inst_list = add_inst(inst_list, buffer);


                snprintf(buffer, 50, "\tmovl\t%s, %%eax\n", left);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, 50, "\tcdq\n");
                inst_list = add_inst(inst_list, buffer);

                char reg[10];
                snprintf(reg, 10, "%%r10d");
                snprintf(buffer, 50, "\tmovl\t%s, %s\n", right, reg);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, 50, "\tidivl\t%s\n", reg);
                inst_list = add_inst(inst_list, buffer);

                snprintf(buffer, 50, "\tmovl\t%%eax, %s\n", left);
                inst_list = add_inst(inst_list, buffer);

                snprintf(buffer, 50, "\tpopq\t%%rdx\n");
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, 50, "\tpopq\t%%rax\n");
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == MOD)
            {
                inst_list = gencode_modulus(left, right, inst_list);
            }
            else
            {
                assert(0 && "Bad mulop type!");
                break;
            }

            break;

        case EXPR_RELOP:
            snprintf(buffer, 50, "\tcmpl\t%s, %s\n", left, right);
            inst_list = add_inst(inst_list, buffer);

            break;

        default:
            assert(0 && "Unsupported expr type in gencode!");
            break;
    }

    return inst_list;
}


/* Gets simple operation of a node */
/* DEPRECATED */
ListNode_t *gencode_op_deprecated(struct Expression *expr, ListNode_t *inst_list,
    char *buffer, int buf_len)
{
    assert(expr != NULL);
    int type;

    switch(expr->type)
    {
        case EXPR_ADDOP:
            type = expr->expr_data.addop_data.addop_type;
            if(type == PLUS)
                snprintf(buffer, buf_len, "addl");
            else if(type == MINUS)
                snprintf(buffer, buf_len, "subl");
            else
            {
                assert(0 && "Bad addop type!");
            }

            break;

        case EXPR_MULOP:
            type = expr->expr_data.addop_data.addop_type;
            if(type == STAR)
                snprintf(buffer, buf_len, "imull");
            else
            {
                assert(0 && "Bad mulop type!");
            }

            break;

        default:
            assert(0 && "Unsupported expr type in gencode!");
            break;
    }

    return inst_list;
}
