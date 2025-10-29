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
#include <stdint.h>
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
static ListNode_t *gencode_string_concat(expr_node_t *node, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_leaf_var(struct Expression *, ListNode_t *, CodeGenContext *, char *, int );
ListNode_t *gencode_op(struct Expression *expr, struct Expression *left_expr,
    struct Expression *right_expr, const char *left, const char *right,
    ListNode_t *inst_list);
ListNode_t *gencode_op_deprecated(struct Expression *expr, ListNode_t *inst_list,
    char *buffer, int buf_len);

static const char *reg_name_for_type(Register_t *reg, struct Expression *expr)
{
    if (reg == NULL)
        return NULL;

    int type = (expr != NULL) ? expr->resolved_type : UNKNOWN_TYPE;
    if (type == REAL_TYPE || type == STRING_TYPE || type == POINTER_TYPE)
        return reg->bit_64;
    return reg->bit_32;
}

static int expr_requires_qword(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    int type = expr->resolved_type;
    return (type == REAL_TYPE || type == STRING_TYPE || type == POINTER_TYPE);
}

ListNode_t *gencode_divide_const_no_optimize(char *left, char *right, ListNode_t *inst_list);
ListNode_t *gencode_divide_no_const(char *left, char *right, ListNode_t *inst_list);

/* Builds an expression tree out of an expression */
/* WARNING: Does not make deep copy of expression */
/* WARNING: Does not do relational expressions */
expr_node_t *build_expr_tree(struct Expression *expr)
{
    assert(expr != NULL);

    if (expr->type == EXPR_TYPECAST && expr->expr_data.typecast_data.expr != NULL)
        return build_expr_tree(expr->expr_data.typecast_data.expr);

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
        case EXPR_RNUM:
        case EXPR_FUNCTION_CALL:
        case EXPR_STRING:
        case EXPR_BOOL:
            new_node->left_expr = NULL;
            new_node->right_expr = NULL;
            break;

        case EXPR_TYPECAST:
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
    else if(node->expr->type == EXPR_ADDOP &&
        node->expr->expr_data.addop_data.addop_type == PLUS &&
        node->expr->resolved_type == STRING_TYPE)
    {
        inst_list = gencode_string_concat(node, inst_list, ctx, target_reg);
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

static ListNode_t *gencode_string_concat(expr_node_t *node, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg)
{
    if (node == NULL || node->left_expr == NULL || node->right_expr == NULL)
        return inst_list;

    char buffer[128];
    Register_t *rhs_reg = get_free_reg(get_reg_stack(), &inst_list);

    if (rhs_reg == NULL)
    {
        StackNode_t *spill_loc = add_l_t("str_concat_rhs");
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", target_reg->bit_64, spill_loc->offset);
        inst_list = add_inst(inst_list, buffer);

        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

        if (codegen_target_is_windows())
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", target_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n", spill_loc->offset);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", target_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rsi\n", spill_loc->offset);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    else
    {
        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, rhs_reg);

        if (codegen_target_is_windows())
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", target_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", rhs_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", target_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", rhs_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tgpc_string_concat\n");
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", target_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    if (rhs_reg != NULL)
        free_reg(get_reg_stack(), rhs_reg);
    free_arg_regs();
    return inst_list;
}

/* Gencode for modulus */
// left is right operand (B), right is left operand (A)
// calculates A mod B, stores result in A's location (right)
ListNode_t *gencode_modulus(const char *left, const char *right, ListNode_t *inst_list, int use_qword)
{
    StackNode_t *temp;
    char buffer[50];

    assert(left != NULL);
    assert(right != NULL);
    assert(inst_list != NULL);
    assert(left[0] != '$');

    /* Move dividend (left operand) into %rax/%eax. */
    if (left[0] == '%')
    {
        snprintf(buffer, sizeof(buffer), use_qword ? "\tmovq\t%s, %%rax\n" : "\tmovl\t%s, %%eax\n", left);
    }
    else if (left[0] == '$')
    {
        if (use_qword)
            snprintf(buffer, sizeof(buffer), "\tmovabsq\t%s, %%rax\n", left);
        else
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%eax\n", left);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), use_qword ? "\tmovq\t%s, %%rax\n" : "\tmovl\t%s, %%eax\n", left);
    }
    inst_list = add_inst(inst_list, buffer);

    // Sign extend eax to edx
    snprintf(buffer, 50, use_qword ? "\tcqo\n" : "\tcdq\n");
    inst_list = add_inst(inst_list, buffer);

    // If divisor (right operand) is a constant, move it to memory
    if(right[0] == '$')
    {
        temp = find_in_temp("TEMP_MOD");
        if(temp == NULL)
            temp = add_l_t("TEMP_MOD");
        snprintf(buffer, 50, use_qword ? "\tmovq\t%s, -%d(%%rbp)\n" : "\tmovl\t%s, -%d(%%rbp)\n", right, temp->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, 50, use_qword ? "\tidivq\t-%d(%%rbp)\n" : "\tidivl\t-%d(%%rbp)\n", temp->offset);
        inst_list = add_inst(inst_list, buffer);
    }
    else // Divisor is a register
    {
        snprintf(buffer, 50, use_qword ? "\tidivq\t%s\n" : "\tidivl\t%s\n", right);
        inst_list = add_inst(inst_list, buffer);
    }

    // Move remainder from edx to the target register (A's location, left)
    snprintf(buffer, 50, use_qword ? "\tmovq\t%%rdx, %s\n" : "\tmovl\t%%edx, %s\n", left);
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
        inst_list = codegen_pass_arguments(expr->expr_data.function_call_data.args_expr, inst_list, ctx,
            expr->expr_data.function_call_data.resolved_func, expr->expr_data.function_call_data.mangled_id);
        snprintf(buffer, 50, "\tcall\t%s\n", expr->expr_data.function_call_data.mangled_id);
        inst_list = add_inst(inst_list, buffer);
        if (expr->resolved_type == STRING_TYPE || expr->resolved_type == POINTER_TYPE)
            snprintf(buffer, 50, "\tmovq\t%%rax, %s\n", target_reg->bit_64);
        else
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

    int use_qword = expr_requires_qword(expr);

#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: Loading value %s into register %s\n", buf_leaf,
        use_qword ? target_reg->bit_64 : target_reg->bit_32);
#endif

    if (use_qword)
        snprintf(buffer, 50, "\tmovq\t%s, %s\n", buf_leaf, target_reg->bit_64);
    else
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

    const char *dest_reg = reg_name_for_type(target_reg, expr);
    inst_list = gencode_op(expr, node->left_expr->expr, right_expr, dest_reg, name_buf, inst_list);

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
        const char *stored_reg = reg_name_for_type(target_reg, node->right_expr->expr);
        if (stored_reg == NULL)
            stored_reg = target_reg->bit_32;
        if (expr_requires_qword(node->right_expr->expr))
            snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", stored_reg, spill_loc->offset);
        else
            snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", stored_reg, spill_loc->offset);
        inst_list = add_inst(inst_list, buffer);

        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

        char spill_mem[30];
        snprintf(spill_mem, 30, "-%d(%%rbp)", spill_loc->offset);
        const char *dest_reg = reg_name_for_type(target_reg, node->expr);
        inst_list = gencode_op(node->expr, node->left_expr->expr, node->right_expr->expr,
            dest_reg, spill_mem, inst_list);
    }
    else
    {
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, temp_reg);
        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
        const char *dest_reg = reg_name_for_type(target_reg, node->expr);
        const char *rhs_reg = reg_name_for_type(temp_reg, node->right_expr->expr);
        inst_list = gencode_op(node->expr, node->left_expr->expr, node->right_expr->expr,
            dest_reg, rhs_reg, inst_list);
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
        const char *stored_reg = reg_name_for_type(target_reg, node->left_expr->expr);
        if (stored_reg == NULL)
            stored_reg = target_reg->bit_32;
        if (expr_requires_qword(node->left_expr->expr))
            snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", stored_reg, spill_loc->offset);
        else
            snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", stored_reg, spill_loc->offset);
        inst_list = add_inst(inst_list, buffer);

        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);

        char spill_mem[30];
        snprintf(spill_mem, 30, "-%d(%%rbp)", spill_loc->offset);
        const char *dest_reg = reg_name_for_type(target_reg, node->expr);
        inst_list = gencode_op(node->expr, node->right_expr->expr, node->left_expr->expr,
            dest_reg, spill_mem, inst_list);
    }
    else
    {
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, temp_reg);
        const char *dest_reg = reg_name_for_type(target_reg, node->expr);
        const char *rhs_reg = reg_name_for_type(temp_reg, node->right_expr->expr);
        inst_list = gencode_op(node->expr, node->left_expr->expr, node->right_expr->expr,
            dest_reg, rhs_reg, inst_list);
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
                    snprintf(buffer, buf_len, "$%lld", node->const_int_value);
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
            snprintf(buffer, buf_len, "$%lld", expr->expr_data.i_num);
            break;

        case EXPR_RNUM:
        {
            double value = expr->expr_data.r_num;
            int64_t bits;
            memcpy(&bits, &value, sizeof(bits));
            snprintf(buffer, buf_len, "$%lld", (long long)bits);
            break;
        }

        case EXPR_BOOL:
            snprintf(buffer, buf_len, "$%d", expr->expr_data.bool_value ? 1 : 0);
            break;

        default:
            assert(0 && "Unsupported expr type in gencode!");
            break;
    }

    return inst_list;
}

/* TODO: Assumes eax and edx registers are free for division */
ListNode_t *gencode_op(struct Expression *expr, struct Expression *left_expr,
    struct Expression *right_expr, const char *left, const char *right,
    ListNode_t *inst_list)
{
    assert(expr != NULL);
    assert(left != NULL);
    assert(right != NULL);

    char buffer[64];

    int result_is_real = (expr->resolved_type == REAL_TYPE);
    if (result_is_real)
    {
        char scratch_reg[8];
        snprintf(scratch_reg, sizeof(scratch_reg), "%%r11");

        /* Load left operand into %%xmm0. */
        if (left[0] == '%')
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", left);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (left[0] == '$')
        {
            snprintf(buffer, sizeof(buffer), "\tmovabsq\t%s, %s\n", left, scratch_reg);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", scratch_reg);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovsd\t%s, %%xmm0\n", left);
            inst_list = add_inst(inst_list, buffer);
        }

        /* Load right operand into %%xmm1. */
        if (right[0] == '%')
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm1\n", right);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (right[0] == '$')
        {
            snprintf(buffer, sizeof(buffer), "\tmovabsq\t%s, %s\n", right, scratch_reg);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm1\n", scratch_reg);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovsd\t%s, %%xmm1\n", right);
            inst_list = add_inst(inst_list, buffer);
        }

        if (expr->type == EXPR_ADDOP)
        {
            int type = expr->expr_data.addop_data.addop_type;
            if (type == PLUS)
                inst_list = add_inst(inst_list, "\taddsd\t%xmm1, %xmm0\n");
            else if (type == MINUS)
                inst_list = add_inst(inst_list, "\tsubsd\t%xmm1, %xmm0\n");
            else
                assert(0 && "Unsupported addop for real numbers");
        }
        else if (expr->type == EXPR_MULOP)
        {
            int type = expr->expr_data.mulop_data.mulop_type;
            if (type == STAR)
                inst_list = add_inst(inst_list, "\tmulsd\t%xmm1, %xmm0\n");
            else if (type == SLASH)
                inst_list = add_inst(inst_list, "\tdivsd\t%xmm1, %xmm0\n");
            else
                assert(0 && "Unsupported mulop for real numbers");
        }
        else
        {
            assert(0 && "Unsupported real operation in gencode_op");
        }

        if (left[0] == '%')
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n", left);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovsd\t%%xmm0, %s\n", left);
            inst_list = add_inst(inst_list, buffer);
        }

        return inst_list;
    }

    int use_qword = expr_requires_qword(expr) || expr_requires_qword(left_expr);

    switch(expr->type)
    {
        case EXPR_ADDOP:
        {
            int type = expr->expr_data.addop_data.addop_type;
            if (type == PLUS)
            {
                if (!use_qword && strcmp(right, "$1") == 0)
                {
                    snprintf(buffer, sizeof(buffer), "\tincl\t%s\n", left);
                }
                else if (use_qword && strcmp(right, "$1") == 0)
                {
                    snprintf(buffer, sizeof(buffer), "\tincq\t%s\n", left);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), use_qword ? "\taddq\t%s, %s\n" : "\taddl\t%s, %s\n", right, left);
                }
                inst_list = add_inst(inst_list, buffer);
            }
            else if (type == MINUS)
            {
                snprintf(buffer, sizeof(buffer), use_qword ? "\tsubq\t%s, %s\n" : "\tsubl\t%s, %s\n", right, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                assert(0 && "Bad addop type!");
            }
            return inst_list;
        }

        case EXPR_MULOP:
        {
            int type = expr->expr_data.mulop_data.mulop_type;
            if (type == STAR)
            {
                snprintf(buffer, sizeof(buffer), use_qword ? "\timulq\t%s, %s\n" : "\timull\t%s, %s\n", right, left);
                inst_list = add_inst(inst_list, buffer);
                return inst_list;
            }
            else if (type == MOD)
            {
                return gencode_modulus(left, right, inst_list, use_qword);
            }
            else if (type == SLASH || type == DIV)
            {
                snprintf(buffer, sizeof(buffer), "\tpushq\t%%rax\n");
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tpushq\t%%rdx\n");
                inst_list = add_inst(inst_list, buffer);

                snprintf(buffer, sizeof(buffer), use_qword ? "\tmovq\t%s, %%rax\n" : "\tmovl\t%s, %%eax\n", left);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), use_qword ? "\tcqo\n" : "\tcdq\n");
                inst_list = add_inst(inst_list, buffer);

                const char *div_reg = use_qword ? "%r10" : "%r10d";
                snprintf(buffer, sizeof(buffer), use_qword ? "\tmovq\t%s, %s\n" : "\tmovl\t%s, %s\n", right, div_reg);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), use_qword ? "\tidivq\t%s\n" : "\tidivl\t%s\n", div_reg);
                inst_list = add_inst(inst_list, buffer);

                snprintf(buffer, sizeof(buffer), use_qword ? "\tmovq\t%%rax, %s\n" : "\tmovl\t%%eax, %s\n", left);
                inst_list = add_inst(inst_list, buffer);

                snprintf(buffer, sizeof(buffer), "\tpopq\t%%rdx\n");
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tpopq\t%%rax\n");
                inst_list = add_inst(inst_list, buffer);
                return inst_list;
            }
            else if (type == XOR)
            {
                snprintf(buffer, sizeof(buffer), use_qword ? "\txorq\t%s, %s\n" : "\txorl\t%s, %s\n", right, left);
                inst_list = add_inst(inst_list, buffer);
                return inst_list;
            }
            else if (type == SHL || type == SHR || type == ROL || type == ROR)
            {
                snprintf(buffer, sizeof(buffer), use_qword ? "\tmovq\t%s, %%rcx\n" : "\tmovl\t%s, %%ecx\n", right);
                inst_list = add_inst(inst_list, buffer);
                const char *op = NULL;
                switch (type)
                {
                    case SHL: op = use_qword ? "\tsalq\t%%cl, %s\n" : "\tsall\t%%cl, %s\n"; break;
                    case SHR: op = use_qword ? "\tsarq\t%%cl, %s\n" : "\tsarl\t%%cl, %s\n"; break;
                    case ROL: op = use_qword ? "\trolq\t%%cl, %s\n" : "\troll\t%%cl, %s\n"; break;
                    case ROR: op = use_qword ? "\trorq\t%%cl, %s\n" : "\trorl\t%%cl, %s\n"; break;
                }
                snprintf(buffer, sizeof(buffer), op, left);
                inst_list = add_inst(inst_list, buffer);
                return inst_list;
            }
            else
            {
                assert(0 && "Bad mulop type!");
            }
            break;
        }

        case EXPR_RELOP:
        {
            snprintf(buffer, sizeof(buffer), use_qword ? "\tcmpq\t%s, %s\n" : "\tcmpl\t%s, %s\n", left, right);
            inst_list = add_inst(inst_list, buffer);
            return inst_list;
        }

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
