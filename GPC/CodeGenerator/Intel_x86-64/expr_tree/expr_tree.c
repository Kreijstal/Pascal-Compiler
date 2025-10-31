/*
    Damon Gwinn
    Tree of simple expressions for the gencode algorithm
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

static inline const char *select_register_name(const Register_t *reg, int type_tag)
{
    if (reg == NULL)
        return NULL;
    return codegen_type_uses_qword(type_tag) ? reg->bit_64 : reg->bit_32;
}

static inline int expr_uses_qword(const struct Expression *expr)
{
    return expr != NULL && codegen_type_uses_qword(expr->resolved_type);
}

static ListNode_t *emit_store_to_stack(ListNode_t *inst_list, const Register_t *reg,
    int type_tag, int offset)
{
    if (inst_list == NULL || reg == NULL)
        return inst_list;

    const char *reg_name = select_register_name(reg, type_tag);
    if (reg_name == NULL)
        return inst_list;

    char buffer[64];
    if (codegen_type_uses_qword(type_tag))
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", reg_name, offset);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", reg_name, offset);
    return add_inst(inst_list, buffer);
}

static ListNode_t *emit_load_from_stack(ListNode_t *inst_list, const Register_t *reg,
    int type_tag, int offset)
{
    if (inst_list == NULL || reg == NULL)
        return inst_list;

    const char *reg_name = select_register_name(reg, type_tag);
    if (reg_name == NULL)
        return inst_list;

    char buffer[64];
    if (codegen_type_uses_qword(type_tag))
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", offset, reg_name);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", offset, reg_name);
    return add_inst(inst_list, buffer);
}

static ListNode_t *gencode_real_binary_op(const char *left_operand,
    const char *right_operand, const char *dest, ListNode_t *inst_list,
    const char *sse_mnemonic)
{
    if (left_operand == NULL || right_operand == NULL || dest == NULL || sse_mnemonic == NULL)
        return inst_list;

    char buffer[80];

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", left_operand);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm1\n", right_operand);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\t%s\t%%xmm1, %%xmm0\n", sse_mnemonic);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n", dest);
    inst_list = add_inst(inst_list, buffer);
    return inst_list;
}

static ListNode_t *gencode_real_negate(const char *value_operand,
    const char *dest, ListNode_t *inst_list)
{
    if (value_operand == NULL || dest == NULL)
        return inst_list;

    char buffer[96];

    if (strcmp(value_operand, dest) != 0)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_operand, dest);
        inst_list = add_inst(inst_list, buffer);
    }

    snprintf(buffer, sizeof(buffer), "\txorq\t$0x8000000000000000, %s\n", dest);
    return add_inst(inst_list, buffer);
}

static const char *reg64_to_reg32(const char *reg_name, char *buffer, size_t buf_size)
{
    if (reg_name == NULL)
        return NULL;
    if (reg_name[0] != '%' || reg_name[1] != 'r')
        return reg_name;

    if (strcmp(reg_name, "%rax") == 0) {
        snprintf(buffer, buf_size, "%%eax");
        return buffer;
    }
    if (strcmp(reg_name, "%rbx") == 0) {
        snprintf(buffer, buf_size, "%%ebx");
        return buffer;
    }
    if (strcmp(reg_name, "%rcx") == 0) {
        snprintf(buffer, buf_size, "%%ecx");
        return buffer;
    }
    if (strcmp(reg_name, "%rdx") == 0) {
        snprintf(buffer, buf_size, "%%edx");
        return buffer;
    }
    if (strcmp(reg_name, "%rsi") == 0) {
        snprintf(buffer, buf_size, "%%esi");
        return buffer;
    }
    if (strcmp(reg_name, "%rdi") == 0) {
        snprintf(buffer, buf_size, "%%edi");
        return buffer;
    }
    if (strcmp(reg_name, "%rbp") == 0) {
        snprintf(buffer, buf_size, "%%ebp");
        return buffer;
    }
    if (strcmp(reg_name, "%rsp") == 0) {
        snprintf(buffer, buf_size, "%%esp");
        return buffer;
    }

    if (reg_name[2] >= '0' && reg_name[2] <= '9')
    {
        snprintf(buffer, buf_size, "%%r%sd", reg_name + 2);
        return buffer;
    }

    return reg_name;
}

/* Helper functions */
ListNode_t *gencode_sign_term(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case0(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case1(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case2(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case3(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
static ListNode_t *gencode_string_concat(expr_node_t *node, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_leaf_var(struct Expression *, ListNode_t *, CodeGenContext *, char *, int );
ListNode_t *gencode_op(struct Expression *expr, const char *left, const char *right,
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
        case EXPR_RECORD_ACCESS:
        case EXPR_INUM:
        case EXPR_RNUM:
        case EXPR_FUNCTION_CALL:
        case EXPR_STRING:
        case EXPR_BOOL:
        case EXPR_NIL:
        case EXPR_SET:
        case EXPR_POINTER_DEREF:
        case EXPR_ADDR:
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

static int leaf_expr_is_simple(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    switch (expr->type)
    {
        case EXPR_VAR_ID:
        case EXPR_INUM:
        case EXPR_RNUM:
        case EXPR_BOOL:
        case EXPR_NIL:
        case EXPR_SET:
            return 1;
        default:
            return 0;
    }
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
        char buffer[64];
        const char *src = select_register_name(node->reg, node->expr->resolved_type);
        const char *dst = select_register_name(target_reg, node->expr->resolved_type);
        if (src != NULL && dst != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmov%s\t%s, %s\n",
                codegen_type_uses_qword(node->expr->resolved_type) ? "q" : "l",
                src, dst);
            inst_list = add_inst(inst_list, buffer);
        }
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
            // For chained concatenations, we need to be careful about register usage
            // Move the second argument to RDX first, then the first argument to RCX
            // This prevents overwriting the second argument when target_reg is reused
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", rhs_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", target_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", rhs_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", target_reg->bit_64);
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
ListNode_t *gencode_modulus(const char *left, const char *right, ListNode_t *inst_list)
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
    assert(ctx != NULL);
    assert(target_reg != NULL);

    char buffer[50];

    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

    int type_tag = node->expr->resolved_type;
    const char *dest = select_register_name(target_reg, type_tag);
    if (type_tag == REAL_TYPE)
    {
        inst_list = gencode_real_negate(dest, dest, inst_list);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tneg%s\t%s\n",
            codegen_type_uses_qword(type_tag) ? "q" : "l", dest);
        inst_list = add_inst(inst_list, buffer);
    }

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
        if (expr->resolved_type == STRING_TYPE || expr->resolved_type == LONGINT_TYPE ||
            expr->resolved_type == REAL_TYPE)
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
    else if (expr->type == EXPR_RECORD_ACCESS)
    {
        return codegen_record_access(expr, inst_list, ctx, target_reg);
    }
    else if (expr->type == EXPR_POINTER_DEREF)
    {
        return codegen_pointer_deref_leaf(expr, inst_list, ctx, target_reg);
    }
    else if (expr->type == EXPR_ADDR)
    {
        return codegen_addressof_leaf(expr, inst_list, ctx, target_reg);
    }
    else if (expr->type == EXPR_STRING)
    {
        char label[20];
        snprintf(label, 20, ".LC%d", ctx->write_label_counter++);
        char add_rodata[1024];
        const char *readonly_section = codegen_readonly_section_directive();
        snprintf(add_rodata, 1024, "%s\n%s:\n\t.string \"%s\"\n\t.text\n",
            readonly_section, label, expr->expr_data.string);
        inst_list = add_inst(inst_list, add_rodata);
        snprintf(buffer, 50, "\tleaq\t%s(%%rip), %s\n", label, target_reg->bit_64);
        return add_inst(inst_list, buffer);
    }

    inst_list = gencode_leaf_var(expr, inst_list, ctx, buf_leaf, 30);

    int use_qword = codegen_type_uses_qword(expr->resolved_type);

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

    char buffer[50];
    char name_buf[30];
    struct Expression *expr, *right_expr;

    expr = node->expr;
    right_expr = node->right_expr->expr;
    assert(right_expr != NULL);
    if (!leaf_expr_is_simple(right_expr))
    {
        Register_t *rhs_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (rhs_reg == NULL)
        {
            StackNode_t *spill_loc = add_l_t("rhs");
            inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);
            snprintf(name_buf, sizeof(name_buf), "-%d(%%rbp)", spill_loc->offset);
            const char *tmp_name = select_register_name(target_reg, right_expr->resolved_type);
            if (tmp_name != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmov%s\t%s, %s\n",
                    codegen_type_uses_qword(right_expr->resolved_type) ? "q" : "l",
                    tmp_name, name_buf);
                inst_list = add_inst(inst_list, buffer);
            }
            inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
            const char *target_name = select_register_name(target_reg, expr->resolved_type);
            inst_list = gencode_op(expr, target_name, name_buf, inst_list);
        }
        else
        {
            StackNode_t *lhs_spill = add_l_t("case1_lhs");
            inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
            inst_list = emit_store_to_stack(inst_list, target_reg, expr->resolved_type, lhs_spill->offset);
            inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, rhs_reg);
            inst_list = emit_load_from_stack(inst_list, target_reg, expr->resolved_type, lhs_spill->offset);
            const char *target_name = select_register_name(target_reg, expr->resolved_type);
            const char *rhs_name = select_register_name(rhs_reg, expr->resolved_type);
            inst_list = gencode_op(expr, target_name, rhs_name, inst_list);
            free_reg(get_reg_stack(), rhs_reg);
        }
        return inst_list;
    }

    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

    inst_list = gencode_leaf_var(right_expr, inst_list, ctx, name_buf, 30);

    const char *target_name = select_register_name(target_reg, expr->resolved_type);
    inst_list = gencode_op(expr, target_name, name_buf, inst_list);

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
        inst_list = emit_store_to_stack(inst_list, target_reg,
            node->expr->resolved_type, spill_loc->offset);

        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

        char spill_mem[30];
        snprintf(spill_mem, 30, "-%d(%%rbp)", spill_loc->offset);
        const char *target_name = select_register_name(target_reg, node->expr->resolved_type);
        inst_list = gencode_op(node->expr, target_name, spill_mem, inst_list);
    }
    else
    {
        StackNode_t *rhs_spill = add_l_t("case2_rhs");
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, temp_reg);
        inst_list = emit_store_to_stack(inst_list, temp_reg, node->expr->resolved_type, rhs_spill->offset);
        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
        inst_list = emit_load_from_stack(inst_list, temp_reg, node->expr->resolved_type, rhs_spill->offset);
        const char *target_name = select_register_name(target_reg, node->expr->resolved_type);
        const char *temp_name = select_register_name(temp_reg, node->expr->resolved_type);
        inst_list = gencode_op(node->expr, target_name, temp_name, inst_list);
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
        inst_list = emit_store_to_stack(inst_list, target_reg,
            node->expr->resolved_type, spill_loc->offset);

        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);

        char spill_mem[30];
        snprintf(spill_mem, 30, "-%d(%%rbp)", spill_loc->offset);
        const char *target_name = select_register_name(target_reg, node->expr->resolved_type);
        inst_list = gencode_op(node->expr, target_name, spill_mem, inst_list);
    }
    else
    {
        StackNode_t *lhs_spill = add_l_t("case3_lhs");
        inst_list = emit_store_to_stack(inst_list, target_reg, node->expr->resolved_type, lhs_spill->offset);
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, temp_reg);
        inst_list = emit_load_from_stack(inst_list, target_reg, node->expr->resolved_type, lhs_spill->offset);
        const char *target_name = select_register_name(target_reg, node->expr->resolved_type);
        const char *temp_name = select_register_name(temp_reg, node->expr->resolved_type);
        inst_list = gencode_op(node->expr, target_name, temp_name, inst_list);
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
            {
                int scope_depth = 0;
                stack_node = find_label_with_depth(expr->expr_data.id, &scope_depth);
                #ifdef DEBUG_CODEGEN
                CODEGEN_DEBUG("DEBUG: gencode_leaf_var: stack_node = %p, scope_depth = %d\n", stack_node, scope_depth);
                #endif

                if(stack_node != NULL)
                {
                    if (scope_depth == 0)
                    {
                        /* Variable is in current scope, access normally */
                        snprintf(buffer, buf_len, "-%d(%%rbp)", stack_node->offset);
                    }
                    else
                    {
                        Register_t *frame_reg = codegen_acquire_static_link(ctx, &inst_list, scope_depth);
                        if (frame_reg != NULL)
                            snprintf(buffer, buf_len, "-%d(%s)", stack_node->offset, frame_reg->bit_64);
                        else
                        {
                            codegen_report_error(ctx,
                                "ERROR: Failed to acquire static link for variable %s.",
                                expr->expr_data.id);
                            snprintf(buffer, buf_len, "-%d(%%rbp)", stack_node->offset);
                        }
                    }
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

        case EXPR_NIL:
            snprintf(buffer, buf_len, "$0");
            break;

        case EXPR_SET:
            snprintf(buffer, buf_len, "$%u", expr->expr_data.set_data.bitmask);
            break;

        default:
            assert(0 && "Unsupported expr type in gencode!");
            break;
    }

    return inst_list;
}

/* TODO: Assumes eax and edx registers are free for division */
ListNode_t *gencode_op(struct Expression *expr, const char *left, const char *right,
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
            if (expr->resolved_type == SET_TYPE)
            {
                switch(type)
                {
                    case PLUS:
                        snprintf(buffer, 50, "\torl\t%s, %s\n", right, left);
                        inst_list = add_inst(inst_list, buffer);
                        break;
                    default:
                        assert(0 && "Unsupported set addop type!");
                        break;
                }
                break;
            }
            if (expr->resolved_type == REAL_TYPE)
            {
                const char *sse_op = NULL;
                switch (type)
                {
                    case PLUS:
                        sse_op = "addsd";
                        break;
                    case MINUS:
                        sse_op = "subsd";
                        break;
                    default:
                        assert(0 && "Unsupported real addop type!");
                        break;
                }
                if (sse_op != NULL)
                    inst_list = gencode_real_binary_op(left, right, left, inst_list, sse_op);
                break;
            }
            {
                const int use_qword_op = codegen_type_uses_qword(expr->resolved_type);
                const char arith_suffix = use_qword_op ? 'q' : 'l';
            switch(type)
            {
                case PLUS:
                {
                    /*
                     * The expression tree emits the literal 1 as the string "$1". Detecting that
                     * special case lets us use INC instead of ADD to save an instruction byte.
                     */
                    if(strcmp(right, "$1") == 0)
                        snprintf(buffer, 50, "\tinc%c\t%s\n", arith_suffix, left);
                    else
                        snprintf(buffer, 50, "\tadd%c\t%s, %s\n", arith_suffix, right, left);
                    inst_list = add_inst(inst_list, buffer);
                    break;
                }
                case MINUS:
                    snprintf(buffer, 50, "\tsub%c\t%s, %s\n", arith_suffix, right, left);
                    inst_list = add_inst(inst_list, buffer);
                    break;
                default:
                    assert(0 && "Bad addop type!");
                    break;
            }

            break;
            }

        case EXPR_MULOP:
            type = expr->expr_data.mulop_data.mulop_type;
            if (expr->resolved_type == SET_TYPE)
            {
                switch(type)
                {
                    case STAR:
                        snprintf(buffer, 50, "\tandl\t%s, %s\n", right, left);
                        inst_list = add_inst(inst_list, buffer);
                        break;
                    default:
                        assert(0 && "Unsupported set mulop type!");
                        break;
                }
                break;
            }
            if (expr->resolved_type == REAL_TYPE)
            {
                const char *sse_op = NULL;
                switch (type)
                {
                    case STAR:
                        sse_op = "mulsd";
                        break;
                    case SLASH:
                        sse_op = "divsd";
                        break;
                    case DIV:
                    case MOD:
                        assert(0 && "Unsupported real mulop type!");
                        break;
                    default:
                        break;
                }
                if (sse_op != NULL)
                    inst_list = gencode_real_binary_op(left, right, left, inst_list, sse_op);
                break;
            }
            {
                const int use_qword_op = codegen_type_uses_qword(expr->resolved_type);
                const char arith_suffix = use_qword_op ? 'q' : 'l';
            if(type == STAR)
            {
                snprintf(buffer, 50, "\timul%c\t%s, %s\n", arith_suffix, right, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == MOD)
            {
                if (use_qword_op)
                {
                    snprintf(buffer, 50, "\tmovq\t%s, %%rax\n", left);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcqo\n");

                    snprintf(buffer, 50, "\tmovq\t%s, %%r10\n", right);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tidivq\t%r10\n");

                    snprintf(buffer, 50, "\tmovq\t%%rdx, %s\n", left);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    char left32[16];
                    char right32[16];
                    const char *mod_left = reg64_to_reg32(left, left32, sizeof(left32));
                    const char *mod_right = reg64_to_reg32(right, right32, sizeof(right32));
                    snprintf(buffer, 50, "\tmovl\t%s, %%eax\n", mod_left);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcdq\n");

                    snprintf(buffer, 50, "\tmovl\t%s, %%r10d\n", mod_right);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tidivl\t%r10d\n");

                    snprintf(buffer, 50, "\tmovl\t%%edx, %s\n", mod_left);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            /* NOTE: Division and modulus is a more special case */
            else if(type == SLASH || type == DIV)
            {
                #ifdef DEBUG_CODEGEN
                CODEGEN_DEBUG("DEBUG: gencode_op: left = %s, right = %s\n", left, right);
                #endif
                // left is the dividend, right is the divisor
                snprintf(buffer, 50, "\tpushq\t%%rdx\n");
                inst_list = add_inst(inst_list, buffer);


                if (use_qword_op)
                {
                    snprintf(buffer, 50, "\tmovq\t%s, %%rax\n", left);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcqo\n");

                    snprintf(buffer, 50, "\tmovq\t%s, %%r10\n", right);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tidivq\t%r10\n");

                    snprintf(buffer, 50, "\tmovq\t%%rax, %s\n", left);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    char left32[16];
                    char right32[16];
                    const char *div_left = reg64_to_reg32(left, left32, sizeof(left32));
                    const char *div_right = reg64_to_reg32(right, right32, sizeof(right32));
                    snprintf(buffer, 50, "\tmovl\t%s, %%eax\n", div_left);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcdq\n");

                    snprintf(buffer, 50, "\tmovl\t%s, %%r10d\n", div_right);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tidivl\t%r10d\n");

                    snprintf(buffer, 50, "\tmovl\t%%eax, %s\n", div_left);
                    inst_list = add_inst(inst_list, buffer);
                }

                snprintf(buffer, 50, "\tpopq\t%%rdx\n");
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == MOD)
            {
                inst_list = gencode_modulus(left, right, inst_list);
            }
            else if(type == XOR)
            {
                snprintf(buffer, 50, "\txor%c\t%s, %s\n", arith_suffix, right, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == SHL)
            {
                char right32[16];
                const char *count = use_qword_op ? reg64_to_reg32(right, right32, sizeof(right32)) : right;
                snprintf(buffer, 50, "\tmovl\t%s, %%ecx\n", count);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, 50, "\tshl%c\t%%cl, %s\n", arith_suffix, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == SHR)
            {
                char right32[16];
                const char *count = use_qword_op ? reg64_to_reg32(right, right32, sizeof(right32)) : right;
                snprintf(buffer, 50, "\tmovl\t%s, %%ecx\n", count);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, 50, "\tsar%c\t%%cl, %s\n", arith_suffix, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == ROL)
            {
                char right32[16];
                const char *count = use_qword_op ? reg64_to_reg32(right, right32, sizeof(right32)) : right;
                snprintf(buffer, 50, "\tmovl\t%s, %%ecx\n", count);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, 50, "\trol%c\t%%cl, %s\n", arith_suffix, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == ROR)
            {
                char right32[16];
                const char *count = use_qword_op ? reg64_to_reg32(right, right32, sizeof(right32)) : right;
                snprintf(buffer, 50, "\tmovl\t%s, %%ecx\n", count);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, 50, "\tror%c\t%%cl, %s\n", arith_suffix, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                assert(0 && "Bad mulop type!");
                break;
            }

            break;
            }

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
