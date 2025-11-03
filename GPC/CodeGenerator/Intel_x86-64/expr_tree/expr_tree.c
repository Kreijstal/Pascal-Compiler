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

/* Function to escape string literals for assembly .string directive */
static char *escape_string_for_assembly(const char *input)
{
    if (input == NULL)
        return NULL;

    /* Calculate maximum possible escaped length */
    size_t len = strlen(input);
    char *escaped = (char *)malloc(len * 2 + 1); /* Worst case: every char needs escaping */
    if (escaped == NULL)
        return NULL;

    char *dest = escaped;
    const char *src = input;

    while (*src != '\0')
    {
        switch (*src)
        {
            case '"':
                *dest++ = '\\';
                *dest++ = '"';
                break;
            case '\\':
                *dest++ = '\\';
                *dest++ = '\\';
                break;
            case '\n':
                *dest++ = '\\';
                *dest++ = 'n';
                break;
            case '\t':
                *dest++ = '\\';
                *dest++ = 't';
                break;
            case '\r':
                *dest++ = '\\';
                *dest++ = 'r';
                break;
            default:
                *dest++ = *src;
                break;
        }
        src++;
    }
    *dest = '\0';

    return escaped;
}

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

static ListNode_t *load_real_operand_into_xmm(CodeGenContext *ctx,
    const char *operand, const char *xmm_reg, ListNode_t *inst_list)
{
    if (ctx == NULL || operand == NULL || xmm_reg == NULL)
        return inst_list;

    char buffer[128];

    if (operand[0] == '$')
    {
        char label[32];
        snprintf(label, sizeof(label), ".LC%d", ctx->write_label_counter++);

        const char *readonly_section = codegen_readonly_section_directive();
        char rodata_buffer[192];
        snprintf(rodata_buffer, sizeof(rodata_buffer), "%s\n%s:\n\t.quad %s\n\t.text\n",
            readonly_section, label, operand + 1);
        inst_list = add_inst(inst_list, rodata_buffer);

        snprintf(buffer, sizeof(buffer), "\tmovsd\t%s(%%rip), %s\n", label, xmm_reg);
        return add_inst(inst_list, buffer);
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", operand, xmm_reg);
    return add_inst(inst_list, buffer);
}

static ListNode_t *gencode_real_binary_op(CodeGenContext *ctx,
    const char *left_operand, const char *right_operand, const char *dest,
    ListNode_t *inst_list, const char *sse_mnemonic)
{
    if (ctx == NULL || left_operand == NULL || right_operand == NULL ||
        dest == NULL || sse_mnemonic == NULL)
    {
        return inst_list;
    }

    inst_list = load_real_operand_into_xmm(ctx, left_operand, "%xmm0", inst_list);
    inst_list = load_real_operand_into_xmm(ctx, right_operand, "%xmm1", inst_list);

    char buffer[80];
    snprintf(buffer, sizeof(buffer), "\t%s\t%%xmm1, %%xmm0\n", sse_mnemonic);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n", dest);
    return add_inst(inst_list, buffer);
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

static const char *reg_to_reg32(const char *reg_name, char *buffer, size_t buf_size)
{
    if (reg_name == NULL)
        return NULL;
    if (reg_name[0] == '%' && reg_name[1] == 'r' && strchr(reg_name, 'd') == NULL)
        return reg64_to_reg32(reg_name, buffer, buf_size);
    return reg_name;
}

static const char *reg32_to_reg8(const char *reg_name, char *buffer, size_t buf_size)
{
    if (reg_name == NULL)
        return NULL;

    if (strcmp(reg_name, "%eax") == 0)
        return "%al";
    if (strcmp(reg_name, "%ebx") == 0)
        return "%bl";
    if (strcmp(reg_name, "%ecx") == 0)
        return "%cl";
    if (strcmp(reg_name, "%edx") == 0)
        return "%dl";
    if (strcmp(reg_name, "%esi") == 0)
        return "%sil";
    if (strcmp(reg_name, "%edi") == 0)
        return "%dil";
    if (strcmp(reg_name, "%ebp") == 0)
        return "%bpl";
    if (strcmp(reg_name, "%esp") == 0)
        return "%spl";

    size_t len = strlen(reg_name);
    if (len > 0 && reg_name[0] == '%' && reg_name[len - 1] == 'd')
    {
        if (buf_size > len)
        {
            memcpy(buffer, reg_name, len + 1);
            buffer[len - 1] = 'b';
            buffer[len] = '\0';
            return buffer;
        }
        return NULL;
    }

    return NULL;
}

static const char *reg32_to_reg64(const char *reg_name, char *buffer, size_t buf_size)
{
    if (reg_name == NULL)
        return NULL;

    if (strcmp(reg_name, "%eax") == 0)
        return "%rax";
    if (strcmp(reg_name, "%ebx") == 0)
        return "%rbx";
    if (strcmp(reg_name, "%ecx") == 0)
        return "%rcx";
    if (strcmp(reg_name, "%edx") == 0)
        return "%rdx";
    if (strcmp(reg_name, "%esi") == 0)
        return "%rsi";
    if (strcmp(reg_name, "%edi") == 0)
        return "%rdi";
    if (strcmp(reg_name, "%ebp") == 0)
        return "%rbp";
    if (strcmp(reg_name, "%esp") == 0)
        return "%rsp";

    size_t len = strlen(reg_name);
    if (len > 0 && reg_name[0] == '%' && reg_name[len - 1] == 'd')
    {
        if (buf_size > len)
        {
            memcpy(buffer, reg_name, len + 1);
            buffer[len - 1] = '\0';
            return buffer;
        }
        return NULL;
    }

    return reg_name;
}

/* Helper functions */
ListNode_t *gencode_sign_term(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case0(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case1(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case2(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case3(expr_node_t *node, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg);
static ListNode_t *promote_char_operand_to_string(expr_node_t *node, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *value_reg);
static ListNode_t *gencode_string_concat(expr_node_t *node, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_leaf_var(struct Expression *, ListNode_t *, CodeGenContext *, char *, int );
ListNode_t *gencode_op(struct Expression *expr, const char *left, const char *right,
    ListNode_t *inst_list, CodeGenContext *ctx);
ListNode_t *gencode_op_deprecated(struct Expression *expr, ListNode_t *inst_list,
    char *buffer, int buf_len, CodeGenContext *ctx);

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
        case EXPR_CHAR_CODE:
        case EXPR_BOOL:
        case EXPR_NIL:
        case EXPR_SET:
        case EXPR_POINTER_DEREF:
        case EXPR_ADDR:
        case EXPR_ADDR_OF_PROC:
            new_node->left_expr = NULL;
            new_node->right_expr = NULL;
            break;

        case EXPR_TYPECAST:
            new_node->left_expr = NULL;
            new_node->right_expr = NULL;
            break;

        case EXPR_RELOP:
            new_node->left_expr = build_expr_tree(expr->expr_data.relop_data.left);
            if (expr->expr_data.relop_data.type == NOT)
            {
                new_node->right_expr = NULL;
                break;
            }
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
        case EXPR_CHAR_CODE:
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
    else if (node->right_expr == NULL)
    {
        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
        const char *target_name = select_register_name(target_reg, node->expr->resolved_type);
        if (target_name != NULL)
            inst_list = gencode_op(node->expr, target_name, target_name, inst_list, ctx);
    }
    else if(node->right_expr != NULL && expr_tree_is_leaf(node->right_expr))
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

static ListNode_t *promote_char_operand_to_string(expr_node_t *node, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *value_reg)
{
    if (node == NULL || node->expr == NULL || value_reg == NULL)
        return inst_list;

    if (node->expr->resolved_type != CHAR_TYPE)
        return inst_list;

    const char *arg_reg32 = current_arg_reg32(0);
    if (arg_reg32 == NULL)
        return inst_list;

    char buffer[128];

    /* Move the character value into the first integer argument register, zero-extending as needed. */
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", value_reg->bit_32, arg_reg32);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tgpc_chr\n");
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", RETURN_REG_64, value_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    free_arg_regs();
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
        inst_list = promote_char_operand_to_string(node->right_expr, inst_list, ctx, target_reg);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", target_reg->bit_64, spill_loc->offset);
        inst_list = add_inst(inst_list, buffer);

        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
        inst_list = promote_char_operand_to_string(node->left_expr, inst_list, ctx, target_reg);

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
        StackNode_t *lhs_spill = add_l_t("str_concat_lhs");
        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
        inst_list = promote_char_operand_to_string(node->left_expr, inst_list, ctx, target_reg);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", target_reg->bit_64, lhs_spill->offset);
        inst_list = add_inst(inst_list, buffer);

        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, rhs_reg);
        inst_list = promote_char_operand_to_string(node->right_expr, inst_list, ctx, rhs_reg);

        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", lhs_spill->offset, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

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
    char buffer[128];

    assert(left != NULL);
    assert(right != NULL);
    assert(inst_list != NULL);

    // Move dividend (A, right) to eax
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%eax\n", right);
    inst_list = add_inst(inst_list, buffer);

    // Sign extend eax to edx
    snprintf(buffer, sizeof(buffer), "\tcltd\n");
    inst_list = add_inst(inst_list, buffer);

    // If divisor (B, left) is a constant, move it to memory
    if(left[0] == '$')
    {
        temp = find_in_temp("TEMP_MOD");
        if(temp == NULL)
            temp = add_l_t("TEMP_MOD");
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", left, temp->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tidivl\t-%d(%%rbp)\n", temp->offset);
        inst_list = add_inst(inst_list, buffer);
    }
    else // Divisor is a register
    {
        snprintf(buffer, sizeof(buffer), "\tidivl\t%s\n", left);
        inst_list = add_inst(inst_list, buffer);
    }

    // Move remainder from edx to the target register (A's location, right)
    snprintf(buffer, sizeof(buffer), "\tmovl\t%%edx, %s\n", right);
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
        /* For function calls, get the GpcType from the resolved_func if available.
         * Note: This still has a potential use-after-free if resolved_func points to freed memory.
         * TODO: Add cached GpcType to FunctionCall structure like we did for ProcedureCall. */
        struct GpcType *func_type = NULL;
        if (expr->expr_data.function_call_data.resolved_func != NULL)
        {
            func_type = expr->expr_data.function_call_data.resolved_func->type;
        }
        
        inst_list = codegen_pass_arguments(expr->expr_data.function_call_data.args_expr,
            inst_list, ctx, func_type, expr->expr_data.function_call_data.id, 0);
        snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", expr->expr_data.function_call_data.mangled_id);
        inst_list = add_inst(inst_list, buffer);
        if (expr->resolved_type == STRING_TYPE || expr->resolved_type == LONGINT_TYPE ||
            expr->resolved_type == REAL_TYPE)
            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", target_reg->bit_64);
        else
            snprintf(buffer, sizeof(buffer), "\tmovl\t%%eax, %s\n", target_reg->bit_32);
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
    else if (expr->type == EXPR_ADDR_OF_PROC)
    {
        HashNode_t *proc_symbol = expr->expr_data.addr_of_proc_data.procedure_symbol;
        if (proc_symbol == NULL || proc_symbol->mangled_id == NULL)
        {
            codegen_report_error(ctx, "ERROR: Missing symbol information for procedure address.");
            return inst_list;
        }
        /* Use leaq (Load Effective Address) with RIP-relative addressing to get the address of the procedure's label */
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", proc_symbol->mangled_id, target_reg->bit_64);
        return add_inst(inst_list, buffer);
    }
    else if (expr->type == EXPR_STRING)
    {
        if (expr->resolved_type == CHAR_TYPE)
        {
            unsigned char value = 0;
            if (expr->expr_data.string != NULL && expr->expr_data.string[0] != '\0')
                value = (unsigned char)expr->expr_data.string[0];
            snprintf(buffer, sizeof(buffer), "\tmovl\t$%u, %s\n", (unsigned)value, target_reg->bit_32);
            return add_inst(inst_list, buffer);
        }

        char label[20];
        snprintf(label, 20, ".LC%d", ctx->write_label_counter++);
        char add_rodata[1024];
        const char *readonly_section = codegen_readonly_section_directive();
        
        /* Escape the string for assembly */
        char *escaped_string = escape_string_for_assembly(expr->expr_data.string);
        if (escaped_string != NULL)
        {
            snprintf(add_rodata, 1024, "%s\n%s:\n\t.string \"%s\"\n\t.text\n",
                readonly_section, label, escaped_string);
            free(escaped_string);
        }
        else
        {
            /* Fallback: use original string if escaping fails */
            snprintf(add_rodata, 1024, "%s\n%s:\n\t.string \"%s\"\n\t.text\n",
                readonly_section, label, expr->expr_data.string);
        }
        
        inst_list = add_inst(inst_list, add_rodata);
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", label, target_reg->bit_64);
        return add_inst(inst_list, buffer);
    }

    inst_list = gencode_leaf_var(expr, inst_list, ctx, buf_leaf, 30);

    int use_qword = codegen_type_uses_qword(expr->resolved_type);

#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: Loading value %s into register %s\n", buf_leaf,
        use_qword ? target_reg->bit_64 : target_reg->bit_32);
#endif

    if (use_qword)
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", buf_leaf, target_reg->bit_64);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", buf_leaf, target_reg->bit_32);

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
            inst_list = gencode_op(expr, target_name, name_buf, inst_list, ctx);
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
            inst_list = gencode_op(expr, target_name, rhs_name, inst_list, ctx);
            free_reg(get_reg_stack(), rhs_reg);
        }
        return inst_list;
    }

    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

    inst_list = gencode_leaf_var(right_expr, inst_list, ctx, name_buf, 30);

    const char *target_name = select_register_name(target_reg, expr->resolved_type);
    inst_list = gencode_op(expr, target_name, name_buf, inst_list, ctx);

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
        inst_list = gencode_op(node->expr, target_name, spill_mem, inst_list, ctx);
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
        inst_list = gencode_op(node->expr, target_name, temp_name, inst_list, ctx);
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
        inst_list = gencode_op(node->expr, target_name, spill_mem, inst_list, ctx);
    }
    else
    {
        StackNode_t *lhs_spill = add_l_t("case3_lhs");
        inst_list = emit_store_to_stack(inst_list, target_reg, node->expr->resolved_type, lhs_spill->offset);
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, temp_reg);
        inst_list = emit_load_from_stack(inst_list, target_reg, node->expr->resolved_type, lhs_spill->offset);
        const char *target_name = select_register_name(target_reg, node->expr->resolved_type);
        const char *temp_name = select_register_name(temp_reg, node->expr->resolved_type);
        inst_list = gencode_op(node->expr, target_name, temp_name, inst_list, ctx);
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

        case EXPR_CHAR_CODE:
            snprintf(buffer, buf_len, "$%u", expr->expr_data.char_code);
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
    ListNode_t *inst_list, CodeGenContext *ctx)
{
    assert(expr != NULL);
    assert(left != NULL);
    assert(right != NULL);

    int type;
    char buffer[128];

    switch(expr->type)
    {
        case EXPR_ADDOP:
            type = expr->expr_data.addop_data.addop_type;
            if (expr->resolved_type == SET_TYPE)
            {
                switch(type)
                {
                    case PLUS:
                        snprintf(buffer, sizeof(buffer), "\torl\t%s, %s\n", right, left);
                        inst_list = add_inst(inst_list, buffer);
                        break;
                    case MINUS:
                        if (right[0] == '$')
                        {
                            unsigned long mask = strtoul(right + 1, NULL, 0);
                            unsigned int complement = ~((unsigned int)mask);
                            snprintf(buffer, sizeof(buffer), "\tandl\t$%u, %s\n", complement, left);
                            inst_list = add_inst(inst_list, buffer);
                        }
                        else
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%r10d\n", right);
                            inst_list = add_inst(inst_list, buffer);
                            inst_list = add_inst(inst_list, "\tnotl\t%r10d\n");
                            snprintf(buffer, sizeof(buffer), "\tandl\t%%r10d, %s\n", left);
                            inst_list = add_inst(inst_list, buffer);
                        }
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
                    inst_list = gencode_real_binary_op(ctx, left, right, left, inst_list, sse_op);
                break;
            }
            {
                const int use_qword_op = codegen_type_uses_qword(expr->resolved_type);
                const char arith_suffix = use_qword_op ? 'q' : 'l';
                if (expr->resolved_type == BOOL && type == OR)
                {
                    snprintf(buffer, sizeof(buffer), "\tor%c\t%s, %s\n", arith_suffix, right, left);
                    inst_list = add_inst(inst_list, buffer);
                    break;
                }
                switch(type)
            {
                case PLUS:
                {
                    /*
                     * The expression tree emits the literal 1 as the string "$1". Detecting that
                     * special case lets us use INC instead of ADD to save an instruction byte.
                     */
                    if(strcmp(right, "$1") == 0)
                        snprintf(buffer, sizeof(buffer), "\tinc%c\t%s\n", arith_suffix, left);
                    else
                        snprintf(buffer, sizeof(buffer), "\tadd%c\t%s, %s\n", arith_suffix, right, left);
                    inst_list = add_inst(inst_list, buffer);
                    break;
                }
                case MINUS:
                    snprintf(buffer, sizeof(buffer), "\tsub%c\t%s, %s\n", arith_suffix, right, left);
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
            if (expr->resolved_type == BOOL && type == AND)
            {
                snprintf(buffer, sizeof(buffer), "\tandl\t%s, %s\n", right, left);
                inst_list = add_inst(inst_list, buffer);
                break;
            }
            if (expr->resolved_type == SET_TYPE)
            {
                switch(type)
                {
                    case STAR:
                        snprintf(buffer, sizeof(buffer), "\tandl\t%s, %s\n", right, left);
                        inst_list = add_inst(inst_list, buffer);
                        break;
                    case XOR:
                        snprintf(buffer, sizeof(buffer), "\txorl\t%s, %s\n", right, left);
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
                    inst_list = gencode_real_binary_op(ctx, left, right, left, inst_list, sse_op);
                break;
            }
            {
                const int use_qword_op = codegen_type_uses_qword(expr->resolved_type);
                const char arith_suffix = use_qword_op ? 'q' : 'l';
            if(type == STAR)
            {
                snprintf(buffer, sizeof(buffer), "\timul%c\t%s, %s\n", arith_suffix, right, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == MOD)
            {
                if (use_qword_op)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rax\n", left);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcqo\n");

                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r10\n", right);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tidivq\t%r10\n");

                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rdx, %s\n", left);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    char left32[16];
                    char right32[16];
                    const char *mod_left = reg64_to_reg32(left, left32, sizeof(left32));
                    const char *mod_right = reg64_to_reg32(right, right32, sizeof(right32));
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%eax\n", mod_left);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcdq\n");

                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%r10d\n", mod_right);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tidivl\t%r10d\n");

                    snprintf(buffer, sizeof(buffer), "\tmovl\t%%edx, %s\n", mod_left);
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
                snprintf(buffer, sizeof(buffer), "\tpushq\t%%rdx\n");
                inst_list = add_inst(inst_list, buffer);


                if (use_qword_op)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rax\n", left);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcqo\n");

                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r10\n", right);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tidivq\t%r10\n");

                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", left);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    char left32[16];
                    char right32[16];
                    const char *div_left = reg64_to_reg32(left, left32, sizeof(left32));
                    const char *div_right = reg64_to_reg32(right, right32, sizeof(right32));
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%eax\n", div_left);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcdq\n");

                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%r10d\n", div_right);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tidivl\t%r10d\n");

                    snprintf(buffer, sizeof(buffer), "\tmovl\t%%eax, %s\n", div_left);
                    inst_list = add_inst(inst_list, buffer);
                }

                snprintf(buffer, sizeof(buffer), "\tpopq\t%%rdx\n");
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == MOD)
            {
                inst_list = gencode_modulus(left, right, inst_list);
            }
            else if(type == XOR)
            {
                snprintf(buffer, sizeof(buffer), "\txor%c\t%s, %s\n", arith_suffix, right, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == SHL)
            {
                char right32[16];
                const char *count = use_qword_op ? reg64_to_reg32(right, right32, sizeof(right32)) : right;
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", count);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tshl%c\t%%cl, %s\n", arith_suffix, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == SHR)
            {
                char right32[16];
                const char *count = use_qword_op ? reg64_to_reg32(right, right32, sizeof(right32)) : right;
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", count);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tsar%c\t%%cl, %s\n", arith_suffix, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == ROL)
            {
                char right32[16];
                const char *count = use_qword_op ? reg64_to_reg32(right, right32, sizeof(right32)) : right;
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", count);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\trol%c\t%%cl, %s\n", arith_suffix, left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == ROR)
            {
                char right32[16];
                const char *count = use_qword_op ? reg64_to_reg32(right, right32, sizeof(right32)) : right;
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", count);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tror%c\t%%cl, %s\n", arith_suffix, left);
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
        {
            int relop_kind = expr->expr_data.relop_data.type;
            struct Expression *left_expr = expr->expr_data.relop_data.left;
            struct Expression *right_expr = expr->expr_data.relop_data.right;

            if (relop_kind == NOT)
            {
                char left32_buf[16];
                char left8_buf[16];
                const char *left32 = reg_to_reg32(left, left32_buf, sizeof(left32_buf));
                const char *left8 = reg32_to_reg8(left32, left8_buf, sizeof(left8_buf));
                if (left32 != NULL && left8 != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", left32, left32);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tsete\t%s\n", left8);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8, left32);
                    inst_list = add_inst(inst_list, buffer);
                }
                break;
            }

            if (relop_kind == IN)
            {
                char left32_buf[16];
                char right32_buf[16];
                char left8_buf[16];
                const char *left32 = reg_to_reg32(left, left32_buf, sizeof(left32_buf));
                const char *right32 = reg_to_reg32(right, right32_buf, sizeof(right32_buf));
                const char *left8 = reg32_to_reg8(left32, left8_buf, sizeof(left8_buf));

                const char *bit_index = left32;
                const char *bit_base = right32;

                if (right != NULL && right[0] == '$')
                {
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%r10d\n", right);
                    inst_list = add_inst(inst_list, buffer);
                    bit_base = "%r10d";
                }

                if (left32 != NULL && left8 != NULL && bit_index != NULL && bit_base != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tbtl\t%s, %s\n", bit_index, bit_base);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tsetc\t%s\n", left8);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8, left32);
                    inst_list = add_inst(inst_list, buffer);
                }
                break;
            }

            if (left_expr != NULL && left_expr->resolved_type == REAL_TYPE)
            {
                char left32_buf[16];
                char right32_buf[16];
                const char *left32 = reg_to_reg32(left, left32_buf, sizeof(left32_buf));
                const char *right32 = reg_to_reg32(right, right32_buf, sizeof(right32_buf));
                if (left32 != NULL)
                {
                    char true_label[32];
                    char done_label[32];
                    gen_label(true_label, sizeof(true_label), ctx);
                    gen_label(done_label, sizeof(done_label), ctx);

                    char left64_buf[16];
                    const char *left_candidate = (left32 != NULL) ? left32 : left;
                    const char *left64 = left_candidate;
                    if (left_candidate != NULL && left_candidate[0] == '%')
                    {
                        const char *converted = reg32_to_reg64(left_candidate, left64_buf, sizeof(left64_buf));
                        if (converted != NULL)
                            left64 = converted;
                    }

                    StackNode_t *lhs_spill = NULL;
                    if (left64 != NULL && left64[0] == '%')
                    {
                        lhs_spill = add_l_t("relop_real_lhs");
                        if (lhs_spill == NULL)
                        {
                            codegen_report_error(ctx, "ERROR: Unable to allocate temporary for real comparison.");
                            break;
                        }

                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", left64, lhs_spill->offset);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovsd\t-%d(%%rbp), %%xmm1\n", lhs_spill->offset);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    if (lhs_spill == NULL && left != NULL)
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovsd\t%s, %%xmm1\n", left);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    int rhs_loaded = 0;
                    StackNode_t *rhs_spill = NULL;
                    if (right != NULL && right[0] == '$')
                    {
                        char label[32];
                        snprintf(label, sizeof(label), ".LC%d", ctx->write_label_counter++);

                        const char *readonly_section = codegen_readonly_section_directive();
                        char rodata_buffer[192];
                        snprintf(rodata_buffer, sizeof(rodata_buffer), "%s\n%s:\n\t.quad %s\n\t.text\n",
                            readonly_section, label, right + 1);
                        inst_list = add_inst(inst_list, rodata_buffer);

                        snprintf(buffer, sizeof(buffer), "\tmovsd\t%s(%%rip), %%xmm0\n", label);
                        inst_list = add_inst(inst_list, buffer);
                        rhs_loaded = 1;
                    }

                    if (!rhs_loaded)
                    {
                        if (right != NULL && right[0] == '%')
                        {
                            char right64_buf[16];
                            const char *right_candidate = right;
                            if (right32 != NULL)
                                right_candidate = right32;
                            const char *right64 = right_candidate;
                            const char *converted = reg32_to_reg64(right_candidate, right64_buf, sizeof(right64_buf));
                            if (converted != NULL)
                                right64 = converted;
                            if (right64 != NULL && right64[0] == '%')
                            {
                                if (rhs_spill == NULL)
                                    rhs_spill = add_l_t("relop_real_rhs_reg");
                                if (rhs_spill == NULL)
                                {
                                    codegen_report_error(ctx, "ERROR: Unable to allocate temporary for real comparison.");
                                    break;
                                }

                                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", right64, rhs_spill->offset);
                                inst_list = add_inst(inst_list, buffer);
                                snprintf(buffer, sizeof(buffer), "\tmovsd\t-%d(%%rbp), %%xmm0\n", rhs_spill->offset);
                                inst_list = add_inst(inst_list, buffer);
                                rhs_loaded = 1;
                            }
                        }
                        if (!rhs_loaded && right != NULL)
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovsd\t%s, %%xmm0\n", right);
                            inst_list = add_inst(inst_list, buffer);
                        }
                    }

                    snprintf(buffer, sizeof(buffer), "\txorl\t%s, %s\n", left32, left32);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tucomisd\t%xmm0, %xmm1\n");

                    switch (relop_kind)
                    {
                        case EQ:
                            snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tje\t%s\n", true_label);
                            inst_list = add_inst(inst_list, buffer);
                            break;
                        case NE:
                            snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", true_label);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tjne\t%s\n", true_label);
                            inst_list = add_inst(inst_list, buffer);
                            break;
                        case LT:
                            snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tjb\t%s\n", true_label);
                            inst_list = add_inst(inst_list, buffer);
                            break;
                        case LE:
                            snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tjbe\t%s\n", true_label);
                            inst_list = add_inst(inst_list, buffer);
                            break;
                        case GT:
                            snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tja\t%s\n", true_label);
                            inst_list = add_inst(inst_list, buffer);
                            break;
                        case GE:
                            snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tjae\t%s\n", true_label);
                            inst_list = add_inst(inst_list, buffer);
                            break;
                        default:
                            break;
                    }

                    snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", done_label);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "%s:\n", true_label);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", left32);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "%s:\n", done_label);
                    inst_list = add_inst(inst_list, buffer);
                }
                break;
            }

            {
                int left_type = (left_expr != NULL) ? left_expr->resolved_type : UNKNOWN_TYPE;
                int right_type = (right_expr != NULL) ? right_expr->resolved_type : UNKNOWN_TYPE;
                int use_qword = codegen_type_uses_qword(left_type) || codegen_type_uses_qword(right_type);
                char cmp_suffix = use_qword ? 'q' : 'l';

                const char *cmp_left = left;
                const char *cmp_right = right;
                char left32_buf[16];
                char right32_buf[16];
                char left64_buf[16];
                char right64_buf[16];

                if (use_qword)
                {
                    const char *left_candidate = left;
                    const char *left32 = reg_to_reg32(left, left32_buf, sizeof(left32_buf));
                    if (left32 != NULL)
                        left_candidate = left32;
                    const char *left64 = reg32_to_reg64(left_candidate, left64_buf, sizeof(left64_buf));
                    if (left64 != NULL)
                        cmp_left = left64;

                    const char *right_candidate = right;
                    const char *right32 = reg_to_reg32(right, right32_buf, sizeof(right32_buf));
                    if (right32 != NULL)
                        right_candidate = right32;
                    const char *right64 = reg32_to_reg64(right_candidate, right64_buf, sizeof(right64_buf));
                    if (right64 != NULL)
                        cmp_right = right64;
                }

                snprintf(buffer, sizeof(buffer), "\tcmp%c\t%s, %s\n", cmp_suffix, cmp_right, cmp_left);
                inst_list = add_inst(inst_list, buffer);

                const char *set_instr = NULL;
                switch (relop_kind)
                {
                    case EQ:
                        set_instr = "sete";
                        break;
                    case NE:
                        set_instr = "setne";
                        break;
                    case LT:
                        set_instr = "setl";
                        break;
                    case LE:
                        set_instr = "setle";
                        break;
                    case GT:
                        set_instr = "setg";
                        break;
                    case GE:
                        set_instr = "setge";
                        break;
                    default:
                        break;
                }

                if (set_instr != NULL)
                {
                    char left32_buf[16];
                    char left8_buf[16];
                    const char *left32 = reg_to_reg32(left, left32_buf, sizeof(left32_buf));
                    const char *left8 = reg32_to_reg8(left32, left8_buf, sizeof(left8_buf));
                    if (left32 != NULL && left8 != NULL)
                    {
                        snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", set_instr, left8);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8, left32);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }
            }

            break;
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
    char *buffer, int buf_len, CodeGenContext *ctx)
{
    (void)ctx;
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
