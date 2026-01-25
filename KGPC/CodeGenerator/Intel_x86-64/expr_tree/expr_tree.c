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
#include <limits.h>
#include "../codegen.h"
#include "expr_tree.h"
#include "../register_types.h"
#include "../codegen_expression.h"
#include "../stackmng/stackmng.h"
#include "../../../flags.h"
#include "../../../Parser/List/List.h"
#include "../../../Parser/ParseTree/tree.h"
#include "../../../Parser/ParseTree/tree_types.h"
#include "../../../Parser/ParseTree/KgpcType.h"
#include "../../../Parser/ParseTree/type_tags.h"
#include "../../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../../../Parser/SemanticCheck/NameMangling.h"

#ifndef CODEGEN_POINTER_SIZE_BYTES
#define CODEGEN_POINTER_SIZE_BYTES 8
#endif

static ListNode_t *codegen_builtin_dynarray_length(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg)
{
    if (expr == NULL || ctx == NULL || target_reg == NULL)
        return inst_list;

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        codegen_report_error(ctx, "ERROR: Length intrinsic expects one argument.");
        return inst_list;
    }

    struct Expression *array_expr = (struct Expression *)args->cur;
    if (array_expr == NULL)
        return inst_list;

    Register_t *desc_reg = NULL;
    if (codegen_expr_is_addressable(array_expr))
        inst_list = codegen_address_for_expr(array_expr, inst_list, ctx, &desc_reg);
    else
        inst_list = codegen_expr_with_result(array_expr, inst_list, ctx, &desc_reg);

    if (codegen_had_error(ctx) || desc_reg == NULL)
        return inst_list;

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t8(%s), %s\n",
        desc_reg->bit_64, target_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), desc_reg);
    return inst_list;
}

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

static inline const char *select_register_name_tag(const Register_t *reg, int type_tag)
{
    if (reg == NULL)
        return NULL;
    return codegen_type_uses_qword(type_tag) ? reg->bit_64 : reg->bit_32;
}

static inline const char *select_register_name(const Register_t *reg,
    const struct Expression *expr, int fallback_tag)
{
    if (reg == NULL)
        return NULL;
    /* Prefer KgpcType-aware width when expression info is available */
    if (expr != NULL && expr_uses_qword_kgpctype(expr))
        return reg->bit_64;
    return select_register_name_tag(reg, fallback_tag);
}

static void expr_tree_register_spill_handler(Register_t *reg, StackNode_t *spill_slot, void *context)
{
    expr_node_t *node = (expr_node_t *)context;
    if (node == NULL || spill_slot == NULL)
        return;
    node->spill_slot = spill_slot;
    node->reg = NULL;
}

static int leaf_expr_requires_reference_value(struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL || ctx == NULL || expr->type != EXPR_VAR_ID)
        return 0;

    int scope_depth = 0;
    StackNode_t *stack_node = find_label_with_depth(expr->expr_data.id, &scope_depth);
    HashNode_t *symbol_node = NULL;
    if (ctx->symtab != NULL)
        FindIdent(&symbol_node, ctx->symtab, expr->expr_data.id);

    int treat_as_reference = 0;
    if (stack_node != NULL && stack_node->is_reference)
        treat_as_reference = 1;
    else if (symbol_node != NULL && symbol_node->is_var_parameter)
        treat_as_reference = 1;

    if (!treat_as_reference)
        return 0;

    int expr_type = expr_get_type_tag(expr);
    if (expr_type == UNKNOWN_TYPE && symbol_node != NULL && symbol_node->type != NULL)
        expr_type = kgpc_type_get_legacy_tag(symbol_node->type);

    int is_array_like =
        expr->array_is_dynamic ||
        expr->is_array_expr ||
        (expr->resolved_kgpc_type != NULL && kgpc_type_is_array(expr->resolved_kgpc_type));

    if (!is_array_like && expr_type != RECORD_TYPE && expr_type != SET_TYPE)
        return 1;

    return 0;
}

static int expr_effective_storage_type(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr != NULL && expr->resolved_kgpc_type != NULL)
    {
        int legacy_tag = kgpc_type_get_legacy_tag(expr->resolved_kgpc_type);
        if (legacy_tag != UNKNOWN_TYPE)
            return legacy_tag;
    }
    
    /* Fall back to symbol table lookup for variables when the resolved type
     * wasn't propagated (common for string-like aliases). */
    if (expr != NULL && ctx != NULL && ctx->symtab != NULL && expr->type == EXPR_VAR_ID)
    {
        HashNode_t *sym_node = NULL;
        if (FindIdent(&sym_node, ctx->symtab, expr->expr_data.id) >= 0 &&
            sym_node != NULL && sym_node->type != NULL)
        {
            int sym_tag = kgpc_type_get_legacy_tag(sym_node->type);
            if (sym_tag != UNKNOWN_TYPE)
                return sym_tag;
        }
    }

    return (expr != NULL) ? expr->resolved_type : UNKNOWN_TYPE;
}

/**
 * Check if an expression requires 64-bit (qword) storage based on its type.
 * This checks both the type tag and storage_size from KgpcType.
 * This is needed to properly handle Int64/QWord/UInt64 which have storage_size=8
 * but use LONGINT_TYPE as their base type tag.
 */
static int expr_requires_qword(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    /* Check type tag first */
    int type_tag = expr->resolved_type;
    if (codegen_type_uses_qword(type_tag))
        return 1;
    
    /* Check storage_size in KgpcType for Int64/QWord/UInt64 */
    if (expr->resolved_kgpc_type != NULL)
    {
        struct TypeAlias *alias = kgpc_type_get_type_alias(expr->resolved_kgpc_type);
        if (alias != NULL && alias->storage_size >= 8)
            return 1;
    }
    
    /* Also check for large integer values that require 64 bits */
    if (expr->type == EXPR_INUM)
    {
        long long val = expr->expr_data.i_num;
        if (val > 2147483647LL || val < -2147483648LL)
            return 1;
    }
    
    return 0;
}

/* Forward declarations */
static const char *reg32_to_reg64(const char *reg_name, char *buffer, size_t buf_size);
static const char *reg64_to_reg32(const char *reg_name, char *buffer, size_t buf_size);
static int expr_is_single_real_local(const struct Expression *expr);

static int expr_is_single_real_local(const struct Expression *expr)
{
    if (expr == NULL || !expr_has_type_tag(expr, REAL_TYPE))
        return 0;

    KgpcType *type = expr_get_kgpc_type(expr);
    if (type == NULL)
        return 0;

    return (kgpc_type_sizeof(type) == 4);
}

static ListNode_t *emit_store_to_stack(ListNode_t *inst_list, const Register_t *reg,
    const struct Expression *expr, int type_tag, int offset)
{
    if (inst_list == NULL || reg == NULL)
        return inst_list;

    int use_qword = (expr != NULL && expr_uses_qword_kgpctype(expr)) ||
        codegen_type_uses_qword(type_tag);
    if (type_tag == REAL_TYPE && expr_is_single_real_local(expr))
        use_qword = 0;
    const char *reg_name = use_qword ? reg->bit_64 : reg->bit_32;
    if (reg_name == NULL)
        return inst_list;

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmov%c\t%s, -%d(%%rbp)\n",
        use_qword ? 'q' : 'l', reg_name, offset);
    return add_inst(inst_list, buffer);
}

static ListNode_t *emit_load_from_stack(ListNode_t *inst_list, const Register_t *reg,
    const struct Expression *expr, int type_tag, int offset)
{
    if (inst_list == NULL || reg == NULL)
        return inst_list;

    int use_qword = (expr != NULL && expr_uses_qword_kgpctype(expr)) ||
        codegen_type_uses_qword(type_tag);
    if (type_tag == REAL_TYPE && expr_is_single_real_local(expr))
        use_qword = 0;
    const char *reg_name = use_qword ? reg->bit_64 : reg->bit_32;
    if (reg_name == NULL)
        return inst_list;

    char buffer[64];
    if (use_qword)
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", offset, reg_name);
    else if (type_tag == CHAR_TYPE)
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t-%d(%%rbp), %s\n", offset, reg_name);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", offset, reg_name);
    return add_inst(inst_list, buffer);
}

static long long expr_integer_constant_value(const struct Expression *expr, const char *operand)
{
    if (expr != NULL)
    {
        switch (expr->type)
        {
            case EXPR_INUM:
                return expr->expr_data.i_num;
            case EXPR_CHAR_CODE:
                return (long long)expr->expr_data.char_code;
            case EXPR_BOOL:
                return expr->expr_data.bool_value ? 1 : 0;
            case EXPR_NIL:
                return 0;
            default:
                break;
        }
    }

    if (operand != NULL && operand[0] == '$')
        return strtoll(operand + 1, NULL, 10);

    return 0;
}

static ListNode_t *load_real_operand_into_xmm(CodeGenContext *ctx,
    struct Expression *operand_expr, const char *operand, const char *xmm_reg,
    ListNode_t *inst_list)
{
    if (ctx == NULL || operand == NULL || xmm_reg == NULL)
        return inst_list;

    int operand_is_real = operand_expr != NULL &&
        expr_has_type_tag(operand_expr, REAL_TYPE);
    int operand_is_longint = operand_expr != NULL &&
        expr_has_type_tag(operand_expr, LONGINT_TYPE);
    int operand_is_integer_like =
        (operand_expr != NULL &&
         (expr_has_type_tag(operand_expr, LONGINT_TYPE) ||
          expr_has_type_tag(operand_expr, INT_TYPE) ||
          expr_has_type_tag(operand_expr, BOOL) ||
          expr_has_type_tag(operand_expr, CHAR_TYPE)));

    char buffer[192];
    int is_single_real = 0;
    if (operand_is_real && operand_expr != NULL)
    {
        KgpcType *real_type = expr_get_kgpc_type(operand_expr);
        if (real_type == NULL && ctx != NULL && ctx->symtab != NULL &&
            operand_expr->type == EXPR_VAR_ID && operand_expr->expr_data.id != NULL)
        {
            HashNode_t *node = NULL;
            if (FindIdent(&node, ctx->symtab, operand_expr->expr_data.id) == 0 &&
                node != NULL && node->type != NULL)
                real_type = node->type;
        }
        if (real_type != NULL)
        {
            long long size = kgpc_type_sizeof(real_type);
            if (size == 4)
                is_single_real = 1;
        }
    }

    if (operand_is_real)
    {
        if (operand[0] == '$')
        {
            char label[32];
            snprintf(label, sizeof(label), ".LC%d", ctx->write_label_counter++);

            const char *readonly_section = codegen_readonly_section_directive();
            char rodata_buffer[192];
            if (is_single_real)
            {
                union {
                    float f;
                    int32_t i;
                } converter;
                converter.f = (float)operand_expr->expr_data.r_num;
                snprintf(rodata_buffer, sizeof(rodata_buffer), "%s\n%s:\n\t.long %d\n\t.text\n",
                    readonly_section, label, (int)converter.i);
            }
            else
            {
                snprintf(rodata_buffer, sizeof(rodata_buffer), "%s\n%s:\n\t.quad %s\n\t.text\n",
                    readonly_section, label, operand + 1);
            }
            inst_list = add_inst(inst_list, rodata_buffer);

            if (is_single_real)
            {
                snprintf(buffer, sizeof(buffer), "\tmovss\t%s(%%rip), %s\n", label, xmm_reg);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tcvtss2sd\t%s, %s\n", xmm_reg, xmm_reg);
                return add_inst(inst_list, buffer);
            }
            snprintf(buffer, sizeof(buffer), "\tmovsd\t%s(%%rip), %s\n", label, xmm_reg);
            return add_inst(inst_list, buffer);
        }

        const char *source_operand = operand;
        char source_buf[16];
        if (operand[0] == '%')
        {
            const char *converted = reg32_to_reg64(operand, source_buf, sizeof(source_buf));
            if (converted != NULL)
                source_operand = converted;
        }

        if (operand[0] == '%')
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", source_operand, xmm_reg);
            return add_inst(inst_list, buffer);
        }

        if (is_single_real)
        {
            snprintf(buffer, sizeof(buffer), "\tmovss\t%s, %s\n", source_operand, xmm_reg);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tcvtss2sd\t%s, %s\n", xmm_reg, xmm_reg);
            return add_inst(inst_list, buffer);
        }

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", source_operand, xmm_reg);
        return add_inst(inst_list, buffer);
    }

    if (!operand_is_integer_like && operand_expr == NULL)
    {
        /* Fallback: assume operand already holds IEEE bits (e.g., from string literal) */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", operand, xmm_reg);
        return add_inst(inst_list, buffer);
    }

    if (operand[0] == '$')
    {
        long long int_value = expr_integer_constant_value(operand_expr, operand);
        double real_value = (double)int_value;
        union
        {
            double d;
            long long i;
        } converter;
        converter.d = real_value;

        char label[32];
        snprintf(label, sizeof(label), ".LC%d", ctx->write_label_counter++);
        const char *readonly_section = codegen_readonly_section_directive();
        char rodata_buffer[192];
        snprintf(rodata_buffer, sizeof(rodata_buffer), "%s\n%s:\n\t.quad %lld\n\t.text\n",
            readonly_section, label, (long long)converter.i);
        inst_list = add_inst(inst_list, rodata_buffer);

        snprintf(buffer, sizeof(buffer), "\tmovsd\t%s(%%rip), %s\n", label, xmm_reg);
        return add_inst(inst_list, buffer);
    }

    /* For cvtsi2sd, we need to match the register size to the instruction suffix:
     * cvtsi2sdl uses 32-bit register, cvtsi2sdq uses 64-bit register */
    const char *convert_instr;
    const char *convert_reg;
    char reg_buf[16];
    if (operand_is_longint)
    {
        convert_instr = "cvtsi2sdq";
        convert_reg = reg32_to_reg64(operand, reg_buf, sizeof(reg_buf));
    }
    else
    {
        convert_instr = "cvtsi2sdl";
        convert_reg = reg64_to_reg32(operand, reg_buf, sizeof(reg_buf));
    }
    snprintf(buffer, sizeof(buffer), "\t%s\t%s, %s\n", convert_instr, convert_reg, xmm_reg);
    return add_inst(inst_list, buffer);
}

static ListNode_t *gencode_real_binary_op(CodeGenContext *ctx,
    struct Expression *left_expr, const char *left_operand,
    struct Expression *right_expr, const char *right_operand,
    const char *dest, ListNode_t *inst_list, const char *sse_mnemonic)
{
    if (ctx == NULL || left_operand == NULL || right_operand == NULL ||
        dest == NULL || sse_mnemonic == NULL)
    {
        return inst_list;
    }

    inst_list = load_real_operand_into_xmm(ctx, left_expr, left_operand, "%xmm0", inst_list);
    inst_list = load_real_operand_into_xmm(ctx, right_expr, right_operand, "%xmm1", inst_list);

    char buffer[80];
    snprintf(buffer, sizeof(buffer), "\t%s\t%%xmm1, %%xmm0\n", sse_mnemonic);
    inst_list = add_inst(inst_list, buffer);

    /* movq requires a 64-bit register, so convert 32-bit register to 64-bit */
    char dest64_buf[16];
    const char *dest64 = reg32_to_reg64(dest, dest64_buf, sizeof(dest64_buf));
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n", dest64);
    return add_inst(inst_list, buffer);
}

static ListNode_t *gencode_real_negate(const char *value_operand,
    const char *dest, ListNode_t *inst_list)
{
    if (value_operand == NULL || dest == NULL)
        return inst_list;

    char buffer[96];
    /* movq requires a 64-bit register, so convert 32-bit operand/dest to 64-bit */
    char value64_buf[16];
    const char *value64 = reg32_to_reg64(value_operand, value64_buf, sizeof(value64_buf));
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", value64);
    inst_list = add_inst(inst_list, buffer);
    inst_list = add_inst(inst_list, "\tpxor\t%xmm1, %xmm1\n");
    inst_list = add_inst(inst_list, "\tsubsd\t%xmm0, %xmm1\n");
    char dest64_buf[16];
    const char *dest64 = reg32_to_reg64(dest, dest64_buf, sizeof(dest64_buf));
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm1, %s\n", dest64);
    return add_inst(inst_list, buffer);
}

/*
 * Convert a 64-bit register name to its 32-bit equivalent.
 * This function is idempotent - if the input is already a 32-bit register, it returns it unchanged.
 * Examples:
 *   %rax -> %eax
 *   %r8  -> %r8d
 *   %r8d -> %r8d (already 32-bit, returns unchanged)
 */
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
        /* Check if it's already a 32-bit register (ends with 'd') */
        size_t len = strlen(reg_name);
        if (len > 0 && reg_name[len - 1] == 'd')
        {
            /* Already a 32-bit register, return as-is */
            return reg_name;
        }
        snprintf(buffer, buf_size, "%%r%sd", reg_name + 2);
        return buffer;
    }

    return reg_name;
}

/*
 * Convert any register to its 32-bit equivalent if it's a 64-bit register.
 * Returns the register unchanged if it's already 32-bit or not a register that needs conversion.
 */
static const char *reg_to_reg32(const char *reg_name, char *buffer, size_t buf_size)
{
    if (reg_name == NULL)
        return NULL;
    if (reg_name[0] == '%' && reg_name[1] == 'r' && strchr(reg_name, 'd') == NULL)
        return reg64_to_reg32(reg_name, buffer, buf_size);
    return reg_name;
}

/*
 * Convert a 32-bit register name to its 8-bit equivalent.
 * Examples:
 *   %eax -> %al
 *   %r8d -> %r8b
 */
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

/*
 * Convert a 32-bit register name to its 64-bit equivalent.
 * This function is idempotent - if the input is already a 64-bit register, it returns it unchanged.
 * Examples:
 *   %eax -> %rax
 *   %r8d -> %r8
 *   %r8  -> %r8 (already 64-bit, returns unchanged)
 */
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

static int operand_is_32bit_register(const char *operand)
{
    if (operand == NULL)
        return 0;
    if (strcmp(operand, "%eax") == 0 ||
        strcmp(operand, "%ebx") == 0 ||
        strcmp(operand, "%ecx") == 0 ||
        strcmp(operand, "%edx") == 0 ||
        strcmp(operand, "%esi") == 0 ||
        strcmp(operand, "%edi") == 0)
        return 1;

    size_t len = strlen(operand);
    return (len > 0 && operand[0] == '%' && operand[len - 1] == 'd');
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
    new_node->spill_slot = NULL;

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
        case EXPR_ANONYMOUS_FUNCTION:
        case EXPR_ANONYMOUS_PROCEDURE:
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

    if (node->reg == NULL && node->spill_slot != NULL)
    {
        inst_list = emit_load_from_stack(inst_list, target_reg,
            node->expr, node->expr->resolved_type, node->spill_slot->offset);
        node->reg = target_reg;
        register_set_spill_callback(target_reg, expr_tree_register_spill_handler, node);
        node->spill_slot = NULL;
        return inst_list;
    }

    if(node->reg != NULL)
    {
        char buffer[64];
        const char *src = select_register_name(node->reg, node->expr, node->expr->resolved_type);
        const char *dst = select_register_name(target_reg, node->expr, node->expr->resolved_type);
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
        register_set_spill_callback(target_reg, expr_tree_register_spill_handler, node);
    }
    /* CASE 1 */
    else if (node->right_expr == NULL)
    {
        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
        const char *target_name = select_register_name(target_reg, node->expr, node->expr->resolved_type);
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

    int is_shortstring = (expr_get_type_tag(node->expr) == SHORTSTRING_TYPE) ||
        is_shortstring_array(node->expr->resolved_type, node->expr->is_array_expr);
    if (is_shortstring)
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
    inst_list = add_inst(inst_list, "\tcall\tkgpc_char_to_string\n");
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", RETURN_REG_64, value_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *promote_shortstring_operand_to_string(expr_node_t *node, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *value_reg)
{
    if (node == NULL || node->expr == NULL || ctx == NULL || value_reg == NULL)
        return inst_list;

    int is_shortstring = (expr_get_type_tag(node->expr) == SHORTSTRING_TYPE) ||
        is_shortstring_array(node->expr->resolved_type, node->expr->is_array_expr);
    if (!is_shortstring)
        return inst_list;

    const char *arg_reg64 = current_arg_reg64(0);
    if (arg_reg64 == NULL)
        return inst_list;

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, arg_reg64);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_shortstring_to_string\n");
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
        inst_list = promote_shortstring_operand_to_string(node->right_expr, inst_list, ctx, target_reg);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", target_reg->bit_64, spill_loc->offset);
        inst_list = add_inst(inst_list, buffer);

        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
        inst_list = promote_char_operand_to_string(node->left_expr, inst_list, ctx, target_reg);
        inst_list = promote_shortstring_operand_to_string(node->left_expr, inst_list, ctx, target_reg);

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
        inst_list = promote_shortstring_operand_to_string(node->left_expr, inst_list, ctx, target_reg);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", target_reg->bit_64, lhs_spill->offset);
        inst_list = add_inst(inst_list, buffer);

        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, rhs_reg);
        inst_list = promote_char_operand_to_string(node->right_expr, inst_list, ctx, rhs_reg);
        inst_list = promote_shortstring_operand_to_string(node->right_expr, inst_list, ctx, rhs_reg);

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
    inst_list = add_inst(inst_list, "\tcall\tkgpc_string_concat\n");
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", target_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    if (rhs_reg != NULL)
    {
        free_reg(get_reg_stack(), rhs_reg);
    }
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
    /* Note: inst_list can be NULL at the start of code generation */

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

    char buffer[256];

    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

    int type_tag = node->expr->resolved_type;
    const int use_qword = expr_uses_qword_kgpctype(node->expr) ||
        codegen_type_uses_qword(type_tag);
    const char *dest = select_register_name(target_reg, node->expr, type_tag);
    if (type_tag == REAL_TYPE)
    {
        inst_list = gencode_real_negate(dest, dest, inst_list);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tneg%s\t%s\n",
            use_qword ? "q" : "l", dest);
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

    /* Buffer must be large enough for very long mangled identifiers (dozens of suffixes). */
    char buffer[CODEGEN_MAX_INST_BUF];
    char buf_leaf[128];
    struct Expression *expr;

    expr = node->expr;
    assert(target_reg != NULL);
    
    CODEGEN_DEBUG("DEBUG gencode_case0: expr->type=%d\n", expr->type);

    if (expr->type == EXPR_FUNCTION_CALL)
    {
        const char *func_mangled_name = expr->expr_data.function_call_data.mangled_id;
        CODEGEN_DEBUG("DEBUG FUNCTION_CALL: mangled=%s, id=%s\n",
            func_mangled_name ? func_mangled_name : "NULL",
            expr->expr_data.function_call_data.id ? expr->expr_data.function_call_data.id : "NULL");
        
        if (func_mangled_name != NULL && strcmp(func_mangled_name, "__kgpc_dynarray_length") == 0)
        {
            inst_list = codegen_builtin_dynarray_length(expr, inst_list, ctx, target_reg);
            // NOTE: Don't free mangled_id here - it will be freed when the AST is destroyed
            // codegen_release_function_call_mangled_id(expr);
            return inst_list;
        }

        if (expr->expr_data.function_call_data.id != NULL &&
            strcasecmp(expr->expr_data.function_call_data.id, "SwapEndian") == 0)
        {
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;
            if (args == NULL || args->cur == NULL)
            {
                codegen_report_error(ctx, "ERROR: SwapEndian intrinsic expects one argument.");
                return inst_list;
            }

            struct Expression *arg_expr = (struct Expression *)args->cur;
            expr_node_t *arg_tree = build_expr_tree(arg_expr);
            if (arg_tree != NULL)
            {
                inst_list = gencode_expr_tree(arg_tree, inst_list, ctx, target_reg);
                free_expr_tree(arg_tree);
            }

            int use_qword = codegen_type_uses_qword(expr->resolved_type);
            const char *swap_reg = use_qword ? target_reg->bit_64 : target_reg->bit_32;
            char swap_suffix = use_qword ? 'q' : 'l';
            snprintf(buffer, sizeof(buffer), "\tbswap%c\t%s\n", swap_suffix, swap_reg);
            inst_list = add_inst(inst_list, buffer);
            return inst_list;
        }

        /* For function calls, get the KgpcType from cached call info populated during semcheck.
         * Fall back to a fresh symbol lookup when metadata is unavailable.
         * IMPORTANT: If is_call_info_valid is true, respect that even if call_kgpc_type is NULL.
         * This allows builtins (like UpCase(char)) to signal that no formal parameter
         * conversion is needed by setting is_call_info_valid=1 with call_kgpc_type=NULL. */
        struct KgpcType *func_type = NULL;
        HashNode_t *func_node = NULL;
        if (expr->expr_data.function_call_data.is_call_info_valid)
        {
            func_type = expr->expr_data.function_call_data.call_kgpc_type;
            if (func_type == NULL && getenv("KGPC_DEBUG_CODEGEN") != NULL) {
                fprintf(stderr, "[CodeGen] expr_tree: is_call_info_valid=1 but call_kgpc_type is NULL for id='%s'\n",
                    expr->expr_data.function_call_data.id ? expr->expr_data.function_call_data.id : "(null)");
            }
        }
        else if (ctx != NULL && ctx->symtab != NULL)
        {
            if (getenv("KGPC_DEBUG_CODEGEN") != NULL) {
                fprintf(stderr, "[CodeGen] expr_tree: is_call_info_valid=0 for id='%s', doing symbol lookup\n",
                    expr->expr_data.function_call_data.id ? expr->expr_data.function_call_data.id : "(null)");
            }
            /* First try lookup by id (short name like "Foo") */
            if (expr->expr_data.function_call_data.id != NULL &&
                FindIdent(&func_node, ctx->symtab,
                    expr->expr_data.function_call_data.id) >= 0 && func_node != NULL)
            {
                func_type = func_node->type;
            }
            /* If not found, try the mangled name (e.g., "TDoubleHelper__Foo_r_i") */
            else if (expr->expr_data.function_call_data.mangled_id != NULL &&
                FindIdent(&func_node, ctx->symtab,
                    expr->expr_data.function_call_data.mangled_id) >= 0 && func_node != NULL)
            {
                func_type = func_node->type;
            }
            if (func_type == NULL && getenv("KGPC_DEBUG_CODEGEN") != NULL) {
                fprintf(stderr, "[CodeGen] expr_tree: func_type lookup FAILED for id='%s' mangled='%s'\n",
                    expr->expr_data.function_call_data.id ? expr->expr_data.function_call_data.id : "(null)",
                    expr->expr_data.function_call_data.mangled_id ? expr->expr_data.function_call_data.mangled_id : "(null)");
            }
        }
        
        /* Check if the function being called requires a static link.
         * Note: KGPC's calling convention uses an implicit first argument (static link)
         * for normal Pascal functions/procedures, so we key off semantic metadata when available. */
        int callee_depth = 0;
        int have_depth = codegen_proc_static_link_depth(ctx, func_mangled_name, &callee_depth);
        int current_depth = codegen_get_lexical_depth(ctx);
        int should_pass_static_link = (func_node != NULL && func_node->requires_static_link) ||
            codegen_proc_requires_static_link(ctx, func_mangled_name);

        enum {
            STATIC_LINK_NONE = 0,
            STATIC_LINK_FROM_RBP,
            STATIC_LINK_FROM_SLOT,
            STATIC_LINK_FROM_REG
        } static_link_source = STATIC_LINK_NONE;
        int static_link_slot_offset = 0;
        Register_t *static_link_reg = NULL;
        int static_link_expr_active = 0;
        
        if (should_pass_static_link)
        {
            if (!have_depth)
            {
                static_link_source = STATIC_LINK_FROM_RBP;
            }
            else if (callee_depth > current_depth)
            {
                static_link_source = STATIC_LINK_FROM_RBP;
            }
            else if (callee_depth == current_depth)
            {
                StackNode_t *static_link_node = find_label("__static_link__");
                if (static_link_node != NULL)
                {
                    static_link_source = STATIC_LINK_FROM_SLOT;
                    static_link_slot_offset = static_link_node->offset;
                }
            }
            else
            {
                static_link_source = STATIC_LINK_FROM_REG;
                int levels_to_traverse = (current_depth - callee_depth) + 1;
                codegen_begin_expression(ctx);
                static_link_expr_active = 1;
                static_link_reg = codegen_acquire_static_link(ctx, &inst_list, levels_to_traverse);
            }
        }
        
        /* Check if this is a constructor call (e.g., TMyClass.Create)
         * Constructors need special handling: allocate memory and initialize VMT */
        int is_constructor = 0;
        Register_t *constructor_instance_reg = NULL;
        StackNode_t *constructor_instance_slot = NULL;
        
        if (func_mangled_name != NULL)
        {
            /* Check if name contains __Create (may be followed by type suffix like __Create_u) */
            const char *create_pos = strstr(func_mangled_name, "__Create");
            if (create_pos != NULL)
                is_constructor = 1;
            else if (strcmp(func_mangled_name, "Create") == 0)
                is_constructor = 1;
            
            CODEGEN_DEBUG("DEBUG Constructor Check: func_mangled_name=%s, is_constructor=%d\n", 
                func_mangled_name, is_constructor);
        }

        /* Avoid infinite recursion when a constructor ends up calling itself (e.g., inherited calls with
         * no parent implementation). In that case, just reuse Self instead of re-entering the constructor. */
        if (is_constructor && func_mangled_name != NULL &&
            ctx->current_subprogram_mangled != NULL &&
            strcmp(func_mangled_name, ctx->current_subprogram_mangled) == 0)
        {
            StackNode_t *self_slot = find_label("Self");
            if (self_slot != NULL && target_reg != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    self_slot->offset, target_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                return inst_list;
            }
        }

        /* Constructors for classes return the constructed instance by value,
         * which uses a hidden sret pointer in the first argument slot. */
        int has_record_return = expr_returns_sret(expr);
        int ctor_has_record_return = (is_constructor && has_record_return);
        StackNode_t *sret_slot = NULL;
        if (has_record_return && !is_constructor)
        {
            long long sret_size = 0;
            KgpcType *return_type = expr_get_kgpc_type(expr);
            if (return_type != NULL)
                sret_size = kgpc_type_sizeof(return_type);
            if (sret_size <= 0 || sret_size > INT_MAX)
                sret_size = CODEGEN_POINTER_SIZE_BYTES;
            sret_slot = add_l_t_bytes("__record_return_tmp__", (int)sret_size);
        }
        
        /* For constructors, allocate memory for the instance */
        if (is_constructor)
        {
            /* Try to get class size from expression's record_type first */
            struct RecordType *class_record = expr->record_type;
            
            CODEGEN_DEBUG("DEBUG Constructor: expr->record_type=%p\n", (void *)class_record);
            
            /* If not available, try to get it from the first argument (class type) */
            if (class_record == NULL)
            {
                ListNode_t *first_arg = expr->expr_data.function_call_data.args_expr;
                if (first_arg != NULL && first_arg->cur != NULL)
                {
                    struct Expression *class_expr = (struct Expression *)first_arg->cur;
                    if (class_expr != NULL)
                        class_record = class_expr->record_type;
                    
                    CODEGEN_DEBUG("DEBUG Constructor: first_arg class_expr=%p, class_record=%p\n", 
                        (void *)class_expr, (void *)class_record);
                }
            }
            
            if (class_record != NULL)
            {
                CODEGEN_DEBUG("DEBUG Constructor: class_record=%p, is_class=%d, properties=%p\n",
                    (void *)class_record, class_record->is_class, (void *)class_record->properties);
                
                if (getenv("KGPC_DEBUG_CODEGEN") != NULL) {
                    struct Expression *cexpr = (struct Expression*)expr->expr_data.function_call_data.args_expr->cur;
                    fprintf(stderr, "[CodeGen] gencode_case0: Checking class_record %p from class_expr %p (type=%d line=%d)\n", 
                        class_record, (void*)cexpr, cexpr->type, cexpr->line_num);
                }
            }
            
            if (class_record != NULL && record_type_is_class(class_record))
            {
                /* Get the size of the class instance */
                long long instance_size = 0;
                if (codegen_sizeof_record_type(ctx, class_record, &instance_size) == 0 &&
                    instance_size > 0)
                {
                    /* Allocate memory using calloc to zero-initialize all fields */
                    const char *calloc_arg1_reg = codegen_target_is_windows() ? "%rcx" : "%rdi";
                    const char *calloc_arg2_reg = codegen_target_is_windows() ? "%rdx" : "%rsi";
                    
                    /* calloc(1, size) - allocate 1 element of instance_size bytes, zeroed */
                    snprintf(buffer, sizeof(buffer), "\tmovq\t$1, %s\n", calloc_arg1_reg);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n",
                        instance_size, calloc_arg2_reg);
                    inst_list = add_inst(inst_list, buffer);
                    
                    inst_list = codegen_vect_reg(inst_list, 0);
                    inst_list = add_inst(inst_list, "\tcall\tcalloc\n");
                    free_arg_regs();
                    
                    /* Save the allocated instance pointer */
                    constructor_instance_reg = get_free_reg(get_reg_stack(), &inst_list);
                    if (constructor_instance_reg == NULL)
                    {
                        codegen_report_error(ctx, 
                            "ERROR: Unable to allocate register for constructor instance.");
                        goto cleanup_constructor;
                    }
                    
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n",
                        constructor_instance_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);

                    /* Spill the instance pointer to a temporary stack slot to survive the call. */
                    constructor_instance_slot = add_l_t("ctor_instance");
                    if (constructor_instance_slot != NULL)
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                            constructor_instance_reg->bit_64, constructor_instance_slot->offset);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    
                    /* Initialize VMT pointer using the class' static VMT label */
                    const char *vmt_label = NULL;
                    if (class_record->type_id != NULL) {
                        static char vmt_buf[256];
                        snprintf(vmt_buf, sizeof(vmt_buf), "%s_VMT", class_record->type_id);
                        vmt_label = vmt_buf;
                    }
                    if (vmt_label != NULL) {
                        Register_t *vmt_reg = get_free_reg(get_reg_stack(), &inst_list);
                        if (vmt_reg != NULL) {
                            snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                                vmt_label, vmt_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                                vmt_reg->bit_64, constructor_instance_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            free_reg(get_reg_stack(), vmt_reg);
                        }
                    }
                }
            }
        }
        
        /* Pass arguments, shifted by hidden return pointer and/or static link */
        int arg_start_index = (has_record_return ? 1 : 0) +
            (should_pass_static_link ? 1 : 0);
        int self_index = -1;
        
        /* For constructors, we need to:
         * 1. Skip the first argument in the list (class type)
         * 2. Shift register allocation by 1 to make room for Self */
        ListNode_t *args_to_pass = expr->expr_data.function_call_data.args_expr;
        if (is_constructor && constructor_instance_reg != NULL)
        {
            /* Place the hidden return pointer (sret) in the first argument slot */
            if (ctor_has_record_return)
            {
                const char *ret_reg = current_arg_reg64(0);
                if (ret_reg != NULL)
                {
                    if (constructor_instance_slot != NULL) {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                            constructor_instance_slot->offset, ret_reg);
                    } else {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                            constructor_instance_reg->bit_64, ret_reg);
                    }
                    inst_list = add_inst(inst_list, buffer);
                }
            }

            /* Move the newly allocated instance pointer into the correct argument register slot
             * for the implicit Self parameter, accounting for a possible static-link argument. */
            self_index = arg_start_index;
            const char *sysv_int_args[] = { "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };
            const char *win_int_args[]  = { "%rcx", "%rdx", "%r8", "%r9" };
            const char *self_reg = NULL;
            if (codegen_target_is_windows()) {
                if (self_index < (int)(sizeof(win_int_args) / sizeof(win_int_args[0])))
                    self_reg = win_int_args[self_index];
            } else {
                if (self_index < (int)(sizeof(sysv_int_args) / sizeof(sysv_int_args[0])))
                    self_reg = sysv_int_args[self_index];
            }
            if (self_reg != NULL) {
                if (constructor_instance_slot != NULL) {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                        constructor_instance_slot->offset, self_reg);
                } else {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                        constructor_instance_reg->bit_64, self_reg);
                }
                inst_list = add_inst(inst_list, buffer);
            }

            /* Skip the first argument (class type) in the argument list */
            if (args_to_pass != NULL)
                args_to_pass = args_to_pass->next;
            /* Shift register allocation by 1 for Self parameter */
            arg_start_index += 1;
        }
        
        const char *proc_name_hint = expr->expr_data.function_call_data.id;
        const char *mangled_name_hint = expr->expr_data.function_call_data.mangled_id;
        if (proc_name_hint == NULL)
            proc_name_hint = mangled_name_hint;

        if (is_constructor) {
            int args_count = 0;
            for (ListNode_t *c = args_to_pass; c != NULL; c = c->next) args_count++;
            fprintf(stderr, "[CODEGEN] Constructor %s: args_to_pass has %d arguments, arg_start_index=%d\n",
                proc_name_hint ? proc_name_hint : "(null)", args_count, arg_start_index);
        }

        inst_list = codegen_pass_arguments(args_to_pass,
            inst_list, ctx, func_type, proc_name_hint, arg_start_index);

        /* Invalidate static link cache after argument evaluation
         * because the static link register may have been clobbered
         * during argument evaluation. This prevents the bug where
         * nested function calls reuse the same register for different
         * static links, causing the wrong frame pointer to be passed. */
        if (static_link_source == STATIC_LINK_FROM_REG && static_link_reg != NULL)
        {
            /* The register was already acquired above, but argument evaluation
             * may have invalidated it. We need to reload it fresh. */
            free_reg(get_reg_stack(), static_link_reg);
            if (ctx->static_link_reg != NULL)
            {
                free_reg(get_reg_stack(), ctx->static_link_reg);
                ctx->static_link_reg = NULL;
                ctx->static_link_reg_level = 0;
            }
            /* Re-acquire the static link register after argument evaluation */
            int levels_to_traverse = (current_depth - callee_depth) + 1;
            static_link_reg = codegen_acquire_static_link(ctx, &inst_list, levels_to_traverse);
        }

        if (should_pass_static_link)
        {
            const char *dest_reg = current_arg_reg64(has_record_return ? 1 : 0);
            assert(dest_reg != NULL && "current_arg_reg64(..) should never return NULL");
            char link_buffer[64];
            switch (static_link_source)
            {
                case STATIC_LINK_FROM_RBP:
                    snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%%rbp, %s\n", dest_reg);
                    inst_list = add_inst(inst_list, link_buffer);
                    break;
                case STATIC_LINK_FROM_SLOT:
                    snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t-%d(%%rbp), %s\n",
                        static_link_slot_offset, dest_reg);
                    inst_list = add_inst(inst_list, link_buffer);
                    break;
                case STATIC_LINK_FROM_REG:
                    if (static_link_reg != NULL)
                    {
                        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%s, %s\n",
                            static_link_reg->bit_64, dest_reg);
                        inst_list = add_inst(inst_list, link_buffer);
                        free_reg(get_reg_stack(), static_link_reg);
                        static_link_reg = NULL;
                    }
                    break;
                default:
                    break;
            }
        }

        if (has_record_return && !is_constructor && sret_slot != NULL)
        {
            const char *ret_reg = current_arg_reg64(0);
            if (ret_reg != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    sret_slot->offset, ret_reg);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        
        /* For constructors, pass the allocated instance as the first argument (Self) */
        if (is_constructor && constructor_instance_reg != NULL && self_index >= 0)
        {
            const char *self_arg_reg = current_arg_reg64(self_index);
            if (constructor_instance_slot != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    constructor_instance_slot->offset, self_arg_reg);
            }
            else
            {
                const char *source_reg = constructor_instance_reg->bit_64;
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                    source_reg, self_arg_reg);
            }
            inst_list = add_inst(inst_list, buffer);
        }

        if (static_link_expr_active)
            codegen_end_expression(ctx);

        /* Check if this is a call through a procedural variable */
        if (expr->expr_data.function_call_data.is_procedural_var_call)
        {
            if (expr->expr_data.function_call_data.procedural_var_expr != NULL)
            {
                /* Evaluate expression producing the function pointer */
                Register_t *func_ptr_reg = NULL;
                codegen_begin_expression(ctx);
                inst_list = codegen_expr_with_result(
                    expr->expr_data.function_call_data.procedural_var_expr,
                    inst_list, ctx, &func_ptr_reg);
                codegen_end_expression(ctx);

                if (func_ptr_reg != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tcall\t*%s\n", func_ptr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), func_ptr_reg);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\t# ERROR: failed to evaluate procedural expression\n");
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else if (expr->expr_data.function_call_data.procedural_var_symbol != NULL)
            {
                /* Call through a procedural variable stored in a symbol */
                const char *var_name = expr->expr_data.function_call_data.id;
                
                /* Find the variable on the stack */
                StackNode_t *stack_node = find_label((char *)var_name);
                if (stack_node != NULL)
                {
                    /* Load the function pointer into a register */
                    Register_t *func_ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
                    if (func_ptr_reg != NULL)
                    {
                        if (stack_node->is_static)
                        {
                            const char *label = (stack_node->static_label != NULL) ?
                                stack_node->static_label : stack_node->label;
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s(%%rip), %s\n", 
                                label, func_ptr_reg->bit_64);
                        }
                        else
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", 
                                stack_node->offset, func_ptr_reg->bit_64);
                        }
                        inst_list = add_inst(inst_list, buffer);
                        
                        /* Call through the function pointer */
                        snprintf(buffer, sizeof(buffer), "\tcall\t*%s\n", func_ptr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        
                        free_reg(get_reg_stack(), func_ptr_reg);
                    }
                }
                else
                {
                    /* Variable not found - emit error */
                    snprintf(buffer, sizeof(buffer), "\t# ERROR: procedural variable %s not found\n", var_name);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\t# ERROR: procedural call target missing\n");
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else if (expr->expr_data.function_call_data.is_virtual_call &&
                 expr->expr_data.function_call_data.vmt_index >= 0)
        {
            /* Virtual method call - dispatch through VMT.
             * The instance pointer (Self) lives in the first argument register:
             *   - SysV:  %rdi
             *   - Win64: %rcx
             * The instance has the VMT pointer at offset 0.
             * We need to:
             *   1. Get the VMT pointer from the instance
             *   2. Index into the VMT to get the method pointer
             *   3. Call through the method pointer */
            int vmt_index = expr->expr_data.function_call_data.vmt_index;
            const char *self_reg = codegen_target_is_windows() ? "%rcx" : "%rdi";
            /* Copy instance pointer to r11 */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r11\n", self_reg);
            inst_list = add_inst(inst_list, buffer);
            /* Get VMT pointer (at offset 0 of instance) */
            snprintf(buffer, sizeof(buffer), "\tmovq\t(%%r11), %%r11\n");
            inst_list = add_inst(inst_list, buffer);
            /* VMT layout: [typeinfo at 0, method1 at 8, method2 at 16, ...] */
            int vmt_offset = vmt_index * 8;
            snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%r11), %%r11\n", vmt_offset);
            inst_list = add_inst(inst_list, buffer);
            /* Call through the VMT entry */
            snprintf(buffer, sizeof(buffer), "\tcall\t*%%r11\n");
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            /* Normal function call */
            const char *call_target = expr->expr_data.function_call_data.mangled_id;
            if (call_target == NULL || call_target[0] == '\0')
            {
                HashNode_t *resolved = expr->expr_data.function_call_data.resolved_func;
                if (resolved != NULL && resolved->mangled_id != NULL &&
                    resolved->mangled_id[0] != '\0')
                {
                    call_target = resolved->mangled_id;
                }
                else if (resolved != NULL && resolved->type != NULL &&
                         resolved->type->kind == TYPE_KIND_PROCEDURE)
                {
                    Tree_t *def = resolved->type->info.proc_info.definition;
                    if (def != NULL)
                    {
                        const char *alias = def->tree_data.subprogram_data.cname_override;
                        if (alias != NULL && alias[0] != '\0')
                            call_target = alias;
                        else if (def->tree_data.subprogram_data.mangled_id != NULL &&
                                 def->tree_data.subprogram_data.mangled_id[0] != '\0')
                            call_target = def->tree_data.subprogram_data.mangled_id;
                    }
                }
            }
            if ((call_target == NULL || call_target[0] == '\0') &&
                ctx != NULL && ctx->symtab != NULL &&
                expr->expr_data.function_call_data.id != NULL)
            {
                HashNode_t *sym = NULL;
                if (FindIdent(&sym, ctx->symtab,
                        expr->expr_data.function_call_data.id) == 0 &&
                    sym != NULL)
                {
                    if (sym->mangled_id != NULL && sym->mangled_id[0] != '\0')
                    {
                        call_target = sym->mangled_id;
                    }
                    else if (sym->type != NULL && sym->type->kind == TYPE_KIND_PROCEDURE)
                    {
                        Tree_t *def = sym->type->info.proc_info.definition;
                        if (def != NULL)
                        {
                            const char *alias = def->tree_data.subprogram_data.cname_override;
                            if (alias != NULL && alias[0] != '\0')
                                call_target = alias;
                            else if (def->tree_data.subprogram_data.mangled_id != NULL &&
                                     def->tree_data.subprogram_data.mangled_id[0] != '\0')
                                call_target = def->tree_data.subprogram_data.mangled_id;
                        }
                    }
                }
            }
            char *computed_mangled = NULL;
            if ((call_target == NULL || call_target[0] == '\0') &&
                expr->expr_data.function_call_data.call_kgpc_type != NULL &&
                expr->expr_data.function_call_data.call_kgpc_type->kind == TYPE_KIND_PROCEDURE)
            {
                Tree_t *def = expr->expr_data.function_call_data.call_kgpc_type
                    ->info.proc_info.definition;
                int is_external = 0;
                if (def != NULL)
                {
                    is_external = def->tree_data.subprogram_data.cname_flag != 0 ||
                        def->tree_data.subprogram_data.cname_override != NULL;
                }
                if (!is_external && expr->expr_data.function_call_data.id != NULL)
                {
                    computed_mangled = MangleFunctionName(
                        expr->expr_data.function_call_data.id,
                        expr->expr_data.function_call_data.call_kgpc_type->info.proc_info.params,
                        ctx->symtab);
                    if (computed_mangled != NULL && computed_mangled[0] != '\0')
                        call_target = computed_mangled;
                }
            }
            if ((call_target == NULL || call_target[0] == '\0') &&
                ctx != NULL && ctx->symtab != NULL &&
                expr->expr_data.function_call_data.id != NULL)
            {
                int arg_count = ListLength(expr->expr_data.function_call_data.args_expr);
                ListNode_t *candidates = FindAllIdents(ctx->symtab,
                    expr->expr_data.function_call_data.id);
                HashNode_t *unique = NULL;
                int matches = 0;
                for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
                {
                    HashNode_t *node = (HashNode_t *)cur->cur;
                    if (node == NULL || node->type == NULL ||
                        node->type->kind != TYPE_KIND_PROCEDURE)
                        continue;
                    ListNode_t *params = node->type->info.proc_info.params;
                    if (ListLength(params) != arg_count)
                        continue;
                    unique = node;
                    matches++;
                    if (matches > 1)
                        break;
                }
                if (matches == 1 && unique != NULL)
                {
                    if (unique->mangled_id != NULL && unique->mangled_id[0] != '\0')
                        call_target = unique->mangled_id;
                    else
                    {
                        computed_mangled = MangleFunctionName(
                            unique->id, unique->type->info.proc_info.params, ctx->symtab);
                        if (computed_mangled != NULL && computed_mangled[0] != '\0')
                            call_target = computed_mangled;
                    }
                }
                if (candidates != NULL)
                    DestroyList(candidates);
            }
            if (call_target == NULL)
                call_target = expr->expr_data.function_call_data.id;
            
            if (call_target != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", call_target);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                /* This should never happen - emit error */
                snprintf(buffer, sizeof(buffer), "\t# ERROR: function call with NULL target\n");
                inst_list = add_inst(inst_list, buffer);
            }
            if (computed_mangled != NULL)
                free(computed_mangled);
        }
        
        inst_list = codegen_cleanup_call_stack(inst_list, ctx);
        // NOTE: Don't free mangled_id here - it will be freed when the AST is destroyed
        // This was causing double-free errors in nested function calls within string concatenations
        // codegen_release_function_call_mangled_id(expr);
        
        /* For constructors, use the return value from the constructor (Self in %rax).
         * Constructors now properly return Self, so we don't need to rely on the
         * saved instance register which could be clobbered during the call. */
        if (is_constructor && constructor_instance_reg != NULL)
        {
            /* Use the allocated instance pointer as the result, regardless of what the callee returns. */
            if (constructor_instance_slot != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    constructor_instance_slot->offset, target_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                    constructor_instance_reg->bit_64, target_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            free_reg(get_reg_stack(), constructor_instance_reg);
        }
        else
        {
            /* Normal function return value */
            if (has_record_return && !is_constructor && sret_slot != NULL)
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    sret_slot->offset, target_reg->bit_64);
            else if (expr_has_type_tag(expr, REAL_TYPE))
            {
                long long real_size = expr_effective_size_bytes(expr);
                if (real_size == 4)
                    inst_list = add_inst(inst_list, "\tcvtss2sd\t%xmm0, %xmm0\n");
                snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n", target_reg->bit_64);
            }
            else if (expr_uses_qword_kgpctype(expr))
                snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", target_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\tmovl\t%%eax, %s\n", target_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }
        return inst_list;
        
cleanup_constructor:
        if (constructor_instance_reg != NULL)
            free_reg(get_reg_stack(), constructor_instance_reg);
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
    else if (expr->type == EXPR_ANONYMOUS_FUNCTION || expr->type == EXPR_ANONYMOUS_PROCEDURE)
    {
        /* Anonymous methods:
         * 1. Generate the function/procedure code as a nested definition
         * 2. Return a pointer to the generated function
         * 
         * The function is generated immediately and we return its address.
         */
        
        /* First, generate the anonymous function body */
        codegen_anonymous_method(expr, ctx, ctx->symtab);
        
        /* Check if generation succeeded */
        if (codegen_had_error(ctx))
        {
            return inst_list;
        }
        
        /* Now load the address of the generated function into the target register */
        struct AnonymousMethod *anon = &expr->expr_data.anonymous_method_data;
        if (anon->generated_name == NULL)
        {
            codegen_report_error(ctx, "ERROR: Anonymous method missing generated name");
            return inst_list;
        }
        
        /* Use leaq (Load Effective Address) with RIP-relative addressing to get the address */
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", 
                 anon->generated_name, target_reg->bit_64);
        return add_inst(inst_list, buffer);
    }
    else if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
    {
        /* Check if this is a string constant reference (but not a procedure address constant) */
        HashNode_t *node = NULL;
        if (FindIdent(&node, ctx->symtab, expr->expr_data.id) >= 0 &&
            node != NULL && node->hash_type == HASHTYPE_CONST &&
            node->const_string_value != NULL &&
            /* Skip if this is a procedure address constant - those use const_string_value
             * to store the procedure name, not an actual string value */
            !(node->type != NULL && node->type->kind == TYPE_KIND_PROCEDURE))
        {
            /* String constant - treat it like a string literal */
            char label[20];
            snprintf(label, 20, ".LC%d", ctx->write_label_counter++);
            char add_rodata[1024];
            const char *readonly_section = codegen_readonly_section_directive();
            
            /* Escape the string for assembly */
            char *escaped_string = escape_string_for_assembly(node->const_string_value);
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
                    readonly_section, label, node->const_string_value);
            }
            
            inst_list = add_inst(inst_list, add_rodata);
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", label, target_reg->bit_64);
            return add_inst(inst_list, buffer);
        }
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

    inst_list = gencode_leaf_var(expr, inst_list, ctx, buf_leaf, sizeof(buf_leaf));

    if (expr->type == EXPR_VAR_ID)
    {
        int scope_depth = 0;
        StackNode_t *stack_node = find_label_with_depth(expr->expr_data.id, &scope_depth);
        HashNode_t *symbol_node = NULL;
        if (ctx != NULL && ctx->symtab != NULL)
            FindIdent(&symbol_node, ctx->symtab, expr->expr_data.id);

        /* Procedures/functions used as values (e.g. @Proc, typed proc constants).
         * Only apply when the identifier is not a local/stack variable in this scope,
         * otherwise this breaks function result variables that share the function name. */
        if (stack_node == NULL &&
            symbol_node != NULL &&
            (symbol_node->hash_type == HASHTYPE_PROCEDURE ||
             symbol_node->hash_type == HASHTYPE_FUNCTION) &&
            symbol_node->mangled_id != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                symbol_node->mangled_id, target_reg->bit_64);
            return add_inst(inst_list, buffer);
        }

        /* Check if this is a procedure address constant - need leaq to get the label address */
        if (symbol_node != NULL && symbol_node->hash_type == HASHTYPE_CONST &&
            symbol_node->type != NULL && symbol_node->type->kind == TYPE_KIND_PROCEDURE)
        {
            /* For procedure address constants, use leaq to get the procedure's address */
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s, %s\n", buf_leaf, target_reg->bit_64);
            return add_inst(inst_list, buffer);
        }

        /* Check if this is a class type used as a value (for class references).
         * The buf_leaf will be "ClassName_VMT(%rip)" and we need leaq to get the address. */
        if (symbol_node != NULL && symbol_node->hash_type == HASHTYPE_TYPE &&
            symbol_node->type != NULL && symbol_node->type->kind == TYPE_KIND_POINTER &&
            symbol_node->type->info.points_to != NULL &&
            symbol_node->type->info.points_to->kind == TYPE_KIND_RECORD &&
            symbol_node->type->info.points_to->info.record_info != NULL &&
            record_type_is_class(symbol_node->type->info.points_to->info.record_info))
        {
            /* For class type used as value, use leaq to get the VMT address */
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s, %s\n", buf_leaf, target_reg->bit_64);
            return add_inst(inst_list, buffer);
        }

        /* Check if this is a VMT label - need address, not value */
        const char *var_name = expr->expr_data.id;
        size_t name_len = var_name != NULL ? strlen(var_name) : 0;
        int is_vmt_label = (name_len > 4 && strcmp(var_name + name_len - 4, "_VMT") == 0);
        
        if (is_vmt_label)
        {
            /* For VMT labels, use leaq to get the address instead of loading the value */
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s, %s\n", buf_leaf, target_reg->bit_64);
            return add_inst(inst_list, buffer);
        }

        int treat_as_reference = 0;
        if (stack_node != NULL && stack_node->is_reference)
            treat_as_reference = 1;
        else if (symbol_node != NULL && symbol_node->is_var_parameter)
            treat_as_reference = 1;

        if (treat_as_reference)
        {
            int expr_type = expr_get_type_tag(expr);
            if (expr_type == UNKNOWN_TYPE && symbol_node != NULL && symbol_node->type != NULL)
                expr_type = kgpc_type_get_legacy_tag(symbol_node->type);

            int is_array_like = expr->array_is_dynamic ||
                expr->is_array_expr ||
                (expr->resolved_kgpc_type != NULL &&
                 kgpc_type_is_array(expr->resolved_kgpc_type));

            int should_deref = 0;
            if (!is_array_like && expr_type != RECORD_TYPE && expr_type != SET_TYPE)
                should_deref = 1;

            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", buf_leaf, target_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            if (!should_deref)
                return inst_list;

            if (expr_type == REAL_TYPE && expr_is_single_real_local(expr))
            {
                char mem_operand[64];
                snprintf(mem_operand, sizeof(mem_operand), "(%s)", target_reg->bit_64);
                inst_list = load_real_operand_into_xmm(ctx, expr, mem_operand, "%xmm0", inst_list);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n", target_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                return inst_list;
            }

            char load_value[80];
            switch (expr_type)
            {
                case STRING_TYPE:
                case POINTER_TYPE:
                case PROCEDURE:
                case FILE_TYPE:
                case TEXT_TYPE:
                case REAL_TYPE:
                case UNKNOWN_TYPE:
                    snprintf(load_value, sizeof(load_value), "\tmovq\t(%s), %s\n",
                        target_reg->bit_64, target_reg->bit_64);
                    break;
                case LONGINT_TYPE:
                    // Now 4 bytes, use movl like INT_TYPE
                    snprintf(load_value, sizeof(load_value), "\tmovl\t(%s), %s\n",
                        target_reg->bit_64, target_reg->bit_32);
                    break;
                case CHAR_TYPE:
                case BOOL:
                    snprintf(load_value, sizeof(load_value), "\tmovzbl\t(%s), %s\n",
                        target_reg->bit_64, target_reg->bit_32);
                    break;
                default:
                    snprintf(load_value, sizeof(load_value), "\tmovl\t(%s), %s\n",
                        target_reg->bit_64, target_reg->bit_32);
                    break;
            }

            inst_list = add_inst(inst_list, load_value);
            return inst_list;
        }

        int is_shortstring = (expr_get_type_tag(expr) == SHORTSTRING_TYPE) ||
            is_shortstring_array(expr->resolved_type, expr->is_array_expr);
        if (is_shortstring)
        {
            if (buf_leaf[0] != '$')
            {
                snprintf(buffer, sizeof(buffer), "\tleaq\t%s, %s\n",
                    buf_leaf, target_reg->bit_64);
                return add_inst(inst_list, buffer);
            }
        }
    }

    if (expr_has_type_tag(expr, REAL_TYPE) && expr_is_single_real_local(expr) &&
        buf_leaf[0] != '$')
    {
        inst_list = load_real_operand_into_xmm(ctx, expr, buf_leaf, "%xmm0", inst_list);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n", target_reg->bit_64);
        return add_inst(inst_list, buffer);
    }

    /* Use expr_requires_qword to check both type tag and storage_size.
     * This properly handles Int64/QWord/UInt64 which have storage_size=8 */
    int desired_qword = expr_requires_qword(expr);
    int storage_tag = expr_effective_storage_type(expr, ctx);
    int storage_qword = 0;
    if (expr != NULL)
        storage_qword = expr_uses_qword_kgpctype(expr);
    if (!storage_qword)
        storage_qword = codegen_type_uses_qword(storage_tag);
    if (!desired_qword && storage_qword)
        desired_qword = 1;
    int is_immediate = (buf_leaf[0] == '$');

#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: Loading value %s into register %s (desired_qword=%d, storage_qword=%d)\n",
        buf_leaf, desired_qword ? target_reg->bit_64 : target_reg->bit_32,
        desired_qword, storage_qword);
#endif

    if (desired_qword && !storage_qword)
    {
        if (is_immediate)
        {
            long long imm_value = 0;
            if (expr != NULL)
            {
                switch (expr->type)
                {
                    case EXPR_INUM:
                        imm_value = expr->expr_data.i_num;
                        break;
                    case EXPR_BOOL:
                        imm_value = expr->expr_data.bool_value ? 1 : 0;
                        break;
                    case EXPR_CHAR_CODE:
                        imm_value = (unsigned int)expr->expr_data.char_code;
                        break;
                    default:
                        imm_value = strtoll(buf_leaf + 1, NULL, 10);
                        break;
                }
            }
            else
            {
                imm_value = strtoll(buf_leaf + 1, NULL, 10);
            }
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n",
                imm_value, target_reg->bit_64);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n",
                buf_leaf, target_reg->bit_64);
        }
        return add_inst(inst_list, buffer);
    }

    if (desired_qword)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", buf_leaf, target_reg->bit_64);
        return add_inst(inst_list, buffer);
    }

    if (storage_tag == CHAR_TYPE && !is_immediate)
    {
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", buf_leaf, target_reg->bit_32);
        return add_inst(inst_list, buffer);
    }

    /* Check if immediate value requires 64 bits */
    if (is_immediate)
    {
        long long imm_value = strtoll(buf_leaf + 1, NULL, 10);
        if (imm_value > 2147483647LL || imm_value < -2147483648LL)
        {
            /* Value doesn't fit in 32 bits - use 64-bit move */
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", imm_value, target_reg->bit_64);
            return add_inst(inst_list, buffer);
        }
    }

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

    char buffer[256];
    char name_buf[128];
    struct Expression *expr, *right_expr;

    expr = node->expr;
    right_expr = node->right_expr->expr;
    struct Expression *left_expr = node->left_expr != NULL ? node->left_expr->expr : NULL;
    assert(left_expr != NULL);
    assert(right_expr != NULL);
    int rhs_requires_reference = leaf_expr_requires_reference_value(right_expr, ctx);

    if (!leaf_expr_is_simple(right_expr) || rhs_requires_reference)
    {
        Register_t *rhs_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (rhs_reg == NULL)
        {
            StackNode_t *spill_loc = add_l_t("rhs");
            inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);
            snprintf(name_buf, sizeof(name_buf), "-%d(%%rbp)", spill_loc->offset);
            const char *tmp_name = select_register_name(target_reg, right_expr, right_expr->resolved_type);
            if (tmp_name != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmov%s\t%s, %s\n",
                    expr_uses_qword_kgpctype(right_expr) ?
                        "q" : (codegen_type_uses_qword(right_expr->resolved_type) ? "q" : "l"),
                    tmp_name, name_buf);
                inst_list = add_inst(inst_list, buffer);
            }
            inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
            const char *target_name = select_register_name(target_reg, left_expr, left_expr != NULL ? left_expr->resolved_type : expr->resolved_type);
            inst_list = gencode_op(expr, target_name, name_buf, inst_list, ctx);
        }
        else
        {
            StackNode_t *lhs_spill = add_l_t("case1_lhs");
            inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
            /* Use the left operand's type for spilling, not the binary expr's result type */
            inst_list = emit_store_to_stack(inst_list, target_reg, left_expr, left_expr->resolved_type, lhs_spill->offset);
            inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, rhs_reg);
            inst_list = emit_load_from_stack(inst_list, target_reg, left_expr, left_expr->resolved_type, lhs_spill->offset);
            const char *target_name = select_register_name(target_reg, left_expr, left_expr->resolved_type);
            const char *rhs_name = select_register_name(rhs_reg, right_expr, right_expr->resolved_type);
            inst_list = gencode_op(expr, target_name, rhs_name, inst_list, ctx);
            free_reg(get_reg_stack(), rhs_reg);
        }
        return inst_list;
    }

    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

    inst_list = gencode_leaf_var(right_expr, inst_list, ctx, name_buf, sizeof(name_buf));

    const char *target_name = select_register_name(target_reg, left_expr, left_expr != NULL ? left_expr->resolved_type : expr->resolved_type);
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
    /* Note: inst_list can be NULL at the start of code generation */
    assert(ctx != NULL);
    assert(target_reg != NULL);

    Register_t *temp_reg;
    struct Expression *right_expr = node->right_expr->expr;
    struct Expression *left_expr = node->left_expr->expr;

    assert(left_expr != NULL);
    assert(right_expr != NULL);

    temp_reg = get_free_reg(get_reg_stack(), &inst_list);
    if(temp_reg == NULL)
    {
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);

        StackNode_t *spill_loc = add_l_t("spill");
        /* Use right operand's type for spilling, not the binary expr's result type */
        inst_list = emit_store_to_stack(inst_list, target_reg,
            right_expr, right_expr->resolved_type, spill_loc->offset);

        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

        char spill_mem[30];
        snprintf(spill_mem, 30, "-%d(%%rbp)", spill_loc->offset);
        const char *target_name = select_register_name(target_reg, left_expr, left_expr->resolved_type);
        inst_list = gencode_op(node->expr, target_name, spill_mem, inst_list, ctx);
    }
    else
    {
        StackNode_t *rhs_spill = add_l_t("case2_rhs");
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, temp_reg);
        /* Use right operand's type for spilling, not the binary expr's result type */
        inst_list = emit_store_to_stack(inst_list, temp_reg, right_expr, right_expr->resolved_type, rhs_spill->offset);
        inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
        inst_list = emit_load_from_stack(inst_list, temp_reg, right_expr, right_expr->resolved_type, rhs_spill->offset);
        const char *target_name = select_register_name(target_reg, left_expr, left_expr->resolved_type);
        const char *temp_name = select_register_name(temp_reg, right_expr, right_expr->resolved_type);
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
    /* Note: inst_list can be NULL at the start of code generation */
    assert(ctx != NULL);
    assert(target_reg != NULL);

    Register_t *temp_reg;
    struct Expression *left_expr = node->left_expr->expr;
    struct Expression *right_expr = node->right_expr->expr;

    assert(left_expr != NULL);
    assert(right_expr != NULL);

    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
    temp_reg = get_free_reg(get_reg_stack(), &inst_list);

    if(temp_reg == NULL)
    {
        StackNode_t *spill_loc = add_l_t("spill");
        /* Use left operand's type for spilling, not the binary expr's result type */
        inst_list = emit_store_to_stack(inst_list, target_reg,
            left_expr, left_expr->resolved_type, spill_loc->offset);

        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);

        char spill_mem[30];
        snprintf(spill_mem, 30, "-%d(%%rbp)", spill_loc->offset);
        const char *target_name = select_register_name(target_reg, right_expr, right_expr->resolved_type);
        inst_list = gencode_op(node->expr, target_name, spill_mem, inst_list, ctx);
    }
    else
    {
        StackNode_t *lhs_spill = add_l_t("case3_lhs");
        /* Use left operand's type for spilling, not the binary expr's result type */
        inst_list = emit_store_to_stack(inst_list, target_reg, left_expr, left_expr->resolved_type, lhs_spill->offset);
        inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, temp_reg);
        inst_list = emit_load_from_stack(inst_list, target_reg, left_expr, left_expr->resolved_type, lhs_spill->offset);
        const char *target_name = select_register_name(target_reg, left_expr, left_expr->resolved_type);
        const char *temp_name = select_register_name(temp_reg, right_expr, right_expr->resolved_type);
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

                /* First check if this is a constant - constants don't need non-local access */
                HashNode_t *node = NULL;
                int found = (ctx != NULL && ctx->symtab != NULL &&
                    FindIdent(&node, ctx->symtab, expr->expr_data.id) >= 0 &&
                    node != NULL);

                if (found && node->hash_type == HASHTYPE_CONST)
                {
                    /* Check if this is a procedure address constant */
                    if (node->type != NULL && node->type->kind == TYPE_KIND_PROCEDURE &&
                        node->const_string_value != NULL)
                    {
                        /* Procedure address constant - load address of the referenced procedure.
                         * The const_string_value holds the original procedure name.
                         * We need to mangle it and use RIP-relative addressing so the 
                         * generated code will use leaq for proper label loading. */
                        char mangled_name[256];
                        snprintf(mangled_name, sizeof(mangled_name), "%s_void", node->const_string_value);
                        /* Use RIP-relative format for label - this causes leaq to be generated */
                        snprintf(buffer, buf_len, "%s(%%rip)", mangled_name);
                    }
                    /* Check if this is a real constant */
                    else if (node->type != NULL && kgpc_type_equals_tag(node->type, REAL_TYPE))
                    {
                        /* Real constant - encode as bit pattern using union for safe type punning */
                        union {
                            double d;
                            int64_t i;
                        } converter;
                        converter.d = node->const_real_value;
                        snprintf(buffer, buf_len, "$%lld", (long long)converter.i);
                    }
                    /* Check if this is a set constant that fits in 8 bytes */
                    else if (node->const_set_value != NULL && node->const_set_size > 0 &&
                             node->const_set_size <= (int)sizeof(long long))
                    {
                        /* Small set constant - use const_int_value */
                        snprintf(buffer, buf_len, "$%lld", node->const_int_value);
                    }
                    /* Check if this is a character set (32 bytes) - needs special handling */
                    else if (node->const_set_value != NULL && node->const_set_size > (int)sizeof(long long))
                    {
                        /* Large set constant (e.g., character set of 32 bytes).
                         * This cannot be represented as an immediate value.
                         * We need to emit the set in rodata and return its address. */
                        inst_list = codegen_emit_const_set_rodata(node, inst_list, ctx);
                        if (node->const_set_label != NULL)
                        {
                            snprintf(buffer, buf_len, "%s(%%rip)", node->const_set_label);
                        }
                        else
                        {
                            /* Error: failed to emit set constant to rodata.
                             * This indicates a bug in codegen_emit_const_set_rodata. */
                            codegen_report_error(ctx,
                                "ERROR: Failed to emit large set constant '%s' to rodata.",
                                expr->expr_data.id ? expr->expr_data.id : "(unknown)");
                            snprintf(buffer, buf_len, "$0");
                        }
                    }
                    else
                    {
                        /* Integer constant */
                        snprintf(buffer, buf_len, "$%lld", node->const_int_value);
                    }
                }
                else if (found && node->hash_type == HASHTYPE_TYPE &&
                         node->type != NULL && node->type->kind == TYPE_KIND_POINTER &&
                         node->type->info.points_to != NULL &&
                         node->type->info.points_to->kind == TYPE_KIND_RECORD &&
                         node->type->info.points_to->info.record_info != NULL &&
                         record_type_is_class(node->type->info.points_to->info.record_info))
                {
                     /* Class type used as value -> Address of VMT
                      * Use RIP-relative addressing for cross-platform compatibility
                      * (Windows x64 doesn't support $symbol immediates) */
                     snprintf(buffer, buf_len, "%s_VMT(%%rip)", expr->expr_data.id);
                }
                else if(stack_node != NULL)
                {
                    if (stack_node->is_static)
                    {
                        const char *label = (stack_node->static_label != NULL) ?
                            stack_node->static_label : stack_node->label;
                        snprintf(buffer, buf_len, "%s(%%rip)", label);
                    }
                    else if (scope_depth == 0)
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
                    /* Check if this is a VMT label (global symbol ending with "_VMT") */
                    const char *var_name = expr != NULL ? expr->expr_data.id : "<unknown>";
                    size_t name_len = var_name != NULL ? strlen(var_name) : 0;
                    int is_vmt_label = (name_len > 4 && strcmp(var_name + name_len - 4, "_VMT") == 0);
                    
                    /* Check if this is a builtin file variable (stdin, stdout, stderr, Input, Output) */
                    int is_builtin_file = 0;
                    const char *global_ptr_name = NULL;
                    if (var_name != NULL)
                    {
                        if (strcasecmp(var_name, "stdin") == 0)
                        {
                            is_builtin_file = 1;
                            global_ptr_name = "stdin_ptr";
                        }
                        else if (strcasecmp(var_name, "stdout") == 0)
                        {
                            is_builtin_file = 1;
                            global_ptr_name = "stdout_ptr";
                        }
                        else if (strcasecmp(var_name, "stderr") == 0)
                        {
                            is_builtin_file = 1;
                            global_ptr_name = "stderr_ptr";
                        }
                        else if (strcasecmp(var_name, "Input") == 0)
                        {
                            is_builtin_file = 1;
                            global_ptr_name = "Input_ptr";
                        }
                        else if (strcasecmp(var_name, "Output") == 0)
                        {
                            is_builtin_file = 1;
                            global_ptr_name = "Output_ptr";
                        }
                    }
                    
                    if (is_vmt_label)
                    {
                        /* VMT is a global label - use RIP-relative addressing */
                        snprintf(buffer, buf_len, "%s(%%rip)", var_name);
                    }
                    else if (is_builtin_file)
                    {
                        /* Builtin file variable - load from global runtime pointer */
                        snprintf(buffer, buf_len, "%s(%%rip)", global_ptr_name);
                    }
                    else
                    {
                        fprintf(stderr,
                            "ERROR: Non-local codegen support disabled while accessing %s.\n",
                            var_name != NULL ? var_name : "<unknown>");
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
            /* Use union for safe type punning */
            union {
                double d;
                int64_t i;
            } converter;
            converter.d = expr->expr_data.r_num;
            snprintf(buffer, buf_len, "$%lld", (long long)converter.i);
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
            if (expr->resolved_type == REAL_TYPE || type == SLASH)
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
                    inst_list = gencode_real_binary_op(ctx,
                        expr->expr_data.mulop_data.left_term, left,
                        expr->expr_data.mulop_data.right_factor, right,
                        left, inst_list, sse_op);
                break;
            }
            /* Handle pointer-pointer subtraction: result is element difference */
            if (expr->is_pointer_diff && type == MINUS)
            {
                struct Expression *left_expr = expr->expr_data.addop_data.left_expr;
                
                /* Convert operands to 64-bit for pointer operations */
                char left64_buf[16], right64_buf[16];
                const char *left64 = reg32_to_reg64(left, left64_buf, sizeof(left64_buf));
                const char *right64 = reg32_to_reg64(right, right64_buf, sizeof(right64_buf));
                
                /* Get element size from the pointer type.
                 * For typed pointers like PByte (^Byte), use pointer_subtype_id to get correct size.
                 * If pointer_subtype_id is set, use UNKNOWN_TYPE so lookup by name works properly.
                 */
                long long element_size = 1;
                if (left_expr != NULL)
                {
                    int lookup_type = left_expr->pointer_subtype;
                    const char *lookup_id = left_expr->pointer_subtype_id;
                    
                    /* If we have a type name, prioritize it over the type tag */
                    if (lookup_id != NULL)
                        lookup_type = UNKNOWN_TYPE;
                    
                    if (lookup_type != UNKNOWN_TYPE || lookup_id != NULL)
                    {
                        if (codegen_sizeof_type_reference(ctx, lookup_type, lookup_id,
                                left_expr->record_type, &element_size) != 0 || element_size <= 0)
                        {
                            element_size = 1; /* Default to byte size */
                        }
                    }
                }
                /* Also check the expression's own type info (set during semcheck) */
                else if (expr->pointer_subtype != UNKNOWN_TYPE || expr->pointer_subtype_id != NULL)
                {
                    int lookup_type = expr->pointer_subtype;
                    const char *lookup_id = expr->pointer_subtype_id;
                    
                    /* If we have a type name, prioritize it over the type tag */
                    if (lookup_id != NULL)
                        lookup_type = UNKNOWN_TYPE;
                    
                    if (codegen_sizeof_type_reference(ctx, lookup_type, lookup_id,
                            expr->record_type, &element_size) != 0 || element_size <= 0)
                    {
                        element_size = 1;
                    }
                }
                
                /* Subtract pointers: left64 = left64 - right64 (in bytes) */
                snprintf(buffer, sizeof(buffer), "\tsubq\t%s, %s\n", right64, left64);
                inst_list = add_inst(inst_list, buffer);
                
                /* Divide by element size to get element count */
                if (element_size > 1)
                {
                    /* Use signed division (idiv) to handle negative differences */
                    /* Result in RAX, remainder in RDX, we need to set up dividend in RDX:RAX */
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rax\n", left64);
                    inst_list = add_inst(inst_list, buffer);
                    /* Sign-extend RAX into RDX:RAX */
                    snprintf(buffer, sizeof(buffer), "\tcqto\n");
                    inst_list = add_inst(inst_list, buffer);
                    /* Load divisor */
                    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %%r11\n", element_size);
                    inst_list = add_inst(inst_list, buffer);
                    /* Divide */
                    snprintf(buffer, sizeof(buffer), "\tidivq\t%%r11\n");
                    inst_list = add_inst(inst_list, buffer);
                    /* Move result back to left64 */
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", left64);
                    inst_list = add_inst(inst_list, buffer);
                }
                break;
            }
            /* Handle pointer arithmetic: pointer + integer or integer + pointer */
            if (expr->resolved_type == POINTER_TYPE && (type == PLUS || type == MINUS))
            {
                struct Expression *left_expr = expr->expr_data.addop_data.left_expr;
                struct Expression *right_expr = expr->expr_data.addop_data.right_term;
                
                int left_is_pointer = (left_expr != NULL && left_expr->resolved_type == POINTER_TYPE);
                int right_is_pointer = (right_expr != NULL && right_expr->resolved_type == POINTER_TYPE);
                
                /* Determine which operand is the pointer and which is the integer */
                const char *ptr_reg = left_is_pointer ? left : right;
                const char *int_reg = left_is_pointer ? right : left;
                struct Expression *ptr_expr = left_is_pointer ? left_expr : right_expr;
                
                /* Get element size */
                long long element_size = 1;
                if (ptr_expr != NULL && ptr_expr->pointer_subtype != UNKNOWN_TYPE)
                {
                    int dummy_type = ptr_expr->pointer_subtype;
                    if (codegen_sizeof_type_reference(ctx, dummy_type, ptr_expr->pointer_subtype_id,
                            ptr_expr->record_type, &element_size) != 0 || element_size <= 0)
                    {
                        element_size = 8; /* Default to pointer size */
                    }
                }
                
                /* Scale the integer offset by element size */
                if (element_size != 1)
                {
                    /* Check if int_reg is an immediate value */
                    if (int_reg[0] == '$')
                    {
                        /* It's an immediate - compute the scaled value directly */
                        long long int_val = strtoll(int_reg + 1, NULL, 0);
                        long long scaled_val = int_val * element_size;
                        snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %%r11\n", scaled_val);
                        inst_list = add_inst(inst_list, buffer);
                        int_reg = "%r11";
                    }
                    else
                    {
                        /* It's a register - multiply in place */
                        snprintf(buffer, sizeof(buffer), "\timulq\t$%lld, %s\n", element_size, int_reg);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }
                
                /* Now add or subtract */
                if (type == PLUS)
                {
                    /* For integer + pointer, we need to put result in correct register */
                    if (right_is_pointer && left_is_pointer == 0)
                    {
                        /* int + ptr: add int to ptr, result goes to left (the int register initially) */
                        snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n", ptr_reg, left);
                    }
                    else
                    {
                        /* ptr + int: add int to ptr */
                        snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n", int_reg, left);
                    }
                }
                else /* MINUS */
                {
                    /* ptr - int: subtract int from ptr */
                    snprintf(buffer, sizeof(buffer), "\tsubq\t%s, %s\n", int_reg, left);
                }
                inst_list = add_inst(inst_list, buffer);
                break;
            }
            {
                const int use_qword_op = codegen_type_uses_qword(expr->resolved_type);
                const char arith_suffix = use_qword_op ? 'q' : 'l';
                char left32_buf[16];
                char right32_buf[16];
                const char *left_op = left;
                const char *right_op = right;
                if (arith_suffix == 'l')
                {
                    left_op = reg_to_reg32(left, left32_buf, sizeof(left32_buf));
                    if (right != NULL && right[0] == '$')
                        right_op = right;
                    else
                        right_op = reg_to_reg32(right, right32_buf, sizeof(right32_buf));
                }
                if (type == OR)
                {
                    snprintf(buffer, sizeof(buffer), "\tor%c\t%s, %s\n", arith_suffix, right_op, left_op);
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
                        snprintf(buffer, sizeof(buffer), "\tinc%c\t%s\n", arith_suffix, left_op);
                    else
                        snprintf(buffer, sizeof(buffer), "\tadd%c\t%s, %s\n", arith_suffix, right_op, left_op);
                    inst_list = add_inst(inst_list, buffer);
                    break;
                }
                case MINUS:
                    snprintf(buffer, sizeof(buffer), "\tsub%c\t%s, %s\n", arith_suffix, right_op, left_op);
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
            if (expr->resolved_type == REAL_TYPE || type == SLASH)
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
                    inst_list = gencode_real_binary_op(ctx,
                        expr->expr_data.mulop_data.left_term, left,
                        expr->expr_data.mulop_data.right_factor, right,
                        left, inst_list, sse_op);
                break;
            }
            {
                const int use_qword_op = codegen_type_uses_qword(expr->resolved_type);
                const char arith_suffix = use_qword_op ? 'q' : 'l';
                const char *op_left = left;
                const char *op_right = right;
                char left64_buf[16], right64_buf[16];

                if (use_qword_op)
                {
                    const char *left64 = reg32_to_reg64(left, left64_buf, sizeof(left64_buf));
                    const char *right64 = reg32_to_reg64(right, right64_buf, sizeof(right64_buf));

                    if (operand_is_32bit_register(left) && left64 != NULL)
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", left, left64);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    if (operand_is_32bit_register(right) && right64 != NULL)
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", right, right64);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    if (left64 != NULL)
                        op_left = left64;
                    if (right64 != NULL)
                        op_right = right64;
                }
            if(type == STAR)
            {
                snprintf(buffer, sizeof(buffer), "\timul%c\t%s, %s\n", arith_suffix, op_right, op_left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == AND)
            {
                snprintf(buffer, sizeof(buffer), "\tand%c\t%s, %s\n", arith_suffix, op_right, op_left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == MOD)
            {
                if (use_qword_op)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rax\n", op_left);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcqo\n");

                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r10\n", op_right);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tidivq\t%r10\n");

                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rdx, %s\n", op_left);
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
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rax\n", op_left);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcqo\n");

                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r10\n", op_right);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tidivq\t%r10\n");

                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", op_left);
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
                snprintf(buffer, sizeof(buffer), "\txor%c\t%s, %s\n", arith_suffix, op_right, op_left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == SHL)
            {
                char right32[16];
                const char *count = use_qword_op ? reg64_to_reg32(op_right, right32, sizeof(right32)) : op_right;
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", count);
                inst_list = add_inst(inst_list, buffer);
                /* Use SAL to match FPC's emitted mnemonics for left shifts */
                snprintf(buffer, sizeof(buffer), "\tsal%c\t%%cl, %s\n", arith_suffix, op_left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == SHR)
            {
                char right32[16];
                const char *count = use_qword_op ? reg64_to_reg32(op_right, right32, sizeof(right32)) : op_right;
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", count);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tsar%c\t%%cl, %s\n", arith_suffix, op_left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == ROL)
            {
                char right32[16];
                const char *count = use_qword_op ? reg64_to_reg32(op_right, right32, sizeof(right32)) : op_right;
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", count);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\trol%c\t%%cl, %s\n", arith_suffix, op_left);
                inst_list = add_inst(inst_list, buffer);
            }
            else if(type == ROR)
            {
                char right32[16];
                const char *count = use_qword_op ? reg64_to_reg32(op_right, right32, sizeof(right32)) : op_right;
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", count);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tror%c\t%%cl, %s\n", arith_suffix, op_left);
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
                    /* When loading an immediate set value, make sure not to clobber the left operand.
                     * Use %r11d if left is in %r10d, otherwise use %r10d. */
                    const char *temp_reg = "%r10d";
                    /* Check if left operand is in r10 (any size: r10, r10d, r10b, r10w) */
                    if (left32 != NULL && strstr(left32, "%r10") == left32)
                        temp_reg = "%r11d";
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", right, temp_reg);
                    inst_list = add_inst(inst_list, buffer);
                    bit_base = temp_reg;
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
                Register_t *imm_reg = NULL;
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

                if (use_qword && cmp_right != NULL && cmp_right[0] == '$')
                {
                    char *endptr = NULL;
                    long long imm_value = strtoll(cmp_right + 1, &endptr, 0);
                    if (endptr != NULL && *endptr == '\0' &&
                        (imm_value > 2147483647LL || imm_value < -2147483648LL))
                    {
                        imm_reg = get_free_reg(get_reg_stack(), &inst_list);
                        if (imm_reg == NULL)
                        {
                            codegen_report_error(ctx,
                                "ERROR: Unable to allocate temporary for 64-bit immediate comparison.");
                            break;
                        }
                        snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n",
                            imm_value, imm_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        cmp_right = imm_reg->bit_64;
                    }
                }

                snprintf(buffer, sizeof(buffer), "\tcmp%c\t%s, %s\n", cmp_suffix, cmp_right, cmp_left);
                inst_list = add_inst(inst_list, buffer);
                if (imm_reg != NULL)
                    free_reg(get_reg_stack(), imm_reg);

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
