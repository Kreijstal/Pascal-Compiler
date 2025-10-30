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
#include "../../Parser/ParseTree/type_tags.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"


int codegen_type_uses_qword(int type_tag)
{
    return (type_tag == LONGINT_TYPE || type_tag == REAL_TYPE ||
        type_tag == POINTER_TYPE || type_tag == STRING_TYPE);
}

static inline const char *register_name_for_type(const Register_t *reg, int type_tag)
{
    if (reg == NULL)
        return NULL;
    return codegen_type_uses_qword(type_tag) ? reg->bit_64 : reg->bit_32;
}

static inline const char *register_name_for_expr(const Register_t *reg, const struct Expression *expr)
{
    if (expr == NULL)
        return register_name_for_type(reg, UNKNOWN_TYPE);
    return register_name_for_type(reg, expr->resolved_type);
}

static inline int expression_uses_qword(const struct Expression *expr)
{
    return expr != NULL && codegen_type_uses_qword(expr->resolved_type);
}

ListNode_t *codegen_pointer_deref_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *codegen_addressof_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *codegen_record_access(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *codegen_record_field_address(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg);


/* Code generation for expressions */
static const char *describe_expression_kind(const struct Expression *expr)
{
    if (expr == NULL)
        return "unknown";

    switch (expr->type)
    {
        case EXPR_VAR_ID:
            return "variable reference";
        case EXPR_ARRAY_ACCESS:
            return "array access";
        case EXPR_FUNCTION_CALL:
            return "function call";
        case EXPR_ADDOP:
            return "additive expression";
        case EXPR_MULOP:
            return "multiplicative expression";
        case EXPR_SIGN_TERM:
            return "signed term";
        case EXPR_RELOP:
            return "relational expression";
        case EXPR_INUM:
            return "integer literal";
        case EXPR_RNUM:
            return "real literal";
        default:
            return "expression";
    }
}

static Register_t *codegen_try_get_reg(ListNode_t **inst_list, CodeGenContext *ctx, const char *usage)
{
    Register_t *reg = get_free_reg(get_reg_stack(), inst_list);
    if (reg == NULL)
        codegen_report_error(ctx, "ERROR: Unable to allocate register for %s.", usage);
    return reg;
}

static ListNode_t *codegen_expr_tree_value(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    expr_node_t *expr_tree = build_expr_tree(expr);
    Register_t *target_reg = codegen_try_get_reg(&inst_list, ctx, describe_expression_kind(expr));
    if (target_reg == NULL)
    {
        free_expr_tree(expr_tree);
        if (out_reg != NULL)
            *out_reg = NULL;
        return inst_list;
    }

    inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
    free_expr_tree(expr_tree);

    if (out_reg != NULL)
        *out_reg = target_reg;
    else
        free_reg(get_reg_stack(), target_reg);
    return inst_list;
}

static ListNode_t *codegen_expr_via_tree(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    return codegen_expr_tree_value(expr, inst_list, ctx, NULL);
}

ListNode_t *codegen_sign_extend32_to64(ListNode_t *inst_list, const char *src_reg32, const char *dst_reg64)
{
    assert(src_reg32 != NULL);
    assert(dst_reg64 != NULL);

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", src_reg32, dst_reg64);
    return add_inst(inst_list, buffer);
}

ListNode_t *codegen_pointer_deref_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg)
{
    if (expr == NULL || ctx == NULL || target_reg == NULL)
        return inst_list;

    struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr == NULL)
    {
        codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
        return inst_list;
    }

    expr_node_t *pointer_tree = build_expr_tree(pointer_expr);
    Register_t *addr_reg = codegen_try_get_reg(&inst_list, ctx, "pointer dereference");
    if (addr_reg == NULL)
    {
        free_expr_tree(pointer_tree);
        return inst_list;
    }

    inst_list = gencode_expr_tree(pointer_tree, inst_list, ctx, addr_reg);
    free_expr_tree(pointer_tree);

    char buffer[64];
    if (codegen_type_uses_qword(expr->resolved_type))
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_64);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}

ListNode_t *codegen_addressof_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg)
{
    if (expr == NULL || ctx == NULL || target_reg == NULL)
        return inst_list;

    struct Expression *inner = expr->expr_data.addr_data.expr;
    if (inner == NULL)
    {
        codegen_report_error(ctx, "ERROR: Address-of operator missing operand.");
        return inst_list;
    }

    char buffer[64];
    if (inner->type == EXPR_VAR_ID)
    {
        StackNode_t *var_node = find_label(inner->expr_data.id);
        if (var_node != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", var_node->offset, target_reg->bit_64);
            return add_inst(inst_list, buffer);
        }
        else if (nonlocal_flag() == 1)
        {
            int offset = 0;
            inst_list = codegen_get_nonlocal(inst_list, inner->expr_data.id, &offset);
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%s), %s\n", offset, current_non_local_reg64(), target_reg->bit_64);
            return add_inst(inst_list, buffer);
        }

        codegen_report_error(ctx,
            "ERROR: Address-of non-local variables is unsupported without -non-local flag.");
        return inst_list;
    }
    else if (inner->type == EXPR_ARRAY_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_array_element_address(inner, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    codegen_report_error(ctx, "ERROR: Unsupported operand for address-of operator.");
    return inst_list;
}

ListNode_t *codegen_record_field_address(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || ctx == NULL || out_reg == NULL)
        return inst_list;

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    if (record_expr == NULL)
        return inst_list;

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(record_expr, inst_list, ctx, &addr_reg);
    if (addr_reg == NULL)
        return inst_list;

    long long offset = expr->expr_data.record_access_data.field_offset;
    if (offset != 0)
    {
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\taddq\t$%lld, %s\n", offset, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    *out_reg = addr_reg;
    return inst_list;
}

ListNode_t *codegen_record_access(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg)
{
    if (expr == NULL || ctx == NULL || target_reg == NULL)
        return inst_list;

    if (expr->resolved_type == RECORD_TYPE)
    {
        codegen_report_error(ctx, "ERROR: Record-valued expressions are unsupported in this context.");
        return inst_list;
    }

    Register_t *addr_reg = NULL;
    inst_list = codegen_record_field_address(expr, inst_list, ctx, &addr_reg);
    if (addr_reg == NULL)
        return inst_list;

    char buffer[64];
    if (codegen_type_uses_qword(expr->resolved_type))
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_64);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}

static ListNode_t *codegen_set_emit_single(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *dest_reg, Register_t *value_reg)
{
    if (dest_reg == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    char skip_label[18];
    gen_label(skip_label, sizeof(skip_label), ctx);

    snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", skip_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tcmpl\t$31, %s\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", skip_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tshll\t%%cl, %s\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\torl\t%s, %s\n", value_reg->bit_32, dest_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", skip_label);
    inst_list = add_inst(inst_list, buffer);
    return inst_list;
}

static ListNode_t *codegen_set_emit_range(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *dest_reg, Register_t *start_reg, Register_t *end_reg)
{
    if (dest_reg == NULL || start_reg == NULL || end_reg == NULL)
        return inst_list;

    Register_t *temp_reg = codegen_try_get_reg(&inst_list, ctx, "set range temp");
    if (temp_reg == NULL)
        return inst_list;

    char order_label[18];
    char loop_label[18];
    char done_label[18];
    char start_floor_label[18];
    char end_cap_label[18];
    gen_label(order_label, sizeof(order_label), ctx);
    gen_label(loop_label, sizeof(loop_label), ctx);
    gen_label(done_label, sizeof(done_label), ctx);
    gen_label(start_floor_label, sizeof(start_floor_label), ctx);
    gen_label(end_cap_label, sizeof(end_cap_label), ctx);

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, %s\n", end_reg->bit_32, start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", order_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", start_reg->bit_32, temp_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", end_reg->bit_32, start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", temp_reg->bit_32, end_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", order_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", end_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tcmpl\t$31, %s\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjge\t%s\n", start_floor_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t$0, %s\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", start_floor_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tcmpl\t$31, %s\n", end_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", end_cap_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t$31, %s\n", end_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", end_cap_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "%s:\n", loop_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, %s\n", end_reg->bit_32, start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", temp_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tshll\t%%cl, %s\n", temp_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\torl\t%s, %s\n", temp_reg->bit_32, dest_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, %s\n", end_reg->bit_32, start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tje\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tincl\t%s\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", loop_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", done_label);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), temp_reg);
    return inst_list;
}

static ListNode_t *codegen_set_literal(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL)
        return inst_list;

    if (expr->expr_data.set_data.is_constant)
    {
        Register_t *dest_reg = codegen_try_get_reg(&inst_list, ctx, "set literal");
        if (dest_reg == NULL)
        {
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }

        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%u, %s\n", expr->expr_data.set_data.bitmask,
            dest_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        if (out_reg != NULL)
            *out_reg = dest_reg;
        else
            free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    Register_t *dest_reg = codegen_try_get_reg(&inst_list, ctx, "set literal");
    if (dest_reg == NULL)
    {
        if (out_reg != NULL)
            *out_reg = NULL;
        return inst_list;
    }

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovl\t$0, %s\n", dest_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    for (ListNode_t *cur = expr->expr_data.set_data.elements; cur != NULL; cur = cur->next)
    {
        struct SetElement *element = (struct SetElement *)cur->cur;
        if (element == NULL)
            continue;

        Register_t *lower_reg = NULL;
        inst_list = codegen_expr_tree_value(element->lower, inst_list, ctx, &lower_reg);
        if (codegen_had_error(ctx) || lower_reg == NULL)
        {
            if (lower_reg != NULL)
                free_reg(get_reg_stack(), lower_reg);
            free_reg(get_reg_stack(), dest_reg);
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }

        Register_t *upper_reg = NULL;
        if (element->upper != NULL)
        {
            inst_list = codegen_expr_tree_value(element->upper, inst_list, ctx, &upper_reg);
            if (codegen_had_error(ctx) || upper_reg == NULL)
            {
                if (upper_reg != NULL)
                    free_reg(get_reg_stack(), upper_reg);
                free_reg(get_reg_stack(), lower_reg);
                free_reg(get_reg_stack(), dest_reg);
                if (out_reg != NULL)
                    *out_reg = NULL;
                return inst_list;
            }
        }

        if (element->upper == NULL)
            inst_list = codegen_set_emit_single(inst_list, ctx, dest_reg, lower_reg);
        else
            inst_list = codegen_set_emit_range(inst_list, ctx, dest_reg, lower_reg, upper_reg);

        if (codegen_had_error(ctx))
        {
            if (upper_reg != NULL)
                free_reg(get_reg_stack(), upper_reg);
            free_reg(get_reg_stack(), lower_reg);
            free_reg(get_reg_stack(), dest_reg);
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }

        if (upper_reg != NULL)
            free_reg(get_reg_stack(), upper_reg);
        free_reg(get_reg_stack(), lower_reg);
    }

    if (out_reg != NULL)
        *out_reg = dest_reg;
    else
        free_reg(get_reg_stack(), dest_reg);
    return inst_list;
}

static ListNode_t *codegen_set_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL)
    {
        if (out_reg != NULL)
            *out_reg = NULL;
        return inst_list;
    }

    if (expr->type == EXPR_SET)
        return codegen_set_literal(expr, inst_list, ctx, out_reg);

    return codegen_expr_tree_value(expr, inst_list, ctx, out_reg);
}

ListNode_t *codegen_expr(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(expr != NULL);
    assert(ctx != NULL);
    CODEGEN_DEBUG("DEBUG: Generating code for expression type %d\n", expr->type);

    if (expr->resolved_type == SET_TYPE)
    {
        Register_t *set_reg = NULL;
        inst_list = codegen_set_expr(expr, inst_list, ctx, &set_reg);
        if (codegen_had_error(ctx))
            return inst_list;
        if (set_reg != NULL)
            free_reg(get_reg_stack(), set_reg);
        return inst_list;
    }

    switch(expr->type) {
        case EXPR_VAR_ID:
            CODEGEN_DEBUG("DEBUG: Processing variable ID expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_RECORD_ACCESS:
            CODEGEN_DEBUG("DEBUG: Processing record access expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_ARRAY_ACCESS:
            CODEGEN_DEBUG("DEBUG: Processing array access expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_MULOP:
            CODEGEN_DEBUG("DEBUG: Processing mulop expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_INUM:
            CODEGEN_DEBUG("DEBUG: Processing integer constant expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_RNUM:
            CODEGEN_DEBUG("DEBUG: Processing real constant expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_BOOL:
            CODEGEN_DEBUG("DEBUG: Processing boolean constant expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_SET:
            CODEGEN_DEBUG("DEBUG: Processing set literal expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_STRING:
            CODEGEN_DEBUG("DEBUG: Processing string literal expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_POINTER_DEREF:
            CODEGEN_DEBUG("DEBUG: Processing pointer dereference expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_ADDR:
            CODEGEN_DEBUG("DEBUG: Processing address-of expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_RELOP:
            CODEGEN_DEBUG("DEBUG: Processing relational operator expression\n");
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return codegen_simple_relop(expr, inst_list, ctx, NULL);
        case EXPR_ADDOP:
            CODEGEN_DEBUG("DEBUG: Processing addop expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_SIGN_TERM:
            CODEGEN_DEBUG("DEBUG: Processing sign term expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_FUNCTION_CALL:
            CODEGEN_DEBUG("DEBUG: Processing function call expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_TYPECAST:
            CODEGEN_DEBUG("DEBUG: Processing typecast expression\n");
            if (expr->expr_data.typecast_data.expr != NULL)
                inst_list = codegen_expr(expr->expr_data.typecast_data.expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        default:
            assert(0 && "Unsupported expression type");
            break;
    }
}

ListNode_t *codegen_expr_with_result(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(out_reg != NULL);
    inst_list = codegen_expr_tree_value(expr, inst_list, ctx, out_reg);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}


ListNode_t *codegen_array_element_address(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg)
{
    assert(expr != NULL);
    assert(expr->type == EXPR_ARRAY_ACCESS);
    assert(ctx != NULL);
    assert(out_reg != NULL);

    const char *array_id = expr->expr_data.array_access_data.id;
    if (array_id == NULL)
    {
        codegen_report_error(ctx, "ERROR: Missing array identifier in access expression.");
        return inst_list;
    }

    inst_list = codegen_expr(expr->expr_data.array_access_data.array_expr, inst_list, ctx);
    if (codegen_had_error(ctx))
        return inst_list;
    Register_t *index_reg = codegen_try_get_reg(&inst_list, ctx, "array index");
    if (index_reg == NULL)
        return inst_list;

    StackNode_t *array_node = find_label((char *)array_id);
    if (array_node == NULL || array_node->is_array == 0)
    {
        codegen_report_error(ctx,
            "ERROR: Array %s not found on stack (non-local arrays unsupported).", array_id);
        free_reg(get_reg_stack(), index_reg);
        return inst_list;
    }

    int element_size = array_node->element_size;
    if (element_size <= 0)
        element_size = DOUBLEWORD;

    int lower_bound = array_node->array_lower_bound;
    char buffer[128];

    if (array_node->is_dynamic)
    {
        Register_t *base_reg = codegen_try_get_reg(&inst_list, ctx, "dynamic array base");
        if (base_reg == NULL)
        {
            free_reg(get_reg_stack(), index_reg);
            return inst_list;
        }

        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", array_node->offset, base_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        if (lower_bound > 0)
        {
            snprintf(buffer, sizeof(buffer), "\tsubl\t$%d, %s\n", lower_bound, index_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (lower_bound < 0)
        {
            snprintf(buffer, sizeof(buffer), "\taddl\t$%d, %s\n", -lower_bound, index_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }

        inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32, index_reg->bit_64);

        int scaled_sizes[] = {1, 2, 4, 8};
        int can_scale = 0;
        for (size_t i = 0; i < sizeof(scaled_sizes) / sizeof(scaled_sizes[0]); ++i)
        {
            if (element_size == scaled_sizes[i])
            {
                can_scale = 1;
                break;
            }
        }

        if (can_scale)
        {
            snprintf(buffer, sizeof(buffer), "\tleaq\t(%s,%s,%d), %s\n", base_reg->bit_64, index_reg->bit_64, element_size, index_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (element_size != 1)
            {
                snprintf(buffer, sizeof(buffer), "\timulq\t$%d, %s\n", element_size, index_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }

            StackNode_t *offset_temp = find_in_temp("array_index_offset");
            if (offset_temp == NULL)
                offset_temp = add_l_t("array_index_offset");

            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", index_reg->bit_64, offset_temp->offset);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", base_reg->bit_64, index_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\taddq\t-%d(%%rbp), %s\n", offset_temp->offset, index_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }

        free_reg(get_reg_stack(), base_reg);
    }
    else
    {
        if (lower_bound > 0)
        {
            snprintf(buffer, sizeof(buffer), "\tsubl\t$%d, %s\n", lower_bound, index_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (lower_bound < 0)
        {
            snprintf(buffer, sizeof(buffer), "\taddl\t$%d, %s\n", -lower_bound, index_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }

        int scaled_sizes[] = {1, 2, 4, 8};
        int can_scale = 0;
        for (size_t i = 0; i < sizeof(scaled_sizes) / sizeof(scaled_sizes[0]); ++i)
        {
            if (element_size == scaled_sizes[i])
            {
                can_scale = 1;
                break;
            }
        }

        inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32, index_reg->bit_64);

        if (can_scale)
        {
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp,%s,%d), %s\n", array_node->offset, index_reg->bit_64, element_size, index_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (element_size != 1)
            {
                snprintf(buffer, sizeof(buffer), "\timulq\t$%d, %s\n", element_size, index_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }

            StackNode_t *offset_temp = find_in_temp("array_index_offset");
            if (offset_temp == NULL)
                offset_temp = add_l_t("array_index_offset");

            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", index_reg->bit_64, offset_temp->offset);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", array_node->offset, index_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\taddq\t-%d(%%rbp), %s\n", offset_temp->offset, index_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    *out_reg = index_reg;
    return inst_list;
}

ListNode_t *codegen_array_access(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg)
{
    assert(expr != NULL);
    assert(target_reg != NULL);

    Register_t *addr_reg = NULL;
    inst_list = codegen_array_element_address(expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    char buffer[100];
    if (codegen_type_uses_qword(expr->resolved_type))
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        if (expr->resolved_type == LONGINT_TYPE)
            inst_list = codegen_sign_extend32_to64(inst_list, target_reg->bit_32, target_reg->bit_64);
    }

    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}


/* Code generation for simple relops */
ListNode_t *codegen_simple_relop(struct Expression *expr, ListNode_t *inst_list,
                                CodeGenContext *ctx, int *relop_type)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(expr != NULL);
    assert(expr->type == EXPR_RELOP);
    assert(inst_list != NULL);
    assert(ctx != NULL);

    CODEGEN_DEBUG("DEBUG: Generating simple relop\n");

    struct Expression *left_expr = expr->expr_data.relop_data.left;
    struct Expression *right_expr = expr->expr_data.relop_data.right;
    int relop_kind = expr->expr_data.relop_data.type;

    if (relop_type != NULL)
        *relop_type = relop_kind;

    inst_list = codegen_expr(expr->expr_data.relop_data.left, inst_list, ctx);
    Register_t *left_reg = get_free_reg(get_reg_stack(), &inst_list);
    inst_list = codegen_expr(expr->expr_data.relop_data.right, inst_list, ctx);
    Register_t *right_reg = front_reg_stack(get_reg_stack());

    if (left_reg == NULL || right_reg == NULL)
        return inst_list;

    char buffer[128];

    if (relop_kind == IN)
    {
        if (relop_type != NULL)
            *relop_type = NE;

        StackNode_t *set_spill = add_l_t("set_relop");
        if (set_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", right_reg->bit_32, set_spill->offset);
            inst_list = add_inst(inst_list, buffer);
        }

        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tshll\t%%cl, %s\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        if (set_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", set_spill->offset, right_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }

        snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", left_reg->bit_32, right_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), left_reg);
        return inst_list;
    }

    if (left_expr != NULL && left_expr->resolved_type == REAL_TYPE)
    {
        const char *left_name = register_name_for_type(left_reg, REAL_TYPE);
        const char *right_name = register_name_for_type(right_reg, REAL_TYPE);
        char true_label[32];
        char done_label[32];
        gen_label(true_label, sizeof(true_label), ctx);
        gen_label(done_label, sizeof(done_label), ctx);

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm1\n", left_name);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", right_name);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\txorl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        inst_list = add_inst(inst_list, "\tucomisd\t%xmm0, %xmm1\n");

        int relop_kind = expr->expr_data.relop_data.type;
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
        snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "%s:\n", done_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        if (relop_type != NULL)
            *relop_type = NE;

        free_reg(get_reg_stack(), left_reg);
        return inst_list;
    }

    int use_qword = expression_uses_qword(left_expr) || expression_uses_qword(right_expr);
    const char *left_name = use_qword
        ? register_name_for_type(left_reg, LONGINT_TYPE)
        : register_name_for_expr(left_reg, left_expr);
    const char *right_name = use_qword
        ? register_name_for_type(right_reg, LONGINT_TYPE)
        : register_name_for_expr(right_reg, right_expr);
    snprintf(buffer, sizeof(buffer), "\tcmp%c\t%s, %s\n", use_qword ? 'q' : 'l', right_name, left_name);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), left_reg);

    CODEGEN_DEBUG("DEBUG: Simple relop generated\n");
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for non-local variable access */
ListNode_t *codegen_get_nonlocal(ListNode_t *inst_list, char *var_id, int *offset)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    CODEGEN_DEBUG("DEBUG: Generating non-local access for %s\n", var_id);

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
    snprintf(buffer, 100, "\tmovq\t-8(%%rbp), %s\n", current_non_local_reg64());
    inst_list = add_inst(inst_list, buffer);

    CODEGEN_DEBUG("DEBUG: Non-local access generated\n");
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for passing arguments */
ListNode_t *codegen_pass_arguments(ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx, HashNode_t *proc_node)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    int arg_num;
    Register_t *top_reg;
    char buffer[50];
    const char *arg_reg_char;
    expr_node_t *expr_tree;

    assert(ctx != NULL);

    ListNode_t *formal_args = NULL;
    if(proc_node != NULL)
        formal_args = proc_node->args;

    typedef struct ArgInfo
    {
        Register_t *reg;
        StackNode_t *spill;
        struct Expression *expr;
    } ArgInfo;

    int total_args = 0;
    for (ListNode_t *cur = args; cur != NULL; cur = cur->next)
        ++total_args;

    ArgInfo *arg_infos = NULL;
    if (total_args > 0)
    {
        arg_infos = (ArgInfo *)calloc((size_t)total_args, sizeof(ArgInfo));
        if (arg_infos == NULL)
        {
            fprintf(stderr, "ERROR: Failed to allocate argument metadata.\n");
            exit(1);
        }
    }

    arg_num = 0;
    while(args != NULL)
    {
        CODEGEN_DEBUG("DEBUG: In codegen_pass_arguments loop, arg_num = %d\n", arg_num);
        struct Expression *arg_expr = (struct Expression *)args->cur;
        CODEGEN_DEBUG("DEBUG: arg_expr at %p, type %d\n", arg_expr, arg_expr->type);

        Tree_t *formal_arg_decl = NULL;
        if(formal_args != NULL)
            formal_arg_decl = (Tree_t *)formal_args->cur;

        if(formal_arg_decl != NULL && formal_arg_decl->tree_data.var_decl_data.is_var_param)
        {
            // Pass by reference
            if (arg_expr->type == EXPR_VAR_ID)
            {
                StackNode_t *var_node = find_label(arg_expr->expr_data.id);
                Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (addr_reg == NULL)
                {
                    fprintf(stderr, "ERROR: Unable to allocate register for by-reference argument.\n");
                    exit(1);
                }

                snprintf(buffer, 50, "\tleaq\t-%d(%%rbp), %s\n", var_node->offset, addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);

                if (arg_infos != NULL)
                {
                arg_infos[arg_num].reg = addr_reg;
                arg_infos[arg_num].spill = NULL;
                arg_infos[arg_num].expr = arg_expr;
                }
            }
            else if (arg_expr->type == EXPR_ARRAY_ACCESS)
            {
                Register_t *addr_reg = NULL;
                inst_list = codegen_array_element_address(arg_expr, inst_list, ctx, &addr_reg);
                if (arg_infos != NULL)
                {
                arg_infos[arg_num].reg = addr_reg;
                arg_infos[arg_num].spill = NULL;
                arg_infos[arg_num].expr = arg_expr;
                }
            }
            else
            {
                fprintf(stderr, "Error: unsupported expression type for var parameter\n");
                assert(0);
            }
        }
        else
        {
            // Pass by value
            expr_tree = build_expr_tree(arg_expr);
            top_reg = get_free_reg(get_reg_stack(), &inst_list);
            CODEGEN_DEBUG("DEBUG: top_reg at %p\n", top_reg);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, top_reg);
            free_expr_tree(expr_tree);

            if (arg_infos != NULL)
            {
                arg_infos[arg_num].reg = top_reg;
                arg_infos[arg_num].spill = NULL;
                arg_infos[arg_num].expr = arg_expr;
            }
        }

        args = args->next;
        if(formal_args != NULL)
            formal_args = formal_args->next;
        ++arg_num;
    }

    for (int i = arg_num - 1; i >= 0; --i)
    {
        arg_reg_char = get_arg_reg64_num(i);
        if (arg_reg_char == NULL)
        {
            fprintf(stderr, "ERROR: Could not get arg register: %d\n", i);
            exit(1);
        }

        if (arg_infos != NULL)
        {
            for (int j = 0; j < i; ++j)
            {
                if (arg_infos[j].reg != NULL &&
                    strcmp(arg_infos[j].reg->bit_64, arg_reg_char) == 0)
                {
                    StackNode_t *spill = add_l_t("arg_spill");
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        arg_infos[j].reg->bit_64, spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), arg_infos[j].reg);
                    arg_infos[j].reg = NULL;
                    arg_infos[j].spill = spill;
                }
            }
        }

        Register_t *stored_reg = arg_infos != NULL ? arg_infos[i].reg : NULL;
        struct Expression *source_expr = arg_infos != NULL ? arg_infos[i].expr : NULL;
        if (stored_reg != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", stored_reg->bit_64, arg_reg_char);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), stored_reg);
        }
        else if (arg_infos != NULL && arg_infos[i].spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                arg_infos[i].spill->offset, arg_reg_char);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            const char *proc_name = (proc_node != NULL && proc_node->id != NULL) ? proc_node->id : "(unknown)";
            fprintf(stderr,
                    "ERROR: Missing evaluated value for argument %d in call to %s (%s).\n",
                    i,
                    proc_name,
                    describe_expression_kind(source_expr));
            exit(1);
        }
    }

    free(arg_infos);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Helper for codegen_get_nonlocal */
ListNode_t * codegen_goto_prev_scope(ListNode_t *inst_list, StackScope_t *cur_scope, char *base)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    char buffer[50];

    assert(inst_list != NULL);
    assert(cur_scope != NULL);
    assert(base != NULL);

    snprintf(buffer, 50, "\tmovq\t(%s), %s\n", base, current_non_local_reg64());
    inst_list = add_inst(inst_list, buffer);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}
