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
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../../identifier_utils.h"

#define CODEGEN_POINTER_SIZE_BYTES 8
#define CODEGEN_SIZEOF_RECURSION_LIMIT 32


int codegen_type_uses_qword(int type_tag)
{
    return (type_tag == LONGINT_TYPE || type_tag == REAL_TYPE ||
        type_tag == POINTER_TYPE || type_tag == STRING_TYPE);
}

int codegen_type_is_signed(int type_tag)
{
    switch (type_tag)
    {
        case INT_TYPE:
        case LONGINT_TYPE:
            return 1;
        default:
            return 0;
    }
}

int codegen_expr_is_signed(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    return codegen_type_is_signed(expr->resolved_type);
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

static int codegen_sizeof_type(CodeGenContext *ctx, int type_tag, const char *type_id,
    struct RecordType *record_type, long long *size_out, int depth);

static int codegen_sizeof_record(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out, int depth);
int codegen_sizeof_record_type(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out);

static int codegen_sizeof_alias(CodeGenContext *ctx, struct TypeAlias *alias,
    long long *size_out, int depth);

static int codegen_sizeof_hashnode(CodeGenContext *ctx, HashNode_t *node,
    long long *size_out, int depth);

static long long codegen_sizeof_var_type(enum VarType var_type);

int codegen_expr_is_addressable(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    switch (expr->type)
    {
        case EXPR_VAR_ID:
        case EXPR_ARRAY_ACCESS:
        case EXPR_RECORD_ACCESS:
        case EXPR_POINTER_DEREF:
            return 1;
        default:
            return 0;
    }
}

static int codegen_sizeof_array_node(CodeGenContext *ctx, HashNode_t *node,
    long long *size_out, int depth)
{
    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx,
            "ERROR: Type resolution exceeded supported recursion depth.");
        return 1;
    }

    if (node->is_dynamic_array || node->array_end < node->array_start)
    {
        codegen_report_error(ctx,
            "ERROR: Unable to determine size of dynamic array %s.",
            node->id != NULL ? node->id : "");
        return 1;
    }

    long long element_size = node->element_size;
    if (element_size <= 0)
    {
        if (node->type_alias != NULL && node->type_alias->is_array)
        {
            if (codegen_sizeof_type(ctx, node->type_alias->array_element_type,
                    node->type_alias->array_element_type_id, NULL,
                    &element_size, depth + 1) != 0)
                return 1;
        }
        else if (node->record_type != NULL && node->var_type == HASHVAR_RECORD)
        {
            if (codegen_sizeof_record(ctx, node->record_type, &element_size,
                    depth + 1) != 0)
                return 1;
        }
        else
        {
            long long base = codegen_sizeof_var_type(node->var_type);
            if (base < 0)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to determine element size for array %s.",
                    node->id != NULL ? node->id : "");
                return 1;
            }
            element_size = base;
        }
    }

    long long count = (long long)node->array_end - (long long)node->array_start + 1;
    if (count < 0)
    {
        codegen_report_error(ctx,
            "ERROR: Invalid bounds for array %s during size computation.",
            node->id != NULL ? node->id : "");
        return 1;
    }

    *size_out = element_size * count;
    return 0;
}

static long long codegen_sizeof_type_tag(int type_tag)
{
    switch (type_tag)
    {
        case INT_TYPE:
        case BOOL:
        case SET_TYPE:
        case ENUM_TYPE:
            return 4;
        case LONGINT_TYPE:
        case REAL_TYPE:
            return 8;
        case STRING_TYPE:
        case POINTER_TYPE:
        case FILE_TYPE:
        case PROCEDURE:
            return CODEGEN_POINTER_SIZE_BYTES;
        case CHAR_TYPE:
            return 1;
        case RECORD_TYPE:
            return -1;
        default:
            return -1;
    }
}

static long long codegen_sizeof_var_type(enum VarType var_type)
{
    switch (var_type)
    {
        case HASHVAR_INTEGER:
        case HASHVAR_BOOLEAN:
        case HASHVAR_SET:
        case HASHVAR_ENUM:
            return 4;
        case HASHVAR_LONGINT:
        case HASHVAR_REAL:
            return 8;
        case HASHVAR_PCHAR:
        case HASHVAR_PROCEDURE:
        case HASHVAR_POINTER:
        case HASHVAR_FILE:
            return CODEGEN_POINTER_SIZE_BYTES;
        case HASHVAR_CHAR:
            return 1;
        default:
            return -1;
    }
}

static int codegen_sizeof_type(CodeGenContext *ctx, int type_tag, const char *type_id,
    struct RecordType *record_type, long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Unable to determine type size due to excessive nesting.");
        return 1;
    }

    if (record_type != NULL)
        return codegen_sizeof_record(ctx, record_type, size_out, depth + 1);

    if (type_tag == RECORD_TYPE && type_id == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to resolve anonymous record type for size computation.");
        return 1;
    }

    if (type_tag != UNKNOWN_TYPE)
    {
        long long base = codegen_sizeof_type_tag(type_tag);
        if (base >= 0)
        {
            *size_out = base;
            return 0;
        }
    }

    if (type_id != NULL && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, ctx->symtab, (char *)type_id) >= 0 && node != NULL)
            return codegen_sizeof_hashnode(ctx, node, size_out, depth + 1);

        codegen_report_error(ctx, "ERROR: Unable to resolve type %s for size computation.", type_id);
        return 1;
    }

    codegen_report_error(ctx, "ERROR: Unable to determine size for expression type %d.", type_tag);
    return 1;
}

static int codegen_sizeof_record(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    if (record == NULL)
    {
        *size_out = 0;
        return 0;
    }

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Record type nesting exceeds supported depth.");
        return 1;
    }

    long long total = 0;
    for (ListNode_t *cur = record->fields; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_RECORD_FIELD || cur->cur == NULL)
            continue;

        struct RecordField *field = (struct RecordField *)cur->cur;
        long long field_size = 0;

        if (field->is_array)
        {
            if (field->array_is_open || field->array_end < field->array_start)
            {
                field_size = CODEGEN_POINTER_SIZE_BYTES;
            }
            else
            {
                long long element_size = 0;
                if (codegen_sizeof_type(ctx, field->array_element_type,
                        field->array_element_type_id, NULL,
                        &element_size, depth + 1) != 0)
                    return 1;

                long long count = (long long)field->array_end - (long long)field->array_start + 1;
                if (count < 0)
                {
                    codegen_report_error(ctx,
                        "ERROR: Invalid bounds for array field %s.",
                        field->name != NULL ? field->name : "");
                    return 1;
                }

                field_size = element_size * count;
            }

            total += field_size;
            continue;
        }

        if (field->nested_record != NULL)
        {
            if (codegen_sizeof_record(ctx, field->nested_record, &field_size, depth + 1) != 0)
                return 1;
        }
        else
        {
            if (codegen_sizeof_type(ctx, field->type, field->type_id, NULL,
                    &field_size, depth + 1) != 0)
                return 1;
        }

        total += field_size;
    }

    *size_out = total;
    return 0;
}

int codegen_sizeof_record_type(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out)
{
    return codegen_sizeof_record(ctx, record, size_out, 0);
}

static int codegen_sizeof_alias(CodeGenContext *ctx, struct TypeAlias *alias,
    long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    if (alias == NULL)
    {
        codegen_report_error(ctx, "ERROR: Incomplete type alias encountered during size computation.");
        return 1;
    }

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Type alias nesting exceeds supported depth.");
        return 1;
    }

    if (alias->is_array)
    {
        if (alias->is_open_array || alias->array_end < alias->array_start)
        {
            codegen_report_error(ctx, "ERROR: Unable to determine size of open array type.");
            return 1;
        }

        long long element_size = 0;
        if (codegen_sizeof_type(ctx, alias->array_element_type, alias->array_element_type_id,
                NULL, &element_size, depth + 1) != 0)
            return 1;

        long long count = (long long)alias->array_end - (long long)alias->array_start + 1;
        if (count < 0)
        {
            codegen_report_error(ctx, "ERROR: Invalid bounds for array type during size computation.");
            return 1;
        }

        *size_out = element_size * count;
        return 0;
    }

    if (alias->base_type != UNKNOWN_TYPE)
        return codegen_sizeof_type(ctx, alias->base_type, NULL, NULL, size_out, depth + 1);

    if (alias->target_type_id != NULL)
        return codegen_sizeof_type(ctx, UNKNOWN_TYPE, alias->target_type_id, NULL,
            size_out, depth + 1);

    codegen_report_error(ctx, "ERROR: Unable to resolve type alias size.");
    return 1;
}

static int codegen_sizeof_hashnode(CodeGenContext *ctx, HashNode_t *node,
    long long *size_out, int depth)
{
    if (size_out == NULL || node == NULL)
        return 1;

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Type resolution exceeded supported recursion depth.");
        return 1;
    }

    if (node->is_array)
        return codegen_sizeof_array_node(ctx, node, size_out, depth);

    if (node->hash_type == HASHTYPE_TYPE)
    {
        if (node->record_type != NULL)
            return codegen_sizeof_record(ctx, node->record_type, size_out, depth + 1);
        if (node->type_alias != NULL)
            return codegen_sizeof_alias(ctx, node->type_alias, size_out, depth + 1);
    }

    if (node->var_type == HASHVAR_RECORD && node->record_type != NULL)
        return codegen_sizeof_record(ctx, node->record_type, size_out, depth + 1);

    if (node->type_alias != NULL)
        return codegen_sizeof_alias(ctx, node->type_alias, size_out, depth + 1);

    long long base = codegen_sizeof_var_type(node->var_type);
    if (base >= 0)
    {
        *size_out = base;
        return 0;
    }

    codegen_report_error(ctx, "ERROR: Unable to determine size for symbol %s.",
        node->id != NULL ? node->id : "");
    return 1;
}

int codegen_get_record_size(CodeGenContext *ctx, struct Expression *expr,
    long long *size_out)
{
    if (expr == NULL || size_out == NULL)
        return 1;

    if (expr->record_type != NULL)
        return codegen_sizeof_record(ctx, expr->record_type, size_out, 0);

    if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, ctx->symtab, expr->expr_data.id) >= 0 && node != NULL)
            return codegen_sizeof_hashnode(ctx, node, size_out, 0);
    }

    if (expr->type == EXPR_POINTER_DEREF)
    {
        if (expr->pointer_subtype_id != NULL && ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *node = NULL;
            if (FindIdent(&node, ctx->symtab, expr->pointer_subtype_id) >= 0 && node != NULL)
                return codegen_sizeof_hashnode(ctx, node, size_out, 0);
        }

        struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
        while (pointer_expr != NULL && pointer_expr->type == EXPR_TYPECAST &&
            pointer_expr->expr_data.typecast_data.expr != NULL)
        {
            pointer_expr = pointer_expr->expr_data.typecast_data.expr;
        }

        if (pointer_expr != NULL)
        {
            if (pointer_expr->record_type != NULL)
                return codegen_sizeof_record(ctx, pointer_expr->record_type, size_out, 0);

            if (pointer_expr->pointer_subtype_id != NULL && ctx != NULL && ctx->symtab != NULL)
            {
                HashNode_t *node = NULL;
                if (FindIdent(&node, ctx->symtab, pointer_expr->pointer_subtype_id) >= 0 && node != NULL)
                    return codegen_sizeof_hashnode(ctx, node, size_out, 0);
            }
        }
    }

    if (expr->type == EXPR_TYPECAST && expr->expr_data.typecast_data.expr != NULL)
        return codegen_get_record_size(ctx, expr->expr_data.typecast_data.expr, size_out);

    codegen_report_error(ctx, "ERROR: Unable to determine size for record expression.");
    return 1;
}

int codegen_sizeof_pointer_target(CodeGenContext *ctx, struct Expression *pointer_expr,
    long long *size_out)
{
    if (pointer_expr == NULL || size_out == NULL)
        return 1;

    int subtype = pointer_expr->pointer_subtype;
    const char *type_id = pointer_expr->pointer_subtype_id;
    struct RecordType *record_type = pointer_expr->record_type;

    if (record_type == NULL && type_id != NULL && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, ctx->symtab, (char *)type_id) >= 0 && node != NULL)
            record_type = node->record_type;
    }

    if (record_type == NULL && subtype == RECORD_TYPE && type_id == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to determine record size for pointer target.");
        return 1;
    }

    return codegen_sizeof_type(ctx, subtype, type_id, record_type, size_out, 0);
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
    codegen_begin_expression(ctx);
    expr_node_t *expr_tree = build_expr_tree(expr);
    Register_t *target_reg = codegen_try_get_reg(&inst_list, ctx, describe_expression_kind(expr));
    if (target_reg == NULL)
    {
        free_expr_tree(expr_tree);
        if (out_reg != NULL)
            *out_reg = NULL;
        codegen_end_expression(ctx);
        return inst_list;
    }

    inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
    free_expr_tree(expr_tree);

    if (out_reg != NULL)
    {
        *out_reg = target_reg;
        codegen_end_expression(ctx);
    }
    else
    {
        codegen_end_expression(ctx);
        free_reg(get_reg_stack(), target_reg);
    }
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

ListNode_t *codegen_zero_extend32_to64(ListNode_t *inst_list, const char *src_reg32, const char *dst_reg32)
{
    assert(src_reg32 != NULL);
    assert(dst_reg32 != NULL);

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", src_reg32, dst_reg32);
    return add_inst(inst_list, buffer);
}

int codegen_sizeof_type_reference(CodeGenContext *ctx, int type_tag, const char *type_id,
    struct RecordType *record_type, long long *size_out)
{
    return codegen_sizeof_type(ctx, type_tag, type_id, record_type, size_out, 0);
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
    {
        if (expr->resolved_type == CHAR_TYPE)
            snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        else
            snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    }
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
    else if (inner->type == EXPR_RECORD_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_record_field_address(inner, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }
    else if (inner->type == EXPR_POINTER_DEREF)
    {
        struct Expression *pointer_expr = inner->expr_data.pointer_deref_data.pointer_expr;
        if (pointer_expr == NULL)
        {
            codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
            return inst_list;
        }

        Register_t *addr_reg = NULL;
        inst_list = codegen_expr_tree_value(pointer_expr, inst_list, ctx, &addr_reg);
        if (addr_reg == NULL)
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
    {
        if (expr->resolved_type == CHAR_TYPE)
            snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        else
            snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    }
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
        case EXPR_NIL:
            CODEGEN_DEBUG("DEBUG: Processing nil literal expression\n");
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

    struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
    struct Expression *index_expr = expr->expr_data.array_access_data.index_expr;

    if (array_expr == NULL)
    {
        codegen_report_error(ctx, "ERROR: Array access missing base expression.");
        return inst_list;
    }

    int base_is_string = (array_expr->resolved_type == STRING_TYPE);

    if (!array_expr->is_array_expr && !base_is_string)
    {
        codegen_report_error(ctx, "ERROR: Expression is not indexable as an array.");
        return inst_list;
    }

    Register_t *index_reg = NULL;
    inst_list = codegen_expr_with_result(index_expr, inst_list, ctx, &index_reg);
    if (codegen_had_error(ctx) || index_reg == NULL)
        return inst_list;

    Register_t *base_reg = NULL;
    if (base_is_string)
    {
        inst_list = codegen_expr_with_result(array_expr, inst_list, ctx, &base_reg);
        if (codegen_had_error(ctx) || base_reg == NULL)
        {
            free_reg(get_reg_stack(), index_reg);
            return inst_list;
        }
    }
    else
    {
        inst_list = codegen_address_for_expr(array_expr, inst_list, ctx, &base_reg);
        if (codegen_had_error(ctx) || base_reg == NULL)
        {
            free_reg(get_reg_stack(), index_reg);
            return inst_list;
        }
    }

    char buffer[128];

    if (!base_is_string && array_expr->array_is_dynamic)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", base_reg->bit_64, base_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    if (base_is_string)
    {
        snprintf(buffer, sizeof(buffer), "\tsubl\t$1, %s\n", index_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        int lower_bound = array_expr->array_lower_bound;
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
    }

    inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32, index_reg->bit_64);

    if (base_is_string)
    {
        snprintf(buffer, sizeof(buffer), "\tleaq\t(%s,%s), %s\n",
            base_reg->bit_64, index_reg->bit_64, index_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        long long element_size_ll = array_expr->array_element_size;
        int need_element_size = 0;
        if (element_size_ll <= 0)
            need_element_size = 1;
        else if (array_expr->array_element_record_type != NULL)
            need_element_size = 1;
        else if (array_expr->array_element_type == RECORD_TYPE)
            need_element_size = 1;
        else if (array_expr->array_element_type == UNKNOWN_TYPE &&
            array_expr->array_element_type_id != NULL)
            need_element_size = 1;

        if (need_element_size)
        {
            if (codegen_sizeof_type(ctx, array_expr->array_element_type,
                    array_expr->array_element_type_id,
                    array_expr->array_element_record_type,
                    &element_size_ll, 0) != 0 || element_size_ll <= 0)
            {
                codegen_report_error(ctx, "ERROR: Unable to determine element size for array access.");
                free_reg(get_reg_stack(), base_reg);
                free_reg(get_reg_stack(), index_reg);
                return inst_list;
            }
        }

        int element_size = (int)element_size_ll;
        static const int scaled_sizes[] = {1, 2, 4, 8};
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
            snprintf(buffer, sizeof(buffer), "\tleaq\t(%s,%s,%d), %s\n",
                base_reg->bit_64, index_reg->bit_64, element_size, index_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (element_size != 1)
            {
                snprintf(buffer, sizeof(buffer), "\timulq\t$%d, %s\n", element_size, index_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }

            snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n", base_reg->bit_64, index_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    free_reg(get_reg_stack(), base_reg);
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
        if (expr->resolved_type == CHAR_TYPE)
        {
            snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        }
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
            if (!codegen_expr_is_addressable(arg_expr))
            {
                codegen_report_error(ctx,
                    "ERROR: Unsupported expression type for var parameter.");
                return inst_list;
            }

            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
                return inst_list;

            if (arg_infos != NULL)
            {
                arg_infos[arg_num].reg = addr_reg;
                arg_infos[arg_num].spill = NULL;
                arg_infos[arg_num].expr = arg_expr;
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
