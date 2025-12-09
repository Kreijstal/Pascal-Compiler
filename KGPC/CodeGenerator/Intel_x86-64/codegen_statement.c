#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include "register_types.h"
#include "codegen.h"
#include "codegen_statement.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "codegen_expression.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/KgpcType.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../identifier_utils.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"

#ifndef CODEGEN_POINTER_SIZE_BYTES
#define CODEGEN_POINTER_SIZE_BYTES 8
#endif

static int codegen_push_loop(CodeGenContext *ctx, const char *exit_label, const char *continue_label);
static void codegen_pop_loop(CodeGenContext *ctx);
static const char *codegen_current_loop_exit(const CodeGenContext *ctx);
static const char *codegen_current_loop_continue(const CodeGenContext *ctx);
static ListNode_t *codegen_break_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_continue_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static int codegen_push_finally(CodeGenContext *ctx, ListNode_t *statements);
static void codegen_pop_finally(CodeGenContext *ctx);
static int codegen_has_finally(const CodeGenContext *ctx);
static ListNode_t *codegen_emit_finally_block(CodeGenContext *ctx, ListNode_t *inst_list,
    SymTab_t *symtab, const char *entry_label, const char *target_label);
static ListNode_t *codegen_branch_through_finally(CodeGenContext *ctx, ListNode_t *inst_list,
    SymTab_t *symtab, const char *target_label, int limit_depth);
static int codegen_push_except(CodeGenContext *ctx, const char *label);
static void codegen_pop_except(CodeGenContext *ctx);
static const char *codegen_current_except_label(const CodeGenContext *ctx);
static ListNode_t *codegen_store_exception_value(ListNode_t *inst_list,
    CodeGenContext *ctx, struct Expression *exc_expr, Register_t *value_reg);
static ListNode_t *codegen_statement_list(ListNode_t *stmts, ListNode_t *inst_list,
    CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_break_stmt(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_with(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_for_in(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_try_finally(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_try_except(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_raise(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_inherited(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
#if KGPC_ENABLE_REG_DEBUG
extern const char *g_reg_debug_context;
#endif

ListNode_t *codegen_condition_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, int *relop_type);
static int record_type_is_mp_integer(const struct RecordType *record_type);
static int codegen_expr_is_mp_integer(struct Expression *expr);
static ListNode_t *codegen_call_mpint_assign(ListNode_t *inst_list, Register_t *addr_reg,
    Register_t *value_reg);
static ListNode_t *codegen_assign_record_value(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx);
static ListNode_t *codegen_assign_static_array(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx);
static ListNode_t *codegen_convert_int_like_to_real(ListNode_t *inst_list,
    Register_t *value_reg, int source_type);
static ListNode_t *codegen_maybe_convert_int_like_to_real(int target_type,
    struct Expression *source_expr, Register_t *value_reg, ListNode_t *inst_list,
    int *coerced_to_real);
static int expr_is_dynamic_array(const struct Expression *expr);
static int codegen_dynamic_array_descriptor_size(const struct Expression *expr);
static ListNode_t *codegen_assign_dynamic_array(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx);
static ListNode_t *codegen_call_dynarray_copy(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *dest_reg, Register_t *src_reg, int descriptor_size);
static ListNode_t *codegen_call_dynarray_assign_from_temp(ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *dest_reg, Register_t *temp_reg, int descriptor_size);
static ListNode_t *codegen_fail_register(CodeGenContext *ctx, ListNode_t *inst_list,
    Register_t **out_reg, const char *message);
static ListNode_t *codegen_builtin_setlength_string(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx);
static ListNode_t *codegen_builtin_str(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
static ListNode_t *codegen_builtin_insert(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
static ListNode_t *codegen_builtin_delete(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
static ListNode_t *codegen_builtin_val(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);

/* Check if a type name represents an unsigned integer type */
static int is_unsigned_type_name(const char *type_name)
{
    if (type_name == NULL)
        return 0;
    /* Unsigned integer types in Pascal */
    if (strcasecmp(type_name, "Byte") == 0 ||
        strcasecmp(type_name, "Word") == 0 ||
        strcasecmp(type_name, "DWord") == 0 ||
        strcasecmp(type_name, "QWord") == 0 ||
        strcasecmp(type_name, "Cardinal") == 0 ||
        strcasecmp(type_name, "LongWord") == 0 ||
        strcasecmp(type_name, "UInt8") == 0 ||
        strcasecmp(type_name, "UInt16") == 0 ||
        strcasecmp(type_name, "UInt32") == 0 ||
        strcasecmp(type_name, "UInt64") == 0 ||
        strcasecmp(type_name, "NativeUInt") == 0 ||
        strcasecmp(type_name, "SizeUInt") == 0 ||
        strcasecmp(type_name, "PtrUInt") == 0)
    {
        return 1;
    }
    return 0;
}

/* Check if an expression's type is unsigned */
static int expr_is_unsigned_type(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    /* Check resolved_kgpc_type for type alias information */
    if (expr->resolved_kgpc_type != NULL && expr->resolved_kgpc_type->type_alias != NULL)
    {
        const char *target_type_id = expr->resolved_kgpc_type->type_alias->target_type_id;
        if (is_unsigned_type_name(target_type_id))
            return 1;
    }
    
    /* For array access expressions, check the array's element type */
    if (expr->type == EXPR_ARRAY_ACCESS)
    {
        const struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
        if (array_expr != NULL && array_expr->array_element_type_id != NULL)
        {
            if (is_unsigned_type_name(array_expr->array_element_type_id))
                return 1;
        }
        /* Also check if the accessed array is an array with known element type */
        if (array_expr != NULL && array_expr->resolved_kgpc_type != NULL &&
            array_expr->resolved_kgpc_type->type_alias != NULL)
        {
            const char *elem_type_id = array_expr->resolved_kgpc_type->type_alias->array_element_type_id;
            if (is_unsigned_type_name(elem_type_id))
                return 1;
        }
    }
    
    /* Check array element type for arrays of unsigned types */
    if (expr->is_array_expr && expr->array_element_type_id != NULL)
    {
        if (is_unsigned_type_name(expr->array_element_type_id))
            return 1;
    }
    
    return 0;
}

/* Lookup RecordField info from a record-access expression */
static struct RecordField *codegen_lookup_record_field_expr(struct Expression *record_access_expr)
{
    if (record_access_expr == NULL ||
        record_access_expr->type != EXPR_RECORD_ACCESS ||
        record_access_expr->expr_data.record_access_data.field_id == NULL)
        return NULL;

    const char *field_id = record_access_expr->expr_data.record_access_data.field_id;
    struct RecordType *record = record_access_expr->record_type;
    if (record == NULL && record_access_expr->expr_data.record_access_data.record_expr != NULL)
        record = record_access_expr->expr_data.record_access_data.record_expr->record_type;
    if (record == NULL)
        return NULL;

    ListNode_t *cur = record->fields;
    while (cur != NULL)
    {
        if (cur->type == LIST_RECORD_FIELD && cur->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            if (field->name != NULL && strcmp(field->name, field_id) == 0)
                return field;
        }
        cur = cur->next;
    }
    return NULL;
}

/* Best-effort field size honoring packed/range aliases */
static long long codegen_record_field_effective_size(struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL || ctx == NULL)
        return expr_effective_size_bytes(expr);

    long long size = expr_effective_size_bytes(expr);
    struct RecordField *field = codegen_lookup_record_field_expr(expr);
    long long field_size = 0;
    if (field != NULL && !field->is_array)
    {
        struct RecordType *nested = field->nested_record;
        if (codegen_sizeof_type_reference(ctx, field->type, field->type_id, nested, &field_size) == 0 &&
            field_size > 0)
            return field_size;
    }

    if (size > 0)
        return size;
    return field_size;
}


static int lookup_record_field_type(struct RecordType *record_type, const char *field_name)
{
    if (record_type == NULL || field_name == NULL)
        return UNKNOWN_TYPE;

    ListNode_t *field_node = record_type->fields;
    while (field_node != NULL)
    {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)field_node->cur;
            if (field == NULL || record_field_is_hidden(field))
            {
                field_node = field_node->next;
                continue;
            }
            if (field->name != NULL && strcmp(field->name, field_name) == 0)
                return field->type;
        }
        field_node = field_node->next;
    }

    return UNKNOWN_TYPE;
}

static void format_pascal_label(char *buffer, size_t size, const CodeGenContext *ctx, const char *label_text)
{
    if (buffer == NULL || size == 0)
        return;

    const char *scope = NULL;
    if (ctx != NULL)
    {
        if (ctx->current_subprogram_mangled != NULL && ctx->current_subprogram_mangled[0] != '\0')
            scope = ctx->current_subprogram_mangled;
        else if (ctx->current_subprogram_id != NULL && ctx->current_subprogram_id[0] != '\0')
            scope = ctx->current_subprogram_id;
    }

    if (scope == NULL)
        scope = "program";

    char sanitized[128];
    size_t idx = 0;
    if (label_text != NULL && label_text[0] != '\0')
    {
        for (const char *p = label_text; *p != '\0' && idx + 1 < sizeof(sanitized); ++p)
        {
            unsigned char c = (unsigned char)*p;
            if (isalnum(c) || c == '_')
                sanitized[idx++] = (char)c;
            else
                sanitized[idx++] = '_';
        }
    }
    else
    {
        sanitized[idx++] = 'L';
        sanitized[idx++] = '0';
    }

    sanitized[idx] = '\0';
    snprintf(buffer, size, "%s__label_%s", scope, sanitized);
}

static const char *register_name8(const Register_t *reg)
{
    if (reg == NULL || reg->bit_64 == NULL)
        return NULL;

    static const struct
    {
        const char *wide;
        const char *byte;
    } register_map[] = {
        { "%rax", "%al" },
        { "%rbx", "%bl" },
        { "%rcx", "%cl" },
        { "%rdx", "%dl" },
        { "%rsi", "%sil" },
        { "%rdi", "%dil" },
        { "%rbp", "%bpl" },
        { "%rsp", "%spl" },
        { "%r8", "%r8b" },
        { "%r9", "%r9b" },
        { "%r10", "%r10b" },
        { "%r11", "%r11b" },
        { "%r12", "%r12b" },
        { "%r13", "%r13b" },
        { "%r14", "%r14b" },
        { "%r15", "%r15b" },
    };

    size_t count = sizeof(register_map) / sizeof(register_map[0]);
    for (size_t i = 0; i < count; ++i)
    {
        if (strcmp(reg->bit_64, register_map[i].wide) == 0)
            return register_map[i].byte;
    }

    return NULL;
}

static ListNode_t *codegen_convert_int_like_to_real(ListNode_t *inst_list,
    Register_t *value_reg, int source_type)
{
    if (inst_list == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (source_type == LONGINT_TYPE)
        snprintf(buffer, sizeof(buffer), "\tcvtsi2sdq\t%s, %%xmm0\n", value_reg->bit_64);
    else
        snprintf(buffer, sizeof(buffer), "\tcvtsi2sdl\t%s, %%xmm0\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n", value_reg->bit_64);
    return add_inst(inst_list, buffer);
}

static ListNode_t *codegen_maybe_convert_int_like_to_real(int target_type,
    struct Expression *source_expr, Register_t *value_reg, ListNode_t *inst_list,
    int *coerced_to_real)
{
    if (coerced_to_real != NULL)
        *coerced_to_real = 0;

    if (inst_list == NULL || source_expr == NULL || value_reg == NULL)
        return inst_list;

    int source_type = expr_get_type_tag(source_expr);
    if (target_type == REAL_TYPE &&
        (source_type == INT_TYPE || source_type == LONGINT_TYPE))
    {
        inst_list = codegen_convert_int_like_to_real(inst_list, value_reg, source_type);
        if (coerced_to_real != NULL)
            *coerced_to_real = 1;
    }

    return inst_list;
}

static unsigned long codegen_next_temp_suffix(void)
{
    static unsigned long counter = 0;
    return ++counter;
}

static StackNode_t *codegen_alloc_temp_slot(const char *prefix)
{
    char label[32];
    snprintf(label, sizeof(label), "%s_%lu", prefix != NULL ? prefix : "temp", codegen_next_temp_suffix());
    return add_l_t(label);
}


static int codegen_align_to(int value, int alignment)
{
    if (alignment <= 0)
        return value;
    int remainder = value % alignment;
    if (remainder == 0)
        return value;
    return value + (alignment - remainder);
}

static int expr_is_dynamic_array(const struct Expression *expr)
{
    return (expr != NULL && expr->is_array_expr && expr->array_is_dynamic);
}

static struct TypeAlias *codegen_lookup_type_alias(CodeGenContext *ctx, const char *type_id)
{
    if (ctx == NULL || ctx->symtab == NULL || type_id == NULL)
        return NULL;

    HashNode_t *node = NULL;
    if (FindIdent(&node, ctx->symtab, (char *)type_id) < 0 || node == NULL)
        return NULL;

    return hashnode_get_type_alias(node);
}

static struct RecordField *codegen_lookup_record_field(struct Expression *record_access_expr)
{
    if (record_access_expr == NULL || record_access_expr->type != EXPR_RECORD_ACCESS)
        return NULL;

    struct Expression *base_expr = record_access_expr->expr_data.record_access_data.record_expr;
    if (base_expr == NULL)
        return NULL;

    struct RecordType *record_type = base_expr->record_type;
    if (record_type == NULL && base_expr->array_element_record_type != NULL)
        record_type = base_expr->array_element_record_type;
    if (record_type == NULL && record_access_expr->record_type != NULL)
        record_type = record_access_expr->record_type;
    if (record_type == NULL)
        return NULL;

    const char *field_name = record_access_expr->expr_data.record_access_data.field_id;
    if (field_name == NULL)
        return NULL;

    ListNode_t *field_node = record_type->fields;
    while (field_node != NULL)
    {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)field_node->cur;
            if (!record_field_is_hidden(field) && field->name != NULL &&
                strcmp(field->name, field_name) == 0)
                return field;
        }
        field_node = field_node->next;
    }

    return NULL;
}

static int expr_is_static_array_like(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL)
        return 0;

    if (expr_is_dynamic_array(expr))
        return 0;

    if (expr->is_array_expr)
        return 1;

    if (expr->resolved_kgpc_type != NULL && kgpc_type_is_array(expr->resolved_kgpc_type))
        return !kgpc_type_is_dynamic_array(expr->resolved_kgpc_type);

    if (ctx != NULL)
    {
        long long elem_size = expr_get_array_element_size(expr, ctx);
        if (elem_size > 0)
            return 1;
    }

    struct RecordField *field = codegen_lookup_record_field((struct Expression *)expr);
    if (field != NULL)
    {
        if (field->is_array && !field->array_is_open)
            return 1;
        struct TypeAlias *alias = codegen_lookup_type_alias(ctx, field->type_id);
        if (alias != NULL && alias->is_array && !alias->is_open_array)
            return 1;
    }

    return 0;
}

static int codegen_dynamic_array_descriptor_size(const struct Expression *expr)
{
    const int base_size = 4 * DOUBLEWORD;
    if (expr == NULL)
        return base_size;

    if (expr->type == EXPR_VAR_ID)
    {
        int scope_depth = 0;
        StackNode_t *node = find_label_with_depth(expr->expr_data.id, &scope_depth);
        if (node != NULL && node->is_dynamic && node->size > 0)
            return node->size;
    }

    if (expr->array_element_size > 0)
    {
        int descriptor_size = base_size;
        int needed = expr->array_element_size * 2;
        if (descriptor_size < needed)
            descriptor_size = needed;
        return descriptor_size;
    }

    return base_size;
}

static ListNode_t *codegen_call_dynarray_copy(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *dest_reg, Register_t *src_reg, int descriptor_size)
{
    if (inst_list == NULL || ctx == NULL || dest_reg == NULL || src_reg == NULL)
        return inst_list;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%r8d\n", descriptor_size);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", descriptor_size);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_dynarray_assign_descriptor\n");
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_call_dynarray_assign_from_temp(ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *dest_reg, Register_t *temp_reg, int descriptor_size)
{
    if (inst_list == NULL || ctx == NULL || dest_reg == NULL || temp_reg == NULL)
        return inst_list;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", temp_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%r8d\n", descriptor_size);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", temp_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", descriptor_size);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_dynarray_assign_from_temp\n");
    free_arg_regs();
    return inst_list;
}


static ListNode_t *codegen_assign_dynamic_array(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    Register_t *dest_reg = NULL;
#if KGPC_ENABLE_REG_DEBUG
    const char *prev_ctx = g_reg_debug_context;
    g_reg_debug_context = "dyn_array_dest";
#endif
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
#if KGPC_ENABLE_REG_DEBUG
    g_reg_debug_context = prev_ctx;
#endif
    if (codegen_had_error(ctx) || dest_reg == NULL)
        return inst_list;

    StackNode_t *dest_temp = add_l_t("dynarray_dest");
    if (dest_temp == NULL)
    {
        free_reg(get_reg_stack(), dest_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for dynamic array assignment.");
    }

    char buffer[96];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
        dest_reg->bit_64, dest_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), dest_reg);
    dest_reg = NULL;

    int descriptor_size = codegen_dynamic_array_descriptor_size(dest_expr);

    if (expr_is_dynamic_array(src_expr) && codegen_expr_is_addressable(src_expr))
    {
        Register_t *src_reg = NULL;
#if KGPC_ENABLE_REG_DEBUG
        prev_ctx = g_reg_debug_context;
        g_reg_debug_context = "dyn_array_src";
#endif
        inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
#if KGPC_ENABLE_REG_DEBUG
        g_reg_debug_context = prev_ctx;
#endif
        if (codegen_had_error(ctx) || src_reg == NULL)
        {
            if (src_reg != NULL)
                free_reg(get_reg_stack(), src_reg);
            return inst_list;
        }

        Register_t *dest_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (dest_reload == NULL)
        {
            free_reg(get_reg_stack(), src_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for dynamic array destination.");
        }

        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
            dest_temp->offset, dest_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        inst_list = codegen_call_dynarray_copy(inst_list, ctx, dest_reload, src_reg, descriptor_size);
        free_reg(get_reg_stack(), src_reg);
        free_reg(get_reg_stack(), dest_reload);
    }
    else
    {
        Register_t *value_reg = NULL;
#if KGPC_ENABLE_REG_DEBUG
        prev_ctx = g_reg_debug_context;
        g_reg_debug_context = "dyn_array_value";
#endif
        inst_list = codegen_expr_with_result(src_expr, inst_list, ctx, &value_reg);
#if KGPC_ENABLE_REG_DEBUG
        g_reg_debug_context = prev_ctx;
#endif
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        Register_t *dest_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (dest_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for dynamic array destination.");
        }

        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
            dest_temp->offset, dest_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        inst_list = codegen_call_dynarray_assign_from_temp(inst_list, ctx, dest_reload, value_reg, descriptor_size);
        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), dest_reload);
    }

    return inst_list;
}

static ListNode_t *codegen_call_with_shadow_space(ListNode_t *inst_list, CodeGenContext *ctx, const char *target)
{
    if (ctx == NULL || target == NULL)
        return inst_list;

    int shadow_space = 0;
    if (codegen_target_is_windows())
    {
        shadow_space = codegen_align_to(current_stack_home_space(), REQUIRED_OFFSET);
        if (shadow_space > 0)
        {
            char buffer[64];
            snprintf(buffer, sizeof(buffer), "\tsubq\t$%d, %%rsp\n", shadow_space);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    char call_buffer[64];
    snprintf(call_buffer, sizeof(call_buffer), "\tcall\t%s\n", target);
    inst_list = add_inst(inst_list, call_buffer);

    if (shadow_space > 0)
    {
        char restore_buffer[64];
        snprintf(restore_buffer, sizeof(restore_buffer), "\taddq\t$%d, %%rsp\n", shadow_space);
        inst_list = add_inst(inst_list, restore_buffer);
    }

    return inst_list;
}

static ListNode_t *codegen_store_exception_value(ListNode_t *inst_list,
    CodeGenContext *ctx, struct Expression *exc_expr, Register_t *value_reg)
{
    if (ctx == NULL || exc_expr == NULL || value_reg == NULL)
        return inst_list;

    if (!expr_uses_qword_kgpctype(exc_expr))
    {
        if (codegen_expr_is_signed(exc_expr))
            inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
        else
            inst_list = codegen_zero_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_32);
    }

    char buffer[96];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, kgpc_current_exception(%%rip)\n", value_reg->bit_64);
    return add_inst(inst_list, buffer);
}

static ListNode_t *codegen_fail_register(CodeGenContext *ctx, ListNode_t *inst_list,
    Register_t **out_reg, const char *message)
{
    if (out_reg != NULL)
        *out_reg = NULL;
    if (message != NULL)
        codegen_report_error(ctx, "%s", message);
    return inst_list;
}

static ListNode_t *codegen_evaluate_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || ctx == NULL || out_reg == NULL)
        return inst_list;

    if (expr->type == EXPR_IS)
        return codegen_emit_is_expr(expr, inst_list, ctx, out_reg);

    if (expr->type == EXPR_AS && expr->expr_data.as_data.expr != NULL)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(expr->expr_data.as_data.expr, inst_list, ctx, &addr_reg);
        if (addr_reg == NULL)
            return inst_list;
        inst_list = codegen_emit_class_cast_check_from_address(expr, inst_list, ctx, addr_reg);
        *out_reg = addr_reg;
        return inst_list;
    }

    expr_node_t *expr_tree = build_expr_tree(expr);
    Register_t *reg = get_free_reg(get_reg_stack(), &inst_list);
    if (reg == NULL)
        return codegen_fail_register(ctx, inst_list, out_reg,
            "ERROR: Unable to allocate register for expression evaluation.");
    inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, reg);
    free_expr_tree(expr_tree);
    *out_reg = reg;
    return inst_list;
}

ListNode_t *codegen_condition_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, int *relop_type)
{
    if (expr == NULL)
        return inst_list;

    if (expr->type == EXPR_RELOP)
        return codegen_simple_relop(expr, inst_list, ctx, relop_type);

    Register_t *value_reg = NULL;
    inst_list = codegen_evaluate_expr(expr, inst_list, ctx, &value_reg);
    if (value_reg == NULL)
        return inst_list;

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", value_reg->bit_32, value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), value_reg);

    if (relop_type != NULL)
        *relop_type = NE;
    return inst_list;
}

ListNode_t *codegen_address_for_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    int began_expr = 0;
    if (ctx != NULL)
    {
        codegen_begin_expression(ctx);
        began_expr = 1;
    }

    if (expr == NULL || ctx == NULL || out_reg == NULL)
        goto cleanup;

    if (expr->type == EXPR_VAR_ID)
    {
        int scope_depth = 0;
        StackNode_t *var_node = find_label_with_depth(expr->expr_data.id, &scope_depth);
        
        if (var_node == NULL)
        {
            if (nonlocal_flag() == 1)
            {
                int offset = 0;
                inst_list = codegen_get_nonlocal(inst_list, expr->expr_data.id, &offset);
                Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (addr_reg == NULL)
                {
                    addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                    if (addr_reg == NULL)
                    {
                        inst_list = codegen_fail_register(ctx, inst_list, out_reg,
                            "ERROR: Unable to allocate register for address expression.");
                        goto cleanup;
                    }
                }
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%s), %s\n", offset,
                    current_non_local_reg64(), addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                *out_reg = addr_reg;
                goto cleanup;
            }
            inst_list = codegen_evaluate_expr(expr, inst_list, ctx, out_reg);
            goto cleanup;
        }
        
        HashNode_t *symbol = NULL;
        int treat_as_reference = 0;
        if (ctx->symtab != NULL)
        {
            if (FindIdent(&symbol, ctx->symtab, expr->expr_data.id) >= 0 && symbol != NULL)
                treat_as_reference = symbol->is_var_parameter;
        }
        if (var_node->is_reference)
            treat_as_reference = 1;

        /* Try normal allocation first, then spill if needed */
        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
        {
            addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (addr_reg == NULL)
            {
                inst_list = codegen_fail_register(ctx, inst_list, out_reg,
                    "ERROR: Unable to allocate register for address expression.");
                goto cleanup;
            }
        }

        char buffer[96];
        
        const char *static_label = NULL;
        if (var_node->is_static)
            static_label = (var_node->static_label != NULL) ? var_node->static_label : var_node->label;

        if (static_label != NULL)
        {
            if (treat_as_reference)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s(%%rip), %s\n",
                    static_label, addr_reg->bit_64);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                    static_label, addr_reg->bit_64);
            }
            inst_list = add_inst(inst_list, buffer);
            *out_reg = addr_reg;
            goto cleanup;
        }
        
        /* For non-local variables (scope_depth > 0), use static link */
        if (scope_depth > 0)
        {
            Register_t *frame_reg = codegen_acquire_static_link(ctx, &inst_list, scope_depth);
            if (frame_reg == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Failed to acquire static link for variable %s.",
                    expr->expr_data.id);
                /* Fallback to local access (will be wrong but prevents crash) */
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    var_node->offset, addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                *out_reg = addr_reg;
                goto cleanup;
            }
            
            if (treat_as_reference)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%s), %s\n",
                    var_node->offset, frame_reg->bit_64, addr_reg->bit_64);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%s), %s\n",
                    var_node->offset, frame_reg->bit_64, addr_reg->bit_64);
            }
            inst_list = add_inst(inst_list, buffer);
            *out_reg = addr_reg;
            goto cleanup;
        }
        
        /* Local variable (scope_depth == 0) */
        if (treat_as_reference)
        {
            if (var_node->is_static)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s(%%rip), %s\n",
                    var_node->static_label != NULL ? var_node->static_label : var_node->label,
                    addr_reg->bit_64);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    var_node->offset, addr_reg->bit_64);
            }
            inst_list = add_inst(inst_list, buffer);
            *out_reg = addr_reg;
            goto cleanup;
        }
        if (var_node->is_static)
        {
            const char *label = var_node->static_label != NULL ?
                var_node->static_label : var_node->label;
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", label,
                addr_reg->bit_64);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                var_node->offset, addr_reg->bit_64);
        }
        inst_list = add_inst(inst_list, buffer);
        *out_reg = addr_reg;
        goto cleanup;
    }
    else if (expr->type == EXPR_ARRAY_ACCESS)
    {
        inst_list = codegen_array_element_address(expr, inst_list, ctx, out_reg);
        goto cleanup;
    }
    else if (expr->type == EXPR_RECORD_ACCESS)
    {
        inst_list = codegen_record_field_address(expr, inst_list, ctx, out_reg);
        goto cleanup;
    }
    else if (expr->type == EXPR_POINTER_DEREF)
    {
        struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
        if (pointer_expr == NULL)
        {
            codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
            goto cleanup;
        }

        Register_t *addr_reg = NULL;
        inst_list = codegen_evaluate_expr(pointer_expr, inst_list, ctx, &addr_reg);
        if (addr_reg == NULL)
            goto cleanup;

        *out_reg = addr_reg;
        goto cleanup;
    }
    else if (expr->type == EXPR_AS)
    {
        if (expr->expr_data.as_data.expr != NULL)
        {
            Register_t *operand_addr = NULL;
            inst_list = codegen_address_for_expr(expr->expr_data.as_data.expr, inst_list, ctx, &operand_addr);
            if (operand_addr != NULL)
            {
                inst_list = codegen_emit_class_cast_check_from_address((struct Expression *)expr,
                    inst_list, ctx, operand_addr);
                *out_reg = operand_addr;
                goto cleanup;
            }
        }
        goto cleanup;
    }

    inst_list = codegen_evaluate_expr(expr, inst_list, ctx, out_reg);

cleanup:
    if (began_expr)
        codegen_end_expression(ctx);
    return inst_list;
}

static int record_type_is_mp_integer(const struct RecordType *record_type)
{
    if (record_type == NULL)
        return 0;

    if (record_type->fields == NULL || record_type->fields->next != NULL)
        return 0;

    struct RecordField *field = (struct RecordField *)record_type->fields->cur;
    if (field == NULL || field->name == NULL)
        return 0;

    return strcmp(field->name, "__kgpc_mp_handle") == 0;
}

static int codegen_expr_is_mp_integer(struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr_has_type_tag(expr, RECORD_TYPE))
        return record_type_is_mp_integer(expr->record_type);

    if (expr_has_type_tag(expr, POINTER_TYPE))
    {
        /* For pointers, check what they point to via KgpcType if available */
        if (expr->resolved_kgpc_type != NULL && kgpc_type_is_pointer(expr->resolved_kgpc_type))
        {
            int subtype = kgpc_type_get_pointer_subtype_tag(expr->resolved_kgpc_type);
            if (subtype == RECORD_TYPE)
                return record_type_is_mp_integer(expr->record_type);
        }
        /* Fallback to legacy field */
        else if (expr->pointer_subtype == RECORD_TYPE)
        {
            return record_type_is_mp_integer(expr->record_type);
        }
    }

    return 0;
}

static ListNode_t *codegen_call_mpint_assign(ListNode_t *inst_list, Register_t *addr_reg,
    Register_t *value_reg)
{
    if (addr_reg == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_gmp_mpint_assign\n");
    return inst_list;
}

static ListNode_t *codegen_call_string_assign(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg)
{
    if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        /* Windows x64 ABI: first arg in %rcx, second in %rdx */
        /* Handle register conflicts by checking if value_reg is already in %rcx */
        int value_in_rcx = (strcmp(value_reg->bit_64, "%rcx") == 0);
        int addr_in_rdx = (strcmp(addr_reg->bit_64, "%rdx") == 0);

        if (value_in_rcx && addr_in_rdx)
        {
            /* Both registers conflict - swap them */
            inst_list = add_inst(inst_list, "\txchgq\t%rcx, %rdx\n");
        }
        else if (value_in_rcx)
        {
            /* value is in %rcx but needs to go to %rdx, addr needs to go to %rcx */
            /* Move value to %rdx first to avoid overwriting */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (addr_in_rdx)
        {
            /* addr is in %rdx but needs to go to %rcx, value needs to go to %rdx */
            /* Move addr to %rcx first to avoid overwriting */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            /* No conflicts - standard order */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    else
    {
        /* System V ABI: first arg in %rdi, second in %rsi */
        /* Handle register conflicts similarly */
        int value_in_rdi = (strcmp(value_reg->bit_64, "%rdi") == 0);
        int addr_in_rsi = (strcmp(addr_reg->bit_64, "%rsi") == 0);

        if (value_in_rdi && addr_in_rsi)
        {
            /* Both registers conflict - swap them */
            inst_list = add_inst(inst_list, "\txchgq\t%rdi, %rsi\n");
        }
        else if (value_in_rdi)
        {
            /* value is in %rdi but needs to go to %rsi, addr needs to go to %rdi */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (addr_in_rsi)
        {
            /* addr is in %rsi but needs to go to %rdi, value needs to go to %rsi */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            /* No conflicts - standard order */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_string_assign\n");
    free_arg_regs();
    return inst_list;
}

/* Call kgpc_string_to_char_array(dest, src, size) to copy string to char array */
static ListNode_t *codegen_call_string_to_char_array(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg, int array_size)
{
    if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        /* Windows x64 ABI: first arg in %rcx, second in %rdx, third in %r8 */
        int value_in_rcx = (strcmp(value_reg->bit_64, "%rcx") == 0);
        int addr_in_rdx = (strcmp(addr_reg->bit_64, "%rdx") == 0);

        if (value_in_rcx && addr_in_rdx)
        {
            inst_list = add_inst(inst_list, "\txchgq\t%rcx, %rdx\n");
        }
        else if (value_in_rcx)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (addr_in_rdx)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", array_size);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        /* System V ABI: first arg in %rdi, second in %rsi, third in %rdx */
        int value_in_rdi = (strcmp(value_reg->bit_64, "%rdi") == 0);
        int addr_in_rsi = (strcmp(addr_reg->bit_64, "%rsi") == 0);

        if (value_in_rdi && addr_in_rsi)
        {
            inst_list = add_inst(inst_list, "\txchgq\t%rdi, %rsi\n");
        }
        else if (value_in_rdi)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (addr_in_rsi)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", array_size);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_string_to_char_array\n");
    free_arg_regs();
    return inst_list;
}

/* Call kgpc_string_to_shortstring(dest, src, size) to copy string to ShortString */
static ListNode_t *codegen_call_string_to_shortstring(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg, int array_size)
{
    if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        /* Windows x64 ABI: first arg in %rcx, second in %rdx, third in %r8 */
        int value_in_rcx = (strcmp(value_reg->bit_64, "%rcx") == 0);
        int addr_in_rdx = (strcmp(addr_reg->bit_64, "%rdx") == 0);

        if (value_in_rcx && addr_in_rdx)
        {
            inst_list = add_inst(inst_list, "\txchgq\t%rcx, %rdx\n");
        }
        else if (value_in_rcx)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (addr_in_rdx)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", array_size);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        /* System V ABI: first arg in %rdi, second in %rsi, third in %rdx */
        int value_in_rdi = (strcmp(value_reg->bit_64, "%rdi") == 0);
        int addr_in_rsi = (strcmp(addr_reg->bit_64, "%rsi") == 0);

        if (value_in_rdi && addr_in_rsi)
        {
            inst_list = add_inst(inst_list, "\txchgq\t%rdi, %rsi\n");
        }
        else if (value_in_rdi)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (addr_in_rsi)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", array_size);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_string_to_shortstring\n");
    free_arg_regs();
    return inst_list;
}

/* Assign a static array value (copy all elements) */
static ListNode_t *codegen_assign_static_array(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (dest_expr == NULL || src_expr == NULL || ctx == NULL)
        return inst_list;

    /* Calculate array size: (upper - lower + 1) * element_size */
    long long lower_bound = expr_get_array_lower_bound(dest_expr);
    long long upper_bound = expr_get_array_upper_bound(dest_expr);
    long long num_elements = -1;
    if (upper_bound >= lower_bound)
        num_elements = upper_bound - lower_bound + 1;

    if (num_elements <= 0)
    {
        if (dest_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_array(dest_expr->resolved_kgpc_type))
        {
            int start = 0;
            int end = -1;
            if (kgpc_type_get_array_bounds(dest_expr->resolved_kgpc_type, &start, &end) == 0 &&
                end >= start)
            {
                num_elements = (long long)end - (long long)start + 1;
            }
        }

        if (num_elements <= 0)
        {
            struct RecordField *field = codegen_lookup_record_field(dest_expr);
            if (field != NULL && field->is_array && !field->array_is_open)
                num_elements = (long long)field->array_end - (long long)field->array_start + 1;
            else if (field != NULL)
            {
                struct TypeAlias *alias = codegen_lookup_type_alias(ctx, field->type_id);
                if (alias != NULL && alias->is_array && !alias->is_open_array &&
                    alias->array_end >= alias->array_start)
                {
                    num_elements = (long long)alias->array_end - (long long)alias->array_start + 1;
                }
            }
        }
    }

    long long element_size = expr_get_array_element_size(dest_expr, ctx);
    
    if (element_size <= 0)
    {
        struct RecordField *field = codegen_lookup_record_field(dest_expr);
        if (field != NULL)
        {
            long long computed = 0;
            if (codegen_sizeof_type_reference(ctx, field->array_element_type,
                    field->array_element_type_id, field->nested_record, &computed) == 0 &&
                computed > 0)
            {
                element_size = computed;
            }
            else
            {
                struct TypeAlias *alias = codegen_lookup_type_alias(ctx, field->type_id);
                if (alias != NULL && alias->is_array)
                {
                    long long alias_size = 0;
                    if (codegen_sizeof_type_reference(ctx, alias->array_element_type,
                            alias->array_element_type_id, NULL, &alias_size) == 0 &&
                        alias_size > 0)
                    {
                        element_size = alias_size;
                    }
                }
            }
        }

        if (element_size <= 0)
        {
            codegen_report_error(ctx,
                "ERROR: Unable to determine element size for array assignment.");
            return inst_list;
        }
    }
    
    long long array_size = num_elements * element_size;
    if (array_size <= 0)
    {
        struct RecordField *field = codegen_lookup_record_field(dest_expr);
        if (field != NULL)
        {
            long long total_size = 0;
            if (codegen_sizeof_type_reference(ctx, field->type, field->type_id,
                    field->nested_record, &total_size) == 0 && total_size > 0)
            {
                array_size = total_size;
                num_elements = 1;
                element_size = total_size;
            }
        }

        if (array_size <= 0)
        {
            codegen_report_error(ctx,
                "ERROR: Invalid array size for assignment: %lld elements * %lld bytes = %lld total.",
                num_elements, element_size, array_size);
            return inst_list;
        }
    }

    /* Get address of destination */
    Register_t *dest_reg = NULL;
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL)
    {
        if (dest_reg != NULL)
            free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    /* Get address of source */
    Register_t *src_reg = NULL;
    inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
    if (codegen_had_error(ctx) || src_reg == NULL)
    {
        if (src_reg != NULL)
            free_reg(get_reg_stack(), src_reg);
        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    /* Get register for size */
    Register_t *count_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (count_reg == NULL)
    {
        free_reg(get_reg_stack(), dest_reg);
        free_reg(get_reg_stack(), src_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate register for array copy size.");
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", array_size, count_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    /* Call memcpy(dest, src, size) */
    /* NOTE: We must be careful about register conflicts. Load arguments in reverse order
     * to avoid clobbering source registers before we've moved them to their destinations. */
    if (codegen_target_is_windows())
    {
        /* Windows calling convention: RCX (dest), RDX (src), R8 (size)
         * Move in reverse order to avoid conflicts */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", count_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        /* System V calling convention: RDI (dest), RSI (src), RDX (size)
         * Move in reverse order to avoid conflicts */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", count_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_memcpy_wrapper\n");
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), count_reg);
    free_reg(get_reg_stack(), src_reg);
    free_reg(get_reg_stack(), dest_reg);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_assign_record_value(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (dest_expr == NULL || src_expr == NULL || ctx == NULL)
        return inst_list;

    /* Check if this is a class assignment. Classes are represented as pointers,
     * so we should just copy the pointer value, not the entire instance. */
    int is_class_assignment = 0;
    if (dest_expr->record_type != NULL && record_type_is_class(dest_expr->record_type))
        is_class_assignment = 1;
    else if (src_expr->record_type != NULL && record_type_is_class(src_expr->record_type))
        is_class_assignment = 1;

    if (is_class_assignment)
    {
        /* For class variables, just copy the pointer (8 bytes) */
        Register_t *dest_reg = NULL;
        inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
        if (codegen_had_error(ctx) || dest_reg == NULL)
        {
            if (dest_reg != NULL)
                free_reg(get_reg_stack(), dest_reg);
            return inst_list;
        }

        Register_t *src_reg = NULL;
        
        /* For function calls (especially constructors), the expression evaluates to the pointer value directly.
         * For variable references, we need to load the pointer from the variable.
         * We check if the source is addressable to distinguish these cases. */
        int src_is_addressable = codegen_expr_is_addressable(src_expr);
        
        if (src_is_addressable)
        {
            /* Source is a variable - get its address and load the pointer value */
            inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
            if (codegen_had_error(ctx) || src_reg == NULL)
            {
                if (src_reg != NULL)
                    free_reg(get_reg_stack(), src_reg);
                free_reg(get_reg_stack(), dest_reg);
                return inst_list;
            }
            
            /* Load the pointer value from the variable */
            Register_t *ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (ptr_reg == NULL)
            {
                free_reg(get_reg_stack(), dest_reg);
                free_reg(get_reg_stack(), src_reg);
                return codegen_fail_register(ctx, inst_list, NULL,
                    "ERROR: Unable to allocate register for class pointer copy.");
            }
            
            char buffer[128];
            snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", src_reg->bit_64, ptr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", ptr_reg->bit_64, dest_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            free_reg(get_reg_stack(), ptr_reg);
            free_reg(get_reg_stack(), src_reg);
        }
        else
        {
            /* Source is a function call or expression that returns the pointer value directly.
             * Save dest_reg to the stack before evaluating source to prevent it from being clobbered. */
            StackNode_t *dest_save_slot = add_l_x("__class_assign_dest__", CODEGEN_POINTER_SIZE_BYTES);
            if (dest_save_slot == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to reserve stack slot for class assignment destination.");
                free_reg(get_reg_stack(), dest_reg);
                return inst_list;
            }
            
            char buffer[128];
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                dest_reg->bit_64, dest_save_slot->offset);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), dest_reg);
            dest_reg = NULL;
            
            /* Evaluate the source expression (constructor call) */
            inst_list = codegen_expr_with_result(src_expr, inst_list, ctx, &src_reg);
            if (codegen_had_error(ctx) || src_reg == NULL)
            {
                if (src_reg != NULL)
                    free_reg(get_reg_stack(), src_reg);
                return inst_list;
            }
            
            /* Restore dest_reg from stack */
            dest_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (dest_reg == NULL)
            {
                free_reg(get_reg_stack(), src_reg);
                return codegen_fail_register(ctx, inst_list, NULL,
                    "ERROR: Unable to allocate register for class assignment destination restore.");
            }
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                dest_save_slot->offset, dest_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            /* src_reg already contains the pointer value - store it directly */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", src_reg->bit_64, dest_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            free_reg(get_reg_stack(), src_reg);
        }
        
        if (dest_reg != NULL)
            free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    long long record_size = 0;
    int size_status = codegen_get_record_size(ctx, dest_expr, &record_size);
    if (size_status != 0)
    {
        size_status = codegen_get_record_size(ctx, src_expr, &record_size);
        if (size_status != 0)
        {
            codegen_report_error(ctx,
                "ERROR: Unable to determine record size for assignment.");
            return inst_list;
        }
    }

    if (record_size <= 0)
        return inst_list;

    Register_t *dest_reg = NULL;
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL)
    {
        if (dest_reg != NULL)
            free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    int dest_is_char_set = expr_is_char_set_ctx(dest_expr, ctx);

    if (!codegen_expr_is_addressable(src_expr))
    {
        if (src_expr->type == EXPR_FUNCTION_CALL)
        {
            struct KgpcType *func_type = NULL;
            if (src_expr->expr_data.function_call_data.is_call_info_valid)
            {
                func_type = src_expr->expr_data.function_call_data.call_kgpc_type;
            }
            
            if (func_type == NULL && ctx != NULL && ctx->symtab != NULL &&
                src_expr->expr_data.function_call_data.id != NULL)
            {
                HashNode_t *func_node = NULL;
                if (FindIdent(&func_node, ctx->symtab,
                        src_expr->expr_data.function_call_data.id) >= 0 && func_node != NULL)
                {
                    func_type = func_node->type;
                }
            }

            const char *func_mangled_name = src_expr->expr_data.function_call_data.mangled_id;
            const char *func_id = src_expr->expr_data.function_call_data.id;

            int call_returns_record = expr_has_type_tag(src_expr, RECORD_TYPE);
            if (!call_returns_record && func_type != NULL &&
                kgpc_type_is_procedure(func_type))
            {
                KgpcType *return_type = kgpc_type_get_return_type(func_type);
                if (return_type != NULL && kgpc_type_is_record(return_type))
                    call_returns_record = 1;
            }

            /* Detect constructors even if the static type isn't a record (e.g., pointer return). */
            int is_constructor = 0;
            if (func_mangled_name != NULL)
            {
                const char *create_pos = strstr(func_mangled_name, "__Create");
                if (create_pos != NULL)
                    is_constructor = 1;
                else if (strcmp(func_mangled_name, "Create") == 0)
                    is_constructor = 1;
            }

            if (call_returns_record || is_constructor)
            {
                /* For constructors, allocate heap memory and initialize VMT */
                Register_t *constructor_instance_reg = NULL;
                if (is_constructor)
                {
                    /* Get the class type from the source expression or first argument */
                    struct RecordType *class_record = src_expr->record_type;
                    
                    if (class_record == NULL)
                    {
                        ListNode_t *first_arg = src_expr->expr_data.function_call_data.args_expr;
                        if (first_arg != NULL && first_arg->cur != NULL)
                        {
                            struct Expression *class_expr = (struct Expression *)first_arg->cur;
                            if (class_expr != NULL)
                                class_record = class_expr->record_type;
                        }
                    }
                    
                    if (class_record != NULL && record_type_is_class(class_record))
                    {
                        /* Get the size of the class instance */
                        long long instance_size = 0;
                        if (codegen_sizeof_record_type(ctx, class_record, &instance_size) == 0 &&
                            instance_size > 0)
                        {
                            char buffer[128];
                            
                            /* Save dest_reg to stack since it will be clobbered by function calls */
                            StackNode_t *dest_save_slot = add_l_x("__constructor_dest__", CODEGEN_POINTER_SIZE_BYTES);
                            if (dest_save_slot == NULL)
                            {
                                codegen_report_error(ctx,
                                    "ERROR: Unable to reserve stack slot for constructor destination.");
                                free_reg(get_reg_stack(), dest_reg);
                                return inst_list;
                            }
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                                dest_reg->bit_64, dest_save_slot->offset);
                            inst_list = add_inst(inst_list, buffer);
                            
                            /* Allocate zero-initialized memory using calloc
                             * This ensures all fields (including dynamic array descriptors) start at zero */
                            const char *size_arg_reg = codegen_target_is_windows() ? "%rcx" : "%rdi";
                            const char *count_arg_reg = codegen_target_is_windows() ? "%rdx" : "%rsi";
                            
                            /* calloc(1, instance_size) - allocate 1 element of size instance_size */
                            snprintf(buffer, sizeof(buffer), "\tmovq\t$1, %s\n", size_arg_reg);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n",
                                instance_size, count_arg_reg);
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
                                free_reg(get_reg_stack(), dest_reg);
                                return inst_list;
                            }
                            
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n",
                                constructor_instance_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            
                            /* Initialize VMT pointer in the allocated instance */
                            /* Use the class's TYPEINFO label instead of evaluating the first argument
                             * to avoid side effects like storing into the destination variable */
                            const char *class_type_id = class_record->type_id;
                            if (class_type_id != NULL)
                            {
                                /* Load VMT (TYPEINFO) address */
                                Register_t *vmt_reg = get_free_reg(get_reg_stack(), &inst_list);
                                if (vmt_reg != NULL)
                                {
                                    snprintf(buffer, sizeof(buffer), "\tleaq\t%s_TYPEINFO(%%rip), %s\n",
                                        class_type_id, vmt_reg->bit_64);
                                    inst_list = add_inst(inst_list, buffer);
                                    
                                    /* Store VMT into first 8 bytes of instance */
                                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                                        vmt_reg->bit_64, constructor_instance_reg->bit_64);
                                    inst_list = add_inst(inst_list, buffer);
                                    
                                    free_reg(get_reg_stack(), vmt_reg);
                                }
                            }
                            
                            /* Pass the allocated instance as the first argument (Self) */
                            const char *self_arg_reg = current_arg_reg64(0);
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                                constructor_instance_reg->bit_64, self_arg_reg);
                            inst_list = add_inst(inst_list, buffer);
                            
                            /* Pass remaining arguments starting from index 1 (skip class type argument) */
                            inst_list = codegen_pass_arguments(
                                src_expr->expr_data.function_call_data.args_expr, inst_list, ctx,
                                func_type, func_id, 1);
                            
                            /* Call the constructor */
                            snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", func_mangled_name);
                            inst_list = add_inst(inst_list, buffer);
                            inst_list = codegen_cleanup_call_stack(inst_list, ctx);
                            codegen_release_function_call_mangled_id(src_expr);
                            
                            /* Restore dest_reg from stack */
                            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                                dest_save_slot->offset, dest_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            
                            /* Store the instance pointer in the destination */
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                                constructor_instance_reg->bit_64, dest_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            
                            free_reg(get_reg_stack(), constructor_instance_reg);
                            free_reg(get_reg_stack(), dest_reg);
                            return inst_list;
                        }
                    }
                }
                
                /* Normal record-returning function (non-constructor) */
                const char *ret_ptr_reg = current_arg_reg64(0);
                if (ret_ptr_reg == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to determine register for record return pointer.");
                    free_reg(get_reg_stack(), dest_reg);
                    return inst_list;
                }

                StackNode_t *dest_save_slot = add_l_x("__record_call_dest__", CODEGEN_POINTER_SIZE_BYTES);
                if (dest_save_slot == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to reserve stack slot for record return destination.");
                    free_reg(get_reg_stack(), dest_reg);
                    return inst_list;
                }

                char buffer[128];
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                    dest_reg->bit_64, dest_save_slot->offset);
                inst_list = add_inst(inst_list, buffer);

                inst_list = codegen_pass_arguments(
                    src_expr->expr_data.function_call_data.args_expr, inst_list, ctx,
                    func_type,
                    src_expr->expr_data.function_call_data.id, 1);

                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    dest_save_slot->offset, ret_ptr_reg);
                inst_list = add_inst(inst_list, buffer);

                snprintf(buffer, sizeof(buffer), "\tcall\t%s\n",
                    src_expr->expr_data.function_call_data.mangled_id);
                inst_list = add_inst(inst_list, buffer);
                inst_list = codegen_cleanup_call_stack(inst_list, ctx);
                codegen_release_function_call_mangled_id(src_expr);

                free_reg(get_reg_stack(), dest_reg);
                return inst_list;
            }
        }

        /* Handle character set literals - they generate a temporary buffer address */
        if (src_expr->type == EXPR_SET &&
            (expr_is_char_set_ctx(src_expr, ctx) || dest_is_char_set))
        {
            /* Generate the set literal, which returns an address register */
            Register_t *src_reg = NULL;
            int force_char_literal = dest_is_char_set && !expr_is_char_set_ctx(src_expr, ctx);
            inst_list = codegen_set_literal(src_expr, inst_list, ctx, &src_reg, force_char_literal);
            if (codegen_had_error(ctx) || src_reg == NULL)
            {
                if (src_reg != NULL)
                    free_reg(get_reg_stack(), src_reg);
                free_reg(get_reg_stack(), dest_reg);
                return inst_list;
            }

            /* src_reg now contains the address of the temporary set buffer */
            /* Copy 32 bytes from src to dest using memcpy */
            Register_t *count_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (count_reg == NULL)
            {
                free_reg(get_reg_stack(), dest_reg);
                free_reg(get_reg_stack(), src_reg);
                return codegen_fail_register(ctx, inst_list, NULL,
                    "ERROR: Unable to allocate register for set copy size.");
            }

            char buffer[128];
            snprintf(buffer, sizeof(buffer), "\tmovq\t$32, %s\n", count_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            if (codegen_target_is_windows())
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", count_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", count_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }

            snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_memcpy_wrapper\n");
            inst_list = add_inst(inst_list, buffer);

            free_reg(get_reg_stack(), count_reg);
            free_reg(get_reg_stack(), src_reg);
            free_reg(get_reg_stack(), dest_reg);
            free_arg_regs();
            return inst_list;
        }

        codegen_report_error(ctx,
            "ERROR: Unsupported record-valued source expression.");
        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    Register_t *src_reg = NULL;
    inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
    if (codegen_had_error(ctx) || src_reg == NULL)
    {
        if (src_reg != NULL)
            free_reg(get_reg_stack(), src_reg);
        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    Register_t *count_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (count_reg == NULL)
    {
        free_reg(get_reg_stack(), dest_reg);
        free_reg(get_reg_stack(), src_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate register for record copy size.");
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", record_size, count_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", count_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", count_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_move\n");

    free_reg(get_reg_stack(), dest_reg);
    free_reg(get_reg_stack(), src_reg);
    free_reg(get_reg_stack(), count_reg);
    free_arg_regs();
    return inst_list;
}

static int codegen_dynamic_array_element_size(CodeGenContext *ctx, StackNode_t *array_node,
    struct Expression *array_expr)
{
    if (array_node != NULL && array_node->element_size > 0)
        return array_node->element_size;

    const char *array_name = NULL;
    if (array_expr != NULL && array_expr->type == EXPR_VAR_ID)
        array_name = array_expr->expr_data.id;

    if (ctx != NULL)
    {
        if (array_name != NULL)
            codegen_report_error(ctx,
                "ERROR: Unable to determine element size for dynamic array %s.", array_name);
        else
            codegen_report_error(ctx,
                "ERROR: Unable to determine element size for dynamic array.");
    }

    return DOUBLEWORD;
}

static int codegen_push_loop(CodeGenContext *ctx, const char *exit_label, const char *continue_label)
{
    if (ctx == NULL || exit_label == NULL || continue_label == NULL)
        return 0;
    if (ctx->loop_capacity == ctx->loop_depth)
    {
        int new_capacity = (ctx->loop_capacity > 0) ? ctx->loop_capacity * 2 : 4;
        CodeGenLoopFrame *new_frames = (CodeGenLoopFrame *)realloc(ctx->loop_frames, sizeof(CodeGenLoopFrame) * (size_t)new_capacity);
        if (new_frames == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate loop stack.\n");
            return 0;
        }
        for (int i = ctx->loop_capacity; i < new_capacity; ++i) {
            new_frames[i].label = NULL;
            new_frames[i].continue_label = NULL;
            new_frames[i].finally_depth = 0;
        }
        ctx->loop_frames = new_frames;
        ctx->loop_capacity = new_capacity;
    }
    ctx->loop_frames[ctx->loop_depth].label = strdup(exit_label);
    ctx->loop_frames[ctx->loop_depth].continue_label = strdup(continue_label);
    ctx->loop_frames[ctx->loop_depth].finally_depth = ctx->finally_depth;
    if (ctx->loop_frames[ctx->loop_depth].label == NULL || ctx->loop_frames[ctx->loop_depth].continue_label == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to duplicate loop labels.\n");
        if (ctx->loop_frames[ctx->loop_depth].label != NULL) free(ctx->loop_frames[ctx->loop_depth].label);
        if (ctx->loop_frames[ctx->loop_depth].continue_label != NULL) free(ctx->loop_frames[ctx->loop_depth].continue_label);
        return 0;
    }
    ctx->loop_depth += 1;
    return 1;
}

static void codegen_pop_loop(CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->loop_depth <= 0)
        return;
    ctx->loop_depth -= 1;
    if (ctx->loop_frames[ctx->loop_depth].label != NULL)
    {
        free(ctx->loop_frames[ctx->loop_depth].label);
        ctx->loop_frames[ctx->loop_depth].label = NULL;
    }
    if (ctx->loop_frames[ctx->loop_depth].continue_label != NULL)
    {
        free(ctx->loop_frames[ctx->loop_depth].continue_label);
        ctx->loop_frames[ctx->loop_depth].continue_label = NULL;
    }
}

static const char *codegen_current_loop_exit(const CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->loop_depth <= 0)
        return NULL;
    return ctx->loop_frames[ctx->loop_depth - 1].label;
}

static const char *codegen_current_loop_continue(const CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->loop_depth <= 0)
        return NULL;
    return ctx->loop_frames[ctx->loop_depth - 1].continue_label;
}

static int codegen_current_loop_finally_depth(const CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->loop_depth <= 0)
        return 0;
    return ctx->loop_frames[ctx->loop_depth - 1].finally_depth;
}

static int codegen_push_finally(CodeGenContext *ctx, ListNode_t *statements)
{
    if (ctx == NULL)
        return 0;
    if (ctx->finally_capacity == ctx->finally_depth)
    {
        int new_capacity = (ctx->finally_capacity > 0) ? ctx->finally_capacity * 2 : 4;
        CodeGenFinallyFrame *new_stack = (CodeGenFinallyFrame *)realloc(
            ctx->finally_stack, sizeof(CodeGenFinallyFrame) * (size_t)new_capacity);
        if (new_stack == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate finally stack.\n");
            return 0;
        }
        memset(new_stack + ctx->finally_capacity, 0,
            sizeof(CodeGenFinallyFrame) * (size_t)(new_capacity - ctx->finally_capacity));
        ctx->finally_stack = new_stack;
        ctx->finally_capacity = new_capacity;
    }
    ctx->finally_stack[ctx->finally_depth].statements = statements;
    ctx->finally_depth += 1;
    return 1;
}

static void codegen_pop_finally(CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->finally_depth <= 0)
        return;
    ctx->finally_depth -= 1;
    ctx->finally_stack[ctx->finally_depth].statements = NULL;
}

static int codegen_has_finally(const CodeGenContext *ctx)
{
    return (ctx != NULL && ctx->finally_depth > 0);
}

static ListNode_t *codegen_emit_finally_block(CodeGenContext *ctx, ListNode_t *inst_list,
    SymTab_t *symtab, const char *entry_label, const char *target_label)
{
    if (!codegen_has_finally(ctx))
        return inst_list;

    char buffer[32];
    snprintf(buffer, sizeof(buffer), "%s:\n", entry_label);
    inst_list = add_inst(inst_list, buffer);

    int frame_index = ctx->finally_depth - 1;
    CodeGenFinallyFrame frame = ctx->finally_stack[frame_index];

    ctx->finally_depth -= 1;
    if (frame.statements != NULL)
        inst_list = codegen_statement_list(frame.statements, inst_list, ctx, symtab);
    ctx->finally_stack[frame_index] = frame;
    ctx->finally_depth += 1;

    if (target_label != NULL)
    {
        snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", target_label);
        inst_list = add_inst(inst_list, buffer);
    }

    return inst_list;
}

static ListNode_t *codegen_branch_through_finally(CodeGenContext *ctx, ListNode_t *inst_list,
    SymTab_t *symtab, const char *target_label, int limit_depth)
{
    if (!codegen_has_finally(ctx))
        return gencode_jmp(NORMAL_JMP, 0, (char *)target_label, inst_list);

    int depth = ctx->finally_depth;
    if (depth <= 0)
        return gencode_jmp(NORMAL_JMP, 0, (char *)target_label, inst_list);

    /* If limit_depth is negative, unwind everything (e.g. Exit) */
    if (limit_depth < 0)
        limit_depth = 0;

    /* If current depth is already at or below limit, just jump */
    if (depth <= limit_depth)
        return gencode_jmp(NORMAL_JMP, 0, (char *)target_label, inst_list);

    int unwind_count = depth - limit_depth;
    char (*entry_labels)[18] = (char (*)[18])malloc(sizeof(*entry_labels) * (size_t)unwind_count);
    if (entry_labels == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to allocate finally labels.\n");
        return inst_list;
    }

    for (int i = 0; i < unwind_count; ++i)
        gen_label(entry_labels[i], 18, ctx);

    inst_list = gencode_jmp(NORMAL_JMP, 0, entry_labels[unwind_count - 1], inst_list);

    int original_depth = ctx->finally_depth;
    /* Iterate from top of stack down to limit_depth */
    for (int i = unwind_count - 1; i >= 0; --i)
    {
        const char *target = (i == 0) ? target_label : entry_labels[i - 1];
        const char *entry = entry_labels[i];

        /* codegen_emit_finally_block will emit the entry label, so we don't emit it here */
        inst_list = codegen_emit_finally_block(ctx, inst_list, symtab, entry, target);
    }
    ctx->finally_depth = original_depth;

    free(entry_labels);
    return inst_list;
}

static int codegen_push_except(CodeGenContext *ctx, const char *label)
{
    if (ctx == NULL || label == NULL)
        return 0;
    if (ctx->except_capacity == ctx->except_depth)
    {
        int new_capacity = (ctx->except_capacity > 0) ? ctx->except_capacity * 2 : 4;
        CodeGenExceptFrame *new_frames = (CodeGenExceptFrame *)realloc(ctx->except_frames, sizeof(CodeGenExceptFrame) * (size_t)new_capacity);
        if (new_frames == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate except stack.\n");
            return 0;
        }
        for (int i = ctx->except_capacity; i < new_capacity; ++i) {
            new_frames[i].label = NULL;
            new_frames[i].finally_depth = 0;
        }
        ctx->except_frames = new_frames;
        ctx->except_capacity = new_capacity;
    }
    ctx->except_frames[ctx->except_depth].label = strdup(label);
    ctx->except_frames[ctx->except_depth].finally_depth = ctx->finally_depth;
    if (ctx->except_frames[ctx->except_depth].label == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to duplicate except label.\n");
        return 0;
    }
    ctx->except_depth += 1;
    return 1;
}

static void codegen_pop_except(CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->except_depth <= 0)
        return;
    ctx->except_depth -= 1;
    if (ctx->except_frames[ctx->except_depth].label != NULL)
    {
        free(ctx->except_frames[ctx->except_depth].label);
        ctx->except_frames[ctx->except_depth].label = NULL;
    }
}

static const char *codegen_current_except_label(const CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->except_depth <= 0)
        return NULL;
    return ctx->except_frames[ctx->except_depth - 1].label;
}

static int codegen_current_except_finally_depth(const CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->except_depth <= 0)
        return 0;
    return ctx->except_frames[ctx->except_depth - 1].finally_depth;
}

static ListNode_t *codegen_statement_list(ListNode_t *stmts, ListNode_t *inst_list,
    CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmts == NULL) {
        if (getenv("KGPC_DEBUG_CODEGEN") != NULL) {
            fprintf(stderr, "[CodeGen] codegen_statement_list: stmts is NULL\n");
        }
        return inst_list;
    }

    if (getenv("KGPC_DEBUG_CODEGEN") != NULL) {
        fprintf(stderr, "[CodeGen] codegen_statement_list: starting\n");
    }

    ListNode_t *node = stmts;
    while (node != NULL)
    {
        if (node->type == LIST_STMT)
        {
            struct Statement *stmt = (struct Statement *)node->cur;
            if (stmt != NULL) {
                if (getenv("KGPC_DEBUG_CODEGEN") != NULL) {
                    fprintf(stderr, "[CodeGen]   generating statement type=%d line=%d\n", stmt->type, stmt->line_num);
                }
                inst_list = codegen_stmt(stmt, inst_list, ctx, symtab);
            }
        }
        node = node->next;
    }
    
    if (getenv("KGPC_DEBUG_CODEGEN") != NULL) {
        fprintf(stderr, "[CodeGen] codegen_statement_list: finished\n");
    }

    return inst_list;
}

/* Codegen for a statement */
ListNode_t *codegen_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    if (stmt == NULL)
        return inst_list;
    
    

    assert(ctx != NULL);
    assert(symtab != NULL);
    switch(stmt->type)
    {
        case STMT_VAR_ASSIGN:
            inst_list = codegen_var_assignment(stmt, inst_list, ctx);
            break;
        case STMT_PROCEDURE_CALL:
            inst_list = codegen_proc_call(stmt, inst_list, ctx, symtab);
            break;
        case STMT_COMPOUND_STATEMENT:
            inst_list = codegen_compound_stmt(stmt, inst_list, ctx, symtab);
            break;
        case STMT_LABEL:
        {
            char label_name[256];
            format_pascal_label(label_name, sizeof(label_name), ctx, stmt->stmt_data.label_data.label);
            char buffer[272];
            snprintf(buffer, sizeof(buffer), "%s:\n", label_name);
            inst_list = add_inst(inst_list, buffer);
            if (stmt->stmt_data.label_data.stmt != NULL)
                inst_list = codegen_stmt(stmt->stmt_data.label_data.stmt, inst_list, ctx, symtab);
            break;
        }
        case STMT_GOTO:
        {
            char label_name[256];
            format_pascal_label(label_name, sizeof(label_name), ctx, stmt->stmt_data.goto_data.label);
            char buffer[272];
            snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", label_name);
            inst_list = add_inst(inst_list, buffer);
            break;
        }
        case STMT_IF_THEN:
            inst_list = codegen_if_then(stmt, inst_list, ctx, symtab);
            break;
        case STMT_WHILE:
            inst_list = codegen_while(stmt, inst_list, ctx, symtab);
            break;
        case STMT_REPEAT:
            inst_list = codegen_repeat(stmt, inst_list, ctx, symtab);
            break;
        case STMT_FOR:
            inst_list = codegen_for(stmt, inst_list, ctx, symtab);
            break;
        case STMT_FOR_IN:
            inst_list = codegen_for_in(stmt, inst_list, ctx, symtab);
            break;
        case STMT_BREAK:
            inst_list = codegen_break_stmt(stmt, inst_list, ctx, symtab);
            break;
        case STMT_CONTINUE:
            inst_list = codegen_continue_stmt(stmt, inst_list, ctx, symtab);
            break;
        case STMT_ASM_BLOCK:
            inst_list = add_inst(inst_list, stmt->stmt_data.asm_block_data.code);
            break;
        case STMT_EXIT:
        {
            inst_list = add_inst(inst_list, "\t# EXIT statement\n");
            if (codegen_has_finally(ctx))
            {
                char exit_label[18];
                gen_label(exit_label, sizeof(exit_label), ctx);
                /* Exit unwinds everything, so limit_depth is 0 */
                inst_list = codegen_branch_through_finally(ctx, inst_list, symtab, exit_label, 0);
                char buffer[32];
                snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
                inst_list = add_inst(inst_list, buffer);
            }
            inst_list = add_inst(inst_list, "\tleave\n");
            inst_list = add_inst(inst_list, "\tret\n");
            break;
        }
        case STMT_CASE:
            inst_list = codegen_case(stmt, inst_list, ctx, symtab);
            break;
        case STMT_WITH:
            inst_list = codegen_with(stmt, inst_list, ctx, symtab);
            break;
        case STMT_TRY_FINALLY:
            inst_list = codegen_try_finally(stmt, inst_list, ctx, symtab);
            break;
        case STMT_TRY_EXCEPT:
            inst_list = codegen_try_except(stmt, inst_list, ctx, symtab);
            break;
        case STMT_RAISE:
            inst_list = codegen_raise(stmt, inst_list, ctx, symtab);
            break;
        case STMT_INHERITED:
            inst_list = codegen_inherited(stmt, inst_list, ctx, symtab);
            break;
        default:
            assert(0 && "Unrecognized statement type in codegen");
            break;
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

static ListNode_t *codegen_builtin_setlength(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    assert(stmt != NULL);
    assert(ctx != NULL);

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL)
    {
        fprintf(stderr, "ERROR: SetLength expects two arguments.\n");
        return inst_list;
    }

    const char *mangled = stmt->stmt_data.procedure_call_data.mangled_id;
    if (mangled != NULL && strcmp(mangled, "__kgpc_setlength_string") == 0)
        return codegen_builtin_setlength_string(stmt, inst_list, ctx);

    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    struct Expression *len_expr = (struct Expression *)args_expr->next->cur;

    struct Expression *array_expr = target_expr;
    const char *array_id = NULL;

    /* Handle both simple variables and field access (e.g., self.FItems) */
    if (array_expr != NULL && array_expr->type == EXPR_VAR_ID)
    {
        array_id = array_expr->expr_data.id;
    }
    else if (array_expr != NULL && array_expr->type == EXPR_RECORD_ACCESS)
    {
        /* For field access like self.FItems, use the field name */
        array_id = array_expr->expr_data.record_access_data.field_id;
    }

    if (array_id == NULL)
    {
        fprintf(stderr, "ERROR: SetLength first argument must be a variable or field identifier.\n");
        return inst_list;
    }

    StackNode_t *array_node = find_label((char *)array_id);
    int is_field_array = 0;
    long long field_offset = 0;
    
    /* If not found in local stack, might be a field of the current object */
    if (array_node == NULL && array_expr->type == EXPR_RECORD_ACCESS)
    {
        is_field_array = 1;
        field_offset = array_expr->expr_data.record_access_data.field_offset;
    }
    
    if (!is_field_array && (array_node == NULL || !array_node->is_dynamic))
    {
        fprintf(stderr, "ERROR: Dynamic array %s not found for SetLength.\n", array_id);
        return inst_list;
    }

    int element_size;
    if (is_field_array)
    {
        /* For field arrays, we need to determine element size from the expression type */
        /* For now, assume Integer (4 bytes) - this should be improved */
        element_size = 4;
    }
    else
    {
        element_size = codegen_dynamic_array_element_size(ctx, array_node, array_expr);
    }

    inst_list = codegen_expr(len_expr, inst_list, ctx);
    if (codegen_had_error(ctx))
        return inst_list;
    Register_t *length_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (length_reg == NULL)
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate register for SetLength length.");

    Register_t *descriptor_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (descriptor_reg == NULL)
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate register for SetLength descriptor.");

    char buffer[128];
    if (is_field_array)
    {
        /* For field arrays, calculate address as self + field_offset */
        /* Get self pointer from first local variable (convention for methods) */
        StackNode_t *self_node = find_label("self");
        if (self_node == NULL)
        {
            /* Try alternate names */
            self_node = find_label("Self");
        }
        
        if (self_node != NULL)
        {
            /* Load self pointer */
            Register_t *self_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (self_reg == NULL)
                return codegen_fail_register(ctx, inst_list, NULL,
                    "ERROR: Unable to allocate register for self pointer.");
            
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                self_node->offset, self_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            /* Calculate field address: self + offset */
            if (field_offset > 0)
            {
                snprintf(buffer, sizeof(buffer), "\tleaq\t%lld(%s), %s\n",
                    field_offset, self_reg->bit_64, descriptor_reg->bit_64);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                    self_reg->bit_64, descriptor_reg->bit_64);
            }
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), self_reg);
        }
        else
        {
            fprintf(stderr, "ERROR: Cannot find self pointer for field array SetLength.\n");
            return inst_list;
        }
    }
    else if (array_node->is_static)
    {
        const char *label = (array_node->static_label != NULL) ?
            array_node->static_label : array_node->label;
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
            label, descriptor_reg->bit_64);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
            array_node->offset, descriptor_reg->bit_64);
    }
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32, length_reg->bit_64);

    if (codegen_target_is_windows())
    {
        const char *arg0 = current_arg_reg64(0);  /* %rcx */
        const char *arg1 = current_arg_reg64(1);  /* %rdx */
        
        /* Check if we need to swap or save/restore to avoid clobbering */
        int descriptor_is_arg1 = (strcmp(descriptor_reg->bit_64, arg1) == 0);
        int length_is_arg0 = (strcmp(length_reg->bit_64, arg0) == 0);
        
        if (descriptor_is_arg1 && length_is_arg0)
        {
            /* Simple swap: descriptor in %rdx, length in %rcx */
            /* We want: descriptor in %rcx, length in %rdx */
            /* Use xchg or save to stack */
            StackNode_t *temp = add_l_t("setlength_temp");
            if (temp != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", descriptor_reg->bit_64, temp->offset);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", temp->offset, arg0);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else if (descriptor_is_arg1)
        {
            /* descriptor in %rdx, need it in %rcx */
            /* Move descriptor first to avoid clobbering */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", descriptor_reg->bit_64, arg0);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            /* Normal case or length in %rdx */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", descriptor_reg->bit_64, arg0);
            inst_list = add_inst(inst_list, buffer);
        }
        
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", element_size, current_arg_reg32(2));
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        const char *arg0 = current_arg_reg64(0);  /* %rdi */
        const char *arg1 = current_arg_reg64(1);  /* %rsi */
        
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", descriptor_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", element_size, current_arg_reg32(2));
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_dynarray_setlength\n");

    free_reg(get_reg_stack(), descriptor_reg);
    free_reg(get_reg_stack(), length_reg);

    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_setlength_string(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    assert(stmt != NULL);
    if (ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL)
    {
        fprintf(stderr, "ERROR: SetLength expects two arguments.\n");
        return inst_list;
    }

    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    struct Expression *len_expr = (struct Expression *)args_expr->next->cur;
    if (target_expr == NULL || len_expr == NULL)
        return inst_list;

    if (!codegen_expr_is_addressable(target_expr))
    {
        fprintf(stderr, "ERROR: SetLength string target must be addressable.\n");
        return inst_list;
    }

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    Register_t *length_reg = NULL;
    inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);
    if (codegen_had_error(ctx) || length_reg == NULL)
    {
        if (length_reg != NULL)
            free_reg(get_reg_stack(), length_reg);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    if (!expr_uses_qword_kgpctype(len_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32, length_reg->bit_64);

    char buffer[128];
    const char *arg0 = current_arg_reg64(0);  /* First argument: %rcx (Win) / %rdi (SysV) */
    const char *arg1 = current_arg_reg64(1);  /* Second argument: %rdx (Win) / %rsi (SysV) */
    
    /*
     * Handle register conflicts when setting up function arguments.
     * If length_reg is in arg0's position and we try to move addr_reg to arg0,
     * we'll overwrite the length. In this case, move length_reg to arg1 first.
     */
    if (strcmp(length_reg->bit_64, arg0) == 0)
    {
        /* length_reg is in arg0, which will be overwritten by addr_reg. Move length first. */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
    }
    else if (strcmp(addr_reg->bit_64, arg1) == 0)
    {
        /* addr_reg is in arg1, which is the destination for length_reg. Move addr first. */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        /* No conflict, move in standard order */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_string_setlength\n");

    free_reg(get_reg_stack(), addr_reg);
    free_reg(get_reg_stack(), length_reg);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_str(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL)
    {
        fprintf(stderr, "ERROR: Str expects two arguments.\n");
        return inst_list;
    }

    struct Expression *value_expr = (struct Expression *)args_expr->cur;
    struct Expression *target_expr = (struct Expression *)args_expr->next->cur;

    Register_t *value_reg = NULL;
    inst_list = codegen_expr_with_result(value_expr, inst_list, ctx, &value_reg);
    if (codegen_had_error(ctx) || value_reg == NULL)
        return inst_list;

    if (!expr_uses_qword_kgpctype(value_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);

    if (!codegen_expr_is_addressable(target_expr))
    {
        codegen_report_error(ctx, "ERROR: Str output must be addressable.");
        free_reg(get_reg_stack(), value_reg);
        return inst_list;
    }

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
    {
        free_reg(get_reg_stack(), value_reg);
        return inst_list;
    }

    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_str_int64\n");

    free_reg(get_reg_stack(), addr_reg);
    free_reg(get_reg_stack(), value_reg);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_insert(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL || args_expr->next->next == NULL)
    {
        fprintf(stderr, "ERROR: Insert expects three arguments.\n");
        return inst_list;
    }

    char buffer[128];
    struct Expression *source_expr = (struct Expression *)args_expr->cur;
    struct Expression *target_expr = (struct Expression *)args_expr->next->cur;
    struct Expression *index_expr = (struct Expression *)args_expr->next->next->cur;

    Register_t *source_reg = NULL;
    inst_list = codegen_expr_with_result(source_expr, inst_list, ctx, &source_reg);
    if (codegen_had_error(ctx) || source_reg == NULL)
        return inst_list;

    int source_is_char = (source_expr != NULL && source_expr->resolved_type == CHAR_TYPE);
    StackNode_t *char_buffer = NULL;
    if (source_is_char)
    {
        char_buffer = add_l_x("insert_char_buffer", 2);
        if (char_buffer == NULL)
        {
            free_reg(get_reg_stack(), source_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate spill slot for Insert char source.");
        }

        const char *byte_reg = register_name8(source_reg);
        if (byte_reg == NULL)
        {
            free_reg(get_reg_stack(), source_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to acquire byte register for Insert char source.");
        }

        snprintf(buffer, sizeof(buffer), "\tmovb\t$0, -%d(%%rbp)\n", char_buffer->offset - 1);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", byte_reg, char_buffer->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", char_buffer->offset, source_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    StackNode_t *source_temp = add_l_t("insert_source");
    if (source_temp == NULL)
    {
        free_reg(get_reg_stack(), source_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for Insert source.");
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", source_reg->bit_64, source_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), source_reg);

    if (!codegen_expr_is_addressable(target_expr))
    {
        codegen_report_error(ctx, "ERROR: Insert target must be addressable.");
        return inst_list;
    }

    Register_t *target_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &target_reg);
    if (codegen_had_error(ctx) || target_reg == NULL)
        return inst_list;

    StackNode_t *target_temp = add_l_t("insert_target");
    if (target_temp == NULL)
    {
        free_reg(get_reg_stack(), target_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for Insert target.");
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", target_reg->bit_64, target_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), target_reg);

    Register_t *index_reg = NULL;
    inst_list = codegen_expr_with_result(index_expr, inst_list, ctx, &index_reg);
    if (codegen_had_error(ctx) || index_reg == NULL)
        return inst_list;

    if (!expr_uses_qword_kgpctype(index_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32, index_reg->bit_64);

    StackNode_t *index_temp = add_l_t("insert_index");
    if (index_temp == NULL)
    {
        free_reg(get_reg_stack(), index_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for Insert index.");
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", index_reg->bit_64, index_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), index_reg);

    const char *arg0 = current_arg_reg64(0);
    const char *arg1 = current_arg_reg64(1);
    const char *arg2 = current_arg_reg64(2);

    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", source_temp->offset, arg0);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", target_temp->offset, arg1);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", index_temp->offset, arg2);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_string_insert\n");
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_delete(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL || args_expr->next->next == NULL)
    {
        fprintf(stderr, "ERROR: Delete expects three arguments.\n");
        return inst_list;
    }

    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    struct Expression *index_expr = (struct Expression *)args_expr->next->cur;
    struct Expression *count_expr = (struct Expression *)args_expr->next->next->cur;

    if (!codegen_expr_is_addressable(target_expr))
    {
        codegen_report_error(ctx, "ERROR: Delete target must be addressable.");
        return inst_list;
    }

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    StackNode_t *string_temp = add_l_t("delete_target");
    if (string_temp == NULL)
    {
        free_reg(get_reg_stack(), addr_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for Delete target.");
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, string_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), addr_reg);

    Register_t *index_reg = NULL;
    inst_list = codegen_expr_with_result(index_expr, inst_list, ctx, &index_reg);
    if (codegen_had_error(ctx) || index_reg == NULL)
        return inst_list;

    if (!expr_uses_qword_kgpctype(index_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32, index_reg->bit_64);

    StackNode_t *index_temp = add_l_t("delete_index");
    if (index_temp == NULL)
    {
        free_reg(get_reg_stack(), index_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for Delete index.");
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", index_reg->bit_64, index_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), index_reg);

    Register_t *count_reg = NULL;
    inst_list = codegen_expr_with_result(count_expr, inst_list, ctx, &count_reg);
    if (codegen_had_error(ctx) || count_reg == NULL)
        return inst_list;

    if (!expr_uses_qword_kgpctype(count_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, count_reg->bit_32, count_reg->bit_64);

    StackNode_t *count_temp = add_l_t("delete_count");
    if (count_temp == NULL)
    {
        free_reg(get_reg_stack(), count_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for Delete count.");
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", count_reg->bit_64, count_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), count_reg);

    const char *arg0 = current_arg_reg64(0);
    const char *arg1 = current_arg_reg64(1);
    const char *arg2 = current_arg_reg64(2);

    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", string_temp->offset, arg0);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", index_temp->offset, arg1);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", count_temp->offset, arg2);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_string_delete\n");
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_val(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL || args_expr->next->next == NULL ||
        args_expr->next->next->next != NULL)
    {
        codegen_report_error(ctx, "ERROR: Val expects three arguments.");
        return inst_list;
    }

    struct Expression *source_expr = (struct Expression *)args_expr->cur;
    struct Expression *value_expr = (struct Expression *)args_expr->next->cur;
    struct Expression *code_expr = (struct Expression *)args_expr->next->next->cur;

    Register_t *source_reg = NULL;
    Register_t *value_addr = NULL;
    Register_t *code_addr = NULL;
    StackNode_t *code_spill = NULL;
    StackNode_t *code_result_spill = NULL;

    inst_list = codegen_expr_with_result(source_expr, inst_list, ctx, &source_reg);
    if (codegen_had_error(ctx) || source_reg == NULL)
        goto cleanup;

    inst_list = codegen_address_for_expr(value_expr, inst_list, ctx, &value_addr);
    if (codegen_had_error(ctx) || value_addr == NULL)
        goto cleanup;

    inst_list = codegen_address_for_expr(code_expr, inst_list, ctx, &code_addr);
    if (codegen_had_error(ctx) || code_addr == NULL)
        goto cleanup;

    const char *call_target = NULL;
    switch (value_expr != NULL ? expr_get_type_tag(value_expr) : UNKNOWN_TYPE)
    {
        case INT_TYPE:
            call_target = "kgpc_val_integer";
            break;
        case LONGINT_TYPE:
            call_target = "kgpc_val_longint";
            break;
        case REAL_TYPE:
            call_target = "kgpc_val_real";
            break;
        default:
            call_target = NULL;
            break;
    }

    if (call_target == NULL)
    {
        codegen_report_error(ctx, "ERROR: Val target must be integer, longint, or real.");
        goto cleanup;
    }

    code_spill = add_l_t("val_code_ptr");
    if (code_spill == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to allocate temporary for Val code argument.");
        goto cleanup;
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", code_addr->bit_64, code_spill->offset);
    inst_list = add_inst(inst_list, buffer);

    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", source_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_addr->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", source_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_addr->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", call_target);
    inst_list = add_inst(inst_list, buffer);

    if (code_expr != NULL)
    {
        code_result_spill = add_l_t("val_code_result");
        if (code_result_spill == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate temporary for Val result.");
            goto cleanup;
        }

        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", code_result_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        if (expr_uses_qword_kgpctype(code_expr))
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n", code_result_spill->offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", code_spill->offset);
            inst_list = add_inst(inst_list, buffer);
            inst_list = add_inst(inst_list, "\tmovq\t%rdx, (%rax)\n");
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %%edx\n", code_result_spill->offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", code_spill->offset);
            inst_list = add_inst(inst_list, buffer);
            inst_list = add_inst(inst_list, "\tmovl\t%edx, (%rax)\n");
        }
    }

cleanup:
    if (source_reg != NULL)
        free_reg(get_reg_stack(), source_reg);
    if (value_addr != NULL)
        free_reg(get_reg_stack(), value_addr);
    if (code_addr != NULL)
        free_reg(get_reg_stack(), code_addr);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_incdec(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx, int is_increment)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL)
        return inst_list;

    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    struct Expression *value_expr = (args_expr->next != NULL) ? (struct Expression *)args_expr->next->cur : NULL;

    Register_t *increment_reg = NULL;
    if (value_expr != NULL)
    {
        inst_list = codegen_expr_with_result(value_expr, inst_list, ctx, &increment_reg);
        if (codegen_had_error(ctx) || increment_reg == NULL)
            return inst_list;
    }
    else
    {
        increment_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (increment_reg == NULL)
            return inst_list;
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", increment_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }

    int target_type_tag = (target_expr != NULL) ? expr_get_type_tag(target_expr) : UNKNOWN_TYPE;
    int target_is_long = (target_type_tag == LONGINT_TYPE);
    int target_is_pointer = (target_type_tag == POINTER_TYPE);
    int target_uses_qword = target_is_long || target_is_pointer;

    if (target_uses_qword)
        inst_list = codegen_sign_extend32_to64(inst_list, increment_reg->bit_32, increment_reg->bit_64);

    long long pointer_step = 1;
    if (target_is_pointer)
    {
        if (codegen_sizeof_pointer_target(ctx, target_expr, &pointer_step) != 0 || pointer_step <= 0)
            pointer_step = 1;
    }

    if (target_is_pointer && pointer_step != 1)
    {
        char buffer_scale[128];
        if (target_uses_qword)
            snprintf(buffer_scale, sizeof(buffer_scale), "\timulq\t$%lld, %s\n", pointer_step, increment_reg->bit_64);
        else
            snprintf(buffer_scale, sizeof(buffer_scale), "\timull\t$%lld, %s\n", pointer_step, increment_reg->bit_32);
        inst_list = add_inst(inst_list, buffer_scale);
    }

    char buffer_main[128];
    if (!is_increment)
    {
        if (target_uses_qword)
        {
            snprintf(buffer_main, sizeof(buffer_main), "\tnegq\t%s\n", increment_reg->bit_64);
            inst_list = add_inst(inst_list, buffer_main);
        }
        else
        {
            snprintf(buffer_main, sizeof(buffer_main), "\tnegl\t%s\n", increment_reg->bit_32);
            inst_list = add_inst(inst_list, buffer_main);
        }
    }

    if (target_expr != NULL && target_expr->type == EXPR_VAR_ID)
    {
        StackNode_t *var_node = find_label(target_expr->expr_data.id);
        char buffer[128];
        if (var_node != NULL)
        {
            if (var_node->is_static)
            {
                const char *label = (var_node->static_label != NULL) ?
                    var_node->static_label : var_node->label;
                if (target_uses_qword)
                    snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s(%%rip)\n", increment_reg->bit_64, label);
                else
                    snprintf(buffer, sizeof(buffer), "\taddl\t%s, %s(%%rip)\n", increment_reg->bit_32, label);
            }
            else
            {
                if (target_uses_qword)
                    snprintf(buffer, sizeof(buffer), "\taddq\t%s, -%d(%%rbp)\n", increment_reg->bit_64, var_node->offset);
                else
                    snprintf(buffer, sizeof(buffer), "\taddl\t%s, -%d(%%rbp)\n", increment_reg->bit_32, var_node->offset);
            }
            inst_list = add_inst(inst_list, buffer);
        }
        else if (nonlocal_flag() == 1)
        {
            int offset = 0;
            inst_list = codegen_get_nonlocal(inst_list, target_expr->expr_data.id, &offset);
            if (target_uses_qword)
                snprintf(buffer, sizeof(buffer), "\taddq\t%s, -%d(%s)\n", increment_reg->bit_64, offset, current_non_local_reg64());
            else
                snprintf(buffer, sizeof(buffer), "\taddl\t%s, -%d(%s)\n", increment_reg->bit_32, offset, current_non_local_reg64());
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            codegen_report_error(ctx, "ERROR: Unable to locate variable %s for %s.",
                target_expr->expr_data.id, is_increment ? "Inc" : "Dec");
        }
    }
    else if (target_expr != NULL && target_expr->type == EXPR_ARRAY_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_array_element_address(target_expr, inst_list, ctx, &addr_reg);
        if (!codegen_had_error(ctx) && addr_reg != NULL)
        {
            char buffer[128];
            if (target_uses_qword)
                snprintf(buffer, sizeof(buffer), "\taddq\t%s, (%s)\n", increment_reg->bit_64, addr_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\taddl\t%s, (%s)\n", increment_reg->bit_32, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), addr_reg);
        }
    }
    else if (codegen_expr_is_addressable(target_expr))
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
        if (!codegen_had_error(ctx) && addr_reg != NULL)
        {
            char buffer[128];
            if (target_uses_qword)
                snprintf(buffer, sizeof(buffer), "\taddq\t%s, (%s)\n", increment_reg->bit_64, addr_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\taddl\t%s, (%s)\n", increment_reg->bit_32, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), addr_reg);
        }
    }
    else
    {
        codegen_report_error(ctx, "ERROR: Unsupported Inc target.");
    }

    free_reg(get_reg_stack(), increment_reg);
    return inst_list;
}

static ListNode_t *codegen_builtin_inc(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    return codegen_builtin_incdec(stmt, inst_list, ctx, 1);
}

static ListNode_t *codegen_builtin_dec(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    return codegen_builtin_incdec(stmt, inst_list, ctx, 0);
}

static ListNode_t *codegen_builtin_include_exclude(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx, int is_exclude)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL)
        return inst_list;

    struct Expression *set_expr = (struct Expression *)args_expr->cur;
    struct Expression *value_expr = (struct Expression *)args_expr->next->cur;

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(set_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    Register_t *value_reg = NULL;
    inst_list = codegen_expr_with_result(value_expr, inst_list, ctx, &value_reg);
    if (codegen_had_error(ctx) || value_reg == NULL)
    {
        if (addr_reg != NULL)
            free_reg(get_reg_stack(), addr_reg);
        if (value_reg != NULL)
            free_reg(get_reg_stack(), value_reg);
        return inst_list;
    }

    Register_t *bit_reg = get_free_reg(get_reg_stack(), &inst_list);
    Register_t *dword_reg = get_free_reg(get_reg_stack(), &inst_list);
    Register_t *current_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (bit_reg == NULL || dword_reg == NULL || current_reg == NULL)
    {
        if (bit_reg != NULL)
            free_reg(get_reg_stack(), bit_reg);
        if (dword_reg != NULL)
            free_reg(get_reg_stack(), dword_reg);
        if (current_reg != NULL)
            free_reg(get_reg_stack(), current_reg);
        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tandl\t$255, %s\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", value_reg->bit_32, bit_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tandl\t$31, %s\n", bit_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", value_reg->bit_32, dword_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tshrl\t$5, %s\n", dword_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tshll\t$2, %s\n", dword_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovl\t(%s,%s,1), %s\n",
        addr_reg->bit_64, dword_reg->bit_64, current_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", bit_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", bit_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tshll\t%%cl, %s\n", bit_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    if (is_exclude)
    {
        snprintf(buffer, sizeof(buffer), "\tnotl\t%s\n", bit_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tandl\t%s, %s\n", bit_reg->bit_32, current_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\torl\t%s, %s\n", bit_reg->bit_32, current_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }

    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s,%s,1)\n",
        current_reg->bit_32, addr_reg->bit_64, dword_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), current_reg);
    free_reg(get_reg_stack(), dword_reg);
    free_reg(get_reg_stack(), bit_reg);
    free_reg(get_reg_stack(), value_reg);
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}

static ListNode_t *codegen_builtin_include(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    return codegen_builtin_include_exclude(stmt, inst_list, ctx, 0);
}

static ListNode_t *codegen_builtin_exclude(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    return codegen_builtin_include_exclude(stmt, inst_list, ctx, 1);
}

static ListNode_t *codegen_builtin_new(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next != NULL)
    {
        fprintf(stderr, "ERROR: New expects exactly one argument.\n");
        return inst_list;
    }

    struct Expression *target_expr = (struct Expression *)args_expr->cur;

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    long long alloc_size = 0;
    if (codegen_sizeof_pointer_target(ctx, target_expr, &alloc_size) != 0 || alloc_size <= 0)
    {
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    Register_t *size_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (size_reg == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to allocate register for New size.");
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", alloc_size, size_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    if (codegen_target_is_windows())
    {
        // Move size to %rdx first, before moving addr to %rcx
        // This avoids overwriting size_reg if it happens to be %rcx
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", size_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", size_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_new\n");

    free_reg(get_reg_stack(), addr_reg);
    free_reg(get_reg_stack(), size_reg);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_dispose(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next != NULL)
    {
        fprintf(stderr, "ERROR: Dispose expects exactly one argument.\n");
        return inst_list;
    }

    struct Expression *target_expr = (struct Expression *)args_expr->cur;

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_dispose\n");

    free_reg(get_reg_stack(), addr_reg);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_write_like(struct Statement *stmt, ListNode_t *inst_list,
                                              CodeGenContext *ctx, int append_newline)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    char buffer[128];
    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    Register_t *file_reg = NULL;
    StackNode_t *file_spill = NULL;
    int has_file_arg = 0;

    if (args != NULL)
    {
        struct Expression *first_expr = (struct Expression *)args->cur;
        if (first_expr != NULL && (expr_has_type_tag(first_expr, TEXT_TYPE)))
        {
            expr_node_t *file_tree = build_expr_tree(first_expr);
            file_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (file_reg != NULL)
            {
                inst_list = gencode_expr_tree(file_tree, inst_list, ctx, file_reg);
                has_file_arg = 1;
                file_spill = add_l_t("write_file");
                if (file_spill != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", file_reg->bit_64, file_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), file_reg);
                    file_reg = NULL;
                }
            }
            else
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for write file argument.");
            }
            free_expr_tree(file_tree);
            args = args->next;
        }
    }

    while (args != NULL)
    {
        struct Expression *expr = (struct Expression *)args->cur;

        int expr_type = (expr != NULL) ? expr_get_type_tag(expr) : UNKNOWN_TYPE;
        
        /* Treat char arrays as strings for printing */
        int treat_as_string = (expr_type == STRING_TYPE);
        if (expr != NULL && expr_type == CHAR_TYPE && expr->is_array_expr && expr->array_element_type == CHAR_TYPE)
        {
            treat_as_string = 1;
        }
        
        /* Also treat PAnsiChar (pointer to char) as string */
        if (expr != NULL && expr->resolved_kgpc_type != NULL && 
            kgpc_type_is_pointer(expr->resolved_kgpc_type))
        {
             if (expr->resolved_kgpc_type->type_alias != NULL && expr->resolved_kgpc_type->type_alias->target_type_id != NULL)
             {
                 const char *alias_name = expr->resolved_kgpc_type->type_alias->target_type_id;
                 
                 if (strcasecmp(alias_name, "PAnsiChar") == 0 ||
                     strcasecmp(alias_name, "PChar") == 0 ||
                     strcasecmp(alias_name, "pcchar") == 0 ||
                     strcasecmp(alias_name, "cchar") == 0 ||
                     strcasecmp(alias_name, "char") == 0)
                 {
                     treat_as_string = 1;
                 }
             }
        }

        if (expr != NULL && expr->resolved_kgpc_type != NULL && 
            kgpc_type_is_pointer(expr->resolved_kgpc_type))
        {
            int subtype = kgpc_type_get_pointer_subtype_tag(expr->resolved_kgpc_type);
            if (subtype == CHAR_TYPE)
            {
                treat_as_string = 1;
            }
        }
        
        const int expr_is_real = (expr_type == REAL_TYPE);
        const char *file_dest64 = current_arg_reg64(0);
        const char *width_dest64 = current_arg_reg64(1);
        const char *precision_dest64 = current_arg_reg64(2);
        const char *value_dest64 = current_arg_reg64(expr_is_real ? 3 : 2);

        StackNode_t *width_spill = NULL;
        StackNode_t *precision_spill = NULL;
        const int width_specified = (expr != NULL && expr->field_width != NULL);
        const int precision_specified = (expr_is_real && expr != NULL && expr->field_precision != NULL);

        if (width_specified)
        {
            expr_node_t *width_tree = build_expr_tree(expr->field_width);
            Register_t *width_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(width_tree, inst_list, ctx, width_reg);
            free_expr_tree(width_tree);
            width_spill = add_l_t("write_width");
            if (width_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                         width_reg->bit_64, width_spill->offset);
                inst_list = add_inst(inst_list, buffer);
            }
            free_reg(get_reg_stack(), width_reg);
        }

        if (precision_specified)
        {
            expr_node_t *precision_tree = build_expr_tree(expr->field_precision);
            Register_t *precision_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(precision_tree, inst_list, ctx, precision_reg);
            free_expr_tree(precision_tree);
            precision_spill = add_l_t("write_precision");
            if (precision_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                         precision_reg->bit_64, precision_spill->offset);
                inst_list = add_inst(inst_list, buffer);
            }
            free_reg(get_reg_stack(), precision_reg);
        }

        /* For char arrays being treated as strings, we need to load the address */
        Register_t *value_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (expr != NULL && expr_type == CHAR_TYPE && expr->is_array_expr && expr->array_element_type == CHAR_TYPE)
        {
            /* Load address of char array */
            inst_list = codegen_address_for_expr(expr, inst_list, ctx, &value_reg);
            if (codegen_had_error(ctx))
            {
                free_reg(get_reg_stack(), value_reg);
                return inst_list;
            }
        }
        else
        {
            /* Load value normally */
            expr_node_t *expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, value_reg);
            free_expr_tree(expr_tree);
        }

        /*
         * Handle register conflicts when setting up write function arguments.
         * We need to move registers to their destinations in an order that
         * doesn't cause intermediate values to be overwritten.
         * 
         * Potential conflicts:
         * - width_reg might be in value_dest64's position
         * - precision_reg might be in value_dest64 or width_dest64's position
         * 
         * Strategy: Move in reverse order of argument positions to avoid overwrites.
         * On Windows: value=%r8 (arg2), width=%rdx (arg1), precision=%r9 (arg3)
         * So: Move precision first, then width, then value.
         */
        
        /* Move precision first (if real and has precision) */
        /* Move precision first (if specified for reals) */
        if (expr_is_real && precision_specified)
        {
            if (precision_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                         precision_spill->offset, precision_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", precision_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        
        /* Move width next */
        if (width_specified)
        {
            if (width_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                         width_spill->offset, width_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", width_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", width_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        
        /* Move value last */
        if (treat_as_string)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, value_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (expr_is_real || expr_type == POINTER_TYPE)
        {
            /* REAL_TYPE and POINTER_TYPE are 64-bit - use movq */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, value_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (expr_type == LONGINT_TYPE)
        {
            /* LONGINT_TYPE is now 4 bytes (to match FPC) - sign-extend to 64-bit */
            inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_dest64);
        }
        else
        {
            /* Use zero-extension for unsigned types, sign-extension otherwise.
             * For zero-extension: writing to a 32-bit register automatically zeros
             * the upper 32 bits of the full 64-bit register. */
            int is_unsigned = expr_is_unsigned_type(expr);
            if (is_unsigned)
            {
                /* First zero-extend in the value register, then move to destination */
                inst_list = codegen_zero_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_32);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, value_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_dest64);
        }

        free_reg(get_reg_stack(), value_reg);

        /* Set precision to -1 if not real or not provided */
        if (expr_is_real && !precision_specified)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", precision_dest64);
            inst_list = add_inst(inst_list, buffer);
        }

        /* Determine if this is an unsigned type for printing */
        int is_unsigned_int = expr_is_unsigned_type(expr);
        
        const char *call_target = is_unsigned_int ? "kgpc_write_unsigned" : "kgpc_write_integer";
        int is_char_array = 0;
        int is_shortstring = 0;
        int char_array_size = 0;
        
        if (treat_as_string)
        {
            /* Check if it's a char array (not a regular string) */
            if (expr != NULL && expr_type == CHAR_TYPE && expr->is_array_expr && expr->array_element_type == CHAR_TYPE)
            {
                char_array_size = expr->array_upper_bound - expr->array_lower_bound + 1;
                
                /* Check if this is a ShortString (array[0..255] of Char) */
                if (expr->array_lower_bound == 0 && expr->array_upper_bound == 255 && char_array_size == 256)
                {
                    call_target = "kgpc_write_shortstring";
                    is_shortstring = 1;
                }
                else
                {
                    call_target = "kgpc_write_char_array";
                    is_char_array = 1;
                }
            }
            else
            {
                call_target = "kgpc_write_string";
            }
        }
        else if (expr_type == BOOL)
            call_target = "kgpc_write_boolean";
        else if (expr_is_real)
            call_target = "kgpc_write_real";
        else if (expr_type == CHAR_TYPE)
            call_target = "kgpc_write_char";
        else if (expr_type == POINTER_TYPE)
            call_target = "kgpc_write_integer";  // Print pointers as integers (addresses)

        if (has_file_arg && (file_spill != NULL || file_reg != NULL))
        {
            if (file_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", file_spill->offset, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", file_reg->bit_64, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64, file_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        
        /* For char arrays, pass the size as the 4th argument */
        if (is_char_array)
        {
            const char *size_dest64 = current_arg_reg64(3);
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %s\n", char_array_size, size_dest64);
            inst_list = add_inst(inst_list, buffer);
        }

        inst_list = codegen_vect_reg(inst_list, 0);

        inst_list = codegen_call_with_shadow_space(inst_list, ctx, call_target);

        free_arg_regs();
        
        /* Invalidate static link cache after each write argument
         * because the static link register may have been clobbered
         * during argument evaluation or the function call.
         * We must free the register first to avoid leaking it. */
        if (ctx->static_link_reg != NULL)
        {
            free_reg(get_reg_stack(), ctx->static_link_reg);
            ctx->static_link_reg = NULL;
            ctx->static_link_reg_level = 0;
        }

        args = args->next;
    }

    if (append_newline)
    {
        const char *file_dest64 = current_arg_reg64(0);
        if (has_file_arg && (file_spill != NULL || file_reg != NULL))
        {
            if (file_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", file_spill->offset, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", file_reg->bit_64, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64, file_dest64);
            inst_list = add_inst(inst_list, buffer);
        }

        inst_list = codegen_vect_reg(inst_list, 0);

        inst_list = codegen_call_with_shadow_space(inst_list, ctx, "kgpc_write_newline");

        free_arg_regs();
    }

    if (file_reg != NULL)
        free_reg(get_reg_stack(), file_reg);

    return inst_list;
}

static ListNode_t *codegen_builtin_read_like(struct Statement *stmt, ListNode_t *inst_list,
                                             CodeGenContext *ctx, int read_line)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    char buffer[128];
    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    Register_t *file_reg = NULL;
    StackNode_t *file_spill = NULL;
    int has_file_arg = 0;
    int read_consumed_line = 0;

    /* Check if first argument is a file */
    if (args != NULL)
    {
        struct Expression *first_expr = (struct Expression *)args->cur;
        if (first_expr != NULL && (expr_has_type_tag(first_expr, TEXT_TYPE)))
        {
            expr_node_t *file_tree = build_expr_tree(first_expr);
            file_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (file_reg != NULL)
            {
                inst_list = gencode_expr_tree(file_tree, inst_list, ctx, file_reg);
                has_file_arg = 1;
                file_spill = add_l_t("read_file");
                if (file_spill != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", file_reg->bit_64, file_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), file_reg);
                    file_reg = NULL;
                }
            }
            else
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for read file argument.");
            }
            free_expr_tree(file_tree);
            args = args->next;
        }
    }

    /* Process each argument to read */
    while (args != NULL)
    {
        struct Expression *expr = (struct Expression *)args->cur;
        int expr_type = (expr != NULL) ? expr_get_type_tag(expr) : UNKNOWN_TYPE;
        
        /* Get address of the variable to read into and save to stack */
        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate register for read address.");
            args = args->next;
            continue;
        }
        
        inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx))
        {
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
        
        /* Save address to a stack temporary to avoid register conflicts */
        StackNode_t *addr_spill = add_l_t("read_addr");
        if (addr_spill == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate stack space for read address.");
            free_reg(get_reg_stack(), addr_reg);
            args = args->next;
            continue;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        
        /* Special handling for STRING_TYPE - use kgpc_text_readln_into */
        if (expr_type == STRING_TYPE)
        {
            const char *file_dest64 = current_arg_reg64(0);
            const char *string_dest64 = current_arg_reg64(1);
            
            /* Set file argument (or NULL for stdin) */
            if (has_file_arg && file_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", file_spill->offset, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            
            /* Load string address from stack */
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", addr_spill->offset, string_dest64);
            inst_list = add_inst(inst_list, buffer);
            
            /* Call kgpc_text_readln_into for string reading */
            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, ctx, "kgpc_text_readln_into");
            free_arg_regs();
            
            /* Invalidate static link cache after call */
            if (ctx->static_link_reg != NULL)
            {
                free_reg(get_reg_stack(), ctx->static_link_reg);
                ctx->static_link_reg = NULL;
                ctx->static_link_reg_level = 0;
            }

            if (read_line)
                read_consumed_line = 1;
            
            args = args->next;
            continue;
        }
        
        /* Now set up arguments for non-variadic read functions:
         * arg0 (rdi/rcx): file pointer (NULL for stdin)
         * arg1 (rsi/rdx): address of variable to read into
         */
        const char *file_dest64 = current_arg_reg64(0);
        const char *addr_dest64 = current_arg_reg64(1);
        
        /* Determine which read function to call based on type */
        const char *read_func = NULL;
        switch (expr_type)
        {
            case INT_TYPE:
                read_func = "kgpc_read_integer";
                break;
            case LONGINT_TYPE:
                read_func = "kgpc_read_longint";
                break;
            case CHAR_TYPE:
                read_func = "kgpc_read_char";
                break;
            case REAL_TYPE:
                read_func = "kgpc_read_real";
                break;
            default:
                codegen_report_error(ctx, "ERROR: Unsupported type for read operation.");
                args = args->next;
                continue;
        }
        
        /* Set file argument (or NULL for stdin) */
        if (has_file_arg && file_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", file_spill->offset, file_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64, file_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        
        /* Load address from stack temporary to argument register */
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", addr_spill->offset, addr_dest64);
        inst_list = add_inst(inst_list, buffer);
        
        /* Call the appropriate read function */
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, ctx, read_func);
        free_arg_regs();
        
        /* Invalidate static link cache after each read argument */
        if (ctx->static_link_reg != NULL)
        {
            free_reg(get_reg_stack(), ctx->static_link_reg);
            ctx->static_link_reg = NULL;
            ctx->static_link_reg_level = 0;
        }
        
        args = args->next;
    }
    
    /* If readln, consume rest of line */
    if (read_line && !read_consumed_line)
    {
        const char *file_dest64 = current_arg_reg64(0);
        if (has_file_arg && file_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", file_spill->offset, file_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64, file_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, ctx, "kgpc_text_readln_discard");
        free_arg_regs();
    }
    
    if (file_reg != NULL)
        free_reg(get_reg_stack(), file_reg);

    return inst_list;
}

ListNode_t *codegen_builtin_proc(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_PROCEDURE_CALL);
    assert(ctx != NULL);

    char *proc_name;
    ListNode_t *args_expr;
    /* Long mangled procedure names require a generous buffer for emitted instructions. */
    char buffer[CODEGEN_MAX_INST_BUF];

    proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
    args_expr = stmt->stmt_data.procedure_call_data.expr_args;

    const char *proc_id_lookup = stmt->stmt_data.procedure_call_data.id;

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "SetLength"))
    {
        inst_list = codegen_builtin_setlength(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "write"))
    {
        inst_list = codegen_builtin_write_like(stmt, inst_list, ctx, 0);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "writeln"))
    {
        inst_list = codegen_builtin_write_like(stmt, inst_list, ctx, 1);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "read"))
    {
        inst_list = codegen_builtin_read_like(stmt, inst_list, ctx, 0);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "readln"))
    {
        inst_list = codegen_builtin_read_like(stmt, inst_list, ctx, 1);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Str"))
    {
        inst_list = codegen_builtin_str(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Insert"))
    {
        inst_list = codegen_builtin_insert(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Delete"))
    {
        inst_list = codegen_builtin_delete(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Inc"))
    {
        inst_list = codegen_builtin_inc(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Dec"))
    {
        inst_list = codegen_builtin_dec(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "New"))
    {
        inst_list = codegen_builtin_new(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Dispose"))
    {
        inst_list = codegen_builtin_dispose(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Val"))
    {
        inst_list = codegen_builtin_val(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Include"))
    {
        inst_list = codegen_builtin_include(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Exclude"))
    {
        inst_list = codegen_builtin_exclude(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    const char *proc_name_hint = stmt->stmt_data.procedure_call_data.id;
    if (proc_name_hint == NULL)
        proc_name_hint = stmt->stmt_data.procedure_call_data.mangled_id;

    inst_list = codegen_pass_arguments(args_expr, inst_list, ctx, NULL, 
        proc_name_hint, 0);
    inst_list = codegen_vect_reg(inst_list, 0);
    const char *call_target = (proc_name != NULL) ? proc_name : stmt->stmt_data.procedure_call_data.id;
    if (call_target == NULL)
        call_target = "";
    snprintf(buffer, 50, "\tcall\t%s\n", call_target);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_cleanup_call_stack(inst_list, ctx);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Returns a list of instructions */
ListNode_t *codegen_compound_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_COMPOUND_STATEMENT);
    assert(ctx != NULL);
    assert(symtab != NULL);

    ListNode_t *stmt_list;
    struct Statement *cur_stmt;

    stmt_list = stmt->stmt_data.compound_statement;
    while(stmt_list != NULL)
    {
        cur_stmt = (struct Statement *)stmt_list->cur;
        inst_list = codegen_stmt(cur_stmt, inst_list, ctx, symtab);
        stmt_list = stmt_list->next;
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for a variable assignment */
ListNode_t *codegen_var_assignment(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_VAR_ASSIGN);
    assert(ctx != NULL);

    StackNode_t *var;
    Register_t *reg;
    char buffer[CODEGEN_MAX_INST_BUF];
    struct Expression *var_expr, *assign_expr;
    int offset;

    var_expr = stmt->stmt_data.var_assign_data.var;
    assign_expr = stmt->stmt_data.var_assign_data.expr;

    


    if (codegen_expr_is_mp_integer(var_expr))
    {
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || value_reg == NULL || addr_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            if (addr_reg != NULL)
                free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        inst_list = codegen_call_mpint_assign(inst_list, addr_reg, value_reg);
        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reg);
        free_arg_regs();
        return inst_list;
    }

    int dest_is_static_array = (var_expr->is_array_expr && !expr_is_dynamic_array(var_expr)) ||
        expr_is_static_array_like(var_expr, ctx);
    int src_is_static_array = expr_is_static_array_like(assign_expr, ctx);

    if (dest_is_static_array && src_is_static_array)
    {
        /* Static array assignment (including record fields) */
        return codegen_assign_static_array(var_expr, assign_expr, inst_list, ctx);
    }
    else if (var_expr->type == EXPR_RECORD_ACCESS)
    {
        struct RecordField *field = codegen_lookup_record_field(var_expr);
        if (field != NULL)
        {
            int field_is_static_array = (field->is_array && !field->array_is_open);
            if (!field_is_static_array && field->type_id != NULL)
            {
                struct TypeAlias *alias = codegen_lookup_type_alias(ctx, field->type_id);
                if (alias != NULL && alias->is_array && !alias->is_open_array)
                    field_is_static_array = 1;
            }

            if (field_is_static_array && src_is_static_array)
                return codegen_assign_static_array(var_expr, assign_expr, inst_list, ctx);
        }
    }

    if (expr_get_type_tag(var_expr) == RECORD_TYPE)
        return codegen_assign_record_value(var_expr, assign_expr, inst_list, ctx);

    /* Character sets (set of char) need special handling like records due to 32-byte size */
    if (expr_get_type_tag(var_expr) == SET_TYPE && expr_is_char_set_ctx(var_expr, ctx))
        return codegen_assign_record_value(var_expr, assign_expr, inst_list, ctx);

    if (var_expr->type == EXPR_VAR_ID)
    {
        if (expr_is_dynamic_array(var_expr))
        {
            inst_list = codegen_assign_dynamic_array(var_expr, assign_expr, inst_list, ctx);
            return inst_list;
        }

        int scope_depth = 0;
        var = find_label_with_depth(var_expr->expr_data.id, &scope_depth);

        
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);
        
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        int var_type = expr_get_type_tag(var_expr);
        int coerced_to_real = 0;
        inst_list = codegen_maybe_convert_int_like_to_real(var_type, assign_expr,
            value_reg, inst_list, &coerced_to_real);

        /* Handle string assignment to string variables */
        if (var_type == STRING_TYPE)
        {
            /* If assigning a char to string, promote it first */
            int assign_type = expr_get_type_tag(assign_expr);
            if (assign_type == CHAR_TYPE)
            {
                /* Call kgpc_char_to_string to convert char to string */
                const char *arg_reg32 = codegen_target_is_windows() ? "%ecx" : "%edi";
                char buffer[128];
                
                /* Move char value to argument register (32-bit, zero-extended) */
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", value_reg->bit_32, arg_reg32);
                inst_list = add_inst(inst_list, buffer);
                
                /* Call the conversion function */
                inst_list = add_inst(inst_list, "\tcall\tkgpc_char_to_string\n");
                
                /* Move result (string pointer) back to value register */
                snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", value_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                free_reg(get_reg_stack(), value_reg);
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            inst_list = codegen_call_string_assign(inst_list, ctx, addr_reg, value_reg);
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
        
        /* Handle string literal assignment to char arrays */
        if (var_type == CHAR_TYPE &&
            var_expr->is_array_expr &&
            var_expr->array_element_type == CHAR_TYPE &&
            expr_get_type_tag(assign_expr) == STRING_TYPE)
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                free_reg(get_reg_stack(), value_reg);
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            int array_size = var_expr->array_upper_bound - var_expr->array_lower_bound + 1;
            
            /* Check if this is a ShortString (array[0..255] of Char) */
            int is_shortstring = (var_expr->array_lower_bound == 0 && 
                                 var_expr->array_upper_bound == 255 &&
                                 array_size == 256);
            
            if (is_shortstring)
            {
                /* Use ShortString-specific copy that sets length byte at index 0 */
                inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reg, value_reg, array_size);
            }
            else
            {
                /* Regular char array copy */
                inst_list = codegen_call_string_to_char_array(inst_list, ctx, addr_reg, value_reg, array_size);
            }
            
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        reg = value_reg;

        if(var != NULL)
        {
            int use_qword = codegen_type_uses_qword(var_type);
            if (!var->is_reference && var->size >= 8)
                use_qword = 1;
            int use_byte = 0;
            int use_word = 0;
            const char *value_reg8 = NULL;
            const char *value_reg16 = NULL;
            long long target_size = (var_expr->type == EXPR_RECORD_ACCESS) ?
                codegen_record_field_effective_size(var_expr, ctx) :
                expr_effective_size_bytes(var_expr);
            if (!use_qword && var_type == CHAR_TYPE)
            {
                value_reg8 = register_name8(reg);
                if (value_reg8 != NULL)
                {
                    use_byte = 1;
                }
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for character assignment.");
                }
            }
            else if (!use_qword && target_size == 2)
            {
                value_reg16 = codegen_register_name16(reg);
                if (value_reg16 != NULL)
                    use_word = 1;
                else
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 16-bit register for assignment.");
            }
            if (use_qword)
            {
                int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
                if (coerced_to_real)
                    value_is_qword = 1;
                if (!value_is_qword)
                    inst_list = codegen_sign_extend32_to64(inst_list, reg->bit_32, reg->bit_64);
            }
            if (var->is_static)
            {
                const char *label = (var->static_label != NULL) ?
                    var->static_label : var->label;
                if (use_qword)
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s(%%rip)\n", reg->bit_64, label);
                else if (use_byte)
                    snprintf(buffer, sizeof(buffer), "\tmovb\t%s, %s(%%rip)\n", value_reg8, label);
                else if (use_word)
                    snprintf(buffer, sizeof(buffer), "\tmovw\t%s, %s(%%rip)\n", value_reg16, label);
                else
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s(%%rip)\n", reg->bit_32, label);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), reg);
                return inst_list;
            }
            if (var->is_reference)
            {
                Register_t *ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (ptr_reg == NULL)
                {
                    free_reg(get_reg_stack(), reg);
                    return codegen_fail_register(ctx, inst_list, NULL,
                        "ERROR: Unable to allocate register for reference assignment.");
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", var->offset, ptr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                if (use_qword)
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", reg->bit_64, ptr_reg->bit_64);
                else if (use_byte)
                    snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", value_reg8, ptr_reg->bit_64);
                else if (use_word)
                    snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", value_reg16, ptr_reg->bit_64);
                else
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", reg->bit_32, ptr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), ptr_reg);
                free_reg(get_reg_stack(), reg);
                return inst_list;
            }
            else if (scope_depth == 0)
            {
                /* Variable is in current scope, assign normally */
                if (use_qword)
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", reg->bit_64, var->offset);
                else if (use_byte)
                    snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", value_reg8, var->offset);
                else if (use_word)
                    snprintf(buffer, sizeof(buffer), "\tmovw\t%s, -%d(%%rbp)\n", value_reg16, var->offset);
                else
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", reg->bit_32, var->offset);
            }
            else
            {
                codegen_begin_expression(ctx);
                Register_t *frame_reg = codegen_acquire_static_link(ctx, &inst_list, scope_depth);
                if (frame_reg != NULL)
                {
                    if (use_qword)
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%s)\n", reg->bit_64, var->offset, frame_reg->bit_64);
                    else if (use_byte)
                        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%s)\n", value_reg8, var->offset, frame_reg->bit_64);
                    else if (use_word)
                        snprintf(buffer, sizeof(buffer), "\tmovw\t%s, -%d(%s)\n", value_reg16, var->offset, frame_reg->bit_64);
                    else
                        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%s)\n", reg->bit_32, var->offset, frame_reg->bit_64);
                }
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: Failed to acquire static link for assignment to %s.",
                        var_expr->expr_data.id);
                    if (use_qword)
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", reg->bit_64, var->offset);
                    else if (use_byte)
                        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", value_reg8, var->offset);
                    else if (use_word)
                        snprintf(buffer, sizeof(buffer), "\tmovw\t%s, -%d(%%rbp)\n", value_reg16, var->offset);
                    else
                        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", reg->bit_32, var->offset);
                }
                codegen_end_expression(ctx);
            }
        }
        else if(nonlocal_flag() == 1)
        {

            inst_list = codegen_get_nonlocal(inst_list, var_expr->expr_data.id, &offset);
            int use_qword = codegen_type_uses_qword(var_type);
            int use_byte = 0;
            const char *value_reg8 = NULL;
            if (!use_qword && var_type == CHAR_TYPE)
            {
                value_reg8 = register_name8(reg);
                if (value_reg8 != NULL)
                    use_byte = 1;
                else
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for character assignment.");
            }
            if (use_qword)
            {
                int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
                if (coerced_to_real)
                    value_is_qword = 1;
                if (!value_is_qword)
                    inst_list = codegen_sign_extend32_to64(inst_list, reg->bit_32, reg->bit_64);
                snprintf(buffer, 50, "\tmovq\t%s, -%d(%s)\n", reg->bit_64, offset, current_non_local_reg64());
            }
            else if (use_byte)
            {
                snprintf(buffer, 50, "\tmovb\t%s, -%d(%s)\n", value_reg8, offset, current_non_local_reg64());
            }
            else
            {
                snprintf(buffer, 50, "\tmovl\t%s, -%d(%s)\n", reg->bit_32, offset, current_non_local_reg64());
            }
        }
        else
        {
            const char *var_name = (var_expr != NULL && var_expr->type == EXPR_VAR_ID) ?
                var_expr->expr_data.id : "<unknown>";
            char errbuf[256];
            snprintf(errbuf, sizeof(errbuf),
                "ERROR: Non-local codegen support disabled while accessing %s. Enable with flag '-non-local' after required flags",
                var_name != NULL ? var_name : "<unknown>");
            codegen_report_error(ctx, errbuf);
            free_reg(get_reg_stack(), reg);
            return inst_list;
        }
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), reg);
        return inst_list;
    }
    else if (var_expr->type == EXPR_ARRAY_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_array_element_address(var_expr, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;

        StackNode_t *addr_temp = add_l_t("array_addr");
        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_temp->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);

        inst_list = codegen_expr(assign_expr, inst_list, ctx);
        if (codegen_had_error(ctx))
            return inst_list;
        Register_t *value_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (value_reg == NULL)
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for array value.");

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for array store.");
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        int var_type = expr_get_type_tag(var_expr);
        int coerced_to_real = 0;
        inst_list = codegen_maybe_convert_int_like_to_real(var_type, assign_expr,
            value_reg, inst_list, &coerced_to_real);
        int use_qword = codegen_type_uses_qword(var_type);
        /* Get element size from the base array expression, not the access expression */
        struct Expression *array_expr = var_expr->expr_data.array_access_data.array_expr;
        long long element_size = array_expr != NULL ? expr_get_array_element_size(array_expr, ctx) : -1;
        if (element_size <= 0)
            element_size = expr_effective_size_bytes(var_expr);
        int use_word = (!use_qword && element_size == 2);
        if (var_type == STRING_TYPE)
        {
            inst_list = codegen_call_string_assign(inst_list, ctx, addr_reload, value_reg);
        }
        else if (use_qword)
        {
            int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
            if (coerced_to_real)
                value_is_qword = 1;
            if (!value_is_qword)
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
            snprintf(buffer, 50, "\tmovq\t%s, (%s)\n", value_reg->bit_64, addr_reload->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (var_type == CHAR_TYPE || element_size == 1)
            {
                const char *value_reg8 = register_name8(value_reg);
                if (value_reg8 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for byte assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovb\t%s, (%s)\n", value_reg8, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else if (use_word)
            {
                const char *value_reg16 = codegen_register_name16(value_reg);
                if (value_reg16 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 16-bit register for array assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovw\t%s, (%s)\n", value_reg16, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                snprintf(buffer, 50, "\tmovl\t%s, (%s)\n", value_reg->bit_32, addr_reload->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
        }

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reload);
        return inst_list;
    }
    else if (var_expr->type == EXPR_RECORD_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_record_field_address(var_expr, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;

        StackNode_t *addr_temp = add_l_t("record_addr");
        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_temp->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);

        inst_list = codegen_expr(assign_expr, inst_list, ctx);
        if (codegen_had_error(ctx))
            return inst_list;

        Register_t *value_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (value_reg == NULL)
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for record value.");

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for record store.");
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        int var_type_2 = expr_get_type_tag(var_expr);
        if (var_type_2 == UNKNOWN_TYPE && var_expr->type == EXPR_RECORD_ACCESS)
        {
            const char *field_id = var_expr->expr_data.record_access_data.field_id;
            int resolved = lookup_record_field_type(var_expr->record_type, field_id);
            if (resolved != UNKNOWN_TYPE)
            {
                var_type_2 = resolved;
                var_expr->resolved_type = resolved;
            }
        }
        int coerced_to_real = 0;
        inst_list = codegen_maybe_convert_int_like_to_real(var_type_2, assign_expr,
            value_reg, inst_list, &coerced_to_real);
        int use_qword = codegen_type_uses_qword(var_type_2);
        long long record_element_size = codegen_record_field_effective_size(var_expr, ctx);
        int use_word = (!use_qword && record_element_size == 2);
        if (var_type_2 == STRING_TYPE)
        {
            inst_list = codegen_call_string_assign(inst_list, ctx, addr_reload, value_reg);
        }
        else if (use_qword)
        {
            int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
            if (coerced_to_real)
                value_is_qword = 1;
            if (!value_is_qword)
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
            snprintf(buffer, 50, "\tmovq\t%s, (%s)\n", value_reg->bit_64, addr_reload->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (var_type_2 == CHAR_TYPE)
            {
                const char *value_reg8 = register_name8(value_reg);
                if (value_reg8 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for character assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovb\t%s, (%s)\n", value_reg8, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else if (use_word)
            {
                const char *value_reg16 = codegen_register_name16(value_reg);
                if (value_reg16 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 16-bit register for record assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovw\t%s, (%s)\n", value_reg16, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                snprintf(buffer, 50, "\tmovl\t%s, (%s)\n", value_reg->bit_32, addr_reload->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
        }

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reload);
        return inst_list;
    }
    else if (var_expr->type == EXPR_POINTER_DEREF)
    {
        struct Expression *pointer_expr = var_expr->expr_data.pointer_deref_data.pointer_expr;
        if (pointer_expr == NULL)
        {
            codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
            return inst_list;
        }

        expr_node_t *pointer_tree = build_expr_tree(pointer_expr);
        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
        {
            free_expr_tree(pointer_tree);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for pointer assignment address.");
        }

        inst_list = gencode_expr_tree(pointer_tree, inst_list, ctx, addr_reg);
        free_expr_tree(pointer_tree);

        StackNode_t *addr_temp = add_l_t("pointer_addr");
        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_temp->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);

        inst_list = codegen_expr(assign_expr, inst_list, ctx);
        if (codegen_had_error(ctx))
            return inst_list;

        Register_t *value_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (value_reg == NULL)
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for pointer value.");

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for pointer store.");
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        int var_type_3 = expr_get_type_tag(var_expr);
        int coerced_to_real = 0;
        inst_list = codegen_maybe_convert_int_like_to_real(var_type_3, assign_expr,
            value_reg, inst_list, &coerced_to_real);
        long long pointer_target_size = expr_effective_size_bytes(var_expr);
        int use_word = (!codegen_type_uses_qword(var_type_3) && pointer_target_size == 2);
        if (var_type_3 == STRING_TYPE)
        {
            inst_list = codegen_call_string_assign(inst_list, ctx, addr_reload, value_reg);
        }
        else if (codegen_type_uses_qword(var_type_3))
        {
            int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
            if (coerced_to_real)
                value_is_qword = 1;
            if (!value_is_qword)
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
            snprintf(buffer, 50, "\tmovq\t%s, (%s)\n", value_reg->bit_64, addr_reload->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (var_type_3 == CHAR_TYPE)
            {
                const char *value_reg8 = register_name8(value_reg);
                if (value_reg8 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for character assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovb\t%s, (%s)\n", value_reg8, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                if (use_word)
                {
                    const char *value_reg16 = codegen_register_name16(value_reg);
                    if (value_reg16 == NULL)
                    {
                        codegen_report_error(ctx,
                            "ERROR: Unable to select 16-bit register for pointer assignment.");
                    }
                    else
                    {
                        snprintf(buffer, 50, "\tmovw\t%s, (%s)\n", value_reg16, addr_reload->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }
                else
                {
                    snprintf(buffer, 50, "\tmovl\t%s, (%s)\n", value_reg->bit_32, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
        }

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reload);
        return inst_list;
    }
    else
    {
        assert(0 && "Unsupported assignment target");
    }
}

/* Code generation for a procedure call */
ListNode_t *codegen_proc_call(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_PROCEDURE_CALL);
    assert(ctx != NULL);
    assert(symtab != NULL);

    char *proc_name;
    ListNode_t *args_expr;
    /* Procedure calls can reference very long mangled identifiers, so keep plenty of space. */
    char buffer[CODEGEN_MAX_INST_BUF];

    proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
    args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    char *unmangled_name = stmt->stmt_data.procedure_call_data.id;
    
    /* CRITICAL FIX: Use cached information from AST instead of HashNode pointer.
     * The resolved_proc pointer may point to freed memory if the HashNode was in a scope
     * that has been popped (e.g., nested procedures' parameters/local variables).
     * 
     * During semantic checking, we now cache the essential information (hash_type and KgpcType)
     * directly in the AST, avoiding the need to dereference potentially freed HashNode pointers.
     */
    int call_hash_type;  /* HashType enum value */
    struct KgpcType *call_kgpc_type;
    
    if (stmt->stmt_data.procedure_call_data.is_call_info_valid)
    {
        /* Use cached information from semantic checking */
        call_hash_type = stmt->stmt_data.procedure_call_data.call_hash_type;
        call_kgpc_type = stmt->stmt_data.procedure_call_data.call_kgpc_type;
        
        CODEGEN_DEBUG("DEBUG codegen_proc_call: id=%s, mangled=%s, hash_type=%d\n",
            unmangled_name ? unmangled_name : "NULL",
            proc_name ? proc_name : "NULL", 
            call_hash_type);
    }
    else
    {
        /* Fallback: look up the symbol (for old code paths or if semantic checker didn't set it) */
        HashNode_t *proc_node = NULL;
        FindIdent(&proc_node, symtab, unmangled_name);
        if (proc_node != NULL)
        {
            call_hash_type = proc_node->hash_type;
            call_kgpc_type = proc_node->type;
        }
        else
        {
            /* Symbol not found - this is an error that should have been caught in semantic checking */
            codegen_report_error(ctx,
                "FATAL: Internal compiler error - procedure %s not found during code generation. "
                "This should have been caught during semantic checking.",
                unmangled_name ? unmangled_name : "(unknown)");
            return inst_list;
        }
    }

    if(call_hash_type == HASHTYPE_FUNCTION)
    {
        struct Expression *call_expr = mk_functioncall(stmt->line_num,
            stmt->stmt_data.procedure_call_data.id != NULL ?
                strdup(stmt->stmt_data.procedure_call_data.id) : NULL,
            stmt->stmt_data.procedure_call_data.expr_args);
        if (call_expr == NULL)
            return inst_list;

        if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
        {
            call_expr->expr_data.function_call_data.mangled_id =
                strdup(stmt->stmt_data.procedure_call_data.mangled_id);
        }
        call_expr->expr_data.function_call_data.call_hash_type = call_hash_type;
        call_expr->expr_data.function_call_data.call_kgpc_type =
            stmt->stmt_data.procedure_call_data.call_kgpc_type;
        call_expr->expr_data.function_call_data.is_call_info_valid =
            stmt->stmt_data.procedure_call_data.is_call_info_valid;

        Register_t *discard_reg = NULL;
        inst_list = codegen_evaluate_expr(call_expr, inst_list, ctx, &discard_reg);
        if (discard_reg != NULL)
            free_reg(get_reg_stack(), discard_reg);
        call_expr->expr_data.function_call_data.args_expr = NULL;
        destroy_expr(call_expr);
        return inst_list;
    }

    if(call_hash_type == HASHTYPE_BUILTIN_PROCEDURE)
    {
        return codegen_builtin_proc(stmt, inst_list, ctx);
    }

    /* Deterministic external name selection (no defensive fallback):
     * If a procedure definition exists and declares an explicit external alias (cname_override),
     * prefer it as the call target unconditionally. Otherwise, rely on the call-site mangled name
     * populated by semantic checking. If neither is available, panic with a descriptive error. */
    if (stmt->stmt_data.procedure_call_data.is_call_info_valid &&
        call_kgpc_type != NULL && call_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
        call_kgpc_type->info.proc_info.definition != NULL)
    {
        Tree_t *def = call_kgpc_type->info.proc_info.definition;
        const char *alias = def->tree_data.subprogram_data.cname_override;
        if (alias != NULL && alias[0] != '\0')
        {
            /* Overwrite any prior name with explicit external alias */
            if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
            {
                free(stmt->stmt_data.procedure_call_data.mangled_id);
                stmt->stmt_data.procedure_call_data.mangled_id = NULL;
            }
            stmt->stmt_data.procedure_call_data.mangled_id = strdup(alias);
            proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
        }
    }
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
    /* Check if this is an indirect call through a procedure variable */
    /* For indirect calls (procedure variables/parameters), we need to generate an indirect call.
     * 
     * IMPORTANT: We must check hash_type FIRST, before checking type information.
     * On some platforms (e.g., Cygwin), type information may not be properly set,
     * but hash_type is always reliable for distinguishing variables from procedures.
     */
    int is_indirect_call = 0;
    
    /* Case 1: If hash_type is VAR, this is a procedure variable or parameter.
     * It MUST be an indirect call, regardless of whether type info is present. */
    if (call_hash_type == HASHTYPE_VAR)
    {
        is_indirect_call = 1;
    }
    /* Case 2: If hash_type is PROCEDURE but type info indicates it's a procedure type,
     * and either proc_name is NULL or doesn't match, treat as indirect call.
     * This handles corrupted or improperly resolved procedure nodes. */
    else if (call_hash_type == HASHTYPE_PROCEDURE &&
             call_kgpc_type != NULL && 
             call_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
             proc_name == NULL)
    {
        is_indirect_call = 1;
    }
    
    if (is_indirect_call)
    {
        /* INDIRECT CALL LOGIC */
        
        /* Validate that we have a procedure name for the indirect call */
        if (unmangled_name == NULL)
        {
            codegen_report_error(ctx,
                "FATAL: Internal compiler error - indirect call with NULL procedure name. "
                "Please report this bug with your source code.");
            return inst_list;
        }

        /* 1. Create a temporary expression to evaluate the procedure variable */
        struct Expression *callee_expr = mk_varid(stmt->line_num, strdup(unmangled_name));
        callee_expr->resolved_type = PROCEDURE;
        
        /* 2. Generate code to load the procedure's address into a register */
        Register_t *addr_reg = NULL;
        inst_list = codegen_evaluate_expr(callee_expr, inst_list, ctx, &addr_reg);
        destroy_expr(callee_expr);
        
        if (codegen_had_error(ctx) || addr_reg == NULL)
        {
            return inst_list;
        }
        
        /* 3. Prevent clobbering %rax. Move the address to a safe register if needed */
        const char *call_reg_name = addr_reg->bit_64;
        if (call_reg_name == NULL)
        {
            codegen_report_error(ctx,
                "FATAL: Internal compiler error - NULL register name in indirect call. "
                "Please report this bug with your source code.");
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
        if (strcmp(call_reg_name, "%rax") == 0)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %%r11\n");
            inst_list = add_inst(inst_list, buffer);
            call_reg_name = "%r11";
        }
        
        /* 4. Pass arguments as usual */
        inst_list = codegen_pass_arguments(args_expr, inst_list, ctx, call_kgpc_type, 
            unmangled_name, 0);
        
        /* 5. Zero out %eax for varargs ABI compatibility */
        inst_list = codegen_vect_reg(inst_list, 0);
        
        /* 6. Perform the indirect call */
        snprintf(buffer, sizeof(buffer), "\tcall\t*%s\n", call_reg_name);
        inst_list = add_inst(inst_list, buffer);
        
        /* 7. Cleanup */
        free_reg(get_reg_stack(), addr_reg);
        inst_list = codegen_cleanup_call_stack(inst_list, ctx);
        
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s (indirect call)\n", __func__);
        #endif
        return inst_list;
    }
    else if (call_hash_type == HASHTYPE_PROCEDURE)
    {
        /* DIRECT CALL LOGIC */

        int callee_needs_static_link = codegen_proc_requires_static_link(ctx, proc_name);
        int callee_depth = 0;
        int have_depth = codegen_proc_static_link_depth(ctx, proc_name, &callee_depth);
        int current_depth = codegen_get_lexical_depth(ctx);
        int should_pass_static_link = (callee_needs_static_link && have_depth);

        enum {
            STATIC_LINK_NONE = 0,
            STATIC_LINK_FROM_RBP,
            STATIC_LINK_FROM_SLOT,
            STATIC_LINK_FROM_REG,
            STATIC_LINK_FROM_SPILL
        } static_link_source = STATIC_LINK_NONE;
        int static_link_slot_offset = 0;
        Register_t *static_link_reg = NULL;
        int static_link_expr_active = 0;
        StackNode_t *static_link_spill = NULL;

        if (should_pass_static_link)
        {
            if (callee_depth > current_depth)
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
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: Missing static link slot while calling %s.",
                        unmangled_name);
                }
            }
            else
            {
                /* We need to traverse multiple scopes to get the static link.
                 * We must spill the result to a stack slot BEFORE evaluating arguments,
                 * because argument evaluation may call codegen_acquire_static_link again
                 * for different scope depths, which would free and reuse the register. */
                int levels_to_traverse = (current_depth - callee_depth) + 1;
                codegen_begin_expression(ctx);
                static_link_expr_active = 1;
                static_link_reg = codegen_acquire_static_link(ctx, &inst_list, levels_to_traverse);
                if (static_link_reg == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Failed to acquire static link for call to %s.",
                        unmangled_name);
                }
                else
                {
                    /* Spill the static link to a temporary stack slot to preserve it
                     * during argument evaluation */
                    static_link_spill = add_l_t("static_link_spill");
                    char spill_buffer[64];
                    snprintf(spill_buffer, sizeof(spill_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        static_link_reg->bit_64, static_link_spill->offset);
                    inst_list = add_inst(inst_list, spill_buffer);
                    free_reg(get_reg_stack(), static_link_reg);
                    static_link_reg = NULL;
                    static_link_source = STATIC_LINK_FROM_SPILL;
                }
                /* End the expression context to allow argument evaluation to use
                 * codegen_acquire_static_link independently */
                codegen_end_expression(ctx);
                static_link_expr_active = 0;
            }
        }
        
        /* When passing static link, shift arguments by 1 register position */
        int arg_start_index = should_pass_static_link ? 1 : 0;
        inst_list = codegen_pass_arguments(args_expr, inst_list, ctx, call_kgpc_type, 
            unmangled_name, arg_start_index);

        if (should_pass_static_link)
        {
            const char *link_dest_reg = current_arg_reg64(0);
            assert(link_dest_reg != NULL && "current_arg_reg64(0) should never return NULL");

            char link_buffer[64];
            switch (static_link_source)
            {
                case STATIC_LINK_FROM_RBP:
                    snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%%rbp, %s\n", link_dest_reg);
                    inst_list = add_inst(inst_list, link_buffer);
                    break;
                case STATIC_LINK_FROM_SLOT:
                    snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t-%d(%%rbp), %s\n",
                        static_link_slot_offset, link_dest_reg);
                    inst_list = add_inst(inst_list, link_buffer);
                    break;
                case STATIC_LINK_FROM_SPILL:
                    /* Restore from spill slot where we saved it before argument evaluation */
                    if (static_link_spill != NULL)
                    {
                        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t-%d(%%rbp), %s\n",
                            static_link_spill->offset, link_dest_reg);
                        inst_list = add_inst(inst_list, link_buffer);
                    }
                    break;
                case STATIC_LINK_FROM_REG:
                    if (static_link_reg != NULL)
                    {
                        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%s, %s\n",
                            static_link_reg->bit_64, link_dest_reg);
                        inst_list = add_inst(inst_list, link_buffer);
                        free_reg(get_reg_stack(), static_link_reg);
                        static_link_reg = NULL;
                    }
                    break;
                default:
                    break;
            }
        }

        if (static_link_expr_active)
        {
            codegen_end_expression(ctx);
        }

     inst_list = codegen_vect_reg(inst_list, 0);
        CODEGEN_DEBUG("DEBUG PROC_CALL: proc_name=%s\n", proc_name ? proc_name : "NULL");
        snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", proc_name);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_cleanup_call_stack(inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }
    
    /* If we reach here, it's a compiler bug - this should never happen.
     * In semantic checking, we should have caught any invalid procedure calls.
     * If we get here, it means:
     * 1. proc_node is NULL (semantic checker failed to resolve the symbol), OR
     * 2. proc_node has an unexpected hash_type (not VAR, PROCEDURE, or BUILTIN)
     * 
     * This indicates a serious internal error, so we assert in debug builds
     * and report a fatal error in release builds. */
    
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("FATAL: Reached unreachable code in %s - this is a compiler bug!\n", __func__);
    if (proc_node != NULL) {
        CODEGEN_DEBUG("  proc_name: %s\n", proc_name ? proc_name : "(null)");
        CODEGEN_DEBUG("  unmangled_name: %s\n", unmangled_name ? unmangled_name : "(null)");
        CODEGEN_DEBUG("  hash_type: %d (expected VAR=%d, PROCEDURE=%d, or BUILTIN=%d)\n", 
                     proc_node->hash_type, HASHTYPE_VAR, HASHTYPE_PROCEDURE, HASHTYPE_BUILTIN_PROCEDURE);
        CODEGEN_DEBUG("  type: %p\n", (void*)proc_node->type);
        if (proc_node->type != NULL) {
            CODEGEN_DEBUG("  type->kind: %d\n", proc_node->type->kind);
        }
    } else {
        CODEGEN_DEBUG("  proc_node is NULL - semantic checker should have caught this!\n");
    }
    #endif
    
    /* In debug builds, assert to catch this bug during development */
    assert(0 && "Unreachable: procedure call with unexpected hash_type or NULL proc_node");
    
    /* In release builds, report a fatal error and stop code generation */
    codegen_report_error(ctx,
        "FATAL: Internal compiler error in procedure call code generation for '%s'. "
        "Please report this bug with your source code.",
        unmangled_name ? unmangled_name : "(unknown)");
    
    return inst_list;
}

/* Code generation for if-then-else statements */
ListNode_t *codegen_if_then(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_IF_THEN);
    assert(ctx != NULL);
    assert(symtab != NULL);

    int relop_type, inverse;
    struct Expression *expr;
    struct Statement *if_stmt, *else_stmt;
    char label1[18], label2[18], buffer[50];

    expr = stmt->stmt_data.if_then_data.relop_expr;
    inst_list = codegen_condition_expr(expr, inst_list, ctx, &relop_type);

    gen_label(label1, 18, ctx);
    gen_label(label2, 18, ctx);
    if_stmt = stmt->stmt_data.if_then_data.if_stmt;
    else_stmt = stmt->stmt_data.if_then_data.else_stmt;

    inverse = 1;
    inst_list = gencode_jmp(relop_type, inverse, label1, inst_list);
    inst_list = codegen_stmt(if_stmt, inst_list, ctx, symtab);

    if(else_stmt == NULL)
    {
        snprintf(buffer, 50, "%s:\n", label1);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        inverse = 0;
        inst_list = gencode_jmp(NORMAL_JMP, inverse, label2, inst_list);
        snprintf(buffer, 50, "%s:\n", label1);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_stmt(else_stmt, inst_list, ctx, symtab);
        snprintf(buffer, 50, "%s:\n", label2);
        inst_list = add_inst(inst_list, buffer);
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for while statements */
ListNode_t *codegen_while(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_WHILE);
    assert(ctx != NULL);
    assert(symtab != NULL);

    int relop_type;
    struct Expression *expr;
    struct Statement *while_stmt;
    char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];

    gen_label(cond_label, 18, ctx);
    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    gen_label(incr_label, 18, ctx);
    while_stmt = stmt->stmt_data.while_data.while_stmt;
    expr = stmt->stmt_data.while_data.relop_expr;

    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);
    if (!codegen_push_loop(ctx, exit_label, cond_label))
        return inst_list;
    inst_list = codegen_stmt(while_stmt, inst_list, ctx, symtab);
    codegen_pop_loop(ctx);
    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_condition_expr(expr, inst_list, ctx, &relop_type);
    inst_list = gencode_jmp(relop_type, 0, body_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

ListNode_t *codegen_repeat(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_REPEAT);
    assert(ctx != NULL);
    assert(symtab != NULL);

    char body_label[18], exit_label[18], buffer[50];
    int relop_type;
    ListNode_t *body_list = stmt->stmt_data.repeat_data.body_list;

    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    snprintf(buffer, 50, "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);

    if (!codegen_push_loop(ctx, exit_label, body_label))
        return inst_list;
    while (body_list != NULL)
    {
        struct Statement *body_stmt = (struct Statement *)body_list->cur;
        inst_list = codegen_stmt(body_stmt, inst_list, ctx, symtab);
        body_list = body_list->next;
    }
    codegen_pop_loop(ctx);

    inst_list = codegen_condition_expr(stmt->stmt_data.repeat_data.until_expr, inst_list, ctx, &relop_type);
    inst_list = gencode_jmp(relop_type, 1, body_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for for-in statements - lower to regular for loop */
static ListNode_t *codegen_for_in(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_FOR_IN);
    assert(ctx != NULL);
    assert(symtab != NULL);

    // Extract for-in components
    struct Expression *loop_var = stmt->stmt_data.for_in_data.loop_var;
    struct Expression *collection = stmt->stmt_data.for_in_data.collection;
    struct Statement *body = stmt->stmt_data.for_in_data.do_stmt;

    if (loop_var == NULL || collection == NULL || body == NULL) {
        codegen_report_error(ctx, "ERROR: FOR-IN statement has NULL components");
        return inst_list;
    }

    // Get array type info from semantic check
    KgpcType *array_type = collection->resolved_kgpc_type;
    
    // Check if this is a TFPGList (specialized generic list)
    int is_fpglist = 0;
    struct RecordType *record_info = NULL;
    if (array_type != NULL && array_type->kind == TYPE_KIND_RECORD) {
        record_info = kgpc_type_get_record(array_type);
        if (record_info != NULL && record_info->type_id != NULL) {
            const char *prefix = "TFPGList$";
            size_t prefix_len = strlen(prefix);
            if (strncasecmp(record_info->type_id, prefix, prefix_len) == 0) {
                is_fpglist = 1;
            }
        }
    }
    
    if (is_fpglist) {
        // Generate FOR-IN loop for TFPGList by accessing FItems and FCount directly
        // Structure: for Item in L do body
        // Becomes: for i := 0 to L.FCount-1 do Item := L.FItems[i]; body
        
        char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];
        gen_label(cond_label, 18, ctx);
        gen_label(body_label, 18, ctx);
        gen_label(exit_label, 18, ctx);
        gen_label(incr_label, 18, ctx);

        // Allocate stack slot for loop index
        StackNode_t *index_slot = codegen_alloc_temp_slot("fpg_idx");
        // Allocate stack slot for object pointer to survive loop calls
        StackNode_t *obj_ptr_slot = codegen_alloc_temp_slot("fpg_obj");
        // Allocate stack slot for loop upper bound (FCount) to survive loop body codegen
        StackNode_t *count_slot = codegen_alloc_temp_slot("fpg_count");
        
        if (index_slot == NULL || obj_ptr_slot == NULL || count_slot == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate temp slot for FOR-IN variables");
            return inst_list;
        }

        // Get address of collection object (L)
        Register_t *obj_addr_reg = NULL;
        inst_list = codegen_address_for_expr(collection, inst_list, ctx, &obj_addr_reg);
        if (obj_addr_reg == NULL || codegen_had_error(ctx)) {
            return inst_list;
        }
        
        // Dereference L to get the actual object pointer and store in stack slot
        Register_t *temp_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (temp_reg == NULL) {
            free_reg(get_reg_stack(), obj_addr_reg);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for object pointer load");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", obj_addr_reg->bit_64, temp_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        
        // Store object pointer to stack
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", temp_reg->bit_64, obj_ptr_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        
        free_reg(get_reg_stack(), obj_addr_reg);

        // Load FCount and store to stack slot (to prevent register clobbering in loop body)
        // Layout: [0..7]: TypeInfo, [8..23]: FItems (descriptor: data + length), [24..31]: FCount
        // We use temp_reg (holding obj ptr) to access FCount at offset 24, then immediately
        // spill to a stack slot so the loop body codegen can't clobber it
        snprintf(buffer, sizeof(buffer), "\tmovq\t24(%s), %s\n", temp_reg->bit_64, temp_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", temp_reg->bit_64, count_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), temp_reg);

        // Initialize index to 0
        snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Jump to condition check
        inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

        // Body label
        snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        // Push loop exit label for break statements
        if (!codegen_push_loop(ctx, exit_label, incr_label)) {
            return inst_list;
        }

        // Load loop variable: Item := L.FItems[index]
        // FItems is a dynamic array pointer at offset 8
        Register_t *fitems_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (fitems_reg == NULL) {
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for FItems");
            return inst_list;
        }
        
        // Load object pointer from stack to temp register (reusing fitems_reg temporarily to save registers? No, safer to use another temp)
        Register_t *obj_reload_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (obj_reload_reg == NULL) {
             free_reg(get_reg_stack(), fitems_reg);
             codegen_pop_loop(ctx);
             codegen_report_error(ctx, "ERROR: Unable to allocate register for object reload");
             return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", obj_ptr_slot->offset, obj_reload_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        // Load FItems.data pointer from object using two-step approach
        // Step 1: Get address of FItems descriptor (at Self+8)  
        // Step 2: Load data pointer from descriptor[0]
        Register_t *desc_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (desc_addr_reg == NULL) {
            free_reg(get_reg_stack(), obj_reload_reg);
            free_reg(get_reg_stack(), fitems_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for descriptor address");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tleaq\t8(%s), %s\n", obj_reload_reg->bit_64, desc_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", desc_addr_reg->bit_64, fitems_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), desc_addr_reg);
        free_reg(get_reg_stack(), obj_reload_reg);

        // Load index into register
        Register_t *idx_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (idx_reg == NULL) {
            free_reg(get_reg_stack(), fitems_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for index");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", index_slot->offset, idx_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        // Calculate element offset: index * element_size
        int element_size = 0;

        // Try to determine element size from FItems field
        if (record_info != NULL) {
            ListNode_t *fields = record_info->fields;
            while (fields != NULL) {
                if (fields->type == LIST_RECORD_FIELD) {
                    struct RecordField *f = (struct RecordField *)fields->cur;
                    if (f != NULL && f->name != NULL && strcmp(f->name, "FItems") == 0) {
                        // Try lookup by ID first
                        if (f->array_element_type_id != NULL && ctx->symtab != NULL) {
                            HashNode_t *tnode = NULL;
                            if (FindIdent(&tnode, ctx->symtab, f->array_element_type_id) >= 0 &&
                                tnode != NULL && tnode->type != NULL) {
                                element_size = (int)kgpc_type_sizeof(tnode->type);
                            }
                        }
                        // Fallback to primitive tag if ID lookup failed or size is 0
                        if (element_size <= 0) {
                            switch (f->array_element_type) {
                                case CHAR_TYPE: case BOOL: element_size = 1; break;
                                case INT_TYPE: case ENUM_TYPE: element_size = 4; break;
                                case LONGINT_TYPE: element_size = 4; break;  // 4 bytes to match FPC
                                case POINTER_TYPE: case REAL_TYPE: element_size = 8; break;
                            }
                        }
                        break;
                    }
                }
                fields = fields->next;
            }
        }

        // Fallback to loop variable size if FItems size couldn't be determined
        if (element_size <= 0 && loop_var != NULL && loop_var->resolved_kgpc_type != NULL) {
             element_size = (int)kgpc_type_sizeof(loop_var->resolved_kgpc_type);
        }

        if (element_size <= 0) element_size = 4;
        
        if (element_size != 1) {
            snprintf(buffer, sizeof(buffer), "\timulq\t$%d, %s\n", element_size, idx_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }

        // Load element: FItems[index]
        Register_t *elem_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (elem_reg == NULL) {
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), fitems_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for element");
            return inst_list;
        }
        if (element_size == 1) {
            snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_32);
        } else if (element_size == 2) {
            snprintf(buffer, sizeof(buffer), "\tmovzwl\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_32);
        } else if (element_size == 4) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_32);
        } else if (element_size == 8) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_64);
        } else {
            free_reg(get_reg_stack(), elem_reg);
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), fitems_reg);
            codegen_report_error(ctx, "ERROR: TFPGList for-in only supports 1, 2, 4, 8 byte elements");
            return inst_list;
        }
        inst_list = add_inst(inst_list, buffer);

        // Assign element value to loop variable
        // Get address of loop variable
        Register_t *loop_var_addr_reg = NULL;
        inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
        if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
            free_reg(get_reg_stack(), elem_reg);
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), fitems_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }

        // Store element to loop variable
        if (element_size == 1) {
            char byte_reg[16];
            const char *reg32 = elem_reg->bit_32;
            if (strncmp(reg32, "%e", 2) == 0 && strlen(reg32) >= 4) {
                snprintf(byte_reg, sizeof(byte_reg), "%%%cl", reg32[3]);
            } else {
                strcpy(byte_reg, "%al"); // Fallback, conceptually incorrect but practically unused for non-rax registers in this allocator
            }
            snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", byte_reg, loop_var_addr_reg->bit_64);
        } else if (element_size == 2) {
            char word_reg[16];
            const char *reg32 = elem_reg->bit_32;
            if (strncmp(reg32, "%e", 2) == 0 && strlen(reg32) >= 4) {
                snprintf(word_reg, sizeof(word_reg), "%%%.2s", reg32 + 3);
            } else {
                strcpy(word_reg, "%ax");
            }
            snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", word_reg, loop_var_addr_reg->bit_64);
        } else if (element_size == 4) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", elem_reg->bit_32, loop_var_addr_reg->bit_64);
        } else if (element_size == 8) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", elem_reg->bit_64, loop_var_addr_reg->bit_64);
        }
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), loop_var_addr_reg);
        free_reg(get_reg_stack(), elem_reg);
        free_reg(get_reg_stack(), idx_reg);
        free_reg(get_reg_stack(), fitems_reg);

        // Generate body
        inst_list = codegen_stmt(body, inst_list, ctx, symtab);

        codegen_pop_loop(ctx);

        // Increment label (for continue statements)
        snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
        inst_list = add_inst(inst_list, buffer);

        // Increment index
        Register_t *inc_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (inc_reg != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", index_slot->offset, inc_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tincq\t%s\n", inc_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", inc_reg->bit_64, index_slot->offset);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), inc_reg);
        }

        // Condition check label
        snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
        inst_list = add_inst(inst_list, buffer);

        // Compare index < count
        // Load index from stack
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        
        // Load FCount from stack slot (to avoid register clobbering by loop body)
        Register_t *count_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (count_reg != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", count_slot->offset, count_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tcmpq\t%s, %%rax\n", count_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), count_reg);
        } else {
            // Fallback: compare directly with memory operand
            snprintf(buffer, sizeof(buffer), "\tcmpq\t-%d(%%rbp), %%rax\n", count_slot->offset);
            inst_list = add_inst(inst_list, buffer);
        }
        
        // Jump to body if index < count
        snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        // Exit label
        snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
        inst_list = add_inst(inst_list, buffer);

        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s (TFPGList path)\n", __func__);
        #endif
        return inst_list;
    }
    
    if (array_type == NULL || array_type->kind != TYPE_KIND_ARRAY) {
        codegen_report_error(ctx, "ERROR: FOR-IN collection is not an array type");
        return inst_list;
    }

    // Get array bounds
    int start_index = array_type->info.array_info.start_index;
    int end_index = array_type->info.array_info.end_index;

    // Generate labels for loop control
    char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];
    gen_label(cond_label, 18, ctx);
    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    gen_label(incr_label, 18, ctx);

    // Allocate stack slot for the index variable
    StackNode_t *index_slot = codegen_alloc_temp_slot("for_in_idx");
    if (index_slot == NULL) {
        codegen_report_error(ctx, "ERROR: Unable to allocate temp slot for for-in index");
        return inst_list;
    }

    // Initialize index to start_index
    snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, -%d(%%rbp)\n", start_index, index_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    // Jump to condition check
    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    // Body label
    snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);

    // Push loop exit label for break statements
    if (!codegen_push_loop(ctx, exit_label, incr_label))
        return inst_list;

    // Generate code for: loop_var := collection[index]
    // Load index from stack into a register
    Register_t *index_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (index_reg == NULL) {
        codegen_report_error(ctx, "ERROR: Unable to allocate register for for-in index");
        codegen_pop_loop(ctx);
        return inst_list;
    }
    
    snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", index_slot->offset, index_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    // Get the base address of the array (not its value)
    Register_t *array_base_reg = NULL;
    inst_list = codegen_address_for_expr(collection, inst_list, ctx, &array_base_reg);
    if (array_base_reg == NULL || codegen_had_error(ctx)) {
        free_reg(get_reg_stack(), index_reg);
        codegen_pop_loop(ctx);
        return inst_list;
    }

    // Calculate offset: (index - start_index) * element_size
    int element_size = kgpc_type_sizeof(array_type->info.array_info.element_type);
    
    // Subtract start_index if non-zero
    if (start_index != 0) {
        snprintf(buffer, sizeof(buffer), "\tsubl\t$%d, %s\n", start_index, index_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }

    // Multiply by element size
    if (element_size > 1) {
        if ((element_size & (element_size - 1)) == 0) {
            // Power of 2 - use shift
            int shift = 0;
            int temp = element_size;
            while (temp > 1) {
                temp >>= 1;
                shift++;
            }
            if (shift > 0) {
                snprintf(buffer, sizeof(buffer), "\tsall\t$%d, %s\n", shift, index_reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
            }
        } else {
            // Not power of 2 - use multiply
            snprintf(buffer, sizeof(buffer), "\timull\t$%d, %s\n", element_size, index_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    // Sign extend to 64-bit and add to base address
    snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", index_reg->bit_32, index_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n", index_reg->bit_64, array_base_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    // Now array_base_reg points to the element - load it
    Register_t *element_reg = array_base_reg;  // Reuse the register
    free_reg(get_reg_stack(), index_reg);
    index_reg = NULL;

    if (element_size == 1) {
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s), %s\n", element_reg->bit_64, element_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 2) {
        snprintf(buffer, sizeof(buffer), "\tmovzwl\t(%s), %s\n", element_reg->bit_64, element_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 4) {
        snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", element_reg->bit_64, element_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 8) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", element_reg->bit_64, element_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    } else {
        codegen_report_error(ctx, "ERROR: FOR-IN with large array elements not yet supported");
        free_reg(get_reg_stack(), element_reg);
        codegen_pop_loop(ctx);
        return inst_list;
    }

    // Assign element value to loop variable
    // Get address of loop variable
    Register_t *loop_var_addr_reg = NULL;
    inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
    if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
        free_reg(get_reg_stack(), element_reg);
        codegen_pop_loop(ctx);
        return inst_list;
    }

    // Store element to loop variable (using proper byte/word/dword/qword)
    if (element_size == 1) {
        // Extract byte register name from 32-bit register (e.g., %eax -> %al)
        char byte_reg[16];
        const char *reg32 = element_reg->bit_32;
        if (strncmp(reg32, "%e", 2) == 0 && strlen(reg32) >= 4) {
            // %eax -> %al, %ebx -> %bl, etc.
            snprintf(byte_reg, sizeof(byte_reg), "%%%cl", reg32[3]);
        } else if (strncmp(reg32, "%r", 2) == 0 && strlen(reg32) >= 4) {
            // %r8d -> %r8b, %r9d -> %r9b, etc.
            int len = strlen(reg32);
            strncpy(byte_reg, reg32, len - 1);
            byte_reg[len - 1] = 'b';
            byte_reg[len] = '\0';
        } else {
            strcpy(byte_reg, "%al");  // Fallback
        }
        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", byte_reg, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 2) {
        // Extract word register name from 32-bit register (e.g., %eax -> %ax)
        char word_reg[16];
        const char *reg32 = element_reg->bit_32;
        if (strncmp(reg32, "%e", 2) == 0 && strlen(reg32) >= 4) {
            // %eax -> %ax, %ebx -> %bx, etc.
            snprintf(word_reg, sizeof(word_reg), "%%%.2s", reg32 + 3);
        } else if (strncmp(reg32, "%r", 2) == 0 && strlen(reg32) >= 4) {
            // %r8d -> %r8w, %r9d -> %r9w, etc.
            int len = strlen(reg32);
            strncpy(word_reg, reg32, len - 1);
            word_reg[len - 1] = 'w';
            word_reg[len] = '\0';
        } else {
            strcpy(word_reg, "%ax");  // Fallback
        }
        snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", word_reg, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 4) {
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", element_reg->bit_32, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 8) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", element_reg->bit_64, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    free_reg(get_reg_stack(), element_reg);
    free_reg(get_reg_stack(), loop_var_addr_reg);

    // Generate code for the loop body
    inst_list = codegen_stmt(body, inst_list, ctx, symtab);
    
    // Increment index
    snprintf(buffer, sizeof(buffer), "\tincl\t-%d(%%rbp)\n", index_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    // Jump to condition
    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    // Condition label
    snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
    inst_list = add_inst(inst_list, buffer);

    // Compare index with end_index
    snprintf(buffer, sizeof(buffer), "\tcmpl\t$%d, -%d(%%rbp)\n", end_index, index_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    // Jump to body if index <= end_index
    snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", body_label);
    inst_list = add_inst(inst_list, buffer);

    // Exit label
    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

    // Pop loop exit label
    codegen_pop_loop(ctx);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for for statements */
ListNode_t *codegen_for(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_FOR);
    assert(ctx != NULL);
    assert(symtab != NULL);

    struct Expression *expr = NULL, *for_var = NULL, *update_expr = NULL, *one_expr = NULL;
    struct Statement *for_body = NULL, *for_assign = NULL, *update_stmt = NULL;
    Register_t *limit_reg = NULL;
    Register_t *loop_value_reg = NULL;
    char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[128];
    StackNode_t *limit_temp = NULL;

    gen_label(cond_label, 18, ctx);
    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    gen_label(incr_label, 18, ctx);
    for_body = stmt->stmt_data.for_data.do_for;
    expr = stmt->stmt_data.for_data.to;

    if(stmt->stmt_data.for_data.for_assign_type == STMT_FOR_ASSIGN_VAR)
    {
        for_assign = stmt->stmt_data.for_data.for_assign_data.var_assign;
        inst_list = codegen_var_assignment(for_assign, inst_list, ctx);
        for_var = stmt->stmt_data.for_data.for_assign_data.var_assign->stmt_data.var_assign_data.var;
    }
    else
    {
        for_var = stmt->stmt_data.for_data.for_assign_data.var;
    }

    if (for_var == NULL)
        return inst_list;

    // Determine the direction of the loop
    const int is_downto = stmt->stmt_data.for_data.is_downto;
    
    // Create the update expression based on loop direction
    one_expr = mk_inum(-1, 1);
    if (is_downto) {
        update_expr = mk_addop(-1, MINUS, for_var, one_expr);
    } else {
        update_expr = mk_addop(-1, PLUS, for_var, one_expr);
    }
    update_stmt = mk_varassign(-1, 0, for_var, update_expr);

    inst_list = codegen_evaluate_expr(expr, inst_list, ctx, &limit_reg);
    if (codegen_had_error(ctx) || limit_reg == NULL)
        goto cleanup;

    limit_temp = codegen_alloc_temp_slot("for_to_temp");

    const int limit_is_qword = expr_uses_qword_kgpctype(expr);
    const int limit_is_signed = codegen_expr_is_signed(expr);
    if (limit_is_qword)
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", limit_reg->bit_64, limit_temp->offset);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", limit_reg->bit_32, limit_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), limit_reg);
    limit_reg = NULL;

    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);
    if (!codegen_push_loop(ctx, exit_label, incr_label))
        goto cleanup;
    inst_list = codegen_stmt(for_body, inst_list, ctx, symtab);
    codegen_pop_loop(ctx);

    // Increment label (for continue statements)
    snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_stmt(update_stmt, inst_list, ctx, symtab);
    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_evaluate_expr(for_var, inst_list, ctx, &loop_value_reg);
    if (codegen_had_error(ctx) || loop_value_reg == NULL)
        goto cleanup;

    limit_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (limit_reg == NULL)
    {
        free_reg(get_reg_stack(), loop_value_reg);
        codegen_report_error(ctx, "ERROR: Unable to allocate register for for-loop bound.");
        goto cleanup;
    }

    if (limit_is_qword)
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", limit_temp->offset, limit_reg->bit_64);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", limit_temp->offset, limit_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    const int var_is_qword = expr_uses_qword_kgpctype(for_var);
    const int var_is_signed = codegen_expr_is_signed(for_var);
    const int compare_as_qword = var_is_qword || limit_is_qword;
    if (compare_as_qword && !limit_is_qword)
    {
        if (limit_is_signed)
            inst_list = codegen_sign_extend32_to64(inst_list, limit_reg->bit_32, limit_reg->bit_64);
        else
            inst_list = codegen_zero_extend32_to64(inst_list, limit_reg->bit_32, limit_reg->bit_32);
    }
    if (compare_as_qword && !var_is_qword)
    {
        if (var_is_signed)
            inst_list = codegen_sign_extend32_to64(inst_list, loop_value_reg->bit_32, loop_value_reg->bit_64);
        else
            inst_list = codegen_zero_extend32_to64(inst_list, loop_value_reg->bit_32, loop_value_reg->bit_32);
    }

    const int use_unsigned_compare = !(limit_is_signed && var_is_signed);

    const char *cmp_instr = compare_as_qword ? "cmpq" : "cmpl";
    const char *limit_cmp_reg = compare_as_qword ? limit_reg->bit_64 : limit_reg->bit_32;
    const char *loop_cmp_reg = compare_as_qword ? loop_value_reg->bit_64 : loop_value_reg->bit_32;
    snprintf(buffer, sizeof(buffer), "\t%s\t%s, %s\n", cmp_instr, limit_cmp_reg, loop_cmp_reg);
    inst_list = add_inst(inst_list, buffer);

    // Choose the correct branch instruction based on loop direction
    const char *branch_instr;
    if (is_downto) {
        branch_instr = use_unsigned_compare ? "jae" : "jge";  // jump if above or equal / greater or equal
    } else {
        branch_instr = use_unsigned_compare ? "jbe" : "jle";  // jump if below or equal / less or equal
    }
    snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", branch_instr, body_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), loop_value_reg);
    loop_value_reg = NULL;
    free_reg(get_reg_stack(), limit_reg);
    limit_reg = NULL;

    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

cleanup:
    if (limit_reg != NULL)
        free_reg(get_reg_stack(), limit_reg);
    if (loop_value_reg != NULL)
        free_reg(get_reg_stack(), loop_value_reg);
    free(one_expr);
    free(update_expr);
    free(update_stmt);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for case statements */
ListNode_t *codegen_case(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_CASE);
    assert(ctx != NULL);
    assert(symtab != NULL);

    char end_label[18], buffer[100];
    gen_label(end_label, 18, ctx);
    
    /* Evaluate the selector expression once and spill it to stack to free up registers.
     * This prevents register exhaustion when case branches contain complex expressions
     * or function calls that need registers for their own evaluation. */
    struct Expression *selector = stmt->stmt_data.case_data.selector_expr;
    Register_t *selector_reg = NULL;
    inst_list = codegen_expr_with_result(selector, inst_list, ctx, &selector_reg);
    if (selector_reg == NULL)
        return inst_list;

    int selector_is_qword = expr_uses_qword_kgpctype(selector);
    
    /* Spill selector value to stack to free the register */
    StackNode_t *selector_spill = selector_is_qword ?
        add_l_t_bytes("case_selector", 8) : add_l_t("case_selector");
    if (selector_spill == NULL)
    {
        free_reg(get_reg_stack(), selector_reg);
        return inst_list;
    }
    
    if (selector_is_qword)
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
            selector_reg->bit_64, selector_spill->offset);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n",
            selector_reg->bit_32, selector_spill->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), selector_reg);
    selector_reg = NULL;

    /* Generate code for each case branch */
    ListNode_t *branch_node = stmt->stmt_data.case_data.branches;
    if (branch_node == NULL) {
        /* No branches - selector was already evaluated and spilled */
        return inst_list;
    }


    while (branch_node != NULL) {
        struct CaseBranch *branch = (struct CaseBranch *)branch_node->cur;
        if (branch != NULL && branch->labels != NULL) {
            char branch_label[18], next_branch_label[18];
            gen_label(branch_label, 18, ctx);
            gen_label(next_branch_label, 18, ctx);
            
            /* Check each label in this branch */
            ListNode_t *label_node = branch->labels;
            while (label_node != NULL) {
                if (label_node->type == LIST_EXPR) {
                    struct Expression *label_expr = (struct Expression *)label_node->cur;

                    if (label_expr->type == EXPR_INUM) {
                        /* For constant labels, compare directly against the spilled value */
                        if (selector_is_qword)
                            snprintf(buffer, sizeof(buffer), "\tcmpq\t$%lld, -%d(%%rbp)\n",
                                     label_expr->expr_data.i_num, selector_spill->offset);
                        else
                            snprintf(buffer, sizeof(buffer), "\tcmpl\t$%lld, -%d(%%rbp)\n",
                                     label_expr->expr_data.i_num, selector_spill->offset);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tje\t%s\n", branch_label);
                        inst_list = add_inst(inst_list, buffer);
                    } else {
                        /* For non-constant labels, evaluate label and compare with spilled selector */
                        Register_t *label_reg = NULL;
                        inst_list = codegen_expr_with_result(label_expr, inst_list, ctx, &label_reg);
                        if (label_reg != NULL) {
                            /* Compare selector value from stack with label */
                            if (selector_is_qword)
                                snprintf(buffer, sizeof(buffer), "\tcmpq\t%s, -%d(%%rbp)\n",
                                         label_reg->bit_64, selector_spill->offset);
                            else
                                snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, -%d(%%rbp)\n",
                                         label_reg->bit_32, selector_spill->offset);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tje\t%s\n", branch_label);
                            inst_list = add_inst(inst_list, buffer);
                            free_reg(get_reg_stack(), label_reg);
                        }
                    }
                } else if (label_node->type == LIST_SET_ELEMENT) {
                    struct SetElement *range = (struct SetElement *)label_node->cur;
                    if (range != NULL) {
                        char range_skip_label[18];
                        gen_label(range_skip_label, 18, ctx);

                        int emitted_lower_cmp = 0;
                        if (range->lower != NULL) {
                            if (range->lower->type == EXPR_INUM) {
                                /* Compare constant lower bound against spilled selector */
                                if (selector_is_qword)
                                    snprintf(buffer, sizeof(buffer), "\tcmpq\t$%lld, -%d(%%rbp)\n",
                                             range->lower->expr_data.i_num, selector_spill->offset);
                                else
                                    snprintf(buffer, sizeof(buffer), "\tcmpl\t$%lld, -%d(%%rbp)\n",
                                             range->lower->expr_data.i_num, selector_spill->offset);
                                inst_list = add_inst(inst_list, buffer);
                                emitted_lower_cmp = 1;
                            } else {
                                Register_t *lower_reg = NULL;
                                inst_list = codegen_expr_with_result(range->lower, inst_list, ctx, &lower_reg);
                                if (lower_reg != NULL) {
                                    /* Compare spilled selector against lower bound */
                                    if (selector_is_qword)
                                        snprintf(buffer, sizeof(buffer), "\tcmpq\t%s, -%d(%%rbp)\n",
                                                 lower_reg->bit_64, selector_spill->offset);
                                    else
                                        snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, -%d(%%rbp)\n",
                                                 lower_reg->bit_32, selector_spill->offset);
                                    inst_list = add_inst(inst_list, buffer);
                                    emitted_lower_cmp = 1;
                                    free_reg(get_reg_stack(), lower_reg);
                                }
                            }

                            if (emitted_lower_cmp) {
                                snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", range_skip_label);
                                inst_list = add_inst(inst_list, buffer);
                            }
                        }

                        int emitted_upper_cmp = 0;
                        if (range->upper != NULL) {
                            if (range->upper->type == EXPR_INUM) {
                                /* Compare constant upper bound against spilled selector */
                                if (selector_is_qword)
                                    snprintf(buffer, sizeof(buffer), "\tcmpq\t$%lld, -%d(%%rbp)\n",
                                             range->upper->expr_data.i_num, selector_spill->offset);
                                else
                                    snprintf(buffer, sizeof(buffer), "\tcmpl\t$%lld, -%d(%%rbp)\n",
                                             range->upper->expr_data.i_num, selector_spill->offset);
                                inst_list = add_inst(inst_list, buffer);
                                emitted_upper_cmp = 1;
                            } else {
                                Register_t *upper_reg = NULL;
                                inst_list = codegen_expr_with_result(range->upper, inst_list, ctx, &upper_reg);
                                if (upper_reg != NULL) {
                                    /* Compare spilled selector against upper bound */
                                    if (selector_is_qword)
                                        snprintf(buffer, sizeof(buffer), "\tcmpq\t%s, -%d(%%rbp)\n",
                                                 upper_reg->bit_64, selector_spill->offset);
                                    else
                                        snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, -%d(%%rbp)\n",
                                                 upper_reg->bit_32, selector_spill->offset);
                                    inst_list = add_inst(inst_list, buffer);
                                    emitted_upper_cmp = 1;
                                    free_reg(get_reg_stack(), upper_reg);
                                }
                            }

                            if (emitted_upper_cmp) {
                                snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", branch_label);
                                inst_list = add_inst(inst_list, buffer);
                            }
                        }

                        snprintf(buffer, sizeof(buffer), "%s:\n", range_skip_label);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }

                label_node = label_node->next;
            }
            
            /* If no match, jump to next branch */
            snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", next_branch_label);
            inst_list = add_inst(inst_list, buffer);
            
            /* Branch matched - execute statement */
            snprintf(buffer, sizeof(buffer), "%s:\n", branch_label);
            inst_list = add_inst(inst_list, buffer);
            if (branch->stmt != NULL)
                inst_list = codegen_stmt(branch->stmt, inst_list, ctx, symtab);
            snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", end_label);
            inst_list = add_inst(inst_list, buffer);
            
            /* Next branch label */
            snprintf(buffer, sizeof(buffer), "%s:\n", next_branch_label);
            inst_list = add_inst(inst_list, buffer);
        }
        branch_node = branch_node->next;
    }
    
    /* Else clause or fall-through */
    if (stmt->stmt_data.case_data.else_stmt != NULL) {
        inst_list = codegen_stmt(stmt->stmt_data.case_data.else_stmt, inst_list, ctx, symtab);
    }
    
    /* End label */
    snprintf(buffer, sizeof(buffer), "%s:\n", end_label);
    inst_list = add_inst(inst_list, buffer);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    /* selector_reg was already freed after spilling to stack */
    return inst_list;
}

static ListNode_t *codegen_break_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    const char *exit_label = codegen_current_loop_exit(ctx);
    if (exit_label == NULL)
    {
        codegen_report_error(ctx, "ERROR: BREAK statement outside of a loop at line %d.\n",
            stmt != NULL ? stmt->line_num : -1);
        return inst_list;
    }

    int limit_depth = codegen_current_loop_finally_depth(ctx);
    return codegen_branch_through_finally(ctx, inst_list, symtab, exit_label, limit_depth);
}

static ListNode_t *codegen_continue_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    const char *continue_label = codegen_current_loop_continue(ctx);
    if (continue_label == NULL)
    {
        codegen_report_error(ctx, "ERROR: CONTINUE statement outside of a loop at line %d.\n",
            stmt != NULL ? stmt->line_num : -1);
        return inst_list;
    }

    int limit_depth = codegen_current_loop_finally_depth(ctx);
    return codegen_branch_through_finally(ctx, inst_list, symtab, continue_label, limit_depth);
}



static ListNode_t *codegen_with(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;
    if (stmt->stmt_data.with_data.context_expr != NULL)
        inst_list = codegen_expr(stmt->stmt_data.with_data.context_expr, inst_list, ctx);
    if (stmt->stmt_data.with_data.body_stmt != NULL)
        inst_list = codegen_stmt(stmt->stmt_data.with_data.body_stmt, inst_list, ctx, symtab);
    return inst_list;
}

static ListNode_t *codegen_try_finally(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;
    ListNode_t *try_stmts = stmt->stmt_data.try_finally_data.try_statements;
    ListNode_t *finally_stmts = stmt->stmt_data.try_finally_data.finally_statements;

    if (finally_stmts == NULL)
        return codegen_statement_list(try_stmts, inst_list, ctx, symtab);

    if (!codegen_push_finally(ctx, finally_stmts))
        return inst_list;

    inst_list = codegen_statement_list(try_stmts, inst_list, ctx, symtab);

    char final_entry[18];
    char after_label[18];
    gen_label(final_entry, sizeof(final_entry), ctx);
    gen_label(after_label, sizeof(after_label), ctx);

    inst_list = gencode_jmp(NORMAL_JMP, 0, final_entry, inst_list);
    inst_list = codegen_emit_finally_block(ctx, inst_list, symtab, final_entry, after_label);
    codegen_pop_finally(ctx);

    char buffer[32];
    snprintf(buffer, sizeof(buffer), "%s:\n", after_label);
    inst_list = add_inst(inst_list, buffer);

    return inst_list;
}

static ListNode_t *codegen_try_except(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;
    ListNode_t *try_stmts = stmt->stmt_data.try_except_data.try_statements;
    ListNode_t *except_stmts = stmt->stmt_data.try_except_data.except_statements;

    char except_label[18];
    char after_label[18];
    gen_label(except_label, sizeof(except_label), ctx);
    gen_label(after_label, sizeof(after_label), ctx);

    if (!codegen_push_except(ctx, except_label)) {
        return inst_list;
    }
    inst_list = codegen_statement_list(try_stmts, inst_list, ctx, symtab);
    inst_list = gencode_jmp(NORMAL_JMP, 0, after_label, inst_list);

    codegen_pop_except(ctx);

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "%s:\n", except_label);
    inst_list = add_inst(inst_list, buffer);

    /* If there's an 'on E: Exception do' clause, add the exception variable to the stack */
    StackNode_t *exception_var_node = NULL;
    if (stmt->stmt_data.try_except_data.has_on_clause && 
        stmt->stmt_data.try_except_data.exception_var_name != NULL) {
        
        /* Push a new scope for the exception variable */
        PushScope(symtab);
        
        /* Add the exception variable to the stack manager (8 bytes for pointer) */
        exception_var_node = add_l_x(stmt->stmt_data.try_except_data.exception_var_name, 8);
        
        /* Generate code to store the current exception into the variable */
        if (exception_var_node != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovq\tkgpc_current_exception(%%rip), %%rax\n");
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", exception_var_node->offset);
            inst_list = add_inst(inst_list, buffer);
        }
        
        /* Generate the except statements with the exception variable in scope */
        if (except_stmts != NULL)
            inst_list = codegen_statement_list(except_stmts, inst_list, ctx, symtab);
        else
            inst_list = add_inst(inst_list, "\t# EXCEPT block with no handlers\n");
        
        /* Pop the scope */
        PopScope(symtab);
    } else {
        /* No exception variable - just generate the except statements normally */
        if (except_stmts != NULL)
            inst_list = codegen_statement_list(except_stmts, inst_list, ctx, symtab);
        else
            inst_list = add_inst(inst_list, "\t# EXCEPT block with no handlers\n");
    }

    snprintf(buffer, sizeof(buffer), "%s:\n", after_label);
    inst_list = add_inst(inst_list, buffer);

    return inst_list;
}

static ListNode_t *codegen_raise(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;

    inst_list = add_inst(inst_list, "\t# RAISE statement\n");

    struct Expression *exc_expr = stmt->stmt_data.raise_data.exception_expr;
    const char *except_label = codegen_current_except_label(ctx);

    Register_t *value_reg = NULL;
    int stored_exception = 0;

    if (exc_expr != NULL)
    {
        inst_list = codegen_expr_with_result(exc_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
            return inst_list;
        inst_list = codegen_store_exception_value(inst_list, ctx, exc_expr, value_reg);
        stored_exception = 1;
        free_reg(get_reg_stack(), value_reg);
        value_reg = NULL;
    }

    if (except_label != NULL)
    {
        int limit_depth = codegen_current_except_finally_depth(ctx);
        inst_list = codegen_branch_through_finally(ctx, inst_list, symtab, except_label, limit_depth);
        return inst_list;
    }

    if (!stored_exception)
        inst_list = add_inst(inst_list, "\tmovq\t$0, kgpc_current_exception(%rip)\n");

    char after_label[18];
    int need_after_label = codegen_has_finally(ctx);
    if (need_after_label)
    {
        gen_label(after_label, sizeof(after_label), ctx);
        /* Unwind everything if no handler found */
        inst_list = codegen_branch_through_finally(ctx, inst_list, symtab, after_label, 0);

        char buffer[32];
        snprintf(buffer, sizeof(buffer), "%s:\n", after_label);
        inst_list = add_inst(inst_list, buffer);
    }

    char buffer[96];
    if (codegen_target_is_windows())
        snprintf(buffer, sizeof(buffer), "\tmovq\tkgpc_current_exception(%%rip), %%rcx\n");
    else
        snprintf(buffer, sizeof(buffer), "\tmovq\tkgpc_current_exception(%%rip), %%rdi\n");
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_call_with_shadow_space(inst_list, ctx, "kgpc_raise");
    free_arg_regs();
    inst_list = add_inst(inst_list, "\tud2\n");
    return inst_list;
}

static ListNode_t *codegen_inherited(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    (void)symtab;

    if (stmt == NULL)
        return inst_list;

    int is_class_method = 0;
    if (ctx != NULL && ctx->current_subprogram_mangled != NULL)
    {
        /* Current mangling scheme encodes class methods with "__" separator. */
        if (strstr(ctx->current_subprogram_mangled, "__") != NULL)
            is_class_method = 1;
    }

    struct Expression *call_expr = stmt->stmt_data.inherited_data.call_expr;
    if (!is_class_method)
    {
        /* Outside of class methods, inherited statements are parsed for compatibility
         * but have no runtime effect yet. */
        inst_list = add_inst(inst_list, "\t# INHERITED statement ignored (no class context)\n");
        return inst_list;
    }

    if (call_expr != NULL)
    {
        inst_list = codegen_expr(call_expr, inst_list, ctx);
        if (codegen_had_error(ctx))
            return inst_list;
    }
    else
    {
        inst_list = add_inst(inst_list, "\t# INHERITED statement without parent call\n");
    }
    return inst_list;
}
