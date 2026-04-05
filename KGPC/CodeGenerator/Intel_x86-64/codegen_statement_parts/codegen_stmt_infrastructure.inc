static ListNode_t *codegen_spill_call_arg_regs_stmt(ListNode_t *inst_list,
    int *int_offsets, int *xmm_offsets)
{
    char buffer[128];
    for (int i = 0; i < kgpc_max_int_arg_regs(); i++)
    {
        const char *reg = current_arg_reg64(i);
        StackNode_t *slot = add_l_t_bytes("__intf_stmt_arg_int", 8);
        int_offsets[i] = slot != NULL ? slot->offset : 0;
        if (slot != NULL && reg != NULL)
        {
            asm_emit_movq_to_stack(buffer, sizeof(buffer), reg, slot->offset);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    for (int i = 0; i < kgpc_max_sse_arg_regs(); i++)
    {
        const char *reg = current_arg_reg_xmm(i);
        StackNode_t *slot = add_l_t_bytes("__intf_stmt_arg_xmm", 16);
        xmm_offsets[i] = slot != NULL ? slot->offset : 0;
        if (slot != NULL && reg != NULL)
        {
            asm_emit_movdqu_to_stack(buffer, sizeof(buffer), reg, slot->offset);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    return inst_list;
}

static ListNode_t *codegen_restore_call_arg_regs_stmt(ListNode_t *inst_list,
    const int *int_offsets, const int *xmm_offsets)
{
    char buffer[128];
    for (int i = 0; i < kgpc_max_int_arg_regs(); i++)
    {
        const char *reg = current_arg_reg64(i);
        if (reg != NULL && int_offsets[i] > 0)
        {
            asm_emit_movq_from_stack(buffer, sizeof(buffer), int_offsets[i], reg);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    for (int i = 0; i < kgpc_max_sse_arg_regs(); i++)
    {
        const char *reg = current_arg_reg_xmm(i);
        if (reg != NULL && xmm_offsets[i] > 0)
        {
            asm_emit_movdqu_from_stack(buffer, sizeof(buffer), xmm_offsets[i], reg);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    return inst_list;
}

static ListNode_t *codegen_fail_register(CodeGenContext *ctx, ListNode_t *inst_list,
    Register_t **out_reg, const char *message);
static int codegen_is_current_return_var_id(const struct Expression *expr, CodeGenContext *ctx);
struct KgpcType *expr_get_kgpc_type(const struct Expression *expr);
static struct RecordField *codegen_lookup_record_field(struct Expression *record_access_expr);
static struct RecordField *codegen_lookup_record_field_expr(struct Expression *record_access_expr,
    CodeGenContext *ctx);
static int codegen_local_type_tag_size(int type_tag);
static void codegen_hydrate_array_literal_from_lhs(struct Expression *lhs_expr,
    struct Expression *rhs_expr, CodeGenContext *ctx);

static int codegen_local_type_tag_size(int type_tag)
{
    /* Use centralized type size mapping from type_tags.h */
    return get_type_tag_size(type_tag);
}

static void codegen_hydrate_array_literal_from_lhs(struct Expression *lhs_expr,
    struct Expression *rhs_expr, CodeGenContext *ctx)
{
    if (lhs_expr == NULL || rhs_expr == NULL || ctx == NULL ||
        rhs_expr->type != EXPR_ARRAY_LITERAL)
        return;

    if (rhs_expr->array_element_size > 0 &&
        (rhs_expr->array_element_type != UNKNOWN_TYPE ||
         rhs_expr->array_element_type_id != NULL))
        return;

    if (lhs_expr->is_array_expr ||
        lhs_expr->array_element_type != UNKNOWN_TYPE ||
        lhs_expr->array_element_type_id != NULL ||
        lhs_expr->array_element_size > 0)
    {
        if (rhs_expr->array_element_type == UNKNOWN_TYPE &&
            lhs_expr->array_element_type != UNKNOWN_TYPE)
            rhs_expr->array_element_type = lhs_expr->array_element_type;
        if (rhs_expr->array_element_type_id == NULL &&
            lhs_expr->array_element_type_id != NULL)
            rhs_expr->array_element_type_id = strdup(lhs_expr->array_element_type_id);
        if (rhs_expr->array_element_size <= 0)
        {
            if (lhs_expr->array_element_size > 0)
                rhs_expr->array_element_size = lhs_expr->array_element_size;
            else if (lhs_expr->array_element_type != UNKNOWN_TYPE)
            {
                int elem_size = codegen_local_type_tag_size(lhs_expr->array_element_type);
                if (elem_size > 0)
                    rhs_expr->array_element_size = elem_size;
            }
        }
    }

    if (rhs_expr->array_element_size > 0 &&
        (rhs_expr->array_element_type != UNKNOWN_TYPE ||
         rhs_expr->array_element_type_id != NULL))
        return;

    if (lhs_expr->type == EXPR_RECORD_ACCESS)
    {
        struct RecordField *field = codegen_lookup_record_field_expr(lhs_expr, ctx);
        if (field != NULL && field->is_array)
        {
            if (rhs_expr->array_element_type == UNKNOWN_TYPE &&
                field->array_element_type != UNKNOWN_TYPE)
                rhs_expr->array_element_type = field->array_element_type;
            if (rhs_expr->array_element_type_id == NULL &&
                field->array_element_type_id != NULL)
                rhs_expr->array_element_type_id = strdup(field->array_element_type_id);
            if (rhs_expr->array_element_size <= 0 &&
                field->array_element_type != UNKNOWN_TYPE)
            {
                int elem_size = codegen_local_type_tag_size(field->array_element_type);
                if (elem_size > 0)
                    rhs_expr->array_element_size = elem_size;
            }
        }
    }

    if (rhs_expr->array_element_size > 0 &&
        (rhs_expr->array_element_type != UNKNOWN_TYPE ||
         rhs_expr->array_element_type_id != NULL))
        return;

    KgpcType *lhs_type = expr_get_kgpc_type(lhs_expr);
    if (lhs_type == NULL && ctx->symtab != NULL && lhs_expr->type == EXPR_VAR_ID &&
        lhs_expr->expr_data.id != NULL)
    {
        HashNode_t *lhs_node = NULL;
        if (FindSymbol(&lhs_node, ctx->symtab, lhs_expr->expr_data.id) != 0 &&
            lhs_node != NULL)
        {
            lhs_type = lhs_node->type;
        }
    }
    if (lhs_type == NULL || !kgpc_type_is_array(lhs_type))
        return;

    KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(lhs_type, ctx->symtab);
    if (elem_type == NULL)
        return;

    if (rhs_expr->array_element_type == UNKNOWN_TYPE)
        rhs_expr->array_element_type = codegen_tag_from_kgpc(elem_type);

    if (rhs_expr->array_element_type_id == NULL)
    {
        const char *elem_type_id = NULL;
        if (elem_type->type_alias != NULL && elem_type->type_alias->alias_name != NULL)
            elem_type_id = elem_type->type_alias->alias_name;
        else if (elem_type->kind == TYPE_KIND_RECORD &&
                 elem_type->info.record_info != NULL &&
                 elem_type->info.record_info->type_id != NULL)
            elem_type_id = elem_type->info.record_info->type_id;
        else if (elem_type->kind == TYPE_KIND_ARRAY &&
                 elem_type->info.array_info.element_type_id != NULL)
            elem_type_id = elem_type->info.array_info.element_type_id;
        else if (elem_type->type_alias != NULL &&
                 elem_type->type_alias->target_type_id != NULL)
            elem_type_id = elem_type->type_alias->target_type_id;

        if (elem_type_id != NULL)
            rhs_expr->array_element_type_id = strdup(elem_type_id);
    }

    if (rhs_expr->array_element_size <= 0)
    {
        long long elem_size = kgpc_type_sizeof(elem_type);
        if (elem_size > 0 && elem_size <= INT_MAX)
            rhs_expr->array_element_size = (int)elem_size;
    }
}

static ListNode_t *codegen_emit_cmp_spill_immediate(ListNode_t *inst_list,
    CodeGenContext *ctx, int compare_as_qword, long long imm_value,
    int spill_offset)
{
    char buffer[128];

    if (compare_as_qword && (imm_value > INT32_MAX || imm_value < INT32_MIN))
    {
        Register_t *imm_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (imm_reg == NULL)
            imm_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (imm_reg == NULL)
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for 64-bit compare immediate.");

        snprintf(buffer, sizeof(buffer), "\tmovabsq\t$%lld, %s\n",
            imm_value, imm_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tcmpq\t%s, -%d(%%rbp)\n",
            imm_reg->bit_64, spill_offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), imm_reg);
        return inst_list;
    }

    if (compare_as_qword)
        snprintf(buffer, sizeof(buffer), "\tcmpq\t$%lld, -%d(%%rbp)\n",
            imm_value, spill_offset);
    else
        snprintf(buffer, sizeof(buffer), "\tcmpl\t$%lld, -%d(%%rbp)\n",
            imm_value, spill_offset);

    return add_inst(inst_list, buffer);
}

#include "codegen_expression.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/ident_ref.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/KgpcType.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../identifier_utils.h"

static int codegen_statement_return_storage_size(KgpcType *return_type)
{
    if (return_type == NULL)
        return 0;

    if (kgpc_type_is_shortstring(return_type))
    {
        long long short_size = kgpc_type_sizeof(return_type);
        if (short_size > 1 && short_size <= INT_MAX)
            return (int)short_size;
        return 256;
    }

    long long type_size = kgpc_type_sizeof(return_type);
    if (type_size > 0 && type_size <= INT_MAX)
        return (int)type_size;

    if (return_type->kind == TYPE_KIND_POINTER)
        return 8;

    if (return_type->kind == TYPE_KIND_PRIMITIVE)
    {
        int tag = kgpc_type_get_primitive_tag(return_type);
        if (tag == EXTENDED_TYPE)
            return 10;
        if (tag == STRING_TYPE || tag == POINTER_TYPE || tag == INT64_TYPE || tag == QWORD_TYPE || tag == REAL_TYPE)
            return 8;
        return 4;
    }

    return 0;
}
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_expr.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_stmt.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_sizeof.h"
#include "../../Parser/ParseTree/from_cparser.h"


/* Cached getenv() — defined in SemCheck.c */
extern const char *kgpc_getenv(const char *name);
#ifndef CODEGEN_POINTER_SIZE_BYTES
#define CODEGEN_POINTER_SIZE_BYTES 8
#endif

/* Helper function to get RecordType from HashNode */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    return hashnode_get_record_type(node);
}

static int codegen_push_loop(CodeGenContext *ctx, const char *exit_label, const char *continue_label);
static void codegen_pop_loop(CodeGenContext *ctx);
static const char *codegen_current_loop_exit(const CodeGenContext *ctx);
static const char *codegen_current_loop_continue(const CodeGenContext *ctx);
static int codegen_with_push(CodeGenContext *ctx, struct Expression *context_expr,
    struct RecordType *record_type);
static void codegen_with_pop(CodeGenContext *ctx);
static void codegen_get_current_return_info(CodeGenContext *ctx, SymTab_t *symtab,
    int *out_is_real, int *out_size);
static void codegen_get_current_return_slot_info(CodeGenContext *ctx,
    int *out_is_real, int *out_size);
static int codegen_get_current_return_shortstring_capacity(CodeGenContext *ctx, SymTab_t *symtab);
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

static HashNode_t *codegen_find_zero_arg_method_node(SymTab_t *symtab,
    const struct RecordType *record, const char *method_name)
{
    if (symtab == NULL || record == NULL || record->type_id == NULL || method_name == NULL)
        return NULL;

    size_t len = strlen(record->type_id) + 2 + strlen(method_name) + 1;
    char *base_name = (char *)malloc(len);
    if (base_name == NULL)
        return NULL;

    snprintf(base_name, len, "%s__%s", record->type_id, method_name);
    ListNode_t *candidates = FindAllIdents(symtab, base_name);
    free(base_name);

    HashNode_t *match = NULL;
    for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
    {
        HashNode_t *cand = (HashNode_t *)cur->cur;
        Tree_t *first_param = NULL;
        const char *first_param_name = NULL;
        if (cand == NULL || cand->type == NULL)
            continue;
        if (cand->hash_type != HASHTYPE_FUNCTION && cand->hash_type != HASHTYPE_PROCEDURE)
            continue;
        ListNode_t *params = kgpc_type_get_procedure_params(cand->type);
        if (params != NULL && params->cur != NULL)
            first_param = (Tree_t *)params->cur;
        if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
            first_param->tree_data.var_decl_data.ids != NULL)
            first_param_name = (const char *)first_param->tree_data.var_decl_data.ids->cur;
        else if (first_param != NULL && first_param->type == TREE_ARR_DECL &&
                 first_param->tree_data.arr_decl_data.ids != NULL)
            first_param_name = (const char *)first_param->tree_data.arr_decl_data.ids->cur;

        if (!((ListLength(params) == 0) ||
              (ListLength(params) == 1 && first_param_name != NULL &&
               strcasecmp(first_param_name, "Self") == 0)))
            continue;
        match = cand;
        break;
    }

    DestroyList(candidates);
    return match;
}

static struct ClassProperty *codegen_find_class_property(const struct RecordType *record,
    const char *property_name)
{
    if (record == NULL || property_name == NULL)
        return NULL;

    for (ListNode_t *cur = record->properties; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_CLASS_PROPERTY || cur->cur == NULL)
            continue;
        struct ClassProperty *prop = (struct ClassProperty *)cur->cur;
        if (prop->name != NULL && pascal_identifier_equals(prop->name, property_name))
            return prop;
    }
    return NULL;
}

static int codegen_get_enumerator_current_info(SymTab_t *symtab, struct RecordType *enum_record,
    HashNode_t **out_current_node, KgpcType **out_current_type)
{
    if (out_current_node != NULL)
        *out_current_node = NULL;
    if (out_current_type != NULL)
        *out_current_type = NULL;
    if (symtab == NULL || enum_record == NULL)
        return 0;

    struct ClassProperty *current_prop = codegen_find_class_property(enum_record, "Current");
    if (current_prop != NULL && current_prop->read_accessor != NULL)
    {
        HashNode_t *getter = codegen_find_zero_arg_method_node(symtab, enum_record,
            current_prop->read_accessor);
        if (getter != NULL && getter->type != NULL)
        {
            KgpcType *ret = kgpc_type_get_return_type(getter->type);
            if (ret != NULL)
            {
                if (out_current_node != NULL)
                    *out_current_node = getter;
                if (out_current_type != NULL)
                {
                    kgpc_type_retain(ret);
                    *out_current_type = ret;
                }
                return 1;
            }
        }
    }

    HashNode_t *getter = codegen_find_zero_arg_method_node(symtab, enum_record, "GetCurrent");
    if (getter != NULL && getter->type != NULL)
    {
        KgpcType *ret = kgpc_type_get_return_type(getter->type);
        if (ret != NULL)
        {
            if (out_current_node != NULL)
                *out_current_node = getter;
            if (out_current_type != NULL)
            {
                kgpc_type_retain(ret);
                *out_current_type = ret;
            }
            return 1;
        }
    }

    return 0;
}
static ListNode_t *codegen_try_finally(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);

static int codegen_expr_is_string_like(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    if (is_string_type(expr_get_type_tag(expr)))
        return 1;
    if (expr->resolved_kgpc_type != NULL)
    {
        if (kgpc_type_is_shortstring(expr->resolved_kgpc_type))
            return 1;
        if (expr->resolved_kgpc_type->type_alias != NULL &&
            expr->resolved_kgpc_type->type_alias->is_shortstring)
            return 1;
    }
    return 0;
}

static int codegen_enum_type_literal_count(KgpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_PRIMITIVE ||
        type->info.primitive_type_tag != ENUM_TYPE ||
        type->type_alias == NULL || type->type_alias->enum_literals == NULL)
        return 0;
    return ListLength(type->type_alias->enum_literals);
}

static int codegen_set_iteration_upper_bound(CodeGenContext *ctx, KgpcType *set_type)
{
    if (set_type == NULL || !kgpc_type_is_set(set_type))
        return -1;

    if (set_type->type_alias != NULL)
    {
        struct TypeAlias *alias = set_type->type_alias;
        if (alias->is_enum_set && alias->inline_enum_values != NULL)
        {
            int count = ListLength(alias->inline_enum_values);
            if (count > 0)
                return count - 1;
        }

        if (alias->set_element_type_id != NULL && ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *elem_node = NULL;
            if (FindSymbol(&elem_node, ctx->symtab, alias->set_element_type_id) != 0 &&
                elem_node != NULL && elem_node->type != NULL)
            {
                int count = codegen_enum_type_literal_count(elem_node->type);
                if (count > 0)
                    return count - 1;
            }
        }
    }

    {
        long long set_size = kgpc_type_sizeof(set_type);
        if (set_size > 0 && set_size <= INT_MAX)
            return (int)(set_size * 8 - 1);
    }

    return 31;
}
static ListNode_t *codegen_try_except(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_on_exception(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_raise(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_inherited(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static int codegen_expr_is_shortstring_array(const struct Expression *expr);
static int codegen_array_access_targets_shortstring(const struct Expression *expr, CodeGenContext *ctx);
static int codegen_get_shortstring_capacity(const struct Expression *expr, CodeGenContext *ctx);
static int codegen_expr_is_shortstring_rhs(const struct Expression *expr, CodeGenContext *ctx);
static int codegen_expr_is_shortstring_value_local(const struct Expression *expr);
#if KGPC_ENABLE_REG_DEBUG
extern const char *g_reg_debug_context;
#endif

ListNode_t *codegen_emit_const_set_rodata(HashNode_t *node, ListNode_t *inst_list, CodeGenContext *ctx);

ListNode_t *codegen_condition_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, int *relop_type);

/* Emit rodata for a set-typed constant (supports 4-byte or 32-byte payloads) */
ListNode_t *codegen_emit_const_set_rodata(HashNode_t *node, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (node == NULL || ctx == NULL || node->const_set_value == NULL || node->const_set_size <= 0)
        return inst_list;

    if (node->const_set_label == NULL)
    {
        char label[32];
        snprintf(label, sizeof(label), ".LCSET%d", ctx->write_label_counter++);

        const char *readonly_section = codegen_readonly_section_directive();
        char rodata[2048];
        size_t pos = 0;
        pos += (size_t)snprintf(rodata + pos, sizeof(rodata) - pos, "%s\n%s:\n",
            readonly_section, label);

        /* Always emit 32 bytes (max set size = 256 bits) so that char-set
           copies (which read 32 bytes) never touch uninitialized rodata. */
        int emit_size = (node->const_set_size < 32) ? 32 : node->const_set_size;
        for (int i = 0; i < emit_size; ++i)
        {
            if (pos + 24 >= sizeof(rodata))
                break;
            unsigned val = (i < node->const_set_size)
                ? (unsigned)node->const_set_value[i] : 0u;
            pos += (size_t)snprintf(rodata + pos, sizeof(rodata) - pos, "\t.byte %u\n",
                val);
        }

        if (pos + 16 < sizeof(rodata))
            pos += (size_t)snprintf(rodata + pos, sizeof(rodata) - pos, "%s\n", codegen_text_section_resume());

        inst_list = add_inst(inst_list, rodata);
        node->const_set_label = strdup(label);
    }

    return inst_list;
}
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
static ListNode_t *codegen_builtin_setlength_unicodestring(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx);
static ListNode_t *codegen_builtin_setlength_shortstring(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx);
static ListNode_t *codegen_builtin_setstring(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx);
static ListNode_t *codegen_builtin_str(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
static ListNode_t *codegen_builtin_writestr(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
static ListNode_t *codegen_builtin_insert(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
static ListNode_t *codegen_builtin_delete(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
static ListNode_t *codegen_builtin_val(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
static ListNode_t *codegen_builtin_prefetch(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
static int codegen_expr_is_extended_storage(const struct Expression *expr);
static ListNode_t *codegen_assign_extended_value(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx);

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

static int expr_integer_store_size(const struct Expression *expr)
{
    int type_tag = expr_get_type_tag(expr);
    if (expr != NULL && expr->resolved_kgpc_type != NULL)
    {
        int kgpc_size = kgpc_type_sizeof(expr->resolved_kgpc_type);
        if (kgpc_size == 1 || kgpc_size == 2 || kgpc_size == 4 || kgpc_size == 8)
            return kgpc_size;
    }

    switch (type_tag)
    {
        case BYTE_TYPE:
        case CHAR_TYPE:
            return 1;
        case WORD_TYPE:
            return 2;
        case INT_TYPE:
        case LONGINT_TYPE:
        case LONGWORD_TYPE:
        case BOOL:
            return 4;
        case INT64_TYPE:
        case QWORD_TYPE:
            return 8;
        default:
            break;
    }

    if (expr_uses_qword_kgpctype(expr))
        return 8;
    return 4;
}

/**
 * Check if an expression requires 64-bit (qword) storage for integers.
 * This returns true for:
 * - Int64, QWord, UInt64 types (storage_size=8)
 * - Integer literals that don't fit in 32 bits
 * - Const variables whose values don't fit in 32 bits
 */
static int expr_is_64bit_integer(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    /* Use robust helper to detect 64-bit types (Int64, QWord, pointers, etc.) */
    if (expr_uses_qword_kgpctype(expr))
        return 1;

    /* Check for large integer literals that require 64 bits */
    if (expr->type == EXPR_INUM)
    {
        long long val = expr->expr_data.i_num;
        if (val > 2147483647LL || val < -2147483648LL)
            return 1;
    }
    
    return 0;
}

/**
 * Check if an expression's effective value requires 64-bit storage.
 * This is like expr_is_64bit_integer but also checks const variable values
 * by looking up the symbol table.
 */
static int expr_value_requires_64bit(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL)
        return 0;
    
    /* First check the standard 64-bit integer conditions */
    if (expr_is_64bit_integer(expr))
        return 1;
    
    /* For variable references, check if it's a const with a large value */
    if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 && node != NULL)
        {
            if (node->type != NULL)
            {
                long long type_size = kgpc_type_sizeof(node->type);
                if (type_size >= 8)
                    return 1;
            }
            if (node->hash_type == HASHTYPE_CONST)
            {
                long long val = node->const_int_value;
                if (val > 2147483647LL || val < -2147483648LL)
                    return 1;
            }
        }
    }
    
    return 0;
}

/**
 * Check if a variable target is a Single (4-byte float) type.
 * Used to determine when to use 32-bit operations and double-to-single conversion.
 */
static int is_single_float_type(int type_tag, long long storage_size)
{
    return (type_tag == REAL_TYPE && storage_size == 4);
}

/* Check if an expression's type is unsigned */
static int expr_is_unsigned_type(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    int type_tag = expr_get_type_tag(expr);
    if (type_tag == BYTE_TYPE || type_tag == WORD_TYPE || type_tag == LONGWORD_TYPE)
        return 1;

    if (expr->resolved_kgpc_type != NULL &&
        expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE)
    {
        int tag = expr->resolved_kgpc_type->info.primitive_type_tag;
        if (tag == BYTE_TYPE || tag == WORD_TYPE || tag == LONGWORD_TYPE)
            return 1;
    }

    /* Check resolved_kgpc_type for type alias information */
    if (expr->resolved_kgpc_type != NULL && expr->resolved_kgpc_type->type_alias != NULL)
    {
        const char *target_type_id = expr->resolved_kgpc_type->type_alias->target_type_id;
        if (is_unsigned_type_name(target_type_id))
            return 1;
        if (expr->resolved_kgpc_type->type_alias->is_range &&
            expr->resolved_kgpc_type->type_alias->range_known &&
            expr->resolved_kgpc_type->type_alias->range_start >= 0)
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
static struct RecordType *codegen_statement_expr_record_type(const struct Expression *expr,
    SymTab_t *symtab)
{
    if (expr == NULL)
        return NULL;

    if (expr->record_type != NULL)
        return expr->record_type;
    if (expr->array_element_record_type != NULL)
        return expr->array_element_record_type;

    KgpcType *expr_type = expr_get_kgpc_type(expr);
    if (expr_type != NULL)
    {
        if (kgpc_type_is_record(expr_type))
            return kgpc_type_get_record(expr_type);
        if (kgpc_type_is_pointer(expr_type) && expr_type->info.points_to != NULL &&
            kgpc_type_is_record(expr_type->info.points_to))
            return kgpc_type_get_record(expr_type->info.points_to);
    }

    if (expr->type == EXPR_TYPECAST && expr->expr_data.typecast_data.expr != NULL)
        return codegen_statement_expr_record_type(expr->expr_data.typecast_data.expr, symtab);

    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL && symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL)
        {
            struct RecordType *record = get_record_type_from_node(node);
            if (record != NULL)
                return record;
            if (node->type != NULL)
            {
                if (kgpc_type_is_record(node->type))
                    return kgpc_type_get_record(node->type);
                if (kgpc_type_is_pointer(node->type) && node->type->info.points_to != NULL &&
                    kgpc_type_is_record(node->type->info.points_to))
                    return kgpc_type_get_record(node->type->info.points_to);
            }
        }
    }

    if (expr->type == EXPR_RECORD_ACCESS && expr->expr_data.record_access_data.record_expr != NULL)
    {
        struct Expression *base_expr = expr->expr_data.record_access_data.record_expr;
        struct RecordType *base_record = codegen_statement_expr_record_type(base_expr, symtab);
        if (base_record != NULL && expr->expr_data.record_access_data.field_id != NULL &&
            symtab != NULL)
        {
            struct RecordField *field = semcheck_find_class_field_including_hidden(symtab,
                base_record, expr->expr_data.record_access_data.field_id, NULL);
            if (field != NULL)
            {
                if (field->nested_record != NULL)
                    return field->nested_record;
                if (field->type_id != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindSymbol(&type_node, symtab, field->type_id) != 0 && type_node != NULL)
                    {
                        struct RecordType *record = get_record_type_from_node(type_node);
                        if (record != NULL)
                            return record;
                        if (type_node->type != NULL)
                        {
                            if (kgpc_type_is_record(type_node->type))
                                return kgpc_type_get_record(type_node->type);
                            if (kgpc_type_is_pointer(type_node->type) &&
                                type_node->type->info.points_to != NULL &&
                                kgpc_type_is_record(type_node->type->info.points_to))
                            {
                                return kgpc_type_get_record(type_node->type->info.points_to);
                            }
                        }
                    }
                }
            }
        }
    }

    return NULL;
}

static struct RecordField *codegen_lookup_record_field_expr(struct Expression *record_access_expr,
    CodeGenContext *ctx)
{
    if (record_access_expr == NULL ||
        record_access_expr->type != EXPR_RECORD_ACCESS ||
        record_access_expr->expr_data.record_access_data.field_id == NULL)
        return NULL;

    const char *field_id = record_access_expr->expr_data.record_access_data.field_id;
    SymTab_t *symtab = ctx != NULL ? ctx->symtab : NULL;
    struct RecordType *record = codegen_statement_expr_record_type(record_access_expr, symtab);
    if (record == NULL && record_access_expr->expr_data.record_access_data.record_expr != NULL)
    {
        record = codegen_statement_expr_record_type(
            record_access_expr->expr_data.record_access_data.record_expr, symtab);
    }
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
    struct RecordField *field = codegen_lookup_record_field_expr(expr, ctx);
    long long field_size = 0;
    if (field != NULL && !field->is_array)
    {
        const char *field_type_id = field->type_id;
        if (field_type_id == NULL && field->type_ref != NULL)
            field_type_id = type_ref_base_name(field->type_ref);

        if (field->type == REAL_TYPE && field_type_id != NULL)
        {
            if (pascal_identifier_equals(field_type_id, "Single"))
                return 4;
            if (pascal_identifier_equals(field_type_id, "Double") ||
                pascal_identifier_equals(field_type_id, "Real"))
                return 8;
        }

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

    switch (reg->reg_id)
    {
        case REG_RAX: return "%al";
        case REG_RBX: return "%bl";
        case REG_RCX: return "%cl";
        case REG_RDX: return "%dl";
        case REG_RSI: return "%sil";
        case REG_RDI: return "%dil";
        case REG_RBP: return "%bpl";
        case REG_RSP: return "%spl";
        case REG_R8: return "%r8b";
        case REG_R9: return "%r9b";
        case REG_R10: return "%r10b";
        case REG_R11: return "%r11b";
        case REG_R12: return "%r12b";
        case REG_R13: return "%r13b";
        case REG_R14: return "%r14b";
        case REG_R15: return "%r15b";
        default: return NULL;
    }
}

static inline RegisterId_t codegen_arg_reg_id_num(int num)
{
    static const RegisterId_t windows_regs[] = { REG_RCX, REG_RDX, REG_R8, REG_R9 };
    static const RegisterId_t sysv_regs[] = { REG_RDI, REG_RSI, REG_RDX, REG_RCX, REG_R8, REG_R9 };
    const RegisterId_t *regs = (g_current_codegen_abi == KGPC_TARGET_ABI_WINDOWS) ? windows_regs : sysv_regs;
    int limit = kgpc_max_int_arg_regs();
    if (num < 0 || num >= limit)
        return REG_INVALID;
    return regs[num];
}

static ListNode_t *codegen_convert_int_like_to_real(ListNode_t *inst_list,
    Register_t *value_reg, int source_type)
{
    if (inst_list == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (is_64bit_integer_type(source_type) || source_type == LONGINT_TYPE)
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
    if (target_type == REAL_TYPE && is_integer_type(source_type))
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


static unsigned long codegen_next_record_ctor_temp_id(void)
{
    static unsigned long counter = 0;
    return ++counter;
}

static StackNode_t *codegen_alloc_record_ctor_temp(long long size)
{
    if (size <= 0 || size > INT_MAX)
        return NULL;

    char label[32];
    snprintf(label, sizeof(label), "record_ctor_%lu", codegen_next_record_ctor_temp_id());
    return add_l_x(label, (int)size);
}

static ListNode_t *codegen_emit_new_dispose_method_fallback(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx, struct Expression *target_expr,
    struct Expression *method_expr)
{
    if (stmt == NULL || ctx == NULL || ctx->symtab == NULL ||
        target_expr == NULL || method_expr == NULL)
        return inst_list;

    struct Expression *method_clone = clone_expression(method_expr);
    if (method_clone == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to clone TP-style New/Dispose method expression.");
        return inst_list;
    }

    char *method_name = NULL;
    ListNode_t *method_args = NULL;
    if (method_clone->type == EXPR_FUNCTION_CALL)
    {
        const char *fn_id = method_clone->expr_data.function_call_data.id;
        if (fn_id != NULL)
            method_name = strdup(fn_id);
        method_args = method_clone->expr_data.function_call_data.args_expr;
        method_clone->expr_data.function_call_data.args_expr = NULL;
    }
    else if (method_clone->type == EXPR_VAR_ID)
    {
        if (method_clone->expr_data.id != NULL)
            method_name = strdup(method_clone->expr_data.id);
    }

    destroy_expr(method_clone);
    if (method_name == NULL)
    {
        if (method_args != NULL)
            DestroyList(method_args);
        codegen_report_error(ctx, "ERROR: Unsupported TP-style New/Dispose method expression.");
        return inst_list;
    }

    size_t placeholder_len = strlen(method_name) + 3;
    char *placeholder_name = (char *)malloc(placeholder_len);
    if (placeholder_name == NULL)
    {
        free(method_name);
        if (method_args != NULL)
            DestroyList(method_args);
        codegen_report_error(ctx, "ERROR: Unable to allocate TP-style New/Dispose placeholder.");
        return inst_list;
    }
    snprintf(placeholder_name, placeholder_len, "__%s", method_name);

    struct Expression *receiver = mk_pointer_deref(stmt->line_num, clone_expression(target_expr));
    if (receiver == NULL)
    {
        free(placeholder_name);
        free(method_name);
        if (method_args != NULL)
            DestroyList(method_args);
        codegen_report_error(ctx, "ERROR: Unable to create TP-style New/Dispose receiver.");
        return inst_list;
    }

    ListNode_t *call_args = CreateListNode(receiver, LIST_EXPR);
    if (call_args == NULL)
    {
        destroy_expr(receiver);
        free(placeholder_name);
        free(method_name);
        if (method_args != NULL)
            DestroyList(method_args);
        codegen_report_error(ctx, "ERROR: Unable to allocate TP-style New/Dispose args.");
        return inst_list;
    }
    call_args->next = method_args;

    struct Statement *call_stmt = mk_procedurecall(stmt->line_num, placeholder_name, call_args);
    if (call_stmt == NULL)
    {
        DestroyList(call_args);
        free(method_name);
        codegen_report_error(ctx, "ERROR: Unable to create TP-style New/Dispose call.");
        return inst_list;
    }
    call_stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
    call_stmt->stmt_data.procedure_call_data.placeholder_method_name = method_name;

    if (semcheck_stmt(ctx->symtab, call_stmt, INT_MAX) != 0)
    {
        destroy_stmt(call_stmt);
        codegen_report_error(ctx, "ERROR: Failed to semcheck TP-style New/Dispose fallback call.");
        return inst_list;
    }

    inst_list = codegen_stmt(call_stmt, inst_list, ctx, ctx->symtab);
    destroy_stmt(call_stmt);
    return inst_list;
}

static StackNode_t *codegen_alloc_incdec_temp(int size)
{
    char label[32];
    snprintf(label, sizeof(label), "incdec_%lu", codegen_next_temp_suffix());
    return add_l_t_bytes(label, size);
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
    if (FindSymbol(&node, ctx->symtab, type_id) == 0 || node == NULL)
        return NULL;

    return hashnode_get_type_alias(node);
}

static struct RecordField *codegen_lookup_record_field_in_members(ListNode_t *members,
    const char *field_name)
{
    for (ListNode_t *field_node = members; field_node != NULL; field_node = field_node->next)
    {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)field_node->cur;
            if (!record_field_is_hidden(field) && field->name != NULL &&
                strcmp(field->name, field_name) == 0)
                return field;
        }
        else if (field_node->type == LIST_VARIANT_PART && field_node->cur != NULL)
        {
            struct VariantPart *variant = (struct VariantPart *)field_node->cur;
            for (ListNode_t *branch_node = variant->branches; branch_node != NULL;
                 branch_node = branch_node->next)
            {
                if (branch_node->type != LIST_VARIANT_BRANCH || branch_node->cur == NULL)
                    continue;
                struct VariantBranch *branch = (struct VariantBranch *)branch_node->cur;
                struct RecordField *field =
                    codegen_lookup_record_field_in_members(branch->members, field_name);
                if (field != NULL)
                    return field;
            }
        }
    }

    return NULL;
}

/* Dereference a KgpcType pointer-to-record (e.g., class instance) and return
 * the underlying RecordType, or NULL if the type is not a pointer to a record. */
static struct RecordType *codegen_deref_pointer_to_record(KgpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_POINTER)
        return NULL;
    KgpcType *pointee = type->info.points_to;
    if (pointee != NULL && kgpc_type_is_record(pointee))
        return kgpc_type_get_record(pointee);
    return NULL;
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
    /* Unwrap typecasts to find record type (e.g., TextRec(t).Name) */
    if (record_type == NULL && base_expr->type == EXPR_TYPECAST)
    {
        struct Expression *inner = base_expr->expr_data.typecast_data.expr;
        if (inner != NULL)
        {
            if (inner->record_type != NULL)
                record_type = inner->record_type;
            else if (inner->array_element_record_type != NULL)
                record_type = inner->array_element_record_type;
        }
        /* Also check typecast's own resolved_kgpc_type for record info */
        if (record_type == NULL && base_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_record(base_expr->resolved_kgpc_type))
        {
            record_type = kgpc_type_get_record(base_expr->resolved_kgpc_type);
        }
    }
    if (record_type == NULL && record_access_expr->record_type != NULL)
        record_type = record_access_expr->record_type;
    /* Fallback: check resolved_kgpc_type on base expression (e.g., array access
     * yielding a record element in auto-generated typed const initializers) */
    if (record_type == NULL && base_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_record(base_expr->resolved_kgpc_type))
    {
        record_type = kgpc_type_get_record(base_expr->resolved_kgpc_type);
    }
    /* For class instances, the base expression's resolved_kgpc_type is a pointer
     * to the class record.  Dereference the pointer to get the underlying record. */
    if (record_type == NULL)
        record_type = codegen_deref_pointer_to_record(base_expr->resolved_kgpc_type);
    if (record_type == NULL)
        record_type = codegen_deref_pointer_to_record(record_access_expr->resolved_kgpc_type);
    if (record_type == NULL)
    {
        if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL)
        {
            const char *fid = record_access_expr->expr_data.record_access_data.field_id;
            fprintf(stderr, "[codegen] lookup_record_field FAIL: field=%s base_type=%d rec_type=%p\n",
                fid ? fid : "<null>", base_expr->type,
                (void*)record_access_expr->record_type);
        }
        return NULL;
    }

    const char *field_name = record_access_expr->expr_data.record_access_data.field_id;
    if (field_name == NULL)
        return NULL;

    return codegen_lookup_record_field_in_members(record_type->fields, field_name);
}

static int expr_is_static_array_like(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL)
        return 0;

    /* Element access is a scalar l-value/r-value, not an array object. */
    if (expr->type == EXPR_ARRAY_ACCESS)
        return 0;

    if (expr_is_dynamic_array(expr))
        return 0;

    /* ShortStrings should NOT be treated as static arrays for assignment.
     * They have special handling via kgpc_string_to_shortstring. */
    if (expr_get_type_tag(expr) == SHORTSTRING_TYPE)
        return 0;
    if (expr->resolved_kgpc_type != NULL && kgpc_type_is_shortstring(expr->resolved_kgpc_type))
        return 0;
    if (codegen_expr_is_shortstring_value_local(expr))
        return 0;

    /* Pointer variables may point to arrays, but assigning the pointer itself
     * is not a static-array copy. Leave pointer-deref cases to normal array
     * handling lower in the pipeline. */
    if (expr->type != EXPR_POINTER_DEREF)
    {
        KgpcType *resolved_type = expr_get_kgpc_type(expr);
        if (resolved_type != NULL && kgpc_type_is_pointer(resolved_type))
            return 0;
        if (expr_get_type_tag(expr) == POINTER_TYPE)
            return 0;
    }

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
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_dynarray_assign_descriptor");
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
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_dynarray_assign_from_temp");
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

    if (src_expr != NULL && src_expr->type == EXPR_ARRAY_LITERAL &&
        src_expr->expr_data.array_literal_data.element_count == 0)
    {
        StackNode_t *zero_desc = add_l_t_bytes("dynarray_zero", descriptor_size);
        if (zero_desc == NULL)
            return inst_list;

        Register_t *zero_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (zero_reg == NULL)
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for dynamic array zero descriptor.");

        char buffer[128];
        if (codegen_target_is_windows())
        {
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rcx\n", zero_desc->offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\txorq\t%%rdx, %%rdx\n");
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%r8d\n", descriptor_size);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rdi\n", zero_desc->offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\txorq\t%%rsi, %%rsi\n");
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", descriptor_size);
            inst_list = add_inst(inst_list, buffer);
        }
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "memset");
        free_arg_regs();

        Register_t *dest_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (dest_reload == NULL)
        {
            free_reg(get_reg_stack(), zero_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for dynamic array destination.");
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
            dest_temp->offset, dest_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
            zero_desc->offset, zero_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        inst_list = codegen_call_dynarray_copy(inst_list, ctx, dest_reload, zero_reg, descriptor_size);
        free_reg(get_reg_stack(), zero_reg);
        free_reg(get_reg_stack(), dest_reload);
        return inst_list;
    }

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

ListNode_t *codegen_call_with_shadow_space(ListNode_t *inst_list, const char *target)
{
    if (target == NULL)
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

static ListNode_t *codegen_promote_char_reg_to_string(ListNode_t *inst_list, Register_t *value_reg)
{
    if (value_reg == NULL)
        return inst_list;

    const char *arg_reg32 = current_arg_reg32(0);
    if (arg_reg32 == NULL)
        return inst_list;

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", value_reg->bit_32, arg_reg32);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", RETURN_REG_64, value_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    free_arg_regs();
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
        reg = get_reg_with_spill(get_reg_stack(), &inst_list);
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
    {
        if (relop_type != NULL)
            *relop_type = NE;
        return inst_list;
    }

    if (expr->type == EXPR_RELOP)
        return codegen_simple_relop(expr, inst_list, ctx, relop_type);

    Register_t *value_reg = NULL;
    inst_list = codegen_evaluate_expr(expr, inst_list, ctx, &value_reg);
    if (value_reg == NULL)
    {
        if (relop_type != NULL)
            *relop_type = NE;
        return inst_list;
    }

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
        if (codegen_is_current_return_var_id(expr, ctx))
        {
            StackNode_t *return_var = ctx->current_return_slot;
            if (return_var != NULL)
            {
                Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (addr_reg == NULL)
                    addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                if (addr_reg == NULL)
                {
                    inst_list = codegen_fail_register(ctx, inst_list, out_reg,
                        "ERROR: Unable to allocate register for return variable address.");
                    goto cleanup;
                }

                char buffer[96];
                if (return_var->is_static)
                {
                    const char *label = (return_var->static_label != NULL) ?
                        return_var->static_label : return_var->label;
                    snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                        label, addr_reg->bit_64);
                }
                else if (return_var->is_reference)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                        return_var->offset, addr_reg->bit_64);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                        return_var->offset, addr_reg->bit_64);
                }
                inst_list = add_inst(inst_list, buffer);
                *out_reg = addr_reg;
                goto cleanup;
            }
        }

        int scope_depth = 0;
        StackNode_t *var_node = find_label_with_depth(expr->expr_data.id, &scope_depth);

        /* Check if this is a set constant - need to emit rodata even if var_node is found */
        if (ctx->symtab != NULL)
        {
            HashNode_t *const_node = NULL;
            if (FindSymbol(&const_node, ctx->symtab, expr->expr_data.id) != 0 &&
                const_node != NULL && const_node->hash_type == HASHTYPE_CONST &&
                const_node->const_set_value != NULL && const_node->const_set_size > 0)
            {
                Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (addr_reg == NULL)
                {
                    addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                    if (addr_reg == NULL)
                    {
                        inst_list = codegen_fail_register(ctx, inst_list, out_reg,
                            "ERROR: Unable to allocate register for set constant.");
                        goto cleanup;
                    }
                }

                inst_list = codegen_emit_const_set_rodata(const_node, inst_list, ctx);
                if (const_node->const_set_label == NULL)
                {
                    inst_list = codegen_fail_register(ctx, inst_list, out_reg,
                        "ERROR: Failed to materialize set constant.");
                    goto cleanup;
                }

                char buffer[96];
                snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                    const_node->const_set_label, addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                *out_reg = addr_reg;
                goto cleanup;
            }
        }

        /* Fallback: resolve via symbol table mangled_id (e.g. class constants
           registered as "TSingleHelper__Epsilon" but referenced as "Epsilon") */
        if (var_node == NULL && ctx->symtab != NULL)
        {
            HashNode_t *sym_node = NULL;
            if (FindSymbol(&sym_node, ctx->symtab, expr->expr_data.id) != 0 &&
                sym_node != NULL && sym_node->mangled_id != NULL)
            {
                var_node = find_label_with_depth(sym_node->mangled_id, &scope_depth);
            }
        }

        if (var_node == NULL)
        {
            /* Procedures/functions used as values (e.g. @Proc, typed proc constants) */
            if (ctx->symtab != NULL)
            {
                HashNode_t *proc_node = NULL;
                if (FindSymbol(&proc_node, ctx->symtab, expr->expr_data.id) != 0 &&
                    proc_node != NULL &&
                    (proc_node->hash_type == HASHTYPE_PROCEDURE ||
                     proc_node->hash_type == HASHTYPE_FUNCTION) &&
                    proc_node->mangled_id != NULL)
                {
                    Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                    if (addr_reg == NULL)
                        addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                    if (addr_reg == NULL)
                    {
                        inst_list = codegen_fail_register(ctx, inst_list, out_reg,
                            "ERROR: Unable to allocate register for procedure address.");
                        goto cleanup;
                    }

                    char buffer[96];
                    snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                        proc_node->mangled_id, addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    *out_reg = addr_reg;
                    goto cleanup;
                }
            }

            /* Constant set literals (set of char) are materialized in rodata */
            if (ctx->symtab != NULL)
            {
                HashNode_t *const_node = NULL;
                int found = FindSymbol(&const_node, ctx->symtab, expr->expr_data.id);
                if (found != 0 &&
                    const_node != NULL && const_node->hash_type == HASHTYPE_CONST &&
                    const_node->const_set_value != NULL && const_node->const_set_size > 0)
                {
                    Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                    if (addr_reg == NULL)
                    {
                        addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                        if (addr_reg == NULL)
                        {
                            inst_list = codegen_fail_register(ctx, inst_list, out_reg,
                                "ERROR: Unable to allocate register for set constant.");
                            goto cleanup;
                        }
                    }

                    inst_list = codegen_emit_const_set_rodata(const_node, inst_list, ctx);
                    if (const_node->const_set_label == NULL)
                    {
                        inst_list = codegen_fail_register(ctx, inst_list, out_reg,
                            "ERROR: Failed to materialize set constant.");
                        goto cleanup;
                    }

                    char buffer[96];
                    snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                        const_node->const_set_label, addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    *out_reg = addr_reg;
                    goto cleanup;
                }
            }

            /* String constants need to be emitted to rodata so their address can be taken */
            if (ctx->symtab != NULL)
            {
                HashNode_t *str_const_node = NULL;
                int str_found = FindSymbol(&str_const_node, ctx->symtab, expr->expr_data.id);
                if (str_found != 0 && str_const_node != NULL &&
                    str_const_node->hash_type == HASHTYPE_CONST &&
                    str_const_node->const_string_value != NULL &&
                    !(str_const_node->type != NULL && str_const_node->type->kind == TYPE_KIND_PROCEDURE))
                {
                    Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                    if (addr_reg == NULL)
                        addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                    if (addr_reg == NULL)
                    {
                        inst_list = codegen_fail_register(ctx, inst_list, out_reg,
                            "ERROR: Unable to allocate register for string constant address.");
                        goto cleanup;
                    }

                    char label[20];
                    snprintf(label, 20, ".LC%d", ctx->write_label_counter++);
                    char add_rodata[1024];
                    const char *readonly_section = codegen_readonly_section_directive();
                    /* Simple assembly-safe emit — control chars are rare in constants */
                    snprintf(add_rodata, 1024, "%s\n%s:\n\t.string \"%s\"\n%s\n",
                        readonly_section, label, str_const_node->const_string_value, codegen_text_section_resume());
                    inst_list = add_inst(inst_list, add_rodata);

                    char buffer[96];
                    snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                        label, addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    *out_reg = addr_reg;
                    goto cleanup;
                }
            }

            if (expr->expr_data.id != NULL)
            {
                const char *builtin_file_ptr = NULL;
                if (pascal_identifier_equals(expr->expr_data.id, "stdin"))
                    builtin_file_ptr = "stdin_ptr";
                else if (pascal_identifier_equals(expr->expr_data.id, "stdout"))
                    builtin_file_ptr = "stdout_ptr";
                else if (pascal_identifier_equals(expr->expr_data.id, "stderr"))
                    builtin_file_ptr = "stderr_ptr";
                else if (pascal_identifier_equals(expr->expr_data.id, "Input"))
                    builtin_file_ptr = "Input_ptr";
                else if (pascal_identifier_equals(expr->expr_data.id, "Output"))
                    builtin_file_ptr = "Output_ptr";

                if (builtin_file_ptr != NULL)
                {
                    Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                    if (addr_reg == NULL)
                        addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                    if (addr_reg == NULL)
                    {
                        inst_list = codegen_fail_register(ctx, inst_list, out_reg,
                            "ERROR: Unable to allocate register for file variable address.");
                        goto cleanup;
                    }

                    char buffer[96];
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s(%%rip), %s\n",
                        builtin_file_ptr, addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    *out_reg = addr_reg;
                    goto cleanup;
                }
            }

            if (nonlocal_flag() == 1)
            {
                int offset = 0;
                inst_list = codegen_get_nonlocal(inst_list, expr->expr_data.id, &offset, ctx);
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
            if (FindSymbol(&symbol, ctx->symtab, expr->expr_data.id) != 0 && symbol != NULL)
                treat_as_reference = symbol->is_var_parameter;
        }
        if (var_node->is_reference)
            treat_as_reference = 1;
        if (treat_as_reference && symbol != NULL && expr->expr_data.id != NULL &&
            pascal_identifier_equals(expr->expr_data.id, "Self") &&
            symbol->type != NULL)
        {
            struct RecordType *self_record = NULL;
            if (kgpc_type_is_pointer(symbol->type) &&
                symbol->type->info.points_to != NULL &&
                symbol->type->info.points_to->kind == TYPE_KIND_RECORD)
            {
                self_record = symbol->type->info.points_to->info.record_info;
            }
            else if (symbol->type->kind == TYPE_KIND_RECORD)
            {
                self_record = symbol->type->info.record_info;
            }
            if (self_record != NULL && record_type_is_class(self_record))
                treat_as_reference = 0;
        }
        /* ShortStrings may be stored as pointers in a single qword slot.
         * In that case we need to load the pointer value, not take the
         * address of the spill slot itself. */
        if (!treat_as_reference && symbol != NULL && symbol->type != NULL &&
            var_node->size <= CODEGEN_POINTER_SIZE_BYTES)
        {
            struct TypeAlias *alias = kgpc_type_get_type_alias(symbol->type);
            int is_shortstring = kgpc_type_is_shortstring(symbol->type) ||
                (alias != NULL && alias->is_shortstring);
            if (!is_shortstring && alias != NULL)
            {
                if ((alias->alias_name != NULL &&
                     pascal_identifier_equals(alias->alias_name, "ShortString")) ||
                    (alias->target_type_id != NULL &&
                     pascal_identifier_equals(alias->target_type_id, "ShortString")))
                {
                    is_shortstring = 1;
                }
            }
            if (is_shortstring)
                treat_as_reference = 1;
        }

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
            /* Static/global variables live at a fixed address; even when used for
             * var parameters we need the address, not the first qword of storage. */
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                static_label, addr_reg->bit_64);
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
                const char *label = var_node->static_label != NULL ?
                    var_node->static_label : var_node->label;
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s(%%rip), %s\n",
                    label != NULL ? label : "",
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
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                label != NULL ? label : "",
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
    else if (expr->type == EXPR_RECORD_CONSTRUCTOR)
    {
        long long record_size = 0;
        if (codegen_get_record_size(ctx, expr, &record_size) != 0 || record_size <= 0)
        {
            codegen_report_error(ctx,
                "ERROR: Unable to determine size for record constructor.");
            goto cleanup;
        }

        StackNode_t *temp_slot = codegen_alloc_record_ctor_temp(record_size);
        if (temp_slot == NULL)
        {
            codegen_report_error(ctx,
                "ERROR: Unable to allocate temporary storage for record constructor.");
            goto cleanup;
        }

        const char *dest_arg_reg = codegen_target_is_windows() ? "%rcx" : "%rdi";
        const char *val_arg_reg = codegen_target_is_windows() ? "%rdx" : "%rsi";
        const char *size_arg_reg = codegen_target_is_windows() ? "%r8"  : "%rdx";
        char buffer[128];

        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
            temp_slot->offset, dest_arg_reg);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\txorq\t%%rax, %%rax\n");
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", val_arg_reg);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", record_size, size_arg_reg);
        inst_list = add_inst(inst_list, buffer);

        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "memset");
        free_arg_regs();

        struct Expression *temp_var = mk_varid(expr->line_num, strdup(temp_slot->label));
        if (temp_var == NULL)
            goto cleanup;
        temp_var->record_type = expr->record_type;
        if (expr->record_type != NULL)
            temp_var->resolved_kgpc_type = create_record_type(expr->record_type);

        ListNode_t *field_node = expr->expr_data.record_constructor_data.fields;
        while (field_node != NULL)
        {
            struct RecordConstructorField *field =
                (struct RecordConstructorField *)field_node->cur;
            if (field != NULL && field->field_id != NULL && field->value != NULL)
            {
                struct Expression *field_access =
                    mk_recordaccess(expr->line_num, temp_var, strdup(field->field_id));
                if (field_access == NULL)
                    goto cleanup;
                field_access->expr_data.record_access_data.field_offset = field->field_offset;
                if (field->field_type == RECORD_TYPE)
                {
                    field_access->record_type = field->field_record_type;
                    if (field->field_record_type != NULL)
                        field_access->resolved_kgpc_type = create_record_type(field->field_record_type);
                }
                else if (field->field_type == POINTER_TYPE)
                {
                    field_access->resolved_kgpc_type = create_pointer_type(NULL);
                }
                else if (field->field_type != UNKNOWN_TYPE)
                {
                    /* Compute the field's actual storage size from record layout.
                     * create_primitive_type(INT_TYPE) defaults to 4 bytes, but in
                     * FPC mode Integer=SmallInt is only 2 bytes.  Derive the real
                     * size from consecutive field offsets and the record's cached
                     * total size. */
                    long long field_storage = 0;
                    if (expr->record_type != NULL && expr->record_type->has_cached_size)
                    {
                        long long next_off = expr->record_type->cached_size;
                        ListNode_t *scan = expr->expr_data.record_constructor_data.fields;
                        while (scan != NULL)
                        {
                            struct RecordConstructorField *other =
                                (struct RecordConstructorField *)scan->cur;
                            if (other != NULL &&
                                other->field_offset > field->field_offset &&
                                other->field_offset < next_off)
                                next_off = other->field_offset;
                            scan = scan->next;
                        }
                        field_storage = next_off - field->field_offset;
                    }
                    if (field_storage > 0 && field_storage != 4 &&
                        field_storage <= INT_MAX)
                        field_access->resolved_kgpc_type =
                            create_primitive_type_with_size(field->field_type, (int)field_storage);
                    else
                        field_access->resolved_kgpc_type = create_primitive_type(field->field_type);
                }

                if (field->field_is_array)
                {
                    field_access->is_array_expr = 1;
                    field_access->array_lower_bound = field->array_start;
                    field_access->array_upper_bound = field->array_end;
                    field_access->array_is_dynamic = field->array_is_open;
                    field_access->array_element_type = field->array_element_type;
                    if (field->array_element_type_id != NULL)
                        field_access->array_element_type_id = strdup(field->array_element_type_id);
                    field_access->array_element_record_type = field->array_element_record_type;
                }

                struct RecordField *actual_field = codegen_lookup_record_field_expr(field_access, ctx);
                if (actual_field != NULL && actual_field->is_array && !field_access->is_array_expr)
                {
                    field_access->is_array_expr = 1;
                    field_access->array_lower_bound = actual_field->array_start;
                    field_access->array_upper_bound = actual_field->array_end;
                    field_access->array_is_dynamic = actual_field->array_is_open;
                    field_access->array_element_type = actual_field->array_element_type;
                    if (field_access->array_element_type_id == NULL &&
                        actual_field->array_element_type_id != NULL)
                    {
                        field_access->array_element_type_id =
                            strdup(actual_field->array_element_type_id);
                    }
                    field_access->array_element_record_type =
                        actual_field->array_element_record;
                    if (field_access->array_element_size <= 0 &&
                        actual_field->array_element_type != UNKNOWN_TYPE)
                    {
                        int elem_size = codegen_local_type_tag_size(actual_field->array_element_type);
                        if (elem_size > 0)
                            field_access->array_element_size = elem_size;
                    }
                }
                if (!field_access->is_array_expr &&
                    field->value->type == EXPR_ARRAY_LITERAL &&
                    field->field_type != UNKNOWN_TYPE)
                {
                    int elem_size = codegen_local_type_tag_size(field->field_type);
                    if (elem_size > 0)
                    {
                        field_access->is_array_expr = 1;
                        field_access->array_lower_bound = 0;
                        field_access->array_upper_bound =
                            field->value->expr_data.array_literal_data.element_count - 1;
                        field_access->array_is_dynamic = 0;
                        field_access->array_element_type = field->field_type;
                        field_access->array_element_size = elem_size;
                        if (field_access->array_element_type_id == NULL &&
                            field->field_type_id != NULL)
                        {
                            field_access->array_element_type_id = strdup(field->field_type_id);
                        }
                    }
                }

                if (field_access->is_array_expr &&
                    field->value->type == EXPR_ARRAY_LITERAL)
                {
                    ListNode_t *element_node = field->value->expr_data.array_literal_data.elements;
                    int index = field_access->array_lower_bound;
                    while (element_node != NULL && index <= field_access->array_upper_bound)
                    {
                        struct Expression *element_expr = (struct Expression *)element_node->cur;
                        struct Expression *index_expr = mk_inum(expr->line_num, index);
                        struct Expression *array_access = mk_arrayaccess(expr->line_num,
                            field_access, index_expr);
                        if (array_access == NULL)
                            goto cleanup;
                        array_access->is_array_expr = 1;
                        array_access->array_lower_bound = field_access->array_lower_bound;
                        array_access->array_upper_bound = field_access->array_upper_bound;
                        array_access->array_is_dynamic = field_access->array_is_dynamic;
                        array_access->array_element_type = field_access->array_element_type;
                        array_access->array_element_size = field_access->array_element_size;
                        if (field_access->array_element_type_id != NULL)
                            array_access->array_element_type_id = strdup(field_access->array_element_type_id);
                        array_access->array_element_record_type = field_access->array_element_record_type;

                        struct Statement *assign_stmt = mk_varassign(expr->line_num, expr->col_num,
                            array_access, element_expr);
                        if (assign_stmt == NULL)
                            goto cleanup;
                        inst_list = codegen_var_assignment(assign_stmt, inst_list, ctx);
                        free(assign_stmt);
                        element_node = element_node->next;
                        ++index;
                    }
                    field_node = field_node->next;
                    continue;
                }

                struct Statement *assign_stmt = mk_varassign(expr->line_num, expr->col_num,
                    field_access, field->value);
                if (assign_stmt == NULL)
                    goto cleanup;
                inst_list = codegen_var_assignment(assign_stmt, inst_list, ctx);
                free(assign_stmt);
            }
            field_node = field_node->next;
        }

        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
        {
            addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (addr_reg == NULL)
            {
                inst_list = codegen_fail_register(ctx, inst_list, out_reg,
                    "ERROR: Unable to allocate register for record constructor address.");
                goto cleanup;
            }
        }
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
            temp_slot->offset, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        *out_reg = addr_reg;
        goto cleanup;
    }
    else if (expr->type == EXPR_ARRAY_LITERAL)
    {
        /* Array literals are materialized into a temporary stack slot by
         * codegen_expr_with_result, which returns a register pointing to
         * the data.  Use that address directly instead of falling through
         * to codegen_evaluate_expr/build_expr_tree which cannot handle
         * compound expressions. */
        inst_list = codegen_expr_with_result(expr, inst_list, ctx, out_reg);
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
    else if (expr->type == EXPR_ADDR)
    {
        struct Expression *inner = expr->expr_data.addr_data.expr;
        if (inner == NULL)
        {
            codegen_report_error(ctx, "ERROR: Address-of expression missing operand.");
            goto cleanup;
        }

        inst_list = codegen_address_for_expr(inner, inst_list, ctx, out_reg);
        goto cleanup;
    }
    else if (expr->type == EXPR_TYPECAST)
    {
        struct Expression *inner = expr->expr_data.typecast_data.expr;
        /* Typecast on an addressable expression: pass the address of the inner
         * expression. This handles reinterpret casts for var parameters, e.g.
         * InterLockedExchange(longint(TLSInitialized), ...) where the first
         * param is 'var Target: longint' and TLSInitialized is longbool. */
        if (inner != NULL && codegen_expr_is_addressable(inner))
        {
            inst_list = codegen_address_for_expr(inner, inst_list, ctx, out_reg);
            goto cleanup;
        }
    }
    else if (expr->type == EXPR_FUNCTION_CALL &&
        (expr_has_type_tag(expr, RECORD_TYPE) || expr_has_type_tag(expr, SHORTSTRING_TYPE) ||
         (expr->resolved_kgpc_type != NULL &&
          expr->resolved_kgpc_type->kind == TYPE_KIND_ARRAY &&
          !kgpc_type_is_dynamic_array(expr->resolved_kgpc_type))))
    {
        /* Record-valued function calls evaluate to a temporary addressable object
         * (sret/hidden pointer), which callers may pass by reference. */
        inst_list = codegen_evaluate_expr(expr, inst_list, ctx, out_reg);
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

