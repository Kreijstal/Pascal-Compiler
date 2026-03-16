/*
    SemCheck_Context.c - With-statement context and type helper management

    This file contains functions for managing:
    - WITH statement context stack (for resolving unqualified field access)
    - Type helper registry (for type helper classes)

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"

static const char *g_semcheck_current_method_owner = NULL;

static const char *semcheck_helper_lookup_fallback_name(const char *type_name)
{
    if (type_name == NULL)
        return NULL;
    if (pascal_identifier_equals(type_name, "AnsiString") ||
        pascal_identifier_equals(type_name, "OpenString") ||
        pascal_identifier_equals(type_name, "RawByteString") ||
        pascal_identifier_equals(type_name, "ShortString"))
        return "String";
    if (pascal_identifier_equals(type_name, "UnicodeString"))
        return "WideString";
    return NULL;
}

static char *semcheck_normalize_helper_base_type_id(SymTab_t *symtab,
    const char *base_type_id)
{
    if (base_type_id == NULL)
        return NULL;

    HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, base_type_id);
    if (type_node != NULL)
    {
        struct TypeAlias *alias = get_type_alias_from_node(type_node);
        int depth = 0;
        while (alias != NULL && alias->target_type_id != NULL && depth < 8)
        {
            if (pascal_identifier_equals(alias->target_type_id, base_type_id))
                break;
            type_node = semcheck_find_preferred_type_node(symtab, alias->target_type_id);
            if (type_node == NULL)
                return strdup(alias->target_type_id);
            base_type_id = alias->target_type_id;
            alias = get_type_alias_from_node(type_node);
            depth++;
        }
    }

    return strdup(base_type_id);
}

static int semcheck_helper_matches_base(TypeHelperEntry *entry,
    int base_type_tag, const char *type_name)
{
    if (entry == NULL)
        return 0;
    if (type_name != NULL && entry->base_type_id != NULL &&
        pascal_identifier_equals(entry->base_type_id, type_name))
        return 1;
    if (base_type_tag != UNKNOWN_TYPE && entry->base_type_tag == base_type_tag)
        return 1;
    return 0;
}

void semcheck_set_current_method_owner(const char *owner_id)
{
    g_semcheck_current_method_owner = owner_id;
}

const char *semcheck_get_current_method_owner(void)
{
    return g_semcheck_current_method_owner;
}

/* With statement context stack */
WithContextEntry *with_context_stack = NULL;
size_t with_context_count = 0;
size_t with_context_capacity = 0;

/* Type helper registry */
ListNode_t *type_helper_entries = NULL;

/*===========================================================================
 * Type Helper Registry Functions
 *===========================================================================*/

void semcheck_register_type_helper(struct RecordType *record_info, SymTab_t *symtab)
{
    if (record_info == NULL || symtab == NULL)
        return;
    if (!record_info->is_type_helper || record_info->helper_base_type_id == NULL)
        return;

    ListNode_t *cur = type_helper_entries;
    while (cur != NULL)
    {
        TypeHelperEntry *entry = (TypeHelperEntry *)cur->cur;
        if (entry != NULL && entry->helper_record == record_info)
            return;
        cur = cur->next;
    }

    TypeHelperEntry *entry = (TypeHelperEntry *)calloc(1, sizeof(TypeHelperEntry));
    if (entry == NULL)
        return;
    entry->base_type_id = semcheck_normalize_helper_base_type_id(symtab,
        record_info->helper_base_type_id);
    entry->base_type_tag = semcheck_map_builtin_type_name(symtab, record_info->helper_base_type_id);
    entry->helper_record = record_info;
    if (entry->base_type_id == NULL)
    {
        free(entry);
        return;
    }

    ListNode_t *node = CreateListNode(entry, LIST_UNSPECIFIED);
    if (node == NULL)
    {
        free(entry->base_type_id);
        free(entry);
        return;
    }
    node->next = type_helper_entries;
    type_helper_entries = node;
}

struct RecordType *semcheck_lookup_type_helper(SymTab_t *symtab,
    int base_type_tag, const char *type_name)
{
    const char *current_owner = semcheck_get_current_method_owner();
    if (current_owner != NULL && symtab != NULL)
    {
        struct RecordType *owner_record = semcheck_lookup_record_type(symtab, current_owner);
        if (owner_record != NULL && owner_record->is_type_helper &&
            owner_record->helper_base_type_id != NULL)
        {
            int owner_base_tag = semcheck_map_builtin_type_name(symtab,
                owner_record->helper_base_type_id);
            if (type_name != NULL)
            {
                if (pascal_identifier_equals(owner_record->helper_base_type_id, type_name))
                    return owner_record;
            }
            else if (base_type_tag != UNKNOWN_TYPE && owner_base_tag == base_type_tag)
                return owner_record;
        }
    }
    if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL)
    {
        fprintf(stderr,
            "[KGPC] lookup_type_helper: base_type_tag=%d type_name=%s\n",
            base_type_tag,
            type_name != NULL ? type_name : "<null>");
    }
    ListNode_t *cur = type_helper_entries;
    while (cur != NULL)
    {
        TypeHelperEntry *entry = (TypeHelperEntry *)cur->cur;
        if (entry != NULL)
        {
            if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL)
            {
                fprintf(stderr,
                    "[KGPC]   helper_entry: base_type_tag=%d base_type_id=%s helper=%s\n",
                    entry->base_type_tag,
                    entry->base_type_id != NULL ? entry->base_type_id : "<null>",
                    entry->helper_record && entry->helper_record->type_id ? entry->helper_record->type_id : "<null>");
            }
            if (semcheck_helper_matches_base(entry, base_type_tag, type_name))
                return entry->helper_record;
        }
        cur = cur->next;
    }
    if (type_name != NULL)
    {
        const char *fallback_name = semcheck_helper_lookup_fallback_name(type_name);
        if (fallback_name != NULL && !pascal_identifier_equals(fallback_name, type_name))
            return semcheck_lookup_type_helper(symtab, UNKNOWN_TYPE, fallback_name);
    }
    return NULL;
}

struct RecordType *semcheck_lookup_type_helper_for_member(SymTab_t *symtab,
    int base_type_tag, const char *type_name, const char *member_name)
{
    if (member_name == NULL)
        return semcheck_lookup_type_helper(symtab, base_type_tag, type_name);

    struct RecordType *preferred = semcheck_lookup_type_helper(symtab, base_type_tag, type_name);
    if (preferred != NULL)
    {
        if (semcheck_find_class_method(symtab, preferred, member_name, NULL) != NULL ||
            semcheck_find_class_field_including_hidden(symtab, preferred, member_name, NULL) != NULL)
            return preferred;
    }

    for (ListNode_t *cur = type_helper_entries; cur != NULL; cur = cur->next)
    {
        TypeHelperEntry *entry = (TypeHelperEntry *)cur->cur;
        if (!semcheck_helper_matches_base(entry, base_type_tag, type_name))
            continue;
        if (entry->helper_record == NULL)
            continue;
        if (entry->helper_record == preferred)
            continue;
        if (semcheck_find_class_method(symtab, entry->helper_record, member_name, NULL) != NULL ||
            semcheck_find_class_field_including_hidden(symtab, entry->helper_record, member_name, NULL) != NULL)
            return entry->helper_record;
    }

    return preferred;
}

/*===========================================================================
 * With-Statement Context Stack Functions
 *===========================================================================*/

int ensure_with_capacity(void)
{
    if (with_context_count < with_context_capacity)
        return 0;

    size_t new_capacity = with_context_capacity == 0 ? 8 : with_context_capacity * 2;
    WithContextEntry *new_stack = realloc(with_context_stack,
        new_capacity * sizeof(*with_context_stack));
    if (new_stack == NULL)
        return 1;

    with_context_stack = new_stack;
    with_context_capacity = new_capacity;
    return 0;
}

int semcheck_with_push(struct Expression *context_expr, struct RecordType *record_type)
{
    if (context_expr == NULL || record_type == NULL)
        return 1;
    if (ensure_with_capacity() != 0)
    {
        fprintf(stderr,
            "Error: failed to expand WITH context stack for semantic analysis.\n");
        return -1;
    }

    with_context_stack[with_context_count].context_expr = context_expr;
    with_context_stack[with_context_count].record_type = record_type;
    ++with_context_count;
    return 0;
}

void semcheck_with_pop(void)
{
    if (with_context_count > 0)
        --with_context_count;
}

struct RecordType *resolve_record_type_for_with(SymTab_t *symtab,
    struct Expression *context_expr, int expr_type, int line_num)
{
    (void)line_num;
    if (context_expr == NULL)
        return NULL;

    if (context_expr->type == EXPR_FUNCTION_CALL)
    {
        int cast_type = UNKNOWN_TYPE;
        semcheck_try_reinterpret_as_typecast(&cast_type, symtab, context_expr, INT_MAX);
    }

    if (context_expr->type == EXPR_TYPECAST)
    {
        const char *target_id = context_expr->expr_data.typecast_data.target_type_id;
        const TypeRef *target_ref = context_expr->expr_data.typecast_data.target_type_ref;
        if (target_id != NULL || target_ref != NULL)
        {
            HashNode_t *type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                target_ref, target_id);
            if (type_node != NULL)
            {
                struct RecordType *record_info = get_record_type_from_node(type_node);
                if (record_info != NULL)
                    return record_info;
            }
        }
    }

    if (expr_type == RECORD_TYPE)
    {
        if (context_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_record(context_expr->resolved_kgpc_type))
            return kgpc_type_get_record(context_expr->resolved_kgpc_type);
        return NULL;
    }

    if (expr_type == POINTER_TYPE)
    {
        struct RecordType *record_info = NULL;
        if (context_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_pointer(context_expr->resolved_kgpc_type) &&
            context_expr->resolved_kgpc_type->info.points_to != NULL &&
            kgpc_type_is_record(context_expr->resolved_kgpc_type->info.points_to))
        {
            record_info = kgpc_type_get_record(context_expr->resolved_kgpc_type->info.points_to);
        }
        if (record_info == NULL && context_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, context_expr->pointer_subtype_id) != 0 &&
                target_node != NULL)
                record_info = get_record_type_from_node(target_node);
        }
        /* Fallback for function call results: when a method/function returns a
         * class type (pointer to record), the resolved_kgpc_type may be a bare
         * pointer without embedded record info. Look up the return type from the
         * call's resolved function symbol to find the pointed-to record. */
        if (record_info == NULL && context_expr->type == EXPR_FUNCTION_CALL)
        {
            HashNode_t *func_node = context_expr->expr_data.function_call_data.resolved_func;
            if (func_node != NULL && func_node->type != NULL)
            {
                KgpcType *ret_type = kgpc_type_get_return_type(func_node->type);
                if (ret_type != NULL && kgpc_type_is_pointer(ret_type) &&
                    ret_type->info.points_to != NULL &&
                    kgpc_type_is_record(ret_type->info.points_to))
                {
                    record_info = kgpc_type_get_record(ret_type->info.points_to);
                }
                else if (ret_type != NULL && kgpc_type_is_pointer(ret_type))
                {
                    /* pointer without embedded record - try type alias */
                    struct TypeAlias *alias = kgpc_type_get_type_alias(ret_type);
                    const char *alias_name = (alias != NULL) ? alias->alias_name : NULL;
                    if (alias_name == NULL && ret_type->info.points_to != NULL)
                    {
                        alias = kgpc_type_get_type_alias(ret_type->info.points_to);
                        alias_name = (alias != NULL) ? alias->alias_name : NULL;
                    }
                    if (alias_name != NULL)
                    {
                        HashNode_t *alias_node = NULL;
                        if (FindIdent(&alias_node, symtab, alias_name) != 0 &&
                            alias_node != NULL)
                            record_info = get_record_type_from_node(alias_node);
                    }
                }
            }
        }
        return record_info;
    }

    return NULL;
}

struct RecordType *semcheck_with_resolve_record_type(SymTab_t *symtab,
    struct Expression *context_expr, int expr_type, int line_num)
{
    return resolve_record_type_for_with(symtab, context_expr, expr_type, line_num);
}

int semcheck_with_try_resolve(const char *field_id, SymTab_t *symtab,
    struct Expression **out_record_expr, int line_num)
{
    if (field_id == NULL || out_record_expr == NULL)
        return 1;

    for (size_t index = with_context_count; index > 0; --index)
    {
        WithContextEntry *entry = &with_context_stack[index - 1];
        if (entry->record_type == NULL)
            continue;

        struct RecordField *field_desc = NULL;
        long long offset = 0;
        if (resolve_record_field(symtab, entry->record_type, field_id,
                &field_desc, &offset, line_num, 1) == 0 && field_desc != NULL)
        {
            struct Expression *clone = clone_expression(entry->context_expr);
            if (clone == NULL)
                return -1;
            *out_record_expr = clone;
            return 0;
        }

        /* Also check class/record properties (e.g. Items, Count, ...) */
        struct ClassProperty *prop = semcheck_find_class_property(symtab,
            entry->record_type, field_id, NULL);
        if (prop != NULL)
        {
            struct Expression *clone = clone_expression(entry->context_expr);
            if (clone == NULL)
                return -1;
            *out_record_expr = clone;
            return 0;
        }
    }

    return 1;
}

/* Variant of semcheck_with_try_resolve that also checks class methods
 * and record fields with procedural types.
 * Returns: 0 = class method found, 2 = procedural field found,
 *          1 = not found, -1 = error */
int semcheck_with_try_resolve_method(const char *method_id, SymTab_t *symtab,
    struct Expression **out_record_expr, int line_num)
{
    if (method_id == NULL || out_record_expr == NULL)
        return 1;

    for (size_t index = with_context_count; index > 0; --index)
    {
        WithContextEntry *entry = &with_context_stack[index - 1];
        if (entry->record_type == NULL)
            continue;

        /* Check class methods first */
        HashNode_t *method_node = semcheck_find_class_method(symtab,
            entry->record_type, method_id, NULL);
        if (method_node != NULL)
        {
            struct Expression *clone = clone_expression(entry->context_expr);
            if (clone == NULL)
                return -1;
            *out_record_expr = clone;
            return 0;
        }

        /* Also check record fields with procedural type (e.g. CompareFn: TCompareFunc) */
        struct RecordField *field_desc = NULL;
        long long offset = 0;
        int rf_result = resolve_record_field(symtab, entry->record_type, method_id,
                &field_desc, &offset, line_num, 1);
        if (rf_result == 0 && field_desc != NULL)
        {
            /* Check if field has a procedural type, either directly or via type_id lookup */
            int is_proc_field = (field_desc->proc_type != NULL);
            if (!is_proc_field && field_desc->type_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindIdent(&type_node, symtab, field_desc->type_id) != 0 &&
                    type_node != NULL && type_node->type != NULL &&
                    type_node->type->kind == TYPE_KIND_PROCEDURE)
                {
                    is_proc_field = 1;
                }
            }
            if (is_proc_field)
            {
                struct Expression *clone = clone_expression(entry->context_expr);
                if (clone == NULL)
                    return -1;
                *out_record_expr = clone;
                return 2; /* procedural field, not a method */
            }
        }
    }

    return 1;
}
