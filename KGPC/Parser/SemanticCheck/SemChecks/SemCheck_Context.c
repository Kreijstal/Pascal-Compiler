/*
    SemCheck_Context.c - With-statement context and type helper management

    This file contains functions for managing:
    - WITH statement context stack (for resolving unqualified field access)
    - Type helper registry (for type helper classes)

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"

static const char *g_semcheck_current_method_owner = NULL;

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
    entry->base_type_id = strdup(record_info->helper_base_type_id);
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
    (void)symtab;
    if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL)
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
            if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL)
            {
                fprintf(stderr,
                    "[KGPC]   helper_entry: base_type_tag=%d base_type_id=%s helper=%s\n",
                    entry->base_type_tag,
                    entry->base_type_id != NULL ? entry->base_type_id : "<null>",
                    entry->helper_record && entry->helper_record->type_id ? entry->helper_record->type_id : "<null>");
            }
            if (type_name != NULL && entry->base_type_id != NULL &&
                pascal_identifier_equals(entry->base_type_id, type_name))
                return entry->helper_record;
            if (base_type_tag != UNKNOWN_TYPE && entry->base_type_tag == base_type_tag)
                return entry->helper_record;
        }
        cur = cur->next;
    }
    return NULL;
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

    if (expr_type == RECORD_TYPE)
        return context_expr->record_type;

    if (expr_type == POINTER_TYPE)
    {
        struct RecordType *record_info = context_expr->record_type;
        if (record_info == NULL && context_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, context_expr->pointer_subtype_id) != -1 &&
                target_node != NULL)
                record_info = get_record_type_from_node(target_node);
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
    }

    return 1;
}
