/*
    SemCheck_Helpers.c - Expression info helper functions

    This file contains helper functions for managing expression metadata:
    - Pointer info management
    - Array info management
    - Record type lookups

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"

/*===========================================================================
 * Pointer Info Helpers
 *===========================================================================*/

void semcheck_clear_pointer_info(struct Expression *expr)
{
    if (expr == NULL)
        return;

    expr->pointer_subtype = UNKNOWN_TYPE;
    if (expr->pointer_subtype_id != NULL)
    {
        free(expr->pointer_subtype_id);
        expr->pointer_subtype_id = NULL;
    }
}

void semcheck_set_pointer_info(struct Expression *expr, int subtype, const char *type_id)
{
    if (expr == NULL)
        return;

    semcheck_clear_pointer_info(expr);
    expr->pointer_subtype = subtype;
    if (type_id != NULL)
    {
        expr->pointer_subtype_id = strdup(type_id);
        if (expr->pointer_subtype_id == NULL)
            fprintf(stderr, "Error: failed to allocate pointer type identifier.\n");
    }
}

/*===========================================================================
 * Array Info Helpers
 *===========================================================================*/

void semcheck_clear_array_info(struct Expression *expr)
{
    if (expr == NULL)
        return;

    expr->is_array_expr = 0;
    expr->array_element_type = UNKNOWN_TYPE;
    if (expr->array_element_type_id != NULL)
    {
        free(expr->array_element_type_id);
        expr->array_element_type_id = NULL;
    }
    expr->array_lower_bound = 0;
    expr->array_upper_bound = -1;
    expr->array_element_size = 0;
    expr->array_is_dynamic = 0;
    expr->array_element_record_type = NULL;
}

void semcheck_set_array_info_from_kgpctype(struct Expression *expr, SymTab_t *symtab,
    KgpcType *array_type, int line_num)
{
    if (expr == NULL || array_type == NULL || array_type->kind != TYPE_KIND_ARRAY)
        return;

    semcheck_clear_array_info(expr);
    expr->is_array_expr = 1;
    expr->array_lower_bound = array_type->info.array_info.start_index;
    expr->array_upper_bound = array_type->info.array_info.end_index;
    expr->array_is_dynamic = kgpc_type_is_dynamic_array(array_type);
    expr->array_element_size = (int)kgpc_type_get_array_element_size(array_type);

    KgpcType *element_type = kgpc_type_get_array_element_type(array_type);
    if (element_type != NULL)
    {
        expr->array_element_type = semcheck_tag_from_kgpc(element_type);
        if (element_type->kind == TYPE_KIND_RECORD)
            expr->array_element_record_type = kgpc_type_get_record(element_type);
        else
            expr->array_element_record_type = NULL;
    }
    else
    {
        expr->array_element_type = UNKNOWN_TYPE;
        expr->array_element_record_type = NULL;
    }

    (void)symtab;
    (void)line_num;
}

/*===========================================================================
 * Record Type Lookup
 *===========================================================================*/

static HashNode_t *semcheck_pick_type_node_with_unit_preference(SymTab_t *symtab,
    const char *type_id, int prefer_unit_defined)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, type_id);
    HashNode_t *best = NULL;
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            if (best == NULL)
            {
                best = node;
            }
            else if (prefer_unit_defined)
            {
                if (!best->defined_in_unit && node->defined_in_unit)
                    best = node;
            }
            else
            {
                if (best->defined_in_unit && !node->defined_in_unit)
                    best = node;
            }
        }
        cur = cur->next;
    }

    if (matches != NULL)
        DestroyList(matches);
    return best;
}

struct RecordType *semcheck_lookup_record_type(SymTab_t *symtab, const char *type_id)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_id);
    if (type_node == NULL)
        return NULL;

    struct RecordType *record = get_record_type_from_node(type_node);
    if (record != NULL)
        return record;

    struct TypeAlias *alias = get_type_alias_from_node(type_node);
    if (alias != NULL && alias->target_type_id != NULL)
    {
        HashNode_t *target_node = semcheck_pick_type_node_with_unit_preference(symtab,
            alias->target_type_id, type_node->defined_in_unit);
        if (target_node == NULL)
            target_node = semcheck_find_preferred_type_node(symtab, alias->target_type_id);
        if (target_node != NULL)
            return get_record_type_from_node(target_node);
    }

    return NULL;
}

/*===========================================================================
 * Array Info from TypeAlias
 *===========================================================================*/

void semcheck_set_array_info_from_alias(struct Expression *expr, SymTab_t *symtab,
    struct TypeAlias *alias, int line_num)
{
    if (expr == NULL)
        return;

    semcheck_clear_array_info(expr);
    if (alias == NULL || !alias->is_array)
        return;

    if (alias->array_dimensions != NULL &&
        alias->array_start == 0 && alias->array_end < alias->array_start)
    {
        ListNode_t *first_dim = alias->array_dimensions;
        if (first_dim != NULL && first_dim->type == LIST_STRING && first_dim->cur != NULL)
        {
            char *dim_str = (char *)first_dim->cur;
            if (strstr(dim_str, "..") == NULL)
            {
                if (pascal_identifier_equals(dim_str, "Boolean"))
                {
                    alias->array_start = 0;
                    alias->array_end = 1;
                    alias->is_open_array = 0;
                }
                else if (symtab != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, dim_str) >= 0 &&
                        type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                    {
                        struct TypeAlias *dim_alias = get_type_alias_from_node(type_node);
                        if (dim_alias != NULL && dim_alias->is_enum &&
                            dim_alias->enum_literals != NULL)
                        {
                            int count = ListLength(dim_alias->enum_literals);
                            if (count > 0)
                            {
                                alias->array_start = 0;
                                alias->array_end = count - 1;
                                alias->is_open_array = 0;
                            }
                        }
                        else if (dim_alias != NULL && dim_alias->is_range &&
                            dim_alias->range_known)
                        {
                            alias->array_start = dim_alias->range_start;
                            alias->array_end = dim_alias->range_end;
                            alias->is_open_array = 0;
                        }
                    }
                }
            }
        }
    }

    expr->is_array_expr = 1;
    expr->array_lower_bound = alias->array_start;
    expr->array_upper_bound = alias->array_end;
    expr->array_is_dynamic = alias->is_open_array;
    expr->array_element_type = alias->array_element_type;
    if (alias->array_element_type_id != NULL)
    {
        expr->array_element_type_id = strdup(alias->array_element_type_id);
        if (expr->array_element_type_id == NULL)
            fprintf(stderr, "Error: failed to allocate array element type identifier.\n");
    }

    if (alias->is_shortstring)
        semcheck_expr_set_resolved_type(expr, SHORTSTRING_TYPE);

    if (expr->array_element_type == UNKNOWN_TYPE && expr->array_element_type_id != NULL)
    {
        int resolved_type = UNKNOWN_TYPE;
        if (resolve_type_identifier(&resolved_type, symtab, expr->array_element_type_id, line_num) == 0)
            expr->array_element_type = resolved_type;
    }

    if (expr->array_element_type == RECORD_TYPE || expr->array_element_type == UNKNOWN_TYPE)
        expr->array_element_record_type = semcheck_lookup_record_type(symtab,
            expr->array_element_type_id);

    long long computed_size = 0;
    int size_status = 1;
    if (expr->array_element_record_type != NULL)
        size_status = sizeof_from_record(symtab, expr->array_element_record_type,
            &computed_size, 0, line_num);
    else if (expr->array_element_type != UNKNOWN_TYPE ||
        expr->array_element_type_id != NULL)
        size_status = sizeof_from_type_ref(symtab, expr->array_element_type,
            expr->array_element_type_id, &computed_size, 0, line_num);

    if (size_status == 0 && computed_size > 0 && computed_size <= INT_MAX)
        expr->array_element_size = (int)computed_size;
}

/*===========================================================================
 * Array Info from HashNode
 *===========================================================================*/

void semcheck_set_array_info_from_hashnode(struct Expression *expr, SymTab_t *symtab,
    HashNode_t *node, int line_num)
{
    if (expr == NULL)
        return;

    semcheck_clear_array_info(expr);
    if (node == NULL || node->type == NULL ||
        !(kgpc_type_is_array(node->type) || kgpc_type_is_array_of_const(node->type)))
        return;

    expr->is_array_expr = 1;

    if (kgpc_type_is_array_of_const(node->type))
    {
        expr->array_lower_bound = 0;
        expr->array_upper_bound = -1;
        expr->array_is_dynamic = 1;
        expr->array_element_type = ARRAY_OF_CONST_TYPE;
        expr->array_element_size = (int)sizeof(kgpc_tvarrec);
        expr->array_element_record_type = semcheck_lookup_record_type(symtab, "TVarRec");
        expr->array_element_type_id = strdup("TVarRec");
        return;
    }

    /* Get array bounds from KgpcType */
    int node_lower_bound, node_upper_bound;
    hashnode_get_array_bounds(node, &node_lower_bound, &node_upper_bound);

    /* Get element size from KgpcType */
    long long node_element_size = hashnode_get_element_size(node);

    /* Check if array is dynamic */
    int node_is_dynamic = hashnode_is_dynamic_array(node);

    expr->array_lower_bound = node_lower_bound;
    expr->array_upper_bound = node_upper_bound;
    expr->array_is_dynamic = node_is_dynamic;
    expr->array_element_size = node_element_size;

    expr->array_element_type = UNKNOWN_TYPE;
    set_type_from_hashtype(&expr->array_element_type, node);
    if (node->type != NULL && kgpc_type_is_array(node->type))
    {
        KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(node->type, symtab);
        if (elem_type != NULL)
        {
            expr->array_element_type = semcheck_tag_from_kgpc(elem_type);
            if (expr->array_element_type == UNKNOWN_TYPE &&
                elem_type->type_alias != NULL &&
                elem_type->type_alias->target_type_id != NULL)
            {
                expr->array_element_type_id = strdup(elem_type->type_alias->target_type_id);
            }
            if (expr->array_element_type_id == NULL &&
                elem_type->kind == TYPE_KIND_POINTER &&
                elem_type->info.points_to != NULL &&
                elem_type->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
                elem_type->info.points_to->info.primitive_type_tag == CHAR_TYPE)
            {
                long long char_size = kgpc_type_sizeof(elem_type->info.points_to);
                if (char_size == 2)
                    expr->array_element_type_id = strdup("PWideChar");
                else
                    expr->array_element_type_id = strdup("PAnsiChar");
            }
            if (elem_type->kind == TYPE_KIND_RECORD)
            {
                expr->array_element_record_type = kgpc_type_get_record(elem_type);
                if (expr->array_element_type_id == NULL &&
                    expr->array_element_record_type != NULL &&
                    expr->array_element_record_type->type_id != NULL)
                {
                    expr->array_element_type_id = strdup(expr->array_element_record_type->type_id);
                }
            }
        }
        else if (node->type->info.array_info.element_type_id != NULL)
        {
            expr->array_element_type_id = strdup(node->type->info.array_info.element_type_id);
        }
    }

    struct TypeAlias *type_alias = get_type_alias_from_node(node);
    if (type_alias != NULL && type_alias->is_array)
    {
        semcheck_set_array_info_from_alias(expr, symtab, type_alias, line_num);

        if (expr->array_element_size <= 0 && node_element_size > 0)
            expr->array_element_size = node_element_size;
        else if (expr->array_element_size <= 0 &&
            type_alias->array_end >= type_alias->array_start)
        {
            long long count = (long long)type_alias->array_end -
                (long long)type_alias->array_start + 1;
            if (count > 0 && type_alias->array_element_type != UNKNOWN_TYPE)
            {
                long long element_size = 0;
                if (sizeof_from_type_ref(symtab, type_alias->array_element_type,
                        type_alias->array_element_type_id, &element_size,
                        0, line_num) == 0)
                {
                    expr->array_element_size = (int)element_size;
                }
            }
        }

        if (!expr->array_is_dynamic && node_is_dynamic)
            expr->array_is_dynamic = 1;

        if (node_lower_bound != 0)
            expr->array_lower_bound = node_lower_bound;
        if (node_upper_bound != 0)
            expr->array_upper_bound = node_upper_bound;
    }
    else if (node->type != NULL && node->type->kind == TYPE_KIND_ARRAY)
    {
        /* For inline array declarations (no TypeAlias), extract info from KgpcType */
        KgpcType *element_type = node->type->info.array_info.element_type;
        if (element_type != NULL)
        {
            if (element_type->kind == TYPE_KIND_RECORD)
            {
                if (element_type->info.record_info != NULL)
                {
                    expr->array_element_record_type = element_type->info.record_info;
                    expr->array_element_type = RECORD_TYPE;
                }
            }
            else if (element_type->kind == TYPE_KIND_PRIMITIVE)
            {
                expr->array_element_type = element_type->info.primitive_type_tag;
            }
            else if (element_type->kind == TYPE_KIND_ARRAY)
            {
                /* Element is itself an array (nested array) - get its type alias if available */
                struct TypeAlias *element_alias = element_type->type_alias;
                if (element_alias != NULL && element_alias->is_array)
                {
                    /* Propagate the element array's information to this expression
                     * so that further indexing works correctly */
                    if (element_alias->array_element_type_id != NULL)
                    {
                        expr->array_element_type_id = strdup(element_alias->array_element_type_id);
                        if (expr->array_element_type_id == NULL)
                            fprintf(stderr, "Error: failed to allocate array element type identifier.\n");
                    }
                    expr->array_element_type = element_alias->array_element_type;
                }
            }
            else if (element_type->kind == TYPE_KIND_POINTER)
            {
                /* Element is a pointer type (e.g., array of PChar) */
                expr->array_element_type = POINTER_TYPE;

                /* Get pointer target type info from the element type */
                KgpcType *points_to = element_type->info.points_to;
                if (points_to != NULL)
                {
                    if (points_to->kind == TYPE_KIND_PRIMITIVE)
                    {
                        /* Store pointer target type for later use in semcheck_arrayaccess */
                        /* This info will be transferred to the array access expression */
                    }
                    else if (points_to->kind == TYPE_KIND_RECORD && points_to->info.record_info != NULL)
                    {
                        expr->array_element_record_type = points_to->info.record_info;
                    }
                }

                /* If the element type has a type_alias with pointer info, use its type_id */
                struct TypeAlias *element_alias = element_type->type_alias;
                if (element_alias != NULL && element_alias->is_pointer && element_alias->pointer_type_id != NULL)
                {
                    expr->array_element_type_id = strdup(element_alias->pointer_type_id);
                    if (expr->array_element_type_id == NULL)
                        fprintf(stderr, "Error: failed to allocate array element type identifier.\n");
                }
            }
        }
    }
    else
    {
        expr->array_element_record_type = get_record_type_from_node(node);
    }
}
