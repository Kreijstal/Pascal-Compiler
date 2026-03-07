/*
    SemCheck_Expr_Types.c - Type cast and access expression semantic checks

    This file contains semantic checking for:
    - Type casts (typecast expression)
    - IS expression (type checking)
    - AS expression (type conversion)
    - Pointer dereference (^)
    - Record/class field access (record.field)
    - Property access and getter/setter transformation

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"
#include "unit_registry.h"
#include "../../ErrVars.h"

int semcheck_resolve_scoped_enum_literal(SymTab_t *symtab, const char *type_name,
    const char *literal_name, long long *out_value);
int semcheck_resolve_scoped_enum_literal_ref(SymTab_t *symtab, const QualifiedIdent *type_ref,
    const char *literal_name, long long *out_value);

static char *semcheck_join_qualified_prefix(const QualifiedIdent *name)
{
    if (name == NULL || name->segments == NULL || name->count <= 1)
        return NULL;
    size_t total = 0;
    for (int i = 0; i < name->count - 1; ++i)
    {
        if (name->segments[i] != NULL)
            total += strlen(name->segments[i]);
        if (i + 1 < name->count - 1)
            total += 1;
    }
    char *out = (char *)malloc(total + 1);
    if (out == NULL)
        return NULL;
    out[0] = '\0';
    for (int i = 0; i < name->count - 1; ++i)
    {
        if (name->segments[i] != NULL)
            strcat(out, name->segments[i]);
        if (i + 1 < name->count - 1)
            strcat(out, ".");
    }
    return out;
}

static char *build_qualified_identifier_from_expr_local(struct Expression *expr)
{
    if (expr == NULL)
        return NULL;
    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
        return strdup(expr->expr_data.id);
    if (expr->type != EXPR_RECORD_ACCESS)
        return NULL;

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    char *field_id = expr->expr_data.record_access_data.field_id;
    if (record_expr == NULL || field_id == NULL)
        return NULL;

    char *base = build_qualified_identifier_from_expr_local(record_expr);
    if (base == NULL)
        return NULL;
    size_t qualified_len = strlen(base) + 1 + strlen(field_id) + 1;
    char *qualified = (char *)malloc(qualified_len);
    if (qualified != NULL)
        snprintf(qualified, qualified_len, "%s.%s", base, field_id);
    free(base);
    return qualified;
}

static int semcheck_has_value_ident(SymTab_t *symtab, const char *id)
{
    if (symtab == NULL || id == NULL)
        return 0;

    for (ListNode_t *cur_scope = symtab->stack_head; cur_scope != NULL; cur_scope = cur_scope->next)
    {
        HashTable_t *table = (HashTable_t *)cur_scope->cur;
        ListNode_t *matches = FindAllIdentsInTable(table, id);
        if (matches != NULL)
        {
            for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
            {
                HashNode_t *node = (HashNode_t *)cur->cur;
                if (node != NULL &&
                    node->hash_type != HASHTYPE_TYPE &&
                    node->hash_type != HASHTYPE_FUNCTION &&
                    node->hash_type != HASHTYPE_PROCEDURE &&
                    node->hash_type != HASHTYPE_BUILTIN_PROCEDURE)
                {
                    DestroyList(matches);
                    return 1;
                }
            }
            DestroyList(matches);
        }
    }

    ListNode_t *builtin_matches = FindAllIdentsInTable(symtab->builtins, id);
    if (builtin_matches != NULL)
    {
        for (ListNode_t *cur = builtin_matches; cur != NULL; cur = cur->next)
        {
            HashNode_t *node = (HashNode_t *)cur->cur;
            if (node != NULL &&
                node->hash_type != HASHTYPE_TYPE &&
                node->hash_type != HASHTYPE_FUNCTION &&
                node->hash_type != HASHTYPE_PROCEDURE &&
                node->hash_type != HASHTYPE_BUILTIN_PROCEDURE)
            {
                DestroyList(builtin_matches);
                return 1;
            }
        }
        DestroyList(builtin_matches);
    }

    return 0;
}

static HashNode_t *semcheck_find_any_proc_symbol(SymTab_t *symtab, const char *id)
{
    if (symtab == NULL || id == NULL)
        return NULL;

    for (ListNode_t *cur_scope = symtab->stack_head; cur_scope != NULL; cur_scope = cur_scope->next)
    {
        HashTable_t *table = (HashTable_t *)cur_scope->cur;
        ListNode_t *matches = FindAllIdentsInTable(table, id);
        if (matches != NULL)
        {
            for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
            {
                HashNode_t *node = (HashNode_t *)cur->cur;
                if (node != NULL &&
                    (node->hash_type == HASHTYPE_FUNCTION || node->hash_type == HASHTYPE_PROCEDURE))
                {
                    DestroyList(matches);
                    return node;
                }
            }
            DestroyList(matches);
        }
    }

    return NULL;
}

int semcheck_typecast(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;

    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_TYPECAST);

    int error_count = 0;
    int inner_type = UNKNOWN_TYPE;
    KgpcType *inner_kgpc_type = NULL;

    if (expr->expr_data.typecast_data.expr != NULL)
    {
        error_count += semcheck_expr_with_type(&inner_kgpc_type, symtab,
            expr->expr_data.typecast_data.expr, max_scope_lev, NO_MUTATE);
        inner_type = semcheck_tag_from_kgpc(inner_kgpc_type);
    }

    const char *target_id = expr->expr_data.typecast_data.target_type_id;
    if (target_id != NULL && pascal_identifier_equals(target_id, "unaligned"))
    {
        if (expr->expr_data.typecast_data.expr == NULL)
        {
            semcheck_error_with_context("Error on line %d, unaligned requires an argument.\n\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        expr->expr_data.typecast_data.target_type = inner_type;
        if (inner_kgpc_type != NULL)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, inner_kgpc_type);
            *type_return = semcheck_tag_from_kgpc(inner_kgpc_type);
        }
        else
        {
            semcheck_expr_set_resolved_type(expr, inner_type);
            *type_return = inner_type;
        }
        return error_count;
    }

    int target_type = expr->expr_data.typecast_data.target_type;
    const TypeRef *target_ref = expr->expr_data.typecast_data.target_type_ref;
    const char *qualifier = expr->expr_data.typecast_data.type_qualifier;
    const char *target_base_id = type_ref_base_name(target_ref);
    if (target_base_id == NULL)
        target_base_id = expr->expr_data.typecast_data.target_type_id;
    if (qualifier == NULL && target_ref != NULL && target_ref->name != NULL &&
        target_ref->name->count > 1)
    {
        char *prefix = semcheck_join_qualified_prefix(target_ref->name);
        if (prefix != NULL)
        {
            expr->expr_data.typecast_data.type_qualifier = prefix;
            qualifier = prefix;
        }
        if (target_base_id != NULL &&
            expr->expr_data.typecast_data.target_type_id != NULL &&
            !pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id, target_base_id))
        {
            free(expr->expr_data.typecast_data.target_type_id);
            expr->expr_data.typecast_data.target_type_id = strdup(target_base_id);
        }
    }
    int builtin_mapped = semcheck_map_builtin_type_name(symtab, target_base_id);
    int target_is_builtin = (builtin_mapped != UNKNOWN_TYPE);
    if (target_type == UNKNOWN_TYPE && builtin_mapped != UNKNOWN_TYPE)
        target_type = builtin_mapped;

    if (qualifier != NULL && !semcheck_is_unit_name(qualifier))
    {
        HashNode_t *qual_node = NULL;
        if (FindIdent(&qual_node, symtab, qualifier) >= 0 &&
            qual_node != NULL && qual_node->hash_type != HASHTYPE_TYPE)
        {
            int call_result = semcheck_reinterpret_typecast_as_call(type_return, symtab,
                expr, max_scope_lev);
            if (call_result == 0 || expr->type != EXPR_TYPECAST)
                return call_result;
        }
    }

    /* Resolve the target type unless we already mapped a builtin */
    if (target_type == UNKNOWN_TYPE || !target_is_builtin)
    {
        HashNode_t *type_node = semcheck_find_preferred_type_node_with_ref(symtab,
            target_ref, target_base_id);
        if (type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
        {
            int call_result = semcheck_reinterpret_typecast_as_call(type_return, symtab,
                expr, max_scope_lev);
            if (call_result == 0)
                return 0;
        }
        error_count += resolve_type_identifier_ref(&target_type, symtab,
            target_id, target_ref, expr->line_num);
    }

    HashNode_t *array_target_node = NULL;
    int target_is_array = 0;
    if (expr->expr_data.typecast_data.target_type_id != NULL)
    {
        array_target_node = semcheck_find_preferred_type_node_with_ref(symtab,
            target_ref, target_id);
        if (array_target_node != NULL && array_target_node->type != NULL &&
            array_target_node->type->kind == TYPE_KIND_ARRAY)
        {
            target_is_array = 1;
        }
    }

    if (target_type == UNKNOWN_TYPE &&
        expr->expr_data.typecast_data.target_type_id == NULL &&
        expr->expr_data.typecast_data.target_type_ref == NULL)
    {
        semcheck_error_with_context("Error on line %d, typecast requires a target type.\n\n",
            expr->line_num);
        ++error_count;
    }

    *type_return = target_type;
    semcheck_expr_set_resolved_type(expr, target_type);

    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    if (target_type == PROCEDURE)
    {
        HashNode_t *target_node = NULL;
        if (expr->expr_data.typecast_data.target_type_id != NULL)
        {
            target_node = semcheck_find_type_node_with_kgpc_type_ref(
                symtab,
                expr->expr_data.typecast_data.target_type_ref,
                expr->expr_data.typecast_data.target_type_id);
            if (target_node == NULL &&
                FindIdent(&target_node, symtab, expr->expr_data.typecast_data.target_type_id) >= 0)
            {
                /* target_node assigned by FindIdent when present */
            }
        }

        if (target_node != NULL && target_node->type != NULL &&
            target_node->type->kind == TYPE_KIND_PROCEDURE)
        {
            kgpc_type_retain(target_node->type);
            expr->resolved_kgpc_type = target_node->type;
        }
    }
    if (target_type == POINTER_TYPE)
    {
        /* Resolve full pointer type info so deref preserves record/element types */
        KgpcType *resolved_ptr = NULL;
        HashNode_t *target_node = NULL;
        struct TypeAlias *alias = NULL;
        if (expr->expr_data.typecast_data.target_type_id != NULL ||
            expr->expr_data.typecast_data.target_type_ref != NULL)
        {
            target_node = semcheck_find_preferred_type_node_with_ref(symtab,
                expr->expr_data.typecast_data.target_type_ref,
                expr->expr_data.typecast_data.target_type_id);
            if (target_node == NULL &&
                expr->expr_data.typecast_data.target_type_id != NULL &&
                FindIdent(&target_node, symtab, expr->expr_data.typecast_data.target_type_id) >= 0)
            {
                /* target_node assigned by FindIdent when present */
            }
            if (target_node == NULL && expr->expr_data.typecast_data.target_type_id != NULL)
            {
                const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
                const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
                if (owner_full == NULL)
                    owner_full = semcheck_get_current_method_owner();
                target_node = semcheck_find_type_node_in_owner_chain(symtab,
                    expr->expr_data.typecast_data.target_type_id, owner_full, owner_outer);
            }
            if (target_node != NULL && target_node->type != NULL)
            {
                resolved_ptr = target_node->type;
                kgpc_type_retain(resolved_ptr);

                if (resolved_ptr->kind == TYPE_KIND_POINTER &&
                    resolved_ptr->info.points_to != NULL)
                {
                    KgpcType *points_to = resolved_ptr->info.points_to;
                    if (points_to->kind == TYPE_KIND_RECORD && points_to->info.record_info != NULL)
                    {
                        semcheck_set_pointer_info(expr, RECORD_TYPE, points_to->info.record_info->type_id);
                    }
                    else if (points_to->kind == TYPE_KIND_PRIMITIVE)
                    {
                        int subtype = kgpc_type_get_primitive_tag(points_to);
                        semcheck_set_pointer_info(expr, subtype, NULL);
                    }
                    else if (points_to->kind == TYPE_KIND_ARRAY)
                    {
                        /* Pointer to array: propagate the array type id so that
                         * dereference can set is_array_expr on the result.
                         * Look up the pointer alias to get the pointee type name. */
                        const char *arr_subtype_id = NULL;
                        struct TypeAlias *ptr_alias = get_type_alias_from_node(target_node);
                        if (ptr_alias != NULL && ptr_alias->pointer_type_id != NULL)
                            arr_subtype_id = ptr_alias->pointer_type_id;
                        semcheck_set_pointer_info(expr, UNKNOWN_TYPE, arr_subtype_id);
                    }
                }
            }
            if (target_node != NULL)
                alias = get_type_alias_from_node(target_node);
            if (alias != NULL && alias->is_pointer)
            {
                KgpcType *alias_type = create_kgpc_type_from_type_alias(alias, symtab, 0);
                if (alias_type != NULL)
                {
                    if (alias->kgpc_type == alias_type)
                        kgpc_type_retain(alias_type);
                    if (resolved_ptr != NULL)
                        destroy_kgpc_type(resolved_ptr);
                    resolved_ptr = alias_type;

                    if (resolved_ptr->kind == TYPE_KIND_POINTER &&
                        resolved_ptr->info.points_to != NULL)
                    {
                        KgpcType *points_to = resolved_ptr->info.points_to;
                        if (points_to->kind == TYPE_KIND_RECORD &&
                            points_to->info.record_info != NULL)
                        {
                            semcheck_set_pointer_info(expr, RECORD_TYPE,
                                points_to->info.record_info->type_id);
                        }
                        else if (points_to->kind == TYPE_KIND_PRIMITIVE)
                        {
                            int subtype = kgpc_type_get_primitive_tag(points_to);
                            semcheck_set_pointer_info(expr, subtype, NULL);
                        }
                        else if (points_to->kind == TYPE_KIND_ARRAY)
                        {
                            const char *arr_subtype_id = NULL;
                            if (alias != NULL && alias->pointer_type_id != NULL)
                                arr_subtype_id = alias->pointer_type_id;
                            semcheck_set_pointer_info(expr, UNKNOWN_TYPE, arr_subtype_id);
                        }
                    }
                }
            }
        }

        if (resolved_ptr == NULL)
        {
            int inferred_subtype = UNKNOWN_TYPE;

            if (inner_kgpc_type != NULL)
            {
                if (kgpc_type_is_string(inner_kgpc_type) ||
                    kgpc_type_is_shortstring(inner_kgpc_type) ||
                    kgpc_type_is_char(inner_kgpc_type))
                {
                    inferred_subtype = CHAR_TYPE;
                }
                else if (kgpc_type_is_array(inner_kgpc_type))
                {
                    KgpcType *elem = inner_kgpc_type->info.array_info.element_type;
                    if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE &&
                        elem->info.primitive_type_tag == CHAR_TYPE)
                    {
                        inferred_subtype = CHAR_TYPE;
                    }
                }
            }

            if (inferred_subtype != UNKNOWN_TYPE)
            {
                KgpcType *points_to = create_primitive_type(inferred_subtype);
                if (points_to != NULL)
                {
                    resolved_ptr = create_pointer_type(points_to);
                    semcheck_set_pointer_info(expr, inferred_subtype, NULL);
                }
                else
                {
                    resolved_ptr = create_pointer_type(NULL);
                    semcheck_set_pointer_info(expr, UNKNOWN_TYPE,
                        expr->expr_data.typecast_data.target_type_id);
                }
            }
            else
            {
                resolved_ptr = create_pointer_type(NULL);
                semcheck_set_pointer_info(expr, UNKNOWN_TYPE,
                    expr->expr_data.typecast_data.target_type_id);
            }
        }
        else if (resolved_ptr->kind == TYPE_KIND_PRIMITIVE &&
            resolved_ptr->info.primitive_type_tag == POINTER_TYPE)
        {
            if (expr->pointer_subtype == UNKNOWN_TYPE && inner_kgpc_type != NULL)
            {
                if (kgpc_type_is_string(inner_kgpc_type) ||
                    kgpc_type_is_shortstring(inner_kgpc_type) ||
                    kgpc_type_is_char(inner_kgpc_type))
                {
                    semcheck_set_pointer_info(expr, CHAR_TYPE, NULL);
                }
                else if (kgpc_type_is_array(inner_kgpc_type))
                {
                    KgpcType *elem = inner_kgpc_type->info.array_info.element_type;
                    if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE &&
                        elem->info.primitive_type_tag == CHAR_TYPE)
                    {
                        semcheck_set_pointer_info(expr, CHAR_TYPE, NULL);
                    }
                }
            }
        }

        if (resolved_ptr != NULL && resolved_ptr->kind == TYPE_KIND_POINTER &&
            resolved_ptr->info.points_to == NULL)
        {
            const char *subtype_id = NULL;
            const TypeRef *subtype_ref = NULL;
            if (alias != NULL)
            {
                if (alias->pointer_type_id != NULL)
                    subtype_id = alias->pointer_type_id;
                else if (alias->target_type_id != NULL)
                    subtype_id = alias->target_type_id;
                if (alias->pointer_type_ref != NULL)
                    subtype_ref = alias->pointer_type_ref;
                else if (alias->target_type_ref != NULL)
                    subtype_ref = alias->target_type_ref;
            }
            if (subtype_id == NULL)
                subtype_id = expr->pointer_subtype_id;
            if (subtype_ref == NULL)
                subtype_ref = expr->pointer_subtype_ref;

            HashNode_t *sub_node = semcheck_find_preferred_type_node_with_ref(symtab,
                subtype_ref, subtype_id);
            if (sub_node == NULL && subtype_id != NULL)
            {
                const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
                const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
                if (owner_full == NULL)
                    owner_full = semcheck_get_current_method_owner();
                sub_node = semcheck_find_type_node_in_owner_chain(symtab, subtype_id, owner_full, owner_outer);
            }

            KgpcType *points_to = NULL;
            int points_to_owned = 0;
            if (sub_node != NULL)
            {
                struct RecordType *record_info = get_record_type_from_node(sub_node);
                if (sub_node->type != NULL)
                {
                    if (sub_node->type->kind == TYPE_KIND_PRIMITIVE &&
                        sub_node->type->info.primitive_type_tag == RECORD_TYPE &&
                        record_info != NULL)
                    {
                        points_to = create_record_type(record_info);
                        points_to_owned = 1;
                        semcheck_set_pointer_info(expr, RECORD_TYPE, record_info->type_id);
                    }
                    else
                    {
                        kgpc_type_retain(sub_node->type);
                        points_to = sub_node->type;
                    }
                }
                else if (record_info != NULL)
                {
                    points_to = create_record_type(record_info);
                    points_to_owned = 1;
                    semcheck_set_pointer_info(expr, RECORD_TYPE, record_info->type_id);
                }
            }
            if (points_to == NULL && subtype_id != NULL)
            {
                struct RecordType *record_info = semcheck_lookup_record_type(symtab, subtype_id);
                if (record_info != NULL)
                {
                    points_to = create_record_type(record_info);
                    points_to_owned = 1;
                    semcheck_set_pointer_info(expr, RECORD_TYPE, record_info->type_id);
                }
            }

            if (points_to != NULL)
            {
                KgpcType *new_ptr = create_pointer_type(points_to);
                if (new_ptr != NULL)
                {
                    if (resolved_ptr != NULL)
                        destroy_kgpc_type(resolved_ptr);
                    resolved_ptr = new_ptr;
                    points_to = NULL;
                }
                if (points_to_owned && points_to != NULL)
                    destroy_kgpc_type(points_to);
            }
        }

        if (alias != NULL && alias->pointer_type_id != NULL)
        {
            if (expr->pointer_subtype_id == NULL ||
                (expr->expr_data.typecast_data.target_type_id != NULL &&
                 pascal_identifier_equals(expr->pointer_subtype_id,
                     expr->expr_data.typecast_data.target_type_id)))
            {
                int subtype_tag = expr->pointer_subtype;
                semcheck_set_pointer_info(expr, subtype_tag, alias->pointer_type_id);
            }
        }

        expr->resolved_kgpc_type = resolved_ptr;
    }
    else if (target_type == RECORD_TYPE)
    {
        HashNode_t *target_node = NULL;
        struct RecordType *record_info = NULL;
        if (expr->expr_data.typecast_data.target_type_id != NULL ||
            expr->expr_data.typecast_data.target_type_ref != NULL)
        {
            target_node = semcheck_find_preferred_type_node_with_ref(
                symtab,
                expr->expr_data.typecast_data.target_type_ref,
                expr->expr_data.typecast_data.target_type_id);
            if (target_node == NULL &&
                expr->expr_data.typecast_data.target_type_id != NULL &&
                FindIdent(&target_node, symtab, expr->expr_data.typecast_data.target_type_id) >= 0)
            {
                /* target_node assigned by FindIdent when present */
            }
            if (target_node == NULL && expr->expr_data.typecast_data.target_type_id != NULL)
            {
                const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
                const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
                if (owner_full == NULL)
                    owner_full = semcheck_get_current_method_owner();
                target_node = semcheck_find_type_node_in_owner_chain(symtab,
                    expr->expr_data.typecast_data.target_type_id, owner_full, owner_outer);
            }
            if (target_node != NULL)
            {
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                {
                    fprintf(stderr, "[SemCheck] typecast record target=%s node=%p kgpc_kind=%d\n",
                        expr->expr_data.typecast_data.target_type_id,
                        (void *)target_node,
                        target_node->type != NULL ? target_node->type->kind : -1);
                }
                record_info = get_record_type_from_node(target_node);
                if (record_info == NULL && target_node->type != NULL &&
                    kgpc_type_is_record(target_node->type))
                {
                    record_info = kgpc_type_get_record(target_node->type);
                }
            }
            if (record_info == NULL)
                record_info = semcheck_lookup_record_type(symtab,
                    expr->expr_data.typecast_data.target_type_id);
        }

        if (record_info != NULL)
        {
            KgpcType *target_type = NULL;
            if (target_node != NULL && target_node->type != NULL &&
                kgpc_type_is_record(target_node->type) &&
                kgpc_type_get_record(target_node->type) != NULL)
            {
                target_type = target_node->type;
            }
            if (target_type != NULL)
            {
                kgpc_type_retain(target_type);
                expr->resolved_kgpc_type = target_type;
            }
            else
            {
                expr->resolved_kgpc_type = create_record_type(record_info);
            }
        }
        else if (target_node != NULL && target_node->type != NULL)
        {
            kgpc_type_retain(target_node->type);
            expr->resolved_kgpc_type = target_node->type;
        }
        else
        {
            expr->resolved_kgpc_type = create_primitive_type(RECORD_TYPE);
        }
    }

    if (expr->resolved_kgpc_type == NULL)
    {
        HashNode_t *target_node = semcheck_find_preferred_type_node_with_ref(symtab,
            expr->expr_data.typecast_data.target_type_ref,
            expr->expr_data.typecast_data.target_type_id);
        if (target_node != NULL && target_node->type != NULL)
        {
            kgpc_type_retain(target_node->type);
            expr->resolved_kgpc_type = target_node->type;
        }
        else if (target_type != UNKNOWN_TYPE)
        {
            expr->resolved_kgpc_type = create_primitive_type(target_type);
        }
    }
    else if (target_type != UNKNOWN_TYPE &&
        expr->resolved_kgpc_type != NULL &&
        expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE)
    {
        int prim_tag = expr->resolved_kgpc_type->info.primitive_type_tag;
        if (prim_tag != target_type)
        {
            destroy_kgpc_type(expr->resolved_kgpc_type);
            expr->resolved_kgpc_type = create_primitive_type(target_type);
        }
        semcheck_clear_array_info(expr);
    }

    if (target_is_array && array_target_node != NULL && array_target_node->type != NULL)
    {
        if (expr->resolved_kgpc_type != NULL)
        {
            destroy_kgpc_type(expr->resolved_kgpc_type);
            expr->resolved_kgpc_type = NULL;
        }
        kgpc_type_retain(array_target_node->type);
        expr->resolved_kgpc_type = array_target_node->type;
        semcheck_set_array_info_from_kgpctype(expr, symtab, array_target_node->type, expr->line_num);
        *type_return = UNKNOWN_TYPE;
    }

    (void)inner_type;
    return error_count;
}

int semcheck_is_expr(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_IS);

    int error_count = 0;
    struct Expression *value_expr = expr->expr_data.is_data.expr;
    if (value_expr == NULL)
    {
        semcheck_error_with_context("Error on line %d, \"is\" operator requires a value expression.\n\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int value_type = UNKNOWN_TYPE;
    KgpcType *value_kgpc_type_is = NULL;
    error_count += semcheck_expr_with_type(&value_kgpc_type_is, symtab, value_expr, max_scope_lev, NO_MUTATE);
    value_type = semcheck_tag_from_kgpc(value_kgpc_type_is);

    struct RecordType *value_record = NULL;
    if (value_kgpc_type_is != NULL)
    {
        KgpcType *inner_is = value_kgpc_type_is;
        if (inner_is->kind == TYPE_KIND_POINTER && inner_is->info.points_to != NULL)
            inner_is = inner_is->info.points_to;
        if (inner_is->kind == TYPE_KIND_RECORD)
            value_record = inner_is->info.record_info;
    }
    
    /* Classes are pointers to records, so we need to handle POINTER_TYPE */
    int is_valid_class = 0;
    if (value_type == RECORD_TYPE && value_record != NULL && record_type_is_class(value_record))
    {
        is_valid_class = 1;
    }
    else if (value_type == POINTER_TYPE && value_record != NULL && record_type_is_class(value_record))
    {
        is_valid_class = 1;
    }
    /* Also check via KgpcType for cases where record_type isn't set on the expression */
    if (!is_valid_class && value_kgpc_type_is != NULL)
    {
        KgpcType *inner_is = value_kgpc_type_is;
        if (inner_is->kind == TYPE_KIND_POINTER && inner_is->info.points_to != NULL)
            inner_is = inner_is->info.points_to;
        if (inner_is->kind == TYPE_KIND_RECORD && inner_is->info.record_info != NULL &&
            record_type_is_class(inner_is->info.record_info))
        {
            is_valid_class = 1;
            if (value_record == NULL)
                value_record = inner_is->info.record_info;
        }
    }
    if (!is_valid_class && value_kgpc_type_is != NULL &&
        value_kgpc_type_is->kind == TYPE_KIND_POINTER)
    {
        KgpcType *points_to = value_kgpc_type_is->info.points_to;
        if (points_to == NULL)
        {
            /* Legacy class-instance flows can lose pointee metadata and degrade
             * to generic pointer. Keep "is" usable in RTL exception paths. */
            is_valid_class = 1;
        }
        else if (points_to->kind == TYPE_KIND_PRIMITIVE &&
                 points_to->info.primitive_type_tag == RECORD_TYPE)
        {
            is_valid_class = 1;
        }
        else if (points_to->kind == TYPE_KIND_POINTER &&
                 points_to->info.points_to != NULL &&
                 points_to->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
                 points_to->info.points_to->info.primitive_type_tag == RECORD_TYPE)
        {
            is_valid_class = 1;
        }
    }

    if (!is_valid_class)
    {
        semcheck_error_with_context("Error on line %d, \"is\" operator requires a class instance on the left-hand side.\n\n",
            expr->line_num);
        ++error_count;
    }

    int target_type = expr->expr_data.is_data.target_type;
    struct RecordType *target_record = NULL;
    if (expr->expr_data.is_data.target_type_id != NULL)
    {
        target_record = semcheck_lookup_record_type(symtab,
            expr->expr_data.is_data.target_type_id);
            
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] is_expr: lookup '%s' -> %p\n", 
                expr->expr_data.is_data.target_type_id, target_record);
            if (target_record) {
                fprintf(stderr, "[SemCheck]   is_class=%d\n", target_record->is_class);
            }
        }
    }
    
    /* Check if target is a class (could be RECORD_TYPE or POINTER_TYPE to record) */
    int is_valid_target = 0;
    if (target_record != NULL && record_type_is_class(target_record))
    {
        is_valid_target = 1;
    }
    /* Also check via symbol table KgpcType when record type lookup fails */
    if (!is_valid_target && expr->expr_data.is_data.target_type_id != NULL)
    {
        HashNode_t *is_target_node = NULL;
        if (FindIdent(&is_target_node, symtab, expr->expr_data.is_data.target_type_id) >= 0 &&
            is_target_node != NULL && is_target_node->type != NULL)
        {
            KgpcType *is_tgt = is_target_node->type;
            if (is_tgt->kind == TYPE_KIND_POINTER && is_tgt->info.points_to != NULL)
                is_tgt = is_tgt->info.points_to;
            if (is_tgt->kind == TYPE_KIND_RECORD && is_tgt->info.record_info != NULL &&
                record_type_is_class(is_tgt->info.record_info))
            {
                is_valid_target = 1;
                target_record = is_tgt->info.record_info;
            }

            /* Accept class-reference variables on RHS (e.g. Obj is ObjType,
             * where ObjType: TClass). These are runtime class refs and do
             * not necessarily carry a concrete RecordType at semcheck time. */
            if (!is_valid_target &&
                (is_target_node->hash_type == HASHTYPE_VAR ||
                 is_target_node->hash_type == HASHTYPE_ARRAY))
            {
                KgpcType *target_var_type = is_target_node->type;
                if (target_var_type != NULL)
                {
                    int target_is_pointer =
                        (target_var_type->kind == TYPE_KIND_POINTER) ||
                        (target_var_type->kind == TYPE_KIND_PRIMITIVE &&
                         target_var_type->info.primitive_type_tag == POINTER_TYPE);
                    if (target_is_pointer)
                        is_valid_target = 1;
                }
            }
        }
    }

    /* When the RHS identifier is a class field like FItemClass: TClass,
     * FindIdent won't find it directly - try resolving via implicit Self. */
    if (!is_valid_target && expr->expr_data.is_data.target_type_id != NULL)
    {
        const char *rhs_id = expr->expr_data.is_data.target_type_id;
        const char *owner_id = semcheck_get_current_method_owner();
        if (owner_id != NULL)
        {
            HashNode_t *owner_node = NULL;
            if (FindIdent(&owner_node, symtab, owner_id) >= 0 &&
                owner_node != NULL && owner_node->type != NULL)
            {
                struct RecordType *owner_rec = NULL;
                if (kgpc_type_is_record(owner_node->type))
                    owner_rec = kgpc_type_get_record(owner_node->type);
                else if (kgpc_type_is_pointer(owner_node->type) &&
                         owner_node->type->info.points_to != NULL &&
                         kgpc_type_is_record(owner_node->type->info.points_to))
                    owner_rec = kgpc_type_get_record(owner_node->type->info.points_to);
                if (owner_rec != NULL)
                {
                    struct RecordField *field = semcheck_find_class_field_including_hidden(
                        symtab, owner_rec, rhs_id, NULL);
                    if (field != NULL)
                    {
                        /* Field found - check if its type is a class reference (pointer) */
                        int is_ptr = field->is_pointer ||
                            field->type == POINTER_TYPE;
                        /* Also check via the field's type_id lookup for class-of types */
                        if (!is_ptr && field->type_id != NULL)
                        {
                            HashNode_t *ft_node = NULL;
                            if (FindIdent(&ft_node, symtab, field->type_id) >= 0 &&
                                ft_node != NULL && ft_node->type != NULL)
                            {
                                KgpcType *ft = ft_node->type;
                                is_ptr = (ft->kind == TYPE_KIND_POINTER) ||
                                    (ft->kind == TYPE_KIND_PRIMITIVE &&
                                     ft->info.primitive_type_tag == POINTER_TYPE);
                            }
                        }
                        if (is_ptr)
                            is_valid_target = 1;
                    }
                }
            }
        }
    }

    if (!is_valid_target)
    {
        semcheck_error_with_context("Error on line %d, \"is\" operator requires a class type on the right-hand side.\n\n",
            expr->line_num);
        ++error_count;
    }
    target_type = RECORD_TYPE;

    expr->expr_data.is_data.target_type = target_type;
    expr->expr_data.is_data.target_record_type = target_record;
    semcheck_expr_set_resolved_type(expr, BOOL);
    *type_return = BOOL;
    return error_count;
}

int semcheck_as_expr(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_AS);

    int error_count = 0;
    struct Expression *value_expr = expr->expr_data.as_data.expr;
    if (value_expr == NULL)
    {
        semcheck_error_with_context("Error on line %d, \"as\" operator requires a value expression.\n\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int value_type = UNKNOWN_TYPE;
    KgpcType *value_kgpc_type_as = NULL;
    error_count += semcheck_expr_with_type(&value_kgpc_type_as, symtab, value_expr, max_scope_lev, NO_MUTATE);
    value_type = semcheck_tag_from_kgpc(value_kgpc_type_as);

    struct RecordType *value_record = NULL;
    if (value_kgpc_type_as != NULL)
    {
        KgpcType *inner_as = value_kgpc_type_as;
        if (inner_as->kind == TYPE_KIND_POINTER && inner_as->info.points_to != NULL)
            inner_as = inner_as->info.points_to;
        if (inner_as->kind == TYPE_KIND_RECORD)
            value_record = inner_as->info.record_info;
    }
    
    /* Classes are pointers to records, so we need to handle POINTER_TYPE */
    int is_valid_class = 0;
    if (value_type == RECORD_TYPE && value_record != NULL && record_type_is_class(value_record))
    {
        is_valid_class = 1;
    }
    else if (value_type == POINTER_TYPE && value_record != NULL && record_type_is_class(value_record))
    {
        is_valid_class = 1;
    }

    if (!is_valid_class)
    {
        semcheck_error_with_context("Error on line %d, \"as\" operator requires a class instance on the left-hand side.\n\n",
            expr->line_num);
        ++error_count;
    }

    int target_type = expr->expr_data.as_data.target_type;
    struct RecordType *target_record = NULL;
    if (expr->expr_data.as_data.target_type_id != NULL)
    {
        target_record = semcheck_lookup_record_type(symtab,
            expr->expr_data.as_data.target_type_id);
    }
    
    /* Check if target is a class (could be RECORD_TYPE or POINTER_TYPE to record) */
    int is_valid_target = 0;
    if (target_record != NULL && record_type_is_class(target_record))
    {
        is_valid_target = 1;
    }
    
    if (!is_valid_target)
    {
        semcheck_error_with_context("Error on line %d, \"as\" operator requires a class type on the right-hand side.\n\n",
            expr->line_num);
        ++error_count;
    }
    /* Determine correct target type */
    target_type = RECORD_TYPE;
    KgpcType *result_kgpc_type = NULL;

    if (target_record != NULL && record_type_is_class(target_record))
    {
        target_type = POINTER_TYPE;
        KgpcType *record_kgpc = create_record_type(target_record);
        if (record_kgpc != NULL)
        {
            result_kgpc_type = create_pointer_type(record_kgpc);
        }
    }
    else
    {
        result_kgpc_type = create_record_type(target_record);
    }

    expr->expr_data.as_data.target_type = target_type;
    expr->expr_data.as_data.target_record_type = target_record;
    semcheck_expr_set_resolved_type(expr, target_type);
    
    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    expr->resolved_kgpc_type = result_kgpc_type;
    
    *type_return = target_type;
    return error_count;
}

int semcheck_pointer_deref(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;

    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_POINTER_DEREF);

    semcheck_clear_pointer_info(expr);
    semcheck_clear_array_info(expr);

    struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr == NULL)
    {
        semcheck_error_with_context("Error on line %d, dereference operator requires an operand.\\n\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int error_count = 0;
    int pointer_type = UNKNOWN_TYPE;
    KgpcType *pointer_kgpc_type = NULL;
    error_count += semcheck_expr_with_type(&pointer_kgpc_type, symtab, pointer_expr,
        max_scope_lev, NO_MUTATE);
    pointer_type = semcheck_tag_from_kgpc(pointer_kgpc_type);

    /* Some pointer aliases lose their tag mapping while still carrying pointer
     * metadata. Recover pointer-ness from explicit pointer metadata so chained
     * dereferences (e.g. ^^) keep working. */
    if (pointer_type != POINTER_TYPE)
    {
        if ((pointer_expr->resolved_kgpc_type != NULL &&
             pointer_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER) ||
            pointer_expr->pointer_subtype != UNKNOWN_TYPE ||
            pointer_expr->pointer_subtype_id != NULL)
        {
            pointer_type = POINTER_TYPE;
        }
    }

    if (pointer_type != POINTER_TYPE)
    {
        semcheck_error_with_context("Error on line %d, dereference operator requires a pointer expression.\\n\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return ++error_count;
    }

    int target_type = pointer_expr->pointer_subtype;
    if (target_type == UNKNOWN_TYPE && pointer_expr->pointer_subtype_id != NULL)
    {
        HashNode_t *target_node = NULL;
        if (FindIdent(&target_node, symtab, pointer_expr->pointer_subtype_id) != -1 &&
            target_node != NULL)
        {
            set_type_from_hashtype(&target_type, target_node);
            struct TypeAlias *alias = get_type_alias_from_node(target_node);
            if (alias != NULL)
            {
                if (alias->base_type != UNKNOWN_TYPE)
                    target_type = alias->base_type;
                else if (alias->is_pointer)
                    target_type = POINTER_TYPE;
                else if (alias->is_set)
                    target_type = SET_TYPE;
                else if (alias->is_enum)
                    target_type = ENUM_TYPE;
                else if (alias->is_file)
                    target_type = FILE_TYPE;
            }
        }

        /* If still unknown, try to infer size from the subtype id.
         * This helps pointer types like PAnsiChar inherit a 1-byte element size
         * instead of defaulting to LONGINT (4 bytes). */
        if (target_type == UNKNOWN_TYPE)
        {
            long long inferred_size = 0;
            if (sizeof_from_type_ref(symtab, UNKNOWN_TYPE, pointer_expr->pointer_subtype_id,
                    &inferred_size, 0, expr->line_num) == 0 && inferred_size > 0)
            {
                if (inferred_size == 1)
                    target_type = CHAR_TYPE;
                else if (inferred_size == 2)
                    target_type = INT_TYPE; /* 16-bit */
                else if (inferred_size <= 4)
                    target_type = INT_TYPE;
                else
                    target_type = LONGINT_TYPE;
            }
        }
    }

    /* If we still don't know the subtype, inspect the pointer symbol's alias info */
    if (target_type == UNKNOWN_TYPE && pointer_expr->type == EXPR_VAR_ID)
    {
        HashNode_t *ptr_node = NULL;
        if (FindIdent(&ptr_node, symtab, pointer_expr->expr_data.id) != -1 && ptr_node != NULL)
        {
            struct TypeAlias *alias = get_type_alias_from_node(ptr_node);
            if (alias != NULL && alias->is_pointer)
            {
                target_type = alias->pointer_type;
                if (target_type == UNKNOWN_TYPE && alias->pointer_type_id != NULL)
                {
                    long long inferred_size = 0;
                    if (sizeof_from_type_ref(symtab, UNKNOWN_TYPE, alias->pointer_type_id,
                            &inferred_size, 0, expr->line_num) == 0 && inferred_size > 0)
                    {
                        if (inferred_size == 1)
                            target_type = CHAR_TYPE;
                        else if (inferred_size <= 4)
                            target_type = INT_TYPE;
                        else
                            target_type = LONGINT_TYPE;
                    }
                }

                if (alias->pointer_type_id != NULL && expr->pointer_subtype_id == NULL)
                    semcheck_set_pointer_info(expr, target_type, alias->pointer_type_id);
            }
        }
    }

    /* If subtype id was absent, try to infer from the resolved KgpcType pointer info */
    if (target_type == UNKNOWN_TYPE && pointer_expr->resolved_kgpc_type != NULL &&
        pointer_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER)
    {
        KgpcType *points_to = pointer_expr->resolved_kgpc_type->info.points_to;
        if (points_to != NULL)
        {
            /* Check the actual kind of the pointed-to type first */
            if (kgpc_type_is_record(points_to))
            {
                target_type = RECORD_TYPE;
            }
            else if (points_to->kind == TYPE_KIND_POINTER)
            {
                target_type = POINTER_TYPE;
            }
            else if (points_to->kind == TYPE_KIND_ARRAY)
            {
                /* Pointer to array: set up array info on the deref result
                 * so it can be indexed with [i] */
                target_type = INT_TYPE; /* placeholder; actual element type set below */
                expr->is_array_expr = 1;
                expr->array_element_type = UNKNOWN_TYPE;
                expr->array_element_type_id = NULL;

                KgpcType *elem_type = points_to->info.array_info.element_type;
                if (elem_type != NULL)
                {
                    target_type = semcheck_tag_from_kgpc(elem_type);
                    if (target_type == UNKNOWN_TYPE)
                        target_type = INT_TYPE;
                    expr->array_element_type = semcheck_tag_from_kgpc(elem_type);
                    if (elem_type->kind == TYPE_KIND_RECORD)
                        expr->array_element_record_type = kgpc_type_get_record(elem_type);
                }
                if (points_to->info.array_info.element_type_id != NULL)
                    expr->array_element_type_id = strdup(points_to->info.array_info.element_type_id);

                /* Set the resolved KgpcType to the array type */
                if (expr->resolved_kgpc_type != NULL)
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                kgpc_type_retain(points_to);
                expr->resolved_kgpc_type = points_to;
            }
            else
            {
                /* Fall back to tag-based then size-based inference for primitive types */
                target_type = semcheck_tag_from_kgpc(points_to);
                if (target_type == UNKNOWN_TYPE)
                {
                    long long inferred_size = kgpc_type_sizeof(points_to);
                    if (inferred_size == 1)
                        target_type = CHAR_TYPE;
                    else if (inferred_size == 2)
                        target_type = INT_TYPE;
                    else if (inferred_size > 0 && inferred_size <= 4)
                        target_type = INT_TYPE;
                    else if (inferred_size > 0)
                        target_type = LONGINT_TYPE;
                }
            }
        }
    }

    if (target_type == UNKNOWN_TYPE)
        target_type = LONGINT_TYPE;

    if (target_type == POINTER_TYPE)
    {
        /* For double-pointer dereference (e.g. PPAnsiChar^), resolve what the
         * resulting pointer (PAnsiChar) points to, so that indexing with [i]
         * can determine the correct element type (Char). */
        int resolved_subtype = UNKNOWN_TYPE;
        const char *resolved_subtype_id = pointer_expr->pointer_subtype_id;

        /* Try to resolve from KgpcType chain first */
        if (pointer_expr->resolved_kgpc_type != NULL &&
            pointer_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER &&
            pointer_expr->resolved_kgpc_type->info.points_to != NULL)
        {
            KgpcType *deref_type = pointer_expr->resolved_kgpc_type->info.points_to;
            if (deref_type->kind == TYPE_KIND_POINTER && deref_type->info.points_to != NULL)
            {
                KgpcType *sub_points_to = deref_type->info.points_to;
                resolved_subtype = semcheck_tag_from_kgpc(sub_points_to);
                if (resolved_subtype == UNKNOWN_TYPE)
                {
                    long long sz = kgpc_type_sizeof(sub_points_to);
                    if (sz == 1) resolved_subtype = CHAR_TYPE;
                    else if (sz <= 4) resolved_subtype = INT_TYPE;
                    else resolved_subtype = LONGINT_TYPE;
                }
            }
        }

        /* If KgpcType didn't help, try resolving from subtype_id */
        if (resolved_subtype == UNKNOWN_TYPE && resolved_subtype_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindIdent(&type_node, symtab, resolved_subtype_id) != -1 && type_node != NULL)
            {
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->is_pointer)
                {
                    if (alias->pointer_type != UNKNOWN_TYPE)
                        resolved_subtype = alias->pointer_type;
                    if (alias->pointer_type_id != NULL)
                    {
                        resolved_subtype_id = alias->pointer_type_id;
                        if (resolved_subtype == UNKNOWN_TYPE)
                        {
                            struct RecordType *sub_record =
                                semcheck_lookup_record_type(symtab, alias->pointer_type_id);
                            if (sub_record != NULL)
                                resolved_subtype = RECORD_TYPE;
                        }
                    }
                }
            }
        }

        semcheck_set_pointer_info(expr, resolved_subtype != UNKNOWN_TYPE ? resolved_subtype : POINTER_TYPE,
            resolved_subtype_id);
    }
    if (pointer_expr->pointer_subtype_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, pointer_expr->pointer_subtype_id) != -1 && type_node != NULL)
        {
            struct TypeAlias *alias = get_type_alias_from_node(type_node);
            if (alias != NULL && alias->is_array)
            {
                semcheck_set_array_info_from_alias(expr, symtab, alias, expr->line_num);
            }
        }
    }

    /* Set the expression's resolved type to the pointed-to type.
     * This is critical for code generation to emit correct-sized loads. */
    semcheck_expr_set_resolved_type(expr, target_type);

    /* Propagate KgpcType from the pointer's target for downstream resolution.
     * This is required for chained dereferences and method dispatch on alias pointers. */
    if (pointer_expr->resolved_kgpc_type != NULL &&
        pointer_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER)
    {
        KgpcType *points_to = pointer_expr->resolved_kgpc_type->info.points_to;
        if (points_to != NULL)
        {
            kgpc_type_retain(points_to);
            semcheck_expr_set_resolved_kgpc_type_shared(expr, points_to);
        }
    }
    if (target_type == RECORD_TYPE &&
        (expr->resolved_kgpc_type == NULL || !kgpc_type_is_record(expr->resolved_kgpc_type)) &&
        pointer_expr->pointer_subtype_id != NULL)
    {
        struct RecordType *record_info = semcheck_lookup_record_type(symtab,
            pointer_expr->pointer_subtype_id);
        if (record_info != NULL)
        {
            KgpcType *record_kgpc = create_record_type(record_info);
            if (record_kgpc != NULL)
            {
                semcheck_expr_set_resolved_kgpc_type_shared(expr, record_kgpc);
                destroy_kgpc_type(record_kgpc);
            }
        }
    }

    *type_return = target_type;
    return error_count;
}

int semcheck_property_type_info(SymTab_t *symtab, struct ClassProperty *property,
    int line_num, int *type_out, struct RecordType **record_out)
{
    if (type_out == NULL || property == NULL)
        return 1;

    int resolved_type = property->type;
    if (property->type_id != NULL || property->type_ref != NULL)
    {
        if (resolve_type_identifier_ref(&resolved_type, symtab, property->type_id,
                property->type_ref, line_num) != 0)
        {
            semcheck_error_with_context("Error on line %d, unable to resolve type for property %s.\n\n",
                line_num, property->name != NULL ? property->name : "<unnamed>");
            return 1;
        }
    }

    if (resolved_type == UNKNOWN_TYPE && property->type_id == NULL && property->type_ref == NULL)
    {
        semcheck_error_with_context("Error on line %d, property %s must specify a type.\n\n",
            line_num, property->name != NULL ? property->name : "<unnamed>");
        return 1;
    }

    *type_out = resolved_type;
    if (record_out != NULL)
    {
        if (resolved_type == RECORD_TYPE)
        {
            HashNode_t *type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                property->type_ref, property->type_id);
            if (type_node == NULL)
                type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                    property->type_ref, property->type_id);
            if (type_node != NULL)
                *record_out = get_record_type_from_node(type_node);
            else if (property->type_id != NULL)
                *record_out = semcheck_lookup_record_type(symtab, property->type_id);
            else
                *record_out = NULL;
        }
        else
            *record_out = NULL;
    }

    return 0;
}

int semcheck_transform_property_getter_call(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating,
    HashNode_t *method_node, struct RecordType *owner_record)
{
    if (expr == NULL || expr->type != EXPR_RECORD_ACCESS || method_node == NULL)
    {
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *object_expr = expr->expr_data.record_access_data.record_expr;
    if (object_expr == NULL)
    {
        semcheck_error_with_context("Error on line %d, property getter requires an object instance.\n\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    /* Check if the getter is a static function (takes no Self parameter).
     * Static getters don't need the object instance as an argument. */
    int is_static_getter = 0;
    if (owner_record != NULL && owner_record->type_id != NULL &&
        method_node->id != NULL)
    {
        is_static_getter = from_cparser_is_method_static(owner_record->type_id,
            method_node->id);
    }
    if (!is_static_getter && method_node->type != NULL &&
        method_node->type->kind == TYPE_KIND_PROCEDURE)
    {
        ListNode_t *params = kgpc_type_get_procedure_params(method_node->type);
        if (params == NULL)
        {
            /* No parameters - this is a static getter */
            is_static_getter = 1;
        }
    }

    expr->expr_data.record_access_data.record_expr = NULL;
    if (expr->expr_data.record_access_data.field_id != NULL)
    {
        free(expr->expr_data.record_access_data.field_id);
        expr->expr_data.record_access_data.field_id = NULL;
    }

    ListNode_t *arg_node = NULL;
    if (!is_static_getter)
    {
        /* Non-static getter - pass object as first argument */
        arg_node = CreateListNode(object_expr, LIST_EXPR);
        if (arg_node == NULL)
        {
            semcheck_error_with_context("Error on line %d, unable to allocate getter argument list.\n\n",
                expr->line_num);
            expr->expr_data.record_access_data.record_expr = object_expr;
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
    }
    else
    {
        /* Static getter - no object argument needed, destroy the object expression */
        destroy_expr(object_expr);
    }

    char *id_copy = method_node->id != NULL ? strdup(method_node->id) : NULL;
    char *mangled_copy = NULL;
    if (method_node->mangled_id != NULL)
        mangled_copy = strdup(method_node->mangled_id);

    if ((method_node->id != NULL && id_copy == NULL) ||
        (method_node->mangled_id != NULL && mangled_copy == NULL))
    {
        semcheck_error_with_context("Error on line %d, unable to prepare property getter call.\n\n",
            expr->line_num);
        free(id_copy);
        free(mangled_copy);
        if (arg_node != NULL)
        {
            /* Restore object_expr ownership before freeing arg_node */
            object_expr = (struct Expression *)arg_node->cur;
            arg_node->cur = NULL;
            free(arg_node);
            expr->expr_data.record_access_data.record_expr = object_expr;
        }
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    expr->type = EXPR_FUNCTION_CALL;
    memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
    expr->expr_data.function_call_data.id = id_copy;
    expr->expr_data.function_call_data.mangled_id = mangled_copy;
    expr->expr_data.function_call_data.args_expr = arg_node;
    /* Set resolved_func so semcheck_funccall goes via method_call_resolved (which runs
     * the VMT dispatch check at lines 5241-5283) rather than the funccall_cleanup
     * fast-path (which skips VMT dispatch, causing direct calls to abstract methods). */
    expr->expr_data.function_call_data.resolved_func = method_node;
    expr->expr_data.function_call_data.call_hash_type = method_node->hash_type;
    semcheck_expr_set_call_kgpc_type(expr, method_node->type, 0);
    expr->expr_data.function_call_data.is_call_info_valid = 1;
    semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
    expr->is_array_expr = 0;
    expr->array_element_type = UNKNOWN_TYPE;
    expr->array_element_type_id = NULL;
    expr->array_element_record_type = NULL;
    expr->array_element_size = 0;

    /* Keep legacy_tag here - expression is rewritten and needs re-checking with int type_return */
    return semcheck_expr_legacy_tag(type_return, symtab, expr, max_scope_lev, mutating);
}

int semcheck_recordaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_RECORD_ACCESS);

    semcheck_clear_array_info(expr);

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    const char *field_id = expr->expr_data.record_access_data.field_id;
    if (record_expr == NULL || field_id == NULL)
    {
        semcheck_error_with_context("Error on line %d, malformed record field access.\n\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }
    if (record_expr->type == EXPR_VAR_ID)
        assert(record_expr->expr_data.id != NULL);

    if (record_expr->type == EXPR_FUNCTION_CALL)
    {
        int rec_cast_type = UNKNOWN_TYPE;
        int cast_result = semcheck_try_reinterpret_as_typecast(&rec_cast_type, symtab,
            record_expr, max_scope_lev);
        if (cast_result != 0)
        {
            *type_return = UNKNOWN_TYPE;
            return cast_result;
        }
    }

    if (record_expr->type == EXPR_VAR_ID || record_expr->type == EXPR_RECORD_ACCESS)
    {
        char *qualified_id = NULL;
        if (record_expr->type == EXPR_VAR_ID)
        {
            size_t qualified_len = strlen(record_expr->expr_data.id) + 1 + strlen(field_id) + 1;
            qualified_id = (char *)malloc(qualified_len);
            if (qualified_id != NULL)
                snprintf(qualified_id, qualified_len, "%s.%s", record_expr->expr_data.id, field_id);
        }
        else
        {
            qualified_id = build_qualified_identifier_from_expr_local(record_expr);
            if (qualified_id != NULL)
            {
                size_t qualified_len = strlen(qualified_id) + 1 + strlen(field_id) + 1;
                char *combined = (char *)malloc(qualified_len);
                if (combined != NULL)
                {
                    snprintf(combined, qualified_len, "%s.%s", qualified_id, field_id);
                    free(qualified_id);
                    qualified_id = combined;
                }
                else
                {
                    free(qualified_id);
                    qualified_id = NULL;
                }
            }
        }

        if (qualified_id != NULL)
        {
            HashNode_t *type_node = NULL;
            const char *resolved_id = qualified_id;
            if (FindIdent(&type_node, symtab, qualified_id) < 0 ||
                type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
            {
                const char *base = semcheck_base_type_name(qualified_id);
                if (base != NULL && base != qualified_id &&
                    FindIdent(&type_node, symtab, base) >= 0 &&
                    type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                {
                    resolved_id = base;
                }
            }

            if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
            {
                assert(resolved_id != NULL);
                assert(expr->expr_data.record_access_data.field_id != NULL);
                /* Determine the resolved type tag for the qualified type.
                 * Only set kgpc_type/tag for enum types to enable
                 * TClass.TEnum.Value resolution.  Other type references
                 * should stay UNKNOWN_TYPE to avoid codegen side-effects. */
                int resolved_tag = UNKNOWN_TYPE;
                struct TypeAlias *ta = hashnode_get_type_alias(type_node);
                if (ta != NULL && ta->is_enum)
                    resolved_tag = ENUM_TYPE;

                destroy_expr(record_expr);
                free(expr->expr_data.record_access_data.field_id);
                expr->expr_data.record_access_data.record_expr = NULL;
                expr->expr_data.record_access_data.field_id = NULL;
                expr->type = EXPR_VAR_ID;
                expr->expr_data.id = strdup(resolved_id);
                if (resolved_tag == ENUM_TYPE && type_node->type != NULL)
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, type_node->type);
                if (resolved_tag != UNKNOWN_TYPE)
                    semcheck_expr_set_resolved_type(expr, resolved_tag);
                free(qualified_id);
                *type_return = resolved_tag;
                return 0;
            }
            free(qualified_id);
        }
    }

    /* Scoped enum support for identifiers that resolve as types (e.g., TEndian.Little).
     * If record_expr is a type name, resolve field_id as enum literal.
     */
    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, record_expr->expr_data.id) >= 0 && type_node != NULL &&
            type_node->hash_type == HASHTYPE_TYPE)
        {
            struct TypeAlias *type_alias = hashnode_get_type_alias(type_node);
            if (type_alias != NULL)
            {
                long long enum_value = 0;
                if ((type_alias->is_enum && type_alias->enum_literals != NULL) ||
                    (type_alias->target_type_id != NULL) ||
                    (type_alias->target_type_ref != NULL && type_alias->target_type_ref->name != NULL))
                {
                    int resolved = 0;
                    if (type_alias->is_enum)
                    {
                        resolved = semcheck_resolve_scoped_enum_literal(symtab,
                            record_expr->expr_data.id, field_id, &enum_value);
                    }
                    else if (type_alias->target_type_ref != NULL &&
                             type_alias->target_type_ref->name != NULL)
                    {
                        resolved = semcheck_resolve_scoped_enum_literal_ref(symtab,
                            type_alias->target_type_ref->name, field_id, &enum_value);
                    }
                    else if (type_alias->target_type_id != NULL)
                    {
                        resolved = semcheck_resolve_scoped_enum_literal(symtab,
                            type_alias->target_type_id, field_id, &enum_value);
                    }

                    if (resolved)
                    {
                        expr->type = EXPR_INUM;
                        expr->expr_data.i_num = enum_value;
                        semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                        if (type_alias->kgpc_type != NULL)
                            semcheck_expr_set_resolved_kgpc_type_shared(expr, type_alias->kgpc_type);
                        *type_return = ENUM_TYPE;
                        return 0;
                    }
                }
            }
            /* Check for nested type access: TMyClass.TMyEnum where TMyEnum is a
             * nested type registered as "TMyClass.TMyEnum" in the symbol table. */
            {
                size_t owner_len = strlen(record_expr->expr_data.id);
                size_t field_len = strlen(field_id);
                char *qualified = (char *)malloc(owner_len + 1 + field_len + 1);
                if (qualified != NULL)
                {
                    snprintf(qualified, owner_len + 1 + field_len + 1, "%s.%s",
                        record_expr->expr_data.id, field_id);
                    HashNode_t *nested_node = NULL;
                    if (FindIdent(&nested_node, symtab, qualified) >= 0 &&
                        nested_node != NULL && nested_node->hash_type == HASHTYPE_TYPE)
                    {
                        struct TypeAlias *nta = hashnode_get_type_alias(nested_node);
                        int nested_tag = UNKNOWN_TYPE;
                        if (nta != NULL && nta->is_enum)
                            nested_tag = ENUM_TYPE;
                        else if (nested_node->type != NULL)
                            nested_tag = semcheck_tag_from_kgpc(nested_node->type);

                        /* Replace record_access with EXPR_VAR_ID for the qualified type */
                        if (record_expr != NULL)
                        {
                            destroy_expr(record_expr);
                            expr->expr_data.record_access_data.record_expr = NULL;
                        }
                        if (expr->expr_data.record_access_data.field_id != NULL)
                        {
                            free(expr->expr_data.record_access_data.field_id);
                            expr->expr_data.record_access_data.field_id = NULL;
                        }
                        expr->type = EXPR_VAR_ID;
                        expr->expr_data.id = qualified;
                        semcheck_expr_set_resolved_type(expr, nested_tag);
                        if (nested_node->type != NULL)
                            semcheck_expr_set_resolved_kgpc_type_shared(expr, nested_node->type);
                        *type_return = nested_tag;
                        return 0;
                    }
                    free(qualified);
                }
            }
        }
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] enum const expr: T=%s found=%d type_node=%p\n",
                record_expr->expr_data.id, type_node != NULL, (void *)type_node);
        }
    }

    /* AST TRANSFORMATION FIX: Parser incorrectly parses `-r.x` as `(-r).x` instead of `-(r.x)`.
     * When we detect this pattern (record access on a sign term), we restructure the AST
     * to have the correct operator precedence: the sign term should wrap the record access. 
     * 
     * We only handle EXPR_SIGN_TERM (unary operators) as binary operators (ADDOP, MULOP)
     * would require more complex transformation logic. */
    if (record_expr->type == EXPR_SIGN_TERM)
        {
            /* Current structure: RECORD_ACCESS(SIGN_TERM(inner_expr), field)
             * Desired structure: SIGN_TERM(RECORD_ACCESS(inner_expr, field)) */
            struct Expression *inner_expr = record_expr->expr_data.sign_term;
            if (inner_expr != NULL)
            {
                /* Create a new RECORD_ACCESS for inner_expr.field */
                struct Expression *new_record_access = (struct Expression *)calloc(1, sizeof(struct Expression));
                if (new_record_access == NULL)
                {
                    semcheck_error_with_context("Error on line %d: failed to allocate expression for AST transformation in semcheck_recordaccess.\n",
                        expr->line_num);
                    *type_return = UNKNOWN_TYPE;
                    return 1;
                }
                
                new_record_access->line_num = expr->line_num;
                new_record_access->type = EXPR_RECORD_ACCESS;
                new_record_access->expr_data.record_access_data.record_expr = inner_expr;
                new_record_access->expr_data.record_access_data.field_id = strdup(field_id);
                if (new_record_access->expr_data.record_access_data.field_id == NULL)
                {
                    semcheck_error_with_context("Error on line %d: failed to duplicate field name in AST transformation.\n",
                        expr->line_num);
                    free(new_record_access);
                    *type_return = UNKNOWN_TYPE;
                    return 1;
                }
                new_record_access->expr_data.record_access_data.field_offset = 0;
                semcheck_expr_set_resolved_type(new_record_access, UNKNOWN_TYPE);
                new_record_access->is_array_expr = 0;
                new_record_access->array_element_type = UNKNOWN_TYPE;
                new_record_access->array_element_type_id = NULL;
                new_record_access->array_element_record_type = NULL;
                new_record_access->array_element_size = 0;
                new_record_access->array_lower_bound = 0;
                new_record_access->array_upper_bound = -1;
                new_record_access->array_is_dynamic = 0;
                new_record_access->pointer_subtype = UNKNOWN_TYPE;
                new_record_access->pointer_subtype_id = NULL;
                
                /* Restructure: make the current expr be the SIGN_TERM wrapping the new RECORD_ACCESS */
                record_expr->expr_data.sign_term = new_record_access;
                
                /* Now swap the types so expr becomes SIGN_TERM and the original SIGN_TERM content is preserved */
                enum ExprType temp_type = expr->type;
                expr->type = record_expr->type;
                record_expr->type = temp_type;
                
                /* Swap the data unions */
                union expr_data temp_data = expr->expr_data;
                expr->expr_data = record_expr->expr_data;
                record_expr->expr_data = temp_data;
                
                /* Now expr is SIGN_TERM wrapping new_record_access, and we can process it as a sign term */
                /* Redirect to semcheck_signterm for the transformed expression */
                return semcheck_signterm(type_return, symtab, expr, max_scope_lev, mutating);
            }
        }

    /* Similar AST transformation for NOT operator: parser produces (NOT record).field
     * instead of NOT (record.field). Detect EXPR_RELOP with NOT and restructure so the
     * NOT wraps the record access rather than the record expression itself. */
    if (record_expr->type == EXPR_RELOP &&
        record_expr->expr_data.relop_data.type == NOT &&
        record_expr->expr_data.relop_data.left != NULL &&
        record_expr->expr_data.relop_data.right == NULL)
    {
        struct Expression *inner_expr = record_expr->expr_data.relop_data.left;

        /* Create new record access for inner_expr.field */
        struct Expression *new_record_access = (struct Expression *)calloc(1, sizeof(struct Expression));
        if (new_record_access == NULL)
        {
            semcheck_error_with_context("Error on line %d: failed to allocate expression for AST transformation in semcheck_recordaccess.\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

        new_record_access->line_num = expr->line_num;
        new_record_access->type = EXPR_RECORD_ACCESS;
        new_record_access->expr_data.record_access_data.record_expr = inner_expr;
        new_record_access->expr_data.record_access_data.field_id = strdup(field_id);
        if (new_record_access->expr_data.record_access_data.field_id == NULL)
        {
            semcheck_error_with_context("Error on line %d: failed to duplicate field name in AST transformation.\n",
                expr->line_num);
            free(new_record_access);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        new_record_access->expr_data.record_access_data.field_offset = 0;

        /* Insert new record access as operand of NOT */
        record_expr->expr_data.relop_data.left = new_record_access;

        /* Swap expression types/data so current node becomes the NOT expression */
        enum ExprType temp_type = expr->type;
        expr->type = record_expr->type;
        record_expr->type = temp_type;

        union expr_data temp_data = expr->expr_data;
        expr->expr_data = record_expr->expr_data;
        record_expr->expr_data = temp_data;

        /* Now expr is the NOT expression wrapping record access; re-run semantic check as relop */
        return semcheck_relop(type_return, symtab, expr, max_scope_lev, mutating);
    }

    /* FPC Bootstrap Feature: Handle unit-qualified identifiers in runtime expressions.
     * When we see UnitName.ConstName and UnitName is an unresolvable identifier,
     * check if ConstName is a known constant/var in the current scope (since unit
     * exports are merged). If so, transform the expression to just the identifier. */
    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
    {
        char *unit_id = record_expr->expr_data.id;
        HashNode_t *unit_check = NULL;
        int unit_is_qualifier = semcheck_is_unit_name(unit_id);
        
        /* Check if the "unit name" identifier exists in symbol table */
        int find_result = FindIdent(&unit_check, symtab, unit_id);
        if (!unit_is_qualifier && find_result == -1 && unit_registry_contains(unit_id))
        {
            unit_is_qualifier = 1;
            if (getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_RECORD_ACCESS] unit-qualifier registry fallback: unit=%s field=%s\n",
                    unit_id != NULL ? unit_id : "(null)",
                    field_id != NULL ? field_id : "(null)");
            }
            if (getenv("KGPC_ASSERT_UNIT_QUALIFIER") != NULL)
                assert(find_result == -1 && "unit-qualifier registry fallback requires unresolved name");
        }
        if (unit_is_qualifier)
        {
            if (getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_RECORD_ACCESS] unit-qualifier resolve: unit=%s field=%s\n",
                    unit_id != NULL ? unit_id : "(null)",
                    field_id != NULL ? field_id : "(null)");
            }
            /* Identifier not found - might be a unit qualifier.
             * Try to look up the field_id directly as it may be an exported constant/var.
             * When qualifier is "System", prefer the builtin (non-unit-imported) entry
             * since later unit imports (e.g., objpas MaxInt) may shadow system builtins. */
            HashNode_t *field_node = NULL;
            char *field_id_copy = strdup(field_id);
            if (field_id_copy != NULL && FindIdent(&field_node, symtab, field_id_copy) >= 0 && field_node != NULL)
            {
                /* For unit-qualified access, find the entry from the correct unit */
                if (unit_is_qualifier && pascal_identifier_equals(unit_id, "System"))
                {
                    ListNode_t *all_nodes = FindAllIdents(symtab, field_id);
                    ListNode_t *cur_node = all_nodes;
                    HashNode_t *system_entry = NULL;
                    while (cur_node != NULL)
                    {
                        HashNode_t *n = (HashNode_t *)cur_node->cur;
                        if (n != NULL && n->hash_type == field_node->hash_type && !n->defined_in_unit)
                        {
                            system_entry = n;
                            break;
                        }
                        cur_node = cur_node->next;
                    }
                    if (system_entry != NULL)
                        field_node = system_entry;
                    /* Free the list nodes (not the HashNode_t payloads) */
                    while (all_nodes != NULL)
                    {
                        ListNode_t *tmp = all_nodes->next;
                        free(all_nodes);
                        all_nodes = tmp;
                    }
                }
                free(field_id_copy);
                /* Found the field as a direct identifier - transform the expression */
                if (field_node->hash_type == HASHTYPE_CONST)
                {
                    /* Transform to integer literal for constants */
                    expr->type = EXPR_INUM;
                    expr->expr_data.i_num = field_node->const_int_value;
                    semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
                    if (field_node->type != NULL)
                    {
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, field_node->type);
                    }
                    *type_return = LONGINT_TYPE;
                    return 0;
                }
                else if (field_node->hash_type == HASHTYPE_VAR || 
                         field_node->hash_type == HASHTYPE_ARRAY)
                {
                    /* Transform to simple variable reference */
                    char *field_copy = strdup(field_id);
                    if (field_copy == NULL)
                    {
                        semcheck_error_with_context("Error on line %d: failed to allocate memory for unit-qualified variable.\n",
                            expr->line_num);
                        *type_return = UNKNOWN_TYPE;
                        return 1;
                    }
                    expr->type = EXPR_VAR_ID;
                    expr->expr_data.id = field_copy;
                    return semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
                }
                else if (field_node->hash_type == HASHTYPE_TYPE)
                {
                    /* Unit.TypeName - transform to simple type reference */
                    char *field_copy = strdup(field_id);
                    if (field_copy == NULL)
                    {
                        semcheck_error_with_context("Error on line %d: failed to allocate memory for unit-qualified type.\n",
                            expr->line_num);
                        *type_return = UNKNOWN_TYPE;
                        return 1;
                    }
                    expr->type = EXPR_VAR_ID;
                    expr->expr_data.id = field_copy;
                    return semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
                }
                else if (field_node->hash_type == HASHTYPE_FUNCTION ||
                         field_node->hash_type == HASHTYPE_PROCEDURE)
                {
                    /* Unit.FuncName - transform to simple identifier and let
                     * semcheck_varid handle the conversion to a zero-arg call */
                    char *field_copy = strdup(field_id);
                    if (field_copy == NULL)
                    {
                        *type_return = UNKNOWN_TYPE;
                        return 1;
                    }
                    expr->type = EXPR_VAR_ID;
                    expr->expr_data.id = field_copy;
                    return semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
                }
                else if (field_node->hash_type == HASHTYPE_FUNCTION_RETURN)
                {
                    /* Unit.QualifiedName may resolve to the function-return helper
                     * symbol first; force a zero-arg function call in expression
                     * context so identifiers like System.GetLoadErrorStr are read
                     * as function values, not record-field-like accesses. */
                    char *field_copy = strdup(field_id);
                    if (field_copy == NULL)
                    {
                        *type_return = UNKNOWN_TYPE;
                        return 1;
                    }
                    expr->type = EXPR_FUNCTION_CALL;
                    memset(&expr->expr_data.function_call_data, 0,
                        sizeof(expr->expr_data.function_call_data));
                    expr->expr_data.function_call_data.id = field_copy;
                    expr->expr_data.function_call_data.args_expr = NULL;
                    expr->expr_data.function_call_data.mangled_id = NULL;
                    semcheck_reset_function_call_cache(expr);
                    return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                }
            }
            else
            {
                free(field_id_copy);
            }
        }
        /* Scoped enum support: TEnumType.EnumValue
         * When unit_check is found and it's a type with an enum, look up the field_id
         * as an enum literal and transform to its ordinal constant. */
        else if (unit_check != NULL && unit_check->hash_type == HASHTYPE_TYPE)
        {
            /* Check if the type has an enum type alias - look up field_id as enum literal */
            struct TypeAlias *type_alias = hashnode_get_type_alias(unit_check);
            if (type_alias != NULL && type_alias->is_enum && type_alias->enum_literals != NULL)
            {
                /* Search for field_id in enum_literals */
                int ordinal = 0;
                ListNode_t *literal_node = type_alias->enum_literals;
                while (literal_node != NULL)
                {
                    if (literal_node->cur != NULL)
                    {
                        char *literal_name = (char *)literal_node->cur;
                        if (strcasecmp(literal_name, field_id) == 0)
                        {
                            /* Found the enum literal - transform to integer constant */
                            expr->type = EXPR_INUM;
                            expr->expr_data.i_num = ordinal;
                            semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                            if (type_alias->kgpc_type != NULL)
                            {
                                /* Use the shared type setter to properly manage reference counting */
                                semcheck_expr_set_resolved_kgpc_type_shared(expr, type_alias->kgpc_type);
                            }
                            else
                            {
                                semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                            }
                            *type_return = ENUM_TYPE;
                            return 0;
                        }
                    }
                    ++ordinal;
                    literal_node = literal_node->next;
                }
                /* Enum literal not found in this type */
                semcheck_error_with_context("Error on line %d, '%s' is not a value of enum type '%s'.\n\n",
                    expr->line_num, field_id, unit_id);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }

            /* If this type is an alias to another type, try resolving scoped enum literal
             * against the alias target (e.g., TEndian = ObjPas.TEndian).
             */
            if (type_alias != NULL)
            {
                long long enum_value = 0;
                int resolved = 0;
                if (type_alias->target_type_ref != NULL &&
                    type_alias->target_type_ref->name != NULL)
                {
                    resolved = semcheck_resolve_scoped_enum_literal_ref(symtab,
                        type_alias->target_type_ref->name, field_id, &enum_value);
                }
                else if (type_alias->target_type_id != NULL)
                {
                    resolved = semcheck_resolve_scoped_enum_literal(symtab,
                        type_alias->target_type_id, field_id, &enum_value);
                }
                if (resolved)
                {
                    expr->type = EXPR_INUM;
                    expr->expr_data.i_num = enum_value;
                    semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                    if (type_alias->kgpc_type != NULL)
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, type_alias->kgpc_type);
                    *type_return = ENUM_TYPE;
                    return 0;
                }

                HashNode_t *literal_node = NULL;
                if (FindIdent(&literal_node, symtab, field_id) >= 0 &&
                    literal_node != NULL &&
                    (literal_node->hash_type == HASHTYPE_CONST ||
                     literal_node->is_constant ||
                     literal_node->is_typed_const))
                {
                    expr->type = EXPR_INUM;
                    expr->expr_data.i_num = literal_node->const_int_value;
                    semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                    if (type_alias->kgpc_type != NULL)
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, type_alias->kgpc_type);
                    *type_return = ENUM_TYPE;
                    return 0;
                }
            }

            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] enum alias fallback failed for %s\n", unit_id);
            }
        }
    }
    /* Enum member access in const expressions: resolve TEnum.Value even if TEnum
     * isn't found as a value in the current scope. */
    if (!mutating && record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
    {
        long long enum_value = 0;
        if (semcheck_resolve_scoped_enum_literal(symtab, record_expr->expr_data.id,
                field_id, &enum_value))
        {
            expr->type = EXPR_INUM;
            expr->expr_data.i_num = enum_value;
            semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
            *type_return = ENUM_TYPE;
            return 0;
        }
    }

    /* Unit-qualified identifier resolution: UnitName.Identifier
     * When the base expression is a unit name, resolve field_id directly
     * from the symbol table and transform this expression accordingly. */
    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL &&
        semcheck_is_unit_name(record_expr->expr_data.id) &&
        !semcheck_has_value_ident(symtab, record_expr->expr_data.id))
    {
        if (getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_RECORD_ACCESS] unit-name branch: id=%s field=%s\n",
                record_expr->expr_data.id,
                field_id != NULL ? field_id : "(null)");
        }
        HashNode_t *unit_sym = NULL;
        if (FindIdent(&unit_sym, symtab, field_id) >= 0 && unit_sym != NULL)
        {
            /* Transform EXPR_RECORD_ACCESS into EXPR_VAR_ID for the resolved identifier */
            destroy_expr(record_expr);
            free(expr->expr_data.record_access_data.field_id);
            expr->expr_data.record_access_data.record_expr = NULL;
            expr->expr_data.record_access_data.field_id = NULL;
            expr->type = EXPR_VAR_ID;
            expr->expr_data.id = strdup(field_id);
            if (expr->expr_data.id == NULL)
            {
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            KgpcType *resolved_kgpc = NULL;
            int result = semcheck_expr_with_type(&resolved_kgpc, symtab, expr, max_scope_lev, mutating);
            *type_return = semcheck_tag_from_kgpc(resolved_kgpc);
            return result;
        }
        if (getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_RECORD_ACCESS] unit-name no symbol: id=%s field=%s\n",
                record_expr->expr_data.id,
                field_id != NULL ? field_id : "(null)");
        }

        /* Fallback for qualified zero-arg function access like
         * UnitName.FuncName used without parentheses in expression context. */
        if (field_id != NULL)
        {
            char *field_copy = strdup(field_id);
            if (field_copy == NULL)
            {
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            destroy_expr(record_expr);
            free(expr->expr_data.record_access_data.field_id);
            expr->expr_data.record_access_data.record_expr = NULL;
            expr->expr_data.record_access_data.field_id = NULL;
            expr->type = EXPR_FUNCTION_CALL;
            memset(&expr->expr_data.function_call_data, 0,
                sizeof(expr->expr_data.function_call_data));
            expr->expr_data.function_call_data.id = field_copy;
            expr->expr_data.function_call_data.args_expr = NULL;
            expr->expr_data.function_call_data.mangled_id = NULL;
            semcheck_reset_function_call_cache(expr);
            return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
        }
    }

    int error_count = 0;
    int record_type = UNKNOWN_TYPE;
    KgpcType *record_kgpc_type = NULL;
    /* Evaluate the record/object expression as a read: in `obj.field := val`,
     * `obj` is read (to obtain the container), only `field` is mutated.
     * Using NO_MUTATE avoids false "property is read-only" errors when an
     * indexed property returns a class reference whose member is assigned. */
    error_count += semcheck_expr_with_type(&record_kgpc_type, symtab, record_expr, max_scope_lev, NO_MUTATE);
    record_type = semcheck_tag_from_kgpc(record_kgpc_type);
    if (getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
    {
        const char *type_str = NULL;
        if (record_kgpc_type != NULL)
            type_str = kgpc_type_to_string(record_kgpc_type);
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_ACCESS] record_expr_type=%d record_type=%d kgpc=%s\n",
            record_expr->type,
            record_type,
            type_str != NULL ? type_str : "<null>");
    }

    if (record_expr->type == EXPR_RECORD_ACCESS)
    {
        struct Expression *inner_rec = record_expr->expr_data.record_access_data.record_expr;
        const char *inner_field = record_expr->expr_data.record_access_data.field_id;
        if (inner_rec != NULL && inner_rec->type == EXPR_VAR_ID &&
            inner_rec->expr_data.id != NULL &&
            pascal_identifier_equals(inner_rec->expr_data.id, "Self") &&
            inner_field != NULL &&
            semcheck_has_value_ident(symtab, inner_field))
        {
            struct Expression *value_expr = mk_varid(expr->line_num, strdup(inner_field));
            if (value_expr != NULL)
            {
                destroy_expr(record_expr);
                expr->expr_data.record_access_data.record_expr = value_expr;
                record_expr = value_expr;
                if (record_kgpc_type != NULL)
                {
                    kgpc_type_release(record_kgpc_type);
                    record_kgpc_type = NULL;
                }
                error_count += semcheck_expr_with_type(&record_kgpc_type, symtab,
                    record_expr, max_scope_lev, NO_MUTATE);
                record_type = semcheck_tag_from_kgpc(record_kgpc_type);
            }
        }
    }

    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
    {
        if (semcheck_has_value_ident(symtab, record_expr->expr_data.id))
        {
            /* Value identifiers shadow implicit Self field access. */
            goto SKIP_SELF_FIELD_REWRITE;
        }
        HashNode_t *self_node = NULL;
        if (FindIdent(&self_node, symtab, "Self") == 0 && self_node != NULL)
        {
            struct RecordType *self_record = get_record_type_from_node(self_node);
            if (self_record != NULL)
            {
                struct RecordType *expr_record = NULL;
                if (record_kgpc_type != NULL && kgpc_type_is_record(record_kgpc_type))
                    expr_record = kgpc_type_get_record(record_kgpc_type);

                if (expr_record == self_record)
                {
                    struct RecordField *self_field = NULL;
                    long long field_offset = 0;
                    if (resolve_record_field(symtab, self_record, record_expr->expr_data.id,
                            &self_field, &field_offset, expr->line_num, 1) == 0 &&
                        self_field != NULL)
                    {
                        struct Expression *self_expr = mk_varid(expr->line_num, strdup("Self"));
                        if (self_expr != NULL)
                        {
                            if (self_node->type != NULL)
                            {
                                self_expr->resolved_kgpc_type = self_node->type;
                                kgpc_type_retain(self_node->type);
                            }
                            else
                            {
                                KgpcType *self_record_type = create_record_type(self_record);
                                if (self_record_type != NULL)
                                    self_expr->resolved_kgpc_type = self_record_type;
                            }

                            char *saved_id = record_expr->expr_data.id;
                            record_expr->expr_data.id = NULL;
                            record_expr->type = EXPR_RECORD_ACCESS;
                            memset(&record_expr->expr_data.record_access_data, 0,
                                sizeof(expr->expr_data.record_access_data));
                            record_expr->expr_data.record_access_data.record_expr = self_expr;
                            record_expr->expr_data.record_access_data.field_id = saved_id;
                            record_expr->expr_data.record_access_data.field_offset = field_offset;

                            if (record_kgpc_type != NULL)
                            {
                                kgpc_type_release(record_kgpc_type);
                                record_kgpc_type = NULL;
                            }
                            error_count += semcheck_expr_with_type(&record_kgpc_type, symtab,
                                record_expr, max_scope_lev, NO_MUTATE);
                            record_type = semcheck_tag_from_kgpc(record_kgpc_type);
                        }
                    }
                }
            }
        }
    }
SKIP_SELF_FIELD_REWRITE:

    if (record_type == ENUM_TYPE)
    {
        const char *expr_type_name = get_expr_type_name(record_expr, symtab);
        const char *enum_type_name = expr_type_name;
        long long enum_value = 0;
        if (enum_type_name != NULL &&
            semcheck_resolve_scoped_enum_literal(symtab, enum_type_name, field_id, &enum_value))
        {
            expr->type = EXPR_INUM;
            expr->expr_data.i_num = enum_value;
            semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
            if (record_kgpc_type != NULL)
                semcheck_expr_set_resolved_kgpc_type_shared(expr, record_kgpc_type);
            *type_return = ENUM_TYPE;
            return error_count;
        }
        if (record_kgpc_type != NULL && record_kgpc_type->type_alias != NULL &&
            record_kgpc_type->type_alias->target_type_id != NULL)
        {
            enum_type_name = record_kgpc_type->type_alias->target_type_id;
            if (semcheck_resolve_scoped_enum_literal(symtab, enum_type_name, field_id, &enum_value))
            {
                expr->type = EXPR_INUM;
                expr->expr_data.i_num = enum_value;
                semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                semcheck_expr_set_resolved_kgpc_type_shared(expr, record_kgpc_type);
                *type_return = ENUM_TYPE;
                return error_count;
            }
        }
    }

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_recordaccess: field_id=%s, record_type=%d\n",
            field_id, record_type);
    }


    struct RecordType *record_info = NULL;
    if (record_type == RECORD_TYPE)
    {
        if (record_info == NULL && record_kgpc_type != NULL &&
            kgpc_type_is_record(record_kgpc_type))
        {
            record_info = kgpc_type_get_record(record_kgpc_type);
        }
        if (getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_RECORD_ACCESS] record_info=%p from_kgpc=%d\n",
                (void *)record_info,
                record_kgpc_type != NULL && kgpc_type_is_record(record_kgpc_type));
        }
        if (record_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_record(record_expr->resolved_kgpc_type)) {
            record_info = kgpc_type_get_record(record_expr->resolved_kgpc_type);
        }
        if (record_info == NULL && record_kgpc_type != NULL &&
            record_kgpc_type->type_alias != NULL)
        {
            const char *alias_target = record_kgpc_type->type_alias->target_type_id;
            const char *alias_name = record_kgpc_type->type_alias->alias_name;
            if (alias_target != NULL)
                record_info = semcheck_lookup_record_type(symtab, alias_target);
            if (record_info == NULL && alias_name != NULL)
                record_info = semcheck_lookup_record_type(symtab, alias_name);
        }
        if (record_info == NULL)
        {
            const char *expr_type_name = get_expr_type_name(record_expr, symtab);
            if (expr_type_name != NULL)
                record_info = semcheck_lookup_record_type(symtab, expr_type_name);
        }
        if (record_info == NULL && record_expr != NULL &&
            record_expr->type == EXPR_ARRAY_ACCESS)
        {
            if (record_expr->array_element_record_type != NULL)
                record_info = record_expr->array_element_record_type;
            if (record_info == NULL && record_expr->array_element_type_id != NULL)
                record_info = semcheck_lookup_record_type(symtab, record_expr->array_element_type_id);

            struct Expression *base_array_expr = record_expr->expr_data.array_access_data.array_expr;
            if (record_info == NULL && base_array_expr != NULL)
            {
                if (base_array_expr->array_element_record_type != NULL)
                    record_info = base_array_expr->array_element_record_type;
                if (record_info == NULL && base_array_expr->array_element_type_id != NULL)
                    record_info = semcheck_lookup_record_type(symtab, base_array_expr->array_element_type_id);
                if (record_info == NULL && base_array_expr->resolved_kgpc_type != NULL &&
                    kgpc_type_is_array(base_array_expr->resolved_kgpc_type))
                {
                    KgpcType *elem_type = kgpc_type_get_array_element_type(base_array_expr->resolved_kgpc_type);
                    if (elem_type != NULL && kgpc_type_is_record(elem_type))
                        record_info = kgpc_type_get_record(elem_type);
                }
            }
        }
        if (record_info == NULL && record_expr->type == EXPR_TYPECAST)
        {
            const char *target_id = record_expr->expr_data.typecast_data.target_type_id;
            if (target_id != NULL)
            {
                HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type(symtab, target_id);
                if (type_node == NULL)
                    FindIdent(&type_node, symtab, target_id);
                if (type_node != NULL)
                {
                    record_info = get_record_type_from_node(type_node);
                    if (record_info == NULL && type_node->type != NULL &&
                        kgpc_type_is_record(type_node->type))
                    {
                        record_info = kgpc_type_get_record(type_node->type);
                    }
                    if (record_expr->resolved_kgpc_type == NULL && type_node->type != NULL)
                    {
                        kgpc_type_retain(type_node->type);
                        record_expr->resolved_kgpc_type = type_node->type;
                    }
                }
            }
        }
    }
    else if (record_type == POINTER_TYPE)
    {
        if (record_info == NULL && record_expr->type == EXPR_POINTER_DEREF)
        {
            struct Expression *ptr_expr = record_expr->expr_data.pointer_deref_data.pointer_expr;
            const char *subtype_id = NULL;
            const TypeRef *subtype_ref = NULL;
            if (ptr_expr != NULL)
            {
                subtype_id = ptr_expr->pointer_subtype_id;
                subtype_ref = ptr_expr->pointer_subtype_ref;
                if (subtype_id == NULL && ptr_expr->type == EXPR_TYPECAST)
                {
                    const char *target_id = ptr_expr->expr_data.typecast_data.target_type_id;
                    const TypeRef *target_ref = ptr_expr->expr_data.typecast_data.target_type_ref;
                    HashNode_t *target_node = semcheck_find_preferred_type_node_with_ref(symtab,
                        target_ref, target_id);
                    if (target_node == NULL && target_id != NULL)
                    {
                        const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
                        const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
                        if (owner_full == NULL)
                            owner_full = semcheck_get_current_method_owner();
                        target_node = semcheck_find_type_node_in_owner_chain(symtab, target_id,
                            owner_full, owner_outer);
                    }
                    if (target_node != NULL)
                    {
                        struct TypeAlias *alias = get_type_alias_from_node(target_node);
                        if (alias != NULL)
                        {
                            if (alias->pointer_type_id != NULL)
                                subtype_id = alias->pointer_type_id;
                            if (alias->pointer_type_ref != NULL)
                                subtype_ref = alias->pointer_type_ref;
                        }
                    }
                }
            }
            if (subtype_id != NULL)
            {
                HashNode_t *target_node = semcheck_find_preferred_type_node_with_ref(symtab,
                    subtype_ref, subtype_id);
                if (target_node == NULL)
                {
                    const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
                    const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
                    if (owner_full == NULL)
                        owner_full = semcheck_get_current_method_owner();
                    target_node = semcheck_find_type_node_in_owner_chain(symtab, subtype_id,
                        owner_full, owner_outer);
                }
                if (target_node != NULL)
                {
                    record_info = get_record_type_from_node(target_node);
                }
                if (record_info == NULL)
                    record_info = semcheck_lookup_record_type(symtab, subtype_id);
                if (record_info != NULL)
                    record_type = RECORD_TYPE;
            }
        }

        /* Try resolved KgpcType pointer target */
        if (record_info == NULL && record_expr->resolved_kgpc_type != NULL &&
            record_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER) {
            KgpcType *pointee = record_expr->resolved_kgpc_type->info.points_to;
            if (pointee != NULL && kgpc_type_is_record(pointee)) {
                record_info = kgpc_type_get_record(pointee);
            }
        }
        if (record_info == NULL && record_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *target_node =
                semcheck_find_type_node_with_kgpc_type(symtab, record_expr->pointer_subtype_id);
            if (target_node == NULL)
                FindIdent(&target_node, symtab, record_expr->pointer_subtype_id);
            if (target_node != NULL)
                record_info = get_record_type_from_node(target_node);
        }

        if (record_info == NULL)
        {
            /* Check for type helpers on Pointer type before giving up.
             * Type helpers can be defined for Pointer, PChar, etc. */
            const char *expr_type_name = get_expr_type_name(record_expr, symtab);
            const char *alias_type_name = NULL;
            if (record_expr->resolved_kgpc_type != NULL &&
                record_expr->resolved_kgpc_type->type_alias != NULL &&
                record_expr->resolved_kgpc_type->type_alias->target_type_id != NULL)
            {
                alias_type_name = record_expr->resolved_kgpc_type->type_alias->target_type_id;
            }
            struct RecordType *helper_record = semcheck_lookup_type_helper(symtab, record_type,
                alias_type_name != NULL ? alias_type_name : expr_type_name);
            if (helper_record != NULL)
            {
                record_type = RECORD_TYPE;
                record_info = helper_record;
            }
            else
            {
                semcheck_error_with_context("Error on line %d, pointer does not reference a record type.\n\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return error_count + 1;
            }
        }
    }
    else
    {
        const char *expr_type_name = get_expr_type_name(record_expr, symtab);
        const char *alias_type_name = NULL;
        if (record_expr->resolved_kgpc_type != NULL &&
            record_expr->resolved_kgpc_type->type_alias != NULL &&
            record_expr->resolved_kgpc_type->type_alias->target_type_id != NULL)
        {
            alias_type_name = record_expr->resolved_kgpc_type->type_alias->target_type_id;
        }
        /* When the record_expr is literally "Self" inside a type helper
         * method body, use the current method's owning helper type rather
         * than the most-recently-registered helper for this base type.
         * This matters when multiple type helpers exist for the same base
         * type (e.g. TWideStringHelper from SysUtils and a user-defined
         * TWideHelper): Self.Length inside TWideStringHelper must resolve
         * to TWideStringHelper's Length property, not TWideHelper's. */
        struct RecordType *helper_record = NULL;
        int is_self_expr = (record_expr->type == EXPR_VAR_ID &&
            record_expr->expr_data.id != NULL &&
            pascal_identifier_equals(record_expr->expr_data.id, "Self"));
        if (is_self_expr)
        {
            const char *current_owner = semcheck_get_current_method_owner();
            if (current_owner != NULL)
            {
                struct RecordType *owner_rec = semcheck_lookup_record_type(symtab, current_owner);
                if (owner_rec != NULL && owner_rec->is_type_helper)
                    helper_record = owner_rec;
            }
        }
        if (helper_record == NULL)
            helper_record = semcheck_lookup_type_helper(symtab, record_type,
                alias_type_name != NULL ? alias_type_name : expr_type_name);
        if (helper_record == NULL && record_type == REAL_TYPE)
        {
            const char *helper_base = alias_type_name != NULL ? alias_type_name : expr_type_name;
            if (helper_base != NULL)
            {
                char helper_name[256];
                snprintf(helper_name, sizeof(helper_name), "T%sHelper", helper_base);
                helper_record = semcheck_lookup_record_type(symtab, helper_name);
            }
        }
        if (helper_record != NULL)
        {
            record_type = RECORD_TYPE;
            record_info = helper_record;
        }
        else if (record_type == REAL_TYPE && field_id != NULL &&
                 pascal_identifier_equals(field_id, "IsNan"))
        {
            /* FPC allows Float.IsNan via type helpers. If helpers weren't registered,
             * fall back to a simple NaN check (x <> x). */
            struct Expression *left_expr = record_expr;
            struct Expression *right_expr = clone_expression(record_expr);
            if (right_expr == NULL)
            {
                semcheck_error_with_context("Error on line %d, failed to allocate IsNan expression.\n\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return error_count + 1;
            }
            if (expr->expr_data.record_access_data.field_id != NULL)
            {
                free(expr->expr_data.record_access_data.field_id);
                expr->expr_data.record_access_data.field_id = NULL;
            }

            expr->type = EXPR_RELOP;
            memset(&expr->expr_data.relop_data, 0, sizeof(expr->expr_data.relop_data));
            expr->expr_data.relop_data.type = NE;
            expr->expr_data.relop_data.left = left_expr;
            expr->expr_data.relop_data.right = right_expr;

            return semcheck_expr_legacy_tag(type_return, symtab, expr, max_scope_lev, mutating);
        }
        else if (record_type == ENUM_TYPE && field_id != NULL)
        {
            /* Scoped enum through nested type: TClass.TEnum.Value
             * The inner access resolved to an enum type; look up field_id as a literal. */
            const char *enum_type_name = expr_type_name;
            if (enum_type_name == NULL && record_expr->resolved_kgpc_type != NULL &&
                record_expr->resolved_kgpc_type->type_alias != NULL)
            {
                enum_type_name = record_expr->resolved_kgpc_type->type_alias->alias_name;
                if (enum_type_name == NULL)
                    enum_type_name = record_expr->resolved_kgpc_type->type_alias->target_type_id;
            }
            long long enum_value = 0;
            int resolved = 0;
            if (enum_type_name != NULL)
                resolved = semcheck_resolve_scoped_enum_literal(symtab, enum_type_name, field_id, &enum_value);
            if (!resolved)
            {
                /* Try looking up the field_id directly as a global enum constant */
                HashNode_t *enum_node = NULL;
                if (FindIdent(&enum_node, symtab, field_id) >= 0 && enum_node != NULL &&
                    enum_node->hash_type == HASHTYPE_CONST)
                {
                    resolved = 1;
                    if (enum_node->type != NULL && enum_node->type->kind == TYPE_KIND_PRIMITIVE)
                        enum_value = enum_node->const_int_value;
                    else
                        enum_value = enum_node->const_int_value;
                }
            }
            if (resolved)
            {
                expr->type = EXPR_INUM;
                expr->expr_data.i_num = enum_value;
                semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                if (record_expr->resolved_kgpc_type != NULL)
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, record_expr->resolved_kgpc_type);
                *type_return = ENUM_TYPE;
                return 0;
            }
            semcheck_error_with_context("Error on line %d, field access requires a record value.\n\n", expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return error_count + 1;
        }
        else
        {
            if (record_expr->type == EXPR_VAR_ID &&
                record_expr->expr_data.id != NULL)
            {
                HashNode_t *type_node =
                    semcheck_find_preferred_type_node(symtab, record_expr->expr_data.id);
                if (type_node == NULL)
                {
                    const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
                    const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
                    if (owner_full == NULL)
                        owner_full = semcheck_get_current_method_owner();
                    type_node = semcheck_find_type_node_in_owner_chain(symtab,
                        record_expr->expr_data.id, owner_full, owner_outer);
                }
                if ((type_node == NULL || type_node->hash_type != HASHTYPE_TYPE))
                {
                    HashNode_t *fallback_node = NULL;
                    if (FindIdent(&fallback_node, symtab, record_expr->expr_data.id) == 0 &&
                        fallback_node != NULL && fallback_node->hash_type == HASHTYPE_TYPE)
                        type_node = fallback_node;
                }
                if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                {
                    struct RecordType *type_record = get_record_type_from_node(type_node);
                    if (type_record != NULL)
                    {
                        record_type = RECORD_TYPE;
                        record_info = type_record;
                        if (record_kgpc_type == NULL || !kgpc_type_is_record(record_kgpc_type))
                        {
                            if (record_kgpc_type != NULL)
                                kgpc_type_release(record_kgpc_type);
                            record_kgpc_type = create_record_type(type_record);
                        }
                    }
                }
            }
            if (record_info != NULL)
            {
                /* Type-qualified record member access (e.g. SizeOf(TRec.Field)). */
            }
            else
            {
            if (getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
            {
                const char *rec_id = NULL;
                if (record_expr != NULL && record_expr->type == EXPR_VAR_ID)
                    rec_id = record_expr->expr_data.id;
                fprintf(stderr,
                    "[KGPC_DEBUG_RECORD_ACCESS] not-record: line=%d expr=%p record_expr=%p type=%d rec_id=%s record_type=%d field=%s\n",
                    expr->line_num,
                    (void *)expr,
                    (void *)record_expr,
                    record_expr != NULL ? record_expr->type : -1,
                    rec_id != NULL ? rec_id : "(null)",
                    record_type,
                    field_id != NULL ? field_id : "(null)");
            }
            semcheck_error_with_context("Error on line %d, field access requires a record value.\n\n", expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return error_count + 1;
            }
        }
    }


    if (record_info == NULL)
    {
        semcheck_error_with_context("Error on line %d, unable to resolve record type for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
    {
        if (semcheck_has_value_ident(symtab, record_expr->expr_data.id))
        {
            /* Local/parameter identifiers must shadow implicit Self field access. */
            goto SKIP_SELF_REWRITE;
        }
        HashNode_t *self_node = NULL;
        if (FindIdent(&self_node, symtab, "Self") == 0 && self_node != NULL)
        {
            struct RecordType *self_record = get_record_type_from_node(self_node);
            if (self_record != NULL)
            {
                struct RecordType *expr_record = NULL;
                if (record_kgpc_type != NULL && kgpc_type_is_record(record_kgpc_type))
                    expr_record = kgpc_type_get_record(record_kgpc_type);

                int self_match = 0;
                if (expr_record == NULL)
                    self_match = 1;
                else if (expr_record->type_id != NULL && self_record->type_id != NULL &&
                         pascal_identifier_equals(expr_record->type_id, self_record->type_id))
                    self_match = 1;

                if (!self_match)
                    goto SKIP_SELF_REWRITE;

                struct RecordField *self_field = NULL;
                long long field_offset = 0;
                if (resolve_record_field(symtab, self_record, record_expr->expr_data.id,
                        &self_field, &field_offset, expr->line_num, 1) == 0 &&
                    self_field != NULL)
                {
                    struct Expression *self_expr = mk_varid(expr->line_num, strdup("Self"));
                    if (self_expr != NULL)
                    {
                        if (self_node->type != NULL)
                        {
                            self_expr->resolved_kgpc_type = self_node->type;
                            kgpc_type_retain(self_node->type);
                        }
                        else
                        {
                            KgpcType *self_record_type = create_record_type(self_record);
                            if (self_record_type != NULL)
                                self_expr->resolved_kgpc_type = self_record_type;
                        }

                        char *saved_id = record_expr->expr_data.id;
                        record_expr->expr_data.id = NULL;
                        record_expr->type = EXPR_RECORD_ACCESS;
                        memset(&record_expr->expr_data.record_access_data, 0,
                            sizeof(expr->expr_data.record_access_data));
                        record_expr->expr_data.record_access_data.record_expr = self_expr;
                        record_expr->expr_data.record_access_data.field_id = saved_id;
                        record_expr->expr_data.record_access_data.field_offset = field_offset;

                        if (self_field->nested_record != NULL)
                            record_info = self_field->nested_record;
                        else if (self_field->type_id != NULL)
                            record_info = semcheck_lookup_record_type(symtab, self_field->type_id);

                        if (record_info != NULL)
                        {
                            if (record_kgpc_type != NULL)
                                kgpc_type_release(record_kgpc_type);
                            record_kgpc_type = create_record_type(record_info);
                            record_type = RECORD_TYPE;
                        }
                    }
                }
            SKIP_SELF_REWRITE:
                ;
            }
        }
    }

    struct RecordField *field_desc = NULL;
    long long field_offset = 0;
    int property_matched = 0;
    /* For classes and records with potential methods, use silent mode when looking for fields,
     * since we'll check properties and methods next */
    int silent_mode = 1;  /* Always use silent mode - we'll print a better error later if needed */
    if (resolve_record_field(symtab, record_info, field_id, &field_desc,
            &field_offset, expr->line_num, silent_mode) != 0 || field_desc == NULL)
    {
        if (record_info != NULL && record_info->is_type_helper &&
            record_info->helper_base_type_id != NULL)
        {
            struct RecordType *base_record =
                semcheck_lookup_record_type(symtab, record_info->helper_base_type_id);
            if (base_record != NULL)
            {
                struct RecordField *base_field = NULL;
                long long base_offset = 0;
                if (resolve_record_field(symtab, base_record, field_id, &base_field,
                        &base_offset, expr->line_num, 1) == 0 && base_field != NULL)
                {
                    record_info = base_record;
                    field_desc = base_field;
                    field_offset = base_offset;
                }
            }
        }
        if (field_desc == NULL && record_kgpc_type != NULL &&
            record_kgpc_type->type_alias != NULL)
        {
            const char *alias_target = record_kgpc_type->type_alias->target_type_id;
            const char *alias_name = record_kgpc_type->type_alias->alias_name;
            if (alias_target != NULL || alias_name != NULL)
            {
                struct RecordType *alias_record = NULL;
                if (alias_target != NULL)
                    alias_record = semcheck_lookup_record_type(symtab, alias_target);
                if (alias_record == NULL && alias_name != NULL)
                    alias_record = semcheck_lookup_record_type(symtab, alias_name);
                if (alias_record != NULL)
                {
                    struct RecordField *alias_field = NULL;
                    long long alias_offset = 0;
                    if (resolve_record_field(symtab, alias_record, field_id, &alias_field,
                            &alias_offset, expr->line_num, 1) == 0 && alias_field != NULL)
                    {
                        record_info = alias_record;
                        field_desc = alias_field;
                        field_offset = alias_offset;
                    }
                }
            }
        }
        if (field_desc == NULL && record_expr->type == EXPR_VAR_ID &&
            record_expr->expr_data.id != NULL)
        {
            HashNode_t *record_node = NULL;
            if (FindIdent(&record_node, symtab, record_expr->expr_data.id) == 0 &&
                record_node != NULL)
            {
                struct RecordType *node_record = get_record_type_from_node(record_node);
                if (node_record != NULL)
                {
                    struct RecordField *node_field = NULL;
                    long long node_offset = 0;
                    if (resolve_record_field(symtab, node_record, field_id, &node_field,
                            &node_offset, expr->line_num, 1) == 0 && node_field != NULL)
                    {
                        record_info = node_record;
                        field_desc = node_field;
                        field_offset = node_offset;
                    }
                }
            }
        }
        if (record_info != NULL && record_info->type_id != NULL)
        {
            ListNode_t *type_matches = FindAllIdents(symtab, record_info->type_id);
            for (ListNode_t *mcur = type_matches; mcur != NULL; mcur = mcur->next)
            {
                HashNode_t *mnode = (HashNode_t *)mcur->cur;
                if (mnode == NULL || mnode->hash_type != HASHTYPE_TYPE)
                    continue;
                struct RecordType *alt_record = get_record_type_from_node(mnode);
                if (alt_record == NULL || alt_record == record_info)
                    continue;
                struct RecordField *alt_field = NULL;
                long long alt_offset = 0;
                if (resolve_record_field(symtab, alt_record, field_id, &alt_field,
                        &alt_offset, expr->line_num, 1) == 0 && alt_field != NULL)
                {
                    record_info = alt_record;
                    field_desc = alt_field;
                    field_offset = alt_offset;
                    break;
                }
            }
            if (type_matches != NULL)
                DestroyList(type_matches);
        }
    }
    if (field_desc == NULL)
    {
        struct RecordType *property_owner = NULL;
        struct ClassProperty *property = semcheck_find_class_property(symtab,
            record_info, field_id, &property_owner);
        if (property != NULL)
        {
            property_matched = 1;
            if (mutating == NO_MUTATE)
            {
                if (property->read_accessor == NULL)
                {
                    semcheck_error_with_context("Error on line %d, property %s is write-only.\n\n",
                        expr->line_num, property->name != NULL ? property->name : field_id);
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }

                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck]   Property read_accessor='%s'\n",
                        property->read_accessor ? property->read_accessor : "<null>");
                }

                struct RecordField *read_field =
                    semcheck_find_class_field_including_hidden(symtab,
                        record_info, property->read_accessor, NULL);
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck]   Found read_field=%p\n", read_field);
                }
                if (read_field != NULL &&
                    resolve_record_field(symtab, record_info, property->read_accessor,
                        &field_desc, &field_offset, expr->line_num, 0) == 0 &&
                    field_desc != NULL)
                {
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck]   Transforming property '%s' to field '%s'\n",
                            field_id, property->read_accessor);
                    }
                    if (!pascal_identifier_equals(field_id, property->read_accessor))
                    {
                        free(expr->expr_data.record_access_data.field_id);
                        expr->expr_data.record_access_data.field_id = strdup(property->read_accessor);
                        if (expr->expr_data.record_access_data.field_id == NULL)
                        {
                            semcheck_error_with_context("Error on line %d, failed to allocate property field name.\n\n",
                                expr->line_num);
                            *type_return = UNKNOWN_TYPE;
                            return error_count + 1;
                        }
                    }
                    goto FIELD_RESOLVED;
                }

                HashNode_t *getter_node = semcheck_find_class_method(symtab,
                    property_owner, property->read_accessor, NULL);
                if (getter_node == NULL)
                {
                    semcheck_error_with_context("Error on line %d, getter %s for property %s not found.\n\n",
                        expr->line_num,
                        property->read_accessor != NULL ? property->read_accessor : "<unknown>",
                        property->name != NULL ? property->name : field_id);
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }
                int getter_is_function = (getter_node->hash_type == HASHTYPE_FUNCTION);
                if (!getter_is_function && getter_node->type != NULL &&
                    getter_node->type->kind == TYPE_KIND_PROCEDURE &&
                    getter_node->type->info.proc_info.return_type == NULL)
                {
                    int prop_type = UNKNOWN_TYPE;
                    struct RecordType *prop_record = NULL;
                    if (semcheck_property_type_info(symtab, property, expr->line_num,
                            &prop_type, &prop_record) == 0)
                    {
                        KgpcType *prop_kgpc = NULL;
                        if (prop_record != NULL)
                            prop_kgpc = create_record_type(prop_record);
                        else if (prop_type != UNKNOWN_TYPE)
                            prop_kgpc = create_primitive_type(prop_type);
                        if (prop_kgpc != NULL)
                        {
                            getter_node->type->info.proc_info.return_type = prop_kgpc;
                            getter_node->hash_type = HASHTYPE_FUNCTION;
                            getter_is_function = 1;
                        }
                    }
                }
                if (!getter_is_function && getter_node->type != NULL &&
                    getter_node->type->kind == TYPE_KIND_PROCEDURE)
                {
                    KgpcType *ret_type = kgpc_type_get_return_type(getter_node->type);
                    if (ret_type != NULL)
                        getter_is_function = 1;
                }
                if (!getter_is_function)
                {
                    semcheck_error_with_context("Error on line %d, property getter %s must be a function.\n\n",
                        expr->line_num, property->read_accessor);
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }

                return semcheck_transform_property_getter_call(type_return, symtab,
                    expr, max_scope_lev, mutating, getter_node, property_owner);
            }
            else
            {
                if (property->write_accessor == NULL)
                {
                    semcheck_error_with_context("Error on line %d, property %s is read-only.\n\n",
                        expr->line_num, property->name != NULL ? property->name : field_id);
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }

                struct RecordField *write_field =
                    semcheck_find_class_field_including_hidden(symtab,
                        record_info, property->write_accessor, NULL);
                if (write_field != NULL &&
                    resolve_record_field(symtab, record_info, property->write_accessor,
                        &field_desc, &field_offset, expr->line_num, 0) == 0 &&
                    field_desc != NULL)
                {
                    if (!pascal_identifier_equals(field_id, property->write_accessor))
                    {
                        free(expr->expr_data.record_access_data.field_id);
                        expr->expr_data.record_access_data.field_id = strdup(property->write_accessor);
                        if (expr->expr_data.record_access_data.field_id == NULL)
                        {
                            semcheck_error_with_context("Error on line %d, failed to allocate property field name.\n\n",
                                expr->line_num);
                            *type_return = UNKNOWN_TYPE;
                            return error_count + 1;
                        }
                    }
                    goto FIELD_RESOLVED;
                }

                if (mutating == BOTH_MUTATE_REFERENCE)
                {
                    semcheck_error_with_context("Error on line %d, property %s cannot be passed as a var parameter.\n\n",
                        expr->line_num, property->name != NULL ? property->name : field_id);
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }

                HashNode_t *setter_node = semcheck_find_class_method(symtab,
                    property_owner, property->write_accessor, NULL);
                if (setter_node == NULL)
                {
                    semcheck_error_with_context("Error on line %d, setter %s for property %s not found.\n\n",
                        expr->line_num,
                        property->write_accessor != NULL ? property->write_accessor : "<unknown>",
                        property->name != NULL ? property->name : field_id);
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }
                if (setter_node->hash_type != HASHTYPE_PROCEDURE)
                {
                    semcheck_error_with_context("Error on line %d, property setter %s must be a procedure.\n\n",
                        expr->line_num, property->write_accessor);
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }

                int property_type = UNKNOWN_TYPE;
                struct RecordType *property_record = NULL;
                if (semcheck_property_type_info(symtab, property, expr->line_num,
                        &property_type, &property_record) != 0)
                {
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }

                semcheck_expr_set_resolved_type(expr, property_type);
                if (property_type == RECORD_TYPE && property_record != NULL)
                {
                    KgpcType *property_record_type = create_record_type(property_record);
                    if (property_record_type != NULL)
                    {
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, property_record_type);
                        destroy_kgpc_type(property_record_type);
                    }
                }
                *type_return = property_type;
                return error_count;
            }

            return error_count;
        }

        /* Check for methods (including constructors) */
        HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, field_id, NULL);
        if (method_node != NULL)
        {
            /* Found a method/constructor */
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] semcheck_recordaccess: Found method %s\n", field_id);
            }
                
                if (method_node->hash_type == HASHTYPE_FUNCTION || 
                    method_node->hash_type == HASHTYPE_PROCEDURE)
                {
                    int is_static_method = 0;
                    if (record_info->type_id != NULL && field_id != NULL) {
                        is_static_method = from_cparser_is_method_static(record_info->type_id, field_id);
                    }

                    /* For overloaded methods, we need to find the correct overload based on
                     * argument count. Since this is a bare record access (s.Foo without parens),
                     * there are no explicit arguments - only the implicit Self parameter.
                     * Find the overload that accepts just Self (1 param). */
                    int args_for_call = is_static_method ? 0 : 1; /* Just Self for non-static */
                    
                    if (record_info->type_id != NULL) {
                        char mangled_base[256];
                        snprintf(mangled_base, sizeof(mangled_base), "%s__%s",
                            record_info->type_id, field_id);
                        
                        ListNode_t *all_methods = FindAllIdents(symtab, mangled_base);
                        if (all_methods != NULL) {
                            /* Find the best overload: one that requires exactly args_for_call params */
                            ListNode_t *cur = all_methods;
                            HashNode_t *best_match = NULL;
                            
                            while (cur != NULL) {
                                HashNode_t *candidate = (HashNode_t *)cur->cur;
                                if (candidate != NULL &&
                                    (candidate->hash_type == HASHTYPE_FUNCTION ||
                                     candidate->hash_type == HASHTYPE_PROCEDURE) &&
                                    candidate->type != NULL)
                                {
                                    ListNode_t *params = kgpc_type_get_procedure_params(candidate->type);
                                    int total_params = semcheck_count_total_params(params);
                                    int required_params = semcheck_count_required_params(params);
                                    
                                    /* Check if this overload accepts args_for_call arguments */
                                    if (args_for_call >= required_params && args_for_call <= total_params) {
                                        /* Prefer exact match over range match */
                                        if (best_match == NULL) {
                                            best_match = candidate;
                                        } else {
                                            /* Pick the one with fewer total params (more specific) */
                                            ListNode_t *best_params = kgpc_type_get_procedure_params(best_match->type);
                                            int best_total = semcheck_count_total_params(best_params);
                                            if (total_params < best_total) {
                                                best_match = candidate;
                                            }
                                        }
                                    }
                                }
                                cur = cur->next;
                            }
                            
                            if (best_match != NULL) {
                                method_node = best_match;
                            }
                            
                            DestroyList(all_methods);
                        }
                    }

                    /* Transform record access into an explicit method call: receiver.Method() */
                    char *method_id = (field_id != NULL) ? strdup(field_id) : NULL;

                    expr->type = EXPR_FUNCTION_CALL;
                    memset(&expr->expr_data.function_call_data, 0,
                        sizeof(expr->expr_data.function_call_data));
                    expr->expr_data.function_call_data.is_method_call_placeholder = 1;
                    expr->expr_data.function_call_data.id = method_id;
                    if (field_id != NULL)
                        expr->expr_data.function_call_data.placeholder_method_name = strdup(field_id);
                    expr->expr_data.function_call_data.mangled_id = NULL;
                    expr->expr_data.function_call_data.resolved_func = NULL;

                    /* For static methods:
                     * - If receiver is a type identifier (TypeName.Method), pass it as first arg
                     *   so semcheck_funccall can detect and handle the type-qualified call
                     * - If receiver is an instance variable, no receiver needed for static method
                     */
                    if (is_static_method) {
                        if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL) {
                            /* Check if the receiver is a type name */
                            HashNode_t *type_node = NULL;
                            if (FindIdent(&type_node, symtab, record_expr->expr_data.id) != -1 &&
                                type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) {
                                /* It's a type-qualified static method call - pass type as first arg */
                                struct Expression *type_arg = record_expr;
                                ListNode_t *arg_node = CreateListNode(type_arg, LIST_EXPR);
                                expr->expr_data.function_call_data.args_expr = arg_node;
                            } else {
                                /* It's an instance variable calling a static method - no receiver needed */
                                expr->expr_data.function_call_data.args_expr = NULL;
                            }
                        } else {
                            expr->expr_data.function_call_data.args_expr = NULL;
                        }
                    } else {
                        struct Expression *receiver = record_expr;
                        ListNode_t *arg_node = CreateListNode(receiver, LIST_EXPR);
                        expr->expr_data.function_call_data.args_expr = arg_node;
                    }
                    /* Check if this is a constructor call (Create) on a class type.
                     * After semcheck_funccall resolves the inherited constructor,
                     * we need to override the return type to be the calling class,
                     * not the class where the constructor is declared. */
                    const char *method_name = NULL;
                    if (method_node->method_name != NULL)
                        method_name = method_node->method_name;
                    else if (field_id != NULL)
                        method_name = field_id;
                    int is_constructor_call = (!is_static_method &&
                        record_info != NULL &&
                        record_type_is_class(record_info) &&
                        !record_info->is_type_helper &&
                        method_name != NULL &&
                        strncasecmp(method_name, "Create", 6) == 0);
                    /* Re-run semantic checking as a function call */
                    semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
                    int funccall_result = semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);

                    /* For constructor calls, override the return type to be the
                     * calling class (e.g. TResolveReferenceVisitor.Create should
                     * return TResolveReferenceVisitor, not TObject). */
                    if (is_constructor_call)
                    {
                        expr->expr_data.function_call_data.is_constructor_call = 1;
                        KgpcType *record_kgpc = create_record_type(record_info);
                        if (record_kgpc != NULL)
                        {
                            KgpcType *ptr_type = create_pointer_type(record_kgpc);
                            if (ptr_type != NULL)
                            {
                                semcheck_expr_set_resolved_kgpc_type_shared(expr, ptr_type);
                                semcheck_expr_set_resolved_type(expr, POINTER_TYPE);
                                if (type_return != NULL)
                                    *type_return = POINTER_TYPE;
                                destroy_kgpc_type(ptr_type);
                            }
                            else
                            {
                                destroy_kgpc_type(record_kgpc);
                            }
                        }
                    }

                    return funccall_result;
                }
        }

        if (property_matched)
            return error_count;

        /* Special handling for default Create constructor */
        /* If the field is "Create" and this is a class type, treat it as a default constructor */
        if (record_type_is_class(record_info) && field_id != NULL && 
            pascal_identifier_equals(field_id, "Create"))
        {
            /* Transform this EXPR_RECORD_ACCESS into EXPR_FUNCTION_CALL */
            /* The record_expr could be either:
             * 1. A type name (EXPR_VAR_ID) like TMyClass.Create - use static VMT
             * 2. A class reference variable like ClassRef.Create - use variable's value as VMT
             */
            if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
            {
                const char *expr_name = record_expr->expr_data.id;
                
                /* Check if this is a type name or a variable name */
                HashNode_t *ident_node = NULL;
                int is_type = 0;
                if (FindIdent(&ident_node, symtab, (char*)expr_name) >= 0 && ident_node != NULL) {
                    is_type = (ident_node->hash_type == HASHTYPE_TYPE);
                }
                
                /* Clean up the old record_access_data before transforming */
                expr->expr_data.record_access_data.record_expr = NULL;
                
                /* Calculate class size using KgpcType */
                KgpcType *record_kgpc = create_record_type(record_info);
                if (record_kgpc == NULL) {
                    semcheck_error_with_context("Error on line %d: Unable to create KgpcType for class %s\n", 
                        expr->line_num, expr_name);
                    destroy_expr(record_expr);
                    return error_count + 1;
                }
                
                long long class_size = kgpc_type_sizeof(record_kgpc);
                if (class_size <= 0) {
                    semcheck_error_with_context("Error on line %d: Unable to determine size for class %s\n", 
                        expr->line_num, expr_name);
                    destroy_expr(record_expr);
                    return error_count + 1;
                }
                
                /* Create argument 1: class size as integer literal */
                struct Expression *size_arg = (struct Expression *)calloc(1, sizeof(struct Expression));
                size_arg->type = EXPR_INUM;
                size_arg->expr_data.i_num = class_size;
                semcheck_expr_set_resolved_type(size_arg, INT_TYPE);
                
                /* Create argument 2: VMT pointer */
                struct Expression *vmt_arg;
                if (is_type) {
                    /* Static class type: use address of global VMT label */
                    vmt_arg = (struct Expression *)calloc(1, sizeof(struct Expression));
                    vmt_arg->type = EXPR_VAR_ID;
                    char vmt_label[256];
                    snprintf(vmt_label, sizeof(vmt_label), "%s_VMT", expr_name);
                    vmt_arg->expr_data.id = strdup(vmt_label);
                    semcheck_expr_set_resolved_type(vmt_arg, POINTER_TYPE);
                } else {
                    /* Class reference variable: use the variable's value as VMT */
                    /* The variable already holds a pointer to the VMT */
                    vmt_arg = record_expr;  /* Reuse the record_expr directly */
                    record_expr = NULL;     /* Prevent double-free */
                }
                
                /* Create argument list */
                ListNode_t *arg1_node = CreateListNode(size_arg, LIST_EXPR);
                ListNode_t *arg2_node = CreateListNode(vmt_arg, LIST_EXPR);
                arg1_node->next = arg2_node;
                
                /* Transform the expression into a function call to __kgpc_default_create */
                expr->type = EXPR_FUNCTION_CALL;
                /* Initialize function_call_data - use memset to clear the union */
                memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
                expr->expr_data.function_call_data.id = strdup("__kgpc_default_create");
                expr->expr_data.function_call_data.mangled_id = strdup("__kgpc_default_create");
                expr->expr_data.function_call_data.args_expr = arg1_node;
                
                /* Set the return type information */
                semcheck_expr_set_resolved_type(expr, POINTER_TYPE);
                
                /* Create a KgpcType for the class (pointer to record) */
                KgpcType *class_kgpc = create_pointer_type(record_kgpc);
                semcheck_expr_set_resolved_kgpc_type_shared(expr, class_kgpc);
                
                *type_return = POINTER_TYPE;
                
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_recordaccess: Transformed '%s.%s' to __kgpc_default_create(%lld, %s) call\n",
                        expr_name, field_id, class_size, is_type ? "(static VMT)" : "(runtime VMT)");
                }
                
                /* Free the record_expr only if we didn't reuse it for vmt_arg */
                if (record_expr != NULL)
                    destroy_expr(record_expr);
                
                return error_count;
            }
        }

        /* Check for methods on non-class records (advanced records) */
        /* This handles {$modeswitch advancedrecords} style record methods */
        /* Unlike classes, advanced records don't use VMT but still have methods
         * registered with mangled names (TypeName__MethodName) in the symbol table */
        if (record_info != NULL && !record_type_is_class(record_info))
        {
            HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, field_id, NULL);
            if (method_node != NULL)
            {
                /* Found a method on an advanced record */
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_recordaccess: Found advanced record method %s\n", field_id);
                }

                if (method_node->hash_type == HASHTYPE_FUNCTION ||
                    method_node->hash_type == HASHTYPE_PROCEDURE)
                {
                    /* Check if this is a static method (no Self parameter) */
                    const char *type_name = record_info->type_id;
                    int is_static_method = 0;
                    if (type_name != NULL && field_id != NULL) {
                        is_static_method = from_cparser_is_method_static(type_name, field_id);
                    }
                    
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck] semcheck_recordaccess: type=%s method=%s is_static=%d\n",
                            type_name ? type_name : "<null>", field_id, is_static_method);
                    }
                    
                    /* Transform record access into an explicit method call.
                     * Use the base mangled name (Type__Method) rather than a
                     * specific overload's mangled_id so that semcheck_funccall
                     * can perform proper overload resolution. */
                    char *base_mangled = NULL;
                    if (type_name != NULL && field_id != NULL)
                    {
                        size_t bm_len = strlen(type_name) + 2 + strlen(field_id) + 1;
                        base_mangled = (char *)malloc(bm_len);
                        if (base_mangled != NULL)
                            snprintf(base_mangled, bm_len, "%s__%s", type_name, field_id);
                    }
                    char *method_id = (base_mangled != NULL) ? base_mangled :
                        ((field_id != NULL) ? strdup(field_id) : NULL);

                    expr->type = EXPR_FUNCTION_CALL;
                    memset(&expr->expr_data.function_call_data, 0,
                        sizeof(expr->expr_data.function_call_data));
                    expr->expr_data.function_call_data.is_method_call_placeholder = 0;
                    expr->expr_data.function_call_data.id = method_id;
                    if (method_id != NULL)
                        expr->expr_data.function_call_data.mangled_id = strdup(method_id);
                    /* Don't pre-bind resolved_func or call_kgpc_type — let
                     * semcheck_funccall handle overload resolution. */
                    expr->expr_data.function_call_data.is_call_info_valid = 0;

                    /* For static methods, don't pass a receiver/Self */
                    if (is_static_method) {
                        expr->expr_data.function_call_data.args_expr = NULL;
                    } else {
                        /* For instance methods, pass receiver as first argument (Self) */
                        struct Expression *receiver = record_expr;
                        ListNode_t *arg_node = CreateListNode(receiver, LIST_EXPR);
                        expr->expr_data.function_call_data.args_expr = arg_node;
                    }

                    /* Re-run semantic checking as a function call */
                    semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
                    return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                }
            }
        }

        /* Check for record helpers: If no method was found on the record itself,
         * look for a "record helper for <RecordType>" and check for methods there. */
        if (record_info != NULL && !record_type_is_class(record_info) &&
            record_info->type_id != NULL && !record_info->is_type_helper)
        {
            struct RecordType *helper_record = semcheck_lookup_type_helper(symtab,
                UNKNOWN_TYPE, record_info->type_id);
            if (helper_record != NULL)
            {
                HashNode_t *method_node = semcheck_find_class_method(symtab,
                    helper_record, field_id, NULL);
                if (method_node != NULL)
                {
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck] semcheck_recordaccess: Found record helper method %s on %s\n",
                            field_id, helper_record->type_id);
                    }

                    if (method_node->hash_type == HASHTYPE_FUNCTION ||
                        method_node->hash_type == HASHTYPE_PROCEDURE)
                    {
                        const char *type_name = helper_record->type_id;
                        int is_static_method = 0;
                        if (type_name != NULL && field_id != NULL) {
                            is_static_method = from_cparser_is_method_static(type_name, field_id);
                        }

                        char *method_id = (method_node->mangled_id != NULL) ?
                            strdup(method_node->mangled_id) :
                            ((field_id != NULL) ? strdup(field_id) : NULL);

                        expr->type = EXPR_FUNCTION_CALL;
                        memset(&expr->expr_data.function_call_data, 0,
                            sizeof(expr->expr_data.function_call_data));
                        expr->expr_data.function_call_data.is_method_call_placeholder = 0;
                        expr->expr_data.function_call_data.id = method_id;
                        if (method_node->mangled_id != NULL)
                            expr->expr_data.function_call_data.mangled_id =
                                strdup(method_node->mangled_id);
                        else if (method_id != NULL)
                            expr->expr_data.function_call_data.mangled_id = strdup(method_id);
                        expr->expr_data.function_call_data.resolved_func = method_node;
                        expr->expr_data.function_call_data.call_hash_type = method_node->hash_type;
                        semcheck_expr_set_call_kgpc_type(expr, method_node->type, 0);
                        expr->expr_data.function_call_data.is_call_info_valid = 1;

                        if (is_static_method) {
                            expr->expr_data.function_call_data.args_expr = NULL;
                        } else {
                            struct Expression *receiver = record_expr;
                            ListNode_t *arg_node = CreateListNode(receiver, LIST_EXPR);
                            expr->expr_data.function_call_data.args_expr = arg_node;
                        }

                        semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
                        return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                    }
                }
            }
        }

        if (record_info != NULL && record_info->is_type_helper && record_info->type_id != NULL)
        {
            char *mangled_const = semcheck_mangle_helper_const_id(record_info->type_id, field_id);
            HashNode_t *const_node = NULL;
            if (mangled_const != NULL &&
                FindIdent(&const_node, symtab, mangled_const) == 0 &&
                const_node != NULL && const_node->hash_type == HASHTYPE_CONST)
            {
                destroy_expr(record_expr);
                expr->expr_data.record_access_data.record_expr = NULL;
                if (expr->expr_data.record_access_data.field_id != NULL)
                {
                    free(expr->expr_data.record_access_data.field_id);
                    expr->expr_data.record_access_data.field_id = NULL;
                }
                expr->type = EXPR_VAR_ID;
                expr->expr_data.id = mangled_const;
                return semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
            }
            if (mangled_const != NULL)
                free(mangled_const);
        }

        /* Type helper string builtins: In type helpers for strings,
         * Self.Length should resolve to Length(Self) */
        if (record_info != NULL && record_info->is_type_helper &&
            record_info->helper_base_type_id != NULL &&
            (pascal_identifier_equals(record_info->helper_base_type_id, "AnsiString") ||
             pascal_identifier_equals(record_info->helper_base_type_id, "String") ||
             pascal_identifier_equals(record_info->helper_base_type_id, "ShortString") ||
             pascal_identifier_equals(record_info->helper_base_type_id, "UnicodeString")))
        {
            if (pascal_identifier_equals(field_id, "Length"))
            {
                /* Transform 'Self.Length' into 'Length(Self)' */
                char *func_id = strdup("Length");
                if (func_id != NULL)
                {
                    /* record_expr is already the Self expression */
                    ListNode_t *args_list = CreateListNode(record_expr, LIST_EXPR);
                    if (args_list != NULL)
                    {
                        /* Clear field_id and convert to function call */
                        if (expr->expr_data.record_access_data.field_id != NULL)
                        {
                            free(expr->expr_data.record_access_data.field_id);
                        }
                        
                        expr->type = EXPR_FUNCTION_CALL;
                        memset(&expr->expr_data.function_call_data, 0,
                            sizeof(expr->expr_data.function_call_data));
                        expr->expr_data.function_call_data.id = func_id;
                        expr->expr_data.function_call_data.args_expr = args_list;
                        expr->expr_data.function_call_data.mangled_id = NULL;
                        semcheck_reset_function_call_cache(expr);
                        return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                    }
                    free(func_id);
                }
            }
        }

        /* Check record_properties for plain records (Delphi advanced records) */
        if (record_info != NULL && record_info->record_properties != NULL)
        {
            struct ClassProperty *property = NULL;
            /* Search record_properties list */
            ListNode_t *pnode = record_info->record_properties;
            while (pnode != NULL)
            {
                if (pnode->type == LIST_CLASS_PROPERTY && pnode->cur != NULL)
                {
                    struct ClassProperty *p = (struct ClassProperty *)pnode->cur;
                    if (p->name != NULL && pascal_identifier_equals(p->name, field_id))
                    {
                        property = p;
                        break;
                    }
                }
                pnode = pnode->next;
            }
            if (property != NULL)
            {
                const char *accessor = (mutating != NO_MUTATE) ? property->write_accessor : property->read_accessor;
                if (accessor != NULL)
                {
                    /* Resolve property to its backing field */
                    if (resolve_record_field(symtab, record_info, accessor,
                            &field_desc, &field_offset, expr->line_num, 1) == 0 && field_desc != NULL)
                    {
                        if (!pascal_identifier_equals(field_id, accessor))
                        {
                            free(expr->expr_data.record_access_data.field_id);
                            expr->expr_data.record_access_data.field_id = strdup(accessor);
                        }
                        goto FIELD_RESOLVED;
                    }

                    if (mutating != NO_MUTATE)
                    {
                        /* For setter method calls on record properties, leave the expression
                         * as EXPR_RECORD_ACCESS so the assignment handler in
                         * semcheck_try_property_assignment() can construct the setter call
                         * with the value argument. Return 0 to suppress "field not found"
                         * and let the assignment handler take over. */
                        return 0;
                    }

                    HashNode_t *method_node = semcheck_find_class_method(symtab, record_info,
                        accessor, NULL);
                    if (method_node != NULL)
                    {
                        struct Expression *receiver = record_expr;
                        ListNode_t *arg_node = CreateListNode(receiver, LIST_EXPR);
                        if (arg_node != NULL)
                        {
                            char *method_id = strdup(accessor);
                            expr->type = EXPR_FUNCTION_CALL;
                            memset(&expr->expr_data.function_call_data, 0,
                                sizeof(expr->expr_data.function_call_data));
                            expr->expr_data.function_call_data.is_method_call_placeholder = 1;
                            expr->expr_data.function_call_data.placeholder_method_name = strdup(accessor);
                            expr->expr_data.function_call_data.id = method_id;
                            if (method_node->mangled_id != NULL)
                                expr->expr_data.function_call_data.mangled_id =
                                    strdup(method_node->mangled_id);
                            expr->expr_data.function_call_data.resolved_func = method_node;
                            expr->expr_data.function_call_data.args_expr = arg_node;
                            semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
                            return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                        }
                    }
                }
            }
        }

        /* For classes and object types, check if the field is a class/object const
         * stored with mangled name ClassName__ConstName. */
        if (record_info != NULL && record_info->type_id != NULL && field_id != NULL)
        {
            char *mangled_const = semcheck_mangle_helper_const_id(record_info->type_id, field_id);
            HashNode_t *const_node = NULL;
            if (mangled_const != NULL &&
                FindIdent(&const_node, symtab, mangled_const) >= 0 &&
                const_node != NULL &&
                (const_node->hash_type == HASHTYPE_CONST ||
                 const_node->hash_type == HASHTYPE_ARRAY ||
                 const_node->hash_type == HASHTYPE_VAR ||
                 const_node->is_typed_const))
            {
                destroy_expr(record_expr);
                expr->expr_data.record_access_data.record_expr = NULL;
                if (expr->expr_data.record_access_data.field_id != NULL)
                {
                    free(expr->expr_data.record_access_data.field_id);
                    expr->expr_data.record_access_data.field_id = NULL;
                }
                expr->type = EXPR_VAR_ID;
                expr->expr_data.id = mangled_const;
                return semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
            }
            if (mangled_const != NULL)
                free(mangled_const);
        }

        /* For classes, check if the field is a nested type (e.g., TClass.TNestedEnum).
         * Nested types are registered as "ClassName.NestedTypeName" in the symbol table. */
        if (record_info != NULL && record_info->type_id != NULL && field_id != NULL)
        {
            size_t owner_len = strlen(record_info->type_id);
            size_t field_len = strlen(field_id);
            char *qualified_name = (char *)malloc(owner_len + 1 + field_len + 1);
            if (qualified_name != NULL)
            {
                snprintf(qualified_name, owner_len + 1 + field_len + 1, "%s.%s",
                    record_info->type_id, field_id);
                HashNode_t *type_node = NULL;
                if (FindIdent(&type_node, symtab, qualified_name) >= 0 &&
                    type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                {
                    /* Found a nested type.  Transform the record access into
                     * a reference to this type so further accesses (e.g.
                     * TClass.TEnum.Value) can resolve correctly. */
                    struct TypeAlias *ta = hashnode_get_type_alias(type_node);
                    int nested_type_tag = UNKNOWN_TYPE;
                    if (ta != NULL && ta->is_enum)
                        nested_type_tag = ENUM_TYPE;
                    else if (type_node->type != NULL)
                        nested_type_tag = semcheck_tag_from_kgpc(type_node->type);

                    /* Transform to EXPR_VAR_ID pointing at the qualified type name
                     * so that outer record_access (e.g. .EnumValue) can resolve
                     * it via the existing scoped-enum handler. */
                    destroy_expr(record_expr);
                    expr->expr_data.record_access_data.record_expr = NULL;
                    if (expr->expr_data.record_access_data.field_id != NULL)
                    {
                        free(expr->expr_data.record_access_data.field_id);
                        expr->expr_data.record_access_data.field_id = NULL;
                    }
                    expr->type = EXPR_VAR_ID;
                    expr->expr_data.id = qualified_name;
                    semcheck_expr_set_resolved_type(expr, nested_type_tag);
                    if (type_node->type != NULL)
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, type_node->type);
                    *type_return = nested_type_tag;
                    return 0;
                }
                free(qualified_name);
            }
        }

        if (record_info != NULL && record_info->type_id != NULL)
            semcheck_error_with_context("Error on line %d, record field %s not found on type '%s'.\n",
                expr->line_num, field_id, record_info->type_id);
        else
            semcheck_error_with_context("Error on line %d, record field %s not found.\n",
                expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

FIELD_RESOLVED:
    expr->expr_data.record_access_data.field_offset = field_offset;

    expr->expr_data.record_access_data.field_offset = field_offset;

    /* Temporarily set unit context to the record's defining unit so field
     * type lookups prefer same-unit types (e.g., system's pstring vs
     * objpas's PString). */
    int saved_unit_ctx_ra = 0;
    int has_unit_ctx_ra = (record_info != NULL && record_info->source_unit_index > 0);
    if (has_unit_ctx_ra)
    {
        saved_unit_ctx_ra = semcheck_save_unit_context();
        semcheck_restore_unit_context(record_info->source_unit_index);
    }

    int field_type = field_desc->type;
    struct RecordType *field_record = field_desc->nested_record;
    if (field_record != NULL)
        field_type = RECORD_TYPE;
    /* Handle inline pointer fields like bufptr: ^Char */
    if (field_desc->is_pointer)
        field_type = POINTER_TYPE;
    /* Procedural fields (function/procedure pointers) */
    if (field_desc->proc_type != NULL)
        field_type = PROCEDURE;
    if (getenv("KGPC_DEBUG_PROC_FIELD") != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_PROC_FIELD] field=%s type=%d raw_type=%d type_id=%s proc_type=%p\n",
            field_id != NULL ? field_id : "<null>",
            field_type,
            field_desc->type,
            field_desc->type_id != NULL ? field_desc->type_id : "<null>",
            (void *)field_desc->proc_type);
    }
    if (getenv("KGPC_DEBUG_POINTER_FIELD") != NULL && field_id != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_POINTER_FIELD] field=%s type=%d is_pointer=%d pointer_type=%d pointer_type_id=%s\n",
            field_id,
            field_desc->type,
            field_desc->is_pointer,
            field_desc->pointer_type,
            field_desc->pointer_type_id ? field_desc->pointer_type_id : "<null>");
    }
    if (getenv("KGPC_DEBUG_RECORD_FIELD") != NULL &&
        field_id != NULL &&
        (pascal_identifier_equals(field_id, "st_ctime") ||
         pascal_identifier_equals(field_id, "st_mtime") ||
         pascal_identifier_equals(field_id, "st_atime") ||
         pascal_identifier_equals(field_id, "VChar")))
    {
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_FIELD] field=%s type=%d type_id=%s record=%p\n",
            field_id,
            field_desc->type,
            field_desc->type_id ? field_desc->type_id : "<null>",
            (void *)field_desc->nested_record);
    }
    if (getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL && field_id != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_ACCESS] field=%s type=%d type_id=%s record=%p resolved=%s\n",
            field_id,
            field_desc->type,
            field_desc->type_id ? field_desc->type_id : "<null>",
            (void *)field_desc->nested_record,
            expr->resolved_kgpc_type ? kgpc_type_to_string(expr->resolved_kgpc_type) : "<null>");
    }

    if (field_desc->is_array)
    {
        semcheck_clear_array_info(expr);
        expr->is_array_expr = 1;
        expr->array_lower_bound = field_desc->array_start;
        expr->array_upper_bound = field_desc->array_end;
        expr->array_is_dynamic = field_desc->array_is_open;
        expr->array_element_type = field_desc->array_element_type;
        const TypeRef *array_element_ref = field_desc->array_element_type_ref;
        const char *array_element_id = field_desc->array_element_type_id;
        if (array_element_id != NULL || array_element_ref != NULL)
        {
            if (array_element_id != NULL)
                expr->array_element_type_id = strdup(array_element_id);
            if (array_element_ref != NULL)
                expr->array_element_type_ref = type_ref_clone(array_element_ref);
            if (expr->array_element_type == UNKNOWN_TYPE)
            {
                HashNode_t *elem_type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                    array_element_ref, array_element_id);
                if (elem_type_node == NULL)
                    elem_type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                        array_element_ref, array_element_id);
                if (elem_type_node != NULL)
                    set_type_from_hashtype(&expr->array_element_type, elem_type_node);
                if (expr->array_element_type == UNKNOWN_TYPE)
                {
                    const char *base_name = array_element_ref != NULL
                        ? type_ref_base_name(array_element_ref)
                        : array_element_id;
                    int builtin = semcheck_map_builtin_type_name(symtab, base_name);
                    if (builtin != UNKNOWN_TYPE)
                        expr->array_element_type = builtin;
                }
            }
            HashNode_t *elem_node = semcheck_find_preferred_type_node_with_ref(symtab,
                array_element_ref, array_element_id);
            if (elem_node == NULL)
                elem_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                    array_element_ref, array_element_id);
            if (elem_node != NULL)
                expr->array_element_record_type = get_record_type_from_node(elem_node);
            if (expr->array_element_record_type == NULL && elem_node != NULL)
            {
                struct TypeAlias *elem_alias = get_type_alias_from_node(elem_node);
                if (elem_alias != NULL)
                {
                    if (elem_alias->inline_record_type != NULL)
                        expr->array_element_record_type = elem_alias->inline_record_type;
                    else if (elem_alias->target_type_id != NULL || elem_alias->target_type_ref != NULL)
                    {
                        HashNode_t *target_node = semcheck_find_preferred_type_node_with_ref(
                            symtab, elem_alias->target_type_ref, elem_alias->target_type_id);
                        if (target_node == NULL)
                            target_node = semcheck_find_type_node_with_kgpc_type_ref(
                                symtab, elem_alias->target_type_ref, elem_alias->target_type_id);
                        if (target_node != NULL)
                        {
                            expr->array_element_record_type = get_record_type_from_node(target_node);
                            if (expr->array_element_record_type == NULL &&
                                target_node->type != NULL &&
                                kgpc_type_is_record(target_node->type))
                            {
                                expr->array_element_record_type =
                                    kgpc_type_get_record(target_node->type);
                            }
                        }
                    }
                }
            }
            if (expr->array_element_record_type == NULL && array_element_id != NULL)
                expr->array_element_record_type = semcheck_lookup_record_type(symtab,
                    array_element_id);
        }
        else if (expr->array_element_type == RECORD_TYPE)
        {
            expr->array_element_record_type = field_desc->array_element_record;
            if (expr->array_element_record_type == NULL)
                expr->array_element_record_type = field_record;
        }

        long long computed_size = 0;
        int size_status = 1;
        if (expr->array_element_record_type != NULL)
            size_status = sizeof_from_record(symtab, expr->array_element_record_type,
                &computed_size, 0, expr->line_num);
        else if (expr->array_element_type != UNKNOWN_TYPE ||
            expr->array_element_type_id != NULL ||
            expr->array_element_type_ref != NULL)
        {
            char *rendered_elem = NULL;
            const char *sizeof_id = expr->array_element_type_id;
            if (sizeof_id == NULL && expr->array_element_type_ref != NULL)
            {
                rendered_elem = type_ref_render_mangled(expr->array_element_type_ref);
                sizeof_id = rendered_elem;
            }
            size_status = sizeof_from_type_ref(symtab, expr->array_element_type,
                sizeof_id, &computed_size, 0, expr->line_num);
            if (rendered_elem != NULL)
                free(rendered_elem);
        }
        if (size_status == 0 && computed_size > 0 && computed_size <= INT_MAX)
            expr->array_element_size = (int)computed_size;

        if (expr->array_element_type != UNKNOWN_TYPE)
            field_type = expr->array_element_type;
        if (expr->array_element_record_type != NULL && field_type == RECORD_TYPE)
            field_record = expr->array_element_record_type;
        if (expr->resolved_kgpc_type == NULL)
        {
            KgpcType *elem_type = NULL;
            int elem_owned = 0;
            if (field_desc->array_element_kgpc_type != NULL)
            {
                elem_type = field_desc->array_element_kgpc_type;
                elem_owned = 0;
            }
            else if (expr->array_element_record_type != NULL)
            {
                elem_type = create_record_type(expr->array_element_record_type);
                elem_owned = 1;
            }
            else if (expr->array_element_type_ref != NULL || expr->array_element_type_id != NULL)
            {
                HashNode_t *elem_node = semcheck_find_type_node_with_kgpc_type_ref(
                    symtab, expr->array_element_type_ref, expr->array_element_type_id);
                if (elem_node != NULL && elem_node->type != NULL)
                    elem_type = elem_node->type;
            }
            if (elem_type == NULL && expr->array_element_type != UNKNOWN_TYPE)
            {
                elem_type = create_primitive_type(expr->array_element_type);
                elem_owned = 1;
            }
            if (elem_type != NULL)
            {
                KgpcType *arr_type = create_array_type(elem_type,
                    expr->array_lower_bound, expr->array_upper_bound);
                if (arr_type != NULL)
                {
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, arr_type);
                    destroy_kgpc_type(arr_type);
                }
                if (elem_owned)
                    destroy_kgpc_type(elem_type);
            }
        }
    }

    struct TypeAlias *array_alias = NULL;
    int field_is_array_type = field_desc->is_array ? 1 : 0;

    if (!field_desc->is_pointer && (field_desc->type_id != NULL || field_desc->type_ref != NULL))
    {
        int resolved_type = field_type;
        const TypeRef *field_ref = field_desc->type_ref;
        const char *type_id_to_use = field_desc->type_id;
        char qualified_name_buf[512];
        int qualified_name_allocated = 0;
        
        HashNode_t *type_node = semcheck_find_preferred_type_node_with_ref(symtab,
            field_ref, type_id_to_use);
        if (type_node == NULL)
            type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                field_ref, type_id_to_use);
        
        if (type_node == NULL && record_info != NULL && record_info->type_id != NULL)
        {
            snprintf(qualified_name_buf, sizeof(qualified_name_buf), "%s.%s", 
                     record_info->type_id, field_desc->type_id);
            type_node = semcheck_find_preferred_type_node(symtab, qualified_name_buf);
            if (type_node == NULL)
                type_node = semcheck_find_type_node_with_kgpc_type(symtab, qualified_name_buf);
            if (type_node != NULL)
            {
                type_id_to_use = qualified_name_buf;
                qualified_name_allocated = 1;
            }
        }
        
        if (resolve_type_identifier_ref(&resolved_type, symtab, type_id_to_use,
                field_ref, expr->line_num) != 0)
            ++error_count;
        field_type = resolved_type;
        
        if (qualified_name_allocated && field_desc->type_id != NULL)
        {
            char *field_type_copy = strdup(field_desc->type_id);
            free(field_desc->type_id);
            field_desc->type_id = strdup(qualified_name_buf);
            if (field_desc->type_ref != NULL)
            {
                type_ref_free(field_desc->type_ref);
                field_desc->type_ref = NULL;
            }
            char **segments = (char **)calloc(2, sizeof(char *));
            if (segments != NULL)
            {
                segments[0] = strdup(record_info->type_id);
                segments[1] = field_type_copy;
                if (segments[0] != NULL && segments[1] != NULL)
                {
                    QualifiedIdent *qid = qualified_ident_from_segments(segments, 2, 1);
                    if (qid != NULL)
                        field_desc->type_ref = type_ref_create(qid, NULL, 0);
                }
                else
                {
                    free(segments[0]);
                    free(field_type_copy);
                    free(segments);
                }
            }
            else
            {
                free(field_type_copy);
            }
        }

        if (type_node != NULL)
        {
            if (type_node->type != NULL && expr->resolved_kgpc_type == NULL)
                semcheck_expr_set_resolved_kgpc_type_shared(expr, type_node->type);
            if (type_node->type != NULL &&
                (kgpc_type_is_array(type_node->type) || kgpc_type_is_array_of_const(type_node->type)) &&
                !expr->is_array_expr)
            {
                semcheck_set_array_info_from_hashnode(expr, symtab, type_node, expr->line_num);
            }
            if (type_node->type != NULL &&
                (kgpc_type_is_array(type_node->type) || kgpc_type_is_array_of_const(type_node->type)))
            {
                field_is_array_type = 1;
            }

            struct RecordType *record_type = get_record_type_from_node(type_node);
            if (record_type != NULL)
                field_record = record_type;
            else
            {
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->target_type_id != NULL)
                {
                    HashNode_t *target_node =
                        semcheck_find_preferred_type_node_with_ref(symtab,
                            alias->target_type_ref, alias->target_type_id);
                    if (target_node != NULL)
                        field_record = get_record_type_from_node(target_node);
                }
            }

            struct TypeAlias *alias = get_type_alias_from_node(type_node);
            if (alias != NULL && alias->is_array)
            {
                array_alias = alias;
                field_is_array_type = 1;
            }
        }

        if (field_record == NULL && field_desc->type_id != NULL)
        {
            field_record = semcheck_lookup_record_type(symtab, field_desc->type_id);
        }

        if (field_record != NULL && field_type == UNKNOWN_TYPE)
            field_type = RECORD_TYPE;
        if (field_is_array_type && field_type == RECORD_TYPE)
            field_type = UNKNOWN_TYPE;
    }

    if (getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL && field_id != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_ACCESS] resolved field=%s type=%d type_id=%s record=%p\n",
            field_id,
            field_type,
            field_desc->type_id ? field_desc->type_id : "<null>",
            (void *)field_record);
    }

    if (field_record != NULL && field_type == RECORD_TYPE)
    {
        expr->record_type = field_record;
        if (expr->resolved_kgpc_type == NULL)
        {
            KgpcType *record_kgpc = create_record_type(field_record);
            if (record_kgpc != NULL)
            {
                semcheck_expr_set_resolved_kgpc_type_shared(expr, record_kgpc);
                destroy_kgpc_type(record_kgpc);
            }
        }
    }

    if (field_type == UNKNOWN_TYPE && field_record == NULL && array_alias == NULL && !field_is_array_type)
    {
        semcheck_error_with_context("Error on line %d, unable to resolve type for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        if (has_unit_ctx_ra)
            semcheck_restore_unit_context(saved_unit_ctx_ra);
        return error_count + 1;
    }

    if (field_type == RECORD_TYPE && field_record == NULL && !field_is_array_type)
    {
        semcheck_error_with_context("Error on line %d, missing record definition for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        if (has_unit_ctx_ra)
            semcheck_restore_unit_context(saved_unit_ctx_ra);
        return error_count + 1;
    }

    if (array_alias != NULL)
        semcheck_set_array_info_from_alias(expr, symtab, array_alias, expr->line_num);
    if (array_alias != NULL && expr->resolved_kgpc_type == NULL)
    {
        KgpcType *arr_type = create_kgpc_type_from_type_alias(array_alias, symtab, 0);
        if (arr_type != NULL)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, arr_type);
            destroy_kgpc_type(arr_type);
        }
    }

    if (expr->resolved_kgpc_type == NULL &&
        field_record == NULL &&
        !field_desc->is_array &&
        !field_desc->is_pointer &&
        field_desc->proc_type == NULL)
    {
        KgpcType *fallback_type = NULL;
        if (field_desc->type_id != NULL || field_desc->type_ref != NULL)
        {
            HashNode_t *type_node = semcheck_find_preferred_type_node_with_ref(
                symtab, field_desc->type_ref, field_desc->type_id);
            if (type_node != NULL && type_node->type != NULL)
                fallback_type = type_node->type;
        }
        if (fallback_type == NULL && field_type != UNKNOWN_TYPE)
        {
            if (field_type == REAL_TYPE)
                fallback_type = create_primitive_type_with_size(REAL_TYPE, 8);
            else
                fallback_type = create_primitive_type(field_type);
        }
        if (fallback_type != NULL)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, fallback_type);
            if (fallback_type->ref_count == 1 && fallback_type->type_alias == NULL)
                destroy_kgpc_type(fallback_type);
        }
    }

    if (expr->resolved_kgpc_type == NULL && field_desc->is_array)
    {
        KgpcType *elem_type = NULL;
        int elem_owned = 0;
        if (field_desc->array_element_kgpc_type != NULL)
        {
            /* Pre-built element type for nested arrays (array of array of ...) */
            elem_type = field_desc->array_element_kgpc_type;
            elem_owned = 0;  /* Borrowed from field descriptor */
        }
        else if (field_desc->array_element_record != NULL)
        {
            elem_type = create_record_type(field_desc->array_element_record);
            elem_owned = 1;
        }
        else if (field_desc->array_element_type_id != NULL)
        {
            HashNode_t *elem_node = semcheck_find_type_node_with_kgpc_type_ref(
                symtab, field_desc->array_element_type_ref, field_desc->array_element_type_id);
            if (elem_node != NULL && elem_node->type != NULL)
                elem_type = elem_node->type;
        }
        if (elem_type == NULL && field_desc->array_element_type != UNKNOWN_TYPE)
        {
            elem_type = create_primitive_type(field_desc->array_element_type);
            elem_owned = 1;
        }
        if (elem_type == NULL && expr->array_element_type != UNKNOWN_TYPE)
        {
            elem_type = create_primitive_type(expr->array_element_type);
            elem_owned = 1;
        }
        if (elem_type == NULL && expr->array_element_record_type != NULL)
        {
            elem_type = create_record_type(expr->array_element_record_type);
            elem_owned = 1;
        }
        if (elem_type == NULL && expr->array_element_type_id != NULL)
        {
            HashNode_t *elem_node = semcheck_find_type_node_with_kgpc_type_ref(
                symtab, expr->array_element_type_ref, expr->array_element_type_id);
            if (elem_node != NULL && elem_node->type != NULL)
                elem_type = elem_node->type;
        }
        if (elem_type != NULL)
        {
            if (!elem_owned)
                kgpc_type_retain(elem_type);
            KgpcType *arr_type = create_array_type(elem_type,
                field_desc->array_start, field_desc->array_end);
            if (arr_type != NULL)
                semcheck_expr_set_resolved_kgpc_type_shared(expr, arr_type);
            if (arr_type != NULL)
                destroy_kgpc_type(arr_type);
            /* Note: elem_type ownership was transferred to create_array_type, do NOT destroy it here */
        }
    }

    if (field_type == POINTER_TYPE &&
        !field_desc->is_pointer &&
        field_desc->pointer_type_id == NULL &&
        field_desc->pointer_type == UNKNOWN_TYPE)
    {
        HashNode_t *ptr_node = NULL;
        if (field_desc->type_id != NULL || field_desc->type_ref != NULL)
        {
            ptr_node = semcheck_find_preferred_type_node_with_ref(symtab,
                field_desc->type_ref, field_desc->type_id);
            if (ptr_node == NULL)
                ptr_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                    field_desc->type_ref, field_desc->type_id);
        }
        if (ptr_node != NULL && ptr_node->type != NULL)
        {
            KgpcType *ptr_type = ptr_node->type;
            int is_untyped_pointer = 0;
            if (ptr_type->kind == TYPE_KIND_PRIMITIVE &&
                ptr_type->info.primitive_type_tag == POINTER_TYPE)
            {
                is_untyped_pointer = 1;
            }
            else if (ptr_type->kind == TYPE_KIND_POINTER)
            {
                KgpcType *pointee = ptr_type->info.points_to;
                if (pointee != NULL &&
                    pointee->kind == TYPE_KIND_PRIMITIVE &&
                    pointee->info.primitive_type_tag == POINTER_TYPE)
                {
                    struct TypeAlias *ptr_alias = get_type_alias_from_node(ptr_node);
                    if (ptr_alias == NULL || !ptr_alias->is_pointer)
                        is_untyped_pointer = 1;
                }
            }
            if (is_untyped_pointer)
            {
                semcheck_set_pointer_info(expr, UNKNOWN_TYPE, NULL);
                if (expr->resolved_kgpc_type != NULL)
                {
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                    expr->resolved_kgpc_type = NULL;
                }
                expr->resolved_kgpc_type = create_primitive_type(POINTER_TYPE);
                *type_return = POINTER_TYPE;
                if (has_unit_ctx_ra)
                    semcheck_restore_unit_context(saved_unit_ctx_ra);
                return error_count;
            }
        }
    }

    if (field_type == POINTER_TYPE)
    {
        int pointer_subtype = UNKNOWN_TYPE;
        const char *pointer_subtype_id = NULL;
        char *pointer_subtype_rendered = NULL;
        int typed_pointer = 0;
        int untyped_pointer_type = 0;
        int force_untyped = 0;
        HashNode_t *type_node = NULL;
        struct TypeAlias *alias = NULL;

        if (field_desc->is_pointer)
            typed_pointer = 1;
        if (field_desc->pointer_type_id != NULL || field_desc->pointer_type != UNKNOWN_TYPE ||
            field_desc->pointer_type_ref != NULL)
            typed_pointer = 1;

        if (!typed_pointer && (field_desc->type_id != NULL || field_desc->type_ref != NULL))
        {
            type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                field_desc->type_ref, field_desc->type_id);
            if (type_node == NULL)
                type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                    field_desc->type_ref, field_desc->type_id);
            if (type_node != NULL)
            {
                alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->is_pointer)
                    typed_pointer = 1;
                if (type_node->type != NULL)
                {
                    if (type_node->type->kind == TYPE_KIND_PRIMITIVE &&
                        type_node->type->info.primitive_type_tag == POINTER_TYPE)
                    {
                        untyped_pointer_type = 1;
                    }
                    else if (type_node->type->kind == TYPE_KIND_POINTER)
                    {
                        KgpcType *points_to = type_node->type->info.points_to;
                        if (points_to != NULL &&
                            points_to->kind == TYPE_KIND_PRIMITIVE &&
                            points_to->info.primitive_type_tag == POINTER_TYPE)
                        {
                            untyped_pointer_type = 1;
                        }
                        else
                        {
                            typed_pointer = 1;
                        }
                    }
                }
                if (untyped_pointer_type && (alias == NULL || !alias->is_pointer))
                    typed_pointer = 0;
            }
        }

        if (!typed_pointer)
        {
            semcheck_set_pointer_info(expr, UNKNOWN_TYPE, NULL);
            if (expr->resolved_kgpc_type == NULL ||
                !(expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                  expr->resolved_kgpc_type->info.primitive_type_tag == POINTER_TYPE))
            {
                if (expr->resolved_kgpc_type != NULL)
                {
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                    expr->resolved_kgpc_type = NULL;
                }
                expr->resolved_kgpc_type = create_primitive_type(POINTER_TYPE);
            }
        }
        else
        {
            if (field_desc->pointer_type_id != NULL || field_desc->pointer_type != UNKNOWN_TYPE)
            {
                if (field_desc->pointer_type != UNKNOWN_TYPE)
                    pointer_subtype = field_desc->pointer_type;
                if (field_desc->pointer_type_id != NULL)
                    pointer_subtype_id = field_desc->pointer_type_id;
                else if (field_desc->pointer_type_ref != NULL)
                {
                    pointer_subtype_rendered = type_ref_render_mangled(field_desc->pointer_type_ref);
                    pointer_subtype_id = pointer_subtype_rendered;
                }
            }
            else if (field_desc->type_id != NULL || field_desc->type_ref != NULL)
            {
                if (type_node == NULL)
                {
                    type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                        field_desc->type_ref, field_desc->type_id);
                    if (type_node == NULL)
                        type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                            field_desc->type_ref, field_desc->type_id);
                }
                if (type_node != NULL)
                {
                    struct TypeAlias *alias_local = get_type_alias_from_node(type_node);
                    if (alias_local != NULL && alias_local->is_pointer)
                    {
                        pointer_subtype = alias_local->pointer_type;
                        if (alias_local->pointer_type_ref != NULL)
                        {
                            pointer_subtype_rendered = type_ref_render_mangled(alias_local->pointer_type_ref);
                            pointer_subtype_id = pointer_subtype_rendered;
                        }
                        else
                            pointer_subtype_id = alias_local->pointer_type_id;
                    }
                    if (pointer_subtype == UNKNOWN_TYPE && type_node->type != NULL)
                        pointer_subtype = kgpc_type_get_pointer_subtype_tag(type_node->type);
                }
            }
            if (pointer_subtype_id == NULL)
            {
                if (field_desc->type_id != NULL)
                {
                    if (!untyped_pointer_type)
                        pointer_subtype_id = field_desc->type_id;
                }
                else if (field_desc->type_ref != NULL)
                    pointer_subtype_id = type_ref_base_name(field_desc->type_ref);
            }
            if (!force_untyped && pointer_subtype_id != NULL)
            {
                HashNode_t *sub_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                    field_desc->pointer_type_ref, pointer_subtype_id);
                if (sub_node != NULL && sub_node->type != NULL)
                {
                    if (sub_node->type->kind == TYPE_KIND_PRIMITIVE &&
                        sub_node->type->info.primitive_type_tag == POINTER_TYPE)
                    {
                        force_untyped = 1;
                    }
                    else if (sub_node->type->kind == TYPE_KIND_POINTER)
                    {
                        KgpcType *points_to = sub_node->type->info.points_to;
                        if (points_to != NULL &&
                            points_to->kind == TYPE_KIND_PRIMITIVE &&
                            points_to->info.primitive_type_tag == POINTER_TYPE)
                        {
                            struct TypeAlias *sub_alias = get_type_alias_from_node(sub_node);
                            if (sub_alias == NULL || !sub_alias->is_pointer)
                                force_untyped = 1;
                        }
                    }
                }
            }
            if (pointer_subtype == POINTER_TYPE &&
                field_desc->is_pointer == 0 &&
                field_desc->pointer_type_id == NULL &&
                field_desc->pointer_type == UNKNOWN_TYPE &&
                (alias == NULL || !alias->is_pointer))
            {
                force_untyped = 1;
            }

            if (force_untyped)
            {
                semcheck_set_pointer_info(expr, UNKNOWN_TYPE, NULL);
                if (pointer_subtype_rendered != NULL)
                    free(pointer_subtype_rendered);
                if (expr->resolved_kgpc_type == NULL ||
                    !(expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                      expr->resolved_kgpc_type->info.primitive_type_tag == POINTER_TYPE))
                {
                    if (expr->resolved_kgpc_type != NULL)
                    {
                        destroy_kgpc_type(expr->resolved_kgpc_type);
                        expr->resolved_kgpc_type = NULL;
                    }
                    expr->resolved_kgpc_type = create_primitive_type(POINTER_TYPE);
                }
            }
            else
            {
                semcheck_set_pointer_info(expr, pointer_subtype, pointer_subtype_id);
                if (pointer_subtype_rendered != NULL)
                    free(pointer_subtype_rendered);
                if (expr->resolved_kgpc_type != NULL &&
                    expr->resolved_kgpc_type->kind != TYPE_KIND_POINTER)
                {
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                    expr->resolved_kgpc_type = NULL;
                }
                if (expr->resolved_kgpc_type == NULL)
                {
                    KgpcType *points_to = NULL;
                    if (pointer_subtype_id != NULL)
                    {
                        HashNode_t *target_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                            field_desc->pointer_type_ref, pointer_subtype_id);
                        if (target_node != NULL && target_node->type != NULL)
                        {
                            points_to = target_node->type;
                            kgpc_type_retain(points_to);
                        }
                    }
                    if (points_to == NULL && pointer_subtype != UNKNOWN_TYPE)
                        points_to = create_primitive_type(pointer_subtype);
                    KgpcType *ptr_type = create_pointer_type(points_to);
                    if (ptr_type != NULL)
                    {
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, ptr_type);
                        destroy_kgpc_type(ptr_type);
                    }
                }

                struct RecordType *pointer_record = NULL;
                if (expr->resolved_kgpc_type != NULL &&
                    expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER)
                {
                    KgpcType *points_to = expr->resolved_kgpc_type->info.points_to;
                    if (points_to != NULL && points_to->kind == TYPE_KIND_RECORD)
                        pointer_record = points_to->info.record_info;
                }
                if (pointer_record == NULL && pointer_subtype_id != NULL)
                    pointer_record = semcheck_lookup_record_type(symtab, pointer_subtype_id);
                (void)pointer_record;
            }
        }
    }
    if (getenv("KGPC_DEBUG_BUFPTR") != NULL &&
        field_id != NULL && pascal_identifier_equals(field_id, "bufptr"))
    {
        fprintf(stderr, "[KGPC_DEBUG_BUFPTR] field=%s type=%d resolved=%s kind=%d\n",
            field_id,
            field_type,
            expr->resolved_kgpc_type ? kgpc_type_to_string(expr->resolved_kgpc_type) : "<null>",
            expr->resolved_kgpc_type ? expr->resolved_kgpc_type->kind : -1);
    }

    /* For procedural type fields (function/procedure pointers), set the full KgpcType
     * so type compatibility can check return types and parameters. */
    if (field_type == PROCEDURE && (field_desc->type_id != NULL || field_desc->type_ref != NULL))
    {
        HashNode_t *proc_type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
            field_desc->type_ref, field_desc->type_id);
        if (proc_type_node != NULL && proc_type_node->type != NULL &&
            proc_type_node->type->kind == TYPE_KIND_PROCEDURE)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, proc_type_node->type);
        }
    }
    else if (field_type == PROCEDURE && field_desc->proc_type != NULL)
    {
        semcheck_expr_set_resolved_kgpc_type_shared(expr, field_desc->proc_type);
    }

    if (field_type == REAL_TYPE)
    {
        int desired_real_size = 0;
        HashNode_t *real_node = NULL;
        if (field_desc->type_id != NULL || field_desc->type_ref != NULL)
        {
            real_node = semcheck_find_preferred_type_node_with_ref(symtab,
                field_desc->type_ref, field_desc->type_id);
            if (real_node == NULL)
                real_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                    field_desc->type_ref, field_desc->type_id);
            if (real_node != NULL && real_node->type != NULL)
            {
                long long size = kgpc_type_sizeof(real_node->type);
                if (size == 4 || size == 8)
                    desired_real_size = (int)size;
            }
        }
        if (desired_real_size == 0)
            desired_real_size = 8;

        int existing_real_size = 0;
        if (expr->resolved_kgpc_type != NULL &&
            expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
            expr->resolved_kgpc_type->info.primitive_type_tag == REAL_TYPE)
        {
            long long size = kgpc_type_sizeof(expr->resolved_kgpc_type);
            if (size > 0)
                existing_real_size = (int)size;
        }

        if (expr->resolved_kgpc_type == NULL ||
            (existing_real_size != 0 && existing_real_size != desired_real_size) ||
            (existing_real_size == 0))
        {
            if (real_node != NULL && real_node->type != NULL &&
                real_node->type->kind == TYPE_KIND_PRIMITIVE &&
                real_node->type->info.primitive_type_tag == REAL_TYPE)
            {
                semcheck_expr_set_resolved_kgpc_type_shared(expr, real_node->type);
            }
            else
            {
                KgpcType *real_type = create_primitive_type_with_size(REAL_TYPE, desired_real_size);
                if (real_type != NULL)
                {
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, real_type);
                    destroy_kgpc_type(real_type);
                }
            }
        }
    }
    else if (expr->resolved_kgpc_type == NULL &&
        field_type != UNKNOWN_TYPE &&
        field_type != RECORD_TYPE &&
        field_type != PROCEDURE)
    {
        KgpcType *prim_type = create_primitive_type(field_type);
        if (prim_type != NULL)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, prim_type);
            destroy_kgpc_type(prim_type);
        }
    }

    if (field_type == RECORD_TYPE && expr->resolved_kgpc_type == NULL && field_record != NULL)
    {
        KgpcType *record_kgpc = create_record_type(field_record);
        if (record_kgpc != NULL)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, record_kgpc);
            destroy_kgpc_type(record_kgpc);
        }
    }
    int preserve_resolved_kgpc = 0;
    if (expr->resolved_kgpc_type != NULL)
    {
        if (kgpc_type_is_array(expr->resolved_kgpc_type) ||
            kgpc_type_is_array_of_const(expr->resolved_kgpc_type) ||
            kgpc_type_is_procedure(expr->resolved_kgpc_type))
        {
            /* Preserve array/procedure KgpcType for overload resolution and var/open-array behavior. */
            preserve_resolved_kgpc = 1;
        }
        else if (kgpc_type_equals_tag(expr->resolved_kgpc_type, field_type))
        {
            /* Keep richer alias metadata (e.g. Single vs generic Real) when the type tag already matches. */
            preserve_resolved_kgpc = 1;
        }
    }

    if (!preserve_resolved_kgpc)
        semcheck_expr_set_resolved_type(expr, field_type);
    *type_return = field_type;
    if (getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL && field_id != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_ACCESS] final field=%s type=%d resolved=%s\n",
            field_id,
            field_type,
            expr->resolved_kgpc_type ? kgpc_type_to_string(expr->resolved_kgpc_type) : "<null>");
    }
    if (getenv("KGPC_DEBUG_RECORD_FIELD") != NULL &&
        field_id != NULL &&
        (pascal_identifier_equals(field_id, "st_ctime") ||
         pascal_identifier_equals(field_id, "st_mtime") ||
         pascal_identifier_equals(field_id, "st_atime")))
    {
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_FIELD] field=%s resolved_type=%d\n",
            field_id, field_type);
    }
    /* Restore unit context */
    if (has_unit_ctx_ra)
        semcheck_restore_unit_context(saved_unit_ctx_ra);
    return error_count;
}

int semcheck_try_reinterpret_as_typecast(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    const char *id = expr->expr_data.function_call_data.id;
    if (id == NULL)
        return 0;
    if (pascal_identifier_equals(id, "Create"))
        return 0;
    if (getenv("KGPC_DEBUG_TYPECAST") != NULL &&
        pascal_identifier_equals(id, "TextRec"))
    {
        fprintf(stderr, "[SemCheck] try_typecast TextRec at line %d\n", expr->line_num);
    }

    /* Only reinterpret as a typecast when there is exactly one argument. */
    int arg_count = 0;
    for (ListNode_t *cur = expr->expr_data.function_call_data.args_expr;
         cur != NULL; cur = cur->next)
    {
        arg_count++;
        if (arg_count > 1)
            return 0;
    }

    /* If a method with this name exists on Self, don't reinterpret as a typecast. */
    HashNode_t *self_node = NULL;
    if (FindIdent(&self_node, symtab, "Self") == 0 && self_node != NULL)
    {
        struct RecordType *self_record = get_record_type_from_node(self_node);
        if (self_record != NULL)
        {
            if (semcheck_find_class_method(symtab, self_record, id, NULL) != NULL)
                return 0;
        }
    }
    char *id_copy = strdup(id);
    if (id_copy == NULL)
        return 0;

    /* Only proceed if the callee resolves to a type identifier or a known builtin type */
    HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type(symtab, id);
    int target_type = UNKNOWN_TYPE;
    if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        set_type_from_hashtype(&target_type, type_node);
    if (target_type == UNKNOWN_TYPE && type_node != NULL &&
        type_node->hash_type == HASHTYPE_TYPE &&
        type_node->type != NULL && kgpc_type_is_record(type_node->type))
    {
        target_type = RECORD_TYPE;
    }
    if (type_node == NULL)
        FindIdent(&type_node, symtab, id);
    if (target_type == UNKNOWN_TYPE && type_node != NULL &&
        type_node->hash_type == HASHTYPE_TYPE &&
        type_node->type != NULL && kgpc_type_is_record(type_node->type))
    {
        target_type = RECORD_TYPE;
    }
    if (target_type == UNKNOWN_TYPE)
        target_type = semcheck_map_builtin_type_name(symtab, id);

    if (type_node == NULL)
    {
        const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
        const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
        if (owner_full == NULL)
            owner_full = semcheck_get_current_method_owner();
        type_node = semcheck_find_type_node_in_owner_chain(symtab, id, owner_full, owner_outer);
        if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            set_type_from_hashtype(&target_type, type_node);
            if (target_type == UNKNOWN_TYPE &&
                type_node->type != NULL && kgpc_type_is_record(type_node->type))
            {
                target_type = RECORD_TYPE;
            }
        }
    }

    int is_unaligned_cast = pascal_identifier_equals(id, "unaligned");
    int is_type_identifier =
        (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) ||
        (target_type != UNKNOWN_TYPE) ||
        is_unaligned_cast;
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
    {
        fprintf(stderr, "[SemCheck] try_typecast id=%s type_node=%p hash_type=%d target_type=%d\n",
            id, (void *)type_node, type_node != NULL ? type_node->hash_type : -1, target_type);
    }
    if (!is_type_identifier)
    {
        if (getenv("KGPC_DEBUG_TYPECAST") != NULL &&
            pascal_identifier_equals(id, "TextRec"))
        {
            fprintf(stderr, "[SemCheck] try_typecast TextRec: not a type identifier (node=%p hash=%d target_type=%d)\n",
                (void *)type_node, type_node != NULL ? type_node->hash_type : -1, target_type);
        }
        free(id_copy);
        return 0;
    }

    /* Require exactly one argument for a typecast */
    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
    {
        fprintf(stderr, "[SemCheck] try_typecast id=%s args=%d\n",
            id, args != NULL ? ListLength(args) : 0);
    }
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, typecast to %s expects exactly one argument.\n",
            expr->line_num, id);
        *type_return = UNKNOWN_TYPE;
        free(id_copy);
        return 1;
    }

    struct Expression *inner_expr = (struct Expression *)args->cur;

    /* Clean up function-call-specific fields without freeing the inner expression */
    if (expr->expr_data.function_call_data.id != NULL)
    {
        free(expr->expr_data.function_call_data.id);
        expr->expr_data.function_call_data.id = NULL;
    }
    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    if (expr->expr_data.function_call_data.call_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->expr_data.function_call_data.call_kgpc_type);
        expr->expr_data.function_call_data.call_kgpc_type = NULL;
    }

    /* Manually free the argument list nodes but keep the expression alive */
    ListNode_t *to_free = args;
    while (to_free != NULL)
    {
        ListNode_t *next = to_free->next;
        to_free->cur = NULL;
        free(to_free);
        to_free = next;
    }
    expr->expr_data.function_call_data.args_expr = NULL;

    /* Reinterpret as a typecast expression */
    expr->type = EXPR_TYPECAST;
    expr->expr_data.typecast_data.target_type = target_type;
    expr->expr_data.typecast_data.target_type_id = id_copy;
    expr->expr_data.typecast_data.expr = inner_expr;

    return semcheck_typecast(type_return, symtab, expr, max_scope_lev, NO_MUTATE);
}

int semcheck_reinterpret_typecast_as_call(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    if (expr == NULL || expr->expr_data.typecast_data.target_type_id == NULL)
        return 1;

    HashNode_t *func_node = NULL;
    int found_func = (FindIdent(&func_node, symtab, expr->expr_data.typecast_data.target_type_id) >= 0 &&
        func_node != NULL);

    /* For dotted identifiers like "widestringmanager.UpperProc", if the full name
     * is not found as a function, check if the prefix is a variable/record - the
     * function call handler will split and resolve it as a record field call. */
    if (!found_func)
    {
        const char *prefix = expr->expr_data.typecast_data.type_qualifier;
        if (prefix != NULL)
        {
            HashNode_t *prefix_node = NULL;
            int prefix_found = (FindIdent(&prefix_node, symtab, prefix) >= 0 && prefix_node != NULL);
            if (prefix_found)
                found_func = 1;  /* Let the function call handler resolve the dotted name */
        }
    }

    if (!found_func)
        return 1;

    if (func_node != NULL &&
        func_node->hash_type != HASHTYPE_FUNCTION &&
        func_node->hash_type != HASHTYPE_PROCEDURE &&
        func_node->hash_type != HASHTYPE_BUILTIN_PROCEDURE)
    {
        /* For dotted identifiers, the func_node may be a variable (record) -
         * that's OK, the function call handler will resolve the field call */
        if (expr->expr_data.typecast_data.type_qualifier == NULL)
            return 1;
    }

    struct Expression *arg_expr = expr->expr_data.typecast_data.expr;
    expr->expr_data.typecast_data.expr = NULL;

    char *call_id = expr->expr_data.typecast_data.target_type_id;
    expr->expr_data.typecast_data.target_type_id = NULL;
    char *call_qualifier = expr->expr_data.typecast_data.type_qualifier;
    expr->expr_data.typecast_data.type_qualifier = NULL;

    expr->type = EXPR_FUNCTION_CALL;
    memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
    expr->expr_data.function_call_data.id = call_id;
    expr->expr_data.function_call_data.call_qualifier = call_qualifier;
    if (arg_expr != NULL)
        expr->expr_data.function_call_data.args_expr = CreateListNode(arg_expr, LIST_EXPR);

    /* Keep legacy_tag here - typecast reinterpreted as call needs re-checking */
    return semcheck_expr_legacy_tag(type_return, symtab, expr, max_scope_lev, NO_MUTATE);
}


/* Try to find a bare method name in the current class for @MethodName patterns.
 * Returns the method HashNode_t if found, NULL otherwise. */
static HashNode_t *find_implicit_self_method(SymTab_t *symtab, const char *name)
{
    if (name == NULL)
        return NULL;
    const char *owner_id = semcheck_get_current_method_owner();
    if (owner_id == NULL)
        return NULL;
    HashNode_t *owner_node = NULL;
    if (FindIdent(&owner_node, symtab, owner_id) < 0 || owner_node == NULL)
        return NULL;
    struct RecordType *owner_rec = NULL;
    if (owner_node->type != NULL)
    {
        if (kgpc_type_is_record(owner_node->type))
            owner_rec = kgpc_type_get_record(owner_node->type);
        else if (kgpc_type_is_pointer(owner_node->type) &&
                 owner_node->type->info.points_to != NULL &&
                 kgpc_type_is_record(owner_node->type->info.points_to))
            owner_rec = kgpc_type_get_record(owner_node->type->info.points_to);
    }
    if (owner_rec == NULL)
        return NULL;
    HashNode_t *method_node = semcheck_find_class_method(symtab, owner_rec, name, NULL);
    if (method_node != NULL &&
        (method_node->hash_type == HASHTYPE_FUNCTION ||
         method_node->hash_type == HASHTYPE_PROCEDURE))
        return method_node;
    return NULL;
}

static int semcheck_is_ident_char_ascii(char c)
{
    return (c == '_') ||
        (c >= '0' && c <= '9') ||
        (c >= 'A' && c <= 'Z') ||
        (c >= 'a' && c <= 'z');
}

static char *semcheck_mangle_specialized_type_text(const char *type_text)
{
    if (type_text == NULL)
        return NULL;
    const char *lt = strchr(type_text, '<');
    if (lt == NULL)
        return strdup(type_text);

    const char *gt = strrchr(type_text, '>');
    if (gt == NULL || gt <= lt)
        return strdup(type_text);

    size_t base_len = (size_t)(lt - type_text);
    char *base = (char *)malloc(base_len + 1);
    if (base == NULL)
        return NULL;
    memcpy(base, type_text, base_len);
    base[base_len] = '\0';

    char args_buf[256];
    size_t args_len = 0;
    int depth = 0;
    for (const char *p = lt + 1; p < gt; ++p)
    {
        char ch = *p;
        if (ch == '<')
        {
            depth++;
            continue;
        }
        if (ch == '>')
        {
            if (depth > 0)
                depth--;
            continue;
        }
        if (depth == 0 && ch == ',')
            ch = '$';
        if (depth == 0 && ch == ' ')
            continue;
        if (args_len + 1 < sizeof(args_buf))
            args_buf[args_len++] = ch;
    }
    args_buf[args_len] = '\0';

    size_t out_len = strlen(base) + 1 + args_len;
    char *out = (char *)malloc(out_len + 1);
    if (out == NULL)
    {
        free(base);
        return NULL;
    }
    snprintf(out, out_len + 1, "%s$%s", base, args_buf);
    free(base);
    return out;
}

static size_t semcheck_find_specialize_span_start(const char *buffer, size_t length, size_t index)
{
    size_t end = index;
    while (end < length && buffer[end] != '\n' && buffer[end] != '\r' && buffer[end] != ';')
        end++;

    for (size_t i = index; i < end; ++i)
    {
        if (buffer[i] == '@')
            return i;
    }

    const char *kw = "specialize";
    size_t kw_len = strlen(kw);
    for (size_t i = index; i + kw_len <= end; ++i)
    {
        if (strncasecmp(buffer + i, kw, kw_len) == 0)
            return i;
    }

    return index;
}

static int semcheck_parse_specialize_addr_target(const char *buffer, size_t length, int index,
    char **type_out, char **method_out)
{
    if (type_out != NULL) *type_out = NULL;
    if (method_out != NULL) *method_out = NULL;
    if (buffer == NULL || length == 0 || index < 0 || (size_t)index >= length)
        return 0;

    size_t i = semcheck_find_specialize_span_start(buffer, length, (size_t)index);
    if (i < length && buffer[i] == '@')
        i++;
    while (i < length && (buffer[i] == ' ' || buffer[i] == '\t'))
        i++;
    const char *kw = "specialize";
    size_t kw_len = strlen(kw);
    if (i + kw_len < length && strncasecmp(buffer + i, kw, kw_len) == 0)
    {
        i += kw_len;
        while (i < length && (buffer[i] == ' ' || buffer[i] == '\t'))
            i++;
    }

    size_t type_start = i;
    int angle_depth = 0;
    while (i < length)
    {
        char ch = buffer[i];
        if (ch == '<')
            angle_depth++;
        else if (ch == '>')
        {
            if (angle_depth > 0)
                angle_depth--;
        }
        else if (ch == '.' && angle_depth == 0)
            break;
        else if (ch == '\n' || ch == ';' || ch == '\r')
            return 0;
        i++;
    }
    if (i >= length || buffer[i] != '.' || i <= type_start)
        return 0;

    size_t type_len = i - type_start;
    char *type_raw = (char *)malloc(type_len + 1);
    if (type_raw == NULL)
        return 0;
    memcpy(type_raw, buffer + type_start, type_len);
    type_raw[type_len] = '\0';

    i++; /* skip '.' */
    size_t method_start = i;
    while (i < length && semcheck_is_ident_char_ascii(buffer[i]))
        i++;
    if (i <= method_start)
    {
        free(type_raw);
        return 0;
    }
    size_t method_len = i - method_start;
    char *method = (char *)malloc(method_len + 1);
    if (method == NULL)
    {
        free(type_raw);
        return 0;
    }
    memcpy(method, buffer + method_start, method_len);
    method[method_len] = '\0';

    char *type_mangled = semcheck_mangle_specialized_type_text(type_raw);
    free(type_raw);
    if (type_mangled == NULL)
    {
        free(method);
        return 0;
    }

    if (type_out != NULL) *type_out = type_mangled; else free(type_mangled);
    if (method_out != NULL) *method_out = method; else free(method);
    return 1;
}

int semcheck_addressof(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;

    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_ADDR);

    semcheck_clear_pointer_info(expr);

    struct Expression *inner = expr->expr_data.addr_data.expr;
    if (inner == NULL)
    {
        semcheck_error_with_context("Error on line %d, address-of operator requires an operand.\\n\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int error_count = 0;
    int inner_type = UNKNOWN_TYPE;
    int treated_as_proc_ref = 0;
    HashNode_t *resolved_proc_symbol = NULL;
    int dbg_specialize_addr = getenv("KGPC_DEBUG_ADDR_SPECIALIZE") != NULL;

    if (dbg_specialize_addr && expr->line_num == 256)
    {
        fprintf(stderr, "[ADDR-SPECIALIZE] line=%d source_index=%d inner_type=%d inner_id=%s\n",
            expr->line_num, expr->source_index, inner->type,
            (inner->type == EXPR_VAR_ID && inner->expr_data.id != NULL) ? inner->expr_data.id : "(n/a)");
    }
    if (inner->type == EXPR_VAR_ID &&
        inner->expr_data.id != NULL &&
        pascal_identifier_equals(inner->expr_data.id, "specialize") &&
        expr->source_index >= 0 &&
        preprocessed_source != NULL && preprocessed_length > 0)
    {
        char *type_id = NULL;
        char *method_id = NULL;
        int parsed_target = semcheck_parse_specialize_addr_target(preprocessed_source, preprocessed_length,
                expr->source_index, &type_id, &method_id);
        if (dbg_specialize_addr)
            fprintf(stderr, "[ADDR-SPECIALIZE] line=%d source_index=%d parsed=%d type=%s method=%s\n",
                expr->line_num, expr->source_index, parsed_target,
                type_id != NULL ? type_id : "(null)",
                method_id != NULL ? method_id : "(null)");
        if (dbg_specialize_addr && !parsed_target && expr->source_index >= 0 &&
            (size_t)expr->source_index < preprocessed_length)
        {
            size_t start = (size_t)expr->source_index;
            size_t end = start + 120;
            if (end > preprocessed_length)
                end = preprocessed_length;
            fprintf(stderr, "[ADDR-SPECIALIZE] source-slice: %.*s\n",
                (int)(end - start), preprocessed_source + start);
        }
        if (parsed_target)
        {
            char *resolved_type = type_id;
            const char *owner = semcheck_get_current_method_owner();
            if (resolved_type != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindIdent(&type_node, symtab, resolved_type) < 0 && owner != NULL)
                {
                    size_t qlen = strlen(owner) + 1 + strlen(resolved_type);
                    char *qualified = (char *)malloc(qlen + 1);
                    if (qualified != NULL)
                    {
                        snprintf(qualified, qlen + 1, "%s.%s", owner, resolved_type);
                        if (FindIdent(&type_node, symtab, qualified) >= 0)
                        {
                            if (dbg_specialize_addr)
                                fprintf(stderr, "[ADDR-SPECIALIZE] qualified type resolved: %s\n", qualified);
                            free(resolved_type);
                            resolved_type = qualified;
                        }
                        else
                        {
                            free(qualified);
                        }
                    }
                }
            }

            if (resolved_type != NULL && method_id != NULL)
            {
                char *base_id = strdup(resolved_type);
                struct Expression *base = mk_varid(expr->line_num, base_id);
                if (base != NULL)
                {
                    struct Expression *access = mk_recordaccess(expr->line_num, base, method_id);
                    if (access != NULL)
                    {
                        if (dbg_specialize_addr)
                            fprintf(stderr, "[ADDR-SPECIALIZE] rewritten to record access: %s.%s\n",
                                resolved_type, method_id);
                        expr->expr_data.addr_data.expr = access;
                        destroy_expr(inner);
                        inner = access;
                        inner->is_specialize_addr_target = 1;
                        method_id = NULL;
                        inner_type = PROCEDURE;
                        treated_as_proc_ref = 1;
                    }
                    else
                    {
                        destroy_expr(base);
                    }
                }
                else if (base_id != NULL)
                {
                    free(base_id);
                }
            }
            if (type_id != NULL && type_id != resolved_type)
                free(type_id);
            if (resolved_type != NULL)
                free(resolved_type);
            if (method_id != NULL)
                free(method_id);
        }
    }

    /* If operand is a bare function/procedure identifier, don't auto-convert to a call */
    if (inner->type == EXPR_VAR_ID && inner->expr_data.id != NULL)
    {
        HashNode_t *inner_symbol = NULL;
        int found = FindIdent(&inner_symbol, symtab, inner->expr_data.id);
        if (found >= 0 &&
            inner_symbol != NULL &&
            (inner_symbol->hash_type == HASHTYPE_FUNCTION ||
             inner_symbol->hash_type == HASHTYPE_PROCEDURE ||
             inner_symbol->hash_type == HASHTYPE_BUILTIN_PROCEDURE))
        {
            inner_type = PROCEDURE;
            treated_as_proc_ref = 1;
            resolved_proc_symbol = inner_symbol;
        }
        /* Fallback: bare method name inside a method body (e.g. @ReadData
         * where ReadData is a method of the current class). */
        if (!treated_as_proc_ref && find_implicit_self_method(symtab, inner->expr_data.id) != NULL)
        {
            inner_type = PROCEDURE;
            treated_as_proc_ref = 1;
        }
    }
    /* Also check if inner is already a FUNCTION_CALL with no args - this can happen
     * when the parser sees a function identifier and auto-converts it to a call.
     * In the @FunctionName case, we don't want to resolve overloads - we want the address. */
    else if (inner->type == EXPR_FUNCTION_CALL && 
             inner->expr_data.function_call_data.args_expr == NULL)
    {
        const char *func_id = inner->expr_data.function_call_data.id;
        if (func_id != NULL)
        {
            HashNode_t *inner_symbol = NULL;
            int found = FindIdent(&inner_symbol, symtab, func_id);
            if (found >= 0 &&
                inner_symbol != NULL &&
                (inner_symbol->hash_type == HASHTYPE_FUNCTION ||
                 inner_symbol->hash_type == HASHTYPE_PROCEDURE ||
                 inner_symbol->hash_type == HASHTYPE_BUILTIN_PROCEDURE))
            {
                /* This is @FunctionName where FunctionName was auto-converted to a call.
                 * Skip overload resolution - we just want the function's address. */
                inner_type = PROCEDURE;
                treated_as_proc_ref = 1;
                resolved_proc_symbol = inner_symbol;
            }
            /* Fallback: bare method name auto-converted to call inside a method body */
            if (!treated_as_proc_ref &&
                find_implicit_self_method(symtab, func_id) != NULL)
            {
                inner_type = PROCEDURE;
                treated_as_proc_ref = 1;
            }
        }
    }
    else if (inner->type == EXPR_FUNCTION_CALL &&
             inner->expr_data.function_call_data.args_expr != NULL &&
             (inner->expr_data.function_call_data.is_method_call_placeholder ||
              inner->expr_data.function_call_data.placeholder_method_name != NULL))
    {
        const char *method_id = inner->expr_data.function_call_data.placeholder_method_name;
        if (method_id == NULL)
            method_id = inner->expr_data.function_call_data.id;

        ListNode_t *arg0 = inner->expr_data.function_call_data.args_expr;
        struct Expression *receiver_expr =
            (arg0 != NULL && arg0->type == LIST_EXPR) ? (struct Expression *)arg0->cur : NULL;
        struct RecordType *rec_info = NULL;

        if (receiver_expr != NULL)
        {
            if (receiver_expr->type == EXPR_VAR_ID && receiver_expr->expr_data.id != NULL)
            {
                HashNode_t *owner_node = NULL;
                if (FindIdent(&owner_node, symtab, receiver_expr->expr_data.id) >= 0 &&
                    owner_node != NULL &&
                    owner_node->hash_type == HASHTYPE_TYPE &&
                    owner_node->type != NULL)
                {
                    KgpcType *owner_type = owner_node->type;
                    if (kgpc_type_is_record(owner_type))
                        rec_info = kgpc_type_get_record(owner_type);
                    else if (kgpc_type_is_pointer(owner_type) &&
                             owner_type->info.points_to != NULL &&
                             kgpc_type_is_record(owner_type->info.points_to))
                        rec_info = kgpc_type_get_record(owner_type->info.points_to);
                }
            }

            if (rec_info == NULL)
            {
                KgpcType *receiver_type = NULL;
                semcheck_expr_with_type(&receiver_type, symtab, receiver_expr, max_scope_lev, NO_MUTATE);
                if (receiver_type != NULL)
                {
                    if (kgpc_type_is_record(receiver_type))
                        rec_info = kgpc_type_get_record(receiver_type);
                    else if (kgpc_type_is_pointer(receiver_type) &&
                             receiver_type->info.points_to != NULL &&
                             kgpc_type_is_record(receiver_type->info.points_to))
                        rec_info = kgpc_type_get_record(receiver_type->info.points_to);
                }
            }
        }

        if (method_id != NULL)
        {
            HashNode_t *method_node = NULL;
            if (rec_info != NULL)
                method_node = semcheck_find_class_method(symtab, rec_info, method_id, NULL);

            if (method_node == NULL)
            {
                const char *owner = semcheck_get_current_method_owner();
                if (owner != NULL)
                {
                    size_t mlen = strlen(owner) + 2 + strlen(method_id);
                    char *mangled_base = (char *)malloc(mlen + 1);
                    if (mangled_base != NULL)
                    {
                        snprintf(mangled_base, mlen + 1, "%s__%s", owner, method_id);
                        ListNode_t *matches = FindAllIdents(symtab, mangled_base);
                        if (matches != NULL)
                        {
                            for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
                            {
                                HashNode_t *candidate = (HashNode_t *)cur->cur;
                                if (candidate != NULL &&
                                    (candidate->hash_type == HASHTYPE_FUNCTION ||
                                     candidate->hash_type == HASHTYPE_PROCEDURE))
                                {
                                    method_node = candidate;
                                    break;
                                }
                            }
                            DestroyList(matches);
                        }
                        free(mangled_base);
                    }
                }
            }

            if (method_node == NULL)
            {
                method_node = semcheck_find_any_proc_symbol(symtab, method_id);
            }

            if (method_node == NULL)
            {
                size_t len = strlen(method_id);
                char *prefixed = (char *)malloc(len + 3);
                if (prefixed != NULL)
                {
                    snprintf(prefixed, len + 3, "__%s", method_id);
                    method_node = semcheck_find_any_proc_symbol(symtab, prefixed);
                    free(prefixed);
                }
            }

            if (method_node != NULL &&
                (method_node->hash_type == HASHTYPE_FUNCTION ||
                 method_node->hash_type == HASHTYPE_PROCEDURE))
            {
                inner_type = PROCEDURE;
                treated_as_proc_ref = 1;
                resolved_proc_symbol = method_node;
            }
        }
    }
    /* Handle @obj.Method - address of a method through record access.
     * The inner expression is EXPR_RECORD_ACCESS where the field is a method name.
     * We need to detect this before semcheck converts it to a function call. */
    else if (inner->type == EXPR_RECORD_ACCESS &&
             inner->expr_data.record_access_data.field_id != NULL)
    {
        const char *field_id = inner->expr_data.record_access_data.field_id;
        struct Expression *record_expr = inner->expr_data.record_access_data.record_expr;
        if (record_expr != NULL)
        {
            struct RecordType *rec_info = NULL;
            int skip_record_expr_semcheck = 0;

            if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
            {
                HashNode_t *owner_node = NULL;
                if (FindIdent(&owner_node, symtab, record_expr->expr_data.id) >= 0 &&
                    owner_node != NULL &&
                    owner_node->hash_type == HASHTYPE_TYPE &&
                    owner_node->type != NULL)
                {
                    KgpcType *owner_type = owner_node->type;
                    if (kgpc_type_is_record(owner_type))
                        rec_info = kgpc_type_get_record(owner_type);
                    else if (kgpc_type_is_pointer(owner_type) &&
                             owner_type->info.points_to != NULL &&
                             kgpc_type_is_record(owner_type->info.points_to))
                        rec_info = kgpc_type_get_record(owner_type->info.points_to);
                }
                else if (dbg_specialize_addr)
                {
                    fprintf(stderr, "[ADDR-SPECIALIZE] owner type not found for %s\n",
                        record_expr->expr_data.id);
                }
                if (inner->is_specialize_addr_target)
                {
                    skip_record_expr_semcheck = 1;
                }
            }

            /* Resolve the base expression to get its record type */
            if (rec_info == NULL && !skip_record_expr_semcheck)
            {
                KgpcType *record_kgpc = NULL;
                semcheck_expr_with_type(&record_kgpc, symtab, record_expr, max_scope_lev, NO_MUTATE);
                int record_tag = semcheck_tag_from_kgpc(record_kgpc);
                if (record_tag == RECORD_TYPE && record_kgpc != NULL && kgpc_type_is_record(record_kgpc))
                    rec_info = kgpc_type_get_record(record_kgpc);
                else if (record_tag == POINTER_TYPE && record_kgpc != NULL &&
                         kgpc_type_is_pointer(record_kgpc) &&
                         record_kgpc->info.points_to != NULL &&
                         kgpc_type_is_record(record_kgpc->info.points_to))
                    rec_info = kgpc_type_get_record(record_kgpc->info.points_to);
            }

            if (rec_info != NULL)
            {
                HashNode_t *method_node = semcheck_find_class_method(symtab, rec_info, field_id, NULL);
                if (method_node != NULL &&
                    (method_node->hash_type == HASHTYPE_FUNCTION ||
                     method_node->hash_type == HASHTYPE_PROCEDURE))
                {
                    if (dbg_specialize_addr)
                        fprintf(stderr, "[ADDR-SPECIALIZE] method resolved on type receiver: %s\n", field_id);
                    inner_type = PROCEDURE;
                    treated_as_proc_ref = 1;
                    resolved_proc_symbol = method_node;
                }
                else if (dbg_specialize_addr)
                {
                    fprintf(stderr, "[ADDR-SPECIALIZE] method not found on type receiver: %s\n", field_id);
                }
            }
            else if (field_id != NULL)
            {
                HashNode_t *fallback_symbol = semcheck_find_any_proc_symbol(symtab, field_id);
                if (fallback_symbol == NULL &&
                    record_expr != NULL &&
                    record_expr->type == EXPR_VAR_ID &&
                    record_expr->expr_data.id != NULL &&
                    pascal_identifier_equals(record_expr->expr_data.id, "Self"))
                {
                    const char *owner = semcheck_get_current_method_owner();
                    if (owner != NULL)
                    {
                        size_t mlen = strlen(owner) + 2 + strlen(field_id);
                        char *mangled_base = (char *)malloc(mlen + 1);
                        if (mangled_base != NULL)
                        {
                            snprintf(mangled_base, mlen + 1, "%s__%s", owner, field_id);
                            ListNode_t *matches = FindAllIdents(symtab, mangled_base);
                            if (matches != NULL)
                            {
                                HashNode_t *owner_match = NULL;
                                HashNode_t *first_proc = NULL;
                                for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
                                {
                                    HashNode_t *candidate = (HashNode_t *)cur->cur;
                                    if (candidate == NULL ||
                                        (candidate->hash_type != HASHTYPE_FUNCTION &&
                                         candidate->hash_type != HASHTYPE_PROCEDURE) ||
                                        candidate->type == NULL)
                                        continue;
                                    if (first_proc == NULL)
                                        first_proc = candidate;

                                    ListNode_t *params = kgpc_type_get_procedure_params(candidate->type);
                                    if (params == NULL || params->cur == NULL)
                                        continue;
                                    Tree_t *first_decl = (Tree_t *)params->cur;
                                    const char *first_type_id = NULL;
                                    if (first_decl->type == TREE_VAR_DECL)
                                        first_type_id = first_decl->tree_data.var_decl_data.type_id;
                                    else if (first_decl->type == TREE_ARR_DECL)
                                        first_type_id = first_decl->tree_data.arr_decl_data.type_id;
                                    if (first_type_id != NULL &&
                                        pascal_identifier_equals(first_type_id, owner))
                                    {
                                        owner_match = candidate;
                                        break;
                                    }
                                }
                                fallback_symbol = (owner_match != NULL) ? owner_match : first_proc;
                                DestroyList(matches);
                            }
                            free(mangled_base);
                        }
                    }
                }
                if (fallback_symbol == NULL)
                {
                    size_t len = strlen(field_id);
                    char *prefixed = (char *)malloc(len + 3);
                    if (prefixed != NULL)
                    {
                        snprintf(prefixed, len + 3, "__%s", field_id);
                        fallback_symbol = semcheck_find_any_proc_symbol(symtab, prefixed);
                        free(prefixed);
                    }
                }
                if (fallback_symbol != NULL)
                {
                    if (dbg_specialize_addr)
                        fprintf(stderr, "[ADDR-SPECIALIZE] fallback proc symbol resolved by name: %s\n", field_id);
                    inner_type = PROCEDURE;
                    treated_as_proc_ref = 1;
                    resolved_proc_symbol = fallback_symbol;
                }
                else if (dbg_specialize_addr)
                {
                    fprintf(stderr, "[ADDR-SPECIALIZE] fallback proc symbol not found: %s\n", field_id);
                }

                /* Keep @TypeLike.Method on the procedure-reference path even if
                 * symbol lookup is deferred/unavailable in this pass. */
                if (fallback_symbol == NULL &&
                    record_expr->type == EXPR_VAR_ID &&
                    record_expr->expr_data.id != NULL &&
                    inner->is_specialize_addr_target)
                {
                    inner_type = PROCEDURE;
                    treated_as_proc_ref = 1;
                }
            }
        }
    }

    if (!treated_as_proc_ref)
    {
        KgpcType *inner_kgpc_type = NULL;
        semcheck_expr_with_type(&inner_kgpc_type, symtab, inner, max_scope_lev, NO_MUTATE);
        inner_type = semcheck_tag_from_kgpc(inner_kgpc_type);
    }

    /* Special case: address-of array expressions (array variables/fields). */
    if (inner != NULL && inner->resolved_kgpc_type != NULL &&
        kgpc_type_is_array(inner->resolved_kgpc_type))
    {
        if (getenv("KGPC_ASSERT_ADDROF_ARRAY") != NULL)
            assert(kgpc_type_is_array(inner->resolved_kgpc_type));
        KgpcType *array_type = inner->resolved_kgpc_type;
        KgpcType *element_type = kgpc_type_get_array_element_type_resolved(array_type, symtab);
        int element_tag = UNKNOWN_TYPE;
        const char *element_type_id = NULL;
        if (element_type != NULL)
        {
            element_tag = semcheck_tag_from_kgpc(element_type);
            if (element_type->type_alias != NULL)
            {
                element_type_id = element_type->type_alias->alias_name != NULL
                    ? element_type->type_alias->alias_name
                    : element_type->type_alias->target_type_id;
            }
        }

        semcheck_set_pointer_info(expr, element_tag, element_type_id);
        *type_return = POINTER_TYPE;

        /* Preserve pointer-to-array KgpcType for overloads like pSigSet. */
        kgpc_type_retain(array_type);
        KgpcType *ptr_type = create_pointer_type(array_type);
        if (ptr_type != NULL)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, ptr_type);
            destroy_kgpc_type(ptr_type);
        }
        return error_count;
    }

    /* Special case: If the inner expression was auto-converted from a function identifier
     * to a function call (because we're in NO_MUTATE mode), we need to reverse that
     * since we're taking the address of the function, not calling it. */
    int converted_to_proc_addr = treated_as_proc_ref;  /* Already converted if we treated it as proc ref */
    if (!converted_to_proc_addr && inner->type == EXPR_FUNCTION_CALL && 
        inner->expr_data.function_call_data.args_expr == NULL)
    {
        const char *func_id = inner->expr_data.function_call_data.id;
        if (func_id != NULL)
        {
            HashNode_t *func_symbol = NULL;
            if (FindIdent(&func_symbol, symtab, func_id) >= 0 &&
                func_symbol != NULL && 
                (func_symbol->hash_type == HASHTYPE_FUNCTION ||
                 func_symbol->hash_type == HASHTYPE_PROCEDURE ||
                 func_symbol->hash_type == HASHTYPE_BUILTIN_PROCEDURE))
            {
                /* This was auto-converted - treat it as a procedure reference instead */
                inner_type = PROCEDURE;
                converted_to_proc_addr = 1;
                /* We'll handle this below in the PROCEDURE case */
            }
        }
    }

    if (inner_type == UNKNOWN_TYPE)
    {
        if (inner->type == EXPR_VAR_ID && inner->expr_data.id != NULL)
        {
            HashNode_t *inner_symbol = NULL;
            if (FindIdent(&inner_symbol, symtab, inner->expr_data.id) >= 0 &&
                inner_symbol != NULL && inner_symbol->type == NULL &&
                inner_symbol->is_var_parameter)
            {
                *type_return = POINTER_TYPE;
                if (expr->resolved_kgpc_type != NULL)
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                expr->resolved_kgpc_type = create_pointer_type(NULL);
                return error_count;
            }
        }
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    const char *type_id = NULL;
    if (inner_type == POINTER_TYPE && inner->pointer_subtype_id != NULL)
        type_id = inner->pointer_subtype_id;
    else if (inner_type == CHAR_TYPE)
    {
        /* Preserve character subtype identity for @string[index] cases,
         * especially WideString/UnicodeString element addresses. */
        if (inner->array_element_type_id != NULL)
            type_id = inner->array_element_type_id;
        else if (inner->resolved_kgpc_type != NULL)
        {
            struct TypeAlias *inner_alias = kgpc_type_get_type_alias(inner->resolved_kgpc_type);
            if (inner_alias != NULL)
            {
                if (inner_alias->alias_name != NULL)
                    type_id = inner_alias->alias_name;
                else if (inner_alias->target_type_id != NULL)
                    type_id = inner_alias->target_type_id;
            }
        }
    }

    struct RecordType *record_info = NULL;
    if (inner->resolved_kgpc_type != NULL)
    {
        if (inner_type == RECORD_TYPE && kgpc_type_is_record(inner->resolved_kgpc_type))
            record_info = kgpc_type_get_record(inner->resolved_kgpc_type);
        else if (inner_type == POINTER_TYPE && kgpc_type_is_pointer(inner->resolved_kgpc_type) &&
                 inner->resolved_kgpc_type->info.points_to != NULL &&
                 kgpc_type_is_record(inner->resolved_kgpc_type->info.points_to))
            record_info = kgpc_type_get_record(inner->resolved_kgpc_type->info.points_to);
    }

    semcheck_set_pointer_info(expr, inner_type, type_id);
    *type_return = POINTER_TYPE;
    
    /* Create a proper KgpcType for the address-of expression */
    KgpcType *pointed_to_type = NULL;
    
    /* Convert inner_type to KgpcType */
    if (inner_type == INT_TYPE) {
        pointed_to_type = create_primitive_type(INT_TYPE);
    } else if (inner_type == LONGINT_TYPE) {
        pointed_to_type = create_primitive_type(LONGINT_TYPE);
    } else if (inner_type == REAL_TYPE) {
        pointed_to_type = create_primitive_type(REAL_TYPE);
    } else if (inner_type == CHAR_TYPE) {
        /* Keep resolved char subtype (e.g. WideChar) rather than collapsing to
         * plain Char; overload resolution depends on this metadata. */
        if (inner->resolved_kgpc_type != NULL && kgpc_type_is_char(inner->resolved_kgpc_type)) {
            pointed_to_type = inner->resolved_kgpc_type;
            kgpc_type_retain(pointed_to_type);
        } else {
            pointed_to_type = create_primitive_type(CHAR_TYPE);
        }
    } else if (inner_type == STRING_TYPE) {
        pointed_to_type = create_primitive_type(STRING_TYPE);
    } else if (inner_type == RECORD_TYPE && record_info != NULL) {
        pointed_to_type = create_record_type(record_info);
    } else if (inner_type == RECORD_TYPE && record_info == NULL) {
        /* Record type without record_info — use inner's resolved KgpcType if available */
        if (inner->resolved_kgpc_type != NULL) {
            pointed_to_type = inner->resolved_kgpc_type;
            kgpc_type_retain(pointed_to_type);
        } else {
            pointed_to_type = create_primitive_type(RECORD_TYPE);
        }
    } else if (inner_type == POINTER_TYPE) {
        /* For pointer types, get the resolved KgpcType of the inner expression */
        if (inner->resolved_kgpc_type != NULL) {
            pointed_to_type = inner->resolved_kgpc_type;
            kgpc_type_retain(pointed_to_type);  /* We're taking a reference */
        } else {
            /* Fallback: create untyped pointer */
            pointed_to_type = NULL;
        }
    } else if (inner_type == PROCEDURE) {
        int proc_type_owned = 0;
        KgpcType *proc_type = NULL;
        
        /* For procedures/functions, we need the actual procedural type, not the return type.
         * semcheck_resolve_expression_kgpc_type returns the return type for functions,
         * so we look up the symbol directly instead. */
        const char *proc_id = NULL;
        if (inner->type == EXPR_VAR_ID)
        {
            proc_id = inner->expr_data.id;
        }
        else if (inner->type == EXPR_FUNCTION_CALL && inner->expr_data.function_call_data.args_expr == NULL)
        {
            proc_id = inner->expr_data.function_call_data.id;
        }
        
        if (proc_id != NULL)
        {
            HashNode_t *proc_symbol = NULL;
            if (FindIdent(&proc_symbol, symtab, proc_id) >= 0 &&
                proc_symbol != NULL && 
                (proc_symbol->hash_type == HASHTYPE_PROCEDURE || proc_symbol->hash_type == HASHTYPE_FUNCTION) &&
                proc_symbol->type != NULL && proc_symbol->type->kind == TYPE_KIND_PROCEDURE)
            {
                /* Use the procedure type from the symbol */
                proc_type = proc_symbol->type;
                proc_type_owned = 0; /* Shared reference */
            }
            /* Fallback: implicit Self method */
            if (proc_type == NULL)
            {
                HashNode_t *method_node = find_implicit_self_method(symtab, proc_id);
                if (method_node != NULL &&
                    method_node->type != NULL && method_node->type->kind == TYPE_KIND_PROCEDURE)
                {
                    proc_type = method_node->type;
                    proc_type_owned = 0;
                }
            }
        }
        else if (resolved_proc_symbol != NULL &&
                 resolved_proc_symbol->type != NULL &&
                 resolved_proc_symbol->type->kind == TYPE_KIND_PROCEDURE)
        {
            proc_type = resolved_proc_symbol->type;
            proc_type_owned = 0;
        }
        
        if (proc_type != NULL)
        {
            if (!proc_type_owned)
                kgpc_type_retain(proc_type);
            pointed_to_type = proc_type;
        }
        /* Handle both EXPR_VAR_ID (for procedures) and EXPR_FUNCTION_CALL (for functions that were auto-converted) */
        if (inner->type == EXPR_VAR_ID)
        {
            HashNode_t *proc_symbol = NULL;
            if (FindIdent(&proc_symbol, symtab, inner->expr_data.id) >= 0 &&
                proc_symbol != NULL && 
                (proc_symbol->hash_type == HASHTYPE_PROCEDURE || proc_symbol->hash_type == HASHTYPE_FUNCTION))
            {
                /* Found directly */
            }
            else
            {
                /* Try implicit Self method lookup */
                proc_symbol = find_implicit_self_method(symtab, inner->expr_data.id);
            }
            if (proc_symbol != NULL &&
                (proc_symbol->hash_type == HASHTYPE_PROCEDURE || proc_symbol->hash_type == HASHTYPE_FUNCTION))
            {
                expr->expr_data.addr_data.expr = NULL;
                destroy_expr(inner);
                expr->type = EXPR_ADDR_OF_PROC;
                expr->expr_data.addr_of_proc_data.proc_mangled_id = proc_symbol->mangled_id ? strdup(proc_symbol->mangled_id) : NULL;
                expr->expr_data.addr_of_proc_data.proc_id = proc_symbol->id ? strdup(proc_symbol->id) : NULL;
                /* Resolve the type NOW while the symbol is still alive,
                 * instead of relying on procedure_symbol later. */
                if (proc_symbol->type != NULL && proc_symbol->type->kind == TYPE_KIND_PROCEDURE)
                {
                    kgpc_type_retain(proc_symbol->type);
                    expr->resolved_kgpc_type = create_pointer_type(proc_symbol->type);
                }
                else
                {
                    KgpcType *generic_proc = create_procedure_type(NULL, NULL);
                    expr->resolved_kgpc_type = create_pointer_type(generic_proc);
                    if (generic_proc != NULL)
                        destroy_kgpc_type(generic_proc);
                }
                converted_to_proc_addr = 1;
            }
        }
        else if (inner->type == EXPR_FUNCTION_CALL && inner->expr_data.function_call_data.args_expr == NULL)
        {
            /* This was auto-converted from a function identifier - get the original symbol */
            const char *func_id = inner->expr_data.function_call_data.id;
            if (func_id != NULL)
            {
                HashNode_t *proc_symbol = NULL;
                if (FindIdent(&proc_symbol, symtab, func_id) >= 0 &&
                    proc_symbol != NULL &&
                    (proc_symbol->hash_type == HASHTYPE_FUNCTION || proc_symbol->hash_type == HASHTYPE_PROCEDURE))
                {
                    /* Found directly */
                }
                else
                {
                    /* Try implicit Self method lookup */
                    proc_symbol = find_implicit_self_method(symtab, func_id);
                }
                if (proc_symbol != NULL &&
                    (proc_symbol->hash_type == HASHTYPE_FUNCTION || proc_symbol->hash_type == HASHTYPE_PROCEDURE))
                {
                    expr->expr_data.addr_data.expr = NULL;
                    destroy_expr(inner);
                    expr->type = EXPR_ADDR_OF_PROC;
                    expr->expr_data.addr_of_proc_data.proc_mangled_id = proc_symbol->mangled_id ? strdup(proc_symbol->mangled_id) : NULL;
                    expr->expr_data.addr_of_proc_data.proc_id = proc_symbol->id ? strdup(proc_symbol->id) : NULL;
                    /* Resolve the type NOW while the symbol is still alive. */
                    if (proc_symbol->type != NULL && proc_symbol->type->kind == TYPE_KIND_PROCEDURE)
                    {
                        kgpc_type_retain(proc_symbol->type);
                        expr->resolved_kgpc_type = create_pointer_type(proc_symbol->type);
                    }
                    else
                    {
                        KgpcType *generic_proc = create_procedure_type(NULL, NULL);
                        expr->resolved_kgpc_type = create_pointer_type(generic_proc);
                        if (generic_proc != NULL)
                            destroy_kgpc_type(generic_proc);
                    }
                    converted_to_proc_addr = 1;
                }
            }
        }
        else if (inner->type == EXPR_RECORD_ACCESS &&
                 inner->expr_data.record_access_data.field_id != NULL)
        {
            /* @obj.Method - get the method symbol from the record type */
            const char *field_id = inner->expr_data.record_access_data.field_id;
            struct Expression *record_expr = inner->expr_data.record_access_data.record_expr;
            struct RecordType *rec_info = NULL;

            if (record_expr != NULL && record_expr->resolved_kgpc_type != NULL)
            {
                KgpcType *rt = record_expr->resolved_kgpc_type;
                if (kgpc_type_is_record(rt))
                    rec_info = kgpc_type_get_record(rt);
                else if (kgpc_type_is_pointer(rt) &&
                         rt->info.points_to != NULL &&
                         kgpc_type_is_record(rt->info.points_to))
                    rec_info = kgpc_type_get_record(rt->info.points_to);
            }

            if (rec_info != NULL)
            {
                struct RecordType *actual_owner = NULL;
                HashNode_t *method_node = semcheck_find_class_method(symtab, rec_info, field_id, &actual_owner);
                if (method_node != NULL &&
                    (method_node->hash_type == HASHTYPE_FUNCTION ||
                     method_node->hash_type == HASHTYPE_PROCEDURE))
                {
                    expr->expr_data.addr_data.expr = NULL;
                    destroy_expr(inner);
                    expr->type = EXPR_ADDR_OF_PROC;
                    expr->expr_data.addr_of_proc_data.proc_mangled_id =
                        method_node->mangled_id ? strdup(method_node->mangled_id) : NULL;
                    expr->expr_data.addr_of_proc_data.proc_id =
                        method_node->id ? strdup(method_node->id) : NULL;
                    if (method_node->type != NULL && method_node->type->kind == TYPE_KIND_PROCEDURE)
                    {
                        kgpc_type_retain(method_node->type);
                        expr->resolved_kgpc_type = create_pointer_type(method_node->type);
                    }
                    else
                    {
                        KgpcType *generic_proc = create_procedure_type(NULL, NULL);
                        expr->resolved_kgpc_type = create_pointer_type(generic_proc);
                        if (generic_proc != NULL)
                            destroy_kgpc_type(generic_proc);
                    }
                    converted_to_proc_addr = 1;
                }
            }
        }
        else if (resolved_proc_symbol != NULL &&
                 (resolved_proc_symbol->hash_type == HASHTYPE_FUNCTION ||
                  resolved_proc_symbol->hash_type == HASHTYPE_PROCEDURE))
        {
            expr->expr_data.addr_data.expr = NULL;
            destroy_expr(inner);
            expr->type = EXPR_ADDR_OF_PROC;
            expr->expr_data.addr_of_proc_data.proc_mangled_id =
                resolved_proc_symbol->mangled_id ? strdup(resolved_proc_symbol->mangled_id) : NULL;
            expr->expr_data.addr_of_proc_data.proc_id =
                resolved_proc_symbol->id ? strdup(resolved_proc_symbol->id) : NULL;
            if (resolved_proc_symbol->type != NULL &&
                resolved_proc_symbol->type->kind == TYPE_KIND_PROCEDURE)
            {
                kgpc_type_retain(resolved_proc_symbol->type);
                expr->resolved_kgpc_type = create_pointer_type(resolved_proc_symbol->type);
            }
            else
            {
                KgpcType *generic_proc = create_procedure_type(NULL, NULL);
                expr->resolved_kgpc_type = create_pointer_type(generic_proc);
                if (generic_proc != NULL)
                    destroy_kgpc_type(generic_proc);
            }
            converted_to_proc_addr = 1;
        }
        else if (inner->type == EXPR_RECORD_ACCESS &&
                 inner->expr_data.record_access_data.field_id != NULL)
        {
            const char *field_id = inner->expr_data.record_access_data.field_id;
            struct Expression *record_expr = inner->expr_data.record_access_data.record_expr;
            if (record_expr == NULL ||
                record_expr->type != EXPR_VAR_ID ||
                record_expr->expr_data.id == NULL ||
                !inner->is_specialize_addr_target)
            {
                converted_to_proc_addr = 0;
            }
            else
            {
            size_t nlen = strlen(field_id) + 2;
            char *synth_name = (char *)malloc(nlen + 1);
            if (synth_name != NULL)
                snprintf(synth_name, nlen + 1, "__%s", field_id);

            if (synth_name != NULL)
            {
                HashNode_t *existing = NULL;
                if (FindIdent(&existing, symtab, synth_name) < 0)
                {
                    KgpcType *generic_proc = create_procedure_type(NULL, NULL);
                    if (generic_proc != NULL)
                    {
                        char *id_dup = strdup(synth_name);
                        char *mangled_dup = strdup(synth_name);
                        if (id_dup != NULL && mangled_dup != NULL)
                            (void)PushProcedureOntoScope_Typed(symtab, id_dup, mangled_dup, generic_proc);
                        else
                        {
                            free(id_dup);
                            free(mangled_dup);
                        }
                        destroy_kgpc_type(generic_proc);
                    }
                }

                expr->expr_data.addr_data.expr = NULL;
                destroy_expr(inner);
                expr->type = EXPR_ADDR_OF_PROC;
                expr->expr_data.addr_of_proc_data.proc_id = strdup(synth_name);
                expr->expr_data.addr_of_proc_data.proc_mangled_id = strdup(synth_name);
                free(synth_name);
            }
            if (expr->resolved_kgpc_type != NULL)
            {
                destroy_kgpc_type(expr->resolved_kgpc_type);
                expr->resolved_kgpc_type = NULL;
            }
            {
                KgpcType *generic_proc = create_procedure_type(NULL, NULL);
                expr->resolved_kgpc_type = create_pointer_type(generic_proc);
                if (generic_proc != NULL)
                    destroy_kgpc_type(generic_proc);
            }
            converted_to_proc_addr = 1;
            }
        }
    }
    /* For other types, we could add more conversions here */
    
    /* Create the pointer type */
    if (pointed_to_type != NULL && expr->type != EXPR_ADDR_OF_PROC) {
        if (expr->resolved_kgpc_type != NULL) {
            destroy_kgpc_type(expr->resolved_kgpc_type);
        }
        expr->resolved_kgpc_type = create_pointer_type(pointed_to_type);
        destroy_kgpc_type(pointed_to_type);  /* create_pointer_type retained it */
    }
    
    /* If we successfully converted to a procedure address, don't count inner expression errors.
     * Those errors were from trying to call the function with no arguments, which is not what we want. */
    if (converted_to_proc_addr && expr->type == EXPR_ADDR_OF_PROC)
    {
        return 0;  /* Success - ignore inner errors */
    }
    
    return error_count;
}
