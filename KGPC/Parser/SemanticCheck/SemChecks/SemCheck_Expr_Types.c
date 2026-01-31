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

    if (expr->expr_data.typecast_data.expr != NULL)
        error_count += semcheck_expr_main(&inner_type, symtab,
            expr->expr_data.typecast_data.expr, max_scope_lev, NO_MUTATE);

    int target_type = expr->expr_data.typecast_data.target_type;
    int builtin_mapped = semcheck_map_builtin_type_name(symtab,
        expr->expr_data.typecast_data.target_type_id);
    int target_is_builtin = (builtin_mapped != UNKNOWN_TYPE);
    if (target_type == UNKNOWN_TYPE && builtin_mapped != UNKNOWN_TYPE)
        target_type = builtin_mapped;

    /* Resolve the target type unless we already mapped a builtin */
    if (target_type == UNKNOWN_TYPE || !target_is_builtin)
    {
        HashNode_t *type_node = semcheck_find_preferred_type_node(symtab,
            expr->expr_data.typecast_data.target_type_id);
        if (type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
        {
            int call_result = semcheck_reinterpret_typecast_as_call(type_return, symtab,
                expr, max_scope_lev);
            if (call_result == 0)
                return 0;
        }
        error_count += resolve_type_identifier(&target_type, symtab,
            expr->expr_data.typecast_data.target_type_id, expr->line_num);
    }

    if (target_type == UNKNOWN_TYPE &&
        expr->expr_data.typecast_data.target_type_id == NULL)
    {
        semcheck_error_with_context("Error on line %d, typecast requires a target type.\n\n",
            expr->line_num);
        ++error_count;
    }

    *type_return = target_type;
    expr->resolved_type = target_type;

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
            target_node = semcheck_find_type_node_with_kgpc_type(
                symtab, expr->expr_data.typecast_data.target_type_id);
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
        if (expr->expr_data.typecast_data.target_type_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, expr->expr_data.typecast_data.target_type_id) >= 0 &&
                target_node != NULL && target_node->type != NULL)
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
                        expr->record_type = points_to->info.record_info;
                    }
                    else if (points_to->kind == TYPE_KIND_PRIMITIVE)
                    {
                        int subtype = kgpc_type_get_primitive_tag(points_to);
                        semcheck_set_pointer_info(expr, subtype, NULL);
                    }
                }
            }
        }

        if (resolved_ptr == NULL)
        {
            resolved_ptr = create_pointer_type(NULL);
            semcheck_set_pointer_info(expr, UNKNOWN_TYPE, expr->expr_data.typecast_data.target_type_id);
        }

        expr->resolved_kgpc_type = resolved_ptr;
    }
    else if (target_type == RECORD_TYPE)
    {
        HashNode_t *target_node = NULL;
        struct RecordType *record_info = NULL;
        if (expr->expr_data.typecast_data.target_type_id != NULL)
        {
            target_node = semcheck_find_type_node_with_kgpc_type(
                symtab, expr->expr_data.typecast_data.target_type_id);
            if (target_node == NULL &&
                FindIdent(&target_node, symtab, expr->expr_data.typecast_data.target_type_id) >= 0)
            {
                /* target_node assigned by FindIdent when present */
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
        }

        expr->record_type = record_info;
        if (target_node != NULL && target_node->type != NULL)
        {
            kgpc_type_retain(target_node->type);
            expr->resolved_kgpc_type = target_node->type;
        }
        else if (record_info != NULL)
        {
            expr->resolved_kgpc_type = create_record_type(record_info);
        }
        else
        {
            expr->resolved_kgpc_type = create_primitive_type(RECORD_TYPE);
        }
    }
    else if (target_type != UNKNOWN_TYPE)
    {
        expr->resolved_kgpc_type = create_primitive_type(target_type);
        semcheck_clear_array_info(expr);
        expr->record_type = NULL;
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
    error_count += semcheck_expr_main(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);

    struct RecordType *value_record = value_expr->record_type;
    
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
    
    if (!is_valid_target)
    {
        semcheck_error_with_context("Error on line %d, \"is\" operator requires a class type on the right-hand side.\n\n",
            expr->line_num);
        ++error_count;
    }
    target_type = RECORD_TYPE;

    expr->expr_data.is_data.target_type = target_type;
    expr->expr_data.is_data.target_record_type = target_record;
    expr->resolved_type = BOOL;
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
    error_count += semcheck_expr_main(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);

    struct RecordType *value_record = value_expr->record_type;
    
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
    expr->record_type = target_record;
    expr->resolved_type = target_type;
    
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
    expr->record_type = NULL;

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
    error_count += semcheck_expr_main(&pointer_type, symtab, pointer_expr,
        max_scope_lev, NO_MUTATE);

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

    if (target_type == UNKNOWN_TYPE)
        target_type = LONGINT_TYPE;

    if (target_type == POINTER_TYPE)
        semcheck_set_pointer_info(expr, POINTER_TYPE, pointer_expr->pointer_subtype_id);
    else if (target_type == RECORD_TYPE)
    {
        expr->record_type = pointer_expr->record_type;
        if (expr->record_type == NULL && pointer_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, pointer_expr->pointer_subtype_id) != -1 && target_node != NULL)
                expr->record_type = get_record_type_from_node(target_node);
        }
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
    expr->resolved_type = target_type;
    *type_return = target_type;
    return error_count;
}

int semcheck_property_type_info(SymTab_t *symtab, struct ClassProperty *property,
    int line_num, int *type_out, struct RecordType **record_out)
{
    if (type_out == NULL || property == NULL)
        return 1;

    int resolved_type = property->type;
    if (property->type_id != NULL)
    {
        if (resolve_type_identifier(&resolved_type, symtab, property->type_id, line_num) != 0)
        {
            semcheck_error_with_context("Error on line %d, unable to resolve type for property %s.\n\n",
                line_num, property->name != NULL ? property->name : "<unnamed>");
            return 1;
        }
    }

    if (resolved_type == UNKNOWN_TYPE && property->type_id == NULL)
    {
        semcheck_error_with_context("Error on line %d, property %s must specify a type.\n\n",
            line_num, property->name != NULL ? property->name : "<unnamed>");
        return 1;
    }

    *type_out = resolved_type;
    if (record_out != NULL)
    {
        if (resolved_type == RECORD_TYPE && property->type_id != NULL)
            *record_out = semcheck_lookup_record_type(symtab, property->type_id);
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
    expr->expr_data.function_call_data.resolved_func = NULL;
    expr->expr_data.function_call_data.call_hash_type = method_node->hash_type;
    semcheck_expr_set_call_kgpc_type(expr, method_node->type, 0);
    expr->expr_data.function_call_data.is_call_info_valid = 1;
    expr->record_type = NULL;
    expr->resolved_type = UNKNOWN_TYPE;
    expr->is_array_expr = 0;
    expr->array_element_type = UNKNOWN_TYPE;
    expr->array_element_type_id = NULL;
    expr->array_element_record_type = NULL;
    expr->array_element_size = 0;

    return semcheck_expr_main(type_return, symtab, expr, max_scope_lev, mutating);
}

int semcheck_recordaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_RECORD_ACCESS);

    expr->record_type = NULL;
    semcheck_clear_array_info(expr);

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    const char *field_id = expr->expr_data.record_access_data.field_id;
    if (record_expr == NULL || field_id == NULL)
    {
        semcheck_error_with_context("Error on line %d, malformed record field access.\n\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
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
                new_record_access->record_type = NULL;
                new_record_access->resolved_type = UNKNOWN_TYPE;
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
        
        /* Check if the "unit name" identifier exists in symbol table */
        int find_result = FindIdent(&unit_check, symtab, unit_id);
        if (find_result == -1)
        {
            /* Identifier not found - might be a unit qualifier.
             * Try to look up the field_id directly as it may be an exported constant/var. */
            HashNode_t *field_node = NULL;
            char *field_id_copy = strdup(field_id);
            if (field_id_copy != NULL && FindIdent(&field_node, symtab, field_id_copy) >= 0 && field_node != NULL)
            {
                free(field_id_copy);
                /* Found the field as a direct identifier - transform the expression */
                if (field_node->hash_type == HASHTYPE_CONST)
                {
                    /* Transform to integer literal for constants */
                    expr->type = EXPR_INUM;
                    expr->expr_data.i_num = field_node->const_int_value;
                    expr->resolved_type = LONGINT_TYPE;
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
                            expr->resolved_type = ENUM_TYPE;
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
        }
    }

    int error_count = 0;
    int record_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&record_type, symtab, record_expr, max_scope_lev, mutating);

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_recordaccess: field_id=%s, record_type=%d, record_expr->record_type=%p\n",
            field_id, record_type, record_expr->record_type);
    }

    struct RecordType *record_info = NULL;
    if (record_type == RECORD_TYPE)
    {
        record_info = record_expr->record_type;
        /* Fallback: infer from resolved KgpcType when legacy record_type is missing */
        if (record_info == NULL && record_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_record(record_expr->resolved_kgpc_type)) {
            record_info = kgpc_type_get_record(record_expr->resolved_kgpc_type);
        }
        if (record_info == NULL && record_expr->type == EXPR_TYPECAST)
        {
            const char *target_id = record_expr->expr_data.typecast_data.target_type_id;
            if (target_id != NULL)
            {
                HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type(symtab, target_id);
                if (type_node == NULL)
                    FindIdent(&type_node, symtab, (char *)target_id);
                if (type_node != NULL)
                {
                    record_info = get_record_type_from_node(type_node);
                    if (record_info == NULL && type_node->type != NULL &&
                        kgpc_type_is_record(type_node->type))
                    {
                        record_info = kgpc_type_get_record(type_node->type);
                    }
                    record_expr->record_type = record_info;
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
        record_info = record_expr->record_type;
        if (record_info == NULL && record_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, record_expr->pointer_subtype_id) != -1 &&
                target_node != NULL)
            {
                record_info = get_record_type_from_node(target_node);
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
        struct RecordType *helper_record = semcheck_lookup_type_helper(symtab, record_type,
            alias_type_name != NULL ? alias_type_name : expr_type_name);
        if (helper_record != NULL)
        {
            record_type = RECORD_TYPE;
            record_info = helper_record;
        }
        /* FPC string helper methods: Transform S.Trim into Trim(S), S.Substring(i) into Substring(S, i) etc.
         * This provides FPC-compatible string method syntax without full type helper infrastructure. */
        else if (record_type == STRING_TYPE && field_id != NULL)
        {
            /* List of known string helper methods that map to SysUtils functions */
            HashNode_t *func_node = NULL;
            int is_string_method = 0;
            if (pascal_identifier_equals(field_id, "Trim") ||
                pascal_identifier_equals(field_id, "TrimLeft") ||
                pascal_identifier_equals(field_id, "TrimRight") ||
                pascal_identifier_equals(field_id, "UpperCase") ||
                pascal_identifier_equals(field_id, "LowerCase") ||
                pascal_identifier_equals(field_id, "Length"))
            {
                /* These are no-arg methods that take the string as single argument */
                if (FindIdent(&func_node, symtab, (char *)field_id) == 0 &&
                    func_node != NULL && func_node->hash_type == HASHTYPE_FUNCTION)
                {
                    is_string_method = 1;
                }
            }
            else if (pascal_identifier_equals(field_id, "Substring") ||
                     pascal_identifier_equals(field_id, "IndexOf") ||
                     pascal_identifier_equals(field_id, "Contains") ||
                     pascal_identifier_equals(field_id, "StartsWith") ||
                     pascal_identifier_equals(field_id, "EndsWith"))
            {
                /* These methods take additional arguments - handled by method call transformation */
                if (FindIdent(&func_node, symtab, (char *)field_id) == 0 &&
                    func_node != NULL && func_node->hash_type == HASHTYPE_FUNCTION)
                {
                    is_string_method = 1;
                }
            }
            
            if (is_string_method && func_node != NULL)
            {
                /* Transform this record access into a function call: field_id(record_expr)
                 * This works because the parser converts S.Trim() into a method call with S prepended */
                char *func_id = strdup(field_id);
                if (func_id != NULL)
                {
                    /* Create a function call expression with the string as first argument */
                    ListNode_t *args_list = CreateListNode(record_expr, LIST_EXPR);
                    
                    expr->type = EXPR_FUNCTION_CALL;
                    memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
                    expr->expr_data.function_call_data.id = func_id;
                    expr->expr_data.function_call_data.args_expr = args_list;
                    expr->expr_data.function_call_data.mangled_id = NULL;
                    
                    return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                }
            }
            
            semcheck_error_with_context("Error on line %d, field access requires a record value.\n\n", expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return error_count + 1;
        }
        else
        {
            semcheck_error_with_context("Error on line %d, field access requires a record value.\n\n", expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return error_count + 1;
        }
    }

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_recordaccess: record_info=%p, is_class=%d\n",
            record_info, record_info ? record_type_is_class(record_info) : -1);
    }

    if (record_info == NULL)
    {
        semcheck_error_with_context("Error on line %d, unable to resolve record type for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
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
        if (record_type_is_class(record_info) || record_info->is_type_helper)
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

                    expr->record_type = (property_type == RECORD_TYPE) ? property_record : NULL;
                    expr->resolved_type = property_type;
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
                    expr->expr_data.function_call_data.id = method_id;
                    if (method_node->mangled_id != NULL)
                        expr->expr_data.function_call_data.mangled_id =
                            strdup(method_node->mangled_id);
                    else if (method_id != NULL)
                        expr->expr_data.function_call_data.mangled_id = strdup(method_id);
                    expr->expr_data.function_call_data.resolved_func = method_node;

                    if (is_static_method) {
                        expr->expr_data.function_call_data.args_expr = NULL;
                    } else {
                        struct Expression *receiver = record_expr;
                        ListNode_t *arg_node = CreateListNode(receiver, LIST_EXPR);
                        expr->expr_data.function_call_data.args_expr = arg_node;
                    }

                    /* Re-run semantic checking as a function call */
                    expr->record_type = NULL;
                    expr->resolved_type = UNKNOWN_TYPE;
                    return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                }
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
                size_arg->resolved_type = INT_TYPE;
                
                /* Create argument 2: VMT pointer */
                struct Expression *vmt_arg;
                if (is_type) {
                    /* Static class type: use address of global VMT label */
                    vmt_arg = (struct Expression *)calloc(1, sizeof(struct Expression));
                    vmt_arg->type = EXPR_VAR_ID;
                    char vmt_label[256];
                    snprintf(vmt_label, sizeof(vmt_label), "%s_VMT", expr_name);
                    vmt_arg->expr_data.id = strdup(vmt_label);
                    vmt_arg->resolved_type = POINTER_TYPE;
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
                expr->record_type = record_info;
                expr->resolved_type = POINTER_TYPE;
                
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
                    
                    /* Transform record access into an explicit method call */
                    char *method_id = (field_id != NULL) ? strdup(field_id) : NULL;

                    expr->type = EXPR_FUNCTION_CALL;
                    memset(&expr->expr_data.function_call_data, 0,
                        sizeof(expr->expr_data.function_call_data));
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
                    expr->record_type = NULL;
                    expr->resolved_type = UNKNOWN_TYPE;
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

                        char *method_id = (field_id != NULL) ? strdup(field_id) : NULL;

                        expr->type = EXPR_FUNCTION_CALL;
                        memset(&expr->expr_data.function_call_data, 0,
                            sizeof(expr->expr_data.function_call_data));
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

                        expr->record_type = NULL;
                        expr->resolved_type = UNKNOWN_TYPE;
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

        semcheck_error_with_context("Error on line %d, record field %s not found.\n", expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

FIELD_RESOLVED:
    expr->expr_data.record_access_data.field_offset = field_offset;

    expr->expr_data.record_access_data.field_offset = field_offset;

    int field_type = field_desc->type;
    struct RecordType *field_record = field_desc->nested_record;
    if (field_record != NULL)
        field_type = RECORD_TYPE;
    /* Handle inline pointer fields like bufptr: ^Char */
    if (field_desc->is_pointer)
        field_type = POINTER_TYPE;
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

    if (field_desc->is_array)
    {
        semcheck_clear_array_info(expr);
        expr->is_array_expr = 1;
        expr->array_lower_bound = field_desc->array_start;
        expr->array_upper_bound = field_desc->array_end;
        expr->array_is_dynamic = field_desc->array_is_open;
        expr->array_element_type = field_desc->array_element_type;
        if (field_desc->array_element_type_id != NULL)
        {
            expr->array_element_type_id = strdup(field_desc->array_element_type_id);
            if (expr->array_element_type == UNKNOWN_TYPE)
            {
                int resolved_type = UNKNOWN_TYPE;
                if (resolve_type_identifier(&resolved_type, symtab, expr->array_element_type_id,
                        expr->line_num) == 0)
                    expr->array_element_type = resolved_type;
            }
            expr->array_element_record_type = semcheck_lookup_record_type(symtab,
                field_desc->array_element_type_id);
        }
        else if (expr->array_element_type == RECORD_TYPE)
        {
            expr->array_element_record_type = field_record;
        }

        long long computed_size = 0;
        int size_status = 1;
        if (expr->array_element_record_type != NULL)
            size_status = sizeof_from_record(symtab, expr->array_element_record_type,
                &computed_size, 0, expr->line_num);
        else if (expr->array_element_type != UNKNOWN_TYPE ||
            expr->array_element_type_id != NULL)
            size_status = sizeof_from_type_ref(symtab, expr->array_element_type,
                expr->array_element_type_id, &computed_size, 0, expr->line_num);
        if (size_status == 0 && computed_size > 0 && computed_size <= INT_MAX)
            expr->array_element_size = (int)computed_size;

        if (expr->array_element_type != UNKNOWN_TYPE)
            field_type = expr->array_element_type;
        if (expr->array_element_record_type != NULL && field_type == RECORD_TYPE)
            field_record = expr->array_element_record_type;
    }

    struct TypeAlias *array_alias = NULL;

    if (field_desc->type_id != NULL)
    {
        int resolved_type = field_type;
        if (resolve_type_identifier(&resolved_type, symtab, field_desc->type_id, expr->line_num) != 0)
            ++error_count;
        field_type = resolved_type;

        HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, field_desc->type_id);
        if (type_node == NULL)
            type_node = semcheck_find_type_node_with_kgpc_type(symtab, field_desc->type_id);

        if (type_node != NULL)
        {
            struct RecordType *record_type = get_record_type_from_node(type_node);
            if (record_type != NULL)
                field_record = record_type;
            else
            {
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->target_type_id != NULL)
                {
                    HashNode_t *target_node =
                        semcheck_find_preferred_type_node(symtab, alias->target_type_id);
                    if (target_node != NULL)
                        field_record = get_record_type_from_node(target_node);
                }
            }

            struct TypeAlias *alias = get_type_alias_from_node(type_node);
            if (alias != NULL && alias->is_array)
                array_alias = alias;
        }

        if (field_record != NULL && field_type == UNKNOWN_TYPE)
            field_type = RECORD_TYPE;
    }

    if (field_type == UNKNOWN_TYPE && field_record == NULL && array_alias == NULL)
    {
        semcheck_error_with_context("Error on line %d, unable to resolve type for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    if (field_type == RECORD_TYPE && field_record == NULL)
    {
        semcheck_error_with_context("Error on line %d, missing record definition for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    if (array_alias != NULL)
        semcheck_set_array_info_from_alias(expr, symtab, array_alias, expr->line_num);

    if (field_type == POINTER_TYPE)
    {
        int pointer_subtype = UNKNOWN_TYPE;
        const char *pointer_subtype_id = NULL;
        if (field_desc->type_id != NULL)
        {
            HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, field_desc->type_id);
            if (type_node == NULL)
                type_node = semcheck_find_type_node_with_kgpc_type(symtab, field_desc->type_id);
            if (type_node != NULL)
            {
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->is_pointer)
                {
                    pointer_subtype = alias->pointer_type;
                    pointer_subtype_id = alias->pointer_type_id;
                }
                if (pointer_subtype == UNKNOWN_TYPE && type_node->type != NULL)
                    pointer_subtype = kgpc_type_get_pointer_subtype_tag(type_node->type);
            }
        }
        semcheck_set_pointer_info(expr, pointer_subtype, pointer_subtype_id);
    }

    expr->record_type = (field_type == RECORD_TYPE) ? field_record : NULL;
    expr->resolved_type = field_type;
    *type_return = field_type;
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
    if (target_type == UNKNOWN_TYPE && type_node != NULL && type_node->type != NULL &&
        kgpc_type_is_record(type_node->type))
    {
        target_type = RECORD_TYPE;
    }
    if (type_node == NULL)
        FindIdent(&type_node, symtab, (char *)id);
    if (target_type == UNKNOWN_TYPE && type_node != NULL && type_node->type != NULL &&
        kgpc_type_is_record(type_node->type))
    {
        target_type = RECORD_TYPE;
    }
    if (target_type == UNKNOWN_TYPE)
        target_type = semcheck_map_builtin_type_name(symtab, id);

    int is_type_identifier =
        (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) ||
        (type_node != NULL && type_node->type != NULL && kgpc_type_is_record(type_node->type)) ||
        (target_type != UNKNOWN_TYPE);
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
    {
        fprintf(stderr, "[SemCheck] try_typecast id=%s type_node=%p hash_type=%d target_type=%d\n",
            id, (void *)type_node, type_node != NULL ? type_node->hash_type : -1, target_type);
    }
    if (!is_type_identifier)
    {
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
    if (FindIdent(&func_node, symtab, expr->expr_data.typecast_data.target_type_id) == -1 ||
        func_node == NULL)
        return 1;

    if (func_node->hash_type != HASHTYPE_FUNCTION &&
        func_node->hash_type != HASHTYPE_PROCEDURE &&
        func_node->hash_type != HASHTYPE_BUILTIN_PROCEDURE)
        return 1;

    struct Expression *arg_expr = expr->expr_data.typecast_data.expr;
    expr->expr_data.typecast_data.expr = NULL;

    char *call_id = expr->expr_data.typecast_data.target_type_id;
    expr->expr_data.typecast_data.target_type_id = NULL;

    expr->type = EXPR_FUNCTION_CALL;
    memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
    expr->expr_data.function_call_data.id = call_id;
    if (arg_expr != NULL)
        expr->expr_data.function_call_data.args_expr = CreateListNode(arg_expr, LIST_EXPR);

    return semcheck_expr_main(type_return, symtab, expr, max_scope_lev, NO_MUTATE);
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
            int found = FindIdent(&inner_symbol, symtab, (char *)func_id);
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
            }
        }
    }

    if (!treated_as_proc_ref)
        semcheck_expr_main(&inner_type, symtab, inner, max_scope_lev, NO_MUTATE);

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
            if (FindIdent(&func_symbol, symtab, (char *)func_id) >= 0 &&
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
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    const char *type_id = NULL;
    if (inner_type == POINTER_TYPE && inner->pointer_subtype_id != NULL)
        type_id = inner->pointer_subtype_id;

    struct RecordType *record_info = NULL;
    if (inner_type == RECORD_TYPE)
        record_info = inner->record_type;
    else if (inner_type == POINTER_TYPE && inner->record_type != NULL)
        record_info = inner->record_type;

    semcheck_set_pointer_info(expr, inner_type, type_id);
    expr->record_type = record_info;
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
        pointed_to_type = create_primitive_type(CHAR_TYPE);
    } else if (inner_type == STRING_TYPE) {
        pointed_to_type = create_primitive_type(STRING_TYPE);
    } else if (inner_type == RECORD_TYPE && record_info != NULL) {
        pointed_to_type = create_record_type(record_info);
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
            if (FindIdent(&proc_symbol, symtab, (char *)proc_id) >= 0 &&
                proc_symbol != NULL && 
                (proc_symbol->hash_type == HASHTYPE_PROCEDURE || proc_symbol->hash_type == HASHTYPE_FUNCTION) &&
                proc_symbol->type != NULL && proc_symbol->type->kind == TYPE_KIND_PROCEDURE)
            {
                /* Use the procedure type from the symbol */
                proc_type = proc_symbol->type;
                proc_type_owned = 0; /* Shared reference */
            }
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
                expr->expr_data.addr_data.expr = NULL;
                destroy_expr(inner);
                expr->type = EXPR_ADDR_OF_PROC;
                expr->expr_data.addr_of_proc_data.procedure_symbol = proc_symbol;
            }
        }
        else if (inner->type == EXPR_FUNCTION_CALL && inner->expr_data.function_call_data.args_expr == NULL)
        {
            /* This was auto-converted from a function identifier - get the original symbol */
            const char *func_id = inner->expr_data.function_call_data.id;
            if (func_id != NULL)
            {
                HashNode_t *proc_symbol = NULL;
                if (FindIdent(&proc_symbol, symtab, (char *)func_id) >= 0 &&
                    proc_symbol != NULL && 
                    (proc_symbol->hash_type == HASHTYPE_FUNCTION || proc_symbol->hash_type == HASHTYPE_PROCEDURE))
                {
                    expr->expr_data.addr_data.expr = NULL;
                    destroy_expr(inner);
                    expr->type = EXPR_ADDR_OF_PROC;
                    expr->expr_data.addr_of_proc_data.procedure_symbol = proc_symbol;
                }
            }
        }
    }
    /* For other types, we could add more conversions here */
    
    /* Create the pointer type */
    if (pointed_to_type != NULL) {
        if (expr->resolved_kgpc_type != NULL) {
            destroy_kgpc_type(expr->resolved_kgpc_type);
        }
        expr->resolved_kgpc_type = create_pointer_type(pointed_to_type);
    }
    
    /* If we successfully converted to a procedure address, don't count inner expression errors.
     * Those errors were from trying to call the function with no arguments, which is not what we want. */
    if (converted_to_proc_addr && expr->type == EXPR_ADDR_OF_PROC)
    {
        return 0;  /* Success - ignore inner errors */
    }
    
    return error_count;
}
