/*
    Damon Gwinn
    Performs semantic checking on a given statement

    NOTE: Max scope level refers to the highest level scope we can reference a variable at
        - 0 is the current scope, 1 is the first above and so on
        - Functions can't have side effects, but they can contain procedures so this is a
            general way to define the maximum scope level

    All functions set the pointer type_return to the type the expression will return
*/

/*
    This file is the main implementation of expression semantic checking.
    Internal functions shared with other SemCheck_Expr_*.c files are declared
    in SemCheck_Expr_Internal.h.
*/

#include "SemCheck_Expr_Internal.h"

struct Expression *clone_expression(const struct Expression *expr)
{
    if (expr == NULL)
        return NULL;

    struct Expression *clone = (struct Expression *)calloc(1, sizeof(struct Expression));
    if (clone == NULL)
        return NULL;

    clone->line_num = expr->line_num;
    clone->type = expr->type;
    clone->resolved_type = expr->resolved_type;
    clone->pointer_subtype = expr->pointer_subtype;
    clone->record_type = expr->record_type;
    
    /* Clone resolved_kgpc_type if present - important for default parameter values
     * that have been transformed (e.g., scoped enum literals to integer constants) */
    if (expr->resolved_kgpc_type != NULL)
    {
        clone->resolved_kgpc_type = expr->resolved_kgpc_type;
        kgpc_type_retain(clone->resolved_kgpc_type);
    }
    
    if (expr->pointer_subtype_id != NULL)
    {
        clone->pointer_subtype_id = strdup(expr->pointer_subtype_id);
        if (clone->pointer_subtype_id == NULL)
        {
            if (clone->resolved_kgpc_type != NULL)
                destroy_kgpc_type(clone->resolved_kgpc_type);
            free(clone);
            return NULL;
        }
    }

    clone->is_array_expr = expr->is_array_expr;
    clone->array_element_type = expr->array_element_type;
    clone->array_lower_bound = expr->array_lower_bound;
    clone->array_upper_bound = expr->array_upper_bound;
    clone->array_element_size = expr->array_element_size;
    clone->array_is_dynamic = expr->array_is_dynamic;
    clone->array_element_record_type = expr->array_element_record_type;
    if (expr->array_element_type_id != NULL)
    {
        clone->array_element_type_id = strdup(expr->array_element_type_id);
        if (clone->array_element_type_id == NULL)
        {
            destroy_expr(clone);
            return NULL;
        }
    }

    if (expr->field_width != NULL)
        clone->field_width = clone_expression(expr->field_width);
    if (expr->field_precision != NULL)
        clone->field_precision = clone_expression(expr->field_precision);

    switch (expr->type)
    {
        case EXPR_VAR_ID:
            clone->expr_data.id = expr->expr_data.id != NULL ? strdup(expr->expr_data.id) : NULL;
            if (expr->expr_data.id != NULL && clone->expr_data.id == NULL)
            {
                destroy_expr(clone);
                return NULL;
            }
            break;
        case EXPR_RECORD_ACCESS:
            clone->expr_data.record_access_data.record_expr =
                clone_expression(expr->expr_data.record_access_data.record_expr);
            clone->expr_data.record_access_data.field_id =
                expr->expr_data.record_access_data.field_id != NULL ?
                    strdup(expr->expr_data.record_access_data.field_id) : NULL;
            clone->expr_data.record_access_data.field_offset =
                expr->expr_data.record_access_data.field_offset;
            if (expr->expr_data.record_access_data.field_id != NULL &&
                clone->expr_data.record_access_data.field_id == NULL)
            {
                destroy_expr(clone);
                return NULL;
            }
            break;
        case EXPR_POINTER_DEREF:
            clone->expr_data.pointer_deref_data.pointer_expr =
                clone_expression(expr->expr_data.pointer_deref_data.pointer_expr);
            break;
        case EXPR_ADDR:
            clone->expr_data.addr_data.expr =
                clone_expression(expr->expr_data.addr_data.expr);
            break;
        case EXPR_SIGN_TERM:
            clone->expr_data.sign_term = clone_expression(expr->expr_data.sign_term);
            break;
        case EXPR_ADDOP:
            clone->expr_data.addop_data.addop_type = expr->expr_data.addop_data.addop_type;
            clone->expr_data.addop_data.left_expr =
                clone_expression(expr->expr_data.addop_data.left_expr);
            clone->expr_data.addop_data.right_term =
                clone_expression(expr->expr_data.addop_data.right_term);
            break;
        case EXPR_MULOP:
            clone->expr_data.mulop_data.mulop_type = expr->expr_data.mulop_data.mulop_type;
            clone->expr_data.mulop_data.left_term =
                clone_expression(expr->expr_data.mulop_data.left_term);
            clone->expr_data.mulop_data.right_factor =
                clone_expression(expr->expr_data.mulop_data.right_factor);
            break;
        case EXPR_ARRAY_ACCESS:
            clone->expr_data.array_access_data.array_expr =
                clone_expression(expr->expr_data.array_access_data.array_expr);
            clone->expr_data.array_access_data.index_expr =
                clone_expression(expr->expr_data.array_access_data.index_expr);
            break;
        case EXPR_TYPECAST:
            clone->expr_data.typecast_data.target_type = expr->expr_data.typecast_data.target_type;
            clone->expr_data.typecast_data.expr =
                clone_expression(expr->expr_data.typecast_data.expr);
            if (expr->expr_data.typecast_data.target_type_id != NULL)
            {
                clone->expr_data.typecast_data.target_type_id =
                    strdup(expr->expr_data.typecast_data.target_type_id);
                if (clone->expr_data.typecast_data.target_type_id == NULL)
                {
                    destroy_expr(clone);
                    return NULL;
                }
            }
            break;
        case EXPR_RELOP:
            clone->expr_data.relop_data.type = expr->expr_data.relop_data.type;
            clone->expr_data.relop_data.left =
                clone_expression(expr->expr_data.relop_data.left);
            clone->expr_data.relop_data.right =
                clone_expression(expr->expr_data.relop_data.right);
            break;
        case EXPR_INUM:
            clone->expr_data.i_num = expr->expr_data.i_num;
            break;
        case EXPR_RNUM:
            clone->expr_data.r_num = expr->expr_data.r_num;
            break;
        case EXPR_STRING:
            clone->expr_data.string = expr->expr_data.string != NULL ?
                strdup(expr->expr_data.string) : NULL;
            if (expr->expr_data.string != NULL && clone->expr_data.string == NULL)
            {
                destroy_expr(clone);
                return NULL;
            }
            break;
        case EXPR_CHAR_CODE:
            clone->expr_data.char_code = expr->expr_data.char_code;
            break;
        case EXPR_BOOL:
            clone->expr_data.bool_value = expr->expr_data.bool_value;
            break;
        case EXPR_NIL:
            break;
        case EXPR_RECORD_CONSTRUCTOR:
        {
            clone->expr_data.record_constructor_data.field_count =
                expr->expr_data.record_constructor_data.field_count;
            clone->expr_data.record_constructor_data.fields_semchecked =
                expr->expr_data.record_constructor_data.fields_semchecked;

            ListNode_t *field_head = NULL;
            ListNode_t *field_tail = NULL;
            ListNode_t *cur = expr->expr_data.record_constructor_data.fields;
            while (cur != NULL)
            {
                struct RecordConstructorField *field =
                    (struct RecordConstructorField *)cur->cur;
                struct RecordConstructorField *field_clone = NULL;
                if (field != NULL)
                {
                    field_clone = (struct RecordConstructorField *)calloc(1, sizeof(*field_clone));
                    if (field_clone == NULL)
                    {
                        destroy_expr(clone);
                        return NULL;
                    }
                    field_clone->field_id = field->field_id != NULL ? strdup(field->field_id) : NULL;
                    field_clone->value = clone_expression(field->value);
                    field_clone->field_offset = field->field_offset;
                    field_clone->field_type = field->field_type;
                    field_clone->field_type_id = field->field_type_id != NULL ?
                        strdup(field->field_type_id) : NULL;
                    field_clone->field_record_type = field->field_record_type;
                    field_clone->field_is_array = field->field_is_array;
                    field_clone->array_start = field->array_start;
                    field_clone->array_end = field->array_end;
                    field_clone->array_element_type = field->array_element_type;
                    field_clone->array_element_type_id = field->array_element_type_id != NULL ?
                        strdup(field->array_element_type_id) : NULL;
                    field_clone->array_is_open = field->array_is_open;
                    field_clone->array_element_record_type = field->array_element_record_type;
                    if ((field->field_id != NULL && field_clone->field_id == NULL) ||
                        (field->field_type_id != NULL && field_clone->field_type_id == NULL) ||
                        (field->array_element_type_id != NULL &&
                         field_clone->array_element_type_id == NULL) ||
                        (field->value != NULL && field_clone->value == NULL))
                    {
                        destroy_expr(clone);
                        return NULL;
                    }
                }

                ListNode_t *node = CreateListNode(field_clone, LIST_UNSPECIFIED);
                if (node == NULL)
                {
                    if (field_clone != NULL)
                    {
                        if (field_clone->value != NULL)
                            destroy_expr(field_clone->value);
                        free(field_clone->field_id);
                        free(field_clone->field_type_id);
                        free(field_clone->array_element_type_id);
                        free(field_clone);
                    }
                    destroy_expr(clone);
                    return NULL;
                }
                if (field_head == NULL)
                {
                    field_head = node;
                    field_tail = node;
                }
                else
                {
                    field_tail->next = node;
                    field_tail = node;
                }
                cur = cur->next;
            }
            clone->expr_data.record_constructor_data.fields = field_head;
            break;
        }
        case EXPR_SET:
        {
            /* Clone set expression: copy bitmask, is_constant, and clone elements list */
            clone->expr_data.set_data.bitmask = expr->expr_data.set_data.bitmask;
            clone->expr_data.set_data.is_constant = expr->expr_data.set_data.is_constant;
            
            /* Clone the elements list if present */
            ListNode_t *elem_head = NULL;
            ListNode_t *elem_tail = NULL;
            ListNode_t *cur = expr->expr_data.set_data.elements;
            while (cur != NULL)
            {
                struct Expression *elem_expr = (struct Expression *)cur->cur;
                struct Expression *elem_clone = clone_expression(elem_expr);
                /* NULL elements are allowed in empty sets */
                
                ListNode_t *node = CreateListNode(elem_clone, LIST_EXPR);
                if (node == NULL)
                {
                    if (elem_clone != NULL)
                        destroy_expr(elem_clone);
                    destroy_expr(clone);
                    return NULL;
                }
                
                if (elem_head == NULL)
                {
                    elem_head = node;
                    elem_tail = node;
                }
                else
                {
                    elem_tail->next = node;
                    elem_tail = node;
                }
                cur = cur->next;
            }
            clone->expr_data.set_data.elements = elem_head;
            break;
        }
        default:
            destroy_expr(clone);
            return NULL;
    }

    return clone;
}

/* Semantic check on a normal expression */
int semcheck_expr(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    assert(type_return != NULL);
    if (expr == NULL)
        return 0;

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] Checking expr type=%d\n", expr->type);
        if (expr->type == EXPR_VAR_ID) {
            fprintf(stderr, "[SemCheck]   Identifier: %s\n", expr->expr_data.id);
        }
    }

    return semcheck_expr_main(type_return, symtab, expr, max_scope_lev, mutating);
}

/* Semantic check on a function expression (no side effects allowed) */
int semcheck_expr_func(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int mutating)
{
    assert(type_return != NULL);
    return semcheck_expr_main(type_return, symtab, expr, 0, mutating);
}

/* Phase 3 Step 3: Resolve KgpcType from an expression
 * This function performs semantic checking and returns the KgpcType of the expression.
 * This is the bridge between the legacy int-based type system and the new KgpcType system.
 * 
 * Implementation strategy:
 * 1. For simple cases (var IDs, function calls), we can directly get KgpcType from symbol table
 * 2. For complex expressions, we use semcheck_expr_main to get the type tag, then convert to KgpcType
 * 3. We also check expr->resolved_kgpc_type if it was previously computed
 * 
 * Returns NULL if type cannot be resolved.
 * If owns_type is not NULL, it will be set to:
 *   - 0 if the caller does NOT own the type (shared reference from symbol table)
 *   - 1 if the caller OWNS the type (must free it with destroy_kgpc_type)
 */
KgpcType* semcheck_resolve_expression_kgpc_type(SymTab_t *symtab, struct Expression *expr,
    int max_scope_lev, int mutating, int *owns_type)
{
    if (symtab == NULL || expr == NULL)
        return NULL;
    
    /* Default: caller owns the type we create */
    if (owns_type != NULL)
        *owns_type = 1;
    
    /* Try to get KgpcType directly from the expression for specific cases */
    switch (expr->type)
    {
        case EXPR_VAR_ID:
        {
            /* For variable IDs, we can get the KgpcType directly from the symbol table */
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, expr->expr_data.id) != -1 && node != NULL)
            {
                if (node->hash_type == HASHTYPE_FUNCTION && mutating != NO_MUTATE &&
                    node->type != NULL)
                {
                    KgpcType *ret_type = kgpc_type_get_return_type(node->type);
                    if (ret_type != NULL)
                    {
                        if (owns_type != NULL)
                            *owns_type = 0;
                        return ret_type;
                    }
                }

                if (node->type != NULL)
                {
                    /* Return a shared reference - caller doesn't own it */
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return node->type;
                }
            }
            break;
        }
        
        case EXPR_FUNCTION_CALL:
        {
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr->expr_data.function_call_data.id != NULL &&
                strcasecmp(expr->expr_data.function_call_data.id, "Create") == 0) {
                fprintf(stderr, "[SemCheck] semcheck_resolve_expression_kgpc_type for Create:\n");
                fprintf(stderr, "[SemCheck]   expr=%p\n", (void*)expr);
                fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type=%p\n", (void*)expr->resolved_kgpc_type);
                if (expr->resolved_kgpc_type != NULL) {
                    fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type->kind=%d\n", expr->resolved_kgpc_type->kind);
                }
            }
            
            /* First, try the cached resolved_kgpc_type if available */
            if (expr->resolved_kgpc_type != NULL)
            {
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr->expr_data.function_call_data.id != NULL &&
                    strcasecmp(expr->expr_data.function_call_data.id, "Create") == 0) {
                    fprintf(stderr, "[SemCheck]   Returning cached resolved_kgpc_type\n");
                }
                if (owns_type != NULL)
                    *owns_type = 0;  /* Shared reference */
                return expr->resolved_kgpc_type;
            }
            
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr->expr_data.function_call_data.id != NULL &&
                strcasecmp(expr->expr_data.function_call_data.id, "Create") == 0) {
                fprintf(stderr, "[SemCheck]   resolved_kgpc_type is NULL, trying other paths\n");
            }
            
            /* Prefer cached call info populated during semantic checking */
            if (expr->expr_data.function_call_data.is_call_info_valid &&
                expr->expr_data.function_call_data.call_kgpc_type != NULL)
            {
                KgpcType *call_type = expr->expr_data.function_call_data.call_kgpc_type;
                if (call_type->kind == TYPE_KIND_PROCEDURE &&
                    call_type->info.proc_info.return_type != NULL)
                {
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return call_type->info.proc_info.return_type;
                }
            }

            if (expr->expr_data.function_call_data.id != NULL && symtab != NULL)
            {
                HashNode_t *func_node = NULL;
                if (FindIdent(&func_node, symtab,
                        expr->expr_data.function_call_data.id) >= 0 &&
                    func_node != NULL && func_node->type != NULL &&
                    func_node->type->kind == TYPE_KIND_PROCEDURE &&
                    func_node->type->info.proc_info.return_type != NULL)
                {
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return func_node->type->info.proc_info.return_type;
                }
            }
            break;
        }
        
        case EXPR_RECORD_ACCESS:
        {
            /* For record field access, we need to resolve the type of the record,
             * then look up the field's type within that record.
             */
            struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
            const char *field_name = expr->expr_data.record_access_data.field_id;
            
            if (record_expr != NULL && field_name != NULL)
            {
                /* Resolve the record expression's type */
                int record_type_owned = 0;
                KgpcType *record_type = semcheck_resolve_expression_kgpc_type(symtab, record_expr, 
                                                                          max_scope_lev, mutating, &record_type_owned);
                
                if (record_type != NULL && record_type->kind == TYPE_KIND_RECORD)
                {
                    /* Look up the field in the record type */
                    struct RecordType *record_info = record_type->info.record_info;
                    if (record_info != NULL)
                    {
                    ListNode_t *field_cursor = record_info->fields;
                    while (field_cursor != NULL)
                    {
                        if (field_cursor->type == LIST_RECORD_FIELD)
                        {
                            struct RecordField *field = (struct RecordField *)field_cursor->cur;
                            if (field != NULL && record_field_is_hidden(field))
                            {
                                field_cursor = field_cursor->next;
                                continue;
                            }
                            if (field != NULL)
                            {
                                if (field->name != NULL && pascal_identifier_equals(field->name, field_name))
                                {
                                    /* Found the field, resolve its type */
                                    KgpcType *field_type = NULL;
                                    
                                    if (field->type_id != NULL)
                                    {
                                        /* User-defined type - look up in symbol table */
                                        HashNode_t *type_node =
                                            semcheck_find_preferred_type_node(symtab, field->type_id);
                                        if (type_node != NULL)
                                        {
                                            if (owns_type != NULL)
                                                *owns_type = 0;
                                            field_type = type_node->type;
                                        }
                                    }
                                    else if (field->nested_record != NULL)
                                    {
                                        /* Nested record type */
                                        if (owns_type != NULL)
                                            *owns_type = 1;
                                        field_type = create_record_type(field->nested_record);
                                    }
                                    else if (field->is_array)
                                    {
                                        /* Array field */
                                        KgpcType *element_type = NULL;
                                        int element_type_borrowed = 0;
                                        if (field->array_element_type_id != NULL)
                                        {
                                            HashNode_t *elem_type_node =
                                                semcheck_find_preferred_type_node(symtab, field->array_element_type_id);
                                            if (elem_type_node != NULL)
                                            {
                                                element_type = elem_type_node->type;
                                                element_type_borrowed = 1;
                                            }
                                        }
                                        else if (field->array_element_type != UNKNOWN_TYPE)
                                        {
                                            if (owns_type != NULL)
                                                *owns_type = 1;
                                            element_type = create_primitive_type(field->array_element_type);
                                        }

                                        if (element_type != NULL)
                                        {
                                            if (owns_type != NULL)
                                                *owns_type = 1;
                                            /* CRITICAL: Retain element_type if borrowed from symbol table
                                             * since create_array_type takes ownership. */
                                            if (element_type_borrowed)
                                                kgpc_type_retain(element_type);
                                            field_type = create_array_type(element_type, field->array_start, field->array_end);
                                        }
                                    }
                                    else if (field->is_pointer)
                                    {
                                        /* Inline pointer field like ^Char */
                                        KgpcType *pointee_type = NULL;
                                        if (field->pointer_type_id != NULL)
                                        {
                                            HashNode_t *pointee_node =
                                                semcheck_find_preferred_type_node(symtab, field->pointer_type_id);
                                            if (pointee_node != NULL && pointee_node->type != NULL)
                                            {
                                                kgpc_type_retain(pointee_node->type);
                                                pointee_type = pointee_node->type;
                                            }
                                        }
                                        if (pointee_type == NULL && field->pointer_type != UNKNOWN_TYPE)
                                        {
                                            pointee_type = create_primitive_type(field->pointer_type);
                                        }
                                        if (owns_type != NULL)
                                            *owns_type = 1;
                                        field_type = create_pointer_type(pointee_type);
                                    }
                                    else
                                    {
                                        /* Primitive type */
                                        if (owns_type != NULL)
                                            *owns_type = 1;
                                        field_type = create_primitive_type(field->type);
                                    }
                                    
                                    /* Clean up record type if we owned it */
                                    if (record_type_owned)
                                        destroy_kgpc_type(record_type);
                                    
                                    /* Return the field type */
                                    return field_type;
                                }
                            }
                        }
                        field_cursor = field_cursor->next;
                    }
                    }
                }
                
                /* Clean up record type if we owned it */
                if (record_type_owned)
                    destroy_kgpc_type(record_type);
            }
            break;
        }

        case EXPR_ARRAY_ACCESS:
        {
            struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
            const char *alias_id = NULL;

            if (array_expr != NULL && array_expr->is_array_expr &&
                array_expr->array_element_type_id != NULL)
            {
                alias_id = array_expr->array_element_type_id;
            }
            else if (expr->array_element_type_id != NULL)
            {
                alias_id = expr->array_element_type_id;
            }

            if (alias_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] Resolving array element type alias: %s\n", alias_id);
                }
                if (FindIdent(&type_node, symtab, (char *)alias_id) != -1 &&
                    type_node != NULL && type_node->type != NULL)
                {
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return type_node->type;
                }
            }

            /* Handle pointer indexing result (e.g., p[i] where p: PPAnsiChar) */
            if (expr->pointer_subtype != UNKNOWN_TYPE || expr->pointer_subtype_id != NULL)
            {
                /* The result of array access is a pointer type */
                KgpcType *points_to = NULL;
                
                /* Try to get the pointed-to type from the symbol table */
                if (expr->pointer_subtype_id != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, (char *)expr->pointer_subtype_id) != -1 &&
                        type_node != NULL && type_node->type != NULL)
                    {
                        kgpc_type_retain(type_node->type);
                        points_to = type_node->type;
                    }
                }
                
                /* If not found, try to create from primitive type tag */
                if (points_to == NULL && expr->pointer_subtype != UNKNOWN_TYPE)
                {
                    points_to = create_primitive_type(expr->pointer_subtype);
                    /* Note: create_primitive_type uses assert and cannot return NULL,
                     * but we pass NULL to create_pointer_type below which is valid
                     * (creates a generic/untyped pointer). */
                }
                
                /* Create a pointer type pointing to the target.
                 * points_to can be NULL here, which creates an untyped pointer (like Pointer). */
                if (owns_type != NULL)
                    *owns_type = 1;
                return create_pointer_type(points_to);
            }

            if (expr->is_array_expr)
            {
                KgpcType *element_type = NULL;
                if (expr->array_element_record_type != NULL)
                {
                    element_type = create_record_type(expr->array_element_record_type);
                }
                else if (expr->array_element_type == ARRAY_OF_CONST_TYPE)
                {
                    struct RecordType *tvarrec_record = semcheck_lookup_record_type(symtab, "TVarRec");
                    if (tvarrec_record != NULL)
                        element_type = create_record_type(tvarrec_record);
                }
                else if (expr->array_element_type != UNKNOWN_TYPE)
                {
                    element_type = create_primitive_type(expr->array_element_type);
                }

                if (element_type != NULL)
                {
                    if (owns_type != NULL)
                        *owns_type = 1;
                    return element_type;
                }
            }
            break;
        }

        case EXPR_ARRAY_LITERAL:
        {
            if (expr->array_element_type == UNKNOWN_TYPE)
                break;

            KgpcType *element_type = create_primitive_type(expr->array_element_type);
            if (element_type == NULL)
                break;

            int end_index = expr->array_upper_bound;
            if (end_index < expr->array_lower_bound)
                end_index = expr->array_lower_bound - 1;

            KgpcType *array_type = create_array_type(element_type,
                expr->array_lower_bound, end_index);
            return array_type;
        }

        case EXPR_POINTER_DEREF:
        {
            /* For pointer dereference, resolve the pointer expression's type,
             * then return what it points to.
             */
            struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
            if (pointer_expr != NULL)
            {
                int ptr_type_owned = 0;
                KgpcType *ptr_type = semcheck_resolve_expression_kgpc_type(symtab, pointer_expr, 
                                                                          max_scope_lev, mutating, &ptr_type_owned);
                if (ptr_type != NULL && ptr_type->kind == TYPE_KIND_POINTER)
                {
                    KgpcType *deref_type = ptr_type->info.points_to;
                    
                    /* Clean up the pointer type if we owned it */
                    if (ptr_type_owned)
                        destroy_kgpc_type(ptr_type);
                    
                    /* Return what the pointer points to - caller doesn't own it (it's part of the pointer type) */
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return deref_type;
                }
                
                /* Clean up if we failed */
                if (ptr_type_owned && ptr_type != NULL)
                    destroy_kgpc_type(ptr_type);
            }
            break;
        }
        case EXPR_TYPECAST:
        {
            const char *target_id = expr->expr_data.typecast_data.target_type_id;
            if (target_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindIdent(&type_node, symtab, (char *)target_id) >= 0 &&
                    type_node != NULL && type_node->type != NULL &&
                    type_node->type->kind == TYPE_KIND_PROCEDURE)
                {
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return type_node->type;
                }
            }
            break;
        }
        
        default:
            break;
    }
    
    /* Check if the expression already has a resolved KgpcType (e.g., from semcheck_expr_main) */
    if (expr->resolved_kgpc_type != NULL)
    {
        /* Use the existing KgpcType - caller doesn't own it (it belongs to the expression) */
        if (owns_type != NULL)
            *owns_type = 0;
        return expr->resolved_kgpc_type;
    }
    
    /* For all other cases or if direct resolution failed, use semcheck_expr_main
     * to get the type tag, then convert to KgpcType */
    int type_tag = UNKNOWN_TYPE;
    int result = semcheck_expr_main(&type_tag, symtab, expr, max_scope_lev, mutating);
    
    if (result != 0 || type_tag == UNKNOWN_TYPE)
        return NULL;

    /* Check if semcheck_expr_main populated resolved_kgpc_type */
    if (expr->resolved_kgpc_type != NULL)
    {
        if (owns_type != NULL)
            *owns_type = 0;
        return expr->resolved_kgpc_type;
    }
    
    /* Create a KgpcType from the type tag - caller owns this */
    if (owns_type != NULL)
        *owns_type = 1;
    
    return create_primitive_type(type_tag);
}

/* Main semantic checking */
int semcheck_expr_main(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    if (expr != NULL && expr->type == EXPR_VAR_ID && getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_expr_main: Checking identifier: %s\n", expr->expr_data.id);
        HashNode_t *ident_node = NULL;
        int index = FindIdent(&ident_node, symtab, expr->expr_data.id);
        if (index == -1) {
            fprintf(stderr, "[SemCheck]   FindIdent failed\n");
        } else {
            fprintf(stderr, "[SemCheck]   Found identifier: %s (index %d)\n", ident_node->id, index);
        }
    }

    if (expr == NULL)
        return 0;

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_expr_main: expr type=%d\n", expr->type);
        if (expr->type == EXPR_VAR_ID) {
            fprintf(stderr, "[SemCheck]   Identifier: %s\n", expr->expr_data.id);
        }
    }

    int return_val;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(type_return != NULL);

    return_val = 0;
    expr->resolved_type = UNKNOWN_TYPE;
    switch(expr->type)
    {
        case EXPR_RELOP:
            return_val += semcheck_relop(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_SIGN_TERM:
            return_val += semcheck_signterm(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_ADDOP:
            return_val += semcheck_addop(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_MULOP:
            return_val += semcheck_mulop(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_VAR_ID:
            return_val += semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_ARRAY_ACCESS:
            return_val += semcheck_arrayaccess(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_RECORD_ACCESS:
            return_val += semcheck_recordaccess(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_FUNCTION_CALL:
            return_val += semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_POINTER_DEREF:
            return_val += semcheck_pointer_deref(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_ADDR_OF_PROC:
        {
            *type_return = POINTER_TYPE;
            expr->resolved_type = POINTER_TYPE;
            if (expr->resolved_kgpc_type == NULL)
            {
                KgpcType *proc_type = NULL;
                if (expr->expr_data.addr_of_proc_data.procedure_symbol != NULL &&
                    expr->expr_data.addr_of_proc_data.procedure_symbol->type != NULL)
                    proc_type = expr->expr_data.addr_of_proc_data.procedure_symbol->type;

                if (proc_type != NULL)
                {
                    kgpc_type_retain(proc_type);
                    expr->resolved_kgpc_type = create_pointer_type(proc_type);
                }
                else
                {
                    expr->resolved_kgpc_type = create_pointer_type(NULL);
                }
            }
            break;
        }
        case EXPR_ADDR:
            return_val += semcheck_addressof(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_TYPECAST:
            return_val += semcheck_typecast(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_IS:
            return_val += semcheck_is_expr(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_AS:
            return_val += semcheck_as_expr(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        /*** BASE CASES ***/
        case EXPR_INUM:
            if (expr->expr_data.i_num > INT_MAX || expr->expr_data.i_num < INT_MIN)
            {
                *type_return = INT64_TYPE;
                /* For values that don't fit in 32 bits, use INT64_TYPE (true 64-bit integer) */
                if (expr->resolved_kgpc_type != NULL)
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                expr->resolved_kgpc_type = create_primitive_type_with_size(INT64_TYPE, 8);
            }
            else
                *type_return = INT_TYPE;
            break;

        case EXPR_RNUM:
            *type_return = REAL_TYPE;
            break;

        case EXPR_STRING:
            if (pascal_frontend_default_shortstring())
                *type_return = SHORTSTRING_TYPE;
            else
                *type_return = STRING_TYPE;
            expr->resolved_type = *type_return;
            break;

        case EXPR_CHAR_CODE:
            *type_return = CHAR_TYPE;
            break;

        case EXPR_BOOL:
            *type_return = BOOL;
            break;
        case EXPR_NIL:
            *type_return = POINTER_TYPE;
            semcheck_clear_pointer_info(expr);
            /* Create a proper KgpcType for nil with points_to = NULL */
            if (expr->resolved_kgpc_type != NULL) {
                destroy_kgpc_type(expr->resolved_kgpc_type);
            }
            expr->resolved_kgpc_type = create_pointer_type(NULL);
            break;
        case EXPR_SET:
            *type_return = SET_TYPE;
            break;

        case EXPR_ARRAY_LITERAL:
        {
            if (expr->array_element_type == UNKNOWN_TYPE &&
                expr->array_element_type_id == NULL)
            {
                semcheck_error_with_context("Error on line %d, unable to infer type for array literal.\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            if (!expr->expr_data.array_literal_data.elements_semchecked)
            {
                int arr_err = semcheck_typecheck_array_literal(expr, symtab, max_scope_lev,
                    expr->array_element_type, expr->array_element_type_id, expr->line_num);
                if (arr_err != 0)
                {
                    *type_return = UNKNOWN_TYPE;
                    return arr_err;
                }
            }
            expr->is_array_expr = 1;
            expr->array_is_dynamic = 1;
            expr->resolved_type = POINTER_TYPE;
            *type_return = POINTER_TYPE;
            return 0;
        }

        case EXPR_RECORD_CONSTRUCTOR:
        {
            if (expr->record_type == NULL)
            {
                semcheck_error_with_context("Error on line %d, unable to infer record type for constructor.\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            if (!expr->expr_data.record_constructor_data.fields_semchecked)
            {
                int rc_err = semcheck_typecheck_record_constructor(expr, symtab, max_scope_lev,
                    expr->record_type, expr->line_num);
                if (rc_err != 0)
                {
                    *type_return = UNKNOWN_TYPE;
                    return rc_err;
                }
            }
            expr->resolved_type = RECORD_TYPE;
            *type_return = RECORD_TYPE;
            return 0;
        }

        case EXPR_ANONYMOUS_FUNCTION:
        case EXPR_ANONYMOUS_PROCEDURE:
        {
            /* Anonymous methods are treated as procedure/function references.
             * Create a KgpcType for better type checking.
             */
            
            /* Set the type to PROCEDURE to represent a function/procedure reference */
            *type_return = PROCEDURE;
            
            /* Create a KgpcType with parameter and return type information */
            ListNode_t *params = expr->expr_data.anonymous_method_data.parameters;
            KgpcType *return_type = NULL;
            
            /* For functions, resolve the return type */
            if (expr->type == EXPR_ANONYMOUS_FUNCTION) {
                char *return_type_id = expr->expr_data.anonymous_method_data.return_type_id;
                if (return_type_id != NULL) {
                    /* Try to look up the type in the symbol table */
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, return_type_id) >= 0 && type_node != NULL) {
                        /* Use the KgpcType from the symbol table if available */
                        if (type_node->type != NULL) {
                            return_type = type_node->type;
                        }
                    }
                    
                    /* If not found in symbol table, it might be a built-in type */
                    if (return_type == NULL) {
                        /* Try to map the type name to a primitive type */
                        int type_tag = UNKNOWN_TYPE;
                        if (strcasecmp(return_type_id, "integer") == 0)
                            type_tag = INT_TYPE;
                        else if (strcasecmp(return_type_id, "longint") == 0)
                            type_tag = LONGINT_TYPE;
                        else if (strcasecmp(return_type_id, "real") == 0 || strcasecmp(return_type_id, "double") == 0)
                            type_tag = REAL_TYPE;
                        else if (strcasecmp(return_type_id, "string") == 0)
                            type_tag = STRING_TYPE;
                        else if (strcasecmp(return_type_id, "char") == 0)
                            type_tag = CHAR_TYPE;
                        else if (strcasecmp(return_type_id, "boolean") == 0)
                            type_tag = BOOL;
                        
                        if (type_tag != UNKNOWN_TYPE) {
                            return_type = create_primitive_type(type_tag);
                            /* Note: This creates a new KgpcType that will be owned by proc_type */
                        }
                    }
                }
            }
            /* For procedures, return_type remains NULL */
            
            /* Create the procedure type to store in the expression */
            KgpcType *proc_type = create_procedure_type(params, return_type);
            if (proc_type != NULL) {
                expr->resolved_kgpc_type = proc_type;
            }
            
            /* Semantic check the anonymous function body */
            /* We need to create a new scope with parameters and return variable */
            if (expr->expr_data.anonymous_method_data.body != NULL)
            {
                PushScope(symtab);
                
                /* Add parameters to the scope */
                if (params != NULL)
                {
                    ListNode_t *param = params;
                    while (param != NULL)
                    {
                        if (param->type == LIST_TREE && param->cur != NULL)
                        {
                            Tree_t *param_tree = (Tree_t *)param->cur;
                            if (param_tree->type == TREE_VAR_DECL)
                            {
                                /* Add parameter as a variable */
                                ListNode_t *ids = param_tree->tree_data.var_decl_data.ids;
                                while (ids != NULL)
                                {
                                    char *param_id = (char *)ids->cur;
                                    int param_type = param_tree->tree_data.var_decl_data.type;
                                    KgpcType *param_kgpc_type = NULL;
                                    
                                    /* Try to resolve parameter type */
                                    if (param_tree->tree_data.var_decl_data.type_id != NULL)
                                    {
                                        HashNode_t *type_node = NULL;
                                        if (FindIdent(&type_node, symtab, param_tree->tree_data.var_decl_data.type_id) >= 0 &&
                                            type_node != NULL && type_node->type != NULL)
                                        {
                                            param_kgpc_type = type_node->type;
                                        }
                                    }
                                    
                                    if (param_kgpc_type == NULL && param_type != UNKNOWN_TYPE)
                                    {
                                        param_kgpc_type = create_primitive_type(param_type);
                                    }
                                    
                                    PushVarOntoScope_Typed(symtab, param_id, param_kgpc_type);
                                    ids = ids->next;
                                }
                            }
                        }
                        param = param->next;
                    }
                }
                
                /* For functions, add the return variable */
                if (expr->type == EXPR_ANONYMOUS_FUNCTION && expr->expr_data.anonymous_method_data.generated_name != NULL)
                {
                    /* Add the function name as the return variable */
                    PushFuncRetOntoScope_Typed(symtab, expr->expr_data.anonymous_method_data.generated_name, return_type);
                    
                    /* Also add "Result" as an alias in the current scope */
                    HashNode_t *result_check = NULL;
                    HashTable_t *cur_hash = (HashTable_t *)symtab->stack_head->cur;
                    result_check = (cur_hash != NULL) ? FindIdentInTable(cur_hash, "Result") : NULL;
                    if (result_check == NULL)
                    {
                        PushFuncRetOntoScope_Typed(symtab, "Result", return_type);
                    }
                }
                
                /* Semantic check the body statement */
                return_val += semcheck_stmt(symtab, expr->expr_data.anonymous_method_data.body, max_scope_lev);
                
                PopScope(symtab);
            }
            
            break;
        }

        default:
            assert(0 && "Bad type in semcheck_expr_main!");
            break;
    }

    expr->resolved_type = *type_return;
    return return_val;
}
