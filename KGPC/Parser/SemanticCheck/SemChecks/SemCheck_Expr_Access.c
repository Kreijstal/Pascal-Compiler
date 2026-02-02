/*
    SemCheck_Expr_Access.c - Array access and function call semantic checks

    This file contains semantic checking for:
    - Array element access (arr[i])
    - Function/procedure calls

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"
#include <time.h>

#define FUNCCALL_TIMINGS_ENABLED() (getenv("KGPC_DEBUG_FUNCCALL_TIMINGS") != NULL)

static double funccall_now_ms(void) {
    return (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
}


/** ARRAY_ACCESS **/
int semcheck_arrayaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val = 0;
    int index_type = UNKNOWN_TYPE;
    int element_type = UNKNOWN_TYPE;
    struct Expression *array_expr;
    struct Expression *access_expr;

    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_ARRAY_ACCESS);

    semcheck_clear_pointer_info(expr);
    semcheck_clear_array_info(expr);
    expr->record_type = NULL;

    array_expr = expr->expr_data.array_access_data.array_expr;
    access_expr = expr->expr_data.array_access_data.index_expr;

    if (array_expr == NULL)
    {
        semcheck_error_with_context("Error on line %d, array access requires a base expression.\n\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    if (array_expr->type == EXPR_VAR_ID)
    {
        HashNode_t *array_node = NULL;
        if (FindIdent(&array_node, symtab, array_expr->expr_data.id) == -1)
        {
            int property_result = semcheck_try_indexed_property_getter(type_return, symtab,
                expr, max_scope_lev, mutating);
            if (property_result >= 0)
                return return_val + property_result;
        }
    }

    int base_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_legacy_tag(&base_type, symtab, array_expr, max_scope_lev, mutating);

    int base_is_string = (is_string_type(base_type) && !array_expr->is_array_expr);
    /* Only treat as pointer indexing if NOT an array expression - for arrays of pointers,
     * we want to go through the array path to properly handle element type info */
    int base_is_pointer = (base_type == POINTER_TYPE && !array_expr->is_array_expr);
    
    if (!array_expr->is_array_expr && !base_is_string && !base_is_pointer)
    {
        int property_result = semcheck_try_indexed_property_getter(type_return, symtab,
            expr, max_scope_lev, mutating);
        if (property_result >= 0)
            return return_val + property_result;

        semcheck_error_with_context("Error on line %d, expression is not indexable as an array.\n\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return return_val + 1;
    }

    if (base_is_string)
    {
        element_type = CHAR_TYPE;
    }
    else if (base_is_pointer)
    {
        /* Pointer indexing: p[i] is equivalent to (p+i)^ */
        /* Get the type that the pointer points to */
        element_type = array_expr->pointer_subtype;
        
        if (element_type == UNKNOWN_TYPE && array_expr->pointer_subtype_id != NULL)
        {
            int resolved_type = UNKNOWN_TYPE;
            if (resolve_type_identifier(&resolved_type, symtab, array_expr->pointer_subtype_id,
                    expr->line_num) == 0)
                element_type = resolved_type;
        }
        
        if (element_type == UNKNOWN_TYPE && array_expr->record_type != NULL)
            element_type = RECORD_TYPE;
        
        /* Copy pointer target type info to result */
        if (element_type == POINTER_TYPE && array_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindIdent(&type_node, symtab, array_expr->pointer_subtype_id) != -1 && type_node != NULL)
            {
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->is_pointer)
                {
                    expr->pointer_subtype = alias->pointer_type;
                    if (alias->pointer_type_id != NULL)
                        expr->pointer_subtype_id = strdup(alias->pointer_type_id);
                    if (alias->pointer_type == RECORD_TYPE && alias->pointer_type_id != NULL)
                        expr->record_type = semcheck_lookup_record_type(symtab, alias->pointer_type_id);
                }
            }
        }
        
        if (element_type == RECORD_TYPE && array_expr->record_type != NULL)
        {
            expr->record_type = array_expr->record_type;
        }
    }
    else
    {
        element_type = array_expr->array_element_type;
        if (element_type == UNKNOWN_TYPE && array_expr->array_element_type_id != NULL)
        {
            int resolved_type = UNKNOWN_TYPE;
            if (resolve_type_identifier(&resolved_type, symtab, array_expr->array_element_type_id,
                    expr->line_num) == 0)
                element_type = resolved_type;
        }
        if (element_type == UNKNOWN_TYPE && array_expr->array_element_record_type != NULL)
            element_type = RECORD_TYPE;
        if (element_type == ARRAY_OF_CONST_TYPE)
        {
            element_type = RECORD_TYPE;
            expr->record_type = semcheck_lookup_record_type(symtab, "TVarRec");
        }

        if (array_expr->array_element_type_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindIdent(&type_node, symtab, array_expr->array_element_type_id) != -1 && type_node != NULL)
            {
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->is_array)
                {
                    semcheck_set_array_info_from_alias(expr, symtab, alias, expr->line_num);
                }
            }
        }

        if (element_type == POINTER_TYPE)
        {
            int pointer_subtype = UNKNOWN_TYPE;
            const char *pointer_subtype_id = NULL;
            struct RecordType *pointer_record = NULL;

            if (array_expr->array_element_type_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindIdent(&type_node, symtab, array_expr->array_element_type_id) != -1 &&
                    type_node != NULL)
                {
                    struct TypeAlias *alias = get_type_alias_from_node(type_node);
                    if (alias != NULL && alias->is_pointer)
                    {
                        pointer_subtype = alias->pointer_type;
                        pointer_subtype_id = alias->pointer_type_id;
                        if (alias->pointer_type == RECORD_TYPE && alias->pointer_type_id != NULL)
                            pointer_record = semcheck_lookup_record_type(symtab, alias->pointer_type_id);
                    }
                }
            }

            if (pointer_subtype_id == NULL && array_expr->array_element_type_id != NULL &&
                (array_expr->array_element_type_id[0] == 'P' ||
                 array_expr->array_element_type_id[0] == 'p') &&
                array_expr->array_element_type_id[1] != '\0')
            {
                pointer_subtype_id = array_expr->array_element_type_id + 1;
                if (pointer_subtype == UNKNOWN_TYPE)
                {
                    int mapped = semcheck_map_builtin_type_name(symtab, pointer_subtype_id);
                    if (mapped != UNKNOWN_TYPE)
                        pointer_subtype = mapped;
                }
            }

            if (pointer_subtype == UNKNOWN_TYPE && array_expr->resolved_kgpc_type != NULL &&
                kgpc_type_is_array(array_expr->resolved_kgpc_type))
            {
                KgpcType *elem_type = kgpc_type_get_array_element_type(array_expr->resolved_kgpc_type);
                if (elem_type != NULL && elem_type->kind == TYPE_KIND_POINTER)
                {
                    int mapped = kgpc_type_get_pointer_subtype_tag(elem_type);
                    if (mapped != UNKNOWN_TYPE)
                        pointer_subtype = mapped;
                }
            }

            semcheck_set_pointer_info(expr, pointer_subtype, pointer_subtype_id);
            if (pointer_subtype == RECORD_TYPE)
                expr->record_type = pointer_record;
            else
                expr->record_type = NULL;
        }
        else if (element_type == RECORD_TYPE)
        {
            expr->record_type = array_expr->array_element_record_type;
            if (expr->record_type == NULL && array_expr->array_element_type_id != NULL)
                expr->record_type = semcheck_lookup_record_type(symtab, array_expr->array_element_type_id);
        }
    }

    return_val += semcheck_expr_legacy_tag(&index_type, symtab, access_expr, max_scope_lev, NO_MUTATE);
    if (!is_ordinal_type(index_type))
    {
        semcheck_error_with_context("Error on line %d, expected ordinal type (integer, char, boolean, or enum) in array index expression!\n\n",
            expr->line_num);
        ++return_val;
    }

    if (element_type == UNKNOWN_TYPE)
        element_type = LONGINT_TYPE;

    *type_return = element_type;
    return return_val;
}

/* Helper to resolve the actual type tag from a TREE_VAR_DECL parameter declaration */
int resolve_param_type(Tree_t *decl, SymTab_t *symtab)
{
    assert(decl != NULL);
    assert(symtab != NULL);
    
    int type_tag = UNKNOWN_TYPE;
    char *type_id = NULL;

    if (decl->type == TREE_VAR_DECL)
    {
        type_tag = decl->tree_data.var_decl_data.type;
        type_id = decl->tree_data.var_decl_data.type_id;
    }
    else if (decl->type == TREE_ARR_DECL)
    {
        type_tag = decl->tree_data.arr_decl_data.type;
        type_id = decl->tree_data.arr_decl_data.type_id;
    }

    if (type_id != NULL)
    {
        if (pascal_identifier_equals(type_id, "ShortString"))
            return SHORTSTRING_TYPE;

        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, type_id) >= 0 && type_node != NULL)
        {
            int resolved_type = UNKNOWN_TYPE;
            set_type_from_hashtype(&resolved_type, type_node);
            if (resolved_type != UNKNOWN_TYPE)
                return resolved_type;
        }
    }

    if (type_tag != UNKNOWN_TYPE)
        return type_tag;

    return UNKNOWN_TYPE;
}



/** FUNC_CALL **/
int semcheck_funccall(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val, scope_return;
    char *id;
    char *mangled_name = NULL;
    int arg_type, cur_arg;
    ListNode_t *true_args, *true_arg_ids, *args_given;
    ListNode_t *overload_candidates = NULL;  /* Declare early to avoid uninitialized use */
    HashNode_t *hash_return;
    Tree_t *arg_decl;
    int was_unit_qualified = 0;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    return_val = 0;
    id = expr->expr_data.function_call_data.id;
    double timing_start_ms = 0.0;
    if (FUNCCALL_TIMINGS_ENABLED()) {
        timing_start_ms = funccall_now_ms();
        fprintf(stderr, "[timing] funccall enter id=%s line=%d\n",
            id != NULL ? id : "(null)", expr->line_num);
    }
    if (expr->expr_data.function_call_data.is_procedural_var_call) {
        if (type_return != NULL)
        {
            KgpcType *call_type = expr->expr_data.function_call_data.call_kgpc_type;
            KgpcType *ret_type = NULL;
            if (call_type != NULL && call_type->kind == TYPE_KIND_PROCEDURE)
            {
                ret_type = kgpc_type_get_return_type(call_type);
                if (ret_type == NULL && call_type->info.proc_info.return_type_id != NULL)
                {
                    HashNode_t *ret_node = semcheck_find_preferred_type_node(symtab,
                        call_type->info.proc_info.return_type_id);
                    if (ret_node != NULL && ret_node->type != NULL)
                        ret_type = ret_node->type;
                }
            }

            if (ret_type != NULL && ret_type->kind == TYPE_KIND_PRIMITIVE)
            {
                *type_return = kgpc_type_get_primitive_tag(ret_type);
                semcheck_expr_set_resolved_type(expr, *type_return);
            }
            else if (ret_type != NULL && ret_type->kind == TYPE_KIND_RECORD)
            {
                *type_return = RECORD_TYPE;
                semcheck_expr_set_resolved_type(expr, RECORD_TYPE);
                expr->record_type = kgpc_type_get_record(ret_type);
            }
            else if (ret_type != NULL && ret_type->kind == TYPE_KIND_POINTER)
            {
                *type_return = POINTER_TYPE;
                /* Directly set resolved_kgpc_type to preserve full pointer type info
                 * (e.g., PAnsiChar needs to be a pointer-to-char, not just POINTER_TYPE) */
                if (expr->resolved_kgpc_type != NULL)
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                kgpc_type_retain(ret_type);
                expr->resolved_kgpc_type = ret_type;
            }
            else if (expr->resolved_kgpc_type != NULL)
            {
                *type_return = semcheck_tag_from_kgpc(expr->resolved_kgpc_type);
            }
            else
            {
                *type_return = PROCEDURE;
                semcheck_expr_set_resolved_type(expr, PROCEDURE);
            }
        }
        return 0;
    }
    args_given = expr->expr_data.function_call_data.args_expr;
    if (id != NULL)
    {
        const char *dot = strrchr(id, '.');
        if (dot != NULL && dot[1] != '\0')
        {
            size_t prefix_len = (size_t)(dot - id);
            char *prefix = (char *)malloc(prefix_len + 1);
            char *unqualified = strdup(dot + 1);
            if (prefix == NULL || unqualified == NULL)
            {
                if (prefix != NULL)
                    free(prefix);
                if (unqualified != NULL)
                    free(unqualified);
                semcheck_error_with_context("Error on line %d: failed to allocate memory for qualified call '%s'.\n",
                    expr->line_num, id);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            memcpy(prefix, id, prefix_len);
            prefix[prefix_len] = '\0';

            int prefix_is_unit = semcheck_is_unit_name(prefix);
            HashNode_t *prefix_node = NULL;
            int prefix_found = (FindIdent(&prefix_node, symtab, prefix) == 0 && prefix_node != NULL);

            if (!prefix_is_unit && prefix_found)
            {
                /* Treat qualified identifier as a member/procedural field call. */
                struct Expression *receiver_expr = mk_varid(expr->line_num, strdup(prefix));
                if (receiver_expr != NULL)
                {
                    ListNode_t *recv_node = CreateListNode(receiver_expr, LIST_EXPR);
                    recv_node->next = args_given;
                    args_given = recv_node;
                    expr->expr_data.function_call_data.args_expr = args_given;
                    expr->expr_data.function_call_data.is_method_call_placeholder = 1;
                }
                free(expr->expr_data.function_call_data.id);
                expr->expr_data.function_call_data.id = unqualified;
                id = unqualified;
            }
            else
            {
                /* Unit-qualified call; strip the unit prefix. */
                free(expr->expr_data.function_call_data.id);
                expr->expr_data.function_call_data.id = unqualified;
                id = unqualified;
                was_unit_qualified = 1;
            }

            free(prefix);
        }
    }
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_funccall: id='%s'\n", id != NULL ? id : "(null)");
    }
    if (getenv("KGPC_DEBUG_CALL_TYPES") != NULL && id != NULL &&
        pascal_identifier_equals(id, "IsDirectory"))
    {
        int arg_count = 0;
        for (ListNode_t *cur = expr->expr_data.function_call_data.args_expr;
             cur != NULL; cur = cur->next)
            arg_count++;
        fprintf(stderr, "[KGPC_DEBUG_CALL_TYPES] call=%s args=%d\n", id, arg_count);
        for (ListNode_t *cur = expr->expr_data.function_call_data.args_expr;
             cur != NULL; cur = cur->next)
        {
            struct Expression *arg_expr = (struct Expression *)cur->cur;
            if (arg_expr != NULL)
                semcheck_debug_expr_brief(arg_expr, "IsDirectory arg");
        }
    }
    if (id != NULL && strcmp(id, "socket") == 0) {
#ifdef DEBUG
        fprintf(stderr, "DEBUG: semcheck_funccall processing socket call. line=%d\n", expr->line_num);
#endif
    }
    args_given = expr->expr_data.function_call_data.args_expr;

    /* FPC Bootstrap Feature: Handle unit-qualified calls that the parser
     * represents as __Function(UnitName, Args...). Only strip the first
     * argument when the unit qualifier is unresolved AND the real function
     * (without "__") exists. */
    if (id != NULL && strncmp(id, "__", 2) == 0 && args_given != NULL)
    {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            if (semcheck_is_unit_name(first_arg->expr_data.id))
            {
                char *real_func_name = strdup(id + 2);
                if (real_func_name != NULL)
                {
                    ListNode_t *func_candidates = FindAllIdents(symtab, real_func_name);
                    if (func_candidates != NULL)
                    {
                        ListNode_t *remaining_args = args_given->next;
                        destroy_expr(first_arg);
                        args_given->cur = NULL;
                        free(args_given);

                        expr->expr_data.function_call_data.args_expr = remaining_args;
                        args_given = remaining_args;

                        free(expr->expr_data.function_call_data.id);
                        expr->expr_data.function_call_data.id = real_func_name;
                        id = real_func_name;

                        DestroyList(func_candidates);
                        real_func_name = NULL;
                        was_unit_qualified = 1;
                    }
                    if (real_func_name != NULL)
                        free(real_func_name);
                }
            }
        }
    }

    /* Handle unit-qualified calls parsed as member access: UnitName.Func(args).
     * For expressions like ObjPas.TEndian(0), the parser creates a method call
     * with ObjPas as the receiver and TEndian as the function name.
     * If ObjPas is a unit name, strip it and check if this is a typecast. */
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && args_given != NULL) {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        fprintf(stderr, "[SemCheck] funccall id='%s' is_method_call_placeholder=%d first_arg_type=%d first_arg_id=%s\n",
            id ? id : "(null)",
            expr->expr_data.function_call_data.is_method_call_placeholder,
            first_arg ? first_arg->type : -1,
            (first_arg && first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id) ? first_arg->expr_data.id : "(null)");
    }
    if (expr->expr_data.function_call_data.is_method_call_placeholder && args_given != NULL)
    {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            int is_unit_qualifier = semcheck_is_unit_name(first_arg->expr_data.id);
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
            {
                fprintf(stderr, "[SemCheck] funccall unit-qual check: receiver=%s is_unit=%d\n",
                    first_arg->expr_data.id, is_unit_qualifier);
            }
            if (!is_unit_qualifier)
            {
                int can_strip = 0;
                HashNode_t *first_node = NULL;
                if (FindIdent(&first_node, symtab, first_arg->expr_data.id) == -1 || first_node == NULL)
                {
                    can_strip = 1;
                }
                else if (first_node->hash_type == HASHTYPE_VAR ||
                         first_node->hash_type == HASHTYPE_ARRAY ||
                         first_node->hash_type == HASHTYPE_CONST ||
                         first_node->hash_type == HASHTYPE_FUNCTION_RETURN)
                {
                    can_strip = 0;
                }
                else if (first_node->hash_type == HASHTYPE_TYPE)
                {
                    can_strip = 0;
                }
                else if (first_node->type != NULL)
                {
                    if (kgpc_type_is_record(first_node->type))
                    {
                        can_strip = 0;
                    }
                    else if (kgpc_type_is_pointer(first_node->type) &&
                        first_node->type->info.points_to != NULL &&
                        kgpc_type_is_record(first_node->type->info.points_to))
                    {
                        can_strip = 0;
                    }
                    else
                    {
                        can_strip = 1;
                    }
                }
                else
                {
                    can_strip = 1;
                }

                if (can_strip)
                {
                    ListNode_t *func_candidates = FindAllIdents(symtab, id);
                    if (func_candidates != NULL)
                    {
                        is_unit_qualifier = 1;
                        DestroyList(func_candidates);
                    }
                }
            }
            if (is_unit_qualifier)
            {
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                {
                    fprintf(stderr, "[SemCheck] funccall unit-qual strip: receiver=%s id=%s\n",
                        first_arg->expr_data.id, id != NULL ? id : "(null)");
                }
                ListNode_t *remaining_args = args_given->next;
                destroy_expr(first_arg);
                args_given->cur = NULL;
                free(args_given);

                expr->expr_data.function_call_data.args_expr = remaining_args;
                args_given = remaining_args;
                expr->expr_data.function_call_data.is_method_call_placeholder = 0;
                was_unit_qualified = 1;

                /* After stripping the unit prefix, immediately check if this is a typecast.
                 * This handles unit-qualified typecasts like ObjPas.TEndian(0). */
                int typecast_result = semcheck_try_reinterpret_as_typecast(type_return, symtab, expr, max_scope_lev);
                if (typecast_result != 0 || expr->type == EXPR_TYPECAST)
                    return typecast_result;
            }
        }
    }

        /* If no explicit receiver was provided (not a method call placeholder), but Self is in scope
         * and defines this method, prepend Self so unqualified method calls resolve correctly. */
        if (!was_unit_qualified && id != NULL && !expr->expr_data.function_call_data.is_method_call_placeholder)
        {
            HashNode_t *self_node = NULL;
            if (FindIdent(&self_node, symtab, "Self") == 0 && self_node != NULL)
            {
                struct RecordType *self_record = get_record_type_from_node(self_node);
                int self_is_helper = 0;
                if (self_record == NULL)
                {
                    int self_type_tag = UNKNOWN_TYPE;
                    const char *self_type_name = NULL;
                    set_type_from_hashtype(&self_type_tag, self_node);
                    if (self_node->type != NULL &&
                        self_node->type->type_alias != NULL &&
                        self_node->type->type_alias->target_type_id != NULL)
                    {
                        self_type_name = self_node->type->type_alias->target_type_id;
                    }
                    struct RecordType *helper_record = semcheck_lookup_type_helper(symtab,
                        self_type_tag, self_type_name);
                    if (helper_record != NULL)
                    {
                        self_record = helper_record;
                        self_is_helper = 1;
                    }
                    if (self_record == NULL)
                    {
                        const char *current_owner = semcheck_get_current_method_owner();
                        if (current_owner != NULL)
                        {
                            struct RecordType *owner_record = semcheck_lookup_record_type(symtab, current_owner);
                            if (owner_record != NULL && owner_record->is_type_helper)
                            {
                                self_record = owner_record;
                                self_is_helper = 1;
                            }
                        }
                    }
                }
                
                /* If Self lookup returns a different class than expected (e.g., TBase instead of TDerived
                 * when we're in a TDerived method), try to find the correct class from the scope. */
                if (self_record != NULL)
                {
                    if (self_is_helper)
                    {
                        /* For type helpers, we need to let both function/method calls and
                         * typecast handling proceed through normal resolution.
                         * The code below handles method lookup, and typecast handling
                         * is done later in semcheck_try_reinterpret_as_typecast. */
                    }
                    if (self_is_helper && args_given != NULL)
                    {
                        struct Expression *first_arg = (struct Expression *)args_given->cur;
                        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
                            first_arg->expr_data.id != NULL &&
                            pascal_identifier_equals(first_arg->expr_data.id, "Self"))
                        {
                            /* Check if this might be a typecast like TSingleRec(Self).
                             * If the identifier is a type, fall through to typecast handling.
                             * If it's a function (like Length), continue to method lookup
                             * and eventually to builtin function handling. */
                            HashNode_t *id_node = NULL;
                            int find_result = FindIdent(&id_node, symtab, id);
                            if (find_result >= 0 &&
                                id_node != NULL && id_node->hash_type == HASHTYPE_TYPE)
                            {
                                /* This is a typecast like TSingleRec(Self), fall through to typecast handling */
                            }
                            /* Note: Don't return early for function calls like Length(Self).
                             * Let the code continue to method lookup and builtin handling. */
                        }
                    }
                    /* First, try to find the method in Self's class.
                     * For overloaded methods, we need to check ALL overloads to find one
                     * that matches our argument count. */
                    HashNode_t *method_node = NULL;
                    int args_count = ListLength(args_given);
                    int expects_self = 0;
                    ListNode_t *method_params = NULL;
                    
                    /* Build the mangled method name */
                    char mangled_method_name[256];
                    if (self_record->type_id != NULL)
                    {
                        snprintf(mangled_method_name, sizeof(mangled_method_name), "%s__%s",
                            self_record->type_id, id);
                        
                        /* Get ALL overloads of this method */
                        ListNode_t *all_methods = FindAllIdents(symtab, mangled_method_name);
                        if (all_methods != NULL)
                        {
                            /* Find the overload that matches our argument count */
                            ListNode_t *cur = all_methods;
                            while (cur != NULL)
                            {
                                HashNode_t *candidate = (HashNode_t *)cur->cur;
                                if (candidate != NULL &&
                                    (candidate->hash_type == HASHTYPE_FUNCTION ||
                                     candidate->hash_type == HASHTYPE_PROCEDURE) &&
                                    candidate->type != NULL)
                                {
                                    ListNode_t *candidate_params = kgpc_type_get_procedure_params(candidate->type);
                                    int candidate_expects_self = 0;
                                    int candidate_compatible = semcheck_method_accepts_arg_count(candidate_params,
                                        args_count, &candidate_expects_self);
                                    
                                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                        fprintf(stderr, "[SemCheck] Method overload check: candidate=%s params=%d args=%d compatible=%d\n",
                                            candidate->mangled_id ? candidate->mangled_id : candidate->id,
                                            semcheck_count_total_params(candidate_params), args_count, candidate_compatible);
                                    }
                                    
                                    if (candidate_compatible)
                                    {
                                        method_node = candidate;
                                        method_params = candidate_params;
                                        expects_self = candidate_expects_self;
                                        break;
                                    }
                                }
                                cur = cur->next;
                            }
                            DestroyList(all_methods);
                        }
                    }
                    
                    /* Fallback: use semcheck_find_class_method if no match found via overloads */
                    if (method_node == NULL)
                    {
                        method_node = semcheck_find_class_method(symtab, self_record, id, NULL);
                        
                        /* Check if the method was found but has wrong parameter count (0 params = forward decl) */
                        int method_params_len = 0;
                        if (method_node != NULL && method_node->type != NULL)
                        {
                            method_params = kgpc_type_get_procedure_params(method_node->type);
                            method_params_len = semcheck_count_total_params(method_params);
                            int found_compatible = semcheck_method_accepts_arg_count(method_params, args_count, &expects_self);
                            if (!found_compatible)
                                method_node = NULL;
                        }
                        
                        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                            fprintf(stderr, "[SemCheck] Method check (fallback): method_node=%p method_params_len=%d\n",
                                (void*)method_node, method_params_len);
                        }
                        
                        /* If method not found, or has wrong params (0 params = forward declaration),
                         * try looking up the method using class_method_bindings */
                        if (method_node == NULL || method_params_len == 0)
                        {
                            int class_count = 0;
                            int found_correct_method = 0;
                            ListNode_t *class_list = from_cparser_find_classes_with_method((char *)id, &class_count);
                            if (class_list != NULL)
                            {
                                ListNode_t *cur_class = class_list;
                                while (cur_class != NULL)
                                {
                                    char *class_name = (char *)cur_class->cur;
                                    if (class_name != NULL)
                                    {
                                        /* Look up this class */
                                        HashNode_t *class_node = NULL;
                                        int find_result = FindIdent(&class_node, symtab, class_name);
                                        if (find_result != -1 && class_node != NULL)
                                        {
                                            struct RecordType *correct_record = get_record_type_from_node(class_node);
                                            if (correct_record != NULL)
                                            {
                                                /* Don't use semcheck_find_class_method because it walks up inheritance
                                                 * and finds forward declarations in parent classes.
                                                 * Instead, look for the exact mangled name directly. */
                                                char local_mangled_name[256];
                                                snprintf(local_mangled_name, sizeof(local_mangled_name), "%s__%s", 
                                                    class_name, (char *)id);
                                                
                                                HashNode_t *correct_method = NULL;
                                                FindIdent(&correct_method, symtab, local_mangled_name);
                                                
                                                /* Check if the correct method has proper parameters */
                                                int correct_params_len = 0;
                                                int correct_expects_self = 0;
                                                int correct_compatible = 0;
                                                if (correct_method != NULL && correct_method->type != NULL)
                                                {
                                                    ListNode_t *correct_params = kgpc_type_get_procedure_params(correct_method->type);
                                                    correct_params_len = semcheck_count_total_params(correct_params);
                                                    correct_compatible = semcheck_method_accepts_arg_count(correct_params, args_count,
                                                        &correct_expects_self);
                                                }

                                                if (correct_method != NULL && correct_params_len > 0 &&
                                                    correct_compatible)
                                                {
                                                    self_record = correct_record;
                                                    method_node = correct_method;
                                                    expects_self = correct_expects_self;
                                                    found_correct_method = 1;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                    ListNode_t *next_class = cur_class->next;
                                    free(cur_class->cur);
                                    cur_class = next_class;
                                }
                                /* Only destroy list if we haven't already (i.e., if we didn't break early) */
                                if (!found_correct_method && class_list != NULL) {
                                    DestroyList(class_list);
                                }
                            }
                        }
                    }
                    
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck] Self injection: Self found, self_record=%s, method=%s, method_node=%p expects_self=%d\n",
                            self_record->type_id ? self_record->type_id : "(null)",
                            id ? id : "(null)", (void*)method_node, expects_self);
                    }

                    if (method_node != NULL)
                    {
                        const char *method_owner_name = NULL;
                        char owner_buf[256];
                        if (method_node->mangled_id != NULL)
                        {
                            const char *sep = strstr(method_node->mangled_id, "__");
                            if (sep != NULL && sep != method_node->mangled_id)
                            {
                                size_t owner_len = (size_t)(sep - method_node->mangled_id);
                                if (owner_len < sizeof(owner_buf))
                                {
                                    memcpy(owner_buf, method_node->mangled_id, owner_len);
                                    owner_buf[owner_len] = '\0';
                                    method_owner_name = owner_buf;
                                }
                            }
                        }
                        if (method_owner_name == NULL && self_record != NULL)
                            method_owner_name = self_record->type_id;

                        if (method_owner_name != NULL && id != NULL &&
                            from_cparser_is_method_static(method_owner_name, id))
                        {
                            expects_self = 0;
                        }
                        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                        {
                            fprintf(stderr, "[SemCheck] Implicit Self injection? method_params_len=%d mangled=%s\n",
                                semcheck_count_total_params(method_params),
                                method_node->mangled_id ? method_node->mangled_id : "(null)");
                        }
                        int already_has_self = 0;
                        if (args_given != NULL && args_given->cur != NULL)
                        {
                            struct Expression *first_arg = (struct Expression *)args_given->cur;
                            if (first_arg->type == EXPR_VAR_ID &&
                                first_arg->expr_data.id != NULL &&
                                pascal_identifier_equals(first_arg->expr_data.id, "Self"))
                            {
                                already_has_self = 1;
                            }
                        }
                        if (expects_self && !already_has_self)
                        {
                            struct Expression *self_expr = mk_varid(expr->line_num, strdup("Self"));
                            ListNode_t *self_arg = CreateListNode(self_expr, LIST_EXPR);
                            self_arg->next = args_given;
                            expr->expr_data.function_call_data.args_expr = self_arg;
                            args_given = self_arg;
                        }
                        if (expr->expr_data.function_call_data.resolved_func == NULL)
                            expr->expr_data.function_call_data.resolved_func = method_node;
                        if (expr->expr_data.function_call_data.mangled_id == NULL)
                        {
                            const char *resolved_name = method_node->mangled_id ?
                                method_node->mangled_id :
                                (method_node->id ? method_node->id : id);
                            if (resolved_name != NULL)
                                expr->expr_data.function_call_data.mangled_id = strdup(resolved_name);
                        }
                        /* Set call_kgpc_type for correct calling convention (e.g., float Self in xmm0) */
                        if (method_node->type != NULL) {
                            semcheck_expr_set_call_kgpc_type(expr, method_node->type, 0);
                            expr->expr_data.function_call_data.call_hash_type = method_node->hash_type;
                            expr->expr_data.function_call_data.is_call_info_valid = 1;
                        }
                        /* Check if this is a virtual method call that needs VMT dispatch */
                        if (expects_self)
                        {
                            const char *class_name = self_record->type_id;
                            if (class_name != NULL && from_cparser_is_method_virtual(class_name, id))
                            {
                                expr->expr_data.function_call_data.is_virtual_call = 1;
                                /* Find VMT index by looking through the class methods */
                                int vmt_index = -1;
                                if (self_record->methods != NULL)
                                {
                                    ListNode_t *method_entry = self_record->methods;
                                    while (method_entry != NULL)
                                    {
                                        struct MethodInfo *info = (struct MethodInfo *)method_entry->cur;
                                        if (info != NULL && info->name != NULL &&
                                            strcasecmp(info->name, id) == 0)
                                        {
                                            vmt_index = info->vmt_index;
                                            break;
                                        }
                                        method_entry = method_entry->next;
                                    }
                                }
                                expr->expr_data.function_call_data.vmt_index = vmt_index;
                                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                                {
                                    fprintf(stderr, "[SemCheck] Virtual method call: %s.%s vmt_index=%d\n",
                                        class_name, id, vmt_index);
                                }
                            }
                        }
                    }
            }
        }
    }

    if (id != NULL && strncmp(id, "__tfpg_ctor$", strlen("__tfpg_ctor$")) == 0)
    {
        if (type_return != NULL)
            *type_return = RECORD_TYPE;
        semcheck_expr_set_resolved_type(expr, RECORD_TYPE);
        return 0;
    }

    /* If this "call" is actually a type identifier, treat it as a typecast */
    int typecast_result = semcheck_try_reinterpret_as_typecast(type_return, symtab, expr, max_scope_lev);
    if (typecast_result != 0 || expr->type == EXPR_TYPECAST)
        return typecast_result;

    /* Detect calls through procedural fields of records (advanced records). The parser may have
     * rewritten `algo.Compare(x, y)` as a method call with `algo` injected as the first argument.
     * If the field is a procedural type, treat it as a procedural variable call instead. */
    if (id != NULL && args_given != NULL)
    {
        struct Expression *receiver_expr = (struct Expression *)args_given->cur;
        int recv_type = UNKNOWN_TYPE;
        semcheck_expr_legacy_tag(&recv_type, symtab, receiver_expr, max_scope_lev, NO_MUTATE);

        struct RecordType *recv_record = NULL;
        if (recv_type == RECORD_TYPE)
        {
            recv_record = receiver_expr->record_type;
        }
        else if (recv_type == POINTER_TYPE)
        {
            if (receiver_expr->record_type != NULL)
                recv_record = receiver_expr->record_type;
            else if (receiver_expr->resolved_kgpc_type != NULL &&
                     receiver_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER)
            {
                KgpcType *pointee = receiver_expr->resolved_kgpc_type->info.points_to;
                if (pointee != NULL && kgpc_type_is_record(pointee))
                    recv_record = kgpc_type_get_record(pointee);
            }
        }
        if (recv_record == NULL && receiver_expr->type == EXPR_VAR_ID &&
            receiver_expr->expr_data.id != NULL)
        {
            HashNode_t *recv_node = NULL;
            if (FindIdent(&recv_node, symtab, receiver_expr->expr_data.id) == 0 && recv_node != NULL)
            {
                recv_record = get_record_type_from_node(recv_node);
                if (recv_record == NULL && recv_node->type != NULL &&
                    recv_node->type->kind == TYPE_KIND_POINTER &&
                    recv_node->type->info.points_to != NULL &&
                    kgpc_type_is_record(recv_node->type->info.points_to))
                {
                    recv_record = kgpc_type_get_record(recv_node->type->info.points_to);
                }
            }
        }

        if (recv_record != NULL)
        {
            const char *field_lookup = id;
            while (field_lookup != NULL && field_lookup[0] == '_' && field_lookup[1] == '_')
                field_lookup += 2;  /* allow __Prefixed identifiers to match field names */

            struct RecordField *field_desc = NULL;
            long long field_offset = 0;
            if (resolve_record_field(symtab, recv_record, field_lookup, &field_desc,
                                     &field_offset, expr->line_num, 1) == 0 &&
                field_desc != NULL)
            {
                int is_proc_field = (field_desc->type == PROCEDURE);
                KgpcType *proc_type = NULL;

                if (field_desc->type_id != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, field_desc->type_id) != -1 &&
                        type_node != NULL && type_node->type != NULL &&
                        type_node->type->kind == TYPE_KIND_PROCEDURE)
                    {
                        proc_type = type_node->type;
                        kgpc_type_retain(proc_type);
                        is_proc_field = 1;
                    }
                }
                else if (field_desc->proc_type != NULL &&
                         field_desc->proc_type->kind == TYPE_KIND_PROCEDURE)
                {
                    proc_type = field_desc->proc_type;
                    kgpc_type_retain(proc_type);
                    is_proc_field = 1;
                }

                if (is_proc_field)
                {
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck] treating %s.%s as procedural field call\n",
                            receiver_expr->type == EXPR_VAR_ID ? receiver_expr->expr_data.id : "<expr>", id);
                    }
                    /* Remove the receiver from the argument list */
                    ListNode_t *remaining_args = args_given->next;
                    expr->expr_data.function_call_data.args_expr = remaining_args;
                    args_given->cur = NULL;
                    free(args_given);

                    /* Build a record-access expression to the procedural field */
                    struct Expression *proc_expr = (struct Expression *)calloc(1, sizeof(struct Expression));
                    if (proc_expr == NULL)
                    {
                        semcheck_error_with_context("Error on line %d: failed to allocate procedural field expression.\n",
                            expr->line_num);
                        *type_return = UNKNOWN_TYPE;
                        return ++return_val;
                    }
                    proc_expr->line_num = expr->line_num;
                    proc_expr->type = EXPR_RECORD_ACCESS;
                    proc_expr->expr_data.record_access_data.record_expr = receiver_expr;
                    proc_expr->expr_data.record_access_data.field_id = strdup(field_lookup);
                    proc_expr->expr_data.record_access_data.field_offset = (int)field_offset;
                    proc_expr->record_type = recv_record;
                    semcheck_expr_set_resolved_type(proc_expr, PROCEDURE);

                    /* Validate arguments against the procedural type if available */
                    if (proc_type != NULL)
                    {
                        ListNode_t *formal_params = kgpc_type_get_procedure_params(proc_type);
                        if (semcheck_count_total_params(formal_params) != ListLength(remaining_args))
                        {
                            semcheck_error_with_context("Error on line %d, call to procedural field %s: expected %d arguments, got %d\n",
                                expr->line_num, id, semcheck_count_total_params(formal_params), ListLength(remaining_args));
                            if (proc_type != NULL)
                                destroy_kgpc_type(proc_type);
                            destroy_expr(proc_expr);
                            *type_return = UNKNOWN_TYPE;
                            return ++return_val;
                        }

                        ListNode_t *formal = formal_params;
                        ListNode_t *actual = remaining_args;
                        int arg_idx = 0;
                        while (formal != NULL && actual != NULL)
                        {
                            Tree_t *formal_decl = (Tree_t *)formal->cur;
                            struct Expression *actual_expr = (struct Expression *)actual->cur;
                            
                            int formal_type = resolve_param_type(formal_decl, symtab);
                            int actual_type = UNKNOWN_TYPE;
                            semcheck_expr_legacy_tag(&actual_type, symtab, actual_expr, max_scope_lev, NO_MUTATE);

                            if (formal_type != UNKNOWN_TYPE && actual_type != UNKNOWN_TYPE &&
                                formal_type != actual_type)
                            {
                                if (!((formal_type == LONGINT_TYPE && actual_type == INT_TYPE) ||
                                      (formal_type == INT_TYPE && actual_type == LONGINT_TYPE) ||
                                      (formal_type == POINTER_TYPE) ||
                                      (actual_type == POINTER_TYPE)))
                                {
                                    fprintf(stderr, "Warning on line %d, argument %d type mismatch in call to procedural field %s\n",
                                        expr->line_num, arg_idx + 1, id);
                                }
                            }
                            
                            formal = formal->next;
                            actual = actual->next;
                            arg_idx++;
                        }

                        /* Cache call info for codegen */
                        expr->expr_data.function_call_data.call_kgpc_type = proc_type;
                        expr->expr_data.function_call_data.call_hash_type =
                            (kgpc_type_get_return_type(proc_type) == NULL) ? HASHTYPE_PROCEDURE : HASHTYPE_FUNCTION;
                        expr->expr_data.function_call_data.is_call_info_valid = 1;

                        KgpcType *ret_type = kgpc_type_get_return_type(proc_type);
                        if (ret_type == NULL && proc_type->info.proc_info.return_type_id != NULL)
                        {
                            HashNode_t *ret_node =
                                semcheck_find_preferred_type_node(symtab, proc_type->info.proc_info.return_type_id);
                            if (ret_node != NULL && ret_node->type != NULL)
                                ret_type = ret_node->type;
                        }
                        /* Resolve alias metadata to get the underlying type */
                        if (ret_type != NULL) {
                            struct TypeAlias *alias = kgpc_type_get_type_alias(ret_type);
                            if (alias != NULL && alias->target_type_id != NULL) {
                                HashNode_t *target_node =
                                    semcheck_find_preferred_type_node(symtab, alias->target_type_id);
                                if (target_node != NULL && target_node->type != NULL)
                                    ret_type = target_node->type;
                            } else if (alias != NULL && alias->base_type != UNKNOWN_TYPE) {
                                /* Alias resolves to a primitive type tag */
                                *type_return = alias->base_type;
                                semcheck_expr_set_resolved_type(expr, *type_return);
                                ret_type = NULL;  /* Mark as handled */
                            }
                        }
                        if (ret_type != NULL && ret_type->kind == TYPE_KIND_PRIMITIVE)
                        {
                            *type_return = kgpc_type_get_primitive_tag(ret_type);
                            semcheck_expr_set_resolved_type(expr, *type_return);
                        }
                        else if (ret_type != NULL && ret_type->kind == TYPE_KIND_RECORD)
                        {
                            *type_return = RECORD_TYPE;
                            semcheck_expr_set_resolved_type(expr, RECORD_TYPE);
                            expr->record_type = kgpc_type_get_record(ret_type);
                        }
                        else if (ret_type != NULL && ret_type->kind == TYPE_KIND_POINTER)
                        {
                            *type_return = POINTER_TYPE;
                            /* Directly set resolved_kgpc_type to preserve full pointer type info */
                            if (expr->resolved_kgpc_type != NULL)
                                destroy_kgpc_type(expr->resolved_kgpc_type);
                            kgpc_type_retain(ret_type);
                            expr->resolved_kgpc_type = ret_type;
                        }
                        else if (ret_type != NULL)
                        {
                            /* Fallback - unhandled type kind */
                            *type_return = PROCEDURE;
                            semcheck_expr_set_resolved_type(expr, PROCEDURE);
                        }
                        /* If ret_type is NULL and we didn't set type_return above (from alias),
                         * keep whatever was set */
                    }
                    else
                    {
                        *type_return = PROCEDURE;
                        semcheck_expr_set_resolved_type(expr, PROCEDURE);
                    }

                    expr->expr_data.function_call_data.is_procedural_var_call = 1;
                    expr->expr_data.function_call_data.procedural_var_symbol = NULL;
                    expr->expr_data.function_call_data.procedural_var_expr = proc_expr;
                    expr->expr_data.function_call_data.is_method_call_placeholder = 0;

                    /* We no longer treat this as a method call; proceed with validated arguments */
                    return return_val;
                }
            }
        }
    }

    if (id != NULL && pascal_identifier_equals(id, "SizeOf"))
        return semcheck_builtin_sizeof(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "GetMem"))
    {
        ListNode_t *args = expr->expr_data.function_call_data.args_expr;
        if (args == NULL || args->next != NULL)
        {
            semcheck_error_with_context("Error on line %d, GetMem expects exactly one argument.\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

        struct Expression *size_expr = (struct Expression *)args->cur;
        int size_type = UNKNOWN_TYPE;
        int error_count = semcheck_expr_legacy_tag(&size_type, symtab, size_expr, max_scope_lev, NO_MUTATE);
        if (error_count != 0)
        {
            *type_return = UNKNOWN_TYPE;
            return error_count;
        }

        HashNode_t *best_match = NULL;
        ListNode_t *candidates = FindAllIdents(symtab, "GetMem");
        for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate == NULL)
                continue;
            if (candidate->hash_type != HASHTYPE_FUNCTION)
                continue;
            if (candidate->type == NULL || candidate->type->kind != TYPE_KIND_PROCEDURE)
                continue;
            ListNode_t *params = kgpc_type_get_procedure_params(candidate->type);
            if (semcheck_count_total_params(params) != 1)
                continue;
            best_match = candidate;
            break;
        }
        if (candidates != NULL)
            DestroyList(candidates);

        semcheck_reset_function_call_cache(expr);
        if (best_match != NULL && best_match->mangled_id != NULL)
        {
            if (expr->expr_data.function_call_data.mangled_id != NULL)
            {
                free(expr->expr_data.function_call_data.mangled_id);
                expr->expr_data.function_call_data.mangled_id = NULL;
            }
            expr->expr_data.function_call_data.mangled_id = strdup(best_match->mangled_id);
            if (expr->expr_data.function_call_data.mangled_id == NULL)
            {
                fprintf(stderr, "Error: failed to allocate mangled name for GetMem.\n");
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            semcheck_set_function_call_target(expr, best_match);
        }
        else
        {
            char *mangled_name = MangleFunctionNameFromCallSite("GetMem", args, symtab, max_scope_lev);
            if (mangled_name != NULL)
            {
                if (expr->expr_data.function_call_data.mangled_id != NULL)
                {
                    free(expr->expr_data.function_call_data.mangled_id);
                    expr->expr_data.function_call_data.mangled_id = NULL;
                }
                expr->expr_data.function_call_data.mangled_id = mangled_name;
            }
        }
        *type_return = POINTER_TYPE;
        semcheck_expr_set_resolved_type(expr, POINTER_TYPE);
        return 0;
    }

    /* AllocMem: allocates memory and zero-initializes it, returns a Pointer */
    if (id != NULL && pascal_identifier_equals(id, "AllocMem"))
        return semcheck_builtin_allocmem(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "ToSingleByteFileSystemEncodedFileName"))
    {
        ListNode_t *args = expr->expr_data.function_call_data.args_expr;
        if (args == NULL || args->next != NULL)
        {
            semcheck_error_with_context("Error on line %d, ToSingleByteFileSystemEncodedFileName expects exactly one argument.\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

        int error_count = 0;
        struct Expression *arg_expr = (struct Expression *)args->cur;
        int arg_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_legacy_tag(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
        if (error_count != 0)
        {
            *type_return = UNKNOWN_TYPE;
            return error_count;
        }

        semcheck_reset_function_call_cache(expr);
        *type_return = STRING_TYPE;
        semcheck_expr_set_resolved_type(expr, STRING_TYPE);
        return 0;
    }

    if (id != NULL && pascal_identifier_equals(id, "ArrayStringToPPchar"))
    {
        ListNode_t *args = expr->expr_data.function_call_data.args_expr;
        if (args == NULL || args->next == NULL || args->next->next != NULL)
        {
            semcheck_error_with_context("Error on line %d, ArrayStringToPPchar expects exactly two arguments.\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

        int error_count = 0;
        struct Expression *arr_expr = (struct Expression *)args->cur;
        struct Expression *reserve_expr = (struct Expression *)args->next->cur;
        int arr_type = UNKNOWN_TYPE;
        int reserve_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_legacy_tag(&arr_type, symtab, arr_expr, max_scope_lev, NO_MUTATE);
        error_count += semcheck_expr_legacy_tag(&reserve_type, symtab, reserve_expr, max_scope_lev, NO_MUTATE);
        if (error_count != 0)
        {
            *type_return = UNKNOWN_TYPE;
            return error_count;
        }

        semcheck_reset_function_call_cache(expr);
        *type_return = POINTER_TYPE;
        semcheck_expr_set_resolved_type(expr, POINTER_TYPE);
        return 0;
    }

    if (id != NULL && pascal_identifier_equals(id, "Chr"))
        return semcheck_builtin_chr(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Ord"))
        return semcheck_builtin_ord(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Pred"))
        return semcheck_builtin_predsucc(type_return, symtab, expr, max_scope_lev, 0);

    if (id != NULL && pascal_identifier_equals(id, "Succ"))
        return semcheck_builtin_predsucc(type_return, symtab, expr, max_scope_lev, 1);

    if (id != NULL && pascal_identifier_equals(id, "Length"))
        return semcheck_builtin_length(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Copy"))
        return semcheck_builtin_copy(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Pos"))
        return semcheck_builtin_pos(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "StrPas"))
        return semcheck_builtin_strpas(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "EOF"))
        return semcheck_builtin_eof(type_return, symtab, expr, max_scope_lev);
    if (id != NULL && pascal_identifier_equals(id, "EOLN"))
        return semcheck_builtin_eoln(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Low"))
        return semcheck_builtin_lowhigh(type_return, symtab, expr, max_scope_lev, 0);

    if (id != NULL && pascal_identifier_equals(id, "High"))
        return semcheck_builtin_lowhigh(type_return, symtab, expr, max_scope_lev, 1);

    if (id != NULL && pascal_identifier_equals(id, "Default"))
        return semcheck_builtin_default(type_return, symtab, expr, max_scope_lev);

    /* Internal runtime function for open/dynamic array High - already resolved */
    if (id != NULL && strcmp(id, "kgpc_dynarray_compute_high") == 0)
    {
        /* This function was already set up by semcheck_builtin_lowhigh for dynamic arrays.
         * Just confirm it returns LONGINT_TYPE and proceed. */
        semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
        *type_return = LONGINT_TYPE;
        return 0;
    }

    if (id != NULL && pascal_identifier_equals(id, "Assigned"))
        return semcheck_builtin_assigned(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Abs"))
        return semcheck_builtin_abs(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "UpperCase"))
    {
        ListNode_t *args = expr->expr_data.function_call_data.args_expr;
        if (args != NULL && args->next == NULL)
        {
            struct Expression *arg_expr = (struct Expression *)args->cur;
            int arg_type = UNKNOWN_TYPE;
            int error_count = semcheck_expr_legacy_tag(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
            if (error_count == 0 && arg_type == CHAR_TYPE)
            {
                if (expr->expr_data.function_call_data.mangled_id != NULL)
                {
                    free(expr->expr_data.function_call_data.mangled_id);
                    expr->expr_data.function_call_data.mangled_id = NULL;
                }
                if (expr->expr_data.function_call_data.id != NULL)
                {
                    free(expr->expr_data.function_call_data.id);
                    expr->expr_data.function_call_data.id = NULL;
                }
                expr->expr_data.function_call_data.id = strdup("kgpc_upcase_char");
                expr->expr_data.function_call_data.mangled_id = strdup("kgpc_upcase_char");
                if (expr->expr_data.function_call_data.mangled_id == NULL)
                {
                    fprintf(stderr, "Error: failed to allocate mangled name for UpperCase.\n");
                    *type_return = UNKNOWN_TYPE;
                    return 1;
                }
                semcheck_reset_function_call_cache(expr);
                if (expr->resolved_kgpc_type != NULL)
                {
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                    expr->resolved_kgpc_type = NULL;
                }
                expr->resolved_kgpc_type = create_primitive_type(CHAR_TYPE);
                semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
                *type_return = CHAR_TYPE;
                return 0;
            }
        }
    }


    if (id != NULL && pascal_identifier_equals(id, "Sqrt"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sqrt", "kgpc_sqrt", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Sin"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sin", "kgpc_sin", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Csc"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Csc", "kgpc_csc", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Sinh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sinh", "kgpc_sinh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Csch"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Csch", "kgpc_csch", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Cos"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Cos", "kgpc_cos", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Sec"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sec", "kgpc_sec", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Cosh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Cosh", "kgpc_cosh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Sech"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sech", "kgpc_sech", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Tan"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Tan", "kgpc_tan", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Cot"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Cot", "kgpc_cot", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Tanh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Tanh", "kgpc_tanh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Coth"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Coth", "kgpc_coth", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcTan"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcTan", "kgpc_arctan", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCot"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCot", "kgpc_arccot", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcTan2"))
        return semcheck_builtin_arctan2(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Hypot"))
        return semcheck_builtin_hypot(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "ArcSin"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcSin", "kgpc_arcsin", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCos"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCos", "kgpc_arccos", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCosh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCosh", "kgpc_arccosh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcSech"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcSech", "kgpc_arcsech", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCsch"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCsch", "kgpc_arccsch", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCoth"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCoth", "kgpc_arccoth", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcSinh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcSinh", "kgpc_arcsinh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcTanh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcTanh", "kgpc_arctanh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "DegToRad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "DegToRad", "kgpc_deg_to_rad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "RadToDeg"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "RadToDeg", "kgpc_rad_to_deg", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "DegToGrad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "DegToGrad", "kgpc_deg_to_grad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "GradToDeg"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "GradToDeg", "kgpc_grad_to_deg", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "GradToRad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "GradToRad", "kgpc_grad_to_rad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "RadToGrad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "RadToGrad", "kgpc_rad_to_grad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "CycleToRad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "CycleToRad", "kgpc_cycle_to_rad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "RadToCycle"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "RadToCycle", "kgpc_rad_to_cycle", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Ln"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Ln", "kgpc_ln", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "LogN"))
        return semcheck_builtin_logn(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Exp"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Exp", "kgpc_exp", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Round"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Round", "kgpc_round", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Trunc"))
        return semcheck_builtin_trunc(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Int"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Int", "kgpc_int", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Frac"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Frac", "kgpc_frac", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Ceil"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Ceil", "kgpc_ceil", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Floor"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Floor", "kgpc_floor", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "UpCase"))
    {
        ListNode_t *args = expr->expr_data.function_call_data.args_expr;
        if (args != NULL && args->next == NULL)
        {
            struct Expression *arg_expr = (struct Expression *)args->cur;
            int arg_type = UNKNOWN_TYPE;
            int error_count = semcheck_expr_legacy_tag(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
            if (error_count == 0 && arg_type == CHAR_TYPE)
                return semcheck_builtin_upcase(type_return, symtab, expr, max_scope_lev);
            if (error_count == 0 && arg_type == STRING_TYPE &&
                arg_expr != NULL && arg_expr->type == EXPR_STRING &&
                arg_expr->expr_data.string != NULL &&
                strlen(arg_expr->expr_data.string) == 1)
                return semcheck_builtin_upcase(type_return, symtab, expr, max_scope_lev);
        }
    }

    if (id != NULL && pascal_identifier_equals(id, "Odd"))
        return semcheck_builtin_odd(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Sqr"))
        return semcheck_builtin_sqr(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Power"))
        return semcheck_builtin_power(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Random"))
        return semcheck_builtin_random(type_return, symtab, expr, max_scope_lev);
    
    if (id != NULL && pascal_identifier_equals(id, "RandomRange"))
        return semcheck_builtin_randomrange(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "RandSeed"))
        return semcheck_builtin_randseed(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Aligned"))
        return semcheck_builtin_aligned(type_return, symtab, expr, max_scope_lev);

    /***** FIRST VERIFY FUNCTION IDENTIFIER *****/

    /***** FIRST VERIFY FUNCTION IDENTIFIER *****/

    /* Resolve unqualified calls against the current static method owner (helpers/class methods). */
    if (!was_unit_qualified && id != NULL && !expr->expr_data.function_call_data.is_method_call_placeholder)
    {
        const char *current_owner = semcheck_get_current_method_owner();
        if (current_owner != NULL)
        {
            struct RecordType *owner_record = semcheck_lookup_record_type(symtab, current_owner);
            if (owner_record != NULL)
            {
                /* Use owner_out to get the ACTUAL owner where method was found (may be parent helper) */
                struct RecordType *actual_method_owner = NULL;
                HashNode_t *method_node = semcheck_find_class_method(symtab, owner_record, id, &actual_method_owner);
                char *mangled_method_name = NULL;
                ListNode_t *method_candidates = NULL;
                /* Use actual_method_owner if found (for inherited methods), else fall back to owner_record */
                struct RecordType *record_for_mangling = (actual_method_owner != NULL) ? actual_method_owner : owner_record;
                int is_static_owner_method = 0;
                if (record_for_mangling->type_id != NULL && id != NULL)
                    is_static_owner_method = from_cparser_is_method_static(record_for_mangling->type_id, id);
                if (record_for_mangling->type_id != NULL)
                {
                    size_t class_len = strlen(record_for_mangling->type_id);
                    size_t method_len = strlen(id);
                    mangled_method_name = (char *)malloc(class_len + 2 + method_len + 1);
                    if (mangled_method_name != NULL)
                        snprintf(mangled_method_name, class_len + 2 + method_len + 1,
                            "%s__%s", record_for_mangling->type_id, id);
                }
                if (mangled_method_name != NULL)
                    method_candidates = FindAllIdents(symtab, mangled_method_name);
                if (method_node == NULL && method_candidates != NULL)
                {
                    for (ListNode_t *cur = method_candidates; cur != NULL; cur = cur->next)
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
                }
                if (method_node != NULL)
                {
                    if (is_static_owner_method && args_given != NULL && args_given->cur != NULL)
                    {
                        struct Expression *first_arg = (struct Expression *)args_given->cur;
                        if (first_arg->type == EXPR_VAR_ID &&
                            first_arg->expr_data.id != NULL &&
                            pascal_identifier_equals(first_arg->expr_data.id, "Self"))
                        {
                            int given_count = ListLength(args_given);
                            int can_strip_self = 0;
                            int has_direct_match = 0;
                            int created_temp_list = 0;
                            ListNode_t *candidate_list = method_candidates;
                            if (candidate_list == NULL)
                            {
                                candidate_list = CreateListNode(method_node, LIST_UNSPECIFIED);
                                created_temp_list = 1;
                            }
                            for (ListNode_t *cur = candidate_list; cur != NULL; cur = cur->next)
                            {
                                HashNode_t *candidate = (HashNode_t *)cur->cur;
                                if (candidate == NULL || candidate->type == NULL ||
                                    (candidate->hash_type != HASHTYPE_FUNCTION &&
                                     candidate->hash_type != HASHTYPE_PROCEDURE))
                                    continue;
                                ListNode_t *params = kgpc_type_get_procedure_params(candidate->type);
                                int total_params = semcheck_count_total_params(params);
                                int required_params = semcheck_count_required_params(params);
                                if (given_count >= required_params && given_count <= total_params)
                                {
                                    int first_param_is_self = 0;
                                    if (params != NULL)
                                    {
                                        Tree_t *decl = (Tree_t *)params->cur;
                                        const char *param_name = NULL;
                                        if (decl != NULL && decl->type == TREE_VAR_DECL &&
                                            decl->tree_data.var_decl_data.ids != NULL)
                                            param_name = (const char *)decl->tree_data.var_decl_data.ids->cur;
                                        else if (decl != NULL && decl->type == TREE_ARR_DECL &&
                                            decl->tree_data.arr_decl_data.ids != NULL)
                                            param_name = (const char *)decl->tree_data.arr_decl_data.ids->cur;
                                        if (param_name != NULL && pascal_identifier_equals(param_name, "Self"))
                                            first_param_is_self = 1;
                                    }
                                    if (first_param_is_self)
                                        has_direct_match = 1;
                                }
                                if (given_count - 1 >= required_params && given_count - 1 <= total_params)
                                    can_strip_self = 1;
                            }
                            if (created_temp_list)
                                DestroyList(candidate_list);
                            if (!has_direct_match && can_strip_self)
                            {
                                ListNode_t *next_arg = args_given->next;
                                args_given->cur = NULL;
                                args_given->next = NULL;
                                args_given = next_arg;
                                expr->expr_data.function_call_data.args_expr = args_given;
                            }
                        }
                    }
                    int has_self_arg = 0;
                    if (args_given != NULL && args_given->cur != NULL)
                    {
                        struct Expression *first_arg = (struct Expression *)args_given->cur;
                        if (first_arg->type == EXPR_VAR_ID &&
                            first_arg->expr_data.id != NULL &&
                            pascal_identifier_equals(first_arg->expr_data.id, "Self"))
                        {
                            has_self_arg = 1;
                        }
                    }

                    int given_count = ListLength(args_given);
                    int has_direct_match = 0;
                    int has_self_match = 0;
                    int created_temp_list = 0;
                    ListNode_t *candidate_list = method_candidates;
                    if (candidate_list == NULL)
                    {
                        candidate_list = CreateListNode(method_node, LIST_UNSPECIFIED);
                        created_temp_list = 1;
                    }
                    for (ListNode_t *cur = candidate_list; cur != NULL; cur = cur->next)
                    {
                        HashNode_t *candidate = (HashNode_t *)cur->cur;
                        if (candidate == NULL || candidate->type == NULL ||
                            (candidate->hash_type != HASHTYPE_FUNCTION &&
                             candidate->hash_type != HASHTYPE_PROCEDURE))
                            continue;

                        ListNode_t *params = kgpc_type_get_procedure_params(candidate->type);
                        int total_params = semcheck_count_total_params(params);
                        int required_params = semcheck_count_required_params(params);
                        int is_static_method = 0;
                        if (record_for_mangling->type_id != NULL && id != NULL)
                            is_static_method = from_cparser_is_method_static(record_for_mangling->type_id, id);

                        int first_is_self = 0;
                        if (params != NULL)
                        {
                            Tree_t *decl = (Tree_t *)params->cur;
                            const char *param_name = NULL;
                            if (decl != NULL && decl->type == TREE_VAR_DECL &&
                                decl->tree_data.var_decl_data.ids != NULL)
                                param_name = (const char *)decl->tree_data.var_decl_data.ids->cur;
                            else if (decl != NULL && decl->type == TREE_ARR_DECL &&
                                decl->tree_data.arr_decl_data.ids != NULL)
                                param_name = (const char *)decl->tree_data.arr_decl_data.ids->cur;

                            if (param_name != NULL && pascal_identifier_equals(param_name, "Self"))
                                first_is_self = 1;
                        }

                        if (is_static_method && first_is_self)
                        {
                            int self_count = 0;
                            Tree_t *first_param = (Tree_t *)params->cur;
                            if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
                                first_param->tree_data.var_decl_data.ids != NULL)
                                self_count = ListLength(first_param->tree_data.var_decl_data.ids);
                            else if (first_param != NULL && first_param->type == TREE_ARR_DECL &&
                                first_param->tree_data.arr_decl_data.ids != NULL)
                                self_count = ListLength(first_param->tree_data.arr_decl_data.ids);
                            if (total_params >= self_count)
                                total_params -= self_count;
                            if (required_params >= self_count)
                                required_params -= self_count;
                            first_is_self = 0;
                        }

                        if (!first_is_self &&
                            given_count >= required_params && given_count <= total_params)
                            has_direct_match = 1;

                        if (first_is_self &&
                            given_count + 1 >= required_params && given_count + 1 <= total_params)
                            has_self_match = 1;
                    }
                    if (created_temp_list)
                        DestroyList(candidate_list);

                    if (!has_direct_match && has_self_match && !has_self_arg)
                    {
                        struct Expression *self_expr = mk_varid(expr->line_num, strdup("Self"));
                        if (self_expr != NULL)
                        {
                            ListNode_t *self_node = CreateListNode(self_expr, LIST_EXPR);
                            self_node->next = args_given;
                            args_given = self_node;
                            expr->expr_data.function_call_data.args_expr = args_given;
                        }
                    }

                    set_type_from_hashtype(type_return, method_node);
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, method_node->type);
                    expr->expr_data.function_call_data.resolved_func = method_node;
                    const char *resolved_method_name = (method_node->mangled_id != NULL) ?
                        method_node->mangled_id : method_node->id;
                    if (expr->expr_data.function_call_data.mangled_id != NULL)
                        free(expr->expr_data.function_call_data.mangled_id);
                    expr->expr_data.function_call_data.mangled_id =
                        (resolved_method_name != NULL) ? strdup(resolved_method_name) : NULL;

                    if (mangled_name != NULL)
                        free(mangled_name);
                    mangled_name = (mangled_method_name != NULL) ? strdup(mangled_method_name)
                                                                : (resolved_method_name != NULL ? strdup(resolved_method_name) : NULL);

                    if (method_candidates != NULL)
                    {
                        overload_candidates = method_candidates;
                    }
                    else
                    {
                        overload_candidates = CreateListNode(method_node, LIST_UNSPECIFIED);
                    }

                    if (mangled_method_name != NULL)
                        free(mangled_method_name);

                    hash_return = method_node;
                    goto method_call_resolved;
                }
                if (mangled_method_name != NULL)
                    free(mangled_method_name);
                if (method_candidates != NULL)
                    DestroyList(method_candidates);
            }
        }
    }

    /* Check for method call with unresolved name (member-access placeholder) where first arg is the type/instance. */
    if (!was_unit_qualified &&
        expr->expr_data.function_call_data.is_method_call_placeholder && args_given != NULL) {
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
        {
            fprintf(stderr, "[SemCheck] funccall method-placeholder: id=%s was_unit_qualified=%d\n",
                id != NULL ? id : "(null)", was_unit_qualified);
        }
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        int first_arg_type_tag;
        semcheck_expr_legacy_tag(&first_arg_type_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);
        
        if (first_arg->resolved_kgpc_type != NULL) {
            KgpcType *owner_type = first_arg->resolved_kgpc_type;
            struct RecordType *record_info = NULL;
            
            if (owner_type->kind == TYPE_KIND_RECORD) {
                record_info = owner_type->info.record_info;
            } else if (owner_type->kind == TYPE_KIND_POINTER &&
                owner_type->info.points_to != NULL &&
                owner_type->info.points_to->kind == TYPE_KIND_RECORD) {
                record_info = owner_type->info.points_to->info.record_info;
            }
            
            if (record_info != NULL && record_info->type_id != NULL) {
                const char *method_name = id;
                if (method_name != NULL && strncmp(method_name, "__", 2) == 0)
                    method_name += 2;
                if (method_name != NULL &&
                    (strncasecmp(method_name, "Create", 6) == 0 ||
                     strcasecmp(method_name, "Destroy") == 0))
                {
                    /* Defer constructor/destructor handling to the specialized path below. */
                }
                else
                {
                    /* Check if this is a static method */
                    int is_static = from_cparser_is_method_static(record_info->type_id, method_name);
                
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_funccall: __method call type=%s method=%s is_static=%d\n",
                        record_info->type_id, method_name, is_static);
                }
                
                    /* Look up the method */
                    HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, method_name, NULL);
                    
                    /* If method not found on record directly, try record helper */
                    struct RecordType *effective_record = record_info;
                    if (method_node == NULL && !record_type_is_class(record_info) && 
                        record_info->type_id != NULL && !record_info->is_type_helper)
                    {
                        struct RecordType *helper_record = semcheck_lookup_type_helper(symtab,
                            UNKNOWN_TYPE, record_info->type_id);
                        if (helper_record != NULL)
                        {
                            method_node = semcheck_find_class_method(symtab, helper_record, method_name, NULL);
                            if (method_node != NULL)
                            {
                                effective_record = helper_record;
                                is_static = from_cparser_is_method_static(helper_record->type_id, method_name);
                                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                    fprintf(stderr, "[SemCheck] semcheck_funccall: Found method %s via record helper %s\n",
                                        method_name, helper_record->type_id);
                                }
                            }
                        }
                    }
                    
                    if (method_node != NULL) {
                        /* Resolve the method name */
                        set_type_from_hashtype(type_return, method_node);
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, method_node->type);
                        expr->expr_data.function_call_data.resolved_func = method_node;
                        const char *resolved_method_name = (method_node->mangled_id != NULL) ?
                            method_node->mangled_id : method_node->id;
                        if (expr->expr_data.function_call_data.mangled_id != NULL)
                            free(expr->expr_data.function_call_data.mangled_id);
                        expr->expr_data.function_call_data.mangled_id =
                            (resolved_method_name != NULL) ? strdup(resolved_method_name) : NULL;
                        
                        /* Prefer all overloads of the resolved method for scoring. */
                        char *mangled_method_name = NULL;
                        if (effective_record->type_id != NULL && method_name != NULL)
                        {
                            size_t class_len = strlen(effective_record->type_id);
                            size_t method_len = strlen(method_name);
                            mangled_method_name = (char *)malloc(class_len + 2 + method_len + 1);
                            if (mangled_method_name != NULL)
                                snprintf(mangled_method_name, class_len + 2 + method_len + 1,
                                    "%s__%s", effective_record->type_id, method_name);
                        }

                        ListNode_t *method_candidates = NULL;
                        if (mangled_method_name != NULL)
                            method_candidates = FindAllIdents(symtab, mangled_method_name);

                        /* Check if ANY overload has Self as first param (instance method).
                         * If there are mixed static/instance overloads, don't remove type arg
                         * until after overload resolution picks the right one. */
                        int any_has_self = 0;
                        ListNode_t *cand_cur = method_candidates;
                        while (cand_cur != NULL && !any_has_self) {
                            HashNode_t *cand = (HashNode_t *)cand_cur->cur;
                            if (cand != NULL && cand->type != NULL) {
                                ListNode_t *cand_params = kgpc_type_get_procedure_params(cand->type);
                                if (cand_params != NULL) {
                                    Tree_t *first_param = (Tree_t *)cand_params->cur;
                                    if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
                                        first_param->tree_data.var_decl_data.ids != NULL) {
                                        const char *first_id = (const char *)first_param->tree_data.var_decl_data.ids->cur;
                                        if (first_id != NULL && pascal_identifier_equals(first_id, "Self"))
                                            any_has_self = 1;
                                    }
                                }
                            }
                            cand_cur = cand_cur->next;
                        }

                        /* Only remove type arg if ALL overloads are static (none have Self param) */
                        if (!any_has_self && is_static) {
                            /* For static methods, remove the first argument (the type identifier) */
                            ListNode_t *old_head = args_given;
                            expr->expr_data.function_call_data.args_expr = old_head->next;
                            old_head->next = NULL;  /* Detach to prevent dangling reference */
                            args_given = expr->expr_data.function_call_data.args_expr;

                            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                fprintf(stderr, "[SemCheck] semcheck_funccall: Removed type arg for static method call\n");
                            }
                        }

                        if (mangled_name != NULL)
                            free(mangled_name);
                        mangled_name = (mangled_method_name != NULL) ? strdup(mangled_method_name)
                                                                    : (resolved_method_name != NULL ? strdup(resolved_method_name) : NULL);

                        if (method_candidates != NULL)
                        {
                            overload_candidates = method_candidates;
                        }
                        else
                        {
                            overload_candidates = CreateListNode(method_node, LIST_UNSPECIFIED);
                        }

                        if (mangled_method_name != NULL)
                            free(mangled_method_name);

                        /* Continue with normal function call processing using the resolved method */
                        hash_return = method_node;
                        goto method_call_resolved;
                    }
                }
            }
            else
            {
                const char *type_name = get_expr_type_name(first_arg, symtab);
                struct RecordType *helper_record = semcheck_lookup_type_helper(symtab,
                    first_arg_type_tag, type_name);
                if (helper_record != NULL && helper_record->type_id != NULL)
                {
                    record_info = helper_record;
                    /* Retry helper method lookup */
                    const char *method_name = id;
                    if (method_name != NULL && strncmp(method_name, "__", 2) == 0)
                        method_name += 2;
                    if (method_name != NULL &&
                        (strncasecmp(method_name, "Create", 6) == 0 ||
                         strcasecmp(method_name, "Destroy") == 0))
                    {
                        /* Defer constructor/destructor handling to the specialized path below. */
                    }
                    else
                    {
                        int is_static = from_cparser_is_method_static(record_info->type_id, method_name);
                        /* Use owner_out to get the actual owner where the method was found (may be parent helper) */
                        struct RecordType *actual_method_owner = NULL;
                        HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, method_name, &actual_method_owner);
                        if (method_node != NULL)
                        {
                            set_type_from_hashtype(type_return, method_node);
                            semcheck_expr_set_resolved_kgpc_type_shared(expr, method_node->type);
                            expr->expr_data.function_call_data.resolved_func = method_node;
                            const char *resolved_method_name = (method_node->mangled_id != NULL) ?
                                method_node->mangled_id : method_node->id;
                            if (expr->expr_data.function_call_data.mangled_id != NULL)
                                free(expr->expr_data.function_call_data.mangled_id);
                            expr->expr_data.function_call_data.mangled_id =
                                (resolved_method_name != NULL) ? strdup(resolved_method_name) : NULL;

                            /* Use actual_method_owner for mangled name (for inherited methods from parent helpers) */
                            struct RecordType *record_for_mangling = (actual_method_owner != NULL) ? actual_method_owner : record_info;
                            char *mangled_method_name = NULL;
                            if (record_for_mangling->type_id != NULL && method_name != NULL)
                            {
                                size_t class_len = strlen(record_for_mangling->type_id);
                                size_t method_len = strlen(method_name);
                                mangled_method_name = (char *)malloc(class_len + 2 + method_len + 1);
                                if (mangled_method_name != NULL)
                                    snprintf(mangled_method_name, class_len + 2 + method_len + 1,
                                        "%s__%s", record_for_mangling->type_id, method_name);
                            }

                            ListNode_t *method_candidates = NULL;
                            if (mangled_method_name != NULL)
                                method_candidates = FindAllIdents(symtab, mangled_method_name);

                            /* Check if ANY overload has Self as first param (instance method).
                             * If there are mixed static/instance overloads, don't remove type arg
                             * until after overload resolution picks the right one. */
                            int any_has_self = 0;
                            ListNode_t *cand_cur = method_candidates;
                            while (cand_cur != NULL && !any_has_self) {
                                HashNode_t *cand = (HashNode_t *)cand_cur->cur;
                                if (cand != NULL && cand->type != NULL) {
                                    ListNode_t *cand_params = kgpc_type_get_procedure_params(cand->type);
                                    if (cand_params != NULL) {
                                        Tree_t *first_param = (Tree_t *)cand_params->cur;
                                        if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
                                            first_param->tree_data.var_decl_data.ids != NULL) {
                                            const char *first_id = (const char *)first_param->tree_data.var_decl_data.ids->cur;
                                            if (first_id != NULL && pascal_identifier_equals(first_id, "Self"))
                                                any_has_self = 1;
                                        }
                                    }
                                }
                                cand_cur = cand_cur->next;
                            }

                            /* Only remove type arg if ALL overloads are static (none have Self param) */
                            if (!any_has_self && is_static) {
                                ListNode_t *old_head = args_given;
                                expr->expr_data.function_call_data.args_expr = old_head->next;
                                old_head->next = NULL;
                                args_given = expr->expr_data.function_call_data.args_expr;
                            }

                            if (mangled_name != NULL)
                                free(mangled_name);
                            mangled_name = (mangled_method_name != NULL) ? strdup(mangled_method_name)
                                                                        : (resolved_method_name != NULL ? strdup(resolved_method_name) : NULL);

                            if (method_candidates != NULL)
                            {
                                overload_candidates = method_candidates;
                            }
                            else
                            {
                                overload_candidates = CreateListNode(method_node, LIST_UNSPECIFIED);
                            }

                            if (mangled_method_name != NULL)
                                free(mangled_method_name);

                            hash_return = method_node;
                            goto method_call_resolved;
                        }
                    }
                }
            }
        }
    }
    
    /* Check for Constructor Call (Create) where first arg is the class type/instance */
    if (id != NULL &&
        (strncasecmp(id, "Create", 6) == 0 || strcasecmp(id, "Destroy") == 0) &&
        args_given != NULL) {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        int first_arg_type_tag;
        semcheck_expr_legacy_tag(&first_arg_type_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);
        
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
             fprintf(stderr, "[SemCheck] semcheck_funccall: first_arg=%p type=%d id=%s record_type=%p resolved_kgpc_type=%p\n", 
                 (void*)first_arg, first_arg->type, 
                 (first_arg->type == EXPR_VAR_ID) ? first_arg->expr_data.id : "N/A",
                 first_arg->record_type,
                 first_arg->resolved_kgpc_type);
        }
        
        /* Check if first arg is a TYPE (for class constructor) or INSTANCE (for method) */
        /* Actually, for MyException.Create, MyException is a TYPE (if static call) or VAR (if instance call) */
        /* If it's a TYPE, resolved_kgpc_type should be the class type? */
        /* Wait, if MyException is a TYPE, semcheck_expr returns TYPE_KIND_TYPE? */
        /* Or the type tag of the type? */
        
        /* Get the record info from either resolved_kgpc_type or record_type */
        KgpcType *owner_type = first_arg->resolved_kgpc_type;
        int owner_type_owned = 0;
        struct RecordType *record_info = NULL;
        
        if (owner_type != NULL) {
             if (owner_type->kind == TYPE_KIND_RECORD) {
                 record_info = owner_type->info.record_info;
             } else if (owner_type->kind == TYPE_KIND_POINTER && 
                        owner_type->info.points_to != NULL &&
                        owner_type->info.points_to->kind == TYPE_KIND_RECORD) {
                 record_info = owner_type->info.points_to->info.record_info;
             }
        }
        
        /* Fallback to record_type if resolved_kgpc_type didn't give us a record */
        if (record_info == NULL && first_arg->record_type != NULL) {
            record_info = first_arg->record_type;
            /* Also need to set owner_type for return type determination later */
            if (owner_type == NULL && record_info != NULL) {
                /* Look up the type to get its KgpcType */
                HashNode_t *type_node = NULL;
                if (record_info->type_id != NULL &&
                    FindIdent(&type_node, symtab, record_info->type_id) != -1 &&
                    type_node != NULL && type_node->type != NULL) {
                    owner_type = type_node->type;
                }
            }
        }

        /* If still unresolved and the first arg is a type identifier, resolve it as a type name. */
        if (record_info == NULL && first_arg->type == EXPR_VAR_ID &&
            first_arg->expr_data.id != NULL)
        {
            HashNode_t *type_node = semcheck_find_preferred_type_node(symtab,
                first_arg->expr_data.id);
            if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
            {
                record_info = get_record_type_from_node(type_node);
                if (owner_type == NULL && type_node->type != NULL)
                    owner_type = type_node->type;
            }
        }
        
        if (record_info != NULL && record_info->type_id != NULL) {
            /* Ensure owner_type represents the class instance pointer for constructors. */
            if (record_type_is_class(record_info))
            {
                if (owner_type != NULL && owner_type->kind == TYPE_KIND_RECORD)
                {
                    kgpc_type_retain(owner_type);
                    KgpcType *ptr_type = create_pointer_type(owner_type);
                    if (ptr_type != NULL)
                    {
                        owner_type = ptr_type;
                        owner_type_owned = 1;
                    }
                }
                else if (owner_type == NULL)
                {
                    KgpcType *rec_type = create_record_type(record_info);
                    if (rec_type != NULL)
                    {
                        KgpcType *ptr_type = create_pointer_type(rec_type);
                        if (ptr_type != NULL)
                        {
                            owner_type = ptr_type;
                            owner_type_owned = 1;
                        }
                        else
                        {
                            destroy_kgpc_type(rec_type);
                        }
                    }
                }
            }
            struct RecordType *method_owner = record_info;
            ListNode_t *method_candidates = NULL;
            char *mangled_method_name = NULL;

            while (method_owner != NULL && method_owner->type_id != NULL)
            {
                size_t class_len = strlen(method_owner->type_id);
                size_t method_len = strlen(id);
                char *candidate_name = (char *)malloc(class_len + 2 + method_len + 1);
                if (candidate_name == NULL)
                    break;
                snprintf(candidate_name, class_len + 2 + method_len + 1, "%s__%s",
                         method_owner->type_id, id);

                ListNode_t *candidates = FindAllIdents(symtab, candidate_name);
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_funccall: Looking for '%s' found %d candidates\n",
                        candidate_name, ListLength(candidates));
                }

                if (candidates != NULL)
                {
                    method_candidates = candidates;
                    mangled_method_name = candidate_name;
                    break;
                }

                free(candidate_name);
                method_owner = semcheck_lookup_parent_record(symtab, method_owner);
            }

            /* If no candidates on the record, retry via any helper for this base type. */
            if (method_candidates == NULL && record_info != NULL &&
                !record_type_is_class(record_info) && record_info->type_id != NULL)
            {
                struct RecordType *helper_record = semcheck_lookup_type_helper(symtab,
                    UNKNOWN_TYPE, record_info->type_id);
                struct RecordType *actual_method_owner = NULL;
                if (helper_record != NULL)
                {
                    HashNode_t *method_node = semcheck_find_class_method(symtab,
                        helper_record, id, &actual_method_owner);
                    struct RecordType *owner_for_mangle =
                        (actual_method_owner != NULL) ? actual_method_owner : helper_record;
                    if (method_node != NULL && owner_for_mangle != NULL &&
                        owner_for_mangle->type_id != NULL)
                    {
                        size_t class_len = strlen(owner_for_mangle->type_id);
                        size_t method_len = strlen(id);
                        char *candidate_name = (char *)malloc(class_len + 2 + method_len + 1);
                        if (candidate_name != NULL)
                        {
                            snprintf(candidate_name, class_len + 2 + method_len + 1,
                                "%s__%s", owner_for_mangle->type_id, id);
                            ListNode_t *candidates = FindAllIdents(symtab, candidate_name);
                            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                fprintf(stderr, "[SemCheck] semcheck_funccall: Looking for '%s' found %d candidates\n",
                                    candidate_name, ListLength(candidates));
                            }
                            if (candidates != NULL)
                            {
                                method_candidates = candidates;
                                mangled_method_name = candidate_name;
                                method_owner = owner_for_mangle;
                            }
                            else
                            {
                                free(candidate_name);
                            }
                        }
                    }
                }
            }

            if (method_candidates != NULL && mangled_method_name != NULL) {
                /* Found at least one method overload */
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_funccall: Found constructor/method %s in class\n", id);
                }

                /* Check if this is a static method (class function with static modifier) */
                int is_static_method = from_cparser_is_method_static(method_owner->type_id, id);
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_funccall: is_static_method=%d for %s.%s\n",
                        is_static_method, method_owner->type_id, id);
                }

                /* Remove the first argument (the class reference) from the argument list
                 * since it's not a real argument to the constructor */
                ListNode_t *old_head = args_given;
                expr->expr_data.function_call_data.args_expr = old_head->next;
                old_head->next = NULL;  /* Detach to prevent dangling reference */
                ListNode_t *user_args = expr->expr_data.function_call_data.args_expr;
                args_given = user_args;  /* Update args_given to reflect removed type arg */

                /* For non-static constructors, add a placeholder Self argument at the front.
                 * Constructors have Self as first parameter, but from user's perspective
                 * they don't pass Self - it's implicitly created.
                 * Static factory methods (class function Create: T; static;) do NOT have Self.
                 * We use EXPR_NIL as the placeholder - codegen will allocate memory. */
                if (!is_static_method) {
                    struct Expression *self_placeholder = (struct Expression *)calloc(1, sizeof(struct Expression));
                    if (self_placeholder != NULL) {
                        /* Use nil as the placeholder - codegen will handle actual allocation */
                        self_placeholder->type = EXPR_NIL;
                        semcheck_expr_set_resolved_type(self_placeholder, POINTER_TYPE);
                        self_placeholder->line_num = expr->line_num;
                        /* Set the resolved_kgpc_type to match the class type for proper type matching */
                        if (owner_type != NULL) {
                            kgpc_type_retain(owner_type);
                            self_placeholder->resolved_kgpc_type = owner_type;
                        }
                        /* Set record_type for class pointer compatibility */
                        self_placeholder->record_type = record_info;

                        ListNode_t *self_node = CreateListNode(self_placeholder, LIST_EXPR);
                        if (self_node != NULL) {
                            self_node->next = user_args;
                            expr->expr_data.function_call_data.args_expr = self_node;
                            args_given = self_node;
                        }
                    }
                }
                
                /* Update the function call id to the mangled name */
                if (expr->expr_data.function_call_data.id != NULL)
                    free(expr->expr_data.function_call_data.id);
                expr->expr_data.function_call_data.id = strdup(mangled_method_name);
                if (expr->expr_data.function_call_data.mangled_id != NULL)
                    free(expr->expr_data.function_call_data.mangled_id);
                expr->expr_data.function_call_data.mangled_id = strdup(mangled_method_name);
                id = expr->expr_data.function_call_data.id;
                
                /* Set up overload candidates for normal resolution */
                overload_candidates = method_candidates;
                
                /* For constructors (Create, CreateFmt, etc.), set up the return type */
                const char *sep = strstr(id, "__");
                const char *method_name = sep != NULL ? sep + 2 : id;
                if (strncasecmp(method_name, "Create", 6) == 0 && owner_type != NULL) {
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck] semcheck_funccall: Setting up return type for constructor %s\n", method_name);
                    }
                    
                    /* Return type is the class itself (pointer to record) */
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, owner_type);
                    *type_return = semcheck_tag_from_kgpc(owner_type);
                    if (owner_type->kind == TYPE_KIND_POINTER && owner_type->info.points_to != NULL &&
                        owner_type->info.points_to->kind == TYPE_KIND_RECORD) {
                        expr->record_type = owner_type->info.points_to->info.record_info;
                    } else if (owner_type->kind == TYPE_KIND_RECORD) {
                        expr->record_type = owner_type->info.record_info;
                    }
                }
                
                free(mangled_method_name);
                /* Continue to normal overload resolution */
                goto method_call_resolved;
            }
            if (mangled_method_name != NULL)
                free(mangled_method_name);
        }
    }

    /* If constructor was already resolved above, skip overload resolution */
    if (getenv("KGPC_DEBUG_CALL_TYPES") != NULL && id != NULL &&
        pascal_identifier_equals(id, "IsDirectory"))
    {
        fprintf(stderr, "[KGPC_DEBUG_CALL_TYPES] IsDirectory resolved_func=%p mangled=%s\n",
            (void *)expr->expr_data.function_call_data.resolved_func,
            expr->expr_data.function_call_data.mangled_id != NULL ?
                expr->expr_data.function_call_data.mangled_id : "<null>");
    }
    if (expr->expr_data.function_call_data.resolved_func != NULL &&
        expr->expr_data.function_call_data.mangled_id != NULL)
    {
        /* Constructor already resolved, skip to argument validation */
        hash_return = expr->expr_data.function_call_data.resolved_func;
        scope_return = 0; /* Constructor is in current scope */
        /* Ensure call_kgpc_type is set for code generator calling convention */
        if (hash_return->type != NULL && !expr->expr_data.function_call_data.is_call_info_valid) {
            semcheck_expr_set_call_kgpc_type(expr, hash_return->type, 0);
            expr->expr_data.function_call_data.call_hash_type = hash_return->hash_type;
            expr->expr_data.function_call_data.is_call_info_valid = 1;
        }
        goto skip_overload_resolution;
    }

    if (id != NULL) {
        overload_candidates = FindAllIdents(symtab, id);
    }

    int prefer_non_builtin = 0;
    if (overload_candidates != NULL)
    {
        ListNode_t *cur = overload_candidates;
        while (cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL &&
                (candidate->hash_type == HASHTYPE_FUNCTION ||
                 candidate->hash_type == HASHTYPE_PROCEDURE))
            {
                if (!semcheck_candidate_is_builtin(symtab, candidate))
                {
                    prefer_non_builtin = 1;
                    break;
                }
            }
            cur = cur->next;
        }
    }

    /* Check if this is a call through a procedural variable */
    /* If 'id' resolves to a variable with a procedural type, handle it specially */
    if (overload_candidates != NULL && overload_candidates->cur != NULL)
    {
        HashNode_t *first_candidate = (HashNode_t *)overload_candidates->cur;
        if (first_candidate->hash_type == HASHTYPE_VAR &&
            first_candidate->type != NULL &&
            first_candidate->type->kind == TYPE_KIND_PROCEDURE)
        {
            /* This is a procedural variable - we're calling through a function pointer */
            KgpcType *proc_type = first_candidate->type;
            ListNode_t *formal_params = kgpc_type_get_procedure_params(proc_type);
            KgpcType *return_type = kgpc_type_get_return_type(proc_type);
            
            /* Validate arguments match the procedural type's signature */
            if (semcheck_count_total_params(formal_params) != ListLength(args_given))
            {
                semcheck_error_with_context("Error on line %d, call to procedural variable %s: expected %d arguments, got %d\n",
                    expr->line_num, id, semcheck_count_total_params(formal_params), ListLength(args_given));
                destroy_list(overload_candidates);
                if (mangled_name != NULL) free(mangled_name);
                *type_return = UNKNOWN_TYPE;
                return ++return_val;
            }
            
            /* Check argument types */
            ListNode_t *formal = formal_params;
            ListNode_t *actual = args_given;
            int arg_idx = 0;
            while (formal != NULL && actual != NULL)
            {
                Tree_t *formal_decl = (Tree_t *)formal->cur;
                struct Expression *actual_expr = (struct Expression *)actual->cur;
                
                int formal_type = resolve_param_type(formal_decl, symtab);
                int actual_type = UNKNOWN_TYPE;
                semcheck_expr_legacy_tag(&actual_type, symtab, actual_expr, max_scope_lev, NO_MUTATE);
                
                /* Simple type check - could be more sophisticated */
                if (formal_type != actual_type && formal_type != UNKNOWN_TYPE && actual_type != UNKNOWN_TYPE)
                {
                    /* Allow some type coercions like INT to LONGINT */
                    if (!((formal_type == LONGINT_TYPE && actual_type == INT_TYPE) ||
                          (formal_type == INT_TYPE && actual_type == LONGINT_TYPE) ||
                          (formal_type == POINTER_TYPE) || /* pointers are flexible */
                          (actual_type == POINTER_TYPE)))
                    {
                        fprintf(stderr, "Warning on line %d, argument %d type mismatch in call to procedural variable %s\n",
                            expr->line_num, arg_idx + 1, id);
                    }
                }
                
                formal = formal->next;
                actual = actual->next;
                arg_idx++;
            }
            
            /* Set the return type */
            if (return_type != NULL)
            {
                if (return_type->kind == TYPE_KIND_PRIMITIVE)
                {
                    *type_return = kgpc_type_get_primitive_tag(return_type);
                }
                else if (return_type->kind == TYPE_KIND_RECORD)
                {
                    *type_return = RECORD_TYPE;
                }
                else if (return_type->kind == TYPE_KIND_POINTER)
                {
                    *type_return = POINTER_TYPE;
                }
                else
                {
                    *type_return = UNKNOWN_TYPE;
                }

                semcheck_expr_set_resolved_kgpc_type_shared(expr, return_type);
                semcheck_expr_set_resolved_type(expr, *type_return);  /* Also set resolved_type explicitly */
            }
            else
            {
                /* It's a procedure (no return value) */
                *type_return = PROCEDURE;
            }
            
            /* Mark this as a procedural variable call */
            expr->expr_data.function_call_data.is_procedural_var_call = 1;
            expr->expr_data.function_call_data.procedural_var_symbol = first_candidate;
            
            destroy_list(overload_candidates);
            if (mangled_name != NULL) free(mangled_name);
            return 0;  /* Success */
        }
    }
    
    if (id == NULL) {
        semcheck_error_with_context("Error on line %d: function call with NULL id\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        destroy_list(overload_candidates);
        return ++return_val;
    }
    mangled_name = MangleFunctionNameFromCallSite(id, args_given, symtab, max_scope_lev);
    if (mangled_name == NULL)
    {
        fprintf(stderr, "Error: failed to mangle function name for call to %s\n", id);
        *type_return = UNKNOWN_TYPE;
        destroy_list(overload_candidates);
        return ++return_val;
    }

method_call_resolved:
    ;  /* Label for jumping here after method resolution */
    int final_status = 0;

    HashNode_t *best_match = NULL;
    int best_score = 0;
    int num_best_matches = 0;

    if (mangled_name != NULL && overload_candidates != NULL)
    {
        ListNode_t *cur = overload_candidates;
        int mangled_matches = 0;
        HashNode_t *first_match = NULL;
        while (cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL && candidate->mangled_id != NULL &&
                strcmp(candidate->mangled_id, mangled_name) == 0)
            {
                if (first_match == NULL)
                    first_match = candidate;
                best_match = candidate;
                best_score = 0;
                num_best_matches = 1;
                mangled_matches++;
            }
            cur = cur->next;
        }
        /* When mangled_matches == 1 but there are other candidates with equivalent signatures,
         * we need to go through full resolution to properly handle declaration order.
         * Check if other candidates share equivalent signature with the match. */
        if (mangled_matches == 1 && first_match != NULL && ListLength(overload_candidates) > 1)
        {
            for (ListNode_t *check = overload_candidates; check != NULL; check = check->next)
            {
                HashNode_t *other = (HashNode_t *)check->cur;
                if (other != first_match && other != NULL &&
                    (other->hash_type == HASHTYPE_FUNCTION || other->hash_type == HASHTYPE_PROCEDURE) &&
                    semcheck_candidates_share_signature(symtab, first_match, other))
                {
                    /* There's another candidate with equivalent signature - defer to full resolution */
                    best_match = NULL;
                    num_best_matches = 0;
                    mangled_matches = 0;
                    break;
                }
            }
        }
        if (mangled_matches > 1)
        {
            /* Defer to overload resolution to apply tie-breakers (e.g., alias vs builtin). */
            best_match = NULL;
            num_best_matches = 0;
        }
    }

    if (best_match == NULL)
    {
        double resolve_t0 = 0.0;
        if (FUNCCALL_TIMINGS_ENABLED())
            resolve_t0 = funccall_now_ms();
        int resolve_status = semcheck_resolve_overload(&best_match, &best_score,
            &num_best_matches, overload_candidates, args_given, symtab, expr,
            max_scope_lev, prefer_non_builtin);
        if (FUNCCALL_TIMINGS_ENABLED()) {
            fprintf(stderr, "[timing] funccall resolve_overload id=%s: %.2f ms\n",
                id != NULL ? id : "(null)", funccall_now_ms() - resolve_t0);
        }
        if (resolve_status == 3)
        {
            *type_return = UNKNOWN_TYPE;
            final_status = ++return_val;
            goto funccall_cleanup;
        }
    }


    if (num_best_matches == 1)
    {
        if (best_match != NULL && best_match->type != NULL &&
            best_match->type->kind == TYPE_KIND_PROCEDURE)
        {
            if (best_match->type->info.proc_info.return_type == NULL &&
                best_match->type->info.proc_info.return_type_id != NULL)
            {
                const char *ret_id = best_match->type->info.proc_info.return_type_id;
                HashNode_t *type_node = NULL;
                KgpcType *ret_type = NULL;
                if (FindIdent(&type_node, symtab, (char *)ret_id) != -1 &&
                    type_node != NULL && type_node->type != NULL)
                {
                    kgpc_type_retain(type_node->type);
                    ret_type = type_node->type;
                }
                else
                {
                    int tag = semcheck_map_builtin_type_name(symtab, ret_id);
                    if (tag != UNKNOWN_TYPE)
                        ret_type = create_primitive_type(tag);
                }
                if (ret_type != NULL)
                {
                    best_match->type->info.proc_info.return_type = ret_type;
                    if (best_match->hash_type == HASHTYPE_PROCEDURE)
                        best_match->hash_type = HASHTYPE_FUNCTION;
                }
            }
            if (best_match->type->info.proc_info.return_type == NULL &&
                (best_match->id != NULL || best_match->mangled_id != NULL))
            {
                const char *method_id = best_match->mangled_id != NULL ?
                    best_match->mangled_id : best_match->id;
                const char *sep = (method_id != NULL) ? strstr(method_id, "__") : NULL;
                if (sep != NULL && sep != method_id && sep[2] != '\0')
                {
                    size_t class_len = (size_t)(sep - method_id);
                    char *class_name = (char *)malloc(class_len + 1);
                    if (class_name != NULL)
                    {
                        memcpy(class_name, method_id, class_len);
                        class_name[class_len] = '\0';
                        const char *method_name = sep + 2;
                        char *method_base = NULL;
                        const char *dollar = strchr(method_name, '$');
                        if (dollar != NULL)
                        {
                            size_t base_len = (size_t)(dollar - method_name);
                            method_base = (char *)malloc(base_len + 1);
                            if (method_base != NULL)
                            {
                                memcpy(method_base, method_name, base_len);
                                method_base[base_len] = '\0';
                            }
                        }
                        const char *method_lookup = (method_base != NULL) ? method_base : method_name;
                        HashNode_t *class_node = NULL;
                        if (FindIdent(&class_node, symtab, class_name) != -1 && class_node != NULL)
                        {
                            struct RecordType *record_info = get_record_type_from_node(class_node);
                            if (record_info != NULL)
                            {
                                struct MethodTemplate *tmpl =
                                    from_cparser_get_method_template(record_info, method_lookup);
                                if (tmpl != NULL && tmpl->return_type_ast != NULL &&
                                    tmpl->return_type_ast->child != NULL)
                                {
                                    KgpcType *ret_type = convert_type_spec_to_kgpctype(
                                        tmpl->return_type_ast->child, symtab);
                                    if (ret_type != NULL)
                                    {
                                        best_match->type->info.proc_info.return_type = ret_type;
                                        if (best_match->hash_type == HASHTYPE_PROCEDURE)
                                            best_match->hash_type = HASHTYPE_FUNCTION;
                                    }
                                }
                            }
                        }
                        if (method_base != NULL)
                            free(method_base);
                        free(class_name);
                    }
                }
            }

            Tree_t *proc_def = best_match->type->info.proc_info.definition;
            int is_external = 0;
            if (proc_def != NULL)
            {
                is_external = proc_def->tree_data.subprogram_data.cname_flag != 0 ||
                    proc_def->tree_data.subprogram_data.cname_override != NULL;
            }
            if (!is_external &&
                (best_match->mangled_id == NULL ||
                 (best_match->id != NULL &&
                  strcmp(best_match->mangled_id, best_match->id) == 0)))
            {
                char *computed = MangleFunctionName(best_match->id,
                    best_match->type->info.proc_info.params, symtab);
                if (computed != NULL)
                {
                    if (best_match->mangled_id != NULL)
                        free(best_match->mangled_id);
                    best_match->mangled_id = computed;
                }
            }
        }
        const char *target_name = best_match->mangled_id;
        if (target_name == NULL || target_name[0] == '\0')
        {
            if (best_match->type != NULL && best_match->type->kind == TYPE_KIND_PROCEDURE)
            {
                Tree_t *proc_def = best_match->type->info.proc_info.definition;
                if (proc_def != NULL)
                {
                    target_name = proc_def->tree_data.subprogram_data.cname_override;
                    if (target_name == NULL)
                        target_name = proc_def->tree_data.subprogram_data.id;
                }
            }
            if (target_name == NULL || target_name[0] == '\0')
                target_name = best_match->id;
        }

        char *resolved_name = NULL;
        if (target_name != NULL)
            resolved_name = strdup(target_name);
        if (resolved_name == NULL)
        {
            fprintf(stderr, "Error: failed to duplicate mangled name for %s\n",
                best_match->id ? best_match->id : "(anonymous)");
            *type_return = UNKNOWN_TYPE;
            final_status = ++return_val;
            goto funccall_cleanup;
        }
        if (expr->expr_data.function_call_data.mangled_id != NULL)
            free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = resolved_name;
        if (best_match->type != NULL && best_match->type->kind == TYPE_KIND_PROCEDURE)
        {
            Tree_t *proc_def = best_match->type->info.proc_info.definition;
            if (proc_def != NULL)
            {
                bool no_body = (proc_def->tree_data.subprogram_data.statement_list == NULL);
                if (no_body)
                {
                    const char *target_name = proc_def->tree_data.subprogram_data.cname_override;
                    if (target_name == NULL)
                    {
                        if (proc_def->tree_data.subprogram_data.cname_flag)
                        {
                            target_name = proc_def->tree_data.subprogram_data.id;
                        }
                        else if (proc_def->tree_data.subprogram_data.mangled_id != NULL)
                        {
                            target_name = proc_def->tree_data.subprogram_data.mangled_id;
                        }
                        else if (best_match->mangled_id != NULL)
                        {
                            target_name = best_match->mangled_id;
                        }
                        else
                        {
                            target_name = proc_def->tree_data.subprogram_data.id;
                        }
                    }
                    if (target_name != NULL)
                    {
                        free(expr->expr_data.function_call_data.mangled_id);
                        expr->expr_data.function_call_data.mangled_id = strdup(target_name);
                    }
                }
                else if (proc_def->tree_data.subprogram_data.cname_flag &&
                         proc_def->tree_data.subprogram_data.mangled_id != NULL)
                {
                    free(expr->expr_data.function_call_data.mangled_id);
                    expr->expr_data.function_call_data.mangled_id =
                        strdup(proc_def->tree_data.subprogram_data.mangled_id);
                }
                else if (proc_def->tree_data.subprogram_data.mangled_id != NULL)
                {
                    free(expr->expr_data.function_call_data.mangled_id);
                    expr->expr_data.function_call_data.mangled_id =
                        strdup(proc_def->tree_data.subprogram_data.mangled_id);
                }
            }
        }
        semcheck_set_function_call_target(expr, best_match);
        semcheck_mark_call_requires_static_link(best_match);
        hash_return = best_match;
        scope_return = 0; // FIXME
    }
    else if (num_best_matches == 0)
    {
        if (id != NULL && pascal_identifier_equals(id, "AllocMem"))
            return semcheck_builtin_allocmem(type_return, symtab, expr, max_scope_lev);
        
        /* Build detailed error message with argument types and available overloads */
        {
            /* First, build a string showing the actual argument types */
            char arg_types_buf[1024] = "(";
            int buf_pos = 1;
            if (args_given != NULL)
            {
                int idx = 0;
                for (ListNode_t *cur = args_given; cur != NULL; cur = cur->next)
                {
                    struct Expression *arg = (struct Expression *)cur->cur;
                    int tag = UNKNOWN_TYPE;
                    semcheck_expr_legacy_tag(&tag, symtab, arg, max_scope_lev, NO_MUTATE);
                    const char *type_name = semcheck_type_tag_name(tag);
                    
                    /* Also check for resolved_kgpc_type for better type info */
                    if (arg != NULL && arg->resolved_kgpc_type != NULL)
                    {
                        const char *kgpc_str = kgpc_type_to_string(arg->resolved_kgpc_type);
                        if (kgpc_str != NULL && kgpc_str[0] != '\0')
                            type_name = kgpc_str;
                    }
                    
                    if (idx > 0 && buf_pos < (int)sizeof(arg_types_buf) - 3)
                    {
                        arg_types_buf[buf_pos++] = ',';
                        arg_types_buf[buf_pos++] = ' ';
                    }
                    int remaining = (int)sizeof(arg_types_buf) - buf_pos - 1;
                    if (remaining > 0)
                    {
                        int written = snprintf(arg_types_buf + buf_pos, remaining, "%s", type_name);
                        if (written > 0)
                            buf_pos += (written < remaining) ? written : remaining - 1;
                    }
                    idx++;
                }
            }
            if (buf_pos < (int)sizeof(arg_types_buf) - 1)
                arg_types_buf[buf_pos++] = ')';
            arg_types_buf[buf_pos] = '\0';
            
            /* Now build a string showing available overloads */
            char overloads_buf[2048] = "";
            int ovl_pos = 0;
            if (overload_candidates != NULL)
            {
                for (ListNode_t *cur = overload_candidates; cur != NULL; cur = cur->next)
                {
                    HashNode_t *cand = (HashNode_t *)cur->cur;
                    if (cand != NULL && cand->type != NULL &&
                        (cand->hash_type == HASHTYPE_FUNCTION ||
                         cand->hash_type == HASHTYPE_PROCEDURE))
                    {
                        int remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                        if (remaining <= 0) break;
                        
                        /* Format: "  - function_name(param_types): return_type" */
                        int written = snprintf(overloads_buf + ovl_pos, remaining, "  - %s(",
                            cand->id ? cand->id : "<anonymous>");
                        if (written > 0) ovl_pos += (written < remaining) ? written : remaining - 1;
                        
                        /* Add parameter types */
                        ListNode_t *params = kgpc_type_get_procedure_params(cand->type);
                        int param_idx = 0;
                        for (ListNode_t *p = params; p != NULL; p = p->next)
                        {
                            Tree_t *param = (Tree_t *)p->cur;
                            if (param != NULL)
                            {
                                remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                                if (remaining <= 0) break;
                                
                                if (param_idx > 0)
                                {
                                    written = snprintf(overloads_buf + ovl_pos, remaining, ", ");
                                    if (written > 0) ovl_pos += (written < remaining) ? written : remaining - 1;
                                    remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                                }
                                
                                const char *param_type_str = "?";
                                if (param->tree_data.var_decl_data.cached_kgpc_type != NULL)
                                    param_type_str = kgpc_type_to_string(param->tree_data.var_decl_data.cached_kgpc_type);
                                else if (param->tree_data.var_decl_data.type_id != NULL)
                                    param_type_str = param->tree_data.var_decl_data.type_id;
                                else if (param->tree_data.var_decl_data.type != UNKNOWN_TYPE)
                                    param_type_str = semcheck_type_tag_name(param->tree_data.var_decl_data.type);
                                
                                written = snprintf(overloads_buf + ovl_pos, remaining, "%s", param_type_str);
                                if (written > 0) ovl_pos += (written < remaining) ? written : remaining - 1;
                                param_idx++;
                            }
                        }
                        
                        remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                        if (remaining > 0)
                        {
                            /* Add return type for functions */
                            KgpcType *ret_type = kgpc_type_get_return_type(cand->type);
                            if (ret_type != NULL)
                            {
                                written = snprintf(overloads_buf + ovl_pos, remaining, "): %s\n",
                                    kgpc_type_to_string(ret_type));
                            }
                            else
                            {
                                written = snprintf(overloads_buf + ovl_pos, remaining, ")\n");
                            }
                            if (written > 0) ovl_pos += (written < remaining) ? written : remaining - 1;
                        }
                    }
                }
            }
            
            if (overloads_buf[0] != '\0')
            {
                semcheck_error_with_context(
                    "Error on line %d, call to function %s%s does not match any available overload.\n"
                    "Available overloads:\n%s",
                    expr->line_num, id, arg_types_buf, overloads_buf);
            }
            else
            {
                /* No overloads found - function is not declared */
                semcheck_error_with_context(
                    "Error on line %d, function %s%s is not declared.\n",
                    expr->line_num, id, arg_types_buf);
            }
        }
        *type_return = UNKNOWN_TYPE;
        final_status = ++return_val;
        goto funccall_cleanup;
    }
    else
    {
        const char *ambig_env = getenv("KGPC_DEBUG_AMBIGUOUS");
        if (ambig_env != NULL && overload_candidates != NULL)
        {
            fprintf(stderr, "[KGPC] Ambiguous call to %s, %d best matches with score %d:\n",
                id ? id : "<unknown>", num_best_matches, best_score);
            ListNode_t *cur = overload_candidates;
            while (cur != NULL)
            {
                HashNode_t *candidate = (HashNode_t *)cur->cur;
                if (candidate != NULL && candidate->type != NULL &&
                    (candidate->hash_type == HASHTYPE_FUNCTION ||
                     candidate->hash_type == HASHTYPE_PROCEDURE))
                {
                    ListNode_t *candidate_args = kgpc_type_get_procedure_params(candidate->type);
                    fprintf(stderr, "  - %s mangled=%s args=%d kind=%d\n",
                        candidate->id ? candidate->id : "<anon>",
                        candidate->mangled_id ? candidate->mangled_id : "<null>",
                        ListLength(candidate_args),
                        candidate->type->kind);
                }
                cur = cur->next;
            }
        }
        semcheck_error_with_context("Error on line %d, call to function %s is ambiguous\n", expr->line_num, id);
        *type_return = UNKNOWN_TYPE;
        final_status = ++return_val;
        goto funccall_cleanup;
    }


skip_overload_resolution:
    /* Overload resolution completed or skipped for constructors */

    if(scope_return == -1) // Should not happen if match_count > 0
    {
        semcheck_error_with_context("Error on line %d, undeclared function %s (mangled to %s)!\n\n", expr->line_num, id, mangled_name);
        ++return_val;

        *type_return = UNKNOWN_TYPE;
    }
    else
    {
        set_hash_meta(hash_return, mutating);
        if(scope_return > max_scope_lev)
        {
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] semcheck_funccall: scope_return (%d) > max_scope_lev (%d)\n",
                    scope_return, max_scope_lev);
            }
            semcheck_error_with_context("Error on line %d, cannot change \"%s\", invalid scope!\n\n",
                expr->line_num, id);
            fprintf(stderr, "[Was it defined above a function declaration?]\n\n");
            ++return_val;
        }
        if(hash_return->hash_type != HASHTYPE_FUNCTION &&
            hash_return->hash_type != HASHTYPE_FUNCTION_RETURN &&
            hash_return->hash_type != HASHTYPE_PROCEDURE)
        {
            semcheck_error_with_context("Error on line %d, \"%s\" is not a function!\n\n",
                expr->line_num, id);
            ++return_val;
        }

        set_type_from_hashtype(type_return, hash_return);
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && id != NULL &&
            (pascal_identifier_equals(id, "GetAnsiString") ||
             pascal_identifier_equals(id, "GetString")))
        {
            KgpcType *ret_dbg = NULL;
            const char *ret_id_dbg = "<null>";
            if (hash_return->type != NULL && hash_return->type->kind == TYPE_KIND_PROCEDURE)
            {
                ret_dbg = hash_return->type->info.proc_info.return_type;
                if (hash_return->type->info.proc_info.return_type_id != NULL)
                    ret_id_dbg = hash_return->type->info.proc_info.return_type_id;
            }
            fprintf(stderr, "[SemCheck] call %s return_type=%p return_type_id=%s\n",
                id, (void *)ret_dbg, ret_id_dbg);
        }

        /* If a procedure type lacks a resolved return type but has a return_type_id,
         * resolve it now so calls like GetAnsiString() return a value. */
        if (*type_return == PROCEDURE &&
            hash_return->type != NULL &&
            hash_return->type->kind == TYPE_KIND_PROCEDURE &&
            hash_return->type->info.proc_info.return_type == NULL &&
            hash_return->type->info.proc_info.return_type_id != NULL)
        {
            const char *ret_id = hash_return->type->info.proc_info.return_type_id;
            KgpcType *ret_type = NULL;
            HashNode_t *ret_node = semcheck_find_type_node_with_kgpc_type(symtab, ret_id);
            if (ret_node != NULL && ret_node->type != NULL)
            {
                ret_type = ret_node->type;
                kgpc_type_retain(ret_type);
            }
            else
            {
                int mapped = semcheck_map_builtin_type_name(symtab, ret_id);
                if (mapped != UNKNOWN_TYPE)
                    ret_type = create_primitive_type(mapped);
            }

            if (ret_type != NULL)
            {
                hash_return->type->info.proc_info.return_type = ret_type;
                if (ret_type->kind == TYPE_KIND_PRIMITIVE)
                    *type_return = ret_type->info.primitive_type_tag;
                else if (ret_type->kind == TYPE_KIND_POINTER)
                    *type_return = POINTER_TYPE;
                else if (ret_type->kind == TYPE_KIND_RECORD)
                    *type_return = RECORD_TYPE;
                else
                    *type_return = UNKNOWN_TYPE;
            }
        }
        
        /* NEW: Also set the resolved KgpcType for this expression */
        if (hash_return->type != NULL && hash_return->type->kind == TYPE_KIND_PROCEDURE)
        {
            KgpcType *return_type = kgpc_type_get_return_type(hash_return->type);
            if (return_type != NULL)
            {
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] Overwriting resolved_kgpc_type from hash_return return_type\n");
                    fprintf(stderr, "[SemCheck]   return_type kind=%d\n", return_type->kind);
                }
                semcheck_expr_set_resolved_kgpc_type_shared(expr, return_type);
                if (return_type->kind == TYPE_KIND_ARRAY)
                    semcheck_set_array_info_from_kgpctype(expr, symtab, return_type, expr->line_num);
                else
                    semcheck_clear_array_info(expr);
                if (return_type->kind == TYPE_KIND_PRIMITIVE &&
                    return_type->info.primitive_type_tag == UNKNOWN_TYPE)
                {
                    char *target_return_id = hash_return->type->info.proc_info.return_type_id;
                    if (target_return_id != NULL)
                    {
                        HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type(symtab, target_return_id);
                        if (type_node != NULL && type_node->type != NULL)
                        {
                            destroy_kgpc_type(return_type);
                            kgpc_type_retain(type_node->type);
                            hash_return->type->info.proc_info.return_type = type_node->type;
                            return_type = type_node->type;
                            semcheck_expr_set_resolved_kgpc_type_shared(expr, type_node->type);
                            if (return_type->kind == TYPE_KIND_ARRAY)
                                semcheck_set_array_info_from_kgpctype(expr, symtab, return_type, expr->line_num);
                            else
                                semcheck_clear_array_info(expr);
                        }
                    }
                }
            }
            else
            {
                /* Do not clear resolved_kgpc_type here, as it may have been set
                 * by constructor handling logic earlier. */
                semcheck_clear_array_info(expr);
            }
        }
        else
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, hash_return->type);
            semcheck_clear_array_info(expr);
        }

        if (*type_return == RECORD_TYPE)
        {
            struct RecordType *record_type = get_record_type_from_node(hash_return);
            if (record_type != NULL)
                expr->record_type = record_type;
            else
                expr->record_type = NULL;
        }
        else
        {
            expr->record_type = NULL;
        }

        if (hash_return->type != NULL && hash_return->type->kind == TYPE_KIND_PROCEDURE)
        {
            ListNode_t *formal_params = kgpc_type_get_procedure_params(hash_return->type);
            if (semcheck_append_default_args(&args_given, formal_params, expr->line_num) != 0)
                ++return_val;
            expr->expr_data.function_call_data.args_expr = args_given;
        }

        /***** THEN VERIFY ARGS INSIDE *****/
        cur_arg = 0;
        /* Get formal arguments from KgpcType instead of deprecated args field */
        true_args = kgpc_type_get_procedure_params(hash_return->type);
        
        /* For constructors, skip the first argument (class type) in args_given
         * AND skip the first formal parameter (Self) in true_args */
        ListNode_t *args_to_validate = args_given;
        ListNode_t *true_args_to_validate = true_args;
        
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] Constructor %s: args_given=%d, true_args=%d\n",
                id, ListLength(args_given), ListLength(true_args));
        }

        int is_constructor_call = 0;
        if (args_given != NULL && hash_return != NULL)
        {
            const char *method_id = (hash_return->id != NULL) ? hash_return->id : hash_return->mangled_id;
            const char *sep = (method_id != NULL) ? strstr(method_id, "__") : NULL;
            if (sep != NULL && sep != method_id)
            {
                size_t class_len = (size_t)(sep - method_id);
                char *class_name = (char *)malloc(class_len + 1);
                if (class_name != NULL)
                {
                    memcpy(class_name, method_id, class_len);
                    class_name[class_len] = '\0';
                    const char *method_name = sep + 2;
                    struct RecordType *record_info = semcheck_lookup_record_type(symtab, class_name);
                    if (record_info != NULL && record_info->is_class &&
                        method_name != NULL &&
                        (pascal_identifier_equals(method_name, "Create") ||
                         pascal_identifier_equals(method_name, "Destroy")) &&
                        !from_cparser_is_method_static(class_name, method_name))
                    {
                        is_constructor_call = 1;
                    }
                    free(class_name);
                }
            }
        }

        if (is_constructor_call)
        {
            /* If lengths match, we assume first arg is class type and first param is Self -> skip both */
            if (ListLength(args_given) == ListLength(true_args)) {
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) fprintf(stderr, "[SemCheck] Skipping both Self and Class Type\n");
                args_to_validate = args_given->next;
                if (true_args_to_validate != NULL)
                    true_args_to_validate = true_args_to_validate->next;
            }
            /* If args_given is one less, we assume class type is implicit but Self is explicit in params -> skip Self only */
            else if (ListLength(args_given) == ListLength(true_args) - 1) {
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) fprintf(stderr, "[SemCheck] Skipping Self only\n");
                if (true_args_to_validate != NULL)
                    true_args_to_validate = true_args_to_validate->next;
            }
        }
            
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                int args_given_count = 0;
                int true_args_count = 0;
                int args_to_validate_count = 0;
                for (ListNode_t *c = args_given; c != NULL; c = c->next) args_given_count++;
                for (ListNode_t *c = true_args_to_validate; c != NULL; c = c->next) true_args_count++;
                for (ListNode_t *c = args_to_validate; c != NULL; c = c->next) args_to_validate_count++;
                fprintf(stderr, "[SemCheck] Constructor %s: args_given=%d, true_args=%d, args_to_validate=%d\n",
                    id, args_given_count, true_args_count, args_to_validate_count);
            }
        
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] Before validation loop: true_args len=%d, args_given len=%d\n", 
                ListLength(true_args), ListLength(args_given));
            
            ListNode_t *cur = true_args;
            int i = 0;
            while (cur != NULL) {
                Tree_t *decl = (Tree_t *)cur->cur;
                if (decl->type == TREE_VAR_DECL) {
                    ListNode_t *ids = decl->tree_data.var_decl_data.ids;
                    while (ids != NULL) {
                        fprintf(stderr, "[SemCheck]   true_arg[%d]: %s\n", i++, (char*)ids->cur);
                        ids = ids->next;
                    }
                } else {
                    fprintf(stderr, "[SemCheck]   true_arg[%d]: (array decl)\n", i++);
                }
                cur = cur->next;
            }
            
            cur = args_given;
            i = 0;
            while (cur != NULL) {
                struct Expression *e = (struct Expression *)cur->cur;
                fprintf(stderr, "[SemCheck]   arg_given[%d]: type=%d\n", i++, e->type);
                cur = cur->next;
            }
        }
        
        while(true_args_to_validate != NULL && args_to_validate != NULL)
        {
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] validation loop: arg %d\n", cur_arg);
                fprintf(stderr, "[SemCheck]   true_args_to_validate=%p next=%p\n", 
                    (void*)true_args_to_validate, (void*)true_args_to_validate->next);
                fprintf(stderr, "[SemCheck]   args_to_validate=%p next=%p\n", 
                    (void*)args_to_validate, (void*)args_to_validate->next);
            }
            cur_arg++;
            assert(args_to_validate->type == LIST_EXPR);
            assert(true_args_to_validate->type == LIST_TREE);

            arg_decl = (Tree_t *)true_args_to_validate->cur;
            if (arg_decl->type != TREE_VAR_DECL && arg_decl->type != TREE_ARR_DECL)
            {
                semcheck_error_with_context("Error on line %d, unsupported parameter declaration in call to %s.\n",
                    expr->line_num, id);
                ++return_val;
                true_args_to_validate = true_args_to_validate->next;
                args_to_validate = args_to_validate->next;
                continue;
            }
            int named_arg_mismatch = 0;
            struct Expression *current_arg_expr = (struct Expression *)args_to_validate->cur;
            if (current_arg_expr != NULL && current_arg_expr->type == EXPR_RELOP &&
                current_arg_expr->expr_data.relop_data.type == EQ &&
                current_arg_expr->expr_data.relop_data.left != NULL &&
                current_arg_expr->expr_data.relop_data.left->type == EXPR_VAR_ID &&
                current_arg_expr->expr_data.relop_data.right != NULL)
            {
                const char *expected_name = NULL;
                if (arg_decl->type == TREE_VAR_DECL &&
                    arg_decl->tree_data.var_decl_data.ids != NULL)
                    expected_name = (const char *)arg_decl->tree_data.var_decl_data.ids->cur;
                else if (arg_decl->type == TREE_ARR_DECL &&
                    arg_decl->tree_data.arr_decl_data.ids != NULL)
                    expected_name = (const char *)arg_decl->tree_data.arr_decl_data.ids->cur;

                const char *given_name = current_arg_expr->expr_data.relop_data.left->expr_data.id;
                if (expected_name != NULL && given_name != NULL)
                {
                    if (pascal_identifier_equals(expected_name, given_name))
                    {
                        struct Expression *named_value = current_arg_expr->expr_data.relop_data.right;
                        int rhs_type = UNKNOWN_TYPE;
                        semcheck_expr_legacy_tag(&rhs_type, symtab, named_value, max_scope_lev, NO_MUTATE);
                        if (semcheck_named_arg_type_compatible(arg_decl, named_value, rhs_type, symtab))
                        {
                            struct Expression *named_left = current_arg_expr->expr_data.relop_data.left;
                            current_arg_expr->expr_data.relop_data.left = NULL;
                            current_arg_expr->expr_data.relop_data.right = NULL;
                            destroy_expr(named_left);
                            destroy_expr(current_arg_expr);
                            current_arg_expr = named_value;
                            args_to_validate->cur = current_arg_expr;
                        }
                    }
                    else if (semcheck_param_list_contains_name(true_args, given_name))
                    {
                        named_arg_mismatch = 1;
                    }
                }
            }
            if (arg_decl->type == TREE_ARR_DECL)
            {
                if (semcheck_prepare_array_literal_argument(arg_decl, current_arg_expr,
                        symtab, max_scope_lev, expr->line_num) != 0)
                {
                    ++return_val;
                    true_args_to_validate = true_args_to_validate->next;
                    args_to_validate = args_to_validate->next;
                    continue;
                }
            }
            if (semcheck_prepare_record_constructor_argument(arg_decl, current_arg_expr,
                    symtab, max_scope_lev, expr->line_num) != 0)
            {
                ++return_val;
                true_args_to_validate = true_args_to_validate->next;
                args_to_validate = args_to_validate->next;
                continue;
            }

            /* Check if the formal parameter is a var or out parameter.
             * If so, mark the actual argument expression as mutated. */
            int arg_mutating = NO_MUTATE;
            if (arg_decl->type == TREE_VAR_DECL && arg_decl->tree_data.var_decl_data.is_var_param)
            {
                arg_mutating = MUTATE;
            }

            return_val += semcheck_expr_legacy_tag(&arg_type,
                symtab, current_arg_expr, max_scope_lev, arg_mutating);
            if (named_arg_mismatch)
                arg_type = UNKNOWN_TYPE;
            if (getenv("KGPC_DEBUG_CALL_TYPES") != NULL &&
                id != NULL &&
                (pascal_identifier_equals(id, "FileDateToDateTime") ||
                 pascal_identifier_equals(id, "FileDateToUniversal")))
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_CALL_TYPES] call=%s arg=%d semcheck_type=%d resolved=%d\n",
                    id, cur_arg, arg_type,
                    current_arg_expr != NULL ? (current_arg_expr->resolved_kgpc_type ? semcheck_tag_from_kgpc(current_arg_expr->resolved_kgpc_type) : UNKNOWN_TYPE) : -1);
                if (current_arg_expr != NULL)
                    semcheck_debug_expr_brief(current_arg_expr, "call arg");
            }
            if (arg_decl->type == TREE_VAR_DECL)
                true_arg_ids = arg_decl->tree_data.var_decl_data.ids;
            else
                true_arg_ids = arg_decl->tree_data.arr_decl_data.ids;

            while(true_arg_ids != NULL && args_to_validate != NULL)
            {
                int expected_type = resolve_param_type(arg_decl, symtab);

                if (arg_decl->type == TREE_ARR_DECL && current_arg_expr != NULL &&
                    current_arg_expr->type == EXPR_ARRAY_LITERAL)
                {
                    arg_type = expected_type;
                }
                if (expected_type == UNKNOWN_TYPE || expected_type == BUILTIN_ANY_TYPE)
                {
                    /* Untyped parameters accept any argument type. */
                    /* No validation needed. */
                }
                else if(arg_type != expected_type)
                {
                    /* Allow integer type conversion (INT_TYPE, LONGINT_TYPE, INT64_TYPE all compatible) */
                    int type_compatible = 0;
                    if (is_integer_type(arg_type) && is_integer_type(expected_type))
                    {
                        type_compatible = 1;
                    }
                    else if (expected_type == STRING_TYPE && arg_type == CHAR_TYPE)
                    {
                        type_compatible = 1;
                    }
                    else if (expected_type == STRING_TYPE && arg_type == SHORTSTRING_TYPE)
                    {
                        type_compatible = 1;
                    }
                    else if (expected_type == SHORTSTRING_TYPE && arg_type == STRING_TYPE)
                    {
                        type_compatible = 1;
                    }
                    else if (expected_type == REAL_TYPE && is_integer_type(arg_type))
                    {
                        type_compatible = 1;
                    }
                    /* For enum types: integer literals and scoped enum literals are compatible with enum parameters */
                    else if (expected_type == ENUM_TYPE && 
                             (is_integer_type(arg_type) || arg_type == ENUM_TYPE))
                    {
                        type_compatible = 1;
                    }
                    /* For array of const: accept any array literal - elements are converted to TVarRec */
                    else if (expected_type == ARRAY_OF_CONST_TYPE &&
                             current_arg_expr != NULL &&
                             current_arg_expr->type == EXPR_ARRAY_LITERAL)
                    {
                        type_compatible = 1;
                    }
                    /* For complex types (records, files, etc.), if both have the same type tag,
                     * consider them compatible. The overload resolution already ensured we have
                     * the right function via name mangling. */
                    else if (arg_type == expected_type)
                    {
                        type_compatible = 1;
                    }

                    /* Allow string literals to be passed as PAnsiChar/PChar parameters.
                     * In Pascal, string literals are implicitly convertible to PChar. */
                    if (!type_compatible && current_arg_expr != NULL &&
                        (arg_type == STRING_TYPE || arg_type == SHORTSTRING_TYPE ||
                         current_arg_expr->type == EXPR_STRING))
                    {
                        const char *param_type_id = NULL;
                        if (arg_decl->type == TREE_VAR_DECL)
                            param_type_id = arg_decl->tree_data.var_decl_data.type_id;
                        if (param_type_id != NULL &&
                            (pascal_identifier_equals(param_type_id, "PAnsiChar") ||
                             pascal_identifier_equals(param_type_id, "PChar") ||
                             pascal_identifier_equals(param_type_id, "PWideChar")))
                        {
                            type_compatible = 1;
                        }
                        /* Also check if parameter is a pointer type pointing to char */
                        if (!type_compatible && expected_type == POINTER_TYPE)
                        {
                            int owns_expected = 0;
                            KgpcType *expected_kgpc = resolve_type_from_vardecl(arg_decl, symtab, &owns_expected);
                            if (expected_kgpc != NULL && kgpc_type_is_pointer(expected_kgpc))
                            {
                                KgpcType *points_to = expected_kgpc->info.points_to;
                                if (points_to != NULL && 
                                    points_to->kind == TYPE_KIND_PRIMITIVE &&
                                    points_to->info.primitive_type_tag == CHAR_TYPE)
                                {
                                    type_compatible = 1;
                                }
                            }
                            if (owns_expected && expected_kgpc != NULL)
                                destroy_kgpc_type(expected_kgpc);
                        }
                    }

                    /* Allow dynamic array parameters to match when both sides are arrays. */
                    if (!type_compatible && current_arg_expr != NULL)
                    {
                        int owns_expected = 0;
                        KgpcType *expected_kgpc = resolve_type_from_vardecl(arg_decl, symtab, &owns_expected);
                        if (expected_kgpc != NULL && expected_kgpc->kind == TYPE_KIND_ARRAY)
                        {
                            KgpcType *arg_kgpc = current_arg_expr->resolved_kgpc_type;
                            if (arg_kgpc != NULL && arg_kgpc->kind == TYPE_KIND_ARRAY)
                            {
                                if (are_types_compatible_for_assignment(expected_kgpc, arg_kgpc, symtab))
                                    type_compatible = 1;
                            }
                            else if (current_arg_expr->is_array_expr)
                            {
                                KgpcType *expected_elem = expected_kgpc->info.array_info.element_type;
                                if (expected_elem != NULL && expected_elem->kind == TYPE_KIND_PRIMITIVE &&
                                    current_arg_expr->array_element_type != UNKNOWN_TYPE &&
                                    expected_elem->info.primitive_type_tag == current_arg_expr->array_element_type)
                                {
                                    type_compatible = 1;
                                }
                            }
                        }
                        if (owns_expected && expected_kgpc != NULL)
                            destroy_kgpc_type(expected_kgpc);
                    }

                    if (!type_compatible && arg_type == UNKNOWN_TYPE &&
                        current_arg_expr != NULL && current_arg_expr->type == EXPR_VAR_ID)
                    {
                        HashNode_t *arg_node = NULL;
                        if (FindIdent(&arg_node, symtab, current_arg_expr->expr_data.id) == 0 &&
                            arg_node != NULL && arg_node->type == NULL)
                        {
                            type_compatible = 1;
                        }
                    }
                    
                    if (!type_compatible)
                    {
                        if (getenv("KGPC_DEBUG_CALL_TYPES") != NULL &&
                            id != NULL &&
                            (pascal_identifier_equals(id, "FileDateToDateTime") ||
                             pascal_identifier_equals(id, "FileDateToUniversal") ||
                             (hash_return != NULL && hash_return->id != NULL &&
                              pascal_identifier_equals(hash_return->id, "TGUIDHelper__Create"))))
                        {
                            fprintf(stderr,
                                "[KGPC_DEBUG_CALL_TYPES] call=%s arg=%d expected=%d actual=%d expr_type=%d kgpc_kind=%d\n",
                                id, cur_arg, expected_type, arg_type,
                                current_arg_expr ? current_arg_expr->type : -1,
                                (current_arg_expr != NULL && current_arg_expr->resolved_kgpc_type != NULL) ?
                                    current_arg_expr->resolved_kgpc_type->kind : -1);
                        }
                        /* Check for Char -> PChar mismatch (workaround for missing @ parser issue) */
                        if (!type_compatible && expected_type == POINTER_TYPE && arg_type == CHAR_TYPE)
                        {
                            int expected_is_pchar = 0;
                            int owns_expected = 0;
                            KgpcType *expected_kgpc = resolve_type_from_vardecl(arg_decl, symtab, &owns_expected);
                            
                            if (expected_kgpc != NULL && kgpc_type_is_pointer(expected_kgpc))
                            {
                                KgpcType *points_to = expected_kgpc->info.points_to;
                                if (points_to != NULL && 
                                    points_to->kind == TYPE_KIND_PRIMITIVE &&
                                    points_to->info.primitive_type_tag == CHAR_TYPE)
                                {
                                    expected_is_pchar = 1;
                                }
                            }
                            
                            if (expected_is_pchar)
                            {
                                /* Wrap in EXPR_ADDR */
                                struct Expression *addr_expr = mk_addressof(current_arg_expr->line_num, current_arg_expr);
                                int new_arg_type = UNKNOWN_TYPE;
                                semcheck_expr_legacy_tag(&new_arg_type, symtab, addr_expr, max_scope_lev, NO_MUTATE);
                                
                                if (new_arg_type == POINTER_TYPE)
                                {
                                    /* Fix applied */
                                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                                        fprintf(stderr, "[SemCheck] Auto-fixing missing @ for PChar argument\n");
                                        
                                    /* Update list node */
                                    args_to_validate->cur = addr_expr;
                                    /* Update current_arg_expr */
                                    current_arg_expr = addr_expr;
                                    arg_type = new_arg_type;
                                    type_compatible = 1;
                                }
                                else
                                {
                                    /* Failed to fix, discard wrapper */
                                    addr_expr->expr_data.addr_data.expr = NULL; /* Detach child */
                                    free(addr_expr); /* Shallow free since we detached child */
                                }
                            }
                            
                            if (owns_expected && expected_kgpc != NULL)
                                destroy_kgpc_type(expected_kgpc);
                        }

                        if (!type_compatible)
                        {
                            /* Get better type descriptions */
                            const char *expected_str = type_tag_to_string(expected_type);
                            const char *given_str = type_tag_to_string(arg_type);
                            
                            /* Try to get more detailed types from resolved_kgpc_type */
                            if (current_arg_expr != NULL && current_arg_expr->resolved_kgpc_type != NULL)
                                given_str = kgpc_type_to_string(current_arg_expr->resolved_kgpc_type);
                            
                            semcheck_error_with_context("Error on line %d, on function call %s, argument %d: Type mismatch (expected: %s, given: %s)!\n\n",
                                expr->line_num, id, cur_arg, expected_str, given_str);
                            ++return_val;
                        }
                    }
                }

                true_arg_ids = true_arg_ids->next;
                args_to_validate = args_to_validate->next;
            }

            true_args_to_validate = true_args_to_validate->next;
        }
        if(true_args_to_validate == NULL && args_to_validate != NULL)
        {
            int allow_implicit_self_only = 0;
            if (expr->expr_data.function_call_data.resolved_func != NULL &&
                expr->expr_data.function_call_data.resolved_func->mangled_id != NULL &&
                strstr(expr->expr_data.function_call_data.resolved_func->mangled_id, "__") != NULL)
            {
                if (args_to_validate->next == NULL)
                {
                    struct Expression *only_arg = (struct Expression *)args_to_validate->cur;
                    if (only_arg != NULL &&
                        only_arg->type == EXPR_VAR_ID &&
                        only_arg->expr_data.id != NULL &&
                        pascal_identifier_equals(only_arg->expr_data.id, "Self"))
                    {
                        allow_implicit_self_only = 1;
                    }
                }
            }
            if (allow_implicit_self_only)
            {
                args_to_validate = NULL;
            }
            if (args_to_validate == NULL)
            {
                /* Skip error for implicit Self-only calls with missing formal params */
            }
            else
            {
            int allow_forward_params = 0;
            if (hash_return != NULL && hash_return->type != NULL &&
                hash_return->type->kind == TYPE_KIND_PROCEDURE &&
                hash_return->type->info.proc_info.params == NULL &&
                hash_return->type->info.proc_info.definition == NULL)
            {
                allow_forward_params = 1;
            }
            if (!allow_forward_params)
            {
                semcheck_error_with_context("Error on line %d, on function call %s, too many arguments given!\n\n",
                    expr->line_num, id);
                ++return_val;
            }
            }
        }
        else if(true_args_to_validate != NULL && args_to_validate == NULL)
        {
            semcheck_error_with_context("Error on line %d, on function call %s, not enough arguments given!\n\n",
                expr->line_num, id);
            ++return_val;
        }
    }

    final_status = return_val;

funccall_cleanup:
    if (FUNCCALL_TIMINGS_ENABLED()) {
        fprintf(stderr, "[timing] funccall exit id=%s total: %.2f ms\n",
            id != NULL ? id : "(null)", funccall_now_ms() - timing_start_ms);
    }
    if (overload_candidates != NULL)
        DestroyList(overload_candidates);
    if (mangled_name != NULL)
        free(mangled_name);
    return final_status;
}

int semcheck_try_indexed_property_getter(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    if (type_return == NULL || symtab == NULL || expr == NULL)
        return -1;
    if (mutating != NO_MUTATE)
        return -1;
    if (expr->type != EXPR_ARRAY_ACCESS)
        return -1;

    struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
    struct Expression *index_expr = expr->expr_data.array_access_data.index_expr;
    if (array_expr == NULL)
        return -1;

    const char *base_id = NULL;
    if (array_expr->type == EXPR_VAR_ID)
        base_id = array_expr->expr_data.id;
    else if (array_expr->type == EXPR_FUNCTION_CALL &&
             array_expr->expr_data.function_call_data.args_expr == NULL)
        base_id = array_expr->expr_data.function_call_data.id;

    if (base_id == NULL || index_expr == NULL)
        return -1;

    size_t id_len = strlen(base_id);
    char *getter_id = (char *)malloc(id_len + 4);
    if (getter_id == NULL)
        return -1;
    snprintf(getter_id, id_len + 4, "Get%s", base_id);

    HashNode_t *getter_node = NULL;
    int getter_found = (FindIdent(&getter_node, symtab, getter_id) == 0);
    if (!getter_found || getter_node == NULL || getter_node->hash_type != HASHTYPE_FUNCTION)
    {
        free(getter_id);
        return -1;
    }

    ListNode_t *args = CreateListNode(index_expr, LIST_EXPR);
    if (args == NULL)
    {
        free(getter_id);
        return -1;
    }

    expr->type = EXPR_FUNCTION_CALL;
    memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
    expr->expr_data.function_call_data.id = getter_id;
    expr->expr_data.function_call_data.args_expr = args;
    expr->expr_data.function_call_data.mangled_id = NULL;
    semcheck_reset_function_call_cache(expr);

    destroy_expr(array_expr);

    return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
}
