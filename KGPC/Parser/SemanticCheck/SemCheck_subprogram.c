/* SemCheck_subprogram.c */
#include "SemCheck_Internal.h"

void copy_default_values_to_impl_params(ListNode_t *fwd_params, ListNode_t *impl_params)
{
    if (fwd_params == NULL || impl_params == NULL)
        return;

    ListNode_t *fwd = fwd_params;
    ListNode_t *impl = impl_params;

    while (fwd != NULL && impl != NULL)
    {
        Tree_t *fwd_decl = (Tree_t *)fwd->cur;
        Tree_t *impl_decl = (Tree_t *)impl->cur;

        if (fwd_decl != NULL && impl_decl != NULL)
        {
            /* Copy default value from forward declaration if impl doesn't have one */
            if (fwd_decl->type == TREE_VAR_DECL && impl_decl->type == TREE_VAR_DECL)
            {
                struct Statement *fwd_init = fwd_decl->tree_data.var_decl_data.initializer;
                if (fwd_init != NULL &&
                    impl_decl->tree_data.var_decl_data.initializer == NULL)
                {
                    /* The initializer is a STMT_VAR_ASSIGN with NULL var, containing the expression */
                    if (fwd_init->type == STMT_VAR_ASSIGN &&
                        fwd_init->stmt_data.var_assign_data.expr != NULL)
                    {
                        struct Expression *cloned_expr = clone_expression(
                            fwd_init->stmt_data.var_assign_data.expr);
                        if (cloned_expr != NULL)
                        {
                            impl_decl->tree_data.var_decl_data.initializer =
                                mk_varassign(fwd_init->line_num, fwd_init->col_num, NULL, cloned_expr);
                        }
                    }
                }
            }
            else if (fwd_decl->type == TREE_ARR_DECL && impl_decl->type == TREE_ARR_DECL)
            {
                struct Statement *fwd_init = fwd_decl->tree_data.arr_decl_data.initializer;
                if (fwd_init != NULL &&
                    impl_decl->tree_data.arr_decl_data.initializer == NULL)
                {
                    if (fwd_init->type == STMT_VAR_ASSIGN &&
                        fwd_init->stmt_data.var_assign_data.expr != NULL)
                    {
                        struct Expression *cloned_expr = clone_expression(
                            fwd_init->stmt_data.var_assign_data.expr);
                        if (cloned_expr != NULL)
                        {
                            impl_decl->tree_data.arr_decl_data.initializer =
                                mk_varassign(fwd_init->line_num, fwd_init->col_num, NULL, cloned_expr);
                        }
                    }
                }
            }
        }

        fwd = fwd->next;
        impl = impl->next;
    }
}

/**
 * For a method implementation (ClassName__MethodName), add class vars to scope
 * so they can be referenced within the method body. This is essential for
 * static methods which have no implicit Self parameter.
 */
void add_class_vars_to_method_scope(SymTab_t *symtab, const char *method_id)
{
    if (symtab == NULL || method_id == NULL)
        return;

    if (getenv("KGPC_DEBUG_CLASS_VAR") != NULL)
        fprintf(stderr, "[KGPC_DEBUG_CLASS_VAR] method_id=%s\n", method_id ? method_id : "<null>");

    /* Check if this is a method (contains "__") */
    const char *sep = strstr(method_id, "__");
    if (sep == NULL || sep == method_id)
        return;

    /* Extract class name */
    size_t class_name_len = (size_t)(sep - method_id);
    char *class_name = (char *)malloc(class_name_len + 1);
    if (class_name == NULL)
        return;
    memcpy(class_name, method_id, class_name_len);
    class_name[class_name_len] = '\0';

    /* For generic types (containing < or >), skip this lookup as
     * the type resolution is handled differently for generics. */
    if (strchr(class_name, '<') != NULL || strchr(class_name, '>') != NULL)
    {
        free(class_name);
        return;
    }

    if (from_cparser_is_type_helper(class_name))
    {
        free(class_name);
        return;
    }

    /* Extract method name */
    const char *method_name = sep + 2;
    if (method_name[0] == '\0')
    {
        free(class_name);
        return;
    }

    /* Only add class fields for static methods. Non-static methods
     * access fields via Self which is handled differently. */
    if (!from_cparser_is_method_static(class_name, method_name))
    {
        free(class_name);
        return;
    }

    /* Look up the class type */
    HashNode_t *class_node = NULL;
    int lookup_result = FindIdent(&class_node, symtab, class_name);
    if (lookup_result == -1 || class_node == NULL)
    {
        free(class_name);
        return;
    }

    struct RecordType *record_info = hashnode_get_record_type_extended(class_node);
    if (record_info == NULL)
    {
        free(class_name);
        return;
    }

    /* Skip type helpers entirely; they do not expose class vars like real classes. */
    if (record_info->is_type_helper)
    {
        free(class_name);
        return;
    }

    /* Only process real class types, not regular records */
    if (!record_info->is_class)
    {
        free(class_name);
        return;
    }

    /* Add class vars from the record fields to the current scope.
     * Class vars are stored in the record's fields list.
     * For class types, all fields are accessible in methods. */
    ListNode_t *field_node = record_info->fields;
    while (field_node != NULL)
    {
        if (field_node->type != LIST_RECORD_FIELD)
        {
            field_node = field_node->next;
            continue;
        }
        struct RecordField *field = (struct RecordField *)field_node->cur;
        if (field != NULL && field->name != NULL && field->name[0] != '\0')
        {
            if (getenv("KGPC_DEBUG_CLASS_VAR") != NULL &&
                pascal_identifier_equals(field->name, "FStandardEncodings"))
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_CLASS_VAR] class=%s field=%s is_array=%d elem_type=%d elem_type_id=%s\n",
                    class_name ? class_name : "<null>",
                    field->name,
                    field->is_array,
                    field->array_element_type,
                    field->array_element_type_id ? field->array_element_type_id : "<null>");
            }
            /* Check if already defined in scope */
            HashNode_t *existing = NULL;
            if (FindIdent(&existing, symtab, field->name) == -1)
            {
                /* Build a KgpcType for this field if needed */
                KgpcType *field_type = NULL;
                if (field->type_id != NULL && field->type_id[0] != '\0')
                {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, field->type_id) != -1 &&
                        type_node != NULL && type_node->type != NULL)
                    {
                        field_type = type_node->type;
                        kgpc_type_retain(field_type);
                    }
                }
                else if (field->is_array)
                {
                    KgpcType *elem_type = NULL;
                    if (field->array_element_type_id != NULL)
                    {
                        HashNode_t *elem_node = NULL;
                        if (FindIdent(&elem_node, symtab, field->array_element_type_id) != -1 &&
                            elem_node != NULL && elem_node->type != NULL)
                        {
                            elem_type = elem_node->type;
                            kgpc_type_retain(elem_type);
                        }
                        else if (getenv("KGPC_DEBUG_CLASS_VAR") != NULL &&
                            pascal_identifier_equals(field->name, "FStandardEncodings"))
                        {
                            fprintf(stderr,
                                "[KGPC_DEBUG_CLASS_VAR] element type lookup failed for %s (node=%p type=%p)\n",
                                field->array_element_type_id ? field->array_element_type_id : "<null>",
                                (void *)elem_node,
                                elem_node ? (void *)elem_node->type : NULL);
                        }
                        else if (elem_node != NULL)
                        {
                            struct RecordType *elem_record = hashnode_get_record_type_extended(elem_node);
                            if (elem_record != NULL)
                                elem_type = create_record_type(elem_record);
                        }
                    }
                    else if (field->array_element_type != UNKNOWN_TYPE)
                    {
                        elem_type = create_primitive_type(field->array_element_type);
                    }
                    if (elem_type != NULL)
                    {
                        if (getenv("KGPC_DEBUG_CLASS_VAR") != NULL &&
                            pascal_identifier_equals(field->name, "FStandardEncodings"))
                        {
                            fprintf(stderr,
                                "[KGPC_DEBUG_CLASS_VAR] resolved elem kgpc=%s kind=%d\n",
                                kgpc_type_to_string(elem_type),
                                elem_type ? elem_type->kind : -1);
                        }
                        field_type = create_array_type(elem_type, field->array_start, field->array_end);
                    }
                }

                /* Push onto scope - using typed variant */
                if (getenv("KGPC_DEBUG_CLASS_VAR") != NULL &&
                    pascal_identifier_equals(field->name, "FStandardEncodings"))
                {
                    fprintf(stderr,
                        "[KGPC_DEBUG_CLASS_VAR] field_type=%s\n",
                        field_type ? kgpc_type_to_string(field_type) : "<null>");
                }
                PushVarOntoScope_Typed(symtab, field->name, field_type);

                /* Release our ref if we retained it */
                if (field_type != NULL)
                    destroy_kgpc_type(field_type);
            }
        }
        field_node = field_node->next;
    }

    /* Add other static methods of the same class to scope, so they can be
     * called without full qualification from within a static method. */
    ListNode_t *class_methods = NULL;
    int method_count = 0;
    get_class_methods(class_name, &class_methods, &method_count);

    ListNode_t *cur_method = class_methods;
    while (cur_method != NULL)
    {
        ClassMethodBinding *binding = (ClassMethodBinding *)cur_method->cur;
        if (binding != NULL && binding->method_name != NULL && binding->method_name[0] != '\0')
        {
            /* Build the mangled name: ClassName__MethodName */
            size_t mangled_len = strlen(class_name) + 2 + strlen(binding->method_name) + 1;
            char *mangled_name = (char *)malloc(mangled_len);
            if (mangled_name != NULL)
            {
                snprintf(mangled_name, mangled_len, "%s__%s", class_name, binding->method_name);

                /* Look up the mangled method in the symbol table */
                HashNode_t *method_node = NULL;
                if (FindIdent(&method_node, symtab, mangled_name) != -1 && method_node != NULL)
                {
                    /* Add an alias using just the method name */
                    HashNode_t *existing = NULL;
                    if (FindIdent(&existing, symtab, binding->method_name) == -1)
                    {
                        /* Push the method onto scope using its short name */
                        if (method_node->type != NULL)
                        {
                            kgpc_type_retain(method_node->type);
                            PushFunctionOntoScope_Typed(symtab, binding->method_name,
                                                        mangled_name, method_node->type);
                            destroy_kgpc_type(method_node->type);
                        }
                    }
                }
                free(mangled_name);
            }
        }
        cur_method = cur_method->next;
    }

    /* Clean up the class_methods list */
    while (class_methods != NULL)
    {
        ListNode_t *next = class_methods->next;
        free(class_methods);
        class_methods = next;
    }

    free(class_name);
}

/**
 * For a method implementation (ClassName__MethodName), copy default parameter
 * values from the class declaration to the implementation's parameters.
 * This is needed because default values are specified in the class declaration
 * but not repeated in the implementation.
 */
void copy_method_decl_defaults_to_impl(SymTab_t *symtab, Tree_t *subprogram)
{
    if (symtab == NULL || subprogram == NULL)
        return;

    const char *method_id = subprogram->tree_data.subprogram_data.id;
    if (method_id == NULL)
        return;

    /* Check if this is a method (contains "__") */
    const char *sep = strstr(method_id, "__");
    if (sep == NULL || sep == method_id)
        return;

    /* Extract class name */
    size_t class_name_len = (size_t)(sep - method_id);
    char *class_name = (char *)malloc(class_name_len + 1);
    if (class_name == NULL)
        return;
    memcpy(class_name, method_id, class_name_len);
    class_name[class_name_len] = '\0';

    /* Extract method name */
    const char *method_name = sep + 2;
    if (method_name[0] == '\0')
    {
        free(class_name);
        return;
    }

    /* Look up the class type */
    HashNode_t *class_node = NULL;
    if (FindIdent(&class_node, symtab, class_name) == -1 || class_node == NULL)
    {
        free(class_name);
        return;
    }

    struct RecordType *record_info = hashnode_get_record_type_extended(class_node);
    if (record_info == NULL)
    {
        free(class_name);
        return;
    }

    /* Find the method template with the declaration's parameters */
    struct MethodTemplate *template = from_cparser_get_method_template(record_info, method_name);
    if (template == NULL || template->params_ast == NULL)
    {
        if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
            fprintf(stderr, "[copy_method_decl_defaults] No method template found for %s.%s\n",
                class_name, method_name);
        free(class_name);
        return;
    }

    if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
        fprintf(stderr, "[copy_method_decl_defaults] Found template for %s.%s, params_ast=%p typ=%d\n",
            class_name, method_name, (void*)template->params_ast, template->params_ast->typ);

    /* Convert the declaration's params_ast to a parameter list */
    /* The params_ast is a PASCAL_T_PARAM or PASCAL_T_PARAM_LIST node */
    ast_t *decl_params_ast = template->params_ast;
    ast_t *param_cursor = decl_params_ast;
    if (decl_params_ast->typ == PASCAL_T_PARAM_LIST)
        param_cursor = decl_params_ast->child;

    /* Iterate through implementation params and declaration params in parallel,
     * copying defaults from declaration to implementation */
    ListNode_t *impl_param = subprogram->tree_data.subprogram_data.args_var;

    /* Skip the Self parameter if present (it won't be in the declaration) */
    if (impl_param != NULL)
    {
        Tree_t *first_param = (Tree_t *)impl_param->cur;
        if (first_param != NULL && first_param->type == TREE_VAR_DECL)
        {
            ListNode_t *ids = first_param->tree_data.var_decl_data.ids;
            if (ids != NULL && ids->cur != NULL)
            {
                const char *param_name = (const char *)ids->cur;
                if (param_name != NULL && strcasecmp(param_name, "Self") == 0)
                    impl_param = impl_param->next;
            }
        }
    }

    while (param_cursor != NULL && impl_param != NULL)
    {
        ast_t *decl_param = param_cursor;
        if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
            fprintf(stderr, "[copy_method_decl_defaults] Processing decl_param typ=%d\n", decl_param->typ);

        if (decl_param->typ == PASCAL_T_PARAM)
        {
            /* Find the TYPE_SPEC and DEFAULT_VALUE in the declaration param */
            ast_t *type_spec = NULL;
            ast_t *default_value = NULL;

            for (ast_t *child = decl_param->child; child != NULL; child = child->next)
            {
                if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                    fprintf(stderr, "[copy_method_decl_defaults]   child typ=%d\n", child->typ);
                if (child->typ == PASCAL_T_TYPE_SPEC)
                    type_spec = child;
                if (child->typ == PASCAL_T_DEFAULT_VALUE)
                    default_value = child;
            }

            /* Check if TYPE_SPEC->next is DEFAULT_VALUE (alternate structure) */
            if (default_value == NULL && type_spec != NULL && type_spec->next != NULL &&
                type_spec->next->typ == PASCAL_T_DEFAULT_VALUE)
            {
                default_value = type_spec->next;
            }

            if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                fprintf(stderr, "[copy_method_decl_defaults] type_spec=%p default_value=%p\n",
                    (void*)type_spec, (void*)default_value);

            if (default_value != NULL)
            {
                /* Get the implementation parameter */
                Tree_t *impl_decl = (Tree_t *)impl_param->cur;
                if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                    fprintf(stderr, "[copy_method_decl_defaults] impl_decl=%p type=%d\n",
                        (void*)impl_decl, impl_decl ? impl_decl->type : -1);

                if (impl_decl != NULL && impl_decl->type == TREE_VAR_DECL)
                {
                    /* Only copy if impl doesn't already have a default */
                    if (impl_decl->tree_data.var_decl_data.initializer == NULL)
                    {
                        /* Convert default_value->child to Expression and wrap in initializer */
                        ast_t *expr_node = default_value->child;
                        if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                            fprintf(stderr, "[copy_method_decl_defaults] expr_node=%p typ=%d\n",
                                (void*)expr_node, expr_node ? expr_node->typ : -1);

                        if (expr_node != NULL)
                        {
                            struct Expression *default_expr = from_cparser_convert_expression(expr_node);
                            if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                                fprintf(stderr, "[copy_method_decl_defaults] default_expr=%p\n",
                                    (void*)default_expr);

                            if (default_expr != NULL)
                            {
                                impl_decl->tree_data.var_decl_data.initializer =
                                    mk_varassign(default_value->line, 0, NULL, default_expr);
                                if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                                    fprintf(stderr, "[copy_method_decl_defaults] COPIED default value!\n");
                            }
                        }
                    }
                    else if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                    {
                        fprintf(stderr, "[copy_method_decl_defaults] impl already has initializer\n");
                    }
                }
            }
        }

        param_cursor = param_cursor->next;
        impl_param = impl_param->next;
    }

    free(class_name);
}

KgpcType *build_function_return_type(Tree_t *subprogram, SymTab_t *symtab,
    int *error_count, int allow_undefined)
{
    const char *debug_env = getenv("KGPC_DEBUG_RETURN_TYPE");
    KgpcType *builtin_return = NULL;
    if (subprogram == NULL || symtab == NULL)
        return NULL;

    /* TODO: Once the symbol table tracks placeholder types, this helper should
     * validate that any returned KgpcType has been fully resolved. */
    HashNode_t *type_node = NULL;
    if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
    {
        type_node = semcheck_find_type_node_with_kgpc_type(symtab, subprogram->tree_data.subprogram_data.return_type_id);
        if (type_node == NULL)
        {
            /* Before reporting error, check for builtin types not in symbol table */
            const char *type_id = subprogram->tree_data.subprogram_data.return_type_id;
            int builtin_type = semcheck_map_builtin_type_name_local(type_id);
            if (builtin_type == UNKNOWN_TYPE)
            {
                if (!allow_undefined)
                {
                    semantic_error(subprogram->line_num, 0, "undefined type %s",
                        subprogram->tree_data.subprogram_data.return_type_id);
                    if (error_count != NULL)
                        ++(*error_count);
                }
            }
            else
            {
                subprogram->tree_data.subprogram_data.return_type = builtin_type;
                builtin_return = create_primitive_type(builtin_type);
            }
        }
    }

    if (debug_env != NULL)
    {
        const char *rt_id = subprogram->tree_data.subprogram_data.return_type_id;
        int primitive_tag = subprogram->tree_data.subprogram_data.return_type;
        const char *resolved_type = (type_node != NULL && type_node->type != NULL)
            ? kgpc_type_to_string(type_node->type)
            : "<null>";
        fprintf(stderr,
            "[KGPC] build_function_return_type: subprogram=%s return_type_id=%s primitive=%d type_node=%p kind=%d resolved=%s\n",
            subprogram->tree_data.subprogram_data.id ? subprogram->tree_data.subprogram_data.id : "<anon>",
            rt_id ? rt_id : "<null>",
            primitive_tag,
            (void *)type_node,
            (type_node != NULL && type_node->type != NULL) ? type_node->type->kind : -1,
            resolved_type);
    }

    if (builtin_return != NULL)
        return builtin_return;

    return kgpc_type_build_function_return(
        subprogram->tree_data.subprogram_data.inline_return_type,
        type_node,
        subprogram->tree_data.subprogram_data.return_type,
        symtab);
}

/* Forward declaration for type resolution helper used in const evaluation. */
int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev)
{
    int return_val, func_return;
    int new_max_scope;
    enum TreeType sub_type;
    struct Statement *body;
    HashNode_t *hash_return;

    assert(symtab != NULL);
    assert(subprogram != NULL);

#ifdef DEBUG
    fprintf(stderr, "DEBUG: semcheck_subprogram called for %s\n", subprogram->tree_data.subprogram_data.id);
#endif

    assert(subprogram->type == TREE_SUBPROGRAM);

    const char *prev_owner = semcheck_get_current_method_owner();
    char *owner_copy = NULL;
    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        const char *mangled = subprogram->tree_data.subprogram_data.mangled_id;
        const char *sep = strstr(mangled, "__");
        if (sep != NULL && sep != mangled)
        {
            size_t len = (size_t)(sep - mangled);
            owner_copy = (char *)malloc(len + 1);
            if (owner_copy != NULL)
            {
                memcpy(owner_copy, mangled, len);
                owner_copy[len] = '\0';
                semcheck_set_current_method_owner(owner_copy);
            }
        }
    }

    /* For class methods, copy default parameter values from the class declaration
     * to the implementation. This is needed because Pascal allows defaults only in
     * the declaration, not in the implementation. */
    copy_method_decl_defaults_to_impl(symtab, subprogram);

    /* Record lexical nesting depth so codegen can reason about static links accurately.
     * Store depth as parent depth + 1 so the top-level program has depth 1 and
     * nested subprograms continue to increase. */
    subprogram->tree_data.subprogram_data.nesting_level = max_scope_lev + 1;
    int default_requires = (subprogram->tree_data.subprogram_data.nesting_level > 1 &&
        !subprogram->tree_data.subprogram_data.defined_in_unit);
    subprogram->tree_data.subprogram_data.requires_static_link = default_requires ? 1 : 0;

    char *id_to_use_for_lookup;

    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val = 0;
    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    // --- Name Mangling Logic ---
    // Only set mangled_id if not already set by predeclare_subprogram (which handles
    // nested functions with unique parent$child naming)
    if (subprogram->tree_data.subprogram_data.mangled_id == NULL) {
        static int debug_external = -1;
        if (debug_external == -1)
            debug_external = (getenv("KGPC_DEBUG_EXTERNAL") != NULL);
        const char *explicit_name = subprogram->tree_data.subprogram_data.cname_override;
        if (explicit_name != NULL) {
            char *overload_mangled = MangleFunctionName(
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.args_var,
                symtab);
            if (overload_mangled != NULL)
                subprogram->tree_data.subprogram_data.mangled_id = overload_mangled;
            else
                subprogram->tree_data.subprogram_data.mangled_id = strdup(explicit_name);
        } else if (subprogram->tree_data.subprogram_data.cname_flag) {
            const char *export_name = subprogram->tree_data.subprogram_data.id;
            if (debug_external) {
                fprintf(stderr, "[SemCheck] cname_flag id=%s alias=%s\n",
                    subprogram->tree_data.subprogram_data.id,
                    export_name != NULL ? export_name : "(null)");
            }
            char *overload_mangled = MangleFunctionName(
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.args_var,
                symtab);
            if (overload_mangled != NULL)
                subprogram->tree_data.subprogram_data.mangled_id = overload_mangled;
            else if (export_name != NULL)
                subprogram->tree_data.subprogram_data.mangled_id = strdup(export_name);
            else
                subprogram->tree_data.subprogram_data.mangled_id = NULL;
        } else {
            // Pass the symbol table to the mangler
            subprogram->tree_data.subprogram_data.mangled_id = MangleFunctionName(
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.args_var,
                symtab);
        }
    }
    id_to_use_for_lookup = subprogram->tree_data.subprogram_data.id;

    /* Check if already declared (e.g., by predeclare_subprogram in two-pass approach) */
    HashNode_t *existing_decl = NULL;
    int already_declared = 0;

    /* For overloaded functions, find the correct overload by matching mangled name */
    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
        ListNode_t *cur = all_matches;

        if (getenv("KGPC_DEBUG_OVERLOAD_MATCH") != NULL)
        {
            fprintf(stderr, "[KGPC] Matching impl %s mangled=%s\n",
                id_to_use_for_lookup ? id_to_use_for_lookup : "<null>",
                subprogram->tree_data.subprogram_data.mangled_id);
            ListNode_t *debug_cur = all_matches;
            while (debug_cur != NULL)
            {
                HashNode_t *debug_cand = (HashNode_t *)debug_cur->cur;
                fprintf(stderr, "  candidate: id=%s mangled=%s\n",
                    debug_cand && debug_cand->id ? debug_cand->id : "<null>",
                    debug_cand && debug_cand->mangled_id ? debug_cand->mangled_id : "<null>");
                debug_cur = debug_cur->next;
            }
        }

        while (cur != NULL && existing_decl == NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL && candidate->mangled_id != NULL &&
                strcmp(candidate->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
            {
                existing_decl = candidate;
                already_declared = 1;
            }
            cur = cur->next;
        }

        if (all_matches != NULL)
            DestroyList(all_matches);
    }

    /* Fallback to simple lookup if no mangled name or no match found */
    if (!already_declared)
        already_declared = (FindIdent(&existing_decl, symtab, id_to_use_for_lookup) == 0);

    if (already_declared && existing_decl != NULL &&
        subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        if (existing_decl->mangled_id != NULL)
            free(existing_decl->mangled_id);
        existing_decl->mangled_id = strdup(subprogram->tree_data.subprogram_data.mangled_id);
    }

    /**** FIRST PLACING SUBPROGRAM ON THE CURRENT SCOPE ****/
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        /* Create KgpcType for the procedure */
        /* Create KgpcType for the procedure */
        KgpcType *proc_type = NULL;
        int created_new_type = 0;

        if (already_declared && existing_decl != NULL && existing_decl->type != NULL)
        {
            proc_type = existing_decl->type;
            Tree_t *prev_def = proc_type->info.proc_info.definition;
            if (subprogram->tree_data.subprogram_data.statement_list != NULL &&
                (prev_def == NULL || prev_def->tree_data.subprogram_data.statement_list == NULL))
            {
                proc_type->info.proc_info.definition = subprogram;
            }
            /* Copy default parameter values from forward declaration to implementation */
            copy_default_values_to_impl_params(
                proc_type->info.proc_info.params,
                subprogram->tree_data.subprogram_data.args_var);
        }
        else
        {
            proc_type = create_procedure_type(
                subprogram->tree_data.subprogram_data.args_var,
                NULL  /* procedures have no return type */
            );
            if (proc_type != NULL)
            {
                proc_type->info.proc_info.definition = subprogram;
                if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                    proc_type->info.proc_info.return_type_id =
                        strdup(subprogram->tree_data.subprogram_data.return_type_id);
            }
            created_new_type = 1;
        }

        /* ARCHITECTURAL FIX: Resolve array bounds in parameter types now that constants are in scope */
        if (proc_type != NULL && proc_type->info.proc_info.params != NULL)
        {
            ListNode_t *param = proc_type->info.proc_info.params;
            while (param != NULL)
            {
                if (param->type == LIST_TREE && param->cur != NULL)
                {
                    Tree_t *param_tree = (Tree_t *)param->cur;
                    /* For array parameters, resolve the bounds */
                    if (param_tree->type == TREE_ARR_DECL)
                    {
                        /* If element type is a named type, look it up to get proper KgpcType */
                        if (param_tree->tree_data.arr_decl_data.type_id != NULL)
                        {
                            HashNode_t *elem_type_node = NULL;
                            if (FindIdent(&elem_type_node, symtab, param_tree->tree_data.arr_decl_data.type_id) != -1 &&
                                elem_type_node != NULL && elem_type_node->type != NULL)
                            {
                                /* Element type resolved - bounds should be in the tree node */
                                /* Nothing to do here */
                            }
                        }
                    }
                }
                param = param->next;
            }
        }

        // Use the typed version to properly set the KgpcType
        // Skip if already declared
        if (!already_declared)
        {
            func_return = PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                            subprogram->tree_data.subprogram_data.mangled_id,
                            proc_type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            FindIdent(&existing_decl, symtab, id_to_use_for_lookup);
        }
        else
        {
            func_return = 0;  /* No error since it's expected to be already declared */

            /* If we created a new type but it was already declared (e.g. existing had no type), update it */
            if (created_new_type && existing_decl != NULL && existing_decl->type == NULL)
            {
                existing_decl->type = proc_type;
            }
            else if (created_new_type)
            {
                /* We created a type but didn't use it (shouldn't happen if logic is correct, but for safety) */
                destroy_kgpc_type(proc_type);
            }
        }

        PushScope(symtab);

        /* For method implementations, add class vars to scope */
        add_class_vars_to_method_scope(symtab, subprogram->tree_data.subprogram_data.id);

        if (existing_decl != NULL && existing_decl->type != NULL)
        {
            kgpc_type_retain(existing_decl->type);
            PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id,
                existing_decl->type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            destroy_kgpc_type(existing_decl->type);
        }

        new_max_scope = max_scope_lev+1;
    }
    else // Function
    {
        KgpcType *return_kgpc_type = NULL;

        /* Reuse the type created during predeclaration when possible. */
        if (already_declared && existing_decl != NULL &&
            existing_decl->type != NULL &&
            existing_decl->type->kind == TYPE_KIND_PROCEDURE)
        {
            return_kgpc_type = kgpc_type_get_return_type(existing_decl->type);
            if (subprogram->tree_data.subprogram_data.statement_list != NULL)
            {
                Tree_t *prev_def = existing_decl->type->info.proc_info.definition;
                if (prev_def == NULL || prev_def->tree_data.subprogram_data.statement_list == NULL)
                {
                    existing_decl->type->info.proc_info.definition = subprogram;
                }
            }
            /* Copy default parameter values from forward declaration to implementation */
            copy_default_values_to_impl_params(
                existing_decl->type->info.proc_info.params,
                subprogram->tree_data.subprogram_data.args_var);
        }

        /* If the predeclare step could not resolve the type (e.g., inline array),
         * build it now and update the existing declaration. */
        if (return_kgpc_type == NULL)
        {
            return_kgpc_type = build_function_return_type(subprogram, symtab, &return_val, 0);
#ifdef DEBUG
            if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after build_function_return_type: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
        }

        KgpcType *func_type = NULL;
        if (!already_declared)
        {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: semcheck_subprogram %s NOT already declared in Pass 2!\n", subprogram->tree_data.subprogram_data.id);
#endif
            func_type = create_procedure_type(
                subprogram->tree_data.subprogram_data.args_var,
                return_kgpc_type
            );
            if (func_type != NULL)
            {
                func_type->info.proc_info.definition = subprogram;
                if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                    func_type->info.proc_info.return_type_id =
                        strdup(subprogram->tree_data.subprogram_data.return_type_id);
            }
            func_return = PushFunctionOntoScope_Typed(symtab, id_to_use_for_lookup,
                            subprogram->tree_data.subprogram_data.mangled_id,
                            func_type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
        }
        else
        {
            func_return = 0;
            if (existing_decl != NULL)
            {
                if (existing_decl->type == NULL)
                {
                    func_type = create_procedure_type(
                        subprogram->tree_data.subprogram_data.args_var,
                        return_kgpc_type
                    );
                    if (func_type != NULL)
                    {
                        func_type->info.proc_info.definition = subprogram;
                        if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                            func_type->info.proc_info.return_type_id =
                                strdup(subprogram->tree_data.subprogram_data.return_type_id);
                    }
                    existing_decl->type = func_type;
                }
                else if (return_kgpc_type != NULL)
                {
                    existing_decl->type->info.proc_info.return_type = return_kgpc_type;
                }
            }
        }

        PushScope(symtab);
        if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL)
            fprintf(stderr, "[KGPC] semcheck_subprogram (func): PushScope for %s\n",
                subprogram->tree_data.subprogram_data.id);

        /* For method implementations, add class vars to scope */
        add_class_vars_to_method_scope(symtab, subprogram->tree_data.subprogram_data.id);

        // **THIS IS THE FIX FOR THE RETURN VALUE**:
        // Use the ORIGINAL name for the internal return variable with KgpcType
        // Always use _Typed variant, even if KgpcType is NULL
        PushFuncRetOntoScope_Typed(symtab, id_to_use_for_lookup, return_kgpc_type);

/* Also add "Result" as an alias for the return variable for Pascal compatibility */
        /* Check if "Result" is already used in the current scope */
        HashNode_t *result_check = NULL;
        HashTable_t *cur_hash = (HashTable_t *)symtab->stack_head->cur;
        result_check = (cur_hash != NULL) ? FindIdentInTable(cur_hash, "Result") : NULL;
        if (result_check == NULL)
        {
            /* "Result" is not already declared in this scope, so add it as an alias */
            PushFuncRetOntoScope_Typed(symtab, "Result", return_kgpc_type);
            if (getenv("KGPC_DEBUG_RESULT") != NULL)
            {
                fprintf(stderr,
                    "[KGPC] add Result alias for %s type=%s\n",
                    subprogram->tree_data.subprogram_data.id ?
                        subprogram->tree_data.subprogram_data.id : "<anon>",
                    kgpc_type_to_string(return_kgpc_type));
            }
        }
        else if (getenv("KGPC_DEBUG_RESULT") != NULL)
        {
            fprintf(stderr,
                "[KGPC] Result alias already exists for %s existing_type=%s\n",
                subprogram->tree_data.subprogram_data.id ?
                    subprogram->tree_data.subprogram_data.id : "<anon>",
                kgpc_type_to_string(result_check->type));
        }

        /* For class methods, also add an alias using the unmangled method name (suffix after __) */
        const char *alias_suffix = NULL;
        if (subprogram->tree_data.subprogram_data.id != NULL)
        {
            const char *sep = strstr(subprogram->tree_data.subprogram_data.id, "__");
            if (sep != NULL && sep[2] != '\0')
                alias_suffix = sep + 2;
        }
        if (alias_suffix != NULL && alias_suffix[0] != '\0')
        {
            size_t alias_len = strcspn(alias_suffix, "_");
            if (alias_len > 0 && alias_len < 128)
            {
                char alias_buf[128];
                memcpy(alias_buf, alias_suffix, alias_len);
                alias_buf[alias_len] = '\0';

                HashNode_t *suffix_check = NULL;
                if (FindIdent(&suffix_check, symtab, alias_buf) == -1)
                    PushFuncRetOntoScope_Typed(symtab, alias_buf, return_kgpc_type);
            }
        }
        /* Note: We don't check for "result" anymore since it conflicts with built-in Result alias */

        /* Note: Type metadata now in KgpcType, no post-creation writes needed */

        new_max_scope = max_scope_lev+1;
    }

    /**** Check the subprogram internals now ****/

    /* Greater than 0 signifies an error */
    if (func_return > 0)
    {
        int current_has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
        HashNode_t *existing_decl = NULL;
        if (FindIdent(&existing_decl, symtab, id_to_use_for_lookup) == 0 &&
            existing_decl != NULL &&
            existing_decl->mangled_id != NULL &&
            subprogram->tree_data.subprogram_data.mangled_id != NULL &&
            strcmp(existing_decl->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
        {
            func_return = 0;
        }
        else
        {
            ListNode_t *candidates = FindAllIdents(symtab, id_to_use_for_lookup);
            ListNode_t *iter = candidates;
            while (iter != NULL)
            {
                HashNode_t *candidate = (HashNode_t *)iter->cur;
                if (candidate != NULL && candidate->type != NULL &&
                    candidate->type->kind == TYPE_KIND_PROCEDURE)
                {
                    Tree_t *def = candidate->type->info.proc_info.definition;
                    int existing_has_body = (def != NULL &&
                        def->tree_data.subprogram_data.statement_list != NULL);
                    if (existing_has_body != current_has_body)
                    {
                        func_return = 0;
                        break;
                    }
                }
                iter = iter->next;
            }
            if (candidates != NULL)
                DestroyList(candidates);
        }
    }
    if(func_return > 0)
    {
        fprintf(stderr, "On line %d: redeclaration of name %s!\n",
            subprogram->line_num, subprogram->tree_data.subprogram_data.id);

        return_val += func_return;
    }

    /* These arguments are themselves like declarations */
    if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL) {
        ListNode_t *arg_debug = subprogram->tree_data.subprogram_data.args_var;
        fprintf(stderr, "[KGPC] semcheck_subprogram: %s args:\n", subprogram->tree_data.subprogram_data.id);
        while (arg_debug != NULL) {
            if (arg_debug->type == LIST_TREE && arg_debug->cur != NULL) {
                Tree_t *arg_tree = (Tree_t *)arg_debug->cur;
                if (arg_tree->type == TREE_VAR_DECL) {
                    ListNode_t *ids = arg_tree->tree_data.var_decl_data.ids;
                    if (ids != NULL && ids->cur != NULL)
                        fprintf(stderr, "  - %s (type_id=%s)\n", (char*)ids->cur,
                            arg_tree->tree_data.var_decl_data.type_id ? arg_tree->tree_data.var_decl_data.type_id : "<null>");
                }
            }
            arg_debug = arg_debug->next;
        }
    }
    return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.args_var);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after args: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    /* Ensure helper methods always have an implicit Self in scope even if the
     * parameter list was parsed without it (e.g., macro-expanded helper types). */
    {
        HashNode_t *self_node = NULL;
        if (FindIdent(&self_node, symtab, "Self") == -1)
        {
            const char *owner_id = semcheck_get_current_method_owner();
            struct RecordType *owner_record = NULL;
            if (owner_id != NULL)
            {
                HashNode_t *owner_node = semcheck_find_preferred_type_node(symtab, owner_id);
                if (owner_node != NULL)
                    owner_record = hashnode_get_record_type_extended(owner_node);
            }
            if (owner_record != NULL && owner_record->is_type_helper &&
                owner_record->helper_base_type_id != NULL)
            {
                KgpcType *self_type = NULL;
                HashNode_t *type_node = semcheck_find_preferred_type_node(symtab,
                    owner_record->helper_base_type_id);
                if (type_node != NULL && type_node->type != NULL)
                {
                    kgpc_type_retain(type_node->type);
                    self_type = type_node->type;
                }
                else
                {
                    int builtin_tag = semcheck_map_builtin_type_name_local(
                        owner_record->helper_base_type_id);
                    if (builtin_tag != UNKNOWN_TYPE)
                        self_type = create_primitive_type(builtin_tag);
                }

                if (self_type != NULL)
                {
                    PushVarOntoScope_Typed(symtab, "Self", self_type);
                    destroy_kgpc_type(self_type);
                }
            }
        }
    }

    return_val += predeclare_enum_literals(symtab, subprogram->tree_data.subprogram_data.type_declarations);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    return_val += predeclare_types(symtab, subprogram->tree_data.subprogram_data.type_declarations);
    return_val += semcheck_const_decls(symtab, subprogram->tree_data.subprogram_data.const_declarations);
    return_val += semcheck_type_decls(symtab, subprogram->tree_data.subprogram_data.type_declarations);
    return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.declarations);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after decls: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    return_val += semcheck_subprograms(symtab, subprogram->tree_data.subprogram_data.subprograms,
                    new_max_scope, subprogram);

    Tree_t *prev_current_subprogram = g_semcheck_current_subprogram;
    g_semcheck_current_subprogram = subprogram;

    body = subprogram->tree_data.subprogram_data.statement_list;
    if (body == NULL)
    {
        g_semcheck_current_subprogram = prev_current_subprogram;
        PopScope(symtab);
#ifdef DEBUG
        fprintf(stderr, "DEBUG: semcheck_subprogram %s returning (no body): %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
        return return_val;
    }

    /* Functions cannot have side effects, so need to call a special function in that case */
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        return_val += semcheck_stmt(symtab,
                body,
                new_max_scope);
    }
    else
    {
        assert(FindIdent(&hash_return, symtab, subprogram->tree_data.subprogram_data.id)
                    == 0);

        ResetHashNodeStatus(hash_return);
        int func_stmt_ret = semcheck_func_stmt(symtab,
                body, new_max_scope);
        return_val += func_stmt_ret;
#ifdef DEBUG
        if (func_stmt_ret > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after semcheck_func_stmt: %d\n", subprogram->tree_data.subprogram_data.id, func_stmt_ret);
#endif

        /* Allow functions with asm blocks to skip explicit return assignment */
        int has_asm = statement_contains_asm_block(body);

        /* Constructors implicitly yield the constructed instance, so do not
         * require an explicit assignment to the return variable. */
        int is_constructor = 0;
        if (subprogram->tree_data.subprogram_data.id != NULL) {
            const char *suffix = strstr(subprogram->tree_data.subprogram_data.id, "__");
            const char *name = (suffix != NULL && suffix[2] != '\0') ? suffix + 2
                                                                     : subprogram->tree_data.subprogram_data.id;
            if (strcasecmp(name, "create") == 0) {
                is_constructor = 1;
            }
        }

        /* Check if either the function name or "Result" was assigned to */
        int return_was_assigned = is_constructor ? 1 : (hash_return->mutated != NO_MUTATE);
        if (!return_was_assigned)
        {
            /* Also check if "Result" was mutated */
            HashNode_t *result_node = NULL;
            if (FindIdent(&result_node, symtab, "Result") == 0 && result_node != NULL)
            {
                return_was_assigned = (result_node->mutated != NO_MUTATE);
            }
        }

        /* Methods use mangled identifiers like Class__Func; allow assignments to the
         * unmangled function name (after the final '__') to satisfy the return check. */
        if (!return_was_assigned && subprogram->tree_data.subprogram_data.id != NULL) {
            const char *id = subprogram->tree_data.subprogram_data.id;
            const char *sep = strstr(id, "__");
            if (sep != NULL && sep[2] != '\0') {
                HashNode_t *suffix_node = NULL;
                if (FindIdent(&suffix_node, symtab, (sep + 2)) == 0 && suffix_node != NULL) {
                    return_was_assigned = (suffix_node->mutated != NO_MUTATE);
                }
            }
        }

        if(!return_was_assigned && !has_asm)
        {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: Checking return for %s. cname_override=%s cname_flag=%d return_was_assigned=%d\n",
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.cname_override ? subprogram->tree_data.subprogram_data.cname_override : "<null>",
                subprogram->tree_data.subprogram_data.cname_flag,
                return_was_assigned);
#endif

            /* Skip check for external functions (cname_flag or cname_override) or declarations without bodies */
            int is_external = (subprogram->tree_data.subprogram_data.cname_override != NULL) ||
                              (subprogram->tree_data.subprogram_data.cname_flag != 0);
            int has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
            if (is_external || !has_body)
            {
                /* External function, no body expected */
            }
            else
            {
                /* FPC treats this as a warning, not an error - function result may be uninitialized */
                fprintf(stderr,
                    "Warning in function %s: function result does not seem to be set\n\n",
                    subprogram->tree_data.subprogram_data.id);
                g_semcheck_warning_count++;
            }
        }
    }

    if(optimize_flag() > 0 && return_val == 0)
    {
        optimize(symtab, subprogram);
    }

    if (subprogram->tree_data.subprogram_data.id != NULL)
    {
        ListNode_t *defs = FindAllIdents(symtab, subprogram->tree_data.subprogram_data.id);
        ListNode_t *iter = defs;
        while (iter != NULL)
        {
            if (iter->cur != NULL)
            {
                HashNode_t *node = (HashNode_t *)iter->cur;
                if (node != NULL &&
                    node->mangled_id != NULL &&
                    subprogram->tree_data.subprogram_data.mangled_id != NULL &&
                    strcmp(node->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
                {
                    node->requires_static_link =
                        subprogram->tree_data.subprogram_data.requires_static_link ? 1 : 0;
                    node->has_nested_requiring_link =
                        subprogram->tree_data.subprogram_data.has_nested_requiring_link ? 1 : 0;
                }
            }
            iter = iter->next;
        }
        DestroyList(defs);
    }

    g_semcheck_current_subprogram = prev_current_subprogram;
    PopScope(symtab);

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s returning at end: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
    if (owner_copy != NULL)
    {
        semcheck_set_current_method_owner(prev_owner);
        free(owner_copy);
    }

    return return_val;
}


/* Pre-declare a subprogram (add to symbol table without processing body)
 * This is used for forward declarations so all procedures are visible
 * before any bodies are processed.
 */
int predeclare_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev, Tree_t *parent_subprogram)
{
    int return_val = 0;
    int func_return;
    enum TreeType sub_type;

    assert(symtab != NULL);
    assert(subprogram != NULL);
    assert(subprogram->type == TREE_SUBPROGRAM);

    char *id_to_use_for_lookup;

    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    /* Debug output for procedure predeclaration */
    if (getenv("KGPC_DEBUG_PREDECLARE_PROC") != NULL)
    {
        fprintf(stderr, "[PREDECLARE_PROC] %s (line %d) parent=%s\n",
                subprogram->tree_data.subprogram_data.id,
                subprogram->line_num,
                parent_subprogram != NULL ? parent_subprogram->tree_data.subprogram_data.id : "(null)");
    }

    // --- Name Mangling Logic ---
    const char *predeclare_name = subprogram->tree_data.subprogram_data.cname_override;
    if (predeclare_name != NULL) {
        subprogram->tree_data.subprogram_data.mangled_id = strdup(predeclare_name);
    } else if (subprogram->tree_data.subprogram_data.cname_flag) {
        subprogram->tree_data.subprogram_data.mangled_id = strdup(subprogram->tree_data.subprogram_data.id);
    } else {
        // Pass the symbol table to the mangler
        char *base_mangled = MangleFunctionName(
            subprogram->tree_data.subprogram_data.id,
            subprogram->tree_data.subprogram_data.args_var,
            symtab);

        // For nested functions, prepend the parent's mangled_id to make the name unique
        if (parent_subprogram != NULL && parent_subprogram->tree_data.subprogram_data.mangled_id != NULL) {
            const char *parent_mangled = parent_subprogram->tree_data.subprogram_data.mangled_id;
            size_t len = strlen(parent_mangled) + 2 + strlen(base_mangled) + 1; // parent$nested\0
            char *nested_mangled = malloc(len);
            if (nested_mangled != NULL) {
                snprintf(nested_mangled, len, "%s$%s", parent_mangled, base_mangled);
                free(base_mangled);
                subprogram->tree_data.subprogram_data.mangled_id = nested_mangled;
            } else {
                subprogram->tree_data.subprogram_data.mangled_id = base_mangled;
            }
        } else {
            subprogram->tree_data.subprogram_data.mangled_id = base_mangled;
        }
    }

    if (getenv("KGPC_DEBUG_PREDECLARE_PROC") != NULL)
    {
        fprintf(stderr, "[PREDECLARE_PROC]   -> mangled_id=%s\n",
                subprogram->tree_data.subprogram_data.mangled_id);
    }

    id_to_use_for_lookup = subprogram->tree_data.subprogram_data.id;

    /* Check if this specific overload is already declared (by matching mangled name) */
    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
        ListNode_t *cur = all_matches;
        int already_exists = 0;
        while (cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL && candidate->mangled_id != NULL &&
                strcmp(candidate->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
            {
                int same_signature = 1;
                if (candidate->type != NULL && candidate->type->kind == TYPE_KIND_PROCEDURE)
                {
                    Tree_t *candidate_def = candidate->type->info.proc_info.definition;
                    if (candidate_def != NULL &&
                        !semcheck_subprogram_signatures_equivalent(subprogram, candidate_def))
                    {
                        same_signature = 0;
                    }
                }
                if (same_signature)
                {
                    already_exists = 1;
                    break;
                }
            }
            /* If this exact subprogram was already registered (even with a different
             * mangled name), reuse that declaration instead of creating a duplicate. */
            if (candidate != NULL && candidate->type != NULL &&
                candidate->type->kind == TYPE_KIND_PROCEDURE &&
                candidate->type->info.proc_info.definition == subprogram)
            {
                if (candidate->mangled_id != NULL)
                    free(candidate->mangled_id);
                candidate->mangled_id = strdup(subprogram->tree_data.subprogram_data.mangled_id);
                already_exists = 1;
                break;
            }
            cur = cur->next;
        }
        if (all_matches != NULL)
            DestroyList(all_matches);

        if (already_exists)
            return 0;  /* Already declared - skip to avoid duplicates */
    }

    /* If a declaration already exists for this name/signature, skip predeclaring
     * the matching implementation body. */
    {
        int current_has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
        ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
        ListNode_t *cur = all_matches;
        while (cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate == NULL || candidate->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
            {
                cur = cur->next;
                continue;
            }
            if (candidate->type != NULL && candidate->type->kind == TYPE_KIND_PROCEDURE)
            {
                Tree_t *def = candidate->type->info.proc_info.definition;
                int existing_has_body = (def != NULL &&
                    def->tree_data.subprogram_data.statement_list != NULL);
                int same_signature = 0;

                if (def != NULL &&
                    semcheck_subprogram_signatures_equivalent(subprogram, def))
                {
                    same_signature = 1;
                }
                else if (candidate->mangled_id != NULL &&
                         subprogram->tree_data.subprogram_data.mangled_id != NULL &&
                         strcmp(candidate->mangled_id,
                                subprogram->tree_data.subprogram_data.mangled_id) == 0)
                {
                    same_signature = 1;
                }

                if (same_signature && existing_has_body != current_has_body)
                {
                    if (all_matches != NULL)
                        DestroyList(all_matches);
                    return 0;
                }
            }
            cur = cur->next;
        }
        if (all_matches != NULL)
            DestroyList(all_matches);
    }

    /**** PLACE SUBPROGRAM ON THE CURRENT SCOPE ****/
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        /* Create KgpcType for the procedure */
        KgpcType *proc_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            NULL  /* procedures have no return type */
        );
        if (proc_type != NULL) {
            proc_type->info.proc_info.definition = subprogram;
            if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                proc_type->info.proc_info.return_type_id =
                    strdup(subprogram->tree_data.subprogram_data.return_type_id);
        }

        // Add to current scope
        func_return = PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        proc_type);

        if(func_return > 0)
        {
            fprintf(stderr, "On line %d: redeclaration of name %s!\n",
                subprogram->line_num, subprogram->tree_data.subprogram_data.id);
            return_val += func_return;
        }
    }
    else // Function
    {
        KgpcType *return_kgpc_type = build_function_return_type(subprogram, symtab, &return_val, 1);
#ifdef DEBUG
        if (return_val > 0) fprintf(stderr, "DEBUG: predeclare_subprogram %s error after build_function_return_type: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

        /* Create function KgpcType */
        KgpcType *func_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            return_kgpc_type  /* functions have a return type */
        );
        if (func_type != NULL) {
            func_type->info.proc_info.definition = subprogram;
            if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                func_type->info.proc_info.return_type_id =
                    strdup(subprogram->tree_data.subprogram_data.return_type_id);
        }

        // Add to current scope
        func_return = PushFunctionOntoScope_Typed(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        func_type);

        if(func_return > 0)
        {
            fprintf(stderr, "On line %d: redeclaration of name %s!\n",
                subprogram->line_num, subprogram->tree_data.subprogram_data.id);
            return_val += func_return;
        }
    }

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: predeclare_subprogram %s returning error: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    return return_val;
}


/* Semantic check on multiple subprograms */
/* A return value greater than 0 indicates how many errors occurred */
/* Forward declaration - we'll define this after semcheck_subprogram */

/* Predeclare a list of subprograms without processing bodies.
 * Safe to call multiple times thanks to duplicate checks in predeclare_subprogram. */
int predeclare_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram)
{
    int return_val = 0;
    ListNode_t *cur = subprograms;
    while (cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        Tree_t *child = (Tree_t *)cur->cur;
        return_val += predeclare_subprogram(symtab, child, max_scope_lev, parent_subprogram);
        /* Note: We intentionally do NOT predeclare nested subprograms here globally.
         * Nested subprograms are predeclared within their parent's scope when
         * semcheck_subprogram calls semcheck_subprograms (which has its own Pass 1).
         * This ensures nested functions with the same name in different parents
         * don't shadow each other. */
        cur = cur->next;
    }
    return return_val;
}

int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram)
{
    ListNode_t *cur;
    int return_val;
    assert(symtab != NULL);

#ifdef DEBUG
    fprintf(stderr, "DEBUG: semcheck_subprograms called. subprograms=%p\n", subprograms);
#endif

    return_val = 0;

    /* ARCHITECTURAL FIX: Two-pass approach to ensure all procedure declarations
     * are visible before processing any bodies. This fixes the issue where unit
     * procedures were not visible in nested user procedures because they came
     * later in the subprograms list.
     *
     * Pass 1: Declare all procedures (add to symbol table)
     * Pass 2: Process bodies (which may reference procedures declared in pass 1)
     */

    /* Pass 1: Pre-declare all procedures at this level */
    cur = subprograms;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        return_val += predeclare_subprogram(symtab, (Tree_t *)cur->cur, max_scope_lev, parent_subprogram);
        cur = cur->next;
    }

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprograms error after Pass 1: %d\n", return_val);
#endif

    /* Pass 2: Process full semantic checking including bodies */
    cur = subprograms;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        Tree_t *child = (Tree_t *)cur->cur;
        if (child != NULL &&
            child->tree_data.subprogram_data.statement_list == NULL &&
            child->tree_data.subprogram_data.cname_flag == 0 &&
            child->tree_data.subprogram_data.cname_override == NULL)
        {
            /* Interface/forward declaration with no body. */
            cur = cur->next;
            continue;
        }
        return_val += semcheck_subprogram(symtab, child, max_scope_lev);
        /* If child needs a static link, mark parent as having nested children that need links.
         * This is used by codegen to know when to PASS a static link when calling nested functions.
         * We do NOT propagate requires_static_link to parent - the parent only needs to RECEIVE
         * a static link if it's nested itself or accesses outer scope variables. */
        if (parent_subprogram != NULL &&
            child != NULL &&
            child->tree_data.subprogram_data.requires_static_link)
        {
            parent_subprogram->tree_data.subprogram_data.has_nested_requiring_link = 1;
        }
        cur = cur->next;
    }

    return return_val;
}
