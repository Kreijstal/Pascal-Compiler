static void semcheck_update_symbol_alias(SymTab_t *symtab, const char *id, const char *alias)
{
    if (symtab == NULL || id == NULL || alias == NULL)
        return;

    HashNode_t *node = NULL;
    if (FindSymbol(&node, symtab, id) != 0 && node != NULL)
    {
        if (node->mangled_id != NULL)
            free(node->mangled_id);
        node->mangled_id = strdup(alias);
    }
}

/* Helper function to get TypeAlias from HashNode, preferring KgpcType when available */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    if (node == NULL)
        return NULL;
    
    /* Use hashnode helper which handles NULL KgpcType */
    return hashnode_get_type_alias(node);
}

static void apply_builtin_integer_alias_metadata(struct TypeAlias *alias, const char *type_name)
{
    if (alias == NULL || type_name == NULL)
        return;

    if (pascal_identifier_equals(type_name, "Byte"))
    {
        alias->base_type = BYTE_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = 0;
        alias->range_end = 255;
        alias->storage_size = 1;
    }
    else if (pascal_identifier_equals(type_name, "ShortInt"))
    {
        alias->base_type = INT_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = -128;
        alias->range_end = 127;
        alias->storage_size = 1;
    }
    else if (pascal_identifier_equals(type_name, "Word"))
    {
        alias->base_type = WORD_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = 0;
        alias->range_end = 65535;
        alias->storage_size = 2;
    }
    else if (pascal_identifier_equals(type_name, "SmallInt"))
    {
        alias->base_type = INT_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = -32768;
        alias->range_end = 32767;
        alias->storage_size = 2;
    }
    else if (pascal_identifier_equals(type_name, "LongInt") ||
             pascal_identifier_equals(type_name, "Integer"))
    {
        alias->base_type = LONGINT_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = -2147483648LL;
        alias->range_end = 2147483647LL;
        alias->storage_size = 4;
    }
    else if (pascal_identifier_equals(type_name, "Cardinal") ||
             pascal_identifier_equals(type_name, "LongWord") ||
             pascal_identifier_equals(type_name, "DWord"))
    {
        alias->base_type = LONGWORD_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = 0;
        alias->range_end = 4294967295LL;
        alias->storage_size = 4;
    }
    else if (pascal_identifier_equals(type_name, "QWord") ||
             pascal_identifier_equals(type_name, "UInt64") ||
             pascal_identifier_equals(type_name, "NativeUInt"))
    {
        alias->base_type = QWORD_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = 0;
        alias->range_end = LLONG_MAX;
        alias->storage_size = 8;
    }
    else if (pascal_identifier_equals(type_name, "NativeInt"))
    {
        alias->base_type = INT64_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = LLONG_MIN;
        alias->range_end = LLONG_MAX;
        alias->storage_size = 8;
    }
}

static void inherit_alias_metadata(SymTab_t *symtab, struct TypeAlias *alias)
{
    if (symtab == NULL || alias == NULL)
        return;

    HashNode_t *target_node = NULL;
    if (alias->target_type_ref != NULL && alias->target_type_ref->name != NULL &&
        alias->target_type_ref->name->count > 1)
    {
        target_node = semcheck_find_exact_type_node_for_qid(symtab,
            alias->target_type_ref->name);
    }
    if (target_node == NULL &&
        (alias->target_type_ref != NULL || alias->target_type_id != NULL))
    {
        target_node = semcheck_find_preferred_type_node_with_ref(symtab,
            alias->target_type_ref, alias->target_type_id);
    }
    if (target_node == NULL)
        return;

    struct TypeAlias *target_alias = get_type_alias_from_node(target_node);
    if (target_alias == NULL)
        return;

    if (alias->storage_size <= 0 && target_alias->storage_size > 0)
        alias->storage_size = target_alias->storage_size;

    if (!alias->is_range && target_alias->is_range && target_alias->range_known)
    {
        alias->is_range = 1;
        alias->range_known = target_alias->range_known;
        alias->range_start = target_alias->range_start;
        alias->range_end = target_alias->range_end;
    }

    if (alias->base_type == UNKNOWN_TYPE && target_alias->base_type != UNKNOWN_TYPE)
        alias->base_type = target_alias->base_type;

    if (!alias->is_enum && target_alias->is_enum)
        alias->is_enum = 1;
    if (target_alias->enum_is_scoped)
        alias->enum_is_scoped = 1;
    if (target_alias->enum_has_explicit_values)
        alias->enum_has_explicit_values = 1;
    if (alias->enum_literals == NULL && target_alias->enum_literals != NULL)
        alias->enum_literals = semcheck_clone_string_list(target_alias->enum_literals);
    if (alias->kgpc_type == NULL && target_alias->kgpc_type != NULL)
    {
        alias->kgpc_type = target_alias->kgpc_type;
        kgpc_type_retain(alias->kgpc_type);
    }
}

/* Helper function to get RecordType from HashNode */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    if (node == NULL) return NULL;
    
    /* Use hashnode helper which handles NULL KgpcType */
    struct RecordType *record = hashnode_get_record_type(node);
    if (record != NULL)
        return record;
        
    /* If not a direct record, check if it's a pointer to a record (Class types are pointers) */
    if (node->type != NULL && kgpc_type_is_pointer(node->type))
    {
        KgpcType *pointed_to = node->type->info.points_to;
        if (pointed_to != NULL && kgpc_type_is_record(pointed_to))
        {
            return kgpc_type_get_record(pointed_to);
        }
    }
    
    return NULL;
}

/**
 * Copy default parameter values from forward declaration to implementation.
 * When a method is declared in a class with default values but implemented
 * without them, the implementation's params need the defaults for overload resolution.
 */
static void copy_default_values_to_impl_params(ListNode_t *fwd_params, ListNode_t *impl_params)
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

/* Check if 'name' matches any parameter identifier in args_var list.
 * Used to prevent class field/method injection from shadowing parameters. */
static int name_matches_parameter(const char *name, ListNode_t *args_var)
{
    if (name == NULL || args_var == NULL) return 0;
    for (ListNode_t *p = args_var; p != NULL; p = p->next)
    {
        if (p->type != LIST_TREE || p->cur == NULL) continue;
        Tree_t *param_tree = (Tree_t *)p->cur;
        ListNode_t *ids = NULL;
        if (param_tree->type == TREE_VAR_DECL)
            ids = param_tree->tree_data.var_decl_data.ids;
        else if (param_tree->type == TREE_ARR_DECL)
            ids = param_tree->tree_data.arr_decl_data.ids;
        for (ListNode_t *id_node = ids; id_node != NULL; id_node = id_node->next)
        {
            if (id_node->cur != NULL && pascal_identifier_equals(name, (const char *)id_node->cur))
                return 1;
        }
    }
    return 0;
}

/**
 * For a method implementation (ClassName__MethodName), add class vars to scope
 * so they can be referenced within the method body. This is essential for
 * static methods which have no implicit Self parameter.
 */
static void add_class_vars_to_method_scope_impl(SymTab_t *symtab,
    const char *owner, const char *method_name, int is_operator,
    ListNode_t *subprogram_args)
{
    if (symtab == NULL || owner == NULL || method_name == NULL || method_name[0] == '\0')
        return;

    if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL)
        fprintf(stderr, "[KGPC_DEBUG_CLASS_VAR] owner=%s method=%s\n", owner, method_name);

    char *class_name = strdup(owner);
    if (class_name == NULL)
        return;

    if (from_cparser_is_type_helper(class_name))
    {
        free(class_name);
        return;
    }

    /* Operator methods (class operator ...), especially on records, should
     * not have owner fields injected into local scope. Their parameters often
     * intentionally reuse field names (e.g. "a"), and injected fields would
     * shadow those parameters. */
    if (is_operator)
    {
        free(class_name);
        return;
    }

    /* Only add class fields for static methods. Non-static methods
     * access fields via Self which is handled differently. */
    int is_static = from_cparser_is_method_static(class_name, method_name);

    /* Look up the class type */
    HashNode_t *class_node = NULL;
    if (!FindSymbol(&class_node, symtab, class_name) || class_node == NULL)
    {
        free(class_name);
        return;
    }

    struct RecordType *record_info = get_record_type_from_node(class_node);
    if (record_info == NULL)
    {
        free(class_name);
        return;
    }
    if (record_info->generic_decl != NULL || record_info->num_generic_args > 0)
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

    int has_class_vars = 0;
    if (record_info->fields != NULL)
    {
        for (ListNode_t *scan = record_info->fields; scan != NULL; scan = scan->next)
        {
            if (scan->type == LIST_RECORD_FIELD && scan->cur != NULL)
            {
                struct RecordField *field = (struct RecordField *)scan->cur;
                if (field->is_class_var == 1)
                {
                    has_class_vars = 1;
                    break;
                }
            }
        }
    }

    /* For class types (not objects), non-static methods access fields via Self.
     * For object types, class vars and consts are NOT accessible via Self,
     * so we must always push them into scope. */
    int is_object_type = !record_type_is_class(record_info) && !record_info->is_type_helper;
    if (!is_static && !is_object_type)
    {
        free(class_name);
        return;
    }
    if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL)
    {
        fprintf(stderr, "[KGPC_DEBUG_CLASS_VAR] class=%s static=%d has_class_vars=%d\n",
            class_name ? class_name : "<null>", is_static, has_class_vars);
    }

    /* Process class types, object types, and advanced records that have fields. */
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
            int allow_all_fields = (record_type_is_class(record_info) && is_static && !has_class_vars);
            if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL)
            {
                fprintf(stderr, "[KGPC_DEBUG_CLASS_VAR] field=%s class_var=%d allow_all=%d\n",
                    field->name ? field->name : "<null>",
                    field->is_class_var, allow_all_fields);
            }
            if (record_type_is_class(record_info) && !allow_all_fields && field->is_class_var != 1)
            {
                field_node = field_node->next;
                continue;
            }
            if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL &&
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
            /* Check if already defined in current local scope only.
             * Using FindIdent here would search unit tables too, causing
             * unit-level symbols (e.g. a var 'x' in system.pp) to prevent
             * pushing record fields like TPointF.x into the method scope.
             * Also skip fields that match parameter names — parameters must
             * shadow class fields (e.g. 'dst' parameter vs field 'dst'). */
            if (name_matches_parameter(field->name, subprogram_args))
            {
                field_node = field_node->next;
                continue;
            }
            HashNode_t *existing = NULL;
            if (symtab->current_scope != NULL && symtab->current_scope != symtab->builtin_scope)
                existing = FindIdentInTable(symtab->current_scope->table, field->name);
            if (existing == NULL)
            {
                /* Build a KgpcType for this field if needed */
                KgpcType *field_type = NULL;
                if (field->type_id != NULL && field->type_id[0] != '\0')
                {
                    HashNode_t *type_node = NULL;
                    if (FindSymbol(&type_node, symtab, field->type_id) != 0 &&
                        type_node != NULL && type_node->type != NULL)
                    {
                        field_type = type_node->type;
                        kgpc_type_retain(field_type);
                    }
                    else if (class_name != NULL)
                    {
                        char qualified_name[512];
                        snprintf(qualified_name, sizeof(qualified_name), "%s.%s", class_name, field->type_id);
                        if (FindSymbol(&type_node, symtab, qualified_name) != 0 &&
                            type_node != NULL && type_node->type != NULL)
                        {
                            field_type = type_node->type;
                            kgpc_type_retain(field_type);
                            free(field->type_id);
                            field->type_id = strdup(qualified_name);
                        }
                    }
                    /* When the field is declared with a ^ prefix (e.g. RefPtr: ^PMyRec),
                     * field->type_id resolves to the pointed-to type (PMyRec).  We need
                     * to wrap it in an additional pointer level to get the correct type
                     * for the field itself (pointer→PMyRec = pointer→pointer→TMyRec). */
                    if (field->is_pointer && field_type != NULL)
                    {
                        KgpcType *ptr_wrapped = create_pointer_type(field_type);
                        kgpc_type_release(field_type);
                        field_type = ptr_wrapped;
                    }
                }
                else if (field->is_array)
                {
                    KgpcType *elem_type = NULL;
                    if (field->array_element_kgpc_type != NULL)
                    {
                        /* Pre-built element type for nested arrays (array of array of ...) */
                        elem_type = field->array_element_kgpc_type;
                        kgpc_type_retain(elem_type);
                    }
                    else if (field->array_element_type_id != NULL)
                    {
                        HashNode_t *elem_node = NULL;
                        if (FindSymbol(&elem_node, symtab, field->array_element_type_id) != 0 &&
                            elem_node != NULL && elem_node->type != NULL)
                        {
                            elem_type = elem_node->type;
                            kgpc_type_retain(elem_type);
                        }
                        else if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL &&
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
                            struct RecordType *elem_record = get_record_type_from_node(elem_node);
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
                        if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL &&
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
                if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL &&
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

    /* Class properties are NOT pushed to scope here; they are resolved
     * dynamically in semcheck_varid() by rewriting the identifier to
     * the backing field (read accessor) at resolution time. This avoids
     * scope level issues in codegen. */

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
            /* Skip methods whose short name matches a parameter name —
             * parameters must shadow class methods (e.g. parameter 'angle'
             * must shadow method 'Angle' inside TVec.Rotate). */
            if (name_matches_parameter(binding->method_name, subprogram_args))
            {
                cur_method = cur_method->next;
                continue;
            }
            char *mangled_name = make_method_lookup_key(class_name, binding->method_name);
            if (mangled_name != NULL)
            {
                /* Look up the mangled method in the symbol table */
                HashNode_t *method_node = NULL;
                if (FindSymbol(&method_node, symtab, mangled_name) != 0 && method_node != NULL)
                {
                    /* Add an alias using just the method name */
                    HashNode_t *existing = NULL;
                    if (FindSymbol(&existing, symtab, binding->method_name) == 0)
                    {
                        /* Push the method onto scope using its short name,
                         * with the actual fully-mangled assembly label (not
                         * the intermediate ClassName__MethodName form). */
                        if (method_node->type != NULL)
                        {
                            const char *real_mangled = method_node->mangled_id ? method_node->mangled_id : mangled_name;
                            PushFunctionOntoScope_Typed(symtab, binding->method_name,
                                                        (char *)real_mangled, method_node->type);
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

static void add_class_vars_to_method_scope_for(SymTab_t *symtab, Tree_t *subprogram, int use_outer_class)
{
    if (subprogram == NULL)
        return;
    const char *owner = use_outer_class
        ? subprogram->tree_data.subprogram_data.owner_class_outer
        : subprogram->tree_data.subprogram_data.owner_class;
    add_class_vars_to_method_scope_impl(symtab,
        owner,
        subprogram->tree_data.subprogram_data.method_name,
        subprogram->tree_data.subprogram_data.is_operator,
        subprogram->tree_data.subprogram_data.args_var);
}

static void add_class_vars_to_method_scope(SymTab_t *symtab, Tree_t *subprogram)
{
    add_class_vars_to_method_scope_for(symtab, subprogram, 0);
}

static void add_outer_class_vars_to_method_scope(SymTab_t *symtab, Tree_t *subprogram)
{
    add_class_vars_to_method_scope_for(symtab, subprogram, 1);
}

/**
 * For a method implementation (ClassName__MethodName), copy default parameter
 * values from the class declaration to the implementation's parameters.
 * This is needed because default values are specified in the class declaration
 * but not repeated in the implementation.
 */
static void copy_method_decl_defaults_to_impl(SymTab_t *symtab, Tree_t *subprogram)
{
    if (symtab == NULL || subprogram == NULL)
        return;
    
    /* Check if this is a method */
    const char *owner = subprogram->tree_data.subprogram_data.owner_class;
    const char *method_name = subprogram->tree_data.subprogram_data.method_name;
    if (owner == NULL || method_name == NULL || method_name[0] == '\0')
        return;

    char *class_name = strdup(owner);
    if (class_name == NULL)
        return;
    
    /* Look up the class type */
    HashNode_t *class_node = NULL;
    if (FindSymbol(&class_node, symtab, class_name) == 0 || class_node == NULL)
    {
        free(class_name);
        return;
    }
    
    struct RecordType *record_info = get_record_type_from_node(class_node);
    if (record_info == NULL)
    {
        free(class_name);
        return;
    }
    
    /* Find the method template with the declaration's parameters */
    struct MethodTemplate *template = from_cparser_get_method_template(record_info, method_name);
    if (template == NULL || template->params_ast == NULL)
    {
        if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
            fprintf(stderr, "[copy_method_decl_defaults] No method template found for %s.%s\n",
                class_name, method_name);
        free(class_name);
        return;
    }
    
    if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
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

    /* For overloaded methods, verify that the template's parameter count matches
     * the implementation's parameter count.  from_cparser_get_method_template
     * matches by name only, so for overloaded methods it may return the wrong
     * template.  Copying defaults from a mismatched template corrupts the
     * parameter list and breaks overload resolution. */
    {
        int decl_count = 0;
        for (ast_t *p = param_cursor; p != NULL; p = p->next)
            if (p->typ == PASCAL_T_PARAM)
                decl_count++;
        int impl_count = 0;
        for (ListNode_t *p = impl_param; p != NULL; p = p->next)
            impl_count++;
        if (decl_count != impl_count)
        {
            free(class_name);
            return;
        }
    }

    while (param_cursor != NULL && impl_param != NULL)
    {
        ast_t *decl_param = param_cursor;
        if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
            fprintf(stderr, "[copy_method_decl_defaults] Processing decl_param typ=%d\n", decl_param->typ);
        
        if (decl_param->typ == PASCAL_T_PARAM)
        {
            /* Find the TYPE_SPEC and DEFAULT_VALUE in the declaration param */
            ast_t *type_spec = NULL;
            ast_t *default_value = NULL;
            
            for (ast_t *child = decl_param->child; child != NULL; child = child->next)
            {
                if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
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
            
            if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                fprintf(stderr, "[copy_method_decl_defaults] type_spec=%p default_value=%p\n",
                    (void*)type_spec, (void*)default_value);
            
            if (default_value != NULL)
            {
                /* Get the implementation parameter */
                Tree_t *impl_decl = (Tree_t *)impl_param->cur;
                if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                    fprintf(stderr, "[copy_method_decl_defaults] impl_decl=%p type=%d\n",
                        (void*)impl_decl, impl_decl ? impl_decl->type : -1);
                
                if (impl_decl != NULL && impl_decl->type == TREE_VAR_DECL)
                {
                    /* Only copy if impl doesn't already have a default */
                    if (impl_decl->tree_data.var_decl_data.initializer == NULL)
                    {
                        /* Convert default_value->child to Expression and wrap in initializer */
                        ast_t *expr_node = default_value->child;
                        if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                            fprintf(stderr, "[copy_method_decl_defaults] expr_node=%p typ=%d\n",
                                (void*)expr_node, expr_node ? expr_node->typ : -1);
                        
                        if (expr_node != NULL)
                        {
                            struct Expression *default_expr = from_cparser_convert_expression(expr_node);
                            if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                                fprintf(stderr, "[copy_method_decl_defaults] default_expr=%p\n",
                                    (void*)default_expr);

                            if (default_expr != NULL)
                            {
                                impl_decl->tree_data.var_decl_data.initializer =
                                    mk_varassign(default_value->line, 0, NULL, default_expr);
                                if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                                    fprintf(stderr, "[copy_method_decl_defaults] COPIED default value!\n");
                            }
                        }
                    }
                    else if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
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
                            
/* Copy method identity fields from a subprogram tree node into a hash node,
 * if the hash node doesn't already have them.  We strdup so the hash node
 * owns the memory and DestroyHashTable can free it uniformly. */
static void copy_method_identity_to_node(HashNode_t *node, Tree_t *subprogram)
{
    if (node == NULL || subprogram == NULL)
        return;
    if (subprogram->tree_data.subprogram_data.is_operator)
        node->is_operator = 1;
    if (subprogram->tree_data.subprogram_data.is_nested_scope)
        node->is_nested_scope = 1;
    if (subprogram->tree_data.subprogram_data.method_name == NULL)
        return;
    if (node->method_name == NULL)
        node->method_name = strdup(subprogram->tree_data.subprogram_data.method_name);
    if (node->owner_class == NULL && subprogram->tree_data.subprogram_data.owner_class != NULL)
        node->owner_class = strdup(subprogram->tree_data.subprogram_data.owner_class);
    if (node->owner_class_full == NULL && subprogram->tree_data.subprogram_data.owner_class_full != NULL)
        node->owner_class_full = strdup(subprogram->tree_data.subprogram_data.owner_class_full);
    if (node->owner_class_outer == NULL && subprogram->tree_data.subprogram_data.owner_class_outer != NULL)
        node->owner_class_outer = strdup(subprogram->tree_data.subprogram_data.owner_class_outer);
}

static void semcheck_propagate_method_identity(SymTab_t *symtab, Tree_t *subprogram)
{
    if (symtab == NULL || subprogram == NULL)
        return;

    const char *id = subprogram->tree_data.subprogram_data.id;
    const char *mangled = subprogram->tree_data.subprogram_data.mangled_id;
    if (id == NULL || mangled == NULL)
        return;

    ListNode_t *candidates = FindAllIdents(symtab, id);
    if (candidates == NULL)
        return;

    ListNode_t *cur = candidates;
    while (cur != NULL)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate != NULL && candidate->mangled_id != NULL &&
            strcmp(candidate->mangled_id, mangled) == 0)
        {
            if (candidate->type != NULL && candidate->type->kind == TYPE_KIND_PROCEDURE &&
                candidate->type->info.proc_info.definition == NULL)
                candidate->type->info.proc_info.definition = subprogram;

            copy_method_identity_to_node(candidate, subprogram);
        }
        cur = cur->next;
    }

    DestroyList(candidates);
}

static HashNode_t *semcheck_find_preferred_type_node_ref_internal(SymTab_t *symtab,
    const TypeRef *type_ref, const char *type_id, int skip_owner_qualify,
    int override_unit_index)
{
    if (symtab == NULL)
        return NULL;

    char *rendered = NULL;
    const char *lookup_id = type_id;
    if (type_ref != NULL)
    {
        rendered = type_ref_render_mangled(type_ref);
        if (rendered != NULL)
            lookup_id = rendered;
    }
    if (lookup_id == NULL)
    {
        free(rendered);
        return NULL;
    }

    int prefer_unit_defined = semcheck_is_explicit_unit_qualified_type_ref(type_ref);
    int qualified_unit_index = 0;
    char *qualified_unit_name = NULL;
    if (type_ref != NULL && type_ref->name != NULL && type_ref->name->count > 1)
    {
        size_t qlen = 0;
        for (int i = 0; i < type_ref->name->count - 1; ++i)
        {
            const char *seg = type_ref->name->segments[i];
            if (seg != NULL)
                qlen += strlen(seg);
            if (i + 1 < type_ref->name->count - 1)
                qlen += 1;
        }
        if (qlen > 0)
        {
            qualified_unit_name = (char *)malloc(qlen + 1);
            if (qualified_unit_name != NULL)
            {
                qualified_unit_name[0] = '\0';
                for (int i = 0; i < type_ref->name->count - 1; ++i)
                {
                    const char *seg = type_ref->name->segments[i];
                    if (seg != NULL)
                        strcat(qualified_unit_name, seg);
                    if (i + 1 < type_ref->name->count - 1)
                        strcat(qualified_unit_name, ".");
                }
                if (unit_registry_contains(qualified_unit_name))
                    qualified_unit_index = unit_registry_add(qualified_unit_name);
            }
        }
    }
    ListNode_t *matches = FindAllIdents(symtab, lookup_id);
    if (matches == NULL && type_ref != NULL && type_ref->name != NULL &&
        type_ref->name->count > 1)
    {
        const char *base_name = type_ref_base_name(type_ref);
        if (base_name != NULL)
            matches = FindAllIdents(symtab, base_name);
    }

    if (matches == NULL && type_ref != NULL && type_ref->num_generic_args > 0)
    {
        const char *base_name = type_ref_base_name(type_ref);
        int arg_count = type_ref->num_generic_args;
        char **arg_types = NULL;
        if (base_name != NULL && arg_count > 0)
        {
            arg_types = (char **)calloc((size_t)arg_count, sizeof(char *));
            if (arg_types != NULL)
            {
                int ok = 1;
                for (int i = 0; i < arg_count; ++i)
                {
                    arg_types[i] = type_ref_render_mangled(type_ref->generic_args[i]);
                    if (arg_types[i] == NULL)
                        ok = 0;
                }
                if (!ok)
                {
                    for (int i = 0; i < arg_count; ++i)
                        free(arg_types[i]);
                    free(arg_types);
                    arg_types = NULL;
                }
            }
        }

        if (base_name != NULL && arg_types != NULL && arg_count > 0)
        {
            GenericTypeDecl *generic = generic_registry_find_decl(base_name);
            if (generic != NULL && generic->record_template != NULL &&
                generic->num_type_params == arg_count)
            {
                struct RecordType *record = clone_record_type(generic->record_template);
                if (record != NULL)
                {
                    if (record->type_id != NULL)
                        free(record->type_id);
                    record->type_id = strdup(lookup_id);
                    record->generic_decl = generic;
                    record->num_generic_args = arg_count;
                    record->generic_args = arg_types;
                    record->is_generic_specialization = 1;
                    arg_types = NULL;

                    /* Substitute generic parameters in record fields/properties. */
                    ListNode_t *field_node = record->fields;
                    while (field_node != NULL)
                    {
                        if (field_node->type == LIST_RECORD_FIELD)
                        {
                            struct RecordField *field = (struct RecordField *)field_node->cur;
                            if (field != NULL)
                            {
                                for (int i = 0; i < generic->num_type_params; ++i)
                                {
                                    const char *param = generic->type_parameters[i];
                                    const char *arg = record->generic_args[i];
                                    if (param != NULL && arg != NULL)
                                    {
                                        if (field->type_id != NULL &&
                                            strcasecmp(field->type_id, param) == 0)
                                        {
                                            free(field->type_id);
                                            field->type_id = strdup(arg);
                                        }
                                        if (field->array_element_type_id != NULL &&
                                            strcasecmp(field->array_element_type_id, param) == 0)
                                        {
                                            free(field->array_element_type_id);
                                            field->array_element_type_id = strdup(arg);
                                        }
                                    }
                                }
                            }
                        }
                        field_node = field_node->next;
                    }
                    ListNode_t *prop_node = record->properties;
                    while (prop_node != NULL)
                    {
                        if (prop_node->type == LIST_CLASS_PROPERTY)
                        {
                            struct ClassProperty *property = (struct ClassProperty *)prop_node->cur;
                            if (property != NULL && property->type_id != NULL)
                            {
                                for (int i = 0; i < generic->num_type_params; ++i)
                                {
                                    const char *param = generic->type_parameters[i];
                                    const char *arg = record->generic_args[i];
                                    if (param != NULL && arg != NULL &&
                                        strcasecmp(property->type_id, param) == 0)
                                    {
                                        free(property->type_id);
                                        property->type_id = strdup(arg);
                                        break;
                                    }
                                }
                            }
                        }
                        prop_node = prop_node->next;
                    }

                    generic_registry_add_specialization(base_name, record->generic_args, arg_count);

                    KgpcType *kgpc_type = create_record_type(record);
                    if (record_type_is_class(record))
                    {
                        KgpcType *ptr = create_pointer_type(kgpc_type);
                        kgpc_type_release(kgpc_type);
                        kgpc_type = ptr;
                    }
                    if (kgpc_type != NULL)
                    {
                        PushTypeOntoScope_Typed(symtab, (char *)lookup_id, kgpc_type);
                        /* Release creator's reference */
                        destroy_kgpc_type(kgpc_type);
                    }
                }
            }
        }

        if (arg_types != NULL)
        {
            for (int i = 0; i < arg_count; ++i)
                free(arg_types[i]);
            free(arg_types);
        }

        matches = FindAllIdents(symtab, lookup_id);
    }

    if (matches == NULL && type_ref != NULL)
    {
        const char *base = type_ref_base_name(type_ref);
        if (base != NULL && base != lookup_id)
            matches = FindAllIdents(symtab, base);
    }

    if (!skip_owner_qualify && matches == NULL)
    {
        int allow_owner_fallback = 0;
        if (type_ref == NULL)
        {
            allow_owner_fallback = 1;
        }
        else if (type_ref->name != NULL &&
                 type_ref->name->count == 1 &&
                 !semcheck_is_explicit_unit_qualified_type_ref(type_ref))
        {
            allow_owner_fallback = 1;
        }

        if (allow_owner_fallback)
        {
            const char *owner_id = semcheck_get_current_method_owner();
            if (owner_id != NULL)
            {
                char qualified_name[512];
                snprintf(qualified_name, sizeof(qualified_name), "%s.%s", owner_id, lookup_id);
                HashNode_t *qualified = semcheck_find_preferred_type_node_ref_internal(
                    symtab, NULL, qualified_name, 1, override_unit_index);
                if (qualified != NULL)
                {
                    free(qualified_unit_name);
                    if (rendered != NULL)
                        free(rendered);
                    return qualified;
                }
            }
        }
    }

    HashNode_t *best = NULL;
    int best_unit_rank = INT_MAX / 2;
    int best_scope_level = INT_MAX / 2;
    int best_same_unit = 0;
    int best_is_forward_stub = 1;
    int debug_tsize = (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL && rendered != NULL &&
                       pascal_identifier_equals(rendered, "TSize"));
    int debug_textended = (kgpc_getenv("KGPC_DEBUG_T80") != NULL && lookup_id != NULL &&
                           pascal_identifier_equals(lookup_id, "TExtended80Rec"));
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            if (qualified_unit_index > 0 &&
                node->source_unit_index > 0 &&
                node->source_unit_index != qualified_unit_index)
            {
                /* Accept if the type's unit or the qualifier's unit is
                 * a dependency of the other (covers transitive merging). */
                if (!unit_registry_is_dep(qualified_unit_index, node->source_unit_index) &&
                    !unit_registry_is_dep(node->source_unit_index, qualified_unit_index))
                {
                    cur = cur->next;
                    continue;
                }
            }
            if (node->source_unit_index != 0)
            {
                const char *unit_name = unit_registry_get(node->source_unit_index);
                int allowed = (unit_name == NULL) ? 1 : semcheck_is_unit_name(unit_name);
                if ((kgpc_getenv("KGPC_DEBUG_MISSING_TYPE") != NULL && lookup_id != NULL) ||
                    debug_textended)
                {
                    if (pascal_identifier_equals(lookup_id, "TFloatFormatProfile") ||
                        pascal_identifier_equals(lookup_id, "TFloatSpecial") ||
                        pascal_identifier_equals(lookup_id, "TDIY_FP_Power_of_10") ||
                        pascal_identifier_equals(lookup_id, "pshortstring") ||
                        pascal_identifier_equals(lookup_id, "TExtended80Rec") ||
                        pascal_identifier_equals(lookup_id, "T"))
                    {
                        fprintf(stderr,
                            "[MISSING_TYPE] candidate id=%s src_unit=%d(%s) allowed=%d\n",
                            node->id ? node->id : "<null>",
                            node->source_unit_index,
                            unit_name ? unit_name : "<null>",
                            allowed);
                    }
                }
                if (unit_name != NULL && !allowed)
                {
                    cur = cur->next;
                    continue;
                }
            }
            int scope_level = semcheck_scope_level_for_type_candidate(symtab, node);
            int is_forward_stub = semcheck_type_candidate_is_forward_stub(node);

            int same_unit = 0;
            int effective_unit_index = override_unit_index;
            /* Prefer the active semantic unit context when available. The current
             * scope's unit_index tracks which unit we're analysing. */
            if (effective_unit_index == 0 && symtab->current_scope != NULL &&
                symtab->current_scope->unit_index > 0)
                effective_unit_index = symtab->current_scope->unit_index;
            if (effective_unit_index == 0)
                effective_unit_index = g_semcheck_current_unit_index;
            int unit_rank = 0;
            if (prefer_unit_defined || effective_unit_index > 0)
                /* In unit context, prefer unit-defined types over local program types */
                unit_rank = node->defined_in_unit ? 0 : 1;
            else
                unit_rank = node->defined_in_unit ? 1 : 0;
            if (!prefer_unit_defined && effective_unit_index != 0 &&
                node->source_unit_index != 0 &&
                node->source_unit_index == effective_unit_index)
                same_unit = 1;

            if (debug_tsize || debug_textended || (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL &&
                lookup_id != NULL && pascal_identifier_equals(lookup_id, "TSize")))
            {
                const char *uname = unit_registry_get(node->source_unit_index);
                fprintf(stderr, "[TYPELOOKUP] candidate lookup='%s' id='%s' defined_in_unit=%d source_unit_idx=%d(%s) "
                    "unit_rank=%d scope_level=%d same_unit=%d forward_stub=%d effective_unit=%d scope_unit=%d type=%p kind=%d\n",
                    lookup_id ? lookup_id : "<null>",
                    node->id ? node->id : "<null>", node->defined_in_unit,
                    node->source_unit_index, uname ? uname : "?",
                    unit_rank, scope_level, same_unit, is_forward_stub, effective_unit_index,
                    symtab->current_scope != NULL ? symtab->current_scope->unit_index : -1,
                    (void *)node->type, node->type ? node->type->kind : -1);
            }

            int take = 0;
            if (best == NULL)
            {
                take = 1;
            }
            else if (prefer_unit_defined)
            {
                if (unit_rank < best_unit_rank)
                    take = 1;
                else if (unit_rank == best_unit_rank && scope_level < best_scope_level)
                    take = 1;
            }
            else if (same_unit > best_same_unit)
            {
                take = 1;
            }
            else if (same_unit == best_same_unit &&
                     ((is_forward_stub < best_is_forward_stub) ||
                      (is_forward_stub == best_is_forward_stub &&
                       (scope_level < best_scope_level ||
                        (scope_level == best_scope_level && unit_rank < best_unit_rank) ||
                        (scope_level == best_scope_level && unit_rank == best_unit_rank &&
                         node->source_unit_index > 0 && best->source_unit_index > 0 &&
                         node->source_unit_index > best->source_unit_index)))))
            {
                take = 1;
            }

            if (debug_tsize || debug_textended)
                fprintf(stderr, "[TYPELOOKUP]   take=%d\n", take);

            if (take)
            {
                best = node;
                best_unit_rank = unit_rank;
                best_scope_level = scope_level;
                best_same_unit = same_unit;
                best_is_forward_stub = is_forward_stub;
            }
        }
        cur = cur->next;
    }
    if ((debug_tsize || debug_textended) && best != NULL)
    {
        fprintf(stderr, "[TYPELOOKUP] WINNER lookup='%s' id='%s' defined_in_unit=%d source_unit_idx=%d kind=%d",
            lookup_id ? lookup_id : "<null>",
            best->id ? best->id : "<null>", best->defined_in_unit,
            best->source_unit_index, best->type ? best->type->kind : -1);
        if (best->type != NULL && best->type->kind == TYPE_KIND_RECORD)
        {
            struct RecordType *ri = best->type->info.record_info;
            fprintf(stderr, " record_info=%p", (void *)ri);
            if (ri != NULL)
            {
                int fc = 0;
                ListNode_t *f = ri->fields;
                while (f != NULL) { fc++; f = f->next; }
                fprintf(stderr, " field_count=%d type_id='%s'", fc,
                    ri->type_id ? ri->type_id : "<null>");
            }
        }
        else if (best->type != NULL && best->type->kind == TYPE_KIND_PRIMITIVE)
        {
            fprintf(stderr, " prim_tag=%d", best->type->info.primitive_type_tag);
        }
        fprintf(stderr, "\n");
    }
    if (matches != NULL)
        DestroyList(matches);
    free(qualified_unit_name);
    if (rendered != NULL)
        free(rendered);
    return best;
}

HashNode_t *semcheck_find_preferred_type_node(SymTab_t *symtab, const char *type_id)
{
    return semcheck_find_preferred_type_node_ref_internal(symtab, NULL, type_id, 0, 0);
}

HashNode_t *semcheck_find_preferred_type_node_for_unit(SymTab_t *symtab,
    const char *type_id, int unit_index)
{
    return semcheck_find_preferred_type_node_ref_internal(symtab, NULL, type_id, 0, unit_index);
}

HashNode_t *semcheck_find_preferred_type_node_with_ref(SymTab_t *symtab,
    const TypeRef *type_ref, const char *type_id)
{
    return semcheck_find_preferred_type_node_ref_internal(symtab, type_ref, type_id, 0, 0);
}

static HashNode_t *semcheck_find_type_node_with_unit_flag(SymTab_t *symtab,
    const char *type_id, int defined_in_unit)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, type_id);
    HashNode_t *best = NULL;
    HashNode_t *fallback_outermost = NULL;
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            if (node->defined_in_unit == defined_in_unit)
            {
                best = node;
                break;
            }
            if (best == NULL)
                best = node;
            fallback_outermost = node;
        }
        cur = cur->next;
    }
    if (best != NULL && best->defined_in_unit != defined_in_unit && defined_in_unit)
        best = fallback_outermost;
    if (matches != NULL)
        DestroyList(matches);
    return best;
}

static HashNode_t *semcheck_find_type_node_with_unit_flag_ref(SymTab_t *symtab,
    const TypeRef *type_ref, const char *type_id, int defined_in_unit)
{
    if (symtab == NULL)
        return NULL;

    char *rendered = NULL;
    const char *lookup_id = type_id;
    if (type_ref != NULL)
    {
        rendered = type_ref_render_mangled(type_ref);
        if (rendered != NULL)
            lookup_id = rendered;
    }
    if (lookup_id == NULL)
    {
        free(rendered);
        return NULL;
    }

    /* Try the rendered/mangled name first */
    HashNode_t *result = semcheck_find_type_node_with_unit_flag(symtab, lookup_id, defined_in_unit);

    /* Fall back to the bare base name when the mangled name yields nothing */
    if (result == NULL && type_ref != NULL)
    {
        const char *base = type_ref_base_name(type_ref);
        if (base != NULL && base != lookup_id)
            result = semcheck_find_type_node_with_unit_flag(symtab, base, defined_in_unit);
    }

    free(rendered);
    return result;
}

/* Helper function to get VarType from HashNode */
static inline enum VarType get_var_type_from_node(HashNode_t *node)
{
    if (node == NULL || node->type == NULL)
        return HASHVAR_UNTYPED;

    switch (node->type->kind)
    {
        case TYPE_KIND_PRIMITIVE:
        {
            int tag = kgpc_type_get_primitive_tag(node->type);
            return map_type_tag_to_var_type(tag);
        }
        case TYPE_KIND_POINTER:
            return HASHVAR_POINTER;
        case TYPE_KIND_ARRAY:
            return HASHVAR_ARRAY;
        case TYPE_KIND_RECORD:
            return HASHVAR_RECORD;
        case TYPE_KIND_PROCEDURE:
            return HASHVAR_PROCEDURE;
        default:
            return HASHVAR_UNTYPED;
    }
}

static inline void mark_hashnode_source_unit(HashNode_t *node, int unit_index) {
    if (node == NULL || unit_index <= 0) return;
    node->source_unit_index = unit_index;
}

static inline void mark_hashnode_unit_info(SymTab_t *symtab, HashNode_t *node,
    int defined_in_unit, int is_public)
{
    if (node == NULL || !defined_in_unit)
        return;
    node->defined_in_unit = 1;
    node->unit_is_public = is_public ? 1 : 0;
    mark_hashnode_source_unit(node, g_semcheck_current_unit_index);
    if (symtab != NULL)
        SymTab_MoveHashNodeToBack(symtab, node);

    const char *cur_unit_str = unit_registry_get(g_semcheck_current_unit_index);
    if (symtab == NULL || node->hash_type != HASHTYPE_TYPE ||
        cur_unit_str == NULL || node->id == NULL)
        return;

    size_t qualified_len = strlen(cur_unit_str) + 1 + strlen(node->id) + 1;
    char *qualified_id = (char *)malloc(qualified_len);
    if (qualified_id == NULL)
        return;
    snprintf(qualified_id, qualified_len, "%s.%s", cur_unit_str, node->id);

    HashNode_t *existing = NULL;
    if (FindSymbol(&existing, symtab, qualified_id) == 0 || existing == NULL)
    {
        if (symtab->current_scope != NULL && symtab->current_scope != symtab->builtin_scope)
        {
            KgpcType *qualified_type = NULL;
            if (node->type != NULL)
            {
                qualified_type = kgpc_type_clone_shallow_owned(node->type);
                if (qualified_type == NULL)
                {
                    free(qualified_id);
                    return;
                }
            }
            (void)AddIdentToTable(symtab->current_scope->table, qualified_id,
                NULL, HASHTYPE_TYPE, qualified_type);
            if (qualified_type != NULL)
                kgpc_type_release(qualified_type);
            if (FindSymbol(&existing, symtab, qualified_id) != 0 && existing != NULL)
            {
                existing->defined_in_unit = 1;
                existing->unit_is_public = node->unit_is_public;
                SymTab_MoveHashNodeToBack(symtab, existing);
            }
        }
    }

    free(qualified_id);
}

static inline void mark_latest_type_node_unit_info(SymTab_t *symtab, const char *type_id,
    int defined_in_unit, int unit_is_public, int source_unit_index)
{
    if (symtab == NULL || type_id == NULL)
        return;
    HashNode_t *type_node = NULL;
    FindSymbol(&type_node, symtab, type_id);
    if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
    {
        mark_hashnode_unit_info(symtab, type_node, defined_in_unit, unit_is_public);
        mark_hashnode_source_unit(type_node, source_unit_index);
    }
}

static Tree_t *g_semcheck_current_subprogram = NULL;

static int semcheck_prefer_unit_defined_owner(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return 0;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.defined_in_unit;
}

const char *semcheck_get_current_subprogram_id(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.id;
}

int semcheck_get_current_unit_index(void)
{
    return g_semcheck_current_unit_index;
}

const char *semcheck_get_current_subprogram_result_var_name(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.result_var_name;
}

const char *semcheck_get_current_subprogram_method_name(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.method_name;
}

const char *semcheck_get_current_subprogram_owner_class(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.owner_class;
}

const char *semcheck_get_current_subprogram_owner_class_full(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.owner_class_full;
}

const char *semcheck_get_current_subprogram_owner_class_outer(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.owner_class_outer;
}

int semcheck_get_current_subprogram_is_constructor(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return 0;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.is_constructor;
}

KgpcType *semcheck_get_current_subprogram_return_kgpc_type(SymTab_t *symtab, int *owns_type)
{
    if (owns_type != NULL)
        *owns_type = 0;
    if (g_semcheck_current_subprogram == NULL || symtab == NULL)
        return NULL;

    const char *rt_id = g_semcheck_current_subprogram->tree_data.subprogram_data.return_type_id;
    const TypeRef *rt_ref = g_semcheck_current_subprogram->tree_data.subprogram_data.return_type_ref;
    if (rt_id != NULL)
    {
        HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab, rt_ref, rt_id);
        if (type_node == NULL)
        {
            const char *owner_full = g_semcheck_current_subprogram->tree_data.subprogram_data.owner_class_full;
            const char *owner_outer = g_semcheck_current_subprogram->tree_data.subprogram_data.owner_class_outer;
            type_node = semcheck_find_type_node_in_owner_chain(symtab, rt_id, owner_full, owner_outer);
        }
        if (type_node != NULL && type_node->type != NULL)
        {
            if (owns_type != NULL)
                *owns_type = 0;
            return type_node->type;
        }

        int builtin_tag = semcheck_map_builtin_type_name_local(rt_id);
        if (builtin_tag != UNKNOWN_TYPE)
        {
            if (owns_type != NULL)
                *owns_type = 1;
            return create_primitive_type(builtin_tag);
        }
    }

    int rt_tag = g_semcheck_current_subprogram->tree_data.subprogram_data.return_type;
    if (rt_tag != UNKNOWN_TYPE)
    {
        if (owns_type != NULL)
            *owns_type = 1;
        return create_primitive_type(rt_tag);
    }

    return NULL;
}

ListNode_t *semcheck_clone_current_subprogram_actual_args(int include_self)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;

    ListNode_t *result = NULL;
    ListNode_t *tail = NULL;
    for (ListNode_t *cur = g_semcheck_current_subprogram->tree_data.subprogram_data.args_var;
         cur != NULL; cur = cur->next)
    {
        Tree_t *decl = (Tree_t *)cur->cur;
        ListNode_t *ids = NULL;
        if (decl == NULL)
            continue;
        if (decl->type == TREE_VAR_DECL)
            ids = decl->tree_data.var_decl_data.ids;
        else if (decl->type == TREE_ARR_DECL)
            ids = decl->tree_data.arr_decl_data.ids;
        else
            continue;

        for (ListNode_t *id_node = ids; id_node != NULL; id_node = id_node->next)
        {
            const char *param_id = (const char *)id_node->cur;
            if (param_id == NULL)
                continue;
            if (!include_self && pascal_identifier_equals(param_id, "Self"))
                continue;

            struct Expression *arg_expr = mk_varid(g_semcheck_current_subprogram->line_num, strdup(param_id));
            if (arg_expr == NULL)
            {
                destroy_list(result);
                return NULL;
            }

            ListNode_t *node = CreateListNode(arg_expr, LIST_EXPR);
            if (node == NULL)
            {
                destroy_expr(arg_expr);
                destroy_list(result);
                return NULL;
            }

            if (result == NULL)
                result = node;
            else
                tail->next = node;
            tail = node;
        }
    }

    return result;
}

void semcheck_mark_static_link_needed(HashNode_t *node)
{
    if (g_semcheck_current_subprogram == NULL || node == NULL)
        return;
    
    /* Only set requires_static_link if the function is actually nested (nesting_level > 1).
     * Top-level functions (nesting_level == 1) that access global variables do NOT need
     * a static link because global variables are accessed via their static labels.
     * 
     * This check prevents the issue where a top-level function like SetBacking, which
     * accesses a global variable, would incorrectly get requires_static_link=1 and then
     * callers would fail trying to pass a static link. */
    int nesting_level = g_semcheck_current_subprogram->tree_data.subprogram_data.nesting_level;
    if (nesting_level <= 1)
        return;

    switch (node->hash_type)
    {
        case HASHTYPE_VAR:
        case HASHTYPE_ARRAY:
        case HASHTYPE_FUNCTION_RETURN:
        case HASHTYPE_CONST:
            g_semcheck_current_subprogram->tree_data.subprogram_data.requires_static_link = 1;
            break;
        default:
            break;
    }
}

void semcheck_mark_call_requires_static_link(HashNode_t *callee)
{
    if (callee == NULL)
        return;
    if (g_semcheck_current_subprogram == NULL)
        return;
    if (!hashnode_requires_static_link(callee))
        return;
    /* When calling a function that requires a static link, the caller needs to
     * be able to PASS a static link. Mark the caller as having nested children
     * that need links, not as requiring a static link itself.
     * The caller only RECEIVES a static link if it's nested or accesses outer vars. */
    g_semcheck_current_subprogram->tree_data.subprogram_data.has_nested_requiring_link = 1;
}

int semcheck_program(SymTab_t *symtab, Tree_t *tree);
int semcheck_unit(SymTab_t *symtab, Tree_t *tree);

int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num);
int semcheck_type_decls(SymTab_t *symtab, ListNode_t *type_decls);
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls);
int semcheck_const_decls(SymTab_t *symtab, ListNode_t *const_decls);

/* Collect typed const declarations from a var_declaration list.
 * If from_unit_only is true, only collect those with defined_in_unit=1.
 * If from_unit_only is false, only collect those with defined_in_unit=0. */
static ListNode_t *collect_typed_const_decls_filtered(SymTab_t *symtab, ListNode_t *decls, int from_unit_only)
{
    ListNode_t *head = NULL;
    ListNode_t *tail = NULL;
    ListNode_t *cur = decls;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;
            if ((tree->type == TREE_VAR_DECL &&
                tree->tree_data.var_decl_data.is_typed_const) ||
                (tree->type == TREE_ARR_DECL &&
                tree->tree_data.arr_decl_data.is_typed_const))
            {
                /* Filter by origin */
                int is_from_unit = (tree->type == TREE_VAR_DECL) ?
                    tree->tree_data.var_decl_data.defined_in_unit :
                    tree->tree_data.arr_decl_data.defined_in_unit;
                if ((from_unit_only && !is_from_unit) || (!from_unit_only && is_from_unit))
                {
                    cur = cur->next;
                    continue;
                }

                int allow = 1;
                const char *type_id = NULL;
                int type_tag = UNKNOWN_TYPE;
                if (tree->type == TREE_VAR_DECL)
                {
                    type_id = tree->tree_data.var_decl_data.type_id;
                    type_tag = tree->tree_data.var_decl_data.type;
                }
                else
                {
                    type_id = tree->tree_data.arr_decl_data.type_id;
                    type_tag = tree->tree_data.arr_decl_data.type;
                }

                if (type_id != NULL)
                {
                    HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_id);
                    if (type_node == NULL &&
                        semcheck_map_builtin_type_name_local(type_id) == UNKNOWN_TYPE)
                    {
                        allow = 0;
                    }
                }
                else if (type_tag == UNKNOWN_TYPE)
                {
                    if (tree->type == TREE_ARR_DECL &&
                        tree->tree_data.arr_decl_data.element_kgpc_type != NULL)
                    {
                        allow = 1;
                    }
                    else
                    {
                        allow = 0;
                    }
                }

                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && tree->tree_data.var_decl_data.ids != NULL)
                {
                    const char *first_id = NULL;
                    if (tree->type == TREE_VAR_DECL)
                        first_id = tree->tree_data.var_decl_data.ids->cur ?
                            (const char *)tree->tree_data.var_decl_data.ids->cur : "<null>";
                    else
                        first_id = tree->tree_data.arr_decl_data.ids->cur ?
                            (const char *)tree->tree_data.arr_decl_data.ids->cur : "<null>";
                    fprintf(stderr, "[SemCheck] collect_typed_const: id=%s type_id=%s type=%d allow=%d inline_alias=%p\n",
                        first_id, type_id ? type_id : "<null>",
                        type_tag, allow,
                        (tree->type == TREE_VAR_DECL) ?
                            (void*)tree->tree_data.var_decl_data.inline_type_alias : NULL);
                }

                if (allow)
                {
                    ListNode_t *node = CreateListNode(tree, LIST_TREE);
                    if (head == NULL)
                    {
                        head = node;
                        tail = node;
                    }
                    else
                    {
                        tail->next = node;
                        tail = node;
                    }
                }
            }
        }
        cur = cur->next;
    }
    return head;
}

/* Collect non-typed var/array declarations (skip typed consts). */
static ListNode_t *collect_non_typed_var_decls(ListNode_t *decls)
{
    ListNode_t *head = NULL;
    ListNode_t *tail = NULL;
    ListNode_t *cur = decls;

    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;
            int skip = 0;
            if (tree->type == TREE_VAR_DECL)
                skip = tree->tree_data.var_decl_data.is_typed_const;
            else if (tree->type == TREE_ARR_DECL)
                skip = tree->tree_data.arr_decl_data.is_typed_const;
            else
                skip = 1;

            if (!skip)
            {
                ListNode_t *node = CreateListNode(tree, LIST_TREE);
                if (head == NULL)
                    head = node;
                else
                    tail->next = node;
                tail = node;
            }
        }
        cur = cur->next;
    }

    return head;
}

static int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls);
static int predeclare_types(SymTab_t *symtab, ListNode_t *type_decls);
static int predeclare_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev, Tree_t *parent_subprogram);

int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev);
