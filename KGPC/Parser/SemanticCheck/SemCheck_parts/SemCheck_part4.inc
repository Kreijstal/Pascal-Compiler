static int semcheck_single_const_decl(SymTab_t *symtab, Tree_t *tree)
{
    int return_val = 0;
    assert(tree != NULL);
    assert(tree->type == TREE_CONST_DECL);

    int saved_imported_unit = g_semcheck_imported_decl_unit_index;
    ScopeNode *saved_scope_for_const = semcheck_switch_to_unit_scope(
        symtab, tree->tree_data.const_decl_data.source_unit_index);
    if (tree->tree_data.const_decl_data.source_unit_index > 0)
    {
        g_semcheck_imported_decl_unit_index =
            tree->tree_data.const_decl_data.source_unit_index;
    }

    struct Expression *value_expr = tree->tree_data.const_decl_data.value;
    if (kgpc_getenv("KGPC_DEBUG_CLASS_CONST") != NULL &&
        tree->tree_data.const_decl_data.id != NULL)
    {
        fprintf(stderr, "[KGPC] const decl %s value_expr=%p type=%d\n",
            tree->tree_data.const_decl_data.id,
            (void *)value_expr,
            value_expr ? value_expr->type : -1);
    }
        
        /* Determine the type of constant by checking the expression */
        int is_string_const = expression_is_string(symtab, value_expr);
        int is_real_const = !is_string_const && expression_contains_real_literal_impl(symtab, value_expr);
        
        if (is_string_const)
        {
            /* Evaluate as string constant */
            char *string_value = NULL;
            if (evaluate_string_const_expr(symtab, value_expr, &string_value) != 0)
            {
                semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, unsupported string const expression.\n", tree->line_num);
                ++return_val;
            }
            else
            {
                int push_result;
                /* In Pascal, a single-character string literal constant is a Char, not a String.
                 * e.g., const PathDelim = '/' should be Char type. */
                if (string_value != NULL && strlen(string_value) == 1)
                {
                    long long char_val = (unsigned char)string_value[0];
                    KgpcType *char_type = create_primitive_type(CHAR_TYPE);
                    push_result = PushConstOntoScope_Typed(symtab,
                        tree->tree_data.const_decl_data.id, char_val, char_type);
                    destroy_kgpc_type(char_type);
                    if (push_result == 0)
                    {
                        HashNode_t *const_node = NULL;
                        if (FindSymbol(&const_node, symtab, tree->tree_data.const_decl_data.id) != 0 && const_node != NULL)
                        {
                            const_node->const_string_value = strdup(string_value);
                        }
                    }
                }
                else
                {
                    push_result = PushStringConstOntoScope(symtab, tree->tree_data.const_decl_data.id, string_value);
                }
                free(string_value);
                if (push_result > 0)
                {
                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, redeclaration of const %s!\n",
                            tree->line_num, tree->tree_data.const_decl_data.id);
                    ++return_val;
                }
                else
                {
                    HashNode_t *const_node = NULL;
                    if (FindSymbol(&const_node, symtab, tree->tree_data.const_decl_data.id) != 0 && const_node != NULL)
                    {
                        if (kgpc_getenv("KGPC_DEBUG_CLASS_CONST") != NULL)
                        {
                            fprintf(stderr, "[KGPC] const pushed: %s\n",
                                tree->tree_data.const_decl_data.id);
                        }
                        mark_hashnode_unit_info(symtab, const_node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
        }
        else if (is_real_const)
        {
            /* Evaluate as real constant */
            double real_value = 0.0;
            if (evaluate_real_const_expr(symtab, value_expr, &real_value) != 0)
            {
                semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, unsupported real const expression.\n", tree->line_num);
                ++return_val;
            }
            else
            {
                int push_result = PushRealConstOntoScope(symtab, tree->tree_data.const_decl_data.id, real_value);
                if (push_result > 0)
                {
                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, redeclaration of const %s!\n",
                            tree->line_num, tree->tree_data.const_decl_data.id);
                    ++return_val;
                }
                else
                {
                    HashNode_t *const_node = NULL;
                    if (FindSymbol(&const_node, symtab, tree->tree_data.const_decl_data.id) != 0 && const_node != NULL)
                    {
                        mark_hashnode_unit_info(symtab, const_node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
        }
        else if (value_expr != NULL && expression_is_set_const_expr(symtab, value_expr))
        {
            /* Evaluate as set constant (supports char sets up to 0..255) */
            unsigned char set_bytes[32];
            size_t set_size = 0;
            long long mask = 0;
            int is_char_set = 0;
            if (evaluate_set_const_bytes(symtab, value_expr, set_bytes, sizeof(set_bytes),
                    &set_size, &mask, &is_char_set) != 0)
            {
                semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, unsupported const expression.\n", tree->line_num);
                ++return_val;
            }
            else
            {
                KgpcType *const_type = create_primitive_type_with_size(SET_TYPE, (int)set_size);
                if (const_type != NULL && const_type->type_alias != NULL)
                {
                    const_type->type_alias->is_set = 1;
                    const_type->type_alias->set_element_type = is_char_set ? CHAR_TYPE : INT_TYPE;
                }
                int push_result = PushSetConstOntoScope(symtab, tree->tree_data.const_decl_data.id,
                    set_bytes, (int)set_size, const_type);
                destroy_kgpc_type(const_type);  /* Release creator's ref */
                if (push_result > 0)
                {
                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, redeclaration of const %s!\n",
                            tree->line_num, tree->tree_data.const_decl_data.id);
                    ++return_val;
                }
                else
                {
                    HashNode_t *const_node = NULL;
                    if (FindSymbol(&const_node, symtab, tree->tree_data.const_decl_data.id) != 0 && const_node != NULL)
                    {
                        mark_hashnode_unit_info(symtab, const_node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
        }
        else if (value_expr != NULL && value_expr->type == EXPR_ADDR &&
                 value_expr->expr_data.addr_data.expr != NULL &&
                 value_expr->expr_data.addr_data.expr->type == EXPR_VAR_ID)
        {
            /* Procedure address constant: const MyProcRef: TProc = @MyProc;
             * Handle forward-declared procedure references.
             * The procedure might be declared as forward and implemented later,
             * so we store the procedure name for later resolution in codegen. */
            char *proc_name = value_expr->expr_data.addr_data.expr->expr_data.id;
            if (proc_name != NULL)
            {
                /* Look up the procedure/function in the symbol table.
                 * It may be forward-declared, so we check if it exists at all. */
                HashNode_t *proc_node = NULL;
                int proc_found = FindSymbol(&proc_node, symtab, proc_name);
                /* Accept if it's a procedure, function, or not yet defined (forward ref).
                 * Forward references will be resolved later during codegen. */
                if (proc_found && proc_node != NULL &&
                    (proc_node->hash_type == HASHTYPE_PROCEDURE ||
                     proc_node->hash_type == HASHTYPE_FUNCTION))
                {
                    /* Found the procedure - store as a procedure address constant */
                    KgpcType *const_type = create_procedure_type(NULL, NULL);

                    /* Use 0 as the value - actual address will be resolved at link time */
                    int push_result = PushConstOntoScope_Typed(symtab,
                        tree->tree_data.const_decl_data.id, 0, const_type);
                    destroy_kgpc_type(const_type);  /* Release creator's ref */

                    if (push_result > 0)
                    {
                        semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, redeclaration of const %s!\n",
                                tree->line_num, tree->tree_data.const_decl_data.id);
                        ++return_val;
                    }
                    else
                    {
                        /* Store the procedure name in the const node for codegen */
                        HashNode_t *const_node = NULL;
                        if (FindSymbol(&const_node, symtab, tree->tree_data.const_decl_data.id) != 0 && const_node != NULL)
                        {
                            /* Store the procedure name as const_string_value for codegen to use */
                            const_node->const_string_value = strdup(proc_name);
                            mark_hashnode_unit_info(symtab, const_node,
                                tree->tree_data.const_decl_data.defined_in_unit,
                                tree->tree_data.const_decl_data.unit_is_public);
                        }
                    }
                }
                else if (!proc_found)
                {
                    /* Not found - might be a forward-declared procedure.
                     * Create the constant anyway; codegen will resolve it. */
                    KgpcType *const_type = create_procedure_type(NULL, NULL);

                    int push_result = PushConstOntoScope_Typed(symtab,
                        tree->tree_data.const_decl_data.id, 0, const_type);
                    destroy_kgpc_type(const_type);  /* Release creator's ref */

                    if (push_result > 0)
                    {
                        semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, redeclaration of const %s!\n",
                                tree->line_num, tree->tree_data.const_decl_data.id);
                        ++return_val;
                    }
                    else
                    {
                        HashNode_t *const_node = NULL;
                        if (FindSymbol(&const_node, symtab, tree->tree_data.const_decl_data.id) != 0 && const_node != NULL)
                        {
                            const_node->const_string_value = strdup(proc_name);
                            mark_hashnode_unit_info(symtab, const_node,
                                tree->tree_data.const_decl_data.defined_in_unit,
                                tree->tree_data.const_decl_data.unit_is_public);
                        }
                    }
                }
                else
                {
                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, '%s' is not a procedure or function.\n",
                            tree->line_num, proc_name);
                    ++return_val;
                }
            }
            else
            {
                semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, invalid procedure address expression.\n",
                        tree->line_num);
                ++return_val;
            }
        }
        else
        {
            /* Evaluate as integer constant */
            long long value = 0;
            
            /* Run semantic check on the value expression to resolve types properly.
             * This is important for scoped enum literals like TEndian.Little
             * which need to have their resolved_kgpc_type set. */
            KgpcType *expr_type = NULL;
            if (value_expr != NULL && value_expr->type == EXPR_RECORD_CONSTRUCTOR)
            {
                const char *decl_type_id = tree->tree_data.const_decl_data.type_id;
                const TypeRef *decl_type_ref = tree->tree_data.const_decl_data.type_ref;
                HashNode_t *type_node = NULL;
                struct RecordType *record_type = NULL;
                type_node = semcheck_find_preferred_type_node_with_ref(symtab, decl_type_ref, decl_type_id);
                if (type_node != NULL && type_node->type != NULL)
                {
                    record_type = kgpc_type_get_record(type_node->type);
                    if (record_type == NULL &&
                        type_node->type->kind == TYPE_KIND_POINTER &&
                        type_node->type->info.points_to != NULL)
                    {
                        record_type = kgpc_type_get_record(type_node->type->info.points_to);
                    }
                }
                if (record_type == NULL && type_node != NULL)
                {
                    record_type = get_record_type_from_node(type_node);
                }
                if (record_type != NULL)
                {
                    semcheck_typecheck_record_constructor(value_expr, symtab, INT_MAX, record_type, tree->line_num);
                }
            }
            if (value_expr != NULL && value_expr->type == EXPR_ARRAY_LITERAL &&
                value_expr->array_element_type == UNKNOWN_TYPE &&
                value_expr->array_element_type_id == NULL)
            {
                const char *decl_type_id = tree->tree_data.const_decl_data.type_id;
                const TypeRef *decl_type_ref = tree->tree_data.const_decl_data.type_ref;
                HashNode_t *type_node = semcheck_find_preferred_type_node_with_ref(
                    symtab, decl_type_ref, decl_type_id);
                KgpcType *decl_type = (type_node != NULL) ? type_node->type : NULL;
                if (decl_type != NULL && kgpc_type_is_array(decl_type))
                {
                    KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(decl_type, symtab);
                    if (elem_type != NULL)
                    {
                        value_expr->array_element_type = semcheck_tag_from_kgpc(elem_type);
                        if (elem_type->kind == TYPE_KIND_RECORD &&
                            elem_type->info.record_info != NULL)
                        {
                            value_expr->array_element_record_type = elem_type->info.record_info;
                            if (value_expr->array_element_type_id == NULL &&
                                elem_type->info.record_info->type_id != NULL)
                            {
                                value_expr->array_element_type_id =
                                    strdup(elem_type->info.record_info->type_id);
                            }
                        }
                        if (value_expr->array_element_type_id == NULL &&
                            elem_type->type_alias != NULL &&
                            elem_type->type_alias->target_type_id != NULL)
                        {
                            value_expr->array_element_type_id =
                                strdup(elem_type->type_alias->target_type_id);
                        }
                    }
                    if (value_expr->array_element_type_id == NULL &&
                        decl_type->info.array_info.element_type_id != NULL)
                    {
                        value_expr->array_element_type_id =
                            strdup(decl_type->info.array_info.element_type_id);
                    }
                }
            }
            /* If this const belongs to a record (id contains '.'), temporarily
               set the method owner so that unqualified nested type references
               (e.g. Node inside HeapTracer) resolve correctly via the owner chain. */
            const char *prev_method_owner = semcheck_get_current_method_owner();
            char *const_owner = NULL;
            {
                const char *const_id = tree->tree_data.const_decl_data.id;
                if (const_id != NULL)
                {
                    const char *dot = strrchr(const_id, '.');
                    if (dot != NULL && dot != const_id)
                    {
                        const_owner = strndup(const_id, (size_t)(dot - const_id));
                        semcheck_set_current_method_owner(const_owner);
                    }
                }
            }
            semcheck_expr_main(symtab, value_expr, INT_MAX, NO_MUTATE, &expr_type);

            if (evaluate_const_expr(symtab, value_expr, &value) != 0)
            {
                semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, unsupported const expression.\n", tree->line_num);
                ++return_val;
                semcheck_set_current_method_owner(prev_method_owner);
                free(const_owner);
            }
            else
            {
                semcheck_set_current_method_owner(prev_method_owner);
                free(const_owner);
                /* Check if constant already exists with same value (for re-exports like ARG_MAX = UnixType.ARG_MAX) */
                HashNode_t *existing = NULL;
                if (FindSymbol(&existing, symtab, tree->tree_data.const_decl_data.id) && existing != NULL &&
                    existing->hash_type == HASHTYPE_CONST &&
                    existing->const_int_value == value)
                {
                    HashNode_t *existing_in_target = NULL;
                    HashTable_t *target_table = SymTab_GetTargetTable(symtab);
                    if (target_table != NULL)
                        existing_in_target = FindIdentInTable(target_table,
                            tree->tree_data.const_decl_data.id);

                    /* Only skip when the current target scope already has this const.
                     * Re-exported same-value consts (e.g. Math.EqualsValue = types.EqualsValue)
                     * must still be published into the re-exporting unit's own table. */
                    if (existing_in_target != NULL &&
                        existing_in_target->hash_type == HASHTYPE_CONST &&
                        existing_in_target->const_int_value == value)
                    {
                        g_semcheck_imported_decl_unit_index = saved_imported_unit;
                        semcheck_restore_scope(symtab, saved_scope_for_const);
                        return 0;
                    }
                }


                
                /* Create KgpcType if this is a set constant or has an explicit type annotation */
                KgpcType *const_type = NULL;
                /* If the const expression is an explicit typecast, prefer that as the const's type. */
                if (value_expr != NULL && value_expr->type == EXPR_TYPECAST)
                {
                    int target_tag = (value_expr->resolved_kgpc_type != NULL)
                        ? semcheck_tag_from_kgpc(value_expr->resolved_kgpc_type)
                        : UNKNOWN_TYPE;
                    if (target_tag == UNKNOWN_TYPE &&
                        value_expr->expr_data.typecast_data.target_type_id != NULL)
                    {
                        target_tag = semcheck_map_builtin_type_name_local(
                            value_expr->expr_data.typecast_data.target_type_id);
                    }
                    if (target_tag != UNKNOWN_TYPE)
                        const_type = create_primitive_type(target_tag);
                }
                if (value_expr != NULL && value_expr->type == EXPR_SET)
                {
                    const_type = create_primitive_type(SET_TYPE);
                }
                else if (value_expr != NULL && value_expr->type == EXPR_BOOL)
                {
                    const_type = create_primitive_type(BOOL);
                }
                /* Check if the expression has enum type (e.g., scoped enum literals like TEnum.Value) */
                else if (value_expr != NULL && value_expr->resolved_kgpc_type != NULL)
                {
                    struct TypeAlias *alias = kgpc_type_get_type_alias(value_expr->resolved_kgpc_type);
                    if (alias != NULL && alias->is_enum)
                    {
                        /* Preserve the enum type for the constant */
                        const_type = value_expr->resolved_kgpc_type;
                        kgpc_type_retain(const_type);
                    }
                    else
                    {
                        /* Preserve unsigned integer types (e.g. QWORD_TYPE from High(QWord))
                         * so that the codegen can use unsigned comparisons. */
                        int tag = semcheck_tag_from_kgpc(value_expr->resolved_kgpc_type);
                        if (is_unsigned_integer_type(tag))
                        {
                            const_type = value_expr->resolved_kgpc_type;
                            kgpc_type_retain(const_type);
                        }
                    }
                }
                else if (tree->tree_data.const_decl_data.type_id != NULL)
                {
                    /* Check if the type annotation is "boolean" */
                    const char *type_id = tree->tree_data.const_decl_data.type_id;
                    if (strcasecmp(type_id, "boolean") == 0)
                    {
                        const_type = create_primitive_type(BOOL);
                    }
                    /* Add more type checks as needed */
                }
                
                /* Use typed or legacy API depending on whether we have a KgpcType */
                int push_result;
                if (const_type != NULL)
                {
                    push_result = PushConstOntoScope_Typed(symtab, tree->tree_data.const_decl_data.id, value, const_type);
                    destroy_kgpc_type(const_type);  /* Release creator's ref */
                }
                else
                {
                    push_result = PushConstOntoScope(symtab, tree->tree_data.const_decl_data.id, value);
                }

                if (push_result > 0)
                {
                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, redeclaration of const %s!\n",
                            tree->line_num, tree->tree_data.const_decl_data.id);
                    ++return_val;
                }
                else
                {
                    HashNode_t *const_node = NULL;
                    if (FindSymbol(&const_node, symtab, tree->tree_data.const_decl_data.id) != 0 && const_node != NULL)
                    {
                        mark_hashnode_unit_info(symtab, const_node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
        }

    g_semcheck_imported_decl_unit_index = saved_imported_unit;
    semcheck_restore_scope(symtab, saved_scope_for_const);
    return return_val;
}

/* Quick pre-pass: push trivially evaluable imported constants (literal integers,
 * literal reals, simple identifier references) onto the symbol table so that
 * cross-unit qualified references like types.EqualsValue can resolve them.
 *
 * Called during unit loading (semcheck_unit_decls_only) and System predeclaration
 * (semcheck_predeclare_program_into_unit_scope) to populate per-unit scopes
 * before full const evaluation.  Cross-unit resolution uses scope dependency
 * edges rather than relying on list concatenation order. */
static void prepush_trivial_imported_consts(SymTab_t *symtab, ListNode_t *const_decls,
    int include_local_unit_consts)
{
    for (ListNode_t *cur = const_decls; cur != NULL; cur = cur->next)
    {
        Tree_t *tree = (Tree_t *)cur->cur;
        int target_unit_index = tree->tree_data.const_decl_data.source_unit_index;
        if (!tree->tree_data.const_decl_data.defined_in_unit && !include_local_unit_consts)
            continue;
        if (tree->tree_data.const_decl_data.id == NULL)
            continue;
        if (target_unit_index <= 0)
            continue;

        ScopeNode *target_scope = GetOrCreateUnitScope(symtab, target_unit_index);
        if (target_scope == NULL || target_scope->table == NULL)
            continue;

        /* Skip if already in symbol table */
        HashNode_t *existing = FindIdentInTable(target_scope->table, tree->tree_data.const_decl_data.id);
        if (existing != NULL &&
            existing != NULL &&
            (existing->hash_type == HASHTYPE_CONST || existing->is_typed_const))
            continue;

        struct Expression *value_expr = tree->tree_data.const_decl_data.value;
        if (value_expr == NULL)
            continue;

        ScopeNode *saved_scope_for_prepush = semcheck_switch_to_unit_scope(symtab, target_unit_index);

        /* Handle string constants (EXPR_STRING, EXPR_CHAR_CODE) */
        if (value_expr->type == EXPR_STRING && value_expr->expr_data.string != NULL)
        {
            const char *string_value = value_expr->expr_data.string;
            /* Single-character strings are Char type in Pascal */
            if (strlen(string_value) == 1)
            {
                long long char_val = (unsigned char)string_value[0];
                KgpcType *char_type = create_primitive_type(CHAR_TYPE);
                int push_result = AddIdentToTable(target_scope->table,
                    tree->tree_data.const_decl_data.id, NULL, HASHTYPE_CONST, char_type);
                destroy_kgpc_type(char_type);
                if (push_result == 0)
                {
                    HashNode_t *node = FindIdentInTable(target_scope->table,
                        tree->tree_data.const_decl_data.id);
                    if (node != NULL)
                    {
                        node->is_constant = 1;
                        node->const_int_value = char_val;
                        node->const_string_value = strdup(string_value);
                        mark_hashnode_unit_info(symtab, node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                        mark_hashnode_source_unit(node, target_unit_index);
                    }
                }
            }
            else
            {
                KgpcType *string_type = create_primitive_type(STRING_TYPE);
                if (string_type != NULL)
                {
                    int push_result = AddIdentToTable(target_scope->table,
                        tree->tree_data.const_decl_data.id, NULL, HASHTYPE_CONST, string_type);
                    if (push_result == 0)
                    {
                        HashNode_t *node = FindIdentInTable(target_scope->table,
                            tree->tree_data.const_decl_data.id);
                        if (node != NULL)
                        {
                            node->is_constant = 1;
                            node->const_string_value = strdup(string_value);
                            mark_hashnode_unit_info(symtab, node,
                                tree->tree_data.const_decl_data.defined_in_unit,
                                tree->tree_data.const_decl_data.unit_is_public);
                            mark_hashnode_source_unit(node, target_unit_index);
                        }
                    }
                    destroy_kgpc_type(string_type);
                }
            }
            semcheck_restore_scope(symtab, saved_scope_for_prepush);
            continue;
        }
        if (value_expr->type == EXPR_CHAR_CODE)
        {
            /* Push as typed Char constant (matching normal semcheck behavior) */
            long long char_val = (unsigned char)(value_expr->expr_data.char_code & 0xFF);
            KgpcType *char_type = create_primitive_type(CHAR_TYPE);
            int push_result = AddIdentToTable(target_scope->table,
                tree->tree_data.const_decl_data.id, NULL, HASHTYPE_CONST, char_type);
            destroy_kgpc_type(char_type);
            if (push_result == 0)
            {
                HashNode_t *node = FindIdentInTable(target_scope->table,
                    tree->tree_data.const_decl_data.id);
                if (node != NULL)
                {
                    node->is_constant = 1;
                    node->const_int_value = char_val;
                    /* Also set string value like normal semcheck does */
                    char str[2] = { (char)(char_val & 0xFF), '\0' };
                    node->const_string_value = strdup(str);
                    mark_hashnode_unit_info(symtab, node,
                        tree->tree_data.const_decl_data.defined_in_unit,
                        tree->tree_data.const_decl_data.unit_is_public);
                    mark_hashnode_source_unit(node, target_unit_index);
                }
            }
            semcheck_restore_scope(symtab, saved_scope_for_prepush);
            continue;
        }

        {
            long long char_value = 0;
            int char_size = 1;
            if (semcheck_const_expr_char_cast_value(value_expr, &char_value, &char_size) == 0)
            {
                KgpcType *char_type = create_primitive_type_with_size(CHAR_TYPE, char_size);
                int push_result = AddIdentToTable(target_scope->table,
                    tree->tree_data.const_decl_data.id, NULL, HASHTYPE_CONST, char_type);
                destroy_kgpc_type(char_type);
                if (push_result == 0)
                {
                    HashNode_t *node = FindIdentInTable(target_scope->table,
                        tree->tree_data.const_decl_data.id);
                    if (node != NULL)
                    {
                        node->is_constant = 1;
                        node->const_int_value = char_value;
                        if (char_size == 1)
                        {
                            char str[2] = { (char)(char_value & 0xFF), '\0' };
                            node->const_string_value = strdup(str);
                        }
                        mark_hashnode_unit_info(symtab, node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                        mark_hashnode_source_unit(node, target_unit_index);
                    }
                }
                semcheck_restore_scope(symtab, saved_scope_for_prepush);
                continue;
            }
        }

        /* Imported string aliases/concatenations (e.g. sLineBreak = LineEnding)
         * must preserve string type here even if the underlying literal was a
         * single character. Otherwise dependent units see them as ordinals. */
        if (expression_is_string(symtab, value_expr))
        {
            char *string_value = NULL;
            if (evaluate_string_const_expr(symtab, value_expr, &string_value) == 0 &&
                string_value != NULL)
            {
                KgpcType *string_type = create_primitive_type(STRING_TYPE);
                if (string_type != NULL &&
                    AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                        NULL, HASHTYPE_CONST, string_type) == 0)
                {
                    HashNode_t *node = FindIdentInTable(target_scope->table,
                        tree->tree_data.const_decl_data.id);
                    if (node != NULL)
                    {
                        node->is_constant = 1;
                        node->const_string_value = string_value;
                        string_value = NULL;
                        if (node->const_string_value[0] != '\0' &&
                            node->const_string_value[1] == '\0')
                        {
                            node->const_int_value = (unsigned char)node->const_string_value[0];
                        }
                        mark_hashnode_unit_info(symtab, node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                        mark_hashnode_source_unit(node, target_unit_index);
                    }
                }
                destroy_kgpc_type(string_type);
            }
            free(string_value);
            semcheck_restore_scope(symtab, saved_scope_for_prepush);
            continue;
        }

        if (value_expr->type == EXPR_BOOL)
        {
            KgpcType *bool_type = create_primitive_type(BOOL);
            int push_result = AddIdentToTable(target_scope->table,
                tree->tree_data.const_decl_data.id, NULL, HASHTYPE_CONST, bool_type);
            destroy_kgpc_type(bool_type);
            if (push_result == 0)
            {
                HashNode_t *node = FindIdentInTable(target_scope->table,
                    tree->tree_data.const_decl_data.id);
                if (node != NULL)
                {
                    node->is_constant = 1;
                    node->const_int_value = value_expr->expr_data.i_num ? 1 : 0;
                    mark_hashnode_unit_info(symtab, node,
                        tree->tree_data.const_decl_data.defined_in_unit,
                        tree->tree_data.const_decl_data.unit_is_public);
                    mark_hashnode_source_unit(node, target_unit_index);
                }
            }
            semcheck_restore_scope(symtab, saved_scope_for_prepush);
            continue;
        }

        /* Imported set constants must stay typed as sets; if we fold them as
         * integers here, later `x in imported_set_const` checks misclassify the
         * right-hand side as ordinal rather than set. */
        if (expression_is_set_const_expr(symtab, value_expr))
        {
            unsigned char set_bytes[32];
            size_t set_size = 0;
            long long mask = 0;
            int is_char_set = 0;
            if (evaluate_set_const_bytes(symtab, value_expr, set_bytes, sizeof(set_bytes),
                    &set_size, &mask, &is_char_set) == 0)
            {
                unsigned char *copy = NULL;
                if (set_size > 0)
                {
                    copy = (unsigned char *)malloc(set_size);
                    if (copy == NULL)
                    {
                        semcheck_restore_scope(symtab, saved_scope_for_prepush);
                        continue;
                    }
                    memcpy(copy, set_bytes, set_size);
                }
                KgpcType *set_type = create_primitive_type_with_size(SET_TYPE, (int)set_size);
                if (set_type != NULL && set_type->type_alias != NULL)
                {
                    set_type->type_alias->is_set = 1;
                    set_type->type_alias->set_element_type = is_char_set ? CHAR_TYPE : INT_TYPE;
                }
                if (set_type != NULL &&
                    AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                        NULL, HASHTYPE_CONST, set_type) == 0)
                {
                    HashNode_t *node = FindIdentInTable(target_scope->table,
                        tree->tree_data.const_decl_data.id);
                    if (node != NULL)
                    {
                        node->is_constant = 1;
                        node->const_set_value = copy;
                        node->const_set_size = (int)set_size;
                        if (set_size <= sizeof(long long))
                        {
                            long long small_mask = 0;
                            for (size_t i = 0; i < set_size; ++i)
                                small_mask |= ((long long)copy[i]) << (i * 8);
                            node->const_int_value = small_mask;
                        }
                        mark_hashnode_unit_info(symtab, node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                        mark_hashnode_source_unit(node, target_unit_index);
                    }
                }
                else
                {
                    free(copy);
                }
                destroy_kgpc_type(set_type);
                semcheck_restore_scope(symtab, saved_scope_for_prepush);
                continue;
            }
        }

        /* Generic constant-fold prepush for imported consts. This is needed
         * during decls-only unit loading so dependent typed const declarations
         * can see non-trivial integer/real constants (e.g. OT_* masks in
         * aasmcpu) before the full const pass runs. */
        {
            long long folded_int = 0;
            if (const_fold_int_expr(symtab, value_expr, &folded_int) == 0)
            {
                int type_tag = (folded_int > INT_MAX || folded_int < INT_MIN) ? INT64_TYPE : INT_TYPE;
                KgpcType *int_type = create_primitive_type(type_tag);
                if (int_type != NULL &&
                    AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                        NULL, HASHTYPE_CONST, int_type) == 0)
                {
                    HashNode_t *node = FindIdentInTable(target_scope->table,
                        tree->tree_data.const_decl_data.id);
                    if (node != NULL)
                    {
                        node->is_constant = 1;
                        node->const_int_value = folded_int;
                        mark_hashnode_unit_info(symtab, node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                        mark_hashnode_source_unit(node, target_unit_index);
                    }
                }
                destroy_kgpc_type(int_type);
                semcheck_restore_scope(symtab, saved_scope_for_prepush);
                continue;
            }

            double folded_real = 0.0;
            if (const_fold_real_expr(symtab, value_expr, &folded_real) == 0)
            {
                KgpcType *real_type = create_primitive_type(REAL_TYPE);
                if (real_type != NULL &&
                    AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                        NULL, HASHTYPE_CONST, real_type) == 0)
                {
                    HashNode_t *node = FindIdentInTable(target_scope->table,
                        tree->tree_data.const_decl_data.id);
                    if (node != NULL)
                    {
                        node->is_constant = 1;
                        node->const_real_value = folded_real;
                        mark_hashnode_unit_info(symtab, node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                        mark_hashnode_source_unit(node, target_unit_index);
                    }
                }
                destroy_kgpc_type(real_type);
                semcheck_restore_scope(symtab, saved_scope_for_prepush);
                continue;
            }
        }

        /* Only handle remaining trivially evaluable expressions */
        if (value_expr->type == EXPR_INUM)
        {
            long long val = value_expr->expr_data.i_num;
            int type_tag = (val > INT_MAX || val < INT_MIN) ? INT64_TYPE : INT_TYPE;
            KgpcType *int_type = create_primitive_type(type_tag);
            if (int_type != NULL &&
                AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                    NULL, HASHTYPE_CONST, int_type) == 0)
            {
                HashNode_t *node = FindIdentInTable(target_scope->table,
                    tree->tree_data.const_decl_data.id);
                if (node != NULL)
                {
                    node->is_constant = 1;
                    node->const_int_value = val;
                    mark_hashnode_unit_info(symtab, node,
                        tree->tree_data.const_decl_data.defined_in_unit,
                        tree->tree_data.const_decl_data.unit_is_public);
                    mark_hashnode_source_unit(node, target_unit_index);
                }
            }
            destroy_kgpc_type(int_type);
        }
        else if (value_expr->type == EXPR_RNUM)
        {
            double val = value_expr->expr_data.r_num;
            KgpcType *real_type = create_primitive_type(REAL_TYPE);
            if (real_type != NULL &&
                AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                    NULL, HASHTYPE_CONST, real_type) == 0)
            {
                HashNode_t *node = FindIdentInTable(target_scope->table,
                    tree->tree_data.const_decl_data.id);
                if (node != NULL)
                {
                    node->is_constant = 1;
                    node->const_real_value = val;
                    mark_hashnode_unit_info(symtab, node,
                        tree->tree_data.const_decl_data.defined_in_unit,
                        tree->tree_data.const_decl_data.unit_is_public);
                    mark_hashnode_source_unit(node, target_unit_index);
                }
            }
            destroy_kgpc_type(real_type);
        }
        else if (value_expr->type == EXPR_VAR_ID && value_expr->expr_data.id != NULL)
        {
            /* Simple reference to another constant */
            HashNode_t *ref = NULL;
            if (FindSymbol(&ref, symtab, value_expr->expr_data.id) != 0 && ref != NULL &&
                (ref->hash_type == HASHTYPE_CONST || ref->is_typed_const))
            {
                int ref_tag = (ref->type != NULL) ? semcheck_tag_from_kgpc(ref->type) : UNKNOWN_TYPE;
                if (ref->const_string_value != NULL)
                {
                    const char *sv = ref->const_string_value;
                    if (strlen(sv) == 1 && ref->type != NULL &&
                        kgpc_type_equals_tag(ref->type, CHAR_TYPE))
                    {
                        /* Reference to a Char const — push as typed Char */
                        long long char_val = (unsigned char)sv[0];
                        KgpcType *char_type = create_primitive_type(CHAR_TYPE);
                        int pr = AddIdentToTable(target_scope->table,
                            tree->tree_data.const_decl_data.id, NULL, HASHTYPE_CONST, char_type);
                        destroy_kgpc_type(char_type);
                        if (pr == 0)
                        {
                            HashNode_t *n2 = FindIdentInTable(target_scope->table,
                                tree->tree_data.const_decl_data.id);
                            if (n2 != NULL)
                            {
                                n2->is_constant = 1;
                                n2->const_int_value = char_val;
                                n2->const_string_value = strdup(sv);
                            }
                        }
                    }
                    else
                    {
                        KgpcType *string_type = create_primitive_type(STRING_TYPE);
                        if (string_type != NULL &&
                            AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                                NULL, HASHTYPE_CONST, string_type) == 0)
                        {
                            HashNode_t *n2 = FindIdentInTable(target_scope->table,
                                tree->tree_data.const_decl_data.id);
                            if (n2 != NULL)
                            {
                                n2->is_constant = 1;
                                n2->const_string_value = strdup(sv);
                            }
                        }
                        destroy_kgpc_type(string_type);
                    }
                }
                else if (ref_tag == BOOL)
                {
                    KgpcType *bool_type = create_primitive_type(BOOL);
                    if (bool_type != NULL &&
                        AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                            NULL, HASHTYPE_CONST, bool_type) == 0)
                    {
                        HashNode_t *n2 = FindIdentInTable(target_scope->table,
                            tree->tree_data.const_decl_data.id);
                        if (n2 != NULL)
                        {
                            n2->is_constant = 1;
                            n2->const_int_value = ref->const_int_value ? 1 : 0;
                        }
                    }
                    destroy_kgpc_type(bool_type);
                }
                else if (ref_tag == SET_TYPE && ref->const_set_value != NULL && ref->const_set_size > 0)
                {
                    unsigned char *copy = (unsigned char *)malloc((size_t)ref->const_set_size);
                    if (copy == NULL)
                    {
                        semcheck_restore_scope(symtab, saved_scope_for_prepush);
                        continue;
                    }
                    memcpy(copy, ref->const_set_value, (size_t)ref->const_set_size);
                    KgpcType *set_type = create_primitive_type_with_size(SET_TYPE, ref->const_set_size);
                    if (set_type != NULL && set_type->type_alias != NULL)
                    {
                        set_type->type_alias->is_set = 1;
                    }
                    if (set_type != NULL &&
                        AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                            NULL, HASHTYPE_CONST, set_type) == 0)
                    {
                        HashNode_t *n2 = FindIdentInTable(target_scope->table,
                            tree->tree_data.const_decl_data.id);
                        if (n2 != NULL)
                        {
                            n2->is_constant = 1;
                            n2->const_set_value = copy;
                            n2->const_set_size = ref->const_set_size;
                            if (ref->const_set_size <= (int)sizeof(long long))
                                n2->const_int_value = ref->const_int_value;
                        }
                    }
                    else
                    {
                        free(copy);
                    }
                    destroy_kgpc_type(set_type);
                }
                else if (ref->type != NULL && kgpc_type_is_real(ref->type))
                {
                    KgpcType *real_type = create_primitive_type(REAL_TYPE);
                    if (real_type != NULL &&
                        AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                            NULL, HASHTYPE_CONST, real_type) == 0)
                    {
                        HashNode_t *n2 = FindIdentInTable(target_scope->table,
                            tree->tree_data.const_decl_data.id);
                        if (n2 != NULL)
                        {
                            n2->is_constant = 1;
                            n2->const_real_value = ref->const_real_value;
                        }
                    }
                    destroy_kgpc_type(real_type);
                }
                else if (ref->type != NULL &&
                         ref_tag != UNKNOWN_TYPE &&
                         (kgpc_type_get_type_alias(ref->type) != NULL ||
                          is_unsigned_integer_type(ref_tag) ||
                          ref_tag == ENUM_TYPE))
                {
                    kgpc_type_retain(ref->type);
                    if (AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                            NULL, HASHTYPE_CONST, ref->type) == 0)
                    {
                        HashNode_t *n2 = FindIdentInTable(target_scope->table,
                            tree->tree_data.const_decl_data.id);
                        if (n2 != NULL)
                        {
                            n2->is_constant = 1;
                            n2->const_int_value = ref->const_int_value;
                        }
                    }
                    destroy_kgpc_type(ref->type);
                }
                else
                {
                    int type_tag = (ref->const_int_value > INT_MAX || ref->const_int_value < INT_MIN)
                        ? INT64_TYPE : INT_TYPE;
                    KgpcType *int_type = create_primitive_type(type_tag);
                    if (int_type != NULL &&
                        AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                            NULL, HASHTYPE_CONST, int_type) == 0)
                    {
                        HashNode_t *n2 = FindIdentInTable(target_scope->table,
                            tree->tree_data.const_decl_data.id);
                        if (n2 != NULL)
                        {
                            n2->is_constant = 1;
                            n2->const_int_value = ref->const_int_value;
                        }
                    }
                    destroy_kgpc_type(int_type);
                }
                HashNode_t *node = FindIdentInTable(target_scope->table,
                    tree->tree_data.const_decl_data.id);
                if (node != NULL)
                {
                    mark_hashnode_unit_info(symtab, node,
                        tree->tree_data.const_decl_data.defined_in_unit,
                        tree->tree_data.const_decl_data.unit_is_public);
                    mark_hashnode_source_unit(node, target_unit_index);
                }
            }
        }
        else if (value_expr->type == EXPR_SIGN_TERM && value_expr->expr_data.sign_term != NULL &&
                 value_expr->expr_data.sign_term->type == EXPR_INUM)
        {
            long long val = -value_expr->expr_data.sign_term->expr_data.i_num;
            int type_tag = (val > INT_MAX || val < INT_MIN) ? INT64_TYPE : INT_TYPE;
            KgpcType *int_type = create_primitive_type(type_tag);
            if (int_type != NULL &&
                AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                    NULL, HASHTYPE_CONST, int_type) == 0)
            {
                HashNode_t *node = FindIdentInTable(target_scope->table,
                    tree->tree_data.const_decl_data.id);
                if (node != NULL)
                {
                    node->is_constant = 1;
                    node->const_int_value = val;
                    mark_hashnode_unit_info(symtab, node,
                        tree->tree_data.const_decl_data.defined_in_unit,
                        tree->tree_data.const_decl_data.unit_is_public);
                    mark_hashnode_source_unit(node, target_unit_index);
                }
            }
            destroy_kgpc_type(int_type);
        }
        else if (value_expr->type == EXPR_FUNCTION_CALL)
        {
            /* Try full const evaluation silently for expressions like Low()/High(). */
            long long val = 0;
            int ok = const_fold_int_expr(symtab, value_expr, &val);
            if (ok == 0)
            {
                int type_tag = (val > INT_MAX || val < INT_MIN) ? INT64_TYPE : INT_TYPE;
                KgpcType *int_type = create_primitive_type(type_tag);
                if (int_type != NULL &&
                    AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                        NULL, HASHTYPE_CONST, int_type) == 0)
                {
                    HashNode_t *node = FindIdentInTable(target_scope->table,
                        tree->tree_data.const_decl_data.id);
                    if (node != NULL)
                    {
                        node->is_constant = 1;
                        node->const_int_value = val;
                        mark_hashnode_unit_info(symtab, node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                        mark_hashnode_source_unit(node, target_unit_index);
                    }
                }
                destroy_kgpc_type(int_type);
            }
            else
            {
                /* Also try real const evaluation */
                double rval = 0.0;
                ok = const_fold_real_expr(symtab, value_expr, &rval);
                if (ok == 0)
                {
                    KgpcType *real_type = create_primitive_type(REAL_TYPE);
                    if (real_type != NULL &&
                        AddIdentToTable(target_scope->table, tree->tree_data.const_decl_data.id,
                            NULL, HASHTYPE_CONST, real_type) == 0)
                    {
                        HashNode_t *node = FindIdentInTable(target_scope->table,
                            tree->tree_data.const_decl_data.id);
                        if (node != NULL)
                        {
                            node->is_constant = 1;
                            node->const_real_value = rval;
                            mark_hashnode_unit_info(symtab, node,
                                tree->tree_data.const_decl_data.defined_in_unit,
                                tree->tree_data.const_decl_data.unit_is_public);
                            mark_hashnode_source_unit(node, target_unit_index);
                        }
                    }
                    destroy_kgpc_type(real_type);
                }
            }
        }

        semcheck_restore_scope(symtab, saved_scope_for_prepush);
    }
}

static int semcheck_const_decls_local(SymTab_t *symtab, ListNode_t *const_decls)
{
    int return_val = 0;
    ListNode_t *cur = const_decls;
    while (cur != NULL)
    {
        assert(cur->type == LIST_TREE);
        Tree_t *tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_CONST_DECL);
        if (kgpc_getenv("KGPC_DEBUG_CLASS_CONST") != NULL &&
            tree->tree_data.const_decl_data.id != NULL)
        {
            fprintf(stderr, "[KGPC] local const seen: %s (defined_in_unit=%d)\n",
                tree->tree_data.const_decl_data.id,
                tree->tree_data.const_decl_data.defined_in_unit);
        }
        if (!tree->tree_data.const_decl_data.defined_in_unit)
            return_val += semcheck_single_const_decl(symtab, tree);
        cur = cur->next;
    }
    return return_val;
}

static int semcheck_const_expr_refs_current_unit_qualified_id(struct Expression *expr)
{
    if (expr == NULL || g_semcheck_current_unit_index == 0)
        return 0;
    if (expr->type != EXPR_RECORD_ACCESS)
        return 0;

    struct Expression *owner = expr->expr_data.record_access_data.record_expr;
    if (owner == NULL || owner->type != EXPR_VAR_ID || owner->expr_data.id == NULL)
        return 0;

    const char *cur_unit_str = unit_registry_get(g_semcheck_current_unit_index);
    return cur_unit_str != NULL && pascal_identifier_equals(owner->expr_data.id, cur_unit_str);
}

static int semcheck_const_decls_imported_filtered(SymTab_t *symtab, ListNode_t *const_decls,
    int defer_current_unit_qualified)
{
    int return_val = 0;
    ListNode_t *cur = const_decls;
    while (cur != NULL)
    {
        assert(cur->type == LIST_TREE);
        Tree_t *tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_CONST_DECL);
        if (tree->tree_data.const_decl_data.defined_in_unit)
        {
            int is_deferred =
                semcheck_const_expr_refs_current_unit_qualified_id(
                    tree->tree_data.const_decl_data.value);
            if ((defer_current_unit_qualified && is_deferred) ||
                (!defer_current_unit_qualified && !is_deferred))
            {
                cur = cur->next;
                continue;
            }
            return_val += semcheck_single_const_decl(symtab, tree);
        }
        cur = cur->next;
    }
    return return_val;
}

struct typed_const_init_backup
{
    Tree_t *tree;
    struct Statement *initializer;
};

static int semcheck_decl_only_should_skip_typed_const_initializer(const struct Statement *initializer)
{
    if (initializer == NULL)
        return 0;

    if (initializer->type == STMT_COMPOUND_STATEMENT)
        return 1;

    if (initializer->type != STMT_VAR_ASSIGN ||
        initializer->stmt_data.var_assign_data.expr == NULL)
        return 0;

    struct Expression *init_expr = initializer->stmt_data.var_assign_data.expr;
    return (init_expr->type == EXPR_ARRAY_LITERAL ||
            init_expr->type == EXPR_RECORD_CONSTRUCTOR);
}

/* During decls-only unit loading we need imported typed const symbols and their
 * declared types visible to dependent units, but we must not semcheck their
 * initializer expressions yet. Those initializers can depend on local enum/set
 * declarations that are only fully valid in the later full semantic pass. */
static int semcheck_decl_only_typed_const_decls(SymTab_t *symtab, ListNode_t *typed_const_decls)
{
    if (typed_const_decls == NULL)
        return 0;

    int count = 0;
    for (ListNode_t *cur = typed_const_decls; cur != NULL; cur = cur->next)
        ++count;

    struct typed_const_init_backup *saved =
        (struct typed_const_init_backup *)calloc((size_t)count, sizeof(*saved));
    if (saved == NULL)
        return semcheck_decls(symtab, typed_const_decls);

    int idx = 0;
    for (ListNode_t *cur = typed_const_decls; cur != NULL; cur = cur->next, ++idx)
    {
        Tree_t *tree = (Tree_t *)cur->cur;
        saved[idx].tree = tree;
        if (tree == NULL)
            continue;

        if (tree->type == TREE_VAR_DECL)
        {
            saved[idx].initializer = tree->tree_data.var_decl_data.initializer;
            if (semcheck_decl_only_should_skip_typed_const_initializer(saved[idx].initializer))
                tree->tree_data.var_decl_data.initializer = NULL;
        }
        else if (tree->type == TREE_ARR_DECL)
        {
            saved[idx].initializer = tree->tree_data.arr_decl_data.initializer;
            if (semcheck_decl_only_should_skip_typed_const_initializer(saved[idx].initializer))
                tree->tree_data.arr_decl_data.initializer = NULL;
        }
    }

    int return_val = semcheck_decls(symtab, typed_const_decls);

    for (int restore_idx = 0; restore_idx < count; ++restore_idx)
    {
        Tree_t *tree = saved[restore_idx].tree;
        if (tree == NULL)
            continue;

        if (tree->type == TREE_VAR_DECL)
            tree->tree_data.var_decl_data.initializer = saved[restore_idx].initializer;
        else if (tree->type == TREE_ARR_DECL)
            tree->tree_data.arr_decl_data.initializer = saved[restore_idx].initializer;
    }

    free(saved);
    return return_val;
}

/* Semantic check on constant declarations.
 *
 * Two-pass processing to handle qualified constant references.
 * Each constant is pushed into its per-unit scope (via source_unit_index),
 * and cross-unit references resolve through scope dependency edges.
 * Imported constants are processed first so that local constants referencing
 * them (e.g. ARG_MAX = UnixType.ARG_MAX) can find them during evaluation.
 *   Pass 1: Process constants from imported units (defined_in_unit=1)
 *   Pass 2: Process local constants (defined_in_unit=0)
 */
int semcheck_const_decls(SymTab_t *symtab, ListNode_t *const_decls)
{
    int return_val = 0;
    prepush_trivial_imported_consts(symtab, const_decls, 0);
    return_val += semcheck_const_decls_imported_filtered(symtab, const_decls, 1);
    return_val += semcheck_const_decls_local(symtab, const_decls);
    /* Retry deferred imported constants that reference CurrentUnit.ConstName.
     * In cyclic unit dependencies (e.g. types <-> math), these can only be
     * resolved after local constants have been declared. */
    return_val += semcheck_const_decls_imported_filtered(symtab, const_decls, 0);
    return return_val;
}

/* Adds built-in functions */
/*TODO: these should be defined in pascal not in semantic analyzer */
static void add_builtin_type_owned(SymTab_t *symtab, const char *name, KgpcType *type)
{
    if (symtab == NULL || name == NULL || type == NULL)
        return;
    if (type->type_alias != NULL && type->type_alias->target_type_id == NULL)
        type->type_alias->target_type_id = strdup(name);
    AddBuiltinType_Typed(symtab, (char *)name, type);
    destroy_kgpc_type(type);  /* Release creator's ref; hash table retained its own */
}

static void add_builtin_alias_type(SymTab_t *symtab, const char *name, int base_type,
    int storage_size)
{
    if (symtab == NULL || name == NULL)
        return;

    struct TypeAlias alias = {0};
    alias.alias_name = (char *)name;  /* Will be duplicated by copy_type_alias */
    alias.base_type = base_type;
    alias.storage_size = storage_size;

    KgpcType *type = create_primitive_type(base_type);
    if (type == NULL)
    {
        return;
    }
    kgpc_type_set_type_alias(type, &alias);
    AddBuiltinType_Typed(symtab, (char *)name, type);
    destroy_kgpc_type(type);
}

static void add_builtin_from_vartype(SymTab_t *symtab, const char *name, enum VarType vt)
{
    KgpcType *t = kgpc_type_from_var_type(vt);
    assert(t != NULL && "Failed to create builtin type");
    add_builtin_type_owned(symtab, name, t);
}

/* Add a string type with explicit type alias to preserve type identity for mangling */
static void add_builtin_string_type_with_alias(SymTab_t *symtab, const char *name,
    enum VarType vt, int is_wide_string)
{
    KgpcType *t = kgpc_type_from_var_type(vt);
    assert(t != NULL && "Failed to create builtin type");
    
    /* Create and attach a type alias with the specific type name */
    struct TypeAlias alias = {0};
    alias.base_type = STRING_TYPE;
    alias.target_type_id = (char *)name;  /* Will be duplicated by copy_type_alias */
    alias.alias_name = (char *)name;      /* Will be duplicated by copy_type_alias */
    alias.is_wide_string = is_wide_string;
    kgpc_type_set_type_alias(t, &alias);
    
    add_builtin_type_owned(symtab, name, t);
}

static void add_builtin_shortstring_type(SymTab_t *symtab, const char *name)
{
    if (symtab == NULL || name == NULL)
        return;

    KgpcType *t = create_primitive_type(SHORTSTRING_TYPE);
    if (t == NULL)
        return;

    struct TypeAlias alias = {0};
    alias.base_type = SHORTSTRING_TYPE;
    alias.target_type_id = (char *)name;  /* Will be duplicated by copy_type_alias */
    alias.alias_name = (char *)name;      /* Will be duplicated by copy_type_alias */
    alias.is_shortstring = 1;
    kgpc_type_set_type_alias(t, &alias);

    add_builtin_type_owned(symtab, name, t);
}

static int semcheck_has_symbol(SymTab_t *symtab, const char *name)
{
    HashNode_t *node = NULL;
    if (symtab == NULL || name == NULL)
        return 0;
    return (FindSymbol(&node, symtab, (char *)name) != 0 && node != NULL);
}

static void ensure_builtin_char_const_if_missing(SymTab_t *symtab, const char *name, int value)
{
    if (!semcheck_has_symbol(symtab, name))
        AddBuiltinCharConst(symtab, name, value);
}

static void ensure_builtin_int_const_if_missing(SymTab_t *symtab, const char *name, long long value)
{
    if (!semcheck_has_symbol(symtab, name))
        AddBuiltinIntConst(symtab, name, value);
}

static void ensure_builtin_alias_type_if_missing(SymTab_t *symtab, const char *name, int base_type, int storage_size)
{
    if (!semcheck_has_symbol(symtab, name))
        add_builtin_alias_type(symtab, name, base_type, storage_size);
}

static void register_simple_builtin_proc(SymTab_t *symtab, const char *name)
{
    assert(name != NULL && "register_simple_builtin_proc: name must not be NULL");
    char *dup_name = strdup(name);
    assert(dup_name != NULL && "register_simple_builtin_proc: strdup failed");
    KgpcType *proc_type = create_procedure_type(NULL, NULL);
    assert(proc_type != NULL && "register_simple_builtin_proc: failed to create procedure type");
    AddBuiltinProc_Typed(symtab, dup_name, proc_type);
    destroy_kgpc_type(proc_type);
    free(dup_name);
}

/* Register a simple built-in function (no parameters) with the given return
 * type tag (e.g. LONGINT_TYPE, CHAR_TYPE, POINTER_TYPE). */
static void register_simple_builtin_func(SymTab_t *symtab, const char *name,
    int return_type_tag)
{
    assert(name != NULL && "register_simple_builtin_func: name must not be NULL");
    char *dup_name = strdup(name);
    assert(dup_name != NULL && "register_simple_builtin_func: strdup failed");
    KgpcType *return_type = create_primitive_type(return_type_tag);
    assert(return_type != NULL && "register_simple_builtin_func: failed to create return type");
    KgpcType *func_type = create_procedure_type(NULL, return_type);
    assert(func_type != NULL && "register_simple_builtin_func: failed to create function type");
    AddBuiltinFunction_Typed(symtab, dup_name, func_type);
    destroy_kgpc_type(func_type);
    destroy_kgpc_type(return_type);
    free(dup_name);
}

/* Register an overloaded unary built-in function: name(param_name: arg_type): ret_type.
 * Caller may invoke this multiple times with different type tags to register overloads. */
static void register_unary_builtin_func(SymTab_t *symtab, const char *name,
    const char *param_name, int arg_type_tag, int return_type_tag)
{
    assert(name != NULL && param_name != NULL);
    char *dup_name = strdup(name);
    assert(dup_name != NULL && "register_unary_builtin_func: strdup failed");
    ListNode_t *param = semcheck_create_builtin_param(param_name, arg_type_tag);
    KgpcType *return_type = create_primitive_type(return_type_tag);
    KgpcType *func_type = create_procedure_type(param, return_type);
    if (func_type != NULL)
    {
        AddBuiltinFunction_Typed(symtab, dup_name, func_type);
        destroy_kgpc_type(func_type);
    }
    if (param != NULL)
        DestroyList(param);
    destroy_kgpc_type(return_type);
    free(dup_name);
}

/* Register an overloaded binary built-in function:
 * name(p1_name: p1_type; p2_name: p2_type): ret_type.
 * Caller may invoke multiple times with different types for overloads. */
__attribute__((unused)) static void register_binary_builtin_func(SymTab_t *symtab, const char *name,
    const char *p1_name, int p1_type_tag,
    const char *p2_name, int p2_type_tag,
    int return_type_tag)
{
    assert(name != NULL && p1_name != NULL && p2_name != NULL);
    char *dup_name = strdup(name);
    assert(dup_name != NULL && "register_binary_builtin_func: strdup failed");
    ListNode_t *p1 = semcheck_create_builtin_param(p1_name, p1_type_tag);
    ListNode_t *p2 = semcheck_create_builtin_param(p2_name, p2_type_tag);
    ListNode_t *params = ConcatList(p1, p2);
    KgpcType *return_type = create_primitive_type(return_type_tag);
    KgpcType *func_type = create_procedure_type(params, return_type);
    if (func_type != NULL)
    {
        AddBuiltinFunction_Typed(symtab, dup_name, func_type);
        destroy_kgpc_type(func_type);
    }
    if (params != NULL)
        DestroyList(params);
    destroy_kgpc_type(return_type);
    free(dup_name);
}

void semcheck_add_builtins(SymTab_t *symtab)
{

    if (!stdlib_loaded_flag())
    {
        /* Platform newline constants to support System/ObjPas resourcestring concatenations */
        AddBuiltinStringConst(symtab, "LineEnding", "\n");
        AddBuiltinStringConst(symtab, "sLineBreak", "\n");
        AddBuiltinCharConst(symtab, "DirectorySeparator", '/');
        AddBuiltinCharConst(symtab, "DriveSeparator", 0);
        AddBuiltinCharConst(symtab, "PathSeparator", ':');
        AddBuiltinCharConst(symtab, "ExtensionSeparator", '.');
        AddBuiltinIntConst(symtab, "MaxPathLen", 4096);

        /* Unix/Linux baseline limits needed by unix.pp aliases (UT.*) */
        AddBuiltinIntConst(symtab, "ARG_MAX", 131072);
        AddBuiltinIntConst(symtab, "NAME_MAX", 255);
        AddBuiltinIntConst(symtab, "PATH_MAX", 4095);
        AddBuiltinIntConst(symtab, "SYS_NMLN", 65);
        AddBuiltinIntConst(symtab, "SIG_MAXSIG", 128);
        AddBuiltinIntConst(symtab, "PRIO_PROCESS", 0);
        AddBuiltinIntConst(symtab, "PRIO_PGRP", 1);
        AddBuiltinIntConst(symtab, "PRIO_USER", 2);
        AddBuiltinIntConst(symtab, "UTSNAME_LENGTH", 65);
        AddBuiltinIntConst(symtab, "fmClosed", 0xD7B0);
        AddBuiltinIntConst(symtab, "fmInput", 0xD7B1);

        /* Sysutils signal helpers (interface declarations may be skipped during parsing). */
        AddBuiltinIntConst(symtab, "RTL_SIGINT", 0);
        AddBuiltinIntConst(symtab, "RTL_SIGFPE", 1);
        AddBuiltinIntConst(symtab, "RTL_SIGSEGV", 2);
        AddBuiltinIntConst(symtab, "RTL_SIGILL", 3);
        AddBuiltinIntConst(symtab, "RTL_SIGBUS", 4);
        AddBuiltinIntConst(symtab, "RTL_SIGQUIT", 5);
        AddBuiltinIntConst(symtab, "RTL_SIGLAST", 5);
        AddBuiltinIntConst(symtab, "RTL_SIGDEFAULT", -1);
        AddBuiltinIntConst(symtab, "ssNotHooked", 0);
        AddBuiltinIntConst(symtab, "ssHooked", 1);
        AddBuiltinIntConst(symtab, "ssOverridden", 2);
    }

    /* Bootstrap stability guard:
     * Some stdlib unit compile paths (notably RTL bootstrap units) may reference
     * these core symbols before imported declarations are visible in the current
     * semcheck pass. Seed only what is missing; never overwrite parsed symbols. */
    ensure_builtin_char_const_if_missing(symtab, "DirectorySeparator", '/');
    ensure_builtin_char_const_if_missing(symtab, "DriveSeparator", 0);
    ensure_builtin_char_const_if_missing(symtab, "PathSeparator", ':');
    ensure_builtin_char_const_if_missing(symtab, "ExtensionSeparator", '.');
    ensure_builtin_int_const_if_missing(symtab, "MaxPathLen", 4096);
    ensure_builtin_int_const_if_missing(symtab, "reRangeError", 4);
    ensure_builtin_int_const_if_missing(symtab, "reInvalidCast", 10);
    ensure_builtin_alias_type_if_missing(symtab, "RTLString", STRING_TYPE, 0);
    
    /* Integer boundary constants - FPC compiler-provided constants.
     * These must be in the builtins table so system.pp can reference them
     * during const evaluation (e.g. MaxInt = maxsmallint). */
    ensure_builtin_int_const_if_missing(symtab, "MaxInt", 32767LL);
    ensure_builtin_int_const_if_missing(symtab, "MaxLongint", 2147483647LL);
    ensure_builtin_int_const_if_missing(symtab, "MaxSmallint", 32767LL);
    ensure_builtin_int_const_if_missing(symtab, "maxsmallint", 32767LL);
    {
        char *name;
        name = strdup("DefaultSystemCodePage");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 65001LL);
            free(name);
        }
        name = strdup("DefaultFileSystemCodePage");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 65001LL);
            free(name);
        }
        /* MaxShortint: Maximum value for ShortInt (8-bit signed) = 2^7 - 1 */
        name = strdup("MaxShortint");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 127LL);
            free(name);
        }
        /* MaxInt64: Maximum value for Int64 (64-bit signed) = 2^63 - 1 */
        name = strdup("MaxInt64");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 9223372036854775807LL);
            free(name);
        }
    }
    
    {
        KgpcType *char_type = create_primitive_type(CHAR_TYPE);
        KgpcType *pchar = create_pointer_type(char_type);
        kgpc_type_release(char_type);  /* pchar retained its own ref */
        KgpcType *ppchar = create_pointer_type(pchar);
        kgpc_type_release(pchar);  /* ppchar retained its own ref */
        if (ppchar != NULL)
        {
            char *envp_name = strdup("EnvP");
            if (envp_name != NULL) {
                PushVarOntoScope_Typed(symtab, envp_name, ppchar);
                free(envp_name);
            }
            char *envp_lower = strdup("envp");
            if (envp_lower != NULL) {
                PushVarOntoScope_Typed(symtab, envp_lower, ppchar);
                free(envp_lower);
            }
            destroy_kgpc_type(ppchar);
        }
    }
    
    /* Primitive core types required to semcheck system.p and user code.
     * Everything else should live in KGPC/Units/system.p (aliases, pointer helpers, etc.). */
    /* Integer is defined by system.p as "Integer = LongInt" (32-bit in objfpc mode).
     * In FPC RTL mode, system.pp defines "Integer = SmallInt" (16-bit) and then
     * objpas.pp overrides it to "Integer = LongInt" via the alias override logic.
     * We do NOT register Integer as a builtin to avoid conflicting with system.p. */
    add_builtin_from_vartype(symtab, "LongInt", HASHVAR_LONGINT);
    add_builtin_type_owned(symtab, "Int64", create_primitive_type_with_size(INT64_TYPE, 8));
    if (!stdlib_loaded_flag())
        add_builtin_alias_type(symtab, "Currency", INT64_TYPE, 8);
    add_builtin_from_vartype(symtab, "Real", HASHVAR_REAL);
    add_builtin_from_vartype(symtab, "Boolean", HASHVAR_BOOLEAN);
    /* FPC-compatible extended boolean types */
    add_builtin_alias_type(symtab, "Boolean8", BOOL, 1);
    add_builtin_alias_type(symtab, "Boolean16", BOOL, 2);
    add_builtin_alias_type(symtab, "Boolean32", BOOL, 4);
    add_builtin_alias_type(symtab, "Boolean64", BOOL, 8);
    /* FPC-compatible byte/word/long boolean types (synonyms) */
    add_builtin_alias_type(symtab, "ByteBool", BOOL, 1);
    add_builtin_alias_type(symtab, "WordBool", BOOL, 2);
    add_builtin_alias_type(symtab, "LongBool", BOOL, 4);
    add_builtin_alias_type(symtab, "QWordBool", BOOL, 8);
    if (!stdlib_loaded_flag())
    {
        add_builtin_alias_type(symtab, "TSignalState", INT_TYPE, 4);
    }
    add_builtin_from_vartype(symtab, "Char", HASHVAR_CHAR);
    add_builtin_from_vartype(symtab, "AnsiChar", HASHVAR_CHAR);
    add_builtin_type_owned(symtab, "WideChar", create_primitive_type_with_size(CHAR_TYPE, 2));
    add_builtin_from_vartype(symtab, "String", HASHVAR_PCHAR);
    add_builtin_from_vartype(symtab, "OpenString", HASHVAR_PCHAR);
    add_builtin_string_type_with_alias(symtab, "AnsiString", HASHVAR_PCHAR, 0);
    add_builtin_shortstring_type(symtab, "ShortString");
    add_builtin_string_type_with_alias(symtab, "RawByteString", HASHVAR_PCHAR, 0);
    add_builtin_string_type_with_alias(symtab, "UnicodeString", HASHVAR_PCHAR, 1);
    add_builtin_string_type_with_alias(symtab, "WideString", HASHVAR_PCHAR, 1);
    if (!stdlib_loaded_flag())
    {
        KgpcType *str_inner = create_primitive_type(STRING_TYPE);
        KgpcType *pansistring = create_pointer_type(str_inner);
        kgpc_type_release(str_inner);
        add_builtin_type_owned(symtab, "PAnsiString", pansistring);

        str_inner = create_primitive_type(STRING_TYPE);
        KgpcType *pstring = create_pointer_type(str_inner);
        kgpc_type_release(str_inner);
        add_builtin_type_owned(symtab, "PString", pstring);
    }
    {
        KgpcType *char_inner = create_primitive_type(CHAR_TYPE);
        KgpcType *pansichar = create_pointer_type(char_inner);
        kgpc_type_release(char_inner);
        add_builtin_type_owned(symtab, "PAnsiChar", pansichar);
    }

    /* Primitive pointer type */
    add_builtin_type_owned(symtab, "Pointer", create_primitive_type(POINTER_TYPE));
    /* Common ordinal aliases (match KGPC system.p sizes) */
    add_builtin_type_owned(symtab, "Byte", create_primitive_type_with_size(BYTE_TYPE, 1));
    add_builtin_type_owned(symtab, "ShortInt", create_primitive_type_with_size(INT_TYPE, 1));
    add_builtin_type_owned(symtab, "SmallInt", create_primitive_type_with_size(INT_TYPE, 2));
    add_builtin_type_owned(symtab, "Word", create_primitive_type_with_size(WORD_TYPE, 2));
    add_builtin_type_owned(symtab, "LongWord", create_primitive_type_with_size(LONGWORD_TYPE, 4));
    add_builtin_type_owned(symtab, "Cardinal", create_primitive_type_with_size(LONGWORD_TYPE, 4));
    add_builtin_type_owned(symtab, "DWord", create_primitive_type_with_size(LONGWORD_TYPE, 4));
    add_builtin_type_owned(symtab, "QWord", create_primitive_type_with_size(QWORD_TYPE, 8));
    add_builtin_type_owned(symtab, "UInt64", create_primitive_type_with_size(QWORD_TYPE, 8));
    add_builtin_type_owned(symtab, "Int8", create_primitive_type_with_size(INT_TYPE, 1));
    add_builtin_type_owned(symtab, "UInt8", create_primitive_type_with_size(BYTE_TYPE, 1));
    add_builtin_type_owned(symtab, "Int16", create_primitive_type_with_size(INT_TYPE, 2));
    add_builtin_type_owned(symtab, "UInt16", create_primitive_type_with_size(WORD_TYPE, 2));
    add_builtin_type_owned(symtab, "Int32", create_primitive_type_with_size(INT_TYPE, 4));
    add_builtin_type_owned(symtab, "UInt32", create_primitive_type_with_size(LONGWORD_TYPE, 4));
    add_builtin_type_owned(symtab, "Float", create_primitive_type_with_size(EXTENDED_TYPE, 10));
    add_builtin_type_owned(symtab, "ValReal", create_primitive_type_with_size(REAL_TYPE, 8));
    add_builtin_type_owned(symtab, "Single", create_primitive_type_with_size(REAL_TYPE, 4));
    add_builtin_type_owned(symtab, "Double", create_primitive_type_with_size(REAL_TYPE, 8));
    add_builtin_type_owned(symtab, "Extended", create_primitive_type_with_size(EXTENDED_TYPE, 10));

    /* Variant and OleVariant (COM interop) - treated as opaque 16-byte types.
     * VARIANT_TYPE auto-coerces to/from any value type at the semantic level. */
    add_builtin_type_owned(symtab, "Variant", create_primitive_type_with_size(VARIANT_TYPE, 16));
    add_builtin_type_owned(symtab, "OleVariant", create_primitive_type_with_size(VARIANT_TYPE, 16));

    /* File/Text primitives (sizes align with system.p TextRec/FileRec layout) */
    add_builtin_type_owned(symtab, "file", create_primitive_type_with_size(FILE_TYPE, 368));
    add_builtin_type_owned(symtab, "File", create_primitive_type_with_size(FILE_TYPE, 368));
    add_builtin_type_owned(symtab, "TypedFile", create_primitive_type_with_size(FILE_TYPE, 368));
    add_builtin_type_owned(symtab, "text", create_primitive_type_with_size(TEXT_TYPE, 640));
    add_builtin_type_owned(symtab, "Text", create_primitive_type_with_size(TEXT_TYPE, 640));

    /* Builtin procedures — only true compiler intrinsics that need special
     * handling (variable args, type-dependent behavior, compile-time magic).
     * Regular procedures belong in system.p, not here. */
    {
        static const char *simple_procs[] = {
            "SetLength", "SetString", "write", "writeln", "writestr",
            "read", "readln",
            "Val", "Str", "Insert", "Delete", "Inc", "Dec",
            "Include", "Exclude", "New", "Dispose", "Assert"
        };
        for (size_t i = 0; i < sizeof(simple_procs) / sizeof(simple_procs[0]); ++i)
            register_simple_builtin_proc(symtab, simple_procs[i]);
    }

    /* Builtin functions — only true compiler intrinsics */
    {
        typedef struct { const char *name; int return_tag; } SimpleBuiltinFunc;
        static const SimpleBuiltinFunc simple_funcs[] = {
            {"Length",       LONGINT_TYPE},
            {"Copy",         STRING_TYPE},
            {"Concat",       STRING_TYPE},
            {"EOF",          BOOL},
            {"EOLN",         BOOL},
            {"SizeOf",       LONGINT_TYPE},
            {"Chr",          CHAR_TYPE},
            {"Ord",          LONGINT_TYPE},
            {"High",         LONGINT_TYPE},
        };
        for (size_t i = 0; i < sizeof(simple_funcs) / sizeof(simple_funcs[0]); ++i)
            register_simple_builtin_func(symtab, simple_funcs[i].name, simple_funcs[i].return_tag);
    }
    /* Standard I/O file variables - stdin, stdout, stderr */
    /* These are Text file variables that can be passed to Write/WriteLn/Read/ReadLn */
    {
        char *stdin_name = strdup("stdin");
        if (stdin_name != NULL) {
            KgpcType *stdin_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(stdin_type != NULL && "Failed to create stdin type");
            PushVarOntoScope_Typed(symtab, stdin_name, stdin_type);
            destroy_kgpc_type(stdin_type);
            free(stdin_name);
        }
        char *stdout_name = strdup("stdout");
        if (stdout_name != NULL) {
            KgpcType *stdout_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(stdout_type != NULL && "Failed to create stdout type");
            PushVarOntoScope_Typed(symtab, stdout_name, stdout_type);
            destroy_kgpc_type(stdout_type);
            free(stdout_name);
        }
        char *stderr_name = strdup("stderr");
        if (stderr_name != NULL) {
            KgpcType *stderr_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(stderr_type != NULL && "Failed to create stderr type");
            PushVarOntoScope_Typed(symtab, stderr_name, stderr_type);
            destroy_kgpc_type(stderr_type);
            free(stderr_name);
        }
        /* Input and Output - standard Pascal file variables */
        int sys_unit_idx = unit_registry_add("System");
        char *input_name = strdup("Input");
        if (input_name != NULL) {
            KgpcType *input_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(input_type != NULL && "Failed to create Input type");
            PushVarOntoScope_Typed(symtab, input_name, input_type);
            destroy_kgpc_type(input_type);
            HashNode_t *input_node = NULL;
            if (FindSymbol(&input_node, symtab, "Input") != 0 && input_node != NULL) {
                input_node->defined_in_unit = 1;
                input_node->unit_is_public = 1;
                mark_hashnode_source_unit(input_node, sys_unit_idx);
            }
            free(input_name);
        }
        char *output_name = strdup("Output");
        if (output_name != NULL) {
            KgpcType *output_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(output_type != NULL && "Failed to create Output type");
            PushVarOntoScope_Typed(symtab, output_name, output_type);
            destroy_kgpc_type(output_type);
            HashNode_t *output_node = NULL;
            if (FindSymbol(&output_node, symtab, "Output") != 0 && output_node != NULL) {
                output_node->defined_in_unit = 1;
                output_node->unit_is_public = 1;
                mark_hashnode_source_unit(output_node, sys_unit_idx);
            }
            free(output_name);
        }
    }

    /* StringOfChar: function StringOfChar(c: Char; l: SizeInt): string
     *                       StringOfChar(c: WideChar; l: SizeInt): UnicodeString */
    {
        const char *func_name = "StringOfChar";
        KgpcType *ansi_return_type = create_primitive_type(STRING_TYPE);
        KgpcType *wide_return_type = create_primitive_type(STRING_TYPE);
        struct TypeAlias *wide_return_alias = calloc(1, sizeof(struct TypeAlias));
        if (wide_return_type != NULL && wide_return_alias != NULL)
        {
            wide_return_alias->alias_name = strdup("UnicodeString");
            wide_return_alias->target_type_id = strdup("UnicodeString");
            wide_return_alias->base_type = STRING_TYPE;
            wide_return_alias->is_wide_string = 1;
            kgpc_type_set_type_alias(wide_return_type, wide_return_alias);
            /* kgpc_type_set_type_alias copies the alias; free the original */
            free(wide_return_alias->alias_name);
            free(wide_return_alias->target_type_id);
            free(wide_return_alias);
        }
        else if (wide_return_alias != NULL)
        {
            free(wide_return_alias);
        }

        /* Char + LongInt */
        ListNode_t *param_c = semcheck_create_builtin_param("c", CHAR_TYPE);
        ListNode_t *param_l = semcheck_create_builtin_param("l", LONGINT_TYPE);
        ListNode_t *params = ConcatList(param_c, param_l);
        KgpcType *func_type = create_procedure_type(params, ansi_return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);

        /* Char + Int64 (SizeInt on 64-bit) */
        param_c = semcheck_create_builtin_param("c", CHAR_TYPE);
        param_l = semcheck_create_builtin_param("l", INT64_TYPE);
        params = ConcatList(param_c, param_l);
        func_type = create_procedure_type(params, ansi_return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);

        /* WideChar (Word) + LongInt */
        param_c = semcheck_create_builtin_param("c", WORD_TYPE);
        param_l = semcheck_create_builtin_param("l", LONGINT_TYPE);
        params = ConcatList(param_c, param_l);
        func_type = create_procedure_type(params, wide_return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);

        /* WideChar (Word) + Int64 (SizeInt on 64-bit) */
        param_c = semcheck_create_builtin_param("c", WORD_TYPE);
        param_l = semcheck_create_builtin_param("l", INT64_TYPE);
        params = ConcatList(param_c, param_l);
        func_type = create_procedure_type(params, wide_return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);

        destroy_kgpc_type(ansi_return_type);
        destroy_kgpc_type(wide_return_type);
    }

    /* BinStr: function BinStr(Val: int64; cnt: byte): shortstring */
    {
        const char *func_name = "BinStr";
        ListNode_t *param_val = semcheck_create_builtin_param("Val", INT64_TYPE);
        ListNode_t *param_cnt = semcheck_create_builtin_param("cnt", BYTE_TYPE);
        ListNode_t *params = ConcatList(param_val, param_cnt);
        KgpcType *return_type = create_primitive_type(SHORTSTRING_TYPE);
        KgpcType *func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);
        destroy_kgpc_type(return_type);
    }

    /* PopCnt: function PopCnt(AValue: QWord): Byte - counts set bits */
    {
        static const int popcnt_types[] = {BYTE_TYPE, WORD_TYPE, INT_TYPE, LONGINT_TYPE, LONGWORD_TYPE, QWORD_TYPE};
        for (size_t i = 0; i < sizeof(popcnt_types) / sizeof(popcnt_types[0]); ++i)
            register_unary_builtin_func(symtab, "PopCnt", "AValue", popcnt_types[i], BYTE_TYPE);
    }

    /* IndexChar: function IndexChar(const buf; len: SizeInt; b: Char): SizeInt */
    {
        const char *func_name = "IndexChar";
        ListNode_t *param_buf = semcheck_create_builtin_param("buf", UNKNOWN_TYPE);
        ListNode_t *param_len = semcheck_create_builtin_param("len", LONGINT_TYPE);
        ListNode_t *param_b = semcheck_create_builtin_param("b", CHAR_TYPE);
        ListNode_t *params = ConcatList(ConcatList(param_buf, param_len), param_b);
        KgpcType *return_type = create_primitive_type(LONGINT_TYPE);
        KgpcType *func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);
        destroy_kgpc_type(return_type);
    }

    /* CompareByte: function CompareByte(const buf1, buf2; len: SizeInt): SizeInt */
    {
        const char *func_name = "CompareByte";
        ListNode_t *param_buf1 = semcheck_create_builtin_param("buf1", UNKNOWN_TYPE);
        ListNode_t *param_buf2 = semcheck_create_builtin_param("buf2", UNKNOWN_TYPE);
        ListNode_t *param_len = semcheck_create_builtin_param("len", LONGINT_TYPE);
        ListNode_t *params = ConcatList(ConcatList(param_buf1, param_buf2), param_len);
        KgpcType *return_type = create_primitive_type(LONGINT_TYPE);
        KgpcType *func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);
        destroy_kgpc_type(return_type);
    }

    /* UniqueString: procedure UniqueString(var S: String) */
    {
        const char *func_name = "UniqueString";
        ListNode_t *param_s = semcheck_create_builtin_param_var("S", STRING_TYPE);
        KgpcType *func_type = create_procedure_type(param_s, NULL);
        if (func_type != NULL)
        {
            AddBuiltinProc_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (param_s != NULL)
            DestroyList(param_s);
    }

    /* Bit scan builtins (FPC compatibility) */
    {
        struct {
            const char *name;
            int param_tag;
        } bsr_bsf[] = {
            {"BsrByte", BYTE_TYPE}, {"BsfByte", BYTE_TYPE},
            {"BsrWord", WORD_TYPE}, {"BsfWord", WORD_TYPE},
            {"BsrDWord", LONGWORD_TYPE}, {"BsfDWord", LONGWORD_TYPE},
            {"BsrQWord", QWORD_TYPE}, {"BsfQWord", QWORD_TYPE},
        };

        for (size_t i = 0; i < sizeof(bsr_bsf) / sizeof(bsr_bsf[0]); i++)
        {
            ListNode_t *param = semcheck_create_builtin_param("AValue", bsr_bsf[i].param_tag);
            KgpcType *return_type = create_primitive_type(LONGINT_TYPE);
            KgpcType *func_type = create_procedure_type(param, return_type);
            if (func_type != NULL)
            {
                AddBuiltinFunction_Typed(symtab, (char *)bsr_bsf[i].name, func_type);
                destroy_kgpc_type(func_type);
            }
            if (param != NULL)
                DestroyList(param);
            destroy_kgpc_type(return_type);
        }
    }

    /* FPC compiler intrinsics for stack frame access.
     * These return Pointer and are used by error handling code.
     * Register both zero-arg and one-arg (Pointer) overloads to match FPC semantics.
     * Also register kgpc_get_frame (the runtime stub these get rewritten to). */
    {
        const char *frame_intrinsics[] = {
            "get_frame", "get_pc_addr", "get_caller_addr", "get_caller_frame",
            "Get_Frame", "Get_Caller_Addr", "Get_Caller_Frame",
            "kgpc_get_frame",
        };
        for (size_t i = 0; i < sizeof(frame_intrinsics) / sizeof(frame_intrinsics[0]); i++)
        {
            /* Zero-argument overload: get_frame() -> Pointer */
            {
                KgpcType *return_type = create_primitive_type(POINTER_TYPE);
                KgpcType *func_type = create_procedure_type(NULL, return_type);
                if (func_type != NULL)
                {
                    AddBuiltinFunction_Typed(symtab, (char *)frame_intrinsics[i], func_type);
                    destroy_kgpc_type(func_type);
                }
                destroy_kgpc_type(return_type);
            }
            /* One-argument overload: get_caller_addr(frame: Pointer) -> Pointer */
            {
                ListNode_t *param = semcheck_create_builtin_param("p", POINTER_TYPE);
                KgpcType *return_type = create_primitive_type(POINTER_TYPE);
                KgpcType *func_type = create_procedure_type(param, return_type);
                if (func_type != NULL)
                {
                    AddBuiltinFunction_Typed(symtab, (char *)frame_intrinsics[i], func_type);
                    destroy_kgpc_type(func_type);
                }
                if (param != NULL)
                    DestroyList(param);
                destroy_kgpc_type(return_type);
            }
        }
    }

    /* Builtins are now in system.p */

    /* Atomic operations: AtomicCmpExchange, AtomicExchange, AtomicIncrement, AtomicDecrement.
     * In FPC these are compiler intrinsics that work on any ordinal/pointer type.
     * We register them with Integer parameters as a common overload. */
    {
        /* AtomicCmpExchange(var Target: T; NewValue: T; Comparand: T): T */
        {
            static const int cmpxchg_types[] = {INT_TYPE, LONGINT_TYPE, INT64_TYPE, POINTER_TYPE};
            for (size_t i = 0; i < sizeof(cmpxchg_types) / sizeof(cmpxchg_types[0]); ++i)
            {
                int t = cmpxchg_types[i];
                ListNode_t *p1 = semcheck_create_builtin_param_var("Target", t);
                ListNode_t *p2 = semcheck_create_builtin_param("NewValue", t);
                ListNode_t *p3 = semcheck_create_builtin_param("Comparand", t);
                p1->next = p2;
                p2->next = p3;
                KgpcType *return_type = create_primitive_type(t);
                KgpcType *func_type = create_procedure_type(p1, return_type);
                if (func_type != NULL)
                {
                    char *n1 = strdup("AtomicCmpExchange");
                    AddBuiltinFunction_Typed(symtab, n1, func_type);
                    /* InterlockedCompareExchange is defined in system.pp
                     * (generic.inc) — do NOT register it as a builtin. */
                    destroy_kgpc_type(func_type);
                    free(n1);
                }
                DestroyList(p1);
                destroy_kgpc_type(return_type);
            }
        }
        /* AtomicExchange(var Target: T; Value: T): T
         * Target is a var parameter — must be passed by reference. */
        {
            static const int xchg_types[] = {INT_TYPE, LONGINT_TYPE, INT64_TYPE, POINTER_TYPE};
            for (size_t i = 0; i < sizeof(xchg_types) / sizeof(xchg_types[0]); ++i)
            {
                int t = xchg_types[i];
                ListNode_t *p1 = semcheck_create_builtin_param_var("Target", t);
                ListNode_t *p2 = semcheck_create_builtin_param("Value", t);
                p1->next = p2;
                KgpcType *return_type = create_primitive_type(t);
                KgpcType *func_type = create_procedure_type(p1, return_type);
                if (func_type != NULL)
                {
                    char *n1 = strdup("AtomicExchange");
                    AddBuiltinFunction_Typed(symtab, n1, func_type);
                    /* InterlockedExchange is defined in system.pp
                     * (generic.inc) — do NOT register it as a builtin. */
                    destroy_kgpc_type(func_type);
                    free(n1);
                }
                DestroyList(p1);
                destroy_kgpc_type(return_type);
            }
        }
        /* AtomicIncrement/AtomicDecrement(var Target: T[; Value: T]): T
         * Target is a var parameter — must be passed by reference.
         * In FPC these are compiler intrinsics that work on any ordinal type.
         * Register overloads for LongInt, Int64, Integer, and QWord. */
        {
            /* InterlockedIncrement/InterlockedDecrement are defined in
             * system.pp (generic.inc) — only register Atomic* builtins. */
            const char *names[] = {
                "AtomicIncrement", "AtomicDecrement",
            };
            static const int atomic_types[] = {LONGINT_TYPE, INT64_TYPE, INT_TYPE, QWORD_TYPE};
            for (size_t i = 0; i < sizeof(names) / sizeof(names[0]); i++)
            {
                for (size_t ti = 0; ti < sizeof(atomic_types) / sizeof(atomic_types[0]); ti++)
                {
                    int t = atomic_types[ti];
                    /* One-arg overload: name(var Target: T): T */
                    {
                        char *dup_name = strdup(names[i]);
                        ListNode_t *p1 = semcheck_create_builtin_param_var("Target", t);
                        KgpcType *return_type = create_primitive_type(t);
                        KgpcType *func_type = create_procedure_type(p1, return_type);
                        if (func_type != NULL)
                        {
                            AddBuiltinFunction_Typed(symtab, dup_name, func_type);
                            destroy_kgpc_type(func_type);
                        }
                        if (p1 != NULL)
                            DestroyList(p1);
                        destroy_kgpc_type(return_type);
                        free(dup_name);
                    }
                    /* Two-arg overload: name(var Target: T; Value: T): T */
                    {
                        char *dup_name = strdup(names[i]);
                        ListNode_t *p1 = semcheck_create_builtin_param_var("Target", t);
                        ListNode_t *p2 = semcheck_create_builtin_param("Value", t);
                        p1->next = p2;
                        KgpcType *return_type = create_primitive_type(t);
                        KgpcType *func_type = create_procedure_type(p1, return_type);
                        if (func_type != NULL)
                        {
                            AddBuiltinFunction_Typed(symtab, dup_name, func_type);
                            destroy_kgpc_type(func_type);
                        }
                        DestroyList(p1);
                        destroy_kgpc_type(return_type);
                        free(dup_name);
                    }
                }
            }
        }
        /* bitsizeof(T): Integer - returns size in bits */
        {
            register_unary_builtin_func(symtab, "bitsizeof", "x", INT_TYPE, INT_TYPE);
            register_unary_builtin_func(symtab, "BitSizeOf", "x", INT_TYPE, INT_TYPE);
        }
        /* Finalize(var v): frees managed resources.
         * POINTER_TYPE is used as a placeholder parameter type; actual type
         * validation is bypassed in the builtin handler which accepts any type. */
        {
            ListNode_t *p1 = semcheck_create_builtin_param("v", POINTER_TYPE);
            KgpcType *proc_type = create_procedure_type(p1, NULL);
            if (proc_type != NULL)
            {
                AddBuiltinProc_Typed(symtab, "Finalize", proc_type);
                destroy_kgpc_type(proc_type);
            }
            DestroyList(p1);
        }
        /* Initialize(var v): initializes managed resources.
         * POINTER_TYPE is used as a placeholder parameter type; actual type
         * validation is bypassed in the builtin handler which accepts any type. */
        {
            ListNode_t *p1 = semcheck_create_builtin_param("v", POINTER_TYPE);
            KgpcType *proc_type = create_procedure_type(p1, NULL);
            if (proc_type != NULL)
            {
                AddBuiltinProc_Typed(symtab, "Initialize", proc_type);
                destroy_kgpc_type(proc_type);
            }
            DestroyList(p1);
        }
    }
}

/* Semantic check for a program */
#define SEMCHECK_TIMINGS_ENABLED() (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL)

