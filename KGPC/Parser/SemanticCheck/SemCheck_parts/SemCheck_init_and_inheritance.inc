SymTab_t *semcheck_init_symtab(void)
{
    double t0 = 0.0;
    SymTab_t *symtab = InitSymTab();
    EnterScope(symtab, 0);  /* Global pre-program scope; builtins live in
                             * unit_scopes[System]->table, not in this scope. */
    if (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL)
        t0 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
    semcheck_add_builtins(symtab);
    if (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL) {
        double t1 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
        fprintf(stderr, "[timing] semcheck_add_builtins: %.2f ms\n", t1 - t0);
    }
    return symtab;
}

/* Wire unit scope dependencies from the unit_registry into the scope tree.
 * Each unit scope gets dep_scopes[] populated from its uses clause,
 * so FindIdent_Tree can search dependencies without the flat-model fallback.
 * Extracted from start_semcheck() so it can be called after all units are loaded
 * but before the final program-level semcheck. */
void wire_all_unit_scope_deps(SymTab_t *symtab)
{
    int num_units = unit_registry_count();
    for (int u = 1; u <= num_units; u++)
    {
        ScopeNode *unit_scope = GetOrCreateUnitScope(symtab, u);
        if (unit_scope == NULL) continue;
        /* Always add System as a dep (every unit implicitly uses System) */
        int sys_idx = unit_registry_add("System");
        if (sys_idx > 0 && sys_idx != u)
        {
            ScopeNode *sys_scope = GetOrCreateUnitScope(symtab, sys_idx);
            if (sys_scope != NULL)
                ScopeAddDependencyTransitive(unit_scope, sys_scope, 1);
        }
        /* Add explicit deps from uses clause */
        for (int d = 1; d <= num_units; d++)
        {
            if (d == u) continue;
            if (unit_registry_is_iface_dep(u, d))
            {
                ScopeNode *dep_scope = GetOrCreateUnitScope(symtab, d);
                if (dep_scope != NULL)
                    ScopeAddDependencyTransitive(unit_scope, dep_scope, 1);
            }
            else if (unit_registry_is_dep(u, d))
            {
                ScopeNode *dep_scope = GetOrCreateUnitScope(symtab, d);
                if (dep_scope != NULL)
                    ScopeAddDependencyTransitive(unit_scope, dep_scope, 0);
            }
        }
    }

    /* Second pass: propagate transitive interface deps that were missed
     * because a dependency hadn't been fully wired yet in the first pass.
     * Units are numbered in load order, but uses-chains can reference
     * higher-numbered units.  After pass 1 every unit has its direct
     * deps; pass 2 copies any newly-visible transitive deps. */
    for (int u = 1; u <= num_units; u++)
    {
        ScopeNode *unit_scope = GetOrCreateUnitScope(symtab, u);
        if (unit_scope == NULL) continue;
        for (int d = 1; d <= num_units; d++)
        {
            if (d == u) continue;
            if (unit_registry_is_iface_dep(u, d))
            {
                ScopeNode *dep_scope = GetOrCreateUnitScope(symtab, d);
                if (dep_scope != NULL)
                    ScopeAddDependencyTransitive(unit_scope, dep_scope, 1);
            }
        }
    }

    /* NOTE: We do NOT add unit deps to current_scope (the global pre-program
     * BLOCK scope) here. The program-level scope gets its deps wired separately
     * in semcheck_program / semcheck_unit via the EnterScope(0) call.
     * Adding deps here would cause duplicate symbol searches because
     * any scope with num_deps > 0 now triggers dep search. */
}

SymTab_t *start_semcheck(Tree_t *parse_tree, int *sem_result)
{
    return start_semcheck_with_symtab(NULL, parse_tree, sem_result);
}

SymTab_t *start_semcheck_with_symtab(SymTab_t *existing_symtab, Tree_t *parse_tree, int *sem_result)
{
    SymTab_t *symtab;
    int return_val;
    double t0 = 0.0;

    assert(parse_tree != NULL);
    assert(sem_result != NULL);

    /* Reset warning counter at start of semantic check */
    g_semcheck_warning_count = 0;

    if (g_semcheck_source_path != NULL)
        file_to_parse = g_semcheck_source_path;
    if (g_semcheck_source_buffer != NULL)
    {
        if (preprocessed_source != NULL)
            free(preprocessed_source);
        preprocessed_source = strdup(g_semcheck_source_buffer);
        preprocessed_length = g_semcheck_source_length;
    }

    if (existing_symtab != NULL)
        symtab = existing_symtab;
    else
        symtab = semcheck_init_symtab();

    wire_all_unit_scope_deps(symtab);

    if (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL)
        t0 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
    if (parse_tree->type == TREE_UNIT) {
        return_val = semcheck_unit(symtab, parse_tree);
    } else {
        return_val = semcheck_program(symtab, parse_tree);
    }
    if (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL) {
        double t1 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
        fprintf(stderr, "[timing] semcheck_program/unit: %.2f ms\n", t1 - t0);
    }

    if(return_val > 0)
        fprintf(stderr, "\nCheck failed with %d error(s)!\n\n", return_val);
    else if (g_semcheck_warning_count > 0)
        fprintf(stderr, "\nCheck successful with %d warning(s)!\n\n", g_semcheck_warning_count);
    else
        fprintf(stderr, "\nCheck successful!\n\n");

    /* Return -1 if there were warnings but no errors (per SemCheck.h API) */
    if (return_val == 0 && g_semcheck_warning_count > 0)
        return_val = -1;

    *sem_result = return_val;
    return symtab;
}

/* Register anonymous enum values from record field declarations.
 * For fields like `kind: (vInteger, vString, vNone)`, the enum values
 * must be visible in the enclosing scope. */
static int register_record_field_enum_literals(SymTab_t *symtab, struct RecordType *record)
{
    if (symtab == NULL || record == NULL)
        return 0;

    int errors = 0;
    ListNode_t *field_node = record->fields;
    while (field_node != NULL)
    {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)field_node->cur;
            if (field->enum_literals != NULL)
            {
                KgpcType *enum_type = create_primitive_type(ENUM_TYPE);
                int ordinal = 0;
                ListNode_t *lit = field->enum_literals;
                while (lit != NULL)
                {
                    if (lit->cur != NULL)
                    {
                        char *name = (char *)lit->cur;
                        HashNode_t *existing = NULL;
                        if (FindSymbol(&existing, symtab, name) == 0)
                        {
                            if (PushConstOntoScope_Typed(symtab, name, ordinal, enum_type) > 0)
                                ++errors;
                        }
                    }
                    ++ordinal;
                    lit = lit->next;
                }
                if (enum_type != NULL)
                    kgpc_type_release(enum_type);
            }
        }
        else if (field_node->type == LIST_VARIANT_PART && field_node->cur != NULL)
        {
            struct VariantPart *variant = (struct VariantPart *)field_node->cur;
            /* Check tag field for anonymous enum */
            if (variant->tag_field != NULL && variant->tag_field->enum_literals != NULL)
            {
                KgpcType *enum_type = create_primitive_type(ENUM_TYPE);
                int ordinal = 0;
                ListNode_t *lit = variant->tag_field->enum_literals;
                while (lit != NULL)
                {
                    if (lit->cur != NULL)
                    {
                        char *name = (char *)lit->cur;
                        HashNode_t *existing = NULL;
                        if (FindSymbol(&existing, symtab, name) == 0)
                        {
                            if (PushConstOntoScope_Typed(symtab, name, ordinal, enum_type) > 0)
                                ++errors;
                        }
                    }
                    ++ordinal;
                    lit = lit->next;
                }
                if (enum_type != NULL)
                    kgpc_type_release(enum_type);
            }
            /* Recurse into variant branches */
            ListNode_t *branch_node = variant->branches;
            while (branch_node != NULL)
            {
                if (branch_node->type == LIST_VARIANT_BRANCH && branch_node->cur != NULL)
                {
                    struct VariantBranch *branch = (struct VariantBranch *)branch_node->cur;
                    /* Create a temporary RecordType to recurse */
                    struct RecordType temp_rec;
                    memset(&temp_rec, 0, sizeof(temp_rec));
                    temp_rec.fields = branch->members;
                    errors += register_record_field_enum_literals(symtab, &temp_rec);
                }
                branch_node = branch_node->next;
            }
        }
        field_node = field_node->next;
    }
    return errors;
}

/* Pushes a bunch of type declarations onto the current scope */
static int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls)
{
    if (symtab == NULL)
        return 0;

    int errors = 0;
    ListNode_t *cur = type_decls;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;
            if (tree->type == TREE_TYPE_DECL &&
                tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
            {
                struct TypeAlias *alias_info = &tree->tree_data.type_decl_data.info.alias;
                if (alias_info != NULL && alias_info->is_enum && alias_info->enum_literals != NULL)
                {
                    if (kgpc_getenv("KGPC_DEBUG_ENUM_LITERALS") != NULL)
                    {
                        fprintf(stderr, "[KGPC] enum predeclare %s scoped=%d line=%d\n",
                            tree->tree_data.type_decl_data.id != NULL ? tree->tree_data.type_decl_data.id : "<anon>",
                            alias_info->enum_is_scoped,
                            tree->line_num);
                    }
                    /* Create ONE shared KgpcType for this enum type if not already created */
                    if (alias_info->kgpc_type == NULL)
                    {
                        alias_info->kgpc_type = create_primitive_type(ENUM_TYPE);
                        if (alias_info->kgpc_type == NULL)
                        {
                            fprintf(stderr, "Error: Failed to create enum type for %s\n",
                                    tree->tree_data.type_decl_data.id);
                            ++errors;
                        }
                    }
                    if (alias_info->kgpc_type != NULL &&
                        alias_info->kgpc_type->type_alias == NULL)
                    {
                        kgpc_type_set_type_alias(alias_info->kgpc_type, alias_info);
                    }

                    /* Unit declarations live in their unit scope so subprogram bodies
                     * see them via the real parent/dependency edges. */
                    ScopeNode *saved_scope_for_enum_type = semcheck_switch_to_unit_scope(
                        symtab, tree->tree_data.type_decl_data.source_unit_index);

                    /* Predeclare the enum type itself so consts can reference it
                     * before full type processing (e.g., array[TEnum] in consts). */
                    if (tree->tree_data.type_decl_data.id != NULL)
                    {
                        HashNode_t *existing_type = FindIdentInCurrentScope(symtab,
                            tree->tree_data.type_decl_data.id);
                        if (existing_type == NULL)
                        {
                            PushTypeOntoScope_Typed(symtab,
                                tree->tree_data.type_decl_data.id, alias_info->kgpc_type);
                            HashNode_t *node = NULL;
                            if (FindSymbol(&node, symtab, tree->tree_data.type_decl_data.id) != 0 &&
                                node != NULL)
                            {
                                mark_hashnode_unit_info(symtab, node,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(node,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                        }
                    }

                    /* Scoped enums expose values only through qualified lookup (TEnum.Value),
                     * so do not inject their literals as global constants in this scope. */
                    if (alias_info->kgpc_type != NULL && !alias_info->enum_is_scoped)
                    {
                        int unit_idx = tree->tree_data.type_decl_data.source_unit_index;
                        ScopeNode *saved_scope_for_enum_literals =
                            semcheck_switch_to_unit_scope(symtab, unit_idx);

                        int ordinal = 0;
                        ListNode_t *literal_node = alias_info->enum_literals;
                        while (literal_node != NULL)
                        {
                            if (literal_node->cur != NULL)
                            {
                                char *literal_name = (char *)literal_node->cur;

                                /* Check for collisions in the target table. */
                                HashTable_t *target_table = SymTab_GetTargetTable(symtab);
                                HashNode_t *existing = FindIdentInTable(target_table, literal_name);
                                if (existing != NULL)
                                {
                                    /* Allow local enum literals to shadow imported unit literals. */
                                    if (!tree->tree_data.type_decl_data.defined_in_unit)
                                    {
                                        if (existing->type != NULL && existing->type != alias_info->kgpc_type)
                                            kgpc_type_release(existing->type);
                                        existing->is_constant = 1;
                                        existing->const_int_value = ordinal;
                                        existing->type = alias_info->kgpc_type;
                                        existing->defined_in_unit = 0;
                                        kgpc_type_retain(existing->type);
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* Imported unit collisions are allowed: keep first visible literal. */
                                    if (tree->tree_data.type_decl_data.defined_in_unit)
                                    {
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* If it exists as a constant with the same value, skip silently */
                                    if (existing->is_constant && existing->const_int_value == ordinal)
                                    {
                                        /* Same enum literal from prelude - not an error */
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* Different value in local declarations is a real conflict. */
                                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, 
                                        "Error on line %d, redeclaration of enum literal %s with different value!\n",
                                        tree->line_num, literal_name);
                                    ++errors;
                                    literal_node = literal_node->next;
                                    ++ordinal;
                                    continue;
                                }
                                
                                /* Use typed API with shared enum KgpcType - all literals reference same type */
                                if (PushConstOntoScope_Typed(symtab, literal_name, ordinal, alias_info->kgpc_type) > 0)
                                {
                                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, 
                                            "Error on line %d, redeclaration of enum literal %s!\n",
                                            tree->line_num, literal_name);
                                    ++errors;
                                }
                                else if (tree->tree_data.type_decl_data.defined_in_unit)
                                {
                                    HashNode_t *literal_node_entry = FindIdentInTable(
                                        target_table, literal_name);
                                    if (literal_node_entry != NULL)
                                    {
                                        literal_node_entry->defined_in_unit = 1;
                                        literal_node_entry->unit_is_public =
                                            tree->tree_data.type_decl_data.unit_is_public ? 1 : 0;
                                        mark_hashnode_source_unit(literal_node_entry, unit_idx);
                                    }
                                }
                            }
                            ++ordinal;
                            literal_node = literal_node->next;
                        }
                        semcheck_restore_scope(symtab, saved_scope_for_enum_literals);
                        /* KgpcType is owned by TypeAlias, will be cleaned up when tree is destroyed */
                    }
                    semcheck_restore_scope(symtab, saved_scope_for_enum_type);
                }
                /* Also handle set types with inline anonymous enum: set of (val1, val2, ...) */
                if (alias_info != NULL && alias_info->is_enum_set && alias_info->inline_enum_values != NULL)
                {
                    /* Create a local KgpcType for the inline enum values (not stored in alias).
                     * The set type itself remains SET_TYPE, but the enum values need their own type. */
                    KgpcType *inline_enum_type = create_primitive_type(ENUM_TYPE);
                    if (inline_enum_type == NULL)
                    {
                        fprintf(stderr, "Error: Failed to create inline enum type for set %s\n",
                                tree->tree_data.type_decl_data.id);
                        ++errors;
                    }
                    else
                    {
                        int unit_idx = tree->tree_data.type_decl_data.source_unit_index;
                        ScopeNode *saved_scope_for_inline_enum_literals =
                            semcheck_switch_to_unit_scope(symtab, unit_idx);
                        int ordinal = 0;
                        ListNode_t *literal_node = alias_info->inline_enum_values;
                        while (literal_node != NULL)
                        {
                            if (literal_node->cur != NULL)
                            {
                                char *literal_name = (char *)literal_node->cur;
                                
                                /* Check for collisions in the current scope only (allow shadowing). */
                                HashNode_t *existing = FindIdentInTable(
                                    symtab->current_scope->table, literal_name);
                                if (existing != NULL)
                                {
                                    /* Allow local enum literals to shadow imported unit literals. */
                                    if (!tree->tree_data.type_decl_data.defined_in_unit)
                                    {
                                        if (existing->type != NULL && existing->type != inline_enum_type)
                                            kgpc_type_release(existing->type);
                                        existing->is_constant = 1;
                                        existing->const_int_value = ordinal;
                                        existing->type = inline_enum_type;
                                        existing->defined_in_unit = 0;
                                        kgpc_type_retain(existing->type);
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    if (tree->tree_data.type_decl_data.defined_in_unit)
                                    {
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* If it exists as a constant with the same value, skip silently */
                                    if (existing->is_constant && existing->const_int_value == ordinal)
                                    {
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* Different value in local declarations is a real conflict. */
                                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, 
                                        "Error on line %d, redeclaration of inline enum literal %s with different value!\n",
                                        tree->line_num, literal_name);
                                    ++errors;
                                    literal_node = literal_node->next;
                                    ++ordinal;
                                    continue;
                                }
                                
                                /* Use typed API with shared enum KgpcType */
                                if (PushConstOntoScope_Typed(symtab, literal_name, ordinal, inline_enum_type) > 0)
                                {
                                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, 
                                            "Error on line %d, redeclaration of inline enum literal %s!\n",
                                            tree->line_num, literal_name);
                                    ++errors;
                                }
                                else if (tree->tree_data.type_decl_data.defined_in_unit)
                                {
                                    HashNode_t *literal_node_entry = FindIdentInTable(
                                        symtab->current_scope->table, literal_name);
                                    if (literal_node_entry != NULL)
                                    {
                                        literal_node_entry->defined_in_unit = 1;
                                        literal_node_entry->unit_is_public =
                                            tree->tree_data.type_decl_data.unit_is_public ? 1 : 0;
                                        mark_hashnode_source_unit(literal_node_entry, unit_idx);
                                    }
                                }
                            }
                            ++ordinal;
                            literal_node = literal_node->next;
                        }
                        semcheck_restore_scope(symtab, saved_scope_for_inline_enum_literals);
                        /* The enum constants now hold references to inline_enum_type, so we can release our ref */
                        destroy_kgpc_type(inline_enum_type);
                    }
                }
            }
            /* Handle record types with fields that have anonymous enum types */
            if (tree->type == TREE_TYPE_DECL &&
                tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
            {
                struct RecordType *record = tree->tree_data.type_decl_data.info.record;
                if (record != NULL)
                    errors += register_record_field_enum_literals(symtab, record);
            }
        }
        cur = cur->next;
    }

    return errors;
}

/* Pre-declare types so they can be used in const expressions like High(MyType).
 * This function creates stub entries in the symbol table for SIMPLE type aliases
 * BEFORE const declarations are processed.
 *
 * This enables patterns like:
 *   type
 *     MyInt = Int64;
 *   const
 *     MaxMyInt = High(MyInt);  // Works because MyInt is pre-declared
 *
 * IMPORTANT: We only pre-declare simple primitive type aliases (e.g., MyInt = Int64).
 * Complex types (pointers, arrays, records, etc.) are NOT pre-declared because
 * they may have forward references that can't be resolved yet.
 */

/* Return the storage size in bytes for a builtin Pascal type name.
 * Used to decide whether a type alias override widens or narrows. */
static long long semcheck_builtin_type_size(const char *name)
{
    if (name == NULL) return 0;
    if (pascal_identifier_equals(name, "LongInt") ||
        pascal_identifier_equals(name, "LongWord") ||
        pascal_identifier_equals(name, "DWord") ||
        pascal_identifier_equals(name, "Cardinal") ||
        pascal_identifier_equals(name, "Single"))
        return 4;
    if (pascal_identifier_equals(name, "SmallInt") ||
        pascal_identifier_equals(name, "Word") ||
        pascal_identifier_equals(name, "WideChar"))
        return 2;
    if (pascal_identifier_equals(name, "Byte") ||
        pascal_identifier_equals(name, "ShortInt") ||
        pascal_identifier_equals(name, "Boolean") ||
        pascal_identifier_equals(name, "Char") ||
        pascal_identifier_equals(name, "AnsiChar"))
        return 1;
    if (pascal_identifier_equals(name, "Int64") ||
        pascal_identifier_equals(name, "QWord") ||
        pascal_identifier_equals(name, "Double") ||
        pascal_identifier_equals(name, "Real"))
        return 8;
    return 0;
}

static int predeclare_types(SymTab_t *symtab, ListNode_t *type_decls)
{
    if (symtab == NULL)
        return 0;

    int errors = 0;
    ListNode_t *cur = type_decls;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;

            if (tree->type == TREE_TYPE_DECL)
            {
                ScopeNode *saved_scope_for_type = semcheck_switch_to_unit_scope(
                    symtab, tree->tree_data.type_decl_data.source_unit_index);
                const char *type_id = tree->tree_data.type_decl_data.id;

                /* Skip if no type id */
                if (type_id == NULL)
                {
                    goto predeclare_types_next;
                }
                
                /* Debug: print predeclare order */
                if (kgpc_getenv("KGPC_DEBUG_PREDECLARE") != NULL)
                {
                    fprintf(stderr, "[predeclare] type '%s' unit=%d",
                        type_id, tree->tree_data.type_decl_data.defined_in_unit);
                    if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
                    {
                        struct TypeAlias *a = &tree->tree_data.type_decl_data.info.alias;
                        fprintf(stderr, " alias target='%s'", a->target_type_id ? a->target_type_id : "<null>");
                    }
                    fprintf(stderr, "\n");
                }
                
                /* Check if already declared (e.g., from a previous pass or builtin) */
                HashNode_t *existing = FindIdentInCurrentScope(symtab, type_id);
                int found = (existing != NULL);
                if (kgpc_getenv("KGPC_DEBUG_TFLOAT") != NULL && type_id != NULL &&
                    pascal_identifier_equals(type_id, "TFloatFormatProfile"))
                {
                    fprintf(stderr, "[TFLOAT] predeclare existing found=%d node=%p kind=%d\n",
                        found, (void *)existing,
                        tree->tree_data.type_decl_data.kind);
                }
                if (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL &&
                    pascal_identifier_equals(type_id, "TSize"))
                {
                    fprintf(stderr, "[TSIZE-FLOW] tree: defined_in_unit=%d src_unit=%d kind=%d | existing=%p found=%d",
                        tree->tree_data.type_decl_data.defined_in_unit,
                        tree->tree_data.type_decl_data.source_unit_index,
                        tree->tree_data.type_decl_data.kind,
                        (void *)existing, found);
                    if (existing != NULL)
                        fprintf(stderr, " ex_defined_in_unit=%d ex_src_unit=%d ex_has_record=%d",
                            existing->defined_in_unit, existing->source_unit_index,
                            get_record_type_from_node(existing) != NULL);
                    fprintf(stderr, "\n");
                }
                if (existing != NULL)
                {
                    /* If the existing symbol is a non-record alias and we are now defining
                     * a record with the same name, replace the alias with the record.
                     * This is required for units that import a type alias (e.g. TSize = LongInt)
                     * and then define their own record TSize with methods.
                     * However, if they come from different units, push as a new entry
                     * so per-unit type resolution can pick the right one. */
                    int cross_unit_replace = 0;
                    if (tree->tree_data.type_decl_data.source_unit_index != 0 &&
                        existing->source_unit_index != 0 &&
                        tree->tree_data.type_decl_data.source_unit_index != existing->source_unit_index)
                        cross_unit_replace = 1;
                    /* A local type (source_unit_index=0) shadowing a unit type
                     * must also be pushed as a separate entry, not replaced
                     * in-place, so unit functions still see their original type. */
                    if (tree->tree_data.type_decl_data.source_unit_index == 0 &&
                        existing->source_unit_index != 0)
                        cross_unit_replace = 1;
                    if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD &&
                        existing->hash_type == HASHTYPE_TYPE &&
                        get_record_type_from_node(existing) == NULL &&
                        !cross_unit_replace)
                    {
                        struct RecordType *record_info = tree->tree_data.type_decl_data.info.record;
                        if (record_info != NULL)
                        {
                            if (record_info->type_id == NULL)
                                record_info->type_id = strdup(type_id);

                            KgpcType *kgpc_type = create_record_type(record_info);
                            if (record_type_is_class(record_info))
                            {
                                KgpcType *ptr = create_pointer_type(kgpc_type);
                                kgpc_type_release(kgpc_type);
                                kgpc_type = ptr;
                            }

                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }

                            if (existing->type != NULL)
                                destroy_kgpc_type(existing->type);
                            if (kgpc_type != NULL)
                                kgpc_type_retain(kgpc_type);
                            existing->type = kgpc_type;
                            mark_hashnode_unit_info(symtab, existing,
                                tree->tree_data.type_decl_data.defined_in_unit,
                                tree->tree_data.type_decl_data.unit_is_public);
                            mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                        }
                        goto predeclare_types_next;
                    }

                    /* If we already have a type alias without range/enum metadata and are now
                     * declaring a range/enum, replace the alias so Low/High work in consts.
                     * This is needed for circular unit references like Math -> Types. */
                    if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                        existing->hash_type == HASHTYPE_TYPE)
                    {
                        struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                        struct TypeAlias *existing_alias = get_type_alias_from_node(existing);
                        int new_has_range = (alias->is_range || alias->range_known);
                        int existing_has_range = (existing_alias != NULL && existing_alias->range_known);
                        int new_has_enum = (alias->is_enum && alias->enum_literals != NULL);
                        int existing_has_enum =
                            (existing_alias != NULL && existing_alias->is_enum &&
                             existing_alias->enum_literals != NULL);

                        if ((new_has_range && !existing_has_range) ||
                            (new_has_enum && !existing_has_enum))
                        {
                            KgpcType *kgpc_type = NULL;
                            if (new_has_enum)
                            {
                                kgpc_type = create_primitive_type(ENUM_TYPE);
                            }
                            else
                            {
                                int base_tag = alias->base_type != UNKNOWN_TYPE ? alias->base_type : INT_TYPE;
                                if (alias->storage_size > 0)
                                    kgpc_type = create_primitive_type_with_size(base_tag, (int)alias->storage_size);
                                else
                                    kgpc_type = create_primitive_type(base_tag);
                            }

                            if (kgpc_type != NULL)
                            {
                                kgpc_type_set_type_alias(kgpc_type, alias);
                                if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                                {
                                    tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                    kgpc_type_retain(kgpc_type);
                                }
                                if (existing->type != NULL)
                                    destroy_kgpc_type(existing->type);
                                kgpc_type_retain(kgpc_type);
                                existing->type = kgpc_type;
                                mark_hashnode_unit_info(symtab, existing,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                                /* Release creator's reference */
                                destroy_kgpc_type(kgpc_type);
                            }
                            goto predeclare_types_next;
                        }

                        /* If we already have a non-pointer alias and the new declaration
                         * is a pointer alias (e.g., punicodemap = ^tunicodemap replacing
                         * a predeclared punicodemap = Pointer stub), upgrade the existing
                         * entry to TYPE_KIND_POINTER so pointer dereferences work. */
                        struct TypeAlias *new_alias_ptr = &tree->tree_data.type_decl_data.info.alias;
                        struct TypeAlias *existing_alias_ptr = get_type_alias_from_node(existing);
                        int new_is_pointer = (new_alias_ptr != NULL && new_alias_ptr->is_pointer);
                        int existing_is_pointer = (existing_alias_ptr != NULL && existing_alias_ptr->is_pointer);
                        if (new_is_pointer && !existing_is_pointer)
                        {
                            KgpcType *kgpc_type = create_kgpc_type_from_type_alias(
                                new_alias_ptr, symtab,
                                tree->tree_data.type_decl_data.defined_in_unit);
                            if (kgpc_type == NULL)
                            {
                                /* Fallback: create a basic pointer type */
                                kgpc_type = create_pointer_type(NULL);
                                kgpc_type_set_type_alias(kgpc_type, new_alias_ptr);
                            }
                            if (kgpc_type != NULL)
                            {
                                if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                                {
                                    tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                    kgpc_type_retain(kgpc_type);
                                }
                                if (existing->type != NULL)
                                    destroy_kgpc_type(existing->type);
                                kgpc_type_retain(kgpc_type);
                                existing->type = kgpc_type;
                                mark_hashnode_unit_info(symtab, existing,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                                destroy_kgpc_type(kgpc_type);
                            }
                            goto predeclare_types_next;
                        }
                    }

                    /* Allow record types from different units to coexist as separate entries.
                     * This handles cases like System.tsiginfo vs SysUtils.tsiginfo and
                     * UnixType.TSize (alias) vs Types.TSize (record).
                     * The ranking logic picks the right one based on same-unit preference. */
                    int existing_is_record = (existing->type != NULL &&
                        existing->type->kind == TYPE_KIND_RECORD);
                    struct TypeAlias *existing_alias = get_type_alias_from_node(existing);
                    int current_is_enum_alias = (
                        tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                        tree->tree_data.type_decl_data.info.alias.is_enum);
                    int existing_is_enum_alias = (
                        existing_alias != NULL &&
                        existing_alias->is_enum);
                    int cross_unit_coexist = (
                        (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD ||
                         (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                            (existing_is_record || current_is_enum_alias || existing_is_enum_alias))) &&
                        existing->source_unit_index != 0 &&
                        tree->tree_data.type_decl_data.source_unit_index != 0 &&
                        existing->source_unit_index != tree->tree_data.type_decl_data.source_unit_index);
                    if (!(existing->defined_in_unit &&
                          !tree->tree_data.type_decl_data.defined_in_unit) &&
                        !cross_unit_coexist)
                    {
                        /* Check if this is a forward class declaration being completed.
                         * Forward: "TObject = class;" creates empty class, then full definition follows.
                         * Allow the full definition to replace the forward declaration. */
                        int is_forward_class_completion = 0;
                        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                        {
                            struct RecordType *new_record = tree->tree_data.type_decl_data.info.record;
                            struct RecordType *existing_record = get_record_type_from_node(existing);
                            if (new_record != NULL && new_record->is_class &&
                                existing_record != NULL && existing_record->is_class)
                            {
                                /* Check if existing has only hidden fields (forward decl) */
                                int has_non_hidden = 0;
                                ListNode_t *fnode = existing_record->fields;
                                while (fnode != NULL)
                                {
                                    if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
                                    {
                                        struct RecordField *f = (struct RecordField *)fnode->cur;
                                        if (!record_field_is_hidden(f))
                                        {
                                            has_non_hidden = 1;
                                            break;
                                        }
                                    }
                                    fnode = fnode->next;
                                }
                                /* The full definition may have fields, properties,
                                 * method templates, or a parent class – any of these
                                 * indicate a real class body replacing the forward stub. */
                                int new_has_body = (new_record->fields != NULL ||
                                    new_record->properties != NULL ||
                                    new_record->method_templates != NULL ||
                                    new_record->parent_class_name != NULL);
                                if (!has_non_hidden && new_has_body)
                                    is_forward_class_completion = 1;
                            }
                            /* Also handle forward interface declarations:
                             * "IMyIntf = interface;" followed by the full definition. */
                            if (new_record != NULL && new_record->is_interface &&
                                existing_record != NULL && existing_record->is_interface)
                            {
                                int existing_has_body = (existing_record->fields != NULL ||
                                    existing_record->properties != NULL ||
                                    existing_record->method_templates != NULL);
                                int new_has_body = (new_record->fields != NULL ||
                                    new_record->properties != NULL ||
                                    new_record->method_templates != NULL ||
                                    new_record->parent_class_name != NULL);
                                if (!existing_has_body && new_has_body)
                                    is_forward_class_completion = 1;
                            }
                        }
                        /* Allow a simple type alias from a unit to override an
                         * existing alias when the target type differs AND both
                         * are plain (non-array, non-pointer) aliases.  This is
                         * required for objpas.pp which redefines
                         *   Integer = LongInt   (overriding system's Integer = SmallInt)
                         * when {$mode objfpc} or {$mode delphi} is active. */
                        int is_alias_override = 0;
                        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                            existing->hash_type == HASHTYPE_TYPE &&
                            tree->tree_data.type_decl_data.defined_in_unit)
                        {
                            struct TypeAlias *new_alias = &tree->tree_data.type_decl_data.info.alias;
                            struct TypeAlias *old_alias = get_type_alias_from_node(existing);
                            if (new_alias != NULL && old_alias != NULL &&
                                new_alias->target_type_id != NULL &&
                                old_alias->target_type_id != NULL &&
                                !new_alias->is_array && !new_alias->is_pointer &&
                                !new_alias->is_set && !new_alias->is_file &&
                                !new_alias->inline_record_type &&
                                /* Also require the OLD alias to not be a pointer/array/set/file.
                                 * This prevents objpas PString=PAnsiString from overriding
                                 * system pstring=^shortstring (which has is_pointer=1). */
                                !old_alias->is_array && !old_alias->is_pointer &&
                                !old_alias->is_set && !old_alias->is_file &&
                                !old_alias->inline_record_type &&
                                !pascal_identifier_equals(new_alias->target_type_id, old_alias->target_type_id))
                            {
                                /* Only allow override for the specific case where
                                 * objpas.pp redefines Integer=LongInt over system's
                                 * Integer=SmallInt.  We also handle MaxInt redefinition.
                                 * Other aliases must not be overridden to avoid breaking
                                 * NativeInt, TDateTime, PString, etc. */
                                if (pascal_identifier_equals(tree->tree_data.type_decl_data.id, "Integer") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PInteger") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "SizeInt") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PtrInt") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "CodePtrInt") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "ValSInt") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "ValUInt"))
                                {
                                    /* Only allow widening overrides (e.g. Integer=LongInt
                                     * over Integer=SmallInt from objpas over system).
                                     * Prevent system's Integer=SmallInt from overriding
                                     * objpas's Integer=LongInt when the merged type
                                     * declarations are processed in an order where
                                     * system comes after objpas. */
                                    long long new_size = new_alias->storage_size;
                                    long long old_size = old_alias->storage_size;
                                    if (new_size <= 0 && new_alias->target_type_id != NULL)
                                        new_size = semcheck_builtin_type_size(new_alias->target_type_id);
                                    if (old_size <= 0 && old_alias->target_type_id != NULL)
                                        old_size = semcheck_builtin_type_size(old_alias->target_type_id);
                                    /* Allow override only if new type is at least as wide,
                                     * or if sizes can't be determined (allow by default). */
                                    if (new_size >= old_size || new_size <= 0 || old_size <= 0)
                                        is_alias_override = 1;
                                }
                            }
                        }
                        if (!is_forward_class_completion && !is_alias_override)
                        {
                            /* Already declared, skip */
                            goto predeclare_types_next;
                        }
                        if (is_alias_override)
                        {
                            /* Replace the existing type with the new alias */
                            struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                            if (alias->target_type_id != NULL)
                                apply_builtin_integer_alias_metadata(alias, alias->target_type_id);
                            int base_tag = alias->base_type != UNKNOWN_TYPE ? alias->base_type : INT_TYPE;
                            KgpcType *kgpc_type = NULL;
                            if (alias->storage_size > 0)
                                kgpc_type = create_primitive_type_with_size(base_tag, (int)alias->storage_size);
                            else
                                kgpc_type = create_primitive_type(base_tag);
                            if (kgpc_type != NULL)
                            {
                                kgpc_type_set_type_alias(kgpc_type, alias);
                                if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                                {
                                    tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                    kgpc_type_retain(kgpc_type);
                                }
                                if (existing->type != NULL)
                                    destroy_kgpc_type(existing->type);
                                kgpc_type_retain(kgpc_type);
                                existing->type = kgpc_type;
                                mark_hashnode_unit_info(symtab, existing,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                                /* Release creator's reference */
                                destroy_kgpc_type(kgpc_type);
                            }
                            goto predeclare_types_next;
                        }
                        else
                        {
                            /* Forward class completion: update existing record in-place.
                             * We must update in-place because existing code (e.g. Self
                             * parameters in method implementations) already holds pointers
                             * to the forward declaration's RecordType. Shadowing would not
                             * update those existing references. */
                            struct RecordType *new_record = tree->tree_data.type_decl_data.info.record;
                            struct RecordType *existing_record = get_record_type_from_node(existing);
                            if (existing_record != NULL && new_record != NULL &&
                                existing_record != new_record)
                            {
                                /* Move new fields into existing record, keeping hidden fields at front */
                                ListNode_t *old_fields = existing_record->fields;
                                existing_record->fields = new_record->fields;
                                new_record->fields = NULL; /* Transfer ownership to existing */
                                /* Mark the depleted record so merge_parent_class_fields
                                 * won't re-merge parent fields into this empty shell. */
                                new_record->parent_fields_merged = 1;
                                /* Re-add hidden fields from forward decl at front,
                                 * but ONLY if the full declaration has no parent class.
                                 * When a parent exists, the hidden fields (e.g.
                                 * __kgpc_class_typeinfo) will be inherited from the
                                 * parent during merge_parent_class_fields, so re-adding
                                 * them here would create duplicates. */
                                if (new_record->parent_class_name == NULL)
                                {
                                    while (old_fields != NULL)
                                    {
                                        ListNode_t *next = old_fields->next;
                                        if (old_fields->type == LIST_RECORD_FIELD && old_fields->cur != NULL)
                                        {
                                            struct RecordField *f = (struct RecordField *)old_fields->cur;
                                            if (record_field_is_hidden(f))
                                            {
                                                /* Skip if transferred fields already have this hidden field
                                                 * (both forward and full root-class decls get __kgpc_class_typeinfo
                                                 * from from_cparser.c, so re-adding would create duplicates). */
                                                int already_has = 0;
                                                if (f->name != NULL) {
                                                    ListNode_t *chk = existing_record->fields;
                                                    while (chk != NULL) {
                                                        if (chk->type == LIST_RECORD_FIELD && chk->cur != NULL) {
                                                            struct RecordField *ef = (struct RecordField *)chk->cur;
                                                            if (record_field_is_hidden(ef) && ef->name != NULL &&
                                                                strcmp(ef->name, f->name) == 0) {
                                                                already_has = 1;
                                                                break;
                                                            }
                                                        }
                                                        chk = chk->next;
                                                    }
                                                }
                                                if (!already_has)
                                                {
                                                    old_fields->next = NULL;
                                                    if (existing_record->fields != NULL)
                                                        existing_record->fields = PushListNodeFront(existing_record->fields, old_fields);
                                                    else
                                                        existing_record->fields = old_fields;
                                                }
                                            }
                                        }
                                        old_fields = next;
                                    }
                                }
                                /* Transfer method templates, properties, parent class, interfaces */
                                if (new_record->method_templates != NULL)
                                {
                                    existing_record->method_templates = new_record->method_templates;
                                    new_record->method_templates = NULL;
                                }
                                if (new_record->properties != NULL)
                                {
                                    existing_record->properties = new_record->properties;
                                    new_record->properties = NULL;
                                }
                                if (new_record->parent_class_name != NULL &&
                                    existing_record != new_record &&
                                    existing_record->parent_class_name != new_record->parent_class_name)
                                {
                                    char *new_parent_copy = strdup(new_record->parent_class_name);
                                    free(existing_record->parent_class_name);
                                    existing_record->parent_class_name = new_parent_copy;
                                }
                                if (new_record->interface_names != NULL &&
                                    existing_record != new_record)
                                {
                                    existing_record->interface_names = new_record->interface_names;
                                    existing_record->num_interfaces = new_record->num_interfaces;
                                    new_record->interface_names = NULL;
                                    new_record->num_interfaces = 0;
                                }
                                /* Store kgpc_type in tree for later reuse */
                                if (tree->tree_data.type_decl_data.kgpc_type == NULL && existing->type != NULL)
                                {
                                    tree->tree_data.type_decl_data.kgpc_type = existing->type;
                                    kgpc_type_retain(existing->type);
                                }
                                mark_hashnode_unit_info(symtab, existing,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                                semcheck_mark_resolved_forward_stub(type_decls, cur,
                                    type_id,
                                    tree->tree_data.type_decl_data.source_unit_index,
                                    existing_record);
                            }
                            goto predeclare_types_next;
                        }
                    }
                }

                /* Predeclare record types so they can be referenced (e.g., as function returns) */
                if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                {
                    struct RecordType *record_info = tree->tree_data.type_decl_data.info.record;

                    /* Annotate the record with its canonical name if missing */
                    if (record_info != NULL && record_info->type_id == NULL)
                        record_info->type_id = strdup(type_id);
                    
                    KgpcType *kgpc_type = create_record_type(record_info);
                    if (record_type_is_class(record_info))
                    {
                        /* Classes are reference types - register as pointers to the record */
                        KgpcType *ptr = create_pointer_type(kgpc_type);
                        kgpc_type_release(kgpc_type); /* create_pointer_type retained inner */
                        kgpc_type = ptr;
                    }
                    if (kgpc_type != NULL)
                    {
                        /* Store in tree for later reuse by semcheck_type_decls */
                        if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                        {
                            tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                            kgpc_type_retain(kgpc_type);
                        }

                        int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                        if (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL &&
                            pascal_identifier_equals(type_id, "TSize"))
                        {
                            fprintf(stderr, "[TSIZE-PUSH] push result=%d tree_defined_in_unit=%d tree_src_unit=%d\n",
                                result, tree->tree_data.type_decl_data.defined_in_unit,
                                tree->tree_data.type_decl_data.source_unit_index);
                        }
                        if (kgpc_getenv("KGPC_DEBUG_FORWARD_CLASS") != NULL && record_info != NULL && record_info->is_class)
                        {
                            fprintf(stderr, "[FWD-PRE] type='%s' push_result=%d fields=%p\n",
                                type_id, result, (void *)record_info->fields);
                            ListNode_t *dbg = record_info->fields;
                            while (dbg != NULL)
                            {
                                if (dbg->type == LIST_RECORD_FIELD && dbg->cur != NULL)
                                {
                                    struct RecordField *f = (struct RecordField *)dbg->cur;
                                    fprintf(stderr, "[FWD-PRE]   field: %s hidden=%d\n",
                                        f->name ? f->name : "<null>", record_field_is_hidden(f));
                                }
                                dbg = dbg->next;
                            }
                        }
                        if (result > 0)
                        {
                            /* Check if this is a forward class declaration being resolved.
                             * Forward class: "TObject = class;" creates an empty class,
                             * then "TObject = class ... end;" provides the full definition.
                             * Update the existing symbol's type with the full definition. */
                            if (record_info != NULL && record_info->is_class)
                            {
                                HashNode_t *existing = NULL;
                                int find_result = FindSymbol(&existing, symtab, type_id);
                                if (kgpc_getenv("KGPC_DEBUG_FORWARD_CLASS") != NULL)
                                    fprintf(stderr, "[FWD] type='%s' find=%d existing=%p hash_type=%d\n",
                                        type_id, find_result, (void*)existing,
                                        existing ? existing->hash_type : -1);
                                if (find_result &&
                                    existing != NULL &&
                                    existing->hash_type == HASHTYPE_TYPE &&
                                    existing->type != NULL)
                                {
                                    struct RecordType *existing_record = get_record_type_from_node(existing);
                                    if (existing_record != NULL && existing_record->is_class)
                                    {
                                        /* Check if existing is a forward declaration (only hidden fields) */
                                        int has_non_hidden_fields = 0;
                                        ListNode_t *fnode = existing_record->fields;
                                        while (fnode != NULL)
                                        {
                                            if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
                                            {
                                                struct RecordField *f = (struct RecordField *)fnode->cur;
                                                if (!record_field_is_hidden(f))
                                                {
                                                    has_non_hidden_fields = 1;
                                                    break;
                                                }
                                            }
                                            fnode = fnode->next;
                                        }
                                        if (!has_non_hidden_fields && record_info->fields != NULL)
                                        {
                                            /* Forward declaration being resolved - update existing record's fields */
                                            /* Prepend the hidden typeinfo field from existing to new record's fields */
                                            ListNode_t *existing_fields = existing_record->fields;
                                            existing_record->fields = record_info->fields;
                                            /* Re-add hidden fields at front, skipping duplicates */
                                            while (existing_fields != NULL)
                                            {
                                                ListNode_t *next = existing_fields->next;
                                                if (existing_fields->type == LIST_RECORD_FIELD && existing_fields->cur != NULL)
                                                {
                                                    struct RecordField *f = (struct RecordField *)existing_fields->cur;
                                                    if (record_field_is_hidden(f))
                                                    {
                                                        int already_has = 0;
                                                        if (f->name != NULL) {
                                                            ListNode_t *chk = existing_record->fields;
                                                            while (chk != NULL) {
                                                                if (chk->type == LIST_RECORD_FIELD && chk->cur != NULL) {
                                                                    struct RecordField *ef = (struct RecordField *)chk->cur;
                                                                    if (record_field_is_hidden(ef) && ef->name != NULL &&
                                                                        strcmp(ef->name, f->name) == 0) {
                                                                        already_has = 1;
                                                                        break;
                                                                    }
                                                                }
                                                                chk = chk->next;
                                                            }
                                                        }
                                                        if (!already_has)
                                                        {
                                                            existing_fields->next = NULL;
                                                            existing_record->fields = PushListNodeFront(existing_record->fields, existing_fields);
                                                        }
                                                    }
                                                    else
                                                    {
                                                        /* Non-hidden field from forward decl - shouldn't happen, just free */
                                                        free(existing_fields);
                                                    }
                                                }
                                                else
                                                {
                                                    free(existing_fields);
                                                }
                                                existing_fields = next;
                                            }
                                            /* Copy over method templates and properties */
                                            if (record_info->method_templates != NULL)
                                                existing_record->method_templates = record_info->method_templates;
                                            if (record_info->properties != NULL)
                                                existing_record->properties = record_info->properties;
                                            if (record_info->parent_class_name != NULL && existing_record->parent_class_name == NULL)
                                                existing_record->parent_class_name = strdup(record_info->parent_class_name);
                                            if (record_info->interface_names != NULL)
                                            {
                                                existing_record->interface_names = record_info->interface_names;
                                                existing_record->num_interfaces = record_info->num_interfaces;
                                                record_info->interface_names = NULL;
                                                record_info->num_interfaces = 0;
                                            }
                                            semcheck_mark_resolved_forward_stub(type_decls, cur,
                                                type_id,
                                                tree->tree_data.type_decl_data.source_unit_index,
                                                existing_record);
                                            result = 0; /* Suppress the error */
                                        }
                                    }
                                }
                            }
                            if (result > 0)
                                errors += result;
                        }
                        else
                        {
                            /* Use FindIdent to get the most recently pushed entry (front of list).
                             * We can't use semcheck_find_type_node_with_unit_flag here because
                             * the newly pushed entry hasn't been marked yet (defined_in_unit=0),
                             * so that function would skip it and find an older entry instead. */
                            HashNode_t *type_node = NULL;
                            FindSymbol(&type_node, symtab, type_id);
                            if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                            {
                                mark_hashnode_unit_info(symtab, type_node,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(type_node, tree->tree_data.type_decl_data.source_unit_index);
                            }
                        }
                        /* Release creator's reference - tree and hash table have their own */
                        destroy_kgpc_type(kgpc_type);
                    }
                }
                /* Only handle TYPE_DECL_ALIAS cases we can resolve early */
                else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
                {
                    struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                    alias->is_char_alias = semcheck_alias_should_be_char_like(type_id,
                        alias->target_type_id) &&
                        !alias->is_pointer && !alias->is_array && !alias->is_set &&
                        !alias->is_enum && !alias->is_file;

                    /* WideChar/UnicodeChar must be predeclared as CHAR_TYPE before
                     * apply_builtin_integer_alias_metadata converts them to WORD_TYPE. */
                    if (alias->is_char_alias && type_id != NULL &&
                        (pascal_identifier_equals(type_id, "WideChar") ||
                         pascal_identifier_equals(type_id, "UnicodeChar")))
                    {
                        if (alias->alias_name == NULL)
                            alias->alias_name = strdup(type_id);
                        alias->storage_size = 2;
                        KgpcType *kgpc_type = create_primitive_type_with_size(CHAR_TYPE, 2);
                        if (kgpc_type != NULL)
                        {
                            kgpc_type_set_type_alias(kgpc_type, alias);
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                        }
                        goto predeclare_types_next;
                    }

                    /* Do NOT apply integer alias metadata to pointer types.
                     * For PWord = ^Word, target_type_id is "Word" (the pointee),
                     * and applying Word's metadata (is_range, storage_size=2) would
                     * corrupt the pointer alias, causing it to be treated as a
                     * 2-byte range type instead of an 8-byte pointer. */
                    if (!alias->is_pointer)
                    {
                        if (alias->target_type_id != NULL)
                            apply_builtin_integer_alias_metadata(alias, alias->target_type_id);
                        else if (type_id != NULL)
                            apply_builtin_integer_alias_metadata(alias, type_id);
                    }

                    /* Predeclare range aliases early so Low/High on subranges work in consts. */
                    if (alias->is_range || alias->range_known)
                    {
                        int base_tag = alias->base_type != UNKNOWN_TYPE ? alias->base_type : INT_TYPE;
                        KgpcType *kgpc_type = NULL;
                        if (alias->storage_size > 0)
                            kgpc_type = create_primitive_type_with_size(base_tag, (int)alias->storage_size);
                        else
                            kgpc_type = create_primitive_type(base_tag);
                        if (kgpc_type != NULL)
                        {
                            kgpc_type_set_type_alias(kgpc_type, alias);
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                            {
                                /* Push failed (duplicate).  Allow a unit-level alias to
                                 * replace the existing entry when it is one of the known
                                 * overridable types (e.g. objpas redefining Integer=LongInt
                                 * over system's Integer=SmallInt). */
                                if (tree->tree_data.type_decl_data.defined_in_unit &&
                                    (pascal_identifier_equals(type_id, "Integer") ||
                                     pascal_identifier_equals(type_id, "PInteger") ||
                                     pascal_identifier_equals(type_id, "SizeInt") ||
                                     pascal_identifier_equals(type_id, "PtrInt") ||
                                     pascal_identifier_equals(type_id, "CodePtrInt") ||
                                     pascal_identifier_equals(type_id, "ValSInt") ||
                                     pascal_identifier_equals(type_id, "ValUInt")))
                                {
                                    HashNode_t *existing_node = NULL;
                                    if (FindSymbol(&existing_node, symtab, type_id) != 0 &&
                                        existing_node != NULL &&
                                        existing_node->hash_type == HASHTYPE_TYPE)
                                    {
                                        if (existing_node->type != NULL)
                                            destroy_kgpc_type(existing_node->type);
                                        kgpc_type_retain(kgpc_type);
                                        existing_node->type = kgpc_type;
                                        mark_hashnode_unit_info(symtab, existing_node,
                                            tree->tree_data.type_decl_data.defined_in_unit,
                                            tree->tree_data.type_decl_data.unit_is_public);
                                        mark_hashnode_source_unit(existing_node,
                                            tree->tree_data.type_decl_data.source_unit_index);
                                        result = 0;
                                    }
                                }
                                if (result > 0)
                                    errors += result;
                            }
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                        }
                        goto predeclare_types_next;
                    }

                    /* Handle inline record aliases (e.g., generic specializations) */
                    if (alias->inline_record_type != NULL)
                    {
                        if (alias->inline_record_type->type_id == NULL)
                            alias->inline_record_type->type_id = strdup(type_id);
                        const char *record_name = alias->inline_record_type->type_id;
                        HashNode_t *existing_inline = FindIdentInCurrentScope(symtab, record_name);
                        if (existing_inline == NULL)
                        {
                            KgpcType *inline_kgpc = create_record_type(alias->inline_record_type);
                            if (record_type_is_class(alias->inline_record_type))
                                inline_kgpc = create_pointer_type(inline_kgpc);

                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = inline_kgpc;
                                kgpc_type_retain(inline_kgpc);
                            }

                            int result = PushTypeOntoScope_Typed(symtab, (char *)record_name, inline_kgpc);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, record_name,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(inline_kgpc);
                        }

                        /* Also register the alias name itself */
                        KgpcType *alias_kgpc = create_record_type(alias->inline_record_type);
                        if (record_type_is_class(alias->inline_record_type))
                            alias_kgpc = create_pointer_type(alias_kgpc);
                        kgpc_type_set_type_alias(alias_kgpc, alias);
                        int alias_result = PushTypeOntoScope_Typed(symtab, (char *)type_id, alias_kgpc);
                        if (alias_result > 0)
                            errors += alias_result;
                        else
                        {
                            mark_latest_type_node_unit_info(symtab, type_id,
                                tree->tree_data.type_decl_data.defined_in_unit,
                                tree->tree_data.type_decl_data.unit_is_public,
                                tree->tree_data.type_decl_data.source_unit_index);
                        }
                        /* Release creator's reference */
                        destroy_kgpc_type(alias_kgpc);

                        goto predeclare_types_next;
                    }

                    /* Pre-declare enum types so they can be used as function return types.
                     * This creates a stub entry - the actual enum literals are registered
                     * separately in predeclare_enum_literals(). */
                    if (alias->is_enum)
                    {
                        KgpcType *kgpc_type = NULL;
                        /* Check if already created during predeclare_enum_literals */
                        if (alias->kgpc_type != NULL)
                        {
                            kgpc_type = alias->kgpc_type;
                            kgpc_type_retain(kgpc_type);  /* Own a reference so we can release uniformly */
                        }
                        else
                        {
                            kgpc_type = create_primitive_type(ENUM_TYPE);
                            if (kgpc_type != NULL)
                            {
                                /* Store in alias - alias now owns this reference */
                                alias->kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);  /* Alias holds reference */
                            }
                        }
                        
                        if (kgpc_type != NULL)
                        {
                            /* Attach the type_alias so scoped enum lookup can find enum_literals */
                            kgpc_type_set_type_alias(kgpc_type, alias);
                            
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);  /* Tree holds reference */
                            }
                            
                            /* PushTypeOntoScope_Typed will retain kgpc_type when adding to symbol table */
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                        }
                        goto predeclare_types_next;
                    }

                    /* Predeclare array/set/file aliases so return types can resolve early. */
                    if (alias->is_array || alias->is_set || alias->is_file)
                    {
                        KgpcType *kgpc_type = create_kgpc_type_from_type_alias(
                            alias, symtab, tree->tree_data.type_decl_data.defined_in_unit);
                        if (kgpc_type != NULL)
                        {
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                            goto predeclare_types_next;
                        }
                        goto predeclare_types_next;
                    }
                    
                    /* Handle pointer aliases to already known element types */
                    if (alias->is_pointer)
                    {
                        KgpcType *kgpc_type = create_kgpc_type_from_type_alias(
                            alias, symtab, tree->tree_data.type_decl_data.defined_in_unit);
                        if (kgpc_type != NULL)
                        {
                            if (kgpc_getenv("KGPC_DEBUG_PREDECLARE_POINTERS") != NULL)
                            {
                                fprintf(stderr, "[KGPC] predeclare pointer alias %s: ptr_type=%d ptr_id=%s kind=%d\n",
                                    type_id,
                                    alias->pointer_type,
                                    alias->pointer_type_id ? alias->pointer_type_id : "<null>",
                                    kgpc_type->kind);
                            }
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);

                            goto predeclare_types_next;
                        }
                    }

                    /* Pre-declare simple alias types (e.g., TEndian = ObjPas.TEndian) so they
                     * are available for const expressions before full type checking. */
                    if (!alias->is_array && !alias->is_set && !alias->is_file &&
                        !alias->is_pointer && alias->base_type == UNKNOWN_TYPE)
                    {
                        KgpcType *kgpc_type = create_kgpc_type_from_type_alias(
                            alias, symtab, tree->tree_data.type_decl_data.defined_in_unit);
                        if (kgpc_type == NULL)
                        {
                            kgpc_type = create_primitive_type(UNKNOWN_TYPE);
                            if (kgpc_type != NULL)
                                kgpc_type_set_type_alias(kgpc_type, alias);
                        }
                        if (kgpc_type != NULL)
                        {
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                            {
                                HashNode_t *existing_type = NULL;
                                if (FindSymbol(&existing_type, symtab, type_id) != 0 &&
                                    existing_type != NULL &&
                                    existing_type->hash_type == HASHTYPE_TYPE &&
                                    existing_type->type != NULL)
                                {
                                    struct TypeAlias *existing_alias =
                                        kgpc_type_get_type_alias(existing_type->type);
                                    int can_overlay_existing =
                                        (existing_alias != NULL) &&
                                        (alias->target_type_ref != NULL ||
                                         alias->target_type_id != NULL) &&
                                        !alias->is_pointer &&
                                        !alias->is_array &&
                                        !alias->is_set &&
                                        !alias->is_file &&
                                        existing_type->source_unit_index == 0 &&
                                        !existing_type->defined_in_unit;
                                    if (can_overlay_existing)
                                    {
                                        inherit_alias_metadata(symtab, alias);
                                        kgpc_type_set_type_alias(existing_type->type, alias);
                                        if (existing_type->type->type_alias != NULL &&
                                            alias->storage_size > 0)
                                        {
                                            existing_type->type->type_alias->storage_size =
                                                alias->storage_size;
                                        }
                                        result = 0;
                                    }
                                }
                                if (result > 0)
                                    errors += result;
                            }
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                            goto predeclare_types_next;
                        }
                    }

                    /* Pre-declare procedural type aliases so const initializers can use them */
                    if (alias->base_type == PROCEDURE ||
                        (tree->tree_data.type_decl_data.kgpc_type != NULL &&
                         tree->tree_data.type_decl_data.kgpc_type->kind == TYPE_KIND_PROCEDURE))
                    {
                        KgpcType *kgpc_type = tree->tree_data.type_decl_data.kgpc_type;
                        if (kgpc_type != NULL)
                        {
                            kgpc_type_set_type_alias(kgpc_type, alias);
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }

                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                        }
                        goto predeclare_types_next;
                    }

                    /* Only pre-declare simple primitive type aliases */
                    KgpcType *kgpc_type = NULL;
                    int created_new_type = 0;

                    /* WideChar/UnicodeChar should be treated as 2-byte CHAR_TYPE, even if aliased to Word. */
                    if (type_id != NULL &&
                        (pascal_identifier_equals(type_id, "WideChar") ||
                         pascal_identifier_equals(type_id, "UnicodeChar")))
                    {
                        kgpc_type = create_primitive_type_with_size(CHAR_TYPE, 2);
                        if (kgpc_type != NULL)
                            created_new_type = 1;
                    }
                    
                    /* Case 1: Direct primitive type tag (e.g., MyInt = Integer where base_type is set)
                     * Exclude PROCEDURE - procedure types are NOT primitive and need special handling.
                     * IMPORTANT: If target_type_id is "WideChar", skip to Case 2 to look it up
                     * from the symbol table where it has correct storage_size=2. Without this check,
                     * WideChar aliases would get 4 bytes (INT_TYPE) instead of 2 bytes. */
                    int skip_case1_for_widechar = (alias->target_type_id != NULL &&
                        pascal_identifier_equals(alias->target_type_id, "WideChar"));
                    if (kgpc_type == NULL && !skip_case1_for_widechar &&
                        alias->base_type != UNKNOWN_TYPE && alias->base_type != 0 &&
                        alias->base_type != PROCEDURE)
                    {
                        if (alias->storage_size > 0)
                            kgpc_type = create_primitive_type_with_size(alias->base_type,
                                (int)alias->storage_size);
                        else
                            kgpc_type = create_primitive_type(alias->base_type);
                        if (kgpc_type != NULL)
                            created_new_type = 1;
                    }
                    /* Case 2: Reference to a known primitive type name */
                    else if (kgpc_type == NULL && alias->target_type_id != NULL)
                    {
                        /* Check if target is a known builtin primitive type */
                        const TypeRef *target_ref = alias->target_type_ref;
                        const char *target = alias->target_type_id;
                        const char *target_base = (target_ref != NULL)
                            ? type_ref_base_name(target_ref)
                            : target;
                        if (pascal_identifier_equals(target_base, "Int64") ||
                            pascal_identifier_equals(target_base, "QWord") ||
                            pascal_identifier_equals(target_base, "UInt64") ||
                            pascal_identifier_equals(target_base, "SizeUInt") ||
                            pascal_identifier_equals(target_base, "SizeInt") ||
                            pascal_identifier_equals(target_base, "PtrUInt") ||
                            pascal_identifier_equals(target_base, "PtrInt") ||
                            pascal_identifier_equals(target_base, "NativeUInt") ||
                            pascal_identifier_equals(target_base, "NativeInt"))
                        {
                            kgpc_type = create_primitive_type(INT64_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "LongInt") ||
                            pascal_identifier_equals(target_base, "Cardinal") ||
                            pascal_identifier_equals(target_base, "LongWord") ||
                            pascal_identifier_equals(target_base, "DWord"))
                        {
                            kgpc_type = create_primitive_type(LONGINT_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Integer") ||
                            pascal_identifier_equals(target_base, "SmallInt") ||
                            pascal_identifier_equals(target_base, "ShortInt") ||
                            pascal_identifier_equals(target_base, "Byte") ||
                            pascal_identifier_equals(target_base, "Word"))
                        {
                            int storage_size = 0;
                            if (pascal_identifier_equals(target_base, "Byte") ||
                                pascal_identifier_equals(target_base, "ShortInt"))
                                storage_size = 1;
                            else if (pascal_identifier_equals(target_base, "Word") ||
                                     pascal_identifier_equals(target_base, "SmallInt"))
                                storage_size = 2;
                            if (storage_size > 0)
                                kgpc_type = create_primitive_type_with_size(INT_TYPE, storage_size);
                            else
                                kgpc_type = create_primitive_type(INT_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Real") ||
                                 pascal_identifier_equals(target_base, "Double") ||
                                 pascal_identifier_equals(target_base, "Single"))
                        {
                            kgpc_type = create_primitive_type(REAL_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Extended"))
                        {
                            kgpc_type = create_primitive_type(EXTENDED_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Boolean"))
                        {
                            kgpc_type = create_primitive_type(BOOL);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Char") ||
                                 pascal_identifier_equals(target_base, "AnsiChar"))
                        {
                            kgpc_type = create_primitive_type(CHAR_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        /* If target is another user-defined type, check if it's already declared */
                        else
                        {
                            HashNode_t *target_node = NULL;
                            char *lookup_target = NULL;
                            char *lookup_unqualified = NULL;
                            const char *lookup_name = target;
                            int has_qualification = 0;
                            if (target_ref != NULL)
                            {
                                lookup_target = type_ref_render_mangled(target_ref);
                                if (lookup_target != NULL)
                                    lookup_name = lookup_target;
                                if (target_ref->name != NULL && target_ref->name->count > 1)
                                    has_qualification = 1;
                            }

                            /* Handle qualified type names like "UnixType.culong" */
                            int found = FindSymbol(&target_node, symtab, lookup_name);
                            if (!found || target_node == NULL)
                            {
                                /* Try unqualified name if we have structured qualification */
                                if (has_qualification)
                                {
                                    lookup_unqualified = type_ref_render_mangled_unqualified(target_ref);
                                    if (lookup_unqualified != NULL)
                                    {
                                        lookup_name = lookup_unqualified;
                                        found = FindSymbol(&target_node, symtab, lookup_name);
                                    }
                                }
                            }
                            if (found && target_node != NULL && has_qualification &&
                                target_node->type != NULL &&
                                kgpc_type_get_type_alias(target_node->type) == alias)
                            {
                                target_node = semcheck_find_type_excluding_alias(symtab, lookup_name, alias);
                            }
                            free(lookup_target);
                            free(lookup_unqualified);
                            
                            if (found &&
                                target_node != NULL && target_node->hash_type == HASHTYPE_TYPE &&
                                target_node->type != NULL)
                            {
                                /* Target type already exists, retain and use it.
                                 * Flag that we're reusing an existing type so we don't
                                 * overwrite its alias info later. */
                                kgpc_type = target_node->type;
                                kgpc_type_retain(kgpc_type);
                            }
                            /* Otherwise, skip - can't resolve this type yet */
                        }
                    }
                    
                    /* Flag to track if we're reusing an existing type (vs creating new) */
                    int reusing_existing = (!created_new_type && kgpc_type != NULL &&
                        kgpc_type_get_type_alias(kgpc_type) != NULL &&
                        kgpc_type_get_type_alias(kgpc_type) != alias);
                    
                    if (kgpc_type != NULL)
                    {
                        if (kgpc_getenv("KGPC_DEBUG_PREDECLARE") != NULL)
                        {
                            fprintf(stderr, "[predeclare] SUCCESS type '%s' kgpc_type=%p kind=%d reusing=%d\n",
                                type_id, (void*)kgpc_type, kgpc_type->kind, reusing_existing);
                        }
                        
                        /* Only modify alias info for newly created types, not reused ones */
                        if (!reusing_existing)
                        {
                            /* IMPORTANT: Inherit storage_size from the original type's alias.
                             * This is critical for types like WideChar (2 bytes) where we retain
                             * the original type but need to preserve its custom storage_size. */
                            struct TypeAlias *original_alias = kgpc_type_get_type_alias(kgpc_type);
                            if (original_alias != NULL && original_alias->storage_size > 0 &&
                                alias->storage_size <= 0)
                            {
                                alias->storage_size = original_alias->storage_size;
                            }
                            
                            /* Set type_alias on KgpcType - only for newly created types */
                            kgpc_type_set_type_alias(kgpc_type, alias);
                        }
                        
                        /* Store in tree for later use by semcheck_type_decls */
                        if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                        {
                            tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                            kgpc_type_retain(kgpc_type);  /* Tree owns a reference */
                        }
                        
                        /* Push onto symbol table */
                        int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                        if (result > 0)
                        {
                            /* Should not happen since we checked above, but handle gracefully */
                            errors++;
                        }
                        else
                        {
                            mark_latest_type_node_unit_info(symtab, type_id,
                                tree->tree_data.type_decl_data.defined_in_unit,
                                tree->tree_data.type_decl_data.unit_is_public,
                                tree->tree_data.type_decl_data.source_unit_index);
                        }
                        /* Release creator's reference */
                        destroy_kgpc_type(kgpc_type);
                    }
                    else if (kgpc_getenv("KGPC_DEBUG_PREDECLARE") != NULL)
                    {
                        fprintf(stderr, "[predeclare] SKIP type '%s' - kgpc_type is NULL\n", type_id);
                    }
                }
                /* Skip TYPE_DECL_RECORD, TYPE_DECL_GENERIC, TYPE_DECL_RANGE - 
                 * these have complex dependencies and are handled by semcheck_type_decls */
predeclare_types_next:
                /* Ensure every record type_decl tree has kgpc_type set, even
                 * if the predeclare pass skipped it as a duplicate.  Without
                 * this, codegen falls back to info.record which may be a
                 * depleted shell after forward-declaration resolution. */
                if (tree->tree_data.type_decl_data.kgpc_type == NULL &&
                    tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                {
                    const char *tid = tree->tree_data.type_decl_data.id;
                    if (tid != NULL)
                    {
                        HashNode_t *canon_node = NULL;
                        if (FindSymbol(&canon_node, symtab, tid) &&
                            canon_node != NULL &&
                            canon_node->hash_type == HASHTYPE_TYPE &&
                            canon_node->type != NULL)
                        {
                            tree->tree_data.type_decl_data.kgpc_type = canon_node->type;
                            kgpc_type_retain(canon_node->type);
                        }
                    }
                }
                semcheck_restore_scope(symtab, saved_scope_for_type);
            }
        }
        cur = cur->next;
    }

    return errors;
}

/* Helper function to check if a statement contains an asm block */
__attribute__((unused)) static int statement_contains_asm_block(struct Statement *stmt)
{
    if (stmt == NULL)
        return 0;
    
    if (stmt->type == STMT_ASM_BLOCK)
        return 1;
    
    if (stmt->type == STMT_COMPOUND_STATEMENT)
    {
        ListNode_t *cur = stmt->stmt_data.compound_statement;
        while (cur != NULL)
        {
            if (cur->type == LIST_STMT && cur->cur != NULL)
            {
                struct Statement *child_stmt = (struct Statement *)cur->cur;
                if (statement_contains_asm_block(child_stmt))
                    return 1;
            }
            cur = cur->next;
        }
    }
    
    return 0;
}

static int record_has_property(struct RecordType *record_info, const char *property_name)
{
    if (record_info == NULL || property_name == NULL)
        return 0;

    ListNode_t *node = record_info->properties;
    while (node != NULL)
    {
        if (node->type == LIST_CLASS_PROPERTY && node->cur != NULL)
        {
            struct ClassProperty *property = (struct ClassProperty *)node->cur;
            if (property->name != NULL &&
                pascal_identifier_equals(property->name, property_name))
                return 1;
        }
        node = node->next;
    }
    return 0;
}

static struct ClassProperty *clone_class_property(const struct ClassProperty *property)
{
    if (property == NULL)
        return NULL;

    struct ClassProperty *clone = (struct ClassProperty *)calloc(1, sizeof(struct ClassProperty));
    if (clone == NULL)
        return NULL;

    clone->name = property->name != NULL ? strdup(property->name) : NULL;
    clone->type = property->type;
    clone->type_id = property->type_id != NULL ? strdup(property->type_id) : NULL;
    clone->type_ref = property->type_ref != NULL ? type_ref_clone(property->type_ref) : NULL;
    clone->read_accessor = property->read_accessor != NULL ? strdup(property->read_accessor) : NULL;
    clone->write_accessor = property->write_accessor != NULL ? strdup(property->write_accessor) : NULL;
    clone->is_indexed = property->is_indexed;
    clone->is_default = property->is_default;

    if ((property->name != NULL && clone->name == NULL) ||
        (property->type_id != NULL && clone->type_id == NULL) ||
        (property->read_accessor != NULL && clone->read_accessor == NULL) ||
        (property->write_accessor != NULL && clone->write_accessor == NULL))
    {
        free(clone->name);
        free(clone->type_id);
        if (clone->type_ref != NULL)
            type_ref_free(clone->type_ref);
        free(clone->read_accessor);
        free(clone->write_accessor);
        free(clone);
        return NULL;
    }

    return clone;
}

static ListNode_t *clone_property_list_unique(struct RecordType *record_info,
    const ListNode_t *properties)
{
    ListNode_t *head = NULL;
    ListNode_t **tail = &head;

    while (properties != NULL)
    {
        if (properties->type == LIST_CLASS_PROPERTY && properties->cur != NULL)
        {
            struct ClassProperty *property = (struct ClassProperty *)properties->cur;
            if (!record_has_property(record_info, property->name))
            {
                struct ClassProperty *clone = clone_class_property(property);
                if (clone != NULL)
                {
                    ListNode_t *node = CreateListNode(clone, LIST_CLASS_PROPERTY);
                    if (node != NULL)
                    {
                        *tail = node;
                        tail = &node->next;
                    }
                    else
                    {
                        free(clone->name);
                        free(clone->type_id);
                        free(clone->read_accessor);
                        free(clone->write_accessor);
                        free(clone);
                    }
                }
            }
        }
        properties = properties->next;
    }

    return head;
}

/* Helper function to check for circular inheritance */
static int check_circular_inheritance(SymTab_t *symtab, const char *class_name, const char *parent_name, int max_depth)
{
    if (class_name == NULL || parent_name == NULL)
        return 0;
    
    /* Check if parent is the same as this class (direct circular reference) */
    if (strcmp(class_name, parent_name) == 0)
        return 1;
    
    /* Prevent infinite recursion by limiting depth */
    if (max_depth <= 0)
        return 1;
    
    /* Look up parent class */
    HashNode_t *parent_node = NULL;
    if (FindSymbol(&parent_node, symtab, parent_name) == 0 || parent_node == NULL)
        return 0;  /* Parent not found yet, not necessarily circular */
    
    struct RecordType *parent_record = get_record_type_from_node(parent_node);
    if (parent_record == NULL || parent_record->parent_class_name == NULL)
        return 0;  /* Parent has no parent, no circular reference */
    
    /* Recursively check parent's parent */
    return check_circular_inheritance(symtab, class_name, parent_record->parent_class_name, max_depth - 1);
}

static int add_class_padding_field(struct RecordType *record_info, long long padding_bytes)
{
    if (record_info == NULL || padding_bytes <= 0)
        return 0;

    struct RecordField *padding = (struct RecordField *)calloc(1, sizeof(struct RecordField));
    if (padding == NULL)
        return 1;

    padding->name = NULL;
    padding->type = CHAR_TYPE;
    padding->is_array = 1;
    padding->array_start = 0;
    padding->array_end = (int)padding_bytes - 1;
    padding->array_element_type = CHAR_TYPE;
    padding->array_is_open = 0;
    padding->is_hidden = 1;

    ListNode_t *node = CreateListNode(padding, LIST_RECORD_FIELD);
    if (record_info->fields == NULL)
        record_info->fields = node;
    else
        record_info->fields = PushListNodeBack(record_info->fields, node);
    return 0;
}

static int compute_class_record_size(SymTab_t *symtab, struct RecordType *record_info,
    long long *size_out, int line_num)
{
    if (record_info == NULL || size_out == NULL)
        return 1;

    return semcheck_compute_record_size(symtab, record_info, size_out,
        line_num >= 0 ? line_num : 0);
}

static int ensure_class_storage_capacity(SymTab_t *symtab, struct RecordType *record_info,
    long long required_size, int line_num)
{
    if (record_info == NULL)
        return 0;

    long long current_size = 0;
    if (compute_class_record_size(symtab, record_info, &current_size, line_num) != 0)
        return 1;

    if (current_size >= required_size)
        return 0;

    long long padding_bytes = required_size - current_size;
    return add_class_padding_field(record_info, padding_bytes);
}

/*
 * Detect and populate default_indexed_property for a class.
 * This enables array-like indexing (obj[i]) to be transformed to field access (obj.field[i]).
 * 
 * Detection rules:
 * 1. If a class has an open array field (array_is_open == 1) and it's the only such field,
 *    or specifically named "FItems" (Delphi convention), use it as the default indexed property.
 * 2. Also inherit from parent class if parent has a default_indexed_property and child doesn't.
 * 
 * @param record_info The class RecordType to configure. Must not be NULL.
 * @param parent_record The parent class RecordType, or NULL for base classes like TObject.
 */
static void detect_default_indexed_property(struct RecordType *record_info, const struct RecordType *parent_record)
{
    assert(record_info != NULL);
    
    /* Already set - don't override */
    if (record_info->default_indexed_property != NULL)
        return;

    /* Prefer explicitly default indexed properties when available */
    for (int pass = 0; pass < 2; ++pass)
    {
        ListNode_t *prop_node = (pass == 0) ? record_info->properties : record_info->record_properties;
        while (prop_node != NULL)
        {
            if (prop_node->type == LIST_CLASS_PROPERTY && prop_node->cur != NULL)
            {
                struct ClassProperty *prop = (struct ClassProperty *)prop_node->cur;
                if (prop->is_default && prop->is_indexed && prop->name != NULL)
                {
                    record_info->default_indexed_property = strdup(prop->name);
                    record_info->default_indexed_element_type = prop->type;
                    record_info->default_indexed_element_type_id = prop->type_id != NULL ?
                        strdup(prop->type_id) : NULL;
                    return;
                }
            }
            prop_node = prop_node->next;
        }
    }
    
    /* First, try to detect from this class's fields */
    struct RecordField *candidate = NULL;
    ListNode_t *cur = record_info->fields;
    while (cur != NULL)
    {
        if (cur->type == LIST_RECORD_FIELD)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            if (field != NULL && field->name != NULL && field->is_array && field->array_is_open)
            {
                /* Found an open array field - it's a candidate for default indexed property */
                /* Prefer "FItems" if we find it (Delphi naming convention for list items) */
                if (pascal_identifier_equals(field->name, "FItems"))
                {
                    candidate = field;
                    break;  /* FItems is the preferred choice */
                }
                else if (candidate == NULL)
                {
                    /* First open array found - tentative candidate */
                    candidate = field;
                }
            }
        }
        cur = cur->next;
    }
    
    if (candidate != NULL)
    {
        record_info->default_indexed_property = strdup(candidate->name);
        record_info->default_indexed_element_type = candidate->array_element_type;
        record_info->default_indexed_element_type_id = candidate->array_element_type_id != NULL ?
            strdup(candidate->array_element_type_id) : NULL;
        return;
    }
    
    /* Inherit from parent if available */
    if (parent_record != NULL && parent_record->default_indexed_property != NULL)
    {
        record_info->default_indexed_property = strdup(parent_record->default_indexed_property);
        record_info->default_indexed_element_type = parent_record->default_indexed_element_type;
        record_info->default_indexed_element_type_id = parent_record->default_indexed_element_type_id != NULL ?
            strdup(parent_record->default_indexed_element_type_id) : NULL;
    }
}

/* Helper function to merge parent class fields into derived class */
static int merge_parent_class_fields(SymTab_t *symtab, struct RecordType *record_info, const char *class_name, int line_num)
{
    if (record_info == NULL || record_info->parent_class_name == NULL)
        return 0;  /* No parent class to merge */

    record_info->has_cached_size = 0;

    /* Guard against double-merging: if parent fields have already been
     * merged into this record, skip to avoid duplicating inherited fields.
     * Also check the canonical RecordType in the symbol table — if another
     * RecordType for the same class was already merged (e.g. forward decl
     * vs full decl creating separate RecordType objects), skip this one. */
    if (record_info->parent_fields_merged)
        return 0;
    if (class_name != NULL) {
        HashNode_t *self_node = semcheck_find_preferred_type_node(symtab, class_name);
        if (self_node != NULL) {
            struct RecordType *canonical = get_record_type_from_node(self_node);
            if (canonical != NULL && canonical != record_info && canonical->parent_fields_merged)
            {
                /* Canonical instance was already merged — propagate its merged
                 * state to this instance so codegen (which may use this instance
                 * via the Tree_t node) sees the correct fields and interfaces. */
                if (!record_info->parent_fields_merged) {
                    record_info->fields = canonical->fields;
                    record_info->num_interfaces = canonical->num_interfaces;
                    record_info->interface_names = canonical->interface_names;
                    record_info->properties = canonical->properties;
                    record_info->cached_size = canonical->cached_size;
                    record_info->has_cached_size = canonical->has_cached_size;
                    record_info->parent_fields_merged = 1;
                }
                return 0;
            }
        }
    }

    /* Check for circular inheritance */
    if (check_circular_inheritance(symtab, class_name, record_info->parent_class_name, 100))
    {
        semcheck_error_with_context("Error on line %d, circular inheritance detected for class '%s'!\n",
                line_num, class_name ? class_name : "<unknown>");
        return 1;
    }

    /* Look up parent class in symbol table */
    HashNode_t *parent_node = semcheck_find_preferred_type_node(symtab, record_info->parent_class_name);
    if (parent_node == NULL)
    {
        semcheck_error_with_context("Error on line %d, parent class '%s' not found!\n",
                line_num, record_info->parent_class_name);
        return 1;
    }

    /* Get parent's RecordType */
    struct RecordType *parent_record = get_record_type_from_node(parent_node);
    if (parent_record == NULL)
    {
        semcheck_error_with_context("Error on line %d, parent class '%s' is not a class/record type!\n",
                line_num, record_info->parent_class_name);
        return 1;
    }

    /* Ensure parent has its own parent fields merged before we clone from it.
     * Switch to the parent's unit scope so that the grandparent lookup
     * searches the parent unit's dependencies (e.g. if A uses B uses C,
     * and B's class inherits from C's class, we need C's scope visible). */
    if (parent_record->parent_class_name != NULL && !parent_record->parent_fields_merged)
    {
        ScopeNode *saved_scope_for_parent = NULL;
        if (parent_node->source_unit_index > 0)
            saved_scope_for_parent = semcheck_switch_to_unit_scope(symtab, parent_node->source_unit_index);
        int parent_merge_result = merge_parent_class_fields(symtab, parent_record,
            record_info->parent_class_name, line_num);
        if (saved_scope_for_parent != NULL)
            semcheck_restore_scope(symtab, saved_scope_for_parent);
        if (parent_merge_result > 0)
            return parent_merge_result;
    }

    /* Clone parent's fields and prepend them to this record's fields */
    ListNode_t *parent_fields = parent_record->fields;
    if (parent_fields != NULL)
    {
        /* We need to clone the parent's field list and prepend it */
        ListNode_t *cloned_parent_fields = NULL;
        ListNode_t *cur = parent_fields;
        ListNode_t *last_cloned = NULL;
        
        while (cur != NULL)
        {
            if (cur->type != LIST_RECORD_FIELD || cur->cur == NULL)
            {
                cur = cur->next;
                continue;
            }
            struct RecordField *original_field = (struct RecordField *)cur->cur;

            /* Skip hidden fields from parent if the record already has them
             * (e.g., __kgpc_class_typeinfo added to forward declarations). */
            if (record_field_is_hidden(original_field) && original_field->name != NULL)
            {
                int already_has = 0;
                ListNode_t *own = record_info->fields;
                while (own != NULL)
                {
                    if (own->type == LIST_RECORD_FIELD && own->cur != NULL)
                    {
                        struct RecordField *f = (struct RecordField *)own->cur;
                        if (record_field_is_hidden(f) && f->name != NULL &&
                            strcmp(f->name, original_field->name) == 0)
                        {
                            already_has = 1;
                            break;
                        }
                    }
                    own = own->next;
                }
                if (already_has)
                {
                    cur = cur->next;
                    continue;
                }
            }

            /* Clone the field */
            struct RecordField *cloned_field = (struct RecordField *)calloc(1, sizeof(struct RecordField));
            if (cloned_field == NULL)
            {
                /* Clean up previously allocated fields */
                while (cloned_parent_fields != NULL)
                {
                    ListNode_t *temp = cloned_parent_fields;
                    cloned_parent_fields = cloned_parent_fields->next;
                    struct RecordField *field = (struct RecordField *)temp->cur;
                    free(field->name);
                    free(field->type_id);
                    if (field->type_ref != NULL)
                        type_ref_free(field->type_ref);
                    free(field->array_element_type_id);
                    if (field->array_element_type_ref != NULL)
                        type_ref_free(field->array_element_type_ref);
                    if (field->pointer_type_ref != NULL)
                        type_ref_free(field->pointer_type_ref);
                    if (field->proc_type != NULL)
                        kgpc_type_release(field->proc_type);
                    free(field);
                    free(temp);
                }
                return 1;
            }
            
            cloned_field->name = original_field->name ? strdup(original_field->name) : NULL;
            cloned_field->type = original_field->type;
            cloned_field->type_id = original_field->type_id ? strdup(original_field->type_id) : NULL;
            cloned_field->type_ref = original_field->type_ref != NULL
                ? type_ref_clone(original_field->type_ref) : NULL;
            cloned_field->nested_record = original_field->nested_record;  /* Share the nested record */
            cloned_field->proc_type = original_field->proc_type;
            if (cloned_field->proc_type != NULL)
                kgpc_type_retain(cloned_field->proc_type);
            cloned_field->is_array = original_field->is_array;
            cloned_field->array_start = original_field->array_start;
            cloned_field->array_end = original_field->array_end;
            cloned_field->array_element_type = original_field->array_element_type;
            cloned_field->array_element_type_id = original_field->array_element_type_id ? 
                strdup(original_field->array_element_type_id) : NULL;
            cloned_field->array_element_type_ref = original_field->array_element_type_ref != NULL
                ? type_ref_clone(original_field->array_element_type_ref) : NULL;
            cloned_field->array_is_open = original_field->array_is_open;
            cloned_field->is_hidden = original_field->is_hidden;
            cloned_field->is_class_var = original_field->is_class_var;
            cloned_field->is_pointer = original_field->is_pointer;
            cloned_field->pointer_type = original_field->pointer_type;
            cloned_field->pointer_type_id = original_field->pointer_type_id ?
                strdup(original_field->pointer_type_id) : NULL;
            cloned_field->pointer_type_ref = original_field->pointer_type_ref != NULL
                ? type_ref_clone(original_field->pointer_type_ref) : NULL;
            /* array_element_record is zeroed by calloc — not cloned to avoid double-free */
            cloned_field->enum_literals = NULL;
            
            /* Create list node for cloned field */
            ListNode_t *new_node = (ListNode_t *)malloc(sizeof(ListNode_t));
            if (new_node == NULL)
            {
                free(cloned_field->name);
                free(cloned_field->type_id);
                if (cloned_field->type_ref != NULL)
                    type_ref_free(cloned_field->type_ref);
                free(cloned_field->array_element_type_id);
                if (cloned_field->array_element_type_ref != NULL)
                    type_ref_free(cloned_field->array_element_type_ref);
                if (cloned_field->pointer_type_ref != NULL)
                    type_ref_free(cloned_field->pointer_type_ref);
                if (cloned_field->proc_type != NULL)
                    kgpc_type_release(cloned_field->proc_type);
                free(cloned_field);
                
                /* Clean up previously allocated fields */
                while (cloned_parent_fields != NULL)
                {
                    ListNode_t *temp = cloned_parent_fields;
                    cloned_parent_fields = cloned_parent_fields->next;
                    struct RecordField *field = (struct RecordField *)temp->cur;
                    free(field->name);
                    free(field->type_id);
                    free(field->array_element_type_id);
                    if (field->proc_type != NULL)
                        kgpc_type_release(field->proc_type);
                    free(field);
                    free(temp);
                }
                return 1;
            }
            
            new_node->type = LIST_RECORD_FIELD;
            new_node->cur = cloned_field;
            new_node->next = NULL;
            
            /* Append to cloned list */
            if (cloned_parent_fields == NULL)
            {
                cloned_parent_fields = new_node;
                last_cloned = new_node;
            }
            else
            {
                last_cloned->next = new_node;
                last_cloned = new_node;
            }
            
            cur = cur->next;
        }
        
        /* Prepend cloned parent fields to this record's fields */
        if (last_cloned != NULL)
        {
            last_cloned->next = record_info->fields;
            record_info->fields = cloned_parent_fields;
        }
    }

    if (parent_record != NULL && parent_record->properties != NULL)
    {
        ListNode_t *cloned_properties = clone_property_list_unique(record_info,
            parent_record->properties);
        if (cloned_properties != NULL)
            record_info->properties = ConcatList(cloned_properties, record_info->properties);
    }

    /* Merge parent's interfaces into child's interface_names.
     * Only add interfaces the child doesn't already declare. */
    if (parent_record != NULL && parent_record->num_interfaces > 0)
    {
        for (int pi = 0; pi < parent_record->num_interfaces; pi++)
        {
            const char *parent_iface = parent_record->interface_names[pi];
            if (parent_iface == NULL) continue;
            int already_declared = 0;
            for (int ci = 0; ci < record_info->num_interfaces; ci++)
            {
                if (record_info->interface_names[ci] != NULL &&
                    strcasecmp(record_info->interface_names[ci], parent_iface) == 0)
                {
                    already_declared = 1;
                    break;
                }
            }
            if (!already_declared)
            {
                int new_count = record_info->num_interfaces + 1;
                char **new_names = (char **)realloc(record_info->interface_names,
                    new_count * sizeof(char *));
                if (new_names != NULL)
                {
                    new_names[new_count - 1] = strdup(parent_iface);
                    record_info->interface_names = new_names;
                    record_info->num_interfaces = new_count;
                }
            }
        }
    }

    /* Detect default indexed property after merging parent fields */
    detect_default_indexed_property(record_info, parent_record);

    record_info->parent_fields_merged = 1;

    /* Class instances are accessed via pointers; parent storage need not grow.
     * Only ensure capacity for non-class (plain record) inheritance. */
    if (parent_record != NULL && !record_info->is_class)
    {
        long long derived_size = 0;
        if (compute_class_record_size(symtab, record_info, &derived_size, line_num) == 0)
            ensure_class_storage_capacity(symtab, parent_record, derived_size, line_num);
    }

    return 0;
}

/* Build Virtual Method Table for a class */
