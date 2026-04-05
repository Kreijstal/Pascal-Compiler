static double semcheck_now_ms(void) {
    return (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
}

static void semcheck_timing_step(const char *label, double *last_ms) {
    if (!SEMCHECK_TIMINGS_ENABLED() || last_ms == NULL)
        return;
    double now = semcheck_now_ms();
    fprintf(stderr, "[timing] semcheck_program %s: %.2f ms\n", label, now - *last_ms);
    *last_ms = now;
}

/* Wire scope tree deps: add each unit in uses_list as a dependency of scope.
 * Also always adds System as a dependency. */
static void wire_scope_deps(SymTab_t *symtab, ScopeNode *scope, ListNode_t *uses_list)
{
    if (scope == NULL) return;

    /* Every scope implicitly depends on System */
    int sys_idx = unit_registry_add("System");
    if (sys_idx > 0)
        ScopeAddDependencyTransitive(scope, GetOrCreateUnitScope(symtab, sys_idx), 1);

    ListNode_t *cur = uses_list;
    while (cur != NULL)
    {
        if (cur->type == LIST_STRING && cur->cur != NULL)
        {
            const char *name = (const char *)cur->cur;
            int idx = unit_registry_add(name);
            if (idx > 0)
                ScopeAddDependencyTransitive(scope, GetOrCreateUnitScope(symtab, idx), 1);
        }
        cur = cur->next;
    }
}

/* Wire program scope deps to ALL loaded units.  This is needed because the
 * current architecture merges all units into the program tree, so subprograms
 * from any unit run under the program scope and need to see their own unit's
 * symbols through the dep_scopes chain. */
static void wire_program_scope_all_units(SymTab_t *symtab, ScopeNode *scope)
{
    if (scope == NULL) return;
    int count = unit_registry_count();
    for (int i = 1; i <= count; i++)
        ScopeAddDependency(scope, GetOrCreateUnitScope(symtab, i), 1);
}

int semcheck_program(SymTab_t *symtab, Tree_t *tree)
{
    int return_val;
    assert(tree != NULL);
    assert(symtab != NULL);
    assert(tree->type == TREE_PROGRAM_TYPE);

    return_val = 0;
    double t0 = 0.0;
    if (SEMCHECK_TIMINGS_ENABLED())
        t0 = semcheck_now_ms();

    EnterScope(symtab, 0);

    semcheck_unit_names_reset();
    semcheck_unit_name_add("System");
    semcheck_unit_names_add_list(tree->tree_data.program_data.uses_units);
    semcheck_timing_step("unit names", &t0);

    /* Wire scope tree deps: program scope can see its directly-used units
     * (matching FPC semantics — transitive deps are NOT visible).
     * Unit declarations switch to per-unit scopes via source_unit_index,
     * so each unit resolves its own deps independently. */
    wire_scope_deps(symtab, symtab->current_scope,
                    tree->tree_data.program_data.uses_units);

    return_val += semcheck_id_not_main(tree->tree_data.program_data.program_id);
    semcheck_timing_step("id check", &t0);

    /* TODO: Push program name onto scope */

    /* TODO: Fix line number bug here */
    return_val += semcheck_args(symtab, tree->tree_data.program_data.args_char,
      tree->line_num);
    semcheck_timing_step("args", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after args: %d\n", return_val);
#endif

    /* Predeclare types and enums.  These are idempotent -- they check for
     * existing entries and skip duplicates.  Unit-owned types are already
     * in their per-unit scopes from semcheck_unit(), but re-predeclaring
     * them here is harmless and ensures cross-references work. */
    return_val += predeclare_enum_literals(symtab, tree->tree_data.program_data.type_declaration);
    return_val += predeclare_types(symtab, tree->tree_data.program_data.type_declaration);
    semcheck_timing_step("predeclare types", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after type predeclare: %d\n", return_val);
#endif

    /* Predeclare subprograms so they can be referenced in const initializers.
     * Idempotent -- skips already-declared entries. */
    return_val += predeclare_subprograms(symtab, tree->tree_data.program_data.subprograms, 0, NULL);
    semcheck_timing_step("predeclare subprograms", &t0);
    if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
    {
        ListNode_t *debug_cur = tree->tree_data.program_data.type_declaration;
        while (debug_cur != NULL)
        {
            if (debug_cur->type == LIST_TREE && debug_cur->cur != NULL)
            {
                Tree_t *debug_tree = (Tree_t *)debug_cur->cur;
                if (debug_tree->type == TREE_TYPE_DECL && debug_tree->tree_data.type_decl_data.id != NULL)
                {
                    fprintf(stderr, "[KGPC] program type decl: %s kind=%d\n",
                        debug_tree->tree_data.type_decl_data.id,
                        debug_tree->tree_data.type_decl_data.kind);
                }
            }
            debug_cur = debug_cur->next;
        }
    }

    /* Four-pass processing for constants to handle all reference patterns:
     *
     * Pass 1: Imported unit untyped constants.
     * Pass 2: Imported unit typed constants (e.g., DirectorySeparator from system.p).
     * Pass 3: Local untyped constants.
     * Pass 4: Local typed constants.
     *
     * Note: trivial imported constants (literal ints, reals, strings) are already
     * pre-pushed into per-unit scopes during unit loading (semcheck_unit_decls_only)
     * and System predeclaration (semcheck_predeclare_program_into_unit_scope).
     * Cross-unit resolution uses scope dependency edges, not list concatenation order.
     */

    /* Pass 1: Imported unit untyped constants */
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.program_data.const_declaration, 1);
    semcheck_timing_step("consts pass1 imported untyped", &t0);

    /* Pass 2: Imported unit typed constants */
    ListNode_t *unit_typed_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.program_data.var_declaration, 1);  /* from_unit_only=true */
    if (unit_typed_consts != NULL)
    {
        return_val += semcheck_decls(symtab, unit_typed_consts);
        DestroyList(unit_typed_consts);
    }
    semcheck_timing_step("consts pass2 imported typed", &t0);

    /* Pass 3: Local untyped constants */
    return_val += semcheck_const_decls_local(symtab, tree->tree_data.program_data.const_declaration);

    /* Pass 3b: Deferred imported constants (qualified refs to current unit) */
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.program_data.const_declaration, 0);
    semcheck_timing_step("consts pass3 local untyped", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after consts: %d\n", return_val);
#endif

    /* Pass 4: Local typed constants (can reference both unit typed consts and local untyped consts) */
    ListNode_t *local_typed_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.program_data.var_declaration, 0);  /* from_unit_only=false */
    if (local_typed_consts != NULL)
    {
        return_val += semcheck_decls(symtab, local_typed_consts);
        DestroyList(local_typed_consts);
    }
    semcheck_timing_step("consts pass4 local typed", &t0);

    return_val += semcheck_type_decls(symtab, tree->tree_data.program_data.type_declaration);
    semcheck_timing_step("type decls", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after types: %d\n", return_val);
#endif

    ListNode_t *program_vars = collect_non_typed_var_decls(tree->tree_data.program_data.var_declaration);
    if (program_vars != NULL)
    {
        return_val += semcheck_decls(symtab, program_vars);
        DestroyList(program_vars);
    }
    semcheck_timing_step("var decls", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after vars: %d\n", return_val);
#endif

    return_val += semcheck_subprograms(symtab, tree->tree_data.program_data.subprograms, 0, NULL);
    semcheck_timing_step("subprograms", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after subprograms: %d\n", return_val);
#endif

    semcheck_refresh_generic_specialization_vmts(
        symtab, tree->tree_data.program_data.type_declaration);
    semcheck_timing_step("refresh generic vmts", &t0);

    return_val += semcheck_stmt(symtab, tree->tree_data.program_data.body_statement, INT_MAX);
    semcheck_timing_step("body", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after body: %d\n", return_val);
#endif

    /* Semantic check unit initialization and finalization blocks from loaded units */
    {
        CompilationContext *comp_ctx = compilation_context_get_active();
        if (comp_ctx != NULL) {
            for (int ui = 0; ui < comp_ctx->loaded_unit_count; ++ui) {
                Tree_t *unit_tree = comp_ctx->loaded_units[ui].unit_tree;
                if (unit_tree == NULL || unit_tree->type != TREE_UNIT)
                    continue;
                int saved_suppress = g_semcheck_error_suppress_source_index;
                const char *saved_unit_ctx = g_semcheck_error_unit_name;
                int unit_idx = comp_ctx->loaded_units[ui].unit_idx;
                if (unit_idx > 0) {
                    const char *uname = unit_registry_get(unit_idx);
                    g_semcheck_error_unit_name = uname ? uname : "<unit>";
                    g_semcheck_error_suppress_source_index = 0;
                }
                /* Switch to the unit's scope so that unit-private variables
                 * and types are visible in init/finalization sections. */
                ScopeNode *saved_scope = semcheck_switch_to_unit_scope(symtab, unit_idx);
                if (unit_tree->tree_data.unit_data.initialization != NULL)
                    return_val += semcheck_stmt(symtab, unit_tree->tree_data.unit_data.initialization, INT_MAX);
                if (unit_tree->tree_data.unit_data.finalization != NULL)
                    return_val += semcheck_stmt(symtab, unit_tree->tree_data.unit_data.finalization, INT_MAX);
                semcheck_restore_scope(symtab, saved_scope);
                g_semcheck_error_suppress_source_index = saved_suppress;
                g_semcheck_error_unit_name = saved_unit_ctx;
            }
        }
    }
    semcheck_timing_step("finalization", &t0);

    if(optimize_flag() > 0 && return_val == 0)
    {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] Before optimize: body_statement = %p\n",
                    (void*)tree->tree_data.program_data.body_statement);
            if (tree->tree_data.program_data.body_statement != NULL) {
                fprintf(stderr, "[KGPC] Body statement type: %d\n",
                        tree->tree_data.program_data.body_statement->type);
                if (tree->tree_data.program_data.body_statement->type == STMT_COMPOUND_STATEMENT) {
                    fprintf(stderr, "[KGPC] Compound statement list: %p\n",
                            (void*)tree->tree_data.program_data.body_statement->stmt_data.compound_statement);
                }
            }
        }
        optimize(symtab, tree);
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] After optimize: body_statement = %p\n",
                    (void*)tree->tree_data.program_data.body_statement);
        }
    }

    /* After semcheck, wire all units to the program scope so that codegen
     * (which uses skip_unit_filter=1) can find symbols from any unit.
     * During semcheck above, only directly-used units were wired (FPC semantics). */
    wire_program_scope_all_units(symtab, symtab->current_scope);

    /* Keep the outermost scope alive for code generation. DestroySymTab will clean it up. */
    semcheck_unit_names_reset();
    return return_val;
}

/* Semantic check for a unit */
int semcheck_unit(SymTab_t *symtab, Tree_t *tree)
{
    int return_val;
    const char *debug_steps = kgpc_getenv("KGPC_DEBUG_SEMSTEPS");
    assert(tree != NULL);
    assert(symtab != NULL);
    assert(tree->type == TREE_UNIT);

    return_val = 0;

    EnterScope(symtab, 0);

    semcheck_unit_names_reset();
    semcheck_unit_name_add("System");
    semcheck_unit_name_add(tree->tree_data.unit_data.unit_id);
    if (tree->tree_data.unit_data.unit_id != NULL)
        g_semcheck_current_unit_index = unit_registry_add(tree->tree_data.unit_data.unit_id);
    symtab->current_unit_index = g_semcheck_current_unit_index;
    /* Fix up unit_index now that we know it */
    symtab->current_scope->unit_index = g_semcheck_current_unit_index;
    semcheck_unit_names_add_list(tree->tree_data.unit_data.interface_uses);
    semcheck_unit_names_add_list(tree->tree_data.unit_data.implementation_uses);
    {
        int sys_idx = unit_registry_add("System");
        if (sys_idx > 0)
            ScopeAddDependency(symtab->current_scope,
                               GetOrCreateUnitScope(symtab, sys_idx), 1);
    }

    /* Wire scope tree deps: unit scope can see interface + implementation uses */
    wire_scope_deps(symtab, symtab->current_scope,
                    tree->tree_data.unit_data.interface_uses);
    /* Also add implementation uses as deps */
    {
        ListNode_t *cur = tree->tree_data.unit_data.implementation_uses;
        while (cur != NULL)
        {
            if (cur->type == LIST_STRING && cur->cur != NULL)
            {
                const char *name = (const char *)cur->cur;
                int idx = unit_registry_add(name);
                if (idx > 0)
                    ScopeAddDependencyTransitive(symtab->current_scope,
                                       GetOrCreateUnitScope(symtab, idx), 0);
            }
            cur = cur->next;
        }
    }

    /* Wire the unit's own scope table as a dep of this UNIT scope so that
     * FindIdent_Tree can see the stable per-unit declarations in
     * unit_scopes[unit_idx]->table. This ensures that type lookups during
     * semcheck_decls/semcheck_subprogram can find per-unit types
     * (e.g. TResourceStringTableList in objpas).
     *
     * Also wire the unit's dependencies (interface + implementation uses)
     * into the stable own_unit_scope so that semcheck_switch_to_unit_scope
     * (which sets current_scope = unit_scopes[unit_idx]) can find symbols
     * from dependencies (e.g. PtrUInt from System) when processing
     * declarations and procedure bodies. */
    if (g_semcheck_current_unit_index > 0)
    {
        ScopeNode *own_unit_scope = GetOrCreateUnitScope(symtab, g_semcheck_current_unit_index);
        if (own_unit_scope != NULL && symtab->current_scope != NULL)
        {
            ScopeAddDependency(symtab->current_scope, own_unit_scope, 1);
            {
                int sys_idx = unit_registry_add("System");
                if (sys_idx > 0)
                    ScopeAddDependency(own_unit_scope,
                                       GetOrCreateUnitScope(symtab, sys_idx), 1);
            }
            /* Wire the interface + implementation uses as deps of own_unit_scope */
            wire_scope_deps(symtab, own_unit_scope,
                            tree->tree_data.unit_data.interface_uses);
            {
                ListNode_t *dep_cur = tree->tree_data.unit_data.implementation_uses;
                while (dep_cur != NULL)
                {
                    if (dep_cur->type == LIST_STRING && dep_cur->cur != NULL)
                    {
                        const char *dep_name = (const char *)dep_cur->cur;
                        int dep_idx = unit_registry_add(dep_name);
                        if (dep_idx > 0)
                            ScopeAddDependencyTransitive(own_unit_scope,
                                               GetOrCreateUnitScope(symtab, dep_idx), 0);
                    }
                    dep_cur = dep_cur->next;
                }
            }
        }
    }

    semcheck_mark_type_decl_units(tree->tree_data.unit_data.interface_type_decls,
        g_semcheck_current_unit_index);
    semcheck_mark_type_decl_units(tree->tree_data.unit_data.implementation_type_decls,
        g_semcheck_current_unit_index);
    semcheck_mark_subprogram_units(tree->tree_data.unit_data.subprograms,
        g_semcheck_current_unit_index);
    /* Mark const and var decls so source_unit_index is set for the unit's own decls
     * (defined_in_unit=0). That lets declaration processing switch directly to the
     * correct unit scope when source_unit_index > 0. */
    semcheck_mark_const_decl_units(tree->tree_data.unit_data.interface_const_decls,
        g_semcheck_current_unit_index);
    semcheck_mark_const_decl_units(tree->tree_data.unit_data.implementation_const_decls,
        g_semcheck_current_unit_index);
    semcheck_mark_var_decl_units(tree->tree_data.unit_data.interface_var_decls,
        g_semcheck_current_unit_index);
    semcheck_mark_var_decl_units(tree->tree_data.unit_data.implementation_var_decls,
        g_semcheck_current_unit_index);

    /* Check interface section */
    int before = return_val;
    return_val += predeclare_enum_literals(symtab, tree->tree_data.unit_data.interface_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface enum predeclare +%d (total %d)\n",
                return_val - before, return_val);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    before = return_val;
    return_val += predeclare_types(symtab, tree->tree_data.unit_data.interface_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface type predeclare +%d (total %d)\n",
                return_val - before, return_val);
    
    /* Check implementation section - predeclare types BEFORE subprograms */
    before = return_val;
    return_val += predeclare_enum_literals(symtab, tree->tree_data.unit_data.implementation_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl enum predeclare +%d (total %d)\n",
                return_val - before, return_val);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    before = return_val;
    return_val += predeclare_types(symtab, tree->tree_data.unit_data.implementation_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl type predeclare +%d (total %d)\n",
                return_val - before, return_val);
    
    /* Now predeclare subprograms AFTER all types (interface + implementation) are predeclared.
     * This ensures function return types referencing implementation types can be resolved.
     * The same subprograms list is shared by interface and implementation. */
    before = return_val;

    /* Debug: dump subprograms list before predeclaration */
    if (kgpc_getenv("KGPC_DEBUG_SUBPROGRAMS_LIST") != NULL)
    {
        fprintf(stderr, "[SUBPROGRAMS_LIST] Dumping subprograms list for unit:\n");
        ListNode_t *debug_cur = tree->tree_data.unit_data.subprograms;
        int count = 0;
        while (debug_cur != NULL)
        {
            count++;
            if (debug_cur->type == LIST_TREE && debug_cur->cur != NULL)
            {
                Tree_t *sub = (Tree_t *)debug_cur->cur;
                if (sub->type == TREE_SUBPROGRAM)
                {
                    fprintf(stderr, "[SUBPROGRAMS_LIST] %d: %s (line %d)\n",
                            count, sub->tree_data.subprogram_data.id, sub->line_num);
                }
            }
            debug_cur = debug_cur->next;
        }
        fprintf(stderr, "[SUBPROGRAMS_LIST] Total: %d subprograms\n", count);
    }

    return_val += predeclare_subprograms(symtab, tree->tree_data.unit_data.subprograms, 0, NULL);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] subprogram predeclare +%d (total %d)\n",
                return_val - before, return_val);
    
    /* Continue interface section processing */
    /* Pre-push trivially evaluable imported consts for cross-unit forward references */
    prepush_trivial_imported_consts(symtab, tree->tree_data.unit_data.interface_const_decls, 0);
    /* Pass 1: Imported unit untyped constants. */
    before = return_val;
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.unit_data.interface_const_decls, 1);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 2: Imported unit typed constants (e.g., prelude/system typed consts). */
    before = return_val;
    ListNode_t *typed_iface_unit_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.interface_var_decls, 1);
    if (typed_iface_unit_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_iface_unit_consts);
        DestroyList(typed_iface_unit_consts);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface typed unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 3: Local untyped const declarations. */
    before = return_val;
    return_val += semcheck_const_decls_local(symtab, tree->tree_data.unit_data.interface_const_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface consts +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.unit_data.interface_const_decls, 0);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface deferred imported consts +%d (total %d)\n",
                return_val - before, return_val);
                
    /* Pass 4: Local interface typed constants - they can reference regular constants */
    before = return_val;
    ListNode_t *typed_iface_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.interface_var_decls, 0);
    if (typed_iface_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_iface_consts);
        DestroyList(typed_iface_consts);
    }
    before = return_val;
    return_val += semcheck_type_decls(symtab, tree->tree_data.unit_data.interface_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface types +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    ListNode_t *iface_vars = collect_non_typed_var_decls(tree->tree_data.unit_data.interface_var_decls);
    if (iface_vars != NULL)
    {
        return_val += semcheck_decls(symtab, iface_vars);
        DestroyList(iface_vars);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface vars +%d (total %d)\n",
                return_val - before, return_val);

    /* Continue implementation section processing */
    /* Pre-push trivially evaluable imported consts from implementation */
    prepush_trivial_imported_consts(symtab, tree->tree_data.unit_data.implementation_const_decls, 0);
    /* Pass 1: Imported unit untyped constants from implementation section. */
    before = return_val;
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.unit_data.implementation_const_decls, 1);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 2: Imported unit typed constants from implementation section. */
    before = return_val;
    ListNode_t *typed_impl_unit_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.implementation_var_decls, 1);
    if (typed_impl_unit_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_impl_unit_consts);
        DestroyList(typed_impl_unit_consts);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl typed unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 3: Local untyped const declarations. */
    before = return_val;
    return_val += semcheck_const_decls_local(symtab, tree->tree_data.unit_data.implementation_const_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl consts +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.unit_data.implementation_const_decls, 0);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl deferred imported consts +%d (total %d)\n",
                return_val - before, return_val);
                
    /* Pass 4: Local implementation typed constants */
    before = return_val;
    ListNode_t *typed_impl_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.implementation_var_decls, 0);
    if (typed_impl_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_impl_consts);
        DestroyList(typed_impl_consts);
    }
    before = return_val;
    return_val += semcheck_type_decls(symtab, tree->tree_data.unit_data.implementation_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl types +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    ListNode_t *impl_vars = collect_non_typed_var_decls(tree->tree_data.unit_data.implementation_var_decls);
    if (impl_vars != NULL)
    {
        return_val += semcheck_decls(symtab, impl_vars);
        DestroyList(impl_vars);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl vars +%d (total %d)\n",
                return_val - before, return_val);

    /* Check subprograms */
    before = return_val;
    return_val += semcheck_subprograms(symtab, tree->tree_data.unit_data.subprograms, 0, NULL);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] subprograms +%d (total %d)\n",
                return_val - before, return_val);

    /* Check initialization section if present */
    if (tree->tree_data.unit_data.initialization != NULL) {
        before = return_val;
        return_val += semcheck_stmt(symtab, tree->tree_data.unit_data.initialization, INT_MAX);
        if (debug_steps != NULL && return_val != before)
            fprintf(stderr, "[SemCheck] initialization +%d (total %d)\n",
                    return_val - before, return_val);
    }

    /* Check finalization section if present */
    if (tree->tree_data.unit_data.finalization != NULL) {
        before = return_val;
        return_val += semcheck_stmt(symtab, tree->tree_data.unit_data.finalization, INT_MAX);
        if (debug_steps != NULL && return_val != before)
            fprintf(stderr, "[SemCheck] finalization +%d (total %d)\n",
                    return_val - before, return_val);
    }

    semcheck_unit_names_reset();
    return return_val;
}

/* Lightweight per-unit semantic check: pre-declares types, enum literals,
 * and subprogram signatures into the per-unit symbol tables. Does NOT do
 * full type resolution, constant evaluation, or body checking -- those are
 * handled later by semcheck_program() on the merged tree.
 * Enters a SCOPE_UNIT scope; caller must call LeaveScope() after this returns. */
void semcheck_predeclare_program_into_unit_scope(SymTab_t *symtab, Tree_t *program_tree,
    int unit_idx)
{
    assert(symtab != NULL);
    assert(program_tree != NULL);
    assert(program_tree->type == TREE_PROGRAM_TYPE);
    if (unit_idx <= 0)
        return;

    /* Switch to the unit's scope for predeclaration */
    ScopeNode *saved_scope = symtab->current_scope;
    ScopeNode *unit_scope = GetOrCreateUnitScope(symtab, unit_idx);
    symtab->current_scope = unit_scope;

    EnterScope(symtab, 0);

    /* Predeclare types and enum literals */
    predeclare_enum_literals(symtab, program_tree->tree_data.program_data.type_declaration);
    predeclare_types(symtab, program_tree->tree_data.program_data.type_declaration);

    /* Predeclare subprogram signatures */
    predeclare_subprograms(symtab, program_tree->tree_data.program_data.subprograms, 0, NULL);

    /* Pre-push trivially evaluable constants */
    prepush_trivial_imported_consts(symtab, program_tree->tree_data.program_data.const_declaration, 1);

    LeaveScope(symtab);
    symtab->current_scope = saved_scope;
}

int semcheck_unit_decls_only(SymTab_t *symtab, Tree_t *tree)
{
    int return_val;
    assert(tree != NULL);
    assert(symtab != NULL);
    assert(tree->type == TREE_UNIT);

    return_val = 0;

    EnterScope(symtab, 0);

    semcheck_unit_names_reset();
    semcheck_unit_name_add("System");
    semcheck_unit_name_add(tree->tree_data.unit_data.unit_id);
    if (tree->tree_data.unit_data.unit_id != NULL)
        g_semcheck_current_unit_index = unit_registry_add(tree->tree_data.unit_data.unit_id);
    symtab->current_unit_index = g_semcheck_current_unit_index;
    /* Fix up unit_index now that we know it */
    symtab->current_scope->unit_index = g_semcheck_current_unit_index;
    semcheck_unit_names_add_list(tree->tree_data.unit_data.interface_uses);
    semcheck_unit_names_add_list(tree->tree_data.unit_data.implementation_uses);
    {
        int sys_idx = unit_registry_add("System");
        if (sys_idx > 0)
            ScopeAddDependency(symtab->current_scope,
                               GetOrCreateUnitScope(symtab, sys_idx), 1);
    }

    /* Wire scope tree deps: unit scope can see interface + implementation uses */
    wire_scope_deps(symtab, symtab->current_scope,
                    tree->tree_data.unit_data.interface_uses);
    {
        ListNode_t *cur = tree->tree_data.unit_data.implementation_uses;
        while (cur != NULL)
        {
            if (cur->type == LIST_STRING && cur->cur != NULL)
            {
                const char *name = (const char *)cur->cur;
                int idx = unit_registry_add(name);
                if (idx > 0)
                    ScopeAddDependencyTransitive(symtab->current_scope,
                                       GetOrCreateUnitScope(symtab, idx), 0);
            }
            cur = cur->next;
        }
    }

    semcheck_mark_type_decl_units(tree->tree_data.unit_data.interface_type_decls,
        g_semcheck_current_unit_index);
    semcheck_mark_type_decl_units(tree->tree_data.unit_data.implementation_type_decls,
        g_semcheck_current_unit_index);
    semcheck_mark_subprogram_units(tree->tree_data.unit_data.subprograms,
        g_semcheck_current_unit_index);
    /* Mark const and var decls so that source_unit_index is set even for the
     * unit's own decls (defined_in_unit=0). This lets declaration processing
     * switch directly to the correct unit scope when source_unit_index > 0. */
    semcheck_mark_const_decl_units(tree->tree_data.unit_data.interface_const_decls,
        g_semcheck_current_unit_index);
    semcheck_mark_const_decl_units(tree->tree_data.unit_data.implementation_const_decls,
        g_semcheck_current_unit_index);
    semcheck_mark_var_decl_units(tree->tree_data.unit_data.interface_var_decls,
        g_semcheck_current_unit_index);
    semcheck_mark_var_decl_units(tree->tree_data.unit_data.implementation_var_decls,
        g_semcheck_current_unit_index);

    /* Wire the unit's own scope table as a dep of this UNIT scope so that
     * FindAllIdents_Tree can see the stable declarations in
     * unit_scopes[unit_idx]->table. Without this, predeclare_subprogram's duplicate
     * detection misses entries already in the unit table, creating duplicates.
     *
     * Also wire the unit's dependencies (interface + implementation uses) into
     * the stable own_unit_scope so that semcheck_subprogram, which sets
     * current_scope = unit_scopes[unit_idx], can find symbols from dependencies
     * (e.g. Argv, argc from System) when processing the unit's procedure bodies. */
    if (g_semcheck_current_unit_index > 0)
    {
        ScopeNode *own_unit_scope = GetOrCreateUnitScope(symtab, g_semcheck_current_unit_index);
        if (own_unit_scope != NULL && symtab->current_scope != NULL)
        {
            ScopeAddDependency(symtab->current_scope, own_unit_scope, 1);
            {
                int sys_idx = unit_registry_add("System");
                if (sys_idx > 0)
                    ScopeAddDependency(own_unit_scope,
                                       GetOrCreateUnitScope(symtab, sys_idx), 1);
            }
            /* Wire the interface + implementation uses as deps of own_unit_scope */
            wire_scope_deps(symtab, own_unit_scope,
                            tree->tree_data.unit_data.interface_uses);
            {
                ListNode_t *dep_cur = tree->tree_data.unit_data.implementation_uses;
                while (dep_cur != NULL)
                {
                    if (dep_cur->type == LIST_STRING && dep_cur->cur != NULL)
                    {
                        const char *dep_name = (const char *)dep_cur->cur;
                        int dep_idx = unit_registry_add(dep_name);
                        if (dep_idx > 0)
                            ScopeAddDependencyTransitive(own_unit_scope,
                                               GetOrCreateUnitScope(symtab, dep_idx), 0);
                    }
                    dep_cur = dep_cur->next;
                }
            }
        }
    }

    /* Only predeclare types (push names) and enum literals -- this is lightweight
     * and doesn't conflict with the later full processing by semcheck_program. */
    return_val += predeclare_enum_literals(symtab, tree->tree_data.unit_data.interface_type_decls);
    return_val += predeclare_types(symtab, tree->tree_data.unit_data.interface_type_decls);
    return_val += predeclare_enum_literals(symtab, tree->tree_data.unit_data.implementation_type_decls);
    return_val += predeclare_types(symtab, tree->tree_data.unit_data.implementation_type_decls);

    /* Predeclare subprogram signatures (no bodies) */
    return_val += predeclare_subprograms(symtab, tree->tree_data.unit_data.subprograms, 0, NULL);

    /* Pre-push trivially evaluable constants (just integer/string values) */
    prepush_trivial_imported_consts(symtab, tree->tree_data.unit_data.interface_const_decls, 1);
    prepush_trivial_imported_consts(symtab, tree->tree_data.unit_data.implementation_const_decls, 1);

    /* Decls-only also needs typed const declarations lowered from const sections
     * into interface/implementation var/array decls. Without this, dependent units
     * can miss typed const arrays like gas_needsuffix during semantic analysis. */
    {
        ListNode_t *typed_iface_unit_consts = collect_typed_const_decls_filtered(symtab,
            tree->tree_data.unit_data.interface_var_decls, 1);
        if (typed_iface_unit_consts != NULL)
        {
            return_val += semcheck_decl_only_typed_const_decls(symtab, typed_iface_unit_consts);
            DestroyList(typed_iface_unit_consts);
        }

        ListNode_t *typed_iface_consts = collect_typed_const_decls_filtered(symtab,
            tree->tree_data.unit_data.interface_var_decls, 0);
        if (typed_iface_consts != NULL)
        {
            return_val += semcheck_decl_only_typed_const_decls(symtab, typed_iface_consts);
            DestroyList(typed_iface_consts);
        }
    }
    {
        ListNode_t *typed_impl_unit_consts = collect_typed_const_decls_filtered(symtab,
            tree->tree_data.unit_data.implementation_var_decls, 1);
        if (typed_impl_unit_consts != NULL)
        {
            return_val += semcheck_decl_only_typed_const_decls(symtab, typed_impl_unit_consts);
            DestroyList(typed_impl_unit_consts);
        }

        ListNode_t *typed_impl_consts = collect_typed_const_decls_filtered(symtab,
            tree->tree_data.unit_data.implementation_var_decls, 0);
        if (typed_impl_consts != NULL)
        {
            return_val += semcheck_decl_only_typed_const_decls(symtab, typed_impl_consts);
            DestroyList(typed_impl_consts);
        }
    }

    semcheck_unit_names_reset();
    return return_val;
}


/* Adds arguments to the symbol table */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num)
{
    ListNode_t *cur;
    int return_val, func_return = 0;
    assert(symtab != NULL);

    return_val = 0;

    cur = args;

    /* Checking if they are declarations
        NOTE: Mismatching arg types is an error */
    if(cur != NULL)
        if(cur->type == LIST_TREE)
            return semcheck_decls(symtab, args);

    while(cur != NULL)
    {
        /* If not a list of declarations, must be a list of strings */
        assert(cur->type == LIST_STRING);

        /* UNTYPED procedure parameters - use NULL KgpcType */
        func_return = PushVarOntoScope_Typed(symtab, (char *)cur->cur, NULL);

        /* Greater than 0 signifies an error */
        if(func_return > 0)
        {
            semcheck_error_with_context("Error on line %d, redeclaration of name %s!\n",
                line_num, (char *)cur->cur);
            return_val += func_return;
        }

        cur = cur->next;
    }

    return return_val;
}

/* Pushes a bunch of declarations onto the current scope */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls)
{
    ListNode_t *cur, *ids, *ids_head;
    Tree_t *tree;
    int return_val, func_return = 0;

    enum VarType var_type;

    assert(symtab != NULL);

    return_val = 0;
    cur = decls;
    while(cur != NULL)
    {
        /* Any declaration is always a tree */
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_VAR_DECL || tree->type == TREE_ARR_DECL);

        int src_unit_idx = (tree->type == TREE_VAR_DECL)
            ? tree->tree_data.var_decl_data.source_unit_index
            : tree->tree_data.arr_decl_data.source_unit_index;
        ScopeNode *saved_scope_for_var = NULL;
        if (src_unit_idx > 0 && g_semcheck_current_subprogram == NULL)
            saved_scope_for_var = semcheck_switch_to_unit_scope(symtab, src_unit_idx);

        if (tree->type == TREE_VAR_DECL)
            ids_head = tree->tree_data.var_decl_data.ids;
        else
            ids_head = tree->tree_data.arr_decl_data.ids;

        ids = ids_head;

        HashNode_t *resolved_type = NULL;
        int owner_type_match = 0;
        const TypeRef *decl_type_ref = NULL;
        const char *decl_type_id = NULL;
        const char *decl_type_base = NULL;
        if (tree->type == TREE_VAR_DECL)
        {
            decl_type_ref = tree->tree_data.var_decl_data.type_ref;
            decl_type_id = tree->tree_data.var_decl_data.type_id;
            decl_type_base = (decl_type_ref != NULL)
                ? type_ref_base_name(decl_type_ref)
                : decl_type_id;
        }
        if (tree->type == TREE_VAR_DECL && decl_type_id != NULL)
        {
            if (kgpc_getenv("KGPC_DEBUG_PSHORTSTRING") != NULL &&
                pascal_identifier_equals(decl_type_id, "pshortstring"))
            {
                int count = 0;
                ListNode_t *matches = FindAllIdents(symtab, decl_type_id);
                for (ListNode_t *m = matches; m != NULL; m = m->next)
                {
                    HashNode_t *n = (HashNode_t *)m->cur;
                    if (n != NULL && n->hash_type == HASHTYPE_TYPE)
                    {
                        fprintf(stderr,
                            "[PShortString] lookup match id=%s type=%p defined_in_unit=%d source_unit_index=%d\n",
                            n->id ? n->id : "<null>", (void *)n->type,
                            n->defined_in_unit, n->source_unit_index);
                        count++;
                    }
                }
                if (matches != NULL)
                    DestroyList(matches);
                fprintf(stderr, "[PShortString] lookup total matches=%d\n", count);
            }
            if (kgpc_getenv("KGPC_DEBUG_TFLOAT") != NULL &&
                (pascal_identifier_equals(decl_type_id, "TFloatFormatProfile") ||
                 pascal_identifier_equals(decl_type_id, "TReal_Type")))
            {
                int count = 0;
                ListNode_t *matches = FindAllIdents(symtab, decl_type_id);
                for (ListNode_t *m = matches; m != NULL; m = m->next)
                {
                    HashNode_t *n = (HashNode_t *)m->cur;
                    if (n != NULL && n->hash_type == HASHTYPE_TYPE)
                    {
                        fprintf(stderr,
                            "[TFLOAT] lookup match id=%s type=%p defined_in_unit=%d source_unit_index=%d\n",
                            n->id ? n->id : "<null>", (void *)n->type,
                            n->defined_in_unit, n->source_unit_index);
                        count++;
                    }
                }
                if (matches != NULL)
                    DestroyList(matches);
                fprintf(stderr, "[TFLOAT] lookup total matches=%d for %s\n",
                    count, decl_type_id);
            }
            if (tree->tree_data.var_decl_data.defined_in_unit)
            {
                /* Imported declarations must stay bound to imported symbols.
                 * When the subprogram's source unit is known, prefer a type
                 * that was visible at the function's declaration site:
                 *  1) Exact match: type from the same source unit
                 *  2) Closest ancestor: type with highest source_unit_index
                 *     that is <= the function's source unit index (i.e., a
                 *     type from a unit loaded before/with the function's unit)
                 *  3) Generic fallback */
                resolved_type = NULL;
                if (g_semcheck_imported_decl_unit_index > 0 && decl_type_id != NULL)
                {
                    HashNode_t *closest_ancestor = NULL;
                    int closest_idx = 0;
                    ListNode_t *candidates = FindAllIdents(symtab, decl_type_id);
                    ListNode_t *c = candidates;
                    int debug_imported = (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL &&
                                         pascal_identifier_equals(decl_type_id, "TSize"));
                    if (debug_imported)
                        fprintf(stderr, "[IMPORTED_DECL] TSize lookup, imported_unit_idx=%d\n",
                            g_semcheck_imported_decl_unit_index);
                    while (c != NULL)
                    {
                        HashNode_t *n = (HashNode_t *)c->cur;
                        if (n != NULL && n->hash_type == HASHTYPE_TYPE)
                        {
                            if (debug_imported)
                                fprintf(stderr, "[IMPORTED_DECL]   candidate src_idx=%d kind=%d\n",
                                    n->source_unit_index, n->type ? n->type->kind : -1);
                            int n_is_unknown = (n->type != NULL &&
                                n->type->kind == TYPE_KIND_PRIMITIVE &&
                                n->type->info.primitive_type_tag == UNKNOWN_TYPE);
                            if (n->source_unit_index == g_semcheck_imported_decl_unit_index)
                            {
                                /* Exact unit match — accept immediately unless it's an
                                 * UNKNOWN_TYPE stub (cross-unit alias that failed to resolve
                                 * at predeclare time). In that case, keep scanning for a
                                 * resolved type from a dependency unit. */
                                if (!n_is_unknown)
                                {
                                    resolved_type = n;
                                    break;
                                }
                                /* Save as fallback in case no resolved type is found */
                                if (closest_ancestor == NULL)
                                    closest_ancestor = n;
                            }
#define candidate_is_unknown_stub(nd) ((nd) != NULL && (nd)->type != NULL && \
    (nd)->type->kind == TYPE_KIND_PRIMITIVE && \
    (nd)->type->info.primitive_type_tag == UNKNOWN_TYPE)
                            /* Prefer symbol from a unit that the importing unit uses */
                            else if (!n_is_unknown && n->source_unit_index > 0 &&
                                unit_registry_is_dep(g_semcheck_imported_decl_unit_index,
                                                     n->source_unit_index))
                            {
                                /* Only upgrade if we don't already have a dep match,
                                 * or this dep has a higher (more specific) index. */
                                if (closest_ancestor == NULL ||
                                    candidate_is_unknown_stub(closest_ancestor) ||
                                    n->source_unit_index > closest_idx)
                                {
                                    closest_ancestor = n;
                                    closest_idx = n->source_unit_index;
                                }
                                /* Don't break — exact match still wins */
                            }
                            else if (!n_is_unknown &&
                                     (closest_ancestor == NULL ||
                                      candidate_is_unknown_stub(closest_ancestor)) &&
                                     n->source_unit_index <= g_semcheck_imported_decl_unit_index)
                            {
                                /* Fallback: accept any non-UNKNOWN type visible to the importing unit,
                                 * including src_idx=0 (System builtins/prelude types). */
                                closest_ancestor = n;
                                closest_idx = n->source_unit_index;
                            }
#undef candidate_is_unknown_stub
                        }
                        c = c->next;
                    }
                    if (debug_imported)
                        fprintf(stderr, "[IMPORTED_DECL]   result: exact=%p ancestor=%p (idx=%d)\n",
                            (void *)resolved_type, (void *)closest_ancestor, closest_idx);
                    if (resolved_type == NULL && closest_ancestor != NULL)
                        resolved_type = closest_ancestor;
                    if (candidates != NULL)
                        DestroyList(candidates);
                }
                if (resolved_type == NULL)
                    resolved_type = semcheck_find_type_node_with_unit_flag_ref(symtab,
                        decl_type_ref, decl_type_id, 1);
            }
            else
            {
                /* In method parameter declarations, a type_id matching the current
                 * owner must bind to that owner type before generic lookup. This
                 * prevents imported aliases with the same name from corrupting
                 * Self/parameter record field resolution. */
                const char *owner_id = semcheck_get_current_method_owner();
                const char *owner_innermost = semcheck_get_current_subprogram_owner_class();
                if (owner_id != NULL &&
                    (decl_type_base != NULL &&
                     (pascal_identifier_equals(decl_type_base, owner_id) ||
                      (owner_innermost != NULL && pascal_identifier_equals(decl_type_base, owner_innermost)))))
                {
                    owner_type_match = 1;
                    if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL &&
                        semcheck_kgpc_type_is_record_like(
                            tree->tree_data.var_decl_data.cached_kgpc_type))
                    {
                        /* Method-owner parameters (Self / owner-typed args) may already
                         * carry precise record KgpcType from parser conversion.
                         * Never rebind them through ambiguous type_id lookup. */
                        resolved_type = NULL;
                    }
                    else
                    {
                    resolved_type = semcheck_find_owner_record_type_node(symtab, owner_id);
                    if (resolved_type == NULL && owner_innermost != NULL)
                        resolved_type = semcheck_find_owner_record_type_node(symtab, owner_innermost);
                    if (resolved_type == NULL)
                        resolved_type = semcheck_find_preferred_type_node(symtab, owner_id);
                    }
                }
                if (resolved_type == NULL && !owner_type_match)
                    resolved_type = semcheck_find_preferred_type_node_with_ref(symtab,
                        decl_type_ref, decl_type_id);
            }
        }
        if (tree->type == TREE_VAR_DECL &&
            decl_type_id != NULL &&
            resolved_type == NULL)
        {
            const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
            const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
            if (owner_full == NULL)
                owner_full = semcheck_get_current_method_owner();
            if (owner_full != NULL)
            {
                semcheck_maybe_qualify_nested_type(symtab, owner_full, owner_outer,
                    &tree->tree_data.var_decl_data.type_id,
                    &tree->tree_data.var_decl_data.type_ref);
                decl_type_ref = tree->tree_data.var_decl_data.type_ref;
                decl_type_id = tree->tree_data.var_decl_data.type_id;
                decl_type_base = (decl_type_ref != NULL)
                    ? type_ref_base_name(decl_type_ref)
                    : decl_type_id;
                if (decl_type_id != NULL)
                {
                    resolved_type = semcheck_find_preferred_type_node_with_ref(symtab,
                        decl_type_ref, decl_type_id);
                }
            }
        }
        if (tree->type == TREE_VAR_DECL)
        {
            /* Determine whether to retain the pre-existing cached type.
             * Normally imported declarations keep their cached type to prevent
             * late scope rebinding from corrupting aliases like BaseUnix.TSize.
             * Exception: if the cached type is an UNKNOWN_TYPE stub (cross-unit
             * alias that failed to resolve at predeclare time), allow replacing it
             * with a resolved type found now that all unit scopes are wired. */
            KgpcType *cached_existing = tree->tree_data.var_decl_data.cached_kgpc_type;
            int cached_is_unknown = (cached_existing != NULL &&
                cached_existing->kind == TYPE_KIND_PRIMITIVE &&
                cached_existing->info.primitive_type_tag == UNKNOWN_TYPE);
            int keep_imported_cached =
                (tree->tree_data.var_decl_data.defined_in_unit &&
                 cached_existing != NULL &&
                 !cached_is_unknown);
            if (resolved_type != NULL && resolved_type->type != NULL)
            {
                if (keep_imported_cached)
                {
                    /* Imported declarations should retain their original resolved type.
                     * Rebinding in later scopes can corrupt aliases like BaseUnix.TSize. */
                }
                else
                {
                /* Clear any pre-existing cached type if we're replacing with resolved type */
                if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                {
                    destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                    tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
                }
                /* Under {$H-}, the parser sets var_decl_data.type to
                 * SHORTSTRING_TYPE for bare 'string' declarations.  The
                 * symbol-table lookup resolves "string" to the generic
                 * STRING_TYPE alias registered by the system unit.  Honour
                 * the parser's decision so that Length/Copy/@s[1] emit the
                 * correct shortstring runtime calls and indexing. */
                if (tree->tree_data.var_decl_data.type == SHORTSTRING_TYPE &&
                    resolved_type->type != NULL &&
                    kgpc_type_is_string(resolved_type->type) &&
                    !kgpc_type_is_shortstring(resolved_type->type))
                {
                    tree->tree_data.var_decl_data.cached_kgpc_type =
                        create_primitive_type(SHORTSTRING_TYPE);
                }
                else
                {
                kgpc_type_retain(resolved_type->type);
                tree->tree_data.var_decl_data.cached_kgpc_type = resolved_type->type;
                }
                }
            }
            else if (resolved_type != NULL && resolved_type->type == NULL && decl_type_id != NULL)
            {
                const char *type_name = decl_type_base != NULL ? decl_type_base : decl_type_id;
                int builtin_tag = semcheck_map_builtin_type_name_local(type_name);
                /* Honour the parser's {$H-} remap: if the tree tag is
                 * SHORTSTRING_TYPE but the name lookup yields STRING_TYPE,
                 * use the tree tag so the runtime gets the right calls. */
                if (builtin_tag == STRING_TYPE &&
                    tree->tree_data.var_decl_data.type == SHORTSTRING_TYPE)
                {
                    builtin_tag = SHORTSTRING_TYPE;
                }
                if (builtin_tag == STRING_TYPE || builtin_tag == SHORTSTRING_TYPE)
                {
                    tree->tree_data.var_decl_data.cached_kgpc_type =
                        create_primitive_type(builtin_tag);
                }
            }
            else if (tree->tree_data.var_decl_data.cached_kgpc_type == NULL &&
                     resolved_type == NULL && decl_type_id != NULL)
            {
                /* Fallback: Create KgpcType for built-in type names not in symbol table */
                const char *type_id = decl_type_base != NULL ? decl_type_base : decl_type_id;
                int builtin_tag = semcheck_map_builtin_type_name_local(type_id);
                /* Honour the parser's {$H-} remap. */
                if (builtin_tag == STRING_TYPE &&
                    tree->tree_data.var_decl_data.type == SHORTSTRING_TYPE)
                {
                    builtin_tag = SHORTSTRING_TYPE;
                }
                if (builtin_tag != UNKNOWN_TYPE)
                {
                    tree->tree_data.var_decl_data.cached_kgpc_type = create_primitive_type(builtin_tag);
                }
                else if (tree->tree_data.var_decl_data.type_ref != NULL &&
                         tree->tree_data.var_decl_data.type_ref->num_generic_args > 0)
                {
                    /* Handle parameterized string types like AnsiString(CP_NONE) */
                    const char *base_name = decl_type_base != NULL ? decl_type_base : decl_type_id;
                    int base_tag = semcheck_map_builtin_type_name_local(base_name);
                    if (base_tag == STRING_TYPE || base_tag == SHORTSTRING_TYPE)
                    {
                        tree->tree_data.var_decl_data.cached_kgpc_type =
                            create_primitive_type(base_tag);
                    }
                    else
                    {
                        tree->tree_data.var_decl_data.cached_kgpc_type =
                            create_primitive_type(POINTER_TYPE);
                    }
                }
            }
            /* Note: If cached_kgpc_type is already set (e.g., for inline procedure types from parser),
             * preserve it rather than destroying it. */
            /* AUDIT: Log when KgpcType is missing for a type_id */
            if (kgpc_getenv("KGPC_AUDIT_TYPES") != NULL && tree->tree_data.var_decl_data.type_id != NULL)
            {
                const char *var_name = (ids_head && ids_head->cur) ? (char*)ids_head->cur : "<unknown>";
                const char *type_id = tree->tree_data.var_decl_data.type_id;
                KgpcType *kgpc = tree->tree_data.var_decl_data.cached_kgpc_type;
                if (kgpc == NULL)
                {
                    fprintf(stderr, "[TYPE_AUDIT] MISSING: var=%s type_id=%s resolved_node=%p\n",
                        var_name, type_id, (void*)resolved_type);
                }
                else if (kgpc->kind == TYPE_KIND_PRIMITIVE && kgpc->info.primitive_type_tag == POINTER_TYPE)
                {
                    /* Pointer type but no points_to info - this is a problem */
                    fprintf(stderr, "[TYPE_AUDIT] INCOMPLETE_PTR: var=%s type_id=%s kind=PRIMITIVE tag=POINTER (should be TYPE_KIND_POINTER)\n",
                        var_name, type_id);
                }
            }
        }
        int skip_initializer = 0;
        while(ids != NULL)
        {
            assert(ids->cur != NULL);
            assert(ids->type == LIST_STRING);

            /* Variable declarations */
            if(tree->type == TREE_VAR_DECL)
            {
                if (tree->tree_data.var_decl_data.is_typed_const)
                {
                    HashNode_t *existing_node = NULL;
                    if (FindSymbol(&existing_node, symtab, ids->cur) != 0 &&
                        existing_node != NULL && existing_node->is_typed_const)
                    {
                        mark_hashnode_unit_info(symtab, existing_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                        skip_initializer = 1;
                        goto next_identifier;
                    }
                }
                if (ids->cur != NULL && strcmp((char *)ids->cur, "Sock") == 0) {
#ifdef DEBUG
                     fprintf(stderr, "DEBUG: semcheck_decls processing Sock. type_id=%s resolved_type=%p\n",
                         tree->tree_data.var_decl_data.type_id ? tree->tree_data.var_decl_data.type_id : "<null>",
                         resolved_type);
#endif
                }

                if (tree->tree_data.var_decl_data.type_id != NULL)
                {
                    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && tree->tree_data.var_decl_data.is_typed_const)
                        fprintf(stderr, "[SemCheck] Typed const with type_id: %s, var: %s\n",
                            tree->tree_data.var_decl_data.type_id,
                            ids && ids->cur ? (char*)ids->cur : "<null>");
                    HashNode_t *type_node = resolved_type;
                    const char *type_id = tree->tree_data.var_decl_data.type_id;
                    int declared_type = tree->tree_data.var_decl_data.type;
                    if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur &&
                        pascal_identifier_equals((char*)ids->cur, "Self"))
                    {
                        fprintf(stderr, "[KGPC] semcheck_decls Self: type_id=%s declared_type=%d type_node=%p type_node->type=%p kind=%d\n",
                            type_id ? type_id : "<null>", declared_type, (void*)type_node,
                            type_node && type_node->type ? (void*)type_node->type : NULL,
                            type_node && type_node->type ? type_node->type->kind : -1);
                    }
                    if (kgpc_getenv("KGPC_DEBUG_VAR_TYPES") != NULL && ids && ids->cur)
                    {
                        fprintf(stderr,
                            "[KGPC] semcheck_decls var=%s type_id=%s declared_type=%d type_node=%p type_node->type=%p cached=%p cached_kind=%d\n",
                            (char*)ids->cur,
                            type_id ? type_id : "<null>",
                            declared_type,
                            (void*)type_node,
                            type_node && type_node->type ? (void*)type_node->type : NULL,
                            (void*)tree->tree_data.var_decl_data.cached_kgpc_type,
                            tree->tree_data.var_decl_data.cached_kgpc_type ?
                                tree->tree_data.var_decl_data.cached_kgpc_type->kind : -1);
                    }

                    if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL &&
                        (type_node == NULL || type_node->type == NULL))
                    {
                        KgpcType *cached = tree->tree_data.var_decl_data.cached_kgpc_type;
                        kgpc_type_retain(cached);
                        func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, cached);
                        if (func_return == 0)
                        {
                            HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                            if (var_node != NULL)
                            {
                                var_node->is_var_parameter =
                                    (tree->tree_data.var_decl_data.is_var_param ||
                                     tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                mark_hashnode_unit_info(symtab, var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                                mark_hashnode_source_unit(var_node,
                                    tree->tree_data.var_decl_data.source_unit_index);
                            }
                        }
                        goto next_identifier;
                    }

                    if (declared_type == SET_TYPE)
                    {
                        KgpcType *set_type = create_primitive_type(SET_TYPE);
                        if (set_type != NULL &&
                            tree->tree_data.var_decl_data.inline_type_alias != NULL)
                        {
                            kgpc_type_set_type_alias(set_type,
                                tree->tree_data.var_decl_data.inline_type_alias);
                        }
                        if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                        {
                            destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                            tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
                        }
                        if (set_type != NULL)
                        {
                            kgpc_type_retain(set_type);
                            tree->tree_data.var_decl_data.cached_kgpc_type = set_type;
                        }
                        func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, set_type);
                        if (func_return == 0)
                        {
                            HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                            if (var_node != NULL)
                            {
                                var_node->is_var_parameter =
                                    (tree->tree_data.var_decl_data.is_var_param ||
                                     tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                mark_hashnode_unit_info(symtab, var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                                mark_hashnode_source_unit(var_node,
                                    tree->tree_data.var_decl_data.source_unit_index);
                            }
                        }
                        goto next_identifier;
                    }

                    /* If declared as pointer type (^TypeName), handle inline pointer */
                    if (declared_type == POINTER_TYPE)
                    {
                        /* This is ^TypeName where TypeName is stored in type_id.
                         * We need to create a pointer type pointing to TypeName. */
                        KgpcType *points_to = NULL;
                        
                        /* Try to resolve the target type */
                        if (type_node != NULL && type_node->type != NULL)
                        {
                            kgpc_type_retain(type_node->type);
                            points_to = type_node->type;
                        }
                        else
                        {
                            /* Check builtin types for the pointer target */
                            int target_tag = semcheck_map_builtin_type_name_local(type_id);
                            if (target_tag != UNKNOWN_TYPE)
                            {
                                points_to = create_primitive_type(target_tag);
                            }
                            else
                            {
                                /* Unknown target type - create a placeholder */
                                points_to = NULL;
                            }
                        }
                        
                        KgpcType *pointer_type = create_pointer_type(points_to);
                        func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, pointer_type);
                        if (func_return == 0)
                        {
                            HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                            if (var_node != NULL)
                            {
                                var_node->is_var_parameter =
                                    (tree->tree_data.var_decl_data.is_var_param ||
                                     tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                mark_hashnode_unit_info(symtab, var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                                mark_hashnode_source_unit(var_node,
                                    tree->tree_data.var_decl_data.source_unit_index);
                            }
                        }
                        goto next_identifier;
                    }

                    /* Check if it's a builtin type even if not in symbol table */
                    if (type_node == NULL)
                    {
                        if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur)
                            fprintf(stderr, "[KGPC] semcheck_decls: %s has type_id=%s but type_node is NULL\n",
                                (char*)ids->cur, type_id);
                        /* Use the central type-name mapper first */
                        int tag = semcheck_map_builtin_type_name_local(type_id);
                        if (tag == SHORTSTRING_TYPE)
                        {
                            /* ShortString is array[0..255] of Char with length at index 0 */
                            var_type = HASHVAR_ARRAY;
                        }
                        else if (tag != UNKNOWN_TYPE)
                        {
                            var_type = map_type_tag_to_var_type(tag);
                        }
                        else if (tree->tree_data.var_decl_data.type_ref != NULL &&
                                 tree->tree_data.var_decl_data.type_ref->num_generic_args > 0)
                            var_type = HASHVAR_POINTER;
                        /* Try resolving via TypeRef (handles unit-qualified types
                         * like baseunix.stat where the qualifier is structural). */
                        else if (decl_type_ref != NULL)
                        {
                            HashNode_t *ref_node = semcheck_find_preferred_type_node_with_ref(
                                symtab, decl_type_ref, type_id);
                            if (ref_node != NULL)
                            {
                                type_node = ref_node;
                                goto var_decl_type_resolved;
                            }
                            else
                            {
                                semantic_error(tree->line_num, 0, "undefined type %s", type_id);
                                return_val++;
                                var_type = HASHVAR_UNTYPED;
                            }
                        }
                        else
                        {
                            semcheck_assert_pointer_type_resolved(type_id);
                            semantic_error(tree->line_num, 0, "undefined type %s", type_id);
                            return_val++;
                            var_type = HASHVAR_UNTYPED;
                        }
                    }
                    var_decl_type_resolved:
                    if (type_node != NULL)
                    {
                        if (type_node->type == NULL && type_id != NULL)
                        {
                            int builtin_tag = semcheck_map_builtin_type_name_local(type_id);
                            if (builtin_tag != UNKNOWN_TYPE)
                            {
                                KgpcType *builtin_type = create_primitive_type(builtin_tag);
                                func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, builtin_type);
                                if (func_return == 0)
                                {
                                    HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                                    if (var_node != NULL)
                                    {
                                        var_node->is_var_parameter =
                                            (tree->tree_data.var_decl_data.is_var_param ||
                                             tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                        mark_hashnode_unit_info(symtab, var_node,
                                            tree->tree_data.var_decl_data.defined_in_unit,
                                            tree->tree_data.var_decl_data.unit_is_public);
                                        mark_hashnode_source_unit(var_node,
                                            tree->tree_data.var_decl_data.source_unit_index);
                                    }
                                }
                                goto next_identifier;
                            }
                        }

                        if (type_node->type != NULL && type_node->type->kind == TYPE_KIND_PROCEDURE)
                        {
                            kgpc_type_retain(type_node->type);
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, type_node->type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                                if (var_node != NULL)
                                {
                                    var_node->is_var_parameter =
                                        (tree->tree_data.var_decl_data.is_var_param ||
                                         tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                    mark_hashnode_unit_info(symtab, var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                    mark_hashnode_source_unit(var_node,
                                        tree->tree_data.var_decl_data.source_unit_index);
                                }
                            }
                            goto next_identifier;
                        }

                        int declared_type_tag = tree->tree_data.var_decl_data.type;
                        int needs_inline_pointer = (declared_type_tag == POINTER_TYPE &&
                            (type_node->type == NULL || type_node->type->kind != TYPE_KIND_POINTER));

                        if (needs_inline_pointer)
                        {
                            KgpcType *points_to = NULL;
                            if (type_node->type != NULL)
                            {
                                kgpc_type_retain(type_node->type);
                                points_to = type_node->type;
                            }
                            else
                            {
                                struct RecordType *target_record = get_record_type_from_node(type_node);
                                if (target_record != NULL)
                                {
                                    points_to = create_record_type(target_record);
                                }
                                else
                                {
                                    enum VarType target_var_type = get_var_type_from_node(type_node);
                                    int primitive_tag = map_var_type_to_type_tag(target_var_type);
                                    if (primitive_tag != UNKNOWN_TYPE)
                                        points_to = create_primitive_type(primitive_tag);
                                }
                            }

                            KgpcType *pointer_type = create_pointer_type(points_to);
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, pointer_type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                                if (var_node != NULL)
                                {
                                    var_node->is_var_parameter =
                                        (tree->tree_data.var_decl_data.is_var_param ||
                                         tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                    mark_hashnode_unit_info(symtab, var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                    mark_hashnode_source_unit(var_node,
                                        tree->tree_data.var_decl_data.source_unit_index);
                                }
                            }
                            goto next_identifier;
                        }

                        var_type = get_var_type_from_node(type_node);
                        int resolved_tag = map_var_type_to_type_tag(var_type);
                        /* Preserve {$H-} remap: if the parser set
                         * SHORTSTRING_TYPE but the type-node resolved to
                         * STRING_TYPE, keep the parser's tag. */
                        if (resolved_tag != UNKNOWN_TYPE &&
                            !(declared_type_tag == SHORTSTRING_TYPE &&
                              resolved_tag == STRING_TYPE))
                            tree->tree_data.var_decl_data.type = resolved_tag;
                        struct TypeAlias *alias = get_type_alias_from_node(type_node);
                        if (alias != NULL && alias->is_array)
                        {
                            /* Ensure array bounds are resolved for aliases using symbolic dimensions. */
                            if (alias->array_dimensions != NULL &&
                                type_node->type != NULL &&
                                type_node->type->kind == TYPE_KIND_ARRAY)
                            {
                                resolve_array_bounds_in_kgpctype(symtab, type_node->type, alias);
                            }

                            /* Prefer the KgpcType from the type node if it is already an array. */
                            if (type_node->type != NULL &&
                                type_node->type->kind == TYPE_KIND_ARRAY)
                            {
                                kgpc_type_retain(type_node->type);
                                func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, type_node->type);
                                kgpc_type_release(type_node->type);  /* Release caller's ref */
                                if (func_return == 0)
                                {
                                    HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                                    if (var_node != NULL)
                                    {
                                        mark_hashnode_unit_info(symtab, var_node,
                                            tree->tree_data.var_decl_data.defined_in_unit,
                                            tree->tree_data.var_decl_data.unit_is_public);
                                        mark_hashnode_source_unit(var_node,
                                            tree->tree_data.var_decl_data.source_unit_index);
                                    }
                                }

                                goto next_identifier;
                            }

                            int start = alias->array_start;
                            int end = alias->array_end;
                            if (alias->is_open_array)
                            {
                                start = 0;
                                end = -1;
                            }

                            /* Get element type - it might be a primitive type or a type reference */
                            KgpcType *element_type = NULL;
                            int element_type_tag = alias->array_element_type;

                            /* If element type is a type reference, resolve it */
                            if (element_type_tag == UNKNOWN_TYPE && alias->array_element_type_id != NULL)
                            {
                                HashNode_t *element_type_node = NULL;
                                if (FindSymbol(&element_type_node, symtab, alias->array_element_type_id) != 0 &&
                                    element_type_node != NULL && element_type_node->type != NULL)
                                {
                                    element_type = element_type_node->type;
                                    /* CRITICAL: Retain element_type since it's borrowed from symbol table
                                     * and create_array_type takes ownership. Without this, both the symbol
                                     * table and array_type would try to free the same type, causing double-free. */
                                    kgpc_type_retain(element_type);
                                }
                            }
                            else if (element_type_tag != UNKNOWN_TYPE)
                            {
                                /* Direct primitive type tag - use create_primitive_type */
                                element_type = create_primitive_type(element_type_tag);
                            }

                            /* If element type is still NULL, create an unknown type to avoid crash */
                            if (element_type == NULL)
                            {
                                element_type = create_primitive_type(UNKNOWN_TYPE);
                            }

                            /* Create array KgpcType - takes ownership of element_type */
                            KgpcType *array_type = create_array_type(element_type, start, end);
                            assert(array_type != NULL && "Failed to create array type");
                            
                            /* Set type_alias on KgpcType so it's properly propagated */
                            kgpc_type_set_type_alias(array_type, alias);
                            
                            func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, array_type);
                            destroy_kgpc_type(array_type);  /* Release creator's ref */
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                                if (var_node != NULL)
                                {
                                    mark_hashnode_unit_info(symtab, var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                    mark_hashnode_source_unit(var_node,
                                        tree->tree_data.var_decl_data.source_unit_index);
                                }
                            }

                            goto next_identifier;
                        }

                        /* For non-array type references (e.g., enum, set, file, record), create KgpcType from type_node */
                        KgpcType *var_kgpc_type = NULL;
                        if (type_node->type != NULL)
                        {
                            var_kgpc_type = type_node->type;
                            /* Under {$H-}, the parser sets declared_type_tag
                             * to SHORTSTRING_TYPE but the symtab lookup
                             * resolves "string" to STRING_TYPE.  Override
                             * with a fresh SHORTSTRING_TYPE primitive so
                             * Length/Copy/@s[1] work correctly. */
                            if (declared_type_tag == SHORTSTRING_TYPE &&
                                var_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                                var_kgpc_type->info.primitive_type_tag == STRING_TYPE)
                            {
                                var_kgpc_type = create_primitive_type(SHORTSTRING_TYPE);
                            }
                            /* If the type node has a primitive KgpcType but the alias says
                             * it's a pointer, rebuild as a proper pointer type so downstream
                             * pointer dereferences resolve correctly. */
                            if (var_kgpc_type->kind == TYPE_KIND_PRIMITIVE && alias != NULL &&
                                alias->is_pointer)
                            {
                                var_kgpc_type = create_kgpc_type_from_type_alias(
                                    alias, symtab, tree->tree_data.var_decl_data.defined_in_unit);
                                if (var_kgpc_type == NULL)
                                {
                                    var_kgpc_type = type_node->type;
                                    kgpc_type_retain(var_kgpc_type);
                                }
                            }
                            /* If the type node is an UNKNOWN primitive stub (e.g., a plain alias
                             * like PString = ObjPas.PString that failed to resolve at predeclare
                             * time but whose target may now be available), try re-resolving via
                             * the alias target.  This handles per-unit scoping where SysUtils's
                             * PString stub is in unit_scopes[sysutils]->table but ObjPas's resolved
                             * pointer type is in unit_scopes[objpas]->table. */
                            else if (var_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                                     var_kgpc_type->info.primitive_type_tag == UNKNOWN_TYPE &&
                                     alias != NULL &&
                                     (alias->target_type_id != NULL || alias->target_type_ref != NULL))
                            {
                                KgpcType *resolved_via_alias = create_kgpc_type_from_type_alias(
                                    alias, symtab, tree->tree_data.var_decl_data.defined_in_unit);
                                if (resolved_via_alias != NULL)
                                    var_kgpc_type = resolved_via_alias;
                                else
                                    kgpc_type_retain(var_kgpc_type);
                            }
                            else
                            {
                                kgpc_type_retain(var_kgpc_type);
                            }
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                        }
                        else
                        {
                            /* Fallback: create KgpcType from legacy fields using helpers */
                            struct RecordType *record_type = get_record_type_from_node(type_node);
                            struct TypeAlias *type_alias = get_type_alias_from_node(type_node);
                            if (type_alias != NULL)
                            {
                                var_kgpc_type = create_kgpc_type_from_type_alias(
                                    type_alias, symtab, tree->tree_data.var_decl_data.defined_in_unit);
                            }
                            if (var_kgpc_type != NULL)
                            {
                                /* No further legacy handling needed */
                            }
                            else if (record_type != NULL)
                            {
                                /* Use the canonical RecordType, not a clone */
                                var_kgpc_type = create_record_type(record_type);
                            }
                            else if (var_type == HASHVAR_POINTER)
                            {
                                /* For pointer types, we need to create a pointer KgpcType */
                                /* Get the TypeAlias to find what the pointer points to */
                                if (type_alias != NULL && type_alias->is_pointer)
                                {
                                    KgpcType *points_to = NULL;
                                    
                                    /* Try to resolve the target type */
                                    if (type_alias->pointer_type_id != NULL)
                                    {
                                        HashNode_t *target_node = NULL;
                                        if (FindSymbol(&target_node, symtab, type_alias->pointer_type_id) != 0 &&
                                            target_node != NULL && target_node->type != NULL)
                                        {
                                            kgpc_type_retain(target_node->type);
                                            points_to = target_node->type;
                                        }
                                    }
                                    
                                    /* If we couldn't resolve it, create a placeholder based on pointer_type */
                                    if (points_to == NULL && type_alias->pointer_type != UNKNOWN_TYPE)
                                    {
                                        points_to = create_primitive_type(type_alias->pointer_type);
                                    }
                                    
                                    if (points_to != NULL)
                                    {
                                        var_kgpc_type = create_pointer_type(points_to);
                                        kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                                    }
                                }
                            }
                            else
                            {
                                var_kgpc_type = kgpc_type_from_var_type(var_type);
                            }

                            if (var_kgpc_type != NULL && type_alias != NULL && var_type != HASHVAR_POINTER)
                            {
                                kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                            }
                            
                            /* Always use _Typed variant, even if KgpcType is NULL (UNTYPED) */
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                            if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur &&
                                pascal_identifier_equals((char*)ids->cur, "Self"))
                                fprintf(stderr, "[KGPC] semcheck_decls: PushVarOntoScope_Typed for Self returned %d\n", func_return);
                        }
                        /* Release caller's ref (either created or retained above) */
                        destroy_kgpc_type(var_kgpc_type);

                        if (func_return == 0)
                        {
                            HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                            if (var_node != NULL)
                            {
                                var_node->is_var_parameter =
                                    (tree->tree_data.var_decl_data.is_var_param ||
                                     tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                mark_hashnode_unit_info(symtab, var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                                mark_hashnode_source_unit(var_node,
                                    tree->tree_data.var_decl_data.source_unit_index);
                            }
                        }
                        goto next_identifier;
                    }
                }
                else if (tree->tree_data.var_decl_data.inferred_type)
                {
                    /* For type inference, use INTEGER as placeholder - will be replaced later */
                    var_type = HASHVAR_INTEGER;  /* Placeholder */
                }
                else if (tree->tree_data.var_decl_data.type == UNKNOWN_TYPE &&
                    tree->tree_data.var_decl_data.type_id == NULL &&
                    tree->tree_data.var_decl_data.is_untyped_param)
                {
                    var_type = HASHVAR_UNTYPED;
                }
                else if(tree->tree_data.var_decl_data.type == INT_TYPE)
                    var_type = HASHVAR_INTEGER;
                else if(tree->tree_data.var_decl_data.type == LONGINT_TYPE)
                    var_type = HASHVAR_LONGINT;
                else if(tree->tree_data.var_decl_data.type == BOOL)
                    var_type = HASHVAR_BOOLEAN;
                else if(tree->tree_data.var_decl_data.type == SET_TYPE)
                    var_type = HASHVAR_SET;
                else if(tree->tree_data.var_decl_data.type == ENUM_TYPE)
                    var_type = HASHVAR_ENUM;
                else if(tree->tree_data.var_decl_data.type == STRING_TYPE)
                    var_type = HASHVAR_PCHAR;
                else if(tree->tree_data.var_decl_data.type == RECORD_TYPE)
                    var_type = HASHVAR_RECORD;
                else if(tree->tree_data.var_decl_data.type == FILE_TYPE)
                    var_type = HASHVAR_FILE;
                else if(tree->tree_data.var_decl_data.type == TEXT_TYPE)
                    var_type = HASHVAR_TEXT;
                else if(tree->tree_data.var_decl_data.type == PROCEDURE)
                    var_type = HASHVAR_PROCEDURE;
                else
                    var_type = HASHVAR_REAL;
                
                /* Create KgpcType for typed variables */
                KgpcType *var_kgpc_type = NULL;
                int var_kgpc_borrowed = 0;
                if (resolved_type != NULL && resolved_type->type != NULL)
                {
                    /* Use KgpcType from resolved type if available */
                    var_kgpc_type = resolved_type->type;
                    var_kgpc_borrowed = 1;
                }
                else if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                {
                    /* Use pre-cached KgpcType (e.g., for inline procedure types) */
                    var_kgpc_type = tree->tree_data.var_decl_data.cached_kgpc_type;
                    var_kgpc_borrowed = 1;
                }
                else if (tree->tree_data.var_decl_data.inline_record_type != NULL)
                {
                    /* Handle inline record type declarations */
                    var_kgpc_type = create_record_type(clone_record_type(tree->tree_data.var_decl_data.inline_record_type));
                }
                else
                {
                    /* Special handling for ShortString - create as array[0..255] of Char */
                    if (var_type == HASHVAR_ARRAY &&
                        tree->tree_data.var_decl_data.type_id != NULL &&
                        pascal_identifier_equals(tree->tree_data.var_decl_data.type_id, "ShortString"))
                    {
                        /* Create ShortString as array[0..255] of Char */
                        KgpcType *char_type = create_primitive_type(CHAR_TYPE);
                        var_kgpc_type = create_array_type(char_type, 0, 255);
                        if (var_kgpc_type != NULL)
                        {
                            struct TypeAlias alias = {0};
                            alias.is_array = 1;
                            alias.array_start = 0;
                            alias.array_end = 255;
                            alias.array_element_type = CHAR_TYPE;
                            alias.array_element_type_id = "char";  /* Will be duplicated by copy_type_alias */
                            alias.is_shortstring = 1;
                            kgpc_type_set_type_alias(var_kgpc_type, &alias);
                        }
                    }
                    /* Handle inline array types (e.g., array[0..2] of PChar) */
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL &&
                             tree->tree_data.var_decl_data.inline_type_alias->is_array)
                    {
                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                            fprintf(stderr, "[SemCheck] Processing inline array for var: %s\n",
                                ids && ids->cur ? (char*)ids->cur : "<null>");
                        struct TypeAlias *alias = tree->tree_data.var_decl_data.inline_type_alias;

                        /* Resolve enum-indexed array bounds (e.g., array[TMyEnum] of ...) */
                        if (alias->array_dimensions != NULL &&
                            alias->array_start == 0 && alias->array_end == 0 &&
                            !alias->is_open_array)
                        {
                            ListNode_t *first_dim = alias->array_dimensions;
                            const char *dim_str = (first_dim != NULL && first_dim->type == LIST_STRING) ?
                                (const char *)first_dim->cur : NULL;
                            if (dim_str != NULL)
                            {
                                HashNode_t *dim_type_node = NULL;
                                if (FindSymbol(&dim_type_node, symtab, dim_str) != 0 &&
                                    dim_type_node != NULL && dim_type_node->hash_type == HASHTYPE_TYPE)
                                {
                                    struct TypeAlias *dim_alias = get_type_alias_from_node(dim_type_node);
                                    if (dim_alias != NULL && dim_alias->is_enum &&
                                        dim_alias->enum_literals != NULL)
                                    {
                                        int count = ListLength(dim_alias->enum_literals);
                                        if (count > 0)
                                        {
                                            alias->array_start = 0;
                                            alias->array_end = count - 1;
                                        }
                                    }
                                    else if (dim_alias != NULL && dim_alias->is_range &&
                                             dim_alias->range_known)
                                    {
                                        alias->array_start = dim_alias->range_start;
                                        alias->array_end = dim_alias->range_end;
                                    }
                                }
                            }
                        }

                        int start = alias->array_start;
                        int end = alias->array_end;
                        if (alias->is_open_array)
                        {
                            start = 0;
                            end = -1;
                        }

                        /* Get element type */
                        KgpcType *element_type = NULL;
                        int element_type_tag = alias->array_element_type;
                        int element_type_borrowed = 0;  /* Track if borrowed from symbol table */

                        if (element_type_tag == UNKNOWN_TYPE && alias->array_element_type_id != NULL)
                        {
                            HashNode_t *element_type_node = NULL;
                            if (FindSymbol(&element_type_node, symtab, alias->array_element_type_id) != 0 &&
                                element_type_node != NULL && element_type_node->type != NULL)
                            {
                                element_type = element_type_node->type;
                                element_type_borrowed = 1;
                            }
                            else
                            {
                                /* Check for builtin type */
                                int builtin_tag = semcheck_map_builtin_type_name_local(alias->array_element_type_id);
                                if (builtin_tag != UNKNOWN_TYPE)
                                    element_type = create_primitive_type(builtin_tag);
                            }
                        }
                        else if (element_type_tag != UNKNOWN_TYPE)
                        {
                            element_type = create_primitive_type(element_type_tag);
                        }

                        if (element_type != NULL)
                        {
                            /* CRITICAL: Retain element_type if borrowed from symbol table
                             * since create_array_type takes ownership. */
                            if (element_type_borrowed)
                                kgpc_type_retain(element_type);
                            var_kgpc_type = create_array_type(element_type, start, end);
                            kgpc_type_set_type_alias(var_kgpc_type, alias);
                        }
                        else
                        {
                            /* Fallback to var_type if element type unresolved */
                            var_kgpc_type = kgpc_type_from_var_type(var_type);
                        }
                    }
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL)
                    {
                        struct TypeAlias *alias = tree->tree_data.var_decl_data.inline_type_alias;
                        if (alias->storage_size > 0)
                        {
                            var_kgpc_type = create_primitive_type_with_size(alias->base_type,
                                (int)alias->storage_size);
                        }
                        else
                        {
                            var_kgpc_type = create_primitive_type(alias->base_type);
                        }
                        if (var_kgpc_type != NULL)
                            kgpc_type_set_type_alias(var_kgpc_type, alias);
                    }
                    else
                    {
                        /* Create KgpcType from var_type */
                        var_kgpc_type = kgpc_type_from_var_type(var_type);
                        if (var_kgpc_type == NULL && var_type == HASHVAR_POINTER)
                        {
                            var_kgpc_type = create_pointer_type(NULL);
                        }
                    }
                    
                    if (var_kgpc_type != NULL)
                    {
                        if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                        {
                            destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                            tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
                        }
                        kgpc_type_retain(var_kgpc_type);
                        tree->tree_data.var_decl_data.cached_kgpc_type = var_kgpc_type;
                    }

                    /* Add metadata from resolved_type if present */
                    if (var_kgpc_type != NULL && resolved_type != NULL)
                    {
                        struct TypeAlias *type_alias = get_type_alias_from_node(resolved_type);
                        if (type_alias != NULL)
                        {
                            kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                        }
                        struct RecordType *record_type = get_record_type_from_node(resolved_type);
                        if (record_type != NULL && var_kgpc_type->kind == TYPE_KIND_RECORD)
                        {
                            /* Use the canonical RecordType from the symbol table,
                             * not a clone. This ensures type identity checks work correctly. */
                            var_kgpc_type->info.record_info = record_type;
                        }
                    }
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL &&
                        var_kgpc_type != NULL)
                    {
                        kgpc_type_set_type_alias(var_kgpc_type,
                            tree->tree_data.var_decl_data.inline_type_alias);
                    }
                }
                
                if (var_kgpc_type != NULL && kgpc_type_is_record(var_kgpc_type))
                {
                    struct RecordType *var_record = kgpc_type_get_record(var_kgpc_type);
                    if (var_record != NULL)
                    {
                        /* Skip size computation for generic templates (not yet specialized)
                         * Size can only be computed after type parameters are substituted */
                        int is_unspecialized_generic = (var_record->generic_decl != NULL && 
                                                         var_record->num_generic_args == 0);
                        
                        if (!is_unspecialized_generic)
                        {
                            long long record_size = 0;
                            if (semcheck_compute_record_size(symtab, var_record, &record_size,
                                    tree->line_num) != 0)
                            {
                                return_val += 1;
                            }
                        }
                    }
                }

                /* Always use _Typed variant, even if KgpcType is NULL (UNTYPED) */
                /* Use PushArrayOntoScope_Typed for ShortString (which is array[0..255] of Char) */
                int is_shortstring = (var_type == HASHVAR_ARRAY && 
                                     tree->tree_data.var_decl_data.type_id != NULL &&
                                     pascal_identifier_equals(tree->tree_data.var_decl_data.type_id, "ShortString"));
                
                if (is_shortstring)
                {
                    if (var_kgpc_borrowed && var_kgpc_type != NULL)
                        kgpc_type_retain(var_kgpc_type);
                    func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                }
                else
                {
                    if (var_kgpc_borrowed && var_kgpc_type != NULL)
                        kgpc_type_retain(var_kgpc_type);
                    func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                }
                /* Release caller's ref (either created or retained above) */
                destroy_kgpc_type(var_kgpc_type);

                if (func_return == 0)
                {
                    HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                    if (var_node != NULL)
                    {
                        var_node->is_var_parameter =
                            (tree->tree_data.var_decl_data.is_var_param ||
                             tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                        mark_hashnode_unit_info(symtab, var_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                        mark_hashnode_source_unit(var_node,
                            tree->tree_data.var_decl_data.source_unit_index);
                    }
                }
            }
            /* Array declarations */
            else
            {
                assert(tree->type == TREE_ARR_DECL);
                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] Processing TREE_ARR_DECL: %s is_typed_const=%d\n",
                        ids && ids->cur ? (char*)ids->cur : "<null>",
                        tree->tree_data.arr_decl_data.is_typed_const);

                KgpcType *element_type = NULL;
                int element_type_borrowed = 0;  /* Track if borrowed from symbol table */
                int is_array_of_const = (tree->tree_data.arr_decl_data.type == ARRAY_OF_CONST_TYPE);

                if (!is_array_of_const &&
                    tree->tree_data.arr_decl_data.element_kgpc_type != NULL)
                {
                    element_type = tree->tree_data.arr_decl_data.element_kgpc_type;
                    element_type_borrowed = 1;
                }

                /* If type_id is specified, resolve it to get the element type */
                const TypeRef *element_type_ref = tree->tree_data.arr_decl_data.type_ref;
                if (kgpc_getenv("KGPC_DEBUG_TZINFO") != NULL &&
                    ids != NULL && ids->cur != NULL &&
                    (pascal_identifier_equals((const char *)ids->cur, "CurrentTZinfo") ||
                     pascal_identifier_equals((const char *)ids->cur, "CurrentTzinfoEx")))
                {
                    fprintf(stderr,
                        "[KGPC_DEBUG_TZINFO] arr=%s type_id=%s type_ref_base=%s\n",
                        (const char *)ids->cur,
                        tree->tree_data.arr_decl_data.type_id ? tree->tree_data.arr_decl_data.type_id : "(null)",
                        element_type_ref ? type_ref_base_name(element_type_ref) : "(null)");
                }
                if (element_type == NULL &&
                    !is_array_of_const &&
                    (tree->tree_data.arr_decl_data.type_id != NULL || element_type_ref != NULL))
                {
                    if (kgpc_getenv("KGPC_DEBUG_TFLOAT") != NULL)
                    {
                        const char *arr_type_id = tree->tree_data.arr_decl_data.type_id;
                        if (arr_type_id == NULL && element_type_ref != NULL)
                            arr_type_id = type_ref_base_name(element_type_ref);
                        if (arr_type_id != NULL &&
                            pascal_identifier_equals(arr_type_id, "TFloatFormatProfile"))
                        {
                            int count = 0;
                            ListNode_t *matches = FindAllIdents(symtab, arr_type_id);
                            for (ListNode_t *m = matches; m != NULL; m = m->next)
                            {
                                HashNode_t *n = (HashNode_t *)m->cur;
                                if (n != NULL && n->hash_type == HASHTYPE_TYPE)
                                {
                                    fprintf(stderr,
                                        "[TFLOAT] array elem match id=%s type=%p defined_in_unit=%d source_unit_index=%d\n",
                                        n->id ? n->id : "<null>", (void *)n->type,
                                        n->defined_in_unit, n->source_unit_index);
                                    count++;
                                }
                            }
                            if (matches != NULL)
                                DestroyList(matches);
                            fprintf(stderr, "[TFLOAT] array elem total matches=%d\n", count);
                        }
                    }
                    HashNode_t *element_type_node = NULL;
                    element_type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                        element_type_ref, tree->tree_data.arr_decl_data.type_id);
                    if (element_type_node == NULL &&
                        !tree->tree_data.arr_decl_data.defined_in_unit)
                    {
                        const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
                        const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
                        if (owner_full == NULL)
                            owner_full = semcheck_get_current_method_owner();
                        if (owner_full != NULL)
                        {
                            semcheck_maybe_qualify_nested_type(symtab, owner_full, owner_outer,
                                &tree->tree_data.arr_decl_data.type_id,
                                &tree->tree_data.arr_decl_data.type_ref);
                            element_type_ref = tree->tree_data.arr_decl_data.type_ref;
                            element_type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                                element_type_ref, tree->tree_data.arr_decl_data.type_id);
                        }
                    }
                    if (element_type_node == NULL)
                    {
                        const char *fallback_id = tree->tree_data.arr_decl_data.type_id;
                        if (fallback_id == NULL && element_type_ref != NULL)
                            fallback_id = type_ref_base_name(element_type_ref);
                        if (fallback_id != NULL)
                        {
                            ListNode_t *matches = FindAllIdents(symtab, fallback_id);
                            for (ListNode_t *m = matches; m != NULL; m = m->next)
                            {
                                HashNode_t *n = (HashNode_t *)m->cur;
                                if (n != NULL && n->hash_type == HASHTYPE_TYPE)
                                {
                                    element_type_node = n;
                                    break;
                                }
                            }
                            if (matches != NULL)
                                DestroyList(matches);
                        }
                    }
                    if (kgpc_getenv("KGPC_DEBUG_MISSING_TYPE") != NULL)
                    {
                        const char *check_id = tree->tree_data.arr_decl_data.type_id;
                        if (check_id == NULL && element_type_ref != NULL)
                            check_id = type_ref_base_name(element_type_ref);
                        if (check_id != NULL &&
                            (pascal_identifier_equals(check_id, "TFloatFormatProfile") ||
                             pascal_identifier_equals(check_id, "TFloatSpecial") ||
                             pascal_identifier_equals(check_id, "TDIY_FP_Power_of_10") ||
                             pascal_identifier_equals(check_id, "pshortstring") ||
                             pascal_identifier_equals(check_id, "T")))
                        {
                            fprintf(stderr, "[MISSING_TYPE] element_type_node=%p for %s line=%d\n",
                                (void *)element_type_node, check_id, tree->line_num);
                        }
                    }
                    if (element_type_node != NULL)
                    {
                        /* Use the KgpcType from the resolved type node */
                        element_type = element_type_node->type;
                        element_type_borrowed = 1;  /* Mark as borrowed */
                        if (element_type == NULL)
                        {
                            /* Fallback for migration: some nodes may not have KgpcType populated yet.
                             * Try to construct KgpcType from legacy record type information. */
                            struct RecordType *record_type = get_record_type_from_node(element_type_node);
                            if (record_type != NULL)
                            {
                                /* Use the canonical RecordType, not a clone */
                                element_type = create_record_type(record_type);
                                element_type_borrowed = 0;  /* New type, not borrowed */
                            }
                        }
                    }
                    if (kgpc_getenv("KGPC_DEBUG_TZINFO") != NULL &&
                        ids != NULL && ids->cur != NULL &&
                        (pascal_identifier_equals((const char *)ids->cur, "CurrentTZinfo") ||
                         pascal_identifier_equals((const char *)ids->cur, "CurrentTzinfoEx")))
                    {
                        fprintf(stderr,
                            "[KGPC_DEBUG_TZINFO] element_type_node=%p type=%p type_id=%s\n",
                            (void *)element_type_node,
                            element_type_node ? (void *)element_type_node->type : NULL,
                            tree->tree_data.arr_decl_data.type_id ? tree->tree_data.arr_decl_data.type_id : "(null)");
                    }
                    else
                    {
                        /* Fallback: check for builtin types not in symbol table */
                        const char *type_id = tree->tree_data.arr_decl_data.type_id;
                        if (type_id == NULL && element_type_ref != NULL)
                            type_id = type_ref_base_name(element_type_ref);
                        int builtin_type = semcheck_map_builtin_type_name_local(type_id);
                        if (builtin_type != UNKNOWN_TYPE)
                        {
                            if (builtin_type == CHAR_TYPE && type_id != NULL &&
                                (pascal_identifier_equals(type_id, "WideChar") ||
                                 pascal_identifier_equals(type_id, "UnicodeChar")))
                            {
                                element_type = create_primitive_type_with_size(CHAR_TYPE, 2);
                            }
                            else if (builtin_type == SHORTSTRING_TYPE &&
                                     tree->tree_data.arr_decl_data.element_kgpc_type != NULL)
                            {
                                /* Use the AST's element_kgpc_type which has specific size
                                 * for string[N] (N+1 bytes) instead of generic 256 */
                                element_type = tree->tree_data.arr_decl_data.element_kgpc_type;
                                kgpc_type_retain(element_type);
                                element_type_borrowed = 0;
                            }
                            else
                            {
                                element_type = create_primitive_type(builtin_type);
                            }
                        }
                        else
                        {
                            const char *missing_id = tree->tree_data.arr_decl_data.type_id;
                            if (missing_id == NULL && element_type_ref != NULL)
                                missing_id = type_ref_base_name(element_type_ref);
                            if (kgpc_getenv("KGPC_DEBUG_MISSING_TYPE") != NULL)
                            {
                                fprintf(stderr, "[MISSING_TYPE] array elem unresolved id=%s line=%d\n",
                                    missing_id ? missing_id : "<null>", tree->line_num);
                            }
                            if (missing_id != NULL)
                            {
                                ListNode_t *matches = FindAllIdents(symtab, missing_id);
                                for (ListNode_t *m = matches; m != NULL; m = m->next)
                                {
                                    HashNode_t *n = (HashNode_t *)m->cur;
                                    if (n != NULL && n->hash_type == HASHTYPE_TYPE)
                                    {
                                        if (kgpc_getenv("KGPC_DEBUG_MISSING_TYPE") != NULL)
                                        {
                                            fprintf(stderr,
                                                "[MISSING_TYPE]   match id=%s type=%p defined_in_unit=%d source_unit_index=%d\n",
                                                n->id ? n->id : "<null>", (void *)n->type,
                                                n->defined_in_unit, n->source_unit_index);
                                        }
                                        if (n->type != NULL)
                                        {
                                            element_type = n->type;
                                            element_type_borrowed = 1;
                                            break;
                                        }
                                    }
                                }
                                if (matches != NULL)
                                    DestroyList(matches);
                            }
                            if (element_type == NULL)
                            {
                                semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d: undefined type %s\n",
                                    tree->line_num, tree->tree_data.arr_decl_data.type_id);
                                return_val++;
                            }
                        }
                    }
                }
                
                /* If element type not resolved from type_id, use primitive type */
                if (element_type == NULL && !is_array_of_const &&
                    tree->tree_data.arr_decl_data.inline_record_type != NULL)
                {
                    element_type = create_record_type(tree->tree_data.arr_decl_data.inline_record_type);
                    element_type_borrowed = 0;
                }

                /* If element type not resolved from type_id, use primitive type */
                if (element_type == NULL && !is_array_of_const)
                {
                    if(tree->tree_data.arr_decl_data.type == INT_TYPE)
                        var_type = HASHVAR_INTEGER;
                    else if(tree->tree_data.arr_decl_data.type == LONGINT_TYPE)
                        var_type = HASHVAR_LONGINT;
                    else if(tree->tree_data.arr_decl_data.type == BOOL)
                        var_type = HASHVAR_BOOLEAN;
                    else if(tree->tree_data.arr_decl_data.type == STRING_TYPE)
                        var_type = HASHVAR_PCHAR;
                    else if(tree->tree_data.arr_decl_data.type == SHORTSTRING_TYPE)
                        var_type = HASHVAR_PCHAR;  /* ShortString is array of char */
                    else if(tree->tree_data.arr_decl_data.type == CHAR_TYPE)
                        var_type = HASHVAR_CHAR;
                    else if(tree->tree_data.arr_decl_data.type == SET_TYPE)
                        var_type = HASHVAR_SET;
                    else if(is_real_family_type(tree->tree_data.arr_decl_data.type))
                        var_type = HASHVAR_REAL;
                    else {
                        semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, 
                            "Error on line %d, unknown array element type %d for %s.\n\n",
                            tree->line_num,
                            tree->tree_data.arr_decl_data.type,
                            ids && ids->cur ? (char*)ids->cur : "<unknown>");
                        return_val++;
                        var_type = HASHVAR_REAL;
                    }
                    
                    element_type = kgpc_type_from_var_type(var_type);
                    assert(element_type != NULL && "Array element type must be createable from VarType");
                }

                if (element_type != NULL && kgpc_type_is_record(element_type))
                {
                    struct RecordType *element_record = kgpc_type_get_record(element_type);
                    if (element_record != NULL && !element_record->has_cached_size)
                    {
                        long long record_size = 0;
                        if (semcheck_compute_record_size(symtab, element_record, &record_size,
                                tree->line_num) != 0)
                        {
                            return_val += 1;
                        }
                    }
                }
                
                /* Resolve array bounds from constant identifiers if necessary.
                 * This handles inline array declarations like: var arr: array[1..N] of integer
                 * where N is a const. The parser stores the original range string (e.g., "1..N")
                 * in range_str, which we parse and resolve here. */
                int start_bound = tree->tree_data.arr_decl_data.s_range;
                int end_bound = tree->tree_data.arr_decl_data.e_range;
                
                /* Use pre-split bounds from parser (avoids strstr on ".." at semcheck time) */
                {
                    const char *s = tree->tree_data.arr_decl_data.range_start_str;
                    const char *e = tree->tree_data.arr_decl_data.range_end_str;
                    if (s != NULL && e != NULL)
                    {
                        while (*s == ' ' || *s == '\t') s++;
                        while (*e == ' ' || *e == '\t') e++;

                        long long start_val = 0;
                        if (resolve_const_identifier(symtab, s, &start_val) == 0)
                            start_bound = (int)start_val;
                        else {
                            char *endptr;
                            long num = strtol(s, &endptr, 10);
                            if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                                start_bound = (int)num;
                        }

                        long long end_val = 0;
                        if (resolve_const_identifier(symtab, e, &end_val) == 0)
                            end_bound = (int)end_val;
                        else {
                            char *endptr;
                            long num = strtol(e, &endptr, 10);
                            if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                                end_bound = (int)num;
                        }
                    }
                }
                
                /* Resolve enum-indexed array bounds (e.g., array[TMyEnum] of ...) */
                if (start_bound == 0 && end_bound == 0 &&
                    tree->tree_data.arr_decl_data.unresolved_index_type != NULL)
                {
                    const char *idx_type = tree->tree_data.arr_decl_data.unresolved_index_type;
                    HashNode_t *idx_node = NULL;
                    if (FindSymbol(&idx_node, symtab, idx_type) != 0 &&
                        idx_node != NULL && idx_node->hash_type == HASHTYPE_TYPE)
                    {
                        struct TypeAlias *idx_alias = get_type_alias_from_node(idx_node);
                        if (idx_alias != NULL && idx_alias->is_enum &&
                            idx_alias->enum_literals != NULL)
                        {
                            int count = ListLength(idx_alias->enum_literals);
                            if (count > 0)
                            {
                                start_bound = 0;
                                end_bound = count - 1;
                            }
                        }
                        else if (idx_alias != NULL && idx_alias->is_range &&
                                 idx_alias->range_known)
                        {
                            start_bound = idx_alias->range_start;
                            end_bound = idx_alias->range_end;
                        }
                    }
                }
                /* Also check array_dimensions for single-identifier dimensions */
                if (start_bound == 0 && end_bound == 0 &&
                    tree->tree_data.arr_decl_data.array_dimensions != NULL)
                {
                    ListNode_t *first_dim = tree->tree_data.arr_decl_data.array_dimensions;
                    const char *dim_str = (first_dim != NULL && first_dim->type == LIST_STRING) ?
                        (const char *)first_dim->cur : NULL;
                    if (dim_str != NULL)
                    {
                        HashNode_t *dim_node = NULL;
                        if (FindSymbol(&dim_node, symtab, dim_str) != 0 &&
                            dim_node != NULL && dim_node->hash_type == HASHTYPE_TYPE)
                        {
                            struct TypeAlias *dim_alias = get_type_alias_from_node(dim_node);
                            if (dim_alias != NULL && dim_alias->is_enum &&
                                dim_alias->enum_literals != NULL)
                            {
                                int count = ListLength(dim_alias->enum_literals);
                                if (count > 0)
                                {
                                    start_bound = 0;
                                    end_bound = count - 1;
                                }
                            }
                            else if (dim_alias != NULL && dim_alias->is_range &&
                                     dim_alias->range_known)
                            {
                                start_bound = dim_alias->range_start;
                                end_bound = dim_alias->range_end;
                            }
                        }
                    }
                }

                /* CRITICAL FIX: Update the tree's s_range and e_range fields with the resolved bounds.
                 * Downstream code (especially the x86-64 code generator) checks arr->e_range < arr->s_range
                 * to determine if an array is dynamic. Without updating these fields, arrays declared with
                 * constant expressions (e.g., array[1..N] where N is a const) would be incorrectly treated
                 * as dynamic arrays, leading to segmentation faults. */
                tree->tree_data.arr_decl_data.s_range = start_bound;
                tree->tree_data.arr_decl_data.e_range = end_bound;
                
                KgpcType *array_type = NULL;
                if (is_array_of_const)
                {
                    array_type = create_array_of_const_type();
                }
                else
                {
                    /* CRITICAL: Retain element_type if borrowed from symbol table
                     * since create_array_type retains internally. */
                    if (element_type_borrowed && element_type != NULL)
                        kgpc_type_retain(element_type);
                    array_type = create_array_type(
                        element_type,
                        start_bound,
                        end_bound
                    );
                    /* Release local ref; create_array_type retained its own */
                    if (!element_type_borrowed && element_type != NULL)
                        kgpc_type_release(element_type);
                }
                assert(array_type != NULL && "Failed to create array type");

                /* If the element type was specified by a type_id (like TAlfa), preserve that information
                 * by creating a minimal TypeAlias and attaching it to the array_type. This allows
                 * nested array indexing to work correctly (e.g., Keywords[1][1] where Keywords is
                 * array[1..5] of TAlfa and TAlfa is array[1..10] of char). */
                if (!is_array_of_const)
                {
                    struct TypeAlias temp_alias = {0};
                    int has_alias = 0;
                    
                    if (tree->tree_data.arr_decl_data.type_id != NULL)
                    {
                        temp_alias.is_array = 1;
                        temp_alias.array_start = start_bound;
                        temp_alias.array_end = end_bound;
                        temp_alias.array_element_type_id = tree->tree_data.arr_decl_data.type_id;  /* Will be duplicated by copy_type_alias */
                        temp_alias.array_element_type = tree->tree_data.arr_decl_data.type;
                        has_alias = 1;
                    }

                    if (tree->tree_data.arr_decl_data.is_shortstring)
                    {
                        if (!has_alias)
                        {
                            temp_alias.is_array = 1;
                            temp_alias.array_start = start_bound;
                            temp_alias.array_end = end_bound;
                            temp_alias.array_element_type = CHAR_TYPE;
                            temp_alias.array_element_type_id = "char";  /* Will be duplicated by copy_type_alias */
                            has_alias = 1;
                        }
                        temp_alias.is_shortstring = 1;
                    }

                    /* Attach multi-dim array_dimensions from the AST declaration.
                     * Only when there are 2+ dimensions — single-dim arrays
                     * don't need linearization metadata. */
                    if (tree->tree_data.arr_decl_data.array_dimensions != NULL &&
                        tree->tree_data.arr_decl_data.array_dimensions->next != NULL)
                    {
                        temp_alias.is_array = 1;
                        if (!has_alias)
                        {
                            temp_alias.array_start = start_bound;
                            temp_alias.array_end = end_bound;
                            temp_alias.array_element_type = tree->tree_data.arr_decl_data.type;
                            if (tree->tree_data.arr_decl_data.type_id != NULL)
                                temp_alias.array_element_type_id = tree->tree_data.arr_decl_data.type_id;
                        }
                        temp_alias.array_dimensions = tree->tree_data.arr_decl_data.array_dimensions;
                        has_alias = 1;
                    }

                    /* Propagate element storage size for shortstring[N] elements
                     * so kgpc_type_get_array_dimension_info uses N+1 instead of 256 */
                    if (has_alias && temp_alias.array_element_storage_size == 0 &&
                        tree->tree_data.arr_decl_data.element_kgpc_type != NULL)
                    {
                        long long esize = kgpc_type_sizeof(tree->tree_data.arr_decl_data.element_kgpc_type);
                        if (esize > 0 && esize <= INT_MAX)
                            temp_alias.array_element_storage_size = (int)esize;
                    }

                    if (has_alias)
                        kgpc_type_set_type_alias(array_type, &temp_alias);

                    /* Clear temp_alias.array_dimensions so it isn't freed (owned by AST) */
                    temp_alias.array_dimensions = NULL;
                }
                
                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] Pushing array: %s, array_type=%p kind=%d elem_kind=%d\n",
                        ids && ids->cur ? (char*)ids->cur : "<null>",
                        (void*)array_type, array_type ? array_type->kind : -1,
                        (array_type && array_type->kind == TYPE_KIND_ARRAY && array_type->info.array_info.element_type) ?
                            array_type->info.array_info.element_type->kind : -1);
                func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, array_type);
                destroy_kgpc_type(array_type);  /* Release creator's ref */
                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] PushArrayOntoScope_Typed returned: %d\n", func_return);
            }

            /* Greater than 0 signifies an error */
            if(func_return > 0)
            {
                semantic_error(tree->line_num, 0, "redeclaration of name %s",
                    (char *)ids->cur);
                return_val += func_return;
            }
            else
            {
                HashNode_t *decl_node = NULL;
                if (FindSymbol(&decl_node, symtab, ids->cur) != 0 && decl_node != NULL)
                {
                    if (tree->type == TREE_VAR_DECL)
                    {
                        decl_node->is_var_parameter =
                            (tree->tree_data.var_decl_data.is_var_param ||
                             tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                        mark_hashnode_unit_info(symtab, decl_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                    }
                    else
                    {
                        mark_hashnode_unit_info(symtab, decl_node,
                            tree->tree_data.arr_decl_data.defined_in_unit,
                            tree->tree_data.arr_decl_data.unit_is_public);
                        if (tree->tree_data.arr_decl_data.is_typed_const)
                            decl_node->is_typed_const = 1;
                    }
                }
            }

next_identifier:
            /* Propagate source_unit_index from AST to HashNode for unit-aware resolution */
            if (func_return == 0 && ids->cur != NULL)
            {
                int ast_source_unit = 0;
                if (tree->type == TREE_VAR_DECL)
                    ast_source_unit = tree->tree_data.var_decl_data.source_unit_index;
                else if (tree->type == TREE_ARR_DECL)
                    ast_source_unit = tree->tree_data.arr_decl_data.source_unit_index;
                if (ast_source_unit > 0)
                {
                    HashNode_t *pushed_node = NULL;
                    if (FindSymbol(&pushed_node, symtab, ids->cur) != 0 && pushed_node != NULL)
                        mark_hashnode_source_unit(pushed_node, ast_source_unit);
                }
            }
            ids = ids->next;
        }

        cur = cur->next;
        if (skip_initializer)
        {
            semcheck_restore_scope(symtab, saved_scope_for_var);
            continue;
        }

        if (tree->type == TREE_VAR_DECL &&
            tree->tree_data.var_decl_data.inline_type_alias != NULL &&
            tree->tree_data.var_decl_data.inline_type_alias->is_enum &&
            tree->tree_data.var_decl_data.inline_type_alias->enum_literals != NULL)
        {
            struct TypeAlias *inline_alias = tree->tree_data.var_decl_data.inline_type_alias;
            if (inline_alias->kgpc_type == NULL)
            {
                inline_alias->kgpc_type = create_primitive_type(ENUM_TYPE);
                if (inline_alias->kgpc_type != NULL)
                    kgpc_type_set_type_alias(inline_alias->kgpc_type, inline_alias);
            }
            if (inline_alias->kgpc_type != NULL)
            {
                int ordinal = 0;
                for (ListNode_t *lit = inline_alias->enum_literals; lit != NULL; lit = lit->next)
                {
                    const char *literal_name = (const char *)lit->cur;
                    if (literal_name == NULL)
                    {
                        ++ordinal;
                        continue;
                    }
                    HashNode_t *existing_lit = NULL;
                    if (FindIdentInCurrentScope(symtab, literal_name) == NULL)
                    {
                        if (PushConstOntoScope_Typed(symtab, (char *)literal_name, ordinal,
                                inline_alias->kgpc_type) > 0)
                        {
                            semcheck_error_with_context_at(tree->line_num, 0, tree->source_index,
                                "Error on line %d, redeclaration of enum literal %s!\n",
                                tree->line_num, literal_name);
                            ++return_val;
                        }
                        else if (FindSymbol(&existing_lit, symtab, literal_name) != 0 &&
                                 existing_lit != NULL)
                        {
                            mark_hashnode_unit_info(symtab, existing_lit,
                                tree->tree_data.var_decl_data.defined_in_unit,
                                tree->tree_data.var_decl_data.unit_is_public);
                            mark_hashnode_source_unit(existing_lit,
                                tree->tree_data.var_decl_data.source_unit_index);
                        }
                    }
                    ++ordinal;
                }
            }
        }

        if (tree->type == TREE_VAR_DECL && tree->tree_data.var_decl_data.initializer != NULL)
        {
            struct Statement *init_stmt = tree->tree_data.var_decl_data.initializer;
            
            /* Handle COMPOUND_STATEMENT initializers (from record const lowering) separately */
            if (init_stmt->type == STMT_COMPOUND_STATEMENT)
            {
                /* This is a lowered record const - just semantic check the compound statement */
                return_val += semcheck_stmt(symtab, init_stmt, INT_MAX);
            }
            else if (ids_head == NULL || ids_head->next != NULL)
            {
                semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, type inference initializers must declare a single identifier.\n",
                    tree->line_num);
                ++return_val;
            }
            else
            {
                char *var_name = (char *)ids_head->cur;
                HashNode_t *var_node = NULL;
                if (FindSymbol(&var_node, symtab, var_name) == 0 || var_node == NULL)
                {
                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, failed to resolve variable %s for initializer.\n",
                        tree->line_num, var_name);
                    ++return_val;
                }
                else
                {
                    struct Statement *init_stmt = tree->tree_data.var_decl_data.initializer;
                    struct Expression *init_expr = init_stmt->stmt_data.var_assign_data.expr;
                    int is_default_param = (init_stmt->type == STMT_VAR_ASSIGN &&
                        init_stmt->stmt_data.var_assign_data.var == NULL);
                    if (init_expr == NULL)
                    {
                        semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, initializer expression is NULL for %s.\n",
                            tree->line_num, var_name);
                        ++return_val;
                        /* Skip remaining processing for this variable but continue with the loop */
                    }
                    else
                    {
                        if (tree->tree_data.var_decl_data.is_typed_const && var_node != NULL)
                            var_node->is_typed_const = 1;

                        if (tree->tree_data.var_decl_data.is_typed_const &&
                            !tree->tree_data.var_decl_data.currency_scaled &&
                            semcheck_is_currency_type_id(tree->tree_data.var_decl_data.type_id))
                        {
                            double real_value = 0.0;
                            if (const_fold_real_expr(symtab, init_expr, &real_value) == 0)
                            {
                                long long scaled = llround(real_value * 10000.0);
                                destroy_expr(init_expr);
                                init_expr = mk_inum(tree->line_num, scaled);
                                init_stmt->stmt_data.var_assign_data.expr = init_expr;
                                tree->tree_data.var_decl_data.currency_scaled = 1;
                            }
                        }

                        KgpcType *expr_type = NULL;
                        if (init_expr->type == EXPR_RECORD_CONSTRUCTOR &&
                            (init_expr->resolved_kgpc_type == NULL ||
                             !kgpc_type_is_record(init_expr->resolved_kgpc_type)))
                        {
                            struct RecordType *record_type = NULL;
                            if (var_node->type != NULL && kgpc_type_is_record(var_node->type))
                                record_type = kgpc_type_get_record(var_node->type);
                            else if (var_node->type != NULL && kgpc_type_is_pointer(var_node->type) &&
                                var_node->type->info.points_to != NULL &&
                                kgpc_type_is_record(var_node->type->info.points_to))
                                record_type = kgpc_type_get_record(var_node->type->info.points_to);
                            else if (tree->tree_data.var_decl_data.type_id != NULL)
                            {
                                HashNode_t *type_node = NULL;
                                if (FindSymbol(&type_node, symtab,
                                        tree->tree_data.var_decl_data.type_id) >= 0 &&
                                    type_node != NULL)
                                {
                                    record_type = hashnode_get_record_type(type_node);
                                    if (record_type == NULL && type_node->type != NULL)
                                    {
                                        if (kgpc_type_is_record(type_node->type))
                                            record_type = kgpc_type_get_record(type_node->type);
                                        else if (kgpc_type_is_pointer(type_node->type) &&
                                            type_node->type->info.points_to != NULL &&
                                            kgpc_type_is_record(type_node->type->info.points_to))
                                            record_type = kgpc_type_get_record(type_node->type->info.points_to);
                                        else if (type_node->type->type_alias != NULL &&
                                            type_node->type->type_alias->target_type_id != NULL)
                                        {
                                            HashNode_t *target_node = NULL;
                                            if (FindSymbol(&target_node, symtab,
                                                    type_node->type->type_alias->target_type_id) >= 0 &&
                                                target_node != NULL)
                                            {
                                                record_type = hashnode_get_record_type(target_node);
                                                if (record_type == NULL && target_node->type != NULL &&
                                                    kgpc_type_is_record(target_node->type))
                                                    record_type = kgpc_type_get_record(target_node->type);
                                            }
                                        }
                                    }
                                }
                                if (record_type == NULL)
                                {
                                    HashNode_t *decl_type_node = NULL;
                                    if (FindSymbol(&decl_type_node, symtab,
                                            tree->tree_data.var_decl_data.type_id) >= 0 &&
                                        decl_type_node != NULL)
                                    {
                                        record_type = hashnode_get_record_type(decl_type_node);
                                        if (record_type == NULL && decl_type_node->type != NULL &&
                                            kgpc_type_is_record(decl_type_node->type))
                                            record_type = kgpc_type_get_record(decl_type_node->type);
                                    }
                                }
                            }
                            if (record_type != NULL)
                            {
                                init_expr->record_type = record_type;
                                KgpcType *record_kgpc = create_record_type(record_type);
                                if (record_kgpc != NULL)
                                {
                                    if (init_expr->resolved_kgpc_type != NULL)
                                        destroy_kgpc_type(init_expr->resolved_kgpc_type);
                                    init_expr->resolved_kgpc_type = record_kgpc;
                                    kgpc_type_retain(record_kgpc);
                                    destroy_kgpc_type(record_kgpc);
                                }
                            }
                        }
                        if (init_expr->type == EXPR_ARRAY_LITERAL &&
                            (init_expr->array_element_type == UNKNOWN_TYPE ||
                             init_expr->array_element_type_id == NULL ||
                             init_expr->array_element_size <= 0))
                        {
                            KgpcType *var_type = (var_node != NULL) ? var_node->type : NULL;
                            if ((var_type == NULL || !kgpc_type_is_array(var_type)) &&
                                tree->tree_data.var_decl_data.type_id != NULL)
                            {
                                HashNode_t *type_node = semcheck_find_preferred_type_node(symtab,
                                    tree->tree_data.var_decl_data.type_id);
                                if (type_node != NULL)
                                    var_type = type_node->type;
                            }
                            if (var_type != NULL && kgpc_type_is_array(var_type))
                            {
                                KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(var_type, symtab);
                                if (elem_type != NULL)
                                {
                                    int elem_tag = semcheck_tag_from_kgpc(elem_type);
                                    if (init_expr->array_element_type == UNKNOWN_TYPE)
                                        init_expr->array_element_type = elem_tag;
                                    if (init_expr->array_element_type_id == NULL &&
                                        elem_type->type_alias != NULL &&
                                        elem_type->type_alias->alias_name != NULL)
                                    {
                                        init_expr->array_element_type_id =
                                            strdup(elem_type->type_alias->alias_name);
                                    }
                                    if (elem_type->kind == TYPE_KIND_RECORD)
                                    {
                                        struct RecordType *rec = kgpc_type_get_record(elem_type);
                                        init_expr->array_element_record_type = rec;
                                        if (init_expr->array_element_type_id == NULL &&
                                            rec != NULL && rec->type_id != NULL)
                                        {
                                            init_expr->array_element_type_id = strdup(rec->type_id);
                                        }
                                    }
                                    if (init_expr->array_element_type_id == NULL &&
                                        elem_type->type_alias != NULL &&
                                        elem_type->type_alias->target_type_id != NULL)
                                    {
                                        init_expr->array_element_type_id =
                                            strdup(elem_type->type_alias->target_type_id);
                                    }
                                    if (init_expr->array_element_size <= 0)
                                    {
                                        long long elem_size = kgpc_type_sizeof(elem_type);
                                        if (elem_size > 0 && elem_size <= INT_MAX)
                                            init_expr->array_element_size = (int)elem_size;
                                    }
                                }
                                if (init_expr->array_element_type_id == NULL &&
                                    var_type->info.array_info.element_type_id != NULL)
                                {
                                    init_expr->array_element_type_id =
                                        strdup(var_type->info.array_info.element_type_id);
                                }
                            }
                        }
                        return_val += semcheck_expr_main(symtab, init_expr, INT_MAX, NO_MUTATE, &expr_type);

                        /* AST TRANSFORMATION: When a typed constant has a procedural target type
                         * and the initializer is a bare procedure/function name, treat it as
                         * taking the address (implicit @) rather than calling the function.
                         * e.g. const do_stop: TStopProc = DefStop; means @DefStop */
                        if (tree->tree_data.var_decl_data.is_typed_const &&
                            var_node != NULL && var_node->type != NULL &&
                            kgpc_type_is_procedure(var_node->type) &&
                            init_expr != NULL &&
                            init_expr->type != EXPR_ADDR_OF_PROC)
                        {
                            HashNode_t *proc_symbol = NULL;
                            const char *proc_name_ref = NULL;
                            if (init_expr->type == EXPR_VAR_ID)
                            {
                                proc_name_ref = init_expr->expr_data.id;
                            }
                            else if (init_expr->type == EXPR_FUNCTION_CALL &&
                                     init_expr->expr_data.function_call_data.args_expr == NULL)
                            {
                                proc_name_ref = init_expr->expr_data.function_call_data.id;
                            }
                            if (proc_name_ref != NULL &&
                                FindSymbol(&proc_symbol, symtab, proc_name_ref) != 0 &&
                                proc_symbol != NULL &&
                                (proc_symbol->hash_type == HASHTYPE_PROCEDURE ||
                                 proc_symbol->hash_type == HASHTYPE_FUNCTION))
                            {
                                init_expr->type = EXPR_ADDR_OF_PROC;
                                {
                                    const char *proc_target = NULL;
                                    if (proc_symbol->internproc_id != NULL &&
                                        proc_symbol->internproc_id[0] != '\0')
                                    {
                                        proc_target = proc_symbol->internproc_id;
                                    }
                                    else if (proc_symbol->type != NULL &&
                                             proc_symbol->type->kind == TYPE_KIND_PROCEDURE &&
                                             proc_symbol->type->info.proc_info.definition != NULL)
                                    {
                                        Tree_t *def = proc_symbol->type->info.proc_info.definition;
                                        if (def->tree_data.subprogram_data.cname_override != NULL &&
                                            def->tree_data.subprogram_data.cname_override[0] != '\0')
                                        {
                                            proc_target = def->tree_data.subprogram_data.cname_override;
                                        }
                                        else if (def->tree_data.subprogram_data.mangled_id != NULL &&
                                                 def->tree_data.subprogram_data.mangled_id[0] != '\0')
                                        {
                                            proc_target = def->tree_data.subprogram_data.mangled_id;
                                        }
                                    }
                                    if (proc_target == NULL &&
                                        proc_symbol->mangled_id != NULL &&
                                        proc_symbol->mangled_id[0] != '\0')
                                    {
                                        proc_target = proc_symbol->mangled_id;
                                    }
                                    init_expr->expr_data.addr_of_proc_data.proc_mangled_id =
                                        proc_target != NULL ? strdup(proc_target) : NULL;
                                    init_expr->expr_data.addr_of_proc_data.proc_id =
                                        proc_symbol->id ? strdup(proc_symbol->id) : NULL;
                                    init_expr->expr_data.addr_of_proc_data.source_unit_index =
                                        proc_symbol->source_unit_index;
                                }
                                /* Resolve the type NOW while the symbol is still alive. */
                                if (expr_type != NULL)
                                    destroy_kgpc_type(expr_type);
                                if (proc_symbol->type != NULL &&
                                    proc_symbol->type->kind == TYPE_KIND_PROCEDURE)
                                {
                                    kgpc_type_retain(proc_symbol->type);
                                    if (init_expr->resolved_kgpc_type != NULL)
                                        destroy_kgpc_type(init_expr->resolved_kgpc_type);
                                    init_expr->resolved_kgpc_type = create_pointer_type(proc_symbol->type);
                                }
                                else
                                {
                                    KgpcType *generic_proc = create_procedure_type(NULL, NULL);
                                    if (init_expr->resolved_kgpc_type != NULL)
                                        destroy_kgpc_type(init_expr->resolved_kgpc_type);
                                    init_expr->resolved_kgpc_type = create_pointer_type(generic_proc);
                                    if (generic_proc != NULL)
                                        destroy_kgpc_type(generic_proc);
                                }
                                expr_type = var_node->type;
                                kgpc_type_retain(expr_type);
                            }
                        }

                        if (tree->tree_data.var_decl_data.is_typed_const && var_node != NULL &&
                            init_expr->type != EXPR_ADDR && init_expr->type != EXPR_ADDR_OF_PROC &&
                            init_expr->type != EXPR_RECORD_CONSTRUCTOR &&
                            init_expr->type != EXPR_ARRAY_LITERAL)
                        {
                        if (expression_is_string(symtab, init_expr))
                            {
                                char *string_value = NULL;
                                if (evaluate_string_const_expr(symtab, init_expr, &string_value) == 0)
                                {
                                    if (var_node->const_string_value != NULL)
                                        free(var_node->const_string_value);
                                    var_node->const_string_value = string_value;
                                    if (string_value != NULL &&
                                        string_value[0] != '\0' && string_value[1] == '\0')
                                    {
                                        var_node->const_int_value = (unsigned char)string_value[0];
                                    }
                                }
                            }
                            else if (expression_contains_real_literal_impl(symtab, init_expr))
                            {
                                double real_value = 0.0;
                                if (const_fold_real_expr(symtab, init_expr, &real_value) == 0)
                                    var_node->const_real_value = real_value;
                            }
                            else if (expression_is_set_const_expr(symtab, init_expr))
                            {
                                unsigned char set_bytes[32];
                                size_t set_size = 0;
                                long long mask = 0;
                                int is_char_set = 0;
                                if (evaluate_set_const_bytes(symtab, init_expr, set_bytes, sizeof(set_bytes),
                                        &set_size, &mask, &is_char_set) == 0)
                                {
                                    unsigned char *copy = (unsigned char *)malloc(set_size);
                                    if (copy != NULL)
                                    {
                                        memcpy(copy, set_bytes, set_size);
                                        if (var_node->const_set_value != NULL)
                                            free(var_node->const_set_value);
                                        var_node->const_set_value = copy;
                                        var_node->const_set_size = (int)set_size;
                                    }
                                }
                            }
                            else
                            {
                                long long value = 0;
                                if (const_fold_int_expr(symtab, init_expr, &value) == 0)
                                    var_node->const_int_value = value;
                            }
                        }

                    int expr_tag = expr_type != NULL ? semcheck_tag_from_kgpc(expr_type) : UNKNOWN_TYPE;
                    if (expr_tag == UNKNOWN_TYPE &&
                        tree->tree_data.var_decl_data.is_typed_const &&
                        init_expr != NULL &&
                        init_expr->type == EXPR_ADDR &&
                        var_node != NULL &&
                        var_node->type != NULL)
                    {
                        int declared_tag = semcheck_tag_from_kgpc(var_node->type);
                        if (declared_tag == POINTER_TYPE || declared_tag == PROCEDURE)
                        {
                            expr_type = var_node->type;
                            kgpc_type_retain(expr_type);
                            expr_tag = declared_tag;
                            if (init_expr->resolved_kgpc_type != NULL)
                                destroy_kgpc_type(init_expr->resolved_kgpc_type);
                            init_expr->resolved_kgpc_type = var_node->type;
                            kgpc_type_retain(init_expr->resolved_kgpc_type);
                        }
                    }
                    if (expr_tag == UNKNOWN_TYPE)
                    {
                        if (!is_default_param)
                        {
                            semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, unable to infer type for %s.\n", tree->line_num, var_name);
                            ++return_val;
                        }
                    }
                    else
                    {
                        enum VarType inferred_var_type = HASHVAR_UNTYPED;
                        int normalized_type = expr_tag;

                        switch(expr_tag)
                        {
                            case INT_TYPE:
                            case LONGINT_TYPE:
                                inferred_var_type = HASHVAR_INTEGER;
                                normalized_type = (expr_tag == LONGINT_TYPE) ? LONGINT_TYPE : INT_TYPE;
                                break;
                            case BYTE_TYPE:
                            case WORD_TYPE:
                                inferred_var_type = HASHVAR_INTEGER;
                                normalized_type = expr_tag;
                                break;
                            case LONGWORD_TYPE:
                                inferred_var_type = HASHVAR_LONGINT;
                                normalized_type = LONGWORD_TYPE;
                                break;
                            case QWORD_TYPE:
                                inferred_var_type = HASHVAR_QWORD;
                                normalized_type = QWORD_TYPE;
                                break;
                            case INT64_TYPE:
                                inferred_var_type = HASHVAR_INT64;
                                normalized_type = INT64_TYPE;
                                break;
                            case BOOL:
                                inferred_var_type = HASHVAR_BOOLEAN;
                                normalized_type = BOOL;
                                break;
                            case REAL_TYPE:
                            case EXTENDED_TYPE:
                                inferred_var_type = HASHVAR_REAL;
                                normalized_type = expr_tag;
                                break;
                            case CHAR_TYPE:
                                inferred_var_type = HASHVAR_CHAR;
                                normalized_type = CHAR_TYPE;
                                if (tree->tree_data.var_decl_data.type_id == NULL)
                                    tree->tree_data.var_decl_data.type_id = strdup("char");
                                break;
                            case STRING_TYPE:
                                inferred_var_type = HASHVAR_PCHAR;
                                normalized_type = STRING_TYPE;
                                if (tree->tree_data.var_decl_data.type_id == NULL)
                                    tree->tree_data.var_decl_data.type_id = strdup("string");
                                break;
                            case SHORTSTRING_TYPE:
                                inferred_var_type = HASHVAR_SHORTSTRING;
                                normalized_type = SHORTSTRING_TYPE;
                                if (tree->tree_data.var_decl_data.type_id == NULL)
                                    tree->tree_data.var_decl_data.type_id = strdup("shortstring");
                                break;
                            case SET_TYPE:
                                inferred_var_type = HASHVAR_SET;
                                normalized_type = SET_TYPE;
                                break;
                            case ENUM_TYPE:
                                inferred_var_type = HASHVAR_ENUM;
                                normalized_type = ENUM_TYPE;
                                break;
                            case POINTER_TYPE:
                                inferred_var_type = HASHVAR_POINTER;
                                normalized_type = POINTER_TYPE;
                                break;
                            case PROCEDURE:
                                inferred_var_type = HASHVAR_PROCEDURE;
                                normalized_type = PROCEDURE;
                                break;
                            case RECORD_TYPE:
                                inferred_var_type = HASHVAR_RECORD;
                                normalized_type = RECORD_TYPE;
                                break;
                            default:
                                semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, unsupported inferred type for %s.\n",
                                    tree->line_num, var_name);
                                ++return_val;
                                inferred_var_type = HASHVAR_UNTYPED;
                                break;
                        }

                        if (tree->tree_data.var_decl_data.inferred_type)
                        {
                            if (inferred_var_type != HASHVAR_UNTYPED)
                            {
                                tree->tree_data.var_decl_data.type = normalized_type;
                                /* Replace or create KgpcType with inferred type */
                                KgpcType *inferred_kgpc_type = create_primitive_type(normalized_type);
                                if (inferred_kgpc_type != NULL)
                                {
                                    if (var_node->type != NULL)
                                    {
                                        /* Free old type and replace */
                                        destroy_kgpc_type(var_node->type);
                                    }
                                    var_node->type = inferred_kgpc_type;
                                    /* Legacy field will be populated by helper if needed */
                                }
                            }
                        }
                        else if (!is_default_param)
                        {
                            enum VarType current_var_type = get_var_type_from_node(var_node);
                            int compatible = (inferred_var_type == current_var_type);
                            
                            /* Allow mixing Integer and LongInt for initializers */
                            if (!compatible)
                            {
                                if ((inferred_var_type == HASHVAR_INTEGER || inferred_var_type == HASHVAR_LONGINT) &&
                                    (current_var_type == HASHVAR_INTEGER || current_var_type == HASHVAR_LONGINT))
                                {
                                    compatible = 1;
                                }
                            }
                            if (!compatible)
                            {
                                if (current_var_type == HASHVAR_REAL &&
                                    (inferred_var_type == HASHVAR_INTEGER ||
                                     inferred_var_type == HASHVAR_LONGINT ||
                                     inferred_var_type == HASHVAR_INT64 ||
                                     inferred_var_type == HASHVAR_QWORD))
                                {
                                    compatible = 1;
                                }
                            }
                            if (!compatible)
                            {
                                if ((current_var_type == HASHVAR_INT64 || current_var_type == HASHVAR_QWORD) &&
                                    (inferred_var_type == HASHVAR_INTEGER || inferred_var_type == HASHVAR_LONGINT ||
                                     inferred_var_type == HASHVAR_INT64 || inferred_var_type == HASHVAR_QWORD))
                                {
                                    compatible = 1;
                                }
                                else if ((inferred_var_type == HASHVAR_INT64 || inferred_var_type == HASHVAR_QWORD) &&
                                         (current_var_type == HASHVAR_INTEGER || current_var_type == HASHVAR_LONGINT))
                                {
                                    compatible = 1;
                                }
                            }

                            /* In {$H-} mode string literals resolve as ShortString.
                             * Allow typed initializers between String and ShortString. */
                            if (!compatible)
                            {
                                if (expr_tag == STRING_TYPE || expr_tag == SHORTSTRING_TYPE)
                                {
                                    KgpcType *var_type = (var_node != NULL) ? var_node->type : NULL;
                                    if (var_type != NULL && kgpc_type_is_shortstring(var_type))
                                    {
                                        compatible = 1;
                                    }
                                }
                            }

                            if (!compatible)
                            {
                                if ((current_var_type == HASHVAR_PCHAR && inferred_var_type == HASHVAR_SHORTSTRING) ||
                                    (current_var_type == HASHVAR_SHORTSTRING && inferred_var_type == HASHVAR_PCHAR))
                                {
                                    compatible = 1;
                                }
                            }

                            if (!compatible && current_var_type == HASHVAR_PCHAR && expr_tag == CHAR_TYPE)
                            {
                                compatible = 1;
                            }
                            if (!compatible &&
                                (expr_tag == STRING_TYPE || expr_tag == SHORTSTRING_TYPE) &&
                                init_expr != NULL && init_expr->type == EXPR_STRING &&
                                init_expr->expr_data.string != NULL &&
                                strlen(init_expr->expr_data.string) == 1 &&
                                var_node != NULL && semcheck_kgpc_type_is_char_like(var_node->type))
                            {
                                compatible = 1;
                                semcheck_expr_set_resolved_type(init_expr, CHAR_TYPE);
                            }
                            /* Allow string literal initializer for array of char or shortstring-like arrays */
                            if (!compatible && current_var_type == HASHVAR_ARRAY &&
                                (expr_tag == STRING_TYPE || expr_tag == SHORTSTRING_TYPE))
                            {
                                KgpcType *var_type = (var_node != NULL) ? var_node->type : NULL;
                                if (var_type != NULL && var_type->kind == TYPE_KIND_ARRAY &&
                                    var_type->info.array_info.element_type != NULL &&
                                    var_type->info.array_info.element_type->kind == TYPE_KIND_PRIMITIVE &&
                                    var_type->info.array_info.element_type->info.primitive_type_tag == CHAR_TYPE)
                                {
                                    char *string_value = NULL;
                                    size_t string_len = 0;
                                    if (init_expr != NULL && init_expr->type == EXPR_STRING &&
                                        init_expr->expr_data.string != NULL)
                                    {
                                        string_len = strlen(init_expr->expr_data.string);
                                    }
                                    else if (evaluate_string_const_expr(symtab, init_expr, &string_value) == 0)
                                    {
                                        if (string_value != NULL)
                                            string_len = strlen(string_value);
                                    }
                                    if (string_value != NULL)
                                        free(string_value);

                                    int array_size = var_type->info.array_info.end_index -
                                                     var_type->info.array_info.start_index + 1;
                                    if (array_size < 0)
                                        array_size = 0;
                                    if (string_len <= (size_t)array_size)
                                    {
                                        compatible = 1;
                                    }
                                    else
                                    {
                                        semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, 
                                            "Error on line %d, initializer string literal too long for %s (string length: %zu, array size: %d).\n",
                                            tree->line_num, var_name, string_len, array_size);
                                        ++return_val;
                                    }
                                }
                            }

                            /* Allow array literal initializer for array variables when element types match */
                            if (!compatible && current_var_type == HASHVAR_ARRAY &&
                                init_expr != NULL && init_expr->type == EXPR_ARRAY_LITERAL)
                            {
                                KgpcType *var_type = (var_node != NULL) ? var_node->type : NULL;
                                if (var_type != NULL && kgpc_type_is_array(var_type))
                                {
                                    KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(var_type, symtab);
                                    int elem_tag = semcheck_tag_from_kgpc(elem_type);
                                    int literal_elem_tag = init_expr->array_element_type;
                                    if (elem_tag != UNKNOWN_TYPE && literal_elem_tag != UNKNOWN_TYPE &&
                                        elem_tag == literal_elem_tag)
                                    {
                                        compatible = 1;
                                    }
                                }
                            }

                            /* Dynamic array typed constant with single-element initializer:
                             * const inv: TIntArray = (42);
                             * The parser strips the parentheses for a single value, producing
                             * a scalar expression instead of EXPR_ARRAY_LITERAL.  Accept if
                             * the scalar type matches the array's element type. */
                            if (!compatible && current_var_type == HASHVAR_ARRAY &&
                                init_expr != NULL && init_expr->type != EXPR_ARRAY_LITERAL)
                            {
                                KgpcType *var_type = (var_node != NULL) ? var_node->type : NULL;
                                if (var_type != NULL && kgpc_type_is_dynamic_array(var_type))
                                {
                                    KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(var_type, symtab);
                                    int elem_tag = semcheck_tag_from_kgpc(elem_type);
                                    if (elem_tag != UNKNOWN_TYPE && expr_tag != UNKNOWN_TYPE)
                                    {
                                        if (elem_tag == expr_tag ||
                                            (is_integer_type(elem_tag) && is_integer_type(expr_tag)))
                                        {
                                            compatible = 1;
                                        }
                                    }
                                }
                            }
                            
                            /* Allow pointer type initializers (including nil) for pointer variables.
                             * Both the initializer expression type and the declared variable type must
                             * be pointer types. This enables patterns like: var my_ptr: PChar = nil; */
                            if (!compatible)
                            {
                                int inferred_is_pointer = (inferred_var_type == HASHVAR_POINTER || expr_tag == POINTER_TYPE);
                                int current_is_pointer = (current_var_type == HASHVAR_POINTER);
                                int current_is_proc = (current_var_type == HASHVAR_PROCEDURE);
                                if (var_node->type != NULL)
                                {
                                    current_is_pointer |= kgpc_type_is_pointer(var_node->type);
                                    if (semcheck_tag_from_kgpc(var_node->type) == POINTER_TYPE)
                                        current_is_pointer = 1;
                                    if (var_node->type->kind == TYPE_KIND_PROCEDURE)
                                        current_is_proc = 1;
                                }
                                if (tree->tree_data.var_decl_data.type == POINTER_TYPE)
                                    current_is_pointer = 1;
                                if (tree->tree_data.var_decl_data.type == PROCEDURE)
                                    current_is_proc = 1;
                                if (inferred_is_pointer && current_is_pointer)
                                    compatible = 1;
                                if (!compatible && inferred_is_pointer && current_is_proc)
                                    compatible = 1;
                                if (!compatible && inferred_is_pointer && var_node != NULL)
                                {
                                    struct TypeAlias *alias = hashnode_get_type_alias(var_node);
                                    if (alias != NULL && (alias->is_pointer ||
                                        (alias->target_type_id != NULL &&
                                         strcasecmp(alias->target_type_id, "Pointer") == 0)))
                                        compatible = 1;
                                }
                            }

                            /* Allow string literal initializer for PAnsiChar / PChar
                             * pointer-to-char types.  The variable's KgpcType is
                             * TYPE_KIND_POINTER with a CHAR_TYPE subtarget, e.g.:
                             *   const signature: PAnsiChar = 'TPF0'; */
                            if (!compatible &&
                                (expr_tag == STRING_TYPE || expr_tag == SHORTSTRING_TYPE))
                            {
                                int var_is_pchar = 0;
                                if (var_node != NULL && var_node->type != NULL &&
                                    kgpc_type_is_pointer(var_node->type))
                                {
                                    int sub_tag = kgpc_type_get_pointer_subtype_tag(var_node->type);
                                    if (sub_tag == CHAR_TYPE)
                                        var_is_pchar = 1;
                                }
                                /* Also check by type_id (PAnsiChar, PChar, PWideChar) */
                                if (!var_is_pchar)
                                {
                                    const char *tid = tree->tree_data.var_decl_data.type_id;
                                    if (tid != NULL &&
                                        (strcasecmp(tid, "PAnsiChar") == 0 ||
                                         strcasecmp(tid, "PChar") == 0 ||
                                         strcasecmp(tid, "PWideChar") == 0))
                                    {
                                        var_is_pchar = 1;
                                    }
                                }
                                if (var_is_pchar)
                                    compatible = 1;
                            }

                            if (!compatible && current_var_type == HASHVAR_RECORD &&
                                (expr_tag == STRING_TYPE || expr_tag == SHORTSTRING_TYPE))
                            {
                                const char *record_id = NULL;
                                if (var_node->type != NULL)
                                {
                                    struct RecordType *record = kgpc_type_get_record(var_node->type);
                                    if (record != NULL && record->type_id != NULL)
                                        record_id = record->type_id;
                                }
                                if (record_id == NULL)
                                    record_id = tree->tree_data.var_decl_data.type_id;
                                if (record_id != NULL &&
                                    (strcasecmp(record_id, "TGuid") == 0 || strcasecmp(record_id, "GUID") == 0))
                                {
                                    compatible = 1;
                                }
                            }

                            if (!compatible)
                            {
                                semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, "Error on line %d, initializer type mismatch for %s.\n",
                                    tree->line_num, var_name);
                                ++return_val;
                            }
                        }
                    }
                    }  /* Close else at line 2054 */
                }  /* Close else at line 2043 */
            }  /* Close else at line 2033 */
        }  /* Close if at line 2017 */
        else if (tree->type == TREE_ARR_DECL && tree->tree_data.arr_decl_data.initializer != NULL)
        {
            return_val += semcheck_stmt(symtab, tree->tree_data.arr_decl_data.initializer, INT_MAX);
        }

        semcheck_restore_scope(symtab, saved_scope_for_var);
    }

    return return_val;
}

/* For nested type methods like "Outer.Inner__Method", register the short alias
 * "Inner__Method" so code using just the inner type name can find the method.
 * Uses structural owner metadata from the subprogram node instead of parsing
 * the dotted owner path string. */
static void register_nested_type_short_alias(SymTab_t *symtab,
    const char *owner_class_outer, const char *owner_class, const char *method_name,
    const char *mangled_id, KgpcType *type, int is_function)
{
    if (owner_class_outer == NULL || owner_class == NULL || method_name == NULL)
        return;
    char *short_name = make_method_lookup_key(owner_class, method_name);
    if (short_name == NULL)
        return;
    HashNode_t *existing_short = NULL;
    if (FindSymbol(&existing_short, symtab, short_name) == 0)
    {
        char *mangled_dup = mangled_id ? strdup(mangled_id) : NULL;
        if (is_function)
            PushFunctionOntoScope_Typed(symtab, short_name, mangled_dup, type);
        else
            PushProcedureOntoScope_Typed(symtab, short_name, mangled_dup, type);
        free(mangled_dup);
    }
    free(short_name);
}

/* Semantic check on an entire subprogram */
/* A return value greater than 0 indicates how many errors occurred */
