#include "../SemCheck_internal.h"

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

    Tree_t *prev_current_subprogram = g_semcheck_current_subprogram;
    g_semcheck_current_subprogram = subprogram;

    const char *prev_owner = semcheck_get_current_method_owner();
    char *owner_copy = NULL;
    if (subprogram->tree_data.subprogram_data.owner_class != NULL)
    {
        /* Use full dotted path when available (e.g. "TOuter.TInner") so that
         * semcheck_get_current_method_owner can walk up to outer classes. */
        const char *owner_src = subprogram->tree_data.subprogram_data.owner_class_full;
        if (owner_src == NULL)
            owner_src = subprogram->tree_data.subprogram_data.owner_class;
        owner_copy = strdup(owner_src);
        if (owner_copy != NULL)
            semcheck_set_current_method_owner(owner_copy);
    }
    else
    {
        /* Standalone procedure/function — clear method owner so that
         * semcheck_proccall doesn't misinterpret calls as method calls.
         * However, for nested functions inside a method body, preserve the
         * enclosing method owner so that unqualified calls to Self's methods
         * (e.g. Error(...) inside a nested function of TReader.ReadComponent)
         * still resolve via Self-injection. */
        if (prev_owner != NULL && max_scope_lev > 0)
        {
            /* Nested function inside a method — keep the enclosing owner */
        }
        else
        {
            semcheck_set_current_method_owner(NULL);
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
    subprogram->tree_data.subprogram_data.is_nested =
        (subprogram->tree_data.subprogram_data.owner_class == NULL &&
         subprogram->tree_data.subprogram_data.nesting_level > 1);
    int default_requires = (subprogram->tree_data.subprogram_data.nesting_level > 1 &&
        !subprogram->tree_data.subprogram_data.defined_in_unit);
    subprogram->tree_data.subprogram_data.requires_static_link = default_requires ? 1 : 0;

    /* Nested local functions inside methods still need the enclosing method's
     * owner metadata so semcheck/codegen can resolve implicit class fields via
     * Self. Preserve lexical nesting semantics above, then inherit owner info. */
    if (subprogram->tree_data.subprogram_data.owner_class == NULL &&
        max_scope_lev > 0 &&
        prev_current_subprogram != NULL &&
        prev_owner != NULL)
    {
        const char *inherit_owner = prev_current_subprogram->tree_data.subprogram_data.owner_class;
        const char *inherit_owner_full = prev_current_subprogram->tree_data.subprogram_data.owner_class_full;
        const char *inherit_owner_outer = prev_current_subprogram->tree_data.subprogram_data.owner_class_outer;

        if (inherit_owner == NULL)
            inherit_owner = prev_owner;
        if (inherit_owner_full == NULL)
            inherit_owner_full = prev_owner;

        if (inherit_owner != NULL)
            subprogram->tree_data.subprogram_data.owner_class = strdup(inherit_owner);
        if (inherit_owner_full != NULL)
            subprogram->tree_data.subprogram_data.owner_class_full = strdup(inherit_owner_full);
        if (inherit_owner_outer != NULL)
            subprogram->tree_data.subprogram_data.owner_class_outer = strdup(inherit_owner_outer);
    }

    char *id_to_use_for_lookup;
    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val = 0;
    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    /* Imported subprogram parameter declarations must retain imported-origin
     * metadata; otherwise semcheck_decls may rebind type_id-based cached types
     * (e.g. UnixType.TSize) against local shadows (e.g. Types.TSize record). */
    if (subprogram->tree_data.subprogram_data.defined_in_unit)
    {
        ListNode_t *arg_mark = subprogram->tree_data.subprogram_data.args_var;
        while (arg_mark != NULL)
        {
            if (arg_mark->type == LIST_TREE && arg_mark->cur != NULL)
            {
                Tree_t *arg_tree = (Tree_t *)arg_mark->cur;
                if (arg_tree->type == TREE_VAR_DECL)
                {
                    arg_tree->tree_data.var_decl_data.defined_in_unit = 1;
                    if (subprogram->tree_data.subprogram_data.unit_is_public)
                        arg_tree->tree_data.var_decl_data.unit_is_public = 1;
                }
                else if (arg_tree->type == TREE_ARR_DECL)
                {
                    arg_tree->tree_data.arr_decl_data.defined_in_unit = 1;
                    if (subprogram->tree_data.subprogram_data.unit_is_public)
                        arg_tree->tree_data.arr_decl_data.unit_is_public = 1;
                }
            }
            arg_mark = arg_mark->next;
        }
    }

    // --- Name Mangling Logic ---
    // Only set mangled_id if not already set by predeclare_subprogram (which handles
    // nested functions with unique parent$child naming)
    if (subprogram->tree_data.subprogram_data.mangled_id == NULL) {
        static int debug_external = -1;
        if (debug_external == -1)
            debug_external = (kgpc_getenv("KGPC_DEBUG_EXTERNAL") != NULL);
        const char *explicit_name = subprogram->tree_data.subprogram_data.cname_override;
        if (explicit_name != NULL) {
            if (subprogram->tree_data.subprogram_data.statement_list != NULL) {
                subprogram->tree_data.subprogram_data.mangled_id = strdup(explicit_name);
            } else {
                char *overload_mangled = MangleFunctionName(
                    subprogram->tree_data.subprogram_data.id,
                    subprogram->tree_data.subprogram_data.args_var,
                    symtab);
                if (overload_mangled != NULL)
                    subprogram->tree_data.subprogram_data.mangled_id = overload_mangled;
                else
                    subprogram->tree_data.subprogram_data.mangled_id = strdup(explicit_name);
            }
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
    
    if (subprogram->tree_data.subprogram_data.cached_predecl_node != NULL)
    {
        existing_decl = (HashNode_t *)subprogram->tree_data.subprogram_data.cached_predecl_node;
        already_declared = 1;
    }
    
    if (!already_declared)
    {
        /* For overloaded functions, find the correct overload by matching mangled name */
        if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
        {
            SubprogramPredeclLookup lookup = semcheck_lookup_subprogram_predecl(
                symtab,
                subprogram,
                id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);

            if (kgpc_getenv("KGPC_DEBUG_OVERLOAD_MATCH") != NULL)
            {
                ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
                fprintf(stderr, "[KGPC] Matching impl %s mangled=%s\n",
                    id_to_use_for_lookup ? id_to_use_for_lookup : "<null>",
                    subprogram->tree_data.subprogram_data.mangled_id);
                for (ListNode_t *debug_cur = all_matches; debug_cur != NULL; debug_cur = debug_cur->next)
                {
                    HashNode_t *debug_cand = (HashNode_t *)debug_cur->cur;
                    fprintf(stderr, "  candidate: id=%s mangled=%s\n",
                        debug_cand && debug_cand->id ? debug_cand->id : "<null>",
                        debug_cand && debug_cand->mangled_id ? debug_cand->mangled_id : "<null>");
                }
                if (all_matches != NULL)
                    DestroyList(all_matches);
            }

            existing_decl = lookup.exact_match != NULL ?
                lookup.exact_match : lookup.first_mangled_match;
            already_declared = (existing_decl != NULL);
        }
        
        /* Fallback to simple lookup if no mangled name or no match found */
        if (!already_declared)
            already_declared = (FindSymbol(&existing_decl, symtab, id_to_use_for_lookup) != 0);
    }

    if (already_declared && existing_decl != NULL &&
        subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        if (existing_decl->mangled_id != NULL)
            free(existing_decl->mangled_id);
        existing_decl->mangled_id = strdup(subprogram->tree_data.subprogram_data.mangled_id);
    }

    /**** FIRST PLACING SUBPROGRAM ON THE CURRENT SCOPE ****/

    /* For unit subprograms (source_unit_index > 0), temporarily switch
     * current_scope to the unit's scope so that EnterScope(SCOPE_SUBPROGRAM)
     * creates the subprogram as a child of the unit scope, not the program
     * scope.  This ensures unit code walks:
     *   SCOPE_SUBPROGRAM -> SCOPE_UNIT -> SCOPE_BUILTIN
     * and never sees program-local declarations. */
    ScopeNode *saved_scope_for_unit = NULL;
    {
        int unit_idx = subprogram->tree_data.subprogram_data.source_unit_index;
        if (unit_idx > 0 && symtab->unit_scopes[unit_idx] != NULL)
        {
            saved_scope_for_unit = symtab->current_scope;
            symtab->current_scope = symtab->unit_scopes[unit_idx];
        }
    }

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
                            if (FindSymbol(&elem_type_node, symtab, param_tree->tree_data.arr_decl_data.type_id) != 0 &&
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
            if (subprogram->tree_data.subprogram_data.defined_in_unit)
            {
                /* Search the TARGET TABLE where the node was just pushed,
                 * not the full scope, to avoid marking a local overload. */
                HashTable_t *target_table = SymTab_GetTargetTable(symtab);
                HashNode_t *pushed = FindIdentInTable(target_table, id_to_use_for_lookup);
                if (pushed != NULL)
                {
                    pushed->defined_in_unit = 1;
                    if (subprogram->tree_data.subprogram_data.unit_is_public)
                        pushed->unit_is_public = 1;
                }
            }
            /* For nested type methods, also register the short alias. */
            register_nested_type_short_alias(symtab,
                subprogram->tree_data.subprogram_data.owner_class_outer,
                subprogram->tree_data.subprogram_data.owner_class,
                subprogram->tree_data.subprogram_data.method_name,
                subprogram->tree_data.subprogram_data.mangled_id, proc_type, 0);
            /* Still set existing_decl for subsequent code that needs it */
            FindSymbol(&existing_decl, symtab, id_to_use_for_lookup);
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

        copy_method_identity_to_node(existing_decl, subprogram);
        semcheck_propagate_method_identity(symtab, subprogram);

        /* Propagate varargs flag from tree to hash node */
        if (existing_decl != NULL && subprogram->tree_data.subprogram_data.is_varargs)
            existing_decl->is_varargs = 1;

        /* Propagate INTERNPROC identifier from tree to hash node */
        if (existing_decl != NULL && subprogram->tree_data.subprogram_data.internproc_id != NULL)
        {
            if (existing_decl->internproc_id != NULL)
                free(existing_decl->internproc_id);
            existing_decl->internproc_id = strdup(subprogram->tree_data.subprogram_data.internproc_id);
        }
        if (existing_decl != NULL && subprogram->tree_data.subprogram_data.internconst_id != NULL)
        {
            if (existing_decl->internconst_id != NULL)
                free(existing_decl->internconst_id);
            existing_decl->internconst_id = strdup(subprogram->tree_data.subprogram_data.internconst_id);
        }

        EnterScope(symtab,
            subprogram->tree_data.subprogram_data.source_unit_index);

        /* For method implementations, add class vars to scope */
        add_class_vars_to_method_scope(symtab, subprogram);
        /* For nested types (e.g. TOuter.TInner), also add outer class
         * vars/consts to scope. owner_class_full contains the dotted path. */
        if (subprogram->tree_data.subprogram_data.owner_class_full != NULL)
            add_outer_class_vars_to_method_scope(symtab, subprogram);

        /* Self-push: make procedure visible in its own body for recursive
         * and sibling overload calls (e.g. TThread.ForceQueue calling static
         * ForceQueue from instance ForceQueue).  semcheck_scope_level_for_candidate
         * skips SCOPE_SUBPROGRAM proc/func entries to find the real declaring
         * scope level. */
        if (existing_decl != NULL && existing_decl->type != NULL)
        {
            PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id,
                existing_decl->type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
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

        /* If the predeclare step could not resolve the type (e.g., inline array,
         * type alias not yet resolved at predeclare time, or array with NULL
         * element type from incomplete resolution), build it now and update
         * the existing declaration. */
        int return_type_needs_rebuild =
            (return_kgpc_type == NULL) ||
            (return_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
             return_kgpc_type->info.primitive_type_tag == UNKNOWN_TYPE) ||
            (return_kgpc_type->kind == TYPE_KIND_ARRAY &&
             return_kgpc_type->info.array_info.element_type == NULL);
        if (return_type_needs_rebuild)
        {
            int before_ret = return_val;
            KgpcType *rebuilt = build_function_return_type(subprogram, symtab, &return_val, 0);
            int rebuilt_is_incomplete =
                (rebuilt == NULL) ||
                (rebuilt->kind == TYPE_KIND_PRIMITIVE &&
                 rebuilt->info.primitive_type_tag == UNKNOWN_TYPE) ||
                (rebuilt->kind == TYPE_KIND_ARRAY &&
                 rebuilt->info.array_info.element_type == NULL);
            if (!rebuilt_is_incomplete)
            {
                return_kgpc_type = rebuilt;
                /* Update the existing declaration's return type so future
                 * references (e.g., from semcheck_subprograms Pass 1.5) see
                 * the resolved type. */
                if (existing_decl != NULL && existing_decl->type != NULL &&
                    existing_decl->type->kind == TYPE_KIND_PROCEDURE)
                {
                    KgpcType *old_ret = existing_decl->type->info.proc_info.return_type;
                    if (rebuilt != old_ret)
                    {
                        kgpc_type_retain(rebuilt);
                        if (old_ret != NULL)
                            kgpc_type_release(old_ret);
                        existing_decl->type->info.proc_info.return_type = rebuilt;
                    }
                }
            }
            else
            {
                if (rebuilt == NULL && return_kgpc_type == NULL)
                    return_kgpc_type = rebuilt;
                /* else keep the existing UNKNOWN_TYPE */
            }
            semcheck_debug_error_step("build_return_type", subprogram, before_ret, return_val);
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
            if (subprogram->tree_data.subprogram_data.defined_in_unit)
            {
                /* Search the TARGET TABLE where the node was just pushed,
                 * not the full scope, to avoid marking a local overload. */
                HashTable_t *target_table = SymTab_GetTargetTable(symtab);
                HashNode_t *pushed = FindIdentInTable(target_table, id_to_use_for_lookup);
                if (pushed != NULL)
                {
                    pushed->defined_in_unit = 1;
                    if (subprogram->tree_data.subprogram_data.unit_is_public)
                        pushed->unit_is_public = 1;
                }
            }
            /* For nested type methods like "Outer.Inner__Method", also register
             * the short alias "Inner__Method" so that generic method bodies using
             * just the inner type name can find the method. */
            register_nested_type_short_alias(symtab,
                subprogram->tree_data.subprogram_data.owner_class_full,
                subprogram->tree_data.subprogram_data.owner_class,
                subprogram->tree_data.subprogram_data.method_name,
                subprogram->tree_data.subprogram_data.mangled_id, func_type, 1);
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

        copy_method_identity_to_node(existing_decl, subprogram);
        semcheck_propagate_method_identity(symtab, subprogram);

        /* Propagate varargs flag from tree to hash node */
        if (existing_decl != NULL && subprogram->tree_data.subprogram_data.is_varargs)
            existing_decl->is_varargs = 1;

        /* Propagate INTERNPROC identifier from tree to hash node */
        if (existing_decl != NULL && subprogram->tree_data.subprogram_data.internproc_id != NULL)
        {
            if (existing_decl->internproc_id != NULL)
                free(existing_decl->internproc_id);
            existing_decl->internproc_id = strdup(subprogram->tree_data.subprogram_data.internproc_id);
        }
        if (existing_decl != NULL && subprogram->tree_data.subprogram_data.internconst_id != NULL)
        {
            if (existing_decl->internconst_id != NULL)
                free(existing_decl->internconst_id);
            existing_decl->internconst_id = strdup(subprogram->tree_data.subprogram_data.internconst_id);
        }

        EnterScope(symtab,
            subprogram->tree_data.subprogram_data.source_unit_index);
        if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL)
            fprintf(stderr, "[KGPC] semcheck_subprogram (func): EnterScope for %s\n",
                subprogram->tree_data.subprogram_data.id);

        /* For method implementations, add class vars to scope */
        add_class_vars_to_method_scope(symtab, subprogram);
        /* For nested types (e.g. TOuter.TInner), also add outer class
         * vars/consts to scope. owner_class_full contains the dotted path. */
        if (subprogram->tree_data.subprogram_data.owner_class_full != NULL)
            add_outer_class_vars_to_method_scope(symtab, subprogram);

        // **THIS IS THE FIX FOR THE RETURN VALUE**:
        // Use the ORIGINAL name for the internal return variable with KgpcType
        // Always use _Typed variant, even if KgpcType is NULL
        PushFuncRetOntoScope_Typed(symtab, id_to_use_for_lookup, return_kgpc_type);
        
/* Also add "Result" as an alias for the return variable for Pascal compatibility */
        /* Check if "Result" is already used in the current scope */
        HashNode_t *result_check = NULL;
        HashTable_t *cur_hash = symtab->current_scope->table;
        result_check = (cur_hash != NULL) ? FindIdentInTable(cur_hash, "Result") : NULL;
        if (result_check == NULL)
        {
            /* "Result" is not already declared in this scope, so add it as an alias */
            PushFuncRetOntoScope_Typed(symtab, "Result", return_kgpc_type);
            if (kgpc_getenv("KGPC_DEBUG_RESULT") != NULL)
            {
                fprintf(stderr,
                    "[KGPC] add Result alias for %s type=%s\n",
                    subprogram->tree_data.subprogram_data.id ?
                        subprogram->tree_data.subprogram_data.id : "<anon>",
                    kgpc_type_to_string(return_kgpc_type));
            }
        }
        else if (kgpc_getenv("KGPC_DEBUG_RESULT") != NULL)
        {
            fprintf(stderr,
                "[KGPC] Result alias already exists for %s existing_type=%s\n",
                subprogram->tree_data.subprogram_data.id ?
                    subprogram->tree_data.subprogram_data.id : "<anon>",
                kgpc_type_to_string(result_check->type));
        }

        /* For operator declarations with named result variables (e.g., "operator :=(src) dest: variant"),
         * push the named result variable as an additional alias for the return variable. */
        if (subprogram->tree_data.subprogram_data.result_var_name != NULL)
        {
            PushFuncRetOntoScope_Typed(symtab, subprogram->tree_data.subprogram_data.result_var_name, return_kgpc_type);
        }

        /* For class/record methods (but NOT type helpers), add an alias using
         * the unmangled method name so that FuncName.field works as result-
         * variable field access.  Type helpers use Result for the return value
         * and the method name should still refer to external functions. */
        const char *alias_suffix = subprogram->tree_data.subprogram_data.method_name;
        int is_type_helper_method = 0;
        if (subprogram->tree_data.subprogram_data.owner_class != NULL)
            is_type_helper_method = from_cparser_is_type_helper(
                subprogram->tree_data.subprogram_data.owner_class);
        if (alias_suffix != NULL && alias_suffix[0] != '\0' && !is_type_helper_method)
        {
            size_t alias_len = strlen(alias_suffix);
            if (alias_len > 0 && alias_len < 128)
            {
                char alias_buf[128];
                memcpy(alias_buf, alias_suffix, alias_len);
                alias_buf[alias_len] = '\0';

                /* Only skip if the name is already a function-return in the
                 * current scope (to avoid duplicates).  Outer-scope entries
                 * (like the method itself) should be shadowed by the return
                 * variable alias. */
                HashNode_t *suffix_check = NULL;
                HashTable_t *cur_tbl = symtab->current_scope->table;
                if (cur_tbl != NULL)
                    suffix_check = FindIdentInTable(cur_tbl, alias_buf);
                if (suffix_check == NULL ||
                    suffix_check->hash_type != HASHTYPE_FUNCTION_RETURN)
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
        if (FindSymbol(&existing_decl, symtab, id_to_use_for_lookup) != 0 &&
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
    if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL) {
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
    /* Register generic type parameters (e.g., T, U) as opaque types in the function scope.
     * This allows parameters of type T and expressions using T to pass semantic checking. */
    if (subprogram->tree_data.subprogram_data.num_generic_type_params > 0) {
        for (int i = 0; i < subprogram->tree_data.subprogram_data.num_generic_type_params; i++) {
            const char *tparam = subprogram->tree_data.subprogram_data.generic_type_params[i];
            assert(tparam != NULL);
            KgpcType *opaque = create_primitive_type(POINTER_TYPE);
            PushTypeOntoScope_Typed(symtab, (char *)tparam, opaque);
            destroy_kgpc_type(opaque);
        }
    } else {
        /* Some parsed method implementations (notably generic class methods in FPC RTL)
         * may omit explicit generic_type_params metadata even though their signatures
         * contain placeholders like T/U. Infer only single-letter uppercase params. */
        ListNode_t *arg_node = subprogram->tree_data.subprogram_data.args_var;
        while (arg_node != NULL) {
            if (arg_node->type == LIST_TREE && arg_node->cur != NULL) {
                Tree_t *arg_tree = (Tree_t *)arg_node->cur;
                const char *type_id = NULL;
                if (arg_tree->type == TREE_VAR_DECL)
                    type_id = arg_tree->tree_data.var_decl_data.type_id;
                else if (arg_tree->type == TREE_ARR_DECL)
                    type_id = arg_tree->tree_data.arr_decl_data.type_id;
                if (type_id != NULL && type_id[0] >= 'A' && type_id[0] <= 'Z' &&
                    type_id[1] == '\0')
                {
                    HashNode_t *existing = NULL;
                    if (FindSymbol(&existing, symtab, type_id) == 0) {
                        KgpcType *opaque = create_primitive_type(POINTER_TYPE);
                        PushTypeOntoScope_Typed(symtab, (char *)type_id, opaque);
                        destroy_kgpc_type(opaque);
                    }
                }
            }
            arg_node = arg_node->next;
        }

        const char *ret_type_id = subprogram->tree_data.subprogram_data.return_type_id;
        if (ret_type_id != NULL && ret_type_id[0] >= 'A' && ret_type_id[0] <= 'Z' &&
            ret_type_id[1] == '\0')
        {
            HashNode_t *existing = NULL;
            if (FindSymbol(&existing, symtab, ret_type_id) == 0) {
                KgpcType *opaque = create_primitive_type(POINTER_TYPE);
                PushTypeOntoScope_Typed(symtab, (char *)ret_type_id, opaque);
                destroy_kgpc_type(opaque);
            }
        }
    }
    {
        int before_args = return_val;
        int saved_imported_unit = g_semcheck_imported_decl_unit_index;
        if (subprogram->tree_data.subprogram_data.defined_in_unit)
        {
            g_semcheck_imported_decl_unit_index = subprogram->tree_data.subprogram_data.source_unit_index;
            if (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL)
            {
                /* Check if any arg has TSize type_id */
                ListNode_t *a = subprogram->tree_data.subprogram_data.args_var;
                while (a != NULL)
                {
                    if (a->type == LIST_TREE && a->cur != NULL)
                    {
                        Tree_t *ad = (Tree_t *)a->cur;
                        if (ad->type == TREE_VAR_DECL && ad->tree_data.var_decl_data.type_id != NULL &&
                            pascal_identifier_equals(ad->tree_data.var_decl_data.type_id, "TSize"))
                        {
                            fprintf(stderr, "[SUBPROGRAM_TSIZE] func=%s source_unit_idx=%d param_type_id=%s\n",
                                subprogram->tree_data.subprogram_data.id ? subprogram->tree_data.subprogram_data.id : "<null>",
                                subprogram->tree_data.subprogram_data.source_unit_index,
                                ad->tree_data.var_decl_data.type_id);
                        }
                    }
                    a = a->next;
                }
            }
        }
        return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.args_var);
        g_semcheck_imported_decl_unit_index = saved_imported_unit;
        semcheck_debug_error_step("args_decls", subprogram, before_args, return_val);
    }
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after args: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    /* Ensure parameter identifiers are present in scope even if parsing produced
     * incomplete type info (e.g., untyped const params in helper methods). */
    {
        ListNode_t *param_node = subprogram->tree_data.subprogram_data.args_var;
        while (param_node != NULL)
        {
            if (param_node->type == LIST_TREE && param_node->cur != NULL)
            {
                Tree_t *param_tree = (Tree_t *)param_node->cur;
                ListNode_t *ids = NULL;
                if (param_tree->type == TREE_VAR_DECL)
                    ids = param_tree->tree_data.var_decl_data.ids;
                else if (param_tree->type == TREE_ARR_DECL)
                    ids = param_tree->tree_data.arr_decl_data.ids;

                while (ids != NULL)
                {
                    HashNode_t *existing = NULL;
                    if (FindSymbol(&existing, symtab, ids->cur) == 0)
                    {
                        KgpcType *param_type = NULL;
                        if (param_tree->type == TREE_VAR_DECL)
                        {
                            param_type = param_tree->tree_data.var_decl_data.cached_kgpc_type;
                            if (param_type == NULL && param_tree->tree_data.var_decl_data.type_id != NULL)
                            {
                                int builtin_tag = semcheck_map_builtin_type_name_local(
                                    param_tree->tree_data.var_decl_data.type_id);
                                /* Honour the parser's {$H-} remap for parameters. */
                                if (builtin_tag == STRING_TYPE &&
                                    param_tree->tree_data.var_decl_data.type == SHORTSTRING_TYPE)
                                {
                                    builtin_tag = SHORTSTRING_TYPE;
                                }
                                if (builtin_tag != UNKNOWN_TYPE)
                                    param_type = create_primitive_type(builtin_tag);
                            }
                            if (param_type != NULL &&
                                param_type == param_tree->tree_data.var_decl_data.cached_kgpc_type)
                            PushVarOntoScope_Typed(symtab, (char *)ids->cur, param_type);
                            if (param_type != NULL &&
                                param_type != param_tree->tree_data.var_decl_data.cached_kgpc_type)
                            {
                                destroy_kgpc_type(param_type);
                            }
                        }
                        else if (param_tree->type == TREE_ARR_DECL)
                        {
                            PushArrayOntoScope_Typed(symtab, (char *)ids->cur, NULL);
                        }

                        if (FindSymbol(&existing, symtab, ids->cur) != 0 && existing != NULL)
                        {
                            int is_var_param = 0;
                            int is_untyped = 0;
                            if (param_tree->type == TREE_VAR_DECL)
                            {
                                is_var_param = param_tree->tree_data.var_decl_data.is_var_param;
                                is_untyped = param_tree->tree_data.var_decl_data.is_untyped_param;
                            }
                            existing->is_var_parameter = (is_var_param || is_untyped) ? 1 : 0;
                        }
                    }
                    ids = ids->next;
                }
            }
            param_node = param_node->next;
        }
    }

    /* Ensure methods always have an implicit Self in scope when missing.
     * This is critical for record/class method bodies that access bare fields
     * (e.g. cx/cy) and for helper methods with expanded signatures. */
    if (kgpc_getenv("KGPC_ASSERT_STATIC_SELF") != NULL)
    {
        if (subprogram->tree_data.subprogram_data.is_static_method)
        {
            HashNode_t *self_node = NULL;
            assert(FindSymbol(&self_node, symtab, "Self") != 0);
        }
    }
    if (!subprogram->tree_data.subprogram_data.is_static_method)
    {
        HashNode_t *self_node = NULL;
        const char *owner_id = semcheck_get_current_method_owner();
        const char *trace_nonlocal = kgpc_getenv("KGPC_TRACE_NONLOCAL");
        struct RecordType *owner_record = NULL;
        int self_is_var_param = 0;
        if (trace_nonlocal != NULL &&
            subprogram->tree_data.subprogram_data.id != NULL)
        {
            fprintf(stderr,
                "[KGPC_TRACE_NONLOCAL] self-inject subprogram=%s owner_ctx=%s owner_class=%s nesting=%d\n",
                subprogram->tree_data.subprogram_data.id,
                owner_id != NULL ? owner_id : "<null>",
                subprogram->tree_data.subprogram_data.owner_class != NULL ?
                    subprogram->tree_data.subprogram_data.owner_class : "<null>",
                subprogram->tree_data.subprogram_data.nesting_level);
        }
        if (owner_id != NULL)
        {
            HashNode_t *owner_node = semcheck_find_owner_record_type_node(symtab, owner_id);
            if (owner_node == NULL)
                owner_node = semcheck_find_preferred_type_node(symtab, owner_id);
            if (owner_node != NULL)
                owner_record = get_record_type_from_node(owner_node);
        }
        if (owner_record != NULL)
        {
            KgpcType *self_type = NULL;

            if (owner_record->is_type_helper && owner_record->helper_base_type_id != NULL)
            {
                self_is_var_param = semcheck_helper_self_is_var(symtab,
                    owner_record->helper_base_type_id);
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
            }
            else
            {
                KgpcType *owner_kgpc = create_record_type(owner_record);
                if (owner_kgpc != NULL && record_type_is_class(owner_record))
                {
                    KgpcType *ptr = create_pointer_type(owner_kgpc);
                    if (ptr != NULL)
                    {
                        kgpc_type_release(owner_kgpc);
                        owner_kgpc = ptr;
                    }
                    self_is_var_param = 0;
                }
                else
                {
                    /* Non-class records pass Self by reference. */
                    self_is_var_param = 1;
                }
                self_type = owner_kgpc;
            }

            if (self_type != NULL)
            {
                    if (FindSymbol(&self_node, symtab, "Self") == 0)
                    {
                        /* Push Self to the method scope (stack head) so it is cleaned up
                         * when the method exits. */
                        PushVarOntoScope_Typed(symtab, "Self", self_type);
                        if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
                            self_node->is_var_parameter = self_is_var_param;
                    if (trace_nonlocal != NULL)
                    {
                        fprintf(stderr,
                            "[KGPC_TRACE_NONLOCAL] self-inject pushed Self for subprogram=%s type=%s is_var=%d\n",
                            subprogram->tree_data.subprogram_data.id != NULL ?
                                subprogram->tree_data.subprogram_data.id : "<null>",
                            kgpc_type_to_string(self_type),
                            self_is_var_param);
                    }
                }
                else if (self_node->type == NULL || !kgpc_type_equals(self_node->type, self_type))
                {
                    if (self_node->type != NULL)
                        destroy_kgpc_type(self_node->type);
                    kgpc_type_retain(self_type);
                    self_node->type = self_type;
                }
                if (self_node != NULL)
                    self_node->is_var_parameter = self_is_var_param;
                if (trace_nonlocal != NULL)
                {
                    fprintf(stderr,
                        "[KGPC_TRACE_NONLOCAL] self-inject final Self node=%p type=%s is_var=%d\n",
                        (void *)self_node,
                        self_node != NULL && self_node->type != NULL ?
                            kgpc_type_to_string(self_node->type) : "<null>",
                        self_is_var_param);
                }
                destroy_kgpc_type(self_type);
            }
        }
    }

    {
        int before_local = return_val;
        return_val += predeclare_enum_literals(symtab, subprogram->tree_data.subprogram_data.type_declarations);
        /* Pre-declare types so they're available for const expressions like High(MyType) */
        return_val += predeclare_types(symtab, subprogram->tree_data.subprogram_data.type_declarations);
        return_val += semcheck_const_decls(symtab, subprogram->tree_data.subprogram_data.const_declarations);
        return_val += semcheck_type_decls(symtab, subprogram->tree_data.subprogram_data.type_declarations);
        return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.declarations);
        semcheck_debug_error_step("local_decls", subprogram, before_local, return_val);
    }
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after decls: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    {
        int before_nested = return_val;
        return_val += semcheck_subprograms(symtab, subprogram->tree_data.subprogram_data.subprograms,
                        new_max_scope, subprogram);
        semcheck_debug_error_step("nested_subprograms", subprogram, before_nested, return_val);
    }

    body = subprogram->tree_data.subprogram_data.statement_list;
    if (body == NULL)
    {
        g_semcheck_current_subprogram = prev_current_subprogram;
        LeaveScope(symtab);
        if (saved_scope_for_unit != NULL)
            symtab->current_scope = saved_scope_for_unit;
#ifdef DEBUG
        fprintf(stderr, "DEBUG: semcheck_subprogram %s returning (no body): %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
        return return_val;
    }

    /* Suppress source_index usage for unit-imported subprogram bodies.
     * Their expressions carry source_index values from a different
     * preprocessed buffer, and resolve_error_source_context cannot
     * disambiguate buffers, producing misleading file:line locations. */
    int saved_suppress = g_semcheck_error_suppress_source_index;
    int saved_error_source_index = g_semcheck_error_source_index;
    const char *saved_error_unit_name = g_semcheck_error_unit_name;
    if (subprogram->tree_data.subprogram_data.defined_in_unit)
    {
        /* Don't suppress source_index — the buffer registry can disambiguate
         * unit buffers via globally unique offsets, allowing {#line} directive
         * resolution to show original include file/line in error messages. */
        g_semcheck_error_suppress_source_index = 0;
        /* Show unit name instead of misleading main-program file:line */
        if (subprogram->tree_data.subprogram_data.source_unit_index > 0)
        {
            const char *uname = unit_registry_get(subprogram->tree_data.subprogram_data.source_unit_index);
            g_semcheck_error_unit_name = uname ? uname : "<unit>";
        }
    }

    /* Functions cannot have side effects, so need to call a special function in that case */
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        {
            int before_body = return_val;
            return_val += semcheck_stmt(symtab, body, new_max_scope);
            semcheck_debug_error_step("proc_body", subprogram, before_body, return_val);
        }
    }
    else
    {
        assert(FindSymbol(&hash_return, symtab, subprogram->tree_data.subprogram_data.id)
                    != 0);

        ResetHashNodeStatus(hash_return);
        int func_stmt_ret = 0;
        {
            int before_body = return_val;
            func_stmt_ret = semcheck_func_stmt(symtab, body, new_max_scope);
            return_val += func_stmt_ret;
            semcheck_debug_error_step("func_body", subprogram, before_body, return_val);
        }
#ifdef DEBUG
        if (func_stmt_ret > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after semcheck_func_stmt: %d\n", subprogram->tree_data.subprogram_data.id, func_stmt_ret);
#endif

        /* Allow functions with asm blocks to skip explicit return assignment */
        /* Constructors implicitly yield the constructed instance, so do not
         * require an explicit assignment to the return variable. */
        int is_constructor = subprogram->tree_data.subprogram_data.is_constructor;

        /* Check if either the function name or "Result" was assigned to */
        int return_was_assigned = is_constructor ? 1 : (hash_return->mutated != NO_MUTATE);
        if (!return_was_assigned)
        {
            /* Also check if "Result" was mutated */
            HashNode_t *result_node = NULL;
            if (FindSymbol(&result_node, symtab, "Result") != 0 && result_node != NULL)
            {
                return_was_assigned = (result_node->mutated != NO_MUTATE);
            }
        }

        /* Methods use mangled identifiers like Class__Func; allow assignments to the
         * unmangled method name to satisfy the return check. */
        if (!return_was_assigned && subprogram->tree_data.subprogram_data.method_name != NULL) {
            const char *mname = subprogram->tree_data.subprogram_data.method_name;
            HashNode_t *suffix_node = NULL;
            if (FindSymbol(&suffix_node, symtab, mname) != 0 && suffix_node != NULL) {
                return_was_assigned = (suffix_node->mutated != NO_MUTATE);
            }
        }
        
        /* Pascal allows functions to exit without explicitly assigning the result;
         * the value is simply uninitialized. Do not emit a warning for this. */
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
                    if (subprogram->tree_data.subprogram_data.is_varargs)
                        node->is_varargs = 1;
                }
            }
            iter = iter->next;
        }
        DestroyList(defs);
    }

    g_semcheck_current_subprogram = prev_current_subprogram;
    LeaveScope(symtab);
    if (saved_scope_for_unit != NULL)
        symtab->current_scope = saved_scope_for_unit;

    /* Restore error context / suppress flag after body processing. */
    g_semcheck_error_suppress_source_index = saved_suppress;
    g_semcheck_error_source_index = saved_error_source_index;
    g_semcheck_error_unit_name = saved_error_unit_name;

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s returning at end: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
    if (owner_copy != NULL)
        free(owner_copy);
    /* Always restore the previous method owner, even for standalone procedures.
     * Without this, the owner from a previously semchecked method leaks into
     * subsequent standalone procedures, causing semcheck_proccall to
     * misinterpret calls as method calls (e.g. Assign → assign_t_ss recursion). */
    semcheck_set_current_method_owner(prev_owner);

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
    SubprogramPredeclLookup lookup;

    assert(symtab != NULL);
    assert(subprogram != NULL);
    assert(subprogram->type == TREE_SUBPROGRAM);

    ScopeNode *saved_scope_for_unit = NULL;
    if (parent_subprogram == NULL &&
        subprogram->tree_data.subprogram_data.source_unit_index > 0)
    {
        saved_scope_for_unit = semcheck_switch_to_unit_scope(
            symtab, subprogram->tree_data.subprogram_data.source_unit_index);
    }

    char *id_to_use_for_lookup;

    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    /* Debug output for procedure predeclaration */
    if (kgpc_getenv("KGPC_DEBUG_PREDECLARE_PROC") != NULL)
    {
        fprintf(stderr, "[PREDECLARE_PROC] %s (line %d) parent=%s\n",
                subprogram->tree_data.subprogram_data.id,
                subprogram->line_num,
                parent_subprogram != NULL ? parent_subprogram->tree_data.subprogram_data.id : "(null)");
    }

    // --- Name Mangling Logic ---
    /* Preserve any dotted class prefix from the original mangled_id set by
     * convert_method_impl (e.g. "TOuter.TInner__DoWork").  This is needed
     * so that semcheck_get_current_method_owner can walk up to outer classes
     * when resolving class vars / consts in nested object methods. */
    /* Operator functions set mangled_id in from_cparser.c to include the
     * return type suffix for unique assembly labels.  Preserve it instead
     * of overwriting with MangleFunctionName (which only uses param types
     * and would collide for overloads differing only in return type). */
    int keep_existing_mangled = (subprogram->tree_data.subprogram_data.is_operator &&
                                 subprogram->tree_data.subprogram_data.mangled_id != NULL);
    if (!keep_existing_mangled && subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        free(subprogram->tree_data.subprogram_data.mangled_id);
        subprogram->tree_data.subprogram_data.mangled_id = NULL;
    }

    const char *predeclare_name = subprogram->tree_data.subprogram_data.cname_override;
    if (keep_existing_mangled) {
        /* Operator mangled_id already set — keep it */
    } else if (predeclare_name != NULL) {
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
                subprogram->tree_data.subprogram_data.is_nested_scope = 1;
            } else {
                subprogram->tree_data.subprogram_data.mangled_id = base_mangled;
            }
        } else {
            subprogram->tree_data.subprogram_data.mangled_id = base_mangled;
        }
    }

    if (kgpc_getenv("KGPC_DEBUG_PREDECLARE_PROC") != NULL)
    {
        fprintf(stderr, "[PREDECLARE_PROC]   -> mangled_id=%s\n",
                subprogram->tree_data.subprogram_data.mangled_id);
    }

    id_to_use_for_lookup = subprogram->tree_data.subprogram_data.id;
    subprogram->tree_data.subprogram_data.cached_predecl_node = NULL;
    lookup = semcheck_lookup_subprogram_predecl(
        symtab,
        subprogram,
        id_to_use_for_lookup,
        subprogram->tree_data.subprogram_data.mangled_id);

    if (lookup.tree_match != NULL)
    {
        semcheck_refresh_predecl_match(lookup.tree_match, subprogram);
        subprogram->tree_data.subprogram_data.cached_predecl_node = (struct HashNode *)lookup.tree_match;
        semcheck_restore_scope(symtab, saved_scope_for_unit);
        return 0;  /* Already declared - skip to avoid duplicates */
    }

    if (lookup.exact_match != NULL)
    {
        semcheck_refresh_predecl_match(lookup.exact_match, subprogram);
        subprogram->tree_data.subprogram_data.cached_predecl_node = (struct HashNode *)lookup.exact_match;
        semcheck_restore_scope(symtab, saved_scope_for_unit);
        return 0;  /* Already declared - skip to avoid duplicates */
    }

    if (lookup.body_pair_match != NULL)
    {
        semcheck_refresh_predecl_match(lookup.body_pair_match, subprogram);
        subprogram->tree_data.subprogram_data.cached_predecl_node = (struct HashNode *)lookup.body_pair_match;
        semcheck_restore_scope(symtab, saved_scope_for_unit);
        return 0;  /* Declaration/body pair already tracked */
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

        /* For nested type methods, also register the short alias during predeclaration. */
        if (func_return == 0)
            register_nested_type_short_alias(symtab,
                subprogram->tree_data.subprogram_data.owner_class_outer,
                subprogram->tree_data.subprogram_data.owner_class,
                subprogram->tree_data.subprogram_data.method_name,
                subprogram->tree_data.subprogram_data.mangled_id, proc_type, 0);

        /* Propagate flags and method identity to the hash node.
         * Search only the TARGET TABLE where the node was just pushed,
         * not the full scope tree, to avoid mistakenly finding and modifying
         * a same-named local overload in the program scope. */
        if (func_return == 0) {
            HashTable_t *target_table = SymTab_GetTargetTable(symtab);
            HashNode_t *node = FindIdentInTable(target_table, id_to_use_for_lookup);
            if (node != NULL) {
                if (subprogram->tree_data.subprogram_data.is_varargs)
                    node->is_varargs = 1;
                if (subprogram->tree_data.subprogram_data.defined_in_unit)
                {
                    node->defined_in_unit = 1;
                    if (subprogram->tree_data.subprogram_data.unit_is_public)
                        node->unit_is_public = 1;
                }
                if (subprogram->tree_data.subprogram_data.is_nested_scope)
                    node->is_nested_scope = 1;
                if (subprogram->tree_data.subprogram_data.internproc_id != NULL) {
                    if (node->internproc_id != NULL) free(node->internproc_id);
                    node->internproc_id = strdup(subprogram->tree_data.subprogram_data.internproc_id);
                }
                if (subprogram->tree_data.subprogram_data.internconst_id != NULL) {
                    if (node->internconst_id != NULL) free(node->internconst_id);
                    node->internconst_id = strdup(subprogram->tree_data.subprogram_data.internconst_id);
                }
                copy_method_identity_to_node(node, subprogram);
            }
        }
        if (func_return == 0)
        {
            SubprogramPredeclLookup pushed_lookup = semcheck_lookup_subprogram_predecl(
                symtab, subprogram, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            HashNode_t *cached = pushed_lookup.exact_match != NULL ?
                pushed_lookup.exact_match : pushed_lookup.first_mangled_match;
            subprogram->tree_data.subprogram_data.cached_predecl_node = (struct HashNode *)cached;
        }
        /* Release creator's reference - hash table retained its own */
        destroy_kgpc_type(proc_type);
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

        /* For nested type methods like "Outer.Inner__Method", also register
         * the short alias "Inner__Method" during predeclaration so that
         * generic method clones can find them. */
        if (func_return == 0)
            register_nested_type_short_alias(symtab,
                subprogram->tree_data.subprogram_data.owner_class_outer,
                subprogram->tree_data.subprogram_data.owner_class,
                subprogram->tree_data.subprogram_data.method_name,
                subprogram->tree_data.subprogram_data.mangled_id, func_type, 1);

        /* Propagate flags — search only the target table (see proc case above). */
        if (func_return == 0) {
            HashTable_t *target_table = SymTab_GetTargetTable(symtab);
            HashNode_t *node = FindIdentInTable(target_table, id_to_use_for_lookup);
            if (node != NULL) {
                if (subprogram->tree_data.subprogram_data.is_varargs)
                    node->is_varargs = 1;
                if (subprogram->tree_data.subprogram_data.defined_in_unit)
                {
                    node->defined_in_unit = 1;
                    if (subprogram->tree_data.subprogram_data.unit_is_public)
                        node->unit_is_public = 1;
                }
                if (subprogram->tree_data.subprogram_data.is_nested_scope)
                    node->is_nested_scope = 1;
                if (subprogram->tree_data.subprogram_data.internproc_id != NULL) {
                    if (node->internproc_id != NULL) free(node->internproc_id);
                    node->internproc_id = strdup(subprogram->tree_data.subprogram_data.internproc_id);
                }
                if (subprogram->tree_data.subprogram_data.internconst_id != NULL) {
                    if (node->internconst_id != NULL) free(node->internconst_id);
                    node->internconst_id = strdup(subprogram->tree_data.subprogram_data.internconst_id);
                }
                copy_method_identity_to_node(node, subprogram);
            }
        }
        if (func_return == 0)
        {
            SubprogramPredeclLookup pushed_lookup = semcheck_lookup_subprogram_predecl(
                symtab, subprogram, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            HashNode_t *cached = pushed_lookup.exact_match != NULL ?
                pushed_lookup.exact_match : pushed_lookup.first_mangled_match;
            subprogram->tree_data.subprogram_data.cached_predecl_node = (struct HashNode *)cached;
        }
        /* Release creator's reference - hash table retained its own */
        destroy_kgpc_type(func_type);
    }

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: predeclare_subprogram %s returning error: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    semcheck_restore_scope(symtab, saved_scope_for_unit);
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
    
    /* Pass 1: Pre-declare all procedures at this level.
     * NOTE: Do NOT skip subprograms that already have cached_predecl_node.
     * predeclare_subprograms() is called multiple times (once globally from
     * semcheck_program, then again per-scope from semcheck_subprograms).
     * Between calls, type aliases like PByte get resolved from integer to
     * pointer, changing the mangled name (e.g. _p_i_i → _p_p_i).  Skipping
     * the second predeclare leaves the HashNode with the stale mangled name,
     * so the VMT references an undefined symbol at link time. */
    cur = subprograms;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        Tree_t *child = (Tree_t *)cur->cur;
        return_val += predeclare_subprogram(symtab, child, max_scope_lev, parent_subprogram);
        cur = cur->next;
    }

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprograms error after Pass 1: %d\n", return_val);
#endif

    /* Pass 1.5: Resolve parameter types for all predeclared subprograms.
     * This must run AFTER type declarations are fully processed (semcheck_type_decls)
     * so that types like PAnsiChar, TypedFile, etc. are available.
     * predeclare_subprogram can't do this because it runs before semcheck_type_decls. */
    cur = subprograms;
    while (cur != NULL)
    {
        assert(cur->cur != NULL);
        Tree_t *child = (Tree_t *)cur->cur;
        ListNode_t *param_cur = child->tree_data.subprogram_data.args_var;
        while (param_cur != NULL)
        {
            Tree_t *param = (Tree_t *)param_cur->cur;
            if (param != NULL && param->type == TREE_VAR_DECL)
            {
                /* Honour the parser's {$H-} remap when the param's tree node
                 * type is SHORTSTRING_TYPE but the cached_kgpc_type is the
                 * generic STRING (AnsiString) builtin — replace with a real
                 * ShortString primitive so codegen treats it correctly.
                 * This catches both the predeclare path (cached set) and
                 * the resolve-now path (cached null). */
                if (param->tree_data.var_decl_data.type == SHORTSTRING_TYPE &&
                    param->tree_data.var_decl_data.cached_kgpc_type != NULL &&
                    kgpc_type_is_string(param->tree_data.var_decl_data.cached_kgpc_type) &&
                    !kgpc_type_is_shortstring(param->tree_data.var_decl_data.cached_kgpc_type))
                {
                    destroy_kgpc_type(param->tree_data.var_decl_data.cached_kgpc_type);
                    param->tree_data.var_decl_data.cached_kgpc_type =
                        create_primitive_type(SHORTSTRING_TYPE);
                }
                else if (param->tree_data.var_decl_data.cached_kgpc_type == NULL)
                {
                    int owns = 0;
                    KgpcType *resolved = resolve_type_from_vardecl(param, symtab, &owns);
                    if (resolved != NULL)
                    {
                        /* Honour the parser's {$H-} remap: when a parameter is
                         * declared with bare type "string" and the parser has
                         * already resolved it to SHORTSTRING_TYPE, install a
                         * ShortString KgpcType instead of the generic String
                         * builtin that resolve_type_from_vardecl would return.
                         * Without this, later codegen paths (e.g.
                         * codegen_current_param_is_ansistring) see AnsiString
                         * semantics for the param while the value passed at the
                         * call site is actually a ShortString buffer. */
                        if (param->tree_data.var_decl_data.type == SHORTSTRING_TYPE &&
                            kgpc_type_is_string(resolved) &&
                            !kgpc_type_is_shortstring(resolved))
                        {
                            if (owns)
                                destroy_kgpc_type(resolved);
                            resolved = create_primitive_type(SHORTSTRING_TYPE);
                            owns = 1;
                        }
                        if (!owns)
                            kgpc_type_retain(resolved);
                        param->tree_data.var_decl_data.cached_kgpc_type = resolved;
                    }
                }
            }
            param_cur = param_cur->next;
        }
        cur = cur->next;
    }

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
        if (child != NULL &&
            child->tree_data.subprogram_data.num_generic_type_params > 0)
        {
            /* Unspecialized generic subprogram — skip body semcheck.
             * Generic bodies are only checked after specialization, when
             * concrete type arguments replace the type parameters. */
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
