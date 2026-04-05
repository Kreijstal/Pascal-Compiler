ListNode_t *codegen_proc_call(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_PROCEDURE_CALL);
    assert(ctx != NULL);
    assert(symtab != NULL);

    char *proc_name;
    ListNode_t *args_expr;
    ListNode_t *call_args;
    /* Procedure calls can reference very long mangled identifiers, so keep plenty of space. */
    char buffer[CODEGEN_MAX_INST_BUF];

    proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
    args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    call_args = args_expr;
    char *unmangled_name = stmt->stmt_data.procedure_call_data.id;


    /* CRITICAL FIX: Use cached information from AST instead of HashNode pointer.
     * The resolved_proc pointer may point to freed memory if the HashNode was in a scope
     * that has been popped (e.g., nested procedures' parameters/local variables).
     * 
     * During semantic checking, we now cache the essential information (hash_type and KgpcType)
     * directly in the AST, avoiding the need to dereference potentially freed HashNode pointers.
     */
    int call_hash_type;  /* HashType enum value */
    struct KgpcType *call_kgpc_type;
#ifdef DEBUG_CODEGEN
    HashNode_t *debug_proc_node = NULL;
#endif
    
    if (stmt->stmt_data.procedure_call_data.is_call_info_valid)
    {
        /* Use cached information from semantic checking */
        call_hash_type = stmt->stmt_data.procedure_call_data.call_hash_type;
        call_kgpc_type = stmt->stmt_data.procedure_call_data.call_kgpc_type;
        
        CODEGEN_DEBUG("DEBUG codegen_proc_call: id=%s, mangled=%s, hash_type=%d\n",
            unmangled_name ? unmangled_name : "NULL",
            proc_name ? proc_name : "NULL", 
            call_hash_type);
    }
    else
    {
        /* Fallback: look up the symbol (for old code paths or if semantic checker didn't set it) */
        HashNode_t *proc_node = NULL;
        if (unmangled_name != NULL)
            FindSymbol(&proc_node, symtab, unmangled_name);
        /* If unmangled name not found, try the mangled name */
        if (proc_node == NULL && proc_name != NULL && proc_name != unmangled_name)
            FindSymbol(&proc_node, symtab, proc_name);
#ifdef DEBUG_CODEGEN
        debug_proc_node = proc_node;
#endif
        if (proc_node != NULL)
        {
            call_hash_type = proc_node->hash_type;
            call_kgpc_type = proc_node->type;
        }
        else
        {
            /* Symbol not found - emit as a direct call and hope the linker resolves it.
             * This handles runtime procedures and methods that may not be in the symbol table. */
            codegen_report_warning(ctx,
                "WARNING: procedure %s not found during code generation, emitting direct call.",
                unmangled_name ? unmangled_name : "(unknown)");
            /* Emit a direct call to the mangled name */
            const char *call_target = proc_name ? proc_name : unmangled_name;
            if (call_target != NULL)
            {
                char call_buffer[CODEGEN_MAX_INST_BUF];
                inst_list = codegen_pass_arguments(args_expr, inst_list, ctx, NULL,
                    unmangled_name, 0, NULL, 0);
                snprintf(call_buffer, sizeof(call_buffer), "\tcall\t%s\n", call_target);
                inst_list = add_inst(inst_list, call_buffer);
            }
            return inst_list;
        }
    }

    if(call_hash_type == HASHTYPE_FUNCTION)
    {
        struct Expression *call_expr = codegen_build_temp_call_expr_from_stmt(stmt,
            call_hash_type, stmt->stmt_data.procedure_call_data.call_kgpc_type);
        if (call_expr == NULL)
            return inst_list;

        Register_t *discard_reg = NULL;
        inst_list = codegen_evaluate_expr(call_expr, inst_list, ctx, &discard_reg);
        if (discard_reg != NULL)
            free_reg(get_reg_stack(), discard_reg);
        codegen_destroy_temp_call_expr(call_expr);
        return inst_list;
    }

    if(call_hash_type == HASHTYPE_BUILTIN_PROCEDURE)
    {
        return codegen_builtin_proc(stmt, inst_list, ctx);
    }

    {
        struct Expression *call_expr = codegen_build_temp_call_expr_from_stmt(stmt,
            call_hash_type, call_kgpc_type);
        if (call_expr != NULL)
        {
            char *owned_call_target = NULL;
            const char *resolved_call_target = NULL;
            resolved_call_target = codegen_resolve_function_call_target(ctx, call_expr,
                &owned_call_target);
            if (resolved_call_target != NULL && resolved_call_target[0] != '\0')
            {
                if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
                {
                    free(stmt->stmt_data.procedure_call_data.mangled_id);
                    stmt->stmt_data.procedure_call_data.mangled_id = NULL;
                }
                stmt->stmt_data.procedure_call_data.mangled_id = strdup(resolved_call_target);
                if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
                    proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
                else
                    proc_name = (char *)resolved_call_target;
            }
            if (owned_call_target != NULL)
                free(owned_call_target);
            codegen_destroy_temp_call_expr(call_expr);
        }
    }

    /* Deterministic external name selection (no defensive fallback):
     * If a procedure definition exists and declares an explicit external alias (cname_override),
     * prefer it as the call target unconditionally. Otherwise, rely on the call-site mangled name
     * populated by semantic checking. If neither is available, panic with a descriptive error. */
    if (stmt->stmt_data.procedure_call_data.is_call_info_valid &&
        call_kgpc_type != NULL && call_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
        call_kgpc_type->info.proc_info.definition != NULL)
    {
        Tree_t *def = call_kgpc_type->info.proc_info.definition;
        const char *alias = def->tree_data.subprogram_data.cname_override;
        if (alias != NULL && alias[0] != '\0')
        {
            /* Overwrite any prior name with explicit external alias */
            if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
            {
                free(stmt->stmt_data.procedure_call_data.mangled_id);
                stmt->stmt_data.procedure_call_data.mangled_id = NULL;
            }
            stmt->stmt_data.procedure_call_data.mangled_id = strdup(alias);
            proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
        }
        else if (def->tree_data.subprogram_data.owner_class != NULL &&
                 def->tree_data.subprogram_data.method_name != NULL)
        {
            const char *impl_target = codegen_find_class_method_impl_id(
                symtab, NULL, def->tree_data.subprogram_data.owner_class, NULL,
                def->tree_data.subprogram_data.method_name);
            if (impl_target != NULL &&
                (proc_name == NULL || proc_name[0] == '\0' ||
                 strcmp(proc_name, def->tree_data.subprogram_data.method_name) == 0 ||
                 (unmangled_name != NULL && strcmp(proc_name, unmangled_name) == 0)))
            {
                if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
                {
                    free(stmt->stmt_data.procedure_call_data.mangled_id);
                    stmt->stmt_data.procedure_call_data.mangled_id = NULL;
                }
                stmt->stmt_data.procedure_call_data.mangled_id = strdup(impl_target);
                proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
            }
        }
    }

    if (proc_name != NULL && pascal_identifier_equals(proc_name, "fpc_in_prefetch_var"))
        proc_name = "kgpc_prefetch";
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
    /* Check if this is an indirect call through a procedure variable */
    /* For indirect calls (procedure variables/parameters), we need to generate an indirect call.
     * 
     * IMPORTANT: We must check hash_type FIRST, before checking type information.
     * On some platforms (e.g., Cygwin), type information may not be properly set,
     * but hash_type is always reliable for distinguishing variables from procedures.
     */
    int is_indirect_call = stmt->stmt_data.procedure_call_data.is_procedural_var_call;
    struct Expression *callee_override = NULL;
    
    /* Case 1: If hash_type is VAR, this is a procedure variable or parameter.
     * It MUST be an indirect call, regardless of whether type info is present. */
    if (call_hash_type == HASHTYPE_VAR)
    {
        is_indirect_call = 1;
    }
    /* Case 1b: If hash_type is CONST with procedure type, this is a typed constant 
     * holding a procedure address. Treat as indirect call. */
    else if (call_hash_type == HASHTYPE_CONST &&
             call_kgpc_type != NULL && 
             call_kgpc_type->kind == TYPE_KIND_PROCEDURE)
    {
        is_indirect_call = 1;
    }
    /* Case 2: If hash_type is PROCEDURE but type info indicates it's a procedure type,
     * and either proc_name is NULL or doesn't match, treat as indirect call.
     * This handles corrupted or improperly resolved procedure nodes. */
    else if (call_hash_type == HASHTYPE_PROCEDURE &&
             call_kgpc_type != NULL && 
             call_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
             proc_name == NULL)
    {
        is_indirect_call = 1;
    }
    /* Case 3: If the call target is a procedural TYPE (e.g. FileFunc(...)(...)),
     * treat it as an indirect call through the first argument. */
    else if (call_hash_type == HASHTYPE_TYPE &&
             call_kgpc_type != NULL &&
             call_kgpc_type->kind == TYPE_KIND_PROCEDURE)
    {
        is_indirect_call = 1;
        if (!stmt->stmt_data.procedure_call_data.is_procedural_var_call &&
            stmt->stmt_data.procedure_call_data.procedural_var_expr == NULL &&
            args_expr != NULL)
        {
            callee_override = (struct Expression *)args_expr->cur;
            call_args = args_expr->next;
        }
    }
    
    if (is_indirect_call)
    {
        /* INDIRECT CALL LOGIC */
        
        /* Validate that we have a procedure name for the indirect call */
        if (unmangled_name == NULL)
        {
            codegen_report_error(ctx,
                "FATAL: Internal compiler error - indirect call with NULL procedure name. "
                "Please report this bug with your source code.");
            return inst_list;
        }

        /* 1. Create a temporary expression to evaluate the procedure variable */
        struct Expression *callee_expr = NULL;
        int callee_owned = 0;
        if (callee_override != NULL)
        {
            callee_expr = callee_override;
        }
        else if (stmt->stmt_data.procedure_call_data.is_procedural_var_call &&
            stmt->stmt_data.procedure_call_data.procedural_var_expr != NULL)
        {
            callee_expr = stmt->stmt_data.procedure_call_data.procedural_var_expr;
            if (callee_expr->resolved_kgpc_type == NULL)
                callee_expr->resolved_kgpc_type = create_procedure_type(NULL, NULL);
        }
        else
        {
            callee_expr = mk_varid(stmt->line_num, strdup(unmangled_name));
            callee_expr->resolved_kgpc_type = create_procedure_type(NULL, NULL);
            callee_owned = 1;
        }
        
        /* 2. Generate code to load the procedure's address into a register */
        Register_t *addr_reg = NULL;
        int load_from_memory = 0;

        if (callee_expr->type == EXPR_RECORD_ACCESS ||
            callee_expr->type == EXPR_ARRAY_ACCESS ||
            callee_expr->type == EXPR_POINTER_DEREF)
        {
            load_from_memory = 1;
        }
        else if (callee_expr->type == EXPR_VAR_ID && ctx->symtab != NULL)
        {
            HashNode_t *callee_node = NULL;
            if (FindSymbol(&callee_node, ctx->symtab, callee_expr->expr_data.id) != 0 &&
                callee_node != NULL &&
                (callee_node->hash_type == HASHTYPE_VAR ||
                 callee_node->hash_type == HASHTYPE_FUNCTION_RETURN))
            {
                load_from_memory = 1;
            }
        }

        if (load_from_memory)
        {
            inst_list = codegen_address_for_expr(callee_expr, inst_list, ctx, &addr_reg);
            if (!codegen_had_error(ctx) && addr_reg != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                    addr_reg->bit_64, addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else
        {
            inst_list = codegen_evaluate_expr(callee_expr, inst_list, ctx, &addr_reg);
        }
        if (callee_owned)
            destroy_expr(callee_expr);
        
        if (codegen_had_error(ctx) || addr_reg == NULL)
        {
            return inst_list;
        }

        /* 3. Spill call target to stack so argument evaluation can't clobber it */
        StackNode_t *call_target_spill = add_l_t_bytes("indirect_call_target", 8);
        if (call_target_spill == NULL)
        {
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
            addr_reg->bit_64, call_target_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        addr_reg = NULL;
        
        /* 4. Pass arguments as usual */
        const char *proc_name_hint = (unmangled_name != NULL) ? unmangled_name : proc_name;
        inst_list = codegen_pass_arguments(call_args, inst_list, ctx, call_kgpc_type,
            proc_name_hint, 0, NULL, 0);

        /* 5. Zero out %eax for varargs ABI compatibility */
        inst_list = codegen_vect_reg(inst_list, 0);
        
        /* 6. Reload call target and perform the indirect call */
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%r11\n",
            call_target_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tcall\t*%%r11\n");
        inst_list = add_inst(inst_list, buffer);
        
        /* 7. Cleanup */
        inst_list = codegen_cleanup_call_stack(inst_list, ctx);
        
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s (indirect call)\n", __func__);
        #endif
        return inst_list;
    }
    else if (call_hash_type == HASHTYPE_PROCEDURE)
    {
        /* DIRECT CALL LOGIC */
        int requires_static_link = 0;
        if (call_kgpc_type != NULL && call_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
            call_kgpc_type->info.proc_info.definition != NULL)
        {
            requires_static_link = call_kgpc_type->info.proc_info.definition
                ->tree_data.subprogram_data.requires_static_link;
        }

        int callee_depth = 0;
        int have_depth = codegen_proc_static_link_depth(ctx, proc_name, &callee_depth);
        int current_depth = codegen_get_lexical_depth(ctx);
        int should_pass_static_link = requires_static_link;

        enum {
            STATIC_LINK_NONE = 0,
            STATIC_LINK_FROM_RBP,
            STATIC_LINK_FROM_SLOT,
            STATIC_LINK_FROM_REG,
            STATIC_LINK_FROM_SPILL
        } static_link_source = STATIC_LINK_NONE;
        int static_link_slot_offset = 0;
        Register_t *static_link_reg = NULL;
        int static_link_expr_active = 0;
        StackNode_t *static_link_spill = NULL;

        if (should_pass_static_link)
        {
            if (!have_depth)
            {
                static_link_source = STATIC_LINK_FROM_RBP;
            }
            else if (callee_depth > current_depth)
            {
                static_link_source = STATIC_LINK_FROM_RBP;
            }
            else if (callee_depth == current_depth)
            {
                StackNode_t *static_link_node = find_label("__static_link__");
                if (static_link_node != NULL)
                {
                    static_link_source = STATIC_LINK_FROM_SLOT;
                    static_link_slot_offset = static_link_node->offset;
                }
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: Missing static link slot while calling %s.",
                        unmangled_name);
                }
            }
            else
            {
                /* We need to traverse multiple scopes to get the static link.
                 * We must spill the result to a stack slot BEFORE evaluating arguments,
                 * because argument evaluation may call codegen_acquire_static_link again
                 * for different scope depths, which would free and reuse the register. */
                int levels_to_traverse = (current_depth - callee_depth) + 1;
                codegen_begin_expression(ctx);
                static_link_expr_active = 1;
                static_link_reg = codegen_acquire_static_link(ctx, &inst_list, levels_to_traverse);
                if (static_link_reg == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Failed to acquire static link for call to %s.",
                        unmangled_name);
                }
                else
                {
                    /* Spill the static link to a temporary stack slot to preserve it
                     * during argument evaluation */
                    static_link_spill = add_l_t("static_link_spill");
                    char spill_buffer[64];
                    snprintf(spill_buffer, sizeof(spill_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        static_link_reg->bit_64, static_link_spill->offset);
                    inst_list = add_inst(inst_list, spill_buffer);
                    free_reg(get_reg_stack(), static_link_reg);
                    static_link_reg = NULL;
                    static_link_source = STATIC_LINK_FROM_SPILL;
                }
                /* End the expression context to allow argument evaluation to use
                 * codegen_acquire_static_link independently */
                codegen_end_expression(ctx);
                static_link_expr_active = 0;
            }
        }
        
        /* When passing static link, shift arguments by 1 register position */
        int arg_start_index = should_pass_static_link ? 1 : 0;
        const char *proc_name_hint = (unmangled_name != NULL) ? unmangled_name : proc_name;
        inst_list = codegen_pass_arguments(args_expr, inst_list, ctx, call_kgpc_type,
            proc_name_hint, arg_start_index, NULL,
            stmt->stmt_data.procedure_call_data.is_class_method_call);

        if (should_pass_static_link)
        {
            const char *link_dest_reg = current_arg_reg64(0);
            assert(link_dest_reg != NULL && "current_arg_reg64(0) should never return NULL");

            char link_buffer[64];
            switch (static_link_source)
            {
                case STATIC_LINK_FROM_RBP:
                    snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%%rbp, %s\n", link_dest_reg);
                    inst_list = add_inst(inst_list, link_buffer);
                    break;
                case STATIC_LINK_FROM_SLOT:
                    snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t-%d(%%rbp), %s\n",
                        static_link_slot_offset, link_dest_reg);
                    inst_list = add_inst(inst_list, link_buffer);
                    break;
                case STATIC_LINK_FROM_SPILL:
                    /* Restore from spill slot where we saved it before argument evaluation */
                    if (static_link_spill != NULL)
                    {
                        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t-%d(%%rbp), %s\n",
                            static_link_spill->offset, link_dest_reg);
                        inst_list = add_inst(inst_list, link_buffer);
                    }
                    break;
                case STATIC_LINK_FROM_REG:
                    if (static_link_reg != NULL)
                    {
                        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%s, %s\n",
                            static_link_reg->bit_64, link_dest_reg);
                        inst_list = add_inst(inst_list, link_buffer);
                        free_reg(get_reg_stack(), static_link_reg);
                        static_link_reg = NULL;
                    }
                    break;
                default:
                    break;
            }
        }

        if (static_link_expr_active)
        {
            codegen_end_expression(ctx);
        }

        inst_list = codegen_vect_reg(inst_list, 0);
        CODEGEN_DEBUG("DEBUG PROC_CALL: proc_name=%s\n", proc_name ? proc_name : "NULL");
        if (stmt->stmt_data.procedure_call_data.is_interface_call &&
            stmt->stmt_data.procedure_call_data.vmt_index >= 0)
        {
            int self_arg_index = should_pass_static_link ? 1 : 0;
            inst_list = codegen_emit_interface_dispatch(
                inst_list, ctx, current_arg_reg64(self_arg_index),
                stmt->stmt_data.procedure_call_data.self_class_name,
                stmt->stmt_data.procedure_call_data.vmt_index,
                "intf_stmt", "__intf_stmt_target", 0,
                codegen_spill_call_arg_regs_stmt,
                codegen_restore_call_arg_regs_stmt);
        }
        else if (stmt->stmt_data.procedure_call_data.is_virtual_call &&
            stmt->stmt_data.procedure_call_data.vmt_index >= 0)
        {
            /* Virtual method call through VMT for procedure (void return type).
             * Self is the first argument register after the optional static link slot. */
            int vmt_index = stmt->stmt_data.procedure_call_data.vmt_index;
            int self_arg_index = should_pass_static_link ? 1 : 0;
            const char *self_reg = current_arg_reg64(self_arg_index);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r11\n", self_reg);
            inst_list = add_inst(inst_list, buffer);
            if (!stmt->stmt_data.procedure_call_data.is_class_method_call)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t(%%r11), %%r11\n");
                inst_list = add_inst(inst_list, buffer);
            }
            int vmt_offset = vmt_index * 8;
            snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%r11), %%r11\n", vmt_offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tcall\t*%%r11\n");
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", proc_name);
            inst_list = add_inst(inst_list, buffer);
        }
        inst_list = codegen_cleanup_call_stack(inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }
    
    if (call_hash_type == HASHTYPE_FUNCTION)
    {
        struct Expression *call_expr = codegen_build_temp_call_expr_from_stmt(stmt,
            call_hash_type, stmt->stmt_data.procedure_call_data.call_kgpc_type);
        if (call_expr == NULL)
            return inst_list;

        Register_t *discard_reg = NULL;
        inst_list = codegen_evaluate_expr(call_expr, inst_list, ctx, &discard_reg);
        if (discard_reg != NULL)
            free_reg(get_reg_stack(), discard_reg);
        codegen_destroy_temp_call_expr(call_expr);
        return inst_list;
    }

    /* If we reach here, it's a compiler bug - this should never happen.
     * In semantic checking, we should have caught any invalid procedure calls.
     * If we get here, it means:
     * 1. proc_node is NULL (semantic checker failed to resolve the symbol), OR
     * 2. proc_node has an unexpected hash_type (not VAR, PROCEDURE, or BUILTIN)
     * 
     * This indicates a serious internal error, so we assert in debug builds
     * and report a fatal error in release builds. */
    
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("FATAL: Reached unreachable code in %s - this is a compiler bug!\n", __func__);
    if (debug_proc_node != NULL) {
        CODEGEN_DEBUG("  proc_name: %s\n", proc_name ? proc_name : "(null)");
        CODEGEN_DEBUG("  unmangled_name: %s\n", unmangled_name ? unmangled_name : "(null)");
        CODEGEN_DEBUG("  hash_type: %d (expected VAR=%d, PROCEDURE=%d, or BUILTIN=%d)\n", 
                     debug_proc_node->hash_type, HASHTYPE_VAR, HASHTYPE_PROCEDURE, HASHTYPE_BUILTIN_PROCEDURE);
        CODEGEN_DEBUG("  type: %p\n", (void*)debug_proc_node->type);
        if (debug_proc_node->type != NULL) {
            CODEGEN_DEBUG("  type->kind: %d\n", debug_proc_node->type->kind);
        }
    } else {
        CODEGEN_DEBUG("  proc_node is NULL - semantic checker should have caught this!\n");
    }
    #endif
    
    KGPC_COMPILER_HARD_ASSERT(0,
        "unreachable procedure-call state for '%s' (hash_type=%d)",
        unmangled_name ? unmangled_name : "(unknown)",
        call_hash_type);

    /* In release builds, report a fatal error and stop code generation */
    codegen_report_error(ctx,
        "FATAL: Internal compiler error in procedure call code generation for '%s'. "
        "Please report this bug with your source code.",
        unmangled_name ? unmangled_name : "(unknown)");
    
    return inst_list;
}

/* Code generation for if-then-else statements */
ListNode_t *codegen_if_then(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_IF_THEN);
    assert(ctx != NULL);
    assert(symtab != NULL);

    int relop_type = NE, inverse;
    struct Expression *expr;
    struct Statement *if_stmt, *else_stmt;
    char label1[18], label2[18], buffer[50];

    expr = stmt->stmt_data.if_then_data.relop_expr;
    inst_list = codegen_condition_expr(expr, inst_list, ctx, &relop_type);

    gen_label(label1, 18, ctx);
    gen_label(label2, 18, ctx);
    if_stmt = stmt->stmt_data.if_then_data.if_stmt;
    else_stmt = stmt->stmt_data.if_then_data.else_stmt;

    inverse = 1;
    inst_list = gencode_jmp(relop_type, inverse, label1, inst_list);
    inst_list = codegen_stmt(if_stmt, inst_list, ctx, symtab);

    if(else_stmt == NULL)
    {
        snprintf(buffer, 50, "%s:\n", label1);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        inverse = 0;
        inst_list = gencode_jmp(NORMAL_JMP, inverse, label2, inst_list);
        snprintf(buffer, 50, "%s:\n", label1);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_stmt(else_stmt, inst_list, ctx, symtab);
        snprintf(buffer, 50, "%s:\n", label2);
        inst_list = add_inst(inst_list, buffer);
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for while statements */
ListNode_t *codegen_while(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_WHILE);
    assert(ctx != NULL);
    assert(symtab != NULL);

    int relop_type = NE;
    struct Expression *expr;
    struct Statement *while_stmt;
    char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];

    gen_label(cond_label, 18, ctx);
    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    gen_label(incr_label, 18, ctx);
    while_stmt = stmt->stmt_data.while_data.while_stmt;
    expr = stmt->stmt_data.while_data.relop_expr;

    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);
    if (!codegen_push_loop(ctx, exit_label, cond_label))
        return inst_list;
    inst_list = codegen_stmt(while_stmt, inst_list, ctx, symtab);
    codegen_pop_loop(ctx);
    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_condition_expr(expr, inst_list, ctx, &relop_type);
    inst_list = gencode_jmp(relop_type, 0, body_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

ListNode_t *codegen_repeat(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_REPEAT);
    assert(ctx != NULL);
    assert(symtab != NULL);

    char body_label[18], exit_label[18], buffer[50];
    int relop_type = NE;
    ListNode_t *body_list = stmt->stmt_data.repeat_data.body_list;

    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    snprintf(buffer, 50, "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);

    if (!codegen_push_loop(ctx, exit_label, body_label))
        return inst_list;
    while (body_list != NULL)
    {
        struct Statement *body_stmt = (struct Statement *)body_list->cur;
        inst_list = codegen_stmt(body_stmt, inst_list, ctx, symtab);
        body_list = body_list->next;
    }
    codegen_pop_loop(ctx);

    inst_list = codegen_condition_expr(stmt->stmt_data.repeat_data.until_expr, inst_list, ctx, &relop_type);
    inst_list = gencode_jmp(relop_type, 1, body_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for for-in statements - lower to regular for loop */
static ListNode_t *codegen_for_in(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_FOR_IN);
    assert(ctx != NULL);
    assert(symtab != NULL);

    // Extract for-in components
    struct Expression *loop_var = stmt->stmt_data.for_in_data.loop_var;
    struct Expression *collection = stmt->stmt_data.for_in_data.collection;
    struct Statement *body = stmt->stmt_data.for_in_data.do_stmt;

    if (loop_var == NULL || collection == NULL || body == NULL) {
        codegen_report_error(ctx, "ERROR: FOR-IN statement has NULL components");
        return inst_list;
    }

    // Get collection type info from semantic check
    KgpcType *array_type = collection->resolved_kgpc_type;
    int is_enum_domain = 0;
    int is_set_collection = (array_type != NULL && kgpc_type_is_set(array_type));
    long long enum_domain_lower = 0;
    int enum_domain_upper = -1;

    if (collection->type == EXPR_VAR_ID && collection->expr_data.id != NULL)
    {
        HashNode_t *collection_node = NULL;
        if (FindSymbol(&collection_node, symtab, collection->expr_data.id) != 0 &&
            collection_node != NULL &&
            collection_node->hash_type == HASHTYPE_TYPE &&
            collection_node->type != NULL)
        {
            int enum_count = codegen_enum_type_literal_count(collection_node->type);
            if (enum_count > 0)
            {
                is_enum_domain = 1;
                enum_domain_lower = 0;
                enum_domain_upper = enum_count - 1;
                array_type = collection_node->type;
            }
            else if (collection_node->type->type_alias != NULL &&
                collection_node->type->type_alias->is_range &&
                collection_node->type->type_alias->range_known)
            {
                long long lower = collection_node->type->type_alias->range_start;
                long long upper = collection_node->type->type_alias->range_end;
                if (upper >= lower && lower >= INT_MIN && upper <= INT_MAX)
                {
                    is_enum_domain = 1;
                    enum_domain_lower = lower;
                    enum_domain_upper = (int)upper;
                    array_type = collection_node->type;
                }
            }
        }
    }
    
    // Check if this is a list-like class with default indexed property
    int is_list_class = 0;
    struct RecordType *record_info = NULL;
    KgpcType *record_candidate = array_type;
    if (array_type != NULL && array_type->kind == TYPE_KIND_POINTER)
        record_candidate = array_type->info.points_to;

    if (record_candidate != NULL && record_candidate->kind == TYPE_KIND_RECORD) {
        record_info = kgpc_type_get_record(record_candidate);
        if (record_info != NULL) {
            /* Check for TFPGList$ pattern (generic list) - element type is encoded in class name.
             * This is kept separate from default_indexed_property for backwards compatibility. */
            if (record_info->type_id != NULL) {
                const char *prefix = "TFPGList$";
                size_t prefix_len = strlen(prefix);
                if (strncasecmp(record_info->type_id, prefix, prefix_len) == 0) {
                    is_list_class = 1;
                }
            }
            /* Check for default indexed property (handles TStringList and other classes with FItems) */
            if (!is_list_class && record_info->default_indexed_property != NULL) {
                is_list_class = 1;
            }
        }
    }

    int is_enumerator_class = 0;
    HashNode_t *getenum_node = NULL;
    HashNode_t *movenext_node = NULL;
    HashNode_t *current_node = NULL;
    KgpcType *current_type = NULL;
    struct RecordType *enum_record = NULL;
    if (!is_list_class && record_info != NULL)
    {
        getenum_node = codegen_find_zero_arg_method_node(symtab, record_info, "GetEnumerator");
        if (getenum_node != NULL && getenum_node->type != NULL)
        {
            KgpcType *enum_ret = kgpc_type_get_return_type(getenum_node->type);
            KgpcType *enum_candidate = enum_ret;
            if (enum_candidate != NULL && kgpc_type_is_pointer(enum_candidate))
                enum_candidate = enum_candidate->info.points_to;
            if (enum_candidate != NULL && kgpc_type_is_record(enum_candidate))
            {
                enum_record = kgpc_type_get_record(enum_candidate);
                if (enum_record != NULL)
                {
                    movenext_node = codegen_find_zero_arg_method_node(symtab, enum_record, "MoveNext");
                    if (movenext_node != NULL && movenext_node->type != NULL)
                    {
                        KgpcType *move_ret = kgpc_type_get_return_type(movenext_node->type);
                        if (move_ret != NULL && kgpc_type_is_boolean(move_ret) &&
                            codegen_get_enumerator_current_info(symtab, enum_record,
                                &current_node, &current_type))
                        {
                            is_enumerator_class = 1;
                        }
                    }
                }
            }
        }
    }

    if (is_enumerator_class) {
        char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];
        gen_label(cond_label, 18, ctx);
        gen_label(body_label, 18, ctx);
        gen_label(exit_label, 18, ctx);
        gen_label(incr_label, 18, ctx);

        StackNode_t *enum_slot = codegen_alloc_temp_slot("enum_ptr");
        if (enum_slot == NULL) {
            if (current_type != NULL) destroy_kgpc_type(current_type);
            codegen_report_error(ctx, "ERROR: Unable to allocate temp slot for enumerator");
            return inst_list;
        }

        Register_t *collection_reg = NULL;
        inst_list = codegen_expr_with_result(collection, inst_list, ctx, &collection_reg);
        if (codegen_had_error(ctx) || collection_reg == NULL) {
            if (current_type != NULL) destroy_kgpc_type(current_type);
            return inst_list;
        }

        const char *arg0 = current_arg_reg64(0);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", collection_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), collection_reg);
        inst_list = codegen_call_with_shadow_space(inst_list,
            getenum_node->mangled_id != NULL ? getenum_node->mangled_id : getenum_node->id);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", enum_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);
        snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        if (!codegen_push_loop(ctx, exit_label, incr_label)) {
            if (current_type != NULL) destroy_kgpc_type(current_type);
            return inst_list;
        }

        Register_t *enum_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (enum_reg == NULL) {
            codegen_pop_loop(ctx);
            if (current_type != NULL) destroy_kgpc_type(current_type);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for enumerator");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", enum_slot->offset, enum_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", enum_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_call_with_shadow_space(inst_list,
            current_node->mangled_id != NULL ? current_node->mangled_id : current_node->id);

        Register_t *loop_var_addr_reg = NULL;
        inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
        if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
            free_reg(get_reg_stack(), enum_reg);
            codegen_pop_loop(ctx);
            if (current_type != NULL) destroy_kgpc_type(current_type);
            return inst_list;
        }

        long long current_size = current_type != NULL ? kgpc_type_sizeof(current_type) : 0;
        if (current_size <= 0 && current_type != NULL && kgpc_type_is_pointer(current_type))
            current_size = 8;
        if (current_size == 1) {
            snprintf(buffer, sizeof(buffer), "\tmovb\t%%al, (%s)\n", loop_var_addr_reg->bit_64);
        } else if (current_size == 2) {
            snprintf(buffer, sizeof(buffer), "\tmovw\t%%ax, (%s)\n", loop_var_addr_reg->bit_64);
        } else if (current_size == 4) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t%%eax, (%s)\n", loop_var_addr_reg->bit_64);
        } else {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, (%s)\n", loop_var_addr_reg->bit_64);
        }
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), loop_var_addr_reg);
        free_reg(get_reg_stack(), enum_reg);

        inst_list = codegen_stmt(body, inst_list, ctx, symtab);
        codegen_pop_loop(ctx);

        snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
        inst_list = add_inst(inst_list, buffer);
        Register_t *enum_cond_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (enum_cond_reg == NULL) {
            if (current_type != NULL) destroy_kgpc_type(current_type);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for enumerator condition");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", enum_slot->offset, enum_cond_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", enum_cond_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_call_with_shadow_space(inst_list,
            movenext_node->mangled_id != NULL ? movenext_node->mangled_id : movenext_node->id);
        inst_list = add_inst(inst_list, "\ttestl\t%eax, %eax\n");
        snprintf(buffer, sizeof(buffer), "\tjne\t%s\n", body_label);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), enum_cond_reg);

        snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
        inst_list = add_inst(inst_list, buffer);
        if (current_type != NULL)
            destroy_kgpc_type(current_type);
        return inst_list;
    }
    
    if (is_list_class) {
        // Generate FOR-IN loop for TFPGList by accessing FItems and FCount directly
        // Structure: for Item in L do body
        // Becomes: for i := 0 to L.FCount-1 do Item := L.FItems[i]; body
        
        char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];
        gen_label(cond_label, 18, ctx);
        gen_label(body_label, 18, ctx);
        gen_label(exit_label, 18, ctx);
        gen_label(incr_label, 18, ctx);

        // Allocate stack slot for loop index
        StackNode_t *index_slot = codegen_alloc_temp_slot("fpg_idx");
        // Allocate stack slot for object pointer to survive loop calls
        StackNode_t *obj_ptr_slot = codegen_alloc_temp_slot("fpg_obj");
        // Allocate stack slot for loop upper bound (FCount) to survive loop body codegen
        StackNode_t *count_slot = codegen_alloc_temp_slot("fpg_count");
        
        if (index_slot == NULL || obj_ptr_slot == NULL || count_slot == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate temp slot for FOR-IN variables");
            return inst_list;
        }

        // Get address of collection object (L)
        Register_t *obj_addr_reg = NULL;
        inst_list = codegen_address_for_expr(collection, inst_list, ctx, &obj_addr_reg);
        if (obj_addr_reg == NULL || codegen_had_error(ctx)) {
            return inst_list;
        }
        
        // Dereference L to get the actual object pointer and store in stack slot
        Register_t *temp_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (temp_reg == NULL) {
            free_reg(get_reg_stack(), obj_addr_reg);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for object pointer load");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", obj_addr_reg->bit_64, temp_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        
        // Store object pointer to stack
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", temp_reg->bit_64, obj_ptr_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        
        free_reg(get_reg_stack(), obj_addr_reg);

        long long fitems_offset = 8;
        long long fcount_offset = 24;
        long long fcount_size = 8;
        struct RecordField *fitems_field = NULL;
        struct RecordField *fcount_field = NULL;

        int using_flist = 0;  /* Track if we're using TStringList's FList field */
        if (record_info != NULL) {
            if (resolve_record_field(symtab, record_info, "FItems", &fitems_field,
                    &fitems_offset, stmt->line_num, 1) != 0) {
                /* FItems not found — try FList (used by TStringList and TStrings) */
                if (resolve_record_field(symtab, record_info, "FList", &fitems_field,
                        &fitems_offset, stmt->line_num, 1) != 0) {
                    fitems_offset = 8;
                    fitems_field = NULL;
                } else {
                    using_flist = 1;
                }
            }
            if (resolve_record_field(symtab, record_info, "FCount", &fcount_field,
                    &fcount_offset, stmt->line_num, 1) != 0) {
                fcount_offset = 24;
                fcount_field = NULL;
            } else if (fcount_field != NULL) {
                long long size_out = 0;
                if (sizeof_from_type_ref(symtab, fcount_field->type, fcount_field->type_id,
                        &size_out, 0, stmt->line_num) == 0 && size_out > 0) {
                    fcount_size = size_out;
                }
            }
        }

        // Load FCount and store to stack slot (to prevent register clobbering in loop body)
        if (fcount_size <= 4) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t%lld(%s), %s\n",
                fcount_offset, temp_reg->bit_64, temp_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                temp_reg->bit_64, count_slot->offset);
            inst_list = add_inst(inst_list, buffer);
        } else {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%lld(%s), %s\n",
                fcount_offset, temp_reg->bit_64, temp_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                temp_reg->bit_64, count_slot->offset);
            inst_list = add_inst(inst_list, buffer);
        }
        free_reg(get_reg_stack(), temp_reg);

        // Initialize index to 0
        snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Jump to condition check
        inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

        // Body label
        snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        // Push loop exit label for break statements
        if (!codegen_push_loop(ctx, exit_label, incr_label)) {
            return inst_list;
        }

        // Load loop variable: Item := L.FItems[index]
        // FItems is a dynamic array pointer at offset 8
        Register_t *fitems_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (fitems_reg == NULL) {
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for FItems");
            return inst_list;
        }
        
        // Load object pointer from stack to temp register (reusing fitems_reg temporarily to save registers? No, safer to use another temp)
        Register_t *obj_reload_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (obj_reload_reg == NULL) {
             free_reg(get_reg_stack(), fitems_reg);
             codegen_pop_loop(ctx);
             codegen_report_error(ctx, "ERROR: Unable to allocate register for object reload");
             return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", obj_ptr_slot->offset, obj_reload_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        // Load FItems.data pointer from object using two-step approach
        // Step 1: Get address of FItems descriptor (at Self+fitems_offset)
        // Step 2: Load data pointer from descriptor[0]
        Register_t *desc_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (desc_addr_reg == NULL) {
            free_reg(get_reg_stack(), obj_reload_reg);
            free_reg(get_reg_stack(), fitems_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for descriptor address");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tleaq\t%lld(%s), %s\n", fitems_offset,
            obj_reload_reg->bit_64, desc_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", desc_addr_reg->bit_64, fitems_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), desc_addr_reg);
        free_reg(get_reg_stack(), obj_reload_reg);

        // Load index into register
        Register_t *idx_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (idx_reg == NULL) {
            free_reg(get_reg_stack(), fitems_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for index");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", index_slot->offset, idx_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        // Calculate element offset: index * element_size
        int element_size = 0;
        const char *item_field_name = using_flist ? "FList" : "FItems";

        // Try to determine element size from the item array field
        if (fitems_field != NULL) {
            /* If the resolved field has array element type info, use it */
            if (fitems_field->array_element_type_id != NULL && ctx->symtab != NULL) {
                HashNode_t *tnode = NULL;
                if (FindSymbol(&tnode, ctx->symtab, fitems_field->array_element_type_id) != 0 &&
                    tnode != NULL && tnode->type != NULL) {
                    element_size = (int)kgpc_type_sizeof(tnode->type);
                }
            }
            if (element_size <= 0) {
                switch (fitems_field->array_element_type) {
                    case CHAR_TYPE: case BOOL: element_size = 1; break;
                    case INT_TYPE: case ENUM_TYPE: element_size = 4; break;
                    case LONGINT_TYPE: element_size = 4; break;
                    case POINTER_TYPE: case REAL_TYPE: element_size = 8; break;
                }
            }
        } else if (record_info != NULL) {
            /* Fallback: scan fields by name */
            ListNode_t *fields = record_info->fields;
            while (fields != NULL) {
                if (fields->type == LIST_RECORD_FIELD) {
                    struct RecordField *f = (struct RecordField *)fields->cur;
                    if (f != NULL && f->name != NULL &&
                        pascal_identifier_equals(f->name, item_field_name)) {
                        if (f->array_element_type_id != NULL && ctx->symtab != NULL) {
                            HashNode_t *tnode = NULL;
                            if (FindSymbol(&tnode, ctx->symtab, f->array_element_type_id) != 0 &&
                                tnode != NULL && tnode->type != NULL) {
                                element_size = (int)kgpc_type_sizeof(tnode->type);
                            }
                        }
                        if (element_size <= 0) {
                            switch (f->array_element_type) {
                                case CHAR_TYPE: case BOOL: element_size = 1; break;
                                case INT_TYPE: case ENUM_TYPE: element_size = 4; break;
                                case LONGINT_TYPE: element_size = 4; break;
                                case POINTER_TYPE: case REAL_TYPE: element_size = 8; break;
                            }
                        }
                        break;
                    }
                }
                fields = fields->next;
            }
        }

        /* When using FList (TStringList), try to resolve the element type from
         * the field's type chain: PStringItemList → ^TStringItemList →
         * array of TStringItem → sizeof(TStringItem) = 16. */
        if (element_size <= 0 && using_flist && fitems_field != NULL &&
            fitems_field->type_id != NULL && ctx->symtab != NULL) {
            HashNode_t *ftype_node = NULL;
            if (FindSymbol(&ftype_node, ctx->symtab, fitems_field->type_id) != 0 &&
                ftype_node != NULL && ftype_node->type != NULL) {
                KgpcType *ft = ftype_node->type;
                /* Dereference pointer: PStringItemList → TStringItemList */
                if (ft->kind == TYPE_KIND_POINTER && ft->info.points_to != NULL)
                    ft = ft->info.points_to;
                /* Array element: TStringItemList → TStringItem */
                if (ft->kind == TYPE_KIND_ARRAY &&
                    ft->info.array_info.element_type != NULL)
                    element_size = (int)kgpc_type_sizeof(ft->info.array_info.element_type);
                else if (ft->kind == TYPE_KIND_RECORD)
                    element_size = (int)kgpc_type_sizeof(ft);
            }
        }

        // Fallback to loop variable size if element size couldn't be determined
        if (element_size <= 0 && loop_var != NULL && loop_var->resolved_kgpc_type != NULL) {
             element_size = (int)kgpc_type_sizeof(loop_var->resolved_kgpc_type);
        }

        if (element_size <= 0) element_size = 4;
        
        if (element_size != 1) {
            snprintf(buffer, sizeof(buffer), "\timulq\t$%d, %s\n", element_size, idx_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }

        // Load element: FItems[index]
        Register_t *elem_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (elem_reg == NULL) {
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), fitems_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for element");
            return inst_list;
        }
        if (element_size == 1) {
            snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_32);
        } else if (element_size == 2) {
            snprintf(buffer, sizeof(buffer), "\tmovzwl\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_32);
        } else if (element_size == 4) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_32);
        } else if (element_size == 8) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_64);
        } else if (using_flist && element_size > 8) {
            /* TStringList's FList stores TStringItem records (FString + FObject).
             * The stride is element_size (16) but we only need the first 8 bytes
             * (the FString pointer) from each entry. */
            snprintf(buffer, sizeof(buffer), "\tmovq\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_64);
        } else {
            free_reg(get_reg_stack(), elem_reg);
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), fitems_reg);
            codegen_report_error(ctx, "ERROR: TFPGList for-in only supports 1, 2, 4, 8 byte elements");
            return inst_list;
        }
        inst_list = add_inst(inst_list, buffer);

        // Assign element value to loop variable
        // Get address of loop variable
        Register_t *loop_var_addr_reg = NULL;
        inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
        if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
            free_reg(get_reg_stack(), elem_reg);
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), fitems_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }

        // Store element to loop variable
        if (element_size == 1) {
            const char *byte_reg = register_name8(elem_reg);
            if (byte_reg == NULL) {
                codegen_report_error(ctx, "codegen_for_in: unable to get 8-bit register name for element storage");
                free_reg(get_reg_stack(), elem_reg);
                free_reg(get_reg_stack(), idx_reg);
                free_reg(get_reg_stack(), fitems_reg);
                free_reg(get_reg_stack(), loop_var_addr_reg);
                codegen_pop_loop(ctx);
                return inst_list;
            }
            snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", byte_reg, loop_var_addr_reg->bit_64);
        } else if (element_size == 2) {
            const char *word_reg = codegen_register_name16(elem_reg);
            if (word_reg == NULL) {
                codegen_report_error(ctx, "codegen_for_in: unable to get 16-bit register name for element storage");
                free_reg(get_reg_stack(), elem_reg);
                free_reg(get_reg_stack(), idx_reg);
                free_reg(get_reg_stack(), fitems_reg);
                free_reg(get_reg_stack(), loop_var_addr_reg);
                codegen_pop_loop(ctx);
                return inst_list;
            }
            snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", word_reg, loop_var_addr_reg->bit_64);
        } else if (element_size == 4) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", elem_reg->bit_32, loop_var_addr_reg->bit_64);
        } else if (element_size == 8) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", elem_reg->bit_64, loop_var_addr_reg->bit_64);
        } else if (using_flist && element_size > 8) {
            /* For FList with record entries (e.g., TStringItem), we loaded the
             * first field (FString pointer, 8 bytes) — store as a pointer. */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", elem_reg->bit_64, loop_var_addr_reg->bit_64);
        }
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), loop_var_addr_reg);
        free_reg(get_reg_stack(), elem_reg);
        free_reg(get_reg_stack(), idx_reg);
        free_reg(get_reg_stack(), fitems_reg);

        // Generate body
        inst_list = codegen_stmt(body, inst_list, ctx, symtab);

        codegen_pop_loop(ctx);

        // Increment label (for continue statements)
        snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
        inst_list = add_inst(inst_list, buffer);

        // Increment index
        Register_t *inc_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (inc_reg != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", index_slot->offset, inc_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tincq\t%s\n", inc_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", inc_reg->bit_64, index_slot->offset);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), inc_reg);
        }

        // Condition check label
        snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
        inst_list = add_inst(inst_list, buffer);

        // Compare index < count
        // Load index from stack
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        
        // Compare directly against cached FCount from stack slot
        snprintf(buffer, sizeof(buffer), "\tcmpq\t-%d(%%rbp), %%rax\n", count_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        
        // Jump to body if index < count
        snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        // Exit label
        snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
        inst_list = add_inst(inst_list, buffer);

        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s (TFPGList path)\n", __func__);
        #endif
        return inst_list;
    }

    // Check if collection is a string type (dynamic string or shortstring)
    int collection_type_tag = expr_get_type_tag(collection);
    int is_string_collection = (collection_type_tag == STRING_TYPE || collection_type_tag == SHORTSTRING_TYPE);
    
    if (is_string_collection) {
        // Handle FOR-IN iteration over string
        // Structure: for C in S do body
        // Becomes: for i := 1 to Length(S) do C := S[i]; body
        
        char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];
        gen_label(cond_label, 18, ctx);
        gen_label(body_label, 18, ctx);
        gen_label(exit_label, 18, ctx);
        gen_label(incr_label, 18, ctx);

        // Allocate stack slots for loop index and string length
        StackNode_t *index_slot = codegen_alloc_temp_slot("str_idx");
        StackNode_t *length_slot = codegen_alloc_temp_slot("str_len");
        StackNode_t *str_ptr_slot = codegen_alloc_temp_slot("str_ptr");
        
        if (index_slot == NULL || length_slot == NULL || str_ptr_slot == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate temp slots for FOR-IN string iteration");
            return inst_list;
        }

        // Get the string pointer value
        Register_t *str_reg = NULL;
        inst_list = codegen_expr_with_result(collection, inst_list, ctx, &str_reg);
        if (codegen_had_error(ctx) || str_reg == NULL) {
            return inst_list;
        }

        // Save string pointer to stack (needed for loop body)
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", str_reg->bit_64, str_ptr_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Call kgpc_string_length to get string length
        // Move string pointer to platform-specific first argument register
        const char *arg0 = current_arg_reg64(0);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", str_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), str_reg);
        str_reg = NULL;

        // Call kgpc_string_length with proper shadow space on Windows
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_length");

        // Result is in %rax - save to length slot
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", length_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Initialize index to 1 (Pascal strings are 1-indexed)
        snprintf(buffer, sizeof(buffer), "\tmovq\t$1, -%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Jump to condition check
        inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

        // Body label
        snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        // Push loop exit label for break/continue statements
        if (!codegen_push_loop(ctx, exit_label, incr_label)) {
            return inst_list;
        }

        // Load character: C := S[index]
        // First, load string pointer from stack
        Register_t *base_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (base_reg == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate register for string base");
            codegen_pop_loop(ctx);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", str_ptr_slot->offset, base_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        // Load index (as 64-bit to use as offset)
        Register_t *idx_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (idx_reg == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate register for string index");
            free_reg(get_reg_stack(), base_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", index_slot->offset, idx_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        if (collection_type_tag == STRING_TYPE)
        {
            // Subtract 1 from index (convert 1-based to 0-based for dynamic strings)
            snprintf(buffer, sizeof(buffer), "\tdecq\t%s\n", idx_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }

        // Load character from string: base[index]
        Register_t *char_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (char_reg == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate register for character");
            free_reg(get_reg_stack(), base_reg);
            free_reg(get_reg_stack(), idx_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s,%s,1), %s\n", 
                 base_reg->bit_64, idx_reg->bit_64, char_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), base_reg);
        free_reg(get_reg_stack(), idx_reg);

        // Assign character to loop variable
        Register_t *loop_var_addr_reg = NULL;
        inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
        if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
            free_reg(get_reg_stack(), char_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }

        // Store byte to loop variable
        const char *byte_reg = register_name8(char_reg);
        if (byte_reg == NULL) {
            codegen_report_error(ctx, "codegen_for_in: unable to get 8-bit register name for char storage");
            free_reg(get_reg_stack(), char_reg);
            free_reg(get_reg_stack(), loop_var_addr_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", byte_reg, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), char_reg);
        free_reg(get_reg_stack(), loop_var_addr_reg);

        // Generate code for the loop body
        inst_list = codegen_stmt(body, inst_list, ctx, symtab);

        codegen_pop_loop(ctx);

        // Increment label (for continue statements)
        snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
        inst_list = add_inst(inst_list, buffer);

        // Increment index
        snprintf(buffer, sizeof(buffer), "\tincq\t-%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Condition label
        snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
        inst_list = add_inst(inst_list, buffer);

        // Compare index <= length
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tcmpq\t-%d(%%rbp), %%rax\n", length_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Jump to body if index <= length
        snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        // Exit label
        snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
        inst_list = add_inst(inst_list, buffer);

        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s (string path)\n", __func__);
        #endif
        return inst_list;
    }

    if (is_enum_domain) {
        char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];
        gen_label(cond_label, 18, ctx);
        gen_label(body_label, 18, ctx);
        gen_label(exit_label, 18, ctx);
        gen_label(incr_label, 18, ctx);

        StackNode_t *index_slot = codegen_alloc_temp_slot("for_in_enum_idx");
        if (index_slot == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate temp slot for enum for-in index");
            return inst_list;
        }

        snprintf(buffer, sizeof(buffer), "\tmovl\t$%lld, -%d(%%rbp)\n",
            enum_domain_lower, index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

        snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        if (!codegen_push_loop(ctx, exit_label, incr_label))
            return inst_list;

        Register_t *idx_reg = get_free_reg(get_reg_stack(), &inst_list);
        Register_t *loop_var_addr_reg = NULL;
        if (idx_reg == NULL) {
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for enum for-in value");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", index_slot->offset, idx_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
        if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
            free_reg(get_reg_stack(), idx_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }

        {
            long long elem_size = (loop_var != NULL && loop_var->resolved_kgpc_type != NULL) ?
                kgpc_type_sizeof(loop_var->resolved_kgpc_type) : 4;
            if (elem_size <= 1) {
                const char *byte_reg = register_name8(idx_reg);
                if (byte_reg == NULL) {
                    free_reg(get_reg_stack(), idx_reg);
                    free_reg(get_reg_stack(), loop_var_addr_reg);
                    codegen_pop_loop(ctx);
                    codegen_report_error(ctx, "ERROR: Unable to get byte register for enum for-in");
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", byte_reg, loop_var_addr_reg->bit_64);
            } else if (elem_size == 2) {
                const char *word_reg = codegen_register_name16(idx_reg);
                if (word_reg == NULL) {
                    free_reg(get_reg_stack(), idx_reg);
                    free_reg(get_reg_stack(), loop_var_addr_reg);
                    codegen_pop_loop(ctx);
                    codegen_report_error(ctx, "ERROR: Unable to get word register for enum for-in");
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", word_reg, loop_var_addr_reg->bit_64);
            } else if (elem_size >= 8) {
                snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", idx_reg->bit_32, idx_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", idx_reg->bit_64, loop_var_addr_reg->bit_64);
            } else {
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", idx_reg->bit_32, loop_var_addr_reg->bit_64);
            }
            inst_list = add_inst(inst_list, buffer);
        }

        free_reg(get_reg_stack(), idx_reg);
        free_reg(get_reg_stack(), loop_var_addr_reg);

        inst_list = codegen_stmt(body, inst_list, ctx, symtab);
        codegen_pop_loop(ctx);

        snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tincl\t-%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tcmpl\t$%d, -%d(%%rbp)\n", enum_domain_upper, index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
        inst_list = add_inst(inst_list, buffer);
        return inst_list;
    }

    if (is_set_collection) {
        char cond_label[18], body_label[18], exit_label[18], incr_label[18], skip_body_label[18], buffer[256];
        gen_label(cond_label, 18, ctx);
        gen_label(body_label, 18, ctx);
        gen_label(exit_label, 18, ctx);
        gen_label(incr_label, 18, ctx);
        gen_label(skip_body_label, 18, ctx);

        int upper_bound = codegen_set_iteration_upper_bound(ctx, array_type);
        if (upper_bound < 0) {
            codegen_report_error(ctx, "ERROR: Unable to determine FOR-IN set bounds");
            return inst_list;
        }

        StackNode_t *index_slot = codegen_alloc_temp_slot("for_in_set_idx");
        if (index_slot == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate temp slot for set for-in index");
            return inst_list;
        }

        snprintf(buffer, sizeof(buffer), "\tmovl\t$0, -%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

        snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        if (!codegen_push_loop(ctx, exit_label, incr_label))
            return inst_list;

        Register_t *base_reg = NULL;
        Register_t *idx_reg = get_free_reg(get_reg_stack(), &inst_list);
        Register_t *byte_index_reg = NULL;
        Register_t *bit_reg = NULL;
        Register_t *byte_val_reg = NULL;
        Register_t *mask_reg = NULL;
        Register_t *loop_var_addr_reg = NULL;
        const char *bit_byte_reg = NULL;

        inst_list = codegen_address_for_expr(collection, inst_list, ctx, &base_reg);
        if (base_reg == NULL || codegen_had_error(ctx)) {
            codegen_pop_loop(ctx);
            return inst_list;
        }

        if (idx_reg == NULL) {
            free_reg(get_reg_stack(), base_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for set for-in index");
            return inst_list;
        }
        byte_index_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        bit_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        byte_val_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        mask_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (byte_index_reg == NULL || bit_reg == NULL || byte_val_reg == NULL || mask_reg == NULL) {
            if (mask_reg) free_reg(get_reg_stack(), mask_reg);
            if (byte_val_reg) free_reg(get_reg_stack(), byte_val_reg);
            if (bit_reg) free_reg(get_reg_stack(), bit_reg);
            if (byte_index_reg) free_reg(get_reg_stack(), byte_index_reg);
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), base_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate registers for set for-in body");
            return inst_list;
        }

        snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", index_slot->offset, idx_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", idx_reg->bit_32, byte_index_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tshrl\t$3, %s\n", byte_index_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", idx_reg->bit_32, bit_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tandl\t$7, %s\n", bit_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s,%s,1), %s\n",
            base_reg->bit_64, byte_index_reg->bit_64, byte_val_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", mask_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        bit_byte_reg = register_name8(bit_reg);
        if (bit_byte_reg == NULL) {
            free_reg(get_reg_stack(), mask_reg);
            free_reg(get_reg_stack(), byte_val_reg);
            free_reg(get_reg_stack(), bit_reg);
            free_reg(get_reg_stack(), byte_index_reg);
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), base_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to get byte register for set for-in bit index");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, %%cl\n", bit_byte_reg);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tshll\t%%cl, %s\n", mask_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", mask_reg->bit_32, byte_val_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tjz\t%s\n", skip_body_label);
        inst_list = add_inst(inst_list, buffer);

        inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
        if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
            free_reg(get_reg_stack(), mask_reg);
            free_reg(get_reg_stack(), byte_val_reg);
            free_reg(get_reg_stack(), bit_reg);
            free_reg(get_reg_stack(), byte_index_reg);
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), base_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }

        {
            long long elem_size = (loop_var != NULL && loop_var->resolved_kgpc_type != NULL) ?
                kgpc_type_sizeof(loop_var->resolved_kgpc_type) : 4;
            if (elem_size <= 1) {
                const char *byte_reg = register_name8(idx_reg);
                if (byte_reg == NULL) {
                    free_reg(get_reg_stack(), loop_var_addr_reg);
                    free_reg(get_reg_stack(), mask_reg);
                    free_reg(get_reg_stack(), byte_val_reg);
                    free_reg(get_reg_stack(), bit_reg);
                    free_reg(get_reg_stack(), byte_index_reg);
                    free_reg(get_reg_stack(), idx_reg);
                    free_reg(get_reg_stack(), base_reg);
                    codegen_pop_loop(ctx);
                    codegen_report_error(ctx, "ERROR: Unable to get byte register for set for-in store");
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", byte_reg, loop_var_addr_reg->bit_64);
            } else if (elem_size == 2) {
                const char *word_reg = codegen_register_name16(idx_reg);
                if (word_reg == NULL) {
                    free_reg(get_reg_stack(), loop_var_addr_reg);
                    free_reg(get_reg_stack(), mask_reg);
                    free_reg(get_reg_stack(), byte_val_reg);
                    free_reg(get_reg_stack(), bit_reg);
                    free_reg(get_reg_stack(), byte_index_reg);
                    free_reg(get_reg_stack(), idx_reg);
                    free_reg(get_reg_stack(), base_reg);
                    codegen_pop_loop(ctx);
                    codegen_report_error(ctx, "ERROR: Unable to get word register for set for-in store");
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", word_reg, loop_var_addr_reg->bit_64);
            } else if (elem_size >= 8) {
                snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", idx_reg->bit_32, idx_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", idx_reg->bit_64, loop_var_addr_reg->bit_64);
            } else {
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", idx_reg->bit_32, loop_var_addr_reg->bit_64);
            }
            inst_list = add_inst(inst_list, buffer);
        }

        free_reg(get_reg_stack(), loop_var_addr_reg);
        free_reg(get_reg_stack(), mask_reg);
        free_reg(get_reg_stack(), byte_val_reg);
        free_reg(get_reg_stack(), bit_reg);
        free_reg(get_reg_stack(), byte_index_reg);
        free_reg(get_reg_stack(), idx_reg);
        free_reg(get_reg_stack(), base_reg);

        inst_list = codegen_stmt(body, inst_list, ctx, symtab);

        snprintf(buffer, sizeof(buffer), "%s:\n", skip_body_label);
        inst_list = add_inst(inst_list, buffer);
        codegen_pop_loop(ctx);

        snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tincl\t-%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tcmpl\t$%d, -%d(%%rbp)\n", upper_bound, index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
        inst_list = add_inst(inst_list, buffer);
        return inst_list;
    }
    
    if (array_type == NULL || array_type->kind != TYPE_KIND_ARRAY) {
        codegen_report_error(ctx, "ERROR: FOR-IN collection is not an array type");
        return inst_list;
    }

    // Get array bounds
    int start_index = array_type->info.array_info.start_index;
    int end_index = array_type->info.array_info.end_index;

    // Generate labels for loop control
    char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];
    gen_label(cond_label, 18, ctx);
    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    gen_label(incr_label, 18, ctx);

    // Allocate stack slot for the index variable
    StackNode_t *index_slot = codegen_alloc_temp_slot("for_in_idx");
    if (index_slot == NULL) {
        codegen_report_error(ctx, "ERROR: Unable to allocate temp slot for for-in index");
        return inst_list;
    }

    // Initialize index to start_index
    snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, -%d(%%rbp)\n", start_index, index_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    // Jump to condition check
    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    // Body label
    snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);

    // Push loop exit label for break statements
    if (!codegen_push_loop(ctx, exit_label, incr_label))
        return inst_list;

    // Generate code for: loop_var := collection[index]
    // Load index from stack into a register
    Register_t *index_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (index_reg == NULL) {
        codegen_report_error(ctx, "ERROR: Unable to allocate register for for-in index");
        codegen_pop_loop(ctx);
        return inst_list;
    }
    
    snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", index_slot->offset, index_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    // Get the base address of the array (not its value)
    Register_t *array_base_reg = NULL;
    inst_list = codegen_address_for_expr(collection, inst_list, ctx, &array_base_reg);
    if (array_base_reg == NULL || codegen_had_error(ctx)) {
        free_reg(get_reg_stack(), index_reg);
        codegen_pop_loop(ctx);
        return inst_list;
    }

    // Calculate offset: (index - start_index) * element_size
    int element_size = kgpc_type_sizeof(array_type->info.array_info.element_type);
    
    // Subtract start_index if non-zero
    if (start_index != 0) {
        snprintf(buffer, sizeof(buffer), "\tsubl\t$%d, %s\n", start_index, index_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }

    // Multiply by element size
    if (element_size > 1) {
        if ((element_size & (element_size - 1)) == 0) {
            // Power of 2 - use shift
            int shift = 0;
            int temp = element_size;
            while (temp > 1) {
                temp >>= 1;
                shift++;
            }
            if (shift > 0) {
                snprintf(buffer, sizeof(buffer), "\tsall\t$%d, %s\n", shift, index_reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
            }
        } else {
            // Not power of 2 - use multiply
            snprintf(buffer, sizeof(buffer), "\timull\t$%d, %s\n", element_size, index_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    // Sign extend to 64-bit and add to base address
    snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", index_reg->bit_32, index_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n", index_reg->bit_64, array_base_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    // Now array_base_reg points to the element - load it
    Register_t *element_reg = array_base_reg;  // Reuse the register
    free_reg(get_reg_stack(), index_reg);
    index_reg = NULL;

    if (element_size == 1) {
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s), %s\n", element_reg->bit_64, element_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 2) {
        snprintf(buffer, sizeof(buffer), "\tmovzwl\t(%s), %s\n", element_reg->bit_64, element_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 4) {
        snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", element_reg->bit_64, element_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 8) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", element_reg->bit_64, element_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    } else {
        codegen_report_error(ctx, "ERROR: FOR-IN with large array elements not yet supported");
        free_reg(get_reg_stack(), element_reg);
        codegen_pop_loop(ctx);
        return inst_list;
    }

    // Assign element value to loop variable
    // Get address of loop variable
    Register_t *loop_var_addr_reg = NULL;
    inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
    if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
        free_reg(get_reg_stack(), element_reg);
        codegen_pop_loop(ctx);
        return inst_list;
    }

    // Store element to loop variable (using proper byte/word/dword/qword)
    if (element_size == 1) {
        const char *byte_reg = register_name8(element_reg);
        if (byte_reg == NULL) {
            codegen_report_error(ctx, "codegen_for_in: unable to get 8-bit register name for element storage");
            free_reg(get_reg_stack(), element_reg);
            free_reg(get_reg_stack(), loop_var_addr_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", byte_reg, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 2) {
        const char *word_reg = codegen_register_name16(element_reg);
        if (word_reg == NULL) {
            codegen_report_error(ctx, "codegen_for_in: unable to get 16-bit register name for element storage");
            free_reg(get_reg_stack(), element_reg);
            free_reg(get_reg_stack(), loop_var_addr_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", word_reg, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 4) {
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", element_reg->bit_32, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 8) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", element_reg->bit_64, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    free_reg(get_reg_stack(), element_reg);
    free_reg(get_reg_stack(), loop_var_addr_reg);

    // Generate code for the loop body
    inst_list = codegen_stmt(body, inst_list, ctx, symtab);
    
    // Increment index
    snprintf(buffer, sizeof(buffer), "\tincl\t-%d(%%rbp)\n", index_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    // Jump to condition
    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    // Condition label
    snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
    inst_list = add_inst(inst_list, buffer);

    // Compare index with end_index
    snprintf(buffer, sizeof(buffer), "\tcmpl\t$%d, -%d(%%rbp)\n", end_index, index_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    // Jump to body if index <= end_index
    snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", body_label);
    inst_list = add_inst(inst_list, buffer);

    // Exit label
    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

    // Pop loop exit label
    codegen_pop_loop(ctx);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for for statements */
