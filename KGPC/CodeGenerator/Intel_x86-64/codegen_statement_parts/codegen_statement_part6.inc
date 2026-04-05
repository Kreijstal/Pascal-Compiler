ListNode_t *codegen_for(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_FOR);
    assert(ctx != NULL);
    assert(symtab != NULL);

    struct Expression *expr = NULL, *for_var = NULL, *update_expr = NULL, *one_expr = NULL;
    struct Statement *for_body = NULL, *for_assign = NULL, *update_stmt = NULL;
    Register_t *limit_reg = NULL;
    Register_t *loop_value_reg = NULL;
    char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[128];
    StackNode_t *limit_temp = NULL;

    gen_label(cond_label, 18, ctx);
    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    gen_label(incr_label, 18, ctx);
    for_body = stmt->stmt_data.for_data.do_for;
    expr = stmt->stmt_data.for_data.to;

    if(stmt->stmt_data.for_data.for_assign_type == STMT_FOR_ASSIGN_VAR)
    {
        for_assign = stmt->stmt_data.for_data.for_assign_data.var_assign;
        inst_list = codegen_var_assignment(for_assign, inst_list, ctx);
        for_var = stmt->stmt_data.for_data.for_assign_data.var_assign->stmt_data.var_assign_data.var;
    }
    else
    {
        for_var = stmt->stmt_data.for_data.for_assign_data.var;
    }

    if (for_var == NULL)
        return inst_list;

    // Determine the direction of the loop
    const int is_downto = stmt->stmt_data.for_data.is_downto;
    
    // Create the update expression based on loop direction
    one_expr = mk_inum(-1, 1);
    if (is_downto) {
        update_expr = mk_addop(-1, MINUS, for_var, one_expr);
    } else {
        update_expr = mk_addop(-1, PLUS, for_var, one_expr);
    }
    // Propagate type information from the loop variable to the update expression
    // This ensures Int64 loop variables use 64-bit operations
    // Note: for_var is guaranteed non-NULL here (checked above at line 7336)
    if (update_expr != NULL)
    {
        if (for_var != NULL && for_var->resolved_kgpc_type != NULL)
        {
            kgpc_type_retain(for_var->resolved_kgpc_type);
            update_expr->resolved_kgpc_type = for_var->resolved_kgpc_type;
        }
        if (for_var->resolved_kgpc_type != NULL)
        {
            update_expr->resolved_kgpc_type = for_var->resolved_kgpc_type;
            kgpc_type_retain(for_var->resolved_kgpc_type);
        }
    }
    update_stmt = mk_varassign(-1, 0, for_var, update_expr);

    inst_list = codegen_evaluate_expr(expr, inst_list, ctx, &limit_reg);
    if (codegen_had_error(ctx) || limit_reg == NULL)
        goto cleanup;

    limit_temp = codegen_alloc_temp_slot("for_to_temp");

    const int limit_is_qword = expr_uses_qword_kgpctype(expr);
    const int limit_is_signed = codegen_expr_is_signed(expr);
    if (limit_is_qword)
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", limit_reg->bit_64, limit_temp->offset);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", limit_reg->bit_32, limit_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), limit_reg);
    limit_reg = NULL;

    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);
    if (!codegen_push_loop(ctx, exit_label, incr_label))
        goto cleanup;
    inst_list = codegen_stmt(for_body, inst_list, ctx, symtab);
    codegen_pop_loop(ctx);

    // Increment label (for continue statements)
    snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
    inst_list = add_inst(inst_list, buffer);

    // Overflow guard: if the loop variable already equals the limit,
    // skip the increment to prevent wrap-around for small types
    // (e.g., Byte 255 + 1 wraps to 0, causing an infinite loop).
    {
        Register_t *guard_reg = NULL;
        inst_list = codegen_evaluate_expr(for_var, inst_list, ctx, &guard_reg);
        if (!codegen_had_error(ctx) && guard_reg != NULL) {
            if (limit_is_qword)
                snprintf(buffer, sizeof(buffer), "\tcmpq\t-%d(%%rbp), %s\n",
                         limit_temp->offset, guard_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\tcmpl\t-%d(%%rbp), %s\n",
                         limit_temp->offset, guard_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), guard_reg);
            snprintf(buffer, sizeof(buffer), "\tje\t%s\n", exit_label);
            inst_list = add_inst(inst_list, buffer);
        } else if (guard_reg != NULL) {
            free_reg(get_reg_stack(), guard_reg);
        }
    }

    inst_list = codegen_stmt(update_stmt, inst_list, ctx, symtab);
    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_evaluate_expr(for_var, inst_list, ctx, &loop_value_reg);
    if (codegen_had_error(ctx) || loop_value_reg == NULL)
        goto cleanup;

    limit_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (limit_reg == NULL)
    {
        free_reg(get_reg_stack(), loop_value_reg);
        codegen_report_error(ctx, "ERROR: Unable to allocate register for for-loop bound.");
        goto cleanup;
    }

    if (limit_is_qword)
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", limit_temp->offset, limit_reg->bit_64);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", limit_temp->offset, limit_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    const int var_is_qword = expr_uses_qword_kgpctype(for_var);
    const int var_is_signed = codegen_expr_is_signed(for_var);
    const int compare_as_qword = var_is_qword || limit_is_qword;
    if (compare_as_qword && !limit_is_qword)
    {
        if (limit_is_signed)
            inst_list = codegen_sign_extend32_to64(inst_list, limit_reg->bit_32, limit_reg->bit_64);
        else
            inst_list = codegen_zero_extend32_to64(inst_list, limit_reg->bit_32, limit_reg->bit_32);
    }
    if (compare_as_qword && !var_is_qword)
    {
        if (var_is_signed)
            inst_list = codegen_sign_extend32_to64(inst_list, loop_value_reg->bit_32, loop_value_reg->bit_64);
        else
            inst_list = codegen_zero_extend32_to64(inst_list, loop_value_reg->bit_32, loop_value_reg->bit_32);
    }

    const int use_unsigned_compare = !(limit_is_signed && var_is_signed);

    const char *cmp_instr = compare_as_qword ? "cmpq" : "cmpl";
    const char *limit_cmp_reg = compare_as_qword ? limit_reg->bit_64 : limit_reg->bit_32;
    const char *loop_cmp_reg = compare_as_qword ? loop_value_reg->bit_64 : loop_value_reg->bit_32;
    snprintf(buffer, sizeof(buffer), "\t%s\t%s, %s\n", cmp_instr, limit_cmp_reg, loop_cmp_reg);
    inst_list = add_inst(inst_list, buffer);

    // Choose the correct branch instruction based on loop direction
    const char *branch_instr;
    if (is_downto) {
        branch_instr = use_unsigned_compare ? "jae" : "jge";  // jump if above or equal / greater or equal
    } else {
        branch_instr = use_unsigned_compare ? "jbe" : "jle";  // jump if below or equal / less or equal
    }
    snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", branch_instr, body_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), loop_value_reg);
    loop_value_reg = NULL;
    free_reg(get_reg_stack(), limit_reg);
    limit_reg = NULL;

    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

cleanup:
    if (limit_reg != NULL)
        free_reg(get_reg_stack(), limit_reg);
    if (loop_value_reg != NULL)
        free_reg(get_reg_stack(), loop_value_reg);
    free(one_expr);
    free(update_expr);
    free(update_stmt);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for case statements */
ListNode_t *codegen_case(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_CASE);
    assert(ctx != NULL);
    assert(symtab != NULL);

    char end_label[18], buffer[100];
    gen_label(end_label, 18, ctx);
    
    /* Evaluate the selector expression once and spill it to stack to free up registers.
     * This prevents register exhaustion when case branches contain complex expressions
     * or function calls that need registers for their own evaluation. */
    struct Expression *selector = stmt->stmt_data.case_data.selector_expr;
    Register_t *selector_reg = NULL;
    inst_list = codegen_expr_with_result(selector, inst_list, ctx, &selector_reg);
    if (selector_reg == NULL)
        return inst_list;

    int selector_is_string = codegen_expr_is_string_like(selector);
    int selector_is_qword = expr_uses_qword_kgpctype(selector);
    
    /* Spill selector value to stack to free the register */
    StackNode_t *selector_spill = (selector_is_string || selector_is_qword) ?
        add_l_t_bytes("case_selector", 8) : add_l_t("case_selector");
    if (selector_spill == NULL)
    {
        free_reg(get_reg_stack(), selector_reg);
        return inst_list;
    }
    
    if (selector_is_string || selector_is_qword)
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
            selector_reg->bit_64, selector_spill->offset);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n",
            selector_reg->bit_32, selector_spill->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), selector_reg);
    selector_reg = NULL;

    /* Generate code for each case branch */
    ListNode_t *branch_node = stmt->stmt_data.case_data.branches;
    if (branch_node == NULL) {
        /* No branches - selector was already evaluated and spilled */
        return inst_list;
    }


    while (branch_node != NULL) {
        struct CaseBranch *branch = (struct CaseBranch *)branch_node->cur;
        if (branch != NULL && branch->labels != NULL) {
            char branch_label[18], next_branch_label[18];
            gen_label(branch_label, 18, ctx);
            gen_label(next_branch_label, 18, ctx);
            
            /* Check each label in this branch */
            ListNode_t *label_node = branch->labels;
            while (label_node != NULL) {
                if (label_node->type == LIST_EXPR) {
                    struct Expression *label_expr = (struct Expression *)label_node->cur;

                    if (selector_is_string) {
                        int label_is_string = codegen_expr_is_string_like(label_expr);
                        int label_needs_char_promo = (label_expr != NULL &&
                            (label_expr->type == EXPR_CHAR_CODE ||
                             (label_expr->type == EXPR_STRING && expr_get_type_tag(label_expr) == CHAR_TYPE)));
                        if (!label_is_string && !label_needs_char_promo) {
                            label_node = label_node->next;
                            continue;
                        }

                        Register_t *label_reg = NULL;
                        inst_list = codegen_expr_with_result(label_expr, inst_list, ctx, &label_reg);
                        if (label_reg != NULL) {
                            if (label_needs_char_promo) {
                                inst_list = codegen_promote_char_reg_to_string(inst_list, label_reg);
                            }

                            const char *arg0 = current_arg_reg64(0);
                            const char *arg1 = current_arg_reg64(1);
                            if (arg0 != NULL && arg1 != NULL) {
                                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                                    selector_spill->offset, arg0);
                                inst_list = add_inst(inst_list, buffer);
                                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                                    label_reg->bit_64, arg1);
                                inst_list = add_inst(inst_list, buffer);
                                inst_list = codegen_vect_reg(inst_list, 0);
                                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_compare");
                                snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", RETURN_REG_32);
                                inst_list = add_inst(inst_list, buffer);
                                snprintf(buffer, sizeof(buffer), "\tje\t%s\n", branch_label);
                                inst_list = add_inst(inst_list, buffer);
                                free_arg_regs();
                            }
                            free_reg(get_reg_stack(), label_reg);
                        }
                    } else if (label_expr->type == EXPR_INUM) {
                        /* For constant labels, compare directly against the spilled value */
                        inst_list = codegen_emit_cmp_spill_immediate(inst_list, ctx,
                            selector_is_qword, label_expr->expr_data.i_num,
                            selector_spill->offset);
                        snprintf(buffer, sizeof(buffer), "\tje\t%s\n", branch_label);
                        inst_list = add_inst(inst_list, buffer);
                    } else if (!selector_is_string) {
                        /* For non-constant labels, evaluate label and compare with spilled selector */
                        Register_t *label_reg = NULL;
                        inst_list = codegen_expr_with_result(label_expr, inst_list, ctx, &label_reg);
                        if (label_reg != NULL) {
                            /* Compare selector value from stack with label */
                            if (selector_is_qword)
                                snprintf(buffer, sizeof(buffer), "\tcmpq\t%s, -%d(%%rbp)\n",
                                         label_reg->bit_64, selector_spill->offset);
                            else
                                snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, -%d(%%rbp)\n",
                                         label_reg->bit_32, selector_spill->offset);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tje\t%s\n", branch_label);
                            inst_list = add_inst(inst_list, buffer);
                            free_reg(get_reg_stack(), label_reg);
                        }
                    }
                } else if (!selector_is_string && label_node->type == LIST_SET_ELEMENT) {
                    struct SetElement *range = (struct SetElement *)label_node->cur;
                    if (range != NULL) {
                        char range_skip_label[18];
                        gen_label(range_skip_label, 18, ctx);

                        int emitted_lower_cmp = 0;
                        if (range->lower != NULL) {
                            if (range->lower->type == EXPR_INUM) {
                                /* Compare constant lower bound against spilled selector */
                                inst_list = codegen_emit_cmp_spill_immediate(inst_list, ctx,
                                    selector_is_qword, range->lower->expr_data.i_num,
                                    selector_spill->offset);
                                emitted_lower_cmp = 1;
                            } else {
                                Register_t *lower_reg = NULL;
                                inst_list = codegen_expr_with_result(range->lower, inst_list, ctx, &lower_reg);
                                if (lower_reg != NULL) {
                                    /* Compare spilled selector against lower bound */
                                    if (selector_is_qword)
                                        snprintf(buffer, sizeof(buffer), "\tcmpq\t%s, -%d(%%rbp)\n",
                                                 lower_reg->bit_64, selector_spill->offset);
                                    else
                                        snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, -%d(%%rbp)\n",
                                                 lower_reg->bit_32, selector_spill->offset);
                                    inst_list = add_inst(inst_list, buffer);
                                    emitted_lower_cmp = 1;
                                    free_reg(get_reg_stack(), lower_reg);
                                }
                            }

                            if (emitted_lower_cmp) {
                                snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", range_skip_label);
                                inst_list = add_inst(inst_list, buffer);
                            }
                        }

                        int emitted_upper_cmp = 0;
                        if (range->upper != NULL) {
                            if (range->upper->type == EXPR_INUM) {
                                /* Compare constant upper bound against spilled selector */
                                inst_list = codegen_emit_cmp_spill_immediate(inst_list, ctx,
                                    selector_is_qword, range->upper->expr_data.i_num,
                                    selector_spill->offset);
                                emitted_upper_cmp = 1;
                            } else {
                                Register_t *upper_reg = NULL;
                                inst_list = codegen_expr_with_result(range->upper, inst_list, ctx, &upper_reg);
                                if (upper_reg != NULL) {
                                    /* Compare spilled selector against upper bound */
                                    if (selector_is_qword)
                                        snprintf(buffer, sizeof(buffer), "\tcmpq\t%s, -%d(%%rbp)\n",
                                                 upper_reg->bit_64, selector_spill->offset);
                                    else
                                        snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, -%d(%%rbp)\n",
                                                 upper_reg->bit_32, selector_spill->offset);
                                    inst_list = add_inst(inst_list, buffer);
                                    emitted_upper_cmp = 1;
                                    free_reg(get_reg_stack(), upper_reg);
                                }
                            }

                            if (emitted_upper_cmp) {
                                snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", branch_label);
                                inst_list = add_inst(inst_list, buffer);
                            }
                        }

                        snprintf(buffer, sizeof(buffer), "%s:\n", range_skip_label);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }

                label_node = label_node->next;
            }
            
            /* If no match, jump to next branch */
            snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", next_branch_label);
            inst_list = add_inst(inst_list, buffer);
            
            /* Branch matched - execute statement */
            snprintf(buffer, sizeof(buffer), "%s:\n", branch_label);
            inst_list = add_inst(inst_list, buffer);
            if (branch->stmt != NULL)
                inst_list = codegen_stmt(branch->stmt, inst_list, ctx, symtab);
            snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", end_label);
            inst_list = add_inst(inst_list, buffer);
            
            /* Next branch label */
            snprintf(buffer, sizeof(buffer), "%s:\n", next_branch_label);
            inst_list = add_inst(inst_list, buffer);
        }
        branch_node = branch_node->next;
    }
    
    /* Else clause or fall-through */
    if (stmt->stmt_data.case_data.else_stmt != NULL) {
        inst_list = codegen_stmt(stmt->stmt_data.case_data.else_stmt, inst_list, ctx, symtab);
    }
    
    /* End label */
    snprintf(buffer, sizeof(buffer), "%s:\n", end_label);
    inst_list = add_inst(inst_list, buffer);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    /* selector_reg was already freed after spilling to stack */
    return inst_list;
}

static ListNode_t *codegen_break_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    const char *exit_label = codegen_current_loop_exit(ctx);
    if (exit_label == NULL)
    {
        codegen_report_error(ctx, "ERROR: BREAK statement outside of a loop at line %d.\n",
            stmt != NULL ? stmt->line_num : -1);
        return inst_list;
    }

    int limit_depth = codegen_current_loop_finally_depth(ctx);
    return codegen_branch_through_finally(ctx, inst_list, symtab, exit_label, limit_depth);
}

static ListNode_t *codegen_continue_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    const char *continue_label = codegen_current_loop_continue(ctx);
    if (continue_label == NULL)
    {
        codegen_report_error(ctx, "ERROR: CONTINUE statement outside of a loop at line %d.\n",
            stmt != NULL ? stmt->line_num : -1);
        return inst_list;
    }

    int limit_depth = codegen_current_loop_finally_depth(ctx);
    return codegen_branch_through_finally(ctx, inst_list, symtab, continue_label, limit_depth);
}



static ListNode_t *codegen_with(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;
    int pushed = 0;
    if (stmt->stmt_data.with_data.context_expr != NULL)
    {
        struct Expression *context_expr = stmt->stmt_data.with_data.context_expr;
        struct RecordType *record_type = codegen_resolve_with_record_type(context_expr, symtab);
        if (record_type != NULL)
            pushed = codegen_with_push(ctx, context_expr, record_type);
        int is_record_context = expr_has_type_tag(context_expr, RECORD_TYPE) ||
            context_expr->record_type != NULL;
        if (is_record_context && codegen_expr_is_addressable(context_expr))
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(context_expr, inst_list, ctx, &addr_reg);
            if (addr_reg != NULL)
                free_reg(get_reg_stack(), addr_reg);
        }
        else
        {
            inst_list = codegen_expr(context_expr, inst_list, ctx);
        }
    }
    if (stmt->stmt_data.with_data.body_stmt != NULL)
        inst_list = codegen_stmt(stmt->stmt_data.with_data.body_stmt, inst_list, ctx, symtab);
    if (pushed)
        codegen_with_pop(ctx);
    return inst_list;
}

static ListNode_t *codegen_try_finally(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;
    ListNode_t *try_stmts = stmt->stmt_data.try_finally_data.try_statements;
    ListNode_t *finally_stmts = stmt->stmt_data.try_finally_data.finally_statements;

    if (finally_stmts == NULL)
        return codegen_statement_list(try_stmts, inst_list, ctx, symtab);

    if (!codegen_push_finally(ctx, finally_stmts))
        return inst_list;

    inst_list = codegen_statement_list(try_stmts, inst_list, ctx, symtab);

    char final_entry[18];
    char after_label[18];
    gen_label(final_entry, sizeof(final_entry), ctx);
    gen_label(after_label, sizeof(after_label), ctx);

    inst_list = gencode_jmp(NORMAL_JMP, 0, final_entry, inst_list);
    inst_list = codegen_emit_finally_block(ctx, inst_list, symtab, final_entry, after_label);
    codegen_pop_finally(ctx);

    char buffer[32];
    snprintf(buffer, sizeof(buffer), "%s:\n", after_label);
    inst_list = add_inst(inst_list, buffer);

    return inst_list;
}

static ListNode_t *codegen_try_except(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;
    ListNode_t *try_stmts = stmt->stmt_data.try_except_data.try_statements;
    ListNode_t *except_stmts = stmt->stmt_data.try_except_data.except_statements;

    char except_label[18];
    char after_label[18];
    gen_label(except_label, sizeof(except_label), ctx);
    gen_label(after_label, sizeof(after_label), ctx);

    /* ── push a runtime except frame (setjmp) ── */
    inst_list = add_inst(inst_list, "\t# TRY/EXCEPT: push runtime except frame\n");
    /* call kgpc_push_except_frame() → returns jmp_buf* in %rax */
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_push_except_frame");
    free_arg_regs();

    /* Spill the jmp_buf pointer to a stack slot so setjmp can use it */
    StackNode_t *jmpbuf_slot = add_l_t("try_except_jmpbuf");
    char buffer[96];
    if (jmpbuf_slot != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", jmpbuf_slot->offset);
        inst_list = add_inst(inst_list, buffer);
    }

    /* call setjmp(jmp_buf*) — argument is in %rax, move to first arg reg */
    if (codegen_target_is_windows())
    {
        /* Windows x64: _setjmp(jmp_buf*, frame_addr) — needs two args.
         * %rcx = jmp_buf pointer, %rdx = frame pointer (RBP) for SEH unwinding. */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %%rcx\n\tmovq\t%%rbp, %%rdx\n");
    }
    else
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %%rdi\n");
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_vect_reg(inst_list, 0);

    /* Windows x64: _setjmp requires frame pointer as 2nd arg for SEH unwinding.
     * Linux: _setjmp avoids saving signal mask for performance. */
    inst_list = codegen_call_with_shadow_space(inst_list, "_setjmp");
    free_arg_regs();

    /* If setjmp returned non-zero, we got here from longjmp → jump to except */
    inst_list = add_inst(inst_list, "\ttestl\t%eax, %eax\n");
    snprintf(buffer, sizeof(buffer), "\tjne\t%s\n", except_label);
    inst_list = add_inst(inst_list, buffer);

    /* ── try body (setjmp returned 0) ── */
    if (!codegen_push_except(ctx, except_label)) {
        return inst_list;
    }
    inst_list = codegen_statement_list(try_stmts, inst_list, ctx, symtab);
    codegen_pop_except(ctx);

    /* Pop the runtime except frame (normal exit from try) */
    inst_list = add_inst(inst_list, "\t# TRY/EXCEPT: pop runtime except frame (normal exit)\n");
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_pop_except_frame");
    free_arg_regs();
    inst_list = gencode_jmp(NORMAL_JMP, 0, after_label, inst_list);

    /* ── except handler (setjmp returned non-zero, or local raise jumped here) ── */
    snprintf(buffer, sizeof(buffer), "%s:\n", except_label);
    inst_list = add_inst(inst_list, buffer);

    /* Set the after-label so codegen_on_exception handlers can jump to it
       after executing their body (only one 'on' clause should fire). */
    const char *saved_on_except_after = ctx->on_except_after_label;
    ctx->on_except_after_label = after_label;

    if (except_stmts != NULL)
        inst_list = codegen_statement_list(except_stmts, inst_list, ctx, symtab);
    else
        inst_list = add_inst(inst_list, "\t# EXCEPT block with no handlers\n");

    ctx->on_except_after_label = saved_on_except_after;

    snprintf(buffer, sizeof(buffer), "%s:\n", after_label);
    inst_list = add_inst(inst_list, buffer);

    return inst_list;
}

static ListNode_t *codegen_on_exception(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    StackNode_t *exception_var_node = NULL;
    char buffer[CODEGEN_MAX_INST_BUF];
    char *var_name = stmt->stmt_data.on_exception_data.exception_var_name;
    char *type_name = stmt->stmt_data.on_exception_data.exception_type_name;

    /* Determine the effective exception type for dispatch.
     * Pascal has two forms:
     *   on E: ExceptionType do ...   → var_name="E", type_name="ExceptionType"
     *   on ExceptionType do ...      → var_name="ExceptionType", type_name=NULL
     * In the second form, var_name is actually the type name. Detect this by
     * checking if var_name resolves to a class/record type in the symbol table. */
    const char *effective_type = type_name;
    int var_is_actually_type = 0;
    if (effective_type == NULL && var_name != NULL && symtab != NULL)
    {
        HashNode_t *sym = NULL;
        if (FindSymbol(&sym, symtab, var_name) != 0 && sym != NULL &&
            sym->hash_type == HASHTYPE_TYPE)
        {
            effective_type = var_name;
            var_is_actually_type = 1;
        }
    }

    /* ── Type-based dispatch: check if current exception matches this handler ── */
    char skip_label[18];
    int has_type_check = 0;
    if (effective_type != NULL)
    {
        gen_label(skip_label, sizeof(skip_label), ctx);
        has_type_check = 1;

        inst_list = add_inst(inst_list, "\t# ON exception type check\n");
        /* Load current exception object pointer */
        inst_list = add_inst(inst_list, "\tmovq\tkgpc_current_exception(%rip), %rax\n");
        /* If exception is nil, skip this handler */
        inst_list = add_inst(inst_list, "\ttestq\t%rax, %rax\n");
        snprintf(buffer, sizeof(buffer), "\tje\t%s\n", skip_label);
        inst_list = add_inst(inst_list, buffer);
        /* Load typeinfo from exception instance: VMT pointer → typeinfo slot */
        inst_list = add_inst(inst_list, "\tmovq\t(%rax), %rax\n");       /* VMT pointer */
        inst_list = add_inst(inst_list, "\tmovq\t56(%rax), %rax\n");     /* vTypeInfo slot */

        /* Call kgpc_rtti_is(exception_typeinfo, handler_typeinfo) */
        if (codegen_target_is_windows())
        {
            inst_list = add_inst(inst_list, "\tmovq\t%rax, %rcx\n");
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s_TYPEINFO(%%rip), %%rdx\n", effective_type);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            inst_list = add_inst(inst_list, "\tmovq\t%rax, %rdi\n");
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s_TYPEINFO(%%rip), %%rsi\n", effective_type);
            inst_list = add_inst(inst_list, buffer);
        }
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_rtti_is");
        free_arg_regs();
        /* If kgpc_rtti_is returned 0, skip this handler */
        inst_list = add_inst(inst_list, "\ttestl\t%eax, %eax\n");
        snprintf(buffer, sizeof(buffer), "\tje\t%s\n", skip_label);
        inst_list = add_inst(inst_list, buffer);
    }

    /* Only create variable binding for the 'on E: Type do' form
     * (not the bare 'on Type do' form where var_name is the type). */
    if (var_name != NULL && !var_is_actually_type) {
        EnterScope(symtab, 0);
        exception_var_node = add_l_x(var_name, 8);

        if (exception_var_node != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovq\tkgpc_current_exception(%%rip), %%rax\n");
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", exception_var_node->offset);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    if (stmt->stmt_data.on_exception_data.handler_stmt != NULL)
        inst_list = codegen_stmt(stmt->stmt_data.on_exception_data.handler_stmt, inst_list, ctx, symtab);

    if (var_name != NULL && !var_is_actually_type) {
        remove_last_l_x(var_name);
        LeaveScope(symtab);
    }

    /* After executing the handler body, jump past the remaining handlers */
    if (has_type_check && ctx->on_except_after_label != NULL)
    {
        snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", ctx->on_except_after_label);
        inst_list = add_inst(inst_list, buffer);
    }

    /* Skip label for when exception type doesn't match this handler */
    if (has_type_check)
    {
        snprintf(buffer, sizeof(buffer), "%s:\n", skip_label);
        inst_list = add_inst(inst_list, buffer);
    }

    return inst_list;
}

static ListNode_t *codegen_raise(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;

    inst_list = add_inst(inst_list, "\t# RAISE statement\n");

    struct Expression *exc_expr = stmt->stmt_data.raise_data.exception_expr;
    const char *except_label = codegen_current_except_label(ctx);

    Register_t *value_reg = NULL;
    int stored_exception = 0;

    if (exc_expr != NULL)
    {
        inst_list = codegen_expr_with_result(exc_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
            return inst_list;
        inst_list = codegen_store_exception_value(inst_list, ctx, exc_expr, value_reg);
        stored_exception = 1;
        free_reg(get_reg_stack(), value_reg);
        value_reg = NULL;
    }

    if (except_label != NULL)
    {
        /* Pop the runtime except frame before local jump to handler */
        inst_list = add_inst(inst_list, "\t# RAISE: pop runtime except frame (local raise)\n");
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_pop_except_frame");
        free_arg_regs();

        int limit_depth = codegen_current_except_finally_depth(ctx);
        inst_list = codegen_branch_through_finally(ctx, inst_list, symtab, except_label, limit_depth);
        return inst_list;
    }

    if (!stored_exception)
        inst_list = add_inst(inst_list, "\tmovq\t$0, kgpc_current_exception(%rip)\n");

    char after_label[18];
    int need_after_label = codegen_has_finally(ctx);
    if (need_after_label)
    {
        gen_label(after_label, sizeof(after_label), ctx);
        /* Unwind everything if no handler found */
        inst_list = codegen_branch_through_finally(ctx, inst_list, symtab, after_label, 0);

        char buffer[32];
        snprintf(buffer, sizeof(buffer), "%s:\n", after_label);
        inst_list = add_inst(inst_list, buffer);
    }

    char buffer[96];
    if (codegen_target_is_windows())
        snprintf(buffer, sizeof(buffer), "\tmovq\tkgpc_current_exception(%%rip), %%rcx\n");
    else
        snprintf(buffer, sizeof(buffer), "\tmovq\tkgpc_current_exception(%%rip), %%rdi\n");
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_raise");
    free_arg_regs();
    inst_list = add_inst(inst_list, "\tud2\n");
    return inst_list;
}

static ListNode_t *codegen_inherited(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    (void)symtab;

    if (stmt == NULL)
        return inst_list;

    int is_class_method = 0;
    if (ctx != NULL && ctx->current_subprogram_owner_class != NULL)
        is_class_method = 1;

    struct Expression *call_expr = stmt->stmt_data.inherited_data.call_expr;
    if (!is_class_method)
    {
        /* Outside of class methods, inherited statements are parsed for compatibility
         * but have no runtime effect yet. */
        inst_list = add_inst(inst_list, "\t# INHERITED statement ignored (no class context)\n");
        return inst_list;
    }

    if (call_expr != NULL)
    {
        inst_list = codegen_expr(call_expr, inst_list, ctx);
        if (codegen_had_error(ctx))
            return inst_list;
    }
    else
    {
        inst_list = add_inst(inst_list, "\t# INHERITED statement without parent call\n");
    }
    return inst_list;
}
