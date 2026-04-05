ListNode_t *codegen_builtin_proc(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_PROCEDURE_CALL);
    assert(ctx != NULL);

    char *proc_name;
    ListNode_t *args_expr;
    /* Long mangled procedure names require a generous buffer for emitted instructions. */
    char buffer[CODEGEN_MAX_INST_BUF];

    proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
    args_expr = stmt->stmt_data.procedure_call_data.expr_args;

    const char *proc_id_lookup = stmt->stmt_data.procedure_call_data.id;

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "SetLength"))
    {
        inst_list = codegen_builtin_setlength(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "SetString"))
    {
        inst_list = codegen_builtin_setstring(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "write"))
    {
        inst_list = codegen_builtin_write_like(stmt, inst_list, ctx, 0);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "writeln"))
    {
        inst_list = codegen_builtin_write_like(stmt, inst_list, ctx, 1);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "writestr"))
    {
        inst_list = codegen_builtin_writestr(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "read"))
    {
        inst_list = codegen_builtin_read_like(stmt, inst_list, ctx, 0);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "readln"))
    {
        inst_list = codegen_builtin_read_like(stmt, inst_list, ctx, 1);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Str"))
    {
        inst_list = codegen_builtin_str(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Insert"))
    {
        inst_list = codegen_builtin_insert(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Delete"))
    {
        inst_list = codegen_builtin_delete(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Inc"))
    {
        inst_list = codegen_builtin_inc(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Dec"))
    {
        inst_list = codegen_builtin_dec(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "New"))
    {
        inst_list = codegen_builtin_new(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Dispose"))
    {
        inst_list = codegen_builtin_dispose(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Val"))
    {
        inst_list = codegen_builtin_val(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Move"))
    {
        inst_list = codegen_builtin_move(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Include"))
    {
        inst_list = codegen_builtin_include(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Exclude"))
    {
        inst_list = codegen_builtin_exclude(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Assert"))
    {
        /* Assert(condition [, message])
         * Evaluate the boolean condition. If true, continue execution.
         * If false, call kgpc_assert_failed(msg, filename, line) which
         * prints the assertion failure and exits with code 227. */
        ListNode_t *assert_args = stmt->stmt_data.procedure_call_data.expr_args;
        struct Expression *cond_expr = (assert_args != NULL) ? (struct Expression *)assert_args->cur : NULL;
        struct Expression *msg_expr = (assert_args != NULL && assert_args->next != NULL)
                                      ? (struct Expression *)assert_args->next->cur : NULL;

        if (cond_expr != NULL)
        {
            int relop_type = 0;
            inst_list = codegen_condition_expr(cond_expr, inst_list, ctx, &relop_type);

            /* Generate label for the "pass" path (condition was true) */
            char pass_label[18];
            gen_label(pass_label, 18, ctx);

            /* Jump to pass_label if condition is TRUE (non-zero).
             * codegen_condition_expr with a boolean expr does testl and sets relop_type=NE.
             * gencode_jmp(NE, inverse=0, ...) emits jne label (jump if not-equal-to-zero = true). */
            inst_list = gencode_jmp(relop_type, 0, pass_label, inst_list);

            /* Failure path: call kgpc_assert_failed(msg, filename, line)
             * ABI: Windows uses rcx/rdx/r8, SysV uses rdi/rsi/rdx */
            const char *arg1_reg = codegen_target_is_windows() ? "%rcx" : "%rdi";
            const char *arg2_reg = codegen_target_is_windows() ? "%rdx" : "%rsi";
            const char *arg3_reg_32 = codegen_target_is_windows() ? "%r8d" : "%edx";

            /* Set up message argument (arg1) */
            if (msg_expr != NULL && msg_expr->type == EXPR_STRING && msg_expr->expr_data.string != NULL)
            {
                const char *readonly_section = codegen_readonly_section_directive();
                char msg_label[64];
                snprintf(msg_label, sizeof(msg_label), ".LC%d", ctx->write_label_counter++);
                char escaped_msg[CODEGEN_MAX_INST_BUF];
                escape_string(escaped_msg, msg_expr->expr_data.string, sizeof(escaped_msg));
                char rodata_buf[CODEGEN_MAX_INST_BUF + 128];
                snprintf(rodata_buf, sizeof(rodata_buf), "%s\n%s:\n\t.string \"%s\"\n%s\n",
                         readonly_section, msg_label, escaped_msg, codegen_text_section_resume());
                inst_list = add_inst(inst_list, rodata_buf);
                snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", msg_label, arg1_reg);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                /* No message provided - pass empty string */
                const char *readonly_section = codegen_readonly_section_directive();
                char msg_label[64];
                snprintf(msg_label, sizeof(msg_label), ".LC%d", ctx->write_label_counter++);
                char rodata_buf[256];
                snprintf(rodata_buf, sizeof(rodata_buf), "%s\n%s:\n\t.string \"\"\n%s\n",
                         readonly_section, msg_label, codegen_text_section_resume());
                inst_list = add_inst(inst_list, rodata_buf);
                snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", msg_label, arg1_reg);
                inst_list = add_inst(inst_list, buffer);
            }
            /* filename argument (arg2) - empty for now */
            {
                const char *readonly_section = codegen_readonly_section_directive();
                char fn_label[64];
                snprintf(fn_label, sizeof(fn_label), ".LC%d", ctx->write_label_counter++);
                char rodata_buf[256];
                snprintf(rodata_buf, sizeof(rodata_buf), "%s\n%s:\n\t.string \"\"\n%s\n",
                         readonly_section, fn_label, codegen_text_section_resume());
                inst_list = add_inst(inst_list, rodata_buf);
                snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", fn_label, arg2_reg);
                inst_list = add_inst(inst_list, buffer);
            }
            /* line number argument (arg3) */
            snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", stmt->line_num, arg3_reg_32);
            inst_list = add_inst(inst_list, buffer);
            /* Zero %eax (no vector regs) and call with shadow space */
            inst_list = add_inst(inst_list, "\txorl\t%eax, %eax\n");
            inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_assert_failed");

            /* Emit pass label */
            snprintf(buffer, sizeof(buffer), "%s:\n", pass_label);
            inst_list = add_inst(inst_list, buffer);
        }
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s (Assert)\n", __func__);
        #endif
        return inst_list;
    }

    const char *builtin_id = stmt->stmt_data.procedure_call_data.id;
    if ((proc_name != NULL && pascal_identifier_equals(proc_name, "fpc_in_prefetch_var")) ||
        (builtin_id != NULL && pascal_identifier_equals(builtin_id, "Prefetch")))
    {
        inst_list = codegen_builtin_prefetch(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    const char *proc_name_hint = stmt->stmt_data.procedure_call_data.id;
    if (proc_name_hint == NULL)
        proc_name_hint = stmt->stmt_data.procedure_call_data.mangled_id;

    inst_list = codegen_pass_arguments(args_expr, inst_list, ctx, NULL,
        proc_name_hint, 0, NULL, 0);
    inst_list = codegen_vect_reg(inst_list, 0);
    const char *call_target = (proc_name != NULL) ? proc_name : stmt->stmt_data.procedure_call_data.id;
    if (call_target != NULL && pascal_identifier_equals(call_target, "fpc_in_prefetch_var"))
    {
        return codegen_builtin_prefetch(stmt, inst_list, ctx);
    }
    if (call_target == NULL)
        call_target = "";
    snprintf(buffer, 50, "\tcall\t%s\n", call_target);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_cleanup_call_stack(inst_list, ctx);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Returns a list of instructions */
ListNode_t *codegen_compound_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_COMPOUND_STATEMENT);
    assert(ctx != NULL);
    assert(symtab != NULL);

    ListNode_t *stmt_list;
    struct Statement *cur_stmt;

    stmt_list = stmt->stmt_data.compound_statement;
    while(stmt_list != NULL)
    {
        cur_stmt = (struct Statement *)stmt_list->cur;
        inst_list = codegen_stmt(cur_stmt, inst_list, ctx, symtab);
        stmt_list = stmt_list->next;
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

static int codegen_expr_is_extended_storage(const struct Expression *expr)
{
    KgpcType *type = expr_get_kgpc_type(expr);
    return kgpc_type_is_extended(type);
}

static ListNode_t *codegen_assign_extended_value(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (dest_expr == NULL || src_expr == NULL || ctx == NULL)
        return inst_list;

    Register_t *dest_addr = NULL;
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_addr);
    if (codegen_had_error(ctx) || dest_addr == NULL)
    {
        if (dest_addr != NULL)
            free_reg(get_reg_stack(), dest_addr);
        return inst_list;
    }

    char buffer[CODEGEN_MAX_INST_BUF];
    if (codegen_expr_is_extended_storage(src_expr) && codegen_expr_is_addressable(src_expr))
    {
        Register_t *src_addr = NULL;
        inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_addr);
        if (codegen_had_error(ctx) || src_addr == NULL)
        {
            free_reg(get_reg_stack(), dest_addr);
            if (src_addr != NULL)
                free_reg(get_reg_stack(), src_addr);
            return inst_list;
        }

        if (codegen_target_is_windows())
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_addr->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_addr->bit_64);
            inst_list = add_inst(inst_list, buffer);
            inst_list = add_inst(inst_list, "\tmovl\t$10, %r8d\n");
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_addr->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_addr->bit_64);
            inst_list = add_inst(inst_list, buffer);
            inst_list = add_inst(inst_list, "\tmovl\t$10, %edx\n");
        }
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
        free_arg_regs();
        free_reg(get_reg_stack(), src_addr);
        free_reg(get_reg_stack(), dest_addr);
        return inst_list;
    }

    inst_list = codegen_materialize_extended_expr(src_expr, inst_list, ctx, dest_addr);
    free_reg(get_reg_stack(), dest_addr);
    return inst_list;
}

/* Code generation for a variable assignment */
ListNode_t *codegen_var_assignment(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_VAR_ASSIGN);
    assert(ctx != NULL);

    StackNode_t *var;
    Register_t *reg;
    char buffer[CODEGEN_MAX_INST_BUF];
    struct Expression *var_expr, *assign_expr;
    int offset;

    var_expr = stmt->stmt_data.var_assign_data.var;
    assign_expr = stmt->stmt_data.var_assign_data.expr;
    codegen_hydrate_array_literal_from_lhs(var_expr, assign_expr, ctx);

    /* Handle string typecast on LHS: e.g., RawByteString(ptr) := 'value'
     * The typecast means this pointer should be treated as a string variable.
     * Generate a string assignment (kgpc_string_assign) instead of direct store. */
    int lhs_is_string_typecast = 0;
    if (var_expr != NULL && var_expr->type == EXPR_TYPECAST &&
        var_expr->expr_data.typecast_data.expr != NULL)
    {
        int target_type = var_expr->expr_data.typecast_data.target_type;
        if (is_string_type(target_type))
            lhs_is_string_typecast = 1;
        else
            var_expr = var_expr->expr_data.typecast_data.expr;
    }

    /* Handle string typecast on LHS: e.g., RawByteString(ptr) := ''
     * Treat the pointer location as a string variable — get its address
     * and call kgpc_string_assign so refcounting works properly.
     * Special case: RHS is empty string '' → store nil directly (FPC
     * AnsiString semantics: empty string == nil pointer). */
    if (lhs_is_string_typecast)
    {
        struct Expression *inner = var_expr->expr_data.typecast_data.expr;

        /* Detect empty string literal: RawByteString(ptr) := ''
         * In FPC, this means "set pointer to nil". */
        int rhs_is_empty = (assign_expr != NULL && assign_expr->type == EXPR_STRING &&
            assign_expr->expr_data.string != NULL &&
            assign_expr->expr_data.string[0] == '\0');
        if (rhs_is_empty)
        {
            /* Store nil directly into the pointer location */
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(inner, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }
            char buffer[CODEGEN_MAX_INST_BUF];
            snprintf(buffer, sizeof(buffer), "\tmovq\t$0, (%s)\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        /* Non-empty string: evaluate RHS and call kgpc_string_assign */
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        /* Get address of the inner pointer (the location to assign to) */
        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(inner, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            if (addr_reg != NULL)
                free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        inst_list = codegen_call_string_assign(inst_list, ctx, addr_reg, value_reg);
        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    if (codegen_expr_is_mp_integer(var_expr))
    {
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || value_reg == NULL || addr_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            if (addr_reg != NULL)
                free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        inst_list = codegen_call_mpint_assign(inst_list, addr_reg, value_reg);
        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reg);
        free_arg_regs();
        return inst_list;
    }

    int dest_is_static_array = (var_expr->is_array_expr && !expr_is_dynamic_array(var_expr)) ||
        expr_is_static_array_like(var_expr, ctx);
    int src_is_static_array = expr_is_static_array_like(assign_expr, ctx);

    if (dest_is_static_array && src_is_static_array)
    {
        /* Static array assignment (including record fields) */
        return codegen_assign_static_array(var_expr, assign_expr, inst_list, ctx);
    }
    else if (var_expr->type == EXPR_RECORD_ACCESS)
    {
        struct RecordField *field = codegen_lookup_record_field(var_expr);
        if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL)
        {
            const char *fid = var_expr->expr_data.record_access_data.field_id;
            fprintf(stderr, "[codegen] static_array_check: field=%s found=%p is_array=%d\n",
                fid ? fid : "<null>", (void*)field, field ? field->is_array : -1);
        }
        if (field != NULL)
        {
            int field_is_static_array = (field->is_array && !field->array_is_open);
            if (!field_is_static_array && field->type_id != NULL)
            {
                struct TypeAlias *alias = codegen_lookup_type_alias(ctx, field->type_id);
                if (alias != NULL && alias->is_array && !alias->is_open_array)
                    field_is_static_array = 1;
            }

            if (field_is_static_array && src_is_static_array)
                return codegen_assign_static_array(var_expr, assign_expr, inst_list, ctx);

            /* Large set fields (> 4 bytes, e.g. set of enum with > 32 elements)
             * need memory-based construction via codegen_assign_record_value,
             * which will force the set literal into the 32-byte path. */
            if (field->type == SET_TYPE && field->has_cached_layout && field->cached_size > 4)
                return codegen_assign_record_value(var_expr, assign_expr, inst_list, ctx);
        }
    }

    if (assign_expr != NULL && assign_expr->type == EXPR_RECORD_CONSTRUCTOR)
        return codegen_assign_record_value(var_expr, assign_expr, inst_list, ctx);

    KgpcType *lhs_kgpc_type = expr_get_kgpc_type(var_expr);
    KgpcType *rhs_kgpc_type = expr_get_kgpc_type(assign_expr);
    int lhs_is_pointer_destination = 0;
    int rhs_is_address_value = 0;
    int rhs_is_addressable_record = 0;

    if (lhs_kgpc_type != NULL &&
        (kgpc_type_is_pointer(lhs_kgpc_type) || kgpc_type_is_procedure(lhs_kgpc_type)))
    {
        lhs_is_pointer_destination = 1;
    }
    else
    {
        int lhs_tag = expr_get_type_tag(var_expr);
        lhs_is_pointer_destination = (lhs_tag == POINTER_TYPE || lhs_tag == PROCEDURE);
    }

    rhs_is_address_value = (assign_expr != NULL &&
        (assign_expr->type == EXPR_ADDR || assign_expr->type == EXPR_ADDR_OF_PROC));
    rhs_is_addressable_record = (assign_expr != NULL &&
        rhs_kgpc_type != NULL &&
        kgpc_type_is_record(rhs_kgpc_type) &&
        (assign_expr->type == EXPR_VAR_ID || assign_expr->type == EXPR_RECORD_ACCESS));

    if (lhs_is_pointer_destination && rhs_is_addressable_record && !rhs_is_address_value)
    {
        assign_expr = mk_addressof(assign_expr->line_num, assign_expr);
        rhs_is_address_value = 1;
    }

    int lhs_is_record_value = 0;
    if (lhs_kgpc_type != NULL)
        lhs_is_record_value = kgpc_type_is_record(lhs_kgpc_type);
    else if (expr_get_type_tag(var_expr) == RECORD_TYPE && !var_expr->is_array_expr)
        lhs_is_record_value = 1;

    if (lhs_is_pointer_destination && rhs_is_address_value)
        lhs_is_record_value = 0;

    if (lhs_is_record_value)
        return codegen_assign_record_value(var_expr, assign_expr, inst_list, ctx);

    if (codegen_expr_is_extended_storage(var_expr))
        return codegen_assign_extended_value(var_expr, assign_expr, inst_list, ctx);

    /* Character sets (set of char) need special handling like records due to 32-byte size */
    if (expr_get_type_tag(var_expr) == SET_TYPE && expr_is_char_set_ctx(var_expr, ctx))
        return codegen_assign_record_value(var_expr, assign_expr, inst_list, ctx);

    /* ShortStrings returned via SRET still need shortstring-aware copy semantics.
     * Route them through the static-array helper so sized shortstrings preserve
     * their actual capacity instead of being treated as 256-byte records. */
    if ((expr_get_type_tag(var_expr) == SHORTSTRING_TYPE ||
         codegen_expr_is_shortstring_array(var_expr)) &&
        !expr_is_dynamic_array(var_expr) &&
        assign_expr != NULL && assign_expr->type == EXPR_FUNCTION_CALL &&
        (expr_get_type_tag(assign_expr) == SHORTSTRING_TYPE ||
         codegen_expr_is_shortstring_value_ctx(assign_expr, ctx)))
    {
        /* Check resolved_kgpc_type: if it's an array of char (not shortstring),
         * this is a plain char array mislabeled as shortstring. Don't intercept. */
        int really_shortstring = 1;
        KgpcType *lhs_ktype = expr_get_kgpc_type(var_expr);
        if (lhs_ktype != NULL && kgpc_type_is_array(lhs_ktype) &&
            !kgpc_type_is_shortstring(lhs_ktype))
        {
            KgpcType *elem = lhs_ktype->info.array_info.element_type;
            if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE &&
                (elem->info.primitive_type_tag == CHAR_TYPE ||
                 elem->info.primitive_type_tag == BYTE_TYPE))
                really_shortstring = 0;
        }
        if (really_shortstring)
        {
            Register_t *dest_addr = NULL;
            Register_t *src_addr = NULL;
            int array_size = codegen_get_shortstring_capacity(var_expr, ctx);
            if (array_size <= 1)
            {
                long long direct_size = expr_effective_size_bytes(var_expr);
                if (direct_size > 1 && direct_size <= INT_MAX)
                    array_size = (int)direct_size;
                else
                    array_size = 256;
            }

            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &dest_addr);
            if (codegen_had_error(ctx) || dest_addr == NULL)
            {
                if (dest_addr != NULL)
                    free_reg(get_reg_stack(), dest_addr);
                return inst_list;
            }

            inst_list = codegen_address_for_expr(assign_expr, inst_list, ctx, &src_addr);
            if (codegen_had_error(ctx) || src_addr == NULL)
            {
                free_reg(get_reg_stack(), dest_addr);
                if (src_addr != NULL)
                    free_reg(get_reg_stack(), src_addr);
                return inst_list;
            }

            inst_list = codegen_call_shortstring_copy(inst_list, ctx, dest_addr, array_size, src_addr);
            free_reg(get_reg_stack(), dest_addr);
            free_reg(get_reg_stack(), src_addr);
            return inst_list;
        }
        /* Fall through to EXPR_RECORD_ACCESS handler which uses char_array copy */
    }

    if (var_expr->type == EXPR_VAR_ID)
    {
        if (expr_is_dynamic_array(var_expr))
        {
            inst_list = codegen_assign_dynamic_array(var_expr, assign_expr, inst_list, ctx);
            return inst_list;
        }

        int scope_depth = 0;
        var = find_label_with_depth(var_expr->expr_data.id, &scope_depth);

        if (var == NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindSymbol(&target_node, ctx->symtab, var_expr->expr_data.id) != 0 &&
                target_node != NULL)
            {
                if (target_node->mangled_id != NULL)
                {
                    var = find_label_with_depth(target_node->mangled_id, &scope_depth);
                }
                if (var == NULL &&
                    (target_node->hash_type == HASHTYPE_FUNCTION_RETURN ||
                     target_node->hash_type == HASHTYPE_FUNCTION))
                {
                    long long size_bytes = 0;
                    if (var_expr->resolved_kgpc_type != NULL)
                        size_bytes = kgpc_type_sizeof(var_expr->resolved_kgpc_type);
                    if (size_bytes <= 0 || size_bytes > INT_MAX)
                        size_bytes = CODEGEN_POINTER_SIZE_BYTES;

                    var = add_l_x(var_expr->expr_data.id, (int)size_bytes);
                    scope_depth = 0;
                }
            }
        }

        
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);

        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        /* When assigning an extended-returning function call to a Real variable,
         * value_reg holds a pointer to the sret buffer containing the 10-byte
         * extended value.  Convert it to a double via kgpc_load_extended_to_bits
         * so the store below writes the correct IEEE-754 double bits. */
        int var_type = expr_get_type_tag(var_expr);
        if (var_type == REAL_TYPE &&
            assign_expr != NULL && assign_expr->type == EXPR_FUNCTION_CALL &&
            expr_returns_sret(assign_expr) &&
            codegen_expr_involves_extended(assign_expr))
        {
            char ext_buf[128];
            if (codegen_target_is_windows())
                snprintf(ext_buf, sizeof(ext_buf), "\tmovq\t%s, %%rcx\n", value_reg->bit_64);
            else
                snprintf(ext_buf, sizeof(ext_buf), "\tmovq\t%s, %%rdi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, ext_buf);
            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_load_extended_to_bits");
            free_arg_regs();
            /* Result is in %rax as double-bit-pattern; move to value_reg */
            snprintf(ext_buf, sizeof(ext_buf), "\tmovq\t%%rax, %s\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, ext_buf);
        }

        int assign_type = expr_get_type_tag(assign_expr);
        int skip_real_coercion = 0;
        if (var != NULL && var_type == REAL_TYPE)
        {
            long long unaligned_size_probe = var->element_size > 0 ? var->element_size : var->size;
            if (var_expr != NULL && var_expr->type == EXPR_RECORD_ACCESS)
            {
                long long field_size_probe = codegen_record_field_effective_size(var_expr, ctx);
                if (field_size_probe > 0)
                    unaligned_size_probe = field_size_probe;
            }
            if (is_single_float_type(var_type, unaligned_size_probe) &&
                (is_integer_type(assign_type) || assign_type == BOOL ||
                 assign_type == CHAR_TYPE || assign_type == ENUM_TYPE))
            {
                skip_real_coercion = 1;
            }
        }
        int coerced_to_real = 0;
        if (!skip_real_coercion)
        {
            inst_list = codegen_maybe_convert_int_like_to_real(var_type, assign_expr,
                value_reg, inst_list, &coerced_to_real);
        }

        /* Handle string assignment to string variables */
        if (var_type == STRING_TYPE)
        {
            /* Under {$H-}, a variable with STRING_TYPE tag may actually be backed
             * by ShortString storage (256-byte stack buffer with length byte).
             * Detect this and route to shortstring-aware assignment instead of
             * kgpc_string_assign which would corrupt the buffer by writing a pointer. */
            int lhs_is_actually_shortstring =
                codegen_expr_is_shortstring_array(var_expr) ||
                codegen_expr_is_shortstring_value_ctx(var_expr, ctx);
            if (lhs_is_actually_shortstring)
            {
                Register_t *addr_reg = NULL;
                inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
                if (codegen_had_error(ctx) || addr_reg == NULL)
                {
                    free_reg(get_reg_stack(), value_reg);
                    if (addr_reg != NULL)
                        free_reg(get_reg_stack(), addr_reg);
                    return inst_list;
                }

                /* codegen_get_shortstring_capacity returns 256 when capacity
                 * cannot be determined; codegen_call_string_to_shortstring
                 * also guards against invalid (<= 1) values internally. */
                int array_size = codegen_get_shortstring_capacity(var_expr, ctx);

                if (codegen_expr_is_shortstring_rhs(assign_expr, ctx))
                {
                    /* Both sides are ShortString — copy preserving the length byte */
                    inst_list = codegen_call_shortstring_copy(inst_list, ctx, addr_reg, array_size, value_reg);
                }
                else
                {
                    /* RHS is an AnsiString or string literal — convert to ShortString
                     * (kgpc_string_to_shortstring writes length byte + payload) */
                    inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reg, value_reg, array_size);
                }
                free_reg(get_reg_stack(), value_reg);
                free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            int inner_is_shortstring = codegen_expr_is_shortstring_rhs(assign_expr, ctx);

            /* If assigning a char to string, promote it first.
             * Also check for typecasts from char to string (e.g. AnsiString(char_value))
             * where expr_get_type_tag returns STRING_TYPE but the actual value is a char. */
            int assign_type = expr_get_type_tag(assign_expr);
            int target_is_wide_string = codegen_expr_is_wide_string_value(var_expr);
            int source_is_wide_string = codegen_expr_is_wide_string_value(assign_expr);
            int source_needs_wide_promotion = (!source_is_wide_string);
            if (!source_needs_wide_promotion &&
                assign_expr != NULL &&
                assign_expr->type == EXPR_TYPECAST &&
                assign_expr->expr_data.typecast_data.expr != NULL &&
                codegen_expr_is_wide_string_value(assign_expr) &&
                !codegen_expr_is_wide_string_value(assign_expr->expr_data.typecast_data.expr) &&
                (expr_has_type_tag(assign_expr->expr_data.typecast_data.expr, STRING_TYPE) ||
                 expr_has_type_tag(assign_expr->expr_data.typecast_data.expr, SHORTSTRING_TYPE) ||
                 assign_expr->expr_data.typecast_data.expr->type == EXPR_STRING))
            {
                source_needs_wide_promotion = 1;
            }
            if (!inner_is_shortstring && assign_type == CHAR_TYPE)
            {
                inst_list = codegen_promote_char_reg_to_string(inst_list, value_reg);
            }
            else if (!inner_is_shortstring &&
                     assign_expr != NULL && assign_expr->type == EXPR_TYPECAST &&
                     assign_expr->expr_data.typecast_data.expr != NULL &&
                     expr_get_type_tag(assign_expr->expr_data.typecast_data.expr) == CHAR_TYPE)
            {
                inst_list = codegen_promote_char_reg_to_string(inst_list, value_reg);
            }

            int rhs_lower = 0;
            int rhs_upper = -1;
            int rhs_is_shortstring = 0;
            if (codegen_get_char_array_bounds(assign_expr, ctx, &rhs_lower, &rhs_upper, &rhs_is_shortstring) &&
                !rhs_is_shortstring)
            {
                Register_t *addr_reg = NULL;
                Register_t *src_reg = NULL;
                inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
                inst_list = codegen_address_for_expr(assign_expr, inst_list, ctx, &src_reg);
                free_reg(get_reg_stack(), value_reg);
                if (codegen_had_error(ctx) || addr_reg == NULL || src_reg == NULL)
                {
                    if (addr_reg != NULL)
                        free_reg(get_reg_stack(), addr_reg);
                    if (src_reg != NULL)
                        free_reg(get_reg_stack(), src_reg);
                    return inst_list;
                }

                int src_len = rhs_upper - rhs_lower + 1;
                if (src_len < 0)
                    src_len = 0;
                inst_list = codegen_call_string_assign_from_char_array(inst_list, ctx,
                    addr_reg, src_reg, src_len);
                free_reg(get_reg_stack(), addr_reg);
                free_reg(get_reg_stack(), src_reg);
                return inst_list;
            }

            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                free_reg(get_reg_stack(), value_reg);
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            /* When the RHS is a ShortString (e.g. from strpas/StrPas), use a dedicated
             * conversion function that strips the length byte and builds a proper AnsiString.
             * Also handle typecasts like TFormatString(HexStr(...)) where the outer type
             * is AnsiString but the inner expression returns ShortString. */
            if (inner_is_shortstring)
                inst_list = codegen_call_string_assign_func(inst_list, ctx, addr_reg, value_reg,
                    "kgpc_string_assign_from_shortstring");
            else if (target_is_wide_string && source_is_wide_string)
                inst_list = codegen_call_string_assign_func(inst_list, ctx, addr_reg, value_reg,
                    "kgpc_unicodestring_assign");
            else if (target_is_wide_string && source_needs_wide_promotion)
                inst_list = codegen_call_string_assign_func(inst_list, ctx, addr_reg, value_reg,
                    "kgpc_unicodestring_assign_from_string");
            else if (!target_is_wide_string && source_is_wide_string)
                inst_list = codegen_call_string_assign_func(inst_list, ctx, addr_reg, value_reg,
                    "kgpc_string_assign_from_unicodestring");
            else
                inst_list = codegen_call_string_assign(inst_list, ctx, addr_reg, value_reg);
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
        
        /* Handle shortstring assignment to char arrays */
        int array_lower = 0;
        int array_upper = -1;
        int array_is_shortstring = 0;
        if (codegen_get_char_array_bounds(var_expr, ctx, &array_lower, &array_upper, &array_is_shortstring) &&
            (codegen_expr_is_shortstring_value_local(assign_expr) ||
             expr_get_type_tag(assign_expr) == SHORTSTRING_TYPE))
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                free_reg(get_reg_stack(), value_reg);
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            int array_size = array_upper - array_lower + 1;
            if (array_size < 0)
                array_size = 0;
            if (array_is_shortstring)
            {
                if (var_expr != NULL && var_expr->type == EXPR_VAR_ID &&
                    codegen_is_current_return_var_id(var_expr, ctx))
                {
                    int short_capacity = codegen_get_current_return_shortstring_capacity(
                        ctx, ctx != NULL ? ctx->symtab : NULL);
                    if (short_capacity > 1)
                        array_size = short_capacity;
                }
                int short_capacity = codegen_get_shortstring_capacity(var_expr, ctx);
                if (short_capacity > 1)
                    array_size = short_capacity;
            }

            if (array_is_shortstring)
            {
                if (assign_expr != NULL && assign_expr->type == EXPR_STRING)
                {
                    /* Literal RHS is a C string pointer; build a proper ShortString
                     * (length byte + payload) instead of treating it as ShortString data. */
                    inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reg, value_reg, array_size);
                }
                else
                {
                    /* Dest is ShortString — use shortstring-to-shortstring copy
                     * which preserves the length byte.
                     * kgpc_shortstring_to_shortstring(dest, dest_size, src) */
                    inst_list = codegen_call_shortstring_copy(inst_list, ctx, addr_reg, array_size, value_reg);
                }
            }
            else
            {
                inst_list = codegen_call_shortstring_to_char_array(inst_list, ctx, addr_reg, value_reg, array_size);
            }
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        /* Handle string literal assignment to char arrays */
        array_lower = 0;
        array_upper = -1;
        array_is_shortstring = 0;
        if (expr_get_type_tag(assign_expr) == STRING_TYPE &&
            codegen_get_char_array_bounds(var_expr, ctx, &array_lower, &array_upper, &array_is_shortstring))
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                free_reg(get_reg_stack(), value_reg);
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            int array_size = array_upper - array_lower + 1;
            if (array_size < 0)
                array_size = 0;
            
            if (array_is_shortstring)
            {
                /* Use ShortString-specific copy that sets length byte at index 0 */
                inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reg, value_reg, array_size);
            }
            else
            {
                /* Regular char array copy */
                inst_list = codegen_call_string_to_char_array(inst_list, ctx, addr_reg, value_reg, array_size);
            }
            
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        /* Handle string literal assignment to SHORTSTRING_TYPE elements (e.g., Names[0] := 'ONE') */
        /* Also check codegen_array_access_targets_shortstring for cases where type info is not available
         * (like auto-generated initializer statements from const arrays) */
        int targets_shortstring = codegen_array_access_targets_shortstring(var_expr, ctx);
        int record_targets_shortstring = 0;
        if (var_expr != NULL && var_expr->type == EXPR_RECORD_ACCESS)
        {
            struct RecordField *target_field = codegen_lookup_record_field(var_expr);
            if (target_field != NULL && target_field->type == SHORTSTRING_TYPE)
                record_targets_shortstring = 1;
        }
        if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL && var_expr->type == EXPR_ARRAY_ACCESS)
        {
            struct Expression *base = var_expr->expr_data.array_access_data.array_expr;
            fprintf(stderr, "[codegen] var_assignment: var_type=%d, targets_shortstring=%d, assign_type=%d",
                var_type, targets_shortstring, expr_get_type_tag(assign_expr));
            if (base != NULL && base->type == EXPR_VAR_ID)
                fprintf(stderr, ", base_id=%s", base->expr_data.id);
            fprintf(stderr, "\n");
        }
        if ((var_type == SHORTSTRING_TYPE || expr_has_type_tag(var_expr, SHORTSTRING_TYPE) ||
             codegen_expr_is_shortstring_array(var_expr) ||
             targets_shortstring || record_targets_shortstring))
        {
            int rhs_lower = 0;
            int rhs_upper = -1;
            int rhs_is_shortstring = 0;
            int lhs_lower = 0;
            int lhs_upper = -1;
            int lhs_is_shortstring = 0;

            if (codegen_get_char_array_bounds(assign_expr, ctx, &rhs_lower, &rhs_upper, &rhs_is_shortstring) &&
                !rhs_is_shortstring &&
                codegen_get_char_array_bounds(var_expr, ctx, &lhs_lower, &lhs_upper, &lhs_is_shortstring))
            {
                Register_t *addr_reg = NULL;
                inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
                if (codegen_had_error(ctx) || addr_reg == NULL)
                {
                    free_reg(get_reg_stack(), value_reg);
                    if (addr_reg != NULL)
                        free_reg(get_reg_stack(), addr_reg);
                    return inst_list;
                }

                int src_len = rhs_upper - rhs_lower + 1;
                int dest_size = lhs_upper - lhs_lower + 1;
                if (src_len < 0) src_len = 0;
                if (dest_size < 0) dest_size = 0;

                inst_list = codegen_call_char_array_to_shortstring(inst_list, ctx, addr_reg, value_reg,
                    src_len, dest_size);
                free_reg(get_reg_stack(), value_reg);
                free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            if (expr_get_type_tag(assign_expr) != STRING_TYPE)
            {
                /* Fall through to default handling if RHS is not a string literal/value. */
            }
            else
            {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                free_reg(get_reg_stack(), value_reg);
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            /* For shortstrings, use the array bounds to determine size.
             * If bounds are not available, default to 256 (standard ShortString). */
            int array_size = codegen_get_shortstring_capacity(var_expr, ctx);
            if (array_size <= 1)
                array_size = 256;
            
            /* Use ShortString-specific copy that sets length byte at index 0 */
            inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reg, value_reg, array_size);
            
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
            }
        }

        /* ShortString assignment should use shortstring-aware copy semantics,
         * not generic record copies. */
        if ((var_type == SHORTSTRING_TYPE || expr_has_type_tag(var_expr, SHORTSTRING_TYPE) ||
             codegen_expr_is_shortstring_array(var_expr) ||
             targets_shortstring || record_targets_shortstring) &&
            expr_get_type_tag(assign_expr) == SHORTSTRING_TYPE)
        {
            return codegen_assign_static_array(var_expr, assign_expr, inst_list, ctx);
        }

        /* Handle CHAR assignment to ShortString arrays - set length=1 and store char at position 1 */
        if (codegen_expr_is_shortstring_array(var_expr) &&
            expr_get_type_tag(assign_expr) == CHAR_TYPE)
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                free_reg(get_reg_stack(), value_reg);
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            char buffer[128];
            /* Store length=1 at position 0 */
            snprintf(buffer, sizeof(buffer), "\tmovb\t$1, (%s)\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            /* Store the character at position 1 */
            const char *char_reg = register_name8(value_reg);
            if (char_reg != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovb\t%s, 1(%s)\n", char_reg, addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                codegen_report_error(ctx, "ERROR: Could not get 8-bit register name for character storage");
            }
            
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        reg = value_reg;

        if(var != NULL)
        {
            int use_qword = codegen_type_uses_qword(var_type);
            /* Override for Single type (4-byte float): check actual storage size
             * Use element_size which stores the unaligned size, not size which may be padded */
            long long unaligned_size = var->element_size > 0 ? var->element_size : var->size;
            if (var_expr != NULL && var_expr->type == EXPR_RECORD_ACCESS)
            {
                long long field_size = codegen_record_field_effective_size(var_expr, ctx);
                if (field_size > 0)
                    unaligned_size = field_size;
            }
            int is_single_target = is_single_float_type(var_type, unaligned_size) && !var->is_reference;
            if (is_single_target)
                use_qword = 0;
            if (!var->is_reference && var->size >= 8 &&
                (var_expr == NULL || var_expr->type != EXPR_RECORD_ACCESS) &&
                !is_single_target)
                use_qword = 1;
            
            /* For Single targets with real source, convert double to single precision.
             * Only convert if the source expression is a real type (double), not integer etc. */
            if (is_single_target && assign_type == REAL_TYPE)
            {
                /* Real expressions are materialized at double precision in registers.
                 * Narrow to Single before storing into a Single target. */
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                inst_list = add_inst(inst_list, "\tcvtsd2ss\t%xmm0, %xmm0\n");
                snprintf(buffer, sizeof(buffer), "\tmovd\t%%xmm0, %s\n", reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
            }
            else if (is_single_target &&
                (is_integer_type(assign_type) || assign_type == BOOL ||
                 assign_type == CHAR_TYPE || assign_type == ENUM_TYPE))
            {
                /* Integer-like source to Single target requires numeric conversion,
                 * not raw bit reinterpretation. */
                snprintf(buffer, sizeof(buffer), "\tcvtsi2ss\t%s, %%xmm0\n", reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovd\t%%xmm0, %s\n", reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
            }
            
            int use_byte = 0;
            int use_word = 0;
            const char *value_reg8 = NULL;
            const char *value_reg16 = NULL;
            long long target_size = (var_expr->type == EXPR_RECORD_ACCESS) ?
                codegen_record_field_effective_size(var_expr, ctx) :
                expr_effective_size_bytes(var_expr);
            /* Cross-check with resolved_kgpc_type for sub-dword fields
             * (e.g. Integer=SmallInt in FPC mode where type tag says 4
             * but actual storage is 2 bytes). */
            if (target_size == 4 && var_expr != NULL &&
                var_expr->resolved_kgpc_type != NULL)
            {
                long long resolved_size = kgpc_type_sizeof(var_expr->resolved_kgpc_type);
                if (resolved_size > 0 && resolved_size < 4)
                    target_size = resolved_size;
            }
            if (!use_qword && var_type == CHAR_TYPE)
            {
                value_reg8 = register_name8(reg);
                if (value_reg8 != NULL)
                {
                    use_byte = 1;
                }
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for character assignment.");
                }
            }
            else if (!use_qword && target_size == 1)
            {
                value_reg8 = register_name8(reg);
                if (value_reg8 != NULL)
                    use_byte = 1;
                else
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for byte-sized assignment.");
            }
            else if (!use_qword && target_size == 2)
            {
                value_reg16 = codegen_register_name16(reg);
                if (value_reg16 != NULL)
                    use_word = 1;
                else
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 16-bit register for assignment.");
            }
            if (use_qword)
            {
                int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
                /* Fallback: check expr type tag for pointer arithmetic / 64-bit ops */
                if (!value_is_qword && assign_expr != NULL)
                {
                    int assign_tag = expr_get_type_tag(assign_expr);
                    if (codegen_type_uses_qword(assign_tag))
                        value_is_qword = 1;
                }
                if (coerced_to_real)
                    value_is_qword = 1;
                /* Check for procedural var calls with pointer return type */
                if (!value_is_qword && assign_expr != NULL &&
                    assign_expr->type == EXPR_FUNCTION_CALL &&
                    assign_expr->expr_data.function_call_data.is_procedural_var_call &&
                    assign_expr->expr_data.function_call_data.call_kgpc_type != NULL)
                {
                    KgpcType *call_type = assign_expr->expr_data.function_call_data.call_kgpc_type;
                    if (call_type->kind == TYPE_KIND_PROCEDURE)
                    {
                        KgpcType *ret_type = kgpc_type_get_return_type(call_type);
                        if (ret_type != NULL && kgpc_type_uses_qword(ret_type))
                            value_is_qword = 1;
                        else if (call_type->info.proc_info.return_type_id != NULL)
                        {
                            const char *ret_id = call_type->info.proc_info.return_type_id;
                            if (kgpc_type_id_uses_qword(ret_id, ctx->symtab))
                                value_is_qword = 1;
                        }
                    }
                }
                if (!value_is_qword)
                {
                    if (assign_expr != NULL && !codegen_expr_is_signed(assign_expr))
                        inst_list = codegen_zero_extend32_to64(inst_list, reg->bit_32, reg->bit_32);
                    else
                        inst_list = codegen_sign_extend32_to64(inst_list, reg->bit_32, reg->bit_64);
                }
            }
            if (var->is_static)
            {
                const char *label = (var->static_label != NULL) ?
                    var->static_label : var->label;
                if (use_qword)
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s(%%rip)\n", reg->bit_64, label);
                else if (use_byte)
                    snprintf(buffer, sizeof(buffer), "\tmovb\t%s, %s(%%rip)\n", value_reg8, label);
                else if (use_word)
                    snprintf(buffer, sizeof(buffer), "\tmovw\t%s, %s(%%rip)\n", value_reg16, label);
                else
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s(%%rip)\n", reg->bit_32, label);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), reg);
                return inst_list;
            }
            if (var->is_reference)
            {
                Register_t *ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (ptr_reg == NULL)
                {
                    free_reg(get_reg_stack(), reg);
                    return codegen_fail_register(ctx, inst_list, NULL,
                        "ERROR: Unable to allocate register for reference assignment.");
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", var->offset, ptr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                if (use_qword)
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", reg->bit_64, ptr_reg->bit_64);
                else if (use_byte)
                    snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", value_reg8, ptr_reg->bit_64);
                else if (use_word)
                    snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", value_reg16, ptr_reg->bit_64);
                else
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", reg->bit_32, ptr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), ptr_reg);
                free_reg(get_reg_stack(), reg);
                return inst_list;
            }
            else if (scope_depth == 0)
            {
                /* Variable is in current scope, assign normally */
                if (use_qword)
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", reg->bit_64, var->offset);
                else if (use_byte)
                    snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", value_reg8, var->offset);
                else if (use_word)
                    snprintf(buffer, sizeof(buffer), "\tmovw\t%s, -%d(%%rbp)\n", value_reg16, var->offset);
                else
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", reg->bit_32, var->offset);
            }
            else
            {
                codegen_begin_expression(ctx);
                Register_t *frame_reg = codegen_acquire_static_link(ctx, &inst_list, scope_depth);
                if (frame_reg != NULL)
                {
                    if (use_qword)
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%s)\n", reg->bit_64, var->offset, frame_reg->bit_64);
                    else if (use_byte)
                        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%s)\n", value_reg8, var->offset, frame_reg->bit_64);
                    else if (use_word)
                        snprintf(buffer, sizeof(buffer), "\tmovw\t%s, -%d(%s)\n", value_reg16, var->offset, frame_reg->bit_64);
                    else
                        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%s)\n", reg->bit_32, var->offset, frame_reg->bit_64);
                }
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: Failed to acquire static link for assignment to %s.",
                        var_expr->expr_data.id);
                    if (use_qword)
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", reg->bit_64, var->offset);
                    else if (use_byte)
                        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", value_reg8, var->offset);
                    else if (use_word)
                        snprintf(buffer, sizeof(buffer), "\tmovw\t%s, -%d(%%rbp)\n", value_reg16, var->offset);
                    else
                        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", reg->bit_32, var->offset);
                }
                codegen_end_expression(ctx);
            }
        }
        else
        {

            inst_list = codegen_get_nonlocal(inst_list, var_expr->expr_data.id, &offset, ctx);
            int use_qword = codegen_type_uses_qword(var_type);
            /* Override for Single type (4-byte float): check resolved type storage */
            long long resolved_size = (var_expr->resolved_kgpc_type != NULL) ?
                kgpc_type_sizeof(var_expr->resolved_kgpc_type) : 8;
            if (is_single_float_type(var_type, resolved_size))
                use_qword = 0;
            int use_byte = 0;
            const char *value_reg8 = NULL;
            if (!use_qword && var_type == CHAR_TYPE)
            {
                value_reg8 = register_name8(reg);
                if (value_reg8 != NULL)
                    use_byte = 1;
                else
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for character assignment.");
            }
            if (use_qword)
            {
                int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
                /* Fallback: check expr type tag for pointer arithmetic / 64-bit ops */
                if (!value_is_qword && assign_expr != NULL)
                {
                    int assign_tag = expr_get_type_tag(assign_expr);
                    if (codegen_type_uses_qword(assign_tag))
                        value_is_qword = 1;
                }
                if (coerced_to_real)
                    value_is_qword = 1;
                /* Check for procedural var calls with pointer return type */
                if (!value_is_qword && assign_expr != NULL &&
                    assign_expr->type == EXPR_FUNCTION_CALL &&
                    assign_expr->expr_data.function_call_data.is_procedural_var_call &&
                    assign_expr->expr_data.function_call_data.call_kgpc_type != NULL)
                {
                    KgpcType *call_type = assign_expr->expr_data.function_call_data.call_kgpc_type;
                    if (call_type->kind == TYPE_KIND_PROCEDURE)
                    {
                        KgpcType *ret_type = kgpc_type_get_return_type(call_type);
                        if (ret_type != NULL && kgpc_type_uses_qword(ret_type))
                            value_is_qword = 1;
                        else if (call_type->info.proc_info.return_type_id != NULL)
                        {
                            const char *ret_id = call_type->info.proc_info.return_type_id;
                            if (kgpc_type_id_uses_qword(ret_id, ctx->symtab))
                                value_is_qword = 1;
                        }
                    }
                }
                if (!value_is_qword)
                {
                    if (assign_expr != NULL && !codegen_expr_is_signed(assign_expr))
                        inst_list = codegen_zero_extend32_to64(inst_list, reg->bit_32, reg->bit_32);
                    else
                        inst_list = codegen_sign_extend32_to64(inst_list, reg->bit_32, reg->bit_64);
                }
                snprintf(buffer, 50, "\tmovq\t%s, -%d(%s)\n", reg->bit_64, offset, current_non_local_reg64());
            }
            else if (use_byte)
            {
                snprintf(buffer, 50, "\tmovb\t%s, -%d(%s)\n", value_reg8, offset, current_non_local_reg64());
            }
            else
            {
                snprintf(buffer, 50, "\tmovl\t%s, -%d(%s)\n", reg->bit_32, offset, current_non_local_reg64());
            }
        }
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), reg);
        return inst_list;
    }
    else if (var_expr->type == EXPR_ARRAY_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_array_element_address(var_expr, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;

        StackNode_t *addr_temp = add_l_t("array_addr");
        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_temp->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);

        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for array store.");
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        int var_type = expr_get_type_tag(var_expr);
        int coerced_to_real = 0;
        inst_list = codegen_maybe_convert_int_like_to_real(var_type, assign_expr,
            value_reg, inst_list, &coerced_to_real);
        int use_qword = codegen_type_uses_qword(var_type);
        /* Get element size from the base array expression, not the access expression */
        struct Expression *array_expr = var_expr->expr_data.array_access_data.array_expr;
        long long element_size = array_expr != NULL ? expr_get_array_element_size(array_expr, ctx) : -1;
        if (var_expr->array_element_size > element_size)
            element_size = var_expr->array_element_size;
        if (element_size < 2 &&
            var_expr->array_element_type_id != NULL &&
            (pascal_identifier_equals(var_expr->array_element_type_id, "WideChar") ||
             pascal_identifier_equals(var_expr->array_element_type_id, "UnicodeChar")))
        {
            element_size = 2;
        }
        if (element_size <= 0)
            element_size = expr_effective_size_bytes(var_expr);
        if (var_expr->array_element_type_id != NULL &&
            (pascal_identifier_equals(var_expr->array_element_type_id, "WideChar") ||
             pascal_identifier_equals(var_expr->array_element_type_id, "UnicodeChar")))
        {
            element_size = 2;
        }
        int use_word = (!use_qword && element_size == 2);
        
        /* Check if target is a shortstring element (e.g., array[...] of string[10]) */
        int targets_shortstring = codegen_array_access_targets_shortstring(var_expr, ctx);
        int record_targets_shortstring = 0;
        if (var_expr != NULL && var_expr->type == EXPR_RECORD_ACCESS)
        {
            struct RecordField *target_field = codegen_lookup_record_field(var_expr);
            if (target_field != NULL && target_field->type == SHORTSTRING_TYPE)
                record_targets_shortstring = 1;
        }
        
        if (var_type == STRING_TYPE)
        {
            if (codegen_expr_is_shortstring_value_ctx(assign_expr, ctx))
            {
                const char *arg_reg64 = current_arg_reg64(0);
                if (arg_reg64 == NULL)
                {
                    free_reg(get_reg_stack(), addr_reload);
                    free_reg(get_reg_stack(), value_reg);
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, arg_reg64);
                inst_list = add_inst(inst_list, buffer);
                inst_list = codegen_vect_reg(inst_list, 0);
                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_to_string");
                snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", value_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                free_arg_regs();
            }
            /* If assigning a char to a string array element, promote it first */
            else if (expr_get_type_tag(assign_expr) == CHAR_TYPE)
            {
                /* Save addr_reload to a temp since kgpc_char_to_string clobbers rax */
                StackNode_t *addr_save = add_l_t("addr_save");
                char arg_buffer[128];
                snprintf(arg_buffer, sizeof(arg_buffer), "\tmovq\t%s, -%d(%%rbp)\n", 
                    addr_reload->bit_64, addr_save->offset);
                inst_list = add_inst(inst_list, arg_buffer);
                
                inst_list = codegen_promote_char_reg_to_string(inst_list, value_reg);
                
                /* Restore addr_reload */
                snprintf(arg_buffer, sizeof(arg_buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    addr_save->offset, addr_reload->bit_64);
                inst_list = add_inst(inst_list, arg_buffer);
            }
            inst_list = codegen_call_string_assign(inst_list, ctx, addr_reload, value_reg);
        }
        else if ((var_type == SHORTSTRING_TYPE || expr_has_type_tag(var_expr, SHORTSTRING_TYPE) ||
                  targets_shortstring || record_targets_shortstring) &&
                 expr_get_type_tag(assign_expr) == STRING_TYPE)
        {
            /* Handle shortstring assignment - copy string content to shortstring buffer */
            int array_size = codegen_get_shortstring_capacity(var_expr, ctx);
            if (array_size <= 1)
                array_size = 256;
            inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reload, value_reg, array_size);
        }
        else if (use_qword)
        {
            int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
            /* Fallback: check expr type tag for pointer arithmetic / 64-bit ops */
            if (!value_is_qword && assign_expr != NULL)
            {
                int assign_tag = expr_get_type_tag(assign_expr);
                if (codegen_type_uses_qword(assign_tag))
                    value_is_qword = 1;
            }
            if (coerced_to_real)
                value_is_qword = 1;
            /* Check for procedural var calls with pointer return type */
            if (!value_is_qword && assign_expr != NULL &&
                assign_expr->type == EXPR_FUNCTION_CALL &&
                assign_expr->expr_data.function_call_data.is_procedural_var_call &&
                assign_expr->expr_data.function_call_data.call_kgpc_type != NULL)
            {
                KgpcType *call_type = assign_expr->expr_data.function_call_data.call_kgpc_type;
                if (call_type->kind == TYPE_KIND_PROCEDURE)
                {
                    KgpcType *ret_type = kgpc_type_get_return_type(call_type);
                    if (ret_type != NULL && kgpc_type_uses_qword(ret_type))
                        value_is_qword = 1;
                    else if (call_type->info.proc_info.return_type_id != NULL)
                    {
                        const char *ret_id = call_type->info.proc_info.return_type_id;
                        if (kgpc_type_id_uses_qword(ret_id, ctx->symtab))
                            value_is_qword = 1;
                    }
                }
            }
            if (!value_is_qword)
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
            snprintf(buffer, 50, "\tmovq\t%s, (%s)\n", value_reg->bit_64, addr_reload->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (use_word)
            {
                const char *value_reg16 = codegen_register_name16(value_reg);
                if (value_reg16 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 16-bit register for array assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovw\t%s, (%s)\n", value_reg16, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else if (var_type == CHAR_TYPE || element_size == 1)
            {
                const char *value_reg8 = register_name8(value_reg);
                if (value_reg8 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for byte assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovb\t%s, (%s)\n", value_reg8, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                snprintf(buffer, 50, "\tmovl\t%s, (%s)\n", value_reg->bit_32, addr_reload->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
        }

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reload);
        return inst_list;
    }
    else if (var_expr->type == EXPR_RECORD_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_record_field_address(var_expr, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;

        StackNode_t *addr_temp = add_l_t("record_addr");
        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_temp->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);

        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for record store.");
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        int var_type_2 = expr_get_type_tag(var_expr);
        if (var_type_2 == UNKNOWN_TYPE && var_expr->type == EXPR_RECORD_ACCESS)
        {
            const char *field_id = var_expr->expr_data.record_access_data.field_id;
            int resolved = lookup_record_field_type(var_expr->record_type, field_id);
            if (resolved != UNKNOWN_TYPE)
            {
                var_type_2 = resolved;
                /* no resolved_type mutation in codegen */
            }
        }
        long long record_element_size = codegen_record_field_effective_size(var_expr, ctx);
        int is_single_real_field = (var_type_2 == REAL_TYPE && record_element_size == 4);
        int assign_type = expr_get_type_tag(assign_expr);
        int assign_is_integer_like = (is_integer_type(assign_type) || assign_type == BOOL ||
            assign_type == CHAR_TYPE || assign_type == ENUM_TYPE);

        int coerced_to_real = 0;
        if (!(is_single_real_field && assign_is_integer_like))
        {
            inst_list = codegen_maybe_convert_int_like_to_real(var_type_2, assign_expr,
                value_reg, inst_list, &coerced_to_real);
        }
        int use_qword = codegen_type_uses_qword(var_type_2);
        if (is_single_real_field)
            use_qword = 0;
        if (!use_qword && record_element_size >= CODEGEN_POINTER_SIZE_BYTES)
            use_qword = 1;
        int use_word = (!use_qword && record_element_size == 2);

        if (is_single_real_field)
        {
            if (assign_is_integer_like)
            {
                snprintf(buffer, sizeof(buffer), "\tcvtsi2ss\t%s, %%xmm0\n", value_reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovd\t%%xmm0, %s\n", value_reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
            }
            else if (assign_type == REAL_TYPE)
            {
                int source_is_qword_real = expr_uses_qword_kgpctype(assign_expr) || coerced_to_real;
                if (source_is_qword_real)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", value_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcvtsd2ss\t%xmm0, %xmm0\n");
                    snprintf(buffer, sizeof(buffer), "\tmovd\t%%xmm0, %s\n", value_reg->bit_32);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
        }

        /* Check if the target record field is a char array (e.g., Name: array[0..255] of AnsiChar).
         * If so, use proper string-to-char-array copy instead of string pointer assignment. */
        int rec_arr_lower = 0, rec_arr_upper = -1, rec_arr_is_short = 0;
        int rec_field_is_char_array = codegen_get_char_array_bounds(var_expr, ctx,
            &rec_arr_lower, &rec_arr_upper, &rec_arr_is_short);
        if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL && var_expr->type == EXPR_RECORD_ACCESS)
        {
            const char *fid = var_expr->expr_data.record_access_data.field_id;
            fprintf(stderr, "[codegen] RECORD char_array_check: field=%s found=%d lower=%d upper=%d is_short=%d\n",
                fid ? fid : "<null>", rec_field_is_char_array, rec_arr_lower, rec_arr_upper, rec_arr_is_short);
        }
        if (rec_field_is_char_array && (expr_get_type_tag(assign_expr) == STRING_TYPE ||
            expr_get_type_tag(assign_expr) == SHORTSTRING_TYPE ||
            codegen_expr_is_shortstring_value_local(assign_expr)))
        {
            int arr_size = rec_arr_upper - rec_arr_lower + 1;
            if (arr_size < 0) arr_size = 0;
            if (rec_arr_is_short)
            {
                int short_capacity = codegen_get_shortstring_capacity(var_expr, ctx);
                if (short_capacity > 1)
                    arr_size = short_capacity;
                if (assign_expr != NULL && assign_expr->type == EXPR_STRING)
                    inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reload, value_reg, arr_size);
                else if (codegen_expr_is_shortstring_value_local(assign_expr))
                {
                    inst_list = codegen_call_shortstring_copy(inst_list, ctx, addr_reload, arr_size, value_reg);
                }
                else
                    inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reload, value_reg, arr_size);
            }
            else
            {
                if (codegen_expr_is_shortstring_value_local(assign_expr))
                    inst_list = codegen_call_shortstring_to_char_array(inst_list, ctx, addr_reload, value_reg, arr_size);
                else
                    inst_list = codegen_call_string_to_char_array(inst_list, ctx, addr_reload, value_reg, arr_size);
            }
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reload);
            return inst_list;
        }

        if ((var_type_2 == SHORTSTRING_TYPE || expr_has_type_tag(var_expr, SHORTSTRING_TYPE)) &&
            (expr_get_type_tag(assign_expr) == STRING_TYPE ||
             expr_get_type_tag(assign_expr) == SHORTSTRING_TYPE ||
             codegen_expr_is_shortstring_value_local(assign_expr)))
        {
            int array_size = codegen_get_shortstring_capacity(var_expr, ctx);
            if (array_size <= 1)
                array_size = 256;

            if (assign_expr != NULL && assign_expr->type == EXPR_STRING)
            {
                inst_list = codegen_call_string_to_shortstring(inst_list, ctx,
                    addr_reload, value_reg, array_size);
            }
            else if (codegen_expr_is_shortstring_value_local(assign_expr))
            {
                inst_list = codegen_call_shortstring_copy(inst_list, ctx, addr_reload, array_size, value_reg);
            }
            else
            {
                inst_list = codegen_call_string_to_shortstring(inst_list, ctx,
                    addr_reload, value_reg, array_size);
            }

            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reload);
            return inst_list;
        }

        if (var_type_2 == STRING_TYPE)
        {
            /* If assigning a char to a string field, promote it first */
            int assign_type_2 = assign_expr != NULL ? expr_get_type_tag(assign_expr) : -1;
            if (assign_type_2 == CHAR_TYPE)
            {
                inst_list = codegen_promote_char_reg_to_string(inst_list, value_reg);
            }
            inst_list = codegen_call_string_assign(inst_list, ctx, addr_reload, value_reg);
        }
        else if (use_qword)
        {
            int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
            /* Fallback: check expr type tag for pointer arithmetic / 64-bit ops */
            if (!value_is_qword && assign_expr != NULL)
            {
                int assign_tag = expr_get_type_tag(assign_expr);
                if (codegen_type_uses_qword(assign_tag))
                    value_is_qword = 1;
            }
            if (coerced_to_real)
                value_is_qword = 1;
            /* Check for procedural var calls with pointer return type */
            if (!value_is_qword && assign_expr != NULL &&
                assign_expr->type == EXPR_FUNCTION_CALL &&
                assign_expr->expr_data.function_call_data.is_procedural_var_call &&
                assign_expr->expr_data.function_call_data.call_kgpc_type != NULL)
            {
                KgpcType *call_type = assign_expr->expr_data.function_call_data.call_kgpc_type;
                if (call_type->kind == TYPE_KIND_PROCEDURE)
                {
                    KgpcType *ret_type = kgpc_type_get_return_type(call_type);
                    if (ret_type != NULL && kgpc_type_uses_qword(ret_type))
                        value_is_qword = 1;
                    else if (call_type->info.proc_info.return_type_id != NULL)
                    {
                        const char *ret_id = call_type->info.proc_info.return_type_id;
                        if (kgpc_type_id_uses_qword(ret_id, ctx->symtab))
                            value_is_qword = 1;
                    }
                }
            }
            if (!value_is_qword)
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
            snprintf(buffer, 50, "\tmovq\t%s, (%s)\n", value_reg->bit_64, addr_reload->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (var_type_2 == CHAR_TYPE)
            {
                const char *value_reg8 = register_name8(value_reg);
                if (value_reg8 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for character assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovb\t%s, (%s)\n", value_reg8, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else if (use_word)
            {
                const char *value_reg16 = codegen_register_name16(value_reg);
                if (value_reg16 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 16-bit register for record assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovw\t%s, (%s)\n", value_reg16, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                snprintf(buffer, 50, "\tmovl\t%s, (%s)\n", value_reg->bit_32, addr_reload->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
        }

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reload);
        return inst_list;
    }
    else if (var_expr->type == EXPR_POINTER_DEREF)
    {
        struct Expression *pointer_expr = var_expr->expr_data.pointer_deref_data.pointer_expr;
        if (pointer_expr == NULL)
        {
            codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
            return inst_list;
        }

        if (codegen_expr_is_shortstring_array(var_expr) &&
            codegen_expr_is_shortstring_rhs(assign_expr, ctx))
        {
            Register_t *dest_addr = NULL;
            Register_t *src_addr = NULL;

            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &dest_addr);
            if (codegen_had_error(ctx) || dest_addr == NULL)
            {
                if (dest_addr != NULL)
                    free_reg(get_reg_stack(), dest_addr);
                return inst_list;
            }

            inst_list = codegen_address_for_expr(assign_expr, inst_list, ctx, &src_addr);
            if (codegen_had_error(ctx) || src_addr == NULL)
            {
                free_reg(get_reg_stack(), dest_addr);
                if (src_addr != NULL)
                    free_reg(get_reg_stack(), src_addr);
                return inst_list;
            }

            {
                char arg_buf[128];
                const char *arg_reg0 = current_arg_reg64(0);
                const char *arg_reg1 = current_arg_reg64(1);
                const char *arg_reg2 = current_arg_reg64(2);
                Register_t *len_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                if (arg_reg0 == NULL || arg_reg1 == NULL || arg_reg2 == NULL || len_reg == NULL)
                {
                    free_reg(get_reg_stack(), dest_addr);
                    free_reg(get_reg_stack(), src_addr);
                    if (len_reg != NULL)
                        free_reg(get_reg_stack(), len_reg);
                    return codegen_fail_register(ctx, inst_list, NULL,
                        "ERROR: Unable to allocate registers for pointer shortstring copy.");
                }

                snprintf(arg_buf, sizeof(arg_buf), "\tmovq\t%s, %s\n", src_addr->bit_64, arg_reg0);
                inst_list = add_inst(inst_list, arg_buf);
                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_length");
                free_arg_regs();

                snprintf(arg_buf, sizeof(arg_buf), "\tmovq\t%%rax, %s\n", len_reg->bit_64);
                inst_list = add_inst(inst_list, arg_buf);
                snprintf(arg_buf, sizeof(arg_buf), "\tincq\t%s\n", len_reg->bit_64);
                inst_list = add_inst(inst_list, arg_buf);

                snprintf(arg_buf, sizeof(arg_buf), "\tmovq\t%s, %s\n", dest_addr->bit_64, arg_reg0);
                inst_list = add_inst(inst_list, arg_buf);
                snprintf(arg_buf, sizeof(arg_buf), "\tmovq\t%s, %s\n", src_addr->bit_64, arg_reg1);
                inst_list = add_inst(inst_list, arg_buf);
                snprintf(arg_buf, sizeof(arg_buf), "\tmovq\t%s, %s\n", len_reg->bit_64, arg_reg2);
                inst_list = add_inst(inst_list, arg_buf);
                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
                free_arg_regs();
                free_reg(get_reg_stack(), len_reg);
            }
            free_reg(get_reg_stack(), dest_addr);
            free_reg(get_reg_stack(), src_addr);
            return inst_list;
        }

        expr_node_t *pointer_tree = build_expr_tree(pointer_expr);
        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
            addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
        {
            free_expr_tree(pointer_tree);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for pointer assignment address.");
        }

        inst_list = gencode_expr_tree(pointer_tree, inst_list, ctx, addr_reg);
        free_expr_tree(pointer_tree);

        StackNode_t *addr_temp = add_l_t("pointer_addr");
        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_temp->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);

        inst_list = codegen_expr(assign_expr, inst_list, ctx);
        if (codegen_had_error(ctx))
            return inst_list;

        Register_t *value_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (value_reg == NULL)
            value_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (value_reg == NULL)
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for pointer value.");

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
            addr_reload = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for pointer store.");
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        int var_type_3 = expr_get_type_tag(var_expr);
        int coerced_to_real = 0;
        inst_list = codegen_maybe_convert_int_like_to_real(var_type_3, assign_expr,
            value_reg, inst_list, &coerced_to_real);
        long long pointer_target_size = expr_effective_size_bytes(var_expr);
        int use_word = (!codegen_type_uses_qword(var_type_3) && pointer_target_size == 2);
        if (var_type_3 == STRING_TYPE)
        {
            inst_list = codegen_call_string_assign(inst_list, ctx, addr_reload, value_reg);
        }
        else if (codegen_type_uses_qword(var_type_3))
        {
            int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
            /* Fallback: check expr type tag for pointer arithmetic / 64-bit ops */
            if (!value_is_qword && assign_expr != NULL)
            {
                int assign_tag = expr_get_type_tag(assign_expr);
                if (codegen_type_uses_qword(assign_tag))
                    value_is_qword = 1;
            }
            if (coerced_to_real)
                value_is_qword = 1;
            /* Check for procedural var calls with pointer return type */
            if (!value_is_qword && assign_expr != NULL &&
                assign_expr->type == EXPR_FUNCTION_CALL &&
                assign_expr->expr_data.function_call_data.is_procedural_var_call &&
                assign_expr->expr_data.function_call_data.call_kgpc_type != NULL)
            {
                KgpcType *call_type = assign_expr->expr_data.function_call_data.call_kgpc_type;
                if (call_type->kind == TYPE_KIND_PROCEDURE)
                {
                    KgpcType *ret_type = kgpc_type_get_return_type(call_type);
                    if (ret_type != NULL && kgpc_type_uses_qword(ret_type))
                        value_is_qword = 1;
                    else if (call_type->info.proc_info.return_type_id != NULL)
                    {
                        const char *ret_id = call_type->info.proc_info.return_type_id;
                        if (kgpc_type_id_uses_qword(ret_id, ctx->symtab))
                            value_is_qword = 1;
                    }
                }
            }
            if (!value_is_qword)
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
            snprintf(buffer, 50, "\tmovq\t%s, (%s)\n", value_reg->bit_64, addr_reload->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (var_type_3 == CHAR_TYPE)
            {
                const char *value_reg8 = register_name8(value_reg);
                if (value_reg8 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for character assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovb\t%s, (%s)\n", value_reg8, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                if (use_word)
                {
                    const char *value_reg16 = codegen_register_name16(value_reg);
                    if (value_reg16 == NULL)
                    {
                        codegen_report_error(ctx,
                            "ERROR: Unable to select 16-bit register for pointer assignment.");
                    }
                    else
                    {
                        snprintf(buffer, 50, "\tmovw\t%s, (%s)\n", value_reg16, addr_reload->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }
                else
                {
                    snprintf(buffer, 50, "\tmovl\t%s, (%s)\n", value_reg->bit_32, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
        }

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reload);
        return inst_list;
    }
    else
    {
        assert(0 && "Unsupported assignment target");
        return inst_list;
    }
}

static struct Expression *codegen_build_temp_call_expr_from_stmt(
    const struct Statement *stmt, int call_hash_type, struct KgpcType *call_kgpc_type)
{
    if (stmt == NULL || stmt->type != STMT_PROCEDURE_CALL)
        return NULL;

    struct Expression *call_expr = mk_functioncall(stmt->line_num,
        stmt->stmt_data.procedure_call_data.id != NULL ?
            strdup(stmt->stmt_data.procedure_call_data.id) : NULL,
        stmt->stmt_data.procedure_call_data.expr_args);
    if (call_expr == NULL)
        return NULL;

    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
    {
        call_expr->expr_data.function_call_data.mangled_id =
            strdup(stmt->stmt_data.procedure_call_data.mangled_id);
    }
    call_expr->expr_data.function_call_data.arg0_is_dynarray_descriptor =
        stmt->stmt_data.procedure_call_data.arg0_is_dynarray_descriptor;
    call_expr->expr_data.function_call_data.call_hash_type = call_hash_type;
    call_expr->expr_data.function_call_data.call_kgpc_type = call_kgpc_type;
    call_expr->expr_data.function_call_data.is_call_info_valid =
        stmt->stmt_data.procedure_call_data.is_call_info_valid;
    call_expr->expr_data.function_call_data.is_virtual_call =
        stmt->stmt_data.procedure_call_data.is_virtual_call;
    call_expr->expr_data.function_call_data.is_interface_call =
        stmt->stmt_data.procedure_call_data.is_interface_call;
    call_expr->expr_data.function_call_data.vmt_index =
        stmt->stmt_data.procedure_call_data.vmt_index;
    if (stmt->stmt_data.procedure_call_data.self_class_name != NULL)
    {
        call_expr->expr_data.function_call_data.self_class_name =
            strdup(stmt->stmt_data.procedure_call_data.self_class_name);
    }
    call_expr->expr_data.function_call_data.is_class_method_call =
        stmt->stmt_data.procedure_call_data.is_class_method_call;
    return call_expr;
}

static void codegen_destroy_temp_call_expr(struct Expression *call_expr)
{
    if (call_expr == NULL)
        return;

    call_expr->expr_data.function_call_data.args_expr = NULL;
    call_expr->expr_data.function_call_data.call_kgpc_type = NULL;
    call_expr->expr_data.function_call_data.resolved_func = NULL;
    destroy_expr(call_expr);
}

/* Code generation for a procedure call */
