static ListNode_t *codegen_builtin_setlength(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    assert(stmt != NULL);
    assert(ctx != NULL);

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL)
    {
        fprintf(stderr, "ERROR: SetLength expects two arguments.\n");
        return inst_list;
    }

    const char *mangled = stmt->stmt_data.procedure_call_data.mangled_id;
    if (mangled != NULL && strcmp(mangled, "__kgpc_setlength_string") == 0)
        return codegen_builtin_setlength_string(stmt, inst_list, ctx);
    if (mangled != NULL && strcmp(mangled, "__kgpc_setlength_unicodestring") == 0)
        return codegen_builtin_setlength_unicodestring(stmt, inst_list, ctx);
    if (mangled != NULL && strcmp(mangled, "__kgpc_setlength_shortstring") == 0)
        return codegen_builtin_setlength_shortstring(stmt, inst_list, ctx);

    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    struct Expression *len_expr = (struct Expression *)args_expr->next->cur;

    struct Expression *array_expr = target_expr;
    const char *array_id = NULL;

    /* Handle both simple variables and field access (e.g., self.FItems) */
    if (array_expr != NULL && array_expr->type == EXPR_VAR_ID)
    {
        array_id = array_expr->expr_data.id;
    }
    else if (array_expr != NULL && array_expr->type == EXPR_RECORD_ACCESS)
    {
        /* For field access like self.FItems, use the field name */
        array_id = array_expr->expr_data.record_access_data.field_id;
    }
    else if (array_expr != NULL && array_expr->type == EXPR_ARRAY_ACCESS)
    {
        /* Nested dynamic array: SetLength(arr[i], n) where arr[i] is itself a dynamic array */
        array_id = "__nested_dynarray__";
    }
    else if (array_expr != NULL && codegen_expr_is_addressable(array_expr))
    {
        /* Accept addressable expressions such as SetLength(p^, n). */
        array_id = "__addressable_dynarray__";
    }

    if (array_id == NULL)
    {
        fprintf(stderr, "ERROR: SetLength first argument must be a variable or field identifier.\n");
        return inst_list;
    }

    int setlength_scope_depth = 0;
    StackNode_t *array_node = find_label_with_depth((char *)array_id, &setlength_scope_depth);

    /* If "Result" not found, try the current function name (function return variable) */
    if (array_node == NULL && pascal_identifier_equals(array_id, "Result") &&
        ctx != NULL && ctx->current_subprogram_id != NULL)
    {
        array_node = find_label_with_depth((char *)ctx->current_subprogram_id, &setlength_scope_depth);
    }

    int is_field_array = 0;
    int is_nested_array = 0;
    int use_expr_address = 0;

    /* If not found in local stack, might be a field of the current object */
    if (array_node == NULL && array_expr->type == EXPR_RECORD_ACCESS)
    {
        is_field_array = 1;
    }
    /* Nested dynamic array: arr[i] where arr[i] is itself a dynamic array */
    if (array_node == NULL && array_expr->type == EXPR_ARRAY_ACCESS)
    {
        is_nested_array = 1;
    }

    if (!is_field_array && !is_nested_array)
    {
        if (array_node == NULL)
        {
            use_expr_address = 1;
        }
        else if (!array_node->is_dynamic)
        {
            array_node->is_array = 1;
            array_node->is_dynamic = 1;
        }
    }

    int element_size;
    if (is_nested_array)
    {
        /* For nested dynamic arrays (array of array of ...), get element size from KgpcType */
        element_size = 8; /* Default: inner elements are pointers (dynamic arrays) */
        KgpcType *arr_type = expr_get_kgpc_type(array_expr);
        if (arr_type != NULL && kgpc_type_is_array(arr_type))
        {
            KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(arr_type, ctx->symtab);
            if (elem_type != NULL)
            {
                long long sz = kgpc_type_sizeof(elem_type);
                if (sz > 0)
                    element_size = (int)sz;
            }
        }
    }
    else if (is_field_array)
    {
        struct RecordField *field = codegen_lookup_record_field_expr(array_expr, ctx);
        element_size = -1;
        if (field != NULL && field->is_array)
        {
            long long elem_size = 0;
            if ((field->array_element_type != UNKNOWN_TYPE ||
                    field->array_element_type_id != NULL ||
                    field->array_element_record != NULL) &&
                codegen_sizeof_type_reference(ctx, field->array_element_type,
                    field->array_element_type_id, field->array_element_record, &elem_size) == 0 &&
                elem_size > 0)
            {
                element_size = (int)elem_size;
            }
        }
        if (array_expr != NULL)
        {
            long long fallback_size = expr_get_array_element_size(array_expr, ctx);
            if (fallback_size <= 0 && ctx != NULL && ctx->symtab != NULL)
            {
                KgpcType *array_type = expr_get_kgpc_type(array_expr);
                if (array_type != NULL && kgpc_type_is_array(array_type))
                {
                    KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(array_type, ctx->symtab);
                    if (elem_type != NULL)
                        fallback_size = kgpc_type_sizeof(elem_type);
                }
            }
            /* With-stack lookup: for unresolved variables in `with` blocks */
            if (fallback_size <= 0 && ctx != NULL && ctx->with_depth > 0 &&
                array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL)
            {
                struct RecordField *with_field = codegen_lookup_with_field(ctx,
                    array_expr->expr_data.id, NULL);
                if (with_field != NULL)
                {
                    long long elem_size = codegen_array_elem_size_from_field(with_field, ctx);
                    if (elem_size > 0)
                        fallback_size = elem_size;
                }
            }
            if (fallback_size <= 0)
            {
                codegen_report_error(ctx,
                    "ERROR: array expression is missing element-size metadata in SetLength.");
                return inst_list;
            }
            if (fallback_size <= INT_MAX && (element_size <= 0 || fallback_size > element_size))
                element_size = (int)fallback_size;
        }
        if (element_size <= 0)
        {
            codegen_report_error(ctx,
                "ERROR: unable to resolve SetLength field-array element size (field=%s).",
                (field != NULL && field->name != NULL) ? field->name : "<unknown>");
            return inst_list;
        }
    }
    else
    {
        element_size = codegen_dynamic_array_element_size(ctx, array_node, array_expr);
    }

    inst_list = codegen_expr(len_expr, inst_list, ctx);
    if (codegen_had_error(ctx))
        return inst_list;
    Register_t *length_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (length_reg == NULL)
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate register for SetLength length.");

    Register_t *descriptor_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (descriptor_reg == NULL)
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate register for SetLength descriptor.");

    char buffer[128];
    if (use_expr_address)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(array_expr, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
            addr_reg->bit_64, descriptor_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
    }
    else if (is_nested_array)
    {
        /* For nested dynamic arrays, compute the address of the inner array element.
         * E.g. SetLength(arr[i], n) where arr[i] is a dynamic array pointer.
         * We need the address of that pointer (not its value). */
        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(array_expr, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
            addr_reg->bit_64, descriptor_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
    }
    else if (is_field_array)
    {
        /* For field arrays, compute the field address from the base record expression. */
        Register_t *field_addr_reg = NULL;
        inst_list = codegen_record_field_address(array_expr, inst_list, ctx, &field_addr_reg);
        if (codegen_had_error(ctx) || field_addr_reg == NULL)
            return inst_list;
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
            field_addr_reg->bit_64, descriptor_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), field_addr_reg);
    }
    else if (array_node->is_static)
    {
        const char *label = (array_node->static_label != NULL) ?
            array_node->static_label : array_node->label;
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
            label, descriptor_reg->bit_64);
    }
    else if (setlength_scope_depth > 0)
    {
        /* Non-local variable: access via static link */
        Register_t *frame_reg = codegen_acquire_static_link(ctx, &inst_list, setlength_scope_depth);
        if (frame_reg == NULL)
        {
            codegen_report_error(ctx,
                "ERROR: Failed to acquire static link for SetLength variable %s.", array_id);
            free_reg(get_reg_stack(), descriptor_reg);
            free_reg(get_reg_stack(), length_reg);
            free_arg_regs();
            return inst_list;
        }
        if (array_node->is_reference)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%s), %s\n",
                array_node->offset, frame_reg->bit_64, descriptor_reg->bit_64);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%s), %s\n",
                array_node->offset, frame_reg->bit_64, descriptor_reg->bit_64);
        }
    }
    else
    {
        if (array_node->is_reference)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                array_node->offset, descriptor_reg->bit_64);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                array_node->offset, descriptor_reg->bit_64);
        }
    }
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32, length_reg->bit_64);

    if (codegen_target_is_windows())
    {
        const char *arg0 = current_arg_reg64(0);  /* %rcx */
        const char *arg1 = current_arg_reg64(1);  /* %rdx */
        RegisterId_t arg0_id = codegen_arg_reg_id_num(0);
        RegisterId_t arg1_id = codegen_arg_reg_id_num(1);
        
        /* Check if we need to swap or save/restore to avoid clobbering */
        int descriptor_is_arg1 = (descriptor_reg->reg_id == arg1_id);
        int length_is_arg0 = (length_reg->reg_id == arg0_id);
        
        if (descriptor_is_arg1 && length_is_arg0)
        {
            /* Simple swap: descriptor in %rdx, length in %rcx */
            /* We want: descriptor in %rcx, length in %rdx */
            /* Use xchg or save to stack */
            StackNode_t *temp = add_l_t("setlength_temp");
            if (temp != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", descriptor_reg->bit_64, temp->offset);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", temp->offset, arg0);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else if (descriptor_is_arg1)
        {
            /* descriptor in %rdx, need it in %rcx */
            /* Move descriptor first to avoid clobbering */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", descriptor_reg->bit_64, arg0);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            /* Normal case or length in %rdx */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", descriptor_reg->bit_64, arg0);
            inst_list = add_inst(inst_list, buffer);
        }
        
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", element_size, current_arg_reg32(2));
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        const char *arg0 = current_arg_reg64(0);  /* %rdi */
        const char *arg1 = current_arg_reg64(1);  /* %rsi */
        
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", descriptor_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", element_size, current_arg_reg32(2));
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_dynarray_setlength");

    free_reg(get_reg_stack(), descriptor_reg);
    free_reg(get_reg_stack(), length_reg);

    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_setlength_string(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    assert(stmt != NULL);
    if (ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL)
    {
        fprintf(stderr, "ERROR: SetLength expects two arguments.\n");
        return inst_list;
    }

    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    struct Expression *len_expr = (struct Expression *)args_expr->next->cur;
    if (target_expr == NULL || len_expr == NULL)
        return inst_list;

    if (!codegen_expr_is_addressable(target_expr))
    {
        fprintf(stderr, "ERROR: SetLength string target must be addressable.\n");
        return inst_list;
    }

    /* Check if length expression contains a function call.
     * If so, we need to spill addr_reg to the stack because
     * the function call will clobber caller-saved registers. */
    int len_has_function_call = expr_contains_function_call(len_expr);
    StackNode_t *addr_spill_slot = NULL;
    char buffer[128];

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    /* Spill addr_reg to stack if len_expr contains a function call */
    if (len_has_function_call)
    {
        addr_spill_slot = add_l_t("setlength_addr_spill");
        if (addr_spill_slot != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                addr_reg->bit_64, addr_spill_slot->offset);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    Register_t *length_reg = NULL;
    inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);
    if (codegen_had_error(ctx) || length_reg == NULL)
    {
        if (length_reg != NULL)
            free_reg(get_reg_stack(), length_reg);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    /* Reload addr_reg from spill slot if we spilled it */
    if (addr_spill_slot != NULL)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
            addr_spill_slot->offset, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    if (!expr_uses_qword_kgpctype(len_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32, length_reg->bit_64);

    const char *arg0 = current_arg_reg64(0);  /* First argument: %rcx (Win) / %rdi (SysV) */
    const char *arg1 = current_arg_reg64(1);  /* Second argument: %rdx (Win) / %rsi (SysV) */
    RegisterId_t arg0_id = codegen_arg_reg_id_num(0);
    RegisterId_t arg1_id = codegen_arg_reg_id_num(1);
    
    /*
     * Handle register conflicts when setting up function arguments.
     * If length_reg is in arg0's position and we try to move addr_reg to arg0,
     * we'll overwrite the length. In this case, move length_reg to arg1 first.
     */
    if (length_reg->reg_id == arg0_id)
    {
        /* length_reg is in arg0, which will be overwritten by addr_reg. Move length first. */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
    }
    else if (addr_reg->reg_id == arg1_id)
    {
        /* addr_reg is in arg1, which is the destination for length_reg. Move addr first. */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        /* No conflict, move in standard order */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_setlength");

    free_reg(get_reg_stack(), addr_reg);
    free_reg(get_reg_stack(), length_reg);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_setlength_unicodestring(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx)
{
    assert(stmt != NULL);
    if (ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL)
    {
        fprintf(stderr, "ERROR: SetLength expects two arguments.\n");
        return inst_list;
    }

    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    struct Expression *len_expr = (struct Expression *)args_expr->next->cur;
    if (target_expr == NULL || len_expr == NULL)
        return inst_list;

    if (!codegen_expr_is_addressable(target_expr))
    {
        fprintf(stderr, "ERROR: SetLength UnicodeString target must be addressable.\n");
        return inst_list;
    }

    int len_has_function_call = expr_contains_function_call(len_expr);
    StackNode_t *addr_spill_slot = NULL;
    char buffer[128];

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    if (len_has_function_call)
    {
        addr_spill_slot = add_l_t("setlength_unicode_addr_spill");
        if (addr_spill_slot != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                addr_reg->bit_64, addr_spill_slot->offset);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    Register_t *length_reg = NULL;
    inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);
    if (codegen_had_error(ctx) || length_reg == NULL)
    {
        if (length_reg != NULL)
            free_reg(get_reg_stack(), length_reg);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    if (addr_spill_slot != NULL)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
            addr_spill_slot->offset, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    if (!expr_uses_qword_kgpctype(len_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32, length_reg->bit_64);

    const char *arg0 = current_arg_reg64(0);
    const char *arg1 = current_arg_reg64(1);
    RegisterId_t arg0_id = codegen_arg_reg_id_num(0);
    RegisterId_t arg1_id = codegen_arg_reg_id_num(1);

    if (length_reg->reg_id == arg0_id)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
    }
    else if (addr_reg->reg_id == arg1_id)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_unicodestring_setlength");

    free_reg(get_reg_stack(), addr_reg);
    free_reg(get_reg_stack(), length_reg);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_setlength_shortstring(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    assert(stmt != NULL);
    if (ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL)
    {
        fprintf(stderr, "ERROR: SetLength expects two arguments.\n");
        return inst_list;
    }

    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    struct Expression *len_expr = (struct Expression *)args_expr->next->cur;
    if (target_expr == NULL || len_expr == NULL)
        return inst_list;

    if (!codegen_expr_is_addressable(target_expr))
    {
        fprintf(stderr, "ERROR: SetLength shortstring target must be addressable.\n");
        return inst_list;
    }

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    Register_t *length_reg = NULL;
    inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);
    if (codegen_had_error(ctx) || length_reg == NULL)
    {
        if (length_reg != NULL)
            free_reg(get_reg_stack(), length_reg);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    if (!expr_uses_qword_kgpctype(len_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32, length_reg->bit_64);

    char buffer[128];
    const char *arg0 = current_arg_reg64(0);
    const char *arg1 = current_arg_reg64(1);
    RegisterId_t arg0_id = codegen_arg_reg_id_num(0);
    RegisterId_t arg1_id = codegen_arg_reg_id_num(1);

    if (length_reg->reg_id == arg0_id)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
    }
    else if (addr_reg->reg_id == arg1_id)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg1);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_setlength");

    free_reg(get_reg_stack(), addr_reg);
    free_reg(get_reg_stack(), length_reg);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_setstring(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    assert(stmt != NULL);
    if (ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL || args_expr->next->next == NULL)
    {
        fprintf(stderr, "ERROR: SetString expects three arguments.\n");
        return inst_list;
    }

    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    struct Expression *buffer_expr = (struct Expression *)args_expr->next->cur;
    struct Expression *len_expr = (struct Expression *)args_expr->next->next->cur;
    if (target_expr == NULL || buffer_expr == NULL || len_expr == NULL)
        return inst_list;

    if (!codegen_expr_is_addressable(target_expr))
    {
        codegen_report_error(ctx, "ERROR: SetString target must be addressable.");
        return inst_list;
    }

    /* Get address of target string variable */
    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    /* Spill addr_reg to avoid clobbering by nested evaluations */
    StackNode_t *addr_slot = codegen_alloc_temp_slot("setstring_addr");
    if (addr_slot == NULL)
    {
        free_reg(get_reg_stack(), addr_reg);
        codegen_report_error(ctx, "ERROR: Unable to allocate spill slot for SetString target.");
        return inst_list;
    }
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_slot->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), addr_reg);
    addr_reg = NULL;

    /* Get buffer pointer value */
    Register_t *buffer_reg = NULL;
    inst_list = codegen_expr_with_result(buffer_expr, inst_list, ctx, &buffer_reg);
    if (codegen_had_error(ctx) || buffer_reg == NULL)
    {
        if (buffer_reg != NULL)
            free_reg(get_reg_stack(), buffer_reg);
        return inst_list;
    }

    /* If buffer is a typecast of a dynamic array to a pointer (e.g. PUnicodeChar(Chars)),
     * load the data pointer from the dynarray descriptor. */
    int buffer_needs_dynarray_data = 0;
    if (buffer_expr != NULL && buffer_expr->type == EXPR_TYPECAST &&
        buffer_expr->expr_data.typecast_data.expr != NULL)
    {
        struct Expression *inner = buffer_expr->expr_data.typecast_data.expr;
        KgpcType *inner_type = expr_get_kgpc_type(inner);
        if (inner_type != NULL && kgpc_type_is_dynamic_array(inner_type))
            buffer_needs_dynarray_data = 1;
    }
    if (buffer_needs_dynarray_data)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                 buffer_reg->bit_64, buffer_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    /* Spill buffer_reg */
    StackNode_t *buffer_slot = codegen_alloc_temp_slot("setstring_buf");
    if (buffer_slot == NULL)
    {
        free_reg(get_reg_stack(), buffer_reg);
        codegen_report_error(ctx, "ERROR: Unable to allocate spill slot for SetString buffer.");
        return inst_list;
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", buffer_reg->bit_64, buffer_slot->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), buffer_reg);
    buffer_reg = NULL;

    /* Get length value */
    Register_t *length_reg = NULL;
    inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);
    if (codegen_had_error(ctx) || length_reg == NULL)
    {
        if (length_reg != NULL)
            free_reg(get_reg_stack(), length_reg);
        return inst_list;
    }

    /* Sign-extend length to 64-bit if needed */
    if (!expr_uses_qword_kgpctype(len_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32, length_reg->bit_64);

    const char *call_target = "kgpc_setstring";
    if (target_expr != NULL && target_expr->resolved_kgpc_type == NULL &&
        target_expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *sym_node = NULL;
        if (FindSymbol(&sym_node, ctx->symtab, target_expr->expr_data.id) != 0 &&
            sym_node != NULL && sym_node->type != NULL)
        {
            target_expr->resolved_kgpc_type = sym_node->type;
        }
    }
    if (target_expr != NULL && target_expr->resolved_kgpc_type != NULL)
    {
        int is_wide = kgpc_type_is_wide_string(target_expr->resolved_kgpc_type);
        if (!is_wide && target_expr->resolved_kgpc_type->type_alias != NULL)
        {
            const char *alias_name = target_expr->resolved_kgpc_type->type_alias->alias_name;
            const char *target_name = target_expr->resolved_kgpc_type->type_alias->target_type_id;
            if ((alias_name != NULL &&
                 (pascal_identifier_equals(alias_name, "UnicodeString") ||
                  pascal_identifier_equals(alias_name, "WideString"))) ||
                (target_name != NULL &&
                 (pascal_identifier_equals(target_name, "UnicodeString") ||
                  pascal_identifier_equals(target_name, "WideString"))))
            {
                is_wide = 1;
            }
        }
        if (!is_wide && target_expr->type == EXPR_VAR_ID &&
            target_expr->expr_data.id != NULL &&
            pascal_identifier_equals(target_expr->expr_data.id, "Result") &&
            ctx != NULL && ctx->symtab != NULL)
        {
            const char *sub_id = ctx->current_subprogram_mangled;
            if (sub_id == NULL || sub_id[0] == '\0')
                sub_id = ctx->current_subprogram_id;
            if (sub_id != NULL)
            {
                HashNode_t *sub_node = NULL;
                if (FindSymbol(&sub_node, ctx->symtab, sub_id) != 0 &&
                    sub_node != NULL && sub_node->type != NULL &&
                    kgpc_type_is_procedure(sub_node->type))
                {
                    const char *ret_id = sub_node->type->info.proc_info.return_type_id;
                    if (ret_id != NULL &&
                        (pascal_identifier_equals(ret_id, "WideString") ||
                         pascal_identifier_equals(ret_id, "UnicodeString")))
                    {
                        is_wide = 1;
                    }
                }
            }
        }
        if (is_wide)
            call_target = "kgpc_setstring_unicode";
    }
    const char *mangled = stmt->stmt_data.procedure_call_data.mangled_id;
    if (mangled != NULL && strcmp(mangled, "kgpc_shortstring_setstring") == 0)
        call_target = "kgpc_shortstring_setstring";

    /* Set up arguments for kgpc_setstring/shortstring_setstring */
    const char *arg0 = current_arg_reg64(0);  /* %rdi or %rcx */
    const char *arg1 = current_arg_reg64(1);  /* %rsi or %rdx */
    const char *arg2 = current_arg_reg64(2);  /* %rdx or %r8 */

    /* Move length to arg2 first (it's in a register) */
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", length_reg->bit_64, arg2);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), length_reg);

    /* Reload buffer and addr from spill slots */
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", buffer_slot->offset, arg1);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", addr_slot->offset, arg0);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_vect_reg(inst_list, 0);
    {
        char call_buffer[128];
        snprintf(call_buffer, sizeof(call_buffer), "\tcall\t%s\n", call_target);
        inst_list = add_inst(inst_list, call_buffer);
    }

    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_str(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL)
    {
        fprintf(stderr, "ERROR: Str expects two arguments.\n");
        return inst_list;
    }

    struct Expression *value_expr = (struct Expression *)args_expr->cur;
    struct Expression *target_expr = (struct Expression *)args_expr->next->cur;

    /* Check if target is ShortString */
    int target_is_shortstring = codegen_expr_is_shortstring_array(target_expr);
    int target_shortstring_capacity = 256;
    if (target_is_shortstring)
        target_shortstring_capacity = codegen_get_shortstring_capacity(target_expr, ctx);

    Register_t *value_reg = NULL;
    inst_list = codegen_expr_with_result(value_expr, inst_list, ctx, &value_reg);
    if (codegen_had_error(ctx) || value_reg == NULL)
        return inst_list;

    int value_is_real = (value_expr != NULL && expr_has_type_tag(value_expr, REAL_TYPE));
    int has_width = (value_expr != NULL && value_expr->field_width != NULL);
    int has_precision = (value_expr != NULL && value_expr->field_precision != NULL);
    Register_t *width_reg = NULL;
    Register_t *precision_reg = NULL;

    if (has_width)
    {
        inst_list = codegen_expr_with_result(value_expr->field_width, inst_list, ctx, &width_reg);
        if (codegen_had_error(ctx) || width_reg == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }
        if (!expr_uses_qword_kgpctype(value_expr->field_width))
            inst_list = codegen_sign_extend32_to64(inst_list, width_reg->bit_32, width_reg->bit_64);
    }
    if (has_precision)
    {
        inst_list = codegen_expr_with_result(value_expr->field_precision, inst_list, ctx, &precision_reg);
        if (codegen_had_error(ctx) || precision_reg == NULL)
        {
            if (width_reg != NULL)
                free_reg(get_reg_stack(), width_reg);
            free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }
        if (!expr_uses_qword_kgpctype(value_expr->field_precision))
            inst_list = codegen_sign_extend32_to64(inst_list, precision_reg->bit_32, precision_reg->bit_64);
    }
    if (!value_is_real && !expr_uses_qword_kgpctype(value_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);

    if (!codegen_expr_is_addressable(target_expr))
    {
        codegen_report_error(ctx, "ERROR: Str output must be addressable.");
        free_reg(get_reg_stack(), value_reg);
        return inst_list;
    }

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
    {
        free_reg(get_reg_stack(), value_reg);
        return inst_list;
    }

    char buffer[128];
    const char *shortstring_suffix = target_is_shortstring ? "_shortstring" : "";
    
    if (value_is_real)
    {
        int value_is_unknown_byref = 0;
        if (value_expr != NULL && value_expr->type == EXPR_VAR_ID &&
            value_expr->expr_data.id != NULL && expr_get_type_tag(value_expr) == UNKNOWN_TYPE &&
            ctx->symtab != NULL)
        {
            HashNode_t *value_sym = NULL;
            if (FindSymbol(&value_sym, ctx->symtab, value_expr->expr_data.id) != 0 &&
                value_sym != NULL && value_sym->is_var_parameter)
            {
                value_is_unknown_byref = 1;
            }
        }

        if (value_is_unknown_byref)
        {
            Register_t *value_addr_reg = NULL;
            inst_list = codegen_address_for_expr(value_expr, inst_list, ctx, &value_addr_reg);
            if (codegen_had_error(ctx) || value_addr_reg == NULL)
            {
                free_reg(get_reg_stack(), addr_reg);
                free_reg(get_reg_stack(), value_reg);
                if (width_reg != NULL)
                    free_reg(get_reg_stack(), width_reg);
                if (precision_reg != NULL)
                    free_reg(get_reg_stack(), precision_reg);
                return inst_list;
            }
            if (codegen_target_is_windows())
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", value_addr_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", value_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_load_extended_to_bits");
            free_arg_regs();
            free_reg(get_reg_stack(), value_addr_reg);
            inst_list = add_inst(inst_list, "\tmovq\t%rax, %xmm0\n");
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        if (has_width || has_precision)
        {
            if (width_reg == NULL)
            {
                width_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (width_reg == NULL)
                {
                    free_reg(get_reg_stack(), addr_reg);
                    free_reg(get_reg_stack(), value_reg);
                    if (precision_reg != NULL)
                        free_reg(get_reg_stack(), precision_reg);
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t$0, %s\n", width_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            if (precision_reg == NULL)
            {
                precision_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (precision_reg == NULL)
                {
                    free_reg(get_reg_stack(), addr_reg);
                    free_reg(get_reg_stack(), value_reg);
                    free_reg(get_reg_stack(), width_reg);
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", precision_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            if (codegen_target_is_windows())
            {
                if (target_is_shortstring)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", width_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer),
                        "\tmovq\t$%d, %%r9\n\tshlq\t$32, %%r9\n\torq\t%s, %%r9\n",
                        target_shortstring_capacity, precision_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    /* Move addr_reg to R9 first to avoid clobbering if addr_reg is R8 */
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r9\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", width_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", precision_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                if (target_is_shortstring)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", width_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer),
                        "\tmovq\t$%d, %%rdx\n\tshlq\t$32, %%rdx\n\torq\t%s, %%rdx\n",
                        target_shortstring_capacity, precision_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    /* Move addr_reg to RDX first to avoid clobbering if addr_reg is RSI */
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", width_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", precision_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            inst_list = codegen_vect_reg(inst_list, 0);
            if (target_is_shortstring)
                snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_str_real_fmt_bounded_shortstring\n");
            else
                snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_str_real_fmt%s\n", shortstring_suffix);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (codegen_target_is_windows())
            {
                if (target_is_shortstring)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n",
                        target_shortstring_capacity);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                if (target_is_shortstring)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rsi\n",
                        target_shortstring_capacity);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            inst_list = codegen_vect_reg(inst_list, 0);
            if (target_is_shortstring)
                snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_str_real_bounded_shortstring\n");
            else
                snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_str_real%s\n", shortstring_suffix);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    else
    {
        if (has_width)
        {
            if (codegen_target_is_windows())
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", value_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", width_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                if (target_is_shortstring)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r9\n",
                        target_shortstring_capacity);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", value_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", width_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                if (target_is_shortstring)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rcx\n",
                        target_shortstring_capacity);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            inst_list = codegen_vect_reg(inst_list, 0);
            if (target_is_shortstring)
                snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_str_int64_fmt_bounded_shortstring\n");
            else
                snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_str_int64_fmt%s\n", shortstring_suffix);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (codegen_target_is_windows())
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", value_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                if (target_is_shortstring)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n",
                        target_shortstring_capacity);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", value_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                if (target_is_shortstring)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n",
                        target_shortstring_capacity);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }

            inst_list = codegen_vect_reg(inst_list, 0);
            if (target_is_shortstring)
                snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_str_int64_bounded_shortstring\n");
            else
                snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_str_int64%s\n", shortstring_suffix);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    if (precision_reg != NULL)
        free_reg(get_reg_stack(), precision_reg);
    if (width_reg != NULL)
        free_reg(get_reg_stack(), width_reg);
    free_reg(get_reg_stack(), addr_reg);
    free_reg(get_reg_stack(), value_reg);
    free_arg_regs();
    return inst_list;
}

/* WriteStr(var S: string; args...) - format values into string S */
static ListNode_t *codegen_builtin_writestr(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL)
    {
        fprintf(stderr, "ERROR: WriteStr requires at least one argument.\n");
        return inst_list;
    }

    /* First argument is the target string variable */
    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    if (!codegen_expr_is_addressable(target_expr))
    {
        codegen_report_error(ctx, "ERROR: WriteStr target must be addressable.");
        return inst_list;
    }

    /* Save target address to stack */
    Register_t *target_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &target_reg);
    if (codegen_had_error(ctx) || target_reg == NULL)
        return inst_list;

    StackNode_t *target_slot = add_l_x("__writestr_target__", CODEGEN_POINTER_SIZE_BYTES);
    if (target_slot == NULL)
    {
        free_reg(get_reg_stack(), target_reg);
        return inst_list;
    }
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", target_reg->bit_64, target_slot->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), target_reg);

    /* Create accumulator string slot initialized to empty string */
    StackNode_t *accum_slot = add_l_x("__writestr_accum__", CODEGEN_POINTER_SIZE_BYTES);
    if (accum_slot == NULL)
        return inst_list;

    /* Initialize accumulator to empty string - use correct ABI for the platform */
    if (codegen_target_is_windows())
    {
        inst_list = add_inst(inst_list, "\txorq\t%rcx, %rcx\n");
    }
    else
    {
        inst_list = add_inst(inst_list, "\txorq\t%rdi, %rdi\n");
    }
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_duplicate");
    free_arg_regs();
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", accum_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    /* Process remaining arguments */
    args_expr = args_expr->next;
    while (args_expr != NULL)
    {
        struct Expression *arg_expr = (struct Expression *)args_expr->cur;
        int arg_type = expr_get_type_tag(arg_expr);

        /* Generate code to format this argument to a string */
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        const char *format_func = NULL;
        if (is_integer_type(arg_type) || arg_type == ENUM_TYPE)
            format_func = "kgpc_int_to_str";
        else if (arg_type == STRING_TYPE)
            format_func = "kgpc_string_duplicate";  /* Just duplicate the string */
        else if (arg_type == CHAR_TYPE)
            format_func = "kgpc_char_to_str";
        else if (arg_type == BOOL)
            format_func = "kgpc_bool_to_str";
        else if (arg_type == REAL_TYPE)
            format_func = "kgpc_real_to_str";
        else
        {
            /* Default to int */
            format_func = "kgpc_int_to_str";
        }

        /* Call format function: result = format_func(value) */
        if (codegen_target_is_windows())
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", value_reg->bit_64);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", value_reg->bit_64);
        }
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), value_reg);

        inst_list = codegen_vect_reg(inst_list, 0);
        snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", format_func);
        inst_list = add_inst(inst_list, buffer);
        free_arg_regs();

        /* Save formatted string temporarily */
        StackNode_t *temp_slot = add_l_x("__writestr_temp__", CODEGEN_POINTER_SIZE_BYTES);
        if (temp_slot == NULL)
            return inst_list;
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", temp_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        /* Concatenate: accum = concat(accum, temp) */
        if (codegen_target_is_windows())
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rcx\n", accum_slot->offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n", temp_slot->offset);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdi\n", accum_slot->offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rsi\n", temp_slot->offset);
            inst_list = add_inst(inst_list, buffer);
        }
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_concat");
        free_arg_regs();

        /* Update accumulator */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", accum_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        args_expr = args_expr->next;
    }

    /* Store result in target: *target = accum */
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", accum_slot->offset);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%r11\n", target_slot->offset);
    inst_list = add_inst(inst_list, buffer);
    inst_list = add_inst(inst_list, "\tmovq\t%rax, (%r11)\n");

    return inst_list;
}

static ListNode_t *codegen_builtin_insert(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL || args_expr->next->next == NULL)
    {
        fprintf(stderr, "ERROR: Insert expects three arguments.\n");
        return inst_list;
    }

    char buffer[128];
    struct Expression *source_expr = (struct Expression *)args_expr->cur;
    struct Expression *target_expr = (struct Expression *)args_expr->next->cur;
    struct Expression *index_expr = (struct Expression *)args_expr->next->next->cur;

    int target_is_shortstring = codegen_expr_is_shortstring_array(target_expr);
    int source_is_shortstring = codegen_expr_is_shortstring_array(source_expr);

    Register_t *source_reg = NULL;
    int source_is_char = (source_expr != NULL && expr_get_type_tag(source_expr) == CHAR_TYPE);
    StackNode_t *char_buffer = NULL;
    if (target_is_shortstring && source_is_shortstring)
    {
        if (!codegen_expr_is_addressable(source_expr))
        {
            codegen_report_error(ctx, "ERROR: Insert shortstring source must be addressable.");
            return inst_list;
        }
        inst_list = codegen_address_for_expr(source_expr, inst_list, ctx, &source_reg);
        if (codegen_had_error(ctx) || source_reg == NULL)
            return inst_list;
    }
    else
    {
        inst_list = codegen_expr_with_result(source_expr, inst_list, ctx, &source_reg);
        if (codegen_had_error(ctx) || source_reg == NULL)
            return inst_list;
    }

    if (source_is_char)
    {
        char_buffer = add_l_x("insert_char_buffer", 2);
        if (char_buffer == NULL)
        {
            free_reg(get_reg_stack(), source_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate spill slot for Insert char source.");
        }

        const char *byte_reg = register_name8(source_reg);
        if (byte_reg == NULL)
        {
            free_reg(get_reg_stack(), source_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to acquire byte register for Insert char source.");
        }

        if (target_is_shortstring)
        {
            snprintf(buffer, sizeof(buffer), "\tmovb\t$1, -%d(%%rbp)\n", char_buffer->offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", byte_reg, char_buffer->offset - 1);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", char_buffer->offset, source_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovb\t$0, -%d(%%rbp)\n", char_buffer->offset - 1);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", byte_reg, char_buffer->offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", char_buffer->offset, source_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    StackNode_t *source_temp = add_l_t("insert_source");
    if (source_temp == NULL)
    {
        free_reg(get_reg_stack(), source_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for Insert source.");
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", source_reg->bit_64, source_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), source_reg);

    if (!codegen_expr_is_addressable(target_expr))
    {
        codegen_report_error(ctx, "ERROR: Insert target must be addressable.");
        return inst_list;
    }

    Register_t *target_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &target_reg);
    if (codegen_had_error(ctx) || target_reg == NULL)
        return inst_list;

    StackNode_t *target_temp = add_l_t("insert_target");
    if (target_temp == NULL)
    {
        free_reg(get_reg_stack(), target_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for Insert target.");
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", target_reg->bit_64, target_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), target_reg);

    Register_t *index_reg = NULL;
    inst_list = codegen_expr_with_result(index_expr, inst_list, ctx, &index_reg);
    if (codegen_had_error(ctx) || index_reg == NULL)
        return inst_list;

    if (!expr_uses_qword_kgpctype(index_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32, index_reg->bit_64);

    StackNode_t *index_temp = add_l_t("insert_index");
    if (index_temp == NULL)
    {
        free_reg(get_reg_stack(), index_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for Insert index.");
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", index_reg->bit_64, index_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), index_reg);

    const char *arg0 = current_arg_reg64(0);
    const char *arg1 = current_arg_reg64(1);
    const char *arg2 = current_arg_reg64(2);
    const char *arg3 = current_arg_reg64(3);

    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", source_temp->offset, arg0);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", target_temp->offset, arg1);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", index_temp->offset, arg2);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_vect_reg(inst_list, 0);
    if (target_is_shortstring)
    {
        int value_is_shortstring = (source_is_shortstring || source_is_char) ? 1 : 0;
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %s\n", value_is_shortstring, arg3);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_insert");
    }
    else
    {
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_insert");
    }
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_delete(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL || args_expr->next->next == NULL)
    {
        fprintf(stderr, "ERROR: Delete expects three arguments.\n");
        return inst_list;
    }

    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    struct Expression *index_expr = (struct Expression *)args_expr->next->cur;
    struct Expression *count_expr = (struct Expression *)args_expr->next->next->cur;

    int target_is_shortstring = codegen_expr_is_shortstring_array(target_expr);

    if (!codegen_expr_is_addressable(target_expr))
    {
        codegen_report_error(ctx, "ERROR: Delete target must be addressable.");
        return inst_list;
    }

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    StackNode_t *string_temp = add_l_t("delete_target");
    if (string_temp == NULL)
    {
        free_reg(get_reg_stack(), addr_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for Delete target.");
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, string_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), addr_reg);

    Register_t *index_reg = NULL;
    inst_list = codegen_expr_with_result(index_expr, inst_list, ctx, &index_reg);
    if (codegen_had_error(ctx) || index_reg == NULL)
        return inst_list;

    if (!expr_uses_qword_kgpctype(index_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32, index_reg->bit_64);

    StackNode_t *index_temp = add_l_t("delete_index");
    if (index_temp == NULL)
    {
        free_reg(get_reg_stack(), index_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for Delete index.");
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", index_reg->bit_64, index_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), index_reg);

    Register_t *count_reg = NULL;
    inst_list = codegen_expr_with_result(count_expr, inst_list, ctx, &count_reg);
    if (codegen_had_error(ctx) || count_reg == NULL)
        return inst_list;

    if (!expr_uses_qword_kgpctype(count_expr))
        inst_list = codegen_sign_extend32_to64(inst_list, count_reg->bit_32, count_reg->bit_64);

    StackNode_t *count_temp = add_l_t("delete_count");
    if (count_temp == NULL)
    {
        free_reg(get_reg_stack(), count_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate spill slot for Delete count.");
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", count_reg->bit_64, count_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), count_reg);

    const char *arg0 = current_arg_reg64(0);
    const char *arg1 = current_arg_reg64(1);
    const char *arg2 = current_arg_reg64(2);

    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", string_temp->offset, arg0);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", index_temp->offset, arg1);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", count_temp->offset, arg2);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_vect_reg(inst_list, 0);
    if (target_is_shortstring)
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_delete");
    else
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_delete");
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_val(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_count = ListLength(args_expr);
    if (args_expr == NULL || (arg_count != 2 && arg_count != 3))
    {
        codegen_report_error(ctx, "ERROR: Val expects two or three arguments.");
        return inst_list;
    }

    struct Expression *source_expr = (struct Expression *)args_expr->cur;
    struct Expression *value_expr = (struct Expression *)args_expr->next->cur;
    struct Expression *code_expr = (arg_count == 3) ? (struct Expression *)args_expr->next->next->cur : NULL;

    Register_t *source_reg = NULL;
    Register_t *value_addr = NULL;
    Register_t *code_addr = NULL;
    StackNode_t *code_spill = NULL;
    StackNode_t *code_result_spill = NULL;
    StackNode_t *value_result_spill = NULL;
    int value_type_tag = (value_expr != NULL) ? expr_get_type_tag(value_expr) : UNKNOWN_TYPE;
    int value_store_size = expr_integer_store_size(value_expr);

    int source_is_shortstring = codegen_expr_is_shortstring_array(source_expr);
    if (!source_is_shortstring && source_expr != NULL &&
        expr_get_type_tag(source_expr) == SHORTSTRING_TYPE)
        source_is_shortstring = 1;

    if (source_is_shortstring)
    {
        /* For ShortString sources, get the address instead of the value */
        inst_list = codegen_address_for_expr(source_expr, inst_list, ctx, &source_reg);
    }
    else
    {
        inst_list = codegen_expr_with_result(source_expr, inst_list, ctx, &source_reg);
    }
    if (codegen_had_error(ctx) || source_reg == NULL)
        goto cleanup;

    inst_list = codegen_address_for_expr(value_expr, inst_list, ctx, &value_addr);
    if (codegen_had_error(ctx) || value_addr == NULL)
        goto cleanup;

    if (code_expr != NULL)
    {
        inst_list = codegen_address_for_expr(code_expr, inst_list, ctx, &code_addr);
        if (codegen_had_error(ctx) || code_addr == NULL)
            goto cleanup;
    }

    const char *call_target = NULL;
    int target_is_extended = 0;
    if (value_expr != NULL)
    {
        KgpcType *value_kgpc = expr_get_kgpc_type(value_expr);
        target_is_extended = (value_type_tag == EXTENDED_TYPE) ||
            (value_kgpc != NULL && kgpc_type_is_extended(value_kgpc)) ||
            codegen_expr_involves_extended(value_expr);
    }
    switch (value_type_tag)
    {
        case BYTE_TYPE:
        case WORD_TYPE:
        case INT_TYPE:
        case LONGWORD_TYPE:
            call_target = source_is_shortstring ? "kgpc_val_integer_ss" : "kgpc_val_integer";
            break;
        case CHAR_TYPE:
        case BOOL:
        case LONGINT_TYPE:
        case INT64_TYPE:
            call_target = source_is_shortstring ? "kgpc_val_longint_ss" : "kgpc_val_longint";
            break;
        case QWORD_TYPE:
            call_target = source_is_shortstring ? "kgpc_val_qword_ss" : "kgpc_val_qword";
            break;
        case REAL_TYPE:
            call_target = source_is_shortstring
                ? (target_is_extended ? "kgpc_val_extended_ss" : "kgpc_val_real_ss")
                : (target_is_extended ? "kgpc_val_extended" : "kgpc_val_real");
            break;
        case EXTENDED_TYPE:
            call_target = source_is_shortstring ? "kgpc_val_extended_ss" : "kgpc_val_extended";
            break;
        default:
            call_target = NULL;
            break;
    }

    if (call_target == NULL)
    {
        codegen_report_error(ctx, "ERROR: Val target must be integer, longint, or real.");
        goto cleanup;
    }

    if (is_integer_type(value_type_tag) || value_type_tag == CHAR_TYPE || value_type_tag == BOOL)
    {
        value_result_spill = add_l_t("val_value_result");
        if (value_result_spill == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate temporary for Val target result.");
            goto cleanup;
        }
    }

    if (code_expr != NULL)
    {
        code_spill = add_l_t("val_code_ptr");
        if (code_spill == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate temporary for Val code argument.");
            goto cleanup;
        }

        char buffer[128];
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", code_addr->bit_64, code_spill->offset);
        inst_list = add_inst(inst_list, buffer);
    }

    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", source_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        if (value_result_spill != NULL)
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rdx\n", value_result_spill->offset);
        else
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_addr->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", source_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        if (value_result_spill != NULL)
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rsi\n", value_result_spill->offset);
        else
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_addr->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    inst_list = codegen_vect_reg(inst_list, 0);
    snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", call_target);
    inst_list = add_inst(inst_list, buffer);

    /* Save the return value (code) before the value copy block clobbers %rax */
    if (code_expr != NULL)
    {
        code_result_spill = add_l_t("val_code_result");
        if (code_result_spill == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate temporary for Val result.");
            goto cleanup;
        }

        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", code_result_spill->offset);
        inst_list = add_inst(inst_list, buffer);
    }

    if (value_result_spill != NULL)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rax\n", value_addr->bit_64);
        inst_list = add_inst(inst_list, buffer);

        switch (value_store_size)
        {
            case 1:
                snprintf(buffer, sizeof(buffer), "\tmovb\t-%d(%%rbp), %%dl\n", value_result_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                inst_list = add_inst(inst_list, "\tmovb\t%dl, (%rax)\n");
                break;
            case 2:
                snprintf(buffer, sizeof(buffer), "\tmovw\t-%d(%%rbp), %%dx\n", value_result_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                inst_list = add_inst(inst_list, "\tmovw\t%dx, (%rax)\n");
                break;
            case 8:
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n", value_result_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                inst_list = add_inst(inst_list, "\tmovq\t%rdx, (%rax)\n");
                break;
            case 4:
            default:
                snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %%edx\n", value_result_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                inst_list = add_inst(inst_list, "\tmovl\t%edx, (%rax)\n");
                break;
        }
    }

    if (code_expr != NULL && code_result_spill != NULL)
    {
        if (expr_uses_qword_kgpctype(code_expr))
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n", code_result_spill->offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", code_spill->offset);
            inst_list = add_inst(inst_list, buffer);
            inst_list = add_inst(inst_list, "\tmovq\t%rdx, (%rax)\n");
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %%edx\n", code_result_spill->offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", code_spill->offset);
            inst_list = add_inst(inst_list, buffer);
            inst_list = add_inst(inst_list, "\tmovl\t%edx, (%rax)\n");
        }
    }

cleanup:
    if (source_reg != NULL)
        free_reg(get_reg_stack(), source_reg);
    if (value_addr != NULL)
        free_reg(get_reg_stack(), value_addr);
    if (code_addr != NULL)
        free_reg(get_reg_stack(), code_addr);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_prefetch(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next != NULL || args->cur == NULL)
    {
        codegen_report_error(ctx, "ERROR: Prefetch expects exactly one argument.");
        return inst_list;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    char buffer[96];
    const char *arg0 = current_arg_reg64(0);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, arg0);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_prefetch");
    free_arg_regs();
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}

static ListNode_t *codegen_builtin_incdec(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx, int is_increment)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL)
        return inst_list;

    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    struct Expression *value_expr = (args_expr->next != NULL) ? (struct Expression *)args_expr->next->cur : NULL;

    Register_t *increment_reg = NULL;
    if (value_expr != NULL)
    {
        inst_list = codegen_expr_with_result(value_expr, inst_list, ctx, &increment_reg);
        if (codegen_had_error(ctx) || increment_reg == NULL)
            return inst_list;
    }
    else
    {
        increment_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (increment_reg == NULL)
            return inst_list;
        char buffer[64];
        if (target_expr != NULL && expr_uses_qword_kgpctype(target_expr))
            snprintf(buffer, sizeof(buffer), "\tmovq\t$1, %s\n", increment_reg->bit_64);
        else
            snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", increment_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }

    int target_type_tag = (target_expr != NULL) ? expr_get_type_tag(target_expr) : UNKNOWN_TYPE;
    int target_is_pointer = (target_type_tag == POINTER_TYPE);
    int target_uses_qword = target_is_pointer;
    if (!target_uses_qword && target_expr != NULL)
        target_uses_qword = expr_uses_qword_kgpctype(target_expr);

    if (target_uses_qword &&
        (value_expr == NULL || !expr_uses_qword_kgpctype(value_expr)))
        inst_list = codegen_sign_extend32_to64(inst_list, increment_reg->bit_32, increment_reg->bit_64);

    long long pointer_step = 1;
    if (target_is_pointer)
    {
        if (codegen_sizeof_pointer_target(ctx, target_expr, &pointer_step) != 0 || pointer_step <= 0)
        {
            pointer_step = 1;
        }
    }

    if (target_is_pointer && pointer_step != 1)
    {
        char buffer_scale[128];
        if (target_uses_qword)
            snprintf(buffer_scale, sizeof(buffer_scale), "\timulq\t$%lld, %s\n", pointer_step, increment_reg->bit_64);
        else
            snprintf(buffer_scale, sizeof(buffer_scale), "\timull\t$%lld, %s\n", pointer_step, increment_reg->bit_32);
        inst_list = add_inst(inst_list, buffer_scale);
    }

    char buffer_main[128];
    if (!is_increment)
    {
        if (target_uses_qword)
        {
            snprintf(buffer_main, sizeof(buffer_main), "\tnegq\t%s\n", increment_reg->bit_64);
            inst_list = add_inst(inst_list, buffer_main);
        }
        else
        {
            snprintf(buffer_main, sizeof(buffer_main), "\tnegl\t%s\n", increment_reg->bit_32);
            inst_list = add_inst(inst_list, buffer_main);
        }
    }

    int needs_addr = 0;
    StackNode_t *inc_spill = NULL;
    if (target_expr != NULL)
    {
        if (target_expr->type == EXPR_VAR_ID)
        {
            int scope_depth = 0;
            StackNode_t *var_node = find_label_with_depth(target_expr->expr_data.id, &scope_depth);
            if (var_node != NULL && var_node->is_reference)
                needs_addr = 1;
        }
        else if (target_expr->type == EXPR_ARRAY_ACCESS || codegen_expr_is_addressable(target_expr))
        {
            needs_addr = 1;
        }
    }

    if (needs_addr)
    {
        int spill_size = target_uses_qword ? 8 : 4;
        inc_spill = codegen_alloc_incdec_temp(spill_size);
        assert(inc_spill != NULL);
        if (inc_spill == NULL)
            return inst_list;

        char spill_buf[96];
        if (target_uses_qword)
            snprintf(spill_buf, sizeof(spill_buf), "\tmovq\t%s, -%d(%%rbp)\n",
                increment_reg->bit_64, inc_spill->offset);
        else
            snprintf(spill_buf, sizeof(spill_buf), "\tmovl\t%s, -%d(%%rbp)\n",
                increment_reg->bit_32, inc_spill->offset);
        inst_list = add_inst(inst_list, spill_buf);

        free_reg(get_reg_stack(), increment_reg);
        increment_reg = NULL;
    }

    if (target_expr != NULL && target_expr->type == EXPR_VAR_ID)
    {
        int scope_depth = 0;
        StackNode_t *var_node = find_label_with_depth(target_expr->expr_data.id, &scope_depth);
        if (var_node == NULL && ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindSymbol(&target_node, ctx->symtab, target_expr->expr_data.id) != 0 &&
                target_node != NULL && target_node->mangled_id != NULL)
            {
                var_node = find_label_with_depth(target_node->mangled_id, &scope_depth);
            }
        }
        char buffer[128];
        if (var_node != NULL)
        {
            if (var_node->is_reference)
            {
                Register_t *addr_reg = NULL;
                inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
                if (!codegen_had_error(ctx) && addr_reg != NULL)
                {
                    const char *value_reg64 = increment_reg != NULL ? increment_reg->bit_64 : "%rax";
                    const char *value_reg32 = increment_reg != NULL ? increment_reg->bit_32 : "%eax";
                    if (inc_spill != NULL && increment_reg == NULL)
                    {
                        char reload_buf[96];
                        if (target_uses_qword)
                            snprintf(reload_buf, sizeof(reload_buf), "\tmovq\t-%d(%%rbp), %%rax\n",
                                inc_spill->offset);
                        else
                            snprintf(reload_buf, sizeof(reload_buf), "\tmovl\t-%d(%%rbp), %%eax\n",
                                inc_spill->offset);
                        inst_list = add_inst(inst_list, reload_buf);
                    }
                    if (target_uses_qword)
                        snprintf(buffer, sizeof(buffer), "\taddq\t%s, (%s)\n", value_reg64, addr_reg->bit_64);
                    else
                        snprintf(buffer, sizeof(buffer), "\taddl\t%s, (%s)\n", value_reg32, addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), addr_reg);
                }
            }
            else if (var_node->is_static)
            {
                const char *label = (var_node->static_label != NULL) ?
                    var_node->static_label : var_node->label;
                if (target_uses_qword)
                    snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s(%%rip)\n", increment_reg->bit_64, label);
                else
                    snprintf(buffer, sizeof(buffer), "\taddl\t%s, %s(%%rip)\n", increment_reg->bit_32, label);
            }
            else if (scope_depth > 0)
            {
                codegen_begin_expression(ctx);
                Register_t *frame_reg = codegen_acquire_static_link(ctx, &inst_list, scope_depth);
                if (frame_reg != NULL)
                {
                    if (target_uses_qword)
                        snprintf(buffer, sizeof(buffer), "\taddq\t%s, -%d(%s)\n", increment_reg->bit_64, var_node->offset, frame_reg->bit_64);
                    else
                        snprintf(buffer, sizeof(buffer), "\taddl\t%s, -%d(%s)\n", increment_reg->bit_32, var_node->offset, frame_reg->bit_64);
                }
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: Failed to acquire static link for inc/dec of %s.",
                        target_expr->expr_data.id);
                    if (target_uses_qword)
                        snprintf(buffer, sizeof(buffer), "\taddq\t%s, -%d(%%rbp)\n", increment_reg->bit_64, var_node->offset);
                    else
                        snprintf(buffer, sizeof(buffer), "\taddl\t%s, -%d(%%rbp)\n", increment_reg->bit_32, var_node->offset);
                }
                codegen_end_expression(ctx);
            }
            else
            {
                if (target_uses_qword)
                    snprintf(buffer, sizeof(buffer), "\taddq\t%s, -%d(%%rbp)\n", increment_reg->bit_64, var_node->offset);
                else
                    snprintf(buffer, sizeof(buffer), "\taddl\t%s, -%d(%%rbp)\n", increment_reg->bit_32, var_node->offset);
            }
            if (!var_node->is_reference)
                inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            int offset = 0;
            inst_list = codegen_get_nonlocal(inst_list, target_expr->expr_data.id, &offset, ctx);
            if (target_uses_qword)
                snprintf(buffer, sizeof(buffer), "\taddq\t%s, -%d(%s)\n", increment_reg->bit_64, offset, current_non_local_reg64());
            else
                snprintf(buffer, sizeof(buffer), "\taddl\t%s, -%d(%s)\n", increment_reg->bit_32, offset, current_non_local_reg64());
            inst_list = add_inst(inst_list, buffer);
        }
    }
    else if (target_expr != NULL && target_expr->type == EXPR_ARRAY_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_array_element_address(target_expr, inst_list, ctx, &addr_reg);
        if (!codegen_had_error(ctx) && addr_reg != NULL)
        {
            char buffer[128];
            const char *value_reg64 = increment_reg != NULL ? increment_reg->bit_64 : "%rax";
            const char *value_reg32 = increment_reg != NULL ? increment_reg->bit_32 : "%eax";
            if (inc_spill != NULL && increment_reg == NULL)
            {
                char reload_buf[96];
                if (target_uses_qword)
                    snprintf(reload_buf, sizeof(reload_buf), "\tmovq\t-%d(%%rbp), %%rax\n",
                        inc_spill->offset);
                else
                    snprintf(reload_buf, sizeof(reload_buf), "\tmovl\t-%d(%%rbp), %%eax\n",
                        inc_spill->offset);
                inst_list = add_inst(inst_list, reload_buf);
            }
            if (target_uses_qword)
                snprintf(buffer, sizeof(buffer), "\taddq\t%s, (%s)\n", value_reg64, addr_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\taddl\t%s, (%s)\n", value_reg32, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), addr_reg);
        }
    }
    else if (codegen_expr_is_addressable(target_expr))
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
        if (!codegen_had_error(ctx) && addr_reg != NULL)
        {
            char buffer[128];
            const char *value_reg64 = increment_reg != NULL ? increment_reg->bit_64 : "%rax";
            const char *value_reg32 = increment_reg != NULL ? increment_reg->bit_32 : "%eax";
            if (inc_spill != NULL && increment_reg == NULL)
            {
                char reload_buf[96];
                if (target_uses_qword)
                    snprintf(reload_buf, sizeof(reload_buf), "\tmovq\t-%d(%%rbp), %%rax\n",
                        inc_spill->offset);
                else
                    snprintf(reload_buf, sizeof(reload_buf), "\tmovl\t-%d(%%rbp), %%eax\n",
                        inc_spill->offset);
                inst_list = add_inst(inst_list, reload_buf);
            }
            if (target_uses_qword)
                snprintf(buffer, sizeof(buffer), "\taddq\t%s, (%s)\n", value_reg64, addr_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\taddl\t%s, (%s)\n", value_reg32, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), addr_reg);
        }
    }
    else
    {
        codegen_report_error(ctx, "ERROR: Unsupported Inc target.");
    }

    if (increment_reg != NULL)
        free_reg(get_reg_stack(), increment_reg);
    return inst_list;
}

static ListNode_t *codegen_builtin_inc(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    return codegen_builtin_incdec(stmt, inst_list, ctx, 1);
}

static ListNode_t *codegen_builtin_dec(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    return codegen_builtin_incdec(stmt, inst_list, ctx, 0);
}

static ListNode_t *codegen_builtin_include_exclude(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx, int is_exclude)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL)
        return inst_list;

    struct Expression *set_expr = (struct Expression *)args_expr->cur;
    struct Expression *value_expr = (struct Expression *)args_expr->next->cur;

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(set_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    Register_t *value_reg = NULL;
    inst_list = codegen_expr_with_result(value_expr, inst_list, ctx, &value_reg);
    if (codegen_had_error(ctx) || value_reg == NULL)
    {
        if (addr_reg != NULL)
            free_reg(get_reg_stack(), addr_reg);
        if (value_reg != NULL)
            free_reg(get_reg_stack(), value_reg);
        return inst_list;
    }

    char buffer[128];

    /* btsl/btrl can operate directly on memory, so we only need the
       bit position in value_reg.  No extra registers needed. */
    snprintf(buffer, sizeof(buffer), "\tandl\t$255, %s\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    if (is_exclude)
    {
        snprintf(buffer, sizeof(buffer), "\tbtrl\t%s, (%s)\n", value_reg->bit_32, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tbtsl\t%s, (%s)\n", value_reg->bit_32, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    free_reg(get_reg_stack(), value_reg);
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}

static ListNode_t *codegen_builtin_include(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    return codegen_builtin_include_exclude(stmt, inst_list, ctx, 0);
}

static ListNode_t *codegen_builtin_exclude(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    return codegen_builtin_include_exclude(stmt, inst_list, ctx, 1);
}

static ListNode_t *codegen_builtin_new(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    struct Expression *method_expr = NULL;
    if (args_expr != NULL && args_expr->next != NULL)
    {
        if (args_expr->next->next == NULL)
            method_expr = (struct Expression *)args_expr->next->cur;
        else
            args_expr = NULL;
    }
    if (args_expr == NULL)
    {
        fprintf(stderr, "ERROR: New expects exactly one argument.\n");
        return inst_list;
    }

    struct Expression *target_expr = (struct Expression *)args_expr->cur;

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    long long alloc_size = 0;
    if (codegen_sizeof_pointer_target(ctx, target_expr, &alloc_size) != 0 || alloc_size <= 0)
    {
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    Register_t *size_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (size_reg == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to allocate register for New size.");
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", alloc_size, size_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    if (codegen_target_is_windows())
    {
        // Move size to %rdx first, before moving addr to %rcx
        // This avoids overwriting size_reg if it happens to be %rcx
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", size_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", size_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_new");

    free_reg(get_reg_stack(), addr_reg);
    free_reg(get_reg_stack(), size_reg);
    free_arg_regs();

    if (method_expr != NULL && !codegen_had_error(ctx))
        inst_list = codegen_emit_new_dispose_method_fallback(stmt, inst_list, ctx,
            target_expr, method_expr);

    return inst_list;
}

static ListNode_t *codegen_builtin_dispose(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    struct Expression *method_expr = NULL;
    if (args_expr != NULL && args_expr->next != NULL)
    {
        if (args_expr->next->next == NULL)
            method_expr = (struct Expression *)args_expr->next->cur;
        else
            args_expr = NULL;
    }
    if (args_expr == NULL)
    {
        fprintf(stderr, "ERROR: Dispose expects exactly one argument.\n");
        return inst_list;
    }

    struct Expression *target_expr = (struct Expression *)args_expr->cur;

    if (method_expr != NULL)
    {
        inst_list = codegen_emit_new_dispose_method_fallback(stmt, inst_list, ctx,
            target_expr, method_expr);
        if (codegen_had_error(ctx))
            return inst_list;
    }

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_dispose");

    free_reg(get_reg_stack(), addr_reg);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_write_like(struct Statement *stmt, ListNode_t *inst_list,
                                              CodeGenContext *ctx, int append_newline)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    char buffer[128];
    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    Register_t *file_reg = NULL;
    StackNode_t *file_spill = NULL;
    int has_file_arg = 0;

    if (args != NULL)
    {
        struct Expression *first_expr = (struct Expression *)args->cur;
        if (first_expr != NULL && (expr_has_type_tag(first_expr, TEXT_TYPE)))
        {
            file_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (file_reg == NULL)
                file_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (file_reg != NULL)
            {
                Register_t *addr_reg = NULL;
                inst_list = codegen_address_for_expr(first_expr, inst_list, ctx, &addr_reg);
                if (addr_reg != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                        addr_reg->bit_64, file_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), addr_reg);
                }
                has_file_arg = 1;
                file_spill = add_l_t("write_file");
                if (file_spill != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", file_reg->bit_64, file_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), file_reg);
                    file_reg = NULL;
                }
            }
            else
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for write file argument.");
            }
            args = args->next;
        }
    }

    while (args != NULL)
    {
        struct Expression *expr = (struct Expression *)args->cur;

        int expr_type = (expr != NULL) ? expr_get_type_tag(expr) : UNKNOWN_TYPE;

        /* If we couldn't get a reliable type tag from the expression itself,
         * try the symbol table (helps with string params/vars that lost their
         * resolved_kgpc_type during earlier passes). */
        if (expr != NULL && ctx != NULL && ctx->symtab != NULL && expr->type == EXPR_VAR_ID)
        {
            HashNode_t *sym_node = NULL;
            if (FindSymbol(&sym_node, ctx->symtab, expr->expr_data.id) != 0 &&
                sym_node != NULL && sym_node->type != NULL)
            {
                int sym_type = codegen_tag_from_kgpc(sym_node->type);
                /* Only override the expression's own type when it is unknown.
                 * If the semantic checker already resolved a type (e.g. Pi as REAL_TYPE
                 * via the builtin const shadowing an FPC internproc function), trust it. */
                if (sym_type != UNKNOWN_TYPE && expr_type == UNKNOWN_TYPE)
                    expr_type = sym_type;
                if (expr->resolved_kgpc_type == NULL)
                    expr->resolved_kgpc_type = sym_node->type;
            }
        }
        
        /* If still unknown, check stack metadata (captures parameter/local sizes when
         * the symtab scope was already popped after semantic analysis). */
        if (expr != NULL && expr->type == EXPR_VAR_ID && expr_type == UNKNOWN_TYPE)
        {
            int scope_depth = 0;
            StackNode_t *stack_node = find_label_with_depth(expr->expr_data.id, &scope_depth);
            if (stack_node != NULL && stack_node->size == 8)
                expr_type = STRING_TYPE;
        }

        /* Propagate the discovered type back into the expression so downstream
         * codegen (expr trees, storage sizing) can make the right width choice. */
        (void)expr;
        
        int expr_is_wide_string = codegen_expr_is_wide_string_value(expr);

        /* Treat char arrays and all managed string forms as strings for printing */
        int treat_as_string = (expr_type == STRING_TYPE || expr_type == SHORTSTRING_TYPE ||
            expr_is_wide_string);
        /* If type info is missing but the literal is a string (and not typed as CHAR),
         * still treat it as a string for write/writeln. This fixes string literals that
         * lost their resolved_type during parsing/semantics. */
        if (!treat_as_string && expr != NULL && expr->type == EXPR_STRING && expr_type != CHAR_TYPE)
            treat_as_string = 1;
        if (expr != NULL && expr_type == CHAR_TYPE && expr->is_array_expr && expr->array_element_type == CHAR_TYPE)
        {
            treat_as_string = 1;
        }

        /* Also treat PAnsiChar (pointer to char) as string */
        if (expr != NULL && expr->resolved_kgpc_type != NULL && 
            kgpc_type_is_pointer(expr->resolved_kgpc_type))
        {
             if (expr->resolved_kgpc_type->type_alias != NULL && expr->resolved_kgpc_type->type_alias->target_type_id != NULL)
             {
                 const char *alias_name = expr->resolved_kgpc_type->type_alias->target_type_id;
                 
                 if (strcasecmp(alias_name, "PAnsiChar") == 0 ||
                     strcasecmp(alias_name, "PChar") == 0 ||
                     strcasecmp(alias_name, "pcchar") == 0 ||
                     strcasecmp(alias_name, "cchar") == 0 ||
                     strcasecmp(alias_name, "char") == 0)
                 {
                     treat_as_string = 1;
                 }
             }
        }

        if (expr != NULL && expr->resolved_kgpc_type != NULL && 
            kgpc_type_is_pointer(expr->resolved_kgpc_type))
        {
            int subtype = kgpc_type_get_pointer_subtype_tag(expr->resolved_kgpc_type);
            if (subtype == CHAR_TYPE)
            {
                treat_as_string = 1;
            }
        }
        
        /* Check for pointer type with char subtype (covers PAnsiChar, PChar, etc.) */
        if (expr != NULL && expr_type == POINTER_TYPE && expr->pointer_subtype == CHAR_TYPE)
        {
            treat_as_string = 1;
        }
        
        /* Check for array access expressions where element type is PAnsiChar/PChar by name */
        if (expr != NULL && expr->is_array_expr && expr->array_element_type_id != NULL)
        {
            if (strcasecmp(expr->array_element_type_id, "PAnsiChar") == 0 ||
                strcasecmp(expr->array_element_type_id, "PChar") == 0 ||
                strcasecmp(expr->array_element_type_id, "pcchar") == 0)
            {
                treat_as_string = 1;
            }
        }
        
        const int expr_is_real = (expr_type == REAL_TYPE);
        const char *file_dest64 = current_arg_reg64(0);
        const char *width_dest64 = current_arg_reg64(1);
        const char *precision_dest64 = current_arg_reg64(2);
        const char *value_dest64 = current_arg_reg64(expr_is_real ? 3 : 2);

        StackNode_t *width_spill = NULL;
        StackNode_t *precision_spill = NULL;
        const int width_specified = (expr != NULL && expr->field_width != NULL);
        const int precision_specified = (expr_is_real && expr != NULL && expr->field_precision != NULL);

        if (width_specified)
        {
            expr_node_t *width_tree = build_expr_tree(expr->field_width);
            Register_t *width_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(width_tree, inst_list, ctx, width_reg);
            free_expr_tree(width_tree);
            width_spill = add_l_t("write_width");
            if (width_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                         width_reg->bit_64, width_spill->offset);
                inst_list = add_inst(inst_list, buffer);
            }
            free_reg(get_reg_stack(), width_reg);
        }

        if (precision_specified)
        {
            expr_node_t *precision_tree = build_expr_tree(expr->field_precision);
            Register_t *precision_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(precision_tree, inst_list, ctx, precision_reg);
            free_expr_tree(precision_tree);
            precision_spill = add_l_t("write_precision");
            if (precision_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                         precision_reg->bit_64, precision_spill->offset);
                inst_list = add_inst(inst_list, buffer);
            }
            free_reg(get_reg_stack(), precision_reg);
        }

        /* For char arrays being treated as strings, we need to load the address */
        Register_t *value_reg = NULL;
        if (expr != NULL && expr_type == CHAR_TYPE && expr->is_array_expr && expr->array_element_type == CHAR_TYPE)
        {
            /* Load address of char array. codegen_address_for_expr allocates its own register. */
            inst_list = codegen_address_for_expr(expr, inst_list, ctx, &value_reg);
            if (codegen_had_error(ctx) || value_reg == NULL)
            {
                if (value_reg != NULL)
                    free_reg(get_reg_stack(), value_reg);
                return inst_list;
            }
        }
        else if (expr != NULL && expr_type == SHORTSTRING_TYPE)
        {
            /* Load address of shortstring (array of char). codegen_address_for_expr allocates its own register. */
            inst_list = codegen_address_for_expr(expr, inst_list, ctx, &value_reg);
            if (codegen_had_error(ctx) || value_reg == NULL)
            {
                if (value_reg != NULL)
                    free_reg(get_reg_stack(), value_reg);
                return inst_list;
            }
        }
        else if (expr != NULL && expr->type == EXPR_RELOP)
        {
            /* Use special relop handling for char set IN operations and other relops.
             * codegen_relop_to_value allocates its own register. */
            inst_list = codegen_relop_to_value(expr, inst_list, ctx, &value_reg);
            if (codegen_had_error(ctx) || value_reg == NULL)
            {
                if (value_reg != NULL)
                    free_reg(get_reg_stack(), value_reg);
                return inst_list;
            }
        }
        else
        {
            /* Load value normally - need to allocate register for expr tree evaluation */
            value_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (value_reg == NULL)
                value_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (value_reg == NULL)
            {
                codegen_report_error(ctx, "ERROR: Unable to allocate register for write value.");
                return inst_list;
            }
            expr_node_t *expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, value_reg);
            free_expr_tree(expr_tree);
        }

        /*
         * Handle register conflicts when setting up write function arguments.
         * We need to move registers to their destinations in an order that
         * doesn't cause intermediate values to be overwritten.
         * 
         * Potential conflicts:
         * - width_reg might be in value_dest64's position
         * - precision_reg might be in value_dest64 or width_dest64's position
         * 
         * Strategy: Move in reverse order of argument positions to avoid overwrites.
         * On Windows: value=%r8 (arg2), width=%rdx (arg1), precision=%r9 (arg3)
         * So: Move precision first, then width, then value.
         */
        
        /* Move precision first (if real and has precision) */
        /* Move precision first (if specified for reals) */
        if (expr_is_real && precision_specified)
        {
            if (precision_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                         precision_spill->offset, precision_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", precision_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        
        /* Move width next */
        if (width_specified)
        {
            if (width_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                         width_spill->offset, width_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", width_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", width_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        
        /* Move value last */
        if (treat_as_string)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, value_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (expr_is_real || expr_type == POINTER_TYPE)
        {
            /* Extended sret function calls leave the buffer ADDRESS in the
             * register.  Convert to double bits for kgpc_write_real. */
            if (expr_is_real && expr_returns_sret(expr) &&
                codegen_expr_involves_extended(expr))
            {
                const char *arg1_64 = codegen_target_is_windows() ? "%rcx" : "%rdi";
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, arg1_64);
                inst_list = add_inst(inst_list, buffer);
                inst_list = codegen_vect_reg(inst_list, 0);
                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_load_extended_to_bits");
                free_arg_regs();
                snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", value_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            /* REAL_TYPE and POINTER_TYPE are 64-bit - use movq */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, value_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (expr_value_requires_64bit(expr, ctx))
        {
            /* Int64/QWord/UInt64 or large const values - use 64-bit move */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, value_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (expr_type == LONGINT_TYPE)
        {
            /* LONGINT_TYPE is now 4 bytes (to match FPC) - allow unsigned aliases to zero-extend */
            if (expr_is_unsigned_type(expr))
            {
                inst_list = codegen_zero_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_32);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, value_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_dest64);
            }
        }
        else
        {
            /* Use zero-extension for unsigned types, sign-extension otherwise.
             * For zero-extension: writing to a 32-bit register automatically zeros
             * the upper 32 bits of the full 64-bit register. */
            int is_unsigned = expr_is_unsigned_type(expr);
            if (is_unsigned)
            {
                /* First zero-extend in the value register, then move to destination */
                inst_list = codegen_zero_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_32);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, value_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_dest64);
        }

        free_reg(get_reg_stack(), value_reg);

        /* Set precision to -1 if not real or not provided */
        if (expr_is_real && !precision_specified)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", precision_dest64);
            inst_list = add_inst(inst_list, buffer);
        }

        /* Determine if this is an unsigned type for printing */
        int is_unsigned_int = expr_is_unsigned_type(expr);
        
        const char *call_target = is_unsigned_int ? "kgpc_write_unsigned" : "kgpc_write_integer";
        int is_char_array = 0;
        int char_array_size = 0;
        
        if (treat_as_string)
        {
            /* Check if it's a char array (not a regular string) */
            if (expr != NULL && expr_type == CHAR_TYPE && expr->is_array_expr && expr->array_element_type == CHAR_TYPE)
            {
                char_array_size = expr->array_upper_bound - expr->array_lower_bound + 1;
                
                /* Check if this is a ShortString-like array (length byte at index 0) */
                if (codegen_expr_is_shortstring_array(expr))
                {
                    call_target = "kgpc_write_shortstring";
                }
                else
                {
                    call_target = "kgpc_write_char_array";
                    is_char_array = 1;
                }
            }
            else if (expr_type == SHORTSTRING_TYPE ||
                     codegen_expr_is_shortstring_array(expr) ||
                     codegen_expr_is_shortstring_value_ctx(expr, ctx))
            {
                /* Handle ShortString type - use special write function that handles length prefix.
                 * Exception: string literals are still stored as C strings even when typed as
                 * SHORTSTRING_TYPE (via {$H-}), so use regular string write for those. */
                if (expr != NULL && expr->type == EXPR_STRING)
                    call_target = "kgpc_write_string";
                else
                    call_target = "kgpc_write_shortstring";
            }
            else
            {
                call_target = expr_is_wide_string ? "kgpc_write_unicodestring" : "kgpc_write_string";
            }
        }
        else if (expr_type == BOOL)
            call_target = "kgpc_write_boolean";
        else if (expr_is_real)
            call_target = "kgpc_write_real";
        else if (expr_type == CHAR_TYPE)
            call_target = "kgpc_write_char";
        else if (expr_type == POINTER_TYPE)
            call_target = "kgpc_write_integer";  // Print pointers as integers (addresses)

        if (has_file_arg && (file_spill != NULL || file_reg != NULL))
        {
            if (file_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", file_spill->offset, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", file_reg->bit_64, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64, file_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        
        /* For char arrays, pass the size as the 4th argument */
        if (is_char_array)
        {
            const char *size_dest64 = current_arg_reg64(3);
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %s\n", char_array_size, size_dest64);
            inst_list = add_inst(inst_list, buffer);
        }

        inst_list = codegen_vect_reg(inst_list, 0);

        inst_list = codegen_call_with_shadow_space(inst_list, call_target);

        free_arg_regs();
        
        /* Invalidate static link cache after each write argument
         * because the static link register may have been clobbered
         * during argument evaluation or the function call.
         * We must free the register first to avoid leaking it. */
        if (ctx->static_link_reg != NULL)
        {
            free_reg(get_reg_stack(), ctx->static_link_reg);
            ctx->static_link_reg = NULL;
            ctx->static_link_reg_level = 0;
        }

        args = args->next;
    }

    if (append_newline)
    {
        const char *file_dest64 = current_arg_reg64(0);
        if (has_file_arg && (file_spill != NULL || file_reg != NULL))
        {
            if (file_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", file_spill->offset, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", file_reg->bit_64, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64, file_dest64);
            inst_list = add_inst(inst_list, buffer);
        }

        inst_list = codegen_vect_reg(inst_list, 0);

        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_write_newline");

        free_arg_regs();
    }

    if (file_reg != NULL)
        free_reg(get_reg_stack(), file_reg);

    return inst_list;
}

static ListNode_t *codegen_builtin_read_like(struct Statement *stmt, ListNode_t *inst_list,
                                             CodeGenContext *ctx, int read_line)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    char buffer[128];
    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    Register_t *file_reg = NULL;
    StackNode_t *file_spill = NULL;
    int has_file_arg = 0;
    int read_consumed_line = 0;

    /* Check if first argument is a file */
    if (args != NULL)
    {
        struct Expression *first_expr = (struct Expression *)args->cur;
        if (first_expr != NULL && (expr_has_type_tag(first_expr, TEXT_TYPE)))
        {
            file_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (file_reg == NULL)
                file_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (file_reg != NULL)
            {
                Register_t *addr_reg = NULL;
                inst_list = codegen_address_for_expr(first_expr, inst_list, ctx, &addr_reg);
                if (addr_reg != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                        addr_reg->bit_64, file_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), addr_reg);
                }
                has_file_arg = 1;
                file_spill = add_l_t("read_file");
                if (file_spill != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", file_reg->bit_64, file_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), file_reg);
                    file_reg = NULL;
                }
            }
            else
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for read file argument.");
            }
            args = args->next;
        }
    }

    /* Process each argument to read */
    while (args != NULL)
    {
        struct Expression *expr = (struct Expression *)args->cur;
        int expr_type = (expr != NULL) ? expr_get_type_tag(expr) : UNKNOWN_TYPE;
        
        /* Get address of the variable to read into and save to stack.
         * codegen_address_for_expr allocates its own register, so we pass NULL
         * and let it handle register allocation. */
        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
        if (addr_reg == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate register for read address.");
            args = args->next;
            continue;
        }
        if (codegen_had_error(ctx))
        {
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
        
        /* Save address to a stack temporary to avoid register conflicts */
        StackNode_t *addr_spill = add_l_t("read_addr");
        if (addr_spill == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate stack space for read address.");
            free_reg(get_reg_stack(), addr_reg);
            args = args->next;
            continue;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        
        /* Special handling for STRING_TYPE - use kgpc_text_readln_into */
        if (expr_type == STRING_TYPE)
        {
            const char *file_dest64 = current_arg_reg64(0);
            const char *string_dest64 = current_arg_reg64(1);
            
            /* Set file argument (or NULL for stdin) */
            if (has_file_arg && file_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", file_spill->offset, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            
            /* Load string address from stack */
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", addr_spill->offset, string_dest64);
            inst_list = add_inst(inst_list, buffer);
            
            /* Call kgpc_text_readln_into for string reading */
            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_text_readln_into");
            free_arg_regs();
            
            /* Invalidate static link cache after call */
            if (ctx->static_link_reg != NULL)
            {
                free_reg(get_reg_stack(), ctx->static_link_reg);
                ctx->static_link_reg = NULL;
                ctx->static_link_reg_level = 0;
            }

            if (read_line)
                read_consumed_line = 1;
            
            args = args->next;
            continue;
        }

        /* Special handling for SHORTSTRING_TYPE - use kgpc_text_readln_into_shortstring */
        if (expr_type == SHORTSTRING_TYPE)
        {
            const char *file_dest64 = current_arg_reg64(0);
            const char *string_dest64 = current_arg_reg64(1);
            
            /* Set file argument (or NULL for stdin) */
            if (has_file_arg && file_spill != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", file_spill->offset, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64, file_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
            
            /* Load shortstring address from stack */
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", addr_spill->offset, string_dest64);
            inst_list = add_inst(inst_list, buffer);
            
            /* Set max length = 255 (standard ShortString) */
            snprintf(buffer, sizeof(buffer), "\tmovl\t$255, %s\n", current_arg_reg32(2));
            inst_list = add_inst(inst_list, buffer);
            
            /* Call kgpc_text_readln_into_shortstring */
            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_text_readln_into_shortstring");
            free_arg_regs();
            
            /* Invalidate static link cache after call */
            if (ctx->static_link_reg != NULL)
            {
                free_reg(get_reg_stack(), ctx->static_link_reg);
                ctx->static_link_reg = NULL;
                ctx->static_link_reg_level = 0;
            }

            if (read_line)
                read_consumed_line = 1;
            
            args = args->next;
            continue;
        }
        
        /* Now set up arguments for non-variadic read functions:
         * arg0 (rdi/rcx): file pointer (NULL for stdin)
         * arg1 (rsi/rdx): address of variable to read into
         */
        const char *file_dest64 = current_arg_reg64(0);
        const char *addr_dest64 = current_arg_reg64(1);
        
        /* Determine which read function to call based on type */
        const char *read_func = NULL;
        switch (expr_type)
        {
            case INT_TYPE:
                read_func = "kgpc_read_integer";
                break;
            case LONGINT_TYPE:
                read_func = "kgpc_read_longint";
                break;
            case CHAR_TYPE:
                read_func = "kgpc_read_char";
                break;
            case REAL_TYPE:
                read_func = "kgpc_read_real";
                break;
            default:
                codegen_report_error(ctx, "ERROR: Unsupported type for read operation.");
                args = args->next;
                continue;
        }
        
        /* Set file argument (or NULL for stdin) */
        if (has_file_arg && file_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", file_spill->offset, file_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64, file_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        
        /* Load address from stack temporary to argument register */
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", addr_spill->offset, addr_dest64);
        inst_list = add_inst(inst_list, buffer);
        
        /* Call the appropriate read function */
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, read_func);
        free_arg_regs();
        
        /* Invalidate static link cache after each read argument */
        if (ctx->static_link_reg != NULL)
        {
            free_reg(get_reg_stack(), ctx->static_link_reg);
            ctx->static_link_reg = NULL;
            ctx->static_link_reg_level = 0;
        }
        
        args = args->next;
    }
    
    /* If readln, consume rest of line */
    if (read_line && !read_consumed_line)
    {
        const char *file_dest64 = current_arg_reg64(0);
        if (has_file_arg && file_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", file_spill->offset, file_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64, file_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_text_readln_discard");
        free_arg_regs();
    }
    
    if (file_reg != NULL)
        free_reg(get_reg_stack(), file_reg);

    return inst_list;
}

static ListNode_t *codegen_builtin_move(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next == NULL)
        return inst_list;

    /* Pascal Move is Move(src, dst, count); kgpc_move expects destination
     * first. Reorder the call explicitly instead of letting the generic
     * builtin dispatcher emit a direct call to "Move". */
    ListNode_t *dst_node = CreateListNode(args->next->cur, LIST_EXPR);
    ListNode_t *src_node = CreateListNode(args->cur, LIST_EXPR);
    ListNode_t *count_node = CreateListNode(args->next->next->cur, LIST_EXPR);
    dst_node->next = src_node;
    src_node->next = count_node;

    inst_list = codegen_pass_arguments(dst_node, inst_list, ctx, NULL, "Move", 0, NULL, 0);
    DestroyList(dst_node);
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
    return codegen_cleanup_call_stack(inst_list, ctx);
}

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
