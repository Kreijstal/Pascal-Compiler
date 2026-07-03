#include "../codegen_stmt_internal.h"

static ListNode_t *codegen_builtin_setlength(struct Statement *stmt,
                                             ListNode_t *inst_list,
                                             CodeGenContext *ctx) {
  assert(stmt != NULL);
  assert(ctx != NULL);

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  if (args_expr == NULL || args_expr->next == NULL) {
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
  if (array_expr != NULL && array_expr->type == EXPR_VAR_ID) {
    array_id = array_expr->expr_data.id;
  } else if (array_expr != NULL && array_expr->type == EXPR_RECORD_ACCESS) {
    /* For field access like self.FItems, use the field name */
    array_id = array_expr->expr_data.record_access_data.field_id;
  } else if (array_expr != NULL && array_expr->type == EXPR_ARRAY_ACCESS) {
    /* Nested dynamic array: SetLength(arr[i], n) where arr[i] is itself a
     * dynamic array */
    array_id = "__nested_dynarray__";
  } else if (array_expr != NULL && codegen_expr_is_addressable(array_expr)) {
    /* Accept addressable expressions such as SetLength(p^, n). */
    array_id = "__addressable_dynarray__";
  }

  if (array_id == NULL) {
    fprintf(stderr, "ERROR: SetLength first argument must be a variable or "
                    "field identifier.\n");
    return inst_list;
  }

  int setlength_scope_depth = 0;
  StackNode_t *array_node =
      find_label_with_depth((char *)array_id, &setlength_scope_depth);

  /* If "Result" not found, try the current function name (function return
   * variable) */
  if (array_node == NULL && pascal_identifier_equals(array_id, "Result") &&
      ctx != NULL && ctx->current_subprogram_id != NULL) {
    array_node = find_label_with_depth((char *)ctx->current_subprogram_id,
                                       &setlength_scope_depth);
  }

  int is_field_array = 0;
  int is_nested_array = 0;
  int use_expr_address = 0;

  /* If not found in local stack, might be a field of the current object */
  if (array_node == NULL && array_expr->type == EXPR_RECORD_ACCESS) {
    is_field_array = 1;
  }
  /* Nested dynamic array: arr[i] where arr[i] is itself a dynamic array */
  if (array_node == NULL && array_expr->type == EXPR_ARRAY_ACCESS) {
    is_nested_array = 1;
  }

  if (!is_field_array && !is_nested_array) {
    if (array_node == NULL) {
      use_expr_address = 1;
    } else if (!array_node->is_dynamic) {
      array_node->is_array = 1;
      array_node->is_dynamic = 1;
    }
  }

  int element_size;
  if (is_nested_array) {
    /* For nested dynamic arrays (array of array of ...), get element size from
     * KgpcType */
    element_size =
        8; /* Default: inner elements are pointers (dynamic arrays) */
    KgpcType *arr_type = expr_get_kgpc_type(array_expr);
    if (arr_type != NULL && kgpc_type_is_array(arr_type)) {
      KgpcType *elem_type =
          kgpc_type_get_array_element_type_resolved(arr_type, ctx->symtab);
      if (elem_type != NULL) {
        long long sz = kgpc_type_sizeof(elem_type);
        if (sz > 0)
          element_size = (int)sz;
      }
    }
  } else if (is_field_array) {
    struct RecordField *field =
        codegen_lookup_record_field_expr(array_expr, ctx);
    element_size = -1;
    if (field != NULL && field->is_array) {
      long long elem_size = 0;
      if ((field->array_element_type != UNKNOWN_TYPE ||
           field->array_element_type_id != NULL ||
           field->array_element_record != NULL) &&
          codegen_sizeof_type_reference(
              ctx, field->array_element_type, field->array_element_type_id,
              field->array_element_record, &elem_size) == 0 &&
          elem_size > 0) {
        element_size = (int)elem_size;
      }
    }
    if (array_expr != NULL) {
      long long expr_element_size =
          expr_get_array_element_size(array_expr, ctx);
      if (expr_element_size <= 0 && ctx != NULL && ctx->symtab != NULL) {
        KgpcType *array_type = expr_get_kgpc_type(array_expr);
        if (array_type != NULL && kgpc_type_is_array(array_type)) {
          KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(
              array_type, ctx->symtab);
          if (elem_type != NULL)
            expr_element_size = kgpc_type_sizeof(elem_type);
        }
      }
      /* With-stack lookup: for unresolved variables in `with` blocks */
      if (expr_element_size <= 0 && ctx != NULL && ctx->with_depth > 0 &&
          array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL) {
        struct RecordField *with_field =
            codegen_lookup_with_field(ctx, array_expr->expr_data.id, NULL);
        if (with_field != NULL) {
          long long elem_size =
              codegen_array_elem_size_from_field(with_field, ctx);
          if (elem_size > 0)
            expr_element_size = elem_size;
        }
      }
      if (expr_element_size <= 0) {
        codegen_report_error(ctx, "ERROR: array expression is missing "
                                  "element-size metadata in SetLength.");
        return inst_list;
      }
      if (expr_element_size <= INT_MAX &&
          (element_size <= 0 || expr_element_size > element_size))
        element_size = (int)expr_element_size;
    }
    if (element_size <= 0) {
      codegen_report_error(
          ctx,
          "ERROR: unable to resolve SetLength field-array element size "
          "(field=%s).",
          (field != NULL && field->name != NULL) ? field->name : "<unknown>");
      return inst_list;
    }
  } else {
    element_size =
        codegen_dynamic_array_element_size(ctx, array_node, array_expr);
  }

  inst_list = codegen_expr(len_expr, inst_list, ctx);
  if (codegen_had_error(ctx))
    return inst_list;
  Register_t *length_reg = get_free_reg(get_reg_stack(), &inst_list);
  if (length_reg == NULL)
    return codegen_fail_register(
        ctx, inst_list, NULL,
        "ERROR: Unable to allocate register for SetLength length.");

  Register_t *descriptor_reg = get_free_reg(get_reg_stack(), &inst_list);
  if (descriptor_reg == NULL)
    return codegen_fail_register(
        ctx, inst_list, NULL,
        "ERROR: Unable to allocate register for SetLength descriptor.");

  char buffer[128];
  if (use_expr_address) {
    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(array_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
      return inst_list;
    {
      Register_t *d[] = {descriptor_reg};
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovq\t%1, %0\n");
    }
    free_reg(get_reg_stack(), addr_reg);
  } else if (is_nested_array) {
    /* For nested dynamic arrays, compute the address of the inner array
     * element. E.g. SetLength(arr[i], n) where arr[i] is a dynamic array
     * pointer. We need the address of that pointer (not its value). */
    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(array_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
      return inst_list;
    {
      Register_t *d[] = {descriptor_reg};
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovq\t%1, %0\n");
    }
    free_reg(get_reg_stack(), addr_reg);
  } else if (is_field_array) {
    /* For field arrays, compute the field address from the base record
     * expression. */
    Register_t *field_addr_reg = NULL;
    inst_list = codegen_record_field_address(array_expr, inst_list, ctx,
                                             &field_addr_reg);
    if (codegen_had_error(ctx) || field_addr_reg == NULL)
      return inst_list;
    {
      Register_t *d[] = {descriptor_reg};
      Register_t *u[] = {field_addr_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovq\t%1, %0\n");
    }
    free_reg(get_reg_stack(), field_addr_reg);
  } else if (array_node->is_static) {
    const char *label = (array_node->static_label != NULL)
                            ? array_node->static_label
                            : array_node->label;
    char tmpl[256];
    snprintf(tmpl, sizeof(tmpl), "\tleaq\t%s(%%rip), %%0\n", label);
    {
      Register_t *d[] = {descriptor_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
    }
  } else if (setlength_scope_depth > 0) {
    /* Non-local variable: access via static link */
    Register_t *frame_reg =
        codegen_acquire_static_link(ctx, &inst_list, setlength_scope_depth);
    if (frame_reg == NULL) {
      codegen_report_error(
          ctx,
          "ERROR: Failed to acquire static link for SetLength variable %s.",
          array_id);
      free_reg(get_reg_stack(), descriptor_reg);
      free_reg(get_reg_stack(), length_reg);
      free_arg_regs();
      return inst_list;
    }
    char tmpl[64];
    if (array_node->is_reference)
      snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%1), %%0\n",
               array_node->offset);
    else
      snprintf(tmpl, sizeof(tmpl), "\tleaq\t-%d(%%1), %%0\n",
               array_node->offset);
    {
      Register_t *d[] = {descriptor_reg};
      Register_t *u[] = {frame_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, tmpl);
    }
  } else {
    char tmpl[64];
    if (array_node->is_reference)
      snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%rbp), %%0\n",
               array_node->offset);
    else
      snprintf(tmpl, sizeof(tmpl), "\tleaq\t-%d(%%rbp), %%0\n",
               array_node->offset);
    {
      Register_t *d[] = {descriptor_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
    }
  }

  inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32,
                                         length_reg->bit_64);

  if (codegen_target_is_windows()) {
    const char *arg0 = current_arg_reg64(0); /* %rcx */
    RegisterId_t arg0_id = codegen_arg_reg_id_num(0);
    RegisterId_t arg1_id = codegen_arg_reg_id_num(1);

    /* Check if we need to swap or save/restore to avoid clobbering */
    int descriptor_is_arg1 = (descriptor_reg->reg_id == arg1_id);
    int length_is_arg0 = (length_reg->reg_id == arg0_id);

    if (descriptor_is_arg1 && length_is_arg0) {
      /* Simple swap: descriptor in %rdx, length in %rcx */
      /* We want: descriptor in %rcx, length in %rdx */
      /* Use xchg or save to stack */
      StackNode_t *temp = add_l_t("setlength_temp");
      if (temp != NULL) {
        {
          /* Integrated: store to the frame slot through the backend vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(temp->offset)}}};
          BeOperand a = {OPK_VREG, BE_W64, {.vreg = descriptor_reg}};
          kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
          inst_list = em.list;
        }
        {
          Register_t *u[] = {length_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 temp->offset, arg0);
        inst_list = add_inst(inst_list, buffer);
      }
    } else if (descriptor_is_arg1) {
      /* descriptor in %rdx, need it in %rcx */
      /* Move descriptor first to avoid clobbering */
      {
        Register_t *u[] = {descriptor_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
      }
      {
        Register_t *u[] = {length_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
      }
    } else {
      /* Normal case or length in %rdx */
      {
        Register_t *u[] = {length_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
      }
      {
        Register_t *u[] = {descriptor_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
      }
    }

    snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", element_size,
             current_arg_reg32(2));
    inst_list = add_inst(inst_list, buffer);
  } else {
    {
      Register_t *u[] = {descriptor_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
    }
    {
      Register_t *u[] = {length_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
    }
    snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", element_size,
             current_arg_reg32(2));
    inst_list = add_inst(inst_list, buffer);
  }
  inst_list =
      codegen_call_with_shadow_space(inst_list, "kgpc_dynarray_setlength");

  free_reg(get_reg_stack(), descriptor_reg);
  free_reg(get_reg_stack(), length_reg);

  free_arg_regs();
  return inst_list;
}

ListNode_t *codegen_builtin_setlength_string(struct Statement *stmt,
                                             ListNode_t *inst_list,
                                             CodeGenContext *ctx) {
  assert(stmt != NULL);
  if (ctx == NULL)
    return inst_list;

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  if (args_expr == NULL || args_expr->next == NULL) {
    fprintf(stderr, "ERROR: SetLength expects two arguments.\n");
    return inst_list;
  }

  struct Expression *target_expr = (struct Expression *)args_expr->cur;
  struct Expression *len_expr = (struct Expression *)args_expr->next->cur;
  if (target_expr == NULL || len_expr == NULL)
    return inst_list;

  if (!codegen_expr_is_addressable(target_expr)) {
    fprintf(stderr, "ERROR: SetLength string target must be addressable.\n");
    return inst_list;
  }

  /* Check if length expression contains a function call.
   * If so, we need to spill addr_reg to the stack because
   * the function call will clobber caller-saved registers. */
  int len_has_function_call = expr_contains_function_call(len_expr);
  StackNode_t *addr_spill_slot = NULL;

  Register_t *addr_reg = NULL;
  inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
  if (codegen_had_error(ctx) || addr_reg == NULL)
    return inst_list;

  /* Spill addr_reg to stack if len_expr contains a function call */
  if (len_has_function_call) {
    addr_spill_slot = add_l_t("setlength_addr_spill");
    if (addr_spill_slot != NULL) {
      /* Integrated: store to the frame slot through the backend vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                       {.mem_frame = {BE_BASE_FP, -(long long)(addr_spill_slot->offset)}}};
      BeOperand a = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
      kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
      inst_list = em.list;
    }
  }

  Register_t *length_reg = NULL;
  inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);
  if (codegen_had_error(ctx) || length_reg == NULL) {
    if (length_reg != NULL)
      free_reg(get_reg_stack(), length_reg);
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  }

  /* Reload addr_reg from spill slot if we spilled it */
  if (addr_spill_slot != NULL) {
    /* Integrated: load from the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
    BeOperand src = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(addr_spill_slot->offset)}}};
    kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
    inst_list = em.list;
  }

  if (!expr_uses_qword_kgpctype(len_expr))
    inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32,
                                           length_reg->bit_64);

  const char *arg0 =
      current_arg_reg64(0); /* First argument: %rcx (Win) / %rdi (SysV) */
  const char *arg1 =
      current_arg_reg64(1); /* Second argument: %rdx (Win) / %rsi (SysV) */
  RegisterId_t arg0_id = codegen_arg_reg_id_num(0);
  RegisterId_t arg1_id = codegen_arg_reg_id_num(1);

  /*
   * Handle register conflicts when setting up function arguments.
   * If length_reg is in arg0's position and we try to move addr_reg to arg0,
   * we'll overwrite the length. In this case, move length_reg to arg1 first.
   */
  char tmpl_arg0[64];
  snprintf(tmpl_arg0, sizeof(tmpl_arg0), "\tmovq\t%%0, %s\n", arg0);
  char tmpl_arg1[64];
  snprintf(tmpl_arg1, sizeof(tmpl_arg1), "\tmovq\t%%0, %s\n", arg1);
  if (length_reg->reg_id == arg0_id) {
    /* length_reg is in arg0, which will be overwritten by addr_reg. Move length
     * first. */
    {
      Register_t *u[] = {length_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg1);
    }
    {
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg0);
    }
  } else if (addr_reg->reg_id == arg1_id) {
    /* addr_reg is in arg1, which is the destination for length_reg. Move addr
     * first. */
    {
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg0);
    }
    {
      Register_t *u[] = {length_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg1);
    }
  } else {
    /* No conflict, move in standard order */
    {
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg0);
    }
    {
      Register_t *u[] = {length_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg1);
    }
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list =
      codegen_call_with_shadow_space(inst_list, "kgpc_string_setlength");

  free_reg(get_reg_stack(), addr_reg);
  free_reg(get_reg_stack(), length_reg);
  free_arg_regs();
  return inst_list;
}

ListNode_t *codegen_builtin_setlength_unicodestring(struct Statement *stmt,
                                                    ListNode_t *inst_list,
                                                    CodeGenContext *ctx) {
  assert(stmt != NULL);
  if (ctx == NULL)
    return inst_list;

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  if (args_expr == NULL || args_expr->next == NULL) {
    fprintf(stderr, "ERROR: SetLength expects two arguments.\n");
    return inst_list;
  }

  struct Expression *target_expr = (struct Expression *)args_expr->cur;
  struct Expression *len_expr = (struct Expression *)args_expr->next->cur;
  if (target_expr == NULL || len_expr == NULL)
    return inst_list;

  if (!codegen_expr_is_addressable(target_expr)) {
    fprintf(stderr,
            "ERROR: SetLength UnicodeString target must be addressable.\n");
    return inst_list;
  }

  int len_has_function_call = expr_contains_function_call(len_expr);
  StackNode_t *addr_spill_slot = NULL;

  Register_t *addr_reg = NULL;
  inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
  if (codegen_had_error(ctx) || addr_reg == NULL)
    return inst_list;

  if (len_has_function_call) {
    addr_spill_slot = add_l_t("setlength_unicode_addr_spill");
    if (addr_spill_slot != NULL) {
      /* Integrated: store to the frame slot through the backend vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                       {.mem_frame = {BE_BASE_FP, -(long long)(addr_spill_slot->offset)}}};
      BeOperand a = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
      kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
      inst_list = em.list;
    }
  }

  Register_t *length_reg = NULL;
  inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);
  if (codegen_had_error(ctx) || length_reg == NULL) {
    if (length_reg != NULL)
      free_reg(get_reg_stack(), length_reg);
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  }

  if (addr_spill_slot != NULL) {
    /* Integrated: load from the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
    BeOperand src = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(addr_spill_slot->offset)}}};
    kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
    inst_list = em.list;
  }

  if (!expr_uses_qword_kgpctype(len_expr))
    inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32,
                                           length_reg->bit_64);

  const char *arg0 = current_arg_reg64(0);
  const char *arg1 = current_arg_reg64(1);
  RegisterId_t arg0_id = codegen_arg_reg_id_num(0);
  RegisterId_t arg1_id = codegen_arg_reg_id_num(1);

  char tmpl_arg0[64];
  snprintf(tmpl_arg0, sizeof(tmpl_arg0), "\tmovq\t%%0, %s\n", arg0);
  char tmpl_arg1[64];
  snprintf(tmpl_arg1, sizeof(tmpl_arg1), "\tmovq\t%%0, %s\n", arg1);
  if (length_reg->reg_id == arg0_id) {
    {
      Register_t *u[] = {length_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg1);
    }
    {
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg0);
    }
  } else if (addr_reg->reg_id == arg1_id) {
    {
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg0);
    }
    {
      Register_t *u[] = {length_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg1);
    }
  } else {
    {
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg0);
    }
    {
      Register_t *u[] = {length_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg1);
    }
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list =
      codegen_call_with_shadow_space(inst_list, "kgpc_unicodestring_setlength");

  free_reg(get_reg_stack(), addr_reg);
  free_reg(get_reg_stack(), length_reg);
  free_arg_regs();
  return inst_list;
}

ListNode_t *codegen_builtin_setlength_shortstring(struct Statement *stmt,
                                                  ListNode_t *inst_list,
                                                  CodeGenContext *ctx) {
  assert(stmt != NULL);
  if (ctx == NULL)
    return inst_list;

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  if (args_expr == NULL || args_expr->next == NULL) {
    fprintf(stderr, "ERROR: SetLength expects two arguments.\n");
    return inst_list;
  }

  struct Expression *target_expr = (struct Expression *)args_expr->cur;
  struct Expression *len_expr = (struct Expression *)args_expr->next->cur;
  if (target_expr == NULL || len_expr == NULL)
    return inst_list;

  if (!codegen_expr_is_addressable(target_expr)) {
    fprintf(stderr,
            "ERROR: SetLength shortstring target must be addressable.\n");
    return inst_list;
  }

  Register_t *addr_reg = NULL;
  inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
  if (codegen_had_error(ctx) || addr_reg == NULL)
    return inst_list;

  Register_t *length_reg = NULL;
  inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);
  if (codegen_had_error(ctx) || length_reg == NULL) {
    if (length_reg != NULL)
      free_reg(get_reg_stack(), length_reg);
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  }

  if (!expr_uses_qword_kgpctype(len_expr))
    inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32,
                                           length_reg->bit_64);

  const char *arg0 = current_arg_reg64(0);
  const char *arg1 = current_arg_reg64(1);
  RegisterId_t arg0_id = codegen_arg_reg_id_num(0);
  RegisterId_t arg1_id = codegen_arg_reg_id_num(1);

  char tmpl_arg0[64];
  snprintf(tmpl_arg0, sizeof(tmpl_arg0), "\tmovq\t%%0, %s\n", arg0);
  char tmpl_arg1[64];
  snprintf(tmpl_arg1, sizeof(tmpl_arg1), "\tmovq\t%%0, %s\n", arg1);
  if (length_reg->reg_id == arg0_id) {
    {
      Register_t *u[] = {length_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg1);
    }
    {
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg0);
    }
  } else if (addr_reg->reg_id == arg1_id) {
    {
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg0);
    }
    {
      Register_t *u[] = {length_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg1);
    }
  } else {
    {
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg0);
    }
    {
      Register_t *u[] = {length_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_arg1);
    }
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list =
      codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_setlength");

  free_reg(get_reg_stack(), addr_reg);
  free_reg(get_reg_stack(), length_reg);
  free_arg_regs();
  return inst_list;
}

ListNode_t *codegen_builtin_setstring(struct Statement *stmt,
                                      ListNode_t *inst_list,
                                      CodeGenContext *ctx) {
  assert(stmt != NULL);
  if (ctx == NULL)
    return inst_list;

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  if (args_expr == NULL || args_expr->next == NULL ||
      args_expr->next->next == NULL) {
    fprintf(stderr, "ERROR: SetString expects three arguments.\n");
    return inst_list;
  }

  struct Expression *target_expr = (struct Expression *)args_expr->cur;
  struct Expression *buffer_expr = (struct Expression *)args_expr->next->cur;
  struct Expression *len_expr = (struct Expression *)args_expr->next->next->cur;
  if (target_expr == NULL || buffer_expr == NULL || len_expr == NULL)
    return inst_list;

  if (!codegen_expr_is_addressable(target_expr)) {
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
  if (addr_slot == NULL) {
    free_reg(get_reg_stack(), addr_reg);
    codegen_report_error(
        ctx, "ERROR: Unable to allocate spill slot for SetString target.");
    return inst_list;
  }
  char buffer[128];
  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(addr_slot->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }
  free_reg(get_reg_stack(), addr_reg);
  addr_reg = NULL;

  /* Get buffer pointer value */
  Register_t *buffer_reg = NULL;
  inst_list =
      codegen_expr_with_result(buffer_expr, inst_list, ctx, &buffer_reg);
  if (codegen_had_error(ctx) || buffer_reg == NULL) {
    if (buffer_reg != NULL)
      free_reg(get_reg_stack(), buffer_reg);
    return inst_list;
  }

  /* For PUnicodeChar(Chars) where Chars is a dynamic array, the
   * Model A descriptor layout puts `data` at offset 0 of the variable's
   * storage. The expression-tree TYPECAST leaf path in gencode_case0
   * already loads `descriptor.data` for that POINTER cast, and a regular
   * 8-byte load of a dynarray variable also yields `data` (offset 0).
   * No extra dereference is required here. */

  /* Spill buffer_reg */
  StackNode_t *buffer_slot = codegen_alloc_temp_slot("setstring_buf");
  if (buffer_slot == NULL) {
    free_reg(get_reg_stack(), buffer_reg);
    codegen_report_error(
        ctx, "ERROR: Unable to allocate spill slot for SetString buffer.");
    return inst_list;
  }
  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(buffer_slot->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = buffer_reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }
  free_reg(get_reg_stack(), buffer_reg);
  buffer_reg = NULL;

  /* Get length value */
  Register_t *length_reg = NULL;
  inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);
  if (codegen_had_error(ctx) || length_reg == NULL) {
    if (length_reg != NULL)
      free_reg(get_reg_stack(), length_reg);
    return inst_list;
  }

  /* Sign-extend length to 64-bit if needed */
  if (!expr_uses_qword_kgpctype(len_expr))
    inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32,
                                           length_reg->bit_64);

  const char *call_target = "kgpc_setstring";
  if (target_expr != NULL && target_expr->resolved_kgpc_type == NULL &&
      target_expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL) {
    HashNode_t *sym_node = NULL;
    if (FindSymbol(&sym_node, ctx->symtab, target_expr->expr_data.id) != 0 &&
        sym_node != NULL && sym_node->type != NULL) {
      target_expr->resolved_kgpc_type = sym_node->type;
    }
  }
  if (target_expr != NULL && target_expr->resolved_kgpc_type != NULL) {
    int is_wide = kgpc_type_is_wide_string(target_expr->resolved_kgpc_type);
    if (!is_wide && target_expr->resolved_kgpc_type->type_alias != NULL) {
      const char *alias_name =
          target_expr->resolved_kgpc_type->type_alias->alias_name;
      const char *target_name =
          target_expr->resolved_kgpc_type->type_alias->target_type_id;
      if ((alias_name != NULL &&
           (pascal_identifier_equals(alias_name, "UnicodeString") ||
            pascal_identifier_equals(alias_name, "WideString"))) ||
          (target_name != NULL &&
           (pascal_identifier_equals(target_name, "UnicodeString") ||
            pascal_identifier_equals(target_name, "WideString")))) {
        is_wide = 1;
      }
    }
    if (!is_wide && target_expr->type == EXPR_VAR_ID &&
        target_expr->expr_data.id != NULL &&
        pascal_identifier_equals(target_expr->expr_data.id, "Result") &&
        ctx != NULL && ctx->symtab != NULL) {
      const char *sub_id = ctx->current_subprogram_mangled;
      if (sub_id == NULL || sub_id[0] == '\0')
        sub_id = ctx->current_subprogram_id;
      if (sub_id != NULL) {
        HashNode_t *sub_node = NULL;
        if (FindSymbol(&sub_node, ctx->symtab, sub_id) != 0 &&
            sub_node != NULL && sub_node->type != NULL &&
            kgpc_type_is_procedure(sub_node->type)) {
          const char *ret_id = sub_node->type->info.proc_info.return_type_id;
          if (ret_id != NULL &&
              (pascal_identifier_equals(ret_id, "WideString") ||
               pascal_identifier_equals(ret_id, "UnicodeString"))) {
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
  const char *arg0 = current_arg_reg64(0); /* %rdi or %rcx */
  const char *arg1 = current_arg_reg64(1); /* %rsi or %rdx */
  const char *arg2 = current_arg_reg64(2); /* %rdx or %r8 */

  /* Move length to arg2 first (it's in a register) */
  {
    char tmpl[64];
    snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, %s\n", arg2);
    Register_t *u[] = {length_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
  }
  free_reg(get_reg_stack(), length_reg);

  /* Reload buffer and addr from spill slots */
  snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
           buffer_slot->offset, arg1);
  inst_list = add_inst(inst_list, buffer);
  snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
           addr_slot->offset, arg0);
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

ListNode_t *codegen_builtin_str(struct Statement *stmt, ListNode_t *inst_list,
                                CodeGenContext *ctx) {
  if (stmt == NULL || ctx == NULL)
    return inst_list;

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  if (args_expr == NULL || args_expr->next == NULL) {
    fprintf(stderr, "ERROR: Str expects two arguments.\n");
    return inst_list;
  }

  struct Expression *value_expr = (struct Expression *)args_expr->cur;
  struct Expression *target_expr = (struct Expression *)args_expr->next->cur;

  /* Check if target is ShortString */
  int target_is_shortstring = codegen_expr_is_shortstring_array(target_expr);
  int target_shortstring_capacity = 256;
  if (target_is_shortstring)
    target_shortstring_capacity =
        codegen_get_shortstring_capacity(target_expr, ctx);

  Register_t *value_reg = NULL;
  inst_list = codegen_expr_with_result(value_expr, inst_list, ctx, &value_reg);
  if (codegen_had_error(ctx) || value_reg == NULL)
    return inst_list;

  int value_is_real =
      (value_expr != NULL && expr_has_type_tag(value_expr, REAL_TYPE));
  int has_width = (value_expr != NULL && value_expr->field_width != NULL);
  int has_precision =
      (value_expr != NULL && value_expr->field_precision != NULL);
  Register_t *width_reg = NULL;
  Register_t *precision_reg = NULL;

  if (has_width) {
    inst_list = codegen_expr_with_result(value_expr->field_width, inst_list,
                                         ctx, &width_reg);
    if (codegen_had_error(ctx) || width_reg == NULL) {
      free_reg(get_reg_stack(), value_reg);
      return inst_list;
    }
    if (!expr_uses_qword_kgpctype(value_expr->field_width))
      inst_list = codegen_sign_extend32_to64(inst_list, width_reg->bit_32,
                                             width_reg->bit_64);
  }
  if (has_precision) {
    inst_list = codegen_expr_with_result(value_expr->field_precision, inst_list,
                                         ctx, &precision_reg);
    if (codegen_had_error(ctx) || precision_reg == NULL) {
      if (width_reg != NULL)
        free_reg(get_reg_stack(), width_reg);
      free_reg(get_reg_stack(), value_reg);
      return inst_list;
    }
    if (!expr_uses_qword_kgpctype(value_expr->field_precision))
      inst_list = codegen_sign_extend32_to64(inst_list, precision_reg->bit_32,
                                             precision_reg->bit_64);
  }
  if (!value_is_real && !expr_uses_qword_kgpctype(value_expr))
    inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32,
                                           value_reg->bit_64);

  if (!codegen_expr_is_addressable(target_expr)) {
    codegen_report_error(ctx, "ERROR: Str output must be addressable.");
    free_reg(get_reg_stack(), value_reg);
    return inst_list;
  }

  Register_t *addr_reg = NULL;
  inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
  if (codegen_had_error(ctx) || addr_reg == NULL) {
    free_reg(get_reg_stack(), value_reg);
    return inst_list;
  }

  char buffer[128];
  const char *shortstring_suffix = target_is_shortstring ? "_shortstring" : "";

  if (value_is_real) {
    int value_is_unknown_byref = 0;
    if (value_expr != NULL && value_expr->type == EXPR_VAR_ID &&
        value_expr->expr_data.id != NULL &&
        expr_get_type_tag(value_expr) == UNKNOWN_TYPE && ctx->symtab != NULL) {
      HashNode_t *value_sym = NULL;
      if (FindSymbol(&value_sym, ctx->symtab, value_expr->expr_data.id) != 0 &&
          value_sym != NULL && value_sym->is_var_parameter) {
        value_is_unknown_byref = 1;
      }
    }

    if (value_is_unknown_byref) {
      Register_t *value_addr_reg = NULL;
      inst_list =
          codegen_address_for_expr(value_expr, inst_list, ctx, &value_addr_reg);
      if (codegen_had_error(ctx) || value_addr_reg == NULL) {
        free_reg(get_reg_stack(), addr_reg);
        free_reg(get_reg_stack(), value_reg);
        if (width_reg != NULL)
          free_reg(get_reg_stack(), width_reg);
        if (precision_reg != NULL)
          free_reg(get_reg_stack(), precision_reg);
        return inst_list;
      }
      if (codegen_target_is_windows()) {
        Register_t *u[] = {value_addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
      } else {
        Register_t *u[] = {value_addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
      }
      inst_list = codegen_vect_reg(inst_list, 0);
      inst_list = codegen_call_with_shadow_space(inst_list,
                                                 "kgpc_load_extended_to_bits");
      free_arg_regs();
      free_reg(get_reg_stack(), value_addr_reg);
      inst_list = add_inst(inst_list, "\tmovq\t%rax, %xmm0\n");
    } else {
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %xmm0\n");
      }
    }
    if (has_width || has_precision) {
      if (width_reg == NULL) {
        width_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (width_reg == NULL) {
          free_reg(get_reg_stack(), addr_reg);
          free_reg(get_reg_stack(), value_reg);
          if (precision_reg != NULL)
            free_reg(get_reg_stack(), precision_reg);
          return inst_list;
        }
        {
          Register_t *d[] = {width_reg};
          inst_list =
              add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t$0, %0\n");
        }
      }
      if (precision_reg == NULL) {
        precision_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (precision_reg == NULL) {
          free_reg(get_reg_stack(), addr_reg);
          free_reg(get_reg_stack(), value_reg);
          free_reg(get_reg_stack(), width_reg);
          return inst_list;
        }
        {
          Register_t *d[] = {precision_reg};
          inst_list =
              add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t$-1, %0\n");
        }
      }
      if (codegen_target_is_windows()) {
        if (target_is_shortstring) {
          {
            Register_t *u[] = {width_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdx\n");
          }
          {
            Register_t *u[] = {addr_reg};
            inst_list =
                add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %r8\n");
          }
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r9\n",
                   target_shortstring_capacity);
          inst_list = add_inst(inst_list, buffer);
          inst_list = add_inst(inst_list, "\tshlq\t$32, %r9\n");
          {
            Register_t *u[] = {precision_reg};
            inst_list =
                add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\torq\t%0, %r9\n");
          }
        } else {
          /* Move addr_reg to R9 first to avoid clobbering if addr_reg is R8 */
          {
            Register_t *u[] = {addr_reg};
            inst_list =
                add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %r9\n");
          }
          {
            Register_t *u[] = {width_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdx\n");
          }
          {
            Register_t *u[] = {precision_reg};
            inst_list =
                add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %r8\n");
          }
        }
      } else {
        if (target_is_shortstring) {
          {
            Register_t *u[] = {width_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdi\n");
          }
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rsi\n");
          }
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n",
                   target_shortstring_capacity);
          inst_list = add_inst(inst_list, buffer);
          inst_list = add_inst(inst_list, "\tshlq\t$32, %rdx\n");
          {
            Register_t *u[] = {precision_reg};
            inst_list =
                add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\torq\t%0, %rdx\n");
          }
        } else {
          /* Move addr_reg to RDX first to avoid clobbering if addr_reg is RSI
           */
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdx\n");
          }
          {
            Register_t *u[] = {width_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdi\n");
          }
          {
            Register_t *u[] = {precision_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rsi\n");
          }
        }
      }
      inst_list = codegen_vect_reg(inst_list, 0);
      if (target_is_shortstring)
        snprintf(buffer, sizeof(buffer),
                 "\tcall\tkgpc_str_real_fmt_bounded_shortstring\n");
      else
        snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_str_real_fmt%s\n",
                 shortstring_suffix);
      inst_list = add_inst(inst_list, buffer);
    } else {
      if (codegen_target_is_windows()) {
        if (target_is_shortstring) {
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdx\n");
          }
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n",
                   target_shortstring_capacity);
          inst_list = add_inst(inst_list, buffer);
        } else {
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdx\n");
          }
        }
      } else {
        if (target_is_shortstring) {
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdi\n");
          }
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rsi\n",
                   target_shortstring_capacity);
          inst_list = add_inst(inst_list, buffer);
        } else {
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdi\n");
          }
        }
      }
      inst_list = codegen_vect_reg(inst_list, 0);
      if (target_is_shortstring)
        snprintf(buffer, sizeof(buffer),
                 "\tcall\tkgpc_str_real_bounded_shortstring\n");
      else
        snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_str_real%s\n",
                 shortstring_suffix);
      inst_list = add_inst(inst_list, buffer);
    }
  } else {
    if (has_width) {
      if (codegen_target_is_windows()) {
        {
          Register_t *u[] = {value_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
        }
        {
          Register_t *u[] = {width_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
        }
        if (target_is_shortstring) {
          {
            Register_t *u[] = {addr_reg};
            inst_list =
                add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %r8\n");
          }
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r9\n",
                   target_shortstring_capacity);
          inst_list = add_inst(inst_list, buffer);
        } else {
          {
            Register_t *u[] = {addr_reg};
            inst_list =
                add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %r8\n");
          }
        }
      } else {
        {
          Register_t *u[] = {value_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
        }
        {
          Register_t *u[] = {width_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
        }
        if (target_is_shortstring) {
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdx\n");
          }
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rcx\n",
                   target_shortstring_capacity);
          inst_list = add_inst(inst_list, buffer);
        } else {
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdx\n");
          }
        }
      }
      inst_list = codegen_vect_reg(inst_list, 0);
      if (target_is_shortstring)
        snprintf(buffer, sizeof(buffer),
                 "\tcall\tkgpc_str_int64_fmt_bounded_shortstring\n");
      else
        snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_str_int64_fmt%s\n",
                 shortstring_suffix);
      inst_list = add_inst(inst_list, buffer);
    } else {
      if (codegen_target_is_windows()) {
        {
          Register_t *u[] = {value_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
        }
        if (target_is_shortstring) {
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdx\n");
          }
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n",
                   target_shortstring_capacity);
          inst_list = add_inst(inst_list, buffer);
        } else {
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdx\n");
          }
        }
      } else {
        {
          Register_t *u[] = {value_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
        }
        if (target_is_shortstring) {
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rsi\n");
          }
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n",
                   target_shortstring_capacity);
          inst_list = add_inst(inst_list, buffer);
        } else {
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rsi\n");
          }
        }
      }

      inst_list = codegen_vect_reg(inst_list, 0);
      if (target_is_shortstring)
        snprintf(buffer, sizeof(buffer),
                 "\tcall\tkgpc_str_int64_bounded_shortstring\n");
      else
        snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_str_int64%s\n",
                 shortstring_suffix);
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
ListNode_t *codegen_builtin_writestr(struct Statement *stmt,
                                     ListNode_t *inst_list,
                                     CodeGenContext *ctx) {
  if (stmt == NULL || ctx == NULL)
    return inst_list;

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  if (args_expr == NULL) {
    fprintf(stderr, "ERROR: WriteStr requires at least one argument.\n");
    return inst_list;
  }

  /* First argument is the target string variable */
  struct Expression *target_expr = (struct Expression *)args_expr->cur;
  if (!codegen_expr_is_addressable(target_expr)) {
    codegen_report_error(ctx, "ERROR: WriteStr target must be addressable.");
    return inst_list;
  }

  /* Save target address to stack */
  Register_t *target_reg = NULL;
  inst_list =
      codegen_address_for_expr(target_expr, inst_list, ctx, &target_reg);
  if (codegen_had_error(ctx) || target_reg == NULL)
    return inst_list;

  StackNode_t *target_slot =
      add_l_x("__writestr_target__", CODEGEN_POINTER_SIZE_BYTES);
  if (target_slot == NULL) {
    free_reg(get_reg_stack(), target_reg);
    return inst_list;
  }
  char buffer[128];
  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(target_slot->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = target_reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }
  free_reg(get_reg_stack(), target_reg);

  /* Create accumulator string slot initialized to empty string */
  StackNode_t *accum_slot =
      add_l_x("__writestr_accum__", CODEGEN_POINTER_SIZE_BYTES);
  if (accum_slot == NULL)
    return inst_list;

  /* Initialize accumulator to empty string - use correct ABI for the platform
   */
  if (codegen_target_is_windows()) {
    inst_list = add_inst(inst_list, "\txorq\t%rcx, %rcx\n");
  } else {
    inst_list = add_inst(inst_list, "\txorq\t%rdi, %rdi\n");
  }
  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list =
      codegen_call_with_shadow_space(inst_list, "kgpc_string_duplicate");
  free_arg_regs();
  snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n",
           accum_slot->offset);
  inst_list = add_inst(inst_list, buffer);

  /* Process remaining arguments */
  args_expr = args_expr->next;
  while (args_expr != NULL) {
    struct Expression *arg_expr = (struct Expression *)args_expr->cur;
    int arg_type = expr_get_type_tag(arg_expr);

    /* Generate code to format this argument to a string */
    Register_t *value_reg = NULL;
    inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &value_reg);
    if (codegen_had_error(ctx) || value_reg == NULL) {
      if (value_reg != NULL)
        free_reg(get_reg_stack(), value_reg);
      return inst_list;
    }

    const char *format_func = NULL;
    if (is_integer_type(arg_type) || arg_type == ENUM_TYPE)
      format_func = "kgpc_int_to_str";
    else if (arg_type == STRING_TYPE)
      format_func = "kgpc_string_duplicate"; /* Just duplicate the string */
    else if (arg_type == CHAR_TYPE)
      format_func = "kgpc_char_to_str";
    else if (arg_type == BOOL)
      format_func = "kgpc_bool_to_str";
    else if (arg_type == REAL_TYPE)
      format_func = "kgpc_real_to_str";
    else {
      /* Default to int */
      format_func = "kgpc_int_to_str";
    }

    /* Call format function: result = format_func(value) */
    if (codegen_target_is_windows()) {
      Register_t *u[] = {value_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
    } else {
      Register_t *u[] = {value_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
    }
    free_reg(get_reg_stack(), value_reg);

    inst_list = codegen_vect_reg(inst_list, 0);
    snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", format_func);
    inst_list = add_inst(inst_list, buffer);
    free_arg_regs();

    /* Save formatted string temporarily */
    StackNode_t *temp_slot =
        add_l_x("__writestr_temp__", CODEGEN_POINTER_SIZE_BYTES);
    if (temp_slot == NULL)
      return inst_list;
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n",
             temp_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    /* Concatenate: accum = concat(accum, temp) */
    if (codegen_target_is_windows()) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rcx\n",
               accum_slot->offset);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n",
               temp_slot->offset);
      inst_list = add_inst(inst_list, buffer);
    } else {
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdi\n",
               accum_slot->offset);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rsi\n",
               temp_slot->offset);
      inst_list = add_inst(inst_list, buffer);
    }
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_concat");
    free_arg_regs();

    /* Update accumulator */
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n",
             accum_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    args_expr = args_expr->next;
  }

  /* Store result in target: *target = accum */
  snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n",
           accum_slot->offset);
  inst_list = add_inst(inst_list, buffer);
  snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%r11\n",
           target_slot->offset);
  inst_list = add_inst(inst_list, buffer);
  inst_list = add_inst(inst_list, "\tmovq\t%rax, (%r11)\n");

  return inst_list;
}

ListNode_t *codegen_builtin_insert(struct Statement *stmt,
                                   ListNode_t *inst_list, CodeGenContext *ctx) {
  if (stmt == NULL || ctx == NULL)
    return inst_list;

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  if (args_expr == NULL || args_expr->next == NULL ||
      args_expr->next->next == NULL) {
    fprintf(stderr, "ERROR: Insert expects three arguments.\n");
    return inst_list;
  }

  char buffer[128];
  struct Expression *source_expr = (struct Expression *)args_expr->cur;
  struct Expression *target_expr = (struct Expression *)args_expr->next->cur;
  struct Expression *index_expr =
      (struct Expression *)args_expr->next->next->cur;

  int target_is_shortstring = codegen_expr_is_shortstring_array(target_expr);
  int source_is_shortstring = codegen_expr_is_shortstring_array(source_expr);

  Register_t *source_reg = NULL;
  int source_is_char =
      (source_expr != NULL && expr_get_type_tag(source_expr) == CHAR_TYPE);
  StackNode_t *char_buffer = NULL;
  if (target_is_shortstring && source_is_shortstring) {
    if (!codegen_expr_is_addressable(source_expr)) {
      codegen_report_error(
          ctx, "ERROR: Insert shortstring source must be addressable.");
      return inst_list;
    }
    inst_list =
        codegen_address_for_expr(source_expr, inst_list, ctx, &source_reg);
    if (codegen_had_error(ctx) || source_reg == NULL)
      return inst_list;
  } else {
    inst_list =
        codegen_expr_with_result(source_expr, inst_list, ctx, &source_reg);
    if (codegen_had_error(ctx) || source_reg == NULL)
      return inst_list;
  }

  if (source_is_char) {
    char_buffer = add_l_x("insert_char_buffer", 2);
    if (char_buffer == NULL) {
      free_reg(get_reg_stack(), source_reg);
      return codegen_fail_register(
          ctx, inst_list, NULL,
          "ERROR: Unable to allocate spill slot for Insert char source.");
    }

    const char *byte_reg = register_name8(source_reg);
    if (byte_reg == NULL) {
      free_reg(get_reg_stack(), source_reg);
      return codegen_fail_register(
          ctx, inst_list, NULL,
          "ERROR: Unable to acquire byte register for Insert char source.");
    }

    if (target_is_shortstring) {
      snprintf(buffer, sizeof(buffer), "\tmovb\t$1, -%d(%%rbp)\n",
               char_buffer->offset);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", byte_reg,
               char_buffer->offset - 1);
      inst_list = add_inst(inst_list, buffer);
      {
        char tmpl[64];
        snprintf(tmpl, sizeof(tmpl), "\tleaq\t-%d(%%rbp), %%0\n",
                 char_buffer->offset);
        Register_t *d[] = {source_reg};
        inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
      }
    } else {
      snprintf(buffer, sizeof(buffer), "\tmovb\t$0, -%d(%%rbp)\n",
               char_buffer->offset - 1);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", byte_reg,
               char_buffer->offset);
      inst_list = add_inst(inst_list, buffer);
      {
        char tmpl[64];
        snprintf(tmpl, sizeof(tmpl), "\tleaq\t-%d(%%rbp), %%0\n",
                 char_buffer->offset);
        Register_t *d[] = {source_reg};
        inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
      }
    }
  }

  StackNode_t *source_temp = add_l_t("insert_source");
  if (source_temp == NULL) {
    free_reg(get_reg_stack(), source_reg);
    return codegen_fail_register(
        ctx, inst_list, NULL,
        "ERROR: Unable to allocate spill slot for Insert source.");
  }
  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(source_temp->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = source_reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }
  free_reg(get_reg_stack(), source_reg);

  if (!codegen_expr_is_addressable(target_expr)) {
    codegen_report_error(ctx, "ERROR: Insert target must be addressable.");
    return inst_list;
  }

  Register_t *target_reg = NULL;
  inst_list =
      codegen_address_for_expr(target_expr, inst_list, ctx, &target_reg);
  if (codegen_had_error(ctx) || target_reg == NULL)
    return inst_list;

  StackNode_t *target_temp = add_l_t("insert_target");
  if (target_temp == NULL) {
    free_reg(get_reg_stack(), target_reg);
    return codegen_fail_register(
        ctx, inst_list, NULL,
        "ERROR: Unable to allocate spill slot for Insert target.");
  }

  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(target_temp->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = target_reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }
  free_reg(get_reg_stack(), target_reg);

  Register_t *index_reg = NULL;
  inst_list = codegen_expr_with_result(index_expr, inst_list, ctx, &index_reg);
  if (codegen_had_error(ctx) || index_reg == NULL)
    return inst_list;

  if (!expr_uses_qword_kgpctype(index_expr))
    inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32,
                                           index_reg->bit_64);

  StackNode_t *index_temp = add_l_t("insert_index");
  if (index_temp == NULL) {
    free_reg(get_reg_stack(), index_reg);
    return codegen_fail_register(
        ctx, inst_list, NULL,
        "ERROR: Unable to allocate spill slot for Insert index.");
  }

  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(index_temp->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = index_reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }
  free_reg(get_reg_stack(), index_reg);

  const char *arg0 = current_arg_reg64(0);
  const char *arg1 = current_arg_reg64(1);
  const char *arg2 = current_arg_reg64(2);
  const char *arg3 = current_arg_reg64(3);

  snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
           source_temp->offset, arg0);
  inst_list = add_inst(inst_list, buffer);
  snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
           target_temp->offset, arg1);
  inst_list = add_inst(inst_list, buffer);
  snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
           index_temp->offset, arg2);
  inst_list = add_inst(inst_list, buffer);

  inst_list = codegen_vect_reg(inst_list, 0);
  if (target_is_shortstring) {
    int value_is_shortstring =
        (source_is_shortstring || source_is_char) ? 1 : 0;
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %s\n", value_is_shortstring,
             arg3);
    inst_list = add_inst(inst_list, buffer);
    inst_list =
        codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_insert");
  } else {
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_insert");
  }
  free_arg_regs();
  return inst_list;
}

ListNode_t *codegen_builtin_delete(struct Statement *stmt,
                                   ListNode_t *inst_list, CodeGenContext *ctx) {
  if (stmt == NULL || ctx == NULL)
    return inst_list;

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  if (args_expr == NULL || args_expr->next == NULL ||
      args_expr->next->next == NULL) {
    fprintf(stderr, "ERROR: Delete expects three arguments.\n");
    return inst_list;
  }

  struct Expression *target_expr = (struct Expression *)args_expr->cur;
  struct Expression *index_expr = (struct Expression *)args_expr->next->cur;
  struct Expression *count_expr =
      (struct Expression *)args_expr->next->next->cur;

  int target_is_shortstring = codegen_expr_is_shortstring_array(target_expr);

  if (!codegen_expr_is_addressable(target_expr)) {
    codegen_report_error(ctx, "ERROR: Delete target must be addressable.");
    return inst_list;
  }

  Register_t *addr_reg = NULL;
  inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
  if (codegen_had_error(ctx) || addr_reg == NULL)
    return inst_list;

  StackNode_t *string_temp = add_l_t("delete_target");
  if (string_temp == NULL) {
    free_reg(get_reg_stack(), addr_reg);
    return codegen_fail_register(
        ctx, inst_list, NULL,
        "ERROR: Unable to allocate spill slot for Delete target.");
  }

  char buffer[128];
  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(string_temp->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }
  free_reg(get_reg_stack(), addr_reg);

  Register_t *index_reg = NULL;
  inst_list = codegen_expr_with_result(index_expr, inst_list, ctx, &index_reg);
  if (codegen_had_error(ctx) || index_reg == NULL)
    return inst_list;

  if (!expr_uses_qword_kgpctype(index_expr))
    inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32,
                                           index_reg->bit_64);

  StackNode_t *index_temp = add_l_t("delete_index");
  if (index_temp == NULL) {
    free_reg(get_reg_stack(), index_reg);
    return codegen_fail_register(
        ctx, inst_list, NULL,
        "ERROR: Unable to allocate spill slot for Delete index.");
  }

  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(index_temp->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = index_reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }
  free_reg(get_reg_stack(), index_reg);

  Register_t *count_reg = NULL;
  inst_list = codegen_expr_with_result(count_expr, inst_list, ctx, &count_reg);
  if (codegen_had_error(ctx) || count_reg == NULL)
    return inst_list;

  if (!expr_uses_qword_kgpctype(count_expr))
    inst_list = codegen_sign_extend32_to64(inst_list, count_reg->bit_32,
                                           count_reg->bit_64);

  StackNode_t *count_temp = add_l_t("delete_count");
  if (count_temp == NULL) {
    free_reg(get_reg_stack(), count_reg);
    return codegen_fail_register(
        ctx, inst_list, NULL,
        "ERROR: Unable to allocate spill slot for Delete count.");
  }

  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(count_temp->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = count_reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }
  free_reg(get_reg_stack(), count_reg);

  const char *arg0 = current_arg_reg64(0);
  const char *arg1 = current_arg_reg64(1);
  const char *arg2 = current_arg_reg64(2);

  snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
           string_temp->offset, arg0);
  inst_list = add_inst(inst_list, buffer);
  snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
           index_temp->offset, arg1);
  inst_list = add_inst(inst_list, buffer);
  snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
           count_temp->offset, arg2);
  inst_list = add_inst(inst_list, buffer);

  inst_list = codegen_vect_reg(inst_list, 0);
  if (target_is_shortstring)
    inst_list =
        codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_delete");
  else
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_delete");
  free_arg_regs();
  return inst_list;
}

typedef struct {
  int type_tag;
  const char *regular;
  const char *shortstring;
} ValHelperEntry;

static const ValHelperEntry val_helper_table[] = {
    {BYTE_TYPE, "kgpc_val_integer", "kgpc_val_integer_ss"},
    {WORD_TYPE, "kgpc_val_integer", "kgpc_val_integer_ss"},
    {INT_TYPE, "kgpc_val_integer", "kgpc_val_integer_ss"},
    {LONGWORD_TYPE, "kgpc_val_integer", "kgpc_val_integer_ss"},
    {CHAR_TYPE, "kgpc_val_longint", "kgpc_val_longint_ss"},
    {BOOL, "kgpc_val_longint", "kgpc_val_longint_ss"},
    {LONGINT_TYPE, "kgpc_val_longint", "kgpc_val_longint_ss"},
    {INT64_TYPE, "kgpc_val_longint", "kgpc_val_longint_ss"},
    {QWORD_TYPE, "kgpc_val_qword", "kgpc_val_qword_ss"},
    {REAL_TYPE, "kgpc_val_real", "kgpc_val_real_ss"},
    {EXTENDED_TYPE, "kgpc_val_extended", "kgpc_val_extended_ss"},
};

ListNode_t *codegen_builtin_val(struct Statement *stmt, ListNode_t *inst_list,
                                CodeGenContext *ctx) {
  if (stmt == NULL || ctx == NULL)
    return inst_list;

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  int arg_count = ListLength(args_expr);
  if (args_expr == NULL || (arg_count != 2 && arg_count != 3)) {
    codegen_report_error(ctx, "ERROR: Val expects two or three arguments.");
    return inst_list;
  }

  struct Expression *source_expr = (struct Expression *)args_expr->cur;
  struct Expression *value_expr = (struct Expression *)args_expr->next->cur;
  struct Expression *code_expr =
      (arg_count == 3) ? (struct Expression *)args_expr->next->next->cur : NULL;

  Register_t *source_reg = NULL;
  Register_t *value_addr = NULL;
  Register_t *code_addr = NULL;
  StackNode_t *code_spill = NULL;
  StackNode_t *code_result_spill = NULL;
  StackNode_t *value_result_spill = NULL;
  int value_type_tag =
      (value_expr != NULL) ? expr_get_type_tag(value_expr) : UNKNOWN_TYPE;
  int value_store_size = expr_integer_store_size(value_expr);

  int source_is_shortstring = codegen_expr_is_shortstring_array(source_expr);
  if (!source_is_shortstring && source_expr != NULL &&
      expr_get_type_tag(source_expr) == SHORTSTRING_TYPE)
    source_is_shortstring = 1;

  if (source_is_shortstring) {
    /* For ShortString sources, get the address instead of the value */
    inst_list =
        codegen_address_for_expr(source_expr, inst_list, ctx, &source_reg);
  } else {
    inst_list =
        codegen_expr_with_result(source_expr, inst_list, ctx, &source_reg);
  }
  if (codegen_had_error(ctx) || source_reg == NULL)
    goto cleanup;

  inst_list = codegen_address_for_expr(value_expr, inst_list, ctx, &value_addr);
  if (codegen_had_error(ctx) || value_addr == NULL)
    goto cleanup;

  if (code_expr != NULL) {
    inst_list = codegen_address_for_expr(code_expr, inst_list, ctx, &code_addr);
    if (codegen_had_error(ctx) || code_addr == NULL)
      goto cleanup;
  }

  const char *call_target = NULL;
  int target_is_extended = 0;
  if (value_expr != NULL) {
    KgpcType *value_kgpc = expr_get_kgpc_type(value_expr);
    target_is_extended =
        (value_type_tag == EXTENDED_TYPE) ||
        (value_kgpc != NULL && kgpc_type_is_extended(value_kgpc)) ||
        codegen_expr_involves_extended(value_expr);
  }
  for (size_t i = 0; i < sizeof(val_helper_table) / sizeof(val_helper_table[0]);
       i++) {
    if (val_helper_table[i].type_tag == value_type_tag) {
      call_target = source_is_shortstring ? val_helper_table[i].shortstring
                                          : val_helper_table[i].regular;
      break;
    }
  }
  if (value_type_tag == REAL_TYPE && target_is_extended)
    call_target =
        source_is_shortstring ? "kgpc_val_extended_ss" : "kgpc_val_extended";

  if (call_target == NULL) {
    codegen_report_error(
        ctx, "ERROR: Val target must be integer, longint, or real.");
    goto cleanup;
  }

  if (is_integer_type(value_type_tag) || value_type_tag == CHAR_TYPE ||
      value_type_tag == BOOL) {
    value_result_spill = add_l_t("val_value_result");
    if (value_result_spill == NULL) {
      codegen_report_error(
          ctx, "ERROR: Unable to allocate temporary for Val target result.");
      goto cleanup;
    }
  }

  if (code_expr != NULL) {
    code_spill = add_l_t("val_code_ptr");
    if (code_spill == NULL) {
      codegen_report_error(
          ctx, "ERROR: Unable to allocate temporary for Val code argument.");
      goto cleanup;
    }

    {
      /* Integrated: store to the frame slot through the backend vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                       {.mem_frame = {BE_BASE_FP, -(long long)(code_spill->offset)}}};
      BeOperand a = {OPK_VREG, BE_W64, {.vreg = code_addr}};
      kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
      inst_list = em.list;
    }
  }

  char buffer[128];
  if (codegen_target_is_windows()) {
    {
      Register_t *u[] = {source_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
    }
    if (value_result_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rdx\n",
               value_result_spill->offset);
      inst_list = add_inst(inst_list, buffer);
    } else {
      {
        Register_t *u[] = {value_addr};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
      }
    }
  } else {
    {
      Register_t *u[] = {source_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
    }
    if (value_result_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rsi\n",
               value_result_spill->offset);
      inst_list = add_inst(inst_list, buffer);
    } else {
      {
        Register_t *u[] = {value_addr};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
      }
    }
  }
  inst_list = codegen_vect_reg(inst_list, 0);
  snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", call_target);
  inst_list = add_inst(inst_list, buffer);

  /* Save the return value (code) before the value copy block clobbers %rax */
  if (code_expr != NULL) {
    code_result_spill = add_l_t("val_code_result");
    if (code_result_spill == NULL) {
      codegen_report_error(
          ctx, "ERROR: Unable to allocate temporary for Val result.");
      goto cleanup;
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n",
             code_result_spill->offset);
    inst_list = add_inst(inst_list, buffer);
  }

  if (value_result_spill != NULL) {
    {
      Register_t *u[] = {value_addr};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rax\n");
    }

    switch (value_store_size) {
    case 1:
      snprintf(buffer, sizeof(buffer), "\tmovb\t-%d(%%rbp), %%dl\n",
               value_result_spill->offset);
      inst_list = add_inst(inst_list, buffer);
      inst_list = add_inst(inst_list, "\tmovb\t%dl, (%rax)\n");
      break;
    case 2:
      snprintf(buffer, sizeof(buffer), "\tmovw\t-%d(%%rbp), %%dx\n",
               value_result_spill->offset);
      inst_list = add_inst(inst_list, buffer);
      inst_list = add_inst(inst_list, "\tmovw\t%dx, (%rax)\n");
      break;
    case 8:
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n",
               value_result_spill->offset);
      inst_list = add_inst(inst_list, buffer);
      inst_list = add_inst(inst_list, "\tmovq\t%rdx, (%rax)\n");
      break;
    case 4:
    default:
      snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %%edx\n",
               value_result_spill->offset);
      inst_list = add_inst(inst_list, buffer);
      inst_list = add_inst(inst_list, "\tmovl\t%edx, (%rax)\n");
      break;
    }
  }

  if (code_expr != NULL && code_result_spill != NULL) {
    if (expr_uses_qword_kgpctype(code_expr)) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n",
               code_result_spill->offset);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n",
               code_spill->offset);
      inst_list = add_inst(inst_list, buffer);
      inst_list = add_inst(inst_list, "\tmovq\t%rdx, (%rax)\n");
    } else {
      snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %%edx\n",
               code_result_spill->offset);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n",
               code_spill->offset);
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

ListNode_t *codegen_builtin_prefetch(struct Statement *stmt,
                                     ListNode_t *inst_list,
                                     CodeGenContext *ctx) {
  if (stmt == NULL || ctx == NULL)
    return inst_list;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || args->next != NULL || args->cur == NULL) {
    codegen_report_error(ctx, "ERROR: Prefetch expects exactly one argument.");
    return inst_list;
  }

  struct Expression *arg_expr = (struct Expression *)args->cur;
  Register_t *addr_reg = NULL;
  inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &addr_reg);
  if (codegen_had_error(ctx) || addr_reg == NULL)
    return inst_list;

  const char *arg0 = current_arg_reg64(0);
  {
    char tmpl[64];
    snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, %s\n", arg0);
    Register_t *u[] = {addr_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
  }
  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_prefetch");
  free_arg_regs();
  free_reg(get_reg_stack(), addr_reg);
  return inst_list;
}

static int codegen_incdec_type_width_from_tag(int type_tag) {
  switch (type_tag) {
  case BYTE_TYPE:
  case CHAR_TYPE:
  case BOOL:
    return 1;
  case WORD_TYPE:
    return 2;
  case INT64_TYPE:
  case QWORD_TYPE:
  case POINTER_TYPE:
    return 8;
  default:
    return 4;
  }
}

static int codegen_incdec_target_width(const struct Expression *target_expr) {
  if (target_expr != NULL) {
    KgpcType *type = expr_get_kgpc_type(target_expr);
    if (type != NULL) {
      long long size = kgpc_type_sizeof(type);
      if (size == 1 || size == 2 || size == 4 || size == 8)
        return (int)size;
    }
  }

  return codegen_incdec_type_width_from_tag(
      (target_expr != NULL) ? expr_get_type_tag(target_expr) : UNKNOWN_TYPE);
}

static const char *codegen_incdec_reg8(const Register_t *reg) {
  if (reg == NULL)
    return NULL;
  switch (reg->reg_id) {
  case REG_RAX:
    return "%al";
  case REG_RBX:
    return "%bl";
  case REG_RCX:
    return "%cl";
  case REG_RDX:
    return "%dl";
  case REG_RSI:
    return "%sil";
  case REG_RDI:
    return "%dil";
  case REG_R8:
    return "%r8b";
  case REG_R9:
    return "%r9b";
  case REG_R10:
    return "%r10b";
  case REG_R11:
    return "%r11b";
  case REG_R12:
    return "%r12b";
  case REG_R13:
    return "%r13b";
  case REG_R14:
    return "%r14b";
  case REG_R15:
    return "%r15b";
  default:
    return NULL;
  }
}

static const char *codegen_incdec_reg_for_width(const Register_t *reg,
                                                int width) {
  if (reg == NULL)
    return NULL;
  if (width == 8)
    return reg->bit_64;
  if (width == 4)
    return reg->bit_32;
  if (width == 2)
    return codegen_register_name16(reg);
  if (width == 1)
    return codegen_incdec_reg8(reg);
  return reg->bit_32;
}

static const char *codegen_incdec_rax_for_width(int width) {
  if (width == 8)
    return "%rax";
  if (width == 4)
    return "%eax";
  if (width == 2)
    return "%ax";
  if (width == 1)
    return "%al";
  return "%eax";
}

static char codegen_incdec_suffix_for_width(int width) {
  if (width == 8)
    return 'q';
  if (width == 4)
    return 'l';
  if (width == 2)
    return 'w';
  if (width == 1)
    return 'b';
  return 'l';
}

static ListNode_t *codegen_incdec_add_reg_to_mem(ListNode_t *inst_list,
                                                 CodeGenContext *ctx,
                                                 int width,
                                                 Register_t *value_reg,
                                                 const char *mem_operand,
                                                 Register_t **extra_uses,
                                                 int extra_use_count) {
  const char *value_name = codegen_incdec_reg_for_width(value_reg, width);
  char suffix = codegen_incdec_suffix_for_width(width);
  char buffer[160];
  snprintf(buffer, sizeof(buffer), "\tadd%c\t%s, %s\n", suffix, value_name,
           mem_operand);

  Register_t *uses[3] = {value_reg, NULL, NULL};
  int use_count = 1;
  for (int i = 0; i < extra_use_count && use_count < 3; ++i) {
    if (extra_uses[i] != NULL)
      uses[use_count++] = extra_uses[i];
  }
  return add_inst_du(inst_list, ctx, NULL, 0, uses, use_count, buffer);
}

static ListNode_t *codegen_incdec_add_rax_to_mem(ListNode_t *inst_list,
                                                 CodeGenContext *ctx,
                                                 int width,
                                                 const char *mem_operand,
                                                 Register_t **extra_uses,
                                                 int extra_use_count) {
  const char *value_name = codegen_incdec_rax_for_width(width);
  char suffix = codegen_incdec_suffix_for_width(width);
  char buffer[160];
  snprintf(buffer, sizeof(buffer), "\tadd%c\t%s, %s\n", suffix, value_name,
           mem_operand);

  if (extra_use_count > 0)
    return add_inst_du(inst_list, ctx, NULL, 0, extra_uses, extra_use_count,
                       buffer);
  return add_inst(inst_list, buffer);
}

static ListNode_t *codegen_builtin_incdec(struct Statement *stmt,
                                          ListNode_t *inst_list,
                                          CodeGenContext *ctx,
                                          int is_increment) {
  if (stmt == NULL || ctx == NULL)
    return inst_list;

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  if (args_expr == NULL)
    return inst_list;

  struct Expression *target_expr = (struct Expression *)args_expr->cur;
  struct Expression *value_expr =
      (args_expr->next != NULL) ? (struct Expression *)args_expr->next->cur
                                : NULL;

  Register_t *increment_reg = NULL;
  if (value_expr != NULL) {
    inst_list =
        codegen_expr_with_result(value_expr, inst_list, ctx, &increment_reg);
    if (codegen_had_error(ctx) || increment_reg == NULL)
      return inst_list;
  } else {
    increment_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (increment_reg == NULL)
      return inst_list;
    if (target_expr != NULL && expr_uses_qword_kgpctype(target_expr)) {
      Register_t *d[] = {increment_reg};
      inst_list =
          add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t$1, %0\n");
    } else {
      Register_t *d[] = {increment_reg};
      inst_list =
          add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovl\t$1, %0\n");
    }
  }

  int target_type_tag =
      (target_expr != NULL) ? expr_get_type_tag(target_expr) : UNKNOWN_TYPE;
  int target_width = codegen_incdec_target_width(target_expr);
  int target_is_pointer = (target_type_tag == POINTER_TYPE);
  int target_uses_qword = (target_width == 8) || target_is_pointer;
  if (!target_uses_qword && target_expr != NULL)
    target_uses_qword = expr_uses_qword_kgpctype(target_expr);

  if (target_uses_qword &&
      (value_expr == NULL || !expr_uses_qword_kgpctype(value_expr))) {
    if (value_expr != NULL && expr_is_unsigned_type(value_expr))
      inst_list = codegen_zero_extend32_to64(inst_list, increment_reg->bit_32,
                                             increment_reg->bit_32);
    else
      inst_list = codegen_sign_extend32_to64(inst_list, increment_reg->bit_32,
                                             increment_reg->bit_64);
  }

  long long pointer_step = 1;
  if (target_is_pointer) {
    if (codegen_sizeof_pointer_target(ctx, target_expr, &pointer_step) != 0 ||
        pointer_step <= 0) {
      pointer_step = 1;
    }
  }

  if (target_is_pointer && pointer_step != 1) {
    char buffer_scale[128];
    if (target_uses_qword) {
      snprintf(buffer_scale, sizeof(buffer_scale), "\timulq\t$%lld, %%0\n",
               pointer_step);
      {
        Register_t *du[] = {increment_reg};
        inst_list = add_inst_du(inst_list, ctx, du, 1, du, 1, buffer_scale);
      }
    } else {
      snprintf(buffer_scale, sizeof(buffer_scale), "\timull\t$%lld, %%0\n",
               pointer_step);
      {
        Register_t *du[] = {increment_reg};
        inst_list = add_inst_du(inst_list, ctx, du, 1, du, 1, buffer_scale);
      }
    }
  }

  if (!is_increment) {
    if (target_uses_qword) {
      {
        Register_t *du[] = {increment_reg};
        inst_list = add_inst_du(inst_list, ctx, du, 1, du, 1, "\tnegq\t%0\n");
      }
    } else {
      {
        Register_t *du[] = {increment_reg};
        inst_list = add_inst_du(inst_list, ctx, du, 1, du, 1, "\tnegl\t%0\n");
      }
    }
  }

  int needs_addr = 0;
  StackNode_t *inc_spill = NULL;
  if (target_expr != NULL) {
    if (target_expr->type == EXPR_VAR_ID) {
      int scope_depth = 0;
      StackNode_t *var_node =
          find_label_with_depth(target_expr->expr_data.id, &scope_depth);
      if (var_node != NULL && var_node->is_reference)
        needs_addr = 1;
    } else if (target_expr->type == EXPR_ARRAY_ACCESS ||
               codegen_expr_is_addressable(target_expr)) {
      needs_addr = 1;
    }
  }

  if (needs_addr) {
    int spill_size = target_uses_qword ? 8 : 4;
    inc_spill = codegen_alloc_incdec_temp(spill_size);
    assert(inc_spill != NULL);
    if (inc_spill == NULL)
      return inst_list;

    {
      /* Integrated: store to the frame slot through the backend vtable. */
      BeWidth w = target_uses_qword ? BE_W64 : BE_W32;
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_MEM_FRAME, w,
                       {.mem_frame = {BE_BASE_FP, -(long long)(inc_spill->offset)}}};
      BeOperand a = {OPK_VREG, w, {.vreg = increment_reg}};
      kgpc_backend_target()->emit(&em, BE_STORE, w, &dst, &a, NULL);
      inst_list = em.list;
    }

    free_reg(get_reg_stack(), increment_reg);
    increment_reg = NULL;
  }

  if (target_expr != NULL && target_expr->type == EXPR_VAR_ID) {
    int scope_depth = 0;
    StackNode_t *var_node =
        find_label_with_depth(target_expr->expr_data.id, &scope_depth);
    if (var_node == NULL && ctx != NULL && ctx->symtab != NULL) {
      HashNode_t *target_node = NULL;
      if (FindSymbol(&target_node, ctx->symtab, target_expr->expr_data.id) !=
              0 &&
          target_node != NULL && target_node->mangled_id != NULL) {
        var_node = find_label_with_depth(target_node->mangled_id, &scope_depth);
      }
    }
    if (var_node != NULL) {
      if (var_node->is_reference) {
        Register_t *addr_reg = NULL;
        inst_list =
            codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
        if (!codegen_had_error(ctx) && addr_reg != NULL) {
          if (inc_spill != NULL && increment_reg == NULL) {
            char reload_buf[96];
            if (target_uses_qword)
              snprintf(reload_buf, sizeof(reload_buf),
                       "\tmovq\t-%d(%%rbp), %%rax\n", inc_spill->offset);
            else
              snprintf(reload_buf, sizeof(reload_buf),
                       "\tmovl\t-%d(%%rbp), %%eax\n", inc_spill->offset);
            inst_list = add_inst(inst_list, reload_buf);
            if (target_uses_qword) {
              Register_t *u[] = {addr_reg};
              inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                      "\taddq\t%rax, (%0)\n");
            } else {
              char mem_operand[64];
              snprintf(mem_operand, sizeof(mem_operand), "(%s)",
                       addr_reg->bit_64);
              Register_t *u[] = {addr_reg};
              inst_list =
                  codegen_incdec_add_rax_to_mem(inst_list, ctx, target_width,
                                                mem_operand, u, 1);
            }
          } else if (target_uses_qword) {
            Register_t *u[] = {increment_reg, addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 2,
                                    "\taddq\t%0, (%1)\n");
          } else {
            char mem_operand[64];
            snprintf(mem_operand, sizeof(mem_operand), "(%s)",
                     addr_reg->bit_64);
            Register_t *u[] = {addr_reg};
            inst_list =
                codegen_incdec_add_reg_to_mem(inst_list, ctx, target_width,
                                              increment_reg, mem_operand, u, 1);
          }
          free_reg(get_reg_stack(), addr_reg);
        }
      } else if (var_node->is_static) {
        const char *label = (var_node->static_label != NULL)
                                ? var_node->static_label
                                : var_node->label;
        char buffer[128];
        if (target_uses_qword) {
          snprintf(buffer, sizeof(buffer), "\taddq\t%%0, %s(%%rip)\n", label);
          {
            Register_t *u[] = {increment_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer);
          }
        } else {
          snprintf(buffer, sizeof(buffer), "%s(%%rip)", label);
          inst_list = codegen_incdec_add_reg_to_mem(
              inst_list, ctx, target_width, increment_reg, buffer, NULL, 0);
        }
      } else if (scope_depth > 0) {
        codegen_begin_expression(ctx);
        Register_t *frame_reg =
            codegen_acquire_static_link(ctx, &inst_list, scope_depth);
        char buffer[128];
        if (frame_reg != NULL) {
          if (target_uses_qword) {
            snprintf(buffer, sizeof(buffer), "\taddq\t%%0, -%d(%%1)\n",
                     var_node->offset);
            {
              Register_t *u[] = {increment_reg, frame_reg};
              inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 2, buffer);
            }
          } else {
            snprintf(buffer, sizeof(buffer), "-%d(%s)", var_node->offset,
                     frame_reg->bit_64);
            Register_t *u[] = {frame_reg};
            inst_list = codegen_incdec_add_reg_to_mem(
                inst_list, ctx, target_width, increment_reg, buffer, u, 1);
          }
        } else {
          codegen_report_error(
              ctx, "ERROR: Failed to acquire static link for inc/dec of %s.",
              target_expr->expr_data.id);
          if (target_uses_qword) {
            snprintf(buffer, sizeof(buffer), "\taddq\t%%0, -%d(%%rbp)\n",
                     var_node->offset);
            {
              Register_t *u[] = {increment_reg};
              inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer);
            }
          } else {
            snprintf(buffer, sizeof(buffer), "-%d(%%rbp)", var_node->offset);
            inst_list = codegen_incdec_add_reg_to_mem(
                inst_list, ctx, target_width, increment_reg, buffer, NULL, 0);
          }
        }
        codegen_end_expression(ctx);
      } else {
        char buffer[128];
        if (target_uses_qword) {
          snprintf(buffer, sizeof(buffer), "\taddq\t%%0, -%d(%%rbp)\n",
                   var_node->offset);
          {
            Register_t *u[] = {increment_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer);
          }
        } else {
          snprintf(buffer, sizeof(buffer), "-%d(%%rbp)", var_node->offset);
          inst_list = codegen_incdec_add_reg_to_mem(
              inst_list, ctx, target_width, increment_reg, buffer, NULL, 0);
        }
      }
      if (var_node->is_reference)
        ; /* already emitted above */
    } else {
      int offset = 0;
      inst_list = codegen_get_nonlocal(inst_list, target_expr->expr_data.id,
                                       &offset, ctx);
      char buffer[128];
      if (target_uses_qword) {
        snprintf(buffer, sizeof(buffer), "\taddq\t%%0, -%d(%s)\n", offset,
                 current_non_local_reg64());
        {
          Register_t *u[] = {increment_reg};
          inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer);
        }
      } else {
        snprintf(buffer, sizeof(buffer), "-%d(%s)", offset,
                 current_non_local_reg64());
        inst_list = codegen_incdec_add_reg_to_mem(
            inst_list, ctx, target_width, increment_reg, buffer, NULL, 0);
      }
    }
  } else if (target_expr != NULL && target_expr->type == EXPR_ARRAY_ACCESS) {
    Register_t *addr_reg = NULL;
    inst_list =
        codegen_array_element_address(target_expr, inst_list, ctx, &addr_reg);
    if (!codegen_had_error(ctx) && addr_reg != NULL) {
      if (inc_spill != NULL && increment_reg == NULL) {
        char reload_buf[96];
        if (target_uses_qword)
          snprintf(reload_buf, sizeof(reload_buf),
                   "\tmovq\t-%d(%%rbp), %%rax\n", inc_spill->offset);
        else
          snprintf(reload_buf, sizeof(reload_buf),
                   "\tmovl\t-%d(%%rbp), %%eax\n", inc_spill->offset);
        inst_list = add_inst(inst_list, reload_buf);
        if (target_uses_qword) {
          Register_t *u[] = {addr_reg};
          inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                  "\taddq\t%rax, (%0)\n");
        } else {
          char mem_operand[64];
          snprintf(mem_operand, sizeof(mem_operand), "(%s)", addr_reg->bit_64);
          Register_t *u[] = {addr_reg};
          inst_list =
              codegen_incdec_add_rax_to_mem(inst_list, ctx, target_width,
                                            mem_operand, u, 1);
        }
      } else if (target_uses_qword) {
        Register_t *u[] = {increment_reg, addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\taddq\t%0, (%1)\n");
      } else {
        char mem_operand[64];
        snprintf(mem_operand, sizeof(mem_operand), "(%s)", addr_reg->bit_64);
        Register_t *u[] = {addr_reg};
        inst_list =
            codegen_incdec_add_reg_to_mem(inst_list, ctx, target_width,
                                          increment_reg, mem_operand, u, 1);
      }
      free_reg(get_reg_stack(), addr_reg);
    }
  } else if (codegen_expr_is_addressable(target_expr)) {
    Register_t *addr_reg = NULL;
    inst_list =
        codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
    if (!codegen_had_error(ctx) && addr_reg != NULL) {
      if (inc_spill != NULL && increment_reg == NULL) {
        char reload_buf[96];
        if (target_uses_qword)
          snprintf(reload_buf, sizeof(reload_buf),
                   "\tmovq\t-%d(%%rbp), %%rax\n", inc_spill->offset);
        else
          snprintf(reload_buf, sizeof(reload_buf),
                   "\tmovl\t-%d(%%rbp), %%eax\n", inc_spill->offset);
        inst_list = add_inst(inst_list, reload_buf);
        if (target_uses_qword) {
          Register_t *u[] = {addr_reg};
          inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                  "\taddq\t%rax, (%0)\n");
        } else {
          char mem_operand[64];
          snprintf(mem_operand, sizeof(mem_operand), "(%s)", addr_reg->bit_64);
          Register_t *u[] = {addr_reg};
          inst_list =
              codegen_incdec_add_rax_to_mem(inst_list, ctx, target_width,
                                            mem_operand, u, 1);
        }
      } else if (target_uses_qword) {
        Register_t *u[] = {increment_reg, addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\taddq\t%0, (%1)\n");
      } else {
        char mem_operand[64];
        snprintf(mem_operand, sizeof(mem_operand), "(%s)", addr_reg->bit_64);
        Register_t *u[] = {addr_reg};
        inst_list =
            codegen_incdec_add_reg_to_mem(inst_list, ctx, target_width,
                                          increment_reg, mem_operand, u, 1);
      }
      free_reg(get_reg_stack(), addr_reg);
    }
  } else {
    codegen_report_error(ctx, "ERROR: Unsupported Inc target.");
  }

  if (increment_reg != NULL)
    free_reg(get_reg_stack(), increment_reg);
  return inst_list;
}

static ListNode_t *codegen_builtin_inc(struct Statement *stmt,
                                       ListNode_t *inst_list,
                                       CodeGenContext *ctx) {
  return codegen_builtin_incdec(stmt, inst_list, ctx, 1);
}

static ListNode_t *codegen_builtin_dec(struct Statement *stmt,
                                       ListNode_t *inst_list,
                                       CodeGenContext *ctx) {
  return codegen_builtin_incdec(stmt, inst_list, ctx, 0);
}

static ListNode_t *codegen_builtin_include_exclude(struct Statement *stmt,
                                                   ListNode_t *inst_list,
                                                   CodeGenContext *ctx,
                                                   int is_exclude) {
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
  if (codegen_had_error(ctx) || value_reg == NULL) {
    if (addr_reg != NULL)
      free_reg(get_reg_stack(), addr_reg);
    if (value_reg != NULL)
      free_reg(get_reg_stack(), value_reg);
    return inst_list;
  }

  char buffer[128];

  /* btsl/btrl can operate directly on memory, so we only need the
     bit position in value_reg.  No extra registers needed. */
  {
    Register_t *du[] = {value_reg};
    inst_list = add_inst_du(inst_list, ctx, du, 1, du, 1, "\tandl\t$255, %0\n");
  }

  if (is_exclude) {
    snprintf(buffer, sizeof(buffer), "\tbtrl\t%%0, (%s)\n", addr_reg->bit_64);
    {
      Register_t *u[] = {value_reg, addr_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 2, buffer);
    }
  } else {
    snprintf(buffer, sizeof(buffer), "\tbtsl\t%%0, (%s)\n", addr_reg->bit_64);
    {
      Register_t *u[] = {value_reg, addr_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 2, buffer);
    }
  }

  free_reg(get_reg_stack(), value_reg);
  free_reg(get_reg_stack(), addr_reg);
  return inst_list;
}

static ListNode_t *codegen_builtin_include(struct Statement *stmt,
                                           ListNode_t *inst_list,
                                           CodeGenContext *ctx) {
  return codegen_builtin_include_exclude(stmt, inst_list, ctx, 0);
}

static ListNode_t *codegen_builtin_exclude(struct Statement *stmt,
                                           ListNode_t *inst_list,
                                           CodeGenContext *ctx) {
  return codegen_builtin_include_exclude(stmt, inst_list, ctx, 1);
}

static ListNode_t *codegen_builtin_new(struct Statement *stmt,
                                       ListNode_t *inst_list,
                                       CodeGenContext *ctx) {
  if (stmt == NULL || ctx == NULL)
    return inst_list;

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  struct Expression *method_expr = NULL;
  if (args_expr != NULL && args_expr->next != NULL) {
    if (args_expr->next->next == NULL)
      method_expr = (struct Expression *)args_expr->next->cur;
    else
      args_expr = NULL;
  }
  if (args_expr == NULL) {
    fprintf(stderr, "ERROR: New expects exactly one argument.\n");
    return inst_list;
  }

  struct Expression *target_expr = (struct Expression *)args_expr->cur;

  Register_t *addr_reg = NULL;
  inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
  if (codegen_had_error(ctx) || addr_reg == NULL)
    return inst_list;

  long long alloc_size = 0;
  if (codegen_sizeof_pointer_target(ctx, target_expr, &alloc_size) != 0 ||
      alloc_size <= 0) {
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  }

  Register_t *size_reg = get_free_reg(get_reg_stack(), &inst_list);
  if (size_reg == NULL) {
    codegen_report_error(ctx,
                         "ERROR: Unable to allocate register for New size.");
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  }

  {
    char tmpl[64];
    snprintf(tmpl, sizeof(tmpl), "\tmovq\t$%lld, %%0\n", alloc_size);
    Register_t *d[] = {size_reg};
    inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
  }

  if (codegen_target_is_windows()) {
    // Move size to %rdx first, before moving addr to %rcx
    // This avoids overwriting size_reg if it happens to be %rcx
    {
      Register_t *u[] = {size_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
    }
    {
      Register_t *u[] = {addr_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
    }
  } else {
    {
      Register_t *u[] = {addr_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
    }
    {
      Register_t *u[] = {size_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
    }
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_new");

  free_reg(get_reg_stack(), addr_reg);
  free_reg(get_reg_stack(), size_reg);
  free_arg_regs();

  if (method_expr != NULL && !codegen_had_error(ctx))
    inst_list = codegen_emit_new_dispose_method_fallback(
        stmt, inst_list, ctx, target_expr, method_expr);

  return inst_list;
}

static ListNode_t *codegen_builtin_dispose(struct Statement *stmt,
                                           ListNode_t *inst_list,
                                           CodeGenContext *ctx) {
  if (stmt == NULL || ctx == NULL)
    return inst_list;

  ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
  struct Expression *method_expr = NULL;
  if (args_expr != NULL && args_expr->next != NULL) {
    if (args_expr->next->next == NULL)
      method_expr = (struct Expression *)args_expr->next->cur;
    else
      args_expr = NULL;
  }
  if (args_expr == NULL) {
    fprintf(stderr, "ERROR: Dispose expects exactly one argument.\n");
    return inst_list;
  }

  struct Expression *target_expr = (struct Expression *)args_expr->cur;

  if (method_expr != NULL) {
    inst_list = codegen_emit_new_dispose_method_fallback(
        stmt, inst_list, ctx, target_expr, method_expr);
    if (codegen_had_error(ctx))
      return inst_list;
  }

  Register_t *addr_reg = NULL;
  /* kgpc_dispose takes a void** (the address of the pointer slot) so it can
   * free *slot and nil the slot.  For an addressable target that slot is the
   * variable itself.  But a non-addressable rvalue argument — e.g.
   * Dispose(pderef(List[i])) where List[i] is a default-indexed property whose
   * getter returns the pointer by value — has no storage of its own.  Evaluate
   * it to the pointer value, spill that into a stack temp, and pass the temp's
   * address: kgpc_dispose then frees the correct pointer and nils the dead
   * temp (FPC's Dispose does not nil the source anyway).  Doing this only here,
   * rather than in codegen_address_for_expr, keeps the address-of a
   * non-addressable class/record rvalue (whose value already serves as its
   * address) unchanged for every other caller. */
  if (!codegen_expr_is_addressable(target_expr)) {
    Register_t *value_reg = NULL;
    inst_list = codegen_evaluate_expr(target_expr, inst_list, ctx, &value_reg);
    if (codegen_had_error(ctx) || value_reg == NULL)
      return inst_list;
    StackNode_t *temp_slot = add_l_t_bytes("dispose_rvalue_tmp", 8);
    if (temp_slot == NULL) {
      free_reg(get_reg_stack(), value_reg);
      codegen_report_error(
          ctx, "ERROR: Unable to allocate temp slot for Dispose argument.");
      return inst_list;
    }
    {
      /* Integrated: store to the frame slot through the backend vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                       {.mem_frame = {BE_BASE_FP, -(long long)(temp_slot->offset)}}};
      BeOperand a = {OPK_VREG, BE_W64, {.vreg = value_reg}};
      kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
      inst_list = em.list;
    }
    {
      Register_t *d[] = {value_reg};
      char tmpl[64];
      snprintf(tmpl, sizeof(tmpl), "\tleaq\t-%d(%%rbp), %%0\n",
               temp_slot->offset);
      inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
    }
    addr_reg = value_reg;
  } else {
    inst_list =
        codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
  }
  if (codegen_had_error(ctx) || addr_reg == NULL)
    return inst_list;

  if (codegen_target_is_windows()) {
    Register_t *u[] = {addr_reg};
    inst_list =
        add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
  } else {
    Register_t *u[] = {addr_reg};
    inst_list =
        add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
  }

  free_reg(get_reg_stack(), addr_reg);

  /* Actually dispatch the deallocation.  Prior codegen loaded the
   * pointer's address into the first ABI arg register but never
   * emitted the call, so every Dispose(p) on a heap node became a
   * no-op and the New()-allocated storage leaked at program exit.
   * kgpc_dispose takes a `void **` (address of the pointer slot),
   * frees the pointee through MemoryManager and nils the slot,
   * matching the Pascal-side `procedure Dispose(var p: Pointer)`
   * contract. */
  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_dispose");
  free_arg_regs();
  return inst_list;
}

static ListNode_t *codegen_builtin_write_like(struct Statement *stmt,
                                              ListNode_t *inst_list,
                                              CodeGenContext *ctx,
                                              int append_newline) {
  if (stmt == NULL || ctx == NULL)
    return inst_list;

  char buffer[128];
  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  Register_t *file_reg = NULL;
  StackNode_t *file_spill = NULL;
  int has_file_arg = 0;

  if (args != NULL) {
    struct Expression *first_expr = (struct Expression *)args->cur;
    if (first_expr != NULL && (expr_has_type_tag(first_expr, TEXT_TYPE))) {
      file_reg = get_free_reg(get_reg_stack(), &inst_list);
      if (file_reg == NULL)
        file_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
      if (file_reg != NULL) {
        Register_t *addr_reg = NULL;
        inst_list =
            codegen_address_for_expr(first_expr, inst_list, ctx, &addr_reg);
        if (addr_reg != NULL) {
          {
            Register_t *d[] = {file_reg};
            Register_t *u[] = {addr_reg};
            inst_list =
                add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovq\t%1, %0\n");
          }
          free_reg(get_reg_stack(), addr_reg);
        }
        has_file_arg = 1;
        file_spill = add_l_t("write_file");
        if (file_spill != NULL) {
          {
            /* Integrated: store to the frame slot through the backend vtable. */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                             {.mem_frame = {BE_BASE_FP, -(long long)(file_spill->offset)}}};
            BeOperand a = {OPK_VREG, BE_W64, {.vreg = file_reg}};
            kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
            inst_list = em.list;
          }
          free_reg(get_reg_stack(), file_reg);
          file_reg = NULL;
        }
      } else {
        codegen_report_error(
            ctx, "ERROR: Unable to allocate register for write file argument.");
      }
      args = args->next;
    }
  }

  while (args != NULL) {
    struct Expression *expr = (struct Expression *)args->cur;

    int expr_type = (expr != NULL) ? expr_get_type_tag(expr) : UNKNOWN_TYPE;

    /* If we couldn't get a reliable type tag from the expression itself,
     * try the symbol table (helps with string params/vars that lost their
     * resolved_kgpc_type during earlier passes). */
    if (expr != NULL && ctx != NULL && ctx->symtab != NULL &&
        expr->type == EXPR_VAR_ID) {
      HashNode_t *sym_node = NULL;
      if (FindSymbol(&sym_node, ctx->symtab, expr->expr_data.id) != 0 &&
          sym_node != NULL && sym_node->type != NULL) {
        int sym_type = codegen_tag_from_kgpc(sym_node->type);
        /* Only override the expression's own type when it is unknown.
         * If the semantic checker already resolved a type (e.g. Pi as REAL_TYPE
         * via the builtin const shadowing an FPC internproc function), trust
         * it. */
        if (sym_type != UNKNOWN_TYPE && expr_type == UNKNOWN_TYPE)
          expr_type = sym_type;
        if (expr->resolved_kgpc_type == NULL)
          expr->resolved_kgpc_type = sym_node->type;
      }
    }

    /* If still unknown, check stack metadata (captures parameter/local sizes
     * when the symtab scope was already popped after semantic analysis). */
    if (expr != NULL && expr->type == EXPR_VAR_ID &&
        expr_type == UNKNOWN_TYPE) {
      int scope_depth = 0;
      StackNode_t *stack_node =
          find_label_with_depth(expr->expr_data.id, &scope_depth);
      if (stack_node != NULL && stack_node->size == 8)
        expr_type = STRING_TYPE;
    }

    /* Propagate the discovered type back into the expression so downstream
     * codegen (expr trees, storage sizing) can make the right width choice. */
    (void)expr;

    int expr_is_wide_string = codegen_expr_is_wide_string_value(expr);

    /* Treat char arrays and all managed string forms as strings for printing */
    int treat_as_string =
        (expr_type == STRING_TYPE || expr_type == SHORTSTRING_TYPE ||
         expr_is_wide_string);
    /* If type info is missing but the literal is a string (and not typed as
     * CHAR), still treat it as a string for write/writeln. This fixes string
     * literals that lost their resolved_type during parsing/semantics. */
    if (!treat_as_string && expr != NULL && expr->type == EXPR_STRING &&
        expr_type != CHAR_TYPE)
      treat_as_string = 1;
    if (expr != NULL && expr_type == CHAR_TYPE && expr->is_array_expr &&
        expr->array_element_type == CHAR_TYPE) {
      treat_as_string = 1;
    }

    /* Also treat PAnsiChar (pointer to char) as string */
    if (expr != NULL && expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_pointer(expr->resolved_kgpc_type)) {
      if (expr->resolved_kgpc_type->type_alias != NULL &&
          expr->resolved_kgpc_type->type_alias->target_type_id != NULL) {
        const char *alias_name =
            expr->resolved_kgpc_type->type_alias->target_type_id;

        if (strcasecmp(alias_name, "PAnsiChar") == 0 ||
            strcasecmp(alias_name, "PChar") == 0 ||
            strcasecmp(alias_name, "pcchar") == 0 ||
            strcasecmp(alias_name, "cchar") == 0 ||
            strcasecmp(alias_name, "char") == 0) {
          treat_as_string = 1;
        }
      }
    }

    if (expr != NULL && expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_pointer(expr->resolved_kgpc_type)) {
      int subtype = kgpc_type_get_pointer_subtype_tag(expr->resolved_kgpc_type);
      if (subtype == CHAR_TYPE) {
        treat_as_string = 1;
      }
    }

    /* Check for pointer type with char subtype (covers PAnsiChar, PChar, etc.)
     */
    if (expr != NULL && expr_type == POINTER_TYPE &&
        expr->pointer_subtype == CHAR_TYPE) {
      treat_as_string = 1;
    }

    /* Check for array access expressions where element type is PAnsiChar/PChar
     * by name */
    if (expr != NULL && expr->is_array_expr &&
        expr->array_element_type_id != NULL) {
      if (strcasecmp(expr->array_element_type_id, "PAnsiChar") == 0 ||
          strcasecmp(expr->array_element_type_id, "PChar") == 0 ||
          strcasecmp(expr->array_element_type_id, "pcchar") == 0) {
        treat_as_string = 1;
      }
    }

    const int expr_is_real = (expr_type == REAL_TYPE);
    /* Windows --no-stdlib: route a write to an explicit Text file through the
     * FPC RTL fpc_Write_Text_* path instead of KGPC's kgpc_write_*.  KGPC's
     * runtime can only output via a FILE*, and a win64 Rewrite'd file's
     * TextRec.Handle is a CreateFile HANDLE (not a CRT fd / GetStdHandle
     * value), so kgpc_textrec_get_stream can't map it and the write leaks to
     * stdout.  The RTL's FileWriteFunc->WriteFile uses the HANDLE directly.
     * (mark_used seeds these RTL functions so they are emitted.)  The RTL ABI
     * is (Len:Longint; var f:Text; value): Len=arg0, @f=arg1, value=arg2 —
     * value lands in arg2 in the kgpc ABI too, so the value marshalling below
     * is reused; only arg0/arg1 swap (Len<->file), the no-width default (0 vs
     * -1) and the call target differ.  REAL (stacked float arg) and plain
     * char-arrays (open-array ABI) are not yet mapped, so they keep the kgpc
     * path. */
    const int route_rtl =
        has_file_arg && no_stdlib_flag() && codegen_target_is_windows() &&
        !expr_is_real &&
        !(expr != NULL && expr_type == CHAR_TYPE && expr->is_array_expr &&
          expr->array_element_type == CHAR_TYPE &&
          !codegen_expr_is_shortstring_array(expr));
    const char *file_dest64 = current_arg_reg64(route_rtl ? 1 : 0);
    const char *width_dest64 = current_arg_reg64(route_rtl ? 0 : 1);
    const char *precision_dest64 = current_arg_reg64(2);
    const char *value_dest64 = current_arg_reg64(expr_is_real ? 3 : 2);

    StackNode_t *width_spill = NULL;
    StackNode_t *precision_spill = NULL;
    const int width_specified = (expr != NULL && expr->field_width != NULL);
    const int precision_specified =
        (expr_is_real && expr != NULL && expr->field_precision != NULL);

    if (width_specified) {
      expr_node_t *width_tree = build_expr_tree(expr->field_width);
      Register_t *width_reg = get_free_reg(get_reg_stack(), &inst_list);
      inst_list = gencode_expr_tree(width_tree, inst_list, ctx, width_reg);
      free_expr_tree(width_tree);
      width_spill = add_l_t("write_width");
      if (width_spill != NULL) {
        /* Integrated: store to the frame slot through the backend vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(width_spill->offset)}}};
        BeOperand a = {OPK_VREG, BE_W64, {.vreg = width_reg}};
        kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
        inst_list = em.list;
      }
      free_reg(get_reg_stack(), width_reg);
    }

    if (precision_specified) {
      expr_node_t *precision_tree = build_expr_tree(expr->field_precision);
      Register_t *precision_reg = get_free_reg(get_reg_stack(), &inst_list);
      inst_list =
          gencode_expr_tree(precision_tree, inst_list, ctx, precision_reg);
      free_expr_tree(precision_tree);
      precision_spill = add_l_t("write_precision");
      if (precision_spill != NULL) {
        /* Integrated: store to the frame slot through the backend vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(precision_spill->offset)}}};
        BeOperand a = {OPK_VREG, BE_W64, {.vreg = precision_reg}};
        kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
        inst_list = em.list;
      }
      free_reg(get_reg_stack(), precision_reg);
    }

    /* For char arrays being treated as strings, we need to load the address */
    Register_t *value_reg = NULL;
    if (expr != NULL && expr_type == CHAR_TYPE && expr->is_array_expr &&
        expr->array_element_type == CHAR_TYPE) {
      /* Load address of char array. codegen_address_for_expr allocates its own
       * register. */
      inst_list = codegen_address_for_expr(expr, inst_list, ctx, &value_reg);
      if (codegen_had_error(ctx) || value_reg == NULL) {
        if (value_reg != NULL)
          free_reg(get_reg_stack(), value_reg);
        return inst_list;
      }
    } else if (expr != NULL && expr_type == SHORTSTRING_TYPE) {
      /* Load address of shortstring (array of char). codegen_address_for_expr
       * allocates its own register. */
      inst_list = codegen_address_for_expr(expr, inst_list, ctx, &value_reg);
      if (codegen_had_error(ctx) || value_reg == NULL) {
        if (value_reg != NULL)
          free_reg(get_reg_stack(), value_reg);
        return inst_list;
      }
    } else if (expr != NULL && expr->type == EXPR_RELOP) {
      /* Use special relop handling for char set IN operations and other relops.
       * codegen_relop_to_value allocates its own register. */
      inst_list = codegen_relop_to_value(expr, inst_list, ctx, &value_reg);
      if (codegen_had_error(ctx) || value_reg == NULL) {
        if (value_reg != NULL)
          free_reg(get_reg_stack(), value_reg);
        return inst_list;
      }
    } else {
      /* Load value normally - need to allocate register for expr tree
       * evaluation */
      value_reg = get_free_reg(get_reg_stack(), &inst_list);
      if (value_reg == NULL)
        value_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
      if (value_reg == NULL) {
        codegen_report_error(
            ctx, "ERROR: Unable to allocate register for write value.");
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
     * Strategy: Move in reverse order of argument positions to avoid
     * overwrites. On Windows: value=%r8 (arg2), width=%rdx (arg1),
     * precision=%r9 (arg3) So: Move precision first, then width, then value.
     */

    /* Move precision first (if real and has precision) */
    /* Move precision first (if specified for reals) */
    if (expr_is_real && precision_specified) {
      if (precision_spill != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 precision_spill->offset, precision_dest64);
        inst_list = add_inst(inst_list, buffer);
      } else {
        snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", precision_dest64);
        inst_list = add_inst(inst_list, buffer);
      }
    }

    /* Move width next */
    if (width_specified) {
      if (width_spill != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 width_spill->offset, width_dest64);
        inst_list = add_inst(inst_list, buffer);
      } else {
        snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", width_dest64);
        inst_list = add_inst(inst_list, buffer);
      }
    } else {
      /* No field width: kgpc_write_* takes -1 (its "unspecified" sentinel);
       * the FPC RTL fpc_Write_Text_* takes 0 (non-iso default Len). */
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
               route_rtl ? "$0" : "$-1", width_dest64);
      inst_list = add_inst(inst_list, buffer);
    }

    /* Move value last */
    if (treat_as_string) {
      char tmpl[64];
      snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, %s\n", value_dest64);
      {
        Register_t *u[] = {value_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
      }
    } else if (expr_is_real || expr_type == POINTER_TYPE) {
      /* Extended sret function calls leave the buffer ADDRESS in the
       * register.  Convert to double bits for kgpc_write_real. */
      if (expr_is_real && expr_returns_sret(expr) &&
          codegen_expr_involves_extended(expr)) {
        const char *arg1_64 = codegen_target_is_windows() ? "%rcx" : "%rdi";
        {
          char tmpl2[64];
          snprintf(tmpl2, sizeof(tmpl2), "\tmovq\t%%0, %s\n", arg1_64);
          Register_t *u[] = {value_reg};
          inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl2);
        }
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(
            inst_list, "kgpc_load_extended_to_bits");
        free_arg_regs();
        {
          Register_t *d[] = {value_reg};
          inst_list =
              add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t%rax, %0\n");
        }
      }
      /* A Single (4-byte) array element or pointer dereference read leaves RAW
       * single bits in the register (per the reg_holds_raw_single convention
       * used by load_real_operand_into_xmm), not the promoted double bits that
       * kgpc_write_real expects. (Record-field reads are already promoted to
       * double by codegen_record_access.) Promote single->double here. */
      if (expr_is_real) {
        if (expr_holds_raw_single_bits(expr,
                                       ctx != NULL ? ctx->symtab : NULL)) {
          {
            char tmpl2[64];
            snprintf(tmpl2, sizeof(tmpl2), "\tmovd\t%s, %%xmm0\n",
                     value_reg->bit_32);
            Register_t *u[] = {value_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl2);
          }
          inst_list = add_inst(inst_list, "\tcvtss2sd\t%xmm0, %xmm0\n");
          {
            char tmpl2[64];
            snprintf(tmpl2, sizeof(tmpl2), "\tmovq\t%%xmm0, %%0\n");
            Register_t *d[] = {value_reg};
            inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl2);
          }
        }
      }
      /* REAL_TYPE and POINTER_TYPE are 64-bit - use movq */
      {
        char tmpl2[64];
        snprintf(tmpl2, sizeof(tmpl2), "\tmovq\t%%0, %s\n", value_dest64);
        Register_t *u[] = {value_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl2);
      }
    } else if (expr_value_requires_64bit(expr, ctx)) {
      /* Int64/QWord/UInt64 or large const values - use 64-bit move */
      char tmpl2[64];
      snprintf(tmpl2, sizeof(tmpl2), "\tmovq\t%%0, %s\n", value_dest64);
      {
        Register_t *u[] = {value_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl2);
      }
    } else if (expr_type == LONGINT_TYPE) {
      /* LONGINT_TYPE is now 4 bytes (to match FPC) - allow unsigned aliases to
       * zero-extend */
      if (expr_is_unsigned_type(expr)) {
        inst_list = codegen_zero_extend32_to64(inst_list, value_reg->bit_32,
                                               value_reg->bit_32);
        char tmpl2[64];
        snprintf(tmpl2, sizeof(tmpl2), "\tmovq\t%%0, %s\n", value_dest64);
        {
          Register_t *u[] = {value_reg};
          inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl2);
        }
      } else {
        inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32,
                                               value_dest64);
      }
    } else {
      /* Use zero-extension for unsigned types, sign-extension otherwise.
       * For zero-extension: writing to a 32-bit register automatically zeros
       * the upper 32 bits of the full 64-bit register. */
      int is_unsigned = expr_is_unsigned_type(expr);
      if (is_unsigned) {
        /* First zero-extend in the value register, then move to destination */
        inst_list = codegen_zero_extend32_to64(inst_list, value_reg->bit_32,
                                               value_reg->bit_32);
        char tmpl2[64];
        snprintf(tmpl2, sizeof(tmpl2), "\tmovq\t%%0, %s\n", value_dest64);
        {
          Register_t *u[] = {value_reg};
          inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl2);
        }
      } else
        inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32,
                                               value_dest64);
    }

    free_reg(get_reg_stack(), value_reg);

    /* Set precision to -1 if not real or not provided */
    if (expr_is_real && !precision_specified) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", precision_dest64);
      inst_list = add_inst(inst_list, buffer);
    }

    /* Determine if this is an unsigned type for printing */
    int is_unsigned_int = expr_is_unsigned_type(expr);

    const char *call_target =
        is_unsigned_int ? "kgpc_write_unsigned" : "kgpc_write_integer";
    int is_char_array = 0;
    int char_array_size = 0;

    if (treat_as_string) {
      /* Check if it's a char array (not a regular string) */
      if (expr != NULL && expr_type == CHAR_TYPE && expr->is_array_expr &&
          expr->array_element_type == CHAR_TYPE) {
        char_array_size = expr->array_upper_bound - expr->array_lower_bound + 1;

        /* Check if this is a ShortString-like array (length byte at index 0) */
        if (codegen_expr_is_shortstring_array(expr)) {
          call_target = "kgpc_write_shortstring";
        } else {
          call_target = "kgpc_write_char_array";
          is_char_array = 1;
        }
      } else if (expr_type == SHORTSTRING_TYPE ||
                 codegen_expr_is_shortstring_array(expr) ||
                 codegen_expr_is_shortstring_value_ctx(expr, ctx)) {
        /* Handle ShortString type - use special write function that handles
         * length prefix. Exception: string literals are still stored as C
         * strings even when typed as SHORTSTRING_TYPE (via {$H-}), so use
         * regular string write for those. */
        if (expr != NULL && expr->type == EXPR_STRING)
          call_target = "kgpc_write_string";
        else
          call_target = "kgpc_write_shortstring";
      } else {
        call_target = expr_is_wide_string ? "kgpc_write_unicodestring"
                                          : "kgpc_write_string";
      }
    } else if (expr_type == BOOL)
      call_target = "kgpc_write_boolean";
    else if (expr_is_real)
      call_target = "kgpc_write_real";
    else if (expr_type == CHAR_TYPE)
      call_target = "kgpc_write_char";
    else if (expr_type == POINTER_TYPE)
      call_target =
          "kgpc_write_integer"; // Print pointers as integers (addresses)

    /* Remap the kgpc_write_* target to the matching FPC RTL fpc_Write_Text_*
     * entry point when routing a Text-file write through the RTL (see the
     * route_rtl note above).  Same (Len, @f, value) ABI; the value already
     * sits in arg2.  Strings use fpc_Write_Text_PChar_As_Pointer, which takes
     * a plain PAnsiChar — exactly KGPC's C-string/ansistring-data pointer — so
     * no FPC-ansistring materialisation is needed.  KGPC emits these RTL
     * compilerprocs under their plain source names (and the [Public,Alias]
     * uppercase names for ShortStr/AnsiStr). */
    if (route_rtl) {
      /* On a 64-bit target FPC compiles only the ValSInt/ValUInt (64-bit)
       * integer writers; the longint/longword/int64/qword variants are
       * {$ifdef}-gated out (text.inc).  The value is already sign/zero-extended
       * to 64 bits in arg2 by the marshalling above, so all integer widths map
       * to SInt/UInt. */
      if (expr_type == POINTER_TYPE)
        call_target = "fpc_Write_Text_UInt";
      else if (strcmp(call_target, "kgpc_write_shortstring") == 0)
        call_target = "FPC_WRITE_TEXT_SHORTSTR";
      else if (strcmp(call_target, "kgpc_write_string") == 0)
        call_target = "fpc_Write_Text_PChar_As_Pointer";
      else if (strcmp(call_target, "kgpc_write_unicodestring") == 0)
        call_target = "fpc_Write_Text_UnicodeStr";
      else if (strcmp(call_target, "kgpc_write_boolean") == 0)
        call_target = "fpc_Write_Text_Boolean";
      else if (strcmp(call_target, "kgpc_write_char") == 0)
        call_target = "fpc_Write_Text_Char";
      else if (strcmp(call_target, "kgpc_write_unsigned") == 0)
        call_target = "fpc_Write_Text_UInt";
      else if (strcmp(call_target, "kgpc_write_integer") == 0)
        call_target = "fpc_Write_Text_SInt";
    }

    if (has_file_arg && (file_spill != NULL || file_reg != NULL)) {
      if (file_spill != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 file_spill->offset, file_dest64);
        inst_list = add_inst(inst_list, buffer);
      } else {
        {
          char tmpl[64];
          snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, %s\n", file_dest64);
          Register_t *u[] = {file_reg};
          inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
        }
      }
    } else {
      snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64,
               file_dest64);
      inst_list = add_inst(inst_list, buffer);
    }

    /* For char arrays, pass the size as the 4th argument */
    if (is_char_array) {
      const char *size_dest64 = current_arg_reg64(3);
      snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %s\n", char_array_size,
               size_dest64);
      inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);

    inst_list = codegen_call_with_shadow_space(inst_list, call_target);

    free_arg_regs();

    /* Invalidate static link cache after each write argument
     * because the static link register may have been clobbered
     * during argument evaluation or the function call.
     * We must free the register first to avoid leaking it. */
    if (ctx->static_link_reg != NULL) {
      free_reg(get_reg_stack(), ctx->static_link_reg);
      ctx->static_link_reg = NULL;
      ctx->static_link_reg_level = 0;
    }

    args = args->next;
  }

  if (append_newline) {
    const char *file_dest64 = current_arg_reg64(0);
    if (has_file_arg && (file_spill != NULL || file_reg != NULL)) {
      if (file_spill != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 file_spill->offset, file_dest64);
        inst_list = add_inst(inst_list, buffer);
      } else {
        {
          char tmpl[64];
          snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, %s\n", file_dest64);
          Register_t *u[] = {file_reg};
          inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
        }
      }
    } else {
      snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64,
               file_dest64);
      inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);

    /* When the value args were routed to the FPC RTL (Windows --no-stdlib with
     * an explicit Text file), terminate the line via the RTL fpc_Writeln_End
     * (var f) so the EOL + flush go through the same TextRec buffer/handle the
     * values did.  fpc_Writeln_End takes @f in arg0, already set above. */
    if (has_file_arg && no_stdlib_flag() && codegen_target_is_windows())
      inst_list = codegen_call_with_shadow_space(inst_list, "fpc_Writeln_End");
    else
      inst_list =
          codegen_call_with_shadow_space(inst_list, "kgpc_write_newline");

    free_arg_regs();
  }

  if (file_reg != NULL)
    free_reg(get_reg_stack(), file_reg);

  return inst_list;
}

static ListNode_t *codegen_builtin_read_like(struct Statement *stmt,
                                             ListNode_t *inst_list,
                                             CodeGenContext *ctx,
                                             int read_line) {
  if (stmt == NULL || ctx == NULL)
    return inst_list;

  char buffer[128];
  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  Register_t *file_reg = NULL;
  StackNode_t *file_spill = NULL;
  int has_file_arg = 0;
  int read_consumed_line = 0;

  /* Check if first argument is a file */
  if (args != NULL) {
    struct Expression *first_expr = (struct Expression *)args->cur;
    if (first_expr != NULL && (expr_has_type_tag(first_expr, TEXT_TYPE))) {
      file_reg = get_free_reg(get_reg_stack(), &inst_list);
      if (file_reg == NULL)
        file_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
      if (file_reg != NULL) {
        Register_t *addr_reg = NULL;
        inst_list =
            codegen_address_for_expr(first_expr, inst_list, ctx, &addr_reg);
        if (addr_reg != NULL) {
          {
            Register_t *d[] = {file_reg};
            Register_t *u[] = {addr_reg};
            inst_list =
                add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovq\t%1, %0\n");
          }
          free_reg(get_reg_stack(), addr_reg);
        }
        has_file_arg = 1;
        file_spill = add_l_t("read_file");
        if (file_spill != NULL) {
          {
            /* Integrated: store to the frame slot through the backend vtable. */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                             {.mem_frame = {BE_BASE_FP, -(long long)(file_spill->offset)}}};
            BeOperand a = {OPK_VREG, BE_W64, {.vreg = file_reg}};
            kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
            inst_list = em.list;
          }
          free_reg(get_reg_stack(), file_reg);
          file_reg = NULL;
        }
      } else {
        codegen_report_error(
            ctx, "ERROR: Unable to allocate register for read file argument.");
      }
      args = args->next;
    }
  }

  /* Windows --no-stdlib: route a read from an explicit Text file through the
   * FPC RTL fpc_Read_Text_AnsiStr / fpc_ReadLn_End path instead of KGPC's
   * kgpc_text_readln_into.  Symmetric to the write routing (see
   * codegen_builtin_write_like): a win64 Reset'd file's TextRec.Handle is a
   * CreateFile HANDLE (not a CRT fd / GetStdHandle value), so KGPC's
   * FILE*-only read path can't reach it and every read sees immediate EOF.
   * The RTL reads through TextRec.InOutFunc->FileReadFunc on the HANDLE.
   * fpc_Read_Text_AnsiStr is (var f; out s; cp) — @f in arg0, @s in arg1 — the
   * (file, addr) layout KGPC already sets up; only the call target and a
   * trailing codepage arg are added.  mark_used seeds the RTL functions.
   *
   * Scope: only AnsiString reads (and Eof, routed in codegen/mark_used) are
   * routed.  The FPC numeric readers (fpc_Read_Text_SInt/Float) and the
   * shortstring reader were verified to misbehave through this path (the
   * numeric value parser returns 0), and KGPC types `string[N]` as CHAR_TYPE,
   * so a blanket char route would corrupt a shortstring target — those stay on
   * the kgpc path (still pending for win64, like the write side's deferral of
   * REAL / open-array).  route_rtl therefore requires EVERY value argument to
   * be a plain AnsiString, so the value reads and the fpc_ReadLn_End tail
   * always agree on the path. */
  int route_rtl =
      has_file_arg && no_stdlib_flag() && codegen_target_is_windows();
  if (route_rtl) {
    for (ListNode_t *scan = args; scan != NULL; scan = scan->next) {
      struct Expression *a = (struct Expression *)scan->cur;
      if (a == NULL || expr_get_type_tag(a) != STRING_TYPE) {
        route_rtl = 0;
        break;
      }
    }
  }

  /* Process each argument to read */
  while (args != NULL) {
    struct Expression *expr = (struct Expression *)args->cur;
    int expr_type = (expr != NULL) ? expr_get_type_tag(expr) : UNKNOWN_TYPE;

    /* Get address of the variable to read into and save to stack.
     * codegen_address_for_expr allocates its own register, so we pass NULL
     * and let it handle register allocation. */
    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
    if (addr_reg == NULL) {
      codegen_report_error(
          ctx, "ERROR: Unable to allocate register for read address.");
      args = args->next;
      continue;
    }
    if (codegen_had_error(ctx)) {
      free_reg(get_reg_stack(), addr_reg);
      return inst_list;
    }

    /* Save address to a stack temporary to avoid register conflicts */
    StackNode_t *addr_spill = add_l_t("read_addr");
    if (addr_spill == NULL) {
      codegen_report_error(
          ctx, "ERROR: Unable to allocate stack space for read address.");
      free_reg(get_reg_stack(), addr_reg);
      args = args->next;
      continue;
    }
    {
      /* Integrated: store to the frame slot through the backend vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                       {.mem_frame = {BE_BASE_FP, -(long long)(addr_spill->offset)}}};
      BeOperand a = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
      kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
      inst_list = em.list;
    }
    free_reg(get_reg_stack(), addr_reg);

    /* Special handling for STRING_TYPE - use kgpc_text_readln_into */
    if (expr_type == STRING_TYPE) {
      const char *file_dest64 = current_arg_reg64(0);
      const char *string_dest64 = current_arg_reg64(1);

      /* Set file argument (or NULL for stdin) */
      if (has_file_arg && file_spill != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 file_spill->offset, file_dest64);
        inst_list = add_inst(inst_list, buffer);
      } else {
        snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64,
                 file_dest64);
        inst_list = add_inst(inst_list, buffer);
      }

      /* Load string address from stack */
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
               addr_spill->offset, string_dest64);
      inst_list = add_inst(inst_list, buffer);

      if (route_rtl) {
        /* fpc_Read_Text_AnsiStr(var f; out s : RawByteString; cp) reads the
         * rest of the line (no EOL consumed) into the ansistring at @s.  cp =
         * CP_ACP (0): TranslatePlaceholderCP maps it to DefaultSystemCodePage,
         * which equals the file's TextRec.CodePage, so no recoding pass runs —
         * the same value FPC's own compiler passes for Read(text, ansistring).
         * The trailing EOL is consumed by fpc_ReadLn_End (the readln-end tail
         * below), so read_consumed_line is left clear unlike the kgpc path. */
        snprintf(buffer, sizeof(buffer), "\txorl\t%s, %s\n",
                 current_arg_reg32(2), current_arg_reg32(2));
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list,
                                                   "FPC_READ_TEXT_ANSISTR");
      } else {
        /* Call kgpc_text_readln_into for string reading */
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list =
            codegen_call_with_shadow_space(inst_list, "kgpc_text_readln_into");
      }
      free_arg_regs();

      /* Invalidate static link cache after call */
      if (ctx->static_link_reg != NULL) {
        free_reg(get_reg_stack(), ctx->static_link_reg);
        ctx->static_link_reg = NULL;
        ctx->static_link_reg_level = 0;
      }

      /* kgpc_text_readln_into already consumes the EOL; the RTL path defers
       * that to fpc_ReadLn_End in the readln-end tail. */
      if (read_line && !route_rtl)
        read_consumed_line = 1;

      args = args->next;
      continue;
    }

    /* Special handling for SHORTSTRING_TYPE - use
     * kgpc_text_readln_into_shortstring */
    if (expr_type == SHORTSTRING_TYPE) {
      const char *file_dest64 = current_arg_reg64(0);
      const char *string_dest64 = current_arg_reg64(1);

      /* Set file argument (or NULL for stdin) */
      if (has_file_arg && file_spill != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 file_spill->offset, file_dest64);
        inst_list = add_inst(inst_list, buffer);
      } else {
        snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64,
                 file_dest64);
        inst_list = add_inst(inst_list, buffer);
      }

      /* Load shortstring address from stack */
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
               addr_spill->offset, string_dest64);
      inst_list = add_inst(inst_list, buffer);

      /* Set max length = 255 (standard ShortString) */
      snprintf(buffer, sizeof(buffer), "\tmovl\t$255, %s\n",
               current_arg_reg32(2));
      inst_list = add_inst(inst_list, buffer);

      /* Call kgpc_text_readln_into_shortstring */
      inst_list = codegen_vect_reg(inst_list, 0);
      inst_list = codegen_call_with_shadow_space(
          inst_list, "kgpc_text_readln_into_shortstring");
      free_arg_regs();

      /* Invalidate static link cache after call */
      if (ctx->static_link_reg != NULL) {
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

    /* Determine which read function to call based on type.  These scalar reads
     * stay on the kgpc path even on Windows --no-stdlib: route_rtl only becomes
     * true when every value argument is a plain AnsiString (see its note), so a
     * non-string read here is never RTL-routed. */
    const char *read_func = NULL;
    switch (expr_type) {
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
    if (has_file_arg && file_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
               file_spill->offset, file_dest64);
      inst_list = add_inst(inst_list, buffer);
    } else {
      snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64,
               file_dest64);
      inst_list = add_inst(inst_list, buffer);
    }

    /* Load address from stack temporary to argument register */
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
             addr_spill->offset, addr_dest64);
    inst_list = add_inst(inst_list, buffer);

    /* Call the appropriate read function */
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, read_func);
    free_arg_regs();

    /* Invalidate static link cache after each read argument */
    if (ctx->static_link_reg != NULL) {
      free_reg(get_reg_stack(), ctx->static_link_reg);
      ctx->static_link_reg = NULL;
      ctx->static_link_reg_level = 0;
    }

    args = args->next;
  }

  /* If readln, consume rest of line */
  if (read_line && !read_consumed_line) {
    const char *file_dest64 = current_arg_reg64(0);
    if (has_file_arg && file_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
               file_spill->offset, file_dest64);
      inst_list = add_inst(inst_list, buffer);
    } else {
      snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", file_dest64,
               file_dest64);
      inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    /* RTL-routed reads leave the trailing EOL in the buffer (fpc_Read_Text_*
     * stop at it without consuming); fpc_ReadLn_End(var f) skips it through the
     * same TextRec the value reads used.  @f is already in arg0. */
    inst_list = codegen_call_with_shadow_space(
        inst_list, route_rtl ? "FPC_READLN_END" : "kgpc_text_readln_discard");
    free_arg_regs();
  }

  if (file_reg != NULL)
    free_reg(get_reg_stack(), file_reg);

  return inst_list;
}

static ListNode_t *codegen_builtin_move(struct Statement *stmt,
                                        ListNode_t *inst_list,
                                        CodeGenContext *ctx) {
  if (stmt == NULL || ctx == NULL)
    return inst_list;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || args->next == NULL || args->next->next == NULL)
    return inst_list;

  /* Pascal Move(src, dst, count) — both src and dst are untyped reference
   * parameters; kgpc_move expects (dest, src, count) in C ABI order.
   * Compute the two reference addresses explicitly rather than dispatching
   * through codegen_pass_arguments with procedure_name="Move", because the
   * latter re-fetches FPC's formal-arg list (const source; var dest; count)
   * and applies formal-position heuristics to the swapped argument list —
   * causing the dst (matched against const source) to be passed by value
   * (an array-element pointer load) instead of by address.  In FPC RTL
   * mode this corrupted TFPList.Delete's destination during pp_bootstrap
   * and produced the 0xa400000001 freelist signature. */
  struct Expression *src_expr = (struct Expression *)args->cur;
  struct Expression *dst_expr = (struct Expression *)args->next->cur;
  struct Expression *count_expr = (struct Expression *)args->next->next->cur;
  if (src_expr == NULL || dst_expr == NULL || count_expr == NULL)
    return inst_list;

  char buffer[128];

  Register_t *dst_addr = NULL;
  inst_list = codegen_address_for_expr(dst_expr, inst_list, ctx, &dst_addr);
  if (codegen_had_error(ctx) || dst_addr == NULL)
    return inst_list;
  StackNode_t *dst_spill = add_l_t("move_dst_spill");
  if (dst_spill != NULL) {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(dst_spill->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = dst_addr}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }
  free_reg(get_reg_stack(), dst_addr);

  Register_t *src_addr = NULL;
  inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_addr);
  if (codegen_had_error(ctx) || src_addr == NULL)
    return inst_list;
  StackNode_t *src_spill = add_l_t("move_src_spill");
  if (src_spill != NULL) {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(src_spill->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = src_addr}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }
  free_reg(get_reg_stack(), src_addr);

  Register_t *count_reg = NULL;
  inst_list = codegen_expr_with_result(count_expr, inst_list, ctx, &count_reg);
  if (codegen_had_error(ctx) || count_reg == NULL)
    return inst_list;

  if (!expr_uses_qword_kgpctype(count_expr))
    inst_list = codegen_sign_extend32_to64(inst_list, count_reg->bit_32,
                                           count_reg->bit_64);

  const char *arg0 = current_arg_reg64(0); /* dst */
  const char *arg1 = current_arg_reg64(1); /* src */
  const char *arg2 = current_arg_reg64(2); /* count */

  {
    char tmpl[64];
    snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, %s\n", arg2);
    Register_t *u[] = {count_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
  }
  free_reg(get_reg_stack(), count_reg);

  if (dst_spill != NULL) {
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
             dst_spill->offset, arg0);
    inst_list = add_inst(inst_list, buffer);
  }
  if (src_spill != NULL) {
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
             src_spill->offset, arg1);
    inst_list = add_inst(inst_list, buffer);
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
  free_arg_regs();
  return inst_list;
}

ListNode_t *codegen_builtin_proc(struct Statement *stmt, ListNode_t *inst_list,
                                 CodeGenContext *ctx) {
#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
#endif
  assert(stmt != NULL);
  assert(stmt->type == STMT_PROCEDURE_CALL);
  assert(ctx != NULL);

  char *proc_name;
  ListNode_t *args_expr;
  /* Long mangled procedure names require a generous buffer for emitted
   * instructions. */
  char buffer[CODEGEN_MAX_INST_BUF];

  proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
  args_expr = stmt->stmt_data.procedure_call_data.expr_args;

  const char *proc_id_lookup = stmt->stmt_data.procedure_call_data.id;

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "SetLength")) {
    inst_list = codegen_builtin_setlength(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "SetString")) {
    inst_list = codegen_builtin_setstring(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "write")) {
    inst_list = codegen_builtin_write_like(stmt, inst_list, ctx, 0);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "writeln")) {
    inst_list = codegen_builtin_write_like(stmt, inst_list, ctx, 1);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "writestr")) {
    inst_list = codegen_builtin_writestr(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "read")) {
    inst_list = codegen_builtin_read_like(stmt, inst_list, ctx, 0);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "readln")) {
    inst_list = codegen_builtin_read_like(stmt, inst_list, ctx, 1);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "Str")) {
    inst_list = codegen_builtin_str(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "Insert")) {
    inst_list = codegen_builtin_insert(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "Delete")) {
    inst_list = codegen_builtin_delete(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "Inc")) {
    inst_list = codegen_builtin_inc(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "Dec")) {
    inst_list = codegen_builtin_dec(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "New")) {
    inst_list = codegen_builtin_new(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "Dispose")) {
    inst_list = codegen_builtin_dispose(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "Val")) {
    inst_list = codegen_builtin_val(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "Move")) {
    inst_list = codegen_builtin_move(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "Include")) {
    inst_list = codegen_builtin_include(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "Exclude")) {
    inst_list = codegen_builtin_exclude(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  if (proc_id_lookup != NULL &&
      pascal_identifier_equals(proc_id_lookup, "Assert")) {
    /* Assert(condition [, message])
     * Evaluate the boolean condition. If true, continue execution.
     * If false, call kgpc_assert_failed(msg, filename, line) which
     * prints the assertion failure and exits with code 227. */
    ListNode_t *assert_args = stmt->stmt_data.procedure_call_data.expr_args;
    struct Expression *cond_expr =
        (assert_args != NULL) ? (struct Expression *)assert_args->cur : NULL;
    struct Expression *msg_expr =
        (assert_args != NULL && assert_args->next != NULL)
            ? (struct Expression *)assert_args->next->cur
            : NULL;

    if (cond_expr != NULL) {
      int relop_type = 0;
      inst_list =
          codegen_condition_expr(cond_expr, inst_list, ctx, &relop_type);

      /* Generate label for the "pass" path (condition was true) */
      char pass_label[18];
      gen_label(pass_label, 18, ctx);

      /* Jump to pass_label if condition is TRUE (non-zero).
       * codegen_condition_expr with a boolean expr does testl and sets
       * relop_type=NE. gencode_jmp(NE, inverse=0, ...) emits jne label (jump if
       * not-equal-to-zero = true). */
      inst_list = gencode_jmp(relop_type, 0, pass_label, inst_list);

      /* Failure path: call kgpc_assert_failed(msg, filename, line)
       * ABI: Windows uses rcx/rdx/r8, SysV uses rdi/rsi/rdx */
      const char *arg1_reg = codegen_target_is_windows() ? "%rcx" : "%rdi";
      const char *arg2_reg = codegen_target_is_windows() ? "%rdx" : "%rsi";
      const char *arg3_reg_32 = codegen_target_is_windows() ? "%r8d" : "%edx";

      /* Set up message argument (arg1) */
      if (msg_expr != NULL && msg_expr->type == EXPR_STRING &&
          msg_expr->expr_data.string != NULL) {
        const char *readonly_section = codegen_readonly_section_directive();
        char msg_label[64];
        snprintf(msg_label, sizeof(msg_label), ".LC%d",
                 ctx->write_label_counter++);
        char escaped_msg[CODEGEN_MAX_INST_BUF];
        escape_string(escaped_msg, msg_expr->expr_data.string,
                      sizeof(escaped_msg));
        char rodata_buf[CODEGEN_MAX_INST_BUF + 128];
        snprintf(rodata_buf, sizeof(rodata_buf),
                 "%s\n%s:\n\t.string \"%s\"\n%s\n", readonly_section, msg_label,
                 escaped_msg, codegen_text_section_resume());
        inst_list = add_inst(inst_list, rodata_buf);
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", msg_label,
                 arg1_reg);
        inst_list = add_inst(inst_list, buffer);
      } else {
        /* No message provided - pass empty string */
        const char *readonly_section = codegen_readonly_section_directive();
        char msg_label[64];
        snprintf(msg_label, sizeof(msg_label), ".LC%d",
                 ctx->write_label_counter++);
        char rodata_buf[256];
        snprintf(rodata_buf, sizeof(rodata_buf),
                 "%s\n%s:\n\t.string \"\"\n%s\n", readonly_section, msg_label,
                 codegen_text_section_resume());
        inst_list = add_inst(inst_list, rodata_buf);
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", msg_label,
                 arg1_reg);
        inst_list = add_inst(inst_list, buffer);
      }
      /* filename argument (arg2) - empty for now */
      {
        const char *readonly_section = codegen_readonly_section_directive();
        char fn_label[64];
        snprintf(fn_label, sizeof(fn_label), ".LC%d",
                 ctx->write_label_counter++);
        char rodata_buf[256];
        snprintf(rodata_buf, sizeof(rodata_buf),
                 "%s\n%s:\n\t.string \"\"\n%s\n", readonly_section, fn_label,
                 codegen_text_section_resume());
        inst_list = add_inst(inst_list, rodata_buf);
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", fn_label,
                 arg2_reg);
        inst_list = add_inst(inst_list, buffer);
      }
      /* line number argument (arg3) */
      snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", stmt->line_num,
               arg3_reg_32);
      inst_list = add_inst(inst_list, buffer);
      /* Zero %eax (no vector regs) and call with shadow space */
      inst_list = add_inst(inst_list, "\txorl\t%eax, %eax\n");
      inst_list =
          codegen_call_with_shadow_space(inst_list, "kgpc_assert_failed");

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
  if ((proc_name != NULL &&
       pascal_identifier_equals(proc_name, "fpc_in_prefetch_var")) ||
      (builtin_id != NULL &&
       pascal_identifier_equals(builtin_id, "Prefetch"))) {
    inst_list = codegen_builtin_prefetch(stmt, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  }

  const char *proc_name_hint = stmt->stmt_data.procedure_call_data.id;
  if (proc_name_hint == NULL)
    proc_name_hint = stmt->stmt_data.procedure_call_data.mangled_id;

  /* For an overloaded RTL procedure dispatched here by its mangled name (e.g.
   * System.Assign -> assign_f_ss), resolve the formal parameters from the
   * overload whose mangled_id matches the call target, rather than letting
   * codegen_pass_arguments fall back to FindSymbol(proc_name_hint), which
   * returns an arbitrary same-named overload.  Without this, a ShortString
   * argument to System.Assign is matched against the RawByteString overload's
   * AnsiString formal and wrongly promoted (kgpc_shortstring_to_string),
   * dropping the string's first character (the FPC bootstrap "Cannot open
   * file" / system.assign filename corruption). */
  struct KgpcType *resolved_overload_type = NULL;
  if (proc_name != NULL && proc_name_hint != NULL && ctx != NULL &&
      ctx->symtab != NULL &&
      !pascal_identifier_equals(proc_name, proc_name_hint)) {
    ListNode_t *cands = FindAllIdents(ctx->symtab, proc_name_hint);
    for (ListNode_t *c = cands; c != NULL; c = c->next) {
      HashNode_t *hn = (HashNode_t *)c->cur;
      if (hn != NULL && hn->mangled_id != NULL &&
          strcmp(hn->mangled_id, proc_name) == 0 && hn->type != NULL &&
          hn->type->kind == TYPE_KIND_PROCEDURE) {
        resolved_overload_type = hn->type;
        break;
      }
    }
    if (cands != NULL)
      DestroyList(cands);
  }

  inst_list = codegen_pass_arguments(args_expr, inst_list, ctx,
                                     resolved_overload_type, proc_name_hint, 0,
                                     NULL, 0);
  inst_list = codegen_vect_reg(inst_list, 0);
  const char *call_target =
      (proc_name != NULL) ? proc_name : stmt->stmt_data.procedure_call_data.id;
  if (call_target != NULL &&
      pascal_identifier_equals(call_target, "fpc_in_prefetch_var")) {
    return codegen_builtin_prefetch(stmt, inst_list, ctx);
  }
  /* Initialize/Finalize are backed by fixed-case runtime stubs (runtime.c).
   * Pascal identifiers are case-insensitive, so the source may spell them
   * lowercase (FPC's text.inc uses `finalize(...)`), which would otherwise
   * emit an undefined `call finalize`.  Canonicalise to the runtime symbol. */
  if (call_target != NULL) {
    if (pascal_identifier_equals(call_target, "Finalize"))
      call_target = "Finalize";
    else if (pascal_identifier_equals(call_target, "Initialize"))
      call_target = "Initialize";
  }
  /* UniqueString(var S): codegen_pass_arguments already marshals the var-param
   * address (a char**) into the first argument register, matching the runtime
   * helper kgpc_string_unique(char **target) which makes S refcount-unique in
   * place.  Route to it instead of the mangled `uniquestring_s`, which has no
   * implementation (KGPC registers UniqueString as a builtin proc but emits no
   * body).  FPC's compiler (e.g. omfbase.pas, cfileutl.fixpath) calls this. */
  if (builtin_id != NULL &&
      pascal_identifier_equals(builtin_id, "UniqueString"))
    call_target = "kgpc_string_unique";
  if (call_target == NULL)
    call_target = "";
  snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", call_target);
  inst_list = add_inst(inst_list, buffer);
  inst_list = codegen_cleanup_call_stack(inst_list, ctx);
#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
  return inst_list;
}

/* Returns a list of instructions */
