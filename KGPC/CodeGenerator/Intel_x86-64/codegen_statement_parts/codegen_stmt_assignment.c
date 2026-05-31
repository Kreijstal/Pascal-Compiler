#include "../../../Parser/pascal_frontend.h"
#include "../codegen_stmt_internal.h"

/* Tracks a destination address register across nested expression evaluation
 * that may spill it. When the LRU/graph-coloring spiller picks our reg, the
 * callback below records the spill slot so we can reload before use. */
typedef struct DestSpillTracker {
  StackNode_t *spill_slot; /* non-NULL if our reg was spilled */
} DestSpillTracker;

static void dest_spill_handler(Register_t *reg, StackNode_t *spill_slot,
                               void *context) {
  (void)reg;
  DestSpillTracker *tracker = (DestSpillTracker *)context;
  if (tracker == NULL || spill_slot == NULL)
    return;
  tracker->spill_slot = spill_slot;
}

/* If dest_reg was spilled while evaluating an intermediate expression, allocate
 * a fresh register, reload the saved value, and swap dest_reg to point at it.
 * The original physical register is now owned by whoever spilled us, so we
 * must NOT clobber it directly. */
static ListNode_t *codegen_reload_if_spilled(ListNode_t *inst_list,
                                             CodeGenContext *ctx,
                                             Register_t **dest_reg,
                                             DestSpillTracker *tracker) {
  if (tracker == NULL || tracker->spill_slot == NULL || dest_reg == NULL ||
      *dest_reg == NULL)
    return inst_list;

  Register_t *new_reg = get_free_reg(get_reg_stack(), &inst_list);
  if (new_reg == NULL)
    new_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
  if (new_reg == NULL)
    return inst_list;

  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
           tracker->spill_slot->offset, new_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);

  /* Release the old (spilled) Register_t entry and swap to the new one.
   * Clearing the spill callback is implicit since free_reg clears it. */
  free_reg(get_reg_stack(), *dest_reg);
  *dest_reg = new_reg;
  tracker->spill_slot = NULL;
  return inst_list;
}

static int codegen_assignment_type_is_class_vmt_value(const KgpcType *type) {
  if (type == NULL)
    return 0;

  if (type->type_alias != NULL && type->type_alias->is_class_reference)
    return 1;

  if (type->kind == TYPE_KIND_POINTER && type->info.points_to != NULL &&
      type->info.points_to->type_alias != NULL &&
      type->info.points_to->type_alias->is_class_reference) {
    return 1;
  }

  return 0;
}

int record_type_is_mp_integer(const struct RecordType *record_type) {
  if (record_type == NULL)
    return 0;

  if (record_type->fields == NULL || record_type->fields->next != NULL)
    return 0;

  struct RecordField *field = (struct RecordField *)record_type->fields->cur;
  if (field == NULL || field->name == NULL)
    return 0;

  return strcmp(field->name, "__kgpc_mp_handle") == 0;
}

int codegen_expr_is_mp_integer(struct Expression *expr) {
  if (expr == NULL)
    return 0;

  if (expr_has_type_tag(expr, RECORD_TYPE))
    return record_type_is_mp_integer(expr->record_type);

  if (expr_has_type_tag(expr, POINTER_TYPE)) {
    /* For pointers, check what they point to via KgpcType if available */
    if (expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_pointer(expr->resolved_kgpc_type)) {
      int subtype = kgpc_type_get_pointer_subtype_tag(expr->resolved_kgpc_type);
      if (subtype == RECORD_TYPE)
        return record_type_is_mp_integer(expr->record_type);
    }
    /* Fallback to legacy field */
    else if (expr->pointer_subtype == RECORD_TYPE) {
      return record_type_is_mp_integer(expr->record_type);
    }
  }

  return 0;
}

ListNode_t *codegen_call_mpint_assign(ListNode_t *inst_list,
                                      CodeGenContext *ctx, Register_t *addr_reg,
                                      Register_t *value_reg) {
  if (addr_reg == NULL || value_reg == NULL)
    return inst_list;

  if (codegen_target_is_windows()) {
    {
      Register_t *u[] = {value_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
    }
    {
      Register_t *u[] = {addr_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
    }
  } else {
    {
      Register_t *u[] = {value_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
    }
    {
      Register_t *u[] = {addr_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
    }
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list =
      codegen_call_with_shadow_space(inst_list, "kgpc_gmp_mpint_assign");
  return inst_list;
}

/* Move two registers into the first two ABI argument registers (arg0, arg1),
 * handling all possible register conflict scenarios. Uses xchgq when both
 * registers are cross-assigned, otherwise moves in the correct order.
 * arg0_reg → first ABI arg register, arg1_reg → second ABI arg register. */
static ListNode_t *codegen_setup_two_arg_regs(ListNode_t *inst_list,
                                              CodeGenContext *ctx,
                                              Register_t *arg0_reg,
                                              Register_t *arg1_reg) {
  char buffer[128];
  const char *abi_arg0 = codegen_target_is_windows() ? "%rcx" : "%rdi";
  const char *abi_arg1 = codegen_target_is_windows() ? "%rdx" : "%rsi";
  const char *tmpl_to_arg0 =
      codegen_target_is_windows() ? "\tmovq\t%0, %rcx\n" : "\tmovq\t%0, %rdi\n";
  const char *tmpl_to_arg1 =
      codegen_target_is_windows() ? "\tmovq\t%0, %rdx\n" : "\tmovq\t%0, %rsi\n";
  int arg0_id = codegen_target_is_windows() ? REG_RCX : REG_RDI;
  int arg1_id = codegen_target_is_windows() ? REG_RDX : REG_RSI;

  int val_in_arg0 = (arg1_reg->reg_id == arg0_id);
  int addr_in_arg1 = (arg0_reg->reg_id == arg1_id);

  if (val_in_arg0 && addr_in_arg1) {
    snprintf(buffer, sizeof(buffer), "\txchgq\t%s, %s\n", abi_arg0, abi_arg1);
    inst_list = add_inst(inst_list, buffer);
  } else if (val_in_arg0) {
    {
      Register_t *u[] = {arg1_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_to_arg1);
    }
    {
      Register_t *u[] = {arg0_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_to_arg0);
    }
  } else if (addr_in_arg1) {
    {
      Register_t *u[] = {arg0_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_to_arg0);
    }
    {
      Register_t *u[] = {arg1_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_to_arg1);
    }
  } else {
    {
      Register_t *u[] = {arg0_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_to_arg0);
    }
    {
      Register_t *u[] = {arg1_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl_to_arg1);
    }
  }

  return inst_list;
}

/* Call a 2-arg runtime function: func(addr_reg, value_reg)
 * addr_reg → first arg (char**), value_reg → second arg (const char*) */
ListNode_t *codegen_call_string_assign_func(ListNode_t *inst_list,
                                            CodeGenContext *ctx,
                                            Register_t *addr_reg,
                                            Register_t *value_reg,
                                            const char *func_name) {
  if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
    return inst_list;

  inst_list = codegen_setup_two_arg_regs(inst_list, ctx, addr_reg, value_reg);
  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, func_name);
  free_arg_regs();
  return inst_list;
}

ListNode_t *codegen_call_string_assign(ListNode_t *inst_list,
                                       CodeGenContext *ctx,
                                       Register_t *addr_reg,
                                       Register_t *value_reg) {
  return codegen_call_string_assign_func(inst_list, ctx, addr_reg, value_reg,
                                         "kgpc_string_assign");
}

int codegen_expr_is_wide_string_value(const struct Expression *expr) {
  if (expr == NULL)
    return 0;

  if (expr->resolved_kgpc_type != NULL) {
    if (kgpc_type_is_wide_string(expr->resolved_kgpc_type))
      return 1;

    if (expr->resolved_kgpc_type->type_alias != NULL) {
      const char *alias_name = expr->resolved_kgpc_type->type_alias->alias_name;
      const char *target_name =
          expr->resolved_kgpc_type->type_alias->target_type_id;
      if ((alias_name != NULL &&
           (pascal_identifier_equals(alias_name, "UnicodeString") ||
            pascal_identifier_equals(alias_name, "WideString"))) ||
          (target_name != NULL &&
           (pascal_identifier_equals(target_name, "UnicodeString") ||
            pascal_identifier_equals(target_name, "WideString")))) {
        return 1;
      }
    }
  }

  if (expr->type == EXPR_FUNCTION_CALL &&
      expr->expr_data.function_call_data.call_kgpc_type != NULL &&
      expr->expr_data.function_call_data.call_kgpc_type->kind ==
          TYPE_KIND_PROCEDURE) {
    KgpcType *call_type = expr->expr_data.function_call_data.call_kgpc_type;
    KgpcType *ret_type = kgpc_type_get_return_type(call_type);
    if (ret_type != NULL && kgpc_type_is_wide_string(ret_type))
      return 1;
    if (call_type->info.proc_info.return_type_id != NULL &&
        (pascal_identifier_equals(call_type->info.proc_info.return_type_id,
                                  "UnicodeString") ||
         pascal_identifier_equals(call_type->info.proc_info.return_type_id,
                                  "WideString"))) {
      return 1;
    }
  }

  if (expr->type == EXPR_TYPECAST &&
      expr->expr_data.typecast_data.target_type_id != NULL &&
      (pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id,
                                "UnicodeString") ||
       pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id,
                                "WideString"))) {
    return 1;
  }

  return 0;
}

/* Call kgpc_string_to_char_array(dest, src, size) to copy string to char array
 */
ListNode_t *codegen_call_string_to_char_array(ListNode_t *inst_list,
                                              CodeGenContext *ctx,
                                              Register_t *addr_reg,
                                              Register_t *value_reg,
                                              int array_size) {
  if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
    return inst_list;

  inst_list = codegen_setup_two_arg_regs(inst_list, ctx, addr_reg, value_reg);

  char buffer[128];
  if (codegen_target_is_windows()) {
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", array_size);
  } else {
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", array_size);
  }
  inst_list = add_inst(inst_list, buffer);

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list =
      codegen_call_with_shadow_space(inst_list, "kgpc_string_to_char_array");
  free_arg_regs();
  return inst_list;
}

/* Call kgpc_ansistr_to_widechararray(dest, src, dest_count) — used when
 * assigning an AnsiString or string literal into a fixed `array[..] of
 * WideChar` (e.g. FileRec.Name on Win64).  The third argument is the
 * WideChar element count, not the byte size. */
ListNode_t *codegen_call_ansistr_to_widechararray(ListNode_t *inst_list,
                                                  CodeGenContext *ctx,
                                                  Register_t *addr_reg,
                                                  Register_t *value_reg,
                                                  int dest_count) {
  if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
    return inst_list;

  inst_list = codegen_setup_two_arg_regs(inst_list, ctx, addr_reg, value_reg);

  char buffer[128];
  if (codegen_target_is_windows())
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", dest_count);
  else
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", dest_count);
  inst_list = add_inst(inst_list, buffer);

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list,
                                             "kgpc_ansistr_to_widechararray");
  free_arg_regs();
  return inst_list;
}

/* Call kgpc_shortstring_to_char_array(dest, src, size) */
ListNode_t *codegen_call_shortstring_to_char_array(ListNode_t *inst_list,
                                                   CodeGenContext *ctx,
                                                   Register_t *addr_reg,
                                                   Register_t *value_reg,
                                                   int array_size) {
  if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
    return inst_list;

  inst_list = codegen_setup_two_arg_regs(inst_list, ctx, addr_reg, value_reg);

  char buffer[128];
  if (codegen_target_is_windows()) {
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", array_size);
  } else {
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", array_size);
  }
  inst_list = add_inst(inst_list, buffer);

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list,
                                             "kgpc_shortstring_to_char_array");
  free_arg_regs();
  return inst_list;
}

/* Call kgpc_char_array_to_shortstring(dest, src, src_len, dest_size) */
ListNode_t *codegen_call_char_array_to_shortstring(ListNode_t *inst_list,
                                                   CodeGenContext *ctx,
                                                   Register_t *addr_reg,
                                                   Register_t *value_reg,
                                                   int src_len, int dest_size) {
  if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
    return inst_list;

  if (codegen_target_is_windows()) {
    /* Windows x64 ABI: rcx, rdx, r8, r9 */
    {
      Register_t *u[] = {addr_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
    }
    {
      Register_t *u[] = {value_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
    }
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", src_len);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r9\n", dest_size);
    inst_list = add_inst(inst_list, buffer);
  } else {
    /* System V ABI: rdi, rsi, rdx, rcx */
    {
      Register_t *u[] = {addr_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
    }
    {
      Register_t *u[] = {value_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
    }
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", src_len);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rcx\n", dest_size);
    inst_list = add_inst(inst_list, buffer);
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list,
                                             "kgpc_char_array_to_shortstring");
  free_arg_regs();
  return inst_list;
}

ListNode_t *codegen_call_string_assign_from_char_array(ListNode_t *inst_list,
                                                       CodeGenContext *ctx,
                                                       Register_t *addr_reg,
                                                       Register_t *value_reg,
                                                       int src_len) {
  if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
    return inst_list;

  if (codegen_target_is_windows()) {
    {
      Register_t *u[] = {addr_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
    }
    {
      Register_t *u[] = {value_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
    }
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", src_len);
    inst_list = add_inst(inst_list, buffer);
  } else {
    {
      Register_t *u[] = {addr_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
    }
    {
      Register_t *u[] = {value_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
    }
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", src_len);
    inst_list = add_inst(inst_list, buffer);
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(
      inst_list, "kgpc_string_assign_from_char_array");
  free_arg_regs();
  return inst_list;
}

/* Call kgpc_unicodestring_assign_from_widechar_array(dest, src, max_count) to
 * convert a fixed `array of WideChar` into a managed UnicodeString. */
ListNode_t *codegen_call_unicodestring_assign_from_widechar_array(
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t *addr_reg,
    Register_t *value_reg, int max_count) {
  if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
    return inst_list;

  if (codegen_target_is_windows()) {
    {
      Register_t *u[] = {addr_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
    }
    {
      Register_t *u[] = {value_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
    }
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", max_count);
    inst_list = add_inst(inst_list, buffer);
  } else {
    {
      Register_t *u[] = {addr_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
    }
    {
      Register_t *u[] = {value_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
    }
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", max_count);
    inst_list = add_inst(inst_list, buffer);
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(
      inst_list, "kgpc_unicodestring_assign_from_widechar_array");
  free_arg_regs();
  return inst_list;
}

/* Check if an array access expression targets a shortstring element.
 * This handles cases like Names[0] where Names is array[...] of ShortString. */
int codegen_array_access_targets_shortstring(const struct Expression *expr,
                                             CodeGenContext *ctx) {
  if (expr == NULL || ctx == NULL)
    return 0;
  if (expr->type != EXPR_ARRAY_ACCESS)
    return 0;
  if (expr->array_element_size == 2 ||
      (expr->array_element_type_id != NULL &&
       (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
        pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))))
    return 0;

  struct Expression *base_expr = expr->expr_data.array_access_data.array_expr;
  if (base_expr == NULL)
    return 0;
  if (base_expr->array_element_size == 2 ||
      (base_expr->array_element_type_id != NULL &&
       (pascal_identifier_equals(base_expr->array_element_type_id,
                                 "WideChar") ||
        pascal_identifier_equals(base_expr->array_element_type_id,
                                 "UnicodeChar"))))
    return 0;

  /* If the array access expression itself has shortstring type info, use that
   */
  if (codegen_expr_is_shortstring_array(expr))
    return 1;

  /* Check if base array is declared with shortstring element type */
  if (base_expr->type == EXPR_VAR_ID && ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, base_expr->expr_data.id) != 0 &&
        node != NULL) {
      /* Check KgpcType for array element info */
      if (node->type != NULL && kgpc_type_is_array(node->type)) {
        KgpcType *elem_type = kgpc_type_get_array_element_type(node->type);
        if (elem_type != NULL) {
          if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL) {
            fprintf(
                stderr,
                "[codegen] checking shortstring: base=%s elem_type->kind=%d\n",
                base_expr->expr_data.id, elem_type->kind);
          }
          /* Element type is a shortstring if it's an array of char with proper
           * bounds */
          if (kgpc_type_is_array(elem_type)) {
            KgpcType *inner_elem = kgpc_type_get_array_element_type(elem_type);
            if (inner_elem != NULL && inner_elem->kind == TYPE_KIND_PRIMITIVE &&
                inner_elem->info.primitive_type_tag == CHAR_TYPE) {
              return 1; /* array element is array of char = shortstring */
            }
          }
          /* Or if the element type tag is SHORTSTRING_TYPE */
          if (elem_type->kind == TYPE_KIND_PRIMITIVE &&
              elem_type->info.primitive_type_tag == SHORTSTRING_TYPE) {
            return 1;
          }
        }
      }
    }
  }

  return 0;
}

int codegen_expr_is_shortstring_array(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if ((expr->array_element_size == 2) ||
      (expr->array_element_type_id != NULL &&
       (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
        pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))))
    return 0;
  if (expr_get_type_tag(expr) == SHORTSTRING_TYPE)
    return 1;
  /* For record field access, only the RecordField's type is authoritative.
   * Plain array[0..255] of AnsiChar fields are NOT shortstrings. */
  if (expr->type == EXPR_RECORD_ACCESS) {
    struct RecordField *field =
        codegen_lookup_record_field((struct Expression *)expr);
    if (field != NULL && field->type == SHORTSTRING_TYPE)
      return 1;
    /* Field lookup is authoritative — if it succeeded, use its type.
     * If it failed, conservatively return 0 rather than relying on the
     * bounds heuristic, which would false-positive on plain char
     * array[0..255] fields like TextRec.Name. */
    return 0;
  }
  if (expr->resolved_kgpc_type != NULL) {
    if (kgpc_type_string_storage_kind(expr->resolved_kgpc_type) ==
        KGPC_STRING_STORAGE_SHORTSTRING)
      return 1;
  }
  return 0;
}

/* Detect whether @p expr resolves to a fixed `array[..] of WideChar` (or
 * UnicodeChar) destination — checks AST metadata, the symbol table, and the
 * resolved KgpcType so it also fires for VAR_ID + RECORD_ACCESS where the
 * element-type id is not threaded onto the access expression. Returns the
 * widechar element count (>0) on a positive hit, 0 otherwise. */
int codegen_dest_widechar_array_count(const struct Expression *expr,
                                      CodeGenContext *ctx) {
  if (expr == NULL)
    return 0;

  int count = 0;
  int matches_metadata =
      (expr->array_element_size == 2) ||
      (expr->array_element_type_id != NULL &&
       (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
        pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar")));

  if (matches_metadata && expr->is_array_expr) {
    int lo = expr_get_array_lower_bound(expr);
    int hi = expr_get_array_upper_bound(expr);
    if (hi >= lo)
      count = hi - lo + 1;
  }

  KgpcType *kgpc = expr_get_kgpc_type(expr);
  if (count == 0 && kgpc != NULL && kgpc_type_is_array(kgpc)) {
    KgpcType *elem = kgpc_type_get_array_element_type(kgpc);
    if (elem != NULL && kgpc_type_sizeof(elem) == 2 &&
        elem->kind == TYPE_KIND_PRIMITIVE &&
        elem->info.primitive_type_tag == CHAR_TYPE) {
      int start = 0, end = -1;
      if (kgpc_type_get_array_bounds(kgpc, &start, &end) == 0 && end >= start)
        count = end - start + 1;
    }
  }

  if (count == 0 && expr->type == EXPR_VAR_ID && ctx != NULL &&
      ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
        node != NULL && node->type != NULL && kgpc_type_is_array(node->type)) {
      KgpcType *elem = kgpc_type_get_array_element_type(node->type);
      if (elem != NULL && kgpc_type_sizeof(elem) == 2 &&
          elem->kind == TYPE_KIND_PRIMITIVE &&
          elem->info.primitive_type_tag == CHAR_TYPE) {
        int start = node->type->info.array_info.start_index;
        int end = node->type->info.array_info.end_index;
        if (end >= start)
          count = end - start + 1;
      }
    }
  }

  if (count == 0 && expr->type == EXPR_RECORD_ACCESS) {
    struct RecordField *field =
        codegen_lookup_record_field((struct Expression *)expr);
    if (field != NULL && field->is_array && !field->array_is_open &&
        field->array_end >= field->array_start) {
      int is_widechar = 0;
      if (field->array_element_kgpc_type != NULL) {
        KgpcType *fe = field->array_element_kgpc_type;
        if (kgpc_type_sizeof(fe) == 2 && fe->kind == TYPE_KIND_PRIMITIVE &&
            fe->info.primitive_type_tag == CHAR_TYPE)
          is_widechar = 1;
      }
      if (!is_widechar && field->array_element_type_id != NULL &&
          (pascal_identifier_equals(field->array_element_type_id, "WideChar") ||
           pascal_identifier_equals(field->array_element_type_id,
                                    "UnicodeChar")))
        is_widechar = 1;
      /* TFileTextRecChar resolves to UnicodeChar on Win64 (FPC RTL
       * systemh.inc); follow the type alias when the element id is a
       * named alias rather than the primitive itself. */
      if (!is_widechar && field->array_element_type_id != NULL &&
          ctx != NULL) {
        struct TypeAlias *alias =
            codegen_lookup_type_alias(ctx, field->array_element_type_id);
        if (alias != NULL && alias->target_type_id != NULL &&
            (pascal_identifier_equals(alias->target_type_id, "WideChar") ||
             pascal_identifier_equals(alias->target_type_id, "UnicodeChar")))
          is_widechar = 1;
      }
      if (is_widechar)
        count = field->array_end - field->array_start + 1;
    }
  }

  return count;
}

static int
codegen_expr_has_widechar_array_metadata_local(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if (expr->array_element_size == 2)
    return 1;
  if (expr->array_element_type_id != NULL &&
      (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
       pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))) {
    return 1;
  }
  return 0;
}

int codegen_expr_is_shortstring_value_local(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if (codegen_expr_has_widechar_array_metadata_local(expr))
    return 0;
  if (expr_get_type_tag(expr) == SHORTSTRING_TYPE)
    return 1;
  if (expr->resolved_kgpc_type != NULL) {
    if (kgpc_type_string_storage_kind(expr->resolved_kgpc_type) ==
        KGPC_STRING_STORAGE_SHORTSTRING)
      return 1;
  }
  return 0;
}

int codegen_shortstring_capacity_from_type_local(KgpcType *type) {
  if (type == NULL)
    return 0;

  struct TypeAlias *alias = kgpc_type_get_type_alias(type);
  if (alias != NULL && alias->is_shortstring) {
    if (alias->array_end >= alias->array_start && alias->array_end >= 0)
      return alias->array_end - alias->array_start + 1;
    if (alias->storage_size > 1)
      return (int)alias->storage_size;
    return 256;
  }

  if (kgpc_type_string_storage_kind(type) == KGPC_STRING_STORAGE_SHORTSTRING) {
    long long type_size = kgpc_type_sizeof(type);
    if (type_size > 1 && type_size <= INT_MAX)
      return (int)type_size;
    return 256;
  }

  return 0;
}

static int codegen_shortstring_capacity_from_array_element_local(
    const struct Expression *expr, CodeGenContext *ctx) {
  if (expr == NULL || expr->type != EXPR_ARRAY_ACCESS)
    return 0;

  struct Expression *base_expr = expr->expr_data.array_access_data.array_expr;
  if (base_expr == NULL)
    return 0;

  KgpcType *base_type = expr_get_kgpc_type(base_expr);
  if (base_type == NULL && base_expr->type == EXPR_VAR_ID && ctx != NULL &&
      ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, base_expr->expr_data.id) != 0 &&
        node != NULL) {
      base_type = node->type;
    }
  }

  if (base_type != NULL && kgpc_type_is_array(base_type)) {
    KgpcType *elem_type = kgpc_type_get_array_element_type(base_type);
    int capacity = codegen_shortstring_capacity_from_type_local(elem_type);
    if (capacity > 0)
      return capacity;
  }

  if (base_expr->type == EXPR_VAR_ID) {
    int scope_depth = 0;
    StackNode_t *stack_node =
        find_label_with_depth(base_expr->expr_data.id, &scope_depth);
    if (stack_node != NULL && stack_node->element_size > 1)
      return stack_node->element_size;
  }

  return 0;
}

static int
codegen_record_field_shortstring_capacity(const struct Expression *expr,
                                          CodeGenContext *ctx) {
  if (expr == NULL || expr->type != EXPR_RECORD_ACCESS)
    return 0;

  struct RecordField *field =
      codegen_lookup_record_field((struct Expression *)expr);
  if (field == NULL)
    return 0;

  int shortstring_like = (field->type == SHORTSTRING_TYPE);
  if (!shortstring_like && expr != NULL) {
    KgpcType *expr_type = expr_get_kgpc_type(expr);
    if (expr_type != NULL) {
      if (kgpc_type_string_storage_kind(expr_type) ==
          KGPC_STRING_STORAGE_SHORTSTRING)
        shortstring_like = 1;
    }
  }

  if (!shortstring_like)
    return 0;

  if (field->type == SHORTSTRING_TYPE) {
    if (field->is_array && field->array_end >= field->array_start &&
        field->array_end >= 0)
      return field->array_end - field->array_start + 1;
  }

  if (ctx != NULL) {
    long long field_size =
        codegen_record_field_effective_size((struct Expression *)expr, ctx);
    if (field_size > 1 && field_size <= INT_MAX)
      return (int)field_size;
  }

  return 0;
}
int codegen_is_current_return_var_id(const struct Expression *expr,
                                     CodeGenContext *ctx) {
  const char *expr_id = NULL;
  const char *current_id = NULL;
  HashNode_t *shadow_node = NULL;

  if (expr == NULL || ctx == NULL || expr->type != EXPR_VAR_ID)
    return 0;

  expr_id = expr->expr_data.id;
  if (expr_id == NULL)
    return 0;

  if (pascal_identifier_equals(expr_id, "Result")) {
    /* A real local/parameter named Result must win over the implicit
     * function-result designator. Only treat bare Result as implicit when
     * semantic lookup did not bind it to an actual symbol. */
    if (ctx->symtab != NULL &&
        FindSymbol(&shadow_node, ctx->symtab, expr_id) != 0 &&
        shadow_node != NULL)
      return 0;
    return 1;
  }

  current_id = ctx->current_subprogram_id;
  if (current_id == NULL)
    return 0;

  if (pascal_identifier_equals(expr_id, current_id))
    return 1;
  if (ctx->current_subprogram_method_name != NULL &&
      pascal_identifier_equals(expr_id, ctx->current_subprogram_method_name))
    return 1;
  if (ctx->current_subprogram_result_name != NULL &&
      pascal_identifier_equals(expr_id, ctx->current_subprogram_result_name))
    return 1;

  return 0;
}

int codegen_get_char_array_bounds(const struct Expression *expr,
                                  CodeGenContext *ctx, int *lower_out,
                                  int *upper_out, int *is_shortstring_out) {
  if (lower_out != NULL)
    *lower_out = 0;
  if (upper_out != NULL)
    *upper_out = -1;
  if (is_shortstring_out != NULL)
    *is_shortstring_out = 0;
  if (expr == NULL)
    return 0;

  int found = 0;
  int lower = 0;
  int upper = -1;

  if (expr != NULL && expr->type == EXPR_VAR_ID && ctx != NULL &&
      codegen_is_current_return_var_id(expr, ctx)) {
    int short_capacity =
        codegen_get_current_return_shortstring_capacity(ctx, ctx->symtab);
    if (short_capacity > 1) {
      lower = 0;
      upper = short_capacity - 1;
      found = 1;
      if (is_shortstring_out != NULL)
        *is_shortstring_out = 1;
    }
  }

  if (!found && expr->is_array_expr && expr->array_element_type == CHAR_TYPE) {
    lower = expr_get_array_lower_bound(expr);
    upper = expr_get_array_upper_bound(expr);
    found = 1;
  } else {
    KgpcType *kgpc = expr_get_kgpc_type(expr);
    int short_capacity = codegen_shortstring_capacity_from_type_local(kgpc);
    if (short_capacity > 0) {
      lower = 0;
      upper = short_capacity - 1;
      found = 1;
      if (is_shortstring_out != NULL)
        *is_shortstring_out = 1;
    } else if (kgpc != NULL && kgpc_type_is_array(kgpc) &&
               kgpc->info.array_info.element_type != NULL &&
               kgpc->info.array_info.element_type->kind ==
                   TYPE_KIND_PRIMITIVE &&
               kgpc->info.array_info.element_type->info.primitive_type_tag ==
                   CHAR_TYPE) {
      lower = kgpc->info.array_info.start_index;
      upper = kgpc->info.array_info.end_index;
      found = 1;
    } else if (expr->type == EXPR_VAR_ID && ctx != NULL &&
               ctx->symtab != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
          node != NULL && node->type != NULL) {
        int node_short_capacity =
            codegen_shortstring_capacity_from_type_local(node->type);
        if (node_short_capacity > 0) {
          lower = 0;
          upper = node_short_capacity - 1;
          found = 1;
          if (is_shortstring_out != NULL)
            *is_shortstring_out = 1;
        } else if (node->type->kind == TYPE_KIND_PROCEDURE &&
                   node->type->info.proc_info.return_type != NULL) {
          int return_short_capacity =
              codegen_shortstring_capacity_from_type_local(
                  node->type->info.proc_info.return_type);
          if (return_short_capacity > 0) {
            lower = 0;
            upper = return_short_capacity - 1;
            found = 1;
            if (is_shortstring_out != NULL)
              *is_shortstring_out = 1;
          }
        } else if (node->type->kind == TYPE_KIND_ARRAY &&
                   node->type->info.array_info.element_type != NULL &&
                   node->type->info.array_info.element_type->kind ==
                       TYPE_KIND_PRIMITIVE &&
                   node->type->info.array_info.element_type->info
                           .primitive_type_tag == CHAR_TYPE) {
          lower = node->type->info.array_info.start_index;
          upper = node->type->info.array_info.end_index;
          found = 1;
          if (is_shortstring_out != NULL && node->type->type_alias != NULL &&
              node->type->type_alias->is_shortstring) {
            *is_shortstring_out = 1;
          }
        }
      }

      if (!found && (expr_get_type_tag(expr) == SHORTSTRING_TYPE ||
                     codegen_expr_is_shortstring_value_local(expr))) {
        int scope_depth = 0;
        StackNode_t *stack_node =
            find_label_with_depth(expr->expr_data.id, &scope_depth);
        if (stack_node != NULL) {
          int slot_size = stack_node->element_size > 0
                              ? stack_node->element_size
                              : stack_node->size;
          if (slot_size > 1) {
            lower = 0;
            upper = slot_size - 1;
            found = 1;
            if (is_shortstring_out != NULL)
              *is_shortstring_out = 1;
          }
        }
      }
    } else if (expr->type == EXPR_RECORD_ACCESS) {
      /* Look up the record field to check if it's a char array */
      struct RecordField *field =
          codegen_lookup_record_field((struct Expression *)expr);
      int short_capacity = codegen_record_field_shortstring_capacity(expr, ctx);
      if (field != NULL && field->is_array &&
          (field->array_element_type == CHAR_TYPE ||
           field->array_element_type == BYTE_TYPE)) {
        lower = field->array_start;
        upper = field->array_end;
        found = 1;
        /* Record fields: only SHORTSTRING_TYPE fields are shortstrings.
         * Plain array[0..255] of AnsiChar is NOT a shortstring. */
        if (is_shortstring_out != NULL)
          *is_shortstring_out = (field->type == SHORTSTRING_TYPE) ? 1 : 0;
      } else if (short_capacity > 1) {
        lower = 0;
        upper = short_capacity - 1;
        found = 1;
        if (is_shortstring_out != NULL)
          *is_shortstring_out = 1;
      }
    }
  }

  if (!found)
    return 0;

  if (lower_out != NULL)
    *lower_out = lower;
  if (upper_out != NULL)
    *upper_out = upper;

  if (is_shortstring_out != NULL) {
    if (expr->type == EXPR_RECORD_ACCESS) {
      /* For record field access, the RecordField's type is authoritative.
       * Do NOT fall through to the array[0..255] heuristic. */
      struct RecordField *field =
          codegen_lookup_record_field((struct Expression *)expr);
      if (field != NULL && field->type == SHORTSTRING_TYPE)
        *is_shortstring_out = 1;
      else if (field != NULL)
        *is_shortstring_out = 0;
      else {
        /* Field lookup failed — conservatively treat as not-shortstring
         * to avoid false positives on plain char array[0..255] fields
         * like TextRec.Name.  codegen_expr_is_shortstring_array already
         * returns 0 in this case. */
        *is_shortstring_out = 0;
      }
    } else {
      int is_short = (*is_shortstring_out != 0);
      if (!is_short)
        is_short = codegen_expr_is_shortstring_array(expr);
      if (!is_short) {
        KgpcType *kgpc = expr_get_kgpc_type(expr);
        if (kgpc_type_string_storage_kind(kgpc) ==
            KGPC_STRING_STORAGE_SHORTSTRING)
          is_short = 1;
      }
      *is_shortstring_out = is_short;
    }
  }

  return 1;
}

/* Detect whether an expression represents a ShortString value on the RHS
 * of an assignment.  Checks context-aware shortstring detection, local
 * shortstring markers, the SHORTSTRING_TYPE tag, and typecast wrappers.
 * Used by both the "LHS is shortstring" and "LHS is AnsiString" assignment
 * branches to keep the detection logic in a single place. */
int codegen_expr_is_shortstring_rhs(const struct Expression *expr,
                                    CodeGenContext *ctx) {
  if (expr == NULL)
    return 0;

  if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL) {
    StackNode_t *slot = find_label(expr->expr_data.id);
    if (slot == NULL && ctx != NULL && ctx->symtab != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
          node != NULL && node->mangled_id != NULL) {
        slot = find_label(node->mangled_id);
      }
    }
    int slot_size = 0;
    if (slot != NULL && !slot->is_reference) {
      slot_size = slot->element_size > 0 ? slot->element_size : slot->size;
      if (slot_size > (int)CODEGEN_POINTER_SIZE_BYTES && slot_size <= 256 &&
          (expr_get_type_tag(expr) == STRING_TYPE ||
           expr_get_type_tag(expr) == SHORTSTRING_TYPE)) {
        return 1;
      }
    }
  }

  KgpcType *expr_type = expr_get_kgpc_type(expr);
  if (expr->type != EXPR_FUNCTION_CALL && expr_type != NULL &&
      kgpc_type_equals_tag(expr_type, STRING_TYPE) &&
      kgpc_type_string_storage_kind(expr_type) !=
          KGPC_STRING_STORAGE_SHORTSTRING) {
    struct TypeAlias *alias = kgpc_type_get_type_alias(expr_type);
    if (alias != NULL && !alias->is_shortstring &&
        alias->target_type_id != NULL &&
        (pascal_identifier_equals(alias->target_type_id, "AnsiString") ||
         pascal_identifier_equals(alias->target_type_id, "RawByteString") ||
         pascal_identifier_equals(alias->target_type_id, "UnicodeString") ||
         pascal_identifier_equals(alias->target_type_id, "WideString"))) {
      return 0;
    }
  }

  if (codegen_expr_is_shortstring_value_ctx(expr, ctx))
    return 1;
  if (codegen_expr_is_shortstring_value_local(expr))
    return 1;
  if (expr_get_type_tag(expr) == SHORTSTRING_TYPE)
    return 1;
  /* Unwrap typecasts: e.g. TFormatString(HexStr(...)) where the outer type
   * is AnsiString but the inner expression returns ShortString. */
  if (expr->type == EXPR_TYPECAST &&
      expr->expr_data.typecast_data.expr != NULL &&
      codegen_expr_is_shortstring_value_ctx(expr->expr_data.typecast_data.expr,
                                            ctx))
    return 1;
  return 0;
}

int codegen_get_shortstring_capacity(const struct Expression *expr,
                                     CodeGenContext *ctx) {
  int explicit_shortstring = 0;
  if (expr != NULL) {
    explicit_shortstring = (expr_get_type_tag(expr) == SHORTSTRING_TYPE) ||
                           codegen_expr_is_shortstring_array(expr);
  }

  if (expr != NULL && expr->type == EXPR_VAR_ID && ctx != NULL &&
      codegen_is_current_return_var_id(expr, ctx)) {
    int short_capacity =
        codegen_get_current_return_shortstring_capacity(ctx, ctx->symtab);
    if (short_capacity > 0)
      return short_capacity;
  }

  if (expr != NULL) {
    int record_field_capacity =
        codegen_record_field_shortstring_capacity(expr, ctx);
    if (record_field_capacity > 1)
      return record_field_capacity;

    int array_element_capacity =
        codegen_shortstring_capacity_from_array_element_local(expr, ctx);
    if (array_element_capacity > 1)
      return array_element_capacity;

    KgpcType *expr_type = expr_get_kgpc_type(expr);
    if (expr_type != NULL && expr_type->kind == TYPE_KIND_PRIMITIVE &&
        expr_type->info.primitive_type_tag == SHORTSTRING_TYPE) {
      int capacity = codegen_shortstring_capacity_from_type_local(expr_type);
      return capacity > 0 ? capacity : 256;
    }
    if (expr_type != NULL) {
      struct TypeAlias *alias = kgpc_type_get_type_alias(expr_type);
      if (alias != NULL && alias->is_shortstring &&
          alias->array_end >= alias->array_start && alias->array_end >= 0) {
        return alias->array_end - alias->array_start + 1;
      }
      if (kgpc_type_is_array(expr_type) && expr_type->type_alias != NULL &&
          expr_type->type_alias->is_shortstring) {
        int start = 0;
        int end = -1;
        if (kgpc_type_get_array_bounds(expr_type, &start, &end) == 0 &&
            end >= start && end >= 0) {
          return end - start + 1;
        }
      }
    }
  }

  if (explicit_shortstring) {
    if (expr != NULL && ctx != NULL) {
      int lower = 0, upper = -1, is_short = 0;
      if (codegen_get_char_array_bounds(expr, ctx, &lower, &upper, &is_short) &&
          is_short && upper >= lower && upper >= 0) {
        return upper - lower + 1;
      }
    }
    return 256;
  }

  if (expr != NULL && expr->is_array_expr) {
    int lower_bound = expr_get_array_lower_bound(expr);
    int upper_bound = expr_get_array_upper_bound(expr);
    if (upper_bound >= lower_bound && upper_bound >= 0)
      return upper_bound - lower_bound + 1;
  }

  if (expr != NULL && expr->type == EXPR_ARRAY_ACCESS) {
    struct Expression *base_expr = expr->expr_data.array_access_data.array_expr;
    KgpcType *base_type = NULL;

    if (base_expr != NULL) {
      base_type = base_expr->resolved_kgpc_type;
      if (base_type == NULL && base_expr->type == EXPR_VAR_ID && ctx != NULL &&
          ctx->symtab != NULL) {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, base_expr->expr_data.id) != 0 &&
            node != NULL) {
          base_type = node->type;
        }
      }
    }

    if (base_type != NULL && kgpc_type_is_array(base_type)) {
      KgpcType *elem_type = kgpc_type_get_array_element_type(base_type);
      if (elem_type != NULL && kgpc_type_is_array(elem_type)) {
        int start = 0;
        int end = -1;
        if (kgpc_type_get_array_bounds(elem_type, &start, &end) == 0 &&
            end >= start && end >= 0) {
          return end - start + 1;
        }
      }
    }
  }

  if (expr != NULL && expr->type == EXPR_VAR_ID && ctx != NULL &&
      ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
        node != NULL) {
      if (node->type != NULL) {
        int capacity = codegen_shortstring_capacity_from_type_local(node->type);
        if (capacity > 0)
          return capacity;

        if (kgpc_type_is_procedure(node->type) &&
            node->type->info.proc_info.return_type != NULL) {
          KgpcType *ret_type = node->type->info.proc_info.return_type;
          capacity = codegen_shortstring_capacity_from_type_local(ret_type);
          if (capacity > 0)
            return capacity;
        }
      }

      int start = 0;
      int end = -1;
      hashnode_get_array_bounds(node, &start, &end);
      if (end >= start && end >= 0)
        return end - start + 1;
    }
  }

  if (expr != NULL && expr->type == EXPR_VAR_ID &&
      (expr_get_type_tag(expr) == SHORTSTRING_TYPE ||
       codegen_expr_is_shortstring_value_local(expr))) {
    int scope_depth = 0;
    StackNode_t *stack_node =
        find_label_with_depth(expr->expr_data.id, &scope_depth);
    if (stack_node != NULL) {
      int slot_size = stack_node->element_size > 0 ? stack_node->element_size
                                                   : stack_node->size;
      if (slot_size > 1)
        return slot_size;
    }
  }

  return 256;
}

/* Call kgpc_string_to_shortstring(dest, src, size) to copy string to
 * ShortString */
ListNode_t *codegen_call_string_to_shortstring(ListNode_t *inst_list,
                                               CodeGenContext *ctx,
                                               Register_t *addr_reg,
                                               Register_t *value_reg,
                                               int array_size) {
  if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
    return inst_list;

  if (array_size <= 1)
    array_size = 256;

  char buffer[128];
  if (codegen_target_is_windows()) {
    /* Windows x64 ABI: first arg in %rcx, second in %rdx, third in %r8 */
    int value_in_rcx = (value_reg->reg_id == REG_RCX);
    int addr_in_rdx = (addr_reg->reg_id == REG_RDX);

    if (value_in_rcx && addr_in_rdx) {
      inst_list = add_inst(inst_list, "\txchgq\t%rcx, %rdx\n");
    } else if (value_in_rcx) {
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
      }
      {
        Register_t *u[] = {addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
      }
    } else if (addr_in_rdx) {
      {
        Register_t *u[] = {addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
      }
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
      }
    } else {
      {
        Register_t *u[] = {addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
      }
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
      }
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", array_size);
    inst_list = add_inst(inst_list, buffer);
  } else {
    /* System V ABI: first arg in %rdi, second in %rsi, third in %rdx */
    int value_in_rdi = (value_reg->reg_id == REG_RDI);
    int addr_in_rsi = (addr_reg->reg_id == REG_RSI);

    if (value_in_rdi && addr_in_rsi) {
      inst_list = add_inst(inst_list, "\txchgq\t%rdi, %rsi\n");
    } else if (value_in_rdi) {
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
      }
      {
        Register_t *u[] = {addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
      }
    } else if (addr_in_rsi) {
      {
        Register_t *u[] = {addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
      }
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
      }
    } else {
      {
        Register_t *u[] = {addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
      }
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
      }
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", array_size);
    inst_list = add_inst(inst_list, buffer);
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list =
      codegen_call_with_shadow_space(inst_list, "kgpc_string_to_shortstring");
  free_arg_regs();
  return inst_list;
}

/* PChar (NUL-terminated C string) → ShortString. Shares the same ABI shape
 * as codegen_call_string_to_shortstring but routes to a runtime helper that
 * never reinterprets src[0] as a length prefix — the source is unconditionally
 * a C string, so strlen+memcpy is the only correct lowering. */
ListNode_t *codegen_call_pchar_to_shortstring(ListNode_t *inst_list,
                                              CodeGenContext *ctx,
                                              Register_t *addr_reg,
                                              Register_t *value_reg,
                                              int array_size) {
  if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
    return inst_list;

  if (array_size <= 1)
    array_size = 256;

  char buffer[128];
  if (codegen_target_is_windows()) {
    int value_in_rcx = (value_reg->reg_id == REG_RCX);
    int addr_in_rdx = (addr_reg->reg_id == REG_RDX);

    if (value_in_rcx && addr_in_rdx) {
      inst_list = add_inst(inst_list, "\txchgq\t%rcx, %rdx\n");
    } else if (value_in_rcx) {
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
      }
      {
        Register_t *u[] = {addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
      }
    } else if (addr_in_rdx) {
      {
        Register_t *u[] = {addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
      }
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
      }
    } else {
      {
        Register_t *u[] = {addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
      }
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
      }
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", array_size);
    inst_list = add_inst(inst_list, buffer);
  } else {
    int value_in_rdi = (value_reg->reg_id == REG_RDI);
    int addr_in_rsi = (addr_reg->reg_id == REG_RSI);

    if (value_in_rdi && addr_in_rsi) {
      inst_list = add_inst(inst_list, "\txchgq\t%rdi, %rsi\n");
    } else if (value_in_rdi) {
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
      }
      {
        Register_t *u[] = {addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
      }
    } else if (addr_in_rsi) {
      {
        Register_t *u[] = {addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
      }
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
      }
    } else {
      {
        Register_t *u[] = {addr_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
      }
      {
        Register_t *u[] = {value_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
      }
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", array_size);
    inst_list = add_inst(inst_list, buffer);
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list =
      codegen_call_with_shadow_space(inst_list, "kgpc_pchar_to_shortstring");
  free_arg_regs();
  return inst_list;
}

ListNode_t *codegen_call_shortstring_copy(ListNode_t *inst_list,
                                          CodeGenContext *ctx,
                                          Register_t *dest_reg, int dest_size,
                                          Register_t *src_reg) {
  char buffer[128];

  if (codegen_target_is_windows()) {
    {
      Register_t *u[] = {dest_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", dest_size);
    inst_list = add_inst(inst_list, buffer);
    {
      Register_t *u[] = {src_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %r8\n");
    }
  } else {
    {
      Register_t *u[] = {dest_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rsi\n", dest_size);
    inst_list = add_inst(inst_list, buffer);
    {
      Register_t *u[] = {src_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
    }
  }

  inst_list = add_inst(inst_list, "\tmovl\t$0, %eax\n");
  inst_list = codegen_call_with_shadow_space(inst_list,
                                             "kgpc_shortstring_to_shortstring");
  free_arg_regs();
  return inst_list;
}

/* Assign a static array value (copy all elements) */
ListNode_t *codegen_assign_static_array(struct Expression *dest_expr,
                                        struct Expression *src_expr,
                                        ListNode_t *inst_list,
                                        CodeGenContext *ctx) {
  if (dest_expr == NULL || src_expr == NULL || ctx == NULL)
    return inst_list;

  /* If the destination is actually a dynamic array (e.g. typed constant
   * whose expression lacks array_is_dynamic), routing through
   * codegen_assign_dynamic_array avoids raw memcpy overflowing the
   * 16-byte descriptor slot. */
  KgpcType *dest_type = expr_get_kgpc_type(dest_expr);
  if (dest_type != NULL && kgpc_type_is_dynamic_array(dest_type))
    return codegen_assign_dynamic_array(dest_expr, src_expr, inst_list, ctx);

  int dest_lower = 0, dest_upper = -1, dest_is_shortstring = 0;
  int src_lower = 0, src_upper = -1, src_is_shortstring = 0;
  int dest_is_char_array = codegen_get_char_array_bounds(
      dest_expr, ctx, &dest_lower, &dest_upper, &dest_is_shortstring);
  int src_is_char_array = codegen_get_char_array_bounds(
      src_expr, ctx, &src_lower, &src_upper, &src_is_shortstring);
  int src_is_shortstring_value =
      codegen_expr_is_shortstring_value_ctx(src_expr, ctx);
  if (src_expr->type == EXPR_ARRAY_LITERAL) {
    src_is_shortstring = 0;
    src_is_shortstring_value = 0;
  }
  int dest_is_shortstring_value =
      codegen_expr_is_shortstring_value_ctx(dest_expr, ctx);

  if ((dest_is_shortstring || dest_is_shortstring_value) &&
      (src_is_shortstring || src_is_shortstring_value)) {
    Register_t *dest_reg = NULL;
    Register_t *src_reg = NULL;
    DestSpillTracker dest_tracker = {NULL};

    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL) {
      if (dest_reg != NULL)
        free_reg(get_reg_stack(), dest_reg);
      return inst_list;
    }

    /* Register a spill callback so if src_expr evaluation needs another
     * register and the spiller picks dest_reg, we can reload it before
     * passing it to codegen_call_shortstring_copy. Without this, the
     * physical register may carry an intermediate value (e.g. a typesym
     * field pointer) that the copy then treats as the destination,
     * silently corrupting the def from which it was loaded. */
    register_set_spill_callback(dest_reg, dest_spill_handler, &dest_tracker);

    inst_list = codegen_expr_with_result(src_expr, inst_list, ctx, &src_reg);
    if (codegen_had_error(ctx) || src_reg == NULL) {
      if (dest_reg != NULL) {
        register_clear_spill_callback(dest_reg);
        free_reg(get_reg_stack(), dest_reg);
      }
      if (src_reg != NULL)
        free_reg(get_reg_stack(), src_reg);
      return inst_list;
    }

    inst_list =
        codegen_reload_if_spilled(inst_list, ctx, &dest_reg, &dest_tracker);

    int dest_size = codegen_get_shortstring_capacity(dest_expr, ctx);
    if (dest_size <= 1)
      dest_size = 256;

    inst_list = codegen_call_shortstring_copy(inst_list, ctx, dest_reg,
                                              dest_size, src_reg);

    register_clear_spill_callback(dest_reg);
    free_reg(get_reg_stack(), dest_reg);
    free_reg(get_reg_stack(), src_reg);
    return inst_list;
  }

  if ((dest_is_shortstring || dest_is_shortstring_value) && src_is_char_array &&
      !src_is_shortstring && src_expr->type != EXPR_ARRAY_LITERAL) {
    Register_t *dest_reg = NULL;
    Register_t *src_reg = NULL;
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL || src_reg == NULL) {
      if (dest_reg != NULL)
        free_reg(get_reg_stack(), dest_reg);
      if (src_reg != NULL)
        free_reg(get_reg_stack(), src_reg);
      return inst_list;
    }

    int src_len = src_upper - src_lower + 1;
    int dest_size = dest_upper - dest_lower + 1;
    if (src_len < 0)
      src_len = 0;
    if (dest_size < 0)
      dest_size = 0;
    inst_list = codegen_call_char_array_to_shortstring(
        inst_list, ctx, dest_reg, src_reg, src_len, dest_size);
    free_reg(get_reg_stack(), dest_reg);
    free_reg(get_reg_stack(), src_reg);
    return inst_list;
  }

  if (dest_is_char_array && !dest_is_shortstring &&
      (src_is_shortstring || src_is_shortstring_value)) {
    Register_t *dest_reg = NULL;
    Register_t *src_reg = NULL;
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL || src_reg == NULL) {
      if (dest_reg != NULL)
        free_reg(get_reg_stack(), dest_reg);
      if (src_reg != NULL)
        free_reg(get_reg_stack(), src_reg);
      return inst_list;
    }

    int dest_size = dest_upper - dest_lower + 1;
    if (dest_size < 0)
      dest_size = 0;
    inst_list = codegen_call_shortstring_to_char_array(inst_list, ctx, dest_reg,
                                                       src_reg, dest_size);
    free_reg(get_reg_stack(), dest_reg);
    free_reg(get_reg_stack(), src_reg);
    return inst_list;
  }

  /* Calculate array size: (upper - lower + 1) * element_size */
  long long lower_bound = expr_get_array_lower_bound(dest_expr);
  long long upper_bound = expr_get_array_upper_bound(dest_expr);
  long long num_elements = -1;
  if (upper_bound >= lower_bound)
    num_elements = upper_bound - lower_bound + 1;

  if (num_elements <= 0) {
    if (dest_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_array(dest_expr->resolved_kgpc_type)) {
      int start = 0;
      int end = -1;
      if (kgpc_type_get_array_bounds(dest_expr->resolved_kgpc_type, &start,
                                     &end) == 0 &&
          end >= start) {
        num_elements = (long long)end - (long long)start + 1;
      }
    }

    if (num_elements <= 0) {
      if (dest_expr->type == EXPR_VAR_ID && ctx != NULL &&
          ctx->symtab != NULL && dest_expr->expr_data.id != NULL) {
        HashNode_t *var_node = NULL;
        if (FindSymbol(&var_node, ctx->symtab, dest_expr->expr_data.id) != 0 &&
            var_node != NULL && var_node->type != NULL &&
            kgpc_type_is_array(var_node->type)) {
          int start = 0;
          int end = -1;
          if (kgpc_type_get_array_bounds(var_node->type, &start, &end) == 0 &&
              end >= start) {
            num_elements = (long long)end - (long long)start + 1;
          }
        }
      }
    }

    /* For pointer dereference destinations (ptr^), extract the array
     * type from the pointer's pointee.  This handles typed constant
     * arrays assigned through pointer indirection. */
    if (num_elements <= 0 && dest_expr->type == EXPR_POINTER_DEREF) {
      KgpcType *deref_type = dest_expr->resolved_kgpc_type;
      /* Try the inner pointer expression's pointee type */
      if (deref_type == NULL || !kgpc_type_is_array(deref_type)) {
        struct Expression *ptr_expr =
            dest_expr->expr_data.pointer_deref_data.pointer_expr;
        if (ptr_expr != NULL && ptr_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_pointer(ptr_expr->resolved_kgpc_type)) {
          KgpcType *pointee = kgpc_type_resolve_pointer_pointee(
              ptr_expr->resolved_kgpc_type, ctx->symtab);
          if (pointee != NULL && kgpc_type_is_array(pointee))
            deref_type = pointee;
        }
        /* Also check pointer_subtype_id on the inner expression */
        if ((deref_type == NULL || !kgpc_type_is_array(deref_type)) &&
            ptr_expr != NULL && ptr_expr->pointer_subtype_id != NULL &&
            ctx->symtab != NULL) {
          HashNode_t *type_node = NULL;
          if (FindSymbol(&type_node, ctx->symtab,
                         ptr_expr->pointer_subtype_id) != 0 &&
              type_node != NULL && type_node->type != NULL &&
              kgpc_type_is_array(type_node->type)) {
            deref_type = type_node->type;
          }
        }
      }
      if (deref_type != NULL && kgpc_type_is_array(deref_type)) {
        int start = 0;
        int end = -1;
        if (kgpc_type_get_array_bounds(deref_type, &start, &end) == 0 &&
            end >= start) {
          num_elements = (long long)end - (long long)start + 1;
        }
      }
    }

    if (num_elements <= 0) {
      struct RecordField *field = codegen_lookup_record_field(dest_expr);
      if (field != NULL && field->is_array && !field->array_is_open)
        num_elements =
            (long long)field->array_end - (long long)field->array_start + 1;
      else if (field != NULL) {
        struct TypeAlias *alias =
            codegen_lookup_type_alias(ctx, field->type_id);
        if (alias != NULL && alias->is_array && !alias->is_open_array &&
            alias->array_end >= alias->array_start) {
          num_elements =
              (long long)alias->array_end - (long long)alias->array_start + 1;
        }
      }
    }

    /* Last resort: derive element count from source array literal.
     * Typed constant arrays (e.g. `const foo: array[0..N] of Rec = (...)`)
     * may have unresolved bounds on the destination when the type comes from
     * a cross-unit declaration.  The source literal knows its own length. */
    if (num_elements <= 0 && src_expr != NULL &&
        src_expr->type == EXPR_ARRAY_LITERAL &&
        src_expr->expr_data.array_literal_data.element_count > 0) {
      num_elements = src_expr->expr_data.array_literal_data.element_count;
    }
  }

  long long element_size = expr_get_array_element_size(dest_expr, ctx);

  /* For pointer dereference destinations, extract element size from
   * the pointer's pointee array type. */
  if (element_size <= 0 && dest_expr->type == EXPR_POINTER_DEREF) {
    struct Expression *ptr_expr =
        dest_expr->expr_data.pointer_deref_data.pointer_expr;
    KgpcType *arr_type = NULL;
    if (ptr_expr != NULL && ptr_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_pointer(ptr_expr->resolved_kgpc_type)) {
      KgpcType *pointee = kgpc_type_resolve_pointer_pointee(
          ptr_expr->resolved_kgpc_type, ctx->symtab);
      if (pointee != NULL && kgpc_type_is_array(pointee))
        arr_type = pointee;
    }
    if (arr_type == NULL && ptr_expr != NULL &&
        ptr_expr->pointer_subtype_id != NULL && ctx->symtab != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, ctx->symtab, ptr_expr->pointer_subtype_id) !=
              0 &&
          type_node != NULL && type_node->type != NULL &&
          kgpc_type_is_array(type_node->type)) {
        arr_type = type_node->type;
      }
    }
    if (arr_type != NULL) {
      long long elem_size = kgpc_type_get_array_element_size(arr_type);
      if (elem_size <= 0) {
        KgpcType *elem_type =
            kgpc_type_get_array_element_type_resolved(arr_type, ctx->symtab);
        if (elem_type != NULL)
          elem_size = kgpc_type_sizeof(elem_type);
      }
      if (elem_size > 0)
        element_size = elem_size;
    }
  }

  if (element_size <= 0) {
    if (dest_expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL &&
        dest_expr->expr_data.id != NULL) {
      HashNode_t *var_node = NULL;
      if (FindSymbol(&var_node, ctx->symtab, dest_expr->expr_data.id) != 0 &&
          var_node != NULL && var_node->type != NULL &&
          kgpc_type_is_array(var_node->type)) {
        long long elem_size = kgpc_type_get_array_element_size(var_node->type);
        if (elem_size <= 0) {
          KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(
              var_node->type, ctx->symtab);
          if (elem_type != NULL)
            elem_size = kgpc_type_sizeof(elem_type);
        }
        if (elem_size > 0)
          element_size = elem_size;
      }
    }
  }

  if (element_size <= 0) {
    struct RecordField *field = codegen_lookup_record_field(dest_expr);
    if (field != NULL) {
      long long computed = 0;
      if (codegen_sizeof_type_reference(ctx, field->array_element_type,
                                        field->array_element_type_id,
                                        field->nested_record, &computed) == 0 &&
          computed > 0) {
        element_size = computed;
      } else {
        struct TypeAlias *alias =
            codegen_lookup_type_alias(ctx, field->type_id);
        if (alias != NULL && alias->is_array) {
          long long alias_size = 0;
          if (codegen_sizeof_type_reference(ctx, alias->array_element_type,
                                            alias->array_element_type_id, NULL,
                                            &alias_size) == 0 &&
              alias_size > 0) {
            element_size = alias_size;
          }
        }
      }
    }

    /* Fall back to the source array literal's element size when the
     * destination type information is incomplete (cross-unit typed consts). */
    if (element_size <= 0 && src_expr != NULL &&
        src_expr->type == EXPR_ARRAY_LITERAL &&
        src_expr->array_element_size > 0) {
      element_size = src_expr->array_element_size;
    }
    if (element_size <= 0 && src_expr != NULL &&
        src_expr->type == EXPR_ARRAY_LITERAL) {
      element_size = expr_get_array_element_size(src_expr, ctx);
    }

    if (element_size <= 0) {
      codegen_report_error(
          ctx, "ERROR: Unable to determine element size for array assignment.");
      return inst_list;
    }
  }

  long long array_size = num_elements * element_size;
  if (array_size <= 0) {
    struct RecordField *field = codegen_lookup_record_field(dest_expr);
    if (field != NULL) {
      long long total_size = 0;
      if (codegen_sizeof_type_reference(ctx, field->type, field->type_id,
                                        field->nested_record,
                                        &total_size) == 0 &&
          total_size > 0) {
        array_size = total_size;
        num_elements = 1;
        element_size = total_size;
      }
    }

    /* Last resort: use sizeof the destination's resolved type directly.
     * This handles pointer dereference destinations (ptr^) where the
     * pointee is a fixed-size type (e.g. char array behind PChar). */
    if (array_size <= 0 && dest_expr->resolved_kgpc_type != NULL) {
      long long type_size = kgpc_type_sizeof(dest_expr->resolved_kgpc_type);
      if (type_size > 0) {
        array_size = type_size;
        num_elements = 1;
        element_size = type_size;
      }
    }

    if (array_size <= 0) {
      const char *dest_name = NULL;
      if (dest_expr != NULL) {
        if (dest_expr->type == EXPR_VAR_ID)
          dest_name = dest_expr->expr_data.id;
        else if (dest_expr->type == EXPR_RECORD_ACCESS)
          dest_name = dest_expr->expr_data.record_access_data.field_id;
      }
      codegen_report_error(
          ctx,
          "ERROR: Invalid array size for assignment: %lld elements * %lld "
          "bytes = %lld total (dest_type=%d%s%s).",
          num_elements, element_size, array_size,
          dest_expr != NULL ? dest_expr->type : -1,
          dest_name != NULL ? " dest=" : "",
          dest_name != NULL ? dest_name : "");
      return inst_list;
    }
  }

  /* Check if the source expression involves a function call.
   * If so, we need to spill the destination address to a stack slot
   * because the function call can clobber any caller-saved register. */
  int src_has_function_call =
      (src_expr->type == EXPR_FUNCTION_CALL) || expr_returns_sret(src_expr);
  StackNode_t *dest_spill_slot = NULL;

  /* Get address of destination */
  Register_t *dest_reg = NULL;
  inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
  if (codegen_had_error(ctx) || dest_reg == NULL) {
    if (dest_reg != NULL)
      free_reg(get_reg_stack(), dest_reg);
    return inst_list;
  }

  /* If source has a function call, spill destination address to preserve it */
  if (src_has_function_call) {
    dest_spill_slot = add_l_t("array_dest_spill");
    if (dest_spill_slot != NULL) {
      char tmpl[96];
      snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, -%d(%%rbp)\n",
               dest_spill_slot->offset);
      Register_t *u[] = {dest_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
    }
  }

  /* Get address of source */
  Register_t *src_reg = NULL;
  inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
  if (codegen_had_error(ctx) || src_reg == NULL) {
    if (src_reg != NULL)
      free_reg(get_reg_stack(), src_reg);
    free_reg(get_reg_stack(), dest_reg);
    return inst_list;
  }

  /* For EXPR_ARRAY_LITERAL, codegen_address_for_expr returns a pointer to a
   * descriptor {data_ptr, count}, not the data itself.  Dereference the
   * descriptor to get the actual data pointer for the memcpy source. */
  if (src_expr->type == EXPR_ARRAY_LITERAL &&
      src_expr->expr_data.array_literal_data.element_count > 0) {
    Register_t *du[] = {src_reg};
    inst_list = add_inst_du(inst_list, ctx, du, 1, du, 1, "\tmovq\t(%0), %0\n");
  }

  /* If we spilled the destination, reload it */
  if (dest_spill_slot != NULL) {
    char tmpl[96];
    snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%rbp), %%0\n",
             dest_spill_slot->offset);
    Register_t *d[] = {dest_reg};
    inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
  }

  /* Call memcpy(dest, src, size).  The byte count is a compile-time constant,
   * so load it straight into the ABI count register (%r8 on Win64, %rdx on
   * SysV) rather than allocating a scratch register — the array copy then
   * needs no extra register and can't fail under register pressure.  Move
   * dest/src to their ABI registers first, then load the immediate last, in
   * case a pool register physically aliases the count register. */
  if (codegen_target_is_windows()) {
    /* Windows calling convention: RCX (dest), RDX (src), R8 (size) */
    {
      Register_t *u[] = {dest_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
    }
    {
      Register_t *u[] = {src_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
    }
    {
      char tmpl[64];
      snprintf(tmpl, sizeof(tmpl), "\tmovq\t$%lld, %%r8\n", array_size);
      inst_list = add_inst(inst_list, tmpl);
    }
  } else {
    /* System V calling convention: RDI (dest), RSI (src), RDX (size) */
    {
      Register_t *u[] = {dest_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
    }
    {
      Register_t *u[] = {src_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
    }
    {
      char tmpl[64];
      snprintf(tmpl, sizeof(tmpl), "\tmovq\t$%lld, %%rdx\n", array_size);
      inst_list = add_inst(inst_list, tmpl);
    }
  }

  inst_list = add_inst(inst_list, "\tcall\tkgpc_memcpy_wrapper\n");

  free_reg(get_reg_stack(), src_reg);
  free_reg(get_reg_stack(), dest_reg);
  free_arg_regs();
  return inst_list;
}

ListNode_t *codegen_assign_record_value(struct Expression *dest_expr,
                                        struct Expression *src_expr,
                                        ListNode_t *inst_list,
                                        CodeGenContext *ctx) {
  if (dest_expr == NULL || src_expr == NULL || ctx == NULL)
    return inst_list;

  if (src_expr->type == EXPR_RECORD_CONSTRUCTOR &&
      src_expr->record_type == NULL) {
    struct RecordType *dest_record = dest_expr->record_type;
    if (dest_record == NULL) {
      KgpcType *dest_type = expr_get_kgpc_type(dest_expr);
      if (dest_type != NULL && kgpc_type_is_record(dest_type))
        dest_record = kgpc_type_get_record(dest_type);
    }
    if (dest_record != NULL) {
      src_expr->record_type = dest_record;
      if (src_expr->resolved_kgpc_type == NULL) {
        KgpcType *record_type = create_record_type(dest_record);
        if (record_type != NULL) {
          src_expr->resolved_kgpc_type = record_type;
        }
      }
    }
  }

  /* Check if this is a class assignment. Classes are represented as pointers,
   * so we should just copy the pointer value, not the entire instance. */
  int is_class_assignment = 0;
  if (dest_expr->record_type != NULL &&
      record_type_is_class(dest_expr->record_type))
    is_class_assignment = 1;
  else if (src_expr->record_type != NULL &&
           record_type_is_class(src_expr->record_type))
    is_class_assignment = 1;
  else if (codegen_assignment_type_is_class_vmt_value(
               expr_get_kgpc_type(dest_expr)) ||
           codegen_assignment_type_is_class_vmt_value(
               expr_get_kgpc_type(src_expr)))
    is_class_assignment = 1;

  if (is_class_assignment) {
    /* For class variables, just copy the pointer (8 bytes) */
    Register_t *dest_reg = NULL;
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL) {
      if (dest_reg != NULL)
        free_reg(get_reg_stack(), dest_reg);
      return inst_list;
    }

    Register_t *src_reg = NULL;

    /* For function calls (especially constructors), the expression evaluates to
     * the pointer value directly. For variable references, we need to load the
     * pointer from the variable. We check if the source is addressable to
     * distinguish these cases. */
    int src_is_addressable = codegen_expr_is_addressable(src_expr);

    if (src_is_addressable) {
      /* Source is a variable - get its address and load the pointer value */
      inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
      if (codegen_had_error(ctx) || src_reg == NULL) {
        if (src_reg != NULL)
          free_reg(get_reg_stack(), src_reg);
        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
      }

      /* Load the pointer value from the variable */
      Register_t *ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
      if (ptr_reg == NULL) {
        free_reg(get_reg_stack(), dest_reg);
        free_reg(get_reg_stack(), src_reg);
        return codegen_fail_register(
            ctx, inst_list, NULL,
            "ERROR: Unable to allocate register for class pointer copy.");
      }

      {
        Register_t *d[] = {ptr_reg};
        Register_t *u[] = {src_reg};
        inst_list =
            add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovq\t(%1), %0\n");
      }
      {
        Register_t *u[] = {ptr_reg, dest_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\tmovq\t%0, (%1)\n");
      }
      free_reg(get_reg_stack(), ptr_reg);
      free_reg(get_reg_stack(), src_reg);
    } else {
      /* Source is a function call or expression that returns the pointer value
       * directly. Save dest_reg to the stack before evaluating source to
       * prevent it from being clobbered. */
      StackNode_t *dest_save_slot =
          add_l_x("__class_assign_dest__", CODEGEN_POINTER_SIZE_BYTES);
      if (dest_save_slot == NULL) {
        codegen_report_error(ctx, "ERROR: Unable to reserve stack slot for "
                                  "class assignment destination.");
        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
      }

      {
        char tmpl[96];
        snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, -%d(%%rbp)\n",
                 dest_save_slot->offset);
        Register_t *u[] = {dest_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
      }
      free_reg(get_reg_stack(), dest_reg);
      dest_reg = NULL;

      /* Evaluate the source expression (constructor call) */
      inst_list = codegen_expr_with_result(src_expr, inst_list, ctx, &src_reg);
      if (codegen_had_error(ctx) || src_reg == NULL) {
        if (src_reg != NULL)
          free_reg(get_reg_stack(), src_reg);
        return inst_list;
      }

      /* Pre-spill src_reg to its own slot and free its physical register
       * BEFORE acquiring dest_reg.  Under heavy register pressure (e.g.
       * deeply nested constructor chains like nmem.pas:1406's
       * tvecnode.left := ctypeconvnode.create_internal(
       *                    ccallnode.createintern('fpc_..._unique',
       *                      ccallparanode.create(
       *                        ctypeconvnode.create_internal(left,voidpointertype),
       *                        nil)),
       *                    left.resultdef);
       * ), the register allocator can hand dest_reg the SAME physical
       * register that src_reg still logically owns — get_free_reg can
       * recycle a register the RHS-evaluation pass internally freed,
       * even though logically src_reg owns it.  The subsequent reload
       * of dest_reg from dest_save_slot then clobbers src_reg's value,
       * and the final "movq %src, (%dest)" collapses into a self-store
       * "movq %X, (%X)" — turning the destination field into a
       * self-pointer.  At the next virtual dispatch on that field
       * (firstpass(left) in tvecnode.pass_1 line 1410) the VMT slot
       * reads from the destination field itself and the indirect call
       * jumps through NULL, crashing pp_bootstrap while compiling
       * aasmbase.pas's ApplyAsmSymbolRestrictions.
       *
       * Mirrors the same fix bfc48be5 applied to
       * codegen_var_assignment's EXPR_RECORD_ACCESS branch in
       * codegen_stmt_calls_and_control.c, but this path goes through
       * codegen_assign_record_value's is_class_assignment branch
       * instead, which the original commit did not cover. */
      StackNode_t *src_save_slot =
          add_l_x("__class_assign_src__", CODEGEN_POINTER_SIZE_BYTES);
      if (src_save_slot != NULL) {
        char tmpl[96];
        snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, -%d(%%rbp)\n",
                 src_save_slot->offset);
        Register_t *u[] = {src_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
        free_reg(get_reg_stack(), src_reg);
        src_reg = NULL;
      }

      /* Restore dest_reg from stack */
      dest_reg = get_free_reg(get_reg_stack(), &inst_list);
      if (dest_reg == NULL) {
        if (src_reg != NULL)
          free_reg(get_reg_stack(), src_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
                                     "ERROR: Unable to allocate register for "
                                     "class assignment destination restore.");
      }
      {
        char tmpl[96];
        snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%rbp), %%0\n",
                 dest_save_slot->offset);
        Register_t *d[] = {dest_reg};
        inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
      }

      /* Allocate a guaranteed-distinct register for src_reg's reload. */
      if (src_save_slot != NULL) {
        src_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (src_reg == NULL) {
          free_reg(get_reg_stack(), dest_reg);
          return codegen_fail_register(ctx, inst_list, NULL,
                                       "ERROR: Unable to allocate register for "
                                       "class assignment source reload.");
        }
        char tmpl[96];
        snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%rbp), %%0\n",
                 src_save_slot->offset);
        Register_t *d[] = {src_reg};
        inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
      }

      /* src_reg already contains the pointer value - store it directly */
      {
        Register_t *u[] = {src_reg, dest_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\tmovq\t%0, (%1)\n");
      }

      free_reg(get_reg_stack(), src_reg);
    }

    if (dest_reg != NULL)
      free_reg(get_reg_stack(), dest_reg);
    return inst_list;
  }

  long long record_size = 0;
  int size_status = codegen_get_record_size(ctx, dest_expr, &record_size);
  if (size_status != 0) {
    size_status = codegen_get_record_size(ctx, src_expr, &record_size);
    if (size_status != 0) {
      codegen_report_error(
          ctx, "ERROR: Unable to determine record size for assignment.");
      return inst_list;
    }
  }

  if (record_size <= 0)
    return inst_list;

  Register_t *dest_reg = NULL;
  Register_t *src_reg = NULL;
  int dest_is_char_set = expr_is_char_set_ctx(dest_expr, ctx);

  /* Default(TRecord) intrinsic: zero-initialize destination without evaluating
   * source */
  if (src_expr->is_default_initializer) {
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL) {
      if (dest_reg != NULL)
        free_reg(get_reg_stack(), dest_reg);
      return inst_list;
    }

    const char *val_arg_reg = codegen_target_is_windows() ? "%rdx" : "%rsi";
    const char *size_arg_reg = codegen_target_is_windows() ? "%r8" : "%rdx";

    {
      const char *tmpl = codegen_target_is_windows() ? "\tmovq\t%0, %rcx\n"
                                                     : "\tmovq\t%0, %rdi\n";
      Register_t *u[] = {dest_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
    }
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\txorq\t%%rax, %%rax\n");
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", val_arg_reg);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", record_size,
             size_arg_reg);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "memset");
    free_arg_regs();

    free_reg(get_reg_stack(), dest_reg);
    return inst_list;
  }

  if (!codegen_expr_is_addressable(src_expr)) {
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL) {
      if (dest_reg != NULL)
        free_reg(get_reg_stack(), dest_reg);
      return inst_list;
    }

    if (src_expr->type == EXPR_FUNCTION_CALL) {
      struct KgpcType *func_type =
          codegen_resolve_function_call_type(ctx, src_expr, NULL);

      const char *func_mangled_name =
          src_expr->expr_data.function_call_data.mangled_id;
      const char *func_id = src_expr->expr_data.function_call_data.id;

      /* Handle string function results assigned to ShortString arrays.
       * Functions like Copy return AnsiString, which needs to be converted to
       * ShortString format. */
      int dest_is_shortstring =
          codegen_expr_is_shortstring_array(dest_expr) ||
          codegen_expr_is_shortstring_value_ctx(dest_expr, ctx);
      int src_returns_string = (expr_get_type_tag(src_expr) == STRING_TYPE);

      if (dest_is_shortstring && src_returns_string) {
        int src_returns_shortstring_sret =
            expr_returns_sret(src_expr) ||
            codegen_expr_is_shortstring_value_ctx(src_expr, ctx);

        /* Save dest address to stack before calling function (function call may
         * clobber registers) */
        StackNode_t *dest_save_slot =
            add_l_x("__shortstring_dest__", CODEGEN_POINTER_SIZE_BYTES);
        if (dest_save_slot == NULL) {
          codegen_report_error(ctx, "ERROR: Unable to reserve stack slot for "
                                    "ShortString destination.");
          free_reg(get_reg_stack(), dest_reg);
          return inst_list;
        }
        {
          char tmpl[96];
          snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, -%d(%%rbp)\n",
                   dest_save_slot->offset);
          Register_t *u[] = {dest_reg};
          inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
        }
        free_reg(get_reg_stack(), dest_reg);
        dest_reg = NULL;

        /* Call the function to get the string result in %rax */
        inst_list = codegen_expr(src_expr, inst_list, ctx);
        if (codegen_had_error(ctx)) {
          return inst_list;
        }

        /* The string result is in %rax - save it to a register */
        Register_t *value_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (value_reg == NULL) {
          return codegen_fail_register(ctx, inst_list, NULL,
                                       "ERROR: Unable to allocate register for "
                                       "string-to-shortstring conversion.");
        }

        {
          Register_t *d[] = {value_reg};
          inst_list =
              add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t%rax, %0\n");
        }

        /* Reload dest address from stack */
        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL) {
          free_reg(get_reg_stack(), value_reg);
          return codegen_fail_register(ctx, inst_list, NULL,
                                       "ERROR: Unable to allocate register for "
                                       "ShortString destination address.");
        }
        {
          char tmpl[96];
          snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%rbp), %%0\n",
                   dest_save_slot->offset);
          Register_t *d[] = {addr_reg};
          inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
        }

        /* Get ShortString capacity */
        int array_size = codegen_get_shortstring_capacity(dest_expr, ctx);
        if (array_size <= 1)
          array_size = 256;

        if (src_returns_shortstring_sret)
          inst_list = codegen_call_shortstring_copy(inst_list, ctx, addr_reg,
                                                    array_size, value_reg);
        else
          inst_list = codegen_call_string_to_shortstring(
              inst_list, ctx, addr_reg, value_reg, array_size);

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
      }

      int call_returns_sret = expr_returns_sret(src_expr);
      int call_returns_record = call_returns_sret;
      if (!call_returns_record && func_type != NULL &&
          kgpc_type_is_procedure(func_type)) {
        KgpcType *return_type = kgpc_type_get_return_type(func_type);
        if (return_type != NULL &&
            (kgpc_type_is_record(return_type) ||
             (return_type->kind == TYPE_KIND_ARRAY &&
              !kgpc_type_is_dynamic_array(return_type)) ||
             kgpc_type_is_shortstring(return_type) ||
             (return_type->type_alias != NULL &&
              return_type->type_alias->is_shortstring))) {
          call_returns_record = 1;
        }
      }
      if (!call_returns_record && src_expr->resolved_kgpc_type != NULL) {
        KgpcType *src_type = src_expr->resolved_kgpc_type;
        if (kgpc_type_is_record(src_type) ||
            (src_type->kind == TYPE_KIND_ARRAY &&
             !kgpc_type_is_dynamic_array(src_type)) ||
            (src_type->type_alias != NULL &&
             src_type->type_alias->is_shortstring)) {
          call_returns_record = 1;
        }
      }
      if (!call_returns_sret && call_returns_record && record_size > 8)
        call_returns_sret = 1;

      /* Detect constructors from semantic checker flag. */
      int is_constructor =
          src_expr->expr_data.function_call_data.is_constructor_call;

      /* Constructor chaining: when a constructor calls another constructor
       * on Self (e.g., Create(name, mode, 438) inside TFileStream.Create),
       * it's a regular method call, not a new allocation. The first arg
       * will be Self, injected by the semantic checker. */
      if (is_constructor &&
          src_expr->expr_data.function_call_data.args_expr != NULL) {
        struct Expression *first_arg =
            (struct Expression *)
                src_expr->expr_data.function_call_data.args_expr->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
            first_arg->expr_data.id != NULL &&
            pascal_identifier_equals(first_arg->expr_data.id, "Self"))
          is_constructor = 0;
      }
      /* Record constructor invoked via the type name (e.g. TRec.Create(...)):
       * the constructor's hidden Self parameter receives the address of the
       * assignment destination (return-value optimization). The constructor
       * writes the fields in place and has no separate return value, so this is
       * neither the class-constructor (heap-allocating) path nor the sret path.
       * Mirror the instance form r.Create(...) where Self = &r. The semantic
       * checker marks this with is_constructor_call and inserts an EXPR_NIL Self
       * placeholder as the first argument (see SemCheck_funccall_method.c). */
      if (is_constructor && expr_has_type_tag(dest_expr, RECORD_TYPE) &&
          src_expr->expr_data.function_call_data.args_expr != NULL) {
        struct Expression *self_arg =
            (struct Expression *)
                src_expr->expr_data.function_call_data.args_expr->cur;
        if (self_arg != NULL && self_arg->type == EXPR_NIL) {
          StackNode_t *dest_save_slot =
              add_l_x("__record_ctor_dest__", CODEGEN_POINTER_SIZE_BYTES);
          if (dest_save_slot == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to reserve stack slot for "
                                      "record constructor destination.");
            free_reg(get_reg_stack(), dest_reg);
            return inst_list;
          }
          {
            char tmpl[96];
            snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, -%d(%%rbp)\n",
                     dest_save_slot->offset);
            Register_t *u[] = {dest_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
          }
          free_reg(get_reg_stack(), dest_reg);

          /* Pass the user arguments (the list after the NIL Self placeholder)
           * into arg registers 1, 2, ... leaving register 0 for Self. The
           * placeholder list node itself must be skipped, otherwise it would be
           * emitted into the first user-argument register. */
          inst_list = codegen_pass_arguments(
              src_expr->expr_data.function_call_data.args_expr->next, inst_list,
              ctx, func_type, func_id, 1, src_expr, 0);

          /* Load the destination address into the Self register (arg reg 0)
           * after argument evaluation so it cannot be clobbered. */
          {
            const char *self_reg = current_arg_reg64(0);
            char tmpl[96];
            snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%rbp), %s\n",
                     dest_save_slot->offset, self_reg);
            inst_list = add_inst(inst_list, tmpl);
          }

          char buffer[128];
          snprintf(buffer, sizeof(buffer), "\tcall\t%s\n",
                   func_mangled_name ? func_mangled_name : func_id);
          inst_list = add_inst(inst_list, buffer);
          inst_list = codegen_cleanup_call_stack(inst_list, ctx);
          codegen_release_function_call_mangled_id(src_expr);
          return inst_list;
        }
      }
      /* Record static factories can also be named Create but they are not
       * class constructors and must not use constructor/sret calling paths. */
      if (is_constructor && expr_has_type_tag(dest_expr, RECORD_TYPE))
        is_constructor = 0;
      if (is_constructor && func_type != NULL &&
          kgpc_type_is_procedure(func_type)) {
        KgpcType *ret_type = kgpc_type_get_return_type(func_type);
        if (ret_type == NULL || !kgpc_type_is_pointer(ret_type) ||
            ret_type->info.points_to == NULL ||
            !kgpc_type_is_record(ret_type->info.points_to) ||
            !record_type_is_class(ret_type->info.points_to->info.record_info)) {
          is_constructor = 0;
        }
      }

      if (call_returns_record || is_constructor) {
        /* For constructors, allocate heap memory and initialize VMT */
        Register_t *constructor_instance_reg = NULL;
        if (is_constructor) {
          /* Get the class type from the source expression or first argument */
          struct RecordType *class_record = src_expr->record_type;
          if (class_record == NULL && src_expr->resolved_kgpc_type != NULL) {
            KgpcType *src_type = src_expr->resolved_kgpc_type;
            if (src_type->kind == TYPE_KIND_RECORD)
              class_record = src_type->info.record_info;
            else if (src_type->kind == TYPE_KIND_POINTER &&
                     src_type->info.points_to != NULL &&
                     src_type->info.points_to->kind == TYPE_KIND_RECORD)
              class_record = src_type->info.points_to->info.record_info;
          }

          if (class_record == NULL) {
            ListNode_t *first_arg =
                src_expr->expr_data.function_call_data.args_expr;
            if (first_arg != NULL && first_arg->cur != NULL) {
              struct Expression *class_expr =
                  (struct Expression *)first_arg->cur;
              if (class_expr != NULL) {
                class_record = class_expr->record_type;
                if (class_record == NULL &&
                    class_expr->resolved_kgpc_type != NULL) {
                  KgpcType *arg_type = class_expr->resolved_kgpc_type;
                  if (arg_type->kind == TYPE_KIND_RECORD)
                    class_record = arg_type->info.record_info;
                  else if (arg_type->kind == TYPE_KIND_POINTER &&
                           arg_type->info.points_to != NULL &&
                           arg_type->info.points_to->kind == TYPE_KIND_RECORD)
                    class_record = arg_type->info.points_to->info.record_info;
                }
                if (class_record == NULL && class_expr->type == EXPR_VAR_ID &&
                    class_expr->expr_data.id != NULL && ctx != NULL &&
                    ctx->symtab != NULL) {
                  HashNode_t *class_node = NULL;
                  if (FindSymbol(&class_node, ctx->symtab,
                                 class_expr->expr_data.id) != 0 &&
                      class_node != NULL &&
                      class_node->hash_type == HASHTYPE_TYPE &&
                      class_node->type != NULL) {
                    if (class_node->type->kind == TYPE_KIND_RECORD)
                      class_record = class_node->type->info.record_info;
                    else if (class_node->type->kind == TYPE_KIND_POINTER &&
                             class_node->type->info.points_to != NULL &&
                             class_node->type->info.points_to->kind ==
                                 TYPE_KIND_RECORD)
                      class_record =
                          class_node->type->info.points_to->info.record_info;
                  }
                }
              }
            }
          }

          if (class_record != NULL && record_type_is_class(class_record)) {
            /* Get the size of the class instance */
            long long instance_size = 0;
            if (codegen_sizeof_record_type(ctx, class_record, &instance_size) ==
                    0 &&
                instance_size > 0) {
              char buffer[128];

              /* Save dest_reg to stack since it will be clobbered by function
               * calls */
              StackNode_t *dest_save_slot =
                  add_l_x("__constructor_dest__", CODEGEN_POINTER_SIZE_BYTES);
              if (dest_save_slot == NULL) {
                codegen_report_error(ctx, "ERROR: Unable to reserve stack slot "
                                          "for constructor destination.");
                free_reg(get_reg_stack(), dest_reg);
                return inst_list;
              }
              {
                char tmpl[96];
                snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, -%d(%%rbp)\n",
                         dest_save_slot->offset);
                Register_t *u[] = {dest_reg};
                inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
              }

              /* Allocate zero-initialized memory through the runtime helper. */
              const char *alloc_arg_reg =
                  codegen_target_is_windows() ? "%rcx" : "%rdi";
              snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n",
                       instance_size, alloc_arg_reg);
              inst_list = add_inst(inst_list, buffer);
              inst_list = codegen_vect_reg(inst_list, 0);
              inst_list =
                  codegen_call_with_shadow_space(inst_list, "kgpc_allocmem");
              free_arg_regs();

              /* Save the allocated instance pointer */
              constructor_instance_reg =
                  get_reg_with_spill(get_reg_stack(), &inst_list);
              if (constructor_instance_reg == NULL) {
                codegen_report_error(ctx, "ERROR: Unable to allocate register "
                                          "for constructor instance.");
                free_reg(get_reg_stack(), dest_reg);
                return inst_list;
              }

              {
                Register_t *d[] = {constructor_instance_reg};
                inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0,
                                        "\tmovq\t%rax, %0\n");
              }

              /* Initialize VMT pointer in the allocated instance */
              /* Use the class's TYPEINFO label instead of evaluating the first
               * argument to avoid side effects like storing into the
               * destination variable */
              const char *class_type_id = class_record->type_id;
              if (class_type_id != NULL) {
                /* Load VMT address */
                Register_t *vmt_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (vmt_reg != NULL) {
                  {
                    char tmpl[256];
                    snprintf(tmpl, sizeof(tmpl), "\tleaq\t%s_VMT(%%rip), %%0\n",
                             class_type_id);
                    Register_t *d[] = {vmt_reg};
                    inst_list =
                        add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
                  }

                  /* Store VMT into first 8 bytes of instance */
                  {
                    Register_t *u[] = {vmt_reg, constructor_instance_reg};
                    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 2,
                                            "\tmovq\t%0, (%1)\n");
                  }

                  free_reg(get_reg_stack(), vmt_reg);
                }

                inst_list = codegen_emit_interface_vtable_slot_init(
                    inst_list, ctx, class_record, class_type_id,
                    constructor_instance_reg);
              }

              /* Pass remaining arguments starting from index 1 (skip class type
               * argument) */
              inst_list = codegen_pass_arguments(
                  src_expr->expr_data.function_call_data.args_expr, inst_list,
                  ctx, func_type, func_id, 1, src_expr, 0);

              /* Emit Self AFTER argument evaluation so that arg-passing code
               * cannot clobber the Self register (e.g. %rdi on SysV). */
              {
                const char *tmpl = codegen_target_is_windows()
                                       ? "\tmovq\t%0, %rcx\n"
                                       : "\tmovq\t%0, %rdi\n";
                Register_t *u[] = {constructor_instance_reg};
                inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
              }

              /* Call the constructor */
              snprintf(buffer, sizeof(buffer), "\tcall\t%s\n",
                       func_mangled_name);
              inst_list = add_inst(inst_list, buffer);
              inst_list = codegen_cleanup_call_stack(inst_list, ctx);
              codegen_release_function_call_mangled_id(src_expr);

              /* Restore dest_reg from stack */
              {
                char tmpl[96];
                snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%rbp), %%0\n",
                         dest_save_slot->offset);
                Register_t *d[] = {dest_reg};
                inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
              }

              /* Store the instance pointer in the destination */
              {
                Register_t *u[] = {constructor_instance_reg, dest_reg};
                inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 2,
                                        "\tmovq\t%0, (%1)\n");
              }

              free_reg(get_reg_stack(), constructor_instance_reg);
              free_reg(get_reg_stack(), dest_reg);
              return inst_list;
            }
          }
        }

        /* Normal record-returning function via sret pointer. */
        if (call_returns_sret) {
          const char *ret_ptr_reg = current_arg_reg64(0);
          if (ret_ptr_reg == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to determine register for "
                                      "record return pointer.");
            free_reg(get_reg_stack(), dest_reg);
            return inst_list;
          }

          StackNode_t *dest_save_slot =
              add_l_x("__record_call_dest__", CODEGEN_POINTER_SIZE_BYTES);
          if (dest_save_slot == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to reserve stack slot for "
                                      "record return destination.");
            free_reg(get_reg_stack(), dest_reg);
            return inst_list;
          }

          {
            char tmpl[96];
            snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, -%d(%%rbp)\n",
                     dest_save_slot->offset);
            Register_t *u[] = {dest_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
          }

          inst_list = codegen_pass_arguments(
              src_expr->expr_data.function_call_data.args_expr, inst_list, ctx,
              func_type, src_expr->expr_data.function_call_data.id, 1, src_expr,
              0);

          char buffer[128];
          snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                   dest_save_slot->offset, ret_ptr_reg);
          inst_list = add_inst(inst_list, buffer);

          /* For class method calls, dereference Self to get VMT pointer.
           * Self is at arg reg 1 (after SRET buffer at arg reg 0). */
          if (src_expr->expr_data.function_call_data.is_class_method_call) {
            const char *self_reg = current_arg_reg64(1);
            snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", self_reg,
                     self_reg);
            inst_list = add_inst(inst_list, buffer);
          }

          snprintf(buffer, sizeof(buffer), "\tcall\t%s\n",
                   src_expr->expr_data.function_call_data.mangled_id);
          inst_list = add_inst(inst_list, buffer);
          inst_list = codegen_cleanup_call_stack(inst_list, ctx);
          codegen_release_function_call_mangled_id(src_expr);

          free_reg(get_reg_stack(), dest_reg);
          return inst_list;
        }
      }

      /* Small record returns (<= 8 bytes) are returned in registers, not via
       * sret. Materialize the call result into a register and store the raw
       * value bytes directly into the destination record slot. */
      if (!is_constructor && call_returns_record && record_size <= 8) {
        StackNode_t *dest_save_slot =
            add_l_x("__small_record_dest__", CODEGEN_POINTER_SIZE_BYTES);
        if (dest_save_slot == NULL) {
          free_reg(get_reg_stack(), dest_reg);
          return inst_list;
        }

        {
          char tmpl[96];
          snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, -%d(%%rbp)\n",
                   dest_save_slot->offset);
          Register_t *u[] = {dest_reg};
          inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
        }
        free_reg(get_reg_stack(), dest_reg);
        dest_reg = NULL;

        Register_t *value_reg = NULL;
        inst_list =
            codegen_expr_with_result(src_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL) {
          if (value_reg != NULL)
            free_reg(get_reg_stack(), value_reg);
          return inst_list;
        }

        dest_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (dest_reg == NULL) {
          free_reg(get_reg_stack(), value_reg);
          return inst_list;
        }
        {
          char tmpl[96];
          snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%rbp), %%0\n",
                   dest_save_slot->offset);
          Register_t *d[] = {dest_reg};
          inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
        }

        if (record_size <= 4) {
          /* For movl, ir_emit_function uses bit_32 for ALL placeholders.
           * The address register in (%1) must remain 64-bit on x86-64, so
           * dest_reg->bit_64 is embedded in the template string directly.
           * value_reg uses %0 which correctly expands to bit_32 for movl. */
          char tmpl[64];
          snprintf(tmpl, sizeof(tmpl), "\tmovl\t%%0, (%s)\n", dest_reg->bit_64);
          Register_t *u[] = {value_reg};
          inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
        } else {
          Register_t *u[] = {value_reg, dest_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\tmovq\t%0, (%1)\n");
        }

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
      }
    }

    /* Handle character set literals - they generate a temporary buffer address
     */
    if (src_expr->type == EXPR_SET &&
        (expr_is_char_set_ctx(src_expr, ctx) || dest_is_char_set)) {
      /* Generate the set literal, which returns an address register */
      Register_t *src_reg = NULL;
      int force_char_literal =
          dest_is_char_set && !expr_is_char_set_ctx(src_expr, ctx);
      inst_list = codegen_set_literal(src_expr, inst_list, ctx, &src_reg,
                                      force_char_literal);
      if (codegen_had_error(ctx) || src_reg == NULL) {
        if (src_reg != NULL)
          free_reg(get_reg_stack(), src_reg);
        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
      }

      /* src_reg now contains the address of the temporary set buffer */
      /* Copy 32 bytes from src to dest via kgpc_memcpy_wrapper(dest, src, 32).
       * The byte count is the compile-time constant 32, so load it straight
       * into the ABI count register (%r8 on Win64, %rdx on SysV) rather than
       * allocating a scratch register for it — this keeps the set copy from
       * needing a third register and so it never fails under register
       * pressure.  Emit the immediate load last, after dest/src have been
       * moved out, in case either physically aliases the count register. */
      if (codegen_target_is_windows()) {
        {
          Register_t *u[] = {dest_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
        }
        {
          Register_t *u[] = {src_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
        }
        inst_list = add_inst(inst_list, "\tmovq\t$32, %r8\n");
      } else {
        {
          Register_t *u[] = {dest_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
        }
        {
          Register_t *u[] = {src_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
        }
        inst_list = add_inst(inst_list, "\tmovq\t$32, %rdx\n");
      }

      inst_list = add_inst(inst_list, "\tcall\tkgpc_memcpy_wrapper\n");

      free_reg(get_reg_stack(), src_reg);
      free_reg(get_reg_stack(), dest_reg);
      free_arg_regs();
      return inst_list;
    }

    /* Handle character set binary operations (union, intersection, difference).
     * For 32-byte sets (set of Char), the standard binary codegen only operates
     * on 4 bytes. Use runtime helpers that operate on all 32 bytes. */
    if (dest_is_char_set &&
        (src_expr->type == EXPR_ADDOP || src_expr->type == EXPR_MULOP)) {
      const char *runtime_func = NULL;
      struct Expression *left_op = NULL;
      struct Expression *right_op = NULL;

      if (src_expr->type == EXPR_ADDOP) {
        int op = src_expr->expr_data.addop_data.addop_type;
        left_op = src_expr->expr_data.addop_data.left_expr;
        right_op = src_expr->expr_data.addop_data.right_term;
        if (op == PLUS)
          runtime_func = "kgpc_set_union_256";
        else if (op == MINUS)
          runtime_func = "kgpc_set_diff_256";
      } else /* EXPR_MULOP */
      {
        int op = src_expr->expr_data.mulop_data.mulop_type;
        left_op = src_expr->expr_data.mulop_data.left_term;
        right_op = src_expr->expr_data.mulop_data.right_factor;
        if (op == STAR)
          runtime_func = "kgpc_set_intersect_256";
      }

      if (runtime_func != NULL && left_op != NULL && right_op != NULL) {
        /* Get addresses of both operands */
        Register_t *left_reg = NULL;
        Register_t *right_reg = NULL;

        inst_list =
            codegen_char_set_address(left_op, inst_list, ctx, &left_reg);
        if (codegen_had_error(ctx) || left_reg == NULL) {
          if (left_reg != NULL)
            free_reg(get_reg_stack(), left_reg);
          free_reg(get_reg_stack(), dest_reg);
          return inst_list;
        }

        inst_list =
            codegen_char_set_address(right_op, inst_list, ctx, &right_reg);
        if (codegen_had_error(ctx) || right_reg == NULL) {
          if (right_reg != NULL)
            free_reg(get_reg_stack(), right_reg);
          free_reg(get_reg_stack(), left_reg);
          free_reg(get_reg_stack(), dest_reg);
          return inst_list;
        }

        /* Call runtime_func(dest, left, right) */
        if (codegen_target_is_windows()) {
          {
            Register_t *u[] = {dest_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rcx\n");
          }
          {
            Register_t *u[] = {left_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdx\n");
          }
          {
            Register_t *u[] = {right_reg};
            inst_list =
                add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %r8\n");
          }
        } else {
          {
            Register_t *u[] = {dest_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdi\n");
          }
          {
            Register_t *u[] = {left_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rsi\n");
          }
          {
            Register_t *u[] = {right_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdx\n");
          }
        }

        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, runtime_func);

        free_reg(get_reg_stack(), right_reg);
        free_reg(get_reg_stack(), left_reg);
        free_reg(get_reg_stack(), dest_reg);
        free_arg_regs();
        return inst_list;
      }
    }

    /* Handle string literal assigned to ShortString (record-like) destination.
     * ShortStrings use Pascal format: length byte at index 0, followed by
     * string data. We need to convert the C string literal to a ShortString. */
    if (src_expr->type == EXPR_STRING) {
      const char *str_data = src_expr->expr_data.string;
      int str_len = (str_data != NULL) ? (int)strlen(str_data) : 0;
      if (str_len > 255)
        str_len = 255; /* ShortString max length */

      /* Put string literal in rodata section */
      const char *readonly_section = codegen_readonly_section_directive();
      char label[64];
      snprintf(label, sizeof(label), ".LC%d", ctx->write_label_counter++);

      char escaped_str[CODEGEN_MAX_INST_BUF];
      escape_string(escaped_str, str_data ? str_data : "", sizeof(escaped_str));
      /* Use larger buffer for string literal embedding to avoid truncation */
      char str_literal_buffer[CODEGEN_MAX_INST_BUF + 128];
      snprintf(str_literal_buffer, sizeof(str_literal_buffer),
               "%s\n%s:\n\t.string \"%s\"\n%s\n", readonly_section, label,
               escaped_str, codegen_text_section_resume());
      inst_list = add_inst(inst_list, str_literal_buffer);

      /* Get register for string literal address */
      Register_t *str_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
      if (str_addr_reg == NULL) {
        free_reg(get_reg_stack(), dest_reg);
        codegen_report_error(
            ctx,
            "ERROR: Unable to allocate register for string literal address.");
        return inst_list;
      }

      /* Load string literal address */
      {
        char tmpl[256];
        snprintf(tmpl, sizeof(tmpl), "\tleaq\t%s(%%rip), %%0\n", label);
        Register_t *d[] = {str_addr_reg};
        inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
      }

      /* Call kgpc_string_to_shortstring(dest, src, max_len).
       * Use the declared capacity for string[N] (= N+1) to avoid
       * overflowing smaller-than-255 buffers.
       * codegen_get_shortstring_capacity returns 256 when capacity
       * cannot be determined; codegen_call_string_to_shortstring
       * also guards against invalid (<= 1) values internally. */
      int dest_capacity = codegen_get_shortstring_capacity(dest_expr, ctx);
      char buffer[128];
      if (codegen_target_is_windows()) {
        {
          Register_t *u[] = {dest_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
        }
        {
          Register_t *u[] = {str_addr_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
        }
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%r8d\n", dest_capacity);
        inst_list = add_inst(inst_list, buffer);
      } else {
        {
          Register_t *u[] = {dest_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
        }
        {
          Register_t *u[] = {str_addr_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
        }
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", dest_capacity);
        inst_list = add_inst(inst_list, buffer);
      }

      inst_list = codegen_vect_reg(inst_list, 0);
      inst_list = codegen_call_with_shadow_space(inst_list,
                                                 "kgpc_string_to_shortstring");
      free_arg_regs();

      free_reg(get_reg_stack(), str_addr_reg);
      free_reg(get_reg_stack(), dest_reg);
      return inst_list;
    }

    {
      Register_t *src_addr_reg = NULL;
      inst_list =
          codegen_address_for_expr(src_expr, inst_list, ctx, &src_addr_reg);
      if (!codegen_had_error(ctx) && src_addr_reg != NULL) {
        long long size_val = record_size > 0 ? record_size : 1;

        if (codegen_target_is_windows()) {
          {
            Register_t *u[] = {dest_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rcx\n");
          }
          {
            Register_t *u[] = {src_addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdx\n");
          }
          char copy_buf[128];
          snprintf(copy_buf, sizeof(copy_buf), "\tmovq\t$%lld, %%r8\n",
                   size_val);
          inst_list = add_inst(inst_list, copy_buf);
        } else {
          {
            Register_t *u[] = {dest_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rdi\n");
          }
          {
            Register_t *u[] = {src_addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovq\t%0, %rsi\n");
          }
          char copy_buf[128];
          snprintf(copy_buf, sizeof(copy_buf), "\tmovq\t$%lld, %%rdx\n",
                   size_val);
          inst_list = add_inst(inst_list, copy_buf);
        }

        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list =
            codegen_call_with_shadow_space(inst_list, "kgpc_memcpy_wrapper");
        free_arg_regs();

        free_reg(get_reg_stack(), src_addr_reg);
        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
      }
      if (src_addr_reg != NULL)
        free_reg(get_reg_stack(), src_addr_reg);
    }

    codegen_report_error(
        ctx, "ERROR: Unsupported record-valued source expression (type=%d).",
        src_expr ? src_expr->type : -1);
    free_reg(get_reg_stack(), dest_reg);
    return inst_list;
  }

  int src_has_call =
      expr_contains_function_call(src_expr) ||
      (src_expr != NULL && src_expr->type == EXPR_RECORD_CONSTRUCTOR);
  int dest_has_call =
      expr_contains_function_call(dest_expr) ||
      (dest_expr != NULL && dest_expr->type == EXPR_RECORD_CONSTRUCTOR);
  StackNode_t *dest_spill = NULL;
  StackNode_t *src_spill = NULL;

  if (src_has_call && !dest_has_call) {
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL) {
      if (dest_reg != NULL)
        free_reg(get_reg_stack(), dest_reg);
      return inst_list;
    }
    dest_spill = add_l_t_bytes("record_copy_dest_spill", 8);
    if (dest_spill == NULL) {
      free_reg(get_reg_stack(), dest_reg);
      return inst_list;
    }
    {
      char tmpl[96];
      snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, -%d(%%rbp)\n",
               dest_spill->offset);
      Register_t *u[] = {dest_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
    }

    inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
    if (codegen_had_error(ctx) || src_reg == NULL) {
      if (src_reg != NULL)
        free_reg(get_reg_stack(), src_reg);
      free_reg(get_reg_stack(), dest_reg);
      return inst_list;
    }
    {
      char tmpl[96];
      snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%rbp), %%0\n",
               dest_spill->offset);
      Register_t *d[] = {dest_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
    }
  } else if (dest_has_call && !src_has_call) {
    inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
    if (codegen_had_error(ctx) || src_reg == NULL) {
      if (src_reg != NULL)
        free_reg(get_reg_stack(), src_reg);
      return inst_list;
    }
    src_spill = add_l_t_bytes("record_copy_src_spill", 8);
    if (src_spill == NULL) {
      free_reg(get_reg_stack(), src_reg);
      return inst_list;
    }
    {
      char tmpl[96];
      snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, -%d(%%rbp)\n",
               src_spill->offset);
      Register_t *u[] = {src_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
    }

    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL) {
      if (dest_reg != NULL)
        free_reg(get_reg_stack(), dest_reg);
      free_reg(get_reg_stack(), src_reg);
      return inst_list;
    }
    {
      char tmpl[96];
      snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%rbp), %%0\n",
               src_spill->offset);
      Register_t *d[] = {src_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, tmpl);
    }
  } else {
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL) {
      if (dest_reg != NULL)
        free_reg(get_reg_stack(), dest_reg);
      return inst_list;
    }

    inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
    if (codegen_had_error(ctx) || src_reg == NULL) {
      if (src_reg != NULL)
        free_reg(get_reg_stack(), src_reg);
      free_reg(get_reg_stack(), dest_reg);
      return inst_list;
    }
  }

  if (codegen_target_is_windows()) {
    {
      Register_t *u[] = {dest_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
    }
    {
      Register_t *u[] = {src_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
    }
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %%r8\n", record_size);
    inst_list = add_inst(inst_list, buffer);
  } else {
    {
      Register_t *u[] = {dest_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
    }
    {
      Register_t *u[] = {src_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
    }
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %%rdx\n", record_size);
    inst_list = add_inst(inst_list, buffer);
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");

  free_reg(get_reg_stack(), dest_reg);
  free_reg(get_reg_stack(), src_reg);
  free_arg_regs();
  return inst_list;
}
