/*
    Record access, pointer dereference, addressof, and set-literal emission for
   x86-64. Extracted from codegen_expression.c — see
   codegen_expression_internal.h for shared helpers.
*/

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(__GLIBC__) || (defined(__APPLE__) && defined(__MACH__)) ||         \
    defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#define HAVE_EXECINFO 1
#include <execinfo.h>
#endif

/* Forward declarations for unresolved method stubs — implementation after
 * includes. */

#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/KgpcType.h"
#include "../../Parser/ParseTree/from_cparser.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../Parser/SemanticCheck/SemCheck.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_Expr_Internal.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_expr.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../../Parser/pascal_frontend.h"
#include "../../flags.h"
#include "../../format_arg.h"
#include "../../identifier_utils.h"
#include "../../unit_registry.h"
#include "codegen.h"
#include "codegen_expr_arguments.h"
#include "codegen_expr_array.h"
#include "codegen_expr_relop.h"
#include "codegen_expr_sizeof.h"
#include "codegen_expression.h"
#include "codegen_expression_internal.h"
#include "expr_tree/expr_tree.h"
#include "register_types.h"
#include "stackmng/stackmng.h"

ListNode_t *codegen_pointer_deref_leaf(struct Expression *expr,
                                       ListNode_t *inst_list,
                                       CodeGenContext *ctx,
                                       Register_t *target_reg) {
  if (expr == NULL || ctx == NULL || target_reg == NULL)
    return inst_list;

  struct Expression *pointer_expr =
      expr->expr_data.pointer_deref_data.pointer_expr;
  if (pointer_expr == NULL) {
    codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
    return inst_list;
  }

  expr_node_t *pointer_tree = build_expr_tree(pointer_expr);
  Register_t *addr_reg =
      codegen_try_get_reg(&inst_list, ctx, "pointer dereference");
  if (addr_reg == NULL) {
    free_expr_tree(pointer_tree);
    return inst_list;
  }

  inst_list = gencode_expr_tree(pointer_tree, inst_list, ctx, addr_reg);
  free_expr_tree(pointer_tree);

  KgpcType *deref_type = expr_get_kgpc_type(expr);
  if (deref_type == NULL) {
    KgpcType *pointer_type = expr_get_kgpc_type(pointer_expr);
    if (pointer_type == NULL && pointer_expr->type == EXPR_VAR_ID &&
        ctx->symtab != NULL && pointer_expr->expr_data.id != NULL) {
      HashNode_t *pointer_node = NULL;
      if (FindSymbol(&pointer_node, ctx->symtab, pointer_expr->expr_data.id) !=
              0 &&
          pointer_node != NULL)
        pointer_type = pointer_node->type;
    }
    if (pointer_type != NULL && kgpc_type_is_pointer(pointer_type))
      deref_type = pointer_type->info.points_to;
  }
  if (deref_type != NULL) {
    struct TypeAlias *alias = kgpc_type_get_type_alias(deref_type);
    int is_set_type =
        (deref_type->kind == TYPE_KIND_PRIMITIVE &&
         deref_type->info.primitive_type_tag == SET_TYPE) ||
        (alias != NULL && alias->is_set);
    int keep_by_address =
        kgpc_type_is_record(deref_type) || kgpc_type_is_array(deref_type) ||
        kgpc_type_is_shortstring(deref_type) ||
        (alias != NULL && alias->is_shortstring);
    /* Small (<=4-byte, register-resident) sets must be LOADED as a value
     * (movl/movq) just like any scalar: `in`, `=` and `:=` all expect the
     * set bitmask in the register, not its address.  Only large (>4-byte,
     * 32-byte char) sets are handled by address like records/arrays.  Keeping
     * the address for small sets made `p^` yield the pointer bits, so e.g.
     * `(e in pflags(arg)^)` tested the address instead of the set. */
    if (is_set_type && kgpc_type_sizeof(deref_type) > 4)
      keep_by_address = 1;
    if (keep_by_address) {
      char addr_buf[64];
      snprintf(addr_buf, sizeof(addr_buf), "\tmovq\t%s, %s\n", addr_reg->bit_64,
               target_reg->bit_64);
      inst_list = add_inst(inst_list, addr_buf);
      free_reg(get_reg_stack(), addr_reg);
      return inst_list;
    }
  }

  long long load_size = expr_effective_size_bytes(expr);
  {
    /* Integrated: pointer-deref value load through the vtable; the address
     * base is a tracked vreg USE (was a baked name, invisible to liveness)
     * and the destination a tracked DEF.  Narrow widths go through emit_ext
     * as sign/zero-extending loads (mnemonics unchanged). */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_VREG, BE_W32, {.vreg = target_reg}};
    if (load_size == 1) {
      const int is_signed = expr_is_signed_kgpctype(expr);
      BeOperand src = {OPK_MEM_BD, BE_W8, {.mem_bd = {addr_reg, 0}}};
      kgpc_backend_target()->emit_ext(&em, &dst, &src, BE_W8, BE_W32,
                                      is_signed);
    } else if (load_size == 2) {
      const int is_signed = expr_is_signed_kgpctype(expr);
      BeOperand src = {OPK_MEM_BD, BE_W16, {.mem_bd = {addr_reg, 0}}};
      kgpc_backend_target()->emit_ext(&em, &dst, &src, BE_W16, BE_W32,
                                      is_signed);
    } else if (expr_uses_qword_kgpctype(expr)) {
      BeOperand dst64 = {OPK_VREG, BE_W64, {.vreg = target_reg}};
      BeOperand src = {OPK_MEM_BD, BE_W64, {.mem_bd = {addr_reg, 0}}};
      kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst64, &src, NULL);
    } else {
      BeOperand src = {OPK_MEM_BD, BE_W32, {.mem_bd = {addr_reg, 0}}};
      kgpc_backend_target()->emit(&em, BE_LOAD, BE_W32, &dst, &src, NULL);
    }
    inst_list = em.list;
  }

  free_reg(get_reg_stack(), addr_reg);
  return inst_list;
}

ListNode_t *codegen_addressof_leaf(struct Expression *expr,
                                   ListNode_t *inst_list, CodeGenContext *ctx,
                                   Register_t *target_reg) {
  if (expr == NULL || ctx == NULL || target_reg == NULL)
    return inst_list;

  struct Expression *inner = expr->expr_data.addr_data.expr;
  if (inner == NULL) {
    codegen_report_error(ctx, "ERROR: Address-of operator missing operand.");
    return inst_list;
  }

  char buffer[CODEGEN_MAX_INST_BUF];
  if (inner->type == EXPR_VAR_ID) {
    if (ctx != NULL && ctx->symtab != NULL) {
      const char *proc_label = NULL;
      char *resolved_label = NULL;
      HashNode_t *sym = NULL;
      if (FindSymbol(&sym, ctx->symtab, inner->expr_data.id) != 0 &&
          sym != NULL && sym->mangled_id != NULL && sym->type != NULL &&
          sym->type->kind == TYPE_KIND_PROCEDURE) {
        proc_label = sym->mangled_id;
      } else if (ctx->current_subprogram_owner_class != NULL) {
        const char *impl_target = codegen_find_class_method_impl_id(
            ctx->symtab, NULL, ctx->current_subprogram_owner_class, NULL,
            inner->expr_data.id);
        if (impl_target != NULL)
          proc_label = impl_target;
        else {
          int needed =
              snprintf(NULL, 0, "%s__%s", ctx->current_subprogram_owner_class,
                       inner->expr_data.id) +
              1;
          resolved_label = malloc((size_t)needed);
          if (resolved_label != NULL) {
            snprintf(resolved_label, (size_t)needed, "%s__%s",
                     ctx->current_subprogram_owner_class, inner->expr_data.id);
            ListNode_t *candidates = FindAllIdents(ctx->symtab, resolved_label);
            int found = 0;
            for (ListNode_t *c = candidates; c != NULL; c = c->next) {
              HashNode_t *cand = (HashNode_t *)c->cur;
              if (cand != NULL && cand->mangled_id != NULL &&
                  cand->type != NULL &&
                  cand->type->kind == TYPE_KIND_PROCEDURE) {
                proc_label = cand->mangled_id;
                found = 1;
                break;
              }
            }
            if (candidates != NULL)
              DestroyList(candidates);
            if (!found) {
              free(resolved_label);
              resolved_label = NULL;
            }
          }
        }
      }
      if (proc_label != NULL) {
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", proc_label,
                 target_reg->bit_64);
        if (resolved_label != NULL)
          free(resolved_label);
        return add_inst(inst_list, buffer);
      }
    }

    StackNode_t *var_node = find_label(inner->expr_data.id);
    if (var_node == NULL && ctx != NULL && ctx->symtab != NULL) {
      HashNode_t *sym_node = NULL;
      if (FindSymbol(&sym_node, ctx->symtab, inner->expr_data.id) != 0 &&
          sym_node != NULL) {
        /* `@unicodemap` inside e.g. cp1252.pas's init block must
         * address cp1252's own unicodemap, not the first-registered
         * one.  Typed-consts in units are stored under
         * "<unit>_$_<name>" (see codegen_var_storage_key); resolve
         * directly via that qualified key. */
        if (sym_node->is_typed_const && sym_node->source_unit_index > 0) {
          char *qualified = codegen_make_unit_qualified_key(
              sym_node->source_unit_index, inner->expr_data.id);
          if (qualified != NULL) {
            var_node = find_label(qualified);
            free(qualified);
          }
        }
        if (var_node == NULL && sym_node->mangled_id != NULL)
          var_node = find_label(sym_node->mangled_id);
      }
    }
    if (var_node != NULL) {
      if (var_node->is_static) {
        const char *label = (var_node->static_label != NULL)
                                ? var_node->static_label
                                : var_node->label;
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", label,
                 target_reg->bit_64);
      } else if (var_node->is_reference) {
        /* For var/out/constref parameters, the stack slot already contains
         * a pointer to the actual variable. So @param returns the VALUE
         * stored in the slot (the pointer), not the address of the slot. */
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 var_node->offset, target_reg->bit_64);
      } else {
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                 var_node->offset, target_reg->bit_64);
      }
      return add_inst(inst_list, buffer);
    } else {
      /* Check for string constants before falling through to nonlocal */
      if (ctx != NULL && ctx->symtab != NULL) {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, inner->expr_data.id) != 0 &&
            node != NULL)
          node = codegen_prefer_visible_var_over_const(ctx, inner->expr_data.id,
                                                       node);

        if (node != NULL && node->hash_type == HASHTYPE_CONST &&
            node->const_string_value != NULL &&
            !(node->type != NULL && node->type->kind == TYPE_KIND_PROCEDURE)) {
          int label_id = ctx->write_label_counter++;
          char str_label[32];
          char ptr_label[40];
          snprintf(str_label, sizeof(str_label), ".LC%d", label_id);
          snprintf(ptr_label, sizeof(ptr_label), ".LC%d_ptr", label_id);
          char rodata_buf[1024];
          const char *readonly_section = codegen_readonly_section_directive();
          char escaped[512];
          escape_string(escaped, node->const_string_value, sizeof(escaped));
          snprintf(rodata_buf, sizeof(rodata_buf),
                   "%s\n%s:\n\t.string \"%s\"\n%s:\n\t.quad\t%s\n%s\n",
                   readonly_section, str_label, escaped, ptr_label, str_label,
                   codegen_text_section_resume());
          inst_list = add_inst(inst_list, rodata_buf);
          snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", ptr_label,
                   target_reg->bit_64);
          return add_inst(inst_list, buffer);
        }
      }

      int offset = 0;
      inst_list =
          codegen_get_nonlocal(inst_list, inner->expr_data.id, &offset, ctx);
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tleaq\t-%d(%s), %%0\n",
                 offset, current_non_local_reg64());
        Register_t *d[] = {target_reg};
        return add_inst_du(inst_list, ctx, d, 1, NULL, 0, buffer_tmpl);
      }
    }
  } else if (inner->type == EXPR_ARRAY_ACCESS) {
    Register_t *addr_reg = NULL;
    inst_list = codegen_array_element_address(inner, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
      return inst_list;

    {
      Register_t *d[] = {target_reg};
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovq\t%1, %0\n");
    }
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  } else if (inner->type == EXPR_RECORD_ACCESS) {
    struct Expression *record_expr =
        inner->expr_data.record_access_data.record_expr;
    const char *field_id = inner->expr_data.record_access_data.field_id;
    if (record_expr != NULL && record_expr->type == EXPR_VAR_ID &&
        record_expr->expr_data.id != NULL && field_id != NULL && ctx != NULL &&
        ctx->symtab != NULL &&
        semcheck_is_unit_name(record_expr->expr_data.id)) {
      int unit_idx = unit_registry_add(record_expr->expr_data.id);
      HashNode_t *field_node = NULL;
      if (unit_idx > 0 && unit_idx < SYMTAB_MAX_UNITS &&
          ctx->symtab->unit_scopes[unit_idx] != NULL) {
        field_node = FindIdentInTable(ctx->symtab->unit_scopes[unit_idx]->table,
                                      field_id);
      }

      if (field_node != NULL && (field_node->hash_type == HASHTYPE_PROCEDURE ||
                                 field_node->hash_type == HASHTYPE_FUNCTION)) {
        const char *emit_target =
            codegen_subprogram_emission_symbol(field_node);
        if (emit_target == NULL || emit_target[0] == '\0')
          emit_target = field_node->mangled_id;
        if (emit_target == NULL || emit_target[0] == '\0')
          emit_target = field_node->id;

        if (emit_target != NULL && emit_target[0] != '\0') {
          snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                   emit_target, target_reg->bit_64);
          return add_inst(inst_list, buffer);
        }
      }
    }

    Register_t *addr_reg = NULL;
    inst_list = codegen_record_field_address(inner, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
      return inst_list;

    {
      Register_t *d[] = {target_reg};
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovq\t%1, %0\n");
    }
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  } else if (inner->type == EXPR_POINTER_DEREF) {
    struct Expression *pointer_expr =
        inner->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr == NULL) {
      codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
      return inst_list;
    }

    Register_t *addr_reg = NULL;
    inst_list =
        codegen_expr_tree_value(pointer_expr, inst_list, ctx, &addr_reg);
    if (addr_reg == NULL)
      return inst_list;

    {
      Register_t *d[] = {target_reg};
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovq\t%1, %0\n");
    }
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  }

  codegen_report_error(ctx,
                       "ERROR: Unsupported operand for address-of operator.");
  return inst_list;
}

/* Recompute the field offset for a class var access when the CLASSVAR storage
 * only contains class var fields (not the full instance layout).  Returns the
 * class-var-only offset for the field named `field_id`, or -1 if not found. */
ListNode_t *codegen_record_field_address(struct Expression *expr,
                                         ListNode_t *inst_list,
                                         CodeGenContext *ctx,
                                         Register_t **out_reg) {
  if (expr == NULL || ctx == NULL || out_reg == NULL)
    return inst_list;

  struct Expression *record_expr =
      expr->expr_data.record_access_data.record_expr;
  if (record_expr == NULL)
    return inst_list;
  const char *field_id = expr->expr_data.record_access_data.field_id;

  /* Check if this is a class field access. Classes are pointers, so we need to
   * load the instance pointer from variable storage for VAR_ID expressions. */
  struct RecordType *record_expr_record =
      codegen_expr_record_type(record_expr, ctx != NULL ? ctx->symtab : NULL);
  int is_class_field =
      (record_expr_record != NULL && record_type_is_class(record_expr_record));
  KgpcType *record_expr_value_type = expr_get_kgpc_type(record_expr);
  int record_expr_value_is_class =
      is_class_field ||
      (record_expr_value_type != NULL &&
       ((kgpc_type_is_record(record_expr_value_type) &&
         record_type_is_class(kgpc_type_get_record(record_expr_value_type))) ||
        (kgpc_type_is_pointer(record_expr_value_type) &&
         record_expr_value_type->info.points_to != NULL &&
         kgpc_type_is_record(record_expr_value_type->info.points_to) &&
         record_type_is_class(
             kgpc_type_get_record(record_expr_value_type->info.points_to)))));

  int is_type_ref = 0;
  const char *type_label = NULL;
  if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL &&
      ctx->symtab != NULL) {
    HashNode_t *symbol = NULL;
    if (FindSymbol(&symbol, ctx->symtab, record_expr->expr_data.id) != 0 &&
        symbol != NULL && symbol->hash_type == HASHTYPE_TYPE) {
      is_type_ref = 1;
      if (record_expr_record != NULL && record_expr_record->type_id != NULL)
        type_label = record_expr_record->type_id;
      else
        type_label = record_expr->expr_data.id;
    }
  }

  const struct RecordType *class_var_owner =
      codegen_record_class_var_owner_named(ctx->symtab, record_expr_record,
                                           field_id);
  if (record_expr_record != NULL && field_id != NULL &&
      record_expr_record->type_id != NULL &&
      (class_var_owner != NULL ||
       codegen_nonstatic_class_method_owner_field_uses_classvar(
           ctx, record_expr_record, record_expr))) {
    Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (addr_reg == NULL)
      addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
    if (addr_reg == NULL) {
      codegen_report_error(
          ctx, "ERROR: Unable to allocate register for class var access.");
      return inst_list;
    }

    inst_list = codegen_emit_classvar_base_address_named(
        inst_list, addr_reg->bit_64,
        class_var_owner != NULL ? class_var_owner : record_expr_record,
        expr->expr_data.record_access_data.field_offset);
    *out_reg = addr_reg;
    return inst_list;
  }

  Register_t *addr_reg = NULL;
  if (!is_type_ref && is_class_field && record_expr->type == EXPR_VAR_ID &&
      record_expr->expr_data.id != NULL &&
      pascal_identifier_equals(record_expr->expr_data.id, "Self") &&
      ctx->current_subprogram_is_nonstatic_class_method &&
      record_expr_record != NULL && record_expr_record->type_id != NULL) {
    addr_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (addr_reg == NULL)
      addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
    if (addr_reg == NULL) {
      codegen_report_error(
          ctx, "ERROR: Unable to allocate register for class var access.");
      return inst_list;
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tleaq\t%s_CLASSVAR(%%rip), %s\n",
             record_expr_record->type_id, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    long long offset = expr->expr_data.record_access_data.field_offset;
    if (offset != 0) {
      snprintf(buffer, sizeof(buffer), "\taddq\t$%lld, %s\n", offset,
               addr_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
    }

    *out_reg = addr_reg;
    return inst_list;
  }

  if (is_type_ref && type_label != NULL) {
    addr_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (addr_reg == NULL)
      addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
    if (addr_reg == NULL) {
      codegen_report_error(
          ctx, "ERROR: Unable to allocate register for class var access.");
      return inst_list;
    }

    char buffer[96];
    snprintf(buffer, sizeof(buffer), "\tleaq\t%s_CLASSVAR(%%rip), %s\n",
             type_label, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
  } else {
    inst_list =
        codegen_address_for_expr(record_expr, inst_list, ctx, &addr_reg);
    if (addr_reg == NULL)
      return inst_list;
  }

  /* codegen_address_for_expr() returns the address of storage for addressable
   * expressions, even when that storage is wrapped in a cast/as node. For
   * class-typed expressions that means addr_reg points at the slot holding the
   * instance pointer, so load the pointer value before applying the field
   * offset. Non-addressable expressions (for example function calls) already
   * materialise the instance pointer directly. */
  int needs_class_deref = (record_expr_value_is_class && !is_type_ref &&
                           codegen_expr_is_addressable(record_expr));
  if (needs_class_deref) {
    {
      Register_t *d[] = {addr_reg};
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovq\t(%1), %0\n");
    }
  }

  long long offset = expr->expr_data.record_access_data.field_offset;
  /* Class var fields accessed via type reference (TMyClass.FValue) use
   * CLASSVAR-relative offsets computed by the semcheck — no VMT adjustment
   * needed since CLASSVAR storage has no VMT prefix. */
  if (offset != 0) {
    char buffer[128];
    if (offset > INT32_MAX || offset < INT32_MIN) {
      /* x86-64 addq only accepts 32-bit sign-extended immediates.
       * For larger offsets, load into a scratch register first. */
      snprintf(buffer, sizeof(buffer), "\tmovabsq\t$%lld, %%r11\n", offset);
      inst_list = add_inst(inst_list, buffer);
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\taddq\t%%r11, %%0\n");
        Register_t *d[] = {addr_reg};
        Register_t *u[] = {addr_reg};
        inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, buffer_tmpl);
      }
    } else {
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\taddq\t$%lld, %%0\n",
                 offset);
        Register_t *d[] = {addr_reg};
        Register_t *u[] = {addr_reg};
        inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, buffer_tmpl);
      }
    }
  }

  *out_reg = addr_reg;
  return inst_list;
}

static HashNode_t *codegen_find_unit_qualified_symbol(CodeGenContext *ctx,
                                                      const char *unit_id,
                                                      const char *field_id) {
  if (ctx == NULL || ctx->symtab == NULL || unit_id == NULL || field_id == NULL)
    return NULL;

  int unit_idx = unit_registry_add(unit_id);
  HashNode_t *field_node = NULL;
  if (unit_idx > 0 && unit_idx < SYMTAB_MAX_UNITS &&
      ctx->symtab->unit_scopes[unit_idx] != NULL) {
    field_node =
        FindIdentInTable(ctx->symtab->unit_scopes[unit_idx]->table, field_id);
    if (field_node != NULL)
      return field_node;
  }

  for (ScopeNode *cur_scope = ctx->symtab->current_scope; cur_scope != NULL;
       cur_scope = cur_scope->parent) {
    HashNode_t *candidate =
        FindIdentInTableForUnit(cur_scope->table, field_id, unit_idx);
    if (candidate != NULL && candidate->source_unit_index == unit_idx)
      return candidate;
  }

  return NULL;
}

ListNode_t *codegen_record_access(struct Expression *expr,
                                  ListNode_t *inst_list, CodeGenContext *ctx,
                                  Register_t *target_reg) {
  if (expr == NULL || ctx == NULL || target_reg == NULL)
    return inst_list;

  {
    struct Expression *record_expr =
        expr->expr_data.record_access_data.record_expr;
    const char *field_id = expr->expr_data.record_access_data.field_id;
    if (record_expr != NULL && record_expr->type == EXPR_VAR_ID &&
        record_expr->expr_data.id != NULL && field_id != NULL &&
        semcheck_is_unit_name(record_expr->expr_data.id)) {
      HashNode_t *field_node = codegen_find_unit_qualified_symbol(
          ctx, record_expr->expr_data.id, field_id);
      if (field_node != NULL && field_node->hash_type == HASHTYPE_CONST) {
        char buffer[96];
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n",
                 field_node->const_int_value, target_reg->bit_64);
        return add_inst(inst_list, buffer);
      }
    }
  }

  if (expr_has_type_tag(expr, RECORD_TYPE)) {
    struct RecordType *record_type =
        codegen_expr_record_type(expr, ctx != NULL ? ctx->symtab : NULL);
    if (record_type == NULL || !record_type_is_class(record_type)) {
      Register_t *addr_reg = NULL;
      inst_list = codegen_record_field_address(expr, inst_list, ctx, &addr_reg);
      if (addr_reg == NULL)
        return inst_list;

      char buffer[64];
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64,
               target_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      free_reg(get_reg_stack(), addr_reg);
      return inst_list;
    }
  }

  Register_t *addr_reg = NULL;
  inst_list = codegen_record_field_address(expr, inst_list, ctx, &addr_reg);
  if (addr_reg == NULL)
    return inst_list;

  if (expr_has_type_tag(expr, SHORTSTRING_TYPE)) {
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64,
             target_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  }

  /* Array-typed fields cannot be loaded into a register as a value: yield
   * the field's address so the consumer can either copy/iterate or treat
   * the array as a pointer (Pascal allows `array of char` to decay to
   * pchar in pchar-compatible contexts). Without this, KGPC would
   * mis-emit `mov[lq] (%addr), %reg` and read 4-or-8 bytes of array
   * contents as if they were a primitive value (root cause of taicpu
   * gencode SIGSEGV when assigning insentry^.code, an array of char,
   * to a pchar codes variable).
   *
   * Detection priority: prefer `expr->resolved_kgpc_type` when it
   * carries the array type, but fall back to the RecordField metadata
   * (`field->is_array`) when the resolved type is missing or has been
   * narrowed to a primitive (e.g. by an enclosing assignment-target
   * coercion). RecordField metadata is the canonical truth for the
   * field's declared shape regardless of any expression-level rewrites. */
  int field_is_array_kind = 0;
  int field_is_dynamic_array = 0;
  if (expr->resolved_kgpc_type != NULL &&
      kgpc_type_is_array(expr->resolved_kgpc_type)) {
    field_is_array_kind = 1;
    if (kgpc_type_is_dynamic_array(expr->resolved_kgpc_type))
      field_is_dynamic_array = 1;
  } else {
    struct RecordField *fmeta = codegen_expr_lookup_record_field(expr, ctx);
    if (fmeta != NULL && fmeta->is_array) {
      field_is_array_kind = 1;
      /* Dynamic arrays are flagged at the parser via array_is_open
       * (bounds end < start, e.g. [0..-1]); same marker is used for
       * dynamic-array fields whose storage is a {data,length}
       * descriptor instead of inline element bytes. */
      if (fmeta->array_is_open)
        field_is_dynamic_array = 1;
    }
  }
  if (field_is_array_kind) {
    char buffer[64];
    if (field_is_dynamic_array) {
      /* Dynamic-array field storage is a 16-byte descriptor
       * { data; length } embedded inline in the record. The
       * "decay-to-pointer" value of such a field is the .data
       * pointer (first 8 bytes of the descriptor), not the
       * descriptor's own address. Without this dereference, casts
       * like `pointer(rec.dyn_arr)` would yield the descriptor's
       * address rather than the heap data pointer, producing
       * struct-shaped garbage when the result is later read as
       * raw bytes.
       *
       * Integrated: 64-bit data-pointer load through the vtable; the
       * descriptor base is a tracked vreg USE and the destination a tracked
       * DEF (were baked names, invisible to liveness). */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_VREG, BE_W64, {.vreg = target_reg}};
      BeOperand src = {OPK_MEM_BD, BE_W64, {.mem_bd = {addr_reg, 0}}};
      kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
      inst_list = em.list;
    } else {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64,
               target_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
    }
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  }

  /* Extended (10-byte / bestreal) record/class field load: the consumer
   * works with double-precision values in GP registers (this code path is
   * reached for arithmetic, comparisons, and assignment targets that are
   * not themselves extended).  A plain `movq (%addr), %reg` would silently
   * truncate the 10-byte representation to its low 8 bytes (the mantissa),
   * yielding nonsense like extended 1.0 -> -0.0 / 3.0 -> -2.0.  Route
   * through the FPU truncation helper to convert the value before the
   * caller observes only 8 bytes.  This keeps FPC compiler internals
   * (e.g. `value_real = 0.0`, `ts64real(value_real)`) numerically faithful
   * when KGPC builds pp_bootstrap. */
  {
    KgpcType *field_type = expr_get_kgpc_type(expr);
    if (field_type != NULL && kgpc_type_is_extended(field_type)) {
      char buffer[128];
      if (codegen_target_is_windows())
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n",
                 addr_reg->bit_64);
      else
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n",
                 addr_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      inst_list = codegen_vect_reg(inst_list, 0);
      inst_list = codegen_call_with_shadow_space(inst_list,
                                                 "kgpc_load_extended_to_bits");
      free_arg_regs();
      snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n",
               target_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      free_reg(get_reg_stack(), addr_reg);
      return inst_list;
    }
  }

  long long field_size = codegen_record_field_effective_size(expr, ctx);
  /* Cross-check with resolved_kgpc_type for sub-dword types that
     codegen_record_field_effective_size may miss (e.g. Integer=SmallInt in FPC
     mode) */
  if (field_size == 4 && expr->resolved_kgpc_type != NULL) {
    long long resolved_size = kgpc_type_sizeof(expr->resolved_kgpc_type);
    if (resolved_size > 0 && resolved_size < 4)
      field_size = resolved_size;
  }
  /* Ensure REAL fields honor resolved size (Single vs Double) */
  if (expr_has_type_tag(expr, REAL_TYPE) && expr->resolved_kgpc_type != NULL) {
    long long resolved_size = kgpc_type_sizeof(expr->resolved_kgpc_type);
    if (resolved_size == 4 || resolved_size == 8)
      field_size = resolved_size;
  }
  int type_tag = expr_get_type_tag(expr);
  int is_single_real_field =
      (expr_has_type_tag(expr, REAL_TYPE) && field_size == 4);
  if (is_single_real_field) {
    /* Single-precision real fields are stored as 4-byte floats, but KGPC keeps
     * real values at double precision in GPRs (matching single *variable*
     * reads, which promote via cvtss2sd, and the expectations of
     * kgpc_write_real, double-precision arithmetic, comparisons and real
     * function returns). Promote the 4-byte single to double on read so every
     * consumer sees a uniform double-bit representation. */
    {
      char buffer_tmpl[64];
      snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovss\t(%s), %%xmm0\n",
               addr_reg->bit_64);
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
    }
    inst_list = add_inst(inst_list, "\tcvtss2sd\t%xmm0, %xmm0\n");
    {
      char buffer_tmpl[64];
      snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%xmm0, %%0\n");
      Register_t *d[] = {target_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, buffer_tmpl);
    }
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  }
  {
    /* Integrated: record/class field value load through the vtable; the
     * field-address base is a tracked vreg USE (was a baked name, invisible
     * to liveness) and the destination a tracked DEF.  Narrow widths go
     * through emit_ext as sign/zero-extending loads (mnemonics unchanged). */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_VREG, BE_W32, {.vreg = target_reg}};
    if (!is_single_real_field &&
        (expr_uses_qword_kgpctype(expr) || field_size == 8)) {
      BeOperand dst64 = {OPK_VREG, BE_W64, {.vreg = target_reg}};
      BeOperand src = {OPK_MEM_BD, BE_W64, {.mem_bd = {addr_reg, 0}}};
      kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst64, &src, NULL);
    } else if (field_size == 1 || expr_has_type_tag(expr, CHAR_TYPE)) {
      const int is_signed =
          (type_tag != CHAR_TYPE && codegen_type_is_signed(type_tag));
      BeOperand src = {OPK_MEM_BD, BE_W8, {.mem_bd = {addr_reg, 0}}};
      kgpc_backend_target()->emit_ext(&em, &dst, &src, BE_W8, BE_W32,
                                      is_signed);
    } else if (field_size == 2) {
      const int is_signed = codegen_type_is_signed(type_tag);
      BeOperand src = {OPK_MEM_BD, BE_W16, {.mem_bd = {addr_reg, 0}}};
      kgpc_backend_target()->emit_ext(&em, &dst, &src, BE_W16, BE_W32,
                                      is_signed);
    } else {
      BeOperand src = {OPK_MEM_BD, BE_W32, {.mem_bd = {addr_reg, 0}}};
      kgpc_backend_target()->emit(&em, BE_LOAD, BE_W32, &dst, &src, NULL);
    }
    inst_list = em.list;
  }

  free_reg(get_reg_stack(), addr_reg);
  return inst_list;
}

static ListNode_t *codegen_set_emit_single(ListNode_t *inst_list,
                                           CodeGenContext *ctx,
                                           Register_t *dest_reg,
                                           Register_t *value_reg) {
  if (dest_reg == NULL || value_reg == NULL)
    return inst_list;

  char buffer[128];
  char skip_label[18];
  gen_label(skip_label, sizeof(skip_label), ctx);

  /* For 32-bit register-resident sets only bits 0..31 exist.  Values
   * outside that domain are not part of the set and must not affect any
   * bit (Pascal: x in S is false when x is outside S's element domain,
   * by symmetry construction must drop such elements rather than wrap). */
  {
    Register_t *u[] = {value_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$0, %0\n");
  }
  {
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    kgpc_backend_target()->emit_branch(&em, BE_LT, skip_label);
    inst_list = em.list;
  }
  {
    Register_t *u[] = {value_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$31, %0\n");
  }
  {
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    kgpc_backend_target()->emit_branch(&em, BE_GT, skip_label);
    inst_list = em.list;
  }

  {
    Register_t *d[] = {dest_reg};
    Register_t *u[] = {value_reg};
    inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tbtsl\t%1, %0\n");
  }
  snprintf(buffer, sizeof(buffer), "%s:\n", skip_label);
  inst_list = add_inst(inst_list, buffer);
  return inst_list;
}

static ListNode_t *codegen_set_emit_range(ListNode_t *inst_list,
                                          CodeGenContext *ctx,
                                          Register_t *dest_reg,
                                          Register_t *start_reg,
                                          Register_t *end_reg) {
  if (dest_reg == NULL || start_reg == NULL || end_reg == NULL)
    return inst_list;

  Register_t *temp_reg = codegen_try_get_reg(&inst_list, ctx, "set range temp");
  if (temp_reg == NULL)
    return inst_list;

  char order_label[18];
  char loop_label[18];
  char done_label[18];
  char start_floor_label[18];
  char end_cap_label[18];
  gen_label(order_label, sizeof(order_label), ctx);
  gen_label(loop_label, sizeof(loop_label), ctx);
  gen_label(done_label, sizeof(done_label), ctx);
  gen_label(start_floor_label, sizeof(start_floor_label), ctx);
  gen_label(end_cap_label, sizeof(end_cap_label), ctx);

  char buffer[128];
  {
    Register_t *u[] = {end_reg, start_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\tcmpl\t%0, %1\n");
  }
  {
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    kgpc_backend_target()->emit_branch(&em, BE_LE, order_label);
    inst_list = em.list;
  }
  {
    Register_t *d[] = {temp_reg};
    Register_t *u[] = {start_reg};
    inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovl\t%1, %0\n");
  }
  {
    Register_t *d[] = {start_reg};
    Register_t *u[] = {end_reg};
    inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovl\t%1, %0\n");
  }
  {
    Register_t *d[] = {end_reg};
    Register_t *u[] = {temp_reg};
    inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovl\t%1, %0\n");
  }
  snprintf(buffer, sizeof(buffer), "%s:\n", order_label);
  inst_list = add_inst(inst_list, buffer);

  /* Guard: if end < 0, the range is empty */
  {
    Register_t *u[] = {end_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$0, %0\n");
  }
  {
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    kgpc_backend_target()->emit_branch(&em, BE_LT, done_label);
    inst_list = em.list;
  }

  /* Floor start at 0 (no negative bit positions) */
  {
    Register_t *u[] = {start_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$0, %0\n");
  }
  {
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    kgpc_backend_target()->emit_branch(&em, BE_GE, start_floor_label);
    inst_list = em.list;
  }
  {
    Register_t *d[] = {start_reg};
    inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovl\t$0, %0\n");
  }
  snprintf(buffer, sizeof(buffer), "%s:\n", start_floor_label);
  inst_list = add_inst(inst_list, buffer);

  /* If start > 31 the entire range is outside the 32-bit set domain. */
  {
    Register_t *u[] = {start_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$31, %0\n");
  }
  {
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    kgpc_backend_target()->emit_branch(&em, BE_GT, done_label);
    inst_list = em.list;
  }

  /* Cap end at 31 — bits beyond the storage simply don't exist. */
  {
    Register_t *u[] = {end_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$31, %0\n");
  }
  {
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    kgpc_backend_target()->emit_branch(&em, BE_LE, end_cap_label);
    inst_list = em.list;
  }
  {
    Register_t *d[] = {end_reg};
    inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovl\t$31, %0\n");
  }
  snprintf(buffer, sizeof(buffer), "%s:\n", end_cap_label);
  inst_list = add_inst(inst_list, buffer);

  snprintf(buffer, sizeof(buffer), "%s:\n", loop_label);
  inst_list = add_inst(inst_list, buffer);
  {
    Register_t *u[] = {end_reg, start_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\tcmpl\t%0, %1\n");
  }
  {
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    kgpc_backend_target()->emit_branch(&em, BE_GT, done_label);
    inst_list = em.list;
  }
  /* Use btsl to set the bit (avoids %ecx shift register conflict) */
  {
    Register_t *d[] = {dest_reg};
    Register_t *u[] = {start_reg};
    inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tbtsl\t%1, %0\n");
  }
  {
    Register_t *u[] = {end_reg, start_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\tcmpl\t%0, %1\n");
  }
  {
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    kgpc_backend_target()->emit_branch(&em, BE_EQ, done_label);
    inst_list = em.list;
  }
  {
    Register_t *d[] = {start_reg};
    inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tincl\t%0\n");
  }
  {
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    kgpc_backend_target()->emit_branch(&em, BE_ALWAYS, loop_label);
    inst_list = em.list;
  }
  snprintf(buffer, sizeof(buffer), "%s:\n", done_label);
  inst_list = add_inst(inst_list, buffer);

  free_reg(get_reg_stack(), temp_reg);
  return inst_list;
}

extern int extract_constant_int(struct Expression *expr, long long *out_value);

static int char_set_resolve_element_value(const struct Expression *expr,
                                          CodeGenContext *ctx,
                                          long long *out_value) {
  if (expr == NULL || out_value == NULL)
    return 1;
  if (extract_constant_int((struct Expression *)expr, out_value) == 0)
    return 0;
  if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL && ctx != NULL &&
      ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
        node != NULL && node->hash_type == HASHTYPE_CONST &&
        node->is_constant) {
      *out_value = node->const_int_value;
      return 0;
    }
  }
  return 1;
}

static int char_set_compute_constant_mask(const struct Expression *expr,
                                          CodeGenContext *ctx,
                                          uint64_t mask_out[4]) {
  mask_out[0] = mask_out[1] = mask_out[2] = mask_out[3] = 0;
  if (expr == NULL || expr->type != EXPR_SET)
    return 1;
  for (ListNode_t *cur = expr->expr_data.set_data.elements; cur != NULL;
       cur = cur->next) {
    struct SetElement *element = (struct SetElement *)cur->cur;
    if (element == NULL)
      continue;
    long long low = 0, high = 0;
    if (char_set_resolve_element_value(element->lower, ctx, &low) != 0)
      return 1;
    if (element->upper != NULL) {
      if (char_set_resolve_element_value(element->upper, ctx, &high) != 0)
        return 1;
    } else {
      high = low;
    }
    if (low > high) {
      long long t = low;
      low = high;
      high = t;
    }
    if (low < 0 || high >= 256)
      return 1;
    for (long long v = low; v <= high; ++v) {
      mask_out[v >> 6] |= (uint64_t)1u << (v & 63);
    }
  }
  return 0;
}

ListNode_t *codegen_set_literal(struct Expression *expr, ListNode_t *inst_list,
                                CodeGenContext *ctx, Register_t **out_reg,
                                int force_char_set) {
  if (expr == NULL)
    return inst_list;

  /* Check if this is a character set literal */
  int is_char_set = force_char_set || expr_is_char_set_ctx(expr, ctx);

  if (is_char_set) {
    /* Compile-time-constant fast path: if every element's ordinal is
       known at compile time, emit the 256-bit mask as static .rodata
       and just leaq it. Skips the per-literal zero+btsl explosion. */
    uint64_t const_mask[4] = {0};
    if (ctx != NULL && expr->type == EXPR_SET &&
        char_set_compute_constant_mask(expr, ctx, const_mask) == 0) {
      Register_t *addr_reg =
          codegen_try_get_reg(&inst_list, ctx, "char set addr");
      if (addr_reg == NULL) {
        if (out_reg != NULL)
          *out_reg = NULL;
        return inst_list;
      }
      int label_id = ctx->write_label_counter++;
      char label[32];
      snprintf(label, sizeof(label), ".LCSET%d", label_id);
      char rodata_buf[512];
      const char *readonly_section = codegen_readonly_section_directive();
      snprintf(
          rodata_buf, sizeof(rodata_buf),
          "%s\n\t.align\t8\n%s:\n\t.quad\t0x%016llx\n\t.quad\t0x%016llx\n\t."
          "quad\t0x%016llx\n\t.quad\t0x%016llx\n%s\n",
          readonly_section, label, (unsigned long long)const_mask[0],
          (unsigned long long)const_mask[1], (unsigned long long)const_mask[2],
          (unsigned long long)const_mask[3], codegen_text_section_resume());
      inst_list = add_inst(inst_list, rodata_buf);
      char lea_buf[128];
      snprintf(lea_buf, sizeof(lea_buf), "\tleaq\t%s(%%rip), %s\n", label,
               addr_reg->bit_64);
      inst_list = add_inst(inst_list, lea_buf);
      if (out_reg != NULL)
        *out_reg = addr_reg;
      else
        free_reg(get_reg_stack(), addr_reg);
      return inst_list;
    }

    /* Character sets need 32 bytes in memory, not a register */
    /* Allocate a temporary 32-byte buffer on the stack */
    StackNode_t *char_set_temp = codegen_alloc_record_temp(32);
    if (char_set_temp == NULL) {
      if (out_reg != NULL)
        *out_reg = NULL;
      return inst_list;
    }

    /* Zero-initialize all 32 bytes */
    char buffer[128];
    for (int i = 0; i < 8; i++) {
      int offset = char_set_temp->offset - (i * 4);
      /* Integrated: store an immediate to the frame slot through the vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_MEM_FRAME, BE_W32,
                       {.mem_frame = {BE_BASE_FP, -(long long)(offset)}}};
      BeOperand a = {OPK_IMM, BE_W32, {.imm = 0}};
      kgpc_backend_target()->emit(&em, BE_STORE, BE_W32, &dst, &a, NULL);
      inst_list = em.list;
    }

    /* Get address register for the set buffer */
    Register_t *addr_reg =
        codegen_try_get_reg(&inst_list, ctx, "char set addr");
    if (addr_reg == NULL) {
      if (out_reg != NULL)
        *out_reg = NULL;
      return inst_list;
    }

    {
      /* Integrated: address-of the frame slot into a physical register via the vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_PHYS, BE_W64, {.phys = addr_reg->bit_64}};
      BeOperand src = {OPK_MEM_FRAME, BE_W64,
                       {.mem_frame = {BE_BASE_FP, -(long long)(char_set_temp->offset)}}};
      kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
      inst_list = em.list;
    }

    /* Now set each element in the character set */
    for (ListNode_t *cur = expr->expr_data.set_data.elements; cur != NULL;
         cur = cur->next) {
      struct SetElement *element = (struct SetElement *)cur->cur;
      if (element == NULL)
        continue;

      Register_t *value_reg = NULL;
      inst_list =
          codegen_expr_tree_value(element->lower, inst_list, ctx, &value_reg);
      if (codegen_had_error(ctx) || value_reg == NULL) {
        if (value_reg != NULL)
          free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reg);
        if (out_reg != NULL)
          *out_reg = NULL;
        return inst_list;
      }

      /* For character sets: Calculate dword index and bit index */
      Register_t *bit_reg =
          codegen_try_get_reg(&inst_list, ctx, "char set bit");
      Register_t *dword_reg =
          codegen_try_get_reg(&inst_list, ctx, "char set dword");
      if (bit_reg == NULL || dword_reg == NULL) {
        if (bit_reg != NULL)
          free_reg(get_reg_stack(), bit_reg);
        if (dword_reg != NULL)
          free_reg(get_reg_stack(), dword_reg);
        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reg);
        if (out_reg != NULL)
          *out_reg = NULL;
        return inst_list;
      }

      /* btsl with memory operand auto-computes byte offset for any bit
         position. Only need value_reg and addr_reg — no separate
         bit_reg/dword_reg. */
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tbtsl\t%%0, (%s)\n",
                 addr_reg->bit_64);
        Register_t *u[] = {value_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }

      free_reg(get_reg_stack(), dword_reg);
      free_reg(get_reg_stack(), bit_reg);
      free_reg(get_reg_stack(), value_reg);

      /* Handle ranges (element->upper != NULL): set bits from lower+1 to upper
         (lower bit was already set by btsl above). */
      if (element->upper != NULL) {
        Register_t *upper_reg = NULL;
        inst_list =
            codegen_expr_tree_value(element->upper, inst_list, ctx, &upper_reg);
        if (codegen_had_error(ctx) || upper_reg == NULL) {
          if (upper_reg != NULL)
            free_reg(get_reg_stack(), upper_reg);
          free_reg(get_reg_stack(), addr_reg);
          if (out_reg != NULL)
            *out_reg = NULL;
          return inst_list;
        }
        /* Get a register for the loop counter, re-evaluate lower */
        Register_t *loop_reg = NULL;
        inst_list =
            codegen_expr_tree_value(element->lower, inst_list, ctx, &loop_reg);
        if (codegen_had_error(ctx) || loop_reg == NULL) {
          if (loop_reg != NULL)
            free_reg(get_reg_stack(), loop_reg);
          free_reg(get_reg_stack(), upper_reg);
          free_reg(get_reg_stack(), addr_reg);
          if (out_reg != NULL)
            *out_reg = NULL;
          return inst_list;
        }
        /* Start from lower+1 (lower already set above) */
        {
          Register_t *d[] = {loop_reg};
          inst_list =
              add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tincl\t%0\n");
        }
        /* Loop: while loop_reg <= upper_reg, btsl and increment */
        int range_label = ++ctx->label_counter;
        int end_label = ++ctx->label_counter;
        snprintf(buffer, sizeof(buffer), ".L%d:\n", range_label);
        inst_list = add_inst(inst_list, buffer);
        {
          Register_t *u[] = {upper_reg, loop_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\tcmpl\t%0, %1\n");
        }
        {
          char lbl[32];
          snprintf(lbl, sizeof(lbl), ".L%d", end_label);
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          kgpc_backend_target()->emit_branch(&em, BE_GT, lbl);
          inst_list = em.list;
        }
        {
          char buffer_tmpl[128];
          snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tbtsl\t%%0, (%s)\n",
                   addr_reg->bit_64);
          Register_t *u[] = {loop_reg};
          inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
        }
        {
          Register_t *d[] = {loop_reg};
          inst_list =
              add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tincl\t%0\n");
        }
        {
          char lbl[32];
          snprintf(lbl, sizeof(lbl), ".L%d", range_label);
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          kgpc_backend_target()->emit_branch(&em, BE_ALWAYS, lbl);
          inst_list = em.list;
        }
        snprintf(buffer, sizeof(buffer), ".L%d:\n", end_label);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), loop_reg);
        free_reg(get_reg_stack(), upper_reg);
      }
    }

    /* Return the address register */
    if (out_reg != NULL)
      *out_reg = addr_reg;
    else
      free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  }

  /* Regular 32-bit sets */
  if (expr->expr_data.set_data.is_constant) {
    Register_t *dest_reg = codegen_try_get_reg(&inst_list, ctx, "set literal");
    if (dest_reg == NULL) {
      if (out_reg != NULL)
        *out_reg = NULL;
      return inst_list;
    }

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovl\t$%u, %s\n",
             expr->expr_data.set_data.bitmask, dest_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    if (out_reg != NULL)
      *out_reg = dest_reg;
    else
      free_reg(get_reg_stack(), dest_reg);
    return inst_list;
  }

  Register_t *dest_reg = codegen_try_get_reg(&inst_list, ctx, "set literal");
  if (dest_reg == NULL) {
    if (out_reg != NULL)
      *out_reg = NULL;
    return inst_list;
  }

  {
    Register_t *d[] = {dest_reg};
    inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovl\t$0, %0\n");
  }

  for (ListNode_t *cur = expr->expr_data.set_data.elements; cur != NULL;
       cur = cur->next) {
    struct SetElement *element = (struct SetElement *)cur->cur;
    if (element == NULL)
      continue;

    Register_t *lower_reg = NULL;
    inst_list =
        codegen_expr_tree_value(element->lower, inst_list, ctx, &lower_reg);
    if (codegen_had_error(ctx) || lower_reg == NULL) {
      if (lower_reg != NULL)
        free_reg(get_reg_stack(), lower_reg);
      free_reg(get_reg_stack(), dest_reg);
      if (out_reg != NULL)
        *out_reg = NULL;
      return inst_list;
    }

    Register_t *upper_reg = NULL;
    if (element->upper != NULL) {
      inst_list =
          codegen_expr_tree_value(element->upper, inst_list, ctx, &upper_reg);
      if (codegen_had_error(ctx) || upper_reg == NULL) {
        if (upper_reg != NULL)
          free_reg(get_reg_stack(), upper_reg);
        free_reg(get_reg_stack(), lower_reg);
        free_reg(get_reg_stack(), dest_reg);
        if (out_reg != NULL)
          *out_reg = NULL;
        return inst_list;
      }
    }

    if (element->upper == NULL)
      inst_list = codegen_set_emit_single(inst_list, ctx, dest_reg, lower_reg);
    else
      inst_list = codegen_set_emit_range(inst_list, ctx, dest_reg, lower_reg,
                                         upper_reg);

    if (codegen_had_error(ctx)) {
      if (upper_reg != NULL)
        free_reg(get_reg_stack(), upper_reg);
      free_reg(get_reg_stack(), lower_reg);
      free_reg(get_reg_stack(), dest_reg);
      if (out_reg != NULL)
        *out_reg = NULL;
      return inst_list;
    }

    if (upper_reg != NULL)
      free_reg(get_reg_stack(), upper_reg);
    free_reg(get_reg_stack(), lower_reg);
  }

  if (out_reg != NULL)
    *out_reg = dest_reg;
  else
    free_reg(get_reg_stack(), dest_reg);
  return inst_list;
}

ListNode_t *codegen_set_expr(struct Expression *expr, ListNode_t *inst_list,
                             CodeGenContext *ctx, Register_t **out_reg) {
  if (expr == NULL) {
    if (out_reg != NULL)
      *out_reg = NULL;
    return inst_list;
  }

  if (expr->type == EXPR_SET)
    return codegen_set_literal(expr, inst_list, ctx, out_reg, 0);

  return codegen_expr_tree_value(expr, inst_list, ctx, out_reg);
}

static Register_t *codegen_clone_register_if_rcx(ListNode_t **inst_list,
                                                 CodeGenContext *ctx,
                                                 Register_t *reg,
                                                 const char *label) {
  if (reg == NULL || inst_list == NULL)
    return reg;

  if (reg->reg_id != REG_RCX)
    return reg;

  static const RegisterId_t preferred_regs[] = {REG_R12, REG_R13, REG_R14,
                                                REG_R15, REG_RBX};
  Register_t *replacement = NULL;
  for (size_t i = 0; i < sizeof(preferred_regs) / sizeof(preferred_regs[0]);
       ++i) {
    if (get_register_by_id(get_reg_stack(), preferred_regs[i], &replacement,
                           inst_list) == 0 &&
        replacement != NULL)
      break;
    replacement = NULL;
  }

  if (replacement == NULL)
    replacement = codegen_try_get_reg(inst_list, ctx, label);

  if (replacement == NULL)
    return reg;

  {
    Register_t *d[] = {replacement};
    Register_t *u[] = {reg};
    *inst_list = add_inst_du(*inst_list, ctx, d, 1, u, 1, "\tmovq\t%1, %0\n");
  }
  free_reg(get_reg_stack(), reg);
  return replacement;
}

ListNode_t *codegen_char_set_address(struct Expression *expr,
                                     ListNode_t *inst_list, CodeGenContext *ctx,
                                     Register_t **out_reg) {
  if (expr == NULL || out_reg == NULL)
    return inst_list;

  Register_t *addr_reg = NULL;
  if (codegen_expr_is_addressable(expr)) {
    inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
  } else if ((expr->type == EXPR_ADDOP || expr->type == EXPR_MULOP) &&
             expr_has_type_tag(expr, SET_TYPE)) {
    /* Set binary operation on char sets: the standard expr tree path only
       handles 4-byte sets.  Recursively materialise 32-byte operand
       addresses and call the appropriate runtime helper. */
    struct Expression *left_op = NULL;
    struct Expression *right_op = NULL;
    const char *runtime_func = NULL;

    if (expr->type == EXPR_ADDOP) {
      int op = expr->expr_data.addop_data.addop_type;
      left_op = expr->expr_data.addop_data.left_expr;
      right_op = expr->expr_data.addop_data.right_term;
      if (op == PLUS)
        runtime_func = "kgpc_set_union_256";
      else if (op == MINUS)
        runtime_func = "kgpc_set_diff_256";
    } else /* EXPR_MULOP */
    {
      int op = expr->expr_data.mulop_data.mulop_type;
      left_op = expr->expr_data.mulop_data.left_term;
      right_op = expr->expr_data.mulop_data.right_factor;
      if (op == STAR)
        runtime_func = "kgpc_set_intersect_256";
      else if (op == XOR)
        runtime_func = "kgpc_set_symdiff_256";
    }

    if (runtime_func != NULL && left_op != NULL && right_op != NULL) {
      /* Allocate 32-byte temp on stack for the result */
      StackNode_t *temp = codegen_alloc_temp_bytes("cset_binop", 32);
      if (temp == NULL) {
        *out_reg = NULL;
        return inst_list;
      }

      /* Get addresses of both operands (recursive for nested ops) */
      Register_t *left_reg = NULL;
      inst_list = codegen_char_set_address(left_op, inst_list, ctx, &left_reg);
      if (codegen_had_error(ctx) || left_reg == NULL) {
        if (left_reg != NULL)
          free_reg(get_reg_stack(), left_reg);
        *out_reg = NULL;
        return inst_list;
      }

      Register_t *right_reg = NULL;
      inst_list =
          codegen_char_set_address(right_op, inst_list, ctx, &right_reg);
      if (codegen_had_error(ctx) || right_reg == NULL) {
        if (right_reg != NULL)
          free_reg(get_reg_stack(), right_reg);
        free_reg(get_reg_stack(), left_reg);
        *out_reg = NULL;
        return inst_list;
      }

      /* Spill left/right to stack since the call clobbers argument regs */
      StackNode_t *left_spill = codegen_alloc_temp_bytes("cset_lspill", 8);
      StackNode_t *right_spill = codegen_alloc_temp_bytes("cset_rspill", 8);
      {
        /* Integrated: store a physical register to the frame slot via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(left_spill->offset)}}};
        BeOperand a = {OPK_PHYS, BE_W64, {.phys = left_reg->bit_64}};
        kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
        inst_list = em.list;
      }
      {
        /* Integrated: store a physical register to the frame slot via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(right_spill->offset)}}};
        BeOperand a = {OPK_PHYS, BE_W64, {.phys = right_reg->bit_64}};
        kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
        inst_list = em.list;
      }
      free_reg(get_reg_stack(), left_reg);
      free_reg(get_reg_stack(), right_reg);

      /* Set up call: runtime_func(dest, left, right) */
      addr_reg = get_free_reg(get_reg_stack(), &inst_list);
      if (addr_reg == NULL)
        addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
      if (addr_reg == NULL) {
        codegen_report_error(
            ctx, "ERROR: Unable to allocate register for char-set compare.");
        *out_reg = NULL;
        return inst_list;
      }
      {
        /* Integrated: address-of the frame slot into a physical register via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = addr_reg->bit_64}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(temp->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }

      if (codegen_target_is_windows()) {
        {
          Register_t *u[] = {addr_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
        }
        {
          /* Integrated: load from the frame slot into a physical register via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdx"}};
          BeOperand src = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(left_spill->offset)}}};
          kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        {
          /* Integrated: load from the frame slot into a physical register via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%r8"}};
          BeOperand src = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(right_spill->offset)}}};
          kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
      } else {
        {
          Register_t *u[] = {addr_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
        }
        {
          /* Integrated: load from the frame slot into a physical register via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rsi"}};
          BeOperand src = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(left_spill->offset)}}};
          kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        {
          /* Integrated: load from the frame slot into a physical register via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdx"}};
          BeOperand src = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(right_spill->offset)}}};
          kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
      }

      inst_list = codegen_vect_reg(inst_list, 0);
      inst_list = codegen_call_with_shadow_space(inst_list, runtime_func);
      free_arg_regs();

      /* addr_reg now points to the temp buffer with the 32-byte result.
         Reload it in case the call clobbered it. */
      {
        /* Integrated: address-of the frame slot into a physical register via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = addr_reg->bit_64}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(temp->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
    } else {
      /* Unknown set op — fall back to value-based codegen */
      inst_list = codegen_set_expr(expr, inst_list, ctx, &addr_reg);
    }
  } else if (expr->type == EXPR_SET) {
    /* Force char set mode for set literals in a char set context */
    inst_list = codegen_set_literal(expr, inst_list, ctx, &addr_reg, 1);
  } else {
    inst_list = codegen_set_expr(expr, inst_list, ctx, &addr_reg);
  }

  if (codegen_had_error(ctx) || addr_reg == NULL) {
    *out_reg = NULL;
    return inst_list;
  }

  addr_reg = codegen_clone_register_if_rcx(&inst_list, ctx, addr_reg,
                                           "char set addr spill");
  *out_reg = addr_reg;
  return inst_list;
}
