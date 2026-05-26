/*
    Damon Gwinn
    Tree of simple expressions for the gencode algorithm
    TODO: Does not handle real numbers
    TODO: Does not handle panic case (not enough registers)
*/

#include "../../../../cparser/parser.h"
#include "../../../Parser/List/List.h"
#include "../../../Parser/ParseTree/KgpcType.h"
#include "../../../Parser/ParseTree/from_cparser.h"
#include "../../../Parser/ParseTree/tree.h"
#include "../../../Parser/ParseTree/tree_types.h"
#include "../../../Parser/SemanticCheck/SemCheck.h"
#include "../../../Parser/SemanticCheck/SemChecks/SemCheck_Expr_Internal.h"
#include "../../../Parser/SemanticCheck/SemChecks/SemCheck_expr.h"
#include "../../../Parser/SemanticCheck/SemChecks/SemCheck_sizeof.h"
#include "../../../Parser/SemanticCheck/SemChecks/SemCheck_stmt.h"
#include "../../../Parser/pascal_frontend.h"
#include "../../../flags.h"
#include "../../../identifier_utils.h"
#include "../codegen.h"
#include "../codegen_expression.h"
#include "../codegen_stmt_internal.h"
#include "../register_types.h"
#include "../stackmng/stackmng.h"
#include "expr_tree.h"
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "expr_tree_internal.h"

static int expr_tree_is_large_set_expr(const struct Expression *expr,
                                       CodeGenContext *ctx) {
  if (expr == NULL)
    return 0;

  if (expr_is_char_set_ctx(expr, ctx))
    return 1;

  KgpcType *type = expr_get_kgpc_type(expr);
  if (type != NULL && kgpc_type_is_set(type) && kgpc_type_sizeof(type) > 4)
    return 1;

  if (expr->type == EXPR_ADDOP && expr_has_type_tag(expr, SET_TYPE)) {
    return expr_tree_is_large_set_expr(expr->expr_data.addop_data.left_expr,
                                       ctx) ||
           expr_tree_is_large_set_expr(expr->expr_data.addop_data.right_term,
                                       ctx);
  }

  if (expr->type == EXPR_MULOP && expr_has_type_tag(expr, SET_TYPE)) {
    return expr_tree_is_large_set_expr(expr->expr_data.mulop_data.left_term,
                                       ctx) ||
           expr_tree_is_large_set_expr(expr->expr_data.mulop_data.right_factor,
                                       ctx);
  }

  return 0;
}

static int expr_tree_is_any_set_expr(const struct Expression *expr,
                                     CodeGenContext *ctx) {
  if (expr == NULL)
    return 0;
  if (expr_tree_is_large_set_expr(expr, ctx))
    return 1;
  KgpcType *type = expr_get_kgpc_type(expr);
  if (type != NULL && kgpc_type_is_set(type))
    return 1;
  if (expr->type == EXPR_SET || expr_has_type_tag(expr, SET_TYPE))
    return 1;
  if (expr->type == EXPR_ADDOP && expr_has_type_tag(expr, SET_TYPE))
    return 1;
  if (expr->type == EXPR_MULOP && expr_has_type_tag(expr, SET_TYPE))
    return 1;
  return 0;
}

static long long expr_tree_set_size_bytes(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  KgpcType *type = expr_get_kgpc_type(expr);
  if (type != NULL && kgpc_type_is_set(type)) {
    long long sz = kgpc_type_sizeof(type);
    if (sz > 0)
      return sz;
  }
  return 0;
}

ListNode_t *gencode_leaf_var(struct Expression *expr, ListNode_t *inst_list,
                             CodeGenContext *ctx, char *buffer, int buf_len,
                             OperandKind *out_kind) {
  assert(expr != NULL);
  assert(buffer != NULL);

  /* Default to LABEL; updated below as specific operand kinds are determined */
  if (out_kind != NULL)
    *out_kind = OPKIND_LABEL;

  StackNode_t *stack_node;
  int offset;

  switch (expr->type) {
  case EXPR_VAR_ID:
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: gencode_leaf_var: id = %s\n", expr->expr_data.id);
#endif
    {
      int scope_depth = 0;
      stack_node = NULL;

      if (ctx != NULL && ctx->current_return_slot != NULL &&
          expr->expr_data.id != NULL) {
        int is_current_result = 0;
        if (pascal_identifier_equals(expr->expr_data.id, "Result")) {
          HashNode_t *shadow_node = NULL;
          if (!(ctx->symtab != NULL &&
                FindSymbol(&shadow_node, ctx->symtab, expr->expr_data.id) !=
                    0 &&
                shadow_node != NULL))
            is_current_result = 1;
        } else if (ctx->current_subprogram_id != NULL &&
                   pascal_identifier_equals(expr->expr_data.id,
                                            ctx->current_subprogram_id))
          is_current_result = 1;
        else if (ctx->current_subprogram_method_name != NULL &&
                 pascal_identifier_equals(expr->expr_data.id,
                                          ctx->current_subprogram_method_name))
          is_current_result = 1;
        else if (ctx->current_subprogram_result_name != NULL &&
                 pascal_identifier_equals(expr->expr_data.id,
                                          ctx->current_subprogram_result_name))
          is_current_result = 1;

        if (is_current_result)
          stack_node = ctx->current_return_slot;
      }

      /* First check if this is a constant - constants don't need non-local
       * access */
      HashNode_t *node = expr_tree_find_preferred_symbol(ctx, expr);
      int found = (node != NULL);
      if (!found && ctx != NULL) {
        node = codegen_find_owner_unit_symbol(ctx, expr->expr_data.id);
        found = (node != NULL);
      }
      if (found && node != NULL &&
          (node->hash_type == HASHTYPE_CONST || node->is_constant) &&
          ctx != NULL && ctx->symtab != NULL) {
        ListNode_t *all = FindAllIdents(ctx->symtab, expr->expr_data.id);
        for (ListNode_t *cur = all; cur != NULL; cur = cur->next) {
          HashNode_t *alt = (HashNode_t *)cur->cur;
          if (alt == NULL || alt == node)
            continue;
          if (!(alt->hash_type == HASHTYPE_VAR ||
                alt->hash_type == HASHTYPE_ARRAY ||
                alt->hash_type == HASHTYPE_FUNCTION_RETURN)) {
            continue;
          }
          if (!expr_tree_symbol_matches_expr_type(alt, expr))
            continue;
          node = alt;
          found = 1;
          break;
        }
        if (all != NULL)
          DestroyList(all);
      }
      /* If FindSymbol returned a callable/type-like symbol but there is a
       * constant with the same name in the active/user scopes (or builtin
       * scope), prefer the constant. This keeps enum literals and user
       * constants from being shadowed by unrelated imported procedures or
       * builtins, without letting globals override local variables/params. */
      if (found && node != NULL &&
          !(node->hash_type == HASHTYPE_CONST || node->is_constant) &&
          (node->hash_type == HASHTYPE_FUNCTION ||
           node->hash_type == HASHTYPE_PROCEDURE ||
           node->hash_type == HASHTYPE_BUILTIN_PROCEDURE ||
           node->hash_type == HASHTYPE_TYPE) &&
          ctx != NULL && ctx->symtab != NULL) {
        /* Check user scope for a constant with the same name */
        HashNode_t *user_const = NULL;
        ScopeNode *scope = ctx->symtab->current_scope;
        while (scope != NULL && user_const == NULL) {
          ListNode_t *all =
              FindAllIdentsInTable(scope->table, expr->expr_data.id);
          for (ListNode_t *a = all; a != NULL; a = a->next) {
            HashNode_t *h = (HashNode_t *)a->cur;
            if (h != NULL &&
                (h->hash_type == HASHTYPE_CONST || h->is_constant)) {
              user_const = h;
              break;
            }
          }
          DestroyList(all);
          scope = scope->parent;
        }
        if (user_const != NULL) {
          node = user_const;
        } else if (ctx->symtab->current_unit_index > 0 &&
                   ctx->symtab->current_unit_index < SYMTAB_MAX_UNITS &&
                   ctx->symtab->unit_scopes[ctx->symtab->current_unit_index] !=
                       NULL &&
                   ctx->symtab->unit_scopes[ctx->symtab->current_unit_index]
                           ->table != NULL) {
          HashNode_t *unit_const = FindIdentInTableForUnit(
              ctx->symtab->unit_scopes[ctx->symtab->current_unit_index]->table,
              expr->expr_data.id, ctx->symtab->current_unit_index);
          if (unit_const != NULL && (unit_const->hash_type == HASHTYPE_CONST ||
                                     unit_const->is_constant)) {
            node = unit_const;
          }
        } else if (ctx->symtab->builtin_scope->table != NULL) {
          HashNode_t *builtin_node = FindIdentInTable(
              ctx->symtab->builtin_scope->table, expr->expr_data.id);
          if (builtin_node != NULL &&
              (builtin_node->hash_type == HASHTYPE_CONST ||
               builtin_node->is_constant))
            node = builtin_node;
        }
      }
      if (found && node != NULL)
        node = codegen_prefer_visible_var_over_const(ctx, expr->expr_data.id,
                                                     node);

      if (stack_node == NULL &&
          !(found && node != NULL &&
            (node->hash_type == HASHTYPE_CONST || node->is_constant)))
        stack_node = find_label_with_depth(expr->expr_data.id, &scope_depth);
#ifdef DEBUG_CODEGEN
      CODEGEN_DEBUG(
          "DEBUG: gencode_leaf_var: stack_node = %p, scope_depth = %d\n",
          stack_node, scope_depth);
#endif

      if (found && node != NULL &&
          (node->hash_type == HASHTYPE_CONST || node->is_constant)) {
        /* Check if this is a procedure address constant */
        if (node->type != NULL && node->type->kind == TYPE_KIND_PROCEDURE &&
            node->const_string_value != NULL) {
          /* Procedure address constant - load address of the referenced
           * procedure. The const_string_value holds the original procedure
           * name. We need to mangle it and use RIP-relative addressing so the
           * generated code will use leaq for proper label loading. */
          char mangled_name[256];
          snprintf(mangled_name, sizeof(mangled_name), "%s_void",
                   node->const_string_value);
          /* Use RIP-relative format for label - this causes leaq to be
           * generated */
          snprintf(buffer, buf_len, "%s(%%rip)", mangled_name);
          if (out_kind)
            *out_kind = OPKIND_LABEL;
        }
        /* Check if this is a real constant */
        else if (node->type != NULL &&
                 kgpc_type_equals_tag(node->type, REAL_TYPE)) {
          long long real_size = kgpc_type_sizeof(node->type);
          if (real_size == 4) {
            /* Single constant: keep 32-bit IEEE payload so later marshalling
             * can move it with movd without losing value bits. */
            union {
              float f;
              uint32_t i;
            } converter;
            converter.f = (float)node->const_real_value;
            snprintf(buffer, buf_len, "$%u", (unsigned)converter.i);
            if (out_kind)
              *out_kind = OPKIND_IMMEDIATE;
          } else {
            /* Double/Real/Extended constants are materialized as 64-bit
             * payload. */
            union {
              double d;
              int64_t i;
            } converter;
            converter.d = node->const_real_value;
            snprintf(buffer, buf_len, "$%lld", (long long)converter.i);
            if (out_kind)
              *out_kind = OPKIND_IMMEDIATE;
          }
        }
        /* Check if this is a set constant that fits in 8 bytes */
        else if (node->const_set_value != NULL && node->const_set_size > 0 &&
                 node->const_set_size <= (int)sizeof(long long)) {
          /* Small set constant - use const_int_value */
          snprintf(buffer, buf_len, "$%lld", node->const_int_value);
          if (out_kind)
            *out_kind = OPKIND_IMMEDIATE;
        }
        /* Check if this is a character set (32 bytes) - needs special handling
         */
        else if (node->const_set_value != NULL &&
                 node->const_set_size > (int)sizeof(long long)) {
          /* Large set constant (e.g., character set of 32 bytes).
           * This cannot be represented as an immediate value.
           * We need to emit the set in rodata and return its address. */
          inst_list = codegen_emit_const_set_rodata(node, inst_list, ctx);
          if (node->const_set_label != NULL) {
            snprintf(buffer, buf_len, "%s(%%rip)", node->const_set_label);
            if (out_kind)
              *out_kind = OPKIND_LABEL;
          } else {
            /* Error: failed to emit set constant to rodata.
             * This indicates a bug in codegen_emit_const_set_rodata. */
            codegen_report_error(
                ctx, "ERROR: Failed to emit large set constant '%s' to rodata.",
                expr->expr_data.id ? expr->expr_data.id : "(unknown)");
            snprintf(buffer, buf_len, "$0");
            if (out_kind)
              *out_kind = OPKIND_IMMEDIATE;
          }
        } else if (node->const_string_value != NULL) {
          /* Check if this is a single-char constant (Char type).
           * If the expression's resolved type is STRING_TYPE, emit
           * as a string constant instead (e.g. sLineBreak promoted
           * from Char to String in a comparison context). */
          if (node->type != NULL && node->type->kind == TYPE_KIND_PRIMITIVE &&
              node->type->info.primitive_type_tag == CHAR_TYPE &&
              expr_get_type_tag(expr) != STRING_TYPE) {
            unsigned char ch = (unsigned char)node->const_string_value[0];
            snprintf(buffer, buf_len, "$%d", (int)ch);
            if (out_kind)
              *out_kind = OPKIND_IMMEDIATE;
          } else {
            /* String constant - emit in rodata and use its address */
            char label[20];
            snprintf(label, 20, ".LC%d", ctx->write_label_counter++);
            char add_rodata[1024];
            const char *readonly_section = codegen_readonly_section_directive();
            char *escaped =
                escape_string_for_assembly(node->const_string_value);
            snprintf(add_rodata, 1024, "%s\n%s:\n\t.string \"%s\"\n%s\n",
                     readonly_section, label,
                     escaped ? escaped : node->const_string_value,
                     codegen_text_section_resume());
            if (escaped)
              free(escaped);
            inst_list = add_inst(inst_list, add_rodata);
            snprintf(buffer, buf_len, "%s(%%rip)", label);
            if (out_kind)
              *out_kind = OPKIND_LABEL;
          }
        } else {
          /* Integer constant */
          snprintf(buffer, buf_len, "$%lld", node->const_int_value);
          if (out_kind)
            *out_kind = OPKIND_IMMEDIATE;
        }
      } else if (found && node->hash_type == HASHTYPE_TYPE &&
                 node->type != NULL && node->type->kind == TYPE_KIND_POINTER &&
                 node->type->info.points_to != NULL &&
                 node->type->info.points_to->kind == TYPE_KIND_RECORD &&
                 node->type->info.points_to->info.record_info != NULL &&
                 record_type_is_class(
                     node->type->info.points_to->info.record_info)) {
        /* Class type used as value -> Address of VMT
         * Use RIP-relative addressing for cross-platform compatibility
         * (Windows x64 doesn't support $symbol immediates) */
        /* Use the canonical type_id from the record_info for the VMT
         * label so it matches the emitted VMT definition (Pascal is
         * case-insensitive, so the expression id may differ in case). */
        const char *vmt_class_label =
            node->type->info.points_to->info.record_info->type_id;
        if (vmt_class_label == NULL)
          vmt_class_label = expr->expr_data.id;
        snprintf(buffer, buf_len, "%s_VMT(%%rip)", vmt_class_label);
        if (out_kind)
          *out_kind = OPKIND_LABEL;
      } else if (stack_node != NULL) {
        if (stack_node->is_static) {
          const char *label = (stack_node->static_label != NULL)
                                  ? stack_node->static_label
                                  : stack_node->label;
          snprintf(buffer, buf_len, "%s(%%rip)", label);
          if (out_kind)
            *out_kind = OPKIND_LABEL;
        } else if (scope_depth == 0) {
          /* Variable is in current scope, access normally */
          snprintf(buffer, buf_len, "-%d(%%rbp)", stack_node->offset);
          if (out_kind)
            *out_kind = OPKIND_MEMORY;
        } else {
          Register_t *frame_reg =
              codegen_acquire_static_link(ctx, &inst_list, scope_depth);
          if (frame_reg != NULL) {
            snprintf(buffer, buf_len, "-%d(%s)", stack_node->offset,
                     frame_reg->bit_64);
            if (out_kind)
              *out_kind = OPKIND_MEMORY;
          } else {
            codegen_report_error(
                ctx, "ERROR: Failed to acquire static link for variable %s.",
                expr->expr_data.id);
            snprintf(buffer, buf_len, "-%d(%%rbp)", stack_node->offset);
            if (out_kind)
              *out_kind = OPKIND_MEMORY;
          }
        }
      } else {
        if (found && node != NULL && node->mangled_id != NULL) {
          StackNode_t *mangled_stack_node = find_label(node->mangled_id);
          if (mangled_stack_node != NULL) {
            if (mangled_stack_node->is_static) {
              const char *label = (mangled_stack_node->static_label != NULL)
                                      ? mangled_stack_node->static_label
                                      : mangled_stack_node->label;
              snprintf(buffer, buf_len, "%s(%%rip)", label);
              if (out_kind)
                *out_kind = OPKIND_LABEL;
              break;
            }
            snprintf(buffer, buf_len, "-%d(%%rbp)", mangled_stack_node->offset);
            if (out_kind)
              *out_kind = OPKIND_MEMORY;
            break;
          }
        }

        /* Bare method name used as a value (e.g. @SetStatus
         * inside a class method from a cached unit).  Try
         * class-qualified lookup: OwnerClass__MethodName. */
        int resolved_as_method = 0;
        if (ctx != NULL && ctx->symtab != NULL) {
          const char *method_owner = ctx->current_subprogram_owner_class;
          if (method_owner != NULL) {
            char qual_name[512];
            snprintf(qual_name, sizeof(qual_name), "%s__%s", method_owner,
                     expr->expr_data.id);
            ListNode_t *candidates = FindAllIdents(ctx->symtab, qual_name);
            for (ListNode_t *c = candidates; c != NULL; c = c->next) {
              HashNode_t *cand = (HashNode_t *)c->cur;
              if (cand != NULL && cand->mangled_id != NULL &&
                  cand->type != NULL &&
                  cand->type->kind == TYPE_KIND_PROCEDURE) {
                snprintf(buffer, buf_len, "%s(%%rip)", cand->mangled_id);
                if (out_kind)
                  *out_kind = OPKIND_LABEL;
                resolved_as_method = 1;
                break;
              }
            }
            if (candidates != NULL)
              DestroyList(candidates);
          }
        }
        if (resolved_as_method)
          break;

        const char *var_name = expr != NULL ? expr->expr_data.id : "<unknown>";
        size_t name_len = var_name != NULL ? strlen(var_name) : 0;
        int is_vmt_label =
            (name_len > 4 && strcmp(var_name + name_len - 4, "_VMT") == 0);

        int is_builtin_file = 0;
        const char *global_ptr_name = NULL;
        if (var_name != NULL) {
          if (strcasecmp(var_name, "stdin") == 0) {
            is_builtin_file = 1;
            global_ptr_name = "stdin_ptr";
          } else if (strcasecmp(var_name, "stdout") == 0) {
            is_builtin_file = 1;
            global_ptr_name = "stdout_ptr";
          } else if (strcasecmp(var_name, "stderr") == 0) {
            is_builtin_file = 1;
            global_ptr_name = "stderr_ptr";
          } else if (strcasecmp(var_name, "StdIn") == 0) {
            is_builtin_file = 1;
            global_ptr_name = "stdin_ptr";
          } else if (strcasecmp(var_name, "StdOut") == 0) {
            is_builtin_file = 1;
            global_ptr_name = "stdout_ptr";
          } else if (strcasecmp(var_name, "StdErr") == 0 ||
                     strcasecmp(var_name, "ErrOutput") == 0) {
            is_builtin_file = 1;
            global_ptr_name = "stderr_ptr";
          } else if (strcasecmp(var_name, "Input") == 0) {
            is_builtin_file = 1;
            global_ptr_name = "Input_ptr";
          } else if (strcasecmp(var_name, "Output") == 0) {
            is_builtin_file = 1;
            global_ptr_name = "Output_ptr";
          }
        }

        if (is_vmt_label) {
          snprintf(buffer, buf_len, "%s(%%rip)", var_name);
          if (out_kind)
            *out_kind = OPKIND_LABEL;
        } else if (is_builtin_file) {
          snprintf(buffer, buf_len, "%s(%%rip)", global_ptr_name);
          if (out_kind)
            *out_kind = OPKIND_LABEL;
        } else {
          inst_list =
              codegen_get_nonlocal(inst_list, expr->expr_data.id, &offset, ctx);
          snprintf(buffer, buf_len, "-%d(%s)", offset,
                   current_non_local_reg64());
          if (out_kind)
            *out_kind = OPKIND_MEMORY;
        }
      }
    }

    break;

  case EXPR_INUM:
    snprintf(buffer, buf_len, "$%lld", expr->expr_data.i_num);
    if (out_kind)
      *out_kind = OPKIND_IMMEDIATE;
    break;

  case EXPR_CHAR_CODE:
    snprintf(buffer, buf_len, "$%u", expr->expr_data.char_code);
    if (out_kind)
      *out_kind = OPKIND_IMMEDIATE;
    break;

  case EXPR_RNUM: {
    if (expr_is_single_real_with_symtab(expr,
                                        ctx != NULL ? ctx->symtab : NULL)) {
      union {
        float f;
        uint32_t i;
      } converter;
      converter.f = (float)expr->expr_data.r_num;
      snprintf(buffer, buf_len, "$%u", (unsigned)converter.i);
    } else {
      /* Use union for safe type punning */
      union {
        double d;
        int64_t i;
      } converter;
      converter.d = expr->expr_data.r_num;
      snprintf(buffer, buf_len, "$%lld", (long long)converter.i);
    }
    if (out_kind)
      *out_kind = OPKIND_IMMEDIATE;
    break;
  }

  case EXPR_BOOL:
    snprintf(buffer, buf_len, "$%d", expr->expr_data.bool_value ? 1 : 0);
    if (out_kind)
      *out_kind = OPKIND_IMMEDIATE;
    break;

  case EXPR_NIL:
    snprintf(buffer, buf_len, "$0");
    if (out_kind)
      *out_kind = OPKIND_IMMEDIATE;
    break;

  case EXPR_SET:
    snprintf(buffer, buf_len, "$%u", expr->expr_data.set_data.bitmask);
    if (out_kind)
      *out_kind = OPKIND_IMMEDIATE;
    break;

  default:
    assert(0 && "Unsupported expr type in gencode!");
    break;
  }

  return inst_list;
}

ListNode_t *gencode_op(struct Expression *expr, const char *left,
                       const Register_t *left_reg, const char *right,
                       const Register_t *right_reg, OperandKind left_kind,
                       OperandKind right_kind, ListNode_t *inst_list,
                       CodeGenContext *ctx) {
  assert(expr != NULL);
  assert(left != NULL);
  assert(right != NULL);

  int type;
  char buffer[128];

  switch (expr->type) {
  case EXPR_ADDOP:
    type = expr->expr_data.addop_data.addop_type;
    if (expr_get_type_tag(expr) == SET_TYPE) {
      /* Set operations use 32-bit instructions; ensure register names are
       * 32-bit */
      const char *left32 = (left_reg != NULL) ? left_reg->bit_32 : left;
      const char *right32 = (right_reg != NULL) ? right_reg->bit_32 : right;
      switch (type) {
      case PLUS:
        snprintf(buffer, sizeof(buffer), "\torl\t%s, %s\n", right32, left32);
        inst_list = add_inst(inst_list, buffer);
        break;
      case MINUS:
        if (right[0] == '$') {
          unsigned long mask = strtoul(right + 1, NULL, 0);
          unsigned int complement = ~((unsigned int)mask);
          snprintf(buffer, sizeof(buffer), "\tandl\t$%u, %s\n", complement,
                   left32);
          inst_list = add_inst(inst_list, buffer);
        } else {
          const char *scratch_reg = "%r10d";
          if (left_reg != NULL && left_reg->reg_id == REG_R10)
            scratch_reg = "%r11d";
          else if (left_reg != NULL && left_reg->reg_id == REG_R11)
            scratch_reg = "%r10d";

          snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", right32,
                   scratch_reg);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tnotl\t%s\n", scratch_reg);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tandl\t%s, %s\n", scratch_reg,
                   left32);
          inst_list = add_inst(inst_list, buffer);
        }
        break;
      default:
        assert(0 && "Unsupported set addop type!");
        break;
      }
      break;
    }
    if (expr_get_type_tag(expr) == REAL_TYPE || type == SLASH) {
      const char *sse_op = NULL;
      switch (type) {
      case PLUS:
        sse_op = "addsd";
        break;
      case MINUS:
        sse_op = "subsd";
        break;
      default:
        assert(0 && "Unsupported real addop type!");
        break;
      }
      if (sse_op != NULL)
        inst_list = gencode_real_binary_op(
            ctx, expr->expr_data.mulop_data.left_term, left, left_reg,
            expr->expr_data.mulop_data.right_factor, right, right_reg, left,
            left_reg, inst_list, sse_op);
      break;
    }
    /* Handle pointer-pointer subtraction: result is element difference */
    if (expr->is_pointer_diff && type == MINUS) {
      struct Expression *left_expr = expr->expr_data.addop_data.left_expr;

      /* Convert operands to 64-bit for pointer operations */
      const char *left64 = reg32_to_reg64(left, left_reg);
      const char *right64 = reg32_to_reg64(right, right_reg);

      /* Get element size from the pointer type.
       * For typed pointers like PByte (^Byte), use pointer_subtype_id to get
       * correct size. If pointer_subtype_id is set, use UNKNOWN_TYPE so lookup
       * by name works properly.
       */
      long long element_size = 1;
      if (left_expr != NULL) {
        int lookup_type = left_expr->pointer_subtype;
        const char *lookup_id = left_expr->pointer_subtype_id;

        if (lookup_type == POINTER_TYPE && lookup_id == NULL)
          lookup_type = UNKNOWN_TYPE;

        /* If we have a type name, prioritize it over the type tag */
        if (lookup_id != NULL)
          lookup_type = UNKNOWN_TYPE;

        if (lookup_type != UNKNOWN_TYPE || lookup_id != NULL) {
          if (codegen_sizeof_type_reference(ctx, lookup_type, lookup_id,
                                            left_expr->record_type,
                                            &element_size) != 0 ||
              element_size <= 0) {
            element_size = 1; /* Default to byte size */
          }
        }
      }
      /* Also check the expression's own type info (set during semcheck) */
      else if (expr->pointer_subtype != UNKNOWN_TYPE ||
               expr->pointer_subtype_id != NULL) {
        int lookup_type = expr->pointer_subtype;
        const char *lookup_id = expr->pointer_subtype_id;

        if (lookup_type == POINTER_TYPE && lookup_id == NULL)
          lookup_type = UNKNOWN_TYPE;

        /* If we have a type name, prioritize it over the type tag */
        if (lookup_id != NULL)
          lookup_type = UNKNOWN_TYPE;

        if (codegen_sizeof_type_reference(ctx, lookup_type, lookup_id,
                                          expr->record_type,
                                          &element_size) != 0 ||
            element_size <= 0) {
          element_size = 1;
        }
      }

      /* Subtract pointers: left64 = left64 - right64 (in bytes) */
      snprintf(buffer, sizeof(buffer), "\tsubq\t%s, %s\n", right64, left64);
      inst_list = add_inst(inst_list, buffer);

      /* Divide by element size to get element count */
      if (element_size > 1) {
        /* Use signed division (idiv) to handle negative differences */
        /* Result in RAX, remainder in RDX, we need to set up dividend in
         * RDX:RAX */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rax\n", left64);
        inst_list = add_inst(inst_list, buffer);
        /* Sign-extend RAX into RDX:RAX */
        snprintf(buffer, sizeof(buffer), "\tcqto\n");
        inst_list = add_inst(inst_list, buffer);
        /* Load divisor */
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %%r11\n",
                 element_size);
        inst_list = add_inst(inst_list, buffer);
        /* Divide */
        snprintf(buffer, sizeof(buffer), "\tidivq\t%%r11\n");
        inst_list = add_inst(inst_list, buffer);
        /* Move result back to left64 */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", left64);
        inst_list = add_inst(inst_list, buffer);
      }
      break;
    }
    /* Handle pointer arithmetic: pointer + integer or integer + pointer */
    if (expr_get_type_tag(expr) == POINTER_TYPE &&
        (type == PLUS || type == MINUS)) {
      struct Expression *left_expr = expr->expr_data.addop_data.left_expr;
      struct Expression *right_expr = expr->expr_data.addop_data.right_term;

      int left_is_pointer =
          (left_expr != NULL && expr_get_type_tag(left_expr) == POINTER_TYPE);
      int right_is_pointer =
          (right_expr != NULL && expr_get_type_tag(right_expr) == POINTER_TYPE);

      /* Promote operands to 64-bit registers for pointer operations */
      const char *left64 = reg32_to_reg64(left, left_reg);
      const char *right64 = reg32_to_reg64(right, right_reg);
      if (left64 == NULL)
        left64 = left;
      if (right64 == NULL)
        right64 = right;

      /* Sign-extend 32-bit operands to 64-bit if needed */
      if (operand_is_32bit_register(left, left_reg) && left64 != left) {
        snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", left, left64);
        inst_list = add_inst(inst_list, buffer);
      }
      if (operand_is_32bit_register(right, right_reg) && right64 != right) {
        snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", right, right64);
        inst_list = add_inst(inst_list, buffer);
      }

      /* Determine which operand is the pointer and which is the integer */
      const char *ptr_reg = left_is_pointer ? left64 : right64;
      const char *int_reg = left_is_pointer ? right64 : left64;
      const Register_t *ptr_reg_reg = left_is_pointer ? left_reg : right_reg;
      struct Expression *ptr_expr = left_is_pointer ? left_expr : right_expr;

      /* Get element size */
      long long element_size = 1;
      if (ptr_expr != NULL && ptr_expr->resolved_kgpc_type != NULL &&
          ptr_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER &&
          ptr_expr->resolved_kgpc_type->info.points_to != NULL &&
          ptr_expr->pointer_subtype != UNKNOWN_TYPE) {
        int dummy_type = ptr_expr->pointer_subtype;
        if (codegen_sizeof_type_reference(
                ctx, dummy_type, ptr_expr->pointer_subtype_id,
                ptr_expr->record_type, &element_size) != 0 ||
            element_size <= 0) {
          element_size = 8; /* Default to pointer size */
        }
      }

      /* Scale the integer offset by element size */
      if (element_size != 1) {
        /* Check if int_reg is an immediate value */
        if (int_reg[0] == '$') {
          /* It's an immediate - compute the scaled value directly.
           * Use a scratch register that doesn't conflict with ptr_reg. */
          long long int_val = strtoll(int_reg + 1, NULL, 0);
          long long scaled_val = int_val * element_size;
          const char *scratch =
              (ptr_reg_reg != NULL && ptr_reg_reg->reg_id == REG_R11) ? "%r10"
                                                                      : "%r11";
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", scaled_val,
                   scratch);
          inst_list = add_inst(inst_list, buffer);
          int_reg = scratch;
        } else {
          /* It's a register or memory operand — use 3-operand imulq
           * to avoid the invalid 2-operand imulq $imm, mem form.
           * Pick a scratch register that doesn't collide with ptr_reg. */
          const Register_t *int_reg_reg =
              left_is_pointer ? right_reg : left_reg;
          const char *scratch;
          if ((ptr_reg_reg != NULL && ptr_reg_reg->reg_id == REG_R11) ||
              (int_reg_reg != NULL && int_reg_reg->reg_id == REG_R11))
            scratch = "%r10";
          else
            scratch = "%r11";
          snprintf(buffer, sizeof(buffer), "\timulq\t$%lld, %s, %s\n",
                   element_size, int_reg, scratch);
          inst_list = add_inst(inst_list, buffer);
          int_reg = scratch;
        }
      }

      /* Now add or subtract */
      if (type == PLUS) {
        /* For integer + pointer, we need to put result in correct register */
        if (right_is_pointer && left_is_pointer == 0) {
          /* int + ptr: add int to ptr, result goes to left */
          snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n", ptr_reg, left64);
        } else {
          /* ptr + int: add int to ptr */
          snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n", int_reg, left64);
        }
      } else /* MINUS */
      {
        /* ptr - int: subtract int from ptr */
        snprintf(buffer, sizeof(buffer), "\tsubq\t%s, %s\n", int_reg, left64);
      }
      inst_list = add_inst(inst_list, buffer);
      break;
    }
    {
      struct Expression *left_expr = expr->expr_data.addop_data.left_expr;
      struct Expression *right_expr = expr->expr_data.addop_data.right_term;
      const int use_qword_op = codegen_type_uses_qword(expr_get_type_tag(expr));
      const char arith_suffix = use_qword_op ? 'q' : 'l';
      const char *left_op = left;
      const char *right_op = right;
      Register_t *right_mem_tmp = NULL;
      if (arith_suffix == 'l') {
        left_op = reg_to_reg32(left, left_reg);
        if (right != NULL && right[0] == '$')
          right_op = right;
        else
          right_op = reg_to_reg32(right, right_reg);
      } else if (arith_suffix == 'q') {
        left_op = reg_to_reg64(left, left_reg);
        if (right != NULL && right[0] == '$')
          right_op = right;
        else
          right_op = reg_to_reg64(right, right_reg);

        /* qword arithmetic over signed 32-bit operands must sign-extend
         * those inputs before using their 64-bit register names. Without
         * this, values like -2 loaded with movl become 0x00000000fffffffe
         * and break Int64 accumulation paths such as Math.SumInt/Mean. */
        if (left_expr != NULL &&
            type_tag_is_signed_32bit_int(expr_get_type_tag(left_expr)) &&
            operand_is_32bit_register(left, left_reg) && left_op != NULL &&
            strcmp(left_op, left) != 0) {
          snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", left, left_op);
          inst_list = add_inst(inst_list, buffer);
        }
        if (right_expr != NULL &&
            type_tag_is_signed_32bit_int(expr_get_type_tag(right_expr)) &&
            operand_is_32bit_register(right, right_reg) && right_op != NULL &&
            strcmp(right_op, right) != 0) {
          snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", right,
                   right_op);
          inst_list = add_inst(inst_list, buffer);
        }

        /* When the right operand is a 32-bit memory reference (no register),
         * addq/subq would read 8 bytes from a 4-byte stack slot, getting
         * garbage in the upper 32 bits.  Load into a temp register and
         * sign/zero-extend before the 64-bit arithmetic.
         * (We only handle the right operand here because the left operand
         * is the destination — the result is written back to it, so it
         * must remain addressable.) */
        if (right_reg == NULL && right != NULL && right[0] != '$' &&
            right[0] != '%' && right_expr != NULL &&
            !codegen_type_uses_qword(expr_get_type_tag(right_expr)) &&
            !expr_uses_qword_kgpctype(right_expr) &&
            is_integer_type(expr_get_type_tag(right_expr))) {
          right_mem_tmp = get_free_reg(get_reg_stack(), &inst_list);
          if (right_mem_tmp != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", right,
                     right_mem_tmp->bit_32);
            inst_list = add_inst(inst_list, buffer);
            if (codegen_type_is_signed(expr_get_type_tag(right_expr)))
              inst_list = codegen_sign_extend32_to64(
                  inst_list, right_mem_tmp->bit_32, right_mem_tmp->bit_64);
            right_op = right_mem_tmp->bit_64;
          }
        }
      }
      if (type == OR) {
        int err = 0;
        inst_list = emit_alu_op_with_large_imm(
            inst_list, ctx, "or", arith_suffix, right_op, left_op, &err);
        if (right_mem_tmp != NULL)
          free_reg(get_reg_stack(), right_mem_tmp);
        if (err)
          break;
        break;
      }
      switch (type) {
      case PLUS: {
        if (strcmp(right, "$1") == 0) {
          snprintf(buffer, sizeof(buffer), "\tinc%c\t%s\n", arith_suffix,
                   left_op);
          inst_list = add_inst(inst_list, buffer);
        } else {
          int err = 0;
          inst_list = emit_alu_op_with_large_imm(
              inst_list, ctx, "add", arith_suffix, right_op, left_op, &err);
          if (err)
            break;
        }
        break;
      }
      case MINUS: {
        int err = 0;
        inst_list = emit_alu_op_with_large_imm(
            inst_list, ctx, "sub", arith_suffix, right_op, left_op, &err);
        if (err)
          break;
        break;
      }
      default:
        assert(0 && "Bad addop type!");
        break;
      }
      if (right_mem_tmp != NULL)
        free_reg(get_reg_stack(), right_mem_tmp);
      /* Sign-extend 32-bit result to 64-bit so negative values are
         correctly represented when stored into 64-bit slots. */
      if (!use_qword_op && left_reg != NULL &&
          !is_unsigned_integer_type(expr_get_type_tag(expr))) {
        snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", left_reg->bit_32,
                 left_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
      }

      break;
    }

  case EXPR_MULOP:
    type = expr->expr_data.mulop_data.mulop_type;
    if (expr_get_type_tag(expr) == BOOL && type == AND) {
      snprintf(buffer, sizeof(buffer), "\tandl\t%s, %s\n", right, left);
      inst_list = add_inst(inst_list, buffer);
      break;
    }
    if (expr_get_type_tag(expr) == SET_TYPE) {
      if (expr_tree_is_large_set_expr(expr, ctx)) {
        Register_t *set_addr_reg = NULL;
        inst_list =
            codegen_char_set_address(expr, inst_list, ctx, &set_addr_reg);
        if (set_addr_reg != NULL && left_reg != NULL) {
          if (set_addr_reg->reg_id != left_reg->reg_id) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                     set_addr_reg->bit_64, left_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
          }
        }
        if (set_addr_reg != NULL)
          free_reg(get_reg_stack(), set_addr_reg);
        break;
      }
      /* Set operations use 32-bit instructions; ensure register names are
       * 32-bit */
      const char *left32 = (left_reg != NULL) ? left_reg->bit_32 : left;
      const char *right32 = (right_reg != NULL) ? right_reg->bit_32 : right;
      switch (type) {
      case STAR:
        snprintf(buffer, sizeof(buffer), "\tandl\t%s, %s\n", right32, left32);
        inst_list = add_inst(inst_list, buffer);
        break;
      case XOR:
        snprintf(buffer, sizeof(buffer), "\txorl\t%s, %s\n", right32, left32);
        inst_list = add_inst(inst_list, buffer);
        break;
      default:
        assert(0 && "Unsupported set mulop type!");
        break;
      }
      break;
    }
    if (expr_get_type_tag(expr) == REAL_TYPE || type == SLASH) {
      const char *sse_op = NULL;
      switch (type) {
      case STAR:
        sse_op = "mulsd";
        break;
      case SLASH:
        sse_op = "divsd";
        break;
      case DIV:
      case MOD:
        assert(0 && "Unsupported real mulop type!");
        break;
      default:
        break;
      }
      if (sse_op != NULL)
        inst_list = gencode_real_binary_op(
            ctx, expr->expr_data.mulop_data.left_term, left, left_reg,
            expr->expr_data.mulop_data.right_factor, right, right_reg, left,
            left_reg, inst_list, sse_op);
      break;
    }
    {
      const int use_qword_op = codegen_type_uses_qword(expr_get_type_tag(expr));
      const char arith_suffix = use_qword_op ? 'q' : 'l';
      const char *op_left = left;
      const char *op_right = right;

      if (use_qword_op) {
        const char *left64 = reg32_to_reg64(left, left_reg);
        const char *right64 = reg32_to_reg64(right, right_reg);

        if (operand_is_32bit_register(left, left_reg) && left64 != NULL) {
          int left_tag =
              expr_get_type_tag(expr->expr_data.mulop_data.left_term);
          if (codegen_type_is_signed(left_tag))
            snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", left,
                     left64);
          else
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", left, left);
          inst_list = add_inst(inst_list, buffer);
        }
        if (operand_is_32bit_register(right, right_reg) && right64 != NULL) {
          int right_tag =
              expr_get_type_tag(expr->expr_data.mulop_data.right_factor);
          if (codegen_type_is_signed(right_tag))
            snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", right,
                     right64);
          else
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", right, right);
          inst_list = add_inst(inst_list, buffer);
        }

        if (left64 != NULL)
          op_left = left64;
        if (right64 != NULL)
          op_right = right64;
      } else {
        /* 32-bit operation: ensure 64-bit registers are narrowed to 32-bit form
         */
        const char *left32 = reg64_to_reg32(left, left_reg);
        const char *right32 = reg64_to_reg32(right, right_reg);
        if (left32 != NULL)
          op_left = left32;
        if (right32 != NULL)
          op_right = right32;
      }
      if (type == STAR) {
        int err = 0;
        inst_list = emit_alu_op_with_large_imm(
            inst_list, ctx, "imul", arith_suffix, op_right, op_left, &err);
        if (err)
          break;
        /* Sign-extend 32-bit result to 64-bit so negative values are
           correctly represented when stored into 64-bit slots (e.g.
           passing Integer result as SizeInt/Int64 function argument). */
        if (!use_qword_op && left_reg != NULL &&
            !is_unsigned_integer_type(expr_get_type_tag(expr))) {
          snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n",
                   left_reg->bit_32, left_reg->bit_64);
          inst_list = add_inst(inst_list, buffer);
        }
      } else if (type == AND) {
        int err = 0;
        inst_list = emit_alu_op_with_large_imm(
            inst_list, ctx, "and", arith_suffix, op_right, op_left, &err);
        if (err)
          break;
      } else if (type == MOD) {
        int is_unsigned = is_unsigned_integer_type(expr_get_type_tag(expr));
        if (use_qword_op) {
          snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rax\n", op_left);
          inst_list = add_inst(inst_list, buffer);
          if (is_unsigned)
            inst_list = add_inst(inst_list, "\txorq\t%rdx, %rdx\n");
          else
            inst_list = add_inst(inst_list, "\tcqo\n");

          const char *tmp_div = select_divisor_temp_reg(left_reg, 1);
          snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", op_right,
                   tmp_div);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\t%s\t%s\n",
                   is_unsigned ? "divq" : "idivq", tmp_div);
          inst_list = add_inst(inst_list, buffer);

          snprintf(buffer, sizeof(buffer), "\tmovq\t%%rdx, %s\n", op_left);
          inst_list = add_inst(inst_list, buffer);
        } else {
          const char *mod_left = reg64_to_reg32(left, left_reg);
          const char *mod_right = reg64_to_reg32(right, right_reg);
          snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%eax\n", mod_left);
          inst_list = add_inst(inst_list, buffer);
          if (is_unsigned)
            inst_list = add_inst(inst_list, "\txorl\t%edx, %edx\n");
          else
            inst_list = add_inst(inst_list, "\tcdq\n");

          const char *tmp_div = select_divisor_temp_reg(left_reg, 0);
          snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", mod_right,
                   tmp_div);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\t%s\t%s\n",
                   is_unsigned ? "divl" : "idivl", tmp_div);
          inst_list = add_inst(inst_list, buffer);

          snprintf(buffer, sizeof(buffer), "\tmovl\t%%edx, %s\n", mod_left);
          inst_list = add_inst(inst_list, buffer);
        }
      }
      /* NOTE: Division and modulus is a more special case */
      else if (type == SLASH || type == DIV) {
#ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: gencode_op: left = %s, right = %s\n", left,
                      right);
#endif
        int is_unsigned = is_unsigned_integer_type(expr_get_type_tag(expr));
        // left is the dividend, right is the divisor
        snprintf(buffer, sizeof(buffer), "\tpushq\t%%rdx\n");
        inst_list = add_inst(inst_list, buffer);

        if (use_qword_op) {
          const char *tmp_div = select_divisor_temp_reg(left_reg, 1);

          snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", op_right,
                   tmp_div);
          inst_list = add_inst(inst_list, buffer);

          snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rax\n", op_left);
          inst_list = add_inst(inst_list, buffer);
          if (is_unsigned)
            inst_list = add_inst(inst_list, "\txorq\t%rdx, %rdx\n");
          else
            inst_list = add_inst(inst_list, "\tcqo\n");

          snprintf(buffer, sizeof(buffer), "\t%s\t%s\n",
                   is_unsigned ? "divq" : "idivq", tmp_div);
          inst_list = add_inst(inst_list, buffer);

          snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", op_left);
          inst_list = add_inst(inst_list, buffer);
        } else {
          const char *div_left = reg64_to_reg32(left, left_reg);
          const char *div_right = reg64_to_reg32(right, right_reg);

          const char *tmp_div = select_divisor_temp_reg(left_reg, 0);

          snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", div_right,
                   tmp_div);
          inst_list = add_inst(inst_list, buffer);

          snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%eax\n", div_left);
          inst_list = add_inst(inst_list, buffer);
          if (is_unsigned)
            inst_list = add_inst(inst_list, "\txorl\t%edx, %edx\n");
          else
            inst_list = add_inst(inst_list, "\tcdq\n");

          snprintf(buffer, sizeof(buffer), "\t%s\t%s\n",
                   is_unsigned ? "divl" : "idivl", tmp_div);
          inst_list = add_inst(inst_list, buffer);

          snprintf(buffer, sizeof(buffer), "\tmovl\t%%eax, %s\n", div_left);
          inst_list = add_inst(inst_list, buffer);
        }

        snprintf(buffer, sizeof(buffer), "\tpopq\t%%rdx\n");
        inst_list = add_inst(inst_list, buffer);
      } else if (type == XOR) {
        int err = 0;
        inst_list = emit_alu_op_with_large_imm(
            inst_list, ctx, "xor", arith_suffix, op_right, op_left, &err);
        if (err)
          break;
      } else if (type == SHL) {
        const char *count =
            use_qword_op ? reg64_to_reg32(op_right, right_reg) : op_right;
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", count);
        inst_list = add_inst(inst_list, buffer);
        /* Use SAL to match FPC's emitted mnemonics for left shifts */
        snprintf(buffer, sizeof(buffer), "\tsal%c\t%%cl, %s\n", arith_suffix,
                 op_left);
        inst_list = add_inst(inst_list, buffer);
      } else if (type == SHR) {
        const char *count =
            use_qword_op ? reg64_to_reg32(op_right, right_reg) : op_right;
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", count);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tshr%c\t%%cl, %s\n", arith_suffix,
                 op_left);
        inst_list = add_inst(inst_list, buffer);
      } else if (type == ROL) {
        const char *count =
            use_qword_op ? reg64_to_reg32(op_right, right_reg) : op_right;
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", count);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\trol%c\t%%cl, %s\n", arith_suffix,
                 op_left);
        inst_list = add_inst(inst_list, buffer);
      } else if (type == ROR) {
        const char *count =
            use_qword_op ? reg64_to_reg32(op_right, right_reg) : op_right;
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", count);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tror%c\t%%cl, %s\n", arith_suffix,
                 op_left);
        inst_list = add_inst(inst_list, buffer);
      } else {
        assert(0 && "Bad mulop type!");
        break;
      }

      break;
    }

  case EXPR_RELOP: {
    int relop_kind = expr->expr_data.relop_data.type;
    struct Expression *left_expr = expr->expr_data.relop_data.left;
    struct Expression *right_expr = expr->expr_data.relop_data.right;

    if (relop_kind == NOT) {
      const char *left32 = reg_to_reg32(left, left_reg);
      const char *left8 = reg32_to_reg8(left32, left_reg);
      if (left32 != NULL && left8 != NULL) {
        snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", left32, left32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tsete\t%s\n", left8);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8, left32);
        inst_list = add_inst(inst_list, buffer);
      }
      break;
    }

    if (relop_kind == IN) {
      const char *left32 = reg_to_reg32(left, left_reg);
      const char *left8 = reg32_to_reg8(left32, left_reg);

      int is_char_set =
          (right_expr != NULL && expr_is_char_set_ctx(right_expr, ctx));

      if (is_char_set) {
        /* Large sets (> 4 bytes) need the ADDRESS of the set, not its value.
         * The expr_tree evaluation loaded only 4 bytes into right_reg, which is
         * insufficient for sets > 32 elements.  Discard the value and
         * re-evaluate the set expression as an address via
         * codegen_char_set_address. */
        if (left32 != NULL && left8 != NULL) {
          /* Free the incorrectly-loaded value register */
          if (right_reg != NULL) {
            free_reg(get_reg_stack(), (Register_t *)right_reg);
            right_reg = NULL;
          }

          Register_t *set_addr_reg = NULL;
          inst_list = codegen_char_set_address(right_expr, inst_list, ctx,
                                               &set_addr_reg);
          if (set_addr_reg != NULL) {
            snprintf(buffer, sizeof(buffer), "\tbtl\t%s, (%s)\n", left32,
                     set_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tsetc\t%s\n", left8);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8,
                     left32);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), set_addr_reg);
          }
        }
      } else {
        /* Check if the set is larger than 4 bytes (not a char set, but still
         * too large for register-form btl).  Sets with > 32 elements store
         * bits beyond index 31 in higher bytes.  Register-form btl only sees
         * the first 4 bytes loaded into right_reg, so elements with ordinal >
         * 31 would always appear absent.  Use address + memory-form btl
         * instead. */
        int set_storage_bytes = 0;
        if (right_expr != NULL) {
          KgpcType *set_ktype = expr_get_kgpc_type(right_expr);
          if (set_ktype != NULL && kgpc_type_is_set(set_ktype)) {
            long long sz = kgpc_type_sizeof(set_ktype);
            if (sz > 0)
              set_storage_bytes = (int)sz;
          }
        }

        if (set_storage_bytes > 4 && left32 != NULL && left8 != NULL) {
          /* Large non-char set: discard loaded value register, re-evaluate
           * as address, then use memory-form btl with correct max-bit bound. */
          if (right_reg != NULL) {
            free_reg(get_reg_stack(), (Register_t *)right_reg);
            right_reg = NULL;
          }
          Register_t *set_addr_reg = NULL;
          inst_list = codegen_char_set_address(right_expr, inst_list, ctx,
                                               &set_addr_reg);
          if (set_addr_reg != NULL) {
            int max_bit = set_storage_bytes * 8 - 1;
            char in_oob[18], in_done[18];
            gen_label(in_oob, sizeof(in_oob), ctx);
            gen_label(in_done, sizeof(in_done), ctx);

            snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", left32);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", in_oob);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tcmpl\t$%d, %s\n", max_bit,
                     left32);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", in_oob);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\tbtl\t%s, (%s)\n", left32,
                     set_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tsetc\t%s\n", left8);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", in_done);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "%s:\n", in_oob);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\txorb\t%s, %s\n", left8, left8);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "%s:\n", in_done);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8,
                     left32);
            inst_list = add_inst(inst_list, buffer);

            free_reg(get_reg_stack(), set_addr_reg);
          }
        } else {
          const char *right32 = reg_to_reg32(right, right_reg);
          const char *bit_index = left32;
          const char *bit_base = right32;

          if (bit_base != NULL && bit_base[0] == '$') {
            /* When loading an immediate set value, make sure not to clobber the
             * left operand. Use %r11d if left is in %r10d, otherwise use %r10d.
             */
            const char *temp_reg = "%r10d";
            /* Check if left operand is in r10 (any size: r10, r10d, r10b, r10w)
             */
            if (left_reg != NULL && left_reg->reg_id == REG_R10)
              temp_reg = "%r11d";
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", bit_base,
                     temp_reg);
            inst_list = add_inst(inst_list, buffer);
            bit_base = temp_reg;
          } else if (bit_base != NULL && right_expr != NULL &&
                     right_expr->type == EXPR_VAR_ID &&
                     right_expr->expr_data.id != NULL && ctx != NULL &&
                     ctx->symtab != NULL) {
            /* If right operand is a var-parameter VAR_ID, the operand the
             * leaf produced is the parameter slot which holds the *pointer*
             * to the caller's set, not the set value.  Dereference: load
             * the pointer, then load 4 bytes of the pointed-to set. */
            HashNode_t *node = NULL;
            if (FindSymbol(&node, ctx->symtab, right_expr->expr_data.id) &&
                node != NULL && node->is_var_parameter) {
              const char *temp_reg64 = "%r10";
              const char *temp_reg32 = "%r10d";
              if (left_reg != NULL && left_reg->reg_id == REG_R10) {
                temp_reg64 = "%r11";
                temp_reg32 = "%r11d";
              }
              snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", bit_base,
                       temp_reg64);
              inst_list = add_inst(inst_list, buffer);
              snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", temp_reg64,
                       temp_reg32);
              inst_list = add_inst(inst_list, buffer);
              bit_base = temp_reg32;
            }
          }

          if (left32 != NULL && left8 != NULL && bit_index != NULL &&
              bit_base != NULL) {
            /* Bound-check elem against actual set width.  For sets ≤ 4 bytes
             * that's at most 31; for truly small sets it may be less, but
             * clamping to 31 is always safe since any element in range will
             * have ordinal ≤ 31.  register-form btl wraps mod 32 so we must
             * reject out-of-domain elements explicitly. */
            int max_bit =
                (set_storage_bytes > 0) ? set_storage_bytes * 8 - 1 : 31;
            if (max_bit > 31)
              max_bit = 31; /* register form only covers 32 bits */
            char in_oob[18];
            char in_done[18];
            gen_label(in_oob, sizeof(in_oob), ctx);
            gen_label(in_done, sizeof(in_done), ctx);

            snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", bit_index);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", in_oob);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tcmpl\t$%d, %s\n", max_bit,
                     bit_index);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", in_oob);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\tbtl\t%s, %s\n", bit_index,
                     bit_base);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tsetc\t%s\n", left8);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", in_done);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "%s:\n", in_oob);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\txorb\t%s, %s\n", left8, left8);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "%s:\n", in_done);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8,
                     left32);
            inst_list = add_inst(inst_list, buffer);
          }
        } /* end small-set register-form path */
      }
      break;
    }

    if ((relop_kind == EQ || relop_kind == NE) && left_reg != NULL &&
        ((left_expr != NULL && expr_tree_is_large_set_expr(left_expr, ctx)) ||
         (right_expr != NULL &&
          expr_tree_is_large_set_expr(right_expr, ctx)))) {
      Register_t *left_addr = NULL;
      Register_t *right_addr = NULL;
      Register_t *tmp = NULL;
      StackNode_t *left_addr_spill = NULL;
      const char *left32 = left_reg->bit_32;
      const char *left8 = reg32_to_reg8(left32, left_reg);
      const char *set_instr = (relop_kind == EQ) ? "sete" : "setne";

      inst_list =
          codegen_char_set_address(left_expr, inst_list, ctx, &left_addr);
      if (left_addr == NULL)
        break;

      left_addr_spill = add_l_t("relop_set_laddr");
      if (left_addr_spill != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                 left_addr->bit_64, left_addr_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), left_addr);
        left_addr = NULL;
      }

      inst_list =
          codegen_char_set_address(right_expr, inst_list, ctx, &right_addr);
      if (right_addr == NULL) {
        if (left_addr != NULL)
          free_reg(get_reg_stack(), left_addr);
        break;
      }

      if (left_addr_spill != NULL) {
        left_addr = get_free_reg(get_reg_stack(), &inst_list);
        if (left_addr == NULL) {
          free_reg(get_reg_stack(), right_addr);
          break;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 left_addr_spill->offset, left_addr->bit_64);
        inst_list = add_inst(inst_list, buffer);
      }

      tmp = get_free_reg(get_reg_stack(), &inst_list);
      if (tmp == NULL) {
        free_reg(get_reg_stack(), left_addr);
        free_reg(get_reg_stack(), right_addr);
        break;
      }

      snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", left_reg->bit_64,
               left_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      for (int i = 0; i < 4; ++i) {
        int byte_off = i * 8;
        snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%s), %s\n", byte_off,
                 left_addr->bit_64, tmp->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\txorq\t%d(%s), %s\n", byte_off,
                 right_addr->bit_64, tmp->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\torq\t%s, %s\n", tmp->bit_64,
                 left_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
      }
      snprintf(buffer, sizeof(buffer), "\ttestq\t%s, %s\n", left_reg->bit_64,
               left_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      if (left8 != NULL && set_instr != NULL) {
        snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", set_instr, left8);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8, left32);
        inst_list = add_inst(inst_list, buffer);
      }

      free_reg(get_reg_stack(), tmp);
      free_reg(get_reg_stack(), left_addr);
      free_reg(get_reg_stack(), right_addr);
      break;
    }

    /* Pascal set subset (LE) / superset (GE) — must be done bitwise,
     * NOT as integer comparison.  For set operands:
     *   LHS <= RHS  iff  (LHS AND NOT RHS) is empty
     *   LHS >= RHS  iff  (RHS AND NOT LHS) is empty
     * Integer LE/GE on the packed bit representation gives wrong
     * answers for disjoint or overlapping sets (e.g.
     * {bit 1} <= {bit 7} is false as a subset but true as 2<=128). */
    if ((relop_kind == LE || relop_kind == GE) && left_reg != NULL &&
        left_expr != NULL && right_expr != NULL &&
        expr_tree_is_any_set_expr(left_expr, ctx) &&
        expr_tree_is_any_set_expr(right_expr, ctx)) {
      long long lsize = expr_tree_set_size_bytes(left_expr);
      long long rsize = expr_tree_set_size_bytes(right_expr);
      long long max_size = (lsize > rsize) ? lsize : rsize;
      if (max_size <= 0)
        max_size = 4;
      int either_large = (max_size > 4) ||
                         expr_tree_is_large_set_expr(left_expr, ctx) ||
                         expr_tree_is_large_set_expr(right_expr, ctx);

      const char *left32 = left_reg->bit_32;
      const char *left8 = reg32_to_reg8(left32, left_reg);

      if (either_large) {
        /* Byte-loop using addresses.  Same shape as the EQ/NE
         * large-set path above, but per byte/qword we compute
         *   (subset_side AND NOT superset_side)
         * and OR the result into the accumulator. */
        Register_t *left_addr = NULL;
        Register_t *right_addr = NULL;
        Register_t *tmp = NULL;
        StackNode_t *left_addr_spill = NULL;

        inst_list =
            codegen_char_set_address(left_expr, inst_list, ctx, &left_addr);
        if (left_addr == NULL)
          break;
        left_addr_spill = add_l_t("relop_setle_laddr");
        if (left_addr_spill != NULL) {
          snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                   left_addr->bit_64, left_addr_spill->offset);
          inst_list = add_inst(inst_list, buffer);
          free_reg(get_reg_stack(), left_addr);
          left_addr = NULL;
        }
        inst_list =
            codegen_char_set_address(right_expr, inst_list, ctx, &right_addr);
        if (right_addr == NULL) {
          if (left_addr != NULL)
            free_reg(get_reg_stack(), left_addr);
          break;
        }
        if (left_addr_spill != NULL) {
          left_addr = get_free_reg(get_reg_stack(), &inst_list);
          if (left_addr == NULL) {
            free_reg(get_reg_stack(), right_addr);
            break;
          }
          snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                   left_addr_spill->offset, left_addr->bit_64);
          inst_list = add_inst(inst_list, buffer);
        }
        tmp = get_free_reg(get_reg_stack(), &inst_list);
        if (tmp == NULL) {
          free_reg(get_reg_stack(), left_addr);
          free_reg(get_reg_stack(), right_addr);
          break;
        }

        /* For LE:  accum |= LHS[i] AND NOT RHS[i]
         * For GE:  accum |= RHS[i] AND NOT LHS[i] */
        const char *a_addr =
            (relop_kind == LE) ? left_addr->bit_64 : right_addr->bit_64;
        const char *b_addr =
            (relop_kind == LE) ? right_addr->bit_64 : left_addr->bit_64;

        snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", left_reg->bit_64,
                 left_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        /* FPC sets are at most 32 bytes (256 elements / 8). */
        int qwords = 4;
        for (int i = 0; i < qwords; ++i) {
          int byte_off = i * 8;
          snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%s), %s\n", byte_off,
                   b_addr, tmp->bit_64);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tnotq\t%s\n", tmp->bit_64);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tandq\t%d(%s), %s\n", byte_off,
                   a_addr, tmp->bit_64);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\torq\t%s, %s\n", tmp->bit_64,
                   left_reg->bit_64);
          inst_list = add_inst(inst_list, buffer);
        }
        snprintf(buffer, sizeof(buffer), "\ttestq\t%s, %s\n", left_reg->bit_64,
                 left_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        if (left8 != NULL && left32 != NULL) {
          snprintf(buffer, sizeof(buffer), "\tsete\t%s\n", left8);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8, left32);
          inst_list = add_inst(inst_list, buffer);
        }
        free_reg(get_reg_stack(), tmp);
        free_reg(get_reg_stack(), left_addr);
        free_reg(get_reg_stack(), right_addr);
        break;
      } else {
        /* Both sets fit in 32 bits.  Operate on the loaded values
         * directly.  `left` and `right` may each be a register or
         * a memory reference; movl/notl/andl handle both. */
        const char *left32_op = left32;
        const char *right32_op =
            (right_reg != NULL) ? right_reg->bit_32 : right;
        if (left32_op == NULL || right32_op == NULL || left8 == NULL)
          break;

        /* Pick scratch register avoiding both operand registers. */
        const char *scratch = "%r10d";
        int avoid_r10 = ((left_reg != NULL && left_reg->reg_id == REG_R10) ||
                         (right_reg != NULL && right_reg->reg_id == REG_R10));
        if (avoid_r10)
          scratch = "%r11d";

        /* LE: scratch = NOT RHS; scratch AND= LHS
         * GE: scratch = NOT LHS; scratch AND= RHS */
        const char *src_to_not = (relop_kind == LE) ? right32_op : left32_op;
        const char *src_to_and = (relop_kind == LE) ? left32_op : right32_op;

        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", src_to_not,
                 scratch);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tnotl\t%s\n", scratch);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tandl\t%s, %s\n", src_to_and,
                 scratch);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", scratch, scratch);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tsete\t%s\n", left8);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8, left32);
        inst_list = add_inst(inst_list, buffer);
        break;
      }
    }

    {
      /* Use ctx-aware detection: under {$H-}, local vars declared as
       * 'string' have STRING_TYPE tag but are actually ShortStrings
       * in the symbol table. */
      int left_is_shortstring =
          (left_expr != NULL &&
           expr_is_shortstring_storage_ctx(left_expr, ctx));
      int right_is_shortstring =
          (right_expr != NULL &&
           expr_is_shortstring_storage_ctx(right_expr, ctx));
      if (expr_function_call_returns_ansistring(left_expr, ctx))
        left_is_shortstring = 0;
      if (expr_function_call_returns_ansistring(right_expr, ctx))
        right_is_shortstring = 0;
      if ((left_is_shortstring || right_is_shortstring) &&
          pascal_frontend_default_shortstring()) {
        if (!left_is_shortstring && left_expr != NULL &&
            expr_has_type_tag(left_expr, STRING_TYPE))
          left_is_shortstring = 1;
        if (!right_is_shortstring && right_expr != NULL &&
            expr_has_type_tag(right_expr, STRING_TYPE))
          right_is_shortstring = 1;
      }
      int left_is_char_array =
          (left_expr != NULL && expr_is_char_array_expr(left_expr) &&
           !left_is_shortstring);
      int right_is_char_array =
          (right_expr != NULL && expr_is_char_array_expr(right_expr) &&
           !right_is_shortstring);

      int left_is_string =
          (left_expr != NULL && (expr_has_type_tag(left_expr, STRING_TYPE) ||
                                 left_is_shortstring || left_is_char_array));
      int right_is_string =
          (right_expr != NULL && (expr_has_type_tag(right_expr, STRING_TYPE) ||
                                  right_is_shortstring || right_is_char_array));
      if (left_is_char_array && left_expr != NULL &&
          left_expr->type == EXPR_ARRAY_ACCESS) {
        long long char_len = 0;
        if (expr_get_char_array_length_expr(left_expr, ctx, &char_len) &&
            char_len > 1 && char_len <= 256) {
          left_is_shortstring = 1;
          left_is_char_array = 0;
          left_is_string = 1;
        }
      }
      if (right_is_char_array && right_expr != NULL &&
          right_expr->type == EXPR_ARRAY_ACCESS) {
        long long char_len = 0;
        if (expr_get_char_array_length_expr(right_expr, ctx, &char_len) &&
            char_len > 1 && char_len <= 256) {
          right_is_shortstring = 1;
          right_is_char_array = 0;
          right_is_string = 1;
        }
      }

      if ((left_is_char_array || right_is_char_array) &&
          (left_is_string || right_is_string)) {
        long long array_len = 0;
        long long rhs_array_len = 0;
        int invert_cmp = 0;
        const char *cmp_func = "kgpc_char_array_compare";
        int compare_full = ((left_is_char_array && right_is_string) ||
                            (right_is_char_array && left_is_string));

        if (left_is_char_array && right_is_char_array) {
          if (!expr_get_char_array_length_expr(left_expr, ctx, &array_len) ||
              !expr_get_char_array_length_expr(right_expr, ctx, &rhs_array_len))
            break;
          cmp_func = "kgpc_char_array_compare_array";
        } else if (left_is_char_array) {
          if (!expr_get_char_array_length_expr(left_expr, ctx, &array_len))
            break;
        } else {
          if (!expr_get_char_array_length_expr(right_expr, ctx, &array_len))
            break;
          invert_cmp = 1;
        }

        /* Spill the other operand before shortstring promotion calls,
         * since function calls clobber caller-saved registers.
         * Use 64-bit register names for movq to avoid register width
         * mismatch when the operand string is a 32-bit register name
         * (e.g. %ebx when the value is actually a 64-bit pointer). */
        int ca_right_needs_spill =
            (right_reg != NULL) || (right != NULL && right[0] == '%');
        int ca_left_needs_spill =
            (left_reg != NULL) || (left != NULL && left[0] == '%');
        const char *right64 = operand_as_reg64(right, right_reg);
        const char *left64 = operand_as_reg64(left, left_reg);
        StackNode_t *spill_other = NULL;
        if (left_is_shortstring && ca_right_needs_spill) {
          spill_other = add_l_t("relop_rhs_preserve");
          if (spill_other != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                     right64, spill_other->offset);
            inst_list = add_inst(inst_list, buffer);
          }
        }
        if (left_is_shortstring)
          inst_list =
              promote_shortstring_reg_operand(inst_list, ctx, left, left_reg);
        if (spill_other != NULL) {
          snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                   spill_other->offset, right64);
          inst_list = add_inst(inst_list, buffer);
        }

        spill_other = NULL;
        if (right_is_shortstring && ca_left_needs_spill) {
          spill_other = add_l_t("relop_lhs_preserve");
          if (spill_other != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", left64,
                     spill_other->offset);
            inst_list = add_inst(inst_list, buffer);
          }
        }
        if (right_is_shortstring)
          inst_list =
              promote_shortstring_reg_operand(inst_list, ctx, right, right_reg);
        if (spill_other != NULL) {
          snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                   spill_other->offset, left64);
          inst_list = add_inst(inst_list, buffer);
        }

        const char *arg0 = current_arg_reg64(0);
        const char *arg1 = current_arg_reg64(1);
        const char *arg2 = current_arg_reg64(2);
        const char *arg3 = current_arg_reg64(3);
        if (arg0 == NULL || arg1 == NULL ||
            (strcmp(cmp_func, "kgpc_char_array_compare_array") == 0 &&
             (arg2 == NULL || arg3 == NULL)))
          break;

        if (strcmp(cmp_func, "kgpc_char_array_compare_array") == 0) {
          inst_list = emit_move_ptr_operand_kind(inst_list, left, left_reg,
                                                 left_kind, arg0);
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", array_len,
                   arg1);
          inst_list = add_inst(inst_list, buffer);
          inst_list = emit_move_ptr_operand_kind(inst_list, right, right_reg,
                                                 right_kind, arg2);
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", rhs_array_len,
                   arg3);
          inst_list = add_inst(inst_list, buffer);
        } else if (!invert_cmp) {
          if (compare_full)
            cmp_func = "kgpc_char_array_compare_full";
          inst_list = emit_move_ptr_operand_kind(inst_list, left, left_reg,
                                                 left_kind, arg0);
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", array_len,
                   arg1);
          inst_list = add_inst(inst_list, buffer);
          inst_list = emit_move_ptr_operand_kind(inst_list, right, right_reg,
                                                 right_kind, arg2);
        } else {
          if (compare_full)
            cmp_func = "kgpc_char_array_compare_full";
          inst_list = emit_move_ptr_operand_kind(inst_list, right, right_reg,
                                                 right_kind, arg0);
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", array_len,
                   arg1);
          inst_list = add_inst(inst_list, buffer);
          inst_list = emit_move_ptr_operand_kind(inst_list, left, left_reg,
                                                 left_kind, arg2);
        }

        inst_list = codegen_vect_reg(inst_list, 0);
        snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", cmp_func);
        inst_list = add_inst(inst_list, buffer);
        if (invert_cmp)
          inst_list = add_inst(inst_list, "\tnegl\t%eax\n");
        inst_list = add_inst(inst_list, "\tcmpl\t$0, %eax\n");
        free_arg_regs();

        const char *left32 = reg_to_reg32(left, left_reg);
        const char *left8 = reg32_to_reg8(left32, left_reg);
        const char *set_instr = NULL;
        switch (relop_kind) {
        case EQ:
          set_instr = "sete";
          break;
        case NE:
          set_instr = "setne";
          break;
        case LT:
          set_instr = "setl";
          break;
        case LE:
          set_instr = "setle";
          break;
        case GT:
          set_instr = "setg";
          break;
        case GE:
          set_instr = "setge";
          break;
        default:
          break;
        }
        if (left32 != NULL && left8 != NULL && set_instr != NULL) {
          snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", set_instr, left8);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8, left32);
          inst_list = add_inst(inst_list, buffer);
        }
        break;
      }
      if (left_is_string && right_is_string) {
        /* Promote char-typed operands to string pointers before
         * kgpc_string_compare.  Detect chars by expression type
         * (EXPR_CHAR_CODE), legacy type tag (CHAR_TYPE), or
         * resolved KgpcType (covers string-index expressions like
         * Result[L] which are char but not EXPR_CHAR_CODE).
         *
         * The operand may be in a register OR an immediate (e.g.
         * "$46" for a single-char EXPR_STRING).  Use mutable
         * copies so promote_char_operand_to_string_ex can update
         * an immediate operand to a register-backed one. */
        int left_is_char_operand =
            (!left_is_shortstring && left_expr != NULL &&
             (left_expr->type == EXPR_CHAR_CODE ||
              expr_get_type_tag(left_expr) == CHAR_TYPE ||
              (left_expr->resolved_kgpc_type != NULL &&
               kgpc_type_is_char(left_expr->resolved_kgpc_type)) ||
              codegen_expr_is_string_char_index(left_expr)));
        int right_is_char_operand =
            (!right_is_shortstring && right_expr != NULL &&
             (right_expr->type == EXPR_CHAR_CODE ||
              expr_get_type_tag(right_expr) == CHAR_TYPE ||
              (right_expr->resolved_kgpc_type != NULL &&
               kgpc_type_is_char(right_expr->resolved_kgpc_type)) ||
              codegen_expr_is_string_char_index(right_expr)));

        /* Mutable copies: promote_char_operand_to_string_ex may
         * upgrade an immediate to a register-backed operand. */
        const char *l_op = left;
        Register_t *l_reg = (Register_t *)left_reg;
        OperandKind l_kind = left_kind;
        const char *r_op = right;
        Register_t *r_reg = (Register_t *)right_reg;
        OperandKind r_kind = right_kind;

        if (left_is_shortstring)
          inst_list = promote_shortstring_operand_ex(
              inst_list, ctx, &l_op, &l_reg, &l_kind, r_op, r_reg);
        if (right_is_shortstring)
          inst_list = promote_shortstring_operand_ex(
              inst_list, ctx, &r_op, &r_reg, &r_kind, l_op, l_reg);

        if (left_is_char_operand)
          inst_list = promote_char_operand_to_string_ex(
              inst_list, &l_op, &l_reg, &l_kind, r_op, r_reg);
        if (right_is_char_operand)
          inst_list = promote_char_operand_to_string_ex(
              inst_list, &r_op, &r_reg, &r_kind, l_op, l_reg);

        const char *arg0 = current_arg_reg64(0);
        const char *arg1 = current_arg_reg64(1);
        if (arg0 == NULL || arg1 == NULL)
          break;
        inst_list =
            emit_move_ptr_operand_kind(inst_list, l_op, l_reg, l_kind, arg0);
        inst_list =
            emit_move_ptr_operand_kind(inst_list, r_op, r_reg, r_kind, arg1);
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list =
            codegen_call_with_shadow_space(inst_list, "kgpc_string_compare");
        inst_list = add_inst(inst_list, "\tcmpl\t$0, %eax\n");
        free_arg_regs();

        /* Free any registers allocated by char→string promotion. */
        if (l_reg != left_reg && l_reg != NULL)
          free_reg(get_reg_stack(), l_reg);
        if (r_reg != right_reg && r_reg != NULL)
          free_reg(get_reg_stack(), r_reg);

        const char *left32 = reg_to_reg32(left, left_reg);
        const char *left8 = reg32_to_reg8(left32, left_reg);
        const char *set_instr = NULL;
        switch (relop_kind) {
        case EQ:
          set_instr = "sete";
          break;
        case NE:
          set_instr = "setne";
          break;
        case LT:
          set_instr = "setl";
          break;
        case LE:
          set_instr = "setle";
          break;
        case GT:
          set_instr = "setg";
          break;
        case GE:
          set_instr = "setge";
          break;
        default:
          break;
        }
        if (left32 != NULL && left8 != NULL && set_instr != NULL) {
          snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", set_instr, left8);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8, left32);
          inst_list = add_inst(inst_list, buffer);
        }
        break;
      }
    }

    if (left_expr != NULL && expr_get_type_tag(left_expr) == REAL_TYPE) {
      const char *left32 = reg_to_reg32(left, left_reg);
      const char *right32 = reg_to_reg32(right, right_reg);
      if (left32 != NULL) {
        char true_label[32];
        char done_label[32];
        gen_label(true_label, sizeof(true_label), ctx);
        gen_label(done_label, sizeof(done_label), ctx);

        const char *left_candidate = (left32 != NULL) ? left32 : left;
        const char *left64 = left_candidate;
        if (left_reg != NULL) {
          const char *converted = reg32_to_reg64(left_candidate, left_reg);
          if (converted != NULL)
            left64 = converted;
        }

        StackNode_t *lhs_spill = NULL;
        if (left_reg != NULL) {
          lhs_spill = add_l_t("relop_real_lhs");
          if (lhs_spill == NULL) {
            codegen_report_error(
                ctx,
                "ERROR: Unable to allocate temporary for real comparison.");
            break;
          }

          snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", left64,
                   lhs_spill->offset);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tmovsd\t-%d(%%rbp), %%xmm1\n",
                   lhs_spill->offset);
          inst_list = add_inst(inst_list, buffer);
        }

        if (lhs_spill == NULL && left != NULL) {
          snprintf(buffer, sizeof(buffer), "\tmovsd\t%s, %%xmm1\n", left);
          inst_list = add_inst(inst_list, buffer);
        }

        int rhs_loaded = 0;
        StackNode_t *rhs_spill = NULL;
        struct Expression *raw_rhs_expr = right_expr;
        while (raw_rhs_expr != NULL && raw_rhs_expr->type == EXPR_TYPECAST &&
               raw_rhs_expr->expr_data.typecast_data.target_type == REAL_TYPE &&
               raw_rhs_expr->expr_data.typecast_data.expr != NULL) {
          raw_rhs_expr = raw_rhs_expr->expr_data.typecast_data.expr;
        }
        int rhs_tag =
            (raw_rhs_expr != NULL)
                ? expr_get_type_tag(raw_rhs_expr)
                : ((right_expr != NULL) ? expr_get_type_tag(right_expr)
                                        : UNKNOWN_TYPE);
        int rhs_is_integer_like = is_integer_type(rhs_tag) || rhs_tag == BOOL ||
                                  rhs_tag == CHAR_TYPE || rhs_tag == ENUM_TYPE;
        if (right != NULL && right[0] == '$') {
          if (rhs_is_integer_like) {
            Register_t *imm_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (imm_reg == NULL) {
              codegen_report_error(ctx, "ERROR: Unable to allocate register "
                                        "for real comparison immediate.");
              break;
            }
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", right,
                     imm_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tcvtsi2sdq\t%s, %%xmm0\n",
                     imm_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), imm_reg);
          } else {
            char label[32];
            snprintf(label, sizeof(label), ".LC%d", ctx->write_label_counter++);

            const char *readonly_section = codegen_readonly_section_directive();
            char rodata_buffer[192];
            snprintf(rodata_buffer, sizeof(rodata_buffer),
                     "%s\n%s:\n\t.quad %s\n%s\n", readonly_section, label,
                     right + 1, codegen_text_section_resume());
            inst_list = add_inst(inst_list, rodata_buffer);

            snprintf(buffer, sizeof(buffer), "\tmovsd\t%s(%%rip), %%xmm0\n",
                     label);
            inst_list = add_inst(inst_list, buffer);
          }
          rhs_loaded = 1;
        }

        if (!rhs_loaded) {
          if (rhs_is_integer_like && right != NULL) {
            if (codegen_type_uses_qword(rhs_tag)) {
              snprintf(buffer, sizeof(buffer), "\tcvtsi2sdq\t%s, %%xmm0\n",
                       right);
              inst_list = add_inst(inst_list, buffer);
            } else {
              snprintf(buffer, sizeof(buffer), "\tcvtsi2sdl\t%s, %%xmm0\n",
                       right);
              inst_list = add_inst(inst_list, buffer);
            }
            rhs_loaded = 1;
          }
          if (!rhs_loaded && right_reg != NULL) {
            if (rhs_is_integer_like) {
              const char *rhs32 = right32 != NULL ? right32 : right;
              const char *rhs64 = reg32_to_reg64(rhs32, right_reg);
              if (rhs64 == NULL)
                rhs64 = rhs32;
              if (codegen_type_uses_qword(rhs_tag)) {
                snprintf(buffer, sizeof(buffer), "\tcvtsi2sdq\t%s, %%xmm0\n",
                         rhs64);
                inst_list = add_inst(inst_list, buffer);
              } else {
                snprintf(buffer, sizeof(buffer), "\tcvtsi2sdl\t%s, %%xmm0\n",
                         rhs32);
                inst_list = add_inst(inst_list, buffer);
              }
              rhs_loaded = 1;
            } else {
              const char *right_candidate = right;
              if (right32 != NULL)
                right_candidate = right32;
              const char *right64 = right_candidate;
              const char *converted =
                  reg32_to_reg64(right_candidate, right_reg);
              if (converted != NULL)
                right64 = converted;
              if (right_reg != NULL) {
                if (rhs_spill == NULL)
                  rhs_spill = add_l_t("relop_real_rhs_reg");
                if (rhs_spill == NULL) {
                  codegen_report_error(ctx, "ERROR: Unable to allocate "
                                            "temporary for real comparison.");
                  break;
                }

                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                         right64, rhs_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer),
                         "\tmovsd\t-%d(%%rbp), %%xmm0\n", rhs_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                rhs_loaded = 1;
              }
            }
          }
          if (!rhs_loaded && right != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovsd\t%s, %%xmm0\n", right);
            inst_list = add_inst(inst_list, buffer);
          }
        }

        snprintf(buffer, sizeof(buffer), "\txorl\t%s, %s\n", left32, left32);
        inst_list = add_inst(inst_list, buffer);
        inst_list = add_inst(inst_list, "\tucomisd\t%xmm0, %xmm1\n");

        switch (relop_kind) {
        case EQ:
          snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tje\t%s\n", true_label);
          inst_list = add_inst(inst_list, buffer);
          break;
        case NE:
          snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", true_label);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tjne\t%s\n", true_label);
          inst_list = add_inst(inst_list, buffer);
          break;
        case LT:
          snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tjb\t%s\n", true_label);
          inst_list = add_inst(inst_list, buffer);
          break;
        case LE:
          snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tjbe\t%s\n", true_label);
          inst_list = add_inst(inst_list, buffer);
          break;
        case GT:
          snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tja\t%s\n", true_label);
          inst_list = add_inst(inst_list, buffer);
          break;
        case GE:
          snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tjae\t%s\n", true_label);
          inst_list = add_inst(inst_list, buffer);
          break;
        default:
          break;
        }

        snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", done_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "%s:\n", true_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", left32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "%s:\n", done_label);
        inst_list = add_inst(inst_list, buffer);
      }
      break;
    }

    {
      int left_type =
          (left_expr != NULL) ? expr_get_type_tag(left_expr) : UNKNOWN_TYPE;
      int right_type =
          (right_expr != NULL) ? expr_get_type_tag(right_expr) : UNKNOWN_TYPE;
      int left_is_tconstexprint = expr_tree_is_tconstexprint_payload(left_expr);
      int right_is_tconstexprint =
          expr_tree_is_tconstexprint_payload(right_expr);
      int use_qword = codegen_type_uses_qword(left_type) ||
                      codegen_type_uses_qword(right_type) ||
                      left_is_tconstexprint || right_is_tconstexprint;
      char cmp_suffix = use_qword ? 'q' : 'l';

      const char *cmp_left = left;
      const char *cmp_right = right;
      Register_t *imm_reg = NULL;
      Register_t *left_mem_tmp = NULL;
      Register_t *right_mem_tmp = NULL;
      if (left_is_tconstexprint && left_reg != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t8(%s), %s\n",
                 left_reg->bit_64, left_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        cmp_left = left_reg->bit_64;
      }
      if (right_is_tconstexprint && right_reg != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t8(%s), %s\n",
                 right_reg->bit_64, right_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        cmp_right = right_reg->bit_64;
      }
      if (use_qword) {
        const char *left_candidate = left;
        const char *left32 = reg_to_reg32(left, left_reg);
        if (left32 != NULL)
          left_candidate = left32;
        const char *left64 = reg32_to_reg64(left_candidate, left_reg);
        if (left64 != NULL)
          cmp_left = left64;

        const char *right_candidate = right;
        const char *right32 = reg_to_reg32(right, right_reg);
        if (right32 != NULL)
          right_candidate = right32;
        const char *right64 = reg32_to_reg64(right_candidate, right_reg);
        if (right64 != NULL)
          cmp_right = right64;

        /* Sign/zero-extend 32-bit operands for 64-bit comparison.
         * When one side is qword (e.g. SizeInt/Int64) and the other
         * was computed as 32-bit (e.g. Integer literal), the upper
         * 32 bits of the register may not match the intended value
         * (e.g. negl produces 32-bit -1 = 0x00000000FFFFFFFF instead
         * of 64-bit -1 = 0xFFFFFFFFFFFFFFFF). */
        if (left_reg != NULL && !codegen_type_uses_qword(left_type) &&
            !(left_expr != NULL && expr_uses_qword_kgpctype(left_expr))) {
          if (codegen_type_is_signed(left_type))
            inst_list = codegen_sign_extend32_to64(inst_list, left_reg->bit_32,
                                                   left_reg->bit_64);
          else
            inst_list = codegen_zero_extend32_to64(inst_list, left_reg->bit_32,
                                                   left_reg->bit_32);
        }
        if (right_reg != NULL && !codegen_type_uses_qword(right_type) &&
            !(right_expr != NULL && expr_uses_qword_kgpctype(right_expr))) {
          if (codegen_type_is_signed(right_type))
            inst_list = codegen_sign_extend32_to64(inst_list, right_reg->bit_32,
                                                   right_reg->bit_64);
          else
            inst_list = codegen_zero_extend32_to64(inst_list, right_reg->bit_32,
                                                   right_reg->bit_32);
        }

        /* When a 32-bit operand is a memory reference (no register),
         * cmpq would read 8 bytes from a 4-byte stack slot, getting
         * garbage in the upper 32 bits.  Load into a temp register
         * and sign/zero-extend before the comparison. */
        if (left_reg == NULL && cmp_left != NULL && cmp_left[0] != '$' &&
            cmp_left[0] != '%' && !codegen_type_uses_qword(left_type) &&
            !(left_expr != NULL && expr_uses_qword_kgpctype(left_expr)) &&
            is_integer_type(left_type)) {
          left_mem_tmp = get_free_reg(get_reg_stack(), &inst_list);
          if (left_mem_tmp != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", cmp_left,
                     left_mem_tmp->bit_32);
            inst_list = add_inst(inst_list, buffer);
            if (codegen_type_is_signed(left_type))
              inst_list = codegen_sign_extend32_to64(
                  inst_list, left_mem_tmp->bit_32, left_mem_tmp->bit_64);
            cmp_left = left_mem_tmp->bit_64;
          }
        }
        if (right_reg == NULL && cmp_right != NULL && cmp_right[0] != '$' &&
            cmp_right[0] != '%' && !codegen_type_uses_qword(right_type) &&
            !(right_expr != NULL && expr_uses_qword_kgpctype(right_expr)) &&
            is_integer_type(right_type)) {
          right_mem_tmp = get_free_reg(get_reg_stack(), &inst_list);
          if (right_mem_tmp != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", cmp_right,
                     right_mem_tmp->bit_32);
            inst_list = add_inst(inst_list, buffer);
            if (codegen_type_is_signed(right_type))
              inst_list = codegen_sign_extend32_to64(
                  inst_list, right_mem_tmp->bit_32, right_mem_tmp->bit_64);
            cmp_right = right_mem_tmp->bit_64;
          }
        }
      } else {
        const char *left32 = reg_to_reg32(left, left_reg);
        if (left32 != NULL)
          cmp_left = left32;
        const char *right32 = reg_to_reg32(right, right_reg);
        if (right32 != NULL)
          cmp_right = right32;
      }

      if (left_is_tconstexprint && left_reg != NULL)
        cmp_left = left_reg->bit_64;
      if (right_is_tconstexprint && right_reg != NULL)
        cmp_right = right_reg->bit_64;

      if (use_qword && cmp_right != NULL && cmp_right[0] == '$') {
        char *endptr = NULL;
        long long imm_value = strtoll(cmp_right + 1, &endptr, 0);
        if (endptr != NULL && *endptr == '\0' &&
            (imm_value > 2147483647LL || imm_value < -2147483648LL)) {
          imm_reg = get_free_reg(get_reg_stack(), &inst_list);
          if (imm_reg == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate temporary for "
                                      "64-bit immediate comparison.");
            break;
          }
          snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", imm_value,
                   imm_reg->bit_64);
          inst_list = add_inst(inst_list, buffer);
          cmp_right = imm_reg->bit_64;
        }
      }

      snprintf(buffer, sizeof(buffer), "\tcmp%c\t%s, %s\n", cmp_suffix,
               cmp_right, cmp_left);
      inst_list = add_inst(inst_list, buffer);
      if (imm_reg != NULL)
        free_reg(get_reg_stack(), imm_reg);
      if (left_mem_tmp != NULL)
        free_reg(get_reg_stack(), left_mem_tmp);
      if (right_mem_tmp != NULL)
        free_reg(get_reg_stack(), right_mem_tmp);

      const char *set_instr = NULL;
      /* Use unsigned comparison instructions when either operand
       * is an unsigned type (e.g. LongWord, Cardinal, DWord).
       * EQ/NE are the same for signed and unsigned. */
      int use_unsigned_cmp = 0;
      if (left_expr != NULL && !codegen_expr_is_signed(left_expr))
        use_unsigned_cmp = 1;
      if (right_expr != NULL && !codegen_expr_is_signed(right_expr))
        use_unsigned_cmp = 1;
      switch (relop_kind) {
      case EQ:
        set_instr = "sete";
        break;
      case NE:
        set_instr = "setne";
        break;
      case LT:
        set_instr = use_unsigned_cmp ? "setb" : "setl";
        break;
      case LE:
        set_instr = use_unsigned_cmp ? "setbe" : "setle";
        break;
      case GT:
        set_instr = use_unsigned_cmp ? "seta" : "setg";
        break;
      case GE:
        set_instr = use_unsigned_cmp ? "setae" : "setge";
        break;
      default:
        break;
      }

      if (set_instr != NULL) {
        const char *left32 = reg_to_reg32(left, left_reg);
        const char *left8 = reg32_to_reg8(left32, left_reg);
        if (left32 != NULL && left8 != NULL) {
          snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", set_instr, left8);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", left8, left32);
          inst_list = add_inst(inst_list, buffer);
        }
      }
    }

    break;
  }

  default:
    assert(0 && "Unsupported expr type in gencode!");
    break;
  }

  return inst_list;
}

/* Gets simple operation of a node */
/* DEPRECATED */
ListNode_t *gencode_op_deprecated(struct Expression *expr,
                                  ListNode_t *inst_list, char *buffer,
                                  int buf_len, CodeGenContext *ctx) {
  (void)ctx;
  assert(expr != NULL);
  int type;

  switch (expr->type) {
  case EXPR_ADDOP:
    type = expr->expr_data.addop_data.addop_type;
    if (type == PLUS)
      snprintf(buffer, buf_len, "addl");
    else if (type == MINUS)
      snprintf(buffer, buf_len, "subl");
    else {
      assert(0 && "Bad addop type!");
    }

    break;

  case EXPR_MULOP:
    type = expr->expr_data.addop_data.addop_type;
    if (type == STAR)
      snprintf(buffer, buf_len, "imull");
    else {
      assert(0 && "Bad mulop type!");
    }

    break;

  default:
    assert(0 && "Unsupported expr type in gencode!");
    break;
  }

  return inst_list;
}
