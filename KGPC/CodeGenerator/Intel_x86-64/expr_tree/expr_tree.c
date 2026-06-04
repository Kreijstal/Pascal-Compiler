/*
    Damon Gwinn
    Tree of simple expressions for the gencode algorithm
    TODO: Does not handle real numbers
*/

#include "expr_tree.h"
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
#include "expr_tree_internal.h"
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int codegen_array_access_targets_shortstring(const struct Expression *expr,
                                             CodeGenContext *ctx);
int expr_tree_is_tconstexprint_payload(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if (expr->type == EXPR_RECORD_ACCESS) {
    if (expr->expr_data.record_access_data.field_id != NULL &&
        pascal_identifier_equals(expr->expr_data.record_access_data.field_id,
                                 "valueord"))
      return 1;
    return expr_tree_is_tconstexprint_payload(
        expr->expr_data.record_access_data.record_expr);
  }
  return 0;
}

static ListNode_t *codegen_spill_call_arg_regs_expr(ListNode_t *inst_list,
                                                    int *int_offsets,
                                                    int *xmm_offsets) {
  char buffer[128];
  for (int i = 0; i < kgpc_max_int_arg_regs(); i++) {
    const char *reg = current_arg_reg64(i);
    StackNode_t *slot = add_l_t_bytes("__intf_expr_arg_int", 8);
    int_offsets[i] = slot != NULL ? slot->offset : 0;
    if (slot != NULL && reg != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", reg,
               slot->offset);
      inst_list = add_inst(inst_list, buffer);
    }
  }
  for (int i = 0; i < kgpc_max_sse_arg_regs(); i++) {
    const char *reg = current_arg_reg_xmm(i);
    StackNode_t *slot = add_l_t_bytes("__intf_expr_arg_xmm", 16);
    xmm_offsets[i] = slot != NULL ? slot->offset : 0;
    if (slot != NULL && reg != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovdqu\t%s, -%d(%%rbp)\n", reg,
               slot->offset);
      inst_list = add_inst(inst_list, buffer);
    }
  }
  return inst_list;
}

static ListNode_t *codegen_restore_call_arg_regs_expr(ListNode_t *inst_list,
                                                      const int *int_offsets,
                                                      const int *xmm_offsets) {
  char buffer[128];
  for (int i = 0; i < kgpc_max_int_arg_regs(); i++) {
    const char *reg = current_arg_reg64(i);
    if (reg != NULL && int_offsets[i] > 0) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
               int_offsets[i], reg);
      inst_list = add_inst(inst_list, buffer);
    }
  }
  for (int i = 0; i < kgpc_max_sse_arg_regs(); i++) {
    const char *reg = current_arg_reg_xmm(i);
    if (reg != NULL && xmm_offsets[i] > 0) {
      snprintf(buffer, sizeof(buffer), "\tmovdqu\t-%d(%%rbp), %s\n",
               xmm_offsets[i], reg);
      inst_list = add_inst(inst_list, buffer);
    }
  }
  return inst_list;
}

static int expr_tree_tag_from_kgpc(const KgpcType *type) {
  if (type == NULL)
    return UNKNOWN_TYPE;
  if (type->kind == TYPE_KIND_PRIMITIVE)
    return type->info.primitive_type_tag;
  if (kgpc_type_is_array_of_const((KgpcType *)type))
    return ARRAY_OF_CONST_TYPE;
  if (kgpc_type_is_array((KgpcType *)type) && type->type_alias != NULL &&
      type->type_alias->is_shortstring)
    return SHORTSTRING_TYPE;
  if (kgpc_type_is_record((KgpcType *)type))
    return RECORD_TYPE;
  if (kgpc_type_is_pointer((KgpcType *)type))
    return POINTER_TYPE;
  if (kgpc_type_is_procedure((KgpcType *)type))
    return PROCEDURE;
  return UNKNOWN_TYPE;
}

int expr_tree_symbol_matches_expr_type(const HashNode_t *node,
                                       const struct Expression *expr) {
  if (node == NULL || expr == NULL)
    return 1;

  KgpcType *expr_type = expr->resolved_kgpc_type;
  KgpcType *node_type = node->type;
  int expr_tag = UNKNOWN_TYPE;
  int node_tag = UNKNOWN_TYPE;

  if (expr_type != NULL && node_type != NULL) {
    if (kgpc_type_is_pointer(expr_type))
      return kgpc_type_is_pointer(node_type);
    if (kgpc_type_is_record(expr_type))
      return kgpc_type_is_record(node_type) || kgpc_type_is_pointer(node_type);
    if (kgpc_type_is_procedure(expr_type))
      return kgpc_type_is_procedure(node_type);
    if (kgpc_type_is_array(expr_type))
      return kgpc_type_is_array(node_type);

    expr_tag = expr_tree_tag_from_kgpc(expr_type);
    node_tag = expr_tree_tag_from_kgpc(node_type);
  } else {
    expr_tag = expr_get_type_tag(expr);
    if (node_type != NULL)
      node_tag = expr_tree_tag_from_kgpc(node_type);
  }

  if (expr_tag == UNKNOWN_TYPE || node_tag == UNKNOWN_TYPE)
    return 1;

  return expr_tag == node_tag;
}

static struct RecordType *expr_tree_lookup_record_type(SymTab_t *symtab,
                                                       const char *type_name) {
  if (symtab == NULL || type_name == NULL)
    return NULL;

  struct RecordType *record = semcheck_lookup_record_type(symtab, type_name);
  if (record != NULL)
    return record;

  HashNode_t *node = NULL;
  if (FindSymbol(&node, symtab, type_name) != 0 && node != NULL)
    return hashnode_get_record_type(node);
  return NULL;
}

static const char *expr_tree_first_ast_symbol_name(const ast_t *node) {
  if (node == NULL)
    return NULL;
  if (node->sym != NULL && node->sym->name != NULL)
    return node->sym->name;
  const char *child_name = expr_tree_first_ast_symbol_name(node->child);
  if (child_name != NULL)
    return child_name;
  return expr_tree_first_ast_symbol_name(node->next);
}

static int expr_tree_method_template_returns_shortstring(
    CodeGenContext *ctx, const struct MethodTemplate *tmpl) {
  if (tmpl == NULL || tmpl->kind != METHOD_TEMPLATE_FUNCTION)
    return 0;
  if (tmpl->method_tree != NULL &&
      tmpl->method_tree->tree_data.subprogram_data.return_type ==
          SHORTSTRING_TYPE) {
    return 1;
  }
  if (ctx != NULL && tmpl->return_type_ast != NULL) {
    KgpcType *ret_type =
        convert_type_spec_to_kgpctype(tmpl->return_type_ast, ctx->symtab);
    if (ret_type != NULL && (kgpc_type_is_shortstring(ret_type) ||
                             (ret_type->type_alias != NULL &&
                              ret_type->type_alias->is_shortstring))) {
      return 1;
    }
  }
  const char *ret_name = expr_tree_first_ast_symbol_name(tmpl->return_type_ast);
  if (ret_name != NULL) {
    if (pascal_identifier_equals(ret_name, "ShortString"))
      return 1;
    if (tmpl->default_shortstring &&
        pascal_identifier_equals(ret_name, "String"))
      return 1;
  }
  return 0;
}

static int
expr_tree_virtual_call_returns_shortstring(CodeGenContext *ctx,
                                           const struct Expression *expr) {
  if (ctx == NULL || expr == NULL || expr->type != EXPR_FUNCTION_CALL ||
      !expr->expr_data.function_call_data.is_virtual_call) {
    return 0;
  }

  const char *owner_name = expr->expr_data.function_call_data.self_class_name;
  if (owner_name == NULL)
    owner_name = expr->expr_data.function_call_data.cached_owner_class;
  const char *method_name =
      expr->expr_data.function_call_data.cached_method_name;
  if (method_name == NULL)
    method_name = expr->expr_data.function_call_data.id;

  struct RecordType *record =
      expr_tree_lookup_record_type(ctx->symtab, owner_name);
  if (record == NULL || method_name == NULL)
    return 0;

  for (ListNode_t *cur = record->method_templates; cur != NULL;
       cur = cur->next) {
    struct MethodTemplate *tmpl = (struct MethodTemplate *)cur->cur;
    if (tmpl != NULL && tmpl->name != NULL &&
        pascal_identifier_equals(tmpl->name, method_name) &&
        expr_tree_method_template_returns_shortstring(ctx, tmpl)) {
      return 1;
    }
  }
  return 0;
}

static int
expr_tree_constructor_owner_is_plain_object(CodeGenContext *ctx,
                                            const struct Expression *expr) {
  if (ctx == NULL || expr == NULL || expr->type != EXPR_FUNCTION_CALL)
    return 0;

  const char *owner_name = expr->expr_data.function_call_data.self_class_name;
  if (owner_name == NULL)
    owner_name = expr->expr_data.function_call_data.cached_owner_class;
  if (owner_name != NULL) {
    struct RecordType *owner_record =
        expr_tree_lookup_record_type(ctx->symtab, owner_name);
    if (owner_record != NULL)
      return !record_type_is_class(owner_record);
  }

  struct Expression *receiver =
      expr->expr_data.function_call_data.constructor_receiver_expr;
  if (receiver == NULL &&
      expr->expr_data.function_call_data.args_expr != NULL) {
    receiver =
        (struct Expression *)expr->expr_data.function_call_data.args_expr->cur;
  }

  KgpcType *receiver_type = expr_get_kgpc_type(receiver);
  if (receiver_type != NULL && kgpc_type_is_pointer(receiver_type) &&
      receiver_type->info.points_to != NULL)
    receiver_type = receiver_type->info.points_to;

  if (receiver_type != NULL && kgpc_type_is_record(receiver_type) &&
      receiver_type->info.record_info != NULL)
    return !record_type_is_class(receiver_type->info.record_info);

  return 0;
}

static int expr_tree_symbol_preference_score(const HashNode_t *node,
                                             const struct Expression *expr,
                                             const CodeGenContext *ctx) {
  if (node == NULL)
    return INT_MIN;
  if (!expr_tree_symbol_matches_expr_type(node, expr))
    return INT_MIN / 2;

  int score = 0;
  switch (node->hash_type) {
  case HASHTYPE_VAR:
  case HASHTYPE_ARRAY:
    score = 400;
    break;
  case HASHTYPE_CONST:
    score = 300;
    break;
  case HASHTYPE_FUNCTION:
  case HASHTYPE_PROCEDURE:
    score = 200;
    break;
  case HASHTYPE_TYPE:
    score = 100;
    break;
  default:
    score = 0;
    break;
  }

  if (ctx != NULL && ctx->symtab != NULL &&
      node->source_unit_index == ctx->symtab->current_unit_index)
    score += 25;

  if (expr != NULL && expr->resolved_kgpc_type != NULL && node->type != NULL) {
    if (expr->resolved_kgpc_type == node->type)
      score += 100;
    else {
      struct TypeAlias *expr_alias =
          kgpc_type_get_type_alias(expr->resolved_kgpc_type);
      struct TypeAlias *node_alias = kgpc_type_get_type_alias(node->type);
      if (expr_alias != NULL && node_alias != NULL) {
        if (expr_alias == node_alias)
          score += 100;
        else if (expr_alias->is_enum && node_alias->is_enum)
          score -= 50;
      }
    }
  }

  return score;
}

HashNode_t *expr_tree_find_preferred_symbol(CodeGenContext *ctx,
                                            const struct Expression *expr) {
  if (ctx == NULL || ctx->symtab == NULL || expr == NULL ||
      expr->type != EXPR_VAR_ID || expr->expr_data.id == NULL)
    return NULL;

  HashNode_t *best = NULL;
  int best_score = INT_MIN;
  ListNode_t *candidates = FindAllIdents(ctx->symtab, expr->expr_data.id);
  for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next) {
    HashNode_t *candidate = (HashNode_t *)cur->cur;
    int score = expr_tree_symbol_preference_score(candidate, expr, ctx);
    if (score > best_score) {
      best = candidate;
      best_score = score;
    }
  }
  if (candidates != NULL)
    DestroyList(candidates);

  return best;
}

static void codegen_typeinfo_label_for_type_id(SymTab_t *symtab,
                                               const char *type_id,
                                               char *buffer, size_t size) {
  codegen_common_typeinfo_label_for_type_id(symtab, type_id, buffer, size);
}
#include "../../../Parser/ParseTree/type_tags.h"
#include "../../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../../Parser/SemanticCheck/NameMangling.h"
#include "../../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../codegen_statement.h"

static int expr_tree_type_is_class_vmt_value(const KgpcType *type) {
  if (type == NULL)
    return 0;

  if (type->type_alias != NULL && type->type_alias->is_class_reference)
    return 1;

  if (type->kind == TYPE_KIND_POINTER && type->info.points_to != NULL) {
    if (type->info.points_to->type_alias != NULL &&
        type->info.points_to->type_alias->is_class_reference) {
      return 1;
    }
  }

  return 0;
}

static struct RecordType *
expr_tree_class_record_from_class_vmt_type(const KgpcType *type) {
  const KgpcType *cur = type;

  if (cur == NULL || !expr_tree_type_is_class_vmt_value(cur))
    return NULL;

  if (cur->kind == TYPE_KIND_POINTER)
    cur = cur->info.points_to;
  if (cur != NULL && cur->kind == TYPE_KIND_POINTER)
    cur = cur->info.points_to;
  if (cur != NULL && cur->kind == TYPE_KIND_RECORD)
    return cur->info.record_info;

  return NULL;
}

static int
expr_tree_expr_is_class_vmt_value(const struct Expression *expr,
                                  CodeGenContext *ctx,
                                  struct RecordType **class_record_out) {
  if (class_record_out != NULL)
    *class_record_out = NULL;
  if (expr == NULL)
    return 0;

  KgpcType *type = expr->resolved_kgpc_type;
  if (type == NULL && ctx != NULL && ctx->symtab != NULL &&
      expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL) {
    HashNode_t *node = expr_tree_find_preferred_symbol(ctx, expr);
    if (node != NULL)
      type = node->type;
  }

  if (!expr_tree_type_is_class_vmt_value(type))
    return 0;

  if (class_record_out != NULL)
    *class_record_out = expr_tree_class_record_from_class_vmt_type(type);
  return 1;
}

static int expr_tree_first_arg_is_class_vmt_value(const struct Expression *expr,
                                                  CodeGenContext *ctx) {
  if (expr == NULL || ctx == NULL || ctx->symtab == NULL ||
      expr->type != EXPR_FUNCTION_CALL ||
      expr->expr_data.function_call_data.args_expr == NULL ||
      expr->expr_data.function_call_data.args_expr->cur == NULL) {
    return 0;
  }

  struct Expression *self_expr =
      (struct Expression *)expr->expr_data.function_call_data.args_expr->cur;
  if (self_expr == NULL)
    return 0;

  if (expr_tree_type_is_class_vmt_value(self_expr->resolved_kgpc_type))
    return 1;

  if (self_expr->type == EXPR_VAR_ID && self_expr->expr_data.id != NULL) {
    if (ctx->current_subprogram_is_nonstatic_class_method &&
        pascal_identifier_equals(self_expr->expr_data.id, "Self")) {
      return 1;
    }

    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, self_expr->expr_data.id) != 0 &&
        node != NULL && expr_tree_type_is_class_vmt_value(node->type)) {
      return 1;
    }
  }

  return 0;
}

/* Cached getenv() — defined in SemCheck.c */
extern const char *kgpc_getenv(const char *name);
#ifndef CODEGEN_POINTER_SIZE_BYTES
#define CODEGEN_POINTER_SIZE_BYTES 8
#endif

static unsigned long codegen_newfunc_temp_counter(void) {
  static unsigned long counter = 0;
  return ++counter;
}

static ListNode_t *codegen_builtin_new_function_call(struct Expression *expr,
                                                     ListNode_t *inst_list,
                                                     CodeGenContext *ctx,
                                                     Register_t *target_reg) {
  if (expr == NULL || ctx == NULL || ctx->symtab == NULL || target_reg == NULL)
    return NULL;

  ListNode_t *args = expr->expr_data.function_call_data.args_expr;
  if (args == NULL || args->cur == NULL)
    return NULL;

  struct Expression *type_arg = (struct Expression *)args->cur;
  struct Expression *method_expr =
      (args->next != NULL) ? (struct Expression *)args->next->cur : NULL;
  KgpcType *ptr_type = type_arg != NULL ? type_arg->resolved_kgpc_type : NULL;

  if (ptr_type == NULL && type_arg != NULL) {
    const char *type_name = NULL;
    if (type_arg->type == EXPR_VAR_ID)
      type_name = type_arg->expr_data.id;
    else if (type_arg->type == EXPR_FUNCTION_CALL)
      type_name = type_arg->expr_data.function_call_data.id;

    if (type_name != NULL) {
      HashNode_t *type_node =
          semcheck_find_preferred_type_node(ctx->symtab, type_name);
      if (type_node != NULL)
        ptr_type = type_node->type;
    }
  }

  if (ptr_type == NULL || !kgpc_type_is_pointer(ptr_type) ||
      ptr_type->info.points_to == NULL)
    return NULL;

  long long alloc_size = kgpc_type_sizeof(ptr_type->info.points_to);
  if (alloc_size <= 0) {
    codegen_report_error(
        ctx, "ERROR: Unable to determine size for New function target.");
    return inst_list;
  }

  char temp_name[64];
  snprintf(temp_name, sizeof(temp_name), "__newfunc_ptr_%lu",
           codegen_newfunc_temp_counter());
  StackNode_t *temp_slot = add_l_t_bytes(temp_name, 8);
  if (temp_slot == NULL) {
    codegen_report_error(
        ctx, "ERROR: Unable to allocate temporary for New function result.");
    return inst_list;
  }

  PushVarOntoScope_Typed(ctx->symtab, temp_name, ptr_type);

  struct Expression *target_expr = mk_varid(expr->line_num, strdup(temp_name));
  if (target_expr == NULL) {
    codegen_report_error(
        ctx, "ERROR: Unable to create temporary target for New function.");
    return inst_list;
  }
  ListNode_t *stmt_args = CreateListNode(target_expr, LIST_EXPR);
  if (stmt_args == NULL) {
    destroy_expr(target_expr);
    codegen_report_error(
        ctx, "ERROR: Unable to allocate argument list for New function.");
    return inst_list;
  }
  if (method_expr != NULL) {
    struct Expression *method_clone = clone_expression(method_expr);
    ListNode_t *method_node =
        method_clone != NULL ? CreateListNode(method_clone, LIST_EXPR) : NULL;
    if (method_node == NULL) {
      if (method_clone != NULL)
        destroy_expr(method_clone);
      DestroyList(stmt_args);
      codegen_report_error(
          ctx, "ERROR: Unable to clone constructor for New function.");
      return inst_list;
    }
    stmt_args->next = method_node;
  }

  struct Statement *new_stmt =
      mk_procedurecall(expr->line_num, strdup("New"), stmt_args);
  if (new_stmt == NULL) {
    DestroyList(stmt_args);
    codegen_report_error(
        ctx, "ERROR: Unable to create New function lowering statement.");
    return inst_list;
  }

  if (semcheck_stmt(ctx->symtab, new_stmt, INT_MAX) != 0) {
    destroy_stmt(new_stmt);
    codegen_report_error(ctx,
                         "ERROR: Failed to semcheck New function lowering.");
    return inst_list;
  }

  inst_list = codegen_stmt(new_stmt, inst_list, ctx, ctx->symtab);
  destroy_stmt(new_stmt);

  if (!codegen_had_error(ctx)) {
    char buffer[96];
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
             temp_slot->offset, target_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
  }

  return inst_list;
}

static int expr_tree_node_is_wide_string(expr_node_t *node) {
  if (node == NULL || node->expr == NULL)
    return 0;

  if (node->expr->type == EXPR_ADDOP &&
      node->expr->expr_data.addop_data.addop_type == PLUS &&
      expr_get_type_tag(node->expr) == STRING_TYPE) {
    return expr_tree_node_is_wide_string(node->left_expr) ||
           expr_tree_node_is_wide_string(node->right_expr);
  }

  if (node->expr->resolved_kgpc_type != NULL) {
    if (kgpc_type_is_wide_string(node->expr->resolved_kgpc_type))
      return 1;

    if (node->expr->resolved_kgpc_type->type_alias != NULL) {
      const char *alias_name =
          node->expr->resolved_kgpc_type->type_alias->alias_name;
      const char *target_name =
          node->expr->resolved_kgpc_type->type_alias->target_type_id;
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

  if (node->expr->type == EXPR_FUNCTION_CALL &&
      node->expr->expr_data.function_call_data.call_kgpc_type != NULL &&
      node->expr->expr_data.function_call_data.call_kgpc_type->kind ==
          TYPE_KIND_PROCEDURE) {
    KgpcType *call_type =
        node->expr->expr_data.function_call_data.call_kgpc_type;
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

  return 0;
}

static ListNode_t *codegen_builtin_dynarray_length(struct Expression *expr,
                                                   ListNode_t *inst_list,
                                                   CodeGenContext *ctx,
                                                   Register_t *target_reg) {
  if (expr == NULL || ctx == NULL || target_reg == NULL)
    return inst_list;

  ListNode_t *args = expr->expr_data.function_call_data.args_expr;
  if (args == NULL || args->next != NULL) {
    codegen_report_error(ctx, "ERROR: Length intrinsic expects one argument.");
    return inst_list;
  }

  struct Expression *array_expr = (struct Expression *)args->cur;
  if (array_expr == NULL)
    return inst_list;

  int use_value = 0;
  if (array_expr->type == EXPR_VAR_ID && ctx->symtab != NULL) {
    StackNode_t *stack_node = find_label(array_expr->expr_data.id);
    if (stack_node != NULL && stack_node->is_reference)
      use_value = 1;

    HashNode_t *node = NULL;
    if (!use_value &&
        FindSymbol(&node, ctx->symtab, array_expr->expr_data.id) != 0 &&
        node != NULL && node->is_var_parameter) {
      use_value = 1;
    }
  }

  Register_t *desc_reg = NULL;
  int desc_is_heap_temp = 0;
  if (!use_value && codegen_expr_is_addressable(array_expr))
    inst_list = codegen_address_for_expr(array_expr, inst_list, ctx, &desc_reg);
  else {
    inst_list = codegen_expr_with_result(array_expr, inst_list, ctx, &desc_reg);
    /* A non-var-param, non-SRET function-call result returning a dynamic
     * array hands us a descriptor block malloc'd by
     * kgpc_dynarray_clone_descriptor in the callee's epilogue.  We are
     * the sole consumer of that temp, so we must release it after
     * reading the length to prevent the descriptor (and its orphaned
     * data buffer) from leaking. */
    if (!use_value && array_expr->type == EXPR_FUNCTION_CALL &&
        !expr_returns_sret(array_expr))
      desc_is_heap_temp = 1;
  }

  if (codegen_had_error(ctx) || desc_reg == NULL)
    return inst_list;

  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\tmovq\t8(%s), %s\n", desc_reg->bit_64,
           target_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);

  if (desc_is_heap_temp) {
    /* Spill length result before the release call clobbers caller-saved
     * registers (including target_reg). */
    StackNode_t *len_spill = add_l_t("dynarray_length_spill");
    if (len_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
               target_reg->bit_64, len_spill->offset);
      inst_list = add_inst(inst_list, buffer);
    }
    if (codegen_target_is_windows()) {
      Register_t *u[] = {desc_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
    } else {
      Register_t *u[] = {desc_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
    }
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(
        inst_list, "kgpc_dynarray_release_temp_descriptor");
    free_arg_regs();
    if (len_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
               len_spill->offset, target_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
    }
  }

  free_reg(get_reg_stack(), desc_reg);
  return inst_list;
}

static int codegen_builtin_lowhigh_bounds_from_tag(int type_tag,
                                                   long long *low_out,
                                                   long long *high_out,
                                                   int *use_qword_out) {
  if (low_out == NULL || high_out == NULL || use_qword_out == NULL)
    return 0;

  switch (type_tag) {
  case BOOL:
    *low_out = 0;
    *high_out = 1;
    *use_qword_out = 0;
    return 1;
  case CHAR_TYPE:
  case BYTE_TYPE:
    *low_out = 0;
    *high_out = 255;
    *use_qword_out = 0;
    return 1;
  case WORD_TYPE:
    *low_out = 0;
    *high_out = 65535;
    *use_qword_out = 0;
    return 1;
  case INT_TYPE:
  case LONGINT_TYPE:
    *low_out = -2147483648LL;
    *high_out = 2147483647LL;
    *use_qword_out = 0;
    return 1;
  case LONGWORD_TYPE:
    *low_out = 0;
    *high_out = 4294967295LL;
    *use_qword_out = 1;
    return 1;
  case INT64_TYPE:
    *low_out = (-9223372036854775807LL - 1);
    *high_out = 9223372036854775807LL;
    *use_qword_out = 1;
    return 1;
  case QWORD_TYPE:
    *low_out = 0;
    *high_out = 9223372036854775807LL;
    *use_qword_out = 1;
    return 1;
  default:
    return 0;
  }
}

static int codegen_builtin_lowhigh_try_alias_value(struct TypeAlias *alias,
                                                   int is_high,
                                                   long long *value_out,
                                                   int *use_qword_out) {
  if (alias == NULL || value_out == NULL || use_qword_out == NULL)
    return 0;

  if (alias->range_known) {
    *value_out = is_high ? alias->range_end : alias->range_start;
    *use_qword_out =
        (alias->range_start < INT32_MIN || alias->range_end > INT32_MAX);
    return 1;
  }

  if (alias->is_enum && alias->enum_literals != NULL &&
      !alias->enum_has_explicit_values) {
    int count = ListLength(alias->enum_literals);
    if (count > 0) {
      *value_out = is_high ? (long long)count - 1 : 0;
      *use_qword_out = 0;
      return 1;
    }
  }

  if (alias->is_shortstring) {
    *value_out = is_high ? alias->array_end : alias->array_start;
    *use_qword_out = 0;
    return 1;
  }

  return 0;
}

static int codegen_builtin_lowhigh_try_value(struct Expression *expr,
                                             CodeGenContext *ctx, int is_high,
                                             long long *value_out,
                                             int *use_qword_out) {
  if (expr == NULL || value_out == NULL || use_qword_out == NULL)
    return 0;

  ListNode_t *args = expr->expr_data.function_call_data.args_expr;
  if (args == NULL || args->next != NULL || args->cur == NULL)
    return 0;

  struct Expression *arg_expr = (struct Expression *)args->cur;
  if (arg_expr == NULL)
    return 0;

  if (ctx != NULL && ctx->symtab != NULL && arg_expr->type == EXPR_VAR_ID &&
      arg_expr->expr_data.id != NULL) {
    HashNode_t *type_node = NULL;
    int found = FindSymbol(&type_node, ctx->symtab, arg_expr->expr_data.id);
    if (found != 0 && type_node != NULL &&
        type_node->hash_type == HASHTYPE_TYPE) {
      struct TypeAlias *type_alias = hashnode_get_type_alias(type_node);
      if (type_alias == NULL && type_node->type != NULL)
        type_alias = kgpc_type_get_type_alias(type_node->type);
      if (codegen_builtin_lowhigh_try_alias_value(type_alias, is_high,
                                                  value_out, use_qword_out))
        return 1;
      long long resolved_low = 0;
      long long resolved_high = 0;
      if (semcheck_resolve_range_bounds_for_type(
              ctx->symtab, arg_expr->expr_data.id, &resolved_low,
              &resolved_high)) {
        *value_out = is_high ? resolved_high : resolved_low;
        *use_qword_out =
            (resolved_low < INT32_MIN || resolved_high > INT32_MAX);
        return 1;
      }
      if (type_node->type != NULL) {
        if (kgpc_type_is_array(type_node->type)) {
          long long low = type_node->type->info.array_info.start_index;
          long long high = type_node->type->info.array_info.end_index;
          if (high >= low) {
            *value_out = is_high ? high : low;
            *use_qword_out = (low < INT32_MIN || high > INT32_MAX);
            return 1;
          }
        }
        if (kgpc_type_is_shortstring(type_node->type)) {
          long long low = 0;
          long long high = kgpc_type_sizeof(type_node->type) - 1;
          if (high >= low) {
            *value_out = is_high ? high : low;
            *use_qword_out = 0;
            return 1;
          }
        }
      }
    }
  }

  if (arg_expr->is_array_expr && !arg_expr->array_is_dynamic &&
      arg_expr->array_upper_bound >= arg_expr->array_lower_bound) {
    *value_out =
        is_high ? arg_expr->array_upper_bound : arg_expr->array_lower_bound;
    *use_qword_out = 0;
    return 1;
  }

  KgpcType *arg_type = expr_get_kgpc_type(arg_expr);
  if (arg_type != NULL) {
    struct TypeAlias *alias = kgpc_type_get_type_alias(arg_type);
    if (codegen_builtin_lowhigh_try_alias_value(alias, is_high, value_out,
                                                use_qword_out))
      return 1;

    /* Enums with explicit values: scan literals in the symbol table
     * to find the actual min/max ordinal values. */
    if (alias != NULL && alias->is_enum && alias->enum_literals != NULL &&
        alias->enum_has_explicit_values && ctx != NULL && ctx->symtab != NULL) {
      long long enum_min = LLONG_MAX;
      long long enum_max = LLONG_MIN;
      int found_any = 0;
      for (ListNode_t *lit = alias->enum_literals; lit != NULL;
           lit = lit->next) {
        const char *name = (const char *)lit->cur;
        if (name == NULL)
          continue;
        HashNode_t *lit_node = NULL;
        if (FindSymbol(&lit_node, ctx->symtab, name) != 0 && lit_node != NULL &&
            lit_node->is_constant) {
          long long val = lit_node->const_int_value;
          if (val < enum_min)
            enum_min = val;
          if (val > enum_max)
            enum_max = val;
          found_any = 1;
        }
      }
      if (found_any) {
        *value_out = is_high ? enum_max : enum_min;
        *use_qword_out = (enum_min < INT32_MIN || enum_max > INT32_MAX);
        return 1;
      }
    }
  }

  long long low = 0;
  long long high = 0;
  int use_qword = 0;
  if (codegen_builtin_lowhigh_bounds_from_tag(expr_get_type_tag(arg_expr), &low,
                                              &high, &use_qword)) {
    *value_out = is_high ? high : low;
    *use_qword_out = use_qword;
    return 1;
  }

  /* Look up the argument variable in the symbol table and try to get
   * bounds from its declared type's TypeAlias (covers enums, subranges,
   * and arrays whose element type aliases weren't propagated to the
   * expression). */
  if (ctx != NULL && ctx->symtab != NULL && arg_expr->type == EXPR_VAR_ID &&
      arg_expr->expr_data.id != NULL) {
    HashNode_t *var_node = NULL;
    int found_sym = FindSymbol(&var_node, ctx->symtab, arg_expr->expr_data.id);
    if (found_sym != 0 && var_node != NULL &&
        (var_node->hash_type == HASHTYPE_VAR ||
         var_node->hash_type == HASHTYPE_ARRAY ||
         var_node->hash_type == HASHTYPE_CONST ||
         var_node->hash_type == HASHTYPE_FUNCTION)) {
      /* Try the variable's KgpcType alias */
      if (var_node->type != NULL) {
        struct TypeAlias *var_alias = kgpc_type_get_type_alias(var_node->type);
        if (codegen_builtin_lowhigh_try_alias_value(var_alias, is_high,
                                                    value_out, use_qword_out))
          return 1;

        /* For arrays, return the index bounds */
        if (kgpc_type_is_array(var_node->type)) {
          long long arr_low = var_node->type->info.array_info.start_index;
          long long arr_high = var_node->type->info.array_info.end_index;
          if (arr_high >= arr_low) {
            *value_out = is_high ? arr_high : arr_low;
            *use_qword_out = (arr_low < INT32_MIN || arr_high > INT32_MAX);
            return 1;
          }
        }

        /* Try primitive tag from the variable's type */
        int var_tag = codegen_tag_from_kgpc(var_node->type);
        if (codegen_builtin_lowhigh_bounds_from_tag(var_tag, &low, &high,
                                                    &use_qword)) {
          *value_out = is_high ? high : low;
          *use_qword_out = use_qword;
          return 1;
        }
      }

      /* Try the hash node's inline type alias (for var decl aliases) */
      struct TypeAlias *node_alias = hashnode_get_type_alias(var_node);
      if (codegen_builtin_lowhigh_try_alias_value(node_alias, is_high,
                                                  value_out, use_qword_out))
        return 1;
    }
  }

  /* Last resort: try to get bounds from the function call's resolved
   * procedure type.  For Low/High the return type matches the argument
   * type, so we can derive bounds from the return type's primitive tag. */
  if (expr->type == EXPR_FUNCTION_CALL &&
      expr->expr_data.function_call_data.call_kgpc_type != NULL) {
    KgpcType *proc_type = expr->expr_data.function_call_data.call_kgpc_type;
    if (proc_type->kind == TYPE_KIND_PROCEDURE &&
        proc_type->info.proc_info.return_type != NULL) {
      KgpcType *ret_type = proc_type->info.proc_info.return_type;
      struct TypeAlias *ret_alias = kgpc_type_get_type_alias(ret_type);
      if (codegen_builtin_lowhigh_try_alias_value(ret_alias, is_high, value_out,
                                                  use_qword_out))
        return 1;
      int ret_tag = codegen_tag_from_kgpc(ret_type);
      if (codegen_builtin_lowhigh_bounds_from_tag(ret_tag, &low, &high,
                                                  &use_qword)) {
        *value_out = is_high ? high : low;
        *use_qword_out = use_qword;
        return 1;
      }
    }
  }

  return 0;
}

static ListNode_t *codegen_builtin_lowhigh_fallback(struct Expression *expr,
                                                    ListNode_t *inst_list,
                                                    CodeGenContext *ctx,
                                                    Register_t *target_reg,
                                                    int is_high) {
  long long value = 0;
  int use_qword = 0;
  char buffer[128];

  (void)ctx;
  if (!codegen_builtin_lowhigh_try_value(expr, ctx, is_high, &value,
                                         &use_qword))
    return NULL;

  if (use_qword)
    snprintf(buffer, sizeof(buffer), "\tmovabsq\t$%lld, %s\n", value,
             target_reg->bit_64);
  else
    snprintf(buffer, sizeof(buffer), "\tmovl\t$%lld, %s\n", value,
             target_reg->bit_32);
  return add_inst(inst_list, buffer);
}

static int codegen_lowhigh_arg_is_type_identifier(struct Expression *expr,
                                                  CodeGenContext *ctx) {
  if (expr == NULL || ctx == NULL || ctx->symtab == NULL)
    return 0;

  ListNode_t *args = expr->expr_data.function_call_data.args_expr;
  if (args == NULL || args->next != NULL || args->cur == NULL)
    return 0;

  struct Expression *arg_expr = (struct Expression *)args->cur;
  if (arg_expr == NULL || arg_expr->type != EXPR_VAR_ID ||
      arg_expr->expr_data.id == NULL)
    return 0;

  HashNode_t *type_node = NULL;
  if (FindSymbol(&type_node, ctx->symtab, arg_expr->expr_data.id) == 0 ||
      type_node == NULL)
    return 0;

  return type_node->hash_type == HASHTYPE_TYPE;
}

static ListNode_t *
codegen_builtin_length_type_fallback(struct Expression *expr,
                                     ListNode_t *inst_list, CodeGenContext *ctx,
                                     Register_t *target_reg) {
  if (expr == NULL || ctx == NULL || ctx->symtab == NULL || target_reg == NULL)
    return NULL;

  ListNode_t *args = expr->expr_data.function_call_data.args_expr;
  if (args == NULL || args->next != NULL || args->cur == NULL)
    return NULL;

  struct Expression *arg_expr = (struct Expression *)args->cur;
  if (arg_expr == NULL || arg_expr->type != EXPR_VAR_ID ||
      arg_expr->expr_data.id == NULL)
    return NULL;

  HashNode_t *type_node = NULL;
  if (FindSymbol(&type_node, ctx->symtab, arg_expr->expr_data.id) == 0 ||
      type_node == NULL)
    return NULL;

  /* Accept either a type identifier (Length(SomeArrayType)) or a variable
   * whose type is a fixed-size non-shortstring array (Length(buf), buf:
   * array[0..N] of WideChar / LongInt / etc.).  Without the variable case
   * the codegen falls through to a generic call to kgpc_shortstring_length,
   * which reads buf[0] as a length byte and yields garbage. */
  KgpcType *resolved_type = NULL;
  struct TypeAlias *type_alias = NULL;
  if (type_node->hash_type == HASHTYPE_TYPE) {
    resolved_type = type_node->type;
    type_alias = hashnode_get_type_alias(type_node);
  } else if (type_node->hash_type == HASHTYPE_VAR ||
             type_node->hash_type == HASHTYPE_ARRAY) {
    resolved_type = type_node->type;
    if (resolved_type != NULL)
      type_alias = kgpc_type_get_type_alias(resolved_type);
    /* Restrict the variable path to fixed-size arrays of non-shortstring
     * element types — shortstring vars must keep the runtime length read,
     * and dynamic arrays / classes need the existing call path.  The
     * is_shortstring flag is only sometimes set on local-var `string[N]`
     * declarations, so we additionally detect the shortstring shape by
     * the actual storage: an array starting at index 0 whose element
     * type is 1-byte Char and whose upper bound is at most 255. */
    if (resolved_type == NULL || !kgpc_type_is_array(resolved_type) ||
        kgpc_type_is_shortstring(resolved_type))
      return NULL;
    if (type_alias != NULL && type_alias->is_shortstring)
      return NULL;
    KgpcType *elem = resolved_type->info.array_info.element_type;
    long long low = resolved_type->info.array_info.start_index;
    long long high = resolved_type->info.array_info.end_index;
    if (elem != NULL && kgpc_type_is_char(elem) && kgpc_type_sizeof(elem) == 1 &&
        low == 0 && high >= 0 && high <= 255)
      return NULL;
  } else {
    return NULL;
  }
  if (type_alias == NULL && resolved_type != NULL)
    type_alias = kgpc_type_get_type_alias(resolved_type);

  long long length_value = -1;
  if (type_alias != NULL && type_alias->is_shortstring)
    length_value = (type_alias->array_end - type_alias->array_start) + 1;
  else if (resolved_type != NULL && kgpc_type_is_shortstring(resolved_type))
    length_value = kgpc_type_sizeof(resolved_type);
  else if (resolved_type != NULL && kgpc_type_is_array(resolved_type)) {
    long long low = resolved_type->info.array_info.start_index;
    long long high = resolved_type->info.array_info.end_index;
    if (high >= low)
      length_value = (high - low) + 1;
  }

  if (length_value < 0)
    return NULL;

  char buffer[128];
  if (length_value > INT32_MAX || length_value < INT32_MIN)
    snprintf(buffer, sizeof(buffer), "\tmovabsq\t$%lld, %s\n", length_value,
             target_reg->bit_64);
  else
    snprintf(buffer, sizeof(buffer), "\tmovl\t$%lld, %s\n", length_value,
             target_reg->bit_32);
  return add_inst(inst_list, buffer);
}

/* Function to escape string literals for assembly .string directive */
char *escape_string_for_assembly(const char *input) {
  if (input == NULL)
    return NULL;

  /* Worst case: every char becomes \\xHH */
  size_t len = strlen(input);
  size_t max_len = len * 4 + 1;
  char *escaped = (char *)malloc(max_len);
  if (escaped == NULL)
    return NULL;

  char *dest = escaped;
  const unsigned char *src = (const unsigned char *)input;
  size_t remaining = max_len;

  while (*src != '\0') {
    switch (*src) {
    case '"':
      *dest++ = '\\';
      *dest++ = '"';
      remaining -= 2;
      break;
    case '\\':
      *dest++ = '\\';
      *dest++ = '\\';
      remaining -= 2;
      break;
    case '\n':
      *dest++ = '\\';
      *dest++ = 'n';
      remaining -= 2;
      break;
    case '\t':
      *dest++ = '\\';
      *dest++ = 't';
      remaining -= 2;
      break;
    case '\r':
      *dest++ = '\\';
      *dest++ = 'r';
      remaining -= 2;
      break;
    default:
      if (isprint(*src)) {
        *dest++ = (char)*src;
        --remaining;
      } else {
        int written = snprintf(dest, remaining, "\\x%02X", *src);
        if (written < 0 || (size_t)written >= remaining) {
          *dest = '\0';
          return escaped;
        }
        dest += written;
        remaining -= (size_t)written;
      }
      break;
    }
    src++;
  }
  *dest = '\0';

  return escaped;
}

static int
expr_tree_should_emit_shortstring_literal(const struct Expression *expr,
                                          const HashNode_t *node) {
  if (expr != NULL) {
    if (expr_get_type_tag(expr) == SHORTSTRING_TYPE)
      return 1;
    if (expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_shortstring(expr->resolved_kgpc_type))
      return 1;
    if (expr->resolved_kgpc_type != NULL &&
        expr->resolved_kgpc_type->type_alias != NULL &&
        expr->resolved_kgpc_type->type_alias->is_shortstring) {
      return 1;
    }
  }

  if (node != NULL && node->type != NULL) {
    if (kgpc_type_is_shortstring(node->type))
      return 1;
    if (node->type->type_alias != NULL &&
        node->type->type_alias->is_shortstring)
      return 1;
  }

  return 0;
}

ListNode_t *expr_tree_emit_string_literal_address(ListNode_t *inst_list,
                                                  CodeGenContext *ctx,
                                                  Register_t *target_reg,
                                                  const char *value,
                                                  int emit_shortstring) {
  if (ctx == NULL || target_reg == NULL || value == NULL)
    return inst_list;

  char label[20];
  snprintf(label, sizeof(label), ".LC%d", ctx->write_label_counter++);

  char add_rodata[1536];
  const char *readonly_section = codegen_readonly_section_directive();
  char *escaped_string = escape_string_for_assembly(value);

  if (emit_shortstring) {
    unsigned int short_len = (unsigned int)strlen(value);
    if (short_len > 255)
      short_len = 255;

    if (escaped_string != NULL) {
      snprintf(add_rodata, sizeof(add_rodata),
               "%s\n%s:\n\t.byte %u\n\t.ascii \"%s\"\n\t.byte 0\n%s\n",
               readonly_section, label, short_len, escaped_string,
               codegen_text_section_resume());
      free(escaped_string);
    } else {
      snprintf(add_rodata, sizeof(add_rodata),
               "%s\n%s:\n\t.byte %u\n\t.ascii \"%s\"\n\t.byte 0\n%s\n",
               readonly_section, label, short_len, value,
               codegen_text_section_resume());
    }
  } else {
    if (escaped_string != NULL) {
      snprintf(add_rodata, sizeof(add_rodata),
               "%s\n%s:\n\t.string \"%s\"\n%s\n", readonly_section, label,
               escaped_string, codegen_text_section_resume());
      free(escaped_string);
    } else {
      snprintf(add_rodata, sizeof(add_rodata),
               "%s\n%s:\n\t.string \"%s\"\n%s\n", readonly_section, label,
               value, codegen_text_section_resume());
    }
  }

  inst_list = add_inst(inst_list, add_rodata);
  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", label,
           target_reg->bit_64);
  return add_inst(inst_list, buffer);
}

static inline const char *select_register_name_tag(const Register_t *reg,
                                                   int type_tag) {
  if (reg == NULL)
    return NULL;
  return codegen_type_uses_qword(type_tag) ? reg->bit_64 : reg->bit_32;
}

static inline const char *select_register_name(const Register_t *reg,
                                               const struct Expression *expr,
                                               int fallback_tag) {
  if (reg == NULL)
    return NULL;
  /* Prefer KgpcType-aware width when expression info is available */
  if (expr != NULL && expr_uses_qword_kgpctype(expr))
    return reg->bit_64;
  return select_register_name_tag(reg, fallback_tag);
}

static inline const char *reg_id_to_8bit_name(RegisterId_t reg_id) {
  switch (reg_id) {
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
  case REG_RBP:
    return "%bpl";
  case REG_RSP:
    return "%spl";
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

static inline int operand_is_reg32(const char *operand, const Register_t *reg) {
  return (reg != NULL && operand == reg->bit_32);
}

static inline const char *operand_as_reg32(const char *operand,
                                           const Register_t *reg) {
  return reg != NULL ? reg->bit_32 : operand;
}

const char *operand_as_reg64(const char *operand, const Register_t *reg) {
  return reg != NULL ? reg->bit_64 : operand;
}

static inline const char *operand_as_reg8(const Register_t *reg) {
  if (reg == NULL)
    return NULL;
  return reg_id_to_8bit_name(reg->reg_id);
}

const char *select_divisor_temp_reg(const Register_t *avoid_reg,
                                    int use_qword) {
  /* NOTE: We currently reserve %r10/%r11 as scratch temporaries for div/mod
   * sequences. This assumes the surrounding code does not rely on these
   * registers holding live values. If this changes, integrate with the register
   * allocator to avoid clobbering. */
  const char *primary = use_qword ? "%r10" : "%r10d";
  const char *fallback = use_qword ? "%r11" : "%r11d";
  if (avoid_reg != NULL && avoid_reg->reg_id == REG_R10)
    return fallback;
  return primary;
}

static void expr_tree_register_spill_handler(Register_t *reg,
                                             StackNode_t *spill_slot,
                                             void *context) {
  expr_node_t *node = (expr_node_t *)context;
  if (node == NULL || spill_slot == NULL)
    return;
  node->spill_slot = spill_slot;
  node->reg = NULL;
}

static Register_t *expr_tree_try_get_temp_reg(ListNode_t **inst_list,
                                              Register_t *avoid_reg) {
  Register_t *reg = get_free_reg(get_reg_stack(), inst_list);
  if (reg == avoid_reg) {
    free_reg(get_reg_stack(), reg);
    reg = NULL;
  }
  if (reg == NULL) {
    reg = get_reg_with_spill(get_reg_stack(), inst_list);
    if (reg == avoid_reg)
      reg = get_reg_with_spill(get_reg_stack(), inst_list);
  }
  if (reg == avoid_reg)
    return NULL;
  return reg;
}

static int leaf_expr_requires_reference_value(struct Expression *expr,
                                              CodeGenContext *ctx) {
  if (expr == NULL || ctx == NULL || expr->type != EXPR_VAR_ID)
    return 0;

  int scope_depth = 0;
  StackNode_t *stack_node =
      find_label_with_depth(expr->expr_data.id, &scope_depth);
  HashNode_t *symbol_node = NULL;
  if (ctx->symtab != NULL)
    FindSymbol(&symbol_node, ctx->symtab, expr->expr_data.id);

  int treat_as_reference = 0;
  if (stack_node != NULL && stack_node->is_reference)
    treat_as_reference = 1;
  else if (symbol_node != NULL && symbol_node->is_var_parameter)
    treat_as_reference = 1;

  if (!treat_as_reference)
    return 0;

  int expr_type = expr_get_type_tag(expr);
  if (expr_type == UNKNOWN_TYPE && symbol_node != NULL &&
      symbol_node->type != NULL)
    expr_type = expr_tree_tag_from_kgpc(symbol_node->type);

  int is_array_like = expr->array_is_dynamic || expr->is_array_expr ||
                      (expr->resolved_kgpc_type != NULL &&
                       kgpc_type_is_array(expr->resolved_kgpc_type));

  if (!is_array_like && expr_type != RECORD_TYPE && expr_type != SET_TYPE)
    return 1;

  return 0;
}

static int expr_effective_storage_type(const struct Expression *expr,
                                       CodeGenContext *ctx) {
  if (expr != NULL && expr->resolved_kgpc_type != NULL) {
    int legacy_tag = expr_tree_tag_from_kgpc(expr->resolved_kgpc_type);
    if (legacy_tag != UNKNOWN_TYPE)
      return legacy_tag;
  }

  /* Fall back to symbol table lookup for variables when the resolved type
   * wasn't propagated (common for string-like aliases). */
  if (expr != NULL && ctx != NULL && ctx->symtab != NULL &&
      expr->type == EXPR_VAR_ID) {
    HashNode_t *sym_node = NULL;
    if (FindSymbol(&sym_node, ctx->symtab, expr->expr_data.id) != 0 &&
        sym_node != NULL && sym_node->type != NULL) {
      int sym_tag = expr_tree_tag_from_kgpc(sym_node->type);
      if (sym_tag != UNKNOWN_TYPE)
        return sym_tag;
    }
  }

  return (expr != NULL) ? expr_get_type_tag(expr) : UNKNOWN_TYPE;
}

static long long expr_effective_storage_size_ctx(const struct Expression *expr,
                                                 CodeGenContext *ctx) {
  if (expr == NULL)
    return 0;

  if (expr->resolved_kgpc_type != NULL) {
    long long size = kgpc_type_sizeof(expr->resolved_kgpc_type);
    if (size > 0)
      return size;
  }

  if (ctx != NULL && ctx->symtab != NULL && expr->type == EXPR_VAR_ID &&
      expr->expr_data.id != NULL) {
    HashNode_t *sym_node = NULL;
    if (FindSymbol(&sym_node, ctx->symtab, expr->expr_data.id) != 0 &&
        sym_node != NULL && sym_node->type != NULL) {
      long long size = kgpc_type_sizeof(sym_node->type);
      if (size > 0)
        return size;
    }
  }

  return expr_effective_size_bytes(expr);
}

/**
 * Check if an expression requires 64-bit (qword) storage based on its type.
 * This checks both the type tag and storage_size from KgpcType.
 * This is needed to properly handle Int64/QWord/UInt64 which have
 * storage_size=8 but use LONGINT_TYPE as their base type tag.
 */
static int expr_requires_qword(const struct Expression *expr) {
  if (expr == NULL)
    return 0;

  /* Check type tag first */
  int type_tag = expr_get_type_tag(expr);
  if (type_tag == REAL_TYPE) {
    /* Single-precision reals use 32-bit payloads in GP registers. */
    if (expr->resolved_kgpc_type != NULL &&
        kgpc_type_sizeof(expr->resolved_kgpc_type) == 4) {
      return 0;
    }
    if (expr_effective_size_bytes(expr) == 4)
      return 0;
    return 1;
  }
  if (codegen_type_uses_qword(type_tag))
    return 1;

  /* Check storage_size in KgpcType for Int64/QWord/UInt64 */
  if (expr->resolved_kgpc_type != NULL) {
    struct TypeAlias *alias =
        kgpc_type_get_type_alias(expr->resolved_kgpc_type);
    if (alias != NULL && alias->storage_size >= 8)
      return 1;
  }

  /* Also check for large integer values that require 64 bits */
  if (expr->type == EXPR_INUM) {
    long long val = expr->expr_data.i_num;
    if (val > 2147483647LL || val < -2147483648LL)
      return 1;
  }

  return 0;
}

/* Forward declarations */
static int expr_is_single_real_local(const struct Expression *expr);

static int expr_has_extended_storage(const struct Expression *expr) {
  KgpcType *type = expr_get_kgpc_type(expr);
  return kgpc_type_is_extended(type);
}

static int expr_is_single_real_local(const struct Expression *expr) {
  if (expr == NULL || !expr_has_type_tag(expr, REAL_TYPE))
    return 0;

  if (expr->type == EXPR_RECORD_ACCESS &&
      expr->expr_data.record_access_data.record_expr != NULL &&
      expr->expr_data.record_access_data.field_id != NULL) {
    struct Expression *record_expr =
        expr->expr_data.record_access_data.record_expr;
    struct RecordType *record = record_expr->record_type;
    if (record == NULL && record_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_record(record_expr->resolved_kgpc_type)) {
      record = kgpc_type_get_record(record_expr->resolved_kgpc_type);
    }
    if (record == NULL && record_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_pointer(record_expr->resolved_kgpc_type) &&
        record_expr->resolved_kgpc_type->info.points_to != NULL &&
        kgpc_type_is_record(record_expr->resolved_kgpc_type->info.points_to)) {
      record =
          kgpc_type_get_record(record_expr->resolved_kgpc_type->info.points_to);
    }
    if (record != NULL) {
      for (ListNode_t *cur = record->fields; cur != NULL; cur = cur->next) {
        if (cur->type != LIST_RECORD_FIELD || cur->cur == NULL)
          continue;
        struct RecordField *field = (struct RecordField *)cur->cur;
        if (field->name == NULL ||
            !pascal_identifier_equals(
                field->name, expr->expr_data.record_access_data.field_id)) {
          continue;
        }
        if (field->type_id != NULL &&
            pascal_identifier_equals(field->type_id, "Single"))
          return 1;
        break;
      }
    }
  }

  KgpcType *type = expr_get_kgpc_type(expr);
  if (type != NULL && kgpc_type_sizeof(type) == 4)
    return 1;

  /* Record-field real expressions may be tagged as REAL while physically
   * stored as Single. Use effective expression size as fallback. */
  return expr_effective_size_bytes(expr) == 4;
}

static struct RecordType *
expr_tree_record_from_expr(CodeGenContext *ctx, const struct Expression *expr) {
  if (expr == NULL)
    return NULL;
  if (expr->record_type != NULL)
    return expr->record_type;
  if (expr->resolved_kgpc_type != NULL) {
    KgpcType *type = expr->resolved_kgpc_type;
    if (kgpc_type_is_record(type))
      return kgpc_type_get_record(type);
    if (kgpc_type_is_pointer(type) && type->info.points_to != NULL &&
        kgpc_type_is_record(type->info.points_to))
      return kgpc_type_get_record(type->info.points_to);
  }
  if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL &&
      expr->expr_data.id != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
        node != NULL) {
      struct RecordType *record = hashnode_get_record_type(node);
      if (record != NULL)
        return record;
      if (node->type != NULL) {
        if (kgpc_type_is_record(node->type))
          return kgpc_type_get_record(node->type);
        if (kgpc_type_is_pointer(node->type) &&
            node->type->info.points_to != NULL &&
            kgpc_type_is_record(node->type->info.points_to))
          return kgpc_type_get_record(node->type->info.points_to);
      }
    }
  }
  return NULL;
}

static KgpcType *
expr_tree_proc_type_from_record_field(CodeGenContext *ctx,
                                      const struct Expression *expr) {
  if (expr == NULL || expr->type != EXPR_RECORD_ACCESS ||
      expr->expr_data.record_access_data.record_expr == NULL ||
      expr->expr_data.record_access_data.field_id == NULL)
    return NULL;

  struct RecordField *resolved_field =
      codegen_lookup_record_field_expr((struct Expression *)expr, ctx);
  if (resolved_field != NULL) {
    if (resolved_field->proc_type != NULL &&
        resolved_field->proc_type->kind == TYPE_KIND_PROCEDURE)
      return resolved_field->proc_type;
    if (resolved_field->type == PROCEDURE && resolved_field->type_id != NULL &&
        ctx != NULL && ctx->symtab != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, ctx->symtab, resolved_field->type_id) != 0 &&
          type_node != NULL && type_node->type != NULL &&
          type_node->type->kind == TYPE_KIND_PROCEDURE)
        return type_node->type;
    }
  }

  struct RecordType *record = expr_tree_record_from_expr(
      ctx, expr->expr_data.record_access_data.record_expr);
  if (record == NULL)
    return NULL;

  for (ListNode_t *cur = record->fields; cur != NULL; cur = cur->next) {
    if (cur->type != LIST_RECORD_FIELD || cur->cur == NULL)
      continue;
    struct RecordField *field = (struct RecordField *)cur->cur;
    if (field->name != NULL &&
        pascal_identifier_equals(field->name,
                                 expr->expr_data.record_access_data.field_id)) {
      return field->proc_type;
    }
  }
  return NULL;
}

int expr_is_single_real_with_symtab(const struct Expression *expr,
                                    SymTab_t *symtab) {
  if (expr_is_single_real_local(expr))
    return 1;
  if (expr == NULL || symtab == NULL || !expr_has_type_tag(expr, REAL_TYPE))
    return 0;
  if (expr->type != EXPR_RECORD_ACCESS ||
      expr->expr_data.record_access_data.record_expr == NULL ||
      expr->expr_data.record_access_data.field_id == NULL) {
    return 0;
  }

  struct Expression *record_expr =
      expr->expr_data.record_access_data.record_expr;
  struct RecordType *record = record_expr->record_type;
  if (record == NULL && record_expr->resolved_kgpc_type != NULL &&
      kgpc_type_is_record(record_expr->resolved_kgpc_type)) {
    record = kgpc_type_get_record(record_expr->resolved_kgpc_type);
  }
  if (record == NULL && record_expr->resolved_kgpc_type != NULL &&
      kgpc_type_is_pointer(record_expr->resolved_kgpc_type) &&
      record_expr->resolved_kgpc_type->info.points_to != NULL &&
      kgpc_type_is_record(record_expr->resolved_kgpc_type->info.points_to)) {
    record =
        kgpc_type_get_record(record_expr->resolved_kgpc_type->info.points_to);
  }
  if (record == NULL)
    return 0;

  for (ListNode_t *cur = record->fields; cur != NULL; cur = cur->next) {
    if (cur->type != LIST_RECORD_FIELD || cur->cur == NULL)
      continue;
    struct RecordField *field = (struct RecordField *)cur->cur;
    if (field->name == NULL ||
        !pascal_identifier_equals(
            field->name, expr->expr_data.record_access_data.field_id)) {
      continue;
    }
    if (field->type_id != NULL) {
      if (pascal_identifier_equals(field->type_id, "Single"))
        return 1;
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, symtab, field->type_id) != 0 &&
          type_node != NULL && type_node->type != NULL &&
          kgpc_type_equals_tag(type_node->type, REAL_TYPE) &&
          kgpc_type_sizeof(type_node->type) == 4) {
        return 1;
      }
    }
    return 0;
  }
  return 0;
}

/* True when evaluating `expr` leaves RAW Single (4-byte) bits in a GPR rather
 * than the promoted double-precision bits real values normally carry.  Only
 * single-typed array elements and pointer dereferences read raw single bits;
 * record-field reads are promoted to double on read (see codegen_record_access)
 * and so are NOT raw-single.  A REAL_TYPE reinterpret typecast wrapping such a
 * read is transparent and is stripped first.  This is the single source of
 * truth for the reg_holds_raw_single convention that load_real_operand_into_xmm,
 * the Single-target assignment paths and the write/argument paths all key on:
 * such a value must be promoted (cvtss2sd) before a double consumer and must
 * NOT be re-narrowed (cvtsd2ss) into a Single target. */
int expr_holds_raw_single_bits(const struct Expression *expr,
                               SymTab_t *symtab) {
  struct Expression *raw = (struct Expression *)expr;
  while (raw != NULL && raw->type == EXPR_TYPECAST &&
         raw->expr_data.typecast_data.target_type == REAL_TYPE &&
         raw->expr_data.typecast_data.expr != NULL) {
    raw = raw->expr_data.typecast_data.expr;
  }
  return raw != NULL &&
         (raw->type == EXPR_ARRAY_ACCESS || raw->type == EXPR_POINTER_DEREF) &&
         expr_is_single_real_with_symtab(raw, symtab);
}

/* In the expr-tree evaluator, operands of string / shortstring / char-array /
 * record type are materialised as a POINTER (the address of the data) in their
 * register, never as an inline value.  When such a register is spilled and
 * reloaded across a sibling operand's evaluation, the full 64-bit pointer must
 * be transferred; sizing the spill/reload from the operand's logical element
 * type (which can be a 1- or 2-byte char/index type — e.g. a shortstring array
 * indexed by a byte-sized subrange) truncates the address and corrupts it.
 * This was the root cause of the shortstring array-element address being
 * truncated to a single byte in a value-context string comparison such as
 * FPC's rax86int findreg_by_intname (int_regname_table[int_regname_index[r]]=s),
 * crashing kgpc_shortstring_to_string with a bogus pointer. */
static int expr_tree_operand_reg_is_address(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if (expr_has_type_tag(expr, STRING_TYPE) ||
      expr_has_type_tag(expr, SHORTSTRING_TYPE) ||
      expr_has_type_tag(expr, RECORD_TYPE))
    return 1;
  if (expr->resolved_kgpc_type != NULL &&
      (kgpc_type_is_shortstring(expr->resolved_kgpc_type) ||
       kgpc_type_is_record(expr->resolved_kgpc_type) ||
       kgpc_type_is_array(expr->resolved_kgpc_type)))
    return 1;
  return 0;
}

static ListNode_t *emit_store_to_stack(ListNode_t *inst_list,
                                       const Register_t *reg,
                                       const struct Expression *expr,
                                       int type_tag, int offset) {
  if (inst_list == NULL || reg == NULL)
    return inst_list;

  int use_qword = (expr != NULL && expr_uses_qword_kgpctype(expr)) ||
                  codegen_type_uses_qword(type_tag) ||
                  expr_tree_operand_reg_is_address(expr);
  /* Note: do NOT downgrade to 32-bit for Single locals here.
   * This function spills register values that may have already been
   * promoted to double (via cvtss2sd), so the full 64 bits must be saved. */
  const char *reg_name = use_qword ? reg->bit_64 : reg->bit_32;
  if (reg_name == NULL)
    return inst_list;

  char buffer[64];
  snprintf(buffer, sizeof(buffer), "\tmov%c\t%s, -%d(%%rbp)\n",
           use_qword ? 'q' : 'l', reg_name, offset);
  return add_inst(inst_list, buffer);
}

static ListNode_t *emit_load_from_stack(ListNode_t *inst_list,
                                        const Register_t *reg,
                                        const struct Expression *expr,
                                        int type_tag, int offset,
                                        CodeGenContext *ctx) {
  if (inst_list == NULL || reg == NULL)
    return inst_list;

  int use_qword = (expr != NULL && expr_uses_qword_kgpctype(expr)) ||
                  codegen_type_uses_qword(type_tag) ||
                  expr_tree_operand_reg_is_address(expr);
  /* Note: do NOT downgrade to 32-bit for Single locals here.
   * This function reloads spilled register values that may have already been
   * promoted to double (via cvtss2sd), so the full 64 bits must be loaded. */
  const char *reg_name = use_qword ? reg->bit_64 : reg->bit_32;
  if (reg_name == NULL)
    return inst_list;

  char buffer[64];
  long long storage_size = expr_effective_storage_size_ctx(expr, NULL);
  if (use_qword)
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", offset,
             reg_name);
  else if (type_tag == CHAR_TYPE)
    snprintf(buffer, sizeof(buffer), "\tmovzbl\t-%d(%%rbp), %s\n", offset,
             reg_name);
  else if (storage_size == 2) {
    if (codegen_type_is_signed(type_tag))
      snprintf(buffer, sizeof(buffer), "\tmovswl\t-%d(%%rbp), %s\n", offset,
               reg_name);
    else
      snprintf(buffer, sizeof(buffer), "\tmovzwl\t-%d(%%rbp), %s\n", offset,
               reg_name);
  } else if (storage_size == 1) {
    if (codegen_type_is_signed(type_tag))
      snprintf(buffer, sizeof(buffer), "\tmovsbl\t-%d(%%rbp), %s\n", offset,
               reg_name);
    else
      snprintf(buffer, sizeof(buffer), "\tmovzbl\t-%d(%%rbp), %s\n", offset,
               reg_name);
  } else
    snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", offset,
             reg_name);
  return add_inst(inst_list, buffer);
}

static long long expr_integer_constant_value(const struct Expression *expr,
                                             const char *operand) {
  if (expr != NULL) {
    switch (expr->type) {
    case EXPR_INUM:
      return expr->expr_data.i_num;
    case EXPR_CHAR_CODE:
      return (long long)expr->expr_data.char_code;
    case EXPR_BOOL:
      return expr->expr_data.bool_value ? 1 : 0;
    case EXPR_NIL:
      return 0;
    default:
      break;
    }
  }

  if (operand != NULL && operand[0] == '$')
    return strtoll(operand + 1, NULL, 10);

  return 0;
}

/**
 * Emit a 64-bit ALU instruction (and/or/xor/add/sub) handling the case where
 * the immediate operand exceeds the signed 32-bit range.  x86-64 ALU
 * instructions with 'q' suffix only accept sign-extended 32-bit immediates,
 * so values outside [-2^31, 2^31-1] must be materialised in a scratch
 * register first.
 *
 * Returns the (possibly updated) inst_list, or NULL on allocation failure.
 * Sets *error to 1 on failure so the caller can break out.
 */
ListNode_t *emit_alu_op_with_large_imm(ListNode_t *inst_list,
                                       CodeGenContext *ctx,
                                       const char *mnemonic, char arith_suffix,
                                       const char *op_right,
                                       const char *op_left, int *error) {
  char buffer[128];
  *error = 0;

  if (op_right == NULL) {
    *error = 1;
    return inst_list;
  }

  if (arith_suffix == 'q' && op_right[0] == '$') {
    char *endptr = NULL;
    long long imm_value = strtoll(op_right + 1, &endptr, 0);
    if (endptr != NULL && *endptr == '\0' &&
        (imm_value > INT32_MAX || imm_value < INT32_MIN)) {
      Register_t *imm_reg = get_free_reg(get_reg_stack(), &inst_list);
      if (imm_reg == NULL)
        imm_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
      if (imm_reg == NULL) {
        codegen_report_error(
            ctx,
            "ERROR: Unable to allocate temporary for 64-bit immediate in %s.",
            mnemonic);
        *error = 1;
        return inst_list;
      }
      snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", imm_value,
               imm_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\t%s%c\t%s, %s\n", mnemonic,
               arith_suffix, imm_reg->bit_64, op_left);
      inst_list = add_inst(inst_list, buffer);
      free_reg(get_reg_stack(), imm_reg);
      return inst_list;
    }
  }

  /* Normal path: immediate fits or operand is a register */
  snprintf(buffer, sizeof(buffer), "\t%s%c\t%s, %s\n", mnemonic, arith_suffix,
           op_right, op_left);
  inst_list = add_inst(inst_list, buffer);
  return inst_list;
}

static ListNode_t *
load_real_operand_into_xmm(CodeGenContext *ctx, struct Expression *operand_expr,
                           const char *operand, const Register_t *operand_reg,
                           const char *xmm_reg, ListNode_t *inst_list) {
  if (ctx == NULL || operand == NULL || xmm_reg == NULL)
    return inst_list;

  if (operand_expr != NULL && codegen_expr_involves_extended(operand_expr) &&
      operand[0] != '$') {
    char buffer[192];
    if (!codegen_expr_is_addressable(operand_expr) || operand[0] == '%') {
      StackNode_t *ext_slot = add_l_t_bytes("__ext_operand", 10);
      Register_t *dest_addr = get_free_reg(get_reg_stack(), &inst_list);
      if (ext_slot == NULL || dest_addr == NULL)
        return inst_list;
      snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
               ext_slot->offset, dest_addr->bit_64);
      inst_list = add_inst(inst_list, buffer);
      inst_list = codegen_materialize_extended_expr(operand_expr, inst_list,
                                                    ctx, dest_addr);
      free_reg(get_reg_stack(), dest_addr);
      if (codegen_target_is_windows())
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rcx\n",
                 ext_slot->offset);
      else
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rdi\n",
                 ext_slot->offset);
      inst_list = add_inst(inst_list, buffer);
    } else if (codegen_target_is_windows()) {
      snprintf(buffer, sizeof(buffer), "\tleaq\t%s, %%rcx\n", operand);
      inst_list = add_inst(inst_list, buffer);
    } else {
      snprintf(buffer, sizeof(buffer), "\tleaq\t%s, %%rdi\n", operand);
      inst_list = add_inst(inst_list, buffer);
    }
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list =
        codegen_call_with_shadow_space(inst_list, "kgpc_load_extended_to_bits");
    free_arg_regs();
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", xmm_reg);
    return add_inst(inst_list, buffer);
  }

  int operand_is_real =
      operand_expr != NULL && expr_has_type_tag(operand_expr, REAL_TYPE);
  int operand_is_longint =
      operand_expr != NULL && expr_uses_qword_kgpctype(operand_expr);
  int operand_is_integer_like =
      (operand_expr != NULL && (expr_has_type_tag(operand_expr, LONGINT_TYPE) ||
                                expr_has_type_tag(operand_expr, INT_TYPE) ||
                                expr_has_type_tag(operand_expr, BOOL) ||
                                expr_has_type_tag(operand_expr, CHAR_TYPE)));
  struct Expression *raw_operand_expr = operand_expr;
  while (raw_operand_expr != NULL && raw_operand_expr->type == EXPR_TYPECAST &&
         raw_operand_expr->expr_data.typecast_data.target_type == REAL_TYPE &&
         raw_operand_expr->expr_data.typecast_data.expr != NULL) {
    raw_operand_expr = raw_operand_expr->expr_data.typecast_data.expr;
  }
  if (operand_is_real && raw_operand_expr != NULL &&
      (expr_has_type_tag(raw_operand_expr, LONGINT_TYPE) ||
       expr_has_type_tag(raw_operand_expr, INT_TYPE) ||
       expr_has_type_tag(raw_operand_expr, BOOL) ||
       expr_has_type_tag(raw_operand_expr, CHAR_TYPE))) {
    operand_is_real = 0;
    operand_is_integer_like = 1;
    operand_is_longint = expr_uses_qword_kgpctype(raw_operand_expr);
  }

  char buffer[192];
  int is_single_real = 0;
  if (operand_is_real && operand_expr != NULL)
    is_single_real = expr_is_single_real_with_symtab(
        operand_expr, ctx != NULL ? ctx->symtab : NULL);

  if (operand_is_real) {
    if (operand[0] == '$') {
      char label[32];
      snprintf(label, sizeof(label), ".LC%d", ctx->write_label_counter++);

      const char *readonly_section = codegen_readonly_section_directive();
      char rodata_buffer[192];
      if (is_single_real) {
        union {
          float f;
          int32_t i;
        } converter;
        converter.f = (float)operand_expr->expr_data.r_num;
        snprintf(rodata_buffer, sizeof(rodata_buffer),
                 "%s\n%s:\n\t.long %d\n%s\n", readonly_section, label,
                 (int)converter.i, codegen_text_section_resume());
      } else {
        snprintf(rodata_buffer, sizeof(rodata_buffer),
                 "%s\n%s:\n\t.quad %s\n%s\n", readonly_section, label,
                 operand + 1, codegen_text_section_resume());
      }
      inst_list = add_inst(inst_list, rodata_buffer);

      if (is_single_real) {
        snprintf(buffer, sizeof(buffer), "\tmovss\t%s(%%rip), %s\n", label,
                 xmm_reg);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tcvtss2sd\t%s, %s\n", xmm_reg,
                 xmm_reg);
        return add_inst(inst_list, buffer);
      }
      snprintf(buffer, sizeof(buffer), "\tmovsd\t%s(%%rip), %s\n", label,
               xmm_reg);
      return add_inst(inst_list, buffer);
    }

    const char *source_operand = operand;
    if (operand_reg != NULL) {
      const char *converted = reg32_to_reg64(operand, operand_reg);
      if (converted != NULL)
        source_operand = converted;
    }

    if (operand_reg != NULL) {
      /* Single-typed array elements and pointer dereferences leave RAW single
       * bits in the register (record-field reads are promoted to double on
       * read, see codegen_record_access).  expr_holds_raw_single_bits also
       * looks through a transparent REAL(...) reinterpret cast, so
       * Real(arr[i]) / Real(p^) are handled too. */
      int reg_holds_raw_single = expr_holds_raw_single_bits(
          operand_expr, ctx != NULL ? ctx->symtab : NULL);
      if (reg_holds_raw_single) {
        const char *reg32 = reg64_to_reg32(operand, operand_reg);
        if (reg32 == NULL)
          reg32 = operand;
        snprintf(buffer, sizeof(buffer), "\tmovd\t%s, %s\n", reg32, xmm_reg);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tcvtss2sd\t%s, %s\n", xmm_reg,
                 xmm_reg);
        return add_inst(inst_list, buffer);
      }
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", source_operand,
               xmm_reg);
      return add_inst(inst_list, buffer);
    }

    if (is_single_real) {
      snprintf(buffer, sizeof(buffer), "\tmovss\t%s, %s\n", source_operand,
               xmm_reg);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tcvtss2sd\t%s, %s\n", xmm_reg,
               xmm_reg);
      return add_inst(inst_list, buffer);
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", source_operand,
             xmm_reg);
    return add_inst(inst_list, buffer);
  }

  if (!operand_is_integer_like && operand_expr == NULL) {
    /* Fallback: assume operand already holds IEEE bits (e.g., from string
     * literal) */
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", operand, xmm_reg);
    return add_inst(inst_list, buffer);
  }

  if (operand[0] == '$') {
    long long int_value = expr_integer_constant_value(operand_expr, operand);
    double real_value = (double)int_value;
    union {
      double d;
      long long i;
    } converter;
    converter.d = real_value;

    char label[32];
    snprintf(label, sizeof(label), ".LC%d", ctx->write_label_counter++);
    const char *readonly_section = codegen_readonly_section_directive();
    char rodata_buffer[192];
    snprintf(rodata_buffer, sizeof(rodata_buffer),
             "%s\n%s:\n\t.quad %lld\n%s\n", readonly_section, label,
             (long long)converter.i, codegen_text_section_resume());
    inst_list = add_inst(inst_list, rodata_buffer);

    snprintf(buffer, sizeof(buffer), "\tmovsd\t%s(%%rip), %s\n", label,
             xmm_reg);
    return add_inst(inst_list, buffer);
  }

  /* For cvtsi2sd, we need to match the register size to the instruction suffix:
   * cvtsi2sdl uses 32-bit register, cvtsi2sdq uses 64-bit register */
  const char *convert_instr;
  const char *convert_reg;
  if (operand_is_longint) {
    convert_instr = "cvtsi2sdq";
    if (operand_reg != NULL)
      convert_reg = reg32_to_reg64(operand, operand_reg);
    else
      convert_reg = operand;
  } else {
    convert_instr = "cvtsi2sdl";
    if (operand_reg != NULL)
      convert_reg = reg64_to_reg32(operand, operand_reg);
    else
      convert_reg = operand;
  }
  snprintf(buffer, sizeof(buffer), "\t%s\t%s, %s\n", convert_instr, convert_reg,
           xmm_reg);
  return add_inst(inst_list, buffer);
}

ListNode_t *
gencode_real_binary_op(CodeGenContext *ctx, struct Expression *left_expr,
                       const char *left_operand, const Register_t *left_reg,
                       struct Expression *right_expr, const char *right_operand,
                       const Register_t *right_reg, const char *dest,
                       const Register_t *dest_reg, ListNode_t *inst_list,
                       const char *sse_mnemonic) {
  if (ctx == NULL || left_operand == NULL || right_operand == NULL ||
      dest == NULL || sse_mnemonic == NULL) {
    return inst_list;
  }

  inst_list = load_real_operand_into_xmm(ctx, left_expr, left_operand, left_reg,
                                         "%xmm0", inst_list);
  inst_list = load_real_operand_into_xmm(ctx, right_expr, right_operand,
                                         right_reg, "%xmm1", inst_list);

  char buffer[80];
  snprintf(buffer, sizeof(buffer), "\t%s\t%%xmm1, %%xmm0\n", sse_mnemonic);
  inst_list = add_inst(inst_list, buffer);

  /* movq requires a 64-bit register, so convert 32-bit register to 64-bit */
  const char *dest64 = reg32_to_reg64(dest, dest_reg);
  snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n", dest64);
  return add_inst(inst_list, buffer);
}

static ListNode_t *gencode_real_negate(const char *value_operand,
                                       const Register_t *value_reg,
                                       const char *dest,
                                       const Register_t *dest_reg,
                                       ListNode_t *inst_list) {
  if (value_operand == NULL || dest == NULL)
    return inst_list;

  char buffer[96];
  /* movq requires a 64-bit register, so convert 32-bit operand/dest to 64-bit
   */
  const char *value64 = reg32_to_reg64(value_operand, value_reg);
  snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", value64);
  inst_list = add_inst(inst_list, buffer);
  inst_list = add_inst(inst_list, "\tpxor\t%xmm1, %xmm1\n");
  inst_list = add_inst(inst_list, "\tsubsd\t%xmm0, %xmm1\n");
  const char *dest64 = reg32_to_reg64(dest, dest_reg);
  snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm1, %s\n", dest64);
  return add_inst(inst_list, buffer);
}

/*
 * Convert a 64-bit register name to its 32-bit equivalent.
 * This function is idempotent - if the input is already a 32-bit register, it
 * returns it unchanged. Examples: %rax -> %eax %r8  -> %r8d %r8d -> %r8d
 * (already 32-bit, returns unchanged)
 */
const char *reg64_to_reg32(const char *reg_name, const Register_t *reg) {
  return operand_as_reg32(reg_name, reg);
}

/*
 * Convert any register to its 32-bit equivalent if it's a 64-bit register.
 * Returns the register unchanged if it's already 32-bit or not a register that
 * needs conversion.
 */
const char *reg_to_reg32(const char *reg_name, const Register_t *reg) {
  if (reg == NULL && reg_name != NULL) {
    if (strcmp(reg_name, "%rax") == 0)
      return "%eax";
    if (strcmp(reg_name, "%rbx") == 0)
      return "%ebx";
    if (strcmp(reg_name, "%rcx") == 0)
      return "%ecx";
    if (strcmp(reg_name, "%rdx") == 0)
      return "%edx";
    if (strcmp(reg_name, "%rsi") == 0)
      return "%esi";
    if (strcmp(reg_name, "%rdi") == 0)
      return "%edi";
    if (strcmp(reg_name, "%r8") == 0)
      return "%r8d";
    if (strcmp(reg_name, "%r9") == 0)
      return "%r9d";
    if (strcmp(reg_name, "%r10") == 0)
      return "%r10d";
    if (strcmp(reg_name, "%r11") == 0)
      return "%r11d";
    if (strcmp(reg_name, "%r12") == 0)
      return "%r12d";
    if (strcmp(reg_name, "%r13") == 0)
      return "%r13d";
    if (strcmp(reg_name, "%r14") == 0)
      return "%r14d";
    if (strcmp(reg_name, "%r15") == 0)
      return "%r15d";
  }
  return operand_as_reg32(reg_name, reg);
}

const char *reg_to_reg64(const char *reg_name, const Register_t *reg) {
  return operand_as_reg64(reg_name, reg);
}

/*
 * Convert a 32-bit register name to its 8-bit equivalent.
 * Examples:
 *   %eax -> %al
 *   %r8d -> %r8b
 */
const char *reg32_to_reg8(const char *reg_name, const Register_t *reg) {
  if (reg == NULL && reg_name != NULL) {
    if (strcmp(reg_name, "%eax") == 0 || strcmp(reg_name, "%rax") == 0)
      return "%al";
    if (strcmp(reg_name, "%ebx") == 0 || strcmp(reg_name, "%rbx") == 0)
      return "%bl";
    if (strcmp(reg_name, "%ecx") == 0 || strcmp(reg_name, "%rcx") == 0)
      return "%cl";
    if (strcmp(reg_name, "%edx") == 0 || strcmp(reg_name, "%rdx") == 0)
      return "%dl";
    if (strcmp(reg_name, "%esi") == 0 || strcmp(reg_name, "%rsi") == 0)
      return "%sil";
    if (strcmp(reg_name, "%edi") == 0 || strcmp(reg_name, "%rdi") == 0)
      return "%dil";
    if (strcmp(reg_name, "%r8d") == 0 || strcmp(reg_name, "%r8") == 0)
      return "%r8b";
    if (strcmp(reg_name, "%r9d") == 0 || strcmp(reg_name, "%r9") == 0)
      return "%r9b";
    if (strcmp(reg_name, "%r10d") == 0 || strcmp(reg_name, "%r10") == 0)
      return "%r10b";
    if (strcmp(reg_name, "%r11d") == 0 || strcmp(reg_name, "%r11") == 0)
      return "%r11b";
    if (strcmp(reg_name, "%r12d") == 0 || strcmp(reg_name, "%r12") == 0)
      return "%r12b";
    if (strcmp(reg_name, "%r13d") == 0 || strcmp(reg_name, "%r13") == 0)
      return "%r13b";
    if (strcmp(reg_name, "%r14d") == 0 || strcmp(reg_name, "%r14") == 0)
      return "%r14b";
    if (strcmp(reg_name, "%r15d") == 0 || strcmp(reg_name, "%r15") == 0)
      return "%r15b";
  }
  return operand_as_reg8(reg);
}

/*
 * Convert a 32-bit register name to its 64-bit equivalent.
 * This function is idempotent - if the input is already a 64-bit register, it
 * returns it unchanged. Examples: %eax -> %rax %r8d -> %r8 %r8  -> %r8 (already
 * 64-bit, returns unchanged)
 */
const char *reg32_to_reg64(const char *reg_name, const Register_t *reg) {
  return operand_as_reg64(reg_name, reg);
}

int operand_is_32bit_register(const char *operand, const Register_t *reg) {
  (void)operand;
  return operand_is_reg32(operand, reg);
}

int type_tag_is_signed_32bit_int(int type_tag) {
  return (type_tag == INT_TYPE || type_tag == LONGINT_TYPE);
}

/* Helper functions */
ListNode_t *gencode_sign_term(expr_node_t *node, ListNode_t *inst_list,
                              CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case0(expr_node_t *node, ListNode_t *inst_list,
                          CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case1(expr_node_t *node, ListNode_t *inst_list,
                          CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case2(expr_node_t *node, ListNode_t *inst_list,
                          CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *gencode_case3(expr_node_t *node, ListNode_t *inst_list,
                          CodeGenContext *ctx, Register_t *target_reg);
static ListNode_t *promote_char_operand_to_string(expr_node_t *node,
                                                  ListNode_t *inst_list,
                                                  CodeGenContext *ctx,
                                                  Register_t *value_reg);
static ListNode_t *gencode_string_concat(expr_node_t *node,
                                         ListNode_t *inst_list,
                                         CodeGenContext *ctx,
                                         Register_t *target_reg);
static struct Expression *
expr_tree_simplify_to_literal(const struct Expression *expr);
static expr_node_t *
build_expr_tree_internal(struct Expression *expr,
                         int preserve_narrowing_in_arithmetic);

/* Builds an expression tree out of an expression */
/* WARNING: Does not make deep copy of expression */
/* WARNING: Does not do relational expressions */
expr_node_t *build_expr_tree(struct Expression *expr) {
  return build_expr_tree_internal(expr, 0);
}

/* A typecast to a signed integer narrower than 32 bits (ShortInt = 1 byte,
 * SmallInt = 2 bytes) reinterprets the low byte(s) of its operand as a signed
 * value.  When the result is consumed at a wider width it must be sign-extended
 * from its OWN width, regardless of the operand's signedness.  If such a cast
 * is stripped, the operand is loaded with the operand's signedness — e.g.
 * SmallInt(word_$ffff) loads the Word zero-extended (movzwl) and stays 65535
 * instead of -1.  Detect these casts so they are preserved as tree leaves and
 * gencode_case0 emits the movsbl/movswl.  size_out, if non-NULL, receives the
 * cast type's width in bytes (1 or 2). */
static int expr_typecast_is_signed_narrow(const struct Expression *expr,
                                          int *size_out) {
  if (expr == NULL || expr->type != EXPR_TYPECAST)
    return 0;
  KgpcType *cast_type = expr_get_kgpc_type(expr);
  if (cast_type == NULL || !kgpc_type_is_signed(cast_type))
    return 0;
  long long sz = kgpc_type_sizeof(cast_type);
  if (sz != 1 && sz != 2)
    return 0;
  if (size_out != NULL)
    *size_out = (int)sz;
  return 1;
}

static expr_node_t *
build_expr_tree_internal(struct Expression *expr,
                         int preserve_narrowing_in_arithmetic) {
  assert(expr != NULL);

  if (expr->type == EXPR_TYPECAST &&
      expr->expr_data.typecast_data.expr != NULL) {
    /* When a typecast converts an array to a pointer type (e.g.
     * PByte(top^.data) where data is array[0..0] of byte), the result should be
     * the ADDRESS of the array, not its element value.  Keep the TYPECAST node
     * as a leaf so gencode_case0 can emit an address computation instead of a
     * value load. */
    struct Expression *tc_inner = expr->expr_data.typecast_data.expr;
    int tc_target = expr->expr_data.typecast_data.target_type;
    int preserve_leaf_typecast = 0;
    if (tc_target == POINTER_TYPE && tc_inner->is_array_expr &&
        codegen_expr_is_addressable(tc_inner)) {
      preserve_leaf_typecast = 1;
    } else if (preserve_narrowing_in_arithmetic &&
               (tc_target == BYTE_TYPE || tc_target == WORD_TYPE)) {
      preserve_leaf_typecast = 1;
    }
    /* Char-valued typecast (e.g. char(65), char(byte(x))) feeding a string
     * concatenation operand. If we strip the typecast here, the operand node
     * carries the inner integer's type (INT_TYPE) and the concat's
     * char->string promotion never fires, so the raw ordinal is handed to
     * kgpc_string_concat as a pointer. Preserve as a leaf so the resolved
     * type stays CHAR_TYPE and gencode_case0 materialises the byte value. */
    else if (preserve_narrowing_in_arithmetic && tc_target == CHAR_TYPE) {
      preserve_leaf_typecast = 1;
    }
    /* Widening typecast Int64(longint_var) / QWord(longint_var) must
     * sign-extend the 32-bit value to 64 bits.  If we strip the typecast
     * here, the inner load is a 32-bit movl that zero-extends, producing
     * 0x00000000FFFFFFFF for longint(-1) instead of 0xFFFFFFFFFFFFFFFF.
     * Preserve as leaf so gencode_case0 can emit movslq after the load. */
    else if ((tc_target == INT64_TYPE || tc_target == QWORD_TYPE) &&
             type_tag_is_signed_32bit_int(expr_get_type_tag(tc_inner))) {
      preserve_leaf_typecast = 1;
    }
    /* Narrowing/reinterpret typecast to a signed sub-32-bit integer
     * (SmallInt(x) / ShortInt(x)).  Stripping it loses the sign reinterpret:
     * SmallInt(word_$ffff) would load the Word zero-extended and stay 65535
     * instead of -1.  This is exactly how FPC's str() default-format sentinels
     * (-1, -32767, created as LongInt consts then narrowed to the SMALLINT
     * compilerproc params) lost their sign in the KGPC-built FPC, pushing the
     * Windows self-host one stage past the canonical stage-3 fixpoint.
     * Preserve as a leaf so gencode_case0 sign-extends from the cast width. */
    else if (expr_typecast_is_signed_narrow(expr, NULL)) {
      preserve_leaf_typecast = 1;
    }
    /* Narrowing typecast Extended -> Double/Single (e.g. FPC's
     * `ts64real(value_real)` where value_real:bestreal=extended).  If we
     * strip this typecast, the inner load reads only 8 bytes of the
     * 10-byte extended representation and treats them as a double — that
     * yields garbage (e.g. extended 3.0 -> double -2.0).  Preserve the
     * typecast as a leaf so gencode_case0 can emit a proper FPU-based
     * narrowing conversion via kgpc_load_extended_to_bits. */
    else if (tc_target == REAL_TYPE) {
      KgpcType *inner_type = expr_get_kgpc_type(tc_inner);
      if (inner_type != NULL && kgpc_type_is_extended(inner_type))
        preserve_leaf_typecast = 1;
    }

    if (!preserve_leaf_typecast) {
      return build_expr_tree_internal(tc_inner,
                                      preserve_narrowing_in_arithmetic);
    }
  }

  expr_node_t *new_node;

  new_node = (expr_node_t *)malloc(sizeof(expr_node_t));
  assert(new_node != NULL);
  new_node->expr = expr;
  new_node->reg = NULL;
  new_node->spill_slot = NULL;

  /* Building the tree */
  switch (expr->type) {
  case EXPR_ADDOP:
    new_node->left_expr =
        build_expr_tree_internal(expr->expr_data.addop_data.left_expr, 1);
    new_node->right_expr =
        build_expr_tree_internal(expr->expr_data.addop_data.right_term, 1);
    break;
  case EXPR_MULOP:
    new_node->left_expr =
        build_expr_tree_internal(expr->expr_data.mulop_data.left_term, 1);
    new_node->right_expr =
        build_expr_tree_internal(expr->expr_data.mulop_data.right_factor, 1);
    break;

  case EXPR_SIGN_TERM:
    new_node->left_expr =
        build_expr_tree_internal(expr->expr_data.sign_term, 1);
    new_node->right_expr = NULL;
    break;

  case EXPR_VAR_ID:
  case EXPR_ARRAY_ACCESS:
  case EXPR_RECORD_ACCESS:
  case EXPR_INUM:
  case EXPR_RNUM:
  case EXPR_FUNCTION_CALL:
  case EXPR_STRING:
  case EXPR_CHAR_CODE:
  case EXPR_BOOL:
  case EXPR_NIL:
  case EXPR_SET:
  case EXPR_POINTER_DEREF:
  case EXPR_ADDR:
  case EXPR_ADDR_OF_PROC:
  case EXPR_TYPEINFO:
  case EXPR_IS:
  case EXPR_AS:
  case EXPR_ANONYMOUS_FUNCTION:
  case EXPR_ANONYMOUS_PROCEDURE:
  case EXPR_RECORD_CONSTRUCTOR:
  case EXPR_ARRAY_LITERAL:
    new_node->left_expr = NULL;
    new_node->right_expr = NULL;
    break;

  case EXPR_TYPECAST:
    new_node->left_expr = NULL;
    new_node->right_expr = NULL;
    break;

  case EXPR_RELOP:
    new_node->left_expr =
        build_expr_tree_internal(expr->expr_data.relop_data.left, 1);
    if (expr->expr_data.relop_data.type == NOT) {
      new_node->right_expr = NULL;
      break;
    }
    assert(expr->expr_data.relop_data.right != NULL);
    new_node->right_expr =
        build_expr_tree_internal(expr->expr_data.relop_data.right, 1);
    break;

  default:
    assert(0 && "Unsupported expr_tree type");
    break;
  }

  /* Setting the labels */
  if (new_node->left_expr != NULL) {
    if (expr_tree_is_leaf(new_node->left_expr) == 1) {
      new_node->left_expr->label = 1;
    }
  }

  if (new_node->left_expr == NULL && new_node->right_expr == NULL) {
    new_node->label = 0;
  } else if (new_node->left_expr == NULL) {
    new_node->label = new_node->right_expr->label;
  } else if (new_node->right_expr == NULL) {
    new_node->label = new_node->left_expr->label;
  } else if (new_node->left_expr->label > new_node->right_expr->label) {
    new_node->label = new_node->left_expr->label;
  } else if (new_node->left_expr->label < new_node->right_expr->label) {
    new_node->label = new_node->right_expr->label;
  } else /* (new_node->left_expr->label == new_node->right_expr->label) */
  {
    new_node->label = new_node->left_expr->label + 1;
  }

  return new_node;
}

static int leaf_expr_is_simple(const struct Expression *expr) {
  if (expr == NULL)
    return 0;

  switch (expr->type) {
  case EXPR_VAR_ID:
  case EXPR_INUM:
  case EXPR_RNUM:
  case EXPR_CHAR_CODE:
  case EXPR_BOOL:
  case EXPR_NIL:
  case EXPR_TYPEINFO:
    return 1;
  default:
    return 0;
  }
}

static const char *expr_tree_register_name8(const Register_t *reg) {
  if (reg == NULL || reg->bit_64 == NULL)
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
  case REG_RBP:
    return "%bpl";
  case REG_RSP:
    return "%spl";
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

static ListNode_t *gencode_shortcircuit_bool(expr_node_t *node,
                                             ListNode_t *inst_list,
                                             CodeGenContext *ctx,
                                             Register_t *target_reg,
                                             int is_or) {
  if (node == NULL || node->left_expr == NULL || node->right_expr == NULL ||
      ctx == NULL || target_reg == NULL)
    return inst_list;

  char skip_label[32];
  char done_label[32];
  gen_label(skip_label, sizeof(skip_label), ctx);
  gen_label(done_label, sizeof(done_label), ctx);

  inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
  if (codegen_had_error(ctx))
    return inst_list;

  const char *reg32 = target_reg->bit_32;
  const char *reg8 = expr_tree_register_name8(target_reg);
  if (reg32 == NULL || reg8 == NULL) {
    codegen_report_error(
        ctx, "ERROR: Unable to select register for short-circuit boolean.");
    return inst_list;
  }

  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", reg32);
  inst_list = add_inst(inst_list, buffer);
  snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", is_or ? "jne" : "je",
           skip_label);
  inst_list = add_inst(inst_list, buffer);

  inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);
  if (codegen_had_error(ctx))
    return inst_list;

  snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", reg32);
  inst_list = add_inst(inst_list, buffer);
  snprintf(buffer, sizeof(buffer), "\tsetne\t%s\n", reg8);
  inst_list = add_inst(inst_list, buffer);
  snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", reg8, reg32);
  inst_list = add_inst(inst_list, buffer);
  snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", done_label);
  inst_list = add_inst(inst_list, buffer);

  snprintf(buffer, sizeof(buffer), "%s:\n", skip_label);
  inst_list = add_inst(inst_list, buffer);
  snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", is_or ? 1 : 0, reg32);
  inst_list = add_inst(inst_list, buffer);

  snprintf(buffer, sizeof(buffer), "%s:\n", done_label);
  inst_list = add_inst(inst_list, buffer);

  node->reg = target_reg;
  register_set_spill_callback(target_reg, expr_tree_register_spill_handler,
                              node);
  return inst_list;
}

/* The famous gencode algorithm */
ListNode_t *gencode_expr_tree(expr_node_t *node, ListNode_t *inst_list,
                              CodeGenContext *ctx, Register_t *target_reg) {
  assert(node != NULL);
  assert(node->expr != NULL);
  assert(ctx != NULL);
  assert(target_reg != NULL);

#ifdef DEBUG_CODEGEN
  fprintf(stderr, "gencode_expr_tree: node->expr->type = %d\n",
          node->expr->type);
#endif

  if (node->reg == NULL && node->spill_slot != NULL) {
    inst_list = emit_load_from_stack(inst_list, target_reg, node->expr,
                                     expr_get_type_tag(node->expr),
                                     node->spill_slot->offset, ctx);
    node->reg = target_reg;
    register_set_spill_callback(target_reg, expr_tree_register_spill_handler,
                                node);
    node->spill_slot = NULL;
    return inst_list;
  }

  if (node->reg != NULL) {
    char buffer[64];
    const char *src = select_register_name(node->reg, node->expr,
                                           expr_get_type_tag(node->expr));
    const char *dst = select_register_name(target_reg, node->expr,
                                           expr_get_type_tag(node->expr));
    if (src != NULL && dst != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmov%s\t%s, %s\n",
               codegen_type_uses_qword(expr_get_type_tag(node->expr)) ? "q"
                                                                      : "l",
               src, dst);
      inst_list = add_inst(inst_list, buffer);
    }
    return inst_list;
  }

  struct Expression *simplified_leaf =
      expr_tree_simplify_to_literal(node->expr);
  if (simplified_leaf != NULL) {
    expr_node_t simplified_node = {0};
    simplified_node.expr = simplified_leaf;
    inst_list = gencode_case0(&simplified_node, inst_list, ctx, target_reg);
    destroy_expr(simplified_leaf);
    node->reg = target_reg;
    register_set_spill_callback(target_reg, expr_tree_register_spill_handler,
                                node);
    return inst_list;
  }

  /* Short-circuit boolean operators */
  if (node->expr->type == EXPR_MULOP && expr_get_type_tag(node->expr) == BOOL &&
      node->expr->expr_data.mulop_data.mulop_type == AND &&
      node->left_expr != NULL && node->right_expr != NULL) {
    return gencode_shortcircuit_bool(node, inst_list, ctx, target_reg, 0);
  }
  if (node->expr->type == EXPR_ADDOP && expr_get_type_tag(node->expr) == BOOL &&
      node->expr->expr_data.addop_data.addop_type == OR &&
      node->left_expr != NULL && node->right_expr != NULL) {
    return gencode_shortcircuit_bool(node, inst_list, ctx, target_reg, 1);
  }

  /*if(node->label > get_num_registers_free(get_reg_stack()))
  {
      fprintf(stderr, "ERROR: codegen more complex than number of registers is
  unsupported!\n"); exit(1);
  }*/

  /* Handle special cases first */
  if (node->expr->type == EXPR_SIGN_TERM) {
    inst_list = gencode_sign_term(node, inst_list, ctx, target_reg);
  } else if (node->expr->type == EXPR_ADDOP &&
             node->expr->expr_data.addop_data.addop_type == PLUS &&
             expr_get_type_tag(node->expr) == STRING_TYPE) {
    inst_list = gencode_string_concat(node, inst_list, ctx, target_reg);
  }
  /* CASE 0 */
  else if (expr_tree_is_leaf(node) == 1) {
    inst_list = gencode_case0(node, inst_list, ctx, target_reg);
    node->reg = target_reg;
    register_set_spill_callback(target_reg, expr_tree_register_spill_handler,
                                node);
  }
  /* CASE 1 */
  else if (node->right_expr == NULL) {
    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
    const char *target_name = select_register_name(
        target_reg, node->expr, expr_get_type_tag(node->expr));
    if (target_name != NULL)
      inst_list = gencode_op(node->expr, target_name, target_reg, target_name,
                             target_reg, OPKIND_REGISTER, OPKIND_REGISTER,
                             inst_list, ctx);
  } else if (node->right_expr != NULL && expr_tree_is_leaf(node->right_expr)) {
    inst_list = gencode_case1(node, inst_list, ctx, target_reg);
  }
  /* CASE 2 */
  else if (node->left_expr->label < node->right_expr->label) {
    inst_list = gencode_case2(node, inst_list, ctx, target_reg);
  }
  /* CASE 3 */
  else if (node->left_expr->label >= node->right_expr->label) {
    inst_list = gencode_case3(node, inst_list, ctx, target_reg);
  } else {
    assert(0 && "Unsupported case in codegen!");
  }

  return inst_list;
}

static ListNode_t *promote_char_operand_to_string(expr_node_t *node,
                                                  ListNode_t *inst_list,
                                                  CodeGenContext *ctx,
                                                  Register_t *value_reg) {
  if (node == NULL || node->expr == NULL || value_reg == NULL)
    return inst_list;

  int is_shortstring = (expr_get_type_tag(node->expr) == SHORTSTRING_TYPE) ||
                       is_shortstring_array(expr_get_type_tag(node->expr),
                                            node->expr->is_array_expr);
  if (is_shortstring)
    return inst_list;

  if (expr_get_type_tag(node->expr) != CHAR_TYPE)
    return inst_list;

  const char *arg_reg32 = current_arg_reg32(0);
  if (arg_reg32 == NULL)
    return inst_list;

  char buffer[128];

  /* Move the character value into the first integer argument register,
   * zero-extending as needed. */
  snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", value_reg->bit_32,
           arg_reg32);
  inst_list = add_inst(inst_list, buffer);

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
  snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", RETURN_REG_64,
           value_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);
  free_arg_regs();
  return inst_list;
}

static ListNode_t *
promote_shortstring_operand_to_string(expr_node_t *node, ListNode_t *inst_list,
                                      CodeGenContext *ctx,
                                      Register_t *value_reg) {
  if (node == NULL || node->expr == NULL || ctx == NULL || value_reg == NULL)
    return inst_list;

  if (!expr_is_shortstring_storage_ctx(node->expr, ctx))
    return inst_list;

  const char *arg_reg64 = current_arg_reg64(0);
  if (arg_reg64 == NULL)
    return inst_list;

  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64,
           arg_reg64);
  inst_list = add_inst(inst_list, buffer);

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list =
      codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_to_string");
  snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", RETURN_REG_64,
           value_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);
  free_arg_regs();
  return inst_list;
}

static ListNode_t *promote_wide_operand_to_string(expr_node_t *node,
                                                  ListNode_t *inst_list,
                                                  CodeGenContext *ctx,
                                                  Register_t *value_reg) {
  if (node == NULL || node->expr == NULL || ctx == NULL || value_reg == NULL)
    return inst_list;

  if (!expr_tree_node_is_wide_string(node))
    return inst_list;

  const char *arg_reg64 = current_arg_reg64(0);
  if (arg_reg64 == NULL)
    return inst_list;

  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64,
           arg_reg64);
  inst_list = add_inst(inst_list, buffer);
  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list,
                                             "kgpc_string_from_unicodestring");
  snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", value_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);
  free_arg_regs();
  return inst_list;
}

static ListNode_t *promote_operand_to_unicodestring(expr_node_t *node,
                                                    ListNode_t *inst_list,
                                                    CodeGenContext *ctx,
                                                    Register_t *value_reg) {
  if (node == NULL || ctx == NULL || value_reg == NULL)
    return inst_list;

  if (!expr_tree_node_is_wide_string(node)) {
    inst_list = promote_char_operand_to_string(node, inst_list, ctx, value_reg);
    inst_list =
        promote_shortstring_operand_to_string(node, inst_list, ctx, value_reg);

    const char *arg_reg64 = current_arg_reg64(0);
    if (arg_reg64 != NULL) {
      char buffer[128];
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64,
               arg_reg64);
      inst_list = add_inst(inst_list, buffer);
      inst_list = codegen_vect_reg(inst_list, 0);
      inst_list = codegen_call_with_shadow_space(
          inst_list, "kgpc_unicodestring_from_string");
      snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n",
               value_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      free_arg_regs();
    }
  }
  return inst_list;
}

ListNode_t *promote_shortstring_reg_operand(ListNode_t *inst_list,
                                            CodeGenContext *ctx,
                                            const char *value_operand,
                                            const Register_t *value_reg) {
  if (inst_list == NULL || ctx == NULL || value_reg == NULL)
    return inst_list;
  (void)value_operand;

  const char *arg_reg64 = current_arg_reg64(0);
  if (arg_reg64 == NULL)
    return inst_list;

  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64,
           arg_reg64);
  inst_list = add_inst(inst_list, buffer);
  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list =
      codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_to_string");
  snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", value_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);
  free_arg_regs();
  return inst_list;
}

static ListNode_t *spill_reg64_operand(ListNode_t *inst_list,
                                       const char *reg_operand,
                                       StackNode_t **spill_slot,
                                       const char *temp_name);
static ListNode_t *restore_spilled_reg64_operand(ListNode_t *inst_list,
                                                 const char *reg_operand,
                                                 StackNode_t *spill_slot);
ListNode_t *
promote_shortstring_operand_ex(ListNode_t *inst_list, CodeGenContext *ctx,
                               const char **operand_ptr, Register_t **reg_ptr,
                               OperandKind *kind_ptr, const char *other_operand,
                               const Register_t *other_reg) {
  if (inst_list == NULL || ctx == NULL || operand_ptr == NULL ||
      *operand_ptr == NULL || reg_ptr == NULL || kind_ptr == NULL)
    return inst_list;

  Register_t *value_reg = *reg_ptr;
  if (value_reg == NULL) {
    value_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (value_reg == NULL)
      return inst_list;
    if (*kind_ptr == OPKIND_MEMORY || *kind_ptr == OPKIND_LABEL) {
      char buffer[128];
      snprintf(buffer, sizeof(buffer), "\tleaq\t%s, %s\n", *operand_ptr,
               value_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
    } else {
      inst_list = emit_move_ptr_operand_kind(inst_list, *operand_ptr, NULL,
                                             *kind_ptr, value_reg->bit_64);
    }
    *operand_ptr = value_reg->bit_64;
    *reg_ptr = value_reg;
    *kind_ptr = OPKIND_REGISTER;
  }

  StackNode_t *other_save = NULL;
  if (other_reg != NULL)
    inst_list = spill_reg64_operand(inst_list, other_reg->bit_64, &other_save,
                                    "relop_shortstring_other");
  else if (other_operand != NULL && other_operand[0] == '%')
    inst_list = spill_reg64_operand(inst_list, other_operand, &other_save,
                                    "relop_shortstring_other");

  inst_list =
      promote_shortstring_reg_operand(inst_list, ctx, *operand_ptr, value_reg);
  *operand_ptr = value_reg->bit_64;
  *reg_ptr = value_reg;
  *kind_ptr = OPKIND_REGISTER;

  if (other_reg != NULL)
    inst_list =
        restore_spilled_reg64_operand(inst_list, other_reg->bit_64, other_save);
  else if (other_operand != NULL && other_operand[0] == '%')
    inst_list =
        restore_spilled_reg64_operand(inst_list, other_operand, other_save);
  return inst_list;
}

static ListNode_t *spill_reg64_operand(ListNode_t *inst_list,
                                       const char *reg_operand,
                                       StackNode_t **spill_slot,
                                       const char *temp_name) {
  if (spill_slot != NULL)
    *spill_slot = NULL;
  if (spill_slot == NULL || reg_operand == NULL || temp_name == NULL)
    return inst_list;

  StackNode_t *slot = add_l_t((char *)temp_name);
  if (slot == NULL)
    return inst_list;

  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", reg_operand,
           slot->offset);
  inst_list = add_inst(inst_list, buffer);
  *spill_slot = slot;
  return inst_list;
}

static ListNode_t *restore_spilled_reg64_operand(ListNode_t *inst_list,
                                                 const char *reg_operand,
                                                 StackNode_t *spill_slot) {
  if (reg_operand == NULL || spill_slot == NULL)
    return inst_list;

  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
           spill_slot->offset, reg_operand);
  return add_inst(inst_list, buffer);
}

static ListNode_t *promote_char_reg_operand_to_string(ListNode_t *inst_list,
                                                      const char *reg_operand,
                                                      const Register_t *reg) {
  if (reg_operand == NULL || reg == NULL)
    return inst_list;

  const char *arg_reg32 = current_arg_reg32(0);
  const char *value_reg32 = reg_to_reg32(reg_operand, reg);
  if (arg_reg32 == NULL || value_reg32 == NULL)
    return inst_list;

  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", value_reg32, arg_reg32);
  inst_list = add_inst(inst_list, buffer);
  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
  snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", RETURN_REG_64,
           reg->bit_64);
  inst_list = add_inst(inst_list, buffer);
  free_arg_regs();
  return inst_list;
}

/* Promote a char operand (possibly an immediate like "$46") to an AnsiString
 * via kgpc_char_to_string.  When the operand is an immediate with no backing
 * register, a new register is allocated and the result is stored there.
 * The caller's operand, register, and kind pointers are updated in-place. */
ListNode_t *promote_char_operand_to_string_ex(ListNode_t *inst_list,
                                              const char **operand_ptr,
                                              Register_t **reg_ptr,
                                              OperandKind *kind_ptr,
                                              const char *other_operand,
                                              const Register_t *other_reg) {
  assert(operand_ptr != NULL && reg_ptr != NULL && kind_ptr != NULL);

  if (*reg_ptr != NULL) {
    /* Operand is already in a register — use existing path. */
    StackNode_t *other_save = NULL;
    if (other_reg != NULL)
      inst_list = spill_reg64_operand(inst_list, other_reg->bit_64, &other_save,
                                      "relop_charpromo_save");
    inst_list =
        promote_char_reg_operand_to_string(inst_list, *operand_ptr, *reg_ptr);
    inst_list = restore_spilled_reg64_operand(
        inst_list, other_reg != NULL ? other_reg->bit_64 : NULL, other_save);
    return inst_list;
  }

  /* Operand is an immediate (e.g. "$46") — materialize into a register. */
  assert(*operand_ptr != NULL);

  StackNode_t *other_save = NULL;
  if (other_reg != NULL)
    inst_list = spill_reg64_operand(inst_list, other_reg->bit_64, &other_save,
                                    "relop_charpromo_imm_save");

  const char *arg_reg32 = current_arg_reg32(0);
  if (arg_reg32 == NULL) {
    inst_list = restore_spilled_reg64_operand(
        inst_list, other_reg != NULL ? other_reg->bit_64 : NULL, other_save);
    return inst_list;
  }

  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", *operand_ptr, arg_reg32);
  inst_list = add_inst(inst_list, buffer);
  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
  free_arg_regs();

  Register_t *result_reg = get_free_reg(get_reg_stack(), &inst_list);
  if (result_reg == NULL) {
    inst_list =
        restore_spilled_reg64_operand(inst_list, other_operand, other_save);
    return inst_list;
  }

  snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", RETURN_REG_64,
           result_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);

  *operand_ptr = result_reg->bit_64;
  *reg_ptr = result_reg;
  *kind_ptr = OPKIND_REGISTER;

  inst_list = restore_spilled_reg64_operand(
      inst_list, other_reg != NULL ? other_reg->bit_64 : NULL, other_save);
  return inst_list;
}

/* Move a pointer-sized operand into a destination register, using the
   appropriate instruction based on the semantic kind of the operand. */
ListNode_t *emit_move_ptr_operand_kind(ListNode_t *inst_list, const char *src,
                                       const Register_t *src_reg,
                                       OperandKind kind, const char *dst) {
  if (inst_list == NULL || src == NULL || dst == NULL)
    return inst_list;
  char buffer[128];
  switch (kind) {
  case OPKIND_REGISTER:
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
             src_reg != NULL ? src_reg->bit_64 : src, dst);
    break;
  case OPKIND_IMMEDIATE:
  case OPKIND_MEMORY:
    /* Both immediates ($val) and memory refs (-N(%rbp)) use movq */
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", src, dst);
    break;
  case OPKIND_LABEL:
    /* RIP-relative labels need leaq to load the address */
    snprintf(buffer, sizeof(buffer), "\tleaq\t%s, %s\n", src, dst);
    break;
  }
  return add_inst(inst_list, buffer);
}

static ListNode_t *gencode_string_concat(expr_node_t *node,
                                         ListNode_t *inst_list,
                                         CodeGenContext *ctx,
                                         Register_t *target_reg) {

  if (node == NULL || node->left_expr == NULL || node->right_expr == NULL)
    return inst_list;

  char buffer[128];
  int result_is_wide = expr_tree_node_is_wide_string(node);

  Register_t *rhs_reg = get_free_reg(get_reg_stack(), &inst_list);

  if (rhs_reg == NULL) {

    StackNode_t *spill_loc = add_l_t("str_concat_rhs");
    inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);
    if (result_is_wide)
      inst_list = promote_operand_to_unicodestring(node->right_expr, inst_list,
                                                   ctx, target_reg);
    else {
      inst_list = promote_char_operand_to_string(node->right_expr, inst_list,
                                                 ctx, target_reg);
      inst_list = promote_shortstring_operand_to_string(
          node->right_expr, inst_list, ctx, target_reg);
      inst_list = promote_wide_operand_to_string(node->right_expr, inst_list,
                                                 ctx, target_reg);
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
             target_reg->bit_64, spill_loc->offset);
    inst_list = add_inst(inst_list, buffer);

    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
    if (result_is_wide)
      inst_list = promote_operand_to_unicodestring(node->left_expr, inst_list,
                                                   ctx, target_reg);
    else {
      inst_list = promote_char_operand_to_string(node->left_expr, inst_list,
                                                 ctx, target_reg);
      inst_list = promote_shortstring_operand_to_string(
          node->left_expr, inst_list, ctx, target_reg);
      inst_list = promote_wide_operand_to_string(node->left_expr, inst_list,
                                                 ctx, target_reg);
    }

    if (codegen_target_is_windows()) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n",
               target_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n",
               spill_loc->offset);
      inst_list = add_inst(inst_list, buffer);
    } else {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n",
               target_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rsi\n",
               spill_loc->offset);
      inst_list = add_inst(inst_list, buffer);
    }
  } else {
    StackNode_t *lhs_spill = add_l_t("str_concat_lhs");
    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
    if (result_is_wide)
      inst_list = promote_operand_to_unicodestring(node->left_expr, inst_list,
                                                   ctx, target_reg);
    else {
      inst_list = promote_char_operand_to_string(node->left_expr, inst_list,
                                                 ctx, target_reg);
      inst_list = promote_shortstring_operand_to_string(
          node->left_expr, inst_list, ctx, target_reg);
      inst_list = promote_wide_operand_to_string(node->left_expr, inst_list,
                                                 ctx, target_reg);
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
             target_reg->bit_64, lhs_spill->offset);
    inst_list = add_inst(inst_list, buffer);

    inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, rhs_reg);
    if (result_is_wide)
      inst_list = promote_operand_to_unicodestring(node->right_expr, inst_list,
                                                   ctx, rhs_reg);
    else {
      inst_list = promote_char_operand_to_string(node->right_expr, inst_list,
                                                 ctx, rhs_reg);
      inst_list = promote_shortstring_operand_to_string(
          node->right_expr, inst_list, ctx, rhs_reg);
      inst_list = promote_wide_operand_to_string(node->right_expr, inst_list,
                                                 ctx, rhs_reg);
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
             lhs_spill->offset, target_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    if (codegen_target_is_windows()) {
      // For chained concatenations, we need to be careful about register usage
      // Move the second argument to RDX first, then the first argument to RCX
      // This prevents overwriting the second argument when target_reg is reused
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", rhs_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n",
               target_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
    } else {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", rhs_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n",
               target_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
    }
  }

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(
      inst_list,
      result_is_wide ? "kgpc_unicodestring_concat" : "kgpc_string_concat");
  snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", target_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);

  if (rhs_reg != NULL) {
    free_reg(get_reg_stack(), rhs_reg);
  }
  free_arg_regs();
  return inst_list;
}

/* Gencode for modulus */
// left is right operand (B), right is left operand (A)
// calculates A mod B, stores result in A's location (right)
ListNode_t *gencode_modulus(const char *left, const char *right,
                            ListNode_t *inst_list) {
  StackNode_t *temp;
  char buffer[128];
  const char *div_operand = left;
  char div_buf[64];

  assert(left != NULL);
  assert(right != NULL);
  /* Note: inst_list can be NULL at the start of code generation */

  // If divisor (B, left) is a constant, move it to memory
  if (left[0] == '$') {
    temp = find_in_temp("TEMP_MOD");
    if (temp == NULL)
      temp = add_l_t("TEMP_MOD");
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", left,
             temp->offset);
    inst_list = add_inst(inst_list, buffer);
    snprintf(div_buf, sizeof(div_buf), "-%d(%%rbp)", temp->offset);
    div_operand = div_buf;
  } else // Divisor is a register
  {
    const char *tmp_div = select_divisor_temp_reg(NULL, 0);
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", left, tmp_div);
    inst_list = add_inst(inst_list, buffer);
    div_operand = tmp_div;
  }

  // Move dividend (A, right) to eax
  snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%eax\n", right);
  inst_list = add_inst(inst_list, buffer);

  // Sign extend eax to edx
  snprintf(buffer, sizeof(buffer), "\tcltd\n");
  inst_list = add_inst(inst_list, buffer);

  snprintf(buffer, sizeof(buffer), "\tidivl\t%s\n", div_operand);
  inst_list = add_inst(inst_list, buffer);

  // Move remainder from edx to the target register (A's location, right)
  snprintf(buffer, sizeof(buffer), "\tmovl\t%%edx, %s\n", right);
  inst_list = add_inst(inst_list, buffer);

  return inst_list;
}

/* Checks if node is a leaf */
int expr_tree_is_leaf(expr_node_t *node) {
  assert(node != NULL);

  if (node->left_expr == NULL && node->right_expr == NULL)
    return 1;

  return 0;
}

/* Prints an expression tree */
void print_expr_tree(expr_node_t *node, int num_indent, FILE *f) {
  assert(node != NULL);
  assert(node->expr != NULL);
  assert(f != NULL);
  int i;

  for (i = 0; i < num_indent; ++i)
    fprintf(f, "  ");

  fprintf(f, "[NODE:%d, L:%d]\n", node->expr->type, node->label);
  if (node->left_expr != NULL) {
    for (i = 0; i < num_indent; ++i)
      fprintf(f, "  ");

    fprintf(f, "[LEFT]\n");
    print_expr_tree(node->left_expr, num_indent + 1, f);
  }
  if (node->right_expr != NULL) {
    for (i = 0; i < num_indent; ++i)
      fprintf(f, "  ");

    fprintf(f, "[RIGHT]\n");
    print_expr_tree(node->right_expr, num_indent + 1, f);
  }
}

/* Frees an allocated expression tree */
void free_expr_tree(expr_node_t *node) {
  if (node != NULL) {
    if (node->reg != NULL &&
        node->reg->spill_callback == expr_tree_register_spill_handler &&
        node->reg->spill_context == node) {
      register_clear_spill_callback(node->reg);
    }
    free_expr_tree(node->left_expr);
    free_expr_tree(node->right_expr);
    free(node);
  }
}

/* Special case for a sign term */
ListNode_t *gencode_sign_term(expr_node_t *node, ListNode_t *inst_list,
                              CodeGenContext *ctx, Register_t *target_reg) {
  assert(node != NULL);
  assert(node->expr != NULL);
  assert(node->expr->type == EXPR_SIGN_TERM);
  assert(ctx != NULL);
  assert(target_reg != NULL);

  char buffer[256];

  inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

  int type_tag = expr_get_type_tag(node->expr);
  const int use_qword =
      expr_uses_qword_kgpctype(node->expr) || codegen_type_uses_qword(type_tag);
  const char *dest = select_register_name(target_reg, node->expr, type_tag);
  if (type_tag == REAL_TYPE) {
    inst_list =
        gencode_real_negate(dest, target_reg, dest, target_reg, inst_list);
  } else {
    snprintf(buffer, sizeof(buffer), "\tneg%s\t%s\n", use_qword ? "q" : "l",
             dest);
    inst_list = add_inst(inst_list, buffer);
    /* After 32-bit negation, sign-extend to 64 bits so the value is
     * correct when later used in a 64-bit context (e.g. passed as Int64
     * argument via movq). Writing a 32-bit register zeroes the upper
     * half, so negl of a positive value leaves e.g. 0x00000000FFFFFFFC
     * instead of the expected 0xFFFFFFFFFFFFFFFC for -4. */
    if (!use_qword) {
      snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", target_reg->bit_32,
               target_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
    }
  }

  return inst_list;
}

/* node is a leaf */
ListNode_t *gencode_case0(expr_node_t *node, ListNode_t *inst_list,
                          CodeGenContext *ctx, Register_t *target_reg) {
#ifdef DEBUG_CODEGEN
  fprintf(stderr, "gencode_case0\n");
#endif
  assert(node != NULL);
  assert(node->expr != NULL);
  assert(ctx != NULL);
  assert(target_reg != NULL);

  /* Buffer must be large enough for very long mangled identifiers (dozens of
   * suffixes). */
  char buffer[CODEGEN_MAX_INST_BUF];
  char buf_leaf[128];
  struct Expression *expr;
  int narrow_signed_size = 2;

  expr = node->expr;
  assert(target_reg != NULL);

  CODEGEN_DEBUG("DEBUG gencode_case0: expr->type=%d\n", expr->type);

  if (expr->type == EXPR_FUNCTION_CALL) {
    const char *func_mangled_name =
        expr->expr_data.function_call_data.mangled_id;
    const char *func_id = expr->expr_data.function_call_data.id;

    /* Snapshot the pending constructor-temp list BEFORE this call's
     * own push (the constructor's alloc-site push happens later in
     * this block).  After the call returns, any entries pushed by
     * argument evaluation between (snapshot + this-call's own push)
     * and the call site are discarded — they were passed to the
     * callee, which may have stored them (a nested constructor's
     * field-init argument, an external proc that takes ownership,
     * etc.).  We cannot prove they remain transient, so we err on
     * the side of leaving them alive to avoid use-after-free. */
    int ctor_pending_snapshot_before_push =
        (ctx != NULL) ? ctx->pending_ctor_temp_count : 0;

    if (func_id != NULL && (pascal_identifier_equals(func_id, "Low") ||
                            pascal_identifier_equals(func_id, "High"))) {
      char *owned_call_target = NULL;
      const char *call_target =
          codegen_resolve_function_call_target(ctx, expr, &owned_call_target);
      int is_builtin_lowhigh =
          (call_target != NULL &&
           (pascal_identifier_equals(call_target, "Low") ||
            pascal_identifier_equals(call_target, "High")));
      int type_ident_arg = codegen_lowhigh_arg_is_type_identifier(expr, ctx);
      if ((call_target == NULL &&
           expr->expr_data.function_call_data.call_kgpc_type == NULL &&
           expr->expr_data.function_call_data.resolved_func == NULL) ||
          is_builtin_lowhigh || type_ident_arg) {
        ListNode_t *lowered = codegen_builtin_lowhigh_fallback(
            expr, inst_list, ctx, target_reg,
            pascal_identifier_equals(func_id, "High"));
        if (owned_call_target != NULL)
          free(owned_call_target);
        if (lowered != NULL)
          return lowered;
      }
      if (owned_call_target != NULL)
        free(owned_call_target);
    }

    if (func_id != NULL && pascal_identifier_equals(func_id, "New")) {
      ListNode_t *lowered =
          codegen_builtin_new_function_call(expr, inst_list, ctx, target_reg);
      if (lowered != NULL)
        return lowered;
    }

    if (func_id != NULL && pascal_identifier_equals(func_id, "Length")) {
      ListNode_t *lowered = codegen_builtin_length_type_fallback(
          expr, inst_list, ctx, target_reg);
      if (lowered != NULL)
        return lowered;
    }

    /* PopCnt builtin: redirect to the runtime's fpc_in_popcnt_x */
    if (func_id != NULL && pascal_identifier_equals(func_id, "PopCnt"))
      func_mangled_name = "fpc_in_popcnt_x";

    if (expr->expr_data.function_call_data.call_kgpc_type != NULL &&
        expr->expr_data.function_call_data.call_kgpc_type->kind ==
            TYPE_KIND_PROCEDURE &&
        expr->expr_data.function_call_data.call_kgpc_type->info.proc_info
                .definition != NULL) {
      Tree_t *def = expr->expr_data.function_call_data.call_kgpc_type->info
                        .proc_info.definition;
      const char *alias = def->tree_data.subprogram_data.cname_override;
      if (alias != NULL && alias[0] != '\0')
        func_mangled_name = alias;
      else if (def->tree_data.subprogram_data.mangled_id != NULL &&
               def->tree_data.subprogram_data.mangled_id[0] != '\0')
        func_mangled_name = def->tree_data.subprogram_data.mangled_id;
    }
    CODEGEN_DEBUG("DEBUG FUNCTION_CALL: mangled=%s, id=%s\n",
                  func_mangled_name ? func_mangled_name : "NULL",
                  func_id ? func_id : "NULL");

    if (func_mangled_name != NULL &&
        strcmp(func_mangled_name, "__kgpc_dynarray_length") == 0) {
      inst_list =
          codegen_builtin_dynarray_length(expr, inst_list, ctx, target_reg);
      // NOTE: Don't free mangled_id here - it will be freed when the AST is
      // destroyed codegen_release_function_call_mangled_id(expr);
      return inst_list;
    }

    if (func_id != NULL && strcasecmp(func_id, "SwapEndian") == 0) {
      ListNode_t *args = expr->expr_data.function_call_data.args_expr;
      if (args == NULL || args->cur == NULL) {
        codegen_report_error(
            ctx, "ERROR: SwapEndian intrinsic expects one argument.");
        return inst_list;
      }

      struct Expression *arg_expr = (struct Expression *)args->cur;
      expr_node_t *arg_tree = build_expr_tree(arg_expr);
      if (arg_tree != NULL) {
        inst_list = gencode_expr_tree(arg_tree, inst_list, ctx, target_reg);
        free_expr_tree(arg_tree);
      }

      int use_qword = codegen_type_uses_qword(expr_get_type_tag(expr));
      long long arg_size = 4;
      if (arg_expr->resolved_kgpc_type != NULL)
        arg_size = kgpc_type_sizeof(arg_expr->resolved_kgpc_type);
      else if (use_qword)
        arg_size = 8;

      if (arg_size == 2) {
        /* 16-bit byte swap: rolw $8, %reg16 then zero-extend to avoid bswapl
         * clobbering upper bits */
        const char *reg16 = codegen_register_name16(target_reg);
        if (reg16 != NULL) {
          snprintf(buffer, sizeof(buffer), "\trolw\t$8, %s\n", reg16);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tmovzwl\t%s, %s\n", reg16,
                   target_reg->bit_32);
          inst_list = add_inst(inst_list, buffer);
        } else {
          /* fallback: shifts */
          snprintf(buffer, sizeof(buffer), "\trolw\t$8, %s\n",
                   target_reg->bit_32);
          inst_list = add_inst(inst_list, buffer);
          snprintf(buffer, sizeof(buffer), "\tandl\t$65535, %s\n",
                   target_reg->bit_32);
          inst_list = add_inst(inst_list, buffer);
        }
      } else {
        const char *swap_reg =
            use_qword ? target_reg->bit_64 : target_reg->bit_32;
        char swap_suffix = use_qword ? 'q' : 'l';
        snprintf(buffer, sizeof(buffer), "\tbswap%c\t%s\n", swap_suffix,
                 swap_reg);
        inst_list = add_inst(inst_list, buffer);
      }
      return inst_list;
    }

    /* For function calls, get the KgpcType from cached call info populated
     * during semcheck. Fall back to a fresh symbol lookup when metadata is
     * unavailable. IMPORTANT: If is_call_info_valid is true, respect that even
     * if call_kgpc_type is NULL. This allows builtins (like UpCase(char)) to
     * signal that no formal parameter conversion is needed by setting
     * is_call_info_valid=1 with call_kgpc_type=NULL. */
    HashNode_t *func_node = NULL;
    struct KgpcType *func_type =
        codegen_resolve_function_call_type(ctx, expr, &func_node);
    if (expr->expr_data.function_call_data.is_call_info_valid &&
        func_type == NULL && kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL) {
      fprintf(stderr,
              "[CodeGen] expr_tree: is_call_info_valid=1 but call_kgpc_type is "
              "NULL for id='%s'\n",
              expr->expr_data.function_call_data.id
                  ? expr->expr_data.function_call_data.id
                  : "(null)");
    } else if (!expr->expr_data.function_call_data.is_call_info_valid &&
               func_type == NULL && kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL) {
      fprintf(stderr,
              "[CodeGen] expr_tree: func_type lookup FAILED for id='%s' "
              "mangled='%s'\n",
              expr->expr_data.function_call_data.id
                  ? expr->expr_data.function_call_data.id
                  : "(null)",
              expr->expr_data.function_call_data.mangled_id
                  ? expr->expr_data.function_call_data.mangled_id
                  : "(null)");
    }

    /* Check if the function being called requires a static link.
     * Note: KGPC's calling convention uses an implicit first argument (static
     * link) for normal Pascal functions/procedures, so we key off semantic
     * metadata when available. */
    int callee_depth = 0;
    int have_depth =
        codegen_proc_static_link_depth(ctx, func_mangled_name, &callee_depth);
    int current_depth = codegen_get_lexical_depth(ctx);
    /* If the codegen registry doesn't know the callee yet (e.g. nested
     * sibling not yet emitted), fall back to the semantic definition's
     * nesting_level. This avoids defaulting to STATIC_LINK_FROM_RBP
     * (== caller's own rbp), which is wrong for sibling calls — the
     * sibling needs the *parent's* frame, which the caller received as
     * its own static link. */
    if (!have_depth && func_type != NULL &&
        func_type->kind == TYPE_KIND_PROCEDURE &&
        func_type->info.proc_info.definition != NULL) {
      int sem_nesting = func_type->info.proc_info.definition->tree_data
                            .subprogram_data.nesting_level;
      if (sem_nesting > 0) {
        /* nesting_level: 1 = top-level (depth 1 in codegen), 2 = nested once,
         * ... */
        callee_depth = sem_nesting;
        have_depth = 1;
      }
    }
    /* Determine whether the callee requires a static link. Check, in order:
     * 1. The HashNode flag (set by semcheck after the callee's body is
     * processed).
     * 2. The Tree node behind the kgpc_type (also set by semcheck — survives
     *    when codegen lookup returns a HashNode that hasn't been refreshed).
     * 3. The codegen registry (populated only after we emit the callee's
     *    prologue — useful when both caller and callee are nested and the
     *    callee was emitted first).
     * Reading the Tree flag is critical for nested-sibling calls where the
     * caller is codegen'd before the callee: at that point neither the
     * HashNode lookup nor the codegen registry yet reflect the callee's
     * static-link requirement, but the Tree does. */
    int tree_requires_static_link = 0;
    if (func_type != NULL && func_type->kind == TYPE_KIND_PROCEDURE &&
        func_type->info.proc_info.definition != NULL) {
      Tree_t *callee_def = func_type->info.proc_info.definition;
      /* The callee receives a static link if its prologue reserves one,
       * which happens when either flag is set (see
       * codegen_subprograms.c:will_need_static_link). The caller must
       * mirror that decision exactly so caller-pass and callee-receive
       * stay in sync. */
      tree_requires_static_link =
          callee_def->tree_data.subprogram_data.requires_static_link ||
          (callee_def->tree_data.subprogram_data.is_nested &&
           callee_def->tree_data.subprogram_data.has_nested_requiring_link);
    }
    int should_pass_static_link =
        (func_node != NULL && func_node->requires_static_link) ||
        tree_requires_static_link ||
        codegen_proc_requires_static_link(ctx, func_mangled_name);

    enum {
      STATIC_LINK_NONE = 0,
      STATIC_LINK_FROM_RBP,
      STATIC_LINK_FROM_SLOT,
      STATIC_LINK_FROM_REG
    } static_link_source = STATIC_LINK_NONE;
    int static_link_slot_offset = 0;
    Register_t *static_link_reg = NULL;
    int static_link_expr_active = 0;

    if (should_pass_static_link) {
      if (!have_depth) {
        static_link_source = STATIC_LINK_FROM_RBP;
      } else if (callee_depth > current_depth) {
        static_link_source = STATIC_LINK_FROM_RBP;
      } else if (callee_depth == current_depth) {
        StackNode_t *static_link_node = find_label("__static_link__");
        if (static_link_node != NULL) {
          static_link_source = STATIC_LINK_FROM_SLOT;
          static_link_slot_offset = static_link_node->offset;
        }
      } else {
        static_link_source = STATIC_LINK_FROM_REG;
        int levels_to_traverse = (current_depth - callee_depth) + 1;
        codegen_begin_expression(ctx);
        static_link_expr_active = 1;
        static_link_reg =
            codegen_acquire_static_link(ctx, &inst_list, levels_to_traverse);
      }
    }

    /* Check if this is a constructor call (e.g., TMyClass.Create)
     * Constructors need special handling: allocate memory and initialize VMT */
    int is_constructor = expr->expr_data.function_call_data.is_constructor_call;
    Register_t *constructor_instance_reg = NULL;
    StackNode_t *constructor_instance_slot = NULL;

    /* Record static factories (e.g., TGUID.Create) can also be named Create
     * but they are not class constructors and must not use constructor
     * calling paths.  Without this guard the SRET buffer for large record
     * returns is never set up, leaving %%rdi uninitialised. */
    if (is_constructor && expr_has_type_tag(expr, RECORD_TYPE))
      is_constructor = 0;
    if (is_constructor && func_type != NULL &&
        kgpc_type_is_procedure(func_type)) {
      KgpcType *ret_type = kgpc_type_get_return_type(func_type);
      /* If the return type is explicitly known and is NOT a class pointer,
       * then this is a record static factory, not a real constructor.
       * When ret_type is NULL (e.g., actual constructors), keep is_constructor
       * since constructors don't declare an explicit return type. */
      if (ret_type != NULL &&
          (!kgpc_type_is_pointer(ret_type) ||
           ret_type->info.points_to == NULL ||
           !kgpc_type_is_record(ret_type->info.points_to) ||
           !record_type_is_class(ret_type->info.points_to->info.record_info))) {
        is_constructor = 0;
      }
    }

    /* Avoid infinite recursion when a constructor ends up calling itself (e.g.,
     * inherited calls with no parent implementation). In that case, just reuse
     * Self instead of re-entering the constructor. */
    if (is_constructor && func_mangled_name != NULL &&
        ctx->current_subprogram_mangled != NULL &&
        strcmp(func_mangled_name, ctx->current_subprogram_mangled) == 0) {
      StackNode_t *self_slot = find_label("Self");
      if (self_slot != NULL && target_reg != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 self_slot->offset, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        return inst_list;
      }
    }

    /* Constructors for classes return the constructed instance by value,
     * which uses a hidden sret pointer in the first argument slot. */
    int force_scalar_string_return =
        expr->expr_data.function_call_data.builtin_call_lowering ==
            BUILTIN_CALL_STRPAS &&
        !expr->expr_data.function_call_data.is_procedural_var_call;
    int has_record_return = expr_returns_sret(expr);
    if (force_scalar_string_return)
      has_record_return = 0;
    if (!force_scalar_string_return && !has_record_return && is_constructor &&
        expr_tree_constructor_owner_is_plain_object(ctx, expr)) {
      has_record_return = 1;
    }
    if (!force_scalar_string_return && !has_record_return &&
        func_type != NULL && func_type->kind == TYPE_KIND_PROCEDURE) {
      KgpcType *ret_type = kgpc_type_get_return_type(func_type);
      if (ret_type != NULL && (kgpc_type_is_shortstring(ret_type) ||
                               (ret_type->type_alias != NULL &&
                                ret_type->type_alias->is_shortstring))) {
        has_record_return = 1;
      } else if (func_type->info.proc_info.definition != NULL &&
                 func_type->info.proc_info.definition->tree_data.subprogram_data
                         .return_type == SHORTSTRING_TYPE) {
        has_record_return = 1;
      } else if (expr_tree_virtual_call_returns_shortstring(ctx, expr)) {
        has_record_return = 1;
      }
    } else if (!force_scalar_string_return && !has_record_return &&
               expr_tree_virtual_call_returns_shortstring(ctx, expr)) {
      has_record_return = 1;
    }
    /* Fallback for proc-var calls returning a non-shortstring record.
     * codegen_expr_sret_size() has no ctx, so it can't resolve return_type_id
     * through the symbol table.  Do it here where ctx is available. */
    long long procvar_sret_size = 0;
    /* Fast path: semcheck cached the sret size directly — avoids pointer
     * validity issues when the allocator zeroes freed KgpcType memory. */
    if (!has_record_return && !force_scalar_string_return &&
        expr->expr_data.function_call_data.is_procedural_var_call &&
        expr->expr_data.function_call_data.cached_procvar_sret_size > 8) {
      has_record_return = 1;
      procvar_sret_size =
          expr->expr_data.function_call_data.cached_procvar_sret_size;
    }
    if (!has_record_return && !force_scalar_string_return &&
        expr->expr_data.function_call_data.is_procedural_var_call) {
      struct Expression *pve =
          expr->expr_data.function_call_data.procedural_var_expr;
      if (pve != NULL) {
        KgpcType *pv_type = expr_tree_proc_type_from_record_field(ctx, pve);
        if (pv_type == NULL)
          pv_type = expr_get_kgpc_type(pve);
        if (pv_type != NULL && pv_type->kind == TYPE_KIND_POINTER &&
            pv_type->info.points_to != NULL)
          pv_type = pv_type->info.points_to;
        if (pv_type != NULL && pv_type->kind == TYPE_KIND_PROCEDURE) {
          KgpcType *pv_ret = kgpc_type_get_return_type(pv_type);
          /* Also fall back when pv_ret is a stale freed pointer —
           * MALLOC_PERTURB_ fills freed memory so kind != TYPE_KIND_RECORD
           * but the pointer is non-NULL (bare MSYS2 UAF). */
          if (!kgpc_type_is_record(pv_ret) &&
              pv_type->info.proc_info.return_type_id != NULL && ctx != NULL &&
              ctx->symtab != NULL) {
            HashNode_t *ret_sym = NULL;
            if (FindSymbol(&ret_sym, ctx->symtab,
                           pv_type->info.proc_info.return_type_id) != 0 &&
                ret_sym != NULL)
              pv_ret = ret_sym->type;
          }
          if (pv_ret != NULL && kgpc_type_is_record(pv_ret)) {
            has_record_return = 1;
            procvar_sret_size = kgpc_type_sizeof(pv_ret);
            if (procvar_sret_size <= 0)
              procvar_sret_size = 2 * CODEGEN_POINTER_SIZE_BYTES;
          }
        }
      }
      /* call_kgpc_type is set by semcheck field detection and holds the proc
       * type with return_type_id even when resolved_kgpc_type was overwritten
       * (e.g. mgr.GetStatus().Name on MSYS overwrites the inner call node's
       * resolved_kgpc_type to String, masking the record return). */
      if (!has_record_return) {
        KgpcType *ck = expr->expr_data.function_call_data.call_kgpc_type;
        if (ck != NULL && ck->kind == TYPE_KIND_POINTER &&
            ck->info.points_to != NULL)
          ck = ck->info.points_to;
        if (ck != NULL && ck->kind == TYPE_KIND_PROCEDURE) {
          KgpcType *ck_ret = kgpc_type_get_return_type(ck);
          /* Fall back to symbol-table lookup when ck_ret is NULL or is a
           * stale freed pointer (MALLOC_PERTURB_ fills freed memory so
           * kind != TYPE_KIND_RECORD but pointer is non-NULL, bare MSYS2). */
          if (!kgpc_type_is_record(ck_ret) &&
              ck->info.proc_info.return_type_id != NULL && ctx != NULL &&
              ctx->symtab != NULL) {
            HashNode_t *ret_sym = NULL;
            if (FindSymbol(&ret_sym, ctx->symtab,
                           ck->info.proc_info.return_type_id) != 0 &&
                ret_sym != NULL)
              ck_ret = ret_sym->type;
          }
          if (ck_ret != NULL && kgpc_type_is_record(ck_ret)) {
            has_record_return = 1;
            procvar_sret_size = kgpc_type_sizeof(ck_ret);
            if (procvar_sret_size <= 0)
              procvar_sret_size = 2 * CODEGEN_POINTER_SIZE_BYTES;
          }
        }
      }
      /* Last-resort: check RecordField->cached_proc_return_sret_size, which is
       * stored in the AST (not a KgpcType) and survives allocator zeroing on
       * bare MSYS2.  All KgpcType-based paths above can fail when proc_type and
       * call_kgpc_type are both freed between semcheck and codegen. */
      if (!has_record_return && pve != NULL &&
          pve->type == EXPR_RECORD_ACCESS) {
        struct RecordField *rf = codegen_lookup_record_field_expr(pve, ctx);
        if (rf != NULL && rf->cached_proc_return_sret_size > 8) {
          has_record_return = 1;
          procvar_sret_size = rf->cached_proc_return_sret_size;
        }
      }
    }
    /* Write back sret size to RecordField cache so that a later call to the
     * same proc-var field (e.g. second mgr.GetStatus() call) can use Fix 3
     * when proc KgpcType is freed between the two codegen traversals.
     * Three fallback sizes in priority order:
     *  1. procvar_sret_size: set by pv_type/call_kgpc_type check above
     *  2. cached_procvar_sret_size: set by semcheck (on the AST node)
     *  3. codegen_expr_sret_size: resolves via retained call_kgpc_type when
     *     semcheck didn't populate cached_procvar_sret_size (proc_type NULL).
     */
    if (has_record_return &&
        expr->expr_data.function_call_data.is_procedural_var_call) {
      long long wb_sret =
          procvar_sret_size > 8
              ? procvar_sret_size
              : expr->expr_data.function_call_data.cached_procvar_sret_size;
      if (wb_sret <= 8)
        wb_sret = codegen_expr_sret_size(expr);
      struct Expression *pve_wb =
          expr->expr_data.function_call_data.procedural_var_expr;
      if (wb_sret > 8 && pve_wb != NULL && pve_wb->type == EXPR_RECORD_ACCESS) {
        struct RecordField *rf_wb =
            codegen_lookup_record_field_expr(pve_wb, ctx);
        if (rf_wb != NULL && rf_wb->cached_proc_return_sret_size == 0)
          rf_wb->cached_proc_return_sret_size = wb_sret;
      }
    }
    int ctor_has_record_return = (is_constructor && has_record_return);
    StackNode_t *sret_slot = NULL;
    if (has_record_return && !is_constructor) {
      long long sret_size = procvar_sret_size > 0
                                ? procvar_sret_size
                                : codegen_expr_sret_size(expr);
      if (sret_size <= 0 &&
          expr_tree_virtual_call_returns_shortstring(ctx, expr))
        sret_size = 256;
      if (sret_size <= 0 || sret_size > INT_MAX)
        sret_size = CODEGEN_POINTER_SIZE_BYTES;
      sret_slot = add_l_t_bytes("__record_return_tmp__", (int)sret_size);
    }

    /* For constructors, allocate memory for the instance */
    if (is_constructor) {
      struct RecordType *class_record = NULL;
      int ctor_type_receiver = 0;
      int ctor_runtime_vmt_receiver = 0;
      StackNode_t *constructor_vmt_slot = NULL;
      struct Expression *constructor_receiver_expr =
          expr->expr_data.function_call_data.constructor_receiver_expr;
      int first_arg_is_runtime_classref =
          expr_tree_first_arg_is_class_vmt_value(expr, ctx);
      struct RecordType *receiver_class_record = NULL;
      int receiver_is_runtime_classref = expr_tree_expr_is_class_vmt_value(
          constructor_receiver_expr, ctx, &receiver_class_record);

      /* Allocate constructor instances for constructor-call forms where the
       * first argument is either a type receiver (TClass.Create) or a
       * semcheck-injected Self placeholder with resolved class pointer type. */
      ListNode_t *first_arg = expr->expr_data.function_call_data.args_expr;
      struct Expression *class_expr =
          (constructor_receiver_expr != NULL)
              ? constructor_receiver_expr
              : ((first_arg != NULL && first_arg->cur != NULL)
                     ? (struct Expression *)first_arg->cur
                     : NULL);
      if (first_arg != NULL && first_arg->cur != NULL) {
        if (class_expr != NULL && class_expr->resolved_kgpc_type != NULL) {
          KgpcType *class_type = class_expr->resolved_kgpc_type;
          if (kgpc_type_is_pointer(class_type) &&
              class_type->info.points_to != NULL &&
              kgpc_type_is_record(class_type->info.points_to)) {
            class_record = class_type->info.points_to->info.record_info;
            ctor_type_receiver = (class_record != NULL);
            if (ctor_type_receiver && constructor_receiver_expr != NULL)
              ctor_runtime_vmt_receiver = 1;
          } else if (kgpc_type_is_record(class_type)) {
            class_record = class_type->info.record_info;
            ctor_type_receiver = (class_record != NULL);
            if (ctor_type_receiver && constructor_receiver_expr != NULL)
              ctor_runtime_vmt_receiver = 1;
          } else if (kgpc_type_is_pointer(class_type) &&
                     class_type->info.points_to != NULL &&
                     kgpc_type_is_pointer(class_type->info.points_to) &&
                     class_type->info.points_to->info.points_to != NULL &&
                     kgpc_type_is_record(
                         class_type->info.points_to->info.points_to)) {
            class_record =
                class_type->info.points_to->info.points_to->info.record_info;
            ctor_type_receiver = (class_record != NULL);
            ctor_runtime_vmt_receiver = 1;
          }
        }

        if (!ctor_runtime_vmt_receiver &&
            (first_arg_is_runtime_classref || receiver_is_runtime_classref)) {
          ctor_runtime_vmt_receiver = 1;
          if (constructor_receiver_expr == NULL)
            constructor_receiver_expr = class_expr;
          if (class_record == NULL && receiver_class_record != NULL) {
            class_record = receiver_class_record;
            ctor_type_receiver = 1;
          }
        }

        if (!ctor_type_receiver && class_expr != NULL &&
            class_expr->type == EXPR_VAR_ID &&
            class_expr->expr_data.id != NULL && ctx != NULL &&
            ctx->symtab != NULL) {
          HashNode_t *class_node = NULL;
          if (FindSymbol(&class_node, ctx->symtab, class_expr->expr_data.id) !=
                  0 &&
              class_node != NULL && class_node->hash_type == HASHTYPE_TYPE &&
              class_node->type != NULL) {
            if (class_node->type->kind == TYPE_KIND_RECORD)
              class_record = class_node->type->info.record_info;
            else if (class_node->type->kind == TYPE_KIND_POINTER &&
                     class_node->type->info.points_to != NULL &&
                     class_node->type->info.points_to->kind == TYPE_KIND_RECORD)
              class_record = class_node->type->info.points_to->info.record_info;
            ctor_type_receiver = (class_record != NULL);
          }
        }
      }

      /* Fallback: derive owner class from the codegen context. */
      if (!ctor_type_receiver && ctx != NULL &&
          ctx->current_subprogram_owner_class != NULL && ctx->symtab != NULL) {
        const char *owner_id = ctx->current_subprogram_owner_class;

        HashNode_t *owner_node = NULL;
        if (FindSymbol(&owner_node, ctx->symtab, owner_id) != 0 &&
            owner_node != NULL && owner_node->type != NULL) {
          if (owner_node->type->kind == TYPE_KIND_RECORD)
            class_record = owner_node->type->info.record_info;
          else if (owner_node->type->kind == TYPE_KIND_POINTER &&
                   owner_node->type->info.points_to != NULL &&
                   owner_node->type->info.points_to->kind == TYPE_KIND_RECORD)
            class_record = owner_node->type->info.points_to->info.record_info;
          ctor_type_receiver = (class_record != NULL);
        }
      }

      if (class_record != NULL) {
        CODEGEN_DEBUG(
            "DEBUG Constructor: class_record=%p, is_class=%d, properties=%p\n",
            (void *)class_record, class_record->is_class,
            (void *)class_record->properties);

        if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL) {
          struct Expression *cexpr =
              (struct Expression *)
                  expr->expr_data.function_call_data.args_expr->cur;
          fprintf(stderr,
                  "[CodeGen] gencode_case0: Checking class_record %p from "
                  "class_expr %p (type=%d line=%d)\n",
                  class_record, (void *)cexpr, cexpr->type, cexpr->line_num);
        }
      }
      /* Constructor chaining: when a constructor calls a sibling constructor
       * on Self, it's a regular method call — do not allocate a new instance.
       */
      if (ctor_type_receiver && constructor_receiver_expr != NULL &&
          constructor_receiver_expr->type == EXPR_VAR_ID &&
          constructor_receiver_expr->expr_data.id != NULL &&
          pascal_identifier_equals(constructor_receiver_expr->expr_data.id,
                                   "Self")) {
        ctor_type_receiver = 0;
        ctor_runtime_vmt_receiver = 0;
      } else if (ctor_type_receiver && constructor_receiver_expr == NULL &&
                 first_arg != NULL && first_arg->cur != NULL) {
        struct Expression *fa = (struct Expression *)first_arg->cur;
        if (fa != NULL && fa->type == EXPR_VAR_ID && fa->expr_data.id != NULL &&
            pascal_identifier_equals(fa->expr_data.id, "Self"))
          ctor_type_receiver = 0;
      }
      if (ctor_type_receiver && class_record != NULL &&
          record_type_is_class(class_record)) {
        /* Get the size of the class instance */
        long long instance_size = 0;
        if (codegen_sizeof_record_type(ctx, class_record, &instance_size) ==
                0 &&
            instance_size > 0) {
          /* Allocate memory through the runtime helper. */
          const char *alloc_arg_reg =
              codegen_target_is_windows() ? "%rcx" : "%rdi";
          if (ctor_runtime_vmt_receiver && constructor_receiver_expr != NULL) {
            Register_t *vmt_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (vmt_reg != NULL) {
              expr_node_t *receiver_tree =
                  build_expr_tree(constructor_receiver_expr);
              if (receiver_tree != NULL) {
                inst_list =
                    gencode_expr_tree(receiver_tree, inst_list, ctx, vmt_reg);
                free_expr_tree(receiver_tree);
                constructor_vmt_slot = add_l_t("ctor_vmt");
                if (constructor_vmt_slot != NULL) {
                  snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                           vmt_reg->bit_64, constructor_vmt_slot->offset);
                  inst_list = add_inst(inst_list, buffer);
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                         vmt_reg->bit_64, alloc_arg_reg);
                inst_list = add_inst(inst_list, buffer);
              } else {
                snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n",
                         instance_size, alloc_arg_reg);
                inst_list = add_inst(inst_list, buffer);
              }
              free_reg(get_reg_stack(), vmt_reg);
            } else {
              snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n",
                       instance_size, alloc_arg_reg);
              inst_list = add_inst(inst_list, buffer);
            }
          } else {
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n",
                     instance_size, alloc_arg_reg);
            inst_list = add_inst(inst_list, buffer);
          }
          inst_list = codegen_vect_reg(inst_list, 0);
          inst_list =
              codegen_call_with_shadow_space(inst_list, "kgpc_allocmem");
          free_arg_regs();

          /* Save the allocated instance pointer */
          constructor_instance_reg =
              get_reg_with_spill(get_reg_stack(), &inst_list);
          if (constructor_instance_reg == NULL) {
            codegen_report_error(
                ctx,
                "ERROR: Unable to allocate register for constructor instance.");
            goto cleanup_constructor;
          }

          snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n",
                   constructor_instance_reg->bit_64);
          inst_list = add_inst(inst_list, buffer);

          /* Spill the instance pointer to a temporary stack slot to survive the
           * call. */
          constructor_instance_slot = add_l_t("ctor_instance");
          if (constructor_instance_slot != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                     constructor_instance_reg->bit_64,
                     constructor_instance_slot->offset);
            inst_list = add_inst(inst_list, buffer);

            /* Track the spill slot for transient-temp cleanup.
             * The reaching expr_tree.c path runs only when the
             * constructor's result is being consumed as an
             * expression value (not by the assignment special
             * case in codegen_stmt_assignment.c, which has its
             * own allocator).  See codegen.h for the ownership
             * semantics; the statement dispatcher pops this
             * entry when the statement transfers ownership
             * (var assignment, raise), and the enclosing
             * function-call expression discards entries
             * pushed by argument evaluation. */
            codegen_push_pending_ctor_temp(ctx,
                                           constructor_instance_slot->offset);
          }

          if (ctor_runtime_vmt_receiver && constructor_receiver_expr != NULL) {
            /* Under register pressure, get_free_reg can return NULL.
               When that happens silently we'd emit no VMT-init store,
               producing instances whose first qword is whatever the
               heap happened to hold — a deferred null-deref at the
               next virtual dispatch. Use get_reg_with_spill so we
               always get a register, and load the instance address
               from its spill slot rather than the live register
               (which may have been spilled out by the same call). */
            Register_t *vmt_reg =
                get_reg_with_spill(get_reg_stack(), &inst_list);
            Register_t *inst_reg =
                get_reg_with_spill(get_reg_stack(), &inst_list);
            if (vmt_reg != NULL && inst_reg != NULL &&
                constructor_vmt_slot != NULL &&
                constructor_instance_slot != NULL) {
              snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                       constructor_vmt_slot->offset, vmt_reg->bit_64);
              inst_list = add_inst(inst_list, buffer);
              snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                       constructor_instance_slot->offset, inst_reg->bit_64);
              inst_list = add_inst(inst_list, buffer);
              snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                       vmt_reg->bit_64, inst_reg->bit_64);
              inst_list = add_inst(inst_list, buffer);
            } else if (vmt_reg != NULL && constructor_vmt_slot != NULL) {
              /* Fallback: live instance register path (no inst_reg available)
               */
              snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                       constructor_vmt_slot->offset, vmt_reg->bit_64);
              inst_list = add_inst(inst_list, buffer);
              snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                       vmt_reg->bit_64, constructor_instance_reg->bit_64);
              inst_list = add_inst(inst_list, buffer);
            }
            if (inst_reg != NULL)
              free_reg(get_reg_stack(), inst_reg);
            if (vmt_reg != NULL)
              free_reg(get_reg_stack(), vmt_reg);

            if (constructor_vmt_slot == NULL) {
              const char *vmt_label = NULL;
              const char *vmt_class_name = class_record->type_id;
              if (vmt_class_name == NULL && constructor_receiver_expr != NULL &&
                  constructor_receiver_expr->type == EXPR_VAR_ID)
                vmt_class_name = constructor_receiver_expr->expr_data.id;
              if (vmt_class_name == NULL && class_expr != NULL &&
                  class_expr->type == EXPR_VAR_ID)
                vmt_class_name = class_expr->expr_data.id;
              if (vmt_class_name == NULL)
                vmt_class_name =
                    expr->expr_data.function_call_data.cached_owner_class;
              if (vmt_class_name == NULL)
                vmt_class_name =
                    expr->expr_data.function_call_data.self_class_name;
              KGPC_COMPILER_HARD_ASSERT(vmt_class_name != NULL &&
                                            vmt_class_name[0] != '\0',
                                        "constructor VMT initialization "
                                        "requires structured class metadata");
              static char vmt_buf[256];
              snprintf(vmt_buf, sizeof(vmt_buf), "%s_VMT", vmt_class_name);
              vmt_label = vmt_buf;
              if (vmt_label != NULL) {
                Register_t *fallback_vmt_reg =
                    get_reg_with_spill(get_reg_stack(), &inst_list);
                if (fallback_vmt_reg != NULL) {
                  snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                           vmt_label, fallback_vmt_reg->bit_64);
                  inst_list = add_inst(inst_list, buffer);
                  snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                           fallback_vmt_reg->bit_64,
                           constructor_instance_reg->bit_64);
                  inst_list = add_inst(inst_list, buffer);
                  free_reg(get_reg_stack(), fallback_vmt_reg);
                }
              }
            }
          } else {
            /* Initialize VMT pointer using the class' static VMT label */
            const char *vmt_label = NULL;
            const char *vmt_class_name = class_record->type_id;
            if (vmt_class_name == NULL && constructor_receiver_expr != NULL &&
                constructor_receiver_expr->type == EXPR_VAR_ID)
              vmt_class_name = constructor_receiver_expr->expr_data.id;
            if (vmt_class_name == NULL && class_expr != NULL &&
                class_expr->type == EXPR_VAR_ID)
              vmt_class_name = class_expr->expr_data.id;
            if (vmt_class_name == NULL)
              vmt_class_name =
                  expr->expr_data.function_call_data.cached_owner_class;
            if (vmt_class_name == NULL)
              vmt_class_name =
                  expr->expr_data.function_call_data.self_class_name;
            KGPC_COMPILER_HARD_ASSERT(vmt_class_name != NULL &&
                                          vmt_class_name[0] != '\0',
                                      "constructor VMT initialization requires "
                                      "structured class metadata");
            static char vmt_buf[256];
            snprintf(vmt_buf, sizeof(vmt_buf), "%s_VMT", vmt_class_name);
            vmt_label = vmt_buf;
            if (vmt_label != NULL) {
              Register_t *vmt_reg = get_free_reg(get_reg_stack(), &inst_list);
              if (vmt_reg != NULL) {
                snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                         vmt_label, vmt_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                         vmt_reg->bit_64, constructor_instance_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), vmt_reg);
              }
            }
          }

          inst_list = codegen_emit_interface_vtable_slot_init(
              inst_list, ctx, class_record, class_record->type_id,
              constructor_instance_reg);
        }
      }
    }

    /* Pass arguments, shifted by hidden return pointer and/or static link */
    int arg_start_index =
        (has_record_return ? 1 : 0) + (should_pass_static_link ? 1 : 0);
    int self_index = -1;

    /* For constructors, we need to:
     * 1. Skip the first argument in the list (class type)
     * 2. Shift register allocation by 1 to make room for Self */
    ListNode_t *args_to_pass = expr->expr_data.function_call_data.args_expr;
    /* Record constructor invoked via the type name (e.g. TRect.Create(...)):
     * the semantic checker marks it with is_constructor_call and inserts an
     * EXPR_NIL Self placeholder as the first argument, but a record (advanced
     * record) is returned by value, so is_constructor was cleared above and
     * this goes through the sret path. The hidden sret pointer occupies arg
     * register 0 and doubles as the constructor's Self, so the NIL placeholder
     * must be skipped — otherwise it would be emitted as a (record-typed) user
     * argument and rejected by codegen_pass_arguments. */
    if (!is_constructor && has_record_return &&
        expr->expr_data.function_call_data.is_constructor_call &&
        args_to_pass != NULL) {
      struct Expression *fa = (struct Expression *)args_to_pass->cur;
      if (fa != NULL && fa->type == EXPR_NIL)
        args_to_pass = args_to_pass->next;
    }
    if (is_constructor && constructor_instance_reg != NULL) {
      /* Place the hidden return pointer (sret) in the first argument slot */
      if (ctor_has_record_return) {
        const char *ret_reg = current_arg_reg64(0);
        if (ret_reg != NULL) {
          if (constructor_instance_slot != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                     constructor_instance_slot->offset, ret_reg);
          } else {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                     constructor_instance_reg->bit_64, ret_reg);
          }
          inst_list = add_inst(inst_list, buffer);
        }
      }

      /* Record the Self parameter slot so it can be emitted AFTER argument
       * evaluation (which may clobber caller-saved registers including the
       * Self register).  The actual movq is deferred to the block below
       * codegen_pass_arguments. */
      self_index = arg_start_index;

      /* Skip the first argument (class type / Self placeholder) unless
       * the constructor was set up from a STMT_PROCEDURE_CALL path where
       * the type receiver was already removed by the semcheck.  In that
       * case, constructor_receiver_expr is set but the first arg is a
       * real user argument, not a placeholder to skip. */
      {
        int skip_first = 1;
        if (expr->expr_data.function_call_data.constructor_receiver_expr !=
                NULL &&
            args_to_pass != NULL) {
          struct Expression *fa = (struct Expression *)args_to_pass->cur;
          if (fa != NULL &&
              expr->expr_data.function_call_data.constructor_receiver_expr !=
                  NULL &&
              fa->type == EXPR_VAR_ID &&
              expr->expr_data.function_call_data.constructor_receiver_expr
                      ->type == EXPR_VAR_ID &&
              fa->expr_data.id != NULL &&
              expr->expr_data.function_call_data.constructor_receiver_expr
                      ->expr_data.id != NULL &&
              pascal_identifier_equals(
                  fa->expr_data.id,
                  expr->expr_data.function_call_data.constructor_receiver_expr
                      ->expr_data.id)) {
            skip_first = 1;
          } else if (fa != NULL && fa->type != EXPR_NIL) {
            /* First arg is not a Self placeholder — it's a real arg.
             * The class was derived from constructor_receiver_expr
             * which was set by the proc_call codegen path. */
            skip_first = 0;
          }
        }
        if (skip_first && args_to_pass != NULL)
          args_to_pass = args_to_pass->next;
      }
      /* Shift register allocation by 1 for Self parameter */
      arg_start_index += 1;
    }

    const char *proc_name_hint = expr->expr_data.function_call_data.id;
    const char *mangled_name_hint =
        expr->expr_data.function_call_data.mangled_id;
    if (proc_name_hint == NULL)
      proc_name_hint = mangled_name_hint;
    /* When the semantic checker rewrote the call to a different runtime
     * function (is_call_info_valid=1, call_kgpc_type=NULL), using the
     * original Pascal id (e.g. "UpCase") for the fallback FindSymbol
     * lookup in codegen_pass_arguments would find the wrong overload
     * (e.g. the string UpCase) and incorrectly promote char args to
     * strings.  Use the mangled name instead so the lookup either finds
     * the correct C runtime entry or finds nothing — in both cases no
     * spurious char-to-string conversion is inserted. */
    if (func_type == NULL &&
        expr->expr_data.function_call_data.is_call_info_valid &&
        mangled_name_hint != NULL)
      proc_name_hint = mangled_name_hint;

    int procvar_method_self_index = -1;
    StackNode_t *procvar_method_self_spill = NULL;
    StackNode_t *procvar_method_code_spill = NULL;
    struct Expression *procvar_callee_expr =
        expr->expr_data.function_call_data.procedural_var_expr;
    struct Expression *procvar_method_storage_expr = procvar_callee_expr;
    KgpcType *procvar_call_type = func_type;
    if (procvar_call_type == NULL &&
        expr->expr_data.function_call_data.call_kgpc_type != NULL &&
        expr->expr_data.function_call_data.call_kgpc_type->kind ==
            TYPE_KIND_PROCEDURE) {
      procvar_call_type = expr->expr_data.function_call_data.call_kgpc_type;
    }
    int procvar_is_method_pointer = 0;

    if (expr->expr_data.function_call_data.is_procedural_var_call &&
        procvar_callee_expr != NULL) {
      KgpcType *callee_type = expr_get_kgpc_type(procvar_callee_expr);
      KgpcType *record_field_proc_type =
          expr_tree_proc_type_from_record_field(ctx, procvar_callee_expr);
      if (record_field_proc_type != NULL &&
          record_field_proc_type->kind == TYPE_KIND_PROCEDURE)
        callee_type = record_field_proc_type;
      else if (callee_type == NULL)
        callee_type = record_field_proc_type;
      if (callee_type != NULL && callee_type->kind == TYPE_KIND_POINTER &&
          callee_type->info.points_to != NULL)
        callee_type = callee_type->info.points_to;
      if (callee_type != NULL && callee_type->kind == TYPE_KIND_PROCEDURE)
        procvar_call_type = callee_type;
      if (kgpc_type_is_method_pointer(callee_type))
        procvar_is_method_pointer = 1;

      if (procvar_callee_expr->type == EXPR_TYPECAST &&
          procvar_callee_expr->expr_data.typecast_data.expr != NULL) {
        procvar_method_storage_expr =
            procvar_callee_expr->expr_data.typecast_data.expr;
        if (!procvar_is_method_pointer) {
          KgpcType *target_type = expr_get_kgpc_type(procvar_callee_expr);
          if (target_type != NULL && target_type->kind == TYPE_KIND_POINTER &&
              target_type->info.points_to != NULL)
            target_type = target_type->info.points_to;
          if (kgpc_type_is_method_pointer(target_type))
            procvar_is_method_pointer = 1;
        }
      }

      if (!procvar_is_method_pointer && procvar_method_storage_expr != NULL &&
          procvar_method_storage_expr->type == EXPR_VAR_ID && ctx != NULL &&
          ctx->symtab != NULL) {
        HashNode_t *callee_node = NULL;
        if (FindSymbol(&callee_node, ctx->symtab,
                       procvar_method_storage_expr->expr_data.id) != 0 &&
            callee_node != NULL && callee_node->type != NULL) {
          KgpcType *symbol_type = callee_node->type;
          if (symbol_type->kind == TYPE_KIND_POINTER &&
              symbol_type->info.points_to != NULL)
            symbol_type = symbol_type->info.points_to;
          if (symbol_type->kind == TYPE_KIND_PROCEDURE)
            procvar_call_type = symbol_type;
          if (kgpc_type_is_method_pointer(symbol_type))
            procvar_is_method_pointer = 1;
        }
      }

      if (procvar_is_method_pointer) {
        Register_t *descriptor_reg = NULL;
        if (codegen_expr_is_addressable(procvar_method_storage_expr))
          inst_list = codegen_address_for_expr(procvar_method_storage_expr,
                                               inst_list, ctx, &descriptor_reg);
        else
          inst_list = codegen_expr_with_result(procvar_method_storage_expr,
                                               inst_list, ctx, &descriptor_reg);

        if (codegen_had_error(ctx) || descriptor_reg == NULL)
          return inst_list;

        procvar_method_self_spill = add_l_t_bytes("procvar_method_self", 8);
        procvar_method_code_spill = add_l_t_bytes("procvar_method_code", 8);
        if (procvar_method_self_spill == NULL ||
            procvar_method_code_spill == NULL) {
          free_reg(get_reg_stack(), descriptor_reg);
          return inst_list;
        }

        snprintf(buffer, sizeof(buffer), "\tmovq\t8(%s), %%r11\n",
                 descriptor_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%r11, -%d(%%rbp)\n",
                 procvar_method_self_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %%r11\n",
                 descriptor_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%r11, -%d(%%rbp)\n",
                 procvar_method_code_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), descriptor_reg);

        procvar_method_self_index = arg_start_index;
        arg_start_index += 1;
      }
    }

    if (is_constructor && kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL) {
      int args_count = 0;
      for (ListNode_t *c = args_to_pass; c != NULL; c = c->next)
        args_count++;
      fprintf(stderr, "[CodeGen] Constructor %s args=%d arg_start=%d\n",
              proc_name_hint ? proc_name_hint : "(null)", args_count,
              arg_start_index);
    }

    inst_list = codegen_pass_arguments(
        args_to_pass, inst_list, ctx, procvar_call_type, proc_name_hint,
        arg_start_index, expr,
        expr->expr_data.function_call_data.is_class_method_call);

    /* Invalidate static link cache after argument evaluation
     * because the static link register may have been clobbered
     * during argument evaluation. This prevents the bug where
     * nested function calls reuse the same register for different
     * static links, causing the wrong frame pointer to be passed. */
    if (static_link_source == STATIC_LINK_FROM_REG && static_link_reg != NULL) {
      /* The register was already acquired above, but argument evaluation
       * may have invalidated it. We need to reload it fresh. */
      free_reg(get_reg_stack(), static_link_reg);
      if (ctx->static_link_reg != NULL) {
        free_reg(get_reg_stack(), ctx->static_link_reg);
        ctx->static_link_reg = NULL;
        ctx->static_link_reg_level = 0;
      }
      /* Re-acquire the static link register after argument evaluation */
      int levels_to_traverse = (current_depth - callee_depth) + 1;
      static_link_reg =
          codegen_acquire_static_link(ctx, &inst_list, levels_to_traverse);
    }

    if (should_pass_static_link) {
      const char *dest_reg = current_arg_reg64(has_record_return ? 1 : 0);
      assert(dest_reg != NULL &&
             "current_arg_reg64(..) should never return NULL");
      char link_buffer[64];
      switch (static_link_source) {
      case STATIC_LINK_FROM_RBP:
        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%%rbp, %s\n",
                 dest_reg);
        inst_list = add_inst(inst_list, link_buffer);
        break;
      case STATIC_LINK_FROM_SLOT:
        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 static_link_slot_offset, dest_reg);
        inst_list = add_inst(inst_list, link_buffer);
        break;
      case STATIC_LINK_FROM_REG:
        if (static_link_reg != NULL) {
          snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%s, %s\n",
                   static_link_reg->bit_64, dest_reg);
          inst_list = add_inst(inst_list, link_buffer);
          free_reg(get_reg_stack(), static_link_reg);
          static_link_reg = NULL;
        }
        break;
      default:
        break;
      }
    }

    if (has_record_return && !is_constructor && sret_slot != NULL) {
      const char *ret_reg = current_arg_reg64(0);
      if (ret_reg != NULL) {
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                 sret_slot->offset, ret_reg);
        inst_list = add_inst(inst_list, buffer);
      }
    }

    /* For constructors, emit the Self argument into the correct register
     * AFTER argument evaluation so that argument-passing code cannot clobber
     * it. */
    if (is_constructor && constructor_instance_reg != NULL && self_index >= 0) {
      const char *self_arg_reg = current_arg_reg64(self_index);
      if (constructor_instance_slot != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 constructor_instance_slot->offset, self_arg_reg);
      } else {
        const char *source_reg = constructor_instance_reg->bit_64;
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", source_reg,
                 self_arg_reg);
      }
      inst_list = add_inst(inst_list, buffer);
    }

    if (procvar_method_self_spill != NULL && procvar_method_self_index >= 0) {
      const char *self_arg_reg = current_arg_reg64(procvar_method_self_index);
      assert(self_arg_reg != NULL &&
             "current_arg_reg64(..) should never return NULL");
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
               procvar_method_self_spill->offset, self_arg_reg);
      inst_list = add_inst(inst_list, buffer);
    }

    if (static_link_expr_active)
      codegen_end_expression(ctx);

    /* Set %al for the SysV varargs ABI.
     * For varargs functions, %al must be an upper bound on the number of XMM
     * registers used.  Use 8 (the maximum) as a conservative upper bound.
     * For non-varargs functions, %al is ignored by the callee but we set it
     * to 0 to avoid stale values from previous operations. */
    {
      int is_varargs_call = 0;
      if (func_type != NULL && func_type->kind == TYPE_KIND_PROCEDURE &&
          func_type->info.proc_info.definition != NULL) {
        Tree_t *def = func_type->info.proc_info.definition;
        is_varargs_call = def->tree_data.subprogram_data.is_varargs;
      }
      inst_list = codegen_vect_reg(inst_list, is_varargs_call ? 8 : 0);
    }

    /* Check if this is a call through a procedural variable */
    if (expr->expr_data.function_call_data.is_procedural_var_call) {
      if (procvar_method_code_spill != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%r11\n",
                 procvar_method_code_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tcall\t*%%r11\n");
        inst_list = add_inst(inst_list, buffer);
      } else if (expr->expr_data.function_call_data.procedural_var_expr !=
                 NULL) {
        /* Evaluate expression producing the function pointer */
        Register_t *func_ptr_reg = NULL;
        codegen_begin_expression(ctx);
        inst_list = codegen_expr_with_result(
            expr->expr_data.function_call_data.procedural_var_expr, inst_list,
            ctx, &func_ptr_reg);
        codegen_end_expression(ctx);

        if (func_ptr_reg != NULL) {
          snprintf(buffer, sizeof(buffer), "\tcall\t*%s\n",
                   func_ptr_reg->bit_64);
          inst_list = add_inst(inst_list, buffer);
          free_reg(get_reg_stack(), func_ptr_reg);
        } else {
          snprintf(buffer, sizeof(buffer),
                   "\t# ERROR: failed to evaluate procedural expression\n");
          inst_list = add_inst(inst_list, buffer);
        }
      } else if (expr->expr_data.function_call_data.procedural_var_symbol !=
                 NULL) {
        /* Call through a procedural variable stored in a symbol */
        const char *var_name = expr->expr_data.function_call_data.id;

        /* Find the variable on the stack, checking for non-local access */
        int proc_var_scope_depth = 0;
        StackNode_t *stack_node =
            find_label_with_depth((char *)var_name, &proc_var_scope_depth);
        if (stack_node != NULL) {
          /* Load the function pointer into a register */
          Register_t *func_ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
          if (func_ptr_reg != NULL) {
            if (stack_node->is_static) {
              const char *label = (stack_node->static_label != NULL)
                                      ? stack_node->static_label
                                      : stack_node->label;
              snprintf(buffer, sizeof(buffer), "\tmovq\t%s(%%rip), %s\n", label,
                       func_ptr_reg->bit_64);
            } else if (proc_var_scope_depth > 0) {
              /* Non-local variable: access through static link */
              Register_t *frame_reg = codegen_acquire_static_link(
                  ctx, &inst_list, proc_var_scope_depth);
              if (frame_reg != NULL)
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%s), %s\n",
                         stack_node->offset, frame_reg->bit_64,
                         func_ptr_reg->bit_64);
              else
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                         stack_node->offset, func_ptr_reg->bit_64);
            } else {
              snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                       stack_node->offset, func_ptr_reg->bit_64);
            }
            inst_list = add_inst(inst_list, buffer);

            /* Call through the function pointer */
            snprintf(buffer, sizeof(buffer), "\tcall\t*%s\n",
                     func_ptr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            free_reg(get_reg_stack(), func_ptr_reg);
          }
        } else {
          /* Variable not found - emit error */
          snprintf(buffer, sizeof(buffer),
                   "\t# ERROR: procedural variable %s not found\n", var_name);
          inst_list = add_inst(inst_list, buffer);
        }
      } else {
        snprintf(buffer, sizeof(buffer),
                 "\t# ERROR: procedural call target missing\n");
        inst_list = add_inst(inst_list, buffer);
      }
    } else if (expr->expr_data.function_call_data.is_interface_call &&
               expr->expr_data.function_call_data.vmt_index >= 0) {
      int self_arg_index = has_record_return ? 1 : 0;
      inst_list = codegen_emit_interface_dispatch(
          inst_list, ctx, current_arg_reg64(self_arg_index),
          expr->expr_data.function_call_data.self_class_name,
          expr->expr_data.function_call_data.vmt_index, "intf_expr",
          "__intf_expr_target", 1, codegen_spill_call_arg_regs_expr,
          codegen_restore_call_arg_regs_expr);
    } else if (expr->expr_data.function_call_data.is_virtual_call &&
               expr->expr_data.function_call_data.vmt_index >= 0) {
      /* Virtual method call - dispatch through VMT.
       * Self is in the first argument register AFTER the SRET pointer (if any):
       *   - No SRET: Self at arg reg 0 (%rdi / %rcx)
       *   - With SRET: Self at arg reg 1 (%rsi / %rdx)
       * The instance has the VMT pointer at offset 0.
       * For class methods, Self already IS the VMT pointer (dereferenced
       * earlier). */
      const char *virtual_owner =
          expr->expr_data.function_call_data.self_class_name;
      if (virtual_owner == NULL)
        virtual_owner = expr->expr_data.function_call_data.cached_owner_class;
      const char *virtual_method =
          expr->expr_data.function_call_data.cached_method_name;
      if (virtual_method == NULL)
        virtual_method =
            expr->expr_data.function_call_data.placeholder_method_name;
      KGPC_COMPILER_HARD_ASSERT(virtual_owner != NULL &&
                                    virtual_owner[0] != '\0',
                                "virtual expression call reached codegen "
                                "without semcheck owner metadata");
      KGPC_COMPILER_HARD_ASSERT(virtual_method != NULL &&
                                    virtual_method[0] != '\0',
                                "virtual expression call reached codegen "
                                "without semcheck method metadata");
      int vmt_index = codegen_resolve_virtual_vmt_index(
          ctx, virtual_owner, virtual_method,
          expr->expr_data.function_call_data.call_kgpc_type);
      int self_arg_index = has_record_return ? 1 : 0;
      const char *self_reg = current_arg_reg64(self_arg_index);
      int dispatch_self_is_vmt =
          expr->expr_data.function_call_data.is_class_method_call;
      if (!dispatch_self_is_vmt &&
          expr->expr_data.function_call_data.cached_owner_class != NULL &&
          expr->expr_data.function_call_data.cached_method_name != NULL &&
          from_cparser_is_method_nonstatic_class_method(
              expr->expr_data.function_call_data.cached_owner_class,
              expr->expr_data.function_call_data.cached_method_name)) {
        dispatch_self_is_vmt = 1;
      }
      if (!dispatch_self_is_vmt &&
          expr->expr_data.function_call_data.self_class_name != NULL &&
          expr->expr_data.function_call_data.id != NULL &&
          from_cparser_is_method_nonstatic_class_method(
              expr->expr_data.function_call_data.self_class_name,
              expr->expr_data.function_call_data.id)) {
        dispatch_self_is_vmt = 1;
      }
      int first_arg_is_vmt = expr_tree_first_arg_is_class_vmt_value(expr, ctx);
      int constructor_receiver_is_vmt =
          expr->expr_data.function_call_data.is_constructor_call &&
          first_arg_is_vmt;
      int self_is_vmt =
          (!expr->expr_data.function_call_data.is_constructor_call) &&
          first_arg_is_vmt;
      dispatch_self_is_vmt = dispatch_self_is_vmt || self_is_vmt;
      if (expr->expr_data.function_call_data.is_constructor_call &&
          constructor_instance_reg != NULL &&
          expr->expr_data.function_call_data.constructor_receiver_expr !=
              NULL &&
          !dispatch_self_is_vmt && self_reg != NULL &&
          expr_tree_expr_is_class_vmt_value(
              expr->expr_data.function_call_data.constructor_receiver_expr, ctx,
              NULL)) {
        StackNode_t *ctor_self_slot = add_l_t("ctor_self_vmt");
        if (ctor_self_slot != NULL) {
          snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", self_reg,
                   ctor_self_slot->offset);
          inst_list = add_inst(inst_list, buffer);
        }

        Register_t *ctor_vmt_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (ctor_vmt_reg != NULL) {
          expr_node_t *receiver_tree = build_expr_tree(
              expr->expr_data.function_call_data.constructor_receiver_expr);
          if (receiver_tree != NULL) {
            inst_list =
                gencode_expr_tree(receiver_tree, inst_list, ctx, ctor_vmt_reg);
            free_expr_tree(receiver_tree);
            if (ctor_self_slot != NULL) {
              snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%r11\n",
                       ctor_self_slot->offset);
              inst_list = add_inst(inst_list, buffer);
            } else {
              snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r11\n", self_reg);
              inst_list = add_inst(inst_list, buffer);
            }
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%%r11)\n",
                     ctor_vmt_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            if (ctor_self_slot != NULL) {
              snprintf(buffer, sizeof(buffer), "\tmovq\t%%r11, %s\n", self_reg);
              inst_list = add_inst(inst_list, buffer);
            }
          }
          free_reg(get_reg_stack(), ctor_vmt_reg);
        }
      }
      if (expr->expr_data.function_call_data.is_constructor_call &&
          expr->expr_data.function_call_data.constructor_receiver_expr ==
              NULL &&
          !dispatch_self_is_vmt && !constructor_receiver_is_vmt) {
        const char *ctor_owner = virtual_owner;
        KGPC_COMPILER_HARD_ASSERT(ctor_owner != NULL && ctor_owner[0] != '\0',
                                  "constructor VMT initialization requires "
                                  "structured owner metadata");
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s_VMT(%%rip), %%r11\n",
                 ctor_owner);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%r11, (%s)\n", self_reg);
        inst_list = add_inst(inst_list, buffer);
      }
      /* Self has already been lowered to the correct calling form during
       * argument passing: instance pointer for normal methods, VMT pointer
       * for non-static class methods. Do not dereference again here. */
      /* Copy Self (or VMT for class methods) to r11 */
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r11\n", self_reg);
      inst_list = add_inst(inst_list, buffer);
      /* Get VMT pointer (at offset 0 of instance).
       * For class methods, Self IS the VMT, so this reads typeinfo (not VMT).
       */
      if (!dispatch_self_is_vmt) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%%r11), %%r11\n");
        inst_list = add_inst(inst_list, buffer);
      }
      /* VMT layout: [12 metadata slots (0-88), method1 at 96, method2 at 104,
       * ...] */
      int vmt_offset = vmt_index * 8;
      snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%r11), %%r11\n",
               vmt_offset);
      inst_list = add_inst(inst_list, buffer);
      /* Call through the VMT entry */
      CallerSaveState caller_state;
      regstack_caller_save(get_reg_stack(), &inst_list, &caller_state);
      snprintf(buffer, sizeof(buffer), "\tcall\t*%%r11\n");
      inst_list = add_inst(inst_list, buffer);
      regstack_caller_restore(get_reg_stack(), &inst_list, &caller_state);
    } else {
      /* Normal function call */
      char *owned_call_target = NULL;
      const char *call_target =
          codegen_resolve_function_call_target(ctx, expr, &owned_call_target);

      if (call_target != NULL &&
          (pascal_identifier_equals(call_target, "Low") ||
           pascal_identifier_equals(call_target, "High"))) {
        ListNode_t *lowered = codegen_builtin_lowhigh_fallback(
            expr, inst_list, ctx, target_reg,
            pascal_identifier_equals(call_target, "High"));
        if (owned_call_target != NULL)
          free(owned_call_target);
        if (lowered != NULL)
          return lowered;
      }

      /* If the call target resolves to a type (not a procedure), this is
       * a typecast that the semcheck didn't rewrite (e.g., from cached
       * unit ASTs).  Treat it as a no-op: evaluate the argument and use
       * its value directly. */
      if (call_target != NULL && ctx != NULL && ctx->symtab != NULL) {
        HashNode_t *target_sym = NULL;
        if (FindSymbol(&target_sym, ctx->symtab, call_target) != 0 &&
            target_sym != NULL && target_sym->hash_type == HASHTYPE_TYPE &&
            (target_sym->type == NULL ||
             target_sym->type->kind != TYPE_KIND_PROCEDURE)) {
          /* Typecast: just evaluate the single argument */
          ListNode_t *args = expr->expr_data.function_call_data.args_expr;
          if (args != NULL && args->cur != NULL) {
            struct Expression *arg_expr = (struct Expression *)args->cur;
            expr_node_t *arg_tree = build_expr_tree(arg_expr);
            if (arg_tree != NULL) {
              inst_list =
                  gencode_expr_tree(arg_tree, inst_list, ctx, target_reg);
              free_expr_tree(arg_tree);
            }
          }
          return inst_list;
        }
      }

      /* PopCnt builtin and mangled variants (e.g. popcnt_u64):
       * redirect to runtime's fpc_in_popcnt_x */
      if (call_target != NULL &&
          (pascal_identifier_equals(call_target, "PopCnt") ||
           strncasecmp(call_target, "popcnt_", 7) == 0)) {
        if (owned_call_target != NULL)
          free(owned_call_target);
        owned_call_target = strdup("fpc_in_popcnt_x");
        call_target = owned_call_target;
      }

      if (call_target != NULL) {
        CallerSaveState caller_state;
        regstack_caller_save(get_reg_stack(), &inst_list, &caller_state);
        snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", call_target);
        inst_list = add_inst(inst_list, buffer);
        regstack_caller_restore(get_reg_stack(), &inst_list, &caller_state);
      } else {
        /* This should never happen - emit error */
        snprintf(buffer, sizeof(buffer),
                 "\t# ERROR: function call with NULL target\n");
        inst_list = add_inst(inst_list, buffer);
      }
    }

    inst_list = codegen_cleanup_call_stack(inst_list, ctx);
    // NOTE: Don't free mangled_id here - it will be freed when the AST is
    // destroyed This was causing double-free errors in nested function calls
    // within string concatenations
    // codegen_release_function_call_mangled_id(expr);

    /* For constructors, use the return value from the constructor (Self in
     * %rax). Constructors now properly return Self, so we don't need to rely on
     * the saved instance register which could be clobbered during the call. */
    if (is_constructor && constructor_instance_reg != NULL) {
      /* Use the allocated instance pointer as the result, regardless of what
       * the callee returns. */
      if (constructor_instance_slot != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 constructor_instance_slot->offset, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
      } else {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                 constructor_instance_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
      }
      free_reg(get_reg_stack(), constructor_instance_reg);
    } else {
      /* Normal function return value */
      if (has_record_return && !is_constructor && sret_slot != NULL)
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                 sret_slot->offset, target_reg->bit_64);
      else if (expr_has_extended_storage(expr)) {
        StackNode_t *ext_slot = add_l_t_bytes("__ext_ret", 10);
        if (ext_slot == NULL)
          return inst_list;

        snprintf(buffer, sizeof(buffer), "\tfstpt\t-%d(%%rbp)\n",
                 ext_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        if (codegen_target_is_windows())
          snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rcx\n",
                   ext_slot->offset);
        else
          snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rdi\n",
                   ext_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(
            inst_list, "kgpc_load_extended_to_bits");
        free_arg_regs();
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n",
                 target_reg->bit_64);
      } else if (expr_has_type_tag(expr, REAL_TYPE)) {
        long long real_size = expr_effective_size_bytes(expr);
        if (real_size == 4)
          inst_list = add_inst(inst_list, "\tcvtss2sd\t%xmm0, %xmm0\n");
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n",
                 target_reg->bit_64);
      } else {
        /* For procedural var calls, check call_kgpc_type's return type */
        int use_qword = expr_uses_qword_kgpctype(expr);
        if (!use_qword && expr_has_type_tag(expr, RECORD_TYPE)) {
          long long record_ret_size = 0;
          if (codegen_get_record_size(ctx, expr, &record_ret_size) == 0 &&
              record_ret_size > 4) {
            use_qword = 1;
          }
        }
        if (!use_qword &&
            expr->expr_data.function_call_data.is_procedural_var_call &&
            expr->expr_data.function_call_data.call_kgpc_type != NULL) {
          KgpcType *call_type =
              expr->expr_data.function_call_data.call_kgpc_type;
          KgpcType *ret_type = kgpc_type_get_return_type(call_type);
          if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL) {
            fprintf(stderr,
                    "[CodeGen] gencode_case0: is_procedural_var_call=1, "
                    "call_type=%p, ret_type=%p, return_type_id=%s\n",
                    (void *)call_type, (void *)ret_type,
                    call_type->info.proc_info.return_type_id
                        ? call_type->info.proc_info.return_type_id
                        : "(null)");
          }
          if (ret_type != NULL && kgpc_type_uses_qword(ret_type))
            use_qword = 1;
          /* Also check return_type_id using type system lookup */
          else if (call_type->info.proc_info.return_type_id != NULL) {
            const char *ret_id = call_type->info.proc_info.return_type_id;
            if (kgpc_type_id_uses_qword(ret_id, ctx->symtab))
              use_qword = 1;
          }
        }
        /* Always use movq for function return values on x86-64.
         * movl truncates pointers and 64-bit values; movq is always safe. */
        (void)use_qword;
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n",
                 target_reg->bit_64);
      }
      inst_list = add_inst(inst_list, buffer);
    }

    /* Discard any constructor-temp pending entries pushed during this
     * call's argument evaluation: the args have just been consumed by
     * the callee (whose body we cannot analyse for ownership), so
     * conservatively assume the callee may have stored them.  Keep
     * this call's own push (when this call IS a constructor) so the
     * caller can still observe ownership transfer at the statement
     * boundary. */
    if (ctx != NULL) {
      int keep_count = ctor_pending_snapshot_before_push;
      if (is_constructor && constructor_instance_slot != NULL)
        keep_count += 1;
      if (ctx->pending_ctor_temp_count > keep_count)
        ctx->pending_ctor_temp_count = keep_count;
    }
    return inst_list;

  cleanup_constructor:
    if (constructor_instance_reg != NULL)
      free_reg(get_reg_stack(), constructor_instance_reg);
    if (ctx != NULL) {
      int keep_count = ctor_pending_snapshot_before_push;
      if (is_constructor && constructor_instance_slot != NULL)
        keep_count += 1;
      if (ctx->pending_ctor_temp_count > keep_count)
        ctx->pending_ctor_temp_count = keep_count;
    }
    return inst_list;
  } else if (expr->type == EXPR_ARRAY_ACCESS) {
    return codegen_array_access(expr, inst_list, ctx, target_reg);
  } else if (expr->type == EXPR_RECORD_ACCESS) {
    return codegen_record_access(expr, inst_list, ctx, target_reg);
  } else if (expr->type == EXPR_TYPECAST &&
             expr->expr_data.typecast_data.expr != NULL &&
             (expr->expr_data.typecast_data.target_type == BYTE_TYPE ||
              expr->expr_data.typecast_data.target_type == WORD_TYPE)) {
    struct Expression *inner_expr = expr->expr_data.typecast_data.expr;
    const int byte_mask = 255;
    const int word_mask = 65535;
    expr_node_t *inner_tree = build_expr_tree(inner_expr);
    if (inner_tree == NULL)
      return inst_list;

    inst_list = gencode_expr_tree(inner_tree, inst_list, ctx, target_reg);
    free_expr_tree(inner_tree);

    if (expr->expr_data.typecast_data.target_type == BYTE_TYPE)
      snprintf(buffer, sizeof(buffer), "\tandl\t$%d, %s\n", byte_mask,
               target_reg->bit_32);
    else
      snprintf(buffer, sizeof(buffer), "\tandl\t$%d, %s\n", word_mask,
               target_reg->bit_32);

    inst_list = add_inst(inst_list, buffer);
    return inst_list;
  } else if (expr->type == EXPR_TYPECAST &&
             expr->expr_data.typecast_data.expr != NULL &&
             expr_typecast_is_signed_narrow(expr, &narrow_signed_size)) {
    /* Signed sub-32-bit reinterpret cast SmallInt(x) / ShortInt(x): evaluate
     * the operand, then sign-extend the low byte(s) to fill the 32-bit
     * register.  Without this the operand keeps its own (often unsigned)
     * extension — SmallInt(word_$ffff) would remain 65535 instead of -1. */
    struct Expression *inner_expr = expr->expr_data.typecast_data.expr;
    expr_node_t *inner_tree = build_expr_tree(inner_expr);
    if (inner_tree == NULL)
      return inst_list;

    inst_list = gencode_expr_tree(inner_tree, inst_list, ctx, target_reg);
    free_expr_tree(inner_tree);

    if (narrow_signed_size == 1) {
      const char *reg8 = expr_tree_register_name8(target_reg);
      if (reg8 != NULL)
        snprintf(buffer, sizeof(buffer), "\tmovsbl\t%s, %s\n", reg8,
                 target_reg->bit_32);
      else
        snprintf(buffer, sizeof(buffer), "\tmovsbl\t%%al, %s\n",
                 target_reg->bit_32);
    } else {
      const char *reg16 = codegen_register_name16(target_reg);
      if (reg16 != NULL)
        snprintf(buffer, sizeof(buffer), "\tmovswl\t%s, %s\n", reg16,
                 target_reg->bit_32);
      else
        snprintf(buffer, sizeof(buffer), "\tmovswl\t%%ax, %s\n",
                 target_reg->bit_32);
    }
    inst_list = add_inst(inst_list, buffer);
    return inst_list;
  } else if (expr->type == EXPR_TYPECAST &&
             expr->expr_data.typecast_data.expr != NULL &&
             expr->expr_data.typecast_data.target_type == CHAR_TYPE) {
    /* Char-valued typecast preserved as a leaf (see build_expr_tree_internal):
     * evaluate the inner ordinal and narrow it to a single byte. The result
     * is a CHAR_TYPE value that downstream char->string promotion can detect
     * and convert before passing it to a string-concat runtime call. */
    struct Expression *inner_expr = expr->expr_data.typecast_data.expr;
    expr_node_t *inner_tree = build_expr_tree(inner_expr);
    if (inner_tree == NULL)
      return inst_list;

    inst_list = gencode_expr_tree(inner_tree, inst_list, ctx, target_reg);
    free_expr_tree(inner_tree);

    snprintf(buffer, sizeof(buffer), "\tandl\t$255, %s\n", target_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    return inst_list;
  } else if (expr->type == EXPR_TYPECAST &&
             expr->expr_data.typecast_data.expr != NULL &&
             (expr->expr_data.typecast_data.target_type == INT64_TYPE ||
              expr->expr_data.typecast_data.target_type == QWORD_TYPE) &&
             type_tag_is_signed_32bit_int(
                 expr_get_type_tag(expr->expr_data.typecast_data.expr))) {
    /* Widening Int64(longint_expr) / QWord(longint_expr): evaluate the
     * inner expression into target_reg's low 32 bits (it's signed), then
     * sign-extend to 64 bits.  Without this, a longint(-1) loaded by movl
     * becomes 0x00000000FFFFFFFF (4294967295) rather than -1, which
     * breaks FPC's constant range checks and other 64-bit signed math. */
    struct Expression *inner_expr = expr->expr_data.typecast_data.expr;
    expr_node_t *inner_tree = build_expr_tree(inner_expr);
    if (inner_tree == NULL)
      return inst_list;

    inst_list = gencode_expr_tree(inner_tree, inst_list, ctx, target_reg);
    free_expr_tree(inner_tree);

    snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", target_reg->bit_32,
             target_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    return inst_list;
  } else if (expr->type == EXPR_TYPECAST &&
             expr->expr_data.typecast_data.target_type == POINTER_TYPE &&
             expr->expr_data.typecast_data.expr != NULL &&
             expr->expr_data.typecast_data.expr->is_array_expr &&
             codegen_expr_is_addressable(expr->expr_data.typecast_data.expr)) {
    /* Array-to-pointer typecast (e.g. PByte(top^.data)):
     * for static arrays, compute the ADDRESS of the array (its
     * first element).  For dynamic arrays, the variable storage
     * is a 16-byte descriptor { data, length } — taking its
     * address yields the descriptor base, but the desired
     * pointer value is descriptor.data which is the first 8
     * bytes; load it. */
    struct Expression *src_expr = expr->expr_data.typecast_data.expr;
    int is_dynarray_src = src_expr->array_is_dynamic;
    if (!is_dynarray_src) {
      KgpcType *src_type = src_expr->resolved_kgpc_type;
      if (src_type != NULL && kgpc_type_is_dynamic_array(src_type))
        is_dynarray_src = 1;
    }
    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &addr_reg);
    if (addr_reg != NULL) {
      char buffer[64];
      if (is_dynarray_src) {
        /* descriptor.data is at offset 0; load via target. */
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64,
                 target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        if (addr_reg != target_reg)
          free_reg(get_reg_stack(), addr_reg);
      } else if (addr_reg != target_reg) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64,
                 target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
      }
    }
    return inst_list;
  } else if (expr->type == EXPR_TYPECAST &&
             expr->expr_data.typecast_data.expr != NULL &&
             expr->expr_data.typecast_data.target_type == REAL_TYPE) {
    /* Narrowing typecast Extended -> Double (preserved as a leaf by
     * build_expr_tree_internal).  Take the address of the inner
     * extended-typed source, call kgpc_load_extended_to_bits to perform
     * an FPU-precision narrowing conversion (fldt + fstpl), and leave
     * the resulting double bits in target_reg. */
    struct Expression *src_expr = expr->expr_data.typecast_data.expr;
    KgpcType *src_type = expr_get_kgpc_type(src_expr);
    if (src_type != NULL && kgpc_type_is_extended(src_type)) {
      Register_t *src_addr = NULL;
      inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_addr);
      if (src_addr != NULL && !codegen_had_error(ctx)) {
        char tmpl[128];
        if (codegen_target_is_windows())
          snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, %%rcx\n");
        else
          snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, %%rdi\n");
        Register_t *u[] = {src_addr};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, tmpl);
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(
            inst_list, "kgpc_load_extended_to_bits");
        free_arg_regs();
        {
          char res[64];
          snprintf(res, sizeof(res), "\tmovq\t%%rax, %s\n", target_reg->bit_64);
          inst_list = add_inst(inst_list, res);
        }
        free_reg(get_reg_stack(), src_addr);
        return inst_list;
      }
      if (src_addr != NULL)
        free_reg(get_reg_stack(), src_addr);
    }
  } else if (expr->type == EXPR_POINTER_DEREF) {
    return codegen_pointer_deref_leaf(expr, inst_list, ctx, target_reg);
  } else if (expr->type == EXPR_ADDR) {
    return codegen_addressof_leaf(expr, inst_list, ctx, target_reg);
  } else if (expr->type == EXPR_ADDR_OF_PROC) {
    /* Use owned string copies for the procedure label */
    const char *proc_label = expr->expr_data.addr_of_proc_data.proc_mangled_id;
    if (proc_label == NULL)
      proc_label = expr->expr_data.addr_of_proc_data.proc_id;
    if (ctx != NULL && ctx->symtab != NULL &&
        ctx->current_subprogram_owner_class != NULL &&
        expr->expr_data.addr_of_proc_data.proc_id != NULL &&
        proc_label != NULL && strchr(proc_label, '_') == NULL) {
      const char *impl_target = codegen_find_class_method_impl_id(
          ctx->symtab, NULL, ctx->current_subprogram_owner_class, NULL,
          expr->expr_data.addr_of_proc_data.proc_id);
      if (impl_target != NULL)
        proc_label = impl_target;
      else {
        char base_name[512];
        HashNode_t *prefix_match = NULL;
        snprintf(base_name, sizeof(base_name), "%s__%s",
                 ctx->current_subprogram_owner_class,
                 expr->expr_data.addr_of_proc_data.proc_id);
        if (FindIdentByPrefix(&prefix_match, ctx->symtab, base_name) != 0 &&
            prefix_match != NULL) {
          const char *emit_target =
              codegen_subprogram_emission_symbol(prefix_match);
          if (emit_target != NULL && emit_target[0] != '\0')
            proc_label = emit_target;
          else if (prefix_match->mangled_id != NULL &&
                   prefix_match->mangled_id[0] != '\0')
            proc_label = prefix_match->mangled_id;
        }
      }
    }
    assert(proc_label != NULL &&
           "EXPR_ADDR_OF_PROC must have proc_mangled_id or proc_id set");
    /* If the proc_label is a bare method name (no mangled_id), try to
     * resolve it in the current class context via the symbol table.
     * This handles @MethodName inside class methods from cached units. */
    char *resolved_label = NULL;
    if (expr->expr_data.addr_of_proc_data.proc_mangled_id == NULL &&
        ctx != NULL && ctx->symtab != NULL) {
      /* Try to find as a standalone function first */
      HashNode_t *sym = NULL;
      if (FindSymbol(&sym, ctx->symtab, proc_label) != 0 && sym != NULL &&
          sym->mangled_id != NULL && sym->type != NULL &&
          sym->type->kind == TYPE_KIND_PROCEDURE) {
        proc_label = sym->mangled_id;
      } else {
        /* Try class-qualified: look for ClassName__MethodName */
        const char *owner = ctx->current_subprogram_owner_class;
        if (owner == NULL) {
          HashNode_t *self_node = NULL;
          if (FindSymbol(&self_node, ctx->symtab, "Self") != 0 &&
              self_node != NULL)
            owner = self_node->owner_class;
        }
        if (owner != NULL) {
          int needed = snprintf(NULL, 0, "%s__%s", owner, proc_label) + 1;
          resolved_label = malloc(needed);
          if (resolved_label != NULL) {
            snprintf(resolved_label, needed, "%s__%s", owner, proc_label);
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
    }
    /* If proc_label is an unprefixed mangled name but the definition was
     * unit-qualified (unit$$ prefix), resolve to the prefixed version.
     * When source_unit_index is set on the expression, prefer the
     * candidate from the same unit to avoid cross-unit collisions
     * (e.g. comprsrc.initglobals vs globals.initglobals). */
    char *collision_label = NULL;
    if (proc_label != NULL && !mangled_id_has_unit_prefix(proc_label) &&
        ctx != NULL && ctx->symtab != NULL) {
      const char *lookup_id = expr->expr_data.addr_of_proc_data.proc_id;
      int target_unit = expr->expr_data.addr_of_proc_data.source_unit_index;
      if (lookup_id != NULL) {
        ListNode_t *candidates = FindAllIdents(ctx->symtab, lookup_id);
        /* First pass: prefer candidate from the same unit as the
         * symbol that semcheck resolved. */
        if (target_unit > 0) {
          for (ListNode_t *c = candidates; c != NULL; c = c->next) {
            HashNode_t *cand = (HashNode_t *)c->cur;
            if (cand == NULL || cand->mangled_id == NULL)
              continue;
            if (cand->source_unit_index != target_unit)
              continue;
            if (!mangled_id_has_unit_prefix(cand->mangled_id))
              continue;
            if (strcmp(mangled_id_get_base(cand->mangled_id), proc_label) ==
                    0 &&
                cand->type != NULL && cand->type->kind == TYPE_KIND_PROCEDURE &&
                cand->type->info.proc_info.definition != NULL &&
                cand->type->info.proc_info.definition->tree_data.subprogram_data
                        .statement_list != NULL) {
              collision_label = strdup(cand->mangled_id);
              proc_label = collision_label;
              break;
            }
          }
        }
        /* Fallback: take any candidate with a unit$$ prefix. */
        if (collision_label == NULL) {
          for (ListNode_t *c = candidates; c != NULL; c = c->next) {
            HashNode_t *cand = (HashNode_t *)c->cur;
            if (cand == NULL || cand->mangled_id == NULL)
              continue;
            if (!mangled_id_has_unit_prefix(cand->mangled_id))
              continue;
            if (strcmp(mangled_id_get_base(cand->mangled_id), proc_label) ==
                    0 &&
                cand->type != NULL && cand->type->kind == TYPE_KIND_PROCEDURE &&
                cand->type->info.proc_info.definition != NULL &&
                cand->type->info.proc_info.definition->tree_data.subprogram_data
                        .statement_list != NULL) {
              collision_label = strdup(cand->mangled_id);
              proc_label = collision_label;
              break;
            }
          }
        }
        if (candidates != NULL)
          DestroyList(candidates);
      }
    }
    /* If proc_label came from proc_mangled_id but the actual function
     * definition has a different (longer) mangled_id — e.g. proc_mangled_id
     * is "pd_abstract_u" but the function is emitted as
     * "pd_abstract_u_tabstractprocdef" — resolve via the symbol table's
     * definition tree to get the correct emission label. */
    if (proc_label != NULL && ctx != NULL && ctx->symtab != NULL &&
        expr->expr_data.addr_of_proc_data.proc_id != NULL) {
      HashNode_t *sym = NULL;
      const char *lookup = expr->expr_data.addr_of_proc_data.proc_id;
      if (FindSymbol(&sym, ctx->symtab, lookup) != 0 && sym != NULL &&
          sym->type != NULL && sym->type->kind == TYPE_KIND_PROCEDURE &&
          sym->type->info.proc_info.definition != NULL) {
        Tree_t *def = sym->type->info.proc_info.definition;
        const char *def_mangled = def->tree_data.subprogram_data.mangled_id;
        if (def_mangled != NULL && def_mangled[0] != '\0' &&
            strcmp(def_mangled, proc_label) != 0) {
          proc_label = def_mangled;
        }
      }
    }
    /* If a receiver expression is attached, this is a method pointer
     * (@obj.Method): build a TMethod = { code: pointer; data: pointer }
     * 16-byte struct on the stack, write code at offset 0, Self at
     * offset 8, then return the ADDRESS of the struct in target_reg.
     * The callee passes the struct pointer like any other 16-byte
     * record argument. */
    int receiver_is_type_reference = 0;
    if (expr->expr_data.addr_of_proc_data.receiver_expr != NULL &&
        expr->expr_data.addr_of_proc_data.receiver_expr->type == EXPR_VAR_ID &&
        expr->expr_data.addr_of_proc_data.receiver_expr->expr_data.id != NULL &&
        ctx != NULL && ctx->symtab != NULL) {
      HashNode_t *receiver_node = NULL;
      if (FindSymbol(
              &receiver_node, ctx->symtab,
              expr->expr_data.addr_of_proc_data.receiver_expr->expr_data.id) !=
              0 &&
          receiver_node != NULL && receiver_node->hash_type == HASHTYPE_TYPE) {
        receiver_is_type_reference = 1;
      }
    }

    if (expr->expr_data.addr_of_proc_data.receiver_expr != NULL &&
        !receiver_is_type_reference) {
      StackNode_t *tm_slot = add_l_t_bytes("__tmethod_temp", 16);
      if (tm_slot == NULL) {
        if (collision_label != NULL)
          free(collision_label);
        if (resolved_label != NULL)
          free(resolved_label);
        return inst_list;
      }
      /* Step 1: load the code address into a scratch reg, then
       * store at offset 0 of the temp. */
      Register_t *code_reg = get_free_reg(get_reg_stack(), &inst_list);
      if (code_reg == NULL) {
        if (collision_label != NULL)
          free(collision_label);
        if (resolved_label != NULL)
          free(resolved_label);
        return inst_list;
      }
      snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", proc_label,
               code_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
               code_reg->bit_64, tm_slot->offset);
      inst_list = add_inst(inst_list, buffer);
      free_reg(get_reg_stack(), code_reg);

      /* Step 2: store the receiver Self pointer at offset 8 of the temp.
       * Classes already evaluate to an instance pointer. Object/record
       * receivers pass their address as Self. */
      Register_t *self_reg = NULL;
      struct RecordType *receiver_record = expr_tree_record_from_expr(
          ctx, expr->expr_data.addr_of_proc_data.receiver_expr);
      if (receiver_record != NULL && !record_type_is_class(receiver_record))
        inst_list = codegen_address_for_expr(
            expr->expr_data.addr_of_proc_data.receiver_expr, inst_list, ctx,
            &self_reg);
      else
        inst_list = codegen_expr_with_result(
            expr->expr_data.addr_of_proc_data.receiver_expr, inst_list, ctx,
            &self_reg);
      if (self_reg == NULL) {
        if (collision_label != NULL)
          free(collision_label);
        if (resolved_label != NULL)
          free(resolved_label);
        return inst_list;
      }
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
               self_reg->bit_64, tm_slot->offset - 8);
      inst_list = add_inst(inst_list, buffer);
      free_reg(get_reg_stack(), self_reg);

      /* Step 3: load the address of the temp into target_reg. */
      snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
               tm_slot->offset, target_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);

      if (collision_label != NULL)
        free(collision_label);
      if (resolved_label != NULL)
        free(resolved_label);
      return inst_list;
    }
    /* Use leaq (Load Effective Address) with RIP-relative addressing to get the
     * address of the procedure's label */
    snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", proc_label,
             target_reg->bit_64);
    if (collision_label != NULL)
      free(collision_label);
    if (resolved_label != NULL)
      free(resolved_label);
    return add_inst(inst_list, buffer);
  } else if (expr->type == EXPR_RECORD_CONSTRUCTOR ||
             expr->type == EXPR_ARRAY_LITERAL) {
    /* Record constructors and array literals are compound values that need
     * to be materialized into temporary storage.  They are not scalar
     * expressions, so delegate to codegen_address_for_expr which already
     * knows how to allocate a temp, fill it in and return its address. */
    Register_t *addr_reg = NULL;
    codegen_begin_expression(ctx);
    inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
    codegen_end_expression(ctx);
    if (addr_reg != NULL) {
      if (addr_reg != target_reg) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64,
                 target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
      }
    }
    return inst_list;
  } else if (expr->type == EXPR_ANONYMOUS_FUNCTION ||
             expr->type == EXPR_ANONYMOUS_PROCEDURE) {
    /* Anonymous methods:
     * 1. Generate the function/procedure code as a nested definition
     * 2. Return a pointer to the generated function
     *
     * The function is generated immediately and we return its address.
     */

    /* First, generate the anonymous function body */
    codegen_anonymous_method(expr, ctx, ctx->symtab);

    /* Check if generation succeeded */
    if (codegen_had_error(ctx)) {
      return inst_list;
    }

    /* Now load the address of the generated function into the target register
     */
    struct AnonymousMethod *anon = &expr->expr_data.anonymous_method_data;
    if (anon->generated_name == NULL) {
      codegen_report_error(ctx,
                           "ERROR: Anonymous method missing generated name");
      return inst_list;
    }

    /* Use leaq (Load Effective Address) with RIP-relative addressing to get the
     * address */
    snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
             anon->generated_name, target_reg->bit_64);
    return add_inst(inst_list, buffer);
  } else if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL) {
    /* Check if this is a string constant reference (but not a procedure address
     * constant) */
    HashNode_t *node = expr_tree_find_preferred_symbol(ctx, expr);
    if (node != NULL)
      node =
          codegen_prefer_visible_var_over_const(ctx, expr->expr_data.id, node);
    if (node != NULL && node->hash_type == HASHTYPE_CONST &&
        node->const_string_value != NULL &&
        /* Skip if this is a procedure address constant - those use
         * const_string_value to store the procedure name, not an actual string
         * value */
        !(node->type != NULL && node->type->kind == TYPE_KIND_PROCEDURE)) {
      /* Check if this is a single-char constant (Char type, not String).
       * However, if the expression's resolved type is STRING_TYPE (e.g. the
       * semantic checker promoted sLineBreak from Char to String for a
       * comparison), emit it as a string constant, not a raw integer. */
      if (node->type != NULL && node->type->kind == TYPE_KIND_PRIMITIVE &&
          node->type->info.primitive_type_tag == CHAR_TYPE &&
          expr_get_type_tag(expr) != STRING_TYPE) {
        /* Char constant - load the character value directly as an immediate */
        unsigned char ch = (unsigned char)node->const_string_value[0];
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", (int)ch,
                 target_reg->bit_32);
        return add_inst(inst_list, buffer);
      }
      /* String constant - treat it like a string literal */
      return expr_tree_emit_string_literal_address(
          inst_list, ctx, target_reg, node->const_string_value,
          expr_tree_should_emit_shortstring_literal(expr, node));
    }
  } else if (expr->type == EXPR_SET) {
    Register_t *set_reg = NULL;
    inst_list = codegen_set_literal(expr, inst_list, ctx, &set_reg, 0);
    if (set_reg == NULL)
      return inst_list;

    if (set_reg != target_reg) {
      int is_char_set = expr_is_char_set_ctx(expr, ctx);
      const char *src_reg = is_char_set ? set_reg->bit_64 : set_reg->bit_32;
      const char *dst_reg =
          is_char_set ? target_reg->bit_64 : target_reg->bit_32;
      snprintf(buffer, sizeof(buffer), "\tmov%c\t%s, %s\n",
               is_char_set ? 'q' : 'l', src_reg, dst_reg);
      inst_list = add_inst(inst_list, buffer);
      free_reg(get_reg_stack(), set_reg);
    }
    return inst_list;
  } else if (expr->type == EXPR_STRING) {
    if (expr_get_type_tag(expr) == CHAR_TYPE) {
      unsigned char value = 0;
      if (expr->expr_data.string != NULL && expr->expr_data.string[0] != '\0')
        value = (unsigned char)expr->expr_data.string[0];
      snprintf(buffer, sizeof(buffer), "\tmovl\t$%u, %s\n", (unsigned)value,
               target_reg->bit_32);
      return add_inst(inst_list, buffer);
    }

    return expr_tree_emit_string_literal_address(
        inst_list, ctx, target_reg, expr->expr_data.string,
        expr_tree_should_emit_shortstring_literal(expr, NULL));
  } else if (expr->type == EXPR_TYPEINFO) {
    const char *type_id = expr->expr_data.typeinfo_data.type_id;
    if (type_id == NULL || type_id[0] == '\0') {
      codegen_report_error(ctx, "ERROR: TypeInfo missing type identifier.");
      return inst_list;
    }
    char label[CODEGEN_MAX_INST_BUF];
    codegen_typeinfo_label_for_type_id(ctx != NULL ? ctx->symtab : NULL,
                                       type_id, label, sizeof(label));
    int buf_len =
        snprintf(NULL, 0, "\tleaq\t%s(%%rip), %s\n", label, target_reg->bit_64);
    if (buf_len > 0) {
      char *tmp_buf = (char *)malloc((size_t)buf_len + 1);
      if (tmp_buf != NULL) {
        snprintf(tmp_buf, (size_t)buf_len + 1, "\tleaq\t%s(%%rip), %s\n", label,
                 target_reg->bit_64);
        inst_list = add_inst(inst_list, tmp_buf);
        free(tmp_buf);
      }
    }
    return inst_list;
  } else if (expr->type == EXPR_IS) {
    /* EXPR_IS is a complex runtime expression that cannot be inlined as
     * a simple leaf operand. Emit the VMT-based type check and move
     * the boolean result to the target register. */
    inst_list = codegen_emit_is_expr(expr, inst_list, ctx, NULL);
    /* codegen_emit_is_expr leaves result in %eax (0 or 1) */
    if (target_reg->reg_id != REG_RAX) {
      char mov_buf[128];
      snprintf(mov_buf, sizeof(mov_buf), "\tmovl\t%%eax, %s\n",
               target_reg->bit_32);
      inst_list = add_inst(inst_list, mov_buf);
    }
    return inst_list;
  } else if (expr->type == EXPR_AS) {
    /* EXPR_AS performs a checked class cast at runtime. Evaluate the
     * inner expression and emit the cast check. */
    if (expr->expr_data.as_data.expr != NULL) {
      Register_t *addr_reg = NULL;
      inst_list = codegen_address_for_expr(expr->expr_data.as_data.expr,
                                           inst_list, ctx, &addr_reg);
      if (addr_reg != NULL) {
        inst_list = codegen_emit_class_cast_check_from_address(expr, inst_list,
                                                               ctx, addr_reg);
        if (target_reg->reg_id != addr_reg->reg_id) {
          char mov_buf[128];
          snprintf(mov_buf, sizeof(mov_buf), "\tmovq\t%s, %s\n",
                   addr_reg->bit_64, target_reg->bit_64);
          inst_list = add_inst(inst_list, mov_buf);
        }
        free_reg(get_reg_stack(), addr_reg);
      }
    }
    return inst_list;
  }

  inst_list =
      gencode_leaf_var(expr, inst_list, ctx, buf_leaf, sizeof(buf_leaf), NULL);

  if (expr->type == EXPR_VAR_ID) {
    int scope_depth = 0;
    StackNode_t *stack_node = NULL;
    HashNode_t *symbol_node = NULL;
    if (ctx != NULL && ctx->symtab != NULL)
      FindSymbol(&symbol_node, ctx->symtab, expr->expr_data.id);

    /* When the leaf was already resolved to an immediate constant, there is
     * no stack slot to find and scanning every scope becomes pathological for
     * large typed-const initializers like the x86 instruction tables. */
    if (buf_leaf[0] != '$')
      stack_node = find_label_with_depth(expr->expr_data.id, &scope_depth);

    long long storage_size = 0;
    /* For by-reference (var/out/constref) parameters the stack slot holds a
     * pointer, so stack_node->size is the slot/pointer width (4 or 8), not the
     * width of the pointed-to value.  Using it to size the dereferencing load
     * reads too many bytes for narrow types (e.g. a `var shortint` read as a
     * 4-byte movl picks up adjacent bytes).  Derive the load width from the
     * value's own type instead. */
    if (stack_node != NULL && !stack_node->is_reference && !stack_node->is_array &&
        !stack_node->is_dynamic && stack_node->size > 0)
      storage_size = stack_node->size;
    if (storage_size <= 0)
      storage_size = expr_effective_storage_size_ctx(expr, ctx);

    /* Procedures/functions used as values (e.g. @Proc, typed proc constants).
     * Only apply when the identifier is not a local/stack variable in this
     * scope, otherwise this breaks function result variables that share the
     * function name. Skip if gencode_leaf_var already resolved the identifier
     * to a constant immediate (e.g. Pi from FPC internproc shadowed by a
     * builtin real constant). Also skip when inside a class method and the bare
     * name is a field of Self — otherwise a global function with the same name
     * shadows the field (e.g. VarFreeMap.L0 field vs global function l0 from
     * msg2inc). */
    if (stack_node == NULL && symbol_node != NULL &&
        (symbol_node->hash_type == HASHTYPE_PROCEDURE ||
         symbol_node->hash_type == HASHTYPE_FUNCTION) &&
        symbol_node->mangled_id != NULL && buf_leaf[0] != '$') {
      int is_self_field = 0;
      if (ctx != NULL && ctx->current_subprogram_owner_class != NULL &&
          ctx->symtab != NULL) {
        struct RecordType *owner_record = semcheck_lookup_record_type(
            ctx->symtab, ctx->current_subprogram_owner_class);
        if (owner_record == NULL &&
            ctx->current_subprogram_owner_class_full != NULL) {
          owner_record = semcheck_lookup_record_type(
              ctx->symtab, ctx->current_subprogram_owner_class_full);
        }
        if (owner_record != NULL) {
          struct RecordField *field_desc = NULL;
          long long field_offset = 0;
          if (resolve_record_field(ctx->symtab, owner_record,
                                   expr->expr_data.id, &field_desc,
                                   &field_offset, 0, 1) == 0 &&
              field_desc != NULL) {
            is_self_field = 1;
          }
        }
      }
      if (!is_self_field) {
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                 symbol_node->mangled_id, target_reg->bit_64);
        return add_inst(inst_list, buffer);
      }
    }
    /* Bare method name used as a procedural reference (e.g., @SetStatus
     * inside a class method).  The symtab might not have a procedure
     * entry for the bare name; try class-qualified lookup. */
    if (stack_node == NULL && symbol_node == NULL && buf_leaf[0] != '$' &&
        ctx != NULL && ctx->symtab != NULL) {
      const char *owner = ctx->current_subprogram_owner_class;
      if (owner != NULL) {
        char qual_name[512];
        snprintf(qual_name, sizeof(qual_name), "%s__%s", owner,
                 expr->expr_data.id);
        ListNode_t *candidates = FindAllIdents(ctx->symtab, qual_name);
        for (ListNode_t *c = candidates; c != NULL; c = c->next) {
          HashNode_t *cand = (HashNode_t *)c->cur;
          if (cand != NULL && cand->mangled_id != NULL && cand->type != NULL &&
              cand->type->kind == TYPE_KIND_PROCEDURE) {
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                     cand->mangled_id, target_reg->bit_64);
            if (candidates != NULL)
              DestroyList(candidates);
            return add_inst(inst_list, buffer);
          }
        }
        if (candidates != NULL)
          DestroyList(candidates);
      }
    }

    /* Check if this is a procedure address constant - need leaq to get the
     * label address */
    if (symbol_node != NULL && symbol_node->hash_type == HASHTYPE_CONST &&
        symbol_node->type != NULL &&
        symbol_node->type->kind == TYPE_KIND_PROCEDURE) {
      /* For procedure address constants, use leaq to get the procedure's
       * address */
      snprintf(buffer, sizeof(buffer), "\tleaq\t%s, %s\n", buf_leaf,
               target_reg->bit_64);
      return add_inst(inst_list, buffer);
    }

    /* Check if this is a class type used as a value (for class references).
     * The buf_leaf will be "ClassName_VMT(%rip)" and we need leaq to get the
     * address. */
    if (symbol_node != NULL && symbol_node->hash_type == HASHTYPE_TYPE &&
        symbol_node->type != NULL &&
        symbol_node->type->kind == TYPE_KIND_POINTER &&
        symbol_node->type->info.points_to != NULL &&
        symbol_node->type->info.points_to->kind == TYPE_KIND_RECORD &&
        symbol_node->type->info.points_to->info.record_info != NULL &&
        record_type_is_class(
            symbol_node->type->info.points_to->info.record_info)) {
      /* For class type used as value, use leaq to get the VMT address */
      snprintf(buffer, sizeof(buffer), "\tleaq\t%s, %s\n", buf_leaf,
               target_reg->bit_64);
      return add_inst(inst_list, buffer);
    }

    /* Check if this is a VMT label - need address, not value */
    const char *var_name = expr->expr_data.id;
    size_t name_len = var_name != NULL ? strlen(var_name) : 0;
    int is_vmt_label =
        (name_len > 4 && strcmp(var_name + name_len - 4, "_VMT") == 0);

    if (is_vmt_label) {
      /* For VMT labels, use leaq to get the address instead of loading the
       * value */
      snprintf(buffer, sizeof(buffer), "\tleaq\t%s, %s\n", buf_leaf,
               target_reg->bit_64);
      return add_inst(inst_list, buffer);
    }

    int treat_as_reference = 0;
    if (stack_node != NULL && stack_node->is_reference)
      treat_as_reference = 1;
    else if (symbol_node != NULL && symbol_node->is_var_parameter)
      treat_as_reference = 1;
    if (treat_as_reference && symbol_node != NULL &&
        expr->expr_data.id != NULL &&
        pascal_identifier_equals(expr->expr_data.id, "Self") &&
        symbol_node->type != NULL) {
      struct RecordType *self_record = NULL;
      if (kgpc_type_is_pointer(symbol_node->type) &&
          symbol_node->type->info.points_to != NULL &&
          symbol_node->type->info.points_to->kind == TYPE_KIND_RECORD) {
        self_record = symbol_node->type->info.points_to->info.record_info;
      } else if (symbol_node->type->kind == TYPE_KIND_RECORD) {
        self_record = symbol_node->type->info.record_info;
      }
      if (self_record != NULL && record_type_is_class(self_record))
        treat_as_reference = 0;
    }

    if (treat_as_reference) {
      int expr_type = expr_get_type_tag(expr);
      if (expr_type == UNKNOWN_TYPE && symbol_node != NULL &&
          symbol_node->type != NULL)
        expr_type = expr_tree_tag_from_kgpc(symbol_node->type);

      int is_array_like = expr->array_is_dynamic || expr->is_array_expr ||
                          (expr->resolved_kgpc_type != NULL &&
                           kgpc_type_is_array(expr->resolved_kgpc_type));

      int is_shortstring = 0;
      if (expr_type == SHORTSTRING_TYPE)
        is_shortstring = 1;
      else if (expr->resolved_kgpc_type != NULL) {
        struct TypeAlias *alias =
            kgpc_type_get_type_alias(expr->resolved_kgpc_type);
        if (kgpc_type_is_shortstring(expr->resolved_kgpc_type) ||
            (alias != NULL && alias->is_shortstring)) {
          is_shortstring = 1;
        }
      }
      /* Also check the symbol table for {$H-} string locals */
      if (!is_shortstring && symbol_node != NULL && symbol_node->type != NULL) {
        if (kgpc_type_is_shortstring(symbol_node->type))
          is_shortstring = 1;
        else {
          struct TypeAlias *alias = kgpc_type_get_type_alias(symbol_node->type);
          if (alias != NULL && alias->is_shortstring)
            is_shortstring = 1;
        }
      }

      int should_deref = 0;
      if (!is_array_like && !is_shortstring && expr_type != RECORD_TYPE &&
          expr_type != SET_TYPE)
        should_deref = 1;

      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", buf_leaf,
               target_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);

      if (!should_deref)
        return inst_list;

      if (expr_type == REAL_TYPE &&
          expr_is_single_real_with_symtab(expr,
                                          ctx != NULL ? ctx->symtab : NULL)) {
        char mem_operand[64];
        snprintf(mem_operand, sizeof(mem_operand), "(%s)", target_reg->bit_64);
        inst_list = load_real_operand_into_xmm(ctx, expr, mem_operand, NULL,
                                               "%xmm0", inst_list);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n",
                 target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        return inst_list;
      }

      if (expr_type == EXTENDED_TYPE ||
          (expr->resolved_kgpc_type != NULL &&
           kgpc_type_is_extended(expr->resolved_kgpc_type))) {
        if (codegen_target_is_windows())
          snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n",
                   target_reg->bit_64);
        else
          snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n",
                   target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(
            inst_list, "kgpc_load_extended_to_bits");
        free_arg_regs();
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n",
                 target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        return inst_list;
      }

      char load_value[80];
      int use_qword = expr_requires_qword(expr) ||
                      codegen_type_uses_qword(expr_type) ||
                      expr_type == UNKNOWN_TYPE;
      if (use_qword) {
        snprintf(load_value, sizeof(load_value), "\tmovq\t(%s), %s\n",
                 target_reg->bit_64, target_reg->bit_64);
      } else if (expr_type == CHAR_TYPE || expr_type == BOOL) {
        snprintf(load_value, sizeof(load_value), "\tmovzbl\t(%s), %s\n",
                 target_reg->bit_64, target_reg->bit_32);
      } else if (storage_size == 2) {
        if (codegen_type_is_signed(expr_type))
          snprintf(load_value, sizeof(load_value), "\tmovswl\t(%s), %s\n",
                   target_reg->bit_64, target_reg->bit_32);
        else
          snprintf(load_value, sizeof(load_value), "\tmovzwl\t(%s), %s\n",
                   target_reg->bit_64, target_reg->bit_32);
      } else if (storage_size == 1) {
        if (codegen_type_is_signed(expr_type))
          snprintf(load_value, sizeof(load_value), "\tmovsbl\t(%s), %s\n",
                   target_reg->bit_64, target_reg->bit_32);
        else
          snprintf(load_value, sizeof(load_value), "\tmovzbl\t(%s), %s\n",
                   target_reg->bit_64, target_reg->bit_32);
      } else {
        snprintf(load_value, sizeof(load_value), "\tmovl\t(%s), %s\n",
                 target_reg->bit_64, target_reg->bit_32);
      }

      inst_list = add_inst(inst_list, load_value);
      return inst_list;
    }

    int is_shortstring = expr_is_shortstring_storage_ctx(expr, ctx);
    if (is_shortstring || expr_is_char_array_expr(expr)) {
      if (buf_leaf[0] != '$') {
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s, %s\n", buf_leaf,
                 target_reg->bit_64);
        return add_inst(inst_list, buffer);
      }
    }
  }

  if (expr_has_extended_storage(expr) && buf_leaf[0] != '$') {
    if (codegen_target_is_windows())
      snprintf(buffer, sizeof(buffer), "%s\t%s, %%rcx\n",
               (buf_leaf[0] == '%') ? "\tmovq" : "\tleaq", buf_leaf);
    else
      snprintf(buffer, sizeof(buffer), "%s\t%s, %%rdi\n",
               (buf_leaf[0] == '%') ? "\tmovq" : "\tleaq", buf_leaf);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list =
        codegen_call_with_shadow_space(inst_list, "kgpc_load_extended_to_bits");
    free_arg_regs();
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", target_reg->bit_64);
    return add_inst(inst_list, buffer);
  }

  if (expr_has_type_tag(expr, REAL_TYPE) &&
      expr_is_single_real_with_symtab(expr, ctx != NULL ? ctx->symtab : NULL) &&
      buf_leaf[0] != '$') {
    inst_list = load_real_operand_into_xmm(ctx, expr, buf_leaf, NULL, "%xmm0",
                                           inst_list);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n",
             target_reg->bit_64);
    return add_inst(inst_list, buffer);
  }

  /* Use expr_requires_qword to check both type tag and storage_size.
   * This properly handles Int64/QWord/UInt64 which have storage_size=8 */
  int desired_qword = expr_requires_qword(expr);
  int storage_tag = expr_effective_storage_type(expr, ctx);
  int storage_qword = 0;
  if (expr != NULL)
    storage_qword = expr_uses_qword_kgpctype(expr);

  /* For procedural var calls, check if return type is a pointer */
  if (!storage_qword && expr != NULL && expr->type == EXPR_FUNCTION_CALL &&
      expr->expr_data.function_call_data.is_procedural_var_call &&
      expr->expr_data.function_call_data.call_kgpc_type != NULL) {
    KgpcType *call_type = expr->expr_data.function_call_data.call_kgpc_type;
    if (call_type->kind == TYPE_KIND_PROCEDURE) {
      KgpcType *ret_type = kgpc_type_get_return_type(call_type);
      if (ret_type != NULL && kgpc_type_uses_qword(ret_type))
        storage_qword = 1;
      else if (call_type->info.proc_info.return_type_id != NULL) {
        const char *ret_id = call_type->info.proc_info.return_type_id;
        if (kgpc_type_id_uses_qword(ret_id, ctx->symtab))
          storage_qword = 1;
      }
    }
  }

  if (!storage_qword)
    storage_qword = codegen_type_uses_qword(storage_tag);
  if (!desired_qword && storage_qword)
    desired_qword = 1;
  int is_immediate = (buf_leaf[0] == '$');

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: Loading value %s into register %s (desired_qword=%d, "
                "storage_qword=%d)\n",
                buf_leaf,
                desired_qword ? target_reg->bit_64 : target_reg->bit_32,
                desired_qword, storage_qword);
#endif

  if (desired_qword && !storage_qword) {
    if (is_immediate) {
      long long imm_value = 0;
      if (expr != NULL) {
        switch (expr->type) {
        case EXPR_INUM:
          imm_value = expr->expr_data.i_num;
          break;
        case EXPR_BOOL:
          imm_value = expr->expr_data.bool_value ? 1 : 0;
          break;
        case EXPR_CHAR_CODE:
          imm_value = (unsigned int)expr->expr_data.char_code;
          break;
        default:
          imm_value = strtoll(buf_leaf + 1, NULL, 10);
          break;
        }
      } else {
        imm_value = strtoll(buf_leaf + 1, NULL, 10);
      }
      snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", imm_value,
               target_reg->bit_64);
    } else {
      if (expr != NULL && !codegen_expr_is_signed(expr))
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", buf_leaf,
                 target_reg->bit_32);
      else
        snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", buf_leaf,
                 target_reg->bit_64);
    }
    return add_inst(inst_list, buffer);
  }

  if (desired_qword) {
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", buf_leaf,
             target_reg->bit_64);
    return add_inst(inst_list, buffer);
  }

  if (storage_tag == CHAR_TYPE && !is_immediate) {
    snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", buf_leaf,
             target_reg->bit_32);
    return add_inst(inst_list, buffer);
  }

  /* Check if immediate value requires 64 bits */
  if (is_immediate) {
    long long imm_value = strtoll(buf_leaf + 1, NULL, 10);
    if (imm_value > 2147483647LL || imm_value < -2147483648LL) {
      /* Value doesn't fit in 32 bits - use 64-bit move */
      snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", imm_value,
               target_reg->bit_64);
      return add_inst(inst_list, buffer);
    }
  }

  /* For sub-dword memory operands, use appropriately sized loads */
  if (!is_immediate && expr != NULL) {
    long long sz = expr_effective_storage_size_ctx(expr, ctx);
    if (expr->type == EXPR_VAR_ID) {
      int scope_depth = 0;
      StackNode_t *stack_node =
          find_label_with_depth(expr->expr_data.id, &scope_depth);
      if (stack_node != NULL && !stack_node->is_array &&
          !stack_node->is_dynamic && stack_node->size > 0)
        sz = stack_node->size;
    }
    if (sz == 2) {
      if (codegen_type_is_signed(storage_tag))
        snprintf(buffer, sizeof(buffer), "\tmovswl\t%s, %s\n", buf_leaf,
                 target_reg->bit_32);
      else
        snprintf(buffer, sizeof(buffer), "\tmovzwl\t%s, %s\n", buf_leaf,
                 target_reg->bit_32);
      return add_inst(inst_list, buffer);
    } else if (sz == 1 && storage_tag != CHAR_TYPE) {
      if (codegen_type_is_signed(storage_tag))
        snprintf(buffer, sizeof(buffer), "\tmovsbl\t%s, %s\n", buf_leaf,
                 target_reg->bit_32);
      else
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", buf_leaf,
                 target_reg->bit_32);
      return add_inst(inst_list, buffer);
    }
  }

  snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", buf_leaf,
           target_reg->bit_32);
  return add_inst(inst_list, buffer);
}

/* right node is a leaf */
ListNode_t *gencode_case1(expr_node_t *node, ListNode_t *inst_list,
                          CodeGenContext *ctx, Register_t *target_reg) {
#ifdef DEBUG_CODEGEN
  fprintf(stderr, "gencode_case1\n");
#endif
  assert(node != NULL);
  assert(node->expr != NULL);
  assert(node->right_expr != NULL);
  assert(node->right_expr->expr != NULL);
  assert(ctx != NULL);
  assert(target_reg != NULL);

  char buffer[256];
  char name_buf[128];
  struct Expression *expr, *right_expr;

  expr = node->expr;
  right_expr = node->right_expr->expr;
  struct Expression *left_expr =
      node->left_expr != NULL ? node->left_expr->expr : NULL;
  assert(left_expr != NULL);
  assert(right_expr != NULL);
  int rhs_requires_reference =
      leaf_expr_requires_reference_value(right_expr, ctx);

  if (!leaf_expr_is_simple(right_expr) || rhs_requires_reference) {
    Register_t *rhs_reg = expr_tree_try_get_temp_reg(&inst_list, target_reg);
    if (rhs_reg == NULL) {
      StackNode_t *spill_loc = add_l_t("rhs");
      inst_list =
          gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);
      snprintf(name_buf, sizeof(name_buf), "-%d(%%rbp)", spill_loc->offset);
      const char *tmp_name = select_register_name(
          target_reg, right_expr, expr_get_type_tag(right_expr));
      if (tmp_name != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmov%s\t%s, %s\n",
                 expr_uses_qword_kgpctype(right_expr)
                     ? "q"
                     : (codegen_type_uses_qword(expr_get_type_tag(right_expr))
                            ? "q"
                            : "l"),
                 tmp_name, name_buf);
        inst_list = add_inst(inst_list, buffer);
      }
      inst_list =
          gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
      const char *target_name =
          select_register_name(target_reg, left_expr,
                               left_expr != NULL ? expr_get_type_tag(left_expr)
                                                 : expr_get_type_tag(expr));
      inst_list = gencode_op(expr, target_name, target_reg, name_buf, NULL,
                             OPKIND_REGISTER, OPKIND_MEMORY, inst_list, ctx);
    } else {
      StackNode_t *lhs_spill = add_l_t("case1_lhs");
      inst_list =
          gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
      /* Use the left operand's type for spilling, not the binary expr's result
       * type */
      inst_list =
          emit_store_to_stack(inst_list, target_reg, left_expr,
                              expr_get_type_tag(left_expr), lhs_spill->offset);
      inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, rhs_reg);
      inst_list = emit_load_from_stack(inst_list, target_reg, left_expr,
                                       expr_get_type_tag(left_expr),
                                       lhs_spill->offset, ctx);
      const char *target_name = select_register_name(
          target_reg, left_expr, expr_get_type_tag(left_expr));
      const char *rhs_name = select_register_name(
          rhs_reg, right_expr, expr_get_type_tag(right_expr));
      inst_list = gencode_op(expr, target_name, target_reg, rhs_name, rhs_reg,
                             OPKIND_REGISTER, OPKIND_REGISTER, inst_list, ctx);
      free_reg(get_reg_stack(), rhs_reg);
    }
    return inst_list;
  }

  inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

  OperandKind rhs_kind = OPKIND_LABEL;
  inst_list = gencode_leaf_var(right_expr, inst_list, ctx, name_buf,
                               sizeof(name_buf), &rhs_kind);

  const char *target_name =
      select_register_name(target_reg, left_expr,
                           left_expr != NULL ? expr_get_type_tag(left_expr)
                                             : expr_get_type_tag(expr));
  inst_list = gencode_op(expr, target_name, target_reg, name_buf, NULL,
                         OPKIND_REGISTER, rhs_kind, inst_list, ctx);

  return inst_list;
}

ListNode_t *gencode_case2(expr_node_t *node, ListNode_t *inst_list,
                          CodeGenContext *ctx, Register_t *target_reg) {
#ifdef DEBUG_CODEGEN
  fprintf(stderr, "gencode_case2\n");
#endif
  assert(node != NULL);
  assert(node->expr != NULL);
  /* Note: inst_list can be NULL at the start of code generation */
  assert(ctx != NULL);
  assert(target_reg != NULL);

  Register_t *temp_reg;
  struct Expression *right_expr = node->right_expr->expr;
  struct Expression *left_expr = node->left_expr->expr;

  assert(left_expr != NULL);
  assert(right_expr != NULL);

  temp_reg = expr_tree_try_get_temp_reg(&inst_list, target_reg);
  if (temp_reg == NULL) {
    inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);

    StackNode_t *spill_loc = add_l_t("spill");
    /* Use right operand's type for spilling, not the binary expr's result type
     */
    inst_list =
        emit_store_to_stack(inst_list, target_reg, right_expr,
                            expr_get_type_tag(right_expr), spill_loc->offset);

    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);

    char spill_mem[30];
    snprintf(spill_mem, 30, "-%d(%%rbp)", spill_loc->offset);
    const char *target_name = select_register_name(
        target_reg, left_expr, expr_get_type_tag(left_expr));
    inst_list = gencode_op(node->expr, target_name, target_reg, spill_mem, NULL,
                           OPKIND_REGISTER, OPKIND_MEMORY, inst_list, ctx);
  } else {
    StackNode_t *rhs_spill = add_l_t("case2_rhs");
    inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, temp_reg);
    /* Use right operand's type for spilling, not the binary expr's result type
     */
    inst_list =
        emit_store_to_stack(inst_list, temp_reg, right_expr,
                            expr_get_type_tag(right_expr), rhs_spill->offset);
    inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
    inst_list = emit_load_from_stack(inst_list, temp_reg, right_expr,
                                     expr_get_type_tag(right_expr),
                                     rhs_spill->offset, ctx);
    const char *target_name = select_register_name(
        target_reg, left_expr, expr_get_type_tag(left_expr));
    const char *temp_name = select_register_name(temp_reg, right_expr,
                                                 expr_get_type_tag(right_expr));
    inst_list =
        gencode_op(node->expr, target_name, target_reg, temp_name, temp_reg,
                   OPKIND_REGISTER, OPKIND_REGISTER, inst_list, ctx);
    free_reg(get_reg_stack(), temp_reg);
  }

  return inst_list;
}

ListNode_t *gencode_case3(expr_node_t *node, ListNode_t *inst_list,
                          CodeGenContext *ctx, Register_t *target_reg) {
#ifdef DEBUG_CODEGEN
  fprintf(stderr, "gencode_case3\n");
#endif
  assert(node != NULL);
  assert(node->expr != NULL);
  /* Note: inst_list can be NULL at the start of code generation */
  assert(ctx != NULL);
  assert(target_reg != NULL);

  Register_t *temp_reg;
  struct Expression *left_expr = node->left_expr->expr;
  struct Expression *right_expr = node->right_expr->expr;

  assert(left_expr != NULL);
  assert(right_expr != NULL);

  inst_list = gencode_expr_tree(node->left_expr, inst_list, ctx, target_reg);
  temp_reg = expr_tree_try_get_temp_reg(&inst_list, target_reg);

  if (temp_reg == NULL) {
    StackNode_t *spill_loc = add_l_t("spill");
    /* Use left operand's type for spilling, not the binary expr's result type
     */
    inst_list =
        emit_store_to_stack(inst_list, target_reg, left_expr,
                            expr_get_type_tag(left_expr), spill_loc->offset);

    inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, target_reg);

    char spill_mem[30];
    snprintf(spill_mem, 30, "-%d(%%rbp)", spill_loc->offset);
    const char *target_name = select_register_name(
        target_reg, right_expr, expr_get_type_tag(right_expr));
    inst_list = gencode_op(node->expr, target_name, target_reg, spill_mem, NULL,
                           OPKIND_REGISTER, OPKIND_MEMORY, inst_list, ctx);
  } else {
    StackNode_t *lhs_spill = add_l_t("case3_lhs");
    /* Use left operand's type for spilling, not the binary expr's result type
     */
    inst_list =
        emit_store_to_stack(inst_list, target_reg, left_expr,
                            expr_get_type_tag(left_expr), lhs_spill->offset);
    inst_list = gencode_expr_tree(node->right_expr, inst_list, ctx, temp_reg);
    inst_list = emit_load_from_stack(inst_list, target_reg, left_expr,
                                     expr_get_type_tag(left_expr),
                                     lhs_spill->offset, ctx);
    const char *target_name = select_register_name(
        target_reg, left_expr, expr_get_type_tag(left_expr));
    const char *temp_name = select_register_name(temp_reg, right_expr,
                                                 expr_get_type_tag(right_expr));
    inst_list =
        gencode_op(node->expr, target_name, target_reg, temp_name, temp_reg,
                   OPKIND_REGISTER, OPKIND_REGISTER, inst_list, ctx);
    free_reg(get_reg_stack(), temp_reg);
  }

  return inst_list;
}

/* Folds constant binary expressions to a literal when safe.
 * Leaves (EXPR_INUM, EXPR_BOOL, EXPR_CHAR_CODE) are already handled correctly
 * by the CASE 0 path and must NOT be routed through here — doing so strips
 * resolved_kgpc_type from the expression, which changes storage_tag and
 * desired_qword in gencode_case0, producing wrong instruction selection.
 * TODO: Still does not fold reals, strings, sets, or mixed-type expressions. */
static struct Expression *
expr_tree_simplify_to_literal(const struct Expression *expr) {
  if (expr == NULL)
    return NULL;

  switch (expr->type) {
  case EXPR_ADDOP: {
    if (optimize_flag() <= 0)
      return NULL;

    struct Expression *left_expr = expr->expr_data.addop_data.left_expr;
    struct Expression *right_expr = expr->expr_data.addop_data.right_term;
    if (left_expr == NULL || right_expr == NULL ||
        left_expr->type != EXPR_INUM || right_expr->type != EXPR_INUM) {
      return NULL;
    }

    long long lhs = left_expr->expr_data.i_num;
    long long rhs = right_expr->expr_data.i_num;
    long long folded = 0;

    if (expr->expr_data.addop_data.addop_type == PLUS) {
      if (__builtin_add_overflow(lhs, rhs, &folded))
        return NULL;
      return mk_inum(expr->line_num, folded);
    }
    if (expr->expr_data.addop_data.addop_type == MINUS) {
      if (__builtin_sub_overflow(lhs, rhs, &folded))
        return NULL;
      return mk_inum(expr->line_num, folded);
    }
    return NULL;
  }

  case EXPR_MULOP: {
    if (optimize_flag() <= 0)
      return NULL;

    struct Expression *left_expr = expr->expr_data.mulop_data.left_term;
    struct Expression *right_expr = expr->expr_data.mulop_data.right_factor;
    if (left_expr == NULL || right_expr == NULL ||
        left_expr->type != EXPR_INUM || right_expr->type != EXPR_INUM) {
      return NULL;
    }

    if (expr->expr_data.mulop_data.mulop_type == STAR) {
      long long folded = 0;
      if (__builtin_mul_overflow(left_expr->expr_data.i_num,
                                 right_expr->expr_data.i_num, &folded)) {
        return NULL;
      }
      return mk_inum(expr->line_num, folded);
    }
    return NULL;
  }

  case EXPR_RELOP: {
    if (optimize_flag() <= 0)
      return NULL;

    struct Expression *left_expr = expr->expr_data.relop_data.left;
    struct Expression *right_expr = expr->expr_data.relop_data.right;
    if (left_expr == NULL || right_expr == NULL ||
        left_expr->type != EXPR_INUM || right_expr->type != EXPR_INUM) {
      return NULL;
    }

    long long lhs = left_expr->expr_data.i_num;
    long long rhs = right_expr->expr_data.i_num;
    int is_true = 0;

    switch (expr->expr_data.relop_data.type) {
    case EQ:
      is_true = (lhs == rhs);
      break;
    case NE:
      is_true = (lhs != rhs);
      break;
    case LT:
      is_true = (lhs < rhs);
      break;
    case LE:
      is_true = (lhs <= rhs);
      break;
    case GT:
      is_true = (lhs > rhs);
      break;
    case GE:
      is_true = (lhs >= rhs);
      break;
    default:
      return NULL;
    }

    return mk_bool(expr->line_num, is_true);
  }

  default:
    return NULL;
  }
}
