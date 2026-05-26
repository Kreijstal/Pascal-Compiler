/*
    String-storage helpers for expression-tree lowering.
*/

#include <stddef.h>
#include <string.h>

#include "../codegen.h"
#include "../codegen_expression.h"
#include "../codegen_stmt_internal.h"
#include "expr_tree_internal.h"
#include "../../../identifier_utils.h"
#include "../../../Parser/ParseTree/KgpcType.h"
#include "../../../Parser/ParseTree/tree.h"
#include "../../../Parser/pascal_frontend.h"
#include "../../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../../Parser/SemanticCheck/SymTab/SymTab.h"

int codegen_array_access_targets_shortstring(const struct Expression *expr,
                                             CodeGenContext *ctx);

static int expr_is_shortstring_storage(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if (expr_get_type_tag(expr) == SHORTSTRING_TYPE)
    return 1;
  if (expr->resolved_kgpc_type != NULL) {
    if (kgpc_type_string_storage_kind(expr->resolved_kgpc_type) ==
        KGPC_STRING_STORAGE_SHORTSTRING)
      return 1;
  }
  if (expr->type == EXPR_POINTER_DEREF) {
    const struct Expression *pointer_expr =
        expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr != NULL && pointer_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_pointer(pointer_expr->resolved_kgpc_type)) {
      KgpcType *points_to = pointer_expr->resolved_kgpc_type->info.points_to;
      if (points_to != NULL) {
        if (kgpc_type_string_storage_kind(points_to) ==
            KGPC_STRING_STORAGE_SHORTSTRING)
          return 1;
      }
    }
  }
  return 0;
}

int expr_function_call_returns_ansistring(const struct Expression *expr,
                                          CodeGenContext *ctx) {
  if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
    return 0;

  KgpcType *call_type = expr->expr_data.function_call_data.call_kgpc_type;
  if (call_type == NULL && ctx != NULL)
    call_type = codegen_resolve_function_call_type(
        ctx, (struct Expression *)expr, NULL);
  if (call_type != NULL && call_type->kind == TYPE_KIND_PROCEDURE) {
    KgpcType *ret_type = kgpc_type_get_return_type(call_type);
    const char *ret_id = call_type->info.proc_info.return_type_id;
    if (kgpc_type_string_storage_kind(ret_type) ==
        KGPC_STRING_STORAGE_MANAGED_ANSI) {
      return 1;
    }
    if (ret_id != NULL && pascal_identifier_equals(ret_id, "AnsiString"))
      return 1;
  }

  return 0;
}

int expr_is_shortstring_storage_ctx(const struct Expression *expr,
                                    CodeGenContext *ctx) {
  if (expr_function_call_returns_ansistring(expr, ctx))
    return 0;

  if (expr_is_shortstring_storage(expr))
    return 1;

  if (expr != NULL && expr->type == EXPR_FUNCTION_CALL &&
      expr_has_type_tag(expr, STRING_TYPE) &&
      !pascal_frontend_default_shortstring()) {
    return 0;
  }

  if (ctx != NULL && codegen_expr_is_shortstring_value_ctx(expr, ctx))
    return 1;

  if (expr != NULL && expr->type == EXPR_ARRAY_ACCESS &&
      codegen_array_access_targets_shortstring(expr, ctx))
    return 1;
  if (expr != NULL && expr->type == EXPR_ARRAY_ACCESS) {
    long long char_len = 0;
    if (expr_get_char_array_length_expr(expr, ctx, &char_len) && char_len > 1 &&
        char_len <= 256)
      return 1;
  }

  if (expr != NULL && expr->type == EXPR_ARRAY_ACCESS && ctx != NULL) {
    struct Expression *base_expr = expr->expr_data.array_access_data.array_expr;
    KgpcType *base_type = NULL;
    if (base_expr != NULL)
      base_type = expr_get_kgpc_type(base_expr);
    if (base_type == NULL && base_expr != NULL &&
        base_expr->type == EXPR_VAR_ID && ctx->symtab != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, ctx->symtab, base_expr->expr_data.id) != 0 &&
          node != NULL) {
        base_type = node->type;
      }
    }
    if (base_type != NULL && kgpc_type_is_array(base_type)) {
      KgpcType *elem_type = kgpc_type_get_array_element_type(base_type);
      if (kgpc_type_string_storage_kind(elem_type) ==
          KGPC_STRING_STORAGE_SHORTSTRING)
        return 1;
    }
  }

  if (expr != NULL && expr->type == EXPR_VAR_ID && ctx != NULL &&
      ctx->symtab != NULL && expr->expr_data.id != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
        node != NULL && node->type != NULL) {
      if (kgpc_type_string_storage_kind(node->type) ==
          KGPC_STRING_STORAGE_SHORTSTRING)
        return 1;
    }
  }
  return 0;
}

int expr_is_char_array_expr(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if (expr->is_array_expr && expr->array_element_type == CHAR_TYPE)
    return 1;
  if (expr->resolved_kgpc_type != NULL &&
      expr->resolved_kgpc_type->kind == TYPE_KIND_ARRAY &&
      expr->resolved_kgpc_type->info.array_info.element_type != NULL &&
      expr->resolved_kgpc_type->info.array_info.element_type->kind ==
          TYPE_KIND_PRIMITIVE &&
      expr->resolved_kgpc_type->info.array_info.element_type->info
              .primitive_type_tag == CHAR_TYPE) {
    return 1;
  }
  if (expr->type == EXPR_POINTER_DEREF) {
    const struct Expression *pointer_expr =
        expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr != NULL && pointer_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_pointer(pointer_expr->resolved_kgpc_type)) {
      KgpcType *points_to = pointer_expr->resolved_kgpc_type->info.points_to;
      if (points_to != NULL && points_to->kind == TYPE_KIND_ARRAY &&
          points_to->info.array_info.element_type != NULL &&
          points_to->info.array_info.element_type->kind ==
              TYPE_KIND_PRIMITIVE &&
          points_to->info.array_info.element_type->info.primitive_type_tag ==
              CHAR_TYPE)
        return 1;
    }
  }
  return 0;
}

int expr_get_char_array_length_expr(const struct Expression *expr,
                                    CodeGenContext *ctx, long long *out_len) {
  if (out_len != NULL)
    *out_len = 0;
  if (expr == NULL)
    return 0;

  long long lower = 0;
  long long upper = -1;
  int found = 0;

  if (expr->is_array_expr && expr->array_element_type == CHAR_TYPE) {
    lower = expr_get_array_lower_bound(expr);
    upper = expr_get_array_upper_bound(expr);
    found = 1;
  } else if (expr->resolved_kgpc_type != NULL &&
             kgpc_type_is_array(expr->resolved_kgpc_type)) {
    KgpcType *elem = kgpc_type_get_array_element_type(expr->resolved_kgpc_type);
    if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE &&
        elem->info.primitive_type_tag == CHAR_TYPE) {
      lower = expr->resolved_kgpc_type->info.array_info.start_index;
      upper = expr->resolved_kgpc_type->info.array_info.end_index;
      found = 1;
    }
  } else if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
        node != NULL && node->type != NULL && kgpc_type_is_array(node->type)) {
      KgpcType *elem = kgpc_type_get_array_element_type(node->type);
      if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE &&
          elem->info.primitive_type_tag == CHAR_TYPE) {
        lower = node->type->info.array_info.start_index;
        upper = node->type->info.array_info.end_index;
        found = 1;
      }
    }
  }

  if (!found || upper < lower)
    return 0;
  if (out_len != NULL)
    *out_len = upper - lower + 1;
  return 1;
}
