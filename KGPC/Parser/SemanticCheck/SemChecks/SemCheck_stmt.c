/*
    Damon Gwinn
    Performs semantic checking on a given statement

    NOTE: Max scope level refers to the highest level scope we can reference a
   variable at
        - 0 is the current scope, 1 is the first above and so on
        - Functions can't have side effects, but they can contain procedures so
   this is a general way to define the maximum scope level
*/

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strncasecmp _strnicmp
#endif
#include "../../../common_utils.h"
#include "../../../unit_registry.h"
#include "../HashTable/HashTable.h"
#include "../NameMangling.h"
#include "../SemCheck.h"
#include "../SymTab/SymTab.h"
#include "SemCheck_expr.h"
#include "SemCheck_overload.h"
#include "SemCheck_sizeof.h"
#include "SemCheck_stmt.h"
#include "SemCheck_stmt_internal.h"
/* WithContextEntry is defined in SemCheck_Expr_Internal.h.  We can't include
 * that header here because of redefinition conflicts with helpers like
 * `semcheck_is_currency_kgpc_type`/`get_type_alias_from_node` that this file
 * also defines as static.  Instead we forward-declare the with stack here. */
struct WithContextEntry_fwd {
  struct Expression *context_expr;
  struct RecordType *record_type;
};
extern struct WithContextEntry_fwd *with_context_stack;

void semcheck_debug_expr_brief(const struct Expression *expr,
                               const char *label);
struct RecordType *get_record_type_from_node(HashNode_t *node);
int semcheck_try_indexed_property_assignment(SymTab_t *symtab,
                                             struct Statement *stmt,
                                             int max_scope_lev);
int semcheck_stmt_method_is_declared_constructor(SymTab_t *symtab,
                                                 struct RecordType *record_info,
                                                 const char *method_name);
#include "../../ParseTree/from_cparser.h"
#include "../../ParseTree/generic_types.h"
#include "../../ParseTree/tree.h"

struct RecordType *semcheck_lookup_record_type(SymTab_t *symtab,
                                               const char *type_id);

int semcheck_stmt_method_is_declared_constructor(SymTab_t *symtab,
                                                 struct RecordType *record_info,
                                                 const char *method_name) {
  if (symtab == NULL || record_info == NULL || method_name == NULL)
    return 0;

  for (struct RecordType *search = record_info; search != NULL;) {
    struct MethodTemplate *tmpl =
        from_cparser_get_method_template(search, method_name);
    if (tmpl != NULL)
      return tmpl->kind == METHOD_TEMPLATE_CONSTRUCTOR;

    if (search->parent_class_name == NULL)
      break;
    search = semcheck_lookup_record_type(symtab, search->parent_class_name);
  }

  return 0;
}
#include "../../List/List.h"
#include "../../ParseTree/ident_ref.h"
#include "../../ParseTree/tree_types.h"
#include "../../ParseTree/type_tags.h"

HashNode_t *semcheck_find_preferred_type_node(SymTab_t *symtab,
                                              const char *type_id);
int semcheck_param_types_compatible(Tree_t *param_decl, KgpcType *expected,
                                    KgpcType *actual, SymTab_t *symtab);

/* Forward declaration from SemCheck_Expr_Resolve.c */
const char *semcheck_type_tag_name(int type_tag);
HashNode_t *semcheck_find_type_node_in_owner_chain(SymTab_t *symtab,
                                                   const char *type_id,
                                                   const char *owner_full,
                                                   const char *owner_outer);
const char *semcheck_get_current_subprogram_owner_class_full(void);
const char *semcheck_get_current_subprogram_owner_class_outer(void);
int semcheck_typecheck_array_literal(struct Expression *expr, SymTab_t *symtab,
                                     int max_scope_lev, int expected_type,
                                     const char *expected_type_id,
                                     int line_num);
int set_type_from_hashtype(int *type, HashNode_t *hash_node);
int semcheck_convert_set_literal_to_array_literal(struct Expression *expr);
int semcheck_try_reinterpret_as_typecast(int *type_return, SymTab_t *symtab,
                                         struct Expression *expr,
                                         int max_scope_lev);
void semcheck_reset_function_call_cache(struct Expression *expr);
int semcheck_expr_is_char_like(struct Expression *expr);
int semcheck_class_type_ids_compatible(SymTab_t *symtab, const char *formal_id,
                                       const char *actual_id);

void semcheck_expr_set_resolved_type(struct Expression *expr, int type_tag);

static int semcheck_expr_best_line(const struct Expression *expr);
int semcheck_expr_best_context(const struct Expression *expr, int *out_line,
                               int *out_col, int *out_source_index);
static int semcheck_expr_list_best_context(ListNode_t *list, int *out_line,
                                           int *out_col, int *out_source_index);
static int semcheck_expr_is_real_family(const struct Expression *expr);
static int semcheck_expr_list_best_line(ListNode_t *list) {
  while (list != NULL) {
    struct Expression *item = (struct Expression *)list->cur;
    int line = semcheck_expr_best_line(item);
    if (line > 0)
      return line;
    list = list->next;
  }
  return 0;
}

static int semcheck_expr_is_plain_zero_index(const struct Expression *expr) {
  return expr != NULL && expr->type == EXPR_INUM && expr->expr_data.i_num == 0;
}

static int semcheck_type_is_promotable_plain_string(const KgpcType *type) {
  if (type == NULL || !kgpc_type_is_string((KgpcType *)type) ||
      kgpc_type_is_shortstring((KgpcType *)type))
    return 0;
  if (type->type_alias == NULL)
    return 1;
  return type->type_alias->alias_name != NULL &&
         strcasecmp(type->type_alias->alias_name, "string") == 0;
}

void semcheck_maybe_promote_index0_string_var_to_shortstring(
    SymTab_t *symtab, struct Statement *stmt) {
  if (symtab == NULL || stmt == NULL || stmt->type != STMT_VAR_ASSIGN)
    return;

  struct Expression *lhs = stmt->stmt_data.var_assign_data.var;
  if (lhs == NULL || lhs->type != EXPR_ARRAY_ACCESS)
    return;
  if (!semcheck_expr_is_plain_zero_index(
          lhs->expr_data.array_access_data.index_expr))
    return;

  struct Expression *base = lhs->expr_data.array_access_data.array_expr;
  if (base == NULL || base->type != EXPR_VAR_ID || base->expr_data.id == NULL)
    return;

  HashNode_t *var_node = NULL;
  if (FindSymbol(&var_node, symtab, base->expr_data.id) == 0 ||
      var_node == NULL)
    return;
  if (var_node->hash_type != HASHTYPE_VAR ||
      !semcheck_type_is_promotable_plain_string(var_node->type))
    return;

  KgpcType *short_type = create_primitive_type(SHORTSTRING_TYPE);
  if (short_type == NULL)
    return;
  destroy_kgpc_type(var_node->type);
  var_node->type = short_type;
}

static HashNode_t *
semcheck_find_zero_arg_method_node(SymTab_t *symtab,
                                   const struct RecordType *record,
                                   const char *method_name) {
  if (symtab == NULL || record == NULL || record->type_id == NULL ||
      method_name == NULL)
    return NULL;

  size_t len = strlen(record->type_id) + 2 + strlen(method_name) + 1;
  char *base_name = (char *)malloc(len);
  if (base_name == NULL)
    return NULL;

  snprintf(base_name, len, "%s__%s", record->type_id, method_name);
  ListNode_t *candidates = FindAllIdents(symtab, base_name);
  free(base_name);

  HashNode_t *match = NULL;
  for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next) {
    HashNode_t *cand = (HashNode_t *)cur->cur;
    Tree_t *first_param = NULL;
    const char *first_param_name = NULL;
    if (cand == NULL || cand->type == NULL)
      continue;
    if (cand->hash_type != HASHTYPE_FUNCTION &&
        cand->hash_type != HASHTYPE_PROCEDURE)
      continue;
    ListNode_t *params = kgpc_type_get_procedure_params(cand->type);
    if (params != NULL && params->cur != NULL)
      first_param = (Tree_t *)params->cur;
    if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
        first_param->tree_data.var_decl_data.ids != NULL)
      first_param_name =
          (const char *)first_param->tree_data.var_decl_data.ids->cur;
    else if (first_param != NULL && first_param->type == TREE_ARR_DECL &&
             first_param->tree_data.arr_decl_data.ids != NULL)
      first_param_name =
          (const char *)first_param->tree_data.arr_decl_data.ids->cur;

    if (!((ListLength(params) == 0) ||
          (ListLength(params) == 1 && first_param_name != NULL &&
           strcasecmp(first_param_name, "Self") == 0)))
      continue;
    match = cand;
    break;
  }

  DestroyList(candidates);
  return match;
}

static int semcheck_get_enumerator_current_type(SymTab_t *symtab,
                                                struct RecordType *enum_record,
                                                KgpcType **out_current_type) {
  if (out_current_type != NULL)
    *out_current_type = NULL;
  if (symtab == NULL || enum_record == NULL || out_current_type == NULL)
    return 0;

  HashNode_t *getcurrent =
      semcheck_find_zero_arg_method_node(symtab, enum_record, "GetCurrent");
  if (getcurrent != NULL && getcurrent->type != NULL) {
    KgpcType *ret = kgpc_type_get_return_type(getcurrent->type);
    if (ret != NULL) {
      kgpc_type_retain(ret);
      *out_current_type = ret;
      return 1;
    }
  }

  return 0;
}

int semcheck_collection_is_enumerator_class(SymTab_t *symtab,
                                            KgpcType *collection_kgpc_type,
                                            KgpcType **out_current_type) {
  if (out_current_type != NULL)
    *out_current_type = NULL;
  if (symtab == NULL || collection_kgpc_type == NULL)
    return 0;

  KgpcType *record_candidate = collection_kgpc_type;
  if (kgpc_type_is_pointer(collection_kgpc_type))
    record_candidate = collection_kgpc_type->info.points_to;
  if (record_candidate == NULL || !kgpc_type_is_record(record_candidate))
    return 0;

  struct RecordType *collection_record = kgpc_type_get_record(record_candidate);
  if (collection_record == NULL)
    return 0;

  HashNode_t *getenum = semcheck_find_zero_arg_method_node(
      symtab, collection_record, "GetEnumerator");
  if (getenum == NULL || getenum->type == NULL)
    return 0;

  KgpcType *enum_ret = kgpc_type_get_return_type(getenum->type);
  if (enum_ret == NULL)
    return 0;

  KgpcType *enum_candidate = enum_ret;
  if (kgpc_type_is_pointer(enum_ret))
    enum_candidate = enum_ret->info.points_to;
  if (enum_candidate == NULL || !kgpc_type_is_record(enum_candidate))
    return 0;

  struct RecordType *enum_record = kgpc_type_get_record(enum_candidate);
  if (enum_record == NULL)
    return 0;

  HashNode_t *movenext =
      semcheck_find_zero_arg_method_node(symtab, enum_record, "MoveNext");
  if (movenext == NULL || movenext->type == NULL)
    return 0;

  KgpcType *move_ret = kgpc_type_get_return_type(movenext->type);
  if (move_ret == NULL || !kgpc_type_is_boolean(move_ret))
    return 0;

  return semcheck_get_enumerator_current_type(symtab, enum_record,
                                              out_current_type);
}

static int semcheck_expr_best_line(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if (expr->line_num > 0)
    return expr->line_num;

  switch (expr->type) {
  case EXPR_RELOP: {
    int line = semcheck_expr_best_line(expr->expr_data.relop_data.left);
    if (line > 0)
      return line;
    return semcheck_expr_best_line(expr->expr_data.relop_data.right);
  }
  case EXPR_SIGN_TERM:
    return semcheck_expr_best_line(expr->expr_data.sign_term);
  case EXPR_ADDOP: {
    int line = semcheck_expr_best_line(expr->expr_data.addop_data.left_expr);
    if (line > 0)
      return line;
    return semcheck_expr_best_line(expr->expr_data.addop_data.right_term);
  }
  case EXPR_MULOP: {
    int line = semcheck_expr_best_line(expr->expr_data.mulop_data.left_term);
    if (line > 0)
      return line;
    return semcheck_expr_best_line(expr->expr_data.mulop_data.right_factor);
  }
  case EXPR_ARRAY_ACCESS: {
    int line =
        semcheck_expr_best_line(expr->expr_data.array_access_data.array_expr);
    if (line > 0)
      return line;
    line =
        semcheck_expr_best_line(expr->expr_data.array_access_data.index_expr);
    if (line > 0)
      return line;
    return semcheck_expr_list_best_line(
        expr->expr_data.array_access_data.extra_indices);
  }
  case EXPR_RECORD_ACCESS:
    return semcheck_expr_best_line(
        expr->expr_data.record_access_data.record_expr);
  case EXPR_FUNCTION_CALL: {
    int line = semcheck_expr_list_best_line(
        expr->expr_data.function_call_data.args_expr);
    if (line > 0)
      return line;
    return semcheck_expr_best_line(
        expr->expr_data.function_call_data.procedural_var_expr);
  }
  case EXPR_POINTER_DEREF:
    return semcheck_expr_best_line(
        expr->expr_data.pointer_deref_data.pointer_expr);
  case EXPR_ADDR:
    return semcheck_expr_best_line(expr->expr_data.addr_data.expr);
  case EXPR_TYPECAST:
    return semcheck_expr_best_line(expr->expr_data.typecast_data.expr);
  case EXPR_IS:
    return semcheck_expr_best_line(expr->expr_data.is_data.expr);
  case EXPR_AS:
    return semcheck_expr_best_line(expr->expr_data.as_data.expr);
  case EXPR_SET: {
    ListNode_t *elements = expr->expr_data.set_data.elements;
    while (elements != NULL) {
      struct SetElement *elem = (struct SetElement *)elements->cur;
      int line = semcheck_expr_best_line(elem ? elem->lower : NULL);
      if (line > 0)
        return line;
      line = semcheck_expr_best_line(elem ? elem->upper : NULL);
      if (line > 0)
        return line;
      elements = elements->next;
    }
    return 0;
  }
  case EXPR_ARRAY_LITERAL:
    return semcheck_expr_list_best_line(
        expr->expr_data.array_literal_data.elements);
  case EXPR_RECORD_CONSTRUCTOR: {
    ListNode_t *fields = expr->expr_data.record_constructor_data.fields;
    while (fields != NULL) {
      struct RecordConstructorField *field =
          (struct RecordConstructorField *)fields->cur;
      int line = semcheck_expr_best_line(field ? field->value : NULL);
      if (line > 0)
        return line;
      fields = fields->next;
    }
    return 0;
  }
  case EXPR_ANONYMOUS_FUNCTION:
  case EXPR_ANONYMOUS_PROCEDURE:
    return 0;
  default:
    break;
  }

  if (expr->field_width != NULL)
    return semcheck_expr_best_line(expr->field_width);
  if (expr->field_precision != NULL)
    return semcheck_expr_best_line(expr->field_precision);
  return 0;
}

static int semcheck_expr_list_best_context(ListNode_t *list, int *out_line,
                                           int *out_col,
                                           int *out_source_index) {
  while (list != NULL) {
    struct Expression *item = (struct Expression *)list->cur;
    if (semcheck_expr_best_context(item, out_line, out_col, out_source_index))
      return 1;
    list = list->next;
  }
  return 0;
}

int semcheck_expr_best_context(const struct Expression *expr, int *out_line,
                               int *out_col, int *out_source_index) {
  if (expr == NULL)
    return 0;

  if (expr->source_index >= 0 || expr->line_num > 0 || expr->col_num > 0) {
    if (out_line != NULL && expr->line_num > 0)
      *out_line = expr->line_num;
    if (out_col != NULL && expr->col_num > 0)
      *out_col = expr->col_num;
    if (out_source_index != NULL && expr->source_index >= 0)
      *out_source_index = expr->source_index;
    return 1;
  }

  switch (expr->type) {
  case EXPR_RELOP:
    if (semcheck_expr_best_context(expr->expr_data.relop_data.left, out_line,
                                   out_col, out_source_index))
      return 1;
    return semcheck_expr_best_context(expr->expr_data.relop_data.right,
                                      out_line, out_col, out_source_index);
  case EXPR_SIGN_TERM:
    return semcheck_expr_best_context(expr->expr_data.sign_term, out_line,
                                      out_col, out_source_index);
  case EXPR_ADDOP:
    if (semcheck_expr_best_context(expr->expr_data.addop_data.left_expr,
                                   out_line, out_col, out_source_index))
      return 1;
    return semcheck_expr_best_context(expr->expr_data.addop_data.right_term,
                                      out_line, out_col, out_source_index);
  case EXPR_MULOP:
    if (semcheck_expr_best_context(expr->expr_data.mulop_data.left_term,
                                   out_line, out_col, out_source_index))
      return 1;
    return semcheck_expr_best_context(expr->expr_data.mulop_data.right_factor,
                                      out_line, out_col, out_source_index);
  case EXPR_ARRAY_ACCESS:
    if (semcheck_expr_best_context(expr->expr_data.array_access_data.array_expr,
                                   out_line, out_col, out_source_index))
      return 1;
    if (semcheck_expr_best_context(expr->expr_data.array_access_data.index_expr,
                                   out_line, out_col, out_source_index))
      return 1;
    return semcheck_expr_list_best_context(
        expr->expr_data.array_access_data.extra_indices, out_line, out_col,
        out_source_index);
  case EXPR_RECORD_ACCESS:
    return semcheck_expr_best_context(
        expr->expr_data.record_access_data.record_expr, out_line, out_col,
        out_source_index);
  case EXPR_FUNCTION_CALL:
    if (semcheck_expr_list_best_context(
            expr->expr_data.function_call_data.args_expr, out_line, out_col,
            out_source_index))
      return 1;
    return semcheck_expr_best_context(
        expr->expr_data.function_call_data.procedural_var_expr, out_line,
        out_col, out_source_index);
  case EXPR_POINTER_DEREF:
    return semcheck_expr_best_context(
        expr->expr_data.pointer_deref_data.pointer_expr, out_line, out_col,
        out_source_index);
  case EXPR_ADDR:
    return semcheck_expr_best_context(expr->expr_data.addr_data.expr, out_line,
                                      out_col, out_source_index);
  case EXPR_TYPECAST:
    return semcheck_expr_best_context(expr->expr_data.typecast_data.expr,
                                      out_line, out_col, out_source_index);
  case EXPR_IS:
    return semcheck_expr_best_context(expr->expr_data.is_data.expr, out_line,
                                      out_col, out_source_index);
  case EXPR_AS:
    return semcheck_expr_best_context(expr->expr_data.as_data.expr, out_line,
                                      out_col, out_source_index);
  case EXPR_SET: {
    ListNode_t *elements = expr->expr_data.set_data.elements;
    while (elements != NULL) {
      struct SetElement *elem = (struct SetElement *)elements->cur;
      if (semcheck_expr_best_context(elem ? elem->lower : NULL, out_line,
                                     out_col, out_source_index))
        return 1;
      if (semcheck_expr_best_context(elem ? elem->upper : NULL, out_line,
                                     out_col, out_source_index))
        return 1;
      elements = elements->next;
    }
    return 0;
  }
  case EXPR_ARRAY_LITERAL:
    return semcheck_expr_list_best_context(
        expr->expr_data.array_literal_data.elements, out_line, out_col,
        out_source_index);
  case EXPR_RECORD_CONSTRUCTOR: {
    ListNode_t *fields = expr->expr_data.record_constructor_data.fields;
    while (fields != NULL) {
      struct RecordConstructorField *field =
          (struct RecordConstructorField *)fields->cur;
      if (semcheck_expr_best_context(field ? field->value : NULL, out_line,
                                     out_col, out_source_index))
        return 1;
      fields = fields->next;
    }
    return 0;
  }
  case EXPR_ANONYMOUS_FUNCTION:
  case EXPR_ANONYMOUS_PROCEDURE:
    return 0;
  default:
    break;
  }

  if (expr->field_width != NULL)
    return semcheck_expr_best_context(expr->field_width, out_line, out_col,
                                      out_source_index);
  if (expr->field_precision != NULL)
    return semcheck_expr_best_context(expr->field_precision, out_line, out_col,
                                      out_source_index);
  return 0;
}

int semcheck_stmt_expr_tag(int *type_return, SymTab_t *symtab,
                           struct Expression *expr, int max_scope_lev,
                           int mutating) {
  KgpcType *resolved = NULL;
  int result =
      semcheck_expr_main(symtab, expr, max_scope_lev, mutating, &resolved);
  if (type_return != NULL) {
    *type_return = semcheck_tag_from_kgpc(resolved);
  }
  return result;
}

struct RecordType *semcheck_stmt_get_record_type_from_node(HashNode_t *node) {
  if (node == NULL)
    return NULL;
  if (node->type != NULL && node->type->kind == TYPE_KIND_RECORD)
    return node->type->info.record_info;
  if (node->type != NULL && node->type->kind == TYPE_KIND_POINTER &&
      node->type->info.points_to != NULL &&
      kgpc_type_is_record(node->type->info.points_to))
    return node->type->info.points_to->info.record_info;
  return NULL;
}
#include "../../../identifier_utils.h"
#include "../../ParseTree/KgpcType.h"
#include "../../ParseTree/from_cparser.h"
#include "../../ParseTree/type_tags.h"

#include <math.h>

static int semcheck_expr_is_shortstring(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if (expr->resolved_kgpc_type != NULL) {
    struct TypeAlias *alias =
        kgpc_type_get_type_alias(expr->resolved_kgpc_type);
    if (alias != NULL) {
      if (alias->is_shortstring)
        return 1;
      if ((alias->alias_name != NULL &&
           pascal_identifier_equals(alias->alias_name, "ShortString")) ||
          (alias->target_type_id != NULL &&
           pascal_identifier_equals(alias->target_type_id, "ShortString"))) {
        return 1;
      }
    }
    if (kgpc_type_is_shortstring(expr->resolved_kgpc_type))
      return 1;
  }
  if (expr->is_array_expr && expr->array_element_type == CHAR_TYPE &&
      expr->array_lower_bound == 0 && expr->array_upper_bound >= 0) {
    return 1;
  }
  return 0;
}

const char *semcheck_record_type_id_from_kgpc(KgpcType *type) {
  if (type == NULL)
    return NULL;

  if (kgpc_type_is_record(type)) {
    struct RecordType *record = kgpc_type_get_record(type);
    if (record != NULL && record->type_id != NULL)
      return record->type_id;
  }

  struct TypeAlias *alias = kgpc_type_get_type_alias(type);
  if (alias != NULL) {
    if (alias->target_type_id != NULL)
      return alias->target_type_id;
    if (alias->alias_name != NULL)
      return alias->alias_name;
  }

  return NULL;
}

static const char *semcheck_record_type_id_from_expr(SymTab_t *symtab,
                                                     struct Expression *expr,
                                                     KgpcType *hint_type) {
  const char *type_id = semcheck_record_type_id_from_kgpc(hint_type);
  if (type_id != NULL || symtab == NULL || expr == NULL ||
      expr->type != EXPR_VAR_ID || expr->expr_data.id == NULL) {
    return type_id;
  }

  HashNode_t *node = NULL;
  if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL &&
      node->type != NULL) {
    return semcheck_record_type_id_from_kgpc(node->type);
  }

  return NULL;
}

int semcheck_type_is_recordish(KgpcType *type) {
  if (type == NULL)
    return 0;
  if (kgpc_type_is_record(type))
    return 1;
  return semcheck_tag_from_kgpc(type) == RECORD_TYPE;
}

static int semcheck_record_assign_operator_score(
    SymTab_t *symtab, HashNode_t *cand, KgpcType *target_type,
    KgpcType *source_type, int *score_out, KgpcType **return_type_out) {
  if (symtab == NULL || cand == NULL || cand->type == NULL ||
      target_type == NULL || source_type == NULL || score_out == NULL) {
    return 0;
  }
  if (!kgpc_type_is_procedure(cand->type))
    return 0;

  ListNode_t *params = kgpc_type_get_procedure_params(cand->type);
  if (params == NULL || params->cur == NULL || params->next != NULL)
    return 0;

  Tree_t *param_decl = (Tree_t *)params->cur;
  int param_owned = 0;
  KgpcType *param_type =
      resolve_type_from_vardecl(param_decl, symtab, &param_owned);
  if (param_type == NULL)
    return 0;

  int arg_rank = kgpc_type_conversion_rank(source_type, param_type);
  if (arg_rank < 0 && semcheck_param_types_compatible(param_decl, param_type,
                                                      source_type, symtab))
    arg_rank = 4;

  KgpcType *ret_type = kgpc_type_get_return_type(cand->type);
  if (ret_type != NULL && ret_type->kind == TYPE_KIND_PRIMITIVE &&
      ret_type->info.primitive_type_tag == VARIANT_TYPE &&
      !(target_type->kind == TYPE_KIND_PRIMITIVE &&
        target_type->info.primitive_type_tag == VARIANT_TYPE)) {
    if (param_owned && param_type != NULL)
      destroy_kgpc_type(param_type);
    return 0;
  }
  int ret_rank = (ret_type != NULL)
                     ? kgpc_type_conversion_rank(ret_type, target_type)
                     : -1;
  if (ret_rank < 0 && ret_type != NULL &&
      are_types_compatible_for_assignment(target_type, ret_type, symtab)) {
    ret_rank = 4;
  }

  if (param_owned && param_type != NULL)
    destroy_kgpc_type(param_type);

  if (arg_rank < 0 || ret_rank < 0 || ret_type == NULL)
    return 0;

  /* Composite key: argument match is more important than return type match.
   * kgpc_type_conversion_rank returns 0=exact, 1=promotion, 2+=conversion,
   * so lower values are better.  The weights (8, 2) ensure argument rank
   * always dominates return rank in the comparison (lower total = better). */
  *score_out = arg_rank * 8 + ret_rank * 2;
  if (return_type_out != NULL)
    *return_type_out = ret_type;
  return 1;
}

static void semcheck_record_assign_consider_candidate(
    SymTab_t *symtab, HashNode_t *cand, KgpcType *target_type,
    KgpcType *source_type, HashNode_t **best_node, KgpcType **best_return_type,
    int *best_score) {
  int cand_score = 0;
  KgpcType *cand_return = NULL;
  if (!semcheck_record_assign_operator_score(
          symtab, cand, target_type, source_type, &cand_score, &cand_return)) {
    return;
  }

  if (*best_node == NULL || cand_score < *best_score) {
    *best_node = cand;
    *best_return_type = cand_return;
    *best_score = cand_score;
  }
}

static void semcheck_record_assign_consider_id(SymTab_t *symtab, const char *id,
                                               KgpcType *target_type,
                                               KgpcType *source_type,
                                               HashNode_t **best_node,
                                               KgpcType **best_return_type,
                                               int *best_score) {
  if (symtab == NULL || id == NULL)
    return;
  ListNode_t *candidates = FindAllIdents(symtab, id);
  for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next) {
    semcheck_record_assign_consider_candidate(
        symtab, (HashNode_t *)cur->cur, target_type, source_type, best_node,
        best_return_type, best_score);
  }
  DestroyList(candidates);
}

static int semcheck_symbol_is_assign_operator(HashNode_t *cand) {
  if (cand == NULL)
    return 0;
  if (cand->is_operator && cand->id != NULL &&
      (pascal_identifier_equals(cand->id, ":=") ||
       pascal_identifier_equals(cand->id, "op_assign")))
    return 1;
  if (cand->method_name != NULL &&
      (pascal_identifier_equals(cand->method_name, "op_assign") ||
       pascal_identifier_equals(cand->method_name, ":=") ||
       pascal_identifier_equals(cand->method_name, "Implicit")))
    return 1;
  /* Standalone operators: id like "int64__op_assign_Tconstexprint" */
  if (cand->is_operator && cand->id != NULL &&
      pascal_strcasestr(cand->id, "__op_assign") != NULL)
    return 1;
  return 0;
}

static HashNode_t *semcheck_find_record_assign_operator_candidate(
    SymTab_t *symtab, const char *target_type_id, const char *source_type_id,
    KgpcType *target_type, KgpcType *source_type, KgpcType **return_type_out) {
  if (symtab == NULL || target_type == NULL || source_type == NULL) {
    return NULL;
  }

  HashNode_t *best_node = NULL;
  KgpcType *best_return_type = NULL;
  int best_score = INT_MAX;
  if (target_type_id != NULL) {
    char target_id[256];
    char target_op_id[256];
    snprintf(target_id, sizeof(target_id), "%s.:=", target_type_id);
    snprintf(target_op_id, sizeof(target_op_id), "%s__op_assign",
             target_type_id);
    semcheck_record_assign_consider_id(symtab, target_id, target_type,
                                       source_type, &best_node,
                                       &best_return_type, &best_score);
    semcheck_record_assign_consider_id(symtab, target_op_id, target_type,
                                       source_type, &best_node,
                                       &best_return_type, &best_score);
  }
  if (source_type_id != NULL) {
    char source_id[256];
    char source_op_id[256];
    snprintf(source_id, sizeof(source_id), "%s.:=", source_type_id);
    snprintf(source_op_id, sizeof(source_op_id), "%s__op_assign",
             source_type_id);
    semcheck_record_assign_consider_id(symtab, source_id, target_type,
                                       source_type, &best_node,
                                       &best_return_type, &best_score);
    semcheck_record_assign_consider_id(symtab, source_op_id, target_type,
                                       source_type, &best_node,
                                       &best_return_type, &best_score);
  }
  if (target_type_id != NULL && source_type_id != NULL) {
    char target_specific_id[320];
    char source_specific_id[320];
    snprintf(target_specific_id, sizeof(target_specific_id), "%s__op_assign_%s",
             target_type_id, source_type_id);
    snprintf(source_specific_id, sizeof(source_specific_id), "%s__op_assign_%s",
             source_type_id, target_type_id);
    semcheck_record_assign_consider_id(symtab, target_specific_id, target_type,
                                       source_type, &best_node,
                                       &best_return_type, &best_score);
    semcheck_record_assign_consider_id(symtab, source_specific_id, target_type,
                                       source_type, &best_node,
                                       &best_return_type, &best_score);
  }

  if (best_node != NULL) {
    if (return_type_out != NULL)
      *return_type_out = best_return_type;
    return best_node;
  }

  /* Class variables are references, not aggregate records.  Falling through to
   * every visible global op_assign can bind unrelated conversions such as
   * olevariant.op_assign(terror) for normal class-reference assignments like
   * `resultdef := pbestrealtype^` in FPC's ninl.pas.  Plain records still need
   * the global scan for FPC helper conversions. */
  if (semcheck_kgpc_type_is_class_reference(target_type) ||
      semcheck_kgpc_type_is_class_reference(source_type))
    return NULL;

  semcheck_record_assign_consider_id(symtab, ":=", target_type, source_type,
                                     &best_node, &best_return_type,
                                     &best_score);
  semcheck_record_assign_consider_id(symtab, "op_assign", target_type,
                                     source_type, &best_node, &best_return_type,
                                     &best_score);

  for (ScopeNode *scope = symtab->current_scope; scope != NULL;
       scope = scope->parent) {
    HashTable_t *table = scope->table;
    if (table != NULL) {
      for (int i = 0; i < TABLE_SIZE; ++i) {
        for (ListNode_t *cur = table->table[i]; cur != NULL; cur = cur->next) {
          HashNode_t *cand = (HashNode_t *)cur->cur;
          if (!semcheck_symbol_is_assign_operator(cand))
            continue;
          semcheck_record_assign_consider_candidate(
              symtab, cand, target_type, source_type, &best_node,
              &best_return_type, &best_score);
        }
      }
    }
  }

  /* Search unit tables — walk unit tables for unit-aware lookup. */
  {
    int caller_unit_index =
        (symtab->current_scope != NULL && symtab->current_scope->unit_index > 0)
            ? symtab->current_scope->unit_index
            : semcheck_get_current_unit_index();

    /* Caller's own unit table */
    if (caller_unit_index > 0 && caller_unit_index < SYMTAB_MAX_UNITS &&
        symtab->unit_scopes[caller_unit_index] != NULL) {
      HashTable_t *table = symtab->unit_scopes[caller_unit_index]->table;
      for (int i = 0; i < TABLE_SIZE; ++i) {
        for (ListNode_t *cur = table->table[i]; cur != NULL; cur = cur->next) {
          HashNode_t *cand = (HashNode_t *)cur->cur;
          if (!semcheck_symbol_is_assign_operator(cand))
            continue;
          semcheck_record_assign_consider_candidate(
              symtab, cand, target_type, source_type, &best_node,
              &best_return_type, &best_score);
        }
      }
    }

    /* Dependency unit tables */
    int num_units = unit_registry_count();
    for (int dep = 1; dep <= num_units; dep++) {
      if (dep == caller_unit_index)
        continue;
      if (!unit_registry_is_dep(caller_unit_index, dep))
        continue;
      if (dep >= SYMTAB_MAX_UNITS || symtab->unit_scopes[dep] == NULL)
        continue;
      HashTable_t *table = symtab->unit_scopes[dep]->table;
      for (int i = 0; i < TABLE_SIZE; ++i) {
        for (ListNode_t *cur = table->table[i]; cur != NULL; cur = cur->next) {
          HashNode_t *cand = (HashNode_t *)cur->cur;
          if (!semcheck_symbol_is_assign_operator(cand))
            continue;
          semcheck_record_assign_consider_candidate(
              symtab, cand, target_type, source_type, &best_node,
              &best_return_type, &best_score);
        }
      }
    }

    /* When current scope is program-level (unit_index == 0), search all unit
     * tables */
    if (caller_unit_index == 0) {
      for (int u = 1; u < SYMTAB_MAX_UNITS; u++) {
        if (symtab->unit_scopes[u] == NULL)
          continue;
        HashTable_t *table = symtab->unit_scopes[u]->table;
        for (int i = 0; i < TABLE_SIZE; ++i) {
          for (ListNode_t *cur = table->table[i]; cur != NULL;
               cur = cur->next) {
            HashNode_t *cand = (HashNode_t *)cur->cur;
            if (!semcheck_symbol_is_assign_operator(cand))
              continue;
            semcheck_record_assign_consider_candidate(
                symtab, cand, target_type, source_type, &best_node,
                &best_return_type, &best_score);
          }
        }
      }
    }
  }

  if (symtab->builtin_scope->table != NULL) {
    HashTable_t *table = symtab->builtin_scope->table;
    for (int i = 0; i < TABLE_SIZE; ++i) {
      for (ListNode_t *cur = table->table[i]; cur != NULL; cur = cur->next) {
        HashNode_t *cand = (HashNode_t *)cur->cur;
        if (!semcheck_symbol_is_assign_operator(cand))
          continue;
        semcheck_record_assign_consider_candidate(
            symtab, cand, target_type, source_type, &best_node,
            &best_return_type, &best_score);
      }
    }
  }

  if (best_node != NULL && return_type_out != NULL)
    *return_type_out = best_return_type;
  return best_node;
}

int semcheck_try_record_conversion_expression(SymTab_t *symtab,
                                              struct Expression **expr_slot,
                                              struct Expression *target_expr,
                                              KgpcType *target_type,
                                              KgpcType **source_type,
                                              int *source_owned) {
  if (symtab == NULL || expr_slot == NULL || *expr_slot == NULL ||
      target_type == NULL || source_type == NULL || *source_type == NULL) {
    return 0;
  }

  int target_is_pointer =
      kgpc_type_is_pointer(target_type) ||
      (target_type->kind == TYPE_KIND_PRIMITIVE &&
       target_type->info.primitive_type_tag == POINTER_TYPE);
  int target_is_record = semcheck_type_is_recordish(target_type);
  int source_is_record = semcheck_type_is_recordish(*source_type);
  /* Require at least one side to be a record/recordish type.  Pointer-to-
   * pointer assignments (e.g. `^taddnode := @check.right` where right is
   * `tnode`) must never enter the operator-overload search — pointee
   * incompatibility is a normal assignment error, not a cue to invent an
   * implicit conversion through unrelated operators such as
   * `olevariant.op_assign(terror)` (FPC compiler/nset.pas makeifblock). */
  if (!target_is_record && !source_is_record)
    return 0;
  (void)target_is_pointer;

  struct Expression *source_expr = *expr_slot;
  const char *source_type_id =
      semcheck_record_type_id_from_expr(symtab, source_expr, *source_type);
  const char *target_type_id =
      semcheck_record_type_id_from_expr(symtab, target_expr, target_type);
  if (source_type_id == NULL && target_type_id == NULL)
    return 0;

  HashNode_t *operator_node = NULL;
  KgpcType *return_type = NULL;
  operator_node = semcheck_find_record_assign_operator_candidate(
      symtab, target_type_id, source_type_id, target_type, *source_type,
      &return_type);
  if (operator_node == NULL || return_type == NULL) {
    return 0;
  }
  if (!are_types_compatible_for_assignment(target_type, return_type, symtab))
    return 0;

  const char *call_id =
      operator_node->mangled_id != NULL
          ? operator_node->mangled_id
          : (operator_node->id != NULL ? operator_node->id : "op_assign");
  struct Expression *call_expr =
      mk_functioncall(source_expr->line_num, strdup(call_id), NULL);
  call_expr->expr_data.function_call_data.is_operator_call = 1;
  call_expr->expr_data.function_call_data.args_expr =
      CreateListNode(source_expr, LIST_EXPR);
  if (operator_node->mangled_id != NULL)
    call_expr->expr_data.function_call_data.mangled_id =
        strdup(operator_node->mangled_id);
  else
    call_expr->expr_data.function_call_data.mangled_id = strdup(call_id);
  call_expr->expr_data.function_call_data.resolved_func = operator_node;
  call_expr->expr_data.function_call_data.call_hash_type = HASHTYPE_FUNCTION;
  call_expr->expr_data.function_call_data.call_kgpc_type = operator_node->type;
  kgpc_type_retain(operator_node->type);
  call_expr->expr_data.function_call_data.is_call_info_valid = 1;

  if (call_expr->resolved_kgpc_type != NULL)
    destroy_kgpc_type(call_expr->resolved_kgpc_type);
  call_expr->resolved_kgpc_type = return_type;
  kgpc_type_retain(return_type);

  *expr_slot = call_expr;
  if (source_owned != NULL && *source_owned && *source_type != NULL)
    destroy_kgpc_type(*source_type);
  *source_type = return_type;
  if (source_owned != NULL)
    *source_owned = 0;
  return 1;
}

int semcheck_try_record_assignment_operator(SymTab_t *symtab,
                                            struct Statement *stmt,
                                            KgpcType *lhs_type,
                                            KgpcType **rhs_type,
                                            int *rhs_owned) {
  if (symtab == NULL || stmt == NULL || lhs_type == NULL || rhs_type == NULL ||
      *rhs_type == NULL || stmt->type != STMT_VAR_ASSIGN)
    return 0;
  return semcheck_try_record_conversion_expression(
      symtab, &stmt->stmt_data.var_assign_data.expr,
      stmt->stmt_data.var_assign_data.var, lhs_type, rhs_type, rhs_owned);
}

static KgpcType *semcheck_param_effective_type(Tree_t *param_decl,
                                               KgpcType *expected) {
  if (param_decl == NULL || expected == NULL)
    return expected;

  if (param_decl->type == TREE_VAR_DECL &&
      param_decl->tree_data.var_decl_data.is_var_param &&
      expected->kind == TYPE_KIND_POINTER && expected->info.points_to != NULL) {
    /* var/out params may be modeled as pointers; compare against the pointee
     * type. */
    return expected->info.points_to;
  }

  return expected;
}

static int semcheck_type_is_typed_file(KgpcType *type, struct SymTab *symtab) {
  if (type == NULL || symtab == NULL)
    return 0;

  HashNode_t *typed_file_node = NULL;
  if (FindSymbol(&typed_file_node, symtab, "TypedFile") == 0 ||
      typed_file_node == NULL)
    return 0;
  if (typed_file_node->type == NULL)
    return 0;
  return typed_file_node->type == type;
}

int semcheck_param_types_compatible(Tree_t *param_decl, KgpcType *expected,
                                    KgpcType *actual, SymTab_t *symtab) {
  if (expected == NULL || actual == NULL)
    return 0;

  KgpcType *effective = semcheck_param_effective_type(param_decl, expected);

  if (param_decl != NULL && param_decl->type == TREE_VAR_DECL &&
      param_decl->tree_data.var_decl_data.is_var_param && effective != NULL &&
      effective->kind == TYPE_KIND_PRIMITIVE &&
      actual->kind == TYPE_KIND_PRIMITIVE) {
    int expected_tag = effective->info.primitive_type_tag;
    int actual_tag = actual->info.primitive_type_tag;
    if (is_integer_type(expected_tag) && is_integer_type(actual_tag) &&
        expected_tag != actual_tag) {
      return 0;
    }
  }

  if (param_decl != NULL && param_decl->type == TREE_VAR_DECL &&
      actual->kind == TYPE_KIND_PRIMITIVE &&
      actual->info.primitive_type_tag == FILE_TYPE) {
    const char *type_id = param_decl->tree_data.var_decl_data.type_id;
    int actual_is_typed = semcheck_type_is_typed_file(actual, symtab);
    if (type_id != NULL) {
      if (pascal_identifier_equals(type_id, "TypedFile") && !actual_is_typed)
        return 0;
      if (pascal_identifier_equals(type_id, "File") && actual_is_typed)
        return 0;
    }
  }

  int compatible =
      are_types_compatible_for_assignment(effective, actual, symtab);
  if (compatible)
    return 1;

  if (param_decl != NULL && param_decl->type == TREE_VAR_DECL &&
      !param_decl->tree_data.var_decl_data.is_var_param &&
      actual->kind == TYPE_KIND_PRIMITIVE) {
    int expected_tag = semcheck_tag_from_kgpc(effective);
    int actual_tag = actual->info.primitive_type_tag;
    if ((expected_tag == LONGINT_TYPE && actual_tag == INT_TYPE) ||
        (expected_tag == INT64_TYPE &&
         (actual_tag == LONGINT_TYPE || actual_tag == INT_TYPE))) {
      return 1;
    }
  }

  if (param_decl != NULL && param_decl->type == TREE_VAR_DECL) {
    const char *type_id = param_decl->tree_data.var_decl_data.type_id;
    if (type_id != NULL && actual->kind == TYPE_KIND_PRIMITIVE) {
      int actual_tag = actual->info.primitive_type_tag;
      if (actual_tag == FILE_TYPE && pascal_identifier_equals(type_id, "File"))
        return 1;
      if (actual_tag == TEXT_TYPE && pascal_identifier_equals(type_id, "Text"))
        return 1;
    }
  }

  return 0;
}

/* Helper to check if a parameter has a default value */
int param_has_default_value(Tree_t *decl) {
  if (decl == NULL)
    return 0;

  if (decl->type == TREE_VAR_DECL) {
    /* Default value is stored in the initializer field */
    if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
      fprintf(
          stderr,
          "[SemCheck] param_has_default_value: TREE_VAR_DECL, initializer=%p\n",
          (void *)decl->tree_data.var_decl_data.initializer);
    }
    return decl->tree_data.var_decl_data.initializer != NULL;
  } else if (decl->type == TREE_ARR_DECL) {
    if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
      fprintf(
          stderr,
          "[SemCheck] param_has_default_value: TREE_ARR_DECL, initializer=%p\n",
          (void *)decl->tree_data.arr_decl_data.initializer);
    }
    return decl->tree_data.arr_decl_data.initializer != NULL;
  }

  return 0;
}

/* Helper to get the default value expression from a parameter */
static struct Expression *get_param_default_value(Tree_t *decl) {
  if (decl == NULL)
    return NULL;

  struct Statement *init = NULL;

  if (decl->type == TREE_VAR_DECL) {
    init = decl->tree_data.var_decl_data.initializer;
  } else if (decl->type == TREE_ARR_DECL) {
    init = decl->tree_data.arr_decl_data.initializer;
  }

  /* The default value is stored as a STMT_VAR_ASSIGN with NULL var, containing
   * the expression */
  if (init != NULL && init->type == STMT_VAR_ASSIGN)
    return init->stmt_data.var_assign_data.expr;

  return NULL;
}

int append_default_args(ListNode_t **args_head, ListNode_t *formal_params,
                        int line_num) {
  if (args_head == NULL)
    return 0;

  ListNode_t *formal = formal_params;
  ListNode_t *actual = *args_head;
  ListNode_t *tail = *args_head;

  while (tail != NULL && tail->next != NULL)
    tail = tail->next;

  while (formal != NULL && actual != NULL) {
    formal = formal->next;
    actual = actual->next;
  }

  while (formal != NULL) {
    Tree_t *param_decl = (Tree_t *)formal->cur;
    if (!param_has_default_value(param_decl))
      break;

    struct Expression *default_expr = get_param_default_value(param_decl);
    if (default_expr == NULL) {
      semcheck_error_with_context(
          "Error on line %d, missing default value expression.\n", line_num);
      return 1;
    }

    struct Expression *default_clone = clone_expression(default_expr);
    if (default_clone == NULL) {
      semcheck_error_with_context(
          "Error on line %d, failed to clone default argument expression.\n",
          line_num);
      return 1;
    }

    ListNode_t *node = CreateListNode(default_clone, LIST_EXPR);
    if (node == NULL) {
      destroy_expr(default_clone);
      semcheck_error_with_context(
          "Error on line %d, failed to allocate default argument node.\n",
          line_num);
      return 1;
    }

    if (*args_head == NULL) {
      *args_head = node;
      tail = node;
    } else {
      tail->next = node;
      tail = node;
    }

    formal = formal->next;
  }

  return 0;
}

/* Helper to get the default value expression from a parameter */
struct Expression *get_param_default_value_stmt(Tree_t *decl) {
  if (decl == NULL)
    return NULL;

  struct Statement *init = NULL;

  if (decl->type == TREE_VAR_DECL) {
    init = decl->tree_data.var_decl_data.initializer;
  } else if (decl->type == TREE_ARR_DECL) {
    init = decl->tree_data.arr_decl_data.initializer;
  }

  /* The default value is stored as a STMT_VAR_ASSIGN with NULL var, containing
   * the expression */
  if (init != NULL && init->type == STMT_VAR_ASSIGN) {
    return init->stmt_data.var_assign_data.expr;
  }

  return NULL;
}

/* Copy a default value expression for use as an argument */
struct Expression *copy_default_expr(struct Expression *src) {
  if (src == NULL)
    return NULL;

  struct Expression *copy = NULL;

  switch (src->type) {
  case EXPR_INUM:
    copy = mk_inum(src->line_num, src->expr_data.i_num);
    break;
  case EXPR_RNUM:
    copy = mk_rnum(src->line_num, src->expr_data.r_num);
    break;
  case EXPR_STRING:
    if (src->expr_data.string != NULL)
      copy = mk_string(src->line_num, strdup(src->expr_data.string));
    break;
  case EXPR_BOOL:
    copy = mk_bool(src->line_num, src->expr_data.bool_value);
    break;
  case EXPR_CHAR_CODE:
    copy = mk_charcode(src->line_num, src->expr_data.char_code);
    break;
  case EXPR_NIL:
    copy = (struct Expression *)malloc(sizeof(struct Expression));
    if (copy != NULL) {
      memset(copy, 0, sizeof(struct Expression));
      copy->type = EXPR_NIL;
      copy->line_num = src->line_num;
    }
    break;
  case EXPR_VAR_ID:
    /* Handle constant references like CPUEndian in default parameters */
    if (src->expr_data.id != NULL) {
      copy = (struct Expression *)malloc(sizeof(struct Expression));
      if (copy != NULL) {
        memset(copy, 0, sizeof(struct Expression));
        copy->type = EXPR_VAR_ID;
        copy->line_num = src->line_num;
        copy->expr_data.id = strdup(src->expr_data.id);
        if (copy->expr_data.id == NULL) {
          free(copy);
          copy = NULL;
        }
      }
    }
    break;
  case EXPR_RECORD_ACCESS:
    copy = clone_expression(src);
    break;
  case EXPR_FUNCTION_CALL:
    copy = clone_expression(src);
    break;
  case EXPR_SET:
    /* Support defaults like [] used by sysutils DateTimeToString options. */
    if (src->expr_data.set_data.elements == NULL) {
      copy = mk_set(src->line_num, src->expr_data.set_data.bitmask, NULL,
                    src->expr_data.set_data.is_constant);
    }
    break;
  default:
    /* For complex expressions, we can't easily copy them.
     * Return NULL and let the caller handle the error. */
    if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
      fprintf(stderr,
              "[SemCheck] copy_default_expr: unsupported expr type %d\n",
              src->type);
    }
    break;
  }

  return copy;
}

int semcheck_loop_depth = 0;
/* Debug helpers used for corruption watchdog logging. */
struct Statement *g_debug_watch_stmt = NULL;
struct Expression *g_debug_watch_to_expr = NULL;

/* Resolve the RecordType for a TFPGList specialization from a LHS expression.
 * This bypasses incomplete kgpc_type inference and looks directly at the symbol
 * table entry for the variable or type identifier. */
static int is_tfpglist_type_id(const char *type_id) {
  return (type_id != NULL &&
          strncasecmp(type_id, "TFPGList$", strlen("TFPGList$")) == 0);
}

/* Resolve the RecordType for a TFPGList specialization from a LHS expression.
 * This bypasses incomplete kgpc_type inference and looks directly at the symbol
 * table entry for the variable or type identifier. */
static struct RecordType *
resolve_tfpglist_record_from_lhs(SymTab_t *symtab, struct Expression *lhs) {
  if (symtab == NULL || lhs == NULL)
    return NULL;

  if (lhs->type != EXPR_VAR_ID || lhs->expr_data.id == NULL)
    return NULL;

  HashNode_t *node = NULL;
  if (FindSymbol(&node, symtab, lhs->expr_data.id) == 0 || node == NULL)
    return NULL;

  struct RecordType *record = hashnode_get_record_type(node);
  if (record == NULL || record->type_id == NULL)
    return NULL;

  if (!is_tfpglist_type_id(record->type_id))
    return NULL;

  return record;
}

static inline struct TypeAlias *get_type_alias_from_node(HashNode_t *node) {
  return hashnode_get_type_alias(node);
}

KgpcType *resolve_param_type_with_owner(Tree_t *param_decl, SymTab_t *symtab,
                                        const char *owner_full,
                                        const char *owner_outer,
                                        int *param_type_owned) {
  KgpcType *param_type =
      resolve_type_from_vardecl(param_decl, symtab, param_type_owned);
  if (param_type != NULL || param_decl == NULL || symtab == NULL)
    return param_type;

  const char *type_id = NULL;
  if (param_decl->type == TREE_VAR_DECL)
    type_id = param_decl->tree_data.var_decl_data.type_id;
  else if (param_decl->type == TREE_ARR_DECL)
    type_id = param_decl->tree_data.arr_decl_data.type_id;

  if (type_id == NULL)
    return NULL;

  const char *resolved_owner_full = owner_full;
  const char *resolved_owner_outer = owner_outer;
  if (resolved_owner_full == NULL && resolved_owner_outer == NULL) {
    resolved_owner_full = semcheck_get_current_subprogram_owner_class_full();
    resolved_owner_outer = semcheck_get_current_subprogram_owner_class_outer();
    if (resolved_owner_full == NULL)
      resolved_owner_full = semcheck_get_current_method_owner();
  }

  HashNode_t *type_node = semcheck_find_type_node_in_owner_chain(
      symtab, type_id, resolved_owner_full, resolved_owner_outer);
  if (type_node == NULL)
    return NULL;

  if (type_node->type != NULL) {
    kgpc_type_retain(type_node->type);
    if (param_type_owned != NULL)
      *param_type_owned = 1;
    return type_node->type;
  }

  struct TypeAlias *alias = get_type_alias_from_node(type_node);
  if (alias != NULL) {
    KgpcType *alias_type = create_kgpc_type_from_type_alias(alias, symtab, 0);
    if (alias_type != NULL) {
      if (alias->kgpc_type == alias_type)
        kgpc_type_retain(alias_type);
      if (param_type_owned != NULL)
        *param_type_owned = 1;
      return alias_type;
    }
  }

  return NULL;
}

static HashNode_t *lookup_hashnode(SymTab_t *symtab, const char *id) {
  if (symtab == NULL || id == NULL)
    return NULL;
  HashNode_t *node = NULL;
  if (FindSymbol(&node, symtab, id) != 0 && node != NULL)
    return node;
  return NULL;
}

int resolve_record_field(SymTab_t *symtab, struct RecordType *record,
                         const char *field_name, struct RecordField **out_field,
                         long long *offset_out, int line_num, int silent);
int resolve_param_type(Tree_t *decl, SymTab_t *symtab);

static const char *
resolve_tfpglist_specialized_id_from_typename(SymTab_t *symtab,
                                              const char *type_name) {
  HashNode_t *type_node = lookup_hashnode(symtab, type_name);
  if (type_node == NULL)
    return NULL;

  struct RecordType *record = hashnode_get_record_type(type_node);
  if (record != NULL && is_tfpglist_type_id(record->type_id))
    return record->type_id;

  struct TypeAlias *alias = get_type_alias_from_node(type_node);
  if (alias != NULL && alias->target_type_id != NULL &&
      is_tfpglist_type_id(alias->target_type_id))
    return alias->target_type_id;

  return NULL;
}

static const char *
resolve_tfpglist_specialized_id_from_expr(SymTab_t *symtab,
                                          struct Expression *expr) {
  if (expr == NULL)
    return NULL;
  if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
    return resolve_tfpglist_specialized_id_from_typename(symtab,
                                                         expr->expr_data.id);
  return NULL;
}

static struct Expression *make_tfpglist_ctor_expr(struct RecordType *record,
                                                  int line_num) {
  if (record == NULL || record->type_id == NULL)
    return NULL;

  const char *type_id = record->type_id;
  const char *prefix = "__tfpg_ctor$";
  size_t len = strlen(prefix) + strlen(type_id) + 1;
  char *ctor_name = (char *)malloc(len);
  if (ctor_name == NULL)
    return NULL;
  strcpy(ctor_name, prefix);
  strcat(ctor_name, type_id);

  struct Expression *call = mk_functioncall(line_num, ctor_name, NULL);
  if (call == NULL)
    return NULL;

  /* Set the mangled_id to the actual Create constructor method name */
  /* Format: ClassName__Create_u */
  size_t mangled_len = strlen(type_id) + strlen("__Create_u") + 1;
  char *mangled_name = (char *)malloc(mangled_len);
  if (mangled_name != NULL) {
    strcpy(mangled_name, type_id);
    strcat(mangled_name, "__Create_u");
    call->expr_data.function_call_data.mangled_id = mangled_name;

    if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL) {
      fprintf(stderr, "[KGPC] TFPG ctor: set mangled_id to %s\n", mangled_name);
    }
  }

  if (call->resolved_kgpc_type != NULL) {
    destroy_kgpc_type(call->resolved_kgpc_type);
    call->resolved_kgpc_type = NULL;
  }
  call->resolved_kgpc_type = create_record_type(record);
  return call;
}

int rewrite_tfpglist_constructor_if_needed(SymTab_t *symtab, int max_scope_lev,
                                           struct Expression *lhs,
                                           struct Expression **rhs_ptr) {
  if (symtab == NULL || lhs == NULL || rhs_ptr == NULL || *rhs_ptr == NULL)
    return 0;
  const char *debug_env = kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES");

  struct RecordType *lhs_record = resolve_tfpglist_record_from_lhs(symtab, lhs);
  if (lhs_record == NULL || lhs_record->type_id == NULL) {
    if (debug_env) {
      fprintf(stderr,
              "[KGPC] TFPG ctor: lhs %s is not TFPGList specialization\n",
              (lhs->expr_data.id != NULL) ? lhs->expr_data.id : "<expr>");
    }
    return 0;
  }

  const char *expected_specialized_id = lhs_record->type_id;
  struct Expression *rhs = *rhs_ptr;

  int matches_pattern = 0;
  if (rhs->type == EXPR_RECORD_ACCESS &&
      rhs->expr_data.record_access_data.field_id != NULL &&
      strcasecmp(rhs->expr_data.record_access_data.field_id, "Create") == 0) {
    const char *candidate = resolve_tfpglist_specialized_id_from_expr(
        symtab, rhs->expr_data.record_access_data.record_expr);
    if (candidate != NULL &&
        strcasecmp(candidate, expected_specialized_id) == 0)
      matches_pattern = 1;
  } else if (rhs->type == EXPR_FUNCTION_CALL &&
             rhs->expr_data.function_call_data.id != NULL) {
    const char *candidate = resolve_tfpglist_specialized_id_from_typename(
        symtab, rhs->expr_data.function_call_data.id);
    if (candidate != NULL &&
        strcasecmp(candidate, expected_specialized_id) == 0) {
      /* Legacy lowering produced a dummy argument referencing the type */
      ListNode_t *args = rhs->expr_data.function_call_data.args_expr;
      if (args == NULL)
        matches_pattern = 1;
      else if (args->next == NULL) {
        struct Expression *arg_expr = (struct Expression *)args->cur;
        if (arg_expr != NULL && arg_expr->type == EXPR_VAR_ID &&
            arg_expr->expr_data.id != NULL &&
            pascal_identifier_equals(arg_expr->expr_data.id,
                                     rhs->expr_data.function_call_data.id))
          matches_pattern = 1;
      }
    }
  }

  if (!matches_pattern) {
    if (debug_env)
      fprintf(stderr,
              "[KGPC] TFPG ctor: rhs did not match constructor pattern\n");
    return 0;
  }

  struct Expression *ctor_expr =
      make_tfpglist_ctor_expr(lhs_record, rhs->line_num);
  if (ctor_expr == NULL) {
    if (debug_env)
      fprintf(stderr, "[KGPC] TFPG ctor: failed to build ctor expression\n");
    return 0;
  }

  if (debug_env)
    fprintf(stderr, "[KGPC] TFPG ctor: rewriting ctor for %s\n",
            expected_specialized_id);

  destroy_expr(rhs);
  *rhs_ptr = ctor_expr;
  return 1;
}
void semcheck_stmt_set_call_kgpc_type(struct Statement *stmt, KgpcType *type,
                                      int owns_existing) {
  if (stmt == NULL || stmt->type != STMT_PROCEDURE_CALL)
    return;

  if (stmt->stmt_data.procedure_call_data.call_kgpc_type != NULL &&
      owns_existing) {
    destroy_kgpc_type(stmt->stmt_data.procedure_call_data.call_kgpc_type);
  }
  stmt->stmt_data.procedure_call_data.call_kgpc_type = NULL;

  if (type != NULL) {
    kgpc_type_retain(type);
    stmt->stmt_data.procedure_call_data.call_kgpc_type = type;
  }
}

void semcheck_stmt_set_call_owner_info(struct Statement *stmt,
                                       const char *owner_class,
                                       const char *method_name) {
  if (stmt == NULL || stmt->type != STMT_PROCEDURE_CALL)
    return;

  if (owner_class != NULL &&
      stmt->stmt_data.procedure_call_data.cached_owner_class != NULL) {
    free(stmt->stmt_data.procedure_call_data.cached_owner_class);
    stmt->stmt_data.procedure_call_data.cached_owner_class = NULL;
  }
  if (method_name != NULL &&
      stmt->stmt_data.procedure_call_data.cached_method_name != NULL) {
    free(stmt->stmt_data.procedure_call_data.cached_method_name);
    stmt->stmt_data.procedure_call_data.cached_method_name = NULL;
  }

  if (owner_class != NULL)
    stmt->stmt_data.procedure_call_data.cached_owner_class =
        strdup(owner_class);
  if (method_name != NULL)
    stmt->stmt_data.procedure_call_data.cached_method_name =
        strdup(method_name);
}

static int semcheck_stmt_proc_type_param_count(KgpcType *type) {
  if (type == NULL || type->kind != TYPE_KIND_PROCEDURE)
    return -1;

  ListNode_t *params = type->info.proc_info.params;
  int count = ListLength(params);
  if (count <= 0)
    return count;

  Tree_t *first_param = (Tree_t *)params->cur;
  if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
      first_param->tree_data.var_decl_data.ids != NULL) {
    const char *first_name =
        (const char *)first_param->tree_data.var_decl_data.ids->cur;
    if (first_name != NULL && pascal_identifier_equals(first_name, "Self"))
      count--;
  }

  return count;
}

static struct MethodInfo *
semcheck_stmt_find_receiver_vmt_method(struct RecordType *receiver_record,
                                       const char *method_name,
                                       KgpcType *call_type) {
  if (receiver_record == NULL || receiver_record->methods == NULL ||
      method_name == NULL)
    return NULL;

  int wanted_param_count = semcheck_stmt_proc_type_param_count(call_type);
  struct MethodInfo *single_name_match = NULL;
  int name_match_count = 0;

  for (ListNode_t *node = receiver_record->methods; node != NULL;
       node = node->next) {
    struct MethodInfo *method = (struct MethodInfo *)node->cur;
    if (method == NULL || method->name == NULL ||
        !(method->is_virtual || method->is_override) ||
        !pascal_identifier_equals(method->name, method_name))
      continue;

    single_name_match = method;
    name_match_count++;
    if (wanted_param_count >= 0) {
      if (method->param_count < 0)
        return NULL;
      if (method->param_count == wanted_param_count)
        return method;
    }
  }

  if (wanted_param_count < 0 && name_match_count == 1)
    return single_name_match;
  return NULL;
}

void semcheck_stmt_set_receiver_virtual_dispatch(
    struct Statement *stmt, struct RecordType *receiver_record,
    const char *method_name, KgpcType *call_type) {
  if (stmt == NULL || stmt->type != STMT_PROCEDURE_CALL ||
      receiver_record == NULL || receiver_record->type_id == NULL ||
      method_name == NULL)
    return;

  struct MethodInfo *method = semcheck_stmt_find_receiver_vmt_method(
      receiver_record, method_name, call_type);
  if (method == NULL)
    return;

  stmt->stmt_data.procedure_call_data.is_virtual_call = 1;
  stmt->stmt_data.procedure_call_data.vmt_index = method->vmt_index;

  free(stmt->stmt_data.procedure_call_data.self_class_name);
  stmt->stmt_data.procedure_call_data.self_class_name =
      strdup(receiver_record->type_id);

  if (stmt->stmt_data.procedure_call_data.cached_method_name != NULL)
    free(stmt->stmt_data.procedure_call_data.cached_method_name);
  stmt->stmt_data.procedure_call_data.cached_method_name = strdup(method_name);
}

/* Helper to check if a TypeAlias represents WideChar/UnicodeChar.
 * WideChar = Word (integer type), so we check alias_name, not CHAR_TYPE. */
static int semcheck_alias_is_widechar(struct TypeAlias *alias) {
  if (alias == NULL)
    return 0;
  /* Check alias_name - this is the declared type name (e.g., "WideChar") */
  if (alias->alias_name != NULL &&
      (pascal_identifier_equals(alias->alias_name, "WideChar") ||
       pascal_identifier_equals(alias->alias_name, "UnicodeChar")))
    return 1;
  return 0;
}

/* Check if an expression's type is WideChar (or aliased to WideChar).
 * WideChar = Word (integer type), so we check alias_name, not CHAR_TYPE. */
int semcheck_expr_is_widechar(SymTab_t *symtab, struct Expression *expr) {
  if (expr == NULL)
    return 0;

  /* Check resolved_kgpc_type */
  if (expr->resolved_kgpc_type != NULL) {
    KgpcType *ktype = expr->resolved_kgpc_type;
    struct TypeAlias *alias = ktype->type_alias;
    if (semcheck_alias_is_widechar(alias))
      return 1;
  }

  /* For EXPR_VAR_ID, look up the variable's type */
  if (expr->type == EXPR_VAR_ID && symtab != NULL &&
      expr->expr_data.id != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL) {
      if (node->type != NULL) {
        KgpcType *ntype = node->type;
        /* Check alias_name in KgpcType's type_alias */
        if (semcheck_alias_is_widechar(ntype->type_alias))
          return 1;
      }

      /* Check TypeAlias from node directly (secondary path) */
      struct TypeAlias *alias = get_type_alias_from_node(node);
      if (semcheck_alias_is_widechar(alias))
        return 1;
    }
  }

  return 0;
}

int semcheck_type_is_char_like(KgpcType *type) {
  if (type == NULL)
    return 0;
  if (kgpc_type_is_char(type))
    return 1;
  if (type->type_alias != NULL && type->type_alias->is_char_alias)
    return 1;
  return 0;
}

int semcheck_force_char_case_builtin_in_assignment(struct Expression *expr) {
  if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
    return 0;

  const char *id = expr->expr_data.function_call_data.id;
  if (id == NULL)
    return 0;

  const char *mangled = NULL;
  if (pascal_identifier_equals(id, "UpCase") ||
      pascal_identifier_equals(id, "UpperCase"))
    mangled = "kgpc_upcase_char";
  else if (pascal_identifier_equals(id, "LowerCase"))
    mangled = "kgpc_lowercase_char";
  else
    return 0;

  ListNode_t *args = expr->expr_data.function_call_data.args_expr;
  if (args == NULL || args->next != NULL)
    return 0;

  struct Expression *arg_expr = (struct Expression *)args->cur;
  if (!semcheck_expr_is_char_like(arg_expr))
    return 0;

  if (expr->expr_data.function_call_data.mangled_id != NULL) {
    free(expr->expr_data.function_call_data.mangled_id);
    expr->expr_data.function_call_data.mangled_id = NULL;
  }
  free(expr->expr_data.function_call_data.id);
  expr->expr_data.function_call_data.id = strdup(mangled);
  expr->expr_data.function_call_data.mangled_id = strdup(mangled);
  semcheck_reset_function_call_cache(expr);
  expr->expr_data.function_call_data.is_call_info_valid = 1;
  if (expr->resolved_kgpc_type != NULL) {
    destroy_kgpc_type(expr->resolved_kgpc_type);
    expr->resolved_kgpc_type = NULL;
  }
  expr->resolved_kgpc_type = create_primitive_type(CHAR_TYPE);
  semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
  return 1;
}

/* Check if expression is an integer constant representable as single-byte Char
 * (0..255). */
int semcheck_expr_is_char_ordinal_const(SymTab_t *symtab,
                                        struct Expression *expr) {
  if (expr == NULL)
    return 0;

  if (expr->type == EXPR_INUM)
    return (expr->expr_data.i_num >= 0 && expr->expr_data.i_num <= 255);

  if (expr->type == EXPR_VAR_ID && symtab != NULL &&
      expr->expr_data.id != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL &&
        (node->hash_type == HASHTYPE_CONST || node->is_typed_const)) {
      return (node->const_int_value >= 0 && node->const_int_value <= 255);
    }
  }

  return 0;
}

int semcheck_proccall(SymTab_t *symtab, struct Statement *stmt,
                      int max_scope_lev);
int semcheck_funccall(int *type_return, SymTab_t *symtab,
                      struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_compoundstmt(SymTab_t *symtab, struct Statement *stmt,
                          int max_scope_lev);
int semcheck_ifthen(SymTab_t *symtab, struct Statement *stmt,
                    int max_scope_lev);
int semcheck_while(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_repeat(SymTab_t *symtab, struct Statement *stmt,
                    int max_scope_lev);
int semcheck_for(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_for_in(SymTab_t *symtab, struct Statement *stmt,
                    int max_scope_lev);
int semcheck_for_assign(SymTab_t *symtab, struct Statement *for_assign,
                        int max_scope_lev);

int semcheck_statement_list_nodes(SymTab_t *symtab, ListNode_t *stmts,
                                  int max_scope_lev);
int semcheck_call_with_proc_var(SymTab_t *symtab, struct Statement *stmt,
                                HashNode_t *proc_node, int max_scope_lev);
int semcheck_try_property_assignment(SymTab_t *symtab, struct Statement *stmt,
                                     int max_scope_lev);
int semcheck_try_module_property_assignment(SymTab_t *symtab,
                                            struct Statement *stmt,
                                            int max_scope_lev);
int semcheck_convert_property_assignment_to_setter(SymTab_t *symtab,
                                                   struct Statement *stmt,
                                                   struct Expression *lhs,
                                                   HashNode_t *setter_node,
                                                   int max_scope_lev);
int semcheck_mangled_suffix_matches_untyped(const char *candidate_suffix,
                                            const char *call_suffix);
HashNode_t *semcheck_find_untyped_mangled_match(ListNode_t *candidates,
                                                const char *proc_id,
                                                const char *call_mangled);
int semcheck_var_decl_is_untyped(Tree_t *decl);
int semcheck_stmt_has_single_overload(SymTab_t *symtab, const char *proc_id);
int semcheck_stmt_try_set_method_mangled_id(SymTab_t *symtab,
                                            struct Statement *stmt,
                                            const char *proc_id,
                                            const char *mangled_id);
int semcheck_set_stmt_call_mangled_id(SymTab_t *symtab, struct Statement *stmt,
                                      int max_scope_lev);

int semcheck_stmt_has_single_overload(SymTab_t *symtab, const char *proc_id) {
  if (symtab == NULL || proc_id == NULL)
    return 0;

  ListNode_t *all_overloads = FindAllIdents(symtab, proc_id);
  int num_overloads = ListLength(all_overloads);
  DestroyList(all_overloads);
  return num_overloads <= 1;
}

int semcheck_stmt_try_set_method_mangled_id(SymTab_t *symtab,
                                            struct Statement *stmt,
                                            const char *proc_id,
                                            const char *mangled_id) {
  if (stmt == NULL || mangled_id == NULL ||
      !semcheck_stmt_has_single_overload(symtab, proc_id))
    return 0;

  if (stmt->stmt_data.procedure_call_data.mangled_id != NULL &&
      stmt->stmt_data.procedure_call_data.mangled_id != mangled_id)
    free(stmt->stmt_data.procedure_call_data.mangled_id);
  stmt->stmt_data.procedure_call_data.mangled_id = strdup(mangled_id);
  return stmt->stmt_data.procedure_call_data.mangled_id != NULL;
}

int semcheck_call_with_proc_var(SymTab_t *symtab, struct Statement *stmt,
                                HashNode_t *proc_node, int max_scope_lev) {
  if (proc_node == NULL || proc_node->type == NULL ||
      proc_node->type->kind != TYPE_KIND_PROCEDURE)
    return 0;

  int return_val = 0;
  ListNode_t *formal_params = proc_node->type->info.proc_info.params;
  ListNode_t *args_given = stmt->stmt_data.procedure_call_data.expr_args;
  int arg_index = 0;
  const char *callee_owner_full = proc_node->owner_class_full;
  const char *callee_owner_outer = proc_node->owner_class_outer;
  if (callee_owner_full == NULL && callee_owner_outer == NULL) {
    Tree_t *proc_def = proc_node->type->info.proc_info.definition;
    if (proc_def != NULL && proc_def->type == TREE_SUBPROGRAM) {
      callee_owner_full = proc_def->tree_data.subprogram_data.owner_class_full;
      callee_owner_outer =
          proc_def->tree_data.subprogram_data.owner_class_outer;
      if (callee_owner_full == NULL)
        callee_owner_full = proc_def->tree_data.subprogram_data.owner_class;
    }
  }

  while (formal_params != NULL && args_given != NULL) {
    ++arg_index;
    assert(formal_params->type == LIST_TREE);
    assert(args_given->type == LIST_EXPR);

    Tree_t *param_decl = (Tree_t *)formal_params->cur;
    struct Expression *arg_expr = (struct Expression *)args_given->cur;

    /* Phase 3: Use KgpcType for comprehensive type checking */
    if (arg_expr != NULL && arg_expr->type == EXPR_FUNCTION_CALL) {
      int cast_type = UNKNOWN_TYPE;
      int cast_result = semcheck_try_reinterpret_as_typecast(
          &cast_type, symtab, arg_expr, max_scope_lev);
      if (cast_result != 0)
        return cast_result;
    }

    /* Resolve KgpcType for the argument expression */
    int arg_type_owned = 0;
    KgpcType *arg_type = semcheck_resolve_expression_kgpc_type(
        symtab, arg_expr, INT_MAX, NO_MUTATE, &arg_type_owned);

    /* Resolve KgpcType for the formal parameter */
    int param_type_owned = 0;
    KgpcType *param_type = NULL;
    if (param_decl != NULL && (param_decl->type == TREE_VAR_DECL ||
                               param_decl->type == TREE_ARR_DECL)) {
      param_type =
          resolve_param_type_with_owner(param_decl, symtab, callee_owner_full,
                                        callee_owner_outer, &param_type_owned);
    }

    /* Both types must be resolved for proper type checking */
    int param_is_untyped = semcheck_var_decl_is_untyped(param_decl);

    if ((arg_type == NULL || param_type == NULL) && !param_is_untyped) {
      /* Suppress cascading errors when types can't be resolved —
       * upstream UNKNOWN_TYPE already reported the root cause. */
    } else if (!param_is_untyped) {
      /* Use comprehensive KgpcType-based type compatibility checking */
      if (!semcheck_param_types_compatible(param_decl, param_type, arg_type,
                                           symtab) &&
          !kgpc_type_equals_tag(arg_type, UNKNOWN_TYPE) &&
          !kgpc_type_equals_tag(param_type, UNKNOWN_TYPE)) {
        semcheck_error_with_context_at(
            stmt->line_num, stmt->col_num, stmt->source_index,
            "Error on line %d, on procedure call %s, argument %d: Type "
            "mismatch (expected %s, got %s)!\n\n",
            stmt->line_num, stmt->stmt_data.procedure_call_data.id, arg_index,
            kgpc_type_to_string(
                semcheck_param_effective_type(param_decl, param_type)),
            kgpc_type_to_string(arg_type));
        ++return_val;
      }
    }
    /* Untyped parameters accept any argument without additional checks */

    /* Clean up owned types */
    if (arg_type_owned && arg_type != NULL)
      destroy_kgpc_type(arg_type);
    if (param_type_owned && param_type != NULL)
      destroy_kgpc_type(param_type);

    formal_params = formal_params->next;
    args_given = args_given->next;
  }

  if (formal_params == NULL && args_given != NULL && !proc_node->is_varargs &&
      proc_node->hash_type != HASHTYPE_BUILTIN_PROCEDURE) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, on procedure call %s, too many arguments given!\n\n",
        stmt->line_num, stmt->stmt_data.procedure_call_data.id);
    ++return_val;
  } else if (formal_params != NULL && args_given == NULL) {
    /* Check if all remaining formal parameters have default values */
    int all_have_defaults = 1;
    for (ListNode_t *fp = formal_params; fp != NULL; fp = fp->next) {
      Tree_t *pd = (Tree_t *)fp->cur;
      if (pd == NULL || !param_has_default_value(pd)) {
        all_have_defaults = 0;
        break;
      }
    }
    if (!all_have_defaults) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, on procedure call %s, not enough arguments "
          "given!\n\n",
          stmt->line_num, stmt->stmt_data.procedure_call_data.id);
      ++return_val;
    }
  }

  return return_val;
}

int try_resolve_builtin_procedure(SymTab_t *symtab, struct Statement *stmt,
                                  const char *expected_name,
                                  builtin_semcheck_handler_t handler,
                                  int max_scope_lev, int *handled) {
  if (handled != NULL)
    *handled = 0;

  if (symtab == NULL || stmt == NULL || expected_name == NULL ||
      handler == NULL)
    return 0;

  char *proc_id = stmt->stmt_data.procedure_call_data.id;
  int forced_system_builtin = 0;
  if (stmt->stmt_data.procedure_call_data.is_method_call_placeholder) {
    const char *placeholder_name =
        stmt->stmt_data.procedure_call_data.placeholder_method_name;
    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    struct Expression *qualifier_expr =
        (args != NULL) ? (struct Expression *)args->cur : NULL;
    const char *derived_name = NULL;

    if (placeholder_name == NULL && proc_id != NULL && proc_id[0] == '_' &&
        proc_id[1] == '_' && proc_id[2] != '\0') {
      derived_name = proc_id + 2;
      placeholder_name = derived_name;
    }

    if (placeholder_name == NULL ||
        !pascal_identifier_equals(placeholder_name, expected_name) ||
        qualifier_expr == NULL || qualifier_expr->type != EXPR_VAR_ID ||
        qualifier_expr->expr_data.id == NULL ||
        !pascal_identifier_equals(qualifier_expr->expr_data.id, "System")) {
      return 0;
    }

    ListNode_t *remaining_args = args->next;
    destroy_expr(qualifier_expr);
    args->cur = NULL;
    free(args);
    stmt->stmt_data.procedure_call_data.expr_args = remaining_args;

    if (proc_id == NULL || !pascal_identifier_equals(proc_id, expected_name)) {
      free(proc_id);
      proc_id = strdup(expected_name);
      if (proc_id == NULL)
        return 1;
      stmt->stmt_data.procedure_call_data.id = proc_id;
    }

    stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
    if (stmt->stmt_data.procedure_call_data.placeholder_method_name != NULL) {
      free(stmt->stmt_data.procedure_call_data.placeholder_method_name);
      stmt->stmt_data.procedure_call_data.placeholder_method_name = NULL;
    }
    forced_system_builtin = 1;
  }

  if (proc_id == NULL || !pascal_identifier_equals(proc_id, expected_name))
    return 0;

  /* Prefer user-defined/prologue procedures over builtins when available.
   * Exception: System.XXX qualified calls always resolve to the builtin. */
  HashNode_t *existing = NULL;
  const char *qualifier = stmt->stmt_data.procedure_call_data.call_qualifier;
  int force_builtin =
      pascal_identifier_equals(expected_name, "Assign") ||
      pascal_identifier_equals(expected_name, "Val") ||
      pascal_identifier_equals(expected_name, "Str") || forced_system_builtin ||
      (qualifier != NULL && pascal_identifier_equals(qualifier, "System"));
  if (!force_builtin && FindSymbol(&existing, symtab, proc_id) != 0 &&
      existing != NULL && existing->hash_type != HASHTYPE_BUILTIN_PROCEDURE) {
    /* Builtin procedure names should still win over implicit/self method
     * visibility. For example, TFPList.Move must not shadow System.Move
     * inside another TFPList method body. Only non-method user/global
     * procedures should suppress builtin resolution here. */
    if (existing->owner_class != NULL)
      existing = NULL;
  }

  if (!force_builtin && existing != NULL &&
      existing->hash_type != HASHTYPE_BUILTIN_PROCEDURE) {
    return 0;
  }

  HashNode_t *builtin_node =
      FindIdentInTable(symtab->builtin_scope->table, proc_id);
  /* Also check unit_scopes[System]->table — builtin procedures live there since
   * per-unit scoping was added. */
  if (builtin_node == NULL) {
    int sys_idx = unit_registry_add("System");
    if (sys_idx > 0 && sys_idx < SYMTAB_MAX_UNITS &&
        symtab->unit_scopes[sys_idx] != NULL) {
      HashNode_t *sys_node =
          FindIdentInTable(symtab->unit_scopes[sys_idx]->table, proc_id);
      if (sys_node != NULL && sys_node->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
        builtin_node = sys_node;
    }
  }
  if (builtin_node != NULL &&
      builtin_node->hash_type == HASHTYPE_BUILTIN_PROCEDURE) {
    stmt->stmt_data.procedure_call_data.resolved_proc = builtin_node;
    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
      free(stmt->stmt_data.procedure_call_data.mangled_id);
    stmt->stmt_data.procedure_call_data.mangled_id = NULL;

    /* Populate call info to avoid use-after-free when HashNode is freed */
    stmt->stmt_data.procedure_call_data.call_hash_type =
        builtin_node->hash_type;
    semcheck_stmt_set_call_kgpc_type(
        stmt, builtin_node->type,
        stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
    stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;

    builtin_node->referenced += 1;
    if (handled != NULL)
      *handled = 1;
    return handler(symtab, stmt, max_scope_lev);
  }

  if (forced_system_builtin) {
    stmt->stmt_data.procedure_call_data.resolved_proc = NULL;
    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
      free(stmt->stmt_data.procedure_call_data.mangled_id);
    stmt->stmt_data.procedure_call_data.mangled_id = NULL;
    stmt->stmt_data.procedure_call_data.call_hash_type =
        HASHTYPE_BUILTIN_PROCEDURE;
    semcheck_stmt_set_call_kgpc_type(
        stmt, NULL,
        stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
    stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
    if (handled != NULL)
      *handled = 1;
    return handler(symtab, stmt, max_scope_lev);
  }

  if (qualifier != NULL && pascal_identifier_equals(qualifier, "System")) {
    stmt->stmt_data.procedure_call_data.resolved_proc = NULL;
    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
      free(stmt->stmt_data.procedure_call_data.mangled_id);
    stmt->stmt_data.procedure_call_data.mangled_id = NULL;
    stmt->stmt_data.procedure_call_data.call_hash_type =
        HASHTYPE_BUILTIN_PROCEDURE;
    semcheck_stmt_set_call_kgpc_type(
        stmt, NULL,
        stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
    stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
    if (handled != NULL)
      *handled = 1;
    return handler(symtab, stmt, max_scope_lev);
  }

  return 0;
}

int semcheck_builtin_setlength(SymTab_t *symtab, struct Statement *stmt,
                               int max_scope_lev) {
  int return_val = 0;
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || args->next == NULL || args->next->next != NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, SetLength expects exactly two arguments.\n",
        stmt->line_num);
    return 1;
  }

  struct Expression *array_expr = (struct Expression *)args->cur;
  struct Expression *length_expr = (struct Expression *)args->next->cur;

#ifdef DEBUG
  fprintf(stderr, "DEBUG: semcheck_builtin_setlength length_expr=%p\n",
          length_expr);
#endif

  int target_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&target_type, symtab, array_expr,
                                       max_scope_lev, MUTATE);
  int target_is_shortstring = semcheck_expr_is_shortstring(array_expr);
  int target_is_wide_string = 0;

  int target_is_string = (target_type == STRING_TYPE);
  /* Secondary check: KgpcType for string (e.g. function result vars with
   * overloads) */
  if (!target_is_string && !target_is_shortstring && array_expr != NULL &&
      array_expr->resolved_kgpc_type != NULL &&
      kgpc_type_is_string(array_expr->resolved_kgpc_type)) {
    target_is_string = 1;
  }
  if (array_expr != NULL && array_expr->resolved_kgpc_type != NULL &&
      kgpc_type_is_wide_string(array_expr->resolved_kgpc_type)) {
    target_is_wide_string = 1;
    target_is_string = 1;
  }
  if (target_is_string) {
    int target_is_dynarray = 0;
    if (array_expr != NULL && array_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_dynamic_array(array_expr->resolved_kgpc_type)) {
      target_is_dynarray = 1;
    } else if (array_expr != NULL && array_expr->type == EXPR_VAR_ID) {
      HashNode_t *array_node = NULL;
      if (FindSymbol(&array_node, symtab, array_expr->expr_data.id) != 0 &&
          array_node != NULL && array_node->type != NULL &&
          kgpc_type_is_dynamic_array(array_node->type)) {
        target_is_dynarray = 1;
      }
    }
    if (target_is_dynarray)
      target_is_string = 0;
  }

  if (target_is_string || target_is_shortstring) {
    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL) {
      free(stmt->stmt_data.procedure_call_data.mangled_id);
      stmt->stmt_data.procedure_call_data.mangled_id = NULL;
    }
    if (target_is_shortstring)
      stmt->stmt_data.procedure_call_data.mangled_id =
          strdup("__kgpc_setlength_shortstring");
    else if (target_is_wide_string)
      stmt->stmt_data.procedure_call_data.mangled_id =
          strdup("__kgpc_setlength_unicodestring");
    else
      stmt->stmt_data.procedure_call_data.mangled_id =
          strdup("__kgpc_setlength_string");
    if (stmt->stmt_data.procedure_call_data.mangled_id == NULL) {
      fprintf(stderr,
              "Error: failed to allocate mangled name for SetLength.\n");
      ++return_val;
    }
  } else {
    /* After semantic checking, check if the expression resolved to a dynamic
     * array */
    /* The expression could be EXPR_VAR_ID, EXPR_RECORD_ACCESS, etc. */
    int is_valid_array = 0;

    if (array_expr != NULL && array_expr->type == EXPR_VAR_ID) {
      /* Simple variable reference */
      HashNode_t *array_node = NULL;
      if (FindSymbol(&array_node, symtab, array_expr->expr_data.id) != 0 &&
          array_node != NULL) {
        set_hash_meta(array_node, BOTH_MUTATE_REFERENCE);

        /* Check if it's a dynamic array using KgpcType first, then legacy field
         */
        int is_dynamic = hashnode_is_dynamic_array(array_node);

        if (is_dynamic && (array_node->hash_type == HASHTYPE_ARRAY ||
                           array_node->hash_type == HASHTYPE_VAR ||
                           array_node->hash_type == HASHTYPE_FUNCTION_RETURN)) {
          is_valid_array = 1;
        }
      }
    } else if (array_expr != NULL && array_expr->type == EXPR_RECORD_ACCESS) {
      /* Record field access - verify the field is actually a dynamic array */
      if (array_expr->resolved_kgpc_type != NULL &&
          kgpc_type_is_dynamic_array(array_expr->resolved_kgpc_type)) {
        is_valid_array = 1;
      }
    } else if (array_expr != NULL && array_expr->type == EXPR_ARRAY_ACCESS) {
      /* Array access result - valid for nested dynamic arrays (array of array
       * of ...) */
      if (array_expr->resolved_kgpc_type != NULL &&
          kgpc_type_is_dynamic_array(array_expr->resolved_kgpc_type)) {
        is_valid_array = 1;
      }
    } else if (array_expr != NULL && array_expr->type == EXPR_POINTER_DEREF) {
      /* Pointer dereference - valid if it resolves to a dynamic array */
      if (array_expr->resolved_kgpc_type != NULL &&
          kgpc_type_is_dynamic_array(array_expr->resolved_kgpc_type)) {
        is_valid_array = 1;
      }
    } else if (array_expr != NULL && array_expr->type == EXPR_FUNCTION_CALL) {
      /* Function call result that returns a dynamic array reference */
      if (array_expr->resolved_kgpc_type != NULL &&
          kgpc_type_is_dynamic_array(array_expr->resolved_kgpc_type)) {
        is_valid_array = 1;
      }
    }

    if (!is_valid_array) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, first argument to SetLength must be a dynamic "
          "array variable.\n",
          stmt->line_num);
      ++return_val;
    }
  }

  int length_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&length_type, symtab, length_expr,
                                       max_scope_lev, NO_MUTATE);
  if (!is_integer_type(length_type)) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, SetLength length argument must be an integer.\n",
        stmt->line_num);
    ++return_val;
  }

  return return_val;
}

int semcheck_builtin_setstring(SymTab_t *symtab, struct Statement *stmt,
                               int max_scope_lev) {
  int return_val = 0;
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || args->next == NULL || args->next->next == NULL ||
      args->next->next->next != NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, SetString expects exactly three arguments.\n",
        stmt->line_num);
    return 1;
  }

  struct Expression *string_expr = (struct Expression *)args->cur;
  struct Expression *buffer_expr = (struct Expression *)args->next->cur;
  struct Expression *length_expr = (struct Expression *)args->next->next->cur;

  /* First argument must be a string variable (output parameter) */
  int string_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&string_type, symtab, string_expr,
                                       max_scope_lev, MUTATE);
  int target_is_shortstring = semcheck_expr_is_shortstring(string_expr);
  if (string_type != STRING_TYPE && string_type != SHORTSTRING_TYPE &&
      string_type != UNKNOWN_TYPE && !target_is_shortstring) {
    semcheck_error_with_context_at(stmt->line_num, stmt->col_num,
                                   stmt->source_index,
                                   "Error on line %d, SetString first argument "
                                   "must be a string variable.\n",
                                   stmt->line_num);
    ++return_val;
  }

  /* Second argument must be a PChar/pointer to char */
  int buffer_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&buffer_type, symtab, buffer_expr,
                                       max_scope_lev, NO_MUTATE);
  if (buffer_type != POINTER_TYPE && buffer_type != UNKNOWN_TYPE) {
    /* Allow if it's an array of char or similar */
    int is_valid = 0;
    if (buffer_expr != NULL && buffer_expr->resolved_kgpc_type != NULL) {
      KgpcType *t = buffer_expr->resolved_kgpc_type;
      if (t->kind == TYPE_KIND_POINTER)
        is_valid = 1;
    }
    if (!is_valid) {
      semcheck_error_with_context_at(stmt->line_num, stmt->col_num,
                                     stmt->source_index,
                                     "Error on line %d, SetString second "
                                     "argument must be a pointer (PChar).\n",
                                     stmt->line_num);
      ++return_val;
    }
  }

  /* Third argument must be an integer length */
  int length_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&length_type, symtab, length_expr,
                                       max_scope_lev, NO_MUTATE);
  if (!is_integer_type(length_type)) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, SetString length argument must be an integer.\n",
        stmt->line_num);
    ++return_val;
  }

  /* Set the mangled function name for codegen */
  if (stmt->stmt_data.procedure_call_data.mangled_id != NULL) {
    free(stmt->stmt_data.procedure_call_data.mangled_id);
    stmt->stmt_data.procedure_call_data.mangled_id = NULL;
  }
  if (target_is_shortstring)
    stmt->stmt_data.procedure_call_data.mangled_id =
        strdup("kgpc_shortstring_setstring");
  else
    stmt->stmt_data.procedure_call_data.mangled_id = strdup("kgpc_setstring");
  if (stmt->stmt_data.procedure_call_data.mangled_id == NULL) {
    fprintf(stderr, "Error: failed to allocate mangled name for SetString.\n");
    ++return_val;
  }

  return return_val;
}

int semcheck_statement_list_nodes(SymTab_t *symtab, ListNode_t *stmts,
                                  int max_scope_lev) {
  int result = 0;
  ListNode_t *cursor = stmts;
  while (cursor != NULL) {
    if (cursor->type == LIST_STMT && cursor->cur != NULL)
      result += semcheck_stmt_main(symtab, (struct Statement *)cursor->cur,
                                   max_scope_lev);
    cursor = cursor->next;
  }
  return result;
}

int semcheck_var_decl_is_untyped(Tree_t *decl) {
  if (decl == NULL || decl->type != TREE_VAR_DECL)
    return 0;
  struct Var *var_info = &decl->tree_data.var_decl_data;
  if (var_info->inline_record_type != NULL)
    return 0;
  return (var_info->type == UNKNOWN_TYPE && var_info->type_id == NULL);
}

int semcheck_mangled_suffix_matches_untyped(const char *candidate_suffix,
                                            const char *call_suffix) {
  if (candidate_suffix == NULL || call_suffix == NULL)
    return 0;

  if (*candidate_suffix == '\0' && *call_suffix == '\0')
    return 1;

  while (*candidate_suffix != '\0' && *call_suffix != '\0') {
    if (*candidate_suffix != '_' || *call_suffix != '_')
      return 0;
    candidate_suffix++;
    call_suffix++;

    const char *cand_end = candidate_suffix;
    while (*cand_end != '_' && *cand_end != '\0')
      cand_end++;
    const char *call_end = call_suffix;
    while (*call_end != '_' && *call_end != '\0')
      call_end++;

    size_t cand_len = (size_t)(cand_end - candidate_suffix);
    size_t call_len = (size_t)(call_end - call_suffix);
    int candidate_is_untyped = (cand_len == 1 && candidate_suffix[0] == 'u');

    if (!candidate_is_untyped) {
      if (cand_len != call_len ||
          strncmp(candidate_suffix, call_suffix, cand_len) != 0)
        return 0;
    }

    candidate_suffix = cand_end;
    call_suffix = call_end;
  }

  return (*candidate_suffix == '\0' && *call_suffix == '\0');
}

HashNode_t *semcheck_find_untyped_mangled_match(ListNode_t *candidates,
                                                const char *proc_id,
                                                const char *call_mangled) {
  if (candidates == NULL || proc_id == NULL || call_mangled == NULL)
    return NULL;

  size_t call_prefix_len = strlen(proc_id);
  if (strlen(call_mangled) < call_prefix_len ||
      strncmp(call_mangled, proc_id, call_prefix_len) != 0)
    return NULL;

  const char *call_suffix = call_mangled + call_prefix_len;
  ListNode_t *cur = candidates;
  while (cur != NULL) {
    HashNode_t *candidate = (HashNode_t *)cur->cur;
    if (candidate != NULL && candidate->mangled_id != NULL &&
        candidate->id != NULL) {
      if (pascal_identifier_equals(candidate->id, proc_id)) {
        size_t cand_prefix_len = strlen(candidate->id);
        if (strlen(candidate->mangled_id) >= cand_prefix_len &&
            strncmp(candidate->mangled_id, candidate->id, cand_prefix_len) ==
                0) {
          const char *cand_suffix = candidate->mangled_id + cand_prefix_len;
          if (semcheck_mangled_suffix_matches_untyped(cand_suffix, call_suffix))
            return candidate;
        }
      }
    }
    cur = cur->next;
  }

  return NULL;
}

int semcheck_builtin_strproc(SymTab_t *symtab, struct Statement *stmt,
                             int max_scope_lev) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || args->next == NULL || args->next->next != NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Str expects exactly two arguments.\n",
        stmt->line_num);
    return 1;
  }

  int return_val = 0;
  struct Expression *value_expr = (struct Expression *)args->cur;
  struct Expression *target_expr = (struct Expression *)args->next->cur;

  int value_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&value_type, symtab, value_expr, INT_MAX,
                                       NO_MUTATE);
  if (!is_ordinal_type(value_type) && !is_real_family_type(value_type)) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Str value must be an ordinal or real.\n",
        stmt->line_num);
    ++return_val;
  }

  if (value_expr != NULL && value_expr->field_width != NULL) {
    int width_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(
        &width_type, symtab, value_expr->field_width, INT_MAX, NO_MUTATE);
    if (!is_integer_type(width_type)) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, Str field width must be an integer.\n",
          stmt->line_num);
      ++return_val;
    }
  }

  if (value_expr != NULL && value_expr->field_precision != NULL) {
    int precision_type = UNKNOWN_TYPE;
    return_val +=
        semcheck_stmt_expr_tag(&precision_type, symtab,
                               value_expr->field_precision, INT_MAX, NO_MUTATE);
    if (!is_integer_type(precision_type)) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, Str field precision must be an integer.\n",
          stmt->line_num);
      ++return_val;
    }
  }

  int target_type = UNKNOWN_TYPE;
  int target_err = semcheck_stmt_expr_tag(&target_type, symtab, target_expr,
                                          max_scope_lev, MUTATE);
  if (target_err > 0 && target_expr != NULL &&
      target_expr->type == EXPR_TYPECAST) {
    /* Allow Inc on typecasted pointer expressions like Inc(PAnsiChar(p), ...)
     */
    target_err = semcheck_stmt_expr_tag(&target_type, symtab, target_expr,
                                        max_scope_lev, NO_MUTATE);
    struct Expression *inner = target_expr->expr_data.typecast_data.expr;
    if (inner != NULL)
      target_err +=
          semcheck_stmt_expr_tag(NULL, symtab, inner, max_scope_lev, MUTATE);
  }
  return_val += target_err;
  if (target_type != STRING_TYPE && target_type != SHORTSTRING_TYPE) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Str output must be a string variable.\n",
        stmt->line_num);
    ++return_val;
  }

  return return_val;
}

int semcheck_builtin_insert(SymTab_t *symtab, struct Statement *stmt,
                            int max_scope_lev) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || args->next == NULL || args->next->next == NULL ||
      args->next->next->next != NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Insert expects exactly three arguments.\n",
        stmt->line_num);
    return 1;
  }

  int error_count = 0;
  struct Expression *source_expr = (struct Expression *)args->cur;
  struct Expression *target_expr = (struct Expression *)args->next->cur;
  struct Expression *index_expr = (struct Expression *)args->next->next->cur;

  int source_type = UNKNOWN_TYPE;
  error_count += semcheck_stmt_expr_tag(&source_type, symtab, source_expr,
                                        max_scope_lev, NO_MUTATE);
  int source_is_shortstring = semcheck_expr_is_shortstring(source_expr);
  /* Also accept dynamic arrays (e.g. TCharArray for TStringBuilder) */
  int source_is_array =
      (source_expr != NULL && source_expr->resolved_kgpc_type != NULL &&
       source_expr->resolved_kgpc_type->kind == TYPE_KIND_ARRAY);
  if (source_type != STRING_TYPE && source_type != CHAR_TYPE &&
      source_type != SHORTSTRING_TYPE && !source_is_shortstring &&
      !source_is_array) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Insert source must be a string or char.\n",
        stmt->line_num);
    ++error_count;
  }

  int target_type = UNKNOWN_TYPE;
  error_count += semcheck_stmt_expr_tag(&target_type, symtab, target_expr,
                                        max_scope_lev, MUTATE);
  int target_is_shortstring = semcheck_expr_is_shortstring(target_expr);
  int target_is_array =
      (target_expr != NULL && target_expr->resolved_kgpc_type != NULL &&
       target_expr->resolved_kgpc_type->kind == TYPE_KIND_ARRAY);
  if (target_type != STRING_TYPE && target_type != SHORTSTRING_TYPE &&
      !target_is_shortstring && !target_is_array) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Insert target must be a string variable.\n",
        stmt->line_num);
    ++error_count;
  }

  int index_type = UNKNOWN_TYPE;
  error_count += semcheck_stmt_expr_tag(&index_type, symtab, index_expr,
                                        max_scope_lev, NO_MUTATE);
  if (!is_integer_type(index_type)) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Insert index must be an integer.\n", stmt->line_num);
    ++error_count;
  }

  if (error_count == 0 && target_is_shortstring) {
    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL) {
      free(stmt->stmt_data.procedure_call_data.mangled_id);
      stmt->stmt_data.procedure_call_data.mangled_id = NULL;
    }
    stmt->stmt_data.procedure_call_data.mangled_id =
        strdup("kgpc_shortstring_insert");
    if (stmt->stmt_data.procedure_call_data.mangled_id == NULL) {
      fprintf(stderr, "Error: failed to allocate mangled name for Insert.\n");
      ++error_count;
    }
  }

  return error_count;
}

int semcheck_builtin_delete(SymTab_t *symtab, struct Statement *stmt,
                            int max_scope_lev) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || args->next == NULL || args->next->next == NULL ||
      args->next->next->next != NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Delete expects exactly three arguments.\n",
        stmt->line_num);
    return 1;
  }

  int error_count = 0;
  struct Expression *target_expr = (struct Expression *)args->cur;
  struct Expression *index_expr = (struct Expression *)args->next->cur;
  struct Expression *count_expr = (struct Expression *)args->next->next->cur;

  int target_type = UNKNOWN_TYPE;
  error_count += semcheck_stmt_expr_tag(&target_type, symtab, target_expr,
                                        max_scope_lev, MUTATE);

  /* Check if target is a string type, shortstring (array of char), or dynamic
   * array */
  int is_valid_target =
      is_string_type(target_type) ||
      is_shortstring_array(target_type, target_expr->is_array_expr);
  /* Also accept dynamic arrays (FPC supports Delete on dynamic arrays) */
  if (!is_valid_target && target_expr->resolved_kgpc_type != NULL &&
      kgpc_type_is_dynamic_array(target_expr->resolved_kgpc_type))
    is_valid_target = 1;
  int target_is_shortstring = semcheck_expr_is_shortstring(target_expr);

  if (!is_valid_target) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Delete target must be a string variable.\n",
        stmt->line_num);
    ++error_count;
  }

  int index_type = UNKNOWN_TYPE;
  error_count += semcheck_stmt_expr_tag(&index_type, symtab, index_expr,
                                        max_scope_lev, NO_MUTATE);
  if (!is_integer_type(index_type)) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Delete index must be an integer.\n", stmt->line_num);
    ++error_count;
  }

  int count_type = UNKNOWN_TYPE;
  error_count += semcheck_stmt_expr_tag(&count_type, symtab, count_expr,
                                        max_scope_lev, NO_MUTATE);
  if (!is_integer_type(count_type)) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Delete count must be an integer.\n", stmt->line_num);
    ++error_count;
  }

  if (error_count == 0 && target_is_shortstring) {
    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL) {
      free(stmt->stmt_data.procedure_call_data.mangled_id);
      stmt->stmt_data.procedure_call_data.mangled_id = NULL;
    }
    stmt->stmt_data.procedure_call_data.mangled_id =
        strdup("kgpc_shortstring_delete");
    if (stmt->stmt_data.procedure_call_data.mangled_id == NULL) {
      fprintf(stderr, "Error: failed to allocate mangled name for Delete.\n");
      ++error_count;
    }
  }

  return error_count;
}

int semcheck_builtin_val(SymTab_t *symtab, struct Statement *stmt,
                         int max_scope_lev) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  int arg_count = ListLength(args);
  if (args == NULL || (arg_count != 2 && arg_count != 3)) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Val expects two or three arguments.\n",
        stmt->line_num);
    return 1;
  }

  int error_count = 0;

  struct Expression *source_expr = (struct Expression *)args->cur;
  int source_type = UNKNOWN_TYPE;
  error_count += semcheck_stmt_expr_tag(&source_type, symtab, source_expr,
                                        max_scope_lev, NO_MUTATE);
  if (!is_string_type(source_type) && source_type != CHAR_TYPE) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Val expects its first argument to be a string.\n",
        stmt->line_num);
    ++error_count;
  }

  struct Expression *value_expr = (struct Expression *)args->next->cur;
  int value_type = UNKNOWN_TYPE;
  error_count += semcheck_stmt_expr_tag(&value_type, symtab, value_expr,
                                        max_scope_lev, MUTATE);
  if (!is_integer_type(value_type) && !is_real_family_type(value_type)) {
    fprintf(stderr,
            "Error on line %d, Val target must be an integer, longint, or real "
            "variable.\n",
            stmt->line_num);
    ++error_count;
  }

  if (arg_count == 3) {
    struct Expression *code_expr = (struct Expression *)args->next->next->cur;
    int code_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&code_type, symtab, code_expr,
                                          max_scope_lev, MUTATE);
    if (!is_integer_type(code_type)) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, Val code argument must be an integer variable.\n",
          stmt->line_num);
      ++error_count;
    }
  }

  return error_count;
}

int semcheck_builtin_inc(SymTab_t *symtab, struct Statement *stmt,
                         int max_scope_lev) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || (args->next != NULL && args->next->next != NULL)) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Inc expects one or two arguments.\n",
        stmt->line_num);
    return 1;
  }

  int return_val = 0;
  struct Expression *target_expr = (struct Expression *)args->cur;
  int target_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&target_type, symtab, target_expr,
                                       max_scope_lev, MUTATE);
  int target_is_pointer = (target_type == POINTER_TYPE);
  if (!is_ordinal_type(target_type) && !target_is_pointer) {
    semcheck_error_with_context_at(stmt->line_num, stmt->col_num,
                                   stmt->source_index,
                                   "Error on line %d, Inc target must be an "
                                   "ordinal or pointer variable.\n",
                                   stmt->line_num);
    ++return_val;
  }

  if (args->next != NULL) {
    struct Expression *value_expr = (struct Expression *)args->next->cur;
    int value_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&value_type, symtab, value_expr,
                                         max_scope_lev, NO_MUTATE);
    int value_is_integer = is_integer_type(value_type);
    if (!value_is_integer && value_expr != NULL) {
      if (value_expr->type == EXPR_INUM)
        value_is_integer = 1;
      else if (value_expr->type == EXPR_FUNCTION_CALL &&
               value_expr->expr_data.function_call_data.id != NULL &&
               pascal_identifier_equals(
                   value_expr->expr_data.function_call_data.id, "SizeOf")) {
        value_is_integer = 1;
      }
    }
    if (!value_is_integer) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, Inc increment must be an integer.\n",
          stmt->line_num);
      ++return_val;
    }
  }

  return return_val;
}

int semcheck_builtin_dec(SymTab_t *symtab, struct Statement *stmt,
                         int max_scope_lev) {
  return semcheck_builtin_inc(symtab, stmt, max_scope_lev);
}

int semcheck_builtin_include_like(SymTab_t *symtab, struct Statement *stmt,
                                  int max_scope_lev, const char *display_name) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || args->next == NULL || args->next->next != NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, %s expects exactly two arguments.\n", stmt->line_num,
        display_name);
    return 1;
  }

  int error_count = 0;
  struct Expression *set_expr = (struct Expression *)args->cur;
  int set_type = UNKNOWN_TYPE;
  int set_type_owned = 0;
  error_count += semcheck_stmt_expr_tag(&set_type, symtab, set_expr,
                                        max_scope_lev, MUTATE);
  KgpcType *set_kgpc_type = semcheck_resolve_expression_kgpc_type(
      symtab, set_expr, max_scope_lev, MUTATE, &set_type_owned);
  if (set_type != SET_TYPE && !kgpc_type_is_set(set_kgpc_type)) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, %s target must be a set.\n", stmt->line_num,
        display_name);
    ++error_count;
  }
  if (set_type_owned && set_kgpc_type != NULL)
    destroy_kgpc_type(set_kgpc_type);

  struct Expression *value_expr = (struct Expression *)args->next->cur;
  int value_type = UNKNOWN_TYPE;
  error_count += semcheck_stmt_expr_tag(&value_type, symtab, value_expr,
                                        max_scope_lev, NO_MUTATE);
  if (!is_ordinal_type(value_type)) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, %s element must be an ordinal value.\n",
        stmt->line_num, display_name);
    ++error_count;
  }

  return error_count;
}

int semcheck_builtin_include(SymTab_t *symtab, struct Statement *stmt,
                             int max_scope_lev) {
  return semcheck_builtin_include_like(symtab, stmt, max_scope_lev, "Include");
}

int semcheck_builtin_exclude(SymTab_t *symtab, struct Statement *stmt,
                             int max_scope_lev) {
  return semcheck_builtin_include_like(symtab, stmt, max_scope_lev, "Exclude");
}

/* Initialize(var v) / Finalize(var v) - accept any managed type */
int semcheck_builtin_initialize_finalize(SymTab_t *symtab,
                                         struct Statement *stmt,
                                         int max_scope_lev,
                                         const char *display_name,
                                         int allow_count_arg) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  int arg_count = ListLength(args);
  if (arg_count < 1 || arg_count > (allow_count_arg ? 2 : 1)) {
    if (allow_count_arg)
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, %s expects one or two arguments.\n",
          stmt->line_num, display_name);
    else
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, %s expects exactly one argument.\n",
          stmt->line_num, display_name);
    return 1;
  }

  int error_count = 0;
  struct Expression *arg_expr = (struct Expression *)args->cur;
  int arg_type = UNKNOWN_TYPE;
  error_count += semcheck_stmt_expr_tag(&arg_type, symtab, arg_expr,
                                        max_scope_lev, MUTATE);
  /* Accept any type - Initialize/Finalize work with all managed types */
  if (allow_count_arg && args->next != NULL) {
    struct Expression *count_expr = (struct Expression *)args->next->cur;
    int count_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&count_type, symtab, count_expr,
                                          max_scope_lev, NO_MUTATE);
    if (!is_integer_type(count_type)) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, %s count argument must be an integer.\n",
          stmt->line_num, display_name);
      ++error_count;
    }
  }
  return error_count;
}

int semcheck_builtin_initialize(SymTab_t *symtab, struct Statement *stmt,
                                int max_scope_lev) {
  return semcheck_builtin_initialize_finalize(symtab, stmt, max_scope_lev,
                                              "Initialize", 0);
}

int semcheck_builtin_finalize(SymTab_t *symtab, struct Statement *stmt,
                              int max_scope_lev) {
  return semcheck_builtin_initialize_finalize(symtab, stmt, max_scope_lev,
                                              "Finalize", 1);
}

int semcheck_builtin_assert(SymTab_t *symtab, struct Statement *stmt,
                            int max_scope_lev) {
  int return_val = 0;
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  int arg_count = ListLength(args);
  if (arg_count < 1 || arg_count > 2) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Assert expects 1 or 2 arguments.\n", stmt->line_num);
    return 1;
  }

  /* First argument: boolean condition */
  struct Expression *cond_expr = (struct Expression *)args->cur;
  int cond_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&cond_type, symtab, cond_expr,
                                       max_scope_lev, NO_MUTATE);

  /* Second argument (optional): string message */
  if (args->next != NULL) {
    struct Expression *msg_expr = (struct Expression *)args->next->cur;
    int msg_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&msg_type, symtab, msg_expr,
                                         max_scope_lev, NO_MUTATE);
  }

  return return_val;
}

int semcheck_builtin_write_like(SymTab_t *symtab, struct Statement *stmt,
                                int max_scope_lev) {
  int return_val = 0;
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  int arg_index = 1;
  int saw_file_arg = 0;
  while (args != NULL) {
    struct Expression *expr = (struct Expression *)args->cur;
    int expr_type = UNKNOWN_TYPE;
    return_val +=
        semcheck_stmt_expr_tag(&expr_type, symtab, expr, INT_MAX, NO_MUTATE);
    int expr_is_char_array = 0;
    if (expr_type == UNKNOWN_TYPE && expr != NULL &&
        expr->resolved_kgpc_type != NULL) {
      KgpcType *expr_type_kgpc = expr->resolved_kgpc_type;
      if (kgpc_type_is_array(expr_type_kgpc)) {
        KgpcType *elem_type =
            kgpc_type_get_array_element_type_resolved(expr_type_kgpc, symtab);
        if (elem_type != NULL && elem_type->kind == TYPE_KIND_PRIMITIVE &&
            elem_type->info.primitive_type_tag == CHAR_TYPE) {
          expr_is_char_array = 1;
        }
      }
    }

    if (!saw_file_arg && expr_type == TEXT_TYPE) {
      saw_file_arg = 1;
      args = args->next;
      continue;
    }

    int expr_is_real =
        (expr_type == REAL_TYPE) || semcheck_expr_is_real_family(expr);

    if (!is_integer_type(expr_type) && expr_type != STRING_TYPE &&
        expr_type != SHORTSTRING_TYPE && expr_type != BOOL &&
        expr_type != POINTER_TYPE && !expr_is_real && expr_type != CHAR_TYPE &&
        expr_type != ENUM_TYPE && !expr_is_char_array &&
        expr_type != UNKNOWN_TYPE && expr_type != RECORD_TYPE &&
        expr_type != PROCEDURE) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, write argument %d must be integer, longint, real, "
          "boolean, string, pointer, or enum.\n",
          stmt->line_num, arg_index);
      ++return_val;
    }

    if (expr != NULL && expr->field_width != NULL) {
      int width_type = UNKNOWN_TYPE;
      return_val += semcheck_stmt_expr_tag(
          &width_type, symtab, expr->field_width, INT_MAX, NO_MUTATE);
      if (!is_integer_type(width_type)) {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num,
                                       stmt->source_index,
                                       "Error on line %d, field width for "
                                       "argument %d must be an integer.\n",
                                       stmt->line_num, arg_index);
        ++return_val;
      }
    }

    if (expr != NULL && expr->field_precision != NULL) {
      int precision_type = UNKNOWN_TYPE;
      return_val += semcheck_stmt_expr_tag(
          &precision_type, symtab, expr->field_precision, INT_MAX, NO_MUTATE);
      if (!is_integer_type(precision_type)) {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num,
                                       stmt->source_index,
                                       "Error on line %d, field precision for "
                                       "argument %d must be an integer.\n",
                                       stmt->line_num, arg_index);
        ++return_val;
      }
    }

    args = args->next;
    ++arg_index;
  }

  return return_val;
}

/* WriteStr(var S: string; args...) - format values into a string variable */
int semcheck_builtin_writestr(SymTab_t *symtab, struct Statement *stmt,
                              int max_scope_lev) {
  int return_val = 0;
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, WriteStr requires at least one argument.\n",
        stmt->line_num);
    return 1;
  }

  /* First argument must be a string variable (var parameter) */
  struct Expression *dest_expr = (struct Expression *)args->cur;
  int dest_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&dest_type, symtab, dest_expr,
                                       max_scope_lev, MUTATE);

  if (dest_type != STRING_TYPE && dest_type != SHORTSTRING_TYPE) {
    semcheck_error_with_context_at(stmt->line_num, stmt->col_num,
                                   stmt->source_index,
                                   "Error on line %d, WriteStr first argument "
                                   "must be a string variable.\n",
                                   stmt->line_num);
    ++return_val;
  }

  /* Remaining arguments are values to format */
  args = args->next;
  int arg_index = 2;
  while (args != NULL) {
    struct Expression *expr = (struct Expression *)args->cur;
    int expr_type = UNKNOWN_TYPE;
    return_val +=
        semcheck_stmt_expr_tag(&expr_type, symtab, expr, INT_MAX, NO_MUTATE);

    int expr_is_real =
        (expr_type == REAL_TYPE) || semcheck_expr_is_real_family(expr);

    if (!is_integer_type(expr_type) && expr_type != STRING_TYPE &&
        expr_type != SHORTSTRING_TYPE && expr_type != BOOL &&
        expr_type != POINTER_TYPE && !expr_is_real && expr_type != CHAR_TYPE &&
        expr_type != ENUM_TYPE) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, WriteStr argument %d must be integer, real, "
          "boolean, string, pointer, or enum.\n",
          stmt->line_num, arg_index);
      ++return_val;
    }

    args = args->next;
    ++arg_index;
  }

  return return_val;
}

static int semcheck_expr_is_real_family(const struct Expression *expr) {
  return (expr != NULL && expr->resolved_kgpc_type != NULL &&
          kgpc_type_is_real(expr->resolved_kgpc_type));
}

int semcheck_builtin_read_like(SymTab_t *symtab, struct Statement *stmt,
                               int max_scope_lev) {
  int return_val = 0;
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  int arg_index = 1;
  int saw_file_arg = 0;

  while (args != NULL) {
    struct Expression *expr = (struct Expression *)args->cur;
    int expr_type = UNKNOWN_TYPE;

    /* For read, we need to check if this is a file argument first */
    return_val += semcheck_stmt_expr_tag(&expr_type, symtab, expr,
                                         max_scope_lev, NO_MUTATE);

    if (!saw_file_arg && expr_type == TEXT_TYPE) {
      saw_file_arg = 1;
      args = args->next;
      arg_index++;
      continue;
    }

    /* After file arg (if any), remaining args must be mutable lvalues */
    /* Re-check with MUTATE flag to ensure it's an lvalue */
    expr_type = UNKNOWN_TYPE;
    return_val +=
        semcheck_stmt_expr_tag(&expr_type, symtab, expr, max_scope_lev, MUTATE);

    if (!is_integer_type(expr_type) && expr_type != CHAR_TYPE &&
        !is_string_type(expr_type) && expr_type != REAL_TYPE &&
        !semcheck_expr_is_real_family(expr)) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, read argument %d must be integer, longint, real, "
          "char, or string variable.\n",
          stmt->line_num, arg_index);
      ++return_val;
    }

    args = args->next;
    ++arg_index;
  }

  return return_val;
}

int semcheck_builtin_untyped_call(SymTab_t *symtab, struct Statement *stmt,
                                  int max_scope_lev, int first_arg_mutate) {
  int return_val = 0;
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  int arg_index = 0;
  while (args != NULL) {
    struct Expression *expr = (struct Expression *)args->cur;
    int expr_type = UNKNOWN_TYPE;
    int mutate_flag = (arg_index == 0 && first_arg_mutate) ? MUTATE : NO_MUTATE;
    return_val += semcheck_stmt_expr_tag(&expr_type, symtab, expr,
                                         max_scope_lev, mutate_flag);
    args = args->next;
    ++arg_index;
  }

  return_val += semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
  return return_val;
}

int semcheck_builtin_assign(SymTab_t *symtab, struct Statement *stmt,
                            int max_scope_lev) {
  return semcheck_builtin_untyped_call(symtab, stmt, max_scope_lev, 1);
}

int semcheck_builtin_close(SymTab_t *symtab, struct Statement *stmt,
                           int max_scope_lev) {
  return semcheck_builtin_untyped_call(symtab, stmt, max_scope_lev, 1);
}

int semcheck_builtin_settextcodepage(SymTab_t *symtab, struct Statement *stmt,
                                     int max_scope_lev) {
  return semcheck_builtin_untyped_call(symtab, stmt, max_scope_lev, 1);
}

/*
 * Per-parameter penalty classification for write/writeln overload resolution.
 * Lower penalty = better match.  Penalties are summed across all parameters
 * to produce a total candidate penalty.
 *
 * The penalty tiers are:
 *   EXACT (0)        — types match exactly
 *   INT_PROMOTION (1) — Int ↔ LongInt interchangeable
 *   STRING_SUBTYPE (2) — STRING_TYPE matches but UnicodeString is less
 * preferred FILE_SUBTYPE (3)  — FILE_TYPE matches but TypedFile doesn't match
 * plain File INCOMPATIBLE (1000) — no valid conversion
 */
enum {
  WRITE_PENALTY_EXACT = 0,
  WRITE_PENALTY_INT_PROMOTION = 1,
  WRITE_PENALTY_STRING_SUBTYPE = 2,
  WRITE_PENALTY_FILE_SUBTYPE = 3,
  WRITE_PENALTY_INCOMPATIBLE = 1000
};

static int semcheck_write_param_penalty(Tree_t *formal_decl, int formal_type,
                                        int actual_type) {
  if (formal_type == UNKNOWN_TYPE || actual_type == UNKNOWN_TYPE)
    return WRITE_PENALTY_EXACT;

  if (formal_type == actual_type) {
    /* When both are STRING_TYPE, prefer RawByteString over UnicodeString.
     * RawByteString is FPC's catch-all byte string type; UnicodeString
     * requires codepage conversion. */
    if (formal_type == STRING_TYPE) {
      const char *formal_type_id =
          (formal_decl != NULL && formal_decl->type == TREE_VAR_DECL)
              ? formal_decl->tree_data.var_decl_data.type_id
              : NULL;
      if (formal_type_id != NULL &&
          strcasecmp(formal_type_id, "UnicodeString") == 0)
        return WRITE_PENALTY_STRING_SUBTYPE;
    }
    /* When both are FILE_TYPE, prefer plain File over TypedFile.
     * A TypedFile formal should not match a plain File actual. */
    if (formal_type == FILE_TYPE) {
      const char *formal_type_id =
          (formal_decl != NULL && formal_decl->type == TREE_VAR_DECL)
              ? formal_decl->tree_data.var_decl_data.type_id
              : NULL;
      if (formal_type_id != NULL &&
          strcasecmp(formal_type_id, "TypedFile") == 0)
        return WRITE_PENALTY_FILE_SUBTYPE;
    }
    return WRITE_PENALTY_EXACT;
  }

  if ((formal_type == LONGINT_TYPE && actual_type == INT_TYPE) ||
      (formal_type == INT_TYPE && actual_type == LONGINT_TYPE))
    return WRITE_PENALTY_INT_PROMOTION;

  return WRITE_PENALTY_INCOMPATIBLE;
}

/*
 * Compute the total penalty for a write/writeln candidate by summing
 * per-parameter penalties.  Used for both initial scoring and tie-breaking
 * recomputation (eliminating code duplication).
 */
static int semcheck_write_candidate_total_penalty(HashNode_t *candidate,
                                                  ListNode_t *actual_args,
                                                  SymTab_t *symtab,
                                                  int max_scope_lev) {
  assert(candidate != NULL);
  assert(candidate->type != NULL);
  assert(candidate->type->kind == TYPE_KIND_PROCEDURE);

  ListNode_t *formal = candidate->type->info.proc_info.params;
  ListNode_t *actual = actual_args;
  int total = 0;

  while (formal != NULL && actual != NULL) {
    Tree_t *formal_decl = (Tree_t *)formal->cur;
    struct Expression *actual_expr = (struct Expression *)actual->cur;
    int formal_type = resolve_param_type(formal_decl, symtab);
    int actual_type = UNKNOWN_TYPE;
    semcheck_stmt_expr_tag(&actual_type, symtab, actual_expr, max_scope_lev,
                           NO_MUTATE);

    total +=
        semcheck_write_param_penalty(formal_decl, formal_type, actual_type);

    formal = formal->next;
    actual = actual->next;
  }

  return total;
}

int semcheck_set_stmt_call_mangled_id(SymTab_t *symtab, struct Statement *stmt,
                                      int max_scope_lev) {
  if (symtab == NULL || stmt == NULL)
    return 0;

  const char *proc_id = stmt->stmt_data.procedure_call_data.id;
  if (proc_id == NULL)
    return 0;

  char *mangled = MangleFunctionNameFromCallSite(
      proc_id, stmt->stmt_data.procedure_call_data.expr_args, symtab,
      max_scope_lev);
  if (mangled == NULL) {
    fprintf(stderr, "Error: failed to mangle procedure name for call to %s.\n",
            proc_id);
    return 1;
  }

  ListNode_t *candidates = FindAllIdents(symtab, proc_id);
  HashNode_t *exact_match = NULL;
  if (candidates != NULL) {
    for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next) {
      HashNode_t *candidate = (HashNode_t *)cur->cur;
      if (candidate != NULL && candidate->mangled_id != NULL &&
          strcmp(candidate->mangled_id, mangled) == 0) {
        exact_match = candidate;
        break;
      }
    }
  }
  if (exact_match == NULL && candidates != NULL) {
    HashNode_t *wildcard =
        semcheck_find_untyped_mangled_match(candidates, proc_id, mangled);
    if (wildcard != NULL && wildcard->mangled_id != NULL) {
      free(mangled);
      mangled = strdup(wildcard->mangled_id);
      if (mangled == NULL) {
        if (candidates != NULL)
          DestroyList(candidates);
        fprintf(stderr,
                "Error: failed to allocate mangled procedure name for %s.\n",
                proc_id);
        return 1;
      }
    }
  }
  if (exact_match == NULL && candidates != NULL) {
    int call_arg_count =
        ListLength(stmt->stmt_data.procedure_call_data.expr_args);
    HashNode_t *best_match = NULL;
    int best_penalty = WRITE_PENALTY_INCOMPATIBLE + 1;
    int num_best = 0;

    for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next) {
      HashNode_t *candidate = (HashNode_t *)cur->cur;
      if (candidate == NULL || candidate->type == NULL ||
          candidate->type->kind != TYPE_KIND_PROCEDURE)
        continue;
      ListNode_t *formal_params = candidate->type->info.proc_info.params;
      if (ListLength(formal_params) != call_arg_count)
        continue;

      int penalty = semcheck_write_candidate_total_penalty(
          candidate, stmt->stmt_data.procedure_call_data.expr_args, symtab,
          max_scope_lev);

      if (penalty < best_penalty) {
        best_penalty = penalty;
        best_match = candidate;
        num_best = 1;
      } else if (penalty == best_penalty) {
        num_best++;
      }
    }

    /* When multiple candidates tie, check if they all share the same
     * mangled_id (e.g. File and TypedFile overloads both mangling as
     * assign_f_rbs).  If so, they're effectively the same overload. */
    if (num_best > 1 && best_match != NULL && best_match->mangled_id != NULL) {
      int all_same = 1;
      for (ListNode_t *cur2 = candidates; cur2 != NULL && all_same;
           cur2 = cur2->next) {
        HashNode_t *c2 = (HashNode_t *)cur2->cur;
        if (c2 == NULL || c2->type == NULL ||
            c2->type->kind != TYPE_KIND_PROCEDURE)
          continue;
        if (ListLength(c2->type->info.proc_info.params) != call_arg_count)
          continue;
        int c2_penalty = semcheck_write_candidate_total_penalty(
            c2, stmt->stmt_data.procedure_call_data.expr_args, symtab,
            max_scope_lev);
        if (c2_penalty == best_penalty && c2->mangled_id != NULL &&
            strcmp(c2->mangled_id, best_match->mangled_id) != 0) {
          all_same = 0;
        }
      }
      if (all_same)
        num_best = 1;
    }

    if (num_best == 1 && best_match != NULL && best_match->mangled_id != NULL) {
      free(mangled);
      mangled = strdup(best_match->mangled_id);
      if (mangled == NULL) {
        if (candidates != NULL)
          DestroyList(candidates);
        fprintf(stderr,
                "Error: failed to allocate mangled procedure name for %s.\n",
                proc_id);
        return 1;
      }
    }
  }
  if (candidates != NULL)
    DestroyList(candidates);

  if (stmt->stmt_data.procedure_call_data.mangled_id != NULL) {
    free(stmt->stmt_data.procedure_call_data.mangled_id);
    stmt->stmt_data.procedure_call_data.mangled_id = NULL;
  }
  stmt->stmt_data.procedure_call_data.mangled_id = mangled;
  return 0;
}

int semcheck_builtin_halt(SymTab_t *symtab, struct Statement *stmt,
                          int max_scope_lev) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL) {
    struct Expression *zero_expr = mk_inum(stmt->line_num, 0);
    if (zero_expr == NULL) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, failed to allocate Halt argument.\n",
          stmt->line_num);
      return 1;
    }
    stmt->stmt_data.procedure_call_data.expr_args =
        CreateListNode(zero_expr, LIST_EXPR);
    return semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
  }

  if (args->next != NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Halt expects zero or one argument.\n",
        stmt->line_num);
    return 1;
  }

  int return_val = 0;
  struct Expression *code_expr = (struct Expression *)args->cur;
  int code_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&code_type, symtab, code_expr,
                                       max_scope_lev, NO_MUTATE);
  return_val += semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
  return return_val;
}

int semcheck_builtin_error(SymTab_t *symtab, struct Statement *stmt,
                           int max_scope_lev) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || args->next != NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Error expects exactly one argument.\n",
        stmt->line_num);
    return 1;
  }

  int return_val = 0;
  struct Expression *code_expr = (struct Expression *)args->cur;
  int code_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&code_type, symtab, code_expr,
                                       max_scope_lev, NO_MUTATE);

  /* System.Error(code) follows Halt semantics in our runtime.
   * Lower to Halt so existing mangling/codegen/runtime paths are reused. */
  if (!pascal_identifier_equals(stmt->stmt_data.procedure_call_data.id,
                                "Halt")) {
    char *halt_name = strdup("Halt");
    if (halt_name == NULL) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, failed to rewrite Error call to Halt.\n",
          stmt->line_num);
      return return_val + 1;
    }

    free(stmt->stmt_data.procedure_call_data.id);
    stmt->stmt_data.procedure_call_data.id = halt_name;
  }

  return_val += semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
  return return_val;
}

int semcheck_builtin_getmem(SymTab_t *symtab, struct Statement *stmt,
                            int max_scope_lev) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  int arg_count = ListLength(args);
  if (arg_count < 1 || arg_count > 2) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, GetMem expects one or two arguments.\n",
        stmt->line_num);
    return 1;
  }

  int return_val = 0;
  if (arg_count == 1) {
    struct Expression *size_expr = (struct Expression *)args->cur;
    int size_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&size_type, symtab, size_expr,
                                         max_scope_lev, NO_MUTATE);
    return_val +=
        semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
    return return_val;
  }

  struct Expression *target_expr = (struct Expression *)args->cur;
  struct Expression *size_expr = (struct Expression *)args->next->cur;
  int target_type = UNKNOWN_TYPE;
  int size_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&target_type, symtab, target_expr,
                                       max_scope_lev, MUTATE);
  return_val += semcheck_stmt_expr_tag(&size_type, symtab, size_expr,
                                       max_scope_lev, NO_MUTATE);
  return_val += semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
  return return_val;
}

int semcheck_builtin_freemem(SymTab_t *symtab, struct Statement *stmt,
                             int max_scope_lev) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  int arg_count = ListLength(args);
  if (arg_count < 1 || arg_count > 2) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, FreeMem expects one or two arguments.\n",
        stmt->line_num);
    return 1;
  }

  int return_val = 0;
  if (args != NULL) {
    struct Expression *ptr_expr = (struct Expression *)args->cur;
    int ptr_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&ptr_type, symtab, ptr_expr,
                                         max_scope_lev, NO_MUTATE);
    if (args->next != NULL) {
      struct Expression *size_expr = (struct Expression *)args->next->cur;
      int size_type = UNKNOWN_TYPE;
      return_val += semcheck_stmt_expr_tag(&size_type, symtab, size_expr,
                                           max_scope_lev, NO_MUTATE);
    }
  }

  return_val += semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
  return return_val;
}

int semcheck_builtin_move(SymTab_t *symtab, struct Statement *stmt,
                          int max_scope_lev) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || args->next == NULL || args->next->next == NULL ||
      args->next->next->next != NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Move expects exactly three arguments.\n",
        stmt->line_num);
    return 1;
  }

  int return_val = 0;
  struct Expression *src_expr = (struct Expression *)args->cur;
  struct Expression *dst_expr = (struct Expression *)args->next->cur;
  struct Expression *count_expr = (struct Expression *)args->next->next->cur;
  int src_type = UNKNOWN_TYPE;
  int dst_type = UNKNOWN_TYPE;
  int count_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&src_type, symtab, src_expr,
                                       max_scope_lev, NO_MUTATE);
  return_val += semcheck_stmt_expr_tag(&dst_type, symtab, dst_expr,
                                       max_scope_lev, MUTATE);
  return_val += semcheck_stmt_expr_tag(&count_type, symtab, count_expr,
                                       max_scope_lev, NO_MUTATE);
  return return_val;
}

int semcheck_builtin_reallocmem(SymTab_t *symtab, struct Statement *stmt,
                                int max_scope_lev) {
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || args->next == NULL || args->next->next != NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, ReallocMem expects exactly two arguments.\n",
        stmt->line_num);
    return 1;
  }

  int return_val = 0;
  struct Expression *target_expr = (struct Expression *)args->cur;
  struct Expression *size_expr = (struct Expression *)args->next->cur;
  int target_type = UNKNOWN_TYPE;
  int size_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&target_type, symtab, target_expr,
                                       max_scope_lev, NO_MUTATE);
  return_val += semcheck_stmt_expr_tag(&size_type, symtab, size_expr,
                                       max_scope_lev, NO_MUTATE);
  return return_val;
}

int semcheck_builtin_new(SymTab_t *symtab, struct Statement *stmt,
                         int max_scope_lev) {
  int return_val = 0;
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  int arg_count = ListLength(args);
  if (args == NULL || arg_count > 2) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, New expects one or two arguments.\\n",
        stmt->line_num);
    return 1;
  }

  struct Expression *target_expr = (struct Expression *)args->cur;
  int pointer_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&pointer_type, symtab, target_expr,
                                       max_scope_lev, MUTATE);

  if (pointer_type != POINTER_TYPE) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, New expects a pointer variable argument.\\n",
        stmt->line_num);
    return ++return_val;
  }

  /* An array-index expression whose element is a pointer (e.g.
   * `new(fbitmap[x1,y1])`) carries its pointee in resolved_kgpc_type rather
   * than in the legacy pointer_subtype fields. */
  int has_kgpc_pointee =
      (target_expr->resolved_kgpc_type != NULL &&
       target_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER &&
       kgpc_type_resolve_pointer_pointee(target_expr->resolved_kgpc_type,
                                         symtab) != NULL);
  if (target_expr->pointer_subtype == UNKNOWN_TYPE &&
      target_expr->pointer_subtype_id == NULL && !has_kgpc_pointee) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, unable to determine allocation type for New.\\n",
        stmt->line_num);
    return ++return_val;
  }

  return return_val;
}

int semcheck_builtin_dispose(SymTab_t *symtab, struct Statement *stmt,
                             int max_scope_lev) {
  int return_val = 0;
  if (stmt == NULL)
    return 0;

  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  int arg_count = ListLength(args);
  if (args == NULL || arg_count > 2) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Dispose expects one or two arguments.\\n",
        stmt->line_num);
    return 1;
  }

  struct Expression *target_expr = (struct Expression *)args->cur;
  int pointer_type = UNKNOWN_TYPE;
  return_val += semcheck_stmt_expr_tag(&pointer_type, symtab, target_expr,
                                       max_scope_lev, MUTATE);

  if (pointer_type != POINTER_TYPE && pointer_type != UNKNOWN_TYPE) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, Dispose expects a pointer variable argument.\\n",
        stmt->line_num);
    return ++return_val;
  }

  return return_val;
}

/* Semantic check on a normal statement */
