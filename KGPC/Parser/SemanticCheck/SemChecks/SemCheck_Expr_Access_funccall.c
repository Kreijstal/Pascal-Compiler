/*
    SemCheck_Expr_Access_funccall.c - Function/procedure call semantic checking

    Extracted from SemCheck_Expr_Access.c. Contains semcheck_funccall and its
    exclusive static helpers.
*/

#include "SemCheck_Expr_Internal.h"
#include "SemCheck_stmt.h"
#include <ctype.h>
#include <limits.h>
#include <time.h>

#include "SemCheck_funccall_internal.h"

double funccall_now_ms(void) {
  return (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
}

int semcheck_expr_is_explicit_char_typecast_for_call_local(
    const struct Expression *expr) {
  if (expr == NULL || expr->type != EXPR_TYPECAST)
    return 0;
  const char *target_id = expr->expr_data.typecast_data.target_type_id;
  if (target_id == NULL)
    return 0;
  return pascal_identifier_equals(target_id, "Char") ||
         pascal_identifier_equals(target_id, "AnsiChar") ||
         pascal_identifier_equals(target_id, "WideChar") ||
         pascal_identifier_equals(target_id, "UnicodeChar");
}

int semcheck_kgpc_type_is_char_like_for_call_local(const KgpcType *type) {
  if (type == NULL)
    return 0;
  if (kgpc_type_is_char((KgpcType *)type))
    return 1;
  if (type->type_alias != NULL && type->type_alias->is_char_alias)
    return 1;
  return 0;
}

int semcheck_expr_is_char_typecast_call_for_call_local(
    const struct Expression *expr) {
  if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
    return 0;
  if (expr->expr_data.function_call_data.args_expr == NULL ||
      expr->expr_data.function_call_data.args_expr->next != NULL)
    return 0;
  const char *target_id = expr->expr_data.function_call_data.id;
  if (target_id == NULL)
    return 0;
  return pascal_identifier_equals(target_id, "Char") ||
         pascal_identifier_equals(target_id, "AnsiChar") ||
         pascal_identifier_equals(target_id, "WideChar") ||
         pascal_identifier_equals(target_id, "UnicodeChar");
}

int semcheck_try_rewrite_internconst_call(int *type_return, SymTab_t *symtab,
                                          struct Expression *expr) {
  HashNode_t *target = NULL;
  const char *intrinsic_id;

  if (type_return == NULL || symtab == NULL || expr == NULL ||
      expr->type != EXPR_FUNCTION_CALL ||
      expr->expr_data.function_call_data.id == NULL)
    return 0;

  if (FindSymbol(&target, symtab, expr->expr_data.function_call_data.id) == 0 ||
      target == NULL || target->internconst_id == NULL ||
      target->internconst_id[0] == '\0')
    return 0;

  intrinsic_id = target->internconst_id;

  if (strcasecmp(intrinsic_id, "fpc_in_const_ptr") != 0)
    return 0;

  /* SPtr is emitted as a bare zero-arg function reference in FPC RTL startup
   * code. Lower it to the runtime stack-pointer helper before normal overload
   * resolution so it doesn't fall back to ordinary mangling. */
  if (expr->expr_data.function_call_data.args_expr != NULL ||
      !pascal_identifier_equals(target->id, "SPtr"))
    return 0;

  free(expr->expr_data.function_call_data.id);
  expr->expr_data.function_call_data.id = strdup("sptr_void");
  if (expr->expr_data.function_call_data.mangled_id != NULL)
    free(expr->expr_data.function_call_data.mangled_id);
  expr->expr_data.function_call_data.mangled_id = strdup("sptr_void");
  semcheck_reset_function_call_cache(expr);
  *type_return = POINTER_TYPE;
  if (expr->resolved_kgpc_type != NULL) {
    destroy_kgpc_type(expr->resolved_kgpc_type);
    expr->resolved_kgpc_type = NULL;
  }
  expr->resolved_kgpc_type = create_primitive_type(POINTER_TYPE);
  return 1;
}

void semcheck_bind_set_literal_to_expected_type_for_call(
    struct Expression *arg_expr, KgpcType *expected_kgpc, int *arg_type) {
  if (arg_expr == NULL || arg_expr->type != EXPR_SET || expected_kgpc == NULL ||
      !kgpc_type_is_set(expected_kgpc))
    return;

  semcheck_expr_set_resolved_kgpc_type_shared(arg_expr, expected_kgpc);
  if (arg_type != NULL)
    *arg_type = SET_TYPE;
}

int semcheck_candidate_source_unit_index(HashNode_t *candidate) {
  if (candidate == NULL)
    return 0;
  if (candidate->source_unit_index > 0)
    return candidate->source_unit_index;
  if (candidate->type != NULL && candidate->type->kind == TYPE_KIND_PROCEDURE &&
      candidate->type->info.proc_info.definition != NULL) {
    int def_idx = candidate->type->info.proc_info.definition->tree_data
                      .subprogram_data.source_unit_index;
    if (def_idx > 0)
      return def_idx;
  }
  return 0;
}

int semcheck_candidate_current_unit_index(SymTab_t *symtab) {
  if (symtab != NULL && symtab->current_scope != NULL &&
      symtab->current_scope->unit_index > 0)
    return symtab->current_scope->unit_index;
  return semcheck_get_current_unit_index();
}

int semcheck_candidate_is_direct_for_current_unit(SymTab_t *symtab,
                                                  HashNode_t *candidate) {
  int source_unit_index = semcheck_candidate_source_unit_index(candidate);
  int current_unit_index;
  int system_unit_index;

  current_unit_index = semcheck_candidate_current_unit_index(symtab);
  if (source_unit_index == 0) {
    if (current_unit_index <= 0 || candidate->defined_in_unit)
      return 1;
    /* Compiler-injected builtins (not from any unit) are always directly
     * accessible.  Only program-local non-unit symbols that aren't nested
     * should be filtered when inside a unit body. */
    if (candidate->hash_type == HASHTYPE_FUNCTION ||
        candidate->hash_type == HASHTYPE_PROCEDURE ||
        candidate->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
      return 1;
    return candidate->is_nested_scope;
  }
  if (current_unit_index <= 0)
    return 1;
  if (source_unit_index == current_unit_index)
    return 1;

  system_unit_index = unit_registry_add("System");
  if (source_unit_index == system_unit_index)
    return 1;

  return unit_registry_is_dep(current_unit_index, source_unit_index);
}

int semcheck_candidate_is_local_callable_for_shadowing(SymTab_t *symtab,
                                                       HashNode_t *candidate) {
  int current_unit_index;
  int candidate_unit_index;

  (void)symtab;
  if (candidate == NULL || candidate->defined_in_unit)
    return 0;
  if (semcheck_candidate_is_builtin(symtab, candidate))
    return 0;
  current_unit_index = semcheck_candidate_current_unit_index(symtab);
  if (current_unit_index > 0) {
    candidate_unit_index = semcheck_candidate_source_unit_index(candidate);
    if (candidate_unit_index != current_unit_index)
      return 0;
  }
  if (candidate->hash_type == HASHTYPE_FUNCTION ||
      candidate->hash_type == HASHTYPE_PROCEDURE)
    return 1;
  if (candidate->hash_type == HASHTYPE_VAR && candidate->type != NULL &&
      candidate->type->kind == TYPE_KIND_PROCEDURE)
    return 1;
  return 0;
}

void semcheck_set_pointer_info_from_kgpc_type(struct Expression *expr,
                                              SymTab_t *symtab,
                                              KgpcType *type) {
  if (expr == NULL || type == NULL || !kgpc_type_is_pointer(type))
    return;

  if (expr->pointer_subtype_id != NULL || expr->pointer_subtype != UNKNOWN_TYPE)
    return;

  int subtype = UNKNOWN_TYPE;
  const char *subtype_id = NULL;

  KgpcType *points_to = type->info.points_to;
  if (points_to != NULL) {
    if (points_to->kind == TYPE_KIND_RECORD) {
      struct RecordType *record = kgpc_type_get_record(points_to);
      if (record != NULL && record->type_id != NULL)
        subtype_id = record->type_id;
      subtype = RECORD_TYPE;
    } else if (points_to->kind == TYPE_KIND_PRIMITIVE) {
      subtype = points_to->info.primitive_type_tag;
    } else if (points_to->kind == TYPE_KIND_POINTER) {
      subtype = POINTER_TYPE;
    }
  }

  if (subtype_id == NULL) {
    struct TypeAlias *alias = kgpc_type_get_type_alias(type);
    if (alias != NULL && alias->is_pointer) {
      if (alias->pointer_type_id != NULL)
        subtype_id = alias->pointer_type_id;
      if (alias->pointer_type != UNKNOWN_TYPE)
        subtype = alias->pointer_type;
      if (subtype == UNKNOWN_TYPE && subtype_id != NULL) {
        struct RecordType *record =
            semcheck_lookup_record_type(symtab, subtype_id);
        if (record != NULL)
          subtype = RECORD_TYPE;
      }
    }
  }

  if (subtype != UNKNOWN_TYPE || subtype_id != NULL)
    semcheck_set_pointer_info(expr, subtype, subtype_id);
}

int semcheck_candidate_is_owner_method(HashNode_t *candidate,
                                       const char *owner_type_id) {
  if (candidate == NULL || owner_type_id == NULL || owner_type_id[0] == '\0')
    return 0;

  if (candidate->mangled_id == NULL)
    return 0;

  size_t owner_len = strlen(owner_type_id);
  return strncmp(candidate->mangled_id, owner_type_id, owner_len) == 0 &&
         candidate->mangled_id[owner_len] == '_' &&
         candidate->mangled_id[owner_len + 1] == '_';
}

ListNode_t *semcheck_find_outer_idents_excluding_owner_methods(
    SymTab_t *symtab, const char *id, const char *owner_type_id) {
  if (symtab == NULL || id == NULL || owner_type_id == NULL ||
      owner_type_id[0] == '\0')
    return NULL;

  for (ScopeNode *scope = symtab->current_scope; scope != NULL;
       scope = scope->parent) {
    HashTable_t *table = scope->table;
    if (table == NULL)
      continue;

    ListNode_t *matches = FindAllIdentsInTable(table, (char *)id);
    if (matches == NULL)
      continue;

    ListNode_t *filtered = NULL;
    ListNode_t *tail = NULL;
    for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
      HashNode_t *candidate = (HashNode_t *)cur->cur;
      if (semcheck_candidate_is_owner_method(candidate, owner_type_id))
        continue;

      ListNode_t *node = CreateListNode(candidate, LIST_UNSPECIFIED);
      if (node == NULL)
        continue;
      if (filtered == NULL)
        filtered = node;
      else
        tail->next = node;
      tail = node;
    }
    DestroyList(matches);

    if (filtered != NULL)
      return filtered;
  }

  ListNode_t *builtin_matches =
      FindAllIdentsInTable(symtab->builtin_scope->table, (char *)id);
  if (builtin_matches == NULL)
    return NULL;

  ListNode_t *filtered_builtins = NULL;
  ListNode_t *tail = NULL;
  for (ListNode_t *cur = builtin_matches; cur != NULL; cur = cur->next) {
    HashNode_t *candidate = (HashNode_t *)cur->cur;
    if (semcheck_candidate_is_owner_method(candidate, owner_type_id))
      continue;

    ListNode_t *node = CreateListNode(candidate, LIST_UNSPECIFIED);
    if (node == NULL)
      continue;
    if (filtered_builtins == NULL)
      filtered_builtins = node;
    else
      tail->next = node;
    tail = node;
  }
  DestroyList(builtin_matches);
  return filtered_builtins;
}

int semcheck_decl_is_untyped_param(Tree_t *decl) {
  if (decl == NULL || decl->type != TREE_VAR_DECL)
    return 0;

  if (decl->tree_data.var_decl_data.is_untyped_param)
    return 1;

  /* Some parsed untyped var/out params are represented as POINTER_TYPE
   * with no explicit type id (e.g. "out Intf"). Treat them as untyped. */
  if (decl->tree_data.var_decl_data.is_var_param &&
      decl->tree_data.var_decl_data.type == POINTER_TYPE &&
      decl->tree_data.var_decl_data.type_id == NULL)
    return 1;

  return 0;
}

int semcheck_call_has_noarg_identifier_calls(ListNode_t *args) {
  for (ListNode_t *cur = args; cur != NULL; cur = cur->next) {
    struct Expression *arg = (struct Expression *)cur->cur;
    if (arg != NULL && arg->type == EXPR_FUNCTION_CALL &&
        arg->expr_data.function_call_data.args_expr == NULL &&
        arg->expr_data.function_call_data.id != NULL) {
      return 1;
    }
  }
  return 0;
}

int semcheck_detach_unary_not_from_receiver(struct Expression *receiver_expr) {
  if (receiver_expr == NULL)
    return 0;

  if (receiver_expr->type == EXPR_RECORD_ACCESS &&
      receiver_expr->expr_data.record_access_data.record_expr != NULL) {
    struct Expression *base =
        receiver_expr->expr_data.record_access_data.record_expr;
    if (base->type == EXPR_RELOP && base->expr_data.relop_data.type == NOT &&
        base->expr_data.relop_data.right == NULL &&
        base->expr_data.relop_data.left != NULL) {
      struct Expression *inner = base->expr_data.relop_data.left;
      base->expr_data.relop_data.left = NULL;
      receiver_expr->expr_data.record_access_data.record_expr = inner;
      destroy_expr(base);
      return 1;
    }
  }

  return 0;
}

int resolve_param_type(Tree_t *decl, SymTab_t *symtab) {
  assert(decl != NULL);
  assert(symtab != NULL);

  int type_tag = UNKNOWN_TYPE;
  char *type_id = NULL;

  if (decl->type == TREE_VAR_DECL) {
    if (semcheck_decl_is_untyped_param(decl))
      return BUILTIN_ANY_TYPE;
    type_tag = decl->tree_data.var_decl_data.type;
    type_id = decl->tree_data.var_decl_data.type_id;
  } else if (decl->type == TREE_ARR_DECL) {
    type_tag = decl->tree_data.arr_decl_data.type;
    type_id = decl->tree_data.arr_decl_data.type_id;
  }

  if (type_id != NULL) {
    if (pascal_identifier_equals(type_id, "ShortString"))
      return SHORTSTRING_TYPE;

    HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_id);
    if (type_node != NULL) {
      int resolved_type = UNKNOWN_TYPE;
      set_type_from_hashtype(&resolved_type, type_node);
      if (resolved_type != UNKNOWN_TYPE)
        return resolved_type;
    }
  }

  if (type_tag != UNKNOWN_TYPE)
    return type_tag;

  return UNKNOWN_TYPE;
}

/** FUNC_CALL **/

int semcheck_funccall(int *type_return, SymTab_t *symtab,
                      struct Expression *expr, int max_scope_lev,
                      int mutating) {
  assert(symtab != NULL);
  assert(expr != NULL);
  assert(expr->type == EXPR_FUNCTION_CALL);

  FunccallCtx ctx = {
      .type_return = type_return,
      .symtab = symtab,
      .expr = expr,
      .max_scope_lev = max_scope_lev,
      .mutating = mutating,
      .return_val = 0,
      .scope_return = 0,
      .final_status = 0,
      .id = expr->expr_data.function_call_data.id,
      .mangled_name = NULL,
      .arg_type = 0,
      .cur_arg = 0,
      .true_args = NULL,
      .true_arg_ids = NULL,
      .args_given = expr->expr_data.function_call_data.args_expr,
      .overload_candidates = NULL,
      .hash_return = NULL,
      .arg_decl = NULL,
      .was_unit_qualified = 0,
      .unit_qualifier_name = NULL,
      .timing_start_ms = 0.0,
      .injected_self = 0,
      .is_operator_dispatch = 0,
      .prefer_non_builtin = 0,
  };

  FunccallState state = FC_NORMALIZE;
  while (state != FC_DONE) {
    switch (state) {
    case FC_NORMALIZE:
      state = funccall_state_normalize(&ctx);
      break;
    case FC_INHERITED:
      state = funccall_state_inherited(&ctx);
      break;
    case FC_INTRINSICS:
      state = funccall_state_intrinsics(&ctx);
      break;
    case FC_EARLY_BUILTINS:
      state = funccall_state_early_builtins(&ctx);
      break;
    case FC_SELF:
      state = funccall_state_self(&ctx);
      break;
    case FC_METHOD:
      state = funccall_state_method(&ctx);
      break;
    case FC_CONSTRUCTOR:
      state = funccall_state_constructor(&ctx);
      break;
    case FC_OVERLOAD_SETUP:
      state = funccall_state_overload_setup(&ctx);
      break;
    case FC_OVERLOAD_RESOLVE:
      state = funccall_state_overload_resolve(&ctx);
      break;
    case FC_OVERLOAD_POST:
      state = funccall_state_overload_post(&ctx);
      break;
    case FC_CLEANUP:
      if (FUNCCALL_TIMINGS_ENABLED()) {
        fprintf(stderr, "[timing] funccall exit id=%s total: %.2f ms\n",
                ctx.id != NULL ? ctx.id : "(null)",
                funccall_now_ms() - ctx.timing_start_ms);
      }
      if (ctx.overload_candidates != NULL)
        DestroyList(ctx.overload_candidates);
      if (ctx.mangled_name != NULL)
        free(ctx.mangled_name);
      if (ctx.unit_qualifier_name != NULL)
        free(ctx.unit_qualifier_name);
      state = FC_DONE;
      break;
    case FC_DONE:
      break;
    }
  }
  return ctx.final_status;
}
