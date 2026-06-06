#include "SemCheck_Expr_Internal.h"
#include "SemCheck_funccall_internal.h"
#include "SemCheck_stmt.h"
#include <ctype.h>
#include <limits.h>
#include <time.h>

static int semcheck_funccall_scope_level_for_candidate(SymTab_t *symtab,
                                                       HashNode_t *candidate) {
  if (symtab == NULL || candidate == NULL || symtab->current_scope == NULL)
    return 0;

  int scope_level = 1;
  for (ScopeNode *scope = symtab->current_scope; scope != NULL;
       scope = scope->parent, ++scope_level) {
    if (scope->table != NULL && FindIdentPtrInTable(scope->table, candidate))
      return scope_level;

    for (int i = 0; i < scope->num_deps; ++i) {
      ScopeNode *dep_scope =
          scope->dep_scopes != NULL ? scope->dep_scopes[i] : NULL;
      if (dep_scope != NULL && dep_scope->table != NULL &&
          FindIdentPtrInTable(dep_scope->table, candidate))
        return scope_level;
    }
  }

  return 0;
}

FunccallState funccall_state_overload_setup(FunccallCtx *ctx) {
  /* If constructor was already resolved above, skip overload resolution */
  if (kgpc_getenv("KGPC_DEBUG_CALL_TYPES") != NULL && ctx->id != NULL &&
      pascal_identifier_equals(ctx->id, "IsDirectory")) {
    fprintf(stderr,
            "[KGPC_DEBUG_CALL_TYPES] IsDirectory resolved_func=%p mangled=%s\n",
            (void *)ctx->expr->expr_data.function_call_data.resolved_func,
            ctx->expr->expr_data.function_call_data.mangled_id != NULL
                ? ctx->expr->expr_data.function_call_data.mangled_id
                : "<null>");
  }
  if (ctx->expr->expr_data.function_call_data.resolved_func != NULL &&
      ctx->expr->expr_data.function_call_data.mangled_id != NULL) {
    /* Constructor already resolved, skip to argument validation */
    ctx->hash_return = ctx->expr->expr_data.function_call_data.resolved_func;
    ctx->scope_return = 1; /* Constructor is in current scope */
    /* Ensure call_kgpc_type is set for code generator calling convention */
    if (ctx->hash_return->type != NULL &&
        !ctx->expr->expr_data.function_call_data.is_call_info_valid) {
      semcheck_expr_set_call_kgpc_type(ctx->expr, ctx->hash_return->type, 0);
      ctx->expr->expr_data.function_call_data.call_hash_type =
          ctx->hash_return->hash_type;
      ctx->expr->expr_data.function_call_data.is_call_info_valid = 1;
    }
    return FC_OVERLOAD_POST;
  }

  if (ctx->overload_candidates == NULL &&
      ctx->expr->expr_data.function_call_data.is_method_call_placeholder &&
      ctx->expr->expr_data.function_call_data.cached_owner_class != NULL &&
      ctx->expr->expr_data.function_call_data.cached_method_name != NULL) {
    const char *owner_class =
        ctx->expr->expr_data.function_call_data.cached_owner_class;
    const char *method_name =
        ctx->expr->expr_data.function_call_data.cached_method_name;
    size_t owner_len = strlen(owner_class);
    size_t method_len = strlen(method_name);
    size_t candidate_len = 0;

    if (!(owner_len > SIZE_MAX - 2 ||
          method_len > SIZE_MAX - owner_len - 2 - 1)) {
      candidate_len = owner_len + 2 + method_len + 1;
      char *candidate_name = (char *)malloc(candidate_len);
      if (candidate_name != NULL) {
        snprintf(candidate_name, candidate_len, "%s__%s", owner_class,
                 method_name);
        ctx->overload_candidates = FindAllIdents(ctx->symtab, candidate_name);
        if (ctx->overload_candidates != NULL) {
          if (ctx->mangled_name != NULL)
            free(ctx->mangled_name);
          ctx->mangled_name = candidate_name;
        } else {
          free(candidate_name);
        }
      }
    }
  }

  if (ctx->id != NULL && ctx->overload_candidates == NULL) {
    ctx->overload_candidates = FindAllIdents(ctx->symtab, ctx->id);
  }
  /* When inside a class method, the early resolution
   * (semcheck_find_class_method) may have correctly resolved an inherited
   * parent-class overload that FindAllIdents(ctx->id) above cannot see — only
   * the child-class forward declaration is in scope. Ensure the early-resolved
   * candidate is in the list so the final overload resolution can consider it.
   * Prepend it to preserve the earlier class-aware priority. */
  if (ctx->expr->expr_data.function_call_data.resolved_func != NULL) {
    HashNode_t *early_resolved =
        ctx->expr->expr_data.function_call_data.resolved_func;
    if (early_resolved->hash_type == HASHTYPE_FUNCTION ||
        early_resolved->hash_type == HASHTYPE_PROCEDURE) {
      if (ctx->overload_candidates != NULL) {
        int already_present = 0;
        for (ListNode_t *c = ctx->overload_candidates; c != NULL; c = c->next) {
          if (c->cur == early_resolved) {
            already_present = 1;
            break;
          }
        }
        if (!already_present) {
          ListNode_t *node = CreateListNode(early_resolved, LIST_UNSPECIFIED);
          if (node != NULL) {
            node->next = ctx->overload_candidates;
            ctx->overload_candidates = node;
          }
        }
      } else {
        ctx->overload_candidates =
            CreateListNode(early_resolved, LIST_UNSPECIFIED);
      }
    }
  }
  if (!ctx->was_unit_qualified && ctx->overload_candidates != NULL) {
    int has_local_candidate = 0;
    int has_direct_candidate = 0;
    int has_indirect_candidate = 0;

    for (ListNode_t *cn = ctx->overload_candidates; cn != NULL; cn = cn->next) {
      HashNode_t *hn = (HashNode_t *)cn->cur;
      if (hn == NULL)
        continue;
      if (semcheck_candidate_is_local_callable_for_shadowing(ctx->symtab, hn)) {
        has_local_candidate = 1;
        continue;
      }
      if (semcheck_candidate_is_builtin(ctx->symtab, hn)) {
        has_direct_candidate = 1;
        continue;
      }
      if (semcheck_candidate_is_direct_for_current_unit(ctx->symtab, hn))
        has_direct_candidate = 1;
      else
        has_indirect_candidate = 1;
    }

    if (has_local_candidate) {
      /* Check whether any local candidate belongs to a unit being compiled
       * (source_unit_index > 0).  When compiling a unit, overloads from
       * directly-used units must remain visible alongside the unit's own
       * overloads (Pascal's overload directive is additive across units).
       * In a program, local functions shadow all imported ones. */
      int local_is_unit_function = 0;
      for (ListNode_t *cn = ctx->overload_candidates; cn != NULL;
           cn = cn->next) {
        HashNode_t *hn = (HashNode_t *)cn->cur;
        if (hn != NULL &&
            semcheck_candidate_is_local_callable_for_shadowing(ctx->symtab,
                                                               hn) &&
            semcheck_candidate_source_unit_index(hn) > 0) {
          local_is_unit_function = 1;
          break;
        }
      }
      ListNode_t *filtered = NULL;
      ListNode_t *tail = NULL;
      for (ListNode_t *cn = ctx->overload_candidates; cn != NULL;
           cn = cn->next) {
        HashNode_t *hn = (HashNode_t *)cn->cur;
        if (hn == NULL)
          continue;
        int keep = 0;
        if (semcheck_candidate_is_local_callable_for_shadowing(ctx->symtab, hn))
          keep = 1;
        else if (local_is_unit_function && hn->defined_in_unit &&
                 (hn->hash_type == HASHTYPE_FUNCTION ||
                  hn->hash_type == HASHTYPE_PROCEDURE) &&
                 semcheck_candidate_is_direct_for_current_unit(ctx->symtab, hn))
          keep = 1;
        if (!keep)
          continue;

        ListNode_t *new_node = CreateListNode(hn, LIST_UNSPECIFIED);
        if (new_node == NULL)
          continue;
        if (filtered == NULL)
          filtered = new_node;
        else
          tail->next = new_node;
        tail = new_node;
      }
      if (filtered != NULL) {
        destroy_list(ctx->overload_candidates);
        ctx->overload_candidates = filtered;
      }
    } else if (has_direct_candidate && has_indirect_candidate) {
      ListNode_t *filtered = NULL;
      ListNode_t *tail = NULL;
      for (ListNode_t *cn = ctx->overload_candidates; cn != NULL;
           cn = cn->next) {
        HashNode_t *hn = (HashNode_t *)cn->cur;
        if (hn == NULL)
          continue;
        if (!semcheck_candidate_is_builtin(ctx->symtab, hn) &&
            !semcheck_candidate_is_direct_for_current_unit(ctx->symtab, hn))
          continue;

        ListNode_t *new_node = CreateListNode(hn, LIST_UNSPECIFIED);
        if (new_node == NULL)
          continue;
        if (filtered == NULL)
          filtered = new_node;
        else
          tail->next = new_node;
        tail = new_node;
      }
      if (filtered != NULL) {
        destroy_list(ctx->overload_candidates);
        ctx->overload_candidates = filtered;
      }
    }
  }
  /* When the call was unit-qualified (e.g. System.Foo), filter candidates to
   * only those belonging to the specified unit.  This prevents a same-named
   * symbol in the current unit from shadowing the intended unit's version.
   * Fall back to unfiltered results if filtering would leave no candidates. */
  if (ctx->was_unit_qualified && ctx->unit_qualifier_name != NULL &&
      ctx->overload_candidates != NULL) {
    ListNode_t *filtered = NULL;
    for (ListNode_t *cn = ctx->overload_candidates; cn != NULL; cn = cn->next) {
      HashNode_t *hn = (HashNode_t *)cn->cur;
      if (hn == NULL)
        continue;
      int match = 0;
      /* Check source_unit_index on the hash node (set for types/vars) */
      if (hn->source_unit_index != 0) {
        const char *src_name = unit_registry_get(hn->source_unit_index);
        if (src_name != NULL &&
            pascal_identifier_equals(src_name, ctx->unit_qualifier_name))
          match = 1;
      }
      /* For function/procedure nodes, check the AST definition's
       * source_unit_index */
      if (!match && hn->type != NULL && hn->type->kind == TYPE_KIND_PROCEDURE &&
          hn->type->info.proc_info.definition != NULL) {
        int def_unit_idx = hn->type->info.proc_info.definition->tree_data
                               .subprogram_data.source_unit_index;
        if (def_unit_idx != 0) {
          const char *src_name = unit_registry_get(def_unit_idx);
          if (src_name != NULL &&
              pascal_identifier_equals(src_name, ctx->unit_qualifier_name))
            match = 1;
        }
      }
      if (match) {
        ListNode_t *new_node = CreateListNode(hn, LIST_UNSPECIFIED);
        if (filtered == NULL)
          filtered = new_node;
        else
          PushListNodeBack(filtered, new_node);
      }
    }
    if (filtered != NULL) {
      destroy_list(ctx->overload_candidates);
      ctx->overload_candidates = filtered;
    }
    free(ctx->unit_qualifier_name);
    ctx->unit_qualifier_name = NULL;
  }
  if (ctx->id != NULL && ctx->args_given != NULL) {
    struct Expression *first_arg = (struct Expression *)ctx->args_given->cur;
    int explicit_self_first =
        (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
         first_arg->expr_data.id != NULL &&
         pascal_identifier_equals(first_arg->expr_data.id, "Self"));

    if (explicit_self_first) {
      const char *current_owner = semcheck_get_current_method_owner();
      struct RecordType *owner_record = NULL;
      if (current_owner != NULL)
        owner_record = semcheck_lookup_record_type(ctx->symtab, current_owner);

      if (owner_record != NULL && owner_record->is_type_helper &&
          owner_record->type_id != NULL) {
        ListNode_t *filtered =
            semcheck_find_outer_idents_excluding_owner_methods(
                ctx->symtab, ctx->id, owner_record->type_id);
        if (filtered != NULL) {
          if (ctx->overload_candidates != NULL)
            destroy_list(ctx->overload_candidates);
          ctx->overload_candidates = filtered;
        }
      }
    }
  }
  if (ctx->overload_candidates == NULL && ctx->id != NULL &&
      ctx->args_given != NULL) {
    struct Expression *first_arg = (struct Expression *)ctx->args_given->cur;
    struct RecordType *helper_record = NULL;
    struct RecordType *actual_method_owner = NULL;
    int helper_tag = UNKNOWN_TYPE;
    const char *helper_name = NULL;
    int arg_type_owned = 0;
    KgpcType *arg_type_local = NULL;

    if (first_arg != NULL) {
      arg_type_local = semcheck_resolve_expression_kgpc_type(
          ctx->symtab, first_arg, ctx->max_scope_lev, NO_MUTATE,
          &arg_type_owned);
      if (arg_type_local != NULL) {
        helper_tag = semcheck_tag_from_kgpc(arg_type_local);
        if (arg_type_local->kind == TYPE_KIND_PRIMITIVE)
          helper_tag = arg_type_local->info.primitive_type_tag;
        if (arg_type_local->kind == TYPE_KIND_RECORD &&
            arg_type_local->info.record_info != NULL &&
            arg_type_local->info.record_info->type_id != NULL)
          helper_name = arg_type_local->info.record_info->type_id;

        struct TypeAlias *alias = kgpc_type_get_type_alias(arg_type_local);
        if (alias != NULL) {
          if (alias->target_type_id != NULL)
            helper_name = alias->target_type_id;
          else if (alias->alias_name != NULL)
            helper_name = alias->alias_name;
        }
      }

      if (arg_type_local != NULL && arg_type_local->kind == TYPE_KIND_RECORD &&
          arg_type_local->info.record_info != NULL) {
        helper_record = semcheck_lookup_type_helper_for_record_member(
            ctx->symtab, arg_type_local->info.record_info, ctx->id);
      }
      if (helper_record == NULL) {
        helper_record = semcheck_lookup_type_helper_for_member(
            ctx->symtab, helper_tag, helper_name, ctx->id);
      }
      if (helper_record == NULL && first_arg->type == EXPR_VAR_ID &&
          first_arg->expr_data.id != NULL) {
        HashNode_t *var_node = NULL;
        if (FindSymbol(&var_node, ctx->symtab, first_arg->expr_data.id) != 0 &&
            var_node != NULL) {
          struct TypeAlias *var_alias = hashnode_get_type_alias(var_node);
          const char *var_helper_name = NULL;
          if (var_alias != NULL) {
            if (var_alias->target_type_id != NULL)
              var_helper_name = var_alias->target_type_id;
            else if (var_alias->alias_name != NULL)
              var_helper_name = var_alias->alias_name;
          }
          if (var_helper_name != NULL)
            helper_record = semcheck_lookup_type_helper_for_member(
                ctx->symtab, UNKNOWN_TYPE, var_helper_name, ctx->id);
        }
      }
    }

    if (arg_type_owned && arg_type_local != NULL)
      destroy_kgpc_type(arg_type_local);

    if (helper_record != NULL) {
      HashNode_t *method_node = semcheck_find_class_method(
          ctx->symtab, helper_record, ctx->id, &actual_method_owner);
      struct RecordType *owner_for_mangle =
          (actual_method_owner != NULL) ? actual_method_owner : helper_record;

      if (method_node != NULL && owner_for_mangle != NULL &&
          owner_for_mangle->type_id != NULL) {
        size_t class_len = strlen(owner_for_mangle->type_id);
        size_t method_len = strlen(ctx->id);
        char *candidate_name = (char *)malloc(class_len + 2 + method_len + 1);
        if (candidate_name != NULL) {
          snprintf(candidate_name, class_len + 2 + method_len + 1, "%s__%s",
                   owner_for_mangle->type_id, ctx->id);
          ListNode_t *candidates = FindAllIdents(ctx->symtab, candidate_name);
          if (candidates != NULL) {
            ctx->overload_candidates = candidates;
            if (ctx->mangled_name != NULL)
              free(ctx->mangled_name);
            ctx->mangled_name = candidate_name;
            return FC_OVERLOAD_RESOLVE;
          }
          free(candidate_name);
        }
      }
    }
  }
  if (kgpc_getenv("KGPC_DEBUG_PROC_VAR") != NULL && ctx->id != NULL &&
      pascal_identifier_equals(ctx->id, "Ctr")) {
    HashNode_t *cand = (ctx->overload_candidates != NULL)
                           ? (HashNode_t *)ctx->overload_candidates->cur
                           : NULL;
    fprintf(stderr,
            "[KGPC_DEBUG_PROC_VAR] call id=%s cand_hash=%d cand_kind=%d "
            "cand_type=%s\n",
            ctx->id, cand ? cand->hash_type : -1,
            cand && cand->type ? cand->type->kind : -1,
            cand && cand->type ? kgpc_type_to_string(cand->type) : "<null>");
  }

  if (ctx->overload_candidates != NULL) {
    ListNode_t *cur = ctx->overload_candidates;
    while (cur != NULL) {
      HashNode_t *candidate = (HashNode_t *)cur->cur;
      if (candidate != NULL && (candidate->hash_type == HASHTYPE_FUNCTION ||
                                candidate->hash_type == HASHTYPE_PROCEDURE)) {
        if (!semcheck_candidate_is_builtin(ctx->symtab, candidate)) {
          ctx->prefer_non_builtin = 1;
          break;
        }
      }
      cur = cur->next;
    }
  }

  /* Check if this is a call through a procedural variable */
  /* If 'id' resolves to a variable with a procedural type, handle it specially
   */
  if (ctx->overload_candidates != NULL &&
      ctx->overload_candidates->cur != NULL) {
    HashNode_t *first_candidate = (HashNode_t *)ctx->overload_candidates->cur;
    if (first_candidate->hash_type == HASHTYPE_VAR &&
        first_candidate->type != NULL &&
        first_candidate->type->kind == TYPE_KIND_PROCEDURE) {
      /* This is a procedural variable - we're calling through a function
       * pointer */
      KgpcType *proc_type = first_candidate->type;
      ListNode_t *formal_params = kgpc_type_get_procedure_params(proc_type);
      KgpcType *return_type = kgpc_type_get_return_type(proc_type);
      if (return_type == NULL &&
          proc_type->info.proc_info.return_type_id != NULL) {
        HashNode_t *ret_node = semcheck_find_preferred_type_node(
            ctx->symtab, proc_type->info.proc_info.return_type_id);
        if (ret_node != NULL && ret_node->type != NULL)
          return_type = ret_node->type;
      }

      /* Validate arguments match the procedural type's signature */
      if (semcheck_count_total_params(formal_params) !=
          ListLength(ctx->args_given)) {
        semcheck_error_with_context_at(
            ctx->expr->line_num, ctx->expr->col_num, ctx->expr->source_index,
            "Error on line %d, call to procedural variable %s: expected %d "
            "arguments, got %d\n",
            ctx->expr->line_num, ctx->id,
            semcheck_count_total_params(formal_params),
            ListLength(ctx->args_given));
        destroy_list(ctx->overload_candidates);
        ctx->overload_candidates = NULL;
        if (ctx->mangled_name != NULL)
          free(ctx->mangled_name);
        ctx->mangled_name = NULL;
        *ctx->type_return = UNKNOWN_TYPE;
        do {
          ctx->final_status = ++ctx->return_val;
          return FC_CLEANUP;
        } while (0);
      }

      /* Check argument types */
      ListNode_t *formal = formal_params;
      ListNode_t *actual = ctx->args_given;
      while (formal != NULL && actual != NULL) {
        Tree_t *formal_decl = (Tree_t *)formal->cur;
        struct Expression *actual_expr = (struct Expression *)actual->cur;

        int formal_type = resolve_param_type(formal_decl, ctx->symtab);
        int actual_type = UNKNOWN_TYPE;
        KgpcType *actual_kgpc_type_proc = NULL;
        semcheck_expr_with_type(&actual_kgpc_type_proc, ctx->symtab,
                                actual_expr, ctx->max_scope_lev, NO_MUTATE);
        actual_type = semcheck_tag_from_kgpc(actual_kgpc_type_proc);

        /* Simple type check - could be more sophisticated */
        if (formal_type != actual_type && formal_type != UNKNOWN_TYPE &&
            actual_type != UNKNOWN_TYPE) {
          /* Allow some type coercions like INT to LONGINT */
          if (!((formal_type == LONGINT_TYPE && actual_type == INT_TYPE) ||
                (formal_type == INT_TYPE && actual_type == LONGINT_TYPE) ||
                (formal_type == POINTER_TYPE) || /* pointers are flexible */
                (actual_type == POINTER_TYPE) ||
                (is_integer_type(formal_type) &&
                 is_integer_type(actual_type)) ||
                (formal_type == REAL_TYPE && is_integer_type(actual_type)) ||
                (is_integer_type(formal_type) && actual_type == REAL_TYPE) ||
                (formal_type == VARIANT_TYPE) ||
                (actual_type == VARIANT_TYPE) ||
                /* Untyped formals (BUILTIN_ANY_TYPE) accept arguments of
                 * any concrete type — this is the var/const/out parameter
                 * pattern FPC's helpers and `array of const` use.  Without
                 * this case the overload resolver reports things like
                 * "got Int64 expected Any" for declarations that pass any
                 * stage of overload-bucket scanning. */
                (formal_type == BUILTIN_ANY_TYPE) ||
                (actual_type == BUILTIN_ANY_TYPE) ||
                (formal_type == RECORD_TYPE) ||
                (actual_type == RECORD_TYPE) ||
                (formal_type == STRING_TYPE && actual_type == CHAR_TYPE) ||
                (formal_type == CHAR_TYPE && actual_type == STRING_TYPE) ||
                (formal_type == SHORTSTRING_TYPE && actual_type == CHAR_TYPE) ||
                (formal_type == STRING_TYPE &&
                 actual_type == SHORTSTRING_TYPE) ||
                (formal_type == SHORTSTRING_TYPE &&
                 actual_type == STRING_TYPE))) {
            semantic_error_at(ctx->expr->line_num, ctx->expr->col_num, -1,
                              "Incompatible types: got \"%s\" expected \"%s\"",
                              type_tag_to_string(actual_type),
                              type_tag_to_string(formal_type));
            destroy_list(ctx->overload_candidates);
            ctx->overload_candidates = NULL;
            if (ctx->mangled_name != NULL)
              free(ctx->mangled_name);
            ctx->mangled_name = NULL;
            *ctx->type_return = UNKNOWN_TYPE;
            do {
              ctx->final_status = ++ctx->return_val;
              return FC_CLEANUP;
            } while (0);
          }
        }

        formal = formal->next;
        actual = actual->next;
      }

      /* Set the return type */
      if (return_type != NULL) {
        if (return_type->kind == TYPE_KIND_PRIMITIVE) {
          *ctx->type_return = kgpc_type_get_primitive_tag(return_type);
        } else if (return_type->kind == TYPE_KIND_RECORD) {
          *ctx->type_return = RECORD_TYPE;
          long long sz = kgpc_type_sizeof(return_type);
          ctx->expr->expr_data.function_call_data.cached_procvar_sret_size =
              (sz > 0) ? sz : 2 * (long long)sizeof(void *);
        } else if (return_type->kind == TYPE_KIND_POINTER) {
          *ctx->type_return = POINTER_TYPE;
        } else {
          *ctx->type_return = UNKNOWN_TYPE;
        }

        semcheck_expr_set_resolved_kgpc_type_shared(ctx->expr, return_type);
        semcheck_expr_set_resolved_type(
            ctx->expr,
            *ctx->type_return); /* Also set resolved_type explicitly */
      } else {
        /* It's a procedure (no return value) */
        *ctx->type_return = PROCEDURE;
      }

      /* Mark this as a procedural variable call */
      ctx->expr->expr_data.function_call_data.is_procedural_var_call = 1;
      ctx->expr->expr_data.function_call_data.procedural_var_symbol =
          first_candidate;

      /* For a method pointer ("procedure of object"), codegen must load the
       * hidden Self/data half of the TMethod from the variable's storage and
       * pass it as the implicit first argument.  It locates that storage via
       * procedural_var_expr.  The plain-variable procvar path never built one,
       * so codegen fell back to a bare indirect call that dropped Self and
       * shifted every argument by one register.  Build an EXPR_VAR_ID for the
       * callee variable so the method-pointer path in codegen can find it.
       * (Plain 8-byte function-pointer procvars keep the symbol path.) */
      if (kgpc_type_is_method_pointer(proc_type) &&
          ctx->expr->expr_data.function_call_data.procedural_var_expr == NULL &&
          ctx->id != NULL) {
        struct Expression *callee_expr =
            mk_varid(ctx->expr->line_num, strdup(ctx->id));
        if (callee_expr != NULL) {
          callee_expr->col_num = ctx->expr->col_num;
          callee_expr->source_index = ctx->expr->source_index;
          semcheck_expr_with_type(NULL, ctx->symtab, callee_expr,
                                  ctx->max_scope_lev, NO_MUTATE);
          ctx->expr->expr_data.function_call_data.procedural_var_expr =
              callee_expr;
          ctx->expr->expr_data.function_call_data.call_kgpc_type = proc_type;
          kgpc_type_retain(proc_type);
          ctx->expr->expr_data.function_call_data.is_call_info_valid = 1;
        }
      }

      destroy_list(ctx->overload_candidates);
      ctx->overload_candidates = NULL;
      if (ctx->mangled_name != NULL)
        free(ctx->mangled_name);
      ctx->mangled_name = NULL;
      do {
        ctx->final_status = ctx->return_val;
        return FC_CLEANUP;
      } while (0);
    }
  }

  if (ctx->id == NULL) {
    /* Default initializers (from Default() lowering) have their id cleared
     * but are already resolved — return their cached type. */
    if (ctx->expr->is_default_initializer) {
      *ctx->type_return = semcheck_tag_from_kgpc(ctx->expr->resolved_kgpc_type);
      if (ctx->overload_candidates != NULL) {
        destroy_list(ctx->overload_candidates);
        ctx->overload_candidates = NULL;
      }
      if (ctx->mangled_name != NULL) {
        free(ctx->mangled_name);
        ctx->mangled_name = NULL;
      }
      do {
        ctx->final_status = 0;
        return FC_CLEANUP;
      } while (0);
    }
    semcheck_error_with_context_at(
        ctx->expr->line_num, ctx->expr->col_num, ctx->expr->source_index,
        "Error on line %d: function call with NULL id\n", ctx->expr->line_num);
    *ctx->type_return = UNKNOWN_TYPE;
    if (ctx->overload_candidates != NULL) {
      destroy_list(ctx->overload_candidates);
      ctx->overload_candidates = NULL;
    }
    do {
      ctx->final_status = ++ctx->return_val;
      return FC_CLEANUP;
    } while (0);
  }
  /* The earlier method-placeholder branch (lines ~70-82) may have stashed a
   * synthesized "Owner__Method" name into ctx->mangled_name; free it before
   * the unconditional reassignment below to avoid orphaning the allocation. */
  if (ctx->mangled_name != NULL) {
    free(ctx->mangled_name);
    ctx->mangled_name = NULL;
  }
  ctx->mangled_name = MangleFunctionNameFromCallSite(
      ctx->id, ctx->args_given, ctx->symtab, ctx->max_scope_lev);
  if (ctx->mangled_name == NULL) {
    fprintf(stderr, "Error: failed to mangle function name for call to %s\n",
            ctx->id);
    *ctx->type_return = UNKNOWN_TYPE;
    destroy_list(ctx->overload_candidates);
    ctx->overload_candidates = NULL;
    do {
      ctx->final_status = ++ctx->return_val;
      return FC_CLEANUP;
    } while (0);
  }

  return FC_OVERLOAD_RESOLVE;
}

FunccallState funccall_state_overload_resolve(FunccallCtx *ctx) {
  ; /* Label for jumping here after method resolution */

  HashNode_t *best_match = NULL;
  int num_best_matches = 0;

  if (ctx->mangled_name != NULL && ctx->overload_candidates != NULL) {
    ListNode_t *cur = ctx->overload_candidates;
    int mangled_matches = 0;
    HashNode_t *first_match = NULL;
    while (cur != NULL) {
      HashNode_t *candidate = (HashNode_t *)cur->cur;
      if (candidate != NULL && candidate->mangled_id != NULL &&
          strcmp(candidate->mangled_id, ctx->mangled_name) == 0) {
        if (first_match == NULL)
          first_match = candidate;
        best_match = candidate;
        num_best_matches = 1;
        mangled_matches++;
      }
      cur = cur->next;
    }
    /* When mangled_matches == 1 but there are other candidates with equivalent
     * signatures, we need to go through full resolution to properly handle
     * declaration order. Check if other candidates share equivalent signature
     * with the match. */
    if (mangled_matches == 1 && first_match != NULL &&
        ListLength(ctx->overload_candidates) > 1) {
      for (ListNode_t *check = ctx->overload_candidates; check != NULL;
           check = check->next) {
        HashNode_t *other = (HashNode_t *)check->cur;
        if (other != first_match && other != NULL &&
            (other->hash_type == HASHTYPE_FUNCTION ||
             other->hash_type == HASHTYPE_PROCEDURE) &&
            semcheck_candidates_share_signature(ctx->symtab, first_match,
                                                other) &&
            !semcheck_candidates_string_subtypes_differ(ctx->symtab,
                                                        first_match, other)) {
          /* There's another candidate with equivalent signature and
           * no string-subtype difference - defer to full resolution */
          best_match = NULL;
          num_best_matches = 0;
          mangled_matches = 0;
          break;
        }
      }
    }
    if (mangled_matches > 1) {
      /* Defer to overload resolution to apply tie-breakers (e.g., alias vs
       * builtin). */
      best_match = NULL;
      num_best_matches = 0;
    }
    if (best_match != NULL &&
        semcheck_call_has_noarg_identifier_calls(ctx->args_given)) {
      /* Calls containing bare no-arg identifier calls may represent type
       * references (e.g. Supports(..., IInterfaceType, ...)); skip mangled
       * fast-path and let full overload resolution classify arguments. */
      best_match = NULL;
      num_best_matches = 0;
    }
  }

  if (best_match == NULL) {
    if (kgpc_getenv("KGPC_DEBUG_CHECKOBS") != NULL && ctx->id != NULL &&
        pascal_identifier_equals(ctx->id, "CheckObserving")) {
      int idx = 0;
      for (ListNode_t *cur = ctx->args_given; cur != NULL;
           cur = cur->next, idx++) {
        struct Expression *arg_expr = (struct Expression *)cur->cur;
        KgpcType *arg_type_local = NULL;
        semcheck_expr_with_type(&arg_type_local, ctx->symtab, arg_expr,
                                ctx->max_scope_lev, NO_MUTATE);
        int arg_tag = semcheck_tag_from_kgpc(arg_type_local);
        fprintf(stderr, "[KGPC_DEBUG_CHECKOBS] arg[%d] type_tag=%d\n", idx,
                arg_tag);
        if (arg_expr != NULL)
          semcheck_debug_expr_brief(arg_expr, "CheckObserving arg");
      }
    }
    double resolve_t0 = 0.0;
    if (FUNCCALL_TIMINGS_ENABLED())
      resolve_t0 = funccall_now_ms();
    int resolve_status = semcheck_resolve_overload(
        &best_match, &num_best_matches, ctx->overload_candidates,
        ctx->args_given, ctx->symtab, ctx->expr, ctx->max_scope_lev,
        ctx->prefer_non_builtin);
    if (FUNCCALL_TIMINGS_ENABLED()) {
      fprintf(stderr, "[timing] funccall resolve_overload id=%s: %.2f ms\n",
              ctx->id != NULL ? ctx->id : "(null)",
              funccall_now_ms() - resolve_t0);
    }
    if (resolve_status == 3) {
      *ctx->type_return = UNKNOWN_TYPE;
      ctx->final_status = ++ctx->return_val;
      return FC_CLEANUP;
    }
  }
  if (best_match != NULL && ctx->overload_candidates != NULL &&
      ctx->overload_candidates->next != NULL) {
    HashNode_t *override_match = NULL;
    int override_count = 0;
    int override_status = semcheck_resolve_overload(
        &override_match, &override_count, ctx->overload_candidates,
        ctx->args_given, ctx->symtab, ctx->expr, ctx->max_scope_lev,
        ctx->prefer_non_builtin);
    if (override_status == 0 && override_match != NULL && override_count == 1 &&
        override_match != best_match) {
      /* Don't override when the candidates differ in string subtypes
       * (e.g. RawByteString vs UnicodeString). The mangled-name shortcut
       * has more specific type information from call-site mangling. */
      if (!semcheck_candidates_string_subtypes_differ(ctx->symtab, best_match,
                                                      override_match)) {
        best_match = override_match;
        num_best_matches = override_count;
      }
    }
  }

  /* Expression callsites must produce a value. If a pure-procedure overload
   * won, rerun resolution using only value-returning candidates. */
  if (best_match != NULL && num_best_matches == 1 && best_match->type != NULL &&
      best_match->type->kind == TYPE_KIND_PROCEDURE) {
    KgpcType *best_ret = kgpc_type_get_return_type(best_match->type);
    if (best_ret == NULL &&
        best_match->type->info.proc_info.return_type_id == NULL) {
      ListNode_t *value_candidates = NULL;
      ListNode_t *value_tail = NULL;
      for (ListNode_t *cur = ctx->overload_candidates; cur != NULL;
           cur = cur->next) {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate == NULL ||
            (candidate->hash_type != HASHTYPE_FUNCTION &&
             candidate->hash_type != HASHTYPE_PROCEDURE) ||
            candidate->type == NULL ||
            candidate->type->kind != TYPE_KIND_PROCEDURE) {
          continue;
        }

        KgpcType *candidate_ret = kgpc_type_get_return_type(candidate->type);
        if (candidate_ret == NULL &&
            candidate->type->info.proc_info.return_type_id == NULL) {
          continue;
        }

        ListNode_t *node = CreateListNode(candidate, LIST_UNSPECIFIED);
        if (node == NULL)
          continue;
        if (value_candidates == NULL) {
          value_candidates = node;
          value_tail = node;
        } else {
          value_tail->next = node;
          value_tail = node;
        }
      }

      if (value_candidates != NULL) {
        HashNode_t *value_match = NULL;
        int value_count = 0;
        int value_status = semcheck_resolve_overload(
            &value_match, &value_count, value_candidates, ctx->args_given,
            ctx->symtab, ctx->expr, ctx->max_scope_lev,
            ctx->prefer_non_builtin);
        if (value_status == 0 && value_match != NULL && value_count == 1) {
          best_match = value_match;
          num_best_matches = value_count;
        }
        DestroyList(value_candidates);
      }
    }
  }

  if (num_best_matches == 1) {
    if (best_match != NULL && best_match->type != NULL &&
        best_match->type->kind == TYPE_KIND_PROCEDURE) {
      if (best_match->type->info.proc_info.return_type == NULL &&
          best_match->type->info.proc_info.return_type_id != NULL) {
        const char *ret_id = best_match->type->info.proc_info.return_type_id;
        HashNode_t *type_node = NULL;
        KgpcType *ret_type = NULL;
        if (FindSymbol(&type_node, ctx->symtab, ret_id) != 0 &&
            type_node != NULL && type_node->type != NULL) {
          kgpc_type_retain(type_node->type);
          ret_type = type_node->type;
        } else {
          int tag = semcheck_map_builtin_type_name(ctx->symtab, ret_id);
          if (tag != UNKNOWN_TYPE)
            ret_type = create_primitive_type(tag);
        }
        if (ret_type != NULL) {
          best_match->type->info.proc_info.return_type = ret_type;
          if (best_match->hash_type == HASHTYPE_PROCEDURE)
            best_match->hash_type = HASHTYPE_FUNCTION;
        }
      }
      if (best_match->type->info.proc_info.return_type == NULL &&
          best_match->owner_class != NULL && best_match->method_name != NULL) {
        HashNode_t *class_node = NULL;
        if (FindSymbol(&class_node, ctx->symtab, best_match->owner_class) !=
                0 &&
            class_node != NULL) {
          struct RecordType *record_info =
              get_record_type_from_node(class_node);
          if (record_info != NULL) {
            struct MethodTemplate *tmpl = from_cparser_get_method_template(
                record_info, best_match->method_name);
            if (tmpl != NULL && tmpl->return_type_ast != NULL &&
                tmpl->return_type_ast->child != NULL) {
              KgpcType *ret_type = convert_type_spec_to_kgpctype(
                  tmpl->return_type_ast->child, ctx->symtab);
              if (ret_type != NULL) {
                best_match->type->info.proc_info.return_type = ret_type;
                if (best_match->hash_type == HASHTYPE_PROCEDURE)
                  best_match->hash_type = HASHTYPE_FUNCTION;
              }
            }
          }
        }
      }

      Tree_t *proc_def = best_match->type->info.proc_info.definition;
      int is_external = 0;
      if (proc_def != NULL) {
        is_external =
            proc_def->tree_data.subprogram_data.cname_flag != 0 ||
            proc_def->tree_data.subprogram_data.cname_override != NULL;
      }
      if (!is_external &&
          (best_match->mangled_id == NULL ||
           (best_match->id != NULL &&
            strcmp(best_match->mangled_id, best_match->id) == 0))) {
        char *computed = MangleFunctionName(
            best_match->id, best_match->type->info.proc_info.params,
            ctx->symtab);
        if (computed != NULL) {
          if (best_match->mangled_id != NULL)
            free(best_match->mangled_id);
          best_match->mangled_id = computed;
        }
      }
    }
    const char *target_name = best_match->mangled_id;
    if (target_name == NULL || target_name[0] == '\0') {
      if (best_match->type != NULL &&
          best_match->type->kind == TYPE_KIND_PROCEDURE) {
        Tree_t *proc_def = best_match->type->info.proc_info.definition;
        if (proc_def != NULL) {
          target_name = proc_def->tree_data.subprogram_data.cname_override;
          if (target_name == NULL)
            target_name = proc_def->tree_data.subprogram_data.id;
        }
      }
      if (target_name == NULL || target_name[0] == '\0')
        target_name = best_match->id;
    }

    char *resolved_name = NULL;
    if (target_name != NULL)
      resolved_name = strdup(target_name);
    if (resolved_name == NULL) {
      fprintf(stderr, "Error: failed to duplicate mangled name for %s\n",
              best_match->id ? best_match->id : "(anonymous)");
      *ctx->type_return = UNKNOWN_TYPE;
      ctx->final_status = ++ctx->return_val;
      return FC_CLEANUP;
    }
    if (ctx->expr->expr_data.function_call_data.mangled_id != NULL)
      free(ctx->expr->expr_data.function_call_data.mangled_id);
    ctx->expr->expr_data.function_call_data.mangled_id = resolved_name;
    if (best_match->type != NULL &&
        best_match->type->kind == TYPE_KIND_PROCEDURE) {
      Tree_t *proc_def = best_match->type->info.proc_info.definition;
      if (proc_def != NULL) {
        bool no_body =
            (proc_def->tree_data.subprogram_data.statement_list == NULL);
        if (no_body) {
          const char *target_name =
              proc_def->tree_data.subprogram_data.cname_override;
          if (target_name == NULL) {
            if (proc_def->tree_data.subprogram_data.cname_flag) {
              target_name = proc_def->tree_data.subprogram_data.id;
            } else if (proc_def->tree_data.subprogram_data.mangled_id != NULL) {
              target_name = proc_def->tree_data.subprogram_data.mangled_id;
            } else if (best_match->mangled_id != NULL) {
              target_name = best_match->mangled_id;
            } else {
              target_name = proc_def->tree_data.subprogram_data.id;
            }
          }
          if (target_name != NULL) {
            free(ctx->expr->expr_data.function_call_data.mangled_id);
            ctx->expr->expr_data.function_call_data.mangled_id =
                strdup(target_name);
          }
        } else if (proc_def->tree_data.subprogram_data.cname_flag &&
                   proc_def->tree_data.subprogram_data.mangled_id != NULL) {
          free(ctx->expr->expr_data.function_call_data.mangled_id);
          ctx->expr->expr_data.function_call_data.mangled_id =
              strdup(proc_def->tree_data.subprogram_data.mangled_id);
        } else if (proc_def->tree_data.subprogram_data.mangled_id != NULL) {
          free(ctx->expr->expr_data.function_call_data.mangled_id);
          ctx->expr->expr_data.function_call_data.mangled_id =
              strdup(proc_def->tree_data.subprogram_data.mangled_id);
        }
      }
    }
    /* Centralized virtual dispatch fallback — catches virtual methods
     * that weren't detected by the early Self-injection check (Path 1).
     * This handles obj.Method() calls that go through placeholder resolution
     * (Path 2 → Path 3) where the receiver is a parameter or field.
     * Applies to all virtual/override methods, not just abstract ones. */
    int best_match_param_count = -1;
    if (best_match->type != NULL &&
        best_match->type->kind == TYPE_KIND_PROCEDURE) {
      best_match_param_count =
          ListLength(best_match->type->info.proc_info.params);
      if (best_match->owner_class != NULL &&
          !from_cparser_is_method_static(best_match->owner_class,
                                         best_match->method_name)) {
        if (best_match_param_count > 0)
          best_match_param_count -= 1;
        else
          best_match_param_count = 0;
      }
    }
    if (best_match->owner_class != NULL && best_match->method_name != NULL &&
        !ctx->expr->expr_data.function_call_data.is_virtual_call &&
        !ctx->expr->expr_data.function_call_data.is_inherited_call &&
        !from_cparser_is_method_static(best_match->owner_class,
                                       best_match->method_name) &&
        from_cparser_is_method_virtual_with_types(
            best_match->owner_class, best_match->method_name,
            best_match_param_count, NULL, 0)) {
      {
        struct RecordType *class_record =
            semcheck_lookup_record_type(ctx->symtab, best_match->owner_class);
        if (class_record != NULL && record_type_is_class(class_record) &&
            class_record->methods != NULL) {
          /* Two-pass match: first prefer exact mangled-name match (handles
           * overloaded constructors like tloopnode.create vs tfornode.create
           * where both have same param_count but different signatures), then
           * fall back to first-by-name+count for backwards compatibility. */
          struct MethodInfo *picked = NULL;
          if (best_match->mangled_id != NULL) {
            for (ListNode_t *me = class_record->methods; me != NULL;
                 me = me->next) {
              struct MethodInfo *mi = (struct MethodInfo *)me->cur;
              if (mi == NULL || mi->name == NULL ||
                  !(mi->is_virtual || mi->is_override) ||
                  strcasecmp(mi->name, best_match->method_name) != 0)
                continue;
              const char *cand_mangled = mi->resolved_mangled_id != NULL
                                             ? mi->resolved_mangled_id
                                             : mi->mangled_name;
              if (cand_mangled != NULL &&
                  strcmp(cand_mangled, best_match->mangled_id) == 0) {
                picked = mi;
                break;
              }
            }
          }
          /* Fallback to first-by-name+count when the exact mangled-name pass
           * found nothing.  This is REQUIRED for ordinary virtual dispatch
           * whenever the resolved overload's mangled id does not literally
           * appear in the VMT method list -- e.g. an abstract base method like
           * tdef.size (virtual;abstract), whose only concrete symbols are the
           * subclass overrides.  Without it the call is emitted as a direct
           * call to a bodyless symbol and the link fails.
           *
           * It MUST be suppressed only for a genuinely OVERLOADED method name.
           * class_record->methods keeps a single entry per name (the
           * VMT-slotted overload), so a non-virtual resolved overload that
           * shares its name with a virtual sibling -- e.g. the non-virtual
           * MemPos_ExeSection(exesec) next to the virtual
           * MemPos_ExeSection(const aname:string) in FPC's TExeOutput -- would
           * otherwise be routed through the sibling's VMT slot, reinterpreting
           * the argument under a mismatched type.  For an overloaded name with
           * no exact mangled match the resolved overload is the non-virtual
           * one, so leaving picked==NULL keeps it a direct call.  Overloadedness
           * is deduped by signature: the same method registered under one key
           * across interface+implementation scopes is not an overload. */
          int name_is_overloaded = QualifiedMethodIsOverloaded(
              ctx->symtab, best_match->owner_class, best_match->method_name);
          if (picked == NULL && !name_is_overloaded) {
            for (ListNode_t *me = class_record->methods; me != NULL;
                 me = me->next) {
              struct MethodInfo *mi = (struct MethodInfo *)me->cur;
              if (mi != NULL && mi->name != NULL &&
                  (mi->is_virtual || mi->is_override) &&
                  strcasecmp(mi->name, best_match->method_name) == 0) {
                if (best_match_param_count >= 0 && mi->param_count >= 0 &&
                    best_match_param_count != mi->param_count)
                  continue;
                picked = mi;
                break;
              }
            }
          }
          if (picked != NULL) {
            ctx->expr->expr_data.function_call_data.is_virtual_call = 1;
            ctx->expr->expr_data.function_call_data.vmt_index =
                picked->vmt_index;
            if (ctx->expr->expr_data.function_call_data.self_class_name == NULL)
              ctx->expr->expr_data.function_call_data.self_class_name =
                  strdup(best_match->owner_class);
            if (ctx->expr->expr_data.function_call_data.cached_owner_class ==
                NULL)
              ctx->expr->expr_data.function_call_data.cached_owner_class =
                  strdup(best_match->owner_class);
            if (ctx->expr->expr_data.function_call_data.cached_method_name ==
                NULL)
              ctx->expr->expr_data.function_call_data.cached_method_name =
                  strdup(best_match->method_name);
          }
        }
      }
    }
    /* Interface method call check — if the owner class is an interface,
     * mark this as an interface call so codegen emits indirect vtable dispatch.
     * The vtable index is the method's position in the interface's
     * method_templates list. Only mark when Self is actually interface-typed to
     * avoid false positives on standalone procedures whose name matches an
     * interface method pattern. */
    if (best_match->owner_class != NULL && best_match->method_name != NULL &&
        !ctx->expr->expr_data.function_call_data.is_interface_call) {
      struct RecordType *iface_record =
          semcheck_lookup_record_type(ctx->symtab, best_match->owner_class);
      if (iface_record != NULL && iface_record->is_interface &&
          iface_record->method_templates != NULL) {
        /* Verify the first argument (Self) is actually interface-typed */
        int self_is_interface = 0;
        ListNode_t *call_args =
            ctx->expr->expr_data.function_call_data.args_expr;
        if (call_args != NULL) {
          struct Expression *self_arg = (struct Expression *)call_args->cur;
          if (self_arg != NULL && self_arg->resolved_kgpc_type != NULL) {
            KgpcType *self_type = self_arg->resolved_kgpc_type;
            if (self_type->kind == TYPE_KIND_POINTER &&
                self_type->info.points_to != NULL)
              self_type = self_type->info.points_to;
            if (self_type->kind == TYPE_KIND_RECORD &&
                self_type->info.record_info != NULL &&
                self_type->info.record_info->is_interface)
              self_is_interface = 1;
          }
        }
        if (self_is_interface) {
          /* method_templates already includes inherited parent
           * methods (prepended during semcheck), so the vtable
           * index is simply the position in the list. */
          int idx = 0;
          for (ListNode_t *mt = iface_record->method_templates; mt != NULL;
               mt = mt->next, idx++) {
            struct MethodTemplate *tmpl = (struct MethodTemplate *)mt->cur;
            if (tmpl != NULL && tmpl->name != NULL &&
                strcasecmp(tmpl->name, best_match->method_name) == 0) {
              ctx->expr->expr_data.function_call_data.is_interface_call = 1;
              ctx->expr->expr_data.function_call_data.vmt_index = idx;
              if (ctx->expr->expr_data.function_call_data.self_class_name ==
                  NULL)
                ctx->expr->expr_data.function_call_data.self_class_name =
                    strdup(best_match->owner_class);
              break;
            }
          }
        }
      }
    }
    /* Centralized class method flag — mark calls to class functions/procedures
     * so codegen passes VMT (not instance) as Self.  Walk the parent chain
     * because the method may be inherited (e.g., TA.ClassName → TObject).
     * We check whether the resolved function's first parameter is named Self;
     * this distinguishes the class method overload from static/instance
     * overloads when multiple overloads share the same name. */
    if (!ctx->expr->expr_data.function_call_data.is_class_method_call) {
      /* Only check methods that have a Self parameter — this excludes static
       * class methods (which have no Self) and non-class functions. */
      int has_self_param = 0;
      if (best_match->type != NULL &&
          best_match->type->kind == TYPE_KIND_PROCEDURE) {
        ListNode_t *params = best_match->type->info.proc_info.params;
        if (params != NULL) {
          Tree_t *first_param = (Tree_t *)params->cur;
          if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
              first_param->tree_data.var_decl_data.ids != NULL) {
            const char *first_id =
                (const char *)first_param->tree_data.var_decl_data.ids->cur;
            if (first_id != NULL && pascal_identifier_equals(first_id, "Self"))
              has_self_param = 1;
          }
        }
      }
      if (has_self_param) {
        const char *cm_class = best_match->owner_class;
        const char *cm_method = best_match->method_name;
        if (cm_class != NULL && cm_method != NULL) {
          struct RecordType *check_record =
              semcheck_lookup_record_type(ctx->symtab, cm_class);
          const char *check_class = cm_class;
          while (check_class != NULL) {
            if (from_cparser_is_method_nonstatic_class_method(check_class,
                                                              cm_method)) {
              ctx->expr->expr_data.function_call_data.is_class_method_call = 1;
              break;
            }
            const char *parent =
                (check_record != NULL) ? check_record->parent_class_name : NULL;
            if (parent == NULL)
              break;
            check_record = semcheck_lookup_record_type(ctx->symtab, parent);
            check_class = parent;
          }
        }
      }
    }
    semcheck_set_function_call_target(ctx->expr, best_match);
    if (ctx->expr->expr_data.function_call_data.is_virtual_call) {
      KGPC_SEMCHECK_HARD_ASSERT(
          ctx->expr->expr_data.function_call_data.cached_owner_class != NULL &&
              ctx->expr->expr_data.function_call_data.cached_owner_class[0] !=
                  '\0',
          "virtual expression call '%s' resolved without owner metadata",
          ctx->id != NULL ? ctx->id : "(unknown)");
      KGPC_SEMCHECK_HARD_ASSERT(
          ctx->expr->expr_data.function_call_data.cached_method_name != NULL &&
              ctx->expr->expr_data.function_call_data.cached_method_name[0] !=
                  '\0',
          "virtual expression call '%s' resolved without method metadata",
          ctx->id != NULL ? ctx->id : "(unknown)");
    }
    semcheck_sync_function_call_target_to_mangled(ctx->expr, ctx->symtab);
    semcheck_mark_call_requires_static_link(best_match);
    ctx->hash_return = best_match;
    ctx->scope_return =
        semcheck_funccall_scope_level_for_candidate(ctx->symtab, best_match);
  } else if (num_best_matches == 0) {
    if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "AllocMem") &&
        !ctx->expr->expr_data.function_call_data.is_method_call_placeholder &&
        ctx->expr->expr_data.function_call_data.call_qualifier == NULL)
      do {
        ctx->final_status = semcheck_builtin_allocmem(
            ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev);
        return FC_CLEANUP;
      } while (0);

    if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "New") &&
        !ctx->expr->expr_data.function_call_data.is_method_call_placeholder &&
        ctx->expr->expr_data.function_call_data.call_qualifier == NULL)
      do {
        ctx->final_status = semcheck_builtin_new_func(
            ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev);
        return FC_CLEANUP;
      } while (0);

    /* WITH context fallback: if the function call couldn't be resolved in
     * normal scope, try resolving via active WITH contexts.  This handles
     * patterns like:  with SomeList.LockList do Add(Self);
     * where Add is a method of the WITH target's class.
     * Also handles procedural fields: with ctx^ do Result := CompareFn(args) */
    if (ctx->id != NULL && with_context_count > 0 &&
        !ctx->expr->expr_data.function_call_data.is_method_call_placeholder) {
      struct Expression *with_expr = NULL;
      int wm = semcheck_with_try_resolve_method(
          ctx->id, ctx->symtab, &with_expr, ctx->expr->line_num);
      if ((wm == 0 || wm == 2) && with_expr != NULL) {
        if (wm == 2) {
          /* Procedural field: transform into record access + indirect call.
           * Convert: CompareFn(args) -> (with_expr.CompareFn)(args)
           * Create an EXPR_RECORD_ACCESS for the field, evaluate it to get
           * the procedural type, then set up as a procedural variable call. */
          struct Expression *field_access =
              (struct Expression *)calloc(1, sizeof(struct Expression));
          if (field_access != NULL) {
            field_access->type = EXPR_RECORD_ACCESS;
            field_access->line_num = ctx->expr->line_num;
            field_access->col_num = ctx->expr->col_num;
            field_access->expr_data.record_access_data.record_expr = with_expr;
            field_access->expr_data.record_access_data.field_id =
                strdup(ctx->id);
            field_access->expr_data.record_access_data.field_offset = 0;

            /* Evaluate the record access to resolve the procedural type */
            KgpcType *field_kgpc_type = NULL;
            semcheck_expr_with_type(&field_kgpc_type, ctx->symtab, field_access,
                                    ctx->max_scope_lev, NO_MUTATE);

            KgpcType *proc_kgpc_type = field_access->resolved_kgpc_type;
            if (proc_kgpc_type != NULL &&
                proc_kgpc_type->kind == TYPE_KIND_PROCEDURE) {
              /* Set return type from the procedural type */
              KgpcType *ret = proc_kgpc_type->info.proc_info.return_type;
              if (ret == NULL &&
                  proc_kgpc_type->info.proc_info.return_type_id != NULL) {
                HashNode_t *ret_node = semcheck_find_preferred_type_node(
                    ctx->symtab, proc_kgpc_type->info.proc_info.return_type_id);
                if (ret_node != NULL && ret_node->type != NULL)
                  ret = ret_node->type;
              }
              if (ret != NULL) {
                *ctx->type_return = semcheck_tag_from_kgpc(ret);
                semcheck_expr_set_resolved_kgpc_type_shared(ctx->expr, ret);
                if (ret->kind == TYPE_KIND_RECORD) {
                  long long sz = kgpc_type_sizeof(ret);
                  ctx->expr->expr_data.function_call_data
                      .cached_procvar_sret_size =
                      (sz > 0) ? sz : 2 * (long long)sizeof(void *);
                }
              }

              /* Convert to procedural variable call */
              ctx->expr->expr_data.function_call_data.is_procedural_var_call =
                  1;
              ctx->expr->expr_data.function_call_data.procedural_var_symbol =
                  NULL;
              ctx->expr->expr_data.function_call_data.procedural_var_expr =
                  field_access;
              ctx->expr->expr_data.function_call_data
                  .is_method_call_placeholder = 0;
              ctx->expr->expr_data.function_call_data.call_kgpc_type =
                  proc_kgpc_type;
              kgpc_type_retain(proc_kgpc_type);
              ctx->expr->expr_data.function_call_data.is_call_info_valid = 1;

              /* Type-check arguments */
              for (ListNode_t *arg_cur =
                       ctx->expr->expr_data.function_call_data.args_expr;
                   arg_cur != NULL; arg_cur = arg_cur->next) {
                struct Expression *arg = (struct Expression *)arg_cur->cur;
                if (arg != NULL)
                  semcheck_expr_with_type(NULL, ctx->symtab, arg,
                                          ctx->max_scope_lev, NO_MUTATE);
              }

              DestroyList(ctx->overload_candidates);
              ctx->overload_candidates = NULL;
              free(ctx->mangled_name);
              ctx->mangled_name = NULL;
              do {
                ctx->final_status = ctx->return_val;
                return FC_CLEANUP;
              } while (0);
            } else {
              destroy_expr(field_access);
            }
          } else {
            destroy_expr(with_expr);
          }
        } else {
          /* Class method: prepend the WITH context expression as Self argument
           */
          ListNode_t *self_node = CreateListNode(with_expr, LIST_EXPR);
          if (self_node != NULL) {
            self_node->next = ctx->expr->expr_data.function_call_data.args_expr;
            ctx->expr->expr_data.function_call_data.args_expr = self_node;
            ctx->expr->expr_data.function_call_data.is_method_call_placeholder =
                1;
            if (ctx->expr->expr_data.function_call_data
                    .placeholder_method_name != NULL)
              free(ctx->expr->expr_data.function_call_data
                       .placeholder_method_name);
            ctx->expr->expr_data.function_call_data.placeholder_method_name =
                (ctx->id != NULL) ? strdup(ctx->id) : NULL;
            /* Free overload list before retry */
            DestroyList(ctx->overload_candidates);
            ctx->overload_candidates = NULL;
            free(ctx->mangled_name);
            ctx->mangled_name = NULL;
            /* Re-evaluate as a method call from scratch */
            int retry_result =
                semcheck_funccall(ctx->type_return, ctx->symtab, ctx->expr,
                                  ctx->max_scope_lev, ctx->mutating);
            do {
              ctx->final_status = retry_result;
              return FC_CLEANUP;
            } while (0);
          } else {
            destroy_expr(with_expr);
          }
        }
      }
    }

    /* Build detailed error message with argument types and available overloads
     */
    {
      if (kgpc_getenv("KGPC_DEBUG_OVERLOAD_FAIL") != NULL) {
        fprintf(stderr,
                "[KGPC_DEBUG_OVERLOAD_FAIL] call=%s line=%d col=%d args=%d\n",
                ctx->id != NULL ? ctx->id : "<null>", ctx->expr->line_num,
                ctx->expr->col_num,
                ctx->args_given != NULL ? ListLength(ctx->args_given) : 0);
        int arg_idx = 0;
        for (ListNode_t *cur = ctx->args_given; cur != NULL; cur = cur->next) {
          struct Expression *arg = (struct Expression *)cur->cur;
          KgpcType *arg_kgpc_type_dbg = NULL;
          semcheck_expr_with_type(&arg_kgpc_type_dbg, ctx->symtab, arg,
                                  ctx->max_scope_lev, NO_MUTATE);
          const char *kgpc_str =
              (arg != NULL && arg->resolved_kgpc_type != NULL)
                  ? kgpc_type_to_string(arg->resolved_kgpc_type)
                  : (arg_kgpc_type_dbg != NULL
                         ? kgpc_type_to_string(arg_kgpc_type_dbg)
                         : "<null>");
          const char *arg_id = NULL;
          if (arg != NULL && arg->type == EXPR_VAR_ID)
            arg_id = arg->expr_data.id;
          fprintf(stderr,
                  "  arg%d expr_type=%d line=%d col=%d src=%d id=%s kgpc=%s "
                  "tag=%d ptr_sub=%d ptr_id=%s array_elem=%d array_id=%s\n",
                  arg_idx, arg != NULL ? arg->type : -1,
                  arg != NULL ? arg->line_num : -1,
                  arg != NULL ? arg->col_num : -1,
                  arg != NULL ? arg->source_index : -1,
                  arg_id != NULL ? arg_id : "<null>",
                  kgpc_str != NULL ? kgpc_str : "<null>",
                  arg != NULL && arg->resolved_kgpc_type != NULL
                      ? semcheck_tag_from_kgpc(arg->resolved_kgpc_type)
                      : semcheck_tag_from_kgpc(arg_kgpc_type_dbg),
                  arg != NULL ? arg->pointer_subtype : -1,
                  arg != NULL && arg->pointer_subtype_id != NULL
                      ? arg->pointer_subtype_id
                      : "<null>",
                  arg != NULL ? arg->array_element_type : -1,
                  arg != NULL && arg->array_element_type_id != NULL
                      ? arg->array_element_type_id
                      : "<null>");
          arg_idx++;
        }
      }

      /* First, build a string showing the actual argument types */
      char arg_types_buf[1024] = "(";
      int buf_pos = 1;
      int any_arg_unknown = 0;
      if (ctx->args_given != NULL) {
        int idx = 0;
        for (ListNode_t *cur = ctx->args_given; cur != NULL; cur = cur->next) {
          struct Expression *arg = (struct Expression *)cur->cur;
          int tag = UNKNOWN_TYPE;
          KgpcType *arg_kgpc_type_dbg = NULL;
          semcheck_expr_with_type(&arg_kgpc_type_dbg, ctx->symtab, arg,
                                  ctx->max_scope_lev, NO_MUTATE);
          tag = semcheck_tag_from_kgpc(arg_kgpc_type_dbg);
          if (tag == UNKNOWN_TYPE)
            any_arg_unknown = 1;
          const char *type_name = semcheck_type_tag_name(tag);

          /* Also check for resolved_kgpc_type for better type info */
          if (arg != NULL && arg->resolved_kgpc_type != NULL) {
            const char *kgpc_str = kgpc_type_to_string(arg->resolved_kgpc_type);
            if (kgpc_str != NULL && kgpc_str[0] != '\0')
              type_name = kgpc_str;
          }

          if (idx > 0 && buf_pos < (int)sizeof(arg_types_buf) - 3) {
            arg_types_buf[buf_pos++] = ',';
            arg_types_buf[buf_pos++] = ' ';
          }
          int remaining = (int)sizeof(arg_types_buf) - buf_pos - 1;
          if (remaining > 0) {
            int written =
                snprintf(arg_types_buf + buf_pos, remaining, "%s", type_name);
            if (written > 0)
              buf_pos += (written < remaining) ? written : remaining - 1;
          }
          idx++;
        }
      }
      if (buf_pos < (int)sizeof(arg_types_buf) - 1)
        arg_types_buf[buf_pos++] = ')';
      arg_types_buf[buf_pos] = '\0';

      /* Now build a string showing available overloads */
      char overloads_buf[2048] = "";
      int ovl_pos = 0;
      if (ctx->overload_candidates != NULL) {
        for (ListNode_t *cur = ctx->overload_candidates; cur != NULL;
             cur = cur->next) {
          HashNode_t *cand = (HashNode_t *)cur->cur;
          if (cand != NULL && cand->type != NULL &&
              (cand->hash_type == HASHTYPE_FUNCTION ||
               cand->hash_type == HASHTYPE_PROCEDURE)) {
            int remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
            if (remaining <= 0)
              break;

            /* Format: "  - function_name(param_types): return_type" */
            int written =
                snprintf(overloads_buf + ovl_pos, remaining, "  - %s(",
                         cand->id ? cand->id : "<anonymous>");
            if (written > 0)
              ovl_pos += (written < remaining) ? written : remaining - 1;

            /* Add parameter types */
            ListNode_t *params = kgpc_type_get_procedure_params(cand->type);
            int param_idx = 0;
            for (ListNode_t *p = params; p != NULL; p = p->next) {
              Tree_t *param = (Tree_t *)p->cur;
              if (param != NULL) {
                remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                if (remaining <= 0)
                  break;

                if (param_idx > 0) {
                  written = snprintf(overloads_buf + ovl_pos, remaining, ", ");
                  if (written > 0)
                    ovl_pos += (written < remaining) ? written : remaining - 1;
                  remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                }

                const char *param_type_str = "?";
                if (param->tree_data.var_decl_data.cached_kgpc_type != NULL)
                  param_type_str = kgpc_type_to_string(
                      param->tree_data.var_decl_data.cached_kgpc_type);
                else if (param->tree_data.var_decl_data.type_id != NULL)
                  param_type_str = param->tree_data.var_decl_data.type_id;
                else if (param->tree_data.var_decl_data.type != UNKNOWN_TYPE)
                  param_type_str = semcheck_type_tag_name(
                      param->tree_data.var_decl_data.type);

                written = snprintf(overloads_buf + ovl_pos, remaining, "%s",
                                   param_type_str);
                if (written > 0)
                  ovl_pos += (written < remaining) ? written : remaining - 1;
                param_idx++;
              }
            }

            remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
            if (remaining > 0) {
              /* Add return type for functions */
              KgpcType *ret_type = kgpc_type_get_return_type(cand->type);
              if (ret_type != NULL) {
                written = snprintf(overloads_buf + ovl_pos, remaining,
                                   "): %s\n", kgpc_type_to_string(ret_type));
              } else {
                written = snprintf(overloads_buf + ovl_pos, remaining, ")\n");
              }
              if (written > 0)
                ovl_pos += (written < remaining) ? written : remaining - 1;
            }
          }
        }
      }

      /* Suppress error when any argument has UNKNOWN_TYPE — the root cause
       * error was already reported upstream. */
      if (!any_arg_unknown) {
        if (overloads_buf[0] != '\0') {
          semcheck_error_with_context_at(
              ctx->expr->line_num, ctx->expr->col_num, ctx->expr->source_index,
              "Error on line %d, call to function %s%s does not match any "
              "available overload.\n"
              "Available overloads:\n%s",
              ctx->expr->line_num, ctx->id, arg_types_buf, overloads_buf);
        } else {
          /* No overloads found - function is not declared */
          semcheck_error_with_context_at(
              ctx->expr->line_num, ctx->expr->col_num, ctx->expr->source_index,
              "Error on line %d, function %s%s is not declared.\n",
              ctx->expr->line_num, ctx->id, arg_types_buf);
        }
        ++ctx->return_val;
      }
    }
    *ctx->type_return = UNKNOWN_TYPE;
    ctx->final_status = ctx->return_val;
    return FC_CLEANUP;
  } else {
    const char *ambig_env = kgpc_getenv("KGPC_DEBUG_AMBIGUOUS");
    if (ambig_env != NULL && ctx->overload_candidates != NULL) {
      fprintf(stderr, "[KGPC] Ambiguous call to %s, %d best matches:\n",
              ctx->id ? ctx->id : "<unknown>", num_best_matches);
      ListNode_t *cur = ctx->overload_candidates;
      while (cur != NULL) {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate != NULL && candidate->type != NULL &&
            (candidate->hash_type == HASHTYPE_FUNCTION ||
             candidate->hash_type == HASHTYPE_PROCEDURE)) {
          ListNode_t *candidate_args =
              kgpc_type_get_procedure_params(candidate->type);
          fprintf(stderr, "  - %s mangled=%s args=%d kind=%d\n",
                  candidate->id ? candidate->id : "<anon>",
                  candidate->mangled_id ? candidate->mangled_id : "<null>",
                  ListLength(candidate_args), candidate->type->kind);
        }
        cur = cur->next;
      }
    }
    semcheck_error_with_context_at(
        ctx->expr->line_num, ctx->expr->col_num, ctx->expr->source_index,
        "Error on line %d, call to function %s is ambiguous\n",
        ctx->expr->line_num, ctx->id);
    *ctx->type_return = UNKNOWN_TYPE;
    ctx->final_status = ++ctx->return_val;
    return FC_CLEANUP;
  }

  return FC_OVERLOAD_POST;
}

FunccallState funccall_state_overload_post(FunccallCtx *ctx) {
  /* Overload resolution completed or skipped for constructors */

  if (!ctx->scope_return) // Should not happen if match_count > 0
  {
    semcheck_error_with_context_at(
        ctx->expr->line_num, ctx->expr->col_num, ctx->expr->source_index,
        "Error on line %d, undeclared function %s (mangled to %s)!\n\n",
        ctx->expr->line_num, ctx->id, ctx->mangled_name);
    ++ctx->return_val;

    *ctx->type_return = UNKNOWN_TYPE;
  } else {
    set_hash_meta(ctx->hash_return, ctx->mutating);
    if (ctx->hash_return->owner_class != NULL &&
        ctx->expr->expr_data.function_call_data.cached_owner_class == NULL) {
      ctx->expr->expr_data.function_call_data.cached_owner_class =
          strdup(ctx->hash_return->owner_class);
    }
    if (ctx->hash_return->method_name != NULL &&
        ctx->expr->expr_data.function_call_data.cached_method_name == NULL) {
      ctx->expr->expr_data.function_call_data.cached_method_name =
          strdup(ctx->hash_return->method_name);
    }
    if (0) /* scope depth check removed — tree scoping has no depth */
    {
      if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr,
                "[SemCheck] semcheck_funccall: scope_return (%d) > "
                "max_scope_lev (%d)\n",
                ctx->scope_return, ctx->max_scope_lev);
      }
      semcheck_error_with_context_at(
          ctx->expr->line_num, ctx->expr->col_num, ctx->expr->source_index,
          "Error on line %d, cannot change \"%s\", invalid scope!\n\n",
          ctx->expr->line_num, ctx->id);
      fprintf(stderr, "[Was it defined above a function declaration?]\n\n");
      ++ctx->return_val;
    }
    if (ctx->hash_return->hash_type != HASHTYPE_FUNCTION &&
        ctx->hash_return->hash_type != HASHTYPE_FUNCTION_RETURN &&
        ctx->hash_return->hash_type != HASHTYPE_PROCEDURE) {
      semcheck_error_with_context_at(
          ctx->expr->line_num, ctx->expr->col_num, ctx->expr->source_index,
          "Error on line %d, \"%s\" is not a function!\n\n",
          ctx->expr->line_num, ctx->id);
      ++ctx->return_val;
    }

    set_type_from_hashtype(ctx->type_return, ctx->hash_return);
    if (ctx->expr->resolved_kgpc_type == NULL &&
        ctx->hash_return->method_name != NULL &&
        strncasecmp(ctx->hash_return->method_name, "Create", 6) == 0 &&
        ctx->hash_return->owner_class != NULL) {
      /* Only force the return type to the owner class if the method was
       * actually declared as a constructor (METHOD_TEMPLATE_CONSTRUCTOR).
       * Functions like CreateDriver that merely start with "Create" must
       * use their declared return type. */
      int is_declared_ctor = 0;
      struct RecordType *ctor_owner = semcheck_lookup_record_type(
          ctx->symtab, ctx->hash_return->owner_class);
      if (ctor_owner != NULL) {
        struct RecordType *search = ctor_owner;
        while (search != NULL) {
          struct MethodTemplate *tmpl = from_cparser_get_method_template(
              search, ctx->hash_return->method_name);
          if (tmpl != NULL) {
            if (tmpl->kind == METHOD_TEMPLATE_CONSTRUCTOR)
              is_declared_ctor = 1;
            break;
          }
          search = semcheck_lookup_parent_record(ctx->symtab, search);
        }
      }
      if (is_declared_ctor && ctor_owner != NULL &&
          record_type_is_class(ctor_owner) && !ctor_owner->is_type_helper) {
        KgpcType *record_kgpc = create_record_type(ctor_owner);
        if (record_kgpc != NULL) {
          KgpcType *ptr_type = create_pointer_type(record_kgpc);
          kgpc_type_release(record_kgpc);
          if (ptr_type != NULL) {
            semcheck_expr_set_resolved_kgpc_type_shared(ctx->expr, ptr_type);
            destroy_kgpc_type(ptr_type);
          }
        }
      }
    }
    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && ctx->id != NULL &&
        (pascal_identifier_equals(ctx->id, "GetAnsiString") ||
         pascal_identifier_equals(ctx->id, "GetString"))) {
      KgpcType *ret_dbg = NULL;
      const char *ret_id_dbg = "<null>";
      if (ctx->hash_return->type != NULL &&
          ctx->hash_return->type->kind == TYPE_KIND_PROCEDURE) {
        ret_dbg = ctx->hash_return->type->info.proc_info.return_type;
        if (ctx->hash_return->type->info.proc_info.return_type_id != NULL)
          ret_id_dbg = ctx->hash_return->type->info.proc_info.return_type_id;
      }
      fprintf(stderr, "[SemCheck] call %s return_type=%p return_type_id=%s\n",
              ctx->id, (void *)ret_dbg, ret_id_dbg);
    }

    /* If a procedure type lacks a resolved return type but has a
     * return_type_id, resolve it now so calls like GetAnsiString() return a
     * value. */
    if (*ctx->type_return == PROCEDURE && ctx->hash_return->type != NULL &&
        ctx->hash_return->type->kind == TYPE_KIND_PROCEDURE &&
        ctx->hash_return->type->info.proc_info.return_type == NULL &&
        ctx->hash_return->type->info.proc_info.return_type_id != NULL) {
      const char *ret_id =
          ctx->hash_return->type->info.proc_info.return_type_id;
      KgpcType *ret_type = NULL;
      HashNode_t *ret_node =
          semcheck_find_type_node_with_kgpc_type(ctx->symtab, ret_id);
      if (ret_node != NULL && ret_node->type != NULL) {
        ret_type = ret_node->type;
        kgpc_type_retain(ret_type);
      } else {
        int mapped = semcheck_map_builtin_type_name(ctx->symtab, ret_id);
        if (mapped != UNKNOWN_TYPE) {
          ret_type = create_primitive_type(mapped);
          /* Preserve type_alias for RawByteString/UnicodeString so
           * overload resolution can distinguish them. */
          if (ret_type != NULL && (strcasecmp(ret_id, "RawByteString") == 0 ||
                                   strcasecmp(ret_id, "UnicodeString") == 0)) {
            struct TypeAlias *alias =
                (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
            if (alias != NULL) {
              alias->alias_name = strdup(ret_id);
              alias->base_type = mapped;
              kgpc_type_set_type_alias(ret_type, alias);
              free(alias->alias_name);
              free(alias);
            }
          }
        }
      }

      if (ret_type != NULL) {
        ctx->hash_return->type->info.proc_info.return_type = ret_type;
        if (ret_type->kind == TYPE_KIND_PRIMITIVE)
          *ctx->type_return = ret_type->info.primitive_type_tag;
        else if (ret_type->kind == TYPE_KIND_POINTER)
          *ctx->type_return = POINTER_TYPE;
        else if (ret_type->kind == TYPE_KIND_RECORD)
          *ctx->type_return = RECORD_TYPE;
        else
          *ctx->type_return = UNKNOWN_TYPE;
      }
    }

    /* NEW: Also set the resolved KgpcType for this expression */
    if (ctx->hash_return->type != NULL &&
        ctx->hash_return->type->kind == TYPE_KIND_PROCEDURE) {
      KgpcType *return_type = kgpc_type_get_return_type(ctx->hash_return->type);
      /* Ensure type_alias is preserved for RawByteString/UnicodeString return
       * types so overload resolution at call sites can distinguish them. */
      if (return_type != NULL && return_type->kind == TYPE_KIND_PRIMITIVE &&
          return_type->info.primitive_type_tag == STRING_TYPE &&
          return_type->type_alias == NULL &&
          ctx->hash_return->type->info.proc_info.return_type_id != NULL) {
        const char *ret_id =
            ctx->hash_return->type->info.proc_info.return_type_id;
        if (strcasecmp(ret_id, "RawByteString") == 0 ||
            strcasecmp(ret_id, "UnicodeString") == 0) {
          struct TypeAlias *alias =
              (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
          if (alias != NULL) {
            alias->alias_name = strdup(ret_id);
            alias->base_type = STRING_TYPE;
            kgpc_type_set_type_alias(return_type, alias);
            free(alias->alias_name);
            free(alias);
          }
        }
      }
      if (return_type != NULL) {
        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
          fprintf(stderr, "[SemCheck] Overwriting resolved_kgpc_type from "
                          "hash_return return_type\n");
          fprintf(stderr, "[SemCheck]   return_type kind=%d\n",
                  return_type->kind);
        }
        int skip_override_for_ctor = 0;
        {
          const char *ctor_method = NULL;
          if (ctx->expr->expr_data.function_call_data.placeholder_method_name !=
              NULL)
            ctor_method =
                ctx->expr->expr_data.function_call_data.placeholder_method_name;
          else if (ctx->hash_return != NULL &&
                   ctx->hash_return->method_name != NULL)
            ctor_method = ctx->hash_return->method_name;
          else
            ctor_method = ctx->expr->expr_data.function_call_data.id;
          if (ctor_method != NULL &&
              strncasecmp(ctor_method, "Create", 6) == 0) {
            /* Only skip override if the method is a declared constructor,
             * not a regular function that starts with "Create". */
            int is_real_ctor =
                ctx->expr->expr_data.function_call_data.is_constructor_call;
            if (is_real_ctor && ctx->expr->resolved_kgpc_type != NULL &&
                ctx->expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER) {
              skip_override_for_ctor = 1;
            }
          }
        }
        if (!skip_override_for_ctor)
          semcheck_expr_set_resolved_kgpc_type_shared(ctx->expr, return_type);
        semcheck_set_pointer_info_from_kgpc_type(ctx->expr, ctx->symtab,
                                                 return_type);
        if (return_type->kind == TYPE_KIND_ARRAY)
          semcheck_set_array_info_from_kgpctype(
              ctx->expr, ctx->symtab, return_type, ctx->expr->line_num);
        else
          semcheck_clear_array_info(ctx->expr);
        if (return_type->kind == TYPE_KIND_PRIMITIVE &&
            return_type->info.primitive_type_tag == UNKNOWN_TYPE) {
          char *target_return_id =
              ctx->hash_return->type->info.proc_info.return_type_id;
          if (target_return_id != NULL) {
            HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type(
                ctx->symtab, target_return_id);
            if (type_node != NULL && type_node->type != NULL) {
              destroy_kgpc_type(return_type);
              kgpc_type_retain(type_node->type);
              ctx->hash_return->type->info.proc_info.return_type =
                  type_node->type;
              return_type = type_node->type;
              semcheck_expr_set_resolved_kgpc_type_shared(ctx->expr,
                                                          type_node->type);
              if (return_type->kind == TYPE_KIND_ARRAY)
                semcheck_set_array_info_from_kgpctype(
                    ctx->expr, ctx->symtab, return_type, ctx->expr->line_num);
              else
                semcheck_clear_array_info(ctx->expr);
            }
          }
        }
      } else {
        /* Do not clear resolved_kgpc_type here, as it may have been set
         * by constructor handling logic earlier. */
        semcheck_clear_array_info(ctx->expr);
      }
    } else {
      semcheck_expr_set_resolved_kgpc_type_shared(ctx->expr,
                                                  ctx->hash_return->type);
      semcheck_clear_array_info(ctx->expr);
    }

    if (ctx->hash_return->type != NULL &&
        ctx->hash_return->type->kind == TYPE_KIND_PROCEDURE) {
      ListNode_t *formal_params =
          kgpc_type_get_procedure_params(ctx->hash_return->type);
      if (semcheck_append_default_args(&ctx->args_given, formal_params,
                                       ctx->expr->line_num) != 0)
        ++ctx->return_val;
      ctx->expr->expr_data.function_call_data.args_expr = ctx->args_given;
    }
    if (!ctx->is_operator_dispatch && ctx->injected_self &&
        ctx->hash_return != NULL && ctx->hash_return->type != NULL &&
        ctx->hash_return->type->kind == TYPE_KIND_PROCEDURE) {
      int resolved_expects_self = 0;
      ListNode_t *params = ctx->hash_return->type->info.proc_info.params;
      if (params != NULL) {
        Tree_t *first_param = (Tree_t *)params->cur;
        if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
            first_param->tree_data.var_decl_data.ids != NULL) {
          const char *first_id =
              (const char *)first_param->tree_data.var_decl_data.ids->cur;
          if (first_id != NULL && pascal_identifier_equals(first_id, "Self"))
            resolved_expects_self = 1;
          if (kgpc_getenv("KGPC_DEBUG_FORMAT") != NULL && ctx->id != NULL &&
              pascal_identifier_equals(ctx->id, "Format")) {
            fprintf(
                stderr,
                "[KGPC_DEBUG_FORMAT] resolved first_param=%s expects_self=%d\n",
                first_id != NULL ? first_id : "(null)", resolved_expects_self);
          }
        }
      }
      if (!resolved_expects_self) {
        ListNode_t *first_arg_node =
            ctx->expr->expr_data.function_call_data.args_expr;
        if (first_arg_node != NULL && first_arg_node->cur != NULL) {
          struct Expression *first_arg =
              (struct Expression *)first_arg_node->cur;
          if (first_arg->type == EXPR_VAR_ID &&
              first_arg->expr_data.id != NULL &&
              pascal_identifier_equals(first_arg->expr_data.id, "Self")) {
            ctx->expr->expr_data.function_call_data.args_expr =
                first_arg_node->next;
            first_arg_node->next = NULL;
            destroy_expr(first_arg);
            free(first_arg_node);
            ctx->args_given = ctx->expr->expr_data.function_call_data.args_expr;
            if (kgpc_getenv("KGPC_DEBUG_FORMAT") != NULL && ctx->id != NULL &&
                pascal_identifier_equals(ctx->id, "Format")) {
              fprintf(stderr, "[KGPC_DEBUG_FORMAT] removed injected Self for "
                              "non-method resolution\n");
            }
          }
        }
      }
    }
    if (kgpc_getenv("KGPC_DEBUG_FORMAT") != NULL && ctx->id != NULL &&
        pascal_identifier_equals(ctx->id, "Format")) {
      const char *owner =
          ctx->hash_return != NULL ? ctx->hash_return->owner_class : NULL;
      const char *hid = ctx->hash_return != NULL ? ctx->hash_return->id : NULL;
      fprintf(stderr,
              "[KGPC_DEBUG_FORMAT] resolved hash_return id=%s owner=%s "
              "mangled=%s\n",
              hid != NULL ? hid : "(null)", owner != NULL ? owner : "(null)",
              (ctx->hash_return != NULL && ctx->hash_return->mangled_id != NULL)
                  ? ctx->hash_return->mangled_id
                  : "(null)");
    }

    /***** THEN VERIFY ARGS INSIDE *****/
    ctx->cur_arg = 0;
    /* Get formal arguments from KgpcType instead of deprecated args field */
    ctx->true_args = kgpc_type_get_procedure_params(ctx->hash_return->type);

    /* For constructors, skip the first argument (class type) in args_given
     * AND skip the first formal parameter (Self) in ctx->true_args */
    ListNode_t *args_to_validate = ctx->args_given;
    ListNode_t *true_args_to_validate = ctx->true_args;

    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
      fprintf(stderr,
              "[SemCheck] Constructor %s: args_given=%d, true_args=%d\n",
              ctx->id, ListLength(ctx->args_given), ListLength(ctx->true_args));
    }
    if (kgpc_getenv("KGPC_DEBUG_FORMAT") != NULL && ctx->id != NULL &&
        pascal_identifier_equals(ctx->id, "Format")) {
      fprintf(stderr,
              "[KGPC_DEBUG_FORMAT] args_given_ptr=%p args_to_validate_ptr=%p "
              "true_args_ptr=%p\n",
              (void *)ctx->args_given, (void *)args_to_validate,
              (void *)ctx->true_args);
      int idx = 0;
      for (ListNode_t *cur = args_to_validate; cur != NULL;
           cur = cur->next, idx++) {
        struct Expression *arg_expr = (struct Expression *)cur->cur;
        const char *arg_id = (arg_expr != NULL && arg_expr->type == EXPR_VAR_ID)
                                 ? arg_expr->expr_data.id
                                 : NULL;
        fprintf(stderr,
                "[KGPC_DEBUG_FORMAT] args_to_validate[%d]=node=%p expr=%p "
                "type=%d id=%s\n",
                idx, (void *)cur, (void *)arg_expr,
                arg_expr != NULL ? arg_expr->type : -1,
                arg_id != NULL ? arg_id : "(null)");
      }
    }

    int is_constructor_call = 0;
    if (ctx->args_given != NULL && ctx->hash_return != NULL &&
        ctx->hash_return->owner_class != NULL) {
      const char *method_name = ctx->hash_return->method_name;
      if (method_name == NULL &&
          ctx->expr->expr_data.function_call_data.placeholder_method_name !=
              NULL)
        method_name =
            ctx->expr->expr_data.function_call_data.placeholder_method_name;
      struct RecordType *record_info = semcheck_lookup_record_type(
          ctx->symtab, ctx->hash_return->owner_class);
      if (record_info != NULL && record_info->is_class &&
          !record_info->is_type_helper && method_name != NULL &&
          (pascal_identifier_equals(method_name, "Create") ||
           pascal_identifier_equals(method_name, "Destroy")) &&
          !from_cparser_is_method_static(ctx->hash_return->owner_class,
                                         method_name)) {
        is_constructor_call = 1;
      }
    }

    if (is_constructor_call) {
      /* If lengths match, we assume first arg is class type and first param is
       * Self -> skip both */
      if (ListLength(ctx->args_given) == ListLength(ctx->true_args)) {
        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
          fprintf(stderr, "[SemCheck] Skipping both Self and Class Type\n");
        args_to_validate = ctx->args_given->next;
        if (true_args_to_validate != NULL)
          true_args_to_validate = true_args_to_validate->next;
      }
      /* If args_given is one less, we assume class type is implicit but Self is
         explicit in params -> skip Self only */
      else if (ListLength(ctx->args_given) == ListLength(ctx->true_args) - 1) {
        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
          fprintf(stderr, "[SemCheck] Skipping Self only\n");
        if (true_args_to_validate != NULL)
          true_args_to_validate = true_args_to_validate->next;
      }
    } else if (true_args_to_validate != NULL && args_to_validate != NULL &&
               ListLength(ctx->true_args) == ListLength(ctx->args_given) + 1) {
      Tree_t *first_formal = (Tree_t *)true_args_to_validate->cur;
      const char *first_formal_name = NULL;
      if (first_formal != NULL && first_formal->type == TREE_VAR_DECL &&
          first_formal->tree_data.var_decl_data.ids != NULL)
        first_formal_name =
            (const char *)first_formal->tree_data.var_decl_data.ids->cur;
      else if (first_formal != NULL && first_formal->type == TREE_ARR_DECL &&
               first_formal->tree_data.arr_decl_data.ids != NULL)
        first_formal_name =
            (const char *)first_formal->tree_data.arr_decl_data.ids->cur;

      struct Expression *first_actual =
          (struct Expression *)args_to_validate->cur;
      if (first_formal_name != NULL &&
          pascal_identifier_equals(first_formal_name, "Self") &&
          first_actual != NULL && first_actual->type == EXPR_VAR_ID &&
          first_actual->expr_data.id != NULL &&
          pascal_identifier_equals(first_actual->expr_data.id, "Self")) {
        /* Mixed static/instance overloads can carry a leading implicit Self
         * formal for class methods; allow call validation to skip it. */
        true_args_to_validate = true_args_to_validate->next;
      }
    }

    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
      int args_given_count = 0;
      int true_args_count = 0;
      int args_to_validate_count = 0;
      for (ListNode_t *c = ctx->args_given; c != NULL; c = c->next)
        args_given_count++;
      for (ListNode_t *c = true_args_to_validate; c != NULL; c = c->next)
        true_args_count++;
      for (ListNode_t *c = args_to_validate; c != NULL; c = c->next)
        args_to_validate_count++;
      fprintf(stderr,
              "[SemCheck] Constructor %s: args_given=%d, true_args=%d, "
              "args_to_validate=%d\n",
              ctx->id, args_given_count, true_args_count,
              args_to_validate_count);
    }

    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
      fprintf(stderr,
              "[SemCheck] Before validation loop: true_args len=%d, args_given "
              "len=%d\n",
              ListLength(ctx->true_args), ListLength(ctx->args_given));

      ListNode_t *cur = ctx->true_args;
      int i = 0;
      while (cur != NULL) {
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl->type == TREE_VAR_DECL) {
          ListNode_t *ids = decl->tree_data.var_decl_data.ids;
          while (ids != NULL) {
            fprintf(stderr, "[SemCheck]   true_arg[%d]: %s\n", i++,
                    (char *)ids->cur);
            ids = ids->next;
          }
        } else {
          fprintf(stderr, "[SemCheck]   true_arg[%d]: (array decl)\n", i++);
        }
        cur = cur->next;
      }

      cur = ctx->args_given;
      i = 0;
      while (cur != NULL) {
        struct Expression *e = (struct Expression *)cur->cur;
        fprintf(stderr, "[SemCheck]   arg_given[%d]: type=%d\n", i++, e->type);
        cur = cur->next;
      }
    }

    while (true_args_to_validate != NULL && args_to_validate != NULL) {
      if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] validation loop: arg %d\n", ctx->cur_arg);
        fprintf(stderr, "[SemCheck]   true_args_to_validate=%p next=%p\n",
                (void *)true_args_to_validate,
                (void *)true_args_to_validate->next);
        fprintf(stderr, "[SemCheck]   args_to_validate=%p next=%p\n",
                (void *)args_to_validate, (void *)args_to_validate->next);
      }
      ctx->cur_arg++;
      assert(args_to_validate->type == LIST_EXPR);
      assert(true_args_to_validate->type == LIST_TREE);

      ctx->arg_decl = (Tree_t *)true_args_to_validate->cur;
      if (ctx->arg_decl->type != TREE_VAR_DECL &&
          ctx->arg_decl->type != TREE_ARR_DECL) {
        semcheck_error_with_context_at(ctx->expr->line_num, ctx->expr->col_num,
                                       ctx->expr->source_index,
                                       "Error on line %d, unsupported "
                                       "parameter declaration in call to %s.\n",
                                       ctx->expr->line_num, ctx->id);
        ++ctx->return_val;
        true_args_to_validate = true_args_to_validate->next;
        args_to_validate = args_to_validate->next;
        continue;
      }
      int named_arg_mismatch = 0;
      struct Expression *current_arg_expr =
          (struct Expression *)args_to_validate->cur;
      if (current_arg_expr != NULL && current_arg_expr->type == EXPR_RELOP &&
          current_arg_expr->expr_data.relop_data.type == EQ &&
          current_arg_expr->expr_data.relop_data.left != NULL &&
          current_arg_expr->expr_data.relop_data.left->type == EXPR_VAR_ID &&
          current_arg_expr->expr_data.relop_data.right != NULL) {
        const char *expected_name = NULL;
        if (ctx->arg_decl->type == TREE_VAR_DECL &&
            ctx->arg_decl->tree_data.var_decl_data.ids != NULL)
          expected_name =
              (const char *)ctx->arg_decl->tree_data.var_decl_data.ids->cur;
        else if (ctx->arg_decl->type == TREE_ARR_DECL &&
                 ctx->arg_decl->tree_data.arr_decl_data.ids != NULL)
          expected_name =
              (const char *)ctx->arg_decl->tree_data.arr_decl_data.ids->cur;

        const char *given_name =
            current_arg_expr->expr_data.relop_data.left->expr_data.id;
        if (expected_name != NULL && given_name != NULL) {
          if (pascal_identifier_equals(expected_name, given_name)) {
            struct Expression *named_value =
                current_arg_expr->expr_data.relop_data.right;
            int rhs_type = UNKNOWN_TYPE;
            KgpcType *rhs_kgpc_type = NULL;
            semcheck_expr_with_type(&rhs_kgpc_type, ctx->symtab, named_value,
                                    ctx->max_scope_lev, NO_MUTATE);
            rhs_type = semcheck_tag_from_kgpc(rhs_kgpc_type);
            if (semcheck_named_arg_type_compatible(ctx->arg_decl, named_value,
                                                   rhs_type, ctx->symtab)) {
              struct Expression *named_left =
                  current_arg_expr->expr_data.relop_data.left;
              current_arg_expr->expr_data.relop_data.left = NULL;
              current_arg_expr->expr_data.relop_data.right = NULL;
              destroy_expr(named_left);
              destroy_expr(current_arg_expr);
              current_arg_expr = named_value;
              args_to_validate->cur = current_arg_expr;
            }
          } else if (semcheck_param_list_contains_name(ctx->true_args,
                                                       given_name)) {
            named_arg_mismatch = 1;
          }
        }
      }
      if (semcheck_prepare_array_literal_argument(
              ctx->arg_decl, current_arg_expr, ctx->symtab, ctx->max_scope_lev,
              ctx->expr->line_num) != 0) {
        ++ctx->return_val;
        true_args_to_validate = true_args_to_validate->next;
        args_to_validate = args_to_validate->next;
        continue;
      }
      if (semcheck_prepare_record_constructor_argument(
              ctx->arg_decl, current_arg_expr, ctx->symtab, ctx->max_scope_lev,
              ctx->expr->line_num) != 0) {
        ++ctx->return_val;
        true_args_to_validate = true_args_to_validate->next;
        args_to_validate = args_to_validate->next;
        continue;
      }

      /* Check if the formal parameter is a var or out parameter.
       * If so, mark the actual argument expression as mutated. */
      int arg_mutating = NO_MUTATE;
      if (ctx->arg_decl->type == TREE_VAR_DECL &&
          ctx->arg_decl->tree_data.var_decl_data.is_var_param) {
        arg_mutating = MUTATE;
      }

      KgpcType *arg_kgpc_type_call = NULL;
      int handled_as_type_ref = 0;
      if (current_arg_expr != NULL &&
          current_arg_expr->type == EXPR_FUNCTION_CALL &&
          current_arg_expr->expr_data.function_call_data.args_expr == NULL &&
          current_arg_expr->expr_data.function_call_data.id != NULL) {
        const char *type_id = current_arg_expr->expr_data.function_call_data.id;
        HashNode_t *type_node =
            semcheck_find_preferred_type_node(ctx->symtab, type_id);
        if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) {
          /* FPC behavior: interface type identifiers in expression context
           * are treated as TGUID references (used by Supports(..., IIntf,
           * ...)). */
          struct RecordType *type_record = get_record_type_from_node(type_node);
          if (type_record != NULL && type_record->is_interface) {
            HashNode_t *tguid_node =
                semcheck_find_type_node_with_kgpc_type(ctx->symtab, "TGUID");
            if (tguid_node != NULL && tguid_node->type != NULL) {
              semcheck_expr_set_resolved_kgpc_type_shared(current_arg_expr,
                                                          tguid_node->type);
              arg_kgpc_type_call = current_arg_expr->resolved_kgpc_type;
              ctx->arg_type = semcheck_tag_from_kgpc(arg_kgpc_type_call);
              handled_as_type_ref = 1;
            }
          }

          if (!handled_as_type_ref && type_node->type != NULL) {
            semcheck_expr_set_resolved_kgpc_type_shared(current_arg_expr,
                                                        type_node->type);
            arg_kgpc_type_call = current_arg_expr->resolved_kgpc_type;
            ctx->arg_type = semcheck_tag_from_kgpc(arg_kgpc_type_call);
            handled_as_type_ref = 1;
          }
        }
      }
      if (!handled_as_type_ref) {
        ctx->return_val += semcheck_expr_with_type(
            &arg_kgpc_type_call, ctx->symtab, current_arg_expr,
            ctx->max_scope_lev, arg_mutating);
        ctx->arg_type = semcheck_tag_from_kgpc(arg_kgpc_type_call);
      }
      if (ctx->arg_type == UNKNOWN_TYPE && current_arg_expr != NULL &&
          current_arg_expr->type == EXPR_VAR_ID &&
          current_arg_expr->expr_data.id != NULL) {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, ctx->symtab,
                       current_arg_expr->expr_data.id) != 0 &&
            type_node != NULL && type_node->hash_type == HASHTYPE_TYPE &&
            type_node->type != NULL) {
          semcheck_expr_set_resolved_kgpc_type_shared(current_arg_expr,
                                                      type_node->type);
          ctx->arg_type = semcheck_tag_from_kgpc(type_node->type);
        }
      }
      if (named_arg_mismatch)
        ctx->arg_type = UNKNOWN_TYPE;
      if (kgpc_getenv("KGPC_DEBUG_FORMAT") != NULL && ctx->id != NULL &&
          pascal_identifier_equals(ctx->id, "Format")) {
        const char *kgpc_str = "<null>";
        if (current_arg_expr != NULL &&
            current_arg_expr->resolved_kgpc_type != NULL)
          kgpc_str = kgpc_type_to_string(current_arg_expr->resolved_kgpc_type);
        fprintf(stderr,
                "[KGPC_DEBUG_FORMAT] arg=%d expr_type=%d arg_type=%d kgpc=%s\n",
                ctx->cur_arg,
                current_arg_expr != NULL ? current_arg_expr->type : -1,
                ctx->arg_type, kgpc_str);
        if (current_arg_expr != NULL)
          semcheck_debug_expr_brief(current_arg_expr, "format arg");
      }
      if (kgpc_getenv("KGPC_DEBUG_CALL_TYPES") != NULL && ctx->id != NULL &&
          (pascal_identifier_equals(ctx->id, "FileDateToDateTime") ||
           pascal_identifier_equals(ctx->id, "FileDateToUniversal"))) {
        fprintf(stderr,
                "[KGPC_DEBUG_CALL_TYPES] call=%s arg=%d semcheck_type=%d "
                "resolved=%d\n",
                ctx->id, ctx->cur_arg, ctx->arg_type,
                current_arg_expr != NULL
                    ? (current_arg_expr->resolved_kgpc_type
                           ? semcheck_tag_from_kgpc(
                                 current_arg_expr->resolved_kgpc_type)
                           : UNKNOWN_TYPE)
                    : -1);
        if (current_arg_expr != NULL)
          semcheck_debug_expr_brief(current_arg_expr, "call arg");
      }
      if (kgpc_getenv("KGPC_DEBUG_COMPAREMEM") != NULL && ctx->id != NULL &&
          pascal_identifier_equals(ctx->id, "CompareMem")) {
        const char *kgpc_str = "<null>";
        if (current_arg_expr != NULL &&
            current_arg_expr->resolved_kgpc_type != NULL)
          kgpc_str = kgpc_type_to_string(current_arg_expr->resolved_kgpc_type);
        fprintf(stderr,
                "[KGPC_DEBUG_COMPAREMEM] arg=%d expr_type=%d tag=%d "
                "kgpc_kind=%d kgpc=%s\n",
                ctx->cur_arg,
                current_arg_expr != NULL ? current_arg_expr->type : -1,
                ctx->arg_type,
                (current_arg_expr != NULL &&
                 current_arg_expr->resolved_kgpc_type != NULL)
                    ? current_arg_expr->resolved_kgpc_type->kind
                    : -1,
                kgpc_str);
        if (current_arg_expr != NULL)
          semcheck_debug_expr_brief(current_arg_expr, "comparemem arg");
      }
      if (ctx->arg_decl->type == TREE_VAR_DECL)
        ctx->true_arg_ids = ctx->arg_decl->tree_data.var_decl_data.ids;
      else
        ctx->true_arg_ids = ctx->arg_decl->tree_data.arr_decl_data.ids;

      while (ctx->true_arg_ids != NULL && args_to_validate != NULL) {
        int param_is_untyped = semcheck_decl_is_untyped_param(ctx->arg_decl);
        int expected_type = resolve_param_type(ctx->arg_decl, ctx->symtab);
        int formal_is_array_decl = 0;
        if (ctx->arg_decl->type == TREE_ARR_DECL) {
          formal_is_array_decl = 1;
        } else if (ctx->arg_decl->type == TREE_VAR_DECL) {
          struct TypeAlias *inline_alias =
              ctx->arg_decl->tree_data.var_decl_data.inline_type_alias;
          if (inline_alias != NULL && inline_alias->is_array)
            formal_is_array_decl = 1;
        }
        if (!formal_is_array_decl) {
          int owns_formal_probe = 0;
          KgpcType *formal_probe = resolve_type_from_vardecl(
              ctx->arg_decl, ctx->symtab, &owns_formal_probe);
          if (formal_probe != NULL &&
              (kgpc_type_is_array(formal_probe) ||
               kgpc_type_is_array_of_const(formal_probe)))
            formal_is_array_decl = 1;
          if (owns_formal_probe && formal_probe != NULL)
            destroy_kgpc_type(formal_probe);
        }
        if (kgpc_getenv("KGPC_DEBUG_FORMAT") != NULL && ctx->id != NULL &&
            pascal_identifier_equals(ctx->id, "Format")) {
          fprintf(stderr,
                  "[KGPC_DEBUG_FORMAT] arg=%d expected_type=%d actual_type=%d "
                  "expr_type=%d\n",
                  ctx->cur_arg, expected_type, ctx->arg_type,
                  current_arg_expr != NULL ? current_arg_expr->type : -1);
        }

        if (formal_is_array_decl && current_arg_expr != NULL &&
            current_arg_expr->type == EXPR_ARRAY_LITERAL) {
          ctx->arg_type = expected_type;
        }
        if (param_is_untyped || expected_type == UNKNOWN_TYPE ||
            ctx->arg_type == UNKNOWN_TYPE ||
            expected_type == BUILTIN_ANY_TYPE) {
          /* Untyped parameters accept any argument type. */
          /* No validation needed. */
        } else if (formal_is_array_decl && current_arg_expr != NULL &&
                   current_arg_expr->resolved_kgpc_type != NULL &&
                   kgpc_type_is_array(current_arg_expr->resolved_kgpc_type)) {
          int owns_expected_kgpc = 0;
          KgpcType *expected_kgpc = resolve_type_from_vardecl(
              ctx->arg_decl, ctx->symtab, &owns_expected_kgpc);
          KgpcType *arg_array = current_arg_expr->resolved_kgpc_type;
          int array_compatible = 0;

          if (expected_kgpc != NULL && kgpc_type_is_array(expected_kgpc)) {
            if (are_types_compatible_for_assignment(expected_kgpc, arg_array,
                                                    ctx->symtab)) {
              array_compatible = 1;
            } else {
              KgpcType *expected_elem =
                  kgpc_type_get_array_element_type_resolved(expected_kgpc,
                                                            ctx->symtab);
              KgpcType *arg_elem = kgpc_type_get_array_element_type_resolved(
                  arg_array, ctx->symtab);
              if (expected_elem != NULL && arg_elem != NULL) {
                if (kgpc_type_equals(expected_elem, arg_elem))
                  array_compatible = 1;
                else if (expected_elem->kind == TYPE_KIND_PRIMITIVE &&
                         arg_elem->kind == TYPE_KIND_PRIMITIVE &&
                         expected_elem->info.primitive_type_tag ==
                             arg_elem->info.primitive_type_tag)
                  array_compatible = 1;
                else if ((kgpc_type_is_pointer(expected_elem) &&
                          kgpc_type_equals_tag(arg_elem, POINTER_TYPE)) ||
                         (kgpc_type_is_pointer(arg_elem) &&
                          kgpc_type_equals_tag(expected_elem, POINTER_TYPE)))
                  array_compatible = 1;
                else if (are_types_compatible_for_assignment(
                             expected_elem, arg_elem, ctx->symtab))
                  array_compatible = 1;
              }
            }
          }

          if (owns_expected_kgpc && expected_kgpc != NULL)
            destroy_kgpc_type(expected_kgpc);

          if (!array_compatible) {
            const char *expected_str = type_tag_to_string(expected_type);
            const char *given_str =
                kgpc_type_to_string(current_arg_expr->resolved_kgpc_type);
            semcheck_error_with_context_at(
                ctx->expr->line_num, ctx->expr->col_num,
                ctx->expr->source_index,
                "Error on line %d, on function call %s, argument %d: Type "
                "mismatch (expected: %s, given: %s)!\n\n",
                ctx->expr->line_num, ctx->id, ctx->cur_arg, expected_str,
                given_str);
            ++ctx->return_val;
          }
        } else if (ctx->arg_type != expected_type) {
          int type_compatible = 0;
          int owns_expected_kgpc = 0;
          KgpcType *expected_kgpc = resolve_type_from_vardecl(
              ctx->arg_decl, ctx->symtab, &owns_expected_kgpc);

          semcheck_bind_set_literal_to_expected_type_for_call(
              current_arg_expr, expected_kgpc, &ctx->arg_type);

          int owns_arg_kgpc = 0;
          KgpcType *arg_kgpc = current_arg_expr != NULL
                                   ? current_arg_expr->resolved_kgpc_type
                                   : NULL;
          if (arg_kgpc == NULL && current_arg_expr != NULL &&
              current_arg_expr->type == EXPR_INUM) {
            arg_kgpc = create_primitive_type(ctx->arg_type);
            owns_arg_kgpc = 1;
          }

          /* KgpcType is the primary compatibility mechanism.
           * Do not reintroduce legacy type-tag fallback checks here. */
          if (expected_kgpc != NULL && arg_kgpc != NULL &&
              are_types_compatible_for_assignment(expected_kgpc, arg_kgpc,
                                                  ctx->symtab)) {
            type_compatible = 1;
          }
          /* Try record→primitive conversion via := operators */
          if (!type_compatible && expected_kgpc != NULL && arg_kgpc != NULL &&
              current_arg_expr != NULL &&
              !(ctx->arg_decl->type == TREE_VAR_DECL &&
                ctx->arg_decl->tree_data.var_decl_data.is_var_param)) {
            int conv_owned = 0;
            KgpcType *conv_type = arg_kgpc;
            struct Expression *conv_expr = current_arg_expr;
            if (semcheck_try_record_conversion_expression(
                    ctx->symtab, &conv_expr, NULL, expected_kgpc, &conv_type,
                    &conv_owned)) {
              args_to_validate->cur = conv_expr;
              current_arg_expr = conv_expr;
              if (are_types_compatible_for_assignment(expected_kgpc, conv_type,
                                                      ctx->symtab)) {
                type_compatible = 1;
                ctx->arg_type = semcheck_tag_from_kgpc(conv_type);
              }
              if (conv_owned && conv_type != NULL)
                destroy_kgpc_type(conv_type);
            }
          }
          /* Integer arguments are assignment-compatible across integer widths.
           */
          if (!type_compatible && is_integer_type(expected_type) &&
              is_integer_type(ctx->arg_type)) {
            type_compatible = 1;
          }
          if (!type_compatible && is_integer_type(ctx->arg_type)) {
            const char *formal_id = NULL;
            if (ctx->arg_decl->type == TREE_VAR_DECL)
              formal_id = ctx->arg_decl->tree_data.var_decl_data.type_id;
            else if (ctx->arg_decl->type == TREE_ARR_DECL)
              formal_id = ctx->arg_decl->tree_data.arr_decl_data.type_id;
            if (formal_id != NULL &&
                pascal_identifier_equals(formal_id, "tconstexprint")) {
              type_compatible = 1;
            }
          }
          /* For array of const: accept any array literal - elements are
           * converted to TVarRec */
          if (!type_compatible && expected_type == ARRAY_OF_CONST_TYPE &&
              current_arg_expr != NULL &&
              current_arg_expr->type == EXPR_ARRAY_LITERAL) {
            type_compatible = 1;
          }

          if (!type_compatible && expected_kgpc == NULL &&
              expected_type != UNKNOWN_TYPE &&
              expected_type != BUILTIN_ANY_TYPE && expected_type != PROCEDURE) {
            KGPC_SEMCHECK_HARD_ASSERT(
                0,
                "missing expected KgpcType in argument compatibility: call=%s "
                "arg=%d expected_tag=%d",
                ctx->id != NULL ? ctx->id : "<unknown>", ctx->cur_arg,
                expected_type);
          }

          if (!type_compatible && current_arg_expr != NULL &&
              current_arg_expr->pointer_subtype_id != NULL) {
            const char *formal_id = NULL;
            if (ctx->arg_decl->type == TREE_VAR_DECL)
              formal_id = ctx->arg_decl->tree_data.var_decl_data.type_id;
            else if (ctx->arg_decl->type == TREE_ARR_DECL)
              formal_id = ctx->arg_decl->tree_data.arr_decl_data.type_id;
            if (formal_id != NULL &&
                semcheck_class_type_ids_compatible(
                    ctx->symtab, formal_id,
                    current_arg_expr->pointer_subtype_id)) {
              type_compatible = 1;
            }
          }

          /* Allow string literals to be passed as PAnsiChar/PChar parameters.
           * In Pascal, string literals are implicitly convertible to PChar. */
          if (!type_compatible && current_arg_expr != NULL &&
              (ctx->arg_type == STRING_TYPE ||
               ctx->arg_type == SHORTSTRING_TYPE ||
               current_arg_expr->type == EXPR_STRING)) {
            const char *param_type_id = NULL;
            if (ctx->arg_decl->type == TREE_VAR_DECL)
              param_type_id = ctx->arg_decl->tree_data.var_decl_data.type_id;
            if (param_type_id != NULL &&
                (pascal_identifier_equals(param_type_id, "PAnsiChar") ||
                 pascal_identifier_equals(param_type_id, "PChar") ||
                 pascal_identifier_equals(param_type_id, "PWideChar"))) {
              type_compatible = 1;
            }
            /* Also check if parameter is a pointer type pointing to char */
            if (!type_compatible && expected_type == POINTER_TYPE) {
              if (expected_kgpc != NULL &&
                  kgpc_type_is_pointer(expected_kgpc)) {
                KgpcType *points_to = expected_kgpc->info.points_to;
                if (points_to != NULL &&
                    points_to->kind == TYPE_KIND_PRIMITIVE &&
                    points_to->info.primitive_type_tag == CHAR_TYPE) {
                  type_compatible = 1;
                }
              }
            }
          }

          /* Allow dynamic array parameters to match when both sides are arrays.
           */
          if (!type_compatible && current_arg_expr != NULL) {
            if (expected_kgpc != NULL &&
                expected_kgpc->kind == TYPE_KIND_ARRAY) {
              if (arg_kgpc != NULL && arg_kgpc->kind == TYPE_KIND_ARRAY) {
                if (are_types_compatible_for_assignment(expected_kgpc, arg_kgpc,
                                                        ctx->symtab))
                  type_compatible = 1;
                else {
                  KgpcType *expected_elem =
                      expected_kgpc->info.array_info.element_type;
                  KgpcType *arg_elem = arg_kgpc->info.array_info.element_type;
                  if (expected_elem != NULL && arg_elem != NULL &&
                      expected_elem->kind == TYPE_KIND_PRIMITIVE &&
                      expected_elem->info.primitive_type_tag == CHAR_TYPE &&
                      kgpc_type_is_shortstring(arg_elem)) {
                    /* Allow ShortString elements to match Char arrays (FPC
                     * open-array behavior). */
                    type_compatible = 1;
                  } else if (expected_elem != NULL && arg_elem != NULL &&
                             ((kgpc_type_is_pointer(expected_elem) &&
                               kgpc_type_equals_tag(arg_elem, POINTER_TYPE)) ||
                              (kgpc_type_is_pointer(arg_elem) &&
                               kgpc_type_equals_tag(expected_elem,
                                                    POINTER_TYPE)))) {
                    type_compatible = 1;
                  } else if (expected_elem != NULL && arg_elem != NULL &&
                             are_types_compatible_for_assignment(
                                 expected_elem, arg_elem, ctx->symtab)) {
                    type_compatible = 1;
                  }
                }
              } else if (current_arg_expr->is_array_expr) {
                KgpcType *expected_elem =
                    expected_kgpc->info.array_info.element_type;
                if (expected_elem != NULL &&
                    expected_elem->kind == TYPE_KIND_PRIMITIVE &&
                    current_arg_expr->array_element_type != UNKNOWN_TYPE &&
                    expected_elem->info.primitive_type_tag ==
                        current_arg_expr->array_element_type) {
                  type_compatible = 1;
                }
              }
            }
          }

          if (!type_compatible && expected_type == PROCEDURE &&
              current_arg_expr != NULL &&
              current_arg_expr->resolved_kgpc_type != NULL) {
            KgpcType *arg_kgpc = current_arg_expr->resolved_kgpc_type;
            if (kgpc_type_is_procedure(arg_kgpc)) {
              type_compatible = 1;
            } else if (kgpc_type_is_pointer(arg_kgpc) &&
                       arg_kgpc->info.points_to != NULL &&
                       kgpc_type_is_procedure(arg_kgpc->info.points_to)) {
              type_compatible = 1;
            }
          }

          if (!type_compatible && ctx->arg_type == UNKNOWN_TYPE &&
              current_arg_expr != NULL &&
              current_arg_expr->type == EXPR_VAR_ID) {
            HashNode_t *arg_node = NULL;
            if (FindSymbol(&arg_node, ctx->symtab,
                           current_arg_expr->expr_data.id) != 0 &&
                arg_node != NULL && arg_node->type == NULL) {
              type_compatible = 1;
            }
          }
          if (!type_compatible && ctx->id != NULL &&
              pascal_identifier_equals(ctx->id, "supports") &&
              (expected_type == POINTER_TYPE || expected_type == RECORD_TYPE) &&
              ctx->arg_type == UNKNOWN_TYPE && ctx->cur_arg == 2) {
            /* Keep Supports() second-argument handling permissive for
             * interface-type identifiers until parser typing is normalized.
             * The RECORD_TYPE case covers the TGUID overload (interface→TGUID).
             */
            type_compatible = 1;
          }
          /* FPC allows passing an interface type identifier where a TGUID
           * record is expected. The compiler implicitly resolves it to the
           * interface's GUID. Until we implement this fully, accept type
           * identifiers (HASHTYPE_TYPE) with pointer kind (interface types) as
           * compatible with record parameters. */
          if (!type_compatible && expected_type == RECORD_TYPE &&
              ctx->arg_type == UNKNOWN_TYPE && current_arg_expr != NULL &&
              current_arg_expr->type == EXPR_FUNCTION_CALL) {
            const char *fc_id =
                current_arg_expr->expr_data.function_call_data.id;
            if (fc_id != NULL) {
              HashNode_t *fc_node = NULL;
              if (FindSymbol(&fc_node, ctx->symtab, fc_id) && fc_node != NULL &&
                  fc_node->hash_type == HASHTYPE_TYPE) {
                type_compatible = 1;
              }
            }
          }

          if (owns_arg_kgpc && arg_kgpc != NULL)
            destroy_kgpc_type(arg_kgpc);
          if (owns_expected_kgpc && expected_kgpc != NULL)
            destroy_kgpc_type(expected_kgpc);

          if (!type_compatible) {
            if (kgpc_getenv("KGPC_ASSERT_STRING_LITERAL_TYPED") != NULL &&
                current_arg_expr != NULL &&
                current_arg_expr->type == EXPR_STRING) {
              assert(ctx->arg_type != UNKNOWN_TYPE &&
                     "string literal should have a resolved type before call "
                     "validation");
            }
            if (kgpc_getenv("KGPC_DEBUG_CALL_TYPES") != NULL &&
                ctx->id != NULL &&
                (pascal_identifier_equals(ctx->id, "FileDateToDateTime") ||
                 pascal_identifier_equals(ctx->id, "FileDateToUniversal") ||
                 (ctx->hash_return != NULL && ctx->hash_return->id != NULL &&
                  pascal_identifier_equals(ctx->hash_return->id,
                                           "TGUIDHelper__Create")))) {
              fprintf(stderr,
                      "[KGPC_DEBUG_CALL_TYPES] call=%s arg=%d expected=%d "
                      "actual=%d expr_type=%d kgpc_kind=%d\n",
                      ctx->id, ctx->cur_arg, expected_type, ctx->arg_type,
                      current_arg_expr ? current_arg_expr->type : -1,
                      (current_arg_expr != NULL &&
                       current_arg_expr->resolved_kgpc_type != NULL)
                          ? current_arg_expr->resolved_kgpc_type->kind
                          : -1);
            }
            /* CompareMem expects pointers; if an array element or record field
             * element was passed without a recognized address-of, auto-wrap
             * with @. */
            if (!type_compatible && expected_type == POINTER_TYPE &&
                ctx->id != NULL &&
                pascal_identifier_equals(ctx->id, "CompareMem") &&
                current_arg_expr != NULL &&
                current_arg_expr->type != EXPR_ADDR &&
                (current_arg_expr->type == EXPR_ARRAY_ACCESS ||
                 current_arg_expr->type == EXPR_RECORD_ACCESS ||
                 current_arg_expr->is_array_expr)) {
              struct Expression *addr_expr =
                  mk_addressof(current_arg_expr->line_num, current_arg_expr);
              int new_arg_type = UNKNOWN_TYPE;
              KgpcType *new_arg_kgpc_type = NULL;
              semcheck_expr_with_type(&new_arg_kgpc_type, ctx->symtab,
                                      addr_expr, ctx->max_scope_lev, NO_MUTATE);
              new_arg_type = semcheck_tag_from_kgpc(new_arg_kgpc_type);

              if (new_arg_type == POINTER_TYPE) {
                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                  fprintf(stderr, "[SemCheck] Auto-fixing missing @ for "
                                  "CompareMem argument\n");
                args_to_validate->cur = addr_expr;
                current_arg_expr = addr_expr;
                ctx->arg_type = new_arg_type;
                type_compatible = 1;
              } else {
                addr_expr->expr_data.addr_data.expr = NULL;
                free(addr_expr);
              }
            }
            /* Implicit Char -> PChar conversion (FPC allows passing Char where
             * PChar is expected) */
            if (!type_compatible && expected_type == POINTER_TYPE &&
                ctx->arg_type == CHAR_TYPE) {
              int expected_is_pchar = 0;
              int owns_expected = 0;
              KgpcType *expected_kgpc = resolve_type_from_vardecl(
                  ctx->arg_decl, ctx->symtab, &owns_expected);

              if (expected_kgpc != NULL &&
                  kgpc_type_is_pointer(expected_kgpc)) {
                KgpcType *points_to = expected_kgpc->info.points_to;
                if (points_to != NULL &&
                    points_to->kind == TYPE_KIND_PRIMITIVE &&
                    points_to->info.primitive_type_tag == CHAR_TYPE) {
                  expected_is_pchar = 1;
                }
              }

              if (expected_is_pchar) {
                /* Wrap in EXPR_ADDR */
                struct Expression *addr_expr =
                    mk_addressof(current_arg_expr->line_num, current_arg_expr);
                int new_arg_type = UNKNOWN_TYPE;
                KgpcType *new_arg_kgpc_type = NULL;
                semcheck_expr_with_type(&new_arg_kgpc_type, ctx->symtab,
                                        addr_expr, ctx->max_scope_lev,
                                        NO_MUTATE);
                new_arg_type = semcheck_tag_from_kgpc(new_arg_kgpc_type);

                if (new_arg_type == POINTER_TYPE) {
                  /* Fix applied */
                  if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] Auto-fixing missing @ for "
                                    "PChar argument\n");

                  /* Update list node */
                  args_to_validate->cur = addr_expr;
                  /* Update current_arg_expr */
                  current_arg_expr = addr_expr;
                  ctx->arg_type = new_arg_type;
                  type_compatible = 1;
                } else {
                  /* Failed to fix, discard wrapper */
                  addr_expr->expr_data.addr_data.expr = NULL; /* Detach child */
                  free(addr_expr); /* Shallow free since we detached child */
                }
              }

              if (owns_expected && expected_kgpc != NULL)
                destroy_kgpc_type(expected_kgpc);
            }

            if (!type_compatible) {
              /* Get better type descriptions */
              const char *expected_str = type_tag_to_string(expected_type);
              const char *given_str = type_tag_to_string(ctx->arg_type);

              /* Try to get more detailed types from resolved_kgpc_type */
              if (current_arg_expr != NULL &&
                  current_arg_expr->resolved_kgpc_type != NULL)
                given_str =
                    kgpc_type_to_string(current_arg_expr->resolved_kgpc_type);

              semcheck_error_with_context_at(
                  ctx->expr->line_num, ctx->expr->col_num,
                  ctx->expr->source_index,
                  "Error on line %d, on function call %s, argument %d: Type "
                  "mismatch (expected: %s, given: %s)!\n\n",
                  ctx->expr->line_num, ctx->id, ctx->cur_arg, expected_str,
                  given_str);
              ++ctx->return_val;
            }
          }
        }

        ctx->true_arg_ids = ctx->true_arg_ids->next;
        args_to_validate = args_to_validate->next;
      }

      true_args_to_validate = true_args_to_validate->next;
    }
    if (true_args_to_validate == NULL && args_to_validate != NULL) {
      int allow_implicit_self_only = 0;
      if (ctx->expr->expr_data.function_call_data.resolved_func != NULL &&
          ctx->expr->expr_data.function_call_data.resolved_func->owner_class !=
              NULL) {
        if (args_to_validate->next == NULL) {
          struct Expression *only_arg =
              (struct Expression *)args_to_validate->cur;
          if (only_arg != NULL &&
              ((only_arg->type == EXPR_VAR_ID &&
                only_arg->expr_data.id != NULL &&
                pascal_identifier_equals(only_arg->expr_data.id, "Self")) ||
               only_arg->type == EXPR_NIL)) {
            allow_implicit_self_only = 1;
          }
        }
      }
      if (allow_implicit_self_only) {
        args_to_validate = NULL;
      }
      if (args_to_validate == NULL) {
        /* Skip error for implicit Self-only calls with missing formal params */
      } else {
        int allow_forward_params = 0;
        if (ctx->hash_return != NULL && ctx->hash_return->type != NULL &&
            ctx->hash_return->type->kind == TYPE_KIND_PROCEDURE &&
            ctx->hash_return->type->info.proc_info.params == NULL &&
            ctx->hash_return->type->info.proc_info.definition == NULL) {
          allow_forward_params = 1;
        }
        if (!allow_forward_params &&
            !(ctx->hash_return != NULL && ctx->hash_return->is_varargs)) {
          semcheck_error_with_context_at(
              ctx->expr->line_num, ctx->expr->col_num, ctx->expr->source_index,
              "Error on line %d, on function call %s, too many arguments "
              "given!\n\n",
              ctx->expr->line_num, ctx->id);
          ++ctx->return_val;
        }
      }
    } else if (true_args_to_validate != NULL && args_to_validate == NULL) {
      semcheck_error_with_context_at(ctx->expr->line_num, ctx->expr->col_num,
                                     ctx->expr->source_index,
                                     "Error on line %d, on function call %s, "
                                     "not enough arguments given!\n\n",
                                     ctx->expr->line_num, ctx->id);
      ++ctx->return_val;
    }
  }

  ctx->final_status = ctx->return_val;
  return FC_CLEANUP;
}
