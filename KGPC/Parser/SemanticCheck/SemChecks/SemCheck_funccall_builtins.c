#include "SemCheck_Expr_Internal.h"
#include "SemCheck_stmt.h"
#include <time.h>
#include <ctype.h>
#include <limits.h>
#include "SemCheck_funccall_internal.h"

FunccallState funccall_state_early_builtins(FunccallCtx *ctx) {
  /* If the function call was already resolved (e.g., transformed from
   * RECORD_ACCESS to FUNCTION_CALL by semcheck_recordaccess for record
   * helper/static methods), use the cached info. Two cases: 1) resolved_func is
   * set (from record helper path) — go through overload resolution for proper
   * return type propagation. 2) resolved_func is NULL but is_call_info_valid
   * (from semcheck_set_function_call_target which clears resolved_func) —
   * return the cached return type directly. This handles re-evaluation of
   * already-resolved method calls (e.g. TEncoding.UTF8 in a chained access like
   * TEncoding.UTF8.GetBytes). */
  if (ctx->expr->expr_data.function_call_data.is_call_info_valid) {
    if (ctx->expr->expr_data.function_call_data.resolved_func != NULL) {
      ctx->hash_return = ctx->expr->expr_data.function_call_data.resolved_func;
      ctx->scope_return = 1;
      ctx->overload_candidates =
          CreateListNode(ctx->hash_return, LIST_UNSPECIFIED);
      if (ctx->expr->expr_data.function_call_data.mangled_id != NULL)
        ctx->mangled_name =
            strdup(ctx->expr->expr_data.function_call_data.mangled_id);
      ctx->args_given = ctx->expr->expr_data.function_call_data.args_expr;
      return FC_OVERLOAD_RESOLVE;
    } else if (ctx->expr->expr_data.function_call_data.mangled_id != NULL) {
      /* Already fully resolved — use cached return type */
      if (ctx->type_return != NULL) {
        KgpcType *cached_type =
            ctx->expr->expr_data.function_call_data.call_kgpc_type;
        if (cached_type != NULL && cached_type->kind == TYPE_KIND_PROCEDURE) {
          KgpcType *ret_type = kgpc_type_get_return_type(cached_type);
          if (ret_type != NULL) {
            *ctx->type_return = semcheck_tag_from_kgpc(ret_type);
            if (ctx->expr->resolved_kgpc_type == NULL)
              semcheck_expr_set_resolved_kgpc_type_shared(ctx->expr, ret_type);
          }
        }
      }
      return FC_CLEANUP;
    }
  }

  if (FUNCCALL_TIMINGS_ENABLED()) {
    ctx->timing_start_ms = funccall_now_ms();
    fprintf(stderr, "[timing] funccall enter id=%s line=%d\n",
            ctx->id != NULL ? ctx->id : "(null)", ctx->expr->line_num);
  }
  if (ctx->expr->expr_data.function_call_data.is_procedural_var_call) {
    /* If call_kgpc_type hasn't been set yet but we have a procedural_var_expr,
     * evaluate it to determine the type (e.g., for typecast-then-call patterns
     * like TClassGetter(Getter)() ) */
    if (ctx->expr->expr_data.function_call_data.call_kgpc_type == NULL &&
        ctx->expr->expr_data.function_call_data.procedural_var_expr != NULL) {
      struct Expression *proc_expr =
          ctx->expr->expr_data.function_call_data.procedural_var_expr;
      ctx->return_val += semcheck_expr_with_type(NULL, ctx->symtab, proc_expr,
                                                 ctx->max_scope_lev, NO_MUTATE);
      /* Try to get the KgpcType from the evaluated expression */
      KgpcType *resolved = proc_expr->resolved_kgpc_type;
      if (resolved == NULL || resolved->kind != TYPE_KIND_PROCEDURE) {
        /* Look up the type by the function call id (which is the typecast type
         * name) */
        HashNode_t *type_node = NULL;
        if (ctx->id != NULL &&
            FindSymbol(&type_node, ctx->symtab, ctx->id) != 0 &&
            type_node != NULL && type_node->type != NULL) {
          resolved = type_node->type;
        }
      }
      if (resolved != NULL && resolved->kind == TYPE_KIND_PROCEDURE) {
        kgpc_type_retain(resolved);
        ctx->expr->expr_data.function_call_data.call_kgpc_type = resolved;
      }
    }
    if (ctx->type_return != NULL) {
      KgpcType *call_type =
          ctx->expr->expr_data.function_call_data.call_kgpc_type;
      KgpcType *ret_type = NULL;
      if (kgpc_getenv("KGPC_DEBUG_PROC_VAR") != NULL &&
          ctx->expr->expr_data.function_call_data.id != NULL &&
          pascal_identifier_equals(ctx->expr->expr_data.function_call_data.id,
                                   "Ctr")) {
        fprintf(stderr, "[KGPC_DEBUG_PROC_VAR] call_type=%s return_id=%s\n",
                call_type ? kgpc_type_to_string(call_type) : "<null>",
                (call_type && call_type->info.proc_info.return_type_id)
                    ? call_type->info.proc_info.return_type_id
                    : "<null>");
      }
      if (call_type != NULL && call_type->kind == TYPE_KIND_PROCEDURE) {
        ret_type = kgpc_type_get_return_type(call_type);
        if (ret_type == NULL &&
            call_type->info.proc_info.return_type_id != NULL) {
          HashNode_t *ret_node = semcheck_find_preferred_type_node(
              ctx->symtab, call_type->info.proc_info.return_type_id);
          if (ret_node != NULL && ret_node->type != NULL) {
            ret_type = ret_node->type;
            /* Materialize return_type so downstream codegen can find it
             * without needing to re-run a symbol table lookup.  Same
             * pattern as SemCheck_Expr_Types.c when it resolves the
             * implicit-funcptr-call return type. */
            kgpc_type_retain(ret_type);
            call_type->info.proc_info.return_type = ret_type;
          }
        }
      }

      /* Look up the RecordField for pve so we can write-through the sret
       * size cache (first call) and read it back (second call, freed type). */
      struct RecordField *pve_field_desc = NULL;
      {
        struct Expression *pve2 =
            ctx->expr->expr_data.function_call_data.procedural_var_expr;
        if (pve2 != NULL && pve2->type == EXPR_RECORD_ACCESS) {
          struct Expression *recv2 =
              pve2->expr_data.record_access_data.record_expr;
          const char *fname2 = pve2->expr_data.record_access_data.field_id;
          if (recv2 != NULL && recv2->type == EXPR_VAR_ID && fname2 != NULL) {
            HashNode_t *rn = NULL;
            if (FindSymbol(&rn, ctx->symtab, recv2->expr_data.id) != 0 &&
                rn != NULL) {
              struct RecordType *rrt = get_record_type_from_node(rn);
              if (rrt != NULL) {
                long long foff = 0;
                resolve_record_field(ctx->symtab, rrt, fname2, &pve_field_desc,
                                     &foff, ctx->expr->line_num, 1);
              }
            }
          }
        }
      }

      if (ret_type != NULL && ret_type->kind == TYPE_KIND_PRIMITIVE) {
        *ctx->type_return = kgpc_type_get_primitive_tag(ret_type);
        semcheck_expr_set_resolved_type(ctx->expr, *ctx->type_return);
      } else if (ret_type != NULL && ret_type->kind == TYPE_KIND_RECORD) {
        *ctx->type_return = RECORD_TYPE;
        semcheck_expr_set_resolved_kgpc_type_shared(ctx->expr, ret_type);
        if (ctx->expr->expr_data.function_call_data.cached_procvar_sret_size ==
            0) {
          long long sz = kgpc_type_sizeof(ret_type);
          ctx->expr->expr_data.function_call_data.cached_procvar_sret_size =
              (sz > 0) ? sz : 2 * (long long)sizeof(void *);
        }
        /* Write-through: persist sret size in RecordField AST cache so
         * codegen can recover when proc_type is freed (MSYS2 UAF). */
        if (pve_field_desc != NULL)
          pve_field_desc->cached_proc_return_sret_size =
              ctx->expr->expr_data.function_call_data.cached_procvar_sret_size;
      } else if (ret_type != NULL && ret_type->kind == TYPE_KIND_POINTER) {
        *ctx->type_return = POINTER_TYPE;
        /* Directly set resolved_kgpc_type to preserve full pointer type info
         * (e.g., PAnsiChar needs to be a pointer-to-char, not just
         * POINTER_TYPE) */
        if (ctx->expr->resolved_kgpc_type != NULL)
          destroy_kgpc_type(ctx->expr->resolved_kgpc_type);
        kgpc_type_retain(ret_type);
        ctx->expr->resolved_kgpc_type = ret_type;
      } else if (ctx->expr->resolved_kgpc_type != NULL) {
        *ctx->type_return =
            semcheck_tag_from_kgpc(ctx->expr->resolved_kgpc_type);
      } else {
        /* Read-back: if proc_type was freed (MSYS2 UAF), recover the sret
         * size that a prior semcheck pass wrote into the RecordField cache. */
        if (pve_field_desc != NULL &&
            pve_field_desc->cached_proc_return_sret_size > 0) {
          *ctx->type_return = RECORD_TYPE;
          ctx->expr->expr_data.function_call_data.cached_procvar_sret_size =
              pve_field_desc->cached_proc_return_sret_size;
        } else {
          *ctx->type_return = PROCEDURE;
          semcheck_expr_set_resolved_type(ctx->expr, PROCEDURE);
        }
      }
    }
    do {
      ctx->final_status = 0;
      return FC_CLEANUP;
    } while (0);
  }

  /* Builtins that accept type identifiers should bypass generic
   * call-shape/argument expression preprocessing. */
  if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "SizeOf"))
    do {
      ctx->final_status = semcheck_builtin_sizeof(
          ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev);
      return FC_CLEANUP;
    } while (0);
  if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "BitSizeOf"))
    do {
      ctx->final_status = semcheck_builtin_bitsizeof(
          ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev);
      return FC_CLEANUP;
    } while (0);
  if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "IsManagedType"))
    do {
      ctx->final_status = semcheck_builtin_ismanagedtype(
          ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev);
      return FC_CLEANUP;
    } while (0);
  if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "TypeInfo"))
    do {
      ctx->final_status = semcheck_builtin_typeinfo(
          ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev);
      return FC_CLEANUP;
    } while (0);

  ctx->args_given = ctx->expr->expr_data.function_call_data.args_expr;
  if (ctx->expr != NULL)
    ctx->is_operator_dispatch =
        ctx->expr->expr_data.function_call_data.is_operator_call;
  if (ctx->id != NULL) {
    const char *qualifier =
        ctx->expr->expr_data.function_call_data.call_qualifier;
    if (qualifier != NULL) {
      int prefix_is_unit = semcheck_is_unit_name(qualifier);

      if (!prefix_is_unit) {
        /* Treat qualified identifier as a member/procedural field call. */
        struct Expression *receiver_expr =
            mk_varid(ctx->expr->line_num, strdup(qualifier));
        if (receiver_expr != NULL) {
          ListNode_t *recv_node = CreateListNode(receiver_expr, LIST_EXPR);
          recv_node->next = ctx->args_given;
          ctx->args_given = recv_node;
          ctx->expr->expr_data.function_call_data.args_expr = ctx->args_given;
          ctx->expr->expr_data.function_call_data.is_method_call_placeholder =
              1;
          if (ctx->expr->expr_data.function_call_data.placeholder_method_name !=
              NULL)
            free(ctx->expr->expr_data.function_call_data
                     .placeholder_method_name);
          ctx->expr->expr_data.function_call_data.placeholder_method_name =
              strdup(ctx->id);
        }
      } else {
        /* Unit-qualified call; qualifier is already stripped from id. */
        ctx->was_unit_qualified = 1;
        if (ctx->unit_qualifier_name == NULL)
          ctx->unit_qualifier_name = strdup(qualifier);
      }

      free(ctx->expr->expr_data.function_call_data.call_qualifier);
      ctx->expr->expr_data.function_call_data.call_qualifier = NULL;
    }
  }
  if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
    fprintf(stderr,
            "[SemCheck] semcheck_funccall: id='%s' expr=%p resolved_func=%p\n",
            ctx->id != NULL ? ctx->id : "(null)", (void *)ctx->expr,
            (void *)ctx->expr->expr_data.function_call_data.resolved_func);
  }
  if (kgpc_getenv("KGPC_DEBUG_CHECKOBS") != NULL && ctx->id != NULL &&
      pascal_identifier_equals(ctx->id, "CheckObserving")) {
    int arg_count = 0;
    for (ListNode_t *cur = ctx->expr->expr_data.function_call_data.args_expr;
         cur != NULL; cur = cur->next)
      arg_count++;
    fprintf(stderr,
            "[KGPC_DEBUG_CHECKOBS] pre-self: placeholder=%d "
            "was_unit_qualified=%d args=%d\n",
            ctx->expr->expr_data.function_call_data.is_method_call_placeholder,
            ctx->was_unit_qualified, arg_count);
  }
  if (kgpc_getenv("KGPC_DEBUG_FORMAT") != NULL && ctx->id != NULL &&
      pascal_identifier_equals(ctx->id, "Format")) {
    int arg_count = 0;
    for (ListNode_t *cur = ctx->expr->expr_data.function_call_data.args_expr;
         cur != NULL; cur = cur->next)
      arg_count++;
    fprintf(
        stderr,
        "[KGPC_DEBUG_FORMAT] call=Format placeholder=%d placeholder_name=%s "
        "args=%d\n",
        ctx->expr->expr_data.function_call_data.is_method_call_placeholder,
        ctx->expr->expr_data.function_call_data.placeholder_method_name != NULL
            ? ctx->expr->expr_data.function_call_data.placeholder_method_name
            : "(null)",
        arg_count);
    if (ctx->expr->expr_data.function_call_data.args_expr != NULL &&
        ctx->expr->expr_data.function_call_data.args_expr->cur != NULL) {
      struct Expression *first =
          (struct Expression *)
              ctx->expr->expr_data.function_call_data.args_expr->cur;
      const char *first_id = (first != NULL && first->type == EXPR_VAR_ID)
                                 ? first->expr_data.id
                                 : NULL;
      fprintf(stderr, "[KGPC_DEBUG_FORMAT] first_arg expr_type=%d id=%s\n",
              first != NULL ? first->type : -1,
              first_id != NULL ? first_id : "(null)");
    }
  }
  if (kgpc_getenv("KGPC_DEBUG_TRIM") != NULL && ctx->id != NULL &&
      pascal_identifier_equals(ctx->id, "Trim")) {
    int arg_count = 0;
    for (ListNode_t *cur = ctx->expr->expr_data.function_call_data.args_expr;
         cur != NULL; cur = cur->next)
      arg_count++;
    fprintf(stderr, "[KGPC_DEBUG_TRIM] call=Trim placeholder=%d args=%d\n",
            ctx->expr->expr_data.function_call_data.is_method_call_placeholder,
            arg_count);
    if (ctx->expr->expr_data.function_call_data.args_expr != NULL &&
        ctx->expr->expr_data.function_call_data.args_expr->cur != NULL) {
      struct Expression *first =
          (struct Expression *)
              ctx->expr->expr_data.function_call_data.args_expr->cur;
      const char *first_id = (first != NULL && first->type == EXPR_VAR_ID)
                                 ? first->expr_data.id
                                 : NULL;
      fprintf(stderr, "[KGPC_DEBUG_TRIM] first_arg expr_type=%d id=%s\n",
              first != NULL ? first->type : -1,
              first_id != NULL ? first_id : "(null)");
    }
  }
  if (kgpc_getenv("KGPC_DEBUG_CALL_TYPES") != NULL && ctx->id != NULL &&
      pascal_identifier_equals(ctx->id, "IsDirectory")) {
    int arg_count = 0;
    for (ListNode_t *cur = ctx->expr->expr_data.function_call_data.args_expr;
         cur != NULL; cur = cur->next)
      arg_count++;
    fprintf(stderr, "[KGPC_DEBUG_CALL_TYPES] call=%s args=%d\n", ctx->id,
            arg_count);
    for (ListNode_t *cur = ctx->expr->expr_data.function_call_data.args_expr;
         cur != NULL; cur = cur->next) {
      struct Expression *arg_expr = (struct Expression *)cur->cur;
      if (arg_expr != NULL)
        semcheck_debug_expr_brief(arg_expr, "IsDirectory arg");
    }
  }
  if (ctx->id != NULL && strcmp(ctx->id, "socket") == 0) {
#ifdef DEBUG
    fprintf(stderr,
            "DEBUG: semcheck_funccall processing socket call. line=%d\n",
            ctx->expr->line_num);
#endif
  }
  ctx->args_given = ctx->expr->expr_data.function_call_data.args_expr;

  if (ctx->expr->expr_data.function_call_data.is_method_call_placeholder &&
      ctx->args_given != NULL && ctx->args_given->cur != NULL) {
    struct Expression *first_arg = (struct Expression *)ctx->args_given->cur;
    int normalize_not_receiver = 0;
    if (first_arg->type == EXPR_RELOP &&
        first_arg->expr_data.relop_data.type == NOT &&
        first_arg->expr_data.relop_data.right == NULL &&
        first_arg->expr_data.relop_data.left != NULL) {
      struct Expression *receiver = first_arg->expr_data.relop_data.left;
      first_arg->expr_data.relop_data.left = NULL;
      ctx->args_given->cur = receiver;
      destroy_expr(first_arg);
      normalize_not_receiver = 1;
    } else if (semcheck_detach_unary_not_from_receiver(first_arg)) {
      normalize_not_receiver = 1;
    }

    if (normalize_not_receiver) {
      struct Expression *call_expr =
          (struct Expression *)calloc(1, sizeof(struct Expression));
      if (call_expr == NULL) {
        semcheck_error_with_context_at(ctx->expr->line_num, ctx->expr->col_num,
                                       ctx->expr->source_index,
                                       "Error on line %d: failed to allocate "
                                       "expression for NOT call fixup.\n",
                                       ctx->expr->line_num);
        *ctx->type_return = UNKNOWN_TYPE;
        do {
          ctx->final_status = 1;
          return FC_CLEANUP;
        } while (0);
      }

      *call_expr = *ctx->expr;
      ctx->expr->type = EXPR_RELOP;
      memset(&ctx->expr->expr_data.relop_data, 0,
             sizeof(ctx->expr->expr_data.relop_data));
      ctx->expr->expr_data.relop_data.type = NOT;
      ctx->expr->expr_data.relop_data.left = call_expr;
      ctx->expr->expr_data.relop_data.right = NULL;
      semcheck_expr_set_resolved_type(ctx->expr, UNKNOWN_TYPE);
      do {
        ctx->final_status =
            semcheck_relop(ctx->type_return, ctx->symtab, ctx->expr,
                           ctx->max_scope_lev, ctx->mutating);
        return FC_CLEANUP;
      } while (0);
    }
  }

  /* FPC Bootstrap Feature: Handle unit-qualified calls that the parser
   * represents as __Function(UnitName, Args...). Only strip the first
   * argument when the unit qualifier is unresolved AND the real function
   * (without "__") exists. */
  if (ctx->expr->expr_data.function_call_data.is_method_call_placeholder &&
      ctx->args_given != NULL) {
    struct Expression *first_arg = (struct Expression *)ctx->args_given->cur;
    /* For interface/class variables accessed via properties, the receiver
     * expression resolves to POINTER_TYPE with record_type == NULL.
     * Semcheck the receiver first so resolved_kgpc_type is available,
     * then extract the record from the pointer's pointee so method
     * dispatch can find the interface's methods. */
    if (first_arg != NULL && first_arg->record_type == NULL) {
      if (first_arg->resolved_kgpc_type == NULL) {
        (void)semcheck_expr_with_type(NULL, ctx->symtab, first_arg,
                                      ctx->max_scope_lev, NO_MUTATE);
      }
      if (first_arg->resolved_kgpc_type != NULL &&
          kgpc_type_is_pointer(first_arg->resolved_kgpc_type)) {
        KgpcType *pointee = kgpc_type_resolve_pointer_pointee(
            first_arg->resolved_kgpc_type, ctx->symtab);
        if (pointee != NULL && kgpc_type_is_record(pointee))
          first_arg->record_type = kgpc_type_get_record(pointee);
      }
    }
    if (first_arg != NULL && first_arg->record_type != NULL &&
        !record_type_is_class(first_arg->record_type)) {
      KgpcType *record_kgpc = create_record_type(first_arg->record_type);
      if (record_kgpc != NULL) {
        semcheck_expr_set_resolved_kgpc_type_shared(first_arg, record_kgpc);
        destroy_kgpc_type(record_kgpc);
      }
    }
    if (first_arg != NULL && first_arg->record_type != NULL &&
        !record_type_is_class(first_arg->record_type)) {
      const char *method_name =
          (ctx->expr->expr_data.function_call_data.placeholder_method_name !=
           NULL)
              ? ctx->expr->expr_data.function_call_data.placeholder_method_name
              : ctx->id;
      if (method_name != NULL) {
        struct RecordType *actual_method_owner = NULL;
        HashNode_t *method_node =
            semcheck_find_class_method(ctx->symtab, first_arg->record_type,
                                       method_name, &actual_method_owner);
        if (method_node != NULL) {
          set_type_from_hashtype(ctx->type_return, method_node);
          semcheck_expr_set_resolved_kgpc_type_shared(ctx->expr,
                                                      method_node->type);
          ctx->expr->expr_data.function_call_data.resolved_func = method_node;
          const char *resolved_method_name = (method_node->mangled_id != NULL)
                                                 ? method_node->mangled_id
                                                 : method_node->id;
          if (ctx->expr->expr_data.function_call_data.mangled_id != NULL)
            free(ctx->expr->expr_data.function_call_data.mangled_id);
          ctx->expr->expr_data.function_call_data.mangled_id =
              (resolved_method_name != NULL) ? strdup(resolved_method_name)
                                             : NULL;

          struct RecordType *record_for_mangling = (actual_method_owner != NULL)
                                                       ? actual_method_owner
                                                       : first_arg->record_type;
          char *mangled_method_name = NULL;
          if (record_for_mangling != NULL &&
              record_for_mangling->type_id != NULL) {
            size_t class_len = strlen(record_for_mangling->type_id);
            size_t method_len = strlen(method_name);
            mangled_method_name =
                (char *)malloc(class_len + 2 + method_len + 1);
            if (mangled_method_name != NULL)
              snprintf(mangled_method_name, class_len + 2 + method_len + 1,
                       "%s__%s", record_for_mangling->type_id, method_name);
          } else if (resolved_method_name != NULL) {
            mangled_method_name = strdup(resolved_method_name);
          }

          ListNode_t *method_candidates = NULL;
          if (mangled_method_name != NULL)
            method_candidates = FindAllIdents(ctx->symtab, mangled_method_name);

          if (ctx->mangled_name != NULL)
            free(ctx->mangled_name);
          ctx->mangled_name = NULL;
          if (mangled_method_name != NULL) {
            ctx->mangled_name = MangleFunctionNameFromCallSite(
                mangled_method_name, ctx->args_given, ctx->symtab,
                ctx->max_scope_lev);
            if (ctx->mangled_name == NULL)
              ctx->mangled_name = strdup(mangled_method_name);
          } else if (resolved_method_name != NULL) {
            ctx->mangled_name = strdup(resolved_method_name);
          }

          if (method_candidates != NULL)
            ctx->overload_candidates = method_candidates;
          else
            ctx->overload_candidates =
                CreateListNode(method_node, LIST_UNSPECIFIED);

          if (mangled_method_name != NULL)
            free(mangled_method_name);

          ctx->hash_return = method_node;
          return FC_OVERLOAD_RESOLVE;
        }
      }
    }
    if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
        first_arg->expr_data.id != NULL) {
      if (semcheck_is_unit_name(first_arg->expr_data.id)) {
        /* In Pascal, local variables and fields shadow unit names.
         * If the receiver name also exists as a local symbol (variable,
         * field, parameter), do NOT treat it as a unit qualifier — let
         * the method resolution handle it as a field access instead. */
        /* Check if the receiver name is also a local variable, field,
         * or parameter — in Pascal, local names shadow unit names.
         * Fields are accessible through Self/WITH context, so also
         * check if Self has a field with this name. */
        HashNode_t *local_sym = NULL;
        int receiver_is_local =
            (FindSymbol(&local_sym, ctx->symtab, first_arg->expr_data.id) !=
                 0 &&
             local_sym != NULL && local_sym->hash_type != HASHTYPE_TYPE);
        if (!receiver_is_local) {
          /* Check if Self has a field with this name */
          HashNode_t *self_node = NULL;
          if (FindSymbol(&self_node, ctx->symtab, "Self") != 0 &&
              self_node != NULL) {
            struct RecordType *self_rec = get_record_type_from_node(self_node);
            if (self_rec != NULL) {
              struct RecordField *self_field =
                  semcheck_find_class_field_including_hidden(
                      ctx->symtab, self_rec, first_arg->expr_data.id, NULL);
              if (self_field != NULL)
                receiver_is_local = 1;
            }
          }
        }

        if (!receiver_is_local) {
          char *real_func_name =
              (ctx->expr->expr_data.function_call_data
                   .placeholder_method_name != NULL)
                  ? strdup(ctx->expr->expr_data.function_call_data
                               .placeholder_method_name)
                  : NULL;
          if (real_func_name != NULL) {
            ListNode_t *func_candidates =
                FindAllIdents(ctx->symtab, real_func_name);
            if (func_candidates != NULL) {
              if (ctx->unit_qualifier_name == NULL &&
                  first_arg->expr_data.id != NULL)
                ctx->unit_qualifier_name = strdup(first_arg->expr_data.id);
              ListNode_t *remaining_args = ctx->args_given->next;
              destroy_expr(first_arg);
              ctx->args_given->cur = NULL;
              free(ctx->args_given);

              ctx->expr->expr_data.function_call_data.args_expr =
                  remaining_args;
              ctx->args_given = remaining_args;

              free(ctx->expr->expr_data.function_call_data.id);
              ctx->expr->expr_data.function_call_data.id = real_func_name;
              ctx->id = real_func_name;

              DestroyList(func_candidates);
              real_func_name = NULL;
              ctx->was_unit_qualified = 1;
              ctx->expr->expr_data.function_call_data
                  .is_method_call_placeholder = 0;
            }
            if (real_func_name != NULL)
              free(real_func_name);
          }
        }
      }
    }
  }

  /* Handle unit-qualified calls parsed as member access: UnitName.Func(args).
   * For expressions like ObjPas.TEndian(0), the parser creates a method call
   * with ObjPas as the receiver and TEndian as the function name.
   * If ObjPas is a unit name, strip it and check if this is a typecast. */
  if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && ctx->args_given != NULL) {
    struct Expression *first_arg = (struct Expression *)ctx->args_given->cur;
    fprintf(
        stderr,
        "[SemCheck] funccall id='%s' is_method_call_placeholder=%d "
        "first_arg_type=%d first_arg_id=%s\n",
        ctx->id ? ctx->id : "(null)",
        ctx->expr->expr_data.function_call_data.is_method_call_placeholder,
        first_arg ? first_arg->type : -1,
        (first_arg && first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id)
            ? first_arg->expr_data.id
            : "(null)");
  }
  if (ctx->expr->expr_data.function_call_data.is_method_call_placeholder &&
      ctx->args_given != NULL) {
    struct Expression *first_arg = (struct Expression *)ctx->args_given->cur;
    if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
        first_arg->expr_data.id != NULL) {
      /* Handle specialized generic type as receiver: specialize
       * T<A>.Method(...) The parser produces receiver="T$A" which may or may
       * not be in the symbol table, but the base type "T" is.  Look up the base
       * type and resolve the method through it (e.g. T$A__Method). */
      const char *dollar = strchr(first_arg->expr_data.id, '$');
      if (dollar != NULL && dollar > first_arg->expr_data.id &&
          ctx->expr->expr_data.function_call_data.placeholder_method_name !=
              NULL) {
        size_t base_len = (size_t)(dollar - first_arg->expr_data.id);
        char *base_name = strndup(first_arg->expr_data.id, base_len);
        if (base_name != NULL) {
          HashNode_t *base_type_node = NULL;
          if (FindSymbol(&base_type_node, ctx->symtab, base_name) != 0 &&
              base_type_node != NULL &&
              base_type_node->hash_type == HASHTYPE_TYPE) {
            const char *method_name =
                ctx->expr->expr_data.function_call_data.placeholder_method_name;
            /* Build mangled method name: "T$A__Method" using the full
             * specialized name */
            const char *spec_name = first_arg->expr_data.id;
            size_t spec_len = strlen(spec_name);
            size_t method_len = strlen(method_name);
            char *mangled_method =
                (char *)malloc(spec_len + 2 + method_len + 1);
            if (mangled_method != NULL) {
              snprintf(mangled_method, spec_len + 2 + method_len + 1, "%s__%s",
                       spec_name, method_name);
              ListNode_t *method_candidates =
                  FindAllIdents(ctx->symtab, mangled_method);
              if (method_candidates != NULL) {
                /* Strip the receiver arg and rewrite to direct call */
                ListNode_t *remaining_args = ctx->args_given->next;
                destroy_expr(first_arg);
                ctx->args_given->cur = NULL;
                free(ctx->args_given);

                ctx->expr->expr_data.function_call_data.args_expr =
                    remaining_args;
                ctx->args_given = remaining_args;

                free(ctx->expr->expr_data.function_call_data.id);
                ctx->expr->expr_data.function_call_data.id = mangled_method;
                ctx->id = mangled_method;
                mangled_method = NULL;

                DestroyList(method_candidates);
                ctx->was_unit_qualified = 1;
                ctx->expr->expr_data.function_call_data
                    .is_method_call_placeholder = 0;
              }
              if (mangled_method != NULL)
                free(mangled_method);
            }
          }
          free(base_name);
        }
      }
      int is_unit_qualifier = semcheck_is_unit_name(first_arg->expr_data.id);
      /* Local variables/parameters shadow unit names in Pascal.
       * If the identifier resolves as a variable, don't treat it
       * as a unit qualifier (e.g., 'node' parameter vs 'node' unit). */
      if (is_unit_qualifier) {
        HashNode_t *var_shadow = NULL;
        if (FindSymbol(&var_shadow, ctx->symtab, first_arg->expr_data.id) !=
                0 &&
            var_shadow != NULL &&
            (var_shadow->hash_type == HASHTYPE_VAR ||
             var_shadow->hash_type == HASHTYPE_CONST)) {
          is_unit_qualifier = 0;
        }
        /* Also check if Self has a field with this name — fields shadow
         * unit names in method bodies but aren't in the symbol table. */
        if (is_unit_qualifier) {
          HashNode_t *self_node2 = NULL;
          if (FindSymbol(&self_node2, ctx->symtab, "Self") != 0 &&
              self_node2 != NULL) {
            struct RecordType *self_rec2 =
                get_record_type_from_node(self_node2);
            if (self_rec2 != NULL &&
                semcheck_find_class_field_including_hidden(
                    ctx->symtab, self_rec2, first_arg->expr_data.id, NULL) !=
                    NULL) {
              is_unit_qualifier = 0;
            }
          }
        }
      }
      if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr,
                "[SemCheck] funccall unit-qual check: receiver=%s is_unit=%d\n",
                first_arg->expr_data.id, is_unit_qualifier);
      }
      if (!is_unit_qualifier) {
        int can_strip = 0;
        HashNode_t *first_node = NULL;
        if (FindSymbol(&first_node, ctx->symtab, first_arg->expr_data.id) ==
                0 ||
            first_node == NULL) {
          /* Before applying the lowercase heuristic, check if this
           * identifier is actually a field of Self.  Inside a method
           * body, class fields like 'fcount' are not top-level symbols
           * and would be wrongly stripped as unit qualifiers. */
          int looks_like_self_field = 0;
          HashNode_t *self_node = NULL;
          if (FindSymbol(&self_node, ctx->symtab, "Self") != 0 &&
              self_node != NULL) {
            struct RecordType *self_record =
                get_record_type_from_node(self_node);
            if (self_record != NULL &&
                semcheck_find_class_field_including_hidden(
                    ctx->symtab, self_record, first_arg->expr_data.id, NULL) !=
                    NULL) {
              looks_like_self_field = 1;
            }
          }

          if (!looks_like_self_field) {
            /* Keep unresolved unit-style qualifiers (typically lowercase)
             * working, but avoid stripping member-style names like
             * Size/TopLeft. */
            int looks_like_unit = 1;
            const char *recv = first_arg->expr_data.id;
            if (recv != NULL && recv[0] != '\0') {
              for (const char *p = recv; *p != '\0'; ++p) {
                if (*p >= 'A' && *p <= 'Z') {
                  looks_like_unit = 0;
                  break;
                }
              }
            } else {
              looks_like_unit = 0;
            }
            if (looks_like_unit)
              can_strip = 1;
          }
          /* Handle static method calls where the receiver type is not
           * directly in the symbol table (e.g. nested type used as a
           * generic type argument). If "Receiver__Method" exists as a
           * function and the method is static, rewrite to a direct call.
           * Non-static methods (constructors, etc.) need Self so we
           * must NOT strip the receiver for them. */
          if (!can_strip && !looks_like_self_field && ctx->id != NULL &&
              first_arg->expr_data.id != NULL &&
              from_cparser_is_method_static(first_arg->expr_data.id, ctx->id)) {
            size_t recv_len = strlen(first_arg->expr_data.id);
            size_t id_len = strlen(ctx->id);
            char *mangled_method = (char *)malloc(recv_len + 2 + id_len + 1);
            if (mangled_method != NULL) {
              snprintf(mangled_method, recv_len + 2 + id_len + 1, "%s__%s",
                       first_arg->expr_data.id, ctx->id);
              ListNode_t *method_candidates =
                  FindAllIdents(ctx->symtab, mangled_method);
              if (method_candidates != NULL) {
                free(ctx->expr->expr_data.function_call_data.id);
                ctx->expr->expr_data.function_call_data.id =
                    strdup(mangled_method);
                ctx->id = ctx->expr->expr_data.function_call_data.id;
                can_strip = 1;
                DestroyList(method_candidates);
              }
              free(mangled_method);
            }
          }
        } else if (first_node->hash_type == HASHTYPE_TYPE) {
          /* Type-qualified casts like UnitName.TypeName(...) can also strip. */
          HashNode_t *nested_type_node = NULL;
          if (ctx->id != NULL &&
              FindSymbol(&nested_type_node, ctx->symtab, ctx->id) != 0 &&
              nested_type_node != NULL &&
              nested_type_node->hash_type == HASHTYPE_TYPE) {
            can_strip = 1;
          }
          /* If the id is not a top-level type, check if it's a nested type
           * within the receiver type (e.g., HeapTracer.Node where Node is
           * a nested type declared inside HeapTracer). Nested types are
           * stored in the symbol table with qualified names like
           * "HeapTracer.Node". */
          if (!can_strip && ctx->id != NULL &&
              first_arg->expr_data.id != NULL) {
            size_t recv_len = strlen(first_arg->expr_data.id);
            size_t id_len = strlen(ctx->id);
            char *qualified_id = (char *)malloc(recv_len + 1 + id_len + 1);
            if (qualified_id != NULL) {
              snprintf(qualified_id, recv_len + 1 + id_len + 1, "%s.%s",
                       first_arg->expr_data.id, ctx->id);
              HashNode_t *qual_type_node = NULL;
              if (FindSymbol(&qual_type_node, ctx->symtab, qualified_id) != 0 &&
                  qual_type_node != NULL &&
                  qual_type_node->hash_type == HASHTYPE_TYPE) {
                free(ctx->expr->expr_data.function_call_data.id);
                ctx->expr->expr_data.function_call_data.id =
                    strdup(qualified_id);
                ctx->id = ctx->expr->expr_data.function_call_data.id;
                can_strip = 1;
              }
              free(qualified_id);
            }
          }
        }

        if (can_strip) {
          ListNode_t *func_candidates = FindAllIdents(ctx->symtab, ctx->id);
          if (func_candidates != NULL) {
            is_unit_qualifier = 1;
            DestroyList(func_candidates);
          }
          /* Qualified nested type names may not be found by FindAllIdents.
           * Fall back to FindSymbol since we already verified the type. */
          if (!is_unit_qualifier) {
            HashNode_t *type_verify = NULL;
            if (FindSymbol(&type_verify, ctx->symtab, ctx->id) != 0 &&
                type_verify != NULL &&
                type_verify->hash_type == HASHTYPE_TYPE) {
              is_unit_qualifier = 1;
            }
          }
        }
      }
      if (is_unit_qualifier) {
        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
          fprintf(stderr,
                  "[SemCheck] funccall unit-qual strip: receiver=%s id=%s\n",
                  first_arg->expr_data.id,
                  ctx->id != NULL ? ctx->id : "(null)");
        }
        if (ctx->unit_qualifier_name == NULL && first_arg->expr_data.id != NULL)
          ctx->unit_qualifier_name = strdup(first_arg->expr_data.id);
        ListNode_t *remaining_args = ctx->args_given->next;
        destroy_expr(first_arg);
        ctx->args_given->cur = NULL;
        free(ctx->args_given);

        ctx->expr->expr_data.function_call_data.args_expr = remaining_args;
        ctx->args_given = remaining_args;
        ctx->expr->expr_data.function_call_data.is_method_call_placeholder = 0;
        ctx->was_unit_qualified = 1;

        /* After stripping the unit prefix, immediately check if this is a
         * typecast. This handles unit-qualified typecasts like
         * ObjPas.TEndian(0). */
        int typecast_result = semcheck_try_reinterpret_as_typecast(
            ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev);
        if (typecast_result != 0 || ctx->expr->type == EXPR_TYPECAST)
          do {
            ctx->final_status = typecast_result;
            return FC_CLEANUP;
          } while (0);
      }
    }
  }

  return FC_SELF;
}
