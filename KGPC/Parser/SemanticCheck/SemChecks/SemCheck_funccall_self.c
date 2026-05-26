#include "SemCheck_Expr_Internal.h"
#include "SemCheck_stmt.h"
#include <time.h>
#include <ctype.h>
#include <limits.h>
#include "SemCheck_funccall_internal.h"

FunccallState funccall_state_self(FunccallCtx *ctx) {
  /* Normalize @array[idx] precedence for CompareMem.
   * If the parser produced ARRAY_ACCESS(ADDR(array), idx), rewrite it to
   * ADDR(ARRAY_ACCESS(array, idx)) so pointer arguments resolve correctly. */
  if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "CompareMem") &&
      ctx->args_given != NULL) {
    ListNode_t *arg_node = ctx->args_given;
    while (arg_node != NULL) {
      struct Expression *arg_expr = (struct Expression *)arg_node->cur;
      if (arg_expr != NULL && arg_expr->type == EXPR_ARRAY_ACCESS &&
          arg_expr->expr_data.array_access_data.array_expr != NULL &&
          arg_expr->expr_data.array_access_data.array_expr->type == EXPR_ADDR) {
        struct Expression *addr_expr =
            arg_expr->expr_data.array_access_data.array_expr;
        struct Expression *addr_inner = addr_expr->expr_data.addr_data.expr;
        /* Rewire: @ (array[idx]) */
        arg_expr->expr_data.array_access_data.array_expr = addr_inner;
        addr_expr->expr_data.addr_data.expr = arg_expr;
        arg_node->cur = addr_expr;
      }
      arg_node = arg_node->next;
    }
  }

  if (kgpc_getenv("KGPC_DEBUG_FORMAT") != NULL && ctx->id != NULL &&
      pascal_identifier_equals(ctx->id, "Format")) {
    fprintf(stderr,
            "[KGPC_DEBUG_FORMAT] pre-self-injection line=%d args_ptr=%p\n",
            ctx->expr->line_num, (void *)ctx->args_given);
    int idx = 0;
    for (ListNode_t *cur = ctx->args_given; cur != NULL;
         cur = cur->next, idx++) {
      struct Expression *arg_expr = (struct Expression *)cur->cur;
      const char *arg_id = (arg_expr != NULL && arg_expr->type == EXPR_VAR_ID)
                               ? arg_expr->expr_data.id
                               : NULL;
      fprintf(stderr,
              "[KGPC_DEBUG_FORMAT] pre args[%d]=node=%p expr=%p type=%d "
              "line=%d id=%s\n",
              idx, (void *)cur, (void *)arg_expr,
              arg_expr != NULL ? arg_expr->type : -1,
              arg_expr != NULL ? arg_expr->line_num : -1,
              arg_id != NULL ? arg_id : "(null)");
    }
  }

  /* If no explicit receiver was provided (not a method call placeholder), but
   * Self is in scope and defines this method, prepend Self so unqualified
   * method calls resolve correctly. */
  if (!ctx->is_operator_dispatch && !ctx->was_unit_qualified &&
      ctx->id != NULL &&
      !ctx->expr->expr_data.function_call_data.is_method_call_placeholder) {
    HashNode_t *global_node = NULL;
    if (FindSymbol(&global_node, ctx->symtab, ctx->id) != 0 &&
        global_node != NULL &&
        (global_node->hash_type == HASHTYPE_FUNCTION ||
         global_node->hash_type == HASHTYPE_PROCEDURE) &&
        global_node->owner_class == NULL) {
      const char *current_owner = semcheck_get_current_method_owner();
      if (current_owner != NULL) {
        /* Inside a type helper method, if the first explicit arg is
         * literally "Self", the caller wants the standalone function
         * (e.g. Format(Self, Args) in syshelps.inc).  Skip Self
         * injection so the standalone overload matches. */
        struct RecordType *owner_rec =
            semcheck_lookup_record_type(ctx->symtab, current_owner);
        if (owner_rec != NULL && owner_rec->is_type_helper) {
          struct Expression *first_arg_expr =
              (ctx->args_given != NULL)
                  ? (struct Expression *)ctx->args_given->cur
                  : NULL;
          int first_is_self =
              (first_arg_expr != NULL && first_arg_expr->type == EXPR_VAR_ID &&
               first_arg_expr->expr_data.id != NULL &&
               pascal_identifier_equals(first_arg_expr->expr_data.id, "Self"));
          if (first_is_self)
            goto skip_self_injection_internal;
        }
        if (kgpc_getenv("KGPC_DEBUG_CHECKOBS") != NULL && ctx->id != NULL &&
            pascal_identifier_equals(ctx->id, "CheckObserving")) {
          fprintf(stderr,
                  "[KGPC_DEBUG_CHECKOBS] global present but in method "
                  "owner=%s; continuing self lookup\n",
                  current_owner);
        }
      } else {
        if (kgpc_getenv("KGPC_DEBUG_CHECKOBS") != NULL && ctx->id != NULL &&
            pascal_identifier_equals(ctx->id, "CheckObserving")) {
          fprintf(stderr,
                  "[KGPC_DEBUG_CHECKOBS] skip_self_injection global_node=%p "
                  "owner_class=%s method_name=%s owner_full=%s\n",
                  (void *)global_node,
                  global_node->owner_class != NULL ? global_node->owner_class
                                                   : "(null)",
                  global_node->method_name != NULL ? global_node->method_name
                                                   : "(null)",
                  global_node->owner_class_full != NULL
                      ? global_node->owner_class_full
                      : "(null)");
        }
        if (kgpc_getenv("KGPC_DEBUG_TRIM") != NULL &&
            pascal_identifier_equals(ctx->id, "Trim")) {
          fprintf(
              stderr,
              "[KGPC_DEBUG_TRIM] skipping Self injection due to global %s\n",
              ctx->id);
        }
        goto skip_self_injection_internal;
      }
    }
    HashNode_t *self_node = NULL;
    int self_found = 0;
    if (FindSymbol(&self_node, ctx->symtab, "Self") != 0 && self_node != NULL) {
      self_found = 1;
      struct RecordType *self_record = get_record_type_from_node(self_node);
      int self_is_helper = 0;
      if (kgpc_getenv("KGPC_DEBUG_TRIM") != NULL && ctx->id != NULL &&
          pascal_identifier_equals(ctx->id, "Trim")) {
        fprintf(stderr,
                "[KGPC_DEBUG_TRIM] Self found: node_type_kind=%d record=%s "
                "is_helper=%d current_owner=%s\n",
                self_node->type != NULL ? self_node->type->kind : -1,
                self_record != NULL && self_record->type_id != NULL
                    ? self_record->type_id
                    : "(null)",
                self_record != NULL ? self_record->is_type_helper : -1,
                semcheck_get_current_method_owner() != NULL
                    ? semcheck_get_current_method_owner()
                    : "(null)");
      }
      if (self_record == NULL) {
        int self_type_tag = UNKNOWN_TYPE;
        const char *self_type_name = NULL;
        set_type_from_hashtype(&self_type_tag, self_node);
        if (self_node->type != NULL && self_node->type->type_alias != NULL &&
            self_node->type->type_alias->target_type_id != NULL) {
          self_type_name = self_node->type->type_alias->target_type_id;
        }
        struct RecordType *helper_record =
            semcheck_lookup_type_helper_for_member(ctx->symtab, self_type_tag,
                                                   self_type_name, ctx->id);
        if (helper_record != NULL) {
          self_record = helper_record;
          self_is_helper = 1;
        }
        if (self_record == NULL) {
          const char *current_owner = semcheck_get_current_method_owner();
          if (current_owner != NULL) {
            struct RecordType *owner_record =
                semcheck_lookup_record_type(ctx->symtab, current_owner);
            if (owner_record != NULL) {
              self_record = owner_record;
              self_is_helper = owner_record->is_type_helper ? 1 : 0;
            }
          }
        }
      }

      /* If Self lookup returns a different class than expected (e.g., TBase
       * instead of TDerived when we're in a TDerived method), try to find the
       * correct class from the scope. */
      if (self_record != NULL) {
        if (self_is_helper) {
          /* For type helpers, we need to let both function/method calls and
           * typecast handling proceed through normal resolution.
           * The code below handles method lookup, and typecast handling
           * is done later in semcheck_try_reinterpret_as_typecast. */
        }
        if (self_is_helper && ctx->args_given != NULL) {
          struct Expression *first_arg =
              (struct Expression *)ctx->args_given->cur;
          if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
              first_arg->expr_data.id != NULL &&
              pascal_identifier_equals(first_arg->expr_data.id, "Self")) {
            /* Check if this might be a typecast like TSingleRec(Self).
             * If the identifier is a type, fall through to typecast handling.
             * If it's a function/procedure that exists as a standalone
             * (owner_class == NULL), the caller is explicitly passing Self
             * (e.g. Format(Self, Args) in syshelps.inc) - skip Self
             * injection so the standalone overload matches. */
            HashNode_t *id_node = NULL;
            if (FindSymbol(&id_node, ctx->symtab, ctx->id) && id_node != NULL) {
              if (id_node->hash_type == HASHTYPE_TYPE) {
                /* This is a typecast like TSingleRec(Self), fall through */
              } else if (id_node->hash_type == HASHTYPE_FUNCTION ||
                         id_node->hash_type == HASHTYPE_PROCEDURE) {
                /* Check if ANY overload is a standalone function */
                ListNode_t *all_idents = FindAllIdents(ctx->symtab, ctx->id);
                ListNode_t *cur_ident = all_idents;
                int has_standalone = 0;
                while (cur_ident != NULL) {
                  HashNode_t *cand = (HashNode_t *)cur_ident->cur;
                  if (cand != NULL && cand->owner_class == NULL &&
                      (cand->hash_type == HASHTYPE_FUNCTION ||
                       cand->hash_type == HASHTYPE_PROCEDURE)) {
                    has_standalone = 1;
                    break;
                  }
                  cur_ident = cur_ident->next;
                }
                if (all_idents != NULL)
                  DestroyList(all_idents);
                if (has_standalone)
                  goto skip_self_injection_internal;
              }
            }
          }
        }
        /* First, try to find the method in Self's class.
         * For overloaded methods, we need to check ALL overloads to find one
         * that matches our argument count. */
        HashNode_t *method_node = NULL;
        int args_count = ListLength(ctx->args_given);
        int expects_self = 0;
        ListNode_t *method_params = NULL;
        int early_overload_resolved =
            0; /* set when type-aware resolution succeeded */

        /* Build the mangled method name */
        char mangled_method_name[256];
        if (self_record->type_id != NULL) {
          snprintf(mangled_method_name, sizeof(mangled_method_name), "%s__%s",
                   self_record->type_id, ctx->id);

          /* Get ALL overloads of this method across the class hierarchy.
           * Overloads may be split between child and parent classes. */
          ListNode_t *all_methods = semcheck_collect_hierarchy_method_overloads(
              ctx->symtab, self_record, ctx->id);
          /* If not found and self_record->type_id differs from the current
           * method owner (e.g. record has "timezone" but owner is "TTimeZone"),
           * retry with the owner name. */
          if (all_methods == NULL) {
            const char *cur_owner = semcheck_get_current_method_owner();
            if (cur_owner != NULL &&
                !pascal_identifier_equals(cur_owner, self_record->type_id)) {
              snprintf(mangled_method_name, sizeof(mangled_method_name),
                       "%s__%s", cur_owner, ctx->id);
              all_methods = FindAllIdents(ctx->symtab, mangled_method_name);
            }
          }
          if (all_methods != NULL) {
            /* Use full overload resolution (type-aware) to pick
             * the best matching overload. */
            HashNode_t *best_candidate = NULL;
            int num_best = 0;
            struct Expression call_stub;
            memset(&call_stub, 0, sizeof(call_stub));
            call_stub.line_num = ctx->expr->line_num;
            call_stub.type = EXPR_FUNCTION_CALL;

            int overload_status = semcheck_resolve_overload(
                &best_candidate, &num_best, all_methods, ctx->args_given,
                ctx->symtab, &call_stub, ctx->max_scope_lev, 0);

            if (overload_status == 0 && best_candidate != NULL &&
                num_best == 1) {
              method_node = best_candidate;
              early_overload_resolved = 1;
              method_params =
                  kgpc_type_get_procedure_params(best_candidate->type);
              /* Check if the first formal param is Self */
              if (method_params != NULL) {
                Tree_t *first_formal = (Tree_t *)method_params->cur;
                if (first_formal != NULL &&
                    first_formal->type == TREE_VAR_DECL &&
                    first_formal->tree_data.var_decl_data.ids != NULL) {
                  const char *first_id =
                      (const char *)
                          first_formal->tree_data.var_decl_data.ids->cur;
                  if (first_id != NULL &&
                      pascal_identifier_equals(first_id, "Self"))
                    expects_self = 1;
                }
              }
            } else {
              /* Fallback to arity-only check when type resolution fails */
              ListNode_t *cur = all_methods;
              int best_total = INT_MAX;
              while (cur != NULL) {
                HashNode_t *candidate = (HashNode_t *)cur->cur;
                if (candidate != NULL &&
                    (candidate->hash_type == HASHTYPE_FUNCTION ||
                     candidate->hash_type == HASHTYPE_PROCEDURE) &&
                    candidate->type != NULL) {
                  ListNode_t *candidate_params =
                      kgpc_type_get_procedure_params(candidate->type);
                  int candidate_expects_self = 0;
                  int candidate_compatible = semcheck_method_accepts_arg_count(
                      candidate_params, args_count, &candidate_expects_self,
                      candidate->is_varargs);
                  int candidate_total =
                      semcheck_count_total_params(candidate_params);

                  if (candidate_compatible && candidate_total < best_total) {
                    method_node = candidate;
                    method_params = candidate_params;
                    expects_self = candidate_expects_self;
                    best_total = candidate_total;
                  }
                }
                cur = cur->next;
              }
            }
            DestroyList(all_methods);
          }
        }

        /* Fallback: use semcheck_find_class_method if no match found via
         * overloads */
        if (method_node == NULL) {
          method_node = semcheck_find_class_method(ctx->symtab, self_record,
                                                   ctx->id, NULL);

          /* Check if the method was found but has wrong parameter count (0
           * params = forward decl) */
          int method_params_len = 0;
          if (method_node != NULL && method_node->type != NULL) {
            method_params = kgpc_type_get_procedure_params(method_node->type);
            method_params_len = semcheck_count_total_params(method_params);
            int found_compatible = semcheck_method_accepts_arg_count(
                method_params, args_count, &expects_self,
                method_node->is_varargs);
            if (!found_compatible)
              method_node = NULL;
          }

          if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr,
                    "[SemCheck] Method check (fallback): method_node=%p "
                    "method_params_len=%d\n",
                    (void *)method_node, method_params_len);
          }

          /* If method not found, or has wrong params (0 params = forward
           * declaration), try looking up the method using class_method_bindings
           */
          if (method_node == NULL || method_params_len == 0) {
            int class_count = 0;
            int found_correct_method = 0;
            ListNode_t *class_list = from_cparser_find_classes_with_method(
                (char *)ctx->id, &class_count);
            if (class_list != NULL) {
              ListNode_t *cur_class = class_list;
              while (cur_class != NULL) {
                char *class_name = (char *)cur_class->cur;
                if (class_name != NULL) {
                  if (self_record != NULL && self_record->type_id != NULL) {
                    int in_self_chain = 0;
                    const char *check_name = self_record->type_id;
                    struct RecordType *check_record = self_record;
                    while (check_name != NULL) {
                      if (strcasecmp(check_name, class_name) == 0) {
                        in_self_chain = 1;
                        break;
                      }
                      const char *parent = (check_record != NULL)
                                               ? check_record->parent_class_name
                                               : NULL;
                      if (parent == NULL)
                        break;
                      check_record =
                          semcheck_lookup_record_type(ctx->symtab, parent);
                      check_name = parent;
                    }
                    if (!in_self_chain) {
                      ListNode_t *next_class = cur_class->next;
                      free(cur_class->cur);
                      cur_class = next_class;
                      continue;
                    }
                  }
                  /* Look up this class */
                  HashNode_t *class_node = NULL;
                  if (FindSymbol(&class_node, ctx->symtab, class_name) &&
                      class_node != NULL) {
                    struct RecordType *correct_record =
                        get_record_type_from_node(class_node);
                    if (correct_record != NULL) {
                      /* Don't use semcheck_find_class_method because it walks
                       * up inheritance and finds forward declarations in parent
                       * classes. Instead, look for the exact mangled name
                       * directly. */
                      char local_mangled_name[256];
                      snprintf(local_mangled_name, sizeof(local_mangled_name),
                               "%s__%s", class_name, (char *)ctx->id);

                      HashNode_t *correct_method = NULL;
                      FindSymbol(&correct_method, ctx->symtab,
                                 local_mangled_name);

                      /* Check if the correct method has proper parameters */
                      int correct_params_len = 0;
                      int correct_expects_self = 0;
                      int correct_compatible = 0;
                      if (correct_method != NULL &&
                          correct_method->type != NULL) {
                        ListNode_t *correct_params =
                            kgpc_type_get_procedure_params(
                                correct_method->type);
                        correct_params_len =
                            semcheck_count_total_params(correct_params);
                        correct_compatible = semcheck_method_accepts_arg_count(
                            correct_params, args_count, &correct_expects_self,
                            correct_method->is_varargs);
                      }

                      if (correct_method != NULL && correct_params_len > 0 &&
                          correct_compatible) {
                        self_record = correct_record;
                        method_node = correct_method;
                        expects_self = correct_expects_self;
                        found_correct_method = 1;
                        break;
                      }
                    }
                  }
                }
                ListNode_t *next_class = cur_class->next;
                free(cur_class->cur);
                cur_class = next_class;
              }
              /* Only destroy list if we haven't already (i.e., if we didn't
               * break early) */
              if (!found_correct_method && class_list != NULL) {
                DestroyList(class_list);
              }
            }
          }
        }

        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
          fprintf(stderr,
                  "[SemCheck] Self injection: Self found, self_record=%s, "
                  "method=%s, method_node=%p expects_self=%d\n",
                  self_record->type_id ? self_record->type_id : "(null)",
                  ctx->id ? ctx->id : "(null)", (void *)method_node,
                  expects_self);
        }
        if (kgpc_getenv("KGPC_DEBUG_CHECKOBS") != NULL && ctx->id != NULL &&
            pascal_identifier_equals(ctx->id, "CheckObserving")) {
          fprintf(
              stderr,
              "[KGPC_DEBUG_CHECKOBS] self_found=%d self_record=%s "
              "args_count=%d expects_self=%d method_node=%p placeholder=%d\n",
              self_found,
              self_record != NULL && self_record->type_id != NULL
                  ? self_record->type_id
                  : "(null)",
              args_count, expects_self, (void *)method_node,
              ctx->expr->expr_data.function_call_data
                  .is_method_call_placeholder);
        }

        if (method_node != NULL) {
          int method_is_overloaded = 0;
          /* When the early type-aware overload resolution already
           * picked a winner using the full class hierarchy, skip
           * the overloaded-method deferral.  The early resolution
           * is authoritative; deferring to the final resolution
           * loses parent-class candidates due to scope filtering. */
          if (!early_overload_resolved && self_record != NULL &&
              self_record->type_id != NULL && ctx->id != NULL) {
            char overload_name[256];
            snprintf(overload_name, sizeof(overload_name), "%s__%s",
                     self_record->type_id, ctx->id);
            ListNode_t *method_overloads =
                FindAllIdents(ctx->symtab, overload_name);
            if (method_overloads != NULL) {
              method_is_overloaded = (method_overloads->next != NULL);
              if (method_is_overloaded)
                ctx->overload_candidates = method_overloads;
              else
                DestroyList(method_overloads);
            }
          }
          if (method_is_overloaded && ctx->mangled_name != NULL) {
            free(ctx->mangled_name);
            ctx->mangled_name = NULL;
          }

          const char *method_owner_name = method_node->owner_class;
          if (method_owner_name == NULL && self_record != NULL)
            method_owner_name = self_record->type_id;

          if (method_owner_name != NULL && ctx->id != NULL &&
              from_cparser_is_method_static(method_owner_name, ctx->id)) {
            expects_self = 0;
          }
          if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr,
                    "[SemCheck] Implicit Self injection? method_params_len=%d "
                    "mangled=%s\n",
                    semcheck_count_total_params(method_params),
                    method_node->mangled_id ? method_node->mangled_id
                                            : "(null)");
          }
          int already_has_self = 0;
          if (ctx->args_given != NULL && ctx->args_given->cur != NULL) {
            struct Expression *first_arg =
                (struct Expression *)ctx->args_given->cur;
            if (first_arg->type == EXPR_VAR_ID &&
                first_arg->expr_data.id != NULL &&
                pascal_identifier_equals(first_arg->expr_data.id, "Self")) {
              already_has_self = 1;
            }
          }
          if (expects_self && !already_has_self) {
            const char *self_arg_name = "Self";
            if (!self_found && self_record != NULL &&
                record_type_is_class(self_record) &&
                !self_record->is_type_helper && self_record->type_id != NULL) {
              self_arg_name = self_record->type_id;
            }
            struct Expression *self_expr =
                mk_varid(ctx->expr->line_num, strdup(self_arg_name));
            if (kgpc_getenv("KGPC_DEBUG_CHECKOBS") != NULL && ctx->id != NULL &&
                pascal_identifier_equals(ctx->id, "CheckObserving")) {
              fprintf(stderr,
                      "[KGPC_DEBUG_CHECKOBS] injecting self arg name=%s "
                      "self_record=%s self_node_type=%d\n",
                      self_arg_name,
                      self_record != NULL && self_record->type_id != NULL
                          ? self_record->type_id
                          : "(null)",
                      self_node != NULL && self_node->type != NULL
                          ? self_node->type->kind
                          : -1);
            }
            if (self_expr != NULL && self_record != NULL &&
                self_record->is_type_helper &&
                self_record->helper_base_type_id != NULL) {
              HashNode_t *base_node = semcheck_find_preferred_type_node(
                  ctx->symtab, self_record->helper_base_type_id);
              if (kgpc_getenv("KGPC_DEBUG_FORMAT") != NULL && ctx->id != NULL &&
                  pascal_identifier_equals(ctx->id, "Format")) {
                fprintf(stderr,
                        "[KGPC_DEBUG_FORMAT] helper base=%s node=%p type=%p\n",
                        self_record->helper_base_type_id != NULL
                            ? self_record->helper_base_type_id
                            : "(null)",
                        (void *)base_node,
                        base_node != NULL ? (void *)base_node->type : NULL);
              }
              if (base_node != NULL && base_node->type != NULL)
                semcheck_expr_set_resolved_kgpc_type_shared(self_expr,
                                                            base_node->type);
            }
            if (self_expr != NULL && self_record != NULL &&
                pascal_identifier_equals(self_arg_name, "Self")) {
              if (self_node != NULL && self_node->type != NULL) {
                semcheck_expr_set_resolved_kgpc_type_shared(self_expr,
                                                            self_node->type);
              } else {
                KgpcType *self_record_type = create_record_type(self_record);
                if (self_record_type != NULL) {
                  semcheck_expr_set_resolved_kgpc_type_shared(self_expr,
                                                              self_record_type);
                  destroy_kgpc_type(self_record_type);
                }
              }
              if (kgpc_getenv("KGPC_DEBUG_CHECKOBS") != NULL &&
                  ctx->id != NULL &&
                  pascal_identifier_equals(ctx->id, "CheckObserving")) {
                fprintf(stderr,
                        "[KGPC_DEBUG_CHECKOBS] self arg resolved_type=%d\n",
                        self_expr->resolved_kgpc_type != NULL
                            ? self_expr->resolved_kgpc_type->kind
                            : -1);
              }
            }
            ListNode_t *self_arg = CreateListNode(self_expr, LIST_EXPR);
            self_arg->next = ctx->args_given;
            ctx->expr->expr_data.function_call_data.args_expr = self_arg;
            ctx->args_given = self_arg;
            ctx->injected_self = 1;
            if (kgpc_getenv("KGPC_DEBUG_FORMAT") != NULL && ctx->id != NULL &&
                pascal_identifier_equals(ctx->id, "Format")) {
              fprintf(
                  stderr,
                  "[KGPC_DEBUG_FORMAT] injected Self for method=%s owner=%s\n",
                  ctx->id,
                  method_owner_name != NULL ? method_owner_name : "(null)");
            }
            if (kgpc_getenv("KGPC_DEBUG_TRIM") != NULL && ctx->id != NULL &&
                pascal_identifier_equals(ctx->id, "Trim")) {
              fprintf(
                  stderr,
                  "[KGPC_DEBUG_TRIM] injected Self for method=%s owner=%s\n",
                  ctx->id,
                  method_owner_name != NULL ? method_owner_name : "(null)");
            }
          }
          if (!method_is_overloaded &&
              ctx->expr->expr_data.function_call_data.resolved_func == NULL)
            ctx->expr->expr_data.function_call_data.resolved_func = method_node;
          if (!method_is_overloaded &&
              ctx->expr->expr_data.function_call_data.mangled_id == NULL) {
            const char *resolved_name =
                method_node->mangled_id
                    ? method_node->mangled_id
                    : (method_node->id ? method_node->id : ctx->id);
            if (resolved_name != NULL)
              ctx->expr->expr_data.function_call_data.mangled_id =
                  strdup(resolved_name);
          }
          /* Set call_kgpc_type for correct calling convention (e.g., float Self
           * in xmm0) */
          if (!method_is_overloaded && method_node->type != NULL) {
            semcheck_expr_set_call_kgpc_type(ctx->expr, method_node->type, 0);
            ctx->expr->expr_data.function_call_data.call_hash_type =
                method_node->hash_type;
            ctx->expr->expr_data.function_call_data.is_call_info_valid = 1;
          }
          /* Check if this is a virtual method call that needs VMT dispatch.
           * Only set for instance method calls (expects_self) since class
           * methods use a different VMT dispatch convention (single
           * indirection). */
          if (expects_self) {
            const char *class_name = self_record->type_id;
            int method_param_count = -1;
            if (method_node != NULL && method_node->type != NULL &&
                method_node->type->kind == TYPE_KIND_PROCEDURE) {
              method_param_count =
                  ListLength(method_node->type->info.proc_info.params);
              if (method_node->owner_class != NULL &&
                  !from_cparser_is_method_static(
                      method_node->owner_class, method_node->method_name != NULL
                                                    ? method_node->method_name
                                                    : ctx->id)) {
                if (method_param_count > 0)
                  method_param_count -= 1;
                else
                  method_param_count = 0;
              }
            }
            if (class_name != NULL &&
                !ctx->expr->expr_data.function_call_data.is_inherited_call &&
                from_cparser_is_method_virtual_with_types(
                    class_name, ctx->id, method_param_count, NULL, 0) &&
                !from_cparser_is_method_static(class_name, ctx->id)) {
              ctx->expr->expr_data.function_call_data.is_virtual_call = 1;
              int vmt_index = -1;
              if (self_record->methods != NULL) {
                ListNode_t *method_entry = self_record->methods;
                while (method_entry != NULL) {
                  struct MethodInfo *info =
                      (struct MethodInfo *)method_entry->cur;
                  if (info != NULL && info->name != NULL &&
                      (info->is_virtual || info->is_override) &&
                      strcasecmp(info->name, ctx->id) == 0) {
                    if (method_param_count >= 0 && info->param_count >= 0 &&
                        method_param_count != info->param_count) {
                      method_entry = method_entry->next;
                      continue;
                    }
                    vmt_index = info->vmt_index;
                    break;
                  }
                  method_entry = method_entry->next;
                }
              }
              ctx->expr->expr_data.function_call_data.vmt_index = vmt_index;
              if (ctx->expr->expr_data.function_call_data.self_class_name ==
                  NULL)
                ctx->expr->expr_data.function_call_data.self_class_name =
                    strdup(class_name);
              if (ctx->expr->expr_data.function_call_data.cached_owner_class ==
                  NULL)
                ctx->expr->expr_data.function_call_data.cached_owner_class =
                    strdup(class_name);
              if (ctx->expr->expr_data.function_call_data.cached_method_name ==
                  NULL)
                ctx->expr->expr_data.function_call_data.cached_method_name =
                    strdup(ctx->id);
            }
            /* Mark class method calls so codegen passes VMT as Self.
             * Walk the parent class chain since the method may be
             * inherited (e.g., TA.ClassName where ClassName is on TObject). */
            {
              const char *check_class = class_name;
              struct RecordType *check_record = self_record;
              while (check_class != NULL) {
                if (from_cparser_is_method_nonstatic_class_method(check_class,
                                                                  ctx->id)) {
                  ctx->expr->expr_data.function_call_data.is_class_method_call =
                      1;
                  break;
                }
                /* Move to parent class */
                const char *parent = (check_record != NULL)
                                         ? check_record->parent_class_name
                                         : NULL;
                if (parent == NULL)
                  break;
                check_record = semcheck_lookup_record_type(ctx->symtab, parent);
                check_class = parent;
              }
            }
          }
        }
      } else if (kgpc_getenv("KGPC_DEBUG_CHECKOBS") != NULL &&
                 ctx->id != NULL &&
                 pascal_identifier_equals(ctx->id, "CheckObserving")) {
        const char *owner = semcheck_get_current_method_owner();
        fprintf(
            stderr,
            "[KGPC_DEBUG_CHECKOBS] self_not_found current_owner=%s "
            "placeholder=%d\n",
            owner != NULL ? owner : "(null)",
            ctx->expr->expr_data.function_call_data.is_method_call_placeholder);
      }
    }
  }

  if (ctx->id != NULL &&
      strncmp(ctx->id, "__tfpg_ctor$", strlen("__tfpg_ctor$")) == 0) {
    if (ctx->type_return != NULL)
      *ctx->type_return = RECORD_TYPE;
    semcheck_expr_set_resolved_type(ctx->expr, RECORD_TYPE);
    do {
      ctx->final_status = 0;
      return FC_CLEANUP;
    } while (0);
  }

  /* If this "call" is actually a type identifier, treat it as a typecast */
  int typecast_result = semcheck_try_reinterpret_as_typecast(
      ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev);
  if (typecast_result != 0 || ctx->expr->type == EXPR_TYPECAST)
    do {
      ctx->final_status = typecast_result;
      return FC_CLEANUP;
    } while (0);

  /* Detect calls through procedural fields of records (advanced records). The
   * parser may have rewritten `algo.Compare(x, y)` as a method call with `algo`
   * injected as the first argument. If the field is a procedural type, treat it
   * as a procedural variable call instead. */
  if (ctx->id != NULL && ctx->args_given != NULL) {
    struct Expression *receiver_expr =
        (struct Expression *)ctx->args_given->cur;
    int recv_type = UNKNOWN_TYPE;
    KgpcType *recv_kgpc_type = NULL;
    semcheck_expr_with_type(&recv_kgpc_type, ctx->symtab, receiver_expr,
                            ctx->max_scope_lev, NO_MUTATE);
    recv_type = semcheck_tag_from_kgpc(recv_kgpc_type);

    struct RecordType *recv_record = NULL;
    if (recv_type == RECORD_TYPE) {
      if (receiver_expr->resolved_kgpc_type != NULL &&
          kgpc_type_is_record(receiver_expr->resolved_kgpc_type))
        recv_record = kgpc_type_get_record(receiver_expr->resolved_kgpc_type);
    } else if (recv_type == POINTER_TYPE) {
      if (receiver_expr->resolved_kgpc_type != NULL &&
          receiver_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER) {
        KgpcType *pointee = receiver_expr->resolved_kgpc_type->info.points_to;
        if (pointee != NULL && kgpc_type_is_record(pointee))
          recv_record = kgpc_type_get_record(pointee);
      }
    }
    if (recv_record == NULL && receiver_expr->type == EXPR_VAR_ID &&
        receiver_expr->expr_data.id != NULL) {
      HashNode_t *recv_node = NULL;
      if (FindSymbol(&recv_node, ctx->symtab, receiver_expr->expr_data.id) !=
              0 &&
          recv_node != NULL) {
        recv_record = get_record_type_from_node(recv_node);
        if (recv_record == NULL && recv_node->type != NULL &&
            recv_node->type->kind == TYPE_KIND_POINTER &&
            recv_node->type->info.points_to != NULL &&
            kgpc_type_is_record(recv_node->type->info.points_to)) {
          recv_record = kgpc_type_get_record(recv_node->type->info.points_to);
        }
      }
    }

    if (recv_record != NULL) {
      const char *field_lookup = ctx->id;
      while (field_lookup != NULL && field_lookup[0] == '_' &&
             field_lookup[1] == '_')
        field_lookup +=
            2; /* allow __Prefixed identifiers to match field names */

      struct RecordField *field_desc = NULL;
      long long field_offset = 0;
      if (resolve_record_field(ctx->symtab, recv_record, field_lookup,
                               &field_desc, &field_offset, ctx->expr->line_num,
                               1) == 0 &&
          field_desc != NULL) {
        int is_proc_field = (field_desc->type == PROCEDURE);
        KgpcType *proc_type = NULL;

        if (field_desc->type_id != NULL) {
          HashNode_t *type_node = NULL;
          if (FindSymbol(&type_node, ctx->symtab, field_desc->type_id) != 0 &&
              type_node != NULL && type_node->type != NULL &&
              type_node->type->kind == TYPE_KIND_PROCEDURE) {
            proc_type = type_node->type;
            kgpc_type_retain(proc_type);
            is_proc_field = 1;
          }
          /* FindSymbol may fail to match TYPE_KIND_PROCEDURE on some hosts
           * (e.g. bare MSYS2 where freed-KgpcType memory is zeroed/perturbed).
           * field_desc->proc_type is pre-retained by semcheck_env_types and is
           * always the correct fallback. */
          if (proc_type == NULL && field_desc->proc_type != NULL &&
              field_desc->proc_type->kind == TYPE_KIND_PROCEDURE) {
            proc_type = field_desc->proc_type;
            kgpc_type_retain(proc_type);
            is_proc_field = 1;
          }
        } else if (field_desc->proc_type != NULL &&
                   field_desc->proc_type->kind == TYPE_KIND_PROCEDURE) {
          proc_type = field_desc->proc_type;
          kgpc_type_retain(proc_type);
          is_proc_field = 1;
        }

        /* Last-resort recovery: a prior semcheck of this same field
         * successfully resolved proc_type and cached the sret size in the
         * RecordField.  Use that to recognise the field as procedural even when
         * the KgpcType is no longer reachable (bare MSYS2 MALLOC_PERTURB_
         * use-after-free scenario). */
        if (!is_proc_field && proc_type == NULL &&
            field_desc->cached_proc_return_sret_size > 0)
          is_proc_field = 1;
        if (is_proc_field) {
          if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(
                stderr, "[SemCheck] treating %s.%s as procedural field call\n",
                receiver_expr->type == EXPR_VAR_ID ? receiver_expr->expr_data.id
                                                   : "<expr>",
                ctx->id);
          }
          /* Remove the receiver from the argument list */
          ListNode_t *remaining_args = ctx->args_given->next;
          ctx->expr->expr_data.function_call_data.args_expr = remaining_args;
          ctx->args_given->cur = NULL;
          free(ctx->args_given);

          /* Build a record-access expression to the procedural field */
          struct Expression *proc_expr =
              (struct Expression *)calloc(1, sizeof(struct Expression));
          if (proc_expr == NULL) {
            semcheck_error_with_context_at(
                ctx->expr->line_num, ctx->expr->col_num,
                ctx->expr->source_index,
                "Error on line %d: failed to allocate procedural field "
                "expression.\n",
                ctx->expr->line_num);
            *ctx->type_return = UNKNOWN_TYPE;
            do {
              ctx->final_status = ++ctx->return_val;
              return FC_CLEANUP;
            } while (0);
          }
          proc_expr->line_num = ctx->expr->line_num;
          proc_expr->type = EXPR_RECORD_ACCESS;
          proc_expr->expr_data.record_access_data.record_expr = receiver_expr;
          proc_expr->expr_data.record_access_data.field_id =
              strdup(field_lookup);
          proc_expr->expr_data.record_access_data.field_offset =
              (int)field_offset;
          /* Cache the RecordType (AST node) on the receiver so codegen can find
           * the field without going through KgpcType (which may be freed on
           * bare MSYS2 between semcheck and codegen). */
          if (recv_record != NULL)
            receiver_expr->record_type = recv_record;
          semcheck_expr_set_resolved_type(proc_expr, PROCEDURE);

          /* Validate arguments against the procedural type if available */
          if (proc_type != NULL) {
            ListNode_t *formal_params =
                kgpc_type_get_procedure_params(proc_type);
            if (semcheck_count_total_params(formal_params) !=
                ListLength(remaining_args)) {
              semcheck_error_with_context_at(
                  ctx->expr->line_num, ctx->expr->col_num,
                  ctx->expr->source_index,
                  "Error on line %d, call to procedural field %s: expected %d "
                  "arguments, got %d\n",
                  ctx->expr->line_num, ctx->id,
                  semcheck_count_total_params(formal_params),
                  ListLength(remaining_args));
              if (proc_type != NULL)
                destroy_kgpc_type(proc_type);
              destroy_expr(proc_expr);
              *ctx->type_return = UNKNOWN_TYPE;
              do {
                ctx->final_status = ++ctx->return_val;
                return FC_CLEANUP;
              } while (0);
            }

            ListNode_t *formal = formal_params;
            ListNode_t *actual = remaining_args;
            while (formal != NULL && actual != NULL) {
              Tree_t *formal_decl = (Tree_t *)formal->cur;
              struct Expression *actual_expr = (struct Expression *)actual->cur;

              int formal_type = resolve_param_type(formal_decl, ctx->symtab);
              int actual_type = UNKNOWN_TYPE;
              KgpcType *actual_kgpc_type = NULL;
              semcheck_expr_with_type(&actual_kgpc_type, ctx->symtab,
                                      actual_expr, ctx->max_scope_lev,
                                      NO_MUTATE);
              actual_type = semcheck_tag_from_kgpc(actual_kgpc_type);

              if (formal_type != UNKNOWN_TYPE && actual_type != UNKNOWN_TYPE &&
                  formal_type != actual_type) {
                if (!((formal_type == LONGINT_TYPE &&
                       actual_type == INT_TYPE) ||
                      (formal_type == INT_TYPE &&
                       actual_type == LONGINT_TYPE) ||
                      (formal_type == POINTER_TYPE) ||
                      (actual_type == POINTER_TYPE) ||
                      (is_integer_type(formal_type) &&
                       is_integer_type(actual_type)) ||
                      (is_real_family_type(formal_type) &&
                       is_integer_type(actual_type)) ||
                      (is_integer_type(formal_type) &&
                       is_real_family_type(actual_type)) ||
                      (is_real_family_type(formal_type) &&
                       is_real_family_type(actual_type)) ||
                      (formal_type == VARIANT_TYPE) ||
                      (actual_type == VARIANT_TYPE) ||
                      (formal_type == RECORD_TYPE) ||
                      (actual_type == RECORD_TYPE) ||
                      (formal_type == STRING_TYPE &&
                       actual_type == CHAR_TYPE) ||
                      (formal_type == CHAR_TYPE &&
                       actual_type == STRING_TYPE) ||
                      (formal_type == SHORTSTRING_TYPE &&
                       actual_type == CHAR_TYPE) ||
                      (formal_type == STRING_TYPE &&
                       actual_type == SHORTSTRING_TYPE) ||
                      (formal_type == SHORTSTRING_TYPE &&
                       actual_type == STRING_TYPE))) {
                  semantic_error_at(
                      ctx->expr->line_num, ctx->expr->col_num, -1,
                      "Incompatible types: got \"%s\" expected \"%s\"",
                      type_tag_to_string(actual_type),
                      type_tag_to_string(formal_type));
                  do {
                    ctx->final_status = ++ctx->return_val;
                    return FC_CLEANUP;
                  } while (0);
                }
              }

              formal = formal->next;
              actual = actual->next;
            }

            /* Cache call info for codegen */
            kgpc_type_retain(proc_type);
            ctx->expr->expr_data.function_call_data.call_kgpc_type = proc_type;
            ctx->expr->expr_data.function_call_data.call_hash_type =
                (kgpc_type_get_return_type(proc_type) == NULL)
                    ? HASHTYPE_PROCEDURE
                    : HASHTYPE_FUNCTION;
            ctx->expr->expr_data.function_call_data.is_call_info_valid = 1;

            KgpcType *ret_type = kgpc_type_get_return_type(proc_type);
            if (ret_type == NULL &&
                proc_type->info.proc_info.return_type_id != NULL) {
              HashNode_t *ret_node = semcheck_find_preferred_type_node(
                  ctx->symtab, proc_type->info.proc_info.return_type_id);
              if (ret_node != NULL && ret_node->type != NULL) {
                ret_type = ret_node->type;
                kgpc_type_retain(ret_type);
                proc_type->info.proc_info.return_type = ret_type;
              }
            }
            /* Update hash type now that return_type is materialized */
            if (ret_type != NULL)
              ctx->expr->expr_data.function_call_data.call_hash_type =
                  HASHTYPE_FUNCTION;
            /* Resolve alias metadata to get the underlying type */
            if (ret_type != NULL) {
              struct TypeAlias *alias = kgpc_type_get_type_alias(ret_type);
              if (alias != NULL && alias->target_type_id != NULL) {
                HashNode_t *target_node = semcheck_find_preferred_type_node(
                    ctx->symtab, alias->target_type_id);
                if (target_node != NULL && target_node->type != NULL)
                  ret_type = target_node->type;
              } else if (alias != NULL && alias->base_type != UNKNOWN_TYPE) {
                /* Alias resolves to a primitive type tag */
                *ctx->type_return = alias->base_type;
                semcheck_expr_set_resolved_type(ctx->expr, *ctx->type_return);
                ret_type = NULL; /* Mark as handled */
              }
            }
            if (ret_type != NULL && ret_type->kind == TYPE_KIND_PRIMITIVE) {
              *ctx->type_return = kgpc_type_get_primitive_tag(ret_type);
              semcheck_expr_set_resolved_type(ctx->expr, *ctx->type_return);
            } else if (ret_type != NULL && ret_type->kind == TYPE_KIND_RECORD) {
              *ctx->type_return = RECORD_TYPE;
              if (ctx->expr->resolved_kgpc_type != NULL)
                destroy_kgpc_type(ctx->expr->resolved_kgpc_type);
              kgpc_type_retain(ret_type);
              ctx->expr->resolved_kgpc_type = ret_type;
              long long sz = kgpc_type_sizeof(ret_type);
              long long new_sret =
                  (sz > 0) ? sz : 2 * (long long)sizeof(void *);
              /* If a prior semcheck already cached a valid sret size, trust it
               * over the current computation — on bare MSYS2 the freed
               * proc_type slot may be reused for a different KgpcType, giving
               * the wrong record size. */
              if (field_desc != NULL &&
                  field_desc->cached_proc_return_sret_size > 0)
                new_sret = field_desc->cached_proc_return_sret_size;
              ctx->expr->expr_data.function_call_data.cached_procvar_sret_size =
                  new_sret;
              if (field_desc != NULL) {
                if (field_desc->cached_proc_return_sret_size == 0)
                  field_desc->cached_proc_return_sret_size = new_sret;
                /* Retain the return type so subsequent calls can recover it
                 * even after proc_type has been freed (bare MSYS2 UAF). */
                if (field_desc->cached_proc_return_kgpc_type == NULL) {
                  kgpc_type_retain(ret_type);
                  field_desc->cached_proc_return_kgpc_type = ret_type;
                }
              }
            } else if (ret_type != NULL &&
                       ret_type->kind == TYPE_KIND_POINTER) {
              *ctx->type_return = POINTER_TYPE;
              /* Directly set resolved_kgpc_type to preserve full pointer type
               * info */
              if (ctx->expr->resolved_kgpc_type != NULL)
                destroy_kgpc_type(ctx->expr->resolved_kgpc_type);
              kgpc_type_retain(ret_type);
              ctx->expr->resolved_kgpc_type = ret_type;
              semcheck_set_pointer_info_from_kgpc_type(ctx->expr, ctx->symtab,
                                                       ret_type);
            } else if (ret_type != NULL) {
              /* Fallback - unhandled or corrupted return type kind.
               * On bare MSYS2 (MALLOC_PERTURB_=48), a freed TStatus
               * KgpcType gets 0x30-filled so its kind != TYPE_KIND_RECORD.
               * If a prior semcheck of this field cached the sret size,
               * use it to restore sret allocation without re-dereferencing
               * the corrupted type. */
              if (field_desc != NULL &&
                  field_desc->cached_proc_return_sret_size > 0) {
                *ctx->type_return = RECORD_TYPE;
                ctx->expr->expr_data.function_call_data
                    .cached_procvar_sret_size =
                    field_desc->cached_proc_return_sret_size;
              } else {
                *ctx->type_return = PROCEDURE;
                semcheck_expr_set_resolved_type(ctx->expr, PROCEDURE);
              }
            }
            /* If ret_type is NULL and we didn't set type_return above (from
             * alias), fall through to the cache check below. */
            if (ctx->expr->expr_data.function_call_data
                        .cached_procvar_sret_size == 0 &&
                field_desc != NULL &&
                field_desc->cached_proc_return_sret_size > 0) {
              ctx->expr->expr_data.function_call_data.cached_procvar_sret_size =
                  field_desc->cached_proc_return_sret_size;
            }
          } else {
            /* proc_type was unavailable (freed/corrupted on bare MSYS2).
             * If a prior semcheck retained the return KgpcType, use it to
             * produce RECORD_TYPE so the outer field-chain access resolves. */
            if (field_desc != NULL &&
                field_desc->cached_proc_return_kgpc_type != NULL) {
              *ctx->type_return = RECORD_TYPE;
              if (ctx->expr->resolved_kgpc_type != NULL)
                destroy_kgpc_type(ctx->expr->resolved_kgpc_type);
              kgpc_type_retain(field_desc->cached_proc_return_kgpc_type);
              ctx->expr->resolved_kgpc_type =
                  field_desc->cached_proc_return_kgpc_type;
              ctx->expr->expr_data.function_call_data.cached_procvar_sret_size =
                  field_desc->cached_proc_return_sret_size;
            } else {
              *ctx->type_return = PROCEDURE;
              semcheck_expr_set_resolved_type(ctx->expr, PROCEDURE);
              if (field_desc != NULL &&
                  field_desc->cached_proc_return_sret_size > 0)
                ctx->expr->expr_data.function_call_data
                    .cached_procvar_sret_size =
                    field_desc->cached_proc_return_sret_size;
            }
          }

          ctx->expr->expr_data.function_call_data.is_procedural_var_call = 1;
          ctx->expr->expr_data.function_call_data.procedural_var_symbol = NULL;
          ctx->expr->expr_data.function_call_data.procedural_var_expr =
              proc_expr;
          ctx->expr->expr_data.function_call_data.is_method_call_placeholder =
              0;

          /* We no longer treat this as a method call; proceed with validated
           * arguments */
          do {
            ctx->final_status = ctx->return_val;
            return FC_CLEANUP;
          } while (0);
        } else if (ctx->expr->expr_data.function_call_data
                       .is_method_call_placeholder &&
                   ctx->args_given->next == NULL) {
          /* Non-procedural field accessed via method call placeholder
           * (obj.field parsed as a function call). Convert to
           * EXPR_RECORD_ACCESS and delegate to the record access semantic
           * checker. */
          if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(
                stderr, "[SemCheck] treating %s.%s as regular field access\n",
                receiver_expr->type == EXPR_VAR_ID ? receiver_expr->expr_data.id
                                                   : "<expr>",
                ctx->id);
          }

          /* Detach receiver from the argument list before converting */
          ctx->args_given->cur = NULL;
          ctx->expr->expr_data.function_call_data.args_expr = NULL;
          free(ctx->args_given);

          /* Convert the node from EXPR_FUNCTION_CALL to EXPR_RECORD_ACCESS */
          char *field_id_copy = strdup(field_lookup);

          /* Clear function call data that is no longer relevant */
          if (ctx->expr->expr_data.function_call_data.id != NULL)
            free(ctx->expr->expr_data.function_call_data.id);
          if (ctx->expr->expr_data.function_call_data.mangled_id != NULL)
            free(ctx->expr->expr_data.function_call_data.mangled_id);

          /* Rewrite the node as a record access */
          ctx->expr->type = EXPR_RECORD_ACCESS;
          memset(&ctx->expr->expr_data, 0, sizeof(ctx->expr->expr_data));
          ctx->expr->expr_data.record_access_data.record_expr = receiver_expr;
          ctx->expr->expr_data.record_access_data.field_id = field_id_copy;
          ctx->expr->expr_data.record_access_data.field_offset =
              (int)field_offset;

          /* Delegate full type resolution to the record access handler */
          do {
            ctx->final_status =
                semcheck_recordaccess(ctx->type_return, ctx->symtab, ctx->expr,
                                      ctx->max_scope_lev, ctx->mutating);
            return FC_CLEANUP;
          } while (0);
        }
      }
    }
  }

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

  if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "GetMem")) {
    ListNode_t *args = ctx->expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL) {
      semcheck_error_with_context_at(
          ctx->expr->line_num, ctx->expr->col_num, ctx->expr->source_index,
          "Error on line %d, GetMem expects exactly one argument.\n",
          ctx->expr->line_num);
      *ctx->type_return = UNKNOWN_TYPE;
      do {
        ctx->final_status = 1;
        return FC_CLEANUP;
      } while (0);
    }

    struct Expression *size_expr = (struct Expression *)args->cur;
    KgpcType *size_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(
        &size_kgpc_type, ctx->symtab, size_expr, ctx->max_scope_lev, NO_MUTATE);
    if (error_count != 0) {
      *ctx->type_return = UNKNOWN_TYPE;
      do {
        ctx->final_status = error_count;
        return FC_CLEANUP;
      } while (0);
    }

    HashNode_t *best_match = NULL;
    ListNode_t *candidates = FindAllIdents(ctx->symtab, "GetMem");
    for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next) {
      HashNode_t *candidate = (HashNode_t *)cur->cur;
      if (candidate == NULL)
        continue;
      if (candidate->hash_type != HASHTYPE_FUNCTION)
        continue;
      if (candidate->type == NULL ||
          candidate->type->kind != TYPE_KIND_PROCEDURE)
        continue;
      ListNode_t *params = kgpc_type_get_procedure_params(candidate->type);
      if (semcheck_count_total_params(params) != 1)
        continue;
      best_match = candidate;
      break;
    }
    if (candidates != NULL)
      DestroyList(candidates);

    semcheck_reset_function_call_cache(ctx->expr);
    if (best_match != NULL && best_match->mangled_id != NULL) {
      if (ctx->expr->expr_data.function_call_data.mangled_id != NULL) {
        free(ctx->expr->expr_data.function_call_data.mangled_id);
        ctx->expr->expr_data.function_call_data.mangled_id = NULL;
      }
      ctx->expr->expr_data.function_call_data.mangled_id =
          strdup(best_match->mangled_id);
      if (ctx->expr->expr_data.function_call_data.mangled_id == NULL) {
        fprintf(stderr, "Error: failed to allocate mangled name for GetMem.\n");
        *ctx->type_return = UNKNOWN_TYPE;
        do {
          ctx->final_status = 1;
          return FC_CLEANUP;
        } while (0);
      }
      semcheck_set_function_call_target(ctx->expr, best_match);
      semcheck_sync_function_call_target_to_mangled(ctx->expr, ctx->symtab);
    } else {
      char *mangled_name_local = MangleFunctionNameFromCallSite(
          "GetMem", args, ctx->symtab, ctx->max_scope_lev);
      if (mangled_name_local != NULL) {
        if (ctx->expr->expr_data.function_call_data.mangled_id != NULL) {
          free(ctx->expr->expr_data.function_call_data.mangled_id);
          ctx->expr->expr_data.function_call_data.mangled_id = NULL;
        }
        ctx->expr->expr_data.function_call_data.mangled_id = mangled_name_local;
      }
    }
    *ctx->type_return = POINTER_TYPE;
    semcheck_expr_set_resolved_type(ctx->expr, POINTER_TYPE);
    do {
      ctx->final_status = 0;
      return FC_CLEANUP;
    } while (0);
  }

  int allow_builtins =
      !ctx->expr->expr_data.function_call_data.is_method_call_placeholder &&
      ctx->expr->expr_data.function_call_data.call_qualifier == NULL;

  if (kgpc_getenv("KGPC_DEBUG_EOF") != NULL && ctx->id != NULL &&
      pascal_identifier_equals(ctx->id, "EOF")) {
    fprintf(stderr,
            "[KGPC_DEBUG_EOF] allow_builtins=%d placeholder=%d qualifier=%s "
            "args=%d\n",
            allow_builtins,
            ctx->expr->expr_data.function_call_data.is_method_call_placeholder,
            ctx->expr->expr_data.function_call_data.call_qualifier != NULL
                ? ctx->expr->expr_data.function_call_data.call_qualifier
                : "(null)",
            ListLength(ctx->expr->expr_data.function_call_data.args_expr));
  }

  /* Table-driven dispatch for builtins that need no argument-type inspection */
  if (allow_builtins && ctx->id != NULL &&
      semcheck_dispatch_builtin_func(ctx->id, ctx->type_return, ctx->symtab,
                                     ctx->expr, ctx->max_scope_lev,
                                     &ctx->final_status))
    return FC_CLEANUP;

  /* Internal runtime function for open/dynamic array High - already resolved */
  if (ctx->id != NULL && strcmp(ctx->id, "kgpc_dynarray_compute_high") == 0) {
    /* This function was already set up by semcheck_builtin_lowhigh for dynamic
     * arrays. Just confirm it returns LONGINT_TYPE and proceed. */
    semcheck_expr_set_resolved_type(ctx->expr, LONGINT_TYPE);
    *ctx->type_return = LONGINT_TYPE;
    do {
      ctx->final_status = 0;
      return FC_CLEANUP;
    } while (0);
  }

  if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "Assigned"))
    do {
      ctx->final_status = semcheck_builtin_assigned(
          ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev);
      return FC_CLEANUP;
    } while (0);

  if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "UpperCase")) {
    ListNode_t *args = ctx->expr->expr_data.function_call_data.args_expr;
    if (args != NULL && args->next == NULL) {
      struct Expression *arg_expr = (struct Expression *)args->cur;
      int arg_type_local = UNKNOWN_TYPE;
      KgpcType *arg_kgpc_type_uc = NULL;
      int arg_cast_type = UNKNOWN_TYPE;
      if (arg_expr != NULL && arg_expr->type == EXPR_FUNCTION_CALL &&
          semcheck_expr_is_char_typecast_call_for_call_local(arg_expr))
        semcheck_try_reinterpret_as_typecast(&arg_cast_type, ctx->symtab,
                                             arg_expr, ctx->max_scope_lev);
      int error_count =
          semcheck_expr_with_type(&arg_kgpc_type_uc, ctx->symtab, arg_expr,
                                  ctx->max_scope_lev, NO_MUTATE);
      arg_type_local = semcheck_tag_from_kgpc(arg_kgpc_type_uc);
      if (error_count == 0 &&
          (arg_type_local == CHAR_TYPE ||
           semcheck_expr_is_char_like(arg_expr) ||
           semcheck_kgpc_type_is_char_like_for_call_local(arg_kgpc_type_uc) ||
           semcheck_expr_is_explicit_char_typecast_for_call_local(arg_expr))) {
        if (ctx->expr->expr_data.function_call_data.mangled_id != NULL) {
          free(ctx->expr->expr_data.function_call_data.mangled_id);
          ctx->expr->expr_data.function_call_data.mangled_id = NULL;
        }
        if (ctx->expr->expr_data.function_call_data.id != NULL) {
          free(ctx->expr->expr_data.function_call_data.id);
          ctx->expr->expr_data.function_call_data.id = NULL;
        }
        ctx->expr->expr_data.function_call_data.id = strdup("kgpc_upcase_char");
        ctx->expr->expr_data.function_call_data.mangled_id =
            strdup("kgpc_upcase_char");
        if (ctx->expr->expr_data.function_call_data.mangled_id == NULL) {
          fprintf(stderr,
                  "Error: failed to allocate mangled name for UpperCase.\n");
          *ctx->type_return = UNKNOWN_TYPE;
          do {
            ctx->final_status = 1;
            return FC_CLEANUP;
          } while (0);
        }
        semcheck_reset_function_call_cache(ctx->expr);
        if (ctx->expr->resolved_kgpc_type != NULL) {
          destroy_kgpc_type(ctx->expr->resolved_kgpc_type);
          ctx->expr->resolved_kgpc_type = NULL;
        }
        ctx->expr->resolved_kgpc_type = create_primitive_type(CHAR_TYPE);
        semcheck_expr_set_resolved_type(ctx->expr, CHAR_TYPE);
        *ctx->type_return = CHAR_TYPE;
        do {
          ctx->final_status = 0;
          return FC_CLEANUP;
        } while (0);
      }
    }
  }

  if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "LowerCase")) {
    ListNode_t *args = ctx->expr->expr_data.function_call_data.args_expr;
    if (args != NULL && args->next == NULL) {
      struct Expression *arg_expr = (struct Expression *)args->cur;
      int arg_type_local = UNKNOWN_TYPE;
      KgpcType *arg_kgpc_type_lc = NULL;
      int arg_cast_type = UNKNOWN_TYPE;
      if (arg_expr != NULL && arg_expr->type == EXPR_FUNCTION_CALL &&
          semcheck_expr_is_char_typecast_call_for_call_local(arg_expr))
        semcheck_try_reinterpret_as_typecast(&arg_cast_type, ctx->symtab,
                                             arg_expr, ctx->max_scope_lev);
      int error_count =
          semcheck_expr_with_type(&arg_kgpc_type_lc, ctx->symtab, arg_expr,
                                  ctx->max_scope_lev, NO_MUTATE);
      arg_type_local = semcheck_tag_from_kgpc(arg_kgpc_type_lc);
      if (error_count == 0 &&
          (arg_type_local == CHAR_TYPE ||
           semcheck_expr_is_char_like(arg_expr) ||
           semcheck_kgpc_type_is_char_like_for_call_local(arg_kgpc_type_lc) ||
           semcheck_expr_is_explicit_char_typecast_for_call_local(arg_expr))) {
        if (ctx->expr->expr_data.function_call_data.mangled_id != NULL) {
          free(ctx->expr->expr_data.function_call_data.mangled_id);
          ctx->expr->expr_data.function_call_data.mangled_id = NULL;
        }
        if (ctx->expr->expr_data.function_call_data.id != NULL) {
          free(ctx->expr->expr_data.function_call_data.id);
          ctx->expr->expr_data.function_call_data.id = NULL;
        }
        ctx->expr->expr_data.function_call_data.id =
            strdup("kgpc_lowercase_char");
        ctx->expr->expr_data.function_call_data.mangled_id =
            strdup("kgpc_lowercase_char");
        if (ctx->expr->expr_data.function_call_data.mangled_id == NULL) {
          fprintf(stderr,
                  "Error: failed to allocate mangled name for LowerCase.\n");
          *ctx->type_return = UNKNOWN_TYPE;
          do {
            ctx->final_status = 1;
            return FC_CLEANUP;
          } while (0);
        }
        semcheck_reset_function_call_cache(ctx->expr);
        if (ctx->expr->resolved_kgpc_type != NULL) {
          destroy_kgpc_type(ctx->expr->resolved_kgpc_type);
          ctx->expr->resolved_kgpc_type = NULL;
        }
        ctx->expr->resolved_kgpc_type = create_primitive_type(CHAR_TYPE);
        semcheck_expr_set_resolved_type(ctx->expr, CHAR_TYPE);
        *ctx->type_return = CHAR_TYPE;
        do {
          ctx->final_status = 0;
          return FC_CLEANUP;
        } while (0);
      }
    }
  }

  /* UpCase(char) is always handled as a builtin (not gated by allow_builtins)
   * to avoid selecting the UnicodeChar overload which calls through
   * widestringmanager (uninitialized on Linux without cwstring). */
  if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "UpCase")) {
    ListNode_t *args = ctx->expr->expr_data.function_call_data.args_expr;
    if (args != NULL && args->next == NULL) {
      struct Expression *arg_expr = (struct Expression *)args->cur;
      int arg_type_local = UNKNOWN_TYPE;
      KgpcType *arg_kgpc_type_upcase = NULL;
      int arg_cast_type = UNKNOWN_TYPE;
      if (arg_expr != NULL && arg_expr->type == EXPR_FUNCTION_CALL &&
          semcheck_expr_is_char_typecast_call_for_call_local(arg_expr))
        semcheck_try_reinterpret_as_typecast(&arg_cast_type, ctx->symtab,
                                             arg_expr, ctx->max_scope_lev);
      int error_count =
          semcheck_expr_with_type(&arg_kgpc_type_upcase, ctx->symtab, arg_expr,
                                  ctx->max_scope_lev, NO_MUTATE);
      arg_type_local = semcheck_tag_from_kgpc(arg_kgpc_type_upcase);
      if (error_count == 0 &&
          (arg_type_local == CHAR_TYPE ||
           semcheck_expr_is_char_like(arg_expr) ||
           semcheck_kgpc_type_is_char_like_for_call_local(
               arg_kgpc_type_upcase) ||
           semcheck_expr_is_explicit_char_typecast_for_call_local(arg_expr)))
        do {
          ctx->final_status = semcheck_builtin_upcase(
              ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev);
          return FC_CLEANUP;
        } while (0);
      if (error_count == 0 && arg_type_local == STRING_TYPE &&
          arg_expr != NULL && arg_expr->type == EXPR_STRING &&
          arg_expr->expr_data.string != NULL &&
          strlen(arg_expr->expr_data.string) == 1)
        do {
          ctx->final_status = semcheck_builtin_upcase(
              ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev);
          return FC_CLEANUP;
        } while (0);
    }
  }

  if (allow_builtins && ctx->id != NULL &&
      pascal_identifier_equals(ctx->id, "Power"))
    do {
      ctx->final_status = semcheck_builtin_power(ctx->type_return, ctx->symtab,
                                                 ctx->expr, ctx->max_scope_lev);
      return FC_CLEANUP;
    } while (0);

  if (allow_builtins && ctx->id != NULL &&
      pascal_identifier_equals(ctx->id, "Aligned"))
    do {
      ctx->final_status = semcheck_builtin_aligned(
          ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev);
      return FC_CLEANUP;
    } while (0);

  /***** FIRST VERIFY FUNCTION IDENTIFIER *****/

  /***** FIRST VERIFY FUNCTION IDENTIFIER *****/

  /* Resolve unqualified calls against the current static method owner
   * (helpers/class methods). */
  if (!ctx->is_operator_dispatch && !ctx->was_unit_qualified &&
      ctx->id != NULL &&
      !ctx->expr->expr_data.function_call_data.is_method_call_placeholder) {
    const char *current_owner = semcheck_get_current_method_owner();
    if (current_owner != NULL) {
      struct RecordType *owner_record =
          semcheck_lookup_record_type(ctx->symtab, current_owner);
      if (owner_record != NULL) {
        /* Use owner_out to get the ACTUAL owner where method was found (may be
         * parent helper) */
        struct RecordType *actual_method_owner = NULL;
        HashNode_t *method_node = semcheck_find_class_method(
            ctx->symtab, owner_record, ctx->id, &actual_method_owner);
        char *mangled_method_name = NULL;
        ListNode_t *method_candidates = NULL;
        /* Use actual_method_owner if found (for inherited methods), else fall
         * back to owner_record */
        struct RecordType *record_for_mangling =
            (actual_method_owner != NULL) ? actual_method_owner : owner_record;
        int is_static_owner_method = 0;
        if (record_for_mangling->type_id != NULL && ctx->id != NULL)
          is_static_owner_method = from_cparser_is_method_static(
              record_for_mangling->type_id, ctx->id);
        if (record_for_mangling->type_id != NULL) {
          size_t class_len = strlen(record_for_mangling->type_id);
          size_t method_len = strlen(ctx->id);
          mangled_method_name = (char *)malloc(class_len + 2 + method_len + 1);
          if (mangled_method_name != NULL)
            snprintf(mangled_method_name, class_len + 2 + method_len + 1,
                     "%s__%s", record_for_mangling->type_id, ctx->id);
        }
        if (mangled_method_name != NULL)
          method_candidates = FindAllIdents(ctx->symtab, mangled_method_name);
        /* Collect overloads from parent classes as well.
         * Overloads may be split across multiple hierarchy levels
         * (e.g. find(ShortString) on TBase, find(Word,Pointer) on TDerived). */
        if (owner_record != NULL && ctx->id != NULL) {
          ListNode_t *hierarchy_candidates =
              semcheck_collect_hierarchy_method_overloads(
                  ctx->symtab, owner_record, ctx->id);
          if (hierarchy_candidates != NULL)
            semcheck_merge_candidate_lists_dedup(&method_candidates,
                                                 hierarchy_candidates);
        }
        if (method_node == NULL && method_candidates != NULL) {
          for (ListNode_t *cur = method_candidates; cur != NULL;
               cur = cur->next) {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL &&
                (candidate->hash_type == HASHTYPE_FUNCTION ||
                 candidate->hash_type == HASHTYPE_PROCEDURE)) {
              method_node = candidate;
              break;
            }
          }
        }
        if (method_node != NULL) {
          int method_is_overloaded =
              (method_candidates != NULL && method_candidates->next != NULL);
          if (method_is_overloaded && ctx->mangled_name != NULL) {
            free(ctx->mangled_name);
            ctx->mangled_name = NULL;
          }

          if (is_static_owner_method && ctx->args_given != NULL &&
              ctx->args_given->cur != NULL) {
            struct Expression *first_arg =
                (struct Expression *)ctx->args_given->cur;
            if (first_arg->type == EXPR_VAR_ID &&
                first_arg->expr_data.id != NULL &&
                pascal_identifier_equals(first_arg->expr_data.id, "Self")) {
              int given_count = ListLength(ctx->args_given);
              int can_strip_self = 0;
              int has_direct_match = 0;
              int created_temp_list = 0;
              ListNode_t *candidate_list = method_candidates;
              if (candidate_list == NULL) {
                candidate_list = CreateListNode(method_node, LIST_UNSPECIFIED);
                created_temp_list = 1;
              }
              for (ListNode_t *cur = candidate_list; cur != NULL;
                   cur = cur->next) {
                HashNode_t *candidate = (HashNode_t *)cur->cur;
                if (candidate == NULL || candidate->type == NULL ||
                    (candidate->hash_type != HASHTYPE_FUNCTION &&
                     candidate->hash_type != HASHTYPE_PROCEDURE))
                  continue;
                ListNode_t *params =
                    kgpc_type_get_procedure_params(candidate->type);
                int total_params = semcheck_count_total_params(params);
                int required_params = semcheck_count_required_params(params);
                if (given_count >= required_params &&
                    given_count <= total_params) {
                  int first_param_is_self = 0;
                  if (params != NULL) {
                    Tree_t *decl = (Tree_t *)params->cur;
                    const char *param_name = NULL;
                    if (decl != NULL && decl->type == TREE_VAR_DECL &&
                        decl->tree_data.var_decl_data.ids != NULL)
                      param_name =
                          (const char *)decl->tree_data.var_decl_data.ids->cur;
                    else if (decl != NULL && decl->type == TREE_ARR_DECL &&
                             decl->tree_data.arr_decl_data.ids != NULL)
                      param_name =
                          (const char *)decl->tree_data.arr_decl_data.ids->cur;
                    if (param_name != NULL &&
                        pascal_identifier_equals(param_name, "Self"))
                      first_param_is_self = 1;
                  }
                  if (first_param_is_self)
                    has_direct_match = 1;
                }
                if (given_count - 1 >= required_params &&
                    given_count - 1 <= total_params)
                  can_strip_self = 1;
              }
              if (created_temp_list)
                DestroyList(candidate_list);
              if (!has_direct_match && can_strip_self) {
                ListNode_t *next_arg = ctx->args_given->next;
                ctx->args_given->cur = NULL;
                ctx->args_given->next = NULL;
                ctx->args_given = next_arg;
                ctx->expr->expr_data.function_call_data.args_expr =
                    ctx->args_given;
              }
            }
          }
          int has_self_arg = 0;
          if (ctx->args_given != NULL && ctx->args_given->cur != NULL) {
            struct Expression *first_arg =
                (struct Expression *)ctx->args_given->cur;
            if (first_arg->type == EXPR_VAR_ID &&
                first_arg->expr_data.id != NULL &&
                pascal_identifier_equals(first_arg->expr_data.id, "Self")) {
              has_self_arg = 1;
            }
          }

          int given_count = ListLength(ctx->args_given);
          int has_direct_match = 0;
          int has_self_match = 0;
          int created_temp_list = 0;
          ListNode_t *candidate_list = method_candidates;
          if (candidate_list == NULL) {
            candidate_list = CreateListNode(method_node, LIST_UNSPECIFIED);
            created_temp_list = 1;
          }
          for (ListNode_t *cur = candidate_list; cur != NULL; cur = cur->next) {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate == NULL || candidate->type == NULL ||
                (candidate->hash_type != HASHTYPE_FUNCTION &&
                 candidate->hash_type != HASHTYPE_PROCEDURE))
              continue;

            ListNode_t *params =
                kgpc_type_get_procedure_params(candidate->type);
            int total_params = semcheck_count_total_params(params);
            int required_params = semcheck_count_required_params(params);
            int is_static_method = 0;
            if (record_for_mangling->type_id != NULL && ctx->id != NULL)
              is_static_method = from_cparser_is_method_static(
                  record_for_mangling->type_id, ctx->id);

            int first_is_self = 0;
            if (params != NULL) {
              Tree_t *decl = (Tree_t *)params->cur;
              const char *param_name = NULL;
              if (decl != NULL && decl->type == TREE_VAR_DECL &&
                  decl->tree_data.var_decl_data.ids != NULL)
                param_name =
                    (const char *)decl->tree_data.var_decl_data.ids->cur;
              else if (decl != NULL && decl->type == TREE_ARR_DECL &&
                       decl->tree_data.arr_decl_data.ids != NULL)
                param_name =
                    (const char *)decl->tree_data.arr_decl_data.ids->cur;

              if (param_name != NULL &&
                  pascal_identifier_equals(param_name, "Self"))
                first_is_self = 1;
            }

            if (is_static_method && first_is_self) {
              int self_count = 0;
              Tree_t *first_param = (Tree_t *)params->cur;
              if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
                  first_param->tree_data.var_decl_data.ids != NULL)
                self_count =
                    ListLength(first_param->tree_data.var_decl_data.ids);
              else if (first_param != NULL &&
                       first_param->type == TREE_ARR_DECL &&
                       first_param->tree_data.arr_decl_data.ids != NULL)
                self_count =
                    ListLength(first_param->tree_data.arr_decl_data.ids);
              if (total_params >= self_count)
                total_params -= self_count;
              if (required_params >= self_count)
                required_params -= self_count;
              first_is_self = 0;
            }

            if (!first_is_self && given_count >= required_params &&
                given_count <= total_params)
              has_direct_match = 1;

            if (first_is_self && given_count + 1 >= required_params &&
                given_count + 1 <= total_params)
              has_self_match = 1;
          }
          if (created_temp_list)
            DestroyList(candidate_list);

          /* Inside a type helper, if a standalone function that takes
           * array-of-const matches the given arg count, prefer it
           * over injecting Self.  E.g. Format('...', [...]) inside
           * TUnicodeStringHelper.Join should call standalone Format. */
          if (!has_direct_match && has_self_match &&
              record_for_mangling != NULL &&
              record_for_mangling->is_type_helper) {
            ListNode_t *standalone_candidates =
                FindAllIdents(ctx->symtab, ctx->id);
            for (ListNode_t *sc = standalone_candidates; sc != NULL;
                 sc = sc->next) {
              HashNode_t *sc_node = (HashNode_t *)sc->cur;
              if (sc_node != NULL && sc_node->owner_class == NULL &&
                  (sc_node->hash_type == HASHTYPE_FUNCTION ||
                   sc_node->hash_type == HASHTYPE_PROCEDURE) &&
                  sc_node->type != NULL) {
                /* Check if this standalone has an array-of-const param */
                ListNode_t *sc_params =
                    kgpc_type_get_procedure_params(sc_node->type);
                int has_aoc = sc_node->is_varargs;
                if (!has_aoc) {
                  for (ListNode_t *p = sc_params; p != NULL; p = p->next) {
                    Tree_t *pdecl = (Tree_t *)p->cur;
                    if (pdecl != NULL && pdecl->type == TREE_ARR_DECL &&
                        pdecl->tree_data.arr_decl_data.type ==
                            ARRAY_OF_CONST_TYPE) {
                      has_aoc = 1;
                      break;
                    }
                  }
                }
                if (has_aoc) {
                  int sc_total = semcheck_count_total_params(sc_params);
                  int sc_req = semcheck_count_required_params(sc_params);
                  if (given_count >= sc_req && given_count <= sc_total) {
                    has_direct_match = 1;
                    break;
                  }
                }
              }
            }
            if (standalone_candidates != NULL)
              DestroyList(standalone_candidates);
          }

          if (!has_direct_match && has_self_match && !has_self_arg) {
            HashNode_t *self_node = NULL;
            int self_found =
                (FindSymbol(&self_node, ctx->symtab, "Self") != 0 &&
                 self_node != NULL);
            const char *self_arg_name = "Self";
            if (!self_found && record_for_mangling != NULL &&
                record_type_is_class(record_for_mangling) &&
                !record_for_mangling->is_type_helper &&
                record_for_mangling->type_id != NULL) {
              self_arg_name = record_for_mangling->type_id;
            }
            struct Expression *self_expr =
                mk_varid(ctx->expr->line_num, strdup(self_arg_name));
            if (self_expr != NULL) {
              if (record_for_mangling != NULL &&
                  record_for_mangling->is_type_helper &&
                  record_for_mangling->helper_base_type_id != NULL) {
                HashNode_t *base_node = semcheck_find_preferred_type_node(
                    ctx->symtab, record_for_mangling->helper_base_type_id);
                if (kgpc_getenv("KGPC_DEBUG_FORMAT") != NULL &&
                    ctx->id != NULL &&
                    pascal_identifier_equals(ctx->id, "Format")) {
                  fprintf(
                      stderr,
                      "[KGPC_DEBUG_FORMAT] helper base=%s node=%p type=%p\n",
                      record_for_mangling->helper_base_type_id != NULL
                          ? record_for_mangling->helper_base_type_id
                          : "(null)",
                      (void *)base_node,
                      base_node != NULL ? (void *)base_node->type : NULL);
                }
                if (base_node != NULL && base_node->type != NULL)
                  semcheck_expr_set_resolved_kgpc_type_shared(self_expr,
                                                              base_node->type);
              }
              if (pascal_identifier_equals(self_arg_name, "Self")) {
                if (self_found && self_node != NULL &&
                    self_node->type != NULL) {
                  semcheck_expr_set_resolved_kgpc_type_shared(self_expr,
                                                              self_node->type);
                } else if (record_for_mangling != NULL) {
                  KgpcType *self_record_type =
                      create_record_type(record_for_mangling);
                  if (self_record_type != NULL) {
                    semcheck_expr_set_resolved_kgpc_type_shared(
                        self_expr, self_record_type);
                    destroy_kgpc_type(self_record_type);
                  }
                }
              }
              ListNode_t *self_node = CreateListNode(self_expr, LIST_EXPR);
              self_node->next = ctx->args_given;
              ctx->args_given = self_node;
              ctx->expr->expr_data.function_call_data.args_expr =
                  ctx->args_given;
              ctx->injected_self = 1;
              if (kgpc_getenv("KGPC_DEBUG_FORMAT") != NULL && ctx->id != NULL &&
                  pascal_identifier_equals(ctx->id, "Format")) {
                fprintf(stderr,
                        "[KGPC_DEBUG_FORMAT] injected Self (overload match)\n");
              }
              if (kgpc_getenv("KGPC_DEBUG_TRIM") != NULL && ctx->id != NULL &&
                  pascal_identifier_equals(ctx->id, "Trim")) {
                fprintf(stderr,
                        "[KGPC_DEBUG_TRIM] injected Self (overload match)\n");
              }
            }
          }

          if (!method_is_overloaded) {
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

            if (ctx->mangled_name != NULL)
              free(ctx->mangled_name);
            /* Keep the exact selected overload mangled id so overload
             * resolution cannot drift to a different same-named method. */
            ctx->mangled_name =
                (resolved_method_name != NULL)
                    ? strdup(resolved_method_name)
                    : (mangled_method_name != NULL ? strdup(mangled_method_name)
                                                   : NULL);
          } else {
            if (ctx->expr->expr_data.function_call_data.mangled_id != NULL) {
              free(ctx->expr->expr_data.function_call_data.mangled_id);
              ctx->expr->expr_data.function_call_data.mangled_id = NULL;
            }
          }

          if (method_candidates != NULL) {
            ctx->overload_candidates = method_candidates;
          } else {
            ctx->overload_candidates =
                CreateListNode(method_node, LIST_UNSPECIFIED);
          }

          if (mangled_method_name != NULL)
            free(mangled_method_name);

          ctx->hash_return = method_node;
          return FC_OVERLOAD_RESOLVE;
        }
        if (mangled_method_name != NULL)
          free(mangled_method_name);
        if (method_candidates != NULL)
          DestroyList(method_candidates);

        /* Method not found on implicit Self — check if id matches a
         * procedural-type field on the owner record. This handles:
         *   FCanObserve(aID)  →  Self.FCanObserve(aID)
         * where FCanObserve is a field of type TCanObserveEvent. */
        {
          struct RecordField *proc_field = NULL;
          long long pf_offset = 0;
          if (resolve_record_field(ctx->symtab, owner_record, ctx->id,
                                   &proc_field, &pf_offset, ctx->expr->line_num,
                                   1) == 0 &&
              proc_field != NULL) {
            KgpcType *proc_kgpc_type = NULL;
            if (proc_field->proc_type != NULL &&
                proc_field->proc_type->kind == TYPE_KIND_PROCEDURE) {
              proc_kgpc_type = proc_field->proc_type;
            } else if (proc_field->type_id != NULL) {
              HashNode_t *type_node = NULL;
              if (FindSymbol(&type_node, ctx->symtab, proc_field->type_id) !=
                      0 &&
                  type_node != NULL && type_node->type != NULL &&
                  type_node->type->kind == TYPE_KIND_PROCEDURE) {
                proc_kgpc_type = type_node->type;
              }
            } else if (proc_field->type == PROCEDURE) {
              /* Inline procedural type without proc_type or type_id */
            }

            if (proc_kgpc_type != NULL &&
                proc_kgpc_type->kind == TYPE_KIND_PROCEDURE) {
              /* Build Self.field access expression */
              struct Expression *self_expr =
                  mk_varid(ctx->expr->line_num, strdup("Self"));
              if (self_expr == NULL) {
                *ctx->type_return = UNKNOWN_TYPE;
                ctx->final_status = ++ctx->return_val;
                return FC_CLEANUP;
              }
              struct Expression *field_access = mk_recordaccess(
                  ctx->expr->line_num, self_expr, strdup(ctx->id));
              if (field_access == NULL) {
                destroy_expr(self_expr);
                *ctx->type_return = UNKNOWN_TYPE;
                ctx->final_status = ++ctx->return_val;
                return FC_CLEANUP;
              }

              /* Resolve the field access expression */
              KgpcType *field_kgpc = NULL;
              semcheck_expr_with_type(&field_kgpc, ctx->symtab, field_access,
                                      ctx->max_scope_lev, NO_MUTATE);

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
              } else {
                *ctx->type_return = PROCEDURE;
                semcheck_expr_set_resolved_type(ctx->expr, PROCEDURE);
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

              /* Type-check arguments */
              for (ListNode_t *arg_cur = ctx->args_given; arg_cur != NULL;
                   arg_cur = arg_cur->next) {
                struct Expression *arg = (struct Expression *)arg_cur->cur;
                if (arg != NULL)
                  semcheck_expr_with_type(NULL, ctx->symtab, arg,
                                          ctx->max_scope_lev, NO_MUTATE);
              }

              ctx->final_status = ctx->return_val;
              return FC_CLEANUP;
            }
          }
        }
      }
    skip_self_injection_internal:;
    }
  }

  return FC_METHOD;
}
