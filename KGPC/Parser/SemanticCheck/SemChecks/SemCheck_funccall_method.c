#include "SemCheck_Expr_Internal.h"
#include "SemCheck_stmt.h"
#include <time.h>
#include <ctype.h>
#include <limits.h>
#include "SemCheck_funccall_internal.h"

FunccallState funccall_state_method(FunccallCtx *ctx) {
  /* Check for method call with unresolved name (member-access placeholder)
   * where first arg is the type/instance. */
  if (!ctx->was_unit_qualified &&
      ctx->expr->expr_data.function_call_data.is_method_call_placeholder &&
      ctx->args_given != NULL) {
    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
      fprintf(stderr,
              "[SemCheck] funccall method-placeholder: id=%s "
              "was_unit_qualified=%d\n",
              ctx->id != NULL ? ctx->id : "(null)", ctx->was_unit_qualified);
    }
    struct Expression *first_arg = (struct Expression *)ctx->args_given->cur;
    /* In method placeholder calls like "BottomRight.ToString(...)" inside a
     * method body, prefer implicit Self member resolution over same-named
     * global functions. */
    if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
        first_arg->expr_data.id != NULL) {
      int try_self_member = 1;
      HashNode_t *first_ident = NULL;
      int first_scope =
          FindSymbol(&first_ident, ctx->symtab, first_arg->expr_data.id);
      HashNode_t *self_node = NULL;
      struct RecordType *self_record = NULL;
      if (FindSymbol(&self_node, ctx->symtab, "Self") != 0 &&
          self_node != NULL) {
        self_record = get_record_type_from_node(self_node);
        if (self_record == NULL && self_node->type != NULL) {
          KgpcType *self_type = self_node->type;
          if (self_type->kind == TYPE_KIND_RECORD)
            self_record = self_type->info.record_info;
          else if (self_type->kind == TYPE_KIND_POINTER &&
                   self_type->info.points_to != NULL &&
                   self_type->info.points_to->kind == TYPE_KIND_RECORD)
            self_record = self_type->info.points_to->info.record_info;
          else if (self_type->type_alias != NULL &&
                   self_type->type_alias->target_type_id != NULL)
            self_record = semcheck_lookup_record_type(
                ctx->symtab, self_type->type_alias->target_type_id);
        }
      }
      if (self_record == NULL) {
        const char *owner = semcheck_get_current_method_owner();
        if (owner != NULL)
          self_record = semcheck_lookup_record_type(ctx->symtab, owner);
      }

      if (self_record != NULL) {
        struct RecordType *field_owner = NULL;
        struct RecordType *prop_owner = NULL;
        struct RecordField *field_desc = semcheck_find_class_field(
            ctx->symtab, self_record, first_arg->expr_data.id, &field_owner);
        struct ClassProperty *prop_desc = semcheck_find_class_property(
            ctx->symtab, self_record, first_arg->expr_data.id, &prop_owner);
        int member_exists = (field_desc != NULL || prop_desc != NULL);
        /* Local scope generally wins, except when the scoped symbol is a
         * non-record placeholder and Self actually exposes a member with this
         * name. */
        if (!member_exists) {
          try_self_member = 0;
        } else if (first_scope == 0 && first_ident != NULL) {
          if (first_ident->hash_type == HASHTYPE_FUNCTION ||
              first_ident->hash_type == HASHTYPE_PROCEDURE ||
              first_ident->hash_type == HASHTYPE_CONST ||
              first_ident->hash_type == HASHTYPE_TYPE) {
            try_self_member = 1;
          } else {
            int scoped_tag = UNKNOWN_TYPE;
            set_type_from_hashtype(&scoped_tag, first_ident);
            if (scoped_tag == RECORD_TYPE || scoped_tag == POINTER_TYPE ||
                scoped_tag == PROCEDURE)
              try_self_member = 0;
            else
              try_self_member = 1;
          }
        }
      } else {
        try_self_member = 0;
      }

      if (try_self_member) {
        struct Expression *self_expr =
            mk_varid(first_arg->line_num, strdup("Self"));
        if (self_expr != NULL) {
          if (self_node != NULL && self_node->type != NULL) {
            self_expr->resolved_kgpc_type = self_node->type;
            kgpc_type_retain(self_node->type);
          }
          struct Expression *member_expr = mk_recordaccess(
              first_arg->line_num, self_expr, strdup(first_arg->expr_data.id));
          if (member_expr != NULL) {
            /* mk_recordaccess strdup'd the field id, so we can destroy
             * the original first_arg Expression — the list cur slot
             * now owns the new Self.<field> RECORD_ACCESS. */
            destroy_expr((struct Expression *)ctx->args_given->cur);
            ctx->args_given->cur = member_expr;
            first_arg = member_expr;
          } else {
            destroy_expr(self_expr);
          }
        }
      }
    }
    if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
        first_arg->expr_data.id != NULL && with_context_count > 0) {
      struct Expression *with_expr = NULL;
      int with_status =
          semcheck_with_try_resolve(first_arg->expr_data.id, ctx->symtab,
                                    &with_expr, ctx->expr->line_num);
      if (with_status == 0 && with_expr != NULL) {
        char *field_id = first_arg->expr_data.id;
        memset(&first_arg->expr_data, 0, sizeof(first_arg->expr_data));
        first_arg->type = EXPR_RECORD_ACCESS;
        first_arg->expr_data.record_access_data.record_expr = with_expr;
        first_arg->expr_data.record_access_data.field_id = field_id;
        first_arg->expr_data.record_access_data.field_offset = 0;
        first_arg->record_type = NULL;
        first_arg->array_element_record_type = NULL;
        first_arg->array_element_type = UNKNOWN_TYPE;
        first_arg->array_element_type_id = NULL;
        semcheck_set_pointer_info(first_arg, UNKNOWN_TYPE, NULL);
        semcheck_expr_set_resolved_type(first_arg, UNKNOWN_TYPE);
      } else if (with_expr != NULL) {
        destroy_expr(with_expr);
      }
    }

    int first_arg_type_tag;
    KgpcType *first_arg_kgpc_type = NULL;
    semcheck_expr_with_type(&first_arg_kgpc_type, ctx->symtab, first_arg,
                            ctx->max_scope_lev, NO_MUTATE);
    first_arg_type_tag = semcheck_tag_from_kgpc(first_arg_kgpc_type);
    (void)first_arg_type_tag; /* Variable is used for potential debugging */
    if (first_arg != NULL && first_arg->type == EXPR_RECORD_ACCESS &&
        (first_arg->resolved_kgpc_type == NULL ||
         first_arg->record_type == NULL)) {
      KgpcType *mutating_type = NULL;
      semcheck_expr_with_type(&mutating_type, ctx->symtab, first_arg,
                              ctx->max_scope_lev, MUTATE);
      /* Do NOT destroy mutating_type here: it is a borrowed reference
       * to first_arg->resolved_kgpc_type (owned by the expression).
       * Destroying it would double-release the type, leaving a dangling
       * pointer in the expression and in any hash node that shares
       * the same KgpcType. */
      (void)mutating_type;
    }
    if (first_arg != NULL && first_arg->resolved_kgpc_type == NULL &&
        first_arg->record_type != NULL) {
      KgpcType *record_kgpc = create_record_type(first_arg->record_type);
      if (record_kgpc != NULL) {
        semcheck_expr_set_resolved_kgpc_type_shared(first_arg, record_kgpc);
        destroy_kgpc_type(record_kgpc);
      }
    }

    struct RecordType *record_info = NULL;
    if (first_arg->record_type != NULL &&
        !record_type_is_class(first_arg->record_type))
      record_info = first_arg->record_type;

    if (record_info != NULL || first_arg->resolved_kgpc_type != NULL ||
        first_arg_kgpc_type != NULL) {
      KgpcType *owner_type = (first_arg->resolved_kgpc_type != NULL)
                                 ? first_arg->resolved_kgpc_type
                                 : first_arg_kgpc_type;

      if (record_info == NULL && owner_type != NULL) {
        if (owner_type->kind == TYPE_KIND_RECORD) {
          record_info = owner_type->info.record_info;
        } else if (owner_type->kind == TYPE_KIND_POINTER) {
          /* Try lazy resolution of unresolved pointer pointees */
          KgpcType *pointee =
              kgpc_type_resolve_pointer_pointee(owner_type, ctx->symtab);
          if (pointee != NULL && pointee->kind == TYPE_KIND_RECORD)
            record_info = pointee->info.record_info;
        }
      }
      /* For "class of T" (metaclass) types, the pointer's pointee may not
       * have been resolved to TYPE_KIND_RECORD at AST conversion time.
       * Try multiple strategies to find the record type:
       * 1. The expression's record_type field (set by record access resolution)
       * 2. Variable lookup via TypeAlias pointer_type_id
       * 3. The KgpcType's own type_alias pointer_type_id */
      if (record_info == NULL && owner_type != NULL &&
          owner_type->kind == TYPE_KIND_POINTER) {
        /* Strategy 1: variable's TypeAlias */
        if (record_info == NULL && first_arg->type == EXPR_VAR_ID &&
            first_arg->expr_data.id != NULL) {
          HashNode_t *var_node = NULL;
          if (FindSymbol(&var_node, ctx->symtab, first_arg->expr_data.id) !=
                  0 &&
              var_node != NULL) {
            record_info = get_record_type_from_node(var_node);
            if (record_info == NULL && var_node->type != NULL &&
                var_node->type->type_alias != NULL &&
                var_node->type->type_alias->pointer_type_id != NULL) {
              record_info = semcheck_lookup_record_type(
                  ctx->symtab, var_node->type->type_alias->pointer_type_id);
            }
          }
        }

        /* Strategy 2: KgpcType's own type_alias */
        if (record_info == NULL && owner_type->type_alias != NULL &&
            owner_type->type_alias->pointer_type_id != NULL) {
          record_info = semcheck_lookup_record_type(
              ctx->symtab, owner_type->type_alias->pointer_type_id);
        }

        /* Strategy 3: expression's pointer_subtype_id */
        if (record_info == NULL && first_arg->pointer_subtype_id != NULL) {
          record_info = semcheck_lookup_record_type(
              ctx->symtab, first_arg->pointer_subtype_id);
        }

        /* Strategy 4: unresolved primitive placeholder — the points_to
         * is PRIMITIVE(RECORD_TYPE) from a forward-declared class type.
         * Try type_alias target_type_id, pointer_type_id, and alias_name. */
        if (record_info == NULL && owner_type->info.points_to != NULL &&
            owner_type->info.points_to->kind == TYPE_KIND_PRIMITIVE) {
          if (owner_type->type_alias != NULL) {
            if (record_info == NULL &&
                owner_type->type_alias->target_type_id != NULL)
              record_info = semcheck_lookup_record_type(
                  ctx->symtab, owner_type->type_alias->target_type_id);
            if (record_info == NULL &&
                owner_type->type_alias->pointer_type_id != NULL)
              record_info = semcheck_lookup_record_type(
                  ctx->symtab, owner_type->type_alias->pointer_type_id);
            if (record_info == NULL &&
                owner_type->type_alias->alias_name != NULL)
              record_info = semcheck_lookup_record_type(
                  ctx->symtab, owner_type->type_alias->alias_name);
          }
          if (record_info == NULL &&
              owner_type->info.points_to->type_alias != NULL) {
            struct TypeAlias *pt_alias = owner_type->info.points_to->type_alias;
            if (record_info == NULL && pt_alias->alias_name != NULL)
              record_info = semcheck_lookup_record_type(ctx->symtab,
                                                        pt_alias->alias_name);
            if (record_info == NULL && pt_alias->target_type_id != NULL)
              record_info = semcheck_lookup_record_type(
                  ctx->symtab, pt_alias->target_type_id);
          }
        }

        /* Fix the KgpcType's points_to so overload resolution sees the
         * correct record type instead of the unresolved primitive placeholder.
         */
        if (record_info != NULL && owner_type->info.points_to != NULL &&
            owner_type->info.points_to->kind != TYPE_KIND_RECORD) {
          KgpcType *old_pointee = owner_type->info.points_to;
          KgpcType *new_pointee = create_record_type(record_info);
          if (new_pointee != NULL) {
            owner_type->info.points_to = new_pointee;
            destroy_kgpc_type(old_pointee);
          }
        }
      }

      /* Fallback: if record_info is still NULL for a method call placeholder,
       * check if first_arg->record_type is set (e.g. for class instances whose
       * KgpcType has an unresolved PRIMITIVE placeholder as points_to). */
      if (record_info == NULL && first_arg != NULL &&
          first_arg->record_type != NULL &&
          record_type_is_class(first_arg->record_type)) {
        record_info = first_arg->record_type;
      }
      if (record_info != NULL && record_info->type_id != NULL) {
        const char *method_name =
            (ctx->expr->expr_data.function_call_data.placeholder_method_name !=
             NULL)
                ? ctx->expr->expr_data.function_call_data
                      .placeholder_method_name
                : ctx->id;
        if (method_name != NULL &&
            (strncasecmp(method_name, "Create", 6) == 0 ||
             strcasecmp(method_name, "Destroy") == 0)) {
          /* Defer constructor/destructor handling to the specialized path
           * below. */
        } else {
          /* Look up the method and capture the actual owner when inherited. */
          struct RecordType *actual_method_owner = NULL;
          HashNode_t *method_node = semcheck_find_class_method(
              ctx->symtab, record_info, method_name, &actual_method_owner);

          /* Check if this is a static method — try the receiver's class first,
           * then the actual owner (inherited static methods are registered
           * under the declaring class, not the receiver's class). */
          int is_static =
              from_cparser_is_method_static(record_info->type_id, method_name);
          if (!is_static && actual_method_owner != NULL &&
              actual_method_owner->type_id != NULL && method_name != NULL) {
            is_static = from_cparser_is_method_static(
                actual_method_owner->type_id, method_name);
          }

          if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr,
                    "[SemCheck] semcheck_funccall: __method call type=%s "
                    "method=%s is_static=%d\n",
                    record_info->type_id, method_name, is_static);
          }

          /* If method not found on the receiver directly, try an applicable
           * helper. */
          struct RecordType *effective_record =
              (actual_method_owner != NULL) ? actual_method_owner : record_info;
          if (method_node == NULL && record_info->type_id != NULL &&
              !record_info->is_type_helper) {
            struct RecordType *helper_record =
                semcheck_lookup_type_helper_for_record_member(
                    ctx->symtab, record_info, method_name);
            if (helper_record != NULL) {
              actual_method_owner = NULL;
              method_node =
                  semcheck_find_class_method(ctx->symtab, helper_record,
                                             method_name, &actual_method_owner);
              if (method_node != NULL) {
                effective_record = (actual_method_owner != NULL)
                                       ? actual_method_owner
                                       : helper_record;
                is_static = from_cparser_is_method_static(
                    helper_record->type_id, method_name);
                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                  fprintf(stderr,
                          "[SemCheck] semcheck_funccall: Found method %s via "
                          "record helper %s\n",
                          method_name, helper_record->type_id);
                }
              }
            }
          }

          if (method_node != NULL) {
            /* Resolve the method name */
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

            /* Prefer all overloads of the resolved method for scoring.
             * IMPORTANT: use base method key (Owner__Method), not a fully
             * mangled signature, otherwise overload resolution sees only one
             * candidate and can pick wrong arity.
             * Walk the entire class hierarchy so that overloads defined at
             * different levels (child + parent) are all visible. */
            char *mangled_method_name = NULL;
            if (effective_record->type_id != NULL && method_name != NULL) {
              size_t class_len = strlen(effective_record->type_id);
              size_t method_len = strlen(method_name);
              mangled_method_name =
                  (char *)malloc(class_len + 2 + method_len + 1);
              if (mangled_method_name != NULL)
                snprintf(mangled_method_name, class_len + 2 + method_len + 1,
                         "%s__%s", effective_record->type_id, method_name);
            } else if (resolved_method_name != NULL) {
              mangled_method_name = strdup(resolved_method_name);
            }

            ListNode_t *method_candidates = NULL;
            if (mangled_method_name != NULL)
              method_candidates =
                  FindAllIdents(ctx->symtab, mangled_method_name);

            /* Collect overloads from parent classes as well.
             * Overloads may be split across multiple hierarchy levels. */
            if (record_info != NULL && method_name != NULL) {
              ListNode_t *hierarchy_candidates =
                  semcheck_collect_hierarchy_method_overloads(
                      ctx->symtab, record_info, method_name);
              if (hierarchy_candidates != NULL)
                semcheck_merge_candidate_lists_dedup(&method_candidates,
                                                     hierarchy_candidates);
            }

            /* Check if ANY overload has Self as first param (instance method).
             * If there are mixed static/instance overloads, don't remove type
             * arg until after overload resolution picks the right one. */
            int any_has_self = 0;
            ListNode_t *cand_cur = method_candidates;
            while (cand_cur != NULL && !any_has_self) {
              HashNode_t *cand = (HashNode_t *)cand_cur->cur;
              if (cand != NULL && cand->type != NULL) {
                ListNode_t *cand_params =
                    kgpc_type_get_procedure_params(cand->type);
                if (cand_params != NULL) {
                  Tree_t *first_param = (Tree_t *)cand_params->cur;
                  if (first_param != NULL &&
                      first_param->type == TREE_VAR_DECL &&
                      first_param->tree_data.var_decl_data.ids != NULL) {
                    const char *first_id =
                        (const char *)
                            first_param->tree_data.var_decl_data.ids->cur;
                    if (first_id != NULL &&
                        pascal_identifier_equals(first_id, "Self"))
                      any_has_self = 1;
                  }
                }
              }
              cand_cur = cand_cur->next;
            }

            /* Strip type receiver for type-qualified method calls when the
             * selected overload set has no implicit Self parameter. This covers
             * static/class methods like TRect.Union(L, R) where the type
             * qualifier is not a runtime argument. */
            int first_arg_is_type_ident = 0;
            if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
                first_arg->expr_data.id != NULL) {
              HashNode_t *first_ident_node = NULL;
              if (FindSymbol(&first_ident_node, ctx->symtab,
                             first_arg->expr_data.id) != 0 &&
                  first_ident_node != NULL &&
                  first_ident_node->hash_type == HASHTYPE_TYPE) {
                first_arg_is_type_ident = 1;
              }
            }
            if (first_arg_is_type_ident && method_candidates != NULL) {
              ListNode_t *filtered_candidates = NULL;
              ListNode_t *filtered_tail = NULL;
              for (ListNode_t *cand_cur2 = method_candidates; cand_cur2 != NULL;
                   cand_cur2 = cand_cur2->next) {
                HashNode_t *cand = (HashNode_t *)cand_cur2->cur;
                if (cand == NULL || cand->type == NULL)
                  continue;
                ListNode_t *cand_params =
                    kgpc_type_get_procedure_params(cand->type);
                Tree_t *cand_first =
                    cand_params != NULL ? (Tree_t *)cand_params->cur : NULL;
                const char *cand_first_name = NULL;
                if (cand_first != NULL && cand_first->type == TREE_VAR_DECL &&
                    cand_first->tree_data.var_decl_data.ids != NULL)
                  cand_first_name =
                      (const char *)
                          cand_first->tree_data.var_decl_data.ids->cur;
                if (cand_first_name != NULL &&
                    pascal_identifier_equals(cand_first_name, "Self"))
                  continue;
                ListNode_t *n = CreateListNode(cand, LIST_UNSPECIFIED);
                if (n == NULL)
                  continue;
                if (filtered_candidates == NULL)
                  filtered_candidates = n;
                else
                  filtered_tail->next = n;
                filtered_tail = n;
              }
              if (filtered_candidates != NULL) {
                DestroyList(method_candidates);
                method_candidates = filtered_candidates;
                any_has_self = 0;
              }
            }
            if (!any_has_self && first_arg_is_type_ident) {
              /* Remove the first argument (the type identifier). */
              ListNode_t *old_head = ctx->args_given;
              ctx->expr->expr_data.function_call_data.args_expr =
                  old_head->next;
              old_head->next = NULL; /* Detach to prevent dangling reference */
              ctx->args_given =
                  ctx->expr->expr_data.function_call_data.args_expr;
              destroy_list(old_head);

              if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] semcheck_funccall: Removed type "
                                "arg for static method call\n");
              }
            } else if (is_static && !any_has_self && !first_arg_is_type_ident &&
                       ctx->args_given != NULL &&
                       ctx->args_given->cur != NULL) {
              /* Static method called via instance variable (e.g.
               * ht.StaticMethod(arg)). The instance is not needed since static
               * methods have no Self parameter. Strip the instance receiver
               * from the argument list. */
              ListNode_t *old_head = ctx->args_given;
              ctx->expr->expr_data.function_call_data.args_expr =
                  old_head->next;
              old_head->next = NULL;
              ctx->args_given =
                  ctx->expr->expr_data.function_call_data.args_expr;
              destroy_list(old_head);

              if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] semcheck_funccall: Removed "
                                "instance arg for static method call\n");
              }
            }

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

            if (method_candidates != NULL) {
              ctx->overload_candidates = method_candidates;
            } else {
              ctx->overload_candidates =
                  CreateListNode(method_node, LIST_UNSPECIFIED);
            }

            if (mangled_method_name != NULL)
              free(mangled_method_name);

            /* If this method is actually a constructor (but not named Create),
             * fix up the return type to ^record instead of procedure. */
            {
              struct RecordType *ctor_search = record_info;
              while (ctor_search != NULL) {
                struct MethodTemplate *tmpl =
                    from_cparser_get_method_template(ctor_search, method_name);
                if (tmpl != NULL) {
                  if (tmpl->kind == METHOD_TEMPLATE_CONSTRUCTOR &&
                      record_type_is_class(record_info) &&
                      !record_info->is_type_helper) {
                    ctx->expr->expr_data.function_call_data
                        .is_constructor_call = 1;
                    KgpcType *rec_type = create_record_type(record_info);
                    if (rec_type != NULL) {
                      KgpcType *ctor_ret = create_pointer_type(rec_type);
                      kgpc_type_release(rec_type);
                      if (ctor_ret != NULL) {
                        semcheck_expr_set_resolved_kgpc_type_shared(ctx->expr,
                                                                    ctor_ret);
                        *ctx->type_return = POINTER_TYPE;
                      }
                    }
                  }
                  break;
                }
                ctor_search =
                    semcheck_lookup_parent_record(ctx->symtab, ctor_search);
              }
            }

            /* Continue with normal function call processing using the resolved
             * method */
            ctx->hash_return = method_node;
            return FC_OVERLOAD_RESOLVE;
          }

          /* Method not found — check if this is a procedural-type field being
           * invoked. For example: ThreadManager.ThreadSwitch(...) where
           * ThreadSwitch is a field of type TThreadSwitchHandler (a procedural
           * type). */
          {
            struct RecordField *proc_field = NULL;
            if (resolve_record_field(ctx->symtab, record_info, method_name,
                                     &proc_field, NULL, ctx->expr->line_num,
                                     1 /* silent */) == 0 &&
                proc_field != NULL) {
              /* Check if the field actually has a procedural type */
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
                /* Remove first_arg from args list (it becomes part of field
                 * access) */
                ListNode_t *old_head = ctx->args_given;
                ctx->args_given = old_head->next;
                old_head->next = NULL;
                old_head->cur = NULL; /* Don't free first_arg, we reuse it */
                free(old_head);
                ctx->expr->expr_data.function_call_data.args_expr =
                    ctx->args_given;

                /* Build a record-access expression: first_arg.field_name */
                struct Expression *field_access = mk_recordaccess(
                    ctx->expr->line_num,
                    first_arg, /* Transfer ownership from args list */
                    strdup(method_name));
                assert(field_access != NULL);

                /* Resolve the field access expression to get its procedural
                 * type */
                KgpcType *field_kgpc = NULL;
                semcheck_expr_with_type(&field_kgpc, ctx->symtab, field_access,
                                        ctx->max_scope_lev, NO_MUTATE);

                /* Set return type from the procedural type */
                KgpcType *ret = proc_kgpc_type->info.proc_info.return_type;
                if (ret == NULL &&
                    proc_kgpc_type->info.proc_info.return_type_id != NULL) {
                  HashNode_t *ret_node = semcheck_find_preferred_type_node(
                      ctx->symtab,
                      proc_kgpc_type->info.proc_info.return_type_id);
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

                /* Type-check the arguments */
                for (ListNode_t *arg_cur = ctx->args_given; arg_cur != NULL;
                     arg_cur = arg_cur->next) {
                  struct Expression *arg = (struct Expression *)arg_cur->cur;
                  if (arg != NULL)
                    semcheck_expr_with_type(NULL, ctx->symtab, arg,
                                            ctx->max_scope_lev, NO_MUTATE);
                }

                do {
                  ctx->final_status = ctx->return_val;
                  return FC_CLEANUP;
                } while (0);
              }
            }
          }
        }
      } else {
        const char *type_name = get_expr_type_name(first_arg, ctx->symtab);
        struct RecordType *helper_record = semcheck_lookup_type_helper(
            ctx->symtab, first_arg_type_tag, type_name);
        if (helper_record != NULL && helper_record->type_id != NULL) {
          record_info = helper_record;
          /* Retry helper method lookup */
          const char *method_name =
              (ctx->expr->expr_data.function_call_data
                   .placeholder_method_name != NULL)
                  ? ctx->expr->expr_data.function_call_data
                        .placeholder_method_name
                  : ctx->id;
          if (method_name != NULL &&
              (strncasecmp(method_name, "Create", 6) == 0 ||
               strcasecmp(method_name, "Destroy") == 0)) {
            /* Defer constructor/destructor handling to the specialized path
             * below. */
          } else {
            /* Use owner_out to get the actual owner where the method was found
             * (may be parent helper) */
            struct RecordType *actual_method_owner = NULL;
            HashNode_t *method_node = semcheck_find_class_method(
                ctx->symtab, record_info, method_name, &actual_method_owner);
            if (method_node != NULL) {
              set_type_from_hashtype(ctx->type_return, method_node);
              semcheck_expr_set_resolved_kgpc_type_shared(ctx->expr,
                                                          method_node->type);
              ctx->expr->expr_data.function_call_data.resolved_func =
                  method_node;
              const char *resolved_method_name =
                  (method_node->mangled_id != NULL) ? method_node->mangled_id
                                                    : method_node->id;
              if (ctx->expr->expr_data.function_call_data.mangled_id != NULL)
                free(ctx->expr->expr_data.function_call_data.mangled_id);
              ctx->expr->expr_data.function_call_data.mangled_id =
                  (resolved_method_name != NULL) ? strdup(resolved_method_name)
                                                 : NULL;

              /* Use actual_method_owner for base key (Owner__Method) so
               * overload resolution can see all overloads on the owner. */
              struct RecordType *record_for_mangling =
                  (actual_method_owner != NULL) ? actual_method_owner
                                                : record_info;
              char *mangled_method_name = NULL;
              if (record_for_mangling->type_id != NULL && method_name != NULL) {
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
                method_candidates =
                    FindAllIdents(ctx->symtab, mangled_method_name);

              /* Check if ANY overload has Self as first param (instance
               * method). If there are mixed static/instance overloads, don't
               * remove type arg until after overload resolution picks the right
               * one. */
              int any_has_self = 0;
              ListNode_t *cand_cur = method_candidates;
              while (cand_cur != NULL && !any_has_self) {
                HashNode_t *cand = (HashNode_t *)cand_cur->cur;
                if (cand != NULL && cand->type != NULL) {
                  ListNode_t *cand_params =
                      kgpc_type_get_procedure_params(cand->type);
                  if (cand_params != NULL) {
                    Tree_t *first_param = (Tree_t *)cand_params->cur;
                    if (first_param != NULL &&
                        first_param->type == TREE_VAR_DECL &&
                        first_param->tree_data.var_decl_data.ids != NULL) {
                      const char *first_id =
                          (const char *)
                              first_param->tree_data.var_decl_data.ids->cur;
                      if (first_id != NULL &&
                          pascal_identifier_equals(first_id, "Self"))
                        any_has_self = 1;
                    }
                  }
                }
                cand_cur = cand_cur->next;
              }

              /* Strip type receiver when overloads have no implicit Self
               * parameter. */
              int first_arg_is_type_ident = 0;
              if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
                  first_arg->expr_data.id != NULL) {
                HashNode_t *first_ident_node = NULL;
                if (FindSymbol(&first_ident_node, ctx->symtab,
                               first_arg->expr_data.id) != 0 &&
                    first_ident_node != NULL &&
                    first_ident_node->hash_type == HASHTYPE_TYPE) {
                  first_arg_is_type_ident = 1;
                }
              }
              if (first_arg_is_type_ident && method_candidates != NULL) {
                ListNode_t *filtered_candidates = NULL;
                ListNode_t *filtered_tail = NULL;
                for (ListNode_t *cand_cur2 = method_candidates;
                     cand_cur2 != NULL; cand_cur2 = cand_cur2->next) {
                  HashNode_t *cand = (HashNode_t *)cand_cur2->cur;
                  if (cand == NULL || cand->type == NULL)
                    continue;
                  ListNode_t *cand_params =
                      kgpc_type_get_procedure_params(cand->type);
                  Tree_t *cand_first =
                      cand_params != NULL ? (Tree_t *)cand_params->cur : NULL;
                  const char *cand_first_name = NULL;
                  if (cand_first != NULL && cand_first->type == TREE_VAR_DECL &&
                      cand_first->tree_data.var_decl_data.ids != NULL)
                    cand_first_name =
                        (const char *)
                            cand_first->tree_data.var_decl_data.ids->cur;
                  if (cand_first_name != NULL &&
                      pascal_identifier_equals(cand_first_name, "Self"))
                    continue;
                  ListNode_t *n = CreateListNode(cand, LIST_UNSPECIFIED);
                  if (n == NULL)
                    continue;
                  if (filtered_candidates == NULL)
                    filtered_candidates = n;
                  else
                    filtered_tail->next = n;
                  filtered_tail = n;
                }
                if (filtered_candidates != NULL) {
                  DestroyList(method_candidates);
                  method_candidates = filtered_candidates;
                  any_has_self = 0;
                }
              }
              if (!any_has_self && first_arg_is_type_ident) {
                ListNode_t *old_head = ctx->args_given;
                ctx->expr->expr_data.function_call_data.args_expr =
                    old_head->next;
                old_head->next = NULL;
                ctx->args_given =
                    ctx->expr->expr_data.function_call_data.args_expr;
                destroy_list(old_head);
              }

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
          }
        }
      }
    }
  }
  return FC_CONSTRUCTOR;
}

FunccallState funccall_state_constructor(FunccallCtx *ctx) {
  /* Check for Constructor Call (Create) where first arg is the class
   * type/instance Also check for static method calls like
   * TCounter.GetDefaultValue where the first arg is a type identifier and the
   * method is declared as static */
  int is_potential_static_method_call = 0;
  int is_potential_class_method_call = 0;
  if (ctx->id != NULL && ctx->args_given != NULL) {
    struct Expression *first_arg = (struct Expression *)ctx->args_given->cur;
    if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
        first_arg->expr_data.id != NULL) {
      /* Check if first arg is a type identifier */
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, ctx->symtab, first_arg->expr_data.id) != 0 &&
          type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) {
        /* It's a type - check if there's a static method with this name */
        struct RecordType *record_info = get_record_type_from_node(type_node);
        if (record_info != NULL && record_info->type_id != NULL) {
          /* Check if the method exists and is static */
          if (from_cparser_is_method_static(record_info->type_id, ctx->id)) {
            is_potential_static_method_call = 1;
          } else if (from_cparser_is_method_class_method(record_info->type_id,
                                                         ctx->id)) {
            is_potential_class_method_call = 1;
            if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
              fprintf(stderr,
                      "[SemCheck] semcheck_funccall: detected class/static "
                      "method call %s.%s\n",
                      record_info->type_id, ctx->id);
            }
          }
        }
      }
    }
  }

  if (ctx->id != NULL &&
      (strncasecmp(ctx->id, "Create", 6) == 0 ||
       strcasecmp(ctx->id, "Destroy") == 0 || is_potential_static_method_call ||
       is_potential_class_method_call) &&
      ctx->args_given != NULL) {
    struct Expression *first_arg = (struct Expression *)ctx->args_given->cur;
    int first_arg_type_tag;
    KgpcType *first_arg_kgpc_type_ctor = NULL;
    semcheck_expr_with_type(&first_arg_kgpc_type_ctor, ctx->symtab, first_arg,
                            ctx->max_scope_lev, NO_MUTATE);
    first_arg_type_tag = semcheck_tag_from_kgpc(first_arg_kgpc_type_ctor);
    (void)first_arg_type_tag; /* Variable is used for potential debugging */

    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
      fprintf(stderr,
              "[SemCheck] semcheck_funccall: first_arg=%p type=%d id=%s "
              "resolved_kgpc_type=%p\n",
              (void *)first_arg, first_arg->type,
              (first_arg->type == EXPR_VAR_ID) ? first_arg->expr_data.id
                                               : "N/A",
              first_arg->resolved_kgpc_type);
    }

    /* Check if first arg is a TYPE (for class constructor) or INSTANCE (for
     * method) */
    /* Actually, for MyException.Create, MyException is a TYPE (if static call)
     * or VAR (if instance call) */
    /* If it's a TYPE, resolved_kgpc_type should be the class type? */
    /* Wait, if MyException is a TYPE, semcheck_expr returns TYPE_KIND_TYPE? */
    /* Or the type tag of the type? */

    /* Get record info from resolved KgpcType metadata. */
    KgpcType *owner_type = first_arg->resolved_kgpc_type;
    struct RecordType *record_info = NULL;

    if (owner_type != NULL) {
      if (owner_type->kind == TYPE_KIND_RECORD) {
        record_info = owner_type->info.record_info;
      } else if (owner_type->kind == TYPE_KIND_POINTER &&
                 owner_type->info.points_to != NULL &&
                 owner_type->info.points_to->kind == TYPE_KIND_RECORD) {
        record_info = owner_type->info.points_to->info.record_info;
      }
    }

    /* If still unresolved and the first arg is a type identifier, resolve it as
     * a type name. */
    if (record_info == NULL && first_arg->type == EXPR_VAR_ID &&
        first_arg->expr_data.id != NULL) {
      HashNode_t *type_node = semcheck_find_preferred_type_node(
          ctx->symtab, first_arg->expr_data.id);
      if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) {
        record_info = get_record_type_from_node(type_node);
        if (owner_type == NULL && type_node->type != NULL)
          owner_type = type_node->type;
      }
    }

    /* Metaclass fallback: when owner_type is pointer-to-primitive(RECORD_TYPE)
     * or a primitive POINTER_TYPE, try to find the target class from type
     * aliases or expression metadata. This handles "class of T" constructor
     * calls. */
    if (record_info == NULL && owner_type != NULL) {
      /* Case 1: TYPE_KIND_POINTER -> TYPE_KIND_PRIMITIVE(RECORD_TYPE) */
      if (owner_type->kind == TYPE_KIND_POINTER &&
          owner_type->info.points_to != NULL &&
          owner_type->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
          owner_type->info.points_to->info.primitive_type_tag == RECORD_TYPE) {
        if (owner_type->type_alias != NULL &&
            owner_type->type_alias->pointer_type_id != NULL)
          record_info = semcheck_lookup_record_type(
              ctx->symtab, owner_type->type_alias->pointer_type_id);
        if (record_info == NULL && first_arg->pointer_subtype_id != NULL)
          record_info = semcheck_lookup_record_type(
              ctx->symtab, first_arg->pointer_subtype_id);
      }
      /* Case 2: TYPE_KIND_PRIMITIVE(POINTER_TYPE) - unresolved pointer type */
      if (record_info == NULL && owner_type->kind == TYPE_KIND_PRIMITIVE &&
          owner_type->info.primitive_type_tag == POINTER_TYPE) {
        if (owner_type->type_alias != NULL &&
            owner_type->type_alias->pointer_type_id != NULL)
          record_info = semcheck_lookup_record_type(
              ctx->symtab, owner_type->type_alias->pointer_type_id);
        if (record_info == NULL && first_arg->pointer_subtype_id != NULL)
          record_info = semcheck_lookup_record_type(
              ctx->symtab, first_arg->pointer_subtype_id);
      }
      /* Case 3: TYPE_KIND_POINTER -> TYPE_KIND_POINTER -> TYPE_KIND_RECORD
       * This handles "class of T" resolved as pointer-to-(pointer-to-record).
       * Also try TYPE_KIND_POINTER with type_alias fallback. */
      if (record_info == NULL && owner_type->kind == TYPE_KIND_POINTER) {
        KgpcType *pointee = owner_type->info.points_to;
        if (pointee != NULL && pointee->kind == TYPE_KIND_POINTER &&
            pointee->info.points_to != NULL &&
            pointee->info.points_to->kind == TYPE_KIND_RECORD) {
          record_info = kgpc_type_get_record(pointee->info.points_to);
        }
        if (record_info == NULL && pointee != NULL &&
            pointee->kind == TYPE_KIND_RECORD) {
          record_info = kgpc_type_get_record(pointee);
        }
        if (record_info == NULL && owner_type->type_alias != NULL &&
            owner_type->type_alias->pointer_type_id != NULL) {
          record_info = semcheck_lookup_record_type(
              ctx->symtab, owner_type->type_alias->pointer_type_id);
        }
        if (record_info == NULL && first_arg->pointer_subtype_id != NULL) {
          record_info = semcheck_lookup_record_type(
              ctx->symtab, first_arg->pointer_subtype_id);
        }
      }
    }

    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
      fprintf(stderr,
              "[SemCheck] ctor resolve: record_info=%s owner_type=%p kind=%d\n",
              (record_info != NULL && record_info->type_id != NULL)
                  ? record_info->type_id
                  : "<null>",
              (void *)owner_type, owner_type != NULL ? owner_type->kind : -1);
    }

    if (record_info != NULL && record_info->type_id != NULL) {
      /* Ensure owner_type represents the class instance pointer for
       * constructors. Explicit receivers like `cordconstnode.create(...)` may
       * arrive as metaclass values (`class of T`) rather than the instance
       * pointer expected by the hidden Self parameter. */
      if (record_type_is_class(record_info) && !record_info->is_type_helper) {
        int owner_is_instance_ptr = 0;
        if (owner_type != NULL && owner_type->kind == TYPE_KIND_POINTER &&
            owner_type->info.points_to != NULL &&
            owner_type->info.points_to->kind == TYPE_KIND_RECORD &&
            kgpc_type_get_record(owner_type->info.points_to) == record_info) {
          owner_is_instance_ptr = 1;
        }

        if (!owner_is_instance_ptr) {
          KgpcType *rec_type = create_record_type(record_info);
          if (rec_type != NULL) {
            KgpcType *ptr_type = create_pointer_type(rec_type);
            kgpc_type_release(rec_type);
            if (ptr_type != NULL) {
              owner_type = ptr_type;
            }
          }
        }
      }
      struct RecordType *method_owner = record_info;
      ListNode_t *method_candidates = NULL;
      char *mangled_method_name = NULL;

      /* In FPC, the most recently declared type helper for a type
       * shadows all earlier helpers.  Check the active helper FIRST
       * so that a program-level helper overrides unit-level helpers
       * whose methods were already merged into the record. */
      if (record_info != NULL && record_info->type_id != NULL &&
          !record_type_is_class(record_info)) {
        struct RecordType *helper_record = semcheck_lookup_type_helper(
            ctx->symtab, UNKNOWN_TYPE, record_info->type_id);
        struct RecordType *actual_method_owner = NULL;
        if (helper_record != NULL) {
          HashNode_t *method_node = semcheck_find_class_method(
              ctx->symtab, helper_record, ctx->id, &actual_method_owner);
          struct RecordType *owner_for_mangle = (actual_method_owner != NULL)
                                                    ? actual_method_owner
                                                    : helper_record;
          if (method_node != NULL && owner_for_mangle != NULL &&
              owner_for_mangle->type_id != NULL) {
            size_t class_len = strlen(owner_for_mangle->type_id);
            size_t method_len = strlen(ctx->id);
            char *candidate_name =
                (char *)malloc(class_len + 2 + method_len + 1);
            if (candidate_name != NULL) {
              snprintf(candidate_name, class_len + 2 + method_len + 1, "%s__%s",
                       owner_for_mangle->type_id, ctx->id);
              ListNode_t *candidates =
                  FindAllIdents(ctx->symtab, candidate_name);
              if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr,
                        "[SemCheck] semcheck_funccall: helper '%s' found %d "
                        "candidates\n",
                        candidate_name, ListLength(candidates));
              }
              if (candidates != NULL) {
                method_candidates = candidates;
                mangled_method_name = candidate_name;
                method_owner = owner_for_mangle;
              } else {
                free(candidate_name);
              }
            }
          }
        }
      }

      /* Fall back to methods on the record/class itself (including
       * methods merged from earlier unit-level helpers). */
      if (method_candidates == NULL) {
        while (method_owner != NULL && method_owner->type_id != NULL) {
          size_t class_len = strlen(method_owner->type_id);
          size_t method_len = strlen(ctx->id);
          char *candidate_name = (char *)malloc(class_len + 2 + method_len + 1);
          if (candidate_name == NULL)
            break;
          snprintf(candidate_name, class_len + 2 + method_len + 1, "%s__%s",
                   method_owner->type_id, ctx->id);

          ListNode_t *candidates = FindAllIdents(ctx->symtab, candidate_name);
          if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr,
                    "[SemCheck] semcheck_funccall: Looking for '%s' found %d "
                    "candidates\n",
                    candidate_name, ListLength(candidates));
          }

          if (candidates != NULL) {
            method_candidates = candidates;
            mangled_method_name = candidate_name;
            break;
          }

          free(candidate_name);
          method_owner =
              semcheck_lookup_parent_record(ctx->symtab, method_owner);
        }
      }

      if (method_candidates != NULL && mangled_method_name != NULL) {
        /* Found at least one method overload */
        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
          fprintf(stderr,
                  "[SemCheck] semcheck_funccall: Found constructor/method %s "
                  "in class\n",
                  ctx->id);
        }

        /* Check if this is a static method (class function with static
         * modifier) */
        int is_static_method =
            from_cparser_is_method_static(method_owner->type_id, ctx->id);
        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
          fprintf(
              stderr,
              "[SemCheck] semcheck_funccall: is_static_method=%d for %s.%s\n",
              is_static_method, method_owner->type_id, ctx->id);
        }

        int is_nonstatic_class_method =
            (!is_static_method && from_cparser_is_method_class_method(
                                      method_owner->type_id, ctx->id));
        int method_is_declared_constructor =
            semcheck_method_is_declared_constructor(ctx->symtab, method_owner,
                                                    ctx->id);
        if (is_nonstatic_class_method) {
          ctx->expr->expr_data.function_call_data.is_class_method_call = 1;
          if (ctx->expr->expr_data.function_call_data.self_class_name == NULL)
            ctx->expr->expr_data.function_call_data.self_class_name =
                strdup(method_owner->type_id);
        } else if (is_static_method || method_is_declared_constructor) {
          if (ctx->expr->expr_data.function_call_data
                  .constructor_receiver_expr != NULL) {
            destroy_expr(ctx->expr->expr_data.function_call_data
                             .constructor_receiver_expr);
            ctx->expr->expr_data.function_call_data.constructor_receiver_expr =
                NULL;
          }
          ctx->expr->expr_data.function_call_data.constructor_receiver_expr =
              clone_expression(first_arg);
          /* Remove the first argument (the class reference) from the argument
           * list since it's not a real argument to a static method or
           * constructor. */
          ListNode_t *old_head = ctx->args_given;
          ctx->expr->expr_data.function_call_data.args_expr = old_head->next;
          old_head->next = NULL; /* Detach to prevent dangling reference */
          destroy_list(old_head);
          ListNode_t *user_args =
              ctx->expr->expr_data.function_call_data.args_expr;
          ctx->args_given =
              user_args; /* Update args_given to reflect removed type arg */

          /* For non-static constructors, add a placeholder Self argument at the
           * front. Constructors have Self as first parameter, but from user's
           * perspective they don't pass Self - it's implicitly created. Static
           * factory methods (class function Create: T; static;) do NOT have
           * Self. We use EXPR_NIL as the placeholder - codegen will allocate
           * memory. */
          if (method_is_declared_constructor) {
            struct Expression *self_placeholder =
                (struct Expression *)calloc(1, sizeof(struct Expression));
            if (self_placeholder != NULL) {
              /* Use nil as the placeholder - codegen will handle actual
               * allocation */
              self_placeholder->type = EXPR_NIL;
              semcheck_expr_set_resolved_type(self_placeholder, POINTER_TYPE);
              self_placeholder->line_num = ctx->expr->line_num;
              /* Set the resolved_kgpc_type to match the class type for proper
               * type matching */
              if (owner_type != NULL) {
                kgpc_type_retain(owner_type);
                self_placeholder->resolved_kgpc_type = owner_type;
              }
              ListNode_t *self_node =
                  CreateListNode(self_placeholder, LIST_EXPR);
              if (self_node != NULL) {
                self_node->next = user_args;
                ctx->expr->expr_data.function_call_data.args_expr = self_node;
                ctx->args_given = self_node;
              }
            }
          }
        } else if (ctx->expr->expr_data.function_call_data
                       .constructor_receiver_expr != NULL) {
          destroy_expr(ctx->expr->expr_data.function_call_data
                           .constructor_receiver_expr);
          ctx->expr->expr_data.function_call_data.constructor_receiver_expr =
              NULL;
        }

        /* Update the function call id to the mangled name */
        if (ctx->expr->expr_data.function_call_data.id != NULL)
          free(ctx->expr->expr_data.function_call_data.id);
        ctx->expr->expr_data.function_call_data.id =
            strdup(mangled_method_name);
        if (ctx->expr->expr_data.function_call_data.mangled_id != NULL)
          free(ctx->expr->expr_data.function_call_data.mangled_id);
        ctx->expr->expr_data.function_call_data.mangled_id =
            strdup(mangled_method_name);
        ctx->id = ctx->expr->expr_data.function_call_data.id;

        /* Set up overload candidates for normal resolution */
        ctx->overload_candidates = method_candidates;

        /* For constructors (Create, CreateFmt, etc.), set up the return type.
         * Get the bare method name from the candidate's structured identity. */
        const char *method_name = NULL;
        if (method_candidates != NULL && method_candidates->cur != NULL) {
          HashNode_t *first_candidate = (HashNode_t *)method_candidates->cur;
          if (first_candidate->method_name != NULL)
            method_name = first_candidate->method_name;
        }
        if (method_name == NULL &&
            ctx->expr->expr_data.function_call_data.placeholder_method_name !=
                NULL)
          method_name =
              ctx->expr->expr_data.function_call_data.placeholder_method_name;
        if (method_name == NULL)
          method_name = ctx->id;
        /* Verify the method was actually declared as a constructor
         * (using the 'constructor' keyword), not just a function whose
         * name happens to start with "Create" (e.g. CreateDriver). */
        method_is_declared_constructor =
            semcheck_method_is_declared_constructor(ctx->symtab, record_info,
                                                    method_name);
        if (method_is_declared_constructor && owner_type != NULL) {
          if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr,
                    "[SemCheck] semcheck_funccall: Setting up return type for "
                    "constructor %s\n",
                    method_name);
          }
          ctx->expr->expr_data.function_call_data.is_constructor_call = 1;

          /* Return type is the static class reference used at the call site,
           * even if the constructor is inherited from a base class. */
          KgpcType *ctor_return_type = NULL;
          int return_type_owned = 0;
          if (record_info != NULL && record_type_is_class(record_info) &&
              !record_info->is_type_helper) {
            KgpcType *rec_type = create_record_type(record_info);
            if (rec_type != NULL) {
              ctor_return_type = create_pointer_type(rec_type);
              kgpc_type_release(rec_type);
              if (ctor_return_type != NULL) {
                return_type_owned = 1;
              }
            }
          }
          if (ctor_return_type == NULL)
            ctor_return_type = owner_type;

          semcheck_expr_set_resolved_kgpc_type_shared(ctx->expr,
                                                      ctor_return_type);
          *ctx->type_return = semcheck_tag_from_kgpc(ctor_return_type);
          if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            struct RecordType *debug_record = NULL;
            if (ctor_return_type != NULL &&
                ctor_return_type->kind == TYPE_KIND_POINTER &&
                ctor_return_type->info.points_to != NULL &&
                ctor_return_type->info.points_to->kind == TYPE_KIND_RECORD)
              debug_record = ctor_return_type->info.points_to->info.record_info;
            else if (ctor_return_type != NULL &&
                     ctor_return_type->kind == TYPE_KIND_RECORD)
              debug_record = ctor_return_type->info.record_info;
            fprintf(stderr, "[SemCheck] ctor return set: kind=%d record=%s\n",
                    ctx->expr->resolved_kgpc_type != NULL
                        ? ctx->expr->resolved_kgpc_type->kind
                        : -1,
                    (debug_record != NULL && debug_record->type_id != NULL)
                        ? debug_record->type_id
                        : "<null>");
          }
          if (return_type_owned && ctor_return_type != NULL)
            destroy_kgpc_type(ctor_return_type);
        }

        free(mangled_method_name);
        /* Continue to normal overload resolution */
        return FC_OVERLOAD_RESOLVE;
      }
      if (mangled_method_name != NULL)
        free(mangled_method_name);
    }
  }

  return FC_OVERLOAD_SETUP;
}
