/*
    SemCheck_stmt_main.c - Main statement dispatch and assignment semantic
   checking

    Extracted from SemCheck_stmt.c. Contains semcheck_stmt_main,
   semcheck_varassign, semcheck_stmt, and property assignment functions.
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
#include "../../../identifier_utils.h"
#include "../../../unit_registry.h"
#include "../../ParseTree/ident_ref.h"
#include "../../ParseTree/tree_types.h"
#include "../../ParseTree/type_tags.h"
#include "../HashTable/HashTable.h"
#include "../NameMangling.h"
#include "../SemCheck.h"
#include "../SymTab/SymTab.h"
#include "SemCheck_overload.h"
#include "SemCheck_sizeof.h"
#include "SemCheck_stmt_internal.h"
#include <math.h>
/* WithContextEntry_fwd must also be declared here */
struct WithContextEntry_fwd {
  struct Expression *context_expr;
  struct RecordType *record_type;
};
extern struct WithContextEntry_fwd *with_context_stack;
extern int semcheck_loop_depth;
extern struct Statement *g_debug_watch_stmt;
extern struct Expression *g_debug_watch_to_expr;
#include "../../ParseTree/from_cparser.h"
#include "../../ParseTree/generic_types.h"
#include "../../ParseTree/tree.h"

void semcheck_debug_expr_brief(const struct Expression *expr,
                               const char *label);
struct RecordType *get_record_type_from_node(HashNode_t *node);
struct RecordType *semcheck_lookup_record_type(SymTab_t *symtab,
                                               const char *type_id);
struct TypeAlias *get_type_alias_from_node(HashNode_t *node);
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
int semcheck_typecheck_array_literal(struct Expression *expr, SymTab_t *symtab,
                                     int max_scope_lev, int expected_type,
                                     const char *expected_type_id,
                                     int line_num);
int semcheck_convert_set_literal_to_array_literal(struct Expression *expr);
int semcheck_class_type_ids_compatible(SymTab_t *symtab, const char *formal_id,
                                       const char *actual_id);
const char *semcheck_get_current_subprogram_id(void);
const char *semcheck_get_current_subprogram_result_var_name(void);
const char *semcheck_get_current_subprogram_method_name(void);
KgpcType *semcheck_get_current_subprogram_return_kgpc_type(SymTab_t *symtab,
                                                           int *owned);

#define SEMSTMT_TIMINGS_ENABLED()                                              \
  (kgpc_getenv("KGPC_DEBUG_SEMSTMT_TIMINGS") != NULL)

static double semstmt_now_ms(void) {
  return (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
}

int semcheck_stmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev) {
  if (stmt != NULL && stmt->type == STMT_PROCEDURE_CALL) {
    int is_dispose = 0;
    struct Statement *extra_stmt =
        transform_two_arg_new_dispose(stmt, &is_dispose);
    if (extra_stmt != NULL) {
      struct Statement *base_stmt =
          (struct Statement *)calloc(1, sizeof(struct Statement));
      ListNode_t *first = (ListNode_t *)calloc(1, sizeof(ListNode_t));
      ListNode_t *second = (ListNode_t *)calloc(1, sizeof(ListNode_t));
      if (base_stmt == NULL || first == NULL || second == NULL) {
        if (base_stmt != NULL)
          free(base_stmt);
        if (first != NULL)
          free(first);
        if (second != NULL)
          free(second);
        destroy_stmt(extra_stmt);
        semcheck_error_with_context_at(
            stmt->line_num, stmt->col_num, stmt->source_index,
            "Error on line %d, unable to allocate statement nodes for "
            "New/Dispose transform.\n",
            stmt->line_num);
        return 1;
      }

      *base_stmt = *stmt;
      first->type = LIST_STMT;
      second->type = LIST_STMT;
      if (is_dispose) {
        first->cur = extra_stmt;
        second->cur = base_stmt;
      } else {
        first->cur = base_stmt;
        second->cur = extra_stmt;
      }
      first->next = second;
      second->next = NULL;

      stmt->type = STMT_COMPOUND_STATEMENT;
      stmt->stmt_data.compound_statement = first;
    }
  }

  int ret = semcheck_stmt_main(symtab, stmt, max_scope_lev);
  if (ret > 0 && kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL && stmt != NULL) {
    fprintf(
        stderr,
        "[KGPC_DEBUG_ERRORS] stmt_error type=%d line=%d col=%d src=%d ret=%d\n",
        stmt->type, stmt->line_num, stmt->col_num, stmt->source_index, ret);
  }
  return ret;
}

/* Semantic check on a function statement (no side effects allowed) */
int semcheck_func_stmt(SymTab_t *symtab, struct Statement *stmt,
                       int max_scope_lev) {
  int ret = semcheck_stmt_main(symtab, stmt, max_scope_lev);
  if (ret > 0 && kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL && stmt != NULL) {
    fprintf(stderr,
            "[KGPC_DEBUG_ERRORS] func_stmt_error type=%d line=%d col=%d src=%d "
            "ret=%d\n",
            stmt->type, stmt->line_num, stmt->col_num, stmt->source_index, ret);
  }
  return ret;
}

static int semcheck_break_stmt(struct Statement *stmt) {
  if (semcheck_loop_depth <= 0) {
    if (stmt != NULL)
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, Break is only valid inside a loop.\n",
          stmt->line_num);
    return 1;
  }
  return 0;
}

static int semcheck_continue_stmt(struct Statement *stmt) {
  if (semcheck_loop_depth <= 0) {
    if (stmt != NULL)
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, Continue is only valid inside a loop.\n",
          stmt->line_num);
    return 1;
  }
  return 0;
}

/* Main semantic checking */

int semcheck_stmt_main(SymTab_t *symtab, struct Statement *stmt,
                       int max_scope_lev) {
  int return_val;

  assert(symtab != NULL);
  if (stmt == NULL)
    return 0;

  static long semcheck_stmt_counter = 0;
  static long semcheck_stmt_limit = -1;
  static int semcheck_stmt_limit_inited = 0;
  static int semcheck_stmt_log_enabled = -1;
  static int semcheck_stmt_verbose = -1;
  if (!semcheck_stmt_limit_inited) {
    const char *limit_env = kgpc_getenv("KGPC_DEBUG_SEMSTMT_LIMIT");
    if (limit_env != NULL)
      semcheck_stmt_limit = atol(limit_env);
    semcheck_stmt_limit_inited = 1;
  }
  if (semcheck_stmt_log_enabled == -1) {
    semcheck_stmt_log_enabled = kgpc_getenv("KGPC_DEBUG_SEMSTMT") != NULL;
  }
  if (semcheck_stmt_verbose == -1) {
    semcheck_stmt_verbose = kgpc_getenv("KGPC_DEBUG_SEMSTMT_VERBOSE") != NULL;
  }
  semcheck_stmt_counter++;
  if (semcheck_stmt_verbose) {
    fprintf(stderr, "[semcheck_stmt] enter type=%d line=%d col=%d\n",
            stmt->type, stmt->line_num, stmt->col_num);
  }
  if (semcheck_stmt_log_enabled && (semcheck_stmt_counter % 10000) == 0) {
    fprintf(stderr, "[semcheck_stmt] count=%ld last_type=%d line=%d\n",
            semcheck_stmt_counter, stmt->type, stmt->line_num);
  }
  if (semcheck_stmt_limit > 0 && semcheck_stmt_counter > semcheck_stmt_limit) {
    fprintf(stderr,
            "ERROR: semcheck_stmt exceeded limit (%ld) at type=%d line=%d.\n",
            semcheck_stmt_limit, stmt->type, stmt->line_num);
    return 1;
  }

  semcheck_set_error_context(stmt->line_num, stmt->col_num, stmt->source_index);

  // In semcheck_for:
  // semcheck_loop_depth++;
  //
  // fprintf(stderr, "DEBUG: semcheck_for stmt=%p line=%d to_expr=%p
  // current_to=%p\n",
  //         stmt, stmt->line_num, to_expr, stmt->stmt_data.for_data.to);
  //
  // if (stmt->line_num == 42) {
  //     watch_stmt = stmt;
  //     watch_to_expr = stmt->stmt_data.for_data.to;
  //     fprintf(stderr, "DEBUG: Watching stmt at line 42\n");
  // }
  //
  // if (to_expr != NULL && ((uintptr_t)to_expr == 0x686374616d ||
  // (uintptr_t)to_expr == 0x1db2)) {
  //     fprintf(stderr, "CRITICAL: to_expr is corrupted in semcheck_for!\n");
  // }
  //
  // return_val += semcheck_stmt_main(symtab, do_for, max_scope_lev);
  // semcheck_loop_depth--;
  //
  // if (stmt->stmt_data.for_data.to != to_expr) {
  //     fprintf(stderr, "CRITICAL: stmt->stmt_data.for_data.to changed from %p
  //     to %p during body processing!\n",
  //             to_expr, stmt->stmt_data.for_data.to);
  // }
  //
  // if (watch_stmt == stmt) {
  //     // We are returning from the watched statement.
  //     // It might be checked again in outer loops, but that's fine.
  // }

  return_val = 0;
  switch (stmt->type) {
  case STMT_VAR_ASSIGN:
    return_val += semcheck_varassign(symtab, stmt, max_scope_lev);
    break;

  case STMT_PROCEDURE_CALL:
    if (stmt->stmt_data.procedure_call_data.id != NULL &&
        pascal_identifier_equals(stmt->stmt_data.procedure_call_data.id,
                                 "fail") &&
        stmt->stmt_data.procedure_call_data.expr_args == NULL &&
        semcheck_get_current_subprogram_is_constructor()) {
      stmt->type = STMT_EXIT;
      memset(&stmt->stmt_data, 0, sizeof(stmt->stmt_data));
      stmt->stmt_data.exit_data.return_expr = NULL;
      return_val += semcheck_stmt_main(symtab, stmt, max_scope_lev);
      break;
    }
    return_val += semcheck_proccall(symtab, stmt, max_scope_lev);
    break;

  case STMT_EXPR:
    if (stmt->stmt_data.expr_stmt_data.expr != NULL) {
      int expr_type;
      return_val += semcheck_stmt_expr_tag(&expr_type, symtab,
                                           stmt->stmt_data.expr_stmt_data.expr,
                                           max_scope_lev, 0);
    }
    break;

  case STMT_COMPOUND_STATEMENT:
    return_val += semcheck_compoundstmt(symtab, stmt, max_scope_lev);
    break;

  case STMT_LABEL:
    if (stmt->stmt_data.label_data.stmt != NULL)
      return_val += semcheck_stmt_main(symtab, stmt->stmt_data.label_data.stmt,
                                       max_scope_lev);
    break;

  case STMT_GOTO: {
    const char *label_name = stmt->stmt_data.goto_data.label;
    const char *scope_name =
        semcheck_label_scope_name(semcheck_get_current_subprogram_id());
    HashNode_t *label_symbol = NULL;
    char *symbol_id;

    if (label_name == NULL || label_name[0] == '\0') {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, goto target label is missing.\n", stmt->line_num);
      ++return_val;
      break;
    }

    symbol_id = semcheck_build_label_symbol_id(scope_name, label_name);
    if (symbol_id == NULL) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, failed to allocate goto label lookup for '%s'.\n",
          stmt->line_num, label_name);
      ++return_val;
      break;
    }

    if (FindSymbol(&label_symbol, symtab, symbol_id) == 0 ||
        label_symbol == NULL) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, goto target label '%s' not declared in scope.\n",
          stmt->line_num, label_name);
      ++return_val;
    } else if (label_symbol->hash_type != HASHTYPE_CONST ||
               label_symbol->type != NULL) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, goto target '%s' is not a label declaration.\n",
          stmt->line_num, label_name);
      ++return_val;
    }
    free(symbol_id);
  } break;

  case STMT_IF_THEN:
    return_val += semcheck_ifthen(symtab, stmt, max_scope_lev);
    break;

  case STMT_WHILE:
    return_val += semcheck_while(symtab, stmt, max_scope_lev);
    break;

  case STMT_REPEAT:
    return_val += semcheck_repeat(symtab, stmt, max_scope_lev);
    break;

  case STMT_FOR:
    return_val += semcheck_for(symtab, stmt, max_scope_lev);
    break;

  case STMT_FOR_IN:
    return_val += semcheck_for_in(symtab, stmt, max_scope_lev);
    break;

  case STMT_BREAK:
    return_val = semcheck_break_stmt(stmt);
    break;
  case STMT_CONTINUE:
    return_val = semcheck_continue_stmt(stmt);
    break;

  case STMT_ASM_BLOCK:
    /* No semantic checking needed for asm blocks */
    break;

  case STMT_EXIT:
    /* Exit statement with optional return expression */
    {
      struct Expression *return_expr = stmt->stmt_data.exit_data.return_expr;
      if (return_expr != NULL) {
        /* Type-check the return expression */
        int expr_type;
        return_val += semcheck_stmt_expr_tag(&expr_type, symtab, return_expr,
                                             max_scope_lev, 0);

        /* Mark Result as assigned if we're in a function context */
        HashNode_t *result_node = NULL;
        if (FindSymbol(&result_node, symtab, "Result") != 0 &&
            result_node != NULL) {
          result_node->mutated = MUTATE;
          if (result_node->type != NULL &&
              return_expr->resolved_kgpc_type != NULL) {
            int return_owned = 0;
            struct Expression *result_expr =
                mk_varid(stmt->line_num, strdup("Result"));
            if (!are_types_compatible_for_assignment(
                    result_node->type, return_expr->resolved_kgpc_type,
                    symtab)) {
              /* Use a local KgpcType* tracker so that the rewriter's
               * `*source_type = return_type` does NOT clobber the
               * arg expression's resolved_kgpc_type.  If we passed
               * `&return_expr->resolved_kgpc_type` here and the
               * rewriter succeeded, return_expr (which becomes the
               * single arg of the synthesized op_assign call) would
               * end up tagged with the call's record return type,
               * causing codegen to treat the integer literal as a
               * by-value record argument. */
              KgpcType *conv_type = return_expr->resolved_kgpc_type;
              if (!semcheck_try_record_conversion_expression(
                      symtab, &stmt->stmt_data.exit_data.return_expr,
                      result_expr, result_node->type, &conv_type,
                      &return_owned)) {
                semcheck_error_with_context_at(
                    stmt->line_num, stmt->col_num, stmt->source_index,
                    "Error on line %d, incompatible return type in exit().\n",
                    stmt->line_num);
                ++return_val;
              }
              if (return_owned && conv_type != NULL)
                destroy_kgpc_type(conv_type);
            }
            destroy_expr(result_expr);
          }
        }
      }
    }
    break;

  case STMT_CASE:
    /* Check the selector expression */
    {
      int selector_type;
      return_val += semcheck_stmt_expr_tag(
          &selector_type, symtab, stmt->stmt_data.case_data.selector_expr,
          max_scope_lev, 0);
    }

    /* Check each case branch */
    {
      ListNode_t *branch_node = stmt->stmt_data.case_data.branches;
      while (branch_node != NULL) {
        struct CaseBranch *branch = (struct CaseBranch *)branch_node->cur;
        if (branch != NULL) {
          /* Check case labels */
          ListNode_t *label_node = branch->labels;
          while (label_node != NULL) {
            if (label_node->type == LIST_EXPR) {
              struct Expression *label_expr =
                  (struct Expression *)label_node->cur;
              int label_type;
              return_val += semcheck_stmt_expr_tag(
                  &label_type, symtab, label_expr, max_scope_lev, 0);
            } else if (label_node->type == LIST_SET_ELEMENT) {
              struct SetElement *range = (struct SetElement *)label_node->cur;
              if (range != NULL) {
                if (range->lower != NULL) {
                  int lower_type;
                  return_val += semcheck_stmt_expr_tag(
                      &lower_type, symtab, range->lower, max_scope_lev, 0);
                }
                if (range->upper != NULL) {
                  int upper_type;
                  return_val += semcheck_stmt_expr_tag(
                      &upper_type, symtab, range->upper, max_scope_lev, 0);
                }
              }
            }
            label_node = label_node->next;
          }
          /* Check the branch statement */
          if (branch->stmt != NULL)
            return_val += semcheck_stmt(symtab, branch->stmt, max_scope_lev);
        }
        branch_node = branch_node->next;
      }
    }

    /* Check the else statement if present */
    if (stmt->stmt_data.case_data.else_stmt != NULL)
      return_val += semcheck_stmt(symtab, stmt->stmt_data.case_data.else_stmt,
                                  max_scope_lev);
    break;

  case STMT_WITH: {
    struct Expression *context_expr = stmt->stmt_data.with_data.context_expr;
    struct Statement *body_stmt = stmt->stmt_data.with_data.body_stmt;
    struct RecordType *record_info = NULL;
    int ctx_type = UNKNOWN_TYPE;
    int pushed = 0;

    if (context_expr == NULL) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, WITH statement requires a context expression.\n\n",
          stmt->line_num);
      ++return_val;
    } else {
      return_val += semcheck_stmt_expr_tag(&ctx_type, symtab, context_expr,
                                           max_scope_lev, NO_MUTATE);
      record_info = semcheck_with_resolve_record_type(symtab, context_expr,
                                                      ctx_type, stmt->line_num);
      if (record_info == NULL) {
        fprintf(stderr,
                "Error on line %d, WITH context must be a record or pointer to "
                "a record.\n\n",
                stmt->line_num);
        ++return_val;
      } else {
        if (context_expr->resolved_kgpc_type == NULL)
          context_expr->resolved_kgpc_type = create_record_type(record_info);
        if (semcheck_with_push(context_expr, record_info) != 0) {
          ++return_val;
        } else {
          pushed = 1;
        }
      }
    }

    if (body_stmt != NULL)
      return_val += semcheck_stmt_main(symtab, body_stmt, max_scope_lev);

    if (pushed)
      semcheck_with_pop();
    break;
  }

  case STMT_TRY_FINALLY:
    return_val += semcheck_statement_list_nodes(
        symtab, stmt->stmt_data.try_finally_data.try_statements, max_scope_lev);
    return_val += semcheck_statement_list_nodes(
        symtab, stmt->stmt_data.try_finally_data.finally_statements,
        max_scope_lev);
    break;

  case STMT_TRY_EXCEPT:
    return_val += semcheck_statement_list_nodes(
        symtab, stmt->stmt_data.try_except_data.try_statements, max_scope_lev);
    return_val += semcheck_statement_list_nodes(
        symtab, stmt->stmt_data.try_except_data.except_statements,
        max_scope_lev);
    break;

  case STMT_ON_EXCEPTION:
    if (stmt->stmt_data.on_exception_data.exception_var_name != NULL) {
      char *var_name = stmt->stmt_data.on_exception_data.exception_var_name;
      char *type_name = stmt->stmt_data.on_exception_data.exception_type_name;
      KgpcType *var_kgpc_type = NULL;

      EnterScope(symtab, 0);

      if (type_name != NULL) {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, type_name) != 0 &&
            type_node != NULL) {
          if (type_node->hash_type == HASHTYPE_TYPE) {
            var_kgpc_type = type_node->type;
          } else {
            fprintf(stderr, "Error: '%s' is not a type at line %d\n", type_name,
                    stmt->line_num);
            return_val++;
          }
        } else {
          fprintf(stderr, "Error: Unknown exception type '%s' at line %d\n",
                  type_name, stmt->line_num);
          return_val++;
        }
      }

      PushVarOntoScope_Typed(symtab, var_name, var_kgpc_type);
      if (stmt->stmt_data.on_exception_data.handler_stmt != NULL)
        return_val += semcheck_stmt_main(
            symtab, stmt->stmt_data.on_exception_data.handler_stmt,
            max_scope_lev);
      LeaveScope(symtab);
    } else if (stmt->stmt_data.on_exception_data.handler_stmt != NULL) {
      return_val += semcheck_stmt_main(
          symtab, stmt->stmt_data.on_exception_data.handler_stmt,
          max_scope_lev);
    }
    break;

  case STMT_RAISE:
    if (stmt->stmt_data.raise_data.exception_expr != NULL) {
      int raise_type = UNKNOWN_TYPE;
      return_val += semcheck_stmt_expr_tag(
          &raise_type, symtab, stmt->stmt_data.raise_data.exception_expr,
          INT_MAX, NO_MUTATE);
    }
    break;

  case STMT_INHERITED:
    if (stmt->stmt_data.inherited_data.call_expr == NULL) {
      semcheck_error_with_context_at(stmt->line_num, stmt->col_num,
                                     stmt->source_index,
                                     "Error on line %d, inherited statement "
                                     "has no resolved call target.\n\n",
                                     stmt->line_num);
      return ++return_val;
    }
    if (stmt->stmt_data.inherited_data.call_expr != NULL) {
      struct Expression *call_expr = stmt->stmt_data.inherited_data.call_expr;
      if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL) {
        const char *cid = NULL;
        if (call_expr->type == EXPR_FUNCTION_CALL)
          cid = call_expr->expr_data.function_call_data.id;
        else if (call_expr->type == EXPR_VAR_ID)
          cid = call_expr->expr_data.id;
        fprintf(stderr, "[INHERITED] stmt line=%d call=%s\n", stmt->line_num,
                cid ? cid : "<null>");
      }

      /* Handle EXPR_VAR_ID by converting to EXPR_FUNCTION_CALL */
      if (call_expr->type == EXPR_VAR_ID) {
        /* Save the id from the VAR_ID before converting */
        char *var_id = call_expr->expr_data.id;

        /* Convert to EXPR_FUNCTION_CALL */
        call_expr->type = EXPR_FUNCTION_CALL;
        memset(&call_expr->expr_data.function_call_data, 0,
               sizeof(call_expr->expr_data.function_call_data));
        call_expr->expr_data.function_call_data.id = var_id;
        call_expr->expr_data.function_call_data.args_expr = NULL;
        call_expr->expr_data.function_call_data.mangled_id = NULL;
        call_expr->expr_data.function_call_data.resolved_func = NULL;
        call_expr->expr_data.function_call_data.call_hash_type = 0;
        call_expr->expr_data.function_call_data.call_kgpc_type = NULL;
        call_expr->expr_data.function_call_data.is_call_info_valid = 0;
      }

      if (call_expr->type == EXPR_FUNCTION_CALL) {
        if (call_expr->expr_data.function_call_data.args_expr == NULL &&
            call_expr->expr_data.function_call_data.is_bare_inherited) {
          ListNode_t *forwarded_args =
              semcheck_clone_current_subprogram_actual_args(0);
          if (forwarded_args != NULL)
            call_expr->expr_data.function_call_data.args_expr = forwarded_args;
        }

        if (1) {
          /* For inherited procedure calls, check if we need to handle
           * Create/Destroy with no parent */
          const char *method_name = call_expr->expr_data.function_call_data.id;
          const char *owner_name_from_node = NULL;
          {
            HashNode_t *call_method_node = NULL;
            if (method_name != NULL &&
                FindSymbol(&call_method_node, symtab, method_name) != 0 &&
                call_method_node != NULL) {
              if (call_method_node->method_name != NULL)
                method_name = call_method_node->method_name;
              owner_name_from_node = call_method_node->owner_class;
            }
          }
          HashNode_t *self_node = NULL;
          const char *parent_class_name = NULL;
          struct RecordType *current_class = NULL;

          if (FindSymbol(&self_node, symtab, "Self") != 0 &&
              self_node != NULL && self_node->type != NULL) {
            /* Handle both direct records and pointers to records (classes) */
            if (self_node->type->kind == TYPE_KIND_RECORD &&
                self_node->type->info.record_info != NULL) {
              current_class = self_node->type->info.record_info;
            } else if (self_node->type->kind == TYPE_KIND_POINTER &&
                       self_node->type->info.points_to != NULL &&
                       self_node->type->info.points_to->kind ==
                           TYPE_KIND_RECORD &&
                       self_node->type->info.points_to->info.record_info !=
                           NULL) {
              current_class = self_node->type->info.points_to->info.record_info;
            }

            if (current_class != NULL) {
              parent_class_name = current_class->parent_class_name;
              if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL &&
                  method_name != NULL &&
                  strcasecmp(method_name, "Create") == 0) {
                fprintf(stderr, "[INHERITED] class=%s parent=%s\n",
                        current_class->type_id ? current_class->type_id
                                               : "<null>",
                        parent_class_name ? parent_class_name : "<null>");
              }

              /* Check if there's no parent class and this is Create or Destroy
               */
              if (current_class->parent_class_name == NULL &&
                  method_name != NULL &&
                  (strcasecmp(method_name, "Create") == 0 ||
                   strcasecmp(method_name, "Destroy") == 0)) {
                /* No parent class - convert to empty compound statement (no-op)
                 */
                if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL) {
                  fprintf(stderr,
                          "[KGPC] Inherited %s with no parent class - "
                          "converting to no-op\n",
                          method_name);
                }
                /* Convert this inherited statement to an empty compound
                 * statement */
                stmt->type = STMT_COMPOUND_STATEMENT;
                stmt->stmt_data.compound_statement = NULL;
                /* No errors */
                break;
              }
            }
          }
          if (current_class == NULL) {
            const char *owner_id = semcheck_get_current_method_owner();
            if (owner_id == NULL && owner_name_from_node != NULL)
              owner_id = owner_name_from_node;
            if (owner_id != NULL) {
              HashNode_t *owner_node = NULL;
              if (FindSymbol(&owner_node, symtab, owner_id) != 0 &&
                  owner_node != NULL)
                current_class =
                    semcheck_stmt_get_record_type_from_node(owner_node);
              if (current_class != NULL)
                parent_class_name = current_class->parent_class_name;
            }
          }
          if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL &&
              method_name != NULL && strcasecmp(method_name, "Create") == 0) {
            fprintf(stderr, "[INHERITED] resolved class=%s parent=%s\n",
                    current_class && current_class->type_id
                        ? current_class->type_id
                        : "<null>",
                    parent_class_name ? parent_class_name : "<null>");
          }
          /* method_name and owner_name_from_node point to HashNode fields,
           * no need to free */

          /* If a parent exists, call the parent class method */
          HashNode_t *parent_method_node = NULL;
          char parent_mangled[512];
          parent_mangled[0] = '\0';
          if (parent_class_name != NULL && method_name != NULL) {
            const char *search_parent = parent_class_name;
            while (search_parent != NULL && parent_method_node == NULL) {
              snprintf(parent_mangled, sizeof(parent_mangled), "%s__%s",
                       search_parent, method_name);

              /* Prefer overload resolution by call-site signature */
              ListNode_t *parent_candidates =
                  FindAllIdents(symtab, parent_mangled);
              if (parent_candidates != NULL) {
                /* Build temp args including Self to match method signatures */
                struct Expression *self_expr =
                    mk_varid(stmt->line_num, strdup("Self"));
                ListNode_t *self_arg = CreateListNode(self_expr, LIST_EXPR);
                self_arg->next =
                    call_expr->expr_data.function_call_data.args_expr;

                char *call_mangled = MangleFunctionNameFromCallSite(
                    parent_mangled, self_arg, symtab, INT_MAX);
                if (call_mangled != NULL) {
                  for (ListNode_t *cur = parent_candidates; cur != NULL;
                       cur = cur->next) {
                    HashNode_t *candidate = (HashNode_t *)cur->cur;
                    if (candidate != NULL && candidate->mangled_id != NULL &&
                        strcmp(candidate->mangled_id, call_mangled) == 0) {
                      parent_method_node = candidate;
                      break;
                    }
                  }
                  free(call_mangled);
                }

                /* Overload resolution path: use semcheck_resolve_overload when
                 * exact mangling doesn't match (e.g., dynamic array types with
                 * different alias names) */
                if (parent_method_node == NULL) {
                  HashNode_t *best_match = NULL;
                  int num_best = 0;
                  semcheck_resolve_overload(&best_match, &num_best,
                                            parent_candidates, self_arg, symtab,
                                            call_expr, INT_MAX, 0);
                  if (best_match != NULL && num_best == 1)
                    parent_method_node = best_match;
                }

                self_arg->next = NULL;
                destroy_expr(self_expr);
                free(self_arg);

                DestroyList(parent_candidates);
              } else {
                if (FindSymbol(&parent_method_node, symtab, parent_mangled) ==
                    0)
                  parent_method_node = NULL;
              }

              if (parent_method_node == NULL) {
                HashNode_t *parent_node = NULL;
                if (FindSymbol(&parent_node, symtab, (char *)search_parent) !=
                        0 &&
                    parent_node != NULL) {
                  struct RecordType *parent_record =
                      semcheck_stmt_get_record_type_from_node(parent_node);
                  search_parent =
                      parent_record ? parent_record->parent_class_name : NULL;
                } else {
                  search_parent = NULL;
                }
              }
            }

            if (parent_method_node == NULL) {
              if (call_expr->expr_data.function_call_data.is_bare_inherited) {
                stmt->stmt_data.inherited_data.call_expr = NULL;
                destroy_expr(call_expr);
                break;
              }
              semcheck_error_with_context_at(
                  stmt->line_num, stmt->col_num, stmt->source_index,
                  "Error on line %d, inherited call to %s has no matching "
                  "overload.\n\n",
                  stmt->line_num,
                  parent_mangled[0] != '\0'
                      ? parent_mangled
                      : (method_name != NULL ? method_name : "(unknown)"));
              return ++return_val;
            }

            if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL) {
              fprintf(stderr,
                      "[INHERITED] Looking for parent method: %s, found: %s\n",
                      parent_mangled,
                      parent_method_node != NULL ? "YES" : "NO");
              if (parent_method_node != NULL) {
                fprintf(stderr, "[INHERITED] Parent method mangled_id: %s\n",
                        parent_method_node->mangled_id
                            ? parent_method_node->mangled_id
                            : "(null)");
                fprintf(stderr, "[INHERITED] Parent method id: %s\n",
                        parent_method_node->id ? parent_method_node->id
                                               : "(null)");
              }
            }
          }

          /* Create temporary argument list for inherited calls without
           * modifying original AST */
          ListNode_t *temp_args = NULL;
          ListNode_t *temp_self_arg = NULL;

          if (parent_method_node != NULL) {
            /* Only prepend Self if parent method was found */
            struct Expression *self_expr =
                mk_varid(stmt->line_num, strdup("Self"));
            temp_self_arg = CreateListNode(self_expr, LIST_EXPR);
            temp_self_arg->next =
                call_expr->expr_data.function_call_data.args_expr;
            temp_args = temp_self_arg;
          } else {
            /* Use original arguments for non-inherited calls */
            temp_args = call_expr->expr_data.function_call_data.args_expr;
          }
          struct Statement temp_call;
          int temp_call_id_owned = 0;
          memset(&temp_call, 0, sizeof(temp_call));
          temp_call.type = STMT_PROCEDURE_CALL;
          temp_call.line_num = stmt->line_num;
          /* For inherited calls, use the parent method's id as the procedure ID
           * for symbol table lookup, and set mangled_id to prevent re-mangling.
           * IMPORTANT: always duplicate mangled_id, because semcheck_proccall
           * may replace/free stmt-owned call names. Borrowing symbol-table
           * storage here risks corrupting method identifiers globally. */
          if (parent_method_node != NULL && parent_method_node->id != NULL) {
            temp_call.stmt_data.procedure_call_data.id =
                strdup(parent_method_node->id);
            if (temp_call.stmt_data.procedure_call_data.id == NULL) {
              semcheck_error_with_context_at(
                  stmt->line_num, stmt->col_num, stmt->source_index,
                  "Error on line %d, out of memory while resolving inherited "
                  "call.\n\n",
                  stmt->line_num);
              return ++return_val;
            }
            temp_call_id_owned = 1;
            /* Pre-set mangled_id to prevent type-based method correction and
             * re-mangling. */
            if (parent_method_node->mangled_id != NULL)
              temp_call.stmt_data.procedure_call_data.mangled_id =
                  strdup(parent_method_node->mangled_id);
            else
              temp_call.stmt_data.procedure_call_data.mangled_id =
                  strdup(parent_method_node->id);
            if (temp_call.stmt_data.procedure_call_data.mangled_id == NULL) {
              semcheck_error_with_context_at(
                  stmt->line_num, stmt->col_num, stmt->source_index,
                  "Error on line %d, out of memory while resolving inherited "
                  "call.\n\n",
                  stmt->line_num);
              return ++return_val;
            }
          } else {
            temp_call.stmt_data.procedure_call_data.id =
                call_expr->expr_data.function_call_data.id;
            temp_call.stmt_data.procedure_call_data.mangled_id = NULL;
          }
          temp_call.stmt_data.procedure_call_data.expr_args = temp_args;
          temp_call.stmt_data.procedure_call_data.resolved_proc = NULL;

          char *method_name_for_constructor_check =
              method_name != NULL ? strdup(method_name) : NULL;
          if (method_name != NULL &&
              method_name_for_constructor_check == NULL) {
            semcheck_error_with_context_at(
                stmt->line_num, stmt->col_num, stmt->source_index,
                "Error on line %d, out of memory while resolving inherited "
                "constructor call.\n\n",
                stmt->line_num);
            return ++return_val;
          }

          if (parent_method_node != NULL && call_expr != NULL &&
              call_expr->type == EXPR_FUNCTION_CALL) {
            call_expr->expr_data.function_call_data.args_expr = temp_args;
            if (parent_method_node->id != NULL) {
              if (call_expr->expr_data.function_call_data.id != NULL)
                free(call_expr->expr_data.function_call_data.id);
              call_expr->expr_data.function_call_data.id =
                  strdup(parent_method_node->id);
            }
            if (parent_method_node->mangled_id != NULL) {
              if (call_expr->expr_data.function_call_data.mangled_id != NULL)
                free(call_expr->expr_data.function_call_data.mangled_id);
              call_expr->expr_data.function_call_data.mangled_id =
                  strdup(parent_method_node->mangled_id);
            }
          }

          return_val += semcheck_proccall(symtab, &temp_call, max_scope_lev);

          if (parent_method_node != NULL &&
              method_name_for_constructor_check != NULL) {
            struct RecordType *parent_owner_record = NULL;
            if (parent_method_node->owner_class != NULL)
              parent_owner_record = semcheck_lookup_record_type(
                  symtab, parent_method_node->owner_class);
            if (parent_owner_record == NULL && parent_class_name != NULL)
              parent_owner_record =
                  semcheck_lookup_record_type(symtab, parent_class_name);

            if (parent_owner_record != NULL &&
                semcheck_stmt_method_is_declared_constructor(
                    symtab, parent_owner_record,
                    method_name_for_constructor_check)) {
              if (call_expr != NULL && call_expr->type == EXPR_FUNCTION_CALL) {
                call_expr->expr_data.function_call_data.is_constructor_call = 1;

                if (parent_owner_record->parent_class_name == NULL ||
                    pascal_identifier_equals(method_name_for_constructor_check,
                                             "Create")) {
                  if (call_expr->expr_data.function_call_data
                          .constructor_receiver_expr != NULL)
                    destroy_expr(call_expr->expr_data.function_call_data
                                     .constructor_receiver_expr);
                  call_expr->expr_data.function_call_data
                      .constructor_receiver_expr =
                      mk_varid(stmt->line_num, strdup("Self"));
                  if (call_expr->expr_data.function_call_data
                          .constructor_receiver_expr == NULL) {
                    semcheck_error_with_context_at(
                        stmt->line_num, stmt->col_num, stmt->source_index,
                        "Error on line %d, out of memory while resolving "
                        "inherited constructor call.\n\n",
                        stmt->line_num);
                    return ++return_val;
                  }
                }
              }
            }
          }

          free(method_name_for_constructor_check);

          if (temp_call_id_owned &&
              temp_call.stmt_data.procedure_call_data.id != NULL) {
            free(temp_call.stmt_data.procedure_call_data.id);
            temp_call.stmt_data.procedure_call_data.id = NULL;
          }

          /* Clean up temporary argument node if we created one */
          if (temp_self_arg != NULL) {
            /* temp_self_arg now belongs to call_expr/temp_args for codegen. */
          }

          if (temp_call.stmt_data.procedure_call_data.mangled_id != NULL) {
            if (call_expr->expr_data.function_call_data.mangled_id != NULL) {
              free(call_expr->expr_data.function_call_data.mangled_id);
              call_expr->expr_data.function_call_data.mangled_id = NULL;
            }
            call_expr->expr_data.function_call_data.mangled_id =
                temp_call.stmt_data.procedure_call_data.mangled_id;
            temp_call.stmt_data.procedure_call_data.mangled_id = NULL;
          }
          call_expr->expr_data.function_call_data.call_hash_type =
              temp_call.stmt_data.procedure_call_data.call_hash_type;
          if (call_expr->expr_data.function_call_data.call_kgpc_type != NULL) {
            destroy_kgpc_type(
                call_expr->expr_data.function_call_data.call_kgpc_type);
            call_expr->expr_data.function_call_data.call_kgpc_type = NULL;
          }
          if (temp_call.stmt_data.procedure_call_data.call_kgpc_type != NULL) {
            kgpc_type_retain(
                temp_call.stmt_data.procedure_call_data.call_kgpc_type);
            call_expr->expr_data.function_call_data.call_kgpc_type =
                temp_call.stmt_data.procedure_call_data.call_kgpc_type;
          }
          call_expr->expr_data.function_call_data.is_call_info_valid =
              temp_call.stmt_data.procedure_call_data.is_call_info_valid;
          semcheck_stmt_set_call_kgpc_type(
              &temp_call, NULL,
              temp_call.stmt_data.procedure_call_data.is_call_info_valid == 1);
          temp_call.stmt_data.procedure_call_data.is_call_info_valid = 0;
          free(temp_call.stmt_data.procedure_call_data.cached_owner_class);
          temp_call.stmt_data.procedure_call_data.cached_owner_class = NULL;
          free(temp_call.stmt_data.procedure_call_data.cached_method_name);
          temp_call.stmt_data.procedure_call_data.cached_method_name = NULL;
          free(temp_call.stmt_data.procedure_call_data.self_class_name);
          temp_call.stmt_data.procedure_call_data.self_class_name = NULL;
          free(temp_call.stmt_data.procedure_call_data.constructor_class_name);
          temp_call.stmt_data.procedure_call_data.constructor_class_name = NULL;
          free(temp_call.stmt_data.procedure_call_data.call_qualifier);
          temp_call.stmt_data.procedure_call_data.call_qualifier = NULL;
        }
      } else {
        /* For other expression types, use general expression checking */
        int expr_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&expr_type, symtab, call_expr,
                                             max_scope_lev, NO_MUTATE);
      }
    }
    break;

  default:
    assert(0 && "Bad type in semcheck_stmt!");
    break;
  }

  return return_val;
}

/****** STMT SEMCHECKS *******/

/** VAR_ASSIGN **/
int semcheck_varassign(SymTab_t *symtab, struct Statement *stmt,
                       int max_scope_lev) {
  int return_val;
  int type_first, type_second;
  struct Expression *var, *expr;
  int lhs_was_typecast;

  assert(symtab != NULL);
  assert(stmt != NULL);
  assert(stmt->type == STMT_VAR_ASSIGN);

  return_val = 0;

  int module_property_result =
      semcheck_try_module_property_assignment(symtab, stmt, max_scope_lev);
  if (module_property_result >= 0)
    return module_property_result;

  var = stmt->stmt_data.var_assign_data.var;
  expr = stmt->stmt_data.var_assign_data.expr;
  lhs_was_typecast = (var != NULL && var->type == EXPR_TYPECAST);

  rewrite_tfpglist_constructor_if_needed(symtab, max_scope_lev, var,
                                         &stmt->stmt_data.var_assign_data.expr);
  expr = stmt->stmt_data.var_assign_data.expr;

  /* NOTE: Grammar will make sure the left side is a variable */
  /* Left side var assigns must abide by scoping rules */
  if (SEMSTMT_TIMINGS_ENABLED()) {
    double t0 = semstmt_now_ms();
    int before_lhs = return_val;
    return_val +=
        semcheck_stmt_expr_tag(&type_first, symtab, var, max_scope_lev, MUTATE);
    fprintf(
        stderr,
        "[timing] varassign lhs semcheck_stmt_expr_tag: %.2f ms (line=%d)\n",
        semstmt_now_ms() - t0, stmt->line_num);
    if (kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL && return_val > before_lhs &&
        var != NULL) {
      fprintf(stderr,
              "[KGPC_DEBUG_ERRORS] varassign_lhs_error line=%d expr_type=%d\n",
              stmt->line_num, var->type);
      if (var->type == EXPR_RECORD_ACCESS &&
          var->expr_data.record_access_data.field_id != NULL)
        fprintf(stderr, "[KGPC_DEBUG_ERRORS]   lhs record field=%s\n",
                var->expr_data.record_access_data.field_id);
    }
  } else {
    int before_lhs = return_val;
    return_val +=
        semcheck_stmt_expr_tag(&type_first, symtab, var, max_scope_lev, MUTATE);
    if (kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL && return_val > before_lhs &&
        var != NULL) {
      fprintf(stderr,
              "[KGPC_DEBUG_ERRORS] varassign_lhs_error line=%d expr_type=%d\n",
              stmt->line_num, var->type);
      if (var->type == EXPR_RECORD_ACCESS &&
          var->expr_data.record_access_data.field_id != NULL)
        fprintf(stderr, "[KGPC_DEBUG_ERRORS]   lhs record field=%s\n",
                var->expr_data.record_access_data.field_id);
    }
  }

  semcheck_maybe_promote_index0_string_var_to_shortstring(symtab, stmt);

  if (var != NULL && var->type == EXPR_TYPECAST) {
    struct Expression *inner = var->expr_data.typecast_data.expr;
    if (inner != NULL) {
      /* Don't strip string typecasts on pointer targets (e.g.,
       * RawByteString(ptr):='') — codegen needs the typecast to know
       * this is a string assignment, not a direct pointer store. */
      int target_type = var->expr_data.typecast_data.target_type;
      int inner_prim_tag =
          (inner->resolved_kgpc_type != NULL)
              ? inner->resolved_kgpc_type->info.primitive_type_tag
              : UNKNOWN_TYPE;
      int is_string_to_pointer =
          is_string_type(target_type) &&
          (inner_prim_tag == POINTER_TYPE || inner_prim_tag == UNKNOWN_TYPE);

      int compatible = 0;
      KgpcType *lhs_type = var->resolved_kgpc_type;
      KgpcType *inner_type = inner->resolved_kgpc_type;
      if (lhs_type != NULL && inner_type != NULL)
        compatible =
            are_types_compatible_for_assignment(lhs_type, inner_type, symtab);
      else if (lhs_type == NULL && inner_type == NULL)
        compatible = 1;
      if (!compatible) {
        TypeRef *target_ref = var->expr_data.typecast_data.target_type_ref;
        if (target_ref != NULL && target_ref->num_generic_args > 0)
          compatible = 1;
      }
      if (compatible && !is_string_to_pointer) {
        stmt->stmt_data.var_assign_data.var = inner;
        var->expr_data.typecast_data.expr = NULL;
        destroy_expr(var);
        var = inner;
      }
    }
  }

  /* Check for record property assignment early, before RHS type checking.
   * This handles plain record (Delphi advanced record) properties with setter
   * methods. Must happen after LHS is evaluated but before type compatibility
   * checks. */
  {
    int property_result =
        semcheck_try_property_assignment(symtab, stmt, max_scope_lev);
    if (property_result >= 0)
      return return_val + property_result;
  }
  {
    int indexed_property_result =
        semcheck_try_indexed_property_assignment(symtab, stmt, max_scope_lev);
    if (indexed_property_result >= 0)
      return return_val + indexed_property_result;
  }

  /* Re-read var and expr from the statement after the property checks and
   * LHS semcheck_stmt_expr_tag.  The LHS expression may have been
   * transformed in-place (e.g. VAR_ID -> RECORD_ACCESS via WITH, or
   * FUNCTION_CALL -> TYPECAST), and the property helpers may in the future
   * rewrite the statement even on the failure path.  Keeping the local
   * pointers in sync avoids stale-pointer / use-after-free crashes. */
  var = stmt->stmt_data.var_assign_data.var;
  expr = stmt->stmt_data.var_assign_data.expr;

  if (expr != NULL && expr->type == EXPR_RECORD_CONSTRUCTOR &&
      (expr->resolved_kgpc_type == NULL ||
       !kgpc_type_is_record(expr->resolved_kgpc_type))) {
    struct RecordType *record_type = NULL;
    if (var != NULL && var->resolved_kgpc_type != NULL) {
      KgpcType *lhs_type = var->resolved_kgpc_type;
      if (kgpc_type_is_record(lhs_type))
        record_type = kgpc_type_get_record(lhs_type);
      else if (kgpc_type_is_pointer(lhs_type) &&
               lhs_type->info.points_to != NULL &&
               kgpc_type_is_record(lhs_type->info.points_to))
        record_type = kgpc_type_get_record(lhs_type->info.points_to);
    }
    if (record_type == NULL && var != NULL && var->type == EXPR_VAR_ID &&
        var->expr_data.id != NULL) {
      HashNode_t *var_node = NULL;
      if (FindSymbol(&var_node, symtab, var->expr_data.id) != 0 &&
          var_node != NULL) {
        record_type = hashnode_get_record_type(var_node);
        if (record_type == NULL) {
          struct TypeAlias *alias = hashnode_get_type_alias(var_node);
          if (alias != NULL && alias->target_type_id != NULL) {
            HashNode_t *target_node = NULL;
            if (FindSymbol(&target_node, symtab, alias->target_type_id) != 0 &&
                target_node != NULL)
              record_type = hashnode_get_record_type(target_node);
          }
        }
      }
    }
    if (record_type == NULL && var != NULL && var->resolved_kgpc_type != NULL) {
      const char *record_id =
          semcheck_record_type_id_from_kgpc(var->resolved_kgpc_type);
      if (record_id != NULL)
        record_type = semcheck_lookup_record_type(symtab, record_id);
    }
    if (record_type == NULL && var != NULL) {
      int lhs_owned = 0;
      KgpcType *lhs_type = semcheck_resolve_expression_kgpc_type(
          symtab, var, max_scope_lev, MUTATE, &lhs_owned);
      if (lhs_type != NULL) {
        if (kgpc_type_is_record(lhs_type))
          record_type = kgpc_type_get_record(lhs_type);
        else if (kgpc_type_is_pointer(lhs_type) &&
                 lhs_type->info.points_to != NULL &&
                 kgpc_type_is_record(lhs_type->info.points_to))
          record_type = kgpc_type_get_record(lhs_type->info.points_to);
        if (record_type == NULL) {
          const char *record_id = semcheck_record_type_id_from_kgpc(lhs_type);
          if (record_id != NULL)
            record_type = semcheck_lookup_record_type(symtab, record_id);
        }
      }
      if (lhs_owned && lhs_type != NULL)
        destroy_kgpc_type(lhs_type);
    }
    if (record_type == NULL && var != NULL && var->type == EXPR_RECORD_ACCESS) {
      struct Expression *record_expr =
          var->expr_data.record_access_data.record_expr;
      const char *field_id = var->expr_data.record_access_data.field_id;
      int record_owned = 0;
      KgpcType *record_expr_type = semcheck_resolve_expression_kgpc_type(
          symtab, record_expr, max_scope_lev, MUTATE, &record_owned);
      if (record_expr_type != NULL) {
        if (kgpc_type_is_pointer(record_expr_type) &&
            record_expr_type->info.points_to != NULL)
          record_expr_type = record_expr_type->info.points_to;
        if (kgpc_type_is_record(record_expr_type) && field_id != NULL) {
          struct RecordType *record_info =
              kgpc_type_get_record(record_expr_type);
          struct RecordField *field_desc = NULL;
          long long field_offset = 0;
          if (record_info != NULL &&
              resolve_record_field(symtab, record_info, field_id, &field_desc,
                                   &field_offset, stmt->line_num, 0) == 0 &&
              field_desc != NULL) {
            if (field_desc->nested_record != NULL) {
              record_type = field_desc->nested_record;
            } else if (field_desc->type_id != NULL) {
              record_type =
                  semcheck_lookup_record_type(symtab, field_desc->type_id);
              if (record_type == NULL) {
                HashNode_t *alias_node = NULL;
                if (FindSymbol(&alias_node, symtab, field_desc->type_id) != 0 &&
                    alias_node != NULL) {
                  struct TypeAlias *alias =
                      get_type_alias_from_node(alias_node);
                  if (alias != NULL && alias->target_type_id != NULL) {
                    record_type = semcheck_lookup_record_type(
                        symtab, alias->target_type_id);
                  }
                }
              }
            }
          }
        }
      }
      if (record_owned && record_expr_type != NULL)
        destroy_kgpc_type(record_expr_type);
    }
    if (record_type != NULL) {
      /* Preserve inferred constructor target record explicitly so later
       * expression passes do not depend on transient KgpcType state. */
      expr->record_type = record_type;
      if (expr->resolved_kgpc_type != NULL &&
          !kgpc_type_is_record(expr->resolved_kgpc_type)) {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
      }
      if (expr->resolved_kgpc_type == NULL)
        expr->resolved_kgpc_type = create_record_type(record_type);
    }
  }
  if (expr != NULL && expr->type == EXPR_ARRAY_LITERAL &&
      expr->array_element_type == UNKNOWN_TYPE &&
      expr->array_element_type_id == NULL && var != NULL &&
      var->resolved_kgpc_type != NULL) {
    KgpcType *lhs_type = var->resolved_kgpc_type;
    if (kgpc_type_is_pointer(lhs_type) && lhs_type->info.points_to != NULL &&
        kgpc_type_is_array(lhs_type->info.points_to)) {
      lhs_type = lhs_type->info.points_to;
    }
    if (kgpc_type_is_array(lhs_type)) {
      KgpcType *lhs_elem =
          kgpc_type_get_array_element_type_resolved(lhs_type, symtab);
      if (lhs_elem != NULL) {
        int elem_tag = semcheck_tag_from_kgpc(lhs_elem);
        if (expr->array_element_type == UNKNOWN_TYPE)
          expr->array_element_type = elem_tag;
        if (lhs_elem->kind == TYPE_KIND_RECORD &&
            lhs_elem->info.record_info != NULL) {
          expr->array_element_record_type = lhs_elem->info.record_info;
          if (expr->array_element_type_id == NULL &&
              lhs_elem->info.record_info->type_id != NULL) {
            expr->array_element_type_id =
                strdup(lhs_elem->info.record_info->type_id);
          }
        }
        if (expr->array_element_type_id == NULL &&
            lhs_elem->type_alias != NULL &&
            lhs_elem->type_alias->target_type_id != NULL) {
          expr->array_element_type_id =
              strdup(lhs_elem->type_alias->target_type_id);
        }
      }
      if (expr->array_element_type_id == NULL &&
          lhs_type->info.array_info.element_type_id != NULL) {
        expr->array_element_type_id =
            strdup(lhs_type->info.array_info.element_type_id);
      }
    }
  }
  if (SEMSTMT_TIMINGS_ENABLED()) {
    double t0 = semstmt_now_ms();
    int before_rhs = return_val;
    return_val +=
        semcheck_stmt_expr_tag(&type_second, symtab, expr, INT_MAX, NO_MUTATE);
    fprintf(
        stderr,
        "[timing] varassign rhs semcheck_stmt_expr_tag: %.2f ms (line=%d)\n",
        semstmt_now_ms() - t0, stmt->line_num);
    if (kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL && return_val > before_rhs &&
        expr != NULL) {
      fprintf(stderr,
              "[KGPC_DEBUG_ERRORS] varassign_rhs_error line=%d expr_type=%d\n",
              stmt->line_num, expr->type);
      if (expr->type == EXPR_FUNCTION_CALL &&
          expr->expr_data.function_call_data.id != NULL)
        fprintf(stderr, "[KGPC_DEBUG_ERRORS]   rhs func=%s\n",
                expr->expr_data.function_call_data.id);
      if (expr->type == EXPR_RECORD_ACCESS &&
          expr->expr_data.record_access_data.field_id != NULL)
        fprintf(stderr, "[KGPC_DEBUG_ERRORS]   rhs record field=%s\n",
                expr->expr_data.record_access_data.field_id);
    }
  } else {
    int before_rhs = return_val;
    return_val +=
        semcheck_stmt_expr_tag(&type_second, symtab, expr, INT_MAX, NO_MUTATE);
    if (kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL && return_val > before_rhs &&
        expr != NULL) {
      fprintf(stderr,
              "[KGPC_DEBUG_ERRORS] varassign_rhs_error line=%d expr_type=%d\n",
              stmt->line_num, expr->type);
      if (expr->type == EXPR_FUNCTION_CALL &&
          expr->expr_data.function_call_data.id != NULL)
        fprintf(stderr, "[KGPC_DEBUG_ERRORS]   rhs func=%s\n",
                expr->expr_data.function_call_data.id);
      if (expr->type == EXPR_RECORD_ACCESS &&
          expr->expr_data.record_access_data.field_id != NULL)
        fprintf(stderr, "[KGPC_DEBUG_ERRORS]   rhs record field=%s\n",
                expr->expr_data.record_access_data.field_id);
    }
  }

  if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr != NULL &&
      expr->type == EXPR_FUNCTION_CALL &&
      expr->expr_data.function_call_data.id != NULL &&
      strcasecmp(expr->expr_data.function_call_data.id, "Create") == 0) {
    fprintf(stderr, "[SemCheck] semcheck_varassign calling "
                    "semcheck_resolve_expression_kgpc_type:\n");
    fprintf(stderr, "[SemCheck]   expr=%p type=%d\n", (void *)expr, expr->type);
    fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type=%p\n",
            (void *)expr->resolved_kgpc_type);
  }

  if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr != NULL &&
      expr->type == EXPR_RECORD_ACCESS) {
    fprintf(stderr,
            "[SemCheck] semcheck_varassign: expr is EXPR_RECORD_ACCESS\n");
    fprintf(stderr, "[SemCheck]   expr=%p\n", (void *)expr);
    fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type=%p\n",
            (void *)expr->resolved_kgpc_type);
  }

  int lhs_owned = 0, rhs_owned = 0;
  KgpcType *lhs_kgpctype = NULL;
  KgpcType *rhs_kgpctype = NULL;
  if (SEMSTMT_TIMINGS_ENABLED()) {
    double t0 = semstmt_now_ms();
    lhs_kgpctype = semcheck_resolve_expression_kgpc_type(
        symtab, var, max_scope_lev, MUTATE, &lhs_owned);
    fprintf(stderr,
            "[timing] varassign lhs resolve_kgpc_type: %.2f ms (line=%d)\n",
            semstmt_now_ms() - t0, stmt->line_num);
    t0 = semstmt_now_ms();
    rhs_kgpctype = semcheck_resolve_expression_kgpc_type(symtab, expr, INT_MAX,
                                                         NO_MUTATE, &rhs_owned);
    fprintf(stderr,
            "[timing] varassign rhs resolve_kgpc_type: %.2f ms (line=%d)\n",
            semstmt_now_ms() - t0, stmt->line_num);
  } else {
    lhs_kgpctype = semcheck_resolve_expression_kgpc_type(
        symtab, var, max_scope_lev, MUTATE, &lhs_owned);
    rhs_kgpctype = semcheck_resolve_expression_kgpc_type(symtab, expr, INT_MAX,
                                                         NO_MUTATE, &rhs_owned);
  }
  int handled_by_kgpctype = 0;

  if (lhs_kgpctype != NULL && rhs_kgpctype != NULL) {
    handled_by_kgpctype = 1;

    if (var != NULL && var->type == EXPR_VAR_ID && var->expr_data.id != NULL) {
      const char *cur_id = semcheck_get_current_subprogram_id();
      if (cur_id != NULL) {
        /* Check if this is "Result" or the function's own name (Pascal-style
         * function result assignment: FuncName := value). Both should use the
         * current function's return type directly from the subprogram tree.
         * This is critical for case-insensitive overloads where FpFStat and
         * FPFStat both exist — FindIdent may resolve to the wrong overload's
         * return type entry in the symbol table.
         * Also check operator result variable name (e.g., "dest" in
         * operator :=(src) dest: variant). */
        const char *result_var =
            semcheck_get_current_subprogram_result_var_name();
        /* For methods, cur_id is mangled (e.g. "TEReader__ReadNext").
         * Also check against just the method name part after "__". */
        const char *method_name = semcheck_get_current_subprogram_method_name();
        int is_result_assign =
            pascal_identifier_equals(var->expr_data.id, "Result") ||
            pascal_identifier_equals(var->expr_data.id, cur_id) ||
            (method_name != NULL &&
             pascal_identifier_equals(var->expr_data.id, method_name)) ||
            (result_var != NULL &&
             pascal_identifier_equals(var->expr_data.id, result_var));
        if (is_result_assign) {
          int ret_owned = 0;
          KgpcType *ret_type = semcheck_get_current_subprogram_return_kgpc_type(
              symtab, &ret_owned);
          if (ret_type != NULL && !(ret_type->kind == TYPE_KIND_PRIMITIVE &&
                                    ret_type->info.primitive_type_tag < 0)) {
            /* Always use the function's declared return type for result
             * variable assignments, even if the current LHS type seems
             * compatible. This handles cases where FindIdent found a
             * different overload's return type entry (e.g., Variant vs
             * String for operator overloads with named result vars). */
            if (lhs_owned && lhs_kgpctype != NULL)
              destroy_kgpc_type(lhs_kgpctype);
            lhs_kgpctype = ret_type;
            lhs_owned = ret_owned;
          } else if (ret_type != NULL && ret_owned) {
            destroy_kgpc_type(ret_type);
          }
        }
      }
    }

    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
      fprintf(stderr, "[SemCheck] Type compatibility check:\n");
      fprintf(stderr, "[SemCheck]   lhs_kgpctype=%p kind=%d\n",
              (void *)lhs_kgpctype, lhs_kgpctype->kind);
      fprintf(stderr, "[SemCheck]   rhs_kgpctype=%p kind=%d\n",
              (void *)rhs_kgpctype, rhs_kgpctype->kind);
      if (lhs_kgpctype->kind == TYPE_KIND_POINTER &&
          lhs_kgpctype->info.points_to != NULL) {
        fprintf(stderr, "[SemCheck]   lhs points_to=%p kind=%d\n",
                (void *)lhs_kgpctype->info.points_to,
                lhs_kgpctype->info.points_to->kind);
        if (lhs_kgpctype->info.points_to->kind == TYPE_KIND_RECORD) {
          fprintf(stderr, "[SemCheck]   lhs record_info=%p\n",
                  (void *)lhs_kgpctype->info.points_to->info.record_info);
        }
      }
      if (rhs_kgpctype->kind == TYPE_KIND_POINTER &&
          rhs_kgpctype->info.points_to != NULL) {
        fprintf(stderr, "[SemCheck]   rhs points_to=%p kind=%d\n",
                (void *)rhs_kgpctype->info.points_to,
                rhs_kgpctype->info.points_to->kind);
        if (rhs_kgpctype->info.points_to->kind == TYPE_KIND_RECORD) {
          fprintf(stderr, "[SemCheck]   rhs record_info=%p\n",
                  (void *)rhs_kgpctype->info.points_to->info.record_info);
        }
      }
    }

    int lhs_is_char = (lhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
                       lhs_kgpctype->info.primitive_type_tag == CHAR_TYPE);
    int rhs_is_single_char_literal = 0;
    int rhs_is_single_char_const = 0;
    if (lhs_is_char && expr != NULL && expr->type == EXPR_STRING &&
        expr->expr_data.string != NULL && strlen(expr->expr_data.string) == 1) {
      rhs_is_single_char_literal = 1;
    }
    if (lhs_is_char && expr != NULL && expr->type == EXPR_VAR_ID &&
        expr->expr_data.id != NULL) {
      HashNode_t *rhs_node = NULL;
      if (FindSymbol(&rhs_node, symtab, expr->expr_data.id) != 0 &&
          rhs_node != NULL && rhs_node->is_constant &&
          rhs_node->const_string_value != NULL &&
          strlen(rhs_node->const_string_value) == 1) {
        rhs_is_single_char_const = 1;
      }
    }
    if ((rhs_is_single_char_literal || rhs_is_single_char_const) &&
        lhs_is_char) {
      semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
      goto assignment_types_ok;
    }

    /* Allow WideChar to string assignment - WideChar (aliased to Word) converts
     * to single-char string. Check if LHS is string and RHS is WideChar before
     * the general compatibility check. */
    int lhs_is_string = (lhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
                         lhs_kgpctype->info.primitive_type_tag == STRING_TYPE);
    if (lhs_is_string && expr != NULL &&
        semcheck_expr_is_widechar(symtab, expr)) {
      /* Mark expression as CHAR_TYPE for codegen to promote to string */
      semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
      goto assignment_types_ok;
    }

    /* Special handling for Currency := Real assignment.
     * Currency is a fixed-point type that stores values scaled by 10000.
     * When assigning a real literal to Currency, we scale it at compile time.
     */
    if (semcheck_is_currency_kgpc_type(lhs_kgpctype) &&
        rhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
        rhs_kgpctype->info.primitive_type_tag == REAL_TYPE && expr != NULL &&
        expr->type == EXPR_RNUM) {
      /* Scale the real value by 10000 and convert to integer */
      long long scaled = llround(expr->expr_data.r_num * 10000.0);
      expr->type = EXPR_INUM;
      expr->expr_data.i_num = scaled;
      semcheck_expr_set_resolved_type(expr, INT64_TYPE);
      goto assignment_types_ok;
    }
    if (semcheck_is_currency_kgpc_type(lhs_kgpctype) &&
        rhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
        rhs_kgpctype->info.primitive_type_tag == REAL_TYPE) {
      /* Allow real-to-currency assignment for non-literals; runtime handles
       * conversion. */
      goto assignment_types_ok;
    }

    if (kgpc_type_is_array(lhs_kgpctype) && kgpc_type_is_array(rhs_kgpctype)) {
      KgpcType *lhs_elem = lhs_kgpctype->info.array_info.element_type;
      KgpcType *rhs_elem = rhs_kgpctype->info.array_info.element_type;
      if (lhs_elem != NULL && rhs_elem != NULL &&
          lhs_elem->kind == TYPE_KIND_PRIMITIVE &&
          rhs_elem->kind == TYPE_KIND_PRIMITIVE &&
          lhs_elem->info.primitive_type_tag == CHAR_TYPE &&
          rhs_elem->info.primitive_type_tag == CHAR_TYPE) {
        goto assignment_types_ok;
      }
    }
    if (expr != NULL && expr->type == EXPR_SET &&
        kgpc_type_is_array(lhs_kgpctype)) {
      if (semcheck_convert_set_literal_to_array_literal(expr) != 0)
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num,
                                       stmt->source_index,
                                       "Error on line %d, unable to convert "
                                       "set literal to array literal.\n\n",
                                       stmt->line_num);
    }

    if (expr != NULL && expr->type == EXPR_ARRAY_LITERAL &&
        kgpc_type_is_array(lhs_kgpctype)) {
      KgpcType *lhs_elem = lhs_kgpctype->info.array_info.element_type;
      int elem_tag = semcheck_tag_from_kgpc(lhs_elem);
      const char *elem_type_id = NULL;
      if (lhs_elem != NULL) {
        if (lhs_elem->type_alias != NULL &&
            lhs_elem->type_alias->alias_name != NULL) {
          elem_type_id = lhs_elem->type_alias->alias_name;
        } else if (lhs_elem->kind == TYPE_KIND_RECORD &&
                   lhs_elem->info.record_info != NULL &&
                   lhs_elem->info.record_info->type_id != NULL) {
          elem_type_id = lhs_elem->info.record_info->type_id;
        } else if (lhs_elem->kind == TYPE_KIND_ARRAY &&
                   lhs_elem->info.array_info.element_type_id != NULL) {
          elem_type_id = lhs_elem->info.array_info.element_type_id;
        } else if (lhs_elem->kind == TYPE_KIND_POINTER &&
                   lhs_elem->type_alias != NULL &&
                   lhs_elem->type_alias->target_type_id != NULL) {
          elem_type_id = lhs_elem->type_alias->target_type_id;
        }
      }
      if (kgpc_getenv("KGPC_DEBUG_ARRAY_ASSIGN") != NULL) {
        fprintf(stderr,
                "[KGPC] array assign @ line %d: elem_tag=%d elem_id=%s\n",
                stmt->line_num, elem_tag,
                elem_type_id != NULL ? elem_type_id : "<null>");
      }
      if (expr->array_element_type == UNKNOWN_TYPE)
        expr->array_element_type = elem_tag;
      if (expr->array_element_type_id == NULL && elem_type_id != NULL)
        expr->array_element_type_id = strdup(elem_type_id);
      if (expr->array_element_size <= 0 && lhs_elem != NULL) {
        long long elem_size = kgpc_type_sizeof(lhs_elem);
        if (elem_size > 0 && elem_size <= INT_MAX)
          expr->array_element_size = (int)elem_size;
      }
      semcheck_typecheck_array_literal(expr, symtab, INT_MAX, elem_tag,
                                       elem_type_id, stmt->line_num);
      goto assignment_types_ok;
    }

    if ((semcheck_type_is_recordish(lhs_kgpctype) &&
         !semcheck_type_is_recordish(rhs_kgpctype)) ||
        (!semcheck_type_is_recordish(lhs_kgpctype) &&
         semcheck_type_is_recordish(rhs_kgpctype))) {
      if (semcheck_try_record_assignment_operator(symtab, stmt, lhs_kgpctype,
                                                  &rhs_kgpctype, &rhs_owned)) {
        expr = stmt->stmt_data.var_assign_data.expr;
        type_second = semcheck_tag_from_kgpc(rhs_kgpctype);
        goto assignment_types_ok;
      }
    }

    if (!are_types_compatible_for_assignment(lhs_kgpctype, rhs_kgpctype,
                                             symtab)) {
      if (var != NULL && expr != NULL && var->pointer_subtype_id != NULL &&
          expr->pointer_subtype_id != NULL &&
          semcheck_class_type_ids_compatible(symtab, var->pointer_subtype_id,
                                             expr->pointer_subtype_id)) {
        goto assignment_types_ok;
      }
      if (semcheck_try_record_assignment_operator(symtab, stmt, lhs_kgpctype,
                                                  &rhs_kgpctype, &rhs_owned)) {
        expr = stmt->stmt_data.var_assign_data.expr;
        type_second = semcheck_tag_from_kgpc(rhs_kgpctype);
        goto assignment_types_ok;
      }

      int allow_char_literal = 0;
      if (semcheck_type_is_char_like(lhs_kgpctype) &&
          semcheck_force_char_case_builtin_in_assignment(expr))
        goto assignment_types_ok;
      if (semcheck_type_is_char_like(lhs_kgpctype) &&
          rhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
          rhs_kgpctype->info.primitive_type_tag == STRING_TYPE &&
          expr != NULL && expr->type == EXPR_STRING &&
          expr->expr_data.string != NULL &&
          strlen(expr->expr_data.string) == 1) {
        allow_char_literal = 1;
        semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
      }
      if (allow_char_literal)
        goto assignment_types_ok;

      /* Allow assigning Char ordinal constants to Char targets
       * (FPC-compatible). */
      if (semcheck_type_is_char_like(lhs_kgpctype) &&
          rhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
          is_integer_type(rhs_kgpctype->info.primitive_type_tag) &&
          semcheck_expr_is_char_ordinal_const(symtab, expr)) {
        semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
        goto assignment_types_ok;
      }

      /* Allow assigning Char / Char ordinal constants to String targets. */
      if (lhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
          lhs_kgpctype->info.primitive_type_tag == STRING_TYPE &&
          rhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
          (rhs_kgpctype->info.primitive_type_tag == CHAR_TYPE ||
           (is_integer_type(rhs_kgpctype->info.primitive_type_tag) &&
            semcheck_expr_is_char_ordinal_const(symtab, expr)))) {
        if (rhs_kgpctype->info.primitive_type_tag != CHAR_TYPE)
          semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
        goto assignment_types_ok;
      }

      /* Allow assigning string literals to PChar/PAnsiChar.
       * In Pascal, string literals can be implicitly converted to PChar. */
      int allow_string_to_pchar = 0;
      if (kgpc_type_is_pointer(lhs_kgpctype)) {
        KgpcType *points_to = lhs_kgpctype->info.points_to;
        if (points_to != NULL && points_to->kind == TYPE_KIND_PRIMITIVE &&
            points_to->info.primitive_type_tag == CHAR_TYPE &&
            (rhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
             (rhs_kgpctype->info.primitive_type_tag == STRING_TYPE ||
              rhs_kgpctype->info.primitive_type_tag == SHORTSTRING_TYPE ||
              rhs_kgpctype->info.primitive_type_tag == CHAR_TYPE))) {
          allow_string_to_pchar = 1;
        }
      }
      if (allow_string_to_pchar)
        goto assignment_types_ok;

      if (kgpc_getenv("KGPC_DEBUG_RESULT") != NULL && var != NULL &&
          var->type == EXPR_VAR_ID && var->expr_data.id != NULL &&
          pascal_identifier_equals(var->expr_data.id, "Result")) {
        fprintf(stderr,
                "[KGPC] assignment Result type mismatch: lhs=%s rhs=%s\n",
                kgpc_type_to_string(lhs_kgpctype),
                kgpc_type_to_string(rhs_kgpctype));
        if (expr != NULL) {
          fprintf(stderr,
                  "[KGPC] rhs expr type=%d resolved_kgpc=%s rhs_kgpc=%s\n",
                  expr->type,
                  expr->resolved_kgpc_type
                      ? kgpc_type_to_string(expr->resolved_kgpc_type)
                      : "<null>",
                  rhs_kgpctype ? kgpc_type_to_string(rhs_kgpctype) : "<null>");
        }
      }

      if (lhs_was_typecast)
        goto assignment_types_ok;

      /* Allow record = integer/pointer and integer = record (type helpers) */
      if ((type_first == RECORD_TYPE &&
           (is_integer_type(type_second) || type_second == ENUM_TYPE ||
            type_second == POINTER_TYPE)) ||
          (type_second == RECORD_TYPE &&
           (is_integer_type(type_first) || type_first == ENUM_TYPE ||
            type_first == POINTER_TYPE)) ||
          (type_first == RECORD_TYPE && type_second == RECORD_TYPE))
        goto assignment_types_ok;

      /* Allow char = string (single-char string literal to char) */
      if (type_first == CHAR_TYPE &&
          (type_second == STRING_TYPE || type_second == SHORTSTRING_TYPE))
        goto assignment_types_ok;

      const char *lhs_name = "<expression>";
      if (var != NULL && var->type == EXPR_VAR_ID)
        lhs_name = var->expr_data.id;
      if (kgpc_getenv("KGPC_DEBUG_ASSIGN") != NULL) {
        fprintf(stderr,
                "[KGPC_DEBUG_ASSIGN] line=%d col=%d lhs=%s lhs_type=%s "
                "rhs_type=%s\n",
                stmt->line_num, stmt->col_num, lhs_name,
                kgpc_type_to_string(lhs_kgpctype),
                kgpc_type_to_string(rhs_kgpctype));
        semcheck_debug_expr_brief(var, "assign lhs");
        semcheck_debug_expr_brief(expr, "assign rhs");
      }
      semantic_error_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "incompatible types in assignment for %s (lhs: %s, rhs: %s)!",
          lhs_name, kgpc_type_to_string(lhs_kgpctype),
          kgpc_type_to_string(rhs_kgpctype));
      ++return_val;
    } else if (type_first == PROCEDURE && type_second == PROCEDURE) {
      /* AST TRANSFORMATION: Mark RHS as procedure address if it's a direct
       * procedure reference */
      /* Only transform if BOTH LHS and RHS are actual procedures (not
       * functions) */
      /* Functions should be called, not have their address taken */
      int lhs_is_procedure = (lhs_kgpctype->kind == TYPE_KIND_PROCEDURE &&
                              lhs_kgpctype->info.proc_info.return_type == NULL);
      int rhs_is_procedure = (rhs_kgpctype->kind == TYPE_KIND_PROCEDURE &&
                              rhs_kgpctype->info.proc_info.return_type == NULL);

      if (lhs_is_procedure && rhs_is_procedure && expr != NULL &&
          expr->type == EXPR_VAR_ID) {
        HashNode_t *rhs_symbol = NULL;
        if (FindSymbol(&rhs_symbol, symtab, expr->expr_data.id) != 0 &&
            rhs_symbol != NULL && rhs_symbol->hash_type == HASHTYPE_PROCEDURE) {
          /* Transform the expression to EXPR_ADDR_OF_PROC.
           * expr_data is a union; the EXPR_VAR_ID slot's strdup'd
           * id must be freed before we reassign expr_data via
           * addr_of_proc_data, or it leaks. */
          free(expr->expr_data.id);
          expr->type = EXPR_ADDR_OF_PROC;
          expr->expr_data.addr_of_proc_data.proc_mangled_id =
              rhs_symbol->mangled_id ? strdup(rhs_symbol->mangled_id) : NULL;
          expr->expr_data.addr_of_proc_data.proc_id =
              rhs_symbol->id ? strdup(rhs_symbol->id) : NULL;
          expr->expr_data.addr_of_proc_data.source_unit_index =
              rhs_symbol->source_unit_index;
          /* Resolve the type NOW while the symbol is still alive. */
          if (rhs_symbol->type != NULL) {
            kgpc_type_retain(rhs_symbol->type);
            expr->resolved_kgpc_type = create_pointer_type(rhs_symbol->type);
            kgpc_type_release(rhs_symbol->type);
          } else {
            expr->resolved_kgpc_type = create_pointer_type(NULL);
          }
        }
      }
    }
  assignment_types_ok:;
  }

  if (!handled_by_kgpctype) {
    int coerced_rhs_type = type_second;
    int types_compatible = (type_first == type_second);

    /* Reject assignment of scalar to array or vice versa */
    /* Arrays must be assigned from arrays, scalars from scalars */
    if (types_compatible && var != NULL && expr != NULL &&
        var->is_array_expr != expr->is_array_expr) {
      types_compatible = 0;
    }

    if (!types_compatible) {
      if ((type_first == LONGINT_TYPE && type_second == INT_TYPE) ||
          (type_first == INT_TYPE && type_second == LONGINT_TYPE)) {
        types_compatible = 1;
      } else if (type_first == REAL_TYPE &&
                 (type_second == INT_TYPE || type_second == LONGINT_TYPE)) {
        types_compatible = 1;
        coerced_rhs_type = REAL_TYPE;
        if (expr != NULL) {
          if (expr->type == EXPR_INUM) {
            double coerced_value = (double)expr->expr_data.i_num;
            expr->type = EXPR_RNUM;
            expr->expr_data.r_num = coerced_value;
          }
          semcheck_expr_set_resolved_type(expr, REAL_TYPE);
        }
      } else if (type_first == CHAR_TYPE && type_second == STRING_TYPE &&
                 expr != NULL && expr->type == EXPR_STRING &&
                 expr->expr_data.string != NULL &&
                 strlen(expr->expr_data.string) == 1) {
        types_compatible = 1;
        coerced_rhs_type = CHAR_TYPE;
        semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
      }
      /* Allow char to string assignment - char will be promoted to
         single-character string */
      /* Only for actual string variables, not char arrays */
      else if (type_first == STRING_TYPE && type_second == CHAR_TYPE &&
               var != NULL && !var->is_array_expr) {
        types_compatible = 1;
        /* Keep CHAR_TYPE so code generator knows to promote */
      } else if (type_first == STRING_TYPE &&
                 (type_second == PROCEDURE || type_second == POINTER_TYPE) &&
                 var != NULL && !var->is_array_expr) {
        types_compatible = 1;
      }
      /* Allow WideChar to string assignment - WideChar will be converted to
       * single-character string. WideChar is aliased to Word (integer), so we
       * need to check the type name. */
      else if (type_first == STRING_TYPE && is_integer_type(type_second) &&
               var != NULL && !var->is_array_expr && expr != NULL &&
               semcheck_expr_is_widechar(symtab, expr)) {
        types_compatible = 1;
        /* Mark expression as CHAR_TYPE for codegen to promote to string */
        semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
      }
      /* Allow char assignment to char arrays (FPC compatibility) */
      else if (type_first == CHAR_TYPE && type_second == CHAR_TYPE &&
               var != NULL && var->is_array_expr &&
               var->array_element_type == CHAR_TYPE &&
               (expr == NULL || !expr->is_array_expr)) {
        types_compatible = 1;
        fprintf(stderr,
                "Warning on line %d, assigning char to array of char copies "
                "only the first element (FPC compatibility).\n\n",
                stmt->line_num);
      }
      /* Allow string literal assignment to char arrays */
      else if (type_first == CHAR_TYPE && type_second == STRING_TYPE &&
               var != NULL && var->is_array_expr &&
               var->array_element_type == CHAR_TYPE && expr != NULL &&
               expr->type == EXPR_STRING) {
        /* Verify string fits in array (including null terminator) */
        size_t string_len =
            expr->expr_data.string != NULL ? strlen(expr->expr_data.string) : 0;
        int array_size = var->array_upper_bound - var->array_lower_bound + 1;

        if (string_len > (size_t)array_size) {
          const char *lhs_name =
              (var->type == EXPR_VAR_ID) ? var->expr_data.id : "<expression>";
          semcheck_error_with_context_at(
              stmt->line_num, stmt->col_num, stmt->source_index,
              "Error on line %d, string literal too long for array %s (string "
              "length: %zu, array size: %d)!\n\n",
              stmt->line_num, lhs_name, string_len, array_size);
          ++return_val;
        } else {
          types_compatible = 1;
          /* Keep the type as STRING_TYPE to signal code generator to emit
           * string-to-array copy */
        }
      }
    }

    if (lhs_was_typecast)
      types_compatible = 1;

    if (!types_compatible) {
      if (var != NULL && var->type == EXPR_TYPECAST) {
        const struct TypeRef *cast_ref =
            var->expr_data.typecast_data.target_type_ref;
        if (cast_ref != NULL && cast_ref->num_generic_args > 0)
          types_compatible = 1;
      }
    }

    if (!types_compatible && type_first != UNKNOWN_TYPE &&
        type_second != UNKNOWN_TYPE) {
      if (kgpc_getenv("KGPC_DEBUG_ASSIGN") != NULL) {
        fprintf(stderr,
                "[KGPC_DEBUG_ASSIGN] legacy mismatch line=%d col=%d "
                "lhs_type=%d rhs_type=%d lhs_expr_type=%d rhs_expr_type=%d\n",
                stmt->line_num, stmt->col_num, type_first, type_second,
                var != NULL ? var->type : -1, expr != NULL ? expr->type : -1);
        semcheck_debug_expr_brief(var, "legacy assign lhs");
        semcheck_debug_expr_brief(expr, "legacy assign rhs");
      }
      const char *lhs_name = "<expression>";
      if (var != NULL && var->type == EXPR_VAR_ID)
        lhs_name = var->expr_data.id;
      const char *lhs_desc = (lhs_kgpctype != NULL)
                                 ? kgpc_type_to_string(lhs_kgpctype)
                                 : "unknown";
      const char *rhs_desc = (rhs_kgpctype != NULL)
                                 ? kgpc_type_to_string(rhs_kgpctype)
                                 : "unknown";
      semantic_error(
          stmt->line_num, stmt->col_num,
          "type mismatch in assignment statement for %s (lhs: %s, rhs: %s)",
          lhs_name, lhs_desc, rhs_desc);
      ++return_val;
    } else {
      type_second = coerced_rhs_type;
    }
  }

  int property_result =
      semcheck_try_property_assignment(symtab, stmt, max_scope_lev);
  if (property_result >= 0) {
    if (lhs_owned && lhs_kgpctype != NULL)
      destroy_kgpc_type(lhs_kgpctype);
    if (rhs_owned && rhs_kgpctype != NULL)
      destroy_kgpc_type(rhs_kgpctype);
    return return_val + property_result;
  }

  /* Clean up owned KgpcTypes */
  if (lhs_owned && lhs_kgpctype != NULL)
    destroy_kgpc_type(lhs_kgpctype);
  if (rhs_owned && rhs_kgpctype != NULL)
    destroy_kgpc_type(rhs_kgpctype);

  return return_val;
}

int semcheck_try_module_property_assignment(SymTab_t *symtab,
                                            struct Statement *stmt,
                                            int max_scope_lev) {
  if (symtab == NULL || stmt == NULL || stmt->type != STMT_VAR_ASSIGN)
    return -1;

  struct Expression *lhs = stmt->stmt_data.var_assign_data.var;
  struct Expression *rhs = stmt->stmt_data.var_assign_data.expr;
  if (lhs == NULL || rhs == NULL || lhs->type != EXPR_VAR_ID)
    return -1;

  const char *prop_name = lhs->expr_data.id;
  if (prop_name == NULL)
    return -1;

  {
    const char *cur_sub_id = semcheck_get_current_subprogram_id();
    const char *result_var = semcheck_get_current_subprogram_result_var_name();
    const char *method_name = semcheck_get_current_subprogram_method_name();
    int is_result_name = (cur_sub_id != NULL &&
                          pascal_identifier_equals(prop_name, cur_sub_id)) ||
                         (result_var != NULL &&
                          pascal_identifier_equals(prop_name, result_var)) ||
                         (method_name != NULL &&
                          pascal_identifier_equals(prop_name, method_name));
    if (is_result_name)
      return -1;
  }

  /* WITH-context fields must be resolved as assignments on the active record,
   * not rewritten as module-property setter calls. */
  if (with_context_count > 0) {
    struct Expression *with_expr = NULL;
    int with_status = semcheck_with_try_resolve(prop_name, symtab, &with_expr,
                                                stmt->line_num);
    if (with_expr != NULL)
      destroy_expr(with_expr);
    if (with_status == 0)
      return -1;
  }

  ListNode_t *matches = FindAllIdents(symtab, prop_name);
  HashNode_t *setter = NULL;
  int has_storage_symbol = 0;

  for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
    HashNode_t *node = (HashNode_t *)cur->cur;
    if (node == NULL)
      continue;
    if (node->hash_type == HASHTYPE_VAR || node->hash_type == HASHTYPE_ARRAY ||
        node->hash_type == HASHTYPE_FUNCTION_RETURN) {
      has_storage_symbol = 1;
      break;
    }
    /* Enum constants are not assignable, so they should not block
     * module-property setter lookup.  Only non-enum constants
     * (typed consts, literal consts) count as storage symbols. */
    if (node->hash_type == HASHTYPE_CONST) {
      int is_enum_literal =
          (node->type != NULL && node->type->kind == TYPE_KIND_PRIMITIVE &&
           kgpc_type_get_primitive_tag(node->type) == ENUM_TYPE);
      if (!is_enum_literal) {
        has_storage_symbol = 1;
        break;
      }
    }
    if (node->hash_type == HASHTYPE_PROCEDURE && node->type != NULL &&
        node->type->kind == TYPE_KIND_PROCEDURE) {
      int param_count = ListLength(node->type->info.proc_info.params);
      if (param_count == 1)
        setter = node;
    }
  }

  if (matches != NULL)
    DestroyList(matches);

  if (has_storage_symbol || setter == NULL)
    return -1;

  char *call_id = lhs->expr_data.id;
  lhs->expr_data.id = NULL;
  destroy_expr(lhs);
  stmt->stmt_data.var_assign_data.var = NULL;
  stmt->stmt_data.var_assign_data.expr = NULL;

  ListNode_t *args = CreateListNode(rhs, LIST_EXPR);
  if (args == NULL) {
    free(call_id);
    return 1;
  }

  stmt->type = STMT_PROCEDURE_CALL;
  memset(&stmt->stmt_data.procedure_call_data, 0,
         sizeof(stmt->stmt_data.procedure_call_data));
  stmt->stmt_data.procedure_call_data.id = call_id;
  stmt->stmt_data.procedure_call_data.expr_args = args;
  stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;

  return semcheck_proccall(symtab, stmt, max_scope_lev);
}

int semcheck_convert_property_assignment_to_setter(SymTab_t *symtab,
                                                   struct Statement *stmt,
                                                   struct Expression *lhs,
                                                   HashNode_t *setter_node,
                                                   int max_scope_lev) {
  struct Expression *object_expr =
      lhs->expr_data.record_access_data.record_expr;
  if (object_expr == NULL) {
    semcheck_error_with_context_at(stmt->line_num, stmt->col_num,
                                   stmt->source_index,
                                   "Error on line %d, property assignment "
                                   "requires an object instance.\n\n",
                                   stmt->line_num);
    return 1;
  }

  lhs->expr_data.record_access_data.record_expr = NULL;
  struct Expression *value_expr = stmt->stmt_data.var_assign_data.expr;
  stmt->stmt_data.var_assign_data.expr = NULL;

  destroy_expr(lhs);
  stmt->stmt_data.var_assign_data.var = NULL;

  ListNode_t *self_arg = CreateListNode(object_expr, LIST_EXPR);
  if (self_arg == NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, unable to allocate setter argument list.\n\n",
        stmt->line_num);
    destroy_expr(object_expr);
    destroy_expr(value_expr);
    return 1;
  }

  ListNode_t *value_arg = CreateListNode(value_expr, LIST_EXPR);
  if (value_arg == NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, unable to allocate setter argument list.\n\n",
        stmt->line_num);
    destroy_expr(object_expr);
    destroy_expr(value_expr);
    free(self_arg);
    return 1;
  }
  self_arg->next = value_arg;

  char *id_copy = setter_node->id != NULL ? strdup(setter_node->id) : NULL;
  char *mangled_copy = NULL;
  if (setter_node->mangled_id != NULL)
    mangled_copy = strdup(setter_node->mangled_id);

  if ((setter_node->id != NULL && id_copy == NULL) ||
      (setter_node->mangled_id != NULL && mangled_copy == NULL)) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, unable to prepare property setter call.\n\n",
        stmt->line_num);
    free(id_copy);
    free(mangled_copy);
    value_arg->next = NULL;
    destroy_expr(object_expr);
    destroy_expr(value_expr);
    free(value_arg);
    free(self_arg);
    return 1;
  }

  stmt->type = STMT_PROCEDURE_CALL;
  stmt->stmt_data.procedure_call_data.id = id_copy;
  stmt->stmt_data.procedure_call_data.mangled_id = mangled_copy;
  stmt->stmt_data.procedure_call_data.expr_args = self_arg;
  stmt->stmt_data.procedure_call_data.resolved_proc = NULL;
  stmt->stmt_data.procedure_call_data.call_hash_type = 0;
  stmt->stmt_data.procedure_call_data.is_call_info_valid = 0;
  stmt->stmt_data.procedure_call_data.is_procedural_var_call = 0;
  stmt->stmt_data.procedure_call_data.procedural_var_symbol = NULL;
  stmt->stmt_data.procedure_call_data.procedural_var_expr = NULL;
  stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
  stmt->stmt_data.procedure_call_data.placeholder_method_name = NULL;
  semcheck_stmt_set_call_kgpc_type(stmt, NULL, 0);

  return semcheck_proccall(symtab, stmt, max_scope_lev);
}

int semcheck_try_property_assignment(SymTab_t *symtab, struct Statement *stmt,
                                     int max_scope_lev) {
  if (stmt == NULL || stmt->type != STMT_VAR_ASSIGN)
    return -1;

  struct Expression *lhs = stmt->stmt_data.var_assign_data.var;
  if (lhs == NULL || lhs->type != EXPR_RECORD_ACCESS)
    return -1;

  const char *property_name = lhs->expr_data.record_access_data.field_id;
  if (property_name == NULL)
    return -1;

  struct Expression *object_expr =
      lhs->expr_data.record_access_data.record_expr;
  if (object_expr == NULL)
    return -1;

  struct RecordType *object_record = semcheck_with_resolve_record_type(
      symtab, object_expr,
      semcheck_tag_from_kgpc(object_expr->resolved_kgpc_type), stmt->line_num);

  if (object_record == NULL)
    return -1;

  struct RecordType *property_owner = NULL;
  struct ClassProperty *property = semcheck_find_class_property(
      symtab, object_record, property_name, &property_owner);
  if (property == NULL || property->write_accessor == NULL)
    return -1;

  if (property_owner == NULL)
    property_owner = object_record;

  struct RecordField *write_field = semcheck_find_class_field_including_hidden(
      symtab, object_record, property->write_accessor, NULL);
  if (write_field != NULL)
    return -1;

  HashNode_t *setter_node = semcheck_find_class_method(
      symtab, property_owner, property->write_accessor, NULL);
  if (setter_node == NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, setter %s for property %s not found.\n\n",
        stmt->line_num,
        property->write_accessor != NULL ? property->write_accessor
                                         : "<unknown>",
        property->name != NULL ? property->name : property_name);
    return 1;
  }

  if (setter_node->hash_type != HASHTYPE_PROCEDURE) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, property setter %s must be a procedure.\n\n",
        stmt->line_num, property->write_accessor);
    return 1;
  }

  return semcheck_convert_property_assignment_to_setter(
      symtab, stmt, lhs, setter_node, max_scope_lev);
}

int semcheck_try_indexed_property_assignment(SymTab_t *symtab,
                                             struct Statement *stmt,
                                             int max_scope_lev) {
  if (symtab == NULL || stmt == NULL || stmt->type != STMT_VAR_ASSIGN)
    return -1;

  struct Expression *lhs = stmt->stmt_data.var_assign_data.var;
  struct Expression *rhs = stmt->stmt_data.var_assign_data.expr;
  if (lhs == NULL || rhs == NULL || lhs->type != EXPR_ARRAY_ACCESS)
    return -1;

  struct Expression *array_expr = lhs->expr_data.array_access_data.array_expr;
  struct Expression *index_expr = lhs->expr_data.array_access_data.index_expr;
  if (array_expr == NULL || index_expr == NULL)
    return -1;

  struct Expression *object_expr = NULL;
  const char *property_name = NULL;
  struct RecordType *object_record = NULL;
  struct RecordType *property_owner = NULL;
  struct ClassProperty *property = NULL;

  if (array_expr->type == EXPR_RECORD_ACCESS) {
    object_expr = array_expr->expr_data.record_access_data.record_expr;
    property_name = array_expr->expr_data.record_access_data.field_id;
    if (object_expr == NULL || property_name == NULL)
      return -1;

    object_record = semcheck_with_resolve_record_type(
        symtab, object_expr,
        semcheck_tag_from_kgpc(object_expr->resolved_kgpc_type),
        stmt->line_num);
    if (object_record == NULL)
      return -1;

    property = semcheck_find_class_property(symtab, object_record,
                                            property_name, &property_owner);
  } else if (array_expr->type == EXPR_VAR_ID) {
    property_name = array_expr->expr_data.id;
    if (property_name == NULL)
      return -1;

    HashNode_t *self_node = NULL;
    if (FindSymbol(&self_node, symtab, "Self") != 0 || self_node == NULL)
      return -1;
    object_record = get_record_type_from_node(self_node);
    if (object_record == NULL)
      return -1;
    property = semcheck_find_class_property(symtab, object_record,
                                            property_name, &property_owner);
    /* Create a Self expression for the setter call (instance methods). */
    object_expr = mk_varid(stmt->line_num, strdup("Self"));
  }

  if (property == NULL || property->write_accessor == NULL ||
      !property->is_indexed)
    return -1;

  if (property_owner == NULL)
    property_owner = object_record;

  /* If write accessor is a field, rewrite the array base and let normal array
   * assignment handle it. */
  struct RecordField *write_field = semcheck_find_class_field_including_hidden(
      symtab, object_record, property->write_accessor, NULL);
  if (write_field != NULL) {
    if (array_expr->type == EXPR_RECORD_ACCESS) {
      if (!pascal_identifier_equals(
              array_expr->expr_data.record_access_data.field_id,
              property->write_accessor)) {
        free(array_expr->expr_data.record_access_data.field_id);
        array_expr->expr_data.record_access_data.field_id =
            strdup(property->write_accessor);
      }
    } else if (array_expr->type == EXPR_VAR_ID) {
      struct Expression *self_expr = mk_varid(stmt->line_num, strdup("Self"));
      if (self_expr == NULL)
        return -1;
      free(array_expr->expr_data.id);
      array_expr->expr_data.id = NULL;
      array_expr->type = EXPR_RECORD_ACCESS;
      memset(&array_expr->expr_data.record_access_data, 0,
             sizeof(array_expr->expr_data.record_access_data));
      array_expr->expr_data.record_access_data.record_expr = self_expr;
      array_expr->expr_data.record_access_data.field_id =
          strdup(property->write_accessor);
      array_expr->expr_data.record_access_data.field_offset = 0;
    }
    return -1;
  }

  HashNode_t *setter_node = semcheck_find_class_method(
      symtab, property_owner, property->write_accessor, NULL);
  if (setter_node == NULL) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, setter %s for property %s not found.\n\n",
        stmt->line_num,
        property->write_accessor != NULL ? property->write_accessor
                                         : "<unknown>",
        property->name != NULL ? property->name : property_name);
    return 1;
  }
  if (setter_node->hash_type != HASHTYPE_PROCEDURE) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, property setter %s must be a procedure.\n\n",
        stmt->line_num, property->write_accessor);
    return 1;
  }

  int is_static_setter = 0;
  if (property_owner != NULL && property_owner->type_id != NULL &&
      setter_node->id != NULL) {
    is_static_setter =
        from_cparser_is_method_static(property_owner->type_id, setter_node->id);
  }
  if (!is_static_setter && setter_node->type != NULL &&
      setter_node->type->kind == TYPE_KIND_PROCEDURE) {
    ListNode_t *params = kgpc_type_get_procedure_params(setter_node->type);
    if (params == NULL)
      is_static_setter = 1;
  }

  /* Save extra_indices before transformation clears array_access_data */
  ListNode_t *extra_indices = lhs->expr_data.array_access_data.extra_indices;
  lhs->expr_data.array_access_data.extra_indices = NULL;

  /* Detach needed subexpressions before destroying lhs. */
  if (array_expr->type == EXPR_RECORD_ACCESS)
    array_expr->expr_data.record_access_data.record_expr = NULL;
  lhs->expr_data.array_access_data.array_expr = NULL;
  lhs->expr_data.array_access_data.index_expr = NULL;
  stmt->stmt_data.var_assign_data.var = NULL;
  stmt->stmt_data.var_assign_data.expr = NULL;

  destroy_expr(lhs);

  ListNode_t *args_head = NULL;
  ListNode_t *args_tail = NULL;
  if (!is_static_setter) {
    if (object_expr == NULL)
      return -1;
    args_head = CreateListNode(object_expr, LIST_EXPR);
    if (args_head == NULL) {
      destroy_expr(object_expr);
      destroy_expr(index_expr);
      destroy_expr(rhs);
      return 1;
    }
    args_tail = args_head;
  } else if (object_expr != NULL) {
    destroy_expr(object_expr);
    object_expr = NULL;
  }

  ListNode_t *index_arg = CreateListNode(index_expr, LIST_EXPR);
  if (index_arg == NULL) {
    if (args_head != NULL)
      destroy_expr((struct Expression *)args_head->cur);
    destroy_expr(index_expr);
    destroy_expr(rhs);
    if (args_head != NULL)
      free(args_head);
    return 1;
  }
  if (args_tail != NULL)
    args_tail->next = index_arg;
  else
    args_head = index_arg;
  args_tail = index_arg;

  /* Append extra indices for multi-index properties (e.g. bitmap[x,y]) */
  while (extra_indices != NULL) {
    ListNode_t *next = extra_indices->next;
    extra_indices->next = NULL;
    args_tail->next = extra_indices;
    args_tail = extra_indices;
    extra_indices = next;
  }

  ListNode_t *value_arg = CreateListNode(rhs, LIST_EXPR);
  if (value_arg == NULL) {
    destroy_expr(rhs);
    return 1;
  }
  args_tail->next = value_arg;

  char *id_copy = setter_node->id != NULL ? strdup(setter_node->id) : NULL;
  char *mangled_copy = NULL;
  if (setter_node->mangled_id != NULL)
    mangled_copy = strdup(setter_node->mangled_id);
  if ((setter_node->id != NULL && id_copy == NULL) ||
      (setter_node->mangled_id != NULL && mangled_copy == NULL)) {
    free(id_copy);
    free(mangled_copy);
    return 1;
  }

  stmt->type = STMT_PROCEDURE_CALL;
  memset(&stmt->stmt_data.procedure_call_data, 0,
         sizeof(stmt->stmt_data.procedure_call_data));
  stmt->stmt_data.procedure_call_data.id = id_copy;
  stmt->stmt_data.procedure_call_data.mangled_id = mangled_copy;
  stmt->stmt_data.procedure_call_data.expr_args = args_head;
  stmt->stmt_data.procedure_call_data.resolved_proc = NULL;
  stmt->stmt_data.procedure_call_data.call_hash_type = 0;
  stmt->stmt_data.procedure_call_data.is_call_info_valid = 0;
  stmt->stmt_data.procedure_call_data.is_procedural_var_call = 0;
  stmt->stmt_data.procedure_call_data.procedural_var_symbol = NULL;
  stmt->stmt_data.procedure_call_data.procedural_var_expr = NULL;
  stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
  stmt->stmt_data.procedure_call_data.placeholder_method_name = NULL;
  semcheck_stmt_set_call_kgpc_type(stmt, NULL, 0);

  return semcheck_proccall(symtab, stmt, max_scope_lev);
}

/* ------------------------------------------------------------------ */
/* INTERNPROC handler: fpc_in_Rewrite_TypedFile / fpc_in_Reset_TypedFile
 *
 * When FPC's RTL declares  Procedure Rewrite(var f : TypedFile);
 * [INTERNPROC: fpc_in_Rewrite_TypedFile];  the compiler is expected to
 * determine the element size from the actual file type and transform
 * the 1-arg call into the 2-arg variant: Rewrite(f, SizeOf(ElementType)).
 *
 * Returns 1 if transformation was applied, 0 otherwise.              */
/* ------------------------------------------------------------------ */
int semcheck_internproc_typedfile_rewrite_reset(SymTab_t *symtab,
                                                struct Statement *stmt) {
  if (symtab == NULL || stmt == NULL)
    return 0;

  const char *proc_id = stmt->stmt_data.procedure_call_data.id;
  if (proc_id == NULL)
    return 0;

  /* Only applies to Rewrite / Reset */
  if (!pascal_identifier_equals(proc_id, "Rewrite") &&
      !pascal_identifier_equals(proc_id, "Reset"))
    return 0;

  /* Must have exactly 1 argument */
  ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
  if (args == NULL || args->next != NULL)
    return 0;

  /* Check whether any overload has the relevant INTERNPROC tag.
   * This ensures we only transform when using the FPC RTL. */
  const char *expected_tag = pascal_identifier_equals(proc_id, "Rewrite")
                                 ? "fpc_in_Rewrite_TypedFile"
                                 : "fpc_in_Reset_TypedFile";
  ListNode_t *candidates = FindAllIdents(symtab, proc_id);
  int found_internproc = 0;
  if (candidates != NULL) {
    for (ListNode_t *c = candidates; c != NULL; c = c->next) {
      HashNode_t *cand = (HashNode_t *)c->cur;
      if (cand != NULL && cand->internproc_id != NULL &&
          strcasecmp(cand->internproc_id, expected_tag) == 0) {
        found_internproc = 1;
        break;
      }
    }
    DestroyList(candidates);
  }
  if (!found_internproc) {
    return 0;
  }
  /* Get the first argument — should be a variable of type file-of-X */
  struct Expression *file_expr = (struct Expression *)args->cur;
  if (file_expr == NULL || file_expr->type != EXPR_VAR_ID ||
      file_expr->expr_data.id == NULL)
    return 0;

  HashNode_t *var_node = NULL;
  if (FindSymbol(&var_node, symtab, file_expr->expr_data.id) == 0 ||
      var_node == NULL || var_node->type == NULL)
    return 0;

  /* Look for file element type in the TypeAlias */
  struct TypeAlias *alias = kgpc_type_get_type_alias(var_node->type);
  if (alias == NULL || !alias->is_file) {
    /* If no TypeAlias from KgpcType, try looking up the variable's type
     * declaration. The type may be stored on a named type alias in the symbol
     * table. */
    return 0;
  }

  /* Determine element size */
  long long elem_size = 0;
  if (alias->file_type != UNKNOWN_TYPE && alias->file_type != 0) {
    elem_size = sizeof_from_type_tag(alias->file_type);
  } else if (alias->file_type_id != NULL) {
    HashNode_t *elem_type_node = NULL;
    if (FindSymbol(&elem_type_node, symtab, alias->file_type_id) != 0 &&
        elem_type_node != NULL && elem_type_node->type != NULL) {
      elem_size = kgpc_type_sizeof(elem_type_node->type);
    }
  }

  if (elem_size <= 0)
    return 0; /* Can't determine size, let normal resolution handle it */

  /* Inject the element size as a second argument: Rewrite(f, elemSize) */
  struct Expression *size_expr = mk_inum(stmt->line_num, (int)elem_size);
  if (size_expr == NULL)
    return 0;

  ListNode_t *size_arg = CreateListNode(size_expr, LIST_EXPR);
  if (size_arg == NULL) {
    destroy_expr(size_expr);
    return 0;
  }

  /* Append size argument after the file argument */
  args->next = size_arg;
  return 1;
}

/** COMPOUNT_STMT **/
int semcheck_compoundstmt(SymTab_t *symtab, struct Statement *stmt,
                          int max_scope_lev) {
  int return_val;
  ListNode_t *stmt_list;
  ListNode_t *slow = NULL;
  ListNode_t *fast = NULL;
  int guard = 0;
  const int guard_limit = 100000;
  assert(symtab != NULL);
  assert(stmt != NULL);
  assert(stmt->type == STMT_COMPOUND_STATEMENT);

  return_val = 0;
  stmt_list = stmt->stmt_data.compound_statement;
  slow = stmt_list;
  fast = stmt_list;
  while (stmt_list != NULL) {
    assert(stmt_list->type == LIST_STMT);
    guard++;
    if (guard > guard_limit) {
      fprintf(stderr,
              "ERROR: semcheck_compoundstmt exceeded guard limit (%d); "
              "possible cycle in stmt list (node=%p).\n",
              guard_limit, (void *)stmt_list);
      break;
    }
    if (fast != NULL && fast->next != NULL) {
      fast = fast->next->next;
      slow = slow ? slow->next : NULL;
      if (fast != NULL && slow == fast) {
        fprintf(stderr,
                "ERROR: Cycle detected in compound statement list (node=%p).\n",
                (void *)stmt_list);
        break;
      }
    }

    if (stmt_list->cur != NULL) {
      /* Transform two-arg New(p,Ctor)/Dispose(p,Dtor) before semcheck */
      int is_dispose = 0;
      struct Statement *extra_stmt = transform_two_arg_new_dispose(
          (struct Statement *)stmt_list->cur, &is_dispose);
      if (extra_stmt != NULL) {
        ListNode_t *new_node = (ListNode_t *)calloc(1, sizeof(ListNode_t));
        new_node->type = LIST_STMT;
        if (is_dispose) {
          /* Dispose: insert destructor call BEFORE Dispose(p) */
          new_node->cur = stmt_list->cur;
          new_node->next = stmt_list->next;
          stmt_list->cur = extra_stmt;
          stmt_list->next = new_node;
        } else {
          /* New: insert constructor call AFTER New(p) */
          new_node->cur = extra_stmt;
          new_node->next = stmt_list->next;
          stmt_list->next = new_node;
        }
      }

      return_val += semcheck_stmt(symtab, (struct Statement *)stmt_list->cur,
                                  max_scope_lev);
    }

    stmt_list = stmt_list->next;
  }

  if (g_debug_watch_stmt != NULL) {
    if (g_debug_watch_stmt->stmt_data.for_data.to != g_debug_watch_to_expr) {
      fprintf(stderr,
              "CRITICAL: g_debug_watch_stmt corrupted at end of compoundstmt! "
              "Changed from %p to %p\n",
              g_debug_watch_to_expr, g_debug_watch_stmt->stmt_data.for_data.to);
    } else {
#ifdef DEBUG
      fprintf(stderr,
              "DEBUG: g_debug_watch_stmt OK at end of compoundstmt. to=%p\n",
              g_debug_watch_stmt->stmt_data.for_data.to);
#endif
    }
  }

  return return_val;
}

/** IF_THEN **/
int semcheck_ifthen(SymTab_t *symtab, struct Statement *stmt,
                    int max_scope_lev) {
  int return_val;
  int if_type;
  struct Expression *relop_expr;
  struct Statement *if_stmt, *else_stmt;

  assert(symtab != NULL);
  assert(stmt != NULL);
  assert(stmt->type == STMT_IF_THEN);

  return_val = 0;
  relop_expr = stmt->stmt_data.if_then_data.relop_expr;
  if_stmt = stmt->stmt_data.if_then_data.if_stmt;
  else_stmt = stmt->stmt_data.if_then_data.else_stmt;

  return_val +=
      semcheck_stmt_expr_tag(&if_type, symtab, relop_expr, INT_MAX, NO_MUTATE);

  if (if_type != BOOL && if_type != UNKNOWN_TYPE) {
    int err_line = stmt->line_num;
    int err_col = stmt->col_num;
    int err_source_index = -1;
    semcheck_expr_best_context(relop_expr, &err_line, &err_col,
                               &err_source_index);
    semcheck_error_with_context_at(
        err_line, err_col, err_source_index,
        "Error on line %d, expected relational inside if statement!\n\n",
        err_line);
    ++return_val;
  }

  return_val += semcheck_stmt_main(symtab, if_stmt, max_scope_lev);
  if (else_stmt != NULL)
    return_val += semcheck_stmt_main(symtab, else_stmt, max_scope_lev);

  return return_val;
}

/** WHILE **/
int semcheck_while(SymTab_t *symtab, struct Statement *stmt,
                   int max_scope_lev) {
  int return_val;
  int while_type;
  struct Expression *relop_expr;
  struct Statement *while_stmt;

  assert(symtab != NULL);
  assert(stmt != NULL);
  assert(stmt->type == STMT_WHILE);

  return_val = 0;
  relop_expr = stmt->stmt_data.while_data.relop_expr;
  while_stmt = stmt->stmt_data.while_data.while_stmt;

  return_val += semcheck_stmt_expr_tag(&while_type, symtab, relop_expr, INT_MAX,
                                       NO_MUTATE);
  if (while_type != BOOL && while_type != UNKNOWN_TYPE) {
    int err_line = stmt->line_num;
    int err_col = stmt->col_num;
    int err_source_index = -1;
    semcheck_expr_best_context(relop_expr, &err_line, &err_col,
                               &err_source_index);
    semcheck_error_with_context_at(
        err_line, err_col, err_source_index,
        "Error on line %d, expected relational inside while statement!\n\n",
        err_line);
    ++return_val;
  }

  semcheck_loop_depth++;
  return_val += semcheck_stmt_main(symtab, while_stmt, max_scope_lev);
  semcheck_loop_depth--;

  return return_val;
}

/** REPEAT **/
int semcheck_repeat(SymTab_t *symtab, struct Statement *stmt,
                    int max_scope_lev) {
  int return_val = 0;
  int until_type = UNKNOWN_TYPE;
  ListNode_t *body_list;

  assert(symtab != NULL);
  assert(stmt != NULL);
  assert(stmt->type == STMT_REPEAT);

  body_list = stmt->stmt_data.repeat_data.body_list;
  semcheck_loop_depth++;
  while (body_list != NULL) {
    struct Statement *body_stmt = (struct Statement *)body_list->cur;
    if (body_stmt != NULL)
      return_val += semcheck_stmt_main(symtab, body_stmt, max_scope_lev);
    body_list = body_list->next;
  }
  semcheck_loop_depth--;

  return_val += semcheck_stmt_expr_tag(&until_type, symtab,
                                       stmt->stmt_data.repeat_data.until_expr,
                                       INT_MAX, NO_MUTATE);
  if (until_type != BOOL && until_type != UNKNOWN_TYPE) {
    int err_line = stmt->line_num;
    int err_col = stmt->col_num;
    int err_source_index = -1;
    semcheck_expr_best_context(stmt->stmt_data.repeat_data.until_expr,
                               &err_line, &err_col, &err_source_index);
    semcheck_error_with_context_at(
        err_line, err_col, err_source_index,
        "Error on line %d, expected relational inside repeat statement!\n\n",
        err_line);
    ++return_val;
  }

  return return_val;
}

/** FOR **/
int semcheck_for(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev) {
  int return_val;
  int for_type = UNKNOWN_TYPE, to_type = UNKNOWN_TYPE;
  enum StmtType for_assign_type; /* Either var or var_assign */
  struct Statement *for_assign;
  struct Expression *for_var;

  struct Expression *to_expr;
  struct Statement *do_for;
  int for_type_owned = 0;
  int to_type_owned = 0;
  KgpcType *for_kgpc_type = NULL;
  KgpcType *to_kgpc_type = NULL;

  assert(symtab != NULL);
  assert(stmt != NULL);
  assert(stmt->type == STMT_FOR);

  for_assign_type = stmt->stmt_data.for_data.for_assign_type;
  assert(for_assign_type == STMT_FOR_VAR ||
         for_assign_type == STMT_FOR_ASSIGN_VAR);

  return_val = 0;
  for_var = NULL;
  if (for_assign_type == STMT_FOR_VAR) {
    for_var = stmt->stmt_data.for_data.for_assign_data.var;
    return_val += semcheck_stmt_expr_tag(&for_type, symtab, for_var,
                                         max_scope_lev, BOTH_MUTATE_REFERENCE);
    /* Check for type */
    if (!is_ordinal_type(for_type) && for_type != UNKNOWN_TYPE) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, expected ordinal type in \"for\" assignment!\n\n",
          stmt->line_num);
      ++return_val;
    }
  } else {
    for_assign = stmt->stmt_data.for_data.for_assign_data.var_assign;
    /* For type checked in here */
    return_val += semcheck_for_assign(symtab, for_assign, max_scope_lev);
    for_var = NULL;
    if (for_assign != NULL) {
      for_var = for_assign->stmt_data.var_assign_data.var;
      for_type = (for_var != NULL)
                     ? semcheck_tag_from_kgpc(for_var->resolved_kgpc_type)
                     : UNKNOWN_TYPE;
    }
  }

  to_expr = stmt->stmt_data.for_data.to;
  do_for = stmt->stmt_data.for_data.do_for;

  if (for_var != NULL) {
    for_kgpc_type = semcheck_resolve_expression_kgpc_type(
        symtab, for_var, max_scope_lev, BOTH_MUTATE_REFERENCE, &for_type_owned);
    if (for_type == UNKNOWN_TYPE && for_kgpc_type != NULL)
      for_type = semcheck_tag_from_kgpc(for_kgpc_type);
  }

  return_val +=
      semcheck_stmt_expr_tag(&to_type, symtab, to_expr, INT_MAX, NO_MUTATE);
  to_kgpc_type = semcheck_resolve_expression_kgpc_type(
      symtab, to_expr, INT_MAX, NO_MUTATE, &to_type_owned);
  if (to_type == UNKNOWN_TYPE && to_kgpc_type != NULL)
    to_type = semcheck_tag_from_kgpc(to_kgpc_type);

  int bounds_compatible = 1;
  if (for_type == UNKNOWN_TYPE && for_kgpc_type == NULL)
    bounds_compatible = 0;

  if (bounds_compatible) {
    if (for_type == to_type) {
      /* ok */
    } else if ((for_type == LONGINT_TYPE && to_type == INT_TYPE) ||
               (for_type == INT_TYPE && to_type == LONGINT_TYPE)) {
      /* ok */
    } else if (for_type == CHAR_TYPE && to_type == STRING_TYPE &&
               to_expr != NULL && to_expr->type == EXPR_STRING &&
               to_expr->expr_data.string != NULL &&
               strlen(to_expr->expr_data.string) == 1) {
      to_type = CHAR_TYPE;
      semcheck_expr_set_resolved_type(to_expr, CHAR_TYPE);
    } else if (for_kgpc_type != NULL && to_kgpc_type != NULL &&
               are_types_compatible_for_assignment(for_kgpc_type, to_kgpc_type,
                                                   symtab)) {
      /* ok */
    } else {
      bounds_compatible = 0;
    }
  }

  if (!bounds_compatible && for_type != UNKNOWN_TYPE &&
      to_type != UNKNOWN_TYPE) {
    semcheck_error_with_context_at(
        stmt->line_num, stmt->col_num, stmt->source_index,
        "Error on line %d, type mismatch in \"to\" assignment!\n\n",
        stmt->line_num);
    ++return_val;
  }

  if (for_kgpc_type != NULL && !is_ordinal_type(for_type) &&
      for_type != UNKNOWN_TYPE) {
    int legacy = semcheck_tag_from_kgpc(for_kgpc_type);
    if (!is_ordinal_type(legacy) && legacy != UNKNOWN_TYPE) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d, expected ordinal type in \"for\" assignment!\n\n",
          stmt->line_num);
      ++return_val;
    }
  }

  semcheck_loop_depth++;

  if (stmt->line_num == 42) {
    g_debug_watch_stmt = stmt;
    g_debug_watch_to_expr = stmt->stmt_data.for_data.to;
#ifdef DEBUG
    fprintf(stderr, "DEBUG: Watching stmt at line 42\n");
#endif
  }

  if (to_expr != NULL &&
      ((uintptr_t)to_expr == 0x686374616d || (uintptr_t)to_expr == 0x1db2)) {
    fprintf(stderr, "CRITICAL: to_expr is corrupted in semcheck_for!\n");
  }

  return_val += semcheck_stmt_main(symtab, do_for, max_scope_lev);
  semcheck_loop_depth--;

  if (stmt->stmt_data.for_data.to != to_expr) {
    fprintf(stderr,
            "CRITICAL: stmt->stmt_data.for_data.to changed from %p to %p "
            "during body processing!\n",
            to_expr, stmt->stmt_data.for_data.to);
  }

  if (for_type_owned && for_kgpc_type != NULL)
    destroy_kgpc_type(for_kgpc_type);
  if (to_type_owned && to_kgpc_type != NULL)
    destroy_kgpc_type(to_kgpc_type);

  return return_val;
}

/** FOR-IN **/
int semcheck_for_in(SymTab_t *symtab, struct Statement *stmt,
                    int max_scope_lev) {
  int return_val = 0;
  int loop_var_type, collection_type;
  int loop_var_nonordinal = 0;

  assert(symtab != NULL);
  assert(stmt != NULL);
  assert(stmt->type == STMT_FOR_IN);

  struct Expression *loop_var = stmt->stmt_data.for_in_data.loop_var;
  struct Expression *collection = stmt->stmt_data.for_in_data.collection;
  struct Statement *do_stmt = stmt->stmt_data.for_in_data.do_stmt;

  /* Check loop variable (must be a lvalue) */
  if (loop_var != NULL) {
    return_val += semcheck_stmt_expr_tag(&loop_var_type, symtab, loop_var,
                                         max_scope_lev, BOTH_MUTATE_REFERENCE);

    if (!is_ordinal_type(loop_var_type) && loop_var_type != UNKNOWN_TYPE) {
      loop_var_nonordinal = 1;
    }
  } else {
    loop_var_type = UNKNOWN_TYPE;
  }

  /* Check collection expression */
  if (collection != NULL) {
    int collection_type_owned = 0;
    int collection_is_array = 0;
    int collection_is_list = 0;
    int collection_is_string = 0;
    int collection_is_set = 0;
    int collection_is_enum_domain = 0;
    int collection_is_enumerator_class = 0;
    const char *list_element_id = NULL;

    return_val += semcheck_stmt_expr_tag(&collection_type, symtab, collection,
                                         INT_MAX, NO_MUTATE);
    collection_is_string = is_string_type(collection_type);

    KgpcType *collection_kgpc_type = semcheck_resolve_expression_kgpc_type(
        symtab, collection, INT_MAX, NO_MUTATE, &collection_type_owned);
    if (collection_kgpc_type != NULL) {
      if (kgpc_type_is_array(collection_kgpc_type)) {
        collection_is_array = 1;
      } else if (kgpc_type_is_set(collection_kgpc_type)) {
        collection_is_set = 1;
      } else if (collection_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                 (collection_kgpc_type->info.primitive_type_tag == ENUM_TYPE ||
                  is_integer_type(
                      collection_kgpc_type->info.primitive_type_tag)) &&
                 collection != NULL && collection->type == EXPR_VAR_ID &&
                 collection->expr_data.id != NULL) {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, collection->expr_data.id) != 0 &&
            type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) {
          collection_is_enum_domain = 1;
        }
      } else {
        /* Lists are represented as pointers to class records */
        KgpcType *record_candidate = collection_kgpc_type;
        if (kgpc_type_is_pointer(collection_kgpc_type))
          record_candidate = collection_kgpc_type->info.points_to;

        if (record_candidate != NULL && kgpc_type_is_record(record_candidate)) {
          struct RecordType *record_info =
              kgpc_type_get_record(record_candidate);
          if (record_info != NULL) {
            /* Prefer structured generic info over mangled-name parsing. */
            if (record_info->generic_decl != NULL &&
                record_info->generic_args != NULL &&
                record_info->num_generic_args > 0 &&
                record_info->generic_decl->name != NULL &&
                pascal_identifier_equals(record_info->generic_decl->name,
                                         "TFPGList")) {
              collection_is_list = 1;
              list_element_id = record_info->generic_args[0];
            }
            /* Check for default indexed property (handles TStringList and other
             * classes with FItems) */
            if (!collection_is_list &&
                record_info->default_indexed_property != NULL) {
              collection_is_list = 1;
              list_element_id = record_info->default_indexed_element_type_id;
            }
          }
        }
      }
    }

    KgpcType *enumerator_current_type = NULL;
    if (!collection_is_array && !collection_is_list && !collection_is_set &&
        !collection_is_enum_domain) {
      collection_is_enumerator_class = semcheck_collection_is_enumerator_class(
          symtab, collection_kgpc_type, &enumerator_current_type);
    }

    if (collection_is_string)
      collection_is_array = 1;

    if (!collection_is_array && !collection_is_list && !collection_is_set &&
        !collection_is_enum_domain && !collection_is_enumerator_class &&
        collection_type != RECORD_TYPE && collection_type != POINTER_TYPE &&
        collection_type != UNKNOWN_TYPE) {
      semcheck_error_with_context_at(
          stmt->line_num, stmt->col_num, stmt->source_index,
          "Error on line %d: for-in loop requires an array expression!\n\n",
          stmt->line_num);
      ++return_val;
    } else if (collection_is_enumerator_class) {
      int loop_var_type_owned = 0;
      KgpcType *loop_var_kgpc = semcheck_resolve_expression_kgpc_type(
          symtab, loop_var, max_scope_lev, MUTATE, &loop_var_type_owned);

      if (loop_var_kgpc == NULL || enumerator_current_type == NULL ||
          !kgpc_type_equals(loop_var_kgpc, enumerator_current_type)) {
        semcheck_error_with_context_at(
            stmt->line_num, stmt->col_num, stmt->source_index,
            "Error on line %d: for-in loop variable type does not match "
            "enumerator Current type!\n\n",
            stmt->line_num);
        ++return_val;
      }

      if (loop_var_type_owned && loop_var_kgpc != NULL)
        destroy_kgpc_type(loop_var_kgpc);
    } else if (!collection_is_list && loop_var_nonordinal) {
      int loop_var_type_owned = 0;
      KgpcType *loop_var_kgpc = semcheck_resolve_expression_kgpc_type(
          symtab, loop_var, max_scope_lev, MUTATE, &loop_var_type_owned);
      KgpcType *element_kgpc = NULL;

      if (collection_is_string) {
        element_kgpc = create_primitive_type(CHAR_TYPE);
      } else if (collection_kgpc_type != NULL &&
                 kgpc_type_is_array(collection_kgpc_type)) {
        element_kgpc = kgpc_type_get_array_element_type(collection_kgpc_type);
        if (element_kgpc != NULL)
          kgpc_type_retain(element_kgpc);
      }

      if (loop_var_kgpc != NULL && element_kgpc != NULL &&
          kgpc_type_equals(loop_var_kgpc, element_kgpc)) {
        loop_var_nonordinal = 0;
      } else {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num,
                                       stmt->source_index,
                                       "Error on line %d: for-in loop variable "
                                       "must be an ordinal type!\n\n",
                                       stmt->line_num);
        ++return_val;
      }

      if (loop_var_type_owned && loop_var_kgpc != NULL)
        destroy_kgpc_type(loop_var_kgpc);
      if (element_kgpc != NULL)
        destroy_kgpc_type(element_kgpc);
    }

    if (collection_type_owned && collection_kgpc_type != NULL)
      destroy_kgpc_type(collection_kgpc_type);
    if (enumerator_current_type != NULL)
      destroy_kgpc_type(enumerator_current_type);
    (void)list_element_id;
  } else {
    collection_type = UNKNOWN_TYPE;
  }

  /* Check body statement */
  if (do_stmt != NULL) {
    semcheck_loop_depth++;
    return_val += semcheck_stmt(symtab, do_stmt, max_scope_lev);
    semcheck_loop_depth--;
  }

  return return_val;
}

/* Essentially the same as the var assignment but with a restriction that it
 * must be an int */
int semcheck_for_assign(SymTab_t *symtab, struct Statement *for_assign,
                        int max_scope_lev) {
  int return_val;
  int type_first, type_second;
  struct Expression *var, *expr;
  int lhs_owned = 0;
  int rhs_owned = 0;
  KgpcType *lhs_kgpc_type = NULL;
  KgpcType *rhs_kgpc_type = NULL;

  assert(symtab != NULL);
  assert(for_assign != NULL);
  assert(for_assign->type == STMT_VAR_ASSIGN);

  return_val = 0;

  var = for_assign->stmt_data.var_assign_data.var;
  expr = for_assign->stmt_data.var_assign_data.expr;

  /* NOTE: Grammar will make sure the left side is a variable */
  return_val += semcheck_stmt_expr_tag(&type_first, symtab, var, max_scope_lev,
                                       BOTH_MUTATE_REFERENCE);
  return_val +=
      semcheck_stmt_expr_tag(&type_second, symtab, expr, INT_MAX, NO_MUTATE);

  lhs_kgpc_type = semcheck_resolve_expression_kgpc_type(
      symtab, var, max_scope_lev, BOTH_MUTATE_REFERENCE, &lhs_owned);
  rhs_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, expr, INT_MAX,
                                                        NO_MUTATE, &rhs_owned);

  if (type_first == UNKNOWN_TYPE && lhs_kgpc_type != NULL)
    type_first = semcheck_tag_from_kgpc(lhs_kgpc_type);
  if (type_second == UNKNOWN_TYPE && rhs_kgpc_type != NULL)
    type_second = semcheck_tag_from_kgpc(rhs_kgpc_type);

  int types_compatible = (type_first == type_second);
  if (!types_compatible) {
    if ((type_first == LONGINT_TYPE && type_second == INT_TYPE) ||
        (type_first == INT_TYPE && type_second == LONGINT_TYPE)) {
      types_compatible = 1;
    } else if (type_first == CHAR_TYPE && type_second == STRING_TYPE &&
               expr != NULL && expr->type == EXPR_STRING &&
               expr->expr_data.string != NULL &&
               strlen(expr->expr_data.string) == 1) {
      types_compatible = 1;
      type_second = CHAR_TYPE;
      semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
    } else if (lhs_kgpc_type != NULL && rhs_kgpc_type != NULL &&
               are_types_compatible_for_assignment(lhs_kgpc_type, rhs_kgpc_type,
                                                   symtab)) {
      types_compatible = 1;
    }
  }

  if (!types_compatible && type_first != UNKNOWN_TYPE &&
      type_second != UNKNOWN_TYPE) {
    semcheck_error_with_context_at(
        for_assign->line_num, for_assign->col_num, for_assign->source_index,
        "Error on line %d, type mismatch in \"for\" assignment statement!\n\n",
        for_assign->line_num);
    ++return_val;
  }

  if (!is_ordinal_type(type_first) && type_first != UNKNOWN_TYPE) {
    semcheck_error_with_context_at(for_assign->line_num, for_assign->col_num,
                                   for_assign->source_index,
                                   "Error on line %d, expected ordinal type in "
                                   "\"for\" assignment statement!\n\n",
                                   for_assign->line_num);
    ++return_val;
  }

  if (return_val == 0) {
    semcheck_expr_set_resolved_type(var, type_first);
    semcheck_expr_set_resolved_type(expr, type_second);
  }

  if (lhs_owned && lhs_kgpc_type != NULL)
    destroy_kgpc_type(lhs_kgpc_type);
  if (rhs_owned && rhs_kgpc_type != NULL)
    destroy_kgpc_type(rhs_kgpc_type);

  return return_val;
}
