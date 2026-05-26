#ifndef SEMCHECK_FUNCCALL_INTERNAL_H
#define SEMCHECK_FUNCCALL_INTERNAL_H

#include "SemCheck_Expr_Internal.h"
#include "SemCheck_stmt.h"

#define FUNCCALL_TIMINGS_ENABLED()                                             \
  (kgpc_getenv("KGPC_DEBUG_FUNCCALL_TIMINGS") != NULL)

/* All shared local state of semcheck_funccall, packed for inter-phase access.
 */
typedef struct {
  /* Parameters (not owned by ctx) */
  int *type_return;
  SymTab_t *symtab;
  struct Expression *expr;
  int max_scope_lev;
  int mutating;
  /* Shared locals */
  int return_val;
  int scope_return;
  int final_status;
  char *id;           /* alias into expr->expr_data.function_call_data.id */
  char *mangled_name; /* owned: free() in cleanup */
  int arg_type;
  int cur_arg;
  ListNode_t *true_args;
  ListNode_t *true_arg_ids;
  ListNode_t
      *args_given; /* alias into expr->expr_data.function_call_data.args_expr */
  ListNode_t *overload_candidates; /* owned: DestroyList() in cleanup */
  HashNode_t *hash_return;
  Tree_t *arg_decl;
  int was_unit_qualified;
  char *unit_qualifier_name; /* owned: free() in cleanup */
  double timing_start_ms;
  int injected_self;
  int is_operator_dispatch;
  int prefer_non_builtin;
} FunccallCtx;

/* FSM state enum.
 * FC_OVERLOAD_RESOLVE corresponds to the original `method_call_resolved:`
 * label. FC_OVERLOAD_POST corresponds to the original
 * `skip_overload_resolution:` label. FC_CLEANUP corresponds to the original
 * `funccall_cleanup:` label. */
typedef enum {
  FC_NORMALIZE,
  FC_INHERITED,
  FC_INTRINSICS,
  FC_EARLY_BUILTINS,
  FC_SELF,
  FC_METHOD,
  FC_CONSTRUCTOR,
  FC_OVERLOAD_SETUP,
  FC_OVERLOAD_RESOLVE,
  FC_OVERLOAD_POST,
  FC_CLEANUP,
  FC_DONE
} FunccallState;

double funccall_now_ms(void);
int semcheck_expr_is_explicit_char_typecast_for_call_local(
    const struct Expression *expr);
int semcheck_kgpc_type_is_char_like_for_call_local(const KgpcType *type);
int semcheck_expr_is_char_typecast_call_for_call_local(
    const struct Expression *expr);
int semcheck_try_rewrite_internconst_call(int *type_return, SymTab_t *symtab,
                                          struct Expression *expr);
void semcheck_bind_set_literal_to_expected_type_for_call(
    struct Expression *arg_expr, KgpcType *expected_kgpc, int *arg_type);
int semcheck_candidate_source_unit_index(HashNode_t *candidate);
int semcheck_candidate_current_unit_index(SymTab_t *symtab);
int semcheck_candidate_is_direct_for_current_unit(SymTab_t *symtab,
                                                  HashNode_t *candidate);
int semcheck_candidate_is_local_callable_for_shadowing(SymTab_t *symtab,
                                                       HashNode_t *candidate);
void semcheck_set_pointer_info_from_kgpc_type(struct Expression *expr,
                                              SymTab_t *symtab, KgpcType *type);
int semcheck_candidate_is_owner_method(HashNode_t *candidate,
                                       const char *owner_type_id);
ListNode_t *semcheck_find_outer_idents_excluding_owner_methods(
    SymTab_t *symtab, const char *id, const char *owner_type_id);
int semcheck_decl_is_untyped_param(Tree_t *decl);
int semcheck_call_has_noarg_identifier_calls(ListNode_t *args);
int semcheck_detach_unary_not_from_receiver(struct Expression *receiver_expr);
int resolve_param_type(Tree_t *decl, SymTab_t *symtab);

/*
 * semcheck_dispatch_builtin_func — table-driven dispatch for Pascal builtins.
 *
 * Looks up id (case-insensitively) in BUILTIN_FUNC_MAP and calls its handler.
 * Returns 1 and writes the handler's return code into *status_out on a hit;
 * returns 0 if the name is not in the table.
 *
 * Builtins that require argument-type inspection before dispatching
 * (UpCase, UpperCase, LowerCase, Trunc) are NOT in the table and must
 * be handled inline.
 */
int semcheck_dispatch_builtin_func(const char *id, int *type_return,
                                   SymTab_t *symtab, struct Expression *expr,
                                   int max_scope_lev, int *status_out);

/* State function declarations — defined in the
 * _early/_builtins/_self/_method/_overload files */
FunccallState funccall_state_normalize(FunccallCtx *ctx);
FunccallState funccall_state_inherited(FunccallCtx *ctx);
FunccallState funccall_state_intrinsics(FunccallCtx *ctx);
FunccallState funccall_state_early_builtins(FunccallCtx *ctx);
FunccallState funccall_state_self(FunccallCtx *ctx);
FunccallState funccall_state_method(FunccallCtx *ctx);
FunccallState funccall_state_constructor(FunccallCtx *ctx);
FunccallState funccall_state_overload_setup(FunccallCtx *ctx);
FunccallState funccall_state_overload_resolve(FunccallCtx *ctx);
FunccallState funccall_state_overload_post(FunccallCtx *ctx);

#endif /* SEMCHECK_FUNCCALL_INTERNAL_H */
