/*
    Overload resolution helpers extracted from SemCheck_expr.c
*/

#ifndef SEMCHECK_OVERLOAD_H
#define SEMCHECK_OVERLOAD_H

#include "SemCheck_expr.h"

typedef enum {
    MATCH_EXACT = 0,
    MATCH_PROMOTION,
    MATCH_CONVERSION,
    MATCH_INCOMPATIBLE
} MatchQualityKind;

/*
 * MatchQuality uses tiers with deterministic tie-breaker flags.
 * No penalty-based scoring is used; ties are resolved by explicit rules.
 */
typedef struct {
    MatchQualityKind kind;
    int exact_type_id;
    int exact_pointer_subtype;
    int exact_array_elem;
    int int_promo_rank;
} MatchQuality;

int semcheck_param_list_contains_name(ListNode_t *params, const char *name);
int semcheck_named_arg_type_compatible(Tree_t *formal_decl,
    struct Expression *rhs_expr, int rhs_type, SymTab_t *symtab);
int semcheck_candidate_is_builtin(SymTab_t *symtab, HashNode_t *node);

int semcheck_count_total_params(ListNode_t *params);
int semcheck_count_required_params(ListNode_t *params);
int semcheck_method_accepts_arg_count(ListNode_t *params, int arg_count, int *expects_self_out);
int semcheck_append_default_args(ListNode_t **args_head, ListNode_t *formal_params, int line_num);

int semcheck_resolve_overload(HashNode_t **best_match_out,
    int *best_rank_out,
    int *num_best_out,
    ListNode_t *overload_candidates,
    ListNode_t *args_given,
    SymTab_t *symtab,
    struct Expression *call_expr,
    int max_scope_lev,
    int prefer_non_builtin);

/* Check if two function candidates have equivalent signatures (same types for all params) */
int semcheck_candidates_share_signature(SymTab_t *symtab, HashNode_t *a, HashNode_t *b);

#endif
