#ifndef SEMCHECK_STMT_INTERNAL_H
#define SEMCHECK_STMT_INTERNAL_H

#include "SemCheck_stmt.h"
#include "SemCheck_expr.h"
#include "../SymTab/SymTab.h"
#include "../../ParseTree/tree_types.h"
#include "../../ParseTree/KgpcType.h"
#include "../../ParseTree/tree.h"

/* Forward declarations of shared helpers defined in SemCheck_stmt.c
 * and used by SemCheck_stmt_proccall.c. */

void semcheck_expr_set_resolved_type(struct Expression *expr, int type_tag);

/* transform_two_arg_new_dispose was extracted from SemCheck_stmt.c into
 * SemCheck_stmt_proccall.c together with semcheck_proccall. */
struct Statement *transform_two_arg_new_dispose(struct Statement *stmt,
                                                int *is_dispose);

/* typedef for builtin handler, defined in SemCheck_stmt.c */
typedef int (*builtin_semcheck_handler_t)(SymTab_t *, struct Statement *, int);

/* Helpers defined in SemCheck_stmt.c, exposed for SemCheck_stmt_proccall.c */
int semcheck_try_indexed_property_assignment(SymTab_t *symtab,
                                             struct Statement *stmt,
                                             int max_scope_lev);
int semcheck_stmt_method_is_declared_constructor(SymTab_t *symtab,
                                                 struct RecordType *record_info,
                                                 const char *method_name);
int semcheck_param_types_compatible(Tree_t *param_decl, KgpcType *expected,
                                    KgpcType *actual, SymTab_t *symtab);
int semcheck_stmt_expr_tag(int *type_return, SymTab_t *symtab,
                           struct Expression *expr, int max_scope_lev,
                           int mutating);
int semcheck_try_record_assignment_operator(SymTab_t *symtab,
                                            struct Statement *stmt,
                                            KgpcType *lhs_type,
                                            KgpcType **rhs_type,
                                            int *rhs_owned);
int param_has_default_value(Tree_t *decl);
int append_default_args(ListNode_t **args_head, ListNode_t *formal_params,
                        int line_num);
void semcheck_stmt_set_call_kgpc_type(struct Statement *stmt, KgpcType *type,
                                      int owns_existing);
void semcheck_stmt_set_call_owner_info(struct Statement *stmt,
                                       const char *owner_class,
                                       const char *method_name);
void semcheck_stmt_set_receiver_virtual_dispatch(
    struct Statement *stmt, struct RecordType *receiver_record,
    const char *method_name, KgpcType *call_type);
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
int semcheck_var_decl_is_untyped(Tree_t *decl);
int semcheck_stmt_has_single_overload(SymTab_t *symtab, const char *proc_id);
int semcheck_stmt_try_set_method_mangled_id(SymTab_t *symtab,
                                            struct Statement *stmt,
                                            const char *proc_id,
                                            const char *mangled_id);
int semcheck_set_stmt_call_mangled_id(SymTab_t *symtab, struct Statement *stmt,
                                      int max_scope_lev);
int rewrite_tfpglist_constructor_if_needed(SymTab_t *symtab, int max_scope_lev,
                                           struct Expression *lhs,
                                           struct Expression **rhs_ptr);
int try_resolve_builtin_procedure(SymTab_t *symtab, struct Statement *stmt,
                                  const char *expected_name,
                                  builtin_semcheck_handler_t handler,
                                  int max_scope_lev, int *handled);
int semcheck_builtin_setlength(SymTab_t *symtab, struct Statement *stmt,
                               int max_scope_lev);
int semcheck_builtin_setstring(SymTab_t *symtab, struct Statement *stmt,
                               int max_scope_lev);
int semcheck_builtin_strproc(SymTab_t *symtab, struct Statement *stmt,
                             int max_scope_lev);
int semcheck_builtin_insert(SymTab_t *symtab, struct Statement *stmt,
                            int max_scope_lev);
int semcheck_builtin_delete(SymTab_t *symtab, struct Statement *stmt,
                            int max_scope_lev);
int semcheck_builtin_val(SymTab_t *symtab, struct Statement *stmt,
                         int max_scope_lev);
int semcheck_builtin_inc(SymTab_t *symtab, struct Statement *stmt,
                         int max_scope_lev);
int semcheck_builtin_dec(SymTab_t *symtab, struct Statement *stmt,
                         int max_scope_lev);
int semcheck_builtin_include_like(SymTab_t *symtab, struct Statement *stmt,
                                  int max_scope_lev, const char *display_name);
int semcheck_builtin_include(SymTab_t *symtab, struct Statement *stmt,
                             int max_scope_lev);
int semcheck_builtin_exclude(SymTab_t *symtab, struct Statement *stmt,
                             int max_scope_lev);
int semcheck_builtin_initialize_finalize(SymTab_t *symtab,
                                         struct Statement *stmt,
                                         int max_scope_lev,
                                         const char *display_name,
                                         int allow_count_arg);
int semcheck_builtin_initialize(SymTab_t *symtab, struct Statement *stmt,
                                int max_scope_lev);
int semcheck_builtin_finalize(SymTab_t *symtab, struct Statement *stmt,
                              int max_scope_lev);
int semcheck_builtin_assert(SymTab_t *symtab, struct Statement *stmt,
                            int max_scope_lev);
int semcheck_builtin_write_like(SymTab_t *symtab, struct Statement *stmt,
                                int max_scope_lev);
int semcheck_builtin_writestr(SymTab_t *symtab, struct Statement *stmt,
                              int max_scope_lev);
int semcheck_builtin_read_like(SymTab_t *symtab, struct Statement *stmt,
                               int max_scope_lev);
int semcheck_builtin_untyped_call(SymTab_t *symtab, struct Statement *stmt,
                                  int max_scope_lev, int first_arg_mutate);
int semcheck_builtin_assign(SymTab_t *symtab, struct Statement *stmt,
                            int max_scope_lev);
int semcheck_builtin_close(SymTab_t *symtab, struct Statement *stmt,
                           int max_scope_lev);
int semcheck_builtin_settextcodepage(SymTab_t *symtab, struct Statement *stmt,
                                     int max_scope_lev);
int semcheck_builtin_halt(SymTab_t *symtab, struct Statement *stmt,
                          int max_scope_lev);
int semcheck_builtin_error(SymTab_t *symtab, struct Statement *stmt,
                           int max_scope_lev);
int semcheck_builtin_getmem(SymTab_t *symtab, struct Statement *stmt,
                            int max_scope_lev);
int semcheck_builtin_freemem(SymTab_t *symtab, struct Statement *stmt,
                             int max_scope_lev);
int semcheck_builtin_move(SymTab_t *symtab, struct Statement *stmt,
                          int max_scope_lev);
int semcheck_builtin_reallocmem(SymTab_t *symtab, struct Statement *stmt,
                                int max_scope_lev);
int semcheck_builtin_new(SymTab_t *symtab, struct Statement *stmt,
                         int max_scope_lev);
int semcheck_builtin_dispose(SymTab_t *symtab, struct Statement *stmt,
                             int max_scope_lev);
int semcheck_internproc_typedfile_rewrite_reset(SymTab_t *symtab,
                                                struct Statement *stmt);

struct RecordType *semcheck_stmt_get_record_type_from_node(HashNode_t *node);

struct Expression *get_param_default_value_stmt(Tree_t *decl);
struct Expression *copy_default_expr(struct Expression *src);
KgpcType *resolve_param_type_with_owner(Tree_t *param_decl, SymTab_t *symtab,
                                        const char *owner_full,
                                        const char *owner_outer,
                                        int *param_type_owned);
HashNode_t *semcheck_find_untyped_mangled_match(ListNode_t *candidates,
                                                const char *proc_id,
                                                const char *call_mangled);

/* Declarations for functions moved to SemCheck_stmt_main.c */
int semcheck_stmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_func_stmt(SymTab_t *symtab, struct Statement *stmt,
                       int max_scope_lev);
int semcheck_stmt_main(SymTab_t *symtab, struct Statement *stmt,
                       int max_scope_lev);
int semcheck_varassign(SymTab_t *symtab, struct Statement *stmt,
                       int max_scope_lev);

/* Declarations for helpers promoted from static in SemCheck_stmt.c */
int semcheck_statement_list_nodes(SymTab_t *symtab, ListNode_t *stmts,
                                  int max_scope_lev);
int semcheck_is_currency_kgpc_type(KgpcType *type);
int semcheck_type_is_recordish(KgpcType *type);
void semcheck_maybe_promote_index0_string_var_to_shortstring(
    SymTab_t *symtab, struct Statement *stmt);
int semcheck_collection_is_enumerator_class(SymTab_t *symtab,
                                            KgpcType *collection_kgpc_type,
                                            KgpcType **out_current_type);
int semcheck_expr_best_context(const struct Expression *expr, int *out_line,
                               int *out_col, int *out_source_index);
const char *semcheck_record_type_id_from_kgpc(KgpcType *type);
int semcheck_expr_is_widechar(SymTab_t *symtab, struct Expression *expr);
int semcheck_type_is_char_like(KgpcType *type);
int semcheck_force_char_case_builtin_in_assignment(struct Expression *expr);
int semcheck_expr_is_char_ordinal_const(SymTab_t *symtab,
                                        struct Expression *expr);

#endif /* SEMCHECK_STMT_INTERNAL_H */
