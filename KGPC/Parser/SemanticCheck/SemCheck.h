/*
    Damon Gwinn
    Performs semantic checking on a given parse tree
*/

#ifndef SEM_CHECK_H
#define SEM_CHECK_H

#include <stdlib.h>
#include <stdio.h>
#include "../ParseTree/tree.h"
#include "SymTab/SymTab.h"

/*
 * Semantic-check invariant policy:
 * Internal compiler invariants must not be softened with "fallback" behavior.
 * These assertions intentionally abort in all builds so violations are fixed
 * instead of being hidden behind downgraded diagnostics.
 */
#define KGPC_SEMCHECK_HARD_ASSERT(cond, fmt, ...)                                 \
    do                                                                             \
    {                                                                              \
        if (!(cond))                                                               \
        {                                                                          \
            fprintf(stderr,                                                        \
                "FATAL: semcheck invariant failed at %s:%d: " fmt "\n",           \
                __FILE__, __LINE__, ##__VA_ARGS__);                                \
            abort();                                                               \
        }                                                                          \
    } while (0)

/* The main function for checking a tree */
/* Return values:
    0       -> Check successful
    -1      -> Check successful with warnings
    >= 1    -> Check failed with n errors
*/
SymTab_t *start_semcheck(Tree_t *parse_tree, int *sem_result);

/* start_semcheck variant that accepts a pre-existing symtab (from per-unit semcheck).
 * If existing_symtab is NULL, creates a new one. */
SymTab_t *start_semcheck_with_symtab(SymTab_t *existing_symtab, Tree_t *parse_tree, int *sem_result);

/* Create and initialize a SymTab_t with global scope and builtins.
 * Call this before loading units to enable per-unit semantic checking. */
SymTab_t *semcheck_init_symtab(void);

/* Wire unit scope dependencies from the unit_registry into the scope tree.
 * Call after all units are loaded but before program-level semcheck. */
void wire_all_unit_scope_deps(SymTab_t *symtab);

/* Semantic-check a single unit tree. Enters a SCOPE_UNIT scope;
 * caller must call LeaveScope() after this returns. */
int semcheck_unit(SymTab_t *symtab, Tree_t *tree);

/* Track which units have been fully semchecked.
 * Declarations from fully-checked units are skipped in semcheck_program(). */
int semcheck_is_unit_fully_checked(int unit_index);
void semcheck_mark_unit_fully_checked(int unit_index);

/* Lightweight per-unit semantic check: processes declarations only (types, consts,
 * vars, subprogram signatures) without checking subprogram bodies.
 * Enters a SCOPE_UNIT scope; caller must call LeaveScope() after this returns. */
int semcheck_unit_decls_only(SymTab_t *symtab, Tree_t *tree);

/* Helper to print semantic error with source code context
 * line_num: line number where error occurred
 * col_num: column number where error occurred (0 if unknown)
 * format: printf-style format string for error message
 * ...: arguments for format string
 */
void semantic_error(int line_num, int col_num, const char *format, ...);
void semantic_error_at(int line_num, int col_num, int source_index, const char *format, ...);
void semcheck_error_with_context_at(int line_num, int col_num, int source_index,
    const char *format, ...);
void semcheck_error_with_context(const char *format, ...);
int semcheck_tag_from_kgpc(const struct KgpcType *type);
void semcheck_set_error_context(int line_num, int col_num, int source_index);
void semcheck_clear_error_context(void);
void semcheck_set_source_path(const char *path);
void semcheck_set_source_buffer(const char *buffer, size_t length);
int semcheck_register_source_buffer(const char *path, const char *buffer, size_t length);

HashNode_t *semcheck_find_type_node_with_kgpc_type(SymTab_t *symtab, const char *type_id);
HashNode_t *semcheck_find_type_node_with_kgpc_type_ref(SymTab_t *symtab,
    const struct TypeRef *type_ref, const char *type_id);
int semcheck_is_unit_name(const char *name);
int semcheck_resolve_scoped_enum_literal(SymTab_t *symtab, const char *type_name,
    const char *literal_name, long long *out_value);
int semcheck_resolve_scoped_enum_literal_ref(SymTab_t *symtab, const struct QualifiedIdent *type_ref,
    const char *literal_name, long long *out_value);
const char *semcheck_get_current_subprogram_id(void);
int semcheck_get_current_unit_index(void);
const char *semcheck_get_current_subprogram_result_var_name(void);
const char *semcheck_get_current_subprogram_method_name(void);
const char *semcheck_get_current_subprogram_owner_class(void);
const char *semcheck_get_current_subprogram_owner_class_full(void);
const char *semcheck_get_current_subprogram_owner_class_outer(void);
int semcheck_get_current_subprogram_is_constructor(void);
KgpcType *semcheck_get_current_subprogram_return_kgpc_type(struct SymTab *symtab, int *owns_type);
ListNode_t *semcheck_clone_current_subprogram_actual_args(int include_self);

int expression_is_set_const_expr(SymTab_t *symtab, struct Expression *expr);
int evaluate_set_const_bytes(SymTab_t *symtab, struct Expression *expr,
    unsigned char *out_bytes, size_t out_bytes_size, size_t *out_size,
    long long *out_mask, int *is_char_set);

/* Predeclare types, enums, subprogram signatures, and trivial constants from
 * a program tree into the given unit's scope.  Used for System/prelude, which
 * is parsed as TREE_PROGRAM_TYPE but whose declarations must be visible to
 * other units loaded before semcheck_program() runs. */
void semcheck_predeclare_program_into_unit_scope(SymTab_t *symtab, Tree_t *program_tree,
    int unit_idx);

/* Cached getenv() for KGPC_* environment variables.
 * getenv() does a linear scan of the environment on each call; with hundreds of
 * debug checks in hot loops, this was consuming 9% of total CPU time.
 * This function caches the result of the first lookup for each variable name. */
const char *kgpc_getenv(const char *name);

#endif
