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

/* Save/restore the current unit context for field type resolution.
 * When resolving field types of a record defined in another unit,
 * temporarily set the unit context to that unit so type lookups
 * prefer same-unit types (e.g., system's pstring vs objpas's PString). */
int semcheck_save_unit_context(void);
void semcheck_restore_unit_context(int saved);

/* Cached getenv() for KGPC_* environment variables.
 * getenv() does a linear scan of the environment on each call; with hundreds of
 * debug checks in hot loops, this was consuming 9% of total CPU time.
 * This function caches the result of the first lookup for each variable name. */
const char *kgpc_getenv(const char *name);

#endif
