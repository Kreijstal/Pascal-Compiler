/*
    Damon Gwinn
    Performs semantic checking on a given statement

    NOTE: Max scope level refers to the highest level scope we can reference a variable at
        - 0 is the current scope, 1 is the first above and so on
        - Functions can't have side effects, but they can contain procedures so this is a
            general way to define the maximum scope level
*/

#ifndef SEM_CHECK_EXPR_H
#define SEM_CHECK_EXPR_H
#define BOTH_MUTATE_REFERENCE 2
#define MUTATE 1
#define NO_MUTATE 0

#include "../SymTab/SymTab.h"
#include "../../ParseTree/tree_types.h"
#include "../../ParseTree/KgpcType.h"

/* Semantic check on a normal expression */
int semcheck_expr(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Semantic check on a function expression (no side effects allowed) */
int semcheck_expr_func(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int mutating);

/* Main semantic checking */
/* NOTE: Using one of the above two functions is more readable */
int semcheck_expr_main(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Helper function to resolve KgpcType from an expression
 * This bridges the legacy type system to the new KgpcType system.
 * Returns NULL if type cannot be resolved.
 * If owns_type is not NULL, it will be set to 1 (caller must free the returned type).
 */
KgpcType* semcheck_resolve_expression_kgpc_type(SymTab_t *symtab, struct Expression *expr,
    int max_scope_lev, int mutating, int *owns_type);

struct Expression *clone_expression(const struct Expression *expr);

void set_hash_meta(HashNode_t *node, int mutating);

int semcheck_with_push(struct Expression *context_expr, struct RecordType *record_type);
void semcheck_with_pop(void);
int semcheck_with_try_resolve(const char *field_id, SymTab_t *symtab,
    struct Expression **out_record_expr, int line_num);
struct RecordType *semcheck_with_resolve_record_type(SymTab_t *symtab,
    struct Expression *context_expr, int expr_type, int line_num);

void semcheck_mark_static_link_needed(int scope_level, HashNode_t *node);
void semcheck_mark_call_requires_static_link(HashNode_t *node);
int semcheck_prepare_array_literal_argument(Tree_t *formal_decl, struct Expression *arg_expr,
    SymTab_t *symtab, int max_scope_lev, int line_num);
int semcheck_prepare_record_constructor_argument(Tree_t *formal_decl, struct Expression *arg_expr,
    SymTab_t *symtab, int max_scope_lev, int line_num);

int semcheck_compute_record_size(SymTab_t *symtab, struct RecordType *record,
    long long *size_out, int line_num);

struct ClassProperty *semcheck_find_class_property(SymTab_t *symtab,
    struct RecordType *record_info, const char *property_name,
    struct RecordType **owner_out);

struct RecordField *semcheck_find_class_field(SymTab_t *symtab,
    struct RecordType *record_info, const char *field_name,
    struct RecordType **owner_out);

struct RecordField *semcheck_find_class_field_including_hidden(SymTab_t *symtab,
    struct RecordType *record_info, const char *field_name,
    struct RecordType **owner_out);

HashNode_t *semcheck_find_class_method(SymTab_t *symtab,
    struct RecordType *record_info, const char *method_name,
    struct RecordType **owner_out);

#endif
