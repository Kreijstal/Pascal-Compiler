/*
 * SemCheck_internal.h — Internal header for SemCheck module.
 *
 * This header declares functions shared between the split implementation
 * files (SemCheck_*.c under SemCheck_parts/) but NOT exposed to external code.
 *
 * External code should only include SemCheck.h for the public API.
 */

#ifndef SEMCHECK_INTERNAL_H
#define SEMCHECK_INTERNAL_H

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#include <time.h>
#ifndef _WIN32
#include <strings.h>
#include <unistd.h>
#include <fcntl.h>
#else
#include <io.h>
#include <fcntl.h>
#endif
#include <math.h>
#include <stdarg.h>
#include "../../common_utils.h"
#include "SemCheck.h"
#include "../ParseTree/ident_ref.h"
#include "SemChecks/SemCheck_sizeof.h"
#include "../../flags.h"
#include "../../identifier_utils.h"
#include "../../unit_registry.h"
#include "../../string_intern.h"
#include "../../Optimizer/optimizer.h"
#include "../../compilation_context.h"
#include "../pascal_frontend.h"
#include "../ParseTree/tree.h"
#include "../ParseTree/tree_types.h"

/* From SemCheck_Expr_Constructors.c */
int semcheck_typecheck_record_constructor(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, struct RecordType *record_type, int line_num);
#include "../ParseTree/KgpcType.h"
#include "../ParseTree/type_tags.h"

HashNode_t *semcheck_find_type_node_in_owner_chain(SymTab_t *symtab,
    const char *type_id, const char *owner_full, const char *owner_outer);
#include "../ParseTree/from_cparser.h"
#include "../ParseTree/operator_registry.h"
#include "../ParseTree/generic_types.h"
#include "../parser_error.h"
#include "../ErrVars.h"
#include "./SymTab/SymTab.h"
#include "./HashTable/HashTable.h"
#include "SemChecks/SemCheck_stmt.h"
#include "SemChecks/SemCheck_expr.h"
#include "NameMangling.h"

/* ===================================================================
 * Cross-module variable declarations
 *
 * These variables are defined in SemCheck_env_types_errors.c but
 * referenced from other SemCheck_*.c modules.
 * =================================================================== */

extern int g_semcheck_current_unit_index;
extern int g_semcheck_error_source_index;
extern int g_semcheck_error_suppress_source_index;
extern int g_semcheck_imported_decl_unit_index;
extern size_t g_semcheck_source_length;
extern int g_semcheck_warning_count;
extern char *g_semcheck_source_path;
extern char *g_semcheck_source_buffer;
extern const char *g_semcheck_error_unit_name;
extern Tree_t *g_semcheck_current_subprogram;

/* Shared macro for timing diagnostics */
#define SEMCHECK_TIMINGS_ENABLED() (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL)

/* ===================================================================
 * Conflicting function names — defined as static inline to avoid
 * linker conflicts with identically-named functions in other modules
 * =================================================================== */

/* Originally in SemCheck_symbols_and_class.inc */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    if (node == NULL) return NULL;
    
    /* Use hashnode helper which handles NULL KgpcType */
    struct RecordType *record = hashnode_get_record_type(node);
    if (record != NULL)
        return record;
        
    /* If not a direct record, check if it's a pointer to a record (Class types are pointers) */
    if (node->type != NULL && kgpc_type_is_pointer(node->type))
    {
        KgpcType *pointed_to = node->type->info.points_to;
        if (pointed_to != NULL && kgpc_type_is_record(pointed_to))
        {
            return kgpc_type_get_record(pointed_to);
        }
    }
    
    return NULL;
}

/* Originally in SemCheck_symbols_and_class.inc */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    if (node == NULL)
        return NULL;
    
    /* Use hashnode helper which handles NULL KgpcType */
    return hashnode_get_type_alias(node);
}

/* Originally in SemCheck_env_types_errors.inc */
static inline const char *semcheck_base_type_name(const char *id)
{
    /* Type identifiers are normalized before semcheck; avoid parsing strings here. */
    return id;
}

/* Originally in SemCheck_env_types_errors.inc */
static inline char *type_ref_render_mangled_unqualified(const TypeRef *ref)
{
    if (ref == NULL)
        return NULL;
    const char *base = type_ref_base_name(ref);
    if (base == NULL)
        return NULL;
    if (ref->num_generic_args <= 0)
        return strdup(base);

    size_t total = strlen(base) + 1;
    for (int i = 0; i < ref->num_generic_args; ++i)
    {
        char *arg = type_ref_render_mangled(ref->generic_args[i]);
        if (arg != NULL)
        {
            total += strlen(arg);
            free(arg);
        }
        if (i + 1 < ref->num_generic_args)
            total += 1;
    }

    char *out = (char *)malloc(total + 1);
    if (out == NULL)
        return NULL;
    out[0] = '\0';
    strcat(out, base);
    strcat(out, "$");
    for (int i = 0; i < ref->num_generic_args; ++i)
    {
        char *arg = type_ref_render_mangled(ref->generic_args[i]);
        if (arg != NULL)
        {
            strcat(out, arg);
            free(arg);
        }
        if (i + 1 < ref->num_generic_args)
            strcat(out, "$");
    }
    return out;
}


/* ===================================================================
 * Cross-module function declarations
 *
 * These functions are implemented in one SemCheck_*.c file but
 * called from another. They are NOT part of the public API.
 * =================================================================== */

/* --- From SemCheck_const_decls_and_builtins.c --- */

void prepush_trivial_imported_consts(SymTab_t *symtab, ListNode_t *const_decls,
    int include_local_unit_consts);
int semcheck_const_decls_imported_filtered(SymTab_t *symtab, ListNode_t *const_decls,
    int defer_current_unit_qualified);
int semcheck_const_decls_local(SymTab_t *symtab, ListNode_t *const_decls);
int semcheck_decl_only_typed_const_decls(SymTab_t *symtab, ListNode_t *typed_const_decls);

/* --- From SemCheck_const_eval.c --- */

typedef struct SubprogramPredeclLookup
{
    HashNode_t *exact_match;
    HashNode_t *first_mangled_match;
    HashNode_t *tree_match;
    HashNode_t *body_pair_match;
} SubprogramPredeclLookup;

KgpcType *build_function_return_type(Tree_t *subprogram, SymTab_t *symtab,
    int *error_count, int allow_undefined);
int const_fold_int_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value);
int const_fold_real_expr(SymTab_t *symtab, struct Expression *expr, double *out_value);
int evaluate_const_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value);
int evaluate_real_const_expr(SymTab_t *symtab, struct Expression *expr, double *out_value);
int evaluate_string_const_expr(SymTab_t *symtab, struct Expression *expr, char **out_value);
int expression_contains_real_literal_impl(SymTab_t *symtab, struct Expression *expr);
int expression_is_string(SymTab_t *symtab, struct Expression *expr);
int semcheck_const_expr_char_cast_value(struct Expression *expr, long long *out_value,
    int *out_char_size);
HashNode_t *semcheck_find_exact_type_node_for_qid(SymTab_t *symtab,
    const QualifiedIdent *type_ref);
SubprogramPredeclLookup semcheck_lookup_subprogram_predecl(
    SymTab_t *symtab,
    Tree_t *subprogram,
    const char *lookup_id,
    const char *mangled_id);
int semcheck_param_list_equivalent(ListNode_t *lhs, ListNode_t *rhs);
void semcheck_refresh_predecl_match(HashNode_t *node, Tree_t *subprogram);

/* --- From SemCheck_env_types_errors.c --- */

char *make_method_lookup_key(const char *class_name, const char *method_name);
enum VarType map_type_tag_to_var_type(int type_tag);
int map_var_type_to_type_tag(enum VarType var_type);
int resolve_array_bound_expr(SymTab_t *symtab, const char *expr, int *out_value);
int resolve_error_source_context(int source_index,
    char *directive_file_out, size_t dir_size, int search_registry);
int resolve_unit_error_location(int source_index, int line_num,
    char *directive_file_out, size_t dir_size,
    char *unit_chain_out, size_t chain_size);
int semcheck_alias_should_be_char_like(const char *alias_id, const char *target_id);
int semcheck_alias_targets_match(const struct TypeAlias *lhs, const struct TypeAlias *rhs);
void semcheck_assert_pointer_type_resolved(const char *id);
ListNode_t *semcheck_clone_string_list(const ListNode_t *src);
ListNode_t *semcheck_create_builtin_param(const char *name, int type_tag);
ListNode_t *semcheck_create_builtin_param_var(const char *name, int type_tag);
void semcheck_debug_error_step(const char *step, Tree_t *subprogram, int before, int after);
char *semcheck_dup_type_id_from_ast(ast_t *node);
HashNode_t *semcheck_find_owner_record_type_node(SymTab_t *symtab, const char *owner_id);
HashNode_t *semcheck_find_type_excluding_alias(SymTab_t *symtab, const char *type_id,
    struct TypeAlias *exclude_alias);
int semcheck_helper_self_is_var(SymTab_t *symtab, const char *base_type_id);
int semcheck_is_currency_type_id(const char *type_id);
int semcheck_is_explicit_unit_qualified_type_ref(const TypeRef *ref);
int semcheck_kgpc_type_is_char_like(const KgpcType *type);
int semcheck_kgpc_type_is_record_like(const KgpcType *type);
int semcheck_map_builtin_type_name_local(const char *id);
void semcheck_mark_const_decl_units(ListNode_t *const_decls, int unit_index);
void semcheck_mark_resolved_forward_stub(ListNode_t *type_decls, ListNode_t *limit,
    const char *type_id, int source_unit_index, const struct RecordType *canonical_record);
void semcheck_mark_subprogram_units(ListNode_t *subprograms, int unit_index);
void semcheck_mark_type_decl_units(ListNode_t *type_decls, int unit_index);
void semcheck_mark_var_decl_units(ListNode_t *var_decls, int unit_index);
void semcheck_maybe_qualify_nested_type(SymTab_t *symtab,
    const char *owner_full, const char *owner_outer,
    char **type_id, TypeRef **type_ref);
int semcheck_param_decl_equivalent(const Tree_t *lhs, const Tree_t *rhs);
void semcheck_qualify_nested_types_for_record(SymTab_t *symtab, struct RecordType *record_info);
struct RecordType *semcheck_record_from_type_decl(Tree_t *tree, SymTab_t *symtab);
void semcheck_restore_scope(SymTab_t *symtab, ScopeNode *saved_scope);
int semcheck_scope_level_for_type_candidate(SymTab_t *symtab, HashNode_t *candidate);
int semcheck_subprogram_signatures_equivalent(const Tree_t *lhs, const Tree_t *rhs);
ScopeNode *semcheck_switch_to_unit_scope(SymTab_t *symtab, int unit_index);
int semcheck_type_candidate_is_forward_stub(HashNode_t *node);
void semcheck_unit_name_add(const char *name);
void semcheck_unit_names_add_list(ListNode_t *units);
void semcheck_unit_names_reset(void);

/* --- From SemCheck_init_and_inheritance.c --- */

void detect_default_indexed_property(struct RecordType *record_info, const struct RecordType *parent_record);
int merge_parent_class_fields(SymTab_t *symtab, struct RecordType *record_info, const char *class_name, int line_num);
int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls);
int predeclare_types(SymTab_t *symtab, ListNode_t *type_decls);

/* --- From SemCheck_program_and_vars.c --- */

void register_nested_type_short_alias(SymTab_t *symtab,
    const char *owner_class_outer, const char *owner_class, const char *method_name,
    const char *mangled_id, KgpcType *type, int is_function);

/* --- From SemCheck_subprograms.c --- */

int predeclare_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev, Tree_t *parent_subprogram);
int predeclare_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram);

/* --- From SemCheck_symbols_and_class.c --- */

void add_class_vars_to_method_scope(SymTab_t *symtab, Tree_t *subprogram);
void add_outer_class_vars_to_method_scope(SymTab_t *symtab, Tree_t *subprogram);
void apply_builtin_integer_alias_metadata(struct TypeAlias *alias, const char *type_name);
ListNode_t *collect_non_typed_var_decls(ListNode_t *decls);
ListNode_t *collect_typed_const_decls_filtered(SymTab_t *symtab, ListNode_t *decls, int from_unit_only);
void copy_default_values_to_impl_params(ListNode_t *fwd_params, ListNode_t *impl_params);
void copy_method_decl_defaults_to_impl(SymTab_t *symtab, Tree_t *subprogram);
void copy_method_identity_to_node(HashNode_t *node, Tree_t *subprogram);
enum VarType get_var_type_from_node(HashNode_t *node);
void inherit_alias_metadata(SymTab_t *symtab, struct TypeAlias *alias);
void mark_hashnode_source_unit(HashNode_t *node, int unit_index);
void mark_hashnode_unit_info(SymTab_t *symtab, HashNode_t *node,
    int defined_in_unit, int is_public);
void mark_latest_type_node_unit_info(SymTab_t *symtab, const char *type_id,
    int defined_in_unit, int unit_is_public, int source_unit_index);
HashNode_t *semcheck_find_type_node_with_unit_flag(SymTab_t *symtab,
    const char *type_id, int defined_in_unit);
HashNode_t *semcheck_find_type_node_with_unit_flag_ref(SymTab_t *symtab,
    const TypeRef *type_ref, const char *type_id, int defined_in_unit);
int semcheck_prefer_unit_defined_owner(void);
void semcheck_propagate_method_identity(SymTab_t *symtab, Tree_t *subprogram);
void semcheck_update_symbol_alias(SymTab_t *symtab, const char *id, const char *alias);

/* --- From SemCheck_vmt_and_type_decls.c --- */

QualifiedIdent *build_qualified_ident_from_expr(struct Expression *expr);
char *build_qualified_identifier_from_expr(struct Expression *expr);
void resolve_array_bounds_in_kgpctype(SymTab_t *symtab, KgpcType *kgpc_type, struct TypeAlias *alias);
int resolve_const_identifier(SymTab_t *symtab, const char *id, long long *out_value);
void semcheck_refresh_generic_specialization_vmts(SymTab_t *symtab,
    ListNode_t *type_decls);

/* --- Additional cross-module declarations (non-static, previously implicit) --- */

HashNode_t *semcheck_find_preferred_type_node_with_ref(SymTab_t *symtab,
    const TypeRef *type_ref, const char *type_id);
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls);
void semcheck_add_builtins(SymTab_t *symtab);
int semcheck_program(SymTab_t *symtab, Tree_t *tree);
int semcheck_type_decls(SymTab_t *symtab, ListNode_t *type_decls);
int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram);
void semcheck_expr_set_resolved_type(struct Expression *expr, int type_tag);
int semcheck_id_not_main(char *id);
int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num);
int semcheck_const_decls(SymTab_t *symtab, ListNode_t *const_decls);

#endif /* SEMCHECK_INTERNAL_H */
