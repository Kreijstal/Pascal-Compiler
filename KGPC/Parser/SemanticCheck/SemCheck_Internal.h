/*
    SemCheck_Internal.h - Internal declarations for semantic analysis

    This header manages shared internal state and function declarations between
    the split modules of the semantic checking stage (SemCheck.c, SemCheck_decls.c,
    and SemCheck_subprogram.c).
*/

#ifndef SEM_CHECK_INTERNAL_H
#define SEM_CHECK_INTERNAL_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <stdint.h>
#include <stdarg.h>
#include <math.h>
#include <time.h>

#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif

#include "SemCheck.h"
#include "../../flags.h"
#include "../../Optimizer/optimizer.h"
#include "../../identifier_utils.h"
#include "../ParseTree/tree.h"
#include "../ParseTree/tree_types.h"
#include "../ParseTree/KgpcType.h"
#include "../ParseTree/type_tags.h"
#include "../ParseTree/from_cparser.h"
#include "../ParseTree/operator_registry.h"
#include "../ParseTree/generic_types.h"
#include "../parser_error.h"
#include "../ErrVars.h"
#include "../pascal_frontend.h"
#include "./SymTab/SymTab.h"
#include "./HashTable/HashTable.h"
#include "NameMangling.h"
#include "SemChecks/SemCheck_stmt.h"
#include "SemChecks/SemCheck_expr.h"

/*===========================================================================
 * Shared State
 *===========================================================================*/

extern ListNode_t *g_semcheck_unit_names;
extern char *g_semcheck_current_unit_name;
extern char *g_semcheck_source_path;
extern char *g_semcheck_source_buffer;
extern size_t g_semcheck_source_length;
extern int g_semcheck_warning_count;
extern int g_semcheck_error_line;
extern int g_semcheck_error_col;
extern int g_semcheck_error_source_index;
extern Tree_t *g_semcheck_current_subprogram;
extern int resolve_type_depth;

#define MAX_RESOLVE_TYPE_DEPTH 100

/*===========================================================================
 * Shared Internal Functions
 *===========================================================================*/

/* Utility & Mapping */
int semcheck_map_builtin_type_name_local(const char *id);
int map_var_type_to_type_tag(enum VarType var_type);
const char *semcheck_base_type_name(const char *id);
const char *resolve_type_to_base_name(SymTab_t *symtab, const char *type_name);
char *semcheck_dup_type_id_from_ast(ast_t *node);
int semcheck_is_char_alias_name(const char *id);
int semcheck_alias_should_be_char_like(const char *alias_id, const char *target_id);
int semcheck_is_currency_type_id(const char *type_id);
int is_real_type_name(SymTab_t *symtab, const char *type_name);

/* Symbol Lookup Helpers */
int semcheck_find_ident_with_qualified_fallback(HashNode_t **out, SymTab_t *symtab, const char *id);
HashNode_t *semcheck_find_type_excluding_alias(SymTab_t *symtab, const char *type_id, struct TypeAlias *exclude_alias);
HashNode_t *semcheck_find_preferred_type_node(SymTab_t *symtab, const char *type_id);
HashNode_t *semcheck_find_type_node_with_unit_flag(SymTab_t *symtab, const char *type_id, int unit_flag);
void semcheck_update_symbol_alias(SymTab_t *symtab, const char *id, const char *alias);

/* Signature & Parameter Helpers */
int semcheck_param_decl_equivalent(const Tree_t *lhs, const Tree_t *rhs);
int semcheck_subprogram_signatures_equivalent(const Tree_t *lhs, const Tree_t *rhs);
int semcheck_param_list_equivalent(ListNode_t *lhs, ListNode_t *rhs);
ListNode_t *semcheck_create_builtin_param(const char *name, int type_tag);
ListNode_t *semcheck_create_builtin_param_var(const char *name, int type_tag);
void copy_default_values_to_impl_params(ListNode_t *fwd_params, ListNode_t *impl_params);

/* Constant Evaluation */
int evaluate_const_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value);
int evaluate_real_const_expr(SymTab_t *symtab, struct Expression *expr, double *out_value);
int evaluate_string_const_expr(SymTab_t *symtab, struct Expression *expr, char **out_value);
int evaluate_set_const_bytes(SymTab_t *symtab, struct Expression *expr, unsigned char *out_bytes, size_t out_bytes_size, size_t *out_size, long long *out_mask, int *is_char_set);
int expression_contains_real_literal_impl(SymTab_t *symtab, struct Expression *expr);
int expression_is_string(SymTab_t *symtab, struct Expression *expr);
int expression_is_set_const_expr(SymTab_t *symtab, struct Expression *expr);
int resolve_const_identifier(SymTab_t *symtab, const char *id, long long *out_value);
int resolve_scoped_enum_literal(SymTab_t *symtab, const char *type_name, const char *literal_id, long long *out_value);
char *build_qualified_identifier_from_expr(struct Expression *expr);

/* Declaration Helpers */
int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls);
int predeclare_types(SymTab_t *symtab, ListNode_t *type_decls);
int predeclare_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev, Tree_t *parent_subprogram);
int predeclare_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev, Tree_t *parent_subprogram);
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls);
int semcheck_type_decls(SymTab_t *symtab, ListNode_t *type_decls);
int semcheck_const_decls(SymTab_t *symtab, ListNode_t *const_decls);
int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev);
int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev, Tree_t *parent_subprogram);
KgpcType *build_function_return_type(Tree_t *subprogram, SymTab_t *symtab, int *return_val, int line_num);
void resolve_array_bounds_in_kgpctype(SymTab_t *symtab, KgpcType *kgpc_type, struct TypeAlias *alias);
int resolve_range_bounds_for_type(SymTab_t *symtab, const char *type_name, long long *out_start, long long *out_end);
void inherit_alias_metadata(SymTab_t *symtab, struct TypeAlias *alias);
void apply_builtin_integer_alias_metadata(struct TypeAlias *alias, const char *type_name);
int semcheck_single_const_decl(SymTab_t *symtab, Tree_t *tree);

/* Class & Record Helpers */
int check_circular_inheritance(SymTab_t *symtab, const char *class_name, const char *parent_name, int max_depth);
int merge_parent_class_fields(SymTab_t *symtab, struct RecordType *record_info, const char *class_name, int line_num);
int build_class_vmt(SymTab_t *symtab, struct RecordType *record_info, const char *class_name, int line_num);
void detect_default_indexed_property(struct RecordType *record_info, const struct RecordType *parent_record);
void add_class_vars_to_method_scope(SymTab_t *symtab, const char *method_id);
void copy_method_decl_defaults_to_impl(SymTab_t *symtab, Tree_t *subprogram);
int record_has_property(struct RecordType *record_info, const char *property_name);
struct ClassProperty *clone_class_property(const struct ClassProperty *property);
ListNode_t *clone_property_list_unique(struct RecordType *record_info, const ListNode_t *parent_props);

/* Error Reporting Internals */
void print_error_context(int line_num, int col_num, int source_index);
int semcheck_print_context_from_file(const char *file_path, int line_num, int col_num, int context_lines);
int semcheck_parse_line_directive(const char *line, size_t len);
int semcheck_line_from_source_offset(const char *buffer, size_t length, int source_offset);

/* Unified Helper Functions (moved to HashTable.h) */
enum VarType get_var_type_from_node(HashNode_t *node);
void mark_hashnode_unit_info(HashNode_t *node, int defined_in_unit, int is_public);
int semcheck_id_not_main(char *id);
int semcheck_const_decls_imported(SymTab_t *symtab, ListNode_t *const_decls);
int semcheck_const_decls_local(SymTab_t *symtab, ListNode_t *const_decls);
int statement_contains_asm_block(struct Statement *stmt);
double semcheck_now_ms(void);
void semcheck_timing_step(const char *label, double *last_ms);
#define SEMCHECK_TIMINGS_ENABLED() (getenv("KGPC_DEBUG_TIMINGS") != NULL)

#endif /* SEM_CHECK_INTERNAL_H */
