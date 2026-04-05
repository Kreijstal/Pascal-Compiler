/*
 * from_cparser_internal.h — Internal header for from_cparser module.
 *
 * This header declares functions shared between the split implementation
 * files (from_cparser_*.c under from_cparser_parts/) but NOT exposed to
 * external code.
 *
 * External code should only include from_cparser.h for the public API.
 */

#ifndef FROM_CPARSER_INTERNAL_H
#define FROM_CPARSER_INTERNAL_H

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <stdarg.h>
#include <limits.h>
#include <errno.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
static inline char* strndup(const char* s, size_t n)
{
    size_t len = strnlen(s, n);
    char* buf = (char*)malloc(len + 1);
    if (buf == NULL)
        return NULL;
    memcpy(buf, s, len);
    buf[len] = '\0';
    return buf;
}
#endif

#include "from_cparser.h"
#include "../../string_intern.h"
#include "../../unit_registry.h"
#include "../../compilation_context.h"
#include "../pascal_frontend.h"

/* Cached getenv() — defined in SemCheck.c */
extern const char *kgpc_getenv(const char *name);

#include "../List/List.h"
#include "tree.h"
#include "tree_types.h"
#include "ident_ref.h"
#include "type_tags.h"
#include "pascal_parser.h"
#include "KgpcType.h"
#include "generic_types.h"
#include "../SemanticCheck/SymTab/SymTab.h"
#include "../../identifier_utils.h"
#include "../pascal_frontend.h"
#include "../ErrVars.h"

/* ===================================================================
 * Cross-module type definitions (moved from implementation files)
 * =================================================================== */

struct TypeHelperMapping
{
    char *helper_id;
    char *base_type_id;
};

typedef struct DeferredInlineSpec {
    Tree_t *type_decl;
    struct DeferredInlineSpec *next;
} DeferredInlineSpec;

typedef struct PendingGenericAlias {
    Tree_t *decl;
    char *base_name;
    ListNode_t *type_args;
    struct PendingGenericAlias *next;
} PendingGenericAlias;

/* Simple hash set for tracking visited AST nodes */
#define VISITED_SET_INITIAL_CAPACITY 256
#define VISITED_SET_LOAD_FACTOR 0.75

typedef struct VisitedSetEntry {
    ast_t *node;
    struct VisitedSetEntry *next;
} VisitedSetEntry;

typedef struct {
    VisitedSetEntry **buckets;
    size_t capacity;
    size_t size;
} VisitedSet;

typedef struct {
    int is_array;
    int is_array_of_const;
    int start;
    int end;
    int element_type;
    char *element_type_id;
    TypeRef *element_type_ref;
    struct KgpcType *element_kgpc_type;
    int is_shortstring;
    int is_open_array;
    ListNode_t *array_dimensions;
    char *array_dim_start_str;
    char *array_dim_end_str;
    int array_dims_parsed;
    int is_pointer;
    int pointer_type;
    char *pointer_type_id;
    TypeRef *pointer_type_ref;
    int is_set;
    int set_element_type;
    char *set_element_type_id;
    TypeRef *set_element_type_ref;
    int is_enum_set;
    ListNode_t *inline_enum_values;
    int is_enum;
    int enum_is_scoped;
    int enum_has_explicit_values;
    ListNode_t *enum_literals;
    int is_file;
    int file_type;
    char *file_type_id;
    TypeRef *file_type_ref;
    int is_record;
    struct RecordType *record_type;
    int is_generic_specialization;
    char *generic_base_name;
    ListNode_t *generic_type_args;
    TypeRef *type_ref;
    int is_range;
    int range_known;
    long long range_start;
    long long range_end;
    char *range_start_str;
    char *range_end_str;
    int is_class_reference;
    char *unresolved_index_type;
} TypeInfo;

typedef struct AstStringValue {
    char *data;
    size_t len;
} AstStringValue;

/* ===================================================================
 * Cross-module variable declarations
 *
 * These variables are defined in from_cparser_init_and_registry.c but
 * referenced from other from_cparser_*.c modules.
 * =================================================================== */

extern int anonymous_method_counter;
extern ListNode_t *class_method_bindings;
extern int g_allow_pending_specializations;
extern ListNode_t *g_const_sections;
extern const char *g_current_method_name;
extern DeferredInlineSpec *g_deferred_inline_specs;
extern int g_frontend_error_count;
extern ast_t *g_implementation_section_ast;
extern ast_t *g_implementation_type_section_ast;
extern struct RecordType *g_instantiate_record;
extern ast_t *g_interface_section_ast;
extern ast_t *g_interface_type_section_ast;
extern PendingGenericAlias *g_pending_generic_aliases;
extern char *g_scoped_enum_source_buffer;
extern size_t g_scoped_enum_source_length;
extern char *g_scoped_enum_source_path;
extern int g_source_offset;
extern const char *g_typed_const_unit_tag;
extern ListNode_t *type_helper_mappings;
extern int typed_const_counter;

/* ===================================================================
 * Cross-module function declarations
 * =================================================================== */

/* --- From from_cparser_const_and_types.c --- */

void append_case_label(ListBuilder *builder, ast_t *label_node);
int apply_shortstring_mode(int type_tag, const char *name);
void ast_string_value_reset(AstStringValue *value);
struct Expression *convert_record_constructor_expr(ast_t *expr_node);
int convert_type_spec(ast_t *type_spec, char **type_id_out,
                             struct RecordType **record_out, TypeInfo *type_info);
int evaluate_const_int_expr(ast_t *expr, int *out_value, int depth);
int evaluate_const_string_ast(ast_t *node, ast_t *const_section,
                                     AstStringValue *out_value, int depth);
int evaluate_simple_const_expr(const char *expr, ast_t *const_section, int *result);
void extend_list(ListNode_t **dest, ListNode_t *src);
int extract_specialize_type_info(ast_t *spec_node, char **base_name_out, ListNode_t **type_args_out);
struct RecordField *find_record_field_by_name(const struct RecordType *record,
    const char *field_name);
ast_t *find_type_decl_in_section(ast_t *type_section, const char *type_name);
int helper_self_param_is_var(const char *base_type_id, struct SymTab *symtab);
struct TypeAlias *helper_self_real_alias(const char *base_type_id);
int map_type_name(const char *name, char **type_id_out);
struct Expression *mk_const_array_element_lhs(int line_num, const char *array_name,
    int outer_index, int inner_index, int is_multidim);
int parse_integer_literal(const char *num_str, int base, long long *out_value, char **out_endptr);
void resolve_array_bounds(TypeInfo *info, ast_t *type_section, ast_t *const_section, const char *id_for_error);
int resolve_array_type_info_from_ast(const char *type_name, ast_t *type_section, TypeInfo *out_info, int depth);
int resolve_const_int_from_ast(const char *identifier, ast_t *const_section, int fallback_value);
int resolve_const_string_from_ast_internal(const char *identifier, ast_t *const_section,
                                                  const char **out_value, int depth);
int resolve_enum_literal_in_type(const char *type_name, const char *literal, ast_t *type_section);
int resolve_enum_ordinal_from_ast(const char *identifier, ast_t *type_section);
int resolve_enum_type_range_from_ast(const char *type_name, ast_t *type_section, int *out_start, int *out_end);
int resolve_enum_type_range_with_fallback(const char *type_name, ast_t *type_section,
                                                 int *out_start, int *out_end);
int tuple_is_record_constructor(ast_t *tuple_node);
int type_info_targets_char_array(const TypeInfo *type_info, int *is_widechar_out);
int type_name_exists_in_section(const char *name, ast_t *type_section);
int type_name_is_class_like(const char *type_name);
ast_t *unwrap_pascal_node(ast_t *node);
ast_t *unwrap_record_constructor_elem(ast_t *elem);

/* --- From from_cparser_declarations.c --- */

void append_const_decls_from_section(ast_t *const_section, ListNode_t **dest,
                                            ListBuilder *var_builder, ast_t *type_section);
void append_labels_from_section(ast_t *label_node, ListBuilder *builder);
void append_type_decls_from_section(ast_t *type_section, ListNode_t **dest,
    ListNode_t **subprograms, ListNode_t **const_decls, ListBuilder *var_builder,
    const char *parent_type_name);
void append_uses_from_section(ast_t *uses_node, ListNode_t **dest);
long long compute_range_storage_size(const TypeInfo *info);
void convert_routine_body(ast_t *body_node, ListNode_t **const_decls,
                                 ListBuilder *var_builder,
                                 ListBuilder *label_builder,
                                 ListNode_t **nested_subs,
                                 struct Statement **body_out,
                                 ListNode_t **type_decl_list);
ListNode_t *convert_var_section(ast_t *section_node);
void qualify_param_decl_types(ListNode_t *params,
    const char *owner_full, const char *owner_outer, SymTab_t *symtab);
int select_range_primitive_tag(const TypeInfo *info);

/* --- From from_cparser_expressions.c --- */

struct Statement *build_nested_with_statements(int line,
                                                      ast_t *context_node,
                                                      struct Statement *body_stmt);
char *collect_asm_text(ast_t *block_node);
struct Statement *convert_assignment(ast_t *assign_node);
struct Expression *convert_expression(ast_t *expr_node);
ListNode_t *convert_expression_list(ast_t *arg_node);
struct Expression *convert_field_width_expr(ast_t *field_width_node);
struct Expression *convert_member_access(ast_t *node);
struct Statement *convert_method_call_statement(ast_t *member_node, ast_t *args_start);
struct Statement *convert_proc_call(ast_t *call_node, bool implicit_identifier);
struct Expression *convert_set_literal(ast_t *set_node);
int extract_constant_int(struct Expression *expr, long long *out_value);
const char *tag_name(tag_t tag);

/* --- From from_cparser_generics.c --- */

void append_specialized_method_clones(Tree_t *decl, ListNode_t **subprograms);
void append_subprogram_node(ListNode_t **dest, Tree_t *tree);
void append_subprograms_from_ast_recursive(ast_t *node, ListNode_t **subprograms,
    VisitedSet *visited);
void append_top_level_subprograms_from_ast(ast_t *node, ListNode_t **subprograms,
    VisitedSet *visited, int in_subprogram);
ListNode_t *collect_constructed_type_args(ast_t *args_node);
char *dup_first_identifier_in_node(ast_t *node);
char *dup_symbol(ast_t *node);
int extract_constructed_type_info(ast_t *spec_node, char **base_name_out, ListNode_t **type_args_out);
ast_t *find_ast_node_type(ast_t *node, int typ);
struct RecordType *instantiate_generic_record(const char *base_name, ListNode_t *type_args, char **specialized_name_out);
int is_method_decl_keyword(const char *sym_name);
char *mangle_specialized_name_from_list(const char *base_name, ListNode_t *type_args);
void record_generic_method_impl(const char *class_name, const char *method_name, ast_t *method_ast);
int resolve_generic_alias_type(const char *base_name, ListNode_t *type_args,
    char **type_id_out, TypeInfo *type_info, int *result_out);
void substitute_generic_identifiers(ast_t *node, char **params, char **args, int count);

/* --- From from_cparser_init_and_registry.c --- */

const char *ast_symbol_name(ast_t *node);
void cmb_index_reset(void);
void cmb_method_index_reset(void);
ast_t *const_decl_index_lookup(const char *name);
void const_decl_index_scan_section(ast_t *section);
int const_section_is_resourcestring(ast_t *const_section);
int count_params_in_method_impl(ast_t *method_node);
void destroy_type_info_contents(TypeInfo *info);
char *encode_operator_name(const char *op_name);
void enum_registry_add(const char *name, int start, int end);
void enum_registry_free(void);
int enum_registry_lookup(const char *name, int *out_start, int *out_end);
void enum_registry_scan_type_section(ast_t *type_section);
const char *find_class_for_method(const char *method_name);
int from_cparser_scopedenums_enabled_at_line(int target_line);
void frontend_error(const char *format, ...);
char *generate_anonymous_method_name(int is_function);
int is_external_directive(const char *directive);
int is_method_static_with_signature(const char *class_name, const char *method_name,
                                           int param_count, const char *param_sig);
int is_operator_token_name(const char *name);
bool is_safe_to_continue(VisitedSet *visited, ast_t *node);
int kgpc_debug_decl_scan_enabled(void);
int kgpc_debug_subprog_enabled(void);
int lookup_const_int(const char *name, int *out_value);
const char *lookup_type_helper_base(const char *helper_id);
char *mangle_method_name(const char *class_name, const char *method_name);
char *mangle_method_name_raw(const char *class_name, const char *method_name);
void mark_var_decl_static_storage(Tree_t *decl);
char *method_param_type_suffix(Tree_t *param_decl);
char *param_type_signature_from_method_impl(ast_t *method_node);
char *param_type_signature_from_params_ast(ast_t *params_ast);
int parse_guid_literal(const char *guid, uint32_t *d1, uint16_t *d2, uint16_t *d3, uint8_t d4[8]);
int parse_range_bound(const char *s);
QualifiedIdent *qualified_ident_from_ast(ast_t *node);
char *qualified_ident_join_prefix(const QualifiedIdent *qid, int count);
void register_class_method_ex(const char *class_name, const char *method_name,
                                      int is_virtual, int is_override, int is_static,
                                      int is_class_method,
                                      int param_count, char *param_sig);
void register_const_int(const char *name, int value);
void register_const_section(ast_t *const_section);
void register_pending_generic_alias(Tree_t *decl, TypeInfo *type_info);
void register_type_helper_mapping(const char *helper_id, const char *base_type_id);
void reset_const_sections(void);
int resolve_const_expr_from_sections(const char *expr, int *result);
struct Expression *set_expr_source_index(struct Expression *expr, ast_t *node);
int split_absolute_target(const char *absolute_target,
    char **out_base, char **out_field);
void tag_operator_call(struct Expression *expr, int is_operator);
TypeRef *type_ref_from_element_info(const TypeInfo *info, const char *type_id);
TypeRef *type_ref_from_info_or_id(const TypeInfo *info, const char *type_id);
TypeRef *type_ref_from_name_and_args(const char *base_name, ListNode_t *type_args);
TypeRef *type_ref_from_pointer_info(const TypeInfo *info, const char *type_id);
TypeRef *type_ref_from_qualifier_and_id(const char *qualifier, const char *id);
TypeRef *type_ref_from_single_name(const char *name);
bool visited_set_add(VisitedSet *set, ast_t *node);
bool visited_set_contains(VisitedSet *set, ast_t *node);
VisitedSet *visited_set_create(void);
void visited_set_destroy(VisitedSet *set);

/* --- From from_cparser_records.c --- */

ast_t *absolute_clause_target(ast_t *node);
void annotate_method_template(struct MethodTemplate *method_template, ast_t *method_ast);
void append_module_property_wrappers(ListNode_t **subprograms, ast_t *property_node);
void collect_record_nested_types(ast_t *node, ListBuilder *nested_type_builder);
struct RecordType *convert_class_type_ex(const char *class_name, ast_t *class_node, ListNode_t **nested_types_out);
ListNode_t *convert_identifier_list(ast_t **cursor);
struct RecordType *convert_interface_type_ex(const char *interface_name, ast_t *interface_node, ListNode_t **nested_types_out);
ListNode_t *convert_param(ast_t *param_node);
ListNode_t *convert_param_list(ast_t **cursor);
void convert_record_members(ast_t *node, ListBuilder *builder,
    ListBuilder *property_builder, ListBuilder *method_template_builder);
struct RecordType *convert_record_type(ast_t *record_node);
struct RecordType *convert_record_type_ex(ast_t *record_node, ListNode_t **nested_types_out);
struct MethodTemplate *create_method_template(ast_t *method_decl_node);
void destroy_method_template_instance(struct MethodTemplate *template);
int is_node_to_skip_as_initializer(ast_t *node);
bool is_var_hint_clause(ast_t *node);
char *pop_last_identifier(ListNode_t **ids);

/* --- From from_cparser_statements_and_programs.c --- */

struct Statement *convert_block(ast_t *block_node);
Tree_t *convert_function(ast_t *func_node);
Tree_t *convert_method_impl(ast_t *method_node);
Tree_t *convert_procedure(ast_t *proc_node);
struct Statement *convert_statement(ast_t *stmt_node);
ListNode_t *convert_statement_list(ast_t *stmt_list_node);
char *extract_external_name_from_node(ast_t *node);
int extract_generic_type_params(ast_t *type_param_list, char ***out_params);
ast_t *find_node_by_type(ast_t *node, int target_type);

#endif /* FROM_CPARSER_INTERNAL_H */
