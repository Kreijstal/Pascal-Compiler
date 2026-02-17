/*
    Internal header for SemCheck_expr module.

    This header declares functions and types that are shared between the split
    implementation files (SemCheck_expr.c, SemCheck_Expr_Builtins.c, etc.)
    but should NOT be exposed to external code.

    External code should only include SemCheck_expr.h for the public API.
*/

#ifndef SEM_CHECK_EXPR_INTERNAL_H
#define SEM_CHECK_EXPR_INTERNAL_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif

#include "SemCheck_expr.h"
#include "SemCheck_stmt.h"
#include "SemCheck_overload.h"
#include "SemCheck_sizeof.h"
#include "../SemCheck.h"
#include "../NameMangling.h"
#include "../HashTable/HashTable.h"
#include "../SymTab/SymTab.h"
#include "../../List/List.h"
#include "../../ParseTree/tree.h"
#include "../../ParseTree/tree_types.h"
#include "../../ParseTree/type_tags.h"
#include "../../ParseTree/KgpcType.h"
#include "../../ParseTree/from_cparser.h"
#include "../../pascal_frontend.h"
#include "../../../identifier_utils.h"
#include "../../../format_arg.h"

/*===========================================================================
 * Type Definitions (internal)
 *===========================================================================*/

/* Entry for the with context stack */
typedef struct WithContextEntry {
    struct Expression *context_expr;
    struct RecordType *record_type;
} WithContextEntry;

/* Entry for type helper registry */
typedef struct TypeHelperEntry {
    char *base_type_id;
    int base_type_tag;
    struct RecordType *helper_record;
} TypeHelperEntry;

/*===========================================================================
 * Shared State (extern declarations - defined in SemCheck_Context.c)
 *===========================================================================*/

/* With statement context stack */
extern WithContextEntry *with_context_stack;
extern size_t with_context_count;
extern size_t with_context_capacity;

/* Type helper registry */
extern ListNode_t *type_helper_entries;

/*===========================================================================
 * Core Type Checking Functions
 *===========================================================================*/

/**
 * LEGACY WRAPPER - Use semcheck_expr_with_type() for new code.
 * 
 * This function is deprecated and should be replaced with either:
 * 1. semcheck_expr_with_type() - returns KgpcType* via output parameter
 * 2. semcheck_expr_main() - same, but more verbose signature
 * 
 * After calling this function, the expression's resolved_kgpc_type field
 * contains the full type information. The integer type_return is just
 * a lossy conversion of that type.
 * 
 * Migration pattern:
 *   OLD: semcheck_expr_legacy_tag(&tag, symtab, expr, scope, mutating);
 *        if (tag == INT_TYPE) ...
 * 
 *   NEW: semcheck_expr_with_type(&type, symtab, expr, scope, mutating);
 *        if (kgpc_type_is_integer(type)) ...
 */
static inline int semcheck_expr_legacy_tag(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int mutating)
{
    KgpcType *resolved = NULL;
    int result = semcheck_expr_main(symtab, expr, max_scope_lev, mutating, &resolved);
    if (type_return != NULL)
        *type_return = semcheck_tag_from_kgpc(resolved);
    return result;
}

/**
 * Semantic check an expression and return its KgpcType.
 * 
 * This is the preferred API for new code. The returned type is owned by the
 * expression (expr->resolved_kgpc_type) and should not be freed by the caller.
 * 
 * @param out_type Output parameter for the resolved type (may be NULL)
 * @param symtab Symbol table
 * @param expr Expression to check
 * @param max_scope_lev Maximum scope level for variable lookup
 * @param mutating Whether this expression is being mutated (assigned to)
 * @return Error count (0 on success)
 */
static inline int semcheck_expr_with_type(KgpcType **out_type, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int mutating)
{
    return semcheck_expr_main(symtab, expr, max_scope_lev, mutating, out_type);
}

/* Verifies a type is an INT_TYPE or REAL_TYPE */
int is_type_ir(int *type);

/* Checks if type is AND or OR operation */
int is_and_or(int *type);

/* Sets a type based on a hash_type - uses KgpcType when available */
int set_type_from_hashtype(int *type, HashNode_t *hash_node);

/* Check if two numeric types are compatible */
int types_numeric_compatible(int lhs, int rhs);

/* Coerce char and string operands for comparison */
void semcheck_coerce_char_string_operands(int *type_first, struct Expression *expr1,
                                          int *type_second, struct Expression *expr2);
void semcheck_expr_set_resolved_type(struct Expression *expr, int type_tag);

/* Check if expression is char-like (single char or char type) */
int semcheck_expr_is_char_like(struct Expression *expr);

/* Check if expression is a char pointer */
int semcheck_expr_is_char_pointer(struct Expression *expr);

/* Check if expression is a wide char pointer */
int semcheck_expr_is_wide_char_pointer(struct Expression *expr);

/* Promote a pointer expression to string type */
void semcheck_promote_pointer_expr_to_string(struct Expression *expr);

/*===========================================================================
 * Expression Checking Functions (access, operators)
 *===========================================================================*/

/* Variable identifier semantic check */
int semcheck_varid(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Array access semantic check */
int semcheck_arrayaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Record access semantic check */
int semcheck_recordaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Function call semantic check */
int semcheck_funccall(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Relational operator semantic check */
int semcheck_relop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Sign term semantic check */
int semcheck_signterm(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Addition operator semantic check */
int semcheck_addop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Multiplication operator semantic check */
int semcheck_mulop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Typecast semantic check */
int semcheck_typecast(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* IS expression semantic check */
int semcheck_is_expr(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* AS expression semantic check */
int semcheck_as_expr(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Pointer dereference semantic check */
int semcheck_pointer_deref(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Address-of operator semantic check */
int semcheck_addressof(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/*===========================================================================
 * Pointer/Array Info Helpers
 *===========================================================================*/

/* Clear pointer info from expression */
void semcheck_clear_pointer_info(struct Expression *expr);

/* Set pointer info on expression */
void semcheck_set_pointer_info(struct Expression *expr, int subtype, const char *type_id);

/* Clear array info from expression */
void semcheck_clear_array_info(struct Expression *expr);

/* Set array info from KgpcType */
void semcheck_set_array_info_from_kgpctype(struct Expression *expr, SymTab_t *symtab,
    KgpcType *array_type, int line_num);

/* Set array info from TypeAlias */
void semcheck_set_array_info_from_alias(struct Expression *expr, SymTab_t *symtab,
    struct TypeAlias *alias, int line_num);

/* Set array info from HashNode */
void semcheck_set_array_info_from_hashnode(struct Expression *expr, SymTab_t *symtab,
    HashNode_t *node, int line_num);

/*===========================================================================
 * Function Call Helpers
 *===========================================================================*/

/* Reset function call cache on expression */
void semcheck_reset_function_call_cache(struct Expression *expr);

/* Set function call target on expression */
void semcheck_set_function_call_target(struct Expression *expr, HashNode_t *target);

/* Set call KgpcType on function call expression */
void semcheck_expr_set_call_kgpc_type(struct Expression *expr, KgpcType *type,
    int owns_existing);

/* Set resolved KgpcType on expression (shared/retained) */
void semcheck_expr_set_resolved_kgpc_type_shared(struct Expression *expr, KgpcType *type);

/*===========================================================================
 * Type Resolution Helpers
 *===========================================================================*/

/* Map builtin type name to type tag */
int semcheck_map_builtin_type_name(SymTab_t *symtab, const char *id);

/* Resolve a type identifier to a type tag */
int resolve_type_identifier(int *out_type, SymTab_t *symtab,
    const char *type_id, int line_num);

/* Get type name from type tag */
const char *semcheck_type_tag_name(int type_tag);

/* Get base type name (after last dot) */
const char *semcheck_base_type_name(const char *id);

/* Normalize char type ID (AnsiChar, WideChar, etc.) */
const char *semcheck_normalize_char_type_id(const char *id);

/* Check if KgpcType is Currency type */
int semcheck_is_currency_kgpc_type(KgpcType *type);

/*===========================================================================
 * Record/Class Helpers
 *===========================================================================*/

/* Get RecordType from HashNode */
struct RecordType *get_record_type_from_node(HashNode_t *node);

/* Get TypeAlias from HashNode */
struct TypeAlias *get_type_alias_from_node(HashNode_t *node);

/* Check if node is a record type */
int node_is_record_type(HashNode_t *node);

/* Lookup record type by type_id */
struct RecordType *semcheck_lookup_record_type(SymTab_t *symtab, const char *type_id);

/* Lookup parent record in class hierarchy */
struct RecordType *semcheck_lookup_parent_record(SymTab_t *symtab,
    struct RecordType *record_info);

/* Find property in class hierarchy */
struct ClassProperty *semcheck_find_class_property(SymTab_t *symtab,
    struct RecordType *record_info, const char *property_name,
    struct RecordType **owner_out);

/* Find field in class hierarchy */
struct RecordField *semcheck_find_class_field(SymTab_t *symtab,
    struct RecordType *record_info, const char *field_name,
    struct RecordType **owner_out);

/* Find field in class hierarchy (including hidden fields) */
struct RecordField *semcheck_find_class_field_including_hidden(SymTab_t *symtab,
    struct RecordType *record_info, const char *field_name,
    struct RecordType **owner_out);

/* Find method in class hierarchy */
HashNode_t *semcheck_find_class_method(SymTab_t *symtab,
    struct RecordType *record_info, const char *method_name,
    struct RecordType **owner_out);

/* Get type name from expression (for operator overloading) */
const char *get_expr_type_name(struct Expression *expr, SymTab_t *symtab);

/*===========================================================================
 * Property Helpers
 *===========================================================================*/

/* Get type info from a class property */
int semcheck_property_type_info(SymTab_t *symtab, struct ClassProperty *property,
    int line_num, int *type_out, struct RecordType **record_out);

/* Transform expression to property getter call */
int semcheck_transform_property_getter_call(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating,
    HashNode_t *method_node, struct RecordType *owner_record);

/*===========================================================================
 * Array/Record Constructor Helpers
 *===========================================================================*/

/* Convert set literal to array literal */
int semcheck_convert_set_literal_to_array_literal(struct Expression *expr);

/* Typecheck an array literal */
int semcheck_typecheck_array_literal(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, int expected_type, const char *expected_type_id, int line_num);

/* Typecheck a record constructor */
int semcheck_typecheck_record_constructor(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, struct RecordType *record_type, int line_num);

/* Get record type from declaration */
struct RecordType *semcheck_record_type_from_decl(Tree_t *decl, SymTab_t *symtab);

/* Get expected KgpcType for a record field */
KgpcType *semcheck_field_expected_kgpc_type(SymTab_t *symtab, struct RecordField *field);

/*===========================================================================
 * With Context Helpers
 *===========================================================================*/

/* Ensure with context stack has capacity */
int ensure_with_capacity(void);

/* Resolve record type for with statement */
struct RecordType *resolve_record_type_for_with(SymTab_t *symtab,
    struct Expression *context_expr, int expr_type, int line_num);

/*===========================================================================
 * Builtin Call Helpers
 *===========================================================================*/

/* Free function call arguments, preserving one expression */
void semcheck_free_call_args(ListNode_t *args, struct Expression *preserve_expr);

/* Replace function call with integer literal */
void semcheck_replace_call_with_integer_literal(struct Expression *expr, long long value);

/* Prepare dynamic array High() call */
int semcheck_prepare_dynarray_high_call(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, struct Expression *array_expr);

/*===========================================================================
 * Builtin Functions (used by SemCheck_Expr_Builtins.c)
 *===========================================================================*/

int semcheck_builtin_chr(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_ord(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_length(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_copy(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_concat(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_pos(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_strpas(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_eof(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_eoln(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_assigned(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_abs(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_trunc(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_predsucc(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int is_succ);
int semcheck_builtin_lowhigh(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int is_high);
int semcheck_builtin_default(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_sizeof(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_ismanagedtype(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_unary_real(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, const char *display_name,
    const char *mangled_name, int result_type);
int semcheck_builtin_arctan2(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_hypot(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_logn(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_upcase(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_odd(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_sqr(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_random(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_randomrange(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_power(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_aligned(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
int semcheck_builtin_allocmem(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);

/*===========================================================================
 * Miscellaneous Internal Helpers
 *===========================================================================*/

/* Try to reinterpret a function call as a typecast */
int semcheck_try_reinterpret_as_typecast(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev);

/* Reinterpret typecast as call (for Create constructors) */
int semcheck_reinterpret_typecast_as_call(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);

/* Try indexed property getter */
int semcheck_try_indexed_property_getter(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

/* Mangle helper const ID */
char *semcheck_mangle_helper_const_id(const char *helper_type_id, const char *field_id);

/* Debug helper */
void semcheck_debug_expr_brief(const struct Expression *expr, const char *label);

#endif /* SEM_CHECK_EXPR_INTERNAL_H */
