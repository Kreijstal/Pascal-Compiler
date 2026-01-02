/*
    Damon Gwinn
    Performs semantic checking on a given statement

    NOTE: Max scope level refers to the highest level scope we can reference a variable at
        - 0 is the current scope, 1 is the first above and so on
        - Functions can't have side effects, but they can contain procedures so this is a
            general way to define the maximum scope level

    All functions set the pointer type_return to the type the expression will return
*/

#include <stdlib.h>

#include <stdio.h>
#include <assert.h>
#include <string.h>
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
#include "../../../identifier_utils.h"
#include "../../../format_arg.h"

static const char *semcheck_base_type_name(const char *id)
{
    if (id == NULL)
        return NULL;
    const char *dot = strrchr(id, '.');
    return (dot != NULL && dot[1] != '\0') ? (dot + 1) : id;
}

int is_type_ir(int *type);
static int types_numeric_compatible(int lhs, int rhs);
static void semcheck_coerce_char_string_operands(int *type_first, struct Expression *expr1,
    int *type_second, struct Expression *expr2);
static int semcheck_expr_is_char_like(struct Expression *expr);

/* Helper function to get type name from an expression for operator overloading */
static const char *get_expr_type_name(struct Expression *expr, SymTab_t *symtab)
{
    if (expr == NULL)
        return NULL;
    
    /* Try to get from record_type field (legacy) */
    if (expr->record_type != NULL && expr->record_type->type_id != NULL)
        return expr->record_type->type_id;
    
    /* Try to get from resolved_kgpc_type */
    if (expr->resolved_kgpc_type != NULL && kgpc_type_is_record(expr->resolved_kgpc_type))
    {
        struct RecordType *rec = kgpc_type_get_record(expr->resolved_kgpc_type);
        if (rec != NULL && rec->type_id != NULL)
            return rec->type_id;
    }
    
    /* For variable IDs, look up in symbol table */
    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL && symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, symtab, expr->expr_data.id) == 0 && node != NULL && node->type != NULL)
        {
            /* Check if this is a record type */
            if (kgpc_type_is_record(node->type))
            {
                struct RecordType *rec = kgpc_type_get_record(node->type);
                
                /* Try to get type_id from record */
                if (rec != NULL && rec->type_id != NULL)
                    return rec->type_id;
                
                /* Try to get from type_alias */
                if (node->type->type_alias != NULL && node->type->type_alias->target_type_id != NULL)
                    return node->type->type_alias->target_type_id;
            }
        }
    }
    
    return NULL;
}

static char *semcheck_mangle_helper_const_id(const char *helper_type_id, const char *field_id)
{
    if (helper_type_id == NULL || field_id == NULL)
        return NULL;
    size_t len = strlen(helper_type_id) + strlen(field_id) + 3;
    char *result = (char *)malloc(len);
    if (result == NULL)
        return NULL;
    snprintf(result, len, "%s__%s", helper_type_id, field_id);
    return result;
}

static int semcheck_map_builtin_type_name(SymTab_t *symtab, const char *id);

typedef struct TypeHelperEntry
{
    char *base_type_id;
    int base_type_tag;
    struct RecordType *helper_record;
} TypeHelperEntry;

static ListNode_t *type_helper_entries = NULL;

void semcheck_register_type_helper(struct RecordType *record_info, SymTab_t *symtab)
{
    if (record_info == NULL || symtab == NULL)
        return;
    if (!record_info->is_type_helper || record_info->helper_base_type_id == NULL)
        return;

    ListNode_t *cur = type_helper_entries;
    while (cur != NULL)
    {
        TypeHelperEntry *entry = (TypeHelperEntry *)cur->cur;
        if (entry != NULL && entry->helper_record == record_info)
            return;
        cur = cur->next;
    }

    TypeHelperEntry *entry = (TypeHelperEntry *)calloc(1, sizeof(TypeHelperEntry));
    if (entry == NULL)
        return;
    entry->base_type_id = strdup(record_info->helper_base_type_id);
    entry->base_type_tag = semcheck_map_builtin_type_name(symtab, record_info->helper_base_type_id);
    entry->helper_record = record_info;
    if (entry->base_type_id == NULL)
    {
        free(entry);
        return;
    }

    ListNode_t *node = CreateListNode(entry, LIST_UNSPECIFIED);
    if (node == NULL)
    {
        free(entry->base_type_id);
        free(entry);
        return;
    }
    node->next = type_helper_entries;
    type_helper_entries = node;
}

struct RecordType *semcheck_lookup_type_helper(SymTab_t *symtab,
    int base_type_tag, const char *type_name)
{
    (void)symtab;
    ListNode_t *cur = type_helper_entries;
    while (cur != NULL)
    {
        TypeHelperEntry *entry = (TypeHelperEntry *)cur->cur;
        if (entry != NULL)
        {
            if (base_type_tag != UNKNOWN_TYPE && entry->base_type_tag == base_type_tag)
                return entry->helper_record;
            if (type_name != NULL && entry->base_type_id != NULL &&
                pascal_identifier_equals(entry->base_type_id, type_name))
                return entry->helper_record;
        }
        cur = cur->next;
    }
    return NULL;
}

int is_and_or(int *type);

static void semcheck_expr_set_call_kgpc_type(struct Expression *expr, KgpcType *type,
    int owns_existing)
{
    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return;

    if (expr->expr_data.function_call_data.call_kgpc_type != NULL && owns_existing)
    {
        destroy_kgpc_type(expr->expr_data.function_call_data.call_kgpc_type);
    }
    expr->expr_data.function_call_data.call_kgpc_type = NULL;

    if (type != NULL)
    {
        kgpc_type_retain(type);
        expr->expr_data.function_call_data.call_kgpc_type = type;
    }
}

static void semcheck_expr_set_resolved_kgpc_type_shared(struct Expression *expr, KgpcType *type)
{
    if (expr == NULL)
        return;

    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }

    if (type != NULL)
    {
        kgpc_type_retain(type);
        expr->resolved_kgpc_type = type;
    }
}

static void semcheck_reset_function_call_cache(struct Expression *expr)
{
    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return;

    int had_call_info = (expr->expr_data.function_call_data.is_call_info_valid == 1);
    expr->expr_data.function_call_data.resolved_func = NULL;
    expr->expr_data.function_call_data.call_hash_type = HASHTYPE_VAR;
    semcheck_expr_set_call_kgpc_type(expr, NULL, had_call_info);
    expr->expr_data.function_call_data.is_call_info_valid = 0;
}

static void semcheck_set_function_call_target(struct Expression *expr, HashNode_t *target)
{
    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return;

    if (target == NULL)
    {
        semcheck_reset_function_call_cache(expr);
        return;
    }

    int had_call_info = (expr->expr_data.function_call_data.is_call_info_valid == 1);
    expr->expr_data.function_call_data.resolved_func = NULL;
    expr->expr_data.function_call_data.call_hash_type = target->hash_type;
    semcheck_expr_set_call_kgpc_type(expr, target->type, had_call_info);
    expr->expr_data.function_call_data.is_call_info_valid = 1;
}

/* Helper function to get RecordType from HashNode */
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

static struct RecordType *semcheck_lookup_parent_record(SymTab_t *symtab,
    struct RecordType *record_info)
{
    if (symtab == NULL || record_info == NULL ||
        record_info->parent_class_name == NULL)
        return NULL;

    HashNode_t *parent_node = NULL;
    if (FindIdent(&parent_node, symtab, record_info->parent_class_name) == -1 ||
        parent_node == NULL)
        return NULL;

    return get_record_type_from_node(parent_node);
}

struct ClassProperty *semcheck_find_class_property(SymTab_t *symtab,
    struct RecordType *record_info, const char *property_name,
    struct RecordType **owner_out)
{
    if (owner_out != NULL)
        *owner_out = NULL;
    if (symtab == NULL || record_info == NULL || property_name == NULL)
        return NULL;

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_find_class_property: searching for '%s' in record_info=%p\n",
            property_name, record_info);
        fprintf(stderr, "[SemCheck]   record_info->properties=%p\n", record_info->properties);
    }

    struct RecordType *current = record_info;
    while (current != NULL)
    {
        ListNode_t *node = current->properties;
        int prop_count = 0;
        while (node != NULL)
        {

            if (node->type == LIST_CLASS_PROPERTY && node->cur != NULL)
            {
                struct ClassProperty *property = (struct ClassProperty *)node->cur;

                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck]   Found property: '%s'\n",
                        property->name ? property->name : "<null>");
                }
                prop_count++;
                if (property->name != NULL &&
                    pascal_identifier_equals(property->name, property_name))
                {

                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck]   MATCHED property '%s'!\n", property->name);
                    }
                    if (owner_out != NULL)
                        *owner_out = current;
                    return property;
                }
            }
            node = node->next;
        }
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck]   Searched %d properties in this record, no match\n", prop_count);
        }
        current = semcheck_lookup_parent_record(symtab, current);
    }
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck]   Property '%s' NOT FOUND\n", property_name);
    }
    return NULL;
}

static struct RecordField *semcheck_find_class_field_impl(SymTab_t *symtab,
    struct RecordType *record_info, const char *field_name,
    struct RecordType **owner_out, int include_hidden)
{
    if (owner_out != NULL)
        *owner_out = NULL;
    if (symtab == NULL || record_info == NULL || field_name == NULL)
        return NULL;

    struct RecordType *current = record_info;
    while (current != NULL)
    {
        ListNode_t *field_node = current->fields;
        while (field_node != NULL)
        {
            if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
            {
                struct RecordField *field = (struct RecordField *)field_node->cur;
                if (field->name != NULL &&
                    (include_hidden || !record_field_is_hidden(field)) &&
                    pascal_identifier_equals(field->name, field_name))
                {
                    if (owner_out != NULL)
                        *owner_out = current;
                    return field;
                }
            }
            field_node = field_node->next;
        }
        current = semcheck_lookup_parent_record(symtab, current);
    }
    return NULL;
}

struct RecordField *semcheck_find_class_field(SymTab_t *symtab,
    struct RecordType *record_info, const char *field_name,
    struct RecordType **owner_out)
{
    return semcheck_find_class_field_impl(symtab, record_info, field_name,
        owner_out, 0);
}

struct RecordField *semcheck_find_class_field_including_hidden(SymTab_t *symtab,
    struct RecordType *record_info, const char *field_name,
    struct RecordType **owner_out)
{
    return semcheck_find_class_field_impl(symtab, record_info, field_name,
        owner_out, 1);
}

HashNode_t *semcheck_find_class_method(SymTab_t *symtab,
    struct RecordType *record_info, const char *method_name,
    struct RecordType **owner_out)
{
    if (owner_out != NULL)
        *owner_out = NULL;
    if (symtab == NULL || record_info == NULL || method_name == NULL)
        return NULL;

    struct RecordType *current = record_info;
    while (current != NULL)
    {
        if (current->type_id != NULL)
        {
            char mangled_name[256];
            snprintf(mangled_name, sizeof(mangled_name), "%s__%s", current->type_id, method_name);
            
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] semcheck_find_class_method: Searching for '%s' (mangled: '%s') in class '%s'\n", 
                    method_name, mangled_name, current->type_id);
            }

            HashNode_t *method_node = NULL;
            int find_result = FindIdent(&method_node, symtab, mangled_name);

            if (find_result != -1 && method_node != NULL)
            {
                if (method_node->hash_type == HASHTYPE_FUNCTION_RETURN)
                {
                    ListNode_t *all_methods = FindAllIdents(symtab, mangled_name);
                    ListNode_t *cur = all_methods;
                    while (cur != NULL)
                    {
                        HashNode_t *candidate = (HashNode_t *)cur->cur;
                        if (candidate != NULL &&
                            (candidate->hash_type == HASHTYPE_FUNCTION ||
                             candidate->hash_type == HASHTYPE_PROCEDURE))
                        {
                            method_node = candidate;
                            break;
                        }
                        cur = cur->next;
                    }
                    if (all_methods != NULL)
                        DestroyList(all_methods);
                }

                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_find_class_method: Found '%s' in class '%s'\n", 
                        method_name, current->type_id);
                }
                if (owner_out != NULL)
                    *owner_out = current;
                return method_node;
            }
        }
        current = semcheck_lookup_parent_record(symtab, current);
    }
    return NULL;
}
int set_type_from_hashtype(int *type, HashNode_t *hash_node);
int semcheck_arrayaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_funccall(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
static int semcheck_typecast(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
static int semcheck_is_expr(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
static int semcheck_as_expr(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
static int semcheck_property_type_info(SymTab_t *symtab, struct ClassProperty *property,
    int line_num, int *type_out, struct RecordType **record_out);
static int semcheck_transform_property_getter_call(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating,
    HashNode_t *method_node);
static void semcheck_clear_pointer_info(struct Expression *expr);
static void semcheck_set_pointer_info(struct Expression *expr, int subtype, const char *type_id);
static int resolve_type_identifier(int *out_type, SymTab_t *symtab,
    const char *type_id, int line_num);
static void semcheck_clear_array_info(struct Expression *expr);
static void semcheck_set_array_info_from_alias(struct Expression *expr, SymTab_t *symtab,
    struct TypeAlias *alias, int line_num);
static void semcheck_set_array_info_from_hashnode(struct Expression *expr, SymTab_t *symtab,
    HashNode_t *node, int line_num);
static struct RecordType *semcheck_lookup_record_type(SymTab_t *symtab, const char *type_id);
static HashNode_t *semcheck_find_preferred_type_node(SymTab_t *symtab, const char *type_id);
static int semcheck_pointer_deref(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
static int semcheck_convert_set_literal_to_array_literal(struct Expression *expr);
static int semcheck_builtin_unary_real(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, const char *display_name,
    const char *mangled_name, int result_type);
static int semcheck_builtin_arctan2(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
static int semcheck_builtin_hypot(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
static int semcheck_builtin_logn(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
static int semcheck_builtin_upcase(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
static int semcheck_builtin_odd(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
static int semcheck_builtin_sqr(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
static int semcheck_builtin_random(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
static int semcheck_builtin_randomrange(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
static int semcheck_builtin_randseed(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
static int semcheck_builtin_power(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
static int semcheck_typecheck_array_literal(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, int expected_type, const char *expected_type_id, int line_num);

static const char *semcheck_normalize_char_type_id(const char *id)
{
    if (id == NULL)
        return NULL;
    if (pascal_identifier_equals(id, "UnicodeChar") ||
        pascal_identifier_equals(id, "WideChar"))
        return "WideChar";
    if (pascal_identifier_equals(id, "Char") ||
        pascal_identifier_equals(id, "AnsiChar"))
        return "AnsiChar";
    return id;
}
int semcheck_prepare_array_literal_argument(Tree_t *formal_decl, struct Expression *arg_expr,
    SymTab_t *symtab, int max_scope_lev, int line_num);
int semcheck_prepare_record_constructor_argument(Tree_t *formal_decl, struct Expression *arg_expr,
    SymTab_t *symtab, int max_scope_lev, int line_num);
static int semcheck_typecheck_record_constructor(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, struct RecordType *record_type, int line_num);
static struct RecordType *semcheck_record_type_from_decl(Tree_t *decl, SymTab_t *symtab);
static KgpcType *semcheck_field_expected_kgpc_type(SymTab_t *symtab, struct RecordField *field);

static int semcheck_try_indexed_property_getter(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    if (type_return == NULL || symtab == NULL || expr == NULL)
        return -1;
    if (mutating != NO_MUTATE)
        return -1;
    if (expr->type != EXPR_ARRAY_ACCESS)
        return -1;

    struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
    struct Expression *index_expr = expr->expr_data.array_access_data.index_expr;
    if (array_expr == NULL)
        return -1;

    const char *base_id = NULL;
    if (array_expr->type == EXPR_VAR_ID)
        base_id = array_expr->expr_data.id;
    else if (array_expr->type == EXPR_FUNCTION_CALL &&
             array_expr->expr_data.function_call_data.args_expr == NULL)
        base_id = array_expr->expr_data.function_call_data.id;

    if (base_id == NULL || index_expr == NULL)
        return -1;

    size_t id_len = strlen(base_id);
    char *getter_id = (char *)malloc(id_len + 4);
    if (getter_id == NULL)
        return -1;
    snprintf(getter_id, id_len + 4, "Get%s", base_id);

    HashNode_t *getter_node = NULL;
    int getter_found = (FindIdent(&getter_node, symtab, getter_id) == 0);
    if (!getter_found || getter_node == NULL || getter_node->hash_type != HASHTYPE_FUNCTION)
    {
        free(getter_id);
        return -1;
    }

    ListNode_t *args = CreateListNode(index_expr, LIST_EXPR);
    if (args == NULL)
    {
        free(getter_id);
        return -1;
    }

    expr->type = EXPR_FUNCTION_CALL;
    memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
    expr->expr_data.function_call_data.id = getter_id;
    expr->expr_data.function_call_data.args_expr = args;
    expr->expr_data.function_call_data.mangled_id = NULL;
    semcheck_reset_function_call_cache(expr);

    destroy_expr(array_expr);

    return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
}

static int semcheck_map_builtin_type_name(SymTab_t *symtab, const char *id)
{
    if (id == NULL)
        return UNKNOWN_TYPE;

    /* Prefer looking up the identifier in the current symbol table so aliases
     * (e.g., SizeInt, NativeInt) use their declared storage sizes. */
    if (symtab != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, (char *)id) == 0 &&
            type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            int mapped = UNKNOWN_TYPE;
            set_type_from_hashtype(&mapped, type_node);
            if (mapped != UNKNOWN_TYPE)
                return mapped;
        }
    }

    /* Minimal fallback for core primitives */
    if (pascal_identifier_equals(id, "Integer"))
        return INT_TYPE;
    if (pascal_identifier_equals(id, "LongInt"))
        return LONGINT_TYPE;
    if (pascal_identifier_equals(id, "Real") || pascal_identifier_equals(id, "Double"))
        return REAL_TYPE;
    if (pascal_identifier_equals(id, "String") ||
        pascal_identifier_equals(id, "AnsiString") ||
        pascal_identifier_equals(id, "RawByteString") ||
        pascal_identifier_equals(id, "UnicodeString") ||
        pascal_identifier_equals(id, "WideString"))
        return STRING_TYPE;
    if (pascal_identifier_equals(id, "ShortString"))
        return SHORTSTRING_TYPE;
    if (pascal_identifier_equals(id, "Char") ||
        pascal_identifier_equals(id, "AnsiChar") ||
        pascal_identifier_equals(id, "WideChar") ||
        pascal_identifier_equals(id, "UnicodeChar"))
        return CHAR_TYPE;
    if (pascal_identifier_equals(id, "Boolean"))
        return BOOL;
    if (pascal_identifier_equals(id, "Pointer"))
        return POINTER_TYPE;
    if (pascal_identifier_equals(id, "CodePointer"))
        return POINTER_TYPE;
    if (pascal_identifier_equals(id, "Byte") || pascal_identifier_equals(id, "Word"))
        return INT_TYPE;

    return UNKNOWN_TYPE;
}

/* Detect Pascal-style typecasts that were parsed as function calls (TypeId(expr))
 * and reinterpret them so the usual EXPR_TYPECAST path can handle them. */
static int semcheck_try_reinterpret_as_typecast(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    const char *id = expr->expr_data.function_call_data.id;
    if (id == NULL)
        return 0;
    char *id_copy = strdup(id);
    if (id_copy == NULL)
        return 0;

    /* Only proceed if the callee resolves to a type identifier or a known builtin type */
    HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type(symtab, id);
    int target_type = UNKNOWN_TYPE;
    if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        set_type_from_hashtype(&target_type, type_node);
    if (target_type == UNKNOWN_TYPE && type_node != NULL && type_node->type != NULL &&
        kgpc_type_is_record(type_node->type))
    {
        target_type = RECORD_TYPE;
    }
    if (type_node == NULL)
        FindIdent(&type_node, symtab, (char *)id);
    if (target_type == UNKNOWN_TYPE && type_node != NULL && type_node->type != NULL &&
        kgpc_type_is_record(type_node->type))
    {
        target_type = RECORD_TYPE;
    }
    if (target_type == UNKNOWN_TYPE)
        target_type = semcheck_map_builtin_type_name(symtab, id);

    int is_type_identifier =
        (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) ||
        (type_node != NULL && type_node->type != NULL && kgpc_type_is_record(type_node->type)) ||
        (target_type != UNKNOWN_TYPE);
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
    {
        fprintf(stderr, "[SemCheck] try_typecast id=%s type_node=%p hash_type=%d target_type=%d\n",
            id, (void *)type_node, type_node != NULL ? type_node->hash_type : -1, target_type);
    }
    if (!is_type_identifier)
    {
        free(id_copy);
        return 0;
    }

    /* Require exactly one argument for a typecast */
    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
    {
        fprintf(stderr, "[SemCheck] try_typecast id=%s args=%d\n",
            id, args != NULL ? ListLength(args) : 0);
    }
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, typecast to %s expects exactly one argument.\n",
            expr->line_num, id);
        *type_return = UNKNOWN_TYPE;
        free(id_copy);
        return 1;
    }

    struct Expression *inner_expr = (struct Expression *)args->cur;

    /* Clean up function-call-specific fields without freeing the inner expression */
    if (expr->expr_data.function_call_data.id != NULL)
    {
        free(expr->expr_data.function_call_data.id);
        expr->expr_data.function_call_data.id = NULL;
    }
    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    if (expr->expr_data.function_call_data.call_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->expr_data.function_call_data.call_kgpc_type);
        expr->expr_data.function_call_data.call_kgpc_type = NULL;
    }

    /* Manually free the argument list nodes but keep the expression alive */
    ListNode_t *to_free = args;
    while (to_free != NULL)
    {
        ListNode_t *next = to_free->next;
        to_free->cur = NULL;
        free(to_free);
        to_free = next;
    }
    expr->expr_data.function_call_data.args_expr = NULL;

    /* Reinterpret as a typecast expression */
    expr->type = EXPR_TYPECAST;
    expr->expr_data.typecast_data.target_type = target_type;
    expr->expr_data.typecast_data.target_type_id = id_copy;
    expr->expr_data.typecast_data.expr = inner_expr;

    return semcheck_typecast(type_return, symtab, expr, max_scope_lev, NO_MUTATE);
}

/* Helper function to get TypeAlias from HashNode, preferring KgpcType when available */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    return hashnode_get_type_alias(node);
}



/* Helper function to check if a node is a record type */
static inline int node_is_record_type(HashNode_t *node)
{
    return hashnode_is_record(node);
}

static int semcheck_recordaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
static int semcheck_addressof(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

typedef struct WithContextEntry {
    struct Expression *context_expr;
    struct RecordType *record_type;
} WithContextEntry;

static WithContextEntry *with_context_stack = NULL;
static size_t with_context_count = 0;
static size_t with_context_capacity = 0;

static int ensure_with_capacity(void);
struct Expression *clone_expression(const struct Expression *expr);
static struct RecordType *resolve_record_type_for_with(SymTab_t *symtab,
    struct Expression *context_expr, int expr_type, int line_num);
static int semcheck_builtin_lowhigh(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int is_high);
static int semcheck_builtin_default(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
static void semcheck_free_call_args(ListNode_t *args, struct Expression *preserve_expr);
static void semcheck_replace_call_with_integer_literal(struct Expression *expr, long long value);
static int semcheck_prepare_dynarray_high_call(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, struct Expression *array_expr);

#define SIZEOF_RECURSION_LIMIT 64
#define POINTER_SIZE_BYTES 8

static int sizeof_from_hashnode(SymTab_t *symtab, HashNode_t *node,
    long long *size_out, int depth, int line_num);
static int sizeof_from_alias(SymTab_t *symtab, struct TypeAlias *alias,
    long long *size_out, int depth, int line_num);
static int sizeof_from_record(SymTab_t *symtab, struct RecordType *record,
    long long *size_out, int depth, int line_num);
static int sizeof_from_type_ref(SymTab_t *symtab, int type_tag,
    const char *type_id, long long *size_out, int depth, int line_num);
int resolve_record_field(SymTab_t *symtab, struct RecordType *record,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int line_num, int silent);
static int compute_field_size(SymTab_t *symtab, struct RecordField *field,
    long long *size_out, int depth, int line_num);
static int sizeof_from_record_members(SymTab_t *symtab, ListNode_t *members,
    long long *size_out, int depth, int line_num);
static int sizeof_from_variant_part(SymTab_t *symtab, struct VariantPart *variant,
    long long *size_out, int depth, int line_num);
static int find_field_in_members(SymTab_t *symtab, ListNode_t *members,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int depth, int line_num, int *found);
static int find_field_in_variant(SymTab_t *symtab, struct VariantPart *variant,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int depth, int line_num, int *found);
static void semcheck_clear_pointer_info(struct Expression *expr)
{
    if (expr == NULL)
        return;

    expr->pointer_subtype = UNKNOWN_TYPE;
    if (expr->pointer_subtype_id != NULL)
    {
        free(expr->pointer_subtype_id);
        expr->pointer_subtype_id = NULL;
    }
}

static void semcheck_set_pointer_info(struct Expression *expr, int subtype, const char *type_id)
{
    if (expr == NULL)
        return;

    semcheck_clear_pointer_info(expr);
    expr->pointer_subtype = subtype;
    if (type_id != NULL)
    {
        expr->pointer_subtype_id = strdup(type_id);
        if (expr->pointer_subtype_id == NULL)
            fprintf(stderr, "Error: failed to allocate pointer type identifier.\n");
    }
}

static void semcheck_clear_array_info(struct Expression *expr)
{
    if (expr == NULL)
        return;

    expr->is_array_expr = 0;
    expr->array_element_type = UNKNOWN_TYPE;
    if (expr->array_element_type_id != NULL)
    {
        free(expr->array_element_type_id);
        expr->array_element_type_id = NULL;
    }
    expr->array_lower_bound = 0;
    expr->array_upper_bound = -1;
    expr->array_element_size = 0;
    expr->array_is_dynamic = 0;
    expr->array_element_record_type = NULL;
}

static void semcheck_set_array_info_from_kgpctype(struct Expression *expr, SymTab_t *symtab,
    KgpcType *array_type, int line_num)
{
    if (expr == NULL || array_type == NULL || array_type->kind != TYPE_KIND_ARRAY)
        return;

    semcheck_clear_array_info(expr);
    expr->is_array_expr = 1;
    expr->array_lower_bound = array_type->info.array_info.start_index;
    expr->array_upper_bound = array_type->info.array_info.end_index;
    expr->array_is_dynamic = kgpc_type_is_dynamic_array(array_type);
    expr->array_element_size = (int)kgpc_type_get_array_element_size(array_type);

    KgpcType *element_type = kgpc_type_get_array_element_type(array_type);
    if (element_type != NULL)
    {
        expr->array_element_type = kgpc_type_get_legacy_tag(element_type);
        if (element_type->kind == TYPE_KIND_RECORD)
            expr->array_element_record_type = kgpc_type_get_record(element_type);
        else
            expr->array_element_record_type = NULL;
    }
    else
    {
        expr->array_element_type = UNKNOWN_TYPE;
        expr->array_element_record_type = NULL;
    }

    (void)symtab;
    (void)line_num;
}

static struct RecordType *semcheck_lookup_record_type(SymTab_t *symtab, const char *type_id)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_id);
    if (type_node == NULL)
        return NULL;

    struct RecordType *record = get_record_type_from_node(type_node);
    if (record != NULL)
        return record;

    struct TypeAlias *alias = get_type_alias_from_node(type_node);
    if (alias != NULL && alias->target_type_id != NULL)
    {
        HashNode_t *target_node = NULL;
        if (FindIdent(&target_node, symtab, alias->target_type_id) != -1 &&
            target_node != NULL)
        {
            return get_record_type_from_node(target_node);
        }
    }

    return NULL;
}

static HashNode_t *semcheck_find_preferred_type_node(SymTab_t *symtab, const char *type_id)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, (char *)type_id);
    if (matches == NULL)
    {
        /* Try stripping unit prefix from qualified name like "baseunix.stat" */
        const char *dot = strrchr(type_id, '.');
        if (dot != NULL && dot[1] != '\0')
            matches = FindAllIdents(symtab, (char *)(dot + 1));
    }
    HashNode_t *best = NULL;
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            if (best == NULL)
                best = node;
            else if (best->defined_in_unit && !node->defined_in_unit)
                best = node;
        }
        cur = cur->next;
    }
    if (matches != NULL)
        DestroyList(matches);
    return best;
}

static void semcheck_set_array_info_from_alias(struct Expression *expr, SymTab_t *symtab,
    struct TypeAlias *alias, int line_num)
{
    if (expr == NULL)
        return;

    semcheck_clear_array_info(expr);
    if (alias == NULL || !alias->is_array)
        return;

    if (alias->array_dimensions != NULL &&
        alias->array_start == 0 && alias->array_end < alias->array_start)
    {
        ListNode_t *first_dim = alias->array_dimensions;
        if (first_dim != NULL && first_dim->type == LIST_STRING && first_dim->cur != NULL)
        {
            char *dim_str = (char *)first_dim->cur;
            if (strstr(dim_str, "..") == NULL)
            {
                if (pascal_identifier_equals(dim_str, "Boolean"))
                {
                    alias->array_start = 0;
                    alias->array_end = 1;
                    alias->is_open_array = 0;
                }
                else if (symtab != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, dim_str) >= 0 &&
                        type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                    {
                        struct TypeAlias *dim_alias = get_type_alias_from_node(type_node);
                        if (dim_alias != NULL && dim_alias->is_enum &&
                            dim_alias->enum_literals != NULL)
                        {
                            int count = ListLength(dim_alias->enum_literals);
                            if (count > 0)
                            {
                                alias->array_start = 0;
                                alias->array_end = count - 1;
                                alias->is_open_array = 0;
                            }
                        }
                        else if (dim_alias != NULL && dim_alias->is_range &&
                            dim_alias->range_known)
                        {
                            alias->array_start = dim_alias->range_start;
                            alias->array_end = dim_alias->range_end;
                            alias->is_open_array = 0;
                        }
                    }
                }
            }
        }
    }

    expr->is_array_expr = 1;
    expr->array_lower_bound = alias->array_start;
    expr->array_upper_bound = alias->array_end;
    expr->array_is_dynamic = alias->is_open_array;
    expr->array_element_type = alias->array_element_type;
    if (alias->array_element_type_id != NULL)
    {
        expr->array_element_type_id = strdup(alias->array_element_type_id);
        if (expr->array_element_type_id == NULL)
            fprintf(stderr, "Error: failed to allocate array element type identifier.\n");
    }

    if (expr->array_element_type == UNKNOWN_TYPE && expr->array_element_type_id != NULL)
    {
        int resolved_type = UNKNOWN_TYPE;
        if (resolve_type_identifier(&resolved_type, symtab, expr->array_element_type_id, line_num) == 0)
            expr->array_element_type = resolved_type;
    }

    if (expr->array_element_type == RECORD_TYPE || expr->array_element_type == UNKNOWN_TYPE)
        expr->array_element_record_type = semcheck_lookup_record_type(symtab,
            expr->array_element_type_id);

    long long computed_size = 0;
    int size_status = 1;
    if (expr->array_element_record_type != NULL)
        size_status = sizeof_from_record(symtab, expr->array_element_record_type,
            &computed_size, 0, line_num);
    else if (expr->array_element_type != UNKNOWN_TYPE ||
        expr->array_element_type_id != NULL)
        size_status = sizeof_from_type_ref(symtab, expr->array_element_type,
            expr->array_element_type_id, &computed_size, 0, line_num);

    if (size_status == 0 && computed_size > 0 && computed_size <= INT_MAX)
        expr->array_element_size = (int)computed_size;
}

static void semcheck_set_array_info_from_hashnode(struct Expression *expr, SymTab_t *symtab,
    HashNode_t *node, int line_num)
{
    if (expr == NULL)
        return;

    semcheck_clear_array_info(expr);
    if (node == NULL || node->type == NULL ||
        !(kgpc_type_is_array(node->type) || kgpc_type_is_array_of_const(node->type)))
        return;

    expr->is_array_expr = 1;

    if (kgpc_type_is_array_of_const(node->type))
    {
        expr->array_lower_bound = 0;
        expr->array_upper_bound = -1;
        expr->array_is_dynamic = 1;
        expr->array_element_type = ARRAY_OF_CONST_TYPE;
        expr->array_element_size = (int)sizeof(kgpc_tvarrec);
        return;
    }
    
    /* Get array bounds from KgpcType */
    int node_lower_bound, node_upper_bound;
    hashnode_get_array_bounds(node, &node_lower_bound, &node_upper_bound);
    
    /* Get element size from KgpcType */
    long long node_element_size = hashnode_get_element_size(node);
    
    /* Check if array is dynamic */
    int node_is_dynamic = hashnode_is_dynamic_array(node);

    expr->array_lower_bound = node_lower_bound;
    expr->array_upper_bound = node_upper_bound;
    expr->array_is_dynamic = node_is_dynamic;
    expr->array_element_size = node_element_size;

    expr->array_element_type = UNKNOWN_TYPE;
    set_type_from_hashtype(&expr->array_element_type, node);

    struct TypeAlias *type_alias = get_type_alias_from_node(node);
    if (type_alias != NULL && type_alias->is_array)
    {
        semcheck_set_array_info_from_alias(expr, symtab, type_alias, line_num);

        if (expr->array_element_size <= 0 && node_element_size > 0)
            expr->array_element_size = node_element_size;
        else if (expr->array_element_size <= 0 &&
            type_alias->array_end >= type_alias->array_start)
        {
            long long count = (long long)type_alias->array_end -
                (long long)type_alias->array_start + 1;
            if (count > 0 && type_alias->array_element_type != UNKNOWN_TYPE)
            {
                long long element_size = 0;
                if (sizeof_from_type_ref(symtab, type_alias->array_element_type,
                        type_alias->array_element_type_id, &element_size,
                        0, line_num) == 0)
                {
                    expr->array_element_size = (int)element_size;
                }
            }
        }

        if (!expr->array_is_dynamic && node_is_dynamic)
            expr->array_is_dynamic = 1;

        if (node_lower_bound != 0)
            expr->array_lower_bound = node_lower_bound;
        if (node_upper_bound != 0)
            expr->array_upper_bound = node_upper_bound;
    }
    else if (node->type != NULL && node->type->kind == TYPE_KIND_ARRAY)
    {
        /* For inline array declarations (no TypeAlias), extract info from KgpcType */
        KgpcType *element_type = node->type->info.array_info.element_type;
        if (element_type != NULL)
        {
            if (element_type->kind == TYPE_KIND_RECORD)
            {
                if (element_type->info.record_info != NULL)
                {
                    expr->array_element_record_type = element_type->info.record_info;
                    expr->array_element_type = RECORD_TYPE;
                }
            }
            else if (element_type->kind == TYPE_KIND_PRIMITIVE)
            {
                expr->array_element_type = element_type->info.primitive_type_tag;
            }
            else if (element_type->kind == TYPE_KIND_ARRAY)
            {
                /* Element is itself an array (nested array) - get its type alias if available */
                struct TypeAlias *element_alias = element_type->type_alias;
                if (element_alias != NULL && element_alias->is_array)
                {
                    /* Propagate the element array's information to this expression
                     * so that further indexing works correctly */
                    if (element_alias->array_element_type_id != NULL)
                    {
                        expr->array_element_type_id = strdup(element_alias->array_element_type_id);
                        if (expr->array_element_type_id == NULL)
                            fprintf(stderr, "Error: failed to allocate array element type identifier.\n");
                    }
                    expr->array_element_type = element_alias->array_element_type;
                }
            }
            else if (element_type->kind == TYPE_KIND_POINTER)
            {
                /* Element is a pointer type (e.g., array of PChar) */
                expr->array_element_type = POINTER_TYPE;

                /* Get pointer target type info from the element type */
                KgpcType *points_to = element_type->info.points_to;
                if (points_to != NULL)
                {
                    if (points_to->kind == TYPE_KIND_PRIMITIVE)
                    {
                        /* Store pointer target type for later use in semcheck_arrayaccess */
                        /* This info will be transferred to the array access expression */
                    }
                    else if (points_to->kind == TYPE_KIND_RECORD && points_to->info.record_info != NULL)
                    {
                        expr->array_element_record_type = points_to->info.record_info;
                    }
                }

                /* If the element type has a type_alias with pointer info, use its type_id */
                struct TypeAlias *element_alias = element_type->type_alias;
                if (element_alias != NULL && element_alias->is_pointer && element_alias->pointer_type_id != NULL)
                {
                    expr->array_element_type_id = strdup(element_alias->pointer_type_id);
                    if (expr->array_element_type_id == NULL)
                        fprintf(stderr, "Error: failed to allocate array element type identifier.\n");
                }
            }
        }
    }
    else
    {
        expr->array_element_record_type = get_record_type_from_node(node);
    }
}

static int semcheck_convert_set_literal_to_array_literal(struct Expression *expr)
{
    if (expr == NULL || expr->type != EXPR_SET)
        return 1;

    /* First pass: ensure no ranges are present */
    ListNode_t *cur = expr->expr_data.set_data.elements;
    while (cur != NULL)
    {
        struct SetElement *element = (struct SetElement *)cur->cur;
        if (element != NULL && element->upper != NULL)
            return 1;
        cur = cur->next;
    }

    /* Second pass: transfer elements */
    ListNode_t *array_list = NULL;
    ListNode_t *array_tail = NULL;
    int count = 0;

    cur = expr->expr_data.set_data.elements;
    while (cur != NULL)
    {
        struct SetElement *element = (struct SetElement *)cur->cur;
        struct Expression *value_expr = NULL;
        if (element != NULL)
        {
            value_expr = element->lower;
            element->lower = NULL;
            element->upper = NULL;
        }

        ListNode_t *node = CreateListNode(value_expr, LIST_EXPR);
        if (array_list == NULL)
        {
            array_list = node;
            array_tail = node;
        }
        else
        {
            array_tail->next = node;
            array_tail = node;
        }
        ++count;

        if (element != NULL)
            destroy_set_element(element);
        ListNode_t *next = cur->next;
        free(cur);
        cur = next;
    }

    expr->expr_data.set_data.elements = NULL;
    expr->type = EXPR_ARRAY_LITERAL;
    expr->expr_data.array_literal_data.elements = array_list;
    expr->expr_data.array_literal_data.element_count = count;
    expr->expr_data.array_literal_data.elements_semchecked = 0;
    expr->is_array_expr = 1;
    expr->array_is_dynamic = 1;
    expr->array_lower_bound = 0;
    expr->array_upper_bound = count - 1;
    expr->array_element_type = UNKNOWN_TYPE;
    if (expr->array_element_type_id != NULL)
    {
        free(expr->array_element_type_id);
        expr->array_element_type_id = NULL;
    }
    expr->array_element_size = 0;
    expr->array_element_record_type = NULL;
    expr->resolved_type = UNKNOWN_TYPE;
    return 0;
}

static int semcheck_typecheck_array_literal(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, int expected_type, const char *expected_type_id, int line_num)
{
    if (expr == NULL || expr->type != EXPR_ARRAY_LITERAL)
        return 0;

    if (expected_type == ARRAY_OF_CONST_TYPE)
    {
        ListNode_t *cur_elem = expr->expr_data.array_literal_data.elements;
        while (cur_elem != NULL)
        {
            struct Expression *element_expr = (struct Expression *)cur_elem->cur;
            int element_type = UNKNOWN_TYPE;
            semcheck_expr_main(&element_type, symtab, element_expr, max_scope_lev, NO_MUTATE);
            cur_elem = cur_elem->next;
        }
        expr->array_element_type = ARRAY_OF_CONST_TYPE;
        expr->array_element_size = (int)sizeof(kgpc_tvarrec);
        expr->array_lower_bound = 0;
        expr->array_upper_bound = expr->expr_data.array_literal_data.element_count - 1;
        expr->is_array_expr = 1;
        expr->array_is_dynamic = 1;
        expr->expr_data.array_literal_data.elements_semchecked = 1;
        return 0;
    }

    int error_count = 0;
    ListNode_t *cur = expr->expr_data.array_literal_data.elements;
    int index = 0;
    while (cur != NULL)
    {
        struct Expression *element_expr = (struct Expression *)cur->cur;
        if (element_expr == NULL)
        {
            ++index;
            cur = cur->next;
            continue;
        }

        int element_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_main(&element_type, symtab, element_expr,
            max_scope_lev, NO_MUTATE);

        if (error_count == 0 && expected_type != UNKNOWN_TYPE &&
            element_type != expected_type)
        {
            int compatible = 0;
            if ((expected_type == LONGINT_TYPE && element_type == INT_TYPE) ||
                (expected_type == INT_TYPE && element_type == LONGINT_TYPE))
                compatible = 1;
            else if (expected_type == STRING_TYPE && element_type == CHAR_TYPE)
                compatible = 1;

            if (!compatible)
            {
                fprintf(stderr, "Error on line %d, element %d of array literal "
                    "does not match expected type.\n", line_num, index);
                ++error_count;
            }
        }

        ++index;
        cur = cur->next;
    }

    if (error_count != 0)
        return error_count;

    if (expr->array_element_size <= 0)
    {
        long long element_size = 0;
        if (sizeof_from_type_ref(symtab, expected_type, expected_type_id,
                &element_size, 0, line_num) == 0 && element_size > 0 &&
            element_size <= INT_MAX)
        {
            expr->array_element_size = (int)element_size;
        }
    }

    expr->array_lower_bound = 0;
    expr->array_upper_bound = expr->expr_data.array_literal_data.element_count - 1;
    expr->is_array_expr = 1;
    expr->array_is_dynamic = 1;
    expr->expr_data.array_literal_data.elements_semchecked = 1;
    return 0;
}

int semcheck_prepare_array_literal_argument(Tree_t *formal_decl, struct Expression *arg_expr,
    SymTab_t *symtab, int max_scope_lev, int line_num)
{
    if (formal_decl == NULL || formal_decl->type != TREE_ARR_DECL || arg_expr == NULL)
        return 0;

    if (arg_expr->type == EXPR_SET)
    {
        if (semcheck_convert_set_literal_to_array_literal(arg_expr) != 0)
        {
            fprintf(stderr, "Error on line %d, open array literal cannot contain ranges.\n",
                line_num);
            return 1;
        }
    }

    if (arg_expr->type != EXPR_ARRAY_LITERAL)
        return 0;

    int expected_type = formal_decl->tree_data.arr_decl_data.type;
    const char *expected_type_id = formal_decl->tree_data.arr_decl_data.type_id;

    if (expected_type == ARRAY_OF_CONST_TYPE)
    {
        ListNode_t *cur_elem = arg_expr->expr_data.array_literal_data.elements;
        while (cur_elem != NULL)
        {
            struct Expression *element_expr = (struct Expression *)cur_elem->cur;
            int element_type = UNKNOWN_TYPE;
            semcheck_expr_main(&element_type, symtab, element_expr,
                max_scope_lev, NO_MUTATE);
            cur_elem = cur_elem->next;
        }
        if (arg_expr->array_element_type_id != NULL)
        {
            free(arg_expr->array_element_type_id);
            arg_expr->array_element_type_id = NULL;
        }
        arg_expr->array_element_type = ARRAY_OF_CONST_TYPE;
        arg_expr->array_element_size = (int)sizeof(kgpc_tvarrec);
        arg_expr->array_lower_bound = 0;
        arg_expr->array_upper_bound = arg_expr->expr_data.array_literal_data.element_count - 1;
        arg_expr->is_array_expr = 1;
        arg_expr->array_is_dynamic = 1;
        arg_expr->expr_data.array_literal_data.elements_semchecked = 1;
        return 0;
    }

    if (arg_expr->array_element_type_id != NULL)
    {
        free(arg_expr->array_element_type_id);
        arg_expr->array_element_type_id = NULL;
    }
    arg_expr->array_element_type = expected_type;
    if (expected_type_id != NULL)
        arg_expr->array_element_type_id = strdup(expected_type_id);
    arg_expr->array_element_record_type = NULL;
    arg_expr->array_element_size = 0;
    arg_expr->array_lower_bound = 0;
    arg_expr->array_upper_bound = arg_expr->expr_data.array_literal_data.element_count - 1;
    arg_expr->is_array_expr = 1;
    arg_expr->array_is_dynamic = 1;

    return semcheck_typecheck_array_literal(arg_expr, symtab, max_scope_lev,
        expected_type, expected_type_id, line_num);
}

static struct RecordType *semcheck_record_type_from_decl(Tree_t *decl, SymTab_t *symtab)
{
    if (decl == NULL || symtab == NULL)
        return NULL;

    if (decl->type == TREE_VAR_DECL)
    {
        if (decl->tree_data.var_decl_data.inline_record_type != NULL)
            return decl->tree_data.var_decl_data.inline_record_type;
    }

    {
        int owns_type = 0;
        KgpcType *resolved = resolve_type_from_vardecl(decl, symtab, &owns_type);
        if (resolved != NULL)
        {
            struct RecordType *record_type = NULL;
            if (kgpc_type_is_record(resolved))
                record_type = kgpc_type_get_record(resolved);
            else if (kgpc_type_is_pointer(resolved) && resolved->info.points_to != NULL &&
                kgpc_type_is_record(resolved->info.points_to))
                record_type = kgpc_type_get_record(resolved->info.points_to);

            if (owns_type)
                destroy_kgpc_type(resolved);

            if (record_type != NULL)
                return record_type;
        }
    }

    if (decl->type == TREE_VAR_DECL)
    {
        if (decl->tree_data.var_decl_data.cached_kgpc_type != NULL)
        {
            KgpcType *cached = decl->tree_data.var_decl_data.cached_kgpc_type;
            if (kgpc_type_is_record(cached))
                return kgpc_type_get_record(cached);
            if (kgpc_type_is_pointer(cached))
            {
                KgpcType *pointee = cached->info.points_to;
                if (pointee != NULL && kgpc_type_is_record(pointee))
                    return kgpc_type_get_record(pointee);
            }
        }

        if (decl->tree_data.var_decl_data.type_id != NULL)
            return semcheck_lookup_record_type(symtab, decl->tree_data.var_decl_data.type_id);
    }

    return NULL;
}

static KgpcType *semcheck_field_expected_kgpc_type(SymTab_t *symtab, struct RecordField *field)
{
    if (symtab == NULL || field == NULL)
        return NULL;

    if (field->is_array)
    {
        KgpcType *element_type = NULL;
        if (field->array_element_type == RECORD_TYPE && field->nested_record != NULL)
        {
            element_type = create_record_type(field->nested_record);
        }
        else if (field->array_element_type_id != NULL)
        {
            HashNode_t *type_node = NULL;
            type_node = semcheck_find_preferred_type_node(symtab, field->array_element_type_id);
            if (type_node != NULL)
            {
                if (type_node->type != NULL)
                {
                    kgpc_type_retain(type_node->type);
                    element_type = type_node->type;
                }
                else
                {
                    struct TypeAlias *alias = hashnode_get_type_alias(type_node);
                    if (alias != NULL)
                        element_type = create_kgpc_type_from_type_alias(alias, symtab);
                }
            }
        }
        if (element_type == NULL && field->array_element_type != UNKNOWN_TYPE)
            element_type = create_primitive_type(field->array_element_type);
        if (element_type == NULL)
            return NULL;
        return create_array_type(element_type, field->array_start, field->array_end);
    }

    if (field->nested_record != NULL)
        return create_record_type(field->nested_record);

    if (field->type_id != NULL)
    {
        HashNode_t *type_node = NULL;
        type_node = semcheck_find_preferred_type_node(symtab, field->type_id);
        if (type_node != NULL)
        {
            if (type_node->type != NULL)
            {
                kgpc_type_retain(type_node->type);
                return type_node->type;
            }
            else
            {
                struct TypeAlias *alias = hashnode_get_type_alias(type_node);
                if (alias != NULL)
                    return create_kgpc_type_from_type_alias(alias, symtab);
            }
        }
    }

    if (field->type != UNKNOWN_TYPE)
        return create_primitive_type(field->type);

    return NULL;
}

static int semcheck_typecheck_record_constructor(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, struct RecordType *record_type, int line_num)
{
    if (expr == NULL || symtab == NULL)
        return 0;

    if (record_type == NULL)
    {
        fprintf(stderr, "Error on line %d, unable to infer record type for constructor.\n",
            line_num);
        expr->resolved_type = UNKNOWN_TYPE;
        return 1;
    }

    expr->record_type = record_type;
    expr->resolved_type = RECORD_TYPE;
    if (expr->resolved_kgpc_type != NULL)
        destroy_kgpc_type(expr->resolved_kgpc_type);
    expr->resolved_kgpc_type = create_record_type(record_type);

    int error_count = 0;
    ListNode_t *cur = expr->expr_data.record_constructor_data.fields;
    while (cur != NULL)
    {
        struct RecordConstructorField *field = (struct RecordConstructorField *)cur->cur;
        if (field == NULL || field->field_id == NULL || field->value == NULL)
        {
            ++error_count;
            cur = cur->next;
            continue;
        }

        for (ListNode_t *prev = expr->expr_data.record_constructor_data.fields;
             prev != cur; prev = prev->next)
        {
            struct RecordConstructorField *prior = (struct RecordConstructorField *)prev->cur;
            if (prior != NULL && prior->field_id != NULL &&
                pascal_identifier_equals(prior->field_id, field->field_id))
            {
                fprintf(stderr, "Error on line %d, duplicate record constructor field %s.\n",
                    line_num, field->field_id);
                ++error_count;
                break;
            }
        }

        struct RecordField *field_desc = NULL;
        long long field_offset = 0;
        if (resolve_record_field(symtab, record_type, field->field_id,
                &field_desc, &field_offset, line_num, 0) != 0 || field_desc == NULL)
        {
            ++error_count;
            cur = cur->next;
            continue;
        }

        field->field_offset = field_offset;
        field->field_type = field_desc->type;
        field->field_record_type = field_desc->nested_record;
        if (field_desc->nested_record != NULL)
            field->field_type = RECORD_TYPE;
        if (field_desc->nested_record == NULL && field_desc->type_id != NULL)
        {
            struct RecordType *resolved_record = semcheck_lookup_record_type(symtab, field_desc->type_id);
            if (resolved_record != NULL)
            {
                field->field_record_type = resolved_record;
                field->field_type = RECORD_TYPE;
            }
        }

        if (field_desc->type_id != NULL)
        {
            free(field->field_type_id);
            field->field_type_id = strdup(field_desc->type_id);
        }

        field->field_is_array = field_desc->is_array;
        field->array_start = field_desc->array_start;
        field->array_end = field_desc->array_end;
        field->array_is_open = field_desc->array_is_open;
        field->array_element_type = field_desc->array_element_type;
        field->array_element_record_type = NULL;
        if (field_desc->array_element_type_id != NULL)
        {
            free(field->array_element_type_id);
            field->array_element_type_id = strdup(field_desc->array_element_type_id);
        }
        if (field_desc->array_element_type == RECORD_TYPE && field_desc->nested_record != NULL)
            field->array_element_record_type = field_desc->nested_record;
        if (field->array_element_record_type == NULL && field_desc->array_element_type_id != NULL)
        {
            struct RecordType *resolved_elem = semcheck_lookup_record_type(symtab, field_desc->array_element_type_id);
            if (resolved_elem != NULL)
            {
                field->array_element_record_type = resolved_elem;
                field->array_element_type = RECORD_TYPE;
            }
        }

        if (!field->field_is_array && field_desc->type_id != NULL)
        {
            HashNode_t *alias_node = NULL;
            if (FindIdent(&alias_node, symtab, field_desc->type_id) != -1 && alias_node != NULL)
            {
                struct TypeAlias *alias = get_type_alias_from_node(alias_node);
                if (alias != NULL && alias->is_array)
                {
                    field->field_is_array = 1;
                    field->array_start = alias->array_start;
                    field->array_end = alias->array_end;
                    field->array_is_open = alias->is_open_array;
                    field->array_element_type = alias->array_element_type;
                    if (alias->array_element_type_id != NULL)
                    {
                        free(field->array_element_type_id);
                        field->array_element_type_id = strdup(alias->array_element_type_id);
                    }
                }
            }
        }

        if (field->value->type == EXPR_RECORD_CONSTRUCTOR && field->value->record_type == NULL)
        {
            if (field->field_type == RECORD_TYPE)
                field->value->record_type = field->field_record_type;
            else if (field->field_is_array && field->array_element_type == RECORD_TYPE)
                field->value->record_type = field->array_element_record_type;
        }

        if (field->field_is_array && field->value != NULL)
        {
            if (field->value->type == EXPR_SET)
            {
                if (semcheck_convert_set_literal_to_array_literal(field->value) != 0)
                {
                    fprintf(stderr, "Error on line %d, array field %s cannot use set ranges.\n",
                        line_num, field->field_id);
                    ++error_count;
                }
            }

            if (field->value->type == EXPR_ARRAY_LITERAL)
            {
                if (field->value->array_element_type == UNKNOWN_TYPE)
                    field->value->array_element_type = field->array_element_type;
                if (field->value->array_element_type_id == NULL &&
                    field->array_element_type_id != NULL)
                    field->value->array_element_type_id = strdup(field->array_element_type_id);

                int arr_err = semcheck_typecheck_array_literal(field->value, symtab, max_scope_lev,
                    field->array_element_type, field->array_element_type_id, line_num);
                if (arr_err != 0)
                {
                    error_count += arr_err;
                }
                else if (!field->array_is_open)
                {
                    int expected_count = field->array_end - field->array_start + 1;
                    int actual_count = field->value->expr_data.array_literal_data.element_count;
                    if (expected_count != actual_count)
                    {
                        fprintf(stderr,
                            "Error on line %d, array field %s expects %d elements, got %d.\n",
                            line_num, field->field_id, expected_count, actual_count);
                        ++error_count;
                    }
                    else
                    {
                        field->value->array_lower_bound = field->array_start;
                        field->value->array_upper_bound = field->array_end;
                        field->value->array_is_dynamic = 0;
                        field->value->is_array_expr = 1;
                    }
                }
            }
        }

        int value_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_main(&value_type, symtab, field->value, max_scope_lev, NO_MUTATE);

        int expected_owned = 1;
        KgpcType *expected_type = semcheck_field_expected_kgpc_type(symtab, field_desc);
        if (expected_type == NULL)
        {
            fprintf(stderr, "Error on line %d, unable to resolve type for field %s.\n",
                line_num, field->field_id);
            ++error_count;
        }
        else
        {
            int value_owned = 0;
            KgpcType *value_type_kgpc = semcheck_resolve_expression_kgpc_type(
                symtab, field->value, max_scope_lev, NO_MUTATE, &value_owned);
            if (value_type_kgpc == NULL)
            {
                fprintf(stderr, "Error on line %d, unable to resolve type for field %s.\n",
                    line_num, field->field_id);
                ++error_count;
            }
            else if (!are_types_compatible_for_assignment(expected_type, value_type_kgpc, symtab))
            {
                fprintf(stderr,
                    "Error on line %d, incompatible types in record constructor for %s "
                    "(expected %s, got %s).\n",
                    line_num, field->field_id,
                    kgpc_type_to_string(expected_type),
                    kgpc_type_to_string(value_type_kgpc));
                ++error_count;
            }
            if (value_owned && value_type_kgpc != NULL)
                destroy_kgpc_type(value_type_kgpc);
        }
        if (expected_owned && expected_type != NULL)
            destroy_kgpc_type(expected_type);

        cur = cur->next;
    }

    expr->expr_data.record_constructor_data.fields_semchecked = 1;
    return error_count;
}

int semcheck_prepare_record_constructor_argument(Tree_t *formal_decl, struct Expression *arg_expr,
    SymTab_t *symtab, int max_scope_lev, int line_num)
{
    if (formal_decl == NULL || arg_expr == NULL || symtab == NULL)
        return 0;

    if (arg_expr->type != EXPR_RECORD_CONSTRUCTOR)
        return 0;

    struct RecordType *record_type = semcheck_record_type_from_decl(formal_decl, symtab);
    if (record_type == NULL)
    {
        fprintf(stderr, "Error on line %d, unable to infer record type for constructor.\n",
            line_num);
        return 1;
    }

    return semcheck_typecheck_record_constructor(arg_expr, symtab, max_scope_lev,
        record_type, line_num);
}

static int ensure_with_capacity(void)
{
    if (with_context_count < with_context_capacity)
        return 0;

    size_t new_capacity = with_context_capacity == 0 ? 8 : with_context_capacity * 2;
    WithContextEntry *new_stack = realloc(with_context_stack,
        new_capacity * sizeof(*with_context_stack));
    if (new_stack == NULL)
        return 1;

    with_context_stack = new_stack;
    with_context_capacity = new_capacity;
    return 0;
}

struct Expression *clone_expression(const struct Expression *expr)
{
    if (expr == NULL)
        return NULL;

    struct Expression *clone = (struct Expression *)calloc(1, sizeof(struct Expression));
    if (clone == NULL)
        return NULL;

    clone->line_num = expr->line_num;
    clone->type = expr->type;
    clone->resolved_type = expr->resolved_type;
    clone->pointer_subtype = expr->pointer_subtype;
    clone->record_type = expr->record_type;
    if (expr->pointer_subtype_id != NULL)
    {
        clone->pointer_subtype_id = strdup(expr->pointer_subtype_id);
        if (clone->pointer_subtype_id == NULL)
        {
            free(clone);
            return NULL;
        }
    }

    clone->is_array_expr = expr->is_array_expr;
    clone->array_element_type = expr->array_element_type;
    clone->array_lower_bound = expr->array_lower_bound;
    clone->array_upper_bound = expr->array_upper_bound;
    clone->array_element_size = expr->array_element_size;
    clone->array_is_dynamic = expr->array_is_dynamic;
    clone->array_element_record_type = expr->array_element_record_type;
    if (expr->array_element_type_id != NULL)
    {
        clone->array_element_type_id = strdup(expr->array_element_type_id);
        if (clone->array_element_type_id == NULL)
        {
            destroy_expr(clone);
            return NULL;
        }
    }

    if (expr->field_width != NULL)
        clone->field_width = clone_expression(expr->field_width);
    if (expr->field_precision != NULL)
        clone->field_precision = clone_expression(expr->field_precision);

    switch (expr->type)
    {
        case EXPR_VAR_ID:
            clone->expr_data.id = expr->expr_data.id != NULL ? strdup(expr->expr_data.id) : NULL;
            if (expr->expr_data.id != NULL && clone->expr_data.id == NULL)
            {
                destroy_expr(clone);
                return NULL;
            }
            break;
        case EXPR_RECORD_ACCESS:
            clone->expr_data.record_access_data.record_expr =
                clone_expression(expr->expr_data.record_access_data.record_expr);
            clone->expr_data.record_access_data.field_id =
                expr->expr_data.record_access_data.field_id != NULL ?
                    strdup(expr->expr_data.record_access_data.field_id) : NULL;
            clone->expr_data.record_access_data.field_offset =
                expr->expr_data.record_access_data.field_offset;
            if (expr->expr_data.record_access_data.field_id != NULL &&
                clone->expr_data.record_access_data.field_id == NULL)
            {
                destroy_expr(clone);
                return NULL;
            }
            break;
        case EXPR_POINTER_DEREF:
            clone->expr_data.pointer_deref_data.pointer_expr =
                clone_expression(expr->expr_data.pointer_deref_data.pointer_expr);
            break;
        case EXPR_ADDR:
            clone->expr_data.addr_data.expr =
                clone_expression(expr->expr_data.addr_data.expr);
            break;
        case EXPR_SIGN_TERM:
            clone->expr_data.sign_term = clone_expression(expr->expr_data.sign_term);
            break;
        case EXPR_ADDOP:
            clone->expr_data.addop_data.addop_type = expr->expr_data.addop_data.addop_type;
            clone->expr_data.addop_data.left_expr =
                clone_expression(expr->expr_data.addop_data.left_expr);
            clone->expr_data.addop_data.right_term =
                clone_expression(expr->expr_data.addop_data.right_term);
            break;
        case EXPR_MULOP:
            clone->expr_data.mulop_data.mulop_type = expr->expr_data.mulop_data.mulop_type;
            clone->expr_data.mulop_data.left_term =
                clone_expression(expr->expr_data.mulop_data.left_term);
            clone->expr_data.mulop_data.right_factor =
                clone_expression(expr->expr_data.mulop_data.right_factor);
            break;
        case EXPR_ARRAY_ACCESS:
            clone->expr_data.array_access_data.array_expr =
                clone_expression(expr->expr_data.array_access_data.array_expr);
            clone->expr_data.array_access_data.index_expr =
                clone_expression(expr->expr_data.array_access_data.index_expr);
            break;
        case EXPR_TYPECAST:
            clone->expr_data.typecast_data.target_type = expr->expr_data.typecast_data.target_type;
            clone->expr_data.typecast_data.expr =
                clone_expression(expr->expr_data.typecast_data.expr);
            if (expr->expr_data.typecast_data.target_type_id != NULL)
            {
                clone->expr_data.typecast_data.target_type_id =
                    strdup(expr->expr_data.typecast_data.target_type_id);
                if (clone->expr_data.typecast_data.target_type_id == NULL)
                {
                    destroy_expr(clone);
                    return NULL;
                }
            }
            break;
        case EXPR_RELOP:
            clone->expr_data.relop_data.type = expr->expr_data.relop_data.type;
            clone->expr_data.relop_data.left =
                clone_expression(expr->expr_data.relop_data.left);
            clone->expr_data.relop_data.right =
                clone_expression(expr->expr_data.relop_data.right);
            break;
        case EXPR_INUM:
            clone->expr_data.i_num = expr->expr_data.i_num;
            break;
        case EXPR_RNUM:
            clone->expr_data.r_num = expr->expr_data.r_num;
            break;
        case EXPR_STRING:
            clone->expr_data.string = expr->expr_data.string != NULL ?
                strdup(expr->expr_data.string) : NULL;
            if (expr->expr_data.string != NULL && clone->expr_data.string == NULL)
            {
                destroy_expr(clone);
                return NULL;
            }
            break;
        case EXPR_CHAR_CODE:
            clone->expr_data.char_code = expr->expr_data.char_code;
            break;
        case EXPR_BOOL:
            clone->expr_data.bool_value = expr->expr_data.bool_value;
            break;
        case EXPR_NIL:
            break;
        case EXPR_RECORD_CONSTRUCTOR:
        {
            clone->expr_data.record_constructor_data.field_count =
                expr->expr_data.record_constructor_data.field_count;
            clone->expr_data.record_constructor_data.fields_semchecked =
                expr->expr_data.record_constructor_data.fields_semchecked;

            ListNode_t *field_head = NULL;
            ListNode_t *field_tail = NULL;
            ListNode_t *cur = expr->expr_data.record_constructor_data.fields;
            while (cur != NULL)
            {
                struct RecordConstructorField *field =
                    (struct RecordConstructorField *)cur->cur;
                struct RecordConstructorField *field_clone = NULL;
                if (field != NULL)
                {
                    field_clone = (struct RecordConstructorField *)calloc(1, sizeof(*field_clone));
                    if (field_clone == NULL)
                    {
                        destroy_expr(clone);
                        return NULL;
                    }
                    field_clone->field_id = field->field_id != NULL ? strdup(field->field_id) : NULL;
                    field_clone->value = clone_expression(field->value);
                    field_clone->field_offset = field->field_offset;
                    field_clone->field_type = field->field_type;
                    field_clone->field_type_id = field->field_type_id != NULL ?
                        strdup(field->field_type_id) : NULL;
                    field_clone->field_record_type = field->field_record_type;
                    field_clone->field_is_array = field->field_is_array;
                    field_clone->array_start = field->array_start;
                    field_clone->array_end = field->array_end;
                    field_clone->array_element_type = field->array_element_type;
                    field_clone->array_element_type_id = field->array_element_type_id != NULL ?
                        strdup(field->array_element_type_id) : NULL;
                    field_clone->array_is_open = field->array_is_open;
                    field_clone->array_element_record_type = field->array_element_record_type;
                    if ((field->field_id != NULL && field_clone->field_id == NULL) ||
                        (field->field_type_id != NULL && field_clone->field_type_id == NULL) ||
                        (field->array_element_type_id != NULL &&
                         field_clone->array_element_type_id == NULL) ||
                        (field->value != NULL && field_clone->value == NULL))
                    {
                        destroy_expr(clone);
                        return NULL;
                    }
                }

                ListNode_t *node = CreateListNode(field_clone, LIST_UNSPECIFIED);
                if (node == NULL)
                {
                    if (field_clone != NULL)
                    {
                        if (field_clone->value != NULL)
                            destroy_expr(field_clone->value);
                        free(field_clone->field_id);
                        free(field_clone->field_type_id);
                        free(field_clone->array_element_type_id);
                        free(field_clone);
                    }
                    destroy_expr(clone);
                    return NULL;
                }
                if (field_head == NULL)
                {
                    field_head = node;
                    field_tail = node;
                }
                else
                {
                    field_tail->next = node;
                    field_tail = node;
                }
                cur = cur->next;
            }
            clone->expr_data.record_constructor_data.fields = field_head;
            break;
        }
        default:
            destroy_expr(clone);
            return NULL;
    }

    return clone;
}

int semcheck_with_push(struct Expression *context_expr, struct RecordType *record_type)
{
    if (context_expr == NULL || record_type == NULL)
        return 1;
    if (ensure_with_capacity() != 0)
    {
        fprintf(stderr,
            "Error: failed to expand WITH context stack for semantic analysis.\n");
        return -1;
    }

    with_context_stack[with_context_count].context_expr = context_expr;
    with_context_stack[with_context_count].record_type = record_type;
    ++with_context_count;
    return 0;
}

void semcheck_with_pop(void)
{
    if (with_context_count > 0)
        --with_context_count;
}

static struct RecordType *resolve_record_type_for_with(SymTab_t *symtab,
    struct Expression *context_expr, int expr_type, int line_num)
{
    (void)line_num;
    if (context_expr == NULL)
        return NULL;

    if (expr_type == RECORD_TYPE)
        return context_expr->record_type;

    if (expr_type == POINTER_TYPE)
    {
        struct RecordType *record_info = context_expr->record_type;
        if (record_info == NULL && context_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, context_expr->pointer_subtype_id) != -1 &&
                target_node != NULL)
                record_info = get_record_type_from_node(target_node);
        }
        return record_info;
    }

    return NULL;
}

struct RecordType *semcheck_with_resolve_record_type(SymTab_t *symtab,
    struct Expression *context_expr, int expr_type, int line_num)
{
    return resolve_record_type_for_with(symtab, context_expr, expr_type, line_num);
}

int semcheck_with_try_resolve(const char *field_id, SymTab_t *symtab,
    struct Expression **out_record_expr, int line_num)
{
    if (field_id == NULL || out_record_expr == NULL)
        return 1;

    for (size_t index = with_context_count; index > 0; --index)
    {
        WithContextEntry *entry = &with_context_stack[index - 1];
        if (entry->record_type == NULL)
            continue;

        struct RecordField *field_desc = NULL;
        long long offset = 0;
        if (resolve_record_field(symtab, entry->record_type, field_id,
                &field_desc, &offset, line_num, 1) == 0 && field_desc != NULL)
        {
            struct Expression *clone = clone_expression(entry->context_expr);
            if (clone == NULL)
                return -1;
            *out_record_expr = clone;
            return 0;
        }
    }

    return 1;
}

int resolve_record_field(SymTab_t *symtab, struct RecordType *record,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int line_num, int silent)
{
    if (record == NULL || field_name == NULL)
        return 1;

    long long offset = 0;
    int found = 0;
    if (find_field_in_members(symtab, record->fields, field_name, out_field,
            &offset, 0, line_num, &found) != 0)
        return 1;

    if (!found)
    {
        if (!silent)
            fprintf(stderr, "Error on line %d, record field %s not found.\n", line_num, field_name);
        return 1;
    }

    if (offset_out != NULL)
        *offset_out = offset;
    return 0;
}
static int semcheck_builtin_chr(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Chr expects exactly one argument.\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr,
        max_scope_lev, NO_MUTATE);
    if (error_count == 0 && arg_type != INT_TYPE && arg_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Chr expects an integer argument.\\n",
            expr->line_num);
        error_count++;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }

        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_chr");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Chr.\\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

        semcheck_reset_function_call_cache(expr);
        *type_return = CHAR_TYPE;
        expr->resolved_type = CHAR_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_ord(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Ord expects exactly one argument.\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr,
        max_scope_lev, NO_MUTATE);
    if (error_count != 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    const char *mangled_name = NULL;
    if (arg_type == STRING_TYPE)
    {
        if (arg_expr != NULL && arg_expr->type == EXPR_STRING)
        {
            char *literal = arg_expr->expr_data.string;
            if (literal == NULL || literal[0] == '\0')
            {
                fprintf(stderr, "Error on line %d, Ord expects a non-empty character literal.\\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            if (literal[1] != '\0')
            {
                fprintf(stderr, "Error on line %d, Ord expects a single character literal.\\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }

            unsigned char ordinal_value = (unsigned char)literal[0];

            destroy_list(expr->expr_data.function_call_data.args_expr);
            expr->expr_data.function_call_data.args_expr = NULL;
            if (expr->expr_data.function_call_data.id != NULL)
            {
                free(expr->expr_data.function_call_data.id);
                expr->expr_data.function_call_data.id = NULL;
            }
            if (expr->expr_data.function_call_data.mangled_id != NULL)
            {
                free(expr->expr_data.function_call_data.mangled_id);
                expr->expr_data.function_call_data.mangled_id = NULL;
            }
            semcheck_reset_function_call_cache(expr);

            expr->type = EXPR_INUM;
            expr->expr_data.i_num = ordinal_value;
            expr->resolved_type = LONGINT_TYPE;
            *type_return = LONGINT_TYPE;
            return 0;
        }

        mangled_name = "kgpc_ord_string";
    }
    else if (arg_type == BOOL)
    {
        /* Constant fold boolean literals */
        if (arg_expr != NULL && arg_expr->type == EXPR_BOOL)
        {
            int ordinal_value = arg_expr->expr_data.bool_value ? 1 : 0;

            destroy_list(expr->expr_data.function_call_data.args_expr);
            expr->expr_data.function_call_data.args_expr = NULL;
            if (expr->expr_data.function_call_data.id != NULL)
            {
                free(expr->expr_data.function_call_data.id);
                expr->expr_data.function_call_data.id = NULL;
            }
            if (expr->expr_data.function_call_data.mangled_id != NULL)
            {
                free(expr->expr_data.function_call_data.mangled_id);
                expr->expr_data.function_call_data.mangled_id = NULL;
            }
            semcheck_reset_function_call_cache(expr);

            expr->type = EXPR_INUM;
            expr->expr_data.i_num = ordinal_value;
            expr->resolved_type = LONGINT_TYPE;
            *type_return = LONGINT_TYPE;
            return 0;
        }

        mangled_name = "kgpc_ord_longint";
    }
    else if (arg_type == INT_TYPE || arg_type == LONGINT_TYPE)
    {
        mangled_name = "kgpc_ord_longint";
    }
    else if (arg_type == CHAR_TYPE)
    {
        /* For char variables, Ord returns the character code */
        mangled_name = "kgpc_ord_longint";
    }
    else if (arg_type == ENUM_TYPE)
    {
        /* For enumerative types, Ord returns the ordinal value (0-based index) */
        mangled_name = "kgpc_ord_longint";
    }

    if (mangled_name != NULL)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }

        expr->expr_data.function_call_data.mangled_id = strdup(mangled_name);
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Ord.\\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

        semcheck_reset_function_call_cache(expr);
        *type_return = LONGINT_TYPE;
        expr->resolved_type = LONGINT_TYPE;
        return 0;
    }

    fprintf(stderr, "Error on line %d, Ord expects an integer, character, or boolean argument.\\n",
        expr->line_num);
    *type_return = UNKNOWN_TYPE;
    return 1;
}

static int semcheck_builtin_length(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL)
    {
        if (getenv("KGPC_DEBUG_LENGTH_ARGS") != NULL)
            fprintf(stderr, "[KGPC] Length args count=0\n");
        fprintf(stderr, "Error on line %d, Length expects exactly one argument.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }
    if (args->next != NULL)
    {
        if (getenv("KGPC_DEBUG_LENGTH_ARGS") != NULL)
        {
            fprintf(stderr, "[KGPC] Length args count=%d\n", ListLength(args));
            for (ListNode_t *cur = args; cur != NULL; cur = cur->next)
            {
                struct Expression *arg_expr = (struct Expression *)cur->cur;
                if (arg_expr == NULL) {
                    fprintf(stderr, "[KGPC]   arg: <null>\n");
                } else if (arg_expr->type == EXPR_VAR_ID) {
                    fprintf(stderr, "[KGPC]   arg: VAR_ID %s\n",
                        arg_expr->expr_data.id != NULL ? arg_expr->expr_data.id : "<null>");
                } else if (arg_expr->type == EXPR_RECORD_ACCESS) {
                    fprintf(stderr, "[KGPC]   arg: RECORD_ACCESS %s\n",
                        arg_expr->expr_data.record_access_data.field_id != NULL ?
                            arg_expr->expr_data.record_access_data.field_id : "<null>");
                } else {
                    fprintf(stderr, "[KGPC]   arg: type=%d\n", arg_expr->type);
                }
            }
        }

        struct Expression *first_arg = (struct Expression *)args->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
            first_arg->expr_data.id != NULL &&
            pascal_identifier_equals(first_arg->expr_data.id, "Self"))
        {
            args = args->next;
            expr->expr_data.function_call_data.args_expr = args;
        }
        if (args == NULL || args->next != NULL)
        {
            fprintf(stderr, "Error on line %d, Length expects exactly one argument.\n", expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);


  int is_dynamic_array = (arg_expr != NULL && arg_expr->is_array_expr && arg_expr->array_is_dynamic);
  int is_static_array = (arg_expr != NULL && arg_expr->is_array_expr && !arg_expr->array_is_dynamic);

    /* For static arrays, convert Length() to a compile-time constant */
    if (error_count == 0 && is_static_array)
    {
        long long length = arg_expr->array_upper_bound - arg_expr->array_lower_bound + 1;
        
        /* Convert this function call into an integer literal expression */
        semcheck_reset_function_call_cache(expr);
        expr->type = EXPR_INUM;
        expr->expr_data.i_num = length;
        expr->resolved_type = LONGINT_TYPE;
        *type_return = LONGINT_TYPE;
        return 0;
    }


    const char *mangled_name = NULL;
    if (error_count == 0 && is_string_type(arg_type))
        mangled_name = "kgpc_string_length";
    else if (error_count == 0 && is_dynamic_array)
        mangled_name = "__kgpc_dynarray_length";
    else if (error_count == 0 && is_static_array)
    {
        long long lower_bound = arg_expr->array_lower_bound;
        long long upper_bound = arg_expr->array_upper_bound;
        long long length_value = 0;
        if (upper_bound >= lower_bound)
            length_value = (upper_bound - lower_bound) + 1;

        destroy_list(expr->expr_data.function_call_data.args_expr);
        expr->expr_data.function_call_data.args_expr = NULL;
        if (expr->expr_data.function_call_data.id != NULL)
        {
            free(expr->expr_data.function_call_data.id);
            expr->expr_data.function_call_data.id = NULL;
        }
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        semcheck_reset_function_call_cache(expr);
        expr->type = EXPR_INUM;
        expr->expr_data.i_num = length_value;
        expr->resolved_type = LONGINT_TYPE;
        *type_return = LONGINT_TYPE;
        return 0;
    }
    else if (error_count == 0)
    {
        fprintf(stderr, "Error on line %d, Length supports string or dynamic array arguments.\n", expr->line_num);
        error_count++;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup(mangled_name);
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Length.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        expr->resolved_type = LONGINT_TYPE;
        *type_return = LONGINT_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_copy(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next == NULL || args->next->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Copy expects exactly three arguments.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *source_expr = (struct Expression *)args->cur;
    struct Expression *index_expr = (struct Expression *)args->next->cur;
    struct Expression *count_expr = (struct Expression *)args->next->next->cur;

    int error_count = 0;
    int source_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&source_type, symtab, source_expr, max_scope_lev, NO_MUTATE);
    if (error_count == 0 && source_type != STRING_TYPE)
    {
        fprintf(stderr, "Error on line %d, Copy expects its first argument to be a string.\n", expr->line_num);
        error_count++;
    }

    int index_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&index_type, symtab, index_expr, max_scope_lev, NO_MUTATE);
    if (error_count == 0 && !is_integer_type(index_type))
    {
        fprintf(stderr, "Error on line %d, Copy index must be an integer.\n", expr->line_num);
        error_count++;
    }

    int count_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&count_type, symtab, count_expr, max_scope_lev, NO_MUTATE);
    if (error_count == 0 && !is_integer_type(count_type))
    {
        fprintf(stderr, "Error on line %d, Copy count must be an integer.\n", expr->line_num);
        error_count++;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_string_copy");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Copy.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        expr->resolved_type = STRING_TYPE;
        *type_return = STRING_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_pos(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Pos expects exactly two arguments.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int error_count = 0;
    struct Expression *substr_expr = (struct Expression *)args->cur;
    struct Expression *value_expr = (struct Expression *)args->next->cur;

    int substr_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&substr_type, symtab, substr_expr, max_scope_lev, NO_MUTATE);
    
    /* Check if substr is a string type, char, or shortstring (array of char) */
    int is_valid_substr = is_string_type(substr_type) || substr_type == CHAR_TYPE ||
                          is_shortstring_array(substr_type, substr_expr->is_array_expr);
    
    if (error_count == 0 && !is_valid_substr)
    {
        fprintf(stderr, "Error on line %d, Pos substring must be a string.\n", expr->line_num);
        ++error_count;
    }

    int value_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);
    
    /* Check if value is a string type OR a shortstring (array of char) */
    int is_valid_value = is_string_type(value_type) ||
                         is_shortstring_array(value_type, value_expr->is_array_expr);
    
    if (error_count == 0 && !is_valid_value)
    {
        fprintf(stderr, "Error on line %d, Pos target must be a string.\n", expr->line_num);
        ++error_count;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_string_pos");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Pos.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        expr->resolved_type = LONGINT_TYPE;
        *type_return = LONGINT_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_strpas(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, StrPas expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int error_count = 0;
    int arg_type = UNKNOWN_TYPE;
    struct Expression *arg_expr = (struct Expression *)args->cur;
    error_count += semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    /* FPC accepts both PChar/PAnsiChar (string-like) pointers */
    if (error_count == 0 &&
        !(arg_type == STRING_TYPE || arg_type == POINTER_TYPE || arg_type == CHAR_TYPE))
    {
        fprintf(stderr, "Error on line %d, StrPas expects a PChar or PAnsiChar argument.\n",
            expr->line_num);
        ++error_count;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_strpas");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for StrPas.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        expr->resolved_type = STRING_TYPE;
        *type_return = STRING_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_eof(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    int error_count = 0;
    const char *mangled_name = NULL;

    if (args == NULL)
    {
        mangled_name = "kgpc_text_eof_default";
    }
    else if (args->next == NULL)
    {
        struct Expression *file_expr = (struct Expression *)args->cur;
        struct Expression *check_expr = file_expr;
        if (file_expr != NULL && file_expr->type == EXPR_ADDR &&
            file_expr->expr_data.addr_data.expr != NULL)
        {
            check_expr = file_expr->expr_data.addr_data.expr;
        }
        int file_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_main(&file_type, symtab, check_expr, max_scope_lev, NO_MUTATE);
        if (file_type != TEXT_TYPE)
        {
            fprintf(stderr, "Error on line %d, EOF expects a text file argument.\n", expr->line_num);
            error_count++;
        }
        else
        {
            mangled_name = "kgpc_text_eof";
            if (file_expr != NULL && file_expr->type != EXPR_ADDR)
            {
                args->cur = mk_addressof(file_expr->line_num, file_expr);
            }
        }
    }
    else
    {
        fprintf(stderr, "Error on line %d, EOF expects zero or one argument.\n", expr->line_num);
        error_count++;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup(mangled_name);
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for EOF.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        expr->resolved_type = BOOL;
        *type_return = BOOL;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_eoln(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    int error_count = 0;
    const char *mangled_name = NULL;

    if (args == NULL)
    {
        mangled_name = "kgpc_text_eoln_default";
    }
    else if (args->next == NULL)
    {
        struct Expression *file_expr = (struct Expression *)args->cur;
        struct Expression *check_expr = file_expr;
        if (file_expr != NULL && file_expr->type == EXPR_ADDR &&
            file_expr->expr_data.addr_data.expr != NULL)
        {
            check_expr = file_expr->expr_data.addr_data.expr;
        }
        int file_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_main(&file_type, symtab, check_expr, max_scope_lev, NO_MUTATE);
        if (file_type != TEXT_TYPE)
        {
            fprintf(stderr, "Error on line %d, EOLN expects a text file argument.\n", expr->line_num);
            error_count++;
        }
        else
        {
            mangled_name = "kgpc_text_eoln";
            if (file_expr != NULL && file_expr->type != EXPR_ADDR)
            {
                args->cur = mk_addressof(file_expr->line_num, file_expr);
            }
        }
    }
    else
    {
        fprintf(stderr, "Error on line %d, EOLN expects zero or one argument.\n", expr->line_num);
        error_count++;
    }

    if (error_count != 0 || mangled_name == NULL)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count != 0 ? error_count : 1;
    }

    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    expr->expr_data.function_call_data.mangled_id = strdup(mangled_name);
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for EOLN.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    expr->resolved_type = BOOL;
    *type_return = BOOL;
    return 0;
}

static void semcheck_free_call_args(ListNode_t *args, struct Expression *preserve_expr)
{
    while (args != NULL)
    {
        ListNode_t *next = args->next;
        if (args->cur != NULL && args->cur != preserve_expr)
            destroy_expr((struct Expression *)args->cur);
        free(args);
        args = next;
    }
}

static void semcheck_replace_call_with_integer_literal(struct Expression *expr, long long value)
{
    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return;

    semcheck_free_call_args(expr->expr_data.function_call_data.args_expr, NULL);
    expr->expr_data.function_call_data.args_expr = NULL;

    if (expr->expr_data.function_call_data.id != NULL)
    {
        free(expr->expr_data.function_call_data.id);
        expr->expr_data.function_call_data.id = NULL;
    }
    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    semcheck_reset_function_call_cache(expr);

    expr->type = EXPR_INUM;
    expr->expr_data.i_num = value;
    expr->resolved_type = LONGINT_TYPE;
}

static int semcheck_prepare_dynarray_high_call(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, struct Expression *array_expr)
{
    if (expr == NULL || array_expr == NULL)
        return 1;

    ListNode_t *old_args = expr->expr_data.function_call_data.args_expr;
    semcheck_free_call_args(old_args, array_expr);
    expr->expr_data.function_call_data.args_expr = NULL;

    struct Expression *lower_expr = mk_inum(expr->line_num, array_expr->array_lower_bound);
    lower_expr->resolved_type = LONGINT_TYPE;

    ListNode_t *new_args = CreateListNode(array_expr, LIST_EXPR);
    new_args = PushListNodeBack(new_args, CreateListNode(lower_expr, LIST_EXPR));
    expr->expr_data.function_call_data.args_expr = new_args;

    /* Change function ID so it won't be processed as built-in High again */
    if (expr->expr_data.function_call_data.id != NULL)
        free(expr->expr_data.function_call_data.id);
    expr->expr_data.function_call_data.id = strdup("kgpc_dynarray_compute_high");
    
    if (expr->expr_data.function_call_data.mangled_id != NULL)
        free(expr->expr_data.function_call_data.mangled_id);
    expr->expr_data.function_call_data.mangled_id = strdup("kgpc_dynarray_compute_high");

    /* Note: We don't call semcheck_reset_function_call_cache here because we want to 
     * mark this as fully resolved (is_call_info_valid=1) rather than reset it */

    int error_count = 0;
    int arg_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&arg_type, symtab, array_expr, max_scope_lev, NO_MUTATE);
    arg_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&arg_type, symtab, lower_expr, max_scope_lev, NO_MUTATE);

    /* Mark as fully resolved internal runtime call */
    expr->resolved_type = LONGINT_TYPE;
    expr->expr_data.function_call_data.is_call_info_valid = 1;
    expr->expr_data.function_call_data.call_hash_type = HASHTYPE_FUNCTION;
    
    if (type_return != NULL)
        *type_return = LONGINT_TYPE;
    return error_count;
}

static int semcheck_builtin_assigned(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Assigned expects exactly one argument.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab,
        (struct Expression *)args->cur, max_scope_lev, NO_MUTATE);

    if (error_count == 0 && arg_type != POINTER_TYPE && arg_type != PROCEDURE)
    {
        fprintf(stderr, "Error on line %d, Assigned expects a pointer or procedure variable.\n", expr->line_num);
        ++error_count;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_assigned");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Assigned.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        expr->resolved_type = BOOL;
        *type_return = BOOL;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_abs(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Abs expects exactly one argument.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    const char *mangled_name = NULL;
    int result_type = UNKNOWN_TYPE;
    if (error_count == 0)
    {
        if (arg_type == INT_TYPE)
        {
            mangled_name = "kgpc_abs_int";
            result_type = INT_TYPE;
        }
        else if (arg_type == LONGINT_TYPE)
        {
            mangled_name = "kgpc_abs_longint";
            result_type = LONGINT_TYPE;
        }
        else if (arg_type == REAL_TYPE)
        {
            mangled_name = "kgpc_abs_real";
            result_type = REAL_TYPE;
        }
        else if (arg_type == INT64_TYPE)
        {
            mangled_name = "kgpc_abs_longint";
            result_type = INT64_TYPE;
        }
        else
        {
            fprintf(stderr, "Error on line %d, Abs expects integer or real arguments.\n", expr->line_num);
            ++error_count;
        }
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup(mangled_name);
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Abs.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        expr->resolved_type = result_type;
        if (expr->resolved_kgpc_type != NULL)
        {
            destroy_kgpc_type(expr->resolved_kgpc_type);
            expr->resolved_kgpc_type = NULL;
        }
        expr->resolved_kgpc_type = create_primitive_type(result_type);
        *type_return = result_type;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_unary_real(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, const char *display_name,
    const char *mangled_name, int result_type)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, %s expects exactly one argument.\n",
            expr->line_num, display_name);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    if (error_count == 0 && arg_type != REAL_TYPE &&
        arg_type != INT_TYPE && arg_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, %s expects a real argument.\n",
            expr->line_num, display_name);
        ++error_count;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup(mangled_name);
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for %s.\n",
                display_name);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        expr->resolved_type = result_type;
        *type_return = result_type;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_arctan2(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, ArcTan2 expects exactly two arguments.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *y_expr = (struct Expression *)args->cur;
    struct Expression *x_expr = (struct Expression *)args->next->cur;
    int y_type = UNKNOWN_TYPE;
    int x_type = UNKNOWN_TYPE;
    int error_count = 0;

    error_count += semcheck_expr_main(&y_type, symtab, y_expr, max_scope_lev, NO_MUTATE);
    error_count += semcheck_expr_main(&x_type, symtab, x_expr, max_scope_lev, NO_MUTATE);

    if (y_type != REAL_TYPE && y_type != INT_TYPE && y_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, ArcTan2 expects numeric arguments.\n",
            expr->line_num);
        ++error_count;
    }
    if (x_type != REAL_TYPE && x_type != INT_TYPE && x_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, ArcTan2 expects numeric arguments.\n",
            expr->line_num);
        ++error_count;
    }

    if (error_count != 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    expr->expr_data.function_call_data.mangled_id = strdup("kgpc_arctan2");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for ArcTan2.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    expr->resolved_type = REAL_TYPE;
    expr->resolved_kgpc_type = create_primitive_type(REAL_TYPE);
    *type_return = REAL_TYPE;
    return 0;
}

static int semcheck_builtin_hypot(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Hypot expects exactly two arguments.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *x_expr = (struct Expression *)args->cur;
    struct Expression *y_expr = (struct Expression *)args->next->cur;
    int x_type = UNKNOWN_TYPE;
    int y_type = UNKNOWN_TYPE;
    int error_count = 0;

    error_count += semcheck_expr_main(&x_type, symtab, x_expr, max_scope_lev, NO_MUTATE);
    error_count += semcheck_expr_main(&y_type, symtab, y_expr, max_scope_lev, NO_MUTATE);

    if (x_type != REAL_TYPE && x_type != INT_TYPE && x_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Hypot expects numeric arguments.\n",
            expr->line_num);
        ++error_count;
    }
    if (y_type != REAL_TYPE && y_type != INT_TYPE && y_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Hypot expects numeric arguments.\n",
            expr->line_num);
        ++error_count;
    }

    if (error_count != 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    expr->expr_data.function_call_data.mangled_id = strdup("kgpc_hypot");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for Hypot.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    expr->resolved_type = REAL_TYPE;
    expr->resolved_kgpc_type = create_primitive_type(REAL_TYPE);
    *type_return = REAL_TYPE;
    return 0;
}

static int semcheck_builtin_logn(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, LogN expects exactly two arguments.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *base_expr = (struct Expression *)args->cur;
    struct Expression *value_expr = (struct Expression *)args->next->cur;
    int base_type = UNKNOWN_TYPE;
    int value_type = UNKNOWN_TYPE;
    int error_count = 0;

    error_count += semcheck_expr_main(&base_type, symtab, base_expr, max_scope_lev, NO_MUTATE);
    error_count += semcheck_expr_main(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);

    if (base_type != REAL_TYPE && base_type != INT_TYPE && base_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, LogN expects numeric arguments.\n",
            expr->line_num);
        ++error_count;
    }
    if (value_type != REAL_TYPE && value_type != INT_TYPE && value_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, LogN expects numeric arguments.\n",
            expr->line_num);
        ++error_count;
    }

    if (error_count != 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    expr->expr_data.function_call_data.mangled_id = strdup("kgpc_logn");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for LogN.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    expr->resolved_type = REAL_TYPE;
    expr->resolved_kgpc_type = create_primitive_type(REAL_TYPE);
    *type_return = REAL_TYPE;
    return 0;
}


static int semcheck_builtin_upcase(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, UpCase expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    if (error_count == 0 && arg_type != CHAR_TYPE)
    {
        if (arg_expr != NULL && arg_expr->type == EXPR_STRING &&
            arg_expr->expr_data.string != NULL &&
            strlen(arg_expr->expr_data.string) == 1)
        {
            unsigned char value = (unsigned char)arg_expr->expr_data.string[0];
            free(arg_expr->expr_data.string);
            arg_expr->expr_data.string = NULL;
            arg_expr->type = EXPR_CHAR_CODE;
            arg_expr->expr_data.char_code = value;
            arg_type = CHAR_TYPE;
        }
        else
        {
            fprintf(stderr, "Error on line %d, UpCase expects a char argument.\n",
                expr->line_num);
            ++error_count;
        }
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_upcase_char");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for UpCase.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        /* Mark as valid so code generator won't look up "UpCase" and find
         * the string overload. With call_kgpc_type=NULL (from reset) and
         * is_call_info_valid=1, no formal parameter info will be used. */
        expr->expr_data.function_call_data.is_call_info_valid = 1;
        expr->resolved_type = CHAR_TYPE;
        expr->resolved_kgpc_type = create_primitive_type(CHAR_TYPE);
        *type_return = CHAR_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_predsucc(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int is_succ)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, %s expects exactly one argument.\n",
            expr->line_num, is_succ ? "Succ" : "Pred");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
    if (error_count == 0 &&
        arg_type != INT_TYPE && arg_type != LONGINT_TYPE && arg_type != INT64_TYPE)
    {
        fprintf(stderr, "Error on line %d, %s expects an integer argument.\n",
            expr->line_num, is_succ ? "Succ" : "Pred");
        ++error_count;
    }

    if (error_count != 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    /* Rewrite Pred/Succ to arg +/- 1 */
    struct Expression *rhs = mk_inum(expr->line_num, 1);
    if (rhs == NULL)
    {
        fprintf(stderr, "Error on line %d, failed to build %s expression.\n",
            expr->line_num, is_succ ? "Succ" : "Pred");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    DestroyList(args);
    if (expr->expr_data.function_call_data.id != NULL)
    {
        free(expr->expr_data.function_call_data.id);
        expr->expr_data.function_call_data.id = NULL;
    }
    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));

    expr->type = EXPR_ADDOP;
    expr->expr_data.addop_data.addop_type = is_succ ? PLUS : MINUS;
    expr->expr_data.addop_data.left_expr = arg_expr;
    expr->expr_data.addop_data.right_term = rhs;
    semcheck_reset_function_call_cache(expr);

    return semcheck_expr_main(type_return, symtab, expr, max_scope_lev, NO_MUTATE);
}

static int semcheck_builtin_odd(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Odd expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    if (error_count == 0 && arg_type != INT_TYPE && arg_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Odd expects an integer argument.\n",
            expr->line_num);
        ++error_count;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_is_odd");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Odd.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        expr->resolved_type = BOOL;
        *type_return = BOOL;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_sqr(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Sqr expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    const char *mangled_name = NULL;
    int result_type = UNKNOWN_TYPE;

    if (error_count == 0)
    {
        if (arg_type == REAL_TYPE)
        {
            mangled_name = "kgpc_sqr_real";
            result_type = REAL_TYPE;
        }
        else if (arg_type == LONGINT_TYPE)
        {
            mangled_name = "kgpc_sqr_int64";
            result_type = LONGINT_TYPE;
        }
        else if (arg_type == INT_TYPE)
        {
            mangled_name = "kgpc_sqr_int32";
            result_type = INT_TYPE;
        }
        else
        {
            fprintf(stderr, "Error on line %d, Sqr expects integer or real arguments.\n",
                expr->line_num);
            ++error_count;
        }
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup(mangled_name);
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Sqr.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        expr->resolved_type = result_type;
        *type_return = result_type;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_sizeof(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);
static int semcheck_builtin_default(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Default expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int target_type = UNKNOWN_TYPE;
    struct RecordType *record_type = NULL;
    KgpcType *target_kgpc_type = NULL;

    /* Prefer resolving the argument as a type identifier to match FPC Default(T) semantics. */
    if (arg_expr != NULL && arg_expr->type == EXPR_VAR_ID && arg_expr->expr_data.id != NULL)
    {
        HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type(symtab,
            arg_expr->expr_data.id);
        if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            set_type_from_hashtype(&target_type, type_node);
            record_type = get_record_type_from_node(type_node);
            target_kgpc_type = type_node->type;
            if (target_kgpc_type != NULL)
                kgpc_type_retain(target_kgpc_type);
        }
    }

    int error_count = 0;
    if (target_type == UNKNOWN_TYPE)
    {
        error_count = semcheck_expr_main(&target_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
        if (error_count != 0)
        {
            *type_return = UNKNOWN_TYPE;
        if (target_kgpc_type != NULL)
                destroy_kgpc_type(target_kgpc_type);
            return error_count;
        }
        record_type = arg_expr != NULL ? arg_expr->record_type : NULL;
        if (arg_expr != NULL && arg_expr->resolved_kgpc_type != NULL)
        {
            target_kgpc_type = arg_expr->resolved_kgpc_type;
            kgpc_type_retain(target_kgpc_type);
        }
    }

    if (target_type == UNKNOWN_TYPE && target_kgpc_type != NULL)
        target_type = kgpc_type_get_legacy_tag(target_kgpc_type);

    if (target_type == UNKNOWN_TYPE)
    {
        fprintf(stderr, "Error on line %d, Default requires a type identifier or typed expression.\n",
            expr->line_num);
        if (target_kgpc_type != NULL)
            destroy_kgpc_type(target_kgpc_type);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    if (record_type == NULL && target_kgpc_type != NULL &&
        target_kgpc_type->kind == TYPE_KIND_RECORD)
    {
        record_type = target_kgpc_type->info.record_info;
    }

    semcheck_free_call_args(expr->expr_data.function_call_data.args_expr, NULL);
    expr->expr_data.function_call_data.args_expr = NULL;
    if (expr->expr_data.function_call_data.id != NULL)
    {
        free(expr->expr_data.function_call_data.id);
        expr->expr_data.function_call_data.id = NULL;
    }
    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    semcheck_reset_function_call_cache(expr);

    /* Lower to literals for primitives/pointers to avoid dangling call targets.
     * For records, mark as zero-init and keep record type info. */
    if (target_type == RECORD_TYPE)
    {
        expr->is_default_initializer = 1;
        expr->resolved_type = target_type;
        expr->record_type = record_type;
        if (expr->resolved_kgpc_type != NULL)
            destroy_kgpc_type(expr->resolved_kgpc_type);
        if (target_kgpc_type != NULL)
            expr->resolved_kgpc_type = target_kgpc_type;
        else if (record_type != NULL)
            expr->resolved_kgpc_type = create_record_type(record_type);
        else
            expr->resolved_kgpc_type = NULL;
        *type_return = target_type;
        return 0;
    }

    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    if (target_kgpc_type != NULL)
        destroy_kgpc_type(target_kgpc_type);

    switch (target_type)
    {
        case INT_TYPE:
        case LONGINT_TYPE:
            expr->type = EXPR_INUM;
            expr->expr_data.i_num = 0;
            expr->resolved_type = target_type;
            expr->resolved_kgpc_type = create_primitive_type(target_type);
            *type_return = target_type;
            return 0;
        case REAL_TYPE:
            expr->type = EXPR_RNUM;
            expr->expr_data.r_num = 0.0f;
            expr->resolved_type = REAL_TYPE;
            expr->resolved_kgpc_type = create_primitive_type(REAL_TYPE);
            *type_return = REAL_TYPE;
            return 0;
        case BOOL:
            expr->type = EXPR_BOOL;
            expr->expr_data.bool_value = 0;
            expr->resolved_type = BOOL;
            expr->resolved_kgpc_type = create_primitive_type(BOOL);
            *type_return = BOOL;
            return 0;
        case CHAR_TYPE:
            expr->type = EXPR_CHAR_CODE;
            expr->expr_data.char_code = 0;
            expr->resolved_type = CHAR_TYPE;
            expr->resolved_kgpc_type = create_primitive_type(CHAR_TYPE);
            *type_return = CHAR_TYPE;
            return 0;
        case STRING_TYPE:
            expr->type = EXPR_STRING;
            expr->expr_data.string = strdup("");
            expr->resolved_type = STRING_TYPE;
            expr->resolved_kgpc_type = create_primitive_type(STRING_TYPE);
            *type_return = STRING_TYPE;
            return 0;
        case POINTER_TYPE:
            expr->type = EXPR_NIL;
            expr->resolved_type = POINTER_TYPE;
            expr->resolved_kgpc_type = create_primitive_type(POINTER_TYPE);
            *type_return = POINTER_TYPE;
            return 0;
        case ENUM_TYPE:
            expr->type = EXPR_INUM;
            expr->expr_data.i_num = 0;
            expr->resolved_type = ENUM_TYPE;
            expr->resolved_kgpc_type = create_primitive_type(ENUM_TYPE);
            *type_return = ENUM_TYPE;
            return 0;
        default:
            fprintf(stderr, "Error on line %d, Default for this type is unsupported in this context.\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
    }
}
static int semcheck_builtin_lowhigh(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int is_high)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, %s expects exactly one argument.\n",
            expr->line_num, is_high ? "High" : "Low");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    if (arg_expr != NULL && arg_expr->type == EXPR_VAR_ID && arg_expr->expr_data.id != NULL)
    {
        const char *type_name = semcheck_base_type_name(arg_expr->expr_data.id);
        HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_name);
        if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            struct TypeAlias *alias = get_type_alias_from_node(type_node);
            long long low = 0;
            long long high = 0;
            int have_bounds = 0;
            int result_type = INT_TYPE;

            if (alias != NULL && alias->is_range && alias->range_known)
            {
                low = alias->range_start;
                high = alias->range_end;
                have_bounds = 1;
                if (low < -2147483648LL || high > 2147483647LL)
                    result_type = INT64_TYPE;
            }
            else if (alias != NULL && alias->target_type_id != NULL)
            {
                HashNode_t *target_node = semcheck_find_preferred_type_node(symtab,
                    alias->target_type_id);
                if (target_node != NULL && target_node->hash_type == HASHTYPE_TYPE)
                {
                    struct TypeAlias *target_alias = get_type_alias_from_node(target_node);
                    if (target_alias != NULL && target_alias->is_range && target_alias->range_known)
                    {
                        low = target_alias->range_start;
                        high = target_alias->range_end;
                        have_bounds = 1;
                        if (low < -2147483648LL || high > 2147483647LL)
                            result_type = INT64_TYPE;
                    }
                }
                if (!have_bounds)
                {
                    const char *target_name = alias->target_type_id;
                    if (pascal_identifier_equals(target_name, "SmallInt")) {
                        low = -32768LL;
                        high = 32767LL;
                        have_bounds = 1;
                    } else if (pascal_identifier_equals(target_name, "Word")) {
                        low = 0;
                        high = 65535LL;
                        have_bounds = 1;
                    } else if (pascal_identifier_equals(target_name, "ShortInt")) {
                        low = -128LL;
                        high = 127LL;
                        have_bounds = 1;
                    } else if (pascal_identifier_equals(target_name, "Byte")) {
                        low = 0;
                        high = 255LL;
                        have_bounds = 1;
                    } else if (pascal_identifier_equals(target_name, "Cardinal") ||
                               pascal_identifier_equals(target_name, "LongWord") ||
                               pascal_identifier_equals(target_name, "DWord")) {
                        low = 0;
                        high = 4294967295LL;
                        have_bounds = 1;
                        result_type = INT64_TYPE;
                    }
                }
            }
            else if (pascal_identifier_equals(type_name, "Int64"))
            {
                low = (-9223372036854775807LL - 1);
                high = 9223372036854775807LL;
                have_bounds = 1;
                result_type = INT64_TYPE;
            }
            else if (pascal_identifier_equals(type_name, "QWord") ||
                     pascal_identifier_equals(type_name, "UInt64"))
            {
                low = 0;
                high = (long long)0xFFFFFFFFFFFFFFFFULL;
                have_bounds = 1;
                result_type = INT64_TYPE;
            }
            else if (pascal_identifier_equals(type_name, "LongInt"))
            {
                low = -2147483648LL;
                high = 2147483647LL;
                have_bounds = 1;
                result_type = LONGINT_TYPE;
            }
            else if (pascal_identifier_equals(type_name, "Integer"))
            {
                low = -2147483648LL;
                high = 2147483647LL;
                have_bounds = 1;
                result_type = INT_TYPE;
            }
            else if (pascal_identifier_equals(type_name, "Cardinal") ||
                     pascal_identifier_equals(type_name, "LongWord") ||
                     pascal_identifier_equals(type_name, "DWord"))
            {
                low = 0;
                high = 4294967295LL;
                have_bounds = 1;
                result_type = INT64_TYPE;
            }
            else if (pascal_identifier_equals(type_name, "SmallInt"))
            {
                low = -32768LL;
                high = 32767LL;
                have_bounds = 1;
            }
            else if (pascal_identifier_equals(type_name, "Word"))
            {
                low = 0;
                high = 65535LL;
                have_bounds = 1;
            }
            else if (pascal_identifier_equals(type_name, "ShortInt"))
            {
                low = -128LL;
                high = 127LL;
                have_bounds = 1;
            }
            else if (pascal_identifier_equals(type_name, "Byte"))
            {
                low = 0;
                high = 255LL;
                have_bounds = 1;
            }
            else if (pascal_identifier_equals(type_name, "Boolean"))
            {
                low = 0;
                high = 1;
                have_bounds = 1;
                result_type = BOOL;
            }
            else if (pascal_identifier_equals(type_name, "Char") ||
                     pascal_identifier_equals(type_name, "AnsiChar"))
            {
                low = 0;
                high = 255;
                have_bounds = 1;
                result_type = CHAR_TYPE;
            }

            if (have_bounds)
            {
                semcheck_replace_call_with_integer_literal(expr, is_high ? high : low);
                expr->resolved_type = result_type;
                *type_return = result_type;
                return 0;
            }
        }
    }

    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
    if (error_count != 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    if (arg_expr->is_array_expr)
    {
        if (!arg_expr->array_is_dynamic)
        {
            long long lower = arg_expr->array_lower_bound;
            long long upper = arg_expr->array_upper_bound;
            if (is_high && upper < lower)
            {
                fprintf(stderr, "Error on line %d, invalid array bounds for High().\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }

            long long bound_value = is_high ? upper : lower;
            semcheck_replace_call_with_integer_literal(expr, bound_value);
            *type_return = LONGINT_TYPE;
            return 0;
        }

        if (!is_high)
        {
            semcheck_replace_call_with_integer_literal(expr, arg_expr->array_lower_bound);
            *type_return = LONGINT_TYPE;
            return 0;
        }

        return semcheck_prepare_dynarray_high_call(type_return, symtab,
            expr, max_scope_lev, arg_expr);
    }

    if (arg_type == STRING_TYPE && !arg_expr->is_array_expr)
    {
        if (!is_high)
        {
            semcheck_replace_call_with_integer_literal(expr, 1);
            *type_return = LONGINT_TYPE;
            return 0;
        }

        if (expr->expr_data.function_call_data.id != NULL)
            free(expr->expr_data.function_call_data.id);
        expr->expr_data.function_call_data.id = strdup("Length");
        if (expr->expr_data.function_call_data.id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate identifier for High(string).\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        semcheck_reset_function_call_cache(expr);
        return semcheck_builtin_length(type_return, symtab, expr, max_scope_lev);
    }

    /* Ordinal overloads */
    if (arg_type == INT_TYPE)
    {
        semcheck_replace_call_with_integer_literal(expr, is_high ? 2147483647LL : -2147483648LL);
        expr->resolved_type = INT_TYPE;
        *type_return = INT_TYPE;
        return 0;
    }
    if (arg_type == LONGINT_TYPE)
    {
        semcheck_replace_call_with_integer_literal(expr, is_high ? 2147483647LL : -2147483648LL);
        expr->resolved_type = LONGINT_TYPE;
        *type_return = LONGINT_TYPE;
        return 0;
    }
    if (arg_type == INT64_TYPE)
    {
        semcheck_replace_call_with_integer_literal(expr, is_high ? 9223372036854775807LL : (-9223372036854775807LL - 1));
        expr->resolved_type = INT64_TYPE;
        *type_return = INT64_TYPE;
        return 0;
    }
    if (arg_type == BOOL)
    {
        semcheck_free_call_args(expr->expr_data.function_call_data.args_expr, NULL);
        expr->expr_data.function_call_data.args_expr = NULL;
        expr->type = EXPR_BOOL;
        expr->expr_data.bool_value = is_high ? 1 : 0;
        expr->resolved_type = BOOL;
        *type_return = BOOL;
        return 0;
    }
    if (arg_type == CHAR_TYPE)
    {
        semcheck_free_call_args(expr->expr_data.function_call_data.args_expr, NULL);
        expr->expr_data.function_call_data.args_expr = NULL;
        expr->type = EXPR_CHAR_CODE;
        expr->expr_data.char_code = (unsigned int)(is_high ? 255 : 0);
        expr->resolved_type = CHAR_TYPE;
        *type_return = CHAR_TYPE;
        return 0;
    }

    fprintf(stderr, "Error on line %d, %s currently supports only array or string arguments.\n",
        expr->line_num, is_high ? "High" : "Low");
    *type_return = UNKNOWN_TYPE;
    return 1;
}

static long long sizeof_from_type_tag(int type_tag)
{
    switch(type_tag)
    {
        case INT_TYPE:
            return 4;
        case LONGINT_TYPE:
            return 4;
        case REAL_TYPE:
            return 8;
        case STRING_TYPE:
            return POINTER_SIZE_BYTES;
        case CHAR_TYPE:
            return 1;
        case BOOL:
            /*
             * Standalone booleans occupy 4 bytes to keep stack accesses aligned,
             * but packed arrays of boolean elements are emitted as 1 byte each.
             * Document the distinction so sizeof(boolean) users are not surprised.
             */
            return 4;
        case POINTER_TYPE:
            return POINTER_SIZE_BYTES;
        case SET_TYPE:
        case ENUM_TYPE:
            return 4;
        case FILE_TYPE:
        case TEXT_TYPE:
            return POINTER_SIZE_BYTES;
        case PROCEDURE:
            return POINTER_SIZE_BYTES;
        default:
            return -1;
    }
}

static int sizeof_from_type_ref(SymTab_t *symtab, int type_tag,
    const char *type_id, long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        fprintf(stderr, "Error on line %d, SizeOf exceeded recursion depth while resolving type.\n",
            line_num);
        return 1;
    }

    if (type_tag != UNKNOWN_TYPE)
    {
        long long base_size = sizeof_from_type_tag(type_tag);
        if (base_size >= 0)
        {
            *size_out = base_size;
            return 0;
        }
    }

    if (type_id != NULL)
    {
        HashNode_t *target_node = semcheck_find_preferred_type_node(symtab, type_id);
        if (target_node == NULL)
        {
            /* For generic type parameters or nested types that haven't been resolved yet,
             * treat as pointer-sized rather than hard error - this allows:
             * - generic templates to be processed without full instantiation
             * - FPC bootstrap where nested types (Public type ...) aren't supported yet
             * Nested procedural types and class references are typically pointer-sized */
            const char *debug_env = getenv("KGPC_DEBUG_TFPG");
            if (debug_env != NULL)
            {
                fprintf(stderr, "[KGPC] SizeOf: unknown type %s at line %d (may be unresolved generic parameter or nested type)\n",
                    type_id, line_num);
            }
            *size_out = POINTER_SIZE_BYTES;  /* Assume pointer-sized for unknown types */
            return 0;  /* Return success to allow processing to continue */
        }
        return sizeof_from_hashnode(symtab, target_node, size_out, depth + 1, line_num);
    }

    /* If both type_tag and type_id are unspecified, we can't compute size.
     * This might happen for generic type parameters - don't print error, just fail gracefully */
    const char *debug_env = getenv("KGPC_DEBUG_TFPG");
    if (debug_env != NULL)
    {
        fprintf(stderr, "[KGPC] SizeOf: unable to resolve type at line %d (unspecified type_tag and type_id)\n",
            line_num);
    }
    *size_out = 0;
    return 1;  /* Return error - caller should check if this is expected */
}

static int sizeof_from_record(SymTab_t *symtab, struct RecordType *record,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (record == NULL)
    {
        *size_out = 0;
        return 0;
    }

    if (record->has_cached_size)
    {
        *size_out = record->cached_size;
        return 0;
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        fprintf(stderr, "Error on line %d, SizeOf exceeded recursion depth while resolving record type.\n",
            line_num);
        return 1;
    }

    long long computed_size = 0;
    
    /* Classes have a VMT pointer at offset 0 */
    if (record->is_class)
        computed_size = 8; /* 64-bit pointer */
        
    if (sizeof_from_record_members(symtab, record->fields, &computed_size, depth + 1, line_num) != 0)
        return 1;

    record->cached_size = computed_size;
    record->has_cached_size = 1;
    *size_out = computed_size;
    return 0;
}

/* Align offset to the specified alignment boundary */
static long long align_offset(long long offset, int alignment)
{
    if (alignment <= 0)
        return offset;
    
    long long remainder = offset % alignment;
    if (remainder == 0)
        return offset;
    
    return offset + (alignment - remainder);
}

/* Get alignment requirement for a field type (64-bit ABI) */
static int get_field_alignment(SymTab_t *symtab, struct RecordField *field, int depth, int line_num)
{
    if (field == NULL)
        return 1;  /* Minimum alignment */
    
    /* Prevent infinite recursion */
    if (depth > SIZEOF_RECURSION_LIMIT)
        return 1;
    
    /* Dynamic arrays are descriptors (pointer + int64), aligned to 8 */
    if (field->is_array && field->array_is_open)
        return POINTER_SIZE_BYTES;

    /*
     * Compute a natural alignment based on the underlying element size.
     * We clamp the result to pointer size (8 on this target) but never
     * return less than 1 so that small scalar fields (Byte/Word) can sit
     * back-to-back without being promoted to 4-byte boundaries.
     */
    long long elem_size = 0;
    int status = 0;

    if (field->is_array)
    {
        /* Static arrays align to their element type, not total size */
        if (field->array_element_type == RECORD_TYPE && field->nested_record != NULL)
            status = sizeof_from_record(symtab, field->nested_record, &elem_size, depth + 1, line_num);
        else if (field->array_element_type != UNKNOWN_TYPE || field->array_element_type_id != NULL)
            status = sizeof_from_type_ref(symtab, field->array_element_type, field->array_element_type_id,
                &elem_size, depth + 1, line_num);
        else
            status = sizeof_from_type_ref(symtab, field->type, field->type_id, &elem_size, depth + 1, line_num);
    }
    else if (field->nested_record != NULL)
    {
        status = sizeof_from_record(symtab, field->nested_record, &elem_size, depth + 1, line_num);
    }
    else
    {
        status = sizeof_from_type_ref(symtab, field->type, field->type_id, &elem_size, depth + 1, line_num);
    }

    if (status != 0 || elem_size <= 0)
        return 1; /* fallback to minimal alignment */

    if (elem_size > POINTER_SIZE_BYTES)
        return POINTER_SIZE_BYTES;
    return (int)elem_size;
}

static int compute_field_size(SymTab_t *symtab, struct RecordField *field,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (field == NULL)
    {
        *size_out = 0;
        return 0;
    }

    const char *debug_env = getenv("KGPC_DEBUG_TFPG");
    if (debug_env != NULL && field->name != NULL) {
        fprintf(stderr, "[KGPC] compute_field_size: field=%s type=%d type_id=%s is_array=%d\n",
            field->name, field->type, field->type_id ? field->type_id : "<null>", field->is_array);
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        fprintf(stderr, "Error on line %d, SizeOf exceeded recursion depth while resolving record field.\n",
            line_num);
        return 1;
    }

    if (field->is_array)
    {
        const char *debug_env = getenv("KGPC_DEBUG_TFPG");
        if (debug_env != NULL && field->name != NULL) {
            fprintf(stderr, "[KGPC] compute_field_size array: field=%s is_open=%d start=%d end=%d\n",
                field->name, field->array_is_open, field->array_start, field->array_end);
        }

        if (field->array_is_open || field->array_end < field->array_start)
        {
            /* Dynamic arrays are descriptors {void *data, int64_t length}, so 16 bytes */
            *size_out = POINTER_SIZE_BYTES * 2;
            return 0;
        }

        long long element_size = 0;
        int elem_status = 1;
        if (field->array_element_type == RECORD_TYPE && field->nested_record != NULL)
            elem_status = sizeof_from_record(symtab, field->nested_record,
                &element_size, depth + 1, line_num);
        else if (field->array_element_type != UNKNOWN_TYPE ||
            field->array_element_type_id != NULL)
            elem_status = sizeof_from_type_ref(symtab, field->array_element_type,
                field->array_element_type_id, &element_size, depth + 1, line_num);
        else
            elem_status = sizeof_from_type_ref(symtab, field->type, field->type_id,
                &element_size, depth + 1, line_num);

        if (elem_status != 0)
            return 1;

        long long count = (long long)field->array_end - (long long)field->array_start + 1;
        if (count < 0)
        {
            fprintf(stderr, "Error on line %d, invalid bounds for array field %s.\n",
                line_num, field->name != NULL ? field->name : "");
            return 1;
        }

        *size_out = element_size * count;
        return 0;
    }

    if (field->nested_record != NULL)
        return sizeof_from_record(symtab, field->nested_record, size_out, depth + 1, line_num);

    return sizeof_from_type_ref(symtab, field->type, field->type_id, size_out, depth + 1, line_num);
}

static int sizeof_from_record_members(SymTab_t *symtab, ListNode_t *members,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    long long total = *size_out;
    int max_alignment = 1;  /* Track maximum alignment for struct padding */
    
    ListNode_t *cur = members;
    while (cur != NULL)
    {
        if (cur->type == LIST_RECORD_FIELD)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            
            /* Get field alignment and align current offset */
            int field_alignment = get_field_alignment(symtab, field, depth + 1, line_num);
            if (field_alignment > max_alignment)
                max_alignment = field_alignment;
            
            total = align_offset(total, field_alignment);
            
            /* Add field size */
            long long field_size = 0;
            int field_result = compute_field_size(symtab, field, &field_size, depth + 1, line_num);
            if (field_result != 0)
            {
#ifdef DEBUG
                fprintf(stderr, "DEBUG: compute_field_size FAILED for field %s: result=%d\n",
                    field->name ? field->name : "<null>", field_result);
#endif
                return 1;
            }
            total += field_size;
        }
        else if (cur->type == LIST_VARIANT_PART)
        {
            struct VariantPart *variant = (struct VariantPart *)cur->cur;
            long long variant_size = 0;
            if (sizeof_from_variant_part(symtab, variant, &variant_size, depth + 1, line_num) != 0)
                return 1;
            total += variant_size;
        }
        cur = cur->next;
    }

    /* Align total size to the maximum alignment (struct padding at end) */
    total = align_offset(total, max_alignment);
    
    *size_out = total;
    return 0;
}

static int sizeof_from_variant_part(SymTab_t *symtab, struct VariantPart *variant,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (variant == NULL)
    {
        *size_out = 0;
        return 0;
    }

    if (variant->has_cached_size)
    {
        *size_out = variant->cached_size;
        return 0;
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        fprintf(stderr, "Error on line %d, SizeOf exceeded recursion depth while resolving variant part.\n",
            line_num);
        return 1;
    }

    long long max_size = 0;
    ListNode_t *cur = variant->branches;
    while (cur != NULL)
    {
        if (cur->type == LIST_VARIANT_BRANCH)
        {
            struct VariantBranch *branch = (struct VariantBranch *)cur->cur;
            long long branch_size = 0;
            if (sizeof_from_record_members(symtab, branch->members, &branch_size,
                    depth + 1, line_num) != 0)
                return 1;
            if (branch_size > max_size)
                max_size = branch_size;
        }
        cur = cur->next;
    }

    variant->cached_size = max_size;
    variant->has_cached_size = 1;
    *size_out = max_size;
    return 0;
}

static int find_field_in_variant(SymTab_t *symtab, struct VariantPart *variant,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int depth, int line_num, int *found)
{
    if (found != NULL)
        *found = 0;

    if (variant == NULL || field_name == NULL)
        return 0;

    ListNode_t *cur = variant->branches;
    while (cur != NULL)
    {
        if (cur->type == LIST_VARIANT_BRANCH)
        {
            struct VariantBranch *branch = (struct VariantBranch *)cur->cur;
            long long branch_offset = 0;
            int branch_found = 0;
            if (find_field_in_members(symtab, branch->members, field_name, out_field,
                    &branch_offset, depth + 1, line_num, &branch_found) != 0)
                return 1;
            if (branch_found)
            {
                if (offset_out != NULL)
                    *offset_out = branch_offset;
                if (found != NULL)
                    *found = 1;
                return 0;
            }
        }
        cur = cur->next;
    }

    return 0;
}

static int find_field_in_members(SymTab_t *symtab, ListNode_t *members,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int depth, int line_num, int *found)
{
    if (found != NULL)
        *found = 0;

    long long offset = 0;
    ListNode_t *cur = members;
    while (cur != NULL)
    {
        if (cur->type == LIST_RECORD_FIELD)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            if (field != NULL && !record_field_is_hidden(field))
            {
                /* Align offset to field's alignment requirement */
                int field_alignment = get_field_alignment(symtab, field, depth + 1, line_num);
                offset = align_offset(offset, field_alignment);
                
                /* Check if this is the target field */
                if (field->name != NULL &&
                    pascal_identifier_equals(field->name, field_name))
                {
                    if (out_field != NULL)
                        *out_field = field;
                    if (offset_out != NULL)
                        *offset_out = offset;
                    if (found != NULL)
                        *found = 1;
                    return 0;
                }
            }

            long long field_size = 0;
            if (compute_field_size(symtab, field, &field_size, depth + 1, line_num) != 0)
                return 1;
            offset += field_size;
        }
        else if (cur->type == LIST_VARIANT_PART)
        {
            struct VariantPart *variant = (struct VariantPart *)cur->cur;
            long long variant_field_offset = 0;
            int variant_found = 0;
            if (find_field_in_variant(symtab, variant, field_name, out_field,
                    &variant_field_offset, depth + 1, line_num, &variant_found) != 0)
                return 1;
            if (variant_found)
            {
                if (offset_out != NULL)
                    *offset_out = offset + variant_field_offset;
                if (found != NULL)
                    *found = 1;
                return 0;
            }

            long long variant_size = 0;
            if (sizeof_from_variant_part(symtab, variant, &variant_size, depth + 1, line_num) != 0)
                return 1;
            offset += variant_size;
        }
        cur = cur->next;
    }

    if (offset_out != NULL)
        *offset_out = offset;
    return 0;
}

static int sizeof_from_alias(SymTab_t *symtab, struct TypeAlias *alias,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (alias == NULL)
    {
        fprintf(stderr, "Error on line %d, SizeOf encountered incomplete type alias.\n",
            line_num);
        return 1;
    }

    if (alias->storage_size > 0 && !alias->is_array && !alias->is_set &&
        !alias->is_enum && !alias->is_file)
    {
        *size_out = alias->storage_size;
        return 0;
    }

    if (alias->is_pointer)
    {
        *size_out = POINTER_SIZE_BYTES;
        return 0;
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        fprintf(stderr, "Error on line %d, SizeOf exceeded recursion depth while resolving type alias.\n",
            line_num);
        return 1;
    }

    if (alias->is_array)
    {
        if (alias->is_open_array || alias->array_end < alias->array_start)
        {
            fprintf(stderr, "Error on line %d, SizeOf cannot determine size of open array type.\n",
                line_num);
            return 1;
        }

        long long element_size = 0;
        if (sizeof_from_type_ref(symtab, alias->array_element_type,
                alias->array_element_type_id, &element_size, depth + 1, line_num) != 0)
            return 1;

        long long count = (long long)alias->array_end - (long long)alias->array_start + 1;
        if (count < 0)
        {
            fprintf(stderr, "Error on line %d, invalid bounds for array type in SizeOf.\n",
                line_num);
            return 1;
        }

        *size_out = element_size * count;
        return 0;
    }

    if (alias->base_type != UNKNOWN_TYPE)
    {
        return sizeof_from_type_ref(symtab, alias->base_type, NULL,
            size_out, depth + 1, line_num);
    }

    if (alias->target_type_id != NULL)
        return sizeof_from_type_ref(symtab, UNKNOWN_TYPE, alias->target_type_id,
            size_out, depth + 1, line_num);

    fprintf(stderr, "Error on line %d, SizeOf encountered unresolved type alias.\n",
        line_num);
    return 1;
}

static int sizeof_from_hashnode(SymTab_t *symtab, HashNode_t *node,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (node == NULL)
    {
        fprintf(stderr, "Error on line %d, SizeOf encountered null symbol information.\n",
            line_num);
        return 1;
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        fprintf(stderr, "Error on line %d, SizeOf exceeded recursion depth while resolving symbol.\n",
            line_num);
        return 1;
    }

    /* PREFERRED PATH: Try using KgpcType directly if available */
    if (node->type != NULL)
    {
        long long size = kgpc_type_sizeof(node->type);
        if (size > 0)
        {
            *size_out = size;
            return 0;
        }
        else if (size == 0)
        {
            /* Zero-sized type (e.g., empty record or zero-length array) */
            *size_out = 0;
            return 0;
        }
        /* else size < 0: kgpc_type_sizeof couldn't determine size, fall through to legacy path */
    }

    /* LEGACY PATH: KgpcType not available or couldn't determine size */
    
    if (node->hash_type == HASHTYPE_TYPE)
    {
        struct RecordType *record = get_record_type_from_node(node);
        if (record != NULL)
            return sizeof_from_record(symtab, record, size_out, depth + 1, line_num);
            
        struct TypeAlias *alias = get_type_alias_from_node(node);
        if (alias != NULL)
        {
            return sizeof_from_alias(symtab, alias, size_out, depth + 1, line_num);
        }

    }

    /* For array size calculation */
    int is_array = hashnode_is_array(node);
    
    if (is_array)
    {
        int is_dynamic = hashnode_is_dynamic_array(node);
        
        if (is_dynamic)
        {
            fprintf(stderr, "Error on line %d, SizeOf cannot determine size of dynamic array %s.\n",
                line_num, node->id);
            return 1;
        }

        /* Get element size */
        long long element_size = hashnode_get_element_size(node);
        if (element_size <= 0)
        {
            fprintf(stderr, "Error on line %d, cannot determine element size for array %s.\n",
                line_num, node->id);
            return 1;
        }
        
        /* Get array bounds */
        int array_start, array_end;
        hashnode_get_array_bounds(node, &array_start, &array_end);
        
        long long count = (long long)array_end - (long long)array_start + 1;
        if (count < 0)
        {
            fprintf(stderr, "Error on line %d, invalid bounds for array %s in SizeOf.\n",
                line_num, node->id);
            return 1;
        }

        *size_out = element_size * count;
        return 0;
    }

    if (node_is_record_type(node))
    {
        struct RecordType *record = get_record_type_from_node(node);
        if (record != NULL)
            return sizeof_from_record(symtab, record, size_out, depth + 1, line_num);
    }

    struct TypeAlias *alias = get_type_alias_from_node(node);
    if (alias != NULL)
        return sizeof_from_alias(symtab, alias, size_out, depth + 1, line_num);

    fprintf(stderr, "Error on line %d, SizeOf cannot determine size of %s.\n",
        line_num, node->id != NULL ? node->id : "symbol");
    return 1;
}

static int semcheck_builtin_sizeof(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, SizeOf expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg = (struct Expression *)args->cur;
    int error_count = 0;
    long long computed_size = 0;

    if (arg != NULL && arg->type == EXPR_VAR_ID)
    {
        char *arg_id = arg->expr_data.id;
        HashNode_t *node = NULL;
        int scope = FindIdent(&node, symtab, arg_id);
        if (scope == -1 || node == NULL)
        {
            /* Check if this is a builtin type name that isn't in the symbol table */
            int is_builtin_type = 0;
            if (pascal_identifier_equals(arg_id, "Integer") ||
                pascal_identifier_equals(arg_id, "LongInt") ||
                pascal_identifier_equals(arg_id, "Real") ||
                pascal_identifier_equals(arg_id, "Double") ||
                pascal_identifier_equals(arg_id, "Char") ||
                pascal_identifier_equals(arg_id, "Boolean") ||
                pascal_identifier_equals(arg_id, "Byte") ||
                pascal_identifier_equals(arg_id, "Word") ||
                pascal_identifier_equals(arg_id, "String") ||
                pascal_identifier_equals(arg_id, "Pointer") ||
                pascal_identifier_equals(arg_id, "SizeUInt") ||
                pascal_identifier_equals(arg_id, "QWord") ||
                pascal_identifier_equals(arg_id, "NativeUInt"))
            {
                is_builtin_type = 1;
                /* Determine the size based on the type */
                if (pascal_identifier_equals(arg_id, "Integer"))
                    computed_size = 4;  /* 32-bit */
                else if (pascal_identifier_equals(arg_id, "Byte"))
                    computed_size = 1;  /* 8-bit unsigned */
                else if (pascal_identifier_equals(arg_id, "Word"))
                    computed_size = 2;  /* 16-bit unsigned */
                else if (pascal_identifier_equals(arg_id, "LongInt") ||
                         pascal_identifier_equals(arg_id, "SizeUInt") ||
                         pascal_identifier_equals(arg_id, "QWord") ||
                         pascal_identifier_equals(arg_id, "NativeUInt"))
                    computed_size = 8;  /* 64-bit */
                else if (pascal_identifier_equals(arg_id, "Real") ||
                         pascal_identifier_equals(arg_id, "Double"))
                    computed_size = 8;  /* 64-bit float */
                else if (pascal_identifier_equals(arg_id, "Char"))
                    computed_size = 1;  /* 8-bit */
                else if (pascal_identifier_equals(arg_id, "Boolean"))
                    computed_size = 1;  /* 8-bit */
                else if (pascal_identifier_equals(arg_id, "String"))
                    computed_size = 8;  /* Pointer */
                else if (pascal_identifier_equals(arg_id, "Pointer"))
                    computed_size = 8;  /* Pointer */
            }
            
            if (!is_builtin_type)
            {
                fprintf(stderr, "Error on line %d, SizeOf references undeclared identifier %s.\n",
                    expr->line_num, arg_id);
                error_count++;
            }
        }
        else
        {
            if (node->hash_type != HASHTYPE_TYPE && scope > max_scope_lev)
            {
                fprintf(stderr, "Error on line %d, SizeOf cannot access %s due to scope restrictions.\n",
                    expr->line_num, arg_id);
                error_count++;
            }

            if (error_count == 0)
            {
                if (node->hash_type == HASHTYPE_VAR || node->hash_type == HASHTYPE_ARRAY ||
                    node->hash_type == HASHTYPE_CONST || node->hash_type == HASHTYPE_FUNCTION_RETURN)
                {
                    set_hash_meta(node, NO_MUTATE);
                }
                else if (node->hash_type != HASHTYPE_TYPE)
                {
                    fprintf(stderr, "Error on line %d, SizeOf argument %s is not a data object.\n",
                        expr->line_num, arg_id);
                    error_count++;
                }
            }

            if (error_count == 0)
            {
                if (node->hash_type != HASHTYPE_TYPE && node->hash_type != HASHTYPE_ARRAY)
                {
                    int dummy_type = UNKNOWN_TYPE;
                    error_count += semcheck_expr_main(&dummy_type, symtab, arg,
                        max_scope_lev, NO_MUTATE);
                }

                if (error_count == 0)
                {
                    struct TypeAlias *alias = get_type_alias_from_node(node);
                    int used_target_size = 0;
                    if (alias != NULL && alias->target_type_id != NULL)
                    {
                        const char *target = alias->target_type_id;
                        if (pascal_identifier_equals(target, "Byte") ||
                            pascal_identifier_equals(target, "ShortInt")) {
                            computed_size = 1;
                            used_target_size = 1;
                        } else if (pascal_identifier_equals(target, "Word") ||
                                   pascal_identifier_equals(target, "SmallInt")) {
                            computed_size = 2;
                            used_target_size = 1;
                        } else if (pascal_identifier_equals(target, "Cardinal") ||
                                   pascal_identifier_equals(target, "LongWord") ||
                                   pascal_identifier_equals(target, "DWord")) {
                            computed_size = 4;
                            used_target_size = 1;
                        }
                    }
                    if (!used_target_size)
                    {
                        if (alias != NULL && alias->storage_size <= 0 &&
                            alias->target_type_id != NULL)
                        {
                            HashNode_t *target_node = semcheck_find_preferred_type_node(symtab,
                                alias->target_type_id);
                            if (target_node != NULL && target_node->hash_type == HASHTYPE_TYPE &&
                                sizeof_from_hashnode(symtab, target_node, &computed_size,
                                    0, expr->line_num) == 0)
                            {
                                /* Use target type size */
                            }
                            else
                            {
                                error_count += sizeof_from_hashnode(symtab, node, &computed_size,
                                    0, expr->line_num);
                            }
                        }
                        else
                        {
                            error_count += sizeof_from_hashnode(symtab, node, &computed_size,
                                0, expr->line_num);
                        }
                    }
                }
            }
        }
    }
    else
    {
        int arg_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_main(&arg_type, symtab, arg, max_scope_lev, NO_MUTATE);
        if (error_count == 0)
            error_count += sizeof_from_type_ref(symtab, arg_type, NULL, &computed_size,
                0, expr->line_num);
    }

    if (error_count == 0)
    {
        if (computed_size < 0)
        {
            fprintf(stderr, "Error on line %d, SizeOf produced an invalid result.\n",
                expr->line_num);
            error_count++;
        }
        /* No upper-bound clamp: computed_size already stored in a long long literal. */
    }

    if (error_count == 0)
    {
        destroy_list(expr->expr_data.function_call_data.args_expr);
        expr->expr_data.function_call_data.args_expr = NULL;
        if (expr->expr_data.function_call_data.id != NULL)
        {
            free(expr->expr_data.function_call_data.id);
            expr->expr_data.function_call_data.id = NULL;
        }
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        semcheck_reset_function_call_cache(expr);

        expr->type = EXPR_INUM;
        expr->expr_data.i_num = computed_size;
        expr->resolved_type = LONGINT_TYPE;
        *type_return = LONGINT_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_relop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_signterm(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_addop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_mulop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_varid(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
    /* Sets hash meta based on given mutating flag */
void set_hash_meta(HashNode_t *node, int mutating)
{
    assert(node != NULL);
    if(mutating == BOTH_MUTATE_REFERENCE)
    {
        node->referenced += 1;
        node->mutated += 1;
    }
    else
    {
        node->referenced += 1-mutating;
        node->mutated += mutating;
    }
}

int semcheck_compute_record_size(SymTab_t *symtab, struct RecordType *record,
    long long *size_out, int line_num)
{
    return sizeof_from_record(symtab, record, size_out, 0, line_num);
}

/* Verifies a type is an INT_TYPE or REAL_TYPE */
int is_type_ir(int *type)
{
    assert(type != NULL);
    return (is_integer_type(*type) || *type == REAL_TYPE);
}

static int types_numeric_compatible(int lhs, int rhs)
{
    if (lhs == rhs)
        return 1;
    /* All integer types are compatible with each other */
    if (is_integer_type(lhs) && is_integer_type(rhs))
        return 1;
    /* Real is compatible with any integer type */
    if ((lhs == REAL_TYPE && is_integer_type(rhs)) ||
        (rhs == REAL_TYPE && is_integer_type(lhs)))
        return 1;
    return 0;
}

static void semcheck_coerce_char_string_operands(int *type_first, struct Expression *expr1,
    int *type_second, struct Expression *expr2)
{
    if (type_first == NULL || type_second == NULL)
        return;

    if (expr1 != NULL && semcheck_expr_is_char_like(expr1) && *type_first != CHAR_TYPE)
        *type_first = CHAR_TYPE;
    if (expr2 != NULL && semcheck_expr_is_char_like(expr2) && *type_second != CHAR_TYPE)
        *type_second = CHAR_TYPE;

    /* Handle CHAR + STRING or STRING + CHAR comparisons
     * Upgrade CHAR to STRING for comparison purposes */
    if ((*type_first == CHAR_TYPE && *type_second == STRING_TYPE) ||
        (*type_first == STRING_TYPE && *type_second == CHAR_TYPE))
    {
        /* Upgrade CHAR operand to STRING */
        if (*type_first == CHAR_TYPE)
        {
            *type_first = STRING_TYPE;
            if (expr1 != NULL)
                expr1->resolved_type = STRING_TYPE;
        }
        else /* *type_second == CHAR_TYPE */
        {
            *type_second = STRING_TYPE;
            if (expr2 != NULL)
                expr2->resolved_type = STRING_TYPE;
        }
    }
    
    /* Handle CHAR + CHAR comparisons where both are string literals
     * Single-character string literals like '/' are parsed as CHAR_TYPE,
     * but should be treated as STRING_TYPE for comparison purposes */
    if (*type_first == CHAR_TYPE && *type_second == CHAR_TYPE)
    {
        int expr1_is_string_literal = (expr1 != NULL && expr1->type == EXPR_STRING);
        int expr2_is_string_literal = (expr2 != NULL && expr2->type == EXPR_STRING);
        
        /* If at least one is a string literal, treat both as strings */
        if (expr1_is_string_literal || expr2_is_string_literal)
        {
            *type_first = STRING_TYPE;
            *type_second = STRING_TYPE;
            if (expr1 != NULL)
                expr1->resolved_type = STRING_TYPE;
            if (expr2 != NULL)
                expr2->resolved_type = STRING_TYPE;
        }
    }
}

static int semcheck_expr_is_char_like(struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    if (expr->resolved_type == CHAR_TYPE)
        return 1;
    if (expr->resolved_kgpc_type != NULL)
    {
        if (kgpc_type_get_primitive_tag(expr->resolved_kgpc_type) == CHAR_TYPE)
            return 1;
        struct TypeAlias *alias = kgpc_type_get_type_alias(expr->resolved_kgpc_type);
        if (alias != NULL && alias->is_char_alias)
            return 1;
    }
    return 0;
}

static int resolve_type_identifier(int *out_type, SymTab_t *symtab,
    const char *type_id, int line_num)
{
    if (type_id == NULL)
        return 0;

    HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_id);
    if (type_node == NULL)
    {
        fprintf(stderr, "Error on line %d, typecast references unknown type %s!\n\n",
            line_num, type_id);
        return 1;
    }

    if (type_node->hash_type != HASHTYPE_TYPE)
    {
        fprintf(stderr, "Error on line %d, %s is not a type identifier.\n\n",
            line_num, type_id);
        return 1;
    }

    set_type_from_hashtype(out_type, type_node);

    struct TypeAlias *alias = get_type_alias_from_node(type_node);
    if (alias != NULL)
    {
        if (alias->base_type != UNKNOWN_TYPE)
            *out_type = alias->base_type;

        if (alias->target_type_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, alias->target_type_id) != -1 &&
                target_node != NULL)
            {
                set_type_from_hashtype(out_type, target_node);
            }
        }

        if (alias->is_pointer)
            *out_type = POINTER_TYPE;
        else if (alias->is_set)
            *out_type = SET_TYPE;
        else if (alias->is_enum)
            *out_type = ENUM_TYPE;
        else if (alias->is_file)
            *out_type = FILE_TYPE;
    }

    return 0;
}
/* Checks if a type is a relational AND or OR */
int is_and_or(int *type)
{
    assert(type != NULL);
    return (*type == AND || *type == OR);
}

static int semcheck_typecast(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;

    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_TYPECAST);

    int error_count = 0;
    int inner_type = UNKNOWN_TYPE;

    if (expr->expr_data.typecast_data.expr != NULL)
        error_count += semcheck_expr_main(&inner_type, symtab,
            expr->expr_data.typecast_data.expr, max_scope_lev, NO_MUTATE);

    int target_type = expr->expr_data.typecast_data.target_type;
    int builtin_mapped = semcheck_map_builtin_type_name(symtab,
        expr->expr_data.typecast_data.target_type_id);
    int target_is_builtin = (builtin_mapped != UNKNOWN_TYPE);
    if (target_type == UNKNOWN_TYPE && builtin_mapped != UNKNOWN_TYPE)
        target_type = builtin_mapped;

    /* Resolve the target type unless we already mapped a builtin */
    if (target_type == UNKNOWN_TYPE || !target_is_builtin)
    {
        error_count += resolve_type_identifier(&target_type, symtab,
            expr->expr_data.typecast_data.target_type_id, expr->line_num);
    }

    if (target_type == UNKNOWN_TYPE &&
        expr->expr_data.typecast_data.target_type_id == NULL)
    {
        fprintf(stderr, "Error on line %d, typecast requires a target type.\n\n",
            expr->line_num);
        ++error_count;
    }

    *type_return = target_type;
    expr->resolved_type = target_type;

    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    if (target_type == PROCEDURE)
    {
        HashNode_t *target_node = NULL;
        if (expr->expr_data.typecast_data.target_type_id != NULL)
        {
            target_node = semcheck_find_type_node_with_kgpc_type(
                symtab, expr->expr_data.typecast_data.target_type_id);
            if (target_node == NULL &&
                FindIdent(&target_node, symtab, expr->expr_data.typecast_data.target_type_id) >= 0)
            {
                /* target_node assigned by FindIdent when present */
            }
        }

        if (target_node != NULL && target_node->type != NULL &&
            target_node->type->kind == TYPE_KIND_PROCEDURE)
        {
            kgpc_type_retain(target_node->type);
            expr->resolved_kgpc_type = target_node->type;
        }
    }
    if (target_type == POINTER_TYPE)
    {
        /* Resolve full pointer type info so deref preserves record/element types */
        KgpcType *resolved_ptr = NULL;
        if (expr->expr_data.typecast_data.target_type_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, expr->expr_data.typecast_data.target_type_id) >= 0 &&
                target_node != NULL && target_node->type != NULL)
            {
                resolved_ptr = target_node->type;
                kgpc_type_retain(resolved_ptr);

                if (resolved_ptr->kind == TYPE_KIND_POINTER &&
                    resolved_ptr->info.points_to != NULL)
                {
                    KgpcType *points_to = resolved_ptr->info.points_to;
                    if (points_to->kind == TYPE_KIND_RECORD && points_to->info.record_info != NULL)
                    {
                        semcheck_set_pointer_info(expr, RECORD_TYPE, points_to->info.record_info->type_id);
                        expr->record_type = points_to->info.record_info;
                    }
                    else if (points_to->kind == TYPE_KIND_PRIMITIVE)
                    {
                        int subtype = kgpc_type_get_primitive_tag(points_to);
                        semcheck_set_pointer_info(expr, subtype, NULL);
                    }
                }
            }
        }

        if (resolved_ptr == NULL)
        {
            resolved_ptr = create_pointer_type(NULL);
            semcheck_set_pointer_info(expr, UNKNOWN_TYPE, expr->expr_data.typecast_data.target_type_id);
        }

        expr->resolved_kgpc_type = resolved_ptr;
    }
    else if (target_type == RECORD_TYPE)
    {
        HashNode_t *target_node = NULL;
        struct RecordType *record_info = NULL;
        if (expr->expr_data.typecast_data.target_type_id != NULL)
        {
            target_node = semcheck_find_type_node_with_kgpc_type(
                symtab, expr->expr_data.typecast_data.target_type_id);
            if (target_node == NULL &&
                FindIdent(&target_node, symtab, expr->expr_data.typecast_data.target_type_id) >= 0)
            {
                /* target_node assigned by FindIdent when present */
            }
            if (target_node != NULL)
            {
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                {
                    fprintf(stderr, "[SemCheck] typecast record target=%s node=%p kgpc_kind=%d\n",
                        expr->expr_data.typecast_data.target_type_id,
                        (void *)target_node,
                        target_node->type != NULL ? target_node->type->kind : -1);
                }
                record_info = get_record_type_from_node(target_node);
                if (record_info == NULL && target_node->type != NULL &&
                    kgpc_type_is_record(target_node->type))
                {
                    record_info = kgpc_type_get_record(target_node->type);
                }
            }
        }

        expr->record_type = record_info;
        if (target_node != NULL && target_node->type != NULL)
        {
            kgpc_type_retain(target_node->type);
            expr->resolved_kgpc_type = target_node->type;
        }
        else if (record_info != NULL)
        {
            expr->resolved_kgpc_type = create_record_type(record_info);
        }
        else
        {
            expr->resolved_kgpc_type = create_primitive_type(RECORD_TYPE);
        }
    }
    else if (target_type != UNKNOWN_TYPE)
    {
        expr->resolved_kgpc_type = create_primitive_type(target_type);
        semcheck_clear_array_info(expr);
        expr->record_type = NULL;
    }

    (void)inner_type;
    return error_count;
}

static int semcheck_is_expr(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_IS);

    int error_count = 0;
    struct Expression *value_expr = expr->expr_data.is_data.expr;
    if (value_expr == NULL)
    {
        fprintf(stderr, "Error on line %d, \"is\" operator requires a value expression.\n\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int value_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);

    struct RecordType *value_record = value_expr->record_type;
    
    /* Classes are pointers to records, so we need to handle POINTER_TYPE */
    int is_valid_class = 0;
    if (value_type == RECORD_TYPE && value_record != NULL && record_type_is_class(value_record))
    {
        is_valid_class = 1;
    }
    else if (value_type == POINTER_TYPE && value_record != NULL && record_type_is_class(value_record))
    {
        is_valid_class = 1;
    }

    if (!is_valid_class)
    {
        fprintf(stderr, "Error on line %d, \"is\" operator requires a class instance on the left-hand side.\n\n",
            expr->line_num);
        ++error_count;
    }

    int target_type = expr->expr_data.is_data.target_type;
    struct RecordType *target_record = NULL;
    if (expr->expr_data.is_data.target_type_id != NULL)
    {
        target_record = semcheck_lookup_record_type(symtab,
            expr->expr_data.is_data.target_type_id);
            
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] is_expr: lookup '%s' -> %p\n", 
                expr->expr_data.is_data.target_type_id, target_record);
            if (target_record) {
                fprintf(stderr, "[SemCheck]   is_class=%d\n", target_record->is_class);
            }
        }
    }
    
    /* Check if target is a class (could be RECORD_TYPE or POINTER_TYPE to record) */
    int is_valid_target = 0;
    if (target_record != NULL && record_type_is_class(target_record))
    {
        is_valid_target = 1;
    }
    
    if (!is_valid_target)
    {
        fprintf(stderr, "Error on line %d, \"is\" operator requires a class type on the right-hand side.\n\n",
            expr->line_num);
        ++error_count;
    }
    target_type = RECORD_TYPE;

    expr->expr_data.is_data.target_type = target_type;
    expr->expr_data.is_data.target_record_type = target_record;
    expr->resolved_type = BOOL;
    *type_return = BOOL;
    return error_count;
}

static int semcheck_as_expr(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_AS);

    int error_count = 0;
    struct Expression *value_expr = expr->expr_data.as_data.expr;
    if (value_expr == NULL)
    {
        fprintf(stderr, "Error on line %d, \"as\" operator requires a value expression.\n\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int value_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);

    struct RecordType *value_record = value_expr->record_type;
    
    /* Classes are pointers to records, so we need to handle POINTER_TYPE */
    int is_valid_class = 0;
    if (value_type == RECORD_TYPE && value_record != NULL && record_type_is_class(value_record))
    {
        is_valid_class = 1;
    }
    else if (value_type == POINTER_TYPE && value_record != NULL && record_type_is_class(value_record))
    {
        is_valid_class = 1;
    }

    if (!is_valid_class)
    {
        fprintf(stderr, "Error on line %d, \"as\" operator requires a class instance on the left-hand side.\n\n",
            expr->line_num);
        ++error_count;
    }

    int target_type = expr->expr_data.as_data.target_type;
    struct RecordType *target_record = NULL;
    if (expr->expr_data.as_data.target_type_id != NULL)
    {
        target_record = semcheck_lookup_record_type(symtab,
            expr->expr_data.as_data.target_type_id);
    }
    
    /* Check if target is a class (could be RECORD_TYPE or POINTER_TYPE to record) */
    int is_valid_target = 0;
    if (target_record != NULL && record_type_is_class(target_record))
    {
        is_valid_target = 1;
    }
    
    if (!is_valid_target)
    {
        fprintf(stderr, "Error on line %d, \"as\" operator requires a class type on the right-hand side.\n\n",
            expr->line_num);
        ++error_count;
    }
    /* Determine correct target type */
    target_type = RECORD_TYPE;
    KgpcType *result_kgpc_type = NULL;

    if (target_record != NULL && record_type_is_class(target_record))
    {
        target_type = POINTER_TYPE;
        KgpcType *record_kgpc = create_record_type(target_record);
        if (record_kgpc != NULL)
        {
            result_kgpc_type = create_pointer_type(record_kgpc);
        }
    }
    else
    {
        result_kgpc_type = create_record_type(target_record);
    }

    expr->expr_data.as_data.target_type = target_type;
    expr->expr_data.as_data.target_record_type = target_record;
    expr->record_type = target_record;
    expr->resolved_type = target_type;
    
    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    expr->resolved_kgpc_type = result_kgpc_type;
    
    *type_return = target_type;
    return error_count;
}

static int semcheck_pointer_deref(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;

    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_POINTER_DEREF);

    semcheck_clear_pointer_info(expr);
    semcheck_clear_array_info(expr);
    expr->record_type = NULL;

    struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr == NULL)
    {
        fprintf(stderr, "Error on line %d, dereference operator requires an operand.\\n\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int error_count = 0;
    int pointer_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&pointer_type, symtab, pointer_expr,
        max_scope_lev, NO_MUTATE);

    if (pointer_type != POINTER_TYPE)
    {
        fprintf(stderr, "Error on line %d, dereference operator requires a pointer expression.\\n\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return ++error_count;
    }

    int target_type = pointer_expr->pointer_subtype;
    if (target_type == UNKNOWN_TYPE && pointer_expr->pointer_subtype_id != NULL)
    {
        HashNode_t *target_node = NULL;
        if (FindIdent(&target_node, symtab, pointer_expr->pointer_subtype_id) != -1 &&
            target_node != NULL)
        {
            set_type_from_hashtype(&target_type, target_node);
            struct TypeAlias *alias = get_type_alias_from_node(target_node);
            if (alias != NULL)
            {
                if (alias->base_type != UNKNOWN_TYPE)
                    target_type = alias->base_type;
                else if (alias->is_pointer)
                    target_type = POINTER_TYPE;
                else if (alias->is_set)
                    target_type = SET_TYPE;
                else if (alias->is_enum)
                    target_type = ENUM_TYPE;
                else if (alias->is_file)
                    target_type = FILE_TYPE;
            }
        }

        /* If still unknown, try to infer size from the subtype id.
         * This helps pointer types like PAnsiChar inherit a 1-byte element size
         * instead of defaulting to LONGINT (4 bytes). */
        if (target_type == UNKNOWN_TYPE)
        {
            long long inferred_size = 0;
            if (sizeof_from_type_ref(symtab, UNKNOWN_TYPE, pointer_expr->pointer_subtype_id,
                    &inferred_size, 0, expr->line_num) == 0 && inferred_size > 0)
            {
                if (inferred_size == 1)
                    target_type = CHAR_TYPE;
                else if (inferred_size == 2)
                    target_type = INT_TYPE; /* 16-bit */
                else if (inferred_size <= 4)
                    target_type = INT_TYPE;
                else
                    target_type = LONGINT_TYPE;
            }
        }
    }

    /* If we still don't know the subtype, inspect the pointer symbol's alias info */
    if (target_type == UNKNOWN_TYPE && pointer_expr->type == EXPR_VAR_ID)
    {
        HashNode_t *ptr_node = NULL;
        if (FindIdent(&ptr_node, symtab, pointer_expr->expr_data.id) != -1 && ptr_node != NULL)
        {
            struct TypeAlias *alias = get_type_alias_from_node(ptr_node);
            if (alias != NULL && alias->is_pointer)
            {
                target_type = alias->pointer_type;
                if (target_type == UNKNOWN_TYPE && alias->pointer_type_id != NULL)
                {
                    long long inferred_size = 0;
                    if (sizeof_from_type_ref(symtab, UNKNOWN_TYPE, alias->pointer_type_id,
                            &inferred_size, 0, expr->line_num) == 0 && inferred_size > 0)
                    {
                        if (inferred_size == 1)
                            target_type = CHAR_TYPE;
                        else if (inferred_size <= 4)
                            target_type = INT_TYPE;
                        else
                            target_type = LONGINT_TYPE;
                    }
                }

                if (alias->pointer_type_id != NULL && expr->pointer_subtype_id == NULL)
                    semcheck_set_pointer_info(expr, target_type, alias->pointer_type_id);
            }
        }
    }

    /* If subtype id was absent, try to infer from the resolved KgpcType pointer info */
    if (target_type == UNKNOWN_TYPE && pointer_expr->resolved_kgpc_type != NULL &&
        pointer_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER)
    {
        KgpcType *points_to = pointer_expr->resolved_kgpc_type->info.points_to;
        if (points_to != NULL)
        {
            long long inferred_size = kgpc_type_sizeof(points_to);
            if (inferred_size == 1)
                target_type = CHAR_TYPE;
            else if (inferred_size == 2)
                target_type = INT_TYPE;
            else if (inferred_size > 0 && inferred_size <= 4)
                target_type = INT_TYPE;
            else if (inferred_size > 0)
                target_type = LONGINT_TYPE;
        }
    }

    if (target_type == UNKNOWN_TYPE)
        target_type = LONGINT_TYPE;

    if (target_type == POINTER_TYPE)
        semcheck_set_pointer_info(expr, POINTER_TYPE, pointer_expr->pointer_subtype_id);
    else if (target_type == RECORD_TYPE)
    {
        expr->record_type = pointer_expr->record_type;
        if (expr->record_type == NULL && pointer_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, pointer_expr->pointer_subtype_id) != -1 && target_node != NULL)
                expr->record_type = get_record_type_from_node(target_node);
        }
    }

    if (pointer_expr->pointer_subtype_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, pointer_expr->pointer_subtype_id) != -1 && type_node != NULL)
        {
            struct TypeAlias *alias = get_type_alias_from_node(type_node);
            if (alias != NULL && alias->is_array)
            {
                semcheck_set_array_info_from_alias(expr, symtab, alias, expr->line_num);
            }
        }
    }

    *type_return = target_type;
    return error_count;
}

static int semcheck_property_type_info(SymTab_t *symtab, struct ClassProperty *property,
    int line_num, int *type_out, struct RecordType **record_out)
{
    if (type_out == NULL || property == NULL)
        return 1;

    int resolved_type = property->type;
    if (property->type_id != NULL)
    {
        if (resolve_type_identifier(&resolved_type, symtab, property->type_id, line_num) != 0)
        {
            fprintf(stderr, "Error on line %d, unable to resolve type for property %s.\n\n",
                line_num, property->name != NULL ? property->name : "<unnamed>");
            return 1;
        }
    }

    if (resolved_type == UNKNOWN_TYPE && property->type_id == NULL)
    {
        fprintf(stderr, "Error on line %d, property %s must specify a type.\n\n",
            line_num, property->name != NULL ? property->name : "<unnamed>");
        return 1;
    }

    *type_out = resolved_type;
    if (record_out != NULL)
    {
        if (resolved_type == RECORD_TYPE && property->type_id != NULL)
            *record_out = semcheck_lookup_record_type(symtab, property->type_id);
        else
            *record_out = NULL;
    }

    return 0;
}

static int semcheck_transform_property_getter_call(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating,
    HashNode_t *method_node)
{
    if (expr == NULL || expr->type != EXPR_RECORD_ACCESS || method_node == NULL)
    {
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *object_expr = expr->expr_data.record_access_data.record_expr;
    if (object_expr == NULL)
    {
        fprintf(stderr, "Error on line %d, property getter requires an object instance.\n\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    expr->expr_data.record_access_data.record_expr = NULL;
    if (expr->expr_data.record_access_data.field_id != NULL)
    {
        free(expr->expr_data.record_access_data.field_id);
        expr->expr_data.record_access_data.field_id = NULL;
    }

    ListNode_t *arg_node = CreateListNode(object_expr, LIST_EXPR);
    if (arg_node == NULL)
    {
        fprintf(stderr, "Error on line %d, unable to allocate getter argument list.\n\n",
            expr->line_num);
        expr->expr_data.record_access_data.record_expr = object_expr;
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    char *id_copy = method_node->id != NULL ? strdup(method_node->id) : NULL;
    char *mangled_copy = NULL;
    if (method_node->mangled_id != NULL)
        mangled_copy = strdup(method_node->mangled_id);

    if ((method_node->id != NULL && id_copy == NULL) ||
        (method_node->mangled_id != NULL && mangled_copy == NULL))
    {
        fprintf(stderr, "Error on line %d, unable to prepare property getter call.\n\n",
            expr->line_num);
        free(id_copy);
        free(mangled_copy);
        free(arg_node);
        expr->expr_data.record_access_data.record_expr = object_expr;
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    expr->type = EXPR_FUNCTION_CALL;
    memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
    expr->expr_data.function_call_data.id = id_copy;
    expr->expr_data.function_call_data.mangled_id = mangled_copy;
    expr->expr_data.function_call_data.args_expr = arg_node;
    expr->expr_data.function_call_data.resolved_func = NULL;
    expr->expr_data.function_call_data.call_hash_type = method_node->hash_type;
    semcheck_expr_set_call_kgpc_type(expr, method_node->type, 0);
    expr->expr_data.function_call_data.is_call_info_valid = 1;
    expr->record_type = NULL;
    expr->resolved_type = UNKNOWN_TYPE;
    expr->is_array_expr = 0;
    expr->array_element_type = UNKNOWN_TYPE;
    expr->array_element_type_id = NULL;
    expr->array_element_record_type = NULL;
    expr->array_element_size = 0;

    return semcheck_expr_main(type_return, symtab, expr, max_scope_lev, mutating);
}

static int semcheck_recordaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_RECORD_ACCESS);

    expr->record_type = NULL;
    semcheck_clear_array_info(expr);

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    const char *field_id = expr->expr_data.record_access_data.field_id;
    if (record_expr == NULL || field_id == NULL)
    {
        fprintf(stderr, "Error on line %d, malformed record field access.\n\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    /* AST TRANSFORMATION FIX: Parser incorrectly parses `-r.x` as `(-r).x` instead of `-(r.x)`.
     * When we detect this pattern (record access on a sign term), we restructure the AST
     * to have the correct operator precedence: the sign term should wrap the record access. 
     * 
     * We only handle EXPR_SIGN_TERM (unary operators) as binary operators (ADDOP, MULOP)
     * would require more complex transformation logic. */
    if (record_expr->type == EXPR_SIGN_TERM)
        {
            /* Current structure: RECORD_ACCESS(SIGN_TERM(inner_expr), field)
             * Desired structure: SIGN_TERM(RECORD_ACCESS(inner_expr, field)) */
            struct Expression *inner_expr = record_expr->expr_data.sign_term;
            if (inner_expr != NULL)
            {
                /* Create a new RECORD_ACCESS for inner_expr.field */
                struct Expression *new_record_access = (struct Expression *)calloc(1, sizeof(struct Expression));
                if (new_record_access == NULL)
                {
                    fprintf(stderr, "Error on line %d: failed to allocate expression for AST transformation in semcheck_recordaccess.\n",
                        expr->line_num);
                    *type_return = UNKNOWN_TYPE;
                    return 1;
                }
                
                new_record_access->line_num = expr->line_num;
                new_record_access->type = EXPR_RECORD_ACCESS;
                new_record_access->expr_data.record_access_data.record_expr = inner_expr;
                new_record_access->expr_data.record_access_data.field_id = strdup(field_id);
                if (new_record_access->expr_data.record_access_data.field_id == NULL)
                {
                    fprintf(stderr, "Error on line %d: failed to duplicate field name in AST transformation.\n",
                        expr->line_num);
                    free(new_record_access);
                    *type_return = UNKNOWN_TYPE;
                    return 1;
                }
                new_record_access->expr_data.record_access_data.field_offset = 0;
                new_record_access->record_type = NULL;
                new_record_access->resolved_type = UNKNOWN_TYPE;
                new_record_access->is_array_expr = 0;
                new_record_access->array_element_type = UNKNOWN_TYPE;
                new_record_access->array_element_type_id = NULL;
                new_record_access->array_element_record_type = NULL;
                new_record_access->array_element_size = 0;
                new_record_access->array_lower_bound = 0;
                new_record_access->array_upper_bound = -1;
                new_record_access->array_is_dynamic = 0;
                new_record_access->pointer_subtype = UNKNOWN_TYPE;
                new_record_access->pointer_subtype_id = NULL;
                
                /* Restructure: make the current expr be the SIGN_TERM wrapping the new RECORD_ACCESS */
                record_expr->expr_data.sign_term = new_record_access;
                
                /* Now swap the types so expr becomes SIGN_TERM and the original SIGN_TERM content is preserved */
                enum ExprType temp_type = expr->type;
                expr->type = record_expr->type;
                record_expr->type = temp_type;
                
                /* Swap the data unions */
                union expr_data temp_data = expr->expr_data;
                expr->expr_data = record_expr->expr_data;
                record_expr->expr_data = temp_data;
                
                /* Now expr is SIGN_TERM wrapping new_record_access, and we can process it as a sign term */
                /* Redirect to semcheck_signterm for the transformed expression */
                return semcheck_signterm(type_return, symtab, expr, max_scope_lev, mutating);
            }
        }

    /* Similar AST transformation for NOT operator: parser produces (NOT record).field
     * instead of NOT (record.field). Detect EXPR_RELOP with NOT and restructure so the
     * NOT wraps the record access rather than the record expression itself. */
    if (record_expr->type == EXPR_RELOP &&
        record_expr->expr_data.relop_data.type == NOT &&
        record_expr->expr_data.relop_data.left != NULL &&
        record_expr->expr_data.relop_data.right == NULL)
    {
        struct Expression *inner_expr = record_expr->expr_data.relop_data.left;

        /* Create new record access for inner_expr.field */
        struct Expression *new_record_access = (struct Expression *)calloc(1, sizeof(struct Expression));
        if (new_record_access == NULL)
        {
            fprintf(stderr, "Error on line %d: failed to allocate expression for AST transformation in semcheck_recordaccess.\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

        new_record_access->line_num = expr->line_num;
        new_record_access->type = EXPR_RECORD_ACCESS;
        new_record_access->expr_data.record_access_data.record_expr = inner_expr;
        new_record_access->expr_data.record_access_data.field_id = strdup(field_id);
        if (new_record_access->expr_data.record_access_data.field_id == NULL)
        {
            fprintf(stderr, "Error on line %d: failed to duplicate field name in AST transformation.\n",
                expr->line_num);
            free(new_record_access);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        new_record_access->expr_data.record_access_data.field_offset = 0;

        /* Insert new record access as operand of NOT */
        record_expr->expr_data.relop_data.left = new_record_access;

        /* Swap expression types/data so current node becomes the NOT expression */
        enum ExprType temp_type = expr->type;
        expr->type = record_expr->type;
        record_expr->type = temp_type;

        union expr_data temp_data = expr->expr_data;
        expr->expr_data = record_expr->expr_data;
        record_expr->expr_data = temp_data;

        /* Now expr is the NOT expression wrapping record access; re-run semantic check as relop */
        return semcheck_relop(type_return, symtab, expr, max_scope_lev, mutating);
    }

    /* FPC Bootstrap Feature: Handle unit-qualified identifiers in runtime expressions.
     * When we see UnitName.ConstName and UnitName is an unresolvable identifier,
     * check if ConstName is a known constant/var in the current scope (since unit
     * exports are merged). If so, transform the expression to just the identifier. */
    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
    {
        char *unit_id = record_expr->expr_data.id;
        HashNode_t *unit_check = NULL;
        
        /* Check if the "unit name" identifier exists in symbol table */
        if (FindIdent(&unit_check, symtab, unit_id) == -1)
        {
            /* Identifier not found - might be a unit qualifier.
             * Try to look up the field_id directly as it may be an exported constant/var. */
            HashNode_t *field_node = NULL;
            char *field_id_copy = strdup(field_id);
            if (field_id_copy != NULL && FindIdent(&field_node, symtab, field_id_copy) >= 0 && field_node != NULL)
            {
                free(field_id_copy);
                /* Found the field as a direct identifier - transform the expression */
                if (field_node->hash_type == HASHTYPE_CONST)
                {
                    /* Transform to integer literal for constants */
                    expr->type = EXPR_INUM;
                    expr->expr_data.i_num = field_node->const_int_value;
                    expr->resolved_type = LONGINT_TYPE;
                    if (field_node->type != NULL)
                    {
                        expr->resolved_kgpc_type = field_node->type;
                    }
                    *type_return = LONGINT_TYPE;
                    return 0;
                }
                else if (field_node->hash_type == HASHTYPE_VAR || 
                         field_node->hash_type == HASHTYPE_ARRAY)
                {
                    /* Transform to simple variable reference */
                    char *field_copy = strdup(field_id);
                    if (field_copy == NULL)
                    {
                        fprintf(stderr, "Error on line %d: failed to allocate memory for unit-qualified variable.\n",
                            expr->line_num);
                        *type_return = UNKNOWN_TYPE;
                        return 1;
                    }
                    expr->type = EXPR_VAR_ID;
                    expr->expr_data.id = field_copy;
                    return semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
                }
                else if (field_node->hash_type == HASHTYPE_TYPE)
                {
                    /* Unit.TypeName - transform to simple type reference */
                    char *field_copy = strdup(field_id);
                    if (field_copy == NULL)
                    {
                        fprintf(stderr, "Error on line %d: failed to allocate memory for unit-qualified type.\n",
                            expr->line_num);
                        *type_return = UNKNOWN_TYPE;
                        return 1;
                    }
                    expr->type = EXPR_VAR_ID;
                    expr->expr_data.id = field_copy;
                    return semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
                }
            }
            else
            {
                free(field_id_copy);
            }
        }
        /* Scoped enum support: TEnumType.EnumValue
         * When unit_check is found and it's a type with an enum, look up the field_id
         * as an enum literal and transform to its ordinal constant. */
        else if (unit_check != NULL && unit_check->hash_type == HASHTYPE_TYPE)
        {
            /* Check if the type has an enum type alias - look up field_id as enum literal */
            struct TypeAlias *type_alias = hashnode_get_type_alias(unit_check);
            if (type_alias != NULL && type_alias->is_enum && type_alias->enum_literals != NULL)
            {
                /* Search for field_id in enum_literals */
                int ordinal = 0;
                ListNode_t *literal_node = type_alias->enum_literals;
                while (literal_node != NULL)
                {
                    if (literal_node->cur != NULL)
                    {
                        char *literal_name = (char *)literal_node->cur;
                        if (strcasecmp(literal_name, field_id) == 0)
                        {
                            /* Found the enum literal - transform to integer constant */
                            expr->type = EXPR_INUM;
                            expr->expr_data.i_num = ordinal;
                            expr->resolved_type = ENUM_TYPE;
                            if (type_alias->kgpc_type != NULL)
                            {
                                /* Use the shared type setter to properly manage reference counting */
                                semcheck_expr_set_resolved_kgpc_type_shared(expr, type_alias->kgpc_type);
                            }
                            *type_return = ENUM_TYPE;
                            return 0;
                        }
                    }
                    ++ordinal;
                    literal_node = literal_node->next;
                }
                /* Enum literal not found in this type */
                fprintf(stderr, "Error on line %d, '%s' is not a value of enum type '%s'.\n\n",
                    expr->line_num, field_id, unit_id);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
        }
    }

    int error_count = 0;
    int record_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&record_type, symtab, record_expr, max_scope_lev, mutating);

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_recordaccess: field_id=%s, record_type=%d, record_expr->record_type=%p\n",
            field_id, record_type, record_expr->record_type);
    }

    struct RecordType *record_info = NULL;
    if (record_type == RECORD_TYPE)
    {
        record_info = record_expr->record_type;
        /* Fallback: infer from resolved KgpcType when legacy record_type is missing */
        if (record_info == NULL && record_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_record(record_expr->resolved_kgpc_type)) {
            record_info = kgpc_type_get_record(record_expr->resolved_kgpc_type);
        }
        if (record_info == NULL && record_expr->type == EXPR_TYPECAST)
        {
            const char *target_id = record_expr->expr_data.typecast_data.target_type_id;
            if (target_id != NULL)
            {
                HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type(symtab, target_id);
                if (type_node == NULL)
                    FindIdent(&type_node, symtab, (char *)target_id);
                if (type_node != NULL)
                {
                    record_info = get_record_type_from_node(type_node);
                    if (record_info == NULL && type_node->type != NULL &&
                        kgpc_type_is_record(type_node->type))
                    {
                        record_info = kgpc_type_get_record(type_node->type);
                    }
                    record_expr->record_type = record_info;
                    if (record_expr->resolved_kgpc_type == NULL && type_node->type != NULL)
                    {
                        kgpc_type_retain(type_node->type);
                        record_expr->resolved_kgpc_type = type_node->type;
                    }
                }
            }
        }
    }
    else if (record_type == POINTER_TYPE)
    {
        record_info = record_expr->record_type;
        if (record_info == NULL && record_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, record_expr->pointer_subtype_id) != -1 &&
                target_node != NULL)
            {
                record_info = get_record_type_from_node(target_node);
            }
        }
        /* Try resolved KgpcType pointer target */
        if (record_info == NULL && record_expr->resolved_kgpc_type != NULL &&
            record_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER) {
            KgpcType *pointee = record_expr->resolved_kgpc_type->info.points_to;
            if (pointee != NULL && kgpc_type_is_record(pointee)) {
                record_info = kgpc_type_get_record(pointee);
            }
        }

        if (record_info == NULL)
        {
            fprintf(stderr, "Error on line %d, pointer does not reference a record type.\n\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return error_count + 1;
        }
    }
    else
    {
        const char *expr_type_name = get_expr_type_name(record_expr, symtab);
        const char *alias_type_name = NULL;
        if (record_expr->resolved_kgpc_type != NULL &&
            record_expr->resolved_kgpc_type->type_alias != NULL &&
            record_expr->resolved_kgpc_type->type_alias->target_type_id != NULL)
        {
            alias_type_name = record_expr->resolved_kgpc_type->type_alias->target_type_id;
        }
        struct RecordType *helper_record = semcheck_lookup_type_helper(symtab, record_type,
            alias_type_name != NULL ? alias_type_name : expr_type_name);
        if (helper_record != NULL)
        {
            record_type = RECORD_TYPE;
            record_info = helper_record;
        }
        else
        {
            fprintf(stderr, "Error on line %d, field access requires a record value.\n\n", expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return error_count + 1;
        }
    }

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_recordaccess: record_info=%p, is_class=%d\n",
            record_info, record_info ? record_type_is_class(record_info) : -1);
    }

    if (record_info == NULL)
    {
        fprintf(stderr, "Error on line %d, unable to resolve record type for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    struct RecordField *field_desc = NULL;
    long long field_offset = 0;
    int property_matched = 0;
    /* For classes and records with potential methods, use silent mode when looking for fields,
     * since we'll check properties and methods next */
    int silent_mode = 1;  /* Always use silent mode - we'll print a better error later if needed */
    if (resolve_record_field(symtab, record_info, field_id, &field_desc,
            &field_offset, expr->line_num, silent_mode) != 0 || field_desc == NULL)
    {
        if (record_type_is_class(record_info))
        {
            struct RecordType *property_owner = NULL;
            struct ClassProperty *property = semcheck_find_class_property(symtab,
                record_info, field_id, &property_owner);
            if (property != NULL)
            {
                property_matched = 1;
                if (mutating == NO_MUTATE)
                {
                    if (property->read_accessor == NULL)
                    {
                        fprintf(stderr, "Error on line %d, property %s is write-only.\n\n",
                            expr->line_num, property->name != NULL ? property->name : field_id);
                        *type_return = UNKNOWN_TYPE;
                        return error_count + 1;
                    }

                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck]   Property read_accessor='%s'\n",
                            property->read_accessor ? property->read_accessor : "<null>");
                    }

                    struct RecordField *read_field =
                        semcheck_find_class_field_including_hidden(symtab,
                            record_info, property->read_accessor, NULL);
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck]   Found read_field=%p\n", read_field);
                    }
                    if (read_field != NULL &&
                        resolve_record_field(symtab, record_info, property->read_accessor,
                            &field_desc, &field_offset, expr->line_num, 0) == 0 &&
                        field_desc != NULL)
                    {
                        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                            fprintf(stderr, "[SemCheck]   Transforming property '%s' to field '%s'\n",
                                field_id, property->read_accessor);
                        }
                        if (!pascal_identifier_equals(field_id, property->read_accessor))
                        {
                            free(expr->expr_data.record_access_data.field_id);
                            expr->expr_data.record_access_data.field_id = strdup(property->read_accessor);
                            if (expr->expr_data.record_access_data.field_id == NULL)
                            {
                                fprintf(stderr, "Error on line %d, failed to allocate property field name.\n\n",
                                    expr->line_num);
                                *type_return = UNKNOWN_TYPE;
                                return error_count + 1;
                            }
                        }
                        goto FIELD_RESOLVED;
                    }

                    HashNode_t *getter_node = semcheck_find_class_method(symtab,
                        property_owner, property->read_accessor, NULL);
                    if (getter_node == NULL)
                    {
                        fprintf(stderr, "Error on line %d, getter %s for property %s not found.\n\n",
                            expr->line_num,
                            property->read_accessor != NULL ? property->read_accessor : "<unknown>",
                            property->name != NULL ? property->name : field_id);
                        *type_return = UNKNOWN_TYPE;
                        return error_count + 1;
                    }
                    if (getter_node->hash_type != HASHTYPE_FUNCTION)
                    {
                        fprintf(stderr, "Error on line %d, property getter %s must be a function.\n\n",
                            expr->line_num, property->read_accessor);
                        *type_return = UNKNOWN_TYPE;
                        return error_count + 1;
                    }

                    return semcheck_transform_property_getter_call(type_return, symtab,
                        expr, max_scope_lev, mutating, getter_node);
                }
                else
                {
                    if (property->write_accessor == NULL)
                    {
                        fprintf(stderr, "Error on line %d, property %s is read-only.\n\n",
                            expr->line_num, property->name != NULL ? property->name : field_id);
                        *type_return = UNKNOWN_TYPE;
                        return error_count + 1;
                    }

                    struct RecordField *write_field =
                        semcheck_find_class_field_including_hidden(symtab,
                            record_info, property->write_accessor, NULL);
                    if (write_field != NULL &&
                        resolve_record_field(symtab, record_info, property->write_accessor,
                            &field_desc, &field_offset, expr->line_num, 0) == 0 &&
                        field_desc != NULL)
                    {
                        if (!pascal_identifier_equals(field_id, property->write_accessor))
                        {
                            free(expr->expr_data.record_access_data.field_id);
                            expr->expr_data.record_access_data.field_id = strdup(property->write_accessor);
                            if (expr->expr_data.record_access_data.field_id == NULL)
                            {
                                fprintf(stderr, "Error on line %d, failed to allocate property field name.\n\n",
                                    expr->line_num);
                                *type_return = UNKNOWN_TYPE;
                                return error_count + 1;
                            }
                        }
                        goto FIELD_RESOLVED;
                    }

                    if (mutating == BOTH_MUTATE_REFERENCE)
                    {
                        fprintf(stderr, "Error on line %d, property %s cannot be passed as a var parameter.\n\n",
                            expr->line_num, property->name != NULL ? property->name : field_id);
                        *type_return = UNKNOWN_TYPE;
                        return error_count + 1;
                    }

                    HashNode_t *setter_node = semcheck_find_class_method(symtab,
                        property_owner, property->write_accessor, NULL);
                    if (setter_node == NULL)
                    {
                        fprintf(stderr, "Error on line %d, setter %s for property %s not found.\n\n",
                            expr->line_num,
                            property->write_accessor != NULL ? property->write_accessor : "<unknown>",
                            property->name != NULL ? property->name : field_id);
                        *type_return = UNKNOWN_TYPE;
                        return error_count + 1;
                    }
                    if (setter_node->hash_type != HASHTYPE_PROCEDURE)
                    {
                        fprintf(stderr, "Error on line %d, property setter %s must be a procedure.\n\n",
                            expr->line_num, property->write_accessor);
                        *type_return = UNKNOWN_TYPE;
                        return error_count + 1;
                    }

                    int property_type = UNKNOWN_TYPE;
                    struct RecordType *property_record = NULL;
                    if (semcheck_property_type_info(symtab, property, expr->line_num,
                            &property_type, &property_record) != 0)
                    {
                        *type_return = UNKNOWN_TYPE;
                        return error_count + 1;
                    }

                    expr->record_type = (property_type == RECORD_TYPE) ? property_record : NULL;
                    expr->resolved_type = property_type;
                    *type_return = property_type;
                    return error_count;
                }

                return error_count;
            }

            /* Check for methods (including constructors) */
            HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, field_id, NULL);
            if (method_node != NULL)
            {
                /* Found a method/constructor */
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_recordaccess: Found method %s\n", field_id);
                }
                
                if (method_node->hash_type == HASHTYPE_FUNCTION || 
                    method_node->hash_type == HASHTYPE_PROCEDURE)
                {
                    int is_static_method = 0;
                    if (record_info->type_id != NULL && field_id != NULL) {
                        is_static_method = from_cparser_is_method_static(record_info->type_id, field_id);
                    }

                    /* Transform record access into an explicit method call: receiver.Method() */
                    char *method_id = (field_id != NULL) ? strdup(field_id) : NULL;

                    expr->type = EXPR_FUNCTION_CALL;
                    memset(&expr->expr_data.function_call_data, 0,
                        sizeof(expr->expr_data.function_call_data));
                    expr->expr_data.function_call_data.id = method_id;
                    if (method_node->mangled_id != NULL)
                        expr->expr_data.function_call_data.mangled_id =
                            strdup(method_node->mangled_id);
                    else if (method_id != NULL)
                        expr->expr_data.function_call_data.mangled_id = strdup(method_id);
                    expr->expr_data.function_call_data.resolved_func = method_node;

                    if (is_static_method) {
                        expr->expr_data.function_call_data.args_expr = NULL;
                    } else {
                        struct Expression *receiver = record_expr;
                        ListNode_t *arg_node = CreateListNode(receiver, LIST_EXPR);
                        expr->expr_data.function_call_data.args_expr = arg_node;
                    }

                    /* Re-run semantic checking as a function call */
                    expr->record_type = NULL;
                    expr->resolved_type = UNKNOWN_TYPE;
                    return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                }
            }
        }

        if (property_matched)
            return error_count;

        /* Special handling for default Create constructor */
        /* If the field is "Create" and this is a class type, treat it as a default constructor */
        if (record_type_is_class(record_info) && field_id != NULL && 
            pascal_identifier_equals(field_id, "Create"))
        {
            /* Transform this EXPR_RECORD_ACCESS into EXPR_FUNCTION_CALL */
            /* The record_expr could be either:
             * 1. A type name (EXPR_VAR_ID) like TMyClass.Create - use static VMT
             * 2. A class reference variable like ClassRef.Create - use variable's value as VMT
             */
            if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
            {
                const char *expr_name = record_expr->expr_data.id;
                
                /* Check if this is a type name or a variable name */
                HashNode_t *ident_node = NULL;
                int is_type = 0;
                if (FindIdent(&ident_node, symtab, (char*)expr_name) >= 0 && ident_node != NULL) {
                    is_type = (ident_node->hash_type == HASHTYPE_TYPE);
                }
                
                /* Clean up the old record_access_data before transforming */
                expr->expr_data.record_access_data.record_expr = NULL;
                
                /* Calculate class size using KgpcType */
                KgpcType *record_kgpc = create_record_type(record_info);
                if (record_kgpc == NULL) {
                    fprintf(stderr, "Error on line %d: Unable to create KgpcType for class %s\n", 
                        expr->line_num, expr_name);
                    destroy_expr(record_expr);
                    return error_count + 1;
                }
                
                long long class_size = kgpc_type_sizeof(record_kgpc);
                if (class_size <= 0) {
                    fprintf(stderr, "Error on line %d: Unable to determine size for class %s\n", 
                        expr->line_num, expr_name);
                    destroy_expr(record_expr);
                    return error_count + 1;
                }
                
                /* Create argument 1: class size as integer literal */
                struct Expression *size_arg = (struct Expression *)calloc(1, sizeof(struct Expression));
                size_arg->type = EXPR_INUM;
                size_arg->expr_data.i_num = class_size;
                size_arg->resolved_type = INT_TYPE;
                
                /* Create argument 2: VMT pointer */
                struct Expression *vmt_arg;
                if (is_type) {
                    /* Static class type: use address of global VMT label */
                    vmt_arg = (struct Expression *)calloc(1, sizeof(struct Expression));
                    vmt_arg->type = EXPR_VAR_ID;
                    char vmt_label[256];
                    snprintf(vmt_label, sizeof(vmt_label), "%s_VMT", expr_name);
                    vmt_arg->expr_data.id = strdup(vmt_label);
                    vmt_arg->resolved_type = POINTER_TYPE;
                } else {
                    /* Class reference variable: use the variable's value as VMT */
                    /* The variable already holds a pointer to the VMT */
                    vmt_arg = record_expr;  /* Reuse the record_expr directly */
                    record_expr = NULL;     /* Prevent double-free */
                }
                
                /* Create argument list */
                ListNode_t *arg1_node = CreateListNode(size_arg, LIST_EXPR);
                ListNode_t *arg2_node = CreateListNode(vmt_arg, LIST_EXPR);
                arg1_node->next = arg2_node;
                
                /* Transform the expression into a function call to __kgpc_default_create */
                expr->type = EXPR_FUNCTION_CALL;
                /* Initialize function_call_data - use memset to clear the union */
                memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
                expr->expr_data.function_call_data.id = strdup("__kgpc_default_create");
                expr->expr_data.function_call_data.mangled_id = strdup("__kgpc_default_create");
                expr->expr_data.function_call_data.args_expr = arg1_node;
                
                /* Set the return type information */
                expr->record_type = record_info;
                expr->resolved_type = POINTER_TYPE;
                
                /* Create a KgpcType for the class (pointer to record) */
                KgpcType *class_kgpc = create_pointer_type(record_kgpc);
                semcheck_expr_set_resolved_kgpc_type_shared(expr, class_kgpc);
                
                *type_return = POINTER_TYPE;
                
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_recordaccess: Transformed '%s.%s' to __kgpc_default_create(%lld, %s) call\n",
                        expr_name, field_id, class_size, is_type ? "(static VMT)" : "(runtime VMT)");
                }
                
                /* Free the record_expr only if we didn't reuse it for vmt_arg */
                if (record_expr != NULL)
                    destroy_expr(record_expr);
                
                return error_count;
            }
        }

        /* Check for methods on non-class records (advanced records) */
        /* This handles {$modeswitch advancedrecords} style record methods */
        /* Unlike classes, advanced records don't use VMT but still have methods
         * registered with mangled names (TypeName__MethodName) in the symbol table */
        if (record_info != NULL && !record_type_is_class(record_info))
        {
            HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, field_id, NULL);
            if (method_node != NULL)
            {
                /* Found a method on an advanced record */
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_recordaccess: Found advanced record method %s\n", field_id);
                }

                if (method_node->hash_type == HASHTYPE_FUNCTION ||
                    method_node->hash_type == HASHTYPE_PROCEDURE)
                {
                    /* Check if this is a static method (no Self parameter) */
                    const char *type_name = record_info->type_id;
                    int is_static_method = 0;
                    if (type_name != NULL && field_id != NULL) {
                        is_static_method = from_cparser_is_method_static(type_name, field_id);
                    }
                    
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck] semcheck_recordaccess: type=%s method=%s is_static=%d\n",
                            type_name ? type_name : "<null>", field_id, is_static_method);
                    }
                    
                    /* Transform record access into an explicit method call */
                    char *method_id = (field_id != NULL) ? strdup(field_id) : NULL;

                    expr->type = EXPR_FUNCTION_CALL;
                    memset(&expr->expr_data.function_call_data, 0,
                        sizeof(expr->expr_data.function_call_data));
                    expr->expr_data.function_call_data.id = method_id;
                    if (method_node->mangled_id != NULL)
                        expr->expr_data.function_call_data.mangled_id =
                            strdup(method_node->mangled_id);
                    else if (method_id != NULL)
                        expr->expr_data.function_call_data.mangled_id = strdup(method_id);
                    expr->expr_data.function_call_data.resolved_func = method_node;
                    semcheck_expr_set_call_kgpc_type(expr, method_node->type, 0);

                    /* For static methods, don't pass a receiver/Self */
                    if (is_static_method) {
                        expr->expr_data.function_call_data.args_expr = NULL;
                    } else {
                        /* For instance methods, pass receiver as first argument (Self) */
                        struct Expression *receiver = record_expr;
                        ListNode_t *arg_node = CreateListNode(receiver, LIST_EXPR);
                        expr->expr_data.function_call_data.args_expr = arg_node;
                    }

                    /* Re-run semantic checking as a function call */
                    expr->record_type = NULL;
                    expr->resolved_type = UNKNOWN_TYPE;
                    return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                }
            }
        }

        if (record_info != NULL && record_info->is_type_helper && record_info->type_id != NULL)
        {
            char *mangled_const = semcheck_mangle_helper_const_id(record_info->type_id, field_id);
            HashNode_t *const_node = NULL;
            if (mangled_const != NULL &&
                FindIdent(&const_node, symtab, mangled_const) == 0 &&
                const_node != NULL && const_node->hash_type == HASHTYPE_CONST)
            {
                destroy_expr(record_expr);
                expr->expr_data.record_access_data.record_expr = NULL;
                if (expr->expr_data.record_access_data.field_id != NULL)
                {
                    free(expr->expr_data.record_access_data.field_id);
                    expr->expr_data.record_access_data.field_id = NULL;
                }
                expr->type = EXPR_VAR_ID;
                expr->expr_data.id = mangled_const;
                return semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
            }
            if (mangled_const != NULL)
                free(mangled_const);
        }

        fprintf(stderr, "Error on line %d, record field %s not found.\n", expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

FIELD_RESOLVED:
    expr->expr_data.record_access_data.field_offset = field_offset;

    expr->expr_data.record_access_data.field_offset = field_offset;

    int field_type = field_desc->type;
    struct RecordType *field_record = field_desc->nested_record;
    if (field_record != NULL)
        field_type = RECORD_TYPE;

    if (field_desc->is_array)
    {
        semcheck_clear_array_info(expr);
        expr->is_array_expr = 1;
        expr->array_lower_bound = field_desc->array_start;
        expr->array_upper_bound = field_desc->array_end;
        expr->array_is_dynamic = field_desc->array_is_open;
        expr->array_element_type = field_desc->array_element_type;
        if (field_desc->array_element_type_id != NULL)
        {
            expr->array_element_type_id = strdup(field_desc->array_element_type_id);
            if (expr->array_element_type == UNKNOWN_TYPE)
            {
                int resolved_type = UNKNOWN_TYPE;
                if (resolve_type_identifier(&resolved_type, symtab, expr->array_element_type_id,
                        expr->line_num) == 0)
                    expr->array_element_type = resolved_type;
            }
            expr->array_element_record_type = semcheck_lookup_record_type(symtab,
                field_desc->array_element_type_id);
        }
        else if (expr->array_element_type == RECORD_TYPE)
        {
            expr->array_element_record_type = field_record;
        }

        long long computed_size = 0;
        int size_status = 1;
        if (expr->array_element_record_type != NULL)
            size_status = sizeof_from_record(symtab, expr->array_element_record_type,
                &computed_size, 0, expr->line_num);
        else if (expr->array_element_type != UNKNOWN_TYPE ||
            expr->array_element_type_id != NULL)
            size_status = sizeof_from_type_ref(symtab, expr->array_element_type,
                expr->array_element_type_id, &computed_size, 0, expr->line_num);
        if (size_status == 0 && computed_size > 0 && computed_size <= INT_MAX)
            expr->array_element_size = (int)computed_size;

        if (expr->array_element_type != UNKNOWN_TYPE)
            field_type = expr->array_element_type;
        if (expr->array_element_record_type != NULL && field_type == RECORD_TYPE)
            field_record = expr->array_element_record_type;
    }

    struct TypeAlias *array_alias = NULL;

    if (field_desc->type_id != NULL)
    {
        int resolved_type = field_type;
        if (resolve_type_identifier(&resolved_type, symtab, field_desc->type_id, expr->line_num) != 0)
            ++error_count;
        field_type = resolved_type;

        HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, field_desc->type_id);
        if (type_node == NULL)
            type_node = semcheck_find_type_node_with_kgpc_type(symtab, field_desc->type_id);

        if (type_node != NULL)
        {
            struct RecordType *record_type = get_record_type_from_node(type_node);
            if (record_type != NULL)
                field_record = record_type;
            else
            {
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->target_type_id != NULL)
                {
                    HashNode_t *target_node =
                        semcheck_find_preferred_type_node(symtab, alias->target_type_id);
                    if (target_node != NULL)
                        field_record = get_record_type_from_node(target_node);
                }
            }

            struct TypeAlias *alias = get_type_alias_from_node(type_node);
            if (alias != NULL && alias->is_array)
                array_alias = alias;
        }

        if (field_record != NULL && field_type == UNKNOWN_TYPE)
            field_type = RECORD_TYPE;
    }

    if (field_type == UNKNOWN_TYPE && field_record == NULL)
    {
        fprintf(stderr, "Error on line %d, unable to resolve type for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    if (field_type == RECORD_TYPE && field_record == NULL)
    {
        fprintf(stderr, "Error on line %d, missing record definition for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    if (array_alias != NULL)
        semcheck_set_array_info_from_alias(expr, symtab, array_alias, expr->line_num);

    expr->record_type = (field_type == RECORD_TYPE) ? field_record : NULL;
    *type_return = field_type;
    return error_count;
}

static int semcheck_addressof(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;

    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_ADDR);

    semcheck_clear_pointer_info(expr);

    struct Expression *inner = expr->expr_data.addr_data.expr;
    if (inner == NULL)
    {
        fprintf(stderr, "Error on line %d, address-of operator requires an operand.\\n\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int error_count = 0;
    int inner_type = UNKNOWN_TYPE;
    int treated_as_proc_ref = 0;

    /* If operand is a bare function/procedure identifier, don't auto-convert to a call */
    if (inner->type == EXPR_VAR_ID && inner->expr_data.id != NULL)
    {
        HashNode_t *inner_symbol = NULL;
        if (FindIdent(&inner_symbol, symtab, inner->expr_data.id) == 0 &&
            inner_symbol != NULL &&
            (inner_symbol->hash_type == HASHTYPE_FUNCTION || inner_symbol->hash_type == HASHTYPE_PROCEDURE))
        {
            inner_type = PROCEDURE;
            treated_as_proc_ref = 1;
        }
    }

    if (!treated_as_proc_ref)
        semcheck_expr_main(&inner_type, symtab, inner, max_scope_lev, NO_MUTATE);

    /* Special case: If the inner expression was auto-converted from a function identifier
     * to a function call (because we're in NO_MUTATE mode), we need to reverse that
     * since we're taking the address of the function, not calling it. */
    int converted_to_proc_addr = 0;  /* Track if we successfully convert to procedure address */
    if (inner->type == EXPR_FUNCTION_CALL && 
        inner->expr_data.function_call_data.args_expr == NULL)
    {
        const char *func_id = inner->expr_data.function_call_data.id;
        if (func_id != NULL)
        {
            HashNode_t *func_symbol = NULL;
            if (FindIdent(&func_symbol, symtab, (char *)func_id) >= 0 &&
                func_symbol != NULL && 
                (func_symbol->hash_type == HASHTYPE_FUNCTION || func_symbol->hash_type == HASHTYPE_PROCEDURE))
            {
                /* This was auto-converted - treat it as a procedure reference instead */
                inner_type = PROCEDURE;
                converted_to_proc_addr = 1;
                /* We'll handle this below in the PROCEDURE case */
            }
        }
    }

    if (inner_type == UNKNOWN_TYPE)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    const char *type_id = NULL;
    if (inner_type == POINTER_TYPE && inner->pointer_subtype_id != NULL)
        type_id = inner->pointer_subtype_id;

    struct RecordType *record_info = NULL;
    if (inner_type == RECORD_TYPE)
        record_info = inner->record_type;
    else if (inner_type == POINTER_TYPE && inner->record_type != NULL)
        record_info = inner->record_type;

    semcheck_set_pointer_info(expr, inner_type, type_id);
    expr->record_type = record_info;
    *type_return = POINTER_TYPE;
    
    /* Create a proper KgpcType for the address-of expression */
    KgpcType *pointed_to_type = NULL;
    
    /* Convert inner_type to KgpcType */
    if (inner_type == INT_TYPE) {
        pointed_to_type = create_primitive_type(INT_TYPE);
    } else if (inner_type == LONGINT_TYPE) {
        pointed_to_type = create_primitive_type(LONGINT_TYPE);
    } else if (inner_type == REAL_TYPE) {
        pointed_to_type = create_primitive_type(REAL_TYPE);
    } else if (inner_type == CHAR_TYPE) {
        pointed_to_type = create_primitive_type(CHAR_TYPE);
    } else if (inner_type == STRING_TYPE) {
        pointed_to_type = create_primitive_type(STRING_TYPE);
    } else if (inner_type == RECORD_TYPE && record_info != NULL) {
        pointed_to_type = create_record_type(record_info);
    } else if (inner_type == POINTER_TYPE) {
        /* For pointer types, get the resolved KgpcType of the inner expression */
        if (inner->resolved_kgpc_type != NULL) {
            pointed_to_type = inner->resolved_kgpc_type;
            kgpc_type_retain(pointed_to_type);  /* We're taking a reference */
        } else {
            /* Fallback: create untyped pointer */
            pointed_to_type = NULL;
        }
    } else if (inner_type == PROCEDURE) {
        int proc_type_owned = 0;
        KgpcType *proc_type = NULL;
        
        /* For procedures/functions, we need the actual procedural type, not the return type.
         * semcheck_resolve_expression_kgpc_type returns the return type for functions,
         * so we look up the symbol directly instead. */
        const char *proc_id = NULL;
        if (inner->type == EXPR_VAR_ID)
        {
            proc_id = inner->expr_data.id;
        }
        else if (inner->type == EXPR_FUNCTION_CALL && inner->expr_data.function_call_data.args_expr == NULL)
        {
            proc_id = inner->expr_data.function_call_data.id;
        }
        
        if (proc_id != NULL)
        {
            HashNode_t *proc_symbol = NULL;
            if (FindIdent(&proc_symbol, symtab, (char *)proc_id) >= 0 &&
                proc_symbol != NULL && 
                (proc_symbol->hash_type == HASHTYPE_PROCEDURE || proc_symbol->hash_type == HASHTYPE_FUNCTION) &&
                proc_symbol->type != NULL && proc_symbol->type->kind == TYPE_KIND_PROCEDURE)
            {
                /* Use the procedure type from the symbol */
                proc_type = proc_symbol->type;
                proc_type_owned = 0; /* Shared reference */
            }
        }
        
        if (proc_type != NULL)
        {
            if (!proc_type_owned)
                kgpc_type_retain(proc_type);
            pointed_to_type = proc_type;
        }

        /* Handle both EXPR_VAR_ID (for procedures) and EXPR_FUNCTION_CALL (for functions that were auto-converted) */
        if (inner->type == EXPR_VAR_ID)
        {
            HashNode_t *proc_symbol = NULL;
            if (FindIdent(&proc_symbol, symtab, inner->expr_data.id) >= 0 &&
                proc_symbol != NULL && 
                (proc_symbol->hash_type == HASHTYPE_PROCEDURE || proc_symbol->hash_type == HASHTYPE_FUNCTION))
            {
                expr->expr_data.addr_data.expr = NULL;
                destroy_expr(inner);
                expr->type = EXPR_ADDR_OF_PROC;
                expr->expr_data.addr_of_proc_data.procedure_symbol = proc_symbol;
            }
        }
        else if (inner->type == EXPR_FUNCTION_CALL && inner->expr_data.function_call_data.args_expr == NULL)
        {
            /* This was auto-converted from a function identifier - get the original symbol */
            const char *func_id = inner->expr_data.function_call_data.id;
            if (func_id != NULL)
            {
                HashNode_t *proc_symbol = NULL;
                if (FindIdent(&proc_symbol, symtab, (char *)func_id) >= 0 &&
                    proc_symbol != NULL && 
                    (proc_symbol->hash_type == HASHTYPE_FUNCTION || proc_symbol->hash_type == HASHTYPE_PROCEDURE))
                {
                    expr->expr_data.addr_data.expr = NULL;
                    destroy_expr(inner);
                    expr->type = EXPR_ADDR_OF_PROC;
                    expr->expr_data.addr_of_proc_data.procedure_symbol = proc_symbol;
                }
            }
        }
    }
    /* For other types, we could add more conversions here */
    
    /* Create the pointer type */
    if (pointed_to_type != NULL) {
        if (expr->resolved_kgpc_type != NULL) {
            destroy_kgpc_type(expr->resolved_kgpc_type);
        }
        expr->resolved_kgpc_type = create_pointer_type(pointed_to_type);
    }
    
    /* If we successfully converted to a procedure address, don't count inner expression errors.
     * Those errors were from trying to call the function with no arguments, which is not what we want. */
    if (converted_to_proc_addr && expr->type == EXPR_ADDR_OF_PROC)
    {
        return 0;  /* Success - ignore inner errors */
    }
    
    return error_count;
}
/* Sets a type based on a hash_type - uses KgpcType when available */
int set_type_from_hashtype(int *type, HashNode_t *hash_node)
{
    assert(type != NULL);
    assert(hash_node != NULL);

    /* Try KgpcType first if available */
    if (hash_node->type != NULL)
    {
        if (hash_node->type->kind == TYPE_KIND_PRIMITIVE)
        {
            *type = kgpc_type_get_primitive_tag(hash_node->type);
            return 0;
        }
        else if (hash_node->type->kind == TYPE_KIND_POINTER)
        {
            *type = POINTER_TYPE;
            return 0;
        }
        else if (hash_node->type->kind == TYPE_KIND_RECORD)
        {
            *type = RECORD_TYPE;
            return 0;
        }
        else if (hash_node->type->kind == TYPE_KIND_PROCEDURE)
        {
            /* When this is a variable/const with a procedural type (not a function/procedure definition),
             * we return PROCEDURE to indicate it's a procedure variable.
             * Only for actual function/procedure definitions (HASHTYPE_FUNCTION/HASHTYPE_PROCEDURE)
             * do we return the function's return type. */
            if (hash_node->hash_type == HASHTYPE_FUNCTION || hash_node->hash_type == HASHTYPE_PROCEDURE)
            {
                /* This is an actual function/procedure definition - return its return type */
                KgpcType *return_type = kgpc_type_get_return_type(hash_node->type);
                if (return_type != NULL)
                {
                    if (return_type->kind == TYPE_KIND_PRIMITIVE)
                    {
                        *type = kgpc_type_get_primitive_tag(return_type);
                        return 0;
                    }
                    else if (return_type->kind == TYPE_KIND_RECORD)
                    {
                        *type = RECORD_TYPE;
                        return 0;
                    }
                    else if (return_type->kind == TYPE_KIND_POINTER)
                    {
                        *type = POINTER_TYPE;
                        return 0;
                    }
                    else if (return_type->kind == TYPE_KIND_ARRAY)
                    {
                        KgpcType *elem_type = return_type->info.array_info.element_type;
                        if (elem_type != NULL && elem_type->kind == TYPE_KIND_PRIMITIVE &&
                            elem_type->info.primitive_type_tag == CHAR_TYPE)
                        {
                            *type = SHORTSTRING_TYPE;
                            return 0;
                        }
                        *type = UNKNOWN_TYPE;
                        return 0;
                    }
                    /* Add other return type kinds as needed */
                    *type = UNKNOWN_TYPE;
                    return 0;
                }
            }
            /* For procedure variables/constants or procedures with no return type */
            *type = PROCEDURE;
            return 0;
        }
        else if (hash_node->type->kind == TYPE_KIND_ARRAY)
        {
            /* For arrays, return the element type's primitive tag if available */
            KgpcType *elem_type = hash_node->type->info.array_info.element_type;
            if (elem_type != NULL && elem_type->kind == TYPE_KIND_PRIMITIVE)
            {
                *type = kgpc_type_get_primitive_tag(elem_type);
                return 0;
            }
            if (elem_type != NULL && elem_type->kind == TYPE_KIND_POINTER)
            {
                *type = POINTER_TYPE;
                return 0;
            }
            if (elem_type != NULL && elem_type->kind == TYPE_KIND_RECORD)
            {
                *type = RECORD_TYPE;
                return 0;
            }
            *type = UNKNOWN_TYPE;
            return 0;
        }
        *type = UNKNOWN_TYPE;
        return 0;
    }

    return 0;
}

/* Semantic check on a normal expression */
int semcheck_expr(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    assert(type_return != NULL);
    if (expr == NULL)
        return 0;

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] Checking expr type=%d\n", expr->type);
        if (expr->type == EXPR_VAR_ID) {
            fprintf(stderr, "[SemCheck]   Identifier: %s\n", expr->expr_data.id);
        }
    }

    return semcheck_expr_main(type_return, symtab, expr, max_scope_lev, mutating);
}

/* Semantic check on a function expression (no side effects allowed) */
int semcheck_expr_func(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int mutating)
{
    assert(type_return != NULL);
    return semcheck_expr_main(type_return, symtab, expr, 0, mutating);
}

/* Phase 3 Step 3: Resolve KgpcType from an expression
 * This function performs semantic checking and returns the KgpcType of the expression.
 * This is the bridge between the legacy int-based type system and the new KgpcType system.
 * 
 * Implementation strategy:
 * 1. For simple cases (var IDs, function calls), we can directly get KgpcType from symbol table
 * 2. For complex expressions, we use semcheck_expr_main to get the type tag, then convert to KgpcType
 * 3. We also check expr->resolved_kgpc_type if it was previously computed
 * 
 * Returns NULL if type cannot be resolved.
 * If owns_type is not NULL, it will be set to:
 *   - 0 if the caller does NOT own the type (shared reference from symbol table)
 *   - 1 if the caller OWNS the type (must free it with destroy_kgpc_type)
 */
KgpcType* semcheck_resolve_expression_kgpc_type(SymTab_t *symtab, struct Expression *expr,
    int max_scope_lev, int mutating, int *owns_type)
{
    if (symtab == NULL || expr == NULL)
        return NULL;
    
    /* Default: caller owns the type we create */
    if (owns_type != NULL)
        *owns_type = 1;
    
    /* Try to get KgpcType directly from the expression for specific cases */
    switch (expr->type)
    {
        case EXPR_VAR_ID:
        {
            /* For variable IDs, we can get the KgpcType directly from the symbol table */
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, expr->expr_data.id) != -1 && node != NULL)
            {
                if (node->hash_type == HASHTYPE_FUNCTION && mutating != NO_MUTATE &&
                    node->type != NULL)
                {
                    KgpcType *ret_type = kgpc_type_get_return_type(node->type);
                    if (ret_type != NULL)
                    {
                        if (owns_type != NULL)
                            *owns_type = 0;
                        return ret_type;
                    }
                }

                if (node->type != NULL)
                {
                    /* Return a shared reference - caller doesn't own it */
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return node->type;
                }
            }
            break;
        }
        
        case EXPR_FUNCTION_CALL:
        {
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr->expr_data.function_call_data.id != NULL &&
                strcasecmp(expr->expr_data.function_call_data.id, "Create") == 0) {
                fprintf(stderr, "[SemCheck] semcheck_resolve_expression_kgpc_type for Create:\n");
                fprintf(stderr, "[SemCheck]   expr=%p\n", (void*)expr);
                fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type=%p\n", (void*)expr->resolved_kgpc_type);
                if (expr->resolved_kgpc_type != NULL) {
                    fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type->kind=%d\n", expr->resolved_kgpc_type->kind);
                }
            }
            
            /* First, try the cached resolved_kgpc_type if available */
            if (expr->resolved_kgpc_type != NULL)
            {
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr->expr_data.function_call_data.id != NULL &&
                    strcasecmp(expr->expr_data.function_call_data.id, "Create") == 0) {
                    fprintf(stderr, "[SemCheck]   Returning cached resolved_kgpc_type\n");
                }
                if (owns_type != NULL)
                    *owns_type = 0;  /* Shared reference */
                return expr->resolved_kgpc_type;
            }
            
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr->expr_data.function_call_data.id != NULL &&
                strcasecmp(expr->expr_data.function_call_data.id, "Create") == 0) {
                fprintf(stderr, "[SemCheck]   resolved_kgpc_type is NULL, trying other paths\n");
            }
            
            /* Prefer cached call info populated during semantic checking */
            if (expr->expr_data.function_call_data.is_call_info_valid &&
                expr->expr_data.function_call_data.call_kgpc_type != NULL)
            {
                KgpcType *call_type = expr->expr_data.function_call_data.call_kgpc_type;
                if (call_type->kind == TYPE_KIND_PROCEDURE &&
                    call_type->info.proc_info.return_type != NULL)
                {
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return call_type->info.proc_info.return_type;
                }
            }

            if (expr->expr_data.function_call_data.id != NULL && symtab != NULL)
            {
                HashNode_t *func_node = NULL;
                if (FindIdent(&func_node, symtab,
                        expr->expr_data.function_call_data.id) >= 0 &&
                    func_node != NULL && func_node->type != NULL &&
                    func_node->type->kind == TYPE_KIND_PROCEDURE &&
                    func_node->type->info.proc_info.return_type != NULL)
                {
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return func_node->type->info.proc_info.return_type;
                }
            }
            break;
        }
        
        case EXPR_RECORD_ACCESS:
        {
            /* For record field access, we need to resolve the type of the record,
             * then look up the field's type within that record.
             */
            struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
            const char *field_name = expr->expr_data.record_access_data.field_id;
            
            if (record_expr != NULL && field_name != NULL)
            {
                /* Resolve the record expression's type */
                int record_type_owned = 0;
                KgpcType *record_type = semcheck_resolve_expression_kgpc_type(symtab, record_expr, 
                                                                          max_scope_lev, mutating, &record_type_owned);
                
                if (record_type != NULL && record_type->kind == TYPE_KIND_RECORD)
                {
                    /* Look up the field in the record type */
                    struct RecordType *record_info = record_type->info.record_info;
                    if (record_info != NULL)
                    {
                    ListNode_t *field_cursor = record_info->fields;
                    while (field_cursor != NULL)
                    {
                        if (field_cursor->type == LIST_RECORD_FIELD)
                        {
                            struct RecordField *field = (struct RecordField *)field_cursor->cur;
                            if (field != NULL && record_field_is_hidden(field))
                            {
                                field_cursor = field_cursor->next;
                                continue;
                            }
                            if (field != NULL)
                            {
                                if (field->name != NULL && strcmp(field->name, field_name) == 0)
                                {
                                    /* Found the field, resolve its type */
                                    KgpcType *field_type = NULL;
                                    
                                    if (field->type_id != NULL)
                                    {
                                        /* User-defined type - look up in symbol table */
                                        HashNode_t *type_node =
                                            semcheck_find_preferred_type_node(symtab, field->type_id);
                                        if (type_node != NULL)
                                        {
                                            if (owns_type != NULL)
                                                *owns_type = 0;
                                            field_type = type_node->type;
                                        }
                                    }
                                    else if (field->nested_record != NULL)
                                    {
                                        /* Nested record type */
                                        if (owns_type != NULL)
                                            *owns_type = 1;
                                        field_type = create_record_type(field->nested_record);
                                    }
                                    else if (field->is_array)
                                    {
                                        /* Array field */
                                        KgpcType *element_type = NULL;
                                        if (field->array_element_type_id != NULL)
                                        {
                                            HashNode_t *elem_type_node =
                                                semcheck_find_preferred_type_node(symtab, field->array_element_type_id);
                                            if (elem_type_node != NULL)
                                            {
                                                element_type = elem_type_node->type;
                                            }
                                        }
                                        else if (field->array_element_type != UNKNOWN_TYPE)
                                        {
                                            if (owns_type != NULL)
                                                *owns_type = 1;
                                            element_type = create_primitive_type(field->array_element_type);
                                        }
                                        
                                        if (element_type != NULL)
                                        {
                                            if (owns_type != NULL)
                                                *owns_type = 1;
                                            field_type = create_array_type(element_type, field->array_start, field->array_end);
                                        }
                                    }
                                    else
                                    {
                                        /* Primitive type */
                                        if (owns_type != NULL)
                                            *owns_type = 1;
                                        field_type = create_primitive_type(field->type);
                                    }
                                    
                                    /* Clean up record type if we owned it */
                                    if (record_type_owned)
                                        destroy_kgpc_type(record_type);
                                    
                                    /* Return the field type */
                                    return field_type;
                                }
                            }
                        }
                        field_cursor = field_cursor->next;
                    }
                    }
                }
                
                /* Clean up record type if we owned it */
                if (record_type_owned)
                    destroy_kgpc_type(record_type);
            }
            break;
        }

        case EXPR_ARRAY_ACCESS:
        {
            struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
            const char *alias_id = NULL;

            if (array_expr != NULL && array_expr->is_array_expr &&
                array_expr->array_element_type_id != NULL)
            {
                alias_id = array_expr->array_element_type_id;
            }
            else if (expr->array_element_type_id != NULL)
            {
                alias_id = expr->array_element_type_id;
            }

            if (alias_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] Resolving array element type alias: %s\n", alias_id);
                }
                if (FindIdent(&type_node, symtab, (char *)alias_id) != -1 &&
                    type_node != NULL && type_node->type != NULL)
                {
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return type_node->type;
                }
            }

            /* Handle pointer indexing result (e.g., p[i] where p: PPAnsiChar) */
            if (expr->pointer_subtype != UNKNOWN_TYPE || expr->pointer_subtype_id != NULL)
            {
                /* The result of array access is a pointer type */
                KgpcType *points_to = NULL;
                
                /* Try to get the pointed-to type from the symbol table */
                if (expr->pointer_subtype_id != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, (char *)expr->pointer_subtype_id) != -1 &&
                        type_node != NULL && type_node->type != NULL)
                    {
                        kgpc_type_retain(type_node->type);
                        points_to = type_node->type;
                    }
                }
                
                /* If not found, try to create from primitive type tag */
                if (points_to == NULL && expr->pointer_subtype != UNKNOWN_TYPE)
                {
                    points_to = create_primitive_type(expr->pointer_subtype);
                    /* Note: create_primitive_type uses assert and cannot return NULL,
                     * but we pass NULL to create_pointer_type below which is valid
                     * (creates a generic/untyped pointer). */
                }
                
                /* Create a pointer type pointing to the target.
                 * points_to can be NULL here, which creates an untyped pointer (like Pointer). */
                if (owns_type != NULL)
                    *owns_type = 1;
                return create_pointer_type(points_to);
            }

            if (expr->is_array_expr)
            {
                int start_index = expr->array_lower_bound;
                int end_index = expr->array_upper_bound;
                if (expr->array_is_dynamic)
                {
                    start_index = 0;
                    end_index = -1;
                }

                KgpcType *element_type = NULL;
                if (expr->array_element_record_type != NULL)
                {
                    element_type = create_record_type(expr->array_element_record_type);
                }
                else if (expr->array_element_type != UNKNOWN_TYPE)
                {
                    element_type = create_primitive_type(expr->array_element_type);
                }

                if (element_type != NULL)
                {
                    if (owns_type != NULL)
                        *owns_type = 1;
                    return create_array_type(element_type, start_index, end_index);
                }
            }
            break;
        }

        case EXPR_ARRAY_LITERAL:
        {
            if (expr->array_element_type == UNKNOWN_TYPE)
                break;

            KgpcType *element_type = create_primitive_type(expr->array_element_type);
            if (element_type == NULL)
                break;

            int end_index = expr->array_upper_bound;
            if (end_index < expr->array_lower_bound)
                end_index = expr->array_lower_bound - 1;

            KgpcType *array_type = create_array_type(element_type,
                expr->array_lower_bound, end_index);
            return array_type;
        }

        case EXPR_POINTER_DEREF:
        {
            /* For pointer dereference, resolve the pointer expression's type,
             * then return what it points to.
             */
            struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
            if (pointer_expr != NULL)
            {
                int ptr_type_owned = 0;
                KgpcType *ptr_type = semcheck_resolve_expression_kgpc_type(symtab, pointer_expr, 
                                                                          max_scope_lev, mutating, &ptr_type_owned);
                if (ptr_type != NULL && ptr_type->kind == TYPE_KIND_POINTER)
                {
                    KgpcType *deref_type = ptr_type->info.points_to;
                    
                    /* Clean up the pointer type if we owned it */
                    if (ptr_type_owned)
                        destroy_kgpc_type(ptr_type);
                    
                    /* Return what the pointer points to - caller doesn't own it (it's part of the pointer type) */
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return deref_type;
                }
                
                /* Clean up if we failed */
                if (ptr_type_owned && ptr_type != NULL)
                    destroy_kgpc_type(ptr_type);
            }
            break;
        }
        case EXPR_TYPECAST:
        {
            const char *target_id = expr->expr_data.typecast_data.target_type_id;
            if (target_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindIdent(&type_node, symtab, (char *)target_id) >= 0 &&
                    type_node != NULL && type_node->type != NULL &&
                    type_node->type->kind == TYPE_KIND_PROCEDURE)
                {
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return type_node->type;
                }
            }
            break;
        }
        
        default:
            break;
    }
    
    /* Check if the expression already has a resolved KgpcType (e.g., from semcheck_expr_main) */
    if (expr->resolved_kgpc_type != NULL)
    {
        /* Use the existing KgpcType - caller doesn't own it (it belongs to the expression) */
        if (owns_type != NULL)
            *owns_type = 0;
        return expr->resolved_kgpc_type;
    }
    
    /* For all other cases or if direct resolution failed, use semcheck_expr_main
     * to get the type tag, then convert to KgpcType */
    int type_tag = UNKNOWN_TYPE;
    int result = semcheck_expr_main(&type_tag, symtab, expr, max_scope_lev, mutating);
    
    if (result != 0 || type_tag == UNKNOWN_TYPE)
        return NULL;

    /* Check if semcheck_expr_main populated resolved_kgpc_type */
    if (expr->resolved_kgpc_type != NULL)
    {
        if (owns_type != NULL)
            *owns_type = 0;
        return expr->resolved_kgpc_type;
    }
    
    /* Create a KgpcType from the type tag - caller owns this */
    if (owns_type != NULL)
        *owns_type = 1;
    
    return create_primitive_type(type_tag);
}

/* Main semantic checking */
int semcheck_expr_main(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    if (expr != NULL && expr->type == EXPR_VAR_ID && getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_expr_main: Checking identifier: %s\n", expr->expr_data.id);
        HashNode_t *ident_node = NULL;
        int index = FindIdent(&ident_node, symtab, expr->expr_data.id);
        if (index == -1) {
            fprintf(stderr, "[SemCheck]   FindIdent failed\n");
        } else {
            fprintf(stderr, "[SemCheck]   Found identifier: %s (index %d)\n", ident_node->id, index);
        }
    }

    if (expr == NULL)
        return 0;

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_expr_main: expr type=%d\n", expr->type);
        if (expr->type == EXPR_VAR_ID) {
            fprintf(stderr, "[SemCheck]   Identifier: %s\n", expr->expr_data.id);
        }
    }

    int return_val;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(type_return != NULL);

    return_val = 0;
    expr->resolved_type = UNKNOWN_TYPE;
    switch(expr->type)
    {
        case EXPR_RELOP:
            return_val += semcheck_relop(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_SIGN_TERM:
            return_val += semcheck_signterm(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_ADDOP:
            return_val += semcheck_addop(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_MULOP:
            return_val += semcheck_mulop(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_VAR_ID:
            return_val += semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_ARRAY_ACCESS:
            return_val += semcheck_arrayaccess(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_RECORD_ACCESS:
            return_val += semcheck_recordaccess(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_FUNCTION_CALL:
            return_val += semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_POINTER_DEREF:
            return_val += semcheck_pointer_deref(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_ADDR_OF_PROC:
        {
            *type_return = POINTER_TYPE;
            expr->resolved_type = POINTER_TYPE;
            if (expr->resolved_kgpc_type == NULL)
            {
                KgpcType *proc_type = NULL;
                if (expr->expr_data.addr_of_proc_data.procedure_symbol != NULL &&
                    expr->expr_data.addr_of_proc_data.procedure_symbol->type != NULL)
                    proc_type = expr->expr_data.addr_of_proc_data.procedure_symbol->type;

                if (proc_type != NULL)
                {
                    kgpc_type_retain(proc_type);
                    expr->resolved_kgpc_type = create_pointer_type(proc_type);
                }
                else
                {
                    expr->resolved_kgpc_type = create_pointer_type(NULL);
                }
            }
            break;
        }
        case EXPR_ADDR:
            return_val += semcheck_addressof(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_TYPECAST:
            return_val += semcheck_typecast(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_IS:
            return_val += semcheck_is_expr(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_AS:
            return_val += semcheck_as_expr(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        /*** BASE CASES ***/
        case EXPR_INUM:
            if (expr->expr_data.i_num > INT_MAX || expr->expr_data.i_num < INT_MIN)
            {
                *type_return = INT64_TYPE;
                /* For values that don't fit in 32 bits, use INT64_TYPE (true 64-bit integer) */
                if (expr->resolved_kgpc_type != NULL)
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                expr->resolved_kgpc_type = create_primitive_type_with_size(INT64_TYPE, 8);
            }
            else
                *type_return = INT_TYPE;
            break;

        case EXPR_RNUM:
            *type_return = REAL_TYPE;
            break;

        case EXPR_STRING:
            *type_return = STRING_TYPE;
            break;

        case EXPR_CHAR_CODE:
            *type_return = CHAR_TYPE;
            break;

        case EXPR_BOOL:
            *type_return = BOOL;
            break;
        case EXPR_NIL:
            *type_return = POINTER_TYPE;
            semcheck_clear_pointer_info(expr);
            /* Create a proper KgpcType for nil with points_to = NULL */
            if (expr->resolved_kgpc_type != NULL) {
                destroy_kgpc_type(expr->resolved_kgpc_type);
            }
            expr->resolved_kgpc_type = create_pointer_type(NULL);
            break;
        case EXPR_SET:
            *type_return = SET_TYPE;
            break;

        case EXPR_ARRAY_LITERAL:
        {
            if (expr->array_element_type == UNKNOWN_TYPE &&
                expr->array_element_type_id == NULL)
            {
                fprintf(stderr, "Error on line %d, unable to infer type for array literal.\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            if (!expr->expr_data.array_literal_data.elements_semchecked)
            {
                int arr_err = semcheck_typecheck_array_literal(expr, symtab, max_scope_lev,
                    expr->array_element_type, expr->array_element_type_id, expr->line_num);
                if (arr_err != 0)
                {
                    *type_return = UNKNOWN_TYPE;
                    return arr_err;
                }
            }
            expr->is_array_expr = 1;
            expr->array_is_dynamic = 1;
            expr->resolved_type = POINTER_TYPE;
            *type_return = POINTER_TYPE;
            return 0;
        }

        case EXPR_RECORD_CONSTRUCTOR:
        {
            if (expr->record_type == NULL)
            {
                fprintf(stderr, "Error on line %d, unable to infer record type for constructor.\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            if (!expr->expr_data.record_constructor_data.fields_semchecked)
            {
                int rc_err = semcheck_typecheck_record_constructor(expr, symtab, max_scope_lev,
                    expr->record_type, expr->line_num);
                if (rc_err != 0)
                {
                    *type_return = UNKNOWN_TYPE;
                    return rc_err;
                }
            }
            expr->resolved_type = RECORD_TYPE;
            *type_return = RECORD_TYPE;
            return 0;
        }

        case EXPR_ANONYMOUS_FUNCTION:
        case EXPR_ANONYMOUS_PROCEDURE:
        {
            /* Anonymous methods are treated as procedure/function references.
             * Create a KgpcType for better type checking.
             */
            
            /* Set the type to PROCEDURE to represent a function/procedure reference */
            *type_return = PROCEDURE;
            
            /* Create a KgpcType with parameter and return type information */
            ListNode_t *params = expr->expr_data.anonymous_method_data.parameters;
            KgpcType *return_type = NULL;
            
            /* For functions, resolve the return type */
            if (expr->type == EXPR_ANONYMOUS_FUNCTION) {
                char *return_type_id = expr->expr_data.anonymous_method_data.return_type_id;
                if (return_type_id != NULL) {
                    /* Try to look up the type in the symbol table */
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, return_type_id) >= 0 && type_node != NULL) {
                        /* Use the KgpcType from the symbol table if available */
                        if (type_node->type != NULL) {
                            return_type = type_node->type;
                        }
                    }
                    
                    /* If not found in symbol table, it might be a built-in type */
                    if (return_type == NULL) {
                        /* Try to map the type name to a primitive type */
                        int type_tag = UNKNOWN_TYPE;
                        if (strcasecmp(return_type_id, "integer") == 0)
                            type_tag = INT_TYPE;
                        else if (strcasecmp(return_type_id, "longint") == 0)
                            type_tag = LONGINT_TYPE;
                        else if (strcasecmp(return_type_id, "real") == 0 || strcasecmp(return_type_id, "double") == 0)
                            type_tag = REAL_TYPE;
                        else if (strcasecmp(return_type_id, "string") == 0)
                            type_tag = STRING_TYPE;
                        else if (strcasecmp(return_type_id, "char") == 0)
                            type_tag = CHAR_TYPE;
                        else if (strcasecmp(return_type_id, "boolean") == 0)
                            type_tag = BOOL;
                        
                        if (type_tag != UNKNOWN_TYPE) {
                            return_type = create_primitive_type(type_tag);
                            /* Note: This creates a new KgpcType that will be owned by proc_type */
                        }
                    }
                }
            }
            /* For procedures, return_type remains NULL */
            
            /* Create the procedure type to store in the expression */
            KgpcType *proc_type = create_procedure_type(params, return_type);
            if (proc_type != NULL) {
                expr->resolved_kgpc_type = proc_type;
            }
            
            /* Semantic check the anonymous function body */
            /* We need to create a new scope with parameters and return variable */
            if (expr->expr_data.anonymous_method_data.body != NULL)
            {
                PushScope(symtab);
                
                /* Add parameters to the scope */
                if (params != NULL)
                {
                    ListNode_t *param = params;
                    while (param != NULL)
                    {
                        if (param->type == LIST_TREE && param->cur != NULL)
                        {
                            Tree_t *param_tree = (Tree_t *)param->cur;
                            if (param_tree->type == TREE_VAR_DECL)
                            {
                                /* Add parameter as a variable */
                                ListNode_t *ids = param_tree->tree_data.var_decl_data.ids;
                                while (ids != NULL)
                                {
                                    char *param_id = (char *)ids->cur;
                                    int param_type = param_tree->tree_data.var_decl_data.type;
                                    KgpcType *param_kgpc_type = NULL;
                                    
                                    /* Try to resolve parameter type */
                                    if (param_tree->tree_data.var_decl_data.type_id != NULL)
                                    {
                                        HashNode_t *type_node = NULL;
                                        if (FindIdent(&type_node, symtab, param_tree->tree_data.var_decl_data.type_id) >= 0 &&
                                            type_node != NULL && type_node->type != NULL)
                                        {
                                            param_kgpc_type = type_node->type;
                                        }
                                    }
                                    
                                    if (param_kgpc_type == NULL && param_type != UNKNOWN_TYPE)
                                    {
                                        param_kgpc_type = create_primitive_type(param_type);
                                    }
                                    
                                    PushVarOntoScope_Typed(symtab, param_id, param_kgpc_type);
                                    ids = ids->next;
                                }
                            }
                        }
                        param = param->next;
                    }
                }
                
                /* For functions, add the return variable */
                if (expr->type == EXPR_ANONYMOUS_FUNCTION && expr->expr_data.anonymous_method_data.generated_name != NULL)
                {
                    /* Add the function name as the return variable */
                    PushFuncRetOntoScope_Typed(symtab, expr->expr_data.anonymous_method_data.generated_name, return_type);
                    
                    /* Also add "Result" as an alias in the current scope */
                    HashNode_t *result_check = NULL;
                    HashTable_t *cur_hash = (HashTable_t *)symtab->stack_head->cur;
                    result_check = (cur_hash != NULL) ? FindIdentInTable(cur_hash, "Result") : NULL;
                    if (result_check == NULL)
                    {
                        PushFuncRetOntoScope_Typed(symtab, "Result", return_type);
                    }
                }
                
                /* Semantic check the body statement */
                return_val += semcheck_stmt(symtab, expr->expr_data.anonymous_method_data.body, max_scope_lev);
                
                PopScope(symtab);
            }
            
            break;
        }

        default:
            assert(0 && "Bad type in semcheck_expr_main!");
            break;
    }

    expr->resolved_type = *type_return;
    return return_val;
}

/****** EXPR SEMCHECKS *******/

/** RELOP **/
int semcheck_relop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val;
    int type_first;
    int type_second = UNKNOWN_TYPE;
    struct Expression *expr1, *expr2;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_RELOP);

    return_val = 0;
    expr1 = expr->expr_data.relop_data.left;
    expr2 = expr->expr_data.relop_data.right;

    return_val += semcheck_expr_main(&type_first, symtab, expr1, max_scope_lev, mutating);
    if(expr2 != NULL)
        return_val += semcheck_expr_main(&type_second, symtab, expr2, max_scope_lev, mutating);

    /* Verifying types */

    /* Type must be a bool (this only happens with a NOT operator) */
    if (expr2 == NULL)
    {
        if (type_first == BOOL)
        {
            *type_return = BOOL;
            return return_val;
        }
        if (is_integer_type(type_first))
        {
            struct Expression *operand = expr->expr_data.relop_data.left;
            struct Expression *neg_one = mk_inum(expr->line_num, -1);
            expr->type = EXPR_MULOP;
            expr->expr_data.mulop_data.mulop_type = XOR;
            expr->expr_data.mulop_data.left_term = operand;
            expr->expr_data.mulop_data.right_factor = neg_one;
            return semcheck_mulop(type_return, symtab, expr, max_scope_lev, mutating);
        }
        fprintf(stderr, "Error on line %d, expected relational type after \"NOT\"!\n\n",
            expr->line_num);
        ++return_val;
        *type_return = UNKNOWN_TYPE;
        return return_val;
    }
    else
    {
        if(is_and_or(&expr->expr_data.relop_data.type))
        {
            if(type_first != BOOL || type_second != BOOL)
            {
                fprintf(stderr,
                    "Error on line %d, expected two relational types between AND/OR!\n\n",
                    expr->line_num);
                ++return_val;
            }
        }
        else
        {
            int relop_type = expr->expr_data.relop_data.type;
            if (relop_type == IN)
            {
                /* Coerce single-character string literals to char type */
                if (type_first == STRING_TYPE && expr1 != NULL && 
                    expr1->type == EXPR_STRING && expr1->expr_data.string != NULL &&
                    strlen(expr1->expr_data.string) == 1)
                {
                    type_first = CHAR_TYPE;
                    expr1->resolved_type = CHAR_TYPE;
                }

                if (type_second != SET_TYPE)
                {
                    fprintf(stderr, "Error on line %d, expected set operand on right side of IN expression!\n\n",
                        expr->line_num);
                    ++return_val;
                }
                if (!is_integer_type(type_first) && type_first != ENUM_TYPE &&
                    type_first != CHAR_TYPE && type_first != BOOL)
                {
                    fprintf(stderr, "Error on line %d, expected integer operand on left side of IN expression!\n\n",
                        expr->line_num);
                    ++return_val;
                }
            }
            else if (relop_type == EQ || relop_type == NE)
            {
                /* Check for operator overloading for record types first */
                if (type_first == RECORD_TYPE && type_second == RECORD_TYPE)
                {
                    const char *left_type_name = get_expr_type_name(expr1, symtab);
                    const char *right_type_name = get_expr_type_name(expr2, symtab);
                    
                    if (left_type_name != NULL && right_type_name != NULL &&
                        strcasecmp(left_type_name, right_type_name) == 0)
                    {
                        const char *op_suffix = (relop_type == EQ) ? "op_eq" : "op_ne";
                        size_t name_len = strlen(left_type_name) + strlen(op_suffix) + 3;
                        char *operator_method = (char *)malloc(name_len);
                        
                        if (operator_method != NULL)
                        {
                            snprintf(operator_method, name_len, "%s__%s", left_type_name, op_suffix);
                            
                            HashNode_t *operator_node = NULL;
                            if (FindIdent(&operator_node, symtab, operator_method) == 0 && operator_node != NULL)
                            {
                                if (operator_node->type != NULL && kgpc_type_is_procedure(operator_node->type))
                                {
                                    KgpcType *return_type = kgpc_type_get_return_type(operator_node->type);
                                    if (return_type != NULL)
                                    {
                                        /* Transform expression from RELOP to FUNCTION_CALL */
                                        struct Expression *saved_left = expr->expr_data.relop_data.left;
                                        struct Expression *saved_right = expr->expr_data.relop_data.right;

                                        expr->type = EXPR_FUNCTION_CALL;
                                        memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));

                                        expr->expr_data.function_call_data.id = strdup(operator_method);
                                        /* Use the actual mangled name from the symbol table */
                                        if (operator_node->mangled_id != NULL)
                                            expr->expr_data.function_call_data.mangled_id = strdup(operator_node->mangled_id);
                                        else
                                            expr->expr_data.function_call_data.mangled_id = strdup(operator_method);
                                        
                                        ListNode_t *arg1 = CreateListNode(saved_left, LIST_EXPR);
                                        ListNode_t *arg2 = CreateListNode(saved_right, LIST_EXPR);
                                        arg1->next = arg2;
                                        expr->expr_data.function_call_data.args_expr = arg1;
                                        
                                        expr->expr_data.function_call_data.resolved_func = operator_node;
                                        expr->expr_data.function_call_data.call_hash_type = HASHTYPE_FUNCTION;
                                        expr->expr_data.function_call_data.call_kgpc_type = operator_node->type;
                                        kgpc_type_retain(operator_node->type);
                                        expr->expr_data.function_call_data.is_call_info_valid = 1;
                                        
                                        if (expr->resolved_kgpc_type != NULL)
                                        {
                                            destroy_kgpc_type(expr->resolved_kgpc_type);
                                        }
                                        expr->resolved_kgpc_type = return_type;
                                        kgpc_type_retain(return_type);
                                        
                                        free(operator_method);
                                        *type_return = BOOL;
                                        return return_val;
                                    }
                                }
                            }
                            free(operator_method);
                        }
                    }
                }
                
                semcheck_coerce_char_string_operands(&type_first, expr1, &type_second, expr2);

                int numeric_ok = types_numeric_compatible(type_first, type_second) &&
                                 is_type_ir(&type_first) && is_type_ir(&type_second);
                int boolean_ok = (type_first == BOOL && type_second == BOOL);
                int string_ok = (is_string_type(type_first) && is_string_type(type_second));
                int char_ok = (type_first == CHAR_TYPE && type_second == CHAR_TYPE);
                int pointer_ok = (type_first == POINTER_TYPE && type_second == POINTER_TYPE);
                int enum_ok = (type_first == ENUM_TYPE && type_second == ENUM_TYPE);
                if (!numeric_ok && !boolean_ok && !string_ok && !char_ok && !pointer_ok && !enum_ok)
                {
                    fprintf(stderr, "Error on line %d, equality comparison requires matching numeric, boolean, string, character, or pointer types!\n\n",
                        expr->line_num);
                    ++return_val;
                }
            }
            else
            {
                semcheck_coerce_char_string_operands(&type_first, expr1, &type_second, expr2);

                int numeric_ok = types_numeric_compatible(type_first, type_second) &&
                                 is_type_ir(&type_first) && is_type_ir(&type_second);
                int string_ok = (is_string_type(type_first) && is_string_type(type_second));
                int char_ok = (type_first == CHAR_TYPE && type_second == CHAR_TYPE);
                int pointer_ok = (type_first == POINTER_TYPE && type_second == POINTER_TYPE);
                int enum_ok = (type_first == ENUM_TYPE && type_second == ENUM_TYPE);

                if(!numeric_ok && !string_ok && !char_ok && !pointer_ok && !enum_ok)
                {
                    fprintf(stderr,
                        "Error on line %d, expected compatible numeric, string, or character types between relational op!\n\n",
                        expr->line_num);
                    ++return_val;
                }
            }
        }
    }

    *type_return = BOOL;
    return return_val;
}

/** SIGN_TERM **/
int semcheck_signterm(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val;
    struct Expression *sign_expr;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_SIGN_TERM);

    return_val = 0;
    sign_expr = expr->expr_data.sign_term;

    return_val += semcheck_expr_main(type_return, symtab, sign_expr, max_scope_lev, mutating);

    /* Checking types */
    if(!is_type_ir(type_return))
    {
        fprintf(stderr, "Error on line %d, expected int or real after \"-\"!\n\n",
            expr->line_num);
        ++return_val;
    }

    return return_val;
}

/** ADDOP **/
int semcheck_addop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val;
    int type_first, type_second;
    struct Expression *expr1, *expr2;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_ADDOP);

    return_val = 0;
    expr1 = expr->expr_data.addop_data.left_expr;
    expr2 = expr->expr_data.addop_data.right_term;

    return_val += semcheck_expr_main(&type_first, symtab, expr1, max_scope_lev, mutating);
    return_val += semcheck_expr_main(&type_second, symtab, expr2, max_scope_lev, mutating);

    int op_type = expr->expr_data.addop_data.addop_type;
    if (op_type == OR)
    {
        if (type_first == BOOL && type_second == BOOL)
        {
            *type_return = BOOL;
            return return_val;
        }
        if (is_integer_type(type_first) && is_integer_type(type_second))
        {
            if (type_first == INT64_TYPE || type_second == INT64_TYPE)
                *type_return = INT64_TYPE;
            else if (type_first == LONGINT_TYPE || type_second == LONGINT_TYPE)
                *type_return = LONGINT_TYPE;
            else
                *type_return = INT_TYPE;
            return return_val;
        }
        fprintf(stderr, "Error on line %d, expected boolean or integer operands for OR expression!\n\n",
            expr->line_num);
        ++return_val;
        *type_return = UNKNOWN_TYPE;
        return return_val;
    }

    if (type_first == SET_TYPE && type_second == SET_TYPE)
    {
        if (op_type == PLUS || op_type == MINUS)
        {
            *type_return = SET_TYPE;
        }
        else
        {
            fprintf(stderr, "Error on line %d, unsupported set additive operator.\n\n",
                expr->line_num);
            ++return_val;
            *type_return = SET_TYPE;
        }
        return return_val;
    }

    if (op_type == PLUS)
    {
        int left_is_string_like = (is_string_type(type_first) || type_first == CHAR_TYPE);
        int right_is_string_like = (is_string_type(type_second) || type_second == CHAR_TYPE);

        if (left_is_string_like && right_is_string_like)
        {
            *type_return = STRING_TYPE;
            return return_val;
        }
    }

    /* Check for pointer arithmetic: pointer + integer or integer + pointer */
    if (op_type == PLUS || op_type == MINUS)
    {
        int left_is_pointer = (type_first == POINTER_TYPE);
        int right_is_pointer = (type_second == POINTER_TYPE);
        int left_is_int = (type_first == INT_TYPE || type_first == LONGINT_TYPE);
        int right_is_int = (type_second == INT_TYPE || type_second == LONGINT_TYPE);

        /* pointer + integer or pointer - integer */
        if (left_is_pointer && right_is_int)
        {
            /* Result is a pointer of the same type as the left operand */
            *type_return = POINTER_TYPE;
            /* Copy pointer metadata from left operand to result */
            if (expr1->pointer_subtype != UNKNOWN_TYPE)
            {
                expr->pointer_subtype = expr1->pointer_subtype;
            }
            if (expr1->pointer_subtype_id != NULL)
            {
                expr->pointer_subtype_id = strdup(expr1->pointer_subtype_id);
            }
            if (expr1->record_type != NULL)
            {
                expr->record_type = expr1->record_type;
            }
            
            /* Propagate KgpcType information for proper type checking */
            if (expr1->resolved_kgpc_type != NULL)
            {
                /* Share the KgpcType from the source pointer (increment ref count) */
                kgpc_type_retain(expr1->resolved_kgpc_type);
                semcheck_expr_set_resolved_kgpc_type_shared(expr, expr1->resolved_kgpc_type);
            }
            
            return return_val;
        }

        /* integer + pointer (only for PLUS) */
        if (op_type == PLUS && left_is_int && right_is_pointer)
        {
            /* Result is a pointer of the same type as the right operand */
            *type_return = POINTER_TYPE;
            /* Copy pointer metadata from right operand to result */
            if (expr2->pointer_subtype != UNKNOWN_TYPE)
            {
                expr->pointer_subtype = expr2->pointer_subtype;
            }
            if (expr2->pointer_subtype_id != NULL)
            {
                expr->pointer_subtype_id = strdup(expr2->pointer_subtype_id);
            }
            if (expr2->record_type != NULL)
            {
                expr->record_type = expr2->record_type;
            }
            
            /* Propagate KgpcType information for proper type checking */
            if (expr2->resolved_kgpc_type != NULL)
            {
                /* Share the KgpcType from the source pointer (increment ref count) */
                kgpc_type_retain(expr2->resolved_kgpc_type);
                semcheck_expr_set_resolved_kgpc_type_shared(expr, expr2->resolved_kgpc_type);
            }
            
            return return_val;
        }

        /* pointer - pointer: result is the element difference (integer) */
        if (op_type == MINUS && left_is_pointer && right_is_pointer)
        {
            /* Result is an integer (element count difference) */
            *type_return = LONGINT_TYPE;
            expr->resolved_type = LONGINT_TYPE;
            /* Mark this expression as a pointer difference operation */
            expr->is_pointer_diff = 1;
            /* Store pointer element size from left operand for codegen */
            if (expr1->pointer_subtype != UNKNOWN_TYPE)
            {
                expr->pointer_subtype = expr1->pointer_subtype;
            }
            if (expr1->pointer_subtype_id != NULL)
            {
                expr->pointer_subtype_id = strdup(expr1->pointer_subtype_id);
            }
            return return_val;
        }
    }

    /* Check for operator overloading for record types */
    if (type_first == RECORD_TYPE && type_second == RECORD_TYPE)
    {
        /* Both operands are records - check if they have an operator overload */
        const char *left_type_name = get_expr_type_name(expr1, symtab);
        const char *right_type_name = get_expr_type_name(expr2, symtab);
        
        /* Check if both types are the same and we have a type name */
        if (left_type_name != NULL && right_type_name != NULL &&
            strcasecmp(left_type_name, right_type_name) == 0)
        {
            /* Construct the operator method name */
            const char *op_suffix = NULL;
            switch (op_type)
            {
                case PLUS: op_suffix = "op_add"; break;
                case MINUS: op_suffix = "op_sub"; break;
                default: break;
            }
            
            if (op_suffix != NULL)
            {
                /* Build the mangled operator method name: TypeName__op_add */
                size_t name_len = strlen(left_type_name) + strlen(op_suffix) + 3;
                char *operator_method = (char *)malloc(name_len);
                if (operator_method != NULL)
                {
                    snprintf(operator_method, name_len, "%s__%s", left_type_name, op_suffix);
                    
                    /* Look up the operator method in the symbol table */
                    HashNode_t *operator_node = NULL;
                    if (FindIdent(&operator_node, symtab, operator_method) == 0 && operator_node != NULL)
                    {
                        /* Found the operator overload! */
                        /* Get the return type from the operator method */
                        if (operator_node->type != NULL && kgpc_type_is_procedure(operator_node->type))
                        {
                            KgpcType *return_type = kgpc_type_get_return_type(operator_node->type);
                            if (return_type != NULL)
                            {
                                *type_return = kgpc_type_get_legacy_tag(return_type);
                                
                                /* Transform expression from ADDOP to FUNCTION_CALL */
                                /* Save the operands before we overwrite the union */
                                struct Expression *saved_left = expr->expr_data.addop_data.left_expr;
                                struct Expression *saved_right = expr->expr_data.addop_data.right_term;

                                /* Change expression type to FUNCTION_CALL */
                                expr->type = EXPR_FUNCTION_CALL;
                                memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));

                                /* Populate function_call_data */
                                expr->expr_data.function_call_data.id = strdup(operator_method);
                                /* Use the actual mangled name from the symbol table */
                                if (operator_node->mangled_id != NULL)
                                    expr->expr_data.function_call_data.mangled_id = strdup(operator_node->mangled_id);
                                else
                                    expr->expr_data.function_call_data.mangled_id = strdup(operator_method);
                                
                                /* Create argument list with both operands */
                                ListNode_t *arg1 = CreateListNode(saved_left, LIST_EXPR);
                                ListNode_t *arg2 = CreateListNode(saved_right, LIST_EXPR);
                                arg1->next = arg2;
                                expr->expr_data.function_call_data.args_expr = arg1;
                                
                                expr->expr_data.function_call_data.resolved_func = operator_node;
                                
                                /* Cache operator method type info for codegen */
                                expr->expr_data.function_call_data.call_hash_type = HASHTYPE_FUNCTION;
                                expr->expr_data.function_call_data.call_kgpc_type = operator_node->type;
                                kgpc_type_retain(operator_node->type); /* Increment ref count */
                                expr->expr_data.function_call_data.is_call_info_valid = 1;
                                
                                /* Set resolved_kgpc_type to the return type */
                                if (expr->resolved_kgpc_type != NULL)
                                {
                                    destroy_kgpc_type(expr->resolved_kgpc_type);
                                }
                                expr->resolved_kgpc_type = return_type;
                                kgpc_type_retain(return_type); /* Increment ref count */
                                
                                /* For record return types, preserve record type info */
                                if (kgpc_type_is_record(return_type))
                                {
                                    struct RecordType *ret_record = kgpc_type_get_record(return_type);
                                    if (ret_record != NULL)
                                    {
                                        expr->record_type = ret_record;
                                    }
                                }
                                
                                free(operator_method);
                                return return_val; /* Success - operator overload transformed */
                            }
                        }
                    }
                    free(operator_method);
                }
            }
        }
    }

    /* Checking numeric types */
    if(!types_numeric_compatible(type_first, type_second))
    {
        semantic_error(expr->line_num, expr->col_num, "type mismatch on addop");
        ++return_val;
    }
    if(!is_type_ir(&type_first) || !is_type_ir(&type_second))
    {
        semantic_error(expr->line_num, expr->col_num, "expected int/real on both sides of addop");
        ++return_val;
    }

    if (type_first == REAL_TYPE || type_second == REAL_TYPE)
        *type_return = REAL_TYPE;
    else if (type_first == LONGINT_TYPE || type_second == LONGINT_TYPE)
        *type_return = LONGINT_TYPE;
    else
        *type_return = type_first;
    return return_val;
}

/** MULOP **/
int semcheck_mulop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val;
    int type_first, type_second;
    struct Expression *expr1, *expr2;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_MULOP);

    return_val = 0;
    expr1 = expr->expr_data.mulop_data.left_term;
    expr2 = expr->expr_data.mulop_data.right_factor;

    return_val += semcheck_expr_main(&type_first, symtab, expr1, max_scope_lev, mutating);
    return_val += semcheck_expr_main(&type_second, symtab, expr2, max_scope_lev, mutating);

    int op_type = expr->expr_data.mulop_data.mulop_type;
    
    /* Handle AND and XOR operators */
    if (op_type == AND || op_type == XOR)
    {
        /* Boolean operations */
        if (type_first == BOOL && type_second == BOOL)
        {
            *type_return = BOOL;
            return return_val;
        }
        
        /* Set operations */
        if (type_first == SET_TYPE && type_second == SET_TYPE)
        {
            *type_return = SET_TYPE;
            return return_val;
        }
        
        /* Integer bitwise operations */
        if (is_integer_type(type_first) && is_integer_type(type_second))
        {
            /* Both operands are integers - bitwise operation */
            /* INT64_TYPE takes precedence as the largest integer type */
            if (type_first == INT64_TYPE || type_second == INT64_TYPE)
                *type_return = INT64_TYPE;
            else if (type_first == LONGINT_TYPE || type_second == LONGINT_TYPE)
                *type_return = LONGINT_TYPE;
            else
                *type_return = INT_TYPE;
            return return_val;
        }
        
        /* Invalid operand types for AND/XOR */
        fprintf(stderr, "Error on line %d, expected boolean, integer, or set operands for %s expression!\n\n",
            expr->line_num, op_type == AND ? "AND" : "XOR");
        ++return_val;
        *type_return = UNKNOWN_TYPE;
        return return_val;
    }

    /* Set operations for STAR operator (intersection) */
    if (type_first == SET_TYPE && type_second == SET_TYPE)
    {
        if (op_type == STAR)
        {
            *type_return = SET_TYPE;
        }
        else
        {
            fprintf(stderr, "Error on line %d, unsupported set multiplicative operator.\n\n",
                expr->line_num);
            ++return_val;
            *type_return = SET_TYPE;
        }
        return return_val;
    }

    /* Check for operator overloading for record types */
    if (type_first == RECORD_TYPE && type_second == RECORD_TYPE)
    {
        /* Both operands are records - check if they have an operator overload */
        const char *left_type_name = get_expr_type_name(expr1, symtab);
        const char *right_type_name = get_expr_type_name(expr2, symtab);
        
        /* Check if both types are the same and we have a type name */
        if (left_type_name != NULL && right_type_name != NULL &&
            strcasecmp(left_type_name, right_type_name) == 0)
        {
            /* Construct the operator method name */
            const char *op_suffix = NULL;
            switch (op_type)
            {
                case STAR: op_suffix = "op_mul"; break;
                case SLASH: op_suffix = "op_div"; break;
                default: break;
            }
            
            if (op_suffix != NULL)
            {
                /* Build the mangled operator method name: TypeName__op_mul */
                size_t name_len = strlen(left_type_name) + strlen(op_suffix) + 3;
                char *operator_method = (char *)malloc(name_len);
                if (operator_method != NULL)
                {
                    snprintf(operator_method, name_len, "%s__%s", left_type_name, op_suffix);
                    
                    /* Look up the operator method in the symbol table */
                    HashNode_t *operator_node = NULL;
                    if (FindIdent(&operator_node, symtab, operator_method) == 0 && operator_node != NULL)
                    {
                        /* Found the operator overload! */
                        if (operator_node->type != NULL && kgpc_type_is_procedure(operator_node->type))
                        {
                            KgpcType *return_type = kgpc_type_get_return_type(operator_node->type);
                            if (return_type != NULL)
                            {
                                *type_return = kgpc_type_get_legacy_tag(return_type);
                                
                                /* Transform expression from MULOP to FUNCTION_CALL */
                                /* Save the operands before we overwrite the union */
                                struct Expression *saved_left = expr->expr_data.mulop_data.left_term;
                                struct Expression *saved_right = expr->expr_data.mulop_data.right_factor;

                                /* Change expression type to FUNCTION_CALL */
                                expr->type = EXPR_FUNCTION_CALL;
                                memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));

                                /* Populate function_call_data */
                                expr->expr_data.function_call_data.id = strdup(operator_method);
                                /* Use the actual mangled name from the symbol table */
                                if (operator_node->mangled_id != NULL)
                                    expr->expr_data.function_call_data.mangled_id = strdup(operator_node->mangled_id);
                                else
                                    expr->expr_data.function_call_data.mangled_id = strdup(operator_method);
                                
                                /* Create argument list with both operands */
                                ListNode_t *arg1 = CreateListNode(saved_left, LIST_EXPR);
                                ListNode_t *arg2 = CreateListNode(saved_right, LIST_EXPR);
                                arg1->next = arg2;
                                expr->expr_data.function_call_data.args_expr = arg1;
                                
                                expr->expr_data.function_call_data.resolved_func = operator_node;
                                
                                /* Cache operator method type info for codegen */
                                expr->expr_data.function_call_data.call_hash_type = HASHTYPE_FUNCTION;
                                expr->expr_data.function_call_data.call_kgpc_type = operator_node->type;
                                kgpc_type_retain(operator_node->type);
                                expr->expr_data.function_call_data.is_call_info_valid = 1;
                                
                                /* Set resolved_kgpc_type to the return type */
                                if (expr->resolved_kgpc_type != NULL)
                                {
                                    destroy_kgpc_type(expr->resolved_kgpc_type);
                                }
                                expr->resolved_kgpc_type = return_type;
                                kgpc_type_retain(return_type);
                                
                                /* For record return types, preserve record type info */
                                if (kgpc_type_is_record(return_type))
                                {
                                    struct RecordType *ret_record = kgpc_type_get_record(return_type);
                                    if (ret_record != NULL)
                                    {
                                        expr->record_type = ret_record;
                                    }
                                }
                                
                                free(operator_method);
                                return return_val; /* Success - operator overload transformed */
                            }
                        }
                    }
                    free(operator_method);
                }
            }
        }
    }

    /* Checking numeric types */
    if(!types_numeric_compatible(type_first, type_second))
    {
        fprintf(stderr, "Error on line %d, type mismatch on mulop!\n\n",
            expr->line_num);
        ++return_val;
    }
    if(!is_type_ir(&type_first) || !is_type_ir(&type_second))
    {
        fprintf(stderr, "Error on line %d, expected int/real on both sides of mulop!\n\n",
            expr->line_num);
        ++return_val;
    }

    /* Handle DIV and MOD operators - integer division only */
    if (op_type == DIV || op_type == MOD)
    {
        if (type_first == REAL_TYPE || type_second == REAL_TYPE)
        {
            fprintf(stderr, "Error on line %d, DIV and MOD operators require integer operands!\n\n",
                expr->line_num);
            ++return_val;
        }
        /* DIV and MOD produce integer results */
        /* INT64_TYPE takes precedence as the largest integer type */
        if (type_first == INT64_TYPE || type_second == INT64_TYPE)
            *type_return = INT64_TYPE;
        else if (type_first == LONGINT_TYPE || type_second == LONGINT_TYPE)
            *type_return = LONGINT_TYPE;
        else
            *type_return = INT_TYPE;
        return return_val;
    }

    /* SLASH (/) always produces REAL_TYPE in Pascal, regardless of operand types */
    if (type_first == REAL_TYPE || type_second == REAL_TYPE || op_type == SLASH)
        *type_return = REAL_TYPE;
    else if (type_first == INT64_TYPE || type_second == INT64_TYPE)
        *type_return = INT64_TYPE;
    else if (type_first == LONGINT_TYPE || type_second == LONGINT_TYPE)
        *type_return = LONGINT_TYPE;
    else
        *type_return = type_first;
    return return_val;
}

/** VAR_ID **/
int semcheck_varid(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val, scope_return;
    char *id;
    HashNode_t *hash_return;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_VAR_ID);

    return_val = 0;
    id = expr->expr_data.id;
    semcheck_clear_pointer_info(expr);
    semcheck_clear_array_info(expr);

    scope_return = FindIdent(&hash_return, symtab, id);
    if(scope_return == -1)
    {
        struct Expression *with_expr = NULL;
        int with_status = semcheck_with_try_resolve(id, symtab, &with_expr, expr->line_num);
        if (with_status == 0 && with_expr != NULL)
        {
            char *field_id = expr->expr_data.id;
            expr->type = EXPR_RECORD_ACCESS;
            expr->expr_data.record_access_data.record_expr = with_expr;
            expr->expr_data.record_access_data.field_id = field_id;
            expr->expr_data.record_access_data.field_offset = 0;
            return semcheck_recordaccess(type_return, symtab, expr, max_scope_lev, mutating);
        }

        /* FPC-style module property fallback: resolve Foo as GetFoo() when present. */
        if (with_status != 0 && id != NULL)
        {
            size_t id_len = strlen(id);
            char *getter_id = (char *)malloc(id_len + 4);
            if (getter_id != NULL)
            {
                snprintf(getter_id, id_len + 4, "Get%s", id);
                HashNode_t *getter_node = NULL;
                int getter_found = (FindIdent(&getter_node, symtab, getter_id) == 0);
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                {
                    fprintf(stderr, "[SemCheck] varid fallback: id=%s getter=%s found=%d hash=%d\n",
                        id, getter_id, getter_found,
                        getter_node != NULL ? getter_node->hash_type : -1);
                }
                if (getter_found &&
                    getter_node != NULL && getter_node->hash_type == HASHTYPE_FUNCTION)
                {
                    expr->type = EXPR_FUNCTION_CALL;
                    memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
                    expr->expr_data.function_call_data.id = getter_id;
                    expr->expr_data.function_call_data.args_expr = NULL;
                    expr->expr_data.function_call_data.mangled_id = NULL;
                    semcheck_reset_function_call_cache(expr);
                    return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                }
                free(getter_id);
            }
        }

        if (with_status != 0 && id != NULL)
        {
            HashNode_t *self_node = NULL;
            if (FindIdent(&self_node, symtab, "Self") == 0 && self_node != NULL)
            {
                struct RecordType *self_record = get_record_type_from_node(self_node);
                if (self_record != NULL)
                {
                    HashNode_t *method_node = semcheck_find_class_method(symtab, self_record, id, NULL);
                    if (method_node != NULL)
                    {
                        expr->type = EXPR_FUNCTION_CALL;
                        memset(&expr->expr_data.function_call_data, 0,
                            sizeof(expr->expr_data.function_call_data));
                        expr->expr_data.function_call_data.id = strdup(id);
                        expr->expr_data.function_call_data.args_expr = NULL;
                        expr->expr_data.function_call_data.mangled_id = NULL;
                        semcheck_reset_function_call_cache(expr);
                        return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                    }
                }
            }
        }

        if (with_status == -1)
        {
            semantic_error(expr->line_num, expr->col_num,
                "unable to resolve WITH context for field \"%s\"", id);
            ++return_val;
        }
        else
        {
            semantic_error(expr->line_num, expr->col_num, "undeclared identifier \"%s\"", id);
            ++return_val;
        }

        *type_return = UNKNOWN_TYPE;
    }
    else
    {
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL &&
            id != NULL && strcmp(id, "DefaultComparer") == 0)
        {
            fprintf(stderr,
                "[SemCheck] Debug DefaultComparer: hash_type=%d scope=%d mutating=%d type_kind=%d\n",
                hash_return != NULL ? hash_return->hash_type : -1,
                scope_return, mutating,
                hash_return != NULL && hash_return->type != NULL ?
                    hash_return->type->kind : -1);
        }
        /* If this is a function being used in an expression context (not being assigned to),
           convert it to a function call with no arguments.
           
           When mutating == NO_MUTATE, we're reading the function's return value.
           When mutating != NO_MUTATE, we're inside the function assigning to its return value,
           which should remain as HASHTYPE_FUNCTION_RETURN access. */
        if(hash_return->hash_type == HASHTYPE_FUNCTION && mutating == NO_MUTATE)
        {
            char *func_id = expr->expr_data.id;
            /* Set to NULL to transfer ownership to function_call_data.id and avoid double-free */
            expr->expr_data.id = NULL;

            expr->type = EXPR_FUNCTION_CALL;
            memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
            expr->expr_data.function_call_data.id = func_id;
            expr->expr_data.function_call_data.args_expr = NULL;
            expr->expr_data.function_call_data.mangled_id = NULL;
            semcheck_reset_function_call_cache(expr);
            
            return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
        }
        
        /* Don't convert procedure identifiers to function calls when used as values
         * This allows procedure variables to work correctly */
        if(hash_return->hash_type == HASHTYPE_PROCEDURE && mutating == NO_MUTATE)
        {
            /* Keep as EXPR_VAR_ID so it can be used as a procedure value */
            set_hash_meta(hash_return, mutating);
            set_type_from_hashtype(type_return, hash_return);
            return 0;
        }
        
        set_hash_meta(hash_return, mutating);
        semcheck_mark_static_link_needed(scope_return, hash_return);
        if(scope_return > max_scope_lev)
        {
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] semcheck_varid: scope_return=%d max_scope_lev=%d\n", scope_return, max_scope_lev);
            }
            if (hash_return->hash_type != HASHTYPE_CONST &&
                hash_return->hash_type != HASHTYPE_TYPE)
            {
                fprintf(stderr, "Error on line %d, cannot change \"%s\", invalid scope!\n",
                    expr->line_num, id);
                fprintf(stderr, "[Was it defined above a function declaration?]\n\n");
                ++return_val;
            }
        }
        int is_function_result = (mutating != NO_MUTATE &&
            hash_return->hash_type == HASHTYPE_FUNCTION);

        KgpcType *effective_type = hash_return->type;
        int node_is_array = 0;
        if (is_function_result)
        {
            /* Prefer the function's return type when assigning to the function name */
            if (hash_return->type != NULL)
            {
                KgpcType *ret_type = kgpc_type_get_return_type(hash_return->type);
                if (ret_type != NULL)
                    effective_type = ret_type;
            }
        }
        else
        {
            if (hash_return->type != NULL &&
                (kgpc_type_is_array(hash_return->type) || kgpc_type_is_array_of_const(hash_return->type)))
            {
                node_is_array = 1;
            }
            else
            {
                node_is_array = hashnode_is_array(hash_return);
            }
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                fprintf(stderr, "[SemCheck] semcheck_varid: id=%s hash_type=%d node_type=%p kind=%d node_is_array=%d\n",
                    id ? id : "<null>", hash_return->hash_type,
                    (void*)hash_return->type,
                    hash_return->type ? hash_return->type->kind : -1,
                    node_is_array);
        }

        if(hash_return->hash_type != HASHTYPE_VAR &&
            hash_return->hash_type != HASHTYPE_FUNCTION_RETURN &&
            hash_return->hash_type != HASHTYPE_TYPE &&
            !node_is_array &&
            !is_function_result)
        {
            if(hash_return->hash_type == HASHTYPE_CONST && mutating == 0)
            {
                /* Constants are readable values. */
            }
            else if(hash_return->hash_type == HASHTYPE_PROCEDURE && mutating == 0)
            {
                /* Procedures can be used as values when not mutating (for procedure variables). */
            }
            else
            {
                fprintf(stderr, "Error on line %d, cannot assign \"%s\", is not a scalar variable!\n\n",
                    expr->line_num, id);
                ++return_val;
            }
        }
        set_type_from_hashtype(type_return, hash_return);
        if (node_is_array)
            semcheck_set_array_info_from_hashnode(expr, symtab, hash_return, expr->line_num);
        else
            semcheck_clear_array_info(expr);
        semcheck_expr_set_resolved_kgpc_type_shared(expr, effective_type);

        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] semcheck_varid: expr=%p, id=%s, type_return=%d\n", (void*)expr, id, *type_return);
        }

        if (*type_return == POINTER_TYPE)
        {
            int subtype = UNKNOWN_TYPE;
            const char *type_id = NULL;
            struct TypeAlias *alias = get_type_alias_from_node(hash_return);
            if (alias != NULL && alias->is_pointer)
            {
                subtype = alias->pointer_type;
                type_id = alias->pointer_type_id;
            }
            
            if (subtype == UNKNOWN_TYPE && hash_return->type != NULL &&
                kgpc_type_is_pointer(hash_return->type))
            {
                subtype = kgpc_type_get_pointer_subtype_tag(hash_return->type);
            }
            
            if (subtype == UNKNOWN_TYPE && type_id != NULL)
            {
                HashNode_t *target_node = NULL;
                if (FindIdent(&target_node, symtab, (char*)type_id) != -1 && target_node != NULL)
                {
                    set_type_from_hashtype(&subtype, target_node);
                }
            }
            
            semcheck_set_pointer_info(expr, subtype, type_id);
            expr->record_type = NULL;
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
            {
                fprintf(stderr,
                    "[SemCheck] semcheck_varid: id=%s pointer_subtype=%d subtype_id=%s\n",
                    id ? id : "<null>", expr->pointer_subtype,
                    expr->pointer_subtype_id ? expr->pointer_subtype_id : "<null>");
            }
            if (expr->pointer_subtype_id != NULL)
            {
                HashNode_t *target_node = NULL;
                if (FindIdent(&target_node, symtab, expr->pointer_subtype_id) != -1 && target_node != NULL)
                    expr->record_type = get_record_type_from_node(target_node);
            }
            else if (hash_return->type != NULL && kgpc_type_is_pointer(hash_return->type))
            {
                KgpcType *points_to = hash_return->type->info.points_to;
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_varid: id=%s, points_to=%p\n", id, points_to);
                    if (points_to) {
                         fprintf(stderr, "[SemCheck] semcheck_varid: points_to->kind=%d\n", points_to->kind);
                    }
                }
                if (points_to != NULL && kgpc_type_is_record(points_to)) {
                    expr->record_type = kgpc_type_get_record(points_to);
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck] semcheck_varid: id=%s, type=POINTER, points_to_record=%p\n", 
                            id, expr->record_type);
                    }
                }
            }
        }
        if (*type_return == RECORD_TYPE)
        {
            expr->record_type = get_record_type_from_node(hash_return);
        }
        else if (*type_return != POINTER_TYPE)
            expr->record_type = NULL;
    }

    return return_val;
}

/** ARRAY_ACCESS **/
int semcheck_arrayaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val = 0;
    int index_type = UNKNOWN_TYPE;
    int element_type = UNKNOWN_TYPE;
    struct Expression *array_expr;
    struct Expression *access_expr;

    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_ARRAY_ACCESS);

    semcheck_clear_pointer_info(expr);
    semcheck_clear_array_info(expr);
    expr->record_type = NULL;

    array_expr = expr->expr_data.array_access_data.array_expr;
    access_expr = expr->expr_data.array_access_data.index_expr;

    if (array_expr == NULL)
    {
        fprintf(stderr, "Error on line %d, array access requires a base expression.\n\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    if (array_expr->type == EXPR_VAR_ID)
    {
        HashNode_t *array_node = NULL;
        if (FindIdent(&array_node, symtab, array_expr->expr_data.id) == -1)
        {
            int property_result = semcheck_try_indexed_property_getter(type_return, symtab,
                expr, max_scope_lev, mutating);
            if (property_result >= 0)
                return return_val + property_result;
        }
    }

    int base_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_main(&base_type, symtab, array_expr, max_scope_lev, mutating);

    int base_is_string = (is_string_type(base_type) && !array_expr->is_array_expr);
    /* Only treat as pointer indexing if NOT an array expression - for arrays of pointers,
     * we want to go through the array path to properly handle element type info */
    int base_is_pointer = (base_type == POINTER_TYPE && !array_expr->is_array_expr);
    
    if (!array_expr->is_array_expr && !base_is_string && !base_is_pointer)
    {
        int property_result = semcheck_try_indexed_property_getter(type_return, symtab,
            expr, max_scope_lev, mutating);
        if (property_result >= 0)
            return return_val + property_result;

        fprintf(stderr, "Error on line %d, expression is not indexable as an array.\n\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return return_val + 1;
    }

    if (base_is_string)
    {
        element_type = CHAR_TYPE;
    }
    else if (base_is_pointer)
    {
        /* Pointer indexing: p[i] is equivalent to (p+i)^ */
        /* Get the type that the pointer points to */
        element_type = array_expr->pointer_subtype;
        
        if (element_type == UNKNOWN_TYPE && array_expr->pointer_subtype_id != NULL)
        {
            int resolved_type = UNKNOWN_TYPE;
            if (resolve_type_identifier(&resolved_type, symtab, array_expr->pointer_subtype_id,
                    expr->line_num) == 0)
                element_type = resolved_type;
        }
        
        if (element_type == UNKNOWN_TYPE && array_expr->record_type != NULL)
            element_type = RECORD_TYPE;
        
        /* Copy pointer target type info to result */
        if (element_type == POINTER_TYPE && array_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindIdent(&type_node, symtab, array_expr->pointer_subtype_id) != -1 && type_node != NULL)
            {
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->is_pointer)
                {
                    expr->pointer_subtype = alias->pointer_type;
                    if (alias->pointer_type_id != NULL)
                        expr->pointer_subtype_id = strdup(alias->pointer_type_id);
                    if (alias->pointer_type == RECORD_TYPE && alias->pointer_type_id != NULL)
                        expr->record_type = semcheck_lookup_record_type(symtab, alias->pointer_type_id);
                }
            }
        }
        
        if (element_type == RECORD_TYPE && array_expr->record_type != NULL)
        {
            expr->record_type = array_expr->record_type;
        }
    }
    else
    {
        element_type = array_expr->array_element_type;
        if (element_type == UNKNOWN_TYPE && array_expr->array_element_type_id != NULL)
        {
            int resolved_type = UNKNOWN_TYPE;
            if (resolve_type_identifier(&resolved_type, symtab, array_expr->array_element_type_id,
                    expr->line_num) == 0)
                element_type = resolved_type;
        }
        if (element_type == UNKNOWN_TYPE && array_expr->array_element_record_type != NULL)
            element_type = RECORD_TYPE;

        if (array_expr->array_element_type_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindIdent(&type_node, symtab, array_expr->array_element_type_id) != -1 && type_node != NULL)
            {
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->is_array)
                {
                    semcheck_set_array_info_from_alias(expr, symtab, alias, expr->line_num);
                }
            }
        }

        if (element_type == POINTER_TYPE)
        {
            int pointer_subtype = UNKNOWN_TYPE;
            const char *pointer_subtype_id = NULL;
            struct RecordType *pointer_record = NULL;

            if (array_expr->array_element_type_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindIdent(&type_node, symtab, array_expr->array_element_type_id) != -1 &&
                    type_node != NULL)
                {
                    struct TypeAlias *alias = get_type_alias_from_node(type_node);
                    if (alias != NULL && alias->is_pointer)
                    {
                        pointer_subtype = alias->pointer_type;
                        pointer_subtype_id = alias->pointer_type_id;
                        if (alias->pointer_type == RECORD_TYPE && alias->pointer_type_id != NULL)
                            pointer_record = semcheck_lookup_record_type(symtab, alias->pointer_type_id);
                    }
                }
            }

            if (pointer_subtype_id == NULL && array_expr->array_element_type_id != NULL &&
                (array_expr->array_element_type_id[0] == 'P' ||
                 array_expr->array_element_type_id[0] == 'p') &&
                array_expr->array_element_type_id[1] != '\0')
            {
                pointer_subtype_id = array_expr->array_element_type_id + 1;
                if (pointer_subtype == UNKNOWN_TYPE)
                {
                    int mapped = semcheck_map_builtin_type_name(symtab, pointer_subtype_id);
                    if (mapped != UNKNOWN_TYPE)
                        pointer_subtype = mapped;
                }
            }

            semcheck_set_pointer_info(expr, pointer_subtype, pointer_subtype_id);
            if (pointer_subtype == RECORD_TYPE)
                expr->record_type = pointer_record;
            else
                expr->record_type = NULL;
        }
        else if (element_type == RECORD_TYPE)
        {
            expr->record_type = array_expr->array_element_record_type;
            if (expr->record_type == NULL && array_expr->array_element_type_id != NULL)
                expr->record_type = semcheck_lookup_record_type(symtab, array_expr->array_element_type_id);
        }
    }

    return_val += semcheck_expr_main(&index_type, symtab, access_expr, max_scope_lev, NO_MUTATE);
    if (!is_ordinal_type(index_type))
    {
        fprintf(stderr, "Error on line %d, expected ordinal type (integer, char, boolean, or enum) in array index expression!\n\n",
            expr->line_num);
        ++return_val;
    }

    if (element_type == UNKNOWN_TYPE)
        element_type = LONGINT_TYPE;

    *type_return = element_type;
    return return_val;
}

/* Helper to resolve the actual type tag from a TREE_VAR_DECL parameter declaration */
int resolve_param_type(Tree_t *decl, SymTab_t *symtab)
{
    assert(decl != NULL);
    assert(symtab != NULL);
    
    int type_tag = UNKNOWN_TYPE;
    char *type_id = NULL;

    if (decl->type == TREE_VAR_DECL)
    {
        type_tag = decl->tree_data.var_decl_data.type;
        type_id = decl->tree_data.var_decl_data.type_id;
    }
    else if (decl->type == TREE_ARR_DECL)
    {
        type_tag = decl->tree_data.arr_decl_data.type;
        type_id = decl->tree_data.arr_decl_data.type_id;
    }

    if (type_id != NULL)
    {
        if (pascal_identifier_equals(type_id, "ShortString"))
            return SHORTSTRING_TYPE;

        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, type_id) >= 0 && type_node != NULL)
        {
            int resolved_type = UNKNOWN_TYPE;
            set_type_from_hashtype(&resolved_type, type_node);
            if (resolved_type != UNKNOWN_TYPE)
                return resolved_type;
        }
    }

    if (type_tag != UNKNOWN_TYPE)
        return type_tag;

    return UNKNOWN_TYPE;
}

static const char *semcheck_get_param_type_id(Tree_t *decl)
{
    if (decl == NULL)
        return NULL;
    if (decl->type == TREE_VAR_DECL)
        return decl->tree_data.var_decl_data.type_id;
    if (decl->type == TREE_ARR_DECL)
        return decl->tree_data.arr_decl_data.type_id;
    return NULL;
}

static int semcheck_get_pointer_param_subtype(Tree_t *decl, SymTab_t *symtab)
{
    int owns_type = 0;
    int subtype = UNKNOWN_TYPE;
    KgpcType *kgpc_type = resolve_type_from_vardecl(decl, symtab, &owns_type);
    if (kgpc_type != NULL && kgpc_type_is_pointer(kgpc_type))
        subtype = kgpc_type_get_pointer_subtype_tag(kgpc_type);
    if (owns_type && kgpc_type != NULL)
        destroy_kgpc_type(kgpc_type);
    return subtype;
}

static int semcheck_candidates_share_signature(SymTab_t *symtab, HashNode_t *a, HashNode_t *b)
{
    if (a == NULL || b == NULL || a->type == NULL || b->type == NULL)
        return 0;

    ListNode_t *args_a = kgpc_type_get_procedure_params(a->type);
    ListNode_t *args_b = kgpc_type_get_procedure_params(b->type);
    if (ListLength(args_a) != ListLength(args_b))
        return 0;

    while (args_a != NULL && args_b != NULL)
    {
        Tree_t *decl_a = (Tree_t *)args_a->cur;
        Tree_t *decl_b = (Tree_t *)args_b->cur;
        int type_a = resolve_param_type(decl_a, symtab);
        int type_b = resolve_param_type(decl_b, symtab);
        if (type_a != type_b)
            return 0;
        if (type_a == POINTER_TYPE)
        {
            const char *id_a = semcheck_get_param_type_id(decl_a);
            const char *id_b = semcheck_get_param_type_id(decl_b);
            if (id_a != NULL || id_b != NULL)
            {
                if (id_a == NULL || id_b == NULL ||
                    !pascal_identifier_equals(id_a, id_b))
                    return 0;
            }
            else
            {
                int sub_a = semcheck_get_pointer_param_subtype(decl_a, symtab);
                int sub_b = semcheck_get_pointer_param_subtype(decl_b, symtab);
                if (sub_a != UNKNOWN_TYPE && sub_b != UNKNOWN_TYPE && sub_a != sub_b)
                    return 0;
                if (sub_a == UNKNOWN_TYPE || sub_b == UNKNOWN_TYPE)
                    return 0;
            }
        }
        args_a = args_a->next;
        args_b = args_b->next;
    }

    return 1;
}

/* Helper to check if a parameter has a default value */
static int param_has_default_value(Tree_t *decl)
{
    if (decl == NULL)
        return 0;
    
    if (decl->type == TREE_VAR_DECL)
    {
        /* Default value is stored in the initializer field */
        return decl->tree_data.var_decl_data.initializer != NULL;
    }
    else if (decl->type == TREE_ARR_DECL)
    {
        return decl->tree_data.arr_decl_data.initializer != NULL;
    }
    
    return 0;
}

static int semcheck_candidate_is_builtin(SymTab_t *symtab, HashNode_t *node)
{
    if (symtab == NULL || node == NULL || node->id == NULL)
        return 0;

    ListNode_t *matches = FindAllIdentsInTable(symtab->builtins, node->id);
    int is_builtin = 0;
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        if (cur->cur == node)
        {
            is_builtin = 1;
            break;
        }
        cur = cur->next;
    }
    if (matches != NULL)
        DestroyList(matches);
    return is_builtin;
}

/* Helper to get the default value expression from a parameter */
static struct Expression *get_param_default_value(Tree_t *decl)
{
    if (decl == NULL)
        return NULL;
    
    struct Statement *init = NULL;
    
    if (decl->type == TREE_VAR_DECL)
    {
        init = decl->tree_data.var_decl_data.initializer;
    }
    else if (decl->type == TREE_ARR_DECL)
    {
        init = decl->tree_data.arr_decl_data.initializer;
    }
    
    /* The default value is stored as a STMT_VAR_ASSIGN with NULL var, containing the expression */
    if (init != NULL && init->type == STMT_VAR_ASSIGN)
    {
        return init->stmt_data.var_assign_data.expr;
    }
    
    return NULL;
}

/* Helper to count required parameters (those without defaults) */
static int count_required_params(ListNode_t *params)
{
    int required = 0;
    ListNode_t *cur = params;
    
    /* Once we see a parameter with a default, all following must also have defaults */
    while (cur != NULL)
    {
        Tree_t *param_decl = (Tree_t *)cur->cur;
        if (!param_has_default_value(param_decl))
            required++;
        else
            break;  /* All remaining params have defaults */
        cur = cur->next;
    }
    
    return required;
}

static int append_default_args(ListNode_t **args_head, ListNode_t *formal_params, int line_num)
{
    if (args_head == NULL)
        return 0;

    ListNode_t *formal = formal_params;
    ListNode_t *actual = *args_head;
    ListNode_t *tail = *args_head;

    while (tail != NULL && tail->next != NULL)
        tail = tail->next;

    while (formal != NULL && actual != NULL)
    {
        formal = formal->next;
        actual = actual->next;
    }

    while (formal != NULL)
    {
        Tree_t *param_decl = (Tree_t *)formal->cur;
        if (!param_has_default_value(param_decl))
            break;

        struct Expression *default_expr = get_param_default_value(param_decl);
        if (default_expr == NULL)
        {
            fprintf(stderr, "Error on line %d, missing default value expression.\n", line_num);
            return 1;
        }

        struct Expression *default_clone = clone_expression(default_expr);
        if (default_clone == NULL)
        {
            fprintf(stderr, "Error on line %d, failed to clone default argument expression.\n", line_num);
            return 1;
        }

        ListNode_t *node = CreateListNode(default_clone, LIST_EXPR);
        if (node == NULL)
        {
            destroy_expr(default_clone);
            fprintf(stderr, "Error on line %d, failed to allocate default argument node.\n", line_num);
            return 1;
        }

        if (*args_head == NULL)
        {
            *args_head = node;
            tail = node;
        }
        else
        {
            tail->next = node;
            tail = node;
        }

        formal = formal->next;
    }

    return 0;
}

/** FUNC_CALL **/
int semcheck_funccall(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val, scope_return;
    char *id;
    char *mangled_name = NULL;
    int arg_type, cur_arg;
    ListNode_t *true_args, *true_arg_ids, *args_given;
    ListNode_t *overload_candidates = NULL;  /* Declare early to avoid uninitialized use */
    HashNode_t *hash_return;
    Tree_t *arg_decl;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    return_val = 0;
    id = expr->expr_data.function_call_data.id;
    if (id != NULL)
    {
        const char *dot = strrchr(id, '.');
        if (dot != NULL && dot[1] != '\0')
        {
            char *unqualified = strdup(dot + 1);
            if (unqualified == NULL)
            {
                fprintf(stderr, "Error on line %d: failed to allocate memory for unit-qualified call '%s'.\n",
                    expr->line_num, id);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            free(expr->expr_data.function_call_data.id);
            expr->expr_data.function_call_data.id = unqualified;
            id = unqualified;
        }
    }
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_funccall: id='%s'\n", id != NULL ? id : "(null)");
    }
    if (id != NULL && strcmp(id, "socket") == 0) {
#ifdef DEBUG
        fprintf(stderr, "DEBUG: semcheck_funccall processing socket call. line=%d\n", expr->line_num);
#endif
    }
    args_given = expr->expr_data.function_call_data.args_expr;

    /* FPC Bootstrap Feature: Handle unit-qualified calls that the parser
     * represents as __Function(UnitName, Args...). Only strip the first
     * argument when the unit qualifier is unresolved AND the real function
     * (without "__") exists. */
    if (id != NULL && strncmp(id, "__", 2) == 0 && args_given != NULL)
    {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            HashNode_t *unit_check = NULL;
            if (FindIdent(&unit_check, symtab, first_arg->expr_data.id) == -1 &&
                semcheck_is_unit_name(first_arg->expr_data.id))
            {
                char *real_func_name = strdup(id + 2);
                if (real_func_name != NULL)
                {
                    ListNode_t *func_candidates = FindAllIdents(symtab, real_func_name);
                    if (func_candidates != NULL)
                    {
                        ListNode_t *remaining_args = args_given->next;
                        destroy_expr(first_arg);
                        args_given->cur = NULL;
                        free(args_given);

                        expr->expr_data.function_call_data.args_expr = remaining_args;
                        args_given = remaining_args;

                        free(expr->expr_data.function_call_data.id);
                        expr->expr_data.function_call_data.id = real_func_name;
                        id = real_func_name;

                        DestroyList(func_candidates);
                        real_func_name = NULL;
                    }
                    if (real_func_name != NULL)
                        free(real_func_name);
                }
            }
        }
    }

    /* Handle unit-qualified calls parsed as member access: UnitName.Func(args). */
    if (expr->expr_data.function_call_data.is_method_call_placeholder && args_given != NULL)
    {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            HashNode_t *unit_check = NULL;
            if (FindIdent(&unit_check, symtab, first_arg->expr_data.id) == -1 &&
                semcheck_is_unit_name(first_arg->expr_data.id))
            {
                ListNode_t *remaining_args = args_given->next;
                destroy_expr(first_arg);
                args_given->cur = NULL;
                free(args_given);

                expr->expr_data.function_call_data.args_expr = remaining_args;
                args_given = remaining_args;
                expr->expr_data.function_call_data.is_method_call_placeholder = 0;
            }
        }
    }

    /* If no receiver was provided, but Self is in scope and defines this method,
     * prepend Self so unqualified method calls resolve correctly. */
    if (id != NULL && (args_given == NULL || args_given->cur == NULL))
    {
        HashNode_t *self_node = NULL;
        if (FindIdent(&self_node, symtab, "Self") == 0 && self_node != NULL)
        {
            struct RecordType *self_record = get_record_type_from_node(self_node);
            if (self_record != NULL)
            {
                HashNode_t *method_node = semcheck_find_class_method(symtab,
                    self_record, id, NULL);
                if (method_node != NULL)
                {
                    ListNode_t *method_params = kgpc_type_get_procedure_params(method_node->type);
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    {
                        fprintf(stderr, "[SemCheck] Implicit Self injection? method_params_len=%d mangled=%s\n",
                            ListLength(method_params),
                            method_node->mangled_id ? method_node->mangled_id : "(null)");
                    }
                    if (method_params != NULL)
                    {
                        struct Expression *self_expr = mk_varid(expr->line_num, strdup("Self"));
                        ListNode_t *self_arg = CreateListNode(self_expr, LIST_EXPR);
                        self_arg->next = args_given;
                        expr->expr_data.function_call_data.args_expr = self_arg;
                        args_given = self_arg;
                    }
                    if (expr->expr_data.function_call_data.resolved_func == NULL)
                        expr->expr_data.function_call_data.resolved_func = method_node;
                    if (expr->expr_data.function_call_data.mangled_id == NULL)
                    {
                        const char *resolved_name = method_node->mangled_id ?
                            method_node->mangled_id :
                            (method_node->id ? method_node->id : id);
                        if (resolved_name != NULL)
                            expr->expr_data.function_call_data.mangled_id = strdup(resolved_name);
                    }
                }
            }
        }
    }

    if (id != NULL && strncmp(id, "__tfpg_ctor$", strlen("__tfpg_ctor$")) == 0)
    {
        if (type_return != NULL)
            *type_return = RECORD_TYPE;
        expr->resolved_type = RECORD_TYPE;
        return 0;
    }

    /* If this "call" is actually a type identifier, treat it as a typecast */
    int typecast_result = semcheck_try_reinterpret_as_typecast(type_return, symtab, expr, max_scope_lev);
    if (typecast_result != 0 || expr->type == EXPR_TYPECAST)
        return typecast_result;

    /* Detect calls through procedural fields of records (advanced records). The parser may have
     * rewritten `algo.Compare(x, y)` as a method call with `algo` injected as the first argument.
     * If the field is a procedural type, treat it as a procedural variable call instead. */
    if (id != NULL && args_given != NULL)
    {
        struct Expression *receiver_expr = (struct Expression *)args_given->cur;
        int recv_type = UNKNOWN_TYPE;
        semcheck_expr_main(&recv_type, symtab, receiver_expr, max_scope_lev, NO_MUTATE);

        struct RecordType *recv_record = NULL;
        if (recv_type == RECORD_TYPE)
        {
            recv_record = receiver_expr->record_type;
        }
        else if (recv_type == POINTER_TYPE)
        {
            if (receiver_expr->record_type != NULL)
                recv_record = receiver_expr->record_type;
            else if (receiver_expr->resolved_kgpc_type != NULL &&
                     receiver_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER)
            {
                KgpcType *pointee = receiver_expr->resolved_kgpc_type->info.points_to;
                if (pointee != NULL && kgpc_type_is_record(pointee))
                    recv_record = kgpc_type_get_record(pointee);
            }
        }
        if (recv_record == NULL && receiver_expr->type == EXPR_VAR_ID &&
            receiver_expr->expr_data.id != NULL)
        {
            HashNode_t *recv_node = NULL;
            if (FindIdent(&recv_node, symtab, receiver_expr->expr_data.id) == 0 && recv_node != NULL)
            {
                recv_record = get_record_type_from_node(recv_node);
                if (recv_record == NULL && recv_node->type != NULL &&
                    recv_node->type->kind == TYPE_KIND_POINTER &&
                    recv_node->type->info.points_to != NULL &&
                    kgpc_type_is_record(recv_node->type->info.points_to))
                {
                    recv_record = kgpc_type_get_record(recv_node->type->info.points_to);
                }
            }
        }

        if (recv_record != NULL)
        {
            const char *field_lookup = id;
            while (field_lookup != NULL && field_lookup[0] == '_' && field_lookup[1] == '_')
                field_lookup += 2;  /* allow __Prefixed identifiers to match field names */

            struct RecordField *field_desc = NULL;
            long long field_offset = 0;
            if (resolve_record_field(symtab, recv_record, field_lookup, &field_desc,
                                     &field_offset, expr->line_num, 1) == 0 &&
                field_desc != NULL)
            {
                int is_proc_field = (field_desc->type == PROCEDURE);
                KgpcType *proc_type = NULL;

                if (field_desc->type_id != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, field_desc->type_id) == 0 &&
                        type_node != NULL && type_node->type != NULL &&
                        type_node->type->kind == TYPE_KIND_PROCEDURE)
                    {
                        proc_type = type_node->type;
                        kgpc_type_retain(proc_type);
                        is_proc_field = 1;
                    }
                }

                if (is_proc_field)
                {
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck] treating %s.%s as procedural field call\n",
                            receiver_expr->type == EXPR_VAR_ID ? receiver_expr->expr_data.id : "<expr>", id);
                    }
                    /* Remove the receiver from the argument list */
                    ListNode_t *remaining_args = args_given->next;
                    expr->expr_data.function_call_data.args_expr = remaining_args;
                    args_given->cur = NULL;
                    free(args_given);

                    /* Build a record-access expression to the procedural field */
                    struct Expression *proc_expr = (struct Expression *)calloc(1, sizeof(struct Expression));
                    if (proc_expr == NULL)
                    {
                        fprintf(stderr, "Error on line %d: failed to allocate procedural field expression.\n",
                            expr->line_num);
                        *type_return = UNKNOWN_TYPE;
                        return ++return_val;
                    }
                    proc_expr->line_num = expr->line_num;
                    proc_expr->type = EXPR_RECORD_ACCESS;
                    proc_expr->expr_data.record_access_data.record_expr = receiver_expr;
                    proc_expr->expr_data.record_access_data.field_id = strdup(field_lookup);
                    proc_expr->expr_data.record_access_data.field_offset = (int)field_offset;
                    proc_expr->record_type = recv_record;
                    proc_expr->resolved_type = PROCEDURE;

                    /* Validate arguments against the procedural type if available */
                    if (proc_type != NULL)
                    {
                        ListNode_t *formal_params = kgpc_type_get_procedure_params(proc_type);
                        if (ListLength(formal_params) != ListLength(remaining_args))
                        {
                            fprintf(stderr, "Error on line %d, call to procedural field %s: expected %d arguments, got %d\n",
                                expr->line_num, id, ListLength(formal_params), ListLength(remaining_args));
                            if (proc_type != NULL)
                                destroy_kgpc_type(proc_type);
                            destroy_expr(proc_expr);
                            *type_return = UNKNOWN_TYPE;
                            return ++return_val;
                        }

                        ListNode_t *formal = formal_params;
                        ListNode_t *actual = remaining_args;
                        int arg_idx = 0;
                        while (formal != NULL && actual != NULL)
                        {
                            Tree_t *formal_decl = (Tree_t *)formal->cur;
                            struct Expression *actual_expr = (struct Expression *)actual->cur;
                            
                            int formal_type = resolve_param_type(formal_decl, symtab);
                            int actual_type = UNKNOWN_TYPE;
                            semcheck_expr_main(&actual_type, symtab, actual_expr, max_scope_lev, NO_MUTATE);

                            if (formal_type != UNKNOWN_TYPE && actual_type != UNKNOWN_TYPE &&
                                formal_type != actual_type)
                            {
                                if (!((formal_type == LONGINT_TYPE && actual_type == INT_TYPE) ||
                                      (formal_type == INT_TYPE && actual_type == LONGINT_TYPE) ||
                                      (formal_type == POINTER_TYPE) ||
                                      (actual_type == POINTER_TYPE)))
                                {
                                    fprintf(stderr, "Warning on line %d, argument %d type mismatch in call to procedural field %s\n",
                                        expr->line_num, arg_idx + 1, id);
                                }
                            }
                            
                            formal = formal->next;
                            actual = actual->next;
                            arg_idx++;
                        }

                        /* Cache call info for codegen */
                        expr->expr_data.function_call_data.call_kgpc_type = proc_type;
                        expr->expr_data.function_call_data.call_hash_type =
                            (kgpc_type_get_return_type(proc_type) == NULL) ? HASHTYPE_PROCEDURE : HASHTYPE_FUNCTION;
                        expr->expr_data.function_call_data.is_call_info_valid = 1;

                        KgpcType *ret_type = kgpc_type_get_return_type(proc_type);
                        if (ret_type != NULL && ret_type->kind == TYPE_KIND_PRIMITIVE)
                        {
                            *type_return = kgpc_type_get_primitive_tag(ret_type);
                            expr->resolved_type = *type_return;
                        }
                        else if (ret_type != NULL && ret_type->kind == TYPE_KIND_RECORD)
                        {
                            *type_return = RECORD_TYPE;
                            expr->resolved_type = RECORD_TYPE;
                            expr->record_type = kgpc_type_get_record(ret_type);
                        }
                        else if (ret_type != NULL && ret_type->kind == TYPE_KIND_POINTER)
                        {
                            *type_return = POINTER_TYPE;
                            expr->resolved_type = POINTER_TYPE;
                        }
                        else
                        {
                            *type_return = PROCEDURE;
                            expr->resolved_type = PROCEDURE;
                        }
                    }
                    else
                    {
                        *type_return = PROCEDURE;
                        expr->resolved_type = PROCEDURE;
                    }

                    expr->expr_data.function_call_data.is_procedural_var_call = 1;
                    expr->expr_data.function_call_data.procedural_var_symbol = NULL;
                    expr->expr_data.function_call_data.procedural_var_expr = proc_expr;

                    /* We no longer treat this as a method call; proceed with validated arguments */
                    return return_val;
                }
            }
        }
    }

    if (id != NULL && pascal_identifier_equals(id, "SizeOf"))
        return semcheck_builtin_sizeof(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "GetMem"))
    {
        ListNode_t *args = expr->expr_data.function_call_data.args_expr;
        if (args == NULL || args->next != NULL)
        {
            fprintf(stderr, "Error on line %d, GetMem expects exactly one argument.\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

        struct Expression *size_expr = (struct Expression *)args->cur;
        int size_type = UNKNOWN_TYPE;
        int error_count = semcheck_expr_main(&size_type, symtab, size_expr, max_scope_lev, NO_MUTATE);
        if (error_count != 0)
        {
            *type_return = UNKNOWN_TYPE;
            return error_count;
        }

        HashNode_t *best_match = NULL;
        ListNode_t *candidates = FindAllIdents(symtab, "GetMem");
        for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate == NULL)
                continue;
            if (candidate->hash_type != HASHTYPE_FUNCTION)
                continue;
            if (candidate->type == NULL || candidate->type->kind != TYPE_KIND_PROCEDURE)
                continue;
            ListNode_t *params = kgpc_type_get_procedure_params(candidate->type);
            if (ListLength(params) != 1)
                continue;
            best_match = candidate;
            break;
        }
        if (candidates != NULL)
            DestroyList(candidates);

        semcheck_reset_function_call_cache(expr);
        if (best_match != NULL && best_match->mangled_id != NULL)
        {
            if (expr->expr_data.function_call_data.mangled_id != NULL)
            {
                free(expr->expr_data.function_call_data.mangled_id);
                expr->expr_data.function_call_data.mangled_id = NULL;
            }
            expr->expr_data.function_call_data.mangled_id = strdup(best_match->mangled_id);
            if (expr->expr_data.function_call_data.mangled_id == NULL)
            {
                fprintf(stderr, "Error: failed to allocate mangled name for GetMem.\n");
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            semcheck_set_function_call_target(expr, best_match);
        }
        else
        {
            char *mangled_name = MangleFunctionNameFromCallSite("GetMem", args, symtab, max_scope_lev);
            if (mangled_name != NULL)
            {
                if (expr->expr_data.function_call_data.mangled_id != NULL)
                {
                    free(expr->expr_data.function_call_data.mangled_id);
                    expr->expr_data.function_call_data.mangled_id = NULL;
                }
                expr->expr_data.function_call_data.mangled_id = mangled_name;
            }
        }
        *type_return = POINTER_TYPE;
        expr->resolved_type = POINTER_TYPE;
        return 0;
    }

    if (id != NULL && pascal_identifier_equals(id, "ToSingleByteFileSystemEncodedFileName"))
    {
        ListNode_t *args = expr->expr_data.function_call_data.args_expr;
        if (args == NULL || args->next != NULL)
        {
            fprintf(stderr, "Error on line %d, ToSingleByteFileSystemEncodedFileName expects exactly one argument.\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

        int error_count = 0;
        struct Expression *arg_expr = (struct Expression *)args->cur;
        int arg_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
        if (error_count != 0)
        {
            *type_return = UNKNOWN_TYPE;
            return error_count;
        }

        semcheck_reset_function_call_cache(expr);
        *type_return = STRING_TYPE;
        expr->resolved_type = STRING_TYPE;
        return 0;
    }

    if (id != NULL && pascal_identifier_equals(id, "ArrayStringToPPchar"))
    {
        ListNode_t *args = expr->expr_data.function_call_data.args_expr;
        if (args == NULL || args->next == NULL || args->next->next != NULL)
        {
            fprintf(stderr, "Error on line %d, ArrayStringToPPchar expects exactly two arguments.\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

        int error_count = 0;
        struct Expression *arr_expr = (struct Expression *)args->cur;
        struct Expression *reserve_expr = (struct Expression *)args->next->cur;
        int arr_type = UNKNOWN_TYPE;
        int reserve_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_main(&arr_type, symtab, arr_expr, max_scope_lev, NO_MUTATE);
        error_count += semcheck_expr_main(&reserve_type, symtab, reserve_expr, max_scope_lev, NO_MUTATE);
        if (error_count != 0)
        {
            *type_return = UNKNOWN_TYPE;
            return error_count;
        }

        semcheck_reset_function_call_cache(expr);
        *type_return = POINTER_TYPE;
        expr->resolved_type = POINTER_TYPE;
        return 0;
    }

    if (id != NULL && pascal_identifier_equals(id, "Chr"))
        return semcheck_builtin_chr(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Ord"))
        return semcheck_builtin_ord(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Pred"))
        return semcheck_builtin_predsucc(type_return, symtab, expr, max_scope_lev, 0);

    if (id != NULL && pascal_identifier_equals(id, "Succ"))
        return semcheck_builtin_predsucc(type_return, symtab, expr, max_scope_lev, 1);

    if (id != NULL && pascal_identifier_equals(id, "Length"))
        return semcheck_builtin_length(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Copy"))
        return semcheck_builtin_copy(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Pos"))
        return semcheck_builtin_pos(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "StrPas"))
        return semcheck_builtin_strpas(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "EOF"))
        return semcheck_builtin_eof(type_return, symtab, expr, max_scope_lev);
    if (id != NULL && pascal_identifier_equals(id, "EOLN"))
        return semcheck_builtin_eoln(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Low"))
        return semcheck_builtin_lowhigh(type_return, symtab, expr, max_scope_lev, 0);

    if (id != NULL && pascal_identifier_equals(id, "High"))
        return semcheck_builtin_lowhigh(type_return, symtab, expr, max_scope_lev, 1);

    if (id != NULL && pascal_identifier_equals(id, "Default"))
        return semcheck_builtin_default(type_return, symtab, expr, max_scope_lev);

    /* Internal runtime function for open/dynamic array High - already resolved */
    if (id != NULL && strcmp(id, "kgpc_dynarray_compute_high") == 0)
    {
        /* This function was already set up by semcheck_builtin_lowhigh for dynamic arrays.
         * Just confirm it returns LONGINT_TYPE and proceed. */
        expr->resolved_type = LONGINT_TYPE;
        *type_return = LONGINT_TYPE;
        return 0;
    }

    if (id != NULL && pascal_identifier_equals(id, "Assigned"))
        return semcheck_builtin_assigned(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Abs"))
        return semcheck_builtin_abs(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "UpperCase"))
    {
        ListNode_t *args = expr->expr_data.function_call_data.args_expr;
        if (args != NULL && args->next == NULL)
        {
            struct Expression *arg_expr = (struct Expression *)args->cur;
            int arg_type = UNKNOWN_TYPE;
            int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
            if (error_count == 0 && arg_type == CHAR_TYPE)
            {
                if (expr->expr_data.function_call_data.mangled_id != NULL)
                {
                    free(expr->expr_data.function_call_data.mangled_id);
                    expr->expr_data.function_call_data.mangled_id = NULL;
                }
                if (expr->expr_data.function_call_data.id != NULL)
                {
                    free(expr->expr_data.function_call_data.id);
                    expr->expr_data.function_call_data.id = NULL;
                }
                expr->expr_data.function_call_data.id = strdup("kgpc_upcase_char");
                expr->expr_data.function_call_data.mangled_id = strdup("kgpc_upcase_char");
                if (expr->expr_data.function_call_data.mangled_id == NULL)
                {
                    fprintf(stderr, "Error: failed to allocate mangled name for UpperCase.\n");
                    *type_return = UNKNOWN_TYPE;
                    return 1;
                }
                semcheck_reset_function_call_cache(expr);
                if (expr->resolved_kgpc_type != NULL)
                {
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                    expr->resolved_kgpc_type = NULL;
                }
                expr->resolved_kgpc_type = create_primitive_type(CHAR_TYPE);
                expr->resolved_type = CHAR_TYPE;
                *type_return = CHAR_TYPE;
                return 0;
            }
        }
    }


    if (id != NULL && pascal_identifier_equals(id, "Sqrt"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sqrt", "kgpc_sqrt", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Sin"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sin", "kgpc_sin", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Csc"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Csc", "kgpc_csc", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Sinh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sinh", "kgpc_sinh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Csch"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Csch", "kgpc_csch", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Cos"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Cos", "kgpc_cos", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Sec"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sec", "kgpc_sec", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Cosh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Cosh", "kgpc_cosh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Sech"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sech", "kgpc_sech", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Tan"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Tan", "kgpc_tan", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Cot"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Cot", "kgpc_cot", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Tanh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Tanh", "kgpc_tanh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Coth"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Coth", "kgpc_coth", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcTan"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcTan", "kgpc_arctan", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCot"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCot", "kgpc_arccot", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcTan2"))
        return semcheck_builtin_arctan2(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Hypot"))
        return semcheck_builtin_hypot(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "ArcSin"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcSin", "kgpc_arcsin", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCos"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCos", "kgpc_arccos", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCosh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCosh", "kgpc_arccosh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcSech"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcSech", "kgpc_arcsech", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCsch"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCsch", "kgpc_arccsch", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCoth"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCoth", "kgpc_arccoth", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcSinh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcSinh", "kgpc_arcsinh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcTanh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcTanh", "kgpc_arctanh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "DegToRad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "DegToRad", "kgpc_deg_to_rad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "RadToDeg"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "RadToDeg", "kgpc_rad_to_deg", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "DegToGrad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "DegToGrad", "kgpc_deg_to_grad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "GradToDeg"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "GradToDeg", "kgpc_grad_to_deg", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "GradToRad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "GradToRad", "kgpc_grad_to_rad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "RadToGrad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "RadToGrad", "kgpc_rad_to_grad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "CycleToRad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "CycleToRad", "kgpc_cycle_to_rad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "RadToCycle"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "RadToCycle", "kgpc_rad_to_cycle", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Ln"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Ln", "kgpc_ln", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "LogN"))
        return semcheck_builtin_logn(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Exp"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Exp", "kgpc_exp", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Round"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Round", "kgpc_round", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Trunc"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Trunc", "kgpc_trunc", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Int"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Int", "kgpc_int", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Frac"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Frac", "kgpc_frac", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Ceil"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Ceil", "kgpc_ceil", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Floor"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Floor", "kgpc_floor", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "UpCase"))
    {
        ListNode_t *args = expr->expr_data.function_call_data.args_expr;
        if (args != NULL && args->next == NULL)
        {
            struct Expression *arg_expr = (struct Expression *)args->cur;
            int arg_type = UNKNOWN_TYPE;
            int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
            if (error_count == 0 && arg_type == CHAR_TYPE)
                return semcheck_builtin_upcase(type_return, symtab, expr, max_scope_lev);
            if (error_count == 0 && arg_type == STRING_TYPE &&
                arg_expr != NULL && arg_expr->type == EXPR_STRING &&
                arg_expr->expr_data.string != NULL &&
                strlen(arg_expr->expr_data.string) == 1)
                return semcheck_builtin_upcase(type_return, symtab, expr, max_scope_lev);
        }
    }

    if (id != NULL && pascal_identifier_equals(id, "Odd"))
        return semcheck_builtin_odd(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Sqr"))
        return semcheck_builtin_sqr(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Power"))
        return semcheck_builtin_power(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Random"))
        return semcheck_builtin_random(type_return, symtab, expr, max_scope_lev);
    
    if (id != NULL && pascal_identifier_equals(id, "RandomRange"))
        return semcheck_builtin_randomrange(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "RandSeed"))
        return semcheck_builtin_randseed(type_return, symtab, expr, max_scope_lev);

    /***** FIRST VERIFY FUNCTION IDENTIFIER *****/

    /***** FIRST VERIFY FUNCTION IDENTIFIER *****/

    /* Check for method call with unresolved name (member-access placeholder) where first arg is the type/instance. */
    if (expr->expr_data.function_call_data.is_method_call_placeholder && args_given != NULL) {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        int first_arg_type_tag;
        semcheck_expr_main(&first_arg_type_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);
        
        if (first_arg->resolved_kgpc_type != NULL) {
            KgpcType *owner_type = first_arg->resolved_kgpc_type;
            struct RecordType *record_info = NULL;
            
            if (owner_type->kind == TYPE_KIND_RECORD) {
                record_info = owner_type->info.record_info;
            } else if (owner_type->kind == TYPE_KIND_POINTER && owner_type->info.points_to->kind == TYPE_KIND_RECORD) {
                record_info = owner_type->info.points_to->info.record_info;
            }
            
            if (record_info != NULL && record_info->type_id != NULL) {
                const char *method_name = id;
                
                /* Check if this is a static method */
                int is_static = from_cparser_is_method_static(record_info->type_id, method_name);
                
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_funccall: __method call type=%s method=%s is_static=%d\n",
                        record_info->type_id, method_name, is_static);
                }
                
                /* Look up the method */
                HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, method_name, NULL);
                if (method_node != NULL) {
                    /* Resolve the method name */
                    set_type_from_hashtype(type_return, method_node);
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, method_node->type);
                    expr->expr_data.function_call_data.resolved_func = method_node;
                    const char *resolved_method_name = (method_node->mangled_id != NULL) ?
                        method_node->mangled_id : method_node->id;
                    if (expr->expr_data.function_call_data.mangled_id != NULL)
                        free(expr->expr_data.function_call_data.mangled_id);
                    expr->expr_data.function_call_data.mangled_id =
                        (resolved_method_name != NULL) ? strdup(resolved_method_name) : NULL;
                    
                    if (is_static) {
                        /* For static methods, remove the first argument (the type identifier) */
                        ListNode_t *old_head = args_given;
                        expr->expr_data.function_call_data.args_expr = old_head->next;
                        old_head->next = NULL;  /* Detach to prevent dangling reference */
                        args_given = expr->expr_data.function_call_data.args_expr;

                        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                            fprintf(stderr, "[SemCheck] semcheck_funccall: Removed type arg for static method call\n");
                        }
                    }

                    /* Update mangled_name to use the resolved name */
                    if (mangled_name != NULL)
                        free(mangled_name);
                    mangled_name = (resolved_method_name != NULL) ? strdup(resolved_method_name) : NULL;

                    /* Initialize overload candidates before jumping to avoid uninitialized access */
                    overload_candidates = CreateListNode(method_node, LIST_UNSPECIFIED);

                    /* Continue with normal function call processing using the resolved method */
                    hash_return = method_node;
                    goto method_call_resolved;
                }
            }
        }
    }
    
    /* Check for Constructor Call (Create) where first arg is the class type/instance */
    if (id != NULL && (strcasecmp(id, "Create") == 0 || strcasecmp(id, "Destroy") == 0) && args_given != NULL) {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        int first_arg_type_tag;
        semcheck_expr_main(&first_arg_type_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);
        
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
             fprintf(stderr, "[SemCheck] semcheck_funccall: first_arg=%p type=%d id=%s record_type=%p\n", 
                 (void*)first_arg, first_arg->type, 
                 (first_arg->type == EXPR_VAR_ID) ? first_arg->expr_data.id : "N/A",
                 first_arg->record_type);
        }
        
        /* Check if first arg is a TYPE (for class constructor) or INSTANCE (for method) */
        /* Actually, for MyException.Create, MyException is a TYPE (if static call) or VAR (if instance call) */
        /* If it's a TYPE, resolved_kgpc_type should be the class type? */
        /* Wait, if MyException is a TYPE, semcheck_expr returns TYPE_KIND_TYPE? */
        /* Or the type tag of the type? */
        
        if (first_arg->resolved_kgpc_type != NULL) {
             /* If it's a record/class type, look up the method in it */
             KgpcType *owner_type = first_arg->resolved_kgpc_type;
             struct RecordType *record_info = NULL;
             
             if (owner_type->kind == TYPE_KIND_RECORD) {
                 record_info = owner_type->info.record_info;
             } else if (owner_type->kind == TYPE_KIND_POINTER && owner_type->info.points_to->kind == TYPE_KIND_RECORD) {
                 record_info = owner_type->info.points_to->info.record_info;
             }
             
             if (record_info != NULL) {
                 HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, id, NULL);
                 if (method_node != NULL) {
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                         fprintf(stderr, "[SemCheck] semcheck_funccall: Found constructor/method %s in class\n", id);
                     }
                     
                     /* It's a method call! */
                     /* We need to set the return type */
                     set_type_from_hashtype(type_return, method_node);
                     semcheck_expr_set_resolved_kgpc_type_shared(expr, method_node->type);
                     expr->expr_data.function_call_data.resolved_func = method_node;
                     const char *resolved_method_name = (method_node->mangled_id != NULL) ?
                        method_node->mangled_id : method_node->id;
                     if (expr->expr_data.function_call_data.mangled_id != NULL)
                         free(expr->expr_data.function_call_data.mangled_id);
                     expr->expr_data.function_call_data.mangled_id =
                        (resolved_method_name != NULL) ? strdup(resolved_method_name) : NULL;
                     
                     if (strcasecmp(id, "Create") == 0) {
                         if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                             fprintf(stderr, "[SemCheck] semcheck_funccall: Fixing return type for Create\n");
                             fprintf(stderr, "[SemCheck]   expr=%p\n", (void*)expr);
                             fprintf(stderr, "[SemCheck]   owner_type kind=%d\n", owner_type->kind);
                         }

                         /* Return type is the class itself (pointer to record) */
                         if (owner_type->kind == TYPE_KIND_RECORD) {
                             if (first_arg->resolved_kgpc_type != NULL) {
                                 if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                     fprintf(stderr, "[SemCheck]   first_arg type kind=%d\n", first_arg->resolved_kgpc_type->kind);
                                 }
                                 
                                 /* Handle both POINTER (class) and RECORD (object/record) types */
                                 if (first_arg->resolved_kgpc_type->kind == TYPE_KIND_POINTER || 
                                     first_arg->resolved_kgpc_type->kind == TYPE_KIND_RECORD) {
                                      semcheck_expr_set_resolved_kgpc_type_shared(expr, first_arg->resolved_kgpc_type);
                                      *type_return = kgpc_type_get_legacy_tag(first_arg->resolved_kgpc_type);
                                      /* Also set record_type for compatibility */
                                      expr->record_type = first_arg->record_type;
                                      if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                          fprintf(stderr, "[SemCheck]   Set return type to %s (tag=%d), record_type=%p\n", 
                                              (first_arg->resolved_kgpc_type->kind == TYPE_KIND_POINTER) ? "POINTER" : "RECORD", 
                                              *type_return, (void*)expr->record_type);
                                      }
                                 }
                             }
                         } else {
                             semcheck_expr_set_resolved_kgpc_type_shared(expr, owner_type);
                             *type_return = kgpc_type_get_legacy_tag(owner_type);
                             /* Also set record_type for compatibility */
                             if (owner_type->kind == TYPE_KIND_POINTER && owner_type->info.points_to != NULL &&
                                 owner_type->info.points_to->kind == TYPE_KIND_RECORD) {
                                 expr->record_type = owner_type->info.points_to->info.record_info;
                             } else if (owner_type->kind == TYPE_KIND_RECORD) {
                                 expr->record_type = owner_type->info.record_info;
                             }
                             if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                 fprintf(stderr, "[SemCheck]   Set return type to owner type (tag=%d), record_type=%p\n", 
                                     *type_return, (void*)expr->record_type);
                             }
                         }
                     }
                     
                     /* Continue to normal argument processing below instead of returning early.
                      * This ensures constructor arguments are validated and processed correctly.
                      * The method_node and return type have already been set above. */
                     if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                         fprintf(stderr, "[SemCheck] After setting constructor return type:\n");
                         fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type=%p\n", (void*)expr->resolved_kgpc_type);
                         if (expr->resolved_kgpc_type != NULL) {
                             fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type->kind=%d\n", expr->resolved_kgpc_type->kind);
                         }
                         fprintf(stderr, "[SemCheck]   expr->record_type=%p\n", (void*)expr->record_type);
                     }
                     goto constructor_resolved;
                 }
             }
        }
    }

constructor_resolved:
    /* Constructor handling completed, continue with normal function call processing */

    /* If constructor was already resolved above, skip overload resolution */
    if (expr->expr_data.function_call_data.resolved_func != NULL &&
        expr->expr_data.function_call_data.mangled_id != NULL)
    {
        /* Constructor already resolved, skip to argument validation */
        hash_return = expr->expr_data.function_call_data.resolved_func;
        scope_return = 0; /* Constructor is in current scope */
        goto skip_overload_resolution;
    }

    if (id != NULL) {
        overload_candidates = FindAllIdents(symtab, id);
    }

    int prefer_non_builtin = 0;
    if (overload_candidates != NULL)
    {
        ListNode_t *cur = overload_candidates;
        while (cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL &&
                (candidate->hash_type == HASHTYPE_FUNCTION ||
                 candidate->hash_type == HASHTYPE_PROCEDURE))
            {
                if (!semcheck_candidate_is_builtin(symtab, candidate))
                {
                    prefer_non_builtin = 1;
                    break;
                }
            }
            cur = cur->next;
        }
    }

    /* Check if this is a call through a procedural variable */
    /* If 'id' resolves to a variable with a procedural type, handle it specially */
    if (overload_candidates != NULL && overload_candidates->cur != NULL)
    {
        HashNode_t *first_candidate = (HashNode_t *)overload_candidates->cur;
        if (first_candidate->hash_type == HASHTYPE_VAR &&
            first_candidate->type != NULL &&
            first_candidate->type->kind == TYPE_KIND_PROCEDURE)
        {
            /* This is a procedural variable - we're calling through a function pointer */
            KgpcType *proc_type = first_candidate->type;
            ListNode_t *formal_params = kgpc_type_get_procedure_params(proc_type);
            KgpcType *return_type = kgpc_type_get_return_type(proc_type);
            
            /* Validate arguments match the procedural type's signature */
            if (ListLength(formal_params) != ListLength(args_given))
            {
                fprintf(stderr, "Error on line %d, call to procedural variable %s: expected %d arguments, got %d\n",
                    expr->line_num, id, ListLength(formal_params), ListLength(args_given));
                destroy_list(overload_candidates);
                if (mangled_name != NULL) free(mangled_name);
                *type_return = UNKNOWN_TYPE;
                return ++return_val;
            }
            
            /* Check argument types */
            ListNode_t *formal = formal_params;
            ListNode_t *actual = args_given;
            int arg_idx = 0;
            while (formal != NULL && actual != NULL)
            {
                Tree_t *formal_decl = (Tree_t *)formal->cur;
                struct Expression *actual_expr = (struct Expression *)actual->cur;
                
                int formal_type = resolve_param_type(formal_decl, symtab);
                int actual_type = UNKNOWN_TYPE;
                semcheck_expr_main(&actual_type, symtab, actual_expr, max_scope_lev, NO_MUTATE);
                
                /* Simple type check - could be more sophisticated */
                if (formal_type != actual_type && formal_type != UNKNOWN_TYPE && actual_type != UNKNOWN_TYPE)
                {
                    /* Allow some type coercions like INT to LONGINT */
                    if (!((formal_type == LONGINT_TYPE && actual_type == INT_TYPE) ||
                          (formal_type == INT_TYPE && actual_type == LONGINT_TYPE) ||
                          (formal_type == POINTER_TYPE) || /* pointers are flexible */
                          (actual_type == POINTER_TYPE)))
                    {
                        fprintf(stderr, "Warning on line %d, argument %d type mismatch in call to procedural variable %s\n",
                            expr->line_num, arg_idx + 1, id);
                    }
                }
                
                formal = formal->next;
                actual = actual->next;
                arg_idx++;
            }
            
            /* Set the return type */
            if (return_type != NULL)
            {
                if (return_type->kind == TYPE_KIND_PRIMITIVE)
                {
                    *type_return = kgpc_type_get_primitive_tag(return_type);
                }
                else if (return_type->kind == TYPE_KIND_RECORD)
                {
                    *type_return = RECORD_TYPE;
                }
                else if (return_type->kind == TYPE_KIND_POINTER)
                {
                    *type_return = POINTER_TYPE;
                }
                else
                {
                    *type_return = UNKNOWN_TYPE;
                }
                
                semcheck_expr_set_resolved_kgpc_type_shared(expr, return_type);
            }
            else
            {
                /* It's a procedure (no return value) */
                *type_return = PROCEDURE;
            }
            
            /* Mark this as a procedural variable call */
            expr->expr_data.function_call_data.is_procedural_var_call = 1;
            expr->expr_data.function_call_data.procedural_var_symbol = first_candidate;
            
            destroy_list(overload_candidates);
            if (mangled_name != NULL) free(mangled_name);
            return 0;  /* Success */
        }
    }
    
    if (id == NULL) {
        fprintf(stderr, "Error on line %d: function call with NULL id\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        destroy_list(overload_candidates);
        return ++return_val;
    }
    mangled_name = MangleFunctionNameFromCallSite(id, args_given, symtab, max_scope_lev);
    if (mangled_name == NULL)
    {
        fprintf(stderr, "Error: failed to mangle function name for call to %s\n", id);
        *type_return = UNKNOWN_TYPE;
        destroy_list(overload_candidates);
        return ++return_val;
    }

method_call_resolved:
    ;  /* Label for jumping here after method resolution */
    int final_status = 0;

    HashNode_t *best_match = NULL;
    int best_score = 9999;
    int num_best_matches = 0;

    if (overload_candidates != NULL)
    {
        ListNode_t *cur = overload_candidates;
        while(cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate == NULL ||
                (candidate->hash_type != HASHTYPE_FUNCTION &&
                 candidate->hash_type != HASHTYPE_PROCEDURE))
            {
                cur = cur->next;
                continue;
            }
            if (prefer_non_builtin && semcheck_candidate_is_builtin(symtab, candidate))
            {
                cur = cur->next;
                continue;
            }

            /* Get formal arguments from KgpcType instead of deprecated args field */
            ListNode_t *candidate_args = kgpc_type_get_procedure_params(candidate->type);
            int total_params = ListLength(candidate_args);
            int required_params = count_required_params(candidate_args);
            int given_count = ListLength(args_given);
            
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] semcheck_funccall: candidate %s args=%d required=%d given=%d\n", 
                    candidate->id, total_params, required_params, given_count);
            }
            
            /* Match if given args is at least required count and at most total params */
            if (given_count >= required_params && given_count <= total_params)
            {
                int current_score = 0;
                ListNode_t *formal_args = candidate_args;
                ListNode_t *call_args = args_given;

                /* Score only the provided arguments */
                while(formal_args != NULL && call_args != NULL)
                {
                    Tree_t *formal_decl = (Tree_t *)formal_args->cur;
                    int formal_type = resolve_param_type(formal_decl, symtab);

                    int call_type;
                    struct Expression *call_expr = (struct Expression *)call_args->cur;
                    if (semcheck_prepare_array_literal_argument(formal_decl, call_expr,
                            symtab, max_scope_lev, expr->line_num) != 0)
                    {
                        *type_return = UNKNOWN_TYPE;
                        final_status = ++return_val;
                        goto funccall_cleanup;
                    }
                    if (semcheck_prepare_record_constructor_argument(formal_decl, call_expr,
                            symtab, max_scope_lev, expr->line_num) != 0)
                    {
                        *type_return = UNKNOWN_TYPE;
                        final_status = ++return_val;
                        goto funccall_cleanup;
                    }
                    semcheck_expr_main(&call_type, symtab, call_expr, max_scope_lev, NO_MUTATE);
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                         fprintf(stderr, "[SemCheck] semcheck_funccall: call_expr=%p type=%d id=%s record_type=%p\n", 
                             (void*)call_expr, call_expr->type, 
                             (call_expr->type == EXPR_VAR_ID) ? call_expr->expr_data.id : "N/A",
                             call_expr->record_type);
                    }
                    if (formal_decl != NULL && formal_decl->type == TREE_ARR_DECL &&
                        call_expr != NULL && call_expr->type == EXPR_ARRAY_LITERAL)
                    {
                        call_type = formal_type;
                    }

                    int pointer_penalty = 0;
                    if (formal_type == POINTER_TYPE && call_type == POINTER_TYPE && call_expr != NULL)
                    {
                        const char *formal_type_id = NULL;
                        int formal_owned = 0;
                        KgpcType *formal_kgpc = resolve_type_from_vardecl(formal_decl, symtab, &formal_owned);
                        int formal_subtype = UNKNOWN_TYPE;
                        const char *formal_subtype_id = NULL;
                        struct TypeAlias *formal_alias = NULL;
                        const char *formal_elem_type_id = NULL;

                        if (formal_decl != NULL && formal_decl->type == TREE_VAR_DECL)
                            formal_type_id = formal_decl->tree_data.var_decl_data.type_id;
                        else if (formal_decl != NULL && formal_decl->type == TREE_ARR_DECL)
                            formal_elem_type_id = formal_decl->tree_data.arr_decl_data.type_id;

                        if (formal_kgpc != NULL && kgpc_type_is_pointer(formal_kgpc))
                        {
                            formal_subtype = kgpc_type_get_pointer_subtype_tag(formal_kgpc);
                            if (formal_subtype == RECORD_TYPE)
                            {
                                KgpcType *points_to = formal_kgpc->info.points_to;
                                if (points_to != NULL && kgpc_type_is_record(points_to))
                                {
                                    struct RecordType *rec = kgpc_type_get_record(points_to);
                                    if (rec != NULL)
                                        formal_subtype_id = rec->type_id;
                                }
                            }
                        }
                        if (formal_subtype == UNKNOWN_TYPE)
                        {
                            if (formal_type_id != NULL)
                            {
                                HashNode_t *type_node = NULL;
                                if (FindIdent(&type_node, symtab, (char *)formal_type_id) != -1 &&
                                    type_node != NULL)
                                {
                                    formal_alias = get_type_alias_from_node(type_node);
                                }
                            }
                            if (formal_alias != NULL && formal_alias->is_pointer)
                            {
                                formal_subtype = formal_alias->pointer_type;
                                formal_subtype_id = formal_alias->pointer_type_id;
                            }
                            if (formal_subtype == UNKNOWN_TYPE && formal_subtype_id != NULL)
                            {
                                int mapped = semcheck_map_builtin_type_name(symtab, formal_subtype_id);
                                if (mapped != UNKNOWN_TYPE)
                                    formal_subtype = mapped;
                            }
                        }
                        if (formal_subtype == UNKNOWN_TYPE && formal_elem_type_id != NULL)
                        {
                            HashNode_t *elem_node = NULL;
                            if (FindIdent(&elem_node, symtab, (char *)formal_elem_type_id) != -1 &&
                                elem_node != NULL)
                            {
                                struct TypeAlias *elem_alias = get_type_alias_from_node(elem_node);
                                if (elem_alias != NULL && elem_alias->is_pointer)
                                {
                                    formal_subtype = elem_alias->pointer_type;
                                    formal_subtype_id = elem_alias->pointer_type_id;
                                }
                            }
                            if (formal_subtype_id == NULL &&
                                (formal_elem_type_id[0] == 'P' || formal_elem_type_id[0] == 'p') &&
                                formal_elem_type_id[1] != '\0')
                            {
                                formal_subtype_id = formal_elem_type_id + 1;
                            }
                            if (formal_subtype == UNKNOWN_TYPE && formal_subtype_id != NULL)
                            {
                                int mapped = semcheck_map_builtin_type_name(symtab, formal_subtype_id);
                                if (mapped != UNKNOWN_TYPE)
                                    formal_subtype = mapped;
                            }
                        }
                        if (formal_subtype_id == NULL && formal_type_id != NULL &&
                            (formal_type_id[0] == 'P' || formal_type_id[0] == 'p') &&
                            formal_type_id[1] != '\0')
                        {
                            formal_subtype_id = formal_type_id + 1;
                        }

                        int call_subtype = call_expr->pointer_subtype;
                        const char *call_subtype_id = call_expr->pointer_subtype_id;
                        const char *call_elem_type_id = NULL;
                        if (call_expr->is_array_expr)
                            call_elem_type_id = call_expr->array_element_type_id;
                        if (call_subtype_id == NULL && call_expr->resolved_kgpc_type != NULL)
                        {
                            struct TypeAlias *call_alias = kgpc_type_get_type_alias(call_expr->resolved_kgpc_type);
                            if (call_alias != NULL && call_alias->pointer_type_id != NULL)
                                call_subtype_id = call_alias->pointer_type_id;
                        }
                        if (call_subtype == UNKNOWN_TYPE && call_expr->resolved_kgpc_type != NULL &&
                            kgpc_type_is_pointer(call_expr->resolved_kgpc_type))
                        {
                            call_subtype = kgpc_type_get_pointer_subtype_tag(call_expr->resolved_kgpc_type);
                        }
                        if (call_subtype == UNKNOWN_TYPE && call_subtype_id != NULL)
                        {
                            HashNode_t *type_node = NULL;
                            if (FindIdent(&type_node, symtab, (char *)call_subtype_id) != -1 &&
                                type_node != NULL)
                            {
                                set_type_from_hashtype(&call_subtype, type_node);
                            }
                            if (call_subtype == UNKNOWN_TYPE)
                            {
                                int mapped = semcheck_map_builtin_type_name(symtab, call_subtype_id);
                                if (mapped != UNKNOWN_TYPE)
                                    call_subtype = mapped;
                            }
                        }
                        if (call_subtype == UNKNOWN_TYPE && call_elem_type_id != NULL)
                        {
                            HashNode_t *elem_node = NULL;
                            if (FindIdent(&elem_node, symtab, (char *)call_elem_type_id) != -1 &&
                                elem_node != NULL)
                            {
                                struct TypeAlias *elem_alias = get_type_alias_from_node(elem_node);
                                if (elem_alias != NULL && elem_alias->is_pointer)
                                {
                                    call_subtype = elem_alias->pointer_type;
                                    call_subtype_id = elem_alias->pointer_type_id;
                                }
                            }
                            if (call_subtype_id == NULL &&
                                (call_elem_type_id[0] == 'P' || call_elem_type_id[0] == 'p') &&
                                call_elem_type_id[1] != '\0')
                            {
                                call_subtype_id = call_elem_type_id + 1;
                            }
                            if (call_subtype == UNKNOWN_TYPE && call_subtype_id != NULL)
                            {
                                int mapped = semcheck_map_builtin_type_name(symtab, call_subtype_id);
                                if (mapped != UNKNOWN_TYPE)
                                    call_subtype = mapped;
                            }
                        }

                        if (formal_subtype != UNKNOWN_TYPE && call_subtype != UNKNOWN_TYPE)
                        {
                            if (formal_subtype != call_subtype)
                            {
                                pointer_penalty = 1000;
                            }
                            else if (formal_subtype_id != NULL && call_subtype_id != NULL)
                            {
                                const char *formal_norm = semcheck_normalize_char_type_id(formal_subtype_id);
                                const char *call_norm = semcheck_normalize_char_type_id(call_subtype_id);
                                if (formal_norm != NULL && call_norm != NULL)
                                {
                                    if (strcasecmp(formal_norm, call_norm) != 0)
                                        pointer_penalty = 1000;
                                }
                                else if (strcasecmp(formal_subtype_id, call_subtype_id) != 0)
                                {
                                    pointer_penalty = 1000;
                                }
                            }
                        }
                        else if (formal_subtype_id != NULL && call_subtype_id != NULL &&
                            strcasecmp(formal_subtype_id, call_subtype_id) != 0)
                        {
                            pointer_penalty = 1000;
                        }
                        if (pointer_penalty == 0 && formal_kgpc != NULL && call_expr->resolved_kgpc_type != NULL &&
                            kgpc_type_is_pointer(formal_kgpc) &&
                            kgpc_type_is_pointer(call_expr->resolved_kgpc_type))
                        {
                            KgpcType *formal_points_to = formal_kgpc->info.points_to;
                            KgpcType *call_points_to = call_expr->resolved_kgpc_type->info.points_to;
                            if (formal_points_to != NULL && call_points_to != NULL &&
                                formal_points_to->kind == TYPE_KIND_PRIMITIVE &&
                                call_points_to->kind == TYPE_KIND_PRIMITIVE &&
                                formal_points_to->info.primitive_type_tag == CHAR_TYPE &&
                                call_points_to->info.primitive_type_tag == CHAR_TYPE)
                            {
                                long long formal_size = kgpc_type_sizeof(formal_points_to);
                                long long call_size = kgpc_type_sizeof(call_points_to);
                                if (formal_size > 0 && call_size > 0 && formal_size != call_size)
                                {
                                    pointer_penalty = 1000;
                                }
                            }
                        }
                        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                        {
                            fprintf(stderr,
                                "[SemCheck] funccall ptrmatch: formal_sub=%d id=%s call_sub=%d id=%s penalty=%d\n",
                                formal_subtype,
                                formal_subtype_id ? formal_subtype_id : "<null>",
                                call_subtype,
                                call_subtype_id ? call_subtype_id : "<null>",
                                pointer_penalty);
                        }

                        if (formal_owned && formal_kgpc != NULL)
                            destroy_kgpc_type(formal_kgpc);
                    }

                    int is_string_literal = (call_expr != NULL && call_expr->type == EXPR_STRING);
                    if (formal_type == UNKNOWN_TYPE || formal_type == BUILTIN_ANY_TYPE)
                        current_score += 0;
                    else if (is_string_literal && formal_type == SHORTSTRING_TYPE &&
                        call_type == STRING_TYPE)
                        current_score += 0;
                    else if(formal_type == call_type)
                        current_score += 0;
                    else if (formal_type == LONGINT_TYPE && call_type == INT_TYPE)
                        current_score += 1;
                    else if (formal_type == INT64_TYPE &&
                        (call_type == LONGINT_TYPE || call_type == INT_TYPE))
                        current_score += 2;
                    else if (formal_type == STRING_TYPE && call_type == CHAR_TYPE)
                        current_score += 1; /* Allow implicit char-to-string promotion */
                    else
                        current_score += 1000; // Mismatch

                    current_score += pointer_penalty;
                    if (formal_type == LONGINT_TYPE && call_type == LONGINT_TYPE)
                    {
                        const char *formal_type_id = semcheck_get_param_type_id(formal_decl);
                        if (formal_type_id != NULL &&
                            (pascal_identifier_equals(formal_type_id, "Cardinal") ||
                             pascal_identifier_equals(formal_type_id, "LongWord") ||
                             pascal_identifier_equals(formal_type_id, "Word")))
                        {
                            current_score += 2;
                        }
                    }

                    formal_args = formal_args->next;
                    call_args = call_args->next;
                }
                
                /* Add small penalty for using default parameters (prefer exact match) */
                int missing_args = total_params - given_count;
                if (missing_args > 0)
                    current_score += missing_args;  /* Small penalty per default param used */

                if(current_score < best_score)
                {
                    best_score = current_score;
                    best_match = candidate;
                    num_best_matches = 1;
                }
                else if (current_score == best_score)
                {
                    int is_duplicate = 0;
                    if (best_match != NULL)
                    {
                        if (best_match->mangled_id != NULL &&
                            candidate->mangled_id != NULL &&
                            strcmp(best_match->mangled_id, candidate->mangled_id) == 0)
                        {
                            is_duplicate = 1;
                        }
                        else if ((best_match->mangled_id == NULL || candidate->mangled_id == NULL) &&
                            best_match->id != NULL && candidate->id != NULL &&
                            strcmp(best_match->id, candidate->id) == 0)
                        {
                            int best_params = ListLength(kgpc_type_get_procedure_params(best_match->type));
                            int cand_params = ListLength(kgpc_type_get_procedure_params(candidate->type));
                            if (best_params == cand_params)
                                is_duplicate = 1;
                        }
                    }
                    if (!is_duplicate && best_match != NULL &&
                        semcheck_candidates_share_signature(symtab, best_match, candidate))
                    {
                        is_duplicate = 1;
                    }
                    if (!is_duplicate)
                        num_best_matches++;
                }
            }
            cur = cur->next;
        }
    }

    if (num_best_matches == 1)
    {
        if (best_match != NULL && best_match->type != NULL &&
            best_match->type->kind == TYPE_KIND_PROCEDURE)
        {
            Tree_t *proc_def = best_match->type->info.proc_info.definition;
            int is_external = 0;
            if (proc_def != NULL)
            {
                is_external = proc_def->tree_data.subprogram_data.cname_flag != 0 ||
                    proc_def->tree_data.subprogram_data.cname_override != NULL;
            }
            if (!is_external &&
                (best_match->mangled_id == NULL ||
                 (best_match->id != NULL &&
                  strcmp(best_match->mangled_id, best_match->id) == 0)))
            {
                char *computed = MangleFunctionName(best_match->id,
                    best_match->type->info.proc_info.params, symtab);
                if (computed != NULL)
                {
                    if (best_match->mangled_id != NULL)
                        free(best_match->mangled_id);
                    best_match->mangled_id = computed;
                }
            }
        }
        const char *target_name = best_match->mangled_id;
        if (target_name == NULL || target_name[0] == '\0')
        {
            if (best_match->type != NULL && best_match->type->kind == TYPE_KIND_PROCEDURE)
            {
                Tree_t *proc_def = best_match->type->info.proc_info.definition;
                if (proc_def != NULL)
                {
                    target_name = proc_def->tree_data.subprogram_data.cname_override;
                    if (target_name == NULL)
                        target_name = proc_def->tree_data.subprogram_data.id;
                }
            }
            if (target_name == NULL || target_name[0] == '\0')
                target_name = best_match->id;
        }

        char *resolved_name = NULL;
        if (target_name != NULL)
            resolved_name = strdup(target_name);
        if (resolved_name == NULL)
        {
            fprintf(stderr, "Error: failed to duplicate mangled name for %s\n",
                best_match->id ? best_match->id : "(anonymous)");
            *type_return = UNKNOWN_TYPE;
            final_status = ++return_val;
            goto funccall_cleanup;
        }
        if (expr->expr_data.function_call_data.mangled_id != NULL)
            free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = resolved_name;
        if (best_match->type != NULL && best_match->type->kind == TYPE_KIND_PROCEDURE)
        {
            Tree_t *proc_def = best_match->type->info.proc_info.definition;
            if (proc_def != NULL)
            {
                bool no_body = (proc_def->tree_data.subprogram_data.statement_list == NULL);
                if (no_body)
                {
                    const char *target_name = proc_def->tree_data.subprogram_data.cname_override;
                    if (target_name == NULL)
                    {
                        if (proc_def->tree_data.subprogram_data.cname_flag)
                        {
                            target_name = proc_def->tree_data.subprogram_data.id;
                        }
                        else if (proc_def->tree_data.subprogram_data.mangled_id != NULL)
                        {
                            target_name = proc_def->tree_data.subprogram_data.mangled_id;
                        }
                        else if (best_match->mangled_id != NULL)
                        {
                            target_name = best_match->mangled_id;
                        }
                        else
                        {
                            target_name = proc_def->tree_data.subprogram_data.id;
                        }
                    }
                    if (target_name != NULL)
                    {
                        free(expr->expr_data.function_call_data.mangled_id);
                        expr->expr_data.function_call_data.mangled_id = strdup(target_name);
                    }
                }
                else if (proc_def->tree_data.subprogram_data.cname_flag &&
                         proc_def->tree_data.subprogram_data.mangled_id != NULL)
                {
                    free(expr->expr_data.function_call_data.mangled_id);
                    expr->expr_data.function_call_data.mangled_id =
                        strdup(proc_def->tree_data.subprogram_data.mangled_id);
                }
                else if (proc_def->tree_data.subprogram_data.mangled_id != NULL)
                {
                    free(expr->expr_data.function_call_data.mangled_id);
                    expr->expr_data.function_call_data.mangled_id =
                        strdup(proc_def->tree_data.subprogram_data.mangled_id);
                }
            }
        }
        semcheck_set_function_call_target(expr, best_match);
        semcheck_mark_call_requires_static_link(best_match);
        hash_return = best_match;
        scope_return = 0; // FIXME
    }
    else if (num_best_matches == 0)
    {
        fprintf(stderr, "Error on line %d, call to function %s does not match any available overload\n", expr->line_num, id);
        *type_return = UNKNOWN_TYPE;
        final_status = ++return_val;
        goto funccall_cleanup;
    }
    else
    {
        const char *ambig_env = getenv("KGPC_DEBUG_AMBIGUOUS");
        if (ambig_env != NULL && overload_candidates != NULL)
        {
            fprintf(stderr, "[KGPC] Ambiguous call to %s, %d best matches with score %d:\n",
                id ? id : "<unknown>", num_best_matches, best_score);
            ListNode_t *cur = overload_candidates;
            while (cur != NULL)
            {
                HashNode_t *candidate = (HashNode_t *)cur->cur;
                if (candidate != NULL && candidate->type != NULL &&
                    (candidate->hash_type == HASHTYPE_FUNCTION ||
                     candidate->hash_type == HASHTYPE_PROCEDURE))
                {
                    ListNode_t *candidate_args = kgpc_type_get_procedure_params(candidate->type);
                    fprintf(stderr, "  - %s mangled=%s args=%d kind=%d\n",
                        candidate->id ? candidate->id : "<anon>",
                        candidate->mangled_id ? candidate->mangled_id : "<null>",
                        ListLength(candidate_args),
                        candidate->type->kind);
                }
                cur = cur->next;
            }
        }
        fprintf(stderr, "Error on line %d, call to function %s is ambiguous\n", expr->line_num, id);
        *type_return = UNKNOWN_TYPE;
        final_status = ++return_val;
        goto funccall_cleanup;
    }


skip_overload_resolution:
    /* Overload resolution completed or skipped for constructors */

    if(scope_return == -1) // Should not happen if match_count > 0
    {
        fprintf(stderr, "Error on line %d, undeclared function %s (mangled to %s)!\n\n", expr->line_num, id, mangled_name);
        ++return_val;

        *type_return = UNKNOWN_TYPE;
    }
    else
    {
        set_hash_meta(hash_return, mutating);
        if(scope_return > max_scope_lev)
        {
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] semcheck_funccall: scope_return (%d) > max_scope_lev (%d)\n",
                    scope_return, max_scope_lev);
            }
            fprintf(stderr, "Error on line %d, cannot change \"%s\", invalid scope!\n\n",
                expr->line_num, id);
            fprintf(stderr, "[Was it defined above a function declaration?]\n\n");
            ++return_val;
        }
        if(hash_return->hash_type != HASHTYPE_FUNCTION &&
            hash_return->hash_type != HASHTYPE_FUNCTION_RETURN &&
            hash_return->hash_type != HASHTYPE_PROCEDURE)
        {
            fprintf(stderr, "Error on line %d, \"%s\" is not a function!\n\n",
                expr->line_num, id);
            ++return_val;
        }

        set_type_from_hashtype(type_return, hash_return);
        
        /* NEW: Also set the resolved KgpcType for this expression */
        if (hash_return->type != NULL && hash_return->type->kind == TYPE_KIND_PROCEDURE)
        {
            KgpcType *return_type = kgpc_type_get_return_type(hash_return->type);
            if (return_type != NULL)
            {
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] Overwriting resolved_kgpc_type from hash_return return_type\n");
                    fprintf(stderr, "[SemCheck]   return_type kind=%d\n", return_type->kind);
                }
                semcheck_expr_set_resolved_kgpc_type_shared(expr, return_type);
                if (return_type->kind == TYPE_KIND_ARRAY)
                    semcheck_set_array_info_from_kgpctype(expr, symtab, return_type, expr->line_num);
                else
                    semcheck_clear_array_info(expr);
                if (return_type->kind == TYPE_KIND_PRIMITIVE &&
                    return_type->info.primitive_type_tag == UNKNOWN_TYPE)
                {
                    char *target_return_id = hash_return->type->info.proc_info.return_type_id;
                    if (target_return_id != NULL)
                    {
                        HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type(symtab, target_return_id);
                        if (type_node != NULL && type_node->type != NULL)
                        {
                            destroy_kgpc_type(return_type);
                            kgpc_type_retain(type_node->type);
                            hash_return->type->info.proc_info.return_type = type_node->type;
                            return_type = type_node->type;
                            semcheck_expr_set_resolved_kgpc_type_shared(expr, type_node->type);
                            if (return_type->kind == TYPE_KIND_ARRAY)
                                semcheck_set_array_info_from_kgpctype(expr, symtab, return_type, expr->line_num);
                            else
                                semcheck_clear_array_info(expr);
                        }
                    }
                }
            }
            else
            {
                /* Do not clear resolved_kgpc_type here, as it may have been set
                 * by constructor handling logic earlier. */
                semcheck_clear_array_info(expr);
            }
        }
        else
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, hash_return->type);
            semcheck_clear_array_info(expr);
        }

        if (*type_return == RECORD_TYPE)
        {
            struct RecordType *record_type = get_record_type_from_node(hash_return);
            if (record_type != NULL)
                expr->record_type = record_type;
            else
                expr->record_type = NULL;
        }
        else
        {
            expr->record_type = NULL;
        }

        if (hash_return->type != NULL && hash_return->type->kind == TYPE_KIND_PROCEDURE)
        {
            ListNode_t *formal_params = kgpc_type_get_procedure_params(hash_return->type);
            if (append_default_args(&args_given, formal_params, expr->line_num) != 0)
                ++return_val;
            expr->expr_data.function_call_data.args_expr = args_given;
        }

        /***** THEN VERIFY ARGS INSIDE *****/
        cur_arg = 0;
        /* Get formal arguments from KgpcType instead of deprecated args field */
        true_args = kgpc_type_get_procedure_params(hash_return->type);
        
        /* For constructors, skip the first argument (class type) in args_given
         * AND skip the first formal parameter (Self) in true_args */
        ListNode_t *args_to_validate = args_given;
        ListNode_t *true_args_to_validate = true_args;
        
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] Constructor %s: args_given=%d, true_args=%d\n",
                id, ListLength(args_given), ListLength(true_args));
        }

        if (id != NULL && (strcasecmp(id, "Create") == 0 || strcasecmp(id, "Destroy") == 0) &&
            args_given != NULL)
        {
            /* If lengths match, we assume first arg is class type and first param is Self -> skip both */
            if (ListLength(args_given) == ListLength(true_args)) {
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) fprintf(stderr, "[SemCheck] Skipping both Self and Class Type\n");
                args_to_validate = args_given->next;
                if (true_args_to_validate != NULL)
                    true_args_to_validate = true_args_to_validate->next;
            }
            /* If args_given is one less, we assume class type is implicit but Self is explicit in params -> skip Self only */
            else if (ListLength(args_given) == ListLength(true_args) - 1) {
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) fprintf(stderr, "[SemCheck] Skipping Self only\n");
                if (true_args_to_validate != NULL)
                    true_args_to_validate = true_args_to_validate->next;
            }
        }
            
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                int args_given_count = 0;
                int true_args_count = 0;
                int args_to_validate_count = 0;
                for (ListNode_t *c = args_given; c != NULL; c = c->next) args_given_count++;
                for (ListNode_t *c = true_args_to_validate; c != NULL; c = c->next) true_args_count++;
                for (ListNode_t *c = args_to_validate; c != NULL; c = c->next) args_to_validate_count++;
                fprintf(stderr, "[SemCheck] Constructor %s: args_given=%d, true_args=%d, args_to_validate=%d\n",
                    id, args_given_count, true_args_count, args_to_validate_count);
            }
        
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] Before validation loop: true_args len=%d, args_given len=%d\n", 
                ListLength(true_args), ListLength(args_given));
            
            ListNode_t *cur = true_args;
            int i = 0;
            while (cur != NULL) {
                Tree_t *decl = (Tree_t *)cur->cur;
                if (decl->type == TREE_VAR_DECL) {
                    ListNode_t *ids = decl->tree_data.var_decl_data.ids;
                    while (ids != NULL) {
                        fprintf(stderr, "[SemCheck]   true_arg[%d]: %s\n", i++, (char*)ids->cur);
                        ids = ids->next;
                    }
                } else {
                    fprintf(stderr, "[SemCheck]   true_arg[%d]: (array decl)\n", i++);
                }
                cur = cur->next;
            }
            
            cur = args_given;
            i = 0;
            while (cur != NULL) {
                struct Expression *e = (struct Expression *)cur->cur;
                fprintf(stderr, "[SemCheck]   arg_given[%d]: type=%d\n", i++, e->type);
                cur = cur->next;
            }
        }
        
        while(true_args_to_validate != NULL && args_to_validate != NULL)
        {
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] validation loop: arg %d\n", cur_arg);
                fprintf(stderr, "[SemCheck]   true_args_to_validate=%p next=%p\n", 
                    (void*)true_args_to_validate, (void*)true_args_to_validate->next);
                fprintf(stderr, "[SemCheck]   args_to_validate=%p next=%p\n", 
                    (void*)args_to_validate, (void*)args_to_validate->next);
            }
            cur_arg++;
            assert(args_to_validate->type == LIST_EXPR);
            assert(true_args_to_validate->type == LIST_TREE);

            arg_decl = (Tree_t *)true_args_to_validate->cur;
            if (arg_decl->type != TREE_VAR_DECL && arg_decl->type != TREE_ARR_DECL)
            {
                fprintf(stderr, "Error on line %d, unsupported parameter declaration in call to %s.\n",
                    expr->line_num, id);
                ++return_val;
                true_args_to_validate = true_args_to_validate->next;
                args_to_validate = args_to_validate->next;
                continue;
            }
            struct Expression *current_arg_expr = (struct Expression *)args_to_validate->cur;
            if (arg_decl->type == TREE_ARR_DECL)
            {
                if (semcheck_prepare_array_literal_argument(arg_decl, current_arg_expr,
                        symtab, max_scope_lev, expr->line_num) != 0)
                {
                    ++return_val;
                    true_args_to_validate = true_args_to_validate->next;
                    args_to_validate = args_to_validate->next;
                    continue;
                }
            }
            if (semcheck_prepare_record_constructor_argument(arg_decl, current_arg_expr,
                    symtab, max_scope_lev, expr->line_num) != 0)
            {
                ++return_val;
                true_args_to_validate = true_args_to_validate->next;
                args_to_validate = args_to_validate->next;
                continue;
            }

            return_val += semcheck_expr_main(&arg_type,
                symtab, current_arg_expr, max_scope_lev, NO_MUTATE);
            if (arg_decl->type == TREE_VAR_DECL)
                true_arg_ids = arg_decl->tree_data.var_decl_data.ids;
            else
                true_arg_ids = arg_decl->tree_data.arr_decl_data.ids;

            while(true_arg_ids != NULL && args_to_validate != NULL)
            {
                int expected_type = resolve_param_type(arg_decl, symtab);

                if (arg_decl->type == TREE_ARR_DECL && current_arg_expr != NULL &&
                    current_arg_expr->type == EXPR_ARRAY_LITERAL)
                {
                    arg_type = expected_type;
                }
                if (expected_type == UNKNOWN_TYPE || expected_type == BUILTIN_ANY_TYPE)
                {
                    /* Untyped parameters accept any argument type. */
                    /* No validation needed. */
                }
                else if(arg_type != expected_type)
                {
                    /* Allow integer type conversion (INT_TYPE, LONGINT_TYPE, INT64_TYPE all compatible) */
                    int type_compatible = 0;
                    if (is_integer_type(arg_type) && is_integer_type(expected_type))
                    {
                        type_compatible = 1;
                    }
                    else if (expected_type == STRING_TYPE && arg_type == CHAR_TYPE)
                    {
                        type_compatible = 1;
                    }
                    else if (expected_type == SHORTSTRING_TYPE && arg_type == STRING_TYPE)
                    {
                        type_compatible = 1;
                    }
                    else if (expected_type == REAL_TYPE && is_integer_type(arg_type))
                    {
                        type_compatible = 1;
                    }
                    /* For complex types (records, files, etc.), if both have the same type tag,
                     * consider them compatible. The overload resolution already ensured we have
                     * the right function via name mangling. */
                    else if (arg_type == expected_type)
                    {
                        type_compatible = 1;
                    }
                    
                    if (!type_compatible)
                    {
                        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                        {
                            fprintf(stderr,
                                "[SemCheck] call %s arg %d type mismatch: expected=%d actual=%d\n",
                                id ? id : "<null>", cur_arg, expected_type, arg_type);
                        }
                        fprintf(stderr, "Error on line %d, on function call %s, argument %d: Type mismatch!\n\n",
                            expr->line_num, id, cur_arg);
                        ++return_val;
                    }
                }

                true_arg_ids = true_arg_ids->next;
                args_to_validate = args_to_validate->next;
            }

            true_args_to_validate = true_args_to_validate->next;
        }
        if(true_args_to_validate == NULL && args_to_validate != NULL)
        {
            fprintf(stderr, "Error on line %d, on function call %s, too many arguments given!\n\n",
                expr->line_num, id);
            ++return_val;
        }
        else if(true_args_to_validate != NULL && args_to_validate == NULL)
        {
            fprintf(stderr, "Error on line %d, on function call %s, not enough arguments given!\n\n",
                expr->line_num, id);
            ++return_val;
        }
    }

    final_status = return_val;

funccall_cleanup:
    if (overload_candidates != NULL)
        DestroyList(overload_candidates);
    if (mangled_name != NULL)
        free(mangled_name);
    return final_status;
}
static int semcheck_builtin_random(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_random_real");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Random.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        if (expr->resolved_kgpc_type != NULL)
        {
            destroy_kgpc_type(expr->resolved_kgpc_type);
            expr->resolved_kgpc_type = NULL;
        }
        expr->resolved_kgpc_type = create_primitive_type(REAL_TYPE);
        expr->resolved_type = REAL_TYPE;
        *type_return = REAL_TYPE;
        return 0;
    }

    if (args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Random expects zero or one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *upper_expr = (struct Expression *)args->cur;
    int upper_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&upper_type, symtab, upper_expr, max_scope_lev, NO_MUTATE);
    int is_real_upper = (upper_type == REAL_TYPE);
    if (!is_real_upper && upper_type != INT_TYPE && upper_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Random parameter must be numeric.\n",
            expr->line_num);
        ++error_count;
    }

    if (error_count != 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    if (is_real_upper)
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_random_real_upper");
    else
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_random_int");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for Random.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    if (is_real_upper)
        expr->resolved_type = REAL_TYPE;
    else if (upper_type == LONGINT_TYPE)
        expr->resolved_type = LONGINT_TYPE;
    else
        expr->resolved_type = INT_TYPE;
    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    expr->resolved_kgpc_type = create_primitive_type(expr->resolved_type);
    *type_return = expr->resolved_type;
    return 0;
}

static int semcheck_builtin_randomrange(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, RandomRange expects exactly two arguments.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *low_expr = (struct Expression *)args->cur;
    struct Expression *high_expr = (struct Expression *)args->next->cur;
    int low_type = UNKNOWN_TYPE;
    int high_type = UNKNOWN_TYPE;
    int error_count = 0;

    error_count += semcheck_expr_main(&low_type, symtab, low_expr, max_scope_lev, NO_MUTATE);
    error_count += semcheck_expr_main(&high_type, symtab, high_expr, max_scope_lev, NO_MUTATE);

    if (low_type != INT_TYPE && low_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, RandomRange lower bound must be integer.\n",
            expr->line_num);
        ++error_count;
    }
    if (high_type != INT_TYPE && high_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, RandomRange upper bound must be integer.\n",
            expr->line_num);
        ++error_count;
    }

    if (error_count != 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    expr->expr_data.function_call_data.mangled_id = strdup("kgpc_random_range");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for RandomRange.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }

    int result_type = (low_type == LONGINT_TYPE || high_type == LONGINT_TYPE)
        ? LONGINT_TYPE : INT_TYPE;
    expr->resolved_type = result_type;
    expr->resolved_kgpc_type = create_primitive_type(result_type);
    *type_return = result_type;
    return 0;
}

static int semcheck_builtin_power(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Power expects exactly two arguments.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *base_expr = (struct Expression *)args->cur;
    struct Expression *exp_expr = (struct Expression *)args->next->cur;
    int base_type = UNKNOWN_TYPE;
    int exp_type = UNKNOWN_TYPE;
    int error_count = 0;

    error_count += semcheck_expr_main(&base_type, symtab, base_expr, max_scope_lev, NO_MUTATE);
    error_count += semcheck_expr_main(&exp_type, symtab, exp_expr, max_scope_lev, NO_MUTATE);

    if (base_type != REAL_TYPE && base_type != INT_TYPE && base_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Power base must be numeric.\n", expr->line_num);
        ++error_count;
    }
    if (exp_type != REAL_TYPE && exp_type != INT_TYPE && exp_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Power exponent must be numeric.\n",
            expr->line_num);
        ++error_count;
    }

    if (error_count != 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    expr->expr_data.function_call_data.mangled_id = strdup("kgpc_power");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for Power.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    expr->resolved_type = REAL_TYPE;
    expr->resolved_kgpc_type = create_primitive_type(REAL_TYPE);
    *type_return = REAL_TYPE;
    return 0;
}

static int semcheck_builtin_randseed(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    (void)symtab;
    (void)max_scope_lev;

    assert(type_return != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    if (expr->expr_data.function_call_data.args_expr != NULL)
    {
        fprintf(stderr, "Error on line %d, RandSeed expects no arguments.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    expr->expr_data.function_call_data.mangled_id = strdup("kgpc_get_randseed");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for RandSeed.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    expr->resolved_type = LONGINT_TYPE;
    expr->resolved_kgpc_type = create_primitive_type(LONGINT_TYPE);
    *type_return = LONGINT_TYPE;
    return 0;
}
