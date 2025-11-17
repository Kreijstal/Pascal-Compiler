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
#include "../../ParseTree/GpcType.h"
#include "../../../identifier_utils.h"
#include "../../../format_arg.h"

int is_type_ir(int *type);
static int types_numeric_compatible(int lhs, int rhs);
static void semcheck_coerce_char_string_operands(int *type_first, struct Expression *expr1,
    int *type_second, struct Expression *expr2);

/* Helper function to get type name from an expression for operator overloading */
static const char *get_expr_type_name(struct Expression *expr, SymTab_t *symtab)
{
    if (expr == NULL)
        return NULL;
    
    /* Try to get from record_type field (legacy) */
    if (expr->record_type != NULL && expr->record_type->type_id != NULL)
        return expr->record_type->type_id;
    
    /* Try to get from resolved_gpc_type */
    if (expr->resolved_gpc_type != NULL && gpc_type_is_record(expr->resolved_gpc_type))
    {
        struct RecordType *rec = gpc_type_get_record(expr->resolved_gpc_type);
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
            if (gpc_type_is_record(node->type))
            {
                struct RecordType *rec = gpc_type_get_record(node->type);
                
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

int is_and_or(int *type);

static void semcheck_expr_set_call_gpc_type(struct Expression *expr, GpcType *type,
    int owns_existing)
{
    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return;

    if (expr->expr_data.function_call_data.call_gpc_type != NULL && owns_existing)
    {
        destroy_gpc_type(expr->expr_data.function_call_data.call_gpc_type);
    }
    expr->expr_data.function_call_data.call_gpc_type = NULL;

    if (type != NULL)
    {
        gpc_type_retain(type);
        expr->expr_data.function_call_data.call_gpc_type = type;
    }
}

static void semcheck_expr_set_resolved_gpc_type_shared(struct Expression *expr, GpcType *type)
{
    if (expr == NULL)
        return;

    if (expr->resolved_gpc_type != NULL)
    {
        destroy_gpc_type(expr->resolved_gpc_type);
        expr->resolved_gpc_type = NULL;
    }

    if (type != NULL)
    {
        gpc_type_retain(type);
        expr->resolved_gpc_type = type;
    }
}

static void semcheck_reset_function_call_cache(struct Expression *expr)
{
    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return;

    int had_call_info = (expr->expr_data.function_call_data.is_call_info_valid == 1);
    expr->expr_data.function_call_data.resolved_func = NULL;
    expr->expr_data.function_call_data.call_hash_type = HASHTYPE_VAR;
    semcheck_expr_set_call_gpc_type(expr, NULL, had_call_info);
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
    semcheck_expr_set_call_gpc_type(expr, target->type, had_call_info);
    expr->expr_data.function_call_data.is_call_info_valid = 1;
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

    return hashnode_get_record_type(parent_node);
}

struct ClassProperty *semcheck_find_class_property(SymTab_t *symtab,
    struct RecordType *record_info, const char *property_name,
    struct RecordType **owner_out)
{
    if (owner_out != NULL)
        *owner_out = NULL;
    if (symtab == NULL || record_info == NULL || property_name == NULL)
        return NULL;

    struct RecordType *current = record_info;
    while (current != NULL)
    {
        ListNode_t *node = current->properties;
        while (node != NULL)
        {
#ifdef DEBUG_PROPERTY_RESOLVE
            fprintf(stderr, "[DEBUG] inspecting property node type=%d\n", node->type);
#endif
            if (node->type == LIST_CLASS_PROPERTY && node->cur != NULL)
            {
                struct ClassProperty *property = (struct ClassProperty *)node->cur;
#ifdef DEBUG_PROPERTY_RESOLVE
                fprintf(stderr, "[DEBUG] candidate property '%s' (len=%zu) vs '%s' (len=%zu)\n",
                    property->name ? property->name : "<unnamed>",
                    property->name ? strlen(property->name) : 0,
                    property_name ? property_name : "<null>",
                    property_name ? strlen(property_name) : 0);
#endif
                if (property->name != NULL &&
                    pascal_identifier_equals(property->name, property_name))
                {
#ifdef DEBUG_PROPERTY_RESOLVE
                    fprintf(stderr, "[DEBUG] matched property %s\n", property->name);
#endif
                    if (owner_out != NULL)
                        *owner_out = current;
                    return property;
                }
            }
            node = node->next;
        }
        current = semcheck_lookup_parent_record(symtab, current);
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
            size_t class_len = strlen(current->type_id);
            size_t method_len = strlen(method_name);
            size_t total = class_len + 2 + method_len + 1;
            char *candidate = (char *)malloc(total);
            if (candidate == NULL)
                return NULL;

            snprintf(candidate, total, "%s__%s", current->type_id, method_name);

            HashNode_t *method_node = NULL;
            int find_result = FindIdent(&method_node, symtab, candidate);
            free(candidate);

            if (find_result != -1 && method_node != NULL)
            {
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
int semcheck_prepare_array_literal_argument(Tree_t *formal_decl, struct Expression *arg_expr,
    SymTab_t *symtab, int max_scope_lev, int line_num);

/* Helper function to get TypeAlias from HashNode, preferring GpcType when available */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    return hashnode_get_type_alias(node);
}

/* Helper function to get RecordType from HashNode */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    return hashnode_get_record_type(node);
}

/* Helper function to check if a node is a record type */
static inline int node_is_record_type(HashNode_t *node)
{
    return hashnode_is_record(node);
}

static int semcheck_pointer_deref(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
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
static struct Expression *clone_expression(const struct Expression *expr);
static struct RecordType *resolve_record_type_for_with(SymTab_t *symtab,
    struct Expression *context_expr, int expr_type, int line_num);
static int semcheck_builtin_lowhigh(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int is_high);
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
static int resolve_record_field(SymTab_t *symtab, struct RecordType *record,
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

static void semcheck_set_array_info_from_gpctype(struct Expression *expr, SymTab_t *symtab,
    GpcType *array_type, int line_num)
{
    if (expr == NULL || array_type == NULL || array_type->kind != TYPE_KIND_ARRAY)
        return;

    semcheck_clear_array_info(expr);
    expr->is_array_expr = 1;
    expr->array_lower_bound = array_type->info.array_info.start_index;
    expr->array_upper_bound = array_type->info.array_info.end_index;
    expr->array_is_dynamic = gpc_type_is_dynamic_array(array_type);
    expr->array_element_size = (int)gpc_type_get_array_element_size(array_type);

    GpcType *element_type = gpc_type_get_array_element_type(array_type);
    if (element_type != NULL)
    {
        expr->array_element_type = gpc_type_get_legacy_tag(element_type);
        if (element_type->kind == TYPE_KIND_RECORD)
            expr->array_element_record_type = gpc_type_get_record(element_type);
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

    HashNode_t *type_node = NULL;
    if (FindIdent(&type_node, symtab, (char *)type_id) == -1 || type_node == NULL)
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

static void semcheck_set_array_info_from_alias(struct Expression *expr, SymTab_t *symtab,
    struct TypeAlias *alias, int line_num)
{
    if (expr == NULL)
        return;

    semcheck_clear_array_info(expr);
    if (alias == NULL || !alias->is_array)
        return;

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
    if (node == NULL || node->hash_type != HASHTYPE_ARRAY)
        return;

    expr->is_array_expr = 1;
    
    /* Get array bounds from GpcType */
    int node_lower_bound, node_upper_bound;
    hashnode_get_array_bounds(node, &node_lower_bound, &node_upper_bound);
    
    /* Get element size from GpcType */
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
        /* For inline array declarations (no TypeAlias), extract info from GpcType */
        GpcType *element_type = node->type->info.array_info.element_type;
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
            /* Other element types (pointers, etc.) can be added here
             * when needed. For now, primitive, record, and array types cover the common cases. */
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
        expr->array_element_size = (int)sizeof(gpc_tvarrec);
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
        arg_expr->array_element_size = (int)sizeof(gpc_tvarrec);
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

static struct Expression *clone_expression(const struct Expression *expr)
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

static int resolve_record_field(SymTab_t *symtab, struct RecordType *record,
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

        expr->expr_data.function_call_data.mangled_id = strdup("gpc_chr");
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

        mangled_name = "gpc_ord_string";
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

        mangled_name = "gpc_ord_longint";
    }
    else if (arg_type == INT_TYPE || arg_type == LONGINT_TYPE)
    {
        mangled_name = "gpc_ord_longint";
    }
    else if (arg_type == CHAR_TYPE)
    {
        /* For char variables, Ord returns the character code */
        mangled_name = "gpc_ord_longint";
    }
    else if (arg_type == ENUM_TYPE)
    {
        /* For enumerative types, Ord returns the ordinal value (0-based index) */
        mangled_name = "gpc_ord_longint";
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
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Length expects exactly one argument.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
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
    if (error_count == 0 && arg_type == STRING_TYPE)
        mangled_name = "gpc_string_length";
    else if (error_count == 0 && is_dynamic_array)
        mangled_name = "__gpc_dynarray_length";
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
    if (error_count == 0 && index_type != INT_TYPE && index_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Copy index must be an integer.\n", expr->line_num);
        error_count++;
    }

    int count_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&count_type, symtab, count_expr, max_scope_lev, NO_MUTATE);
    if (error_count == 0 && count_type != INT_TYPE && count_type != LONGINT_TYPE)
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
        expr->expr_data.function_call_data.mangled_id = strdup("gpc_string_copy");
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
    if (error_count == 0 && substr_type != STRING_TYPE)
    {
        fprintf(stderr, "Error on line %d, Pos substring must be a string.\n", expr->line_num);
        ++error_count;
    }

    int value_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);
    if (error_count == 0 && value_type != STRING_TYPE)
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
        expr->expr_data.function_call_data.mangled_id = strdup("gpc_string_pos");
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
        mangled_name = "gpc_text_eof_default";
    }
    else if (args->next == NULL)
    {
        struct Expression *file_expr = (struct Expression *)args->cur;
        int file_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_main(&file_type, symtab, file_expr, max_scope_lev, NO_MUTATE);
        if (file_type != TEXT_TYPE)
        {
            fprintf(stderr, "Error on line %d, EOF expects a text file argument.\n", expr->line_num);
            error_count++;
        }
        else
        {
            mangled_name = "gpc_text_eof";
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
        mangled_name = "gpc_text_eoln_default";
    }
    else if (args->next == NULL)
    {
        struct Expression *file_expr = (struct Expression *)args->cur;
        int file_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_main(&file_type, symtab, file_expr, max_scope_lev, NO_MUTATE);
        if (file_type != TEXT_TYPE)
        {
            fprintf(stderr, "Error on line %d, EOLN expects a text file argument.\n", expr->line_num);
            error_count++;
        }
        else
        {
            mangled_name = "gpc_text_eoln";
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

    if (expr->expr_data.function_call_data.mangled_id != NULL)
        free(expr->expr_data.function_call_data.mangled_id);
    expr->expr_data.function_call_data.mangled_id = strdup("gpc_dynarray_compute_high");
    semcheck_reset_function_call_cache(expr);

    int error_count = 0;
    int arg_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&arg_type, symtab, array_expr, max_scope_lev, NO_MUTATE);
    arg_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&arg_type, symtab, lower_expr, max_scope_lev, NO_MUTATE);

    expr->resolved_type = LONGINT_TYPE;
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
        expr->expr_data.function_call_data.mangled_id = strdup("gpc_assigned");
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
            mangled_name = "gpc_abs_int";
            result_type = INT_TYPE;
        }
        else if (arg_type == LONGINT_TYPE)
        {
            mangled_name = "gpc_abs_longint";
            result_type = LONGINT_TYPE;
        }
        else if (arg_type == REAL_TYPE)
        {
            mangled_name = "gpc_abs_real";
            result_type = REAL_TYPE;
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
    expr->expr_data.function_call_data.mangled_id = strdup("gpc_arctan2");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for ArcTan2.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_gpc_type != NULL)
    {
        destroy_gpc_type(expr->resolved_gpc_type);
        expr->resolved_gpc_type = NULL;
    }
    expr->resolved_type = REAL_TYPE;
    expr->resolved_gpc_type = create_primitive_type(REAL_TYPE);
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
    expr->expr_data.function_call_data.mangled_id = strdup("gpc_hypot");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for Hypot.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_gpc_type != NULL)
    {
        destroy_gpc_type(expr->resolved_gpc_type);
        expr->resolved_gpc_type = NULL;
    }
    expr->resolved_type = REAL_TYPE;
    expr->resolved_gpc_type = create_primitive_type(REAL_TYPE);
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
    expr->expr_data.function_call_data.mangled_id = strdup("gpc_logn");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for LogN.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_gpc_type != NULL)
    {
        destroy_gpc_type(expr->resolved_gpc_type);
        expr->resolved_gpc_type = NULL;
    }
    expr->resolved_type = REAL_TYPE;
    expr->resolved_gpc_type = create_primitive_type(REAL_TYPE);
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
        fprintf(stderr, "Error on line %d, UpCase expects a char argument.\n",
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
        expr->expr_data.function_call_data.mangled_id = strdup("gpc_upcase_char");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for UpCase.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        expr->resolved_type = CHAR_TYPE;
        *type_return = CHAR_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
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
        expr->expr_data.function_call_data.mangled_id = strdup("gpc_is_odd");
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
            mangled_name = "gpc_sqr_real";
            result_type = REAL_TYPE;
        }
        else if (arg_type == LONGINT_TYPE)
        {
            mangled_name = "gpc_sqr_int64";
            result_type = LONGINT_TYPE;
        }
        else if (arg_type == INT_TYPE)
        {
            mangled_name = "gpc_sqr_int32";
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
            return 8;
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
        HashNode_t *target_node = NULL;
        if (FindIdent(&target_node, symtab, (char *)type_id) == -1 || target_node == NULL)
        {
            fprintf(stderr, "Error on line %d, SizeOf references unknown type %s.\n",
                line_num, type_id);
            return 1;
        }
        return sizeof_from_hashnode(symtab, target_node, size_out, depth + 1, line_num);
    }

    fprintf(stderr, "Error on line %d, unable to resolve type information for SizeOf.\n",
        line_num);
    return 1;
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
    if (sizeof_from_record_members(symtab, record->fields, &computed_size, depth + 1, line_num) != 0)
        return 1;

    record->cached_size = computed_size;
    record->has_cached_size = 1;
    *size_out = computed_size;
    return 0;
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

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        fprintf(stderr, "Error on line %d, SizeOf exceeded recursion depth while resolving record field.\n",
            line_num);
        return 1;
    }

    if (field->is_array)
    {
        if (field->array_is_open || field->array_end < field->array_start)
        {
            *size_out = POINTER_SIZE_BYTES;
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

    long long total = 0;
    ListNode_t *cur = members;
    while (cur != NULL)
    {
        if (cur->type == LIST_RECORD_FIELD)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            long long field_size = 0;
            if (compute_field_size(symtab, field, &field_size, depth + 1, line_num) != 0)
                return 1;
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
            if (field != NULL && !record_field_is_hidden(field) &&
                field->name != NULL &&
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

    /* PREFERRED PATH: Try using GpcType directly if available */
    if (node->type != NULL)
    {
        long long size = gpc_type_sizeof(node->type);
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
        /* else size < 0: gpc_type_sizeof couldn't determine size, fall through to legacy path */
    }

    /* LEGACY PATH: GpcType not available or couldn't determine size */
    
    if (node->hash_type == HASHTYPE_TYPE)
    {
        struct RecordType *record = get_record_type_from_node(node);
        if (record != NULL)
            return sizeof_from_record(symtab, record, size_out, depth + 1, line_num);
            
        struct TypeAlias *alias = get_type_alias_from_node(node);
        if (alias != NULL)
            return sizeof_from_alias(symtab, alias, size_out, depth + 1, line_num);

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
            fprintf(stderr, "Error on line %d, SizeOf references undeclared identifier %s.\n",
                expr->line_num, arg_id);
            error_count++;
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
                    error_count += sizeof_from_hashnode(symtab, node, &computed_size,
                        0, expr->line_num);
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
    return (*type == INT_TYPE || *type == REAL_TYPE || *type == LONGINT_TYPE);
}

static int types_numeric_compatible(int lhs, int rhs)
{
    if (lhs == rhs)
        return 1;
    if ((lhs == INT_TYPE && rhs == LONGINT_TYPE) || (lhs == LONGINT_TYPE && rhs == INT_TYPE))
        return 1;
    if ((lhs == REAL_TYPE && (rhs == INT_TYPE || rhs == LONGINT_TYPE)) ||
        (rhs == REAL_TYPE && (lhs == INT_TYPE || lhs == LONGINT_TYPE)))
        return 1;
    return 0;
}

static void semcheck_coerce_char_string_operands(int *type_first, struct Expression *expr1,
    int *type_second, struct Expression *expr2)
{
    if (type_first == NULL || type_second == NULL)
        return;

    if ((*type_first == CHAR_TYPE && *type_second == STRING_TYPE) ||
        (*type_first == STRING_TYPE && *type_second == CHAR_TYPE))
    {
        struct Expression *string_expr = (*type_first == STRING_TYPE) ? expr1 : expr2;
        int *string_type_ptr = (*type_first == STRING_TYPE) ? type_first : type_second;

        if (string_expr != NULL && string_expr->type == EXPR_STRING &&
            string_expr->expr_data.string != NULL &&
            strlen(string_expr->expr_data.string) == 1)
        {
            *string_type_ptr = CHAR_TYPE;
            string_expr->resolved_type = CHAR_TYPE;
        }
    }
}

static int resolve_type_identifier(int *out_type, SymTab_t *symtab,
    const char *type_id, int line_num)
{
    if (type_id == NULL)
        return 0;

    HashNode_t *type_node = NULL;
    if (FindIdent(&type_node, symtab, (char *)type_id) == -1 || type_node == NULL)
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
    error_count += resolve_type_identifier(&target_type, symtab,
        expr->expr_data.typecast_data.target_type_id, expr->line_num);

    if (target_type == UNKNOWN_TYPE &&
        expr->expr_data.typecast_data.target_type_id == NULL)
    {
        fprintf(stderr, "Error on line %d, typecast requires a target type.\n\n",
            expr->line_num);
        ++error_count;
    }

    *type_return = target_type;

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
    if (value_type != RECORD_TYPE || value_record == NULL || !record_type_is_class(value_record))
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
    }
    if (target_record == NULL || !record_type_is_class(target_record))
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
    if (value_type != RECORD_TYPE || value_record == NULL || !record_type_is_class(value_record))
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
    if (target_record == NULL || !record_type_is_class(target_record))
    {
        fprintf(stderr, "Error on line %d, \"as\" operator requires a class type on the right-hand side.\n\n",
            expr->line_num);
        ++error_count;
    }
    target_type = RECORD_TYPE;

    expr->expr_data.as_data.target_type = target_type;
    expr->expr_data.as_data.target_record_type = target_record;
    expr->record_type = target_record;
    expr->resolved_type = RECORD_TYPE;
    if (expr->resolved_gpc_type != NULL)
    {
        destroy_gpc_type(expr->resolved_gpc_type);
        expr->resolved_gpc_type = NULL;
    }
    expr->resolved_gpc_type = create_record_type(target_record);
    *type_return = RECORD_TYPE;
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
    expr->expr_data.function_call_data.id = id_copy;
    expr->expr_data.function_call_data.mangled_id = mangled_copy;
    expr->expr_data.function_call_data.args_expr = arg_node;
    expr->expr_data.function_call_data.resolved_func = NULL;
    expr->expr_data.function_call_data.call_hash_type = method_node->hash_type;
    semcheck_expr_set_call_gpc_type(expr, method_node->type,
        expr->expr_data.function_call_data.is_call_info_valid == 1);
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

    int error_count = 0;
    int record_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&record_type, symtab, record_expr, max_scope_lev, mutating);

    struct RecordType *record_info = NULL;
    if (record_type == RECORD_TYPE)
    {
        record_info = record_expr->record_type;
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
        fprintf(stderr, "Error on line %d, field access requires a record value.\n\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
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
    if (resolve_record_field(symtab, record_info, field_id, &field_desc,
            &field_offset, expr->line_num, 0) != 0 || field_desc == NULL)
    {
#ifdef DEBUG_PROPERTY_RESOLVE
        fprintf(stderr, "DEBUG property lookup: class=%d props=%p name=%s\n",
            record_type_is_class(record_info), (void *)record_info->properties, field_id);
#endif
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

                    struct RecordField *read_field =
                        semcheck_find_class_field_including_hidden(symtab,
                            record_info, property->read_accessor, NULL);
                    if (read_field != NULL &&
                        resolve_record_field(symtab, record_info, property->read_accessor,
                            &field_desc, &field_offset, expr->line_num, 0) == 0 &&
                        field_desc != NULL)
                    {
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
        }

        if (property_matched)
            return error_count;

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

        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, field_desc->type_id) == -1 || type_node == NULL)
        {
            type_node = semcheck_find_type_node_with_gpc_type(symtab, field_desc->type_id);
        }

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
                    HashNode_t *target_node = NULL;
                    if (FindIdent(&target_node, symtab, alias->target_type_id) != -1 &&
                        target_node != NULL)
                    {
                        field_record = get_record_type_from_node(target_node);
                    }
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
    error_count += semcheck_expr_main(&inner_type, symtab, inner, max_scope_lev, NO_MUTATE);

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
    
    /* Create a proper GpcType for the address-of expression */
    GpcType *pointed_to_type = NULL;
    
    /* Convert inner_type to GpcType */
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
    } else if (inner_type == PROCEDURE) {
        int proc_type_owned = 0;
        GpcType *proc_type = semcheck_resolve_expression_gpc_type(symtab, inner,
            max_scope_lev, NO_MUTATE, &proc_type_owned);
        if (proc_type != NULL)
        {
            if (!proc_type_owned)
                gpc_type_retain(proc_type);
            pointed_to_type = proc_type;
        }

        if (inner->type == EXPR_VAR_ID)
        {
            HashNode_t *proc_symbol = NULL;
            if (FindIdent(&proc_symbol, symtab, inner->expr_data.id) >= 0 &&
                proc_symbol != NULL && proc_symbol->hash_type == HASHTYPE_PROCEDURE)
            {
                expr->expr_data.addr_data.expr = NULL;
                destroy_expr(inner);
                expr->type = EXPR_ADDR_OF_PROC;
                expr->expr_data.addr_of_proc_data.procedure_symbol = proc_symbol;
            }
        }
    }
    /* For other types, we could add more conversions here */
    
    /* Create the pointer type */
    if (pointed_to_type != NULL) {
        if (expr->resolved_gpc_type != NULL) {
            destroy_gpc_type(expr->resolved_gpc_type);
        }
        expr->resolved_gpc_type = create_pointer_type(pointed_to_type);
    }
    
    return error_count;
}
/* Sets a type based on a hash_type - uses GpcType when available */
int set_type_from_hashtype(int *type, HashNode_t *hash_node)
{
    assert(type != NULL);
    assert(hash_node != NULL);

    /* Try GpcType first if available */
    if (hash_node->type != NULL)
    {
        if (hash_node->type->kind == TYPE_KIND_PRIMITIVE)
        {
            *type = gpc_type_get_primitive_tag(hash_node->type);
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
            /* For functions, return the return type, not PROCEDURE */
            GpcType *return_type = gpc_type_get_return_type(hash_node->type);
            if (return_type != NULL)
            {
                if (return_type->kind == TYPE_KIND_PRIMITIVE)
                {
                    *type = gpc_type_get_primitive_tag(return_type);
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
                /* Add other return type kinds as needed */
                *type = UNKNOWN_TYPE;
                return 0;
            }
            /* No return type means it's a procedure (void) */
            *type = PROCEDURE;
            return 0;
        }
        else if (hash_node->type->kind == TYPE_KIND_ARRAY)
        {
            /* For arrays, return the element type's primitive tag if available */
            GpcType *elem_type = hash_node->type->info.array_info.element_type;
            if (elem_type != NULL && elem_type->kind == TYPE_KIND_PRIMITIVE)
            {
                *type = gpc_type_get_primitive_tag(elem_type);
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
    return semcheck_expr_main(type_return, symtab, expr, max_scope_lev, mutating);
}

/* Semantic check on a function expression (no side effects allowed) */
int semcheck_expr_func(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int mutating)
{
    assert(type_return != NULL);
    return semcheck_expr_main(type_return, symtab, expr, 0, mutating);
}

/* Phase 3 Step 3: Resolve GpcType from an expression
 * This function performs semantic checking and returns the GpcType of the expression.
 * This is the bridge between the legacy int-based type system and the new GpcType system.
 * 
 * Implementation strategy:
 * 1. For simple cases (var IDs, function calls), we can directly get GpcType from symbol table
 * 2. For complex expressions, we use semcheck_expr_main to get the type tag, then convert to GpcType
 * 3. We also check expr->resolved_gpc_type if it was previously computed
 * 
 * Returns NULL if type cannot be resolved.
 * If owns_type is not NULL, it will be set to:
 *   - 0 if the caller does NOT own the type (shared reference from symbol table)
 *   - 1 if the caller OWNS the type (must free it with destroy_gpc_type)
 */
GpcType* semcheck_resolve_expression_gpc_type(SymTab_t *symtab, struct Expression *expr,
    int max_scope_lev, int mutating, int *owns_type)
{
    if (symtab == NULL || expr == NULL)
        return NULL;
    
    /* Default: caller owns the type we create */
    if (owns_type != NULL)
        *owns_type = 1;
    
    /* Try to get GpcType directly from the expression for specific cases */
    switch (expr->type)
    {
        case EXPR_VAR_ID:
        {
            /* For variable IDs, we can get the GpcType directly from the symbol table */
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, expr->expr_data.id) != -1 && node != NULL)
            {
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
            /* First, try the cached resolved_gpc_type if available */
            if (expr->resolved_gpc_type != NULL)
            {
                if (owns_type != NULL)
                    *owns_type = 0;  /* Shared reference */
                return expr->resolved_gpc_type;
            }
            
            /* Prefer cached call info populated during semantic checking */
            if (expr->expr_data.function_call_data.is_call_info_valid &&
                expr->expr_data.function_call_data.call_gpc_type != NULL)
            {
                GpcType *call_type = expr->expr_data.function_call_data.call_gpc_type;
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
                GpcType *record_type = semcheck_resolve_expression_gpc_type(symtab, record_expr, 
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
                                    GpcType *field_type = NULL;
                                    
                                    if (field->type_id != NULL)
                                    {
                                        /* User-defined type - look up in symbol table */
                                        HashNode_t *type_node = NULL;
                                        if (FindIdent(&type_node, symtab, field->type_id) != -1 && type_node != NULL)
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
                                        GpcType *element_type = NULL;
                                        if (field->array_element_type_id != NULL)
                                        {
                                            HashNode_t *elem_type_node = NULL;
                                            if (FindIdent(&elem_type_node, symtab, field->array_element_type_id) != -1 && elem_type_node != NULL)
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
                                        destroy_gpc_type(record_type);
                                    
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
                    destroy_gpc_type(record_type);
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
                if (FindIdent(&type_node, symtab, (char *)alias_id) != -1 &&
                    type_node != NULL && type_node->type != NULL)
                {
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return type_node->type;
                }
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

                GpcType *element_type = NULL;
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

            GpcType *element_type = create_primitive_type(expr->array_element_type);
            if (element_type == NULL)
                break;

            int end_index = expr->array_upper_bound;
            if (end_index < expr->array_lower_bound)
                end_index = expr->array_lower_bound - 1;

            GpcType *array_type = create_array_type(element_type,
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
                GpcType *ptr_type = semcheck_resolve_expression_gpc_type(symtab, pointer_expr, 
                                                                          max_scope_lev, mutating, &ptr_type_owned);
                if (ptr_type != NULL && ptr_type->kind == TYPE_KIND_POINTER)
                {
                    GpcType *deref_type = ptr_type->info.points_to;
                    
                    /* Clean up the pointer type if we owned it */
                    if (ptr_type_owned)
                        destroy_gpc_type(ptr_type);
                    
                    /* Return what the pointer points to - caller doesn't own it (it's part of the pointer type) */
                    if (owns_type != NULL)
                        *owns_type = 0;
                    return deref_type;
                }
                
                /* Clean up if we failed */
                if (ptr_type_owned && ptr_type != NULL)
                    destroy_gpc_type(ptr_type);
            }
            break;
        }
        
        default:
            break;
    }
    
    /* Check if the expression already has a resolved GpcType (e.g., from semcheck_expr_main) */
    if (expr->resolved_gpc_type != NULL)
    {
        /* Use the existing GpcType - caller doesn't own it (it belongs to the expression) */
        if (owns_type != NULL)
            *owns_type = 0;
        return expr->resolved_gpc_type;
    }
    
    /* For all other cases or if direct resolution failed, use semcheck_expr_main
     * to get the type tag, then convert to GpcType */
    int type_tag = UNKNOWN_TYPE;
    int result = semcheck_expr_main(&type_tag, symtab, expr, max_scope_lev, mutating);
    
    if (result != 0 || type_tag == UNKNOWN_TYPE)
        return NULL;
    
    /* Create a GpcType from the type tag - caller owns this */
    if (owns_type != NULL)
        *owns_type = 1;
    
    return create_primitive_type(type_tag);
}

/* Main semantic checking */
int semcheck_expr_main(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
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
                *type_return = LONGINT_TYPE;
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
            /* Create a proper GpcType for nil with points_to = NULL */
            if (expr->resolved_gpc_type != NULL) {
                destroy_gpc_type(expr->resolved_gpc_type);
            }
            expr->resolved_gpc_type = create_pointer_type(NULL);
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

        case EXPR_ANONYMOUS_FUNCTION:
        case EXPR_ANONYMOUS_PROCEDURE:
        {
            /* Anonymous methods are treated as procedure/function references.
             * Create a GpcType for better type checking.
             */
            
            /* Set the type to PROCEDURE to represent a function/procedure reference */
            *type_return = PROCEDURE;
            
            /* Create a GpcType with parameter and return type information */
            ListNode_t *params = expr->expr_data.anonymous_method_data.parameters;
            GpcType *return_type = NULL;
            
            /* For functions, resolve the return type */
            if (expr->type == EXPR_ANONYMOUS_FUNCTION) {
                char *return_type_id = expr->expr_data.anonymous_method_data.return_type_id;
                if (return_type_id != NULL) {
                    /* Try to look up the type in the symbol table */
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, return_type_id) >= 0 && type_node != NULL) {
                        /* Use the GpcType from the symbol table if available */
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
                            /* Note: This creates a new GpcType that will be owned by proc_type */
                        }
                    }
                }
            }
            /* For procedures, return_type remains NULL */
            
            /* Create the procedure type to store in the expression */
            GpcType *proc_type = create_procedure_type(params, return_type);
            if (proc_type != NULL) {
                expr->resolved_gpc_type = proc_type;
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
                                    GpcType *param_gpc_type = NULL;
                                    
                                    /* Try to resolve parameter type */
                                    if (param_tree->tree_data.var_decl_data.type_id != NULL)
                                    {
                                        HashNode_t *type_node = NULL;
                                        if (FindIdent(&type_node, symtab, param_tree->tree_data.var_decl_data.type_id) >= 0 &&
                                            type_node != NULL && type_node->type != NULL)
                                        {
                                            param_gpc_type = type_node->type;
                                        }
                                    }
                                    
                                    if (param_gpc_type == NULL && param_type != UNKNOWN_TYPE)
                                    {
                                        param_gpc_type = create_primitive_type(param_type);
                                    }
                                    
                                    PushVarOntoScope_Typed(symtab, param_id, param_gpc_type);
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
                    
                    /* Also add "Result" as an alias */
                    HashNode_t *result_check = NULL;
                    if (FindIdent(&result_check, symtab, "Result") == -1)
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
        if (type_first != BOOL)
        {
            fprintf(stderr, "Error on line %d, expected relational type after \"NOT\"!\n\n",
                expr->line_num);
            ++return_val;
        }
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
                if (type_first != INT_TYPE && type_first != LONGINT_TYPE &&
                    type_first != ENUM_TYPE && type_first != CHAR_TYPE && type_first != BOOL)
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
                                if (operator_node->type != NULL && gpc_type_is_procedure(operator_node->type))
                                {
                                    GpcType *return_type = gpc_type_get_return_type(operator_node->type);
                                    if (return_type != NULL)
                                    {
                                        /* Transform expression from RELOP to FUNCTION_CALL */
                                        struct Expression *saved_left = expr->expr_data.relop_data.left;
                                        struct Expression *saved_right = expr->expr_data.relop_data.right;
                                        
                                        expr->type = EXPR_FUNCTION_CALL;
                                        
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
                                        expr->expr_data.function_call_data.call_gpc_type = operator_node->type;
                                        gpc_type_retain(operator_node->type);
                                        expr->expr_data.function_call_data.is_call_info_valid = 1;
                                        
                                        if (expr->resolved_gpc_type != NULL)
                                        {
                                            destroy_gpc_type(expr->resolved_gpc_type);
                                        }
                                        expr->resolved_gpc_type = return_type;
                                        gpc_type_retain(return_type);
                                        
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
                int string_ok = (type_first == STRING_TYPE && type_second == STRING_TYPE);
                int char_ok = (type_first == CHAR_TYPE && type_second == CHAR_TYPE);
                if (!numeric_ok && !boolean_ok && !string_ok && !char_ok)
                {
                    fprintf(stderr, "Error on line %d, equality comparison requires matching numeric, boolean, string, or character types!\n\n",
                        expr->line_num);
                    ++return_val;
                }
            }
            else
            {
                semcheck_coerce_char_string_operands(&type_first, expr1, &type_second, expr2);

                int numeric_ok = types_numeric_compatible(type_first, type_second) &&
                                 is_type_ir(&type_first) && is_type_ir(&type_second);
                int string_ok = (type_first == STRING_TYPE && type_second == STRING_TYPE);
                int char_ok = (type_first == CHAR_TYPE && type_second == CHAR_TYPE);

                if(!numeric_ok && !string_ok && !char_ok)
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
        if (type_first != BOOL || type_second != BOOL)
        {
            fprintf(stderr, "Error on line %d, expected boolean operands for OR expression!\n\n",
                expr->line_num);
            ++return_val;
        }
        *type_return = BOOL;
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
        int left_is_string_like = (type_first == STRING_TYPE || type_first == CHAR_TYPE);
        int right_is_string_like = (type_second == STRING_TYPE || type_second == CHAR_TYPE);

        if (left_is_string_like && right_is_string_like)
        {
            *type_return = STRING_TYPE;
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
                        if (operator_node->type != NULL && gpc_type_is_procedure(operator_node->type))
                        {
                            GpcType *return_type = gpc_type_get_return_type(operator_node->type);
                            if (return_type != NULL)
                            {
                                *type_return = gpc_type_get_legacy_tag(return_type);
                                
                                /* Transform expression from ADDOP to FUNCTION_CALL */
                                /* Save the operands before we overwrite the union */
                                struct Expression *saved_left = expr->expr_data.addop_data.left_expr;
                                struct Expression *saved_right = expr->expr_data.addop_data.right_term;
                                
                                /* Change expression type to FUNCTION_CALL */
                                expr->type = EXPR_FUNCTION_CALL;
                                
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
                                expr->expr_data.function_call_data.call_gpc_type = operator_node->type;
                                gpc_type_retain(operator_node->type); /* Increment ref count */
                                expr->expr_data.function_call_data.is_call_info_valid = 1;
                                
                                /* Set resolved_gpc_type to the return type */
                                if (expr->resolved_gpc_type != NULL)
                                {
                                    destroy_gpc_type(expr->resolved_gpc_type);
                                }
                                expr->resolved_gpc_type = return_type;
                                gpc_type_retain(return_type); /* Increment ref count */
                                
                                /* For record return types, preserve record type info */
                                if (gpc_type_is_record(return_type))
                                {
                                    struct RecordType *ret_record = gpc_type_get_record(return_type);
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
    if (op_type == AND)
    {
        if (type_first != BOOL || type_second != BOOL)
        {
            fprintf(stderr, "Error on line %d, expected boolean operands for AND expression!\n\n",
                expr->line_num);
            ++return_val;
        }
        *type_return = BOOL;
        return return_val;
    }

    if (type_first == SET_TYPE && type_second == SET_TYPE)
    {
        if (op_type == STAR || op_type == XOR)
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
                        if (operator_node->type != NULL && gpc_type_is_procedure(operator_node->type))
                        {
                            GpcType *return_type = gpc_type_get_return_type(operator_node->type);
                            if (return_type != NULL)
                            {
                                *type_return = gpc_type_get_legacy_tag(return_type);
                                
                                /* Transform expression from MULOP to FUNCTION_CALL */
                                /* Save the operands before we overwrite the union */
                                struct Expression *saved_left = expr->expr_data.mulop_data.left_term;
                                struct Expression *saved_right = expr->expr_data.mulop_data.right_factor;
                                
                                /* Change expression type to FUNCTION_CALL */
                                expr->type = EXPR_FUNCTION_CALL;
                                
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
                                expr->expr_data.function_call_data.call_gpc_type = operator_node->type;
                                gpc_type_retain(operator_node->type);
                                expr->expr_data.function_call_data.is_call_info_valid = 1;
                                
                                /* Set resolved_gpc_type to the return type */
                                if (expr->resolved_gpc_type != NULL)
                                {
                                    destroy_gpc_type(expr->resolved_gpc_type);
                                }
                                expr->resolved_gpc_type = return_type;
                                gpc_type_retain(return_type);
                                
                                /* For record return types, preserve record type info */
                                if (gpc_type_is_record(return_type))
                                {
                                    struct RecordType *ret_record = gpc_type_get_record(return_type);
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

    if (type_first == REAL_TYPE || type_second == REAL_TYPE)
        *type_return = REAL_TYPE;
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
            fprintf(stderr, "Error on line %d, cannot change \"%s\", invalid scope!\n",
                expr->line_num, id);
            fprintf(stderr, "[Was it defined above a function declaration?]\n\n");
            ++return_val;
        }
        int node_is_array = hashnode_is_array(hash_return);

        if(hash_return->hash_type != HASHTYPE_VAR &&
            hash_return->hash_type != HASHTYPE_FUNCTION_RETURN &&
            !node_is_array)
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
        semcheck_expr_set_resolved_gpc_type_shared(expr, hash_return->type);

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
                gpc_type_is_pointer(hash_return->type))
            {
                subtype = gpc_type_get_pointer_subtype_tag(hash_return->type);
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
            if (expr->pointer_subtype_id != NULL)
            {
                HashNode_t *target_node = NULL;
                if (FindIdent(&target_node, symtab, expr->pointer_subtype_id) != -1 && target_node != NULL)
                    expr->record_type = get_record_type_from_node(target_node);
            }
            else if (hash_return->type != NULL && gpc_type_is_pointer(hash_return->type))
            {
                GpcType *points_to = hash_return->type->info.points_to;
                if (points_to != NULL && gpc_type_is_record(points_to))
                    expr->record_type = gpc_type_get_record(points_to);
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

    int base_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_main(&base_type, symtab, array_expr, max_scope_lev, mutating);

    int base_is_string = (base_type == STRING_TYPE && !array_expr->is_array_expr);
    if (!array_expr->is_array_expr && !base_is_string)
    {
        fprintf(stderr, "Error on line %d, expression is not indexable as an array.\n\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return return_val + 1;
    }

    if (base_is_string)
    {
        element_type = CHAR_TYPE;
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
            const char *pointer_type_id = NULL;
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
                        pointer_type_id = alias->pointer_type_id;
                        if (alias->pointer_type == RECORD_TYPE && alias->pointer_type_id != NULL)
                            pointer_record = semcheck_lookup_record_type(symtab, alias->pointer_type_id);
                    }
                }
            }

            semcheck_set_pointer_info(expr, pointer_subtype, pointer_type_id);
            if (pointer_subtype == RECORD_TYPE)
                expr->record_type = pointer_record;
            else
                expr->record_type = NULL;
        }
        else if (element_type == RECORD_TYPE)
        {
            expr->record_type = array_expr->array_element_record_type;
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
static int resolve_param_type(Tree_t *decl, SymTab_t *symtab)
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

    if (type_tag != UNKNOWN_TYPE)
        return type_tag;

    if (type_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, type_id) >= 0 && type_node != NULL)
        {
            int resolved_type = UNKNOWN_TYPE;
            set_type_from_hashtype(&resolved_type, type_node);
            return resolved_type;
        }
    }

    return UNKNOWN_TYPE;
}

/** FUNC_CALL **/
int semcheck_funccall(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val, scope_return;
    char *id;
    char *mangled_name;
    int arg_type, cur_arg;
    ListNode_t *true_args, *true_arg_ids, *args_given;
    HashNode_t *hash_return;
    Tree_t *arg_decl;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    return_val = 0;
    id = expr->expr_data.function_call_data.id;
    args_given = expr->expr_data.function_call_data.args_expr;

    if (id != NULL && strncmp(id, "__tfpg_ctor$", strlen("__tfpg_ctor$")) == 0)
    {
        if (type_return != NULL)
            *type_return = RECORD_TYPE;
        expr->resolved_type = RECORD_TYPE;
        return 0;
    }

    if (id != NULL && pascal_identifier_equals(id, "SizeOf"))
        return semcheck_builtin_sizeof(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Chr"))
        return semcheck_builtin_chr(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Ord"))
        return semcheck_builtin_ord(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Length"))
        return semcheck_builtin_length(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Copy"))
        return semcheck_builtin_copy(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Pos"))
        return semcheck_builtin_pos(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "EOF"))
        return semcheck_builtin_eof(type_return, symtab, expr, max_scope_lev);
    if (id != NULL && pascal_identifier_equals(id, "EOLN"))
        return semcheck_builtin_eoln(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Low"))
        return semcheck_builtin_lowhigh(type_return, symtab, expr, max_scope_lev, 0);

    if (id != NULL && pascal_identifier_equals(id, "High"))
        return semcheck_builtin_lowhigh(type_return, symtab, expr, max_scope_lev, 1);

    if (id != NULL && pascal_identifier_equals(id, "Assigned"))
        return semcheck_builtin_assigned(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Abs"))
        return semcheck_builtin_abs(type_return, symtab, expr, max_scope_lev);


    if (id != NULL && pascal_identifier_equals(id, "Sqrt"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sqrt", "gpc_sqrt", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Sin"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sin", "gpc_sin", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Csc"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Csc", "gpc_csc", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Sinh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sinh", "gpc_sinh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Csch"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Csch", "gpc_csch", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Cos"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Cos", "gpc_cos", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Sec"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sec", "gpc_sec", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Cosh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Cosh", "gpc_cosh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Sech"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Sech", "gpc_sech", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Tan"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Tan", "gpc_tan", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Cot"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Cot", "gpc_cot", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Tanh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Tanh", "gpc_tanh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Coth"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Coth", "gpc_coth", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcTan"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcTan", "gpc_arctan", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCot"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCot", "gpc_arccot", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcTan2"))
        return semcheck_builtin_arctan2(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Hypot"))
        return semcheck_builtin_hypot(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "ArcSin"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcSin", "gpc_arcsin", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCos"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCos", "gpc_arccos", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCosh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCosh", "gpc_arccosh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcSech"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcSech", "gpc_arcsech", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCsch"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCsch", "gpc_arccsch", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcCoth"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcCoth", "gpc_arccoth", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcSinh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcSinh", "gpc_arcsinh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "ArcTanh"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "ArcTanh", "gpc_arctanh", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "DegToRad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "DegToRad", "gpc_deg_to_rad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "RadToDeg"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "RadToDeg", "gpc_rad_to_deg", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "DegToGrad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "DegToGrad", "gpc_deg_to_grad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "GradToDeg"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "GradToDeg", "gpc_grad_to_deg", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "GradToRad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "GradToRad", "gpc_grad_to_rad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "RadToGrad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "RadToGrad", "gpc_rad_to_grad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "CycleToRad"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "CycleToRad", "gpc_cycle_to_rad", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "RadToCycle"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "RadToCycle", "gpc_rad_to_cycle", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Ln"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Ln", "gpc_ln", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "LogN"))
        return semcheck_builtin_logn(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Exp"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Exp", "gpc_exp", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Round"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Round", "gpc_round", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Trunc"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Trunc", "gpc_trunc", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Int"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Int", "gpc_int", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Frac"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Frac", "gpc_frac", REAL_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Ceil"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Ceil", "gpc_ceil", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "Floor"))
        return semcheck_builtin_unary_real(type_return, symtab, expr, max_scope_lev,
            "Floor", "gpc_floor", LONGINT_TYPE);

    if (id != NULL && pascal_identifier_equals(id, "UpCase"))
        return semcheck_builtin_upcase(type_return, symtab, expr, max_scope_lev);

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

    ListNode_t *overload_candidates = FindAllIdents(symtab, id);
    mangled_name = MangleFunctionNameFromCallSite(id, args_given, symtab, max_scope_lev);
    if (mangled_name == NULL)
    {
        fprintf(stderr, "Error: failed to mangle function name for call to %s\n",
            id != NULL ? id : "(unknown)");
        *type_return = UNKNOWN_TYPE;
        destroy_list(overload_candidates);
        return ++return_val;
    }

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

            /* Get formal arguments from GpcType instead of deprecated args field */
            ListNode_t *candidate_args = gpc_type_get_procedure_params(candidate->type);
            
            if (ListLength(candidate_args) == ListLength(args_given))
            {
                int current_score = 0;
                ListNode_t *formal_args = candidate_args;
                ListNode_t *call_args = args_given;

                while(formal_args != NULL)
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
                    semcheck_expr_main(&call_type, symtab, call_expr, max_scope_lev, NO_MUTATE);
                    if (formal_decl != NULL && formal_decl->type == TREE_ARR_DECL &&
                        call_expr != NULL && call_expr->type == EXPR_ARRAY_LITERAL)
                    {
                        call_type = formal_type;
                    }

                    if(formal_type == call_type)
                        current_score += 0;
                    else if (formal_type == LONGINT_TYPE && call_type == INT_TYPE)
                        current_score += 1;
                    else if (formal_type == STRING_TYPE && call_type == CHAR_TYPE)
                        current_score += 1; /* Allow implicit char-to-string promotion */
                    else
                        current_score += 1000; // Mismatch

                    formal_args = formal_args->next;
                    call_args = call_args->next;
                }

                if(current_score < best_score)
                {
                    best_score = current_score;
                    best_match = candidate;
                    num_best_matches = 1;
                }
                else if (current_score == best_score)
                {
                    num_best_matches++;
                }
            }
            cur = cur->next;
        }
    }

    if (num_best_matches == 1)
    {
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
                        target_name = proc_def->tree_data.subprogram_data.id;
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
        fprintf(stderr, "Error on line %d, call to function %s is ambiguous\n", expr->line_num, id);
        *type_return = UNKNOWN_TYPE;
        final_status = ++return_val;
        goto funccall_cleanup;
    }


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
            fprintf(stderr, "Error on line %d, cannot change \"%s\", invalid scope!\n\n",
                expr->line_num, id);
            fprintf(stderr, "[Was it defined above a function declaration?]\n\n");
            ++return_val;
        }
        if(hash_return->hash_type != HASHTYPE_FUNCTION &&
            hash_return->hash_type != HASHTYPE_FUNCTION_RETURN)
        {
            fprintf(stderr, "Error on line %d, \"%s\" is not a function!\n\n",
                expr->line_num, id);
            ++return_val;
        }

        set_type_from_hashtype(type_return, hash_return);
        
        /* NEW: Also set the resolved GpcType for this expression */
        if (hash_return->type != NULL && hash_return->type->kind == TYPE_KIND_PROCEDURE)
        {
            GpcType *return_type = gpc_type_get_return_type(hash_return->type);
            if (return_type != NULL)
            {
                semcheck_expr_set_resolved_gpc_type_shared(expr, return_type);
                if (return_type->kind == TYPE_KIND_ARRAY)
                    semcheck_set_array_info_from_gpctype(expr, symtab, return_type, expr->line_num);
                else
                    semcheck_clear_array_info(expr);
                if (return_type->kind == TYPE_KIND_PRIMITIVE &&
                    return_type->info.primitive_type_tag == UNKNOWN_TYPE)
                {
                    char *target_return_id = hash_return->type->info.proc_info.return_type_id;
                    if (target_return_id != NULL)
                    {
                        HashNode_t *type_node = semcheck_find_type_node_with_gpc_type(symtab, target_return_id);
                        if (type_node != NULL && type_node->type != NULL)
                        {
                            destroy_gpc_type(return_type);
                            gpc_type_retain(type_node->type);
                            hash_return->type->info.proc_info.return_type = type_node->type;
                            return_type = type_node->type;
                            semcheck_expr_set_resolved_gpc_type_shared(expr, type_node->type);
                            if (return_type->kind == TYPE_KIND_ARRAY)
                                semcheck_set_array_info_from_gpctype(expr, symtab, return_type, expr->line_num);
                            else
                                semcheck_clear_array_info(expr);
                        }
                    }
                }
            }
            else
            {
                semcheck_expr_set_resolved_gpc_type_shared(expr, NULL);
                semcheck_clear_array_info(expr);
            }
        }
        else
        {
            semcheck_expr_set_resolved_gpc_type_shared(expr, hash_return->type);
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

        /***** THEN VERIFY ARGS INSIDE *****/
        cur_arg = 0;
        /* Get formal arguments from GpcType instead of deprecated args field */
        true_args = gpc_type_get_procedure_params(hash_return->type);
        while(args_given != NULL && true_args != NULL)
        {
            ++cur_arg;
            assert(args_given->type == LIST_EXPR);
            assert(true_args->type == LIST_TREE);

            arg_decl = (Tree_t *)true_args->cur;
            if (arg_decl->type != TREE_VAR_DECL && arg_decl->type != TREE_ARR_DECL)
            {
                fprintf(stderr, "Error on line %d, unsupported parameter declaration in call to %s.\n",
                    expr->line_num, id);
                ++return_val;
                true_args = true_args->next;
                args_given = args_given->next;
                continue;
            }
            struct Expression *current_arg_expr = (struct Expression *)args_given->cur;
            if (arg_decl->type == TREE_ARR_DECL)
            {
                if (semcheck_prepare_array_literal_argument(arg_decl, current_arg_expr,
                        symtab, max_scope_lev, expr->line_num) != 0)
                {
                    ++return_val;
                    true_args = true_args->next;
                    args_given = args_given->next;
                    continue;
                }
            }

            return_val += semcheck_expr_main(&arg_type,
                symtab, current_arg_expr, max_scope_lev, NO_MUTATE);
            if (arg_decl->type == TREE_VAR_DECL)
                true_arg_ids = arg_decl->tree_data.var_decl_data.ids;
            else
                true_arg_ids = arg_decl->tree_data.arr_decl_data.ids;

            while(true_arg_ids != NULL && args_given != NULL)
            {
                int expected_type = resolve_param_type(arg_decl, symtab);
                if (arg_decl->type == TREE_ARR_DECL && current_arg_expr != NULL &&
                    current_arg_expr->type == EXPR_ARRAY_LITERAL)
                {
                    arg_type = expected_type;
                }
                if(arg_type != expected_type && expected_type != BUILTIN_ANY_TYPE)
                {
                    /* Allow integer/longint conversion */
                    int type_compatible = 0;
                    if ((arg_type == INT_TYPE && expected_type == LONGINT_TYPE) ||
                        (arg_type == LONGINT_TYPE && expected_type == INT_TYPE))
                    {
                        type_compatible = 1;
                    }
                    else if (expected_type == STRING_TYPE && arg_type == CHAR_TYPE)
                    {
                        type_compatible = 1;
                    }
                    else if (expected_type == REAL_TYPE &&
                        (arg_type == INT_TYPE || arg_type == LONGINT_TYPE))
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
                        fprintf(stderr, "Error on line %d, on function call %s, argument %d: Type mismatch!\n\n",
                            expr->line_num, id, cur_arg);
                        ++return_val;
                    }
                }

                true_arg_ids = true_arg_ids->next;
                args_given = args_given->next;
            }

            true_args = true_args->next;
        }
        if(true_args == NULL && args_given != NULL)
        {
            fprintf(stderr, "Error on line %d, on function call %s, too many arguments given!\n\n",
                expr->line_num, id);
            ++return_val;
        }
        else if(true_args != NULL && args_given == NULL)
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
        expr->expr_data.function_call_data.mangled_id = strdup("gpc_random_real");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Random.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        if (expr->resolved_gpc_type != NULL)
        {
            destroy_gpc_type(expr->resolved_gpc_type);
            expr->resolved_gpc_type = NULL;
        }
        expr->resolved_gpc_type = create_primitive_type(REAL_TYPE);
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
        expr->expr_data.function_call_data.mangled_id = strdup("gpc_random_real_upper");
    else
        expr->expr_data.function_call_data.mangled_id = strdup("gpc_random_int");
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
    if (expr->resolved_gpc_type != NULL)
    {
        destroy_gpc_type(expr->resolved_gpc_type);
        expr->resolved_gpc_type = NULL;
    }
    expr->resolved_gpc_type = create_primitive_type(expr->resolved_type);
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
    expr->expr_data.function_call_data.mangled_id = strdup("gpc_random_range");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for RandomRange.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_gpc_type != NULL)
    {
        destroy_gpc_type(expr->resolved_gpc_type);
        expr->resolved_gpc_type = NULL;
    }

    int result_type = (low_type == LONGINT_TYPE || high_type == LONGINT_TYPE)
        ? LONGINT_TYPE : INT_TYPE;
    expr->resolved_type = result_type;
    expr->resolved_gpc_type = create_primitive_type(result_type);
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
    expr->expr_data.function_call_data.mangled_id = strdup("gpc_power");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for Power.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_gpc_type != NULL)
    {
        destroy_gpc_type(expr->resolved_gpc_type);
        expr->resolved_gpc_type = NULL;
    }
    expr->resolved_type = REAL_TYPE;
    expr->resolved_gpc_type = create_primitive_type(REAL_TYPE);
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
    expr->expr_data.function_call_data.mangled_id = strdup("gpc_get_randseed");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for RandSeed.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->resolved_gpc_type != NULL)
    {
        destroy_gpc_type(expr->resolved_gpc_type);
        expr->resolved_gpc_type = NULL;
    }
    expr->resolved_type = LONGINT_TYPE;
    expr->resolved_gpc_type = create_primitive_type(LONGINT_TYPE);
    *type_return = LONGINT_TYPE;
    return 0;
}
