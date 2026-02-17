/*
    Damon Gwinn
    Performs semantic checking on a given statement

    NOTE: Max scope level refers to the highest level scope we can reference a variable at
        - 0 is the current scope, 1 is the first above and so on
        - Functions can't have side effects, but they can contain procedures so this is a
            general way to define the maximum scope level
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <time.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strncasecmp _strnicmp
#endif
#include "SemCheck_stmt.h"
#include "SemCheck_expr.h"
#include "SemCheck_overload.h"
#include "../SemCheck.h"
#include "../NameMangling.h"
#include "../HashTable/HashTable.h"
#include "../SymTab/SymTab.h"
#include "../../ParseTree/tree.h"
#include "../../ParseTree/tree_types.h"
#include "../../ParseTree/type_tags.h"
#include "../../List/List.h"

/* Forward declaration from SemCheck_Expr_Resolve.c */
const char *semcheck_type_tag_name(int type_tag);
int semcheck_typecheck_array_literal(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, int expected_type, const char *expected_type_id, int line_num);
int set_type_from_hashtype(int *type, HashNode_t *hash_node);
struct RecordType *semcheck_lookup_record_type(SymTab_t *symtab, const char *type_id);

#define SEMSTMT_TIMINGS_ENABLED() (getenv("KGPC_DEBUG_SEMSTMT_TIMINGS") != NULL)

static double semstmt_now_ms(void) {
    return (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
}

void semcheck_expr_set_resolved_type(struct Expression *expr, int type_tag);

static int semcheck_stmt_expr_tag(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int mutating)
{
    KgpcType *resolved = NULL;
    int result = semcheck_expr_main(symtab, expr, max_scope_lev, mutating, &resolved);
    if (type_return != NULL)
    {
        *type_return = semcheck_tag_from_kgpc(resolved);
    }
    return result;
}

static inline struct RecordType* semcheck_stmt_get_record_type_from_node(HashNode_t *node)
{
    if (node == NULL)
        return NULL;
    if (node->type != NULL && node->type->kind == TYPE_KIND_RECORD)
        return node->type->info.record_info;
    if (node->type != NULL && node->type->kind == TYPE_KIND_POINTER &&
        node->type->info.points_to != NULL && kgpc_type_is_record(node->type->info.points_to))
        return node->type->info.points_to->info.record_info;
    return NULL;
}
#include "../../ParseTree/type_tags.h"
#include "../../ParseTree/KgpcType.h"
#include "../../ParseTree/from_cparser.h"
#include "../../../identifier_utils.h"
#include <math.h>

/* Check if the given KgpcType represents a Currency type.
 * Currency is a special type that stores values scaled by 10000 internally.
 * Returns 1 if the type is Currency, 0 otherwise.
 */
static int semcheck_is_currency_kgpc_type(KgpcType *type)
{
    if (type == NULL)
        return 0;
    if (type->kind != TYPE_KIND_PRIMITIVE)
        return 0;
    if (type->info.primitive_type_tag != INT64_TYPE)
        return 0;
    /* Check if the type alias name is "Currency" */
    struct TypeAlias *alias = kgpc_type_get_type_alias(type);
    if (alias != NULL && alias->alias_name != NULL &&
        pascal_identifier_equals(alias->alias_name, "Currency"))
        return 1;
    return 0;
}

static int semcheck_expr_is_shortstring(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    if (expr->resolved_kgpc_type != NULL)
    {
        struct TypeAlias *alias = kgpc_type_get_type_alias(expr->resolved_kgpc_type);
        if (alias != NULL)
        {
            if (alias->is_shortstring)
                return 1;
            if ((alias->alias_name != NULL &&
                 pascal_identifier_equals(alias->alias_name, "ShortString")) ||
                (alias->target_type_id != NULL &&
                 pascal_identifier_equals(alias->target_type_id, "ShortString")))
            {
                return 1;
            }
        }
    }
    if (expr->is_array_expr &&
        expr->array_element_type == CHAR_TYPE &&
        expr->array_lower_bound == 0 &&
        expr->array_upper_bound >= 0)
    {
        return 1;
    }
    return 0;
}

static const char *semcheck_record_type_id_from_kgpc(KgpcType *type)
{
    if (type == NULL)
        return NULL;

    if (kgpc_type_is_record(type))
    {
        struct RecordType *record = kgpc_type_get_record(type);
        if (record != NULL && record->type_id != NULL)
            return record->type_id;
    }

    struct TypeAlias *alias = kgpc_type_get_type_alias(type);
    if (alias != NULL)
    {
        if (alias->target_type_id != NULL)
            return alias->target_type_id;
        if (alias->alias_name != NULL)
            return alias->alias_name;
    }

    return NULL;
}

static const char *semcheck_record_type_id_from_expr(SymTab_t *symtab,
    struct Expression *expr, KgpcType *fallback_type)
{
    const char *type_id = semcheck_record_type_id_from_kgpc(fallback_type);
    if (type_id != NULL || symtab == NULL || expr == NULL || expr->type != EXPR_VAR_ID ||
        expr->expr_data.id == NULL)
    {
        return type_id;
    }

    HashNode_t *node = NULL;
    if (FindIdent(&node, symtab, expr->expr_data.id) == 0 && node != NULL &&
        node->type != NULL)
    {
        return semcheck_record_type_id_from_kgpc(node->type);
    }

    return NULL;
}

static int semcheck_type_is_recordish(KgpcType *type)
{
    if (type == NULL)
        return 0;
    if (kgpc_type_is_record(type))
        return 1;
    return semcheck_tag_from_kgpc(type) == RECORD_TYPE;
}

static int semcheck_find_operator_symbol(HashNode_t **operator_node,
    SymTab_t *symtab, const char *operator_method)
{
    if (operator_node == NULL || symtab == NULL || operator_method == NULL)
        return 0;
    *operator_node = NULL;

    if (FindIdent(operator_node, symtab, operator_method) == 0 &&
        *operator_node != NULL && (*operator_node)->type != NULL &&
        kgpc_type_is_procedure((*operator_node)->type))
    {
        return 1;
    }

    if (FindIdentByPrefix(operator_node, symtab, operator_method) == 0 &&
        *operator_node != NULL && (*operator_node)->type != NULL &&
        kgpc_type_is_procedure((*operator_node)->type))
    {
        return 1;
    }

    /* Assignment operators are frequently keyed by id=':=' with mangled_id
     * carrying the owner/op name. Fall back to mangled prefix matching. */
    char *needle = pascal_identifier_lower_dup(operator_method);
    if (needle == NULL)
        return 0;
    const char *operator_ids[] = {":=", "op_assign"};
    for (size_t i = 0; i < sizeof(operator_ids) / sizeof(operator_ids[0]); ++i)
    {
        ListNode_t *candidates = FindAllIdents(symtab, operator_ids[i]);
        for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
        {
            HashNode_t *cand = (HashNode_t *)cur->cur;
            if (cand == NULL || cand->type == NULL || !kgpc_type_is_procedure(cand->type) ||
                cand->mangled_id == NULL)
            {
                continue;
            }
            char *mangled = pascal_identifier_lower_dup(cand->mangled_id);
            if (mangled == NULL)
                continue;
            int prefix_match = strstr(mangled, needle) != NULL;
            free(mangled);
            if (prefix_match)
            {
                *operator_node = cand;
                DestroyList(candidates);
                free(needle);
                return 1;
            }
        }
        DestroyList(candidates);
    }
    free(needle);

    return 0;
}

static HashNode_t *semcheck_find_record_assign_operator_candidate(SymTab_t *symtab,
    const char *target_type_id, const char *source_type_id)
{
    if (symtab == NULL || target_type_id == NULL || source_type_id == NULL)
        return NULL;

    char target_id[256];
    char source_id[256];
    char target_op_id[256];
    char source_op_id[256];
    snprintf(target_id, sizeof(target_id), "%s.:=", target_type_id);
    snprintf(source_id, sizeof(source_id), "%s.:=", source_type_id);
    snprintf(target_op_id, sizeof(target_op_id), "%s__op_assign", target_type_id);
    snprintf(source_op_id, sizeof(source_op_id), "%s__op_assign", source_type_id);
    const char *operator_ids[] = {":=", "op_assign", target_id, source_id, target_op_id, source_op_id};

    size_t target_prefix_len = strlen(target_type_id) + strlen("__op_assign") + 1;
    size_t source_prefix_len = strlen(source_type_id) + strlen("__op_assign") + 1;
    char *target_prefix = (char *)malloc(target_prefix_len);
    char *source_prefix = (char *)malloc(source_prefix_len);
    if (target_prefix == NULL || source_prefix == NULL)
    {
        free(target_prefix);
        free(source_prefix);
        return NULL;
    }
    snprintf(target_prefix, target_prefix_len, "%s__op_assign", target_type_id);
    snprintf(source_prefix, source_prefix_len, "%s__op_assign", source_type_id);
    for (char *p = target_prefix; *p != '\0'; ++p)
        *p = (char)tolower((unsigned char)*p);
    for (char *p = source_prefix; *p != '\0'; ++p)
        *p = (char)tolower((unsigned char)*p);

    HashNode_t *fallback_match = NULL;
    for (size_t i = 0; i < sizeof(operator_ids) / sizeof(operator_ids[0]); ++i)
    {
        ListNode_t *candidates = FindAllIdents(symtab, operator_ids[i]);
        for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
        {
            HashNode_t *cand = (HashNode_t *)cur->cur;
            if (cand == NULL || cand->type == NULL || !kgpc_type_is_procedure(cand->type) ||
                cand->mangled_id == NULL)
            {
                continue;
            }
            char *mangled = pascal_identifier_lower_dup(cand->mangled_id);
            if (mangled == NULL)
                continue;
            int target_match = strstr(mangled, target_prefix) != NULL;
            int source_match = strstr(mangled, source_prefix) != NULL;
            free(mangled);
            if (target_match)
            {
                DestroyList(candidates);
                free(target_prefix);
                free(source_prefix);
                return cand;
            }
            if (fallback_match == NULL && source_match)
            {
                fallback_match = cand;
            }
        }
        DestroyList(candidates);
    }

    if (fallback_match != NULL)
    {
        free(target_prefix);
        free(source_prefix);
        return fallback_match;
    }

    /* Last-resort lookup: scan visible scopes by mangled_id. */
    HashNode_t *scan_fallback = NULL;
    ListNode_t *scope = symtab->stack_head;
    while (scope != NULL)
    {
        HashTable_t *table = (HashTable_t *)scope->cur;
        if (table != NULL)
        {
            for (int i = 0; i < TABLE_SIZE; ++i)
            {
                for (ListNode_t *cur = table->table[i]; cur != NULL; cur = cur->next)
                {
                    HashNode_t *cand = (HashNode_t *)cur->cur;
                    if (cand == NULL || cand->mangled_id == NULL || cand->type == NULL ||
                        !kgpc_type_is_procedure(cand->type))
                    {
                        continue;
                    }
                    char *mangled = pascal_identifier_lower_dup(cand->mangled_id);
                    if (mangled == NULL)
                        continue;
                    int target_match = strstr(mangled, target_prefix) != NULL;
                    int source_match = strstr(mangled, source_prefix) != NULL;
                    free(mangled);
                    if (target_match)
                    {
                        free(target_prefix);
                        free(source_prefix);
                        return cand;
                    }
                    if (scan_fallback == NULL && source_match)
                        scan_fallback = cand;
                }
            }
        }
        scope = scope->next;
    }

    if (scan_fallback == NULL && symtab->builtins != NULL)
    {
        HashTable_t *table = symtab->builtins;
        for (int i = 0; i < TABLE_SIZE; ++i)
        {
            for (ListNode_t *cur = table->table[i]; cur != NULL; cur = cur->next)
            {
                HashNode_t *cand = (HashNode_t *)cur->cur;
                if (cand == NULL || cand->mangled_id == NULL || cand->type == NULL ||
                    !kgpc_type_is_procedure(cand->type))
                {
                    continue;
                }
                char *mangled = pascal_identifier_lower_dup(cand->mangled_id);
                if (mangled == NULL)
                    continue;
                int source_match = strstr(mangled, source_prefix) != NULL;
                free(mangled);
                if (source_match)
                {
                    scan_fallback = cand;
                    break;
                }
            }
            if (scan_fallback != NULL)
                break;
        }
    }

    free(target_prefix);
    free(source_prefix);
    return scan_fallback;
}

static int semcheck_try_record_conversion_expression(SymTab_t *symtab,
    struct Expression **expr_slot, struct Expression *target_expr,
    KgpcType *target_type, KgpcType **source_type, int *source_owned)
{
    if (symtab == NULL || expr_slot == NULL || *expr_slot == NULL ||
        target_type == NULL || source_type == NULL || *source_type == NULL)
    {
        return 0;
    }

    int target_is_pointer = kgpc_type_is_pointer(target_type) ||
        (target_type->kind == TYPE_KIND_PRIMITIVE &&
            target_type->info.primitive_type_tag == POINTER_TYPE);
    int target_is_record = semcheck_type_is_recordish(target_type);
    if ((!target_is_pointer && !target_is_record) || !semcheck_type_is_recordish(*source_type))
        return 0;

    struct Expression *source_expr = *expr_slot;
    const char *source_type_id = semcheck_record_type_id_from_expr(symtab, source_expr, *source_type);
    const char *target_type_id = semcheck_record_type_id_from_expr(symtab, target_expr, target_type);
    if (source_type_id == NULL || target_type_id == NULL)
        return 0;

    /* Class operator := is represented as *__op_assign in the symbol table. */
    const char *op_suffix = "op_assign";
    size_t name_len = strlen(target_type_id) + strlen(op_suffix) + strlen(source_type_id) + 4;
    char *operator_method = (char *)malloc(name_len);
    if (operator_method == NULL)
        return 0;

    HashNode_t *operator_node = NULL;
    int found_operator = 0;
    snprintf(operator_method, name_len, "%s__%s_%s", target_type_id, op_suffix, source_type_id);
    if (semcheck_find_operator_symbol(&operator_node, symtab, operator_method))
        found_operator = 1;

    if (!found_operator)
    {
        snprintf(operator_method, name_len, "%s__%s", target_type_id, op_suffix);
        if (semcheck_find_operator_symbol(&operator_node, symtab, operator_method))
            found_operator = 1;
    }

    if (!found_operator)
    {
        snprintf(operator_method, name_len, "%s__%s_%s", source_type_id, op_suffix, target_type_id);
        if (semcheck_find_operator_symbol(&operator_node, symtab, operator_method))
            found_operator = 1;
    }

    if (!found_operator)
    {
        snprintf(operator_method, name_len, "%s__%s", source_type_id, op_suffix);
        if (semcheck_find_operator_symbol(&operator_node, symtab, operator_method))
            found_operator = 1;
    }

    if (!found_operator)
    {
        operator_node = semcheck_find_record_assign_operator_candidate(symtab,
            target_type_id, source_type_id);
        if (operator_node != NULL)
            found_operator = 1;
    }

    if (!found_operator)
    {
        free(operator_method);
        return 0;
    }

    KgpcType *return_type = kgpc_type_get_return_type(operator_node->type);
    if (return_type == NULL || !are_types_compatible_for_assignment(target_type, return_type, symtab))
    {
        free(operator_method);
        return 0;
    }

    struct Expression *call_expr = mk_functioncall(source_expr->line_num, strdup(operator_method), NULL);
    call_expr->expr_data.function_call_data.args_expr = CreateListNode(source_expr, LIST_EXPR);
    if (operator_node->mangled_id != NULL)
        call_expr->expr_data.function_call_data.mangled_id = strdup(operator_node->mangled_id);
    else
        call_expr->expr_data.function_call_data.mangled_id = strdup(operator_method);
    call_expr->expr_data.function_call_data.resolved_func = operator_node;
    call_expr->expr_data.function_call_data.call_hash_type = HASHTYPE_FUNCTION;
    call_expr->expr_data.function_call_data.call_kgpc_type = operator_node->type;
    kgpc_type_retain(operator_node->type);
    call_expr->expr_data.function_call_data.is_call_info_valid = 1;

    if (call_expr->resolved_kgpc_type != NULL)
        destroy_kgpc_type(call_expr->resolved_kgpc_type);
    call_expr->resolved_kgpc_type = return_type;
    kgpc_type_retain(return_type);

    *expr_slot = call_expr;
    if (source_owned != NULL && *source_owned && *source_type != NULL)
        destroy_kgpc_type(*source_type);
    *source_type = return_type;
    if (source_owned != NULL)
        *source_owned = 0;

    free(operator_method);
    return 1;
}

static int semcheck_try_record_assignment_operator(SymTab_t *symtab,
    struct Statement *stmt, KgpcType *lhs_type, KgpcType **rhs_type,
    int *rhs_owned)
{
    if (symtab == NULL || stmt == NULL || lhs_type == NULL || rhs_type == NULL ||
        *rhs_type == NULL || stmt->type != STMT_VAR_ASSIGN)
        return 0;
    return semcheck_try_record_conversion_expression(symtab,
        &stmt->stmt_data.var_assign_data.expr, stmt->stmt_data.var_assign_data.var,
        lhs_type, rhs_type, rhs_owned);
}

static KgpcType *semcheck_param_effective_type(Tree_t *param_decl, KgpcType *expected)
{
    if (param_decl == NULL || expected == NULL)
        return expected;

    if (param_decl->type == TREE_VAR_DECL &&
        param_decl->tree_data.var_decl_data.is_var_param &&
        expected->kind == TYPE_KIND_POINTER &&
        expected->info.points_to != NULL)
    {
        /* var/out params may be modeled as pointers; compare against the pointee type. */
        return expected->info.points_to;
    }

    return expected;
}

static int semcheck_type_is_typed_file(KgpcType *type, struct SymTab *symtab)
{
    if (type == NULL || symtab == NULL)
        return 0;

    HashNode_t *typed_file_node = NULL;
    if (FindIdent(&typed_file_node, symtab, "TypedFile") < 0 || typed_file_node == NULL)
        return 0;
    if (typed_file_node->type == NULL)
        return 0;
    return typed_file_node->type == type;
}

static int semcheck_param_types_compatible(Tree_t *param_decl, KgpcType *expected, KgpcType *actual, SymTab_t *symtab)
{
    if (expected == NULL || actual == NULL)
        return 0;

    KgpcType *effective = semcheck_param_effective_type(param_decl, expected);

    if (param_decl != NULL && param_decl->type == TREE_VAR_DECL &&
        param_decl->tree_data.var_decl_data.is_var_param &&
        effective != NULL &&
        effective->kind == TYPE_KIND_PRIMITIVE &&
        actual->kind == TYPE_KIND_PRIMITIVE)
    {
        int expected_tag = effective->info.primitive_type_tag;
        int actual_tag = actual->info.primitive_type_tag;
        if (is_integer_type(expected_tag) && is_integer_type(actual_tag) &&
            expected_tag != actual_tag)
        {
            return 0;
        }
    }

    if (param_decl != NULL && param_decl->type == TREE_VAR_DECL &&
        actual->kind == TYPE_KIND_PRIMITIVE &&
        actual->info.primitive_type_tag == FILE_TYPE)
    {
        const char *type_id = param_decl->tree_data.var_decl_data.type_id;
        int actual_is_typed = semcheck_type_is_typed_file(actual, symtab);
        if (type_id != NULL)
        {
            if (pascal_identifier_equals(type_id, "TypedFile") && !actual_is_typed)
                return 0;
            if (pascal_identifier_equals(type_id, "File") && actual_is_typed)
                return 0;
        }
    }

    int compatible = are_types_compatible_for_assignment(effective, actual, symtab);
    if (compatible)
        return 1;

    if (param_decl != NULL && param_decl->type == TREE_VAR_DECL &&
        !param_decl->tree_data.var_decl_data.is_var_param &&
        actual->kind == TYPE_KIND_PRIMITIVE)
    {
        int expected_tag = semcheck_tag_from_kgpc(effective);
        int actual_tag = actual->info.primitive_type_tag;
        if ((expected_tag == LONGINT_TYPE && actual_tag == INT_TYPE) ||
            (expected_tag == INT64_TYPE &&
                (actual_tag == LONGINT_TYPE || actual_tag == INT_TYPE)))
        {
            return 1;
        }
    }

    if (param_decl != NULL && param_decl->type == TREE_VAR_DECL)
    {
        const char *type_id = param_decl->tree_data.var_decl_data.type_id;
        if (type_id != NULL && actual->kind == TYPE_KIND_PRIMITIVE)
        {
            int actual_tag = actual->info.primitive_type_tag;
            if (actual_tag == FILE_TYPE && pascal_identifier_equals(type_id, "File"))
                return 1;
            if (actual_tag == TEXT_TYPE && pascal_identifier_equals(type_id, "Text"))
                return 1;
        }
    }

    return 0;
}

/* Helper to check if a parameter has a default value */
static int param_has_default_value(Tree_t *decl)
{
    if (decl == NULL)
        return 0;
    
    if (decl->type == TREE_VAR_DECL)
    {
        /* Default value is stored in the initializer field */
        if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
            fprintf(stderr, "[SemCheck] param_has_default_value: TREE_VAR_DECL, initializer=%p\n",
                (void*)decl->tree_data.var_decl_data.initializer);
        }
        return decl->tree_data.var_decl_data.initializer != NULL;
    }
    else if (decl->type == TREE_ARR_DECL)
    {
        if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
            fprintf(stderr, "[SemCheck] param_has_default_value: TREE_ARR_DECL, initializer=%p\n",
                (void*)decl->tree_data.arr_decl_data.initializer);
        }
        return decl->tree_data.arr_decl_data.initializer != NULL;
    }
    
    return 0;
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
        return init->stmt_data.var_assign_data.expr;

    return NULL;
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
            semcheck_error_with_context("Error on line %d, missing default value expression.\n", line_num);
            return 1;
        }

        struct Expression *default_clone = clone_expression(default_expr);
        if (default_clone == NULL)
        {
            semcheck_error_with_context("Error on line %d, failed to clone default argument expression.\n", line_num);
            return 1;
        }

        ListNode_t *node = CreateListNode(default_clone, LIST_EXPR);
        if (node == NULL)
        {
            destroy_expr(default_clone);
            semcheck_error_with_context("Error on line %d, failed to allocate default argument node.\n", line_num);
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

/* Helper to get the default value expression from a parameter */
static struct Expression *get_param_default_value_stmt(Tree_t *decl)
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

/* Copy a default value expression for use as an argument */
static struct Expression *copy_default_expr(struct Expression *src)
{
    if (src == NULL)
        return NULL;
    
    struct Expression *copy = NULL;
    
    switch (src->type)
    {
        case EXPR_INUM:
            copy = mk_inum(src->line_num, src->expr_data.i_num);
            break;
        case EXPR_RNUM:
            copy = mk_rnum(src->line_num, src->expr_data.r_num);
            break;
        case EXPR_STRING:
            if (src->expr_data.string != NULL)
                copy = mk_string(src->line_num, strdup(src->expr_data.string));
            break;
        case EXPR_BOOL:
            copy = mk_bool(src->line_num, src->expr_data.bool_value);
            break;
        case EXPR_CHAR_CODE:
            copy = mk_charcode(src->line_num, src->expr_data.char_code);
            break;
        case EXPR_NIL:
            copy = (struct Expression *)malloc(sizeof(struct Expression));
            if (copy != NULL) {
                memset(copy, 0, sizeof(struct Expression));
                copy->type = EXPR_NIL;
                copy->line_num = src->line_num;
            }
            break;
        case EXPR_VAR_ID:
            /* Handle constant references like CPUEndian in default parameters */
            if (src->expr_data.id != NULL)
            {
                copy = (struct Expression *)malloc(sizeof(struct Expression));
                if (copy != NULL) {
                    memset(copy, 0, sizeof(struct Expression));
                    copy->type = EXPR_VAR_ID;
                    copy->line_num = src->line_num;
                    copy->expr_data.id = strdup(src->expr_data.id);
                    if (copy->expr_data.id == NULL) {
                        free(copy);
                        copy = NULL;
                    }
                }
            }
            break;
        case EXPR_SET:
            /* Support defaults like [] used by sysutils DateTimeToString options. */
            if (src->expr_data.set_data.elements == NULL)
            {
                copy = mk_set(src->line_num, src->expr_data.set_data.bitmask, NULL,
                    src->expr_data.set_data.is_constant);
            }
            break;
        default:
            /* For complex expressions, we can't easily copy them.
             * Return NULL and let the caller handle the error. */
            if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
                fprintf(stderr, "[SemCheck] copy_default_expr: unsupported expr type %d\n", src->type);
            }
            break;
    }
    
    return copy;
}

static int semcheck_loop_depth = 0;
/* Debug helpers used for corruption watchdog logging. */
static struct Statement *g_debug_watch_stmt = NULL;
static struct Expression *g_debug_watch_to_expr = NULL;

/* Resolve the RecordType for a TFPGList specialization from a LHS expression.
 * This bypasses incomplete kgpc_type inference and looks directly at the symbol
 * table entry for the variable or type identifier. */
static int is_tfpglist_type_id(const char *type_id)
{
    return (type_id != NULL &&
            strncasecmp(type_id, "TFPGList$", strlen("TFPGList$")) == 0);
}

/* Resolve the RecordType for a TFPGList specialization from a LHS expression.
 * This bypasses incomplete kgpc_type inference and looks directly at the symbol
 * table entry for the variable or type identifier. */
static struct RecordType *resolve_tfpglist_record_from_lhs(SymTab_t *symtab,
    struct Expression *lhs)
{
    if (symtab == NULL || lhs == NULL)
        return NULL;

    if (lhs->type != EXPR_VAR_ID || lhs->expr_data.id == NULL)
        return NULL;

    HashNode_t *node = NULL;
    if (FindIdent(&node, symtab, lhs->expr_data.id) < 0 || node == NULL)
        return NULL;

    struct RecordType *record = hashnode_get_record_type(node);
    if (record == NULL || record->type_id == NULL)
        return NULL;

    if (!is_tfpglist_type_id(record->type_id))
        return NULL;

    return record;
}

static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    return hashnode_get_type_alias(node);
}

static HashNode_t *lookup_hashnode(SymTab_t *symtab, const char *id)
{
    if (symtab == NULL || id == NULL)
        return NULL;
    HashNode_t *node = NULL;
    if (FindIdent(&node, symtab, id) >= 0 && node != NULL)
        return node;
    return NULL;
}

int resolve_record_field(SymTab_t *symtab, struct RecordType *record,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int line_num, int silent);
int resolve_param_type(Tree_t *decl, SymTab_t *symtab);

static const char *resolve_tfpglist_specialized_id_from_typename(SymTab_t *symtab, const char *type_name)
{
    HashNode_t *type_node = lookup_hashnode(symtab, type_name);
    if (type_node == NULL)
        return NULL;

    struct RecordType *record = hashnode_get_record_type(type_node);
    if (record != NULL && is_tfpglist_type_id(record->type_id))
        return record->type_id;

    struct TypeAlias *alias = get_type_alias_from_node(type_node);
    if (alias != NULL && alias->target_type_id != NULL &&
        is_tfpglist_type_id(alias->target_type_id))
        return alias->target_type_id;

    return NULL;
}

static const char *resolve_tfpglist_specialized_id_from_expr(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return NULL;
    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
        return resolve_tfpglist_specialized_id_from_typename(symtab, expr->expr_data.id);
    return NULL;
}

static struct Expression *make_tfpglist_ctor_expr(struct RecordType *record, int line_num)
{
    if (record == NULL || record->type_id == NULL)
        return NULL;

    const char *type_id = record->type_id;
    const char *prefix = "__tfpg_ctor$";
    size_t len = strlen(prefix) + strlen(type_id) + 1;
    char *ctor_name = (char *)malloc(len);
    if (ctor_name == NULL)
        return NULL;
    strcpy(ctor_name, prefix);
    strcat(ctor_name, type_id);

    struct Expression *call = mk_functioncall(line_num, ctor_name, NULL);
    if (call == NULL)
        return NULL;

    /* Set the mangled_id to the actual Create constructor method name */
    /* Format: ClassName__Create_u */
    size_t mangled_len = strlen(type_id) + strlen("__Create_u") + 1;
    char *mangled_name = (char *)malloc(mangled_len);
    if (mangled_name != NULL)
    {
        strcpy(mangled_name, type_id);
        strcat(mangled_name, "__Create_u");
        call->expr_data.function_call_data.mangled_id = mangled_name;
        
        if (getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
        {
            fprintf(stderr, "[KGPC] TFPG ctor: set mangled_id to %s\n", mangled_name);
        }
    }

    if (call->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(call->resolved_kgpc_type);
        call->resolved_kgpc_type = NULL;
    }
    call->resolved_kgpc_type = create_record_type(record);
    return call;
}

static int rewrite_tfpglist_constructor_if_needed(SymTab_t *symtab,
    int max_scope_lev, struct Expression *lhs, struct Expression **rhs_ptr)
{
    if (symtab == NULL || lhs == NULL || rhs_ptr == NULL || *rhs_ptr == NULL)
        return 0;
    const char *debug_env = getenv("KGPC_DEBUG_GENERIC_CLONES");

    struct RecordType *lhs_record = resolve_tfpglist_record_from_lhs(symtab, lhs);
    if (lhs_record == NULL || lhs_record->type_id == NULL)
    {
        if (debug_env)
        {
            fprintf(stderr,
                "[KGPC] TFPG ctor: lhs %s is not TFPGList specialization\n",
                (lhs->expr_data.id != NULL) ? lhs->expr_data.id : "<expr>");
        }
        return 0;
    }

    const char *expected_specialized_id = lhs_record->type_id;
    struct Expression *rhs = *rhs_ptr;

    int matches_pattern = 0;
    if (rhs->type == EXPR_RECORD_ACCESS &&
        rhs->expr_data.record_access_data.field_id != NULL &&
        strcasecmp(rhs->expr_data.record_access_data.field_id, "Create") == 0)
    {
        const char *candidate = resolve_tfpglist_specialized_id_from_expr(symtab,
            rhs->expr_data.record_access_data.record_expr);
        if (candidate != NULL &&
            strcasecmp(candidate, expected_specialized_id) == 0)
            matches_pattern = 1;
    }
    else if (rhs->type == EXPR_FUNCTION_CALL &&
             rhs->expr_data.function_call_data.id != NULL)
    {
        const char *candidate = resolve_tfpglist_specialized_id_from_typename(
            symtab, rhs->expr_data.function_call_data.id);
        if (candidate != NULL &&
            strcasecmp(candidate, expected_specialized_id) == 0)
        {
            /* Legacy lowering produced a dummy argument referencing the type */
            ListNode_t *args = rhs->expr_data.function_call_data.args_expr;
            if (args == NULL)
                matches_pattern = 1;
            else if (args->next == NULL)
            {
                struct Expression *arg_expr = (struct Expression *)args->cur;
                if (arg_expr != NULL && arg_expr->type == EXPR_VAR_ID &&
                    arg_expr->expr_data.id != NULL &&
                    pascal_identifier_equals(arg_expr->expr_data.id,
                        rhs->expr_data.function_call_data.id))
                    matches_pattern = 1;
            }
        }
    }

    if (!matches_pattern)
    {
        if (debug_env)
            fprintf(stderr, "[KGPC] TFPG ctor: rhs did not match constructor pattern\n");
        return 0;
    }

    struct Expression *ctor_expr =
        make_tfpglist_ctor_expr(lhs_record, rhs->line_num);
    if (ctor_expr == NULL)
    {
        if (debug_env)
            fprintf(stderr, "[KGPC] TFPG ctor: failed to build ctor expression\n");
        return 0;
    }

    if (debug_env)
        fprintf(stderr, "[KGPC] TFPG ctor: rewriting ctor for %s\n",
            expected_specialized_id);

    destroy_expr(rhs);
    *rhs_ptr = ctor_expr;
    return 1;
}
static void semcheck_stmt_set_call_kgpc_type(struct Statement *stmt, KgpcType *type,
    int owns_existing)
{
    if (stmt == NULL || stmt->type != STMT_PROCEDURE_CALL)
        return;

    if (stmt->stmt_data.procedure_call_data.call_kgpc_type != NULL && owns_existing)
    {
        destroy_kgpc_type(stmt->stmt_data.procedure_call_data.call_kgpc_type);
    }
    stmt->stmt_data.procedure_call_data.call_kgpc_type = NULL;

    if (type != NULL)
    {
        kgpc_type_retain(type);
        stmt->stmt_data.procedure_call_data.call_kgpc_type = type;
    }
}

static int semcheck_expr_is_char_set(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr->resolved_kgpc_type != NULL)
    {
        struct TypeAlias *alias = expr->resolved_kgpc_type->type_alias;
        if (alias != NULL && alias->is_set &&
            (alias->set_element_type == CHAR_TYPE ||
             (alias->set_element_type_id != NULL &&
              (pascal_identifier_equals(alias->set_element_type_id, "Char") ||
               pascal_identifier_equals(alias->set_element_type_id, "AnsiChar")))))
            return 1;
    }

    if (expr->type == EXPR_VAR_ID && symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, symtab, expr->expr_data.id) >= 0 && node != NULL)
        {
            struct TypeAlias *alias = get_type_alias_from_node(node);
            if (alias != NULL && alias->is_set &&
                (alias->set_element_type == CHAR_TYPE ||
                 (alias->set_element_type_id != NULL &&
                  (pascal_identifier_equals(alias->set_element_type_id, "Char") ||
                   pascal_identifier_equals(alias->set_element_type_id, "AnsiChar")))))
                return 1;
        }
    }

    return 0;
}

/* Helper to check if a TypeAlias represents WideChar/UnicodeChar.
 * WideChar = Word (integer type), so we check alias_name, not CHAR_TYPE. */
static int semcheck_alias_is_widechar(struct TypeAlias *alias)
{
    if (alias == NULL)
        return 0;
    /* Check alias_name - this is the declared type name (e.g., "WideChar") */
    if (alias->alias_name != NULL &&
        (pascal_identifier_equals(alias->alias_name, "WideChar") ||
         pascal_identifier_equals(alias->alias_name, "UnicodeChar")))
        return 1;
    return 0;
}

/* Check if an expression's type is WideChar (or aliased to WideChar).
 * WideChar = Word (integer type), so we check alias_name, not CHAR_TYPE. */
static int semcheck_expr_is_widechar(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    /* Check resolved_kgpc_type */
    if (expr->resolved_kgpc_type != NULL)
    {
        KgpcType *ktype = expr->resolved_kgpc_type;
        struct TypeAlias *alias = ktype->type_alias;
        if (semcheck_alias_is_widechar(alias))
            return 1;
    }

    /* For EXPR_VAR_ID, look up the variable's type */
    if (expr->type == EXPR_VAR_ID && symtab != NULL && expr->expr_data.id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, symtab, expr->expr_data.id) >= 0 && node != NULL)
        {
            if (node->type != NULL)
            {
                KgpcType *ntype = node->type;
                /* Check alias_name in KgpcType's type_alias */
                if (semcheck_alias_is_widechar(ntype->type_alias))
                    return 1;
            }

            /* Check TypeAlias from node directly (fallback) */
            struct TypeAlias *alias = get_type_alias_from_node(node);
            if (semcheck_alias_is_widechar(alias))
                return 1;
        }
    }

    return 0;
}

int semcheck_stmt_main(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);

int semcheck_varassign(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_proccall(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_funccall(int *type_return, SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_compoundstmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_ifthen(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_while(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_repeat(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_for(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_for_in(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_for_assign(SymTab_t *symtab, struct Statement *for_assign, int max_scope_lev);

static int semcheck_statement_list_nodes(SymTab_t *symtab, ListNode_t *stmts, int max_scope_lev);
static int semcheck_call_with_proc_var(SymTab_t *symtab, struct Statement *stmt, HashNode_t *proc_node,
    int max_scope_lev);
static int semcheck_try_property_assignment(SymTab_t *symtab,
    struct Statement *stmt, int max_scope_lev);
static int semcheck_try_module_property_assignment(SymTab_t *symtab,
    struct Statement *stmt, int max_scope_lev);
static int semcheck_convert_property_assignment_to_setter(SymTab_t *symtab,
    struct Statement *stmt, struct Expression *lhs, HashNode_t *setter_node,
    int max_scope_lev);
static int semcheck_mangled_suffix_matches_untyped(const char *candidate_suffix,
    const char *call_suffix);
static HashNode_t *semcheck_find_untyped_mangled_match(ListNode_t *candidates,
    const char *proc_id, const char *call_mangled);
static int semcheck_var_decl_is_untyped(Tree_t *decl);
static int semcheck_set_stmt_call_mangled_id(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);

static int semcheck_call_with_proc_var(SymTab_t *symtab, struct Statement *stmt, HashNode_t *proc_node,
    int max_scope_lev)
{
    (void)max_scope_lev;
    if (proc_node == NULL || proc_node->type == NULL ||
        proc_node->type->kind != TYPE_KIND_PROCEDURE)
        return 0;

    int return_val = 0;
    ListNode_t *formal_params = proc_node->type->info.proc_info.params;
    ListNode_t *args_given = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_index = 0;

    while (formal_params != NULL && args_given != NULL)
    {
        ++arg_index;
        assert(formal_params->type == LIST_TREE);
        assert(args_given->type == LIST_EXPR);

        Tree_t *param_decl = (Tree_t *)formal_params->cur;
        struct Expression *arg_expr = (struct Expression *)args_given->cur;

        /* Phase 3: Use KgpcType for comprehensive type checking */
        
        /* Resolve KgpcType for the argument expression */
        int arg_type_owned = 0;
        KgpcType *arg_type = semcheck_resolve_expression_kgpc_type(symtab, arg_expr, INT_MAX, NO_MUTATE, &arg_type_owned);
        
        /* Resolve KgpcType for the formal parameter */
        int param_type_owned = 0;
        KgpcType *param_type = NULL;
        if (param_decl != NULL && param_decl->type == TREE_VAR_DECL)
        {
            param_type = resolve_type_from_vardecl(param_decl, symtab, &param_type_owned);
        }



        /* Both types must be resolved for proper type checking */
        int param_is_untyped = semcheck_var_decl_is_untyped(param_decl);

        if ((arg_type == NULL || param_type == NULL) && !param_is_untyped)
        {
            fprintf(stderr,
                "Error on line %d, on procedure call %s, argument %d: Unable to resolve type!\n\n",
                stmt->line_num,
                stmt->stmt_data.procedure_call_data.id,
                arg_index);
            ++return_val;
        }
        else if (!param_is_untyped)
        {
            /* Use comprehensive KgpcType-based type compatibility checking */
            if (!semcheck_param_types_compatible(param_decl, param_type, arg_type, symtab))
            {
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                    "Error on line %d, on procedure call %s, argument %d: Type mismatch (expected %s, got %s)!\n\n",
                    stmt->line_num,
                    stmt->stmt_data.procedure_call_data.id,
                    arg_index,
                    kgpc_type_to_string(semcheck_param_effective_type(param_decl, param_type)),
                    kgpc_type_to_string(arg_type));
                ++return_val;
            }
        }
        /* Untyped parameters accept any argument without additional checks */

        /* Clean up owned types */
        if (arg_type_owned && arg_type != NULL)
            destroy_kgpc_type(arg_type);
        if (param_type_owned && param_type != NULL)
            destroy_kgpc_type(param_type);

        formal_params = formal_params->next;
        args_given = args_given->next;
    }

    if (formal_params == NULL && args_given != NULL)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
            "Error on line %d, on procedure call %s, too many arguments given!\n\n",
            stmt->line_num, stmt->stmt_data.procedure_call_data.id);
        ++return_val;
    }
    else if (formal_params != NULL && args_given == NULL)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
            "Error on line %d, on procedure call %s, not enough arguments given!\n\n",
            stmt->line_num, stmt->stmt_data.procedure_call_data.id);
        ++return_val;
    }

    return return_val;
}

typedef int (*builtin_semcheck_handler_t)(SymTab_t *, struct Statement *, int);

static int try_resolve_builtin_procedure(SymTab_t *symtab,
    struct Statement *stmt,
    const char *expected_name,
    builtin_semcheck_handler_t handler,
    int max_scope_lev,
    int *handled)
{
    if (handled != NULL)
        *handled = 0;

    if (symtab == NULL || stmt == NULL || expected_name == NULL || handler == NULL)
        return 0;

    char *proc_id = stmt->stmt_data.procedure_call_data.id;
    if (proc_id == NULL || !pascal_identifier_equals(proc_id, expected_name))
        return 0;

    /* Prefer user-defined/prologue procedures over builtins when available. */
    HashNode_t *existing = NULL;
    int force_builtin = pascal_identifier_equals(expected_name, "Assign");
    if (!force_builtin &&
        FindIdent(&existing, symtab, proc_id) != -1 && existing != NULL &&
        existing->hash_type != HASHTYPE_BUILTIN_PROCEDURE)
    {
        return 0;
    }

    HashNode_t *builtin_node = FindIdentInTable(symtab->builtins, proc_id);
    if (builtin_node != NULL && builtin_node->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
    {
        stmt->stmt_data.procedure_call_data.resolved_proc = builtin_node;
        stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        
        /* Populate call info to avoid use-after-free when HashNode is freed */
        stmt->stmt_data.procedure_call_data.call_hash_type = builtin_node->hash_type;
        semcheck_stmt_set_call_kgpc_type(stmt, builtin_node->type,
            stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
        stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
        
        builtin_node->referenced += 1;
        if (handled != NULL)
            *handled = 1;
        return handler(symtab, stmt, max_scope_lev);
    }

    return 0;
}

static int semcheck_builtin_setlength(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, SetLength expects exactly two arguments.\n", stmt->line_num);
        return 1;
    }

    struct Expression *array_expr = (struct Expression *)args->cur;
    struct Expression *length_expr = (struct Expression *)args->next->cur;
    
#ifdef DEBUG
    fprintf(stderr, "DEBUG: semcheck_builtin_setlength length_expr=%p\n", length_expr);
#endif

    int target_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&target_type, symtab, array_expr, max_scope_lev, MUTATE);
    int target_is_shortstring = semcheck_expr_is_shortstring(array_expr);

    int target_is_string = (target_type == STRING_TYPE);
    /* Fallback: check KgpcType for string (e.g. function result vars with overloads) */
    if (!target_is_string && !target_is_shortstring &&
        array_expr != NULL && array_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_string(array_expr->resolved_kgpc_type))
    {
        target_is_string = 1;
    }
    if (target_is_string)
    {
        int target_is_dynarray = 0;
        if (array_expr != NULL && array_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_dynamic_array(array_expr->resolved_kgpc_type))
        {
            target_is_dynarray = 1;
        }
        else if (array_expr != NULL && array_expr->type == EXPR_VAR_ID)
        {
            HashNode_t *array_node = NULL;
            if (FindIdent(&array_node, symtab, array_expr->expr_data.id) != -1 &&
                array_node != NULL && array_node->type != NULL &&
                kgpc_type_is_dynamic_array(array_node->type))
            {
                target_is_dynarray = 1;
            }
        }
        if (target_is_dynarray)
            target_is_string = 0;
    }

    if (target_is_string || target_is_shortstring)
    {
        if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
        {
            free(stmt->stmt_data.procedure_call_data.mangled_id);
            stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        }
        if (target_is_shortstring)
            stmt->stmt_data.procedure_call_data.mangled_id = strdup("__kgpc_setlength_shortstring");
        else
            stmt->stmt_data.procedure_call_data.mangled_id = strdup("__kgpc_setlength_string");
        if (stmt->stmt_data.procedure_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for SetLength.\n");
            ++return_val;
        }
    }
    else
    {
        /* After semantic checking, check if the expression resolved to a dynamic array */
        /* The expression could be EXPR_VAR_ID, EXPR_RECORD_ACCESS, etc. */
        int is_valid_array = 0;
        
        if (array_expr != NULL && array_expr->type == EXPR_VAR_ID)
        {
            /* Simple variable reference */
            HashNode_t *array_node = NULL;
            if (FindIdent(&array_node, symtab, array_expr->expr_data.id) != -1 && array_node != NULL)
            {
                set_hash_meta(array_node, BOTH_MUTATE_REFERENCE);
                
                /* Check if it's a dynamic array using KgpcType first, then legacy field */
                int is_dynamic = 0;
                if (array_node->type != NULL)
                {
                    is_dynamic = kgpc_type_is_dynamic_array(array_node->type);
                }
                else
                {
                    is_dynamic = hashnode_is_dynamic_array(array_node);
                }
                
                if (is_dynamic &&
                    (array_node->hash_type == HASHTYPE_ARRAY ||
                     array_node->hash_type == HASHTYPE_VAR ||
                     array_node->hash_type == HASHTYPE_FUNCTION_RETURN))
                {
                    is_valid_array = 1;
                }
            }
        }
        else if (array_expr != NULL && array_expr->type == EXPR_RECORD_ACCESS)
        {
            /* Record field access - if semantic check passed, assume it's valid
             * TODO: Could enhance this to verify the field is actually a dynamic array */
            is_valid_array = 1;
        }
        
        if (!is_valid_array)
        {
            semcheck_error_with_context("Error on line %d, first argument to SetLength must be a dynamic array variable.\n", stmt->line_num);
            ++return_val;
        }
    }

    int length_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&length_type, symtab, length_expr, max_scope_lev, NO_MUTATE);
    if (!is_integer_type(length_type))
    {
        semcheck_error_with_context("Error on line %d, SetLength length argument must be an integer.\n", stmt->line_num);
        ++return_val;
    }

    return return_val;
}

static int semcheck_builtin_setstring(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next == NULL || args->next->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, SetString expects exactly three arguments.\n", stmt->line_num);
        return 1;
    }

    struct Expression *string_expr = (struct Expression *)args->cur;
    struct Expression *buffer_expr = (struct Expression *)args->next->cur;
    struct Expression *length_expr = (struct Expression *)args->next->next->cur;

    /* First argument must be a string variable (output parameter) */
    int string_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&string_type, symtab, string_expr, max_scope_lev, MUTATE);
    int target_is_shortstring = semcheck_expr_is_shortstring(string_expr);
    if (string_type != STRING_TYPE && string_type != UNKNOWN_TYPE && !target_is_shortstring)
    {
        semcheck_error_with_context("Error on line %d, SetString first argument must be a string variable.\n", stmt->line_num);
        ++return_val;
    }

    /* Second argument must be a PChar/pointer to char */
    int buffer_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&buffer_type, symtab, buffer_expr, max_scope_lev, NO_MUTATE);
    if (buffer_type != POINTER_TYPE && buffer_type != UNKNOWN_TYPE)
    {
        /* Allow if it's an array of char or similar */
        int is_valid = 0;
        if (buffer_expr != NULL && buffer_expr->resolved_kgpc_type != NULL)
        {
            KgpcType *t = buffer_expr->resolved_kgpc_type;
            if (t->kind == TYPE_KIND_POINTER)
                is_valid = 1;
        }
        if (!is_valid)
        {
            semcheck_error_with_context("Error on line %d, SetString second argument must be a pointer (PChar).\n", stmt->line_num);
            ++return_val;
        }
    }

    /* Third argument must be an integer length */
    int length_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&length_type, symtab, length_expr, max_scope_lev, NO_MUTATE);
    if (!is_integer_type(length_type))
    {
        semcheck_error_with_context("Error on line %d, SetString length argument must be an integer.\n", stmt->line_num);
        ++return_val;
    }

    /* Set the mangled function name for codegen */
    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
    {
        free(stmt->stmt_data.procedure_call_data.mangled_id);
        stmt->stmt_data.procedure_call_data.mangled_id = NULL;
    }
    if (target_is_shortstring)
        stmt->stmt_data.procedure_call_data.mangled_id = strdup("kgpc_shortstring_setstring");
    else
        stmt->stmt_data.procedure_call_data.mangled_id = strdup("kgpc_setstring");
    if (stmt->stmt_data.procedure_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for SetString.\n");
        ++return_val;
    }

    return return_val;
}

static int semcheck_statement_list_nodes(SymTab_t *symtab, ListNode_t *stmts, int max_scope_lev)
{
    int result = 0;
    ListNode_t *cursor = stmts;
    while (cursor != NULL)
    {
        if (cursor->type == LIST_STMT && cursor->cur != NULL)
            result += semcheck_stmt_main(symtab, (struct Statement *)cursor->cur, max_scope_lev);
        cursor = cursor->next;
    }
    return result;
}

static int semcheck_var_decl_is_untyped(Tree_t *decl)
{
    if (decl == NULL || decl->type != TREE_VAR_DECL)
        return 0;
    struct Var *var_info = &decl->tree_data.var_decl_data;
    if (var_info->inline_record_type != NULL)
        return 0;
    return (var_info->type == UNKNOWN_TYPE && var_info->type_id == NULL);
}

static int semcheck_mangled_suffix_matches_untyped(const char *candidate_suffix,
    const char *call_suffix)
{
    if (candidate_suffix == NULL || call_suffix == NULL)
        return 0;

    if (*candidate_suffix == '\0' && *call_suffix == '\0')
        return 1;

    while (*candidate_suffix != '\0' && *call_suffix != '\0')
    {
        if (*candidate_suffix != '_' || *call_suffix != '_')
            return 0;
        candidate_suffix++;
        call_suffix++;

        const char *cand_end = candidate_suffix;
        while (*cand_end != '_' && *cand_end != '\0')
            cand_end++;
        const char *call_end = call_suffix;
        while (*call_end != '_' && *call_end != '\0')
            call_end++;

        size_t cand_len = (size_t)(cand_end - candidate_suffix);
        size_t call_len = (size_t)(call_end - call_suffix);
        int candidate_is_untyped = (cand_len == 1 && candidate_suffix[0] == 'u');

        if (!candidate_is_untyped)
        {
            if (cand_len != call_len || strncmp(candidate_suffix, call_suffix, cand_len) != 0)
                return 0;
        }

        candidate_suffix = cand_end;
        call_suffix = call_end;
    }

    return (*candidate_suffix == '\0' && *call_suffix == '\0');
}

static HashNode_t *semcheck_find_untyped_mangled_match(ListNode_t *candidates,
    const char *proc_id, const char *call_mangled)
{
    if (candidates == NULL || proc_id == NULL || call_mangled == NULL)
        return NULL;

    size_t call_prefix_len = strlen(proc_id);
    if (strlen(call_mangled) < call_prefix_len ||
        strncmp(call_mangled, proc_id, call_prefix_len) != 0)
        return NULL;

    const char *call_suffix = call_mangled + call_prefix_len;
    ListNode_t *cur = candidates;
    while (cur != NULL)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate != NULL && candidate->mangled_id != NULL && candidate->id != NULL)
        {
            if (pascal_identifier_equals(candidate->id, proc_id))
            {
                size_t cand_prefix_len = strlen(candidate->id);
                if (strlen(candidate->mangled_id) >= cand_prefix_len &&
                    strncmp(candidate->mangled_id, candidate->id, cand_prefix_len) == 0)
                {
                    const char *cand_suffix = candidate->mangled_id + cand_prefix_len;
                    if (semcheck_mangled_suffix_matches_untyped(cand_suffix, call_suffix))
                        return candidate;
                }
            }
        }
        cur = cur->next;
    }

    return NULL;
}

static int semcheck_builtin_strproc(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Str expects exactly two arguments.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    struct Expression *value_expr = (struct Expression *)args->cur;
    struct Expression *target_expr = (struct Expression *)args->next->cur;

    int value_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&value_type, symtab, value_expr, INT_MAX, NO_MUTATE);
    if (!is_integer_type(value_type) && value_type != REAL_TYPE)
    {
        semcheck_error_with_context("Error on line %d, Str value must be an integer or real.\n", stmt->line_num);
        ++return_val;
    }

    if (value_expr != NULL && value_expr->field_width != NULL)
    {
        int width_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&width_type, symtab, value_expr->field_width, INT_MAX, NO_MUTATE);
        if (!is_integer_type(width_type))
        {
            semcheck_error_with_context("Error on line %d, Str field width must be an integer.\n", stmt->line_num);
            ++return_val;
        }
    }

    if (value_expr != NULL && value_expr->field_precision != NULL)
    {
        int precision_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&precision_type, symtab, value_expr->field_precision, INT_MAX, NO_MUTATE);
        if (!is_integer_type(precision_type))
        {
            semcheck_error_with_context("Error on line %d, Str field precision must be an integer.\n", stmt->line_num);
            ++return_val;
        }
    }

    int target_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    if (target_type != STRING_TYPE && target_type != SHORTSTRING_TYPE)
    {
        semcheck_error_with_context("Error on line %d, Str output must be a string variable.\n", stmt->line_num);
        ++return_val;
    }

    return return_val;
}

static int semcheck_builtin_insert(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next == NULL ||
        args->next->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Insert expects exactly three arguments.\n",
            stmt->line_num);
        return 1;
    }

    int error_count = 0;
    struct Expression *source_expr = (struct Expression *)args->cur;
    struct Expression *target_expr = (struct Expression *)args->next->cur;
    struct Expression *index_expr = (struct Expression *)args->next->next->cur;

    int source_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&source_type, symtab, source_expr, max_scope_lev, NO_MUTATE);
    int source_is_shortstring = semcheck_expr_is_shortstring(source_expr);
    if (source_type != STRING_TYPE && source_type != CHAR_TYPE && !source_is_shortstring)
    {
        semcheck_error_with_context("Error on line %d, Insert source must be a string or char.\n",
            stmt->line_num);
        ++error_count;
    }

    int target_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    int target_is_shortstring = semcheck_expr_is_shortstring(target_expr);
    if (target_type != STRING_TYPE && !target_is_shortstring)
    {
        semcheck_error_with_context("Error on line %d, Insert target must be a string variable.\n",
            stmt->line_num);
        ++error_count;
    }

    int index_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&index_type, symtab, index_expr, max_scope_lev, NO_MUTATE);
    if (!is_integer_type(index_type))
    {
        semcheck_error_with_context("Error on line %d, Insert index must be an integer.\n",
            stmt->line_num);
        ++error_count;
    }

    if (error_count == 0 && target_is_shortstring)
    {
        if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
        {
            free(stmt->stmt_data.procedure_call_data.mangled_id);
            stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        }
        stmt->stmt_data.procedure_call_data.mangled_id = strdup("kgpc_shortstring_insert");
        if (stmt->stmt_data.procedure_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Insert.\n");
            ++error_count;
        }
    }

    return error_count;
}

static int semcheck_builtin_delete(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next == NULL ||
        args->next->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Delete expects exactly three arguments.\n",
            stmt->line_num);
        return 1;
    }

    int error_count = 0;
    struct Expression *target_expr = (struct Expression *)args->cur;
    struct Expression *index_expr = (struct Expression *)args->next->cur;
    struct Expression *count_expr = (struct Expression *)args->next->next->cur;

    int target_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    
    /* Check if target is a string type OR a shortstring (array of char) */
    int is_valid_target = is_string_type(target_type) ||
                          is_shortstring_array(target_type, target_expr->is_array_expr);
    int target_is_shortstring = semcheck_expr_is_shortstring(target_expr);
    
    if (!is_valid_target)
    {
        semcheck_error_with_context("Error on line %d, Delete target must be a string variable.\n",
            stmt->line_num);
        ++error_count;
    }

    int index_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&index_type, symtab, index_expr, max_scope_lev, NO_MUTATE);
    if (!is_integer_type(index_type))
    {
        semcheck_error_with_context("Error on line %d, Delete index must be an integer.\n",
            stmt->line_num);
        ++error_count;
    }

    int count_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&count_type, symtab, count_expr, max_scope_lev, NO_MUTATE);
    if (!is_integer_type(count_type))
    {
        semcheck_error_with_context("Error on line %d, Delete count must be an integer.\n",
            stmt->line_num);
        ++error_count;
    }

    if (error_count == 0 && target_is_shortstring)
    {
        if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
        {
            free(stmt->stmt_data.procedure_call_data.mangled_id);
            stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        }
        stmt->stmt_data.procedure_call_data.mangled_id = strdup("kgpc_shortstring_delete");
        if (stmt->stmt_data.procedure_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Delete.\n");
            ++error_count;
        }
    }

    return error_count;
}

static int semcheck_builtin_val(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next == NULL ||
        args->next->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Val expects exactly three arguments.\n",
            stmt->line_num);
        return 1;
    }

    int error_count = 0;

    struct Expression *source_expr = (struct Expression *)args->cur;
    int source_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&source_type, symtab, source_expr, max_scope_lev, NO_MUTATE);
    if (!is_string_type(source_type) && source_type != CHAR_TYPE)
    {
        semcheck_error_with_context("Error on line %d, Val expects its first argument to be a string.\n",
            stmt->line_num);
        ++error_count;
    }

    struct Expression *value_expr = (struct Expression *)args->next->cur;
    int value_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&value_type, symtab, value_expr, max_scope_lev, MUTATE);
    if (!is_integer_type(value_type) && value_type != REAL_TYPE)
    {
        fprintf(stderr,
            "Error on line %d, Val target must be an integer, longint, or real variable.\n",
            stmt->line_num);
        ++error_count;
    }

    struct Expression *code_expr = (struct Expression *)args->next->next->cur;
    int code_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&code_type, symtab, code_expr, max_scope_lev, MUTATE);
    if (!is_integer_type(code_type))
    {
        semcheck_error_with_context("Error on line %d, Val code argument must be an integer variable.\n",
            stmt->line_num);
        ++error_count;
    }

    return error_count;
}

static int semcheck_builtin_inc(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || (args->next != NULL && args->next->next != NULL))
    {
        semcheck_error_with_context("Error on line %d, Inc expects one or two arguments.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    struct Expression *target_expr = (struct Expression *)args->cur;
    int target_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    int target_is_pointer = (target_type == POINTER_TYPE);
    if (!is_ordinal_type(target_type) && !target_is_pointer)
    {
        semcheck_error_with_context("Error on line %d, Inc target must be an ordinal or pointer variable.\n", stmt->line_num);
        ++return_val;
    }

    if (args->next != NULL)
    {
        struct Expression *value_expr = (struct Expression *)args->next->cur;
        int value_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);
        if (!is_integer_type(value_type))
        {
            semcheck_error_with_context("Error on line %d, Inc increment must be an integer.\n", stmt->line_num);
            ++return_val;
        }
    }

    return return_val;
}

static int semcheck_builtin_dec(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_inc(symtab, stmt, max_scope_lev);
}

static int semcheck_builtin_include_like(SymTab_t *symtab, struct Statement *stmt,
    int max_scope_lev, const char *display_name)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, %s expects exactly two arguments.\n",
            stmt->line_num, display_name);
        return 1;
    }

    int error_count = 0;
    struct Expression *set_expr = (struct Expression *)args->cur;
    int set_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&set_type, symtab, set_expr, max_scope_lev, MUTATE);
    if (set_type != SET_TYPE)
    {
        semcheck_error_with_context("Error on line %d, %s target must be a set.\n",
            stmt->line_num, display_name);
        ++error_count;
    }

    struct Expression *value_expr = (struct Expression *)args->next->cur;
    int value_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);
    if (!is_ordinal_type(value_type))
    {
        semcheck_error_with_context("Error on line %d, %s element must be an ordinal value.\n",
            stmt->line_num, display_name);
        ++error_count;
    }

    return error_count;
}

static int semcheck_builtin_include(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_include_like(symtab, stmt, max_scope_lev, "Include");
}

static int semcheck_builtin_exclude(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_include_like(symtab, stmt, max_scope_lev, "Exclude");
}

/* Initialize(var v) / Finalize(var v) - accept any managed type */
static int semcheck_builtin_initialize_finalize(SymTab_t *symtab, struct Statement *stmt,
    int max_scope_lev, const char *display_name)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, %s expects exactly one argument.\n",
            stmt->line_num, display_name);
        return 1;
    }

    int error_count = 0;
    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&arg_type, symtab, arg_expr, max_scope_lev, MUTATE);
    /* Accept any type - Initialize/Finalize work with all managed types */
    return error_count;
}

static int semcheck_builtin_initialize(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_initialize_finalize(symtab, stmt, max_scope_lev, "Initialize");
}

static int semcheck_builtin_finalize(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_initialize_finalize(symtab, stmt, max_scope_lev, "Finalize");
}

static int semcheck_builtin_assert(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_count = ListLength(args);
    if (arg_count < 1 || arg_count > 2)
    {
        semcheck_error_with_context("Error on line %d, Assert expects 1 or 2 arguments.\n",
            stmt->line_num);
        return 1;
    }

    /* First argument: boolean condition */
    struct Expression *cond_expr = (struct Expression *)args->cur;
    int cond_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&cond_type, symtab, cond_expr, max_scope_lev, NO_MUTATE);

    /* Second argument (optional): string message */
    if (args->next != NULL)
    {
        struct Expression *msg_expr = (struct Expression *)args->next->cur;
        int msg_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&msg_type, symtab, msg_expr, max_scope_lev, NO_MUTATE);
    }

    return return_val;
}

static int semcheck_builtin_write_like(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_index = 1;
    int saw_file_arg = 0;
    while (args != NULL)
    {
        struct Expression *expr = (struct Expression *)args->cur;
        int expr_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&expr_type, symtab, expr, INT_MAX, NO_MUTATE);
        int expr_is_char_array = 0;
        if (expr_type == UNKNOWN_TYPE && expr != NULL && expr->resolved_kgpc_type != NULL)
        {
            KgpcType *expr_type_kgpc = expr->resolved_kgpc_type;
            if (kgpc_type_is_array(expr_type_kgpc))
            {
                KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(expr_type_kgpc, symtab);
                if (elem_type != NULL && elem_type->kind == TYPE_KIND_PRIMITIVE &&
                    elem_type->info.primitive_type_tag == CHAR_TYPE)
                {
                    expr_is_char_array = 1;
                }
            }
        }

        if (!saw_file_arg && expr_type == TEXT_TYPE)
        {
            saw_file_arg = 1;
            args = args->next;
            continue;
        }

        if (!is_integer_type(expr_type) && expr_type != STRING_TYPE && expr_type != SHORTSTRING_TYPE &&
            expr_type != BOOL && expr_type != POINTER_TYPE && expr_type != REAL_TYPE &&
            expr_type != CHAR_TYPE && expr_type != ENUM_TYPE && !expr_is_char_array)
        {
            semcheck_error_with_context("Error on line %d, write argument %d must be integer, longint, real, boolean, string, pointer, or enum.\n",
                    stmt->line_num, arg_index);
            ++return_val;
        }

        if (expr != NULL && expr->field_width != NULL)
        {
            int width_type = UNKNOWN_TYPE;
            return_val += semcheck_stmt_expr_tag(&width_type, symtab, expr->field_width, INT_MAX, NO_MUTATE);
            if (!is_integer_type(width_type))
            {
                semcheck_error_with_context("Error on line %d, field width for argument %d must be an integer.\n",
                        stmt->line_num, arg_index);
                ++return_val;
            }
        }

        if (expr != NULL && expr->field_precision != NULL)
        {
            int precision_type = UNKNOWN_TYPE;
            return_val += semcheck_stmt_expr_tag(&precision_type, symtab, expr->field_precision, INT_MAX, NO_MUTATE);
            if (!is_integer_type(precision_type))
            {
                semcheck_error_with_context("Error on line %d, field precision for argument %d must be an integer.\n",
                        stmt->line_num, arg_index);
                ++return_val;
            }
        }

        args = args->next;
        ++arg_index;
    }

    return return_val;
}

/* WriteStr(var S: string; args...) - format values into a string variable */
static int semcheck_builtin_writestr(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL)
    {
        semcheck_error_with_context("Error on line %d, WriteStr requires at least one argument.\n",
                stmt->line_num);
        return 1;
    }

    /* First argument must be a string variable (var parameter) */
    struct Expression *dest_expr = (struct Expression *)args->cur;
    int dest_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&dest_type, symtab, dest_expr, max_scope_lev, MUTATE);
    
    if (dest_type != STRING_TYPE && dest_type != SHORTSTRING_TYPE)
    {
        semcheck_error_with_context("Error on line %d, WriteStr first argument must be a string variable.\n",
                stmt->line_num);
        ++return_val;
    }

    /* Remaining arguments are values to format */
    args = args->next;
    int arg_index = 2;
    while (args != NULL)
    {
        struct Expression *expr = (struct Expression *)args->cur;
        int expr_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&expr_type, symtab, expr, INT_MAX, NO_MUTATE);

        if (!is_integer_type(expr_type) && expr_type != STRING_TYPE && expr_type != SHORTSTRING_TYPE && 
            expr_type != BOOL && expr_type != POINTER_TYPE && expr_type != REAL_TYPE && 
            expr_type != CHAR_TYPE && expr_type != ENUM_TYPE)
        {
            semcheck_error_with_context("Error on line %d, WriteStr argument %d must be integer, real, boolean, string, pointer, or enum.\n",
                    stmt->line_num, arg_index);
            ++return_val;
        }

        args = args->next;
        ++arg_index;
    }

    return return_val;
}

static int semcheck_builtin_read_like(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_index = 1;
    int saw_file_arg = 0;
    
    while (args != NULL)
    {
        struct Expression *expr = (struct Expression *)args->cur;
        int expr_type = UNKNOWN_TYPE;
        
        /* For read, we need to check if this is a file argument first */
        return_val += semcheck_stmt_expr_tag(&expr_type, symtab, expr, max_scope_lev, NO_MUTATE);
        
        if (!saw_file_arg && expr_type == TEXT_TYPE)
        {
            saw_file_arg = 1;
            args = args->next;
            arg_index++;
            continue;
        }
        
        /* After file arg (if any), remaining args must be mutable lvalues */
        /* Re-check with MUTATE flag to ensure it's an lvalue */
        expr_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&expr_type, symtab, expr, max_scope_lev, MUTATE);
        
        if (!is_integer_type(expr_type) && expr_type != CHAR_TYPE && expr_type != STRING_TYPE && expr_type != REAL_TYPE)
        {
            semcheck_error_with_context("Error on line %d, read argument %d must be integer, longint, real, char, or string variable.\n",
                    stmt->line_num, arg_index);
            ++return_val;
        }
        
        args = args->next;
        ++arg_index;
    }

    return return_val;
}

static int semcheck_builtin_untyped_call(SymTab_t *symtab, struct Statement *stmt,
    int max_scope_lev, int first_arg_mutate)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_index = 0;
    while (args != NULL)
    {
        struct Expression *expr = (struct Expression *)args->cur;
        int expr_type = UNKNOWN_TYPE;
        int mutate_flag = (arg_index == 0 && first_arg_mutate) ? MUTATE : NO_MUTATE;
        return_val += semcheck_stmt_expr_tag(&expr_type, symtab, expr, max_scope_lev, mutate_flag);
        args = args->next;
        ++arg_index;
    }

    return_val += semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
    return return_val;
}

static int semcheck_builtin_assign(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_untyped_call(symtab, stmt, max_scope_lev, 1);
}

static int semcheck_builtin_close(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_untyped_call(symtab, stmt, max_scope_lev, 1);
}

static int semcheck_builtin_settextcodepage(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_untyped_call(symtab, stmt, max_scope_lev, 1);
}

static int semcheck_set_stmt_call_mangled_id(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (symtab == NULL || stmt == NULL)
        return 0;

    const char *proc_id = stmt->stmt_data.procedure_call_data.id;
    if (proc_id == NULL)
        return 0;

    char *mangled = MangleFunctionNameFromCallSite(proc_id,
        stmt->stmt_data.procedure_call_data.expr_args, symtab, max_scope_lev);
    if (mangled == NULL)
    {
        fprintf(stderr, "Error: failed to mangle procedure name for call to %s.\n", proc_id);
        return 1;
    }

    ListNode_t *candidates = FindAllIdents(symtab, proc_id);
    HashNode_t *exact_match = NULL;
    if (candidates != NULL)
    {
        for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL && candidate->mangled_id != NULL &&
                strcmp(candidate->mangled_id, mangled) == 0)
            {
                exact_match = candidate;
                break;
            }
        }
    }
    if (exact_match == NULL && candidates != NULL)
    {
        HashNode_t *wildcard = semcheck_find_untyped_mangled_match(candidates, proc_id, mangled);
        if (wildcard != NULL && wildcard->mangled_id != NULL)
        {
            free(mangled);
            mangled = strdup(wildcard->mangled_id);
            if (mangled == NULL)
            {
                if (candidates != NULL)
                    DestroyList(candidates);
                fprintf(stderr, "Error: failed to allocate mangled procedure name for %s.\n", proc_id);
                return 1;
            }
        }
    }
    if (exact_match == NULL && candidates != NULL)
    {
        int call_arg_count = ListLength(stmt->stmt_data.procedure_call_data.expr_args);
        HashNode_t *best_match = NULL;
        int best_score = 9999;
        int num_best = 0;

        for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate == NULL || candidate->type == NULL ||
                candidate->type->kind != TYPE_KIND_PROCEDURE)
                continue;
            ListNode_t *formal_params = candidate->type->info.proc_info.params;
            if (ListLength(formal_params) != call_arg_count)
                continue;

            ListNode_t *formal = formal_params;
            ListNode_t *actual = stmt->stmt_data.procedure_call_data.expr_args;
            int current_score = 0;
            while (formal != NULL && actual != NULL)
            {
                Tree_t *formal_decl = (Tree_t *)formal->cur;
                struct Expression *actual_expr = (struct Expression *)actual->cur;
                int formal_type = resolve_param_type(formal_decl, symtab);
                int actual_type = UNKNOWN_TYPE;
                semcheck_stmt_expr_tag(&actual_type, symtab, actual_expr, max_scope_lev, NO_MUTATE);

                if (formal_type == UNKNOWN_TYPE || actual_type == UNKNOWN_TYPE)
                    current_score += 0;
                else if (formal_type == actual_type)
                    current_score += 0;
                else if ((formal_type == LONGINT_TYPE && actual_type == INT_TYPE) ||
                         (formal_type == INT_TYPE && actual_type == LONGINT_TYPE))
                    current_score += 1;
                else
                    current_score += 1000;

                formal = formal->next;
                actual = actual->next;
            }

            if (current_score < best_score)
            {
                best_score = current_score;
                best_match = candidate;
                num_best = 1;
            }
            else if (current_score == best_score)
            {
                num_best++;
            }
        }

        if (num_best == 1 && best_match != NULL && best_match->mangled_id != NULL)
        {
            free(mangled);
            mangled = strdup(best_match->mangled_id);
            if (mangled == NULL)
            {
                if (candidates != NULL)
                    DestroyList(candidates);
                fprintf(stderr, "Error: failed to allocate mangled procedure name for %s.\n", proc_id);
                return 1;
            }
        }
    }
    if (candidates != NULL)
        DestroyList(candidates);

    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
    {
        free(stmt->stmt_data.procedure_call_data.mangled_id);
        stmt->stmt_data.procedure_call_data.mangled_id = NULL;
    }
    stmt->stmt_data.procedure_call_data.mangled_id = mangled;
    return 0;
}

static int semcheck_builtin_halt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL)
    {
        struct Expression *zero_expr = mk_inum(stmt->line_num, 0);
        if (zero_expr == NULL)
        {
            semcheck_error_with_context("Error on line %d, failed to allocate Halt argument.\n", stmt->line_num);
            return 1;
        }
        stmt->stmt_data.procedure_call_data.expr_args = CreateListNode(zero_expr, LIST_EXPR);
        return semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
    }

    if (args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Halt expects zero or one argument.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    struct Expression *code_expr = (struct Expression *)args->cur;
    int code_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&code_type, symtab, code_expr, max_scope_lev, NO_MUTATE);
    return_val += semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
    return return_val;
}

static int semcheck_builtin_getmem(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_count = ListLength(args);
    if (arg_count < 1 || arg_count > 2)
    {
        semcheck_error_with_context("Error on line %d, GetMem expects one or two arguments.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    if (arg_count == 1)
    {
        struct Expression *size_expr = (struct Expression *)args->cur;
        int size_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&size_type, symtab, size_expr, max_scope_lev, NO_MUTATE);
        return_val += semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
        return return_val;
    }

    struct Expression *target_expr = (struct Expression *)args->cur;
    struct Expression *size_expr = (struct Expression *)args->next->cur;
    int target_type = UNKNOWN_TYPE;
    int size_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    return_val += semcheck_stmt_expr_tag(&size_type, symtab, size_expr, max_scope_lev, NO_MUTATE);
    return_val += semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
    return return_val;
}

static int semcheck_builtin_freemem(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_count = ListLength(args);
    if (arg_count < 1 || arg_count > 2)
    {
        semcheck_error_with_context("Error on line %d, FreeMem expects one or two arguments.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    if (args != NULL)
    {
        struct Expression *ptr_expr = (struct Expression *)args->cur;
        int ptr_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&ptr_type, symtab, ptr_expr, max_scope_lev, NO_MUTATE);
        if (args->next != NULL)
        {
            struct Expression *size_expr = (struct Expression *)args->next->cur;
            int size_type = UNKNOWN_TYPE;
            return_val += semcheck_stmt_expr_tag(&size_type, symtab, size_expr, max_scope_lev, NO_MUTATE);
        }
    }

    return_val += semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
    return return_val;
}

static int semcheck_builtin_move(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next == NULL || args->next->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Move expects exactly three arguments.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    struct Expression *src_expr = (struct Expression *)args->cur;
    struct Expression *dst_expr = (struct Expression *)args->next->cur;
    struct Expression *count_expr = (struct Expression *)args->next->next->cur;
    int src_type = UNKNOWN_TYPE;
    int dst_type = UNKNOWN_TYPE;
    int count_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&src_type, symtab, src_expr, max_scope_lev, NO_MUTATE);
    return_val += semcheck_stmt_expr_tag(&dst_type, symtab, dst_expr, max_scope_lev, MUTATE);
    return_val += semcheck_stmt_expr_tag(&count_type, symtab, count_expr, max_scope_lev, NO_MUTATE);
    return return_val;
}

static int semcheck_builtin_reallocmem(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, ReallocMem expects exactly two arguments.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    struct Expression *target_expr = (struct Expression *)args->cur;
    struct Expression *size_expr = (struct Expression *)args->next->cur;
    int target_type = UNKNOWN_TYPE;
    int size_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&target_type, symtab, target_expr, max_scope_lev, NO_MUTATE);
    return_val += semcheck_stmt_expr_tag(&size_type, symtab, size_expr, max_scope_lev, NO_MUTATE);
    return return_val;
}

static int semcheck_builtin_setcodepage(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_untyped_call(symtab, stmt, max_scope_lev, 0);
}

static int semcheck_builtin_interlockedexchangeadd(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, InterlockedExchangeAdd expects exactly two arguments.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    struct Expression *target_expr = (struct Expression *)args->cur;
    struct Expression *value_expr = (struct Expression *)args->next->cur;
    int target_type = UNKNOWN_TYPE;
    int value_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&target_type, symtab, target_expr, max_scope_lev, NO_MUTATE);
    return_val += semcheck_stmt_expr_tag(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);
    return return_val;
}

static int semcheck_builtin_new(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, New expects exactly one argument.\\n", stmt->line_num);
        return 1;
    }

    struct Expression *target_expr = (struct Expression *)args->cur;
    int pointer_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&pointer_type, symtab, target_expr, max_scope_lev, MUTATE);

    if (pointer_type != POINTER_TYPE)
    {
        semcheck_error_with_context("Error on line %d, New expects a pointer variable argument.\\n", stmt->line_num);
        return ++return_val;
    }

    if (target_expr->pointer_subtype == UNKNOWN_TYPE && target_expr->pointer_subtype_id == NULL)
    {
        semcheck_error_with_context("Error on line %d, unable to determine allocation type for New.\\n", stmt->line_num);
        return ++return_val;
    }

    return return_val;
}

static int semcheck_builtin_dispose(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Dispose expects exactly one argument.\\n", stmt->line_num);
        return 1;
    }

    struct Expression *target_expr = (struct Expression *)args->cur;
    int pointer_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&pointer_type, symtab, target_expr, max_scope_lev, MUTATE);

    if (pointer_type != POINTER_TYPE)
    {
        semcheck_error_with_context("Error on line %d, Dispose expects a pointer variable argument.\\n", stmt->line_num);
        return ++return_val;
    }

    return return_val;
}

/* Semantic check on a normal statement */
int semcheck_stmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_stmt_main(symtab, stmt, max_scope_lev);
}

/* Semantic check on a function statement (no side effects allowed) */
int semcheck_func_stmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_stmt_main(symtab, stmt, max_scope_lev);
}

static int semcheck_break_stmt(struct Statement *stmt)
{
    if (semcheck_loop_depth <= 0)
    {
        if (stmt != NULL)
            semcheck_error_with_context("Error on line %d, Break is only valid inside a loop.\n", stmt->line_num);
        return 1;
    }
    return 0;
}

static int semcheck_continue_stmt(struct Statement *stmt)
{
    if (semcheck_loop_depth <= 0)
    {
        if (stmt != NULL)
            semcheck_error_with_context("Error on line %d, Continue is only valid inside a loop.\n", stmt->line_num);
        return 1;
    }
    return 0;
}


/* Main semantic checking */

int semcheck_stmt_main(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;

    assert(symtab != NULL);
    if (stmt == NULL)
        return 0;

    static long semcheck_stmt_counter = 0;
    static long semcheck_stmt_limit = -1;
    static int semcheck_stmt_limit_inited = 0;
    static int semcheck_stmt_log_enabled = -1;
    static int semcheck_stmt_verbose = -1;
    if (!semcheck_stmt_limit_inited) {
        const char *limit_env = getenv("KGPC_DEBUG_SEMSTMT_LIMIT");
        if (limit_env != NULL)
            semcheck_stmt_limit = atol(limit_env);
        semcheck_stmt_limit_inited = 1;
    }
    if (semcheck_stmt_log_enabled == -1) {
        semcheck_stmt_log_enabled = getenv("KGPC_DEBUG_SEMSTMT") != NULL;
    }
    if (semcheck_stmt_verbose == -1) {
        semcheck_stmt_verbose = getenv("KGPC_DEBUG_SEMSTMT_VERBOSE") != NULL;
    }
    semcheck_stmt_counter++;
    if (semcheck_stmt_verbose) {
        fprintf(stderr, "[semcheck_stmt] enter type=%d line=%d col=%d\n",
                stmt->type, stmt->line_num, stmt->col_num);
    }
    if (semcheck_stmt_log_enabled && (semcheck_stmt_counter % 10000) == 0) {
        fprintf(stderr, "[semcheck_stmt] count=%ld last_type=%d line=%d\n",
                semcheck_stmt_counter, stmt->type, stmt->line_num);
    }
    if (semcheck_stmt_limit > 0 && semcheck_stmt_counter > semcheck_stmt_limit) {
        fprintf(stderr, "ERROR: semcheck_stmt exceeded limit (%ld) at type=%d line=%d.\n",
                semcheck_stmt_limit, stmt->type, stmt->line_num);
        return 1;
    }

    semcheck_set_error_context(stmt->line_num, stmt->col_num, stmt->source_index);
    
    // In semcheck_for:
    // semcheck_loop_depth++;
    // 
    // fprintf(stderr, "DEBUG: semcheck_for stmt=%p line=%d to_expr=%p current_to=%p\n", 
    //         stmt, stmt->line_num, to_expr, stmt->stmt_data.for_data.to);
    // 
    // if (stmt->line_num == 42) {
    //     watch_stmt = stmt;
    //     watch_to_expr = stmt->stmt_data.for_data.to;
    //     fprintf(stderr, "DEBUG: Watching stmt at line 42\n");
    // }
    // 
    // if (to_expr != NULL && ((uintptr_t)to_expr == 0x686374616d || (uintptr_t)to_expr == 0x1db2)) {
    //     fprintf(stderr, "CRITICAL: to_expr is corrupted in semcheck_for!\n");
    // }
    // 
    // return_val += semcheck_stmt_main(symtab, do_for, max_scope_lev);
    // semcheck_loop_depth--;
    // 
    // if (stmt->stmt_data.for_data.to != to_expr) {
    //     fprintf(stderr, "CRITICAL: stmt->stmt_data.for_data.to changed from %p to %p during body processing!\n",
    //             to_expr, stmt->stmt_data.for_data.to);
    // }
    // 
    // if (watch_stmt == stmt) {
    //     // We are returning from the watched statement.
    //     // It might be checked again in outer loops, but that's fine.
    // }

    return_val = 0;
    switch(stmt->type)
    {
        case STMT_VAR_ASSIGN:
            return_val += semcheck_varassign(symtab, stmt, max_scope_lev);
            break;

        case STMT_PROCEDURE_CALL:
            return_val += semcheck_proccall(symtab, stmt, max_scope_lev);
            break;

        case STMT_EXPR:
            if (stmt->stmt_data.expr_stmt_data.expr != NULL)
            {
                int expr_type;
                return_val += semcheck_stmt_expr_tag(&expr_type, symtab,
                    stmt->stmt_data.expr_stmt_data.expr, max_scope_lev, 0);
            }
            break;

        case STMT_COMPOUND_STATEMENT:
            return_val += semcheck_compoundstmt(symtab, stmt, max_scope_lev);
            break;

        case STMT_LABEL:
            if (stmt->stmt_data.label_data.stmt != NULL)
                return_val += semcheck_stmt_main(symtab, stmt->stmt_data.label_data.stmt, max_scope_lev);
            break;

        case STMT_GOTO:
            /* TODO: Validate that the target label exists within scope */
            break;

        case STMT_IF_THEN:
            return_val += semcheck_ifthen(symtab, stmt, max_scope_lev);
            break;

        case STMT_WHILE:
            return_val += semcheck_while(symtab, stmt, max_scope_lev);
            break;

        case STMT_REPEAT:
            return_val += semcheck_repeat(symtab, stmt, max_scope_lev);
            break;

        case STMT_FOR:
            return_val += semcheck_for(symtab, stmt, max_scope_lev);
            break;

        case STMT_FOR_IN:
            return_val += semcheck_for_in(symtab, stmt, max_scope_lev);
            break;

        case STMT_BREAK:
            return_val = semcheck_break_stmt(stmt);
            break;
        case STMT_CONTINUE:
            return_val = semcheck_continue_stmt(stmt);
            break;

        case STMT_ASM_BLOCK:
            /* No semantic checking needed for asm blocks */
            break;

        case STMT_EXIT:
            /* Exit statement with optional return expression */
            {
                struct Expression *return_expr = stmt->stmt_data.exit_data.return_expr;
                if (return_expr != NULL)
                {
                    /* Type-check the return expression */
                    int expr_type;
                    return_val += semcheck_stmt_expr_tag(&expr_type, symtab, return_expr, max_scope_lev, 0);
                    
                    /* Mark Result as assigned if we're in a function context */
                    HashNode_t *result_node = NULL;
                    if (FindIdent(&result_node, symtab, "Result") == 0 && result_node != NULL)
                    {
                        result_node->mutated = MUTATE;
                        if (result_node->type != NULL && return_expr->resolved_kgpc_type != NULL)
                        {
                            if (!are_types_compatible_for_assignment(result_node->type,
                                                                    return_expr->resolved_kgpc_type, symtab))
                            {
                                semcheck_error_with_context("Error on line %d, incompatible return type in exit().\n",
                                    stmt->line_num);
                                ++return_val;
                            }
                        }
                    }
                }
            }
            break;

        case STMT_CASE:
            /* Check the selector expression */
            {
                int selector_type;
                return_val += semcheck_stmt_expr_tag(&selector_type, symtab, stmt->stmt_data.case_data.selector_expr, max_scope_lev, 0);
            }
            
            /* Check each case branch */
            {
                ListNode_t *branch_node = stmt->stmt_data.case_data.branches;
                while (branch_node != NULL) {
                    struct CaseBranch *branch = (struct CaseBranch *)branch_node->cur;
                    if (branch != NULL) {
                        /* Check case labels */
                        ListNode_t *label_node = branch->labels;
                        while (label_node != NULL) {
                            if (label_node->type == LIST_EXPR) {
                                struct Expression *label_expr = (struct Expression *)label_node->cur;
                                int label_type;
                                return_val += semcheck_stmt_expr_tag(&label_type, symtab, label_expr, max_scope_lev, 0);
                            } else if (label_node->type == LIST_SET_ELEMENT) {
                                struct SetElement *range = (struct SetElement *)label_node->cur;
                                if (range != NULL) {
                                    if (range->lower != NULL) {
                                        int lower_type;
                                        return_val += semcheck_stmt_expr_tag(&lower_type, symtab, range->lower, max_scope_lev, 0);
                                    }
                                    if (range->upper != NULL) {
                                        int upper_type;
                                        return_val += semcheck_stmt_expr_tag(&upper_type, symtab, range->upper, max_scope_lev, 0);
                                    }
                                }
                            }
                            label_node = label_node->next;
                        }
                        /* Check the branch statement */
                        if (branch->stmt != NULL)
                            return_val += semcheck_stmt(symtab, branch->stmt, max_scope_lev);
                    }
                    branch_node = branch_node->next;
                }
            }
            
            /* Check the else statement if present */
            if (stmt->stmt_data.case_data.else_stmt != NULL)
                return_val += semcheck_stmt(symtab, stmt->stmt_data.case_data.else_stmt, max_scope_lev);
            break;

        case STMT_WITH:
        {
            struct Expression *context_expr = stmt->stmt_data.with_data.context_expr;
            struct Statement *body_stmt = stmt->stmt_data.with_data.body_stmt;
            struct RecordType *record_info = NULL;
            int ctx_type = UNKNOWN_TYPE;
            int pushed = 0;

            if (context_expr == NULL)
            {
                semcheck_error_with_context("Error on line %d, WITH statement requires a context expression.\\n\\n",
                    stmt->line_num);
                ++return_val;
            }
            else
            {
                return_val += semcheck_stmt_expr_tag(&ctx_type, symtab, context_expr, max_scope_lev, NO_MUTATE);
                record_info = semcheck_with_resolve_record_type(symtab, context_expr, ctx_type, stmt->line_num);
                if (record_info == NULL)
                {
                    fprintf(stderr,
                        "Error on line %d, WITH context must be a record or pointer to a record.\\n\\n",
                        stmt->line_num);
                    ++return_val;
                }
                else
                {
                    if (context_expr->resolved_kgpc_type == NULL)
                        context_expr->resolved_kgpc_type = create_record_type(record_info);
                    if (semcheck_with_push(context_expr, record_info) != 0)
                    {
                        ++return_val;
                    }
                    else
                    {
                        pushed = 1;
                    }
                }
            }

            if (body_stmt != NULL)
                return_val += semcheck_stmt_main(symtab, body_stmt, max_scope_lev);

            if (pushed)
                semcheck_with_pop();
            break;
        }

        case STMT_TRY_FINALLY:
            return_val += semcheck_statement_list_nodes(symtab, stmt->stmt_data.try_finally_data.try_statements, max_scope_lev);
            return_val += semcheck_statement_list_nodes(symtab, stmt->stmt_data.try_finally_data.finally_statements, max_scope_lev);
            break;

        case STMT_TRY_EXCEPT:
            return_val += semcheck_statement_list_nodes(symtab, stmt->stmt_data.try_except_data.try_statements, max_scope_lev);
            
            /* If there's an 'on E: Exception do' clause, create a local scope for the exception variable */
            if (stmt->stmt_data.try_except_data.has_on_clause && 
                stmt->stmt_data.try_except_data.exception_var_name != NULL) {
                
                /* Push a new scope for the exception variable */
                PushScope(symtab);
                
                /* Add the exception variable to the symbol table */
                char *var_name = stmt->stmt_data.try_except_data.exception_var_name;
                char *type_name = stmt->stmt_data.try_except_data.exception_type_name;
                
                /* Look up the type if specified, otherwise use NULL (defaults to integer) */
                KgpcType *var_kgpc_type = NULL;
                if (type_name != NULL) {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, type_name) >= 0 && type_node != NULL) {
                        if (type_node->hash_type == HASHTYPE_TYPE) {
                            var_kgpc_type = type_node->type;
                        } else {
                            fprintf(stderr, "Error: '%s' is not a type at line %d\n", 
                                    type_name, stmt->line_num);
                            return_val++;
                        }
                    } else {
                        fprintf(stderr, "Error: Unknown exception type '%s' at line %d\n", 
                                type_name, stmt->line_num);
                        return_val++;
                    }
                }
                
                /* Push the exception variable onto the scope (NULL type defaults to integer) */
                PushVarOntoScope_Typed(symtab, var_name, var_kgpc_type);
                
                /* Semantic check the except statements in the new scope */
                return_val += semcheck_statement_list_nodes(symtab, stmt->stmt_data.try_except_data.except_statements, max_scope_lev);
                
                /* Pop the scope */
                PopScope(symtab);
            } else {
                /* No exception variable - just check the except statements normally */
                return_val += semcheck_statement_list_nodes(symtab, stmt->stmt_data.try_except_data.except_statements, max_scope_lev);
            }
            break;

        case STMT_RAISE:
            if (stmt->stmt_data.raise_data.exception_expr != NULL)
            {
                int raise_type = UNKNOWN_TYPE;
                return_val += semcheck_stmt_expr_tag(&raise_type, symtab, stmt->stmt_data.raise_data.exception_expr, INT_MAX, NO_MUTATE);
            }
            break;

        case STMT_INHERITED:
            if (stmt->stmt_data.inherited_data.call_expr != NULL)
            {
                struct Expression *call_expr = stmt->stmt_data.inherited_data.call_expr;
                if (getenv("KGPC_DEBUG_INHERITED") != NULL)
                {
                    const char *cid = NULL;
                    if (call_expr->type == EXPR_FUNCTION_CALL)
                        cid = call_expr->expr_data.function_call_data.id;
                    else if (call_expr->type == EXPR_VAR_ID)
                        cid = call_expr->expr_data.id;
                    fprintf(stderr, "[INHERITED] stmt line=%d call=%s\n",
                        stmt->line_num, cid ? cid : "<null>");
                }
                
                /* Handle EXPR_VAR_ID by converting to EXPR_FUNCTION_CALL */
                if (call_expr->type == EXPR_VAR_ID)
                {
                    /* Save the id from the VAR_ID before converting */
                    char *var_id = call_expr->expr_data.id;

                    /* Convert to EXPR_FUNCTION_CALL */
                    call_expr->type = EXPR_FUNCTION_CALL;
                    memset(&call_expr->expr_data.function_call_data, 0, sizeof(call_expr->expr_data.function_call_data));
                    call_expr->expr_data.function_call_data.id = var_id;
                    call_expr->expr_data.function_call_data.args_expr = NULL;
                    call_expr->expr_data.function_call_data.mangled_id = NULL;
                    call_expr->expr_data.function_call_data.resolved_func = NULL;
                    call_expr->expr_data.function_call_data.call_hash_type = 0;
                    call_expr->expr_data.function_call_data.call_kgpc_type = NULL;
                    call_expr->expr_data.function_call_data.is_call_info_valid = 0;
                }
                
                if (call_expr->type == EXPR_FUNCTION_CALL)
                {
                    if (1)
                    {
                        /* For inherited procedure calls, check if we need to handle Create/Destroy with no parent */
                        const char *method_name = call_expr->expr_data.function_call_data.id;
                        char *method_name_buf = NULL;
                        char *owner_name_buf = NULL;
                        if (method_name != NULL)
                        {
                            const char *sep = strstr(method_name, "__");
                            if (sep != NULL)
                            {
                                size_t owner_len = (size_t)(sep - method_name);
                                size_t meth_len = strlen(sep + 2);
                                owner_name_buf = (char *)malloc(owner_len + 1);
                                method_name_buf = (char *)malloc(meth_len + 1);
                                if (owner_name_buf != NULL && method_name_buf != NULL)
                                {
                                    memcpy(owner_name_buf, method_name, owner_len);
                                    owner_name_buf[owner_len] = '\0';
                                    memcpy(method_name_buf, sep + 2, meth_len + 1);
                                    method_name = method_name_buf;
                                }
                            }
                        }
                        HashNode_t *self_node = NULL;
                        const char *parent_class_name = NULL;
                        struct RecordType *current_class = NULL;

                        if (FindIdent(&self_node, symtab, "Self") != -1 && self_node != NULL &&
                            self_node->type != NULL)
                        {
                            /* Handle both direct records and pointers to records (classes) */
                            if (self_node->type->kind == TYPE_KIND_RECORD &&
                                self_node->type->info.record_info != NULL)
                            {
                                current_class = self_node->type->info.record_info;
                            }
                            else if (self_node->type->kind == TYPE_KIND_POINTER &&
                                     self_node->type->info.points_to != NULL &&
                                     self_node->type->info.points_to->kind == TYPE_KIND_RECORD &&
                                     self_node->type->info.points_to->info.record_info != NULL)
                            {
                                    current_class = self_node->type->info.points_to->info.record_info;
                            }

                            if (current_class != NULL)
                            {
                                parent_class_name = current_class->parent_class_name;
                                if (getenv("KGPC_DEBUG_INHERITED") != NULL && method_name != NULL &&
                                    strcasecmp(method_name, "Create") == 0)
                                {
                                    fprintf(stderr,
                                        "[INHERITED] class=%s parent=%s\n",
                                        current_class->type_id ? current_class->type_id : "<null>",
                                        parent_class_name ? parent_class_name : "<null>");
                                }

                                /* Check if there's no parent class and this is Create or Destroy */
                                if (current_class->parent_class_name == NULL && method_name != NULL &&
                                    (strcasecmp(method_name, "Create") == 0 || strcasecmp(method_name, "Destroy") == 0))
                                {
                                    /* No parent class - convert to empty compound statement (no-op) */
                                    if (getenv("KGPC_DEBUG_INHERITED") != NULL)
                                    {
                                        fprintf(stderr, "[KGPC] Inherited %s with no parent class - converting to no-op\n",
                                                method_name);
                                    }
                                    /* Convert this inherited statement to an empty compound statement */
                                    stmt->type = STMT_COMPOUND_STATEMENT;
                                    stmt->stmt_data.compound_statement = NULL;
                                    /* No errors */
                                    break;
                                }
                            }
                        }
                        if (current_class == NULL)
                        {
                            const char *owner_id = semcheck_get_current_method_owner();
                            if (owner_id == NULL && owner_name_buf != NULL)
                                owner_id = owner_name_buf;
                            if (owner_id != NULL)
                            {
                                HashNode_t *owner_node = NULL;
                                if (FindIdent(&owner_node, symtab, owner_id) != -1 && owner_node != NULL)
                                    current_class = semcheck_stmt_get_record_type_from_node(owner_node);
                                if (current_class != NULL)
                                    parent_class_name = current_class->parent_class_name;
                            }
                        }
                        if (getenv("KGPC_DEBUG_INHERITED") != NULL && method_name != NULL &&
                            strcasecmp(method_name, "Create") == 0)
                        {
                            fprintf(stderr,
                                "[INHERITED] resolved class=%s parent=%s\n",
                                current_class && current_class->type_id ? current_class->type_id : "<null>",
                                parent_class_name ? parent_class_name : "<null>");
                        }
                        if (owner_name_buf != NULL)
                            free(owner_name_buf);
                        if (method_name_buf != NULL)
                            free(method_name_buf);

                        /* If a parent exists, call the parent class method */
                        HashNode_t *parent_method_node = NULL;
                        char parent_mangled[512];
                        parent_mangled[0] = '\0';
                        if (parent_class_name != NULL && method_name != NULL)
                        {
                            const char *search_parent = parent_class_name;
                            while (search_parent != NULL && parent_method_node == NULL)
                            {
                                snprintf(parent_mangled, sizeof(parent_mangled), "%s__%s",
                                    search_parent, method_name);

                                /* Prefer overload resolution by call-site signature */
                                ListNode_t *parent_candidates = FindAllIdents(symtab, parent_mangled);
                                if (parent_candidates != NULL)
                                {
                                    /* Build temp args including Self to match method signatures */
                                    struct Expression *self_expr = mk_varid(stmt->line_num, strdup("Self"));
                                    ListNode_t *self_arg = CreateListNode(self_expr, LIST_EXPR);
                                    self_arg->next = call_expr->expr_data.function_call_data.args_expr;

                                    char *call_mangled = MangleFunctionNameFromCallSite(parent_mangled,
                                        self_arg, symtab, INT_MAX);
                                    if (call_mangled != NULL)
                                    {
                                        for (ListNode_t *cur = parent_candidates; cur != NULL; cur = cur->next)
                                        {
                                            HashNode_t *candidate = (HashNode_t *)cur->cur;
                                            if (candidate != NULL && candidate->mangled_id != NULL &&
                                                strcmp(candidate->mangled_id, call_mangled) == 0)
                                            {
                                                parent_method_node = candidate;
                                                break;
                                            }
                                        }
                                        free(call_mangled);
                                    }

                                    self_arg->next = NULL;
                                    destroy_expr(self_expr);
                                    free(self_arg);

                                    DestroyList(parent_candidates);
                                }
                                else
                                {
                                    if (FindIdent(&parent_method_node, symtab, parent_mangled) == -1)
                                        parent_method_node = NULL;
                                }

                                if (parent_method_node == NULL)
                                {
                                    HashNode_t *parent_node = NULL;
                                    if (FindIdent(&parent_node, symtab, (char *)search_parent) != -1 &&
                                        parent_node != NULL)
                                    {
                                        struct RecordType *parent_record =
                                            semcheck_stmt_get_record_type_from_node(parent_node);
                                        search_parent = parent_record ? parent_record->parent_class_name : NULL;
                                    }
                                    else
                                    {
                                        search_parent = NULL;
                                    }
                                }
                            }

                            if (parent_method_node == NULL)
                            {
                                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                                    "Error on line %d, inherited call to %s has no matching overload.\n\n",
                                    stmt->line_num,
                                    parent_mangled[0] != '\0' ? parent_mangled :
                                        (method_name != NULL ? method_name : "(unknown)"));
                                return ++return_val;
                            }

                            if (getenv("KGPC_DEBUG_INHERITED") != NULL)
                            {
                                fprintf(stderr, "[INHERITED] Looking for parent method: %s, found: %s\n",
                                    parent_mangled, parent_method_node != NULL ? "YES" : "NO");
                                if (parent_method_node != NULL)
                                {
                                    fprintf(stderr, "[INHERITED] Parent method mangled_id: %s\n",
                                        parent_method_node->mangled_id ? parent_method_node->mangled_id : "(null)");
                                    fprintf(stderr, "[INHERITED] Parent method id: %s\n",
                                        parent_method_node->id ? parent_method_node->id : "(null)");
                                }
                            }
                        }

                        /* Create temporary argument list for inherited calls without modifying original AST */
                        ListNode_t *temp_args = NULL;
                        ListNode_t *temp_self_arg = NULL;

                        if (parent_method_node != NULL)
                        {
                            /* Only prepend Self if parent method was found */
                            struct Expression *self_expr = mk_varid(stmt->line_num, strdup("Self"));
                            temp_self_arg = CreateListNode(self_expr, LIST_EXPR);
                            temp_self_arg->next = call_expr->expr_data.function_call_data.args_expr;
                            temp_args = temp_self_arg;
                        }
                        else
                        {
                            /* Use original arguments for non-inherited calls */
                            temp_args = call_expr->expr_data.function_call_data.args_expr;
                        }

                        struct Statement temp_call;
                        int temp_call_id_owned = 0;
                        memset(&temp_call, 0, sizeof(temp_call));
                        temp_call.type = STMT_PROCEDURE_CALL;
                        temp_call.line_num = stmt->line_num;
                        /* For inherited calls, use the parent method's id as the procedure ID
                         * for symbol table lookup, and set mangled_id to prevent re-mangling.
                         * IMPORTANT: always duplicate mangled_id, because semcheck_proccall may
                         * replace/free stmt-owned call names. Borrowing symbol-table storage here
                         * risks corrupting method identifiers globally. */
                        if (parent_method_node != NULL && parent_method_node->id != NULL)
                        {
                            temp_call.stmt_data.procedure_call_data.id = strdup(parent_method_node->id);
                            if (temp_call.stmt_data.procedure_call_data.id == NULL)
                            {
                                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                                    "Error on line %d, out of memory while resolving inherited call.\n\n",
                                    stmt->line_num);
                                return ++return_val;
                            }
                            temp_call_id_owned = 1;
                            /* Pre-set mangled_id to prevent type-based method correction and re-mangling. */
                            if (parent_method_node->mangled_id != NULL)
                                temp_call.stmt_data.procedure_call_data.mangled_id =
                                    strdup(parent_method_node->mangled_id);
                            else
                                temp_call.stmt_data.procedure_call_data.mangled_id =
                                    strdup(parent_method_node->id);
                            if (temp_call.stmt_data.procedure_call_data.mangled_id == NULL)
                            {
                                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                                    "Error on line %d, out of memory while resolving inherited call.\n\n",
                                    stmt->line_num);
                                return ++return_val;
                            }
                        }
                        else
                        {
                            temp_call.stmt_data.procedure_call_data.id = call_expr->expr_data.function_call_data.id;
                            temp_call.stmt_data.procedure_call_data.mangled_id = NULL;
                        }
                        temp_call.stmt_data.procedure_call_data.expr_args = temp_args;
                        temp_call.stmt_data.procedure_call_data.resolved_proc = NULL;

                        if (parent_method_node != NULL && call_expr != NULL &&
                            call_expr->type == EXPR_FUNCTION_CALL)
                        {
                            call_expr->expr_data.function_call_data.args_expr = temp_args;
                            if (parent_method_node->id != NULL)
                            {
                                if (call_expr->expr_data.function_call_data.id != NULL)
                                    free(call_expr->expr_data.function_call_data.id);
                                call_expr->expr_data.function_call_data.id = strdup(parent_method_node->id);
                            }
                            if (parent_method_node->mangled_id != NULL)
                            {
                                if (call_expr->expr_data.function_call_data.mangled_id != NULL)
                                    free(call_expr->expr_data.function_call_data.mangled_id);
                                call_expr->expr_data.function_call_data.mangled_id =
                                    strdup(parent_method_node->mangled_id);
                            }
                        }

                        return_val += semcheck_proccall(symtab, &temp_call, max_scope_lev);

                        if (temp_call_id_owned && temp_call.stmt_data.procedure_call_data.id != NULL)
                        {
                            free(temp_call.stmt_data.procedure_call_data.id);
                            temp_call.stmt_data.procedure_call_data.id = NULL;
                        }

                        /* Clean up temporary argument node if we created one */
                        if (temp_self_arg != NULL)
                        {
                            temp_self_arg->next = NULL;  /* Detach to avoid double-free */
                            /* Note: self_expr will be cleaned up with the statement tree */
                        }

                        if (temp_call.stmt_data.procedure_call_data.mangled_id != NULL)
                        {
                            if (call_expr->expr_data.function_call_data.mangled_id != NULL)
                            {
                                free(call_expr->expr_data.function_call_data.mangled_id);
                                call_expr->expr_data.function_call_data.mangled_id = NULL;
                            }
                            call_expr->expr_data.function_call_data.mangled_id = temp_call.stmt_data.procedure_call_data.mangled_id;
                            temp_call.stmt_data.procedure_call_data.mangled_id = NULL;
                        }
                        call_expr->expr_data.function_call_data.call_hash_type =
                            temp_call.stmt_data.procedure_call_data.call_hash_type;
                        if (call_expr->expr_data.function_call_data.call_kgpc_type != NULL)
                        {
                            destroy_kgpc_type(call_expr->expr_data.function_call_data.call_kgpc_type);
                            call_expr->expr_data.function_call_data.call_kgpc_type = NULL;
                        }
                        if (temp_call.stmt_data.procedure_call_data.call_kgpc_type != NULL)
                        {
                            kgpc_type_retain(temp_call.stmt_data.procedure_call_data.call_kgpc_type);
                            call_expr->expr_data.function_call_data.call_kgpc_type =
                                temp_call.stmt_data.procedure_call_data.call_kgpc_type;
                        }
                        call_expr->expr_data.function_call_data.is_call_info_valid =
                            temp_call.stmt_data.procedure_call_data.is_call_info_valid;
                        semcheck_stmt_set_call_kgpc_type(&temp_call, NULL,
                            temp_call.stmt_data.procedure_call_data.is_call_info_valid == 1);
                        temp_call.stmt_data.procedure_call_data.is_call_info_valid = 0;
                    }
                }
                else
                {
                    /* For other expression types, use general expression checking */
                    int expr_type = UNKNOWN_TYPE;
                    return_val += semcheck_stmt_expr_tag(&expr_type, symtab, call_expr, max_scope_lev, NO_MUTATE);
                }
            }
            break;

        default:
            assert(0 && "Bad type in semcheck_stmt!");
            break;
    }

    return return_val;
}


/****** STMT SEMCHECKS *******/

/** VAR_ASSIGN **/
int semcheck_varassign(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;
    int type_first, type_second;
    struct Expression *var, *expr;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_VAR_ASSIGN);

    return_val = 0;

    int module_property_result = semcheck_try_module_property_assignment(symtab, stmt, max_scope_lev);
    if (module_property_result >= 0)
        return module_property_result;

    var = stmt->stmt_data.var_assign_data.var;
    expr = stmt->stmt_data.var_assign_data.expr;

    rewrite_tfpglist_constructor_if_needed(symtab, max_scope_lev, var,
        &stmt->stmt_data.var_assign_data.expr);
    expr = stmt->stmt_data.var_assign_data.expr;

    /* NOTE: Grammar will make sure the left side is a variable */
    /* Left side var assigns must abide by scoping rules */
    if (SEMSTMT_TIMINGS_ENABLED()) {
        double t0 = semstmt_now_ms();
        return_val += semcheck_stmt_expr_tag(&type_first, symtab, var, max_scope_lev, MUTATE);
        fprintf(stderr, "[timing] varassign lhs semcheck_stmt_expr_tag: %.2f ms (line=%d)\n",
                semstmt_now_ms() - t0, stmt->line_num);
    } else {
        return_val += semcheck_stmt_expr_tag(&type_first, symtab, var, max_scope_lev, MUTATE);
    }

    /* Check for record property assignment early, before RHS type checking.
     * This handles plain record (Delphi advanced record) properties with setter methods.
     * Must happen after LHS is evaluated but before type compatibility checks. */
    {
        int property_result = semcheck_try_property_assignment(symtab, stmt, max_scope_lev);
        if (property_result >= 0)
            return return_val + property_result;
    }

    if (expr != NULL && expr->type == EXPR_RECORD_CONSTRUCTOR &&
        (expr->resolved_kgpc_type == NULL ||
         !kgpc_type_is_record(expr->resolved_kgpc_type)))
    {
        struct RecordType *record_type = NULL;
        if (var != NULL && var->resolved_kgpc_type != NULL)
        {
            KgpcType *lhs_type = var->resolved_kgpc_type;
            if (kgpc_type_is_record(lhs_type))
                record_type = kgpc_type_get_record(lhs_type);
            else if (kgpc_type_is_pointer(lhs_type) && lhs_type->info.points_to != NULL &&
                kgpc_type_is_record(lhs_type->info.points_to))
                record_type = kgpc_type_get_record(lhs_type->info.points_to);
        }
        if (record_type == NULL && var != NULL && var->type == EXPR_VAR_ID &&
            var->expr_data.id != NULL)
        {
            HashNode_t *var_node = NULL;
            if (FindIdent(&var_node, symtab, var->expr_data.id) >= 0 && var_node != NULL)
            {
                record_type = hashnode_get_record_type(var_node);
                if (record_type == NULL)
                {
                    struct TypeAlias *alias = hashnode_get_type_alias(var_node);
                    if (alias != NULL && alias->target_type_id != NULL)
                    {
                        HashNode_t *target_node = NULL;
                        if (FindIdent(&target_node, symtab, alias->target_type_id) >= 0 &&
                            target_node != NULL)
                            record_type = hashnode_get_record_type(target_node);
                    }
                }
            }
        }
        if (record_type != NULL)
        {
            /* Preserve inferred constructor target record explicitly so later
             * expression passes do not depend on transient KgpcType state. */
            expr->record_type = record_type;
            if (expr->resolved_kgpc_type != NULL &&
                !kgpc_type_is_record(expr->resolved_kgpc_type))
            {
                destroy_kgpc_type(expr->resolved_kgpc_type);
                expr->resolved_kgpc_type = NULL;
            }
            if (expr->resolved_kgpc_type == NULL)
                expr->resolved_kgpc_type = create_record_type(record_type);
        }
    }
    if (SEMSTMT_TIMINGS_ENABLED()) {
        double t0 = semstmt_now_ms();
        return_val += semcheck_stmt_expr_tag(&type_second, symtab, expr, INT_MAX, NO_MUTATE);
        fprintf(stderr, "[timing] varassign rhs semcheck_stmt_expr_tag: %.2f ms (line=%d)\n",
                semstmt_now_ms() - t0, stmt->line_num);
    } else {
        return_val += semcheck_stmt_expr_tag(&type_second, symtab, expr, INT_MAX, NO_MUTATE);
    }

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr != NULL && expr->type == EXPR_FUNCTION_CALL &&
        expr->expr_data.function_call_data.id != NULL &&
        strcasecmp(expr->expr_data.function_call_data.id, "Create") == 0) {
        fprintf(stderr, "[SemCheck] semcheck_varassign calling semcheck_resolve_expression_kgpc_type:\n");
        fprintf(stderr, "[SemCheck]   expr=%p type=%d\n", (void*)expr, expr->type);
        fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type=%p\n", (void*)expr->resolved_kgpc_type);
    }
    
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr != NULL && expr->type == EXPR_RECORD_ACCESS) {
        fprintf(stderr, "[SemCheck] semcheck_varassign: expr is EXPR_RECORD_ACCESS\n");
        fprintf(stderr, "[SemCheck]   expr=%p\n", (void*)expr);
        fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type=%p\n", (void*)expr->resolved_kgpc_type);
    }

    int lhs_owned = 0, rhs_owned = 0;
    KgpcType *lhs_kgpctype = NULL;
    KgpcType *rhs_kgpctype = NULL;
    if (SEMSTMT_TIMINGS_ENABLED()) {
        double t0 = semstmt_now_ms();
        lhs_kgpctype = semcheck_resolve_expression_kgpc_type(symtab, var, max_scope_lev, MUTATE, &lhs_owned);
        fprintf(stderr, "[timing] varassign lhs resolve_kgpc_type: %.2f ms (line=%d)\n",
                semstmt_now_ms() - t0, stmt->line_num);
        t0 = semstmt_now_ms();
        rhs_kgpctype = semcheck_resolve_expression_kgpc_type(symtab, expr, INT_MAX, NO_MUTATE, &rhs_owned);
        fprintf(stderr, "[timing] varassign rhs resolve_kgpc_type: %.2f ms (line=%d)\n",
                semstmt_now_ms() - t0, stmt->line_num);
    } else {
        lhs_kgpctype = semcheck_resolve_expression_kgpc_type(symtab, var, max_scope_lev, MUTATE, &lhs_owned);
        rhs_kgpctype = semcheck_resolve_expression_kgpc_type(symtab, expr, INT_MAX, NO_MUTATE, &rhs_owned);
    }
    int handled_by_kgpctype = 0;

    if (lhs_kgpctype != NULL && rhs_kgpctype != NULL)
    {
        handled_by_kgpctype = 1;

        if (var != NULL && var->type == EXPR_VAR_ID && var->expr_data.id != NULL)
        {
            const char *cur_id = semcheck_get_current_subprogram_id();
            if (cur_id != NULL)
            {
                /* Check if this is "Result" or the function's own name (Pascal-style
                 * function result assignment: FuncName := value). Both should use the
                 * current function's return type directly from the subprogram tree.
                 * This is critical for case-insensitive overloads where FpFStat and
                 * FPFStat both exist — FindIdent may resolve to the wrong overload's
                 * return type entry in the symbol table.
                 * Also check operator result variable name (e.g., "dest" in
                 * operator :=(src) dest: variant). */
                const char *result_var = semcheck_get_current_subprogram_result_var_name();
                int is_result_assign = pascal_identifier_equals(var->expr_data.id, "Result") ||
                                       pascal_identifier_equals(var->expr_data.id, cur_id) ||
                                       (result_var != NULL && pascal_identifier_equals(var->expr_data.id, result_var));
                if (is_result_assign)
                {
                    int ret_owned = 0;
                    KgpcType *ret_type = semcheck_get_current_subprogram_return_kgpc_type(symtab, &ret_owned);
                    if (ret_type != NULL &&
                        !(ret_type->kind == TYPE_KIND_PRIMITIVE && ret_type->info.primitive_type_tag < 0))
                    {
                        /* Always use the function's declared return type for result
                         * variable assignments, even if the current LHS type seems
                         * compatible. This handles cases where FindIdent found a
                         * different overload's return type entry (e.g., Variant vs
                         * String for operator overloads with named result vars). */
                        if (lhs_owned && lhs_kgpctype != NULL)
                            destroy_kgpc_type(lhs_kgpctype);
                        lhs_kgpctype = ret_type;
                        lhs_owned = ret_owned;
                    }
                    else if (ret_type != NULL && ret_owned)
                    {
                        destroy_kgpc_type(ret_type);
                    }
                }
            }
        }

        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] Type compatibility check:\n");
            fprintf(stderr, "[SemCheck]   lhs_kgpctype=%p kind=%d\n", (void*)lhs_kgpctype, lhs_kgpctype->kind);
            fprintf(stderr, "[SemCheck]   rhs_kgpctype=%p kind=%d\n", (void*)rhs_kgpctype, rhs_kgpctype->kind);
            if (lhs_kgpctype->kind == TYPE_KIND_POINTER && lhs_kgpctype->info.points_to != NULL) {
                fprintf(stderr, "[SemCheck]   lhs points_to=%p kind=%d\n", 
                    (void*)lhs_kgpctype->info.points_to, lhs_kgpctype->info.points_to->kind);
                if (lhs_kgpctype->info.points_to->kind == TYPE_KIND_RECORD) {
                    fprintf(stderr, "[SemCheck]   lhs record_info=%p\n", 
                        (void*)lhs_kgpctype->info.points_to->info.record_info);
                }
            }
            if (rhs_kgpctype->kind == TYPE_KIND_POINTER && rhs_kgpctype->info.points_to != NULL) {
                fprintf(stderr, "[SemCheck]   rhs points_to=%p kind=%d\n", 
                    (void*)rhs_kgpctype->info.points_to, rhs_kgpctype->info.points_to->kind);
                if (rhs_kgpctype->info.points_to->kind == TYPE_KIND_RECORD) {
                    fprintf(stderr, "[SemCheck]   rhs record_info=%p\n", 
                        (void*)rhs_kgpctype->info.points_to->info.record_info);
                }
            }
        }
        
        int lhs_is_char = (lhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
            lhs_kgpctype->info.primitive_type_tag == CHAR_TYPE);
        int rhs_is_single_char_literal = 0;
        int rhs_is_single_char_const = 0;
        if (lhs_is_char && expr != NULL && expr->type == EXPR_STRING &&
            expr->expr_data.string != NULL && strlen(expr->expr_data.string) == 1)
        {
            rhs_is_single_char_literal = 1;
        }
        if (lhs_is_char && expr != NULL && expr->type == EXPR_VAR_ID &&
            expr->expr_data.id != NULL)
        {
            HashNode_t *rhs_node = NULL;
            if (FindIdent(&rhs_node, symtab, expr->expr_data.id) >= 0 &&
                rhs_node != NULL && rhs_node->is_constant &&
                rhs_node->const_string_value != NULL &&
                strlen(rhs_node->const_string_value) == 1)
            {
                rhs_is_single_char_const = 1;
            }
        }
        if ((rhs_is_single_char_literal || rhs_is_single_char_const) && lhs_is_char)
        {
            semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
            goto assignment_types_ok;
        }

        /* Allow WideChar to string assignment - WideChar (aliased to Word) converts to single-char string.
         * Check if LHS is string and RHS is WideChar before the general compatibility check. */
        int lhs_is_string = (lhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
            lhs_kgpctype->info.primitive_type_tag == STRING_TYPE);
        if (lhs_is_string && expr != NULL && semcheck_expr_is_widechar(symtab, expr))
        {
            /* Mark expression as CHAR_TYPE for codegen to promote to string */
            semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
            goto assignment_types_ok;
        }

        /* Special handling for Currency := Real assignment.
         * Currency is a fixed-point type that stores values scaled by 10000.
         * When assigning a real literal to Currency, we scale it at compile time. */
        if (semcheck_is_currency_kgpc_type(lhs_kgpctype) &&
            rhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
            rhs_kgpctype->info.primitive_type_tag == REAL_TYPE &&
            expr != NULL && expr->type == EXPR_RNUM)
        {
            /* Scale the real value by 10000 and convert to integer */
            long long scaled = llround(expr->expr_data.r_num * 10000.0);
            expr->type = EXPR_INUM;
            expr->expr_data.i_num = scaled;
            semcheck_expr_set_resolved_type(expr, INT64_TYPE);
            goto assignment_types_ok;
        }
        if (semcheck_is_currency_kgpc_type(lhs_kgpctype) &&
            rhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
            rhs_kgpctype->info.primitive_type_tag == REAL_TYPE)
        {
            /* Allow real-to-currency assignment for non-literals; runtime handles conversion. */
            goto assignment_types_ok;
        }

        if (kgpc_type_is_array(lhs_kgpctype) && kgpc_type_is_array(rhs_kgpctype))
        {
            KgpcType *lhs_elem = lhs_kgpctype->info.array_info.element_type;
            KgpcType *rhs_elem = rhs_kgpctype->info.array_info.element_type;
            if (lhs_elem != NULL && rhs_elem != NULL &&
                lhs_elem->kind == TYPE_KIND_PRIMITIVE &&
                rhs_elem->kind == TYPE_KIND_PRIMITIVE &&
                lhs_elem->info.primitive_type_tag == CHAR_TYPE &&
                rhs_elem->info.primitive_type_tag == CHAR_TYPE)
            {
                goto assignment_types_ok;
            }
        }
        if (expr != NULL && expr->type == EXPR_ARRAY_LITERAL &&
            kgpc_type_is_array(lhs_kgpctype))
        {
            KgpcType *lhs_elem = lhs_kgpctype->info.array_info.element_type;
            int elem_tag = semcheck_tag_from_kgpc(lhs_elem);
            const char *elem_type_id = NULL;
            if (lhs_elem != NULL && lhs_elem->kind == TYPE_KIND_RECORD &&
                lhs_elem->info.record_info != NULL &&
                lhs_elem->info.record_info->type_id != NULL)
            {
                elem_type_id = lhs_elem->info.record_info->type_id;
            }
            if (getenv("KGPC_DEBUG_ARRAY_ASSIGN") != NULL)
            {
                fprintf(stderr,
                    "[KGPC] array assign @ line %d: elem_tag=%d elem_id=%s\n",
                    stmt->line_num,
                    elem_tag,
                    elem_type_id != NULL ? elem_type_id : "<null>");
            }
            if (expr->array_element_type == UNKNOWN_TYPE)
                expr->array_element_type = elem_tag;
            if (expr->array_element_type_id == NULL && elem_type_id != NULL)
                expr->array_element_type_id = strdup(elem_type_id);
            semcheck_typecheck_array_literal(expr, symtab, INT_MAX,
                elem_tag, elem_type_id, stmt->line_num);
            goto assignment_types_ok;
        }

        if (!are_types_compatible_for_assignment(lhs_kgpctype, rhs_kgpctype, symtab))
        {
            if (semcheck_try_record_assignment_operator(symtab, stmt, lhs_kgpctype,
                    &rhs_kgpctype, &rhs_owned))
            {
                expr = stmt->stmt_data.var_assign_data.expr;
                type_second = semcheck_tag_from_kgpc(rhs_kgpctype);
                goto assignment_types_ok;
            }

            int allow_char_literal = 0;
            if (lhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
                lhs_kgpctype->info.primitive_type_tag == CHAR_TYPE &&
                rhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
                rhs_kgpctype->info.primitive_type_tag == STRING_TYPE &&
                expr != NULL && expr->type == EXPR_STRING &&
                expr->expr_data.string != NULL &&
                strlen(expr->expr_data.string) == 1)
            {
                allow_char_literal = 1;
                semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
            }
            if (allow_char_literal)
                goto assignment_types_ok;

            /* Allow assigning string literals to PChar/PAnsiChar.
             * In Pascal, string literals can be implicitly converted to PChar. */
            int allow_string_to_pchar = 0;
            if (kgpc_type_is_pointer(lhs_kgpctype))
            {
                KgpcType *points_to = lhs_kgpctype->info.points_to;
                if (points_to != NULL && 
                    points_to->kind == TYPE_KIND_PRIMITIVE &&
                    points_to->info.primitive_type_tag == CHAR_TYPE &&
                    (rhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
                     (rhs_kgpctype->info.primitive_type_tag == STRING_TYPE ||
                      rhs_kgpctype->info.primitive_type_tag == SHORTSTRING_TYPE ||
                      rhs_kgpctype->info.primitive_type_tag == CHAR_TYPE)))
                {
                    allow_string_to_pchar = 1;
                }
            }
            if (allow_string_to_pchar)
                goto assignment_types_ok;

            if (getenv("KGPC_DEBUG_RESULT") != NULL && var != NULL &&
                var->type == EXPR_VAR_ID && var->expr_data.id != NULL &&
                pascal_identifier_equals(var->expr_data.id, "Result"))
            {
                fprintf(stderr,
                    "[KGPC] assignment Result type mismatch: lhs=%s rhs=%s\n",
                    kgpc_type_to_string(lhs_kgpctype),
                    kgpc_type_to_string(rhs_kgpctype));
                if (expr != NULL)
                {
                    fprintf(stderr,
                        "[KGPC] rhs expr type=%d resolved_kgpc=%s rhs_kgpc=%s\n",
                        expr->type,
                        expr->resolved_kgpc_type ? kgpc_type_to_string(expr->resolved_kgpc_type) : "<null>",
                        rhs_kgpctype ? kgpc_type_to_string(rhs_kgpctype) : "<null>");
                }
            }

            const char *lhs_name = "<expression>";
            if (var != NULL && var->type == EXPR_VAR_ID)
                lhs_name = var->expr_data.id;
            semantic_error_at(stmt->line_num, stmt->col_num, stmt->source_index,
                "incompatible types in assignment for %s (lhs: %s, rhs: %s)!",
                lhs_name,
                kgpc_type_to_string(lhs_kgpctype),
                kgpc_type_to_string(rhs_kgpctype));
            ++return_val;
        }
        else if (type_first == PROCEDURE && type_second == PROCEDURE)
        {
            /* AST TRANSFORMATION: Mark RHS as procedure address if it's a direct procedure reference */
            /* Only transform if BOTH LHS and RHS are actual procedures (not functions) */
            /* Functions should be called, not have their address taken */
            int lhs_is_procedure = (lhs_kgpctype->kind == TYPE_KIND_PROCEDURE && 
                                    lhs_kgpctype->info.proc_info.return_type == NULL);
            int rhs_is_procedure = (rhs_kgpctype->kind == TYPE_KIND_PROCEDURE && 
                                    rhs_kgpctype->info.proc_info.return_type == NULL);
            
            if (lhs_is_procedure && rhs_is_procedure && expr != NULL && expr->type == EXPR_VAR_ID)
            {
                HashNode_t *rhs_symbol = NULL;
                if (FindIdent(&rhs_symbol, symtab, expr->expr_data.id) >= 0 &&
                    rhs_symbol != NULL && rhs_symbol->hash_type == HASHTYPE_PROCEDURE)
                {
                    /* Transform the expression to EXPR_ADDR_OF_PROC */
                    expr->type = EXPR_ADDR_OF_PROC;
                    expr->expr_data.addr_of_proc_data.procedure_symbol = rhs_symbol;
                }
            }
        }
assignment_types_ok:
        ;
    }

    if (!handled_by_kgpctype)
    {
        int coerced_rhs_type = type_second;
        int types_compatible = (type_first == type_second);
        
        /* Reject assignment of scalar to array or vice versa */
        /* Arrays must be assigned from arrays, scalars from scalars */
        if (types_compatible && var != NULL && expr != NULL &&
            var->is_array_expr != expr->is_array_expr)
        {
            types_compatible = 0;
        }

        if (!types_compatible)
        {
            if ((type_first == LONGINT_TYPE && type_second == INT_TYPE) ||
                (type_first == INT_TYPE && type_second == LONGINT_TYPE))
            {
                types_compatible = 1;
            }
            else if (type_first == REAL_TYPE &&
                (type_second == INT_TYPE || type_second == LONGINT_TYPE))
            {
                types_compatible = 1;
                coerced_rhs_type = REAL_TYPE;
                if (expr != NULL)
                {
                    if (expr->type == EXPR_INUM)
                    {
                        double coerced_value = (double)expr->expr_data.i_num;
                        expr->type = EXPR_RNUM;
                        expr->expr_data.r_num = coerced_value;
                    }
                    semcheck_expr_set_resolved_type(expr, REAL_TYPE);
                }
            }
            else if (type_first == CHAR_TYPE && type_second == STRING_TYPE &&
                expr != NULL && expr->type == EXPR_STRING &&
                expr->expr_data.string != NULL && strlen(expr->expr_data.string) == 1)
            {
                types_compatible = 1;
                coerced_rhs_type = CHAR_TYPE;
                semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
            }
            /* Allow char to string assignment - char will be promoted to single-character string */
            /* Only for actual string variables, not char arrays */
            else if (type_first == STRING_TYPE && type_second == CHAR_TYPE &&
                var != NULL && !var->is_array_expr)
            {
                types_compatible = 1;
                /* Keep CHAR_TYPE so code generator knows to promote */
            }
            else if (type_first == STRING_TYPE &&
                (type_second == PROCEDURE || type_second == POINTER_TYPE) &&
                var != NULL && !var->is_array_expr)
            {
                types_compatible = 1;
            }
            /* Allow WideChar to string assignment - WideChar will be converted to single-character string.
             * WideChar is aliased to Word (integer), so we need to check the type name. */
            else if (type_first == STRING_TYPE && is_integer_type(type_second) &&
                var != NULL && !var->is_array_expr &&
                expr != NULL && semcheck_expr_is_widechar(symtab, expr))
            {
                types_compatible = 1;
                /* Mark expression as CHAR_TYPE for codegen to promote to string */
                semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
            }
            /* Allow char assignment to char arrays (FPC compatibility) */
            else if (type_first == CHAR_TYPE && type_second == CHAR_TYPE &&
                var != NULL && var->is_array_expr && var->array_element_type == CHAR_TYPE &&
                (expr == NULL || !expr->is_array_expr))
            {
                types_compatible = 1;
                fprintf(stderr,
                    "Warning on line %d, assigning char to array of char copies only the first element (FPC compatibility).\n\n",
                    stmt->line_num);
            }
            /* Allow string literal assignment to char arrays */
            else if (type_first == CHAR_TYPE && type_second == STRING_TYPE &&
                var != NULL && var->is_array_expr && var->array_element_type == CHAR_TYPE &&
                expr != NULL && expr->type == EXPR_STRING)
            {
                /* Verify string fits in array (including null terminator) */
                size_t string_len = expr->expr_data.string != NULL ? strlen(expr->expr_data.string) : 0;
                int array_size = var->array_upper_bound - var->array_lower_bound + 1;
                
                if (string_len > (size_t)array_size)
                {
                    const char *lhs_name = (var->type == EXPR_VAR_ID) ? var->expr_data.id : "<expression>";
                    semcheck_error_with_context(
                        "Error on line %d, string literal too long for array %s (string length: %zu, array size: %d)!\n\n",
                        stmt->line_num,
                        lhs_name,
                        string_len,
                        array_size);
                    ++return_val;
                }
                else
                {
                    types_compatible = 1;
                    /* Keep the type as STRING_TYPE to signal code generator to emit string-to-array copy */
                }
            }
        }

        if (!types_compatible)
        {
            const char *lhs_name = "<expression>";
            if (var != NULL && var->type == EXPR_VAR_ID)
                lhs_name = var->expr_data.id;
            const char *lhs_desc = (lhs_kgpctype != NULL) ? kgpc_type_to_string(lhs_kgpctype) : "unknown";
            const char *rhs_desc = (rhs_kgpctype != NULL) ? kgpc_type_to_string(rhs_kgpctype) : "unknown";
            semantic_error(stmt->line_num, stmt->col_num,
                "type mismatch in assignment statement for %s (lhs: %s, rhs: %s)",
                lhs_name,
                lhs_desc,
                rhs_desc);
            ++return_val;
        }
        else
        {
            type_second = coerced_rhs_type;
        }
    }

    int property_result = semcheck_try_property_assignment(symtab, stmt, max_scope_lev);
    if (property_result >= 0)
    {
        if (lhs_owned && lhs_kgpctype != NULL)
            destroy_kgpc_type(lhs_kgpctype);
        if (rhs_owned && rhs_kgpctype != NULL)
            destroy_kgpc_type(rhs_kgpctype);
        return return_val + property_result;
    }

    /* Clean up owned KgpcTypes */
    if (lhs_owned && lhs_kgpctype != NULL)
        destroy_kgpc_type(lhs_kgpctype);
    if (rhs_owned && rhs_kgpctype != NULL)
        destroy_kgpc_type(rhs_kgpctype);

    return return_val;
}

static int semcheck_try_module_property_assignment(SymTab_t *symtab,
    struct Statement *stmt, int max_scope_lev)
{
    if (symtab == NULL || stmt == NULL || stmt->type != STMT_VAR_ASSIGN)
        return -1;

    struct Expression *lhs = stmt->stmt_data.var_assign_data.var;
    struct Expression *rhs = stmt->stmt_data.var_assign_data.expr;
    if (lhs == NULL || rhs == NULL || lhs->type != EXPR_VAR_ID)
        return -1;

    const char *prop_name = lhs->expr_data.id;
    if (prop_name == NULL)
        return -1;

    ListNode_t *matches = FindAllIdents(symtab, prop_name);
    HashNode_t *setter = NULL;
    int has_storage_symbol = 0;

    for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node == NULL)
            continue;
        if (node->hash_type == HASHTYPE_VAR || node->hash_type == HASHTYPE_ARRAY ||
            node->hash_type == HASHTYPE_CONST || node->hash_type == HASHTYPE_FUNCTION_RETURN)
        {
            has_storage_symbol = 1;
            break;
        }
        if (node->hash_type == HASHTYPE_PROCEDURE && node->type != NULL &&
            node->type->kind == TYPE_KIND_PROCEDURE)
        {
            int param_count = ListLength(node->type->info.proc_info.params);
            if (param_count == 1)
                setter = node;
        }
    }

    if (matches != NULL)
        DestroyList(matches);

    if (has_storage_symbol || setter == NULL)
        return -1;

    char *call_id = lhs->expr_data.id;
    lhs->expr_data.id = NULL;
    destroy_expr(lhs);
    stmt->stmt_data.var_assign_data.var = NULL;
    stmt->stmt_data.var_assign_data.expr = NULL;

    ListNode_t *args = CreateListNode(rhs, LIST_EXPR);
    if (args == NULL)
    {
        free(call_id);
        return 1;
    }

    stmt->type = STMT_PROCEDURE_CALL;
    memset(&stmt->stmt_data.procedure_call_data, 0, sizeof(stmt->stmt_data.procedure_call_data));
    stmt->stmt_data.procedure_call_data.id = call_id;
    stmt->stmt_data.procedure_call_data.expr_args = args;
    stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;

    return semcheck_proccall(symtab, stmt, max_scope_lev);
}

static int semcheck_convert_property_assignment_to_setter(SymTab_t *symtab,
    struct Statement *stmt, struct Expression *lhs, HashNode_t *setter_node,
    int max_scope_lev)
{
    struct Expression *object_expr = lhs->expr_data.record_access_data.record_expr;
    if (object_expr == NULL)
    {
        semcheck_error_with_context("Error on line %d, property assignment requires an object instance.\n\n",
            stmt->line_num);
        return 1;
    }

    lhs->expr_data.record_access_data.record_expr = NULL;
    struct Expression *value_expr = stmt->stmt_data.var_assign_data.expr;
    stmt->stmt_data.var_assign_data.expr = NULL;

    destroy_expr(lhs);
    stmt->stmt_data.var_assign_data.var = NULL;

    ListNode_t *self_arg = CreateListNode(object_expr, LIST_EXPR);
    if (self_arg == NULL)
    {
        semcheck_error_with_context("Error on line %d, unable to allocate setter argument list.\n\n",
            stmt->line_num);
        destroy_expr(object_expr);
        destroy_expr(value_expr);
        return 1;
    }

    ListNode_t *value_arg = CreateListNode(value_expr, LIST_EXPR);
    if (value_arg == NULL)
    {
        semcheck_error_with_context("Error on line %d, unable to allocate setter argument list.\n\n",
            stmt->line_num);
        destroy_expr(object_expr);
        destroy_expr(value_expr);
        free(self_arg);
        return 1;
    }
    self_arg->next = value_arg;

    char *id_copy = setter_node->id != NULL ? strdup(setter_node->id) : NULL;
    char *mangled_copy = NULL;
    if (setter_node->mangled_id != NULL)
        mangled_copy = strdup(setter_node->mangled_id);

    if ((setter_node->id != NULL && id_copy == NULL) ||
        (setter_node->mangled_id != NULL && mangled_copy == NULL))
    {
        semcheck_error_with_context("Error on line %d, unable to prepare property setter call.\n\n",
            stmt->line_num);
        free(id_copy);
        free(mangled_copy);
        value_arg->next = NULL;
        destroy_expr(object_expr);
        destroy_expr(value_expr);
        free(value_arg);
        free(self_arg);
        return 1;
    }

    stmt->type = STMT_PROCEDURE_CALL;
    stmt->stmt_data.procedure_call_data.id = id_copy;
    stmt->stmt_data.procedure_call_data.mangled_id = mangled_copy;
    stmt->stmt_data.procedure_call_data.expr_args = self_arg;
    stmt->stmt_data.procedure_call_data.resolved_proc = NULL;
    stmt->stmt_data.procedure_call_data.call_hash_type = 0;
    stmt->stmt_data.procedure_call_data.is_call_info_valid = 0;
    stmt->stmt_data.procedure_call_data.is_procedural_var_call = 0;
    stmt->stmt_data.procedure_call_data.procedural_var_symbol = NULL;
    stmt->stmt_data.procedure_call_data.procedural_var_expr = NULL;
    semcheck_stmt_set_call_kgpc_type(stmt, NULL, 0);

    return semcheck_proccall(symtab, stmt, max_scope_lev);
}

static int semcheck_try_property_assignment(SymTab_t *symtab,
    struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL || stmt->type != STMT_VAR_ASSIGN)
        return -1;

    struct Expression *lhs = stmt->stmt_data.var_assign_data.var;
    if (lhs == NULL || lhs->type != EXPR_RECORD_ACCESS)
        return -1;

    const char *property_name = lhs->expr_data.record_access_data.field_id;
    if (property_name == NULL)
        return -1;

    struct Expression *object_expr = lhs->expr_data.record_access_data.record_expr;
    if (object_expr == NULL)
        return -1;

    struct RecordType *object_record = semcheck_with_resolve_record_type(symtab,
        object_expr, semcheck_tag_from_kgpc(object_expr->resolved_kgpc_type), stmt->line_num);

    if (object_record == NULL)
        return -1;

    struct RecordType *property_owner = NULL;
    struct ClassProperty *property = semcheck_find_class_property(symtab,
        object_record, property_name, &property_owner);
    if (property == NULL || property->write_accessor == NULL)
        return -1;

    if (property_owner == NULL)
        property_owner = object_record;

    struct RecordField *write_field =
        semcheck_find_class_field_including_hidden(symtab,
            object_record, property->write_accessor, NULL);
    if (write_field != NULL)
        return -1;

    HashNode_t *setter_node = semcheck_find_class_method(symtab,
        property_owner, property->write_accessor, NULL);
    if (setter_node == NULL)
    {
        semcheck_error_with_context("Error on line %d, setter %s for property %s not found.\n\n",
            stmt->line_num,
            property->write_accessor != NULL ? property->write_accessor : "<unknown>",
            property->name != NULL ? property->name : property_name);
        return 1;
    }

    if (setter_node->hash_type != HASHTYPE_PROCEDURE)
    {
        semcheck_error_with_context("Error on line %d, property setter %s must be a procedure.\n\n",
            stmt->line_num, property->write_accessor);
        return 1;
    }

    return semcheck_convert_property_assignment_to_setter(symtab, stmt, lhs,
        setter_node, max_scope_lev);
}

/** PROCEDURE_CALL **/
int semcheck_proccall(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val, scope_return, cur_arg;
    HashNode_t *sym_return;
    ListNode_t *true_args, *true_arg_ids, *args_given;
    Tree_t *arg_decl;
    char *proc_id;
    char *mangled_name;
    int static_arg_already_removed = 0;
    int static_method_receiver = 0;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_PROCEDURE_CALL);

    return_val = 0;

    proc_id = stmt->stmt_data.procedure_call_data.id;
    args_given = stmt->stmt_data.procedure_call_data.expr_args;

    /* If this is a method call placeholder with a type identifier receiver,
     * resolve it to the class method immediately to avoid type-helper detours. */
    if (proc_id != NULL && strncmp(proc_id, "__", 2) == 0 && args_given != NULL)
    {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindIdent(&type_node, symtab, first_arg->expr_data.id) >= 0 &&
                type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
            {
                struct RecordType *record_info = semcheck_stmt_get_record_type_from_node(type_node);
                if (record_info != NULL && record_info->type_id != NULL)
                {
                    const char *method_name = proc_id + 2;
                    size_t class_len = strlen(record_info->type_id);
                    size_t method_len = strlen(method_name);
                    char *new_proc_id = (char *)malloc(class_len + 2 + method_len + 1);
                    if (new_proc_id != NULL)
                    {
                        sprintf(new_proc_id, "%s__%s", record_info->type_id, method_name);
                        free(proc_id);
                        proc_id = new_proc_id;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        ListNode_t *remaining_args = args_given->next;
                        destroy_expr(first_arg);
                        args_given->cur = NULL;
                        free(args_given);
                        stmt->stmt_data.procedure_call_data.expr_args = remaining_args;
                        args_given = remaining_args;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                        static_arg_already_removed = 1;
                    }
                }
            }
        }
    }

    /* FPC Bootstrap Feature: Handle unit-qualified procedure calls.
     * When the parser sees Unit.Procedure(args), it creates a procedure call with id "__Procedure"
     * and passes Unit as the first argument (as if it were a method call).
     * We need to detect this pattern and transform it back to a direct procedure call.
     *
     * Pattern: proc_id starts with "__", first arg is a VAR_ID that doesn't exist in symbol table
     * (it's the unit name), and the procedure name (without "__" prefix) exists in symbol table.
     */
    if (proc_id != NULL && strncmp(proc_id, "__", 2) == 0 && args_given != NULL)
    {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            char *potential_unit_name = first_arg->expr_data.id;
            HashNode_t *unit_check = NULL;

            /* Check if the first argument is NOT a known variable (i.e., it's a unit qualifier) */
            if (FindIdent(&unit_check, symtab, potential_unit_name) == -1)
            {
                /* First arg is not a known identifier - might be a unit qualifier.
                 * Try to look up the procedure name without the "__" prefix. */
                char *real_proc_name = strdup(proc_id + 2);  /* Skip the "__" prefix */
                if (real_proc_name == NULL)
                {
                    /* strdup failed - skip transformation, will report error later */
                }
                else
                {
                    ListNode_t *proc_candidates = FindAllIdents(symtab, real_proc_name);

                    if (proc_candidates != NULL)
                    {
                        /* Found the procedure by name. Transform the call:
                         * 1. Remove the first argument (the unit qualifier)
                         * 2. Change proc_id to the real procedure name (without "__")
                         */
                        /* Save the remaining args before modifying the list */
                        ListNode_t *remaining_args = args_given->next;

                        /* Free the unit qualifier expression and list node.
                         * Note: remaining_args holds the saved pointer value, so
                         * freeing args_given doesn't affect it. */
                        destroy_expr(first_arg);
                        args_given->cur = NULL;
                        free(args_given);

                        /* Update the statement with the transformed call */
                        stmt->stmt_data.procedure_call_data.expr_args = remaining_args;

                        /* Update proc_id - we already have real_proc_name allocated */
                        free(proc_id);
                        proc_id = real_proc_name;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        args_given = remaining_args;

                        DestroyList(proc_candidates);

                        /* Continue with normal procedure call handling using the transformed call */
                    }
                    else
                    {
                        /* Procedure not found - free real_proc_name and fall through to report error */
                        free(real_proc_name);
                    }
                }
            }
        }
    }

    /* If no explicit receiver was provided, but Self is in scope and defines this method,
     * prepend Self so unqualified method calls resolve correctly. */
    if (proc_id != NULL && !stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
    {
        HashNode_t *self_node = NULL;
        struct RecordType *self_record = NULL;
        if (FindIdent(&self_node, symtab, "Self") != -1 && self_node != NULL)
        {
            self_record = semcheck_stmt_get_record_type_from_node(self_node);
            if (self_record == NULL)
            {
                int self_type_tag = UNKNOWN_TYPE;
                const char *self_type_name = NULL;
                set_type_from_hashtype(&self_type_tag, self_node);
                if (self_node->type != NULL &&
                    self_node->type->type_alias != NULL &&
                    self_node->type->type_alias->target_type_id != NULL)
                {
                    self_type_name = self_node->type->type_alias->target_type_id;
                }

                struct RecordType *helper_record = semcheck_lookup_type_helper(
                    symtab, self_type_tag, self_type_name);
                if (helper_record != NULL)
                    self_record = helper_record;
            }
        }

        if (self_record == NULL)
        {
            const char *current_owner = semcheck_get_current_method_owner();
            if (current_owner != NULL)
                self_record = semcheck_lookup_record_type(symtab, current_owner);
        }

        if (self_record != NULL)
        {
            HashNode_t *method_node = semcheck_find_class_method(symtab, self_record, proc_id, NULL);
            if (method_node != NULL &&
                (method_node->hash_type == HASHTYPE_PROCEDURE ||
                 method_node->hash_type == HASHTYPE_FUNCTION))
            {
                /* Prepend Self to arguments */
                struct Expression *self_expr = mk_varid(stmt->line_num, strdup("Self"));
                ListNode_t *self_arg = CreateListNode(self_expr, LIST_EXPR);
                self_arg->next = args_given;
                stmt->stmt_data.procedure_call_data.expr_args = self_arg;
                args_given = self_arg;

                /* Update proc_id to mangled name */
                size_t class_len = strlen(self_record->type_id);
                size_t method_len = strlen(proc_id);
                char *new_proc_id = (char *)malloc(class_len + 2 + method_len + 1);
                if (new_proc_id != NULL)
                {
                    sprintf(new_proc_id, "%s__%s", self_record->type_id, proc_id);
                    free(proc_id);
                    proc_id = new_proc_id;
                    stmt->stmt_data.procedure_call_data.id = proc_id;
                }
            }
            else if (strstr(proc_id, "__") == NULL && self_record != NULL && self_record->type_id != NULL)
            {
                /* Check if proc_id is a procedural-type field or property of Self's class.
                 * This handles patterns like FCallBack(Self,a,b,c) and OnQueryInterface(x,y,z)
                 * where FCallBack is a field of type TThunkCallBack (procedural type)
                 * and OnQueryInterface is a property reading from a procedural-type field. */
                const char *field_name = proc_id;
                int is_proc_field = 0;

                /* Use semcheck_lookup_record_type to get a safe, validated RecordType from the symbol table,
                 * since self_record obtained from temp call contexts can have corrupt data */
                struct RecordType *safe_record = semcheck_lookup_record_type(symtab, self_record->type_id);
                if (safe_record != NULL)
                {
                    for (ListNode_t *f = safe_record->fields; f != NULL; f = f->next)
                    {
                        if (f->type != LIST_RECORD_FIELD || f->cur == NULL)
                            continue;
                        struct RecordField *rf = (struct RecordField *)f->cur;
                        if (rf->name == NULL)
                            continue;
                        if (strcasecmp(rf->name, field_name) == 0)
                        {
                            if (rf->proc_type != NULL)
                            {
                                is_proc_field = 1;
                                break;
                            }
                            else if (rf->type_id != NULL)
                            {
                                HashNode_t *type_node = NULL;
                                if (FindIdent(&type_node, symtab, rf->type_id) != -1 &&
                                    type_node != NULL && type_node->type != NULL &&
                                    type_node->type->kind == TYPE_KIND_PROCEDURE)
                                {
                                    is_proc_field = 1;
                                    break;
                                }
                            }
                        }
                    }
                    if (!is_proc_field)
                    {
                        /* Check properties — if a property's read_accessor is a procedural-type field */
                        struct ClassProperty *prop = semcheck_find_class_property(symtab, safe_record, proc_id, NULL);
                        if (prop != NULL && prop->read_accessor != NULL)
                        {
                            for (ListNode_t *f2 = safe_record->fields; f2 != NULL; f2 = f2->next)
                            {
                                if (f2->type != LIST_RECORD_FIELD || f2->cur == NULL)
                                    continue;
                                struct RecordField *rf2 = (struct RecordField *)f2->cur;
                                if (rf2->name == NULL)
                                    continue;
                                if (strcasecmp(rf2->name, prop->read_accessor) == 0)
                                {
                                    if (rf2->proc_type != NULL)
                                    {
                                        is_proc_field = 1;
                                        field_name = prop->read_accessor;
                                        break;
                                    }
                                    else if (rf2->type_id != NULL)
                                    {
                                        HashNode_t *type_node = NULL;
                                        if (FindIdent(&type_node, symtab, rf2->type_id) != -1 &&
                                            type_node != NULL && type_node->type != NULL &&
                                            type_node->type->kind == TYPE_KIND_PROCEDURE)
                                        {
                                            is_proc_field = 1;
                                            field_name = prop->read_accessor;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                if (is_proc_field)
                {
                    /* Convert to Self.field(...) procedural variable call.
                     * Build a record access expression Self.field and set it as procedural_var_expr */
                    struct Expression *self_expr = mk_varid(stmt->line_num, strdup("Self"));
                    struct Expression *field_access = mk_recordaccess(stmt->line_num,
                        self_expr, strdup(field_name));

                    stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                    stmt->stmt_data.procedure_call_data.procedural_var_expr = field_access;
                    stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;

                    /* Check the expression for type resolution */
                    int field_tag = UNKNOWN_TYPE;
                    return_val += semcheck_stmt_expr_tag(&field_tag, symtab, field_access, max_scope_lev, NO_MUTATE);

                    return return_val;
                }
            }
        }
    }

    int handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Halt",
        semcheck_builtin_halt, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "SetLength",
        semcheck_builtin_setlength, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "SetString",
        semcheck_builtin_setstring, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "write",
        semcheck_builtin_write_like, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "writeln",
        semcheck_builtin_write_like, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "writestr",
        semcheck_builtin_writestr, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "read",
        semcheck_builtin_read_like, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "readln",
        semcheck_builtin_read_like, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Assign",
        semcheck_builtin_assign, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Close",
        semcheck_builtin_close, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "SetTextCodePage",
        semcheck_builtin_settextcodepage, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "GetMem",
        semcheck_builtin_getmem, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "FreeMem",
        semcheck_builtin_freemem, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Move",
        semcheck_builtin_move, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "ReallocMem",
        semcheck_builtin_reallocmem, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "SetCodePage",
        semcheck_builtin_setcodepage, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "InterlockedExchangeAdd",
        semcheck_builtin_interlockedexchangeadd, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Val",
        semcheck_builtin_val, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Str",
        semcheck_builtin_strproc, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Insert",
        semcheck_builtin_insert, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Delete",
        semcheck_builtin_delete, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Inc",
        semcheck_builtin_inc, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Dec",
        semcheck_builtin_dec, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Include",
        semcheck_builtin_include, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Exclude",
        semcheck_builtin_exclude, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "New",
        semcheck_builtin_new, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Dispose",
        semcheck_builtin_dispose, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Initialize",
        semcheck_builtin_initialize, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Finalize",
        semcheck_builtin_finalize, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Assert",
        semcheck_builtin_assert, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    /* Handle procedural fields on records (advanced records) similarly to function calls */
    if (proc_id != NULL && args_given != NULL)
    {
        struct Expression *receiver_expr = (struct Expression *)args_given->cur;
        if (receiver_expr != NULL && receiver_expr->type == EXPR_RECORD_CONSTRUCTOR &&
            (receiver_expr->resolved_kgpc_type == NULL ||
             !kgpc_type_is_record(receiver_expr->resolved_kgpc_type)))
        {
            receiver_expr = NULL;
        }
        if (receiver_expr != NULL)
        {
            int recv_type = UNKNOWN_TYPE;
            semcheck_stmt_expr_tag(&recv_type, symtab, receiver_expr, max_scope_lev, NO_MUTATE);

            struct RecordType *recv_record = NULL;
            if (recv_type == RECORD_TYPE &&
                receiver_expr->resolved_kgpc_type != NULL &&
                receiver_expr->resolved_kgpc_type->kind == TYPE_KIND_RECORD)
            {
                recv_record = kgpc_type_get_record(receiver_expr->resolved_kgpc_type);
            }
            else if (recv_type == POINTER_TYPE)
            {
                if (receiver_expr->resolved_kgpc_type != NULL &&
                    receiver_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER)
                {
                    KgpcType *pointee = receiver_expr->resolved_kgpc_type->info.points_to;
                    if (pointee != NULL && kgpc_type_is_record(pointee))
                        recv_record = kgpc_type_get_record(pointee);
                }
            }
            /* Also try resolved_kgpc_type directly for record types when record_type is NULL */
            if (recv_record == NULL && receiver_expr->resolved_kgpc_type != NULL &&
                receiver_expr->resolved_kgpc_type->kind == TYPE_KIND_RECORD)
            {
                recv_record = kgpc_type_get_record(receiver_expr->resolved_kgpc_type);
            }
            if (recv_record == NULL && receiver_expr->type == EXPR_VAR_ID &&
                receiver_expr->expr_data.id != NULL)
            {
                HashNode_t *recv_node = NULL;
                if (FindIdent(&recv_node, symtab, receiver_expr->expr_data.id) == 0 &&
                    recv_node != NULL)
                {
                    recv_record = semcheck_stmt_get_record_type_from_node(recv_node);
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
                const char *field_lookup = proc_id;
                while (field_lookup != NULL && field_lookup[0] == '_' && field_lookup[1] == '_')
                    field_lookup += 2;

                struct RecordField *field_desc = NULL;
                long long field_offset = 0;
                int field_found = resolve_record_field(symtab, recv_record, field_lookup, &field_desc,
                                         &field_offset, stmt->line_num, 1);
                if (field_found == 0 && field_desc != NULL)
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
                        /* If not found directly, try with class prefix (for nested types) */
                        if (!is_proc_field && recv_record != NULL && recv_record->type_id != NULL)
                        {
                            char qualified[512];
                            snprintf(qualified, sizeof(qualified), "%s.%s", recv_record->type_id, field_desc->type_id);
                            type_node = NULL;
                            if (FindIdent(&type_node, symtab, qualified) == 0 &&
                                type_node != NULL && type_node->type != NULL &&
                                type_node->type->kind == TYPE_KIND_PROCEDURE)
                            {
                                proc_type = type_node->type;
                                kgpc_type_retain(proc_type);
                                is_proc_field = 1;
                            }
                        }
                    }
                    else if (field_desc->proc_type != NULL &&
                             field_desc->proc_type->kind == TYPE_KIND_PROCEDURE)
                    {
                        proc_type = field_desc->proc_type;
                        kgpc_type_retain(proc_type);
                        is_proc_field = 1;
                    }

                    if (is_proc_field)
                    {
                        /* Remove receiver argument */
                        ListNode_t *remaining_args = args_given->next;
                        stmt->stmt_data.procedure_call_data.expr_args = remaining_args;
                        args_given->cur = NULL;
                        free(args_given);

                        /* Build record access expression for the procedural field */
                        struct Expression *proc_expr = (struct Expression *)calloc(1, sizeof(struct Expression));
                        if (proc_expr == NULL)
                        {
                            semcheck_error_with_context("Error on line %d: failed to allocate procedural field expression.\n",
                                stmt->line_num);
                            if (proc_type != NULL) destroy_kgpc_type(proc_type);
                            return ++return_val;
                        }
                        proc_expr->line_num = stmt->line_num;
                        proc_expr->type = EXPR_RECORD_ACCESS;
                        proc_expr->expr_data.record_access_data.record_expr = receiver_expr;
                        proc_expr->expr_data.record_access_data.field_id = strdup(field_lookup);
                        proc_expr->expr_data.record_access_data.field_offset = (int)field_offset;
                        if (proc_type != NULL)
                        {
                            if (proc_expr->resolved_kgpc_type != NULL)
                                destroy_kgpc_type(proc_expr->resolved_kgpc_type);
                            proc_expr->resolved_kgpc_type = proc_type;
                        }

                        /* Validate argument count/types if we know the procedural signature */
                        if (proc_type != NULL)
                        {
                            ListNode_t *formal_params = kgpc_type_get_procedure_params(proc_type);
                            if (ListLength(formal_params) != ListLength(remaining_args))
                            {
                                semcheck_error_with_context("Error on line %d, call to procedural field %s: expected %d arguments, got %d\n",
                                    stmt->line_num, proc_id, ListLength(formal_params), ListLength(remaining_args));
                                destroy_expr(proc_expr);
                                /* proc_type already released by destroy_expr via resolved_kgpc_type */
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
                                semcheck_stmt_expr_tag(&actual_type, symtab, actual_expr, max_scope_lev, NO_MUTATE);
                                if (formal_type != UNKNOWN_TYPE && actual_type != UNKNOWN_TYPE &&
                                    formal_type != actual_type)
                                {
                                    if (!((formal_type == LONGINT_TYPE && actual_type == INT_TYPE) ||
                                          (formal_type == INT_TYPE && actual_type == LONGINT_TYPE) ||
                                          (formal_type == POINTER_TYPE) || (actual_type == POINTER_TYPE) ||
                                          (is_integer_type(formal_type) && is_integer_type(actual_type)) ||
                                          (formal_type == REAL_TYPE && is_integer_type(actual_type)) ||
                                          (is_integer_type(formal_type) && actual_type == REAL_TYPE) ||
                                          (formal_type == VARIANT_TYPE) ||
                                          (actual_type == VARIANT_TYPE) ||
                                          (formal_type == RECORD_TYPE) ||
                                          (actual_type == RECORD_TYPE) ||
                                          (formal_type == STRING_TYPE && actual_type == CHAR_TYPE) ||
                                          (formal_type == CHAR_TYPE && actual_type == STRING_TYPE) ||
                                          (formal_type == SHORTSTRING_TYPE && actual_type == CHAR_TYPE)))
                                {
                                    semantic_error_at(stmt->line_num, stmt->col_num, -1,
                                        "Incompatible types: got \"%s\" expected \"%s\"",
                                        type_tag_to_string(actual_type),
                                        type_tag_to_string(formal_type));
                                    destroy_expr(proc_expr);
                                    return ++return_val;
                                }
                                }
                                formal = formal->next;
                                actual = actual->next;
                                arg_idx++;
                            }

                            kgpc_type_retain(proc_type);
                            stmt->stmt_data.procedure_call_data.call_kgpc_type = proc_type;
                            stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                            stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
                        }
                        else
                        {
                            stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                        }

                        stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                        stmt->stmt_data.procedure_call_data.procedural_var_symbol = NULL;
                        stmt->stmt_data.procedure_call_data.procedural_var_expr = proc_expr;
                        return return_val;
                    }
                }
            }
        }
    }

    
    /* Check for method call with unresolved name (member-access placeholder) where first arg is the instance. */
    if (stmt->stmt_data.procedure_call_data.is_method_call_placeholder && args_given != NULL) {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        int first_arg_type_tag;
        semcheck_stmt_expr_tag(&first_arg_type_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);

        KgpcType *owner_type = first_arg->resolved_kgpc_type;
        struct RecordType *record_info = NULL;

        if (owner_type != NULL) {
            if (owner_type->kind == TYPE_KIND_RECORD) {
                record_info = owner_type->info.record_info;
            } else if (owner_type->kind == TYPE_KIND_POINTER &&
                       owner_type->info.points_to != NULL &&
                       owner_type->info.points_to->kind == TYPE_KIND_RECORD) {
                record_info = owner_type->info.points_to->info.record_info;
            } else if (owner_type->kind == TYPE_KIND_POINTER &&
                       owner_type->info.points_to != NULL &&
                       owner_type->info.points_to->kind == TYPE_KIND_POINTER &&
                       owner_type->info.points_to->info.points_to != NULL &&
                       owner_type->info.points_to->info.points_to->kind == TYPE_KIND_RECORD) {
                /* Double-pointer deref: pts^^.Method where pts: ^^Record */
                record_info = owner_type->info.points_to->info.points_to->info.record_info;
            }
        }

        /* Do not rely on legacy record_type metadata; prefer resolved KgpcType only. */

        if (first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindIdent(&type_node, symtab, first_arg->expr_data.id) >= 0 &&
                type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
            {
                static_method_receiver = 1;
                if (record_info == NULL)
                    record_info = semcheck_stmt_get_record_type_from_node(type_node);
            }
        }

        if (record_info != NULL && record_info->type_id != NULL) {
            const char *method_name = proc_id;
            if (method_name != NULL && strncmp(method_name, "__", 2) == 0)
                method_name += 2;

            int is_static = from_cparser_is_method_static(record_info->type_id, method_name);
            if (static_method_receiver)
                is_static = 1;
            HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, method_name, NULL);

            if (method_node != NULL) {
                /* Keep class-prefixed id for static/class calls (e.g. ClassName.Create),
                 * but canonicalize instance calls to the resolved owner id. */
                if (static_method_receiver)
                {
                    size_t class_len = strlen(record_info->type_id);
                    size_t method_len = strlen(method_name);
                    char *new_proc_id = (char *)malloc(class_len + 2 + method_len + 1);
                    if (new_proc_id != NULL) {
                        sprintf(new_proc_id, "%s__%s", record_info->type_id, method_name);
                        free(proc_id);
                        proc_id = new_proc_id;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                    }
                }
                else if (method_node->id != NULL)
                {
                    char *new_proc_id = strdup(method_node->id);
                    if (new_proc_id != NULL) {
                        free(proc_id);
                        proc_id = new_proc_id;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                    }
                }
                else
                {
                    /* Fallback: synthesize class-prefixed method id. */
                    size_t class_len = strlen(record_info->type_id);
                    size_t method_len = strlen(method_name);
                    char *new_proc_id = (char *)malloc(class_len + 2 + method_len + 1);
                    if (new_proc_id != NULL) {
                        sprintf(new_proc_id, "%s__%s", record_info->type_id, method_name);
                        free(proc_id);
                        proc_id = new_proc_id;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                    }
                }

                if (method_node->mangled_id != NULL) {
                    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL &&
                        stmt->stmt_data.procedure_call_data.mangled_id != method_node->mangled_id)
                        free(stmt->stmt_data.procedure_call_data.mangled_id);
                    stmt->stmt_data.procedure_call_data.mangled_id = strdup(method_node->mangled_id);
                }

                int receiver_is_type_ident = 0;
                if (args_given != NULL && args_given->cur != NULL)
                {
                    struct Expression *receiver_expr = (struct Expression *)args_given->cur;
                    if (receiver_expr != NULL && receiver_expr->type == EXPR_VAR_ID &&
                        receiver_expr->expr_data.id != NULL)
                    {
                        HashNode_t *receiver_node = NULL;
                        if (FindIdent(&receiver_node, symtab, receiver_expr->expr_data.id) >= 0 &&
                            receiver_node != NULL && receiver_node->hash_type == HASHTYPE_TYPE)
                            receiver_is_type_ident = 1;
                    }
                }

                if (is_static && receiver_is_type_ident) {
                    /* For static methods, remove the first argument (the instance/type identifier) */
                    ListNode_t *old_head = args_given;
                    stmt->stmt_data.procedure_call_data.expr_args = old_head->next;
                    old_head->next = NULL;
                    args_given = stmt->stmt_data.procedure_call_data.expr_args;
                    static_arg_already_removed = 1;
                }
            }
            else
            {
                /* Method not found — check if this is a procedural-type field being invoked. */
                struct RecordField *proc_field = NULL;
                long long proc_field_offset = 0;
                if (resolve_record_field(symtab, record_info, method_name,
                        &proc_field, &proc_field_offset, stmt->line_num, 1 /* silent */) == 0 &&
                    proc_field != NULL)
                {
                    KgpcType *proc_type = NULL;
                    if (proc_field->proc_type != NULL &&
                        proc_field->proc_type->kind == TYPE_KIND_PROCEDURE)
                    {
                        proc_type = proc_field->proc_type;
                    }
                    else if (proc_field->type_id != NULL)
                    {
                        HashNode_t *type_node = NULL;
                        if (FindIdent(&type_node, symtab, proc_field->type_id) >= 0 &&
                            type_node != NULL && type_node->type != NULL &&
                            type_node->type->kind == TYPE_KIND_PROCEDURE)
                        {
                            proc_type = type_node->type;
                        }
                    }

                    if (proc_type != NULL)
                    {
                        /* Remove first_arg from args (it becomes part of the field access expr) */
                        ListNode_t *old_head = args_given;
                        args_given = old_head->next;
                        old_head->cur = NULL; /* Don't free first_arg, we reuse it */
                        free(old_head);
                        stmt->stmt_data.procedure_call_data.expr_args = args_given;

                        /* Build record access expression for the procedural field */
                        struct Expression *proc_expr = mk_recordaccess(
                            stmt->line_num, first_arg, strdup(method_name));
                        proc_expr->expr_data.record_access_data.field_offset = (int)proc_field_offset;
                        kgpc_type_retain(proc_type);
                        proc_expr->resolved_kgpc_type = proc_type;

                        /* Type-check the arguments */
                        for (ListNode_t *arg_cur = args_given; arg_cur != NULL; arg_cur = arg_cur->next)
                        {
                            struct Expression *arg = (struct Expression *)arg_cur->cur;
                            if (arg != NULL)
                                semcheck_stmt_expr_tag(NULL, symtab, arg, max_scope_lev, NO_MUTATE);
                        }

                        kgpc_type_retain(proc_type);
                        stmt->stmt_data.procedure_call_data.call_kgpc_type = proc_type;
                        stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                        stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
                        stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                        stmt->stmt_data.procedure_call_data.procedural_var_symbol = NULL;
                        stmt->stmt_data.procedure_call_data.procedural_var_expr = proc_expr;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                        return return_val;
                    }
                }
            }
        }
    }

    /* First, check if this is a static method call.
     * Method calls can have two patterns:
     * 1. __MethodName(object, ...) - method call without class prefix
     * 2. ClassName__MethodName(object, ...) - method call with class prefix
     */
    char *method_double_underscore = (proc_id != NULL) ? strstr(proc_id, "__") : NULL;
    if (method_double_underscore != NULL && args_given != NULL && !static_arg_already_removed) {
        const char *method_name = method_double_underscore + 2;
        const char *class_name = NULL;
        int need_free_class_name = 0;
        
        if (method_double_underscore == proc_id) {
            /* Case 1: __MethodName - need to get class from first argument */
            if (args_given != NULL && args_given->cur != NULL) {
                struct Expression *first_arg = (struct Expression *)args_given->cur;
                
                /* Try to get the record type of the first argument */
                struct RecordType *record_type = NULL;
                if (first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, first_arg->expr_data.id) != -1 &&
                        type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                    {
                        record_type = semcheck_stmt_get_record_type_from_node(type_node);
                        if (record_type != NULL && record_type->type_id != NULL)
                            class_name = record_type->type_id;
                    }
                }
                if (first_arg->type == EXPR_VAR_ID) {
                    /* Look up the variable to get its type */
                    HashNode_t *var_node = NULL;
                    if (FindIdent(&var_node, symtab, first_arg->expr_data.id) != -1 && var_node != NULL &&
                        var_node->type != NULL && var_node->type->kind == TYPE_KIND_RECORD) {
                        record_type = var_node->type->info.record_info;
                    }
                }

                /* Prefer resolved KgpcType for deref chains like pts^^.Method(...)
                 * where legacy record_type metadata may be absent. */
                if (record_type == NULL && first_arg->resolved_kgpc_type != NULL)
                {
                    KgpcType *arg_type = first_arg->resolved_kgpc_type;
                    if (arg_type->kind == TYPE_KIND_RECORD)
                    {
                        record_type = arg_type->info.record_info;
                    }
                    else if (arg_type->kind == TYPE_KIND_POINTER &&
                             arg_type->info.points_to != NULL)
                    {
                        KgpcType *pointee = arg_type->info.points_to;
                        if (pointee->kind == TYPE_KIND_RECORD)
                            record_type = pointee->info.record_info;
                        else if (pointee->kind == TYPE_KIND_POINTER &&
                                 pointee->info.points_to != NULL &&
                                 pointee->info.points_to->kind == TYPE_KIND_RECORD)
                            record_type = pointee->info.points_to->info.record_info;
                    }
                }

                if (record_type == NULL && first_arg->pointer_subtype_id != NULL)
                {
                    HashNode_t *subtype_node = NULL;
                    if (FindIdent(&subtype_node, symtab, first_arg->pointer_subtype_id) != -1 &&
                        subtype_node != NULL)
                    {
                        record_type = semcheck_stmt_get_record_type_from_node(subtype_node);
                    }
                }

                if (record_type == NULL || record_type->type_id == NULL)
                {
                    int arg_type_owned = 0;
                    KgpcType *arg_type = semcheck_resolve_expression_kgpc_type(symtab,
                        first_arg, max_scope_lev, NO_MUTATE, &arg_type_owned);
                    int helper_tag = UNKNOWN_TYPE;
                    semcheck_stmt_expr_tag(&helper_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);
                    const char *helper_name = NULL;
                    if (arg_type != NULL)
                    {
                        if (arg_type->kind == TYPE_KIND_PRIMITIVE)
                            helper_tag = arg_type->info.primitive_type_tag;
                        struct TypeAlias *alias = kgpc_type_get_type_alias(arg_type);
                        if (alias != NULL)
                        {
                            if (alias->target_type_id != NULL)
                                helper_name = alias->target_type_id;
                            else if (alias->alias_name != NULL)
                                helper_name = alias->alias_name;
                        }
                    }
                    struct RecordType *helper_record = semcheck_lookup_type_helper(symtab,
                        helper_tag, helper_name);
                    if (helper_record == NULL && first_arg->type == EXPR_VAR_ID &&
                        first_arg->expr_data.id != NULL)
                    {
                        HashNode_t *var_node = NULL;
                        if (FindIdent(&var_node, symtab, first_arg->expr_data.id) != -1 &&
                            var_node != NULL)
                        {
                            struct TypeAlias *var_alias = hashnode_get_type_alias(var_node);
                            const char *var_helper_name = NULL;
                            if (var_alias != NULL)
                            {
                                if (var_alias->target_type_id != NULL)
                                    var_helper_name = var_alias->target_type_id;
                                else if (var_alias->alias_name != NULL)
                                    var_helper_name = var_alias->alias_name;
                            }
                            if (var_helper_name != NULL)
                                helper_record = semcheck_lookup_type_helper(symtab,
                                    UNKNOWN_TYPE, var_helper_name);
                        }
                    }
                    if (helper_record != NULL)
                        record_type = helper_record;
                    if (arg_type_owned && arg_type != NULL)
                        destroy_kgpc_type(arg_type);
                }

                if (class_name == NULL && record_type != NULL && record_type->type_id != NULL) {
                    if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL) {
                        fprintf(stderr, "[SemCheck] method placeholder: resolved helper record %s for %s\n",
                            record_type->type_id, method_name != NULL ? method_name : "<null>");
                    }
                    class_name = record_type->type_id;
                }
            }
        } else {
            /* Case 2: ClassName__MethodName - extract class from proc_id */
            size_t class_len = method_double_underscore - proc_id;
            char *extracted_class = (char *)malloc(class_len + 1);
            if (extracted_class != NULL) {
                memcpy(extracted_class, proc_id, class_len);
                extracted_class[class_len] = '\0';
                class_name = extracted_class;
                need_free_class_name = 1;
            }
            /* If malloc failed, class_name remains NULL and we skip the rest */
        }
        
        if (class_name != NULL && method_name != NULL) {
            int is_static = from_cparser_is_method_static(class_name, method_name);
            HashNode_t *resolved_method = NULL;
            HashNode_t *class_node = NULL;
            if (FindIdent(&class_node, symtab, class_name) != -1 && class_node != NULL)
            {
                struct RecordType *class_record = semcheck_stmt_get_record_type_from_node(class_node);
                if (class_record != NULL)
                    resolved_method = semcheck_find_class_method(symtab, class_record, method_name, NULL);
            }
            
            /* If proc_id started with __, update it to include the class name */
            if (method_double_underscore == proc_id) {
                size_t class_len = strlen(class_name);
                size_t method_len = strlen(method_name);
                char *new_proc_id = (char *)malloc(class_len + 2 + method_len + 1);
                if (new_proc_id != NULL) {
                    sprintf(new_proc_id, "%s__%s", class_name, method_name);
                    free(proc_id);
                    proc_id = new_proc_id;
                    stmt->stmt_data.procedure_call_data.id = proc_id;
                }
            }

            if (resolved_method != NULL && resolved_method->mangled_id != NULL)
            {
                if (stmt->stmt_data.procedure_call_data.mangled_id != NULL &&
                    stmt->stmt_data.procedure_call_data.mangled_id != resolved_method->mangled_id)
                    free(stmt->stmt_data.procedure_call_data.mangled_id);
                stmt->stmt_data.procedure_call_data.mangled_id = strdup(resolved_method->mangled_id);
            }
            if (!static_method_receiver && resolved_method != NULL && resolved_method->id != NULL)
            {
                char *resolved_proc_id = strdup(resolved_method->id);
                if (resolved_proc_id != NULL)
                {
                    free(proc_id);
                    proc_id = resolved_proc_id;
                    stmt->stmt_data.procedure_call_data.id = proc_id;
                }
            }
            
            int receiver_is_type_ident = 0;
            if (args_given != NULL && args_given->cur != NULL)
            {
                struct Expression *receiver_expr = (struct Expression *)args_given->cur;
                if (receiver_expr != NULL && receiver_expr->type == EXPR_VAR_ID &&
                    receiver_expr->expr_data.id != NULL)
                {
                    HashNode_t *receiver_node = NULL;
                    if (FindIdent(&receiver_node, symtab, receiver_expr->expr_data.id) >= 0 &&
                        receiver_node != NULL && receiver_node->hash_type == HASHTYPE_TYPE)
                        receiver_is_type_ident = 1;
                }
            }

            if (is_static && receiver_is_type_ident) {
                /* For static methods, remove the first argument (the type identifier) */
                args_given = args_given->next;
                stmt->stmt_data.procedure_call_data.expr_args = args_given;
            }
        }
        
        if (need_free_class_name && class_name != NULL) {
            free((void *)class_name);
        }
    }

    
    char *type_resolution_double_underscore = (proc_id != NULL) ? strstr(proc_id, "__") : NULL;
    if (type_resolution_double_underscore != NULL && args_given != NULL &&
        !static_arg_already_removed &&
        stmt->stmt_data.procedure_call_data.mangled_id == NULL) {
        /* Extract the method name (part after __) */
        char *method_name_part = type_resolution_double_underscore + 2;
        
        /* Get the first argument (should be the object/Self parameter) */
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL) {
            /* IMPORTANT: Call semcheck_expr_main FIRST to resolve the expression type.
             * This may modify/replace first_arg->resolved_kgpc_type.
             * Only AFTER this call should we get the KgpcType, otherwise we risk
             * getting a pointer that gets freed when semcheck_expr_main updates the type.
             * (e.g., for 'as' expressions which destroy and replace resolved_kgpc_type)
             */
            int helper_tag = UNKNOWN_TYPE;
            semcheck_stmt_expr_tag(&helper_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);
            
            /* Now it's safe to get the KgpcType since semcheck_expr_main has finalized it */
            int arg_type_owned = 0;
            KgpcType *arg_type = semcheck_resolve_expression_kgpc_type(symtab, first_arg, INT_MAX, NO_MUTATE, &arg_type_owned);
            
            if (arg_type != NULL) {
                struct RecordType *obj_record_type = NULL;
                
                if (arg_type->kind == TYPE_KIND_RECORD) {
                    obj_record_type = arg_type->info.record_info;
                } else if (arg_type->kind == TYPE_KIND_POINTER && 
                           arg_type->info.points_to != NULL &&
                           arg_type->info.points_to->kind == TYPE_KIND_RECORD) {
                    obj_record_type = arg_type->info.points_to->info.record_info;
                }
                
                if (obj_record_type == NULL)
                {
                    const char *helper_name = NULL;
                    if (arg_type->kind == TYPE_KIND_PRIMITIVE)
                        helper_tag = arg_type->info.primitive_type_tag;
                    struct TypeAlias *alias = kgpc_type_get_type_alias(arg_type);
                    if (alias != NULL)
                    {
                        if (alias->target_type_id != NULL)
                            helper_name = alias->target_type_id;
                        else if (alias->alias_name != NULL)
                            helper_name = alias->alias_name;
                    }
                    struct RecordType *helper_record = semcheck_lookup_type_helper(symtab,
                        helper_tag, helper_name);
                    if (helper_record != NULL)
                        obj_record_type = helper_record;
                }
                
                if (obj_record_type != NULL) {
                    /* Found the object with a record type. Now find the class name for this type.
                     * Use the type_id stored directly on the RecordType, which is the canonical
                     * type name where methods are registered. This avoids issues with type aliases
                     * (e.g., IInterface = IUnknown) where walking the symbol table might find the
                     * alias name instead of the original type name. */
                char *correct_class_name = obj_record_type->type_id;
                
                
                if (correct_class_name != NULL) {
                    /* Walk up the inheritance chain to find the method */
                    struct RecordType *current_record = obj_record_type;
                    char *current_class_name = correct_class_name;
                    int method_found = 0;
                    
                    while (current_record != NULL && current_class_name != NULL) {
                        /* Build the mangled name for the current class */
                        size_t class_len = strlen(current_class_name);
                        size_t method_len = strlen(method_name_part);
                        char *mangled_name = (char *)malloc(class_len + 2 + method_len + 1);
                        if (mangled_name == NULL) {
                            /* Malloc failed, skip to next iteration */
                            break;
                        }
                        sprintf(mangled_name, "%s__%s", current_class_name, method_name_part);
                        
                        /* Check if this mangled name exists in the symbol table */
                        HashNode_t *proc_node = NULL;
                        if (FindIdent(&proc_node, symtab, mangled_name) != -1 && proc_node != NULL) {
                            /* Found it! Update the procedure ID */
                            free(proc_id);
                            proc_id = mangled_name;
                            stmt->stmt_data.procedure_call_data.id = proc_id;
                            /* Don't set mangled_id here - let the normal mangling process handle it */
                            method_found = 1;
                            break;
                        }
                        
                        free(mangled_name);
                        
                        /* Not found in this class, try parent */
                        if (current_record->parent_class_name != NULL) {
                            char *parent_name = current_record->parent_class_name;
                            
                            /* Look up parent class record type */
                            HashNode_t *parent_node = NULL;
                            if (FindIdent(&parent_node, symtab, parent_name) != -1 && 
                                parent_node != NULL && parent_node->type != NULL) {
                                
                                if (parent_node->type->kind == TYPE_KIND_RECORD) {
                                    current_record = parent_node->type->info.record_info;
                                } else if (parent_node->type->kind == TYPE_KIND_POINTER && 
                                           parent_node->type->info.points_to != NULL &&
                                           parent_node->type->info.points_to->kind == TYPE_KIND_RECORD) {
                                    current_record = parent_node->type->info.points_to->info.record_info;
                                } else {
                                    current_record = NULL;
                                }
                                current_class_name = parent_name;
                            } else {
                                /* Parent not found in symbol table (shouldn't happen for valid code) */
                                current_record = NULL;
                            }
                        } else {
                            /* No parent */
                            current_record = NULL;
                        }
                    }
                    
                    if (!method_found) {
                        /* If we didn't find it in the hierarchy, fallback to the original class name 
                         * so the error message makes sense (or maybe it's a virtual method that will be resolved later?)
                         * Actually, if we don't find it, we should probably leave it as is or try to construct
                         * the name for the base class to let the standard check fail with a clear message.
                         */
                         size_t class_len = strlen(correct_class_name);
                         size_t method_len = strlen(method_name_part);
                         char *mangled_name = (char *)malloc(class_len + 2 + method_len + 1);
                         if (mangled_name != NULL) {
                             sprintf(mangled_name, "%s__%s", correct_class_name, method_name_part);
                             free(proc_id);
                             proc_id = mangled_name;
                             stmt->stmt_data.procedure_call_data.id = proc_id;
                             /* Don't set mangled_id here - let the normal mangling process handle it */
                         }
                    }
                }
            }
        }
            if (arg_type_owned && arg_type != NULL)
                destroy_kgpc_type(arg_type);
        }
    }

    /* For inherited calls where mangled_id is already set, use it directly 
     * instead of re-mangling based on the call site arguments.
     * The mangled_id already includes the correct parameter signature. */
    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL) {
        mangled_name = strdup(stmt->stmt_data.procedure_call_data.mangled_id);
    } else {
        mangled_name = MangleFunctionNameFromCallSite(proc_id, args_given, symtab, INT_MAX);
    }
    assert(mangled_name != NULL);

    /* Check for procedural type typecast-call pattern: TypeName(source)(args)
     * The parser creates a call to TypeName with (source, args...) as flat arguments.
     * If TypeName is a procedural type, the first arg is the typecast source and
     * remaining args are the actual call arguments. Transform into indirect call. */
    {
        HashNode_t *type_check = NULL;
        int type_scope = FindIdent(&type_check, symtab, proc_id);
        if (type_scope != -1 && type_check != NULL &&
            type_check->hash_type == HASHTYPE_TYPE &&
            type_check->type != NULL && type_check->type->kind == TYPE_KIND_PROCEDURE &&
            args_given != NULL)
        {
            /* Count expected parameters from the procedural type */
            int expected_params = 0;
            if (type_check->type->info.proc_info.params != NULL)
            {
                for (ListNode_t *p = type_check->type->info.proc_info.params; p != NULL; p = p->next)
                    expected_params++;
            }

            /* Count actual args given */
            int actual_args = 0;
            for (ListNode_t *a = args_given; a != NULL; a = a->next)
                actual_args++;

            /* If we have exactly expected_params + 1 arguments, the first is the typecast source */
            if (actual_args == expected_params + 1)
            {
                struct Expression *typecast_source = (struct Expression *)args_given->cur;

                /* Create a typecast expression wrapping the source */
                struct Expression *typecast_expr = (struct Expression *)calloc(1, sizeof(struct Expression));
                assert(typecast_expr != NULL);
                typecast_expr->type = EXPR_TYPECAST;
                typecast_expr->line_num = stmt->line_num;
                typecast_expr->col_num = stmt->col_num;
                typecast_expr->source_index = stmt->source_index;
                typecast_expr->expr_data.typecast_data.target_type_id = strdup(proc_id);
                typecast_expr->expr_data.typecast_data.expr = typecast_source;

                /* Semcheck the typecast */
                int typecast_tag = UNKNOWN_TYPE;
                return_val += semcheck_stmt_expr_tag(&typecast_tag, symtab, typecast_expr, max_scope_lev, NO_MUTATE);

                /* Remove first list node only; the Expression* it held is now owned
                 * by typecast_expr->expr_data.typecast_data.expr, so we NULL .cur
                 * before freeing the ListNode_t shell. */
                ListNode_t *call_args = args_given->next;
                args_given->next = NULL;
                args_given->cur = NULL;
                free(args_given);  /* Free only the ListNode_t, not the Expression */
                stmt->stmt_data.procedure_call_data.expr_args = call_args;
                args_given = call_args;

                /* Set up as a procedural var call through the typecast */
                stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                stmt->stmt_data.procedure_call_data.procedural_var_expr = typecast_expr;
                stmt->stmt_data.procedure_call_data.resolved_proc = type_check;

                free(mangled_name);

                return return_val + semcheck_call_with_proc_var(symtab, stmt, type_check, max_scope_lev);
            }
        }
    }

    ListNode_t *overload_candidates = FindAllIdents(symtab, proc_id);
    HashNode_t *resolved_proc = NULL;
    int match_count = 0;

    if (overload_candidates != NULL)
    {
        ListNode_t *cur = overload_candidates;
        while(cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate->mangled_id != NULL && strcmp(candidate->mangled_id, mangled_name) == 0)
            {
                /* Found a match. For procedures registered in multiple scopes
                 * (e.g., for recursion), we may find the same mangled name multiple times.
                 * Just keep the first match - they're functionally equivalent. */
                if (resolved_proc == NULL) {
                    resolved_proc = candidate;
                }
                match_count++;
            }
            cur = cur->next;
        }
    }
    
    /* If no match found and this is a method call, try parent classes */
    if (resolved_proc == NULL && proc_id != NULL && strstr(proc_id, "__") != NULL) {
        char *double_underscore = strstr(proc_id, "__");
        if (double_underscore != NULL) {
            /* Extract class name and method name */
            size_t class_name_len = double_underscore - proc_id;
            char *class_name = (char *)malloc(class_name_len + 1);
            if (class_name != NULL) {
                memcpy(class_name, proc_id, class_name_len);
                class_name[class_name_len] = '\0';
            }
            char *method_name = strdup(double_underscore + 2);
            
            if (class_name != NULL && method_name != NULL) {
                if (getenv("KGPC_DEBUG_INHERITED") != NULL)
                {
                    fprintf(stderr, "[KGPC] Trying to resolve inherited call: class=%s method=%s\n",
                            class_name, method_name);
                }
                /* Look up the class to find its parent */
                HashNode_t *class_node = NULL;
                if (FindIdent(&class_node, symtab, class_name) != -1 && class_node != NULL) {
                    struct RecordType *record_info = semcheck_stmt_get_record_type_from_node(class_node);
                    if (record_info == NULL)
                        goto proccall_parent_resolve_done;
                    char *parent_class_name = record_info->parent_class_name;
                    
                    if (getenv("KGPC_DEBUG_INHERITED") != NULL)
                    {
                        fprintf(stderr, "[KGPC]   Found class %s, parent_class_name=%s\n",
                                class_name, parent_class_name ? parent_class_name : "<NULL>");
                    }
                    
                    /* Walk up the inheritance chain */
                    while (parent_class_name != NULL && resolved_proc == NULL) {
                        /* Try to find the method in the parent class */
                        char *parent_method_name = (char *)malloc(strlen(parent_class_name) + 2 + strlen(method_name) + 1);
                        if (parent_method_name != NULL) {
                            snprintf(parent_method_name, strlen(parent_class_name) + 2 + strlen(method_name) + 1,
                                    "%s__%s", parent_class_name, method_name);
                            
                            /* Use the same name mangling function that's used for regular method calls */
                            char *parent_mangled_name = MangleFunctionNameFromCallSite(parent_method_name, args_given, symtab, INT_MAX);
                            if (parent_mangled_name != NULL) {
                                /* Look for the parent method using the base name, then check mangled names */
                                ListNode_t *parent_candidates = FindAllIdents(symtab, parent_method_name);
                                
                                if (parent_candidates != NULL) {
                                    ListNode_t *cur = parent_candidates;
                                    while (cur != NULL) {
                                        HashNode_t *candidate = (HashNode_t *)cur->cur;
                                        if (candidate->mangled_id != NULL && strcmp(candidate->mangled_id, parent_mangled_name) == 0) {
                                            /* Found the method in parent class - use it */
                                            resolved_proc = candidate;
                                            match_count = 1;
                                            
                                            /* Use the resolved declaration id, not a mangled signature string. */
                                            if (candidate->id != NULL) {
                                                free(stmt->stmt_data.procedure_call_data.id);
                                                stmt->stmt_data.procedure_call_data.id = strdup(candidate->id);
                                                proc_id = stmt->stmt_data.procedure_call_data.id;
                                            }
                                            
                                            break;
                                        }
                                        cur = cur->next;
                                    }
                                }
                                
                                free(parent_mangled_name);
                            }
                            
                            free(parent_method_name);
                        }
                        
                        if (resolved_proc != NULL) {
                            break;  /* Found the method, stop walking up the chain */
                        }
                        
                        /* Move to the next parent class */
                        if (parent_class_name != NULL) {
                            HashNode_t *parent_class_node = NULL;
                            if (FindIdent(&parent_class_node, symtab, parent_class_name) != -1 && parent_class_node != NULL) {
                                record_info = semcheck_stmt_get_record_type_from_node(parent_class_node);
                                if (record_info == NULL)
                                    break;
                                parent_class_name = record_info->parent_class_name;
                            } else {
                                break;  /* Parent class not found, stop the chain */
                            }
                        } else {
                            break;
                        }
                    }
                }
            }
            
proccall_parent_resolve_done:
            if (class_name != NULL) free(class_name);
            if (method_name != NULL) free(method_name);
        }
    }

    /* If we found multiple matches but they all have the same mangled name,
     * treat it as a single match (they're duplicates from different scopes) */
    int force_best_match = 0;
    if (match_count > 1 && resolved_proc != NULL) {
        /* Verify all matches have the same mangled name */
        int same_mangled = 1;
        ListNode_t *cur = overload_candidates;
        while (cur != NULL && same_mangled) {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate->mangled_id != NULL && strcmp(candidate->mangled_id, mangled_name) == 0) {
                if (strcmp(candidate->mangled_id, resolved_proc->mangled_id) != 0) {
                    same_mangled = 0;
                }
            }
            cur = cur->next;
        }
        if (same_mangled) {
            match_count = 0;
            resolved_proc = NULL;
            force_best_match = 1;
        }
    }

    if (match_count == 0 && overload_candidates != NULL && !force_best_match)
    {
        HashNode_t *wildcard_proc = semcheck_find_untyped_mangled_match(overload_candidates,
            proc_id, mangled_name);
        if (wildcard_proc != NULL)
        {
            resolved_proc = wildcard_proc;
            match_count = 1;
            if (wildcard_proc->mangled_id != NULL)
            {
                free(mangled_name);
                mangled_name = strdup(wildcard_proc->mangled_id);
            }
        }
    }

    /* If no exact mangled match, choose the best overload deterministically */
    if (match_count == 0 && overload_candidates != NULL)
    {
        HashNode_t *best_candidate = NULL;
        int num_best_matches = 0;
        struct Expression call_stub;
        memset(&call_stub, 0, sizeof(call_stub));
        call_stub.line_num = stmt->line_num;
        call_stub.type = EXPR_FUNCTION_CALL;

        int overload_status = semcheck_resolve_overload(&best_candidate, NULL, &num_best_matches,
            overload_candidates, args_given, symtab, &call_stub, max_scope_lev, 0);

        if (overload_status == 0 && best_candidate != NULL && num_best_matches == 1)
        {
            resolved_proc = best_candidate;
            match_count = 1;
            if (best_candidate->mangled_id != NULL)
            {
                free(mangled_name);
                mangled_name = strdup(best_candidate->mangled_id);
            }
        }
        else if (overload_status == 2)
        {
            match_count = num_best_matches > 0 ? num_best_matches : 2;
        }
        else if (overload_status == 3)
        {
            DestroyList(overload_candidates);
            free(mangled_name);
            return ++return_val;
        }
        else
        {
            match_count = 0;
        }
    }

    if (match_count == 1 && overload_candidates != NULL && overload_candidates->next != NULL)
    {
        HashNode_t *best_candidate = NULL;
        int num_best_matches = 0;
        struct Expression call_stub;
        memset(&call_stub, 0, sizeof(call_stub));
        call_stub.line_num = stmt->line_num;
        call_stub.type = EXPR_FUNCTION_CALL;

        int overload_status = semcheck_resolve_overload(&best_candidate, NULL, &num_best_matches,
            overload_candidates, args_given, symtab, &call_stub, max_scope_lev, 0);

        if (overload_status == 0 && best_candidate != NULL && num_best_matches == 1 &&
            best_candidate != resolved_proc)
        {
            resolved_proc = best_candidate;
            if (best_candidate->mangled_id != NULL)
            {
                free(mangled_name);
                mangled_name = strdup(best_candidate->mangled_id);
            }
        }
    }

    if (match_count == 1)
    {
        if (resolved_proc->mangled_id != NULL)
            stmt->stmt_data.procedure_call_data.mangled_id = strdup(resolved_proc->mangled_id);
        else if (resolved_proc->type != NULL && resolved_proc->type->kind == TYPE_KIND_PROCEDURE)
        {
            ListNode_t *formal_params = kgpc_type_get_procedure_params(resolved_proc->type);
            if (formal_params != NULL)
                stmt->stmt_data.procedure_call_data.mangled_id =
                    MangleFunctionName(resolved_proc->id, formal_params, symtab);
        }
        else if (mangled_name != NULL)
            stmt->stmt_data.procedure_call_data.mangled_id = strdup(mangled_name);
        else
            stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        if (stmt->stmt_data.procedure_call_data.mangled_id == NULL &&
            (resolved_proc->hash_type == HASHTYPE_PROCEDURE ||
             resolved_proc->hash_type == HASHTYPE_BUILTIN_PROCEDURE ||
             resolved_proc->hash_type == HASHTYPE_FUNCTION) &&
            resolved_proc->id != NULL)
        {
            /* Ensure direct calls have a concrete target name even without external alias */
            stmt->stmt_data.procedure_call_data.mangled_id = strdup(resolved_proc->id);
        }
        /* External name override for procedures handled during codegen to avoid side-effects here */
        stmt->stmt_data.procedure_call_data.resolved_proc = resolved_proc;
        
        /* Populate call info to avoid use-after-free when HashNode is freed */
        stmt->stmt_data.procedure_call_data.call_hash_type = resolved_proc->hash_type;
        semcheck_stmt_set_call_kgpc_type(stmt, resolved_proc->type,
            stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
        stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
        semcheck_mark_call_requires_static_link(resolved_proc);
        
        /* Fill in missing arguments with default values */
        if (resolved_proc->type != NULL && resolved_proc->type->kind == TYPE_KIND_PROCEDURE)
        {
            ListNode_t *formal_params = resolved_proc->type->info.proc_info.params;
            ListNode_t *call_args = stmt->stmt_data.procedure_call_data.expr_args;
            int given_count = ListLength(call_args);
            int formal_count = ListLength(formal_params);
            
            if (given_count < formal_count)
            {
                /* Need to add default arguments */
                ListNode_t *formal_cur = formal_params;
                int arg_index = 0;
                
                /* Skip to the position after the last given argument */
                while (arg_index < given_count && formal_cur != NULL)
                {
                    arg_index++;
                    formal_cur = formal_cur->next;
                }
                
                /* For each remaining formal parameter, add its default value */
                ListNode_t *args_tail = call_args;
                while (args_tail != NULL && args_tail->next != NULL)
                    args_tail = args_tail->next;
                
                while (formal_cur != NULL)
                {
                    Tree_t *param_decl = (Tree_t *)formal_cur->cur;
                    struct Expression *default_expr = get_param_default_value_stmt(param_decl);
                    
                    if (default_expr != NULL)
                    {
                        struct Expression *copy = copy_default_expr(default_expr);
                        if (copy != NULL)
                        {
                            ListNode_t *new_arg = CreateListNode(copy, LIST_EXPR);
                            if (args_tail != NULL)
                            {
                                args_tail->next = new_arg;
                                args_tail = new_arg;
                            }
                            else
                            {
                                /* No arguments given, start the list */
                                stmt->stmt_data.procedure_call_data.expr_args = new_arg;
                                args_tail = new_arg;
                            }
                            
                            if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
                                fprintf(stderr, "[SemCheck] Added default arg %d for %s\n", 
                                    arg_index, proc_id != NULL ? proc_id : "(null)");
                            }
                        }
                        else
                        {
                            semcheck_error_with_context(
                                "Error on line %d, could not copy default value for parameter %d of %s.\n\n",
                                stmt->line_num,
                                arg_index,
                                proc_id != NULL ? proc_id : "(null)");
                            return_val++;
                        }
                    }
                    
                    arg_index++;
                    formal_cur = formal_cur->next;
                }
            }
        }
        
        sym_return = resolved_proc;
        scope_return = 0; // FIXME: This needs to be properly calculated
    }
    else if (match_count == 0)
    {
        HashNode_t *proc_var = NULL;
        int proc_scope = FindIdent(&proc_var, symtab, proc_id);
        /* Check for procedure variables (HASHTYPE_VAR) or procedure constants (HASHTYPE_CONST)
         * with a procedural type. This allows calling procedure variables and typed constants
         * that hold procedure addresses like: const MyProcRef: TProc = @MyProc; */
        if (proc_scope != -1 && proc_var != NULL && 
            (proc_var->hash_type == HASHTYPE_VAR || proc_var->hash_type == HASHTYPE_CONST) &&
            proc_var->type != NULL && proc_var->type->kind == TYPE_KIND_PROCEDURE)
        {
            DestroyList(overload_candidates);
            free(mangled_name);

            proc_var->referenced += 1;
            if (proc_scope > max_scope_lev)
            {
                semcheck_error_with_context("Error on line %d, %s cannot be called in the current context!\n\n",
                    stmt->line_num, proc_id);
                return_val++;
                return return_val;
            }

            /* Set the resolved_proc field so codegen knows this is an indirect call */
            stmt->stmt_data.procedure_call_data.resolved_proc = proc_var;
            
            /* Populate call info to avoid use-after-free when HashNode is freed */
            stmt->stmt_data.procedure_call_data.call_hash_type = proc_var->hash_type;
            semcheck_stmt_set_call_kgpc_type(stmt, proc_var->type,
                stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
            stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;

            return return_val + semcheck_call_with_proc_var(symtab, stmt, proc_var, max_scope_lev);
        }


        /* Build detailed error message with argument types and available overloads */
        {
            /* First, build a string showing the actual argument types */
            char arg_types_buf[1024] = "(";
            int buf_pos = 1;
            if (args_given != NULL)
            {
                int idx = 0;
                for (ListNode_t *cur = args_given; cur != NULL; cur = cur->next)
                {
                    struct Expression *arg = (struct Expression *)cur->cur;
                    int tag = UNKNOWN_TYPE;
                    semcheck_stmt_expr_tag(&tag, symtab, arg, max_scope_lev, NO_MUTATE);
                    const char *type_name = semcheck_type_tag_name(tag);
                    
                    /* Also check for resolved_kgpc_type for better type info */
                    if (arg != NULL && arg->resolved_kgpc_type != NULL)
                    {
                        const char *kgpc_str = kgpc_type_to_string(arg->resolved_kgpc_type);
                        if (kgpc_str != NULL && kgpc_str[0] != '\0')
                            type_name = kgpc_str;
                    }
                    
                    if (idx > 0 && buf_pos < (int)sizeof(arg_types_buf) - 3)
                    {
                        arg_types_buf[buf_pos++] = ',';
                        arg_types_buf[buf_pos++] = ' ';
                    }
                    int remaining = (int)sizeof(arg_types_buf) - buf_pos - 1;
                    if (remaining > 0)
                    {
                        int written = snprintf(arg_types_buf + buf_pos, remaining, "%s", type_name);
                        if (written > 0)
                            buf_pos += (written < remaining) ? written : remaining - 1;
                    }
                    idx++;
                }
            }
            if (buf_pos < (int)sizeof(arg_types_buf) - 1)
                arg_types_buf[buf_pos++] = ')';
            arg_types_buf[buf_pos] = '\0';
            
            /* Now build a string showing available overloads */
            char overloads_buf[2048] = "";
            int ovl_pos = 0;
            if (overload_candidates != NULL)
            {
                for (ListNode_t *cur = overload_candidates; cur != NULL; cur = cur->next)
                {
                    HashNode_t *cand = (HashNode_t *)cur->cur;
                    if (cand != NULL && cand->type != NULL &&
                        (cand->hash_type == HASHTYPE_FUNCTION ||
                         cand->hash_type == HASHTYPE_PROCEDURE))
                    {
                        int remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                        if (remaining <= 0) break;
                        
                        /* Format: "  - procedure_name(param_types)" */
                        int written = snprintf(overloads_buf + ovl_pos, remaining, "  - %s(",
                            cand->id ? cand->id : "<anonymous>");
                        if (written > 0) ovl_pos += (written < remaining) ? written : remaining - 1;
                        
                        /* Add parameter types */
                        ListNode_t *params = kgpc_type_get_procedure_params(cand->type);
                        int param_idx = 0;
                        for (ListNode_t *p = params; p != NULL; p = p->next)
                        {
                            Tree_t *param = (Tree_t *)p->cur;
                            if (param != NULL)
                            {
                                remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                                if (remaining <= 0) break;
                                
                                if (param_idx > 0)
                                {
                                    written = snprintf(overloads_buf + ovl_pos, remaining, ", ");
                                    if (written > 0) ovl_pos += (written < remaining) ? written : remaining - 1;
                                    remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                                }
                                
                                const char *param_type_str = "?";
                                if (param->tree_data.var_decl_data.cached_kgpc_type != NULL)
                                    param_type_str = kgpc_type_to_string(param->tree_data.var_decl_data.cached_kgpc_type);
                                else if (param->tree_data.var_decl_data.type_id != NULL)
                                    param_type_str = param->tree_data.var_decl_data.type_id;
                                else if (param->tree_data.var_decl_data.type != UNKNOWN_TYPE)
                                    param_type_str = semcheck_type_tag_name(param->tree_data.var_decl_data.type);
                                
                                written = snprintf(overloads_buf + ovl_pos, remaining, "%s", param_type_str);
                                if (written > 0) ovl_pos += (written < remaining) ? written : remaining - 1;
                                param_idx++;
                            }
                        }
                        
                        remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                        if (remaining > 0)
                        {
                            written = snprintf(overloads_buf + ovl_pos, remaining, ")\n");
                            if (written > 0) ovl_pos += (written < remaining) ? written : remaining - 1;
                        }
                    }
                }
            }
            
            if (overloads_buf[0] != '\0')
            {
                semcheck_error_with_context(
                    "Error on line %d, call to procedure %s%s does not match any available overload.\n"
                    "Available overloads:\n%s",
                    stmt->line_num, proc_id, arg_types_buf, overloads_buf);
            }
            else
            {
                /* No overloads found - procedure is not declared */
                semcheck_error_with_context(
                    "Error on line %d, procedure %s%s is not declared.\n",
                    stmt->line_num, proc_id, arg_types_buf);
            }
        }
        DestroyList(overload_candidates);
        free(mangled_name);
        return ++return_val;
    }
    else
    {
        semcheck_error_with_context("Error on line %d, call to procedure %s is ambiguous\n", stmt->line_num, proc_id);
        DestroyList(overload_candidates);
        free(mangled_name);
        return ++return_val;
    }
    DestroyList(overload_candidates);
    free(mangled_name);

    if(scope_return == -1) // Should not happen if match_count > 0
    {
        semcheck_error_with_context("Error on line %d, unrecognized procedure call %s\n", stmt->line_num,
            proc_id);
        ++return_val;
    }
    else
    {
        sym_return->referenced += 1; /* Moved here: only access if sym_return is valid */

        if (sym_return->type != NULL && sym_return->type->kind == TYPE_KIND_PROCEDURE)
        {
            ListNode_t *formal_params = kgpc_type_get_procedure_params(sym_return->type);
            if (append_default_args(&args_given, formal_params, stmt->line_num) != 0)
                ++return_val;
            stmt->stmt_data.procedure_call_data.expr_args = args_given;
        }

        if(scope_return > max_scope_lev)
        {
            semcheck_error_with_context("Error on line %d, %s cannot be called in the current context!\n\n",
                stmt->line_num, proc_id);
            fprintf(stderr, "[Was it defined above the current function context?]\n");

            ++return_val;
        }
        if(sym_return->hash_type != HASHTYPE_PROCEDURE &&
            sym_return->hash_type != HASHTYPE_BUILTIN_PROCEDURE &&
            sym_return->hash_type != HASHTYPE_FUNCTION)
        {
            semcheck_error_with_context("Error on line %d, expected %s to be a procedure, function, or builtin!\n\n",
                stmt->line_num, proc_id);

            ++return_val;
        }

        /***** VERIFY ARGUMENTS USING KGPCTYPE ARCHITECTURE *****/
        cur_arg = 0;
        /* Get formal arguments from KgpcType instead of deprecated args field */
        true_args = kgpc_type_get_procedure_params(sym_return->type);
        while(args_given != NULL && true_args != NULL)
        {
            ++cur_arg;
            assert(args_given->type == LIST_EXPR);
            assert(true_args->type == LIST_TREE);
            
            arg_decl = (Tree_t *)true_args->cur;
            assert(arg_decl->type == TREE_VAR_DECL || arg_decl->type == TREE_ARR_DECL);
            true_arg_ids = (arg_decl->type == TREE_VAR_DECL) ? 
                arg_decl->tree_data.var_decl_data.ids : 
                arg_decl->tree_data.arr_decl_data.ids;

            while(true_arg_ids != NULL && args_given != NULL)
            {
                struct Expression *arg_expr = (struct Expression *)args_given->cur;

                if (arg_decl != NULL && arg_decl->type == TREE_ARR_DECL)
                {
                    if (semcheck_prepare_array_literal_argument(arg_decl, arg_expr,
                            symtab, INT_MAX, stmt->line_num) != 0)
                    {
                        ++return_val;
                        args_given = args_given->next;
                        true_arg_ids = true_arg_ids->next;
                        continue;
                    }
                }
                if (semcheck_prepare_record_constructor_argument(arg_decl, arg_expr,
                        symtab, INT_MAX, stmt->line_num) != 0)
                {
                    ++return_val;
                    args_given = args_given->next;
                    true_arg_ids = true_arg_ids->next;
                    continue;
                }
                
                /* ALWAYS resolve both sides to KgpcType for proper type checking */
                int expected_type_owned = 0;
                KgpcType *expected_kgpc_type = resolve_type_from_vardecl(arg_decl, symtab, &expected_type_owned);
                if (getenv("KGPC_DEBUG_FMTSTR") != NULL && proc_id != NULL &&
                    strcasecmp(proc_id, "FmtStr") == 0)
                {
                    if (arg_decl->type == TREE_VAR_DECL)
                    {
                        fprintf(stderr,
                            "[KGPC_DEBUG_FMTSTR] param %d VAR_DECL type=%d type_id=%s\n",
                            cur_arg,
                            arg_decl->tree_data.var_decl_data.type,
                            arg_decl->tree_data.var_decl_data.type_id ?
                                arg_decl->tree_data.var_decl_data.type_id : "<null>");
                    }
                    else if (arg_decl->type == TREE_ARR_DECL)
                    {
                        fprintf(stderr,
                            "[KGPC_DEBUG_FMTSTR] param %d ARR_DECL elem_type=%d elem_type_id=%s\n",
                            cur_arg,
                            arg_decl->tree_data.arr_decl_data.type,
                            arg_decl->tree_data.arr_decl_data.type_id ?
                                arg_decl->tree_data.arr_decl_data.type_id : "<null>");
                    }
                }
                
                /* For var/out parameters, we need to mark the argument as mutated.
                 * This is important for tracking whether Result was assigned in a function. */
                int param_is_var_out = (arg_decl->type == TREE_VAR_DECL &&
                                        arg_decl->tree_data.var_decl_data.is_var_param);
                int mutate_flag = param_is_var_out ? MUTATE : NO_MUTATE;
                
                /* Call semcheck_expr_main to properly mark the variable as mutated */
                int dummy_type = UNKNOWN_TYPE;
                semcheck_stmt_expr_tag(&dummy_type, symtab, arg_expr, INT_MAX, mutate_flag);
                
                int arg_type_owned = 0;
                KgpcType *arg_kgpc_type = NULL;
                if (arg_expr != NULL && arg_expr->type == EXPR_ARRAY_LITERAL)
                {
                    arg_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, arg_expr, INT_MAX, mutate_flag, &arg_type_owned);
                }
                else if (arg_expr != NULL && arg_expr->resolved_kgpc_type != NULL)
                {
                    arg_kgpc_type = arg_expr->resolved_kgpc_type;
                    arg_type_owned = 0;
                }
                else
                {
                    arg_type_owned = 0;
                    semcheck_expr_main(symtab, arg_expr, INT_MAX, mutate_flag, &arg_kgpc_type);
                }
                if (arg_kgpc_type == NULL)
                {
                    arg_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, arg_expr, INT_MAX, mutate_flag, &arg_type_owned);
                }
                if (getenv("KGPC_DEBUG_FMTSTR") != NULL && proc_id != NULL &&
                    strcasecmp(proc_id, "FmtStr") == 0)
                {
                    fprintf(stderr,
                        "[KGPC_DEBUG_FMTSTR] param %d expected=%s arg=%s\n",
                        cur_arg,
                        expected_kgpc_type ? kgpc_type_to_string(expected_kgpc_type) : "<null>",
                        arg_kgpc_type ? kgpc_type_to_string(arg_kgpc_type) : "<null>");
                }
                int param_is_untyped = semcheck_var_decl_is_untyped(arg_decl);

                /* Perform type compatibility check using KgpcType */
                int types_match = param_is_untyped ? 1 : 0;
                if ((expected_kgpc_type == NULL || arg_kgpc_type == NULL) && !param_is_untyped)
                {
                    /* Fallback: if we can't get KgpcTypes, report error */
                    semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                        "Error on line %d, on procedure call %s, argument %d: Unable to resolve types (expected=%p, arg=%p)!\n\n",
                        stmt->line_num, proc_id, cur_arg, (void*)expected_kgpc_type, (void*)arg_kgpc_type);
                    if (expected_kgpc_type != NULL)
                        fprintf(stderr, "  Expected type: %s\n", kgpc_type_to_string(expected_kgpc_type));
                    if (arg_kgpc_type != NULL)
                        fprintf(stderr, "  Argument type: %s\n", kgpc_type_to_string(arg_kgpc_type));
                    ++return_val;
                }
                else if (!param_is_untyped)
                {
                    types_match = are_types_compatible_for_assignment(expected_kgpc_type, arg_kgpc_type, symtab);
                    if (!types_match && !param_is_var_out && expected_kgpc_type != NULL &&
                        arg_kgpc_type != NULL && arg_expr != NULL)
                    {
                        if (semcheck_try_record_conversion_expression(symtab, &arg_expr, NULL,
                                expected_kgpc_type, &arg_kgpc_type, &arg_type_owned))
                        {
                            args_given->cur = arg_expr;
                            types_match = are_types_compatible_for_assignment(
                                expected_kgpc_type, arg_kgpc_type, symtab);
                        }
                    }
                    
                    /* Special AST transformation for procedure parameters */
                    if (types_match && 
                        expected_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
                        arg_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
                        arg_expr != NULL && arg_expr->type == EXPR_VAR_ID)
                    {
                        HashNode_t *arg_node = NULL;
                        if (FindIdent(&arg_node, symtab, arg_expr->expr_data.id) != -1 &&
                            arg_node != NULL && arg_node->hash_type == HASHTYPE_PROCEDURE)
                        {
                            /* Transform the expression to EXPR_ADDR_OF_PROC */
                            arg_expr->type = EXPR_ADDR_OF_PROC;
                            arg_expr->expr_data.addr_of_proc_data.procedure_symbol = arg_node;
                        }
                    }
                }

                /* Save type strings before cleanup for error message */
                char expected_type_str[256] = "<unknown>";
                char given_type_str[256] = "<unknown>";
                if (expected_kgpc_type != NULL)
                    snprintf(expected_type_str, sizeof(expected_type_str), "%s", kgpc_type_to_string(expected_kgpc_type));
                if (arg_kgpc_type != NULL)
                    snprintf(given_type_str, sizeof(given_type_str), "%s", kgpc_type_to_string(arg_kgpc_type));

                /* Clean up owned types */
                if (expected_type_owned && expected_kgpc_type != NULL)
                    destroy_kgpc_type(expected_kgpc_type);
                if (arg_type_owned && arg_kgpc_type != NULL)
                    destroy_kgpc_type(arg_kgpc_type);

                if (!types_match)
                {
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    {
                        fprintf(stderr,
                            "[SemCheck] proccall %s arg %d mismatch: expected=%s actual=%s\n",
                            proc_id ? proc_id : "<null>",
                            cur_arg,
                            expected_type_str,
                            given_type_str);
                    }
                    semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                        "Error on line %d, on procedure call %s, argument %d: Type mismatch (expected: %s, given: %s)!\n\n",
                        stmt->line_num, proc_id, cur_arg, expected_type_str, given_type_str);
                    ++return_val;
                }

                args_given = args_given->next;
                true_arg_ids = true_arg_ids->next;
            }

            true_args = true_args->next;
        }

        /* Verify arg counts match up */
        if(true_args == NULL && args_given != NULL)
        {
            int allow_implicit_self_only = 0;
            if (args_given->next == NULL)
            {
                struct Expression *only_arg = (struct Expression *)args_given->cur;
                if (only_arg != NULL &&
                    ((only_arg->type == EXPR_VAR_ID &&
                      only_arg->expr_data.id != NULL &&
                      pascal_identifier_equals(only_arg->expr_data.id, "Self")) ||
                     only_arg->type == EXPR_NIL))
                {
                    allow_implicit_self_only = 1;
                }
            }
            if (allow_implicit_self_only)
                args_given = NULL;
        }
        if(true_args == NULL && args_given != NULL)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                "Error on line %d, on procedure call %s, too many arguments given!\n\n",
                stmt->line_num, proc_id);
            ++return_val;
        }
        else if(true_args != NULL && args_given == NULL)
        {
            /* Check if all remaining parameters have default values */
            int all_have_defaults = 1;
            ListNode_t *remaining = true_args;
            while (remaining != NULL)
            {
                Tree_t *decl = (Tree_t *)remaining->cur;
                if (!param_has_default_value(decl))
                {
                    all_have_defaults = 0;
                    break;
                }
                remaining = remaining->next;
            }
            
            if (!all_have_defaults)
            {
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                    "Error on line %d, on procedure call %s, not enough arguments given!\n\n",
                    stmt->line_num, proc_id);
                ++return_val;
            }
        }
    }

    return return_val;
}

/** COMPOUNT_STMT **/
int semcheck_compoundstmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;
    ListNode_t *stmt_list;
    ListNode_t *slow = NULL;
    ListNode_t *fast = NULL;
    int guard = 0;
    const int guard_limit = 100000;
    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_COMPOUND_STATEMENT);

    return_val = 0;
    stmt_list = stmt->stmt_data.compound_statement;
    slow = stmt_list;
    fast = stmt_list;
    while (stmt_list != NULL)
    {
        assert(stmt_list->type == LIST_STMT);
        guard++;
        if (guard > guard_limit) {
            fprintf(stderr, "ERROR: semcheck_compoundstmt exceeded guard limit (%d); possible cycle in stmt list (node=%p).\n",
                    guard_limit, (void*)stmt_list);
            break;
        }
        if (fast != NULL && fast->next != NULL) {
            fast = fast->next->next;
            slow = slow ? slow->next : NULL;
            if (fast != NULL && slow == fast) {
                fprintf(stderr, "ERROR: Cycle detected in compound statement list (node=%p).\n",
                        (void*)stmt_list);
                break;
            }
        }

        if (stmt_list->cur != NULL)
        {
            return_val += semcheck_stmt_main(symtab,
                (struct Statement *)stmt_list->cur, max_scope_lev);
        }

        stmt_list = stmt_list->next;
    }

    if (g_debug_watch_stmt != NULL) {
        if (g_debug_watch_stmt->stmt_data.for_data.to != g_debug_watch_to_expr) {
            fprintf(stderr, "CRITICAL: g_debug_watch_stmt corrupted at end of compoundstmt! Changed from %p to %p\n",
                    g_debug_watch_to_expr, g_debug_watch_stmt->stmt_data.for_data.to);
        } else {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: g_debug_watch_stmt OK at end of compoundstmt. to=%p\n", g_debug_watch_stmt->stmt_data.for_data.to);
#endif
        }
    }

    return return_val;
}

/** IF_THEN **/
int semcheck_ifthen(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;
    int if_type;
    struct Expression *relop_expr;
    struct Statement *if_stmt, *else_stmt;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_IF_THEN);

    return_val = 0;
    relop_expr = stmt->stmt_data.if_then_data.relop_expr;
    if_stmt = stmt->stmt_data.if_then_data.if_stmt;
    else_stmt = stmt->stmt_data.if_then_data.else_stmt;

    return_val += semcheck_stmt_expr_tag(&if_type, symtab, relop_expr, INT_MAX, NO_MUTATE);

    if(if_type != BOOL)
    {
        semcheck_error_with_context("Error on line %d, expected relational inside if statement!\n\n",
                stmt->line_num);
        ++return_val;
    }

    return_val += semcheck_stmt_main(symtab, if_stmt, max_scope_lev);
    if(else_stmt != NULL)
        return_val += semcheck_stmt_main(symtab, else_stmt, max_scope_lev);

    return return_val;
}

/** WHILE **/
int semcheck_while(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;
    int while_type;
    struct Expression *relop_expr;
    struct Statement *while_stmt;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_WHILE);

    return_val = 0;
    relop_expr = stmt->stmt_data.while_data.relop_expr;
    while_stmt = stmt->stmt_data.while_data.while_stmt;

    return_val += semcheck_stmt_expr_tag(&while_type, symtab, relop_expr, INT_MAX, NO_MUTATE);
    if(while_type != BOOL)
    {
        semcheck_error_with_context("Error on line %d, expected relational inside while statement!\n\n",
                stmt->line_num);
        ++return_val;
    }

    semcheck_loop_depth++;
    return_val += semcheck_stmt_main(symtab, while_stmt, max_scope_lev);
    semcheck_loop_depth--;

    return return_val;
}

/** REPEAT **/
int semcheck_repeat(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    int until_type = UNKNOWN_TYPE;
    ListNode_t *body_list;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_REPEAT);

    body_list = stmt->stmt_data.repeat_data.body_list;
    semcheck_loop_depth++;
    while (body_list != NULL)
    {
        struct Statement *body_stmt = (struct Statement *)body_list->cur;
        if (body_stmt != NULL)
            return_val += semcheck_stmt_main(symtab, body_stmt, max_scope_lev);
        body_list = body_list->next;
    }
    semcheck_loop_depth--;

    return_val += semcheck_stmt_expr_tag(&until_type, symtab, stmt->stmt_data.repeat_data.until_expr, INT_MAX, NO_MUTATE);
    if (until_type != BOOL)
    {
        semcheck_error_with_context("Error on line %d, expected relational inside repeat statement!\n\n",
            stmt->line_num);
        ++return_val;
    }

    return return_val;
}

/** FOR **/
int semcheck_for(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;
    int for_type, to_type;
    enum StmtType for_assign_type; /* Either var or var_assign */
    struct Statement *for_assign;
    struct Expression *for_var;

    struct Expression *to_expr;
    struct Statement *do_for;
    int for_type_owned = 0;
    int to_type_owned = 0;
    KgpcType *for_kgpc_type = NULL;
    KgpcType *to_kgpc_type = NULL;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_FOR);

    for_assign_type = stmt->stmt_data.for_data.for_assign_type;
    assert(for_assign_type == STMT_FOR_VAR || for_assign_type == STMT_FOR_ASSIGN_VAR);

    return_val = 0;
    for_var = NULL;
    if(for_assign_type == STMT_FOR_VAR)
    {
        for_var = stmt->stmt_data.for_data.for_assign_data.var;
        return_val += semcheck_stmt_expr_tag(&for_type, symtab, for_var, max_scope_lev, BOTH_MUTATE_REFERENCE);
        /* Check for type */
        if(!is_ordinal_type(for_type))
        {
            semcheck_error_with_context("Error on line %d, expected ordinal type in \"for\" assignment!\n\n",
                    stmt->line_num);
            ++return_val;
        }
    }
    else
    {
        for_assign = stmt->stmt_data.for_data.for_assign_data.var_assign;
        /* For type checked in here */
        return_val += semcheck_for_assign(symtab, for_assign, max_scope_lev);
        for_var = NULL;
        if (for_assign != NULL)
        {
            for_var = for_assign->stmt_data.var_assign_data.var;
            for_type = (for_var != NULL) ? semcheck_tag_from_kgpc(for_var->resolved_kgpc_type) : UNKNOWN_TYPE;
        }
    }


    to_expr = stmt->stmt_data.for_data.to;
    do_for = stmt->stmt_data.for_data.do_for;

    if (for_var != NULL)
    {
        for_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, for_var,
            max_scope_lev, BOTH_MUTATE_REFERENCE, &for_type_owned);
        if (for_type == UNKNOWN_TYPE && for_kgpc_type != NULL)
            for_type = semcheck_tag_from_kgpc(for_kgpc_type);
    }

    return_val += semcheck_stmt_expr_tag(&to_type, symtab, to_expr, INT_MAX, NO_MUTATE);
    to_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, to_expr,
        INT_MAX, NO_MUTATE, &to_type_owned);
    if (to_type == UNKNOWN_TYPE && to_kgpc_type != NULL)
        to_type = semcheck_tag_from_kgpc(to_kgpc_type);

    int bounds_compatible = 1;
    if (for_type == UNKNOWN_TYPE && for_kgpc_type == NULL)
        bounds_compatible = 0;

    if (bounds_compatible)
    {
        if (for_type == to_type)
        {
            /* ok */
        }
        else if ((for_type == LONGINT_TYPE && to_type == INT_TYPE) ||
            (for_type == INT_TYPE && to_type == LONGINT_TYPE))
        {
            /* ok */
        }
        else if (for_type == CHAR_TYPE && to_type == STRING_TYPE &&
            to_expr != NULL && to_expr->type == EXPR_STRING &&
            to_expr->expr_data.string != NULL && strlen(to_expr->expr_data.string) == 1)
        {
            to_type = CHAR_TYPE;
            semcheck_expr_set_resolved_type(to_expr, CHAR_TYPE);
        }
        else if (for_kgpc_type != NULL && to_kgpc_type != NULL &&
            are_types_compatible_for_assignment(for_kgpc_type, to_kgpc_type, symtab))
        {
            /* ok */
        }
        else
        {
            bounds_compatible = 0;
        }
    }

    if (!bounds_compatible)
    {
        semcheck_error_with_context("Error on line %d, type mismatch in \"to\" assignment!\n\n",
                stmt->line_num);
        ++return_val;
    }

    if (for_kgpc_type != NULL && !is_ordinal_type(for_type))
    {
        int legacy = semcheck_tag_from_kgpc(for_kgpc_type);
        if (!is_ordinal_type(legacy))
        {
            semcheck_error_with_context("Error on line %d, expected ordinal type in \"for\" assignment!\n\n",
                stmt->line_num);
            ++return_val;
        }
    }

    semcheck_loop_depth++;
    
    if (stmt->line_num == 42) {
        g_debug_watch_stmt = stmt;
        g_debug_watch_to_expr = stmt->stmt_data.for_data.to;
#ifdef DEBUG
        fprintf(stderr, "DEBUG: Watching stmt at line 42\n");
#endif
    }

    if (to_expr != NULL && ((uintptr_t)to_expr == 0x686374616d || (uintptr_t)to_expr == 0x1db2)) {
        fprintf(stderr, "CRITICAL: to_expr is corrupted in semcheck_for!\n");
    }
    
    return_val += semcheck_stmt_main(symtab, do_for, max_scope_lev);
    semcheck_loop_depth--;

    if (stmt->stmt_data.for_data.to != to_expr) {
        fprintf(stderr, "CRITICAL: stmt->stmt_data.for_data.to changed from %p to %p during body processing!\n",
                to_expr, stmt->stmt_data.for_data.to);
    }

    if (for_type_owned && for_kgpc_type != NULL)
        destroy_kgpc_type(for_kgpc_type);
    if (to_type_owned && to_kgpc_type != NULL)
        destroy_kgpc_type(to_kgpc_type);

    return return_val;
}

/** FOR-IN **/
int semcheck_for_in(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    int loop_var_type, collection_type;
    int loop_var_nonordinal = 0;
    
    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_FOR_IN);
    
    struct Expression *loop_var = stmt->stmt_data.for_in_data.loop_var;
    struct Expression *collection = stmt->stmt_data.for_in_data.collection;
    struct Statement *do_stmt = stmt->stmt_data.for_in_data.do_stmt;
    
    /* Check loop variable (must be a lvalue) */
    if (loop_var != NULL) {
        return_val += semcheck_stmt_expr_tag(&loop_var_type, symtab, loop_var, max_scope_lev, BOTH_MUTATE_REFERENCE);
        
        if (!is_ordinal_type(loop_var_type) && loop_var_type != UNKNOWN_TYPE) {
            loop_var_nonordinal = 1;
        }
    } else {
        loop_var_type = UNKNOWN_TYPE;
    }
    
    /* Check collection expression */
    if (collection != NULL) {
        int collection_type_owned = 0;
        int collection_is_array = 0;
        int collection_is_list = 0;
        int collection_is_string = 0;
        const char *list_element_id = NULL;

        return_val += semcheck_stmt_expr_tag(&collection_type, symtab, collection, INT_MAX, NO_MUTATE);
        collection_is_string = is_string_type(collection_type);
        
        KgpcType *collection_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, collection, 
                                                                            INT_MAX, NO_MUTATE, &collection_type_owned);
        if (collection_kgpc_type != NULL) {
            if (kgpc_type_is_array(collection_kgpc_type)) {
                collection_is_array = 1;
            } else {
                /* Lists are represented as pointers to class records */
                KgpcType *record_candidate = collection_kgpc_type;
                if (kgpc_type_is_pointer(collection_kgpc_type))
                    record_candidate = collection_kgpc_type->info.points_to;

                if (record_candidate != NULL && kgpc_type_is_record(record_candidate)) {
                    struct RecordType *record_info = kgpc_type_get_record(record_candidate);
                    if (record_info != NULL) {
                        /* Check for TFPGList$ pattern (generic list) - extracts element type from the mangled name.
                         * This is kept separate from default_indexed_property because TFPGList$ encodes the 
                         * element type in its class name (e.g., TFPGList$String for list of strings). */
                        if (record_info->type_id != NULL) {
                            const char *prefix = "TFPGList$";
                            size_t prefix_len = strlen(prefix);
                            if (strncasecmp(record_info->type_id, prefix, prefix_len) == 0) {
                                collection_is_list = 1;
                                list_element_id = record_info->type_id + prefix_len;
                            }
                        }
                        /* Check for default indexed property (handles TStringList and other classes with FItems) */
                        if (!collection_is_list && record_info->default_indexed_property != NULL) {
                            collection_is_list = 1;
                            list_element_id = record_info->default_indexed_element_type_id;
                        }
                    }
                }
            }
        }

        if (collection_is_string)
            collection_is_array = 1;

        if (!collection_is_array && !collection_is_list) {
            semcheck_error_with_context("Error on line %d: for-in loop requires an array expression!\n\n",
                    stmt->line_num);
            ++return_val;
        } else if (!collection_is_list && loop_var_nonordinal) {
            int loop_var_type_owned = 0;
            KgpcType *loop_var_kgpc = semcheck_resolve_expression_kgpc_type(symtab, loop_var,
                max_scope_lev, MUTATE, &loop_var_type_owned);
            KgpcType *element_kgpc = NULL;

            if (collection_is_string)
            {
                element_kgpc = create_primitive_type(CHAR_TYPE);
            }
            else if (collection_kgpc_type != NULL && kgpc_type_is_array(collection_kgpc_type))
            {
                element_kgpc = kgpc_type_get_array_element_type(collection_kgpc_type);
                if (element_kgpc != NULL)
                    kgpc_type_retain(element_kgpc);
            }

            if (loop_var_kgpc != NULL && element_kgpc != NULL &&
                kgpc_type_equals(loop_var_kgpc, element_kgpc))
            {
                loop_var_nonordinal = 0;
            }
            else
            {
                semcheck_error_with_context("Error on line %d: for-in loop variable must be an ordinal type!\n\n",
                        stmt->line_num);
                ++return_val;
            }

            if (loop_var_type_owned && loop_var_kgpc != NULL)
                destroy_kgpc_type(loop_var_kgpc);
            if (element_kgpc != NULL)
                destroy_kgpc_type(element_kgpc);
        }

        if (collection_type_owned && collection_kgpc_type != NULL)
            destroy_kgpc_type(collection_kgpc_type);
        (void)list_element_id;
    } else {
        collection_type = UNKNOWN_TYPE;
    }
    
    /* Check body statement */
    if (do_stmt != NULL) {
        semcheck_loop_depth++;
        return_val += semcheck_stmt(symtab, do_stmt, max_scope_lev);
        semcheck_loop_depth--;
    }
    
    return return_val;
}

/* Essentially the same as the var assignment but with a restriction that it must be an int */
int semcheck_for_assign(SymTab_t *symtab, struct Statement *for_assign, int max_scope_lev)
{
    int return_val;
    int type_first, type_second;
    struct Expression *var, *expr;
    int lhs_owned = 0;
    int rhs_owned = 0;
    KgpcType *lhs_kgpc_type = NULL;
    KgpcType *rhs_kgpc_type = NULL;

    assert(symtab != NULL);
    assert(for_assign != NULL);
    assert(for_assign->type == STMT_VAR_ASSIGN);

    return_val = 0;

    var = for_assign->stmt_data.var_assign_data.var;
    expr = for_assign->stmt_data.var_assign_data.expr;

    /* NOTE: Grammar will make sure the left side is a variable */
    return_val += semcheck_stmt_expr_tag(&type_first, symtab, var, max_scope_lev, BOTH_MUTATE_REFERENCE);
    return_val += semcheck_stmt_expr_tag(&type_second, symtab, expr, INT_MAX, NO_MUTATE);

    lhs_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, var, max_scope_lev,
        BOTH_MUTATE_REFERENCE, &lhs_owned);
    rhs_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, expr, INT_MAX,
        NO_MUTATE, &rhs_owned);

    if (type_first == UNKNOWN_TYPE && lhs_kgpc_type != NULL)
        type_first = semcheck_tag_from_kgpc(lhs_kgpc_type);
    if (type_second == UNKNOWN_TYPE && rhs_kgpc_type != NULL)
        type_second = semcheck_tag_from_kgpc(rhs_kgpc_type);

    int types_compatible = (type_first == type_second);
    if (!types_compatible)
    {
        if ((type_first == LONGINT_TYPE && type_second == INT_TYPE) ||
            (type_first == INT_TYPE && type_second == LONGINT_TYPE))
        {
            types_compatible = 1;
        }
        else if (type_first == CHAR_TYPE && type_second == STRING_TYPE &&
            expr != NULL && expr->type == EXPR_STRING &&
            expr->expr_data.string != NULL && strlen(expr->expr_data.string) == 1)
        {
            types_compatible = 1;
            type_second = CHAR_TYPE;
            semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
        }
        else if (lhs_kgpc_type != NULL && rhs_kgpc_type != NULL &&
            are_types_compatible_for_assignment(lhs_kgpc_type, rhs_kgpc_type, symtab))
        {
            types_compatible = 1;
        }
    }

    if (!types_compatible)
    {
        semcheck_error_with_context("Error on line %d, type mismatch in \"for\" assignment statement!\n\n",
            for_assign->line_num);
        ++return_val;
    }

    if (!is_ordinal_type(type_first))
    {
        semcheck_error_with_context("Error on line %d, expected ordinal type in \"for\" assignment statement!\n\n",
            for_assign->line_num);
        ++return_val;
    }

    if (return_val == 0)
    {
        semcheck_expr_set_resolved_type(var, type_first);
        semcheck_expr_set_resolved_type(expr, type_second);
    }

    if (lhs_owned && lhs_kgpc_type != NULL)
        destroy_kgpc_type(lhs_kgpc_type);
    if (rhs_owned && rhs_kgpc_type != NULL)
        destroy_kgpc_type(rhs_kgpc_type);

    return return_val;
}
