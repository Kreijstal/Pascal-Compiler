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
static inline char* strndup(const char* s, size_t n)
{
    size_t len = strnlen(s, n);
    char* buf = (char*)malloc(len + 1);
    if (buf == NULL) return NULL;
    memcpy(buf, s, len);
    buf[len] = '\0';
    return buf;
}
#endif
#include "SemCheck_stmt.h"
#include "SemCheck_expr.h"
#include "SemCheck_overload.h"
#include "../SemCheck.h"
#include "../NameMangling.h"
#include "../HashTable/HashTable.h"
#include "../SymTab/SymTab.h"
#include "../../../unit_registry.h"
#include "SemCheck_sizeof.h"

void semcheck_debug_expr_brief(const struct Expression *expr, const char *label);
struct RecordType *get_record_type_from_node(HashNode_t *node);
static int semcheck_try_indexed_property_assignment(SymTab_t *symtab,
    struct Statement *stmt, int max_scope_lev);
#include "../../ParseTree/generic_types.h"
#include "../../ParseTree/tree.h"
#include "../../ParseTree/tree_types.h"
#include "../../ParseTree/ident_ref.h"
#include "../../ParseTree/type_tags.h"
#include "../../List/List.h"

HashNode_t *semcheck_find_preferred_type_node(SymTab_t *symtab, const char *type_id);
static int semcheck_param_types_compatible(Tree_t *param_decl, KgpcType *expected,
    KgpcType *actual, SymTab_t *symtab);

/* Forward declaration from SemCheck_Expr_Resolve.c */
const char *semcheck_type_tag_name(int type_tag);
HashNode_t *semcheck_find_type_node_in_owner_chain(SymTab_t *symtab,
    const char *type_id, const char *owner_full, const char *owner_outer);
const char *semcheck_get_current_subprogram_owner_class_full(void);
const char *semcheck_get_current_subprogram_owner_class_outer(void);
int semcheck_typecheck_array_literal(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, int expected_type, const char *expected_type_id, int line_num);
int set_type_from_hashtype(int *type, HashNode_t *hash_node);
struct RecordType *semcheck_lookup_record_type(SymTab_t *symtab, const char *type_id);
int semcheck_convert_set_literal_to_array_literal(struct Expression *expr);
int semcheck_try_reinterpret_as_typecast(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev);
void semcheck_reset_function_call_cache(struct Expression *expr);
int semcheck_expr_is_char_like(struct Expression *expr);
int semcheck_class_type_ids_compatible(SymTab_t *symtab,
    const char *formal_id, const char *actual_id);

#define SEMSTMT_TIMINGS_ENABLED() (kgpc_getenv("KGPC_DEBUG_SEMSTMT_TIMINGS") != NULL)

static double semstmt_now_ms(void) {
    return (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
}

void semcheck_expr_set_resolved_type(struct Expression *expr, int type_tag);

static int semcheck_expr_best_line(const struct Expression *expr);
static int semcheck_expr_best_context(const struct Expression *expr,
    int *out_line, int *out_col, int *out_source_index);
static int semcheck_expr_list_best_context(ListNode_t *list,
    int *out_line, int *out_col, int *out_source_index);
static int semcheck_expr_is_real_family(const struct Expression *expr);
static int semcheck_expr_list_best_line(ListNode_t *list)
{
    while (list != NULL)
    {
        struct Expression *item = (struct Expression *)list->cur;
        int line = semcheck_expr_best_line(item);
        if (line > 0)
            return line;
        list = list->next;
    }
    return 0;
}

static HashNode_t *semcheck_find_zero_arg_method_node(SymTab_t *symtab,
    const struct RecordType *record, const char *method_name)
{
    if (symtab == NULL || record == NULL || record->type_id == NULL || method_name == NULL)
        return NULL;

    size_t len = strlen(record->type_id) + 2 + strlen(method_name) + 1;
    char *base_name = (char *)malloc(len);
    if (base_name == NULL)
        return NULL;

    snprintf(base_name, len, "%s__%s", record->type_id, method_name);
    ListNode_t *candidates = FindAllIdents(symtab, base_name);
    free(base_name);

    HashNode_t *match = NULL;
    for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
    {
        HashNode_t *cand = (HashNode_t *)cur->cur;
        Tree_t *first_param = NULL;
        const char *first_param_name = NULL;
        if (cand == NULL || cand->type == NULL)
            continue;
        if (cand->hash_type != HASHTYPE_FUNCTION && cand->hash_type != HASHTYPE_PROCEDURE)
            continue;
        ListNode_t *params = kgpc_type_get_procedure_params(cand->type);
        if (params != NULL && params->cur != NULL)
            first_param = (Tree_t *)params->cur;
        if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
            first_param->tree_data.var_decl_data.ids != NULL)
            first_param_name = (const char *)first_param->tree_data.var_decl_data.ids->cur;
        else if (first_param != NULL && first_param->type == TREE_ARR_DECL &&
                 first_param->tree_data.arr_decl_data.ids != NULL)
            first_param_name = (const char *)first_param->tree_data.arr_decl_data.ids->cur;

        if (!((ListLength(params) == 0) ||
              (ListLength(params) == 1 && first_param_name != NULL &&
               strcasecmp(first_param_name, "Self") == 0)))
            continue;
        match = cand;
        break;
    }

    DestroyList(candidates);
    return match;
}

static int semcheck_get_enumerator_current_type(SymTab_t *symtab,
    struct RecordType *enum_record, KgpcType **out_current_type)
{
    if (out_current_type != NULL)
        *out_current_type = NULL;
    if (symtab == NULL || enum_record == NULL || out_current_type == NULL)
        return 0;

    HashNode_t *getcurrent = semcheck_find_zero_arg_method_node(symtab, enum_record, "GetCurrent");
    if (getcurrent != NULL && getcurrent->type != NULL)
    {
        KgpcType *ret = kgpc_type_get_return_type(getcurrent->type);
        if (ret != NULL)
        {
            kgpc_type_retain(ret);
            *out_current_type = ret;
            return 1;
        }
    }

    return 0;
}

static int semcheck_collection_is_enumerator_class(SymTab_t *symtab, KgpcType *collection_kgpc_type,
    KgpcType **out_current_type)
{
    if (out_current_type != NULL)
        *out_current_type = NULL;
    if (symtab == NULL || collection_kgpc_type == NULL)
        return 0;

    KgpcType *record_candidate = collection_kgpc_type;
    if (kgpc_type_is_pointer(collection_kgpc_type))
        record_candidate = collection_kgpc_type->info.points_to;
    if (record_candidate == NULL || !kgpc_type_is_record(record_candidate))
        return 0;

    struct RecordType *collection_record = kgpc_type_get_record(record_candidate);
    if (collection_record == NULL)
        return 0;

    HashNode_t *getenum = semcheck_find_zero_arg_method_node(symtab, collection_record, "GetEnumerator");
    if (getenum == NULL || getenum->type == NULL)
        return 0;

    KgpcType *enum_ret = kgpc_type_get_return_type(getenum->type);
    if (enum_ret == NULL)
        return 0;

    KgpcType *enum_candidate = enum_ret;
    if (kgpc_type_is_pointer(enum_ret))
        enum_candidate = enum_ret->info.points_to;
    if (enum_candidate == NULL || !kgpc_type_is_record(enum_candidate))
        return 0;

    struct RecordType *enum_record = kgpc_type_get_record(enum_candidate);
    if (enum_record == NULL)
        return 0;

    HashNode_t *movenext = semcheck_find_zero_arg_method_node(symtab, enum_record, "MoveNext");
    if (movenext == NULL || movenext->type == NULL)
        return 0;

    KgpcType *move_ret = kgpc_type_get_return_type(movenext->type);
    if (move_ret == NULL || !kgpc_type_is_boolean(move_ret))
        return 0;

    return semcheck_get_enumerator_current_type(symtab, enum_record, out_current_type);
}

static int semcheck_expr_best_line(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    if (expr->line_num > 0)
        return expr->line_num;

    switch (expr->type)
    {
        case EXPR_RELOP:
        {
            int line = semcheck_expr_best_line(expr->expr_data.relop_data.left);
            if (line > 0)
                return line;
            return semcheck_expr_best_line(expr->expr_data.relop_data.right);
        }
        case EXPR_SIGN_TERM:
            return semcheck_expr_best_line(expr->expr_data.sign_term);
        case EXPR_ADDOP:
        {
            int line = semcheck_expr_best_line(expr->expr_data.addop_data.left_expr);
            if (line > 0)
                return line;
            return semcheck_expr_best_line(expr->expr_data.addop_data.right_term);
        }
        case EXPR_MULOP:
        {
            int line = semcheck_expr_best_line(expr->expr_data.mulop_data.left_term);
            if (line > 0)
                return line;
            return semcheck_expr_best_line(expr->expr_data.mulop_data.right_factor);
        }
        case EXPR_ARRAY_ACCESS:
        {
            int line = semcheck_expr_best_line(expr->expr_data.array_access_data.array_expr);
            if (line > 0)
                return line;
            line = semcheck_expr_best_line(expr->expr_data.array_access_data.index_expr);
            if (line > 0)
                return line;
            return semcheck_expr_list_best_line(expr->expr_data.array_access_data.extra_indices);
        }
        case EXPR_RECORD_ACCESS:
            return semcheck_expr_best_line(expr->expr_data.record_access_data.record_expr);
        case EXPR_FUNCTION_CALL:
        {
            int line = semcheck_expr_list_best_line(expr->expr_data.function_call_data.args_expr);
            if (line > 0)
                return line;
            return semcheck_expr_best_line(expr->expr_data.function_call_data.procedural_var_expr);
        }
        case EXPR_POINTER_DEREF:
            return semcheck_expr_best_line(expr->expr_data.pointer_deref_data.pointer_expr);
        case EXPR_ADDR:
            return semcheck_expr_best_line(expr->expr_data.addr_data.expr);
        case EXPR_TYPECAST:
            return semcheck_expr_best_line(expr->expr_data.typecast_data.expr);
        case EXPR_IS:
            return semcheck_expr_best_line(expr->expr_data.is_data.expr);
        case EXPR_AS:
            return semcheck_expr_best_line(expr->expr_data.as_data.expr);
        case EXPR_SET:
        {
            ListNode_t *elements = expr->expr_data.set_data.elements;
            while (elements != NULL)
            {
                struct SetElement *elem = (struct SetElement *)elements->cur;
                int line = semcheck_expr_best_line(elem ? elem->lower : NULL);
                if (line > 0)
                    return line;
                line = semcheck_expr_best_line(elem ? elem->upper : NULL);
                if (line > 0)
                    return line;
                elements = elements->next;
            }
            return 0;
        }
        case EXPR_ARRAY_LITERAL:
            return semcheck_expr_list_best_line(expr->expr_data.array_literal_data.elements);
        case EXPR_RECORD_CONSTRUCTOR:
        {
            ListNode_t *fields = expr->expr_data.record_constructor_data.fields;
            while (fields != NULL)
            {
                struct RecordConstructorField *field = (struct RecordConstructorField *)fields->cur;
                int line = semcheck_expr_best_line(field ? field->value : NULL);
                if (line > 0)
                    return line;
                fields = fields->next;
            }
            return 0;
        }
        case EXPR_ANONYMOUS_FUNCTION:
        case EXPR_ANONYMOUS_PROCEDURE:
            return 0;
        default:
            break;
    }

    if (expr->field_width != NULL)
        return semcheck_expr_best_line(expr->field_width);
    if (expr->field_precision != NULL)
        return semcheck_expr_best_line(expr->field_precision);
    return 0;
}

static int semcheck_expr_list_best_context(ListNode_t *list,
    int *out_line, int *out_col, int *out_source_index)
{
    while (list != NULL)
    {
        struct Expression *item = (struct Expression *)list->cur;
        if (semcheck_expr_best_context(item, out_line, out_col, out_source_index))
            return 1;
        list = list->next;
    }
    return 0;
}

static int semcheck_expr_best_context(const struct Expression *expr,
    int *out_line, int *out_col, int *out_source_index)
{
    if (expr == NULL)
        return 0;

    if (expr->source_index >= 0 || expr->line_num > 0 || expr->col_num > 0)
    {
        if (out_line != NULL && expr->line_num > 0)
            *out_line = expr->line_num;
        if (out_col != NULL && expr->col_num > 0)
            *out_col = expr->col_num;
        if (out_source_index != NULL && expr->source_index >= 0)
            *out_source_index = expr->source_index;
        return 1;
    }

    switch (expr->type)
    {
        case EXPR_RELOP:
            if (semcheck_expr_best_context(expr->expr_data.relop_data.left,
                    out_line, out_col, out_source_index))
                return 1;
            return semcheck_expr_best_context(expr->expr_data.relop_data.right,
                out_line, out_col, out_source_index);
        case EXPR_SIGN_TERM:
            return semcheck_expr_best_context(expr->expr_data.sign_term,
                out_line, out_col, out_source_index);
        case EXPR_ADDOP:
            if (semcheck_expr_best_context(expr->expr_data.addop_data.left_expr,
                    out_line, out_col, out_source_index))
                return 1;
            return semcheck_expr_best_context(expr->expr_data.addop_data.right_term,
                out_line, out_col, out_source_index);
        case EXPR_MULOP:
            if (semcheck_expr_best_context(expr->expr_data.mulop_data.left_term,
                    out_line, out_col, out_source_index))
                return 1;
            return semcheck_expr_best_context(expr->expr_data.mulop_data.right_factor,
                out_line, out_col, out_source_index);
        case EXPR_ARRAY_ACCESS:
            if (semcheck_expr_best_context(expr->expr_data.array_access_data.array_expr,
                    out_line, out_col, out_source_index))
                return 1;
            if (semcheck_expr_best_context(expr->expr_data.array_access_data.index_expr,
                    out_line, out_col, out_source_index))
                return 1;
            return semcheck_expr_list_best_context(expr->expr_data.array_access_data.extra_indices,
                out_line, out_col, out_source_index);
        case EXPR_RECORD_ACCESS:
            return semcheck_expr_best_context(expr->expr_data.record_access_data.record_expr,
                out_line, out_col, out_source_index);
        case EXPR_FUNCTION_CALL:
            if (semcheck_expr_list_best_context(expr->expr_data.function_call_data.args_expr,
                    out_line, out_col, out_source_index))
                return 1;
            return semcheck_expr_best_context(expr->expr_data.function_call_data.procedural_var_expr,
                out_line, out_col, out_source_index);
        case EXPR_POINTER_DEREF:
            return semcheck_expr_best_context(expr->expr_data.pointer_deref_data.pointer_expr,
                out_line, out_col, out_source_index);
        case EXPR_ADDR:
            return semcheck_expr_best_context(expr->expr_data.addr_data.expr,
                out_line, out_col, out_source_index);
        case EXPR_TYPECAST:
            return semcheck_expr_best_context(expr->expr_data.typecast_data.expr,
                out_line, out_col, out_source_index);
        case EXPR_IS:
            return semcheck_expr_best_context(expr->expr_data.is_data.expr,
                out_line, out_col, out_source_index);
        case EXPR_AS:
            return semcheck_expr_best_context(expr->expr_data.as_data.expr,
                out_line, out_col, out_source_index);
        case EXPR_SET:
        {
            ListNode_t *elements = expr->expr_data.set_data.elements;
            while (elements != NULL)
            {
                struct SetElement *elem = (struct SetElement *)elements->cur;
                if (semcheck_expr_best_context(elem ? elem->lower : NULL,
                        out_line, out_col, out_source_index))
                    return 1;
                if (semcheck_expr_best_context(elem ? elem->upper : NULL,
                        out_line, out_col, out_source_index))
                    return 1;
                elements = elements->next;
            }
            return 0;
        }
        case EXPR_ARRAY_LITERAL:
            return semcheck_expr_list_best_context(expr->expr_data.array_literal_data.elements,
                out_line, out_col, out_source_index);
        case EXPR_RECORD_CONSTRUCTOR:
        {
            ListNode_t *fields = expr->expr_data.record_constructor_data.fields;
            while (fields != NULL)
            {
                struct RecordConstructorField *field = (struct RecordConstructorField *)fields->cur;
                if (semcheck_expr_best_context(field ? field->value : NULL,
                        out_line, out_col, out_source_index))
                    return 1;
                fields = fields->next;
            }
            return 0;
        }
        case EXPR_ANONYMOUS_FUNCTION:
        case EXPR_ANONYMOUS_PROCEDURE:
            return 0;
        default:
            break;
    }

    if (expr->field_width != NULL)
        return semcheck_expr_best_context(expr->field_width, out_line, out_col, out_source_index);
    if (expr->field_precision != NULL)
        return semcheck_expr_best_context(expr->field_precision, out_line, out_col, out_source_index);
    return 0;
}

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

static int semcheck_current_subprogram_is_constructor_fallback(SymTab_t *symtab)
{
    if (semcheck_get_current_subprogram_is_constructor())
        return 1;

    const char *method_name = semcheck_get_current_subprogram_method_name();
    const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
    const char *owner = semcheck_get_current_subprogram_owner_class();
    const char *owner_name = owner_full != NULL ? owner_full : owner;
    if (symtab == NULL || method_name == NULL || owner_name == NULL)
        return 0;

    HashNode_t *self_node = NULL;
    if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
    {
        struct RecordType *self_record = get_record_type_from_node(self_node);
        if (self_record != NULL && self_record->method_templates != NULL)
        {
            for (ListNode_t *cur = self_record->method_templates; cur != NULL; cur = cur->next)
            {
                struct MethodTemplate *tmpl = (struct MethodTemplate *)cur->cur;
                if (tmpl != NULL && tmpl->name != NULL &&
                    pascal_identifier_equals(tmpl->name, method_name) &&
                    tmpl->kind == METHOD_TEMPLATE_CONSTRUCTOR)
                    return 1;
            }
        }
    }

    HashNode_t *owner_node = NULL;
    if (FindSymbol(&owner_node, symtab, (char *)owner_name) != 0 || owner_node == NULL)
        return 0;

    struct RecordType *record = get_record_type_from_node(owner_node);
    if (record == NULL || record->method_templates == NULL)
        return 0;

    for (ListNode_t *cur = record->method_templates; cur != NULL; cur = cur->next)
    {
        struct MethodTemplate *tmpl = (struct MethodTemplate *)cur->cur;
        if (tmpl != NULL && tmpl->name != NULL &&
            pascal_identifier_equals(tmpl->name, method_name) &&
            tmpl->kind == METHOD_TEMPLATE_CONSTRUCTOR)
            return 1;
    }

    return 0;
}
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
    if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL &&
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

static int semcheck_record_assign_operator_score(SymTab_t *symtab, HashNode_t *cand,
    KgpcType *target_type, KgpcType *source_type, int *score_out, KgpcType **return_type_out)
{
    if (symtab == NULL || cand == NULL || cand->type == NULL ||
        target_type == NULL || source_type == NULL || score_out == NULL)
    {
        return 0;
    }
    if (!kgpc_type_is_procedure(cand->type))
        return 0;

    ListNode_t *params = kgpc_type_get_procedure_params(cand->type);
    if (params == NULL || params->cur == NULL || params->next != NULL)
        return 0;

    Tree_t *param_decl = (Tree_t *)params->cur;
    int param_owned = 0;
    KgpcType *param_type = resolve_type_from_vardecl(param_decl, symtab, &param_owned);
    if (param_type == NULL)
        return 0;

    int arg_rank = kgpc_type_conversion_rank(source_type, param_type);
    if (arg_rank < 0 && semcheck_param_types_compatible(param_decl, param_type, source_type, symtab))
        arg_rank = 4;

    KgpcType *ret_type = kgpc_type_get_return_type(cand->type);
    if (ret_type != NULL &&
        ret_type->kind == TYPE_KIND_PRIMITIVE &&
        ret_type->info.primitive_type_tag == VARIANT_TYPE &&
        !(target_type->kind == TYPE_KIND_PRIMITIVE &&
          target_type->info.primitive_type_tag == VARIANT_TYPE))
    {
        if (param_owned && param_type != NULL)
            destroy_kgpc_type(param_type);
        return 0;
    }
    int ret_rank = (ret_type != NULL) ? kgpc_type_conversion_rank(ret_type, target_type) : -1;
    if (ret_rank < 0 && ret_type != NULL &&
        are_types_compatible_for_assignment(target_type, ret_type, symtab))
    {
        ret_rank = 4;
    }

    if (param_owned && param_type != NULL)
        destroy_kgpc_type(param_type);

    if (arg_rank < 0 || ret_rank < 0 || ret_type == NULL)
        return 0;

    /* Composite key: argument match is more important than return type match.
     * kgpc_type_conversion_rank returns 0=exact, 1=promotion, 2+=conversion,
     * so lower values are better.  The weights (8, 2) ensure argument rank
     * always dominates return rank in the comparison (lower total = better). */
    *score_out = arg_rank * 8 + ret_rank * 2;
    if (return_type_out != NULL)
        *return_type_out = ret_type;
    return 1;
}

static void semcheck_record_assign_consider_candidate(SymTab_t *symtab, HashNode_t *cand,
    KgpcType *target_type, KgpcType *source_type,
    HashNode_t **best_node, KgpcType **best_return_type, int *best_score)
{
    int cand_score = 0;
    KgpcType *cand_return = NULL;
    if (!semcheck_record_assign_operator_score(symtab, cand, target_type, source_type,
            &cand_score, &cand_return))
    {
        return;
    }

    if (*best_node == NULL || cand_score < *best_score)
    {
        *best_node = cand;
        *best_return_type = cand_return;
        *best_score = cand_score;
    }
}

static void semcheck_record_assign_consider_id(SymTab_t *symtab, const char *id,
    KgpcType *target_type, KgpcType *source_type,
    HashNode_t **best_node, KgpcType **best_return_type, int *best_score)
{
    if (symtab == NULL || id == NULL)
        return;
    ListNode_t *candidates = FindAllIdents(symtab, id);
    for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
    {
        semcheck_record_assign_consider_candidate(symtab, (HashNode_t *)cur->cur,
            target_type, source_type, best_node, best_return_type, best_score);
    }
    DestroyList(candidates);
}

static int semcheck_symbol_is_assign_operator(HashNode_t *cand)
{
    if (cand == NULL)
        return 0;
    if (cand->is_operator &&
        cand->id != NULL &&
        (pascal_identifier_equals(cand->id, ":=") ||
         pascal_identifier_equals(cand->id, "op_assign")))
        return 1;
    if (cand->method_name != NULL &&
        (pascal_identifier_equals(cand->method_name, "op_assign") ||
         pascal_identifier_equals(cand->method_name, ":=") ||
         pascal_identifier_equals(cand->method_name, "Implicit")))
        return 1;
    /* Standalone operators: id like "int64__op_assign_Tconstexprint" */
    if (cand->is_operator && cand->id != NULL &&
        pascal_strcasestr(cand->id, "__op_assign") != NULL)
        return 1;
    return 0;
}

static HashNode_t *semcheck_find_record_assign_operator_candidate(SymTab_t *symtab,
    const char *target_type_id, const char *source_type_id,
    KgpcType *target_type, KgpcType *source_type, KgpcType **return_type_out)
{
    if (symtab == NULL || target_type == NULL || source_type == NULL)
    {
        return NULL;
    }

    HashNode_t *best_node = NULL;
    KgpcType *best_return_type = NULL;
    int best_score = INT_MAX;
    if (target_type_id != NULL)
    {
        char target_id[256];
        char target_op_id[256];
        snprintf(target_id, sizeof(target_id), "%s.:=", target_type_id);
        snprintf(target_op_id, sizeof(target_op_id), "%s__op_assign", target_type_id);
        semcheck_record_assign_consider_id(symtab, target_id, target_type, source_type,
            &best_node, &best_return_type, &best_score);
        semcheck_record_assign_consider_id(symtab, target_op_id, target_type, source_type,
            &best_node, &best_return_type, &best_score);
    }
    if (source_type_id != NULL)
    {
        char source_id[256];
        char source_op_id[256];
        snprintf(source_id, sizeof(source_id), "%s.:=", source_type_id);
        snprintf(source_op_id, sizeof(source_op_id), "%s__op_assign", source_type_id);
        semcheck_record_assign_consider_id(symtab, source_id, target_type, source_type,
            &best_node, &best_return_type, &best_score);
        semcheck_record_assign_consider_id(symtab, source_op_id, target_type, source_type,
            &best_node, &best_return_type, &best_score);
    }
    if (target_type_id != NULL && source_type_id != NULL)
    {
        char target_specific_id[320];
        char source_specific_id[320];
        snprintf(target_specific_id, sizeof(target_specific_id), "%s__op_assign_%s",
            target_type_id, source_type_id);
        snprintf(source_specific_id, sizeof(source_specific_id), "%s__op_assign_%s",
            source_type_id, target_type_id);
        semcheck_record_assign_consider_id(symtab, target_specific_id, target_type, source_type,
            &best_node, &best_return_type, &best_score);
        semcheck_record_assign_consider_id(symtab, source_specific_id, target_type, source_type,
            &best_node, &best_return_type, &best_score);
    }

    if (best_node != NULL)
    {
        if (return_type_out != NULL)
            *return_type_out = best_return_type;
        return best_node;
    }

    semcheck_record_assign_consider_id(symtab, ":=", target_type, source_type,
        &best_node, &best_return_type, &best_score);
    semcheck_record_assign_consider_id(symtab, "op_assign", target_type, source_type,
        &best_node, &best_return_type, &best_score);

    for (ScopeNode *scope = symtab->current_scope; scope != NULL; scope = scope->parent)
    {
        HashTable_t *table = scope->table;
        if (table != NULL)
        {
            for (int i = 0; i < TABLE_SIZE; ++i)
            {
                for (ListNode_t *cur = table->table[i]; cur != NULL; cur = cur->next)
                {
                    HashNode_t *cand = (HashNode_t *)cur->cur;
                    if (!semcheck_symbol_is_assign_operator(cand))
                        continue;
                    semcheck_record_assign_consider_candidate(symtab, cand, target_type,
                        source_type, &best_node, &best_return_type, &best_score);
                }
            }
        }
    }

    /* Search unit tables — walk unit tables for unit-aware lookup. */
    {
        int caller_unit_index = (symtab->current_scope != NULL && symtab->current_scope->unit_index > 0)
            ? symtab->current_scope->unit_index
            : semcheck_get_current_unit_index();

        /* Caller's own unit table */
        if (caller_unit_index > 0 && caller_unit_index < SYMTAB_MAX_UNITS &&
            symtab->unit_scopes[caller_unit_index] != NULL)
        {
            HashTable_t *table = symtab->unit_scopes[caller_unit_index]->table;
            for (int i = 0; i < TABLE_SIZE; ++i)
            {
                for (ListNode_t *cur = table->table[i]; cur != NULL; cur = cur->next)
                {
                    HashNode_t *cand = (HashNode_t *)cur->cur;
                    if (!semcheck_symbol_is_assign_operator(cand))
                        continue;
                    semcheck_record_assign_consider_candidate(symtab, cand, target_type,
                        source_type, &best_node, &best_return_type, &best_score);
                }
            }
        }

        /* Dependency unit tables */
        int num_units = unit_registry_count();
        for (int dep = 1; dep <= num_units; dep++)
        {
            if (dep == caller_unit_index)
                continue;
            if (!unit_registry_is_dep(caller_unit_index, dep))
                continue;
            if (dep >= SYMTAB_MAX_UNITS || symtab->unit_scopes[dep] == NULL)
                continue;
            HashTable_t *table = symtab->unit_scopes[dep]->table;
            for (int i = 0; i < TABLE_SIZE; ++i)
            {
                for (ListNode_t *cur = table->table[i]; cur != NULL; cur = cur->next)
                {
                    HashNode_t *cand = (HashNode_t *)cur->cur;
                    if (!semcheck_symbol_is_assign_operator(cand))
                        continue;
                    semcheck_record_assign_consider_candidate(symtab, cand, target_type,
                        source_type, &best_node, &best_return_type, &best_score);
                }
            }
        }

        /* When current scope is program-level (unit_index == 0), search all unit tables */
        if (caller_unit_index == 0)
        {
            for (int u = 1; u < SYMTAB_MAX_UNITS; u++)
            {
                if (symtab->unit_scopes[u] == NULL)
                    continue;
                HashTable_t *table = symtab->unit_scopes[u]->table;
                for (int i = 0; i < TABLE_SIZE; ++i)
                {
                    for (ListNode_t *cur = table->table[i]; cur != NULL; cur = cur->next)
                    {
                        HashNode_t *cand = (HashNode_t *)cur->cur;
                        if (!semcheck_symbol_is_assign_operator(cand))
                            continue;
                        semcheck_record_assign_consider_candidate(symtab, cand, target_type,
                            source_type, &best_node, &best_return_type, &best_score);
                    }
                }
            }
        }
    }

    if (symtab->builtin_scope->table != NULL)
    {
        HashTable_t *table = symtab->builtin_scope->table;
        for (int i = 0; i < TABLE_SIZE; ++i)
        {
            for (ListNode_t *cur = table->table[i]; cur != NULL; cur = cur->next)
            {
                HashNode_t *cand = (HashNode_t *)cur->cur;
                if (!semcheck_symbol_is_assign_operator(cand))
                    continue;
                semcheck_record_assign_consider_candidate(symtab, cand, target_type,
                    source_type, &best_node, &best_return_type, &best_score);
            }
        }
    }

    if (best_node != NULL && return_type_out != NULL)
        *return_type_out = best_return_type;
    return best_node;
}

int semcheck_try_record_conversion_expression(SymTab_t *symtab,
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
    int source_is_record = semcheck_type_is_recordish(*source_type);
    if (!target_is_pointer && !target_is_record && !source_is_record)
        return 0;

    struct Expression *source_expr = *expr_slot;
    const char *source_type_id = semcheck_record_type_id_from_expr(symtab, source_expr, *source_type);
    const char *target_type_id = semcheck_record_type_id_from_expr(symtab, target_expr, target_type);

    HashNode_t *operator_node = NULL;
    KgpcType *return_type = NULL;
    operator_node = semcheck_find_record_assign_operator_candidate(symtab,
        target_type_id, source_type_id, target_type, *source_type, &return_type);
    if (operator_node == NULL || return_type == NULL)
    {
        return 0;
    }
    if (!are_types_compatible_for_assignment(target_type, return_type, symtab))
        return 0;

    const char *call_id = operator_node->mangled_id != NULL
        ? operator_node->mangled_id
        : (operator_node->id != NULL ? operator_node->id : "op_assign");
    struct Expression *call_expr = mk_functioncall(source_expr->line_num, strdup(call_id), NULL);
    call_expr->expr_data.function_call_data.is_operator_call = 1;
    call_expr->expr_data.function_call_data.args_expr = CreateListNode(source_expr, LIST_EXPR);
    if (operator_node->mangled_id != NULL)
        call_expr->expr_data.function_call_data.mangled_id = strdup(operator_node->mangled_id);
    else
        call_expr->expr_data.function_call_data.mangled_id = strdup(call_id);
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
    if (FindSymbol(&typed_file_node, symtab, "TypedFile") == 0 || typed_file_node == NULL)
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
        if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
            fprintf(stderr, "[SemCheck] param_has_default_value: TREE_VAR_DECL, initializer=%p\n",
                (void*)decl->tree_data.var_decl_data.initializer);
        }
        return decl->tree_data.var_decl_data.initializer != NULL;
    }
    else if (decl->type == TREE_ARR_DECL)
    {
        if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
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
        case EXPR_RECORD_ACCESS:
            copy = clone_expression(src);
            break;
        case EXPR_FUNCTION_CALL:
            copy = clone_expression(src);
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
            if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
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
    if (FindSymbol(&node, symtab, lhs->expr_data.id) == 0 || node == NULL)
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

static KgpcType *resolve_param_type_with_owner(Tree_t *param_decl, SymTab_t *symtab,
    const char *owner_full, const char *owner_outer, int *param_type_owned)
{
    KgpcType *param_type = resolve_type_from_vardecl(param_decl, symtab, param_type_owned);
    if (param_type != NULL || param_decl == NULL || symtab == NULL)
        return param_type;

    const char *type_id = NULL;
    if (param_decl->type == TREE_VAR_DECL)
        type_id = param_decl->tree_data.var_decl_data.type_id;
    else if (param_decl->type == TREE_ARR_DECL)
        type_id = param_decl->tree_data.arr_decl_data.type_id;

    if (type_id == NULL)
        return NULL;

    const char *resolved_owner_full = owner_full;
    const char *resolved_owner_outer = owner_outer;
    if (resolved_owner_full == NULL && resolved_owner_outer == NULL)
    {
        resolved_owner_full = semcheck_get_current_subprogram_owner_class_full();
        resolved_owner_outer = semcheck_get_current_subprogram_owner_class_outer();
        if (resolved_owner_full == NULL)
            resolved_owner_full = semcheck_get_current_method_owner();
    }

    HashNode_t *type_node = semcheck_find_type_node_in_owner_chain(symtab, type_id,
        resolved_owner_full, resolved_owner_outer);
    if (type_node == NULL)
        return NULL;

    if (type_node->type != NULL)
    {
        kgpc_type_retain(type_node->type);
        if (param_type_owned != NULL)
            *param_type_owned = 1;
        return type_node->type;
    }

    struct TypeAlias *alias = get_type_alias_from_node(type_node);
    if (alias != NULL)
    {
        KgpcType *alias_type = create_kgpc_type_from_type_alias(alias, symtab, 0);
        if (alias_type != NULL)
        {
            if (alias->kgpc_type == alias_type)
                kgpc_type_retain(alias_type);
            if (param_type_owned != NULL)
                *param_type_owned = 1;
            return alias_type;
        }
    }

    return NULL;
}

static HashNode_t *lookup_hashnode(SymTab_t *symtab, const char *id)
{
    if (symtab == NULL || id == NULL)
        return NULL;
    HashNode_t *node = NULL;
    if (FindSymbol(&node, symtab, id) != 0 && node != NULL)
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
        
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
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
    const char *debug_env = kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES");

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

static void semcheck_stmt_set_call_owner_info(struct Statement *stmt,
    const char *owner_class, const char *method_name)
{
    if (stmt == NULL || stmt->type != STMT_PROCEDURE_CALL)
        return;

    if (stmt->stmt_data.procedure_call_data.cached_owner_class != NULL)
    {
        free(stmt->stmt_data.procedure_call_data.cached_owner_class);
        stmt->stmt_data.procedure_call_data.cached_owner_class = NULL;
    }
    if (stmt->stmt_data.procedure_call_data.cached_method_name != NULL)
    {
        free(stmt->stmt_data.procedure_call_data.cached_method_name);
        stmt->stmt_data.procedure_call_data.cached_method_name = NULL;
    }

    if (owner_class != NULL)
        stmt->stmt_data.procedure_call_data.cached_owner_class = strdup(owner_class);
    if (method_name != NULL)
        stmt->stmt_data.procedure_call_data.cached_method_name = strdup(method_name);
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
        if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL)
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

static int semcheck_type_is_char_like(KgpcType *type)
{
    if (type == NULL)
        return 0;
    if (kgpc_type_is_char(type))
        return 1;
    if (type->type_alias != NULL && type->type_alias->is_char_alias)
        return 1;
    return 0;
}

static int semcheck_force_char_case_builtin_in_assignment(struct Expression *expr)
{
    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return 0;

    const char *id = expr->expr_data.function_call_data.id;
    if (id == NULL)
        return 0;

    const char *mangled = NULL;
    if (pascal_identifier_equals(id, "UpCase") ||
        pascal_identifier_equals(id, "UpperCase"))
        mangled = "kgpc_upcase_char";
    else if (pascal_identifier_equals(id, "LowerCase"))
        mangled = "kgpc_lowercase_char";
    else
        return 0;

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
        return 0;

    struct Expression *arg_expr = (struct Expression *)args->cur;
    if (!semcheck_expr_is_char_like(arg_expr))
        return 0;

    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    free(expr->expr_data.function_call_data.id);
    expr->expr_data.function_call_data.id = strdup(mangled);
    expr->expr_data.function_call_data.mangled_id = strdup(mangled);
    semcheck_reset_function_call_cache(expr);
    expr->expr_data.function_call_data.is_call_info_valid = 1;
    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    expr->resolved_kgpc_type = create_primitive_type(CHAR_TYPE);
    semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
    return 1;
}

/* Check if expression is an integer constant representable as single-byte Char (0..255). */
static int semcheck_expr_is_char_ordinal_const(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr->type == EXPR_INUM)
        return (expr->expr_data.i_num >= 0 && expr->expr_data.i_num <= 255);

    if (expr->type == EXPR_VAR_ID && symtab != NULL && expr->expr_data.id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL &&
            (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
        {
            return (node->const_int_value >= 0 && node->const_int_value <= 255);
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
static struct Statement *transform_two_arg_new_dispose(struct Statement *stmt,
    int *is_dispose);
static int semcheck_mangled_suffix_matches_untyped(const char *candidate_suffix,
    const char *call_suffix);
static HashNode_t *semcheck_find_untyped_mangled_match(ListNode_t *candidates,
    const char *proc_id, const char *call_mangled);
static int semcheck_var_decl_is_untyped(Tree_t *decl);
static int semcheck_stmt_has_single_overload(SymTab_t *symtab, const char *proc_id);
static int semcheck_stmt_try_set_method_mangled_id(SymTab_t *symtab,
    struct Statement *stmt, const char *proc_id, const char *mangled_id);
static int semcheck_set_stmt_call_mangled_id(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);

static int semcheck_stmt_has_single_overload(SymTab_t *symtab, const char *proc_id)
{
    if (symtab == NULL || proc_id == NULL)
        return 0;

    ListNode_t *all_overloads = FindAllIdents(symtab, proc_id);
    int num_overloads = ListLength(all_overloads);
    DestroyList(all_overloads);
    return num_overloads <= 1;
}

static int semcheck_stmt_try_set_method_mangled_id(SymTab_t *symtab,
    struct Statement *stmt, const char *proc_id, const char *mangled_id)
{
    if (stmt == NULL || mangled_id == NULL ||
        !semcheck_stmt_has_single_overload(symtab, proc_id))
        return 0;

    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL &&
        stmt->stmt_data.procedure_call_data.mangled_id != mangled_id)
        free(stmt->stmt_data.procedure_call_data.mangled_id);
    stmt->stmt_data.procedure_call_data.mangled_id = strdup(mangled_id);
    return stmt->stmt_data.procedure_call_data.mangled_id != NULL;
}

static int semcheck_call_with_proc_var(SymTab_t *symtab, struct Statement *stmt, HashNode_t *proc_node,
    int max_scope_lev)
{
    if (proc_node == NULL || proc_node->type == NULL ||
        proc_node->type->kind != TYPE_KIND_PROCEDURE)
        return 0;

    int return_val = 0;
    ListNode_t *formal_params = proc_node->type->info.proc_info.params;
    ListNode_t *args_given = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_index = 0;
    const char *callee_owner_full = proc_node->owner_class_full;
    const char *callee_owner_outer = proc_node->owner_class_outer;
    if (callee_owner_full == NULL && callee_owner_outer == NULL)
    {
        Tree_t *proc_def = proc_node->type->info.proc_info.definition;
        if (proc_def != NULL && proc_def->type == TREE_SUBPROGRAM)
        {
            callee_owner_full = proc_def->tree_data.subprogram_data.owner_class_full;
            callee_owner_outer = proc_def->tree_data.subprogram_data.owner_class_outer;
            if (callee_owner_full == NULL)
                callee_owner_full = proc_def->tree_data.subprogram_data.owner_class;
        }
    }

    while (formal_params != NULL && args_given != NULL)
    {
        ++arg_index;
        assert(formal_params->type == LIST_TREE);
        assert(args_given->type == LIST_EXPR);

        Tree_t *param_decl = (Tree_t *)formal_params->cur;
        struct Expression *arg_expr = (struct Expression *)args_given->cur;

        /* Phase 3: Use KgpcType for comprehensive type checking */
        if (arg_expr != NULL && arg_expr->type == EXPR_FUNCTION_CALL)
        {
            int cast_type = UNKNOWN_TYPE;
            int cast_result = semcheck_try_reinterpret_as_typecast(&cast_type, symtab,
                arg_expr, max_scope_lev);
            if (cast_result != 0)
                return cast_result;
        }
        
        /* Resolve KgpcType for the argument expression */
        int arg_type_owned = 0;
        KgpcType *arg_type = semcheck_resolve_expression_kgpc_type(symtab, arg_expr, INT_MAX, NO_MUTATE, &arg_type_owned);
        
        /* Resolve KgpcType for the formal parameter */
        int param_type_owned = 0;
        KgpcType *param_type = NULL;
        if (param_decl != NULL &&
            (param_decl->type == TREE_VAR_DECL || param_decl->type == TREE_ARR_DECL))
        {
            param_type = resolve_param_type_with_owner(param_decl, symtab,
                callee_owner_full, callee_owner_outer, &param_type_owned);
        }



        /* Both types must be resolved for proper type checking */
        int param_is_untyped = semcheck_var_decl_is_untyped(param_decl);

        if ((arg_type == NULL || param_type == NULL) && !param_is_untyped)
        {
            /* Suppress cascading errors when types can't be resolved —
             * upstream UNKNOWN_TYPE already reported the root cause. */
        }
        else if (!param_is_untyped)
        {
            /* Use comprehensive KgpcType-based type compatibility checking */
            if (!semcheck_param_types_compatible(param_decl, param_type, arg_type, symtab) &&
                !kgpc_type_equals_tag(arg_type, UNKNOWN_TYPE) &&
                !kgpc_type_equals_tag(param_type, UNKNOWN_TYPE))
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

    if (formal_params == NULL && args_given != NULL && !proc_node->is_varargs &&
        proc_node->hash_type != HASHTYPE_BUILTIN_PROCEDURE)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
            "Error on line %d, on procedure call %s, too many arguments given!\n\n",
            stmt->line_num, stmt->stmt_data.procedure_call_data.id);
        ++return_val;
    }
    else if (formal_params != NULL && args_given == NULL)
    {
        /* Check if all remaining formal parameters have default values */
        int all_have_defaults = 1;
        for (ListNode_t *fp = formal_params; fp != NULL; fp = fp->next)
        {
            Tree_t *pd = (Tree_t *)fp->cur;
            if (pd == NULL || !param_has_default_value(pd))
            {
                all_have_defaults = 0;
                break;
            }
        }
        if (!all_have_defaults)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                "Error on line %d, on procedure call %s, not enough arguments given!\n\n",
                stmt->line_num, stmt->stmt_data.procedure_call_data.id);
            ++return_val;
        }
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
    int forced_system_builtin = 0;
    if (stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
    {
        const char *placeholder_name =
            stmt->stmt_data.procedure_call_data.placeholder_method_name;
        ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
        struct Expression *qualifier_expr =
            (args != NULL) ? (struct Expression *)args->cur : NULL;
        const char *derived_name = NULL;

        if (placeholder_name == NULL &&
            proc_id != NULL &&
            proc_id[0] == '_' &&
            proc_id[1] == '_' &&
            proc_id[2] != '\0')
        {
            derived_name = proc_id + 2;
            placeholder_name = derived_name;
        }

        if (placeholder_name == NULL ||
            !pascal_identifier_equals(placeholder_name, expected_name) ||
            qualifier_expr == NULL ||
            qualifier_expr->type != EXPR_VAR_ID ||
            qualifier_expr->expr_data.id == NULL ||
            !pascal_identifier_equals(qualifier_expr->expr_data.id, "System"))
        {
            return 0;
        }

        ListNode_t *remaining_args = args->next;
        destroy_expr(qualifier_expr);
        args->cur = NULL;
        free(args);
        stmt->stmt_data.procedure_call_data.expr_args = remaining_args;

        if (proc_id == NULL || !pascal_identifier_equals(proc_id, expected_name))
        {
            free(proc_id);
            proc_id = strdup(expected_name);
            if (proc_id == NULL)
                return 1;
            stmt->stmt_data.procedure_call_data.id = proc_id;
        }

        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
        if (stmt->stmt_data.procedure_call_data.placeholder_method_name != NULL)
        {
            free(stmt->stmt_data.procedure_call_data.placeholder_method_name);
            stmt->stmt_data.procedure_call_data.placeholder_method_name = NULL;
        }
        forced_system_builtin = 1;
    }

    if (proc_id == NULL || !pascal_identifier_equals(proc_id, expected_name))
        return 0;

    /* Prefer user-defined/prologue procedures over builtins when available.
     * Exception: System.XXX qualified calls always resolve to the builtin. */
    HashNode_t *existing = NULL;
    const char *qualifier = stmt->stmt_data.procedure_call_data.call_qualifier;
    int force_builtin = pascal_identifier_equals(expected_name, "Assign") ||
                        pascal_identifier_equals(expected_name, "Val") ||
                        pascal_identifier_equals(expected_name, "Str") ||
                        forced_system_builtin ||
                        (qualifier != NULL &&
                         pascal_identifier_equals(qualifier, "System"));
    if (!force_builtin &&
        FindSymbol(&existing, symtab, proc_id) != 0 && existing != NULL &&
        existing->hash_type != HASHTYPE_BUILTIN_PROCEDURE)
    {
        /* Builtin procedure names should still win over implicit/self method
         * visibility. For example, TFPList.Move must not shadow System.Move
         * inside another TFPList method body. Only non-method user/global
         * procedures should suppress builtin resolution here. */
        if (existing->owner_class != NULL)
            existing = NULL;
    }

    if (!force_builtin && existing != NULL &&
        existing->hash_type != HASHTYPE_BUILTIN_PROCEDURE)
    {
        return 0;
    }

    HashNode_t *builtin_node = FindIdentInTable(symtab->builtin_scope->table, proc_id);
    /* Also check unit_scopes[System]->table — builtin procedures live there since
     * per-unit scoping was added. */
    if (builtin_node == NULL)
    {
        int sys_idx = unit_registry_add("System");
        if (sys_idx > 0 && sys_idx < SYMTAB_MAX_UNITS &&
            symtab->unit_scopes[sys_idx] != NULL)
        {
            HashNode_t *sys_node = FindIdentInTable(
                symtab->unit_scopes[sys_idx]->table, proc_id);
            if (sys_node != NULL &&
                sys_node->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
                builtin_node = sys_node;
        }
    }
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

    if (forced_system_builtin)
    {
        stmt->stmt_data.procedure_call_data.resolved_proc = NULL;
        stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_BUILTIN_PROCEDURE;
        semcheck_stmt_set_call_kgpc_type(stmt, NULL,
            stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
        stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
        if (handled != NULL)
            *handled = 1;
        return handler(symtab, stmt, max_scope_lev);
    }

    if (qualifier != NULL && pascal_identifier_equals(qualifier, "System"))
    {
        stmt->stmt_data.procedure_call_data.resolved_proc = NULL;
        stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_BUILTIN_PROCEDURE;
        semcheck_stmt_set_call_kgpc_type(stmt, NULL,
            stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
        stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, SetLength expects exactly two arguments.\n", stmt->line_num);
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
    int target_is_wide_string = 0;

    int target_is_string = (target_type == STRING_TYPE);
    /* Fallback: check KgpcType for string (e.g. function result vars with overloads) */
    if (!target_is_string && !target_is_shortstring &&
        array_expr != NULL && array_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_string(array_expr->resolved_kgpc_type))
    {
        target_is_string = 1;
    }
    if (array_expr != NULL && array_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_wide_string(array_expr->resolved_kgpc_type))
    {
        target_is_wide_string = 1;
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
            if (FindSymbol(&array_node, symtab, array_expr->expr_data.id) != 0 &&
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
        else if (target_is_wide_string)
            stmt->stmt_data.procedure_call_data.mangled_id = strdup("__kgpc_setlength_unicodestring");
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
            if (FindSymbol(&array_node, symtab, array_expr->expr_data.id) != 0 && array_node != NULL)
            {
                set_hash_meta(array_node, BOTH_MUTATE_REFERENCE);
                
                /* Check if it's a dynamic array using KgpcType first, then legacy field */
                int is_dynamic = hashnode_is_dynamic_array(array_node);
                
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
        else if (array_expr != NULL && array_expr->type == EXPR_ARRAY_ACCESS)
        {
            /* Array access result - valid for nested dynamic arrays (array of array of ...) */
            is_valid_array = 1;
        }
        else if (array_expr != NULL && array_expr->type == EXPR_POINTER_DEREF)
        {
            /* Pointer dereference - could point to a dynamic array */
            is_valid_array = 1;
        }
        else if (array_expr != NULL && array_expr->type == EXPR_FUNCTION_CALL)
        {
            /* Function call result that returns a dynamic array reference */
            is_valid_array = 1;
        }
        
        if (!is_valid_array)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, first argument to SetLength must be a dynamic array variable.\n", stmt->line_num);
            ++return_val;
        }
    }

    int length_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&length_type, symtab, length_expr, max_scope_lev, NO_MUTATE);
    if (!is_integer_type(length_type))
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, SetLength length argument must be an integer.\n", stmt->line_num);
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, SetString expects exactly three arguments.\n", stmt->line_num);
        return 1;
    }

    struct Expression *string_expr = (struct Expression *)args->cur;
    struct Expression *buffer_expr = (struct Expression *)args->next->cur;
    struct Expression *length_expr = (struct Expression *)args->next->next->cur;

    /* First argument must be a string variable (output parameter) */
    int string_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&string_type, symtab, string_expr, max_scope_lev, MUTATE);
    int target_is_shortstring = semcheck_expr_is_shortstring(string_expr);
    if (string_type != STRING_TYPE && string_type != SHORTSTRING_TYPE && string_type != UNKNOWN_TYPE && !target_is_shortstring)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, SetString first argument must be a string variable.\n", stmt->line_num);
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
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, SetString second argument must be a pointer (PChar).\n", stmt->line_num);
            ++return_val;
        }
    }

    /* Third argument must be an integer length */
    int length_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&length_type, symtab, length_expr, max_scope_lev, NO_MUTATE);
    if (!is_integer_type(length_type))
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, SetString length argument must be an integer.\n", stmt->line_num);
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Str expects exactly two arguments.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    struct Expression *value_expr = (struct Expression *)args->cur;
    struct Expression *target_expr = (struct Expression *)args->next->cur;

    int value_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&value_type, symtab, value_expr, INT_MAX, NO_MUTATE);
    if (!is_ordinal_type(value_type) && !is_real_family_type(value_type))
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Str value must be an ordinal or real.\n", stmt->line_num);
        ++return_val;
    }

    if (value_expr != NULL && value_expr->field_width != NULL)
    {
        int width_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&width_type, symtab, value_expr->field_width, INT_MAX, NO_MUTATE);
        if (!is_integer_type(width_type))
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Str field width must be an integer.\n", stmt->line_num);
            ++return_val;
        }
    }

    if (value_expr != NULL && value_expr->field_precision != NULL)
    {
        int precision_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&precision_type, symtab, value_expr->field_precision, INT_MAX, NO_MUTATE);
        if (!is_integer_type(precision_type))
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Str field precision must be an integer.\n", stmt->line_num);
            ++return_val;
        }
    }

    int target_type = UNKNOWN_TYPE;
    int target_err = semcheck_stmt_expr_tag(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    if (target_err > 0 && target_expr != NULL && target_expr->type == EXPR_TYPECAST)
    {
        /* Allow Inc on typecasted pointer expressions like Inc(PAnsiChar(p), ...) */
        target_err = semcheck_stmt_expr_tag(&target_type, symtab, target_expr, max_scope_lev, NO_MUTATE);
        struct Expression *inner = target_expr->expr_data.typecast_data.expr;
        if (inner != NULL)
            target_err += semcheck_stmt_expr_tag(NULL, symtab, inner, max_scope_lev, MUTATE);
    }
    return_val += target_err;
    if (target_type != STRING_TYPE && target_type != SHORTSTRING_TYPE)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Str output must be a string variable.\n", stmt->line_num);
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Insert expects exactly three arguments.\n",
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
    /* Also accept dynamic arrays (e.g. TCharArray for TStringBuilder) */
    int source_is_array = (source_expr != NULL && source_expr->resolved_kgpc_type != NULL &&
        source_expr->resolved_kgpc_type->kind == TYPE_KIND_ARRAY);
    if (source_type != STRING_TYPE && source_type != CHAR_TYPE &&
        source_type != SHORTSTRING_TYPE &&
        !source_is_shortstring && !source_is_array)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Insert source must be a string or char.\n",
            stmt->line_num);
        ++error_count;
    }

    int target_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    int target_is_shortstring = semcheck_expr_is_shortstring(target_expr);
    int target_is_array = (target_expr != NULL && target_expr->resolved_kgpc_type != NULL &&
        target_expr->resolved_kgpc_type->kind == TYPE_KIND_ARRAY);
    if (target_type != STRING_TYPE && target_type != SHORTSTRING_TYPE &&
        !target_is_shortstring && !target_is_array)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Insert target must be a string variable.\n",
            stmt->line_num);
        ++error_count;
    }

    int index_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&index_type, symtab, index_expr, max_scope_lev, NO_MUTATE);
    if (!is_integer_type(index_type))
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Insert index must be an integer.\n",
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Delete expects exactly three arguments.\n",
            stmt->line_num);
        return 1;
    }

    int error_count = 0;
    struct Expression *target_expr = (struct Expression *)args->cur;
    struct Expression *index_expr = (struct Expression *)args->next->cur;
    struct Expression *count_expr = (struct Expression *)args->next->next->cur;

    int target_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    
    /* Check if target is a string type, shortstring (array of char), or dynamic array */
    int is_valid_target = is_string_type(target_type) ||
                          is_shortstring_array(target_type, target_expr->is_array_expr);
    /* Also accept dynamic arrays (FPC supports Delete on dynamic arrays) */
    if (!is_valid_target && target_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_dynamic_array(target_expr->resolved_kgpc_type))
        is_valid_target = 1;
    int target_is_shortstring = semcheck_expr_is_shortstring(target_expr);
    
    if (!is_valid_target)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Delete target must be a string variable.\n",
            stmt->line_num);
        ++error_count;
    }

    int index_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&index_type, symtab, index_expr, max_scope_lev, NO_MUTATE);
    if (!is_integer_type(index_type))
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Delete index must be an integer.\n",
            stmt->line_num);
        ++error_count;
    }

    int count_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&count_type, symtab, count_expr, max_scope_lev, NO_MUTATE);
    if (!is_integer_type(count_type))
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Delete count must be an integer.\n",
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
    int arg_count = ListLength(args);
    if (args == NULL || (arg_count != 2 && arg_count != 3))
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Val expects two or three arguments.\n",
            stmt->line_num);
        return 1;
    }

    int error_count = 0;

    struct Expression *source_expr = (struct Expression *)args->cur;
    int source_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&source_type, symtab, source_expr, max_scope_lev, NO_MUTATE);
    if (!is_string_type(source_type) && source_type != CHAR_TYPE)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Val expects its first argument to be a string.\n",
            stmt->line_num);
        ++error_count;
    }

    struct Expression *value_expr = (struct Expression *)args->next->cur;
    int value_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&value_type, symtab, value_expr, max_scope_lev, MUTATE);
    if (!is_integer_type(value_type) && !is_real_family_type(value_type))
    {
        fprintf(stderr,
            "Error on line %d, Val target must be an integer, longint, or real variable.\n",
            stmt->line_num);
        ++error_count;
    }

    if (arg_count == 3)
    {
        struct Expression *code_expr = (struct Expression *)args->next->next->cur;
        int code_type = UNKNOWN_TYPE;
        error_count += semcheck_stmt_expr_tag(&code_type, symtab, code_expr, max_scope_lev, MUTATE);
        if (!is_integer_type(code_type))
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Val code argument must be an integer variable.\n",
                stmt->line_num);
            ++error_count;
        }
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Inc expects one or two arguments.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    struct Expression *target_expr = (struct Expression *)args->cur;
    int target_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    int target_is_pointer = (target_type == POINTER_TYPE);
    if (!is_ordinal_type(target_type) && !target_is_pointer)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Inc target must be an ordinal or pointer variable.\n", stmt->line_num);
        ++return_val;
    }

    if (args->next != NULL)
    {
        struct Expression *value_expr = (struct Expression *)args->next->cur;
        int value_type = UNKNOWN_TYPE;
        return_val += semcheck_stmt_expr_tag(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);
        int value_is_integer = is_integer_type(value_type);
        if (!value_is_integer && value_expr != NULL)
        {
            if (value_expr->type == EXPR_INUM)
                value_is_integer = 1;
            else if (value_expr->type == EXPR_FUNCTION_CALL &&
                     value_expr->expr_data.function_call_data.id != NULL &&
                     pascal_identifier_equals(value_expr->expr_data.function_call_data.id, "SizeOf"))
            {
                value_is_integer = 1;
            }
        }
        if (!value_is_integer)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Inc increment must be an integer.\n", stmt->line_num);
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, %s expects exactly two arguments.\n",
            stmt->line_num, display_name);
        return 1;
    }

    int error_count = 0;
    struct Expression *set_expr = (struct Expression *)args->cur;
    int set_type = UNKNOWN_TYPE;
    int set_type_owned = 0;
    error_count += semcheck_stmt_expr_tag(&set_type, symtab, set_expr, max_scope_lev, MUTATE);
    KgpcType *set_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, set_expr,
        max_scope_lev, MUTATE, &set_type_owned);
    if (set_type != SET_TYPE && !kgpc_type_is_set(set_kgpc_type))
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, %s target must be a set.\n",
            stmt->line_num, display_name);
        ++error_count;
    }
    if (set_type_owned && set_kgpc_type != NULL)
        destroy_kgpc_type(set_kgpc_type);

    struct Expression *value_expr = (struct Expression *)args->next->cur;
    int value_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);
    if (!is_ordinal_type(value_type))
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, %s element must be an ordinal value.\n",
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
    int max_scope_lev, const char *display_name, int allow_count_arg)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_count = ListLength(args);
    if (arg_count < 1 || arg_count > (allow_count_arg ? 2 : 1))
    {
        if (allow_count_arg)
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, %s expects one or two arguments.\n",
                stmt->line_num, display_name);
        else
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, %s expects exactly one argument.\n",
                stmt->line_num, display_name);
        return 1;
    }

    int error_count = 0;
    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    error_count += semcheck_stmt_expr_tag(&arg_type, symtab, arg_expr, max_scope_lev, MUTATE);
    /* Accept any type - Initialize/Finalize work with all managed types */
    if (allow_count_arg && args->next != NULL)
    {
        struct Expression *count_expr = (struct Expression *)args->next->cur;
        int count_type = UNKNOWN_TYPE;
        error_count += semcheck_stmt_expr_tag(&count_type, symtab, count_expr, max_scope_lev, NO_MUTATE);
        if (!is_integer_type(count_type))
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, %s count argument must be an integer.\n",
                stmt->line_num, display_name);
            ++error_count;
        }
    }
    return error_count;
}

static int semcheck_builtin_initialize(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_initialize_finalize(symtab, stmt, max_scope_lev, "Initialize", 0);
}

static int semcheck_builtin_finalize(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_initialize_finalize(symtab, stmt, max_scope_lev, "Finalize", 1);
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Assert expects 1 or 2 arguments.\n",
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

        int expr_is_real = (expr_type == REAL_TYPE) || semcheck_expr_is_real_family(expr);

        if (!is_integer_type(expr_type) && expr_type != STRING_TYPE && expr_type != SHORTSTRING_TYPE &&
            expr_type != BOOL && expr_type != POINTER_TYPE && !expr_is_real &&
            expr_type != CHAR_TYPE && expr_type != ENUM_TYPE && !expr_is_char_array &&
            expr_type != UNKNOWN_TYPE && expr_type != RECORD_TYPE &&
            expr_type != PROCEDURE)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, write argument %d must be integer, longint, real, boolean, string, pointer, or enum.\n",
                    stmt->line_num, arg_index);
            ++return_val;
        }

        if (expr != NULL && expr->field_width != NULL)
        {
            int width_type = UNKNOWN_TYPE;
            return_val += semcheck_stmt_expr_tag(&width_type, symtab, expr->field_width, INT_MAX, NO_MUTATE);
            if (!is_integer_type(width_type))
            {
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, field width for argument %d must be an integer.\n",
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
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, field precision for argument %d must be an integer.\n",
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, WriteStr requires at least one argument.\n",
                stmt->line_num);
        return 1;
    }

    /* First argument must be a string variable (var parameter) */
    struct Expression *dest_expr = (struct Expression *)args->cur;
    int dest_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&dest_type, symtab, dest_expr, max_scope_lev, MUTATE);
    
    if (dest_type != STRING_TYPE && dest_type != SHORTSTRING_TYPE)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, WriteStr first argument must be a string variable.\n",
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

        int expr_is_real = (expr_type == REAL_TYPE) || semcheck_expr_is_real_family(expr);

        if (!is_integer_type(expr_type) && expr_type != STRING_TYPE && expr_type != SHORTSTRING_TYPE && 
            expr_type != BOOL && expr_type != POINTER_TYPE && !expr_is_real && 
            expr_type != CHAR_TYPE && expr_type != ENUM_TYPE)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, WriteStr argument %d must be integer, real, boolean, string, pointer, or enum.\n",
                    stmt->line_num, arg_index);
            ++return_val;
        }

        args = args->next;
        ++arg_index;
    }

    return return_val;
}

static int semcheck_expr_is_real_family(const struct Expression *expr)
{
    return (expr != NULL &&
        expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_real(expr->resolved_kgpc_type));
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
        
        if (!is_integer_type(expr_type) && expr_type != CHAR_TYPE && expr_type != STRING_TYPE &&
            expr_type != REAL_TYPE && !semcheck_expr_is_real_family(expr))
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, read argument %d must be integer, longint, real, char, or string variable.\n",
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

/*
 * Per-parameter penalty classification for write/writeln overload resolution.
 * Lower penalty = better match.  Penalties are summed across all parameters
 * to produce a total candidate penalty.
 *
 * The penalty tiers are:
 *   EXACT (0)        — types match exactly
 *   INT_PROMOTION (1) — Int ↔ LongInt interchangeable
 *   STRING_SUBTYPE (2) — STRING_TYPE matches but UnicodeString is less preferred
 *   FILE_SUBTYPE (3)  — FILE_TYPE matches but TypedFile doesn't match plain File
 *   INCOMPATIBLE (1000) — no valid conversion
 */
enum {
    WRITE_PENALTY_EXACT = 0,
    WRITE_PENALTY_INT_PROMOTION = 1,
    WRITE_PENALTY_STRING_SUBTYPE = 2,
    WRITE_PENALTY_FILE_SUBTYPE = 3,
    WRITE_PENALTY_INCOMPATIBLE = 1000
};

static int semcheck_write_param_penalty(Tree_t *formal_decl, int formal_type, int actual_type)
{
    if (formal_type == UNKNOWN_TYPE || actual_type == UNKNOWN_TYPE)
        return WRITE_PENALTY_EXACT;

    if (formal_type == actual_type)
    {
        /* When both are STRING_TYPE, prefer RawByteString over UnicodeString.
         * RawByteString is FPC's catch-all byte string type; UnicodeString
         * requires codepage conversion. */
        if (formal_type == STRING_TYPE)
        {
            const char *formal_type_id = (formal_decl != NULL && formal_decl->type == TREE_VAR_DECL)
                ? formal_decl->tree_data.var_decl_data.type_id : NULL;
            if (formal_type_id != NULL && strcasecmp(formal_type_id, "UnicodeString") == 0)
                return WRITE_PENALTY_STRING_SUBTYPE;
        }
        /* When both are FILE_TYPE, prefer plain File over TypedFile.
         * A TypedFile formal should not match a plain File actual. */
        if (formal_type == FILE_TYPE)
        {
            const char *formal_type_id = (formal_decl != NULL && formal_decl->type == TREE_VAR_DECL)
                ? formal_decl->tree_data.var_decl_data.type_id : NULL;
            if (formal_type_id != NULL && strcasecmp(formal_type_id, "TypedFile") == 0)
                return WRITE_PENALTY_FILE_SUBTYPE;
        }
        return WRITE_PENALTY_EXACT;
    }

    if ((formal_type == LONGINT_TYPE && actual_type == INT_TYPE) ||
        (formal_type == INT_TYPE && actual_type == LONGINT_TYPE))
        return WRITE_PENALTY_INT_PROMOTION;

    return WRITE_PENALTY_INCOMPATIBLE;
}

/*
 * Compute the total penalty for a write/writeln candidate by summing
 * per-parameter penalties.  Used for both initial scoring and tie-breaking
 * recomputation (eliminating code duplication).
 */
static int semcheck_write_candidate_total_penalty(HashNode_t *candidate,
    ListNode_t *actual_args, SymTab_t *symtab, int max_scope_lev)
{
    assert(candidate != NULL);
    assert(candidate->type != NULL);
    assert(candidate->type->kind == TYPE_KIND_PROCEDURE);

    ListNode_t *formal = candidate->type->info.proc_info.params;
    ListNode_t *actual = actual_args;
    int total = 0;

    while (formal != NULL && actual != NULL)
    {
        Tree_t *formal_decl = (Tree_t *)formal->cur;
        struct Expression *actual_expr = (struct Expression *)actual->cur;
        int formal_type = resolve_param_type(formal_decl, symtab);
        int actual_type = UNKNOWN_TYPE;
        semcheck_stmt_expr_tag(&actual_type, symtab, actual_expr, max_scope_lev, NO_MUTATE);

        total += semcheck_write_param_penalty(formal_decl, formal_type, actual_type);

        formal = formal->next;
        actual = actual->next;
    }

    return total;
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
        int best_penalty = WRITE_PENALTY_INCOMPATIBLE + 1;
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

            int penalty = semcheck_write_candidate_total_penalty(candidate,
                stmt->stmt_data.procedure_call_data.expr_args, symtab, max_scope_lev);

            if (penalty < best_penalty)
            {
                best_penalty = penalty;
                best_match = candidate;
                num_best = 1;
            }
            else if (penalty == best_penalty)
            {
                num_best++;
            }
        }

        /* When multiple candidates tie, check if they all share the same
         * mangled_id (e.g. File and TypedFile overloads both mangling as
         * assign_f_rbs).  If so, they're effectively the same overload. */
        if (num_best > 1 && best_match != NULL && best_match->mangled_id != NULL)
        {
            int all_same = 1;
            for (ListNode_t *cur2 = candidates; cur2 != NULL && all_same; cur2 = cur2->next)
            {
                HashNode_t *c2 = (HashNode_t *)cur2->cur;
                if (c2 == NULL || c2->type == NULL || c2->type->kind != TYPE_KIND_PROCEDURE)
                    continue;
                if (ListLength(c2->type->info.proc_info.params) != call_arg_count)
                    continue;
                int c2_penalty = semcheck_write_candidate_total_penalty(c2,
                    stmt->stmt_data.procedure_call_data.expr_args, symtab, max_scope_lev);
                if (c2_penalty == best_penalty && c2->mangled_id != NULL &&
                    strcmp(c2->mangled_id, best_match->mangled_id) != 0)
                {
                    all_same = 0;
                }
            }
            if (all_same)
                num_best = 1;
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
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, failed to allocate Halt argument.\n", stmt->line_num);
            return 1;
        }
        stmt->stmt_data.procedure_call_data.expr_args = CreateListNode(zero_expr, LIST_EXPR);
        return semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
    }

    if (args->next != NULL)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Halt expects zero or one argument.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    struct Expression *code_expr = (struct Expression *)args->cur;
    int code_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&code_type, symtab, code_expr, max_scope_lev, NO_MUTATE);
    return_val += semcheck_set_stmt_call_mangled_id(symtab, stmt, max_scope_lev);
    return return_val;
}

static int semcheck_builtin_error(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Error expects exactly one argument.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    struct Expression *code_expr = (struct Expression *)args->cur;
    int code_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&code_type, symtab, code_expr, max_scope_lev, NO_MUTATE);

    /* System.Error(code) follows Halt semantics in our runtime.
     * Lower to Halt so existing mangling/codegen/runtime paths are reused. */
    if (!pascal_identifier_equals(stmt->stmt_data.procedure_call_data.id, "Halt"))
    {
        char *halt_name = strdup("Halt");
        if (halt_name == NULL)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, failed to rewrite Error call to Halt.\n", stmt->line_num);
            return return_val + 1;
        }

        free(stmt->stmt_data.procedure_call_data.id);
        stmt->stmt_data.procedure_call_data.id = halt_name;
    }

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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, GetMem expects one or two arguments.\n", stmt->line_num);
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, FreeMem expects one or two arguments.\n", stmt->line_num);
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Move expects exactly three arguments.\n", stmt->line_num);
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, ReallocMem expects exactly two arguments.\n", stmt->line_num);
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

static int semcheck_builtin_new(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_count = ListLength(args);
    if (args == NULL || arg_count > 2)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, New expects one or two arguments.\\n", stmt->line_num);
        return 1;
    }

    struct Expression *target_expr = (struct Expression *)args->cur;
    int pointer_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&pointer_type, symtab, target_expr, max_scope_lev, MUTATE);

    if (pointer_type != POINTER_TYPE)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, New expects a pointer variable argument.\\n", stmt->line_num);
        return ++return_val;
    }

    if (target_expr->pointer_subtype == UNKNOWN_TYPE && target_expr->pointer_subtype_id == NULL)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, unable to determine allocation type for New.\\n", stmt->line_num);
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
    int arg_count = ListLength(args);
    if (args == NULL || arg_count > 2)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Dispose expects one or two arguments.\\n", stmt->line_num);
        return 1;
    }

    struct Expression *target_expr = (struct Expression *)args->cur;
    int pointer_type = UNKNOWN_TYPE;
    return_val += semcheck_stmt_expr_tag(&pointer_type, symtab, target_expr, max_scope_lev, MUTATE);

    if (pointer_type != POINTER_TYPE && pointer_type != UNKNOWN_TYPE)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Dispose expects a pointer variable argument.\\n", stmt->line_num);
        return ++return_val;
    }

    return return_val;
}

/* Semantic check on a normal statement */
int semcheck_stmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt != NULL && stmt->type == STMT_PROCEDURE_CALL)
    {
        int is_dispose = 0;
        struct Statement *extra_stmt = transform_two_arg_new_dispose(stmt, &is_dispose);
        if (extra_stmt != NULL)
        {
            struct Statement *base_stmt = (struct Statement *)calloc(1, sizeof(struct Statement));
            ListNode_t *first = (ListNode_t *)calloc(1, sizeof(ListNode_t));
            ListNode_t *second = (ListNode_t *)calloc(1, sizeof(ListNode_t));
            if (base_stmt == NULL || first == NULL || second == NULL)
            {
                if (base_stmt != NULL)
                    free(base_stmt);
                if (first != NULL)
                    free(first);
                if (second != NULL)
                    free(second);
                destroy_stmt(extra_stmt);
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                    "Error on line %d, unable to allocate statement nodes for New/Dispose transform.\n",
                    stmt->line_num);
                return 1;
            }

            *base_stmt = *stmt;
            first->type = LIST_STMT;
            second->type = LIST_STMT;
            if (is_dispose)
            {
                first->cur = extra_stmt;
                second->cur = base_stmt;
            }
            else
            {
                first->cur = base_stmt;
                second->cur = extra_stmt;
            }
            first->next = second;
            second->next = NULL;

            stmt->type = STMT_COMPOUND_STATEMENT;
            stmt->stmt_data.compound_statement = first;
        }
    }

    int ret = semcheck_stmt_main(symtab, stmt, max_scope_lev);
    if (ret > 0 && kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL && stmt != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_ERRORS] stmt_error type=%d line=%d col=%d src=%d ret=%d\n",
            stmt->type, stmt->line_num, stmt->col_num, stmt->source_index, ret);
    }
    return ret;
}

/* Semantic check on a function statement (no side effects allowed) */
int semcheck_func_stmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int ret = semcheck_stmt_main(symtab, stmt, max_scope_lev);
    if (ret > 0 && kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL && stmt != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_ERRORS] func_stmt_error type=%d line=%d col=%d src=%d ret=%d\n",
            stmt->type, stmt->line_num, stmt->col_num, stmt->source_index, ret);
    }
    return ret;
}

static int semcheck_break_stmt(struct Statement *stmt)
{
    if (semcheck_loop_depth <= 0)
    {
        if (stmt != NULL)
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Break is only valid inside a loop.\n", stmt->line_num);
        return 1;
    }
    return 0;
}

static int semcheck_continue_stmt(struct Statement *stmt)
{
    if (semcheck_loop_depth <= 0)
    {
        if (stmt != NULL)
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Continue is only valid inside a loop.\n", stmt->line_num);
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
        const char *limit_env = kgpc_getenv("KGPC_DEBUG_SEMSTMT_LIMIT");
        if (limit_env != NULL)
            semcheck_stmt_limit = atol(limit_env);
        semcheck_stmt_limit_inited = 1;
    }
    if (semcheck_stmt_log_enabled == -1) {
        semcheck_stmt_log_enabled = kgpc_getenv("KGPC_DEBUG_SEMSTMT") != NULL;
    }
    if (semcheck_stmt_verbose == -1) {
        semcheck_stmt_verbose = kgpc_getenv("KGPC_DEBUG_SEMSTMT_VERBOSE") != NULL;
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
            if (stmt->stmt_data.procedure_call_data.id != NULL &&
                pascal_identifier_equals(stmt->stmt_data.procedure_call_data.id, "fail") &&
                stmt->stmt_data.procedure_call_data.expr_args == NULL &&
                semcheck_current_subprogram_is_constructor_fallback(symtab))
            {
                stmt->type = STMT_EXIT;
                memset(&stmt->stmt_data, 0, sizeof(stmt->stmt_data));
                stmt->stmt_data.exit_data.return_expr = NULL;
                return_val += semcheck_stmt_main(symtab, stmt, max_scope_lev);
                break;
            }
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
                    if (FindSymbol(&result_node, symtab, "Result") != 0 && result_node != NULL)
                    {
                        result_node->mutated = MUTATE;
                        if (result_node->type != NULL && return_expr->resolved_kgpc_type != NULL)
                        {
                            int return_owned = 0;
                            struct Expression *result_expr = mk_varid(stmt->line_num, strdup("Result"));
                            if (!are_types_compatible_for_assignment(result_node->type,
                                                                    return_expr->resolved_kgpc_type, symtab))
                            {
                                if (!semcheck_try_record_conversion_expression(symtab,
                                        &stmt->stmt_data.exit_data.return_expr,
                                        result_expr,
                                        result_node->type,
                                        &return_expr->resolved_kgpc_type,
                                        &return_owned))
                                {
                                    semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, incompatible return type in exit().\n",
                                        stmt->line_num);
                                    ++return_val;
                                }
                            }
                            destroy_expr(result_expr);
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
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, WITH statement requires a context expression.\\n\\n",
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
            return_val += semcheck_statement_list_nodes(symtab, stmt->stmt_data.try_except_data.except_statements, max_scope_lev);
            break;

        case STMT_ON_EXCEPTION:
            if (stmt->stmt_data.on_exception_data.exception_var_name != NULL) {
                char *var_name = stmt->stmt_data.on_exception_data.exception_var_name;
                char *type_name = stmt->stmt_data.on_exception_data.exception_type_name;
                KgpcType *var_kgpc_type = NULL;

                EnterScope(symtab, 0);

                if (type_name != NULL) {
                    HashNode_t *type_node = NULL;
                    if (FindSymbol(&type_node, symtab, type_name) != 0 && type_node != NULL) {
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

                PushVarOntoScope_Typed(symtab, var_name, var_kgpc_type);
                if (stmt->stmt_data.on_exception_data.handler_stmt != NULL)
                    return_val += semcheck_stmt_main(symtab,
                        stmt->stmt_data.on_exception_data.handler_stmt, max_scope_lev);
                LeaveScope(symtab);
            } else if (stmt->stmt_data.on_exception_data.handler_stmt != NULL) {
                return_val += semcheck_stmt_main(symtab,
                    stmt->stmt_data.on_exception_data.handler_stmt, max_scope_lev);
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
                if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL)
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
                        const char *owner_name_from_node = NULL;
                        {
                            HashNode_t *call_method_node = NULL;
                            if (method_name != NULL &&
                                FindSymbol(&call_method_node, symtab, method_name) != 0 &&
                                call_method_node != NULL)
                            {
                                if (call_method_node->method_name != NULL)
                                    method_name = call_method_node->method_name;
                                owner_name_from_node = call_method_node->owner_class;
                            }
                        }
                        HashNode_t *self_node = NULL;
                        const char *parent_class_name = NULL;
                        struct RecordType *current_class = NULL;

                        if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL &&
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
                                if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL && method_name != NULL &&
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
                                    if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL)
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
                            if (owner_id == NULL && owner_name_from_node != NULL)
                                owner_id = owner_name_from_node;
                            if (owner_id != NULL)
                            {
                                HashNode_t *owner_node = NULL;
                                if (FindSymbol(&owner_node, symtab, owner_id) != 0 && owner_node != NULL)
                                    current_class = semcheck_stmt_get_record_type_from_node(owner_node);
                                if (current_class != NULL)
                                    parent_class_name = current_class->parent_class_name;
                            }
                        }
                        if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL && method_name != NULL &&
                            strcasecmp(method_name, "Create") == 0)
                        {
                            fprintf(stderr,
                                "[INHERITED] resolved class=%s parent=%s\n",
                                current_class && current_class->type_id ? current_class->type_id : "<null>",
                                parent_class_name ? parent_class_name : "<null>");
                        }
                        /* method_name and owner_name_from_node point to HashNode fields,
                         * no need to free */

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

                                    /* Fallback: use semcheck_resolve_overload when exact mangling
                                     * doesn't match (e.g., dynamic array types with different alias names) */
                                    if (parent_method_node == NULL)
                                    {
                                        HashNode_t *best_match = NULL;
                                        int num_best = 0;
                                        semcheck_resolve_overload(&best_match, &num_best,
                                            parent_candidates, self_arg, symtab, call_expr, INT_MAX, 0);
                                        if (best_match != NULL && num_best == 1)
                                            parent_method_node = best_match;
                                    }

                                    self_arg->next = NULL;
                                    destroy_expr(self_expr);
                                    free(self_arg);

                                    DestroyList(parent_candidates);
                                }
                                else
                                {
                                    if (FindSymbol(&parent_method_node, symtab, parent_mangled) == 0)
                                        parent_method_node = NULL;
                                }

                                if (parent_method_node == NULL)
                                {
                                    HashNode_t *parent_node = NULL;
                                    if (FindSymbol(&parent_node, symtab, (char *)search_parent) != 0 &&
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

                            if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL)
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
                            /* temp_self_arg now belongs to call_expr/temp_args for codegen. */
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
    int lhs_was_typecast;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_VAR_ASSIGN);

    return_val = 0;

    int module_property_result = semcheck_try_module_property_assignment(symtab, stmt, max_scope_lev);
    if (module_property_result >= 0)
        return module_property_result;

    var = stmt->stmt_data.var_assign_data.var;
    expr = stmt->stmt_data.var_assign_data.expr;
    lhs_was_typecast = (var != NULL && var->type == EXPR_TYPECAST);

    rewrite_tfpglist_constructor_if_needed(symtab, max_scope_lev, var,
        &stmt->stmt_data.var_assign_data.expr);
    expr = stmt->stmt_data.var_assign_data.expr;

    /* NOTE: Grammar will make sure the left side is a variable */
    /* Left side var assigns must abide by scoping rules */
    if (SEMSTMT_TIMINGS_ENABLED()) {
        double t0 = semstmt_now_ms();
        int before_lhs = return_val;
        return_val += semcheck_stmt_expr_tag(&type_first, symtab, var, max_scope_lev, MUTATE);
        fprintf(stderr, "[timing] varassign lhs semcheck_stmt_expr_tag: %.2f ms (line=%d)\n",
                semstmt_now_ms() - t0, stmt->line_num);
        if (kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL && return_val > before_lhs && var != NULL)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_ERRORS] varassign_lhs_error line=%d expr_type=%d\n",
                stmt->line_num, var->type);
            if (var->type == EXPR_RECORD_ACCESS && var->expr_data.record_access_data.field_id != NULL)
                fprintf(stderr, "[KGPC_DEBUG_ERRORS]   lhs record field=%s\n",
                    var->expr_data.record_access_data.field_id);
        }
    } else {
        int before_lhs = return_val;
        return_val += semcheck_stmt_expr_tag(&type_first, symtab, var, max_scope_lev, MUTATE);
        if (kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL && return_val > before_lhs && var != NULL)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_ERRORS] varassign_lhs_error line=%d expr_type=%d\n",
                stmt->line_num, var->type);
            if (var->type == EXPR_RECORD_ACCESS && var->expr_data.record_access_data.field_id != NULL)
                fprintf(stderr, "[KGPC_DEBUG_ERRORS]   lhs record field=%s\n",
                    var->expr_data.record_access_data.field_id);
        }
    }

    if (var != NULL && var->type == EXPR_TYPECAST)
    {
        struct Expression *inner = var->expr_data.typecast_data.expr;
        if (inner != NULL)
        {
            /* Don't strip string typecasts on pointer targets (e.g.,
             * RawByteString(ptr):='') — codegen needs the typecast to know
             * this is a string assignment, not a direct pointer store. */
            int target_type = var->expr_data.typecast_data.target_type;
            int inner_prim_tag = (inner->resolved_kgpc_type != NULL)
                ? inner->resolved_kgpc_type->info.primitive_type_tag : UNKNOWN_TYPE;
            int is_string_to_pointer = is_string_type(target_type) &&
                (inner_prim_tag == POINTER_TYPE || inner_prim_tag == UNKNOWN_TYPE);

            int compatible = 0;
            KgpcType *lhs_type = var->resolved_kgpc_type;
            KgpcType *inner_type = inner->resolved_kgpc_type;
            if (lhs_type != NULL && inner_type != NULL)
                compatible = are_types_compatible_for_assignment(lhs_type, inner_type, symtab);
            else if (lhs_type == NULL && inner_type == NULL)
                compatible = 1;
            if (!compatible)
            {
                TypeRef *target_ref = var->expr_data.typecast_data.target_type_ref;
                if (target_ref != NULL && target_ref->num_generic_args > 0)
                    compatible = 1;
            }
            if (compatible && !is_string_to_pointer)
            {
                stmt->stmt_data.var_assign_data.var = inner;
                var->expr_data.typecast_data.expr = NULL;
                destroy_expr(var);
                var = inner;
            }
        }
    }

    /* Check for record property assignment early, before RHS type checking.
     * This handles plain record (Delphi advanced record) properties with setter methods.
     * Must happen after LHS is evaluated but before type compatibility checks. */
    {
        int property_result = semcheck_try_property_assignment(symtab, stmt, max_scope_lev);
        if (property_result >= 0)
            return return_val + property_result;
    }
    {
        int indexed_property_result = semcheck_try_indexed_property_assignment(symtab, stmt, max_scope_lev);
        if (indexed_property_result >= 0)
            return return_val + indexed_property_result;
    }

    /* Re-read var and expr from the statement after the property checks and
     * LHS semcheck_stmt_expr_tag.  The LHS expression may have been
     * transformed in-place (e.g. VAR_ID -> RECORD_ACCESS via WITH, or
     * FUNCTION_CALL -> TYPECAST), and the property helpers may in the future
     * rewrite the statement even on the failure path.  Keeping the local
     * pointers in sync avoids stale-pointer / use-after-free crashes. */
    var = stmt->stmt_data.var_assign_data.var;
    expr = stmt->stmt_data.var_assign_data.expr;

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
            if (FindSymbol(&var_node, symtab, var->expr_data.id) != 0 && var_node != NULL)
            {
                record_type = hashnode_get_record_type(var_node);
                if (record_type == NULL)
                {
                    struct TypeAlias *alias = hashnode_get_type_alias(var_node);
                    if (alias != NULL && alias->target_type_id != NULL)
                    {
                        HashNode_t *target_node = NULL;
                        if (FindSymbol(&target_node, symtab, alias->target_type_id) != 0 &&
                            target_node != NULL)
                            record_type = hashnode_get_record_type(target_node);
                    }
                }
            }
        }
        if (record_type == NULL && var != NULL && var->resolved_kgpc_type != NULL)
        {
            const char *record_id = semcheck_record_type_id_from_kgpc(var->resolved_kgpc_type);
            if (record_id != NULL)
                record_type = semcheck_lookup_record_type(symtab, record_id);
        }
        if (record_type == NULL && var != NULL)
        {
            int lhs_owned = 0;
            KgpcType *lhs_type = semcheck_resolve_expression_kgpc_type(
                symtab, var, max_scope_lev, MUTATE, &lhs_owned);
            if (lhs_type != NULL)
            {
                if (kgpc_type_is_record(lhs_type))
                    record_type = kgpc_type_get_record(lhs_type);
                else if (kgpc_type_is_pointer(lhs_type) && lhs_type->info.points_to != NULL &&
                    kgpc_type_is_record(lhs_type->info.points_to))
                    record_type = kgpc_type_get_record(lhs_type->info.points_to);
                if (record_type == NULL)
                {
                    const char *record_id = semcheck_record_type_id_from_kgpc(lhs_type);
                    if (record_id != NULL)
                        record_type = semcheck_lookup_record_type(symtab, record_id);
                }
            }
            if (lhs_owned && lhs_type != NULL)
                destroy_kgpc_type(lhs_type);
        }
        if (record_type == NULL && var != NULL && var->type == EXPR_RECORD_ACCESS)
        {
            struct Expression *record_expr = var->expr_data.record_access_data.record_expr;
            const char *field_id = var->expr_data.record_access_data.field_id;
            int record_owned = 0;
            KgpcType *record_expr_type = semcheck_resolve_expression_kgpc_type(
                symtab, record_expr, max_scope_lev, MUTATE, &record_owned);
            if (record_expr_type != NULL)
            {
                if (kgpc_type_is_pointer(record_expr_type) &&
                    record_expr_type->info.points_to != NULL)
                    record_expr_type = record_expr_type->info.points_to;
                if (kgpc_type_is_record(record_expr_type) && field_id != NULL)
                {
                    struct RecordType *record_info = kgpc_type_get_record(record_expr_type);
                    struct RecordField *field_desc = NULL;
                    long long field_offset = 0;
                    if (record_info != NULL &&
                        resolve_record_field(symtab, record_info, field_id,
                            &field_desc, &field_offset, stmt->line_num, 0) == 0 &&
                        field_desc != NULL)
                    {
                        if (field_desc->nested_record != NULL)
                        {
                            record_type = field_desc->nested_record;
                        }
                        else if (field_desc->type_id != NULL)
                        {
                            record_type = semcheck_lookup_record_type(symtab, field_desc->type_id);
                            if (record_type == NULL)
                            {
                                HashNode_t *alias_node = NULL;
                                if (FindSymbol(&alias_node, symtab, field_desc->type_id) != 0 &&
                                    alias_node != NULL)
                                {
                                    struct TypeAlias *alias = get_type_alias_from_node(alias_node);
                                    if (alias != NULL && alias->target_type_id != NULL)
                                    {
                                        record_type = semcheck_lookup_record_type(symtab, alias->target_type_id);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (record_owned && record_expr_type != NULL)
                destroy_kgpc_type(record_expr_type);
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
    if (expr != NULL && expr->type == EXPR_ARRAY_LITERAL &&
        expr->array_element_type == UNKNOWN_TYPE &&
        expr->array_element_type_id == NULL &&
        var != NULL && var->resolved_kgpc_type != NULL)
    {
        KgpcType *lhs_type = var->resolved_kgpc_type;
        if (kgpc_type_is_pointer(lhs_type) && lhs_type->info.points_to != NULL &&
            kgpc_type_is_array(lhs_type->info.points_to))
        {
            lhs_type = lhs_type->info.points_to;
        }
        if (kgpc_type_is_array(lhs_type))
        {
            KgpcType *lhs_elem = kgpc_type_get_array_element_type_resolved(lhs_type, symtab);
            if (lhs_elem != NULL)
            {
                int elem_tag = semcheck_tag_from_kgpc(lhs_elem);
                if (expr->array_element_type == UNKNOWN_TYPE)
                    expr->array_element_type = elem_tag;
                if (lhs_elem->kind == TYPE_KIND_RECORD &&
                    lhs_elem->info.record_info != NULL)
                {
                    expr->array_element_record_type = lhs_elem->info.record_info;
                    if (expr->array_element_type_id == NULL &&
                        lhs_elem->info.record_info->type_id != NULL)
                    {
                        expr->array_element_type_id = strdup(lhs_elem->info.record_info->type_id);
                    }
                }
                if (expr->array_element_type_id == NULL &&
                    lhs_elem->type_alias != NULL &&
                    lhs_elem->type_alias->target_type_id != NULL)
                {
                    expr->array_element_type_id = strdup(lhs_elem->type_alias->target_type_id);
                }
            }
            if (expr->array_element_type_id == NULL &&
                lhs_type->info.array_info.element_type_id != NULL)
            {
                expr->array_element_type_id = strdup(lhs_type->info.array_info.element_type_id);
            }
        }
    }
    if (SEMSTMT_TIMINGS_ENABLED()) {
        double t0 = semstmt_now_ms();
        int before_rhs = return_val;
        return_val += semcheck_stmt_expr_tag(&type_second, symtab, expr, INT_MAX, NO_MUTATE);
        fprintf(stderr, "[timing] varassign rhs semcheck_stmt_expr_tag: %.2f ms (line=%d)\n",
                semstmt_now_ms() - t0, stmt->line_num);
        if (kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL && return_val > before_rhs && expr != NULL)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_ERRORS] varassign_rhs_error line=%d expr_type=%d\n",
                stmt->line_num, expr->type);
            if (expr->type == EXPR_FUNCTION_CALL && expr->expr_data.function_call_data.id != NULL)
                fprintf(stderr, "[KGPC_DEBUG_ERRORS]   rhs func=%s\n",
                    expr->expr_data.function_call_data.id);
            if (expr->type == EXPR_RECORD_ACCESS && expr->expr_data.record_access_data.field_id != NULL)
                fprintf(stderr, "[KGPC_DEBUG_ERRORS]   rhs record field=%s\n",
                    expr->expr_data.record_access_data.field_id);
        }
    } else {
        int before_rhs = return_val;
        return_val += semcheck_stmt_expr_tag(&type_second, symtab, expr, INT_MAX, NO_MUTATE);
        if (kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL && return_val > before_rhs && expr != NULL)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_ERRORS] varassign_rhs_error line=%d expr_type=%d\n",
                stmt->line_num, expr->type);
            if (expr->type == EXPR_FUNCTION_CALL && expr->expr_data.function_call_data.id != NULL)
                fprintf(stderr, "[KGPC_DEBUG_ERRORS]   rhs func=%s\n",
                    expr->expr_data.function_call_data.id);
            if (expr->type == EXPR_RECORD_ACCESS && expr->expr_data.record_access_data.field_id != NULL)
                fprintf(stderr, "[KGPC_DEBUG_ERRORS]   rhs record field=%s\n",
                    expr->expr_data.record_access_data.field_id);
        }
    }

    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr != NULL && expr->type == EXPR_FUNCTION_CALL &&
        expr->expr_data.function_call_data.id != NULL &&
        strcasecmp(expr->expr_data.function_call_data.id, "Create") == 0) {
        fprintf(stderr, "[SemCheck] semcheck_varassign calling semcheck_resolve_expression_kgpc_type:\n");
        fprintf(stderr, "[SemCheck]   expr=%p type=%d\n", (void*)expr, expr->type);
        fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type=%p\n", (void*)expr->resolved_kgpc_type);
    }
    
    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr != NULL && expr->type == EXPR_RECORD_ACCESS) {
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
                /* For methods, cur_id is mangled (e.g. "TEReader__ReadNext").
                 * Also check against just the method name part after "__". */
                const char *method_name = semcheck_get_current_subprogram_method_name();
                int is_result_assign = pascal_identifier_equals(var->expr_data.id, "Result") ||
                                       pascal_identifier_equals(var->expr_data.id, cur_id) ||
                                       (method_name != NULL && pascal_identifier_equals(var->expr_data.id, method_name)) ||
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

        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
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
            if (FindSymbol(&rhs_node, symtab, expr->expr_data.id) != 0 &&
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
        if (expr != NULL && expr->type == EXPR_SET &&
            kgpc_type_is_array(lhs_kgpctype))
        {
            if (semcheck_convert_set_literal_to_array_literal(expr) != 0)
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, unable to convert set literal to array literal.\n\n",
                    stmt->line_num);
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
            if (kgpc_getenv("KGPC_DEBUG_ARRAY_ASSIGN") != NULL)
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

        if ((semcheck_type_is_recordish(lhs_kgpctype) &&
             !semcheck_type_is_recordish(rhs_kgpctype)) ||
            (!semcheck_type_is_recordish(lhs_kgpctype) &&
             semcheck_type_is_recordish(rhs_kgpctype)))
        {
            if (semcheck_try_record_assignment_operator(symtab, stmt, lhs_kgpctype,
                    &rhs_kgpctype, &rhs_owned))
            {
                expr = stmt->stmt_data.var_assign_data.expr;
                type_second = semcheck_tag_from_kgpc(rhs_kgpctype);
                goto assignment_types_ok;
            }
        }

        if (!are_types_compatible_for_assignment(lhs_kgpctype, rhs_kgpctype, symtab))
        {
            if (var != NULL && expr != NULL &&
                var->pointer_subtype_id != NULL &&
                expr->pointer_subtype_id != NULL &&
                semcheck_class_type_ids_compatible(symtab,
                    var->pointer_subtype_id, expr->pointer_subtype_id))
            {
                goto assignment_types_ok;
            }
            if (semcheck_try_record_assignment_operator(symtab, stmt, lhs_kgpctype,
                    &rhs_kgpctype, &rhs_owned))
            {
                expr = stmt->stmt_data.var_assign_data.expr;
                type_second = semcheck_tag_from_kgpc(rhs_kgpctype);
                goto assignment_types_ok;
            }

            int allow_char_literal = 0;
            if (semcheck_type_is_char_like(lhs_kgpctype) &&
                semcheck_force_char_case_builtin_in_assignment(expr))
                goto assignment_types_ok;
            if (semcheck_type_is_char_like(lhs_kgpctype) &&
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

            /* Allow assigning Char ordinal constants to Char targets (FPC-compatible). */
            if (semcheck_type_is_char_like(lhs_kgpctype) &&
                rhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
                is_integer_type(rhs_kgpctype->info.primitive_type_tag) &&
                semcheck_expr_is_char_ordinal_const(symtab, expr))
            {
                semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
                goto assignment_types_ok;
            }

            /* Allow assigning Char / Char ordinal constants to String targets. */
            if (lhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
                lhs_kgpctype->info.primitive_type_tag == STRING_TYPE &&
                rhs_kgpctype->kind == TYPE_KIND_PRIMITIVE &&
                (rhs_kgpctype->info.primitive_type_tag == CHAR_TYPE ||
                 (is_integer_type(rhs_kgpctype->info.primitive_type_tag) &&
                  semcheck_expr_is_char_ordinal_const(symtab, expr))))
            {
                if (rhs_kgpctype->info.primitive_type_tag != CHAR_TYPE)
                    semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
                goto assignment_types_ok;
            }

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

            if (kgpc_getenv("KGPC_DEBUG_RESULT") != NULL && var != NULL &&
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

            if (lhs_was_typecast)
                goto assignment_types_ok;

            /* Allow record = integer/pointer and integer = record (type helpers) */
            if ((type_first == RECORD_TYPE && (is_integer_type(type_second) || type_second == ENUM_TYPE || type_second == POINTER_TYPE)) ||
                (type_second == RECORD_TYPE && (is_integer_type(type_first) || type_first == ENUM_TYPE || type_first == POINTER_TYPE)) ||
                (type_first == RECORD_TYPE && type_second == RECORD_TYPE))
                goto assignment_types_ok;

            /* Allow char = string (single-char string literal to char) */
            if (type_first == CHAR_TYPE && (type_second == STRING_TYPE || type_second == SHORTSTRING_TYPE))
                goto assignment_types_ok;

            const char *lhs_name = "<expression>";
            if (var != NULL && var->type == EXPR_VAR_ID)
                lhs_name = var->expr_data.id;
            if (kgpc_getenv("KGPC_DEBUG_ASSIGN") != NULL)
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_ASSIGN] line=%d col=%d lhs=%s lhs_type=%s rhs_type=%s\n",
                    stmt->line_num,
                    stmt->col_num,
                    lhs_name,
                    kgpc_type_to_string(lhs_kgpctype),
                    kgpc_type_to_string(rhs_kgpctype));
                semcheck_debug_expr_brief(var, "assign lhs");
                semcheck_debug_expr_brief(expr, "assign rhs");
            }
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
                if (FindSymbol(&rhs_symbol, symtab, expr->expr_data.id) != 0 &&
                    rhs_symbol != NULL && rhs_symbol->hash_type == HASHTYPE_PROCEDURE)
                {
                    /* Transform the expression to EXPR_ADDR_OF_PROC */
                    expr->type = EXPR_ADDR_OF_PROC;
                    expr->expr_data.addr_of_proc_data.proc_mangled_id = rhs_symbol->mangled_id ? strdup(rhs_symbol->mangled_id) : NULL;
                    expr->expr_data.addr_of_proc_data.proc_id = rhs_symbol->id ? strdup(rhs_symbol->id) : NULL;
                    /* Resolve the type NOW while the symbol is still alive. */
                    if (rhs_symbol->type != NULL)
                    {
                        kgpc_type_retain(rhs_symbol->type);
                        expr->resolved_kgpc_type = create_pointer_type(rhs_symbol->type);
                    }
                    else
                    {
                        expr->resolved_kgpc_type = create_pointer_type(NULL);
                    }
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
                    semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, 
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

        if (lhs_was_typecast)
            types_compatible = 1;

        if (!types_compatible)
        {
            if (var != NULL && var->type == EXPR_TYPECAST)
            {
                const struct TypeRef *cast_ref = var->expr_data.typecast_data.target_type_ref;
                if (cast_ref != NULL && cast_ref->num_generic_args > 0)
                    types_compatible = 1;
            }
        }

        if (!types_compatible && type_first != UNKNOWN_TYPE && type_second != UNKNOWN_TYPE)
        {
            if (kgpc_getenv("KGPC_DEBUG_ASSIGN") != NULL)
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_ASSIGN] legacy mismatch line=%d col=%d lhs_type=%d rhs_type=%d lhs_expr_type=%d rhs_expr_type=%d\n",
                    stmt->line_num,
                    stmt->col_num,
                    type_first,
                    type_second,
                    var != NULL ? var->type : -1,
                    expr != NULL ? expr->type : -1);
                semcheck_debug_expr_brief(var, "legacy assign lhs");
                semcheck_debug_expr_brief(expr, "legacy assign rhs");
            }
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

    {
        const char *cur_sub_id = semcheck_get_current_subprogram_id();
        const char *result_var = semcheck_get_current_subprogram_result_var_name();
        const char *method_name = semcheck_get_current_subprogram_method_name();
        int is_result_name =
            (cur_sub_id != NULL && pascal_identifier_equals(prop_name, cur_sub_id)) ||
            (result_var != NULL && pascal_identifier_equals(prop_name, result_var)) ||
            (method_name != NULL && pascal_identifier_equals(prop_name, method_name));
        if (is_result_name)
            return -1;
    }

    /* WITH-context fields must be resolved as assignments on the active record,
     * not rewritten as module-property setter calls. */
    if (with_context_count > 0)
    {
        struct Expression *with_expr = NULL;
        int with_status = semcheck_with_try_resolve(prop_name, symtab, &with_expr, stmt->line_num);
        if (with_expr != NULL)
            destroy_expr(with_expr);
        if (with_status == 0)
            return -1;
    }

    ListNode_t *matches = FindAllIdents(symtab, prop_name);
    HashNode_t *setter = NULL;
    int has_storage_symbol = 0;

    for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node == NULL)
            continue;
        if (node->hash_type == HASHTYPE_VAR || node->hash_type == HASHTYPE_ARRAY ||
            node->hash_type == HASHTYPE_FUNCTION_RETURN)
        {
            has_storage_symbol = 1;
            break;
        }
        /* Enum constants are not assignable, so they should not block
         * module-property setter lookup.  Only non-enum constants
         * (typed consts, literal consts) count as storage symbols. */
        if (node->hash_type == HASHTYPE_CONST)
        {
            int is_enum_literal = (node->type != NULL &&
                node->type->kind == TYPE_KIND_PRIMITIVE &&
                kgpc_type_get_primitive_tag(node->type) == ENUM_TYPE);
            if (!is_enum_literal)
            {
                has_storage_symbol = 1;
                break;
            }
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, property assignment requires an object instance.\n\n",
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, unable to allocate setter argument list.\n\n",
            stmt->line_num);
        destroy_expr(object_expr);
        destroy_expr(value_expr);
        return 1;
    }

    ListNode_t *value_arg = CreateListNode(value_expr, LIST_EXPR);
    if (value_arg == NULL)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, unable to allocate setter argument list.\n\n",
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, unable to prepare property setter call.\n\n",
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
    stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
    stmt->stmt_data.procedure_call_data.placeholder_method_name = NULL;
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
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, setter %s for property %s not found.\n\n",
            stmt->line_num,
            property->write_accessor != NULL ? property->write_accessor : "<unknown>",
            property->name != NULL ? property->name : property_name);
        return 1;
    }

    if (setter_node->hash_type != HASHTYPE_PROCEDURE)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, property setter %s must be a procedure.\n\n",
            stmt->line_num, property->write_accessor);
        return 1;
    }

    return semcheck_convert_property_assignment_to_setter(symtab, stmt, lhs,
        setter_node, max_scope_lev);
}

static int semcheck_try_indexed_property_assignment(SymTab_t *symtab,
    struct Statement *stmt, int max_scope_lev)
{
    if (symtab == NULL || stmt == NULL || stmt->type != STMT_VAR_ASSIGN)
        return -1;

    struct Expression *lhs = stmt->stmt_data.var_assign_data.var;
    struct Expression *rhs = stmt->stmt_data.var_assign_data.expr;
    if (lhs == NULL || rhs == NULL || lhs->type != EXPR_ARRAY_ACCESS)
        return -1;

    struct Expression *array_expr = lhs->expr_data.array_access_data.array_expr;
    struct Expression *index_expr = lhs->expr_data.array_access_data.index_expr;
    if (array_expr == NULL || index_expr == NULL)
        return -1;

    struct Expression *object_expr = NULL;
    const char *property_name = NULL;
    struct RecordType *object_record = NULL;
    struct RecordType *property_owner = NULL;
    struct ClassProperty *property = NULL;

    if (array_expr->type == EXPR_RECORD_ACCESS)
    {
        object_expr = array_expr->expr_data.record_access_data.record_expr;
        property_name = array_expr->expr_data.record_access_data.field_id;
        if (object_expr == NULL || property_name == NULL)
            return -1;

        object_record = semcheck_with_resolve_record_type(symtab,
            object_expr, semcheck_tag_from_kgpc(object_expr->resolved_kgpc_type), stmt->line_num);
        if (object_record == NULL)
            return -1;

        property = semcheck_find_class_property(symtab, object_record, property_name, &property_owner);
    }
    else if (array_expr->type == EXPR_VAR_ID)
    {
        property_name = array_expr->expr_data.id;
        if (property_name == NULL)
            return -1;

        HashNode_t *self_node = NULL;
        if (FindSymbol(&self_node, symtab, "Self") != 0 || self_node == NULL)
            return -1;
        object_record = get_record_type_from_node(self_node);
        if (object_record == NULL)
            return -1;
        property = semcheck_find_class_property(symtab, object_record, property_name, &property_owner);
        /* Create a Self expression for the setter call (instance methods). */
        object_expr = mk_varid(stmt->line_num, strdup("Self"));
    }

    if (property == NULL || property->write_accessor == NULL || !property->is_indexed)
        return -1;

    if (property_owner == NULL)
        property_owner = object_record;

    /* If write accessor is a field, rewrite the array base and let normal array assignment handle it. */
    struct RecordField *write_field =
        semcheck_find_class_field_including_hidden(symtab, object_record, property->write_accessor, NULL);
    if (write_field != NULL)
    {
        if (array_expr->type == EXPR_RECORD_ACCESS)
        {
            if (!pascal_identifier_equals(array_expr->expr_data.record_access_data.field_id,
                    property->write_accessor))
            {
                free(array_expr->expr_data.record_access_data.field_id);
                array_expr->expr_data.record_access_data.field_id = strdup(property->write_accessor);
            }
        }
        else if (array_expr->type == EXPR_VAR_ID)
        {
            struct Expression *self_expr = mk_varid(stmt->line_num, strdup("Self"));
            if (self_expr == NULL)
                return -1;
            free(array_expr->expr_data.id);
            array_expr->expr_data.id = NULL;
            array_expr->type = EXPR_RECORD_ACCESS;
            memset(&array_expr->expr_data.record_access_data, 0,
                sizeof(array_expr->expr_data.record_access_data));
            array_expr->expr_data.record_access_data.record_expr = self_expr;
            array_expr->expr_data.record_access_data.field_id = strdup(property->write_accessor);
            array_expr->expr_data.record_access_data.field_offset = 0;
        }
        return -1;
    }

    HashNode_t *setter_node = semcheck_find_class_method(symtab, property_owner,
        property->write_accessor, NULL);
    if (setter_node == NULL)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, setter %s for property %s not found.\n\n",
            stmt->line_num,
            property->write_accessor != NULL ? property->write_accessor : "<unknown>",
            property->name != NULL ? property->name : property_name);
        return 1;
    }
    if (setter_node->hash_type != HASHTYPE_PROCEDURE)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, property setter %s must be a procedure.\n\n",
            stmt->line_num, property->write_accessor);
        return 1;
    }

    int is_static_setter = 0;
    if (property_owner != NULL && property_owner->type_id != NULL && setter_node->id != NULL)
    {
        is_static_setter = from_cparser_is_method_static(property_owner->type_id, setter_node->id);
    }
    if (!is_static_setter && setter_node->type != NULL &&
        setter_node->type->kind == TYPE_KIND_PROCEDURE)
    {
        ListNode_t *params = kgpc_type_get_procedure_params(setter_node->type);
        if (params == NULL)
            is_static_setter = 1;
    }

    /* Save extra_indices before transformation clears array_access_data */
    ListNode_t *extra_indices = lhs->expr_data.array_access_data.extra_indices;
    lhs->expr_data.array_access_data.extra_indices = NULL;

    /* Detach needed subexpressions before destroying lhs. */
    if (array_expr->type == EXPR_RECORD_ACCESS)
        array_expr->expr_data.record_access_data.record_expr = NULL;
    lhs->expr_data.array_access_data.array_expr = NULL;
    lhs->expr_data.array_access_data.index_expr = NULL;
    stmt->stmt_data.var_assign_data.var = NULL;
    stmt->stmt_data.var_assign_data.expr = NULL;

    destroy_expr(lhs);

    ListNode_t *args_head = NULL;
    ListNode_t *args_tail = NULL;
    if (!is_static_setter)
    {
        if (object_expr == NULL)
            return -1;
        args_head = CreateListNode(object_expr, LIST_EXPR);
        if (args_head == NULL)
        {
            destroy_expr(object_expr);
            destroy_expr(index_expr);
            destroy_expr(rhs);
            return 1;
        }
        args_tail = args_head;
    }
    else if (object_expr != NULL)
    {
        destroy_expr(object_expr);
        object_expr = NULL;
    }

    ListNode_t *index_arg = CreateListNode(index_expr, LIST_EXPR);
    if (index_arg == NULL)
    {
        if (args_head != NULL)
            destroy_expr((struct Expression *)args_head->cur);
        destroy_expr(index_expr);
        destroy_expr(rhs);
        if (args_head != NULL)
            free(args_head);
        return 1;
    }
    if (args_tail != NULL)
        args_tail->next = index_arg;
    else
        args_head = index_arg;
    args_tail = index_arg;

    /* Append extra indices for multi-index properties (e.g. bitmap[x,y]) */
    while (extra_indices != NULL)
    {
        ListNode_t *next = extra_indices->next;
        extra_indices->next = NULL;
        args_tail->next = extra_indices;
        args_tail = extra_indices;
        extra_indices = next;
    }

    ListNode_t *value_arg = CreateListNode(rhs, LIST_EXPR);
    if (value_arg == NULL)
    {
        destroy_expr(rhs);
        return 1;
    }
    args_tail->next = value_arg;

    char *id_copy = setter_node->id != NULL ? strdup(setter_node->id) : NULL;
    char *mangled_copy = NULL;
    if (setter_node->mangled_id != NULL)
        mangled_copy = strdup(setter_node->mangled_id);
    if ((setter_node->id != NULL && id_copy == NULL) ||
        (setter_node->mangled_id != NULL && mangled_copy == NULL))
    {
        free(id_copy);
        free(mangled_copy);
        return 1;
    }

    stmt->type = STMT_PROCEDURE_CALL;
    memset(&stmt->stmt_data.procedure_call_data, 0, sizeof(stmt->stmt_data.procedure_call_data));
    stmt->stmt_data.procedure_call_data.id = id_copy;
    stmt->stmt_data.procedure_call_data.mangled_id = mangled_copy;
    stmt->stmt_data.procedure_call_data.expr_args = args_head;
    stmt->stmt_data.procedure_call_data.resolved_proc = NULL;
    stmt->stmt_data.procedure_call_data.call_hash_type = 0;
    stmt->stmt_data.procedure_call_data.is_call_info_valid = 0;
    stmt->stmt_data.procedure_call_data.is_procedural_var_call = 0;
    stmt->stmt_data.procedure_call_data.procedural_var_symbol = NULL;
    stmt->stmt_data.procedure_call_data.procedural_var_expr = NULL;
    stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
    stmt->stmt_data.procedure_call_data.placeholder_method_name = NULL;
    semcheck_stmt_set_call_kgpc_type(stmt, NULL, 0);

    return semcheck_proccall(symtab, stmt, max_scope_lev);
}

/* ------------------------------------------------------------------ */
/* INTERNPROC handler: fpc_in_Rewrite_TypedFile / fpc_in_Reset_TypedFile
 *
 * When FPC's RTL declares  Procedure Rewrite(var f : TypedFile);
 * [INTERNPROC: fpc_in_Rewrite_TypedFile];  the compiler is expected to
 * determine the element size from the actual file type and transform
 * the 1-arg call into the 2-arg variant: Rewrite(f, SizeOf(ElementType)).
 *
 * Returns 1 if transformation was applied, 0 otherwise.              */
/* ------------------------------------------------------------------ */
static int semcheck_internproc_typedfile_rewrite_reset(
    SymTab_t *symtab, struct Statement *stmt)
{
    if (symtab == NULL || stmt == NULL)
        return 0;

    const char *proc_id = stmt->stmt_data.procedure_call_data.id;
    if (proc_id == NULL)
        return 0;

    /* Only applies to Rewrite / Reset */
    if (!pascal_identifier_equals(proc_id, "Rewrite") &&
        !pascal_identifier_equals(proc_id, "Reset"))
        return 0;

    /* Must have exactly 1 argument */
    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next != NULL)
        return 0;

    /* Check whether any overload has the relevant INTERNPROC tag.
     * This ensures we only transform when using the FPC RTL. */
    const char *expected_tag = pascal_identifier_equals(proc_id, "Rewrite")
        ? "fpc_in_Rewrite_TypedFile"
        : "fpc_in_Reset_TypedFile";
    ListNode_t *candidates = FindAllIdents(symtab, proc_id);
    int found_internproc = 0;
    if (candidates != NULL)
    {
        for (ListNode_t *c = candidates; c != NULL; c = c->next)
        {
            HashNode_t *cand = (HashNode_t *)c->cur;
            if (cand != NULL && cand->internproc_id != NULL &&
                strcasecmp(cand->internproc_id, expected_tag) == 0)
            {
                found_internproc = 1;
                break;
            }
        }
        DestroyList(candidates);
    }
    if (!found_internproc)
    {
        return 0;
    }
    /* Get the first argument — should be a variable of type file-of-X */
    struct Expression *file_expr = (struct Expression *)args->cur;
    if (file_expr == NULL || file_expr->type != EXPR_VAR_ID ||
        file_expr->expr_data.id == NULL)
        return 0;

    HashNode_t *var_node = NULL;
    if (FindSymbol(&var_node, symtab, file_expr->expr_data.id) == 0 ||
        var_node == NULL || var_node->type == NULL)
        return 0;

    /* Look for file element type in the TypeAlias */
    struct TypeAlias *alias = kgpc_type_get_type_alias(var_node->type);
    if (alias == NULL || !alias->is_file)
    {
        /* If no TypeAlias from KgpcType, try looking up the variable's type declaration.
         * The type may be stored on a named type alias in the symbol table. */
        return 0;
    }

    /* Determine element size */
    long long elem_size = 0;
    if (alias->file_type != UNKNOWN_TYPE && alias->file_type != 0)
    {
        elem_size = sizeof_from_type_tag(alias->file_type);
    }
    else if (alias->file_type_id != NULL)
    {
        HashNode_t *elem_type_node = NULL;
        if (FindSymbol(&elem_type_node, symtab, alias->file_type_id) != 0 &&
            elem_type_node != NULL && elem_type_node->type != NULL)
        {
            elem_size = kgpc_type_sizeof(elem_type_node->type);
        }
    }

    if (elem_size <= 0)
        return 0;  /* Can't determine size, let normal resolution handle it */

    /* Inject the element size as a second argument: Rewrite(f, elemSize) */
    struct Expression *size_expr = mk_inum(stmt->line_num, (int)elem_size);
    if (size_expr == NULL)
        return 0;

    ListNode_t *size_arg = CreateListNode(size_expr, LIST_EXPR);
    if (size_arg == NULL)
    {
        destroy_expr(size_expr);
        return 0;
    }

    /* Append size argument after the file argument */
    args->next = size_arg;
    return 1;
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
    int was_unit_qualified = 0;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_PROCEDURE_CALL);

    return_val = 0;

    proc_id = stmt->stmt_data.procedure_call_data.id;
    args_given = stmt->stmt_data.procedure_call_data.expr_args;

    if (args_given != NULL)
    {
        const char *cur_sub_id = semcheck_get_current_subprogram_id();
        const char *result_var = semcheck_get_current_subprogram_result_var_name();
        const char *method_name = semcheck_get_current_subprogram_method_name();
        const char *replacement = (result_var != NULL && result_var[0] != '\0')
            ? result_var : "Result";
        for (ListNode_t *arg_cur = args_given; arg_cur != NULL; arg_cur = arg_cur->next)
        {
            if (arg_cur->type != LIST_EXPR || arg_cur->cur == NULL)
                continue;
            struct Expression *arg_expr = (struct Expression *)arg_cur->cur;
            if (arg_expr->type != EXPR_VAR_ID || arg_expr->expr_data.id == NULL)
                continue;
            const char *arg_id = arg_expr->expr_data.id;
            int is_result_name =
                (cur_sub_id != NULL && pascal_identifier_equals(arg_id, cur_sub_id)) ||
                (result_var != NULL && pascal_identifier_equals(arg_id, result_var)) ||
                (method_name != NULL && pascal_identifier_equals(arg_id, method_name));
            if (!is_result_name)
                continue;

            if (!pascal_identifier_equals(arg_id, replacement))
            {
                /* Don't rename if the original name is a local variable or
                 * parameter (it takes precedence over the result variable). */
                HashNode_t *orig_check = NULL;
                if (symtab != NULL && symtab->current_scope != NULL &&
                    symtab->current_scope->table != NULL)
                {
                    orig_check = FindIdentInTable(symtab->current_scope->table, arg_id);
                    if (orig_check != NULL &&
                        (orig_check->hash_type == HASHTYPE_VAR ||
                         orig_check->hash_type == HASHTYPE_ARRAY))
                        continue;
                }
                /* Don't rename if a user-declared local variable with the
                 * replacement name would shadow the function return slot. */
                HashNode_t *local_check = NULL;
                if (symtab != NULL && symtab->current_scope != NULL &&
                    symtab->current_scope->table != NULL)
                {
                    local_check = FindIdentInTable(symtab->current_scope->table, replacement);
                    if (local_check != NULL &&
                        local_check->hash_type == HASHTYPE_FUNCTION_RETURN)
                        local_check = NULL;
                }
                if (local_check != NULL)
                    continue;
                char *dup = strdup(replacement);
                if (dup == NULL)
                    return 1;
                free(arg_expr->expr_data.id);
                arg_expr->expr_data.id = dup;
            }
        }
    }

    /* If this is a method call placeholder with a type identifier receiver,
     * resolve it to the class method immediately to avoid type-helper detours. */
    if (stmt->stmt_data.procedure_call_data.is_method_call_placeholder && args_given != NULL)
    {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
            first_arg->expr_data.id != NULL && with_context_count > 0)
        {
            struct Expression *with_expr = NULL;
            int with_status = semcheck_with_try_resolve(first_arg->expr_data.id,
                symtab, &with_expr, stmt->line_num);
            if (with_status == 0 && with_expr != NULL)
            {
                char *field_id = first_arg->expr_data.id;
                memset(&first_arg->expr_data, 0, sizeof(first_arg->expr_data));
                first_arg->type = EXPR_RECORD_ACCESS;
                first_arg->expr_data.record_access_data.record_expr = with_expr;
                first_arg->expr_data.record_access_data.field_id = field_id;
                first_arg->expr_data.record_access_data.field_offset = 0;
                first_arg->record_type = NULL;
                first_arg->array_element_record_type = NULL;
                first_arg->array_element_type = UNKNOWN_TYPE;
                first_arg->array_element_type_id = NULL;
                first_arg->pointer_subtype = UNKNOWN_TYPE;
                semcheck_expr_set_resolved_type(first_arg, UNKNOWN_TYPE);
            }
            else if (with_expr != NULL)
            {
                destroy_expr(with_expr);
            }
        }
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            /* Keep unit qualifiers out of the type-receiver fast path.
             * Calls like System.Error(...) should be handled by the unit-qualified
             * rewrite below, not rewritten as TypeName__MethodName. */
            if (semcheck_is_unit_name(first_arg->expr_data.id))
                goto skip_type_receiver_rewrite;

            HashNode_t *type_node = NULL;
            int type_found = (FindSymbol(&type_node, symtab, first_arg->expr_data.id) != 0 &&
                type_node != NULL && type_node->hash_type == HASHTYPE_TYPE);

            /* Handle specialized generic type receiver: specialize T<A>.Method(...)
             * The parser produces receiver="T$A" which may not be in the symbol
             * table directly, but the base type "T" is.  Fall back to the base name. */
            if (!type_found)
            {
                const char *dollar = strchr(first_arg->expr_data.id, '$');
                if (dollar != NULL && dollar > first_arg->expr_data.id)
                {
                    char *gen_base = strndup(first_arg->expr_data.id, (size_t)(dollar - first_arg->expr_data.id));
                    if (gen_base != NULL)
                    {
                        type_node = NULL;
                        if (FindSymbol(&type_node, symtab, gen_base) != 0 &&
                            type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                        {
                            type_found = 1;
                        }
                        free(gen_base);
                    }
                }
            }

            if (type_found)
            {
                struct RecordType *record_info = semcheck_stmt_get_record_type_from_node(type_node);
                if (record_info != NULL && record_info->type_id != NULL &&
                    stmt->stmt_data.procedure_call_data.placeholder_method_name != NULL)
                {
                    const char *method_name = stmt->stmt_data.procedure_call_data.placeholder_method_name;

                    /* Check if method_name is actually a field of procedure type on the class.
                     * E.g., tmodule.finish_module(hp) where finish_module is a class var
                     * of procedural type, not a method. Since the receiver is a type name,
                     * only class vars (or static fields) are valid here. */
                    {
                        int is_classvar_proc = 0;
                        struct RecordType *walk_rec = record_info;
                        while (walk_rec != NULL && !is_classvar_proc)
                        {
                            for (ListNode_t *f = walk_rec->fields; f != NULL; f = f->next)
                            {
                                if (f->type != LIST_RECORD_FIELD || f->cur == NULL)
                                    continue;
                                struct RecordField *rf = (struct RecordField *)f->cur;
                                if (rf->name == NULL)
                                    continue;
                                if (strcasecmp(rf->name, method_name) != 0)
                                    continue;
                                /* Found field — check if it has procedure type */
                                if (rf->proc_type != NULL)
                                {
                                    is_classvar_proc = 1;
                                    break;
                                }
                                if (rf->type_id != NULL)
                                {
                                    HashNode_t *ft_node = NULL;
                                    if (FindSymbol(&ft_node, symtab, rf->type_id) != 0 &&
                                        ft_node != NULL && ft_node->type != NULL &&
                                        ft_node->type->kind == TYPE_KIND_PROCEDURE)
                                    {
                                        is_classvar_proc = 1;
                                        break;
                                    }
                                }
                            }
                            /* Walk parent class hierarchy */
                            const char *parent = walk_rec->parent_class_name;
                            walk_rec = (parent != NULL) ? semcheck_lookup_record_type(symtab, parent) : NULL;
                        }
                        if (is_classvar_proc)
                        {
                            /* Convert to a procedural variable call through the class var.
                             * Build TypeName.field_name as a class-field access expression. */
                            struct Expression *type_expr = mk_varid(stmt->line_num,
                                strdup(first_arg->expr_data.id));
                            struct Expression *field_access = mk_recordaccess(stmt->line_num,
                                type_expr, strdup(method_name));

                            /* Remove the type receiver from the argument list */
                            ListNode_t *remaining_args = args_given->next;
                            destroy_expr(first_arg);
                            args_given->cur = NULL;
                            free(args_given);
                            stmt->stmt_data.procedure_call_data.expr_args = remaining_args;

                            stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                            stmt->stmt_data.procedure_call_data.procedural_var_expr = field_access;
                            stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                            stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
                            stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;

                            int field_tag = UNKNOWN_TYPE;
                            return_val += semcheck_stmt_expr_tag(&field_tag, symtab, field_access,
                                max_scope_lev, NO_MUTATE);

                            return return_val;
                        }
                    }

                    int is_static_method = from_cparser_is_method_static(record_info->type_id,
                        method_name);
                    int is_nonstatic_class_method =
                        (!is_static_method &&
                         from_cparser_is_method_class_method(record_info->type_id,
                             method_name));
                    size_t class_len = strlen(record_info->type_id);
                    size_t method_len = strlen(method_name);
                    char *new_proc_id = (char *)malloc(class_len + 2 + method_len + 1);
                    if (new_proc_id != NULL)
                    {
                        sprintf(new_proc_id, "%s__%s", record_info->type_id, method_name);
                        free(proc_id);
                        proc_id = new_proc_id;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                        if (is_nonstatic_class_method)
                        {
                            stmt->stmt_data.procedure_call_data.is_class_method_call = 1;
                            if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                            {
                                stmt->stmt_data.procedure_call_data.self_class_name =
                                    strdup(record_info->type_id);
                            }
                        }
                        else
                        {
                            ListNode_t *remaining_args = args_given->next;
                            destroy_expr(first_arg);
                            args_given->cur = NULL;
                            free(args_given);
                            stmt->stmt_data.procedure_call_data.expr_args = remaining_args;
                            args_given = remaining_args;
                            static_arg_already_removed = 1;
                        }
                    }
                }
            }
        }
    }
skip_type_receiver_rewrite:

    /* FPC Bootstrap Feature: Handle unit-qualified procedure calls.
     * When the parser sees Unit.Procedure(args), it creates a procedure call with id "__Procedure"
     * and passes Unit as the first argument (as if it were a method call).
     * We need to detect this pattern and transform it back to a direct procedure call.
     *
     * Pattern: proc_id starts with "__", first arg is a VAR_ID that names a known unit
     * (preferred) or is unresolved in the symbol table (fallback heuristic), and the procedure
     * name (without "__" prefix) exists in symbol table.
     */
    if (stmt->stmt_data.procedure_call_data.is_method_call_placeholder && args_given != NULL)
    {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            char *potential_unit_name = first_arg->expr_data.id;
            HashNode_t *unit_check = NULL;
            int is_unit_qualifier = semcheck_is_unit_name(potential_unit_name);

            /* Local variables/parameters shadow unit names in Pascal.
             * If the identifier resolves as a variable, don't treat it
             * as a unit qualifier (e.g., 'node' parameter vs 'node' unit). */
            if (is_unit_qualifier)
            {
                HashNode_t *var_check = NULL;
                if (FindSymbol(&var_check, symtab, potential_unit_name) != 0 &&
                    var_check != NULL &&
                    (var_check->hash_type == HASHTYPE_VAR ||
                     var_check->hash_type == HASHTYPE_CONST))
                {
                    is_unit_qualifier = 0;
                }
            }

            /* Prefer explicit unit-name recognition; keep unresolved-name fallback for
             * parser shapes where unit qualifiers are not injected into symbol tables. */
            if (!is_unit_qualifier &&
                FindSymbol(&unit_check, symtab, potential_unit_name) == 0)
            {
                int looks_like_self_member = 0;
                HashNode_t *self_node = NULL;
                if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
                {
                    struct RecordType *self_record = semcheck_stmt_get_record_type_from_node(self_node);
                    if (self_record != NULL &&
                        semcheck_find_class_field_including_hidden(symtab, self_record,
                            potential_unit_name, NULL) != NULL)
                    {
                        looks_like_self_member = 1;
                    }
                    if (!looks_like_self_member &&
                        semcheck_find_class_property(symtab, self_record,
                            potential_unit_name, NULL) != NULL)
                    {
                        looks_like_self_member = 1;
                    }
                    if (!looks_like_self_member &&
                        semcheck_find_class_method(symtab, self_record,
                            potential_unit_name, NULL) != NULL)
                    {
                        looks_like_self_member = 1;
                    }
                }

                /* Before treating as a unit qualifier, check if the identifier
                 * resolves via active WITH contexts.  This prevents
                 * 'with s do Data.Reset' from being misidentified as a
                 * unit-qualified call when Data is a WITH-scoped property. */
                if (!looks_like_self_member && with_context_count > 0)
                {
                    struct Expression *with_check = NULL;
                    int ws = semcheck_with_try_resolve(potential_unit_name,
                        symtab, &with_check, stmt->line_num);
                    if (ws == 0 && with_check != NULL)
                    {
                        looks_like_self_member = 1; /* not a unit; it's a WITH field */
                        destroy_expr(with_check);
                    }
                }
                if (!looks_like_self_member)
                    is_unit_qualifier = 1;
            }

            if (is_unit_qualifier)
            {
                /* Unit-qualified call; resolve using the structured method name. */
                char *real_proc_name = NULL;
                char *unit_qualifier_copy = strdup(potential_unit_name);
                if (stmt->stmt_data.procedure_call_data.placeholder_method_name != NULL)
                {
                    real_proc_name =
                        strdup(stmt->stmt_data.procedure_call_data.placeholder_method_name);
                }
                else if (proc_id != NULL &&
                    proc_id[0] == '_' &&
                    proc_id[1] == '_' &&
                    proc_id[2] != '\0')
                {
                    real_proc_name = strdup(proc_id + 2);
                }
                if (real_proc_name == NULL)
                {
                    free(unit_qualifier_copy);
                    /* strdup failed - skip transformation, will report error later */
                }
                else
                {
                    if (pascal_identifier_equals(potential_unit_name, "System") &&
                        pascal_identifier_equals(real_proc_name, "Error"))
                    {
                        ListNode_t *remaining_args = args_given->next;
                        destroy_expr(first_arg);
                        args_given->cur = NULL;
                        free(args_given);

                        stmt->stmt_data.procedure_call_data.expr_args = remaining_args;
                        free(proc_id);
                        proc_id = strdup("Halt");
                        if (proc_id == NULL)
                        {
                            free(real_proc_name);
                            return 1;
                        }
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                        if (stmt->stmt_data.procedure_call_data.call_qualifier != NULL)
                        {
                            free(stmt->stmt_data.procedure_call_data.call_qualifier);
                            stmt->stmt_data.procedure_call_data.call_qualifier = NULL;
                        }
                        stmt->stmt_data.procedure_call_data.call_qualifier = unit_qualifier_copy;
                        unit_qualifier_copy = NULL;
                        if (stmt->stmt_data.procedure_call_data.placeholder_method_name != NULL)
                        {
                            free(stmt->stmt_data.procedure_call_data.placeholder_method_name);
                            stmt->stmt_data.procedure_call_data.placeholder_method_name = NULL;
                        }
                        args_given = remaining_args;
                        was_unit_qualified = 1;
                        free(real_proc_name);
                    }
                    else
                    {
                    int force_strip_system_qualifier =
                        pascal_identifier_equals(potential_unit_name, "System");
                    ListNode_t *proc_candidates = FindAllIdents(symtab, real_proc_name);

                    if (proc_candidates != NULL || force_strip_system_qualifier)
                    {
                        /* Found the procedure by name. Transform the call:
                         * 1. Remove the first argument (the unit qualifier)
                         * 2. Change proc_id to the real procedure name (without "__")
                         *
                         * System-qualified builtins are not always present as ordinary
                         * symbol-table procedures, but semcheck still needs the
                         * placeholder receiver stripped so later builtin resolution can
                         * match names like Error, Halt, and Exit.
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
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                        if (stmt->stmt_data.procedure_call_data.call_qualifier != NULL)
                        {
                            free(stmt->stmt_data.procedure_call_data.call_qualifier);
                            stmt->stmt_data.procedure_call_data.call_qualifier = NULL;
                        }
                        stmt->stmt_data.procedure_call_data.call_qualifier = unit_qualifier_copy;
                        unit_qualifier_copy = NULL;
                        if (stmt->stmt_data.procedure_call_data.placeholder_method_name != NULL)
                        {
                            free(stmt->stmt_data.procedure_call_data.placeholder_method_name);
                            stmt->stmt_data.procedure_call_data.placeholder_method_name = NULL;
                        }
                        args_given = remaining_args;
                        was_unit_qualified = 1;

                        if (proc_candidates != NULL)
                            DestroyList(proc_candidates);

                        /* Continue with normal procedure call handling using the transformed call */
                    }
                    else
                    {
                        /* Procedure not found - allow System.Exit without a symbol table entry. */
                        if (pascal_identifier_equals(real_proc_name, "Exit"))
                        {
                            ListNode_t *remaining_args = args_given->next;

                            destroy_expr(first_arg);
                            args_given->cur = NULL;
                            free(args_given);

                            stmt->stmt_data.procedure_call_data.expr_args = remaining_args;

                            free(proc_id);
                            proc_id = real_proc_name;
                            stmt->stmt_data.procedure_call_data.id = proc_id;
                            stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                            if (stmt->stmt_data.procedure_call_data.call_qualifier != NULL)
                            {
                                free(stmt->stmt_data.procedure_call_data.call_qualifier);
                                stmt->stmt_data.procedure_call_data.call_qualifier = NULL;
                            }
                            stmt->stmt_data.procedure_call_data.call_qualifier = unit_qualifier_copy;
                            unit_qualifier_copy = NULL;
                            args_given = remaining_args;
                            was_unit_qualified = 1;
                        }
                        else
                        {
                            /* Procedure not found - free real_proc_name and fall through to report error */
                            free(real_proc_name);
                        }
                    }
                    free(unit_qualifier_copy);
                    }
                }
            }
        }
    }

    /* Treat System.Exit (or unqualified Exit) as a built-in procedure call.
     * This avoids resolving Exit against class methods like TMonitor.Exit. */
    if (proc_id != NULL &&
        pascal_identifier_equals(proc_id, "Exit") &&
        !stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
    {
        int arg_count = 0;
        for (ListNode_t *scan = args_given; scan != NULL; scan = scan->next)
            ++arg_count;

        if (arg_count > 1)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Exit() expects at most one argument.\n\n",
                stmt->line_num);
            return 1;
        }

        struct Expression *exit_expr = NULL;
        if (arg_count == 1)
        {
            int expr_type = UNKNOWN_TYPE;
            exit_expr = (struct Expression *)args_given->cur;
            if (exit_expr != NULL)
                return_val += semcheck_stmt_expr_tag(&expr_type, symtab, exit_expr, max_scope_lev, 0);
        }

        /* Transform the procedure call into an Exit statement for codegen. */
        stmt->type = STMT_EXIT;
        stmt->stmt_data.exit_data.return_expr = exit_expr;
        stmt->stmt_data.procedure_call_data.expr_args = NULL;
        if (stmt->stmt_data.procedure_call_data.id != NULL)
        {
            free(stmt->stmt_data.procedure_call_data.id);
            stmt->stmt_data.procedure_call_data.id = NULL;
        }
        if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
        {
            free(stmt->stmt_data.procedure_call_data.mangled_id);
            stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        }
        while (args_given != NULL)
        {
            ListNode_t *next = args_given->next;
            args_given->cur = NULL;
            free(args_given);
            args_given = next;
        }

        return return_val;
    }

    /* INTERNPROC: Transform TypedFile Rewrite/Reset calls by injecting element size.
     * Must happen before overload resolution so the 2-arg variant is selected. */
    if (semcheck_internproc_typedfile_rewrite_reset(symtab, stmt))
        args_given = stmt->stmt_data.procedure_call_data.expr_args;

    /* If no explicit receiver was provided, but Self is in scope and defines this method,
     * prepend Self so unqualified method calls resolve correctly. */
    if (!was_unit_qualified && proc_id != NULL &&
        !stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
    {
        HashNode_t *self_node = NULL;
        struct RecordType *self_record = NULL;
        if (kgpc_getenv("KGPC_DEBUG_ASSIGN") != NULL &&
            pascal_identifier_equals(proc_id, "Assign"))
        {
            HashNode_t *dbg_self = NULL;
            int dbg_found = FindSymbol(&dbg_self, symtab, "Self");
            HashNode_t *dbg_proc = NULL;
            FindSymbol(&dbg_proc, symtab, "Assign");
            const char *dbg_owner = semcheck_get_current_method_owner();
            fprintf(stderr, "[ASSIGN-TRACE] Self_found=%d owner=%s scope_kind=%d proc_owner=%s proc_hash=%d\n",
                dbg_found != 0, dbg_owner ? dbg_owner : "<null>",
                symtab->current_scope ? symtab->current_scope->num_deps : -1,
                (dbg_proc && dbg_proc->owner_class) ? dbg_proc->owner_class : "<null>",
                dbg_proc ? dbg_proc->hash_type : -1);
        }
        if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
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
            const char *cur_owner = semcheck_get_current_method_owner();
            struct RecordType *owner_record = NULL;
            if (cur_owner != NULL)
                owner_record = semcheck_lookup_record_type(symtab, cur_owner);

            struct RecordType *lookup_record = (owner_record != NULL) ? owner_record : self_record;
            HashNode_t *method_node = semcheck_find_class_method(symtab, lookup_record, proc_id, NULL);
            /* If not found and self_record->type_id differs from the current
             * method owner (e.g. record has "timezone" but owner is "TTimeZone"),
             * retry with the owner's record. */
            if (method_node == NULL && self_record->type_id != NULL)
            {
                if (cur_owner != NULL && !pascal_identifier_equals(cur_owner, self_record->type_id))
                {
                    if (owner_record != NULL)
                    {
                        method_node = semcheck_find_class_method(symtab, owner_record, proc_id, NULL);
                        if (method_node != NULL)
                            self_record = owner_record;
                    }
                }
            }
            else if (method_node != NULL && owner_record != NULL)
            {
                self_record = owner_record;
            }
            if (method_node != NULL &&
                (method_node->hash_type == HASHTYPE_PROCEDURE ||
                 method_node->hash_type == HASHTYPE_FUNCTION))
            {
                /* Save bare method name before rewrite for virtual dispatch check */
                char *bare_method_name = strdup(proc_id);

                /* Prepend Self to arguments only for non-static methods.
                 * Static class methods have no Self parameter. */
                const char *receiver_class_name =
                    (method_node->owner_class != NULL) ? method_node->owner_class :
                    self_record->type_id;
                int method_is_static = (receiver_class_name != NULL && bare_method_name != NULL) ?
                    from_cparser_is_method_static(receiver_class_name, bare_method_name) : 0;
                int method_is_class =
                    (receiver_class_name != NULL && bare_method_name != NULL && !method_is_static) ?
                    from_cparser_is_method_class_method(receiver_class_name, bare_method_name) : 0;
                if (!method_is_static)
                {
                    struct Expression *receiver_expr = mk_varid(stmt->line_num, strdup("Self"));
                    ListNode_t *receiver_arg = (receiver_expr != NULL) ?
                        CreateListNode(receiver_expr, LIST_EXPR) : NULL;
                    if (receiver_arg != NULL)
                    {
                        receiver_arg->next = args_given;
                        stmt->stmt_data.procedure_call_data.expr_args = receiver_arg;
                        args_given = receiver_arg;
                    }
                    if (method_is_class)
                    {
                        stmt->stmt_data.procedure_call_data.is_class_method_call = 1;
                        if (stmt->stmt_data.procedure_call_data.self_class_name == NULL &&
                            receiver_class_name != NULL)
                        {
                            stmt->stmt_data.procedure_call_data.self_class_name =
                                strdup(receiver_class_name);
                        }
                    }
                }
                else
                {
                    /* Mark that static method Self handling is already done, so the
                     * downstream placeholder-removal code doesn't strip an explicit
                     * Self argument that was part of the original call site. */
                    static_arg_already_removed = 1;
                }

                /* Update proc_id to the resolved method's id (e.g. TBase__Bump, not TDerived__Bump
                 * when the method is inherited from a parent class). */
                if (method_node->id != NULL)
                {
                    char *new_proc_id = strdup(method_node->id);
                    if (new_proc_id != NULL)
                    {
                        free(proc_id);
                        proc_id = new_proc_id;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                    }
                }

                /* Check if this is a virtual/abstract method call that needs VMT dispatch.
                 * Only for instance methods (not class/static methods), since class methods
                 * use a different VMT dispatch convention (single indirection). */
                /* Use the actual call argument count (excluding Self) for VMT overload
                 * matching instead of method_node's parameter count, because
                 * semcheck_find_class_method may return the wrong overload. */
                int method_param_count = -1;
                {
                    int actual_arg_count = ListLength(args_given);
                    if (!method_is_static && actual_arg_count > 0)
                        actual_arg_count -= 1; /* subtract Self */
                    method_param_count = actual_arg_count;
                }
                if (self_record->type_id != NULL && bare_method_name != NULL &&
                    from_cparser_is_method_virtual_with_signature(
                        self_record->type_id,
                        bare_method_name,
                        method_param_count,
                        NULL) &&
                    !from_cparser_is_method_static(self_record->type_id, bare_method_name))
                {
                    stmt->stmt_data.procedure_call_data.is_virtual_call = 1;
                    int vmt_index = -1;
                    if (self_record->methods != NULL)
                    {
                        ListNode_t *method_entry = self_record->methods;
                        while (method_entry != NULL)
                        {
                            struct MethodInfo *info = (struct MethodInfo *)method_entry->cur;
                            if (info != NULL && info->name != NULL &&
                                (info->is_virtual || info->is_override) &&
                                strcasecmp(info->name, bare_method_name) == 0)
                            {
                                if (method_param_count >= 0 && info->param_count >= 0 &&
                                    method_param_count != info->param_count)
                                {
                                    method_entry = method_entry->next;
                                    continue;
                                }
                                vmt_index = info->vmt_index;
                                break;
                            }
                            method_entry = method_entry->next;
                        }
                    }
                    stmt->stmt_data.procedure_call_data.vmt_index = vmt_index;
                    stmt->stmt_data.procedure_call_data.self_class_name =
                        strdup(self_record->type_id);
                }
                /* Interface method call check */
                if (self_record != NULL && self_record->is_interface &&
                    self_record->type_id != NULL && bare_method_name != NULL &&
                    !stmt->stmt_data.procedure_call_data.is_interface_call &&
                    self_record->method_templates != NULL)
                {
                    int idx = 0;
                    for (ListNode_t *mt = self_record->method_templates; mt != NULL; mt = mt->next, idx++)
                    {
                        struct MethodTemplate *tmpl = (struct MethodTemplate *)mt->cur;
                        if (tmpl != NULL && tmpl->name != NULL &&
                            strcasecmp(tmpl->name, bare_method_name) == 0)
                        {
                            stmt->stmt_data.procedure_call_data.is_interface_call = 1;
                            stmt->stmt_data.procedure_call_data.vmt_index = idx;
                            if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                                stmt->stmt_data.procedure_call_data.self_class_name =
                                    strdup(self_record->type_id);
                            break;
                        }
                    }
                }
                /* Mark class method calls so codegen passes VMT as Self.
                 * Walk the parent class chain since the method may be inherited. */
                if (self_record->type_id != NULL && bare_method_name != NULL)
                {
                    const char *check_class = self_record->type_id;
                    struct RecordType *check_record = self_record;
                    while (check_class != NULL)
                    {
                        if (from_cparser_is_method_nonstatic_class_method(check_class, bare_method_name))
                        {
                            stmt->stmt_data.procedure_call_data.is_class_method_call = 1;
                            break;
                        }
                        const char *parent = (check_record != NULL) ? check_record->parent_class_name : NULL;
                        if (parent == NULL) break;
                        check_record = semcheck_lookup_record_type(symtab, parent);
                        check_class = parent;
                    }
                }
                free(bare_method_name);
            }
            else if (self_record != NULL && self_record->type_id != NULL)
            {
                /* Check if proc_id is already a resolved method call (has owner_class in symbol table) */
                HashNode_t *proc_check_node = NULL;
                int is_already_method = 0;
                if (FindSymbol(&proc_check_node, symtab, proc_id) != 0 && proc_check_node != NULL &&
                    proc_check_node->owner_class != NULL)
                    is_already_method = 1;
                if (!is_already_method)
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
                                /* proc_type is resolved during semcheck_qualify_nested_types_for_record
                                 * so this path handles non-nested procedural types only. */
                                HashNode_t *type_node = NULL;
                                if (FindSymbol(&type_node, symtab, rf->type_id) != 0 &&
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
                                        if (FindSymbol(&type_node, symtab, rf2->type_id) != 0 &&
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
                    stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;

                    /* Check the expression for type resolution */
                    int field_tag = UNKNOWN_TYPE;
                    return_val += semcheck_stmt_expr_tag(&field_tag, symtab, field_access, max_scope_lev, NO_MUTATE);

                    return return_val;
                }
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
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Error",
        semcheck_builtin_error, max_scope_lev, &handled_builtin);
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
                if (FindSymbol(&recv_node, symtab, receiver_expr->expr_data.id) != 0 &&
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
                        if (FindSymbol(&type_node, symtab, field_desc->type_id) != 0 &&
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
                            if (FindSymbol(&type_node, symtab, qualified) != 0 &&
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
                            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d: failed to allocate procedural field expression.\n",
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
                                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, call to procedural field %s: expected %d arguments, got %d\n",
                                    stmt->line_num, proc_id, ListLength(formal_params), ListLength(remaining_args));
                                destroy_expr(proc_expr);
                                /* proc_type already released by destroy_expr via resolved_kgpc_type */
                                return ++return_val;
                            }

                            ListNode_t *formal = formal_params;
                            ListNode_t *actual = remaining_args;
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
                                          (is_real_family_type(formal_type) && is_integer_type(actual_type)) ||
                                          (is_integer_type(formal_type) && is_real_family_type(actual_type)) ||
                                          (is_real_family_type(formal_type) && is_real_family_type(actual_type)) ||
                                          (formal_type == VARIANT_TYPE) ||
                                          (actual_type == VARIANT_TYPE) ||
                                          (formal_type == BUILTIN_ANY_TYPE) ||
                                          (actual_type == BUILTIN_ANY_TYPE) ||
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

    
    /* When call_qualifier is already set to a known unit name (e.g. System.Seek
     * inside a method body), the parser may have set is_method_call_placeholder
     * with Self as the first arg.  Skip method resolution entirely and handle
     * as a unit-qualified free procedure call.  We cannot modify the AST here
     * because the same nodes are shared across unit copies. */
    if (stmt->stmt_data.procedure_call_data.is_method_call_placeholder &&
        stmt->stmt_data.procedure_call_data.call_qualifier != NULL &&
        semcheck_is_unit_name(stmt->stmt_data.procedure_call_data.call_qualifier) &&
        args_given != NULL)
    {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
            first_arg->expr_data.id != NULL &&
            pascal_identifier_equals(first_arg->expr_data.id, "Self"))
        {
            /* Type-check Self (first arg) so it doesn't leave unresolved types,
             * then skip it for the actual call. */
            int self_type_tag = UNKNOWN_TYPE;
            semcheck_stmt_expr_tag(&self_type_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);
            /* Advance past Self for overload resolution */
            args_given = args_given->next;
            was_unit_qualified = 1;
            goto skip_method_placeholder_resolution;
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
            } else if (owner_type->kind == TYPE_KIND_POINTER) {
                /* Try lazy resolution of unresolved pointer pointees */
                KgpcType *pointee = kgpc_type_resolve_pointer_pointee(owner_type, symtab);
                if (pointee != NULL && pointee->kind == TYPE_KIND_RECORD) {
                    record_info = pointee->info.record_info;
                } else if (pointee != NULL && pointee->kind == TYPE_KIND_POINTER) {
                    KgpcType *pointee2 = kgpc_type_resolve_pointer_pointee(pointee, symtab);
                    if (pointee2 != NULL && pointee2->kind == TYPE_KIND_RECORD)
                        record_info = pointee2->info.record_info;
                }
            }
        }

        /* Do not rely on legacy record_type metadata; prefer resolved KgpcType only. */

        if (first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, symtab, first_arg->expr_data.id) != 0 &&
                type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
            {
                static_method_receiver = 1;
                if (record_info == NULL)
                    record_info = semcheck_stmt_get_record_type_from_node(type_node);
            }
        }

        if (record_info != NULL && record_info->type_id != NULL) {
            const char *method_name = (stmt->stmt_data.procedure_call_data.placeholder_method_name != NULL)
                ? stmt->stmt_data.procedure_call_data.placeholder_method_name : proc_id;

            struct RecordType *actual_method_owner = NULL;
            HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, method_name, &actual_method_owner);
            int is_static = from_cparser_is_method_static(record_info->type_id, method_name);
            /* Check the actual method owner for inherited static methods */
            if (!is_static && actual_method_owner != NULL &&
                actual_method_owner->type_id != NULL && method_name != NULL) {
                is_static = from_cparser_is_method_static(actual_method_owner->type_id, method_name);
            }
            int is_nonstatic_class_method =
                (!is_static &&
                 from_cparser_is_method_class_method(record_info->type_id, method_name));
            if (!is_nonstatic_class_method && !is_static && actual_method_owner != NULL &&
                actual_method_owner->type_id != NULL && method_name != NULL) {
                is_nonstatic_class_method = from_cparser_is_method_class_method(actual_method_owner->type_id, method_name);
            }

            if (method_node != NULL) {
                /* Keep class-prefixed id for static/class calls (e.g. ClassName.Create),
                 * but canonicalize instance calls to the resolved owner id.
                 * Use the actual method owner's class name so inherited methods
                 * resolve to the defining class (e.g. TObject__Free, not TChild__Free). */
                if (static_method_receiver || is_nonstatic_class_method)
                {
                    const char *owner_id = (actual_method_owner != NULL && actual_method_owner->type_id != NULL)
                        ? actual_method_owner->type_id : record_info->type_id;
                    size_t class_len = strlen(owner_id);
                    size_t method_len = strlen(method_name);
                    char *new_proc_id = (char *)malloc(class_len + 2 + method_len + 1);
                    if (new_proc_id != NULL) {
                        sprintf(new_proc_id, "%s__%s", owner_id, method_name);
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

                semcheck_stmt_try_set_method_mangled_id(symtab, stmt, proc_id,
                    method_node->mangled_id);

                int receiver_is_type_ident = 0;
                if (args_given != NULL && args_given->cur != NULL)
                {
                    struct Expression *receiver_expr = (struct Expression *)args_given->cur;
                    if (receiver_expr != NULL && receiver_expr->type == EXPR_VAR_ID &&
                        receiver_expr->expr_data.id != NULL)
                    {
                        HashNode_t *receiver_node = NULL;
                        if (FindSymbol(&receiver_node, symtab, receiver_expr->expr_data.id) != 0 &&
                            receiver_node != NULL && receiver_node->hash_type == HASHTYPE_TYPE)
                            receiver_is_type_ident = 1;
                    }
                }

                if (is_nonstatic_class_method && receiver_is_type_ident)
                {
                    stmt->stmt_data.procedure_call_data.is_class_method_call = 1;
                    if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                    {
                        stmt->stmt_data.procedure_call_data.self_class_name =
                            strdup(record_info->type_id);
                    }
                }
                else if (is_static && receiver_is_type_ident) {
                    /* For static methods, remove the first argument (the instance/type identifier) */
                    ListNode_t *old_head = args_given;
                    stmt->stmt_data.procedure_call_data.expr_args = old_head->next;
                    old_head->next = NULL;
                    args_given = stmt->stmt_data.procedure_call_data.expr_args;
                    static_arg_already_removed = 1;
                }
                else if (is_static && !receiver_is_type_ident && args_given != NULL)
                {
                    /* Static method called via instance variable or implicit Self.
                     * Static methods have no Self parameter, so strip the receiver. */
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
                        if (FindSymbol(&type_node, symtab, proc_field->type_id) != 0 &&
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
    /* Check if proc_id represents a method call.
     * Use symbol table lookup for structured identity instead of parsing "__". */
    HashNode_t *proc_method_node = NULL;
    int proc_is_method_placeholder = stmt->stmt_data.procedure_call_data.is_method_call_placeholder;
    int proc_is_method = proc_is_method_placeholder;
    const char *proc_method_name_resolved = NULL;
    const char *proc_owner_class_resolved = NULL;
    if (!proc_is_method && proc_id != NULL &&
        FindSymbol(&proc_method_node, symtab, proc_id) != 0 && proc_method_node != NULL &&
        proc_method_node->owner_class != NULL)
    {
        proc_is_method = 1;
        proc_method_name_resolved = proc_method_node->method_name;
        proc_owner_class_resolved = proc_method_node->owner_class;
    }
    if (proc_is_method && args_given != NULL && !static_arg_already_removed) {
        const char *method_name = proc_is_method_placeholder
            ? stmt->stmt_data.procedure_call_data.placeholder_method_name
            : proc_method_name_resolved;
        const char *class_name = proc_owner_class_resolved;
        int need_free_class_name = 0;

        if (proc_is_method_placeholder) {
            /* Case 1: __MethodName - need to get class from first argument */
            if (args_given != NULL && args_given->cur != NULL) {
                struct Expression *first_arg = (struct Expression *)args_given->cur;
                
                /* Try to get the record type of the first argument */
                struct RecordType *record_type = NULL;
                if (first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindSymbol(&type_node, symtab, first_arg->expr_data.id) != 0 &&
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
                    if (FindSymbol(&var_node, symtab, first_arg->expr_data.id) != 0 && var_node != NULL &&
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
                    if (FindSymbol(&subtype_node, symtab, first_arg->pointer_subtype_id) != 0 &&
                        subtype_node != NULL)
                    {
                        record_type = semcheck_stmt_get_record_type_from_node(subtype_node);
                    }
                }

                if (record_type == NULL || record_type->type_id == NULL)
                {
                    int helper_tag = UNKNOWN_TYPE;
                    semcheck_stmt_expr_tag(&helper_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);
                    /* Resolve arg_type AFTER semcheck_stmt_expr_tag — the latter may
                       free and replace types on the expression, invalidating earlier pointers. */
                    int arg_type_owned = 0;
                    KgpcType *arg_type = semcheck_resolve_expression_kgpc_type(symtab,
                        first_arg, max_scope_lev, NO_MUTATE, &arg_type_owned);
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
                        if (FindSymbol(&var_node, symtab, first_arg->expr_data.id) != 0 &&
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
                    if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL) {
                        fprintf(stderr, "[SemCheck] method placeholder: resolved helper record %s for %s\n",
                            record_type->type_id, method_name != NULL ? method_name : "<null>");
                    }
                    class_name = record_type->type_id;
                }
            }
        } else {
            /* Case 2: ClassName__MethodName - class_name already set from symbol table lookup */
            /* class_name = proc_owner_class_resolved (set above) */
        }
        
        if (class_name != NULL && method_name != NULL) {
            int is_static = from_cparser_is_method_static(class_name, method_name);
            int is_nonstatic_class_method =
                (!is_static &&
                 from_cparser_is_method_class_method(class_name, method_name));
            HashNode_t *resolved_method = NULL;
            HashNode_t *class_node = NULL;
            if (FindSymbol(&class_node, symtab, class_name) != 0 && class_node != NULL)
            {
                struct RecordType *class_record = semcheck_stmt_get_record_type_from_node(class_node);
                if (class_record != NULL)
                    resolved_method = semcheck_find_class_method(symtab, class_record, method_name, NULL);
            }
            
            /* If proc_id started with __, update it to include the class name */
            if (proc_is_method_placeholder) {
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

            {
                const char *overload_check_id =
                    (resolved_method != NULL && resolved_method->id != NULL)
                    ? resolved_method->id : proc_id;
                semcheck_stmt_try_set_method_mangled_id(symtab, stmt, overload_check_id,
                    resolved_method != NULL ? resolved_method->mangled_id : NULL);
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
            int receiver_is_self = 0;
            if (args_given != NULL && args_given->cur != NULL)
            {
                struct Expression *receiver_expr = (struct Expression *)args_given->cur;
                if (receiver_expr != NULL && receiver_expr->type == EXPR_VAR_ID &&
                    receiver_expr->expr_data.id != NULL)
                {
                    if (pascal_identifier_equals(receiver_expr->expr_data.id, "Self"))
                        receiver_is_self = 1;
                    HashNode_t *receiver_node = NULL;
                    if (FindSymbol(&receiver_node, symtab, receiver_expr->expr_data.id) != 0 &&
                        receiver_node != NULL && receiver_node->hash_type == HASHTYPE_TYPE)
                        receiver_is_type_ident = 1;
                }
            }

            if (is_nonstatic_class_method && !receiver_is_type_ident && !receiver_is_self)
            {
                stmt->stmt_data.procedure_call_data.is_class_method_call = 1;
                if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                    stmt->stmt_data.procedure_call_data.self_class_name = strdup(class_name);
            }

            if (is_static && receiver_is_type_ident) {
                /* For static methods, remove the first argument (the type identifier) */
                args_given = args_given->next;
                stmt->stmt_data.procedure_call_data.expr_args = args_given;
            }
            else if (is_static && !receiver_is_type_ident && args_given != NULL)
            {
                /* Static method called via instance variable or implicit Self.
                 * Static methods have no Self parameter, so strip the receiver. */
                args_given = args_given->next;
                stmt->stmt_data.procedure_call_data.expr_args = args_given;
            }
        }

        if (need_free_class_name && class_name != NULL) {
            free((void *)class_name);
        }
    }

    
    /* Re-check if proc_id is a method (may have been updated by previous block) */
    HashNode_t *type_res_method_node = NULL;
    const char *type_res_method_name = NULL;
    int proc_is_method_for_type_res = stmt->stmt_data.procedure_call_data.is_method_call_placeholder;
    if (!proc_is_method_for_type_res && proc_id != NULL &&
        FindSymbol(&type_res_method_node, symtab, proc_id) != 0 && type_res_method_node != NULL &&
        type_res_method_node->owner_class != NULL)
    {
        proc_is_method_for_type_res = 1;
        type_res_method_name = type_res_method_node->method_name;
    }
    if (proc_is_method_for_type_res && type_res_method_name == NULL)
        type_res_method_name = stmt->stmt_data.procedure_call_data.placeholder_method_name;
    if (proc_is_method_for_type_res && args_given != NULL &&
        !static_arg_already_removed &&
        stmt->stmt_data.procedure_call_data.mangled_id == NULL) {
        const char *method_name_part = type_res_method_name;
        
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
                        if (FindSymbol(&proc_node, symtab, mangled_name) != 0 && proc_node != NULL) {
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
                            if (FindSymbol(&parent_node, symtab, parent_name) != 0 && 
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

skip_method_placeholder_resolution:

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
        if (FindSymbol(&type_check, symtab, proc_id) && type_check != NULL &&
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

    /* When the call was unit-qualified (e.g. System.Seek), filter candidates to
     * only those belonging to the specified unit.  This prevents a same-named
     * method in the current class from shadowing the intended unit's version.
     * Fall back to unfiltered results if filtering would leave no candidates. */
    if (was_unit_qualified && stmt->stmt_data.procedure_call_data.call_qualifier != NULL &&
        overload_candidates != NULL)
    {
        const char *uq_name = stmt->stmt_data.procedure_call_data.call_qualifier;
        ListNode_t *filtered = NULL;
        ListNode_t *filtered_tail = NULL;
        for (ListNode_t *cn = overload_candidates; cn != NULL; cn = cn->next)
        {
            HashNode_t *hn = (HashNode_t *)cn->cur;
            if (hn == NULL) continue;
            int match = 0;
            if (hn->source_unit_index != 0)
            {
                const char *src_name = unit_registry_get(hn->source_unit_index);
                if (src_name != NULL && pascal_identifier_equals(src_name, uq_name))
                    match = 1;
            }
            if (!match && hn->type != NULL && hn->type->kind == TYPE_KIND_PROCEDURE &&
                hn->type->info.proc_info.definition != NULL)
            {
                int def_unit_idx =
                    hn->type->info.proc_info.definition->tree_data.subprogram_data.source_unit_index;
                if (def_unit_idx != 0)
                {
                    const char *src_name = unit_registry_get(def_unit_idx);
                    if (src_name != NULL && pascal_identifier_equals(src_name, uq_name))
                        match = 1;
                }
            }
            if (match)
            {
                ListNode_t *new_node = CreateListNode(hn, LIST_UNSPECIFIED);
                if (filtered == NULL)
                    filtered = new_node;
                else
                    filtered_tail->next = new_node;
                filtered_tail = new_node;
            }
        }
        if (filtered != NULL)
        {
            DestroyList(overload_candidates);
            overload_candidates = filtered;
        }
    }

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
    HashNode_t *parent_lookup_node = NULL;
    if (resolved_proc == NULL && proc_id != NULL &&
        FindSymbol(&parent_lookup_node, symtab, proc_id) != 0 && parent_lookup_node != NULL &&
        parent_lookup_node->owner_class != NULL) {
        {
            char *class_name = strdup(parent_lookup_node->owner_class);
            char *method_name = strdup(parent_lookup_node->method_name);

            if (class_name != NULL && method_name != NULL) {
                if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL)
                {
                    fprintf(stderr, "[KGPC] Trying to resolve inherited call: class=%s method=%s\n",
                            class_name, method_name);
                }
                /* Look up the class to find its parent */
                HashNode_t *class_node = NULL;
                if (FindSymbol(&class_node, symtab, class_name) != 0 && class_node != NULL) {
                    struct RecordType *record_info = semcheck_stmt_get_record_type_from_node(class_node);
                    if (record_info == NULL)
                        goto proccall_parent_resolve_done;
                    char *parent_class_name = record_info->parent_class_name;
                    
                    if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL)
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
                            HashNode_t *parent_class_node = semcheck_find_preferred_type_node(symtab, parent_class_name);
                            if (parent_class_node != NULL) {
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

    /* If no overloads found and proc_id looks like ClassName__MethodName,
     * try walking the class hierarchy to find the method in a parent class.
     * This handles cases like TElfVersionDef.Create where the constructor
     * is inherited from TFPHashObject but not redeclared in the child. */
    if (resolved_proc == NULL && overload_candidates == NULL && proc_id != NULL)
    {
        const char *dunder = strstr(proc_id, "__");
        if (dunder != NULL && dunder > proc_id)
        {
            char *class_name = strndup(proc_id, (size_t)(dunder - proc_id));
            const char *method_name = dunder + 2;
            if (class_name != NULL && method_name[0] != '\0')
            {
                HashNode_t *class_node = semcheck_find_preferred_type_node(symtab, class_name);
                if (class_node == NULL)
                    FindSymbol(&class_node, symtab, class_name);
                struct RecordType *record_info = (class_node != NULL)
                    ? semcheck_stmt_get_record_type_from_node(class_node) : NULL;
                const char *parent_class_name = (record_info != NULL)
                    ? record_info->parent_class_name : NULL;
                while (parent_class_name != NULL && resolved_proc == NULL)
                {
                    size_t plen = strlen(parent_class_name);
                    size_t mlen = strlen(method_name);
                    char *parent_proc_id = (char *)malloc(plen + 2 + mlen + 1);
                    if (parent_proc_id == NULL)
                        break;
                    sprintf(parent_proc_id, "%s__%s", parent_class_name, method_name);

                    char *parent_mangled = MangleFunctionNameFromCallSite(
                        parent_proc_id, args_given, symtab, INT_MAX);
                    ListNode_t *parent_candidates = FindAllIdents(symtab, parent_proc_id);
                    if (parent_candidates != NULL)
                    {
                        for (ListNode_t *pc = parent_candidates; pc != NULL; pc = pc->next)
                        {
                            HashNode_t *cand = (HashNode_t *)pc->cur;
                            if (cand->mangled_id != NULL && parent_mangled != NULL &&
                                strcmp(cand->mangled_id, parent_mangled) == 0)
                            {
                                resolved_proc = cand;
                                match_count = 1;
                                if (cand->id != NULL)
                                {
                                    free(stmt->stmt_data.procedure_call_data.id);
                                    stmt->stmt_data.procedure_call_data.id = strdup(cand->id);
                                    proc_id = stmt->stmt_data.procedure_call_data.id;
                                }
                                break;
                            }
                        }
                        if (resolved_proc == NULL)
                        {
                            /* No mangled match — try overload resolution on parent candidates */
                            overload_candidates = parent_candidates;
                            parent_candidates = NULL;
                            free(stmt->stmt_data.procedure_call_data.id);
                            stmt->stmt_data.procedure_call_data.id = strdup(parent_proc_id);
                            proc_id = stmt->stmt_data.procedure_call_data.id;
                            free(mangled_name);
                            mangled_name = parent_mangled;
                            parent_mangled = NULL;
                        }
                        DestroyList(parent_candidates);
                    }
                    free(parent_mangled);
                    free(parent_proc_id);
                    if (resolved_proc != NULL || overload_candidates != NULL)
                        break;
                    /* Move to next parent */
                    HashNode_t *pnode = semcheck_find_preferred_type_node(symtab, parent_class_name);
                    struct RecordType *prec = (pnode != NULL)
                        ? semcheck_stmt_get_record_type_from_node(pnode) : NULL;
                    parent_class_name = (prec != NULL) ? prec->parent_class_name : NULL;
                }
            }
            free(class_name);
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
            match_count = 1;
            /* resolved_proc already set to first match — keep it,
             * duplicates from different scopes are functionally equivalent */
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

    /* Explicit type-qualified class method calls can arrive here without the
     * synthetic Self/class receiver in statement context (e.g. TClass.Proc()).
     * Reinsert the class receiver before overload resolution so arity matches
     * the hidden Self parameter used by non-static class methods. */
    if (!static_arg_already_removed && overload_candidates != NULL &&
        stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
    {
        int receiver_is_type_ident = 0;
        int receiver_is_self = 0;
        if (args_given != NULL && args_given->cur != NULL)
        {
            struct Expression *receiver_expr = (struct Expression *)args_given->cur;
            if (receiver_expr != NULL && receiver_expr->type == EXPR_VAR_ID &&
                receiver_expr->expr_data.id != NULL)
            {
                if (pascal_identifier_equals(receiver_expr->expr_data.id, "Self"))
                    receiver_is_self = 1;
                else
                {
                    HashNode_t *receiver_node = NULL;
                    if (FindSymbol(&receiver_node, symtab, receiver_expr->expr_data.id) != 0 &&
                        receiver_node != NULL && receiver_node->hash_type == HASHTYPE_TYPE)
                        receiver_is_type_ident = 1;
                }
            }
        }

        if (!receiver_is_type_ident && !receiver_is_self)
        {
            const char *missing_class_self = NULL;
            int given_count = ListLength(args_given);
            for (ListNode_t *cur = overload_candidates; cur != NULL; cur = cur->next)
            {
                HashNode_t *candidate = (HashNode_t *)cur->cur;
                if (candidate == NULL || candidate->owner_class == NULL ||
                    candidate->method_name == NULL || candidate->type == NULL ||
                    candidate->type->kind != TYPE_KIND_PROCEDURE)
                    continue;
                if (!from_cparser_is_method_class_method(candidate->owner_class,
                        candidate->method_name) ||
                    from_cparser_is_method_static(candidate->owner_class,
                        candidate->method_name))
                    continue;

                ListNode_t *params = kgpc_type_get_procedure_params(candidate->type);
                if (params == NULL)
                    continue;
                Tree_t *first_param = (Tree_t *)params->cur;
                if (first_param == NULL || first_param->type != TREE_VAR_DECL ||
                    first_param->tree_data.var_decl_data.ids == NULL)
                    continue;
                const char *first_name =
                    (const char *)first_param->tree_data.var_decl_data.ids->cur;
                if (first_name == NULL || !pascal_identifier_equals(first_name, "Self"))
                    continue;

                if (ListLength(params) == given_count + 1)
                {
                    missing_class_self = candidate->owner_class;
                    break;
                }
            }

            if (missing_class_self != NULL)
            {
                struct Expression *class_expr = mk_varid(stmt->line_num,
                    strdup(missing_class_self));
                ListNode_t *class_arg = (class_expr != NULL) ?
                    CreateListNode(class_expr, LIST_EXPR) : NULL;
                if (class_arg != NULL)
                {
                    class_arg->next = args_given;
                    args_given = class_arg;
                    stmt->stmt_data.procedure_call_data.expr_args = args_given;
                }
                stmt->stmt_data.procedure_call_data.is_class_method_call = 1;
                if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                    stmt->stmt_data.procedure_call_data.self_class_name =
                        strdup(missing_class_self);
            }
        }
    }

    /* Before overload resolution, check if WITH context provides a method.
     * This prevents builtins like Concat from shadowing class methods in
     * `with obj do Concat(...)` patterns.
     * However, do NOT intercept if a global procedure/function with the same
     * name exists — e.g., system Move(src,dst,count) must not be hijacked
     * by TFPList.Move(curIndex,newIndex) in a WITH block. */
    if (match_count == 0 && proc_id != NULL && with_context_count > 0 &&
        !stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
    {
        /* Skip WITH interception if a non-method, non-builtin symbol with this
         * name exists AND has matching arity.  Builtin functions like Concat are
         * registered with 0 params but are actually variadic — they must not
         * prevent WITH methods from being found (e.g., `with LinkScript do
         * Concat('...')` where LinkScript has a Concat method). */
        HashNode_t *global_proc = NULL;
        int has_global_proc = 0;
        if (FindSymbol(&global_proc, symtab, proc_id) && global_proc != NULL &&
            (global_proc->hash_type == HASHTYPE_FUNCTION ||
             global_proc->hash_type == HASHTYPE_PROCEDURE))
        {
            /* Check if the global proc's param count matches the call args.
             * If the global proc has 0 params but the call has args, it's
             * likely a variadic builtin and should not block WITH resolution. */
            int global_param_count = 0;
            if (global_proc->type != NULL && kgpc_type_is_procedure(global_proc->type))
                global_param_count = ListLength(global_proc->type->info.proc_info.params);
            int call_arg_count = ListLength(stmt->stmt_data.procedure_call_data.expr_args);
            if (global_param_count > 0 || call_arg_count == 0)
                has_global_proc = 1;
        }

        struct Expression *with_expr = NULL;
        int wm = has_global_proc ? 1 : semcheck_with_try_resolve_method(proc_id, symtab, &with_expr, stmt->line_num);
        if ((wm == 0 || wm == 2) && with_expr != NULL)
        {
            if (wm == 2)
            {
                struct Expression *field_access = mk_recordaccess(stmt->line_num,
                    with_expr, strdup(proc_id));
                if (field_access != NULL)
                {
                    stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                    stmt->stmt_data.procedure_call_data.procedural_var_expr = field_access;
                    stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                    stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
                    DestroyList(overload_candidates);
                    free(mangled_name);
                    int field_tag = UNKNOWN_TYPE;
                    return return_val + semcheck_stmt_expr_tag(&field_tag, symtab,
                        field_access, max_scope_lev, NO_MUTATE);
                }
            }

            /* Prepend the WITH context expression as Self argument */
            ListNode_t *self_node = CreateListNode(with_expr, LIST_EXPR);
            if (self_node != NULL)
            {
                self_node->next = stmt->stmt_data.procedure_call_data.expr_args;
                stmt->stmt_data.procedure_call_data.expr_args = self_node;
                stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
                DestroyList(overload_candidates);
                overload_candidates = NULL;
                free(mangled_name);
                mangled_name = NULL;
                return semcheck_proccall(symtab, stmt, max_scope_lev);
            }
            else
            {
                destroy_expr(with_expr);
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

        int overload_status = semcheck_resolve_overload(&best_candidate, &num_best_matches,
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

        int overload_status = semcheck_resolve_overload(&best_candidate, &num_best_matches,
            overload_candidates, args_given, symtab, &call_stub, max_scope_lev, 0);

        if (overload_status == 0 && best_candidate != NULL && num_best_matches == 1 &&
            best_candidate != resolved_proc)
        {
            /* Only override the exact mangled match if the secondary resolution
             * has the same formal param count.  The exact mangled match already
             * verified the arg count via name mangling; allowing a candidate with
             * different arity to replace it causes false "not enough arguments" errors
             * when allow_implicit_leading_self adjusts arity in the resolver. */
            int best_total = 0;
            if (best_candidate->type != NULL && best_candidate->type->kind == TYPE_KIND_PROCEDURE)
                best_total = ListLength(kgpc_type_get_procedure_params(best_candidate->type));
            int resolved_total = 0;
            if (resolved_proc->type != NULL && resolved_proc->type->kind == TYPE_KIND_PROCEDURE)
                resolved_total = ListLength(kgpc_type_get_procedure_params(resolved_proc->type));
            int given_count = ListLength(args_given);
            if (best_total == given_count || best_total == resolved_total)
            {
                resolved_proc = best_candidate;
                if (best_candidate->mangled_id != NULL)
                {
                    free(mangled_name);
                    mangled_name = strdup(best_candidate->mangled_id);
                }
            }
        }
    }

    if (match_count == 1)
    {
        int has_matching_impl = 0;
        if (resolved_proc != NULL && overload_candidates != NULL)
        {
            for (ListNode_t *cand_node = overload_candidates; cand_node != NULL; cand_node = cand_node->next)
            {
                HashNode_t *cand = (HashNode_t *)cand_node->cur;
                if (cand == NULL || cand == resolved_proc || cand->type == NULL ||
                    cand->type->kind != TYPE_KIND_PROCEDURE ||
                    cand->type->info.proc_info.definition == NULL)
                    continue;
                Tree_t *cand_def = cand->type->info.proc_info.definition;
                if (cand_def->tree_data.subprogram_data.statement_list == NULL)
                    continue;
                if (resolved_proc->id != NULL && cand->id != NULL &&
                    pascal_identifier_equals(cand->id, resolved_proc->id))
                {
                    has_matching_impl = 1;
                    break;
                }
            }
        }
        if (kgpc_getenv("KGPC_DEBUG_ASSIGN") != NULL &&
            pascal_identifier_equals(proc_id, "Assign"))
            fprintf(stderr, "[ASSIGN-RESOLVED] mangled=%s match_count=%d\n",
                resolved_proc->mangled_id ? resolved_proc->mangled_id : "<null>", match_count);
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
        if (resolved_proc->type != NULL && resolved_proc->type->kind == TYPE_KIND_PROCEDURE)
        {
            Tree_t *proc_def = resolved_proc->type->info.proc_info.definition;
            if (proc_def != NULL && proc_def->tree_data.subprogram_data.statement_list == NULL)
            {
                const char *target_name = proc_def->tree_data.subprogram_data.cname_override;
                if (target_name == NULL || target_name[0] == '\0')
                {
                    if (proc_def->tree_data.subprogram_data.cname_flag)
                        target_name = proc_def->tree_data.subprogram_data.id;
                    else if (!has_matching_impl &&
                             proc_def->tree_data.subprogram_data.id != NULL &&
                             proc_def->tree_data.subprogram_data.id[0] != '\0')
                        target_name = proc_def->tree_data.subprogram_data.id;
                    else if (proc_def->tree_data.subprogram_data.mangled_id != NULL &&
                             proc_def->tree_data.subprogram_data.mangled_id[0] != '\0')
                        target_name = proc_def->tree_data.subprogram_data.mangled_id;
                    else if (resolved_proc->mangled_id != NULL &&
                             resolved_proc->mangled_id[0] != '\0')
                        target_name = resolved_proc->mangled_id;
                    else
                        target_name = resolved_proc->id;
                }
                if (target_name != NULL && target_name[0] != '\0')
                {
                    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
                        free(stmt->stmt_data.procedure_call_data.mangled_id);
                    stmt->stmt_data.procedure_call_data.mangled_id = strdup(target_name);
                }
            }
        }
        stmt->stmt_data.procedure_call_data.resolved_proc = resolved_proc;

        /* Populate call info to avoid use-after-free when HashNode is freed */
        stmt->stmt_data.procedure_call_data.call_hash_type = resolved_proc->hash_type;
        semcheck_stmt_set_call_kgpc_type(stmt, resolved_proc->type,
            stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
        semcheck_stmt_set_call_owner_info(stmt,
            resolved_proc->owner_class, resolved_proc->method_name);
        stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
        semcheck_mark_call_requires_static_link(resolved_proc);

        /* Centralized virtual dispatch fallback — catches abstract virtual methods
         * that weren't detected by the early Self-injection check. Only applies to
         * methods without a body (abstract) and not class/static methods (which use
         * single-indirection VMT dispatch that codegen doesn't support yet). */
        int resolved_param_count = -1;
        if (resolved_proc->type != NULL && resolved_proc->type->kind == TYPE_KIND_PROCEDURE)
        {
            resolved_param_count = ListLength(resolved_proc->type->info.proc_info.params);
            if (resolved_proc->owner_class != NULL &&
                !from_cparser_is_method_static(resolved_proc->owner_class,
                    resolved_proc->method_name))
            {
                if (resolved_param_count > 0)
                    resolved_param_count -= 1;
                else
                    resolved_param_count = 0;
            }
        }
        if (resolved_proc->owner_class != NULL && resolved_proc->method_name != NULL &&
            !stmt->stmt_data.procedure_call_data.is_virtual_call &&
            !from_cparser_is_method_static(resolved_proc->owner_class,
                resolved_proc->method_name) &&
            from_cparser_is_method_virtual_with_signature(resolved_proc->owner_class,
                resolved_proc->method_name,
                resolved_param_count,
                NULL))
        {
            struct RecordType *class_record = semcheck_lookup_record_type(symtab,
                resolved_proc->owner_class);
            if (class_record != NULL && record_type_is_class(class_record) &&
                class_record->methods != NULL)
            {
                struct MethodInfo *fallback_virtual = NULL;
                for (ListNode_t *me = class_record->methods; me != NULL; me = me->next)
                {
                    struct MethodInfo *mi = (struct MethodInfo *)me->cur;
                    if (mi != NULL && mi->name != NULL &&
                        (mi->is_virtual || mi->is_override) &&
                        strcasecmp(mi->name, resolved_proc->method_name) == 0)
                    {
                        if (fallback_virtual == NULL)
                            fallback_virtual = mi;
                        if (resolved_param_count >= 0 && mi->param_count >= 0 &&
                            resolved_param_count != mi->param_count)
                        {
                            continue;
                        }
                        stmt->stmt_data.procedure_call_data.is_virtual_call = 1;
                        stmt->stmt_data.procedure_call_data.vmt_index = mi->vmt_index;
                        if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                            stmt->stmt_data.procedure_call_data.self_class_name =
                                strdup(resolved_proc->owner_class);
                        break;
                    }
                }
                if (!stmt->stmt_data.procedure_call_data.is_virtual_call &&
                    fallback_virtual != NULL)
                {
                    stmt->stmt_data.procedure_call_data.is_virtual_call = 1;
                    stmt->stmt_data.procedure_call_data.vmt_index = fallback_virtual->vmt_index;
                    if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                        stmt->stmt_data.procedure_call_data.self_class_name =
                            strdup(resolved_proc->owner_class);
                }
            }
        }
        /* Interface method call check — if the owner class is an interface,
         * mark this as an interface call so codegen emits indirect vtable dispatch.
         * Only mark as interface call when Self is actually interface-typed:
         * check the first argument's resolved type to avoid false positives on
         * standalone procedures whose name matches an interface method pattern. */
        if (resolved_proc->owner_class != NULL && resolved_proc->method_name != NULL &&
            !stmt->stmt_data.procedure_call_data.is_interface_call)
        {
            struct RecordType *iface_record = semcheck_lookup_record_type(symtab,
                resolved_proc->owner_class);
            if (iface_record != NULL && iface_record->is_interface &&
                iface_record->method_templates != NULL)
            {
                /* Verify the first argument (Self) is actually interface-typed.
                 * This prevents false positives on direct calls to standalone
                 * procedures like IMyCounter__DoIncrement(p) where p is Pointer. */
                int self_is_interface = 0;
                ListNode_t *call_args = stmt->stmt_data.procedure_call_data.expr_args;
                if (call_args != NULL)
                {
                    struct Expression *self_arg = (struct Expression *)call_args->cur;
                    if (self_arg != NULL && self_arg->resolved_kgpc_type != NULL)
                    {
                        KgpcType *self_type = self_arg->resolved_kgpc_type;
                        /* Dereference pointer to get the underlying record type */
                        if (self_type->kind == TYPE_KIND_POINTER && self_type->info.points_to != NULL)
                            self_type = self_type->info.points_to;
                        if (self_type->kind == TYPE_KIND_RECORD && self_type->info.record_info != NULL &&
                            self_type->info.record_info->is_interface)
                            self_is_interface = 1;
                    }
                }
                if (self_is_interface)
                {
                    int idx = 0;
                    for (ListNode_t *mt = iface_record->method_templates; mt != NULL; mt = mt->next, idx++)
                    {
                        struct MethodTemplate *tmpl = (struct MethodTemplate *)mt->cur;
                        if (tmpl != NULL && tmpl->name != NULL &&
                            strcasecmp(tmpl->name, resolved_proc->method_name) == 0)
                        {
                            stmt->stmt_data.procedure_call_data.is_interface_call = 1;
                            stmt->stmt_data.procedure_call_data.vmt_index = idx;
                            if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                                stmt->stmt_data.procedure_call_data.self_class_name =
                                    strdup(resolved_proc->owner_class);
                            break;
                        }
                    }
                }
            }
        }

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
                            
                            if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
                                fprintf(stderr, "[SemCheck] Added default arg %d for %s\n", 
                                    arg_index, proc_id != NULL ? proc_id : "(null)");
                            }
                        }
                        else
                        {
                            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, 
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
        scope_return = 1; // found
    }
    else if (match_count == 0)
    {
        HashNode_t *proc_var = NULL;
        /* Check for procedure variables (HASHTYPE_VAR) or procedure constants (HASHTYPE_CONST)
         * with a procedural type. This allows calling procedure variables and typed constants
         * that hold procedure addresses like: const MyProcRef: TProc = @MyProc; */
        if (FindSymbol(&proc_var, symtab, proc_id) && proc_var != NULL &&
            (proc_var->hash_type == HASHTYPE_VAR || proc_var->hash_type == HASHTYPE_CONST) &&
            proc_var->type != NULL && proc_var->type->kind == TYPE_KIND_PROCEDURE)
        {
            DestroyList(overload_candidates);
            free(mangled_name);

            proc_var->referenced += 1;
            if (0) /* scope depth check removed — tree scoping has no depth */
            {
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, %s cannot be called in the current context!\n\n",
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

        /* WITH context fallback: if the procedure call couldn't be resolved in
         * normal scope, try resolving via active WITH contexts.  This handles
         * patterns like:  with SomeList.LockList do Add(Self);
         * where Add is a method of the WITH target's class. */
        if (proc_id != NULL && with_context_count > 0)
        {
            struct Expression *with_expr = NULL;
            int wm = semcheck_with_try_resolve_method(proc_id, symtab, &with_expr, stmt->line_num);
            if ((wm == 0 || wm == 2) && with_expr != NULL)
            {
                if (wm == 2)
                {
                    struct Expression *field_access = mk_recordaccess(stmt->line_num,
                        with_expr, strdup(proc_id));
                    if (field_access == NULL)
                    {
                        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d: failed to allocate procedural field expression.\n",
                            stmt->line_num);
                        DestroyList(overload_candidates);
                        free(mangled_name);
                        return return_val + 1;
                    }

                    stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                    stmt->stmt_data.procedure_call_data.procedural_var_expr = field_access;
                    stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                    stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;

                    DestroyList(overload_candidates);
                    free(mangled_name);

                    {
                        int field_tag = UNKNOWN_TYPE;
                        return return_val + semcheck_stmt_expr_tag(&field_tag, symtab,
                            field_access, max_scope_lev, NO_MUTATE);
                    }
                }

                /* Prepend the WITH context expression as Self argument */
                ListNode_t *self_node = CreateListNode(with_expr, LIST_EXPR);
                if (self_node != NULL)
                {
                    self_node->next = stmt->stmt_data.procedure_call_data.expr_args;
                    stmt->stmt_data.procedure_call_data.expr_args = self_node;
                    /* Mark as method call so the retry resolves via class method lookup */
                    stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
                    /* Free overload list before retry */
                    DestroyList(overload_candidates);
                    overload_candidates = NULL;
                    free(mangled_name);
                    mangled_name = NULL;
                    /* Re-evaluate as a method call from scratch */
                    return semcheck_proccall(symtab, stmt, max_scope_lev);
                }
                else
                {
                    destroy_expr(with_expr);
                }
            }
        }

        /* Build detailed error message with argument types and available overloads */
        {
            /* First, build a string showing the actual argument types */
            char arg_types_buf[1024] = "(";
            int buf_pos = 1;
            int any_arg_unknown = 0;
            if (args_given != NULL)
            {
                int idx = 0;
                for (ListNode_t *cur = args_given; cur != NULL; cur = cur->next)
                {
                    struct Expression *arg = (struct Expression *)cur->cur;
                    int tag = UNKNOWN_TYPE;
                    semcheck_stmt_expr_tag(&tag, symtab, arg, max_scope_lev, NO_MUTATE);
                    if (tag == UNKNOWN_TYPE)
                        any_arg_unknown = 1;
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

            /* Suppress error when any argument has UNKNOWN_TYPE — the root cause
             * error was already reported upstream. */
            if (any_arg_unknown)
            {
                /* Silently skip — cascading from unresolved arg types */
            }
            else if (overloads_buf[0] != '\0')
            {
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                    "Error on line %d, call to procedure %s%s does not match any available overload.\n"
                    "Available overloads:\n%s",
                    stmt->line_num, proc_id, arg_types_buf, overloads_buf);
                ++return_val;
            }
            else
            {
                if (stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
                {
                    HashNode_t *synth_node = NULL;
                    if (FindSymbol(&synth_node, symtab, proc_id) == 0)
                    {
                        KgpcType *synth_type = create_procedure_type(NULL, NULL);
                        if (synth_type != NULL)
                        {
                            char *id_dup = strdup(proc_id);
                            char *mangled_dup = strdup(proc_id);
                            if (id_dup != NULL && mangled_dup != NULL)
                                (void)PushProcedureOntoScope_Typed(symtab, id_dup, mangled_dup, synth_type);
                            else
                            {
                                free(id_dup);
                                free(mangled_dup);
                            }
                            destroy_kgpc_type(synth_type);
                        }
                    }
                    DestroyList(overload_candidates);
                    free(mangled_name);
                    return return_val;
                }
                /* No overloads found - procedure is not declared */
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                    "Error on line %d, procedure %s%s is not declared.\n",
                    stmt->line_num, proc_id, arg_types_buf);
                ++return_val;
            }
        }
        DestroyList(overload_candidates);
        free(mangled_name);
        return return_val;
    }
    else
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, call to procedure %s is ambiguous\n", stmt->line_num, proc_id);
        DestroyList(overload_candidates);
        free(mangled_name);
        return ++return_val;
    }
    DestroyList(overload_candidates);
    free(mangled_name);

    if(!scope_return) // Should not happen if match_count > 0
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, unrecognized procedure call %s\n", stmt->line_num,
            proc_id);
        ++return_val;
    }
    else
    {
        if (with_context_count > 0 &&
            proc_id != NULL &&
            sym_return != NULL &&
            sym_return->owner_class == NULL &&
            !stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
        {
            int try_with_override = 0;
            if (sym_return->type != NULL && sym_return->type->kind == TYPE_KIND_PROCEDURE)
            {
                ListNode_t *params = kgpc_type_get_procedure_params(sym_return->type);
                if (params != NULL && params->cur != NULL)
                {
                    Tree_t *first_decl = (Tree_t *)params->cur;
                    const char *first_type_id = NULL;
                    if (first_decl != NULL && first_decl->type == TREE_VAR_DECL)
                        first_type_id = first_decl->tree_data.var_decl_data.type_id;
                    else if (first_decl != NULL && first_decl->type == TREE_ARR_DECL)
                        first_type_id = first_decl->tree_data.arr_decl_data.type_id;
                    if (first_type_id != NULL &&
                        strlen(first_type_id) == 1 &&
                        first_type_id[0] >= 'A' && first_type_id[0] <= 'Z')
                    {
                        try_with_override = 1;
                    }
                }
            }
            if (try_with_override)
            {
                struct Expression *with_expr = NULL;
                int wm = semcheck_with_try_resolve_method(proc_id, symtab, &with_expr, stmt->line_num);
                if (wm == 0 && with_expr != NULL)
                {
                    ListNode_t *self_node = CreateListNode(with_expr, LIST_EXPR);
                    if (self_node != NULL)
                    {
                        self_node->next = stmt->stmt_data.procedure_call_data.expr_args;
                        stmt->stmt_data.procedure_call_data.expr_args = self_node;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
                        if (stmt->stmt_data.procedure_call_data.placeholder_method_name == NULL)
                            stmt->stmt_data.procedure_call_data.placeholder_method_name = strdup(proc_id);
                        return semcheck_proccall(symtab, stmt, max_scope_lev);
                    }
                    destroy_expr(with_expr);
                }
            }
        }

        sym_return->referenced += 1; /* Moved here: only access if sym_return is valid */

        if (sym_return->type != NULL && sym_return->type->kind == TYPE_KIND_PROCEDURE)
        {
            ListNode_t *formal_params = kgpc_type_get_procedure_params(sym_return->type);
            if (append_default_args(&args_given, formal_params, stmt->line_num) != 0)
                ++return_val;
            stmt->stmt_data.procedure_call_data.expr_args = args_given;
        }

        if(0) /* scope depth check removed — tree scoping has no depth */
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, %s cannot be called in the current context!\n\n",
                stmt->line_num, proc_id);
            fprintf(stderr, "[Was it defined above the current function context?]\n");

            ++return_val;
        }
        if(sym_return->hash_type != HASHTYPE_PROCEDURE &&
            sym_return->hash_type != HASHTYPE_BUILTIN_PROCEDURE &&
            sym_return->hash_type != HASHTYPE_FUNCTION)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, expected %s to be a procedure, function, or builtin!\n\n",
                stmt->line_num, proc_id);

            ++return_val;
        }

        /***** VERIFY ARGUMENTS USING KGPCTYPE ARCHITECTURE *****/
        const char *callee_owner_full = sym_return->owner_class_full;
        const char *callee_owner_outer = sym_return->owner_class_outer;
        if (callee_owner_full == NULL && callee_owner_outer == NULL)
        {
            Tree_t *proc_def = sym_return->type->info.proc_info.definition;
            if (proc_def != NULL && proc_def->type == TREE_SUBPROGRAM)
            {
                callee_owner_full = proc_def->tree_data.subprogram_data.owner_class_full;
                callee_owner_outer = proc_def->tree_data.subprogram_data.owner_class_outer;
                if (callee_owner_full == NULL)
                    callee_owner_full = proc_def->tree_data.subprogram_data.owner_class;
            }
        }
        cur_arg = 0;
        /* Get formal arguments from KgpcType instead of deprecated args field */
        true_args = kgpc_type_get_procedure_params(sym_return->type);
        /* Skip implicit Self parameter when args don't include it
         * (e.g., ClassName.Create(args) where the type qualifier was stripped) */
        if (true_args != NULL && true_args->cur != NULL)
        {
            Tree_t *first_formal = (Tree_t *)true_args->cur;
            if (first_formal->type == TREE_VAR_DECL &&
                first_formal->tree_data.var_decl_data.ids != NULL)
            {
                const char *ff_id = (const char *)first_formal->tree_data.var_decl_data.ids->cur;
                if (ff_id != NULL && pascal_identifier_equals(ff_id, "Self"))
                {
                    int n_args = ListLength(args_given);
                    int n_params = ListLength(true_args);
                    if (n_args == n_params - 1)
                        true_args = true_args->next;
                }
            }
        }
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

                if (semcheck_prepare_array_literal_argument(arg_decl, arg_expr,
                        symtab, INT_MAX, stmt->line_num) != 0)
                {
                    ++return_val;
                    args_given = args_given->next;
                    true_arg_ids = true_arg_ids->next;
                    continue;
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
                KgpcType *expected_kgpc_type = resolve_param_type_with_owner(arg_decl, symtab,
                    callee_owner_full, callee_owner_outer, &expected_type_owned);
                if (kgpc_getenv("KGPC_DEBUG_FMTSTR") != NULL && proc_id != NULL &&
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
                if (kgpc_getenv("KGPC_DEBUG_FMTSTR") != NULL && proc_id != NULL &&
                    strcasecmp(proc_id, "FmtStr") == 0)
                {
                    fprintf(stderr,
                        "[KGPC_DEBUG_FMTSTR] param %d expected=%s arg=%s\n",
                        cur_arg,
                        expected_kgpc_type ? kgpc_type_to_string(expected_kgpc_type) : "<null>",
                        arg_kgpc_type ? kgpc_type_to_string(arg_kgpc_type) : "<null>");
                }
                int param_is_untyped = semcheck_var_decl_is_untyped(arg_decl);
                if (param_is_untyped &&
                    proc_id != NULL &&
                    cur_arg == 1 &&
                    (pascal_identifier_equals(proc_id, "write") ||
                     pascal_identifier_equals(proc_id, "read") ||
                     pascal_identifier_equals(proc_id, "fpWrite") ||
                     pascal_identifier_equals(proc_id, "fpRead")) &&
                    arg_expr != NULL &&
                    arg_expr->type != EXPR_ADDR &&
                    arg_kgpc_type != NULL &&
                    !kgpc_type_is_pointer(arg_kgpc_type) &&
                    (arg_expr->type == EXPR_VAR_ID ||
                     arg_expr->type == EXPR_ARRAY_ACCESS ||
                     arg_expr->type == EXPR_RECORD_ACCESS ||
                     arg_expr->type == EXPR_POINTER_DEREF))
                {
                    struct Expression *addr_expr = mk_addressof(arg_expr->line_num, arg_expr);
                    KgpcType *new_arg_kgpc_type = NULL;
                    int new_arg_type_owned = 0;
                    semcheck_expr_main(symtab, addr_expr, INT_MAX, NO_MUTATE, &new_arg_kgpc_type);
                    if (new_arg_kgpc_type == NULL)
                    {
                        new_arg_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, addr_expr,
                            INT_MAX, NO_MUTATE, &new_arg_type_owned);
                    }
                    if (new_arg_kgpc_type != NULL && kgpc_type_is_pointer(new_arg_kgpc_type))
                    {
                        args_given->cur = addr_expr;
                        arg_expr = addr_expr;
                        arg_kgpc_type = new_arg_kgpc_type;
                        arg_type_owned = new_arg_type_owned;
                    }
                    else if (addr_expr != NULL)
                    {
                        addr_expr->expr_data.addr_data.expr = NULL;
                        destroy_expr(addr_expr);
                    }
                }

                /* Perform type compatibility check using KgpcType */
                int types_match = param_is_untyped ? 1 : 0;
                if ((expected_kgpc_type == NULL || arg_kgpc_type == NULL) && !param_is_untyped)
                {
                    /* Suppress cascading errors when types can't be resolved —
                     * upstream UNKNOWN_TYPE already reported the root cause. */
                }
                else if (!param_is_untyped)
                {
                    types_match = are_types_compatible_for_assignment(expected_kgpc_type, arg_kgpc_type, symtab);
                    if (!types_match && expected_kgpc_type != NULL && arg_kgpc_type != NULL &&
                        expected_kgpc_type->kind == TYPE_KIND_ARRAY &&
                        arg_kgpc_type->kind == TYPE_KIND_ARRAY)
                    {
                        KgpcType *expected_elem = kgpc_type_get_array_element_type_resolved(expected_kgpc_type, symtab);
                        KgpcType *arg_elem = kgpc_type_get_array_element_type_resolved(arg_kgpc_type, symtab);
                        if (expected_elem != NULL && arg_elem != NULL)
                        {
                            if (kgpc_type_equals(expected_elem, arg_elem) ||
                                are_types_compatible_for_assignment(expected_elem, arg_elem, symtab) ||
                                (kgpc_type_is_pointer(expected_elem) &&
                                 kgpc_type_equals_tag(arg_elem, POINTER_TYPE)) ||
                                (kgpc_type_is_pointer(arg_elem) &&
                                 kgpc_type_equals_tag(expected_elem, POINTER_TYPE)))
                            {
                                types_match = 1;
                            }
                        }
                    }
                    if (!types_match && expected_kgpc_type != NULL && arg_kgpc_type != NULL &&
                        arg_expr != NULL &&
                        expected_kgpc_type->kind == TYPE_KIND_POINTER &&
                        expected_kgpc_type->info.points_to != NULL &&
                        expected_kgpc_type->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
                        expected_kgpc_type->info.points_to->info.primitive_type_tag == CHAR_TYPE &&
                        arg_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                        arg_kgpc_type->info.primitive_type_tag == CHAR_TYPE &&
                        arg_expr->type != EXPR_ADDR &&
                        (arg_expr->type == EXPR_VAR_ID ||
                         arg_expr->type == EXPR_ARRAY_ACCESS ||
                         arg_expr->type == EXPR_RECORD_ACCESS ||
                         arg_expr->type == EXPR_POINTER_DEREF))
                    {
                        struct Expression *addr_expr = mk_addressof(arg_expr->line_num, arg_expr);
                        KgpcType *new_arg_kgpc_type = NULL;
                        int new_arg_type_owned = 0;

                        semcheck_expr_main(symtab, addr_expr, INT_MAX, NO_MUTATE, &new_arg_kgpc_type);
                        if (new_arg_kgpc_type == NULL)
                        {
                            new_arg_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, addr_expr,
                                INT_MAX, NO_MUTATE, &new_arg_type_owned);
                        }

                        if (new_arg_kgpc_type != NULL &&
                            are_types_compatible_for_assignment(expected_kgpc_type, new_arg_kgpc_type, symtab))
                        {
                            args_given->cur = addr_expr;
                            arg_expr = addr_expr;
                            if (arg_type_owned && arg_kgpc_type != NULL)
                                destroy_kgpc_type(arg_kgpc_type);
                            arg_kgpc_type = new_arg_kgpc_type;
                            arg_type_owned = new_arg_type_owned;
                            types_match = 1;
                        }
                        else
                        {
                            if (new_arg_type_owned && new_arg_kgpc_type != NULL)
                                destroy_kgpc_type(new_arg_kgpc_type);
                            if (addr_expr != NULL)
                            {
                                addr_expr->expr_data.addr_data.expr = NULL;
                                destroy_expr(addr_expr);
                            }
                        }
                    }
                    /* Class method Self compatibility: if expected is record and given
                     * is ^record (or vice versa), and this is argument 1 (Self) of a
                     * class method, accept the match. Classes are reference types so
                     * Self is always a pointer, but the formal parameter may have been
                     * registered with the plain record type due to type alias collisions. */
                    if (!types_match && cur_arg == 1 &&
                        expected_kgpc_type != NULL && arg_kgpc_type != NULL)
                    {
                        int expected_is_record = (expected_kgpc_type->kind == TYPE_KIND_RECORD);
                        int given_is_ptr_record = (arg_kgpc_type->kind == TYPE_KIND_POINTER &&
                            arg_kgpc_type->info.points_to != NULL &&
                            arg_kgpc_type->info.points_to->kind == TYPE_KIND_RECORD);
                        int expected_is_ptr_record = (expected_kgpc_type->kind == TYPE_KIND_POINTER &&
                            expected_kgpc_type->info.points_to != NULL &&
                            expected_kgpc_type->info.points_to->kind == TYPE_KIND_RECORD);
                        int given_is_record = (arg_kgpc_type->kind == TYPE_KIND_RECORD);
                        if ((expected_is_record && given_is_ptr_record) ||
                            (expected_is_ptr_record && given_is_record))
                        {
                            types_match = 1;
                        }
                    }
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
                        if (FindSymbol(&arg_node, symtab, arg_expr->expr_data.id) != 0 &&
                            arg_node != NULL && arg_node->hash_type == HASHTYPE_PROCEDURE)
                        {
                            /* Transform the expression to EXPR_ADDR_OF_PROC */
                            arg_expr->type = EXPR_ADDR_OF_PROC;
                            arg_expr->expr_data.addr_of_proc_data.proc_mangled_id = arg_node->mangled_id ? strdup(arg_node->mangled_id) : NULL;
                            arg_expr->expr_data.addr_of_proc_data.proc_id = arg_node->id ? strdup(arg_node->id) : NULL;
                            /* Resolve the type NOW while the symbol is still alive. */
                            if (arg_node->type != NULL)
                            {
                                kgpc_type_retain(arg_node->type);
                                arg_expr->resolved_kgpc_type = create_pointer_type(arg_node->type);
                            }
                            else
                            {
                                arg_expr->resolved_kgpc_type = create_pointer_type(NULL);
                            }
                        }
                    }
                }

                /* Save type strings before cleanup for error message */
                char expected_type_str[256] = "<unknown>";
                char given_type_str[256] = "<unknown>";
                const char *formal_id_dbg = NULL;
                if (arg_decl != NULL && arg_decl->type == TREE_VAR_DECL)
                    formal_id_dbg = arg_decl->tree_data.var_decl_data.type_id;
                else if (arg_decl != NULL && arg_decl->type == TREE_ARR_DECL)
                    formal_id_dbg = arg_decl->tree_data.arr_decl_data.type_id;
                if (!types_match && arg_expr != NULL &&
                    formal_id_dbg != NULL &&
                    arg_expr->pointer_subtype_id != NULL &&
                    semcheck_class_type_ids_compatible(symtab, formal_id_dbg,
                        arg_expr->pointer_subtype_id))
                {
                    types_match = 1;
                }
                if (expected_kgpc_type != NULL)
                    snprintf(expected_type_str, sizeof(expected_type_str), "%s", kgpc_type_to_string(expected_kgpc_type));
                if (arg_kgpc_type != NULL)
                    snprintf(given_type_str, sizeof(given_type_str), "%s", kgpc_type_to_string(arg_kgpc_type));

                /* Check for UNKNOWN_TYPE before cleanup */
                int either_unknown = (kgpc_type_equals_tag(expected_kgpc_type, UNKNOWN_TYPE) ||
                                      kgpc_type_equals_tag(arg_kgpc_type, UNKNOWN_TYPE));

                /* Clean up owned types */
                if (expected_type_owned && expected_kgpc_type != NULL)
                    destroy_kgpc_type(expected_kgpc_type);
                if (arg_type_owned && arg_kgpc_type != NULL)
                    destroy_kgpc_type(arg_kgpc_type);

                if (!types_match && !either_unknown)
                {
                    if (kgpc_getenv("KGPC_DEBUG_SYMCREAT_INSERTSYM") != NULL &&
                        proc_id != NULL &&
                        pascal_identifier_equals(proc_id, "TSymtable__insertsym"))
                    {
                        fprintf(stderr,
                            "[KGPC_DEBUG_SYMCREAT_INSERTSYM] arg=%d formal_id=%s expected=%s actual=%s expr_type=%d ptr_sub=%d ptr_id=%s\n",
                            cur_arg,
                            formal_id_dbg != NULL ? formal_id_dbg : "<null>",
                            expected_type_str,
                            given_type_str,
                            arg_expr != NULL ? arg_expr->type : -1,
                            arg_expr != NULL ? arg_expr->pointer_subtype : -1,
                            (arg_expr != NULL && arg_expr->pointer_subtype_id != NULL)
                                ? arg_expr->pointer_subtype_id : "<null>");
                    }
                    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
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
        if(true_args == NULL && args_given != NULL && !(sym_return != NULL && sym_return->is_varargs) &&
            !(sym_return != NULL && sym_return->hash_type == HASHTYPE_BUILTIN_PROCEDURE))
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

/* Transform TP-style New(p, Constructor(args)) into New(p) + p^.Constructor(args)
 * and Dispose(p, Destructor) into p^.Destructor + Dispose(p).
 * Returns a new statement to insert after (for New) or before (for Dispose) the
 * current statement in the list, or NULL if no transformation needed. */
static struct Statement *transform_two_arg_new_dispose(struct Statement *stmt, int *is_dispose)
{
    if (stmt == NULL || stmt->type != STMT_PROCEDURE_CALL)
        return NULL;

    char *proc_id = stmt->stmt_data.procedure_call_data.id;
    if (proc_id == NULL)
        return NULL;

    int is_new = pascal_identifier_equals(proc_id, "New");
    int is_disp = pascal_identifier_equals(proc_id, "Dispose");
    if (!is_new && !is_disp)
        return NULL;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || (args->next != NULL && args->next->next != NULL))
        return NULL;  /* Not exactly 2 args */

    if (is_dispose)
        *is_dispose = is_disp;

    /* Extract the pointer expr (first arg) and ctor/dtor expr (second arg) */
    struct Expression *ptr_expr = (struct Expression *)args->cur;
    struct Expression *method_expr = (struct Expression *)args->next->cur;

    /* Strip the second arg from the New/Dispose call, making it single-arg */
    args->next->cur = NULL;
    ListNode_t *second_node = args->next;
    args->next = NULL;
    free(second_node);

    /* Build the method call statement: p^.Method(args) */
    if (method_expr == NULL || ptr_expr == NULL)
        return NULL;

    /* Get the method name and args from the method_expr.
     * method_expr is either:
     *   - EXPR_FUNCTION_CALL for Create(42) — id="Create", args=[42]
     *   - EXPR_VAR_ID for Destroy — id="Destroy"
     */
    char *method_name = NULL;
    ListNode_t *method_args = NULL;

    if (method_expr->type == EXPR_FUNCTION_CALL)
    {
        const char *fn_id = method_expr->expr_data.function_call_data.id;
        if (fn_id != NULL)
            method_name = strdup(fn_id);
        method_args = method_expr->expr_data.function_call_data.args_expr;
        /* Detach args from the expression so we can reuse them */
        method_expr->expr_data.function_call_data.args_expr = NULL;
        destroy_expr(method_expr);
    }
    else if (method_expr->type == EXPR_VAR_ID)
    {
        if (method_expr->expr_data.id != NULL)
            method_name = strdup(method_expr->expr_data.id);
        destroy_expr(method_expr);
    }
    else
    {
        destroy_expr(method_expr);
        return NULL;
    }

    if (method_name == NULL)
        return NULL;

    /* Build the placeholder proc name: __MethodName */
    size_t name_len = strlen(method_name) + 3;
    char *placeholder_name = (char *)malloc(name_len);
    if (placeholder_name == NULL)
    {
        free(method_name);
        return NULL;
    }
    snprintf(placeholder_name, name_len, "__%s", method_name);

    /* Build the receiver expression: clone of ptr_expr followed by ^ deref */
    struct Expression *receiver = mk_pointer_deref(stmt->line_num,
        clone_expression(ptr_expr));

    /* Build the argument list: receiver (self) + method args */
    ListNode_t *call_args = (ListNode_t *)calloc(1, sizeof(ListNode_t));
    call_args->type = LIST_EXPR;
    call_args->cur = receiver;
    call_args->next = method_args;  /* may be NULL */

    struct Statement *method_call = mk_procedurecall(stmt->line_num, placeholder_name, call_args);
    if (method_call != NULL)
    {
        method_call->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
        method_call->stmt_data.procedure_call_data.placeholder_method_name = method_name;
    }
    else
    {
        free(method_name);
    }

    return method_call;
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
            /* Transform two-arg New(p,Ctor)/Dispose(p,Dtor) before semcheck */
            int is_dispose = 0;
            struct Statement *extra_stmt = transform_two_arg_new_dispose(
                (struct Statement *)stmt_list->cur, &is_dispose);
            if (extra_stmt != NULL)
            {
                ListNode_t *new_node = (ListNode_t *)calloc(1, sizeof(ListNode_t));
                new_node->type = LIST_STMT;
                if (is_dispose)
                {
                    /* Dispose: insert destructor call BEFORE Dispose(p) */
                    new_node->cur = stmt_list->cur;
                    new_node->next = stmt_list->next;
                    stmt_list->cur = extra_stmt;
                    stmt_list->next = new_node;
                }
                else
                {
                    /* New: insert constructor call AFTER New(p) */
                    new_node->cur = extra_stmt;
                    new_node->next = stmt_list->next;
                    stmt_list->next = new_node;
                }
            }

            return_val += semcheck_stmt(symtab,
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

    if(if_type != BOOL && if_type != UNKNOWN_TYPE)
    {
        int err_line = stmt->line_num;
        int err_col = stmt->col_num;
        int err_source_index = -1;
        semcheck_expr_best_context(relop_expr, &err_line, &err_col, &err_source_index);
        semcheck_error_with_context_at(err_line, err_col, err_source_index,
                "Error on line %d, expected relational inside if statement!\n\n",
                err_line);
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
    if(while_type != BOOL && while_type != UNKNOWN_TYPE)
    {
        int err_line = stmt->line_num;
        int err_col = stmt->col_num;
        int err_source_index = -1;
        semcheck_expr_best_context(relop_expr, &err_line, &err_col, &err_source_index);
        semcheck_error_with_context_at(err_line, err_col, err_source_index,
                "Error on line %d, expected relational inside while statement!\n\n",
                err_line);
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
    if (until_type != BOOL && until_type != UNKNOWN_TYPE)
    {
        int err_line = stmt->line_num;
        int err_col = stmt->col_num;
        int err_source_index = -1;
        semcheck_expr_best_context(stmt->stmt_data.repeat_data.until_expr,
            &err_line, &err_col, &err_source_index);
        semcheck_error_with_context_at(err_line, err_col, err_source_index,
            "Error on line %d, expected relational inside repeat statement!\n\n",
            err_line);
        ++return_val;
    }

    return return_val;
}

/** FOR **/
int semcheck_for(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;
    int for_type = UNKNOWN_TYPE, to_type = UNKNOWN_TYPE;
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
        if(!is_ordinal_type(for_type) && for_type != UNKNOWN_TYPE)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, expected ordinal type in \"for\" assignment!\n\n",
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

    if (!bounds_compatible && for_type != UNKNOWN_TYPE && to_type != UNKNOWN_TYPE)
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, type mismatch in \"to\" assignment!\n\n",
                stmt->line_num);
        ++return_val;
    }

    if (for_kgpc_type != NULL && !is_ordinal_type(for_type) && for_type != UNKNOWN_TYPE)
    {
        int legacy = semcheck_tag_from_kgpc(for_kgpc_type);
        if (!is_ordinal_type(legacy) && legacy != UNKNOWN_TYPE)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, expected ordinal type in \"for\" assignment!\n\n",
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
        int collection_is_set = 0;
        int collection_is_enum_domain = 0;
        int collection_is_enumerator_class = 0;
        const char *list_element_id = NULL;

        return_val += semcheck_stmt_expr_tag(&collection_type, symtab, collection, INT_MAX, NO_MUTATE);
        collection_is_string = is_string_type(collection_type);
        
        KgpcType *collection_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, collection, 
                                                                            INT_MAX, NO_MUTATE, &collection_type_owned);
        if (collection_kgpc_type != NULL) {
            if (kgpc_type_is_array(collection_kgpc_type)) {
                collection_is_array = 1;
            } else if (kgpc_type_is_set(collection_kgpc_type)) {
                collection_is_set = 1;
            } else if (collection_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                       (collection_kgpc_type->info.primitive_type_tag == ENUM_TYPE ||
                        is_integer_type(collection_kgpc_type->info.primitive_type_tag)) &&
                       collection != NULL &&
                       collection->type == EXPR_VAR_ID &&
                       collection->expr_data.id != NULL) {
                HashNode_t *type_node = NULL;
                if (FindSymbol(&type_node, symtab, collection->expr_data.id) != 0 &&
                    type_node != NULL &&
                    type_node->hash_type == HASHTYPE_TYPE)
                {
                    collection_is_enum_domain = 1;
                }
            } else {
                /* Lists are represented as pointers to class records */
                KgpcType *record_candidate = collection_kgpc_type;
                if (kgpc_type_is_pointer(collection_kgpc_type))
                    record_candidate = collection_kgpc_type->info.points_to;

                if (record_candidate != NULL && kgpc_type_is_record(record_candidate)) {
                    struct RecordType *record_info = kgpc_type_get_record(record_candidate);
                    if (record_info != NULL) {
                        /* Prefer structured generic info over mangled-name parsing. */
                        if (record_info->generic_decl != NULL &&
                            record_info->generic_args != NULL &&
                            record_info->num_generic_args > 0 &&
                            record_info->generic_decl->name != NULL &&
                            pascal_identifier_equals(record_info->generic_decl->name, "TFPGList"))
                        {
                            collection_is_list = 1;
                            list_element_id = record_info->generic_args[0];
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

        KgpcType *enumerator_current_type = NULL;
        if (!collection_is_array && !collection_is_list &&
            !collection_is_set && !collection_is_enum_domain)
        {
            collection_is_enumerator_class =
                semcheck_collection_is_enumerator_class(symtab, collection_kgpc_type,
                    &enumerator_current_type);
        }

        if (collection_is_string)
            collection_is_array = 1;

        if (!collection_is_array && !collection_is_list &&
            !collection_is_set && !collection_is_enum_domain &&
            !collection_is_enumerator_class &&
            collection_type != RECORD_TYPE && collection_type != POINTER_TYPE &&
            collection_type != UNKNOWN_TYPE) {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d: for-in loop requires an array expression!\n\n",
                    stmt->line_num);
            ++return_val;
        } else if (collection_is_enumerator_class) {
            int loop_var_type_owned = 0;
            KgpcType *loop_var_kgpc = semcheck_resolve_expression_kgpc_type(symtab, loop_var,
                max_scope_lev, MUTATE, &loop_var_type_owned);

            if (loop_var_kgpc == NULL || enumerator_current_type == NULL ||
                !kgpc_type_equals(loop_var_kgpc, enumerator_current_type))
            {
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d: for-in loop variable type does not match enumerator Current type!\n\n",
                        stmt->line_num);
                ++return_val;
            }

            if (loop_var_type_owned && loop_var_kgpc != NULL)
                destroy_kgpc_type(loop_var_kgpc);
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
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d: for-in loop variable must be an ordinal type!\n\n",
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
        if (enumerator_current_type != NULL)
            destroy_kgpc_type(enumerator_current_type);
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

    if (!types_compatible && type_first != UNKNOWN_TYPE && type_second != UNKNOWN_TYPE)
    {
        semcheck_error_with_context_at(for_assign->line_num, for_assign->col_num, for_assign->source_index,
            "Error on line %d, type mismatch in \"for\" assignment statement!\n\n",
            for_assign->line_num);
        ++return_val;
    }

    if (!is_ordinal_type(type_first) && type_first != UNKNOWN_TYPE)
    {
        semcheck_error_with_context_at(for_assign->line_num, for_assign->col_num, for_assign->source_index,
            "Error on line %d, expected ordinal type in \"for\" assignment statement!\n\n",
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
