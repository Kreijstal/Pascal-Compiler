/*
    SemCheck_Expr_Utils.c - Shared expression helper functions

    This file contains utility helpers shared across SemCheck_Expr_* modules,
    including type coercion, char/pointer helpers, call cache utilities, and
    small debug helpers.

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"

int semcheck_is_currency_kgpc_type(KgpcType *type)
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

void semcheck_debug_expr_brief(const struct Expression *expr, const char *label)
{
    if (expr == NULL)
    {
        fprintf(stderr, "[SemCheck]   %s: <null>\n", label);
        return;
    }

    fprintf(stderr, "[SemCheck]   %s: expr_type=%d line=%d col=%d",
        label, expr->type, expr->line_num, expr->col_num);

    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
    {
        fprintf(stderr, " id=%s", expr->expr_data.id);
    }
    else if (expr->type == EXPR_FUNCTION_CALL &&
        expr->expr_data.function_call_data.id != NULL)
    {
        fprintf(stderr, " call=%s", expr->expr_data.function_call_data.id);
    }
    else if (expr->type == EXPR_RECORD_ACCESS &&
        expr->expr_data.record_access_data.field_id != NULL)
    {
        fprintf(stderr, " field=%s", expr->expr_data.record_access_data.field_id);
    }
    else if (expr->type == EXPR_ARRAY_ACCESS &&
        expr->expr_data.array_access_data.array_expr != NULL &&
        expr->expr_data.array_access_data.array_expr->type == EXPR_VAR_ID &&
        expr->expr_data.array_access_data.array_expr->expr_data.id != NULL)
    {
        fprintf(stderr, " array=%s", expr->expr_data.array_access_data.array_expr->expr_data.id);
    }
    fprintf(stderr, "\n");
}

const char *get_expr_type_name(struct Expression *expr, SymTab_t *symtab)
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

char *semcheck_mangle_helper_const_id(const char *helper_type_id, const char *field_id)
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

int is_type_ir(int *type)
{
    assert(type != NULL);
    return (is_integer_type(*type) || *type == REAL_TYPE);
}

int types_numeric_compatible(int lhs, int rhs)
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

void semcheck_coerce_char_string_operands(int *type_first, struct Expression *expr1,
    int *type_second, struct Expression *expr2)
{
    if (type_first == NULL || type_second == NULL)
        return;

    if (expr1 != NULL && semcheck_expr_is_char_like(expr1) && *type_first != CHAR_TYPE)
        *type_first = CHAR_TYPE;
    if (expr2 != NULL && semcheck_expr_is_char_like(expr2) && *type_second != CHAR_TYPE)
        *type_second = CHAR_TYPE;

    int expr1_is_char_ptr = semcheck_expr_is_char_pointer(expr1);
    int expr2_is_char_ptr = semcheck_expr_is_char_pointer(expr2);
    if ((*type_first == STRING_TYPE && expr2_is_char_ptr) ||
        (*type_second == STRING_TYPE && expr1_is_char_ptr))
    {
        if (*type_first == STRING_TYPE && expr2_is_char_ptr)
        {
            *type_second = STRING_TYPE;
            semcheck_promote_pointer_expr_to_string(expr2);
        }
        else if (*type_second == STRING_TYPE && expr1_is_char_ptr)
        {
            *type_first = STRING_TYPE;
            semcheck_promote_pointer_expr_to_string(expr1);
        }
    }

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

int semcheck_expr_is_char_like(struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    if (expr->type == EXPR_STRING && expr->expr_data.string != NULL &&
        strlen(expr->expr_data.string) == 1)
        return 1;
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

int semcheck_expr_is_char_pointer(struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    if (expr->resolved_type != POINTER_TYPE &&
        (expr->resolved_kgpc_type == NULL || !kgpc_type_is_pointer(expr->resolved_kgpc_type)))
        return 0;
    if (expr->pointer_subtype == CHAR_TYPE)
        return 1;
    if (expr->pointer_subtype_id != NULL)
    {
        const char *normalized = semcheck_normalize_char_type_id(expr->pointer_subtype_id);
        if (normalized != NULL &&
            (pascal_identifier_equals(normalized, "AnsiChar") ||
             pascal_identifier_equals(normalized, "WideChar")))
            return 1;
    }
    if (expr->resolved_kgpc_type != NULL && kgpc_type_is_pointer(expr->resolved_kgpc_type))
    {
        KgpcType *pointee = expr->resolved_kgpc_type->info.points_to;
        if (pointee != NULL &&
            pointee->kind == TYPE_KIND_PRIMITIVE &&
            pointee->info.primitive_type_tag == CHAR_TYPE)
            return 1;
    }
    return 0;
}

int semcheck_expr_is_wide_char_pointer(struct Expression *expr)
{
    if (!semcheck_expr_is_char_pointer(expr))
        return 0;
    if (expr->pointer_subtype_id != NULL)
    {
        const char *normalized = semcheck_normalize_char_type_id(expr->pointer_subtype_id);
        if (normalized != NULL && pascal_identifier_equals(normalized, "WideChar"))
            return 1;
    }
    if (expr->resolved_kgpc_type != NULL && kgpc_type_is_pointer(expr->resolved_kgpc_type))
    {
        KgpcType *pointee = expr->resolved_kgpc_type->info.points_to;
        if (pointee != NULL && kgpc_type_sizeof(pointee) == 2)
            return 1;
    }
    return 0;
}

void semcheck_promote_pointer_expr_to_string(struct Expression *expr)
{
    if (expr == NULL)
        return;
    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    expr->resolved_type = STRING_TYPE;
}

int is_and_or(int *type)
{
    assert(type != NULL);
    return (*type == AND || *type == OR);
}

void semcheck_expr_set_call_kgpc_type(struct Expression *expr, KgpcType *type,
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

void semcheck_expr_set_resolved_kgpc_type_shared(struct Expression *expr, KgpcType *type)
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

void semcheck_reset_function_call_cache(struct Expression *expr)
{
    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return;

    int had_call_info = (expr->expr_data.function_call_data.is_call_info_valid == 1);
    expr->expr_data.function_call_data.resolved_func = NULL;
    expr->expr_data.function_call_data.call_hash_type = HASHTYPE_VAR;
    semcheck_expr_set_call_kgpc_type(expr, NULL, had_call_info);
    expr->expr_data.function_call_data.is_call_info_valid = 0;
}

void semcheck_set_function_call_target(struct Expression *expr, HashNode_t *target)
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
