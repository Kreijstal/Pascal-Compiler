/*
    SemCheck_Expr_Builtins.c - Builtin function semantic checking

    This file contains the semantic checking for Pascal builtin functions
    like Chr, Ord, Length, Copy, Abs, Sin, Cos, Random, etc.

    Separated from SemCheck_expr.c for modularity.
*/

#include "SemCheck_Expr_Internal.h"

static char *build_qualified_identifier_from_expr_local(struct Expression *expr)
{
    if (expr == NULL)
        return NULL;
    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
        return strdup(expr->expr_data.id);
    if (expr->type != EXPR_RECORD_ACCESS)
        return NULL;

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    char *field_id = expr->expr_data.record_access_data.field_id;
    if (record_expr == NULL || field_id == NULL)
        return NULL;

    char *base = build_qualified_identifier_from_expr_local(record_expr);
    if (base == NULL)
        return NULL;
    size_t qualified_len = strlen(base) + 1 + strlen(field_id) + 1;
    char *qualified = (char *)malloc(qualified_len);
    if (qualified != NULL)
        snprintf(qualified, qualified_len, "%s.%s", base, field_id);
    free(base);
    return qualified;
}

/*===========================================================================
 * String/Character Builtins
 *===========================================================================*/

/* Detect whether an expression should be treated as a ShortString.
 * This mirrors the logic in SemCheck_stmt.c so helpers and statements
 * agree on ShortString handling. */
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
        if (kgpc_type_is_shortstring(expr->resolved_kgpc_type))
            return 1;
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

int semcheck_builtin_chr(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Chr expects exactly one argument.\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    KgpcType *arg_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr,
        max_scope_lev, NO_MUTATE);
    if (error_count == 0 && !kgpc_type_is_integer(arg_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, Chr expects an integer argument.\\n",
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
        semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_builtin_ord(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Ord expects exactly one argument.\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    KgpcType *arg_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr,
        max_scope_lev, NO_MUTATE);
    if (error_count != 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    const char *mangled_name = NULL;
    if (kgpc_type_is_string(arg_kgpc_type))
    {
        if (arg_expr != NULL && arg_expr->type == EXPR_STRING)
        {
            char *literal = arg_expr->expr_data.string;
            if (literal == NULL || literal[0] == '\0')
            {
                semcheck_error_with_context("Error on line %d, Ord expects a non-empty character literal.\\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            if (literal[1] != '\0')
            {
                semcheck_error_with_context("Error on line %d, Ord expects a single character literal.\\n",
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
            semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
            *type_return = LONGINT_TYPE;
            return 0;
        }

        mangled_name = "kgpc_ord_string";
    }
    else if (kgpc_type_is_boolean(arg_kgpc_type))
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
            semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
            *type_return = LONGINT_TYPE;
            return 0;
        }

        mangled_name = "kgpc_ord_longint";
    }
    else if (kgpc_type_is_integer(arg_kgpc_type))
    {
        mangled_name = "kgpc_ord_longint";
    }
    else if (kgpc_type_is_char(arg_kgpc_type))
    {
        /* For char variables, Ord returns the character code */
        mangled_name = "kgpc_ord_longint";
    }
    else if (kgpc_type_equals_tag(arg_kgpc_type, ENUM_TYPE))
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
        semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
        return 0;
    }

    semcheck_error_with_context("Error on line %d, Ord expects an integer, character, or boolean argument.\\n",
        expr->line_num);
    *type_return = UNKNOWN_TYPE;
    return 1;
}

int semcheck_builtin_length(int *type_return, SymTab_t *symtab,
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
        semcheck_error_with_context("Error on line %d, Length expects exactly one argument.\n", expr->line_num);
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
            semcheck_error_with_context("Error on line %d, Length expects exactly one argument.\n", expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    KgpcType *arg_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);


    int is_dynamic_array = (arg_expr != NULL && arg_expr->is_array_expr && arg_expr->array_is_dynamic);
    if (!is_dynamic_array && arg_expr != NULL && arg_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_dynamic_array(arg_expr->resolved_kgpc_type))
    {
        is_dynamic_array = 1;
    }
    if (!is_dynamic_array && arg_expr != NULL && arg_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_array(arg_expr->resolved_kgpc_type))
    {
        struct TypeAlias *alias = kgpc_type_get_type_alias(arg_expr->resolved_kgpc_type);
        if (alias != NULL && alias->is_open_array)
            is_dynamic_array = 1;
    }
    int is_static_array = (arg_expr != NULL && arg_expr->is_array_expr && !arg_expr->array_is_dynamic);
    int is_shortstring = semcheck_expr_is_shortstring(arg_expr);

    /* For static arrays, convert Length() to a compile-time constant */
    if (error_count == 0 && is_static_array && !is_shortstring)
    {
        long long length = arg_expr->array_upper_bound - arg_expr->array_lower_bound + 1;
        
        /* Convert this function call into an integer literal expression */
        semcheck_reset_function_call_cache(expr);
        expr->type = EXPR_INUM;
        expr->expr_data.i_num = length;
        semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
        *type_return = LONGINT_TYPE;
        return 0;
    }


    const char *mangled_name = NULL;
    int is_char_pointer = semcheck_expr_is_char_pointer(arg_expr);
    int is_wide_char_pointer = 0;
    if (is_char_pointer)
        is_wide_char_pointer = semcheck_expr_is_wide_char_pointer(arg_expr);
    if (error_count == 0 && is_dynamic_array)
        mangled_name = "__kgpc_dynarray_length";
    else if (error_count == 0 && (kgpc_type_is_string(arg_kgpc_type) || is_shortstring))
        mangled_name = is_shortstring ? "kgpc_shortstring_length" : "kgpc_string_length";
    else if (error_count == 0 && is_char_pointer)
        mangled_name = is_wide_char_pointer ? "kgpc_widechar_length" : "kgpc_string_length";
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
        semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
        *type_return = LONGINT_TYPE;
        return 0;
    }
    else if (error_count == 0)
    {
        if (getenv("KGPC_DEBUG_LENGTH") != NULL)
        {
            semcheck_debug_expr_brief(arg_expr, "Length arg");
            fprintf(stderr,
                "[KGPC_DEBUG_LENGTH] tag=%d kgpc=%s is_array=%d dyn=%d short=%d\n",
                semcheck_tag_from_kgpc(arg_kgpc_type),
                arg_expr->resolved_kgpc_type ? kgpc_type_to_string(arg_expr->resolved_kgpc_type) : "<null>",
                is_static_array,
                is_dynamic_array,
                is_shortstring);
        }
        semcheck_error_with_context("Error on line %d, Length supports string or dynamic array arguments.\n", expr->line_num);
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
        semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
        *type_return = LONGINT_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_builtin_copy(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    int arg_count = ListLength(args);

    /* Copy accepts 2 or 3 arguments:
       Copy(S, Index) - copies from Index to end of string
       Copy(S, Index, Count) - copies Count characters starting from Index */
    if (arg_count < 2 || arg_count > 3)
    {
        semcheck_error_with_context("Error on line %d, Copy expects two or three arguments.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *source_expr = (struct Expression *)args->cur;
    struct Expression *index_expr = (struct Expression *)args->next->cur;
    struct Expression *count_expr = NULL;

    int error_count = 0;
    KgpcType *source_kgpc_type = NULL;
    error_count += semcheck_expr_with_type(&source_kgpc_type, symtab, source_expr, max_scope_lev, NO_MUTATE);

    /* Check for ShortString (array[0..N] of char) like Length does */
    int is_shortstring = semcheck_expr_is_shortstring(source_expr);

    if (error_count == 0 && !kgpc_type_is_string(source_kgpc_type) && !is_shortstring)
    {
        semcheck_error_with_context("Error on line %d, Copy expects its first argument to be a string.\n", expr->line_num);
        error_count++;
    }

    KgpcType *index_kgpc_type = NULL;
    error_count += semcheck_expr_with_type(&index_kgpc_type, symtab, index_expr, max_scope_lev, NO_MUTATE);
    if (error_count == 0 && !kgpc_type_is_integer(index_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, Copy index must be an integer.\n", expr->line_num);
        error_count++;
    }

    if (arg_count == 3)
    {
        count_expr = (struct Expression *)args->next->next->cur;
        KgpcType *count_kgpc_type = NULL;
        error_count += semcheck_expr_with_type(&count_kgpc_type, symtab, count_expr, max_scope_lev, NO_MUTATE);
        if (error_count == 0 && !kgpc_type_is_integer(count_kgpc_type))
        {
            semcheck_error_with_context("Error on line %d, Copy count must be an integer.\n", expr->line_num);
            error_count++;
        }
    }
    else
    {
        /* 2-argument form: synthesize a large count value to copy to end of string.
           Using INT_MAX as runtime clips to available length. */
        count_expr = mk_inum(expr->line_num, (long long)INT_MAX);
        assert(count_expr != NULL);
        semcheck_expr_set_resolved_type(count_expr, LONGINT_TYPE);
        ListNode_t *count_node = CreateListNode(count_expr, LIST_EXPR);
        assert(count_node != NULL);
        args->next->next = count_node;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        /* Use ShortString-specific copy if source is a ShortString */
        const char *copy_func = is_shortstring ? "kgpc_shortstring_copy" : "kgpc_string_copy";
        expr->expr_data.function_call_data.mangled_id = strdup(copy_func);
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Copy.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        semcheck_expr_set_resolved_type(expr, STRING_TYPE);
        *type_return = STRING_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_builtin_concat(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    int arg_count = ListLength(args);

    if (arg_count < 2)
    {
        semcheck_error_with_context("Error on line %d, Concat expects at least two arguments.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    /* Validate all arguments are string or char compatible */
    int error_count = 0;
    ListNode_t *cur = args;
    while (cur != NULL)
    {
        struct Expression *arg = (struct Expression *)cur->cur;
        KgpcType *arg_type = NULL;
        error_count += semcheck_expr_with_type(&arg_type, symtab, arg, max_scope_lev, NO_MUTATE);
        if (error_count == 0 && arg_type != NULL &&
            !kgpc_type_is_string(arg_type) && !kgpc_type_is_char(arg_type) &&
            !semcheck_expr_is_shortstring(arg))
        {
            semcheck_error_with_context("Error on line %d, Concat arguments must be string or char.\n", expr->line_num);
            error_count++;
        }
        cur = cur->next;
    }

    if (error_count > 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    /* Transform Concat(a, b, c, ...) into nested addop: ((a + b) + c) + ...
     * This reuses the existing string concatenation code path. */
    cur = args;
    struct Expression *result = (struct Expression *)cur->cur;
    cur = cur->next;
    while (cur != NULL)
    {
        struct Expression *next_arg = (struct Expression *)cur->cur;
        result = mk_addop(expr->line_num, PLUS, result, next_arg);
        semcheck_expr_set_resolved_type(result, STRING_TYPE);
        cur = cur->next;
    }

    /* Replace the function call expression with the addop tree in-place.
     * Copy the addop data into the original expression node. */
    expr->type = result->type;
    expr->expr_data = result->expr_data;
    expr->resolved_kgpc_type = result->resolved_kgpc_type;
    semcheck_expr_set_resolved_type(expr, STRING_TYPE);

    /* Free only the outermost wrapper node (inner nodes are reused) */
    free(result);

    *type_return = STRING_TYPE;
    return 0;
}

int semcheck_builtin_pos(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL ||
        (args->next->next != NULL && args->next->next->next != NULL))
    {
        semcheck_error_with_context("Error on line %d, Pos expects two or three arguments.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int error_count = 0;
    struct Expression *substr_expr = (struct Expression *)args->cur;
    struct Expression *value_expr = (struct Expression *)args->next->cur;
    struct Expression *start_expr = NULL;
    int has_start = 0;
    if (args->next->next != NULL)
    {
        has_start = 1;
        start_expr = (struct Expression *)args->next->next->cur;
    }

    KgpcType *substr_kgpc_type = NULL;
    error_count += semcheck_expr_with_type(&substr_kgpc_type, symtab, substr_expr, max_scope_lev, NO_MUTATE);
    
    /* Check if substr is a string type, char, or shortstring (array of char) */
    int substr_is_char = kgpc_type_is_char(substr_kgpc_type);
    int is_valid_substr = kgpc_type_is_string(substr_kgpc_type) || substr_is_char ||
                          kgpc_type_is_shortstring(substr_kgpc_type) ||
                          is_shortstring_array(semcheck_tag_from_kgpc(substr_kgpc_type), substr_expr->is_array_expr);
    
    if (error_count == 0 && !is_valid_substr)
    {
        semcheck_error_with_context("Error on line %d, Pos substring must be a string.\n", expr->line_num);
        ++error_count;
    }

    KgpcType *value_kgpc_type = NULL;
    error_count += semcheck_expr_with_type(&value_kgpc_type, symtab, value_expr, max_scope_lev, NO_MUTATE);
    
    int value_is_char = kgpc_type_is_char(value_kgpc_type);
    /* Check if value is a string type OR a shortstring (array of char).
     * Allow char targets when the substring is also a string/shortstring/char. */
    int is_valid_value = kgpc_type_is_string(value_kgpc_type) || kgpc_type_is_shortstring(value_kgpc_type) ||
                         is_shortstring_array(semcheck_tag_from_kgpc(value_kgpc_type), value_expr->is_array_expr) ||
                         (value_is_char && (substr_is_char || kgpc_type_is_string(substr_kgpc_type) ||
                                            kgpc_type_is_shortstring(substr_kgpc_type) ||
                                            is_shortstring_array(semcheck_tag_from_kgpc(substr_kgpc_type),
                                                substr_expr->is_array_expr)));
    
    if (error_count == 0 && !is_valid_value)
    {
        semcheck_error_with_context("Error on line %d, Pos target must be a string.\n", expr->line_num);
        ++error_count;
    }
    if (error_count == 0 && has_start)
    {
        KgpcType *start_kgpc_type = NULL;
        error_count += semcheck_expr_with_type(&start_kgpc_type, symtab, start_expr, max_scope_lev, NO_MUTATE);
        if (error_count == 0 && !kgpc_type_is_integer(start_kgpc_type))
        {
            semcheck_error_with_context("Error on line %d, Pos start index must be an integer.\n",
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

        /* Determine which runtime overload to use based on ShortString layout */
        int substr_is_short = kgpc_type_is_shortstring(substr_kgpc_type) ||
                              is_shortstring_array(semcheck_tag_from_kgpc(substr_kgpc_type), substr_expr->is_array_expr);
        int value_is_short = kgpc_type_is_shortstring(value_kgpc_type) ||
                             is_shortstring_array(semcheck_tag_from_kgpc(value_kgpc_type), value_expr->is_array_expr);

        const char *mangled_name = has_start ? "kgpc_string_pos_from" : "kgpc_string_pos";
        if (substr_is_char)
        {
            if (value_is_char)
                mangled_name = has_start ? "kgpc_string_pos_cc_from" : "kgpc_string_pos_cc";
            else if (value_is_short)
                mangled_name = has_start ? "kgpc_string_pos_cs_from" : "kgpc_string_pos_cs";
            else
                mangled_name = has_start ? "kgpc_string_pos_ca_from" : "kgpc_string_pos_ca";
        }
        else if (value_is_char)
        {
            if (substr_is_short)
                mangled_name = has_start ? "kgpc_string_pos_sc_from" : "kgpc_string_pos_sc";
            else
                mangled_name = has_start ? "kgpc_string_pos_ac_from" : "kgpc_string_pos_ac";
        }
        else if (substr_is_short && value_is_short)
            mangled_name = has_start ? "kgpc_string_pos_ss_from" : "kgpc_string_pos_ss";
        else if (substr_is_short)
            mangled_name = has_start ? "kgpc_string_pos_sa_from" : "kgpc_string_pos_sa";
        else if (value_is_short)
            mangled_name = has_start ? "kgpc_string_pos_as_from" : "kgpc_string_pos_as";

        expr->expr_data.function_call_data.mangled_id = strdup(mangled_name);
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Pos.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
        *type_return = LONGINT_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_builtin_strpas(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    int arg_count = ListLength(args);
    if (args == NULL || (arg_count != 1 && arg_count != 2))
    {
        semcheck_error_with_context("Error on line %d, StrPas expects one or two arguments.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int error_count = 0;
    struct Expression *arg_expr = (struct Expression *)args->cur;
    KgpcType *arg_kgpc_type = NULL;
    error_count += semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    /* FPC accepts both PChar/PAnsiChar (string-like) pointers */
    int strpas_arg_ok = kgpc_type_is_string(arg_kgpc_type) ||
        kgpc_type_is_pointer(arg_kgpc_type) || kgpc_type_is_char(arg_kgpc_type);
    if (!strpas_arg_ok && arg_kgpc_type != NULL && kgpc_type_is_array(arg_kgpc_type))
    {
        KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(arg_kgpc_type, symtab);
        if (elem_type != NULL && kgpc_type_is_char(elem_type))
            strpas_arg_ok = 1;
        else if (arg_kgpc_type->type_alias != NULL &&
            arg_kgpc_type->type_alias->array_element_type == CHAR_TYPE)
            strpas_arg_ok = 1;
        else if (arg_kgpc_type->type_alias != NULL &&
            arg_kgpc_type->type_alias->array_element_type_id != NULL &&
            (pascal_identifier_equals(arg_kgpc_type->type_alias->array_element_type_id, "AnsiChar") ||
             pascal_identifier_equals(arg_kgpc_type->type_alias->array_element_type_id, "Char") ||
             pascal_identifier_equals(arg_kgpc_type->type_alias->array_element_type_id, "WideChar")))
            strpas_arg_ok = 1;
    }
    if (!strpas_arg_ok && arg_expr != NULL && arg_expr->is_array_expr)
    {
        if (arg_expr->array_element_type == CHAR_TYPE)
            strpas_arg_ok = 1;
        else if (arg_expr->array_element_type_id != NULL &&
            (pascal_identifier_equals(arg_expr->array_element_type_id, "AnsiChar") ||
             pascal_identifier_equals(arg_expr->array_element_type_id, "Char") ||
             pascal_identifier_equals(arg_expr->array_element_type_id, "WideChar")))
            strpas_arg_ok = 1;
    }
    /* Fallback: check resolved_kgpc_type tag (e.g. argv[l] indexing returns pointer) */
    if (!strpas_arg_ok && arg_expr != NULL && arg_expr->resolved_kgpc_type != NULL)
    {
        if (kgpc_type_equals_tag(arg_expr->resolved_kgpc_type, POINTER_TYPE) ||
            kgpc_type_equals_tag(arg_expr->resolved_kgpc_type, CHAR_TYPE) ||
            kgpc_type_equals_tag(arg_expr->resolved_kgpc_type, STRING_TYPE))
            strpas_arg_ok = 1;
    }
    if (error_count == 0 && !strpas_arg_ok)
    {
        semcheck_error_with_context("Error on line %d, StrPas expects a PChar or PAnsiChar argument.\n",
            expr->line_num);
        ++error_count;
    }

    if (error_count == 0 && arg_count == 2)
    {
        struct Expression *len_expr = (struct Expression *)args->next->cur;
        KgpcType *len_kgpc_type = NULL;
        error_count += semcheck_expr_with_type(&len_kgpc_type, symtab, len_expr, max_scope_lev, NO_MUTATE);
        if (error_count == 0 && !kgpc_type_is_integer(len_kgpc_type))
        {
            semcheck_error_with_context("Error on line %d, StrPas length must be an integer.\n",
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
        expr->expr_data.function_call_data.mangled_id = strdup(arg_count == 2 ? "kgpc_strpas_len" : "kgpc_strpas");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for StrPas.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        semcheck_expr_set_resolved_type(expr, STRING_TYPE);
        *type_return = STRING_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_builtin_eof(int *type_return, SymTab_t *symtab,
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
        KgpcType *file_kgpc_type = NULL;
        error_count += semcheck_expr_with_type(&file_kgpc_type, symtab, check_expr, max_scope_lev, NO_MUTATE);
        if (!kgpc_type_equals_tag(file_kgpc_type, TEXT_TYPE) &&
            !kgpc_type_equals_tag(file_kgpc_type, FILE_TYPE))
        {
            semcheck_error_with_context("Error on line %d, EOF expects a text file argument.\n", expr->line_num);
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
        semcheck_error_with_context("Error on line %d, EOF expects zero or one argument.\n", expr->line_num);
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
        semcheck_expr_set_resolved_type(expr, BOOL);
        *type_return = BOOL;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_builtin_eoln(int *type_return, SymTab_t *symtab,
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
        KgpcType *file_kgpc_type = NULL;
        error_count += semcheck_expr_with_type(&file_kgpc_type, symtab, check_expr, max_scope_lev, NO_MUTATE);
        if (!kgpc_type_equals_tag(file_kgpc_type, TEXT_TYPE))
        {
            semcheck_error_with_context("Error on line %d, EOLN expects a text file argument.\n", expr->line_num);
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
        semcheck_error_with_context("Error on line %d, EOLN expects zero or one argument.\n", expr->line_num);
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
    semcheck_expr_set_resolved_type(expr, BOOL);
    *type_return = BOOL;
    return 0;
}

void semcheck_free_call_args(ListNode_t *args, struct Expression *preserve_expr)
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

void semcheck_replace_call_with_integer_literal(struct Expression *expr, long long value)
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
    semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
}

int semcheck_prepare_dynarray_high_call(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, struct Expression *array_expr)
{
    if (expr == NULL || array_expr == NULL)
        return 1;

    ListNode_t *old_args = expr->expr_data.function_call_data.args_expr;
    semcheck_free_call_args(old_args, array_expr);
    expr->expr_data.function_call_data.args_expr = NULL;

    struct Expression *lower_expr = mk_inum(expr->line_num, array_expr->array_lower_bound);
    semcheck_expr_set_resolved_type(lower_expr, LONGINT_TYPE);

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
    expr->expr_data.function_call_data.arg0_is_dynarray_descriptor = 1;

    /* Note: We don't call semcheck_reset_function_call_cache here because we want to 
     * mark this as fully resolved (is_call_info_valid=1) rather than reset it */

    int error_count = 0;
    KgpcType *array_kgpc_type = NULL;
    error_count += semcheck_expr_with_type(&array_kgpc_type, symtab, array_expr, max_scope_lev, NO_MUTATE);
    KgpcType *lower_kgpc_type = NULL;
    error_count += semcheck_expr_with_type(&lower_kgpc_type, symtab, lower_expr, max_scope_lev, NO_MUTATE);
    (void)array_kgpc_type;
    (void)lower_kgpc_type;

    /* Mark as fully resolved internal runtime call */
    semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
    expr->expr_data.function_call_data.is_call_info_valid = 1;
    expr->expr_data.function_call_data.call_hash_type = HASHTYPE_FUNCTION;
    
    if (type_return != NULL)
        *type_return = LONGINT_TYPE;
    return error_count;
}

int semcheck_builtin_assigned(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Assigned expects exactly one argument.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    KgpcType *arg_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab,
        (struct Expression *)args->cur, max_scope_lev, NO_MUTATE);
    if (getenv("KGPC_DEBUG_ASSIGNED") != NULL)
    {
        struct Expression *arg_expr = (struct Expression *)args->cur;
        fprintf(stderr,
            "[KGPC_DEBUG_ASSIGNED] expr_type=%d kgpc=%s kind=%d\n",
            arg_expr != NULL ? arg_expr->type : -1,
            arg_kgpc_type != NULL ? kgpc_type_to_string(arg_kgpc_type) : "<null>",
            arg_kgpc_type != NULL ? arg_kgpc_type->kind : -1);
    }

    /* Assigned accepts pointers (TYPE_KIND_POINTER or POINTER_TYPE primitive) and procedure types */
    int is_valid_type = kgpc_type_is_pointer(arg_kgpc_type) || 
                        kgpc_type_is_procedure(arg_kgpc_type) ||
                        kgpc_type_equals_tag(arg_kgpc_type, POINTER_TYPE);
    if (error_count == 0 && !is_valid_type)
    {
        semcheck_error_with_context("Error on line %d, Assigned expects a pointer or procedure variable.\n", expr->line_num);
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
        semcheck_expr_set_resolved_type(expr, BOOL);
        *type_return = BOOL;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_builtin_abs(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Abs expects exactly one argument.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    KgpcType *arg_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    const char *mangled_name = NULL;
    int result_type = UNKNOWN_TYPE;
    if (error_count == 0)
    {
        int arg_type = semcheck_tag_from_kgpc(arg_kgpc_type);
        if (arg_type == REAL_TYPE)
        {
            mangled_name = "kgpc_abs_real";
            result_type = REAL_TYPE;
        }
        else if (arg_type == QWORD_TYPE || arg_type == LONGWORD_TYPE || 
                 arg_type == BYTE_TYPE || arg_type == WORD_TYPE)
        {
            /* Unsigned types: abs is a no-op, use identity helper */
            mangled_name = "kgpc_abs_unsigned";
            result_type = arg_type;
        }
        else if (arg_type == INT64_TYPE || arg_type == LONGINT_TYPE)
        {
            /* 64-bit and 32-bit signed long types use the 64-bit abs helper */
            mangled_name = "kgpc_abs_longint";
            result_type = arg_type;
        }
        else if (is_integer_type(arg_type))
        {
            /* Smaller signed integer types (INT_TYPE) use 32-bit abs */
            mangled_name = "kgpc_abs_int";
            result_type = arg_type;
        }
        else
        {
            semcheck_error_with_context("Error on line %d, Abs expects integer or real arguments.\n", expr->line_num);
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
        semcheck_expr_set_resolved_type(expr, result_type);
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

int semcheck_builtin_unary_real(int *type_return, SymTab_t *symtab,
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
        semcheck_error_with_context("Error on line %d, %s expects exactly one argument.\n",
            expr->line_num, display_name);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    KgpcType *arg_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    if (error_count == 0)
    {
        if (!kgpc_type_is_numeric(arg_kgpc_type))
        {
            semcheck_error_with_context("Error on line %d, %s expects a real argument.\n",
                expr->line_num, display_name);
            ++error_count;
        }
        else
        {
            int arg_type = semcheck_tag_from_kgpc(arg_kgpc_type);
            if (arg_type != REAL_TYPE)
            {
                char *real_id = strdup("Real");
                struct Expression *cast_expr = mk_typecast(expr->line_num, REAL_TYPE, real_id, arg_expr);
                if (cast_expr == NULL)
                {
                    fprintf(stderr, "Error: failed to allocate typecast for %s.\n", display_name);
                    ++error_count;
                }
                else
                {
                    args->cur = cast_expr;
                    arg_expr = cast_expr;
                    int cast_type = UNKNOWN_TYPE;
                    error_count += semcheck_typecast(&cast_type, symtab, cast_expr,
                        max_scope_lev, NO_MUTATE);
                }
            }
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
            fprintf(stderr, "Error: failed to allocate mangled name for %s.\n",
                display_name);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        semcheck_expr_set_resolved_type(expr, result_type);
        *type_return = result_type;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

/* Special Trunc handler that detects Currency type and uses appropriate scaling. */
int semcheck_builtin_trunc(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Trunc expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    KgpcType *arg_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    if (error_count == 0 && !kgpc_type_is_numeric(arg_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, Trunc expects a real argument.\n",
            expr->line_num);
        ++error_count;
    }

    if (error_count == 0)
    {
        const char *mangled = "kgpc_trunc";
        
        /* Check if argument is Currency type - if so, use currency-specific trunc */
        int is_currency = 0;
        if (arg_expr->resolved_kgpc_type != NULL &&
            semcheck_is_currency_kgpc_type(arg_expr->resolved_kgpc_type))
            is_currency = 1;
        if (!is_currency && arg_kgpc_type != NULL &&
            semcheck_is_currency_kgpc_type(arg_kgpc_type))
            is_currency = 1;
        if (!is_currency && arg_expr->type == EXPR_VAR_ID && arg_expr->expr_data.id != NULL)
        {
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, arg_expr->expr_data.id) == 0 &&
                node != NULL && node->type != NULL &&
                semcheck_is_currency_kgpc_type(node->type))
                is_currency = 1;
        }
        if (is_currency)
        {
            mangled = "kgpc_trunc_currency";
        }

        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup(mangled);
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Trunc.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
        *type_return = LONGINT_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_builtin_arctan2(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, ArcTan2 expects exactly two arguments.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *y_expr = (struct Expression *)args->cur;
    struct Expression *x_expr = (struct Expression *)args->next->cur;
    KgpcType *y_kgpc_type = NULL;
    KgpcType *x_kgpc_type = NULL;
    int error_count = 0;

    error_count += semcheck_expr_with_type(&y_kgpc_type, symtab, y_expr, max_scope_lev, NO_MUTATE);
    error_count += semcheck_expr_with_type(&x_kgpc_type, symtab, x_expr, max_scope_lev, NO_MUTATE);

    if (!kgpc_type_is_numeric(y_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, ArcTan2 expects numeric arguments.\n",
            expr->line_num);
        ++error_count;
    }
    if (!kgpc_type_is_numeric(x_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, ArcTan2 expects numeric arguments.\n",
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
    semcheck_expr_set_resolved_type(expr, REAL_TYPE);
    expr->resolved_kgpc_type = create_primitive_type(REAL_TYPE);
    *type_return = REAL_TYPE;
    return 0;
}

int semcheck_builtin_hypot(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Hypot expects exactly two arguments.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *x_expr = (struct Expression *)args->cur;
    struct Expression *y_expr = (struct Expression *)args->next->cur;
    KgpcType *x_kgpc_type = NULL;
    KgpcType *y_kgpc_type = NULL;
    int error_count = 0;

    error_count += semcheck_expr_with_type(&x_kgpc_type, symtab, x_expr, max_scope_lev, NO_MUTATE);
    error_count += semcheck_expr_with_type(&y_kgpc_type, symtab, y_expr, max_scope_lev, NO_MUTATE);

    if (!kgpc_type_is_numeric(x_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, Hypot expects numeric arguments.\n",
            expr->line_num);
        ++error_count;
    }
    if (!kgpc_type_is_numeric(y_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, Hypot expects numeric arguments.\n",
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
    semcheck_expr_set_resolved_type(expr, REAL_TYPE);
    expr->resolved_kgpc_type = create_primitive_type(REAL_TYPE);
    *type_return = REAL_TYPE;
    return 0;
}

int semcheck_builtin_logn(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, LogN expects exactly two arguments.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *base_expr = (struct Expression *)args->cur;
    struct Expression *value_expr = (struct Expression *)args->next->cur;
    KgpcType *base_kgpc_type = NULL;
    KgpcType *value_kgpc_type = NULL;
    int error_count = 0;

    error_count += semcheck_expr_with_type(&base_kgpc_type, symtab, base_expr, max_scope_lev, NO_MUTATE);
    error_count += semcheck_expr_with_type(&value_kgpc_type, symtab, value_expr, max_scope_lev, NO_MUTATE);

    if (!kgpc_type_is_numeric(base_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, LogN expects numeric arguments.\n",
            expr->line_num);
        ++error_count;
    }
    if (!kgpc_type_is_numeric(value_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, LogN expects numeric arguments.\n",
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
    semcheck_expr_set_resolved_type(expr, REAL_TYPE);
    expr->resolved_kgpc_type = create_primitive_type(REAL_TYPE);
    *type_return = REAL_TYPE;
    return 0;
}


int semcheck_builtin_upcase(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, UpCase expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    KgpcType *arg_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    if (error_count == 0 && !kgpc_type_is_char(arg_kgpc_type))
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
        }
        else
        {
            semcheck_error_with_context("Error on line %d, UpCase expects a char argument.\n",
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
        semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
        expr->resolved_kgpc_type = create_primitive_type(CHAR_TYPE);
        *type_return = CHAR_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_builtin_predsucc(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int is_succ)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, %s expects exactly one argument.\n",
            expr->line_num, is_succ ? "Succ" : "Pred");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    KgpcType *arg_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
    if (error_count == 0 && !kgpc_type_is_integer(arg_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, %s expects an integer argument.\n",
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
        semcheck_error_with_context("Error on line %d, failed to build %s expression.\n",
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

    /* Use legacy_tag here since this function's API returns int type tag via type_return.
     * The expression is rewritten to an ADDOP and needs to be re-checked. */
    return semcheck_expr_legacy_tag(type_return, symtab, expr, max_scope_lev, NO_MUTATE);
}

int semcheck_builtin_odd(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Odd expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    KgpcType *arg_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    if (error_count == 0)
    {
        int is_integer = kgpc_type_is_integer(arg_kgpc_type);
        if (!is_integer)
        {
            struct TypeAlias *alias = kgpc_type_get_type_alias(arg_kgpc_type);
            if (alias != NULL && alias->base_type != UNKNOWN_TYPE)
                is_integer = is_integer_type(alias->base_type);
        }
        if (!is_integer)
        {
            semcheck_error_with_context("Error on line %d, Odd expects an integer argument.\n",
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
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_is_odd");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Odd.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        semcheck_expr_set_resolved_type(expr, BOOL);
        *type_return = BOOL;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_builtin_sqr(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Sqr expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    KgpcType *arg_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    const char *mangled_name = NULL;
    int result_type = UNKNOWN_TYPE;

    if (error_count == 0)
    {
        int arg_type = semcheck_tag_from_kgpc(arg_kgpc_type);
        if (arg_type == UNKNOWN_TYPE)
        {
            struct TypeAlias *alias = kgpc_type_get_type_alias(arg_kgpc_type);
            if (alias != NULL && alias->base_type != UNKNOWN_TYPE)
                arg_type = alias->base_type;
        }
        if (arg_type == REAL_TYPE)
        {
            mangled_name = "kgpc_sqr_real";
            result_type = REAL_TYPE;
        }
        else if (is_64bit_integer_type(arg_type) || arg_type == LONGINT_TYPE || arg_type == LONGWORD_TYPE)
        {
            /* 64-bit and 32-bit long types use the 64-bit sqr helper */
            mangled_name = "kgpc_sqr_int64";
            result_type = arg_type;
        }
        else if (is_integer_type(arg_type))
        {
            /* Smaller integer types (INT_TYPE, BYTE_TYPE, WORD_TYPE) use 32-bit sqr */
            mangled_name = "kgpc_sqr_int32";
            result_type = arg_type;
        }
        else
        {
            semcheck_error_with_context("Error on line %d, Sqr expects integer or real arguments.\n",
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
        semcheck_expr_set_resolved_type(expr, result_type);
        *type_return = result_type;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}


/*===========================================================================
 * Type/Memory Builtins (Default, Low/High, SizeOf)
 *==========================================================================*/

int semcheck_builtin_default(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Default expects exactly one argument.\n",
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
        KgpcType *resolved_kgpc_type = NULL;
        error_count = semcheck_expr_with_type(&resolved_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
        if (error_count != 0)
        {
            *type_return = UNKNOWN_TYPE;
        if (target_kgpc_type != NULL)
                destroy_kgpc_type(target_kgpc_type);
            return error_count;
        }
        target_type = semcheck_tag_from_kgpc(resolved_kgpc_type);
        if (arg_expr != NULL && arg_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_record(arg_expr->resolved_kgpc_type))
            record_type = kgpc_type_get_record(arg_expr->resolved_kgpc_type);
        if (arg_expr != NULL && arg_expr->resolved_kgpc_type != NULL)
        {
            target_kgpc_type = arg_expr->resolved_kgpc_type;
            kgpc_type_retain(target_kgpc_type);
        }
    }

    if (target_type == UNKNOWN_TYPE && target_kgpc_type != NULL)
        target_type = semcheck_tag_from_kgpc(target_kgpc_type);

    if (target_type == UNKNOWN_TYPE)
    {
        semcheck_error_with_context("Error on line %d, Default requires a type identifier or typed expression.\n",
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
        semcheck_expr_set_resolved_type(expr, target_type);
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
            semcheck_expr_set_resolved_type(expr, target_type);
            expr->resolved_kgpc_type = create_primitive_type(target_type);
            *type_return = target_type;
            return 0;
        case REAL_TYPE:
            expr->type = EXPR_RNUM;
            expr->expr_data.r_num = 0.0;
            semcheck_expr_set_resolved_type(expr, REAL_TYPE);
            expr->resolved_kgpc_type = create_primitive_type(REAL_TYPE);
            *type_return = REAL_TYPE;
            return 0;
        case BOOL:
            expr->type = EXPR_BOOL;
            expr->expr_data.bool_value = 0;
            semcheck_expr_set_resolved_type(expr, BOOL);
            expr->resolved_kgpc_type = create_primitive_type(BOOL);
            *type_return = BOOL;
            return 0;
        case CHAR_TYPE:
            expr->type = EXPR_CHAR_CODE;
            expr->expr_data.char_code = 0;
            semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
            expr->resolved_kgpc_type = create_primitive_type(CHAR_TYPE);
            *type_return = CHAR_TYPE;
            return 0;
        case STRING_TYPE:
            expr->type = EXPR_STRING;
            expr->expr_data.string = strdup("");
            semcheck_expr_set_resolved_type(expr, STRING_TYPE);
            expr->resolved_kgpc_type = create_primitive_type(STRING_TYPE);
            *type_return = STRING_TYPE;
            return 0;
        case POINTER_TYPE:
            expr->type = EXPR_NIL;
            semcheck_expr_set_resolved_type(expr, POINTER_TYPE);
            expr->resolved_kgpc_type = create_primitive_type(POINTER_TYPE);
            *type_return = POINTER_TYPE;
            return 0;
        case ENUM_TYPE:
            expr->type = EXPR_INUM;
            expr->expr_data.i_num = 0;
            semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
            expr->resolved_kgpc_type = create_primitive_type(ENUM_TYPE);
            *type_return = ENUM_TYPE;
            return 0;
        default:
            semcheck_error_with_context("Error on line %d, Default for this type is unsupported in this context.\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
    }
}

int semcheck_builtin_typeinfo(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, TypeInfo expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    struct TypeAlias *alias = NULL;
    const char *type_name = NULL;
    char *qualified_name = NULL;
    char *type_name_owned = NULL;

    if (arg_expr != NULL && (arg_expr->type == EXPR_VAR_ID || arg_expr->type == EXPR_RECORD_ACCESS))
    {
        if (arg_expr->type == EXPR_VAR_ID)
            type_name = arg_expr->expr_data.id;
        else
        {
            qualified_name = build_qualified_identifier_from_expr_local(arg_expr);
            type_name = qualified_name;
        }

        if (type_name != NULL)
        {
            const char *raw_name = type_name;
            const char *base_name = semcheck_base_type_name(raw_name);
            HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, raw_name);
            if (type_node == NULL && base_name != NULL && base_name != raw_name)
                type_node = semcheck_find_preferred_type_node(symtab, base_name);
            if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
            {
                alias = get_type_alias_from_node(type_node);
                if (alias == NULL && type_node->type != NULL)
                    alias = kgpc_type_get_type_alias(type_node->type);
                if (alias != NULL && alias->alias_name != NULL)
                    type_name = alias->alias_name;
                else if (type_node->id != NULL)
                    type_name = type_node->id;
            }
        }
    }

    if (qualified_name != NULL)
    {
        if (type_name == qualified_name)
        {
            type_name_owned = strdup(qualified_name);
            type_name = type_name_owned;
        }
        free(qualified_name);
        qualified_name = NULL;
    }

    if (alias == NULL)
    {
        KgpcType *resolved_type = NULL;
        int error_count = semcheck_expr_with_type(&resolved_type, symtab, arg_expr,
            max_scope_lev, NO_MUTATE);
        if (error_count != 0)
        {
            if (type_name_owned != NULL)
                free(type_name_owned);
            *type_return = UNKNOWN_TYPE;
            return error_count;
        }
        if (resolved_type != NULL)
            alias = kgpc_type_get_type_alias(resolved_type);
        if (alias != NULL && alias->alias_name != NULL)
            type_name = alias->alias_name;
    }

    if (alias == NULL || !alias->is_enum || alias->enum_literals == NULL)
    {
        semcheck_error_with_context("Error on line %d, TypeInfo currently supports enum types only.\n",
            expr->line_num);
        if (type_name_owned != NULL)
            free(type_name_owned);
        *type_return = UNKNOWN_TYPE;
        return 1;
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

    expr->type = EXPR_TYPEINFO;
    if (expr->expr_data.typeinfo_data.type_id != NULL)
    {
        free(expr->expr_data.typeinfo_data.type_id);
        expr->expr_data.typeinfo_data.type_id = NULL;
    }
    if (type_name != NULL)
        expr->expr_data.typeinfo_data.type_id = strdup(type_name);
    else
        expr->expr_data.typeinfo_data.type_id = strdup("unknown");
    if (expr->expr_data.typeinfo_data.type_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate TypeInfo label string.\n");
        if (type_name_owned != NULL)
            free(type_name_owned);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    expr->resolved_kgpc_type = create_pointer_type(NULL);
    semcheck_expr_set_resolved_type(expr, POINTER_TYPE);
    *type_return = POINTER_TYPE;
    if (type_name_owned != NULL)
        free(type_name_owned);
    return 0;
}
int semcheck_builtin_lowhigh(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int is_high)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, %s expects exactly one argument.\n",
            expr->line_num, is_high ? "High" : "Low");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    if (arg_expr != NULL && (arg_expr->type == EXPR_VAR_ID || arg_expr->type == EXPR_RECORD_ACCESS))
    {
        char *qualified_name = NULL;
        const char *type_name = NULL;
        if (arg_expr->type == EXPR_VAR_ID)
            type_name = arg_expr->expr_data.id;
        else
        {
            qualified_name = build_qualified_identifier_from_expr_local(arg_expr);
            type_name = qualified_name;
        }
        const char *raw_name = type_name;
        const char *base_name = semcheck_base_type_name(raw_name);
        HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, raw_name);
        if (type_node == NULL && base_name != NULL && base_name != raw_name)
            type_node = semcheck_find_preferred_type_node(symtab, base_name);
        if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            struct TypeAlias *alias = get_type_alias_from_node(type_node);
            long long low = 0;
            long long high = 0;
            int have_bounds = 0;
            int result_type = INT_TYPE;

            if (alias != NULL && alias->is_enum && alias->enum_literals != NULL)
            {
                int count = ListLength(alias->enum_literals);
                if (count > 0)
                {
                    low = 0;
                    high = count - 1;
                    have_bounds = 1;
                }
            }
            if (!have_bounds && alias != NULL && alias->is_range && alias->range_known)
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
                    } else if (pascal_identifier_equals(target_name, "QWord") ||
                               pascal_identifier_equals(target_name, "UInt64")) {
                        low = 0;
                        high = (long long)0xFFFFFFFFFFFFFFFFULL;
                        have_bounds = 1;
                        result_type = INT64_TYPE;
                    }
                }
            }
            else if (base_name != NULL && pascal_identifier_equals(base_name, "Int64"))
            {
                low = (-9223372036854775807LL - 1);
                high = 9223372036854775807LL;
                have_bounds = 1;
                result_type = INT64_TYPE;
            }
            else if (base_name != NULL &&
                     (pascal_identifier_equals(base_name, "QWord") ||
                      pascal_identifier_equals(base_name, "UInt64")))
            {
                low = 0;
                high = (long long)0xFFFFFFFFFFFFFFFFULL;
                have_bounds = 1;
                result_type = INT64_TYPE;
            }
            else if (base_name != NULL &&
                     (pascal_identifier_equals(base_name, "NativeUInt") ||
                      pascal_identifier_equals(base_name, "SizeUInt") ||
                      pascal_identifier_equals(base_name, "PtrUInt")))
            {
                low = 0;
                high = (long long)0xFFFFFFFFFFFFFFFFULL;
                have_bounds = 1;
                result_type = INT64_TYPE;
            }
            else if (base_name != NULL &&
                     (pascal_identifier_equals(base_name, "NativeInt") ||
                      pascal_identifier_equals(base_name, "SizeInt") ||
                      pascal_identifier_equals(base_name, "PtrInt")))
            {
                low = (-9223372036854775807LL - 1);
                high = 9223372036854775807LL;
                have_bounds = 1;
                result_type = INT64_TYPE;
            }
            else if (base_name != NULL && pascal_identifier_equals(base_name, "LongInt"))
            {
                low = -2147483648LL;
                high = 2147483647LL;
                have_bounds = 1;
                result_type = LONGINT_TYPE;
            }
            else if (base_name != NULL && pascal_identifier_equals(base_name, "Integer"))
            {
                low = -2147483648LL;
                high = 2147483647LL;
                have_bounds = 1;
                result_type = INT_TYPE;
            }
            else if (base_name != NULL &&
                     (pascal_identifier_equals(base_name, "Cardinal") ||
                      pascal_identifier_equals(base_name, "LongWord") ||
                      pascal_identifier_equals(base_name, "DWord")))
            {
                low = 0;
                high = 4294967295LL;
                have_bounds = 1;
                result_type = INT64_TYPE;
            }
            else if (base_name != NULL && pascal_identifier_equals(base_name, "SmallInt"))
            {
                low = -32768LL;
                high = 32767LL;
                have_bounds = 1;
            }
            else if (base_name != NULL && pascal_identifier_equals(base_name, "Word"))
            {
                low = 0;
                high = 65535LL;
                have_bounds = 1;
            }
            else if (base_name != NULL && pascal_identifier_equals(base_name, "ShortInt"))
            {
                low = -128LL;
                high = 127LL;
                have_bounds = 1;
            }
            else if (base_name != NULL && pascal_identifier_equals(base_name, "Byte"))
            {
                low = 0;
                high = 255LL;
                have_bounds = 1;
            }
            else if (base_name != NULL && pascal_identifier_equals(base_name, "Boolean"))
            {
                low = 0;
                high = 1;
                have_bounds = 1;
                result_type = BOOL;
            }
            else if (base_name != NULL &&
                     (pascal_identifier_equals(base_name, "Char") ||
                      pascal_identifier_equals(base_name, "AnsiChar")))
            {
                low = 0;
                high = 255;
                have_bounds = 1;
                result_type = CHAR_TYPE;
            }

            if (have_bounds)
            {
                semcheck_replace_call_with_integer_literal(expr, is_high ? high : low);
                semcheck_expr_set_resolved_type(expr, result_type);
                if (qualified_name != NULL)
                    free(qualified_name);
                *type_return = result_type;
                return 0;
            }
        }
        if (qualified_name != NULL)
            free(qualified_name);
    }

    KgpcType *arg_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
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
                semcheck_error_with_context("Error on line %d, invalid array bounds for High().\n",
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

    if (kgpc_type_is_string(arg_kgpc_type) && !arg_expr->is_array_expr)
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

    /* Ordinal overloads - use type tag for detailed matching */
    int arg_type = semcheck_tag_from_kgpc(arg_kgpc_type);
    if (arg_type == INT_TYPE)
    {
        semcheck_replace_call_with_integer_literal(expr, is_high ? 2147483647LL : -2147483648LL);
        semcheck_expr_set_resolved_type(expr, INT_TYPE);
        *type_return = INT_TYPE;
        return 0;
    }
    if (arg_type == LONGINT_TYPE)
    {
        semcheck_replace_call_with_integer_literal(expr, is_high ? 2147483647LL : -2147483648LL);
        semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
        *type_return = LONGINT_TYPE;
        return 0;
    }
    if (arg_type == INT64_TYPE)
    {
        semcheck_replace_call_with_integer_literal(expr, is_high ? 9223372036854775807LL : (-9223372036854775807LL - 1));
        semcheck_expr_set_resolved_type(expr, INT64_TYPE);
        *type_return = INT64_TYPE;
        return 0;
    }
    if (arg_type == BYTE_TYPE)
    {
        semcheck_replace_call_with_integer_literal(expr, is_high ? 255LL : 0LL);
        semcheck_expr_set_resolved_type(expr, BYTE_TYPE);
        *type_return = BYTE_TYPE;
        return 0;
    }
    if (arg_type == WORD_TYPE)
    {
        semcheck_replace_call_with_integer_literal(expr, is_high ? 65535LL : 0LL);
        semcheck_expr_set_resolved_type(expr, WORD_TYPE);
        *type_return = WORD_TYPE;
        return 0;
    }
    if (arg_type == LONGWORD_TYPE)
    {
        semcheck_replace_call_with_integer_literal(expr, is_high ? 4294967295LL : 0LL);
        semcheck_expr_set_resolved_type(expr, LONGWORD_TYPE);
        *type_return = LONGWORD_TYPE;
        return 0;
    }
    if (arg_type == QWORD_TYPE)
    {
        semcheck_replace_call_with_integer_literal(expr, is_high ? 9223372036854775807LL : 0LL);
        semcheck_expr_set_resolved_type(expr, QWORD_TYPE);
        *type_return = QWORD_TYPE;
        return 0;
    }
    if (arg_type == BOOL)
    {
        semcheck_free_call_args(expr->expr_data.function_call_data.args_expr, NULL);
        expr->expr_data.function_call_data.args_expr = NULL;
        expr->type = EXPR_BOOL;
        expr->expr_data.bool_value = is_high ? 1 : 0;
        semcheck_expr_set_resolved_type(expr, BOOL);
        *type_return = BOOL;
        return 0;
    }
    if (arg_type == CHAR_TYPE)
    {
        semcheck_free_call_args(expr->expr_data.function_call_data.args_expr, NULL);
        expr->expr_data.function_call_data.args_expr = NULL;
        expr->type = EXPR_CHAR_CODE;
        expr->expr_data.char_code = (unsigned int)(is_high ? 255 : 0);
        semcheck_expr_set_resolved_type(expr, CHAR_TYPE);
        *type_return = CHAR_TYPE;
        return 0;
    }

    semcheck_error_with_context("Error on line %d, %s currently supports only array or string arguments.\n",
        expr->line_num, is_high ? "High" : "Low");
    *type_return = UNKNOWN_TYPE;
    return 1;
}

int semcheck_builtin_sizeof(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, SizeOf expects exactly one argument.\n",
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
                semcheck_error_with_context("Error on line %d, SizeOf references undeclared identifier %s.\n",
                    expr->line_num, arg_id);
                error_count++;
            }
        }
        else
        {
            if (node->hash_type != HASHTYPE_TYPE && scope > max_scope_lev)
            {
                semcheck_error_with_context("Error on line %d, SizeOf cannot access %s due to scope restrictions.\n",
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
                    semcheck_error_with_context("Error on line %d, SizeOf argument %s is not a data object.\n",
                        expr->line_num, arg_id);
                    error_count++;
                }
            }

            if (error_count == 0)
            {
                if (node->hash_type != HASHTYPE_TYPE && node->hash_type != HASHTYPE_ARRAY)
                {
                    KgpcType *dummy_kgpc_type = NULL;
                    error_count += semcheck_expr_with_type(&dummy_kgpc_type, symtab, arg,
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
                        alias->target_type_id != NULL &&
                        !alias->is_pointer && !alias->is_array && !alias->is_set &&
                        !alias->is_enum && !alias->is_file)
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
        if (arg != NULL && arg->resolved_kgpc_type != NULL)
        {
            long long size = kgpc_type_sizeof(arg->resolved_kgpc_type);
            if (size >= 0)
            {
                computed_size = size;
            }
            else
            {
                KgpcType *arg_kgpc_type = NULL;
                error_count += semcheck_expr_with_type(&arg_kgpc_type, symtab, arg, max_scope_lev, NO_MUTATE);
                if (error_count == 0)
                    error_count += sizeof_from_type_ref(symtab, semcheck_tag_from_kgpc(arg_kgpc_type), NULL, &computed_size,
                        0, expr->line_num);
            }
        }
        else
        {
            KgpcType *arg_kgpc_type = NULL;
            error_count += semcheck_expr_with_type(&arg_kgpc_type, symtab, arg, max_scope_lev, NO_MUTATE);
            if (error_count == 0)
                error_count += sizeof_from_type_ref(symtab, semcheck_tag_from_kgpc(arg_kgpc_type), NULL, &computed_size,
                    0, expr->line_num);
        }
    }

    if (error_count == 0)
    {
        if (computed_size < 0)
        {
            semcheck_error_with_context("Error on line %d, SizeOf produced an invalid result.\n",
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
        semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
        *type_return = LONGINT_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_builtin_ismanagedtype(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, IsManagedType expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    KgpcType *arg_kgpc_type = NULL;
    int error_count = 0;
    int is_managed = 0;

    if (arg_expr != NULL && arg_expr->type == EXPR_VAR_ID && arg_expr->expr_data.id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, arg_expr->expr_data.id) != -1 &&
            type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            arg_kgpc_type = type_node->type;
        }
        else
        {
            int builtin_tag = semcheck_map_builtin_type_name(symtab, arg_expr->expr_data.id);
            if (builtin_tag != UNKNOWN_TYPE)
            {
                arg_kgpc_type = create_primitive_type(builtin_tag);
            }
        }
    }
    else
    {
        error_count += semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
    }

    if (error_count != 0 || arg_kgpc_type == NULL)
    {
        *type_return = UNKNOWN_TYPE;
        return (error_count == 0) ? 1 : error_count;
    }

    int arg_tag = semcheck_tag_from_kgpc(arg_kgpc_type);
    if (arg_tag == STRING_TYPE || arg_tag == SHORTSTRING_TYPE || arg_tag == VARIANT_TYPE)
        is_managed = 1;
    if (kgpc_type_is_dynamic_array(arg_kgpc_type))
        is_managed = 1;

    semcheck_free_call_args(expr->expr_data.function_call_data.args_expr, NULL);
    expr->expr_data.function_call_data.args_expr = NULL;
    expr->type = EXPR_BOOL;
    expr->expr_data.bool_value = is_managed ? 1 : 0;
    semcheck_expr_set_resolved_type(expr, BOOL);
    *type_return = BOOL;
    return 0;
}

/*===========================================================================
 * Random/Power Builtins
 *==========================================================================*/

int semcheck_builtin_random(int *type_return, SymTab_t *symtab,
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
        semcheck_expr_set_resolved_type(expr, REAL_TYPE);
        *type_return = REAL_TYPE;
        return 0;
    }

    if (args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Random expects zero or one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *upper_expr = (struct Expression *)args->cur;
    KgpcType *upper_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&upper_kgpc_type, symtab, upper_expr, max_scope_lev, NO_MUTATE);
    int upper_type = semcheck_tag_from_kgpc(upper_kgpc_type);
    int is_real_upper = (upper_type == REAL_TYPE);
    if (!is_real_upper && !kgpc_type_is_integer(upper_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, Random parameter must be numeric.\n",
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
    else if (upper_type == INT64_TYPE)
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_random_int64");
    else
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_random_int");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for Random.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    if (is_real_upper)
        semcheck_expr_set_resolved_type(expr, REAL_TYPE);
    else if (upper_type == INT64_TYPE)
        semcheck_expr_set_resolved_type(expr, INT64_TYPE);
    else if (upper_type == LONGINT_TYPE)
        semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
    else
        semcheck_expr_set_resolved_type(expr, INT_TYPE);
    semcheck_reset_function_call_cache(expr);
    *type_return = semcheck_tag_from_kgpc(expr->resolved_kgpc_type);
    return 0;
}

int semcheck_builtin_randomrange(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, RandomRange expects exactly two arguments.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *low_expr = (struct Expression *)args->cur;
    struct Expression *high_expr = (struct Expression *)args->next->cur;
    KgpcType *low_kgpc_type = NULL;
    KgpcType *high_kgpc_type = NULL;
    int error_count = 0;

    error_count += semcheck_expr_with_type(&low_kgpc_type, symtab, low_expr, max_scope_lev, NO_MUTATE);
    error_count += semcheck_expr_with_type(&high_kgpc_type, symtab, high_expr, max_scope_lev, NO_MUTATE);

    if (!kgpc_type_is_integer(low_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, RandomRange lower bound must be integer.\n",
            expr->line_num);
        ++error_count;
    }
    if (!kgpc_type_is_integer(high_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, RandomRange upper bound must be integer.\n",
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

    int low_type = semcheck_tag_from_kgpc(low_kgpc_type);
    int high_type = semcheck_tag_from_kgpc(high_kgpc_type);
    int result_type = INT_TYPE;
    if (low_type == INT64_TYPE || high_type == INT64_TYPE)
        result_type = INT64_TYPE;
    else if (low_type == LONGINT_TYPE || high_type == LONGINT_TYPE)
        result_type = LONGINT_TYPE;
    semcheck_expr_set_resolved_type(expr, result_type);
    expr->resolved_kgpc_type = create_primitive_type(result_type);
    *type_return = result_type;
    return 0;
}

int semcheck_builtin_power(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Power expects exactly two arguments.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *base_expr = (struct Expression *)args->cur;
    struct Expression *exp_expr = (struct Expression *)args->next->cur;
    KgpcType *base_kgpc_type = NULL;
    KgpcType *exp_kgpc_type = NULL;
    int error_count = 0;

    error_count += semcheck_expr_with_type(&base_kgpc_type, symtab, base_expr, max_scope_lev, NO_MUTATE);
    error_count += semcheck_expr_with_type(&exp_kgpc_type, symtab, exp_expr, max_scope_lev, NO_MUTATE);

    if (!kgpc_type_is_numeric(base_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, Power base must be numeric.\n", expr->line_num);
        ++error_count;
    }
    if (!kgpc_type_is_numeric(exp_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, Power exponent must be numeric.\n",
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
    semcheck_expr_set_resolved_type(expr, REAL_TYPE);
    expr->resolved_kgpc_type = create_primitive_type(REAL_TYPE);
    *type_return = REAL_TYPE;
    return 0;
}

int semcheck_builtin_aligned(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    (void)symtab;

    assert(type_return != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args != NULL && args->next == NULL)
    {
        /* One-argument Aligned(Value) is an FPC intrinsic used to ensure alignment of
         * untyped parameters. Treat it as a no-op identity expression. */
        struct Expression *arg_expr = (struct Expression *)args->cur;
        if (arg_expr == NULL)
        {
            semcheck_error_with_context("Error on line %d, Aligned requires an argument.\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

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
        if (expr->expr_data.function_call_data.args_expr != NULL)
        {
            DestroyList(expr->expr_data.function_call_data.args_expr);
            expr->expr_data.function_call_data.args_expr = NULL;
        }
        if (expr->resolved_kgpc_type != NULL)
        {
            destroy_kgpc_type(expr->resolved_kgpc_type);
            expr->resolved_kgpc_type = NULL;
        }

        struct Expression tmp = *arg_expr;
        *expr = tmp;
        free(arg_expr);

        return semcheck_expr_legacy_tag(type_return, symtab, expr, max_scope_lev, NO_MUTATE);
    }

    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, Aligned expects exactly two arguments: pointer and alignment.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    /* First argument: pointer */
    struct Expression *ptr_arg = (struct Expression *)args->cur;
    KgpcType *ptr_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&ptr_kgpc_type, symtab, ptr_arg, max_scope_lev, NO_MUTATE);
    int is_valid_ptr = kgpc_type_is_pointer(ptr_kgpc_type) || kgpc_type_equals_tag(ptr_kgpc_type, POINTER_TYPE);
    if (error_count == 0 && !is_valid_ptr)
    {
        semcheck_error_with_context("Error on line %d, Aligned first argument must be a pointer.\n",
            expr->line_num);
        ++error_count;
    }

    /* Second argument: alignment (integer) */
    struct Expression *align_arg = (struct Expression *)args->next->cur;
    KgpcType *align_kgpc_type = NULL;
    error_count += semcheck_expr_with_type(&align_kgpc_type, symtab, align_arg, max_scope_lev, NO_MUTATE);
    if (error_count == 0 && !kgpc_type_is_integer(align_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, Aligned second argument must be an integer.\n",
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
        expr->expr_data.function_call_data.mangled_id = strdup("kgpc_aligned");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Aligned.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        semcheck_reset_function_call_cache(expr);
        if (expr->resolved_kgpc_type != NULL)
        {
            destroy_kgpc_type(expr->resolved_kgpc_type);
            expr->resolved_kgpc_type = NULL;
        }
        semcheck_expr_set_resolved_type(expr, BOOL);
        expr->resolved_kgpc_type = create_primitive_type(BOOL);
        *type_return = BOOL;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_builtin_allocmem(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        semcheck_error_with_context("Error on line %d, AllocMem expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *size_expr = (struct Expression *)args->cur;
    KgpcType *size_kgpc_type = NULL;
    int error_count = semcheck_expr_with_type(&size_kgpc_type, symtab, size_expr, max_scope_lev, NO_MUTATE);
    if (error_count != 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    if (!kgpc_type_is_integer(size_kgpc_type))
    {
        semcheck_error_with_context("Error on line %d, AllocMem size argument must be an integer.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    semcheck_reset_function_call_cache(expr);
    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
    expr->expr_data.function_call_data.mangled_id = strdup("kgpc_allocmem");
    if (expr->expr_data.function_call_data.mangled_id == NULL)
    {
        fprintf(stderr, "Error: failed to allocate mangled name for AllocMem.\n");
        *type_return = UNKNOWN_TYPE;
        return 1;
    }
    semcheck_expr_set_resolved_type(expr, POINTER_TYPE);
    expr->expr_data.function_call_data.is_call_info_valid = 1;
    *type_return = POINTER_TYPE;
    return 0;
}
