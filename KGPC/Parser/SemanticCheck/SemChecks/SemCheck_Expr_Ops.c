/*
    SemCheck_Expr_Ops.c - Expression operator semantic checks

    This file contains semantic checking for expression operators:
    - Relational operators (=, <>, <, >, <=, >=, in)
    - Sign terms (+, -, not)
    - Addition operators (+, -, or, xor)
    - Multiplication operators (*, /, div, mod, and, shl, shr)
    - Variable identifier resolution

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"

static int semcheck_expr_is_char_array_like(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    KgpcType *kgpc = expr->resolved_kgpc_type;
    if (kgpc == NULL)
        return 0;
    if (kgpc_type_is_shortstring(kgpc))
        return 1;
    if (kgpc->kind != TYPE_KIND_ARRAY)
        return 0;
    if (kgpc->info.array_info.element_type == NULL)
        return 0;
    if (kgpc->info.array_info.element_type->kind != TYPE_KIND_PRIMITIVE)
        return 0;
    return kgpc->info.array_info.element_type->info.primitive_type_tag == CHAR_TYPE;
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

    KgpcType *kgpc_type_first = NULL;
    KgpcType *kgpc_type_second = NULL;
    return_val += semcheck_expr_with_type(&kgpc_type_first, symtab, expr1, max_scope_lev, mutating);
    type_first = semcheck_tag_from_kgpc(kgpc_type_first);
    if(expr2 != NULL)
    {
        return_val += semcheck_expr_with_type(&kgpc_type_second, symtab, expr2, max_scope_lev, mutating);
        type_second = semcheck_tag_from_kgpc(kgpc_type_second);
    }

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
        semcheck_error_with_context("Error on line %d, expected relational type after \"NOT\"!\n\n",
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
                    semcheck_expr_set_resolved_type(expr1, CHAR_TYPE);
                }

                if (type_second != SET_TYPE)
                {
                    semcheck_error_with_context("Error on line %d, expected set operand on right side of IN expression!\n\n",
                        expr->line_num);
                    ++return_val;
                }
                if (!is_integer_type(type_first) && type_first != ENUM_TYPE &&
                    type_first != CHAR_TYPE && type_first != BOOL)
                {
                    semcheck_error_with_context("Error on line %d, expected integer operand on left side of IN expression!\n\n",
                        expr->line_num);
                    ++return_val;
                }
            }
            else if (relop_type == EQ || relop_type == NE)
            {
                /* Check for operator overloading for record types first */
                if ((type_first == RECORD_TYPE && type_second == RECORD_TYPE) ||
                    (type_first == RECORD_TYPE && type_second == POINTER_TYPE))
                {
                    struct Expression *record_expr = expr1;
                    struct Expression *other_expr = expr2;
                    const char *record_type_name = NULL;
                    const char *right_type_name = NULL;

                    if (type_first == RECORD_TYPE && type_second == RECORD_TYPE)
                    {
                        record_type_name = get_expr_type_name(expr1, symtab);
                        right_type_name = get_expr_type_name(expr2, symtab);
                        if (record_type_name == NULL || right_type_name == NULL ||
                            strcasecmp(record_type_name, right_type_name) != 0)
                            goto relop_fallback;
                    }
                    else
                    {
                        record_type_name = get_expr_type_name(record_expr, symtab);
                        if (record_type_name == NULL)
                            goto relop_fallback;
                    }

                    if (record_type_name != NULL)
                    {
                        const char *op_suffix = (relop_type == EQ) ? "op_eq" : "op_ne";
                        size_t name_len = strlen(record_type_name) + strlen(op_suffix) + 3;
                        char *operator_method = (char *)malloc(name_len);
                        
                        if (operator_method != NULL)
                        {
                            snprintf(operator_method, name_len, "%s__%s", record_type_name, op_suffix);
                            
                            HashNode_t *operator_node = NULL;
                            if (FindIdent(&operator_node, symtab, operator_method) == 0 && operator_node != NULL)
                            {
                                if (operator_node->type != NULL && kgpc_type_is_procedure(operator_node->type))
                                {
                                    KgpcType *return_type = kgpc_type_get_return_type(operator_node->type);
                                    if (return_type != NULL)
                                    {
                                        /* Transform expression from RELOP to FUNCTION_CALL */
                                        expr->type = EXPR_FUNCTION_CALL;
                                        memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));

                                        expr->expr_data.function_call_data.id = strdup(operator_method);
                                        /* Use the actual mangled name from the symbol table */
                                        if (operator_node->mangled_id != NULL)
                                            expr->expr_data.function_call_data.mangled_id = strdup(operator_node->mangled_id);
                                        else
                                            expr->expr_data.function_call_data.mangled_id = strdup(operator_method);
                                        
                                        ListNode_t *arg1 = CreateListNode(record_expr, LIST_EXPR);
                                        ListNode_t *arg2 = CreateListNode(other_expr, LIST_EXPR);
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
relop_fallback:
                
                semcheck_coerce_char_string_operands(&type_first, expr1, &type_second, expr2);

                int numeric_ok = types_numeric_compatible(type_first, type_second) &&
                                 is_type_ir(&type_first) && is_type_ir(&type_second);
                int boolean_ok = (type_first == BOOL && type_second == BOOL);
                int string_ok = (is_string_type(type_first) && is_string_type(type_second));
                if (!string_ok && expr1 != NULL && expr2 != NULL)
                {
                    int left_char_array = semcheck_expr_is_char_array_like(expr1);
                    int right_char_array = semcheck_expr_is_char_array_like(expr2);
                    if ((is_string_type(type_first) || left_char_array) &&
                        (is_string_type(type_second) || right_char_array))
                    {
                        string_ok = 1;
                    }
                }
                int char_ok = (type_first == CHAR_TYPE && type_second == CHAR_TYPE);
                /* Allow comparison of pointers AND procedures */
                int pointer_ok = ((type_first == POINTER_TYPE || type_first == PROCEDURE) && 
                                  (type_second == POINTER_TYPE || type_second == PROCEDURE));
                if (!pointer_ok && expr1 != NULL && expr2 != NULL)
                {
                    KgpcType *kgpc1 = expr1->resolved_kgpc_type;
                    KgpcType *kgpc2 = expr2->resolved_kgpc_type;
                    if (kgpc1 != NULL && kgpc2 != NULL)
                    {
                        int tag1 = semcheck_tag_from_kgpc(kgpc1);
                        int tag2 = semcheck_tag_from_kgpc(kgpc2);
                        if ((tag1 == POINTER_TYPE || tag1 == PROCEDURE) &&
                            (tag2 == POINTER_TYPE || tag2 == PROCEDURE))
                        {
                            pointer_ok = 1;
                        }
                    }
                }
                int enum_ok = (type_first == ENUM_TYPE && type_second == ENUM_TYPE);
                
                /* Check for string/PChar comparison.
                 * In Pascal, comparing AnsiString with PAnsiChar is valid.
                 * Use type tags first (string vs pointer), then refine with KgpcType if available. */
                int string_pchar_ok = 0;
                if (!string_ok && expr1 != NULL && expr2 != NULL)
                {
                    /* First try using type tags - if one is string and other is pointer,
                     * this is likely a string/PChar comparison */
                    int tag_string1 = is_string_type(type_first);
                    int tag_string2 = is_string_type(type_second);
                    int tag_pointer1 = (type_first == POINTER_TYPE);
                    int tag_pointer2 = (type_second == POINTER_TYPE);
                    
                    if ((tag_string1 && tag_pointer2) || (tag_pointer1 && tag_string2))
                    {
                        /* Likely string/PChar - allow it. 
                         * We could verify with KgpcType but it's not always available */
                        string_pchar_ok = 1;
                    }
                    
                    /* Also check using KgpcType for more accurate detection when available */
                    KgpcType *kgpc1 = expr1->resolved_kgpc_type;
                    KgpcType *kgpc2 = expr2->resolved_kgpc_type;
                    if (kgpc1 != NULL && kgpc2 != NULL)
                    {
                        /* Check if one is string and other is PChar (pointer to char) */
                        int is_pchar1 = (kgpc1->kind == TYPE_KIND_POINTER && kgpc1->info.points_to != NULL &&
                                         kgpc1->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
                                         kgpc1->info.points_to->info.primitive_type_tag == CHAR_TYPE);
                        int is_pchar2 = (kgpc2->kind == TYPE_KIND_POINTER && kgpc2->info.points_to != NULL &&
                                         kgpc2->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
                                         kgpc2->info.points_to->info.primitive_type_tag == CHAR_TYPE);
                        int is_string1 = (kgpc1->kind == TYPE_KIND_PRIMITIVE && 
                                          (kgpc1->info.primitive_type_tag == STRING_TYPE ||
                                           kgpc1->info.primitive_type_tag == SHORTSTRING_TYPE));
                        int is_string2 = (kgpc2->kind == TYPE_KIND_PRIMITIVE && 
                                          (kgpc2->info.primitive_type_tag == STRING_TYPE ||
                                           kgpc2->info.primitive_type_tag == SHORTSTRING_TYPE));
                        
                        if ((is_pchar1 && is_string2) || (is_string1 && is_pchar2))
                            string_pchar_ok = 1;
                        /* Also allow string to string comparison via KgpcType */
                        if (is_string1 && is_string2)
                            string_ok = 1;
                    }
                }
                
                /* Also check using KgpcType for enum compatibility (handles scoped enums) */
                if (!enum_ok && expr1 != NULL && expr2 != NULL)
                {
                    KgpcType *kgpc1 = expr1->resolved_kgpc_type;
                    KgpcType *kgpc2 = expr2->resolved_kgpc_type;
                    if (kgpc1 != NULL && kgpc2 != NULL)
                    {
                        struct TypeAlias *alias1 = kgpc_type_get_type_alias(kgpc1);
                        struct TypeAlias *alias2 = kgpc_type_get_type_alias(kgpc2);
                        if ((alias1 != NULL && alias1->is_enum) ||
                            (alias2 != NULL && alias2->is_enum))
                        {
                            /* At least one side is an enum - allow comparison */
                            enum_ok = 1;
                        }
                    }
                }

                /* Check for dynamic array compared with nil.
                 * In Pascal, dynamic arrays can be compared with nil to check if they are empty/uninitialized.
                 * A := nil; if A = nil then ... */
                int dynarray_nil_ok = 0;
                if (!pointer_ok && expr1 != NULL && expr2 != NULL)
                {
                    KgpcType *kgpc1 = expr1->resolved_kgpc_type;
                    KgpcType *kgpc2 = expr2->resolved_kgpc_type;
                    if (kgpc1 != NULL && kgpc2 != NULL)
                    {
                        int is_dynarray1 = kgpc_type_is_dynamic_array(kgpc1);
                        int is_dynarray2 = kgpc_type_is_dynamic_array(kgpc2);
                        int is_nil1 = (kgpc1->kind == TYPE_KIND_POINTER && kgpc1->info.points_to == NULL);
                        int is_nil2 = (kgpc2->kind == TYPE_KIND_POINTER && kgpc2->info.points_to == NULL);
                        
                        if ((is_dynarray1 && is_nil2) || (is_nil1 && is_dynarray2))
                            dynarray_nil_ok = 1;
                    }
                }
                
                if (!numeric_ok && !boolean_ok && !string_ok && !char_ok && !pointer_ok && !enum_ok && !string_pchar_ok && !dynarray_nil_ok)
                {
                    semcheck_error_with_context("Error on line %d, equality comparison requires matching numeric, boolean, string, character, or pointer types!\n\n",
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
                if (!string_ok && expr1 != NULL && expr2 != NULL)
                {
                    int left_char_array = semcheck_expr_is_char_array_like(expr1);
                    int right_char_array = semcheck_expr_is_char_array_like(expr2);
                    if ((is_string_type(type_first) || left_char_array) &&
                        (is_string_type(type_second) || right_char_array))
                    {
                        string_ok = 1;
                    }
                }
                int char_ok = (type_first == CHAR_TYPE && type_second == CHAR_TYPE);
                /* Allow comparison of pointers AND procedures */
                int pointer_ok = ((type_first == POINTER_TYPE || type_first == PROCEDURE) && 
                                  (type_second == POINTER_TYPE || type_second == PROCEDURE));
                if (!pointer_ok && expr1 != NULL && expr2 != NULL)
                {
                    KgpcType *kgpc1 = expr1->resolved_kgpc_type;
                    KgpcType *kgpc2 = expr2->resolved_kgpc_type;
                    if (kgpc1 != NULL && kgpc2 != NULL)
                    {
                        int tag1 = semcheck_tag_from_kgpc(kgpc1);
                        int tag2 = semcheck_tag_from_kgpc(kgpc2);
                        if ((tag1 == POINTER_TYPE || tag1 == PROCEDURE) &&
                            (tag2 == POINTER_TYPE || tag2 == PROCEDURE))
                        {
                            pointer_ok = 1;
                        }
                    }
                }
                int enum_ok = (type_first == ENUM_TYPE && type_second == ENUM_TYPE);
                
                /* Check for string/PChar comparison.
                 * Use type tags first, then refine with KgpcType if available. */
                int string_pchar_ok = 0;
                if (!string_ok && expr1 != NULL && expr2 != NULL)
                {
                    /* First try using type tags */
                    int tag_string1 = is_string_type(type_first);
                    int tag_string2 = is_string_type(type_second);
                    int tag_pointer1 = (type_first == POINTER_TYPE);
                    int tag_pointer2 = (type_second == POINTER_TYPE);
                    
                    if ((tag_string1 && tag_pointer2) || (tag_pointer1 && tag_string2))
                        string_pchar_ok = 1;
                    
                    /* Also check using KgpcType when available */
                    KgpcType *kgpc1 = expr1->resolved_kgpc_type;
                    KgpcType *kgpc2 = expr2->resolved_kgpc_type;
                    if (kgpc1 != NULL && kgpc2 != NULL)
                    {
                        int is_pchar1 = (kgpc1->kind == TYPE_KIND_POINTER && kgpc1->info.points_to != NULL &&
                                         kgpc1->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
                                         kgpc1->info.points_to->info.primitive_type_tag == CHAR_TYPE);
                        int is_pchar2 = (kgpc2->kind == TYPE_KIND_POINTER && kgpc2->info.points_to != NULL &&
                                         kgpc2->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
                                         kgpc2->info.points_to->info.primitive_type_tag == CHAR_TYPE);
                        int is_string1 = (kgpc1->kind == TYPE_KIND_PRIMITIVE && 
                                          (kgpc1->info.primitive_type_tag == STRING_TYPE ||
                                           kgpc1->info.primitive_type_tag == SHORTSTRING_TYPE));
                        int is_string2 = (kgpc2->kind == TYPE_KIND_PRIMITIVE && 
                                          (kgpc2->info.primitive_type_tag == STRING_TYPE ||
                                           kgpc2->info.primitive_type_tag == SHORTSTRING_TYPE));
                        
                        if ((is_pchar1 && is_string2) || (is_string1 && is_pchar2))
                            string_pchar_ok = 1;
                        if (is_string1 && is_string2)
                            string_ok = 1;
                    }
                }

                /* Check for dynamic array compared with nil */
                int dynarray_nil_ok = 0;
                if (!pointer_ok && expr1 != NULL && expr2 != NULL)
                {
                    KgpcType *kgpc1 = expr1->resolved_kgpc_type;
                    KgpcType *kgpc2 = expr2->resolved_kgpc_type;
                    if (kgpc1 != NULL && kgpc2 != NULL)
                    {
                        int is_dynarray1 = kgpc_type_is_dynamic_array(kgpc1);
                        int is_dynarray2 = kgpc_type_is_dynamic_array(kgpc2);
                        int is_nil1 = (kgpc1->kind == TYPE_KIND_POINTER && kgpc1->info.points_to == NULL);
                        int is_nil2 = (kgpc2->kind == TYPE_KIND_POINTER && kgpc2->info.points_to == NULL);
                        
                        if ((is_dynarray1 && is_nil2) || (is_nil1 && is_dynarray2))
                            dynarray_nil_ok = 1;
                    }
                }

                if(!numeric_ok && !string_ok && !char_ok && !pointer_ok && !enum_ok && !string_pchar_ok && !dynarray_nil_ok)
                {
                    semcheck_error_with_context(
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

    KgpcType *sign_kgpc_type = NULL;
    return_val += semcheck_expr_with_type(&sign_kgpc_type, symtab, sign_expr, max_scope_lev, mutating);
    *type_return = semcheck_tag_from_kgpc(sign_kgpc_type);

    /* Checking types */
    if(!is_type_ir(type_return))
    {
        semcheck_error_with_context("Error on line %d, expected int or real after \"-\"!\n\n",
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

    KgpcType *kgpc_type_first = NULL;
    KgpcType *kgpc_type_second = NULL;
    return_val += semcheck_expr_with_type(&kgpc_type_first, symtab, expr1, max_scope_lev, mutating);
    return_val += semcheck_expr_with_type(&kgpc_type_second, symtab, expr2, max_scope_lev, mutating);
    type_first = semcheck_tag_from_kgpc(kgpc_type_first);
    type_second = semcheck_tag_from_kgpc(kgpc_type_second);

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
        semcheck_error_with_context("Error on line %d, expected boolean or integer operands for OR expression!\n\n",
            expr->line_num);
        if (getenv("KGPC_DEBUG_ANDOR") != NULL)
        {
            fprintf(stderr,
                "[SemCheck] OR mismatch at line %d: lhs=%s(%d) rhs=%s(%d)\n",
                expr->line_num,
                semcheck_type_tag_name(type_first), type_first,
                semcheck_type_tag_name(type_second), type_second);
            semcheck_debug_expr_brief(expr1, "OR lhs");
            semcheck_debug_expr_brief(expr2, "OR rhs");
        }
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
            semcheck_error_with_context("Error on line %d, unsupported set additive operator.\n\n",
                expr->line_num);
            ++return_val;
            *type_return = SET_TYPE;
        }
        return return_val;
    }

    if (op_type == PLUS)
    {
        if (is_string_type(type_first) && semcheck_expr_is_char_pointer(expr2))
        {
            semcheck_promote_pointer_expr_to_string(expr2);
            *type_return = STRING_TYPE;
            return return_val;
        }
        if (is_string_type(type_second) && semcheck_expr_is_char_pointer(expr1))
        {
            semcheck_promote_pointer_expr_to_string(expr1);
            *type_return = STRING_TYPE;
            return return_val;
        }

        int left_is_string_like = (is_string_type(type_first) || type_first == CHAR_TYPE ||
                                   semcheck_expr_is_char_array_like(expr1));
        int right_is_string_like = (is_string_type(type_second) || type_second == CHAR_TYPE ||
                                    semcheck_expr_is_char_array_like(expr2));

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
        int left_is_int = (type_first == INT_TYPE || type_first == LONGINT_TYPE || type_first == INT64_TYPE);
        int right_is_int = (type_second == INT_TYPE || type_second == LONGINT_TYPE || type_second == INT64_TYPE);

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
            semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
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
                                *type_return = semcheck_tag_from_kgpc(return_type);
                                
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
        if (getenv("KGPC_DEBUG_ADDOP") != NULL)
        {
            fprintf(stderr,
                "[SemCheck] addop mismatch at line %d: lhs=%s(%d) rhs=%s(%d)\n",
                expr->line_num,
                semcheck_type_tag_name(type_first), type_first,
                semcheck_type_tag_name(type_second), type_second);
            semcheck_debug_expr_brief(expr1, "ADD lhs");
            semcheck_debug_expr_brief(expr2, "ADD rhs");
        }
        ++return_val;
    }
    if(!is_type_ir(&type_first) || !is_type_ir(&type_second))
    {
        semantic_error(expr->line_num, expr->col_num, "expected int/real on both sides of addop");
        if (getenv("KGPC_DEBUG_ADDOP") != NULL)
        {
            fprintf(stderr,
                "[SemCheck] addop non-numeric at line %d: lhs=%s(%d) rhs=%s(%d)\n",
                expr->line_num,
                semcheck_type_tag_name(type_first), type_first,
                semcheck_type_tag_name(type_second), type_second);
            semcheck_debug_expr_brief(expr1, "ADD lhs");
            semcheck_debug_expr_brief(expr2, "ADD rhs");
        }
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

    KgpcType *kgpc_type_first = NULL;
    KgpcType *kgpc_type_second = NULL;
    return_val += semcheck_expr_with_type(&kgpc_type_first, symtab, expr1, max_scope_lev, mutating);
    return_val += semcheck_expr_with_type(&kgpc_type_second, symtab, expr2, max_scope_lev, mutating);
    type_first = semcheck_tag_from_kgpc(kgpc_type_first);
    type_second = semcheck_tag_from_kgpc(kgpc_type_second);

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
        semcheck_error_with_context("Error on line %d, expected boolean, integer, or set operands for %s expression!\n\n",
            expr->line_num, op_type == AND ? "AND" : "XOR");
        if (getenv("KGPC_DEBUG_ANDOR") != NULL)
        {
            fprintf(stderr,
                "[SemCheck] %s mismatch at line %d: lhs=%s(%d) rhs=%s(%d)\n",
                op_type == AND ? "AND" : "XOR",
                expr->line_num,
                semcheck_type_tag_name(type_first), type_first,
                semcheck_type_tag_name(type_second), type_second);
            semcheck_debug_expr_brief(expr1, "AND lhs");
            semcheck_debug_expr_brief(expr2, "AND rhs");
        }
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
            semcheck_error_with_context("Error on line %d, unsupported set multiplicative operator.\n\n",
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
                case POWER: op_suffix = "op_pow"; break;
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
                                *type_return = semcheck_tag_from_kgpc(return_type);
                                
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
        semcheck_error_with_context("Error on line %d, type mismatch on mulop!\n\n",
            expr->line_num);
        ++return_val;
    }
    if(!is_type_ir(&type_first) || !is_type_ir(&type_second))
    {
        semcheck_error_with_context("Error on line %d, expected int/real on both sides of mulop!\n\n",
            expr->line_num);
        ++return_val;
    }

    /* Handle DIV and MOD operators - integer division only */
    if (op_type == DIV || op_type == MOD)
    {
        if (type_first == REAL_TYPE || type_second == REAL_TYPE)
        {
            semcheck_error_with_context("Error on line %d, DIV and MOD operators require integer operands!\n\n",
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
static struct RecordType *semcheck_resolve_helper_self_record(SymTab_t *symtab,
    HashNode_t *self_node, struct RecordType *helper_self_record)
{
    struct RecordType *self_record = helper_self_record;
    if (self_record == NULL && self_node != NULL)
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
        if (self_type_name == NULL && self_node->type != NULL)
        {
            KgpcType *pointed = kgpc_type_is_pointer(self_node->type) ?
                self_node->type->info.points_to : NULL;
            if (pointed != NULL && pointed->type_alias != NULL &&
                pointed->type_alias->target_type_id != NULL)
            {
                self_type_name = pointed->type_alias->target_type_id;
            }
        }
        struct RecordType *helper_record = semcheck_lookup_type_helper(symtab,
            self_type_tag, self_type_name);
        if (helper_record != NULL)
        {
            self_record = helper_record;
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] varid fallback: Using type helper %s for Self\n",
                    helper_record->type_id ? helper_record->type_id : "(null)");
            }
        }
        if (self_record == NULL)
            self_record = helper_self_record;
    }
    return self_record;
}

static int semcheck_try_helper_member(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int mutating,
    HashNode_t *self_node, struct RecordType *self_record, const char *id)
{
    if (symtab == NULL || expr == NULL || id == NULL || self_record == NULL)
        return -1;

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

    struct RecordType *property_owner = NULL;
    struct ClassProperty *property = semcheck_find_class_property(symtab,
        self_record, id, &property_owner);
    if (property != NULL)
    {
        char *self_str = strdup("Self");
        if (self_str != NULL)
        {
            struct Expression *self_expr = mk_varid(expr->line_num, self_str);
            if (self_expr != NULL)
            {
                self_expr->record_type = self_record;
                if (self_node != NULL && self_node->type != NULL)
                {
                    self_expr->resolved_kgpc_type = self_node->type;
                    kgpc_type_retain(self_node->type);
                }

                char *saved_id = expr->expr_data.id;
                expr->expr_data.id = NULL;

                expr->type = EXPR_RECORD_ACCESS;
                memset(&expr->expr_data.record_access_data, 0,
                    sizeof(expr->expr_data.record_access_data));

                expr->expr_data.record_access_data.record_expr = self_expr;
                expr->expr_data.record_access_data.field_id = saved_id;

                return semcheck_recordaccess(type_return, symtab, expr,
                    max_scope_lev, mutating);
            }
        }
    }

    struct RecordType *field_owner = NULL;
    struct RecordField *field_desc = semcheck_find_class_field(symtab,
        self_record, id, &field_owner);
    if (field_desc != NULL)
    {
        char *self_str = strdup("Self");
        if (self_str != NULL)
        {
            struct Expression *self_expr = mk_varid(expr->line_num, self_str);
            if (self_expr != NULL)
            {
                self_expr->record_type = self_record;
                if (self_node != NULL && self_node->type != NULL)
                {
                    self_expr->resolved_kgpc_type = self_node->type;
                    kgpc_type_retain(self_node->type);
                }

                char *saved_id = expr->expr_data.id;
                expr->expr_data.id = NULL;

                expr->type = EXPR_RECORD_ACCESS;
                memset(&expr->expr_data.record_access_data, 0,
                    sizeof(expr->expr_data.record_access_data));

                expr->expr_data.record_access_data.record_expr = self_expr;
                expr->expr_data.record_access_data.field_id = saved_id;

                long long field_offset = 0;
                if (resolve_record_field(symtab, self_record, id,
                    NULL, &field_offset, 0, 1) == 0)
                {
                    expr->expr_data.record_access_data.field_offset = field_offset;
                }

                return semcheck_recordaccess(type_return, symtab, expr,
                    max_scope_lev, mutating);
            }
        }
    }

    return -1;
}

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

    scope_return = FindIdent(&hash_return, symtab, id);
    if (scope_return == -1 && id != NULL)
    {
        const char *owner = semcheck_get_current_method_owner();
        if (owner != NULL)
        {
            char mangled[256];
            snprintf(mangled, sizeof(mangled), "%s__%s", owner, id);
            HashNode_t *class_const = NULL;
            int class_const_scope = FindIdent(&class_const, symtab, mangled);
            if (getenv("KGPC_DEBUG_CLASS_CONST") != NULL)
            {
                fprintf(stderr, "[KGPC] class const lookup %s -> scope=%d node=%p\n",
                    mangled, class_const_scope, (void*)class_const);
            }
            if (class_const_scope >= 0 && class_const != NULL &&
                class_const->hash_type == HASHTYPE_CONST)
            {
                if (expr->expr_data.id != NULL)
                    free(expr->expr_data.id);
                expr->expr_data.id = strdup(mangled);
                id = expr->expr_data.id;
                hash_return = class_const;
                scope_return = class_const_scope;
            }
        }
        /* Qualified identifier resolution: split dotted identifiers like
         * THorzRectAlign.Left into prefix (type) + suffix (member).
         * This handles scoped enum values and unit-qualified constants
         * in contexts where the parser produces a single flat identifier
         * (e.g. case labels). */
        if (scope_return == -1)
        {
            const char *dot = strchr(id, '.');
            if (dot != NULL && dot[1] != '\0')
            {
                size_t prefix_len = (size_t)(dot - id);
                char *prefix = (char *)malloc(prefix_len + 1);
                const char *suffix = dot + 1;
                assert(prefix != NULL);
                memcpy(prefix, id, prefix_len);
                prefix[prefix_len] = '\0';

                HashNode_t *prefix_node = NULL;
                int prefix_scope = FindIdent(&prefix_node, symtab, prefix);
                if (prefix_scope >= 0 && prefix_node != NULL &&
                    prefix_node->hash_type == HASHTYPE_TYPE)
                {
                    struct TypeAlias *type_alias = get_type_alias_from_node(prefix_node);
                    if (type_alias != NULL && type_alias->is_enum &&
                        type_alias->enum_literals != NULL)
                    {
                        int ordinal = 0;
                        ListNode_t *literal_node = type_alias->enum_literals;
                        while (literal_node != NULL)
                        {
                            if (literal_node->cur != NULL)
                            {
                                char *literal_name = (char *)literal_node->cur;
                                if (strcasecmp(literal_name, suffix) == 0)
                                {
                                    free(prefix);
                                    expr->type = EXPR_INUM;
                                    expr->expr_data.i_num = ordinal;
                                    semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                                    if (type_alias->kgpc_type != NULL)
                                        semcheck_expr_set_resolved_kgpc_type_shared(expr, type_alias->kgpc_type);
                                    *type_return = ENUM_TYPE;
                                    return 0;
                                }
                            }
                            ++ordinal;
                            literal_node = literal_node->next;
                        }
                    }
                    /* Also check for class constants: Type.ConstName */
                    char mangled_qid[512];
                    snprintf(mangled_qid, sizeof(mangled_qid), "%s__%s", prefix, suffix);
                    HashNode_t *class_const_node = NULL;
                    int cc_scope = FindIdent(&class_const_node, symtab, mangled_qid);
                    if (cc_scope >= 0 && class_const_node != NULL &&
                        class_const_node->hash_type == HASHTYPE_CONST)
                    {
                        free(prefix);
                        if (expr->expr_data.id != NULL)
                            free(expr->expr_data.id);
                        expr->expr_data.id = strdup(mangled_qid);
                        id = expr->expr_data.id;
                        hash_return = class_const_node;
                        scope_return = cc_scope;
                        goto resolved;
                    }
                }
                else if (prefix_scope == -1)
                {
                    /* Prefix not found - might be a unit qualifier.
                     * Try looking up the suffix directly. */
                    HashNode_t *field_node = NULL;
                    if (FindIdent(&field_node, symtab, suffix) >= 0 && field_node != NULL)
                    {
                        if (field_node->hash_type == HASHTYPE_CONST)
                        {
                            free(prefix);
                            expr->type = EXPR_INUM;
                            expr->expr_data.i_num = field_node->const_int_value;
                            semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
                            if (field_node->type != NULL)
                                semcheck_expr_set_resolved_kgpc_type_shared(expr, field_node->type);
                            *type_return = LONGINT_TYPE;
                            return 0;
                        }
                        else if (field_node->hash_type == HASHTYPE_VAR ||
                                 field_node->hash_type == HASHTYPE_ARRAY)
                        {
                            free(prefix);
                            char *field_copy = strdup(suffix);
                            assert(field_copy != NULL);
                            if (expr->expr_data.id != NULL)
                                free(expr->expr_data.id);
                            expr->expr_data.id = field_copy;
                            id = expr->expr_data.id;
                            scope_return = FindIdent(&hash_return, symtab, id);
                            goto resolved;
                        }
                    }
                }
                free(prefix);
            }
        }
resolved:;
    }
    if (getenv("KGPC_DEBUG_RESULT") != NULL && id != NULL &&
        pascal_identifier_equals(id, "Result"))
    {
        fprintf(stderr,
            "[KGPC] semcheck_varid Result: scope_return=%d hash_return=%p hash_type=%d type=%s\n",
            scope_return,
            (void*)hash_return,
            hash_return != NULL ? hash_return->hash_type : -1,
            hash_return != NULL ? kgpc_type_to_string(hash_return->type) : "<null>");
    }
    if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && id != NULL &&
        pascal_identifier_equals(id, "Self"))
    {
        fprintf(stderr, "[KGPC] semcheck_varid: FindIdent Self scope_return=%d hash_return=%p kind=%d\n",
            scope_return, (void*)hash_return,
            hash_return && hash_return->type ? hash_return->type->kind : -1);
    }
    if (scope_return != -1 && with_status != 0 && id != NULL &&
        hash_return != NULL && hash_return->hash_type == HASHTYPE_FUNCTION)
    {
        HashNode_t *self_node = NULL;
        if (FindIdent(&self_node, symtab, "Self") == 0 && self_node != NULL)
        {
            struct RecordType *self_record = get_record_type_from_node(self_node);
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
                if (self_type_name == NULL && self_node->type != NULL)
                {
                    KgpcType *pointed = kgpc_type_is_pointer(self_node->type) ?
                        self_node->type->info.points_to : NULL;
                    if (pointed != NULL && pointed->type_alias != NULL &&
                        pointed->type_alias->target_type_id != NULL)
                    {
                        self_type_name = pointed->type_alias->target_type_id;
                    }
                }
                self_record = semcheck_lookup_type_helper(symtab, self_type_tag, self_type_name);
            }

            if (self_record != NULL)
            {
                struct RecordType *property_owner = NULL;
                struct ClassProperty *property = semcheck_find_class_property(symtab,
                    self_record, id, &property_owner);
                if (property != NULL)
                {
                    char *self_str = strdup("Self");
                    if (self_str != NULL)
                    {
                        struct Expression *self_expr = mk_varid(expr->line_num, self_str);
                        if (self_expr != NULL)
                        {
                            self_expr->record_type = self_record;
                            if (self_node->type != NULL)
                            {
                                self_expr->resolved_kgpc_type = self_node->type;
                                kgpc_type_retain(self_node->type);
                            }

                            char *saved_id = expr->expr_data.id;
                            expr->expr_data.id = NULL;

                            expr->type = EXPR_RECORD_ACCESS;
                            memset(&expr->expr_data.record_access_data, 0,
                                sizeof(expr->expr_data.record_access_data));

                            expr->expr_data.record_access_data.record_expr = self_expr;
                            expr->expr_data.record_access_data.field_id = saved_id;

                            return semcheck_recordaccess(type_return, symtab, expr,
                                max_scope_lev, mutating);
                        }
                    }
                }
            }
        }
    }
    HashNode_t *helper_self_node = NULL;
    struct RecordType *helper_self_record = NULL;
    int helper_context = 0;
    if (FindIdent(&helper_self_node, symtab, "Self") == 0 && helper_self_node != NULL)
    {
        helper_self_record = get_record_type_from_node(helper_self_node);
        if (helper_self_record == NULL)
        {
            helper_self_record = semcheck_resolve_helper_self_record(symtab,
                helper_self_node, helper_self_record);
        }
        if (helper_self_record != NULL && helper_self_record->is_type_helper)
            helper_context = 1;
    }

    if (scope_return > 0 && id != NULL && helper_self_node != NULL)
    {
        struct RecordType *self_record = helper_self_record;
        if (self_record == NULL)
        {
            self_record = semcheck_resolve_helper_self_record(symtab,
                helper_self_node, helper_self_record);
        }
        int helper_result = semcheck_try_helper_member(type_return, symtab, expr,
            max_scope_lev, mutating, helper_self_node, self_record, id);
        if (helper_result >= 0)
            return helper_result;
    }

    if (scope_return == -1)
    {
        if (scope_return == -1)
        {
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

            if ((with_status != 0 || helper_context) && id != NULL)
            {
                HashNode_t *self_node = helper_self_node;
                if (self_node != NULL)
                {
                    struct RecordType *self_record = semcheck_resolve_helper_self_record(symtab,
                        self_node, helper_self_record);
                    int helper_result = semcheck_try_helper_member(type_return, symtab, expr,
                        max_scope_lev, mutating, self_node, self_record, id);
                    if (helper_result >= 0)
                        return helper_result;
                    
                    /* Type helper string builtins: In type helpers for strings, 
                     * bare 'Length' should resolve to Length(Self).
                     * Check both when Self is a record type helper or when it's a primitive string type. */
                    int is_string_type_helper = 0;
                    if (self_record != NULL && self_record->is_type_helper &&
                        self_record->helper_base_type_id != NULL &&
                        (pascal_identifier_equals(self_record->helper_base_type_id, "AnsiString") ||
                         pascal_identifier_equals(self_record->helper_base_type_id, "String") ||
                         pascal_identifier_equals(self_record->helper_base_type_id, "ShortString") ||
                         pascal_identifier_equals(self_record->helper_base_type_id, "UnicodeString") ||
                         pascal_identifier_equals(self_record->helper_base_type_id, "WideString")))
                    {
                        is_string_type_helper = 1;
                    }
                    /* Also check when Self is a primitive string type - this happens when
                     * type helper passes the primitive directly as Self parameter */
                    if (!is_string_type_helper && self_node->type != NULL)
                    {
                        const char *current_owner = semcheck_get_current_method_owner();
                        if (current_owner != NULL)
                        {
                            struct RecordType *owner_record = semcheck_lookup_record_type(symtab, current_owner);
                            if (owner_record != NULL && owner_record->is_type_helper &&
                                owner_record->helper_base_type_id != NULL &&
                                (pascal_identifier_equals(owner_record->helper_base_type_id, "AnsiString") ||
                                 pascal_identifier_equals(owner_record->helper_base_type_id, "String") ||
                                 pascal_identifier_equals(owner_record->helper_base_type_id, "ShortString") ||
                                 pascal_identifier_equals(owner_record->helper_base_type_id, "UnicodeString") ||
                                 pascal_identifier_equals(owner_record->helper_base_type_id, "WideString")))
                            {
                                is_string_type_helper = 1;
                            }
                        }
                    }
                    
                    if (is_string_type_helper && pascal_identifier_equals(id, "Length"))
                    {
                        /* Transform 'Length' into 'Length(Self)' */
                        char *self_str = strdup("Self");
                        if (self_str != NULL)
                        {
                            struct Expression *self_expr = mk_varid(expr->line_num, self_str);
                            if (self_expr != NULL)
                            {
                                ListNode_t *args_list = CreateListNode(self_expr, LIST_EXPR);
                                if (args_list != NULL)
                                {
                                    char *saved_id = expr->expr_data.id;
                                    expr->expr_data.id = NULL;
                                    
                                    expr->type = EXPR_FUNCTION_CALL;
                                    memset(&expr->expr_data.function_call_data, 0,
                                        sizeof(expr->expr_data.function_call_data));
                                    expr->expr_data.function_call_data.id = saved_id;
                                    expr->expr_data.function_call_data.args_expr = args_list;
                                    expr->expr_data.function_call_data.mangled_id = NULL;
                                    semcheck_reset_function_call_cache(expr);
                                    return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                                }
                                destroy_expr(self_expr);
                            }
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
                semantic_error_at(expr->line_num, expr->col_num, expr->source_index, "undeclared identifier \"%s\"", id);
                ++return_val;
            }

            *type_return = UNKNOWN_TYPE;
        }
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
            /* Prefer helper properties when inside a type helper method body. */
            if (id != NULL)
            {
                HashNode_t *self_node = NULL;
                if (FindIdent(&self_node, symtab, "Self") == 0 && self_node != NULL)
                {
                    struct RecordType *self_record = get_record_type_from_node(self_node);
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
                        if (self_type_name == NULL && self_node->type != NULL)
                        {
                            KgpcType *pointed = kgpc_type_is_pointer(self_node->type) ?
                                self_node->type->info.points_to : NULL;
                            if (pointed != NULL && pointed->type_alias != NULL &&
                                pointed->type_alias->target_type_id != NULL)
                            {
                                self_type_name = pointed->type_alias->target_type_id;
                            }
                        }
                        self_record = semcheck_lookup_type_helper(symtab, self_type_tag, self_type_name);
                    }

                    if (self_record == NULL)
                    {
                        const char *current_owner = semcheck_get_current_method_owner();
                        if (current_owner != NULL)
                        {
                            struct RecordType *owner_record = semcheck_lookup_record_type(symtab, current_owner);
                            if (owner_record != NULL && owner_record->is_type_helper)
                                self_record = owner_record;
                        }
                    }

                    if (self_record != NULL && self_record->is_type_helper)
                    {
                        struct RecordType *property_owner = NULL;
                        struct ClassProperty *property = semcheck_find_class_property(symtab,
                            self_record, id, &property_owner);
                        if (property != NULL)
                        {
                            char *self_str = strdup("Self");
                            if (self_str != NULL)
                            {
                                struct Expression *self_expr = mk_varid(expr->line_num, self_str);
                                if (self_expr != NULL)
                                {
                                    self_expr->record_type = self_record;
                                    if (self_node->type != NULL)
                                    {
                                        self_expr->resolved_kgpc_type = self_node->type;
                                        kgpc_type_retain(self_node->type);
                                    }

                                    char *saved_id = expr->expr_data.id;
                                    expr->expr_data.id = NULL;

                                    expr->type = EXPR_RECORD_ACCESS;
                                    memset(&expr->expr_data.record_access_data, 0,
                                        sizeof(expr->expr_data.record_access_data));

                                    expr->expr_data.record_access_data.record_expr = self_expr;
                                    expr->expr_data.record_access_data.field_id = saved_id;

                                    return semcheck_recordaccess(type_return, symtab, expr,
                                        max_scope_lev, mutating);
                                }
                            }
                        }
                        else
                        {
                            size_t getter_len = strlen(id) + 4;
                            char *getter_name = (char *)malloc(getter_len);
                            if (getter_name != NULL)
                            {
                                snprintf(getter_name, getter_len, "Get%s", id);
                                HashNode_t *getter_node = semcheck_find_class_method(symtab,
                                    self_record, getter_name, NULL);
                                if (getter_node != NULL)
                                {
                                    char *self_str = strdup("Self");
                                    if (self_str != NULL)
                                    {
                                        struct Expression *self_expr = mk_varid(expr->line_num, self_str);
                                        if (self_expr != NULL)
                                        {
                                            ListNode_t *args_list = CreateListNode(self_expr, LIST_EXPR);
                                            if (args_list != NULL)
                                            {
                                                expr->type = EXPR_FUNCTION_CALL;
                                                memset(&expr->expr_data.function_call_data, 0,
                                                    sizeof(expr->expr_data.function_call_data));
                                                expr->expr_data.function_call_data.id = getter_name;
                                                expr->expr_data.function_call_data.args_expr = args_list;
                                                expr->expr_data.function_call_data.mangled_id = NULL;
                                                semcheck_reset_function_call_cache(expr);
                                                return semcheck_funccall(type_return, symtab, expr,
                                                    max_scope_lev, mutating);
                                            }
                                            destroy_expr(self_expr);
                                        }
                                    }
                                    free(getter_name);
                                }
                                else
                                {
                                    free(getter_name);
                                }
                            }
                        }
                    }
                }
            }
            char *func_id = expr->expr_data.id;
            /* Set to NULL to transfer ownership to function_call_data.id and avoid double-free */
            expr->expr_data.id = NULL;

            expr->type = EXPR_FUNCTION_CALL;
            memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
            expr->expr_data.function_call_data.id = func_id;
            expr->expr_data.function_call_data.args_expr = NULL;
            expr->expr_data.function_call_data.mangled_id = NULL;
            semcheck_reset_function_call_cache(expr);
            
            /* Type helper string builtins: In type helpers for strings, 
             * bare 'Length' should resolve to Length(Self) */
            if (pascal_identifier_equals(func_id, "Length"))
            {
                const char *current_owner = semcheck_get_current_method_owner();
                if (current_owner != NULL)
                {
                    struct RecordType *owner_record = semcheck_lookup_record_type(symtab, current_owner);
                    if (owner_record != NULL && owner_record->is_type_helper &&
                        owner_record->helper_base_type_id != NULL &&
                        (pascal_identifier_equals(owner_record->helper_base_type_id, "AnsiString") ||
                         pascal_identifier_equals(owner_record->helper_base_type_id, "String") ||
                         pascal_identifier_equals(owner_record->helper_base_type_id, "ShortString") ||
                         pascal_identifier_equals(owner_record->helper_base_type_id, "UnicodeString") ||
                         pascal_identifier_equals(owner_record->helper_base_type_id, "WideString")))
                    {
                        /* Add Self as the argument */
                        char *self_str = strdup("Self");
                        if (self_str != NULL)
                        {
                            struct Expression *self_expr = mk_varid(expr->line_num, self_str);
                            if (self_expr != NULL)
                            {
                                ListNode_t *args_list = CreateListNode(self_expr, LIST_EXPR);
                                if (args_list != NULL)
                                {
                                    expr->expr_data.function_call_data.args_expr = args_list;
                                }
                                else
                                {
                                    destroy_expr(self_expr);
                                }
                            }
                        }
                    }
                }
            }
            
            return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
        }
        
        /* Don't convert procedure identifiers to function calls when used as values
         * This allows procedure variables to work correctly */
        if(hash_return->hash_type == HASHTYPE_PROCEDURE && mutating == NO_MUTATE)
        {
            /* Keep as EXPR_VAR_ID so it can be used as a procedure value */
            set_hash_meta(hash_return, mutating);
            set_type_from_hashtype(type_return, hash_return);
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
            {
                fprintf(stderr,
                    "[SemCheck] semcheck_varid: proc value id=%s type=%p kind=%d\n",
                    id ? id : "<null>",
                    (void*)hash_return->type,
                    hash_return->type != NULL ? hash_return->type->kind : -1);
            }
            if (hash_return->type != NULL)
                semcheck_expr_set_resolved_kgpc_type_shared(expr, hash_return->type);
            return 0;
        }

        /* Interface type identifiers can be used as GUIDs (FPC behavior). */
        if (hash_return->hash_type == HASHTYPE_TYPE && mutating == NO_MUTATE)
        {
            struct RecordType *type_record = get_record_type_from_node(hash_return);
            if (type_record != NULL && type_record->is_interface)
            {
                HashNode_t *tguid_node = semcheck_find_type_node_with_kgpc_type(symtab, "TGUID");
                if (tguid_node != NULL && tguid_node->type != NULL)
                {
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, tguid_node->type);
                    *type_return = semcheck_tag_from_kgpc(tguid_node->type);
                    return 0;
                }
            }
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
                semcheck_error_with_context("Error on line %d, cannot change \"%s\", invalid scope!\n",
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
                semcheck_error_with_context("Error on line %d, cannot assign \"%s\", is not a scalar variable!\n\n",
                    expr->line_num, id);
                ++return_val;
            }
        }
        set_type_from_hashtype(type_return, hash_return);
        if (node_is_array)
            semcheck_set_array_info_from_hashnode(expr, symtab, hash_return, expr->line_num);
        else
            semcheck_clear_array_info(expr);
        int force_shortstring = 0;
        struct TypeAlias *node_alias = get_type_alias_from_node(hash_return);
        if (node_alias != NULL && node_alias->is_shortstring)
            force_shortstring = 1;
        if (!force_shortstring && hash_return->type != NULL &&
            hash_return->type->type_alias != NULL &&
            hash_return->type->type_alias->is_shortstring)
        {
            force_shortstring = 1;
        }

        if (force_shortstring && !node_is_array)
        {
            KgpcType *short_type = create_primitive_type(SHORTSTRING_TYPE);
            semcheck_expr_set_resolved_kgpc_type_shared(expr, short_type);
            destroy_kgpc_type(short_type);
        }
        else
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, effective_type);
        }

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

            if (subtype == UNKNOWN_TYPE && type_id == NULL && alias != NULL &&
                alias->alias_name != NULL &&
                (alias->alias_name[0] == 'P' || alias->alias_name[0] == 'p') &&
                alias->alias_name[1] != '\0')
            {
                type_id = alias->alias_name + 1;
                subtype = semcheck_map_builtin_type_name(symtab, type_id);
                if (subtype == UNKNOWN_TYPE)
                    subtype = POINTER_TYPE;
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
