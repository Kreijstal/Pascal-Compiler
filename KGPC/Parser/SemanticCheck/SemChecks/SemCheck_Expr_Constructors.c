/*
    SemCheck_Expr_Constructors.c - Array/record literal helpers

    This file contains helpers for typechecking array literals,
    record constructors, and preparing arguments for open-array or
    record-typed parameters.

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"
#include <limits.h>

static int integer_literal_fits_type(long long value, int expected_type)
{
    switch (expected_type)
    {
        case BYTE_TYPE:
            return value >= 0 && value <= 255;
        case WORD_TYPE:
            return value >= 0 && value <= 65535;
        case LONGWORD_TYPE:
            return value >= 0 && value <= 4294967295LL;
        case QWORD_TYPE:
            return value >= 0;
        case INT_TYPE:
        case LONGINT_TYPE:
            return value >= INT_MIN && value <= INT_MAX;
        case INT64_TYPE:
            return value >= LLONG_MIN && value <= LLONG_MAX;
        default:
            return 0;
    }
}

int semcheck_convert_set_literal_to_array_literal(struct Expression *expr)
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
    semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
    return 0;
}

int semcheck_typecheck_array_literal(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, int expected_type, const char *expected_type_id, int line_num)
{
    if (expr == NULL || expr->type != EXPR_ARRAY_LITERAL)
        return 0;

    if (getenv("KGPC_DEBUG_ARRAY_LITERAL") != NULL)
    {
        fprintf(stderr,
            "[KGPC] typecheck array literal @ line %d: expected_type=%d expected_id=%s\n",
            line_num,
            expected_type,
            expected_type_id != NULL ? expected_type_id : "<null>");
    }

    if (expected_type == ARRAY_OF_CONST_TYPE)
    {
        ListNode_t *cur_elem = expr->expr_data.array_literal_data.elements;
        while (cur_elem != NULL)
        {
            struct Expression *element_expr = (struct Expression *)cur_elem->cur;
            KgpcType *element_kgpc_type = NULL;
            semcheck_expr_with_type(&element_kgpc_type, symtab, element_expr, max_scope_lev, NO_MUTATE);
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

        if (element_expr->type == EXPR_ARRAY_LITERAL)
        {
            if (expected_type != UNKNOWN_TYPE)
            {
                if (element_expr->array_element_type == UNKNOWN_TYPE)
                    element_expr->array_element_type = expected_type;
                if (element_expr->array_element_type_id == NULL && expected_type_id != NULL)
                    element_expr->array_element_type_id = strdup(expected_type_id);
            }
            int nested_err = semcheck_typecheck_array_literal(element_expr, symtab, max_scope_lev,
                expected_type, expected_type_id, line_num);
            if (nested_err != 0)
                error_count += nested_err;
            ++index;
            cur = cur->next;
            continue;
        }

        KgpcType *element_kgpc_type = NULL;
        error_count += semcheck_expr_with_type(&element_kgpc_type, symtab, element_expr,
            max_scope_lev, NO_MUTATE);
        int element_type = semcheck_tag_from_kgpc(element_kgpc_type);

        if (error_count == 0 && expected_type != UNKNOWN_TYPE &&
            element_type != expected_type)
        {
            int compatible = 0;
            if (is_integer_type(expected_type) && is_integer_type(element_type))
            {
                if (element_expr->type == EXPR_INUM)
                {
                    if (integer_literal_fits_type(element_expr->expr_data.i_num, expected_type))
                        compatible = 1;
                }
                else
                {
                    compatible = 1;
                }
            }
            else if (expected_type == REAL_TYPE && is_integer_type(element_type))
            {
                compatible = 1;
            }
            else if (expected_type == STRING_TYPE && element_type == CHAR_TYPE)
            {
                compatible = 1;
            }

            if (!compatible)
            {
                if (getenv("KGPC_DEBUG_ARRAY_LITERAL") != NULL)
                {
                    fprintf(stderr,
                        "[KGPC] array literal mismatch @ line %d index %d: expected=%d (%s) got=%d\n",
                        line_num,
                        index,
                        expected_type,
                        expected_type_id != NULL ? expected_type_id : "<null>",
                        element_type);
                }
                semcheck_error_with_context("Error on line %d, element %d of array literal "
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
    if (formal_decl == NULL || arg_expr == NULL)
        return 0;

    int expected_type = UNKNOWN_TYPE;
    const char *expected_type_id = NULL;
    int expected_is_array_of_const = 0;
    int is_open_array_param = (formal_decl->type == TREE_ARR_DECL);
    if (formal_decl->type == TREE_ARR_DECL)
    {
        expected_type = formal_decl->tree_data.arr_decl_data.type;
        expected_type_id = formal_decl->tree_data.arr_decl_data.type_id;
    }
    else if (formal_decl->type == TREE_VAR_DECL)
    {
        expected_type = formal_decl->tree_data.var_decl_data.type;
        expected_type_id = formal_decl->tree_data.var_decl_data.type_id;
    }
    else
    {
        return 0;
    }

    if (expected_type == ARRAY_OF_CONST_TYPE)
        expected_is_array_of_const = 1;
    if (!expected_is_array_of_const)
    {
        int owns_expected = 0;
        KgpcType *expected_kgpc = resolve_type_from_vardecl(formal_decl, symtab, &owns_expected);
        if (expected_kgpc != NULL && expected_kgpc->kind == TYPE_KIND_ARRAY)
        {
            KgpcType *elem = expected_kgpc->info.array_info.element_type;
            if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE &&
                elem->info.primitive_type_tag == ARRAY_OF_CONST_TYPE)
            {
                expected_is_array_of_const = 1;
            }
        }
        if (owns_expected && expected_kgpc != NULL)
            destroy_kgpc_type(expected_kgpc);
    }

    if (getenv("KGPC_DEBUG_ARRAY_LITERAL") != NULL && arg_expr->type == EXPR_ARRAY_LITERAL)
    {
        fprintf(stderr,
            "[KGPC] array literal @ line %d: formal_decl=%s expected_type=%d expected_id=%s array_of_const=%d\n",
            line_num,
            formal_decl->type == TREE_ARR_DECL ? "ARR_DECL" :
            (formal_decl->type == TREE_VAR_DECL ? "VAR_DECL" : "OTHER"),
            expected_type,
            expected_type_id != NULL ? expected_type_id : "<null>",
            expected_is_array_of_const);
    }

    if (arg_expr->type == EXPR_SET)
    {
        if (!expected_is_array_of_const && !is_open_array_param)
            return 0;
        if (semcheck_convert_set_literal_to_array_literal(arg_expr) != 0)
        {
            semcheck_error_with_context("Error on line %d, open array literal cannot contain ranges.\n",
                line_num);
            return 1;
        }
    }

    if (arg_expr->type != EXPR_ARRAY_LITERAL)
        return 0;

    if (expected_is_array_of_const)
    {
        ListNode_t *cur_elem = arg_expr->expr_data.array_literal_data.elements;
        while (cur_elem != NULL)
        {
            struct Expression *element_expr = (struct Expression *)cur_elem->cur;
            KgpcType *element_kgpc_type = NULL;
            semcheck_expr_with_type(&element_kgpc_type, symtab, element_expr,
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

struct RecordType *semcheck_record_type_from_decl(Tree_t *decl, SymTab_t *symtab)
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

KgpcType *semcheck_field_expected_kgpc_type(SymTab_t *symtab, struct RecordField *field)
{
    if (symtab == NULL || field == NULL)
        return NULL;

    if (field->proc_type != NULL)
    {
        kgpc_type_retain(field->proc_type);
        return field->proc_type;
    }

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

    /* Handle inline pointer fields like bufptr: ^Char */
    if (field->is_pointer)
    {
        KgpcType *pointee_type = NULL;
        if (field->pointer_type_id != NULL)
        {
            HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, field->pointer_type_id);
            if (type_node != NULL)
            {
                if (type_node->type != NULL)
                {
                    kgpc_type_retain(type_node->type);
                    pointee_type = type_node->type;
                }
                else
                {
                    struct TypeAlias *alias = hashnode_get_type_alias(type_node);
                    if (alias != NULL)
                        pointee_type = create_kgpc_type_from_type_alias(alias, symtab);
                }
            }
        }
        if (pointee_type == NULL && field->pointer_type != UNKNOWN_TYPE)
            pointee_type = create_primitive_type(field->pointer_type);
        return create_pointer_type(pointee_type);
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

int semcheck_typecheck_record_constructor(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, struct RecordType *record_type, int line_num)
{
    if (expr == NULL || symtab == NULL)
        return 0;

    if (record_type == NULL)
    {
        semcheck_error_with_context("Error on line %d, unable to infer record type for constructor.\n",
            line_num);
        semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
        return 1;
    }

    expr->record_type = record_type;
    semcheck_expr_set_resolved_type(expr, RECORD_TYPE);
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
                semcheck_error_with_context("Error on line %d, duplicate record constructor field %s.\n",
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
                    semcheck_error_with_context("Error on line %d, array field %s cannot use set ranges.\n",
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

        KgpcType *value_kgpc_type = NULL;
        error_count += semcheck_expr_with_type(&value_kgpc_type, symtab, field->value, max_scope_lev, NO_MUTATE);

        int expected_owned = 1;
        KgpcType *expected_type = semcheck_field_expected_kgpc_type(symtab, field_desc);
        if (expected_type == NULL)
        {
            semcheck_error_with_context("Error on line %d, unable to resolve type for field %s.\n",
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
                semcheck_error_with_context("Error on line %d, unable to resolve type for field %s.\n",
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
        semcheck_error_with_context("Error on line %d, unable to infer record type for constructor.\n",
            line_num);
        return 1;
    }

    return semcheck_typecheck_record_constructor(arg_expr, symtab, max_scope_lev,
        record_type, line_num);
}
