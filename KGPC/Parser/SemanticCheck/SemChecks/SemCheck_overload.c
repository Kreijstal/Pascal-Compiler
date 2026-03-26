/*
    Overload resolution extracted from SemCheck_expr.c
*/

#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <assert.h>
#include <stdio.h>
#include <limits.h>

#include "SemCheck_overload.h"
#include "../HashTable/HashTable.h"
#include "../SemCheck.h"
#include "SemCheck_Expr_Internal.h"
#include "../../ParseTree/type_tags.h"
#include "../../ParseTree/tree.h"
#include "../../../identifier_utils.h"

int semcheck_candidate_is_builtin(SymTab_t *symtab, HashNode_t *node)
{
    if (symtab == NULL || node == NULL || node->id == NULL)
        return 0;

    ListNode_t *matches = FindAllIdentsInTable(symtab->builtin_scope->table, node->id);
    int is_builtin = 0;
    for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
    {
        if (cur->cur == node)
        {
            is_builtin = 1;
            break;
        }
    }
    if (matches != NULL)
        DestroyList(matches);
    return is_builtin;
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

static int semcheck_string_kind_from_type_id(const char *type_id);

static int semcheck_array_elem_legacy_tag(KgpcType *type)
{
    if (type == NULL)
        return UNKNOWN_TYPE;
    if (type->kind == TYPE_KIND_PRIMITIVE)
        return type->info.primitive_type_tag;
    if (type->kind == TYPE_KIND_ARRAY)
    {
        if (type->info.array_info.start_index == 0 &&
            type->info.array_info.end_index == 255)
        {
            KgpcType *elem = type->info.array_info.element_type;
            if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE)
            {
                int tag = elem->info.primitive_type_tag;
                if (tag == CHAR_TYPE || tag == STRING_TYPE || tag == SHORTSTRING_TYPE)
                    return SHORTSTRING_TYPE;
            }
        }
    }
    return UNKNOWN_TYPE;
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

/*
 * Check if two primitive type tags are size-and-signedness compatible.
 * This is used for VAR/OUT parameter matching where Integer/LongInt and
 * Cardinal/LongWord/DWord should be treated as the same type.
 * Returns 1 if compatible, 0 otherwise.
 */
static int are_primitive_tags_compatible(int tag_a, int tag_b)
{
    if (tag_a == tag_b)
        return 1;
    
    /* Integer and LongInt are both 32-bit signed on x86-64 */
    if ((tag_a == INT_TYPE || tag_a == LONGINT_TYPE) &&
        (tag_b == INT_TYPE || tag_b == LONGINT_TYPE))
        return 1;
    
    /* Cardinal, LongWord, DWord are all 32-bit unsigned */
    /* They share the LONGWORD_TYPE tag, but might have different type_alias names */
    if (tag_a == LONGWORD_TYPE && tag_b == LONGWORD_TYPE)
        return 1;
    
    /* Int64 and QWord are both 64-bit and interchangeable for var params */
    if ((tag_a == INT64_TYPE || tag_a == QWORD_TYPE) &&
        (tag_b == INT64_TYPE || tag_b == QWORD_TYPE))
        return 1;

    return 0;
}

static int semcheck_is_widechar_like_type(KgpcType *type)
{
    if (type == NULL || !kgpc_type_is_char(type))
        return 0;

    struct TypeAlias *alias = kgpc_type_get_type_alias(type);
    if (alias != NULL)
    {
        if ((alias->alias_name != NULL &&
             (pascal_identifier_equals(alias->alias_name, "WideChar") ||
              pascal_identifier_equals(alias->alias_name, "UnicodeChar"))) ||
            (alias->target_type_id != NULL &&
             (pascal_identifier_equals(alias->target_type_id, "WideChar") ||
              pascal_identifier_equals(alias->target_type_id, "UnicodeChar"))))
        {
            return 1;
        }
    }

    /* WideChar/UnicodeChar are 2-byte CHAR_TYPE in KGPC. */
    return kgpc_type_sizeof(type) == 2;
}

int semcheck_candidates_share_signature(SymTab_t *symtab, HashNode_t *a, HashNode_t *b)
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
        {
            int owns_a = 0;
            int owns_b = 0;
            int same_kgpc = 0;
            KgpcType *kg_a = resolve_type_from_vardecl(decl_a, symtab, &owns_a);
            KgpcType *kg_b = resolve_type_from_vardecl(decl_b, symtab, &owns_b);
            if (kg_a != NULL && kg_b != NULL && kgpc_type_equals(kg_a, kg_b))
                same_kgpc = 1;
            if (owns_a && kg_a != NULL)
                destroy_kgpc_type(kg_a);
            if (owns_b && kg_b != NULL)
                destroy_kgpc_type(kg_b);
            if (!same_kgpc && !are_primitive_tags_compatible(type_a, type_b))
                return 0;
        }
        if (type_a == POINTER_TYPE)
        {
            const char *id_a = semcheck_get_param_type_id(decl_a);
            const char *id_b = semcheck_get_param_type_id(decl_b);
            if (id_a == NULL && id_b == NULL)
            {
                /* Both untyped pointers - this param matches, check remaining. */
                args_a = args_a->next;
                args_b = args_b->next;
                continue;
            }
            int owns_a = 0;
            int owns_b = 0;
            KgpcType *kg_a = resolve_type_from_vardecl(decl_a, symtab, &owns_a);
            KgpcType *kg_b = resolve_type_from_vardecl(decl_b, symtab, &owns_b);
            if (kg_a != NULL && kg_b != NULL)
            {
                int eq = kgpc_type_equals(kg_a, kg_b);
                if (owns_a && kg_a != NULL)
                    destroy_kgpc_type(kg_a);
                if (owns_b && kg_b != NULL)
                    destroy_kgpc_type(kg_b);
                if (!eq)
                {
                    if (id_a != NULL && id_b != NULL)
                    {
                        int kind_a = semcheck_string_kind_from_type_id(id_a);
                        int kind_b = semcheck_string_kind_from_type_id(id_b);
                        if (kind_a != 0 && kind_a == kind_b)
                        {
                            args_a = args_a->next;
                            args_b = args_b->next;
                            continue;
                        }
                    }
                    return 0;
                }
            }
            else
            {
                if (owns_a && kg_a != NULL)
                    destroy_kgpc_type(kg_a);
                if (owns_b && kg_b != NULL)
                    destroy_kgpc_type(kg_b);

                if (id_a != NULL || id_b != NULL)
                {
                    if (id_a == NULL || id_b == NULL ||
                        !pascal_identifier_equals(id_a, id_b))
                    {
                        if (id_a != NULL && id_b != NULL)
                        {
                            int kind_a = semcheck_string_kind_from_type_id(id_a);
                            int kind_b = semcheck_string_kind_from_type_id(id_b);
                            if (kind_a != 0 && kind_a == kind_b)
                            {
                                args_a = args_a->next;
                                args_b = args_b->next;
                                continue;
                            }
                        }
                        return 0;
                    }
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
        }
        args_a = args_a->next;
        args_b = args_b->next;
    }

    return 1;
}

/* Check if two candidates differ in string subtypes (e.g. RawByteString vs
 * UnicodeString). Used in the override check to prevent the full overload
 * resolution from overriding a correct mangled-name shortcut match. */
int semcheck_candidates_string_subtypes_differ(SymTab_t *symtab, HashNode_t *a, HashNode_t *b)
{
    if (a == NULL || b == NULL || a->type == NULL || b->type == NULL)
        return 0;
    if (a->type->kind != TYPE_KIND_PROCEDURE || b->type->kind != TYPE_KIND_PROCEDURE)
        return 0;

    ListNode_t *args_a = a->type->info.proc_info.params;
    ListNode_t *args_b = b->type->info.proc_info.params;

    while (args_a != NULL && args_b != NULL)
    {
        Tree_t *decl_a = (Tree_t *)args_a->cur;
        Tree_t *decl_b = (Tree_t *)args_b->cur;
        int type_a = resolve_param_type(decl_a, symtab);
        int type_b = resolve_param_type(decl_b, symtab);

        if (type_a == STRING_TYPE && type_b == STRING_TYPE)
        {
            const char *id_a = semcheck_get_param_type_id(decl_a);
            const char *id_b = semcheck_get_param_type_id(decl_b);
            if (id_a != NULL && id_b != NULL &&
                !pascal_identifier_equals(id_a, id_b))
            {
                int kind_a = semcheck_string_kind_from_type_id(id_a);
                int kind_b = semcheck_string_kind_from_type_id(id_b);
                if (kind_a != kind_b || (kind_a == 0 && kind_b == 0))
                    return 1;
            }
        }
        args_a = args_a->next;
        args_b = args_b->next;
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
        return decl->tree_data.var_decl_data.initializer != NULL;
    }
    else if (decl->type == TREE_ARR_DECL)
    {
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
    {
        return init->stmt_data.var_assign_data.expr;
    }

    return NULL;
}

/* Helper to count required parameters (those without defaults) */
static int count_param_decl_ids(Tree_t *param_decl)
{
    if (param_decl == NULL)
        return 0;
    if (param_decl->type == TREE_VAR_DECL &&
        param_decl->tree_data.var_decl_data.ids != NULL)
        return ListLength(param_decl->tree_data.var_decl_data.ids);
    if (param_decl->type == TREE_ARR_DECL &&
        param_decl->tree_data.arr_decl_data.ids != NULL)
        return ListLength(param_decl->tree_data.arr_decl_data.ids);
    return 0;
}

int semcheck_count_total_params(ListNode_t *params)
{
    int total = 0;
    for (ListNode_t *cur = params; cur != NULL; cur = cur->next)
    {
        Tree_t *param_decl = (Tree_t *)cur->cur;
        total += count_param_decl_ids(param_decl);
    }
    return total;
}

int semcheck_count_required_params(ListNode_t *params)
{
    int required = 0;
    ListNode_t *cur = params;

    /* Once we see a parameter with a default, all following must also have defaults */
    while (cur != NULL)
    {
        Tree_t *param_decl = (Tree_t *)cur->cur;
        int has_default = param_has_default_value(param_decl);
        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            const char *param_id = "?";
            if (param_decl && param_decl->type == TREE_VAR_DECL && param_decl->tree_data.var_decl_data.ids)
                param_id = (char *)param_decl->tree_data.var_decl_data.ids->cur;
            fprintf(stderr, "[SemCheck] count_required_params: param=%s has_default=%d\n", param_id, has_default);
        }
        if (!has_default)
            required += count_param_decl_ids(param_decl);
        else
            break;  /* All remaining params have defaults */
        cur = cur->next;
    }

    return required;
}

int semcheck_method_accepts_arg_count(ListNode_t *params, int arg_count, int *expects_self_out, int is_varargs)
{
    int expects_self = 0;
    int total_params = semcheck_count_total_params(params);
    int required_params = semcheck_count_required_params(params);

    if (params != NULL)
    {
        Tree_t *first_param = (Tree_t *)params->cur;
        if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
            first_param->tree_data.var_decl_data.ids != NULL)
        {
            const char *first_id = (const char *)first_param->tree_data.var_decl_data.ids->cur;
            if (first_id != NULL && pascal_identifier_equals(first_id, "Self"))
                expects_self = 1;
        }
    }

    if (expects_self)
    {
        int self_count = 0;
        if (params != NULL)
        {
            Tree_t *first_param = (Tree_t *)params->cur;
            self_count = count_param_decl_ids(first_param);
        }
        if (required_params >= self_count)
            required_params -= self_count;
        if (total_params >= self_count)
            total_params -= self_count;
    }

    if (expects_self_out != NULL)
        *expects_self_out = expects_self;

    /* For varargs functions, only check the lower bound */
    if (is_varargs)
        return (arg_count >= required_params);

    return (arg_count >= required_params && arg_count <= total_params);
}

int semcheck_append_default_args(ListNode_t **args_head, ListNode_t *formal_params, int line_num)
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

int semcheck_named_arg_type_compatible(Tree_t *formal_decl,
    struct Expression *rhs_expr,
    int rhs_type,
    SymTab_t *symtab)
{
    int expected_type = resolve_param_type(formal_decl, symtab);

    if (expected_type == UNKNOWN_TYPE || rhs_type == UNKNOWN_TYPE)
        return 1;
    if (expected_type == rhs_type)
        return 1;
    if (is_integer_type(expected_type) && is_integer_type(rhs_type))
        return 1;
    if (expected_type == REAL_TYPE && is_integer_type(rhs_type))
        return 1;
    if (is_string_type(expected_type) &&
        (is_string_type(rhs_type) || rhs_type == CHAR_TYPE))
        return 1;
    if (expected_type == ENUM_TYPE && rhs_type == ENUM_TYPE)
        return 1;
    if ((expected_type == POINTER_TYPE || expected_type == PROCEDURE) &&
        (rhs_type == POINTER_TYPE || rhs_type == PROCEDURE))
        return 1;
    if (expected_type == POINTER_TYPE && rhs_type == CHAR_TYPE)
    {
        int owns_expected = 0;
        KgpcType *expected_kgpc = resolve_type_from_vardecl(formal_decl, symtab, &owns_expected);
        if (expected_kgpc != NULL && kgpc_type_is_pointer(expected_kgpc))
        {
            KgpcType *points_to = expected_kgpc->info.points_to;
            if (points_to != NULL &&
                points_to->kind == TYPE_KIND_PRIMITIVE &&
                points_to->info.primitive_type_tag == CHAR_TYPE)
            {
                if (owns_expected)
                    destroy_kgpc_type(expected_kgpc);
                return 1;
            }
        }
        if (owns_expected && expected_kgpc != NULL)
            destroy_kgpc_type(expected_kgpc);
    }

    if (rhs_expr != NULL && rhs_expr->resolved_kgpc_type != NULL)
    {
        int owns_expected = 0;
        KgpcType *expected_kgpc = resolve_type_from_vardecl(formal_decl, symtab, &owns_expected);
        if (expected_kgpc != NULL)
        {
            if (are_types_compatible_for_assignment(expected_kgpc, rhs_expr->resolved_kgpc_type, symtab))
            {
                if (owns_expected)
                    destroy_kgpc_type(expected_kgpc);
                return 1;
            }
            if (owns_expected)
                destroy_kgpc_type(expected_kgpc);
        }
    }

    return 0;
}

int semcheck_param_list_contains_name(ListNode_t *params, const char *name)
{
    if (params == NULL || name == NULL)
        return 0;

    for (ListNode_t *cur = params; cur != NULL; cur = cur->next)
    {
        Tree_t *decl = (Tree_t *)cur->cur;
        const char *param_name = NULL;
        if (decl != NULL && decl->type == TREE_VAR_DECL &&
            decl->tree_data.var_decl_data.ids != NULL)
        {
            param_name = (const char *)decl->tree_data.var_decl_data.ids->cur;
        }
        else if (decl != NULL && decl->type == TREE_ARR_DECL &&
            decl->tree_data.arr_decl_data.ids != NULL)
        {
            param_name = (const char *)decl->tree_data.arr_decl_data.ids->cur;
        }

        if (param_name != NULL && pascal_identifier_equals(param_name, name))
            return 1;
    }
    return 0;
}

static int semcheck_infer_literal_elem_type(struct Expression *expr)
{
    struct Expression *elem_expr = NULL;
    if (expr == NULL)
        return UNKNOWN_TYPE;

    if (expr->type == EXPR_SET)
    {
        struct SetElement *set_elem = NULL;
        if (expr->expr_data.set_data.elements != NULL)
            set_elem = (struct SetElement *)expr->expr_data.set_data.elements->cur;
        if (set_elem != NULL)
            elem_expr = set_elem->lower;
    }
    else if (expr->type == EXPR_ARRAY_LITERAL)
    {
        if (expr->expr_data.array_literal_data.elements != NULL)
            elem_expr = (struct Expression *)expr->expr_data.array_literal_data.elements->cur;
    }

    if (elem_expr == NULL)
        return UNKNOWN_TYPE;

    if (elem_expr->type == EXPR_INUM)
        return INT_TYPE;
    if (elem_expr->type == EXPR_CHAR_CODE)
        return CHAR_TYPE;
    if (elem_expr->type == EXPR_STRING)
    {
        if (elem_expr->expr_data.string != NULL &&
            strlen(elem_expr->expr_data.string) == 1)
            return CHAR_TYPE;
        return STRING_TYPE;
    }
    if (elem_expr->type == EXPR_BOOL)
        return BOOL;
    if (elem_expr->type == EXPR_RNUM)
        return REAL_TYPE;

    return UNKNOWN_TYPE;
}

static int semcheck_resolve_arg_kgpc_type(struct Expression *arg_expr,
    SymTab_t *symtab,
    int max_scope_lev,
    int *owns_type_out,
    KgpcType **arg_type_out)
{
    if (owns_type_out != NULL)
        *owns_type_out = 0;
    if (arg_type_out != NULL)
        *arg_type_out = NULL;
    if (arg_expr == NULL)
        return UNKNOWN_TYPE;

    if (arg_expr->type == EXPR_FUNCTION_CALL &&
        arg_expr->expr_data.function_call_data.args_expr == NULL &&
        arg_expr->expr_data.function_call_data.id != NULL)
    {
        HashNode_t *type_node = semcheck_find_preferred_type_node(symtab,
            arg_expr->expr_data.function_call_data.id);
        if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE &&
            type_node->type != NULL)
        {
            KgpcType *resolved = type_node->type;
            struct RecordType *type_record = get_record_type_from_node(type_node);
            if (type_record != NULL && type_record->is_interface)
            {
                HashNode_t *tguid_node = semcheck_find_type_node_with_kgpc_type(symtab, "TGUID");
                if (tguid_node != NULL && tguid_node->type != NULL)
                    resolved = tguid_node->type;
            }
            if (arg_type_out != NULL)
                *arg_type_out = resolved;
            if (owns_type_out != NULL)
                *owns_type_out = 0;
            return semcheck_tag_from_kgpc(resolved);
        }
    }

    int arg_tag = UNKNOWN_TYPE;
    KgpcType *arg_kgpc_type = NULL;
    semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);
    arg_tag = semcheck_tag_from_kgpc(arg_kgpc_type);

    if (arg_expr->type == EXPR_TYPECAST &&
        arg_expr->expr_data.typecast_data.target_type_id != NULL &&
        (arg_kgpc_type == NULL ||
         (arg_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
          arg_kgpc_type->info.primitive_type_tag == RECORD_TYPE)))
    {
        struct RecordType *rec = semcheck_lookup_record_type(symtab,
            arg_expr->expr_data.typecast_data.target_type_id);
        if (rec != NULL)
        {
            if (owns_type_out != NULL)
                *owns_type_out = 1;
            if (arg_type_out != NULL)
                *arg_type_out = create_record_type(rec);
            return RECORD_TYPE;
        }
    }

    KgpcType *arg_type = arg_expr->resolved_kgpc_type;
    if (arg_type != NULL && arg_tag != UNKNOWN_TYPE)
    {
        int keep = 0;
        if (arg_tag == POINTER_TYPE)
        {
            if (arg_type->kind == TYPE_KIND_POINTER)
                keep = 1;
            /* Also keep TYPE_KIND_PRIMITIVE with is_pointer type_alias —
             * these carry named class type info that would be lost if we
             * fell through to the generic fallback. */
            else if (arg_type->kind == TYPE_KIND_PRIMITIVE &&
                     arg_type->type_alias != NULL &&
                     arg_type->type_alias->is_pointer)
                keep = 1;
        }
        else if (arg_type->kind == TYPE_KIND_PRIMITIVE &&
            arg_type->info.primitive_type_tag == arg_tag)
            keep = 1;
        else if (arg_tag == POINTER_TYPE && arg_type->kind == TYPE_KIND_POINTER)
            keep = 1;
        else if (arg_tag == RECORD_TYPE && arg_type->kind == TYPE_KIND_RECORD)
            keep = 1;
        else if (arg_tag == PROCEDURE && arg_type->kind == TYPE_KIND_PROCEDURE)
            keep = 1;
        else if (arg_type->kind == TYPE_KIND_ARRAY)
        {
            KgpcType *elem = arg_type->info.array_info.element_type;
            if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE &&
                elem->info.primitive_type_tag == arg_tag)
                keep = 1;
            else if (arg_tag == ARRAY_OF_CONST_TYPE &&
                kgpc_type_is_array_of_const(arg_type))
                keep = 1;
        }
        if (!keep)
            arg_type = NULL;
    }
    if (arg_type == NULL && arg_expr->is_array_expr)
    {
        KgpcType *elem_type = NULL;
        int elem_owned = 0;
        if (arg_expr->array_element_type_id != NULL && symtab != NULL)
        {
            HashNode_t *elem_node = NULL;
            if (FindSymbol(&elem_node, symtab, arg_expr->array_element_type_id) != 0 &&
                elem_node != NULL && elem_node->type != NULL)
                elem_type = elem_node->type;
        }
        if (elem_type == NULL && arg_expr->array_element_type != UNKNOWN_TYPE)
        {
            elem_type = create_primitive_type(arg_expr->array_element_type);
            elem_owned = 1;
        }
        if (elem_type != NULL)
        {
            if (!elem_owned)
                kgpc_type_retain(elem_type);
            int start = arg_expr->array_lower_bound;
            int end = arg_expr->array_upper_bound;
            if (arg_expr->array_is_dynamic)
                end = start - 1;
            KgpcType *arr_type = create_array_type(elem_type, start, end);
            if (arr_type != NULL)
            {
                arg_type = arr_type;
                if (owns_type_out != NULL)
                    *owns_type_out = 1;
                /* create_array_type takes ownership of elem_type, so we must NOT destroy it here.
                 * The elem_type will be freed when arr_type is destroyed. */
            }
            else if (elem_owned && elem_type != NULL)
            {
                /* Only destroy elem_type if array creation failed (ownership not transferred) */
                destroy_kgpc_type(elem_type);
            }
        }
    }
    if (arg_type == NULL)
    {
        switch (arg_tag)
        {
            case INT_TYPE:
            case LONGINT_TYPE:
            case INT64_TYPE:
            case REAL_TYPE:
            case STRING_TYPE:
            case SHORTSTRING_TYPE:
            case CHAR_TYPE:
            case BOOL:
            case ENUM_TYPE:
            case SET_TYPE:
            case FILE_TYPE:
            case TEXT_TYPE:
            case BYTE_TYPE:
            case WORD_TYPE:
            case LONGWORD_TYPE:
            case QWORD_TYPE:
            case PROCEDURE:
                arg_type = create_primitive_type(arg_tag);
                break;
            case POINTER_TYPE:
                if (arg_expr != NULL && arg_expr->pointer_subtype_id != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindSymbol(&type_node, symtab, arg_expr->pointer_subtype_id) != 0 &&
                        type_node != NULL && type_node->type != NULL)
                    {
                        arg_type = create_pointer_type(type_node->type);
                    }
                }
                if (arg_type == NULL && arg_expr != NULL &&
                    arg_expr->pointer_subtype != UNKNOWN_TYPE)
                {
                    KgpcType *points_to = create_primitive_type(arg_expr->pointer_subtype);
                    if (points_to != NULL)
                    {
                        arg_type = create_pointer_type(points_to);
                        kgpc_type_release(points_to);
                    }
                }
                if (arg_type == NULL)
                    arg_type = create_pointer_type(NULL);
                break;
            default:
                break;
        }
        if (arg_type != NULL && owns_type_out != NULL)
            *owns_type_out = 1;
    }
    if (arg_type_out != NULL)
        *arg_type_out = arg_type;
    if ((arg_tag == INT_TYPE || arg_tag == LONGINT_TYPE || arg_tag == INT64_TYPE) &&
        arg_type != NULL && arg_type->type_alias != NULL)
    {
        const char *alias_name = arg_type->type_alias->alias_name != NULL
            ? arg_type->type_alias->alias_name
            : arg_type->type_alias->target_type_id;
        if (alias_name != NULL)
        {
            int mapped = semcheck_map_builtin_type_name(symtab, alias_name);
            if (mapped == BYTE_TYPE || mapped == WORD_TYPE ||
                mapped == LONGWORD_TYPE || mapped == QWORD_TYPE)
                arg_tag = mapped;
        }
    }
    return arg_tag;
}

static void semcheck_bind_set_literal_to_formal_type(struct Expression *arg_expr,
    KgpcType *formal_kgpc,
    int *owns_arg,
    KgpcType **arg_kgpc,
    int *arg_tag)
{
    if (arg_expr == NULL || arg_expr->type != EXPR_SET ||
        formal_kgpc == NULL || !kgpc_type_is_set(formal_kgpc))
        return;

    semcheck_expr_set_resolved_kgpc_type_shared(arg_expr, formal_kgpc);

    if (owns_arg != NULL && *owns_arg && arg_kgpc != NULL && *arg_kgpc != NULL)
        destroy_kgpc_type(*arg_kgpc);

    if (arg_kgpc != NULL)
    {
        kgpc_type_retain(formal_kgpc);
        *arg_kgpc = formal_kgpc;
    }
    if (owns_arg != NULL)
        *owns_arg = 1;
    if (arg_tag != NULL)
        *arg_tag = SET_TYPE;
}


static MatchQuality semcheck_make_quality(MatchQualityKind kind)
{
    MatchQuality q;
    q.kind = kind;
    q.exact_type_id = 0;
    q.integer_rank = 0;
    q.string_rank = 0;
    return q;
}

static MatchQuality semcheck_match_from_rank(int rank)
{
    if (rank == 0)
        return semcheck_make_quality(MATCH_EXACT);
    if (rank == 1)
        return semcheck_make_quality(MATCH_PROMOTION);
    if (rank > 1)
    {
        MatchQuality q = semcheck_make_quality(MATCH_CONVERSION);
        q.integer_rank = rank;  /* preserve distance for tiebreaking */
        return q;
    }
    return semcheck_make_quality(MATCH_INCOMPATIBLE);
}

static const char *semcheck_overload_record_type_id_from_kgpc(KgpcType *type)
{
    if (type == NULL)
        return NULL;

    if (type->kind == TYPE_KIND_RECORD && type->info.record_info != NULL &&
        type->info.record_info->type_id != NULL)
        return type->info.record_info->type_id;

    if (type->kind == TYPE_KIND_POINTER && type->info.points_to != NULL &&
        type->info.points_to->kind == TYPE_KIND_RECORD &&
        type->info.points_to->info.record_info != NULL &&
        type->info.points_to->info.record_info->type_id != NULL)
        return type->info.points_to->info.record_info->type_id;

    if (type->type_alias != NULL)
    {
        if (type->type_alias->target_type_id != NULL)
            return type->type_alias->target_type_id;
        if (type->type_alias->alias_name != NULL)
            return type->type_alias->alias_name;
    }

    return NULL;
}

static int semcheck_overload_type_is_recordish(KgpcType *type)
{
    if (type == NULL)
        return 0;
    if (kgpc_type_is_record(type))
        return 1;
    return semcheck_tag_from_kgpc(type) == RECORD_TYPE;
}

static int semcheck_overload_record_assign_operator_score(SymTab_t *symtab, HashNode_t *cand,
    KgpcType *target_type, KgpcType *source_type)
{
    if (symtab == NULL || cand == NULL || cand->type == NULL ||
        target_type == NULL || source_type == NULL)
        return 0;
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
    if (arg_rank < 0 && are_types_compatible_for_assignment(param_type, source_type, symtab))
        arg_rank = 4;

    KgpcType *ret_type = kgpc_type_get_return_type(cand->type);
    int ret_rank = (ret_type != NULL) ? kgpc_type_conversion_rank(ret_type, target_type) : -1;
    if (ret_rank < 0 && ret_type != NULL &&
        are_types_compatible_for_assignment(target_type, ret_type, symtab))
        ret_rank = 4;

    if (param_owned && param_type != NULL)
        destroy_kgpc_type(param_type);

    return (arg_rank >= 0 && ret_rank >= 0 && ret_type != NULL);
}

static int semcheck_overload_symbol_is_assign_operator(HashNode_t *cand)
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
         pascal_identifier_equals(cand->method_name, ":=")))
        return 1;
    return 0;
}

/* Check whether an operator := conversion exists between actual and formal
 * types, in either direction.  Handles both "assign to record from actual"
 * (formal is record) and "assign from record to formal" (actual is record,
 * e.g. Tconstexprint → qword). */
static int semcheck_overload_has_record_assign_conversion(SymTab_t *symtab,
    KgpcType *actual_kgpc, KgpcType *formal_kgpc, int actual_tag)
{
    int formal_is_record = (formal_kgpc != NULL && semcheck_overload_type_is_recordish(formal_kgpc));
    int actual_is_record = (actual_kgpc != NULL && semcheck_overload_type_is_recordish(actual_kgpc));
    if (symtab == NULL || (!formal_is_record && !actual_is_record))
        return 0;

    int actual_owned = 0;
    if (actual_kgpc == NULL)
    {
        if (actual_tag == UNKNOWN_TYPE)
            return 0;
        actual_kgpc = create_primitive_type(actual_tag);
        actual_owned = (actual_kgpc != NULL);
    }
    if (actual_kgpc == NULL)
        return 0;

    /* When only the actual side is a record, formal_kgpc may be a primitive
     * type (e.g. qword) that semcheck_overload_record_type_id_from_kgpc
     * cannot name.  The operator lookup still works via the source (actual
     * record) name — the score function checks the operator's return type
     * against formal_kgpc directly. */
    const char *target_type_id = semcheck_overload_record_type_id_from_kgpc(formal_kgpc);
    const char *source_type_id = semcheck_overload_record_type_id_from_kgpc(actual_kgpc);
    int found = 0;

    char ids[8][320];
    for (int i = 0; i < 8; ++i)
        ids[i][0] = '\0';
    if (target_type_id != NULL)
    {
        snprintf(ids[0], sizeof(ids[0]), "%s.:=", target_type_id);
        snprintf(ids[1], sizeof(ids[1]), "%s__op_assign", target_type_id);
    }
    if (source_type_id != NULL)
    {
        snprintf(ids[2], sizeof(ids[2]), "%s.:=", source_type_id);
        snprintf(ids[3], sizeof(ids[3]), "%s__op_assign", source_type_id);
    }
    if (target_type_id != NULL && source_type_id != NULL)
    {
        snprintf(ids[4], sizeof(ids[4]), "%s__op_assign_%s", target_type_id, source_type_id);
        snprintf(ids[5], sizeof(ids[5]), "%s__op_assign_%s", source_type_id, target_type_id);
    }

    for (int i = 0; i < 8 && !found; ++i)
    {
        if (ids[i][0] == '\0')
            continue;
        ListNode_t *cands = FindAllIdents(symtab, ids[i]);
        for (ListNode_t *cur = cands; cur != NULL; cur = cur->next)
        {
            if (semcheck_overload_record_assign_operator_score(symtab,
                    (HashNode_t *)cur->cur, formal_kgpc, actual_kgpc))
            {
                found = 1;
                break;
            }
        }
        DestroyList(cands);
    }

    if (!found)
    {
        ListNode_t *cands = FindAllIdents(symtab, "op_assign");
        for (ListNode_t *cur = cands; cur != NULL; cur = cur->next)
        {
            if (semcheck_overload_record_assign_operator_score(symtab,
                    (HashNode_t *)cur->cur, formal_kgpc, actual_kgpc))
            {
                found = 1;
                break;
            }
        }
        DestroyList(cands);
    }

    if (!found)
    {
        ListNode_t *cands = FindAllIdents(symtab, ":=");
        for (ListNode_t *cur = cands; cur != NULL; cur = cur->next)
        {
            if (semcheck_overload_record_assign_operator_score(symtab,
                    (HashNode_t *)cur->cur, formal_kgpc, actual_kgpc))
            {
                found = 1;
                break;
            }
        }
        DestroyList(cands);
    }

    if (!found)
    {
        for (ScopeNode *scope = symtab->current_scope; scope != NULL && scope != symtab->builtin_scope && !found; scope = scope->parent)
        {
            HashTable_t *table = scope->table;
            if (table == NULL)
                continue;
            for (int i = 0; i < TABLE_SIZE && !found; ++i)
            {
                for (ListNode_t *cur = table->table[i]; cur != NULL; cur = cur->next)
                {
                    HashNode_t *cand = (HashNode_t *)cur->cur;
                    if (!semcheck_overload_symbol_is_assign_operator(cand))
                        continue;
                    if (semcheck_overload_record_assign_operator_score(symtab, cand,
                            formal_kgpc, actual_kgpc))
                    {
                        found = 1;
                        break;
                    }
                }
            }
        }
    }

    if (!found && symtab->builtin_scope->table != NULL)
    {
        for (int i = 0; i < TABLE_SIZE && !found; ++i)
        {
            for (ListNode_t *cur = symtab->builtin_scope->table->table[i]; cur != NULL; cur = cur->next)
            {
                HashNode_t *cand = (HashNode_t *)cur->cur;
                if (!semcheck_overload_symbol_is_assign_operator(cand))
                    continue;
                if (semcheck_overload_record_assign_operator_score(symtab, cand,
                        formal_kgpc, actual_kgpc))
                {
                    found = 1;
                    break;
                }
            }
        }
    }

    if (actual_owned)
        destroy_kgpc_type(actual_kgpc);
    return found;
}

static MatchQuality semcheck_classify_match(int actual_tag, KgpcType *actual_kgpc,
    int formal_tag, KgpcType *formal_kgpc, int is_var_param, SymTab_t *symtab,
    int is_integer_literal)
{
    if (formal_tag == UNKNOWN_TYPE || formal_tag == BUILTIN_ANY_TYPE)
    {
        /* Untyped formals accept any argument, but score lower than typed formals
         * so that typed overloads (e.g. FpRead(buf: PAnsiChar)) are preferred
         * over untyped ones (e.g. FpRead(var buf)) when both match. */
        if (actual_tag == UNKNOWN_TYPE && actual_kgpc == NULL)
            return semcheck_make_quality(MATCH_CONVERSION);
        return semcheck_make_quality(MATCH_CONVERSION);
    }

    /* When the actual is untyped (e.g. forwarding an untyped const parameter),
     * allow it to match any typed formal as a conversion.  Both untyped→untyped
     * and untyped→typed get MATCH_CONVERSION, but the tiebreaker prefers
     * overloads with fewer untyped params. */
    if (actual_tag == UNKNOWN_TYPE && actual_kgpc == NULL && !is_var_param)
        return semcheck_make_quality(MATCH_CONVERSION);

    if (is_var_param)
    {
        if (actual_kgpc != NULL && formal_kgpc != NULL)
        {
            if (kgpc_type_equals(actual_kgpc, formal_kgpc))
                return semcheck_make_quality(MATCH_EXACT);
            int formal_is_untyped_ptr = (formal_kgpc->kind == TYPE_KIND_PRIMITIVE &&
                formal_kgpc->info.primitive_type_tag == POINTER_TYPE) ||
                (formal_kgpc->kind == TYPE_KIND_POINTER &&
                    formal_kgpc->info.points_to == NULL);
            int actual_is_untyped_ptr = (actual_kgpc->kind == TYPE_KIND_PRIMITIVE &&
                actual_kgpc->info.primitive_type_tag == POINTER_TYPE) ||
                (actual_kgpc->kind == TYPE_KIND_POINTER &&
                    actual_kgpc->info.points_to == NULL);
            int formal_is_ptr = (formal_kgpc->kind == TYPE_KIND_POINTER || formal_is_untyped_ptr);
            int actual_is_ptr = (actual_kgpc->kind == TYPE_KIND_POINTER || actual_is_untyped_ptr);
            if (formal_is_ptr && actual_is_ptr)
            {
                if (formal_is_untyped_ptr || actual_is_untyped_ptr)
                    return semcheck_make_quality(MATCH_EXACT);
                /* For class types: allow subclass to match parent class type */
                /* This handles constructor Self parameter matching where both are ^record */
                if (formal_kgpc->kind == TYPE_KIND_POINTER && actual_kgpc->kind == TYPE_KIND_POINTER)
                {
                    /* Check if types are directly compatible */
                    if (are_types_compatible_for_assignment(formal_kgpc, actual_kgpc, symtab))
                        return semcheck_make_quality(MATCH_EXACT);
                    /* For class pointers, also check if the pointed-to records are related.
                     * This handles cases where ^Record and ^record represent the same class
                     * but have different KgpcType instances. */
                    KgpcType *formal_inner = formal_kgpc->info.points_to;
                    KgpcType *actual_inner = actual_kgpc->info.points_to;
                    if (formal_inner != NULL && actual_inner != NULL &&
                        formal_inner->kind == TYPE_KIND_RECORD &&
                        actual_inner->kind == TYPE_KIND_RECORD)
                    {
                        /* If both are records, check if they share the same record_info
                         * or if one is a subclass of the other */
                        if (formal_inner->info.record_info == actual_inner->info.record_info)
                            return semcheck_make_quality(MATCH_EXACT);
                        /* Also allow if actual's record has the formal's record as an ancestor
                         * via the class hierarchy (checked by name since record_info may differ) */
                        if (are_types_compatible_for_assignment(formal_inner, actual_inner, symtab))
                            return semcheck_make_quality(MATCH_EXACT);
                    }
                }
            }
            if (actual_kgpc->kind == TYPE_KIND_RECORD &&
                formal_kgpc->kind == TYPE_KIND_RECORD &&
                formal_kgpc->info.record_info != NULL &&
                formal_kgpc->info.record_info->is_class)
            {
                if (are_types_compatible_for_assignment(formal_kgpc, actual_kgpc, symtab))
                    return semcheck_make_quality(MATCH_EXACT);
            }
            /* For primitive types with the same size and signedness, treat as compatible.
               This allows Integer/LongInt and Cardinal/LongWord/DWord to be interchangeable
               for VAR/OUT parameters, matching FPC behavior. */
            if (actual_kgpc->kind == TYPE_KIND_PRIMITIVE &&
                formal_kgpc->kind == TYPE_KIND_PRIMITIVE &&
                are_primitive_tags_compatible(actual_kgpc->info.primitive_type_tag,
                                              formal_kgpc->info.primitive_type_tag))
            {
                return semcheck_make_quality(MATCH_EXACT);
            }
            /* Allow String <-> ShortString for var params (openstring compatibility). */
            if (actual_kgpc->kind == TYPE_KIND_PRIMITIVE &&
                formal_kgpc->kind == TYPE_KIND_PRIMITIVE &&
                is_string_type(actual_kgpc->info.primitive_type_tag) &&
                is_string_type(formal_kgpc->info.primitive_type_tag))
                return semcheck_make_quality(MATCH_PROMOTION);
            /* For non-primitive types (class/interface/pointer), use assignment
             * compatibility. Primitive integer types (e.g. Int64 vs Longint) must
             * NOT match via assignment compatibility for var/out params. */
            if (!(actual_kgpc->kind == TYPE_KIND_PRIMITIVE &&
                  formal_kgpc->kind == TYPE_KIND_PRIMITIVE) &&
                are_types_compatible_for_assignment(formal_kgpc, actual_kgpc, symtab))
                return semcheck_make_quality(MATCH_PROMOTION);
            /* For real family types (REAL_TYPE / EXTENDED_TYPE), allow
             * var/out param matching with PROMOTION quality so that
             * overload resolution can still consider the candidate.
             * Without this, SinCos(Extended) is INCOMPATIBLE when
             * the argument is Float (= Extended stored as REAL_TYPE). */
            if (is_real_family_type(actual_kgpc->info.primitive_type_tag) &&
                is_real_family_type(formal_kgpc->info.primitive_type_tag))
            {
                int rank = kgpc_type_conversion_rank(actual_kgpc, formal_kgpc);
                if (rank >= 0)
                    return semcheck_match_from_rank(rank);
                return semcheck_make_quality(MATCH_PROMOTION);
            }
            return semcheck_make_quality(MATCH_INCOMPATIBLE);
        }
        if (actual_tag == formal_tag)
            return semcheck_make_quality(MATCH_EXACT);
        /* Check for compatible primitive types (e.g., Integer/LongInt) */
        if (are_primitive_tags_compatible(actual_tag, formal_tag))
            return semcheck_make_quality(MATCH_EXACT);
        if (is_string_type(actual_tag) && is_string_type(formal_tag))
            return semcheck_make_quality(MATCH_EXACT);
        return semcheck_make_quality(MATCH_INCOMPATIBLE);
    }

    if (formal_tag == UNKNOWN_TYPE || formal_tag == BUILTIN_ANY_TYPE)
    {
        if (actual_tag == UNKNOWN_TYPE && actual_kgpc == NULL)
            return semcheck_make_quality(MATCH_CONVERSION);
        return semcheck_make_quality(MATCH_EXACT);
    }
    /* Variant parameters accept any type, but should never be preferred
     * over a specific type match in overload resolution. */
    if (formal_tag == VARIANT_TYPE || actual_tag == VARIANT_TYPE)
        return semcheck_make_quality(MATCH_CONVERSION);
    /* For pointer types, don't return early - need to compare subtypes */
    if (actual_tag == formal_tag && formal_tag != POINTER_TYPE)
    {
        /* REAL_TYPE is used as a catch-all tag for all floating-point types
         * (Single, Double, Extended, Real, ValReal, Currency, etc.).
         * When both actual and formal have the same REAL_TYPE tag, compare
         * the type_alias names for exact equality so that e.g.
         * Double actual vs Extended formal is scored as PROMOTION rather
         * than EXACT.  This prevents ambiguity among Max(Single), Max(Double),
         * Max(Extended) overloads. */
        if (is_real_family_type(formal_tag) && actual_kgpc != NULL && formal_kgpc != NULL)
        {
            /* Use kgpc_type_conversion_rank to distinguish between
             * Single (4), Double (8), and Extended (10) for overload
             * resolution.  kgpc_type_equals returns false when sizes
             * or primitive tags differ; conversion_rank then scores
             * by size (widening > narrowing). */
            int rank = kgpc_type_conversion_rank(actual_kgpc, formal_kgpc);
            if (rank >= 0)
                return semcheck_match_from_rank(rank);
            /* Fall through to default EXACT if rank can't be determined */
        }
        /* For record types, compare the actual record identity.
         * Different record types (e.g. TGUID vs TObject) sharing the
         * same RECORD_TYPE tag must not score as EXACT — this prevents
         * Supports(IUnknown, TGUID, out) from beating
         * Supports(IUnknown, TClass, out) when the argument is an
         * interface reference, not an actual TGUID. */
        if (formal_tag == RECORD_TYPE && formal_kgpc != NULL)
        {
            if (kgpc_getenv("KGPC_DEBUG_OVERLOAD_QUALITY") != NULL)
                fprintf(stderr, "[KGPC] RECORD match: actual_kgpc=%p formal_kgpc=%p equals=%d\n",
                    (void*)actual_kgpc, (void*)formal_kgpc,
                    actual_kgpc ? kgpc_type_equals(actual_kgpc, formal_kgpc) : -1);
            if (actual_kgpc == NULL)
            {
                /* Actual type info lost (e.g. interface reference) —
                 * can't verify it matches the formal record, demote. */
                return semcheck_make_quality(MATCH_CONVERSION);
            }
            if (!kgpc_type_equals(actual_kgpc, formal_kgpc))
            {
                if (are_types_compatible_for_assignment(formal_kgpc, actual_kgpc, symtab))
                    return semcheck_make_quality(MATCH_CONVERSION);
                return semcheck_make_quality(MATCH_INCOMPATIBLE);
            }
        }
        return semcheck_make_quality(MATCH_EXACT);
    }
    /* String types are mutually compatible (STRING_TYPE, SHORTSTRING_TYPE).
     * AnsiString (STRING_TYPE) → RawByteString formal should rank higher than
     * AnsiString → ShortString, since AnsiString IS-A RawByteString.
     * This prevents Assign(t, AnsiString(s)) from picking the ShortString
     * overload (assign_t_ss) over the RawByteString overload (assign_t_rbs). */
    if (is_string_type(formal_tag) && is_string_type(actual_tag))
    {
        if (actual_tag == STRING_TYPE && formal_tag == STRING_TYPE)
        {
            /* Both are AnsiString-family (STRING_TYPE).  Check alias names:
             * AnsiString → RawByteString is exact, other cross-aliases are promotion. */
            const char *formal_alias = (formal_kgpc != NULL && formal_kgpc->type_alias != NULL)
                ? formal_kgpc->type_alias->alias_name : NULL;
            if (formal_alias != NULL &&
                pascal_identifier_equals(formal_alias, "RawByteString"))
                return semcheck_make_quality(MATCH_EXACT);
        }
        /* AnsiString → ShortString or vice versa: promotion (lossy conversion) */
        return semcheck_make_quality(MATCH_PROMOTION);
    }

    /* Plain Pointer is implicitly convertible to typed pointer parameters
     * such as PChar/PAnsiChar in RTL code (e.g. FpWrite/FpRead wrappers). */
    if (actual_tag == POINTER_TYPE && actual_kgpc != NULL &&
        actual_kgpc->kind == TYPE_KIND_PRIMITIVE &&
        actual_kgpc->info.primitive_type_tag == POINTER_TYPE &&
        formal_kgpc != NULL && formal_kgpc->kind == TYPE_KIND_POINTER)
    {
        MatchQuality q = semcheck_make_quality(MATCH_PROMOTION);
        if (kgpc_type_get_pointer_subtype_tag(formal_kgpc) != UNKNOWN_TYPE)
            q.exact_type_id = 1;
        return q;
    }

    if (actual_kgpc != NULL && formal_kgpc != NULL &&
        actual_kgpc->kind == TYPE_KIND_POINTER &&
        formal_kgpc->kind == TYPE_KIND_POINTER)
    {
        int actual_sub = kgpc_type_get_pointer_subtype_tag(actual_kgpc);
        int formal_sub = kgpc_type_get_pointer_subtype_tag(formal_kgpc);
        if (actual_sub != UNKNOWN_TYPE && formal_sub != UNKNOWN_TYPE)
        {
            if (actual_sub == formal_sub)
            {
                /* Both point to the same type tag, but for RECORD_TYPE we need
                 * to compare the actual record types more deeply to distinguish
                 * e.g. ^TObject from ^IUnknown */
                if (actual_sub == RECORD_TYPE)
                {
                    KgpcType *actual_inner = actual_kgpc->info.points_to;
                    KgpcType *formal_inner = formal_kgpc->info.points_to;
                    if (actual_inner != NULL && formal_inner != NULL &&
                        actual_inner->kind == TYPE_KIND_RECORD &&
                        formal_inner->kind == TYPE_KIND_RECORD)
                    {
                        struct RecordType *actual_rec = actual_inner->info.record_info;
                        struct RecordType *formal_rec = formal_inner->info.record_info;
                        if (actual_rec == formal_rec)
                        {
                            MatchQuality q = semcheck_make_quality(MATCH_EXACT);
                            q.exact_type_id = 1;
                            return q;
                        }
                        /* Check if types are compatible via inheritance */
                        if (are_types_compatible_for_assignment(formal_inner, actual_inner, symtab))
                        {
                            MatchQuality q = semcheck_make_quality(MATCH_PROMOTION);
                            q.exact_type_id = 1;
                            return q;
                        }
                        return semcheck_make_quality(MATCH_INCOMPATIBLE);
                    }
                    /* One or both inner types are degraded (TYPE_KIND_PRIMITIVE
                     * with RECORD_TYPE tag from cross-unit resolution).  Try to
                     * match via type_alias names on the outer pointer types
                     * before falling through to the default EXACT.
                     * Only apply this when at least one inner type is actually
                     * in degraded form (TYPE_KIND_PRIMITIVE with RECORD_TYPE tag). */
                    int actual_degraded = (actual_inner != NULL &&
                        actual_inner->kind == TYPE_KIND_PRIMITIVE &&
                        actual_inner->info.primitive_type_tag == RECORD_TYPE);
                    int formal_degraded = (formal_inner != NULL &&
                        formal_inner->kind == TYPE_KIND_PRIMITIVE &&
                        formal_inner->info.primitive_type_tag == RECORD_TYPE);
                    if (actual_inner != NULL && formal_inner != NULL &&
                        (actual_degraded || formal_degraded))
                    {
                        const char *actual_name = NULL;
                        const char *formal_name = NULL;
                        if (actual_kgpc->type_alias != NULL && actual_kgpc->type_alias->pointer_type_id != NULL)
                            actual_name = actual_kgpc->type_alias->pointer_type_id;
                        else if (actual_kgpc->type_alias != NULL && actual_kgpc->type_alias->alias_name != NULL)
                            actual_name = actual_kgpc->type_alias->alias_name;
                        if (formal_kgpc->type_alias != NULL && formal_kgpc->type_alias->pointer_type_id != NULL)
                            formal_name = formal_kgpc->type_alias->pointer_type_id;
                        else if (formal_kgpc->type_alias != NULL && formal_kgpc->type_alias->alias_name != NULL)
                            formal_name = formal_kgpc->type_alias->alias_name;
                        if (actual_name != NULL && formal_name != NULL)
                        {
                            if (pascal_identifier_equals(actual_name, formal_name))
                            {
                                MatchQuality q = semcheck_make_quality(MATCH_EXACT);
                                q.exact_type_id = 1;
                                return q;
                            }
                        }
                        /* Fall through to default EXACT — both point to
                         * RECORD_TYPE but can't distinguish; allow match. */
                    }
                }
                MatchQuality q = semcheck_make_quality(MATCH_EXACT);
                q.exact_type_id = 1;
                return q;
            }
            /* When pointer subtypes differ, check if the pointed-to types
             * are compatible integers (e.g. ^LongInt vs ^Cardinal/^DWORD).
             * FPC allows implicit conversion between pointers to same-sized
             * integer types in value parameter contexts. */
            if (is_integer_type(actual_sub) && is_integer_type(formal_sub))
                return semcheck_make_quality(MATCH_CONVERSION);
            /* Accept char/integer pointee conversions as implicit pointer
             * conversions (needed by RTL helpers using char/word aliases). */
            if ((is_integer_type(actual_sub) || actual_sub == CHAR_TYPE) &&
                (is_integer_type(formal_sub) || formal_sub == CHAR_TYPE))
            {
                return semcheck_make_quality(MATCH_CONVERSION);
            }
            /* Plain Pointer is compatible with typed pointer formals and vice versa.
             * This is required for FPC RTL wrappers like FpRead/FpWrite that pass
             * Pointer values to PAnsiChar syscall bindings. */
            if ((actual_sub == POINTER_TYPE && formal_sub != UNKNOWN_TYPE) ||
                (formal_sub == POINTER_TYPE && actual_sub != UNKNOWN_TYPE))
            {
                return semcheck_make_quality(MATCH_CONVERSION);
            }
            /* Also allow WideChar/UnicodeChar pointers to match word-like
             * integer pointers (used by RTL Unicode map helpers). */
            if (actual_kgpc->info.points_to != NULL && formal_kgpc->info.points_to != NULL)
            {
                int actual_is_widechar = semcheck_is_widechar_like_type(actual_kgpc->info.points_to);
                int formal_is_widechar = semcheck_is_widechar_like_type(formal_kgpc->info.points_to);
                if ((actual_is_widechar && is_integer_type(formal_sub)) ||
                    (formal_is_widechar && is_integer_type(actual_sub)))
                {
                    return semcheck_make_quality(MATCH_CONVERSION);
                }
            }
            /* Also try kgpc_type_conversion_rank on the pointed-to types */
            if (actual_kgpc->info.points_to != NULL && formal_kgpc->info.points_to != NULL)
            {
                int rank = kgpc_type_conversion_rank(actual_kgpc->info.points_to, formal_kgpc->info.points_to);
                if (rank >= 0)
                    return semcheck_make_quality(MATCH_CONVERSION);
            }
            /* "class of T" compatibility: when a class type name (^record)
             * is passed where a class reference (^^record = class of T)
             * is expected, allow it as a conversion.  The formal's pointee
             * is itself a pointer-to-record (the class instance type). */
            if (actual_sub == RECORD_TYPE && formal_sub == POINTER_TYPE &&
                actual_kgpc->info.points_to != NULL &&
                formal_kgpc->info.points_to != NULL &&
                formal_kgpc->info.points_to->kind == TYPE_KIND_POINTER &&
                formal_kgpc->info.points_to->info.points_to != NULL &&
                formal_kgpc->info.points_to->info.points_to->kind == TYPE_KIND_RECORD)
            {
                /* Check if the actual class is compatible with the formal's
                 * base class (the record inside the class reference). */
                KgpcType *actual_inner = actual_kgpc->info.points_to;
                KgpcType *formal_inner = formal_kgpc->info.points_to->info.points_to;
                if (actual_inner->kind == TYPE_KIND_RECORD &&
                    formal_inner->kind == TYPE_KIND_RECORD)
                {
                    if (actual_inner->info.record_info == formal_inner->info.record_info ||
                        are_types_compatible_for_assignment(formal_inner, actual_inner, symtab))
                    {
                        return semcheck_make_quality(MATCH_CONVERSION);
                    }
                }
            }
            return semcheck_make_quality(MATCH_INCOMPATIBLE);
        }
        if (actual_sub != UNKNOWN_TYPE || formal_sub != UNKNOWN_TYPE)
        {
            /* When an untyped Pointer is passed to a class/interface formal
             * (TObject, TClass, IInterface), score it worse than a typed
             * pointer formal (PTypeInfo, PChar, etc.).  FPC prefers plain
             * pointer conversions over class-pointer conversions.
             * Additionally, prefer class formals (TObject) over interface
             * formals (IInterface) to resolve ambiguity in Supports() etc. */
            if (actual_sub == UNKNOWN_TYPE && formal_sub == RECORD_TYPE &&
                formal_kgpc->info.points_to != NULL &&
                formal_kgpc->info.points_to->kind == TYPE_KIND_RECORD &&
                formal_kgpc->info.points_to->info.record_info != NULL &&
                record_type_is_class(formal_kgpc->info.points_to->info.record_info))
            {
                MatchQuality q = semcheck_make_quality(MATCH_CONVERSION);
                /* Interfaces rank worse than classes: when Pointer matches both
                 * Supports(TObject,...) and Supports(IInterface,...), prefer the
                 * class overload, matching FPC behavior. */
                if (formal_kgpc->info.points_to->info.record_info->is_interface)
                    q.integer_rank = 1;
                return q;
            }
            /* Untyped pointer -> typed non-class pointer: score as
             * PROMOTION so it ties with untyped-formal PROMOTION, then
             * the untyped-params tiebreaker picks the typed overload. */
            if (actual_sub == UNKNOWN_TYPE)
            {
                MatchQuality q = semcheck_make_quality(MATCH_PROMOTION);
                q.exact_type_id = 1;
                return q;
            }
            return semcheck_make_quality(MATCH_CONVERSION);
        }
        return semcheck_make_quality(MATCH_PROMOTION);
    }

    /* Integer to integer: always treat as promotion for overload ordering. */
    if (is_integer_type(actual_tag) && is_integer_type(formal_tag))
    {
        (void)is_integer_literal;
        return semcheck_make_quality(MATCH_PROMOTION);
    }

    /* Nil/Pointer should not implicitly convert to Boolean in overload resolution */
    if (actual_tag == POINTER_TYPE && formal_tag == BOOL)
        return semcheck_make_quality(MATCH_INCOMPATIBLE);

    if (actual_kgpc != NULL && formal_kgpc != NULL)
    {
        int rank = kgpc_type_conversion_rank(actual_kgpc, formal_kgpc);
        if (rank >= 0)
            return semcheck_match_from_rank(rank);
        /* Skip are_types_compatible_for_assignment when the actual is a
         * floating-point type and the formal is an integer type.  In Pascal,
         * Real→Integer is NOT an implicit conversion (requires Trunc/Round).
         * The generic compatibility check is too permissive here and would
         * allow Real→Integer as MATCH_CONVERSION, causing ambiguity between
         * Max(Extended) and Max(Integer) overloads. */
        if (!(is_real_family_type(actual_tag) && is_integer_type(formal_tag)) &&
            are_types_compatible_for_assignment(formal_kgpc, actual_kgpc, symtab))
            return semcheck_make_quality(MATCH_CONVERSION);
    }
    {
        const char *formal_type_id = semcheck_overload_record_type_id_from_kgpc(formal_kgpc);
        if (formal_type_id != NULL &&
            pascal_identifier_equals(formal_type_id, "tconstexprint") &&
            is_integer_type(actual_tag))
        {
            return semcheck_make_quality(MATCH_CONVERSION);
        }
        /* Reverse direction: actual is a record with := operator to the formal type.
         * E.g., Tconstexprint has operator := to qword/int64/bestreal, so passing
         * a Tconstexprint where an integer/real is expected should be allowed. */
        const char *actual_type_id = semcheck_overload_record_type_id_from_kgpc(actual_kgpc);
        if (actual_type_id != NULL &&
            semcheck_overload_type_is_recordish(actual_kgpc) &&
            (is_integer_type(formal_tag) || is_real_family_type(formal_tag)))
        {
            /* Check if the record type has a := operator to an integer or real type.
             * Operator IDs include the return type suffix, e.g.:
             *   Tconstexprint__op_assign_qword
             *   Tconstexprint__op_assign_int64
             *   Tconstexprint__op_assign_bestreal
             * Try common integer/real type suffixes. */
            static const char *int_suffixes[] = {
                "qword", "int64", "longint", "integer", "smallint",
                "word", "byte", "shortint", "cardinal", NULL
            };
            static const char *real_suffixes[] = {
                "bestreal", "real", "double", "single", "extended", NULL
            };
            const char **suffixes = is_integer_type(formal_tag) ? int_suffixes : real_suffixes;
            /* Also try both directions if the formal is numeric */
            if (is_integer_type(formal_tag) || is_real_family_type(formal_tag))
            {
                int has_conversion = 0;
                char assign_id[320];
                for (int si = 0; suffixes[si] != NULL && !has_conversion; ++si)
                {
                    snprintf(assign_id, sizeof(assign_id), "%s__op_assign_%s",
                        actual_type_id, suffixes[si]);
                    ListNode_t *cands = FindAllIdents(symtab, assign_id);
                    if (cands != NULL) { has_conversion = 1; DestroyList(cands); }
                }
                /* Also try real suffixes for integer formals (qword->int coercion) */
                if (!has_conversion && is_integer_type(formal_tag))
                {
                    for (int si = 0; real_suffixes[si] != NULL && !has_conversion; ++si)
                    {
                        snprintf(assign_id, sizeof(assign_id), "%s__op_assign_%s",
                            actual_type_id, real_suffixes[si]);
                        ListNode_t *cands = FindAllIdents(symtab, assign_id);
                        if (cands != NULL) { has_conversion = 1; DestroyList(cands); }
                    }
                }
                if (has_conversion)
                    return semcheck_make_quality(MATCH_CONVERSION);
            }
        }
    }
    if (semcheck_overload_has_record_assign_conversion(symtab, actual_kgpc, formal_kgpc, actual_tag))
        return semcheck_make_quality(MATCH_CONVERSION);
    if (is_integer_type(actual_tag) && is_real_family_type(formal_tag))
        return semcheck_make_quality(MATCH_CONVERSION);

    /* Boolean to integer: allow implicit conversion for builtins like FillChar.
     * We treat it as promotion so it's preferred over more complex conversions. */
    if (actual_tag == BOOL && is_integer_type(formal_tag))
    {
        return semcheck_make_quality(MATCH_PROMOTION);
    }
    if (is_string_type(formal_tag) && actual_tag == CHAR_TYPE)
        return semcheck_make_quality(MATCH_PROMOTION);
    if (actual_tag == CHAR_TYPE && formal_kgpc != NULL && kgpc_type_is_pointer(formal_kgpc))
    {
        KgpcType *points_to = formal_kgpc->info.points_to;
        if (points_to != NULL && points_to->kind == TYPE_KIND_PRIMITIVE &&
            points_to->info.primitive_type_tag == CHAR_TYPE)
            return semcheck_make_quality(MATCH_PROMOTION);
    }
    if (is_string_type(actual_tag) && formal_kgpc != NULL && kgpc_type_is_pointer(formal_kgpc))
    {
        KgpcType *points_to = formal_kgpc->info.points_to;
        if (points_to != NULL && points_to->kind == TYPE_KIND_PRIMITIVE &&
            points_to->info.primitive_type_tag == CHAR_TYPE)
            return semcheck_make_quality(MATCH_CONVERSION);
    }

    /* @Method produces POINTER_TYPE wrapping a TYPE_KIND_PROCEDURE.
     * Allow it to match a PROCEDURE formal parameter. */
    if (actual_tag == POINTER_TYPE && formal_tag == PROCEDURE)
    {
        if (actual_kgpc != NULL && kgpc_type_is_pointer(actual_kgpc) &&
            actual_kgpc->info.points_to != NULL &&
            actual_kgpc->info.points_to->kind == TYPE_KIND_PROCEDURE)
            return semcheck_make_quality(MATCH_PROMOTION);
        /* Also accept untyped @Method (pointer without inner type info) */
        if (actual_kgpc != NULL && kgpc_type_is_pointer(actual_kgpc) &&
            actual_kgpc->info.points_to == NULL)
            return semcheck_make_quality(MATCH_CONVERSION);
    }

    /* Class/interface TYPE name used as class reference (TClass) argument.
     * In FPC, bare type names like IComponentRef or TMyClass can be passed
     * where TClass is expected.  The actual is RECORD_TYPE (class/interface
     * record) while the formal is POINTER_TYPE -> class/record.  Treat as
     * a conversion match. */
    if (actual_tag == RECORD_TYPE && formal_tag == POINTER_TYPE &&
        actual_kgpc != NULL && actual_kgpc->kind == TYPE_KIND_RECORD &&
        formal_kgpc != NULL && formal_kgpc->kind == TYPE_KIND_POINTER)
    {
        /* Check that the formal points to a class/record type.
         * TClass is pointer-to-record; the inner type may be either
         * TYPE_KIND_RECORD or TYPE_KIND_PRIMITIVE with RECORD_TYPE tag. */
        int formal_points_to_class = 0;
        if (formal_kgpc->info.points_to != NULL)
        {
            KgpcType *inner = formal_kgpc->info.points_to;
            if (inner->kind == TYPE_KIND_RECORD)
            {
                formal_points_to_class = (inner->info.record_info != NULL) ?
                    record_type_is_class(inner->info.record_info) : 1;
            }
            else if (inner->kind == TYPE_KIND_PRIMITIVE &&
                     inner->info.primitive_type_tag == RECORD_TYPE)
            {
                formal_points_to_class = 1;
            }
        }
        else
        {
            /* Untyped pointer — may represent TClass */
            int sub = kgpc_type_get_pointer_subtype_tag(formal_kgpc);
            if (sub == RECORD_TYPE)
                formal_points_to_class = 1;
        }
        if (formal_points_to_class)
            return semcheck_make_quality(MATCH_CONVERSION);
    }

    /* Same-tag fallback: when both tags match but the KgpcType comparison
     * couldn't be done (one or both NULL, or kind mismatch due to degraded
     * types from cross-unit resolution), treat as MATCH_CONVERSION rather
     * than INCOMPATIBLE.  This prevents false overload rejections when types
     * from imported units degrade to generic forms (e.g. ^record, Enum).
     *
     * Only apply this when:
     * 1. At least one side has a NULL or degraded KgpcType, AND
     * 2. The tag is one known to degrade across unit boundaries. */
    if (actual_tag == formal_tag &&
        (actual_kgpc == NULL || formal_kgpc == NULL ||
         actual_kgpc->kind != formal_kgpc->kind) &&
        (actual_tag == RECORD_TYPE || actual_tag == ENUM_TYPE ||
         actual_tag == SET_TYPE || actual_tag == POINTER_TYPE ||
         actual_tag == EXTENDED_TYPE || actual_tag == VARIANT_TYPE))
        return semcheck_make_quality(MATCH_CONVERSION);

    return semcheck_make_quality(MATCH_INCOMPATIBLE);
}

static const char *semcheck_get_expr_decl_type_id(struct Expression *expr, SymTab_t *symtab)
{
    if (expr == NULL || symtab == NULL)
        return NULL;

    if (expr->array_element_type_id != NULL)
        return expr->array_element_type_id;

    if (expr->pointer_subtype_id != NULL)
        return expr->pointer_subtype_id;

    if (expr->type == EXPR_TYPECAST &&
        expr->expr_data.typecast_data.target_type_id != NULL)
        return expr->expr_data.typecast_data.target_type_id;

    if (expr->resolved_kgpc_type != NULL && expr->resolved_kgpc_type->type_alias != NULL)
    {
        if (expr->resolved_kgpc_type->type_alias->alias_name != NULL)
            return expr->resolved_kgpc_type->type_alias->alias_name;
        if (expr->resolved_kgpc_type->type_alias->target_type_id != NULL)
            return expr->resolved_kgpc_type->type_alias->target_type_id;
    }

    if (expr->type != EXPR_VAR_ID)
        return NULL;

    HashNode_t *node = NULL;
    if (FindSymbol(&node, symtab, expr->expr_data.id) == 0 || node == NULL || node->type == NULL)
        return NULL;

    if (node->type->type_alias != NULL && node->type->type_alias->alias_name != NULL)
        return node->type->type_alias->alias_name;
    if (node->type->type_alias != NULL && node->type->type_alias->target_type_id != NULL)
        return node->type->type_alias->target_type_id;
    if (node->type->kind == TYPE_KIND_RECORD &&
        node->type->info.record_info != NULL &&
        node->type->info.record_info->type_id != NULL)
        return node->type->info.record_info->type_id;
    return NULL;
}

static const char *semcheck_explicit_string_cast_target_id(struct Expression *expr)
{
    if (expr == NULL)
        return NULL;

    if (expr->type == EXPR_TYPECAST)
    {
        const char *target_id = expr->expr_data.typecast_data.target_type_id;
        int target_type = expr->expr_data.typecast_data.target_type;
        if (target_id != NULL && is_string_type(target_type))
            return target_id;
        return NULL;
    }

    if (expr->type != EXPR_FUNCTION_CALL)
        return NULL;

    if (expr->expr_data.function_call_data.id == NULL ||
        expr->expr_data.function_call_data.args_expr == NULL ||
        expr->expr_data.function_call_data.args_expr->next != NULL)
        return NULL;

    const char *call_id = expr->expr_data.function_call_data.id;
    if (pascal_identifier_equals(call_id, "AnsiString") ||
        pascal_identifier_equals(call_id, "RawByteString") ||
        pascal_identifier_equals(call_id, "String") ||
        pascal_identifier_equals(call_id, "ShortString") ||
        pascal_identifier_equals(call_id, "WideString") ||
        pascal_identifier_equals(call_id, "UnicodeString"))
        return call_id;

    return NULL;
}

static int semcheck_string_kind_from_type_id(const char *type_id)
{
    if (type_id == NULL)
        return 0;

    const char *id = type_id;
    if (id[0] == 'P' || id[0] == 'p')
        id = id + 1;

    if (pascal_identifier_equals(id, "WideChar") ||
        pascal_identifier_equals(id, "UnicodeChar") ||
        pascal_identifier_equals(id, "WideString") ||
        pascal_identifier_equals(id, "UnicodeString"))
        return 2;

    if (pascal_identifier_equals(id, "Char") ||
        pascal_identifier_equals(id, "AnsiChar") ||
        pascal_identifier_equals(id, "String") ||
        pascal_identifier_equals(id, "AnsiString") ||
        pascal_identifier_equals(id, "RawByteString") ||
        pascal_identifier_equals(id, "ShortString"))
        return 1;

    return 0;
}

static const char *semcheck_numeric_type_canonical_id(const char *type_id)
{
    if (type_id == NULL)
        return NULL;

    const char *id = semcheck_base_type_name(type_id);

    if (pascal_identifier_equals(id, "Byte") || pascal_identifier_equals(id, "UInt8"))
        return "UInt8";
    if (pascal_identifier_equals(id, "ShortInt") || pascal_identifier_equals(id, "Int8"))
        return "Int8";
    if (pascal_identifier_equals(id, "Word") || pascal_identifier_equals(id, "UInt16"))
        return "UInt16";
    if (pascal_identifier_equals(id, "SmallInt") || pascal_identifier_equals(id, "Int16"))
        return "Int16";
    if (pascal_identifier_equals(id, "LongWord") || pascal_identifier_equals(id, "Cardinal") ||
        pascal_identifier_equals(id, "DWord") || pascal_identifier_equals(id, "UInt32"))
        return "UInt32";
    if (pascal_identifier_equals(id, "Integer") || pascal_identifier_equals(id, "LongInt") ||
        pascal_identifier_equals(id, "Int32"))
        return "Int32";
    if (pascal_identifier_equals(id, "Int64"))
        return "Int64";
    if (pascal_identifier_equals(id, "QWord") || pascal_identifier_equals(id, "UInt64"))
        return "UInt64";

    return NULL;
}

static int semcheck_integer_promotion_rank(int actual_tag, KgpcType *actual_kgpc,
    int formal_tag, KgpcType *formal_kgpc, int is_integer_literal)
{
    long long actual_size = -1;
    long long formal_size = -1;
    int actual_unsigned = (actual_tag == BYTE_TYPE || actual_tag == WORD_TYPE ||
        actual_tag == LONGWORD_TYPE || actual_tag == QWORD_TYPE);
    int formal_unsigned = (formal_tag == BYTE_TYPE || formal_tag == WORD_TYPE ||
        formal_tag == LONGWORD_TYPE || formal_tag == QWORD_TYPE);

    if (actual_kgpc != NULL)
        actual_size = kgpc_type_sizeof(actual_kgpc);
    if (formal_kgpc != NULL)
        formal_size = kgpc_type_sizeof(formal_kgpc);

    if (actual_size <= 0)
    {
        switch (actual_tag)
        {
            case BYTE_TYPE: actual_size = 1; break;
            case WORD_TYPE: actual_size = 2; break;
            case INT_TYPE:
            case LONGINT_TYPE:
            case LONGWORD_TYPE: actual_size = 4; break;
            case INT64_TYPE:
            case QWORD_TYPE: actual_size = 8; break;
            default: actual_size = -1; break;
        }
    }

    if (formal_size <= 0)
    {
        switch (formal_tag)
        {
            case BYTE_TYPE: formal_size = 1; break;
            case WORD_TYPE: formal_size = 2; break;
            case INT_TYPE:
            case LONGINT_TYPE:
            case LONGWORD_TYPE: formal_size = 4; break;
            case INT64_TYPE:
            case QWORD_TYPE: formal_size = 8; break;
            default: formal_size = -1; break;
        }
    }

    if (actual_size > 0 && formal_size > 0)
    {
        int actual_rank = -1;
        int formal_rank = -1;
        if (actual_size == 1) actual_rank = 0;
        else if (actual_size == 2) actual_rank = 1;
        else if (actual_size == 4) actual_rank = 2;
        else if (actual_size == 8) actual_rank = 3;

        if (formal_size == 1) formal_rank = 0;
        else if (formal_size == 2) formal_rank = 1;
        else if (formal_size == 4) formal_rank = 2;
        else if (formal_size == 8) formal_rank = 3;

        if (formal_size == actual_size)
        {
            if (actual_unsigned == formal_unsigned)
                return 0;
            return is_integer_literal ? 0 : 1;
        }
        if (formal_size > actual_size && actual_rank != -1 && formal_rank != -1)
            return 1 + (formal_rank - actual_rank);
        if (formal_size > actual_size)
            return 2;
        if (actual_rank != -1 && formal_rank != -1)
        {
            int base = 10 + (actual_rank - formal_rank);
            if (actual_size > formal_size && formal_size == 4)
            {
                if (formal_tag == LONGINT_TYPE)
                    base -= 1;
            }
            return base;
        }
        return 3;
    }

    if (actual_tag == formal_tag)
        return 0;

    if (is_integer_literal)
    {
        switch (formal_tag)
        {
            case INT_TYPE:
            case LONGINT_TYPE:
                return 0;
            case INT64_TYPE:
                return 1;
            default:
                return 2;
        }
    }

    return 1;
}

/*
 * Compare two MatchQuality values.
 * Returns -1 if a is better, 1 if b is better, 0 if equal.
 */
static int compare_single_quality(const MatchQuality *a, const MatchQuality *b)
{
    if (a->kind < b->kind)
        return -1;
    if (a->kind > b->kind)
        return 1;
    if (a->exact_type_id != b->exact_type_id)
        return a->exact_type_id > b->exact_type_id ? -1 : 1;
    if (a->integer_rank < b->integer_rank)
        return -1;
    if (a->integer_rank > b->integer_rank)
        return 1;
    if (a->string_rank < b->string_rank)
        return -1;
    if (a->string_rank > b->string_rank)
        return 1;
    return 0;
}

/*
 * Compare conversion sequences for two candidates.
 * Returns -1 if a is better, 1 if b is better, 0 if neither is better.
 *
 * Per standard overload resolution rules:
 * A is better than B if for every argument conv[a][i] <= conv[b][i]
 * and for at least one argument conv[a][i] < conv[b][i].
 * Otherwise ambiguous (return 0).
 */
static int semcheck_compare_match_quality(int arg_count,
    const MatchQuality *a, const MatchQuality *b)
{
    int a_exact = 0;
    int b_exact = 0;
    int a_promotion = 0;
    int b_promotion = 0;
    int a_conversion = 0;
    int b_conversion = 0;
    int a_incompatible = 0;
    int b_incompatible = 0;
    int a_string_rank = 0;
    int b_string_rank = 0;
    int a_integer_rank = 0;
    int b_integer_rank = 0;

    for (int i = 0; i < arg_count; i++)
    {
        switch (a[i].kind)
        {
            case MATCH_EXACT:
                a_exact++;
                break;
            case MATCH_PROMOTION:
                a_promotion++;
                break;
            case MATCH_CONVERSION:
                a_conversion++;
                break;
            case MATCH_INCOMPATIBLE:
                a_incompatible++;
                break;
        }
        switch (b[i].kind)
        {
            case MATCH_EXACT:
                b_exact++;
                break;
            case MATCH_PROMOTION:
                b_promotion++;
                break;
            case MATCH_CONVERSION:
                b_conversion++;
                break;
            case MATCH_INCOMPATIBLE:
                b_incompatible++;
                break;
        }
        a_string_rank += a[i].string_rank;
        b_string_rank += b[i].string_rank;
        a_integer_rank += a[i].integer_rank;
        b_integer_rank += b[i].integer_rank;
    }

    if (a_incompatible != b_incompatible)
        return a_incompatible < b_incompatible ? -1 : 1;
    if (a_conversion != b_conversion)
        return a_conversion < b_conversion ? -1 : 1;
    if (a_promotion != b_promotion)
        return a_promotion < b_promotion ? -1 : 1;
    if (a_exact != b_exact)
        return a_exact > b_exact ? -1 : 1;
    if (a_string_rank != b_string_rank)
        return a_string_rank < b_string_rank ? -1 : 1;
    if (a_integer_rank != b_integer_rank)
        return a_integer_rank < b_integer_rank ? -1 : 1;

    /* Same conversion-class histogram: fall back to the per-argument compare
     * so exact subtype/id matches can still dominate deterministically. */
    {
        int a_better_count = 0;
        int b_better_count = 0;
        int a_worst = 0, b_worst = 0;  /* worst (highest) kind for each */
        for (int i = 0; i < arg_count; i++)
        {
            int cmp = compare_single_quality(&a[i], &b[i]);
            if (cmp < 0)
                a_better_count++;
            else if (cmp > 0)
                b_better_count++;
            if ((int)a[i].kind > a_worst) a_worst = (int)a[i].kind;
            if ((int)b[i].kind > b_worst) b_worst = (int)b[i].kind;
        }
        if (a_better_count > 0 && b_better_count == 0)
            return -1;
        if (b_better_count > 0 && a_better_count == 0)
            return 1;
        /* Cross-argument tradeoff: neither Pareto-dominates.
         * Prefer the candidate with better worst-case argument. */
        if (a_worst != b_worst)
            return a_worst < b_worst ? -1 : 1;
        /* Prefer the candidate that is strictly better on more arguments. */
        if (a_better_count != b_better_count)
            return a_better_count > b_better_count ? -1 : 1;
    }

    return 0;
}

/* Count untyped var params in a candidate's parameter list */
static int semcheck_count_untyped_params(HashNode_t *candidate)
{
    if (candidate == NULL || candidate->type == NULL)
        return 0;
    int count = 0;
    ListNode_t *params = kgpc_type_get_procedure_params(candidate->type);
    while (params != NULL)
    {
        Tree_t *decl = (Tree_t *)params->cur;
        if (decl != NULL && decl->type == TREE_VAR_DECL)
        {
            struct Var *var_info = &decl->tree_data.var_decl_data;
            if (var_info->is_untyped_param ||
                (var_info->type == UNKNOWN_TYPE && var_info->type_id == NULL &&
                 var_info->inline_record_type == NULL))
                count++;
        }
        else if (decl != NULL && decl->type == TREE_ARR_DECL)
        {
            struct Array *arr_info = &decl->tree_data.arr_decl_data;
            if (arr_info->type == UNKNOWN_TYPE && arr_info->type_id == NULL)
                count++;
        }
        params = params->next;
    }
    return count;
}


int semcheck_resolve_overload(HashNode_t **best_match_out,
    int *best_rank_out,
    int *num_best_out,
    ListNode_t *overload_candidates,
    ListNode_t *args_given,
    SymTab_t *symtab,
    struct Expression *call_expr,
    int max_scope_lev,
    int prefer_non_builtin)
{
    if (best_match_out != NULL)
        *best_match_out = NULL;
    if (best_rank_out != NULL)
        *best_rank_out = 0;
    if (num_best_out != NULL)
        *num_best_out = 0;

    if (overload_candidates == NULL)
        return 1;

    HashNode_t *best_match = NULL;
    MatchQuality *best_qualities = NULL;
    int best_missing = 0;
    int num_best = 0;
    int given_count = ListLength(args_given);

    ListNode_t *slow = overload_candidates;
    ListNode_t *fast = overload_candidates;
    int guard = 0;
    const int guard_limit = 100000;

    for (ListNode_t *cur = overload_candidates; cur != NULL; cur = cur->next)
    {
        guard++;
        if (guard > guard_limit) {
            fprintf(stderr, "ERROR: semcheck_resolve_overload exceeded guard limit (%d); possible cycle in overload candidates list (node=%p).\n",
                guard_limit, (void*)cur);
            break;
        }
        if (fast != NULL && fast->next != NULL) {
            fast = fast->next->next;
            slow = slow ? slow->next : NULL;
            if (fast != NULL && slow == fast) {
                fprintf(stderr, "ERROR: Cycle detected in overload candidates list (node=%p).\n",
                    (void*)cur);
                break;
            }
        }
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate == NULL ||
            (candidate->hash_type != HASHTYPE_FUNCTION &&
             candidate->hash_type != HASHTYPE_PROCEDURE &&
             candidate->hash_type != HASHTYPE_BUILTIN_PROCEDURE) ||
            candidate->type == NULL)
            continue;
        if (prefer_non_builtin && semcheck_candidate_is_builtin(symtab, candidate))
            continue;

        ListNode_t *candidate_args = kgpc_type_get_procedure_params(candidate->type);
        int total_params = semcheck_count_total_params(candidate_args);
        int required_params = semcheck_count_required_params(candidate_args);
        int allow_implicit_leading_self = 0;
        int allow_implicit_leading_type_qualifier = 0;

        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] semcheck_resolve_overload: candidate %s args=%d required=%d given=%d\n",
                candidate->id, total_params, required_params, given_count);
        }

        if (candidate_args != NULL && args_given != NULL && args_given->cur != NULL)
        {
            Tree_t *first_formal = (Tree_t *)candidate_args->cur;
            const char *first_formal_name = NULL;
            if (first_formal != NULL && first_formal->type == TREE_VAR_DECL &&
                first_formal->tree_data.var_decl_data.ids != NULL)
                first_formal_name = (const char *)first_formal->tree_data.var_decl_data.ids->cur;
            else if (first_formal != NULL && first_formal->type == TREE_ARR_DECL &&
                first_formal->tree_data.arr_decl_data.ids != NULL)
                first_formal_name = (const char *)first_formal->tree_data.arr_decl_data.ids->cur;

            struct Expression *first_actual = (struct Expression *)args_given->cur;
            if (first_formal_name != NULL && pascal_identifier_equals(first_formal_name, "Self") &&
                first_actual != NULL && first_actual->type == EXPR_VAR_ID &&
                first_actual->expr_data.id != NULL &&
                pascal_identifier_equals(first_actual->expr_data.id, "Self") &&
                total_params > 0 && given_count == total_params - 1 &&
                given_count < required_params)
            {
                allow_implicit_leading_self = 1;
            }

            if (!allow_implicit_leading_self &&
                first_actual != NULL && first_actual->type == EXPR_VAR_ID &&
                first_actual->expr_data.id != NULL &&
                given_count == total_params + 1)
            {
                int candidate_is_owner_method = 0;
                if (candidate->owner_class != NULL &&
                    pascal_identifier_equals(candidate->owner_class, first_actual->expr_data.id))
                    candidate_is_owner_method = 1;
                else if (candidate->mangled_id != NULL)
                {
                    size_t owner_len = strlen(first_actual->expr_data.id);
                    if (strncasecmp(candidate->mangled_id, first_actual->expr_data.id, owner_len) == 0 &&
                        candidate->mangled_id[owner_len] == '_' &&
                        candidate->mangled_id[owner_len + 1] == '_')
                        candidate_is_owner_method = 1;
                }
                if (candidate_is_owner_method &&
                    !(first_formal_name != NULL && pascal_identifier_equals(first_formal_name, "Self")))
                    allow_implicit_leading_type_qualifier = 1;
            }
        }

        int arity_matches = ((given_count >= required_params && given_count <= total_params) ||
            (total_params == 0 && given_count > 0 &&
             candidate->type != NULL &&
             candidate->type->info.proc_info.definition == NULL) ||
            (candidate->is_varargs && given_count >= required_params));
        if (!arity_matches && allow_implicit_leading_self)
        {
            int adj_total = total_params - 1;
            int adj_required = required_params > 0 ? required_params - 1 : 0;
            if (given_count >= adj_required && given_count <= adj_total)
                arity_matches = 1;
        }
        /* When the first formal is Self and we have exactly one fewer arg than
         * params, Self was likely consumed by a ClassName.Method() rewrite.
         * Skip Self in both arity and matching. */
        if (!arity_matches && !allow_implicit_leading_self &&
            candidate_args != NULL && total_params > 0 &&
            given_count == total_params - 1)
        {
            Tree_t *first_formal_check = (Tree_t *)candidate_args->cur;
            const char *ff_name = NULL;
            if (first_formal_check != NULL && first_formal_check->type == TREE_VAR_DECL &&
                first_formal_check->tree_data.var_decl_data.ids != NULL)
                ff_name = (const char *)first_formal_check->tree_data.var_decl_data.ids->cur;
            if (ff_name != NULL && pascal_identifier_equals(ff_name, "Self"))
            {
                allow_implicit_leading_self = 1;
                int adj_total = total_params - 1;
                int adj_required = required_params > 0 ? required_params - 1 : 0;
                if (given_count >= adj_required && given_count <= adj_total)
                    arity_matches = 1;
            }
        }
        if (!arity_matches && allow_implicit_leading_type_qualifier)
        {
            int adj_total = total_params;
            int adj_required = required_params;
            int given_effective = given_count - 1;
            if (given_effective >= adj_required && given_effective <= adj_total)
                arity_matches = 1;
        }
        if (!arity_matches)
            continue;

        MatchQuality *qualities = NULL;
        if (given_count > 0)
        {
            qualities = (MatchQuality *)calloc((size_t)given_count, sizeof(MatchQuality));
            if (qualities == NULL)
                return 3;
        }

        int candidate_valid = 1;
        ListNode_t *formal_args = candidate_args;
        if (allow_implicit_leading_self && formal_args != NULL)
            formal_args = formal_args->next;
        ListNode_t *call_args = args_given;
        int arg_index = 0;
        if (allow_implicit_leading_type_qualifier && call_args != NULL)
        {
            if (qualities != NULL && given_count > 0)
                qualities[0] = semcheck_make_quality(MATCH_EXACT);
            arg_index = 1;
            call_args = call_args->next;
        }

        while (formal_args != NULL && call_args != NULL)
        {
            Tree_t *formal_decl = (Tree_t *)formal_args->cur;
            struct Expression *arg_expr = (struct Expression *)call_args->cur;

            if (arg_expr != NULL && arg_expr->type == EXPR_RELOP &&
                arg_expr->expr_data.relop_data.type == EQ &&
                arg_expr->expr_data.relop_data.left != NULL &&
                arg_expr->expr_data.relop_data.left->type == EXPR_VAR_ID &&
                arg_expr->expr_data.relop_data.right != NULL)
            {
                const char *formal_name = NULL;
                if (formal_decl != NULL && formal_decl->type == TREE_VAR_DECL &&
                    formal_decl->tree_data.var_decl_data.ids != NULL)
                    formal_name = (const char *)formal_decl->tree_data.var_decl_data.ids->cur;
                else if (formal_decl != NULL && formal_decl->type == TREE_ARR_DECL &&
                    formal_decl->tree_data.arr_decl_data.ids != NULL)
                    formal_name = (const char *)formal_decl->tree_data.arr_decl_data.ids->cur;

                const char *given_name = arg_expr->expr_data.relop_data.left->expr_data.id;
                if (formal_name != NULL && given_name != NULL)
                {
                    if (pascal_identifier_equals(formal_name, given_name))
                    {
                        struct Expression *rhs_expr = arg_expr->expr_data.relop_data.right;
                        int rhs_type = UNKNOWN_TYPE;
                        KgpcType *rhs_kgpc_type = NULL;
                        semcheck_expr_with_type(&rhs_kgpc_type, symtab, rhs_expr, max_scope_lev, NO_MUTATE);
                        rhs_type = semcheck_tag_from_kgpc(rhs_kgpc_type);
                        if (semcheck_named_arg_type_compatible(formal_decl, rhs_expr, rhs_type, symtab))
                            arg_expr = rhs_expr;
                    }
                    else if (semcheck_param_list_contains_name(candidate_args, given_name))
                    {
                        candidate_valid = 0;
                        break;
                    }
                }
            }

            int formal_is_array_decl = 0;
            struct TypeAlias *formal_inline_alias = NULL;
            if (formal_decl != NULL)
            {
                if (formal_decl->type == TREE_ARR_DECL)
                    formal_is_array_decl = 1;
                else if (formal_decl->type == TREE_VAR_DECL)
                {
                    formal_inline_alias = formal_decl->tree_data.var_decl_data.inline_type_alias;
                    if (formal_inline_alias != NULL && formal_inline_alias->is_array)
                        formal_is_array_decl = 1;
                }
            }
            if (!formal_is_array_decl && formal_decl != NULL)
            {
                int owns_probe = 0;
                KgpcType *probe = resolve_type_from_vardecl(formal_decl, symtab, &owns_probe);
                if (probe != NULL && (kgpc_type_is_array(probe) || kgpc_type_is_array_of_const(probe)))
                    formal_is_array_decl = 1;
                if (owns_probe && probe != NULL)
                    destroy_kgpc_type(probe);
            }

            int arg_is_array = 0;
            if (arg_expr != NULL &&
                (arg_expr->type == EXPR_SET || arg_expr->type == EXPR_ARRAY_LITERAL ||
                 arg_expr->array_element_type != UNKNOWN_TYPE ||
                 (arg_expr->array_element_type_id != NULL && arg_expr->is_array_expr)))
            {
                arg_is_array = 1;
            }
            else if (arg_expr != NULL)
            {
                int owns_probe = 0;
                KgpcType *arg_probe = NULL;
                semcheck_resolve_arg_kgpc_type(arg_expr, symtab, max_scope_lev, &owns_probe, &arg_probe);
                if (arg_probe != NULL && (kgpc_type_is_array(arg_probe) || kgpc_type_is_array_of_const(arg_probe)))
                    arg_is_array = 1;
                if (owns_probe && arg_probe != NULL)
                    destroy_kgpc_type(arg_probe);
            }

            if (formal_is_array_decl && arg_is_array)
            {
                MatchQuality quality = semcheck_make_quality(MATCH_PROMOTION);
                int owns_formal = 0;
                KgpcType *formal_kgpc = resolve_type_from_vardecl(formal_decl, symtab, &owns_formal);
                if (formal_kgpc == NULL && formal_inline_alias != NULL)
                {
                    formal_kgpc = create_kgpc_type_from_type_alias(formal_inline_alias, symtab, 0);
                    if (formal_inline_alias->kgpc_type == NULL && formal_kgpc != NULL)
                        owns_formal = 1;
                }
                if (formal_kgpc != NULL && formal_kgpc->kind == TYPE_KIND_ARRAY_OF_CONST)
                {
                    quality = semcheck_make_quality(MATCH_PROMOTION);
                }
                else if (formal_kgpc != NULL && formal_kgpc->kind == TYPE_KIND_ARRAY)
                {
                    KgpcType *formal_elem = kgpc_type_get_array_element_type_resolved(formal_kgpc, symtab);
                    int formal_elem_tag = semcheck_array_elem_legacy_tag(formal_elem);
                    int arg_elem_tag = UNKNOWN_TYPE;
                    int arg_is_literal = 0;
                    if (arg_expr->type == EXPR_SET || arg_expr->type == EXPR_ARRAY_LITERAL)
                    {
                        arg_elem_tag = semcheck_infer_literal_elem_type(arg_expr);
                        arg_is_literal = 1;
                    }
                    else if (arg_expr->array_element_type != UNKNOWN_TYPE)
                        arg_elem_tag = arg_expr->array_element_type;
                    else if (arg_expr->array_element_type_id != NULL)
                    {
                        arg_elem_tag = semcheck_map_builtin_type_name(symtab, arg_expr->array_element_type_id);
                        if (arg_elem_tag == UNKNOWN_TYPE)
                        {
                            HashNode_t *type_node = NULL;
                            if (FindSymbol(&type_node, symtab, arg_expr->array_element_type_id) != 0 && type_node != NULL)
                                set_type_from_hashtype(&arg_elem_tag, type_node);
                        }
                    }

                    int owns_arg = 0;
                    KgpcType *arg_kgpc = NULL;
                    if (arg_elem_tag == UNKNOWN_TYPE && arg_expr != NULL)
                    {
                        semcheck_resolve_arg_kgpc_type(arg_expr, symtab, max_scope_lev,
                            &owns_arg, &arg_kgpc);
                        if (arg_kgpc != NULL && arg_kgpc->kind == TYPE_KIND_ARRAY)
                        {
                            KgpcType *arg_elem = kgpc_type_get_array_element_type_resolved(arg_kgpc, symtab);
                            int tag = semcheck_array_elem_legacy_tag(arg_elem);
                            if (tag != UNKNOWN_TYPE)
                                arg_elem_tag = tag;
                            if (arg_elem_tag == UNKNOWN_TYPE && arg_elem != NULL)
                                arg_elem_tag = semcheck_tag_from_kgpc(arg_elem);
                        }
                    }
                    else if (arg_kgpc != NULL && arg_kgpc->kind == TYPE_KIND_ARRAY)
                    {
                        KgpcType *arg_elem = kgpc_type_get_array_element_type_resolved(arg_kgpc, symtab);
                        if (arg_elem != NULL)
                        {
                            int elem_tag = semcheck_tag_from_kgpc(arg_elem);
                            if (elem_tag == SHORTSTRING_TYPE && arg_elem_tag == CHAR_TYPE)
                                arg_elem_tag = SHORTSTRING_TYPE;
                        }
                    }

                    if (arg_elem_tag != UNKNOWN_TYPE &&
                        (formal_elem_tag != UNKNOWN_TYPE || formal_elem != NULL))
                    {
                        int rank = -1;
                        if (formal_elem_tag != UNKNOWN_TYPE)
                        {
                            if (arg_elem_tag != UNKNOWN_TYPE && arg_elem_tag == formal_elem_tag)
                            {
                                rank = 0;
                            }
                            else
                            {
                            KgpcType *arg_elem = create_primitive_type(arg_elem_tag);
                            KgpcType *formal_elem_prim = create_primitive_type(formal_elem_tag);
                            rank = kgpc_type_conversion_rank(arg_elem, formal_elem_prim);
                            destroy_kgpc_type(arg_elem);
                            destroy_kgpc_type(formal_elem_prim);
                            }
                        }
                        else
                        {
                            KgpcType *arg_elem = create_primitive_type(arg_elem_tag);
                            rank = kgpc_type_conversion_rank(arg_elem, formal_elem);
                            destroy_kgpc_type(arg_elem);
                        }
                        if (rank < 0)
                        {
                            if (!arg_is_literal &&
                                arg_elem_tag == SHORTSTRING_TYPE &&
                                formal_elem_tag == CHAR_TYPE)
                            {
                                /* Allow ShortString arrays to match Char arrays,
                                   but make it worse than exact to prefer real Char arrays. */
                                rank = 1;
                            }
                            else if (!arg_is_literal &&
                                arg_elem_tag == SHORTSTRING_TYPE &&
                                formal_elem_tag == SHORTSTRING_TYPE)
                            {
                                rank = 1;
                            }
                        }

                        if (rank < 0 && formal_elem != NULL && arg_elem_tag == CHAR_TYPE &&
                            formal_elem->kind == TYPE_KIND_ARRAY &&
                            formal_elem->info.array_info.element_type != NULL &&
                            formal_elem->info.array_info.element_type->kind == TYPE_KIND_PRIMITIVE &&
                            formal_elem->info.array_info.element_type->info.primitive_type_tag == CHAR_TYPE)
                        {
                            rank = 1;
                        }
                        if (rank < 0)
                        {
                            if (arg_is_literal)
                            {
                                candidate_valid = 0;
                                if (owns_formal && formal_kgpc != NULL)
                                    destroy_kgpc_type(formal_kgpc);
                                break;
                            }
                            quality = semcheck_make_quality(MATCH_CONVERSION);
                        }
                        else
                        {
                            quality = semcheck_match_from_rank(rank);
                        }
                        if (arg_expr->type == EXPR_SET)
                        {
                            if (quality.kind == MATCH_EXACT)
                                quality = semcheck_make_quality(MATCH_PROMOTION);
                            else if (quality.kind == MATCH_PROMOTION)
                                quality = semcheck_make_quality(MATCH_CONVERSION);
                        }
                        if (formal_elem_tag != UNKNOWN_TYPE && formal_elem_tag == arg_elem_tag)
                            quality.exact_type_id = 1;

                    }

                    if (owns_arg && arg_kgpc != NULL)
                        destroy_kgpc_type(arg_kgpc);
                }

                if (owns_formal && formal_kgpc != NULL)
                    destroy_kgpc_type(formal_kgpc);

                if (quality.kind == MATCH_INCOMPATIBLE)
                {
                    candidate_valid = 0;
                    break;
                }

                if (arg_expr != NULL)
                {
                    const char *formal_id = semcheck_get_param_type_id(formal_decl);
                    const char *arg_decl_id = semcheck_get_expr_decl_type_id(arg_expr, symtab);
                    if (formal_id != NULL && arg_decl_id != NULL &&
                        pascal_identifier_equals(formal_id, arg_decl_id))
                        quality.exact_type_id = 1;
                    if (!quality.exact_type_id && formal_id != NULL && arg_decl_id != NULL)
                    {
                        const char *formal_num = semcheck_numeric_type_canonical_id(formal_id);
                        const char *arg_num = semcheck_numeric_type_canonical_id(arg_decl_id);
                        if (formal_num != NULL && arg_num != NULL &&
                            pascal_identifier_equals(formal_num, arg_num))
                            quality.exact_type_id = 1;
                    }
                    if (formal_id != NULL)
                    {
                        KgpcType *arg_expr_kgpc = arg_expr->resolved_kgpc_type;
                        int arg_expr_tag = (arg_expr_kgpc != NULL)
                            ? semcheck_tag_from_kgpc(arg_expr_kgpc)
                            : UNKNOWN_TYPE;
                        int formal_kind = semcheck_string_kind_from_type_id(formal_id);
                        int arg_kind = (arg_decl_id != NULL)
                            ? semcheck_string_kind_from_type_id(arg_decl_id)
                            : 0;
                        if (arg_kind == 0 && arg_expr_kgpc != NULL)
                        {
                            if (semcheck_is_widechar_like_type(arg_expr_kgpc))
                                arg_kind = 2;
                            else if (kgpc_type_is_char(arg_expr_kgpc))
                                arg_kind = 1;
                        }
                        if (formal_kind != 0 && arg_kind != 0)
                        {
                            if (formal_kind == arg_kind)
                                quality.exact_type_id = 1;
                            else
                            {
                                if (quality.kind == MATCH_EXACT)
                                    quality.kind = MATCH_PROMOTION;
                                quality.string_rank = 1;
                            }
                        }
                        else if (formal_kind == 2 &&
                                 (arg_expr_tag == CHAR_TYPE || arg_expr_tag == BYTE_TYPE))
                        {
                            if (quality.kind == MATCH_EXACT)
                                quality.kind = MATCH_PROMOTION;
                            quality.string_rank = 1;
                        }
                    }
                }

                qualities[arg_index++] = quality;
            }
            else
            {
                if (semcheck_prepare_array_literal_argument(formal_decl, arg_expr,
                        symtab, max_scope_lev, call_expr->line_num) != 0)
                {
                    free(qualities);
                    if (best_match_out != NULL)
                        *best_match_out = NULL;
                    return 3;
                }
                if (semcheck_prepare_record_constructor_argument(formal_decl, arg_expr,
                        symtab, max_scope_lev, call_expr->line_num) != 0)
                {
                    free(qualities);
                    if (best_match_out != NULL)
                        *best_match_out = NULL;
                    return 3;
                }

                int owns_formal = 0;
                int owns_arg = 0;
                KgpcType *formal_kgpc = resolve_type_from_vardecl(formal_decl, symtab, &owns_formal);
                KgpcType *arg_kgpc = NULL;
                int arg_tag = semcheck_resolve_arg_kgpc_type(arg_expr, symtab, max_scope_lev, &owns_arg, &arg_kgpc);

                int is_var_param = 0;
                if (formal_decl != NULL && formal_decl->type == TREE_VAR_DECL)
                    is_var_param = formal_decl->tree_data.var_decl_data.is_var_param;

                int formal_tag = resolve_param_type(formal_decl, symtab);

                /* If formal_kgpc is a bare real type (no alias name), try to
                 * replace it with the symbol-table type which carries proper
                 * size info (Single=4, Double=8, Extended=10). */
                if (formal_kgpc != NULL &&
                    formal_kgpc->kind == TYPE_KIND_PRIMITIVE &&
                    is_real_family_type(formal_kgpc->info.primitive_type_tag) &&
                    (formal_kgpc->type_alias == NULL ||
                     formal_kgpc->type_alias->alias_name == NULL))
                {
                    const char *ftype_id = semcheck_get_param_type_id(formal_decl);
                    if (ftype_id != NULL)
                    {
                        HashNode_t *ftype_node = NULL;
                        if (FindSymbol(&ftype_node, symtab, ftype_id) &&
                            ftype_node != NULL &&
                            ftype_node->hash_type == HASHTYPE_TYPE &&
                            ftype_node->type != NULL &&
                            ftype_node->type->kind == TYPE_KIND_PRIMITIVE)
                        {
                                if (owns_formal)
                                destroy_kgpc_type(formal_kgpc);
                            formal_kgpc = ftype_node->type;
                            kgpc_type_retain(formal_kgpc);
                            owns_formal = 1;
                        }
                    }
                }

                if (formal_kgpc == NULL)
                {
                    switch (formal_tag)
                    {
                        case INT_TYPE:
                        case LONGINT_TYPE:
                        case INT64_TYPE:
                        case REAL_TYPE:
                        case STRING_TYPE:
                        case SHORTSTRING_TYPE:
                        case CHAR_TYPE:
                        case BOOL:
                        case ENUM_TYPE:
                        case SET_TYPE:
                        case POINTER_TYPE:
                        case FILE_TYPE:
                        case TEXT_TYPE:
                        case BYTE_TYPE:
                        case WORD_TYPE:
                        case LONGWORD_TYPE:
                        case QWORD_TYPE:
                        case PROCEDURE:
                            formal_kgpc = create_primitive_type(formal_tag);
                            owns_formal = 1;
                            break;
                        default:
                            break;
                    }
                }

                semcheck_bind_set_literal_to_formal_type(arg_expr, formal_kgpc,
                    &owns_arg, &arg_kgpc, &arg_tag);

                MatchQuality quality = semcheck_make_quality(MATCH_INCOMPATIBLE);

                /* When the formal parameter is an array type but the argument
                 * is not an array, the candidate is generally incompatible.
                 * Without this check, resolve_param_type returns the *element*
                 * type (e.g. BYTE_TYPE for "array of Byte") and classify_match
                 * may spuriously promote a scalar argument (e.g. Boolean) to
                 * an integer match, hiding the correct untyped overload.
                 *
                 * Exception 1: string/char arguments can match "array of Char"
                 * parameters (FPC auto-converts the string to a char array).
                 * Exception 2: untyped arguments (from untyped const/var params)
                 * can match any typed formal, including arrays. */
                if (formal_is_array_decl && !arg_is_array &&
                    !(arg_tag == UNKNOWN_TYPE && arg_kgpc == NULL))
                {
                    int allow_string_to_char_array = 0;
                    int allow_pointer_backed_array = 0;
                    if (is_string_type(arg_tag) || arg_tag == CHAR_TYPE)
                    {
                        /* Check if formal element type is Char */
                        KgpcType *formal_elem = NULL;
                        if (formal_kgpc != NULL && formal_kgpc->kind == TYPE_KIND_ARRAY)
                            formal_elem = kgpc_type_get_array_element_type_resolved(
                                formal_kgpc, symtab);
                        int elem_tag = formal_elem ? semcheck_tag_from_kgpc(formal_elem)
                                                   : formal_tag;
                        if (elem_tag == CHAR_TYPE)
                            allow_string_to_char_array = 1;
                    }
                    /* Generic/dynamic array values can flow through as pointer-typed
                     * expressions (notably with specialize TArray<T> aliases).  Do not
                     * reject these before quality classification. */
                    if (!allow_string_to_char_array &&
                        (arg_tag == POINTER_TYPE ||
                         (arg_kgpc != NULL && arg_kgpc->kind == TYPE_KIND_POINTER)))
                    {
                        allow_pointer_backed_array = 1;
                    }
                    if (!allow_string_to_char_array && !allow_pointer_backed_array)
                    {
                        candidate_valid = 0;
                        if (owns_formal && formal_kgpc != NULL)
                            destroy_kgpc_type(formal_kgpc);
                        if (owns_arg && arg_kgpc != NULL)
                            destroy_kgpc_type(arg_kgpc);
                        break;
                    }
                }

                if (formal_kgpc == NULL)
                {
                    if (formal_tag == UNKNOWN_TYPE)
                    {
                        /* Truly untyped formal: use classify_match which
                         * scores untyped→untyped as MATCH_CONVERSION so the
                         * tiebreaker can prefer typed overloads. */
                        int is_integer_literal = (arg_expr != NULL && arg_expr->type == EXPR_INUM);
                        quality = semcheck_classify_match(arg_tag, arg_kgpc,
                            formal_tag, formal_kgpc, is_var_param, symtab, is_integer_literal);
                    }
                    else
                    {
                        quality = semcheck_make_quality(MATCH_PROMOTION);
                    }
                }
                else
                {
                    int is_integer_literal = (arg_expr != NULL && arg_expr->type == EXPR_INUM);
                    if (kgpc_getenv("KGPC_DEBUG_OVERLOAD_QUALITY") != NULL)
                    {
                        const char *formal_id_dbg = semcheck_get_param_type_id(formal_decl);
                        int formal_ptr_sub = (formal_kgpc != NULL &&
                            formal_kgpc->kind == TYPE_KIND_POINTER) ?
                            kgpc_type_get_pointer_subtype_tag(formal_kgpc) : -1;
                        int arg_ptr_sub = (arg_kgpc != NULL &&
                            arg_kgpc->kind == TYPE_KIND_POINTER) ?
                            kgpc_type_get_pointer_subtype_tag(arg_kgpc) : -1;
                        fprintf(stderr,
                            "[KGPC]   arg_tag=%d formal_tag=%d formal_kind=%d arg_kind=%d var=%d formal_id=%s formal_ptr=%d arg_ptr=%d is_int_literal=%d\n",
                            arg_tag, formal_tag,
                            formal_kgpc ? formal_kgpc->kind : -1,
                            arg_kgpc ? arg_kgpc->kind : -1,
                            is_var_param,
                            formal_id_dbg ? formal_id_dbg : "<null>",
                            formal_ptr_sub,
                            arg_ptr_sub,
                            is_integer_literal);
                    }
                    quality = semcheck_classify_match(arg_tag, arg_kgpc, formal_tag, formal_kgpc,
                        is_var_param, symtab, is_integer_literal);

                    if (quality.kind == MATCH_INCOMPATIBLE && !is_var_param &&
                        is_integer_type(arg_tag))
                    {
                        const char *formal_type_id = semcheck_get_param_type_id(formal_decl);
                        if (formal_type_id != NULL &&
                            pascal_identifier_equals(formal_type_id, "tconstexprint"))
                        {
                            quality = semcheck_make_quality(MATCH_CONVERSION);
                        }
                    }
                    /* Penalize var/out parameters that receive non-addressable expressions
                     * (literals, computed values). A literal like 1 cannot be passed by
                     * reference, so the value-param overload should be preferred.
                     * We demote to MATCH_INCOMPATIBLE so that value-param overloads win.
                     * Skip Self parameters — they are implicit and always valid. */
                    if (is_var_param && arg_expr != NULL &&
                        quality.kind == MATCH_EXACT)
                    {
                        /* Check if this is the Self parameter (implicit first param of methods) */
                        int is_self_param = 0;
                        if (formal_decl != NULL && formal_decl->type == TREE_VAR_DECL &&
                            formal_decl->tree_data.var_decl_data.ids != NULL)
                        {
                            const char *param_name = (const char *)formal_decl->tree_data.var_decl_data.ids->cur;
                            if (param_name != NULL && pascal_identifier_equals(param_name, "Self"))
                                is_self_param = 1;
                        }

                        if (!is_self_param)
                        {
                            int is_addressable = 0;
                            switch (arg_expr->type)
                            {
                                case EXPR_VAR_ID:
                                case EXPR_ARRAY_ACCESS:
                                case EXPR_RECORD_ACCESS:
                                case EXPR_POINTER_DEREF:
                                case EXPR_FUNCTION_CALL:
                                case EXPR_TYPECAST:
                                    is_addressable = 1;
                                    break;
                                default:
                                    break;
                            }
                            if (!is_addressable)
                                quality = semcheck_make_quality(MATCH_INCOMPATIBLE);
                        }
                    }

                    const char *formal_id = semcheck_get_param_type_id(formal_decl);
                    if (formal_id != NULL && is_var_param &&
                        pascal_identifier_equals(formal_id, "OpenString") &&
                        is_string_type(arg_tag))
                    {
                        quality = semcheck_make_quality(MATCH_EXACT);
                        quality.exact_type_id = 1;
                    }

                    if (arg_expr != NULL && arg_expr->type == EXPR_STRING &&
                        arg_expr->expr_data.string != NULL &&
                        strlen(arg_expr->expr_data.string) != 1)
                    {
                        if (formal_id != NULL && pascal_identifier_equals(formal_id, "ShortString"))
                        {
                            quality.kind = MATCH_EXACT;
                            quality.exact_type_id = 1;
                            quality.string_rank = 0;
                        }
                        else if (formal_id != NULL &&
                            (pascal_identifier_equals(formal_id, "UnicodeString") ||
                             pascal_identifier_equals(formal_id, "WideString")))
                        {
                            if (quality.kind == MATCH_EXACT)
                                quality.kind = MATCH_PROMOTION;
                            quality.string_rank = 1;
                        }
                        else if (formal_id != NULL &&
                            (pascal_identifier_equals(formal_id, "RawByteString") ||
                             pascal_identifier_equals(formal_id, "AnsiString") ||
                             pascal_identifier_equals(formal_id, "String")))
                        {
                            /* String literals prefer ShortString over RawByteString
                             * (FPC behavior). Typecasts like AnsiString(s) should
                             * match RawByteString without penalty. */
                            quality.kind = MATCH_EXACT;
                            if (arg_expr != NULL && arg_expr->type == EXPR_STRING)
                                quality.string_rank = 1;
                        }
                        else if (formal_id == NULL)
                        {
                            if (formal_tag == SHORTSTRING_TYPE)
                            {
                                /* STRING_TYPE arg to SHORTSTRING_TYPE formal is a
                                 * narrowing conversion (lossy), not exact. This ensures
                                 * RawByteString/AnsiString overloads are preferred over
                                 * ShortString when the actual arg is a long string. */
                                if (arg_tag == STRING_TYPE)
                                    quality.kind = MATCH_PROMOTION;
                                else
                                    quality.kind = MATCH_EXACT;
                            }
                            else if (formal_tag == STRING_TYPE)
                            {
                                quality.kind = MATCH_EXACT;
                            }
                        }
                    }

                    const char *explicit_string_cast_id =
                        semcheck_explicit_string_cast_target_id(arg_expr);
                    if (explicit_string_cast_id != NULL ||
                        (arg_expr != NULL && arg_expr->type == EXPR_TYPECAST))
                    {
                        int cast_target = (arg_expr != NULL && arg_expr->type == EXPR_TYPECAST)
                            ? arg_expr->expr_data.typecast_data.target_type
                            : UNKNOWN_TYPE;
                        const char *cast_target_id = explicit_string_cast_id;
                        if (cast_target_id == NULL && arg_expr != NULL &&
                            arg_expr->type == EXPR_TYPECAST)
                            cast_target_id = arg_expr->expr_data.typecast_data.target_type_id;
                        /* String typecast: AnsiString(s) should prefer RawByteString/String
                         * overloads over ShortString. The typecast explicitly requests the
                         * target type, so ShortString is a narrowing mismatch. */
                        if (cast_target_id != NULL &&
                            (pascal_identifier_equals(cast_target_id, "AnsiString") ||
                             pascal_identifier_equals(cast_target_id, "RawByteString") ||
                             pascal_identifier_equals(cast_target_id, "String")))
                        {
                            if (formal_tag == SHORTSTRING_TYPE ||
                                (formal_id != NULL && pascal_identifier_equals(formal_id, "ShortString")))
                            {
                                quality.kind = MATCH_PROMOTION;
                                quality.string_rank = 10;
                            }
                            else if (formal_id != NULL &&
                                (pascal_identifier_equals(formal_id, "RawByteString") ||
                                 pascal_identifier_equals(formal_id, "AnsiString") ||
                                 pascal_identifier_equals(formal_id, "String")))
                            {
                                quality.kind = MATCH_EXACT;
                                quality.string_rank = 0;
                            }
                        }
                        if (cast_target == POINTER_TYPE)
                        {
                            if (formal_tag == POINTER_TYPE)
                            {
                                quality.kind = MATCH_EXACT;
                                if (formal_kgpc != NULL && formal_kgpc->kind == TYPE_KIND_POINTER)
                                {
                                    if (cast_target_id != NULL &&
                                        kgpc_type_get_pointer_subtype_tag(formal_kgpc) != UNKNOWN_TYPE)
                                    {
                                        quality.exact_type_id = 1;
                                    }
                                }
                            }
                            else if (is_string_type(formal_tag))
                            {
                                if (quality.kind != MATCH_INCOMPATIBLE)
                                    quality.kind = MATCH_CONVERSION;
                            }
                        }
                    }

                    if (is_integer_type(arg_tag) && is_integer_type(formal_tag))
                    {
                        quality.integer_rank = semcheck_integer_promotion_rank(
                            arg_tag, arg_kgpc, formal_tag, formal_kgpc, is_integer_literal);
                    }
                    /* When an integer argument matches an enum formal, penalize
                     * so that real integer formals (SizeInt, Int64, etc.) are
                     * preferred over enum formals in overload resolution. */
                    else if (is_integer_type(arg_tag) && formal_tag == ENUM_TYPE)
                    {
                        quality.integer_rank = 10;
                    }
                    /* For char types, prefer smaller formal type (AnsiChar over WideChar).
                     * kgpc_type_equals(AnsiChar, WideChar) returns true (same CHAR_TYPE tag),
                     * so without this fix, AnsiChar→WideChar would get MATCH_EXACT (kind=0).
                     * We detect WideChar formals and upgrade kind to MATCH_PROMOTION. */
                    if (arg_tag == CHAR_TYPE && formal_tag == CHAR_TYPE)
                    {
                        long long formal_size = formal_kgpc != NULL ? kgpc_type_sizeof(formal_kgpc) : 1;
                        int is_wide = (formal_size > 1);
                        int actual_char_kind = 0;
                        int formal_char_kind = is_wide ? 2 : 1;
                        /* Also check by name: WideChar type alias may not carry size in formal params */
                        const char *fid = semcheck_get_param_type_id(formal_decl);
                        if (!is_wide)
                        {
                            if (fid != NULL && (pascal_identifier_equals(fid, "WideChar") ||
                                                pascal_identifier_equals(fid, "UnicodeChar")))
                            {
                                is_wide = 1;
                                formal_char_kind = 2;
                            }
                        }
                        if (arg_expr != NULL)
                        {
                            const char *arg_decl_id = semcheck_get_expr_decl_type_id(arg_expr, symtab);
                            if (arg_decl_id != NULL)
                                actual_char_kind = semcheck_string_kind_from_type_id(arg_decl_id);
                            if (actual_char_kind == 0 && arg_kgpc != NULL)
                            {
                                if (semcheck_is_widechar_like_type(arg_kgpc))
                                    actual_char_kind = 2;
                                else if (kgpc_type_is_char(arg_kgpc))
                                    actual_char_kind = 1;
                            }
                        }
                        if (actual_char_kind != 0 && formal_char_kind != 0)
                        {
                            if (actual_char_kind == formal_char_kind)
                            {
                                quality.string_rank = 0;
                            }
                            else
                            {
                                if (quality.kind == MATCH_EXACT)
                                    quality.kind = MATCH_PROMOTION;
                                quality.string_rank = 1;
                            }
                        }
                        else
                        {
                            if (is_wide && quality.kind == MATCH_EXACT)
                                quality.kind = MATCH_PROMOTION;
                            quality.string_rank = is_wide ? 1 : 0;
                        }
                    }
                    if (arg_expr != NULL)
                    {
                        const char *formal_id = semcheck_get_param_type_id(formal_decl);
                        const char *arg_decl_id = semcheck_get_expr_decl_type_id(arg_expr, symtab);
                        const char *arg_type_id = arg_decl_id;
                        const char *explicit_string_cast_id =
                            semcheck_explicit_string_cast_target_id(arg_expr);
                        if (explicit_string_cast_id != NULL)
                            arg_type_id = explicit_string_cast_id;
                        if (arg_type_id == NULL && arg_kgpc != NULL && arg_kgpc->type_alias != NULL)
                        {
                            arg_type_id = arg_kgpc->type_alias->alias_name != NULL
                                ? arg_kgpc->type_alias->alias_name
                                : arg_kgpc->type_alias->target_type_id;
                        }
                        if (arg_type_id == NULL && arg_tag == STRING_TYPE)
                            arg_type_id = "String";
                        if (formal_id != NULL && arg_type_id != NULL &&
                            pascal_identifier_equals(formal_id, arg_type_id))
                            quality.exact_type_id = 1;
                        if (!quality.exact_type_id && formal_id != NULL && arg_type_id != NULL)
                        {
                            int formal_kind = semcheck_string_kind_from_type_id(formal_id);
                            int arg_kind = semcheck_string_kind_from_type_id(arg_type_id);
                            if (formal_kind != 0 && arg_kind != 0)
                            {
                                if (formal_kind == arg_kind)
                                    quality.exact_type_id = 1;
                                else
                                {
                                    if (quality.kind == MATCH_EXACT)
                                        quality.kind = MATCH_PROMOTION;
                                    quality.string_rank = 1;
                                }
                            }
                        }
                        if (!quality.exact_type_id && formal_id != NULL && arg_type_id != NULL)
                        {
                            const char *formal_num = semcheck_numeric_type_canonical_id(formal_id);
                            const char *arg_num = semcheck_numeric_type_canonical_id(arg_type_id);
                            if (formal_num != NULL && arg_num != NULL &&
                                pascal_identifier_equals(formal_num, arg_num))
                                quality.exact_type_id = 1;
                        }
                        if (formal_id != NULL && arg_type_id != NULL &&
                            is_string_type(formal_tag) && is_string_type(arg_tag))
                        {
                            int formal_kind = semcheck_string_kind_from_type_id(formal_id);
                            int arg_kind = semcheck_string_kind_from_type_id(arg_type_id);
                            if (formal_kind != 0 && formal_kind == arg_kind)
                                quality.exact_type_id = 1;
                        }
                        if (formal_id != NULL && arg_type_id != NULL &&
                            formal_tag == POINTER_TYPE && arg_tag == STRING_TYPE)
                        {
                            int formal_kind = semcheck_string_kind_from_type_id(formal_id);
                            int arg_kind = semcheck_string_kind_from_type_id(arg_type_id);
                            if (formal_kind != 0 && formal_kind == arg_kind)
                                quality.exact_type_id = 1;
                        }
                    }
                }

                if (owns_formal && formal_kgpc != NULL)
                    destroy_kgpc_type(formal_kgpc);
                if (owns_arg && arg_kgpc != NULL)
                    destroy_kgpc_type(arg_kgpc);

                if (quality.kind == MATCH_INCOMPATIBLE)
                {
                    candidate_valid = 0;
                    break;
                }

                qualities[arg_index++] = quality;
            }

            formal_args = formal_args->next;
            call_args = call_args->next;
        }

        if (!candidate_valid)
        {
            free(qualities);
            continue;
        }

        int missing_args = total_params - given_count;
        if (kgpc_getenv("KGPC_DEBUG_OVERLOAD_QUALITY") != NULL)
        {
            fprintf(stderr, "[KGPC] candidate %s (%s) qualities:",
                candidate->id ? candidate->id : "<null>",
                candidate->mangled_id ? candidate->mangled_id : "<null>");
            for (int qi = 0; qi < given_count; qi++)
                fprintf(stderr, " (%d,%d,%d,%d)", (int)qualities[qi].kind,
                    qualities[qi].exact_type_id, qualities[qi].integer_rank,
                    qualities[qi].string_rank);
            fprintf(stderr, " missing=%d\n", missing_args);
        }
        if (num_best == 0)
        {
            best_match = candidate;
            best_qualities = qualities;
            best_missing = missing_args;
            num_best = 1;
        }
        else
        {
            int cmp = semcheck_compare_match_quality(given_count, qualities, best_qualities);
            int tiebreak_cmp = 0;
            if (cmp == 0)
            {
                int cand_untyped = semcheck_count_untyped_params(candidate);
                int best_untyped = semcheck_count_untyped_params(best_match);
                if (cand_untyped != best_untyped)
                    tiebreak_cmp = cand_untyped < best_untyped ? -1 : 1;
            }
            if (cmp == 0 && tiebreak_cmp == 0)
            {
                int cand_builtin = semcheck_candidate_is_builtin(symtab, candidate);
                int best_builtin = semcheck_candidate_is_builtin(symtab, best_match);
                if (cand_builtin != best_builtin)
                    tiebreak_cmp = cand_builtin ? 1 : -1;
            }

            /* Candidate is strictly better if:
             * - better conversion sequence, OR
             * - same quality but fewer defaulted parameters */
            if (cmp < 0 || (cmp == 0 && missing_args < best_missing) || tiebreak_cmp < 0)
            {
                free(best_qualities);
                best_match = candidate;
                best_qualities = qualities;
                best_missing = missing_args;
                num_best = 1;
            }
            else if (cmp > 0 || (cmp == 0 && missing_args > best_missing) || tiebreak_cmp > 0)
            {
                /* Best is strictly better */
                free(qualities);
            }
            else
            {
                /* cmp == 0 && missing_args == best_missing: potential ambiguity */
                if (best_match != NULL &&
                    best_match->mangled_id != NULL &&
                    candidate->mangled_id != NULL &&
                    pascal_identifier_equals(best_match->mangled_id, candidate->mangled_id))
                {
                    /* Identical mangled target: treat as duplicate candidate, not ambiguity. */
                    free(qualities);
                    continue;
                }

                int share_sig = 0;
                if (best_match != NULL)
                    share_sig = semcheck_candidates_share_signature(symtab, best_match, candidate);
                if (share_sig)
                {
                    /* Identical signatures are non-ambiguous.
                     * Candidate list is in reverse declaration order (newest first).
                     * Prefer the later candidate in the list (earliest declared). */
                    free(best_qualities);
                    best_match = candidate;
                    best_qualities = qualities;
                    best_missing = missing_args;
                    num_best = 1;
                    continue;
                }
                /* True ambiguity: neither is strictly better.
                 * As a tiebreaker, prefer candidates with typed parameters
                 * over those with untyped var params (e.g., FpRead(buf: PAnsiChar)
                 * is preferred over FpRead(var buf) when both match equally). */
                int cand_untyped = semcheck_count_untyped_params(candidate);
                int best_untyped = semcheck_count_untyped_params(best_match);
                if (cand_untyped < best_untyped)
                {
                    free(best_qualities);
                    best_match = candidate;
                    best_qualities = qualities;
                    best_missing = missing_args;
                    num_best = 1;
                }
                else if (best_untyped < cand_untyped)
                {
                    free(qualities);
                }
                else
                {
                    int cand_builtin = semcheck_candidate_is_builtin(symtab, candidate);
                    int best_builtin = semcheck_candidate_is_builtin(symtab, best_match);
                    if (cand_builtin != best_builtin)
                    {
                        if (cand_builtin)
                            free(qualities);
                        else
                        {
                            free(best_qualities);
                            best_match = candidate;
                            best_qualities = qualities;
                            best_missing = missing_args;
                            num_best = 1;
                        }
                    }
                    else
                    {
                        /* Neither candidate strictly dominates.  Keep the
                         * current best rather than reporting ambiguity.
                         * When the comparison function returns 0 after all
                         * tiebreakers, both candidates are equally valid.
                         * Prefer the candidate with a resolved mangled name
                         * over one without (generic specialization artifacts). */
                        if (candidate->mangled_id != NULL && best_match->mangled_id == NULL)
                        {
                            free(best_qualities);
                            best_match = candidate;
                            best_qualities = qualities;
                            best_missing = missing_args;
                            num_best = 1;
                        }
                        else
                        {
                            free(qualities);
                        }
                    }
                }
            }
        }
    }

    if (best_match_out != NULL)
        *best_match_out = best_match;
    if (best_rank_out != NULL)
        *best_rank_out = 0;
    if (num_best_out != NULL)
        *num_best_out = num_best;

    if (best_qualities != NULL)
        free(best_qualities);

    if (num_best == 1)
        return 0;
    if (num_best == 0)
        return 1;
    return 2;
}
