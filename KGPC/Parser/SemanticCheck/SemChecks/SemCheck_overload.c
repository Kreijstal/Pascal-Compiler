/*
    Overload resolution extracted from SemCheck_expr.c
*/

#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <assert.h>
#include <stdio.h>

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

    ListNode_t *matches = FindAllIdentsInTable(symtab->builtins, node->id);
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

static int semcheck_mangled_is_syscall(const char *mangled_id)
{
    if (mangled_id == NULL)
        return 0;
    return strncmp(mangled_id, "FPC_SYSC_", 9) == 0;
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
    
    /* Int64 and QWord have different signedness, so not compatible */
    
    return 0;
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
            return 0;
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
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
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

int semcheck_method_accepts_arg_count(ListNode_t *params, int arg_count, int *expects_self_out)
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

    int arg_tag = UNKNOWN_TYPE;
    semcheck_expr_legacy_tag(&arg_tag, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    KgpcType *arg_type = arg_expr->resolved_kgpc_type;
    if (arg_type != NULL && arg_tag != UNKNOWN_TYPE)
    {
        int keep = 0;
        if (arg_tag == POINTER_TYPE)
        {
            if (arg_type->kind == TYPE_KIND_POINTER)
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
            if (FindIdent(&elem_node, symtab, arg_expr->array_element_type_id) == 0 &&
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
                    if (FindIdent(&type_node, symtab, arg_expr->pointer_subtype_id) == 0 &&
                        type_node != NULL && type_node->type != NULL)
                    {
                        kgpc_type_retain(type_node->type);
                        arg_type = create_pointer_type(type_node->type);
                    }
                }
                if (arg_type == NULL && arg_expr != NULL &&
                    arg_expr->pointer_subtype != UNKNOWN_TYPE)
                {
                    KgpcType *points_to = create_primitive_type(arg_expr->pointer_subtype);
                    if (points_to != NULL)
                        arg_type = create_pointer_type(points_to);
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


static MatchQuality semcheck_make_quality(MatchQualityKind kind)
{
    MatchQuality q;
    q.kind = kind;
    q.exact_type_id = 0;
    q.exact_pointer_subtype = 0;
    q.exact_array_elem = 0;
    q.int_promo_rank = 0;
    q.char_promo_rank = 0;
    return q;
}

static MatchQuality semcheck_match_from_rank(int rank)
{
    if (rank == 0)
        return semcheck_make_quality(MATCH_EXACT);
    if (rank == 1)
        return semcheck_make_quality(MATCH_PROMOTION);
    if (rank > 1)
        return semcheck_make_quality(MATCH_CONVERSION);
    return semcheck_make_quality(MATCH_INCOMPATIBLE);
}

static MatchQuality semcheck_classify_match(int actual_tag, KgpcType *actual_kgpc,
    int formal_tag, KgpcType *formal_kgpc, int is_var_param, SymTab_t *symtab,
    int is_integer_literal)
{
    if (is_var_param)
    {
        if (actual_kgpc != NULL && formal_kgpc != NULL)
        {
            if (kgpc_type_equals(actual_kgpc, formal_kgpc))
                return semcheck_make_quality(MATCH_EXACT);
            int formal_is_untyped_ptr = (formal_kgpc->kind == TYPE_KIND_PRIMITIVE &&
                formal_kgpc->info.primitive_type_tag == POINTER_TYPE);
            int actual_is_untyped_ptr = (actual_kgpc->kind == TYPE_KIND_PRIMITIVE &&
                actual_kgpc->info.primitive_type_tag == POINTER_TYPE);
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
            return semcheck_make_quality(MATCH_INCOMPATIBLE);
        }
        if (actual_tag == formal_tag)
            return semcheck_make_quality(MATCH_EXACT);
        /* Check for compatible primitive types (e.g., Integer/LongInt) */
        if (are_primitive_tags_compatible(actual_tag, formal_tag))
            return semcheck_make_quality(MATCH_EXACT);
        return semcheck_make_quality(MATCH_INCOMPATIBLE);
    }

    if (formal_tag == UNKNOWN_TYPE || formal_tag == BUILTIN_ANY_TYPE)
        return semcheck_make_quality(MATCH_EXACT);
    /* For pointer types, don't return early - need to compare subtypes */
    if (actual_tag == formal_tag && formal_tag != POINTER_TYPE)
        return semcheck_make_quality(MATCH_EXACT);
    /* String types are mutually compatible (STRING_TYPE, SHORTSTRING_TYPE) */
    if (is_string_type(formal_tag) && is_string_type(actual_tag))
        return semcheck_make_quality(MATCH_PROMOTION);

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
                MatchQuality q = semcheck_make_quality(MATCH_EXACT);
                q.exact_pointer_subtype = 1;
                return q;
            }
            return semcheck_make_quality(MATCH_INCOMPATIBLE);
        }
        if (actual_sub != UNKNOWN_TYPE || formal_sub != UNKNOWN_TYPE)
            return semcheck_make_quality(MATCH_CONVERSION);
        return semcheck_make_quality(MATCH_PROMOTION);
    }

    /* Integer to integer: always treat as promotion for overload ordering. */
    if (is_integer_type(actual_tag) && is_integer_type(formal_tag))
    {
        (void)is_integer_literal;
        return semcheck_make_quality(MATCH_PROMOTION);
    }

    if (actual_kgpc != NULL && formal_kgpc != NULL)
    {
        int rank = kgpc_type_conversion_rank(actual_kgpc, formal_kgpc);
        if (rank >= 0)
            return semcheck_match_from_rank(rank);
        if (are_types_compatible_for_assignment(formal_kgpc, actual_kgpc, symtab))
            return semcheck_make_quality(MATCH_CONVERSION);
    }
    if (is_integer_type(actual_tag) && formal_tag == REAL_TYPE)
        return semcheck_make_quality(MATCH_CONVERSION);
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

    return semcheck_make_quality(MATCH_INCOMPATIBLE);
}

static const char *semcheck_get_expr_decl_type_id(struct Expression *expr, SymTab_t *symtab)
{
    if (expr == NULL || symtab == NULL)
        return NULL;

    if (expr->array_element_type_id != NULL)
        return expr->array_element_type_id;

    if (expr->type != EXPR_VAR_ID)
        return NULL;

    HashNode_t *node = NULL;
    if (FindIdent(&node, symtab, expr->expr_data.id) < 0 || node == NULL || node->type == NULL)
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
        pascal_identifier_equals(id, "RawByteString"))
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
    if (a->exact_pointer_subtype != b->exact_pointer_subtype)
        return a->exact_pointer_subtype > b->exact_pointer_subtype ? -1 : 1;
    if (a->exact_array_elem != b->exact_array_elem)
        return a->exact_array_elem > b->exact_array_elem ? -1 : 1;
    if (a->int_promo_rank < b->int_promo_rank)
        return -1;
    if (a->int_promo_rank > b->int_promo_rank)
        return 1;
    if (a->char_promo_rank < b->char_promo_rank)
        return -1;
    if (a->char_promo_rank > b->char_promo_rank)
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
    int a_better_somewhere = 0;
    int b_better_somewhere = 0;

    for (int i = 0; i < arg_count; i++)
    {
        int cmp = compare_single_quality(&a[i], &b[i]);
        if (cmp < 0)
            a_better_somewhere = 1;
        else if (cmp > 0)
            b_better_somewhere = 1;
    }

    /* A is strictly better if better somewhere and not worse anywhere */
    if (a_better_somewhere && !b_better_somewhere)
        return -1;
    /* B is strictly better if better somewhere and not worse anywhere */
    if (b_better_somewhere && !a_better_somewhere)
        return 1;
    /* Otherwise ambiguous (either equal everywhere, or mixed) */
    return 0;
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
             candidate->hash_type != HASHTYPE_PROCEDURE) ||
            candidate->type == NULL)
            continue;
        if (prefer_non_builtin && semcheck_candidate_is_builtin(symtab, candidate))
            continue;

        ListNode_t *candidate_args = kgpc_type_get_procedure_params(candidate->type);
        int total_params = semcheck_count_total_params(candidate_args);
        int required_params = semcheck_count_required_params(candidate_args);

        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] semcheck_resolve_overload: candidate %s args=%d required=%d given=%d\n",
                candidate->id, total_params, required_params, given_count);
        }

        if (!((given_count >= required_params && given_count <= total_params) ||
            (total_params == 0 && given_count > 0 &&
             candidate->type != NULL &&
             candidate->type->info.proc_info.definition == NULL)))
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
        ListNode_t *call_args = args_given;
        int arg_index = 0;

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
                        semcheck_expr_legacy_tag(&rhs_type, symtab, rhs_expr, max_scope_lev, NO_MUTATE);
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

            if (formal_is_array_decl &&
                arg_expr != NULL &&
                (arg_expr->type == EXPR_SET || arg_expr->type == EXPR_ARRAY_LITERAL ||
                 arg_expr->array_element_type != UNKNOWN_TYPE ||
                 arg_expr->array_element_type_id != NULL))
            {
                MatchQuality quality = semcheck_make_quality(MATCH_PROMOTION);
                int owns_formal = 0;
                KgpcType *formal_kgpc = resolve_type_from_vardecl(formal_decl, symtab, &owns_formal);
                if (formal_kgpc == NULL && formal_inline_alias != NULL)
                {
                    formal_kgpc = create_kgpc_type_from_type_alias(formal_inline_alias, symtab);
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
                            if (FindIdent(&type_node, symtab, arg_expr->array_element_type_id) != -1 && type_node != NULL)
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
                        }
                    }

                    if (arg_elem_tag != UNKNOWN_TYPE &&
                        (formal_elem_tag != UNKNOWN_TYPE || formal_elem != NULL))
                    {
                        int rank = -1;
                        if (formal_elem_tag != UNKNOWN_TYPE)
                        {
                            KgpcType *arg_elem = create_primitive_type(arg_elem_tag);
                            KgpcType *formal_elem_prim = create_primitive_type(formal_elem_tag);
                            rank = kgpc_type_conversion_rank(arg_elem, formal_elem_prim);
                            destroy_kgpc_type(arg_elem);
                            destroy_kgpc_type(formal_elem_prim);
                        }
                        else
                        {
                            KgpcType *arg_elem = create_primitive_type(arg_elem_tag);
                            rank = kgpc_type_conversion_rank(arg_elem, formal_elem);
                            destroy_kgpc_type(arg_elem);
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
                            quality.exact_array_elem = 1;
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

                MatchQuality quality = semcheck_make_quality(MATCH_INCOMPATIBLE);
                if (formal_kgpc == NULL)
                {
                    quality = semcheck_make_quality(MATCH_PROMOTION);
                }
                else
                {
                    int is_integer_literal = (arg_expr != NULL && arg_expr->type == EXPR_INUM);
                    if (getenv("KGPC_DEBUG_OVERLOAD_QUALITY") != NULL)
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

                    if (arg_expr != NULL && arg_expr->type == EXPR_STRING &&
                        arg_expr->expr_data.string != NULL &&
                        strlen(arg_expr->expr_data.string) != 1)
                    {
                        const char *formal_id = semcheck_get_param_type_id(formal_decl);
                        if (formal_id != NULL && pascal_identifier_equals(formal_id, "ShortString"))
                        {
                            quality.kind = MATCH_EXACT;
                            quality.exact_type_id = 1;
                        }
                        else if (formal_id != NULL &&
                            (pascal_identifier_equals(formal_id, "UnicodeString") ||
                             pascal_identifier_equals(formal_id, "RawByteString") ||
                             pascal_identifier_equals(formal_id, "WideString") ||
                             pascal_identifier_equals(formal_id, "AnsiString") ||
                             pascal_identifier_equals(formal_id, "String")))
                        {
                            if (quality.kind == MATCH_EXACT)
                                quality.kind = MATCH_PROMOTION;
                        }
                        else if (formal_id == NULL)
                        {
                            if (formal_tag == SHORTSTRING_TYPE)
                            {
                                quality.kind = MATCH_EXACT;
                            }
                            else if (formal_tag == STRING_TYPE)
                            {
                                if (quality.kind == MATCH_EXACT)
                                    quality.kind = MATCH_PROMOTION;
                            }
                        }
                    }

                    if (is_integer_type(arg_tag) && is_integer_type(formal_tag))
                    {
                        quality.int_promo_rank = semcheck_integer_promotion_rank(
                            arg_tag, arg_kgpc, formal_tag, formal_kgpc, is_integer_literal);
                    }
                    /* For char types, prefer smaller formal type (AnsiChar over WideChar) */
                    if (arg_tag == CHAR_TYPE && formal_tag == CHAR_TYPE)
                    {
                        long long formal_size = formal_kgpc != NULL ? kgpc_type_sizeof(formal_kgpc) : 1;
                        if (formal_size <= 1)
                            quality.char_promo_rank = 0;  /* AnsiChar: best */
                        else
                            quality.char_promo_rank = 1;  /* WideChar: worse */
                    }
                    if (arg_expr != NULL)
                    {
                        const char *formal_id = semcheck_get_param_type_id(formal_decl);
                        const char *arg_decl_id = semcheck_get_expr_decl_type_id(arg_expr, symtab);
                        const char *arg_type_id = arg_decl_id;
                        if (arg_type_id == NULL && arg_kgpc != NULL && arg_kgpc->type_alias != NULL)
                        {
                            arg_type_id = arg_kgpc->type_alias->alias_name != NULL
                                ? arg_kgpc->type_alias->alias_name
                                : arg_kgpc->type_alias->target_type_id;
                        }
                        if (formal_id != NULL && arg_type_id != NULL &&
                            pascal_identifier_equals(formal_id, arg_type_id))
                            quality.exact_type_id = 1;
                        if (!quality.exact_type_id && formal_id != NULL && arg_type_id != NULL)
                        {
                            const char *formal_num = semcheck_numeric_type_canonical_id(formal_id);
                            const char *arg_num = semcheck_numeric_type_canonical_id(arg_type_id);
                            if (formal_num != NULL && arg_num != NULL &&
                                pascal_identifier_equals(formal_num, arg_num))
                                quality.exact_type_id = 1;
                        }
                        if (formal_id != NULL && arg_type_id != NULL &&
                            formal_tag == POINTER_TYPE && arg_tag == STRING_TYPE)
                        {
                            int formal_kind = semcheck_string_kind_from_type_id(formal_id);
                            int arg_kind = semcheck_string_kind_from_type_id(arg_type_id);
                            if (formal_kind != 0 && formal_kind == arg_kind)
                                quality.exact_pointer_subtype = 1;
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
        if (getenv("KGPC_DEBUG_OVERLOAD_QUALITY") != NULL)
        {
            fprintf(stderr, "[KGPC] candidate %s qualities:", candidate->id ? candidate->id : "<null>");
            for (int qi = 0; qi < given_count; qi++)
                fprintf(stderr, " (%d,%d,%d,%d,%d)", (int)qualities[qi].kind,
                    qualities[qi].exact_type_id, qualities[qi].exact_pointer_subtype,
                    qualities[qi].exact_array_elem, qualities[qi].int_promo_rank);
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
            int specificity_cmp = 0;

            if (cmp == 0)
            {
                int cand_typeid = 0;
                int best_typeid = 0;
                int cand_array = 0;
                int best_array = 0;
                int cand_ptr = 0;
                int best_ptr = 0;
                int cand_int_max = 0;
                int best_int_max = 0;

                for (int qi = 0; qi < given_count; qi++)
                {
                    if (qualities[qi].exact_type_id)
                        cand_typeid++;
                    if (best_qualities[qi].exact_type_id)
                        best_typeid++;
                    if (qualities[qi].exact_array_elem)
                        cand_array++;
                    if (best_qualities[qi].exact_array_elem)
                        best_array++;
                    if (qualities[qi].exact_pointer_subtype)
                        cand_ptr++;
                    if (best_qualities[qi].exact_pointer_subtype)
                        best_ptr++;
                    if (qualities[qi].int_promo_rank > cand_int_max)
                        cand_int_max = qualities[qi].int_promo_rank;
                    if (best_qualities[qi].int_promo_rank > best_int_max)
                        best_int_max = best_qualities[qi].int_promo_rank;
                }

                if (cand_typeid != best_typeid)
                    specificity_cmp = cand_typeid > best_typeid ? -1 : 1;
                else if (cand_array != best_array)
                    specificity_cmp = cand_array > best_array ? -1 : 1;
                else if (cand_ptr != best_ptr)
                    specificity_cmp = cand_ptr > best_ptr ? -1 : 1;
                else if (cand_int_max != best_int_max)
                    specificity_cmp = cand_int_max < best_int_max ? -1 : 1;
                else
                {
                    int cand_sys = semcheck_mangled_is_syscall(candidate->mangled_id);
                    int best_sys = semcheck_mangled_is_syscall(best_match->mangled_id);
                    if (cand_sys != best_sys)
                        specificity_cmp = cand_sys ? 1 : -1;
                }
            }

            /* Candidate is strictly better if:
             * - better conversion sequence, OR
             * - same quality but fewer defaulted parameters */
            if (cmp < 0 || (cmp == 0 && missing_args < best_missing) || specificity_cmp < 0)
            {
                free(best_qualities);
                best_match = candidate;
                best_qualities = qualities;
                best_missing = missing_args;
                num_best = 1;
            }
            else if (cmp > 0 || (cmp == 0 && missing_args > best_missing) || specificity_cmp > 0)
            {
                /* Best is strictly better */
                free(qualities);
            }
            else
            {
                /* cmp == 0 && missing_args == best_missing: potential ambiguity */
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
                    continue;
                }
                /* True ambiguity: neither is strictly better */
                num_best++;
                free(qualities);
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
