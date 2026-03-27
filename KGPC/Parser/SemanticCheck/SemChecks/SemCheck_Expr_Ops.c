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

int semcheck_resolve_overload(HashNode_t **best_match_out,
    int *num_best_out,
    ListNode_t *overload_candidates,
    ListNode_t *args_given,
    SymTab_t *symtab,
    struct Expression *call_expr,
    int max_scope_lev,
    int prefer_non_builtin);

static int semcheck_operator_lookup_unit_index(SymTab_t *symtab)
{
    if (symtab != NULL && symtab->current_scope != NULL && symtab->current_scope->unit_index > 0)
        return symtab->current_scope->unit_index;
    return semcheck_get_current_unit_index();
}

/*
 * Per-operand match quality for binary operator candidate resolution.
 * EXACT > COMPATIBLE > INCOMPATIBLE, ranked by decreasing desirability.
 */
typedef enum {
    BINOP_MATCH_EXACT = 2,        /* kgpc_type_equals: types are identical */
    BINOP_MATCH_COMPATIBLE = 1,   /* are_types_compatible_for_assignment: implicit conversion ok */
    BINOP_MATCH_INCOMPATIBLE = 0  /* no valid conversion */
} BinopOperandMatch;

/*
 * Find the best matching binary operator candidate from a list of overloads.
 * Each candidate's two formal parameters are compared against the actual
 * left/right operand types.  The candidate with the highest total match
 * quality (sum of per-operand matches) wins.  If no candidate is valid,
 * returns NULL.
 */
static HashNode_t *semcheck_find_best_binary_operator_candidate(
    ListNode_t *candidates, KgpcType *left_arg_type, KgpcType *right_arg_type,
    SymTab_t *symtab)
{
    HashNode_t *best = NULL;
    int best_quality = -1;  /* sum of BinopOperandMatch values for the best candidate */

    for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate == NULL ||
            (candidate->hash_type != HASHTYPE_FUNCTION &&
             candidate->hash_type != HASHTYPE_PROCEDURE) ||
            candidate->type == NULL)
            continue;

        ListNode_t *params = kgpc_type_get_procedure_params(candidate->type);
        if (params == NULL || params->next == NULL)
            continue;

        Tree_t *left_decl = (Tree_t *)params->cur;
        Tree_t *right_decl = (Tree_t *)params->next->cur;
        int owns_left = 0;
        int owns_right = 0;
        KgpcType *left_formal = resolve_type_from_vardecl(left_decl, symtab, &owns_left);
        KgpcType *right_formal = resolve_type_from_vardecl(right_decl, symtab, &owns_right);

        int valid = 1;
        int quality = 0;

        if (left_formal != NULL && left_arg_type != NULL)
        {
            if (kgpc_type_equals(left_formal, left_arg_type))
                quality += BINOP_MATCH_EXACT;
            else if (are_types_compatible_for_assignment(left_formal, left_arg_type, symtab))
                quality += BINOP_MATCH_COMPATIBLE;
            else
                valid = 0;
        }
        if (valid && right_formal != NULL && right_arg_type != NULL)
        {
            if (kgpc_type_equals(right_formal, right_arg_type))
                quality += BINOP_MATCH_EXACT;
            else if (are_types_compatible_for_assignment(right_formal, right_arg_type, symtab))
                quality += BINOP_MATCH_COMPATIBLE;
            else
                valid = 0;
        }

        if (owns_left && left_formal != NULL)
            destroy_kgpc_type(left_formal);
        if (owns_right && right_formal != NULL)
            destroy_kgpc_type(right_formal);

        if (valid && quality > best_quality)
        {
            best = candidate;
            best_quality = quality;
        }
    }

    return best;
}

static int semcheck_find_ident_by_prefix_visible(HashNode_t **hash_return,
    SymTab_t *symtab, const char *prefix)
{
    int return_val = 0;
    int caller_unit_index;

    if (hash_return == NULL || symtab == NULL || prefix == NULL)
        return -1;

    caller_unit_index = semcheck_operator_lookup_unit_index(symtab);

    /* 1. Scope stack (local scopes, parameters, WITH contexts) */
    for (ScopeNode *cur = symtab->current_scope; cur != NULL; cur = cur->parent)
    {
        HashNode_t *node = FindIdentByPrefixInTableForUnit(cur->table,
            prefix, caller_unit_index);
        if (node != NULL)
        {
            *hash_return = node;
            return return_val;
        }
        ++return_val;
    }

    /* 2. Unit scopes — own unit first, then dependency units. */
    {
        int global_level = 0;

        /* 2a. Caller's own unit scope table */
        if (caller_unit_index > 0 && caller_unit_index < SYMTAB_MAX_UNITS &&
            symtab->unit_scopes[caller_unit_index] != NULL)
        {
            HashNode_t *node = FindIdentByPrefixInTableForUnit(
                symtab->unit_scopes[caller_unit_index]->table, prefix, caller_unit_index);
            if (node != NULL)
            {
                *hash_return = node;
                return global_level;
            }
        }

        /* 2b. Dependency unit scope tables */
        int num_units = unit_registry_count();
        for (int dep = 1; dep <= num_units; dep++)
        {
            if (dep == caller_unit_index)
                continue;
            if (!unit_registry_is_dep(caller_unit_index, dep))
                continue;
            if (dep >= SYMTAB_MAX_UNITS ||
                symtab->unit_scopes[dep] == NULL ||
                symtab->unit_scopes[dep]->table == NULL)
                continue;
            HashNode_t *node = FindIdentByPrefixInTableForUnit(
                symtab->unit_scopes[dep]->table, prefix, caller_unit_index);
            if (node != NULL)
            {
                *hash_return = node;
                return global_level;
            }
        }

        /* 2c. When current scope is program-level (unit_index == 0), search all unit scopes. */
        if (caller_unit_index == 0)
        {
            for (int u = 1; u < SYMTAB_MAX_UNITS; u++)
            {
                if (symtab->unit_scopes[u] == NULL ||
                    symtab->unit_scopes[u]->table == NULL)
                    continue;
                HashNode_t *node = FindIdentByPrefixInTableForUnit(
                    symtab->unit_scopes[u]->table, prefix, caller_unit_index);
                if (node != NULL)
                {
                    *hash_return = node;
                    return global_level;
                }
            }
        }
    }

    /* 3. Builtins */
    {
        HashNode_t *node = FindIdentByPrefixInTable(symtab->builtin_scope->table, prefix);
        if (node != NULL)
        {
            *hash_return = node;
            return return_val;
        }
    }

    *hash_return = NULL;
    return -1;
}

static int semcheck_candidate_returns_bool(SymTab_t *symtab, HashNode_t *candidate)
{
    if (candidate == NULL || candidate->type == NULL ||
        candidate->type->kind != TYPE_KIND_PROCEDURE)
        return 0;

    KgpcType *return_type = kgpc_type_get_return_type(candidate->type);
    if (return_type != NULL)
        return semcheck_tag_from_kgpc(return_type) == BOOL;

    if (candidate->type->info.proc_info.return_type_id != NULL)
    {
        HashNode_t *ret_node = semcheck_find_preferred_type_node(symtab,
            candidate->type->info.proc_info.return_type_id);
        if (ret_node != NULL && ret_node->type != NULL)
            return semcheck_tag_from_kgpc(ret_node->type) == BOOL;

        return semcheck_map_builtin_type_name(symtab,
            candidate->type->info.proc_info.return_type_id) == BOOL;
    }

    return 0;
}

static int semcheck_try_refine_funccall_to_bool(
    SymTab_t *symtab,
    struct Expression *call_expr,
    int max_scope_lev,
    int mutating,
    int *type_tag_inout)
{
    if (symtab == NULL || call_expr == NULL || type_tag_inout == NULL ||
        call_expr->type != EXPR_FUNCTION_CALL ||
        call_expr->expr_data.function_call_data.id == NULL)
        return 0;

    ListNode_t *all_candidates = FindAllIdents(symtab, call_expr->expr_data.function_call_data.id);
    if (all_candidates == NULL)
        return 0;

    ListNode_t *bool_candidates = NULL;
    ListNode_t *tail = NULL;
    for (ListNode_t *cur = all_candidates; cur != NULL; cur = cur->next)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate == NULL)
            continue;
        if (!semcheck_candidate_returns_bool(symtab, candidate))
            continue;

        ListNode_t *node = CreateListNode(candidate, LIST_UNSPECIFIED);
        if (node == NULL)
            continue;
        if (bool_candidates == NULL)
            bool_candidates = node;
        else
            tail->next = node;
        tail = node;
    }
    DestroyList(all_candidates);

    if (bool_candidates == NULL)
        return 0;

    HashNode_t *best_match = NULL;
    int num_best = 0;
    int resolve_status = semcheck_resolve_overload(&best_match, &num_best,
        bool_candidates, call_expr->expr_data.function_call_data.args_expr,
        symtab, call_expr, max_scope_lev, 1);
    DestroyList(bool_candidates);
    if (resolve_status != 0 || best_match == NULL || num_best != 1)
        return 0;

    semcheck_reset_function_call_cache(call_expr);
    semcheck_set_function_call_target(call_expr, best_match);
    if (call_expr->expr_data.function_call_data.mangled_id != NULL)
        free(call_expr->expr_data.function_call_data.mangled_id);
    call_expr->expr_data.function_call_data.mangled_id =
        best_match->mangled_id != NULL ? strdup(best_match->mangled_id) : strdup(best_match->id);
    semcheck_sync_function_call_target_to_mangled(call_expr, symtab);
    semcheck_expr_set_resolved_type(call_expr, BOOL);
    {
        KgpcType *bool_type = create_primitive_type(BOOL);
        if (bool_type != NULL)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(call_expr, bool_type);
            destroy_kgpc_type(bool_type);
        }
    }
    call_expr->expr_data.function_call_data.is_call_info_valid = 1;
    *type_tag_inout = BOOL;
    (void)mutating;
    return 1;
}

static void semcheck_set_result_expr_metadata(struct Expression *expr, SymTab_t *symtab,
    KgpcType *type)
{
    if (expr == NULL)
        return;

    if (type != NULL && kgpc_type_is_array(type))
        semcheck_set_array_info_from_kgpctype(expr, symtab, type, expr->line_num);
    else
        semcheck_clear_array_info(expr);

    if (type != NULL && kgpc_type_is_pointer(type))
    {
        KgpcType *points_to = type->info.points_to;
        if (points_to == NULL)
        {
            semcheck_set_pointer_info(expr, UNKNOWN_TYPE, NULL);
        }
        else if (points_to->kind == TYPE_KIND_RECORD &&
            points_to->info.record_info != NULL &&
            points_to->info.record_info->type_id != NULL)
        {
            semcheck_set_pointer_info(expr, RECORD_TYPE,
                points_to->info.record_info->type_id);
        }
        else if (points_to->kind == TYPE_KIND_PRIMITIVE)
        {
            semcheck_set_pointer_info(expr, points_to->info.primitive_type_tag, NULL);
        }
        else
        {
            semcheck_set_pointer_info(expr, UNKNOWN_TYPE, NULL);
        }
    }
    else
    {
        semcheck_clear_pointer_info(expr);
    }
}

static int semcheck_normalize_result_identifier(struct Expression *expr)
{
    if (expr == NULL || expr->type != EXPR_VAR_ID)
        return 0;

    const char *result_var = semcheck_get_current_subprogram_result_var_name();
    const char *replacement = (result_var != NULL && result_var[0] != '\0')
        ? result_var : "Result";
    if (expr->expr_data.id != NULL && pascal_identifier_equals(expr->expr_data.id, replacement))
        return 0;

    char *dup = strdup(replacement);
    if (dup == NULL)
        return -1;
    free(expr->expr_data.id);
    expr->expr_data.id = dup;
    return 0;
}

static int semcheck_hashnode_is_callable(const HashNode_t *node)
{
    if (node == NULL)
        return 0;
    return node->hash_type == HASHTYPE_FUNCTION ||
        node->hash_type == HASHTYPE_PROCEDURE ||
        node->hash_type == HASHTYPE_BUILTIN_PROCEDURE;
}

static int semcheck_hashnode_is_value_symbol(const HashNode_t *node)
{
    if (node == NULL)
        return 0;
    if (semcheck_hashnode_is_callable(node))
        return 0;
    if (node->hash_type == HASHTYPE_TYPE)
        return 0;
    return 1;
}

static HashNode_t *semcheck_find_preferred_value_ident(
    SymTab_t *symtab, const char *id, int *scope_out)
{
    if (symtab == NULL || id == NULL)
        return NULL;

    int scope = 0;
    for (ScopeNode *cur_scope = symtab->current_scope; cur_scope != NULL; cur_scope = cur_scope->parent)
    {
        HashTable_t *table = cur_scope->table;
        ListNode_t *matches = FindAllIdentsInTable(table, id);
        if (matches != NULL)
        {
            for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
            {
                HashNode_t *candidate = (HashNode_t *)cur->cur;
                if (semcheck_hashnode_is_value_symbol(candidate))
                {
                    if (scope_out != NULL)
                        *scope_out = scope;
                    DestroyList(matches);
                    return candidate;
                }
            }
            DestroyList(matches);
        }
        ++scope;
    }

    ListNode_t *builtin_matches = FindAllIdentsInTable(symtab->builtin_scope->table, id);
    if (builtin_matches != NULL)
    {
        for (ListNode_t *cur = builtin_matches; cur != NULL; cur = cur->next)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (semcheck_hashnode_is_value_symbol(candidate))
            {
                if (scope_out != NULL)
                    *scope_out = scope;
                DestroyList(builtin_matches);
                return candidate;
            }
        }
        DestroyList(builtin_matches);
    }

    return NULL;
}

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

static HashNode_t *semcheck_pick_type_node_with_origin_preference(SymTab_t *symtab,
    const char *type_id, int prefer_unit_defined)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, type_id);
    HashNode_t *best = NULL;
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate != NULL && candidate->hash_type == HASHTYPE_TYPE)
        {
            if (best == NULL)
            {
                best = candidate;
            }
            else if (prefer_unit_defined)
            {
                if (!best->defined_in_unit && candidate->defined_in_unit)
                    best = candidate;
            }
            else
            {
                if (best->defined_in_unit && !candidate->defined_in_unit)
                    best = candidate;
            }
        }
        cur = cur->next;
    }

    if (matches != NULL)
        DestroyList(matches);
    return best;
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
            semcheck_expr_set_resolved_type(expr, BOOL);
            KgpcType *bool_type = create_primitive_type(BOOL);
            if (bool_type != NULL)
            {
                semcheck_expr_set_resolved_kgpc_type_shared(expr, bool_type);
                destroy_kgpc_type(bool_type);
            }
            return return_val;
        }
        if (is_integer_type(type_first) || type_first == ENUM_TYPE ||
            type_first == POINTER_TYPE || type_first == RECORD_TYPE)
        {
            struct Expression *operand = expr->expr_data.relop_data.left;
            struct Expression *neg_one = mk_inum(expr->line_num, -1);
            expr->type = EXPR_MULOP;
            expr->expr_data.mulop_data.mulop_type = XOR;
            expr->expr_data.mulop_data.left_term = operand;
            expr->expr_data.mulop_data.right_factor = neg_one;
            return semcheck_mulop(type_return, symtab, expr, max_scope_lev, mutating);
        }
        if (type_first == UNKNOWN_TYPE)
        {
            *type_return = UNKNOWN_TYPE;
            return return_val;
        }
        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, expected relational type after \"NOT\"!\n\n",
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

                if (type_second != SET_TYPE && type_second != UNKNOWN_TYPE)
                {
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, expected set operand on right side of IN expression!\n\n",
                        expr->line_num);
                    ++return_val;
                }
                if (!is_integer_type(type_first) && type_first != ENUM_TYPE &&
                    type_first != CHAR_TYPE && type_first != BOOL &&
                    type_first != UNKNOWN_TYPE)
                {
                    /* Fallback: check KgpcType for char/integer (e.g. PPAnsiChar^[i] indexing) */
                    int in_ok = 0;
                    if (expr1 != NULL && expr1->resolved_kgpc_type != NULL)
                    {
                        if (kgpc_type_is_char(expr1->resolved_kgpc_type) ||
                            kgpc_type_is_integer(expr1->resolved_kgpc_type))
                            in_ok = 1;
                    }
                    if (!in_ok)
                    {
                        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, expected integer operand on left side of IN expression!\n\n",
                            expr->line_num);
                        ++return_val;
                    }
                }
            }
            else if (relop_type == EQ || relop_type == NE)
            {
                /* Check for operator overloading for record types first */
                if (type_first == RECORD_TYPE || type_second == RECORD_TYPE)
                {
                    struct Expression *record_expr = expr1;
                    const char *record_type_name = NULL;

                    if (type_first == POINTER_TYPE && type_second == RECORD_TYPE)
                    {
                        record_expr = expr2;
                    }
                    else if (type_first != RECORD_TYPE && type_second == RECORD_TYPE)
                    {
                        record_expr = expr2;
                    }

                    record_type_name = get_expr_type_name(record_expr, symtab);
                    if (record_type_name == NULL)
                        goto relop_fallback;

                    if (record_type_name != NULL)
                    {
                        const char *op_suffix = (relop_type == EQ) ? "op_eq" : "op_ne";
                        size_t name_len = strlen(record_type_name) + strlen(op_suffix) + 3;
                        char *operator_method = (char *)malloc(name_len);
                        
                        if (operator_method != NULL)
                        {
                            snprintf(operator_method, name_len, "%s__%s", record_type_name, op_suffix);
                            
                            HashNode_t *operator_node = NULL;
                            ListNode_t *operator_candidates = FindAllIdents(symtab, operator_method);
                            if (operator_candidates != NULL)
                            {
                                KgpcType *left_arg_type = expr1 != NULL ? expr1->resolved_kgpc_type : NULL;
                                KgpcType *right_arg_type = expr2 != NULL ? expr2->resolved_kgpc_type : NULL;
                                HashNode_t *best_exact = semcheck_find_best_binary_operator_candidate(
                                    operator_candidates, left_arg_type, right_arg_type, symtab);
                                if (best_exact != NULL)
                                    operator_node = best_exact;

                                HashNode_t *best_match = NULL;
                                int num_best = 0;
                                ListNode_t *args_given = NULL;
                                if (operator_node == NULL)
                                {
                                    args_given = CreateListNode(expr1, LIST_EXPR);
                                    if (args_given != NULL)
                                    {
                                        args_given->next = CreateListNode(expr2, LIST_EXPR);
                                        int resolve_status = semcheck_resolve_overload(
                                            &best_match,
                                            &num_best,
                                            operator_candidates,
                                            args_given,
                                            symtab,
                                            expr,
                                            max_scope_lev,
                                            1);
                                        if (resolve_status == 0 && best_match != NULL)
                                            operator_node = best_match;
                                        DestroyList(args_given);
                                    }
                                }
                                if (operator_node == NULL)
                                {
                                    for (ListNode_t *cur = operator_candidates; cur != NULL; cur = cur->next)
                                    {
                                        HashNode_t *candidate = (HashNode_t *)cur->cur;
                                        if (candidate != NULL &&
                                            (candidate->hash_type == HASHTYPE_FUNCTION ||
                                             candidate->hash_type == HASHTYPE_PROCEDURE))
                                        {
                                            operator_node = candidate;
                                            break;
                                        }
                                    }
                                }
                                DestroyList(operator_candidates);
                            }
                            if (operator_node == NULL &&
                                semcheck_find_ident_by_prefix_visible(&operator_node, symtab, operator_method) >= 0 &&
                                operator_node != NULL)
                            {
                                /* fallback for return-type-disambiguated names not indexed by base id */
                            }
                            if (operator_node != NULL)
                            {
                                if (operator_node->type != NULL && kgpc_type_is_procedure(operator_node->type))
                                {
                                    KgpcType *return_type = kgpc_type_get_return_type(operator_node->type);
                                    if (return_type != NULL)
                                    {
                                        /* Transform expression from RELOP to FUNCTION_CALL */
                                        expr->type = EXPR_FUNCTION_CALL;
                                        memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
                                        expr->expr_data.function_call_data.is_operator_call = 1;

                                        expr->expr_data.function_call_data.id = strdup(operator_method);
                                        /* Use the actual mangled name from the symbol table */
                                        if (operator_node->mangled_id != NULL)
                                            expr->expr_data.function_call_data.mangled_id = strdup(operator_node->mangled_id);
                                        else
                                            expr->expr_data.function_call_data.mangled_id = strdup(operator_method);
                                        
                                        ListNode_t *arg1 = CreateListNode(expr1, LIST_EXPR);
                                        ListNode_t *arg2 = CreateListNode(expr2, LIST_EXPR);
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
                /* FPC allows comparing strings with chars (e.g. shortstring = char). */
                if (!string_ok)
                {
                    if ((is_string_type(type_first) && type_second == CHAR_TYPE) ||
                        (type_first == CHAR_TYPE && is_string_type(type_second)))
                        string_ok = 1;
                }
                int char_ok = (type_first == CHAR_TYPE && type_second == CHAR_TYPE) ||
                              (type_first == CHAR_TYPE && is_integer_type(type_second)) ||
                              (is_integer_type(type_first) && type_second == CHAR_TYPE);
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
                int enum_ok = ((type_first == ENUM_TYPE || is_integer_type(type_first)) &&
                               (type_second == ENUM_TYPE || is_integer_type(type_second)) &&
                               (type_first == ENUM_TYPE || type_second == ENUM_TYPE));

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

                /* Check for dynamic array or pointer compared with nil.
                 * In Pascal, dynamic arrays can be compared with nil to check if they are empty/uninitialized.
                 * A := nil; if A = nil then ...
                 * Also, any pointer (including class/interface pointers) can be compared with nil. */
                int dynarray_nil_ok = 0;
                int pointer_nil_ok = 0;
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
                        int is_pointer1 = (kgpc1->kind == TYPE_KIND_POINTER);
                        int is_pointer2 = (kgpc2->kind == TYPE_KIND_POINTER);
                        int is_record1 = (kgpc1->kind == TYPE_KIND_RECORD);
                        int is_record2 = (kgpc2->kind == TYPE_KIND_RECORD);
                        
                        if ((is_dynarray1 && is_nil2) || (is_nil1 && is_dynarray2))
                            dynarray_nil_ok = 1;
                        /* Allow pointer = nil or nil = pointer comparisons.
                         * Also allow class/interface (record) = nil comparisons since classes are pointer types. */
                        if ((is_pointer1 && is_nil2) || (is_nil1 && is_pointer2) ||
                            (is_record1 && is_nil2) || (is_nil1 && is_record2))
                            pointer_nil_ok = 1;
                    }
                }
                
                int record_ok = (type_first == RECORD_TYPE && type_second == RECORD_TYPE);
                int set_ok = (type_first == SET_TYPE && type_second == SET_TYPE);
                if (!set_ok && expr1 != NULL && expr2 != NULL)
                {
                    KgpcType *kgpc1 = expr1->resolved_kgpc_type;
                    KgpcType *kgpc2 = expr2->resolved_kgpc_type;
                    struct TypeAlias *alias1 = kgpc1 != NULL ? kgpc_type_get_type_alias(kgpc1) : NULL;
                    struct TypeAlias *alias2 = kgpc2 != NULL ? kgpc_type_get_type_alias(kgpc2) : NULL;
                    if ((alias1 != NULL && alias1->is_set) ||
                        (alias2 != NULL && alias2->is_set) ||
                        (kgpc1 != NULL && kgpc1->kind == TYPE_KIND_PRIMITIVE &&
                         kgpc1->info.primitive_type_tag == SET_TYPE) ||
                        (kgpc2 != NULL && kgpc2->kind == TYPE_KIND_PRIMITIVE &&
                         kgpc2->info.primitive_type_tag == SET_TYPE))
                    {
                        if ((alias1 != NULL && alias1->is_set) || kgpc1 == NULL ||
                            (alias2 != NULL && alias2->is_set) || kgpc2 == NULL ||
                            relop_type == EQ || relop_type == NE ||
                            relop_type == LE || relop_type == GE)
                        {
                            set_ok = 1;
                        }
                    }
                }
                int class_record_ok = 0;
                if (!record_ok && expr1 != NULL && expr2 != NULL)
                {
                    KgpcType *kgpc1 = expr1->resolved_kgpc_type;
                    KgpcType *kgpc2 = expr2->resolved_kgpc_type;
                    if (kgpc1 != NULL && kgpc2 != NULL)
                    {
                        struct RecordType *rec1 = NULL;
                        struct RecordType *rec2 = NULL;
                        int rec1_is_class = 0;
                        int rec2_is_class = 0;

                        if (kgpc1->kind == TYPE_KIND_RECORD)
                        {
                            rec1 = kgpc1->info.record_info;
                            rec1_is_class = record_type_is_class(rec1);
                        }
                        else if (kgpc1->kind == TYPE_KIND_POINTER &&
                                 kgpc1->info.points_to != NULL &&
                                 kgpc1->info.points_to->kind == TYPE_KIND_RECORD)
                        {
                            rec1 = kgpc1->info.points_to->info.record_info;
                            rec1_is_class = record_type_is_class(rec1);
                        }

                        if (kgpc2->kind == TYPE_KIND_RECORD)
                        {
                            rec2 = kgpc2->info.record_info;
                            rec2_is_class = record_type_is_class(rec2);
                        }
                        else if (kgpc2->kind == TYPE_KIND_POINTER &&
                                 kgpc2->info.points_to != NULL &&
                                 kgpc2->info.points_to->kind == TYPE_KIND_RECORD)
                        {
                            rec2 = kgpc2->info.points_to->info.record_info;
                            rec2_is_class = record_type_is_class(rec2);
                        }

                        if (rec1 != NULL && rec2 != NULL && rec1_is_class != rec2_is_class)
                        {
                            struct RecordType *nonclass = rec1_is_class ? rec2 : rec1;
                            if (nonclass != NULL && nonclass->fields == NULL &&
                                nonclass->record_properties == NULL && !nonclass->is_type_helper)
                            {
                                class_record_ok = 1;
                            }
                        }
                    }
                }
                int unknown_nil_ok = 0;
                if (!numeric_ok && !boolean_ok && !string_ok && !char_ok && !pointer_ok &&
                    !enum_ok && !string_pchar_ok && !dynarray_nil_ok && !pointer_nil_ok &&
                    !record_ok && !set_ok && !class_record_ok &&
                    (type_first == UNKNOWN_TYPE || type_second == UNKNOWN_TYPE) &&
                    expr1 != NULL && expr2 != NULL)
                {
                    KgpcType *kgpc1 = expr1->resolved_kgpc_type;
                    KgpcType *kgpc2 = expr2->resolved_kgpc_type;
                    if (type_first == UNKNOWN_TYPE && kgpc2 != NULL &&
                        kgpc2->kind == TYPE_KIND_POINTER && kgpc2->info.points_to == NULL)
                        unknown_nil_ok = 1;
                    else if (type_second == UNKNOWN_TYPE && kgpc1 != NULL &&
                        kgpc1->kind == TYPE_KIND_POINTER && kgpc1->info.points_to == NULL)
                        unknown_nil_ok = 1;
                }

                /* Fallback for record/scalar equality where only implicit casts are
                 * available at this point (e.g. TUInt24Rec overload bodies in FPC RTL).
                 * Try to cast the record operand to the scalar operand type and re-check. */
                if (!numeric_ok && !boolean_ok && !string_ok && !char_ok && !pointer_ok &&
                    !enum_ok && !string_pchar_ok && !dynarray_nil_ok && !pointer_nil_ok &&
                    !record_ok && !set_ok && !class_record_ok && !unknown_nil_ok &&
                    expr1 != NULL && expr2 != NULL)
                {
                    if (type_first == RECORD_TYPE && type_second != UNKNOWN_TYPE &&
                        type_second != RECORD_TYPE)
                    {
                        const char *target_name = get_expr_type_name(expr2, symtab);
                        if (target_name == NULL || target_name[0] == '\0')
                            target_name = semcheck_type_tag_pascal_name(type_second);
                        if (target_name != NULL)
                        {
                            struct Expression *cast_expr = mk_typecast(expr1->line_num,
                                type_second, strdup(target_name), expr1);
                            if (cast_expr != NULL)
                            {
                                expr->expr_data.relop_data.left = cast_expr;
                                return semcheck_relop(type_return, symtab, expr, max_scope_lev, mutating);
                            }
                        }
                    }
                    else if (type_second == RECORD_TYPE && type_first != UNKNOWN_TYPE &&
                        type_first != RECORD_TYPE)
                    {
                        const char *target_name = get_expr_type_name(expr1, symtab);
                        if (target_name == NULL || target_name[0] == '\0')
                            target_name = semcheck_type_tag_pascal_name(type_first);
                        if (target_name != NULL)
                        {
                            struct Expression *cast_expr = mk_typecast(expr2->line_num,
                                type_first, strdup(target_name), expr2);
                            if (cast_expr != NULL)
                            {
                                expr->expr_data.relop_data.right = cast_expr;
                                return semcheck_relop(type_return, symtab, expr, max_scope_lev, mutating);
                            }
                        }
                    }
                }
                
                if (!numeric_ok && !boolean_ok && !string_ok && !char_ok && !pointer_ok && !enum_ok && !string_pchar_ok && !dynarray_nil_ok && !pointer_nil_ok
                    && !record_ok && !set_ok && !class_record_ok && !unknown_nil_ok
                    && type_first != VARIANT_TYPE && type_second != VARIANT_TYPE
                    && type_first != UNKNOWN_TYPE && type_second != UNKNOWN_TYPE
                    && type_first != RECORD_TYPE && type_second != RECORD_TYPE
                    && !((is_integer_type(type_first) && type_second == POINTER_TYPE) ||
                         (type_first == POINTER_TYPE && is_integer_type(type_second))))
                {
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, equality comparison requires matching numeric, boolean, string, character, or pointer types!\n\n",
                        expr->line_num);
                    ++return_val;
                }
            }
            else
            {
                if (type_first == RECORD_TYPE || type_second == RECORD_TYPE)
                {
                    struct Expression *record_expr = (type_first == RECORD_TYPE) ? expr1 : expr2;
                    const char *record_type_name = get_expr_type_name(record_expr, symtab);
                    const char *op_suffix = NULL;
                    switch (relop_type)
                    {
                        case LT: op_suffix = "op_lt"; break;
                        case LE: op_suffix = "op_le"; break;
                        case GT: op_suffix = "op_gt"; break;
                        case GE: op_suffix = "op_ge"; break;
                        default: break;
                    }
                    if (record_type_name != NULL && op_suffix != NULL)
                    {
                        size_t name_len = strlen(record_type_name) + strlen(op_suffix) + 3;
                        char *operator_method = (char *)malloc(name_len);
                        if (operator_method != NULL)
                        {
                            snprintf(operator_method, name_len, "%s__%s", record_type_name, op_suffix);
                            HashNode_t *operator_node = NULL;
                            ListNode_t *operator_candidates = FindAllIdents(symtab, operator_method);
                            if (operator_candidates != NULL)
                            {
                                HashNode_t *best_match = NULL;
                                int num_best = 0;
                                ListNode_t *args_given = CreateListNode(expr1, LIST_EXPR);
                                if (args_given != NULL)
                                {
                                    args_given->next = CreateListNode(expr2, LIST_EXPR);
                                    if (semcheck_resolve_overload(&best_match, &num_best,
                                            operator_candidates, args_given, symtab, expr,
                                            max_scope_lev, 1) == 0 && best_match != NULL)
                                    {
                                        operator_node = best_match;
                                    }
                                    DestroyList(args_given);
                                }
                                if (operator_node == NULL)
                                {
                                    for (ListNode_t *cur = operator_candidates; cur != NULL; cur = cur->next)
                                    {
                                        HashNode_t *candidate = (HashNode_t *)cur->cur;
                                        if (candidate != NULL &&
                                            (candidate->hash_type == HASHTYPE_FUNCTION ||
                                             candidate->hash_type == HASHTYPE_PROCEDURE))
                                        {
                                            operator_node = candidate;
                                            break;
                                        }
                                    }
                                }
                                DestroyList(operator_candidates);
                            }
                            if (operator_node == NULL &&
                                semcheck_find_ident_by_prefix_visible(&operator_node, symtab, operator_method) >= 0 &&
                                operator_node != NULL)
                            {
                                /* fallback */
                            }
                            if (operator_node != NULL && operator_node->type != NULL &&
                                kgpc_type_is_procedure(operator_node->type))
                            {
                                KgpcType *return_type = kgpc_type_get_return_type(operator_node->type);
                                if (return_type != NULL)
                                {
                                    expr->type = EXPR_FUNCTION_CALL;
                                    expr->expr_data.function_call_data.is_operator_call = 1;
                                    memset(&expr->expr_data.function_call_data, 0,
                                        sizeof(expr->expr_data.function_call_data));
                                    expr->expr_data.function_call_data.id = strdup(operator_method);
                                    expr->expr_data.function_call_data.mangled_id =
                                        operator_node->mangled_id != NULL
                                            ? strdup(operator_node->mangled_id)
                                            : strdup(operator_method);
                                    ListNode_t *arg1 = CreateListNode(expr1, LIST_EXPR);
                                    ListNode_t *arg2 = CreateListNode(expr2, LIST_EXPR);
                                    arg1->next = arg2;
                                    expr->expr_data.function_call_data.args_expr = arg1;
                                    expr->expr_data.function_call_data.resolved_func = operator_node;
                                    expr->expr_data.function_call_data.call_hash_type = HASHTYPE_FUNCTION;
                                    expr->expr_data.function_call_data.call_kgpc_type = operator_node->type;
                                    kgpc_type_retain(operator_node->type);
                                    expr->expr_data.function_call_data.is_call_info_valid = 1;
                                    if (expr->resolved_kgpc_type != NULL)
                                        destroy_kgpc_type(expr->resolved_kgpc_type);
                                    expr->resolved_kgpc_type = return_type;
                                    kgpc_type_retain(return_type);
                                    free(operator_method);
                                    *type_return = BOOL;
                                    return return_val;
                                }
                            }
                            free(operator_method);
                        }
                    }
                }
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
                /* FPC allows comparing strings with chars. */
                if (!string_ok)
                {
                    if ((is_string_type(type_first) && type_second == CHAR_TYPE) ||
                        (type_first == CHAR_TYPE && is_string_type(type_second)))
                        string_ok = 1;
                }
                int char_ok = (type_first == CHAR_TYPE && type_second == CHAR_TYPE) ||
                              (type_first == CHAR_TYPE && is_integer_type(type_second)) ||
                              (is_integer_type(type_first) && type_second == CHAR_TYPE);
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
                int enum_ok = ((type_first == ENUM_TYPE || is_integer_type(type_first)) &&
                               (type_second == ENUM_TYPE || is_integer_type(type_second)) &&
                               (type_first == ENUM_TYPE || type_second == ENUM_TYPE));

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

                /* Check for dynamic array or pointer compared with nil */
                int dynarray_nil_ok = 0;
                int pointer_nil_ok = 0;
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
                        int is_pointer1 = (kgpc1->kind == TYPE_KIND_POINTER);
                        int is_pointer2 = (kgpc2->kind == TYPE_KIND_POINTER);
                        int is_record1 = (kgpc1->kind == TYPE_KIND_RECORD);
                        int is_record2 = (kgpc2->kind == TYPE_KIND_RECORD);
                        
                        if ((is_dynarray1 && is_nil2) || (is_nil1 && is_dynarray2))
                            dynarray_nil_ok = 1;
                        if ((is_pointer1 && is_nil2) || (is_nil1 && is_pointer2) ||
                            (is_record1 && is_nil2) || (is_nil1 && is_record2))
                            pointer_nil_ok = 1;
                    }
                }

                int record_ok = (type_first == RECORD_TYPE && type_second == RECORD_TYPE);
                int set_ok = 0;
                if (relop_type == LE || relop_type == GE)
                {
                    set_ok = (type_first == SET_TYPE && type_second == SET_TYPE);
                    if (!set_ok && expr1 != NULL && expr2 != NULL)
                    {
                        KgpcType *kgpc1 = expr1->resolved_kgpc_type;
                        KgpcType *kgpc2 = expr2->resolved_kgpc_type;
                        struct TypeAlias *alias1 = kgpc1 != NULL ? kgpc_type_get_type_alias(kgpc1) : NULL;
                        struct TypeAlias *alias2 = kgpc2 != NULL ? kgpc_type_get_type_alias(kgpc2) : NULL;
                        if ((alias1 != NULL && alias1->is_set) ||
                            (alias2 != NULL && alias2->is_set) ||
                            (kgpc1 != NULL && kgpc1->kind == TYPE_KIND_PRIMITIVE &&
                             kgpc1->info.primitive_type_tag == SET_TYPE) ||
                            (kgpc2 != NULL && kgpc2->kind == TYPE_KIND_PRIMITIVE &&
                             kgpc2->info.primitive_type_tag == SET_TYPE))
                        {
                            set_ok = 1;
                        }
                    }
                }

                /* Record/scalar implicit conversion fallback for ordering relops.
                 * Same pattern as the equality relop fallback above (lines ~925-959). */
                if (!numeric_ok && !string_ok && !char_ok && !pointer_ok && !enum_ok &&
                    !string_pchar_ok && !dynarray_nil_ok && !pointer_nil_ok &&
                    !record_ok && !set_ok &&
                    expr1 != NULL && expr2 != NULL)
                {
                    if (type_first == RECORD_TYPE && type_second != UNKNOWN_TYPE &&
                        type_second != RECORD_TYPE)
                    {
                        const char *target_name = get_expr_type_name(expr2, symtab);
                        if (target_name == NULL || target_name[0] == '\0')
                            target_name = semcheck_type_tag_pascal_name(type_second);
                        if (target_name != NULL)
                        {
                            struct Expression *cast_expr = mk_typecast(expr1->line_num,
                                type_second, strdup(target_name), expr1);
                            if (cast_expr != NULL)
                            {
                                expr->expr_data.relop_data.left = cast_expr;
                                return semcheck_relop(type_return, symtab, expr, max_scope_lev, mutating);
                            }
                        }
                    }
                    else if (type_second == RECORD_TYPE && type_first != UNKNOWN_TYPE &&
                        type_first != RECORD_TYPE)
                    {
                        const char *target_name = get_expr_type_name(expr1, symtab);
                        if (target_name == NULL || target_name[0] == '\0')
                            target_name = semcheck_type_tag_pascal_name(type_first);
                        if (target_name != NULL)
                        {
                            struct Expression *cast_expr = mk_typecast(expr2->line_num,
                                type_first, strdup(target_name), expr2);
                            if (cast_expr != NULL)
                            {
                                expr->expr_data.relop_data.right = cast_expr;
                                return semcheck_relop(type_return, symtab, expr, max_scope_lev, mutating);
                            }
                        }
                    }
                }

                if(!numeric_ok && !string_ok && !char_ok && !pointer_ok && !enum_ok && !string_pchar_ok && !dynarray_nil_ok && !pointer_nil_ok
                    && !record_ok && !set_ok
                    && type_first != VARIANT_TYPE && type_second != VARIANT_TYPE
                    && type_first != UNKNOWN_TYPE && type_second != UNKNOWN_TYPE
                    && !((is_integer_type(type_first) && type_second == POINTER_TYPE) ||
                         (type_first == POINTER_TYPE && is_integer_type(type_second))))
                {
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index,
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
    if (*type_return == UNKNOWN_TYPE)
        return return_val;
    if (*type_return == POINTER_TYPE || *type_return == RECORD_TYPE)
        return return_val;
    if(!is_type_ir(type_return))
    {
        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, expected int or real after \"-\"!\n\n",
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
        if ((is_integer_type(type_first) || type_first == ENUM_TYPE ||
             type_first == POINTER_TYPE || type_first == RECORD_TYPE) &&
            (is_integer_type(type_second) || type_second == ENUM_TYPE ||
             type_second == POINTER_TYPE || type_second == RECORD_TYPE))
        {
            if (type_first == INT64_TYPE || type_second == INT64_TYPE)
                *type_return = INT64_TYPE;
            else if (type_first == QWORD_TYPE || type_second == QWORD_TYPE)
                *type_return = QWORD_TYPE;
            else if (type_first == LONGINT_TYPE || type_second == LONGINT_TYPE)
                *type_return = LONGINT_TYPE;
            else if (type_first == ENUM_TYPE && type_second == ENUM_TYPE)
                *type_return = ENUM_TYPE;
            else
                *type_return = INT_TYPE;
            return return_val;
        }
        if (type_first == UNKNOWN_TYPE || type_second == UNKNOWN_TYPE)
        {
            *type_return = UNKNOWN_TYPE;
            return return_val;
        }
        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, expected boolean or integer operands for OR expression!\n\n",
            expr->line_num);
        if (kgpc_getenv("KGPC_DEBUG_ANDOR") != NULL)
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
            semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, unsupported set additive operator.\n\n",
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
                                   semcheck_expr_is_char_array_like(expr1) ||
                                   semcheck_expr_is_char_pointer(expr1));
        int right_is_string_like = (is_string_type(type_second) || type_second == CHAR_TYPE ||
                                    semcheck_expr_is_char_array_like(expr2) ||
                                    semcheck_expr_is_char_pointer(expr2));

        if (left_is_string_like && right_is_string_like)
        {
            if (semcheck_expr_is_char_pointer(expr1))
                semcheck_promote_pointer_expr_to_string(expr1);
            if (semcheck_expr_is_char_pointer(expr2))
                semcheck_promote_pointer_expr_to_string(expr2);
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
            semcheck_set_pointer_info(expr, expr1->pointer_subtype, expr1->pointer_subtype_id);
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
            semcheck_set_pointer_info(expr, expr2->pointer_subtype, expr2->pointer_subtype_id);
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
            semcheck_set_pointer_info(expr, expr1->pointer_subtype, expr1->pointer_subtype_id);
            return return_val;
        }
    }

    /* Check for operator overloading when at least one operand is a record type. */
    if (type_first == RECORD_TYPE || type_second == RECORD_TYPE)
    {
        const char *left_type_name = get_expr_type_name(expr1, symtab);
        const char *right_type_name = get_expr_type_name(expr2, symtab);
        const char *record_type_name = NULL;
        if (type_first == RECORD_TYPE && left_type_name != NULL)
            record_type_name = left_type_name;
        else if (type_second == RECORD_TYPE && right_type_name != NULL)
            record_type_name = right_type_name;

        if (kgpc_getenv("KGPC_DEBUG_ADDOP") != NULL)
        {
            fprintf(stderr,
                "[SemCheck] addop record branch line %d: lhs_type_name=%s rhs_type_name=%s chosen=%s op=%d\n",
                expr->line_num,
                left_type_name != NULL ? left_type_name : "<null>",
                right_type_name != NULL ? right_type_name : "<null>",
                record_type_name != NULL ? record_type_name : "<null>",
                op_type);
        }

        if (record_type_name != NULL)
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
                size_t name_len = strlen(record_type_name) + strlen(op_suffix) + 3;
                char *operator_method = (char *)malloc(name_len);
                if (operator_method != NULL)
                {
                    snprintf(operator_method, name_len, "%s__%s", record_type_name, op_suffix);

                    HashNode_t *operator_node = NULL;
                    ListNode_t *operator_candidates = FindAllIdents(symtab, operator_method);
                    if (kgpc_getenv("KGPC_DEBUG_ADDOP") != NULL)
                    {
                        fprintf(stderr,
                            "[SemCheck] addop operator key=%s candidates=%d scope_unit=%d lookup_unit=%d\n",
                            operator_method,
                            ListLength(operator_candidates),
                            (symtab != NULL && symtab->current_scope != NULL) ? symtab->current_scope->unit_index : -1,
                            semcheck_operator_lookup_unit_index(symtab));
                    }
                    if (operator_candidates != NULL)
                    {
                        KgpcType *left_arg_type = expr1 != NULL ? expr1->resolved_kgpc_type : NULL;
                        KgpcType *right_arg_type = expr2 != NULL ? expr2->resolved_kgpc_type : NULL;
                        HashNode_t *best_exact = semcheck_find_best_binary_operator_candidate(
                            operator_candidates, left_arg_type, right_arg_type, symtab);
                        if (best_exact != NULL)
                            operator_node = best_exact;

                                                HashNode_t *best_match = NULL;
                        int num_best = 0;
                        if (operator_node == NULL)
                        {
                            ListNode_t *args_given = CreateListNode(expr1, LIST_EXPR);
                            if (args_given != NULL)
                            {
                                args_given->next = CreateListNode(expr2, LIST_EXPR);
                                int resolve_status = semcheck_resolve_overload(
                                    &best_match,
                                    &num_best,
                                    operator_candidates,
                                    args_given,
                                    symtab,
                                    expr,
                                    max_scope_lev,
                                    1);
                                if (resolve_status == 0 && best_match != NULL)
                                    operator_node = best_match;
                                DestroyList(args_given);
                            }
                        }
                        if (operator_node == NULL)
                        {
                            for (ListNode_t *cur = operator_candidates; cur != NULL; cur = cur->next)
                            {
                                HashNode_t *candidate = (HashNode_t *)cur->cur;
                                if (candidate != NULL &&
                                    (candidate->hash_type == HASHTYPE_FUNCTION ||
                                     candidate->hash_type == HASHTYPE_PROCEDURE))
                                {
                                    operator_node = candidate;
                                    break;
                                }
                            }
                        }
                        DestroyList(operator_candidates);
                    }
                    /* For binary operators, try exact lookup with second param type
                     * first to distinguish from unary operators with the same name. */
                    if (operator_node == NULL && right_type_name != NULL)
                    {
                        size_t exact_len = strlen(operator_method) + strlen(right_type_name) +
                            strlen(record_type_name) + 3;
                        char *operator_exact = (char *)malloc(exact_len);
                        if (operator_exact != NULL)
                        {
                            snprintf(operator_exact, exact_len, "%s_%s_%s",
                                operator_method, right_type_name, record_type_name);
                            FindSymbol(&operator_node, symtab, operator_exact);
                            free(operator_exact);
                        }
                    }
                    if (operator_node == NULL &&
                        semcheck_find_ident_by_prefix_visible(&operator_node, symtab, operator_method) >= 0 &&
                        operator_node != NULL)
                    {
                        /* Verify the prefix-matched operator has the right arity
                         * (binary operators need 2 params, not 1). */
                        if (operator_node->type != NULL)
                        {
                            ListNode_t *check_params = kgpc_type_get_procedure_params(operator_node->type);
                            int param_count = ListLength(check_params);
                            if (param_count == 1 && expr2 != NULL)
                            {
                                /* Found unary but need binary — reject this match */
                                operator_node = NULL;
                            }
                        }
                    }
                    if (operator_node == NULL)
                    {
                        size_t exact_len = strlen(operator_method) + strlen(record_type_name) + 2;
                        char *operator_exact = (char *)malloc(exact_len);
                        if (operator_exact != NULL)
                        {
                            snprintf(operator_exact, exact_len, "%s_%s", operator_method, record_type_name);
                            FindSymbol(&operator_node, symtab, operator_exact);
                            free(operator_exact);
                        }
                    }
                    if (operator_node != NULL)
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
                                expr->expr_data.function_call_data.is_operator_call = 1;

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

    /* Pointer arithmetic: pointer +/- integer yields pointer (FPC {$POINTERMATH ON}) */
    if ((type_first == POINTER_TYPE && is_integer_type(type_second)) ||
        (is_integer_type(type_first) && type_second == POINTER_TYPE))
    {
        *type_return = POINTER_TYPE;
        if (expr->resolved_kgpc_type != NULL)
            destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = create_primitive_type(POINTER_TYPE);
        return return_val;
    }
    /* Pointer - pointer yields integer (distance between two pointers) */
    if (type_first == POINTER_TYPE && type_second == POINTER_TYPE)
    {
        *type_return = INT64_TYPE;
        if (expr->resolved_kgpc_type != NULL)
            destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = create_primitive_type_with_size(INT64_TYPE, 8);
        return return_val;
    }
    /* Suppress cascading errors when either operand is already UNKNOWN_TYPE */
    if (type_first == UNKNOWN_TYPE || type_second == UNKNOWN_TYPE)
    {
        *type_return = UNKNOWN_TYPE;
        return return_val;
    }
    /* Allow pointer/record types to pass through as integer-like */
    if (type_first == RECORD_TYPE || type_second == RECORD_TYPE ||
        type_first == POINTER_TYPE || type_second == POINTER_TYPE)
    {
        *type_return = INT_TYPE;
        return return_val;
    }
    /* Checking numeric types */
    if(!types_numeric_compatible(type_first, type_second))
    {
        semantic_error(expr->line_num, expr->col_num, "type mismatch on addop: lhs is %s, rhs is %s",
            semcheck_type_tag_name(type_first), semcheck_type_tag_name(type_second));
        if (kgpc_getenv("KGPC_DEBUG_ADDOP") != NULL)
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
        if (kgpc_getenv("KGPC_DEBUG_ADDOP") != NULL)
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

    if (type_first == EXTENDED_TYPE || type_second == EXTENDED_TYPE)
        *type_return = EXTENDED_TYPE;
    else if (type_first == REAL_TYPE || type_second == REAL_TYPE)
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
        if (type_first == BOOL && type_second != BOOL && expr2 != NULL &&
            expr2->type == EXPR_FUNCTION_CALL)
        {
            semcheck_try_refine_funccall_to_bool(symtab, expr2, max_scope_lev, mutating, &type_second);
        }
        if (type_second == BOOL && type_first != BOOL && expr1 != NULL &&
            expr1->type == EXPR_FUNCTION_CALL)
        {
            semcheck_try_refine_funccall_to_bool(symtab, expr1, max_scope_lev, mutating, &type_first);
        }

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
        
        /* Integer/enum/pointer/record bitwise operations (enums are ordinal types in Pascal) */
        if ((is_integer_type(type_first) || type_first == ENUM_TYPE ||
             type_first == POINTER_TYPE || type_first == RECORD_TYPE) &&
            (is_integer_type(type_second) || type_second == ENUM_TYPE ||
             type_second == POINTER_TYPE || type_second == RECORD_TYPE))
        {
            /* Both operands are integers/enums - bitwise operation */
            /* INT64_TYPE/QWORD_TYPE take precedence as the largest integer types */
            if (type_first == INT64_TYPE || type_second == INT64_TYPE)
                *type_return = INT64_TYPE;
            else if (type_first == QWORD_TYPE || type_second == QWORD_TYPE)
                *type_return = QWORD_TYPE;
            else if (type_first == LONGINT_TYPE || type_second == LONGINT_TYPE)
                *type_return = LONGINT_TYPE;
            else if (type_first == ENUM_TYPE && type_second == ENUM_TYPE)
                *type_return = ENUM_TYPE;
            else
                *type_return = INT_TYPE;
            return return_val;
        }

        /* Suppress cascading errors from UNKNOWN_TYPE */
        if (type_first == UNKNOWN_TYPE || type_second == UNKNOWN_TYPE)
        {
            *type_return = UNKNOWN_TYPE;
            return return_val;
        }
        /* Invalid operand types for AND/XOR */
        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, expected boolean, integer, or set operands for %s expression!\n\n",
            expr->line_num, op_type == AND ? "AND" : "XOR");
        if (kgpc_getenv("KGPC_DEBUG_ANDOR") != NULL)
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
            semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, unsupported set multiplicative operator.\n\n",
                expr->line_num);
            ++return_val;
            *type_return = SET_TYPE;
        }
        return return_val;
    }

    /* Check for operator overloading when at least one operand is a record type. */
    if (type_first == RECORD_TYPE || type_second == RECORD_TYPE)
    {
        const char *left_type_name = get_expr_type_name(expr1, symtab);
        const char *right_type_name = get_expr_type_name(expr2, symtab);
        const char *record_type_name = NULL;
        if (type_first == RECORD_TYPE && left_type_name != NULL)
            record_type_name = left_type_name;
        else if (type_second == RECORD_TYPE && right_type_name != NULL)
            record_type_name = right_type_name;

        if (record_type_name != NULL)
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
                size_t name_len = strlen(record_type_name) + strlen(op_suffix) + 3;
                char *operator_method = (char *)malloc(name_len);
                if (operator_method != NULL)
                {
                    snprintf(operator_method, name_len, "%s__%s", record_type_name, op_suffix);

                    /* Look up the operator method in the symbol table.
                     * Try exact match first, then prefix match for return-type-suffixed names. */
                    HashNode_t *operator_node = NULL;
                    if (FindSymbol(&operator_node, symtab, operator_method) != 0 && operator_node != NULL)
                    {
                        /* exact match */
                    }
                    else if (semcheck_find_ident_by_prefix_visible(&operator_node, symtab, operator_method) >= 0 && operator_node != NULL)
                    {
                        /* prefix match — return-type-disambiguated name */
                    }
                    if (operator_node != NULL)
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
                                expr->expr_data.function_call_data.is_operator_call = 1;

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

    /* Suppress cascading errors when either operand is already UNKNOWN_TYPE */
    if (type_first == UNKNOWN_TYPE || type_second == UNKNOWN_TYPE)
    {
        *type_return = UNKNOWN_TYPE;
        return return_val;
    }
    /* Allow pointer/record types to pass through as integer-like */
    if (type_first == RECORD_TYPE || type_second == RECORD_TYPE ||
        type_first == POINTER_TYPE || type_second == POINTER_TYPE)
    {
        *type_return = INT_TYPE;
        return return_val;
    }
    /* Checking numeric types */
    if(!types_numeric_compatible(type_first, type_second))
    {
        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, type mismatch on mulop: lhs is %s, rhs is %s!\n\n",
            expr->line_num, semcheck_type_tag_name(type_first), semcheck_type_tag_name(type_second));
        ++return_val;
    }
    if(!is_type_ir(&type_first) || !is_type_ir(&type_second))
    {
        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, expected int/real on both sides of mulop, got lhs: %s, rhs: %s!\n\n",
            expr->line_num, semcheck_type_tag_name(type_first), semcheck_type_tag_name(type_second));
        ++return_val;
    }

    /* Handle DIV and MOD operators - integer division only */
    if (op_type == DIV || op_type == MOD)
    {
        if (type_first == REAL_TYPE || type_second == REAL_TYPE ||
            type_first == EXTENDED_TYPE || type_second == EXTENDED_TYPE)
        {
            semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, DIV and MOD operators require integer operands!\n\n",
                expr->line_num);
            ++return_val;
        }
        /* DIV and MOD produce integer results */
        /* INT64_TYPE/QWORD_TYPE take precedence as the largest integer types */
        if (type_first == INT64_TYPE || type_second == INT64_TYPE)
            *type_return = INT64_TYPE;
        else if (type_first == QWORD_TYPE || type_second == QWORD_TYPE)
            *type_return = QWORD_TYPE;
        else if (type_first == LONGINT_TYPE || type_second == LONGINT_TYPE)
            *type_return = LONGINT_TYPE;
        else
            *type_return = INT_TYPE;
        return return_val;
    }

    /* SLASH (/) always produces REAL_TYPE in Pascal, regardless of operand types */
    if (type_first == EXTENDED_TYPE || type_second == EXTENDED_TYPE)
        *type_return = EXTENDED_TYPE;
    else if (type_first == REAL_TYPE || type_second == REAL_TYPE || op_type == SLASH)
        *type_return = REAL_TYPE;
    else if (type_first == INT64_TYPE || type_second == INT64_TYPE)
        *type_return = INT64_TYPE;
    else if (type_first == QWORD_TYPE || type_second == QWORD_TYPE)
        *type_return = QWORD_TYPE;
    else if (type_first == LONGWORD_TYPE || type_second == LONGWORD_TYPE)
        *type_return = LONGWORD_TYPE;
    else if (type_first == LONGINT_TYPE || type_second == LONGINT_TYPE)
        *type_return = LONGINT_TYPE;
    else if (is_integer_type(type_first) && is_integer_type(type_second))
        /* FPC promotes narrow ordinal arithmetic/shift results to Integer. */
        *type_return = INT_TYPE;
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
            if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
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
        expr->expr_data.function_call_data.is_operator_call = 1;
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
                if (self_node != NULL && self_node->type != NULL)
                {
                    self_expr->resolved_kgpc_type = self_node->type;
                    kgpc_type_retain(self_node->type);
                }
                else
                {
                    KgpcType *self_record_type = create_record_type(self_record);
                    if (self_record_type != NULL)
                    {
                        self_expr->resolved_kgpc_type = self_record_type;
                    }
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
                if (self_node != NULL && self_node->type != NULL)
                {
                    self_expr->resolved_kgpc_type = self_node->type;
                    kgpc_type_retain(self_node->type);
                }
                else
                {
                    KgpcType *self_record_type = create_record_type(self_record);
                    if (self_record_type != NULL)
                    {
                        self_expr->resolved_kgpc_type = self_record_type;
                    }
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

static int semcheck_try_self_field_access(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev, int mutating,
    HashNode_t *self_node, struct RecordType *self_record, const char *id)
{
    if (type_return == NULL || symtab == NULL || expr == NULL || id == NULL ||
        self_node == NULL || self_record == NULL)
        return -1;

    int preferred_scope = 0;
    if (semcheck_find_preferred_value_ident(symtab, id, &preferred_scope) != NULL &&
        preferred_scope == 0)
    {
        return -1;
    }

    struct RecordType *field_owner = NULL;
    struct RecordField *field = semcheck_find_class_field_including_hidden(symtab,
        self_record, id, &field_owner);
    if (kgpc_getenv("KGPC_DEBUG_MONITOR") != NULL &&
        id != NULL && pascal_identifier_equals(id, "_MonitorData"))
    {
        fprintf(stderr,
            "[KGPC_DEBUG_MONITOR] try_self_field record=%s field=%s found=%d owner=%s\n",
            self_record != NULL && self_record->type_id != NULL ? self_record->type_id : "<null>",
            id,
            field != NULL,
            field_owner != NULL && field_owner->type_id != NULL ? field_owner->type_id : "<null>");
        if (field == NULL && self_record != NULL && self_record->fields != NULL)
        {
            int field_count = 0;
            for (ListNode_t *cur = self_record->fields; cur != NULL && field_count < 5; cur = cur->next)
            {
                if (cur->type == LIST_RECORD_FIELD && cur->cur != NULL)
                {
                    struct RecordField *rf = (struct RecordField *)cur->cur;
                    fprintf(stderr,
                        "  field[%d]=%s hidden=%d\n",
                        field_count,
                        rf->name != NULL ? rf->name : "<null>",
                        rf->is_hidden);
                    field_count++;
                }
            }
        }
    }
    if (field == NULL)
    {
        const char *trace_nonlocal = kgpc_getenv("KGPC_TRACE_NONLOCAL");
        if (trace_nonlocal != NULL && id != NULL && pascal_identifier_equals(id, trace_nonlocal))
        {
            fprintf(stderr,
                "[KGPC_TRACE_NONLOCAL] self-field miss id=%s record=%s self_node=%p self_type=%s\n",
                id,
                self_record != NULL && self_record->type_id != NULL ? self_record->type_id : "<null>",
                (void *)self_node,
                self_node != NULL && self_node->type != NULL ? kgpc_type_to_string(self_node->type) : "<null>");
        }
        return -1;
    }

    {
        const char *trace_nonlocal = kgpc_getenv("KGPC_TRACE_NONLOCAL");
        if (trace_nonlocal != NULL && id != NULL && pascal_identifier_equals(id, trace_nonlocal))
        {
            fprintf(stderr,
                "[KGPC_TRACE_NONLOCAL] self-field hit id=%s record=%s owner=%s\n",
                id,
                self_record != NULL && self_record->type_id != NULL ? self_record->type_id : "<null>",
                field_owner != NULL && field_owner->type_id != NULL ? field_owner->type_id : "<null>");
        }
    }

    char *self_str = strdup("Self");
    if (self_str == NULL)
        return -1;
    struct Expression *self_expr = mk_varid(expr->line_num, self_str);
    if (self_expr == NULL)
        return -1;

    if (self_node->type != NULL)
    {
        self_expr->resolved_kgpc_type = self_node->type;
        kgpc_type_retain(self_node->type);
    }
    else
    {
        KgpcType *self_record_type = create_record_type(self_record);
        if (self_record_type != NULL)
            self_expr->resolved_kgpc_type = self_record_type;
    }

    char *saved_id = expr->expr_data.id;
    expr->expr_data.id = NULL;
    expr->type = EXPR_RECORD_ACCESS;
    memset(&expr->expr_data.record_access_data, 0,
        sizeof(expr->expr_data.record_access_data));
    expr->expr_data.record_access_data.record_expr = self_expr;
    expr->expr_data.record_access_data.field_id = saved_id;

    return semcheck_recordaccess(type_return, symtab, expr, max_scope_lev, mutating);
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
    if (mutating != NO_MUTATE && id != NULL)
    {
        const char *cur_sub_id = semcheck_get_current_subprogram_id();
        const char *result_var = semcheck_get_current_subprogram_result_var_name();
        const char *method_name = semcheck_get_current_subprogram_method_name();
        if (kgpc_getenv("KGPC_DEBUG_RESULT_NAME") != NULL &&
            pascal_identifier_equals(id, "correct_fpuregister"))
        {
            fprintf(stderr,
                "[KGPC_DEBUG_RESULT_NAME] early id=%s mutating=%d cur=%s result=%s method=%s\n",
                id, mutating,
                cur_sub_id != NULL ? cur_sub_id : "<null>",
                result_var != NULL ? result_var : "<null>",
                method_name != NULL ? method_name : "<null>");
        }
        int is_result_name =
            (cur_sub_id != NULL && pascal_identifier_equals(id, cur_sub_id)) ||
            (result_var != NULL && pascal_identifier_equals(id, result_var)) ||
            (method_name != NULL && pascal_identifier_equals(id, method_name));
            if (is_result_name)
            {
                int owns_ret = 0;
                KgpcType *ret_type = semcheck_get_current_subprogram_return_kgpc_type(symtab, &owns_ret);
                if (ret_type != NULL)
                {
                    /* Only normalize the identifier to "Result" when there is
                     * no user-declared local variable with the same name,
                     * otherwise the codegen would pick up the local instead of
                     * the function return slot. */
                    const char *norm_result_var = semcheck_get_current_subprogram_result_var_name();
                    const char *norm_replacement = (norm_result_var != NULL && norm_result_var[0] != '\0')
                        ? norm_result_var : "Result";
                    int has_local_clash = 0;
                    if (!pascal_identifier_equals(id, norm_replacement))
                    {
                        HashNode_t *local_check = NULL;
                        int local_scope = 0;
                        local_check = semcheck_find_preferred_value_ident(symtab, norm_replacement, &local_scope);
                        if (local_check != NULL && local_scope == 0 &&
                            local_check->hash_type != HASHTYPE_FUNCTION_RETURN)
                            has_local_clash = 1;
                    }
                    if (!has_local_clash)
                    {
                        if (semcheck_normalize_result_identifier(expr) != 0)
                        {
                            if (owns_ret)
                                destroy_kgpc_type(ret_type);
                            return -1;
                        }
                    }
                    *type_return = semcheck_tag_from_kgpc(ret_type);
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, ret_type);
                    semcheck_set_result_expr_metadata(expr, symtab, ret_type);
                    if (owns_ret)
                        destroy_kgpc_type(ret_type);
                    return return_val;
                }
            }
    }
    if (kgpc_getenv("KGPC_DEBUG_EOF") != NULL && id != NULL &&
        pascal_identifier_equals(id, "EOF"))
    {
        fprintf(stderr, "[KGPC_DEBUG_EOF] varid EOF: mutating=%d scope=%d\n",
            mutating, max_scope_lev);
    }

    int direct_value_scope = 0;
    HashNode_t *direct_value_node = semcheck_find_preferred_value_ident(symtab, id,
        &direct_value_scope);
    int direct_current_scope_value = (direct_value_node != NULL && direct_value_scope == 0);

    struct Expression *with_expr = NULL;
    if (kgpc_getenv("KGPC_DEBUG_WITH") != NULL &&
        (pascal_identifier_equals(id, "BufPtr") ||
         pascal_identifier_equals(id, "Bytes") ||
         pascal_identifier_equals(id, "Chars")))
    {
        fprintf(stderr, "[KGPC_DEBUG_WITH] varid=%s with_context_count=%zu line=%d\n",
            id != NULL ? id : "(null)", with_context_count, expr->line_num);
    }
    int with_status = semcheck_with_try_resolve(id, symtab, &with_expr, expr->line_num);
    if (kgpc_getenv("KGPC_DEBUG_MONITOR") != NULL &&
        id != NULL && pascal_identifier_equals(id, "_MonitorData"))
    {
        fprintf(stderr,
            "[KGPC_DEBUG_MONITOR] varid=%s line=%d with_status=%d with_expr=%p\n",
            id, expr->line_num, with_status, (void *)with_expr);
    }
    if (kgpc_getenv("KGPC_DEBUG_WITH") != NULL &&
        (pascal_identifier_equals(id, "BufPtr") ||
         pascal_identifier_equals(id, "Bytes") ||
         pascal_identifier_equals(id, "Chars")))
    {
        fprintf(stderr, "[KGPC_DEBUG_WITH] varid=%s with_status=%d with_expr=%p line=%d\n",
            id != NULL ? id : "(null)", with_status, (void *)with_expr, expr->line_num);
    }
    if (with_status == 0 && with_expr != NULL && !direct_current_scope_value)
    {
        if (kgpc_getenv("KGPC_DEBUG_FIELD") != NULL) {
            fprintf(stderr, "[SemCheck] WITH resolved '%s' at line %d, with_expr->type=%d\n",
                id, expr->line_num, with_expr->type);
        }
        char *field_id = expr->expr_data.id;
        expr->type = EXPR_RECORD_ACCESS;
        expr->expr_data.record_access_data.record_expr = with_expr;
        expr->expr_data.record_access_data.field_id = field_id;
        expr->expr_data.record_access_data.field_offset = 0;
        return semcheck_recordaccess(type_return, symtab, expr, max_scope_lev, mutating);
    }
    if (with_status == 1 && with_context_count > 0 && id != NULL &&
        !direct_current_scope_value)
    {
        struct Expression *with_method_expr = NULL;
        int with_method_status = semcheck_with_try_resolve_method(id, symtab,
            &with_method_expr, expr->line_num);
        if ((with_method_status == 0 || with_method_status == 2) &&
            with_method_expr != NULL)
        {
            if (with_method_status == 2)
            {
                char *field_id = expr->expr_data.id;
                expr->type = EXPR_RECORD_ACCESS;
                expr->expr_data.record_access_data.record_expr = with_method_expr;
                expr->expr_data.record_access_data.field_id = field_id;
                expr->expr_data.record_access_data.field_offset = 0;
                return semcheck_recordaccess(type_return, symtab, expr, max_scope_lev, mutating);
            }

            ListNode_t *self_arg = CreateListNode(with_method_expr, LIST_EXPR);
            char *id_copy = strdup(id);
            char *placeholder_name = strdup(id);
            if (self_arg == NULL || id_copy == NULL || placeholder_name == NULL)
            {
                if (self_arg != NULL)
                    DestroyList(self_arg);
                else
                    destroy_expr(with_method_expr);
                free(id_copy);
                free(placeholder_name);
                return -1;
            }

            free(expr->expr_data.id);
            expr->type = EXPR_FUNCTION_CALL;
            memset(&expr->expr_data.function_call_data, 0,
                sizeof(expr->expr_data.function_call_data));
            expr->expr_data.function_call_data.id = id_copy;
            expr->expr_data.function_call_data.args_expr = self_arg;
            expr->expr_data.function_call_data.is_method_call_placeholder = 1;
            expr->expr_data.function_call_data.placeholder_method_name = placeholder_name;
            semcheck_reset_function_call_cache(expr);
            return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
        }
    }

    if (id == NULL)
    {
        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index,
            "Error on line %d, variable identifier is NULL.\n\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    scope_return = FindSymbol(&hash_return, symtab, id);

    /* When in assignment context, an enum constant cannot be the target.
     * If FindSymbol returned an enum constant (e.g. from a local enum that
     * shadows a module-level variable with the same name), look for an
     * assignable alternative (variable or typed constant) in broader scopes. */
    if (scope_return && mutating != NO_MUTATE && hash_return != NULL &&
        hash_return->hash_type == HASHTYPE_CONST && !hash_return->is_typed_const &&
        hash_return->type != NULL &&
        hash_return->type->kind == TYPE_KIND_PRIMITIVE &&
        kgpc_type_get_primitive_tag(hash_return->type) == ENUM_TYPE)
    {
        ListNode_t *all = FindAllIdents(symtab, id);
        for (ListNode_t *cur = all; cur != NULL; cur = cur->next)
        {
            HashNode_t *alt = (HashNode_t *)cur->cur;
            if (alt == NULL || alt == hash_return)
                continue;
            if (alt->hash_type == HASHTYPE_VAR ||
                alt->hash_type == HASHTYPE_FUNCTION_RETURN ||
                (alt->hash_type == HASHTYPE_CONST && alt->is_typed_const))
            {
                hash_return = alt;
                break;
            }
        }
        if (all != NULL)
            DestroyList(all);
    }

    if (kgpc_getenv("KGPC_DEBUG_EOF") != NULL && id != NULL &&
        pascal_identifier_equals(id, "EOF"))
    {
        fprintf(stderr,
            "[KGPC_DEBUG_EOF] FindIdent scope=%d node=%p hash_type=%d\n",
            scope_return, (void *)hash_return,
            hash_return != NULL ? hash_return->hash_type : -1);
    }
    if (kgpc_getenv("KGPC_DEBUG_MONITOR") != NULL &&
        id != NULL && pascal_identifier_equals(id, "_MonitorData"))
    {
        fprintf(stderr,
            "[KGPC_DEBUG_MONITOR] FindIdent id=%s scope=%d node=%p hash_type=%d\n",
            id, scope_return, (void *)hash_return,
            hash_return != NULL ? hash_return->hash_type : -1);
    }
    if (id != NULL)
    {
        const char *trace_nonlocal = kgpc_getenv("KGPC_TRACE_NONLOCAL");
        if (trace_nonlocal != NULL && pascal_identifier_equals(id, trace_nonlocal))
        {
            fprintf(stderr,
                "[KGPC_TRACE_NONLOCAL] sem_varid id=%s scope=%d hash=%p hash_type=%d subprogram=%s owner_ctx=%s\n",
                id,
                scope_return,
                (void *)hash_return,
                hash_return != NULL ? hash_return->hash_type : -1,
                semcheck_get_current_subprogram_id() != NULL ? semcheck_get_current_subprogram_id() : "<null>",
                semcheck_get_current_method_owner() != NULL ? semcheck_get_current_method_owner() : "<null>");
        }
    }
    /* When inside a class method, class fields take precedence over
     * outer-scope constants/types with the same name. If FindSymbol found
     * a non-variable symbol, check if Self has a field and prefer it. */
    if (scope_return && hash_return != NULL && id != NULL &&
        hash_return->hash_type != HASHTYPE_VAR &&
        hash_return->hash_type != HASHTYPE_FUNCTION_RETURN &&
        hash_return->hash_type != HASHTYPE_ARRAY)
    {
        HashNode_t *self_node = NULL;
        if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
        {
            struct RecordType *self_record = get_record_type_from_node(self_node);
            if (self_record == NULL)
                self_record = semcheck_resolve_helper_self_record(symtab,
                    self_node, self_record);
            if (self_record != NULL)
            {
                int field_result = semcheck_try_self_field_access(type_return, symtab, expr,
                    max_scope_lev, mutating, self_node, self_record, id);
                if (field_result >= 0)
                    return field_result;
            }
        }
    }
    if (!scope_return && id != NULL)
    {
        const char *owner = semcheck_get_current_method_owner();
        if (owner != NULL)
        {
            char mangled[256];
            snprintf(mangled, sizeof(mangled), "%s__%s", owner, id);
            HashNode_t *class_const = NULL;
            int class_const_scope = FindSymbol(&class_const, symtab, mangled);
            if (kgpc_getenv("KGPC_DEBUG_CLASS_CONST") != NULL)
            {
                fprintf(stderr, "[KGPC] class const lookup %s -> scope=%d node=%p\n",
                    mangled, class_const_scope, (void*)class_const);
            }
            if (class_const_scope && class_const != NULL &&
                (class_const->hash_type == HASHTYPE_CONST ||
                 class_const->hash_type == HASHTYPE_ARRAY ||
                 class_const->hash_type == HASHTYPE_VAR ||
                 class_const->is_typed_const))
            {
                if (expr->expr_data.id != NULL)
                    free(expr->expr_data.id);
                expr->expr_data.id = strdup(mangled);
                id = expr->expr_data.id;
                hash_return = class_const;
                scope_return = class_const_scope;
            }
            /* For nested types like HeapInc.ThreadState, also try the
             * outermost class: HeapInc__ConstName */
            if (!class_const_scope)
            {
                const char *outer = semcheck_get_current_subprogram_owner_class_outer();
                if (outer != NULL)
                {
                    char outer_mangled[256];
                    snprintf(outer_mangled, sizeof(outer_mangled), "%s__%s",
                             outer, id);
                    class_const = NULL;
                    class_const_scope = FindSymbol(&class_const, symtab, outer_mangled);
                    if (class_const_scope && class_const != NULL &&
                        (class_const->hash_type == HASHTYPE_CONST ||
                         class_const->hash_type == HASHTYPE_ARRAY ||
                         class_const->hash_type == HASHTYPE_VAR ||
                         class_const->is_typed_const))
                    {
                        if (expr->expr_data.id != NULL)
                            free(expr->expr_data.id);
                        expr->expr_data.id = strdup(outer_mangled);
                        id = expr->expr_data.id;
                        hash_return = class_const;
                        scope_return = class_const_scope;
                    }
                }
            }
        }
        /* Qualified identifier resolution: split dotted identifiers like
         * THorzRectAlign.Left into prefix (type) + suffix (member).
         * This handles scoped enum values and unit-qualified constants
         * in contexts where the parser produces a single flat identifier
         * (e.g. case labels). */
        if (!scope_return)
        {
            if (expr->id_ref != NULL && expr->id_ref->count >= 2)
            {
                const char *prefix = expr->id_ref->segments[0];
                const char *suffix = expr->id_ref->segments[expr->id_ref->count - 1];
                size_t prefix_len = strlen(prefix);

                HashNode_t *prefix_node = NULL;
                int prefix_scope = FindSymbol(&prefix_node, symtab, prefix);
                if (prefix_scope && prefix_node != NULL &&
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
                    size_t mangled_len = prefix_len + 2 + strlen(suffix) + 1;
                    char *mangled_qid = (char *)malloc(mangled_len);
                    assert(mangled_qid != NULL);
                    snprintf(mangled_qid, mangled_len, "%s__%s", prefix, suffix);
                    HashNode_t *class_const_node = NULL;
                    int cc_scope = FindSymbol(&class_const_node, symtab, mangled_qid);
                    if (cc_scope && class_const_node != NULL &&
                        (class_const_node->hash_type == HASHTYPE_CONST ||
                         class_const_node->hash_type == HASHTYPE_ARRAY ||
                         class_const_node->hash_type == HASHTYPE_VAR ||
                         class_const_node->is_typed_const))
                    {
                        if (expr->expr_data.id != NULL)
                            free(expr->expr_data.id);
                        expr->expr_data.id = strdup(mangled_qid);
                        free(mangled_qid);
                        id = expr->expr_data.id;
                        hash_return = class_const_node;
                        scope_return = cc_scope;
                        goto resolved;
                    }
                    free(mangled_qid);
                }
                else if (!prefix_scope || semcheck_is_unit_name(prefix))
                {
                    /* Prefix not found - might be a unit qualifier.
                     * Try looking up the suffix directly. */
                    HashNode_t *field_node = NULL;
                    if (FindSymbol(&field_node, symtab, suffix) != 0 && field_node != NULL)
                    {
                        if (field_node->hash_type == HASHTYPE_CONST)
                        {
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
                            char *field_copy = strdup(suffix);
                            assert(field_copy != NULL);
                            if (expr->expr_data.id != NULL)
                                free(expr->expr_data.id);
                            expr->expr_data.id = field_copy;
                            id = expr->expr_data.id;
                            scope_return = FindSymbol(&hash_return, symtab, id);
                            goto resolved;
                        }
                    }
                }
            }
        }
        /* Class property resolution in static methods: if the identifier
         * matches a class property of the enclosing record, rewrite it
         * to reference the backing field (read accessor). */
        if (!scope_return && owner != NULL)
        {
            HashNode_t *owner_node = NULL;
            if (FindSymbol(&owner_node, symtab, owner) != 0 && owner_node != NULL)
            {
                struct RecordType *owner_record = get_record_type_from_node(owner_node);
                if (owner_record != NULL)
                {
                    /* Search both 'properties' and 'record_properties' lists */
                    for (int pp = 0; pp < 2 && !scope_return; pp++)
                    {
                        ListNode_t *pnode = (pp == 0) ? owner_record->properties
                                                       : owner_record->record_properties;
                        while (pnode != NULL)
                        {
                            if (pnode->type == LIST_CLASS_PROPERTY && pnode->cur != NULL)
                            {
                                struct ClassProperty *cprop = (struct ClassProperty *)pnode->cur;
                                if (cprop->name != NULL &&
                                    pascal_identifier_equals(cprop->name, id) &&
                                    cprop->read_accessor != NULL)
                                {
                                    /* Rewrite identifier to reference the backing field */
                                    HashNode_t *accessor_node = NULL;
                                    int acc_scope = FindSymbol(&accessor_node, symtab,
                                        cprop->read_accessor);
                                    if (acc_scope && accessor_node != NULL)
                                    {
                                        char *new_id = strdup(cprop->read_accessor);
                                        assert(new_id != NULL);
                                        free(expr->expr_data.id);
                                        expr->expr_data.id = new_id;
                                        id = expr->expr_data.id;
                                        hash_return = accessor_node;
                                        scope_return = acc_scope;
                                    }
                                    break;
                                }
                            }
                            pnode = pnode->next;
                        }
                    }
                }
            }
        }
        if (!scope_return)
        {
            HashNode_t *self_node = NULL;
            if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
            {
                if (id != NULL)
                {
                    const char *trace_nonlocal = kgpc_getenv("KGPC_TRACE_NONLOCAL");
                    if (trace_nonlocal != NULL && pascal_identifier_equals(id, trace_nonlocal))
                    {
                        fprintf(stderr,
                            "[KGPC_TRACE_NONLOCAL] sem_varid self-fallback id=%s self_node=%p self_hash=%d\n",
                            id, (void *)self_node, self_node->hash_type);
                    }
                }
                struct RecordType *self_record = get_record_type_from_node(self_node);
                if (self_record == NULL)
                {
                    self_record = semcheck_resolve_helper_self_record(symtab,
                        self_node, self_record);
                }
                if (self_record != NULL)
                {
                    int field_result = semcheck_try_self_field_access(type_return, symtab, expr,
                        max_scope_lev, mutating, self_node, self_record, id);
                    if (field_result >= 0)
                        return field_result;
                    int helper_result = semcheck_try_helper_member(type_return, symtab, expr,
                        max_scope_lev, mutating, self_node, self_record, id);
                    if (helper_result >= 0)
                        return helper_result;
                }
            }
        }
resolved:;
    }
    if (kgpc_getenv("KGPC_DEBUG_RESULT") != NULL && id != NULL &&
        pascal_identifier_equals(id, "Result"))
    {
        fprintf(stderr,
            "[KGPC] semcheck_varid Result: scope_return=%d hash_return=%p hash_type=%d type=%s\n",
            scope_return,
            (void*)hash_return,
            hash_return != NULL ? hash_return->hash_type : -1,
            hash_return != NULL ? kgpc_type_to_string(hash_return->type) : "<null>");
    }
    if (scope_return && hash_return != NULL &&
        hash_return->hash_type == HASHTYPE_FUNCTION &&
        mutating == NO_MUTATE && with_status != 0)
    {
        int value_scope = scope_return;
        HashNode_t *value_node = semcheck_find_preferred_value_ident(symtab, id, &value_scope);
        if (value_node != NULL)
        {
            hash_return = value_node;
            scope_return = value_scope;
        }
    }
    if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && id != NULL &&
        pascal_identifier_equals(id, "Self"))
    {
        fprintf(stderr, "[KGPC] semcheck_varid: FindIdent Self scope_return=%d hash_return=%p kind=%d\n",
            scope_return, (void*)hash_return,
            hash_return && hash_return->type ? hash_return->type->kind : -1);
    }
    if (scope_return && with_status != 0 && id != NULL &&
        hash_return != NULL && hash_return->hash_type == HASHTYPE_FUNCTION)
    {
        HashNode_t *self_node = NULL;
        if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
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
                            if (self_node->type != NULL)
                            {
                                self_expr->resolved_kgpc_type = self_node->type;
                                kgpc_type_retain(self_node->type);
                            }
                            else
                            {
                                KgpcType *self_record_type = create_record_type(self_record);
                                if (self_record_type != NULL)
                                {
                                    self_expr->resolved_kgpc_type = self_record_type;
                                }
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
    if (FindSymbol(&helper_self_node, symtab, "Self") != 0 && helper_self_node != NULL)
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
        int is_value_symbol = (hash_return != NULL &&
            hash_return->hash_type != HASHTYPE_TYPE &&
            hash_return->hash_type != HASHTYPE_FUNCTION &&
            hash_return->hash_type != HASHTYPE_PROCEDURE &&
            hash_return->hash_type != HASHTYPE_BUILTIN_PROCEDURE);
        /* Skip Self-member resolution if the identifier is the current function's
         * own name (Pascal-style function result assignment: FuncName := value).
         * Otherwise, a method like TEReader.Pos sees 'Pos' on the LHS and
         * semcheck_try_helper_member resolves it as Self.Pos(), which then
         * collides with builtin Pos(). */
        const char *cur_sub_id = semcheck_get_current_subprogram_id();
        int is_func_result_name = 0;
        if (cur_sub_id != NULL) {
            const char *bare_name = semcheck_get_current_subprogram_method_name();
            const char *func_name = (bare_name != NULL) ? bare_name : cur_sub_id;
            if (pascal_identifier_equals(id, func_name))
                is_func_result_name = 1;
        }
        if (!is_func_result_name && !is_value_symbol)
        {
            struct RecordType *self_record = helper_self_record;
            if (self_record == NULL)
            {
                self_record = semcheck_resolve_helper_self_record(symtab,
                    helper_self_node, helper_self_record);
            }
            if (kgpc_getenv("KGPC_DEBUG_MONITOR") != NULL &&
                id != NULL && pascal_identifier_equals(id, "_MonitorData"))
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_MONITOR] scope>0 self_node=%p self_record=%p helper_context=%d\n",
                    (void *)helper_self_node, (void *)self_record, helper_context);
            }
            int field_result = semcheck_try_self_field_access(type_return, symtab, expr,
                max_scope_lev, mutating, helper_self_node, self_record, id);
            if (field_result >= 0)
                return field_result;
            int helper_result = semcheck_try_helper_member(type_return, symtab, expr,
                max_scope_lev, mutating, helper_self_node, self_record, id);
            if (helper_result >= 0)
                return helper_result;
        }
    }

    if (!scope_return)
    {
        if (!scope_return)
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
                    int getter_found = (FindSymbol(&getter_node, symtab, getter_id) != 0);
                    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
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
                        expr->expr_data.function_call_data.is_operator_call = 1;
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
                    if (kgpc_getenv("KGPC_DEBUG_MONITOR") != NULL &&
                        id != NULL && pascal_identifier_equals(id, "_MonitorData"))
                    {
                        fprintf(stderr,
                            "[KGPC_DEBUG_MONITOR] scope=-1 self_node=%p self_record=%p helper_context=%d\n",
                            (void *)self_node, (void *)self_record, helper_context);
                        if (self_record != NULL && self_record->type_id != NULL)
                        {
                            char mangled[256];
                            snprintf(mangled, sizeof(mangled), "%s__%s", self_record->type_id, id);
                            HashNode_t *mnode = NULL;
                            int mscope = FindSymbol(&mnode, symtab, mangled);
                            fprintf(stderr,
                                "[KGPC_DEBUG_MONITOR] mangled lookup %s scope=%d node=%p hash=%d\n",
                                mangled, mscope, (void *)mnode,
                                mnode != NULL ? mnode->hash_type : -1);
                        }
                    }
                    int field_result = semcheck_try_self_field_access(type_return, symtab, expr,
                        max_scope_lev, mutating, self_node, self_record, id);
                    if (field_result >= 0)
                        return field_result;
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
                                    expr->expr_data.function_call_data.is_operator_call = 1;
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

            /* Also try implicit Self member lookup for regular methods (not only
             * type helpers). This resolves bare identifiers like Size/TopLeft/
             * BottomRight in record/class methods before reporting undeclared id. */
            if (with_status != 0 && id != NULL)
            {
                HashNode_t *self_node = helper_self_node;
                if (self_node == NULL)
                    FindSymbol(&self_node, symtab, "Self");
                if (self_node != NULL)
                {
                    struct RecordType *self_record = get_record_type_from_node(self_node);
                    if (self_record == NULL)
                        self_record = semcheck_resolve_helper_self_record(symtab, self_node, NULL);
                    if (self_record == NULL)
                    {
                        const char *owner = semcheck_get_current_method_owner();
                        if (owner != NULL)
                            self_record = semcheck_lookup_record_type(symtab, owner);
                    }
                    if (self_record != NULL)
                    {
                        int member_result = semcheck_try_helper_member(type_return, symtab, expr,
                            max_scope_lev, mutating, self_node, self_record, id);
                        if (member_result >= 0)
                            return member_result;
                    }
                }
            }

            if (with_status == -1)
            {
                semantic_error(expr->line_num, expr->col_num,
                    "unable to resolve WITH context for field \"%s\"", id);
                ++return_val;
            }
            else if (id != NULL)
            {
                HashNode_t *type_fallback = semcheck_find_preferred_type_node(symtab, id);
                if (type_fallback == NULL && expr->id_ref != NULL &&
                    expr->id_ref->count > 1)
                {
                    char *qualified = qualified_ident_join(expr->id_ref, ".");
                    if (qualified != NULL)
                    {
                        type_fallback = semcheck_find_preferred_type_node(symtab, qualified);
                        free(qualified);
                    }
                }
                if (type_fallback != NULL && type_fallback->hash_type == HASHTYPE_TYPE)
                {
                    set_hash_meta(type_fallback, mutating);
                    set_type_from_hashtype(type_return, type_fallback);
                    if (type_fallback->type != NULL)
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, type_fallback->type);
                    return return_val;
                }
            }
            else if (id != NULL && semcheck_is_unit_name(id))
            {
                /* Unit identifiers may appear as the left side of unit-qualified
                 * expressions (UnitName.Member). Let record-access/type checking
                 * handle the qualified member without emitting a standalone
                 * undeclared-identifier error for the unit token itself. */
                *type_return = UNKNOWN_TYPE;
                semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
                return return_val;
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
        if (mutating != NO_MUTATE && id != NULL &&
            pascal_identifier_equals(id, "Result") &&
            hash_return != NULL && hash_return->type != NULL &&
            hash_return->hash_type != HASHTYPE_FUNCTION &&
            hash_return->hash_type != HASHTYPE_PROCEDURE)
        {
            set_hash_meta(hash_return, mutating);
            set_type_from_hashtype(type_return, hash_return);
            semcheck_expr_set_resolved_kgpc_type_shared(expr, hash_return->type);
            semcheck_set_result_expr_metadata(expr, symtab, hash_return->type);
            return return_val;
        }
        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL &&
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
            if (kgpc_getenv("KGPC_DEBUG_RESULT_NAME") != NULL &&
                id != NULL && pascal_identifier_equals(id, "correct_fpuregister"))
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_RESULT_NAME] late id=%s cur=%s result=%s method=%s hash_type=%d\n",
                    id,
                    semcheck_get_current_subprogram_id() != NULL ? semcheck_get_current_subprogram_id() : "<null>",
                    semcheck_get_current_subprogram_result_var_name() != NULL ? semcheck_get_current_subprogram_result_var_name() : "<null>",
                    semcheck_get_current_subprogram_method_name() != NULL ? semcheck_get_current_subprogram_method_name() : "<null>",
                    hash_return->hash_type);
            }
            /* In {$mode objfpc}, a bare function/method name without () inside
             * the function's own body refers to the result variable, not a
             * recursive call.  E.g. ReadNext(ReadAddress, sizeof(ReadAddress))
             * inside TEReader.ReadAddress should pass the result variable. */
            const char *_cur_sub_id = semcheck_get_current_subprogram_id();
            int _is_own_result = 0;
            if (_cur_sub_id != NULL && id != NULL)
            {
                const char *_bare = semcheck_get_current_subprogram_method_name();
                const char *_fname = (_bare != NULL) ? _bare : _cur_sub_id;
                if (pascal_identifier_equals(id, _fname))
                    _is_own_result = 1;
            }
            if (_is_own_result)
            {
                /* Only normalize when no user-declared local would shadow
                 * the function return slot. */
                const char *_norm_rv = semcheck_get_current_subprogram_result_var_name();
                const char *_norm_repl = (_norm_rv != NULL && _norm_rv[0] != '\0')
                    ? _norm_rv : "Result";
                int _has_local = 0;
                if (!pascal_identifier_equals(id, _norm_repl))
                {
                    HashNode_t *_lc = semcheck_find_preferred_value_ident(symtab, _norm_repl, NULL);
                    if (_lc != NULL && _lc->hash_type != HASHTYPE_FUNCTION_RETURN)
                        _has_local = 1;
                }
                if (!_has_local && semcheck_normalize_result_identifier(expr) != 0)
                    return -1;
                /* Treat as result variable: use the function's return type */
                KgpcType *ret_type = kgpc_type_get_return_type(hash_return->type);
                if (ret_type != NULL)
                {
                    *type_return = semcheck_tag_from_kgpc(ret_type);
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, ret_type);
                }
                else
                {
                    set_type_from_hashtype(type_return, hash_return);
                }
                return return_val;
            }

            /* Prefer implicit Self members over same-named global functions.
             * Example: inside TRectF methods, bare "BottomRight" should resolve
             * to Self.BottomRight (property) rather than a global BottomRight(). */
            if (id != NULL)
            {
                HashNode_t *self_node = NULL;
                if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
                {
                    struct RecordType *self_record = get_record_type_from_node(self_node);
                    if (self_record == NULL)
                        self_record = semcheck_resolve_helper_self_record(symtab, self_node, NULL);
                    if (self_record == NULL)
                    {
                        const char *owner = semcheck_get_current_method_owner();
                        if (owner != NULL)
                            self_record = semcheck_lookup_record_type(symtab, owner);
                    }
                    if (self_record != NULL)
                    {
                        int member_result = semcheck_try_helper_member(type_return, symtab, expr,
                            max_scope_lev, mutating, self_node, self_record, id);
                        if (member_result >= 0)
                            return member_result;
                    }
                }
            }

            /* Prefer helper properties when inside a type helper method body. */
            if (id != NULL)
            {
                HashNode_t *self_node = NULL;
                if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
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
                                    if (self_node->type != NULL)
                                    {
                                        self_expr->resolved_kgpc_type = self_node->type;
                                        kgpc_type_retain(self_node->type);
                                    }
                                    else
                                    {
                                        KgpcType *self_record_type = create_record_type(self_record);
                                        if (self_record_type != NULL)
                                        {
                                            self_expr->resolved_kgpc_type = self_record_type;
                                        }
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
                                        expr->expr_data.function_call_data.is_operator_call = 1;
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
            expr->expr_data.function_call_data.is_operator_call = 1;
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
            /* Inside a method body, a bare identifier matching a sibling method
             * may have been found as HASHTYPE_PROCEDURE (from the short-name
             * stub) when the real mangled entry is HASHTYPE_FUNCTION.
             * Check if Owner__Name is a function and treat as implicit call. */
            const char *owner = semcheck_get_current_method_owner();
            if (owner != NULL && id != NULL)
            {
                char mangled_method[512];
                snprintf(mangled_method, sizeof(mangled_method), "%s__%s", owner, id);
                HashNode_t *mangled_node = NULL;
                if (FindSymbol(&mangled_node, symtab, mangled_method) != 0 &&
                    mangled_node != NULL &&
                    mangled_node->hash_type == HASHTYPE_FUNCTION &&
                    mangled_node->type != NULL &&
                    kgpc_type_is_procedure(mangled_node->type))
                {
                    KgpcType *ret = kgpc_type_get_return_type(mangled_node->type);
                    if (ret != NULL)
                    {
                        /* Rewrite as implicit zero-arg function call */
                        *type_return = semcheck_tag_from_kgpc(ret);
                        char *call_id = strdup(mangled_method);
                        if (call_id != NULL)
                        {
                            free(expr->expr_data.id);
                            expr->expr_data.id = call_id;
                        }
                        expr->type = EXPR_FUNCTION_CALL;
                        memset(&expr->expr_data.function_call_data, 0,
                               sizeof(expr->expr_data.function_call_data));
                        expr->expr_data.function_call_data.id = strdup(mangled_method);
                        if (mangled_node->mangled_id != NULL)
                            expr->expr_data.function_call_data.mangled_id = strdup(mangled_node->mangled_id);
                        else
                            expr->expr_data.function_call_data.mangled_id = strdup(mangled_method);
                        expr->expr_data.function_call_data.args_expr = NULL;
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, ret);
                        return 0;
                    }
                }
            }
            /* Keep as EXPR_VAR_ID so it can be used as a procedure value */
            set_hash_meta(hash_return, mutating);
            set_type_from_hashtype(type_return, hash_return);
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
        semcheck_mark_static_link_needed(hash_return);
        if(0) /* scope depth check removed — tree scoping has no depth */
        {
            if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] semcheck_varid: scope_return=%d max_scope_lev=%d\n", scope_return, max_scope_lev);
            }
            if (mutating != NO_MUTATE &&
                hash_return->hash_type != HASHTYPE_CONST &&
                hash_return->hash_type != HASHTYPE_TYPE)
            {
                semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, cannot change \"%s\", invalid scope!\n",
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
            if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                fprintf(stderr, "[SemCheck] semcheck_varid: id=%s hash_type=%d node_type=%p kind=%d node_is_array=%d defined_in_unit=%d source_unit=%d\n",
                    id ? id : "<null>", hash_return->hash_type,
                    (void*)hash_return->type,
                    hash_return->type ? hash_return->type->kind : -1,
                    node_is_array,
                    hash_return->defined_in_unit,
                    hash_return->source_unit_index);
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
                semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, cannot assign \"%s\", is not a scalar variable!\n\n",
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

        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] semcheck_varid: expr=%p, id=%s, type_return=%d\n", (void*)expr, id, *type_return);
        }

        if (*type_return == POINTER_TYPE)
        {
            int subtype = UNKNOWN_TYPE;
            const char *type_id = NULL;
            struct TypeAlias *alias = get_type_alias_from_node(hash_return);
            int prefer_unit_pointer_targets = hash_return->defined_in_unit;
            if (alias != NULL && alias->alias_name != NULL)
            {
                HashNode_t *alias_type_node = semcheck_find_type_node_with_kgpc_type(symtab,
                    alias->alias_name);
                if (alias_type_node != NULL && alias_type_node->defined_in_unit)
                    prefer_unit_pointer_targets = 1;
            }
            if (!prefer_unit_pointer_targets && hash_return->type != NULL &&
                hash_return->type->type_alias != NULL &&
                hash_return->type->type_alias->alias_name != NULL)
            {
                HashNode_t *alias_type_node = semcheck_find_type_node_with_kgpc_type(symtab,
                    hash_return->type->type_alias->alias_name);
                if (alias_type_node != NULL && alias_type_node->defined_in_unit)
                    prefer_unit_pointer_targets = 1;
            }
            if (alias != NULL && alias->is_pointer)
            {
                subtype = alias->pointer_type;
                type_id = alias->pointer_type_id;
            }
            
            if (subtype == UNKNOWN_TYPE && hash_return->type != NULL &&
                kgpc_type_is_pointer(hash_return->type))
            {
                subtype = kgpc_type_get_pointer_subtype_tag(hash_return->type);
                /* Also try to get the type_id from the KgpcType's points_to info */
                if (type_id == NULL)
                {
                    KgpcType *pts = hash_return->type->info.points_to;
                    if (pts != NULL && pts->type_alias != NULL && pts->type_alias->alias_name != NULL)
                        type_id = pts->type_alias->alias_name;
                }
            }

            /* If the variable's own type has an alias_name (e.g. "PMyRec"),
             * look up the corresponding type node to get pointer target info */
            if (subtype == UNKNOWN_TYPE && alias != NULL && alias->alias_name != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindSymbol(&type_node, symtab, alias->alias_name) != 0 &&
                    type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                {
                    struct TypeAlias *type_alias = get_type_alias_from_node(type_node);
                    if (type_alias != NULL && type_alias->is_pointer)
                    {
                        subtype = type_alias->pointer_type;
                        type_id = type_alias->pointer_type_id;
                    }
                    /* Also check the type node's KgpcType for resolved pointer info */
                    if (subtype == UNKNOWN_TYPE && type_node->type != NULL &&
                        kgpc_type_is_pointer(type_node->type))
                    {
                        subtype = kgpc_type_get_pointer_subtype_tag(type_node->type);
                        if (type_id == NULL)
                        {
                            KgpcType *pts = type_node->type->info.points_to;
                            if (pts != NULL && pts->type_alias != NULL && pts->type_alias->alias_name != NULL)
                                type_id = pts->type_alias->alias_name;
                        }
                    }
                }
            }
            
            if (subtype == UNKNOWN_TYPE && type_id != NULL)
            {
                HashNode_t *target_node = semcheck_pick_type_node_with_origin_preference(symtab,
                    type_id, prefer_unit_pointer_targets);
                if (target_node == NULL &&
                    FindSymbol(&target_node, symtab, (char*)type_id) == 0)
                    target_node = NULL;
                if (target_node != NULL)
                {
                    set_type_from_hashtype(&subtype, target_node);
                }
                if (subtype == UNKNOWN_TYPE)
                    subtype = semcheck_map_builtin_type_name(symtab, type_id);
            }

            if (subtype == UNKNOWN_TYPE && type_id == NULL && alias != NULL &&
                alias->alias_name != NULL &&
                (alias->alias_name[0] == 'P' || alias->alias_name[0] == 'p') &&
                alias->alias_name[1] != '\0')
            {
                const char *candidate_type_id = alias->alias_name + 1;
                HashNode_t *candidate_node = NULL;
                if (FindSymbol(&candidate_node, symtab, (char *)candidate_type_id) != 0 &&
                    candidate_node != NULL)
                {
                    type_id = candidate_type_id;
                    set_type_from_hashtype(&subtype, candidate_node);
                }
                if (subtype == UNKNOWN_TYPE)
                {
                    subtype = semcheck_map_builtin_type_name(symtab, candidate_type_id);
                    if (subtype != UNKNOWN_TYPE)
                        type_id = candidate_type_id;
                }
                if (subtype == UNKNOWN_TYPE)
                    subtype = POINTER_TYPE;
            }

            if (subtype == RECORD_TYPE && type_id == NULL)
            {
                if (alias != NULL)
                {
                    if (alias->pointer_type_id != NULL)
                        type_id = alias->pointer_type_id;
                    else if (alias->target_type_id != NULL)
                        type_id = alias->target_type_id;
                }
                if (type_id == NULL && hash_return->type != NULL &&
                    hash_return->type->type_alias != NULL)
                {
                    struct TypeAlias *resolved_alias = hash_return->type->type_alias;
                    if (resolved_alias->pointer_type_id != NULL)
                        type_id = resolved_alias->pointer_type_id;
                    else if (resolved_alias->target_type_id != NULL)
                        type_id = resolved_alias->target_type_id;
                }
                if (type_id == NULL)
                {
                    struct RecordType *node_record = get_record_type_from_node(hash_return);
                    if (node_record != NULL && node_record->type_id != NULL)
                        type_id = node_record->type_id;
                }
                if (type_id == NULL && hash_return->type != NULL &&
                    kgpc_type_is_pointer(hash_return->type) &&
                    hash_return->type->info.points_to != NULL &&
                    kgpc_type_is_record(hash_return->type->info.points_to))
                {
                    struct RecordType *pointee_record =
                        kgpc_type_get_record(hash_return->type->info.points_to);
                    if (pointee_record != NULL && pointee_record->type_id != NULL)
                        type_id = pointee_record->type_id;
                }
            }
            
            if (subtype == UNKNOWN_TYPE && type_id == NULL && kgpc_getenv("KGPC_DEBUG_CG_ERR"))
            {
                fprintf(stderr, "[semcheck-debug] varid pointer UNKNOWN: id=%s line=%d hash_type=%d\n",
                    id, expr->line_num, hash_return->hash_type);
                fprintf(stderr, "[semcheck-debug]   alias=%p", (void*)alias);
                if (alias) fprintf(stderr, " alias_name=%s is_pointer=%d pointer_type=%d pointer_type_id=%s",
                    alias->alias_name ? alias->alias_name : "<null>",
                    alias->is_pointer, alias->pointer_type,
                    alias->pointer_type_id ? alias->pointer_type_id : "<null>");
                fprintf(stderr, "\n");
                fprintf(stderr, "[semcheck-debug]   hash_return->type=%p", (void*)hash_return->type);
                if (hash_return->type) fprintf(stderr, " kind=%d", hash_return->type->kind);
                fprintf(stderr, "\n");
            }
            semcheck_set_pointer_info(expr, subtype, type_id);
            if (expr->resolved_kgpc_type != NULL &&
                kgpc_type_is_pointer(expr->resolved_kgpc_type) &&
                expr->resolved_kgpc_type->info.points_to == NULL)
            {
                KgpcType *resolved_points_to = NULL;
                int points_to_owned = 0;

                if (expr->pointer_subtype_id != NULL)
                {
                    HashNode_t *target_node = semcheck_pick_type_node_with_origin_preference(symtab,
                        expr->pointer_subtype_id, prefer_unit_pointer_targets);
                    if (target_node == NULL)
                    {
                        target_node = semcheck_find_type_node_with_kgpc_type(symtab,
                            expr->pointer_subtype_id);
                    }
                    if (target_node != NULL && target_node->type != NULL)
                    {
                        resolved_points_to = target_node->type;
                        kgpc_type_retain(resolved_points_to);
                    }
                    else
                    {
                        struct RecordType *target_record = semcheck_lookup_record_type(symtab,
                            expr->pointer_subtype_id);
                        if (target_record != NULL)
                        {
                            resolved_points_to = create_record_type(target_record);
                            points_to_owned = 1;
                        }
                    }
                }

                if (resolved_points_to == NULL &&
                    expr->pointer_subtype != UNKNOWN_TYPE &&
                    expr->pointer_subtype != POINTER_TYPE)
                {
                    resolved_points_to = create_primitive_type(expr->pointer_subtype);
                    points_to_owned = 1;
                }

                if (resolved_points_to != NULL)
                {
                    KgpcType *resolved_pointer = create_pointer_type(resolved_points_to);
                    if (resolved_pointer != NULL)
                    {
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, resolved_pointer);
                        destroy_kgpc_type(resolved_pointer);
                    }
                    if (points_to_owned)
                        destroy_kgpc_type(resolved_points_to);
                }
            }
            if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
            {
                fprintf(stderr,
                    "[SemCheck] semcheck_varid: id=%s pointer_subtype=%d subtype_id=%s\n",
                    id ? id : "<null>", expr->pointer_subtype,
                    expr->pointer_subtype_id ? expr->pointer_subtype_id : "<null>");
            }
            if (hash_return->type != NULL && kgpc_type_is_pointer(hash_return->type))
            {
                KgpcType *points_to = hash_return->type->info.points_to;
                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_varid: id=%s, points_to=%p\n", id, points_to);
                    if (points_to) {
                         fprintf(stderr, "[SemCheck] semcheck_varid: points_to->kind=%d\n", points_to->kind);
                    }
                }
                if (points_to != NULL && kgpc_type_is_record(points_to) &&
                    kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_varid: id=%s, type=POINTER, points_to_record=%p\n",
                        id, (void *)kgpc_type_get_record(points_to));
                }
            }
        }
    }

    return return_val;
}
