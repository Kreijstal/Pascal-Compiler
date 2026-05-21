/*
    SemCheck_Expr_Types_recordaccess.c - Record/class field access semantic checking

    Extracted from SemCheck_Expr_Types.c. Contains semcheck_recordaccess.
*/

#include "SemCheck_Expr_Types_internal.h"
#include "unit_registry.h"
#include "../../ErrVars.h"

static void record_access_clear_payload(struct Expression *expr, int destroy_record_expr)
{
    if (expr == NULL || expr->type != EXPR_RECORD_ACCESS)
        return;
    if (destroy_record_expr && expr->expr_data.record_access_data.record_expr != NULL)
        destroy_expr(expr->expr_data.record_access_data.record_expr);
    expr->expr_data.record_access_data.record_expr = NULL;
    free(expr->expr_data.record_access_data.field_id);
    expr->expr_data.record_access_data.field_id = NULL;
}

int semcheck_recordaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_RECORD_ACCESS);

    semcheck_clear_array_info(expr);

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    const char *field_id = expr->expr_data.record_access_data.field_id;
    if (record_expr == NULL || field_id == NULL)
    {
        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, malformed record field access.\n\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }
    if (record_expr->type == EXPR_VAR_ID)
        assert(record_expr->expr_data.id != NULL);

    if (record_expr->type == EXPR_FUNCTION_CALL)
    {
        int rec_cast_type = UNKNOWN_TYPE;
        int cast_result = semcheck_try_reinterpret_as_typecast(&rec_cast_type, symtab,
            record_expr, max_scope_lev);
        if (cast_result != 0)
        {
            *type_return = UNKNOWN_TYPE;
            return cast_result;
        }
    }

    if (record_expr->type == EXPR_VAR_ID || record_expr->type == EXPR_RECORD_ACCESS)
    {
        char *qualified_id = NULL;
        if (record_expr->type == EXPR_VAR_ID)
        {
            size_t qualified_len = strlen(record_expr->expr_data.id) + 1 + strlen(field_id) + 1;
            qualified_id = (char *)malloc(qualified_len);
            if (qualified_id != NULL)
                snprintf(qualified_id, qualified_len, "%s.%s", record_expr->expr_data.id, field_id);
        }
        else
        {
            qualified_id = build_qualified_identifier_from_expr_local(record_expr);
            if (qualified_id != NULL)
            {
                size_t qualified_len = strlen(qualified_id) + 1 + strlen(field_id) + 1;
                char *combined = (char *)malloc(qualified_len);
                if (combined != NULL)
                {
                    snprintf(combined, qualified_len, "%s.%s", qualified_id, field_id);
                    free(qualified_id);
                    qualified_id = combined;
                }
                else
                {
                    free(qualified_id);
                    qualified_id = NULL;
                }
            }
        }

        if (qualified_id != NULL)
        {
            HashNode_t *type_node = NULL;
            const char *resolved_id = qualified_id;
            if (FindSymbol(&type_node, symtab, qualified_id) == 0 ||
                type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
            {
                const char *base = semcheck_base_type_name(qualified_id);
                if (base != NULL && base != qualified_id &&
                    FindSymbol(&type_node, symtab, base) != 0 &&
                    type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                {
                    resolved_id = base;
                }
            }

            if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
            {
                assert(resolved_id != NULL);
                assert(expr->expr_data.record_access_data.field_id != NULL);
                /* Determine the resolved type tag for the qualified type.
                 * Only set kgpc_type/tag for enum types to enable
                 * TClass.TEnum.Value resolution.  Other type references
                 * should stay UNKNOWN_TYPE to avoid codegen side-effects. */
                int resolved_tag = UNKNOWN_TYPE;
                struct TypeAlias *ta = hashnode_get_type_alias(type_node);
                if (ta != NULL && ta->is_enum)
                    resolved_tag = ENUM_TYPE;

                destroy_expr(record_expr);
                free(expr->expr_data.record_access_data.field_id);
                expr->expr_data.record_access_data.record_expr = NULL;
                expr->expr_data.record_access_data.field_id = NULL;
                expr->type = EXPR_VAR_ID;
                expr->expr_data.id = strdup(resolved_id);
                if (resolved_tag == ENUM_TYPE && type_node->type != NULL)
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, type_node->type);
                if (resolved_tag != UNKNOWN_TYPE)
                    semcheck_expr_set_resolved_type(expr, resolved_tag);
                free(qualified_id);
                *type_return = resolved_tag;
                return 0;
            }
            free(qualified_id);
        }
    }

    /* Scoped enum support for identifiers that resolve as types (e.g., TEndian.Little).
     * If record_expr is a type name, resolve field_id as enum literal.
     * Skip if the identifier is a known variable — e.g., a local variable named
     * "hmodule" should not be confused with the HMODULE type alias.
     */
    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
    {
        /* If there's a variable with this name, don't try scoped enum resolution */
        HashNode_t *var_check = NULL;
        int var_found = FindSymbol(&var_check, symtab, record_expr->expr_data.id);
        if (var_found != 0 && var_check != NULL &&
            (var_check->hash_type == HASHTYPE_VAR ||
             var_check->hash_type == HASHTYPE_ARRAY ||
             var_check->hash_type == HASHTYPE_FUNCTION_RETURN))
        {
            goto skip_scoped_enum_resolution;
        }
        long long enum_value = 0;
        HashNode_t *enum_type_node = semcheck_find_visible_enum_type_candidate_with_literal(
            symtab, record_expr->expr_data.id, field_id, &enum_value);
        if (enum_type_node != NULL)
        {
            record_access_clear_payload(expr, 1);
            expr->type = EXPR_INUM;
            expr->expr_data.i_num = enum_value;
            semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
            if (enum_type_node->type != NULL)
                semcheck_expr_set_resolved_kgpc_type_shared(expr, enum_type_node->type);
            *type_return = ENUM_TYPE;
            return 0;
        }

        HashNode_t *type_node = semcheck_find_preferred_type_node(symtab,
            record_expr->expr_data.id);
        if ((type_node == NULL || type_node->hash_type != HASHTYPE_TYPE) &&
            FindSymbol(&type_node, symtab, record_expr->expr_data.id) != 0 &&
            type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            /* fall back to the raw symbol-table order only if the preferred lookup
             * found nothing; full bootstrap has many visible aliases with the same
             * base name, so raw FindSymbol() can pick the wrong owner. */
        }
        /* If the type is still not found, try looking it up via the enclosing
         * class owner chain (e.g., CheckFlag inside HeapTracer.PrevMgrToFreeBatch
         * should resolve to HeapTracer.CheckFlag). */
        if (type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
        {
            const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
            const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
            if (owner_full == NULL)
                owner_full = semcheck_get_current_method_owner();
            HashNode_t *chain_node = semcheck_find_type_node_in_owner_chain(symtab,
                record_expr->expr_data.id, owner_full, owner_outer);
            if (chain_node != NULL && chain_node->hash_type == HASHTYPE_TYPE)
                type_node = chain_node;
        }
        if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            struct TypeAlias *type_alias = hashnode_get_type_alias(type_node);
            if (type_alias != NULL)
            {
                long long enum_value = 0;
                if ((type_alias->is_enum && type_alias->enum_literals != NULL) ||
                    (type_alias->target_type_id != NULL) ||
                    (type_alias->target_type_ref != NULL && type_alias->target_type_ref->name != NULL))
                {
                    int resolved = 0;
                    /* Use the node's registered id (which may be qualified,
                     * e.g. "HeapTracer.CheckFlag") so that scoped enum
                     * resolution can find the type in the symbol table. */
                    const char *resolve_name = (type_node->id != NULL)
                        ? type_node->id : record_expr->expr_data.id;
                    if (type_alias->is_enum)
                    {
                        resolved = semcheck_resolve_scoped_enum_literal(symtab,
                            resolve_name, field_id, &enum_value);
                    }
                    else if (type_alias->target_type_ref != NULL &&
                             type_alias->target_type_ref->name != NULL)
                    {
                        resolved = semcheck_resolve_scoped_enum_literal_ref(symtab,
                            type_alias->target_type_ref->name, field_id, &enum_value);
                    }
                    else if (type_alias->target_type_id != NULL)
                    {
                        resolved = semcheck_resolve_scoped_enum_literal(symtab,
                            type_alias->target_type_id, field_id, &enum_value);
                    }

                    if (resolved)
                    {
                        record_access_clear_payload(expr, 1);
                        expr->type = EXPR_INUM;
                        expr->expr_data.i_num = enum_value;
                        semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                        if (type_alias->kgpc_type != NULL)
                            semcheck_expr_set_resolved_kgpc_type_shared(expr, type_alias->kgpc_type);
                        *type_return = ENUM_TYPE;
                        return 0;
                    }
                }
            }
            /* Check for nested type access: TMyClass.TMyEnum where TMyEnum is a
             * nested type registered as "TMyClass.TMyEnum" in the symbol table. */
            {
                size_t owner_len = strlen(record_expr->expr_data.id);
                size_t field_len = strlen(field_id);
                char *qualified = (char *)malloc(owner_len + 1 + field_len + 1);
                if (qualified != NULL)
                {
                    snprintf(qualified, owner_len + 1 + field_len + 1, "%s.%s",
                        record_expr->expr_data.id, field_id);
                    HashNode_t *nested_node = NULL;
                    if (FindSymbol(&nested_node, symtab, qualified) != 0 &&
                        nested_node != NULL && nested_node->hash_type == HASHTYPE_TYPE)
                    {
                        struct TypeAlias *nta = hashnode_get_type_alias(nested_node);
                        int nested_tag = UNKNOWN_TYPE;
                        if (nta != NULL && nta->is_enum)
                            nested_tag = ENUM_TYPE;
                        else if (nested_node->type != NULL)
                            nested_tag = semcheck_tag_from_kgpc(nested_node->type);

                        /* Replace record_access with EXPR_VAR_ID for the qualified type */
                        if (record_expr != NULL)
                        {
                            destroy_expr(record_expr);
                            expr->expr_data.record_access_data.record_expr = NULL;
                        }
                        if (expr->expr_data.record_access_data.field_id != NULL)
                        {
                            free(expr->expr_data.record_access_data.field_id);
                            expr->expr_data.record_access_data.field_id = NULL;
                        }
                        expr->type = EXPR_VAR_ID;
                        expr->expr_data.id = qualified;
                        semcheck_expr_set_resolved_type(expr, nested_tag);
                        if (nested_node->type != NULL)
                            semcheck_expr_set_resolved_kgpc_type_shared(expr, nested_node->type);
                        *type_return = nested_tag;
                        return 0;
                    }
                    free(qualified);
                }
            }
        }
        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] enum const expr: T=%s found=%d type_node=%p",
                record_expr->expr_data.id, type_node != NULL, (void *)type_node);
            if (type_node != NULL)
                fprintf(stderr, " id=%s hash_type=%d unit=%d",
                    type_node->id ? type_node->id : "NULL",
                    type_node->hash_type,
                    type_node->source_unit_index);
            fprintf(stderr, "\n");
        }
    }
skip_scoped_enum_resolution:

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
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d: failed to allocate expression for AST transformation in semcheck_recordaccess.\n",
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
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d: failed to duplicate field name in AST transformation.\n",
                        expr->line_num);
                    free(new_record_access);
                    *type_return = UNKNOWN_TYPE;
                    return 1;
                }
                new_record_access->expr_data.record_access_data.field_offset = 0;
                semcheck_expr_set_resolved_type(new_record_access, UNKNOWN_TYPE);
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

    /* Extended NOT restructuring: for chained member access like `not x.a.b`,
     * the parser produces `((NOT x).a).b`. The single-level NOT fix below only
     * handles the immediate case. Walk the nested RECORD_ACCESS chain to find
     * NOT buried deeper and pull it to the top level. */
    {
        struct Expression *walk = record_expr;
        while (walk != NULL && walk->type == EXPR_RECORD_ACCESS)
        {
            struct Expression *inner = walk->expr_data.record_access_data.record_expr;
            if (inner != NULL && inner->type == EXPR_RELOP &&
                inner->expr_data.relop_data.type == NOT &&
                inner->expr_data.relop_data.left != NULL &&
                inner->expr_data.relop_data.right == NULL)
            {
                /* Found NOT buried inside the chain. Remove it from the chain
                 * and wrap the entire current expression in NOT. */
                struct Expression *not_node = inner;
                struct Expression *not_operand = not_node->expr_data.relop_data.left;
                walk->expr_data.record_access_data.record_expr = not_operand;

                /* Reuse the orphaned NOT node to hold the current expr's data */
                *not_node = *expr;

                /* Make current expr a NOT wrapping the chain */
                expr->type = EXPR_RELOP;
                memset(&expr->expr_data, 0, sizeof(expr->expr_data));
                expr->expr_data.relop_data.type = NOT;
                expr->expr_data.relop_data.left = not_node;
                expr->expr_data.relop_data.right = NULL;

                return semcheck_relop(type_return, symtab, expr, max_scope_lev, mutating);
            }
            walk = inner;
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
            semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d: failed to allocate expression for AST transformation in semcheck_recordaccess.\n",
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
            semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d: failed to duplicate field name in AST transformation.\n",
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

    /* FPC Bootstrap Feature: Handle unit-qualified identifiers in runtime expressions.
     * When we see UnitName.ConstName and UnitName is an unresolvable identifier,
     * check if ConstName is a known constant/var in the current scope (since unit
     * exports are merged). If so, transform the expression to just the identifier. */
    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
    {
        char *unit_id = record_expr->expr_data.id;
        HashNode_t *unit_check = NULL;
        HashNode_t *preferred_type = NULL;
        int unit_is_qualifier = semcheck_is_unit_name(unit_id);
        
        /* Check if the "unit name" identifier exists in symbol table */
        int find_result = FindSymbol(&unit_check, symtab, unit_id);
        /* Only look for a preferred type when the identifier is not a known
         * variable — otherwise a local variable like `hmodule` whose name
         * collides with a type (HMODULE) would be misresolved as a scoped
         * enum qualifier instead of a record/class field access. */
        if (!unit_is_qualifier &&
            !(find_result &&
              (unit_check->hash_type == HASHTYPE_VAR ||
               unit_check->hash_type == HASHTYPE_ARRAY ||
               unit_check->hash_type == HASHTYPE_FUNCTION_RETURN)))
            preferred_type = semcheck_find_preferred_type_node(symtab, unit_id);
        if (!unit_is_qualifier && !find_result && unit_registry_contains(unit_id))
        {
            unit_is_qualifier = 1;
            if (kgpc_getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_RECORD_ACCESS] unit-qualifier registry fallback: unit=%s field=%s\n",
                    unit_id != NULL ? unit_id : "(null)",
                    field_id != NULL ? field_id : "(null)");
            }
            if (kgpc_getenv("KGPC_ASSERT_UNIT_QUALIFIER") != NULL)
                assert(!find_result && "unit-qualifier registry fallback requires unresolved name");
        }
        if (unit_is_qualifier)
        {
            if (kgpc_getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_RECORD_ACCESS] unit-qualifier resolve: unit=%s field=%s\n",
                    unit_id != NULL ? unit_id : "(null)",
                    field_id != NULL ? field_id : "(null)");
            }
            /* Unit-qualified access: look up field_id directly in the unit's own table.
             * This ensures e.g. System.MaxInt returns System's MaxInt (32767),
             * not ObjPas's redefined MaxInt (2147483647). */
            HashNode_t *field_node = NULL;
            int unit_idx = unit_registry_add(unit_id);
            if (unit_idx > 0 && unit_idx < SYMTAB_MAX_UNITS &&
                symtab->unit_scopes[unit_idx] != NULL)
            {
                field_node = FindIdentInTable(symtab->unit_scopes[unit_idx]->table, field_id);
            }
            if (field_node == NULL)
            {
                for (ScopeNode *cur_scope = symtab->current_scope;
                     cur_scope != NULL; cur_scope = cur_scope->parent)
                {
                    HashNode_t *candidate = FindIdentInTableForUnit(
                        cur_scope->table, field_id, unit_idx);
                    if (candidate != NULL && candidate->source_unit_index == unit_idx)
                    {
                        field_node = candidate;
                        break;
                    }
                }
            }
            /* No fallback to FindSymbol — if the unit's own table doesn't
             * have the identifier, it's genuinely not exported by that unit.
             * The filtered scope-chain search above only accepts symbols
             * whose source_unit_index matches the explicit unit qualifier. */
            if (field_node != NULL)
            {
                /* Found the field as a direct identifier - transform the expression */
                if (field_node->hash_type == HASHTYPE_CONST)
                {
                    /* Transform to integer literal for constants */
                    record_access_clear_payload(expr, 1);
                    expr->type = EXPR_INUM;
                    expr->expr_data.i_num = field_node->const_int_value;
                    semcheck_expr_set_resolved_type(expr, LONGINT_TYPE);
                    if (field_node->type != NULL)
                    {
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, field_node->type);
                    }
                    *type_return = LONGINT_TYPE;
                    return 0;
                }
                else if (field_node->hash_type == HASHTYPE_VAR || 
                         field_node->hash_type == HASHTYPE_ARRAY)
                {
                    /* Transform to simple variable reference */
                    char *field_copy = strdup(field_id);
                    if (field_copy == NULL)
                    {
                        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d: failed to allocate memory for unit-qualified variable.\n",
                            expr->line_num);
                        *type_return = UNKNOWN_TYPE;
                        return 1;
                    }
                    record_access_clear_payload(expr, 1);
                    expr->type = EXPR_VAR_ID;
                    expr->expr_data.id = field_copy;
                    return semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
                }
                else if (field_node->hash_type == HASHTYPE_TYPE)
                {
                    char *qualified_name = NULL;
                    HashNode_t *qualified_type = NULL;
                    size_t qualified_len = strlen(unit_id) + 1 + strlen(field_id) + 1;
                    qualified_name = (char *)malloc(qualified_len);
                    if (qualified_name != NULL)
                        snprintf(qualified_name, qualified_len, "%s.%s", unit_id, field_id);

                    if (qualified_name != NULL)
                    {
                        if (FindSymbol(&qualified_type, symtab, qualified_name) == 0 ||
                            qualified_type == NULL || qualified_type->hash_type != HASHTYPE_TYPE)
                        {
                            QualifiedIdent *qid = qualified_ident_from_dotted(qualified_name);
                            if (qid != NULL)
                            {
                                qualified_type = semcheck_find_exact_qualified_type_node(symtab, qid);
                                qualified_ident_free(qid);
                            }
                        }
                    }

                    if (qualified_name == NULL)
                    {
                        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d: failed to allocate memory for unit-qualified type.\n",
                            expr->line_num);
                        *type_return = UNKNOWN_TYPE;
                        return 1;
                    }
                    if (qualified_type == NULL)
                    {
                        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, 
                            "Error on line %d: unable to resolve unit-qualified type %s.\n",
                            expr->line_num, qualified_name);
                        free(qualified_name);
                        *type_return = UNKNOWN_TYPE;
                        return 1;
                    }

                    if (record_expr != NULL)
                    {
                        destroy_expr(record_expr);
                        expr->expr_data.record_access_data.record_expr = NULL;
                    }
                    if (expr->expr_data.record_access_data.field_id != NULL)
                    {
                        free(expr->expr_data.record_access_data.field_id);
                        expr->expr_data.record_access_data.field_id = NULL;
                    }
                    expr->type = EXPR_VAR_ID;
                    expr->expr_data.id = qualified_name;
                    semcheck_expr_set_resolved_type(expr,
                        semcheck_tag_from_kgpc(qualified_type->type));
                    if (qualified_type->type != NULL)
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, qualified_type->type);
                    *type_return = semcheck_tag_from_kgpc(qualified_type->type);
                    return 0;
                }
                else if (field_node->hash_type == HASHTYPE_FUNCTION ||
                         field_node->hash_type == HASHTYPE_PROCEDURE)
                {
                    /* Unit.FuncName - transform to simple identifier and let
                     * semcheck_varid handle the conversion to a zero-arg call */
                    char *field_copy = strdup(field_id);
                    if (field_copy == NULL)
                    {
                        *type_return = UNKNOWN_TYPE;
                        return 1;
                    }
                    record_access_clear_payload(expr, 1);
                    expr->type = EXPR_VAR_ID;
                    expr->expr_data.id = field_copy;
                    return semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
                }
                else if (field_node->hash_type == HASHTYPE_FUNCTION_RETURN)
                {
                    /* Unit.QualifiedName may resolve to the function-return helper
                     * symbol first; force a zero-arg function call in expression
                     * context so identifiers like System.GetLoadErrorStr are read
                     * as function values, not record-field-like accesses. */
                    char *field_copy = strdup(field_id);
                    if (field_copy == NULL)
                    {
                        *type_return = UNKNOWN_TYPE;
                        return 1;
                    }
                    record_access_clear_payload(expr, 1);
                    expr->type = EXPR_FUNCTION_CALL;
                    memset(&expr->expr_data.function_call_data, 0,
                        sizeof(expr->expr_data.function_call_data));
                    expr->expr_data.function_call_data.id = field_copy;
                    expr->expr_data.function_call_data.args_expr = NULL;
                    expr->expr_data.function_call_data.mangled_id = NULL;
                    semcheck_reset_function_call_cache(expr);
                    return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                }
            }
        }
        /* Scoped enum support: TEnumType.EnumValue
         * When unit_check is found and it's a type with an enum, look up the field_id
         * as an enum literal and transform to its ordinal constant. */
        else if (preferred_type != NULL && preferred_type->hash_type == HASHTYPE_TYPE)
        {
            /* Check if the type has an enum type alias - look up field_id as enum literal */
            struct TypeAlias *type_alias = hashnode_get_type_alias(preferred_type);
            if (type_alias != NULL && type_alias->is_enum && type_alias->enum_literals != NULL)
            {
                /* Search for field_id in enum_literals */
                int ordinal = 0;
                long long alias_target_value = 0;
                int resolved_via_target = 0;
                ListNode_t *literal_node = type_alias->enum_literals;
                while (literal_node != NULL)
                {
                    if (literal_node->cur != NULL)
                    {
                        char *literal_name = (char *)literal_node->cur;
                        if (strcasecmp(literal_name, field_id) == 0)
                        {
                            /* Found the enum literal - transform to integer constant */
                            record_access_clear_payload(expr, 1);
                            expr->type = EXPR_INUM;
                            expr->expr_data.i_num = ordinal;
                            semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                            if (type_alias->kgpc_type != NULL)
                            {
                                /* Use the shared type setter to properly manage reference counting */
                                semcheck_expr_set_resolved_kgpc_type_shared(expr, type_alias->kgpc_type);
                            }
                            else
                            {
                                semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                            }
                            *type_return = ENUM_TYPE;
                            return 0;
                        }
                    }
                    ++ordinal;
                    literal_node = literal_node->next;
                }
                if (type_alias->target_type_ref != NULL &&
                    type_alias->target_type_ref->name != NULL)
                {
                    resolved_via_target = semcheck_resolve_scoped_enum_literal_ref(symtab,
                        type_alias->target_type_ref->name, field_id, &alias_target_value);
                    HashNode_t *target_type_node = semcheck_find_exact_type_node_for_ref(symtab,
                        type_alias->target_type_ref, type_alias->target_type_id, field_id);
                    struct TypeAlias *target_alias =
                        (target_type_node != NULL) ? hashnode_get_type_alias(target_type_node) : NULL;
                    if (!resolved_via_target && semcheck_type_alias_has_enum_literal(target_alias, field_id))
                    {
                        int ordinal = 0;
                        for (ListNode_t *literal_node = target_alias->enum_literals;
                             literal_node != NULL; literal_node = literal_node->next, ++ordinal)
                        {
                            const char *literal_name = (const char *)literal_node->cur;
                            if (literal_name != NULL &&
                                pascal_identifier_equals(literal_name, field_id))
                            {
                                alias_target_value = ordinal;
                                resolved_via_target = 1;
                                break;
                            }
                        }
                    }
                    char *qualified_target = type_ref_render_source(type_alias->target_type_ref);
                    if (!resolved_via_target && qualified_target != NULL)
                    {
                        resolved_via_target = semcheck_resolve_scoped_enum_literal(symtab,
                            qualified_target, field_id, &alias_target_value);
                        free(qualified_target);
                    }
                    if (!resolved_via_target)
                    {
                        resolved_via_target = semcheck_resolve_scoped_enum_literal_ref(symtab,
                            type_alias->target_type_ref->name, field_id, &alias_target_value);
                    }
                }
                else if (type_alias->target_type_id != NULL)
                {
                    resolved_via_target = semcheck_resolve_scoped_enum_literal(symtab,
                        type_alias->target_type_id, field_id, &alias_target_value);
                }
                if (resolved_via_target)
                {
                    record_access_clear_payload(expr, 1);
                    expr->type = EXPR_INUM;
                    expr->expr_data.i_num = alias_target_value;
                    semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                    if (type_alias->kgpc_type != NULL)
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, type_alias->kgpc_type);
                    *type_return = ENUM_TYPE;
                    return 0;
                }
            }

            /* If this type is an alias to another type, try resolving scoped enum literal
             * against the alias target (e.g., TEndian = ObjPas.TEndian).
             */
            if (type_alias != NULL)
            {
                long long enum_value = 0;
                int resolved = 0;
                if (type_alias->target_type_ref != NULL &&
                    type_alias->target_type_ref->name != NULL)
                {
                    resolved = semcheck_resolve_scoped_enum_literal_ref(symtab,
                        type_alias->target_type_ref->name, field_id, &enum_value);
                    HashNode_t *target_type_node = semcheck_find_exact_type_node_for_ref(symtab,
                        type_alias->target_type_ref, type_alias->target_type_id, field_id);
                    struct TypeAlias *target_alias =
                        (target_type_node != NULL) ? hashnode_get_type_alias(target_type_node) : NULL;
                    if (!resolved && semcheck_type_alias_has_enum_literal(target_alias, field_id))
                    {
                        int ordinal = 0;
                        for (ListNode_t *literal_node = target_alias->enum_literals;
                             literal_node != NULL; literal_node = literal_node->next, ++ordinal)
                        {
                            const char *literal_name = (const char *)literal_node->cur;
                            if (literal_name != NULL &&
                                pascal_identifier_equals(literal_name, field_id))
                            {
                                enum_value = ordinal;
                                resolved = 1;
                                break;
                            }
                        }
                    }
                    char *qualified_target = type_ref_render_source(type_alias->target_type_ref);
                    if (!resolved && qualified_target != NULL)
                    {
                        resolved = semcheck_resolve_scoped_enum_literal(symtab,
                            qualified_target, field_id, &enum_value);
                        free(qualified_target);
                    }
                    if (!resolved)
                    {
                        resolved = semcheck_resolve_scoped_enum_literal_ref(symtab,
                            type_alias->target_type_ref->name, field_id, &enum_value);
                    }
                }
                else if (type_alias->target_type_id != NULL)
                {
                    resolved = semcheck_resolve_scoped_enum_literal(symtab,
                        type_alias->target_type_id, field_id, &enum_value);
                }
                if (resolved)
                {
                    record_access_clear_payload(expr, 1);
                    expr->type = EXPR_INUM;
                    expr->expr_data.i_num = enum_value;
                    semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                    if (type_alias->kgpc_type != NULL)
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, type_alias->kgpc_type);
                    *type_return = ENUM_TYPE;
                    return 0;
                }

                HashNode_t *literal_node = NULL;
                if (FindSymbol(&literal_node, symtab, field_id) != 0 &&
                    literal_node != NULL &&
                    (literal_node->hash_type == HASHTYPE_CONST ||
                     literal_node->is_constant ||
                     literal_node->is_typed_const))
                {
                    record_access_clear_payload(expr, 1);
                    expr->type = EXPR_INUM;
                    expr->expr_data.i_num = literal_node->const_int_value;
                    semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                    if (type_alias->kgpc_type != NULL)
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, type_alias->kgpc_type);
                    *type_return = ENUM_TYPE;
                    return 0;
                }
            }

            if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] enum alias fallback failed for %s\n", unit_id);
            }
        }
    }
    /* Enum member access in const expressions: resolve TEnum.Value even if TEnum
     * isn't found as a value in the current scope. */
    if (!mutating && record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
    {
        long long enum_value = 0;
        if (semcheck_resolve_scoped_enum_literal(symtab, record_expr->expr_data.id,
                field_id, &enum_value))
        {
            record_access_clear_payload(expr, 1);
            expr->type = EXPR_INUM;
            expr->expr_data.i_num = enum_value;
            semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
            *type_return = ENUM_TYPE;
            return 0;
        }
    }

    /* Unit-qualified identifier resolution: UnitName.Identifier
     * When the base expression is a unit name, resolve field_id directly
     * from the symbol table and transform this expression accordingly. */
    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL &&
        semcheck_is_unit_name(record_expr->expr_data.id) &&
        !semcheck_has_value_ident(symtab, record_expr->expr_data.id))
    {
        const char *trace_nonlocal = kgpc_getenv("KGPC_TRACE_NONLOCAL");
        int trace_unit_case = (trace_nonlocal != NULL &&
            record_expr->expr_data.id != NULL &&
            (strcmp(trace_nonlocal, "1") == 0 ||
             pascal_identifier_equals(record_expr->expr_data.id, trace_nonlocal)));
        if (trace_unit_case)
        {
            fprintf(stderr,
                "[KGPC_TRACE_NONLOCAL] semcheck unit-branch unit=%s field=%s line=%d\n",
                record_expr->expr_data.id,
                field_id != NULL ? field_id : "<null>",
                expr->line_num);
        }
        if (kgpc_getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_RECORD_ACCESS] unit-name branch: id=%s field=%s\n",
                record_expr->expr_data.id,
                field_id != NULL ? field_id : "(null)");
        }
        HashNode_t *unit_sym = NULL;
        int unit_idx = unit_registry_add(record_expr->expr_data.id);
        if (unit_idx > 0 && unit_idx < SYMTAB_MAX_UNITS &&
            symtab->unit_scopes[unit_idx] != NULL)
        {
            unit_sym = FindIdentInTable(symtab->unit_scopes[unit_idx]->table, field_id);
        }
        if (trace_unit_case)
        {
            fprintf(stderr,
                "[KGPC_TRACE_NONLOCAL] semcheck unit-branch lookup unit=%s idx=%d hit=%s hash=%d\n",
                record_expr->expr_data.id,
                unit_idx,
                unit_sym != NULL && unit_sym->id != NULL ? unit_sym->id : "<null>",
                unit_sym != NULL ? unit_sym->hash_type : -1);
        }
        if (unit_sym == NULL &&
            FindSymbol(&unit_sym, symtab, field_id) != 0 && unit_sym != NULL)
        {
            if (trace_unit_case)
            {
                fprintf(stderr,
                    "[KGPC_TRACE_NONLOCAL] semcheck unit-branch fallback hit=%s hash=%d\n",
                    unit_sym->id != NULL ? unit_sym->id : "<null>",
                    unit_sym->hash_type);
            }
            /* Transform EXPR_RECORD_ACCESS into EXPR_VAR_ID for the resolved identifier */
            char *saved_field_id = strdup(field_id);
            destroy_expr(record_expr);
            free(expr->expr_data.record_access_data.field_id);
            expr->expr_data.record_access_data.record_expr = NULL;
            expr->expr_data.record_access_data.field_id = NULL;
            expr->type = EXPR_VAR_ID;
            expr->expr_data.id = saved_field_id;
            if (expr->expr_data.id == NULL)
            {
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            KgpcType *resolved_kgpc = NULL;
            int result = semcheck_expr_with_type(&resolved_kgpc, symtab, expr, max_scope_lev, mutating);
            *type_return = semcheck_tag_from_kgpc(resolved_kgpc);
            return result;
        }
        if (kgpc_getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_RECORD_ACCESS] unit-name no symbol: id=%s field=%s\n",
                record_expr->expr_data.id,
                field_id != NULL ? field_id : "(null)");
        }

        /* Fallback for qualified zero-arg function access like
         * UnitName.FuncName used without parentheses in expression context. */
        if (field_id != NULL)
        {
            char *field_copy = strdup(field_id);
            if (field_copy == NULL)
            {
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            destroy_expr(record_expr);
            free(expr->expr_data.record_access_data.field_id);
            expr->expr_data.record_access_data.record_expr = NULL;
            expr->expr_data.record_access_data.field_id = NULL;
            expr->type = EXPR_FUNCTION_CALL;
            memset(&expr->expr_data.function_call_data, 0,
                sizeof(expr->expr_data.function_call_data));
            expr->expr_data.function_call_data.id = field_copy;
            expr->expr_data.function_call_data.args_expr = NULL;
            expr->expr_data.function_call_data.mangled_id = NULL;
            semcheck_reset_function_call_cache(expr);
            return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
        }
    }

    int error_count = 0;
    int record_type = UNKNOWN_TYPE;
    KgpcType *record_kgpc_type = NULL;
    /* Evaluate the record/object expression as a read: in `obj.field := val`,
     * `obj` is read (to obtain the container), only `field` is mutated.
     * Using NO_MUTATE avoids false "property is read-only" errors when an
     * indexed property returns a class reference whose member is assigned. */
    error_count += semcheck_expr_with_type(&record_kgpc_type, symtab, record_expr, max_scope_lev, NO_MUTATE);
    record_type = semcheck_tag_from_kgpc(record_kgpc_type);
    if (kgpc_getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
    {
        const char *type_str = NULL;
        if (record_kgpc_type != NULL)
            type_str = kgpc_type_to_string(record_kgpc_type);
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_ACCESS] record_expr_type=%d record_type=%d kgpc=%s\n",
            record_expr->type,
            record_type,
            type_str != NULL ? type_str : "<null>");
    }

    /* When the record expression resolved to a function/procedure type (e.g. a
     * function pointer field like `GetFPCHeapStatus: function: TFPCHeapStatus`),
     * and we are trying to access a field on it (e.g. `.CurrHeapUsed`), treat
     * this as an implicit call of the parameterless function pointer and use
     * the return type for the field access.  This mirrors FPC semantics where
     * `rec.FuncPtrField.ResultField` implicitly invokes the function pointer. */
    if (record_type == PROCEDURE && record_kgpc_type != NULL &&
        kgpc_type_is_procedure(record_kgpc_type))
    {
        KgpcType *ret_type = kgpc_type_get_return_type(record_kgpc_type);
        if (ret_type == NULL && record_kgpc_type->info.proc_info.return_type_id != NULL)
        {
            /* The return_type was not materialized during AST construction
             * (convert_field_decl passes NULL symtab).  Resolve it now and
             * store it back so every subsequent use sees the resolved type. */
            HashNode_t *ret_node = NULL;
            if (FindSymbol(&ret_node, symtab, record_kgpc_type->info.proc_info.return_type_id) != 0 &&
                ret_node != NULL && ret_node->type != NULL)
            {
                ret_type = ret_node->type;
                kgpc_type_retain(ret_type);
                record_kgpc_type->info.proc_info.return_type = ret_type;
            }
        }
        if (ret_type != NULL)
        {
            int ret_tag = semcheck_tag_from_kgpc(ret_type);
            if (ret_tag == RECORD_TYPE)
            {
                if (record_kgpc_type != NULL)
                    kgpc_type_release(record_kgpc_type);
                record_kgpc_type = ret_type;
                kgpc_type_retain(record_kgpc_type);
                record_type = RECORD_TYPE;
            }
        }
    }

    if (record_expr->type == EXPR_RECORD_ACCESS)
    {
        struct Expression *inner_rec = record_expr->expr_data.record_access_data.record_expr;
        const char *inner_field = record_expr->expr_data.record_access_data.field_id;
        if (inner_rec != NULL && inner_rec->type == EXPR_VAR_ID &&
            inner_rec->expr_data.id != NULL &&
            pascal_identifier_equals(inner_rec->expr_data.id, "Self") &&
            inner_field != NULL &&
            semcheck_has_value_ident(symtab, inner_field))
        {
            struct Expression *value_expr = mk_varid(expr->line_num, strdup(inner_field));
            if (value_expr != NULL)
            {
                destroy_expr(record_expr);
                expr->expr_data.record_access_data.record_expr = value_expr;
                record_expr = value_expr;
                /* record_kgpc_type was a borrowed ref owned by the old record_expr
                 * which was already freed by destroy_expr above — just clear it. */
                record_kgpc_type = NULL;
                error_count += semcheck_expr_with_type(&record_kgpc_type, symtab,
                    record_expr, max_scope_lev, NO_MUTATE);
                record_type = semcheck_tag_from_kgpc(record_kgpc_type);
            }
        }
    }

    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
    {
        if (semcheck_has_value_ident(symtab, record_expr->expr_data.id))
        {
            /* Value identifiers shadow implicit Self field access. */
            goto SKIP_SELF_FIELD_REWRITE;
        }
        HashNode_t *self_node = NULL;
        if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
        {
            struct RecordType *self_record = get_record_type_from_node(self_node);
            if (self_record != NULL)
            {
                struct RecordType *expr_record = NULL;
                if (record_kgpc_type != NULL && kgpc_type_is_record(record_kgpc_type))
                    expr_record = kgpc_type_get_record(record_kgpc_type);

                if (expr_record == self_record)
                {
                    struct RecordField *self_field = NULL;
                    long long field_offset = 0;
                    if (resolve_record_field(symtab, self_record, record_expr->expr_data.id,
                            &self_field, &field_offset, expr->line_num, 1) == 0 &&
                        self_field != NULL)
                    {
                        struct Expression *self_expr = mk_varid(expr->line_num, strdup("Self"));
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
                                    self_expr->resolved_kgpc_type = self_record_type;
                            }

                            char *saved_id = record_expr->expr_data.id;
                            record_expr->expr_data.id = NULL;
                            record_expr->type = EXPR_RECORD_ACCESS;
                            memset(&record_expr->expr_data.record_access_data, 0,
                                sizeof(expr->expr_data.record_access_data));
                            record_expr->expr_data.record_access_data.record_expr = self_expr;
                            record_expr->expr_data.record_access_data.field_id = saved_id;
                            record_expr->expr_data.record_access_data.field_offset = field_offset;

                            /* record_kgpc_type is borrowed — just clear it */
                            record_kgpc_type = NULL;
                            error_count += semcheck_expr_with_type(&record_kgpc_type, symtab,
                                record_expr, max_scope_lev, NO_MUTATE);
                            record_type = semcheck_tag_from_kgpc(record_kgpc_type);
                        }
                    }
                }
            }
        }
    }
SKIP_SELF_FIELD_REWRITE:

    if (record_type == ENUM_TYPE)
    {
        const char *expr_type_name = get_expr_type_name(record_expr, symtab);
        const char *enum_type_name = expr_type_name;
        long long enum_value = 0;
        if (enum_type_name != NULL &&
            semcheck_resolve_scoped_enum_literal(symtab, enum_type_name, field_id, &enum_value))
        {
            /* Switching expr from EXPR_RECORD_ACCESS to EXPR_INUM overwrites the
             * union, so the record_expr subtree and field_id strdup held in the
             * record_access_data slot must be released first; otherwise the
             * inner Expression and its strdup'd id both leak. record_kgpc_type
             * is a borrowed reference owned by record_expr's resolved_kgpc_type,
             * so retain it first to keep it alive across the destroy. See the
             * ownership rule in docs/AST_OWNERSHIP_RULE.md. */
            if (record_kgpc_type != NULL)
                kgpc_type_retain(record_kgpc_type);
            record_access_clear_payload(expr, 1);
            expr->type = EXPR_INUM;
            expr->expr_data.i_num = enum_value;
            semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
            if (record_kgpc_type != NULL)
            {
                semcheck_expr_set_resolved_kgpc_type_shared(expr, record_kgpc_type);
                kgpc_type_release(record_kgpc_type);
            }
            *type_return = ENUM_TYPE;
            return error_count;
        }
        if (record_kgpc_type != NULL && record_kgpc_type->type_alias != NULL &&
            record_kgpc_type->type_alias->target_type_id != NULL)
        {
            enum_type_name = record_kgpc_type->type_alias->target_type_id;
            if (semcheck_resolve_scoped_enum_literal(symtab, enum_type_name, field_id, &enum_value))
            {
                /* Same union-overwrite pattern as above: reclaim the
                 * record_access payload before switching to EXPR_INUM, and
                 * keep record_kgpc_type alive across the destroy. */
                kgpc_type_retain(record_kgpc_type);
                record_access_clear_payload(expr, 1);
                expr->type = EXPR_INUM;
                expr->expr_data.i_num = enum_value;
                semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                semcheck_expr_set_resolved_kgpc_type_shared(expr, record_kgpc_type);
                kgpc_type_release(record_kgpc_type);
                *type_return = ENUM_TYPE;
                return error_count;
            }
        }
    }

    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_recordaccess: field_id=%s, record_type=%d\n",
            field_id, record_type);
    }


    struct RecordType *record_info = NULL;
    if (record_type == RECORD_TYPE)
    {
        if (record_info == NULL && record_kgpc_type != NULL &&
            kgpc_type_is_record(record_kgpc_type))
        {
            record_info = kgpc_type_get_record(record_kgpc_type);
        }
        if (kgpc_getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_RECORD_ACCESS] record_info=%p from_kgpc=%d\n",
                (void *)record_info,
                record_kgpc_type != NULL && kgpc_type_is_record(record_kgpc_type));
        }
        if (record_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_record(record_expr->resolved_kgpc_type)) {
            record_info = kgpc_type_get_record(record_expr->resolved_kgpc_type);
        }
        if (record_info == NULL && record_kgpc_type != NULL &&
            record_kgpc_type->type_alias != NULL)
        {
            const char *alias_target = record_kgpc_type->type_alias->target_type_id;
            const char *alias_name = record_kgpc_type->type_alias->alias_name;
            if (alias_target != NULL)
                record_info = semcheck_lookup_record_type(symtab, alias_target);
            if (record_info == NULL && alias_name != NULL)
                record_info = semcheck_lookup_record_type(symtab, alias_name);
        }
        if (record_info == NULL)
        {
            const char *expr_type_name = get_expr_type_name(record_expr, symtab);
            if (expr_type_name != NULL)
                record_info = semcheck_lookup_record_type(symtab, expr_type_name);
        }
        if (record_info == NULL && record_expr != NULL &&
            record_expr->type == EXPR_ARRAY_ACCESS)
        {
            if (record_expr->array_element_record_type != NULL)
                record_info = record_expr->array_element_record_type;
            if (record_info == NULL && record_expr->array_element_type_id != NULL)
                record_info = semcheck_lookup_record_type(symtab, record_expr->array_element_type_id);

            struct Expression *base_array_expr = record_expr->expr_data.array_access_data.array_expr;
            if (record_info == NULL && base_array_expr != NULL)
            {
                if (base_array_expr->array_element_record_type != NULL)
                    record_info = base_array_expr->array_element_record_type;
                if (record_info == NULL && base_array_expr->array_element_type_id != NULL)
                    record_info = semcheck_lookup_record_type(symtab, base_array_expr->array_element_type_id);
                if (record_info == NULL && base_array_expr->resolved_kgpc_type != NULL &&
                    kgpc_type_is_array(base_array_expr->resolved_kgpc_type))
                {
                    KgpcType *elem_type = kgpc_type_get_array_element_type(base_array_expr->resolved_kgpc_type);
                    if (elem_type != NULL && kgpc_type_is_record(elem_type))
                        record_info = kgpc_type_get_record(elem_type);
                }
            }
        }
        if (record_info == NULL && record_expr->type == EXPR_TYPECAST)
        {
            const char *target_id = record_expr->expr_data.typecast_data.target_type_id;
            if (target_id != NULL)
            {
                HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type(symtab, target_id);
                if (type_node == NULL)
                    FindSymbol(&type_node, symtab, target_id);
                if (type_node != NULL)
                {
                    record_info = get_record_type_from_node(type_node);
                    if (record_info == NULL && type_node->type != NULL &&
                        kgpc_type_is_record(type_node->type))
                    {
                        record_info = kgpc_type_get_record(type_node->type);
                    }
                    if (record_expr->resolved_kgpc_type == NULL && type_node->type != NULL)
                    {
                        kgpc_type_retain(type_node->type);
                        record_expr->resolved_kgpc_type = type_node->type;
                    }
                }
            }
        }
    }
    else if (record_type == POINTER_TYPE)
    {
        if (record_info == NULL && record_expr->type == EXPR_POINTER_DEREF)
        {
            struct Expression *ptr_expr = record_expr->expr_data.pointer_deref_data.pointer_expr;
            const char *subtype_id = NULL;
            const TypeRef *subtype_ref = NULL;
            if (ptr_expr != NULL)
            {
                subtype_id = ptr_expr->pointer_subtype_id;
                subtype_ref = ptr_expr->pointer_subtype_ref;
                if (subtype_id == NULL && ptr_expr->type == EXPR_TYPECAST)
                {
                    const char *target_id = ptr_expr->expr_data.typecast_data.target_type_id;
                    const TypeRef *target_ref = ptr_expr->expr_data.typecast_data.target_type_ref;
                    HashNode_t *target_node = semcheck_find_preferred_type_node_with_ref(symtab,
                        target_ref, target_id);
                    if (target_node == NULL && target_id != NULL)
                    {
                        const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
                        const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
                        if (owner_full == NULL)
                            owner_full = semcheck_get_current_method_owner();
                        target_node = semcheck_find_type_node_in_owner_chain(symtab, target_id,
                            owner_full, owner_outer);
                    }
                    if (target_node != NULL)
                    {
                        struct TypeAlias *alias = get_type_alias_from_node(target_node);
                        if (alias != NULL)
                        {
                            if (alias->pointer_type_id != NULL)
                                subtype_id = alias->pointer_type_id;
                            if (alias->pointer_type_ref != NULL)
                                subtype_ref = alias->pointer_type_ref;
                        }
                    }
                }
            }
            if (subtype_id != NULL)
            {
                HashNode_t *target_node = semcheck_find_preferred_type_node_with_ref(symtab,
                    subtype_ref, subtype_id);
                if (target_node == NULL)
                {
                    const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
                    const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
                    if (owner_full == NULL)
                        owner_full = semcheck_get_current_method_owner();
                    target_node = semcheck_find_type_node_in_owner_chain(symtab, subtype_id,
                        owner_full, owner_outer);
                }
                if (target_node != NULL)
                {
                    record_info = get_record_type_from_node(target_node);
                }
                if (record_info == NULL)
                    record_info = semcheck_lookup_record_type(symtab, subtype_id);
                if (record_info != NULL)
                    record_type = RECORD_TYPE;
            }
        }

        /* Try resolved KgpcType pointer target */
        if (record_info == NULL && record_expr->resolved_kgpc_type != NULL &&
            record_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER) {
            KgpcType *pointee = kgpc_type_resolve_pointer_pointee(
                record_expr->resolved_kgpc_type, symtab);
            if (pointee != NULL && kgpc_type_is_record(pointee)) {
                record_info = kgpc_type_get_record(pointee);
            }
        }
        if (record_info == NULL && record_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *target_node =
                semcheck_find_type_node_with_kgpc_type(symtab, record_expr->pointer_subtype_id);
            if (target_node == NULL)
                FindSymbol(&target_node, symtab, record_expr->pointer_subtype_id);
            if (target_node != NULL)
                record_info = get_record_type_from_node(target_node);
        }
        if (record_info == NULL)
        {
            /* Check for type helpers on Pointer type before giving up.
             * Type helpers can be defined for Pointer, PChar, etc.
             * Skip when record_type is UNKNOWN_TYPE — the helper lookup
             * would incorrectly match an unrelated helper (e.g. TLongIntHelper)
             * and produce cascading "field not found" errors. */
            if (record_type != UNKNOWN_TYPE)
            {
                const char *expr_type_name = get_expr_type_name(record_expr, symtab);
                struct RecordType *helper_record = semcheck_lookup_type_helper_for_member(symtab,
                    record_type, expr_type_name, field_id);
                if (helper_record != NULL)
                {
                    record_type = RECORD_TYPE;
                    record_info = helper_record;
                }
            }
            if (record_info == NULL)
            {
                semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, pointer does not reference a record type.\n\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return error_count + 1;
            }
        }
    }
    else
    {
        /* When the record_expr is a pointer dereference that resolved to a
         * non-record type (e.g. LongInt instead of the actual record), try
         * to recover the record info from the pointer's subtype before
         * falling through to type helper lookups. */
        if (record_expr->type == EXPR_POINTER_DEREF)
        {
            struct Expression *ptr_expr = record_expr->expr_data.pointer_deref_data.pointer_expr;
            const char *subtype_id = ptr_expr ? ptr_expr->pointer_subtype_id : NULL;
            if (subtype_id != NULL)
            {
                struct RecordType *ptr_rec = semcheck_lookup_record_type(symtab, subtype_id);
                if (ptr_rec != NULL)
                {
                    record_type = RECORD_TYPE;
                    record_info = ptr_rec;
                }
            }
            if (record_info == NULL && ptr_expr != NULL &&
                ptr_expr->resolved_kgpc_type != NULL &&
                kgpc_type_is_pointer(ptr_expr->resolved_kgpc_type))
            {
                KgpcType *pointee = ptr_expr->resolved_kgpc_type->info.points_to;
                if (pointee != NULL && kgpc_type_is_record(pointee))
                {
                    record_type = RECORD_TYPE;
                    record_info = kgpc_type_get_record(pointee);
                }
            }
            /* Additional recovery: the dereference expression itself may carry
             * pointer_subtype_id propagated from Fix 1 in
             * semcheck_expr_type_pointer_deref, or via the TypeRef chain. */
            if (record_info == NULL && record_expr->pointer_subtype_id != NULL)
            {
                struct RecordType *ptr_rec = semcheck_lookup_record_type(symtab,
                    record_expr->pointer_subtype_id);
                if (ptr_rec != NULL)
                {
                    record_type = RECORD_TYPE;
                    record_info = ptr_rec;
                }
            }
            /* Try the dereference expression's resolved_kgpc_type: if it is a
             * record type, we can use it directly. */
            if (record_info == NULL && record_expr->resolved_kgpc_type != NULL &&
                kgpc_type_is_record(record_expr->resolved_kgpc_type))
            {
                record_info = kgpc_type_get_record(record_expr->resolved_kgpc_type);
                if (record_info != NULL)
                    record_type = RECORD_TYPE;
            }
        }
        if (record_type == RECORD_TYPE && record_info != NULL)
        {
            /* Successfully recovered record type from pointer dereference */
        }
        else if (record_expr->type == EXPR_POINTER_DEREF)
        {
            /* Issue #482: A pointer dereference that resolved to a non-record
             * primitive type (e.g. LONGINT_TYPE from the LONGINT fallback)
             * must NOT fall through to type helper lookup.  Type helpers are
             * for actual primitive values, not for misresolved pointer targets.
             * Report a specific error instead of silently matching a helper. */
            semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index,
                "Error on line %d, pointer dereference does not reference a record type "
                "(cannot access field '%s').\n\n",
                expr->line_num, field_id != NULL ? field_id : "?");
            *type_return = UNKNOWN_TYPE;
            return error_count + 1;
        }
        else
        {
        const char *expr_type_name = get_expr_type_name(record_expr, symtab);
        /* When the record_expr is literally "Self" inside a type helper
         * method body, use the current method's owning helper type rather
         * than the most-recently-registered helper for this base type.
         * This matters when multiple type helpers exist for the same base
         * type (e.g. TWideStringHelper from SysUtils and a user-defined
         * TWideHelper): Self.Length inside TWideStringHelper must resolve
         * to TWideStringHelper's Length property, not TWideHelper's. */
        struct RecordType *helper_record = NULL;
        int is_self_expr = (record_expr->type == EXPR_VAR_ID &&
            record_expr->expr_data.id != NULL &&
            pascal_identifier_equals(record_expr->expr_data.id, "Self"));
        if (is_self_expr)
        {
            const char *current_owner = semcheck_get_current_method_owner();
            if (current_owner != NULL)
            {
                struct RecordType *owner_rec = semcheck_lookup_record_type(symtab, current_owner);
                if (owner_rec != NULL && owner_rec->is_type_helper)
                    helper_record = owner_rec;
            }
        }
        if (helper_record == NULL && record_info != NULL)
            helper_record = semcheck_lookup_type_helper_for_record_member(symtab,
                record_info, field_id);
        if (helper_record == NULL && record_type != UNKNOWN_TYPE)
            helper_record = semcheck_lookup_type_helper_for_member(symtab,
                record_type, expr_type_name, field_id);
        if (helper_record == NULL && is_real_family_type(record_type))
        {
            const char *helper_base = expr_type_name;
            if (helper_base != NULL)
            {
                char helper_name[256];
                snprintf(helper_name, sizeof(helper_name), "T%sHelper", helper_base);
                helper_record = semcheck_lookup_record_type(symtab, helper_name);
            }
        }
        if (helper_record != NULL)
        {
            record_type = RECORD_TYPE;
            record_info = helper_record;
        }
        else if (is_real_family_type(record_type) && field_id != NULL &&
                 pascal_identifier_equals(field_id, "IsNan"))
        {
            /* FPC allows Float.IsNan via type helpers. If helpers weren't registered,
             * fall back to a simple NaN check (x <> x). */
            struct Expression *left_expr = record_expr;
            struct Expression *right_expr = clone_expression(record_expr);
            if (right_expr == NULL)
            {
                semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, failed to allocate IsNan expression.\n\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return error_count + 1;
            }
            if (expr->expr_data.record_access_data.field_id != NULL)
            {
                free(expr->expr_data.record_access_data.field_id);
                expr->expr_data.record_access_data.field_id = NULL;
            }

            expr->type = EXPR_RELOP;
            memset(&expr->expr_data.relop_data, 0, sizeof(expr->expr_data.relop_data));
            expr->expr_data.relop_data.type = NE;
            expr->expr_data.relop_data.left = left_expr;
            expr->expr_data.relop_data.right = right_expr;

            return semcheck_expr_legacy_tag(type_return, symtab, expr, max_scope_lev, mutating);
        }
        else if (record_type == ENUM_TYPE && field_id != NULL)
        {
            /* Scoped enum through nested type: TClass.TEnum.Value
             * The inner access resolved to an enum type; look up field_id as a literal. */
            const char *enum_type_name = expr_type_name;
            struct TypeAlias *resolved_alias =
                (record_expr->resolved_kgpc_type != NULL)
                    ? record_expr->resolved_kgpc_type->type_alias : NULL;
            if (enum_type_name == NULL && resolved_alias != NULL)
            {
                enum_type_name = resolved_alias->alias_name;
                if (enum_type_name == NULL)
                    enum_type_name = resolved_alias->target_type_id;
            }
            long long enum_value = 0;
            int resolved = 0;
            if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
            {
                HashNode_t *enum_type_node = semcheck_find_visible_enum_type_candidate_with_literal(
                    symtab, record_expr->expr_data.id, field_id, &enum_value);
                if (enum_type_node != NULL)
                {
                    resolved = 1;
                    if (record_expr->resolved_kgpc_type == NULL && enum_type_node->type != NULL)
                        semcheck_expr_set_resolved_kgpc_type_shared(record_expr,
                            enum_type_node->type);
                }
            }
            if (resolved_alias != NULL)
            {
                resolved = resolved || semcheck_try_resolve_enum_literal_from_type_alias(
                    symtab, resolved_alias, field_id, &enum_value);
            }
            if (enum_type_name != NULL)
                resolved = resolved || semcheck_resolve_scoped_enum_literal(symtab,
                    enum_type_name, field_id, &enum_value);
            if (!resolved)
            {
                /* Try looking up the field_id directly as a global enum constant */
                HashNode_t *enum_node = NULL;
                if (FindSymbol(&enum_node, symtab, field_id) != 0 && enum_node != NULL &&
                    enum_node->hash_type == HASHTYPE_CONST)
                {
                    resolved = 1;
                    if (enum_node->type != NULL && enum_node->type->kind == TYPE_KIND_PRIMITIVE)
                        enum_value = enum_node->const_int_value;
                    else
                        enum_value = enum_node->const_int_value;
                }
            }
            if (resolved)
            {
                expr->type = EXPR_INUM;
                expr->expr_data.i_num = enum_value;
                semcheck_expr_set_resolved_type(expr, ENUM_TYPE);
                if (record_expr->resolved_kgpc_type != NULL)
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, record_expr->resolved_kgpc_type);
                *type_return = ENUM_TYPE;
                return 0;
            }
            semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, field access requires a record value.\n\n", expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return error_count + 1;
        }
        else
        {
            if (record_expr->type == EXPR_VAR_ID &&
                record_expr->expr_data.id != NULL)
            {
                HashNode_t *type_node =
                    semcheck_find_preferred_type_node(symtab, record_expr->expr_data.id);
                if (type_node == NULL)
                {
                    const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
                    const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
                    if (owner_full == NULL)
                        owner_full = semcheck_get_current_method_owner();
                    type_node = semcheck_find_type_node_in_owner_chain(symtab,
                        record_expr->expr_data.id, owner_full, owner_outer);
                }
                if ((type_node == NULL || type_node->hash_type != HASHTYPE_TYPE))
                {
                    HashNode_t *fallback_node = NULL;
                    if (FindSymbol(&fallback_node, symtab, record_expr->expr_data.id) != 0 &&
                        fallback_node != NULL && fallback_node->hash_type == HASHTYPE_TYPE)
                        type_node = fallback_node;
                }
                if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                {
                    struct RecordType *type_record = get_record_type_from_node(type_node);
                    if (type_record != NULL)
                    {
                        record_type = RECORD_TYPE;
                        record_info = type_record;
                        if (record_kgpc_type == NULL || !kgpc_type_is_record(record_kgpc_type))
                        {
                            if (record_kgpc_type != NULL)
                                kgpc_type_release(record_kgpc_type);
                            record_kgpc_type = create_record_type(type_record);
                        }
                    }
                }
            }
            if (record_info != NULL)
            {
                /* Type-qualified record member access (e.g. SizeOf(TRec.Field)). */
            }
            else
            {
            if (kgpc_getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL)
            {
                const char *rec_id = NULL;
                if (record_expr != NULL && record_expr->type == EXPR_VAR_ID)
                    rec_id = record_expr->expr_data.id;
                fprintf(stderr,
                    "[KGPC_DEBUG_RECORD_ACCESS] not-record: line=%d expr=%p record_expr=%p type=%d rec_id=%s record_type=%d field=%s\n",
                    expr->line_num,
                    (void *)expr,
                    (void *)record_expr,
                    record_expr != NULL ? record_expr->type : -1,
                    rec_id != NULL ? rec_id : "(null)",
                    record_type,
                    field_id != NULL ? field_id : "(null)");
            }
            if (record_type == UNKNOWN_TYPE)
            {
                *type_return = UNKNOWN_TYPE;
                return error_count;
            }
            semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, field access requires a record value.\n\n", expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return error_count + 1;
            }
        }
        } /* close else from pointer deref recovery */
    }


    if (record_info == NULL)
    {
        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, unable to resolve record type for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
    {
        if (semcheck_has_value_ident(symtab, record_expr->expr_data.id))
        {
            /* Local/parameter identifiers must shadow implicit Self field access. */
            goto SKIP_SELF_REWRITE;
        }
        HashNode_t *self_node = NULL;
        if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
        {
            struct RecordType *self_record = get_record_type_from_node(self_node);
            if (self_record != NULL)
            {
                struct RecordType *expr_record = NULL;
                if (record_kgpc_type != NULL && kgpc_type_is_record(record_kgpc_type))
                    expr_record = kgpc_type_get_record(record_kgpc_type);

                int self_match = 0;
                if (expr_record == NULL)
                    self_match = 1;
                else if (expr_record->type_id != NULL && self_record->type_id != NULL &&
                         pascal_identifier_equals(expr_record->type_id, self_record->type_id))
                    self_match = 1;

                if (!self_match)
                    goto SKIP_SELF_REWRITE;

                struct RecordField *self_field = NULL;
                long long field_offset = 0;
                if (resolve_record_field(symtab, self_record, record_expr->expr_data.id,
                        &self_field, &field_offset, expr->line_num, 1) == 0 &&
                    self_field != NULL)
                {
                    struct Expression *self_expr = mk_varid(expr->line_num, strdup("Self"));
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
                                self_expr->resolved_kgpc_type = self_record_type;
                        }

                        char *saved_id = record_expr->expr_data.id;
                        record_expr->expr_data.id = NULL;
                        record_expr->type = EXPR_RECORD_ACCESS;
                        memset(&record_expr->expr_data.record_access_data, 0,
                            sizeof(expr->expr_data.record_access_data));
                        record_expr->expr_data.record_access_data.record_expr = self_expr;
                        record_expr->expr_data.record_access_data.field_id = saved_id;
                        record_expr->expr_data.record_access_data.field_offset = field_offset;

                        if (self_field->nested_record != NULL)
                            record_info = self_field->nested_record;
                        else if (self_field->type_id != NULL)
                            record_info = semcheck_lookup_record_type(symtab, self_field->type_id);

                        if (record_info != NULL)
                        {
                            if (record_kgpc_type != NULL)
                                kgpc_type_release(record_kgpc_type);
                            record_kgpc_type = create_record_type(record_info);
                            record_type = RECORD_TYPE;
                        }
                    }
                }
            SKIP_SELF_REWRITE:
                ;
            }
        }
    }

    struct RecordField *field_desc = NULL;
    long long field_offset = 0;
    int property_matched = 0;
    /* For classes and records with potential methods, use silent mode when looking for fields,
     * since we'll check properties and methods next */
    int silent_mode = 1;  /* Always use silent mode - we'll print a better error later if needed */
    if (resolve_record_field(symtab, record_info, field_id, &field_desc,
            &field_offset, expr->line_num, silent_mode) != 0 || field_desc == NULL)
    {
        if (record_info != NULL && record_info->is_type_helper &&
            record_info->helper_base_type_id != NULL)
        {
            struct RecordType *base_record =
                semcheck_lookup_record_type(symtab, record_info->helper_base_type_id);
            if (base_record != NULL)
            {
                struct RecordField *base_field = NULL;
                long long base_offset = 0;
                if (resolve_record_field(symtab, base_record, field_id, &base_field,
                        &base_offset, expr->line_num, 1) == 0 && base_field != NULL)
                {
                    record_info = base_record;
                    field_desc = base_field;
                    field_offset = base_offset;
                }
            }
        }
        if (field_desc == NULL && record_kgpc_type != NULL &&
            record_kgpc_type->type_alias != NULL)
        {
            const char *alias_target = record_kgpc_type->type_alias->target_type_id;
            const char *alias_name = record_kgpc_type->type_alias->alias_name;
            if (alias_target != NULL || alias_name != NULL)
            {
                struct RecordType *alias_record = NULL;
                if (alias_target != NULL)
                    alias_record = semcheck_lookup_record_type(symtab, alias_target);
                if (alias_record == NULL && alias_name != NULL)
                    alias_record = semcheck_lookup_record_type(symtab, alias_name);
                if (alias_record != NULL)
                {
                    struct RecordField *alias_field = NULL;
                    long long alias_offset = 0;
                    if (resolve_record_field(symtab, alias_record, field_id, &alias_field,
                            &alias_offset, expr->line_num, 1) == 0 && alias_field != NULL)
                    {
                        record_info = alias_record;
                        field_desc = alias_field;
                        field_offset = alias_offset;
                    }
                }
            }
        }
        if (field_desc == NULL && record_expr->type == EXPR_VAR_ID &&
            record_expr->expr_data.id != NULL)
        {
            HashNode_t *record_node = NULL;
            if (FindSymbol(&record_node, symtab, record_expr->expr_data.id) != 0 &&
                record_node != NULL)
            {
                struct RecordType *node_record = get_record_type_from_node(record_node);
                if (node_record != NULL)
                {
                    struct RecordField *node_field = NULL;
                    long long node_offset = 0;
                    if (resolve_record_field(symtab, node_record, field_id, &node_field,
                            &node_offset, expr->line_num, 1) == 0 && node_field != NULL)
                    {
                        record_info = node_record;
                        field_desc = node_field;
                        field_offset = node_offset;
                    }
                }
            }
        }
        if (record_info != NULL && record_info->type_id != NULL)
        {
            ListNode_t *type_matches = FindAllIdents(symtab, record_info->type_id);
            for (ListNode_t *mcur = type_matches; mcur != NULL; mcur = mcur->next)
            {
                HashNode_t *mnode = (HashNode_t *)mcur->cur;
                if (mnode == NULL || mnode->hash_type != HASHTYPE_TYPE)
                    continue;
                struct RecordType *alt_record = get_record_type_from_node(mnode);
                if (alt_record == NULL || alt_record == record_info)
                    continue;
                struct RecordField *alt_field = NULL;
                long long alt_offset = 0;
                if (resolve_record_field(symtab, alt_record, field_id, &alt_field,
                        &alt_offset, expr->line_num, 1) == 0 && alt_field != NULL)
                {
                    record_info = alt_record;
                    field_desc = alt_field;
                    field_offset = alt_offset;
                    break;
                }
            }
            if (type_matches != NULL)
                DestroyList(type_matches);
        }
    }
    if (field_desc == NULL)
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
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, property %s is write-only.\n\n",
                        expr->line_num, property->name != NULL ? property->name : field_id);
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }

                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck]   Property read_accessor='%s'\n",
                        property->read_accessor ? property->read_accessor : "<null>");
                }

                struct RecordField *read_field =
                    semcheck_find_class_field_including_hidden(symtab,
                        record_info, property->read_accessor, NULL);
                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck]   Found read_field=%p\n", read_field);
                }
                if (read_field != NULL &&
                    resolve_record_field(symtab, record_info, property->read_accessor,
                        &field_desc, &field_offset, expr->line_num, 0) == 0 &&
                    field_desc != NULL)
                {
                    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck]   Transforming property '%s' to field '%s'\n",
                            field_id, property->read_accessor);
                    }
                    if (!pascal_identifier_equals(field_id, property->read_accessor))
                    {
                        free(expr->expr_data.record_access_data.field_id);
                        expr->expr_data.record_access_data.field_id = strdup(property->read_accessor);
                        if (expr->expr_data.record_access_data.field_id == NULL)
                        {
                            semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, failed to allocate property field name.\n\n",
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
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, getter %s for property %s not found.\n\n",
                        expr->line_num,
                        property->read_accessor != NULL ? property->read_accessor : "<unknown>",
                        property->name != NULL ? property->name : field_id);
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }
                int getter_is_function = (getter_node->hash_type == HASHTYPE_FUNCTION);
                if (!getter_is_function && getter_node->type != NULL &&
                    getter_node->type->kind == TYPE_KIND_PROCEDURE &&
                    getter_node->type->info.proc_info.return_type == NULL)
                {
                    int prop_type = UNKNOWN_TYPE;
                    struct RecordType *prop_record = NULL;
                    if (semcheck_property_type_info(symtab, property, expr->line_num,
                            &prop_type, &prop_record) == 0)
                    {
                        KgpcType *prop_kgpc = NULL;
                        if (prop_record != NULL)
                            prop_kgpc = create_record_type(prop_record);
                        else if (prop_type != UNKNOWN_TYPE)
                            prop_kgpc = create_primitive_type(prop_type);
                        if (prop_kgpc != NULL)
                        {
                            getter_node->type->info.proc_info.return_type = prop_kgpc;
                            getter_node->hash_type = HASHTYPE_FUNCTION;
                            getter_is_function = 1;
                        }
                    }
                }
                if (!getter_is_function && getter_node->type != NULL &&
                    getter_node->type->kind == TYPE_KIND_PROCEDURE)
                {
                    KgpcType *ret_type = kgpc_type_get_return_type(getter_node->type);
                    if (ret_type != NULL)
                        getter_is_function = 1;
                }
                if (!getter_is_function)
                {
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, property getter %s must be a function.\n\n",
                        expr->line_num, property->read_accessor);
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }

                return semcheck_transform_property_getter_call(type_return, symtab,
                    expr, max_scope_lev, mutating, getter_node, property_owner);
            }
            else
            {
                if (property->write_accessor == NULL)
                {
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, property %s is read-only.\n\n",
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
                            semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, failed to allocate property field name.\n\n",
                                expr->line_num);
                            *type_return = UNKNOWN_TYPE;
                            return error_count + 1;
                        }
                    }
                    goto FIELD_RESOLVED;
                }

                if (mutating == BOTH_MUTATE_REFERENCE)
                {
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, property %s cannot be passed as a var parameter.\n\n",
                        expr->line_num, property->name != NULL ? property->name : field_id);
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }

                HashNode_t *setter_node = semcheck_find_class_method(symtab,
                    property_owner, property->write_accessor, NULL);
                if (setter_node == NULL)
                {
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, setter %s for property %s not found.\n\n",
                        expr->line_num,
                        property->write_accessor != NULL ? property->write_accessor : "<unknown>",
                        property->name != NULL ? property->name : field_id);
                    *type_return = UNKNOWN_TYPE;
                    return error_count + 1;
                }
                if (setter_node->hash_type != HASHTYPE_PROCEDURE)
                {
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, property setter %s must be a procedure.\n\n",
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

                semcheck_expr_set_resolved_type(expr, property_type);
                if (property_type == RECORD_TYPE && property_record != NULL)
                {
                    KgpcType *property_record_type = create_record_type(property_record);
                    if (property_record_type != NULL)
                    {
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, property_record_type);
                        destroy_kgpc_type(property_record_type);
                    }
                }
                *type_return = property_type;
                return error_count;
            }

            return error_count;
        }

        /* Check for methods (including constructors) */
        struct RecordType *method_owner_record = NULL;
        HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, field_id, &method_owner_record);
        if (method_node != NULL)
        {
            /* Found a method/constructor */
            if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] semcheck_recordaccess: Found method %s\n", field_id);
            }

                if (method_node->hash_type == HASHTYPE_FUNCTION ||
                    method_node->hash_type == HASHTYPE_PROCEDURE)
                {
                    int is_static_method = 0;
                    if (record_info->type_id != NULL && field_id != NULL) {
                        is_static_method = from_cparser_is_method_static(record_info->type_id, field_id);
                    }
                    /* If not found on the receiver's class, check the actual owner
                     * (inherited static methods are registered under the declaring class). */
                    if (!is_static_method && method_owner_record != NULL &&
                        method_owner_record->type_id != NULL &&
                        field_id != NULL) {
                        is_static_method = from_cparser_is_method_static(method_owner_record->type_id, field_id);
                    }

                    /* For overloaded methods, we need to find the correct overload based on
                     * argument count. Since this is a bare record access (s.Foo without parens),
                     * there are no explicit arguments - only the implicit Self parameter.
                     * Find the overload that accepts just Self (1 param). */
                    int args_for_call = is_static_method ? 0 : 1; /* Just Self for non-static */
                    
                    if (record_info->type_id != NULL) {
                        char mangled_base[256];
                        snprintf(mangled_base, sizeof(mangled_base), "%s__%s",
                            record_info->type_id, field_id);
                        
                        ListNode_t *all_methods = FindAllIdents(symtab, mangled_base);
                        if (all_methods != NULL) {
                            /* Find the best overload: one that requires exactly args_for_call params */
                            ListNode_t *cur = all_methods;
                            HashNode_t *best_match = NULL;
                            
                            while (cur != NULL) {
                                HashNode_t *candidate = (HashNode_t *)cur->cur;
                                if (candidate != NULL &&
                                    (candidate->hash_type == HASHTYPE_FUNCTION ||
                                     candidate->hash_type == HASHTYPE_PROCEDURE) &&
                                    candidate->type != NULL)
                                {
                                    ListNode_t *params = kgpc_type_get_procedure_params(candidate->type);
                                    int total_params = semcheck_count_total_params(params);
                                    int required_params = semcheck_count_required_params(params);
                                    
                                    /* Check if this overload accepts args_for_call arguments */
                                    if (args_for_call >= required_params && args_for_call <= total_params) {
                                        /* Prefer exact match over range match */
                                        if (best_match == NULL) {
                                            best_match = candidate;
                                        } else {
                                            /* Pick the one with fewer total params (more specific) */
                                            ListNode_t *best_params = kgpc_type_get_procedure_params(best_match->type);
                                            int best_total = semcheck_count_total_params(best_params);
                                            if (total_params < best_total) {
                                                best_match = candidate;
                                            }
                                        }
                                    }
                                }
                                cur = cur->next;
                            }
                            
                            if (best_match != NULL) {
                                method_node = best_match;
                            }
                            
                            DestroyList(all_methods);
                        }
                    }

                    /* Transform record access into an explicit method call: receiver.Method() */
                    char *method_id = (field_id != NULL) ? strdup(field_id) : NULL;

                    /* For static methods:
                     * - If receiver is a type identifier (TypeName.Method), pass it as first arg
                     *   so semcheck_funccall can detect and handle the type-qualified call
                     * - If receiver is an instance variable, no receiver needed for static method
                     */
                    ListNode_t *call_args = NULL;
                    int receiver_moved = 0;
                    if (is_static_method) {
                        if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL) {
                            /* Check if the receiver is a type name */
                            HashNode_t *type_node = NULL;
                            if (FindSymbol(&type_node, symtab, record_expr->expr_data.id) != 0 &&
                                type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) {
                                /* It's a type-qualified static method call - pass type as first arg */
                                struct Expression *type_arg = record_expr;
                                call_args = CreateListNode(type_arg, LIST_EXPR);
                                receiver_moved = (call_args != NULL);
                            }
                        }
                    } else {
                        struct Expression *receiver = record_expr;
                        call_args = CreateListNode(receiver, LIST_EXPR);
                        receiver_moved = (call_args != NULL);
                    }
                    /* Check if this is a declared constructor on a class type.
                     * After semcheck_funccall resolves the inherited constructor,
                     * we need to override the return type to be the calling class,
                     * not the class where the constructor is declared. */
                    const char *method_name = NULL;
                    if (method_node->method_name != NULL)
                        method_name = method_node->method_name;
                    else if (field_id != NULL)
                        method_name = field_id;
                    int is_constructor_call = (!is_static_method &&
                        record_info != NULL &&
                        record_type_is_class(record_info) &&
                        !record_info->is_type_helper &&
                        method_name != NULL &&
                        semcheck_method_is_declared_constructor(symtab, record_info, method_name));
                    char *method_name_copy = (method_name != NULL) ? strdup(method_name) : NULL;
                    record_access_clear_payload(expr, !receiver_moved);

                    expr->type = EXPR_FUNCTION_CALL;
                    memset(&expr->expr_data.function_call_data, 0,
                        sizeof(expr->expr_data.function_call_data));
                    expr->expr_data.function_call_data.is_method_call_placeholder = 1;
                    expr->expr_data.function_call_data.id = method_id;
                    expr->expr_data.function_call_data.placeholder_method_name = method_name_copy;
                    expr->expr_data.function_call_data.mangled_id = NULL;
                    expr->expr_data.function_call_data.resolved_func = NULL;
                    if (method_owner_record != NULL && method_owner_record->type_id != NULL)
                        expr->expr_data.function_call_data.cached_owner_class =
                            strdup(method_owner_record->type_id);
                    if (method_name_copy != NULL)
                        expr->expr_data.function_call_data.cached_method_name = strdup(method_name_copy);
                    expr->expr_data.function_call_data.args_expr = call_args;
                    /* Re-run semantic checking as a function call */
                    semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
                    int funccall_result = semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);

                    /* For constructor calls, override the return type to be the
                     * calling class (e.g. TResolveReferenceVisitor.Create should
                     * return TResolveReferenceVisitor, not TObject). */
                    if (is_constructor_call)
                    {
                        expr->expr_data.function_call_data.is_constructor_call = 1;
                        KgpcType *record_kgpc = create_record_type(record_info);
                        if (record_kgpc != NULL)
                        {
                            KgpcType *ptr_type = create_pointer_type(record_kgpc);
                            /* Release local ref; create_pointer_type retained its own */
                            kgpc_type_release(record_kgpc);
                            if (ptr_type != NULL)
                            {
                                semcheck_expr_set_resolved_kgpc_type_shared(expr, ptr_type);
                                semcheck_expr_set_resolved_type(expr, POINTER_TYPE);
                                if (type_return != NULL)
                                    *type_return = POINTER_TYPE;
                                destroy_kgpc_type(ptr_type);
                            }
                        }
                    }

                    return funccall_result;
                }
        }

        if (property_matched)
            return error_count;

        /* Special handling for default Create constructor */
        /* If the field is "Create" and this is a class type, treat it as a default constructor */
        if (record_type_is_class(record_info) && field_id != NULL && 
            pascal_identifier_equals(field_id, "Create"))
        {
            /* Transform this EXPR_RECORD_ACCESS into EXPR_FUNCTION_CALL */
            /* The record_expr could be either:
             * 1. A type name (EXPR_VAR_ID) like TMyClass.Create - use static VMT
             * 2. A class reference variable like ClassRef.Create - use variable's value as VMT
             */
            if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL)
            {
                const char *expr_name = record_expr->expr_data.id;
                
                /* Check if this is a type name or a variable name */
                HashNode_t *ident_node = NULL;
                int is_type = 0;
                if (FindSymbol(&ident_node, symtab, (char*)expr_name) != 0 && ident_node != NULL) {
                    is_type = (ident_node->hash_type == HASHTYPE_TYPE);
                }
                
                /* Clean up the old record_access_data before transforming */
                expr->expr_data.record_access_data.record_expr = NULL;
                
                /* Calculate class size using KgpcType */
                KgpcType *record_kgpc = create_record_type(record_info);
                if (record_kgpc == NULL) {
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d: Unable to create KgpcType for class %s\n", 
                        expr->line_num, expr_name);
                    destroy_expr(record_expr);
                    return error_count + 1;
                }
                
                long long class_size = kgpc_type_sizeof(record_kgpc);
                if (class_size <= 0) {
                    semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d: Unable to determine size for class %s\n", 
                        expr->line_num, expr_name);
                    destroy_expr(record_expr);
                    return error_count + 1;
                }
                
                /* Create argument 1: class size as integer literal */
                struct Expression *size_arg = (struct Expression *)calloc(1, sizeof(struct Expression));
                size_arg->type = EXPR_INUM;
                size_arg->expr_data.i_num = class_size;
                semcheck_expr_set_resolved_type(size_arg, INT_TYPE);
                
                /* Create argument 2: VMT pointer */
                struct Expression *vmt_arg;
                if (is_type) {
                    /* Static class type: use address of global VMT label */
                    vmt_arg = (struct Expression *)calloc(1, sizeof(struct Expression));
                    vmt_arg->type = EXPR_VAR_ID;
                    char vmt_label[256];
                    snprintf(vmt_label, sizeof(vmt_label), "%s_VMT", expr_name);
                    vmt_arg->expr_data.id = strdup(vmt_label);
                    semcheck_expr_set_resolved_type(vmt_arg, POINTER_TYPE);
                } else {
                    /* Class reference variable: use the variable's value as VMT */
                    /* The variable already holds a pointer to the VMT */
                    vmt_arg = record_expr;  /* Reuse the record_expr directly */
                    record_expr = NULL;     /* Prevent double-free */
                }
                
                /* Create argument list */
                ListNode_t *arg1_node = CreateListNode(size_arg, LIST_EXPR);
                ListNode_t *arg2_node = CreateListNode(vmt_arg, LIST_EXPR);
                arg1_node->next = arg2_node;
                char *debug_field_id = (field_id != NULL) ? strdup(field_id) : NULL;
                if (expr->expr_data.record_access_data.field_id != NULL)
                {
                    free(expr->expr_data.record_access_data.field_id);
                    expr->expr_data.record_access_data.field_id = NULL;
                }
                
                /* Transform the expression into a function call to __kgpc_default_create */
                expr->type = EXPR_FUNCTION_CALL;
                /* Initialize function_call_data - use memset to clear the union */
                memset(&expr->expr_data.function_call_data, 0, sizeof(expr->expr_data.function_call_data));
                expr->expr_data.function_call_data.id = strdup("__kgpc_default_create");
                expr->expr_data.function_call_data.mangled_id = strdup("__kgpc_default_create");
                expr->expr_data.function_call_data.args_expr = arg1_node;
                
                /* Set the return type information */
                semcheck_expr_set_resolved_type(expr, POINTER_TYPE);
                
                /* Create a KgpcType for the class (pointer to record) */
                KgpcType *class_kgpc = create_pointer_type(record_kgpc);
                kgpc_type_release(record_kgpc);
                semcheck_expr_set_resolved_kgpc_type_shared(expr, class_kgpc);
                destroy_kgpc_type(class_kgpc);
                
                *type_return = POINTER_TYPE;
                
                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_recordaccess: Transformed '%s.%s' to __kgpc_default_create(%lld, %s) call\n",
                        expr_name, debug_field_id != NULL ? debug_field_id : "<null>",
                        class_size, is_type ? "(static VMT)" : "(runtime VMT)");
                }
                free(debug_field_id);
                
                /* Free the record_expr only if we didn't reuse it for vmt_arg */
                if (record_expr != NULL)
                    destroy_expr(record_expr);
                
                return error_count;
            }
        }

        /* Check for methods on non-class records (advanced records) */
        /* This handles {$modeswitch advancedrecords} style record methods */
        /* Unlike classes, advanced records don't use VMT but still have methods
         * registered with mangled names (TypeName__MethodName) in the symbol table */
        if (record_info != NULL && !record_type_is_class(record_info))
        {
            HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, field_id, NULL);
            if (method_node != NULL)
            {
                /* Found a method on an advanced record */
                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_recordaccess: Found advanced record method %s\n", field_id);
                }

                if (method_node->hash_type == HASHTYPE_FUNCTION ||
                    method_node->hash_type == HASHTYPE_PROCEDURE)
                {
                    /* Check if this is a static method (no Self parameter) */
                    const char *type_name = record_info->type_id;
                    int is_static_method = 0;
                    if (type_name != NULL && field_id != NULL) {
                        is_static_method = from_cparser_is_method_static(type_name, field_id);
                    }
                    
                    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck] semcheck_recordaccess: type=%s method=%s is_static=%d\n",
                            type_name ? type_name : "<null>", field_id, is_static_method);
                    }
                    
                    /* Transform record access into an explicit method call.
                     * Use the base mangled name (Type__Method) rather than a
                     * specific overload's mangled_id so that semcheck_funccall
                     * can perform proper overload resolution. */
                    char *base_mangled = NULL;
                    if (type_name != NULL && field_id != NULL)
                    {
                        size_t bm_len = strlen(type_name) + 2 + strlen(field_id) + 1;
                        base_mangled = (char *)malloc(bm_len);
                        if (base_mangled != NULL)
                            snprintf(base_mangled, bm_len, "%s__%s", type_name, field_id);
                    }
                    char *method_id = (base_mangled != NULL) ? base_mangled :
                        ((field_id != NULL) ? strdup(field_id) : NULL);

                    ListNode_t *call_args = NULL;
                    int receiver_moved = 0;
                    if (!is_static_method) {
                        /* For instance methods, pass receiver as first argument (Self) */
                        struct Expression *receiver = record_expr;
                        call_args = CreateListNode(receiver, LIST_EXPR);
                        receiver_moved = (call_args != NULL);
                    }
                    record_access_clear_payload(expr, !receiver_moved);

                    expr->type = EXPR_FUNCTION_CALL;
                    memset(&expr->expr_data.function_call_data, 0,
                        sizeof(expr->expr_data.function_call_data));
                    expr->expr_data.function_call_data.is_method_call_placeholder = 0;
                    expr->expr_data.function_call_data.id = method_id;
                    if (method_id != NULL)
                        expr->expr_data.function_call_data.mangled_id = strdup(method_id);
                    /* Don't pre-bind resolved_func or call_kgpc_type — let
                     * semcheck_funccall handle overload resolution. */
                    expr->expr_data.function_call_data.is_call_info_valid = 0;
                    expr->expr_data.function_call_data.args_expr = call_args;

                    /* Re-run semantic checking as a function call */
                    semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
                    return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                }
            }
        }

        /* Check for type helpers: If no method was found on the target type itself,
         * look for a helper bound to that type and check for methods there. */
        if (record_info != NULL &&
            record_info->type_id != NULL && !record_info->is_type_helper)
        {
            struct RecordType *helper_record =
                semcheck_lookup_type_helper_for_record_member(symtab,
                    record_info, field_id);
            if (helper_record != NULL)
            {
                HashNode_t *method_node = semcheck_find_class_method(symtab,
                    helper_record, field_id, NULL);
                if (method_node != NULL)
                {
                    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck] semcheck_recordaccess: Found type helper method %s on %s\n",
                            field_id, helper_record->type_id);
                    }

                    if (method_node->hash_type == HASHTYPE_FUNCTION ||
                        method_node->hash_type == HASHTYPE_PROCEDURE)
                    {
                        const char *type_name = helper_record->type_id;
                        int is_static_method = 0;
                        if (type_name != NULL && field_id != NULL) {
                            is_static_method = from_cparser_is_method_static(type_name, field_id);
                        }

                        char *method_id = (method_node->mangled_id != NULL) ?
                            strdup(method_node->mangled_id) :
                            ((field_id != NULL) ? strdup(field_id) : NULL);

                        ListNode_t *call_args = NULL;
                        int receiver_moved = 0;
                        if (!is_static_method) {
                            struct Expression *receiver = record_expr;
                            call_args = CreateListNode(receiver, LIST_EXPR);
                            receiver_moved = (call_args != NULL);
                        }
                        record_access_clear_payload(expr, !receiver_moved);

                        expr->type = EXPR_FUNCTION_CALL;
                        memset(&expr->expr_data.function_call_data, 0,
                            sizeof(expr->expr_data.function_call_data));
                        expr->expr_data.function_call_data.is_method_call_placeholder = 0;
                        expr->expr_data.function_call_data.id = method_id;
                        if (method_node->mangled_id != NULL)
                            expr->expr_data.function_call_data.mangled_id =
                                strdup(method_node->mangled_id);
                        else if (method_id != NULL)
                            expr->expr_data.function_call_data.mangled_id = strdup(method_id);
                        expr->expr_data.function_call_data.resolved_func = method_node;
                        expr->expr_data.function_call_data.call_hash_type = method_node->hash_type;
                        semcheck_expr_set_call_kgpc_type(expr, method_node->type, 0);
                        expr->expr_data.function_call_data.is_call_info_valid = 1;
                        expr->expr_data.function_call_data.args_expr = call_args;

                        semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
                        return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                    }
                }
            }
        }

        if (record_info != NULL && record_info->is_type_helper && record_info->type_id != NULL)
        {
            char *mangled_const = semcheck_mangle_helper_const_id(record_info->type_id, field_id);
            HashNode_t *const_node = NULL;
            if (mangled_const != NULL &&
                FindSymbol(&const_node, symtab, mangled_const) != 0 &&
                const_node != NULL && const_node->hash_type == HASHTYPE_CONST)
            {
                destroy_expr(record_expr);
                expr->expr_data.record_access_data.record_expr = NULL;
                if (expr->expr_data.record_access_data.field_id != NULL)
                {
                    free(expr->expr_data.record_access_data.field_id);
                    expr->expr_data.record_access_data.field_id = NULL;
                }
                expr->type = EXPR_VAR_ID;
                expr->expr_data.id = mangled_const;
                return semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
            }
            if (mangled_const != NULL)
                free(mangled_const);
        }

        /* Type helper string builtins: In type helpers for strings,
         * Self.Length should resolve to Length(Self) */
        if (record_info != NULL && record_info->is_type_helper &&
            record_info->helper_base_type_id != NULL &&
            (pascal_identifier_equals(record_info->helper_base_type_id, "AnsiString") ||
             pascal_identifier_equals(record_info->helper_base_type_id, "String") ||
             pascal_identifier_equals(record_info->helper_base_type_id, "ShortString") ||
             pascal_identifier_equals(record_info->helper_base_type_id, "UnicodeString") ||
             pascal_identifier_equals(record_info->helper_base_type_id, "WideString")))
        {
            if (pascal_identifier_equals(field_id, "Length"))
            {
                /* Transform 'Self.Length' into 'Length(Self)' */
                char *func_id = strdup("Length");
                if (func_id != NULL)
                {
                    /* record_expr is already the Self expression */
                    ListNode_t *args_list = CreateListNode(record_expr, LIST_EXPR);
                    if (args_list != NULL)
                    {
                        record_access_clear_payload(expr, 0);
                        expr->type = EXPR_FUNCTION_CALL;
                        memset(&expr->expr_data.function_call_data, 0,
                            sizeof(expr->expr_data.function_call_data));
                        expr->expr_data.function_call_data.id = func_id;
                        expr->expr_data.function_call_data.args_expr = args_list;
                        expr->expr_data.function_call_data.mangled_id = NULL;
                        semcheck_reset_function_call_cache(expr);
                        return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                    }
                    free(func_id);
                }
            }
        }

        /* Check record_properties for plain records (Delphi advanced records) */
        if (record_info != NULL && record_info->record_properties != NULL)
        {
            struct ClassProperty *property = NULL;
            /* Search record_properties list */
            ListNode_t *pnode = record_info->record_properties;
            while (pnode != NULL)
            {
                if (pnode->type == LIST_CLASS_PROPERTY && pnode->cur != NULL)
                {
                    struct ClassProperty *p = (struct ClassProperty *)pnode->cur;
                    if (p->name != NULL && pascal_identifier_equals(p->name, field_id))
                    {
                        property = p;
                        break;
                    }
                }
                pnode = pnode->next;
            }
            if (property != NULL)
            {
                const char *accessor = (mutating != NO_MUTATE) ? property->write_accessor : property->read_accessor;
                if (accessor != NULL)
                {
                    /* Resolve property to its backing field */
                    if (resolve_record_field(symtab, record_info, accessor,
                            &field_desc, &field_offset, expr->line_num, 1) == 0 && field_desc != NULL)
                    {
                        if (!pascal_identifier_equals(field_id, accessor))
                        {
                            free(expr->expr_data.record_access_data.field_id);
                            expr->expr_data.record_access_data.field_id = strdup(accessor);
                        }
                        goto FIELD_RESOLVED;
                    }

                    if (mutating != NO_MUTATE)
                    {
                        /* For setter method calls on record properties, leave the expression
                         * as EXPR_RECORD_ACCESS so the assignment handler in
                         * semcheck_try_property_assignment() can construct the setter call
                         * with the value argument. Return 0 to suppress "field not found"
                         * and let the assignment handler take over. */
                        return 0;
                    }

                    HashNode_t *method_node = semcheck_find_class_method(symtab, record_info,
                        accessor, NULL);
                    if (method_node != NULL)
                    {
                        struct Expression *receiver = record_expr;
                        ListNode_t *arg_node = CreateListNode(receiver, LIST_EXPR);
                        if (arg_node != NULL)
                        {
                            char *method_id = strdup(accessor);
                            record_access_clear_payload(expr, 0);
                            expr->type = EXPR_FUNCTION_CALL;
                            memset(&expr->expr_data.function_call_data, 0,
                                sizeof(expr->expr_data.function_call_data));
                            expr->expr_data.function_call_data.is_method_call_placeholder = 1;
                            expr->expr_data.function_call_data.placeholder_method_name = strdup(accessor);
                            expr->expr_data.function_call_data.id = method_id;
                            if (method_node->mangled_id != NULL)
                                expr->expr_data.function_call_data.mangled_id =
                                    strdup(method_node->mangled_id);
                            expr->expr_data.function_call_data.resolved_func = method_node;
                            expr->expr_data.function_call_data.args_expr = arg_node;
                            semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
                            return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
                        }
                    }
                }
            }
        }

        /* For classes and object types, check if the field is a class/object const
         * stored with mangled name ClassName__ConstName. */
        if (record_info != NULL && record_info->type_id != NULL && field_id != NULL)
        {
            char *mangled_const = semcheck_mangle_helper_const_id(record_info->type_id, field_id);
            HashNode_t *const_node = NULL;
            if (mangled_const != NULL &&
                FindSymbol(&const_node, symtab, mangled_const) != 0 &&
                const_node != NULL &&
                (const_node->hash_type == HASHTYPE_CONST ||
                 const_node->hash_type == HASHTYPE_ARRAY ||
                 const_node->hash_type == HASHTYPE_VAR ||
                 const_node->is_typed_const))
            {
                destroy_expr(record_expr);
                expr->expr_data.record_access_data.record_expr = NULL;
                if (expr->expr_data.record_access_data.field_id != NULL)
                {
                    free(expr->expr_data.record_access_data.field_id);
                    expr->expr_data.record_access_data.field_id = NULL;
                }
                expr->type = EXPR_VAR_ID;
                expr->expr_data.id = mangled_const;
                return semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
            }
            if (mangled_const != NULL)
                free(mangled_const);
        }

        /* For classes, check if the field is a nested type (e.g., TClass.TNestedEnum).
         * Nested types are registered as "ClassName.NestedTypeName" in the symbol table. */
        if (record_info != NULL && record_info->type_id != NULL && field_id != NULL)
        {
            size_t owner_len = strlen(record_info->type_id);
            size_t field_len = strlen(field_id);
            char *qualified_name = (char *)malloc(owner_len + 1 + field_len + 1);
            if (qualified_name != NULL)
            {
                snprintf(qualified_name, owner_len + 1 + field_len + 1, "%s.%s",
                    record_info->type_id, field_id);
                HashNode_t *type_node = NULL;
                if (FindSymbol(&type_node, symtab, qualified_name) != 0 &&
                    type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                {
                    /* Found a nested type.  Transform the record access into
                     * a reference to this type so further accesses (e.g.
                     * TClass.TEnum.Value) can resolve correctly. */
                    struct TypeAlias *ta = hashnode_get_type_alias(type_node);
                    int nested_type_tag = UNKNOWN_TYPE;
                    if (ta != NULL && ta->is_enum)
                        nested_type_tag = ENUM_TYPE;
                    else if (type_node->type != NULL)
                        nested_type_tag = semcheck_tag_from_kgpc(type_node->type);

                    /* Transform to EXPR_VAR_ID pointing at the qualified type name
                     * so that outer record_access (e.g. .EnumValue) can resolve
                     * it via the existing scoped-enum handler. */
                    destroy_expr(record_expr);
                    expr->expr_data.record_access_data.record_expr = NULL;
                    if (expr->expr_data.record_access_data.field_id != NULL)
                    {
                        free(expr->expr_data.record_access_data.field_id);
                        expr->expr_data.record_access_data.field_id = NULL;
                    }
                    expr->type = EXPR_VAR_ID;
                    expr->expr_data.id = qualified_name;
                    semcheck_expr_set_resolved_type(expr, nested_type_tag);
                    if (type_node->type != NULL)
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, type_node->type);
                    *type_return = nested_type_tag;
                    return 0;
                }
                free(qualified_name);
            }
        }

        if (record_info != NULL && record_info->type_id != NULL)
        {
            semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, record field %s not found on type '%s'.\n",
                expr->line_num, field_id, record_info->type_id);
        }
        else
            semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, record field %s not found.\n",
                expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

FIELD_RESOLVED:
    /* Re-read field_id: property resolution may have freed and replaced
       the original string, making the local pointer stale. */
    field_id = expr->expr_data.record_access_data.field_id;
    expr->expr_data.record_access_data.field_offset = field_offset;

    int field_type = field_desc->type;
    struct RecordType *field_record = field_desc->nested_record;
    if (field_record != NULL)
        field_type = RECORD_TYPE;
    /* Handle inline pointer fields like bufptr: ^Char */
    if (field_desc->is_pointer)
        field_type = POINTER_TYPE;
    /* Procedural fields (function/procedure pointers) */
    if (field_desc->proc_type != NULL)
        field_type = PROCEDURE;
    if (kgpc_getenv("KGPC_DEBUG_PROC_FIELD") != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_PROC_FIELD] field=%s type=%d raw_type=%d type_id=%s proc_type=%p\n",
            field_id != NULL ? field_id : "<null>",
            field_type,
            field_desc->type,
            field_desc->type_id != NULL ? field_desc->type_id : "<null>",
            (void *)field_desc->proc_type);
    }
    if (kgpc_getenv("KGPC_DEBUG_POINTER_FIELD") != NULL && field_id != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_POINTER_FIELD] field=%s type=%d is_pointer=%d pointer_type=%d pointer_type_id=%s\n",
            field_id,
            field_desc->type,
            field_desc->is_pointer,
            field_desc->pointer_type,
            field_desc->pointer_type_id ? field_desc->pointer_type_id : "<null>");
    }
    if (kgpc_getenv("KGPC_DEBUG_RECORD_FIELD") != NULL &&
        field_id != NULL &&
        (pascal_identifier_equals(field_id, "st_ctime") ||
         pascal_identifier_equals(field_id, "st_mtime") ||
         pascal_identifier_equals(field_id, "st_atime") ||
         pascal_identifier_equals(field_id, "VChar")))
    {
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_FIELD] field=%s type=%d type_id=%s record=%p\n",
            field_id,
            field_desc->type,
            field_desc->type_id ? field_desc->type_id : "<null>",
            (void *)field_desc->nested_record);
    }
    if (kgpc_getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL && field_id != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_ACCESS] field=%s type=%d type_id=%s record=%p resolved=%s is_array=%d elem_type=%d elem_type_id=%s elem_record=%p elem_kgpc=%p is_pointer=%d\n",
            field_id,
            field_desc->type,
            field_desc->type_id ? field_desc->type_id : "<null>",
            (void *)field_desc->nested_record,
            expr->resolved_kgpc_type ? kgpc_type_to_string(expr->resolved_kgpc_type) : "<null>",
            field_desc->is_array,
            field_desc->array_element_type,
            field_desc->array_element_type_id ? field_desc->array_element_type_id : "<null>",
            (void *)field_desc->array_element_record,
            (void *)field_desc->array_element_kgpc_type,
            field_desc->is_pointer);
    }

    if (field_desc->is_array)
    {
        semcheck_clear_array_info(expr);
        expr->is_array_expr = 1;
        expr->array_lower_bound = field_desc->array_start;
        expr->array_upper_bound = field_desc->array_end;
        expr->array_is_dynamic = field_desc->array_is_open;
        expr->array_element_type = field_desc->array_element_type;
        const TypeRef *array_element_ref = field_desc->array_element_type_ref;
        const char *array_element_id = field_desc->array_element_type_id;
        if (array_element_id != NULL || array_element_ref != NULL)
        {
            if (array_element_id != NULL)
                expr->array_element_type_id = strdup(array_element_id);
            if (array_element_ref != NULL)
                expr->array_element_type_ref = type_ref_clone(array_element_ref);
            if (expr->array_element_type == UNKNOWN_TYPE)
            {
                HashNode_t *elem_type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                    array_element_ref, array_element_id);
                if (elem_type_node == NULL)
                    elem_type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                        array_element_ref, array_element_id);
                if (elem_type_node != NULL)
                    set_type_from_hashtype(&expr->array_element_type, elem_type_node);
                if (expr->array_element_type == UNKNOWN_TYPE)
                {
                    const char *base_name = array_element_ref != NULL
                        ? type_ref_base_name(array_element_ref)
                        : array_element_id;
                    int builtin = semcheck_map_builtin_type_name(symtab, base_name);
                    if (builtin != UNKNOWN_TYPE)
                        expr->array_element_type = builtin;
                }
            }
            HashNode_t *elem_node = semcheck_find_preferred_type_node_with_ref(symtab,
                array_element_ref, array_element_id);
            if (elem_node == NULL)
                elem_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                    array_element_ref, array_element_id);
            if (elem_node != NULL)
                expr->array_element_record_type = get_record_type_from_node(elem_node);
            if (expr->array_element_record_type == NULL && elem_node != NULL)
            {
                struct TypeAlias *elem_alias = get_type_alias_from_node(elem_node);
                if (elem_alias != NULL)
                {
                    if (elem_alias->inline_record_type != NULL)
                        expr->array_element_record_type = elem_alias->inline_record_type;
                    else if (elem_alias->target_type_id != NULL || elem_alias->target_type_ref != NULL)
                    {
                        HashNode_t *target_node = semcheck_find_preferred_type_node_with_ref(
                            symtab, elem_alias->target_type_ref, elem_alias->target_type_id);
                        if (target_node == NULL)
                            target_node = semcheck_find_type_node_with_kgpc_type_ref(
                                symtab, elem_alias->target_type_ref, elem_alias->target_type_id);
                        if (target_node != NULL)
                        {
                            expr->array_element_record_type = get_record_type_from_node(target_node);
                            if (expr->array_element_record_type == NULL &&
                                target_node->type != NULL &&
                                kgpc_type_is_record(target_node->type))
                            {
                                expr->array_element_record_type =
                                    kgpc_type_get_record(target_node->type);
                            }
                        }
                    }
                }
            }
            if (expr->array_element_record_type == NULL && array_element_id != NULL)
                expr->array_element_record_type = semcheck_lookup_record_type(symtab,
                    array_element_id);
            if (expr->array_element_record_type != NULL &&
                expr->array_element_type == UNKNOWN_TYPE)
            {
                expr->array_element_type = RECORD_TYPE;
            }
        }
        else if (expr->array_element_type == RECORD_TYPE)
        {
            expr->array_element_record_type = field_desc->array_element_record;
            if (expr->array_element_record_type == NULL)
                expr->array_element_record_type = field_record;
        }

        long long computed_size = 0;
        int size_status = 1;
        if (expr->array_element_record_type != NULL)
            size_status = sizeof_from_record(symtab, expr->array_element_record_type,
                &computed_size, 0, expr->line_num);
        else if (expr->array_element_type != UNKNOWN_TYPE ||
            expr->array_element_type_id != NULL ||
            expr->array_element_type_ref != NULL)
        {
            char *rendered_elem = NULL;
            const char *sizeof_id = expr->array_element_type_id;
            if (sizeof_id == NULL && expr->array_element_type_ref != NULL)
            {
                rendered_elem = type_ref_render_mangled(expr->array_element_type_ref);
                sizeof_id = rendered_elem;
            }
            size_status = sizeof_from_type_ref(symtab, expr->array_element_type,
                sizeof_id, &computed_size, 0, expr->line_num);
            if (rendered_elem != NULL)
                free(rendered_elem);
        }
        if (size_status == 0 && computed_size > 0 && computed_size <= INT_MAX)
            expr->array_element_size = (int)computed_size;

        if (expr->array_element_type != UNKNOWN_TYPE &&
            field_desc->type != SHORTSTRING_TYPE)
            field_type = expr->array_element_type;
        if (expr->array_element_record_type != NULL && field_type == RECORD_TYPE)
            field_record = expr->array_element_record_type;
        if (expr->resolved_kgpc_type == NULL &&
            field_desc->type != SHORTSTRING_TYPE)
        {
            KgpcType *elem_type = NULL;
            int elem_owned = 0;
            int elem_type_tag_hint = expr->array_element_type;
            if (field_desc->array_element_kgpc_type != NULL)
            {
                elem_type = field_desc->array_element_kgpc_type;
                elem_owned = 0;
            }
            else if (expr->array_element_type_ref != NULL || expr->array_element_type_id != NULL)
            {
                HashNode_t *elem_node = semcheck_find_preferred_type_node_with_ref(
                    symtab, expr->array_element_type_ref, expr->array_element_type_id);
                if (elem_node == NULL)
                    elem_node = semcheck_find_type_node_with_kgpc_type_ref(
                        symtab, expr->array_element_type_ref, expr->array_element_type_id);
                if (elem_node != NULL && elem_node->type != NULL)
                {
                    elem_type = semcheck_create_value_kgpc_type_from_node_local(elem_node);
                    elem_owned = 1;
                }
            }
            else if (elem_type_tag_hint == RECORD_TYPE && expr->array_element_record_type != NULL)
            {
                elem_type = semcheck_create_value_kgpc_type_for_record_local(
                    expr->array_element_record_type);
                elem_owned = 1;
            }
            else if (elem_type_tag_hint == RECORD_TYPE && expr->array_element_type_id != NULL)
            {
                struct RecordType *elem_record = semcheck_lookup_record_type(
                    symtab, expr->array_element_type_id);
                if (elem_record != NULL)
                {
                    elem_type = semcheck_create_value_kgpc_type_for_record_local(elem_record);
                    elem_owned = 1;
                }
            }
            if (elem_type == NULL && expr->array_element_type != UNKNOWN_TYPE)
            {
                elem_type = create_primitive_type(expr->array_element_type);
                elem_owned = 1;
            }
            if (elem_type != NULL)
            {
                if (kgpc_getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL && field_id != NULL &&
                    pascal_identifier_equals(field_id, "FArr"))
                {
                    fprintf(stderr,
                        "[KGPC_DEBUG_RECORD_ACCESS] build-array field=%s elem_type=%s kind=%d elem_owned=%d elem_record=%p array_elem_record=%p array_elem_type_id=%s\n",
                        field_id,
                        kgpc_type_to_string(elem_type),
                        elem_type->kind,
                        elem_owned,
                        elem_type->kind == TYPE_KIND_RECORD ? (void *)elem_type->info.record_info : NULL,
                        (void *)expr->array_element_record_type,
                        expr->array_element_type_id ? expr->array_element_type_id : "<null>");
                }
                KgpcType *arr_type = create_array_type(elem_type,
                    expr->array_lower_bound, expr->array_upper_bound);
                if (arr_type != NULL)
                {
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, arr_type);
                    destroy_kgpc_type(arr_type);
                }
                if (elem_owned)
                    destroy_kgpc_type(elem_type);
            }
        }
    }

    struct TypeAlias *array_alias = NULL;
    int field_is_array_type = field_desc->is_array ? 1 : 0;

    if (!field_desc->is_pointer && (field_desc->type_id != NULL || field_desc->type_ref != NULL))
    {
        int resolved_type = field_type;
        const TypeRef *field_ref = field_desc->type_ref;
        const char *type_id_to_use = field_desc->type_id;
        char qualified_name_buf[512];
        int qualified_name_allocated = 0;
        int needs_type_id_resolution = (resolved_type != SHORTSTRING_TYPE);
        
        HashNode_t *type_node = semcheck_find_preferred_type_node_with_ref(symtab,
            field_ref, type_id_to_use);
        if (type_node == NULL)
            type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                field_ref, type_id_to_use);

        if (type_node == NULL && record_info != NULL && record_info->type_id != NULL)
        {
            snprintf(qualified_name_buf, sizeof(qualified_name_buf), "%s.%s",
                     record_info->type_id, field_desc->type_id);
            type_node = semcheck_find_preferred_type_node(symtab, qualified_name_buf);
            if (type_node == NULL)
                type_node = semcheck_find_type_node_with_kgpc_type(symtab, qualified_name_buf);
            if (type_node != NULL)
            {
                type_id_to_use = qualified_name_buf;
                qualified_name_allocated = 1;
            }
        }
        /* Fallback: suffix search for nested types (e.g., TPtrWrapper → TMarshal.TPtrWrapper) */
        if (type_node == NULL && field_desc->type_id != NULL)
        {
            for (ScopeNode *cur = symtab->current_scope; cur != NULL && type_node == NULL; cur = cur->parent)
                type_node = FindTypeBySuffixInTable(cur->table, field_desc->type_id);
            if (type_node == NULL)
            {
                for (int u = 0; u < SYMTAB_MAX_UNITS; u++)
                {
                    if (symtab->unit_scopes[u] != NULL && symtab->unit_scopes[u]->table != NULL)
                    {
                        type_node = FindTypeBySuffixInTable(symtab->unit_scopes[u]->table, field_desc->type_id);
                        if (type_node != NULL)
                            break;
                    }
                }
            }
        }
        
        if (needs_type_id_resolution)
        {
            if (resolve_type_identifier_ref(&resolved_type, symtab, type_id_to_use,
                    field_ref, expr->line_num) != 0)
                ++error_count;
        }
        field_type = resolved_type;
        
        if (qualified_name_allocated && field_desc->type_id != NULL)
        {
            char *field_type_copy = strdup(field_desc->type_id);
            free(field_desc->type_id);
            field_desc->type_id = strdup(qualified_name_buf);
            if (field_desc->type_ref != NULL)
            {
                type_ref_free(field_desc->type_ref);
                field_desc->type_ref = NULL;
            }
            char **segments = (char **)calloc(2, sizeof(char *));
            if (segments != NULL)
            {
                segments[0] = strdup(record_info->type_id);
                segments[1] = field_type_copy;
                if (segments[0] != NULL && segments[1] != NULL)
                {
                    QualifiedIdent *qid = qualified_ident_from_segments(segments, 2, 1);
                    if (qid != NULL)
                        field_desc->type_ref = type_ref_create(qid, NULL, 0);
                }
                else
                {
                    free(segments[0]);
                    free(field_type_copy);
                    free(segments);
                }
            }
            else
            {
                free(field_type_copy);
            }
        }

        if (type_node != NULL)
        {
            struct TypeAlias *alias = get_type_alias_from_node(type_node);
            if (expr->resolved_kgpc_type == NULL)
            {
                if (type_node->type != NULL)
                {
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, type_node->type);
                }
                else if (alias != NULL)
                {
                    KgpcType *alias_type = alias->kgpc_type;
                    if (alias_type == NULL)
                        alias_type = create_kgpc_type_from_type_alias(alias, symtab, 0);
                    if (alias_type != NULL)
                    {
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, alias_type);
                        if (alias_type != alias->kgpc_type)
                            destroy_kgpc_type(alias_type);
                    }
                }
            }
            if (type_node->type != NULL &&
                (kgpc_type_is_array(type_node->type) || kgpc_type_is_array_of_const(type_node->type)) &&
                !kgpc_type_is_shortstring(type_node->type) &&
                !expr->is_array_expr)
            {
                semcheck_set_array_info_from_hashnode(expr, symtab, type_node, expr->line_num);
            }
            if (type_node->type != NULL &&
                (kgpc_type_is_array(type_node->type) || kgpc_type_is_array_of_const(type_node->type)) &&
                !kgpc_type_is_shortstring(type_node->type))
            {
                field_is_array_type = 1;
            }

            struct RecordType *record_type = get_record_type_from_node(type_node);
            if (record_type != NULL)
                field_record = record_type;
            else
            {
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->target_type_id != NULL)
                {
                    HashNode_t *target_node =
                        semcheck_find_preferred_type_node_with_ref(symtab,
                            alias->target_type_ref, alias->target_type_id);
                    if (target_node != NULL)
                        field_record = get_record_type_from_node(target_node);
                }
            }

            if (alias != NULL && alias->is_array &&
                !(expr->resolved_kgpc_type != NULL && kgpc_type_is_shortstring(expr->resolved_kgpc_type)))
            {
                array_alias = alias;
                field_is_array_type = 1;
            }
        }

        if (field_record == NULL && field_desc->type_id != NULL)
        {
            field_record = semcheck_lookup_record_type(symtab, field_desc->type_id);
        }

        if (field_record != NULL && field_type == UNKNOWN_TYPE)
            field_type = RECORD_TYPE;
        if (field_is_array_type && field_desc->type != SHORTSTRING_TYPE)
            field_type = UNKNOWN_TYPE;
        if (field_type == SHORTSTRING_TYPE)
        {
            if (expr->resolved_kgpc_type != NULL &&
                !kgpc_type_is_shortstring(expr->resolved_kgpc_type))
            {
                destroy_kgpc_type(expr->resolved_kgpc_type);
                expr->resolved_kgpc_type = NULL;
            }
            field_is_array_type = 0;
            array_alias = NULL;
            KgpcType *primitive_type = create_primitive_type(field_type);
            if (primitive_type != NULL)
            {
                semcheck_expr_set_resolved_kgpc_type_shared(expr, primitive_type);
                destroy_kgpc_type(primitive_type);
            }
        }
    }

    if (kgpc_getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL && field_id != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_ACCESS] resolved field=%s type=%d type_id=%s record=%p\n",
            field_id,
            field_type,
            field_desc->type_id ? field_desc->type_id : "<null>",
            (void *)field_record);
    }

    if (field_record != NULL && field_type == RECORD_TYPE)
    {
        expr->record_type = field_record;
        if (expr->resolved_kgpc_type == NULL)
        {
            KgpcType *record_kgpc = semcheck_create_value_kgpc_type_for_record_local(field_record);
            if (record_kgpc != NULL)
            {
                semcheck_expr_set_resolved_kgpc_type_shared(expr, record_kgpc);
                destroy_kgpc_type(record_kgpc);
            }
        }
    }

    if (field_type == UNKNOWN_TYPE && field_record == NULL && array_alias == NULL && !field_is_array_type)
    {
        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, unable to resolve type for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    if (field_type == RECORD_TYPE && field_record == NULL && !field_is_array_type)
    {
        semcheck_error_with_context_at(expr->line_num, expr->col_num, expr->source_index, "Error on line %d, missing record definition for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    if (array_alias != NULL)
        semcheck_set_array_info_from_alias(expr, symtab, array_alias, expr->line_num);
    if (array_alias != NULL && expr->resolved_kgpc_type == NULL)
    {
        KgpcType *arr_type = create_kgpc_type_from_type_alias(array_alias, symtab, 0);
        if (arr_type != NULL)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, arr_type);
            destroy_kgpc_type(arr_type);
        }
    }

    if (expr->resolved_kgpc_type == NULL &&
        field_record == NULL &&
        !field_desc->is_array &&
        !field_desc->is_pointer &&
        field_desc->proc_type == NULL)
    {
        KgpcType *fallback_type = NULL;
        int fallback_owned = 0;
        if (field_desc->type_id != NULL || field_desc->type_ref != NULL)
        {
            HashNode_t *type_node = semcheck_find_preferred_type_node_with_ref(
                symtab, field_desc->type_ref, field_desc->type_id);
            if (type_node != NULL && type_node->type != NULL)
                fallback_type = type_node->type;
        }
        if (fallback_type == NULL && field_type != UNKNOWN_TYPE)
        {
            if (field_type == REAL_TYPE)
                fallback_type = create_primitive_type_with_size(REAL_TYPE, 8);
            else
                fallback_type = create_primitive_type(field_type);
            fallback_owned = (fallback_type != NULL);
        }
        if (fallback_type != NULL)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, fallback_type);
            if (fallback_owned)
                destroy_kgpc_type(fallback_type);
        }
    }

    if (expr->resolved_kgpc_type == NULL && field_desc->is_array)
    {
        KgpcType *elem_type = NULL;
        int elem_owned = 0;
        int elem_type_tag_hint = field_desc->array_element_type;
        if (elem_type_tag_hint == UNKNOWN_TYPE)
            elem_type_tag_hint = expr->array_element_type;
        if (field_desc->array_element_kgpc_type != NULL)
        {
            /* Pre-built element type for nested arrays (array of array of ...) */
            elem_type = field_desc->array_element_kgpc_type;
            elem_owned = 0;  /* Borrowed from field descriptor */
        }
        else if (field_desc->array_element_type_id != NULL)
        {
            HashNode_t *elem_node = semcheck_find_preferred_type_node_with_ref(
                symtab, field_desc->array_element_type_ref, field_desc->array_element_type_id);
            if (elem_node == NULL)
                elem_node = semcheck_find_type_node_with_kgpc_type_ref(
                    symtab, field_desc->array_element_type_ref, field_desc->array_element_type_id);
            if (elem_node != NULL && elem_node->type != NULL)
            {
                elem_type = semcheck_create_value_kgpc_type_from_node_local(elem_node);
                elem_owned = 1;
            }
        }
        else if (elem_type_tag_hint == RECORD_TYPE && field_desc->array_element_record != NULL)
        {
            elem_type = semcheck_create_value_kgpc_type_for_record_local(
                field_desc->array_element_record);
            elem_owned = 1;
        }
        else if (elem_type_tag_hint == RECORD_TYPE && field_desc->array_element_type_id != NULL)
        {
            struct RecordType *elem_record = semcheck_lookup_record_type(
                symtab, field_desc->array_element_type_id);
            if (elem_record != NULL)
            {
                elem_type = semcheck_create_value_kgpc_type_for_record_local(elem_record);
                elem_owned = 1;
            }
        }
        if (elem_type == NULL && field_desc->array_element_type != UNKNOWN_TYPE)
        {
            elem_type = create_primitive_type(field_desc->array_element_type);
            elem_owned = 1;
        }
        if (elem_type == NULL && expr->array_element_type != UNKNOWN_TYPE)
        {
            elem_type = create_primitive_type(expr->array_element_type);
            elem_owned = 1;
        }
        if (elem_type == NULL && elem_type_tag_hint == RECORD_TYPE &&
            expr->array_element_record_type != NULL)
        {
            elem_type = semcheck_create_value_kgpc_type_for_record_local(
                expr->array_element_record_type);
            elem_owned = 1;
        }
        if (elem_type == NULL && expr->array_element_type_id != NULL)
        {
            HashNode_t *elem_node = semcheck_find_preferred_type_node_with_ref(
                symtab, expr->array_element_type_ref, expr->array_element_type_id);
            if (elem_node == NULL)
                elem_node = semcheck_find_type_node_with_kgpc_type_ref(
                    symtab, expr->array_element_type_ref, expr->array_element_type_id);
            if (elem_node != NULL && elem_node->type != NULL)
            {
                elem_type = semcheck_create_value_kgpc_type_from_node_local(elem_node);
                elem_owned = 1;
            }
        }
        if (elem_type != NULL)
        {
            if (!elem_owned)
                kgpc_type_retain(elem_type);
            KgpcType *arr_type = create_array_type(elem_type,
                field_desc->array_start, field_desc->array_end);
            if (arr_type != NULL)
                semcheck_expr_set_resolved_kgpc_type_shared(expr, arr_type);
            if (arr_type != NULL)
                destroy_kgpc_type(arr_type);
            /* Note: elem_type ownership was transferred to create_array_type, do NOT destroy it here */
        }
    }

    if (field_type == POINTER_TYPE &&
        !field_desc->is_array &&
        !(expr->resolved_kgpc_type != NULL &&
          (kgpc_type_is_array(expr->resolved_kgpc_type) ||
           kgpc_type_is_array_of_const(expr->resolved_kgpc_type))) &&
        !field_desc->is_pointer &&
        field_desc->pointer_type_id == NULL &&
        field_desc->pointer_type == UNKNOWN_TYPE)
    {
        HashNode_t *ptr_node = NULL;
        if (field_desc->type_id != NULL || field_desc->type_ref != NULL)
        {
            ptr_node = semcheck_find_preferred_type_node_with_ref(symtab,
                field_desc->type_ref, field_desc->type_id);
            if (ptr_node == NULL)
                ptr_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                    field_desc->type_ref, field_desc->type_id);
        }
        if (ptr_node != NULL && ptr_node->type != NULL)
        {
            KgpcType *ptr_type = ptr_node->type;
            int is_untyped_pointer = 0;
            if (ptr_type->kind == TYPE_KIND_PRIMITIVE &&
                ptr_type->info.primitive_type_tag == POINTER_TYPE)
            {
                is_untyped_pointer = 1;
            }
            else if (ptr_type->kind == TYPE_KIND_POINTER)
            {
                KgpcType *pointee = ptr_type->info.points_to;
                if (pointee != NULL &&
                    pointee->kind == TYPE_KIND_PRIMITIVE &&
                    pointee->info.primitive_type_tag == POINTER_TYPE)
                {
                    struct TypeAlias *ptr_alias = get_type_alias_from_node(ptr_node);
                    if (ptr_alias == NULL || !ptr_alias->is_pointer)
                        is_untyped_pointer = 1;
                }
            }
            if (is_untyped_pointer)
            {
                semcheck_set_pointer_info(expr, UNKNOWN_TYPE, NULL);
                if (expr->resolved_kgpc_type != NULL)
                {
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                    expr->resolved_kgpc_type = NULL;
                }
                expr->resolved_kgpc_type = create_primitive_type(POINTER_TYPE);
                *type_return = POINTER_TYPE;
                return error_count;
            }
        }
    }

    if (field_type == POINTER_TYPE &&
        !field_desc->is_array &&
        !(expr->resolved_kgpc_type != NULL &&
          (kgpc_type_is_array(expr->resolved_kgpc_type) ||
           kgpc_type_is_array_of_const(expr->resolved_kgpc_type))))
    {
        int pointer_subtype = UNKNOWN_TYPE;
        const char *pointer_subtype_id = NULL;
        char *pointer_subtype_rendered = NULL;
        int typed_pointer = 0;
        int untyped_pointer_type = 0;
        int force_untyped = 0;
        HashNode_t *type_node = NULL;
        struct TypeAlias *alias = NULL;

        if (field_desc->is_pointer)
            typed_pointer = 1;
        if (field_desc->pointer_type_id != NULL || field_desc->pointer_type != UNKNOWN_TYPE ||
            field_desc->pointer_type_ref != NULL)
            typed_pointer = 1;

        if (!typed_pointer && (field_desc->type_id != NULL || field_desc->type_ref != NULL))
        {
            type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                field_desc->type_ref, field_desc->type_id);
            if (type_node == NULL)
                type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                    field_desc->type_ref, field_desc->type_id);
            if (type_node != NULL)
            {
                alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->is_pointer)
                    typed_pointer = 1;
                if (type_node->type != NULL)
                {
                    if (type_node->type->kind == TYPE_KIND_PRIMITIVE &&
                        type_node->type->info.primitive_type_tag == POINTER_TYPE)
                    {
                        untyped_pointer_type = 1;
                    }
                    else if (type_node->type->kind == TYPE_KIND_POINTER)
                    {
                        KgpcType *points_to = type_node->type->info.points_to;
                        if (points_to != NULL &&
                            points_to->kind == TYPE_KIND_PRIMITIVE &&
                            points_to->info.primitive_type_tag == POINTER_TYPE)
                        {
                            untyped_pointer_type = 1;
                        }
                        else
                        {
                            typed_pointer = 1;
                        }
                    }
                }
                if (untyped_pointer_type && (alias == NULL || !alias->is_pointer))
                    typed_pointer = 0;
            }
        }

        if (!typed_pointer)
        {
            semcheck_set_pointer_info(expr, UNKNOWN_TYPE, NULL);
            if (expr->resolved_kgpc_type == NULL ||
                !(expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                  expr->resolved_kgpc_type->info.primitive_type_tag == POINTER_TYPE))
            {
                if (expr->resolved_kgpc_type != NULL)
                {
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                    expr->resolved_kgpc_type = NULL;
                }
                expr->resolved_kgpc_type = create_primitive_type(POINTER_TYPE);
            }
        }
        else
        {
            if (field_desc->pointer_type_id != NULL || field_desc->pointer_type != UNKNOWN_TYPE)
            {
                if (field_desc->pointer_type != UNKNOWN_TYPE)
                    pointer_subtype = field_desc->pointer_type;
                if (field_desc->pointer_type_id != NULL)
                    pointer_subtype_id = field_desc->pointer_type_id;
                else if (field_desc->pointer_type_ref != NULL)
                {
                    pointer_subtype_rendered = type_ref_render_mangled(field_desc->pointer_type_ref);
                    pointer_subtype_id = pointer_subtype_rendered;
                }
            }
            else if (field_desc->type_id != NULL || field_desc->type_ref != NULL)
            {
                if (type_node == NULL)
                {
                    type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                        field_desc->type_ref, field_desc->type_id);
                    if (type_node == NULL)
                        type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                            field_desc->type_ref, field_desc->type_id);
                }
                if (type_node != NULL)
                {
                    struct TypeAlias *alias_local = get_type_alias_from_node(type_node);
                    if (alias_local != NULL && alias_local->is_pointer)
                    {
                        pointer_subtype = alias_local->pointer_type;
                        if (alias_local->pointer_type_ref != NULL)
                        {
                            pointer_subtype_rendered = type_ref_render_mangled(alias_local->pointer_type_ref);
                            pointer_subtype_id = pointer_subtype_rendered;
                        }
                        else
                            pointer_subtype_id = alias_local->pointer_type_id;
                    }
                    if (pointer_subtype == UNKNOWN_TYPE && type_node->type != NULL)
                        pointer_subtype = kgpc_type_get_pointer_subtype_tag(type_node->type);
                }
            }
            if (pointer_subtype_id == NULL)
            {
                if (field_desc->type_id != NULL)
                {
                    if (!untyped_pointer_type)
                        pointer_subtype_id = field_desc->type_id;
                }
                else if (field_desc->type_ref != NULL)
                    pointer_subtype_id = type_ref_base_name(field_desc->type_ref);
            }
            if (!force_untyped && pointer_subtype_id != NULL)
            {
                HashNode_t *sub_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                    field_desc->pointer_type_ref, pointer_subtype_id);
                if (sub_node != NULL && sub_node->type != NULL)
                {
                    if (sub_node->type->kind == TYPE_KIND_PRIMITIVE &&
                        sub_node->type->info.primitive_type_tag == POINTER_TYPE)
                    {
                        force_untyped = 1;
                    }
                    else if (sub_node->type->kind == TYPE_KIND_POINTER)
                    {
                        KgpcType *points_to = sub_node->type->info.points_to;
                        if (points_to != NULL &&
                            points_to->kind == TYPE_KIND_PRIMITIVE &&
                            points_to->info.primitive_type_tag == POINTER_TYPE)
                        {
                            struct TypeAlias *sub_alias = get_type_alias_from_node(sub_node);
                            if (sub_alias == NULL || !sub_alias->is_pointer)
                                force_untyped = 1;
                        }
                    }
                }
            }
            if (pointer_subtype == POINTER_TYPE &&
                field_desc->is_pointer == 0 &&
                field_desc->pointer_type_id == NULL &&
                field_desc->pointer_type == UNKNOWN_TYPE &&
                (alias == NULL || !alias->is_pointer))
            {
                force_untyped = 1;
            }

            if (force_untyped)
            {
                semcheck_set_pointer_info(expr, UNKNOWN_TYPE, NULL);
                if (pointer_subtype_rendered != NULL)
                    free(pointer_subtype_rendered);
                if (expr->resolved_kgpc_type == NULL ||
                    !(expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                      expr->resolved_kgpc_type->info.primitive_type_tag == POINTER_TYPE))
                {
                    if (expr->resolved_kgpc_type != NULL)
                    {
                        destroy_kgpc_type(expr->resolved_kgpc_type);
                        expr->resolved_kgpc_type = NULL;
                    }
                    expr->resolved_kgpc_type = create_primitive_type(POINTER_TYPE);
                }
            }
            else
            {
                semcheck_set_pointer_info(expr, pointer_subtype, pointer_subtype_id);
                if (pointer_subtype_rendered != NULL)
                    free(pointer_subtype_rendered);
                if (expr->resolved_kgpc_type != NULL &&
                    expr->resolved_kgpc_type->kind != TYPE_KIND_POINTER)
                {
                    destroy_kgpc_type(expr->resolved_kgpc_type);
                    expr->resolved_kgpc_type = NULL;
                }
                if (expr->resolved_kgpc_type == NULL)
                {
                    KgpcType *points_to = NULL;
                    if (pointer_subtype_id != NULL)
                    {
                        HashNode_t *target_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                            field_desc->pointer_type_ref, pointer_subtype_id);
                        if (target_node != NULL && target_node->type != NULL)
                        {
                            points_to = target_node->type;
                            kgpc_type_retain(points_to);
                        }
                    }
                    if (points_to == NULL && pointer_subtype != UNKNOWN_TYPE)
                        points_to = create_primitive_type(pointer_subtype);
                    KgpcType *ptr_type = create_pointer_type(points_to);
                    if (points_to != NULL)
                        kgpc_type_release(points_to);
                    if (ptr_type != NULL)
                    {
                        semcheck_expr_set_resolved_kgpc_type_shared(expr, ptr_type);
                        destroy_kgpc_type(ptr_type);
                    }
                }

                struct RecordType *pointer_record = NULL;
                if (expr->resolved_kgpc_type != NULL &&
                    expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER)
                {
                    KgpcType *points_to = expr->resolved_kgpc_type->info.points_to;
                    if (points_to != NULL && points_to->kind == TYPE_KIND_RECORD)
                        pointer_record = points_to->info.record_info;
                }
                if (pointer_record == NULL && pointer_subtype_id != NULL)
                    pointer_record = semcheck_lookup_record_type(symtab, pointer_subtype_id);
                (void)pointer_record;
            }
        }
    }
    if (kgpc_getenv("KGPC_DEBUG_BUFPTR") != NULL &&
        field_id != NULL && pascal_identifier_equals(field_id, "bufptr"))
    {
        fprintf(stderr, "[KGPC_DEBUG_BUFPTR] field=%s type=%d resolved=%s kind=%d\n",
            field_id,
            field_type,
            expr->resolved_kgpc_type ? kgpc_type_to_string(expr->resolved_kgpc_type) : "<null>",
            expr->resolved_kgpc_type ? expr->resolved_kgpc_type->kind : -1);
    }

    /* For procedural type fields (function/procedure pointers), set the full KgpcType
     * so type compatibility can check return types and parameters. */
    if (field_type == PROCEDURE && (field_desc->type_id != NULL || field_desc->type_ref != NULL))
    {
        HashNode_t *proc_type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
            field_desc->type_ref, field_desc->type_id);
        if (proc_type_node != NULL && proc_type_node->type != NULL &&
            proc_type_node->type->kind == TYPE_KIND_PROCEDURE)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, proc_type_node->type);
        }
        else if (field_desc->proc_type != NULL &&
                 field_desc->proc_type->kind == TYPE_KIND_PROCEDURE)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, field_desc->proc_type);
        }
    }
    else if (field_type == PROCEDURE && field_desc->proc_type != NULL)
    {
        semcheck_expr_set_resolved_kgpc_type_shared(expr, field_desc->proc_type);
    }

    if (field_type == REAL_TYPE)
    {
        int desired_real_size = 0;
        HashNode_t *real_node = NULL;
        if (field_desc->type_id != NULL || field_desc->type_ref != NULL)
        {
            real_node = semcheck_find_preferred_type_node_with_ref(symtab,
                field_desc->type_ref, field_desc->type_id);
            if (real_node == NULL)
                real_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
                    field_desc->type_ref, field_desc->type_id);
            if (real_node != NULL && real_node->type != NULL)
            {
                long long size = kgpc_type_sizeof(real_node->type);
                if (size == 4 || size == 8)
                    desired_real_size = (int)size;
            }
        }
        if (desired_real_size == 0)
            desired_real_size = 8;

        int existing_real_size = 0;
        if (expr->resolved_kgpc_type != NULL &&
            expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
            expr->resolved_kgpc_type->info.primitive_type_tag == REAL_TYPE)
        {
            long long size = kgpc_type_sizeof(expr->resolved_kgpc_type);
            if (size > 0)
                existing_real_size = (int)size;
        }

        if (expr->resolved_kgpc_type == NULL ||
            (existing_real_size != 0 && existing_real_size != desired_real_size) ||
            (existing_real_size == 0))
        {
            if (real_node != NULL && real_node->type != NULL &&
                real_node->type->kind == TYPE_KIND_PRIMITIVE &&
                real_node->type->info.primitive_type_tag == REAL_TYPE)
            {
                semcheck_expr_set_resolved_kgpc_type_shared(expr, real_node->type);
            }
            else
            {
                KgpcType *real_type = create_primitive_type_with_size(REAL_TYPE, desired_real_size);
                if (real_type != NULL)
                {
                    semcheck_expr_set_resolved_kgpc_type_shared(expr, real_type);
                    destroy_kgpc_type(real_type);
                }
            }
        }
    }
    else if (expr->resolved_kgpc_type == NULL &&
        field_type != UNKNOWN_TYPE &&
        field_type != RECORD_TYPE &&
        field_type != PROCEDURE)
    {
        KgpcType *prim_type = create_primitive_type(field_type);
        if (prim_type != NULL)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, prim_type);
            destroy_kgpc_type(prim_type);
        }
    }

    if (field_type == RECORD_TYPE && expr->resolved_kgpc_type == NULL && field_record != NULL)
    {
        KgpcType *record_kgpc = semcheck_create_value_kgpc_type_for_record_local(field_record);
        if (record_kgpc != NULL)
        {
            semcheck_expr_set_resolved_kgpc_type_shared(expr, record_kgpc);
            destroy_kgpc_type(record_kgpc);
        }
    }
    if (field_desc->type == SHORTSTRING_TYPE)
    {
        field_type = SHORTSTRING_TYPE;
        if (expr->resolved_kgpc_type != NULL &&
            !kgpc_type_is_shortstring(expr->resolved_kgpc_type))
        {
            destroy_kgpc_type(expr->resolved_kgpc_type);
            expr->resolved_kgpc_type = NULL;
        }
        if (expr->resolved_kgpc_type == NULL)
        {
            KgpcType *short_type = create_primitive_type(SHORTSTRING_TYPE);
            if (short_type != NULL)
            {
                semcheck_expr_set_resolved_kgpc_type_shared(expr, short_type);
                destroy_kgpc_type(short_type);
            }
        }
    }
    int preserve_resolved_kgpc = 0;
    if (expr->resolved_kgpc_type != NULL)
    {
        if (kgpc_type_is_array(expr->resolved_kgpc_type) ||
            kgpc_type_is_array_of_const(expr->resolved_kgpc_type) ||
            kgpc_type_is_procedure(expr->resolved_kgpc_type))
        {
            /* Preserve array/procedure KgpcType for overload resolution and var/open-array behavior. */
            preserve_resolved_kgpc = 1;
        }
        else if (kgpc_type_equals_tag(expr->resolved_kgpc_type, field_type))
        {
            /* Keep richer alias metadata (e.g. Single vs generic Real) when the type tag already matches. */
            preserve_resolved_kgpc = 1;
        }
    }

    if (!preserve_resolved_kgpc)
        semcheck_expr_set_resolved_type(expr, field_type);

    /* When the field is a class/pointer type with a known type_id (e.g. TInner),
     * set pointer_subtype_id AFTER semcheck_expr_set_resolved_type so it isn't
     * overwritten. This allows chained field access (obj.classField.innerField)
     * to resolve the intermediate class type. */
    if (field_type == POINTER_TYPE && field_desc != NULL &&
        field_desc->type_id != NULL && expr->pointer_subtype_id == NULL)
    {
        semcheck_set_pointer_info(expr, POINTER_TYPE, field_desc->type_id);
    }

    *type_return = field_type;
    if (kgpc_getenv("KGPC_DEBUG_RECORD_ACCESS") != NULL && field_id != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_ACCESS] final field=%s type=%d resolved=%s\n",
            field_id,
            field_type,
            expr->resolved_kgpc_type ? kgpc_type_to_string(expr->resolved_kgpc_type) : "<null>");
    }
    if (kgpc_getenv("KGPC_DEBUG_RECORD_FIELD") != NULL &&
        field_id != NULL &&
        (pascal_identifier_equals(field_id, "st_ctime") ||
         pascal_identifier_equals(field_id, "st_mtime") ||
         pascal_identifier_equals(field_id, "st_atime")))
    {
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_FIELD] field=%s resolved_type=%d\n",
            field_id, field_type);
    }
    return error_count;
}
