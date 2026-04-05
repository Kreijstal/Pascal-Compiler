void semcheck_mark_static_link_needed(HashNode_t *node)
{
    if (g_semcheck_current_subprogram == NULL || node == NULL)
        return;
    
    /* Only set requires_static_link if the function is actually nested (nesting_level > 1).
     * Top-level functions (nesting_level == 1) that access global variables do NOT need
     * a static link because global variables are accessed via their static labels.
     * 
     * This check prevents the issue where a top-level function like SetBacking, which
     * accesses a global variable, would incorrectly get requires_static_link=1 and then
     * callers would fail trying to pass a static link. */
    int nesting_level = g_semcheck_current_subprogram->tree_data.subprogram_data.nesting_level;
    if (nesting_level <= 1)
        return;

    switch (node->hash_type)
    {
        case HASHTYPE_VAR:
        case HASHTYPE_ARRAY:
        case HASHTYPE_FUNCTION_RETURN:
        case HASHTYPE_CONST:
            g_semcheck_current_subprogram->tree_data.subprogram_data.requires_static_link = 1;
            break;
        default:
            break;
    }
}

void semcheck_mark_call_requires_static_link(HashNode_t *callee)
{
    if (callee == NULL)
        return;
    if (g_semcheck_current_subprogram == NULL)
        return;
    if (!hashnode_requires_static_link(callee))
        return;
    /* When calling a function that requires a static link, the caller needs to
     * be able to PASS a static link. Mark the caller as having nested children
     * that need links, not as requiring a static link itself.
     * The caller only RECEIVES a static link if it's nested or accesses outer vars. */
    g_semcheck_current_subprogram->tree_data.subprogram_data.has_nested_requiring_link = 1;
}

int semcheck_program(SymTab_t *symtab, Tree_t *tree);
int semcheck_unit(SymTab_t *symtab, Tree_t *tree);

int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num);
int semcheck_type_decls(SymTab_t *symtab, ListNode_t *type_decls);
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls);
int semcheck_const_decls(SymTab_t *symtab, ListNode_t *const_decls);

/* Collect typed const declarations from a var_declaration list.
 * If from_unit_only is true, only collect those with defined_in_unit=1.
 * If from_unit_only is false, only collect those with defined_in_unit=0. */
static ListNode_t *collect_typed_const_decls_filtered(SymTab_t *symtab, ListNode_t *decls, int from_unit_only)
{
    ListNode_t *head = NULL;
    ListNode_t *tail = NULL;
    ListNode_t *cur = decls;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;
            if ((tree->type == TREE_VAR_DECL &&
                tree->tree_data.var_decl_data.is_typed_const) ||
                (tree->type == TREE_ARR_DECL &&
                tree->tree_data.arr_decl_data.is_typed_const))
            {
                /* Filter by origin */
                int is_from_unit = (tree->type == TREE_VAR_DECL) ?
                    tree->tree_data.var_decl_data.defined_in_unit :
                    tree->tree_data.arr_decl_data.defined_in_unit;
                if ((from_unit_only && !is_from_unit) || (!from_unit_only && is_from_unit))
                {
                    cur = cur->next;
                    continue;
                }

                int allow = 1;
                const char *type_id = NULL;
                int type_tag = UNKNOWN_TYPE;
                if (tree->type == TREE_VAR_DECL)
                {
                    type_id = tree->tree_data.var_decl_data.type_id;
                    type_tag = tree->tree_data.var_decl_data.type;
                }
                else
                {
                    type_id = tree->tree_data.arr_decl_data.type_id;
                    type_tag = tree->tree_data.arr_decl_data.type;
                }

                if (type_id != NULL)
                {
                    HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_id);
                    if (type_node == NULL &&
                        semcheck_map_builtin_type_name_local(type_id) == UNKNOWN_TYPE)
                    {
                        allow = 0;
                    }
                }
                else if (type_tag == UNKNOWN_TYPE)
                {
                    if (tree->type == TREE_ARR_DECL &&
                        tree->tree_data.arr_decl_data.element_kgpc_type != NULL)
                    {
                        allow = 1;
                    }
                    else
                    {
                        allow = 0;
                    }
                }

                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && tree->tree_data.var_decl_data.ids != NULL)
                {
                    const char *first_id = NULL;
                    if (tree->type == TREE_VAR_DECL)
                        first_id = tree->tree_data.var_decl_data.ids->cur ?
                            (const char *)tree->tree_data.var_decl_data.ids->cur : "<null>";
                    else
                        first_id = tree->tree_data.arr_decl_data.ids->cur ?
                            (const char *)tree->tree_data.arr_decl_data.ids->cur : "<null>";
                    fprintf(stderr, "[SemCheck] collect_typed_const: id=%s type_id=%s type=%d allow=%d inline_alias=%p\n",
                        first_id, type_id ? type_id : "<null>",
                        type_tag, allow,
                        (tree->type == TREE_VAR_DECL) ?
                            (void*)tree->tree_data.var_decl_data.inline_type_alias : NULL);
                }

                if (allow)
                {
                    ListNode_t *node = CreateListNode(tree, LIST_TREE);
                    if (head == NULL)
                    {
                        head = node;
                        tail = node;
                    }
                    else
                    {
                        tail->next = node;
                        tail = node;
                    }
                }
            }
        }
        cur = cur->next;
    }
    return head;
}

/* Collect non-typed var/array declarations (skip typed consts). */
static ListNode_t *collect_non_typed_var_decls(ListNode_t *decls)
{
    ListNode_t *head = NULL;
    ListNode_t *tail = NULL;
    ListNode_t *cur = decls;

    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;
            int skip = 0;
            if (tree->type == TREE_VAR_DECL)
                skip = tree->tree_data.var_decl_data.is_typed_const;
            else if (tree->type == TREE_ARR_DECL)
                skip = tree->tree_data.arr_decl_data.is_typed_const;
            else
                skip = 1;

            if (!skip)
            {
                ListNode_t *node = CreateListNode(tree, LIST_TREE);
                if (head == NULL)
                    head = node;
                else
                    tail->next = node;
                tail = node;
            }
        }
        cur = cur->next;
    }

    return head;
}

static int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls);
static int predeclare_types(SymTab_t *symtab, ListNode_t *type_decls);
static int predeclare_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev, Tree_t *parent_subprogram);

int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev);
int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram);

/* Resolve the return type for a function declaration once so callers share the same KgpcType. */
HashNode_t *semcheck_find_type_node_with_kgpc_type_ref(SymTab_t *symtab,
    const TypeRef *type_ref, const char *type_id)
{
    if (symtab == NULL || (type_id == NULL && type_ref == NULL))
        return NULL;

    /* Prefer the generic-aware lookup so we can instantiate types like
     * TFPGListEnumerator$TMyRecord on demand for function return types.
     * However, do not return a same-unit UNKNOWN_TYPE placeholder — fall
     * through to FindAllIdents so we can pick up a resolved type from another
     * unit (e.g., system's TDateTime=Real over sysutils's stub
     * TDateTime=System.TDateTime that failed to resolve at predeclare time). */
    HashNode_t *preferred = semcheck_find_preferred_type_node_with_ref(symtab, type_ref, type_id);
    if (preferred != NULL && preferred->type != NULL &&
        !(preferred->type->kind == TYPE_KIND_PRIMITIVE &&
          preferred->type->info.primitive_type_tag == UNKNOWN_TYPE))
        return preferred;

    char *rendered = NULL;
    const char *lookup_id = type_id;
    if (type_ref != NULL)
    {
        rendered = type_ref_render_mangled(type_ref);
        if (rendered != NULL)
            lookup_id = rendered;
    }
    if (lookup_id == NULL)
    {
        free(rendered);
        return NULL;
    }

    HashNode_t *result = NULL;
    ListNode_t *all_nodes = FindAllIdents(symtab, lookup_id);
    ListNode_t *cur = all_nodes;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL)
        {
            if (node->type != NULL)
            {
                /* Prefer a fully resolved type over an UNKNOWN_TYPE placeholder. */
                if (node->type->kind != TYPE_KIND_PRIMITIVE ||
                    node->type->info.primitive_type_tag != UNKNOWN_TYPE)
                {
                    result = node;
                    break;
                }
                if (result == NULL)
                    result = node;
            }
            if (result == NULL)
                result = node;
        }
        cur = cur->next;
    }

    if (all_nodes != NULL)
        DestroyList(all_nodes);

    free(rendered);
    return result;
}

HashNode_t *semcheck_find_type_node_with_kgpc_type(SymTab_t *symtab, const char *type_id)
{
    return semcheck_find_type_node_with_kgpc_type_ref(symtab, NULL, type_id);
}

static KgpcType *build_function_return_type(Tree_t *subprogram, SymTab_t *symtab,
    int *error_count, int allow_undefined)
{
    KgpcType *builtin_return = NULL;
    if (subprogram == NULL || symtab == NULL)
        return NULL;

    /* TODO: Once the symbol table tracks placeholder types, this helper should
     * validate that any returned KgpcType has been fully resolved. */
    HashNode_t *type_node = NULL;
    if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
    {
        const char *owner_full = subprogram->tree_data.subprogram_data.owner_class_full;
        const char *owner_outer = subprogram->tree_data.subprogram_data.owner_class_outer;
        if (owner_full == NULL)
            owner_full = subprogram->tree_data.subprogram_data.owner_class;
        if (owner_full != NULL)
        {
            semcheck_maybe_qualify_nested_type(symtab, owner_full, owner_outer,
                &subprogram->tree_data.subprogram_data.return_type_id,
                &subprogram->tree_data.subprogram_data.return_type_ref);
        }
        type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
            subprogram->tree_data.subprogram_data.return_type_ref,
            subprogram->tree_data.subprogram_data.return_type_id);
        if (type_node == NULL)
        {
            const char *owner_full = subprogram->tree_data.subprogram_data.owner_class_full;
            const char *owner_outer = subprogram->tree_data.subprogram_data.owner_class_outer;
            type_node = semcheck_find_type_node_in_owner_chain(symtab,
                subprogram->tree_data.subprogram_data.return_type_id, owner_full, owner_outer);
        }
        if (type_node == NULL)
        {
            /* Before reporting error, check for builtin types not in symbol table */
            const char *type_id = subprogram->tree_data.subprogram_data.return_type_id;
            const TypeRef *type_ref = subprogram->tree_data.subprogram_data.return_type_ref;
            const char *type_base = type_ref != NULL ? type_ref_base_name(type_ref) : type_id;
            int builtin_type = semcheck_map_builtin_type_name_local(type_base);
            if (builtin_type == UNKNOWN_TYPE)
            {
                /* Check if this is a generic type parameter of the function */
                int is_generic_param = 0;
                for (int i = 0; i < subprogram->tree_data.subprogram_data.num_generic_type_params; i++) {
                    if (type_base != NULL &&
                        strcasecmp(type_base, subprogram->tree_data.subprogram_data.generic_type_params[i]) == 0) {
                        is_generic_param = 1;
                        break;
                    }
                }
                if (is_generic_param) {
                    builtin_return = create_primitive_type(POINTER_TYPE);
                } else if (!allow_undefined)
                {
                    semantic_error(subprogram->line_num, 0, "undefined type %s",
                        subprogram->tree_data.subprogram_data.return_type_id);
                    if (error_count != NULL)
                        ++(*error_count);
                }
            }
            else
            {
                /* Preserve SHORTSTRING_TYPE set during AST conversion under {$H-};
                 * semcheck_map_builtin_type_name_local maps bare "String" to
                 * STRING_TYPE, but the per-file directive already chose correctly. */
                if (subprogram->tree_data.subprogram_data.return_type == SHORTSTRING_TYPE &&
                    builtin_type == STRING_TYPE)
                {
                    builtin_return = create_primitive_type(SHORTSTRING_TYPE);
                }
                else
                {
                    subprogram->tree_data.subprogram_data.return_type = builtin_type;
                    builtin_return = create_primitive_type(builtin_type);
                }
            }
        }
    }

    if (builtin_return != NULL)
        return builtin_return;

    KgpcType *result = kgpc_type_build_function_return(
        subprogram->tree_data.subprogram_data.inline_return_type,
        type_node,
        subprogram->tree_data.subprogram_data.return_type,
        symtab);

    /* Under {$H-}, from_cparser sets return_type = SHORTSTRING_TYPE on
     * functions returning bare 'string'.  If the symbol table resolved
     * "String" to the system unit's AnsiString alias (STRING_TYPE),
     * override to SHORTSTRING so sret and value-type semantics apply. */
    if (subprogram->tree_data.subprogram_data.return_type == SHORTSTRING_TYPE &&
        result != NULL && result->kind == TYPE_KIND_PRIMITIVE &&
        kgpc_type_get_primitive_tag(result) == STRING_TYPE)
    {
        destroy_kgpc_type(result);
        result = create_primitive_type(SHORTSTRING_TYPE);
    }

    return result;
}

/* Forward declarations for type resolution helpers used in const evaluation. */
static HashNode_t *find_type_node_with_range_metadata(SymTab_t *symtab,
    const char *base_name, const struct TypeAlias *exclude_alias);
static HashNode_t *semcheck_find_exact_type_node_for_qid(SymTab_t *symtab,
    const QualifiedIdent *type_ref);
static const char *resolve_type_to_base_name(SymTab_t *symtab,
    const QualifiedIdent *type_id,
    const char *type_name);

static int is_real_type_name(SymTab_t *symtab, const char *type_name)
{
    if (type_name == NULL)
        return 0;

    const char *resolved = resolve_type_to_base_name(symtab, NULL, type_name);
    const char *name = (resolved != NULL) ? resolved : type_name;

    return (pascal_identifier_equals(name, "Real") ||
            pascal_identifier_equals(name, "Float") ||
            pascal_identifier_equals(name, "ValReal") ||
            pascal_identifier_equals(name, "Double") ||
            pascal_identifier_equals(name, "Single") ||
            pascal_identifier_equals(name, "Extended") ||
            pascal_identifier_equals(name, "Comp") ||
            pascal_identifier_equals(name, "Currency"));
}

/* Helper to check if an expression contains a real number literal or real constant */
static int expression_contains_real_literal_impl(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    if (expr->type == EXPR_RNUM)
        return 1;
    
    if (expr->type == EXPR_VAR_ID && symtab != NULL)
    {
        /* Check if this variable ID refers to a real constant */
        HashNode_t *node = NULL;
        if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 &&
            node != NULL && node->hash_type == HASHTYPE_CONST &&
            node->type != NULL && kgpc_type_is_real(node->type))
        {
            return 1;
        }
        return 0;
    }
    
    if (expr->type == EXPR_SIGN_TERM)
    {
        return expression_contains_real_literal_impl(symtab, expr->expr_data.sign_term);
    }

    if (expr->type == EXPR_TYPECAST)
    {
        const char *target_id = expr->expr_data.typecast_data.target_type_id;
        int target_type = expr->expr_data.typecast_data.target_type;
        if (is_real_family_type(target_type) || is_real_type_name(symtab, target_id))
            return 1;
        return 0;
    }

    if (expr->type == EXPR_FUNCTION_CALL)
    {
        const char *id = expr->expr_data.function_call_data.id;
        if (id != NULL && pascal_identifier_equals(id, "Trunc"))
            return 0;
        if (id != NULL && is_real_type_name(symtab, id))
            return 1;
        return 0;
    }

    if (expr->type == EXPR_ADDOP)
    {
        return expression_contains_real_literal_impl(symtab, expr->expr_data.addop_data.left_expr) ||
               expression_contains_real_literal_impl(symtab, expr->expr_data.addop_data.right_term);
    }
    
    if (expr->type == EXPR_MULOP)
    {
        /* Pascal '/' (SLASH) always produces a real result, even with integer operands */
        if (expr->expr_data.mulop_data.mulop_type == SLASH)
            return 1;
        return expression_contains_real_literal_impl(symtab, expr->expr_data.mulop_data.left_term) ||
               expression_contains_real_literal_impl(symtab, expr->expr_data.mulop_data.right_factor);
    }
    
    return 0;
}

/* Helper to check if an expression is a string expression */
static int expression_is_string(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    if (expr->type == EXPR_STRING || expr->type == EXPR_CHAR_CODE)
        return 1;

    if (expr->type == EXPR_VAR_ID && symtab != NULL && expr->expr_data.id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL)
        {
            if ((node->hash_type == HASHTYPE_CONST || node->is_typed_const) &&
                node->const_string_value != NULL)
                return 1;
            if (node->type != NULL && node->type->kind == TYPE_KIND_PRIMITIVE)
            {
                int tag = kgpc_type_get_primitive_tag(node->type);
                if (tag == STRING_TYPE || tag == SHORTSTRING_TYPE)
                    return 1;
            }
        }
    }
    
    if (expr->type == EXPR_RECORD_ACCESS && symtab != NULL &&
        expr->expr_data.record_access_data.field_id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, symtab, expr->expr_data.record_access_data.field_id) != 0 &&
            node != NULL)
        {
            if ((node->hash_type == HASHTYPE_CONST || node->is_typed_const) &&
                node->const_string_value != NULL)
                return 1;
            if (node->type != NULL && node->type->kind == TYPE_KIND_PRIMITIVE)
            {
                int tag = kgpc_type_get_primitive_tag(node->type);
                if (tag == STRING_TYPE || tag == SHORTSTRING_TYPE)
                    return 1;
            }
        }
    }

    if (expr->type == EXPR_ADDOP && expr->expr_data.addop_data.addop_type == PLUS)
    {
        /* String concatenation */
        return expression_is_string(symtab, expr->expr_data.addop_data.left_expr) ||
               expression_is_string(symtab, expr->expr_data.addop_data.right_term);
    }

    return 0;
}

static int const_fold_int_expr_mode(SymTab_t *symtab, struct Expression *expr,
    long long *out_value, int emit_diagnostics);
static int const_fold_real_expr_mode(SymTab_t *symtab, struct Expression *expr,
    double *out_value, int emit_diagnostics);
static int const_fold_int_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value);
static int const_fold_real_expr(SymTab_t *symtab, struct Expression *expr, double *out_value);
static int evaluate_const_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value);
static int evaluate_real_const_expr(SymTab_t *symtab, struct Expression *expr, double *out_value);
int semcheck_resolve_scoped_enum_literal(SymTab_t *symtab, const char *type_name,
    const char *literal_name, long long *out_value);
static char *build_qualified_identifier_from_expr(struct Expression *expr);
static QualifiedIdent *build_qualified_ident_from_expr(struct Expression *expr);

int expression_is_set_const_expr(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr->type == EXPR_SET)
        return 1;

    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 &&
            node != NULL && node->const_set_value != NULL)
            return 1;
    }

    if (expr->type == EXPR_RECORD_ACCESS &&
        expr->expr_data.record_access_data.field_id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, symtab, expr->expr_data.record_access_data.field_id) != 0 &&
            node != NULL && node->const_set_value != NULL)
            return 1;
    }

    if (expr->type == EXPR_ADDOP)
    {
        int op = expr->expr_data.addop_data.addop_type;
        if ((op == PLUS || op == MINUS) &&
            expression_is_set_const_expr(symtab, expr->expr_data.addop_data.left_expr) &&
            expression_is_set_const_expr(symtab, expr->expr_data.addop_data.right_term))
            return 1;
    }

    if (expr->type == EXPR_MULOP)
    {
        int op = expr->expr_data.mulop_data.mulop_type;
        if ((op == STAR || op == AND) &&
            expression_is_set_const_expr(symtab, expr->expr_data.mulop_data.left_term) &&
            expression_is_set_const_expr(symtab, expr->expr_data.mulop_data.right_factor))
            return 1;
    }

    return 0;
}

/* Evaluate a set literal into a byte array (supports up to 0..255) */
int evaluate_set_const_bytes(SymTab_t *symtab, struct Expression *expr,
    unsigned char *out_bytes, size_t out_bytes_size, size_t *out_size,
    long long *out_mask, int *is_char_set)
{
    if (expr == NULL || out_bytes == NULL || out_bytes_size < 32)
        return 1;

    memset(out_bytes, 0, out_bytes_size);

    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 &&
            node != NULL && node->const_set_value != NULL)
        {
            size_t size = (size_t)node->const_set_size;
            if (size > out_bytes_size)
                return 1;
            memcpy(out_bytes, node->const_set_value, size);
            if (out_size != NULL)
                *out_size = size;
            if (out_mask != NULL && size <= sizeof(long long))
            {
                long long mask = 0;
                for (size_t i = 0; i < size; ++i)
                    mask |= ((long long)out_bytes[i]) << (i * 8);
                *out_mask = mask;
            }
            if (is_char_set != NULL)
                *is_char_set = (size > 4);
            return 0;
        }
        return 1;
    }

    if (expr->type == EXPR_RECORD_ACCESS &&
        expr->expr_data.record_access_data.field_id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, symtab, expr->expr_data.record_access_data.field_id) != 0 &&
            node != NULL && node->const_set_value != NULL)
        {
            size_t size = (size_t)node->const_set_size;
            if (size > out_bytes_size)
                return 1;
            memcpy(out_bytes, node->const_set_value, size);
            if (out_size != NULL)
                *out_size = size;
            if (out_mask != NULL && size <= sizeof(long long))
            {
                long long mask = 0;
                for (size_t i = 0; i < size; ++i)
                    mask |= ((long long)out_bytes[i]) << (i * 8);
                *out_mask = mask;
            }
            if (is_char_set != NULL)
                *is_char_set = (size > 4);
            return 0;
        }
        return 1;
    }

    if (expr->type == EXPR_ADDOP || expr->type == EXPR_MULOP)
    {
        unsigned char left_bytes[32];
        unsigned char right_bytes[32];
        size_t left_size = 0;
        size_t right_size = 0;
        int left_char = 0;
        int right_char = 0;
        int op = (expr->type == EXPR_ADDOP) ?
            expr->expr_data.addop_data.addop_type :
            expr->expr_data.mulop_data.mulop_type;
        struct Expression *left_expr = (expr->type == EXPR_ADDOP) ?
            expr->expr_data.addop_data.left_expr :
            expr->expr_data.mulop_data.left_term;
        struct Expression *right_expr = (expr->type == EXPR_ADDOP) ?
            expr->expr_data.addop_data.right_term :
            expr->expr_data.mulop_data.right_factor;

        if (evaluate_set_const_bytes(symtab, left_expr, left_bytes, sizeof(left_bytes),
                &left_size, NULL, &left_char) != 0)
            return 1;
        if (evaluate_set_const_bytes(symtab, right_expr, right_bytes, sizeof(right_bytes),
                &right_size, NULL, &right_char) != 0)
            return 1;

        size_t used = (left_size > right_size) ? left_size : right_size;
        if (used == 0)
            used = 4;

        for (size_t i = 0; i < used; ++i)
        {
            unsigned char l = (i < left_size) ? left_bytes[i] : 0;
            unsigned char r = (i < right_size) ? right_bytes[i] : 0;
            if (expr->type == EXPR_ADDOP && op == PLUS)
                out_bytes[i] = (unsigned char)(l | r);
            else if (expr->type == EXPR_ADDOP && op == MINUS)
                out_bytes[i] = (unsigned char)(l & (unsigned char)(~r));
            else if (expr->type == EXPR_MULOP && (op == STAR || op == AND))
                out_bytes[i] = (unsigned char)(l & r);
            else
                return 1;
        }

        if (out_size != NULL)
            *out_size = used;
        if (out_mask != NULL && used <= sizeof(long long))
        {
            long long mask = 0;
            for (size_t i = 0; i < used; ++i)
                mask |= ((long long)out_bytes[i]) << (i * 8);
            *out_mask = mask;
        }
        if (is_char_set != NULL)
            *is_char_set = (left_char || right_char || used > 4);
        return 0;
    }

    if (expr->type != EXPR_SET)
        return 1;

    size_t used = 0;
    long long mask = 0;
    long long max_value = 0;
    int char_set = 0;

    ListNode_t *element = expr->expr_data.set_data.elements;
    while (element != NULL)
    {
        if (element->cur != NULL)
        {
            struct SetElement *set_element = (struct SetElement *)element->cur;
            long long lower = 0;
            long long upper = 0;
            if (set_element->lower == NULL ||
                evaluate_const_expr(symtab, set_element->lower, &lower) != 0)
            {
                fprintf(stderr, "Error: set element is not a constant expression.\n");
                return 1;
            }
            if (set_element->upper != NULL)
            {
                if (evaluate_const_expr(symtab, set_element->upper, &upper) != 0)
                {
                    fprintf(stderr, "Error: set element upper bound is not a constant expression.\n");
                    return 1;
                }
            }
            else
            {
                upper = lower;
            }

            if (lower > upper)
            {
                long long tmp = lower;
                lower = upper;
                upper = tmp;
            }

            if (upper > max_value)
                max_value = upper;
            if (upper > 31)
                char_set = 1;

            if (upper < 0 || upper > 255 || lower < 0)
            {
                fprintf(stderr, "Error: set literal value %lld out of supported range 0..255.\n", upper);
                return 1;
            }

            for (long long value = lower; value <= upper; ++value)
            {
                size_t byte_index = (size_t)(value / 8);
                unsigned bit_mask = 1u << (value % 8);
                out_bytes[byte_index] |= (unsigned char)bit_mask;
            }
        }
        element = element->next;
    }

    used = (max_value <= 31) ? 4 : 32;
    if (out_size != NULL)
        *out_size = used;
    if (out_mask != NULL && used <= sizeof(long long))
    {
        for (size_t i = 0; i < used; ++i)
            mask |= ((long long)out_bytes[i]) << (i * 8);
        *out_mask = mask;
    }
    if (is_char_set != NULL)
        *is_char_set = (char_set || max_value > 31);
    return 0;
}

/* Evaluates a string constant expression, allocating a new string */
static int evaluate_string_const_expr(SymTab_t *symtab, struct Expression *expr, char **out_value)
{
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type)
    {
        case EXPR_STRING:
            *out_value = strdup(expr->expr_data.string);
            return (*out_value == NULL) ? 1 : 0;
        
        case EXPR_CHAR_CODE:
        {
            /* Character code: convert to a single-character string */
            *out_value = (char *)malloc(2);
            if (*out_value == NULL)
                return 1;
            (*out_value)[0] = (char)(expr->expr_data.char_code & 0xFF);
            (*out_value)[1] = '\0';
            return 0;
        }
        
        case EXPR_VAR_ID:
        {
            HashNode_t *node = NULL;
            if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL &&
                (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
            {
                if (node->const_string_value != NULL)
                {
                    *out_value = strdup(node->const_string_value);
                    return (*out_value == NULL) ? 1 : 0;
                }
                /* Char constants (e.g. DirectorySeparator = '/') */
                if (node->type != NULL &&
                    semcheck_tag_from_kgpc(node->type) == CHAR_TYPE)
                {
                    *out_value = (char *)malloc(2);
                    if (*out_value == NULL)
                        return 1;
                    (*out_value)[0] = (char)(node->const_int_value & 0xFF);
                    (*out_value)[1] = '\0';
                    return 0;
                }
            }
            fprintf(stderr, "Error: constant %s is undefined or not a string const.\n", expr->expr_data.id);
            return 1;
        }
        
        case EXPR_RECORD_ACCESS:
        {
            const char *fid = expr->expr_data.record_access_data.field_id;
            HashNode_t *node = NULL;
            if (fid != NULL &&
                FindSymbol(&node, symtab, fid) != 0 && node != NULL &&
                (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
            {
                if (node->const_string_value != NULL)
                {
                    *out_value = strdup(node->const_string_value);
                    return (*out_value == NULL) ? 1 : 0;
                }
                if (node->type != NULL &&
                    semcheck_tag_from_kgpc(node->type) == CHAR_TYPE)
                {
                    *out_value = (char *)malloc(2);
                    if (*out_value == NULL)
                        return 1;
                    (*out_value)[0] = (char)(node->const_int_value & 0xFF);
                    (*out_value)[1] = '\0';
                    return 0;
                }
            }
            fprintf(stderr, "Error: constant %s is undefined or not a string const.\n",
                fid ? fid : "<null>");
            return 1;
        }

        case EXPR_ADDOP:
        {
            if (expr->expr_data.addop_data.addop_type != PLUS)
            {
                fprintf(stderr, "Error: only + operator is supported for string concatenation.\n");
                return 1;
            }

            char *left = NULL, *right = NULL;
            if (evaluate_string_const_expr(symtab, expr->expr_data.addop_data.left_expr, &left) != 0)
                return 1;
            if (evaluate_string_const_expr(symtab, expr->expr_data.addop_data.right_term, &right) != 0)
            {
                free(left);
                return 1;
            }
            
            /* Concatenate strings */
            size_t len = strlen(left) + strlen(right) + 1;
            *out_value = (char *)malloc(len);
            if (*out_value == NULL)
            {
                free(left);
                free(right);
                return 1;
            }
            strcpy(*out_value, left);
            strcat(*out_value, right);
            
            free(left);
            free(right);
            return 0;
        }
        
        default:
            break;
    }

    fprintf(stderr, "Error: unsupported string const expression.\n");
    return 1;
}

static int semcheck_const_expr_char_cast_value(struct Expression *expr, long long *out_value,
    int *out_char_size)
{
    const char *target_id = NULL;
    struct Expression *arg = NULL;

    if (out_value == NULL)
        return 1;

    if (expr == NULL)
        return 1;

    if (expr->type == EXPR_FUNCTION_CALL)
    {
        ListNode_t *args = expr->expr_data.function_call_data.args_expr;
        if (args == NULL || args->next != NULL || args->cur == NULL)
            return 1;
        target_id = expr->expr_data.function_call_data.id;
        arg = (struct Expression *)args->cur;
    }
    else if (expr->type == EXPR_TYPECAST)
    {
        target_id = expr->expr_data.typecast_data.target_type_id;
        arg = expr->expr_data.typecast_data.expr;
    }
    else
    {
        return 1;
    }

    if (target_id == NULL || arg == NULL)
        return 1;
    if (semcheck_map_builtin_type_name_local(target_id) != CHAR_TYPE)
        return 1;

    long long value = 0;
    if (const_fold_int_expr(NULL, arg, &value) != 0)
        return 1;

    if (out_char_size != NULL)
    {
        if (pascal_identifier_equals(target_id, "WideChar") ||
            pascal_identifier_equals(target_id, "UnicodeChar"))
            *out_char_size = 2;
        else
            *out_char_size = 1;
    }
    *out_value = value;
    return 0;
}

typedef struct SubprogramPredeclLookup
{
    HashNode_t *exact_match;
    HashNode_t *first_mangled_match;
    HashNode_t *tree_match;
    HashNode_t *body_pair_match;
} SubprogramPredeclLookup;

static int semcheck_subprogram_node_source_unit_index(HashNode_t *candidate)
{
    if (candidate == NULL)
        return 0;
    if (candidate->type != NULL &&
        candidate->type->kind == TYPE_KIND_PROCEDURE &&
        candidate->type->info.proc_info.definition != NULL)
    {
        int def_unit_idx =
            candidate->type->info.proc_info.definition->tree_data.subprogram_data.source_unit_index;
        if (def_unit_idx > 0)
            return def_unit_idx;
    }
    return candidate->source_unit_index;
}

static SubprogramPredeclLookup semcheck_lookup_subprogram_predecl(
    SymTab_t *symtab,
    Tree_t *subprogram,
    const char *lookup_id,
    const char *mangled_id)
{
    SubprogramPredeclLookup result;
    memset(&result, 0, sizeof(result));

    if (symtab == NULL || subprogram == NULL || lookup_id == NULL)
        return result;

    ListNode_t *all_matches = FindAllIdents(symtab, lookup_id);
    ListNode_t *cur = all_matches;
    int current_has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
    int current_source_unit = subprogram->tree_data.subprogram_data.source_unit_index;

    while (cur != NULL)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        Tree_t *def = NULL;
        int existing_has_body = 0;
        int mangled_match = 0;
        int signature_match = 0;
        int candidate_source_unit = 0;

        if (candidate == NULL)
        {
            cur = cur->next;
            continue;
        }

        if (candidate->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
        {
            cur = cur->next;
            continue;
        }

        candidate_source_unit = semcheck_subprogram_node_source_unit_index(candidate);
        if ((current_source_unit > 0 && candidate_source_unit > 0 &&
             candidate_source_unit != current_source_unit) ||
            (current_source_unit == 0 && candidate_source_unit > 0))
        {
            cur = cur->next;
            continue;
        }

        if (candidate->mangled_id != NULL &&
            mangled_id != NULL &&
            strcmp(candidate->mangled_id, mangled_id) == 0)
        {
            mangled_match = 1;
            if (result.first_mangled_match == NULL)
                result.first_mangled_match = candidate;
        }

        if (candidate->type != NULL && candidate->type->kind == TYPE_KIND_PROCEDURE)
        {
            def = candidate->type->info.proc_info.definition;
            existing_has_body = (def != NULL &&
                def->tree_data.subprogram_data.statement_list != NULL);

            if (def == subprogram)
                result.tree_match = candidate;

            if (def != NULL &&
                semcheck_subprogram_signatures_equivalent(subprogram, def))
            {
                signature_match = 1;
                if (mangled_match && result.exact_match == NULL)
                    result.exact_match = candidate;
            }

            if (result.body_pair_match == NULL &&
                existing_has_body != current_has_body &&
                (signature_match || mangled_match))
            {
                result.body_pair_match = candidate;
            }
        }

        cur = cur->next;
    }

    if (all_matches != NULL)
        DestroyList(all_matches);

    return result;
}

static void semcheck_refresh_predecl_match(HashNode_t *node, Tree_t *subprogram)
{
    if (node == NULL || subprogram == NULL)
        return;

    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        if (node->mangled_id != NULL)
            free(node->mangled_id);
        node->mangled_id = strdup(subprogram->tree_data.subprogram_data.mangled_id);
    }

    if (subprogram->tree_data.subprogram_data.is_varargs)
        node->is_varargs = 1;
    if (subprogram->tree_data.subprogram_data.defined_in_unit)
    {
        /* Only set defined_in_unit if the node is from the same unit as the
         * subprogram. When a unit function (e.g. system.BinStr) has an
         * identical signature to a program-local function, the predecl lookup
         * may match the local node. Setting defined_in_unit=1 on the local
         * node would prevent it from being recognized as a local overload,
         * causing wrong overload resolution. */
        int node_unit = semcheck_subprogram_node_source_unit_index(node);
        int sub_unit = subprogram->tree_data.subprogram_data.source_unit_index;
        if (node_unit == sub_unit || (node_unit == 0 && !node->defined_in_unit && sub_unit == 0))
            node->defined_in_unit = 1;
        if (subprogram->tree_data.subprogram_data.unit_is_public)
            node->unit_is_public = 1;
    }
    if (subprogram->tree_data.subprogram_data.is_nested_scope)
        node->is_nested_scope = 1;

    /* Propagate method identity so consumers can use structural fields
     * instead of parsing mangled identifiers. */
    copy_method_identity_to_node(node, subprogram);

    if (node->type != NULL &&
        node->type->kind == TYPE_KIND_PROCEDURE &&
        subprogram->tree_data.subprogram_data.statement_list != NULL)
    {
        Tree_t *prev_def = node->type->info.proc_info.definition;
        if (prev_def == NULL ||
            prev_def->tree_data.subprogram_data.statement_list == NULL)
        {
            node->type->info.proc_info.definition = subprogram;
        }
    }
}

static int semcheck_param_list_equivalent(ListNode_t *lhs, ListNode_t *rhs)
{
    ListNode_t *lcur = lhs;
    ListNode_t *rcur = rhs;
    while (lcur != NULL && rcur != NULL)
    {
        Tree_t *lhs_decl = (Tree_t *)lcur->cur;
        Tree_t *rhs_decl = (Tree_t *)rcur->cur;
        if (!semcheck_param_decl_equivalent(lhs_decl, rhs_decl))
            return 0;
        lcur = lcur->next;
        rcur = rcur->next;
    }
    return (lcur == NULL && rcur == NULL);
}

/* Extract and validate a single argument from a const-expression function call.
 * Returns the argument expression on success, or NULL on failure (with error
 * printed to stderr). */
static struct Expression *extract_single_const_arg(ListNode_t *args, const char *func_name)
{
    assert(func_name != NULL);
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error: %s in const expression requires exactly one argument.\n", func_name);
        return NULL;
    }
    struct Expression *arg = (struct Expression *)args->cur;
    if (arg == NULL)
    {
        fprintf(stderr, "Error: %s argument is NULL.\n", func_name);
        return NULL;
    }
    return arg;
}

/* Look up the low and high bounds of a built-in Pascal type by name.
 * Returns 1 on success (bounds written to *low_out and *high_out), 0 if the
 * type is not a recognised built-in. */
static int get_builtin_type_bounds(const char *base_name,
    long long *low_out, long long *high_out)
{
    assert(low_out != NULL && high_out != NULL);
    if (base_name == NULL)
        return 0;

    typedef struct {
        const char *names[3];
        long long low;
        long long high;
    } TypeBoundsEntry;
    /* Note: QWord/UInt64 high is INT64_MAX (not UINT64_MAX) because the
     * const-expression evaluator uses signed long long throughout.  Proper
     * unsigned 64-bit semantics are not yet supported. */
    static const TypeBoundsEntry table[] = {
        { {"Int64",    NULL,       NULL},         (-9223372036854775807LL - 1), 9223372036854775807LL },
        { {"QWord",    "UInt64",   NULL},         0LL,                         9223372036854775807LL  },
        { {"LongInt",  "Integer",  NULL},         -2147483648LL,               2147483647LL           },
        { {"ValSInt",  NULL,       NULL},         -2147483648LL,               2147483647LL           },
        { {"Cardinal", "LongWord", "DWord"},      0LL,                         4294967295LL           },
        { {"SmallInt", NULL,       NULL},         -32768LL,                    32767LL                },
        { {"Word",     NULL,       NULL},         0LL,                         65535LL                },
        { {"ShortInt", NULL,       NULL},         -128LL,                      127LL                  },
        { {"Byte",     NULL,       NULL},         0LL,                         255LL                  },
        { {"Boolean",  NULL,       NULL},         0LL,                         1LL                    },
        { {"Char",     "AnsiChar", NULL},         0LL,                         255LL                  },
    };
    if (pascal_identifier_equals(base_name, "SizeInt") ||
        pascal_identifier_equals(base_name, "NativeInt") ||
        pascal_identifier_equals(base_name, "PtrInt"))
    {
        if (sizeof(void*) >= 8)
        {
            *low_out = (-9223372036854775807LL - 1);
            *high_out = 9223372036854775807LL;
        }
        else
        {
            *low_out = -2147483648LL;
            *high_out = 2147483647LL;
        }
        return 1;
    }
    if (pascal_identifier_equals(base_name, "SizeUInt") ||
        pascal_identifier_equals(base_name, "NativeUInt") ||
        pascal_identifier_equals(base_name, "PtrUInt"))
    {
        if (sizeof(void*) >= 8)
        {
            *low_out = 0LL;
            *high_out = 9223372036854775807LL;
        }
        else
        {
            *low_out = 0LL;
            *high_out = 4294967295LL;
        }
        return 1;
    }
    for (size_t i = 0; i < sizeof(table) / sizeof(table[0]); ++i)
    {
        for (int j = 0; j < 3 && table[i].names[j] != NULL; ++j)
        {
            if (pascal_identifier_equals(base_name, table[i].names[j]))
            {
                *low_out = table[i].low;
                *high_out = table[i].high;
                return 1;
            }
        }
    }
    return 0;
}

static int const_fold_real_expr_mode(SymTab_t *symtab, struct Expression *expr, double *out_value,
    int emit_diagnostics)
{
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type)
    {
        case EXPR_TYPECAST:
        {
            const char *target_id = expr->expr_data.typecast_data.target_type_id;
            int target_type = expr->expr_data.typecast_data.target_type;
            if (is_real_family_type(target_type) || is_real_type_name(symtab, target_id))
            {
                return const_fold_real_expr_mode(symtab, expr->expr_data.typecast_data.expr,
                    out_value, emit_diagnostics);
            }
            if (emit_diagnostics)
                fprintf(stderr, "Error: unsupported real const typecast target.\n");
            return 1;
        }
        case EXPR_RNUM:
            *out_value = (double)expr->expr_data.r_num;
            return 0;
        case EXPR_INUM:
            /* Integer in real context - promote to real */
            *out_value = (double)expr->expr_data.i_num;
            return 0;
        case EXPR_VAR_ID:
        {
            HashNode_t *node = NULL;
            if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL &&
                (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
            {
                if (node->type != NULL && kgpc_type_is_real(node->type))
                {
                    *out_value = node->const_real_value;
                    return 0;
                }
                *out_value = (double)node->const_int_value;
                return 0;
            }
            if (emit_diagnostics)
                fprintf(stderr, "Error: constant %s is undefined or not a const.\n", expr->expr_data.id);
            return 1;
        }
        case EXPR_SIGN_TERM:
        {
            double value;
            if (const_fold_real_expr_mode(symtab, expr->expr_data.sign_term, &value,
                    emit_diagnostics) != 0)
                return 1;
            *out_value = -value;
            return 0;
        }
        case EXPR_ADDOP:
        {
            double left, right;
            if (const_fold_real_expr_mode(symtab, expr->expr_data.addop_data.left_expr, &left,
                    emit_diagnostics) != 0)
                return 1;
            if (const_fold_real_expr_mode(symtab, expr->expr_data.addop_data.right_term, &right,
                    emit_diagnostics) != 0)
                return 1;
            switch (expr->expr_data.addop_data.addop_type)
            {
                case PLUS:
                    *out_value = left + right;
                    return 0;
                case MINUS:
                    *out_value = left - right;
                    return 0;
                default:
                    break;
            }
            break;
        }
        case EXPR_MULOP:
        {
            double left, right;
            if (const_fold_real_expr_mode(symtab, expr->expr_data.mulop_data.left_term, &left,
                    emit_diagnostics) != 0)
                return 1;
            if (const_fold_real_expr_mode(symtab, expr->expr_data.mulop_data.right_factor, &right,
                    emit_diagnostics) != 0)
                return 1;
            switch (expr->expr_data.mulop_data.mulop_type)
            {
                case STAR:
                    *out_value = left * right;
                    return 0;
                case SLASH:
                    /* Allow 0.0/0.0 to produce NaN, and x/0.0 to produce Inf/-Inf
                     * This is required for FPC's TSingleHelper.NaN/TDoubleHelper.NaN etc */
                    *out_value = left / right;
                    return 0;
                default:
                    break;
            }
            break;
        }
        case EXPR_FUNCTION_CALL:
        {
            const char *id = expr->expr_data.function_call_data.id;
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;
            if (id != NULL && is_real_type_name(symtab, id))
            {
                struct Expression *arg = extract_single_const_arg(args, id);
                if (arg == NULL)
                    return 1;
                return const_fold_real_expr_mode(symtab, arg, out_value, emit_diagnostics);
            }
            if (id != NULL && pascal_identifier_equals(id, "Ln"))
            {
                struct Expression *arg = extract_single_const_arg(args, "Ln");
                if (arg == NULL)
                    return 1;
                double value = 0.0;
                if (const_fold_real_expr_mode(symtab, arg, &value, emit_diagnostics) != 0)
                    return 1;
                if (value <= 0.0)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Ln argument must be > 0 in const expression.\n");
                    return 1;
                }
                *out_value = log(value);
                return 0;
            }
            break;
        }
        default:
            break;
    }

    if (emit_diagnostics)
        fprintf(stderr, "Error: unsupported real const expression.\n");
    return 1;
}

/* Helper function to resolve a type name to its base primitive type name.
 * This follows type aliases until we reach a known primitive type.
 * Returns the resolved type name (caller should not free), or NULL if unknown.
 */
/* Track recursion depth to prevent stack overflow from circular type definitions.
 * Note: This compiler is single-threaded, so static variable is safe. */
static int resolve_type_depth = 0;
#define MAX_RESOLVE_TYPE_DEPTH 100

static const char *resolve_type_to_base_name(SymTab_t *symtab,
    const QualifiedIdent *type_id,
    const char *type_name)
{
    const char *input_name = NULL;
    if (type_id != NULL)
        input_name = qualified_ident_last(type_id);
    if (input_name == NULL)
        input_name = type_name;
    if (input_name == NULL)
        return NULL;
    
    /* Prevent infinite recursion from circular type definitions */
    if (resolve_type_depth >= MAX_RESOLVE_TYPE_DEPTH)
    {
        KGPC_SEMCHECK_HARD_ASSERT(0,
            "type resolution depth limit reached for '%s' (possible circular definition)",
            input_name);
        return NULL;
    }
    
    /* First, check if it's already a known primitive type */
    if (pascal_identifier_equals(input_name, "Int64") ||
        pascal_identifier_equals(input_name, "QWord") ||
        pascal_identifier_equals(input_name, "UInt64") ||
        pascal_identifier_equals(input_name, "LongInt") ||
        pascal_identifier_equals(input_name, "Integer") ||
        pascal_identifier_equals(input_name, "Cardinal") ||
        pascal_identifier_equals(input_name, "LongWord") ||
        pascal_identifier_equals(input_name, "DWord") ||
        pascal_identifier_equals(input_name, "SmallInt") ||
        pascal_identifier_equals(input_name, "Word") ||
        pascal_identifier_equals(input_name, "ShortInt") ||
        pascal_identifier_equals(input_name, "Byte") ||
        pascal_identifier_equals(input_name, "Boolean") ||
        pascal_identifier_equals(input_name, "Char") ||
        pascal_identifier_equals(input_name, "AnsiChar") ||
        pascal_identifier_equals(input_name, "WideChar") ||
        pascal_identifier_equals(input_name, "Pointer") ||
        pascal_identifier_equals(input_name, "PChar") ||
        pascal_identifier_equals(input_name, "Double") ||
        pascal_identifier_equals(input_name, "Real") ||
        pascal_identifier_equals(input_name, "Single"))
    {
        return input_name;
    }
    
    /* Try to look up as a type alias in the symbol table */
    if (symtab != NULL)
    {
        HashNode_t *type_node = NULL;
        const char *lookup_name = type_name;
        char *qualified_name = NULL;

        /* Prefer unit-defined types when we have a structured unit qualification. */
        if (type_id != NULL && type_id->count > 1)
        {
            const char *unit_name = type_id->segments != NULL ? type_id->segments[0] : NULL;
            const char *base_name = qualified_ident_last(type_id);
            if (unit_name != NULL && base_name != NULL && semcheck_is_unit_name(unit_name))
            {
                type_node = semcheck_find_type_node_with_unit_flag(symtab, base_name, 1);
                if (type_node != NULL && type_node->hash_type != HASHTYPE_TYPE)
                    type_node = NULL;
            }
        }

        if (type_id != NULL && type_node == NULL)
        {
            qualified_name = qualified_ident_join(type_id, ".");
            if (qualified_name != NULL)
                lookup_name = qualified_name;
        }

        /* Resolve with preferred-type lookup (unit/scope-aware + nested suffix fallback). */
        if (type_node == NULL)
            type_node = semcheck_find_preferred_type_node(symtab, lookup_name);

        if (kgpc_getenv("KGPC_DEBUG_RESOLVE_TYPE") != NULL)
        {
            fprintf(stderr, "[resolve_type] '%s' initial lookup node=%p (%s)\n",
                input_name, (void*)type_node, lookup_name != NULL ? lookup_name : "<null>");
        }
        
        /* If lookup failed and this is a qualified name, try the unqualified part */
        if (type_node == NULL)
        {
            const char *last = (type_id != NULL) ? qualified_ident_last(type_id) : NULL;
            if (last != NULL)
            {
                lookup_name = last;
                type_node = semcheck_find_preferred_type_node(symtab, lookup_name);
                if (kgpc_getenv("KGPC_DEBUG_RESOLVE_TYPE") != NULL)
                {
                    fprintf(stderr, "[resolve_type] '%s' unqualified '%s' node=%p\n",
                        input_name, lookup_name, (void*)type_node);
                }
            }
        }
        if (qualified_name != NULL)
            free(qualified_name);

        if (type_node != NULL)
        {
            if (kgpc_getenv("KGPC_DEBUG_RESOLVE_TYPE") != NULL)
            {
                fprintf(stderr, "[resolve_type] '%s' hash_type=%d type=%p\n",
                    input_name, type_node->hash_type, (void*)type_node->type);
            }
            if (type_node->hash_type == HASHTYPE_TYPE && type_node->type != NULL)
            {
                /* Get the underlying type from KgpcType */
                KgpcType *kgpc_type = type_node->type;
                
                /* CRITICAL: Check TypeAlias FIRST for target_type_id before checking primitive tag.
                 * This preserves the original type name for small integer types (Byte, Word, etc.)
                 * which are all mapped to LONGINT_TYPE by the predeclare_types function but need
                 * to retain their original type semantics for SizeOf/High/Low operations. */
                struct TypeAlias *alias = kgpc_type_get_type_alias(kgpc_type);
                if (alias != NULL && alias->target_type_id != NULL && type_id != NULL)
                {
                    char *qualified = qualified_ident_join(type_id, ".");
                    if (qualified != NULL &&
                        pascal_identifier_equals(alias->target_type_id, qualified))
                    {
                        HashNode_t *alt = semcheck_find_type_excluding_alias(symtab, qualified, alias);
                        if (alt != NULL && alt->type != NULL)
                        {
                            type_node = alt;
                            kgpc_type = alt->type;
                            alias = kgpc_type_get_type_alias(kgpc_type);
                        }
                    }
                    free(qualified);
                }
                if (kgpc_getenv("KGPC_DEBUG_RESOLVE_TYPE") != NULL)
                {
                    fprintf(stderr, "[resolve_type] '%s' kgpc_type kind=%d alias=%p\n",
                        input_name, kgpc_type->kind, (void*)alias);
                    if (alias != NULL)
                    {
                        fprintf(stderr, "[resolve_type] '%s' alias target_type_id='%s' base_type=%d\n",
                            input_name, alias->target_type_id ? alias->target_type_id : "<null>",
                            alias->base_type);
                    }
                }
                if (alias != NULL)
                {
                    int allow_recursion = 1;
                    if (alias->target_type_ref != NULL &&
                        alias->target_type_ref->name != NULL &&
                        type_id != NULL &&
                        qualified_ident_equals_ci(alias->target_type_ref->name, type_id))
                    {
                        allow_recursion = 0;
                    }
                    if (alias->target_type_id != NULL &&
                        pascal_identifier_equals(alias->target_type_id, input_name))
                    {
                        allow_recursion = 0;
                    }
                    if (alias->target_type_id != NULL &&
                        type_name != NULL &&
                        pascal_identifier_equals(alias->target_type_id, type_name))
                    {
                        allow_recursion = 0;
                    }

                    if (!allow_recursion && type_id != NULL)
                    {
                        const char *base_name = qualified_ident_last(type_id);
                        HashNode_t *alt = find_type_node_with_range_metadata(symtab, base_name, alias);
                        if (alt != NULL && alt->type != NULL)
                        {
                            kgpc_type = alt->type;
                            alias = kgpc_type_get_type_alias(kgpc_type);
                        }
                    }

                    /* Recursively resolve via target_type_id if available.
                     * This will eventually reach a builtin type like "Word", "Byte", etc.
                     * which are handled by the builtin check at the top of this function. */
                    if (allow_recursion &&
                        (alias->target_type_ref != NULL || alias->target_type_id != NULL))
                    {
                        resolve_type_depth++;
                        const char *result = resolve_type_to_base_name(symtab,
                            alias->target_type_ref != NULL ? alias->target_type_ref->name : NULL,
                            alias->target_type_id);
                        resolve_type_depth--;
                        return result;
                    }
                    /* Check for base_type tag if no target_type_id */
                    if (alias->base_type != UNKNOWN_TYPE && alias->base_type != 0)
                    {
                        switch (alias->base_type)
                        {
                            case INT_TYPE: return "Integer";
                            case LONGINT_TYPE: return "LongInt";
                            case INT64_TYPE: return "Int64";
                            case BOOL: return "Boolean";
                            case CHAR_TYPE: return "Char";
                            case REAL_TYPE: return "Real";
                            case EXTENDED_TYPE: return "Extended";
                            case POINTER_TYPE: return "Pointer";
                            default: break;
                        }
                    }
                }
                
                /* Fallback: Check primitive type tag if no alias info */
                if (kgpc_type->kind == TYPE_KIND_PRIMITIVE)
                {
                    switch (kgpc_type->info.primitive_type_tag)
                    {
                        case INT_TYPE: return "Integer";
                        case LONGINT_TYPE: return "LongInt";
                        case INT64_TYPE: return "Int64";
                        case BOOL: return "Boolean";
                        case CHAR_TYPE: return "Char";
                        case REAL_TYPE: return "Real";
                        case EXTENDED_TYPE: return "Extended";
                        case POINTER_TYPE: return "Pointer";
                        default: break;
                    }
                }
            }
        }
    }
    
    return NULL; /* Unknown type */
}

static int resolve_range_bounds_for_type_ref(SymTab_t *symtab, const QualifiedIdent *type_ref,
    long long *out_low, long long *out_high);

static int resolve_range_bounds_for_type(SymTab_t *symtab, const char *type_name,
    long long *out_low, long long *out_high)
{
    if (symtab == NULL || type_name == NULL || out_low == NULL || out_high == NULL)
        return 0;

    HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_name);
    if (type_node == NULL)
        return 0;

    struct TypeAlias *alias = get_type_alias_from_node(type_node);
    if (alias != NULL)
    {
        if (alias->is_enum && alias->enum_literals != NULL &&
            !alias->enum_has_explicit_values)
        {
            int count = ListLength(alias->enum_literals);
            if (count > 0)
            {
                *out_low = 0;
                *out_high = (long long)count - 1;
                return 1;
            }
        }

        if (alias->range_known)
        {
            *out_low = alias->range_start;
            *out_high = alias->range_end;
            return 1;
        }

        if (alias->target_type_ref != NULL && alias->target_type_ref->name != NULL &&
            resolve_range_bounds_for_type_ref(symtab, alias->target_type_ref->name,
                out_low, out_high))
        {
            return 1;
        }

        if (alias->target_type_id != NULL &&
            !pascal_identifier_equals(alias->target_type_id, type_name) &&
            resolve_range_bounds_for_type(symtab, alias->target_type_id, out_low, out_high))
        {
            return 1;
        }
    }

    const char *base_name = semcheck_base_type_name(type_name);
    HashNode_t *alt = find_type_node_with_range_metadata(symtab, base_name, alias);
    if (alt != NULL)
    {
        struct TypeAlias *alt_alias = get_type_alias_from_node(alt);
        if (alt_alias != NULL && alt_alias->range_known)
        {
            *out_low = alt_alias->range_start;
            *out_high = alt_alias->range_end;
            return 1;
        }
    }

    return 0;
}

int semcheck_resolve_range_bounds_for_type(SymTab_t *symtab, const char *type_name,
    long long *out_low, long long *out_high)
{
    return resolve_range_bounds_for_type(symtab, type_name, out_low, out_high);
}

static HashNode_t *find_type_node_with_range_metadata(SymTab_t *symtab,
    const char *base_name, const struct TypeAlias *exclude_alias)
{
    if (symtab == NULL || base_name == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, base_name);
    HashNode_t *best = NULL;
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type == HASHTYPE_TYPE && node->type != NULL)
        {
            struct TypeAlias *alias = kgpc_type_get_type_alias(node->type);
            if (alias == NULL || alias == exclude_alias)
            {
                cur = cur->next;
                continue;
            }
            if (alias->is_range && alias->range_known)
            {
                best = node;
                break;
            }
            if (best == NULL && alias->base_type != UNKNOWN_TYPE)
                best = node;
        }
        cur = cur->next;
    }
    if (matches != NULL)
        DestroyList(matches);
    return best;
}

static HashNode_t *semcheck_find_exact_type_node_for_qid(SymTab_t *symtab,
    const QualifiedIdent *type_ref)
{
    if (symtab == NULL || type_ref == NULL)
        return NULL;

    HashNode_t *type_node = NULL;
    char *qualified = qualified_ident_join(type_ref, ".");
    if (qualified != NULL)
    {
        if (FindSymbol(&type_node, symtab, qualified) != 0 &&
            type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            free(qualified);
            return type_node;
        }
        type_node = semcheck_find_preferred_type_node(symtab, qualified);
        if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            free(qualified);
            return type_node;
        }
        free(qualified);
    }

    if (type_ref->count > 1 && type_ref->segments != NULL && type_ref->segments[0] != NULL)
    {
        const char *unit_name = type_ref->segments[0];
        const char *base_name = qualified_ident_last(type_ref);
        if (base_name != NULL)
        {
            ListNode_t *matches = FindAllIdents(symtab, base_name);
            for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
            {
                HashNode_t *candidate = (HashNode_t *)cur->cur;
                if (candidate == NULL || candidate->hash_type != HASHTYPE_TYPE)
                    continue;
                const char *candidate_unit = unit_registry_get(candidate->source_unit_index);
                if (candidate_unit != NULL &&
                    pascal_identifier_equals(candidate_unit, unit_name))
                {
                    type_node = candidate;
                    if (candidate->defined_in_unit)
                        break;
                }
            }
            if (matches != NULL)
                DestroyList(matches);
            if (type_node != NULL)
                return type_node;
        }
    }

    TypeRef temp_ref = {0};
    temp_ref.name = (QualifiedIdent *)type_ref;
    return semcheck_find_preferred_type_node_with_ref(symtab, &temp_ref, NULL);
}

static int resolve_range_bounds_for_type_ref(SymTab_t *symtab, const QualifiedIdent *type_ref,
    long long *out_low, long long *out_high)
{
    if (symtab == NULL || type_ref == NULL || out_low == NULL || out_high == NULL)
        return 0;

    HashNode_t *type_node = semcheck_find_exact_type_node_for_qid(symtab, type_ref);
    if (type_node == NULL)
        return 0;
    if (type_node->hash_type != HASHTYPE_TYPE)
        return 0;

    struct TypeAlias *alias = get_type_alias_from_node(type_node);
    if (alias != NULL && alias->is_enum && alias->enum_literals != NULL &&
        !alias->enum_has_explicit_values)
    {
        int count = ListLength(alias->enum_literals);
        if (count > 0)
        {
            *out_low = 0;
            *out_high = (long long)count - 1;
            return 1;
        }
    }
    if (alias != NULL && alias->target_type_id != NULL)
    {
        char *qualified = qualified_ident_join(type_ref, ".");
        if (qualified != NULL &&
            pascal_identifier_equals(alias->target_type_id, qualified))
        {
            HashNode_t *alt = semcheck_find_type_excluding_alias(symtab, qualified, alias);
            if (alt != NULL && alt->hash_type == HASHTYPE_TYPE)
            {
                type_node = alt;
                alias = get_type_alias_from_node(type_node);
            }
        }
        free(qualified);
    }
    if (alias == NULL || !alias->range_known)
    {
        const char *base_name = qualified_ident_last(type_ref);
        HashNode_t *alt = find_type_node_with_range_metadata(symtab, base_name, alias);
        if (alt != NULL)
            alias = get_type_alias_from_node(alt);
    }
    if (alias != NULL && alias->range_known)
    {
        *out_low = alias->range_start;
        *out_high = alias->range_end;
        return 1;
    }

    if (alias != NULL)
    {
        if (alias->target_type_ref != NULL && alias->target_type_ref->name != NULL &&
            !qualified_ident_equals_ci(alias->target_type_ref->name, type_ref) &&
            resolve_range_bounds_for_type_ref(symtab, alias->target_type_ref->name,
                out_low, out_high))
        {
            return 1;
        }

        if (alias->target_type_id != NULL)
        {
            char *qualified = qualified_ident_join(type_ref, ".");
            int same_target = (qualified != NULL &&
                pascal_identifier_equals(alias->target_type_id, qualified));
            free(qualified);
            if (!same_target &&
                resolve_range_bounds_for_type(symtab, alias->target_type_id,
                    out_low, out_high))
            {
                return 1;
            }
        }
    }

    return 0;
}

static int evaluate_const_expr_ordinal_bounds(SymTab_t *symtab, struct Expression *expr,
    long long *out_low, long long *out_high)
{
    if (expr == NULL || out_low == NULL || out_high == NULL)
        return 0;

    if (expr->resolved_kgpc_type != NULL)
    {
        struct TypeAlias *alias = kgpc_type_get_type_alias(expr->resolved_kgpc_type);
        if (alias != NULL)
        {
            if (alias->is_enum && alias->enum_literals != NULL &&
                !alias->enum_has_explicit_values)
            {
                int count = ListLength(alias->enum_literals);
                if (count > 0)
                {
                    *out_low = 0;
                    *out_high = (long long)count - 1;
                    return 1;
                }
            }
            if (alias->range_known)
            {
                *out_low = alias->range_start;
                *out_high = alias->range_end;
                return 1;
            }
            if (alias->target_type_ref != NULL && alias->target_type_ref->name != NULL &&
                resolve_range_bounds_for_type_ref(symtab, alias->target_type_ref->name,
                    out_low, out_high))
            {
                return 1;
            }
            if (alias->target_type_id != NULL &&
                resolve_range_bounds_for_type(symtab, alias->target_type_id, out_low, out_high))
            {
                return 1;
            }
            if (alias->alias_name != NULL &&
                get_builtin_type_bounds(alias->alias_name, out_low, out_high))
            {
                return 1;
            }
            if (alias->target_type_id != NULL &&
                get_builtin_type_bounds(alias->target_type_id, out_low, out_high))
            {
                return 1;
            }
        }

        if (expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE)
        {
            switch (expr->resolved_kgpc_type->info.primitive_type_tag)
            {
                case BYTE_TYPE: *out_low = 0; *out_high = 255; return 1;
                case WORD_TYPE: *out_low = 0; *out_high = 65535; return 1;
                case LONGWORD_TYPE: *out_low = 0; *out_high = 4294967295LL; return 1;
                case QWORD_TYPE: *out_low = 0; *out_high = 9223372036854775807LL; return 1;
                case INT_TYPE:
                case LONGINT_TYPE: *out_low = -2147483648LL; *out_high = 2147483647LL; return 1;
                case INT64_TYPE: *out_low = (-9223372036854775807LL - 1); *out_high = 9223372036854775807LL; return 1;
                case BOOL: *out_low = 0; *out_high = 1; return 1;
                case CHAR_TYPE: *out_low = 0; *out_high = 255; return 1;
                default: break;
            }
        }
    }

    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL &&
        resolve_range_bounds_for_type(symtab, expr->expr_data.id, out_low, out_high))
    {
        return 1;
    }

    if (expr->type == EXPR_RECORD_ACCESS)
    {
        QualifiedIdent *qid = build_qualified_ident_from_expr(expr);
        if (qid != NULL)
        {
            int resolved = resolve_range_bounds_for_type_ref(symtab, qid, out_low, out_high);
            qualified_ident_free(qid);
            if (resolved)
                return 1;
        }
    }

    return 0;
}

static int const_typecast_target_is_ordinal(SymTab_t *symtab, struct Expression *expr,
    long long *out_low, long long *out_high)
{
    const TypeRef *target_ref = NULL;
    const char *target_id = NULL;

    if (expr == NULL || out_low == NULL || out_high == NULL)
        return 0;

    target_ref = expr->expr_data.typecast_data.target_type_ref;
    target_id = expr->expr_data.typecast_data.target_type_id;

    if (target_ref != NULL && target_ref->name != NULL &&
        resolve_range_bounds_for_type_ref(symtab, target_ref->name, out_low, out_high))
    {
        return 1;
    }
    if (target_id != NULL &&
        resolve_range_bounds_for_type(symtab, target_id, out_low, out_high))
    {
        return 1;
    }
    if (target_id != NULL &&
        get_builtin_type_bounds(semcheck_base_type_name(target_id), out_low, out_high))
    {
        return 1;
    }
    return 0;
}

static int const_fold_int_expr_mode(SymTab_t *symtab, struct Expression *expr, long long *out_value,
    int emit_diagnostics)
{
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type)
    {
        case EXPR_RNUM:
            /* Real numbers in integer context - truncate or error */
            if (emit_diagnostics)
                fprintf(stderr, "Error: real number constant in integer context.\n");
            return 1;
        case EXPR_INUM:
            *out_value = expr->expr_data.i_num;
            return 0;
        case EXPR_BOOL:
            *out_value = expr->expr_data.bool_value ? 1 : 0;
            return 0;
        case EXPR_NIL:
            /* nil is represented as 0 in pointer context */
            *out_value = 0;
            return 0;
        case EXPR_STRING:
            /* Handle character literals in const expressions */
            if (expr->expr_data.string != NULL && 
                expr->expr_data.string[0] != '\0' && 
                expr->expr_data.string[1] == '\0')
            {
                /* Single character literal - return its ASCII value */
                *out_value = (unsigned char)expr->expr_data.string[0];
                return 0;
            }
            if (emit_diagnostics)
                fprintf(stderr, "Error: string literal in const expression must be a single character.\n");
            return 1;
        case EXPR_CHAR_CODE:
            *out_value = (unsigned char)(expr->expr_data.char_code & 0xFF);
            return 0;
        case EXPR_VAR_ID:
        {
            HashNode_t *node = NULL;
            if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL &&
                (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
            {
                if (node->const_string_value != NULL)
                {
                    if (node->const_string_value[0] == '\0')
                    {
                        *out_value = 0;
                        return 0;
                    }
                    if (node->const_string_value[1] == '\0')
                    {
                        *out_value = (unsigned char)node->const_string_value[0];
                        return 0;
                    }
                }
                *out_value = node->const_int_value;
                return 0;
            }
            if (emit_diagnostics)
                fprintf(stderr, "Error: constant %s is undefined or not a const.\n", expr->expr_data.id);
            return 1;
        }
        case EXPR_SIGN_TERM:
        {
            long long value;
            if (const_fold_int_expr_mode(symtab, expr->expr_data.sign_term, &value,
                    emit_diagnostics) != 0)
                return 1;
            *out_value = -value;
            return 0;
        }
        case EXPR_TYPECAST:
        {
            long long inner_value = 0;
            long long ordinal_low = 0;
            long long ordinal_high = 0;
            if (const_fold_int_expr_mode(symtab, expr->expr_data.typecast_data.expr, &inner_value,
                    emit_diagnostics) != 0)
            {
                double real_value = 0.0;
                if (const_fold_real_expr_mode(symtab, expr->expr_data.typecast_data.expr, &real_value,
                        emit_diagnostics) != 0)
                    return 1;
                inner_value = (long long)real_value;
            }

            int target_type = expr->expr_data.typecast_data.target_type;
            const char *id = NULL;
            if (expr->expr_data.typecast_data.target_type_id != NULL)
                id = semcheck_base_type_name(expr->expr_data.typecast_data.target_type_id);
            if (kgpc_getenv("KGPC_DEBUG_CONST_CAST") != NULL)
            {
                fprintf(stderr, "[KGPC] const cast id=%s target_type=%d inner=%lld\n",
                    id != NULL ? id : "<null>", target_type, inner_value);
            }
            if (target_type == UNKNOWN_TYPE &&
                expr->expr_data.typecast_data.target_type_id != NULL)
            {
                if (id == NULL)
                    break;
                if (strcasecmp(id, "Byte") == 0 || strcasecmp(id, "Word") == 0 ||
                    strcasecmp(id, "Integer") == 0 || strcasecmp(id, "ShortInt") == 0 ||
                    strcasecmp(id, "SmallInt") == 0)
                {
                    if (strcasecmp(id, "Byte") == 0)
                        target_type = BYTE_TYPE;
                    else if (strcasecmp(id, "Word") == 0)
                        target_type = WORD_TYPE;
                    else
                        target_type = INT_TYPE;
                }
                else if (strcasecmp(id, "LongInt") == 0)
                    target_type = LONGINT_TYPE;
                else if (strcasecmp(id, "Cardinal") == 0 || strcasecmp(id, "LongWord") == 0 ||
                         strcasecmp(id, "DWord") == 0)
                    target_type = LONGWORD_TYPE;
                else if (strcasecmp(id, "Int64") == 0 || strcasecmp(id, "UInt64") == 0 ||
                         strcasecmp(id, "QWord") == 0 ||
                         strcasecmp(id, "SizeInt") == 0 || strcasecmp(id, "SizeUInt") == 0 ||
                         strcasecmp(id, "NativeInt") == 0 || strcasecmp(id, "NativeUInt") == 0)
                {
                    if (strcasecmp(id, "QWord") == 0 || strcasecmp(id, "UInt64") == 0)
                        target_type = QWORD_TYPE;
                    else
                        target_type = INT64_TYPE;
                }
                else if (strcasecmp(id, "Pointer") == 0)
                    target_type = POINTER_TYPE;
                else if (strcasecmp(id, "Char") == 0)
                    target_type = CHAR_TYPE;
                else if (strcasecmp(id, "Boolean") == 0)
                    target_type = BOOL;
            }

            switch (target_type)
            {
                case CHAR_TYPE:
                {
                    int is_wide_char = 0;
                    if (id != NULL &&
                        (strcasecmp(id, "WideChar") == 0 ||
                         strcasecmp(id, "UnicodeChar") == 0))
                    {
                        is_wide_char = 1;
                    }
                    long long max_char_value = is_wide_char ? 65535LL : 255LL;
                    if (inner_value < 0 || inner_value > max_char_value)
                    {
                            if (emit_diagnostics)
                                fprintf(stderr, "Error: typecast value %lld out of range for %s.\n",
                            inner_value, is_wide_char ? "WideChar" : "Char");
                        return 1;
                    }
                    if (is_wide_char)
                        *out_value = (uint16_t)inner_value;
                    else
                        *out_value = (unsigned char)inner_value;
                    return 0;
                }
                case BOOL:
                    *out_value = (inner_value != 0);
                    return 0;
                case ENUM_TYPE:
                    if (const_typecast_target_is_ordinal(symtab, expr, &ordinal_low, &ordinal_high))
                    {
                        if (inner_value < ordinal_low || inner_value > ordinal_high)
                        {
                            if (emit_diagnostics)
                                fprintf(stderr,
                                "Error: typecast value %lld out of range for ordinal target.\n",
                                inner_value);
                            return 1;
                        }
                    }
                    *out_value = inner_value;
                    return 0;
                case INT_TYPE:
                case LONGINT_TYPE:
                    if (id != NULL)
                    {
                        if (strcasecmp(id, "ShortInt") == 0)
                        {
                            *out_value = (int8_t)inner_value;
                            return 0;
                        }
                        if (strcasecmp(id, "SmallInt") == 0)
                        {
                            *out_value = (int16_t)inner_value;
                            return 0;
                        }
                    }
                    *out_value = (int32_t)inner_value;
                    return 0;
                case BYTE_TYPE:
                    *out_value = (uint8_t)inner_value;
                    return 0;
                case WORD_TYPE:
                    *out_value = (uint16_t)inner_value;
                    return 0;
                case LONGWORD_TYPE:
                    *out_value = (uint32_t)inner_value;
                    return 0;
                case QWORD_TYPE:
                    *out_value = (uint64_t)inner_value;
                    return 0;
                case INT64_TYPE:
                    if (id != NULL &&
                        (strcasecmp(id, "QWord") == 0 || strcasecmp(id, "UInt64") == 0 ||
                         strcasecmp(id, "SizeUInt") == 0 || strcasecmp(id, "NativeUInt") == 0 ||
                         strcasecmp(id, "PtrUInt") == 0))
                    {
                        *out_value = (uint64_t)inner_value;
                        return 0;
                    }
                    *out_value = inner_value;
                    return 0;
                case POINTER_TYPE:
                    *out_value = inner_value;
                    return 0;
                case UNKNOWN_TYPE:
                    if (const_typecast_target_is_ordinal(symtab, expr, &ordinal_low, &ordinal_high))
                    {
                        if (inner_value < ordinal_low || inner_value > ordinal_high)
                        {
                            if (emit_diagnostics)
                                fprintf(stderr,
                                "Error: typecast value %lld out of range for ordinal target.\n",
                                inner_value);
                            return 1;
                        }
                    }
                    /* Treat unresolved ordinal aliases as integer-like passthrough */
                    *out_value = inner_value;
                    return 0;
                default:
                    if (const_typecast_target_is_ordinal(symtab, expr, &ordinal_low, &ordinal_high))
                    {
                        if (inner_value < ordinal_low || inner_value > ordinal_high)
                        {
                            if (emit_diagnostics)
                                fprintf(stderr,
                                "Error: typecast value %lld out of range for ordinal target.\n",
                                inner_value);
                            return 1;
                        }
                        *out_value = inner_value;
                        return 0;
                    }
            if (emit_diagnostics)
                fprintf(stderr, "Error: unsupported const typecast target.\n");
            return 1;
            }
        }
        case EXPR_ADDOP:
        {
            long long left, right;
            if (const_fold_int_expr_mode(symtab, expr->expr_data.addop_data.left_expr, &left,
                    emit_diagnostics) != 0)
                return 1;
            if (const_fold_int_expr_mode(symtab, expr->expr_data.addop_data.right_term, &right,
                    emit_diagnostics) != 0)
                return 1;
            switch (expr->expr_data.addop_data.addop_type)
            {
                case PLUS:
                    *out_value = left + right;
                    return 0;
                case MINUS:
                    *out_value = left - right;
                    return 0;
                case OR:
                    /* Bitwise OR for const expressions (FPC bootstrap: S_IRWXU = S_IRUSR or S_IWUSR) */
                    *out_value = left | right;
                    return 0;
                default:
                    break;
            }
            break;
        }
        case EXPR_MULOP:
        {
            long long left, right;
            if (const_fold_int_expr_mode(symtab, expr->expr_data.mulop_data.left_term, &left,
                    emit_diagnostics) != 0)
                return 1;
            if (const_fold_int_expr_mode(symtab, expr->expr_data.mulop_data.right_factor, &right,
                    emit_diagnostics) != 0)
                return 1;
            switch (expr->expr_data.mulop_data.mulop_type)
            {
                case STAR:
                    *out_value = left * right;
                    return 0;
                case DIV:
                    if (right == 0)
                    {
                        if (emit_diagnostics)
                            fprintf(stderr, "Error: division by zero in const expression.\n");
                        return 1;
                    }
                    *out_value = left / right;
                    return 0;
                case MOD:
                    if (right == 0)
                    {
                        if (emit_diagnostics)
                            fprintf(stderr, "Error: modulo by zero in const expression.\n");
                        return 1;
                    }
                    *out_value = left % right;
                    return 0;
                case SHL:
                    /* Shift left: left shl right */
                    *out_value = left << right;
                    return 0;
                case SHR:
                    /* Shift right: left shr right */
                    *out_value = (unsigned long long)left >> right;
                    return 0;
                case AND:
                    /* Bitwise AND for const expressions (FPC bootstrap) */
                    *out_value = left & right;
                    return 0;
                case XOR:
                    /* Bitwise XOR for const expressions (FPC bootstrap) */
                    *out_value = left ^ right;
                    return 0;
                default:
                    break;
            }
            break;
        }
        case EXPR_SET:
        {
            unsigned char bytes[32];
            size_t used = 0;
            long long mask = 0;
            if (evaluate_set_const_bytes(symtab, expr, bytes, sizeof(bytes), &used, &mask, NULL) != 0)
                return 1;
            if (used > 8)
            {
                if (emit_diagnostics)
                    fprintf(stderr, "Error: set literal requires 32-byte storage and cannot be reduced to integer const.\n");
                return 1;
            }
            *out_value = mask;
            return 0;
        }
        case EXPR_RECORD_ACCESS:
        {
            /* Handle qualified constants like UnitName.ConstName */
            /* The field_id is the constant name, look it up directly in the symbol table */
            /* since unit exports are already imported into the current scope */
            char *field_id = expr->expr_data.record_access_data.field_id;
            if (field_id != NULL)
            {
                HashNode_t *node = NULL;
                if (FindSymbol(&node, symtab, field_id) != 0 && node != NULL &&
                    (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
                {
                    *out_value = node->const_int_value;
                    return 0;
                }
                struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
                if (record_expr != NULL)
                {
                    char *owner_name = build_qualified_identifier_from_expr(record_expr);
                    if (owner_name != NULL)
                    {
                        long long enum_value = 0;
                        if (semcheck_resolve_scoped_enum_literal(symtab, owner_name,
                            field_id, &enum_value))
                        {
                            *out_value = enum_value;
                            free(owner_name);
                            return 0;
                        }
                    }

                    if (owner_name != NULL)
                    {
                        size_t qualified_len = strlen(owner_name) + 1 + strlen(field_id) + 1;
                        char *qualified = (char *)malloc(qualified_len);
                        if (qualified != NULL)
                        {
                            snprintf(qualified, qualified_len, "%s.%s", owner_name, field_id);
                            if (FindSymbol(&node, symtab, qualified) != 0 &&
                                node != NULL &&
                                (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
                            {
                                *out_value = node->const_int_value;
                                free(qualified);
                                free(owner_name);
                                return 0;
                            }
                            free(qualified);
                        }
                        free(owner_name);
                        owner_name = NULL;
                    }

                    long long enum_value = 0;
                    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL &&
                        semcheck_resolve_scoped_enum_literal(symtab, record_expr->expr_data.id,
                            field_id, &enum_value))
                    {
                        *out_value = enum_value;
                        return 0;
                    }
                }
            }
            if (emit_diagnostics)
                fprintf(stderr, "Error: qualified constant '%s' is undefined or not a const.\n",
                        field_id ? field_id : "(null)");
            return 1;
        }
        case EXPR_FUNCTION_CALL:
        {
            /* Handle Ord() function for constant expressions */
            char *id = expr->expr_data.function_call_data.id;
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;

            if (id != NULL &&
                (pascal_identifier_equals(id, "Succ") || pascal_identifier_equals(id, "Pred")))
            {
                struct Expression *arg = extract_single_const_arg(args, id);
                long long arg_value = 0;
                long long low = 0;
                long long high = 0;
                long long result = 0;
                int is_succ = pascal_identifier_equals(id, "Succ");

                if (arg == NULL)
                    return 1;
                if (const_fold_int_expr_mode(symtab, arg, &arg_value, emit_diagnostics) != 0)
                    return 1;
                if (!evaluate_const_expr_ordinal_bounds(symtab, arg, &low, &high))
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: %s expects an ordinal constant argument.\n", id);
                    return 1;
                }

                result = is_succ ? (arg_value + 1) : (arg_value - 1);
                if (result < low || result > high)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: %s result out of range for ordinal type.\n", id);
                    return 1;
                }

                *out_value = result;
                return 0;
            }

            if (id != NULL && args != NULL && args->next == NULL)
            {
                HashNode_t *type_node = NULL;
                int found_type = (FindSymbol(&type_node, symtab, id) != 0 &&
                    type_node != NULL && type_node->hash_type == HASHTYPE_TYPE);
                const char *base_id = semcheck_base_type_name(id);
                if (!found_type && base_id != NULL && base_id != id)
                {
                    found_type = (FindSymbol(&type_node, symtab, base_id) != 0 &&
                        type_node != NULL && type_node->hash_type == HASHTYPE_TYPE);
                }

                if (found_type || semcheck_map_builtin_type_name_local(id) != UNKNOWN_TYPE)
                {
                    struct Expression *arg = (struct Expression *)args->cur;
                    long long inner_value = 0;
                    if (const_fold_int_expr_mode(symtab, arg, &inner_value, emit_diagnostics) != 0)
                    {
                        double real_value = 0.0;
                        if (const_fold_real_expr_mode(symtab, arg, &real_value,
                                emit_diagnostics) != 0)
                            return 1;
                        inner_value = (long long)real_value;
                    }
                    *out_value = inner_value;
                    return 0;
                }
            }
            
            if (id != NULL && pascal_identifier_equals(id, "Ord"))
            {
                struct Expression *arg = extract_single_const_arg(args, "Ord");
                if (arg == NULL)
                    return 1;
                
                /* Handle character literal */
                if (arg->type == EXPR_STRING)
                {
                    char *literal = arg->expr_data.string;
                    if (literal == NULL || literal[0] == '\0')
                    {
                        if (emit_diagnostics)
                            fprintf(stderr, "Error: Ord expects a non-empty character literal.\n");
                        return 1;
                    }
                    if (literal[1] != '\0')
                    {
                        if (emit_diagnostics)
                            fprintf(stderr, "Error: Ord expects a single character literal.\n");
                        return 1;
                    }
                    *out_value = (unsigned char)literal[0];
                    return 0;
                }
                /* Handle character code literal */
                else if (arg->type == EXPR_CHAR_CODE)
                {
                    *out_value = (unsigned char)arg->expr_data.char_code;
                    return 0;
                }
                /* Handle boolean literal */
                else if (arg->type == EXPR_BOOL)
                {
                    *out_value = arg->expr_data.bool_value ? 1 : 0;
                    return 0;
                }
                /* Handle integer literal */
                else if (arg->type == EXPR_INUM)
                {
                    *out_value = arg->expr_data.i_num;
                    return 0;
                }
                /* Handle const variable reference */
                else if (arg->type == EXPR_VAR_ID)
                {
                    HashNode_t *node = NULL;
                    if (FindSymbol(&node, symtab, arg->expr_data.id) &&
                        node != NULL &&
                        (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
                    {
                        if (node->const_string_value != NULL &&
                            node->const_string_value[0] != '\0' &&
                            node->const_string_value[1] == '\0')
                        {
                            *out_value = (unsigned char)node->const_string_value[0];
                            return 0;
                        }
                        *out_value = node->const_int_value;
                        return 0;
                    }
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Ord argument %s is not a constant.\n", arg->expr_data.id);
                    return 1;
                }
                /* Handle nested const expressions */
                else
                {
                    long long arg_value;
                    if (const_fold_int_expr_mode(symtab, arg, &arg_value, emit_diagnostics) == 0)
                    {
                        *out_value = arg_value;
                        return 0;
                    }
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Ord argument is not a valid const expression.\n");
                    return 1;
                }
            }
            
            /* Handle High() function for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "High"))
            {
                struct Expression *arg = extract_single_const_arg(args, "High");
                if (arg == NULL)
                    return 1;
                
                /* High expects a type identifier */
                if (arg->type == EXPR_VAR_ID || arg->type == EXPR_RECORD_ACCESS)
                {
                    QualifiedIdent *type_id_ref = build_qualified_ident_from_expr(arg);
                    char *qualified_name = NULL;
                    const char *type_name = NULL;
                    int status = 1;
                    if (type_id_ref != NULL)
                        qualified_name = qualified_ident_join(type_id_ref, ".");
                    if (arg->type == EXPR_VAR_ID)
                        type_name = arg->expr_data.id;
                    if (type_name == NULL)
                        type_name = qualified_name;
                    const char *base_name = (type_id_ref != NULL) ?
                        qualified_ident_last(type_id_ref) :
                        semcheck_base_type_name(type_name);
                    const char *lookup_name = type_name;
                    long long range_low = 0;
                    long long range_high = 0;
                    if ((type_id_ref != NULL &&
                         resolve_range_bounds_for_type_ref(symtab, type_id_ref, &range_low, &range_high)) ||
                        (lookup_name != NULL &&
                         resolve_range_bounds_for_type(symtab, lookup_name, &range_low, &range_high)))
                    {
                        *out_value = range_high;
                        status = 0;
                        goto high_cleanup;
                    }
                    /* Resolve type aliases to base type */
                    const char *resolved = resolve_type_to_base_name(symtab, type_id_ref, lookup_name);
                    if (resolved != NULL)
                        lookup_name = resolved;
                    
                    /* Map common type names to their High values */
                    {
                        long long bounds_low, bounds_high;
                        if (get_builtin_type_bounds(base_name, &bounds_low, &bounds_high))
                        {
                            *out_value = bounds_high;
                            status = 0;
                            goto high_cleanup;
                        }
                    }
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: High(%s) - unsupported type in const expression.\n",
                            qualified_name != NULL ? qualified_name : arg->expr_data.id);
                    status = 1;
high_cleanup:
                    if (qualified_name != NULL)
                        free(qualified_name);
                    qualified_ident_free(type_id_ref);
                    return status;
                }
                if (emit_diagnostics)
                    fprintf(stderr, "Error: High expects a type identifier as argument.\n");
                return 1;
            }
            
            /* Handle Low() function for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "Low"))
            {
                struct Expression *arg = extract_single_const_arg(args, "Low");
                if (arg == NULL)
                    return 1;
                
                /* Low expects a type identifier */
                if (arg->type == EXPR_VAR_ID || arg->type == EXPR_RECORD_ACCESS)
                {
                    QualifiedIdent *type_id_ref = build_qualified_ident_from_expr(arg);
                    char *qualified_name = NULL;
                    const char *type_name = NULL;
                    int status = 1;
                    if (type_id_ref != NULL)
                        qualified_name = qualified_ident_join(type_id_ref, ".");
                    if (arg->type == EXPR_VAR_ID)
                        type_name = arg->expr_data.id;
                    if (type_name == NULL)
                        type_name = qualified_name;
                    const char *base_name = (type_id_ref != NULL) ?
                        qualified_ident_last(type_id_ref) :
                        semcheck_base_type_name(type_name);
                    const char *lookup_name = type_name;
                    long long range_low = 0;
                    long long range_high = 0;
                    if ((type_id_ref != NULL &&
                         resolve_range_bounds_for_type_ref(symtab, type_id_ref, &range_low, &range_high)) ||
                        (lookup_name != NULL &&
                         resolve_range_bounds_for_type(symtab, lookup_name, &range_low, &range_high)))
                    {
                        *out_value = range_low;
                        status = 0;
                        goto low_cleanup;
                    }
                    /* Resolve type aliases to base type */
                    const char *resolved = resolve_type_to_base_name(symtab, type_id_ref, lookup_name);
                    if (resolved != NULL)
                        lookup_name = resolved;
                        
                    /* Map common type names to their Low values */
                    {
                        long long bounds_low, bounds_high;
                        if (get_builtin_type_bounds(base_name, &bounds_low, &bounds_high))
                        {
                            *out_value = bounds_low;
                            status = 0;
                            goto low_cleanup;
                        }
                    }
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Low(%s) - unsupported type in const expression.\n",
                            qualified_name != NULL ? qualified_name : arg->expr_data.id);
                    status = 1;
low_cleanup:
                    if (qualified_name != NULL)
                        free(qualified_name);
                    qualified_ident_free(type_id_ref);
                    return status;
                }
                if (emit_diagnostics)
                    fprintf(stderr, "Error: Low expects a type identifier as argument.\n");
                return 1;
            }
            
            /* Handle SizeOf() function for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "SizeOf"))
            {
                struct Expression *arg = extract_single_const_arg(args, "SizeOf");
                if (arg == NULL)
                    return 1;
                
                /* SizeOf expects a type identifier */
                if (arg->type == EXPR_VAR_ID)
                {
                    QualifiedIdent *type_id_ref = NULL;
                    const char *type_name = arg->expr_data.id;
                    const char *base_name = type_name;
                    if (arg->id_ref != NULL)
                    {
                        type_id_ref = qualified_ident_clone(arg->id_ref);
                        base_name = qualified_ident_last(type_id_ref);
                    }
                    /* Resolve type aliases to base type */
                    const char *resolved = resolve_type_to_base_name(symtab, type_id_ref, type_name);
                    if (resolved != NULL)
                        base_name = resolved;

                    /* Prefer computing from an actual type node (handles nested/local types). */
                    HashNode_t *size_type_node = NULL;
                    if (symtab != NULL)
                    {
                        if (type_id_ref != NULL && type_id_ref->count > 1)
                        {
                            char *qualified_name = qualified_ident_join(type_id_ref, ".");
                            if (qualified_name != NULL)
                            {
                                size_type_node = semcheck_find_preferred_type_node(symtab, qualified_name);
                                free(qualified_name);
                            }
                        }
                        if (size_type_node == NULL && type_name != NULL)
                            size_type_node = semcheck_find_preferred_type_node(symtab, type_name);
                    }
                    if (size_type_node != NULL && size_type_node->hash_type == HASHTYPE_TYPE)
                    {
                        long long computed_size = 0;
                        if (sizeof_from_hashnode(symtab, size_type_node, &computed_size, 0, expr->line_num) == 0)
                        {
                            *out_value = computed_size;
                            qualified_ident_free(type_id_ref);
                            return 0;
                        }
                    }
                        
                    /* Map common type names to their sizes (in bytes) */
                    /* 64-bit types */
                    if (pascal_identifier_equals(base_name, "Int64") ||
                        pascal_identifier_equals(base_name, "QWord") ||
                        pascal_identifier_equals(base_name, "UInt64") ||
                        pascal_identifier_equals(base_name, "Pointer") ||
                        pascal_identifier_equals(base_name, "PChar") ||
                        pascal_identifier_equals(base_name, "Double") ||
                        pascal_identifier_equals(base_name, "Real")) {
                        *out_value = 8LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    /* 32-bit types */
                    if (pascal_identifier_equals(base_name, "LongInt") ||
                        pascal_identifier_equals(base_name, "LongWord") ||
                        pascal_identifier_equals(base_name, "Cardinal") ||
                        pascal_identifier_equals(base_name, "DWord") ||
                        pascal_identifier_equals(base_name, "Integer") ||
                        pascal_identifier_equals(base_name, "Single")) {
                        *out_value = 4LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    /* 16-bit types */
                    if (pascal_identifier_equals(base_name, "SmallInt") ||
                        pascal_identifier_equals(base_name, "Word") ||
                        pascal_identifier_equals(base_name, "WideChar")) {
                        *out_value = 2LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    /* 8-bit types */
                    if (pascal_identifier_equals(base_name, "ShortInt") ||
                        pascal_identifier_equals(base_name, "Byte") ||
                        pascal_identifier_equals(base_name, "Char") ||
                        pascal_identifier_equals(base_name, "AnsiChar") ||
                        pascal_identifier_equals(base_name, "Boolean")) {
                        *out_value = 1LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: SizeOf(%s) - unsupported type in const expression.\n", arg->expr_data.id);
                    qualified_ident_free(type_id_ref);
                    return 1;
                }
                if (emit_diagnostics)
                    fprintf(stderr, "Error: SizeOf expects a type identifier as argument.\n");
                return 1;
            }
            
            /* Handle BitSizeOf() function for constant expressions - returns SizeOf(T) * 8 */
            if (id != NULL && pascal_identifier_equals(id, "BitSizeOf"))
            {
                struct Expression *arg = extract_single_const_arg(args, "BitSizeOf");
                if (arg == NULL)
                    return 1;
                
                /* BitSizeOf expects a type identifier, same as SizeOf */
                if (arg->type == EXPR_VAR_ID)
                {
                    QualifiedIdent *type_id_ref = NULL;
                    const char *type_name = arg->expr_data.id;
                    const char *base_name = type_name;
                    if (arg->id_ref != NULL)
                    {
                        type_id_ref = qualified_ident_clone(arg->id_ref);
                        base_name = qualified_ident_last(type_id_ref);
                    }
                    /* Resolve type aliases to base type */
                    const char *resolved = resolve_type_to_base_name(symtab, type_id_ref, type_name);
                    if (resolved != NULL)
                        base_name = resolved;

                    /* Prefer computing from an actual type node (handles nested/local types). */
                    HashNode_t *size_type_node = NULL;
                    if (symtab != NULL)
                    {
                        if (type_id_ref != NULL && type_id_ref->count > 1)
                        {
                            char *qualified_name = qualified_ident_join(type_id_ref, ".");
                            if (qualified_name != NULL)
                            {
                                size_type_node = semcheck_find_preferred_type_node(symtab, qualified_name);
                                free(qualified_name);
                            }
                        }
                        if (size_type_node == NULL && type_name != NULL)
                            size_type_node = semcheck_find_preferred_type_node(symtab, type_name);
                    }
                    if (size_type_node != NULL && size_type_node->hash_type == HASHTYPE_TYPE)
                    {
                        long long computed_size = 0;
                        if (sizeof_from_hashnode(symtab, size_type_node, &computed_size, 0, expr->line_num) == 0)
                        {
                            *out_value = computed_size * 8;
                            qualified_ident_free(type_id_ref);
                            return 0;
                        }
                    }
                        
                    /* Map common type names to their sizes in bits */
                    /* 64-bit types */
                    if (pascal_identifier_equals(base_name, "Int64") ||
                        pascal_identifier_equals(base_name, "QWord") ||
                        pascal_identifier_equals(base_name, "UInt64") ||
                        pascal_identifier_equals(base_name, "Pointer") ||
                        pascal_identifier_equals(base_name, "PChar") ||
                        pascal_identifier_equals(base_name, "Double") ||
                        pascal_identifier_equals(base_name, "Real")) {
                        *out_value = 64LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    /* 32-bit types */
                    if (pascal_identifier_equals(base_name, "LongInt") ||
                        pascal_identifier_equals(base_name, "LongWord") ||
                        pascal_identifier_equals(base_name, "Cardinal") ||
                        pascal_identifier_equals(base_name, "DWord") ||
                        pascal_identifier_equals(base_name, "Integer") ||
                        pascal_identifier_equals(base_name, "Single")) {
                        *out_value = 32LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    /* 16-bit types */
                    if (pascal_identifier_equals(base_name, "SmallInt") ||
                        pascal_identifier_equals(base_name, "Word") ||
                        pascal_identifier_equals(base_name, "WideChar")) {
                        *out_value = 16LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    /* 8-bit types */
                    if (pascal_identifier_equals(base_name, "ShortInt") ||
                        pascal_identifier_equals(base_name, "Byte") ||
                        pascal_identifier_equals(base_name, "Char") ||
                        pascal_identifier_equals(base_name, "AnsiChar") ||
                        pascal_identifier_equals(base_name, "Boolean")) {
                        *out_value = 8LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: BitSizeOf(%s) - unsupported type in const expression.\n", arg->expr_data.id);
                    qualified_ident_free(type_id_ref);
                    return 1;
                }
                if (emit_diagnostics)
                    fprintf(stderr, "Error: BitSizeOf expects a type identifier as argument.\n");
                return 1;
            }

            /* Handle Chr() function for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "Chr"))
            {
                struct Expression *arg = extract_single_const_arg(args, "Chr");
                if (arg == NULL)
                    return 1;
                
                /* Evaluate the argument as a const expression */
                long long char_code;
                if (const_fold_int_expr_mode(symtab, arg, &char_code, emit_diagnostics) != 0)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Chr argument must be a const expression.\n");
                    return 1;
                }
                
                /* Validate the character code is in valid range (0..255) */
                if (char_code < 0 || char_code > 255)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Chr argument %lld is out of valid range (0..255).\n", char_code);
                    return 1;
                }
                
                *out_value = char_code;
                return 0;
            }
            
            /* Handle Pointer() typecast for constant expressions (FPC bootstrap dl.pp) */
            if (id != NULL && pascal_identifier_equals(id, "Pointer"))
            {
                struct Expression *arg = extract_single_const_arg(args, "Pointer");
                if (arg == NULL)
                    return 1;
                
                /* Evaluate the argument as a const expression */
                long long ptr_value;
                if (const_fold_int_expr_mode(symtab, arg, &ptr_value, emit_diagnostics) != 0)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Pointer argument must be a const expression.\n");
                    return 1;
                }
                
                *out_value = ptr_value;
                return 0;
            }
            
            /* Handle PtrInt() and PtrUInt() typecasts for constant expressions */
            if (id != NULL && (pascal_identifier_equals(id, "PtrInt") || pascal_identifier_equals(id, "PtrUInt")))
            {
                struct Expression *arg = extract_single_const_arg(args, id);
                if (arg == NULL)
                    return 1;
                
                /* Evaluate the argument as a const expression */
                long long int_value;
                if (const_fold_int_expr_mode(symtab, arg, &int_value, emit_diagnostics) != 0)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: %s argument must be a const expression.\n", id);
                    return 1;
                }
                
                *out_value = int_value;
                return 0;
            }

            /* Handle Trunc() for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "Trunc"))
            {
                struct Expression *arg = extract_single_const_arg(args, "Trunc");
                if (arg == NULL)
                    return 1;
                double real_value = 0.0;
                if (const_fold_real_expr_mode(symtab, arg, &real_value, emit_diagnostics) == 0)
                {
                    *out_value = (long long)real_value;
                    return 0;
                }
                long long int_value = 0;
                if (const_fold_int_expr_mode(symtab, arg, &int_value, emit_diagnostics) == 0)
                {
                    *out_value = int_value;
                    return 0;
                }
                if (emit_diagnostics)
                    fprintf(stderr, "Error: Trunc argument must be a const expression.\n");
                return 1;
            }
            
            /* Handle Cardinal, LongWord, DWord, QWord, Int64, UInt64, HRESULT and other integer typecasts
             * for constant expressions (FPC bootstrap: Cardinal(not Cardinal(0)), HRESULT($80020004)) */
            if (id != NULL && (pascal_identifier_equals(id, "Cardinal") ||
                               pascal_identifier_equals(id, "LongWord") ||
                               pascal_identifier_equals(id, "DWord") ||
                               pascal_identifier_equals(id, "QWord") ||
                               pascal_identifier_equals(id, "Int64") ||
                               pascal_identifier_equals(id, "UInt64") ||
                               pascal_identifier_equals(id, "NativeInt") ||
                               pascal_identifier_equals(id, "NativeUInt") ||
                               pascal_identifier_equals(id, "SizeInt") ||
                               pascal_identifier_equals(id, "SizeUInt") ||
                               pascal_identifier_equals(id, "ShortInt") ||
                               pascal_identifier_equals(id, "SmallInt") ||
                               pascal_identifier_equals(id, "LongInt") ||
                               pascal_identifier_equals(id, "HRESULT")))
            {
                struct Expression *arg = extract_single_const_arg(args, id);
                if (arg == NULL)
                    return 1;
                
                /* Evaluate the argument as a const expression */
                long long int_value;
                if (const_fold_int_expr_mode(symtab, arg, &int_value, emit_diagnostics) != 0)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: %s argument must be a const expression.\n", id);
                    return 1;
                }
                
                /* Apply appropriate mask for the target type */
                if (pascal_identifier_equals(id, "Cardinal") ||
                    pascal_identifier_equals(id, "LongWord") ||
                    pascal_identifier_equals(id, "DWord"))
                {
                    *out_value = (unsigned int)(int_value & 0xFFFFFFFFULL);
                }
                else if (pascal_identifier_equals(id, "ShortInt"))
                {
                    *out_value = (signed char)(int_value & 0xFF);
                }
                else if (pascal_identifier_equals(id, "SmallInt"))
                {
                    *out_value = (short)(int_value & 0xFFFF);
                }
                else if (pascal_identifier_equals(id, "LongInt") ||
                         pascal_identifier_equals(id, "HRESULT"))
                {
                    /* LongInt and HRESULT are 32-bit signed integers */
                    *out_value = (int)(int_value & 0xFFFFFFFFULL);
                }
                else
                {
                    /* QWord, Int64, UInt64, NativeInt, NativeUInt, SizeInt, SizeUInt - 64-bit */
                    *out_value = int_value;
                }
                return 0;
            }

            if (id != NULL && args != NULL && args->next == NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindSymbol(&type_node, symtab, id) != 0 && type_node != NULL && type_node->type != NULL)
                {
                    int legacy_tag = semcheck_tag_from_kgpc(type_node->type);
                    if (legacy_tag == UNKNOWN_TYPE && type_node->type->type_alias != NULL)
                        legacy_tag = type_node->type->type_alias->base_type;
                    if (legacy_tag != UNKNOWN_TYPE)
                    {
                        struct Expression *arg = (struct Expression *)args->cur;
                        long long int_value;
                        if (arg == NULL)
                        {
                            if (emit_diagnostics)
                                fprintf(stderr, "Error: %s argument is NULL.\n", id);
                            return 1;
                        }
                        if (const_fold_int_expr_mode(symtab, arg, &int_value, emit_diagnostics) != 0)
                        {
                            if (emit_diagnostics)
                                fprintf(stderr, "Error: %s argument must be a const expression.\n", id);
                            return 1;
                        }
                        if (type_node->type->type_alias != NULL)
                        {
                            int storage_size = type_node->type->type_alias->storage_size;
                            if (storage_size > 0 && storage_size < 8)
                            {
                                unsigned long long mask = (1ULL << (storage_size * 8)) - 1ULL;
                                int_value = (long long)((unsigned long long)int_value & mask);
                            }
                        }
                        *out_value = int_value;
                        return 0;
                    }
                }
            }

            if (emit_diagnostics)
            {
                if (id != NULL)
                    fprintf(stderr, "Error: const expression uses unsupported function %s on line %d.\n", id, expr->line_num);
                fprintf(stderr, "Error: only Ord(), High(), Low(), SizeOf(), BitSizeOf(), Chr(), Trunc(), and integer typecasts are supported in const expressions.\n");
            }
            return 1;
        }
        case EXPR_RELOP:
        {
            /* Handle NOT operator for constant expressions (bitwise NOT) */
            /* FPC bootstrap uses: Cardinal(not Cardinal(0)) */
            if (expr->expr_data.relop_data.type == NOT)
            {
                /* NOT is a unary operator - right operand is NULL */
                struct Expression *operand = expr->expr_data.relop_data.left;
                if (operand == NULL)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: NOT operator requires an operand.\n");
                    return 1;
                }
                
                long long operand_value;
                if (const_fold_int_expr_mode(symtab, operand, &operand_value, emit_diagnostics) != 0)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: NOT operand must be a const expression.\n");
                    return 1;
                }
                
                /* Bitwise NOT */
                *out_value = ~operand_value;
                return 0;
            }
            /* Relational operators (comparisons) for const expressions */
            if (expr->expr_data.relop_data.left != NULL && expr->expr_data.relop_data.right != NULL)
            {
                long long left_val, right_val;
                if (const_fold_int_expr_mode(symtab, expr->expr_data.relop_data.left, &left_val,
                        emit_diagnostics) != 0)
                    return 1;
                if (const_fold_int_expr_mode(symtab, expr->expr_data.relop_data.right, &right_val,
                        emit_diagnostics) != 0)
                    return 1;
                
                switch (expr->expr_data.relop_data.type)
                {
                    case EQ: *out_value = (left_val == right_val) ? 1 : 0; return 0;
                    case NE: *out_value = (left_val != right_val) ? 1 : 0; return 0;
                    case LT: *out_value = (left_val < right_val) ? 1 : 0; return 0;
                    case LE: *out_value = (left_val <= right_val) ? 1 : 0; return 0;
                    case GT: *out_value = (left_val > right_val) ? 1 : 0; return 0;
                    case GE: *out_value = (left_val >= right_val) ? 1 : 0; return 0;
                    default:
                        break;
                }
            }
            if (emit_diagnostics)
                fprintf(stderr, "Error: unsupported relational operator in const expression.\n");
            return 1;
        }
        case EXPR_ADDR:
        {
            /* Handle @Record(nil^).field as compile-time offsetof() */
            struct Expression *inner = expr->expr_data.addr_data.expr;
            if (inner == NULL || inner->type != EXPR_RECORD_ACCESS)
                break;

            char *field_id = inner->expr_data.record_access_data.field_id;
            struct Expression *base_expr = inner->expr_data.record_access_data.record_expr;
            if (field_id == NULL || base_expr == NULL)
                break;

            /* The base expression must be a nil pointer dereference typecast:
             * Either EXPR_FUNCTION_CALL(TypeName, args=[EXPR_POINTER_DEREF(EXPR_NIL)])
             * or EXPR_TYPECAST(TypeName, expr=EXPR_POINTER_DEREF(EXPR_NIL)) */
            const char *type_name = NULL;
            struct Expression *deref_check = NULL;

            if (base_expr->type == EXPR_FUNCTION_CALL)
            {
                ListNode_t *args = base_expr->expr_data.function_call_data.args_expr;
                if (args != NULL && args->next == NULL && args->cur != NULL)
                {
                    deref_check = (struct Expression *)args->cur;
                    type_name = base_expr->expr_data.function_call_data.id;
                }
            }
            else if (base_expr->type == EXPR_TYPECAST)
            {
                deref_check = base_expr->expr_data.typecast_data.expr;
                type_name = base_expr->expr_data.typecast_data.target_type_id;
            }

            if (type_name == NULL || deref_check == NULL)
                break;

            /* Verify that the argument is nil^ (EXPR_POINTER_DEREF(EXPR_NIL)) */
            if (deref_check->type != EXPR_POINTER_DEREF ||
                deref_check->expr_data.pointer_deref_data.pointer_expr == NULL ||
                deref_check->expr_data.pointer_deref_data.pointer_expr->type != EXPR_NIL)
                break;

            /* Look up the record type in the symbol table */
            HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_name);
            if (type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
            {
                /* Try stripping unit prefix (e.g. "HeapTracer.Node" -> "Node") */
                const char *dot = strrchr(type_name, '.');
                if (dot != NULL)
                {
                    type_node = semcheck_find_preferred_type_node(symtab, dot + 1);
                }
                if (type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
                    break;
            }

            struct RecordType *record = hashnode_get_record_type(type_node);
            if (record == NULL)
                break;

            /* Compute field offset using resolve_record_field */
            struct RecordField *field_desc = NULL;
            long long field_offset = 0;
            if (resolve_record_field(symtab, record, field_id, &field_desc,
                                     &field_offset, expr->line_num, 1) == 0 &&
                field_desc != NULL)
            {
                *out_value = field_offset;
                return 0;
            }
            break;
        }
        default:
            break;
    }

    if (emit_diagnostics)
        fprintf(stderr, "Error: unsupported const expression.\n");
    return 1;
}

static int const_fold_int_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value)
{
    return const_fold_int_expr_mode(symtab, expr, out_value, 0);
}

static int const_fold_real_expr(SymTab_t *symtab, struct Expression *expr, double *out_value)
{
    return const_fold_real_expr_mode(symtab, expr, out_value, 0);
}

static int evaluate_const_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value)
{
    return const_fold_int_expr_mode(symtab, expr, out_value, 1);
}

static int evaluate_real_const_expr(SymTab_t *symtab, struct Expression *expr, double *out_value)
{
    return const_fold_real_expr_mode(symtab, expr, out_value, 1);
}

/* The main function for checking a tree */
/* Return values:
    0       -> Check successful
    -1      -> Check successful with warnings
    >= 1    -> Check failed with n errors
*/
/* Create and initialize a SymTab_t with a global scope and builtins.
 * This is extracted from start_semcheck() so it can be called earlier
 * (before unit loading) for per-unit semantic checking. */
SymTab_t *semcheck_init_symtab(void)
{
    double t0 = 0.0;
    SymTab_t *symtab = InitSymTab();
    EnterScope(symtab, 0);  /* Global pre-program scope; builtins live in
                             * unit_scopes[System]->table, not in this scope. */
    if (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL)
        t0 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
    semcheck_add_builtins(symtab);
    if (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL) {
        double t1 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
        fprintf(stderr, "[timing] semcheck_add_builtins: %.2f ms\n", t1 - t0);
    }
    return symtab;
}

/* Wire unit scope dependencies from the unit_registry into the scope tree.
 * Each unit scope gets dep_scopes[] populated from its uses clause,
 * so FindIdent_Tree can search dependencies without the flat-model fallback.
 * Extracted from start_semcheck() so it can be called after all units are loaded
 * but before the final program-level semcheck. */
void wire_all_unit_scope_deps(SymTab_t *symtab)
{
    int num_units = unit_registry_count();
    for (int u = 1; u <= num_units; u++)
    {
        ScopeNode *unit_scope = GetOrCreateUnitScope(symtab, u);
        if (unit_scope == NULL) continue;
        /* Always add System as a dep (every unit implicitly uses System) */
        int sys_idx = unit_registry_add("System");
        if (sys_idx > 0 && sys_idx != u)
        {
            ScopeNode *sys_scope = GetOrCreateUnitScope(symtab, sys_idx);
            if (sys_scope != NULL)
                ScopeAddDependencyTransitive(unit_scope, sys_scope, 1);
        }
        /* Add explicit deps from uses clause */
        for (int d = 1; d <= num_units; d++)
        {
            if (d == u) continue;
            if (unit_registry_is_iface_dep(u, d))
            {
                ScopeNode *dep_scope = GetOrCreateUnitScope(symtab, d);
                if (dep_scope != NULL)
                    ScopeAddDependencyTransitive(unit_scope, dep_scope, 1);
            }
            else if (unit_registry_is_dep(u, d))
            {
                ScopeNode *dep_scope = GetOrCreateUnitScope(symtab, d);
                if (dep_scope != NULL)
                    ScopeAddDependencyTransitive(unit_scope, dep_scope, 0);
            }
        }
    }

    /* Second pass: propagate transitive interface deps that were missed
     * because a dependency hadn't been fully wired yet in the first pass.
     * Units are numbered in load order, but uses-chains can reference
     * higher-numbered units.  After pass 1 every unit has its direct
     * deps; pass 2 copies any newly-visible transitive deps. */
    for (int u = 1; u <= num_units; u++)
    {
        ScopeNode *unit_scope = GetOrCreateUnitScope(symtab, u);
        if (unit_scope == NULL) continue;
        for (int d = 1; d <= num_units; d++)
        {
            if (d == u) continue;
            if (unit_registry_is_iface_dep(u, d))
            {
                ScopeNode *dep_scope = GetOrCreateUnitScope(symtab, d);
                if (dep_scope != NULL)
                    ScopeAddDependencyTransitive(unit_scope, dep_scope, 1);
            }
        }
    }

    /* NOTE: We do NOT add unit deps to current_scope (the global pre-program
     * BLOCK scope) here. The program-level scope gets its deps wired separately
     * in semcheck_program / semcheck_unit via the EnterScope(0) call.
     * Adding deps here would cause duplicate symbol searches because
     * any scope with num_deps > 0 now triggers dep search. */
}

SymTab_t *start_semcheck(Tree_t *parse_tree, int *sem_result)
{
    return start_semcheck_with_symtab(NULL, parse_tree, sem_result);
}

SymTab_t *start_semcheck_with_symtab(SymTab_t *existing_symtab, Tree_t *parse_tree, int *sem_result)
{
    SymTab_t *symtab;
    int return_val;
    double t0 = 0.0;

    assert(parse_tree != NULL);
    assert(sem_result != NULL);

    /* Reset warning counter at start of semantic check */
    g_semcheck_warning_count = 0;

    if (g_semcheck_source_path != NULL)
        file_to_parse = g_semcheck_source_path;
    if (g_semcheck_source_buffer != NULL)
    {
        if (preprocessed_source != NULL)
            free(preprocessed_source);
        preprocessed_source = strdup(g_semcheck_source_buffer);
        preprocessed_length = g_semcheck_source_length;
    }

    if (existing_symtab != NULL)
        symtab = existing_symtab;
    else
        symtab = semcheck_init_symtab();

    wire_all_unit_scope_deps(symtab);

    if (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL)
        t0 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
    if (parse_tree->type == TREE_UNIT) {
        return_val = semcheck_unit(symtab, parse_tree);
    } else {
        return_val = semcheck_program(symtab, parse_tree);
    }
    if (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL) {
        double t1 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
        fprintf(stderr, "[timing] semcheck_program/unit: %.2f ms\n", t1 - t0);
    }

    if(return_val > 0)
        fprintf(stderr, "\nCheck failed with %d error(s)!\n\n", return_val);
    else if (g_semcheck_warning_count > 0)
        fprintf(stderr, "\nCheck successful with %d warning(s)!\n\n", g_semcheck_warning_count);
    else
        fprintf(stderr, "\nCheck successful!\n\n");

    /* Return -1 if there were warnings but no errors (per SemCheck.h API) */
    if (return_val == 0 && g_semcheck_warning_count > 0)
        return_val = -1;

    *sem_result = return_val;
    return symtab;
}

/* Register anonymous enum values from record field declarations.
 * For fields like `kind: (vInteger, vString, vNone)`, the enum values
 * must be visible in the enclosing scope. */
static int register_record_field_enum_literals(SymTab_t *symtab, struct RecordType *record)
{
    if (symtab == NULL || record == NULL)
        return 0;

    int errors = 0;
    ListNode_t *field_node = record->fields;
    while (field_node != NULL)
    {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)field_node->cur;
            if (field->enum_literals != NULL)
            {
                KgpcType *enum_type = create_primitive_type(ENUM_TYPE);
                int ordinal = 0;
                ListNode_t *lit = field->enum_literals;
                while (lit != NULL)
                {
                    if (lit->cur != NULL)
                    {
                        char *name = (char *)lit->cur;
                        HashNode_t *existing = NULL;
                        if (FindSymbol(&existing, symtab, name) == 0)
                        {
                            if (PushConstOntoScope_Typed(symtab, name, ordinal, enum_type) > 0)
                                ++errors;
                        }
                    }
                    ++ordinal;
                    lit = lit->next;
                }
                if (enum_type != NULL)
                    kgpc_type_release(enum_type);
            }
        }
        else if (field_node->type == LIST_VARIANT_PART && field_node->cur != NULL)
        {
            struct VariantPart *variant = (struct VariantPart *)field_node->cur;
            /* Check tag field for anonymous enum */
            if (variant->tag_field != NULL && variant->tag_field->enum_literals != NULL)
            {
                KgpcType *enum_type = create_primitive_type(ENUM_TYPE);
                int ordinal = 0;
                ListNode_t *lit = variant->tag_field->enum_literals;
                while (lit != NULL)
                {
                    if (lit->cur != NULL)
                    {
                        char *name = (char *)lit->cur;
                        HashNode_t *existing = NULL;
                        if (FindSymbol(&existing, symtab, name) == 0)
                        {
                            if (PushConstOntoScope_Typed(symtab, name, ordinal, enum_type) > 0)
                                ++errors;
                        }
                    }
                    ++ordinal;
                    lit = lit->next;
                }
                if (enum_type != NULL)
                    kgpc_type_release(enum_type);
            }
            /* Recurse into variant branches */
            ListNode_t *branch_node = variant->branches;
            while (branch_node != NULL)
            {
                if (branch_node->type == LIST_VARIANT_BRANCH && branch_node->cur != NULL)
                {
                    struct VariantBranch *branch = (struct VariantBranch *)branch_node->cur;
                    /* Create a temporary RecordType to recurse */
                    struct RecordType temp_rec;
                    memset(&temp_rec, 0, sizeof(temp_rec));
                    temp_rec.fields = branch->members;
                    errors += register_record_field_enum_literals(symtab, &temp_rec);
                }
                branch_node = branch_node->next;
            }
        }
        field_node = field_node->next;
    }
    return errors;
}

/* Pushes a bunch of type declarations onto the current scope */
static int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls)
{
    if (symtab == NULL)
        return 0;

    int errors = 0;
    ListNode_t *cur = type_decls;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;
            if (tree->type == TREE_TYPE_DECL &&
                tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
            {
                struct TypeAlias *alias_info = &tree->tree_data.type_decl_data.info.alias;
                if (alias_info != NULL && alias_info->is_enum && alias_info->enum_literals != NULL)
                {
                    if (kgpc_getenv("KGPC_DEBUG_ENUM_LITERALS") != NULL)
                    {
                        fprintf(stderr, "[KGPC] enum predeclare %s scoped=%d line=%d\n",
                            tree->tree_data.type_decl_data.id != NULL ? tree->tree_data.type_decl_data.id : "<anon>",
                            alias_info->enum_is_scoped,
                            tree->line_num);
                    }
                    /* Create ONE shared KgpcType for this enum type if not already created */
                    if (alias_info->kgpc_type == NULL)
                    {
                        alias_info->kgpc_type = create_primitive_type(ENUM_TYPE);
                        if (alias_info->kgpc_type == NULL)
                        {
                            fprintf(stderr, "Error: Failed to create enum type for %s\n",
                                    tree->tree_data.type_decl_data.id);
                            ++errors;
                        }
                    }
                    if (alias_info->kgpc_type != NULL &&
                        alias_info->kgpc_type->type_alias == NULL)
                    {
                        kgpc_type_set_type_alias(alias_info->kgpc_type, alias_info);
                    }

                    /* Unit declarations live in their unit scope so subprogram bodies
                     * see them via the real parent/dependency edges. */
                    ScopeNode *saved_scope_for_enum_type = semcheck_switch_to_unit_scope(
                        symtab, tree->tree_data.type_decl_data.source_unit_index);

                    /* Predeclare the enum type itself so consts can reference it
                     * before full type processing (e.g., array[TEnum] in consts). */
                    if (tree->tree_data.type_decl_data.id != NULL)
                    {
                        HashNode_t *existing_type = FindIdentInCurrentScope(symtab,
                            tree->tree_data.type_decl_data.id);
                        if (existing_type == NULL)
                        {
                            PushTypeOntoScope_Typed(symtab,
                                tree->tree_data.type_decl_data.id, alias_info->kgpc_type);
                            HashNode_t *node = NULL;
                            if (FindSymbol(&node, symtab, tree->tree_data.type_decl_data.id) != 0 &&
                                node != NULL)
                            {
                                mark_hashnode_unit_info(symtab, node,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(node,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                        }
                    }

                    /* Scoped enums expose values only through qualified lookup (TEnum.Value),
                     * so do not inject their literals as global constants in this scope. */
                    if (alias_info->kgpc_type != NULL && !alias_info->enum_is_scoped)
                    {
                        int unit_idx = tree->tree_data.type_decl_data.source_unit_index;
                        ScopeNode *saved_scope_for_enum_literals =
                            semcheck_switch_to_unit_scope(symtab, unit_idx);

                        int ordinal = 0;
                        ListNode_t *literal_node = alias_info->enum_literals;
                        while (literal_node != NULL)
                        {
                            if (literal_node->cur != NULL)
                            {
                                char *literal_name = (char *)literal_node->cur;

                                /* Check for collisions in the target table. */
                                HashTable_t *target_table = SymTab_GetTargetTable(symtab);
                                HashNode_t *existing = FindIdentInTable(target_table, literal_name);
                                if (existing != NULL)
                                {
                                    /* Allow local enum literals to shadow imported unit literals. */
                                    if (!tree->tree_data.type_decl_data.defined_in_unit)
                                    {
                                        if (existing->type != NULL && existing->type != alias_info->kgpc_type)
                                            kgpc_type_release(existing->type);
                                        existing->is_constant = 1;
                                        existing->const_int_value = ordinal;
                                        existing->type = alias_info->kgpc_type;
                                        existing->defined_in_unit = 0;
                                        kgpc_type_retain(existing->type);
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* Imported unit collisions are allowed: keep first visible literal. */
                                    if (tree->tree_data.type_decl_data.defined_in_unit)
                                    {
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* If it exists as a constant with the same value, skip silently */
                                    if (existing->is_constant && existing->const_int_value == ordinal)
                                    {
                                        /* Same enum literal from prelude - not an error */
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* Different value in local declarations is a real conflict. */
                                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, 
                                        "Error on line %d, redeclaration of enum literal %s with different value!\n",
                                        tree->line_num, literal_name);
                                    ++errors;
                                    literal_node = literal_node->next;
                                    ++ordinal;
                                    continue;
                                }
                                
                                /* Use typed API with shared enum KgpcType - all literals reference same type */
                                if (PushConstOntoScope_Typed(symtab, literal_name, ordinal, alias_info->kgpc_type) > 0)
                                {
                                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, 
                                            "Error on line %d, redeclaration of enum literal %s!\n",
                                            tree->line_num, literal_name);
                                    ++errors;
                                }
                                else if (tree->tree_data.type_decl_data.defined_in_unit)
                                {
                                    HashNode_t *literal_node_entry = FindIdentInTable(
                                        target_table, literal_name);
                                    if (literal_node_entry != NULL)
                                    {
                                        literal_node_entry->defined_in_unit = 1;
                                        literal_node_entry->unit_is_public =
                                            tree->tree_data.type_decl_data.unit_is_public ? 1 : 0;
                                        mark_hashnode_source_unit(literal_node_entry, unit_idx);
                                    }
                                }
                            }
                            ++ordinal;
                            literal_node = literal_node->next;
                        }
                        semcheck_restore_scope(symtab, saved_scope_for_enum_literals);
                        /* KgpcType is owned by TypeAlias, will be cleaned up when tree is destroyed */
                    }
                    semcheck_restore_scope(symtab, saved_scope_for_enum_type);
                }
                /* Also handle set types with inline anonymous enum: set of (val1, val2, ...) */
                if (alias_info != NULL && alias_info->is_enum_set && alias_info->inline_enum_values != NULL)
                {
                    /* Create a local KgpcType for the inline enum values (not stored in alias).
                     * The set type itself remains SET_TYPE, but the enum values need their own type. */
                    KgpcType *inline_enum_type = create_primitive_type(ENUM_TYPE);
                    if (inline_enum_type == NULL)
                    {
                        fprintf(stderr, "Error: Failed to create inline enum type for set %s\n",
                                tree->tree_data.type_decl_data.id);
                        ++errors;
                    }
                    else
                    {
                        int unit_idx = tree->tree_data.type_decl_data.source_unit_index;
                        ScopeNode *saved_scope_for_inline_enum_literals =
                            semcheck_switch_to_unit_scope(symtab, unit_idx);
                        int ordinal = 0;
                        ListNode_t *literal_node = alias_info->inline_enum_values;
                        while (literal_node != NULL)
                        {
                            if (literal_node->cur != NULL)
                            {
                                char *literal_name = (char *)literal_node->cur;
                                
                                /* Check for collisions in the current scope only (allow shadowing). */
                                HashNode_t *existing = FindIdentInTable(
                                    symtab->current_scope->table, literal_name);
                                if (existing != NULL)
                                {
                                    /* Allow local enum literals to shadow imported unit literals. */
                                    if (!tree->tree_data.type_decl_data.defined_in_unit)
                                    {
                                        if (existing->type != NULL && existing->type != inline_enum_type)
                                            kgpc_type_release(existing->type);
                                        existing->is_constant = 1;
                                        existing->const_int_value = ordinal;
                                        existing->type = inline_enum_type;
                                        existing->defined_in_unit = 0;
                                        kgpc_type_retain(existing->type);
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    if (tree->tree_data.type_decl_data.defined_in_unit)
                                    {
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* If it exists as a constant with the same value, skip silently */
                                    if (existing->is_constant && existing->const_int_value == ordinal)
                                    {
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* Different value in local declarations is a real conflict. */
                                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, 
                                        "Error on line %d, redeclaration of inline enum literal %s with different value!\n",
                                        tree->line_num, literal_name);
                                    ++errors;
                                    literal_node = literal_node->next;
                                    ++ordinal;
                                    continue;
                                }
                                
                                /* Use typed API with shared enum KgpcType */
                                if (PushConstOntoScope_Typed(symtab, literal_name, ordinal, inline_enum_type) > 0)
                                {
                                    semcheck_error_with_context_at(tree->line_num, 0, tree->source_index, 
                                            "Error on line %d, redeclaration of inline enum literal %s!\n",
                                            tree->line_num, literal_name);
                                    ++errors;
                                }
                                else if (tree->tree_data.type_decl_data.defined_in_unit)
                                {
                                    HashNode_t *literal_node_entry = FindIdentInTable(
                                        symtab->current_scope->table, literal_name);
                                    if (literal_node_entry != NULL)
                                    {
                                        literal_node_entry->defined_in_unit = 1;
                                        literal_node_entry->unit_is_public =
                                            tree->tree_data.type_decl_data.unit_is_public ? 1 : 0;
                                        mark_hashnode_source_unit(literal_node_entry, unit_idx);
                                    }
                                }
                            }
                            ++ordinal;
                            literal_node = literal_node->next;
                        }
                        semcheck_restore_scope(symtab, saved_scope_for_inline_enum_literals);
                        /* The enum constants now hold references to inline_enum_type, so we can release our ref */
                        destroy_kgpc_type(inline_enum_type);
                    }
                }
            }
            /* Handle record types with fields that have anonymous enum types */
            if (tree->type == TREE_TYPE_DECL &&
                tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
            {
                struct RecordType *record = tree->tree_data.type_decl_data.info.record;
                if (record != NULL)
                    errors += register_record_field_enum_literals(symtab, record);
            }
        }
        cur = cur->next;
    }

    return errors;
}
