static int map_relop_tag(int tag) {
    switch (tag) {
    case PASCAL_T_EQ: return EQ;
    case PASCAL_T_NE: return NE;
    case PASCAL_T_LT: return LT;
    case PASCAL_T_LE: return LE;
    case PASCAL_T_GT: return GT;
    case PASCAL_T_GE: return GE;
    case PASCAL_T_IN: return IN;
    case PASCAL_T_AND: return AND;
    case PASCAL_T_OR:  return OR;
    default: return UNKNOWN_TYPE;
    }
}

static int map_addop_tag(int tag) {
    switch (tag) {
    case PASCAL_T_ADD: return PLUS;
    case PASCAL_T_SUB: return MINUS;
    case PASCAL_T_OR:  return OR;
    default: return UNKNOWN_TYPE;
    }
}

static int map_mulop_tag(int tag) {
    switch (tag) {
    case PASCAL_T_MUL: return STAR;
    case PASCAL_T_DIV: return SLASH;
    case PASCAL_T_INTDIV: return DIV;
    case PASCAL_T_MOD: return MOD;
    case PASCAL_T_POWER: return POWER;
    case PASCAL_T_AND: return AND;
    case PASCAL_T_XOR: return XOR;
    case PASCAL_T_SHL: return SHL;
    case PASCAL_T_SHR: return SHR;
    case PASCAL_T_ROL: return ROL;
    case PASCAL_T_ROR: return ROR;
    default: return UNKNOWN_TYPE;
    }
}

static void append_asm_line(char **buffer, size_t *length, const char *line) {
    if (line == NULL)
        return;
    size_t line_len = strlen(line);
    size_t new_len = *length + line_len + 1; /* newline */
    char *tmp = realloc(*buffer, new_len + 1);
    if (tmp == NULL)
        return;
    memcpy(tmp + *length, line, line_len);
    tmp[*length + line_len] = '\n';
    tmp[new_len] = '\0';
    *buffer = tmp;
    *length = new_len;
}

static void collect_asm_lines(ast_t *node, char **buffer, size_t *length) {
    for (ast_t *cur = node; cur != NULL && cur != ast_nil; cur = cur->next) {
        if (cur->sym != NULL && cur->sym->name != NULL)
            append_asm_line(buffer, length, cur->sym->name);
        if (cur->child != NULL)
            collect_asm_lines(cur->child, buffer, length);
    }
}

static char *collect_asm_text(ast_t *block_node) {
    char *buffer = NULL;
    size_t length = 0;
    collect_asm_lines(block_node, &buffer, &length);
    return buffer;
}

static int extract_constant_int(struct Expression *expr, long long *out_value) {
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type) {
    case EXPR_INUM:
        *out_value = expr->expr_data.i_num;
        return 0;
    case EXPR_CHAR_CODE:
        *out_value = expr->expr_data.char_code;
        return 0;
    case EXPR_STRING:
        /* Handle single-character string literals as char values */
        if (expr->expr_data.string != NULL && strlen(expr->expr_data.string) == 1) {
            *out_value = (unsigned char)expr->expr_data.string[0];
            return 0;
        }
        return 1;
    case EXPR_SIGN_TERM:
        if (extract_constant_int(expr->expr_data.sign_term, out_value) != 0)
            return 1;
        {
            unsigned long long bits = (unsigned long long)(*out_value);
            bits = 0ULL - bits;
            *out_value = (long long)bits;
        }
        return 0;
    case EXPR_ADDOP: {
        long long left_value = 0;
        long long right_value = 0;
        if (extract_constant_int(expr->expr_data.addop_data.left_expr, &left_value) != 0)
            return 1;
        if (extract_constant_int(expr->expr_data.addop_data.right_term, &right_value) != 0)
            return 1;
        if (expr->expr_data.addop_data.addop_type == PLUS)
            *out_value = left_value + right_value;
        else if (expr->expr_data.addop_data.addop_type == MINUS)
            *out_value = left_value - right_value;
        else
            return 1;
        return 0;
    }
    case EXPR_MULOP: {
        long long left_value = 0;
        long long right_value = 0;
        if (extract_constant_int(expr->expr_data.mulop_data.left_term, &left_value) != 0)
            return 1;
        if (extract_constant_int(expr->expr_data.mulop_data.right_factor, &right_value) != 0)
            return 1;
        if (expr->expr_data.mulop_data.mulop_type == STAR) {
            *out_value = left_value * right_value;
            return 0;
        }
        return 1;
    }
    default:
        break;
    }

    return 1;
}

static ListNode_t *append_set_element(ListNode_t *elements, struct SetElement *element)
{
    if (element == NULL)
        return elements;

    ListNode_t *node = CreateListNode(element, LIST_SET_ELEMENT);
    if (elements == NULL)
        return node;
    return PushListNodeBack(elements, node);
}

static struct Expression *convert_set_literal(ast_t *set_node) {
    if (set_node == NULL)
        return mk_set(0, 0, NULL, 1);

    uint32_t mask = 0;
    int all_constant = 1;
    ListNode_t *elements = NULL;

    for (ast_t *element = set_node->child; element != NULL && element != ast_nil; element = element->next) {
        ast_t *unwrapped = unwrap_pascal_node(element);
        if (unwrapped == NULL)
            continue;

        if (unwrapped->typ == PASCAL_T_RANGE) {
            ast_t *start_node = unwrap_pascal_node(unwrapped->child);
            ast_t *end_node = NULL;
            if (start_node != NULL)
                end_node = unwrap_pascal_node(start_node->next);

            struct Expression *start_expr = convert_expression(start_node);
            struct Expression *end_expr = convert_expression(end_node);
            elements = append_set_element(elements, mk_set_element(start_expr, end_expr));

            long long start_value = 0;
            long long end_value = 0;
            int have_start = (start_expr != NULL) && extract_constant_int(start_expr, &start_value) == 0;
            int have_end = (end_expr != NULL) && extract_constant_int(end_expr, &end_value) == 0;

            if (!have_start || !have_end)
            {
                all_constant = 0;
                continue;
            }

            if (start_value > end_value) {
                long long tmp = start_value;
                start_value = end_value;
                end_value = tmp;
            }
            for (long long value = start_value; value <= end_value; ++value) {
                if (value >= 0 && value < 32)
                    mask |= (uint32_t)1u << value;
                else
                    all_constant = 0;  /* Value outside bitmask range */
            }
        } else {
            struct Expression *value_expr = convert_expression(unwrapped);
            elements = append_set_element(elements, mk_set_element(value_expr, NULL));

            long long value = 0;
            if (value_expr == NULL || extract_constant_int(value_expr, &value) != 0) {
                all_constant = 0;
                continue;
            }

            if (value >= 0 && value < 32)
                mask |= (uint32_t)1u << value;
            else
                all_constant = 0;  /* Value outside bitmask range */
        }
    }

    return mk_set(set_node->line, mask, elements, all_constant);
}

static struct Expression *convert_inherited_operator_expr(int line_num, const char *op_id, ast_t *arg_node);

static struct Expression *convert_factor(ast_t *expr_node) {
    expr_node = unwrap_pascal_node(expr_node);
    if (expr_node == NULL) {
        return NULL;
    }

    switch (expr_node->typ) {
    case PASCAL_T_INTEGER:
    {
        const char *num_str = (expr_node->sym != NULL) ? expr_node->sym->name : "0";
        int base = 10;
        /* Handle hex literals ($FF), binary literals (%1010), octal literals (&777) */
        if (num_str[0] == '$')
        {
            base = 16;
            num_str++;  /* Skip the $ prefix */
        }
        else if (num_str[0] == '%')
        {
            base = 2;
            num_str++;  /* Skip the % prefix */
        }
        else if (num_str[0] == '&')
        {
            base = 8;
            num_str++;  /* Skip the & prefix */
        }
        long long literal_value = 0;
        if (parse_integer_literal(num_str, base, &literal_value, NULL) != 0)
            literal_value = 0;
        return mk_inum(expr_node->line, literal_value);
    }
    case PASCAL_T_REAL:
        return mk_rnum(expr_node->line, strtod(expr_node->sym->name, NULL));
    case PASCAL_T_STRING:
        return mk_string(expr_node->line, dup_symbol(expr_node));
    case PASCAL_T_CHAR:
    {
        /* Convert character literal to character code */
        const char *char_str = (expr_node->sym != NULL) ? expr_node->sym->name : NULL;
        if (char_str != NULL && strlen(char_str) >= 1)
        {
            return mk_charcode(expr_node->line, (unsigned int)char_str[0]);
        }
        else
        {
            /* Fallback: create empty character */
            return mk_charcode(expr_node->line, 0);
        }
    }
    case PASCAL_T_CHAR_CODE:
    {
        unsigned int char_value = 0;
        const char *literal = (expr_node->sym != NULL) ? expr_node->sym->name : NULL;
        if (literal != NULL)
        {
            const char *digits = literal;
            if (*digits == '#')
                ++digits;

            int base = 10;
            if (*digits == '$')
            {
                base = 16;
                ++digits;
            }

            if (*digits != '\0')
            {
                char *endptr = NULL;
                long parsed = strtol(digits, &endptr, base);
                if (endptr != NULL && *endptr == '\0')
                {
                    if (parsed < 0)
                        parsed = 0;
                    char_value = (unsigned int)parsed;
                }
                else
                {
                    fprintf(stderr,
                        "ERROR: invalid character code literal %s at line %d.\n",
                        literal, expr_node->line);
                }
            }
            else
            {
                fprintf(stderr,
                    "ERROR: invalid character code literal %s at line %d.\n",
                    literal, expr_node->line);
            }
        }
        else
        {
            fprintf(stderr,
                "ERROR: invalid character code literal at line %d.\n",
                expr_node->line);
        }
        return mk_charcode(expr_node->line, char_value);
    }
    case PASCAL_T_BOOLEAN:
        if (expr_node->sym != NULL && expr_node->sym->name != NULL) {
            const char *value = expr_node->sym->name;
            int bool_value = (strcasecmp(value, "true") == 0);
            return mk_bool(expr_node->line, bool_value);
        }
        return mk_bool(expr_node->line, 0);
    case PASCAL_T_SET:
        return convert_set_literal(expr_node);
    case PASCAL_T_NIL:
        return mk_nil(expr_node->line);
    case PASCAL_T_IDENTIFIER:
        return mk_varid(expr_node->line, dup_symbol(expr_node));
    case PASCAL_T_QUALIFIED_IDENTIFIER:
    {
        QualifiedIdent *qid = qualified_ident_from_ast(expr_node);
        char *joined = qualified_ident_join(qid, ".");
        struct Expression *expr = mk_varid(expr_node->line, joined);
        if (expr != NULL)
        {
            if (expr->id_ref != NULL)
                qualified_ident_free(expr->id_ref);
            expr->id_ref = qid;
            qid = NULL;
        }
        if (qid != NULL)
            qualified_ident_free(qid);
        return expr;
    }
    case PASCAL_T_CONSTRUCTED_TYPE:
    {
        char *base_name = NULL;
        ListNode_t *type_args = NULL;
        if (extract_constructed_type_info(expr_node, &base_name, &type_args))
        {
            char *specialized_name = mangle_specialized_name_from_list(base_name, type_args);
            if (specialized_name == NULL && base_name != NULL)
                specialized_name = strdup(base_name);

            /* Trigger generic instantiation for inline specialize in expression
             * context — but only if this specialization hasn't already been
             * registered by the normal type-declaration pipeline. */
            if (base_name != NULL && type_args != NULL) {
                /* Skip inline spec creation if any type arg is itself a
                 * generic type parameter (e.g. specialize T<T> inside a
                 * generic method body — not a concrete specialization). */
                int has_type_param_arg = 0;
                if (generic_registry_current_context() != NULL) {
                    for (ListNode_t *a = type_args; a != NULL; a = a->next) {
                        if (a->type == LIST_STRING && a->cur != NULL &&
                            generic_registry_is_type_param((const char *)a->cur)) {
                            has_type_param_arg = 1;
                            break;
                        }
                    }
                }
                if (has_type_param_arg) goto skip_inline_spec;

                ListNode_t *args_copy = NULL;
                ListNode_t *args_tail = NULL;
                for (ListNode_t *a = type_args; a != NULL; a = a->next) {
                    if (a->type == LIST_STRING && a->cur != NULL) {
                        ListNode_t *n = CreateListNode(strdup((char *)a->cur), LIST_STRING);
                        if (n != NULL) {
                            if (args_tail != NULL) { args_tail->next = n; args_tail = n; }
                            else { args_copy = n; args_tail = n; }
                        }
                    }
                }
                if (args_copy != NULL) {
                    char *inst_name = NULL;
                    struct RecordType *record = instantiate_generic_record(
                        base_name, args_copy, &inst_name);
                    if (record != NULL) {
                        char *rec_id = inst_name != NULL ? strdup(inst_name) : strdup(specialized_name);
                        Tree_t *type_decl = mk_record_type(expr_node->line, rec_id, record);
                        if (type_decl != NULL) {
                            DeferredInlineSpec *entry = (DeferredInlineSpec *)calloc(1, sizeof(DeferredInlineSpec));
                            if (entry != NULL) {
                                entry->type_decl = type_decl;
                                entry->next = g_deferred_inline_specs;
                                g_deferred_inline_specs = entry;
                            } else {
                                destroy_tree(type_decl);
                            }
                        }
                    }
                    /* args_copy ownership: instantiate_generic_record takes ownership
                     * on success (it stores args in the record); on failure it may or
                     * may not have freed them, but we can safely destroy_list a NULL. */
                    if (record == NULL)
                        destroy_list(args_copy);
                    if (inst_name != NULL)
                        free(inst_name);
                }
                skip_inline_spec: ;
            }

            if (type_args != NULL)
                destroy_list(type_args);
            if (base_name != NULL)
                free(base_name);
            if (specialized_name != NULL)
                return mk_varid(expr_node->line, specialized_name);
        }
        return NULL;
    }
    case PASCAL_T_FUNC_CALL: {
        ast_t *child = expr_node->child;
        char *id = NULL;
        if (kgpc_getenv("KGPC_DEBUG_SPECIALIZE_CALLS") != NULL)
        {
            fprintf(stderr, "[KGPC_DEBUG_SPECIALIZE_CALLS] FUNC_CALL line=%d child_typ=%d(%s)\n",
                expr_node->line,
                child != NULL ? child->typ : -1,
                child != NULL ? pascal_tag_to_string(child->typ) : "<null>");
            if (child != NULL && child->sym != NULL && child->sym->name != NULL)
            {
                fprintf(stderr, "[KGPC_DEBUG_SPECIALIZE_CALLS]   child sym=%s\n",
                    child->sym->name);
            }
            if (child != NULL && child->child != NULL)
            {
                fprintf(stderr, "[KGPC_DEBUG_SPECIALIZE_CALLS]   child->child typ=%d(%s) sym=%s\n",
                    child->child->typ,
                    pascal_tag_to_string(child->child->typ),
                    (child->child->sym != NULL && child->child->sym->name != NULL)
                        ? child->child->sym->name : "<null>");
            }
        }
        if (child != NULL && child->typ == PASCAL_T_MEMBER_ACCESS)
        {
            struct Expression *callee = convert_expression(child);
            ListNode_t *args = convert_expression_list(child->next);
            if (callee != NULL && callee->type == EXPR_RECORD_ACCESS &&
                callee->expr_data.record_access_data.record_expr != NULL &&
                callee->expr_data.record_access_data.field_id != NULL)
            {
                struct Expression *base_expr =
                    callee->expr_data.record_access_data.record_expr;
                char *method_id = strdup(callee->expr_data.record_access_data.field_id);
                callee->expr_data.record_access_data.record_expr = NULL;
                destroy_expr(callee);
                if (base_expr != NULL)
                    args = PushListNodeFront(args, CreateListNode(base_expr, LIST_EXPR));
                struct Expression *call_expr = mk_functioncall(expr_node->line, method_id, args);
                tag_operator_call(call_expr, is_operator_token_name(method_id));
                if (call_expr != NULL)
                {
                    call_expr->expr_data.function_call_data.is_method_call_placeholder = 1;
                    call_expr->expr_data.function_call_data.placeholder_method_name =
                        method_id != NULL ? strdup(method_id) : NULL;
                }
                return call_expr;
            }
            if (callee != NULL)
                destroy_expr(callee);
            return mk_functioncall(expr_node->line, NULL, args);
        }
        int saw_inherited = 0;
        if (child != NULL) {
            if (child->typ == PASCAL_T_IDENTIFIER) {
                const char *name = ast_symbol_name(child);
                if (name != NULL && strcasecmp(name, "inherited") == 0) {
                    saw_inherited = 1;
                    child = child->next;
                    /* Now child points to the method name; extract it */
                    if (child != NULL && child->typ == PASCAL_T_IDENTIFIER) {
                        id = dup_symbol(child);
                        child = child->next;
                    } else if (child != NULL) {
                        ast_t *unwrapped = unwrap_pascal_node(child);
                        if (unwrapped != NULL && unwrapped->typ == PASCAL_T_IDENTIFIER) {
                            id = dup_symbol(unwrapped);
                            child = child->next;
                        }
                    }
                }
                else if (name != NULL)
                    id = dup_symbol(child);
            } else if (child->typ == PASCAL_T_TYPECAST &&
                       child->child != NULL &&
                       child->child->typ == PASCAL_T_IDENTIFIER) {
                /* Handle chained call: TypeName(source)(outer_args)
                 * Parser creates: FUNC_CALL(TYPECAST(TypeName, source), outer_args)
                 * Convert the typecast part to an expression, then create a call
                 * with the outer args that invokes the typecast result. */
                struct Expression *typecast_expr = convert_expression(child);
                ast_t *outer_args_start = child->next;
                ListNode_t *outer_args = convert_expression_list(outer_args_start);
                /* Create a function call where the callee is the typecast expression */
                char *type_id = dup_symbol(child->child);
                struct Expression *call_expr = mk_functioncall(expr_node->line, type_id, outer_args);
                if (call_expr != NULL) {
                    call_expr->expr_data.function_call_data.is_procedural_var_call = 1;
                    call_expr->expr_data.function_call_data.procedural_var_expr = typecast_expr;
                }
                return call_expr;
            } else if (child->child != NULL && child->child->typ == PASCAL_T_IDENTIFIER) {
                const char *name = ast_symbol_name(child->child);
                if (name != NULL && strcasecmp(name, "inherited") == 0)
                    saw_inherited = 1;
                else if (name != NULL)
                    id = dup_symbol(child->child);
            }
            if (!saw_inherited)
                child = child->next;
        }
        if (id == NULL && !saw_inherited) {
            for (ast_t *scan = expr_node->child; scan != NULL; scan = scan->next) {
                ast_t *node = unwrap_pascal_node(scan);
                if (node == NULL)
                    node = scan;
                if (node == NULL)
                    continue;
                if (node->typ == PASCAL_T_ARG_LIST)
                    break;
                if (node->typ == PASCAL_T_IDENTIFIER) {
                    const char *name = ast_symbol_name(node);
                    if (name == NULL)
                        continue;
                    id = strdup(name);
                    break;
                }
            }
        }
        int is_bare_inherited = 0;
        if (id == NULL && saw_inherited && g_current_method_name != NULL) {
            /* cparser encodes bare "inherited" as FUNC_CALL with an empty child node.
             * In method bodies, that means "inherited <current-method-name>". */
            id = strdup(g_current_method_name);
            is_bare_inherited = 1;
        }
        if (saw_inherited && id != NULL)
        {
            struct Expression *inherited_op = convert_inherited_operator_expr(expr_node->line, id, child);
            if (inherited_op != NULL)
            {
                free(id);
                return inherited_op;
            }
        }
        ListNode_t *args = convert_expression_list(child);
        struct Expression *result = mk_functioncall(expr_node->line, id, args);
        tag_operator_call(result, is_operator_token_name(id));
        if (saw_inherited && result != NULL) {
            result->expr_data.function_call_data.is_inherited_call = 1;
            result->expr_data.function_call_data.is_bare_inherited = is_bare_inherited;
        }
        return result;
    }
    case PASCAL_T_ARRAY_ACCESS: {
        ast_t *array_node = expr_node->child;
        ast_t *index_expr_node = array_node != NULL ? array_node->next : NULL;
        struct Expression *base = convert_expression(array_node);
        struct Expression *first_index = convert_expression(index_expr_node);
        struct Expression *result = mk_arrayaccess(expr_node->line, base, first_index);
        
        /* For multi-dimensional arrays like arr[x, y, z], the indices are chained
         * via ->next. Store additional indices in extra_indices list. */
        if (index_expr_node != NULL && index_expr_node->next != NULL) {
            ast_t *extra_idx_node = index_expr_node->next;
            ListNode_t *extra_indices = NULL;
            ListNode_t *extra_tail = NULL;
            
            while (extra_idx_node != NULL) {
                struct Expression *extra_idx = convert_expression(extra_idx_node);
                if (extra_idx != NULL) {
                    ListNode_t *new_node = CreateListNode(extra_idx, LIST_EXPR);
                    if (new_node != NULL) {
                        if (extra_tail == NULL) {
                            extra_indices = new_node;
                            extra_tail = new_node;
                        } else {
                            extra_tail->next = new_node;
                            extra_tail = new_node;
                        }
                    }
                }
                extra_idx_node = extra_idx_node->next;
            }
            result->expr_data.array_access_data.extra_indices = extra_indices;
        }
        return result;
    }
    default:
        return NULL;
    }
}

static struct Expression *convert_binary_expr(ast_t *node, int type) {
    ast_t *left_node = node->child;
    ast_t *right_node = left_node != NULL ? left_node->next : NULL;
    struct Expression *left = convert_expression(left_node);
    struct Expression *right = convert_expression(right_node);

    switch (type) {
    case PASCAL_T_ADD:
    case PASCAL_T_SUB:
    case PASCAL_T_OR:
        return mk_addop(node->line, map_addop_tag(type), left, right);
    case PASCAL_T_MUL:
    case PASCAL_T_DIV:
    case PASCAL_T_INTDIV:
    case PASCAL_T_MOD:
    case PASCAL_T_POWER:
    case PASCAL_T_AND:
    case PASCAL_T_XOR:
    case PASCAL_T_SHL:
    case PASCAL_T_SHR:
    case PASCAL_T_ROL:
    case PASCAL_T_ROR:
        return mk_mulop(node->line, map_mulop_tag(type), left, right);
    case PASCAL_T_EQ:
    case PASCAL_T_NE:
    case PASCAL_T_LT:
    case PASCAL_T_LE:
    case PASCAL_T_GT:
    case PASCAL_T_GE:
    case PASCAL_T_IN:
        return mk_relop(node->line, map_relop_tag(type), left, right);
    default:
        break;
    }
    return NULL;
}

static struct Expression *convert_inherited_operator_expr(int line_num, const char *op_id, ast_t *arg_node)
{
    if (g_current_method_name == NULL || op_id == NULL || arg_node == NULL || arg_node->next != NULL)
        return NULL;

    struct Expression *lhs = mk_functioncall(line_num, strdup(g_current_method_name), NULL);
    if (lhs == NULL)
        return NULL;
    lhs->expr_data.function_call_data.is_inherited_call = 1;

    struct Expression *rhs = convert_expression(arg_node);
    if (rhs == NULL)
    {
        destroy_expr(lhs);
        return NULL;
    }

    if (strcasecmp(op_id, "and") == 0)
        return mk_mulop(line_num, AND, lhs, rhs);
    if (strcasecmp(op_id, "xor") == 0)
        return mk_mulop(line_num, XOR, lhs, rhs);
    if (strcasecmp(op_id, "or") == 0)
        return mk_addop(line_num, OR, lhs, rhs);

    destroy_expr(lhs);
    destroy_expr(rhs);
    return NULL;
}

static struct Expression *convert_unary_expr(ast_t *node) {
    struct Expression *inner = convert_expression(node->child);
    // Only negate for PASCAL_T_NEG, unary plus (PASCAL_T_POS) returns the inner expression as-is
    if (node->typ == PASCAL_T_NEG) {
        return mk_signterm(node->line, inner);
    }
    return inner;
}

static const char *tag_name(tag_t tag) {
    const char *name = pascal_tag_to_string(tag);
    return (name != NULL) ? name : "UNKNOWN";
}

static struct Expression *convert_expression(ast_t *expr_node) {
    ast_t *original_node = expr_node;  /* Save for source_index */
    expr_node = unwrap_pascal_node(expr_node);

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_expression: typ=%d line=%d\n", expr_node->typ, expr_node->line);
        if (expr_node->next != NULL) {
            fprintf(stderr, "[KGPC] convert_expression: has next sibling typ=%d\n", expr_node->next->typ);
        }
    }

    if (expr_node == NULL || expr_node == ast_nil)
        return NULL;

    /* PASCAL_T_NONE nodes are empty/placeholder nodes from the parser
     * (e.g., optional clauses that were absent).  Treat as NULL silently. */
    if (expr_node->typ == PASCAL_T_NONE)
        return NULL;

    struct Expression *result = NULL;
    switch (expr_node->typ) {
    case PASCAL_T_INTEGER:
    case PASCAL_T_REAL:
    case PASCAL_T_STRING:
    case PASCAL_T_CHAR:
    case PASCAL_T_CHAR_CODE:
    case PASCAL_T_BOOLEAN:
    case PASCAL_T_IDENTIFIER:
    case PASCAL_T_QUALIFIED_IDENTIFIER:
    case PASCAL_T_CONSTRUCTED_TYPE:
    case PASCAL_T_FUNC_CALL:
    case PASCAL_T_ARRAY_ACCESS:
    case PASCAL_T_SET:
    case PASCAL_T_NIL:
        result = convert_factor(expr_node);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_MEMBER_ACCESS:
        result = convert_member_access(expr_node);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_ADD:
    case PASCAL_T_SUB:
    case PASCAL_T_MUL:
    case PASCAL_T_POWER:
    case PASCAL_T_DIV:
    case PASCAL_T_INTDIV:
    case PASCAL_T_MOD:
    case PASCAL_T_EQ:
    case PASCAL_T_NE:
    case PASCAL_T_LT:
    case PASCAL_T_LE:
    case PASCAL_T_GT:
    case PASCAL_T_GE:
    case PASCAL_T_IN:
    case PASCAL_T_AND:
    case PASCAL_T_OR:
    case PASCAL_T_XOR:
    case PASCAL_T_SHL:
    case PASCAL_T_SHR:
    case PASCAL_T_ROL:
    case PASCAL_T_ROR:
        result = convert_binary_expr(expr_node, expr_node->typ);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_SET_UNION:
        result = convert_binary_expr(expr_node, PASCAL_T_ADD);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_SET_INTERSECT:
        result = convert_binary_expr(expr_node, PASCAL_T_MUL);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_SET_DIFF:
        result = convert_binary_expr(expr_node, PASCAL_T_SUB);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_SET_SYM_DIFF:
        result = convert_binary_expr(expr_node, PASCAL_T_XOR);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_IS:
    {
        ast_t *value_node = expr_node->child;
        ast_t *type_node = value_node != NULL ? value_node->next : NULL;
        struct Expression *value_expr = convert_expression(value_node);
        int target_type = UNKNOWN_TYPE;
        char *target_type_id = NULL;
        struct RecordType *inline_record = NULL;
        TypeInfo type_info;
        memset(&type_info, 0, sizeof(TypeInfo));
        TypeRef *type_ref_local = NULL;
        
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_expression IS: value_node=%p type_node=%p\n", value_node, type_node);
            if (type_node) fprintf(stderr, "[KGPC]   type_node typ=%d\n", type_node->typ);
        }
        
        if (type_node != NULL)
        {
            target_type = convert_type_spec(type_node, &target_type_id, &inline_record, &type_info);
            
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC]   convert_type_spec result: type=%d id=%s\n", target_type, target_type_id ? target_type_id : "<null>");
            }
            
            type_ref_local = type_ref_from_info_or_id(&type_info, target_type_id);
            destroy_type_info_contents(&type_info);
            if (inline_record != NULL)
                destroy_record_type(inline_record);
        }
        result = mk_is(expr_node->line, value_expr, target_type, target_type_id);
        if (result != NULL)
            result->expr_data.is_data.target_type_ref = type_ref_local;
        else if (type_ref_local != NULL)
            type_ref_free(type_ref_local);
        return set_expr_source_index(result, original_node);
    }
    case PASCAL_T_AS:
    {
        ast_t *value_node = expr_node->child;
        ast_t *type_node = value_node != NULL ? value_node->next : NULL;
        struct Expression *value_expr = convert_expression(value_node);
        int target_type = UNKNOWN_TYPE;
        char *target_type_id = NULL;
        struct RecordType *inline_record = NULL;
        TypeInfo type_info;
        memset(&type_info, 0, sizeof(TypeInfo));
        TypeRef *type_ref_local = NULL;
        if (type_node != NULL)
        {
            target_type = convert_type_spec(type_node, &target_type_id, &inline_record, &type_info);
            type_ref_local = type_ref_from_info_or_id(&type_info, target_type_id);
            destroy_type_info_contents(&type_info);
            if (inline_record != NULL)
                destroy_record_type(inline_record);
        }
        result = mk_as(expr_node->line, value_expr, target_type, target_type_id);
        if (result != NULL)
            result->expr_data.as_data.target_type_ref = type_ref_local;
        else if (type_ref_local != NULL)
            type_ref_free(type_ref_local);
        return set_expr_source_index(result, original_node);
    }
    case PASCAL_T_NEG:
    case PASCAL_T_POS:
        result = convert_unary_expr(expr_node);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_NOT:
    {
        struct Expression *not_operand = convert_expression(expr_node->child);
        result = mk_relop(expr_node->line, NOT, not_operand, NULL);
        return set_expr_source_index(result, original_node);
    }
    case PASCAL_T_TUPLE:
    {
        if (tuple_is_record_constructor(expr_node))
        {
            return convert_record_constructor_expr(expr_node);
        }

        ListNode_t *elements = NULL;
        ListNode_t *tail = NULL;
        int count = 0;

        for (ast_t *elem = expr_node->child; elem != NULL; elem = elem->next)
        {
            ast_t *unwrapped = unwrap_pascal_node(elem);
            if (unwrapped == NULL)
                continue;
            struct Expression *elem_expr = convert_expression(unwrapped);
            if (elem_expr == NULL)
                goto tuple_cleanup;

            ListNode_t *node = CreateListNode(elem_expr, LIST_EXPR);
            if (node == NULL)
            {
                destroy_expr(elem_expr);
                goto tuple_cleanup;
            }
            if (elements == NULL)
            {
                elements = node;
                tail = node;
            }
            else
            {
                tail->next = node;
                tail = node;
            }
            ++count;
        }

        return mk_array_literal(expr_node->line, elements, count);

tuple_cleanup:
        while (elements != NULL)
        {
            ListNode_t *next = elements->next;
            if (elements->cur != NULL)
                destroy_expr((struct Expression *)elements->cur);
            free(elements);
            elements = next;
        }
        return NULL;
    }
    case PASCAL_T_RECORD_CONSTRUCTOR:
    {
        return convert_record_constructor_expr(expr_node);
    }
    case PASCAL_T_FIELD_WIDTH:
    {
        /* FPC record constructors with a single field like: (state: (1,2,3))
         * can be parsed as FIELD_WIDTH. Detect tuple RHS and reinterpret as record ctor. */
        ast_t *base_node = expr_node->child;
        ast_t *format_node = (base_node != NULL) ? base_node->next : NULL;
        ast_t *format_unwrapped = unwrap_pascal_node(format_node);
        if (base_node != NULL && base_node->typ == PASCAL_T_IDENTIFIER &&
            base_node->sym != NULL && base_node->sym->name != NULL &&
            format_unwrapped != NULL && format_unwrapped->typ == PASCAL_T_TUPLE)
        {
            struct Expression *field_value = convert_expression(format_unwrapped);
            if (field_value == NULL)
                return NULL;

            struct RecordConstructorField *field =
                (struct RecordConstructorField *)calloc(1, sizeof(struct RecordConstructorField));
            if (field == NULL)
            {
                destroy_expr(field_value);
                return NULL;
            }
            field->field_id = strdup(base_node->sym->name);
            field->value = field_value;

            ListNode_t *node = CreateListNode(field, LIST_UNSPECIFIED);
            if (node == NULL)
            {
                if (field->field_id != NULL)
                    free(field->field_id);
                destroy_expr(field_value);
                free(field);
                return NULL;
            }

            return mk_record_constructor(expr_node->line, node, 1);
        }

        return convert_field_width_expr(expr_node);
    }
    case PASCAL_T_TYPECAST:
    {
        ast_t *type_node = expr_node->child;
        ast_t *value_node = (type_node != NULL) ? type_node->next : NULL;

        int target_type = UNKNOWN_TYPE;
        char *target_type_id = NULL;
        char *target_type_qualifier = NULL;
        struct RecordType *record_type = NULL;
        TypeInfo type_info;
        memset(&type_info, 0, sizeof(TypeInfo));
        TypeRef *type_ref_local = NULL;

        ast_t *unwrapped_type = unwrap_pascal_node(type_node);
        if (unwrapped_type != NULL)
        {
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL)
                fprintf(stderr, "[KGPC] TYPECAST handler: type_node typ=%d unwrapped typ=%d\n",
                    type_node ? type_node->typ : -1, unwrapped_type->typ);
            target_type = convert_type_spec(unwrapped_type, &target_type_id,
                &record_type, &type_info);
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL)
                fprintf(stderr, "[KGPC] TYPECAST handler: after convert_type_spec target_type=%d target_type_id=%s\n",
                    target_type, target_type_id ? target_type_id : "(null)");
            type_ref_local = type_ref_from_info_or_id(&type_info, target_type_id);
            destroy_type_info_contents(&type_info);
            if (record_type != NULL)
            {
                destroy_record_type(record_type);
                record_type = NULL;
            }

            if (target_type_id == NULL && type_ref_local != NULL && type_ref_local->name != NULL)
            {
                QualifiedIdent *qid = type_ref_local->name;
                if (qid->count > 0 && qid->segments != NULL)
                {
                    const char *base = qid->segments[qid->count - 1];
                    if (base != NULL)
                        target_type_id = strdup(base);
                    if (target_type_id != NULL && target_type_qualifier == NULL && qid->count > 1)
                    {
                        target_type_qualifier = qualified_ident_join_prefix(qid, qid->count - 1);
                    }
                }
            }

            if (target_type == UNKNOWN_TYPE && target_type_id == NULL &&
                unwrapped_type->typ == PASCAL_T_IDENTIFIER)
            {
                target_type_id = dup_symbol(unwrapped_type);
            }
            /* Handle qualified identifiers like widestringmanager.UpperUnicodeStringProc
             * parsed as member access in typecast position */
            if (target_type == UNKNOWN_TYPE && target_type_id == NULL &&
                unwrapped_type->typ == PASCAL_T_MEMBER_ACCESS)
            {
                ast_t *base = unwrapped_type->child;
                ast_t *field = (base != NULL) ? base->next : NULL;
                char *base_name = dup_symbol(base);
                char *field_name = dup_symbol(field);
                if (base_name != NULL && field_name != NULL)
                {
                    /* Store components separately as structured data:
                     * - target_type_id: bare name (field/type name)
                     * - target_type_qualifier: base (record variable or parent type)
                     * Semcheck constructs the qualified lookup key when needed. */
                    target_type_id = field_name;
                    field_name = NULL;  /* ownership transferred */
                    target_type_qualifier = base_name;
                    base_name = NULL;   /* ownership transferred */
                }
                if (base_name != NULL) free(base_name);
                if (field_name != NULL) free(field_name);
            }
            if (target_type_id != NULL && target_type_qualifier == NULL &&
                type_ref_local != NULL && type_ref_local->name != NULL &&
                type_ref_local->name->count > 1)
            {
                QualifiedIdent *qid = type_ref_local->name;
                int prefix_count = qid->count - 1;
                char *prefix = qualified_ident_join_prefix(qid, prefix_count);
                const char *base = qid->segments[qid->count - 1];
                if (prefix != NULL && base != NULL)
                {
                    free(target_type_id);
                    target_type_id = strdup(base);
                    if (target_type_id == NULL)
                    {
                        target_type_id = NULL;
                        free(prefix);
                    }
                    else
                    {
                        target_type_qualifier = prefix;
                    }
                }
                else
                {
                    free(prefix);
                }
            }
            /* Handle generic specialization typecasts like specialize TArray<T>(Result).
             * Extract the base type name and build a TypeRef with args. */
            if (target_type == UNKNOWN_TYPE && target_type_id == NULL)
            {
                char *gen_base = NULL;
                ListNode_t *gen_args = NULL;
                if (extract_specialize_type_info(type_node, &gen_base, &gen_args))
                {
                    target_type_id = strdup(gen_base);
                    if (type_ref_local != NULL)
                        type_ref_free(type_ref_local);
                    type_ref_local = type_ref_from_name_and_args(gen_base, gen_args);
                    free(gen_base);
                    if (gen_args != NULL)
                        destroy_list(gen_args);
                }
            }
            if (target_type == UNKNOWN_TYPE && target_type_id == NULL &&
                unwrapped_type->typ == PASCAL_T_CONSTRUCTED_TYPE)
            {
                char *gen_base = NULL;
                ListNode_t *gen_args = NULL;
                if (extract_constructed_type_info(type_node, &gen_base, &gen_args))
                {
                    target_type_id = strdup(gen_base);
                    if (type_ref_local != NULL)
                        type_ref_free(type_ref_local);
                    type_ref_local = type_ref_from_name_and_args(gen_base, gen_args);
                    free(gen_base);
                    if (gen_args != NULL) destroy_list(gen_args);
                }
            }
            if (type_ref_local == NULL && unwrapped_type != NULL)
            {
                char *gen_base = NULL;
                ListNode_t *gen_args = NULL;
                if (extract_constructed_type_info(type_node, &gen_base, &gen_args) ||
                    extract_specialize_type_info(type_node, &gen_base, &gen_args))
                {
                    if (type_ref_local != NULL)
                        type_ref_free(type_ref_local);
                    type_ref_local = type_ref_from_name_and_args(gen_base, gen_args);
                    free(gen_base);
                    if (gen_args != NULL) destroy_list(gen_args);
                }
            }
        }

        ListNode_t *args = NULL;
        int arg_count = 0;
        if (value_node != NULL)
        {
            args = convert_expression_list(value_node);
            arg_count = ListLength(args);
        }

        if (arg_count > 1)
        {
            char *call_id = target_type_id;
            target_type_id = NULL;
            struct Expression *call_expr = mk_functioncall(expr_node->line, call_id, args);
            if (call_expr != NULL)
                call_expr->expr_data.function_call_data.call_qualifier = target_type_qualifier;
            else if (target_type_qualifier != NULL)
                free(target_type_qualifier);
            target_type_qualifier = NULL;
            return call_expr;
        }

        struct Expression *inner_expr = NULL;
        if (arg_count == 1 && args != NULL)
        {
            inner_expr = (struct Expression *)args->cur;
            free(args);
        }
        else
        {
            if (args != NULL)
            {
                DestroyList(args);
                args = NULL;
            }
            inner_expr = convert_expression(value_node);
        }
        struct Expression *tc_expr = mk_typecast(expr_node->line, target_type, target_type_id, inner_expr);
        if (tc_expr != NULL)
        {
            tc_expr->expr_data.typecast_data.type_qualifier = target_type_qualifier;
            if (type_ref_local != NULL)
            {
                tc_expr->expr_data.typecast_data.target_type_ref = type_ref_local;
                type_ref_local = NULL;
            }
            else
            {
                tc_expr->expr_data.typecast_data.target_type_ref =
                    type_ref_from_qualifier_and_id(target_type_qualifier, target_type_id);
            }
        }
        else if (target_type_qualifier != NULL)
            free(target_type_qualifier);
        target_type_qualifier = NULL;
        if (type_ref_local != NULL)
            type_ref_free(type_ref_local);
        return tc_expr;
    }
    case PASCAL_T_DEREF:
    {
        struct Expression *result = mk_pointer_deref(expr_node->line, convert_expression(expr_node->child));
        return set_expr_source_index(result, expr_node);
    }
    case PASCAL_T_ADDR:
    {
        /* Resolve @specialize Type<T>.Method at conversion time while
         * preprocessed_source still points to the correct unit's buffer.
         * Once units are merged into the program tree the source indices
         * become invalid for the main program's buffer, so the semcheck
         * fallback (which re-parses source text) would fail. */
        ast_t *child = expr_node->child;
        if (child != NULL && child->sym != NULL && child->sym->name != NULL &&
            strcasecmp(child->sym->name, "specialize") == 0 &&
            preprocessed_source != NULL && preprocessed_length > 0 &&
            expr_node->index >= 0 && (size_t)expr_node->index < preprocessed_length)
        {
            size_t idx = (size_t)expr_node->index;
            /* Scan forward to find '@' on this line */
            size_t line_end = idx;
            while (line_end < preprocessed_length &&
                   preprocessed_source[line_end] != '\n' &&
                   preprocessed_source[line_end] != '\r' &&
                   preprocessed_source[line_end] != ';')
                line_end++;

            size_t at_pos = idx;
            for (size_t s = idx; s < line_end; ++s)
            {
                if (preprocessed_source[s] == '@') { at_pos = s; break; }
            }
            size_t i = at_pos;
            if (i < line_end && preprocessed_source[i] == '@')
                i++;
            while (i < line_end && (preprocessed_source[i] == ' ' || preprocessed_source[i] == '\t'))
                i++;
            /* Skip optional 'specialize' keyword */
            const char *kw = "specialize";
            size_t kw_len = 10;
            if (i + kw_len <= line_end && strncasecmp(preprocessed_source + i, kw, kw_len) == 0)
            {
                i += kw_len;
                while (i < line_end && (preprocessed_source[i] == ' ' || preprocessed_source[i] == '\t'))
                    i++;
            }
            /* Parse type name including angle brackets: Type<T> */
            size_t type_start = i;
            int angle_depth = 0;
            while (i < line_end)
            {
                char ch = preprocessed_source[i];
                if (ch == '<') angle_depth++;
                else if (ch == '>') { if (angle_depth > 0) angle_depth--; }
                else if (ch == '.' && angle_depth == 0) break;
                else if (ch == '\n' || ch == ';' || ch == '\r') break;
                i++;
            }
            if (i < line_end && preprocessed_source[i] == '.' && i > type_start)
            {
                /* Extract type name */
                size_t type_len = i - type_start;
                char *type_raw = (char *)malloc(type_len + 1);
                if (type_raw != NULL)
                {
                    memcpy(type_raw, preprocessed_source + type_start, type_len);
                    type_raw[type_len] = '\0';
                    i++; /* skip '.' */
                    /* Parse method name */
                    size_t method_start = i;
                    while (i < line_end && (isalnum((unsigned char)preprocessed_source[i]) || preprocessed_source[i] == '_'))
                        i++;
                    if (i > method_start)
                    {
                        size_t method_len = i - method_start;
                        char *method_id = (char *)malloc(method_len + 1);
                        if (method_id != NULL)
                        {
                            memcpy(method_id, preprocessed_source + method_start, method_len);
                            method_id[method_len] = '\0';
                            /* Mangle generic type: Type<T> -> Type$T */
                            char *lt = strchr(type_raw, '<');
                            char *mangled = NULL;
                            if (lt != NULL)
                            {
                                char *gt = strrchr(type_raw, '>');
                                if (gt != NULL && gt > lt)
                                {
                                    size_t base_len = (size_t)(lt - type_raw);
                                    char args_buf[256];
                                    size_t args_len = 0;
                                    int adepth = 0;
                                    for (const char *p = lt + 1; p < gt; ++p)
                                    {
                                        char ac = *p;
                                        if (ac == '<') { adepth++; continue; }
                                        if (ac == '>') { if (adepth > 0) adepth--; continue; }
                                        if (adepth == 0 && ac == ',') ac = '$';
                                        if (adepth == 0 && ac == ' ') continue;
                                        if (args_len + 1 < sizeof(args_buf))
                                            args_buf[args_len++] = ac;
                                    }
                                    args_buf[args_len] = '\0';
                                    size_t out_len = base_len + 1 + args_len;
                                    mangled = (char *)malloc(out_len + 1);
                                    if (mangled != NULL)
                                        snprintf(mangled, out_len + 1, "%.*s$%s", (int)base_len, type_raw, args_buf);
                                }
                            }
                            if (mangled == NULL)
                                mangled = strdup(type_raw);

                            if (mangled != NULL)
                            {
                                /* Rewrite: @(specialize) -> @(MangledType.Method) */
                                struct Expression *base_expr = mk_varid(expr_node->line, mangled);
                                struct Expression *access = mk_recordaccess(expr_node->line, base_expr, method_id);
                                if (access != NULL)
                                {
                                    access->is_specialize_addr_target = 1;
                                    struct Expression *result = mk_addressof(expr_node->line, access);
                                    free(type_raw);
                                    return set_expr_source_index(result, expr_node);
                                }
                                /* access creation failed; fall through */
                                destroy_expr(base_expr);
                            }
                            else
                            {
                                free(method_id);
                            }
                        }
                    }
                    free(type_raw);
                }
            }
        }
        struct Expression *result = mk_addressof(expr_node->line, convert_expression(expr_node->child));
        return set_expr_source_index(result, expr_node);
    }
    case PASCAL_T_ANONYMOUS_FUNCTION:
    {
        /* Anonymous function: params -> return_type -> body 
         * Structure from parser:
         * - If params exist: child is first PARAM, params are linked via ->next
         * - Return type is in a sibling after all params
         * - Body is after return type
         */
        char *generated_name = generate_anonymous_method_name(1);
        if (generated_name == NULL) {
            fprintf(stderr, "ERROR: Failed to generate name for anonymous function at line %d\n", expr_node->line);
            return NULL;
        }
        
        /* Navigate the AST structure */
        ast_t *current = expr_node->child;
        ast_t *params_start = NULL;
        ast_t *return_type_node = NULL;
        ast_t *body_node = NULL;
        
        /* Check if first child is a PARAM (parameters exist) or RETURN_TYPE (no params) */
        if (current != NULL) {
            if (current->typ == PASCAL_T_PARAM) {
                /* We have parameters - collect them all */
                params_start = current;
                /* Find the last PARAM in the chain */
                while (current != NULL && current->typ == PASCAL_T_PARAM) {
                    current = current->next;
                }
                /* Now current should be RETURN_TYPE */
                if (current != NULL && current->typ == PASCAL_T_RETURN_TYPE) {
                    return_type_node = current;
                    body_node = current->next;
                }
            } else if (current->typ == PASCAL_T_RETURN_TYPE) {
                /* No parameters, directly to return type */
                return_type_node = current;
                body_node = current->next;
            }
        }
        
        /* Convert parameters */
        ListNode_t *parameters = NULL;
        if (params_start != NULL) {
            ast_t *param_cursor = params_start;
            parameters = convert_param_list(&param_cursor);
        }
        
        /* Convert return type */
        int return_type = UNKNOWN_TYPE;
        char *return_type_id = NULL;
        if (return_type_node != NULL && return_type_node->child != NULL) {
            if (return_type_node->child->sym != NULL && return_type_node->child->sym->name != NULL) {
                return_type_id = strdup(return_type_node->child->sym->name);
            }
        }
        
        /* Convert body */
        struct Statement *body = NULL;
        if (body_node != NULL) {
            body = convert_statement(body_node);
        }
        
        return mk_anonymous_function(expr_node->line, generated_name, parameters, 
                                      return_type, return_type_id, body);
    }
    case PASCAL_T_ANONYMOUS_PROCEDURE:
    {
        /* Anonymous procedure: params -> body
         * Structure from parser (same as anonymous function but without return type):
         * - If params exist: child is first PARAM, params are linked via ->next
         * - Body is after all params (or first child if no params)
         */
        char *generated_name = generate_anonymous_method_name(0);
        if (generated_name == NULL) {
            fprintf(stderr, "ERROR: Failed to generate name for anonymous procedure at line %d\n", expr_node->line);
            return NULL;
        }
        
        /* Navigate the AST structure */
        ast_t *current = expr_node->child;
        ast_t *params_start = NULL;
        ast_t *body_node = NULL;
        
        /* Check if first child is a PARAM (parameters exist) or a statement (no params) */
        if (current != NULL) {
            /* Skip NONE nodes */
            while (current != NULL && current->typ == PASCAL_T_NONE) {
                current = current->child;
            }
            
            if (current != NULL && current->typ == PASCAL_T_PARAM) {
                /* We have parameters - collect them all */
                params_start = current;
                /* Find the last PARAM in the chain */
                while (current != NULL && current->typ == PASCAL_T_PARAM) {
                    current = current->next;
                }
                /* Now current should be the body */
                body_node = current;
            } else {
                /* No parameters, directly to body */
                body_node = current;
            }
        }
        
        /* Convert parameters */
        ListNode_t *parameters = NULL;
        if (params_start != NULL) {
            ast_t *param_cursor = params_start;
            parameters = convert_param_list(&param_cursor);
        }
        
        /* Convert body */
        struct Statement *body = NULL;
        if (body_node != NULL) {
            body = convert_statement(body_node);
            if (body == NULL) {
                fprintf(stderr, "ERROR: Failed to convert anonymous procedure body at line %d\n", 
                        expr_node->line);
                if (generated_name) free(generated_name);
                if (parameters) destroy_list(parameters);
                return NULL;
            }
        } else {
            fprintf(stderr, "WARNING: Anonymous procedure has no body at line %d\n", expr_node->line);
        }
        
        return mk_anonymous_procedure(expr_node->line, generated_name, parameters, body);
    }
    default: {
        const char *name = tag_name(expr_node->typ);
        fprintf(stderr, "ERROR: unsupported expression tag %d (%s) at line %d.\n",
                expr_node->typ, name, expr_node->line);
        break;
    }
    }

    return NULL;
}

static ListNode_t *convert_expression_list(ast_t *arg_node) {
    ListBuilder builder;
    list_builder_init(&builder);
    
    /* Pre-check: verify the next chain doesn't have cycles using Floyd's algorithm.
     * If a cycle is found, break it by setting the back-edge to NULL.
     * This fixes cycles introduced by the parser's suffix chain construction
     * (e.g., typecast-then-call patterns like TProc(x)(args)). */
    {
        ast_t *slow = arg_node, *fast = arg_node;
        int has_cycle = 0;
        while (fast != NULL && fast != ast_nil && fast->next != NULL && fast->next != ast_nil) {
            slow = slow->next;
            fast = fast->next->next;
            if (slow == fast) {
                has_cycle = 1;
                break;
            }
        }
        if (has_cycle) {
            /* Find cycle start using Floyd's phase 2 */
            slow = arg_node;
            while (slow != fast) { slow = slow->next; fast = fast->next; }
            /* Walk around the cycle to find the back-edge */
            ast_t *cycle_prev = slow;
            ast_t *cycle_cur = slow->next;
            while (cycle_cur != slow) {
                cycle_prev = cycle_cur;
                cycle_cur = cycle_cur->next;
            }
            /* Break the cycle */
            cycle_prev->next = NULL;
        }
    }
    
    /* Create visited set to detect circular references */
    VisitedSet *visited = visited_set_create();
    if (visited == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate visited set for expression list traversal\n");
        return NULL;
    }
    
    /* Pre-check: verify the next chain doesn't have cycles using Floyd's algorithm */
    {
        ast_t *slow = arg_node, *fast = arg_node;
        int has_cycle = 0;
        while (fast != NULL && fast != ast_nil && fast->next != NULL && fast->next != ast_nil) {
            slow = slow->next;
            fast = fast->next->next;
            if (slow == fast) {
                has_cycle = 1;
                break;
            }
        }
        if (has_cycle) {
            fprintf(stderr, "PRE-CHECK: Cycle detected in expression list next-chain starting at %p\n", (void*)arg_node);
            /* Find cycle start */
            slow = arg_node;
            while (slow != fast) { slow = slow->next; fast = fast->next; }
            fprintf(stderr, "  Cycle starts at: %p type=%d line=%d sym=%s\n", (void*)slow,
                    slow->typ, slow->line, (slow->sym && slow->sym->name) ? slow->sym->name : "?");
            /* Just dump first few nodes */
            ast_t *d = arg_node;
            for (int i = 0; i < 10 && d != NULL && d != ast_nil; i++) {
                fprintf(stderr, "  [%d] %p type=%d line=%d sym=%s%s\n", i, (void*)d,
                        d->typ, d->line, (d->sym && d->sym->name) ? d->sym->name : "?",
                        d == slow ? " <-- CYCLE START" : "");
                d = d->next;
                if (d == slow && i > 0) break;
            }
        }
    }
    
    ast_t *cur = arg_node;

    /*
     * cparser often wraps Pascal arguments in synthetic tuple/statement nodes. Unwrap each
     * layer so we only convert real expressions and preserve field-width wrappers explicitly.
     */
    while (cur != NULL && cur != ast_nil) {
        /* Check for circular reference before processing */
        if (!is_safe_to_continue(visited, cur)) {
            fprintf(stderr, "ERROR: Circular reference detected in expression list, stopping traversal\n");
            break;
        }
        
        ast_t *unwrapped = unwrap_pascal_node(cur);
        if (unwrapped != NULL && unwrapped->typ == PASCAL_T_FIELD_WIDTH) {
            struct Expression *expr = convert_field_width_expr(unwrapped);
            if (expr != NULL)
                list_builder_append(&builder, expr, LIST_EXPR);
        } else if (unwrapped != NULL) {
            struct Expression *expr = convert_expression(unwrapped);
            if (expr != NULL)
                list_builder_append(&builder, expr, LIST_EXPR);
        }
        cur = cur->next;
    }

    visited_set_destroy(visited);
    return list_builder_finish(&builder);
}

static struct Expression *convert_field_width_expr(ast_t *field_width_node) {
    if (field_width_node == NULL)
        return NULL;

    ast_t *cursor = field_width_node;
    struct Expression *formats[2] = { NULL, NULL };
    int format_count = 0;

    while (cursor != NULL && cursor->typ == PASCAL_T_FIELD_WIDTH) {
        ast_t *base_node = cursor->child;
        ast_t *format_node = (base_node != NULL) ? base_node->next : NULL;

        if (format_node != NULL && format_node != ast_nil) {
            struct Expression *format_expr = convert_expression(format_node);
            if (format_expr != NULL) {
                if (format_count < 2) {
                    formats[format_count++] = format_expr;
                } else {
                    destroy_expr(formats[0]);
                    formats[0] = formats[1];
                    formats[1] = format_expr;
                    format_count = 2;
                }
            }
        }

        cursor = base_node;
    }

    struct Expression *base_expr = convert_expression(cursor);
    if (base_expr == NULL) {
        for (int i = 0; i < format_count; ++i) {
            if (formats[i] != NULL)
                destroy_expr(formats[i]);
        }
        return NULL;
    }

    if (format_count >= 1) {
        struct Expression *width_expr = formats[format_count - 1];
        if (base_expr->field_width == NULL)
            base_expr->field_width = width_expr;
        else if (width_expr != NULL)
            destroy_expr(width_expr);
    }

    if (format_count >= 2) {
        base_expr->field_precision = formats[format_count - 2];
    }

    return base_expr;
}

static struct Expression *convert_member_access_chain(int line,
    struct Expression *base_expr, ast_t *field_node);

static struct Expression *convert_member_access(ast_t *node) {
    if (node == NULL)
        return NULL;

    ast_t *base_node = node->child;
    ast_t *field_node = (base_node != NULL) ? base_node->next : NULL;
    ast_t *args_node = (field_node != NULL) ? field_node->next : NULL;
    if (kgpc_getenv("KGPC_DEBUG_SPECIALIZE_CALLS") != NULL &&
        node != NULL && (node->line == 255 || node->line == 256))
    {
        fprintf(stderr,
            "[KGPC_DEBUG_SPECIALIZE_CALLS] MEMBER_ACCESS line=%d base=%d(%s:%s) field=%d(%s:%s) args=%d(%s:%s)\n",
            node->line,
            base_node != NULL ? base_node->typ : -1,
            base_node != NULL ? pascal_tag_to_string(base_node->typ) : "<null>",
            (base_node != NULL && base_node->sym != NULL && base_node->sym->name != NULL)
                ? base_node->sym->name : "<null>",
            field_node != NULL ? field_node->typ : -1,
            field_node != NULL ? pascal_tag_to_string(field_node->typ) : "<null>",
            (field_node != NULL && field_node->sym != NULL && field_node->sym->name != NULL)
                ? field_node->sym->name : "<null>",
            args_node != NULL ? args_node->typ : -1,
            args_node != NULL ? pascal_tag_to_string(args_node->typ) : "<null>",
            (args_node != NULL && args_node->sym != NULL && args_node->sym->name != NULL)
                ? args_node->sym->name : "<null>");
        for (ast_t *it = field_node != NULL ? field_node->child : NULL; it != NULL; it = it->next)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_SPECIALIZE_CALLS]   field.child typ=%d(%s:%s) grandchild=%d(%s)\n",
                it->typ,
                pascal_tag_to_string(it->typ),
                (it->sym != NULL && it->sym->name != NULL) ? it->sym->name : "<null>",
                it->child != NULL ? it->child->typ : -1,
                it->child != NULL ? pascal_tag_to_string(it->child->typ) : "<null>");
        }
    }
    if (kgpc_getenv("KGPC_DEBUG_SPECIALIZE_CALLS") != NULL)
    {
        const char *field_sym = (field_node != NULL && field_node->sym != NULL &&
                                 field_node->sym->name != NULL)
            ? field_node->sym->name : "<null>";
        fprintf(stderr,
            "[KGPC_DEBUG_SPECIALIZE_CALLS] MEMBER_ACCESS base=%d(%s) field=%d(%s:%s) args=%d(%s)\n",
            base_node != NULL ? base_node->typ : -1,
            base_node != NULL ? pascal_tag_to_string(base_node->typ) : "<null>",
            field_node != NULL ? field_node->typ : -1,
            field_node != NULL ? pascal_tag_to_string(field_node->typ) : "<null>",
            field_sym,
            args_node != NULL ? args_node->typ : -1,
            args_node != NULL ? pascal_tag_to_string(args_node->typ) : "<null>");
        if (field_node != NULL && field_node->typ == PASCAL_T_FUNC_CALL && field_node->child != NULL)
        {
            ast_t *fc = field_node->child;
            const char *fc_sym = (fc->sym != NULL && fc->sym->name != NULL) ? fc->sym->name : "<null>";
            fprintf(stderr,
                "[KGPC_DEBUG_SPECIALIZE_CALLS]   MEMBER_ACCESS func child=%d(%s:%s)\n",
                fc->typ, pascal_tag_to_string(fc->typ), fc_sym);
            if (fc->child != NULL)
            {
                const char *fc2_sym = (fc->child->sym != NULL && fc->child->sym->name != NULL)
                    ? fc->child->sym->name : "<null>";
                fprintf(stderr,
                    "[KGPC_DEBUG_SPECIALIZE_CALLS]   MEMBER_ACCESS func child->child=%d(%s:%s)\n",
                    fc->child->typ, pascal_tag_to_string(fc->child->typ), fc2_sym);
            }
        }
    }

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_member_access: base=%d field=%d args=%d\n",
            base_node ? base_node->typ : -1,
            field_node ? field_node->typ : -1,
            args_node ? args_node->typ : -1);
    }

    /* Check for function call (MEMBER_ACCESS with ARG_LIST as 3rd child) */
    if (args_node != NULL && args_node->typ == PASCAL_T_ARG_LIST) {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_member_access: detected ARG_LIST child, converting to function call\n");
        }
        
        struct Expression *base_expr = convert_expression(base_node);
        char *method_id = dup_symbol(field_node);
        
        /* Convert args */
        ListNode_t *args_list = NULL;
        ListNode_t *tail = NULL;
        ast_t *arg_child = args_node->child;
        while (arg_child != NULL) {
            struct Expression *arg_expr = convert_expression(arg_child);
            if (arg_expr != NULL) {
                ListNode_t *node = CreateListNode(arg_expr, LIST_EXPR);
                if (args_list == NULL) {
                    args_list = node;
                    tail = node;
                } else {
                    tail->next = node;
                    tail = node;
                }
            }
            arg_child = arg_child->next;
        }
        
        /* Prepend base expression to args */
        args_list = PushListNodeFront(args_list, CreateListNode(base_expr, LIST_EXPR));
        
        struct Expression *call_expr = (struct Expression *)calloc(1, sizeof(struct Expression));
        call_expr->line_num = node->line;
        call_expr->type = EXPR_FUNCTION_CALL;
        call_expr->expr_data.function_call_data.id = method_id;
        call_expr->expr_data.function_call_data.args_expr = args_list;
        call_expr->expr_data.function_call_data.resolved_func = NULL;
        call_expr->expr_data.function_call_data.is_method_call_placeholder = 1;
        call_expr->expr_data.function_call_data.placeholder_method_name = strdup(method_id);

        return call_expr;
    }

    struct Expression *record_expr = convert_expression(base_node);
    if (record_expr == NULL)
        return NULL;

    return convert_member_access_chain(node->line, record_expr, field_node);
}

static struct Expression *convert_member_access_chain(int line,
    struct Expression *base_expr, ast_t *field_node) {
    if (base_expr == NULL)
        return NULL;

    if (field_node == NULL) {
        destroy_expr(base_expr);
        return NULL;
    }

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_member_access_chain: field_node typ=%d, has_next=%d\n",
            field_node->typ, field_node->next != NULL);
        if (field_node->next != NULL) {
            fprintf(stderr, "[KGPC]   next typ=%d\n", field_node->next->typ);
        }
        if (field_node->child != NULL) {
            fprintf(stderr, "[KGPC]   child typ=%d\n", field_node->child->typ);
            if (field_node->child->next != NULL) {
                fprintf(stderr, "[KGPC]   child->next typ=%d\n", field_node->child->next->typ);
            }
        }
    }

    ast_t *unwrapped = unwrap_pascal_node(field_node);
    if (unwrapped == NULL)
        unwrapped = field_node;

    /* Check if this is PASCAL_T_FUNC_CALL (type 43) */
    if (unwrapped != NULL && unwrapped->typ == PASCAL_T_FUNC_CALL) {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_member_access_chain: detected FUNC_CALL, converting to method call\n");
        }
        
        ast_t *method_id_node = unwrapped->child;
        ast_t *args_node = (method_id_node != NULL) ? method_id_node->next : NULL;
        
        char *method_id = NULL;
        char *placeholder_name = NULL;
        if (method_id_node != NULL) {
            ast_t *method_unwrapped = unwrap_pascal_node(method_id_node);
            if (method_unwrapped == NULL)
                method_unwrapped = method_id_node;
            if (kgpc_getenv("KGPC_DEBUG_SPECIALIZE_CALLS") != NULL &&
                method_unwrapped != NULL &&
                method_unwrapped->typ == PASCAL_T_IDENTIFIER &&
                method_unwrapped->sym != NULL &&
                method_unwrapped->sym->name != NULL &&
                strcasecmp(method_unwrapped->sym->name, "specialize") == 0)
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_SPECIALIZE_CALLS] METHOD specialize-shape line=%d base_expr_type=%d args_node=%d(%s)\n",
                    line,
                    base_expr != NULL ? base_expr->type : -1,
                    args_node != NULL ? args_node->typ : -1,
                    args_node != NULL ? pascal_tag_to_string(args_node->typ) : "<null>");
                for (ast_t *dbg = args_node, *it = dbg; it != NULL; it = it->next)
                {
                    const char *sym = (it->sym != NULL && it->sym->name != NULL) ? it->sym->name : "<null>";
                    fprintf(stderr,
                        "[KGPC_DEBUG_SPECIALIZE_CALLS]   method-arg node typ=%d(%s) sym=%s child=%d(%s)\n",
                        it->typ, pascal_tag_to_string(it->typ), sym,
                        it->child != NULL ? it->child->typ : -1,
                        it->child != NULL ? pascal_tag_to_string(it->child->typ) : "<null>");
                }
            }
            if (method_unwrapped->typ == PASCAL_T_IDENTIFIER) {
                method_id = dup_symbol(method_unwrapped);
                if (method_id != NULL)
                    placeholder_name = strdup(method_id);
            } else if (method_unwrapped->typ == PASCAL_T_CONSTRUCTED_TYPE) {
                char *base_name = NULL;
                ListNode_t *type_args = NULL;
                if (extract_constructed_type_info(method_unwrapped, &base_name, &type_args)) {
                    if (base_name != NULL) {
                        method_id = mangle_specialized_name_from_list(base_name, type_args);
                        if (method_id == NULL)
                            method_id = strdup(base_name);
                        placeholder_name = strdup(base_name);
                    }
                }
                if (base_name != NULL)
                    free(base_name);
                if (type_args != NULL)
                    destroy_list(type_args);
            }
        }

        if (method_id != NULL) {
            
            /* Convert args - handle both ARG_LIST and direct siblings */
            ListNode_t *args_list = NULL;
            if (args_node != NULL && args_node->typ == PASCAL_T_ARG_LIST) {
                /* Arguments wrapped in ARG_LIST node - iterate through children */
                ast_t *arg_child = args_node->child;
                ListNode_t *tail = NULL;
                while (arg_child != NULL) {
                    struct Expression *arg_expr = convert_expression(arg_child);
                    if (arg_expr != NULL) {
                        ListNode_t *new_node = CreateListNode(arg_expr, LIST_EXPR);
                        if (args_list == NULL) {
                            args_list = new_node;
                            tail = new_node;
                        } else {
                            if (tail == NULL) {
                                // Find tail if we're appending to existing list
                                tail = args_list;
                                while (tail->next != NULL) tail = tail->next;
                            }
                            tail->next = new_node;
                            tail = new_node;
                        }
                    }
                    arg_child = arg_child->next;
                }
            } else if (args_node != NULL) {
                /* Arguments as direct siblings - iterate through all siblings */
                ast_t *arg_child = args_node;
                ListNode_t *tail = NULL;
                while (arg_child != NULL) {
                    struct Expression *arg_expr = convert_expression(arg_child);
                    if (arg_expr != NULL) {
                        ListNode_t *new_node = CreateListNode(arg_expr, LIST_EXPR);
                        if (args_list == NULL) {
                            args_list = new_node;
                            tail = new_node;
                        } else {
                            if (tail == NULL) {
                                tail = args_list;
                                while (tail->next != NULL) tail = tail->next;
                            }
                            tail->next = new_node;
                            tail = new_node;
                        }
                    }
                    arg_child = arg_child->next;
                }
            }
            /* If args_node is NULL, args_list remains NULL (zero arguments) */
            
            /* Prepend base expression to args */
            ListNode_t *base_node_list = CreateListNode(base_expr, LIST_EXPR);
            if (args_list == NULL) {
                args_list = base_node_list;
            } else {
                args_list = PushListNodeFront(args_list, base_node_list);
            }
            
            struct Expression *call_expr = (struct Expression *)calloc(1, sizeof(struct Expression));
            call_expr->line_num = line;
            call_expr->type = EXPR_FUNCTION_CALL;
            call_expr->expr_data.function_call_data.id = method_id;
            call_expr->expr_data.function_call_data.args_expr = args_list;
            call_expr->expr_data.function_call_data.resolved_func = NULL;
            call_expr->resolved_kgpc_type = NULL;
            call_expr->expr_data.function_call_data.is_method_call_placeholder = 1;
            call_expr->expr_data.function_call_data.placeholder_method_name =
                (placeholder_name != NULL) ? placeholder_name : strdup(method_id);
            placeholder_name = NULL;

            return call_expr;
        }
        if (placeholder_name != NULL)
            free(placeholder_name);
    }

    /* Check if field_node has ARG_LIST as sibling (method call) */
    if (unwrapped != NULL && unwrapped->next != NULL && unwrapped->next->typ == PASCAL_T_ARG_LIST) {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_member_access_chain: detected ARG_LIST sibling, converting to function call\n");
        }
        
        char *method_id = dup_symbol(unwrapped);
        
        /* Convert args from ARG_LIST */
        ListNode_t *args_list = NULL;
        ListNode_t *tail = NULL;
        ast_t *arg_child = unwrapped->next->child;
        while (arg_child != NULL) {
            struct Expression *arg_expr = convert_expression(arg_child);
            if (arg_expr != NULL) {
                ListNode_t *node = CreateListNode(arg_expr, LIST_EXPR);
                if (args_list == NULL) {
                    args_list = node;
                    tail = node;
                } else {
                    tail->next = node;
                    tail = node;
                }
            }
            arg_child = arg_child->next;
        }
        
        /* Prepend base expression to args */
        args_list = PushListNodeFront(args_list, CreateListNode(base_expr, LIST_EXPR));
        
        struct Expression *call_expr = (struct Expression *)calloc(1, sizeof(struct Expression));
        call_expr->line_num = line;
        call_expr->type = EXPR_FUNCTION_CALL;
        call_expr->expr_data.function_call_data.id = method_id;
        call_expr->expr_data.function_call_data.args_expr = args_list;
        call_expr->expr_data.function_call_data.resolved_func = NULL;
        call_expr->expr_data.function_call_data.is_method_call_placeholder = 1;
        call_expr->expr_data.function_call_data.placeholder_method_name = strdup(method_id);

        return call_expr;
    }

    if (unwrapped == NULL)
        unwrapped = field_node;

    if (unwrapped == NULL) {
        destroy_expr(base_expr);
        return NULL;
    }

    int node_line = (unwrapped->line != 0) ? unwrapped->line : line;

    switch (unwrapped->typ) {
    case PASCAL_T_TYPECAST: {
        /* Static/generic class method syntax may be encoded as a TYPECAST
         * in the member field position:
         *   TMarshal.specialize FixArray<T>(Arr)
         * => MEMBER_ACCESS(base=TMarshal, field=TYPECAST(CONSTRUCTED_TYPE(FixArray<T>), Arr))
         */
        ast_t *cast_type_node = unwrapped->child;
        ast_t *cast_value_node = (cast_type_node != NULL) ? cast_type_node->next : NULL;
        ast_t *cast_type = unwrap_pascal_node(cast_type_node);
        if (cast_type == NULL)
            cast_type = cast_type_node;

        char *method_id = NULL;
        char *placeholder_name = NULL;
        if (cast_type != NULL && cast_type->typ == PASCAL_T_IDENTIFIER) {
            method_id = dup_symbol(cast_type);
            if (method_id != NULL)
                placeholder_name = strdup(method_id);
        } else if (cast_type != NULL && cast_type->typ == PASCAL_T_CONSTRUCTED_TYPE) {
            char *base_name = NULL;
            ListNode_t *type_args = NULL;
            if (extract_constructed_type_info(cast_type, &base_name, &type_args)) {
                if (base_name != NULL) {
                    method_id = mangle_specialized_name_from_list(base_name, type_args);
                    if (method_id == NULL)
                        method_id = strdup(base_name);
                    placeholder_name = strdup(base_name);
                }
            }
            if (base_name != NULL)
                free(base_name);
            if (type_args != NULL)
                destroy_list(type_args);
        }

        if (method_id == NULL) {
            destroy_expr(base_expr);
            return NULL;
        }

        ListNode_t *args_list = NULL;
        ListNode_t *tail = NULL;
        if (cast_value_node != NULL && cast_value_node->typ == PASCAL_T_ARG_LIST) {
            for (ast_t *arg = cast_value_node->child; arg != NULL; arg = arg->next) {
                struct Expression *arg_expr = convert_expression(arg);
                if (arg_expr == NULL)
                    continue;
                ListNode_t *new_node = CreateListNode(arg_expr, LIST_EXPR);
                if (new_node == NULL)
                {
                    destroy_expr(arg_expr);
                    continue;
                }
                if (args_list == NULL) {
                    args_list = new_node;
                    tail = new_node;
                } else {
                    tail->next = new_node;
                    tail = new_node;
                }
            }
        } else if (cast_value_node != NULL) {
            /* Convert all argument expressions (siblings linked via ->next).
             * With sep_by in the parser, multi-argument specialize calls
             * produce sibling nodes rather than a single expression. */
            for (ast_t *arg = cast_value_node; arg != NULL; arg = arg->next) {
                struct Expression *arg_expr = convert_expression(arg);
                if (arg_expr == NULL)
                    continue;
                ListNode_t *new_node = CreateListNode(arg_expr, LIST_EXPR);
                if (new_node == NULL)
                {
                    destroy_expr(arg_expr);
                    continue;
                }
                if (args_list == NULL) {
                    args_list = new_node;
                    tail = new_node;
                } else {
                    tail->next = new_node;
                    tail = new_node;
                }
            }
        }

        args_list = PushListNodeFront(args_list, CreateListNode(base_expr, LIST_EXPR));
        struct Expression *call_expr = mk_functioncall(line, method_id, args_list);
        if (call_expr != NULL) {
            call_expr->expr_data.function_call_data.is_method_call_placeholder = 1;
            call_expr->expr_data.function_call_data.placeholder_method_name =
                (placeholder_name != NULL) ? placeholder_name : strdup(method_id);
            placeholder_name = NULL;
        }
        if (placeholder_name != NULL)
            free(placeholder_name);
        return call_expr;
    }
    case PASCAL_T_IDENTIFIER: {
        char *field_id = dup_symbol(unwrapped);
        if (field_id == NULL) {
            destroy_expr(base_expr);
            return NULL;
        }
        return mk_recordaccess(node_line, base_expr, field_id);
    }
    case PASCAL_T_BOOLEAN: {
        /* Handle scoped enum literals like TUseBoolStrs.False or TUseBoolStrs.True
         * The parser treats 'False' and 'True' as boolean keywords, but in the context
         * of a member access (after a dot), they should be treated as identifiers
         * that reference enum literals. */
        char *field_id = dup_symbol(unwrapped);
        if (field_id == NULL) {
            destroy_expr(base_expr);
            return NULL;
        }
        return mk_recordaccess(node_line, base_expr, field_id);
    }
    case PASCAL_T_ARRAY_ACCESS: {
        ast_t *array_base = unwrapped->child;
        ast_t *index_node = (array_base != NULL) ? array_base->next : NULL;
        struct Expression *field_expr = convert_member_access_chain(node_line, base_expr, array_base);
        if (field_expr == NULL)
            return NULL;

        struct Expression *index_expr = convert_expression(index_node);
        if (index_expr == NULL) {
            destroy_expr(field_expr);
            return NULL;
        }

        struct Expression *result = mk_arrayaccess(node_line, field_expr, index_expr);
        if (result == NULL)
            return NULL;

        if (index_node != NULL && index_node->next != NULL)
        {
            ast_t *extra_idx_node = index_node->next;
            ListNode_t *extra_indices = NULL;
            ListNode_t *extra_tail = NULL;
            while (extra_idx_node != NULL)
            {
                struct Expression *extra_idx = convert_expression(extra_idx_node);
                if (extra_idx != NULL)
                {
                    ListNode_t *new_node = CreateListNode(extra_idx, LIST_EXPR);
                    if (new_node != NULL)
                    {
                        if (extra_tail == NULL)
                        {
                            extra_indices = new_node;
                            extra_tail = new_node;
                        }
                        else
                        {
                            extra_tail->next = new_node;
                            extra_tail = new_node;
                        }
                    }
                }
                extra_idx_node = extra_idx_node->next;
            }
            result->expr_data.array_access_data.extra_indices = extra_indices;
        }

        return result;
    }
    case PASCAL_T_MEMBER_ACCESS: {
        ast_t *inner_base = unwrapped->child;
        ast_t *inner_field = (inner_base != NULL) ? inner_base->next : NULL;
        struct Expression *next_base = convert_member_access_chain(node_line, base_expr, inner_base);
        if (next_base == NULL)
            return NULL;
        return convert_member_access_chain(node_line, next_base, inner_field);
    }
    case PASCAL_T_DEREF: {
        ast_t *inner = unwrapped->child;
        struct Expression *target = convert_member_access_chain(node_line, base_expr, inner);
        if (target == NULL)
            return NULL;
        return mk_pointer_deref(node_line, target);
    }
    default:
        break;
    }

    if (unwrapped->child != NULL) {
        ast_t *child_id = unwrap_pascal_node(unwrapped->child);
        if (child_id != NULL && child_id->typ == PASCAL_T_IDENTIFIER) {
            char *field_id = dup_symbol(child_id);
            if (field_id == NULL) {
                destroy_expr(base_expr);
                return NULL;
            }
            return mk_recordaccess(node_line, base_expr, field_id);
        }
    }

    destroy_expr(base_expr);
    return NULL;
}

static struct Statement *convert_assignment(ast_t *assign_node) {
    ast_t *lhs = assign_node->child;
    ast_t *rhs = lhs != NULL ? lhs->next : NULL;

    struct Expression *left = convert_expression(lhs);
    struct Expression *right = convert_expression(rhs);
    if (kgpc_getenv("KGPC_DEBUG_SPECIALIZE_CALLS") != NULL && right == NULL)
    {
        ast_t *u_rhs = unwrap_pascal_node(rhs);
        ast_t *dbg_rhs = (u_rhs != NULL) ? u_rhs : rhs;
        fprintf(stderr,
            "[KGPC_DEBUG_SPECIALIZE_CALLS] ASSIGN rhs-convert-null line=%d lhs_typ=%d rhs_typ=%d(%s) rhs_sym=%s rhs_child_typ=%d(%s)\n",
            assign_node != NULL ? assign_node->line : -1,
            lhs != NULL ? lhs->typ : -1,
            dbg_rhs != NULL ? dbg_rhs->typ : -1,
            dbg_rhs != NULL ? pascal_tag_to_string(dbg_rhs->typ) : "<null>",
            (dbg_rhs != NULL && dbg_rhs->sym != NULL && dbg_rhs->sym->name != NULL)
                ? dbg_rhs->sym->name : "<null>",
            (dbg_rhs != NULL && dbg_rhs->child != NULL) ? dbg_rhs->child->typ : -1,
            (dbg_rhs != NULL && dbg_rhs->child != NULL)
                ? pascal_tag_to_string(dbg_rhs->child->typ) : "<null>");
    }
    if (kgpc_getenv("KGPC_DEBUG_SPECIALIZE_CALLS") != NULL &&
        assign_node != NULL && assign_node->line == 256)
    {
        ast_t *u_rhs = unwrap_pascal_node(rhs);
        ast_t *dbg_rhs = (u_rhs != NULL) ? u_rhs : rhs;
        fprintf(stderr,
            "[KGPC_DEBUG_SPECIALIZE_CALLS] ASSIGN line=256 rhs=%d(%s:%s)\n",
            dbg_rhs != NULL ? dbg_rhs->typ : -1,
            dbg_rhs != NULL ? pascal_tag_to_string(dbg_rhs->typ) : "<null>",
            (dbg_rhs != NULL && dbg_rhs->sym != NULL && dbg_rhs->sym->name != NULL)
                ? dbg_rhs->sym->name : "<null>");
        for (ast_t *it = rhs != NULL ? rhs->next : NULL; it != NULL; it = it->next)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_SPECIALIZE_CALLS]   ASSIGN raw rhs.next typ=%d(%s:%s) child=%d(%s)\n",
                it->typ,
                pascal_tag_to_string(it->typ),
                (it->sym != NULL && it->sym->name != NULL) ? it->sym->name : "<null>",
                it->child != NULL ? it->child->typ : -1,
                it->child != NULL ? pascal_tag_to_string(it->child->typ) : "<null>");
        }
        for (ast_t *it = dbg_rhs != NULL ? dbg_rhs->next : NULL; it != NULL; it = it->next)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_SPECIALIZE_CALLS]   ASSIGN rhs.next typ=%d(%s:%s) child=%d(%s)\n",
                it->typ,
                pascal_tag_to_string(it->typ),
                (it->sym != NULL && it->sym->name != NULL) ? it->sym->name : "<null>",
                it->child != NULL ? it->child->typ : -1,
                it->child != NULL ? pascal_tag_to_string(it->child->typ) : "<null>");
        }
    }
    struct Statement *stmt = mk_varassign(assign_node->line, assign_node->col, left, right);
    if (stmt != NULL && assign_node->index >= 0) {
        stmt->source_index = assign_node->index + g_source_offset;
    }
    return stmt;
}

static struct Statement *convert_proc_call(ast_t *call_node, bool implicit_identifier) {
    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_proc_call: typ=%d line=%d\n",
            call_node ? call_node->typ : -1, call_node ? call_node->line : -1);
        if (call_node && call_node->child) {
            fprintf(stderr, "[KGPC]   child typ=%d\n", call_node->child->typ);
        }
    }
    ast_t *child = call_node->child;
    ast_t *args_start = NULL;
    char *id = NULL;

    if (child != NULL && child->typ == PASCAL_T_MEMBER_ACCESS) {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC]   Handling MEMBER_ACCESS\n");
        }
        /* Check for unit-qualified procedure calls (e.g. System.SetLength).
         * If the left side is a known unit name, create a direct procedure call
         * with call_qualifier set instead of a method call placeholder. */
        ast_t *lhs = child->child;
        ast_t *rhs = (lhs != NULL) ? lhs->next : NULL;
        if (lhs != NULL && lhs->typ == PASCAL_T_IDENTIFIER && rhs != NULL)
        {
            char *lhs_name = dup_symbol(lhs);
            if (lhs_name != NULL && unit_registry_contains(lhs_name))
            {
                /* Extract the procedure name from rhs */
                ast_t *method_ident = unwrap_pascal_node(rhs);
                ast_t *call_args = child->next;
                if (method_ident != NULL && method_ident->typ == PASCAL_T_FUNC_CALL)
                {
                    call_args = method_ident->child ? method_ident->child->next : NULL;
                    method_ident = method_ident->child;
                    if (method_ident != NULL)
                        method_ident = unwrap_pascal_node(method_ident);
                }
                if (method_ident != NULL && method_ident->typ == PASCAL_T_IDENTIFIER)
                {
                    char *proc_name = dup_symbol(method_ident);
                    ListNode_t *args = convert_expression_list(call_args);
                    struct Statement *call = mk_procedurecall(call_node->line, proc_name, args);
                    if (call != NULL) {
                        if (call_node->index >= 0)
                            call->source_index = call_node->index + g_source_offset;
                        call->stmt_data.procedure_call_data.call_qualifier = lhs_name;
                    } else {
                        free(lhs_name);
                    }
                    return call;
                }
            }
            free(lhs_name);
        }
        ast_t *args_node = child->next;
        struct Statement *method_stmt = convert_method_call_statement(child, args_node);
        if (method_stmt != NULL) {
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC]   convert_method_call_statement returned statement\n");
            }
            return method_stmt;
        }
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC]   convert_method_call_statement returned NULL\n");
        }
    }

    if (call_node->typ == PASCAL_T_IDENTIFIER) {
        id = dup_symbol(call_node);
        args_start = call_node->next;
    } else if (child != NULL) {
        if (implicit_identifier && child->typ == PASCAL_T_IDENTIFIER) {
            id = dup_symbol(child);
            args_start = child->next;
        } else {
            if (child->typ == PASCAL_T_IDENTIFIER) {
                id = dup_symbol(child);
                args_start = child->next;
            } else if (child->typ == PASCAL_T_TYPECAST && child->child != NULL &&
                       child->child->typ == PASCAL_T_IDENTIFIER) {
                /* Handle chained call pattern: TypeName(source)(args)
                 * Parser creates: FUNC_CALL -> TYPECAST(TypeName, source) -> outer args
                 * child is TYPECAST with: child->child = IDENTIFIER(TypeName), rest = source args
                 * call_node has: child->next = outer call args
                 * We need to preserve both: create a procedure call where the first arg
                 * is the typecast source, followed by the outer call args. */
                id = dup_symbol(child->child);
                /* Build args: inner args (typecast source) + outer args (call args) */
                ast_t *inner_args = child->child->next;
                ast_t *outer_args = child->next;
                if (inner_args != NULL) {
                    /* Chain inner args before outer args */
                    ast_t *tail = inner_args;
                    while (tail->next != NULL)
                        tail = tail->next;
                    tail->next = outer_args;
                    args_start = inner_args;
                } else {
                    args_start = outer_args;
                }
            } else if (child->child != NULL && child->child->typ == PASCAL_T_IDENTIFIER) {
                id = dup_symbol(child->child);
                args_start = child->next;
            }
        }
    }

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC]   id=%s\n", id ? id : "(null)");
    }

    ListNode_t *args = convert_expression_list(args_start);
    struct Statement *call = mk_procedurecall(call_node->line, id, args);
    if (call != NULL) {
        if (call_node->index >= 0)
            call->source_index = call_node->index + g_source_offset;
        /* If id starts with __, it's a method call placeholder from convert_method_call_statement.
         * Extract the bare method name and set the structured flag. */
        if (id != NULL && strncmp(id, "__", 2) == 0) {
            call->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
            call->stmt_data.procedure_call_data.placeholder_method_name = strdup(id + 2);
        }
    }
    return call;
}

static struct Statement *convert_method_call_statement(ast_t *member_node, ast_t *args_start) {
    if (member_node == NULL)
        return NULL;

    ast_t *object_node = member_node->child;
    ast_t *field_node = (object_node != NULL) ? object_node->next : NULL;
    if (field_node == NULL)
        return NULL;

    ast_t *identifier_node = unwrap_pascal_node(field_node);
    if (identifier_node == NULL)
        identifier_node = field_node;

    /* Handle the case where field_node is a FUNC_CALL: obj.method(args)
     * In this case, we need to extract the method name from the FUNC_CALL node */
    ast_t *method_name_node = identifier_node;
    if (identifier_node != NULL && identifier_node->typ == PASCAL_T_FUNC_CALL) {
        /* The function name is the first child of the FUNC_CALL node */
        method_name_node = identifier_node->child;
        if (method_name_node != NULL) {
            method_name_node = unwrap_pascal_node(method_name_node);
        }
        /* args_start should be the second child (the arguments) if not already provided */
        if (args_start == NULL && identifier_node->child != NULL) {
            args_start = identifier_node->child->next;
        }
    }

    if (method_name_node == NULL || method_name_node->typ != PASCAL_T_IDENTIFIER)
        return NULL;

    char *method_name = dup_symbol(method_name_node);
    if (method_name == NULL)
        return NULL;

    /* For method calls, we need the object's type to determine which class's
     * method to call. However, at parse time we don't have full type information.
     * 
     * Solution: Don't mangle the name during parsing. Instead, store the unmangled
     * method name and let the semantic checker resolve it based on the object's type.
     * 
     * For now, we'll use find_class_for_method as a heuristic to try to determine
     * the correct class. This works when there's only one class with this method,
     * but fails when multiple classes have the same method name (polymorphism).
     * 
     * A proper fix would require semantic analysis to resolve the method name
     * based on the actual object type.
     */
    /* Disable heuristic - always use semantic resolution */
    const char *class_name = NULL; /* find_class_for_method(method_name); */
    
    char *proc_name = NULL;
    if (class_name != NULL) {
        proc_name = mangle_method_name(class_name, method_name);
    } else {
        /* No class found at parse time. Create a placeholder name that semantic checker
         * will resolve based on the object's type. Use format "__methodname" to indicate
         * this is a method call that needs type-based resolution. */
        size_t len = strlen(method_name) + 3;  /* __ + name + \0 */
        proc_name = (char *)malloc(len);
        if (proc_name != NULL) {
            snprintf(proc_name, len, "__%s", method_name);
        }
    }
    
    if (proc_name == NULL) {
        free(method_name);
        return NULL;
    }

    struct Expression *object_expr = convert_expression(object_node);
    if (object_expr == NULL) {
        free(proc_name);
        free(method_name);
        return NULL;
    }

    ListBuilder arg_builder;
    list_builder_init(&arg_builder);
    list_builder_append(&arg_builder, object_expr, LIST_EXPR);

    if (args_start != NULL) {
        ListNode_t *converted_args = convert_expression_list(args_start);
        list_builder_extend(&arg_builder, converted_args);
    }

    ListNode_t *args = list_builder_finish(&arg_builder);
    struct Statement *call = mk_procedurecall(member_node->line, proc_name, args);
    if (call != NULL) {
        call->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
        call->stmt_data.procedure_call_data.placeholder_method_name = method_name;
    } else {
        free(method_name);
    }
    return call;
}

static struct Statement *build_nested_with_statements(int line,
                                                      ast_t *context_node,
                                                      struct Statement *body_stmt) {
    if (context_node == NULL || context_node == ast_nil)
        return NULL;

    ast_t *next_context = context_node->next;
    struct Statement *inner_stmt = body_stmt;
    if (next_context != NULL && next_context != ast_nil) {
        inner_stmt = build_nested_with_statements(line, next_context, body_stmt);
        if (inner_stmt == NULL)
            return NULL;
    }

    ast_t *unwrapped = unwrap_pascal_node(context_node);
    struct Expression *expr = convert_expression(unwrapped);
    if (expr == NULL)
        return NULL;

    return mk_with(line, expr, inner_stmt);
}

