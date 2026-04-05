#include "../from_cparser_internal.h"

struct Statement *convert_statement(ast_t *stmt_node) {
    stmt_node = unwrap_pascal_node(stmt_node);
    if (stmt_node == NULL)
    {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL)
            fprintf(stderr, "[KGPC] convert_statement: stmt_node NULL after unwrap\n");
        return NULL;
    }
    /* PASCAL_T_NONE with NULL child means empty statement - skip it */
    if (stmt_node->typ == PASCAL_T_NONE && stmt_node->child == NULL)
        return NULL;
    switch (stmt_node->typ) {
    case PASCAL_T_ASSIGNMENT:
        return convert_assignment(stmt_node);
    case PASCAL_T_LABEL_STMT: {
        ast_t *label_node = stmt_node->child;
        ast_t *stmt_part = NULL;
        if (label_node != NULL && label_node != ast_nil)
            stmt_part = label_node->next;

        ast_t *unwrapped_label = unwrap_pascal_node(label_node);
        char *label = NULL;
        if (unwrapped_label != NULL && unwrapped_label != ast_nil)
            label = dup_symbol(unwrapped_label);

        struct Statement *inner_stmt = convert_statement(unwrap_pascal_node(stmt_part));
        return mk_label(stmt_node->line, label, inner_stmt);
    }
    case PASCAL_T_GOTO_STMT: {
        ast_t *label_node = unwrap_pascal_node(stmt_node->child);
        char *label = NULL;
        if (label_node != NULL && label_node != ast_nil)
            label = dup_symbol(label_node);
        return mk_goto(stmt_node->line, label);
    }
    case PASCAL_T_STATEMENT_LIST: {
        /* A STATEMENT_LIST appearing as a statement — flatten as compound statement */
        ast_t *inner = stmt_node->child;
        if (inner == NULL)
            return NULL;
        ListNode_t *stmts = convert_statement_list(inner);
        return mk_compoundstatement(stmt_node->line, stmts);
    }
    case PASCAL_T_STATEMENT: {
        ast_t *inner = unwrap_pascal_node(stmt_node->child);
        if (inner == NULL)
            return NULL;
        if (inner->typ == PASCAL_T_IDENTIFIER) {
            char *name = dup_symbol(inner);
            if (name != NULL) {
                bool is_assembler = strcasecmp(name, "assembler") == 0;
                free(name);
                if (is_assembler)
                    return NULL;
            }
        }
        if (inner->typ == PASCAL_T_IDENTIFIER || inner->typ == PASCAL_T_FUNC_CALL)
            return convert_proc_call(inner, inner->typ == PASCAL_T_IDENTIFIER);
        if (inner->typ == PASCAL_T_MEMBER_ACCESS) {
            struct Statement *method_stmt = convert_method_call_statement(inner, NULL);
            if (method_stmt != NULL)
                return method_stmt;
        }
        {
            struct Expression *expr = convert_expression(inner);
            if (expr != NULL)
                return mk_exprstmt(stmt_node->line, stmt_node->col, expr);
        }
        return convert_statement(inner);
    }
    case PASCAL_T_FUNC_CALL:
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL)
            fprintf(stderr, "[KGPC] convert_statement: FUNC_CALL at line %d\n", stmt_node->line);
        return convert_proc_call(stmt_node, true);
    case PASCAL_T_MEMBER_ACCESS: {
        struct Statement *method_stmt = convert_method_call_statement(stmt_node, NULL);
        if (method_stmt != NULL)
            return method_stmt;
        return NULL;
    }
    case PASCAL_T_ASM_BLOCK: {
        /* stmt_node->child is the asm body (PASCAL_T_NONE with asm text
         * in sym->name).  Its ->next siblings may include clobber reglist
         * nodes ['eax', 'ebx', ...] which must NOT be collected as asm.
         * Temporarily sever the sibling chain to isolate the body. */
        ast_t *asm_body_node = stmt_node->child;
        assert(asm_body_node != NULL && asm_body_node->typ == PASCAL_T_NONE);
        ast_t *saved_next = asm_body_node->next;
        asm_body_node->next = NULL;
        char *code = collect_asm_text(asm_body_node);
        asm_body_node->next = saved_next;
        return mk_asmblock(stmt_node->line, code);
    }
    case PASCAL_T_BREAK_STMT:
        return mk_break(stmt_node->line);
    case PASCAL_T_CONTINUE_STMT:
        return mk_continue(stmt_node->line);
    case PASCAL_T_BEGIN_BLOCK:
        return convert_block(stmt_node);
    case PASCAL_T_IF_STMT: {
        ast_t *cond = stmt_node->child;
        ast_t *then_wrapper = cond != NULL ? cond->next : NULL;
        ast_t *else_wrapper = then_wrapper != NULL ? then_wrapper->next : NULL;

        struct Expression *condition = convert_expression(cond);
        struct Statement *then_stmt = convert_statement(unwrap_pascal_node(then_wrapper));
        struct Statement *else_stmt = convert_statement(unwrap_pascal_node(else_wrapper));
        return mk_ifthen(stmt_node->line, condition, then_stmt, else_stmt);
    }
    case PASCAL_T_WHILE_STMT: {
        ast_t *cond = stmt_node->child;
        ast_t *body = unwrap_pascal_node(cond != NULL ? cond->next : NULL);
        struct Expression *condition = convert_expression(cond);
        struct Statement *body_stmt = convert_statement(body);
        return mk_while(stmt_node->line, condition, body_stmt);
    }
    case PASCAL_T_REPEAT_STMT: {
        ast_t *body_wrapper = stmt_node->child;
        ast_t *cond_node = body_wrapper != NULL ? body_wrapper->next : NULL;

        ListNode_t *body_list = NULL;
        if (body_wrapper != NULL) {
            ast_t *body_nodes = body_wrapper->child != NULL ? body_wrapper->child : body_wrapper;
            body_list = convert_statement_list(body_nodes);
        }

        struct Expression *condition = convert_expression(cond_node);
        return mk_repeat(stmt_node->line, body_list, condition);
    }
    case PASCAL_T_FOR_STMT: {
        ast_t *init_node = unwrap_pascal_node(stmt_node->child);
        ast_t *direction_wrapper = init_node != NULL ? init_node->next : NULL;
        ast_t *direction_node = unwrap_pascal_node(direction_wrapper);
        ast_t *end_wrapper = direction_wrapper != NULL ? direction_wrapper->next : NULL;
        ast_t *end_node = unwrap_pascal_node(end_wrapper);
        ast_t *body_node = unwrap_pascal_node(end_wrapper != NULL ? end_wrapper->next : NULL);

        // Determine if this is a downto loop based on the direction node
        int is_downto = 0;
        if (direction_node != NULL && direction_node->typ == PASCAL_T_DOWNTO) {
            is_downto = 1;
        }

        struct Expression *end_expr = convert_expression(end_node);
        struct Statement *body_stmt = convert_statement(body_node);

        if (init_node != NULL && init_node->typ == PASCAL_T_ASSIGNMENT) {
            struct Statement *assign_stmt = convert_assignment(init_node);
            return mk_forassign(stmt_node->line, assign_stmt, end_expr, body_stmt, is_downto);
        }

        struct Expression *var_expr = convert_expression(init_node);
        if (var_expr == NULL)
            return NULL;
        return mk_forvar(stmt_node->line, var_expr, end_expr, body_stmt, is_downto);
    }
    case PASCAL_T_FOR_IN_STMT: {
        // Pattern matches for_stmt: unwrap the nested NONE seq, then walk siblings
        ast_t *id_node = unwrap_pascal_node(stmt_node->child);
        ast_t *expr_wrapper = id_node != NULL ? id_node->next : NULL;
        ast_t *expr_node = unwrap_pascal_node(expr_wrapper);
        ast_t *body_wrapper = expr_wrapper != NULL ? expr_wrapper->next : NULL;
        ast_t *body_node = unwrap_pascal_node(body_wrapper);
        
        #ifdef DEBUG_FOR_IN_STATEMENT
        fprintf(stderr, "DEBUG FOR_IN: id=%p typ=%d, expr=%p typ=%d, body=%p typ=%d\n",
                (void*)id_node, id_node ? id_node->typ : -1,
                (void*)expr_node, expr_node ? expr_node->typ : -1,
                (void*)body_node, body_node ? body_node->typ : -1);
        if (id_node && id_node->sym) {
            fprintf(stderr, "  id.sym->name=%s\n", id_node->sym->name);
        }
        #endif
        
        if (id_node == NULL || id_node->typ != PASCAL_T_IDENTIFIER) {
            fprintf(stderr, "ERROR: FOR_IN missing loop variable identifier\n");
            return NULL;
        }
        
        if (expr_node == NULL) {
            fprintf(stderr, "ERROR: FOR_IN missing collection expression\n");
            return NULL;
        }
        
        if (body_node == NULL) {
            fprintf(stderr, "ERROR: FOR_IN missing body statement\n");
            return NULL;
        }
        
        // Convert identifier to expression
        struct Expression *var_expr = NULL;
        if (id_node->sym != NULL) {
            var_expr = mk_varid(id_node->line, strdup(id_node->sym->name));
        } else {
            fprintf(stderr, "ERROR: FOR_IN identifier has no symbol\n");
            return NULL;
        }
        
        // Convert collection expression
        struct Expression *collection_expr = convert_expression(expr_node);
        if (collection_expr == NULL) {
            fprintf(stderr, "ERROR: FOR_IN collection expression conversion failed\n");
            destroy_expr(var_expr);
            return NULL;
        }
        
        // Convert body statement  
        struct Statement *body_stmt = convert_statement(body_node);
        if (body_stmt == NULL) {
            fprintf(stderr, "ERROR: FOR_IN body statement conversion failed\n");
            destroy_expr(var_expr);
            destroy_expr(collection_expr);
            return NULL;
        }

        return mk_for_in(stmt_node->line, var_expr, collection_expr, body_stmt);
    }
    case PASCAL_T_EXIT_STMT: {
        /* Exit statement may have an optional return expression: exit(expression) */
        ast_t *child = stmt_node->child;
        struct Expression *return_expr = NULL;
        
        /* Skip the 'exit' keyword token if present */
        while (child != NULL && child->typ == PASCAL_T_NONE && child->child == NULL) {
            child = child->next;
        }
        
        /* Check for a parenthesized expression - the NONE node with a child expression */
        if (child != NULL) {
            ast_t *unwrapped = unwrap_pascal_node(child);
            /* Look inside the optional NONE wrapper that contains: '(' expr ')' */
            if (unwrapped != NULL && unwrapped->typ == PASCAL_T_NONE && unwrapped->child != NULL) {
                /* Skip opening paren, get the expression */
                ast_t *expr_node = unwrapped->child;
                /* Skip to actual expression (skip the opening parenthesis) */
                while (expr_node != NULL && 
                       (expr_node->typ == PASCAL_T_NONE || expr_node->typ == PASCAL_T_CHAR) &&
                       expr_node->child == NULL) {
                    expr_node = expr_node->next;
                }
                if (expr_node != NULL) {
                    ast_t *actual_expr = unwrap_pascal_node(expr_node);
                    if (actual_expr != NULL) {
                        return_expr = convert_expression(actual_expr);
                    }
                }
            } else if (unwrapped != NULL) {
                /* Try to convert as expression (shouldn't happen but handle anyway) */
                return_expr = convert_expression(unwrapped);
            }
        }
        
        return mk_exit_with_value(stmt_node->line, return_expr);
    }
    case PASCAL_T_WITH_STMT: {
        ast_t *contexts_wrapper = stmt_node->child;
        ast_t *body_node = unwrap_pascal_node(contexts_wrapper != NULL ? contexts_wrapper->next : NULL);

        struct Statement *body_stmt = convert_statement(body_node);
        if (body_stmt == NULL)
            return NULL;

        if (contexts_wrapper == NULL)
            return NULL;

        if (contexts_wrapper->typ != PASCAL_T_WITH_CONTEXTS) {
            ast_t *single_context = unwrap_pascal_node(contexts_wrapper);
            struct Expression *context_expr = convert_expression(single_context);
            if (context_expr == NULL)
                return NULL;
            return mk_with(stmt_node->line, context_expr, body_stmt);
        }

        ast_t *contexts_start = contexts_wrapper->child;
        if (contexts_start == NULL || contexts_start == ast_nil)
            return NULL;

        return build_nested_with_statements(stmt_node->line, contexts_start, body_stmt);
    }
    case PASCAL_T_TRY_BLOCK: {
        ListBuilder try_builder;
        ListBuilder finally_builder;
        ListBuilder except_builder;
        list_builder_init(&try_builder);
        list_builder_init(&finally_builder);
        list_builder_init(&except_builder);
        
        ast_t *cur = stmt_node->child;
        while (cur != NULL) {
            ast_t *unwrapped = unwrap_pascal_node(cur);
            if (unwrapped == NULL) {
                cur = cur->next;
                continue;
            }
            if (unwrapped->typ == PASCAL_T_FINALLY_BLOCK || unwrapped->typ == PASCAL_T_EXCEPT_BLOCK) {
                ListBuilder *target = (unwrapped->typ == PASCAL_T_FINALLY_BLOCK) ? &finally_builder : &except_builder;
                ast_t *inner = unwrapped->child;
                while (inner != NULL) {
                    ast_t *inner_unwrapped = unwrap_pascal_node(inner);
                    
                    /* Check if this is an 'on' clause with exception variable */
                    if (inner_unwrapped != NULL && inner_unwrapped->typ == PASCAL_T_ON_CLAUSE) {
                        char *exception_var_name = NULL;
                        char *exception_type_name = NULL;
                        /* Extract exception variable and type from on clause */
                        /* Structure: on <var> [: <type>] do <stmt> */
                        ast_t *on_child = inner_unwrapped->child;
                        
                        /* First child should be the variable name */
                        if (on_child != NULL && on_child->typ == PASCAL_T_IDENTIFIER) {
                            if (on_child->sym != NULL)
                                exception_var_name = strdup(on_child->sym->name);
                            on_child = on_child->next;
                        }
                        
                        /* Next might be a NONE node containing the type, or directly the type */
                        if (on_child != NULL) {
                            /* Unwrap if it's a NONE node */
                            ast_t *type_node = (on_child->typ == PASCAL_T_NONE && on_child->child != NULL) ? on_child->child : on_child;
                            
                            /* Look for the type identifier */
                            if (type_node->typ == PASCAL_T_IDENTIFIER) {
                                if (type_node->sym != NULL)
                                    exception_type_name = strdup(type_node->sym->name);
                            }
                            on_child = on_child->next;
                        }
                        
                        /* Find the statement (should be after all the header stuff) */
                        while (on_child != NULL && on_child->typ != PASCAL_T_STATEMENT && 
                               on_child->typ != PASCAL_T_BEGIN_BLOCK && on_child->typ != PASCAL_T_ASSIGNMENT &&
                               on_child->typ != PASCAL_T_FUNC_CALL) {
                            on_child = on_child->next;
                        }
                        
                        /* Convert the statement */
                        if (on_child != NULL) {
                            struct Statement *inner_stmt = convert_statement(unwrap_pascal_node(on_child));
                            if (inner_stmt != NULL) {
                                struct Statement *on_stmt = mk_on_exception(stmt_node->line,
                                    exception_var_name, exception_type_name, inner_stmt);
                                list_builder_append(target, on_stmt, LIST_STMT);
                                exception_var_name = NULL;
                                exception_type_name = NULL;
                            }
                        }
                        if (exception_var_name != NULL)
                            free(exception_var_name);
                        if (exception_type_name != NULL)
                            free(exception_type_name);
                    } else {
                        struct Statement *inner_stmt = convert_statement(inner_unwrapped);
                        if (inner_stmt != NULL)
                            list_builder_append(target, inner_stmt, LIST_STMT);
                    }
                    inner = inner->next;
                }
            } else {
                struct Statement *try_stmt = convert_statement(unwrapped);
                if (try_stmt != NULL)
                    list_builder_append(&try_builder, try_stmt, LIST_STMT);
            }
            cur = cur->next;
        }

        ListNode_t *try_stmts = list_builder_finish(&try_builder);
        ListNode_t *finally_stmts = list_builder_finish(&finally_builder);
        ListNode_t *except_stmts = list_builder_finish(&except_builder);

        if (finally_stmts == NULL && except_stmts == NULL) {
            if (try_stmts == NULL)
                return NULL;
            if (try_stmts->next == NULL) {
                struct Statement *passthrough = (struct Statement *)try_stmts->cur;
                DestroyList(try_stmts);
                return passthrough;
            }
            return mk_compoundstatement(stmt_node->line, try_stmts);
        }

        if (finally_stmts != NULL)
            return mk_tryfinally(stmt_node->line, try_stmts, finally_stmts);
        return mk_tryexcept(stmt_node->line, try_stmts, except_stmts, NULL, NULL);
    }
    case PASCAL_T_RAISE_STMT: {
        struct Expression *exc_expr = convert_expression(unwrap_pascal_node(stmt_node->child));
        return mk_raise(stmt_node->line, exc_expr);
    }
    case PASCAL_T_INHERITED_STMT: {
        struct Expression *call_expr = convert_expression(unwrap_pascal_node(stmt_node->child));
        return mk_inherited(stmt_node->line, call_expr);
    }
    case PASCAL_T_CASE_STMT: {
        ast_t *selector = stmt_node->child;
        ast_t *branches_start = selector != NULL ? selector->next : NULL;

        struct Expression *selector_expr = convert_expression(selector);
        ListBuilder branches_builder;
        list_builder_init(&branches_builder);
        struct Statement *else_stmt = NULL;
        
        /* Walk through all siblings after the selector */
        ast_t *cur = branches_start;
        while (cur != NULL) {
            if (cur->typ == PASCAL_T_CASE_BRANCH) {
                /* Process this branch */
                ListBuilder labels_builder;
                list_builder_init(&labels_builder);
                struct Statement *branch_stmt = NULL;
                
                /* Walk through children of the branch */
                ast_t *child = cur->child;
                while (child != NULL) {
                    if (child->typ == PASCAL_T_CASE_LABEL_LIST) {
                        /* CASE_LABEL_LIST contains the labels */
                        ast_t *label_node = child->child;
                while (label_node != NULL) {
                    append_case_label(&labels_builder, label_node);
                    label_node = label_node->next;
                }
            } else if (child->typ == PASCAL_T_CASE_LABEL ||
                       child->typ == PASCAL_T_INTEGER ||
                       child->typ == PASCAL_T_IDENTIFIER ||
                       child->typ == PASCAL_T_CHAR ||
                       child->typ == PASCAL_T_CHAR_CODE ||
                       child->typ == PASCAL_T_STRING ||
                       child->typ == PASCAL_T_RANGE) {
                append_case_label(&labels_builder, child);
            } else {
                /* Any other child type is the statement for this branch
                 * (e.g. PASCAL_T_STATEMENT, PASCAL_T_ASSIGNMENT,
                 * PASCAL_T_FUNC_CALL, PASCAL_T_BEGIN_BLOCK,
                 * PASCAL_T_IF_STMT, PASCAL_T_FOR_STMT, PASCAL_T_WHILE_STMT,
                 * PASCAL_T_REPEAT_STMT, PASCAL_T_CASE_STMT, PASCAL_T_WITH_STMT,
                 * PASCAL_T_EXIT_STMT, PASCAL_T_BREAK_STMT, etc.) */
                branch_stmt = convert_statement(child);
                break; /* Statement is last */
                    }
                    child = child->next;
                }
                
                /* Create the branch */
                struct CaseBranch *branch = (struct CaseBranch *)malloc(sizeof(struct CaseBranch));
                if (branch != NULL) {
                    branch->labels = list_builder_finish(&labels_builder);
                    branch->stmt = branch_stmt;
                    list_builder_append(&branches_builder, branch, LIST_CASE_BRANCH);
                }
            } else if (cur->typ == PASCAL_T_ELSE) {
                /* Optional else clause */
                if (cur->child != NULL) {
                    else_stmt = convert_statement(cur->child);
                }
            }
            cur = cur->next;
        }
        
        return mk_case(stmt_node->line, selector_expr, list_builder_finish(&branches_builder), else_stmt);
    }
    default: {
        /* Try to convert unknown tags as expressions (e.g., NOT, AND, OR in const contexts) */
        struct Expression *expr = convert_expression(stmt_node);
        if (expr != NULL)
            return mk_exprstmt(stmt_node->line, stmt_node->col, expr);

        const char *name = tag_name(stmt_node->typ);
        fprintf(stderr, "ERROR: unsupported statement tag %d (%s) at line %d.",
                stmt_node->typ, name, stmt_node->line);
        if (stmt_node->sym != NULL && stmt_node->sym->name != NULL)
            fprintf(stderr, " (symbol: %s)", stmt_node->sym->name);
        if (stmt_node->child != NULL)
            fprintf(stderr, " (child: %s/%d)", tag_name(stmt_node->child->typ), stmt_node->child->line);
        fprintf(stderr, "\n");
        break;
    }
    }

    return NULL;
}

ListNode_t *convert_statement_list(ast_t *stmt_list_node) {
    ListBuilder builder;
    list_builder_init(&builder);
    
    /* Create visited set to detect circular references */
    VisitedSet *visited = visited_set_create();
    if (visited == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate visited set for statement list traversal\n");
        return NULL;
    }
    
    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_statement_list: starting, stmt_list_node=%p\n", (void*)stmt_list_node);
    }
    
    ast_t *cur = stmt_list_node;
    int stmt_count = 0;
    while (cur != NULL && cur != ast_nil) {
        /* Check for circular reference before processing */
        if (!is_safe_to_continue(visited, cur)) {
            fprintf(stderr, "ERROR: Circular reference detected in statement list, stopping traversal\n");
            break;
        }
        
        ast_t *unwrapped = unwrap_pascal_node(cur);
        if (unwrapped == NULL) {
            cur = cur->next;
            continue;
        }

        if (unwrapped->typ == PASCAL_T_NONE && unwrapped->child == NULL) {
            cur = cur->next;
            continue;
        }

        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_statement_list: processing stmt %d, typ=%d line=%d\n", 
                    stmt_count, unwrapped->typ, unwrapped->line);
            fprintf(stderr, "[KGPC]   calling convert_statement...\n");
            fflush(stderr);
        }

        struct Statement *stmt = convert_statement(unwrapped);
        
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC]   convert_statement returned: %p\n", (void*)stmt);
            fflush(stderr);
        }
        
        if (stmt != NULL) {
            list_builder_append(&builder, stmt, LIST_STMT);
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC]   -> converted successfully\n");
                fflush(stderr);
            }
        } else if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC]   -> convert_statement returned NULL, dropped statement typ=%d line=%d\n",
                unwrapped->typ, unwrapped->line);
            fflush(stderr);
        }
        cur = cur->next;
        stmt_count++;
    }

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_statement_list: processed %d statements\n", stmt_count);
    }

    visited_set_destroy(visited);
    return list_builder_finish(&builder);
}

struct Statement *convert_block(ast_t *block_node) {
    if (block_node == NULL)
    {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL)
            fprintf(stderr, "[KGPC] convert_block: block_node is NULL\n");
        return NULL;
    }

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_block: typ=%d line=%d\n", block_node->typ, block_node->line);
        if (block_node->child) {
            fprintf(stderr, "[KGPC]   child typ=%d line=%d\n", 
                    block_node->child->typ, block_node->child->line);
            // Check if child is ast_nil
            if (block_node->child == ast_nil) {
                fprintf(stderr, "[KGPC]   child is ast_nil!\n");
            }
        } else {
            fprintf(stderr, "[KGPC]   child is NULL\n");
        }
    }

    ast_t *stmts = block_node->child;
    if (stmts == ast_nil) {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_block: stmts is ast_nil, treating as NULL\n");
        }
        stmts = NULL;
    }
    ListNode_t *list = convert_statement_list(stmts);
    if (list == NULL && kgpc_getenv("KGPC_DEBUG_BODY") != NULL)
        fprintf(stderr, "[KGPC] convert_block: statement list is NULL\n");
    return mk_compoundstatement(block_node->line, list);
}

/* Recursively search an AST subtree for a keyword (case-insensitive).
 * Returns 1 if found, 0 otherwise. Limits depth to avoid runaway recursion. */
/* Search an AST node list (linked via ->next) and their children for a keyword. */
static int ast_has_keyword_in_list(ast_t *node, const char *keyword, int max_depth) {
    if (node == NULL || max_depth <= 0)
        return 0;
    if (node->sym != NULL && node->sym->name != NULL &&
        strcasecmp(node->sym->name, keyword) == 0)
        return 1;
    if (ast_has_keyword_in_list(node->child, keyword, max_depth - 1))
        return 1;
    return ast_has_keyword_in_list(node->next, keyword, max_depth);
}

static int ast_list_has_directive_keyword(ast_t *node, const char *keyword, int max_depth)
{
    if (node == NULL || keyword == NULL || max_depth <= 0)
        return 0;

    for (ast_t *cur = node; cur != NULL; cur = cur->next)
    {
        if (cur->typ == PASCAL_T_FUNCTION_BODY ||
            cur->typ == PASCAL_T_BEGIN_BLOCK ||
            cur->typ == PASCAL_T_ASM_BLOCK ||
            cur->typ == PASCAL_T_CONST_SECTION ||
            cur->typ == PASCAL_T_VAR_SECTION ||
            cur->typ == PASCAL_T_TYPE_SECTION ||
            cur->typ == PASCAL_T_LABEL_SECTION ||
            cur->typ == PASCAL_T_PROCEDURE_DECL ||
            cur->typ == PASCAL_T_FUNCTION_DECL ||
            cur->typ == PASCAL_T_METHOD_IMPL)
            break;

        if (cur->sym != NULL && cur->sym->name != NULL &&
            strcasecmp(cur->sym->name, keyword) == 0)
            return 1;

        switch (cur->typ)
        {
            case PASCAL_T_NONE:
            case PASCAL_T_IDENTIFIER:
            case PASCAL_T_EXTERNAL_NAME:
            case PASCAL_T_EXTERNAL_NAME_EXPR:
                if (ast_has_keyword_in_list(cur->child, keyword, max_depth - 1))
                    return 1;
                break;
            default:
                break;
        }
    }

    return 0;
}

static int ast_has_routine_directive(ast_t *node, const char *keyword, int max_depth)
{
    if (node == NULL || keyword == NULL || max_depth <= 0)
        return 0;
    return ast_list_has_directive_keyword(node->child, keyword, max_depth);
}

/* Build a TypeAlias capturing a complex return type from TypeInfo, transferring
 * ownership of heap-allocated fields.  Cleans up remaining TypeInfo contents. */
static struct TypeAlias *build_inline_return_alias(TypeInfo *type_info, int return_type,
                                                   char *return_type_id)
{
    struct TypeAlias *alias = NULL;
    if (type_info->is_array || type_info->is_pointer || type_info->is_set ||
        type_info->is_enum || type_info->is_file || type_info->is_record) {
        alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
        if (alias != NULL) {
            alias->base_type = return_type;
            alias->target_type_id = return_type_id;
            if (type_info->is_array) {
                alias->is_array = 1;
                alias->array_start = type_info->start;
                alias->array_end = type_info->end;
                alias->array_element_type = type_info->element_type;
                alias->array_element_type_id = type_info->element_type_id;
                if (type_info->element_kgpc_type != NULL) {
                    long long esize = kgpc_type_sizeof(type_info->element_kgpc_type);
                    if (esize > 0 && esize <= INT_MAX)
                        alias->array_element_storage_size = (int)esize;
                }
                alias->is_shortstring = type_info->is_shortstring;
                alias->is_open_array = type_info->is_open_array;
            }
            if (type_info->is_pointer) {
                alias->is_pointer = 1;
                alias->pointer_type = type_info->pointer_type;
                alias->pointer_type_id = type_info->pointer_type_id;
            }
            if (type_info->is_set) {
                alias->is_set = 1;
                alias->set_element_type = type_info->set_element_type;
                alias->set_element_type_id = type_info->set_element_type_id;
            }
            if (type_info->is_enum) {
                alias->is_enum = 1;
                alias->enum_is_scoped = type_info->enum_is_scoped;
                alias->enum_literals = type_info->enum_literals;
            }
            if (type_info->is_file) {
                alias->is_file = 1;
                alias->file_type = type_info->file_type;
                alias->file_type_id = type_info->file_type_id;
            }
            /* NULL out transferred pointers so destroy_type_info_contents
             * won't double-free them. */
            type_info->element_type_id = NULL;
            type_info->pointer_type_id = NULL;
            type_info->set_element_type_id = NULL;
            type_info->enum_literals = NULL;
            type_info->file_type_id = NULL;
        }
    }
    destroy_type_info_contents(type_info);
    return alias;
}

Tree_t *convert_method_impl(ast_t *method_node) {
    if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
        fprintf(stderr, "[KGPC] convert_method_impl entry (method_node=%p)\n", (void*)method_node);
    }

    if (method_node == NULL)
        return NULL;

    ast_t *cur = method_node->child;
    /* Skip optional keyword identifiers like "class" or "generic" that appear
     * before the qualified identifier (e.g., "class function THost.SeedSum"). */
    int method_decl_uses_operator_keyword = 0;
    while (cur != NULL) {
        ast_t *skip_node = unwrap_pascal_node(cur);
        if (skip_node == NULL) skip_node = cur;
        if (skip_node->typ == PASCAL_T_IDENTIFIER &&
            skip_node->sym != NULL && skip_node->sym->name != NULL &&
            is_method_decl_keyword(skip_node->sym->name)) {
            if (strcasecmp(skip_node->sym->name, "operator") == 0)
                method_decl_uses_operator_keyword = 1;
            cur = cur->next;
            continue;
        }
        break;
    }
    ast_t *qualified = unwrap_pascal_node(cur);

    if (kgpc_getenv("KGPC_DEBUG_OPERATOR") != NULL) {
        fprintf(stderr, "[Operator] convert_method_impl: cur=%p qualified=%p\n", (void*)cur, (void*)qualified);
        if (qualified != NULL) {
            fprintf(stderr, "[Operator]   qualified->typ=%d\n", qualified->typ);
            if (qualified->sym && qualified->sym->name) {
                fprintf(stderr, "[Operator]   qualified->sym->name=%s\n", qualified->sym->name);
            }
        }
    }

    /* Check for standalone operator: the first child is directly the operator symbol like "+" or "-"
     * This is detected by checking if the first identifier is a known operator symbol */
    if (qualified != NULL && qualified->typ == PASCAL_T_IDENTIFIER) {
        char *potential_op = dup_symbol(qualified);
        int is_operator_symbol = (potential_op != NULL && is_operator_token_name(potential_op));
        int should_parse_standalone_operator = method_decl_uses_operator_keyword || is_operator_symbol;
        /* Check if this is an operator symbol (encoded to op_xxx means it's an operator) */
        char *encoded_op = (potential_op != NULL && should_parse_standalone_operator)
            ? encode_operator_name(potential_op) : NULL;
        free(potential_op);

        if (should_parse_standalone_operator) {
            /* This is a standalone operator - qualified is the operator symbol directly */
            if (kgpc_getenv("KGPC_DEBUG_OPERATOR") != NULL) {
                fprintf(stderr, "[Operator] Detected operator symbol: encoded=%s\n", encoded_op);
            }
            /* Get the param list to determine the type.
             * The next node could be PASCAL_T_PARAM_LIST or directly a PASCAL_T_PARAM */
            ast_t *param_node = qualified->next;
            if (kgpc_getenv("KGPC_DEBUG_OPERATOR") != NULL) {
                fprintf(stderr, "[Operator] param_node=%p typ=%d (PARAM_LIST=%d, PARAM=%d)\n",
                    (void*)param_node, param_node ? param_node->typ : -1,
                    PASCAL_T_PARAM_LIST, PASCAL_T_PARAM);
            }
            ast_t *first_param = NULL;
            if (param_node != NULL) {
                if (param_node->typ == PASCAL_T_PARAM_LIST) {
                    first_param = param_node->child;
                } else if (param_node->typ == PASCAL_T_PARAM) {
                    first_param = param_node;
                }
            }
            if (first_param != NULL) {
                /* Parse the parameter to get its type */
                ast_t *param_cursor = first_param;
                ListNode_t *params = convert_param_list(&param_cursor);
                if (params != NULL) {
                    Tree_t *first_param_tree = (Tree_t *)params->cur;
                    if (first_param_tree != NULL &&
                        first_param_tree->type == TREE_VAR_DECL &&
                        first_param_tree->tree_data.var_decl_data.type_id != NULL) {
                        const char *param_type_id = first_param_tree->tree_data.var_decl_data.type_id;
                        /* Get second param type for binary operator disambiguation */
                        const char *second_param_type_id = NULL;
                        if (params->next != NULL) {
                            Tree_t *second_param_tree = (Tree_t *)params->next->cur;
                            if (second_param_tree != NULL && second_param_tree->type == TREE_VAR_DECL &&
                                second_param_tree->tree_data.var_decl_data.type_id != NULL)
                                second_param_type_id = second_param_tree->tree_data.var_decl_data.type_id;
                        }

                        /* Build mangled name: TypeName__op_suffix[_SecondParamType] */
                        size_t name_len = strlen(param_type_id) + strlen(encoded_op) + 3
                            + (second_param_type_id != NULL ? strlen(second_param_type_id) + 1 : 0);
                        char *mangled_name = (char *)malloc(name_len);
                        if (mangled_name != NULL) {
                            if (second_param_type_id != NULL)
                                snprintf(mangled_name, name_len, "%s__%s_%s", param_type_id, encoded_op, second_param_type_id);
                            else
                                snprintf(mangled_name, name_len, "%s__%s", param_type_id, encoded_op);

                            if (kgpc_getenv("KGPC_DEBUG_OPERATOR") != NULL) {
                                fprintf(stderr, "[Operator] Standalone operator: %s\n", mangled_name);
                            }

                            /* Find return type by scanning siblings of param_node */
                            ast_t *return_type_node = NULL;
                            char *result_var_name_method = NULL;
                            ast_t *scan = param_node->next;
                            while (scan != NULL) {
                                if (scan->typ == PASCAL_T_RETURN_TYPE) {
                                    return_type_node = scan;
                                    break;
                                }
                                /* Named result identifier before RETURN_TYPE (e.g., dest : variant) */
                                if (scan->typ == PASCAL_T_IDENTIFIER && scan->next != NULL &&
                                    scan->next->typ == PASCAL_T_RETURN_TYPE) {
                                    result_var_name_method = dup_symbol(scan);
                                    scan = scan->next;
                                    return_type_node = scan;
                                    break;
                                }
                                /* Skip other params */
                                if (scan->typ != PASCAL_T_PARAM)
                                    break;
                                scan = scan->next;
                            }

                            char *return_type_id = NULL;
                            int return_type = UNKNOWN_TYPE;
                            struct TypeAlias *inline_return_type = NULL;
                            TypeRef *return_type_ref = NULL;

                            if (return_type_node != NULL) {
                                TypeInfo type_info;
                                memset(&type_info, 0, sizeof(TypeInfo));
                                return_type = convert_type_spec(return_type_node->child, &return_type_id, NULL, &type_info);
                                if (return_type_ref == NULL)
                                    return_type_ref = type_ref_from_info_or_id(&type_info, return_type_id);
                                if (return_type_id == NULL && return_type_node->sym != NULL &&
                                    return_type_node->sym->name != NULL)
                                    return_type_id = strdup(return_type_node->sym->name);
                                destroy_type_info_contents(&type_info);
                            }

                            /* Keep the base name (TypeName__op_suffix) as the
                             * symbol table id so FindAllIdents can find all
                             * overloads.  The full suffixed name goes into
                             * mangled_id for unique assembly labels. */
                            size_t base_id_len = strlen(param_type_id) + strlen(encoded_op) + 3;
                            char *base_id = (char *)malloc(base_id_len);
                            if (base_id != NULL)
                                snprintf(base_id, base_id_len, "%s__%s", param_type_id, encoded_op);
                            const char *ret_suffix = return_type_id;
                            if (ret_suffix == NULL) ret_suffix = result_var_name_method;
                            if (ret_suffix != NULL) {
                                size_t new_len = strlen(mangled_name) + strlen(ret_suffix) + 2;
                                char *new_name = (char *)malloc(new_len);
                                if (new_name != NULL) {
                                    snprintf(new_name, new_len, "%s_%s", mangled_name, ret_suffix);
                                    free(mangled_name);
                                    mangled_name = new_name;
                                }
                            }

                            /* Find the body */
                            struct Statement *body = NULL;
                            ListNode_t *op_const_decls = NULL;
                            ListBuilder op_var_builder; list_builder_init(&op_var_builder);
                            ListBuilder op_label_builder; list_builder_init(&op_label_builder);
                            ListNode_t *op_nested_subs = NULL;
                            ListNode_t *op_type_decls = NULL;
                            ast_t *body_cursor = return_type_node ? return_type_node->next : param_node->next;
                            while (body_cursor != NULL) {
                                if (body_cursor->typ == PASCAL_T_BEGIN_BLOCK) {
                                    body = convert_block(body_cursor);
                                    break;
                                } else if (body_cursor->typ == PASCAL_T_FUNCTION_BODY) {
                                    convert_routine_body(body_cursor, &op_const_decls, &op_var_builder, &op_label_builder,
                                        &op_nested_subs, &body, &op_type_decls);
                                    break;
                                }
                                body_cursor = body_cursor->next;
                            }

                            /* Create the function tree: use base_id as the
                             * symbol table key; store the full suffixed name
                             * as mangled_id for unique assembly labels. */
                            ListNode_t *op_var_decls = list_builder_finish(&op_var_builder);
                            ListNode_t *op_label_decls = list_builder_finish(&op_label_builder);
                            Tree_t *tree = mk_function(method_node->line,
                                base_id != NULL ? base_id : mangled_name,
                                params, op_const_decls,
                                op_label_decls, op_type_decls, op_var_decls, op_nested_subs, body, return_type, return_type_id, inline_return_type, 0, 0);
                            if (tree != NULL) {
                                if (base_id != NULL)
                                    tree->tree_data.subprogram_data.mangled_id = mangled_name;
                                else
                                    tree->tree_data.subprogram_data.mangled_id = strdup(tree->tree_data.subprogram_data.id);
                                tree->tree_data.subprogram_data.is_operator = 1;
                            } else
                                free(mangled_name);
                            if (base_id != NULL && tree != NULL)
                                ; /* base_id ownership transferred to mk_function */
                            else if (base_id != NULL && tree == NULL)
                                free(base_id);
                            if (tree != NULL && method_node->index >= 0)
                                tree->source_index = method_node->index + g_source_offset;
                            if (tree != NULL && result_var_name_method != NULL) {
                                tree->tree_data.subprogram_data.return_type_ref =
                                    return_type_ref != NULL ? type_ref_clone(return_type_ref)
                                                            : type_ref_from_single_name(return_type_id);
                                tree->tree_data.subprogram_data.result_var_name = result_var_name_method;
                            } else if (result_var_name_method != NULL) {
                                free(result_var_name_method);
                            }

                            free(encoded_op);
                            return tree;
                        }
                    }
                    /* Don't destroy params here - they'll be used in the tree */
                }
            }
            free(encoded_op);
            return NULL; /* Couldn't process the operator */
        }
        free(encoded_op);
    }

    if (qualified == NULL || qualified->typ != PASCAL_T_QUALIFIED_IDENTIFIER) {
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
            fprintf(stderr, "[KGPC] convert_method_impl: cur=%p typ=%d\n",
                    (void*)cur, cur ? cur->typ : -1);
            if (cur && cur->sym && cur->sym->name) {
                fprintf(stderr, "[KGPC]   cur->sym->name=%s\n", cur->sym->name);
            }
            fprintf(stderr, "[KGPC] convert_method_impl: qualified=%p typ=%d\n",
                    (void*)qualified, qualified ? qualified->typ : -1);
            if (qualified && qualified->sym && qualified->sym->name) {
                fprintf(stderr, "[KGPC]   qualified->sym->name=%s\n", qualified->sym->name);
            }
            if (qualified && qualified->child) {
                fprintf(stderr, "[KGPC]   qualified->child->typ=%d\n", qualified->child->typ);
            }
            fprintf(stderr, "[KGPC] convert_method_impl: no qualified identifier (typ=%d)\n",
                    qualified ? qualified->typ : -1);
        }
        return NULL;
    }

    ast_t *class_node = qualified->child;
    if (class_node == NULL)
        return NULL;

    // Collect all IDENTIFIER nodes from the qualified name.
    // The grammar produces: IDENTIFIER [TYPE_PARAM_LIST] IDENTIFIER [IDENTIFIER]*
    // For "Class.Method" we get 2 identifiers, for "Class.Inner.Method" we get 3, etc.
    // The LAST identifier is always the method name; all preceding ones form the class path.
    ast_t *ident_nodes[32];  // more than enough for any reasonable nesting depth
    int ident_count = 0;

    ast_t *method_type_param_list = NULL;  /* TYPE_PARAM_LIST after the last IDENTIFIER (method-level generic) */
    ast_t *pending_type_param_list = NULL;
    for (ast_t *cursor = class_node; cursor != NULL; cursor = cursor->next) {
        if (cursor->typ == PASCAL_T_IDENTIFIER && ident_count < 32) {
            ident_nodes[ident_count++] = cursor;
            pending_type_param_list = NULL;  /* TYPE_PARAM_LIST before an IDENTIFIER is class-level, reset */
        }
        if (cursor->typ == PASCAL_T_TYPE_PARAM_LIST) {
            pending_type_param_list = cursor;
        }
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
            fprintf(stderr, "[KGPC] convert_method_impl: child typ=%d (%s) name=%s\n",
                    cursor->typ, pascal_tag_to_string(cursor->typ),
                    (cursor->sym && cursor->sym->name) ? cursor->sym->name : "<null>");
        }
    }
    method_type_param_list = pending_type_param_list;  /* Last TYPE_PARAM_LIST with no IDENTIFIER after it */

    if (ident_count < 2)
        return NULL;  // Need at least ClassName and MethodName

    ast_t *method_id_node = ident_nodes[ident_count - 1];  // last identifier = method name

    // Build class_name by joining all identifiers except the last with "."
    size_t class_name_len = 0;
    for (int i = 0; i < ident_count - 1; i++) {
        char *seg = dup_symbol(ident_nodes[i]);
        if (seg != NULL) {
            class_name_len += strlen(seg) + 1;  // +1 for '.' or '\0'
            free(seg);
        }
    }

    char *class_name = (char *)malloc(class_name_len);
    if (class_name == NULL)
        return NULL;
    class_name[0] = '\0';
    for (int i = 0; i < ident_count - 1; i++) {
        char *seg = dup_symbol(ident_nodes[i]);
        if (seg != NULL) {
            if (i > 0) strcat(class_name, ".");
            strcat(class_name, seg);
            free(seg);
        }
    }

    char *method_name = dup_symbol(method_id_node);
    if (method_name == NULL) {
        free(class_name);
        return NULL;
    }
    
    // For generic classes, strip the type parameters from the class name
    // e.g., "TFPGList<T>" -> "TFPGList"
    char *cleaned_class_name = NULL;
    if (class_name != NULL) {
        char *bracket = strchr(class_name, '<');
        if (bracket != NULL) {
            size_t len = (size_t)(bracket - class_name);
            cleaned_class_name = (char *)malloc(len + 1);
            if (cleaned_class_name != NULL) {
                memcpy(cleaned_class_name, class_name, len);
                cleaned_class_name[len] = '\0';
            }
        }
    }

    const char *registered_class = find_class_for_method(method_name);
    /* Prefer the explicitly specified class name from the qualified identifier,
     * falling back to the registered class if no explicit class was given */
    const char *effective_class = (cleaned_class_name != NULL) ? cleaned_class_name : 
                                  (class_name != NULL) ? class_name : registered_class;
    
    /* For nested types like TManager.TState, the effective class for mangling
     * should use just the last component (TState) since that's the actual type
     * name that methods are registered under.  Keep the full dotted name in
     * effective_class_full for const/var lookups.  */
    const char *effective_class_full = effective_class;
    char *effective_class_last = NULL;
    char *effective_class_outer = NULL;
    if (effective_class != NULL)
    {
        const char *last_dot = strrchr(effective_class, '.');
        if (last_dot != NULL && last_dot[1] != '\0')
        {
            /* "TOuter.TInner" -> outer="TOuter", last="TInner" */
            effective_class_outer = strndup(effective_class, (size_t)(last_dot - effective_class));
            effective_class_last = strdup(last_dot + 1);
            effective_class = effective_class_last;
        }
    }

    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[FromCParser] convert_method_impl: class_name=%s method_name=%s effective_class=%s effective_class_full=%s\n",
            class_name ? class_name : "<null>",
            method_name ? method_name : "<null>",
            effective_class ? effective_class : "<null>",
            effective_class_full ? effective_class_full : "<null>");
    }
    
    /* Don't re-register the method here - it was already registered during class declaration */
    
    /* Check if this method was declared as static. Use signature-aware lookup
     * for overload disambiguation. Also detect class methods (Self = VMT). */
    int is_static_method = 0;
    int is_class_method_impl = from_cparser_is_method_class_method(effective_class, method_name);
    int method_param_count = count_params_in_method_impl(method_node);
    char *method_param_sig = param_type_signature_from_method_impl(method_node);
    int method_declares_operator = method_decl_uses_operator_keyword;
    if (method_node != NULL && method_name != NULL)
    {
        struct MethodTemplate impl_template = {0};
        impl_template.name = (char *)method_name;
        annotate_method_template(&impl_template, method_node);
        /* Only truly static methods skip Self; class methods have Self = VMT pointer */
        if (impl_template.is_static)
            is_static_method = 1;
        if (impl_template.is_class_method)
            is_class_method_impl = 1;
    }
    if (!is_static_method)
        is_static_method = is_method_static_with_signature(effective_class, method_name,
            method_param_count, method_param_sig);
    /* A static class method (class function ... static) has no Self */
    if (is_class_method_impl && is_static_method)
        is_static_method = 1;
    /* A non-static class method (class function) has Self = VMT, so don't skip it */
    else if (is_class_method_impl && !is_static_method)
        is_static_method = 0;
    if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
        fprintf(stderr, "[KGPC] convert_method_impl: class=%s method=%s is_static=%d\n",
                effective_class ? effective_class : "<null>", 
                method_name ? method_name : "<null>",
                is_static_method);
    }
    
    const char *mangle_owner = effective_class;
    if (effective_class_full != NULL && effective_class_full != effective_class)
        mangle_owner = effective_class_full;
    char *proc_name = mangle_method_name_raw(mangle_owner, method_name);
    if (proc_name == NULL) {
        free(class_name);
        free(method_name);
        free(effective_class_last);
        if (method_param_sig != NULL)
            free(method_param_sig);
        return NULL;
    }
    
    /* Check if this is a class operator.
     * Named operators like Add/Subtract must come from "operator" declarations,
     * otherwise normal methods (e.g. TList.Add) are misclassified. */
    int is_class_operator = 0;
    if (method_declares_operator)
    {
        is_class_operator = 1;
    }
    else if (method_name != NULL)
    {
        /* Fallback only for symbolic operators to keep legacy parser cases working.
         * Named operators (Equal, Add, etc.) must NOT be matched here because
         * regular methods with those names (e.g. tdynamicarray.equal) would be
         * misclassified as operators and lose their implicit Self parameter. */
        if (strcmp(method_name, "+") == 0 || strcmp(method_name, "-") == 0 ||
            strcmp(method_name, "*") == 0 || strcmp(method_name, "/") == 0 ||
            strcmp(method_name, "=") == 0 || strcmp(method_name, "<>") == 0 ||
            strcmp(method_name, "<") == 0 || strcmp(method_name, ">") == 0 ||
            strcmp(method_name, "<=") == 0 || strcmp(method_name, ">=") == 0 ||
            strcmp(method_name, "**") == 0 || strcmp(method_name, ":=") == 0) {
            is_class_operator = 1;
        }
    }
    int is_constructor = (method_node->typ == PASCAL_T_CONSTRUCTOR_DECL) ||
        (method_name != NULL && strcasecmp(method_name, "create") == 0);

    ListBuilder params_builder;
    list_builder_init(&params_builder);
    char **generic_type_params = NULL;
    int num_generic_type_params = 0;
    /* Extract method-level generic type params found inside the qualified identifier */
    if (method_type_param_list != NULL) {
        num_generic_type_params = extract_generic_type_params(method_type_param_list, &generic_type_params);
    }
    ListNode_t *const_decls = NULL;
    ListBuilder var_builder;
    list_builder_init(&var_builder);
    ListBuilder label_builder;
    list_builder_init(&label_builder);
    ListNode_t *nested_subs = NULL;
    struct Statement *body = NULL;
    int is_nostackframe = 0;
    ast_t *type_section_ast = NULL;  /* Track local type section for enum resolution */
    ListNode_t *type_decls = NULL;

    /* Return type information for methods that are functions (like class operators) */
    char *return_type_id = NULL;
    int return_type = UNKNOWN_TYPE;
    struct TypeAlias *inline_return_type = NULL;
    int has_return_type = 0;
    TypeRef *return_type_ref = NULL;

    const char *helper_base = (effective_class != NULL) ? lookup_type_helper_base(effective_class) : NULL;
    int is_helper_method = (helper_base != NULL);
    if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && effective_class != NULL)
        fprintf(stderr, "[KGPC] convert_method_impl: looking up helper base for %s -> %s\n",
            effective_class, helper_base ? helper_base : "(not found)");

    /* Add Self parameter only for instance methods, not for class operators or static methods */
    if (kgpc_getenv("KGPC_ASSERT_STATIC_SELF") != NULL && is_static_method) {
        assert(!is_class_operator && "static method should not be class operator");
        assert(method_param_sig != NULL || method_param_count >= 0);
    }

    if (!is_class_operator && !is_static_method) {
        ListNode_t *self_ids = CreateListNode(strdup("Self"), LIST_STRING);
        char *self_type_id = NULL;
        int self_type_tag = UNKNOWN_TYPE;
        struct TypeAlias *self_type_alias = NULL;
        int self_is_var = 1;
        if (effective_class != NULL) {
            if (is_helper_method) {
                self_type_id = strdup(helper_base);
                /* Resolve the type tag for type helper Self parameters.
                 * This is important for correct calling convention (e.g., Double should use xmm0). */
                self_type_tag = map_type_name(helper_base, NULL);
                self_type_alias = helper_self_real_alias(helper_base);
                self_is_var = helper_self_param_is_var(helper_base, NULL);
            } else {
                self_type_id = strdup(effective_class_full ? effective_class_full : effective_class);
                if (type_name_is_class_like(effective_class))
                    self_is_var = 0;
            }
        }
        Tree_t *self_param = mk_vardecl(method_node->line, self_ids, self_type_tag,
            self_type_id, self_is_var, 0, NULL, NULL, self_type_alias, NULL);
        if (self_param != NULL)
            self_param->tree_data.var_decl_data.type_ref = type_ref_from_single_name(self_type_id);
        list_builder_append(&params_builder, self_param, LIST_TREE);
    }

    const char *prev_method_name_ctx = g_current_method_name;
    g_current_method_name = method_name;
    cur = qualified->next;
    while (cur != NULL) {
        ast_t *node = unwrap_pascal_node(cur);
        if (node == NULL)
            node = cur;

        switch (node->typ) {
        case PASCAL_T_TYPE_PARAM_LIST:
            if (num_generic_type_params == 0) {
                num_generic_type_params = extract_generic_type_params(node, &generic_type_params);
            }
            break;
        case PASCAL_T_PARAM_LIST: {
            ast_t *param_cursor = node->child;
            ListNode_t *extra_params = convert_param_list(&param_cursor);
            if (extra_params != NULL)
                list_builder_extend(&params_builder, extra_params);
            break;
        }
        case PASCAL_T_PARAM: {
            ListNode_t *extra_params = convert_param(node);
            if (extra_params != NULL)
                list_builder_extend(&params_builder, extra_params);
            break;
        }
        case PASCAL_T_RETURN_TYPE: {
            /* Method has a return type - it's a function, not a procedure */
            has_return_type = 1;
            TypeInfo type_info = {0};
            return_type = convert_type_spec(node->child, &return_type_id, NULL, &type_info);
            /* Same NULL-child fallback as convert_function: when the parser
             * produces a RETURN_TYPE node without a child subtree (forward
             * declarations), use the sym name for {$H-} remapping. */
            if (return_type == UNKNOWN_TYPE && return_type_id == NULL &&
                node->sym != NULL && node->sym->name != NULL)
            {
                if (pascal_frontend_default_shortstring() &&
                    strcasecmp(node->sym->name, "string") == 0)
                {
                    return_type = SHORTSTRING_TYPE;
                    return_type_id = strdup(node->sym->name);
                }
            }
            if (return_type_ref == NULL)
                return_type_ref = type_ref_from_info_or_id(&type_info, return_type_id);
            if (return_type_id == NULL && node->sym != NULL && node->sym->name != NULL)
                return_type_id = strdup(node->sym->name);
            inline_return_type = build_inline_return_alias(&type_info, return_type, return_type_id);
            break;
        }
        case PASCAL_T_TYPE_SECTION:
            if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
                fprintf(stderr, "[KGPC] convert_function TYPE_SECTION at line=%d\n", node->line);
            }
            append_type_decls_from_section(node, &type_decls, &nested_subs,
                &const_decls, &var_builder, NULL);
            type_section_ast = node;  /* Save for const array enum resolution */
            break;
        case PASCAL_T_CONST_SECTION:
            append_const_decls_from_section(node, &const_decls, &var_builder, type_section_ast);
            break;
        case PASCAL_T_VAR_SECTION:
            list_builder_extend(&var_builder, convert_var_section(node));
            break;
        case PASCAL_T_LABEL_SECTION:
            append_labels_from_section(node, &label_builder);
            break;
        case PASCAL_T_FUNCTION_DECL:
        case PASCAL_T_PROCEDURE_DECL: {
            /* Handle nested functions/procedures within method implementations */
            Tree_t *sub = (node->typ == PASCAL_T_PROCEDURE_DECL)
                              ? convert_procedure(node)
                              : convert_function(node);
            append_subprogram_node(&nested_subs, sub);
            break;
        }
        case PASCAL_T_FUNCTION_BODY:
            convert_routine_body(node, &const_decls, &var_builder, &label_builder,
                                 &nested_subs, &body, &type_decls);
            break;
        case PASCAL_T_BEGIN_BLOCK:
            body = convert_block(node);
            break;
        case PASCAL_T_ASM_BLOCK: {
            struct Statement *stmt = convert_statement(node);
            ListBuilder stmts_builder;
            list_builder_init(&stmts_builder);
            if (stmt != NULL)
                list_builder_append(&stmts_builder, stmt, LIST_STMT);
            body = mk_compoundstatement(node->line, list_builder_finish(&stmts_builder));
            break;
        }
        case PASCAL_T_IDENTIFIER: {
            char *dir_sym = dup_symbol(node);
            if (dir_sym != NULL) {
                free(dir_sym);
            }
            break;
        }
        default:
            break;
        }
        cur = cur->next;
    }

    /* Constructors implicitly return their class instance as a pointer. */
    if (is_constructor && !has_return_type) {
        has_return_type = 1;
        if (return_type_id != NULL)
            free(return_type_id);
        return_type_id = (effective_class != NULL) ? strdup(effective_class) : NULL;
        return_type = POINTER_TYPE;
    }

    /* Wrap method body in WITH Self for instance methods, but not for helpers, class operators, or static methods */
    if (body != NULL && !is_class_operator && !is_static_method && !is_helper_method) {
        struct Expression *self_expr = mk_varid(method_node->line, strdup("Self"));
        body = mk_with(method_node->line, self_expr, body);
    }

    ListNode_t *params = list_builder_finish(&params_builder);
    ListNode_t *label_decls = list_builder_finish(&label_builder);

    if (is_class_operator && method_name != NULL && mangle_owner != NULL)
    {
        char *encoded_method = encode_operator_name(method_name);
        char *param_suffix = NULL;
        char *ret_suffix = NULL;
        if (params != NULL && params->cur != NULL)
            param_suffix = method_param_type_suffix((Tree_t *)params->cur);
        if (return_type_ref != NULL)
            ret_suffix = type_ref_render_mangled(return_type_ref);
        if (ret_suffix == NULL && return_type_id != NULL)
            ret_suffix = strdup(return_type_id);

        if (encoded_method != NULL && (param_suffix != NULL || ret_suffix != NULL))
        {
            size_t name_len = strlen(mangle_owner) + strlen(encoded_method) + 3;
            if (param_suffix != NULL)
                name_len += strlen(param_suffix) + 1;
            if (ret_suffix != NULL)
                name_len += strlen(ret_suffix) + 1;
            char *disambiguated = (char *)malloc(name_len);
            if (disambiguated != NULL)
            {
                int written = snprintf(disambiguated, name_len, "%s__%s", mangle_owner, encoded_method);
                if (written > 0 && (size_t)written < name_len)
                {
                    size_t used = (size_t)written;
                    if (param_suffix != NULL)
                    {
                        snprintf(disambiguated + used, name_len - used, "_%s", param_suffix);
                        used = strlen(disambiguated);
                    }
                    if (ret_suffix != NULL)
                        snprintf(disambiguated + used, name_len - used, "_%s", ret_suffix);
                    free(proc_name);
                    proc_name = disambiguated;
                }
                else
                {
                    free(disambiguated);
                }
            }
        }

        free(encoded_method);
        free(param_suffix);
        free(ret_suffix);
    }
    
    Tree_t *tree;
    if (has_return_type) {
        tree = mk_function(method_node->line, proc_name, params, const_decls,
                          label_decls, type_decls, list_builder_finish(&var_builder),
                          nested_subs, body, return_type, return_type_id, inline_return_type, 0, 0);
    } else {
        tree = mk_procedure(method_node->line, proc_name, params, const_decls,
                           label_decls, type_decls, list_builder_finish(&var_builder),
                           nested_subs, body, 0, 0);
    }
    if (tree != NULL && method_node->index >= 0)
        tree->source_index = method_node->index + g_source_offset;
    if (!is_nostackframe)
        is_nostackframe = ast_has_routine_directive(method_node, "nostackframe", 8);
    if (tree != NULL && is_nostackframe)
        tree->tree_data.subprogram_data.nostackframe = 1;
    if (tree != NULL) {
        if (has_return_type)
            tree->tree_data.subprogram_data.return_type_ref =
                return_type_ref != NULL ? return_type_ref : type_ref_from_single_name(return_type_id);
        else if (return_type_ref != NULL)
            type_ref_free(return_type_ref);
        /* Use the full dotted class path for mangled_id so that
         * semcheck_get_current_method_owner returns the full nested path.
         * This allows const lookup to fall back to outer classes.
         * E.g., for TManager.TState.Init, mangled_id = "TManager.TState__Init" */
        if (effective_class_full != NULL && effective_class_full != effective_class)
        {
            char *full_mangled = mangle_method_name_raw(effective_class_full, method_name);
            tree->tree_data.subprogram_data.mangled_id = full_mangled != NULL ? full_mangled : strdup(proc_name);
        }
        else
        {
            tree->tree_data.subprogram_data.mangled_id = strdup(proc_name);
        }
        tree->tree_data.subprogram_data.method_name = (char *)string_intern(method_name);
        tree->tree_data.subprogram_data.owner_class = (char *)string_intern(effective_class);
        tree->tree_data.subprogram_data.is_constructor = is_constructor;
        tree->tree_data.subprogram_data.is_static_method = is_static_method;
        tree->tree_data.subprogram_data.is_operator = method_declares_operator;
        if (effective_class_full != NULL && effective_class_full != effective_class)
            tree->tree_data.subprogram_data.owner_class_full = (char *)string_intern(effective_class_full);
        if (tree->tree_data.subprogram_data.owner_class_full == NULL &&
            tree->tree_data.subprogram_data.owner_class != NULL &&
            strchr(tree->tree_data.subprogram_data.owner_class, '.') != NULL)
        {
            tree->tree_data.subprogram_data.owner_class_full =
                tree->tree_data.subprogram_data.owner_class;
        }
        if (effective_class_outer != NULL)
            tree->tree_data.subprogram_data.owner_class_outer = (char *)string_intern(effective_class_outer);
        if (tree->tree_data.subprogram_data.owner_class_outer == NULL &&
            tree->tree_data.subprogram_data.owner_class_full != NULL)
        {
            const char *full = tree->tree_data.subprogram_data.owner_class_full;
            const char *last_dot = strrchr(full, '.');
            if (last_dot != NULL && last_dot > full)
            {
                size_t len = (size_t)(last_dot - full);
                char *temp = strndup(full, len);
                if (temp != NULL)
                {
                    tree->tree_data.subprogram_data.owner_class_outer = (char *)string_intern(temp);
                    free(temp);
                }
            }
        }
        if (num_generic_type_params > 0) {
            tree->tree_data.subprogram_data.generic_type_params = generic_type_params;
            tree->tree_data.subprogram_data.num_generic_type_params = num_generic_type_params;
            tree->tree_data.subprogram_data.is_generic_template = 1;
            tree->tree_data.subprogram_data.generic_template_ast = copy_ast_detached(method_node);
            tree->tree_data.subprogram_data.generic_template_source_offset = g_source_offset;
            generic_type_params = NULL;
            num_generic_type_params = 0;
        }
    }
    else if (return_type_ref != NULL)
    {
        type_ref_free(return_type_ref);
    }

    record_generic_method_impl(effective_class, method_name, method_node);
    
    /* Check if this method belongs to a generic type. If so, we've recorded the AST
     * template above and should NOT generate a concrete implementation. Return NULL
     * to prevent adding this to the subprograms list. */
    GenericTypeDecl *generic_decl = generic_registry_find_decl(effective_class);
    if (generic_decl != NULL && generic_decl->record_template != NULL) {
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL && effective_class != NULL && method_name != NULL) {
            fprintf(stderr, "[KGPC] convert_method_impl: recorded template for %s.%s, not generating concrete impl\n", 
                    effective_class, method_name);
        }
        if (cleaned_class_name != NULL)
            free(cleaned_class_name);
        free(class_name);
        free(method_name);
        free(effective_class_last);
        free(effective_class_outer);
        if (method_param_sig != NULL)
            free(method_param_sig);
        g_current_method_name = prev_method_name_ctx;
        return NULL;
    }
    
    if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL && effective_class != NULL && method_name != NULL) {
        fprintf(stderr, "[KGPC] convert_method_impl: class=%s method=%s\n", effective_class, method_name);
    }

    if (cleaned_class_name != NULL)
        free(cleaned_class_name);
    if (generic_type_params != NULL) {
        for (int i = 0; i < num_generic_type_params; i++)
            free(generic_type_params[i]);
        free(generic_type_params);
    }
    free(class_name);
    free(method_name);
    free(effective_class_last);
    free(effective_class_outer);
    if (method_param_sig != NULL)
        free(method_param_sig);
    g_current_method_name = prev_method_name_ctx;
    return tree;
}

/* Extract generic type parameter names from a PASCAL_T_TYPE_PARAM_LIST AST node.
 * Returns the number of type parameters found, and fills *out_params with
 * a malloc'd array of strdup'd parameter names. */
int extract_generic_type_params(ast_t *type_param_list, char ***out_params) {
    assert(type_param_list != NULL);
    assert(type_param_list->typ == PASCAL_T_TYPE_PARAM_LIST);
    assert(out_params != NULL);

    int count = 0;
    for (ast_t *p = type_param_list->child; p != NULL; p = p->next) {
        if (p->typ == PASCAL_T_IDENTIFIER)
            count++;
    }
    if (count == 0) {
        *out_params = NULL;
        return 0;
    }
    char **params = (char **)malloc(sizeof(char *) * count);
    assert(params != NULL);
    int i = 0;
    for (ast_t *p = type_param_list->child; p != NULL; p = p->next) {
        if (p->typ == PASCAL_T_IDENTIFIER) {
            params[i] = dup_symbol(p);
            assert(params[i] != NULL);
            i++;
        }
    }
    *out_params = params;
    return count;
}

char *extract_external_name_from_node(ast_t *node)
{
    if (node == NULL || node->child == NULL)
        return NULL;

    if (node->child->typ == PASCAL_T_STRING)
        return dup_symbol(node->child);

    if (node->child->typ == PASCAL_T_IDENTIFIER)
        return dup_symbol(node->child);

    if (node->child->typ == PASCAL_T_EXTERNAL_NAME_EXPR &&
        node->child->child != NULL &&
        (node->child->child->typ == PASCAL_T_STRING ||
         node->child->child->typ == PASCAL_T_IDENTIFIER))
        return dup_symbol(node->child->child);

    for (ast_t *c = node->child; c != NULL; c = c->next) {
        if (c->typ == PASCAL_T_STRING || c->typ == PASCAL_T_IDENTIFIER)
            return dup_symbol(c);
    }

    return NULL;
}

/* ---- Shared state and helpers for convert_procedure/convert_function ---- */

/* Mutable state accumulated while walking routine declaration AST children.
 * Used by process_subprogram_declarations() to avoid duplicating ~190 lines
 * of switch/case logic between convert_procedure and convert_function.
 *
 * Lifecycle: init with subprogram_decl_state_init(), populate via
 * process_subprogram_declarations(), then transfer ownership of results
 * to the Tree_t via finalize_subprogram_tree().
 *
 * Ownership: external_alias, internproc_id_str, internconst_id_str are
 * heap-allocated strings owned by this struct.  finalize_subprogram_tree()
 * transfers them to the tree node or frees them if tree is NULL. */
typedef struct {
    ListNode_t  *const_decls;
    ListBuilder  var_decls_builder;
    ListBuilder  label_decls_builder;
    ListNode_t  *nested_subs;
    struct Statement *body;
    int           is_external;
    int           is_nostackframe;
    int           is_varargs;
    char         *external_alias;
    char         *internproc_id_str;
    char         *internconst_id_str;
    ast_t        *type_section_ast;
    ListNode_t   *type_decls;
} SubprogramDeclState;

static void subprogram_decl_state_init(SubprogramDeclState *s) {
    s->const_decls = NULL;
    list_builder_init(&s->var_decls_builder);
    list_builder_init(&s->label_decls_builder);
    s->nested_subs = NULL;
    s->body = NULL;
    s->is_external = 0;
    s->is_nostackframe = 0;
    s->is_varargs = 0;
    s->external_alias = NULL;
    s->internproc_id_str = NULL;
    s->internconst_id_str = NULL;
    s->type_section_ast = NULL;
    s->type_decls = NULL;
}

/* Process the body/declaration children of a procedure or function AST node.
 * This is the shared core that handles type/const/var sections, nested
 * subprograms, body blocks, directives, extern names, etc.
 * On entry, *cursor points to the first child node to process.
 * On return, *cursor is NULL (all children consumed). */
static void process_subprogram_declarations(ast_t **cursor, SubprogramDeclState *s) {
    ast_t *cur = *cursor;
    while (cur != NULL) {
        switch (cur->typ) {
        case PASCAL_T_TYPE_SECTION:
            append_type_decls_from_section(cur, &s->type_decls, &s->nested_subs,
                &s->const_decls, &s->var_decls_builder, NULL);
            s->type_section_ast = cur;
            break;
        case PASCAL_T_CONST_SECTION:
            append_const_decls_from_section(cur, &s->const_decls, &s->var_decls_builder, s->type_section_ast);
            break;
        case PASCAL_T_VAR_SECTION:
            list_builder_extend(&s->var_decls_builder, convert_var_section(cur));
            break;
        case PASCAL_T_LABEL_SECTION:
            append_labels_from_section(cur, &s->label_decls_builder);
            break;
        case PASCAL_T_PROCEDURE_DECL:
        case PASCAL_T_FUNCTION_DECL: {
            Tree_t *sub = (cur->typ == PASCAL_T_PROCEDURE_DECL)
                              ? convert_procedure(cur)
                              : convert_function(cur);
            append_subprogram_node(&s->nested_subs, sub);
            break;
        }
        case PASCAL_T_METHOD_IMPL: {
            Tree_t *method_tree = convert_method_impl(cur);
            append_subprogram_node(&s->nested_subs, method_tree);
            break;
        }
        case PASCAL_T_FUNCTION_BODY:
            convert_routine_body(cur, &s->const_decls, &s->var_decls_builder, &s->label_decls_builder,
                                 &s->nested_subs, &s->body, &s->type_decls);
            break;
        case PASCAL_T_BEGIN_BLOCK:
            s->body = convert_block(cur);
            break;
        case PASCAL_T_ASM_BLOCK: {
            struct Statement *stmt = convert_statement(cur);
            ListBuilder stmts_builder;
            list_builder_init(&stmts_builder);
            if (stmt != NULL)
                list_builder_append(&stmts_builder, stmt, LIST_STMT);
            s->body = mk_compoundstatement(cur->line, list_builder_finish(&stmts_builder));
            break;
        }
        case PASCAL_T_IDENTIFIER: {
            char *self_sym = dup_symbol(cur);
            if (self_sym != NULL) {
                if (is_external_directive(self_sym))
                    s->is_external = 1;
                else if (strcasecmp(self_sym, "alias") == 0) {
                    ast_t *val = cur->child;
                    if (val == NULL || val->typ != PASCAL_T_STRING)
                        val = cur->next;
                    if (val != NULL && val->typ == PASCAL_T_STRING) {
                        char *name = dup_symbol(val);
                        if (name != NULL) {
                            if (s->external_alias != NULL)
                                free(s->external_alias);
                            s->external_alias = name;
                        }
                        if (val == cur->next)
                            cur = cur->next;
                    }
                } else if (strcasecmp(self_sym, "internproc") == 0 ||
                           strcasecmp(self_sym, "internconst") == 0 ||
                           strcasecmp(self_sym, "compilerproc") == 0) {
                    s->is_external = 1;
                    ast_t *val = cur->next;
                    if (val != NULL && val->typ == PASCAL_T_IDENTIFIER &&
                        val->sym != NULL && val->sym->name != NULL) {
                        if (strcasecmp(self_sym, "internconst") == 0) {
                            if (s->internconst_id_str != NULL)
                                free(s->internconst_id_str);
                            s->internconst_id_str = strdup(val->sym->name);
                        } else {
                            if (s->internproc_id_str != NULL)
                                free(s->internproc_id_str);
                            s->internproc_id_str = strdup(val->sym->name);
                        }
                        if (s->external_alias != NULL)
                            free(s->external_alias);
                        s->external_alias = strdup(val->sym->name);
                        cur = cur->next;
                    }
                } else {
                    if (strncasecmp(self_sym, "fpc_in_", 7) == 0 ||
                        strncasecmp(self_sym, "fpc_", 4) == 0) {
                        s->is_external = 1;
                        if (s->external_alias != NULL)
                            free(s->external_alias);
                        s->external_alias = strdup(self_sym);
                        if (s->internproc_id_str != NULL)
                            free(s->internproc_id_str);
                        s->internproc_id_str = strdup(self_sym);
                    }
                }
                free(self_sym);
            }
            if (cur->child != NULL && cur->child->typ == PASCAL_T_IDENTIFIER) {
                char *directive = dup_symbol(cur->child);
                if (directive != NULL) {
                    if (is_external_directive(directive))
                        s->is_external = 1;
                }
                free(directive);
            }
            break;
        }
        case PASCAL_T_EXTERNAL_NAME:
        case PASCAL_T_EXTERNAL_NAME_EXPR:
            {
                char *name = extract_external_name_from_node(cur);
                if (name != NULL) {
                    if (s->external_alias != NULL)
                        free(s->external_alias);
                    s->external_alias = name;
                }
                s->is_external = 1;
            }
            break;
        case PASCAL_T_NONE:
            {
                for (ast_t *child = cur->child; child != NULL; child = child->next) {
                    if (child->typ == PASCAL_T_IDENTIFIER && child->sym != NULL) {
                        char *kw = child->sym->name;
                        if (strcasecmp(kw, "internproc") == 0 ||
                            strcasecmp(kw, "internconst") == 0 ||
                            strcasecmp(kw, "compilerproc") == 0) {
                            s->is_external = 1;
                            ast_t *val = child->next;
                            while (val != NULL && val->typ == PASCAL_T_NONE)
                                val = val->child;
                            if (val != NULL && val->typ == PASCAL_T_IDENTIFIER) {
                                if (strcasecmp(kw, "internconst") == 0) {
                                    if (s->internconst_id_str != NULL)
                                        free(s->internconst_id_str);
                                    s->internconst_id_str = dup_symbol(val);
                                } else {
                                    if (s->internproc_id_str != NULL)
                                        free(s->internproc_id_str);
                                    s->internproc_id_str = dup_symbol(val);
                                }
                                if (s->external_alias != NULL)
                                    free(s->external_alias);
                                s->external_alias = dup_symbol(val);
                            } else if (val != NULL && val->typ == PASCAL_T_STRING) {
                                if (strcasecmp(kw, "internconst") == 0) {
                                    if (s->internconst_id_str != NULL)
                                        free(s->internconst_id_str);
                                    s->internconst_id_str = dup_symbol(val);
                                } else {
                                    if (s->internproc_id_str != NULL)
                                        free(s->internproc_id_str);
                                    s->internproc_id_str = dup_symbol(val);
                                }
                                if (s->external_alias != NULL)
                                    free(s->external_alias);
                                s->external_alias = dup_symbol(val);
                            }
                        } else if (strcasecmp(kw, "external") == 0) {
                            s->is_external = 1;
                        }
                    } else if (child->typ == PASCAL_T_EXTERNAL_NAME || child->typ == PASCAL_T_EXTERNAL_NAME_EXPR) {
                        char *name = extract_external_name_from_node(child);
                        if (name != NULL) {
                            if (s->external_alias != NULL)
                                free(s->external_alias);
                            s->external_alias = name;
                        }
                        s->is_external = 1;
                    }
                }
            }
            break;
        default:
            break;
        }
        cur = cur->next;
    }
    *cursor = cur;
}

/* Finalize the externals/generic state on a freshly-created subprogram tree.
 * Transfers ownership of external_alias, internproc_id_str, internconst_id_str
 * and generic_type_params to the tree, or frees them if tree is NULL.
 * Also sets nostackframe/varargs/source_index. */
static void finalize_subprogram_tree(Tree_t *tree, ast_t *node,
    SubprogramDeclState *s, char **generic_type_params, int num_generic_type_params)
{
    if (tree != NULL && node->index >= 0)
        tree->source_index = node->index + g_source_offset;
    if (!s->is_nostackframe)
        s->is_nostackframe = ast_has_routine_directive(node, "nostackframe", 8);
    if (tree != NULL && s->is_nostackframe)
        tree->tree_data.subprogram_data.nostackframe = 1;
    if (tree != NULL && s->is_varargs)
        tree->tree_data.subprogram_data.is_varargs = 1;
    if (tree != NULL && s->is_external && s->external_alias == NULL &&
        tree->tree_data.subprogram_data.id != NULL)
        s->external_alias = strdup(tree->tree_data.subprogram_data.id);
    if (tree != NULL && s->external_alias != NULL)
        tree->tree_data.subprogram_data.cname_override = s->external_alias;
    else if (s->external_alias != NULL)
        free(s->external_alias);
    s->external_alias = NULL;
    if (tree != NULL && s->internproc_id_str != NULL)
        tree->tree_data.subprogram_data.internproc_id = s->internproc_id_str;
    else if (s->internproc_id_str != NULL)
        free(s->internproc_id_str);
    s->internproc_id_str = NULL;
    if (tree != NULL && s->internconst_id_str != NULL)
        tree->tree_data.subprogram_data.internconst_id = s->internconst_id_str;
    else if (s->internconst_id_str != NULL)
        free(s->internconst_id_str);
    s->internconst_id_str = NULL;
    if (tree != NULL && num_generic_type_params > 0) {
        tree->tree_data.subprogram_data.generic_type_params = generic_type_params;
        tree->tree_data.subprogram_data.num_generic_type_params = num_generic_type_params;
        tree->tree_data.subprogram_data.is_generic_template = 1;
        tree->tree_data.subprogram_data.generic_template_ast = copy_ast_detached(node);
        tree->tree_data.subprogram_data.generic_template_source_offset = g_source_offset;
    } else if (generic_type_params != NULL) {
        for (int i = 0; i < num_generic_type_params; i++)
            free(generic_type_params[i]);
        free(generic_type_params);
    }
}

/* ---- End shared subprogram helpers ---- */

Tree_t *convert_procedure(ast_t *proc_node) {
    ast_t *cur = proc_node->child;
    char *id = NULL;
    static int debug_external_nodes = -1;
    if (debug_external_nodes == -1)
        debug_external_nodes = (kgpc_getenv("KGPC_DEBUG_EXTERNAL") != NULL);

    while (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER &&
           cur->sym != NULL && cur->sym->name != NULL &&
           (strcasecmp(cur->sym->name, "generic") == 0 ||
            strcasecmp(cur->sym->name, "class") == 0))
    {
        cur = cur->next;
    }

    if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER)
        id = dup_symbol(cur);

    /* Standalone operator declarations sometimes arrive through the
     * procedure-declaration grammar path even though they have a return type.
     * Reuse convert_function() so interface declarations get the same
     * mangled ids as implementations (e.g. TMyType__op_sub_TMyType). */
    if (id != NULL &&
        (strcasecmp(id, "operator") == 0 || is_operator_token_name(id)))
    {
        free(id);
        return convert_function(proc_node);
    }

    if (cur != NULL)
        cur = cur->next;

    /* Skip generic type parameter list (e.g., <T, U>) */
    char **generic_type_params = NULL;
    int num_generic_type_params = 0;
    if (cur != NULL && cur->typ == PASCAL_T_TYPE_PARAM_LIST) {
        num_generic_type_params = extract_generic_type_params(cur, &generic_type_params);
        cur = cur->next;
    }

    ListNode_t *params = NULL;
    if (cur != NULL && cur->typ == PASCAL_T_PARAM_LIST) {
        ast_t *param_cursor = cur->child;
        params = convert_param_list(&param_cursor);
        cur = cur->next;
    } else {
        while (cur != NULL && cur->typ == PASCAL_T_PARAM) {
            ListNode_t *param_nodes = convert_param(cur);
            extend_list(&params, param_nodes);
            cur = cur->next;
        }
    }

    SubprogramDeclState ds;
    subprogram_decl_state_init(&ds);
    process_subprogram_declarations(&cur, &ds);

    if (!ds.is_varargs)
        ds.is_varargs = ast_has_routine_directive(proc_node, "varargs", 8);
    ListNode_t *label_decls = list_builder_finish(&ds.label_decls_builder);
    Tree_t *tree = mk_procedure(proc_node->line, id, params, ds.const_decls,
                                label_decls, ds.type_decls, list_builder_finish(&ds.var_decls_builder),
                                ds.nested_subs, ds.body, ds.is_external, 0);
    finalize_subprogram_tree(tree, proc_node, &ds, generic_type_params, num_generic_type_params);
    return tree;
}

Tree_t *convert_function(ast_t *func_node) {
    ast_t *cur = func_node->child;
    char *id = NULL;
    char *operator_symbol = NULL;  /* For standalone operators */
    int is_standalone_operator = 0;
    static int debug_external_nodes = -1;
    if (debug_external_nodes == -1)
        debug_external_nodes = (kgpc_getenv("KGPC_DEBUG_EXTERNAL") != NULL);

    while (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER &&
           cur->sym != NULL && cur->sym->name != NULL &&
           (strcasecmp(cur->sym->name, "generic") == 0 ||
            strcasecmp(cur->sym->name, "class") == 0))
    {
        cur = cur->next;
    }

    if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER)
        id = dup_symbol(cur);

    /* Check if this is a standalone operator declaration */
    if (id != NULL &&
        (strcasecmp(id, "operator") == 0 || is_operator_token_name(id))) {
        is_standalone_operator = 1;
        if (is_operator_token_name(id)) {
            operator_symbol = strdup(id);
        } else if (cur != NULL) {
            /* Get the operator symbol from the next child */
            cur = cur->next;
            if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER) {
                operator_symbol = dup_symbol(cur);
            }
        }
    }

    if (cur != NULL) {
        cur = cur->next;
    }

    /* Skip generic type parameter list (e.g., <T, U>) */
    char **generic_type_params = NULL;
    int num_generic_type_params = 0;
    if (cur != NULL && cur->typ == PASCAL_T_TYPE_PARAM_LIST) {
        num_generic_type_params = extract_generic_type_params(cur, &generic_type_params);
        cur = cur->next;
    }

    ListNode_t *params = NULL;
    if (cur != NULL && cur->typ == PASCAL_T_PARAM_LIST) {
        ast_t *param_cursor = cur->child;
        params = convert_param_list(&param_cursor);
        cur = cur->next;
    } else {
        while (cur != NULL && cur->typ == PASCAL_T_PARAM) {
            ListNode_t *param_nodes = convert_param(cur);
            extend_list(&params, param_nodes);
            cur = cur->next;
        }
    }

    /* For standalone operators, save operator info now but defer name mangling
     * until after the return type is parsed, so we can include the return type
     * in the mangled name to disambiguate overloads like variant__op_assign_byte
     * vs variant__op_assign_real. */
    char *deferred_encoded_op = NULL;
    const char *deferred_param_type_id = NULL;
    const char *deferred_second_param_type_id = NULL;
    if (is_standalone_operator && operator_symbol != NULL && params != NULL) {
        /* Get the type of the first parameter (params is a list of Tree_t* with TREE_VAR_DECL) */
        Tree_t *first_param = (Tree_t *)params->cur;
        if (kgpc_getenv("KGPC_DEBUG_OPERATOR") != NULL) {
            fprintf(stderr, "[Operator] is_standalone_operator=%d operator_symbol=%s\n",
                is_standalone_operator, operator_symbol);
        }
        if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
            first_param->tree_data.var_decl_data.type_id != NULL) {
            deferred_encoded_op = encode_operator_name(operator_symbol);
            /* Capture second param type for binary operator disambiguation */
            if (params->next != NULL) {
                Tree_t *second_param = (Tree_t *)params->next->cur;
                if (second_param != NULL && second_param->type == TREE_VAR_DECL &&
                    second_param->tree_data.var_decl_data.type_id != NULL)
                    deferred_second_param_type_id = second_param->tree_data.var_decl_data.type_id;
            }
            deferred_param_type_id = first_param->tree_data.var_decl_data.type_id;
        }
        free(operator_symbol);
        operator_symbol = NULL;
    } else if (operator_symbol != NULL) {
        free(operator_symbol);
    }

    char *return_type_id = NULL;
    int return_type = UNKNOWN_TYPE;
    struct TypeAlias *inline_return_type = NULL;
    char *result_var_name = NULL;
    TypeRef *return_type_ref = NULL;

    /* For operator declarations with named results (e.g., "operator :=(source: byte) dest: variant"),
     * the named result identifier appears as a PASCAL_T_IDENTIFIER node before PASCAL_T_RETURN_TYPE. */
    if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER && is_standalone_operator) {
        result_var_name = dup_symbol(cur);
        cur = cur->next;
    }

    if (cur != NULL && cur->typ == PASCAL_T_RETURN_TYPE) {
        TypeInfo type_info = {0};
        return_type = convert_type_spec(cur->child, &return_type_id, NULL, &type_info);
        /* When the PASCAL_T_RETURN_TYPE node has no child (e.g. interface/forward
         * declarations where the parser strips the type subtree), fall back to
         * the sym name on the RETURN_TYPE node itself.  Only apply the {$H-}
         * remapping for bare 'string'; leave other types as UNKNOWN_TYPE for
         * semcheck to resolve via the symbol table. */
        if (return_type == UNKNOWN_TYPE && return_type_id == NULL &&
            cur->sym != NULL && cur->sym->name != NULL)
        {
            if (pascal_frontend_default_shortstring() &&
                strcasecmp(cur->sym->name, "string") == 0)
            {
                return_type = SHORTSTRING_TYPE;
                return_type_id = strdup(cur->sym->name);
            }
        }
        if (return_type_ref == NULL)
            return_type_ref = type_ref_from_info_or_id(&type_info, return_type_id);
        if (return_type_id == NULL && cur->sym != NULL && cur->sym->name != NULL)
            return_type_id = strdup(cur->sym->name);
        inline_return_type = build_inline_return_alias(&type_info, return_type, return_type_id);
        cur = cur->next;
    }

    /* Build operator names: the base name (TypeName__op_suffix) goes into id
     * so FindAllIdents can find all overloads.  The full suffixed name (with
     * param types and return type) goes into mangled_id for unique asm labels,
     * avoiding collisions between overloads with same params but different
     * return types (e.g. Tconstexprint__op_assign returning qword vs int64). */
    char *operator_mangled_id = NULL;
    if (deferred_encoded_op != NULL && deferred_param_type_id != NULL) {
        const char *ret_suffix = return_type_id;
        if (ret_suffix == NULL) ret_suffix = result_var_name;

        /* Set id to the base name: TypeName__op_suffix */
        size_t base_len = strlen(deferred_param_type_id) + strlen(deferred_encoded_op) + 3;
        char *base_name = (char *)malloc(base_len);
        if (base_name != NULL) {
            snprintf(base_name, base_len, "%s__%s",
                deferred_param_type_id, deferred_encoded_op);
            free(id);
            id = base_name;
        }

        /* Build the full suffixed name for mangled_id */
        size_t name_len = strlen(deferred_param_type_id) + strlen(deferred_encoded_op) + 3
            + (ret_suffix != NULL ? strlen(ret_suffix) + 1 : 0)
            + (deferred_second_param_type_id != NULL ? strlen(deferred_second_param_type_id) + 1 : 0);
        char *mangled_name = (char *)malloc(name_len);
        if (mangled_name != NULL) {
            if (ret_suffix != NULL && deferred_second_param_type_id != NULL) {
                snprintf(mangled_name, name_len, "%s__%s_%s_%s",
                    deferred_param_type_id, deferred_encoded_op,
                    deferred_second_param_type_id, ret_suffix);
            } else if (ret_suffix != NULL) {
                snprintf(mangled_name, name_len, "%s__%s_%s",
                    deferred_param_type_id, deferred_encoded_op, ret_suffix);
            } else if (deferred_second_param_type_id != NULL) {
                snprintf(mangled_name, name_len, "%s__%s_%s",
                    deferred_param_type_id, deferred_encoded_op,
                    deferred_second_param_type_id);
            } else {
                snprintf(mangled_name, name_len, "%s__%s",
                    deferred_param_type_id, deferred_encoded_op);
            }
            operator_mangled_id = mangled_name;
        }
        free(deferred_encoded_op);
        deferred_encoded_op = NULL;
    }
    if (deferred_encoded_op != NULL) free(deferred_encoded_op);

    SubprogramDeclState ds;
    subprogram_decl_state_init(&ds);
    process_subprogram_declarations(&cur, &ds);

    if (!ds.is_varargs)
        ds.is_varargs = ast_has_routine_directive(func_node, "varargs", 8);
    ListNode_t *label_decls = list_builder_finish(&ds.label_decls_builder);
    Tree_t *tree = mk_function(func_node->line, id, params, ds.const_decls,
                                label_decls, ds.type_decls, list_builder_finish(&ds.var_decls_builder), ds.nested_subs, ds.body,
                                return_type, return_type_id, inline_return_type, ds.is_external, 0);
    if (tree != NULL)
        tree->tree_data.subprogram_data.return_type_ref =
            return_type_ref != NULL ? return_type_ref : type_ref_from_single_name(return_type_id);
    else if (return_type_ref != NULL)
        type_ref_free(return_type_ref);
    finalize_subprogram_tree(tree, func_node, &ds, generic_type_params, num_generic_type_params);
    if (tree != NULL && result_var_name != NULL) {
        tree->tree_data.subprogram_data.result_var_name = result_var_name;
    } else if (result_var_name != NULL) {
        free(result_var_name);
    }
    if (tree != NULL && is_standalone_operator)
        tree->tree_data.subprogram_data.is_operator = 1;
    if (tree != NULL && operator_mangled_id != NULL)
        tree->tree_data.subprogram_data.mangled_id = operator_mangled_id;
    else if (operator_mangled_id != NULL)
        free(operator_mangled_id);
    return tree;
}

/**
 * Helper function to recursively search for an AST node with a specific type.
 * Uses depth-first search, checking children before siblings.
 * 
 * Traversal order:
 * 1. Check if current node matches target_type
 * 2. Recursively search in child chain
 * 3. Recursively search in sibling chain
 * 
 * @param node The node to start searching from
 * @param target_type The type ID to search for
 * @return The first node found with the target type, or NULL if not found
 */
ast_t *find_node_by_type(ast_t *node, int target_type) {
    if (node == NULL) {
        return NULL;
    }
    
    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] find_node_by_type: visiting node typ=%d, looking for typ=%d\n", 
                node->typ, target_type);
    }
    
    if (node->typ == target_type) {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] find_node_by_type: FOUND target typ=%d\n", target_type);
        }
        return node;
    }
    
    /* Search in children */
    ast_t *result = find_node_by_type(node->child, target_type);
    if (result != NULL) {
        return result;
    }
    
    /* Search in siblings */
    return find_node_by_type(node->next, target_type);
}

/**
 * Helper to find the last occurrence of a node type in a subtree.
 * Performs a depth-first traversal but keeps updating the result so that
 * the deepest/rightmost match is returned. This is useful when multiple
 * BEGIN blocks exist (e.g., function bodies plus program body) and we want
 * the final program block rather than the first nested block.
 */
static ast_t *find_last_node_by_type(ast_t *node, int target_type) {
    ast_t *last = NULL;
    while (node != NULL) {
        /* Recurse into children first to honor depth-first ordering */
        ast_t *child_last = find_last_node_by_type(node->child, target_type);
        if (child_last != NULL)
            last = child_last;
        
        if (node->typ == target_type)
            last = node;
        
        node = node->next;
    }
    return last;
}

static void resolve_deferred_arrays_in_list(ListNode_t *decl_list)
{
    for (ListNode_t *cur = decl_list; cur != NULL; cur = cur->next) {
        if (cur->type != LIST_TREE) continue;
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl == NULL || decl->type != TREE_ARR_DECL) continue;
        if (decl->tree_data.arr_decl_data.unresolved_index_type == NULL) continue;

        const char *type_name = decl->tree_data.arr_decl_data.unresolved_index_type;
        int start, end;
        if (enum_registry_lookup(type_name, &start, &end) == 0) {
            decl->tree_data.arr_decl_data.s_range = start;
            decl->tree_data.arr_decl_data.e_range = end;
            free(decl->tree_data.arr_decl_data.unresolved_index_type);
            decl->tree_data.arr_decl_data.unresolved_index_type = NULL;
        } else {
            /* Not an error: semcheck will resolve enum-indexed array bounds
             * from the symbol table at a later stage. */
        }
    }
}

void from_cparser_resolve_deferred_arrays(Tree_t *program)
{
    if (program == NULL) return;

    if (program->type == TREE_PROGRAM_TYPE) {
        resolve_deferred_arrays_in_list(program->tree_data.program_data.var_declaration);
    }
}

void from_cparser_cleanup(void)
{
    /* Free type helper mappings (strdup'd strings + struct + list nodes) */
    while (type_helper_mappings != NULL) {
        ListNode_t *next = type_helper_mappings->next;
        struct TypeHelperMapping *entry = (struct TypeHelperMapping *)type_helper_mappings->cur;
        if (entry != NULL) {
            free(entry->helper_id);
            free(entry->base_type_id);
            free(entry);
        }
        free(type_helper_mappings);
        type_helper_mappings = next;
    }

    /* Free class method bindings (interned strings - do NOT free, just free structs + list nodes) */
    cmb_index_reset();
    cmb_method_index_reset();
    while (class_method_bindings != NULL) {
        ListNode_t *next = class_method_bindings->next;
        ClassMethodBinding *binding = (ClassMethodBinding *)class_method_bindings->cur;
        if (binding != NULL && binding->param_sig != NULL)
            free(binding->param_sig);
        free(binding); /* ClassMethodBinding struct */
        free(class_method_bindings);
        class_method_bindings = next;
    }

    /* Free pending generic aliases */
    while (g_pending_generic_aliases != NULL) {
        PendingGenericAlias *next = g_pending_generic_aliases->next;
        free(g_pending_generic_aliases->base_name);
        if (g_pending_generic_aliases->type_args != NULL)
            destroy_list(g_pending_generic_aliases->type_args);
        free(g_pending_generic_aliases);
        g_pending_generic_aliases = next;
    }

    /* Free deferred inline specializations */
    while (g_deferred_inline_specs != NULL) {
        DeferredInlineSpec *next = g_deferred_inline_specs->next;
        if (g_deferred_inline_specs->type_decl != NULL)
            destroy_tree(g_deferred_inline_specs->type_decl);
        free(g_deferred_inline_specs);
        g_deferred_inline_specs = next;
    }

    /* Free scoped enum source cache */
    free(g_scoped_enum_source_path);
    g_scoped_enum_source_path = NULL;
    free(g_scoped_enum_source_buffer);
    g_scoped_enum_source_buffer = NULL;
    g_scoped_enum_source_length = 0;

    /* Reset const sections */
    reset_const_sections();

    /* Clear borrowed AST pointers */
    g_interface_type_section_ast = NULL;
    g_implementation_type_section_ast = NULL;
    g_interface_section_ast = NULL;
    g_implementation_section_ast = NULL;
    g_current_method_name = NULL;

    /* Reset counters */
    anonymous_method_counter = 0;
    typed_const_counter = 0;
    g_allow_pending_specializations = 0;
    g_frontend_error_count = 0;

    /* Free cross-unit enum registry */
    enum_registry_free();
}

Tree_t *tree_from_pascal_ast(ast_t *program_ast) {
    Tree_t *final_tree = NULL;
    if (program_ast == NULL)
        return NULL;

    typed_const_counter = 0;
    g_typed_const_unit_tag = "p";
    reset_const_sections();
    g_interface_type_section_ast = NULL;
    g_implementation_type_section_ast = NULL;

    ast_t *cur = program_ast;
    if (cur->typ == PASCAL_T_NONE)
        cur = cur->child;

    if (cur == NULL) {
        fprintf(stderr, "ERROR: Empty Pascal AST.\n");
        return NULL;
    }

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] tree_from_pascal_ast: root typ=%d\n", cur->typ);
    }
    
    if (cur->typ == PASCAL_T_PROGRAM_DECL || cur->typ == 88) {
        /* The structure of PASCAL_T_PROGRAM_DECL is:
         *   - optional(program_header)       [PASCAL_T_PROGRAM_HEADER or NULL/skipped]
         *   - optional(uses_section)          [PASCAL_T_USES_SECTION or NULL/skipped]
         *   - many(declaration_or_section)    [list of sections]
         *   - optional(main_block)            [PASCAL_T_MAIN_BLOCK or NULL/skipped]
         *   - "."
         * We need to check if the first child is a PASCAL_T_PROGRAM_HEADER.
         */
        ast_t *first_child = cur->child;
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] tree_from_pascal_ast: program block entered. first_child=%p\n", first_child);
            if (first_child) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: first_child->typ=%d\n", first_child->typ);
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: first_child->next=%p\n", first_child->next);
                
                // Print all siblings to understand the structure
                int sibling_count = 0;
                ast_t *sibling = first_child;
                while (sibling != NULL && sibling_count < 100) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: sibling[%d] typ=%d next=%p child=%p\n", 
                            sibling_count, sibling->typ, sibling->next, sibling->child);
                    sibling = sibling->next;
                    sibling_count++;
                }
                if (sibling != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: ... more siblings ...\n");
                }
                
                // Also check if there are children beyond the sibling chain
                // by looking at the last sibling's child
                if (sibling_count > 0) {
                    ast_t *last = first_child;
                    while (last->next != NULL) last = last->next;
                    if (last->child != NULL) {
                        fprintf(stderr, "[KGPC] tree_from_pascal_ast: Last sibling (typ=%d) has children:\n", last->typ);
                        ast_t *child = last->child;
                        int child_count = 0;
                        while (child != NULL && child_count < 20) {
                            fprintf(stderr, "[KGPC] tree_from_pascal_ast:   child[%d] typ=%d next=%p\n", child_count, child->typ, child->next);
                            
                            // Print siblings of this child
                            if (child->next != NULL) {
                                fprintf(stderr, "[KGPC] tree_from_pascal_ast:     Siblings of child[%d]:\n", child_count);
                                ast_t *sibling = child->next;
                                int sib_count = 0;
                                while (sibling != NULL && sib_count < 10) {
                                    fprintf(stderr, "[KGPC] tree_from_pascal_ast:       sibling[%d] typ=%d\n", sib_count, sibling->typ);
                                    sibling = sibling->next;
                                    sib_count++;
                                }
                            }
                            
                            child = child->next;
                            child_count++;
                        }
                    }
                }
            }
        }
        ast_t *program_header_node = NULL;
        char *program_id = NULL;
        
        /* Check if the first child is a program header */
        if (first_child != NULL && first_child->typ == PASCAL_T_PROGRAM_HEADER) {
            program_header_node = first_child;
            /* PASCAL_T_PROGRAM_HEADER structure:
             *   - "program" keyword (discarded by parser)
             *   - identifier (program name)
             *   - optional parameter list
             *   - ";" (discarded)
             */
            ast_t *name_node = program_header_node->child;
            if (name_node != NULL) {
                program_id = dup_symbol(name_node);
            }
        }
        
        /* If no program header or no program name, use default */
        if (program_id == NULL) {
            program_id = strdup("program");
        }

        ListNode_t *args = NULL;  /* Program parameters not currently supported */
        ListNode_t *uses = NULL;
        ListNode_t *const_decls = NULL;
        ListBuilder var_decls_builder;
        list_builder_init(&var_decls_builder);
        ListBuilder label_builder;
        list_builder_init(&label_builder);
        ListNode_t *type_decls = NULL;
        ast_t *type_section_ast = NULL;  /* Keep AST for enum resolution */
        ListNode_t *subprograms = NULL;
        struct Statement *body = NULL;
        int body_line = -1;

        /* Create visited set to detect circular references in top-level sections */
        VisitedSet *visited = visited_set_create();
        if (visited == NULL) {
            fprintf(stderr, "ERROR: Failed to allocate visited set for program sections\n");
            free(program_id);
            return NULL;
        }

        /* Start iterating from the first child, or the node after the program header */
        ast_t *section = program_header_node != NULL ? program_header_node->next : first_child;
        while (section != NULL) {
            if (kgpc_getenv("KGPC_DEBUG_PROGRAM_SECTIONS") != NULL) {
                fprintf(stderr, "[kgpc program] section typ=%d (%s) line=%d\n",
                    section->typ, pascal_tag_to_string(section->typ), section->line);
            }
            /* Check for circular reference before processing */
            if (!is_safe_to_continue(visited, section)) {
                fprintf(stderr, "ERROR: Circular reference detected in program sections, stopping traversal\n");
                break;
            }
            
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Visiting PROGRAM section type %d\n", section->typ);
            }
            
            switch (section->typ) {
            case PASCAL_T_CONST_SECTION:
                append_const_decls_from_section(section, &const_decls, &var_decls_builder, type_section_ast);
                break;
            case PASCAL_T_VAR_SECTION:
                list_builder_extend(&var_decls_builder, convert_var_section(section));
                break;
            case PASCAL_T_TYPE_SECTION:
                type_section_ast = section;  /* Save for enum resolution */
                append_type_decls_from_section(section, &type_decls, &subprograms,
                    &const_decls, &var_decls_builder, NULL);
                break;
            case PASCAL_T_USES_SECTION:
                append_uses_from_section(section, &uses);
                break;
            case PASCAL_T_LABEL_SECTION:
                append_labels_from_section(section, &label_builder);
                break;
            case PASCAL_T_PROCEDURE_DECL:
            case PASCAL_T_FUNCTION_DECL: {
                Tree_t *sub = (section->typ == PASCAL_T_PROCEDURE_DECL)
                                  ? convert_procedure(section)
                                  : convert_function(section);
                append_subprogram_node(&subprograms, sub);
                break;
            }
            case PASCAL_T_METHOD_IMPL:
            case PASCAL_T_CONSTRUCTOR_DECL:
            case PASCAL_T_DESTRUCTOR_DECL: {
                Tree_t *method_tree = convert_method_impl(section);
                append_subprogram_node(&subprograms, method_tree);
                break;
            }
            case PASCAL_T_PROPERTY_DECL: {
                /* Module-level property declaration (FPC extension). */
                append_module_property_wrappers(&subprograms, section);
                break;
            }
            case PASCAL_T_BEGIN_BLOCK:
            case PASCAL_T_MAIN_BLOCK: {
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found block type %d\n", section->typ);
                }
                struct Statement *candidate_body = convert_block(section);
                /* Prefer the last non-empty block. If the candidate has no statements,
                 * keep the previous body (if any) but still remember the candidate if
                 * we haven't found anything else. */
                if (candidate_body != NULL &&
                    candidate_body->stmt_data.compound_statement != NULL) {
                    if (section->line >= body_line) {
                        body = candidate_body;
                        body_line = section->line;
                    }
                } else if (body == NULL) {
                    body = candidate_body;
                    body_line = section->line;
                }
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: body assigned, body=%p\n", body);
                }
                break;
            }
            case PASCAL_T_TYPE_SPEC:
            case PASCAL_T_VAR_DECL:
            case PASCAL_T_TYPE_DECL:
            case PASCAL_T_CONST_DECL:
                /* These are components of sections, not top-level sections themselves.
                 * They should be children of VAR_SECTION, TYPE_SECTION, or CONST_SECTION.
                 * If we encounter them here, it means the AST structure is malformed,
                 * but we can safely skip them to continue parsing. */
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Skipping declaration component type %d (should be child of section)\n", section->typ);
                }
                break;
            default:
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Skipping unknown node type %d\n", section->typ);
                }
                break;
            }
            section = section->next;
        }
        
        /* If we haven't found a main block, search recursively in the AST tree.
         * This handles the case where the parser's seq() and many() combinators
         * don't properly link subsequent children in the sibling chain. */
        if (body == NULL) {
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Main block not found in sibling chain, searching recursively\n");
            }
            
            /* Search for VAR_SECTION */
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Searching for VAR_SECTION (typ=%d)\n", PASCAL_T_VAR_SECTION);
            }
            ast_t* var_section_node = find_node_by_type(cur->child, PASCAL_T_VAR_SECTION);
            if (var_section_node != NULL) {
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found VAR_SECTION via recursive search\n");
                }
                list_builder_extend(&var_decls_builder, convert_var_section(var_section_node));
            }
            
            /* Search for MAIN_BLOCK */
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Searching for MAIN_BLOCK (typ=%d)\n", PASCAL_T_MAIN_BLOCK);
            }
            ast_t* main_block_node = find_last_node_by_type(cur->child, PASCAL_T_MAIN_BLOCK);
            if (main_block_node == NULL) {
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: MAIN_BLOCK not found, searching for BEGIN_BLOCK (typ=%d)\n", PASCAL_T_BEGIN_BLOCK);
                }
                main_block_node = find_last_node_by_type(cur->child, PASCAL_T_BEGIN_BLOCK);
            }
            /* Also try typ=112 which appears in the AST */
            if (main_block_node == NULL) {
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: BEGIN_BLOCK not found, trying typ=112\n");
                }
                main_block_node = find_last_node_by_type(cur->child, 112);
            }
            if (main_block_node != NULL) {
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found MAIN_BLOCK via recursive search (typ=%d)\n", main_block_node->typ);
                }
                body = convert_block(main_block_node);
            } else {
                /* MAIN_BLOCK not found directly. Check if there's a typ=100 node that contains it */
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: MAIN_BLOCK not found directly, searching for typ=100 wrapper\n");
                }
                ast_t* wrapper_node = find_last_node_by_type(cur->child, 100);
                if (wrapper_node != NULL && wrapper_node->child != NULL) {
                    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                        fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found typ=100 wrapper, checking its children\n");
                    }
                    /* Check if the wrapper's child is a MAIN_BLOCK or BEGIN_BLOCK */
                    ast_t* child = wrapper_node->child;
                    int child_count = 0;
                    while (child != NULL) {
                        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                            fprintf(stderr, "[KGPC] tree_from_pascal_ast: typ=100 child[%d] has typ=%d\n", child_count, child->typ);
                        }
                        if (child->typ == PASCAL_T_MAIN_BLOCK || child->typ == PASCAL_T_BEGIN_BLOCK) {
                            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found MAIN_BLOCK (typ=%d) inside typ=100 wrapper\n", child->typ);
                            }
                            main_block_node = child;
                            body = convert_block(main_block_node);
                            break;
                        }
                        child = child->next;
                        child_count++;
                    }
                }
                
                if (body == NULL && kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: MAIN_BLOCK not found via recursive search\n");
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: PASCAL_T_MAIN_BLOCK=%d, PASCAL_T_BEGIN_BLOCK=%d\n", 
                            PASCAL_T_MAIN_BLOCK, PASCAL_T_BEGIN_BLOCK);
                }
            }
        }

        visited_set_destroy(visited);
        
        
        ListNode_t *label_decls = list_builder_finish(&label_builder);
        
        /* If no main block was found, create an empty one to avoid NULL body */
        if (body == NULL) {
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: No main block found, creating empty body\\n");
            }
            body = mk_compoundstatement(cur->line, NULL);
        }
        
        Tree_t *tree = mk_program(cur->line, program_id, args, uses, label_decls, const_decls,
                                  list_builder_finish(&var_decls_builder), type_decls, subprograms, body);
        final_tree = tree;
        /* Clear borrowed AST pointers before the caller frees the raw AST. */
        g_interface_type_section_ast = NULL;
        g_implementation_type_section_ast = NULL;
        return final_tree;
    }


    if (cur->typ == PASCAL_T_UNIT_DECL) {
        ast_t *unit_name_node = cur->child;
        char *unit_id = unit_name_node != NULL ? dup_symbol(unit_name_node) : strdup("unit");
        /* Lowercase the unit tag for deterministic typed-const labels */
        for (char *p = unit_id; p != NULL && *p != '\0'; p++)
            *p = (char)tolower((unsigned char)*p);
        g_typed_const_unit_tag = unit_id;
        typed_const_counter = 0;
        ast_t *unit_scan_copy = copy_ast(cur);

        ListNode_t *interface_uses = NULL;
        ListNode_t *interface_const_decls = NULL;
        ListNode_t *interface_type_decls = NULL;
        ListBuilder interface_var_builder;
        list_builder_init(&interface_var_builder);
        ast_t *interface_type_section_ast = NULL;  /* Track interface types for enum resolution */
        ListNode_t *implementation_uses = NULL;
        ListNode_t *implementation_const_decls = NULL;
        ListNode_t *implementation_type_decls = NULL;
        ListBuilder implementation_var_builder;
        list_builder_init(&implementation_var_builder);
        ast_t *implementation_type_section_ast = NULL;  /* Track implementation types for enum resolution */
        ListNode_t *subprograms = NULL;
        struct Statement *initialization = NULL;
        struct Statement *finalization = NULL;

        // Navigate through unit children to find sections by type
        ast_t *section = unit_name_node != NULL ? unit_name_node->next : NULL;
        ast_t *interface_node = NULL;
        ast_t *implementation_node = NULL;
        ast_t *initialization_node = NULL;
        ast_t *finalization_node = NULL;

        /* Create visited set for unit-level sections */
        VisitedSet *visited_unit = visited_set_create();
        if (visited_unit == NULL) {
            fprintf(stderr, "ERROR: Failed to allocate visited set for unit sections\n");
            free(unit_id);
            if (unit_scan_copy != NULL)
                free_ast(unit_scan_copy);
            return NULL;
        }

        while (section != NULL) {
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Visiting section type %d\n", section->typ);
            }
            /* Check for circular reference */
            if (!is_safe_to_continue(visited_unit, section)) {
                fprintf(stderr, "ERROR: Circular reference detected in unit sections, stopping traversal\n");
                break;
            }
            
            if (section->typ == PASCAL_T_INTERFACE_SECTION) {
                interface_node = section;
                if (kgpc_getenv("KGPC_DEBUG_UNIT_SECTIONS") != NULL) {
                    fprintf(stderr, "[UNIT_SECTIONS] interface at line %d\n", section->line);
                }
            } else if (section->typ == PASCAL_T_IMPLEMENTATION_SECTION) {
                implementation_node = section;
                if (kgpc_getenv("KGPC_DEBUG_UNIT_SECTIONS") != NULL) {
                    fprintf(stderr, "[UNIT_SECTIONS] implementation at line %d\n", section->line);
                }
            } else if (section->typ == PASCAL_T_INITIALIZATION_SECTION) {
                initialization_node = section;
                if (kgpc_getenv("KGPC_DEBUG_UNIT_SECTIONS") != NULL) {
                    fprintf(stderr, "[UNIT_SECTIONS] initialization at line %d\n", section->line);
                }
            } else if (section->typ == PASCAL_T_FINALIZATION_SECTION) {
                finalization_node = section;
            }
            section = section->next;
        }
        
        visited_set_destroy(visited_unit);

        g_interface_section_ast = interface_node;
        g_implementation_section_ast = implementation_node;

        if (interface_node != NULL && interface_node->typ == PASCAL_T_INTERFACE_SECTION) {
            /* Create visited set for interface sections */
            VisitedSet *visited_if = visited_set_create();
            if (visited_if == NULL) {
                fprintf(stderr, "ERROR: Failed to allocate visited set for interface sections\n");
            } else {
                ast_t *section = interface_node->child;
                while (section != NULL) {
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Visiting PROGRAM section type %d\n", section->typ);
            }
            /* Check for circular reference */
                    if (!is_safe_to_continue(visited_if, section)) {
                        section = section->next;
                        continue;
                    }
                    
                    ast_t *node_cursor = unwrap_pascal_node(section);
                    if (node_cursor != NULL) {
                        if (kgpc_getenv("KGPC_DEBUG_PROPERTY") != NULL) {
                            fprintf(stderr, "[KGPC] interface node typ=%d (%s)\n",
                                node_cursor->typ, pascal_tag_to_string(node_cursor->typ));
                        }
                        switch (node_cursor->typ) {
                        case PASCAL_T_USES_SECTION:
                            append_uses_from_section(node_cursor, &interface_uses);
                            break;
                        case PASCAL_T_TYPE_SECTION:
                            if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
                                fprintf(stderr, "[KGPC] interface TYPE_SECTION at line=%d\n", node_cursor->line);
                            }
                            interface_type_section_ast = node_cursor;  /* Save for const array enum resolution */
                            g_interface_type_section_ast = node_cursor;
                            enum_registry_scan_type_section(node_cursor);
                            append_type_decls_from_section(node_cursor, &interface_type_decls,
                                NULL, &interface_const_decls, &interface_var_builder, NULL);
                            break;
                        case PASCAL_T_CONST_SECTION:
                            append_const_decls_from_section(node_cursor, &interface_const_decls,
                                                            &interface_var_builder, interface_type_section_ast);
                            break;
                        case PASCAL_T_VAR_SECTION:
                            list_builder_extend(&interface_var_builder, convert_var_section(node_cursor));
                            break;
                        case PASCAL_T_PROCEDURE_DECL: {
                            Tree_t *proc = convert_procedure(node_cursor);
                            if (kgpc_debug_subprog_enabled()) {
                                char *proc_id = (node_cursor->child != NULL) ? dup_symbol(node_cursor->child) : strdup("?");
                                fprintf(stderr, "[KGPC] convert_procedure(%s) => %p\n", proc_id, (void*)proc);
                                free(proc_id);
                            }
                            if (proc != NULL) {
                                proc->tree_data.subprogram_data.unit_is_public = 1;  /* Interface procedure */
                            }
                            append_subprogram_node(&subprograms, proc);
                            break;
                        }
                        case PASCAL_T_FUNCTION_DECL: {
                            Tree_t *func = convert_function(node_cursor);
                            if (kgpc_debug_subprog_enabled()) {
                                char *func_id = (node_cursor->child != NULL) ? dup_symbol(node_cursor->child) : strdup("?");
                                fprintf(stderr, "[KGPC] convert_function(%s) => %p\n", func_id, (void*)func);
                                free(func_id);
                            }
                            if (func != NULL) {
                                func->tree_data.subprogram_data.unit_is_public = 1;  /* Interface function */
                            }
                            append_subprogram_node(&subprograms, func);
                            break;
                        }
                        case PASCAL_T_PROPERTY_DECL:
                            /* Module-level property declaration (FPC extension). */
                            append_module_property_wrappers(&subprograms, node_cursor);
                            break;
                        default:
                            break;
                        }
                    }
                    section = section->next;
                }
                visited_set_destroy(visited_if);
            }
        }

        if (interface_node != NULL)
        {
            VisitedSet *visited_subs = visited_set_create();
            if (visited_subs != NULL)
            {
                append_subprograms_from_ast_recursive(interface_node->child, &subprograms,
                    visited_subs);
                visited_set_destroy(visited_subs);
            }
        }

        if (implementation_node != NULL && implementation_node->typ == PASCAL_T_IMPLEMENTATION_SECTION) {
            /* Debug: count implementation section nodes */
            if (kgpc_getenv("KGPC_DEBUG_IMPL_SECTION") != NULL) {
                ast_t *dbg = implementation_node->child;
                int count = 0;
                int max_line = 0;
                int proc_count = 0, func_count = 0, method_count = 0, type_count = 0;
                while (dbg != NULL) {
                    count++;
                    if (dbg->line > max_line) max_line = dbg->line;
                    if (dbg->typ == PASCAL_T_PROCEDURE_DECL) proc_count++;
                    else if (dbg->typ == PASCAL_T_FUNCTION_DECL) func_count++;
                    else if (dbg->typ == PASCAL_T_METHOD_IMPL) method_count++;
                    else if (dbg->typ == PASCAL_T_TYPE_SECTION) type_count++;
                    dbg = dbg->next;
                }
                fprintf(stderr, "[IMPL_SECTION] child count=%d max_line=%d procs=%d funcs=%d methods=%d types=%d\n",
                        count, max_line, proc_count, func_count, method_count, type_count);
            }

            /* Create visited set for implementation sections */
            VisitedSet *visited_impl = visited_set_create();
            if (visited_impl == NULL) {
                fprintf(stderr, "ERROR: Failed to allocate visited set for implementation sections\n");
            } else {
                ast_t *definition = implementation_node->child;
                while (definition != NULL && definition != ast_nil) {
                    /* Check for circular reference */
                    if (!is_safe_to_continue(visited_impl, definition)) {
                        definition = definition->next;
                        continue;
                    }

                    if (kgpc_getenv("KGPC_DEBUG_IMPL_NONE") != NULL &&
                        definition->typ == PASCAL_T_NONE &&
                        definition->child == NULL &&
                        definition->sym != NULL &&
                        definition->sym->name != NULL) {
                        fprintf(stderr, "[KGPC] impl NONE at line=%d: %.120s\n",
                            definition->line, definition->sym->name);
                    }
                    ast_t *node_cursor = unwrap_pascal_node(definition);
                    if (node_cursor != NULL) {
                        if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
                            fprintf(stderr, "[KGPC] implementation section node typ=%d\n", node_cursor->typ);
                        }
                        switch (node_cursor->typ) {
                        case PASCAL_T_USES_SECTION:
                            append_uses_from_section(node_cursor, &implementation_uses);
                            break;
                        case PASCAL_T_TYPE_SECTION:
                            implementation_type_section_ast = node_cursor;  /* Save for const array enum resolution */
                            g_implementation_type_section_ast = node_cursor;
                            enum_registry_scan_type_section(node_cursor);
                            append_type_decls_from_section(node_cursor, &implementation_type_decls,
                                &subprograms, &implementation_const_decls, &implementation_var_builder, NULL);
                            break;
                        case PASCAL_T_CONST_SECTION:
                            append_const_decls_from_section(node_cursor, &implementation_const_decls,
                                                            &implementation_var_builder, implementation_type_section_ast);
                            break;
                        case PASCAL_T_VAR_SECTION:
                            list_builder_extend(&implementation_var_builder, convert_var_section(node_cursor));
                            break;
                        case PASCAL_T_PROCEDURE_DECL: {
                            Tree_t *proc = convert_procedure(node_cursor);
                            if (kgpc_debug_subprog_enabled() || kgpc_getenv("KGPC_DEBUG_IMPL_PROCS") != NULL) {
                                char *proc_id = (node_cursor->child != NULL) ? dup_symbol(node_cursor->child) : strdup("?");
                                fprintf(stderr, "[KGPC] impl convert_procedure(%s) line=%d => %p\n", proc_id, node_cursor->line, (void*)proc);
                                free(proc_id);
                            }
                            append_subprogram_node(&subprograms, proc);
                            break;
                        }
                        case PASCAL_T_FUNCTION_DECL: {
                            Tree_t *func = convert_function(node_cursor);
                            if (kgpc_debug_subprog_enabled() || kgpc_getenv("KGPC_DEBUG_IMPL_PROCS") != NULL) {
                                char *func_id = (node_cursor->child != NULL) ? dup_symbol(node_cursor->child) : strdup("?");
                                fprintf(stderr, "[KGPC] impl convert_function(%s) line=%d => %p\n", func_id, node_cursor->line, (void*)func);
                                free(func_id);
                            }
                            append_subprogram_node(&subprograms, func);
                            break;
                        }
                        case PASCAL_T_METHOD_IMPL:
                        case PASCAL_T_CONSTRUCTOR_DECL:
                        case PASCAL_T_DESTRUCTOR_DECL: {
                            Tree_t *method_tree = convert_method_impl(node_cursor);
                            append_subprogram_node(&subprograms, method_tree);
                            break;
                        }
                        default:
                            break;
                        }
                    }
                    definition = definition->next;
                }
                visited_set_destroy(visited_impl);
            }
        }

        if (implementation_node != NULL)
        {
            VisitedSet *visited_subs = visited_set_create();
            if (visited_subs != NULL)
            {
                append_subprograms_from_ast_recursive(implementation_node->child, &subprograms,
                    visited_subs);
                visited_set_destroy(visited_subs);
            }
        }

        {
            /* Final safety scan: walk the full unit AST to pick up any subprograms
             * that are not wired into the interface/implementation sibling chains. */
            VisitedSet *visited_subs = visited_set_create();
            if (visited_subs != NULL)
            {
                ast_t *scan_root = unit_scan_copy != NULL ? unit_scan_copy : cur;
                append_top_level_subprograms_from_ast(scan_root, &subprograms, visited_subs, 0);
                visited_set_destroy(visited_subs);
            }
        }

        if (initialization_node != NULL && initialization_node->typ == PASCAL_T_INITIALIZATION_SECTION) {
            /* The initialization section's child can be:
             * 1. A PASCAL_T_NONE wrapper from make_stmt_list_parser, with stmt_list as child
             * 2. The first statement directly (if the wrapper was optimized away)
             * We handle both cases by checking if child is PASCAL_T_NONE. */
            ast_t *stmt_list_seq = initialization_node->child;
            if (kgpc_getenv("KGPC_DEBUG_UNIT_INIT") != NULL) {
                fprintf(stderr, "[KGPC] initialization_node: typ=%d line=%d\n", 
                        initialization_node->typ, initialization_node->line);
                if (stmt_list_seq != NULL) {
                    fprintf(stderr, "[KGPC]   stmt_list_seq: typ=%d line=%d\n", 
                            stmt_list_seq->typ, stmt_list_seq->line);
                }
            }
            if (stmt_list_seq != NULL) {
                ListNode_t *stmts = NULL;
                if (stmt_list_seq->typ == PASCAL_T_NONE) {
                    /* Wrapped structure: NONE -> child is stmt list */
                    if (stmt_list_seq->child != NULL) {
                        stmts = convert_statement_list(stmt_list_seq->child);
                    }
                } else {
                    /* Direct structure: child IS the first statement in a linked list */
                    stmts = convert_statement_list(stmt_list_seq);
                }
                initialization = mk_compoundstatement(initialization_node->line, stmts);
            }
        }

        if (finalization_node != NULL && finalization_node->typ == PASCAL_T_FINALIZATION_SECTION) {
            /* Same structure handling as initialization */
            ast_t *stmt_list_seq = finalization_node->child;
            if (stmt_list_seq != NULL) {
                ListNode_t *stmts = NULL;
                if (stmt_list_seq->typ == PASCAL_T_NONE) {
                    if (stmt_list_seq->child != NULL) {
                        stmts = convert_statement_list(stmt_list_seq->child);
                    }
                } else {
                    stmts = convert_statement_list(stmt_list_seq);
                }
                finalization = mk_compoundstatement(finalization_node->line, stmts);
            }
        }

        Tree_t *tree = mk_unit(cur->line, unit_id, interface_uses,
                               interface_const_decls, interface_type_decls,
                               list_builder_finish(&interface_var_builder), implementation_uses,
                               implementation_const_decls,
                               implementation_type_decls, list_builder_finish(&implementation_var_builder),
                               subprograms, initialization, finalization);
        if (unit_scan_copy != NULL)
            free_ast(unit_scan_copy);
        /* Clear borrowed AST pointers before the caller frees the raw AST. */
        g_interface_type_section_ast = NULL;
        g_implementation_type_section_ast = NULL;
        return tree;
    }

    fprintf(stderr, "ERROR: Unsupported Pascal AST root type %d.\n", cur->typ);
    return NULL;
}

/* Get the method template for a class method.
 * This is used to copy default parameter values from class declarations to implementations.
 * For overloaded methods, we need to match by parameter count.
 */
struct MethodTemplate *from_cparser_get_method_template(struct RecordType *record, const char *method_name)
{
    if (record == NULL || method_name == NULL)
        return NULL;
    
    ListNode_t *cur = record->method_templates;
    struct MethodTemplate *fallback_template = NULL;
    
    while (cur != NULL)
    {
        struct MethodTemplate *template = (struct MethodTemplate *)cur->cur;
        if (template != NULL && template->name != NULL &&
            strcasecmp(template->name, method_name) == 0)
        {
            /* For overloaded methods, prefer templates that have default values */
            if (template->params_ast != NULL)
            {
                /* Check if this template has default values */
                ast_t *param = template->params_ast;
                if (param->typ == PASCAL_T_PARAM_LIST)
                    param = param->child;
                
                while (param != NULL)
                {
                    if (param->typ == PASCAL_T_PARAM)
                    {
                        for (ast_t *child = param->child; child != NULL; child = child->next)
                        {
                            if (child->typ == PASCAL_T_DEFAULT_VALUE)
                                return template;  /* Found one with defaults */
                        }
                    }
                    param = param->next;
                }
            }
            
            /* Remember first matching template as fallback */
            if (fallback_template == NULL)
                fallback_template = template;
        }
        cur = cur->next;
    }
    
    /* Return fallback if no template with defaults found */
    return fallback_template;
}

/* Public wrapper for convert_expression */
struct Expression *from_cparser_convert_expression(ast_t *expr_node)
{
    return convert_expression(expr_node);
}
