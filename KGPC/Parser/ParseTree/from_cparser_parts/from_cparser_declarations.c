#include "../from_cparser_internal.h"

static Tree_t *convert_var_decl(ast_t *decl_node) {
    ast_t *cur = decl_node->child;
    ast_t *first_non_identifier = cur;
    while (first_non_identifier != NULL && first_non_identifier->typ == PASCAL_T_IDENTIFIER)
        first_non_identifier = first_non_identifier->next;

    ListNode_t *ids = convert_identifier_list(&cur);
    if (ids == NULL) {
        fprintf(stderr, "ERROR: variable declaration missing identifier list.\n");
        return NULL;
    }
    char *type_id = NULL;
    int var_type = UNKNOWN_TYPE;
    TypeInfo type_info = {0};
    ast_t *type_node = first_non_identifier;
    if (type_node != NULL && type_node->typ == PASCAL_T_TYPE_SPEC) {
        var_type = convert_type_spec(type_node, &type_id, NULL, &type_info);
        cur = type_node->next;
    } else if (type_node != NULL && type_node->typ == PASCAL_T_IDENTIFIER && type_node->next == NULL) {
        char *type_name = dup_symbol(type_node);
        if (type_name != NULL) {
            var_type = map_type_name(type_name, &type_id);
            var_type = apply_shortstring_mode(var_type, type_name);
            if (var_type == UNKNOWN_TYPE && type_id == NULL)
                type_id = type_name;
            else
                free(type_name);
        }
        cur = type_node->next;
    } else {
        cur = type_node;
    }

    if (var_type == UNKNOWN_TYPE && (type_node == NULL || var_type == UNKNOWN_TYPE)) {
        ast_t *search = decl_node->child;
        while (search != NULL && search->typ == PASCAL_T_IDENTIFIER)
            search = search->next;
        if (search != NULL && search->typ == PASCAL_T_TYPE_SPEC) {
            if (type_id != NULL) {
                free(type_id);
                type_id = NULL;
            }
            /* Free any type_info from the first convert_type_spec call,
             * since convert_type_spec resets all fields to NULL without freeing */
            destroy_type_info_contents(&type_info);
            var_type = convert_type_spec(search, &type_id, NULL, &type_info);
        } else if (search != NULL && search->typ == PASCAL_T_IDENTIFIER) {
            char *type_name = dup_symbol(search);
            if (type_name != NULL) {
                int mapped = map_type_name(type_name, &type_id);
                mapped = apply_shortstring_mode(mapped, type_name);
                if (mapped == UNKNOWN_TYPE && type_id == NULL)
                    type_id = type_name;
                else
                    free(type_name);
                var_type = mapped;
            }
        }
    }

    /* Handle inline procedure/function types for variables */
    KgpcType *inline_proc_type = NULL;
    if (var_type == PROCEDURE) {
        /* Find the procedure type specification node */
        ast_t *search = decl_node->child;
        while (search != NULL && search->typ == PASCAL_T_IDENTIFIER)
            search = search->next;
        if (search != NULL) {
            ast_t *spec_node = search;
            if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
                spec_node = spec_node->child;
            spec_node = unwrap_pascal_node(spec_node);
            if (spec_node != NULL &&
                (spec_node->typ == PASCAL_T_PROCEDURE_TYPE ||
                 spec_node->typ == PASCAL_T_FUNCTION_TYPE ||
                 spec_node->typ == PASCAL_T_REFERENCE_TO_TYPE))
            {
                inline_proc_type = convert_type_spec_to_kgpctype(search, NULL);
            }
        }
    }

    if (type_info.is_array) {
        int element_type = type_info.element_type;
        char *element_type_id = type_info.element_type_id;
        struct RecordType *inline_record = type_info.record_type;
        if (inline_record != NULL)
            type_info.record_type = NULL;
        
        /* Handle optional initializer for arrays */
        struct Statement *initializer_stmt = NULL;
        if (cur != NULL) {
            /* Handle optional initializer wrapper: optional(seq(...)) creates PASCAL_T_NONE node */
            ast_t *init_node = cur;
            if (init_node->typ == PASCAL_T_NONE && init_node->child != NULL) {
                if (absolute_clause_target(init_node) != NULL || is_var_hint_clause(init_node)) {
                    init_node = NULL;
                } else {
                /* The wrapper contains "=" token followed by the expression.
                 * Skip to find the actual expression node (not the "=" token) */
                ast_t *child = init_node->child;
                if (child != NULL && child->next != NULL) {
                    /* Skip the first child (the "=" token) and use the second */
                    init_node = child->next;
                }
                }
            }

            /* Skip nodes that should not be treated as initializers */
            if (is_node_to_skip_as_initializer(init_node)) {
                init_node = NULL;
            }
            
            if (init_node != NULL && ids != NULL && ids->next == NULL) {
                char *var_name = (char *)ids->cur;
                
                /* Check if initializer is a tuple (array literal) */
                if (init_node->typ == PASCAL_T_TUPLE) {
                    /* Create multiple assignment statements for array elements */
                    ListBuilder stmt_builder;
                    list_builder_init(&stmt_builder);
                    
                    int index = type_info.start;
                    for (ast_t *elem = init_node->child; elem != NULL; elem = elem->next) {
                        struct Expression *elem_expr = convert_expression(elem);
                        if (elem_expr != NULL) {
                            struct Expression *index_expr = mk_inum(decl_node->line, index);
                            struct Expression *base_expr = mk_varid(decl_node->line, strdup(var_name));
                            struct Expression *array_access = mk_arrayaccess(decl_node->line, base_expr, index_expr);
                            struct Statement *assign_stmt = mk_varassign(decl_node->line, decl_node->col, array_access, elem_expr);
                            list_builder_append(&stmt_builder, assign_stmt, LIST_STMT);
                        }
                        index++;
                    }
                    
                    /* Create compound statement from all assignments */
                    ListNode_t *stmt_list = list_builder_finish(&stmt_builder);
                    if (stmt_list != NULL) {
                        initializer_stmt = mk_compoundstatement(decl_node->line, stmt_list);
                    }
                } else {
                    /* Single expression initializer (not a tuple) */
                    struct Expression *init_expr = convert_expression(init_node);
                    if (init_expr != NULL) {
                        struct Expression *lhs = mk_varid(decl_node->line, strdup(var_name));
                        initializer_stmt = mk_varassign(decl_node->line, decl_node->col, lhs, init_expr);
                    }
                }
            }
        }
        
        /* Extract the range string from array_dimensions if available */
        char *range_str = NULL;
        if (type_info.array_dimensions != NULL && type_info.array_dimensions->cur != NULL) {
            range_str = strdup((char *)type_info.array_dimensions->cur);
        }
        
        Tree_t *decl = mk_arraydecl(decl_node->line, ids, element_type, element_type_id,
                                    type_info.start, type_info.end, range_str, initializer_stmt,
                                    inline_record);
        if (decl != NULL)
            decl->tree_data.arr_decl_data.is_shortstring = type_info.is_shortstring;
        if (decl != NULL)
            decl->tree_data.arr_decl_data.type_ref =
                type_ref_from_element_info(&type_info, element_type_id);
        if (decl != NULL && type_info.unresolved_index_type != NULL) {
            decl->tree_data.arr_decl_data.unresolved_index_type = type_info.unresolved_index_type;
            type_info.unresolved_index_type = NULL;  /* ownership transferred */
        }
        /* Transfer array_dimensions for multi-dim linearization */
        if (decl != NULL && type_info.array_dimensions != NULL &&
            type_info.array_dimensions->next != NULL) {
            decl->tree_data.arr_decl_data.array_dimensions = type_info.array_dimensions;
            type_info.array_dimensions = NULL;
        }
        /* For single-dimension enum-indexed arrays (e.g., array[TColor] of ...),
         * the bounds are 0,0 because the enum wasn't resolved at parse time.
         * Transfer the dimension name as unresolved_index_type so semcheck can resolve it. */
        if (decl != NULL && decl->tree_data.arr_decl_data.unresolved_index_type == NULL &&
            type_info.start == 0 && type_info.end == 0 &&
            type_info.array_dimensions != NULL &&
            type_info.array_dimensions->type == LIST_STRING &&
            type_info.array_dimensions->cur != NULL)
        {
            const char *dim_str = (const char *)type_info.array_dimensions->cur;
            /* Only if it's a single identifier (no "..") */
            if (strstr(dim_str, "..") == NULL) {
                decl->tree_data.arr_decl_data.unresolved_index_type = strdup(dim_str);
            }
        }
        type_info.element_type_id = NULL;
        destroy_type_info_contents(&type_info);
        if (type_id != NULL)
            free(type_id);
        return decl;
    }

    if (type_info.is_pointer) {
        if (type_info.pointer_type_id != NULL && type_id == NULL)
            type_id = strdup(type_info.pointer_type_id);
        var_type = POINTER_TYPE;
    } else if (type_info.is_set) {
        if (type_info.set_element_type_id != NULL && type_id == NULL)
            type_id = strdup(type_info.set_element_type_id);
        var_type = SET_TYPE;
    } else if (type_info.is_enum) {
        var_type = ENUM_TYPE;
    } else if (type_info.is_file) {
        var_type = FILE_TYPE;
    } else if (type_info.is_record) {
        var_type = RECORD_TYPE;
    }

    int inferred = 0;
    struct Statement *initializer_stmt = NULL;
    if (cur != NULL) {
        /* Handle optional initializer wrapper: optional(seq(...)) creates PASCAL_T_NONE node */
        ast_t *init_node = cur;
        if (init_node->typ == PASCAL_T_NONE && init_node->child != NULL) {
            if (absolute_clause_target(init_node) != NULL || is_var_hint_clause(init_node)) {
                init_node = NULL;
            } else {
            /* The wrapper contains "=" token followed by the expression.
             * Skip to find the actual expression node (not the "=" token) */
            ast_t *child = init_node->child;
            if (child != NULL && child->next != NULL) {
                /* Skip the first child (the "=" token) and use the second */
                init_node = child->next;
            }
            }
        }

        /* Skip nodes that should not be treated as initializers */
        if (is_node_to_skip_as_initializer(init_node)) {
            init_node = NULL;
        }
        
        /* Skip EXTERNAL_NAME and PUBLIC_NAME modifiers - they're handled later */
        if (init_node != NULL && 
            init_node->typ != PASCAL_T_EXTERNAL_NAME && 
            init_node->typ != PASCAL_T_EXTERNAL_NAME_EXPR &&
            init_node->typ != PASCAL_T_PUBLIC_NAME) {
            struct Expression *init_expr = convert_expression(init_node);
            if (init_expr != NULL) {
                if (ids != NULL && ids->next == NULL) {
                    inferred = (var_type == UNKNOWN_TYPE && type_id == NULL) ? 1 : 0;
                    char *var_name = (char *)ids->cur;
                    struct Expression *lhs = mk_varid(decl_node->line, strdup(var_name));
                    initializer_stmt = mk_varassign(decl_node->line, decl_node->col, lhs, init_expr);
                } else {
                    destroy_expr(init_expr);
                }
            }
        }
    }

    if (var_type == UNKNOWN_TYPE && ids != NULL && ids->next != NULL && type_node == NULL) {
        ListNode_t *prev = NULL;
        ListNode_t *iter = ids;
        while (iter->next != NULL) {
            prev = iter;
            iter = iter->next;
        }
        if (prev != NULL && iter != NULL && iter->type == LIST_STRING) {
            char *type_name = strdup((char *)iter->cur);
            if (type_name != NULL) {
                int mapped = map_type_name(type_name, &type_id);
                mapped = apply_shortstring_mode(mapped, type_name);
                if (mapped == UNKNOWN_TYPE && type_id == NULL)
                    type_id = type_name;
                else
                    free(type_name);
                var_type = mapped;
            }
            prev->next = NULL;
            free(iter->cur);
            free(iter);
        }
    }

    /* Transfer ownership of inline record type from type_info to the vardecl */
    struct RecordType *inline_record = NULL;
    if (type_info.is_record && type_info.record_type != NULL) {
        inline_record = type_info.record_type;
        type_info.record_type = NULL;  /* Transfer ownership */
    }

    struct TypeAlias *inline_alias = NULL;
    if (type_info.is_range)
    {
        inline_alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
        if (inline_alias != NULL)
        {
            inline_alias->is_range = 1;
            inline_alias->range_known = type_info.range_known;
            inline_alias->range_start = type_info.range_start;
            inline_alias->range_end = type_info.range_end;
            inline_alias->base_type = select_range_primitive_tag(&type_info);
            inline_alias->storage_size = compute_range_storage_size(&type_info);
            if (var_type == UNKNOWN_TYPE)
                var_type = inline_alias->base_type;
        }
    }
    if (inline_alias == NULL && type_info.is_file && type_id == NULL)
    {
        inline_alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
        if (inline_alias != NULL)
        {
            inline_alias->is_file = 1;
            inline_alias->file_type = type_info.file_type;
            inline_alias->base_type = FILE_TYPE;
            if (type_info.file_type_id != NULL)
                inline_alias->file_type_id = strdup(type_info.file_type_id);
            inline_alias->file_type_ref =
                type_ref_from_single_name(type_info.file_type_id);
        }
    }
    if (inline_alias == NULL && type_info.is_set)
    {
        inline_alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
        if (inline_alias != NULL)
        {
            inline_alias->is_set = 1;
            inline_alias->set_element_type = type_info.set_element_type;
            inline_alias->base_type = SET_TYPE;
            if (type_info.set_element_type_id != NULL)
                inline_alias->set_element_type_id = strdup(type_info.set_element_type_id);
            inline_alias->set_element_type_ref =
                type_ref_from_single_name(type_info.set_element_type_id);
        }
    }
    if (inline_alias == NULL && type_info.is_enum)
    {
        inline_alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
        if (inline_alias != NULL)
        {
            inline_alias->is_enum = 1;
            inline_alias->base_type = ENUM_TYPE;
            inline_alias->enum_is_scoped = type_info.enum_is_scoped;
            inline_alias->enum_has_explicit_values = type_info.enum_has_explicit_values;
            inline_alias->enum_literals = type_info.enum_literals;
            type_info.enum_literals = NULL;
        }
    }

    /* Scan for external/public name modifiers (FPC bootstrap compatibility) */
    char *cname_override = NULL;
    int is_external = 0;
    ast_t *scan = decl_node->child;
    while (scan != NULL) {
        if (scan->typ == PASCAL_T_EXTERNAL_NAME) {
            /* External name: variable is defined externally, use specified symbol */
            if (scan->child != NULL && scan->child->typ == PASCAL_T_STRING) {
                if (cname_override != NULL)
                    free(cname_override);
                cname_override = dup_symbol(scan->child);
            } else if (scan->child != NULL) {
                /* Child may be EXTERNAL_NAME_EXPR wrapping the string */
                char *name = extract_external_name_from_node(scan);
                if (name != NULL) {
                    if (cname_override != NULL)
                        free(cname_override);
                    cname_override = name;
                }
            }
            is_external = 1;
        } else if (scan->typ == PASCAL_T_EXTERNAL_NAME_EXPR) {
            char *name = extract_external_name_from_node(scan);
            if (name != NULL) {
                if (cname_override != NULL)
                    free(cname_override);
                cname_override = name;
            }
            is_external = 1;
        } else if (scan->typ == PASCAL_T_PUBLIC_NAME) {
            /* Public name: variable is exported with specified symbol.
             * The parser wraps the string in EXTERNAL_NAME_EXPR, so
             * structure is: PUBLIC_NAME -> EXTERNAL_NAME_EXPR -> STRING */
            char *name = extract_external_name_from_node(scan);
            if (name != NULL) {
                if (cname_override != NULL)
                    free(cname_override);
                cname_override = name;
            } else if (scan->child != NULL && scan->child->typ == PASCAL_T_STRING) {
                if (cname_override != NULL)
                    free(cname_override);
                cname_override = dup_symbol(scan->child);
            }
            /* is_external = 0 for public; variable is defined here but exported */
        }
        scan = scan->next;
    }

    char *absolute_target = NULL;
    {
        ast_t *abs_node = decl_node->child;
        if (kgpc_getenv("KGPC_DEBUG_ABSOLUTE") != NULL) {
            int idx = 0;
            for (ast_t *dbg = abs_node; dbg != NULL; dbg = dbg->next) {
                fprintf(stderr, "[KGPC] absolute scan %d: typ=%d sym=%s\n",
                    idx++, dbg->typ,
                    (dbg->sym != NULL && dbg->sym->name != NULL) ? dbg->sym->name : "<null>");
            }
        }
        while (abs_node != NULL) {
            ast_t *target = absolute_clause_target(abs_node);
            if (target != NULL) {
                absolute_target = dup_symbol(target);
                break;
            }
            abs_node = abs_node->next;
        }
    }
    if (kgpc_debug_decl_scan_enabled() && absolute_target != NULL && ids != NULL && ids->type == LIST_STRING) {
        fprintf(stderr, "[KGPC] var absolute: %s -> %s\n",
            (char *)ids->cur, absolute_target);
    }

    Tree_t *decl = mk_vardecl(decl_node->line, ids, var_type, type_id, 0,
        inferred, initializer_stmt, inline_record, inline_alias, absolute_target);
    if (decl == NULL && absolute_target != NULL)
        free(absolute_target);

    if (decl != NULL)
    {
        decl->tree_data.var_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);
        if (absolute_target != NULL)
        {
            char *abs_base = NULL;
            char *abs_field = NULL;
            if (split_absolute_target(absolute_target, &abs_base, &abs_field))
            {
                decl->tree_data.var_decl_data.absolute_base_id = abs_base;
                decl->tree_data.var_decl_data.absolute_field_id = abs_field;
            }
            else
            {
                free(abs_base);
                free(abs_field);
            }
        }
    }
    
    /* Store inline procedure type in cached_kgpc_type */
    if (decl != NULL && inline_proc_type != NULL) {
        decl->tree_data.var_decl_data.cached_kgpc_type = inline_proc_type;
        inline_proc_type = NULL;  /* Transfer ownership */
    } else if (inline_proc_type != NULL) {
        kgpc_type_release(inline_proc_type);
    }
    
    /* Apply external/public name override */
    if (decl != NULL && cname_override != NULL) {
        decl->tree_data.var_decl_data.cname_override = cname_override;
        decl->tree_data.var_decl_data.is_external = is_external;
    } else if (cname_override != NULL) {
        free(cname_override);
    }

    destroy_type_info_contents(&type_info);

    return decl;
}

ListNode_t *convert_var_section(ast_t *section_node) {
    ListBuilder decls_builder;
    list_builder_init(&decls_builder);
    
    /* Create visited set to detect circular references */
    VisitedSet *visited = visited_set_create();
    if (visited == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate visited set for var section traversal\n");
        return NULL;
    }
    
    ast_t *cur = section_node->child;
    while (cur != NULL && cur->typ == PASCAL_T_VAR_DECL) {
        /* Check for circular reference before processing */
        if (!is_safe_to_continue(visited, cur)) {
            fprintf(stderr, "ERROR: Circular reference detected in var section, stopping traversal\n");
            break;
        }
        if (kgpc_debug_decl_scan_enabled()) {
            ast_t *id_node = cur->child;
            if (id_node != NULL && id_node->sym != NULL) {
                fprintf(stderr, "[KGPC] var decl: %s\n", id_node->sym->name);
            }
        }

        Tree_t *decl = convert_var_decl(cur);
        if (decl != NULL)
            list_builder_append(&decls_builder, decl, LIST_TREE);
        cur = cur->next;
    }

    visited_set_destroy(visited);
    return list_builder_finish(&decls_builder);
}

static int lower_const_array(ast_t *const_decl_node, char **id_ptr, TypeInfo *type_info,
                             ast_t *value_node, ListBuilder *var_builder, ast_t *type_section, ast_t *const_section) {
    if (id_ptr == NULL || *id_ptr == NULL || type_info == NULL)
        return -1;

    if (var_builder == NULL) {
        fprintf(stderr,
                "ERROR: Cannot lower const array %s without a variable declaration list.\n",
                *id_ptr);
        return -1;
    }

    /* For multi-dimensional arrays like array[a..b, c..d], we handle them
     * by treating the second dimension as an inner array. The outer dimension
     * determines the number of rows, and the inner dimension determines columns.
     * We support 2D and 3D arrays (one or two levels of nesting). */
    int is_multidim = (type_info->array_dimensions != NULL && type_info->array_dimensions->next != NULL);
    int is_3d = 0;
    int is_4d = 0;
    int multidim_inner_start = 0, multidim_inner_end = -1;
    int multidim_infer_bounds = 0;  /* Set to 1 if we need to infer bounds from initializer */
    int dim3_start = 0, dim3_end = -1;
    int dim3_infer_bounds = 0;
    int dim4_start = 0, dim4_end = -1;
    int dim4_infer_bounds = 0;
    if (is_multidim) {
        /* Check for 3 dimensions */
        if (type_info->array_dimensions->next->next != NULL) {
            is_3d = 1;
            /* Check for 4 dimensions */
            if (type_info->array_dimensions->next->next->next != NULL) {
                is_4d = 1;
                /* Check for more than 4 dimensions - not yet supported */
                if (type_info->array_dimensions->next->next->next->next != NULL) {
                    fprintf(stderr, "ERROR: Unsupported 5+ dimensional const array %s.\n", *id_ptr);
                    return -1;
                }
            }
            /* Extract 2nd dimension bounds */
            const char *dim2_range = (const char *)type_info->array_dimensions->next->cur;
            if (dim2_range != NULL) {
                char *range_copy = strdup(dim2_range);
                if (range_copy != NULL) {
                    char *dotdot = strstr(range_copy, "..");
                    if (dotdot != NULL) {
                        *dotdot = '\0';
                        multidim_inner_start = parse_range_bound(range_copy);
                        multidim_inner_end = parse_range_bound(dotdot + 2);
                        if (multidim_inner_start == 0 && multidim_inner_end == 0) {
                            int has_alpha = 0;
                            for (const char *p = range_copy; *p; ++p)
                                if (isalpha((unsigned char)*p)) { has_alpha = 1; break; }
                            if (has_alpha) {
                                int s = resolve_enum_ordinal_from_ast(range_copy, type_section);
                                int e = resolve_enum_ordinal_from_ast(dotdot + 2, type_section);
                                if (s >= 0 && e >= 0) {
                                    multidim_inner_start = s;
                                    multidim_inner_end = e;
                                } else {
                                    multidim_infer_bounds = 1;
                                }
                            }
                        }
                    } else {
                        multidim_infer_bounds = 1;
                    }
                    free(range_copy);
                }
            }
            /* Extract 3rd dimension bounds */
            const char *dim3_range = (const char *)type_info->array_dimensions->next->next->cur;
            if (dim3_range != NULL) {
                char *range_copy = strdup(dim3_range);
                if (range_copy != NULL) {
                    char *dotdot = strstr(range_copy, "..");
                    if (dotdot != NULL) {
                        *dotdot = '\0';
                        dim3_start = parse_range_bound(range_copy);
                        dim3_end = parse_range_bound(dotdot + 2);
                        if (dim3_start == 0 && dim3_end == 0) {
                            int has_alpha = 0;
                            for (const char *p = range_copy; *p; ++p)
                                if (isalpha((unsigned char)*p)) { has_alpha = 1; break; }
                            if (has_alpha) {
                                int s = resolve_enum_ordinal_from_ast(range_copy, type_section);
                                int e = resolve_enum_ordinal_from_ast(dotdot + 2, type_section);
                                if (s >= 0 && e >= 0) {
                                    dim3_start = s;
                                    dim3_end = e;
                                } else {
                                    dim3_infer_bounds = 1;
                                }
                            }
                        }
                    } else {
                        dim3_infer_bounds = 1;
                    }
                    free(range_copy);
                }
            }
            /* Extract 4th dimension bounds */
            if (is_4d) {
                const char *dim4_range = (const char *)type_info->array_dimensions->next->next->next->cur;
                if (dim4_range != NULL) {
                    char *range_copy = strdup(dim4_range);
                    if (range_copy != NULL) {
                        char *dotdot = strstr(range_copy, "..");
                        if (dotdot != NULL) {
                            *dotdot = '\0';
                            dim4_start = parse_range_bound(range_copy);
                            dim4_end = parse_range_bound(dotdot + 2);
                            if (dim4_start == 0 && dim4_end == 0) {
                                int has_alpha = 0;
                                for (const char *p = range_copy; *p; ++p)
                                    if (isalpha((unsigned char)*p)) { has_alpha = 1; break; }
                                if (has_alpha) {
                                    int s = resolve_enum_ordinal_from_ast(range_copy, type_section);
                                    int e = resolve_enum_ordinal_from_ast(dotdot + 2, type_section);
                                    if (s >= 0 && e >= 0) {
                                        dim4_start = s;
                                        dim4_end = e;
                                    } else {
                                        dim4_infer_bounds = 1;
                                    }
                                }
                            }
                        } else {
                            dim4_infer_bounds = 1;
                        }
                        free(range_copy);
                    }
                }
            }
        } else {
            /* Extract inner dimension bounds from the second dimension string */
            const char *inner_range = (const char *)type_info->array_dimensions->next->cur;
            if (inner_range != NULL) {
                /* Parse range like "1..240" or "0..3" */
                char *range_copy = strdup(inner_range);
                if (range_copy != NULL) {
                    char *dotdot = strstr(range_copy, "..");
                    if (dotdot != NULL) {
                        *dotdot = '\0';
                        multidim_inner_start = parse_range_bound(range_copy);
                        multidim_inner_end = parse_range_bound(dotdot + 2);
                        /* Enum subrange like OS_F32..OS_F128 parses as 0..0;
                         * try resolving enum ordinals first */
                        if (multidim_inner_start == 0 && multidim_inner_end == 0) {
                            int has_alpha = 0;
                            for (const char *p = range_copy; *p; ++p)
                                if (isalpha((unsigned char)*p)) { has_alpha = 1; break; }
                            if (has_alpha) {
                                int s = resolve_enum_ordinal_from_ast(range_copy, type_section);
                                int e = resolve_enum_ordinal_from_ast(dotdot + 2, type_section);
                                if (s >= 0 && e >= 0) {
                                    multidim_inner_start = s;
                                    multidim_inner_end = e;
                                } else {
                                    multidim_infer_bounds = 1;
                                }
                            }
                        }
                    } else {
                        /* Enum type - need to infer bounds from initializer */
                        multidim_infer_bounds = 1;
                    }
                    free(range_copy);
                }
            }
        }
    }

    if (type_info->is_open_array) {
        /* Open array typed constants: count initializer elements and convert
         * to a static array[0..N-1].  This matches FPC behaviour where
         * "const V: array of Integer = (1,2,3)" is laid out as a fixed-size
         * array whose bounds are inferred from the initializer. */
        ast_t *open_tuple = value_node;
        if (open_tuple != NULL) {
            ast_t *uw = unwrap_pascal_node(open_tuple);
            if (uw != NULL &&
                (uw->typ == PASCAL_T_TYPE_SPEC ||
                 uw->typ == PASCAL_T_ARRAY_TYPE ||
                 uw->typ == PASCAL_T_RECORD_TYPE ||
                 uw->typ == PASCAL_T_POINTER_TYPE ||
                 uw->typ == PASCAL_T_PROCEDURE_TYPE ||
                 uw->typ == PASCAL_T_FUNCTION_TYPE) &&
                uw->next != NULL) {
                open_tuple = unwrap_pascal_node(uw->next);
            } else {
                open_tuple = uw;
            }
        }
        int elem_count = 0;
        if (open_tuple == NULL) {
            fprintf(stderr, "ERROR: Open array typed const %s has no elements.\n", *id_ptr);
            return -1;
        } else if (open_tuple->typ == PASCAL_T_TUPLE) {
            for (ast_t *e = open_tuple->child; e != NULL; e = e->next)
                ++elem_count;
        } else {
            /* Single-element parenthesized initializers like "(42)" are parsed
             * as parenthesized expressions rather than one-element tuples. The
             * normal tuple-wrapping path below will canonicalize the AST. */
            elem_count = 1;
        }
        if (elem_count == 0) {
            fprintf(stderr, "ERROR: Open array typed const %s has no elements.\n", *id_ptr);
            return -1;
        }
        type_info->start = 0;
        type_info->end = elem_count - 1;
        type_info->is_open_array = 0;
    }

    ast_t *tuple_node = value_node;
    int is_string_initializer = 0;
    const char *string_initializer = NULL;
    AstStringValue owned_string_initializer = {0};
    if (tuple_node != NULL) {
        ast_t *unwrapped = unwrap_pascal_node(tuple_node);
        if (unwrapped != NULL &&
            (unwrapped->typ == PASCAL_T_TYPE_SPEC ||
             unwrapped->typ == PASCAL_T_ARRAY_TYPE ||
             unwrapped->typ == PASCAL_T_RECORD_TYPE ||
             unwrapped->typ == PASCAL_T_POINTER_TYPE ||
             unwrapped->typ == PASCAL_T_PROCEDURE_TYPE ||
             unwrapped->typ == PASCAL_T_FUNCTION_TYPE) &&
            unwrapped->next != NULL) {
            tuple_node = unwrap_pascal_node(unwrapped->next);
        } else {
            tuple_node = unwrapped;
        }
    }
    if (tuple_node == NULL) {
        fprintf(stderr, "ERROR: Const array %s must use tuple syntax for its initializer.\n",
                *id_ptr);
        return -1;
    }
    int single_record_element = 0;
    int is_widechar_array_target = 0;
    int is_char_array_target = type_info_targets_char_array(type_info, &is_widechar_array_target);
    if (tuple_node->typ == PASCAL_T_STRING) {
        if (is_char_array_target || is_widechar_array_target) {
            is_string_initializer = 1;
            if (tuple_node->sym != NULL && tuple_node->sym->name != NULL)
                string_initializer = tuple_node->sym->name;
        } else {
            fprintf(stderr, "ERROR: Const array %s string initializer requires a char array type.\n",
                    *id_ptr);
            return -1;
        }
    } else if (tuple_node->typ == PASCAL_T_IDENTIFIER) {
        if (!is_char_array_target && !is_widechar_array_target) {
            fprintf(stderr, "ERROR: Const array %s string initializer requires a char array type.\n",
                    *id_ptr);
            return -1;
        }
        if (tuple_node->sym != NULL && tuple_node->sym->name != NULL) {
            if (resolve_const_string_from_ast_internal(tuple_node->sym->name, const_section,
                                                       &string_initializer, 0) == 0 &&
                string_initializer != NULL) {
                is_string_initializer = 1;
            } else {
                fprintf(stderr, "ERROR: Const array %s must use tuple syntax for its initializer.\n",
                        *id_ptr);
                return -1;
            }
        } else {
            fprintf(stderr, "ERROR: Const array %s must use tuple syntax for its initializer.\n",
                    *id_ptr);
            return -1;
        }
    } else if (tuple_node->typ == PASCAL_T_RECORD_CONSTRUCTOR) {
        single_record_element = 1;
    } else if (is_char_array_target || is_widechar_array_target) {
        if (evaluate_const_string_ast(tuple_node, const_section, &owned_string_initializer, 0) == 0) {
            is_string_initializer = 1;
        } else if (tuple_node->typ != PASCAL_T_TUPLE) {
            fprintf(stderr, "ERROR: Const array %s must use tuple syntax for its initializer.\n",
                    *id_ptr);
            ast_string_value_reset(&owned_string_initializer);
            return -1;
        }
    } else if (tuple_node->typ != PASCAL_T_TUPLE) {
        /* Single-element parenthesized initializer: (nil), (0), (expr), etc.
         * The parser parses (expr) as a parenthesized expression rather than
         * a 1-element tuple, so wrap the value into a synthetic TUPLE node. */
        ast_t *wrapper = new_ast();
        wrapper->typ = PASCAL_T_TUPLE;
        wrapper->child = tuple_node;
        wrapper->next = NULL;
        /* Detach from any sibling chain so iteration sees exactly 1 element */
        ast_t *saved_next = tuple_node->next;
        tuple_node->next = NULL;
        tuple_node = wrapper;
        /* Note: wrapper is a small allocation; it is not freed here because
         * the AST is freed in bulk after translation.  saved_next is unused
         * since single-element array consts only have one value. */
        (void)saved_next;
    }

    int start = type_info->start;
    int end = type_info->end;
    
    resolve_array_bounds(type_info, type_section, const_section, *id_ptr);
    start = type_info->start;
    end = type_info->end;
    
    int expected_count = -1;
    if (end >= start)
        expected_count = end - start + 1;

    while (!single_record_element &&
           expected_count != 1 &&
           tuple_node != NULL &&
           tuple_node->typ == PASCAL_T_TUPLE &&
           tuple_node->child != NULL &&
           tuple_node->child->next == NULL) {
        ast_t *nested_tuple = unwrap_pascal_node(tuple_node->child);
        if (nested_tuple == NULL ||
            nested_tuple->typ != PASCAL_T_TUPLE ||
            tuple_is_record_constructor(nested_tuple)) {
            break;
        }
        tuple_node = nested_tuple;
    }

    int actual_count = 0;
    int is_shortstring_target = (type_info->is_shortstring != 0);
    if (is_string_initializer) {
        if (owned_string_initializer.data != NULL || owned_string_initializer.len > 0)
            actual_count = (int)owned_string_initializer.len;
        else if (string_initializer != NULL)
            actual_count = (int)strlen(string_initializer);
    } else if (single_record_element) {
        actual_count = 1;
    } else {
        for (ast_t *elem = tuple_node->child; elem != NULL; elem = elem->next)
            ++actual_count;
    }

    if (expected_count >= 0 && actual_count != expected_count) {
        if (start == 0 && end == 0 && type_info->array_dimensions != NULL &&
            type_info->array_dimensions->cur != NULL) {
            const char *range_str = (const char *)type_info->array_dimensions->cur;
            int has_alpha = 0;
            for (const char *p = range_str; p != NULL && *p != '\0'; ++p) {
                if (isalpha((unsigned char)*p)) {
                    has_alpha = 1;
                    break;
                }
            }
            if (has_alpha) {
                end = start + actual_count - 1;
                expected_count = actual_count;
            }
        }
    }

    if (is_shortstring_target && expected_count > 0) {
        int visible_capacity = expected_count - 1;
        if (actual_count > visible_capacity) {
            ast_string_value_reset(&owned_string_initializer);
            fprintf(stderr,
                    "ERROR: Const shortstring %s initializer length %d exceeds declared capacity %d.\n",
                    *id_ptr, actual_count, visible_capacity);
            return -1;
        }
    } else if (expected_count >= 0 && actual_count != expected_count) {
        ast_string_value_reset(&owned_string_initializer);
        fprintf(stderr,
                "ERROR: Const array %s initializer count %d does not match declared range %d..%d.\n",
                *id_ptr, actual_count, start, end);
        return -1;
    }

    if (expected_count < 0)
        end = start + actual_count - 1;

    ListBuilder stmt_builder;
    list_builder_init(&stmt_builder);

    int index = start;
    TypeInfo element_array_info = {0};
    int element_is_array = 0;
    if (type_info->element_type == UNKNOWN_TYPE && type_info->element_type_id != NULL) {
        if (resolve_array_type_info_from_ast(type_info->element_type_id, type_section, &element_array_info, 0) == 0 &&
            element_array_info.is_array) {
            element_is_array = 1;
            if (element_array_info.array_dimensions != NULL &&
                element_array_info.array_dimensions->next != NULL) {
                fprintf(stderr, "ERROR: Unsupported multi-dimensional array element for const %s.\n", *id_ptr);
                destroy_type_info_contents(&element_array_info);
                return -1;
            }
            resolve_array_bounds(&element_array_info, type_section, const_section, type_info->element_type_id);
        }
    }

    /* For 2D/3D/4D arrays (array[a..b, c..d, ...]), treat as nested arrays */
    int element_is_2d_array = 0;  /* For 3D arrays, elements are 2D arrays */
    int element_is_3d_array = 0;  /* For 4D arrays, elements are 3D arrays */
    TypeInfo element_2d_inner_info = {0};
    TypeInfo element_3d_inner_info = {0};
    if (is_multidim && !element_is_array) {
        element_is_array = 1;
        element_array_info.is_array = 1;
        element_array_info.start = multidim_inner_start;
        element_array_info.end = multidim_inner_end;
        element_array_info.element_type = type_info->element_type;
        element_array_info.element_type_id = type_info->element_type_id != NULL ? strdup(type_info->element_type_id) : NULL;

        /* For 3D arrays, elements of element_array are themselves arrays */
        if (is_3d) {
            element_is_2d_array = 1;
            element_2d_inner_info.is_array = 1;
            element_2d_inner_info.start = dim3_start;
            element_2d_inner_info.end = dim3_end;
            element_2d_inner_info.element_type = type_info->element_type;
            element_2d_inner_info.element_type_id = type_info->element_type_id != NULL ? strdup(type_info->element_type_id) : NULL;
        }

        /* For 4D arrays, elements of element_2d_inner are themselves arrays */
        if (is_4d) {
            element_is_3d_array = 1;
            element_3d_inner_info.is_array = 1;
            element_3d_inner_info.start = dim4_start;
            element_3d_inner_info.end = dim4_end;
            element_3d_inner_info.element_type = type_info->element_type;
            element_3d_inner_info.element_type_id = type_info->element_type_id != NULL ? strdup(type_info->element_type_id) : NULL;
        }

        /* No need to flatten — array_dimensions will be preserved for linearization */

        /* For enum-indexed arrays, infer inner dimension from first row of initializer */
        if (multidim_infer_bounds && tuple_node != NULL && tuple_node->typ == PASCAL_T_TUPLE) {
            ast_t *first_row = tuple_node->child;
            if (first_row != NULL) {
                ast_t *first_row_unwrapped = unwrap_pascal_node(first_row);
                if (first_row_unwrapped != NULL && first_row_unwrapped->typ == PASCAL_T_TUPLE) {
                    int inner_count = 0;
                    for (ast_t *inner = first_row_unwrapped->child; inner != NULL; inner = inner->next)
                        ++inner_count;
                    element_array_info.start = 0;
                    element_array_info.end = inner_count - 1;
                }
            }
        }
        /* For 3D arrays with enum-indexed 3rd dimension, infer from first element */
        if (is_3d && dim3_infer_bounds && tuple_node != NULL && tuple_node->typ == PASCAL_T_TUPLE) {
            ast_t *first_row = tuple_node->child;
            if (first_row != NULL) {
                ast_t *first_row_unwrapped = unwrap_pascal_node(first_row);
                if (first_row_unwrapped != NULL && first_row_unwrapped->typ == PASCAL_T_TUPLE) {
                    ast_t *first_inner = first_row_unwrapped->child;
                    if (first_inner != NULL) {
                        ast_t *first_inner_unwrapped = unwrap_pascal_node(first_inner);
                        if (first_inner_unwrapped != NULL && first_inner_unwrapped->typ == PASCAL_T_TUPLE) {
                            int innermost_count = 0;
                            for (ast_t *elem = first_inner_unwrapped->child; elem != NULL; elem = elem->next)
                                ++innermost_count;
                            element_2d_inner_info.start = 0;
                            element_2d_inner_info.end = innermost_count - 1;
                        }
                    }
                }
            }
        }
        /* For 4D arrays with enum-indexed 4th dimension, infer from first element */
        if (is_4d && dim4_infer_bounds && tuple_node != NULL && tuple_node->typ == PASCAL_T_TUPLE) {
            ast_t *first_row = tuple_node->child;
            if (first_row != NULL) {
                ast_t *first_row_unwrapped = unwrap_pascal_node(first_row);
                if (first_row_unwrapped != NULL && first_row_unwrapped->typ == PASCAL_T_TUPLE) {
                    ast_t *first_inner = first_row_unwrapped->child;
                    if (first_inner != NULL) {
                        ast_t *first_inner_unwrapped = unwrap_pascal_node(first_inner);
                        if (first_inner_unwrapped != NULL && first_inner_unwrapped->typ == PASCAL_T_TUPLE) {
                            ast_t *first_inner2 = first_inner_unwrapped->child;
                            if (first_inner2 != NULL) {
                                ast_t *first_inner2_unwrapped = unwrap_pascal_node(first_inner2);
                                if (first_inner2_unwrapped != NULL && first_inner2_unwrapped->typ == PASCAL_T_TUPLE) {
                                    int innermost_count = 0;
                                    for (ast_t *elem = first_inner2_unwrapped->child; elem != NULL; elem = elem->next)
                                        ++innermost_count;
                                    element_3d_inner_info.start = 0;
                                    element_3d_inner_info.end = innermost_count - 1;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if (is_string_initializer) {
        const unsigned char *str = NULL;
        if (owned_string_initializer.data != NULL)
            str = (const unsigned char *)owned_string_initializer.data;
        else if (string_initializer != NULL)
            str = (const unsigned char *)string_initializer;
        if (is_shortstring_target) {
            struct Expression *len_rhs = mk_charcode(const_decl_node->line, (unsigned int)actual_count);
            struct Expression *len_index_expr = mk_inum(const_decl_node->line, index);
            struct Expression *len_base_expr = mk_varid(const_decl_node->line, strdup(*id_ptr));
            struct Expression *len_lhs = mk_arrayaccess(const_decl_node->line, len_base_expr, len_index_expr);
            struct Statement *len_assign = mk_varassign(const_decl_node->line, const_decl_node->col,
                len_lhs, len_rhs);
            list_builder_append(&stmt_builder, len_assign, LIST_STMT);
            ++index;
        }
        for (int i = 0; i < actual_count; ++i) {
            unsigned char byte = (str != NULL) ? str[i] : 0;
            struct Expression *rhs = mk_charcode(const_decl_node->line, (unsigned int)byte);
            struct Expression *index_expr = mk_inum(const_decl_node->line, index);
            struct Expression *base_expr = mk_varid(const_decl_node->line, strdup(*id_ptr));
            if (is_widechar_array_target) {
                base_expr->is_array_expr = 1;
                base_expr->array_element_type = CHAR_TYPE;
                base_expr->array_element_size = 2;
                base_expr->array_element_type_id = strdup("WideChar");
            }
            struct Expression *lhs = mk_arrayaccess(const_decl_node->line, base_expr, index_expr);
            if (is_widechar_array_target) {
                lhs->array_element_type = CHAR_TYPE;
                lhs->array_element_size = 2;
                lhs->array_element_type_id = strdup("WideChar");
                lhs->array_lower_bound = start;
                lhs->array_upper_bound = end;
            }
            struct Statement *assign = mk_varassign(const_decl_node->line, const_decl_node->col, lhs, rhs);
            list_builder_append(&stmt_builder, assign, LIST_STMT);
            ++index;
        }
    } else {
        ast_t *element = single_record_element ? tuple_node : tuple_node->child;
        while (element != NULL) {
            ast_t *unwrapped = unwrap_pascal_node(element);

            if (element_is_array) {
                AstStringValue row_string_initializer = {0};
                int row_is_string_initializer = 0;
                int row_is_widechar_target = 0;
                int row_is_char_array_target =
                    (!element_is_2d_array && !element_is_3d_array &&
                     type_info_targets_char_array(&element_array_info, &row_is_widechar_target));

                if (row_is_char_array_target && unwrapped != NULL &&
                    evaluate_const_string_ast(unwrapped, const_section,
                        &row_string_initializer, 0) == 0)
                {
                    row_is_string_initializer = 1;
                }

                if (!row_is_string_initializer &&
                    (unwrapped == NULL || unwrapped->typ != PASCAL_T_TUPLE)) {
                    fprintf(stderr, "ERROR: Const array %s expects tuple initializer for element %d.\n",
                            *id_ptr, index);
                    destroy_list(stmt_builder.head);
                    destroy_type_info_contents(&element_array_info);
                    ast_string_value_reset(&row_string_initializer);
                    return -1;
                }

                int inner_start = element_array_info.start;
                int inner_end = element_array_info.end;
                int inner_expected = -1;
                if (inner_end >= inner_start)
                    inner_expected = inner_end - inner_start + 1;

                int inner_actual = 0;
                if (row_is_string_initializer) {
                    inner_actual = (int)row_string_initializer.len;
                } else {
                    for (ast_t *inner = unwrapped->child; inner != NULL; inner = inner->next)
                        ++inner_actual;
                }

                if (!row_is_string_initializer &&
                    inner_expected >= 0 && inner_actual != inner_expected) {
                    if (inner_start == 0 && inner_end == 0 &&
                        element_array_info.array_dimensions != NULL &&
                        element_array_info.array_dimensions->cur != NULL) {
                        const char *range_str = (const char *)element_array_info.array_dimensions->cur;
                        int has_alpha = 0;
                        for (const char *p = range_str; p != NULL && *p != '\0'; ++p) {
                            if (isalpha((unsigned char)*p)) {
                                has_alpha = 1;
                                break;
                            }
                        }
                        if (has_alpha) {
                            inner_end = inner_start + inner_actual - 1;
                            inner_expected = inner_actual;
                        }
                    }
                }

                if (row_is_string_initializer && inner_expected >= 0 && inner_actual > inner_expected) {
                    fprintf(stderr,
                            "ERROR: Const array %s element %d string initializer length %d exceeds declared range %d..%d.\n",
                            *id_ptr, index, inner_actual, inner_start, inner_end);
                    destroy_list(stmt_builder.head);
                    destroy_type_info_contents(&element_array_info);
                    ast_string_value_reset(&row_string_initializer);
                    return -1;
                }

                if (!row_is_string_initializer &&
                    inner_expected >= 0 && inner_actual != inner_expected) {
                    fprintf(stderr,
                            "ERROR: Const array %s element %d initializer count %d does not match declared range %d..%d.\n",
                            *id_ptr, index, inner_actual, inner_start, inner_end);
                    destroy_list(stmt_builder.head);
                    destroy_type_info_contents(&element_array_info);
                    ast_string_value_reset(&row_string_initializer);
                    return -1;
                }

                if (row_is_string_initializer) {
                    int fill_count = (inner_expected >= 0) ? inner_expected : inner_actual;
                    int inner_index = inner_start;
                    for (int byte_index = 0; byte_index < fill_count; ++byte_index) {
                        unsigned char byte = 0;
                        struct Expression *rhs;
                        struct Expression *lhs;

                        if ((size_t)byte_index < row_string_initializer.len &&
                            row_string_initializer.data != NULL)
                            byte = (unsigned char)row_string_initializer.data[byte_index];
                        rhs = mk_charcode(element->line, (unsigned int)byte);
                        lhs = mk_const_array_element_lhs(element->line, *id_ptr, index,
                            inner_index, is_multidim);
                        if (row_is_widechar_target) {
                            lhs->array_element_type = CHAR_TYPE;
                            lhs->array_element_size = 2;
                            lhs->array_element_type_id = strdup("WideChar");
                            lhs->array_lower_bound = inner_start;
                            lhs->array_upper_bound = (inner_expected >= 0) ? inner_end :
                                (inner_start + fill_count - 1);
                        }
                        list_builder_append(&stmt_builder,
                            mk_varassign(element->line, element->col, lhs, rhs), LIST_STMT);
                        ++inner_index;
                    }

                    ast_string_value_reset(&row_string_initializer);
                    ++index;
                    element = single_record_element ? NULL : element->next;
                    continue;
                }

                int inner_index = inner_start;
                for (ast_t *inner = unwrapped->child; inner != NULL; inner = inner->next) {
                    ast_t *inner_unwrapped = unwrap_pascal_node(inner);

                    /* For 3D arrays, handle the innermost dimension */
                    if (element_is_2d_array) {
                        if (inner_unwrapped == NULL || inner_unwrapped->typ != PASCAL_T_TUPLE) {
                            fprintf(stderr, "ERROR: Const 3D array %s expects tuple for element [%d][%d].\n",
                                    *id_ptr, index, inner_index);
                            destroy_list(stmt_builder.head);
                            destroy_type_info_contents(&element_array_info);
                            destroy_type_info_contents(&element_2d_inner_info);
                            return -1;
                        }

                        int innermost_index = element_2d_inner_info.start;
                        for (ast_t *innermost = inner_unwrapped->child; innermost != NULL; innermost = innermost->next) {
                            ast_t *innermost_unwrapped = unwrap_pascal_node(innermost);

                            /* For 4D arrays, handle the 4th dimension */
                            if (element_is_3d_array) {
                                if (innermost_unwrapped == NULL || innermost_unwrapped->typ != PASCAL_T_TUPLE) {
                                    fprintf(stderr, "ERROR: Const 4D array %s expects tuple for element [%d][%d][%d].\n",
                                            *id_ptr, index, inner_index, innermost_index);
                                    destroy_list(stmt_builder.head);
                                    destroy_type_info_contents(&element_array_info);
                                    destroy_type_info_contents(&element_2d_inner_info);
                                    destroy_type_info_contents(&element_3d_inner_info);
                                    return -1;
                                }

                                int dim4_index = element_3d_inner_info.start;
                                for (ast_t *dim4_elem = innermost_unwrapped->child; dim4_elem != NULL; dim4_elem = dim4_elem->next) {
                                    ast_t *dim4_unwrapped = unwrap_pascal_node(dim4_elem);
                                    struct Expression *rhs = convert_expression(dim4_unwrapped);
                                    if (rhs == NULL) {
                                        fprintf(stderr, "ERROR: Unsupported const array element in %s[%d][%d][%d][%d].\n",
                                                *id_ptr, index, inner_index, innermost_index, dim4_index);
                                        destroy_list(stmt_builder.head);
                                        destroy_type_info_contents(&element_array_info);
                                        destroy_type_info_contents(&element_2d_inner_info);
                                        destroy_type_info_contents(&element_3d_inner_info);
                                        return -1;
                                    }

                                    struct Expression *outer_index_expr = mk_inum(element->line, index);
                                    struct Expression *base_expr = mk_varid(element->line, strdup(*id_ptr));
                                    struct Expression *inner_index_expr = mk_inum(element->line, inner_index);
                                    struct Expression *innermost_index_expr = mk_inum(element->line, innermost_index);
                                    struct Expression *dim4_index_expr = mk_inum(element->line, dim4_index);
                                    struct Expression *lhs;
                                    if (is_multidim) {
                                        /* True multi-dim: arr[d1, d2, d3, d4] */
                                        lhs = mk_arrayaccess(element->line, base_expr, outer_index_expr);
                                        ListNode_t *extra3 = CreateListNode(dim4_index_expr, LIST_EXPR);
                                        ListNode_t *extra2 = CreateListNode(innermost_index_expr, LIST_EXPR);
                                        ListNode_t *extra1 = CreateListNode(inner_index_expr, LIST_EXPR);
                                        extra1->next = extra2;
                                        extra2->next = extra3;
                                        lhs->expr_data.array_access_data.extra_indices = extra1;
                                    } else {
                                        /* Array-of-array-of-array-of-array: arr[d1][d2][d3][d4] */
                                        struct Expression *a1 = mk_arrayaccess(element->line, base_expr, outer_index_expr);
                                        struct Expression *a2 = mk_arrayaccess(element->line, a1, inner_index_expr);
                                        struct Expression *a3 = mk_arrayaccess(element->line, a2, innermost_index_expr);
                                        lhs = mk_arrayaccess(element->line, a3, dim4_index_expr);
                                    }
                                    struct Statement *assign = mk_varassign(element->line, element->col, lhs, rhs);
                                    list_builder_append(&stmt_builder, assign, LIST_STMT);
                                    ++dim4_index;
                                }
                            } else {
                            struct Expression *rhs = convert_expression(innermost_unwrapped);
                            if (rhs == NULL) {
                                fprintf(stderr, "ERROR: Unsupported const array element in %s[%d][%d][%d].\n",
                                        *id_ptr, index, inner_index, innermost_index);
                                destroy_list(stmt_builder.head);
                                destroy_type_info_contents(&element_array_info);
                                destroy_type_info_contents(&element_2d_inner_info);
                                return -1;
                            }

                            struct Expression *outer_index_expr = mk_inum(element->line, index);
                            struct Expression *base_expr = mk_varid(element->line, strdup(*id_ptr));
                            struct Expression *inner_index_expr = mk_inum(element->line, inner_index);
                            struct Expression *innermost_index_expr = mk_inum(element->line, innermost_index);
                            struct Expression *lhs;
                            if (is_multidim) {
                                /* True multi-dim: arr[outer, inner, innermost] */
                                lhs = mk_arrayaccess(element->line, base_expr, outer_index_expr);
                                ListNode_t *extra2 = CreateListNode(innermost_index_expr, LIST_EXPR);
                                ListNode_t *extra1 = CreateListNode(inner_index_expr, LIST_EXPR);
                                extra1->next = extra2;
                                lhs->expr_data.array_access_data.extra_indices = extra1;
                            } else {
                                /* Array-of-array-of-array: arr[outer][inner][innermost] */
                                struct Expression *outer_access = mk_arrayaccess(element->line, base_expr, outer_index_expr);
                                struct Expression *middle_access = mk_arrayaccess(element->line, outer_access, inner_index_expr);
                                lhs = mk_arrayaccess(element->line, middle_access, innermost_index_expr);
                            }
                            struct Statement *assign = mk_varassign(element->line, element->col, lhs, rhs);
                            list_builder_append(&stmt_builder, assign, LIST_STMT);
                            }
                            ++innermost_index;
                        }
                    } else {
                        struct Expression *rhs = convert_expression(inner_unwrapped);
                        if (rhs == NULL) {
                            fprintf(stderr, "ERROR: Unsupported const array element in %s[%d].\n", *id_ptr, index);
                            destroy_list(stmt_builder.head);
                            destroy_type_info_contents(&element_array_info);
                            ast_string_value_reset(&row_string_initializer);
                            return -1;
                        }

                        struct Expression *lhs = mk_const_array_element_lhs(
                            element->line, *id_ptr, index, inner_index, is_multidim);
                        if (row_is_widechar_target) {
                            lhs->array_element_type = CHAR_TYPE;
                            lhs->array_element_size = 2;
                            lhs->array_element_type_id = strdup("WideChar");
                            lhs->array_lower_bound = inner_start;
                            lhs->array_upper_bound = inner_end;
                        }
                        struct Statement *assign = mk_varassign(element->line, element->col, lhs, rhs);
                        list_builder_append(&stmt_builder, assign, LIST_STMT);
                    }
                    ++inner_index;
                }

                ast_string_value_reset(&row_string_initializer);
                ++index;
                element = single_record_element ? NULL : element->next;
                continue;
            }

            /* A single-field record constructor (Ch: [...]) gets parsed as FIELD_WIDTH.
               Convert it to a single-child RECORD_CONSTRUCTOR for uniform handling.
               Skip when the field value is a TUPLE — convert_expression's FIELD_WIDTH
               handler already recognizes TUPLE values as record constructors. */
            if (unwrapped != NULL && unwrapped->typ == PASCAL_T_FIELD_WIDTH &&
                unwrapped->child != NULL && unwrapped->child->typ == PASCAL_T_IDENTIFIER) {
                ast_t *fval = unwrapped->child->next;
                ast_t *fval_unwrapped = unwrap_pascal_node(fval);
                if (fval_unwrapped == NULL || fval_unwrapped->typ != PASCAL_T_TUPLE) {
                    unwrapped->typ = PASCAL_T_ASSIGNMENT;
                    ast_t *wrapper = new_ast();
                    *wrapper = *unwrapped;
                    wrapper->next = NULL;
                    unwrapped->typ = PASCAL_T_RECORD_CONSTRUCTOR;
                    unwrapped->child = wrapper;
                    unwrapped->sym = NULL;
                }
            }
            /* Special handling for record constructors in const arrays */
            if (unwrapped != NULL &&
                (unwrapped->typ == PASCAL_T_RECORD_CONSTRUCTOR ||
                 tuple_is_record_constructor(unwrapped))) {
                /* Generate field assignments for each field in the record constructor */
                ast_t *field_assignment = unwrapped->child;
                while (field_assignment != NULL) {
                    ast_t *assignment_node = unwrap_record_constructor_elem(field_assignment);
                    if (assignment_node != NULL &&
                        (assignment_node->typ == PASCAL_T_ASSIGNMENT ||
                         assignment_node->typ == PASCAL_T_FIELD_WIDTH)) {
                        ast_t *field_name_node = assignment_node->child;
                        ast_t *field_value_node = (field_name_node != NULL) ? field_name_node->next : NULL;
                        
                        if (field_name_node != NULL && field_value_node != NULL && field_name_node->sym != NULL) {
                            char *field_name = field_name_node->sym->name;
                            struct Expression *field_value = NULL;
                            if (field_value_node != NULL && field_value_node->typ == PASCAL_T_TUPLE &&
                                tuple_is_record_constructor(field_value_node))
                            {
                                field_value = convert_record_constructor_expr(field_value_node);
                            }
                            else
                            {
                                field_value = convert_expression(field_value_node);
                            }
                            
                            if (field_value == NULL) {
                                fprintf(stderr, "ERROR: Failed to convert field value for %s[%d].%s.\n",
                                        *id_ptr, index, field_name);
                                destroy_list(stmt_builder.head);
                                return -1;
                            }
                            
                            if (field_value != NULL) {
                                /* Create expression: array_name[index].field_name */
                                struct Expression *index_expr = mk_inum(element->line, index);
                                struct Expression *base_expr = mk_varid(element->line, strdup(*id_ptr));
                                struct Expression *array_elem = mk_arrayaccess(element->line, base_expr, index_expr);
                                struct Expression *lhs = mk_recordaccess(element->line, array_elem, strdup(field_name));
                                
                                /* Generate assignment statement */
                                struct Statement *field_assign = mk_varassign(element->line, element->col, lhs, field_value);
                                list_builder_append(&stmt_builder, field_assign, LIST_STMT);
                            } else {
                                fprintf(stderr, "ERROR: Unsupported field value in record constructor for %s[%d].%s.\n",
                                        *id_ptr, index, field_name);
                                destroy_list(stmt_builder.head);
                                return -1;
                            }
                        }
                    }
                    field_assignment = field_assignment->next;
                }
            } else {
                /* Regular expression element */
                struct Expression *rhs = convert_expression(unwrapped);
                if (rhs == NULL) {
                    fprintf(stderr, "ERROR: Unsupported const array element in %s.\n", *id_ptr);
                    destroy_list(stmt_builder.head);
                    return -1;
                }

                struct Expression *index_expr = mk_inum(element->line, index);
                struct Expression *base_expr = mk_varid(element->line, strdup(*id_ptr));
                struct Expression *lhs = mk_arrayaccess(element->line, base_expr, index_expr);
                struct Statement *assign = mk_varassign(element->line, element->col, lhs, rhs);
                list_builder_append(&stmt_builder, assign, LIST_STMT);
            }

            ++index;
            element = single_record_element ? NULL : element->next;
        }
    }

    destroy_type_info_contents(&element_array_info);
    destroy_type_info_contents(&element_2d_inner_info);
    destroy_type_info_contents(&element_3d_inner_info);
    ast_string_value_reset(&owned_string_initializer);

    ListNode_t *assignments = list_builder_finish(&stmt_builder);
    struct Statement *initializer = NULL;
    if (assignments != NULL)
        initializer = mk_compoundstatement(const_decl_node->line, assignments);

    ListNode_t *ids = CreateListNode(*id_ptr, LIST_STRING);
    char *range_str = NULL;
    if (type_info->array_dimensions != NULL && type_info->array_dimensions->cur != NULL) {
        range_str = strdup((char *)type_info->array_dimensions->cur);
    }
    int decl_element_type = type_info->element_type;
    char *decl_element_type_id = type_info->element_type_id != NULL ?
        strdup(type_info->element_type_id) : NULL;
    struct RecordType *inline_record = type_info->record_type;
    if (inline_record != NULL)
        type_info->record_type = NULL;
    Tree_t *array_decl = mk_arraydecl(const_decl_node->line, ids, decl_element_type,
                                      decl_element_type_id, start, end, range_str, initializer,
                                      inline_record);
    array_decl->tree_data.arr_decl_data.type_ref =
        type_ref_from_element_info(type_info, type_info->element_type_id);
    if (type_info->element_kgpc_type != NULL)
    {
        array_decl->tree_data.arr_decl_data.element_kgpc_type = type_info->element_kgpc_type;
        kgpc_type_retain(array_decl->tree_data.arr_decl_data.element_kgpc_type);
    }
    type_info->element_type_id = NULL;

    /* Transfer array_dimensions to declaration for multi-dim linearization.
     * Only when 2+ dimensions — single-dim doesn't need linearization. */
    if (type_info->array_dimensions != NULL &&
        type_info->array_dimensions->next != NULL) {
        array_decl->tree_data.arr_decl_data.array_dimensions = type_info->array_dimensions;
        type_info->array_dimensions = NULL;
    }

    if (type_info->unresolved_index_type != NULL) {
        array_decl->tree_data.arr_decl_data.unresolved_index_type = type_info->unresolved_index_type;
        type_info->unresolved_index_type = NULL;
    }
    /* Fallback: for single-dim enum-indexed arrays where resolve_array_bounds
     * didn't set unresolved_index_type (e.g., enum was defined later), extract
     * the dimension name from array_dimensions. */
    if (array_decl->tree_data.arr_decl_data.unresolved_index_type == NULL &&
        start == 0 && end == 0 &&
        type_info->array_dimensions != NULL &&
        type_info->array_dimensions->type == LIST_STRING &&
        type_info->array_dimensions->cur != NULL)
    {
        const char *dim_str = (const char *)type_info->array_dimensions->cur;
        if (strstr(dim_str, "..") == NULL) {
            array_decl->tree_data.arr_decl_data.unresolved_index_type = strdup(dim_str);
        }
    }
    array_decl->tree_data.arr_decl_data.is_typed_const = 1;
    array_decl->tree_data.arr_decl_data.has_static_storage = 1;

    char label_buffer[128];
    snprintf(label_buffer, sizeof(label_buffer), "__kgpc_tconst_array_%s_%d", g_typed_const_unit_tag, typed_const_counter);
    array_decl->tree_data.arr_decl_data.static_label = strdup(label_buffer);
    snprintf(label_buffer, sizeof(label_buffer), "__kgpc_tconst_guard_%s_%d", g_typed_const_unit_tag, typed_const_counter);
    array_decl->tree_data.arr_decl_data.init_guard_label = strdup(label_buffer);
    ++typed_const_counter;

    list_builder_append(var_builder, array_decl, LIST_TREE);

    *id_ptr = NULL;
    return 0;
}

static Tree_t *convert_const_decl(ast_t *const_decl_node, ListBuilder *var_builder, ast_t *type_section, ast_t *const_section) {
    if (const_decl_node == NULL)
        return NULL;

    ast_t *cur = const_decl_node->child;
    if (cur == NULL)
        return NULL;

    char *id = dup_symbol(cur);
    if (id == NULL)
        return NULL;

    cur = cur->next;
    char *type_id = NULL;
    TypeInfo type_info = {0};

    if (cur != NULL &&
        (cur->typ == PASCAL_T_TYPE_SPEC ||
         cur->typ == PASCAL_T_ARRAY_TYPE ||
         cur->typ == PASCAL_T_RECORD_TYPE ||
         cur->typ == PASCAL_T_POINTER_TYPE ||
         cur->typ == PASCAL_T_PROCEDURE_TYPE ||
         cur->typ == PASCAL_T_FUNCTION_TYPE)) {
        int spec_type = convert_type_spec(cur, &type_id, NULL, &type_info);
        /* For inline procedure/function type constants, create a typed variable.
         * convert_type_spec returns PROCEDURE but doesn't set type_id for inline
         * procedure types like: VarClearProc: procedure(var v: TVarData) = nil; */
        ast_t *unwrapped_cur = cur;
        if (unwrapped_cur->typ == PASCAL_T_TYPE_SPEC && unwrapped_cur->child != NULL)
            unwrapped_cur = unwrapped_cur->child;
        unwrapped_cur = unwrap_pascal_node(unwrapped_cur);
        if (spec_type == PROCEDURE && type_id == NULL && unwrapped_cur != NULL &&
            (unwrapped_cur->typ == PASCAL_T_PROCEDURE_TYPE || unwrapped_cur->typ == PASCAL_T_FUNCTION_TYPE)) {
            ast_t *proc_type_node = unwrapped_cur;
            cur = cur->next;
            ast_t *init_value = unwrap_pascal_node(cur);
            struct Expression *init_expr = convert_expression(init_value);
            ListNode_t *ids = CreateListNode(id, LIST_STRING);
            struct Expression *lhs = mk_varid(const_decl_node->line, strdup(id));
            struct Statement *initializer_stmt = NULL;
            if (init_expr != NULL)
                initializer_stmt = mk_varassign(const_decl_node->line, const_decl_node->col,
                                                 lhs, init_expr);
            else
                destroy_expr(lhs);
            Tree_t *decl = mk_vardecl(const_decl_node->line, ids, PROCEDURE, NULL, 0, 0,
                                       initializer_stmt, NULL, NULL, NULL);
            decl->tree_data.var_decl_data.is_typed_const = 1;
            mark_var_decl_static_storage(decl);
            decl->tree_data.var_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);
            /* Attach the inline procedure type for proper type checking */
            KgpcType *proc_kgpc = convert_type_spec_to_kgpctype(proc_type_node, NULL);
            if (proc_kgpc != NULL) {
                decl->tree_data.var_decl_data.cached_kgpc_type = proc_kgpc;
                kgpc_type_retain(proc_kgpc);
            }
            list_builder_append(var_builder, decl, LIST_TREE);
            destroy_type_info_contents(&type_info);
            return NULL;
        }
        cur = cur->next;
    } else if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER &&
               cur->sym != NULL && cur->sym->name != NULL &&
               cur->next != NULL) {
        /* Bare identifier as type annotation (common in record/class const sections).
         * AST: IDENTIFIER("Single") -> REAL("1.5")
         * The identifier is the type name, the next sibling is the value.
         * Only consume when map_type_name recognizes it as a known type
         * to avoid misinterpreting keywords like "set" in "set of TEnum". */
        char *type_name = dup_symbol(cur);
        if (type_name != NULL) {
            int mapped = map_type_name(type_name, &type_id);
            mapped = apply_shortstring_mode(mapped, type_name);
            if (mapped != UNKNOWN_TYPE || type_id != NULL) {
                /* Known type name — consume and advance */
                free(type_name);
                cur = cur->next;
            } else {
                /* Unknown type name — treat as type annotation for typed consts
                 * like: cnodeutils: tnodeutilsclass = tnodeutils
                 * where 'tnodeutilsclass' is not a builtin type. */
                type_id = type_name;
                cur = cur->next;
            }
        }
    } else if (cur != NULL && cur->typ == PASCAL_T_NONE) {
        ast_t *type_node = cur->child;
        while (type_node != NULL &&
               type_node->typ != PASCAL_T_TYPE_SPEC &&
               type_node->typ != PASCAL_T_ARRAY_TYPE &&
               type_node->typ != PASCAL_T_RECORD_TYPE &&
               type_node->typ != PASCAL_T_POINTER_TYPE &&
               type_node->typ != PASCAL_T_PROCEDURE_TYPE &&
               type_node->typ != PASCAL_T_FUNCTION_TYPE &&
               type_node->typ != PASCAL_T_IDENTIFIER) {
            type_node = type_node->next;
        }
        if (type_node != NULL) {
            if (type_node->typ == PASCAL_T_TYPE_SPEC ||
                type_node->typ == PASCAL_T_ARRAY_TYPE ||
                type_node->typ == PASCAL_T_RECORD_TYPE ||
                type_node->typ == PASCAL_T_POINTER_TYPE ||
                type_node->typ == PASCAL_T_PROCEDURE_TYPE ||
                type_node->typ == PASCAL_T_FUNCTION_TYPE) {
                convert_type_spec(type_node, &type_id, NULL, &type_info);
            } else if (type_node->typ == PASCAL_T_IDENTIFIER) {
                char *type_name = dup_symbol(type_node);
                if (type_name != NULL) {
                    int mapped = map_type_name(type_name, &type_id);
                    mapped = apply_shortstring_mode(mapped, type_name);
                    if (mapped == UNKNOWN_TYPE && type_id == NULL)
                        type_id = type_name;
                    else
                        free(type_name);
                }
            }
            cur = cur->next;
        }
    }

    /* When the type is a named identifier (like TNames), resolve it to check
     * if it's an array type alias so we can use lower_const_array. */
    if (type_id != NULL && !type_info.is_array) {
        TypeInfo resolved_info = {0};
        if (resolve_array_type_info_from_ast(type_id, type_section, &resolved_info, 0) == 0) {
            resolve_array_bounds(&resolved_info, type_section, const_section, id);
            type_info = resolved_info;
        }
    }

    if (type_id == NULL && const_section_is_resourcestring(const_section)) {
        type_id = strdup("AnsiString");
    }

    ast_t *value_node = unwrap_pascal_node(cur);
    if (value_node == NULL) {
        fprintf(stderr, "ERROR: Unsupported const declaration for %s.\n", id);
        if (type_id != NULL)
            free(type_id);
        free(id);
        destroy_type_info_contents(&type_info);
        return NULL;
    }

    /* A single-field record constructor like (Ch: [Ch_Mop2]) gets parsed as a
       parenthesized FIELD_WIDTH expression (the : is the WriteLn format operator).
       When we have a typed const, convert FIELD_WIDTH back to a single-field
       RECORD_CONSTRUCTOR so the lowering code handles it correctly.
       FIELD_WIDTH structure:  child=IDENTIFIER(Ch) -> next=SET(...)
       Needed structure:       RECORD_CONSTRUCTOR -> child=ASSIGNMENT -> child=IDENTIFIER(Ch) -> next=SET(...)
       We repurpose the FIELD_WIDTH node as the ASSIGNMENT and create a wrapper. */
    if (value_node->typ == PASCAL_T_FIELD_WIDTH && type_id != NULL &&
        value_node->child != NULL && value_node->child->typ == PASCAL_T_IDENTIFIER) {
        /* Check the field value — if it's a TUPLE, the existing convert_expression
           FIELD_WIDTH handler already handles it as a single-field record constructor.
           Only convert for non-TUPLE values (e.g. set constructors) that need help. */
        ast_t *field_val = value_node->child->next;
        ast_t *field_val_unwrapped = unwrap_pascal_node(field_val);
        if (field_val_unwrapped == NULL || field_val_unwrapped->typ != PASCAL_T_TUPLE) {
            /* Turn the FIELD_WIDTH into an ASSIGNMENT (same child structure) */
            value_node->typ = PASCAL_T_ASSIGNMENT;
            /* Wrap: create a RECORD_CONSTRUCTOR that replaces value_node in the AST.
               We do this by swapping: copy value_node's ASSIGNMENT data into a new node,
               then make value_node the RECORD_CONSTRUCTOR pointing to it. */
            ast_t *assignment = new_ast();
            *assignment = *value_node;
            assignment->next = NULL;
            value_node->typ = PASCAL_T_RECORD_CONSTRUCTOR;
            value_node->child = assignment;
            value_node->sym = NULL;
        }
    }

    if (type_info.is_array) {
        if (lower_const_array(const_decl_node, &id, &type_info, value_node, var_builder, type_section, const_section) != 0)
            free(id);

        if (type_id != NULL)
            free(type_id);
        destroy_type_info_contents(&type_info);
        return NULL;
    }

    if (type_id != NULL) {
        int empty_tuple_record_const = 0;
        if (value_node != NULL && (value_node->typ == PASCAL_T_TUPLE || value_node->typ == PASCAL_T_NONE)) {
            ast_t *child = value_node->child;
            if (child == NULL ||
                (child->typ == PASCAL_T_NONE && child->child == NULL && child->next == NULL)) {
                empty_tuple_record_const = 1;
            }
        }

        int typed_const_tag = map_type_name(type_id, NULL);
        if (typed_const_tag == STRING_TYPE) {
            struct Expression *init_expr = convert_expression(value_node);
            if (init_expr == NULL) {
                fprintf(stderr, "ERROR: Unsupported typed const expression for %s.\n", id);
                free(id);
                free(type_id);
                destroy_type_info_contents(&type_info);
                return NULL;
            }

            ListNode_t *ids = CreateListNode(id, LIST_STRING);
            struct Expression *lhs = mk_varid(const_decl_node->line, strdup(id));
            struct Statement *initializer_stmt = mk_varassign(const_decl_node->line, const_decl_node->col,
                                                              lhs, init_expr);
            Tree_t *decl = mk_vardecl(const_decl_node->line, ids, typed_const_tag, type_id, 0, 0,
                                      initializer_stmt, NULL, NULL, NULL);
            decl->tree_data.var_decl_data.is_typed_const = 1;
            mark_var_decl_static_storage(decl);
            decl->tree_data.var_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);
            list_builder_append(var_builder, decl, LIST_TREE);

            destroy_type_info_contents(&type_info);
            return NULL;
        }
        /* If the value is a tuple that looks like a record constructor (field: value; ...),
           treat it as a record constructor instead of a plain expression. */
        if (value_node != NULL && value_node->typ == PASCAL_T_TUPLE &&
            tuple_is_record_constructor(value_node)) {
            value_node->typ = PASCAL_T_RECORD_CONSTRUCTOR;
        }
        if (value_node != NULL && value_node->typ != PASCAL_T_RECORD_CONSTRUCTOR && !empty_tuple_record_const) {
            struct Expression *init_expr = convert_expression(value_node);
            if (init_expr == NULL) {
                fprintf(stderr, "ERROR: Unsupported typed const expression for %s.\n", id);
                free(id);
                free(type_id);
                destroy_type_info_contents(&type_info);
                return NULL;
            }

            ListNode_t *ids = CreateListNode(id, LIST_STRING);
            struct Expression *lhs = mk_varid(const_decl_node->line, strdup(id));
            struct Statement *initializer_stmt = mk_varassign(const_decl_node->line, const_decl_node->col,
                                                              lhs, init_expr);
            Tree_t *decl = mk_vardecl(const_decl_node->line, ids, typed_const_tag, type_id, 0, 0,
                                      initializer_stmt, NULL, NULL, NULL);
            decl->tree_data.var_decl_data.is_typed_const = 1;
            mark_var_decl_static_storage(decl);
            decl->tree_data.var_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);
            list_builder_append(var_builder, decl, LIST_TREE);

            destroy_type_info_contents(&type_info);
            return NULL;
        }

        if (empty_tuple_record_const) {
            int var_type = UNKNOWN_TYPE;
            struct RecordType *inline_record = NULL;
            if (type_id != NULL) {
                var_type = map_type_name(type_id, NULL);
                var_type = apply_shortstring_mode(var_type, type_id);
            } else if (type_info.is_record && type_info.record_type != NULL)
            {
                var_type = RECORD_TYPE;
                inline_record = type_info.record_type;
                type_info.record_type = NULL;
            }

            ListNode_t *var_ids = CreateListNode(id, LIST_STRING);
            Tree_t *var_decl = mk_vardecl(const_decl_node->line, var_ids, var_type,
                type_id, 0, 0, NULL, inline_record, NULL, NULL);
            var_decl->tree_data.var_decl_data.is_typed_const = 1;
            mark_var_decl_static_storage(var_decl);
            var_decl->tree_data.var_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);

            if (var_builder != NULL)
                list_builder_append(var_builder, var_decl, LIST_TREE);

            destroy_type_info_contents(&type_info);
            return NULL;
        }
    }

    /* Handle record constructor constants by lowering to variable with field initializers */
    if (value_node != NULL && value_node->typ == PASCAL_T_RECORD_CONSTRUCTOR) {
        /* Lower record const to variable declaration with field-by-field initialization */
        ListBuilder field_stmts;
        list_builder_init(&field_stmts);
        
        struct RecordType *record_info = NULL;
        if (type_info.is_record && type_info.record_type != NULL)
            record_info = type_info.record_type;

        ast_t *field_assignment = value_node->child;
        while (field_assignment != NULL) {
            if (field_assignment->typ == PASCAL_T_ASSIGNMENT ||
                field_assignment->typ == PASCAL_T_FIELD_WIDTH) {
                ast_t *field_name_node = field_assignment->child;
                ast_t *field_value_node = (field_name_node != NULL) ? field_name_node->next : NULL;
                
                if (field_name_node != NULL && field_value_node != NULL && field_name_node->sym != NULL) {
                    char *field_name = field_name_node->sym->name;
                    struct Expression *field_value = NULL;
                    if (field_value_node != NULL && field_value_node->typ == PASCAL_T_TUPLE &&
                        tuple_is_record_constructor(field_value_node))
                    {
                        field_value = convert_record_constructor_expr(field_value_node);
                    }
                    else
                    {
                        field_value = convert_expression(field_value_node);
                    }
                    
                    if (field_value != NULL) {
                        if (record_info != NULL && field_value->type == EXPR_ARRAY_LITERAL) {
                            struct RecordField *field_desc =
                                find_record_field_by_name(record_info, field_name);
                            if (field_desc != NULL && field_desc->is_array) {
                                if (kgpc_getenv("KGPC_DEBUG_RECORD_CONST") != NULL)
                                {
                                    fprintf(stderr,
                                        "[KGPC] record const %s.%s array elem type=%d id=%s\n",
                                        id,
                                        field_name,
                                        field_desc->array_element_type,
                                        field_desc->array_element_type_id != NULL
                                            ? field_desc->array_element_type_id
                                            : "<null>");
                                }
                                if (field_value->array_element_type == UNKNOWN_TYPE)
                                    field_value->array_element_type = field_desc->array_element_type;
                                if (field_value->array_element_type_id == NULL &&
                                    field_desc->array_element_type_id != NULL)
                                    field_value->array_element_type_id =
                                        strdup(field_desc->array_element_type_id);
                            }
                        }
                        /* Create expression: const_name.field_name */
                        struct Expression *base_expr = mk_varid(const_decl_node->line, strdup(id));
                        struct Expression *lhs = mk_recordaccess(const_decl_node->line, base_expr, strdup(field_name));
                        
                        /* Generate assignment statement */
                        struct Statement *field_assign = mk_varassign(const_decl_node->line, const_decl_node->col, lhs, field_value);
                        list_builder_append(&field_stmts, field_assign, LIST_STMT);
                    } else {
                        fprintf(stderr, "ERROR: Unsupported field value in record constructor for %s.%s.\n",
                                id, field_name);
                        destroy_list(field_stmts.head);
                        if (type_id != NULL)
                            free(type_id);
                        free(id);
                        destroy_type_info_contents(&type_info);
                        return NULL;
                    }
                }
            }
            field_assignment = field_assignment->next;
        }
        
        ListNode_t *field_assignments = list_builder_finish(&field_stmts);
        struct Statement *initializer = NULL;
        if (field_assignments != NULL)
            initializer = mk_compoundstatement(const_decl_node->line, field_assignments);
        
        /* Determine the record type from type_id or inline type info */
        int var_type = UNKNOWN_TYPE;
        struct RecordType *inline_record = NULL;
        if (type_id != NULL) {
            var_type = map_type_name(type_id, NULL);
            var_type = apply_shortstring_mode(var_type, type_id);
        } else if (type_info.is_record && type_info.record_type != NULL) {
            var_type = RECORD_TYPE;
            inline_record = type_info.record_type;
            type_info.record_type = NULL;
        }
        
        /* Create variable declaration for the record const */
        ListNode_t *var_ids = CreateListNode(id, LIST_STRING);
        Tree_t *var_decl = mk_vardecl(const_decl_node->line, var_ids, var_type,
            type_id, 0, 0, initializer, inline_record, NULL, NULL);
        var_decl->tree_data.var_decl_data.is_typed_const = 1;
        mark_var_decl_static_storage(var_decl);
        var_decl->tree_data.var_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);

        if (var_builder != NULL)
            list_builder_append(var_builder, var_decl, LIST_TREE);
        
        destroy_type_info_contents(&type_info);
        return NULL; /* Record const is lowered to variable, no const decl returned */
    }

    int const_value = 0;
    if (evaluate_const_int_expr(value_node, &const_value, 0) == 0) {
        register_const_int(id, const_value);
    } else {
        /* Handle scoped enum literals like TEnum.Value */
        ast_t *unwrapped_value = unwrap_pascal_node(value_node);
        if (unwrapped_value != NULL && unwrapped_value->typ == PASCAL_T_MEMBER_ACCESS) {
            ast_t *base = unwrap_pascal_node(unwrapped_value->child);
            if (base != NULL && base->typ == PASCAL_T_IDENTIFIER &&
                base->sym != NULL && base->sym->name != NULL &&
                unwrapped_value->sym != NULL && unwrapped_value->sym->name != NULL) {
                int ordinal = resolve_enum_literal_in_type(
                    base->sym->name, unwrapped_value->sym->name, type_section);
                if (ordinal >= 0) {
                    register_const_int(id, ordinal);
                }
            }
        } else if (unwrapped_value != NULL && unwrapped_value->typ == PASCAL_T_FUNC_CALL) {
            /* Treat TypeName(expr) as a typecast in const expressions. */
            const char *callee = (unwrapped_value->sym != NULL) ? unwrapped_value->sym->name : NULL;
            int is_type = 0;
            if (callee != NULL) {
                if (map_type_name(callee, NULL) != UNKNOWN_TYPE)
                    is_type = 1;
                else if (type_name_exists_in_section(callee, type_section))
                    is_type = 1;
            }

            if (is_type) {
                ast_t *args = unwrap_pascal_node(unwrapped_value->child);
                ast_t *arg = (args != NULL) ? unwrap_pascal_node(args->child) : NULL;
                if (arg != NULL && (args->child == NULL || args->child->next == NULL)) {
                    if (evaluate_const_int_expr(arg, &const_value, 0) == 0)
                        register_const_int(id, const_value);
                }
            }
        }
    }

    struct Expression *value_expr = convert_expression(value_node);
    if (value_expr == NULL) {
        fprintf(stderr, "ERROR: Unsupported const expression for %s.\n", id);
        if (type_id != NULL)
            free(type_id);
        free(id);
        return NULL;
    }

    Tree_t *decl = mk_constdecl(const_decl_node->line, id, type_id, value_expr);
    if (decl != NULL)
        decl->tree_data.const_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);

    destroy_type_info_contents(&type_info);

    return decl;
}

void append_const_decls_from_section(ast_t *const_section, ListNode_t **dest,
                                            ListBuilder *var_builder, ast_t *type_section) {
    if (const_section == NULL || dest == NULL)
        return;

    register_const_section(const_section);

    ListNode_t **tail = dest;
    while (*tail != NULL)
        tail = &(*tail)->next;

    ast_t *const_decl = const_section->child;
    while (const_decl != NULL) {
        ast_t *node = unwrap_pascal_node(const_decl);
        if (node == NULL)
            node = const_decl;
        if (node != NULL && node->typ == PASCAL_T_CONST_DECL) {
            if (kgpc_debug_decl_scan_enabled()) {
                ast_t *id_node = node->child;
                if (id_node != NULL && id_node->sym != NULL) {
                    fprintf(stderr, "[KGPC] const decl: %s\n", id_node->sym->name);
                }
            }
            Tree_t *decl = convert_const_decl(node, var_builder, type_section, const_section);
            if (decl != NULL) {
                ListNode_t *node = CreateListNode(decl, LIST_TREE);
                *tail = node;
                tail = &node->next;
            }
        }
        const_decl = const_decl->next;
    }
}

int select_range_primitive_tag(const TypeInfo *info)
{
    if (info == NULL || !info->is_range)
        return INT_TYPE;

    if (!info->range_known)
        return INT_TYPE;

    long long start = info->range_start;
    long long end = info->range_end;
    if (start >= 0 && end < 0)
    {
        unsigned long long u_end = (unsigned long long)end;
        if (u_end <= 0xFFULL)
            return BYTE_TYPE;
        if (u_end <= 0xFFFFULL)
            return WORD_TYPE;
        if (u_end <= 0xFFFFFFFFULL)
            return LONGWORD_TYPE;
        return QWORD_TYPE;
    }

    if (start > end) {
        long long tmp = start;
        start = end;
        end = tmp;
    }

    if (start >= 0)
    {
        if (end <= 0xFF)
            return BYTE_TYPE;
        if (end <= 0xFFFF)
            return WORD_TYPE;
        if (end <= 0xFFFFFFFFLL)
            return LONGWORD_TYPE;
        return QWORD_TYPE;
    }

    if (start >= INT_MIN && end <= INT_MAX)
        return INT_TYPE;

    /* Signed range exceeds 32-bit, use 64-bit Int64 */
    return INT64_TYPE;
}


long long compute_range_storage_size(const TypeInfo *info)
{
    if (info == NULL || !info->is_range || !info->range_known)
        return 0;

    long long start = info->range_start;
    long long end = info->range_end;
    if (start >= 0 && end < 0)
    {
        unsigned long long u_end = (unsigned long long)end;
        if (u_end <= 0xFFULL)
            return 1;
        if (u_end <= 0xFFFFULL)
            return 2;
        if (u_end <= 0xFFFFFFFFULL)
            return 4;
        return 8;
    }

    if (start > end)
    {
        long long tmp = start;
        start = end;
        end = tmp;
    }

    if (start >= 0)
    {
        if (end <= 0xFF)
            return 1;
        if (end <= 0xFFFF)
            return 2;
        if (end <= 0xFFFFFFFFLL)
            return 4;
        return 8;
    }

    /* Signed ranges */
    if (start >= -128 && end <= 127)
        return 1;
    if (start >= -32768 && end <= 32767)
        return 2;
    if (start >= INT_MIN && end <= INT_MAX)
        return 4;

    return 8;
}

static Tree_t *convert_type_decl_ex(ast_t *type_decl_node, ListNode_t **method_clones, ListNode_t **nested_type_decls_out) {
    if (type_decl_node == NULL)
        return NULL;

    if (nested_type_decls_out != NULL)
        *nested_type_decls_out = NULL;

    type_decl_node = unwrap_pascal_node(type_decl_node);
    if (type_decl_node == NULL)
        return NULL;

    ast_t *id_node = type_decl_node->child;
    while (id_node != NULL && id_node->typ != PASCAL_T_IDENTIFIER)
        id_node = id_node->next;
    if (id_node == NULL)
        return NULL;

    char *id = dup_symbol(id_node);
    if (id == NULL)
        return NULL;

    ast_t *spec_node = id_node->next;
    while (spec_node != NULL && spec_node->typ != PASCAL_T_TYPE_SPEC &&
           spec_node->typ != PASCAL_T_RECORD_TYPE && spec_node->typ != PASCAL_T_OBJECT_TYPE &&
           spec_node->typ != PASCAL_T_CLASS_TYPE &&
           spec_node->typ != PASCAL_T_INTERFACE_TYPE &&
           spec_node->typ != PASCAL_T_PROCEDURE_TYPE &&
           spec_node->typ != PASCAL_T_FUNCTION_TYPE &&
           spec_node->typ != PASCAL_T_REFERENCE_TO_TYPE) {
        spec_node = spec_node->next;
    }

    char *type_id = NULL;
    struct RecordType *record_type = NULL;
    TypeInfo type_info = {0};
    int mapped_type = UNKNOWN_TYPE;
    ast_t *class_spec = NULL;
    ast_t *interface_spec = NULL;
    ListNode_t *nested_type_sections = NULL;
    if (spec_node != NULL) {
        const char *trace_sym = kgpc_getenv("KGPC_TRACE_NONLOCAL");
        if (trace_sym != NULL && id != NULL && pascal_identifier_equals(id, trace_sym))
        {
            ast_t *dbg = spec_node;
            fprintf(stderr,
                "[KGPC_TRACE_NONLOCAL] raw_type_decl id=%s spec typ=%d sym=%s child_typ=%d child_sym=%s next_typ=%d next_sym=%s\n",
                id,
                dbg != NULL ? dbg->typ : -1,
                (dbg != NULL && dbg->sym != NULL && dbg->sym->name != NULL) ? dbg->sym->name : "<null>",
                (dbg != NULL && dbg->child != NULL) ? dbg->child->typ : -1,
                (dbg != NULL && dbg->child != NULL && dbg->child->sym != NULL && dbg->child->sym->name != NULL) ? dbg->child->sym->name : "<null>",
                (dbg != NULL && dbg->next != NULL) ? dbg->next->typ : -1,
                (dbg != NULL && dbg->next != NULL && dbg->next->sym != NULL && dbg->next->sym->name != NULL) ? dbg->next->sym->name : "<null>");
            if (dbg != NULL && dbg->child != NULL)
            {
                fprintf(stderr,
                    "[KGPC_TRACE_NONLOCAL] raw_type_decl child child_typ=%d child_sym=%s next_typ=%d next_sym=%s\n",
                    dbg->child->child != NULL ? dbg->child->child->typ : -1,
                    (dbg->child->child != NULL && dbg->child->child->sym != NULL && dbg->child->child->sym->name != NULL) ? dbg->child->child->sym->name : "<null>",
                    dbg->child->next != NULL ? dbg->child->next->typ : -1,
                    (dbg->child->next != NULL && dbg->child->next->sym != NULL && dbg->child->next->sym->name != NULL) ? dbg->child->next->sym->name : "<null>");
            }
        }
        if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
            fprintf(stderr, "[KGPC] convert_type_decl spec_node typ=%d sym=%s for id=%s\n",
                spec_node->typ,
                (spec_node->sym != NULL && spec_node->sym->name != NULL) ? spec_node->sym->name : "<null>",
                id != NULL ? id : "<null>");
        if (spec_node->typ == PASCAL_T_CLASS_TYPE) {
            class_spec = spec_node;
        } else if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL &&
                   spec_node->child->typ == PASCAL_T_CLASS_TYPE) {
            class_spec = spec_node->child;
        }
        /* Handle "class of T" - check if TYPE_SPEC wraps CLASS_OF_TYPE */
        else if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL &&
                 spec_node->child->typ == PASCAL_T_CLASS_OF_TYPE) {
            /* For class of T, we need to find T's record type and use it */
            ast_t *class_of_node = spec_node->child;
            ast_t *target_name = class_of_node->child;
            while (target_name != NULL && target_name->typ != PASCAL_T_IDENTIFIER)
                target_name = target_name->next;
            if (target_name != NULL && kgpc_getenv("KGPC_DEBUG_TFPG") != NULL) {
                fprintf(stderr, "[KGPC] convert_type_decl: class of target=%s for id=%s\n",
                    (target_name->sym && target_name->sym->name) ? target_name->sym->name : "<null>",
                    id);
            }
        }

        if (spec_node->typ == PASCAL_T_INTERFACE_TYPE) {
            interface_spec = spec_node;
        } else if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL &&
                   spec_node->child->typ == PASCAL_T_INTERFACE_TYPE) {
            interface_spec = spec_node->child;
        }

        if (interface_spec != NULL) {
            record_type = convert_interface_type_ex(id, interface_spec, &nested_type_sections);
            if (nested_type_sections != NULL && nested_type_decls_out != NULL) {
                ListNode_t *section_cursor = nested_type_sections;
                while (section_cursor != NULL) {
                    ast_t *type_section_ast = (ast_t *)section_cursor->cur;
                    if (type_section_ast != NULL) {
                        append_type_decls_from_section(type_section_ast, nested_type_decls_out,
                            NULL, NULL, NULL, id);
                    }
                    section_cursor = section_cursor->next;
                }
            }
            while (nested_type_sections != NULL) {
                ListNode_t *next = nested_type_sections->next;
                free(nested_type_sections);
                nested_type_sections = next;
            }
        } else if (class_spec != NULL) {
            record_type = convert_class_type_ex(id, class_spec, &nested_type_sections);
            /* Process nested type sections - extract and convert nested type declarations */
            if (nested_type_sections != NULL && nested_type_decls_out != NULL) {
                ListNode_t *section_cursor = nested_type_sections;
                while (section_cursor != NULL) {
                    ast_t *type_section_ast = (ast_t *)section_cursor->cur;
                    if (type_section_ast != NULL) {
                        /* Recursively process this nested type section */
                        append_type_decls_from_section(type_section_ast, nested_type_decls_out,
                            NULL, NULL, NULL, id);
                        if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL)
                            fprintf(stderr, "[KGPC] convert_type_decl: processed nested TYPE_SECTION for class %s\n", id);
                    }
                    section_cursor = section_cursor->next;
                }
            }
            /* Clean up the section list (don't destroy AST nodes, just the list) */
            while (nested_type_sections != NULL) {
                ListNode_t *next = nested_type_sections->next;
                free(nested_type_sections);
                nested_type_sections = next;
            }
        } else {
            /* Check if this is a record type - also extract nested types */
            ast_t *record_spec = NULL;
            if (spec_node->typ == PASCAL_T_RECORD_TYPE || spec_node->typ == PASCAL_T_OBJECT_TYPE) {
                record_spec = spec_node;
            } else if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL &&
                       (spec_node->child->typ == PASCAL_T_RECORD_TYPE || spec_node->child->typ == PASCAL_T_OBJECT_TYPE)) {
                record_spec = spec_node->child;
            }

            if (record_spec != NULL) {
                /* Handle record type with nested type extraction */
                record_type = convert_record_type_ex(record_spec, &nested_type_sections);
                mapped_type = RECORD_TYPE;
                if (nested_type_sections != NULL && nested_type_decls_out != NULL) {
                    ListNode_t *section_cursor = nested_type_sections;
                    while (section_cursor != NULL) {
                        ast_t *type_section_ast = (ast_t *)section_cursor->cur;
                        if (type_section_ast != NULL) {
                            append_type_decls_from_section(type_section_ast, nested_type_decls_out,
                                NULL, NULL, NULL, id);
                            if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL)
                                fprintf(stderr, "[KGPC] convert_type_decl: processed nested TYPE_SECTION for record %s\n", id);
                        }
                        section_cursor = section_cursor->next;
                    }
                }
                while (nested_type_sections != NULL) {
                    ListNode_t *next = nested_type_sections->next;
                    free(nested_type_sections);
                    nested_type_sections = next;
                }
            } else {
        mapped_type = convert_type_spec(spec_node, &type_id, &record_type, &type_info);
        /* Preserve qualified type names for alias targets like ObjPas.TEndian. */
        {
            ast_t *spec_child = spec_node;
            if (spec_child != NULL && spec_child->typ == PASCAL_T_TYPE_SPEC && spec_child->child != NULL)
                spec_child = spec_child->child;
            spec_child = unwrap_pascal_node(spec_child);
            if (spec_child != NULL &&
                (spec_child->typ == PASCAL_T_IDENTIFIER ||
                 spec_child->typ == PASCAL_T_QUALIFIED_IDENTIFIER ||
                 spec_child->typ == PASCAL_T_MEMBER_ACCESS))
            {
                QualifiedIdent *qid = qualified_ident_from_ast(spec_child);
                if (qid != NULL)
                {
                    if (type_info.type_ref == NULL)
                        type_info.type_ref = type_ref_create(qualified_ident_clone(qid), NULL, 0);
                    if (qid->count > 1)
                    {
                        char *qualified = qualified_ident_join(qid, ".");
                        if (qualified != NULL)
                        {
                            free(type_id);
                            type_id = qualified;
                        }
                    }
                    qualified_ident_free(qid);
                }
            }
        }
            }
            if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
                fprintf(stderr, "[KGPC] convert_type_decl after convert_type_spec id=%s mapped=%d type_id=%s record_type=%p type_info.record=%p\n",
                    id, mapped_type,
                    type_id != NULL ? type_id : "<null>",
                    (void *)record_type, (void *)type_info.record_type);
        }
    }

    KgpcType *kgpc_type = NULL;
    if (record_type != NULL) {
        KgpcType *rec_type = create_record_type(record_type);
        if (record_type->is_class || record_type->is_interface) {
            /* Classes and interfaces are pointers to records */
            kgpc_type = create_pointer_type(rec_type);
            kgpc_type_release(rec_type);
        } else {
            kgpc_type = rec_type;
        }
    } else if (spec_node != NULL) {
        kgpc_type = convert_type_spec_to_kgpctype(spec_node, NULL);
    }

    Tree_t *decl = NULL;
    if (record_type != NULL) {
        /* Set the type ID for the record */
        if (record_type->type_id == NULL && id != NULL)
            record_type->type_id = strdup(id);

        if (record_type->is_type_helper && record_type->helper_base_type_id != NULL &&
            record_type->type_id != NULL)
        {
            if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL)
                fprintf(stderr, "[KGPC] Registering type helper mapping: %s -> %s\n",
                    record_type->type_id, record_type->helper_base_type_id);
            register_type_helper_mapping(record_type->type_id, record_type->helper_base_type_id);
        }
        
        /* For advanced records, register any method declarations */
        /* Walk the fields list looking for stored method AST nodes */
        ListNode_t *field_cur = record_type->fields;
        while (field_cur != NULL) {
            if (field_cur->type == LIST_UNSPECIFIED) {
                /* This is a method AST node stored during convert_record_members */
                ast_t *method_ast = (ast_t *)field_cur->cur;
                if (method_ast != NULL && (method_ast->typ == PASCAL_T_METHOD_DECL ||
                    method_ast->typ == PASCAL_T_CONSTRUCTOR_DECL ||
                    method_ast->typ == PASCAL_T_DESTRUCTOR_DECL)) {
                    struct MethodTemplate *template = create_method_template(method_ast);
                    if (template != NULL) {
                        if (!template->is_interface_delegation) {
                            int param_count = from_cparser_count_params_ast(template->params_ast);
                            char *param_sig = param_type_signature_from_params_ast(template->params_ast);
                            register_class_method_ex(id, template->name,
                                template->is_virtual, template->is_override, template->is_static,
                                template->is_class_method,
                                param_count, param_sig);
                        }
                        if (kgpc_getenv("KGPC_DEBUG_CLASS_METHODS") != NULL)
                            fprintf(stderr, "[KGPC] Registered record method %s.%s (static=%d)\n",
                                id, template->name, template->is_static);
                        destroy_method_template_instance(template);
                    }
                }
            }
            field_cur = field_cur->next;
        }

        if (record_type->is_type_helper && record_type->method_templates != NULL && id != NULL)
        {
            ListNode_t *tmpl_cur = record_type->method_templates;
            while (tmpl_cur != NULL)
            {
                if (tmpl_cur->type == LIST_METHOD_TEMPLATE)
                {
                    struct MethodTemplate *template = (struct MethodTemplate *)tmpl_cur->cur;
                    if (template != NULL)
                    {
                        if (template->is_class_method)
                            template->is_static = 1;
                        if (!template->is_interface_delegation) {
                            int param_count = from_cparser_count_params_ast(template->params_ast);
                            char *param_sig = param_type_signature_from_params_ast(template->params_ast);
                            register_class_method_ex(id, template->name,
                                template->is_virtual, template->is_override, template->is_static,
                                template->is_class_method,
                                param_count, param_sig);
                        }
                        if (kgpc_getenv("KGPC_DEBUG_CLASS_METHODS") != NULL)
                            fprintf(stderr, "[KGPC] Registered helper method %s.%s (static=%d)\n",
                                id, template->name, template->is_static);
                    }
                }
                tmpl_cur = tmpl_cur->next;
            }
        }
        
        /* Direct record/class type declaration */
        decl = mk_record_type(type_decl_node->line, id, record_type);
    } else if (type_info.is_array) {
        decl = mk_typealiasdecl(type_decl_node->line, id, 1, type_info.element_type,
                                 type_info.element_type_id, type_info.start, type_info.end);
        if (decl != NULL) {
            decl->tree_data.type_decl_data.info.alias.is_shortstring =
                type_info.is_shortstring ||
                (id != NULL && pascal_identifier_equals(id, "ShortString"));
            if (type_info.element_kgpc_type != NULL) {
                long long esize = kgpc_type_sizeof(type_info.element_kgpc_type);
                if (esize > 0 && esize <= INT_MAX)
                    decl->tree_data.type_decl_data.info.alias.array_element_storage_size = (int)esize;
            }
        }
        type_info.element_type_id = NULL;
    } else if (type_info.is_record && type_id != NULL) {
        /* Alias to a record type (including generic specializations) */
        decl = mk_typealiasdecl(type_decl_node->line, id, 0, RECORD_TYPE, type_id, 0, 0);
        type_id = NULL;
    } else if (mapped_type != UNKNOWN_TYPE || type_id != NULL) {
        decl = mk_typealiasdecl(type_decl_node->line, id, 0, mapped_type, type_id, 0, 0);
        type_id = NULL;
    } else if (type_info.is_range) {
        int base_tag = select_range_primitive_tag(&type_info);
        decl = mk_typealiasdecl(type_decl_node->line, id, 0, base_tag, NULL, 0, 0);
    } else {
        decl = mk_typedecl(type_decl_node->line, id, 0, 0);
    }

    if (decl != NULL)
    {
        const char *trace_sym = kgpc_getenv("KGPC_TRACE_NONLOCAL");
        if (trace_sym != NULL && id != NULL && pascal_identifier_equals(id, trace_sym) &&
            decl->type == TREE_TYPE_DECL &&
            decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
        {
            struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
            fprintf(stderr,
                "[KGPC_TRACE_NONLOCAL] parse_type_decl id=%s base=%d target=%s is_array=%d [%d,%d] short=%d kgpc=%p\n",
                id,
                alias->base_type,
                alias->target_type_id != NULL ? alias->target_type_id : "<null>",
                alias->is_array,
                alias->array_start,
                alias->array_end,
                alias->is_shortstring,
                (void *)decl->tree_data.type_decl_data.kgpc_type);
        }
        decl->tree_data.type_decl_data.kgpc_type = kgpc_type;
        if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL &&
            decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
            decl->tree_data.type_decl_data.id != NULL)
        {
            fprintf(stderr, "[KGPC] convert_type_decl alias %s: base=%d target=%s kgpc=%p kind=%d type_info.is_record=%d record=%p generic=%d\n",
                decl->tree_data.type_decl_data.id,
                decl->tree_data.type_decl_data.info.alias.base_type,
                decl->tree_data.type_decl_data.info.alias.target_type_id ?
                    decl->tree_data.type_decl_data.info.alias.target_type_id : "<null>",
                (void *)decl->tree_data.type_decl_data.kgpc_type,
                decl->tree_data.type_decl_data.kgpc_type ?
                    decl->tree_data.type_decl_data.kgpc_type->kind : -1,
                type_info.is_record,
                (void *)type_info.record_type,
                type_info.is_generic_specialization);
        }
    }
    else if (kgpc_type != NULL)
        destroy_kgpc_type(kgpc_type);

    if (decl != NULL && decl->type == TREE_TYPE_DECL &&
        decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
        struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
        /* Set the alias name */
        if (decl->tree_data.type_decl_data.id != NULL && alias->alias_name == NULL) {
            alias->alias_name = strdup(decl->tree_data.type_decl_data.id);
        }
        if (type_info.array_dimensions != NULL) {
            alias->array_dimensions = type_info.array_dimensions;
            type_info.array_dimensions = NULL;
        }
        if (type_info.array_dims_parsed) {
            alias->array_dim_start_str = type_info.array_dim_start_str;
            alias->array_dim_end_str = type_info.array_dim_end_str;
            alias->array_dims_parsed = 1;
            type_info.array_dim_start_str = NULL;
            type_info.array_dim_end_str = NULL;
        }
        if (type_info.type_ref != NULL) {
            type_ref_free(alias->target_type_ref);
            alias->target_type_ref = type_info.type_ref;
            type_info.type_ref = NULL;
        }
        if (type_info.range_start_str != NULL) {
            free(alias->range_start_str);
            alias->range_start_str = type_info.range_start_str;
            type_info.range_start_str = NULL;
        }
        if (type_info.range_end_str != NULL) {
            free(alias->range_end_str);
            alias->range_end_str = type_info.range_end_str;
            type_info.range_end_str = NULL;
        }
        if (type_info.element_type_ref != NULL) {
            type_ref_free(alias->array_element_type_ref);
            alias->array_element_type_ref = type_info.element_type_ref;
            type_info.element_type_ref = NULL;
        }
        alias->is_pointer = type_info.is_pointer;
        alias->is_class_reference = type_info.is_class_reference;
        alias->pointer_type = type_info.pointer_type;
        if (type_info.pointer_type_id != NULL) {
            alias->pointer_type_id = type_info.pointer_type_id;
            type_info.pointer_type_id = NULL;
        }
        if (type_info.pointer_type_ref != NULL) {
            type_ref_free(alias->pointer_type_ref);
            alias->pointer_type_ref = type_info.pointer_type_ref;
            type_info.pointer_type_ref = NULL;
        }
        alias->is_set = type_info.is_set;
        alias->set_element_type = type_info.set_element_type;
        if (type_info.set_element_type_id != NULL) {
            alias->set_element_type_id = type_info.set_element_type_id;
            type_info.set_element_type_id = NULL;
        }
        if (type_info.set_element_type_ref != NULL) {
            type_ref_free(alias->set_element_type_ref);
            alias->set_element_type_ref = type_info.set_element_type_ref;
            type_info.set_element_type_ref = NULL;
        }
        if (type_info.is_set && type_info.range_known) {
            alias->range_known = 1;
            alias->range_start = type_info.range_start;
            alias->range_end = type_info.range_end;
        }
        alias->is_enum_set = type_info.is_enum_set;
        if (type_info.inline_enum_values != NULL) {
            alias->inline_enum_values = type_info.inline_enum_values;
            type_info.inline_enum_values = NULL;
        }
        alias->is_enum = type_info.is_enum;
        alias->enum_is_scoped = type_info.enum_is_scoped;
        alias->enum_has_explicit_values = type_info.enum_has_explicit_values;
        if (type_info.enum_literals != NULL) {
            alias->enum_literals = type_info.enum_literals;
            type_info.enum_literals = NULL;
        }
        alias->is_file = type_info.is_file;
        alias->file_type = type_info.file_type;
        if (type_info.file_type_id != NULL) {
            alias->file_type_id = type_info.file_type_id;
            type_info.file_type_id = NULL;
        }
        if (type_info.file_type_ref != NULL) {
            type_ref_free(alias->file_type_ref);
            alias->file_type_ref = type_info.file_type_ref;
            type_info.file_type_ref = NULL;
        }
        if (type_info.is_record && type_info.record_type != NULL) {
            alias->inline_record_type = type_info.record_type;
            type_info.record_type = NULL;
            if (alias->inline_record_type->is_class) {
                KgpcType *rec = create_record_type(alias->inline_record_type);
                KgpcType *inline_type = create_pointer_type(rec);
                kgpc_type_release(rec);
                if (decl->tree_data.type_decl_data.kgpc_type != NULL)
                    destroy_kgpc_type(decl->tree_data.type_decl_data.kgpc_type);
                decl->tree_data.type_decl_data.kgpc_type = inline_type;
            } else if (decl->tree_data.type_decl_data.kgpc_type == NULL) {
                decl->tree_data.type_decl_data.kgpc_type =
                    create_record_type(alias->inline_record_type);
            }
        }
        if (type_info.is_range) {
            alias->is_range = 1;
            alias->range_known = type_info.range_known;
            alias->range_start = type_info.range_start;
            alias->range_end = type_info.range_end;
            alias->storage_size = compute_range_storage_size(&type_info);
        }
        if (type_info.is_generic_specialization && type_info.record_type == NULL) {
            register_pending_generic_alias(decl, &type_info);
        }
    }

    if (type_id != NULL)
        free(type_id);
    destroy_type_info_contents(&type_info);

    if (decl == NULL) {
        free(id);
        destroy_record_type(record_type);
    }

    if (decl != NULL && method_clones != NULL)
        append_specialized_method_clones(decl, method_clones);

    return decl;
}

static Tree_t *convert_generic_type_decl(ast_t *type_decl_node) {
    if (type_decl_node == NULL)
        return NULL;

    type_decl_node = unwrap_pascal_node(type_decl_node);
    if (type_decl_node == NULL)
        return NULL;

    ast_t *id_node = type_decl_node->child;
    if (id_node == NULL)
        return NULL;

    char *id = dup_symbol(id_node);
    if (id == NULL)
        return NULL;

    ast_t *param_list = id_node->next;
    ast_t *type_spec_node = param_list != NULL ? param_list->next : NULL;
    if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
        fprintf(stderr, "[KGPC] convert_generic_type_decl %s (type_spec_node=%p typ=%d)\n",
            id, (void *)type_spec_node, type_spec_node != NULL ? type_spec_node->typ : -1);
    struct RecordType *record_template = NULL;

    int param_count = 0;
    char **param_names = NULL;
    if (param_list != NULL && param_list->typ == PASCAL_T_TYPE_PARAM_LIST) {
        /* First pass: count parameters */
        for (ast_t *param = param_list->child; param != NULL; param = param->next) {
            ast_t *name_node = param;
            if (name_node->typ == PASCAL_T_TYPE_PARAM && name_node->child != NULL)
                name_node = name_node->child;
            while (name_node != NULL && name_node->typ != PASCAL_T_IDENTIFIER)
                name_node = name_node->next;
            if (name_node != NULL && name_node->sym != NULL)
                param_count++;
        }

        if (param_count > 0) {
            param_names = (char **)calloc((size_t)param_count, sizeof(char *));
            if (param_names == NULL) {
                free(id);
                return NULL;
            }

            int index = 0;
            for (ast_t *param = param_list->child; param != NULL && index < param_count; param = param->next) {
                ast_t *name_node = param;
                if (name_node->typ == PASCAL_T_TYPE_PARAM && name_node->child != NULL)
                    name_node = name_node->child;
                while (name_node != NULL && name_node->typ != PASCAL_T_IDENTIFIER)
                    name_node = name_node->next;
                if (name_node != NULL && name_node->sym != NULL) {
                    param_names[index] = strdup(name_node->sym->name);
                    if (param_names[index] == NULL) {
                        /* Cleanup previously allocated names */
                        for (int i = 0; i < index; ++i)
                            free(param_names[i]);
                        free(param_names);
                        free(id);
                        return NULL;
                    }
                    index++;
                }
            }
            param_count = index;
        }
    }

    ListNode_t *generic_nested_types = NULL;
    if (type_spec_node != NULL) {
        ast_t *spec_body = type_spec_node;
        if (spec_body->typ == PASCAL_T_TYPE_SPEC && spec_body->child != NULL)
            spec_body = spec_body->child;
        if (spec_body != NULL) {
            if (spec_body->typ == PASCAL_T_CLASS_TYPE)
            {
                ListNode_t *nested_type_sections = NULL;
                if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
                    fprintf(stderr, "[KGPC] generic class decl %s\n", id);
                record_template = convert_class_type_ex(id, spec_body, &nested_type_sections);
                /* Convert nested type sections into type declarations for later specialization */
                if (nested_type_sections != NULL) {
                    ListNode_t *section_cursor = nested_type_sections;
                    while (section_cursor != NULL) {
                        ast_t *type_section_ast = (ast_t *)section_cursor->cur;
                        if (type_section_ast != NULL) {
                            append_type_decls_from_section(type_section_ast, &generic_nested_types,
                                NULL, NULL, NULL, id);
                        }
                        section_cursor = section_cursor->next;
                    }
                    /* Clean up section list (not AST nodes) */
                    while (nested_type_sections != NULL) {
                        ListNode_t *next = nested_type_sections->next;
                        free(nested_type_sections);
                        nested_type_sections = next;
                    }
                }
            }
            else if (spec_body->typ == PASCAL_T_RECORD_TYPE || spec_body->typ == PASCAL_T_OBJECT_TYPE)
                record_template = convert_record_type(spec_body);
        }
    }

    Tree_t *decl = mk_typedecl(type_decl_node->line, id, 0, 0);
    if (decl == NULL) {
        if (param_names != NULL) {
            for (int i = 0; i < param_count; ++i)
                free(param_names[i]);
            free(param_names);
        }
        free(id);
        destroy_record_type(record_template);
        return NULL;
    }

    decl->tree_data.type_decl_data.kind = TYPE_DECL_GENERIC;
    decl->tree_data.type_decl_data.info.generic.type_parameters = param_names;
    decl->tree_data.type_decl_data.info.generic.num_type_params = param_count;
    decl->tree_data.type_decl_data.info.generic.original_ast = NULL;
    if (type_spec_node != NULL)
        decl->tree_data.type_decl_data.info.generic.original_ast = copy_ast_detached(type_spec_node);
    decl->tree_data.type_decl_data.info.generic.record_template = record_template;

    /* Register the generic declaration for future specialization */
    GenericTypeDecl *generic_decl = generic_registry_add_decl(id, param_names, param_count, decl);
    if (generic_decl != NULL && generic_nested_types != NULL) {
        generic_decl->nested_type_decls = generic_nested_types;
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL) {
            int count = 0;
            ListNode_t *cur = generic_nested_types;
            while (cur != NULL) { count++; cur = cur->next; }
            fprintf(stderr, "[KGPC] stored %d nested type decls for generic %s\n", count, id);
        }
    }

    (void)type_spec_node; /* Placeholder for future template storage */

    return decl;
}



void convert_routine_body(ast_t *body_node, ListNode_t **const_decls,
                                 ListBuilder *var_builder,
                                 ListBuilder *label_builder,
                                 ListNode_t **nested_subs,
                                 struct Statement **body_out,
                                 ListNode_t **type_decl_list) {
    if (body_node == NULL)
        return;

    ast_t *type_section_ast = NULL;  /* Track local type section for enum resolution */
    
    ast_t *cursor = body_node->child;
    while (cursor != NULL) {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node != NULL) {
            switch (node->typ) {
            case PASCAL_T_TYPE_SECTION:
                if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
                    fprintf(stderr, "[KGPC] convert_routine_body TYPE_SECTION at line=%d\n", node->line);
                }
                if (type_decl_list != NULL)
                    append_type_decls_from_section(node, type_decl_list, nested_subs,
                        const_decls, var_builder, NULL);
                type_section_ast = node;  /* Save for const array enum resolution */
                break;
            case PASCAL_T_CONST_SECTION:
                append_const_decls_from_section(node, const_decls, var_builder, type_section_ast);
                break;
            case PASCAL_T_VAR_SECTION:
                list_builder_extend(var_builder, convert_var_section(node));
                break;
            case PASCAL_T_LABEL_SECTION:
                append_labels_from_section(node, label_builder);
                break;
            case PASCAL_T_PROCEDURE_DECL: {
                if (nested_subs != NULL) {
                    Tree_t *proc = convert_procedure(node);
                    append_subprogram_node(nested_subs, proc);
                }
                break;
            }
            case PASCAL_T_FUNCTION_DECL: {
                if (nested_subs != NULL) {
                    Tree_t *func = convert_function(node);
                    append_subprogram_node(nested_subs, func);
                }
                break;
            }
            case PASCAL_T_METHOD_IMPL: {
                if (nested_subs != NULL) {
                    Tree_t *method_tree = convert_method_impl(node);
                    append_subprogram_node(nested_subs, method_tree);
                }
                break;
            }
            case PASCAL_T_BEGIN_BLOCK:
            case PASCAL_T_MAIN_BLOCK:
                if (body_out != NULL)
                    *body_out = convert_block(node);
                break;
            case PASCAL_T_ASM_BLOCK: {
                if (body_out != NULL) {
                    struct Statement *stmt = convert_statement(node);
                    ListBuilder stmts_builder;
                    list_builder_init(&stmts_builder);
                    if (stmt != NULL)
                        list_builder_append(&stmts_builder, stmt, LIST_STMT);
                    *body_out = mk_compoundstatement(node->line, list_builder_finish(&stmts_builder));
                }
                break;
            }
            default:
                break;
            }
        }
        cursor = cursor->next;
    }
}

void append_uses_from_section(ast_t *uses_node, ListNode_t **dest) {
    if (uses_node == NULL || dest == NULL)
        return;

    ListNode_t **tail = dest;
    while (*tail != NULL)
        tail = &(*tail)->next;

    /* Create visited set to detect circular references */
    VisitedSet *visited = visited_set_create();
    if (visited == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate visited set for uses traversal\n");
        return;
    }

    ast_t *unit = uses_node->child;
    while (unit != NULL) {
        /* Check for circular reference before processing */
        if (!is_safe_to_continue(visited, unit)) {
            fprintf(stderr, "ERROR: Circular reference detected in uses section, stopping traversal\n");
            break;
        }
        
        if (unit->typ == PASCAL_T_USES_UNIT) {
            char *dup = dup_symbol(unit);
            if (dup != NULL) {
                ListNode_t *node = CreateListNode(dup, LIST_STRING);
                *tail = node;
                tail = &node->next;
            }
        }
        unit = unit->next;
    }
    
    visited_set_destroy(visited);
}

void append_labels_from_section(ast_t *label_node, ListBuilder *builder) {
    if (label_node == NULL || builder == NULL)
        return;

    /* Create visited set to detect circular references */
    VisitedSet *visited = visited_set_create();
    if (visited == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate visited set for labels traversal\n");
        return;
    }

    ast_t *cur = label_node->child;
    while (cur != NULL) {
        /* Check for circular reference before processing */
        if (!is_safe_to_continue(visited, cur)) {
            fprintf(stderr, "ERROR: Circular reference detected in label section, stopping traversal\n");
            break;
        }
        
        ast_t *node = unwrap_pascal_node(cur);
        if (node == NULL)
            node = cur;

        if (node != NULL && (node->typ == PASCAL_T_INTEGER || node->typ == PASCAL_T_IDENTIFIER)) {
            char *label = dup_symbol(node);
            if (label != NULL)
                list_builder_append(builder, label, LIST_STRING);
        }
        cur = cur->next;
    }
    
    visited_set_destroy(visited);
}

static char *mangle_helper_const_name(const char *helper_id, const char *const_id)
{
    if (helper_id == NULL || const_id == NULL)
        return NULL;
    size_t len = strlen(helper_id) + strlen(const_id) + 3;
    char *name = (char *)malloc(len);
    if (name == NULL)
        return NULL;
    snprintf(name, len, "%s__%s", helper_id, const_id);
    return name;
}

typedef struct {
    char *original;
    char *mangled;
} ClassConstMap;

static const char *lookup_class_const_map(ListNode_t *map, const char *name)
{
    if (map == NULL || name == NULL)
        return NULL;
    for (ListNode_t *cur = map; cur != NULL; cur = cur->next) {
        ClassConstMap *entry = (ClassConstMap *)cur->cur;
        if (entry != NULL && entry->original != NULL &&
            pascal_identifier_equals(entry->original, name)) {
            return entry->mangled;
        }
    }
    return NULL;
}

static int type_name_exists_in_sections(const char *name, ListNode_t *sections)
{
    if (name == NULL || sections == NULL)
        return 0;
    for (ListNode_t *cur = sections; cur != NULL; cur = cur->next)
    {
        if (cur->cur == NULL)
            continue;
        ast_t *section = (ast_t *)cur->cur;
        if (type_name_exists_in_section(name, section))
            return 1;
    }
    return 0;
}

static void qualify_expr_type_id(struct Expression *expr, const char *owner_id)
{
    if (expr == NULL || owner_id == NULL)
        return;
    if (expr->expr_data.id == NULL)
        return;
    if (expr->id_ref != NULL && expr->id_ref->count > 1)
        return;

    char *base_name = strdup(expr->expr_data.id);
    if (base_name == NULL)
        return;
    size_t len = strlen(owner_id) + 1 + strlen(base_name) + 1;
    char *qualified = (char *)malloc(len);
    if (qualified == NULL)
    {
        free(base_name);
        return;
    }
    snprintf(qualified, len, "%s.%s", owner_id, base_name);
    free(expr->expr_data.id);
    expr->expr_data.id = qualified;

    if (expr->id_ref != NULL)
        qualified_ident_free(expr->id_ref);
    char **segments = (char **)calloc(2, sizeof(char *));
    if (segments == NULL)
    {
        free(base_name);
        return;
    }
    segments[0] = strdup(owner_id);
    segments[1] = base_name;
    if (segments[0] == NULL || segments[1] == NULL)
    {
        free(segments[0]);
        free(segments[1]);
        free(segments);
        return;
    }
    expr->id_ref = qualified_ident_from_segments(segments, 2, 1);
}

static void qualify_type_ref_id(char **type_id, struct TypeRef **type_ref,
    const char *owner_id, const char *base_name)
{
    if (type_id == NULL || owner_id == NULL || base_name == NULL)
        return;
    char *base_dup = strdup(base_name);
    if (base_dup == NULL)
        return;
    size_t len = strlen(owner_id) + 1 + strlen(base_name) + 1;
    char *qualified = (char *)malloc(len);
    if (qualified == NULL)
    {
        free(base_dup);
        return;
    }
    snprintf(qualified, len, "%s.%s", owner_id, base_name);
    free(*type_id);
    *type_id = qualified;
    if (type_ref != NULL)
    {
        if (*type_ref != NULL)
            type_ref_free(*type_ref);
        char **segments = (char **)calloc(2, sizeof(char *));
        if (segments == NULL)
        {
            free(base_dup);
            return;
        }
        segments[0] = strdup(owner_id);
        segments[1] = base_dup;
        if (segments[0] == NULL || segments[1] == NULL)
        {
            free(segments[0]);
            free(segments[1]);
            free(segments);
            return;
        }
        QualifiedIdent *qid = qualified_ident_from_segments(segments, 2, 1);
        *type_ref = type_ref_create(qid, NULL, 0);
    }
    else
    {
        free(base_dup);
    }
}

static int type_id_is_qualified(const char *type_id, const struct TypeRef *type_ref)
{
    (void)type_id;
    if (type_ref != NULL && type_ref->name != NULL && type_ref->name->count > 1)
        return 1;
    return 0;
}

static struct TypeRef *ensure_type_ref_from_id(char **type_id, struct TypeRef **type_ref);

static void qualify_param_type_id(char **type_id, struct TypeRef **type_ref,
    const char *owner_full, const char *owner_outer, SymTab_t *symtab)
{
    if (type_id == NULL || *type_id == NULL || symtab == NULL)
        return;
    if (type_ref != NULL && *type_ref == NULL)
        ensure_type_ref_from_id(type_id, type_ref);
    if (type_id_is_qualified(*type_id, type_ref != NULL ? *type_ref : NULL))
        return;
    if (map_type_name(*type_id, NULL) != UNKNOWN_TYPE)
        return;

    const char *owners[2] = { owner_full, owner_outer };
    for (size_t i = 0; i < 2; ++i)
    {
        const char *owner = owners[i];
        if (owner == NULL)
            continue;
        size_t len = strlen(owner) + 1 + strlen(*type_id) + 1;
        char *qualified = (char *)malloc(len);
        if (qualified == NULL)
            continue;
        snprintf(qualified, len, "%s.%s", owner, *type_id);
        HashNode_t *node = NULL;
        if (FindSymbol(&node, symtab, qualified) != 0 &&
            node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            free(qualified);
            qualify_type_ref_id(type_id, type_ref, owner, *type_id);
            break;
        }
        free(qualified);
    }
}

void qualify_param_decl_types(ListNode_t *params,
    const char *owner_full, const char *owner_outer, SymTab_t *symtab)
{
    if (params == NULL || symtab == NULL)
        return;
    for (ListNode_t *cur = params; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl->type == TREE_VAR_DECL)
        {
            qualify_param_type_id(&decl->tree_data.var_decl_data.type_id,
                &decl->tree_data.var_decl_data.type_ref,
                owner_full, owner_outer, symtab);
        }
        else if (decl->type == TREE_ARR_DECL)
        {
            qualify_param_type_id(&decl->tree_data.arr_decl_data.type_id,
                &decl->tree_data.arr_decl_data.type_ref,
                owner_full, owner_outer, symtab);
        }
    }
}

static void rewrite_class_const_expr_nested_types(struct Expression *expr,
    const char *owner_id, ListNode_t *nested_type_sections)
{
    if (expr == NULL || owner_id == NULL || nested_type_sections == NULL)
        return;

    switch (expr->type) {
        case EXPR_FUNCTION_CALL: {
            const char *func_id = expr->expr_data.function_call_data.id;
            /* A function call whose id matches a nested type is really a typecast
               (e.g. Node(nil^) where Node is a nested record type).  Qualify
               the id so the semantic checker can resolve it. */
            if (func_id != NULL &&
                type_name_exists_in_sections(func_id, nested_type_sections))
            {
                size_t olen = strlen(owner_id);
                size_t flen = strlen(func_id);
                char *qualified = (char *)malloc(olen + 1 + flen + 1);
                if (qualified != NULL)
                {
                    snprintf(qualified, olen + 1 + flen + 1, "%s.%s", owner_id, func_id);
                    free(expr->expr_data.function_call_data.id);
                    expr->expr_data.function_call_data.id = qualified;
                    func_id = qualified;
                }
            }
            int is_type_intrinsic = func_id != NULL &&
                (pascal_identifier_equals(func_id, "SizeOf") ||
                 pascal_identifier_equals(func_id, "TypeInfo") ||
                 pascal_identifier_equals(func_id, "IsManagedType") ||
                 pascal_identifier_equals(func_id, "High") ||
                 pascal_identifier_equals(func_id, "Low"));
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;
            for (ListNode_t *cur = args; cur != NULL; cur = cur->next) {
                if (cur->type != LIST_EXPR)
                    continue;
                struct Expression *arg = (struct Expression *)cur->cur;
                if (is_type_intrinsic && arg != NULL && arg->type == EXPR_VAR_ID &&
                    arg->expr_data.id != NULL &&
                    type_name_exists_in_sections(arg->expr_data.id, nested_type_sections))
                {
                    qualify_expr_type_id(arg, owner_id);
                }
                rewrite_class_const_expr_nested_types(arg, owner_id, nested_type_sections);
            }
            break;
        }
        case EXPR_TYPEINFO:
            if (expr->expr_data.typeinfo_data.type_id != NULL &&
                type_name_exists_in_sections(expr->expr_data.typeinfo_data.type_id,
                    nested_type_sections))
            {
                qualify_type_ref_id(&expr->expr_data.typeinfo_data.type_id,
                    &expr->expr_data.typeinfo_data.type_ref,
                    owner_id, expr->expr_data.typeinfo_data.type_id);
            }
            break;
        case EXPR_TYPECAST:
            rewrite_class_const_expr_nested_types(expr->expr_data.typecast_data.expr,
                owner_id, nested_type_sections);
            if (expr->expr_data.typecast_data.target_type_id != NULL &&
                expr->expr_data.typecast_data.type_qualifier == NULL &&
                type_name_exists_in_sections(expr->expr_data.typecast_data.target_type_id,
                    nested_type_sections))
            {
                qualify_type_ref_id(&expr->expr_data.typecast_data.target_type_id,
                    &expr->expr_data.typecast_data.target_type_ref,
                    owner_id, expr->expr_data.typecast_data.target_type_id);
            }
            break;
        case EXPR_IS:
            rewrite_class_const_expr_nested_types(expr->expr_data.is_data.expr,
                owner_id, nested_type_sections);
            if (expr->expr_data.is_data.target_type_id != NULL &&
                type_name_exists_in_sections(expr->expr_data.is_data.target_type_id,
                    nested_type_sections))
            {
                qualify_type_ref_id(&expr->expr_data.is_data.target_type_id,
                    &expr->expr_data.is_data.target_type_ref,
                    owner_id, expr->expr_data.is_data.target_type_id);
            }
            break;
        case EXPR_AS:
            rewrite_class_const_expr_nested_types(expr->expr_data.as_data.expr,
                owner_id, nested_type_sections);
            if (expr->expr_data.as_data.target_type_id != NULL &&
                type_name_exists_in_sections(expr->expr_data.as_data.target_type_id,
                    nested_type_sections))
            {
                qualify_type_ref_id(&expr->expr_data.as_data.target_type_id,
                    &expr->expr_data.as_data.target_type_ref,
                    owner_id, expr->expr_data.as_data.target_type_id);
            }
            break;
        case EXPR_ADDOP:
            rewrite_class_const_expr_nested_types(expr->expr_data.addop_data.left_expr,
                owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(expr->expr_data.addop_data.right_term,
                owner_id, nested_type_sections);
            break;
        case EXPR_MULOP:
            rewrite_class_const_expr_nested_types(expr->expr_data.mulop_data.left_term,
                owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(expr->expr_data.mulop_data.right_factor,
                owner_id, nested_type_sections);
            break;
        case EXPR_RELOP:
            rewrite_class_const_expr_nested_types(expr->expr_data.relop_data.left,
                owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(expr->expr_data.relop_data.right,
                owner_id, nested_type_sections);
            break;
        case EXPR_SIGN_TERM:
            rewrite_class_const_expr_nested_types(expr->expr_data.sign_term,
                owner_id, nested_type_sections);
            break;
        case EXPR_ARRAY_ACCESS:
            rewrite_class_const_expr_nested_types(expr->expr_data.array_access_data.array_expr,
                owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(expr->expr_data.array_access_data.index_expr,
                owner_id, nested_type_sections);
            for (ListNode_t *cur = expr->expr_data.array_access_data.extra_indices;
                 cur != NULL; cur = cur->next) {
                if (cur->type == LIST_EXPR)
                    rewrite_class_const_expr_nested_types((struct Expression *)cur->cur,
                        owner_id, nested_type_sections);
            }
            break;
        case EXPR_RECORD_ACCESS:
            rewrite_class_const_expr_nested_types(expr->expr_data.record_access_data.record_expr,
                owner_id, nested_type_sections);
            break;
        case EXPR_SET:
            for (ListNode_t *cur = expr->expr_data.set_data.elements; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_SET_ELEMENT && cur->cur != NULL) {
                    struct SetElement *elem = (struct SetElement *)cur->cur;
                    rewrite_class_const_expr_nested_types(elem->lower, owner_id, nested_type_sections);
                    rewrite_class_const_expr_nested_types(elem->upper, owner_id, nested_type_sections);
                }
            }
            break;
        case EXPR_ARRAY_LITERAL:
            for (ListNode_t *cur = expr->expr_data.array_literal_data.elements; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_EXPR)
                    rewrite_class_const_expr_nested_types((struct Expression *)cur->cur,
                        owner_id, nested_type_sections);
            }
            break;
        case EXPR_RECORD_CONSTRUCTOR:
            for (ListNode_t *cur = expr->expr_data.record_constructor_data.fields; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_UNSPECIFIED && cur->cur != NULL) {
                    struct RecordConstructorField *field = (struct RecordConstructorField *)cur->cur;
                    rewrite_class_const_expr_nested_types(field->value, owner_id, nested_type_sections);
                }
            }
            break;
        case EXPR_ADDR:
            rewrite_class_const_expr_nested_types(expr->expr_data.addr_data.expr,
                owner_id, nested_type_sections);
            break;
        case EXPR_POINTER_DEREF:
            rewrite_class_const_expr_nested_types(expr->expr_data.pointer_deref_data.pointer_expr,
                owner_id, nested_type_sections);
            break;
        default:
            break;
    }
}

static void rewrite_class_const_statement_nested_types(struct Statement *stmt,
    const char *owner_id, ListNode_t *nested_type_sections)
{
    if (stmt == NULL || owner_id == NULL || nested_type_sections == NULL)
        return;

    switch (stmt->type) {
        case STMT_VAR_ASSIGN:
            rewrite_class_const_expr_nested_types(stmt->stmt_data.var_assign_data.var,
                owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(stmt->stmt_data.var_assign_data.expr,
                owner_id, nested_type_sections);
            break;
        case STMT_COMPOUND_STATEMENT:
            for (ListNode_t *cur = stmt->stmt_data.compound_statement; cur != NULL; cur = cur->next)
            {
                if (cur->type == LIST_STMT)
                    rewrite_class_const_statement_nested_types((struct Statement *)cur->cur,
                        owner_id, nested_type_sections);
            }
            break;
        case STMT_IF_THEN:
            rewrite_class_const_expr_nested_types(stmt->stmt_data.if_then_data.relop_expr,
                owner_id, nested_type_sections);
            rewrite_class_const_statement_nested_types(stmt->stmt_data.if_then_data.if_stmt,
                owner_id, nested_type_sections);
            rewrite_class_const_statement_nested_types(stmt->stmt_data.if_then_data.else_stmt,
                owner_id, nested_type_sections);
            break;
        case STMT_WHILE:
            rewrite_class_const_expr_nested_types(stmt->stmt_data.while_data.relop_expr,
                owner_id, nested_type_sections);
            rewrite_class_const_statement_nested_types(stmt->stmt_data.while_data.while_stmt,
                owner_id, nested_type_sections);
            break;
        case STMT_REPEAT:
            for (ListNode_t *cur = stmt->stmt_data.repeat_data.body_list; cur != NULL; cur = cur->next)
            {
                if (cur->type == LIST_STMT)
                    rewrite_class_const_statement_nested_types((struct Statement *)cur->cur,
                        owner_id, nested_type_sections);
            }
            rewrite_class_const_expr_nested_types(stmt->stmt_data.repeat_data.until_expr,
                owner_id, nested_type_sections);
            break;
        case STMT_FOR:
            if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_ASSIGN_VAR)
                rewrite_class_const_statement_nested_types(stmt->stmt_data.for_data.for_assign_data.var_assign,
                    owner_id, nested_type_sections);
            else if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_VAR)
                rewrite_class_const_expr_nested_types(stmt->stmt_data.for_data.for_assign_data.var,
                    owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(stmt->stmt_data.for_data.to,
                owner_id, nested_type_sections);
            rewrite_class_const_statement_nested_types(stmt->stmt_data.for_data.do_for,
                owner_id, nested_type_sections);
            break;
        case STMT_FOR_IN:
            rewrite_class_const_expr_nested_types(stmt->stmt_data.for_in_data.loop_var,
                owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(stmt->stmt_data.for_in_data.collection,
                owner_id, nested_type_sections);
            rewrite_class_const_statement_nested_types(stmt->stmt_data.for_in_data.do_stmt,
                owner_id, nested_type_sections);
            break;
        case STMT_EXPR:
            rewrite_class_const_expr_nested_types(stmt->stmt_data.expr_stmt_data.expr,
                owner_id, nested_type_sections);
            break;
        default:
            break;
    }
}

static void rewrite_class_const_expr(struct Expression *expr, ListNode_t *map)
{
    if (expr == NULL || map == NULL)
        return;

    switch (expr->type) {
        case EXPR_VAR_ID: {
            const char *mangled = lookup_class_const_map(map, expr->expr_data.id);
            if (mangled != NULL) {
                free(expr->expr_data.id);
                expr->expr_data.id = strdup(mangled);
            }
            break;
        }
        case EXPR_ADDOP:
            rewrite_class_const_expr(expr->expr_data.addop_data.left_expr, map);
            rewrite_class_const_expr(expr->expr_data.addop_data.right_term, map);
            break;
        case EXPR_MULOP:
            rewrite_class_const_expr(expr->expr_data.mulop_data.left_term, map);
            rewrite_class_const_expr(expr->expr_data.mulop_data.right_factor, map);
            break;
        case EXPR_RELOP:
            rewrite_class_const_expr(expr->expr_data.relop_data.left, map);
            rewrite_class_const_expr(expr->expr_data.relop_data.right, map);
            break;
        case EXPR_SIGN_TERM:
            rewrite_class_const_expr(expr->expr_data.sign_term, map);
            break;
        case EXPR_FUNCTION_CALL: {
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;
            for (ListNode_t *cur = args; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_EXPR)
                    rewrite_class_const_expr((struct Expression *)cur->cur, map);
            }
            break;
        }
        case EXPR_ARRAY_ACCESS: {
            rewrite_class_const_expr(expr->expr_data.array_access_data.array_expr, map);
            rewrite_class_const_expr(expr->expr_data.array_access_data.index_expr, map);
            for (ListNode_t *cur = expr->expr_data.array_access_data.extra_indices;
                 cur != NULL; cur = cur->next) {
                if (cur->type == LIST_EXPR)
                    rewrite_class_const_expr((struct Expression *)cur->cur, map);
            }
            break;
        }
        case EXPR_RECORD_ACCESS:
            rewrite_class_const_expr(expr->expr_data.record_access_data.record_expr, map);
            break;
        case EXPR_TYPECAST:
            rewrite_class_const_expr(expr->expr_data.typecast_data.expr, map);
            break;
        case EXPR_SET:
            for (ListNode_t *cur = expr->expr_data.set_data.elements; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_SET_ELEMENT && cur->cur != NULL) {
                    struct SetElement *elem = (struct SetElement *)cur->cur;
                    rewrite_class_const_expr(elem->lower, map);
                    rewrite_class_const_expr(elem->upper, map);
                }
            }
            break;
        case EXPR_ARRAY_LITERAL:
            for (ListNode_t *cur = expr->expr_data.array_literal_data.elements; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_EXPR)
                    rewrite_class_const_expr((struct Expression *)cur->cur, map);
            }
            break;
        case EXPR_RECORD_CONSTRUCTOR:
            for (ListNode_t *cur = expr->expr_data.record_constructor_data.fields; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_UNSPECIFIED && cur->cur != NULL) {
                    struct RecordConstructorField *field = (struct RecordConstructorField *)cur->cur;
                    rewrite_class_const_expr(field->value, map);
                }
            }
            break;
        case EXPR_ADDR:
            rewrite_class_const_expr(expr->expr_data.addr_data.expr, map);
            break;
        case EXPR_POINTER_DEREF:
            rewrite_class_const_expr(expr->expr_data.pointer_deref_data.pointer_expr, map);
            break;
        case EXPR_IS:
            rewrite_class_const_expr(expr->expr_data.is_data.expr, map);
            break;
        case EXPR_AS:
            rewrite_class_const_expr(expr->expr_data.as_data.expr, map);
            break;
        default:
            break;
    }
}

static void rewrite_class_const_statement(struct Statement *stmt, ListNode_t *map)
{
    if (stmt == NULL || map == NULL)
        return;

    switch (stmt->type) {
        case STMT_VAR_ASSIGN:
            rewrite_class_const_expr(stmt->stmt_data.var_assign_data.var, map);
            rewrite_class_const_expr(stmt->stmt_data.var_assign_data.expr, map);
            break;
        case STMT_COMPOUND_STATEMENT:
            for (ListNode_t *cur = stmt->stmt_data.compound_statement; cur != NULL; cur = cur->next)
            {
                if (cur->type == LIST_STMT)
                    rewrite_class_const_statement((struct Statement *)cur->cur, map);
            }
            break;
        case STMT_IF_THEN:
            rewrite_class_const_expr(stmt->stmt_data.if_then_data.relop_expr, map);
            rewrite_class_const_statement(stmt->stmt_data.if_then_data.if_stmt, map);
            rewrite_class_const_statement(stmt->stmt_data.if_then_data.else_stmt, map);
            break;
        case STMT_WHILE:
            rewrite_class_const_expr(stmt->stmt_data.while_data.relop_expr, map);
            rewrite_class_const_statement(stmt->stmt_data.while_data.while_stmt, map);
            break;
        case STMT_REPEAT:
            for (ListNode_t *cur = stmt->stmt_data.repeat_data.body_list; cur != NULL; cur = cur->next)
            {
                if (cur->type == LIST_STMT)
                    rewrite_class_const_statement((struct Statement *)cur->cur, map);
            }
            rewrite_class_const_expr(stmt->stmt_data.repeat_data.until_expr, map);
            break;
        case STMT_FOR:
            if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_ASSIGN_VAR)
                rewrite_class_const_statement(stmt->stmt_data.for_data.for_assign_data.var_assign, map);
            else if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_VAR)
                rewrite_class_const_expr(stmt->stmt_data.for_data.for_assign_data.var, map);
            rewrite_class_const_expr(stmt->stmt_data.for_data.to, map);
            rewrite_class_const_statement(stmt->stmt_data.for_data.do_for, map);
            break;
        case STMT_FOR_IN:
            rewrite_class_const_expr(stmt->stmt_data.for_in_data.loop_var, map);
            rewrite_class_const_expr(stmt->stmt_data.for_in_data.collection, map);
            rewrite_class_const_statement(stmt->stmt_data.for_in_data.do_stmt, map);
            break;
        case STMT_EXPR:
            rewrite_class_const_expr(stmt->stmt_data.expr_stmt_data.expr, map);
            break;
        default:
            break;
    }
}

static void add_class_const_map_entry(ListBuilder *builder, const char *original,
                                      const char *mangled)
{
    if (builder == NULL || original == NULL || mangled == NULL)
        return;
    ClassConstMap *entry = (ClassConstMap *)calloc(1, sizeof(ClassConstMap));
    if (entry == NULL)
        return;
    entry->original = strdup(original);
    entry->mangled = strdup(mangled);
    list_builder_append(builder, entry, LIST_UNSPECIFIED);
}

static ast_t *find_class_spec(ast_t *type_decl_node)
{
    if (type_decl_node == NULL || type_decl_node->typ != PASCAL_T_TYPE_DECL)
        return NULL;

    ast_t *id_node = type_decl_node->child;
    while (id_node != NULL && id_node->typ != PASCAL_T_IDENTIFIER)
        id_node = id_node->next;
    if (id_node == NULL)
        return NULL;

    ast_t *spec_node = id_node->next;
    while (spec_node != NULL &&
           spec_node->typ != PASCAL_T_TYPE_SPEC &&
           spec_node->typ != PASCAL_T_CLASS_TYPE &&
           spec_node->typ != PASCAL_T_OBJECT_TYPE &&
           spec_node->typ != PASCAL_T_RECORD_TYPE)
    {
        spec_node = spec_node->next;
    }
    if (spec_node == NULL)
        return NULL;

    if (spec_node->typ == PASCAL_T_CLASS_TYPE || spec_node->typ == PASCAL_T_OBJECT_TYPE ||
        spec_node->typ == PASCAL_T_RECORD_TYPE)
        return spec_node;
    if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL &&
        (spec_node->child->typ == PASCAL_T_CLASS_TYPE || spec_node->child->typ == PASCAL_T_OBJECT_TYPE ||
         spec_node->child->typ == PASCAL_T_RECORD_TYPE))
        return spec_node->child;

    return NULL;
}

static ast_t *find_helper_record_spec(ast_t *type_decl_node)
{
    if (type_decl_node == NULL || type_decl_node->typ != PASCAL_T_TYPE_DECL)
        return NULL;

    ast_t *id_node = type_decl_node->child;
    while (id_node != NULL && id_node->typ != PASCAL_T_IDENTIFIER)
        id_node = id_node->next;
    if (id_node == NULL)
        return NULL;

    ast_t *spec_node = id_node->next;
    while (spec_node != NULL &&
           spec_node->typ != PASCAL_T_TYPE_SPEC &&
           spec_node->typ != PASCAL_T_RECORD_TYPE &&
           spec_node->typ != PASCAL_T_OBJECT_TYPE)
    {
        spec_node = spec_node->next;
    }

    if (spec_node == NULL)
        return NULL;

    if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
        spec_node = spec_node->child;

    if (spec_node->sym == NULL || spec_node->sym->name == NULL)
        return NULL;

    if (strcasecmp(spec_node->sym->name, "helper") != 0)
        return NULL;

    return spec_node;
}

static void append_class_const_decls_from_type_decl(ast_t *type_decl_node,
    const char *class_id, ListNode_t **const_decls, ListBuilder *var_builder,
    ast_t *type_section)
{
    if (type_decl_node == NULL || class_id == NULL || const_decls == NULL ||
        var_builder == NULL)
        return;

    ast_t *class_spec = find_class_spec(type_decl_node);
    if (class_spec == NULL)
        return;

    /* Skip if this is a type helper — append_helper_const_decls_from_type_decl
     * already handles const sections for helpers with proper mangling. */
    if (find_helper_record_spec(type_decl_node) != NULL)
        return;

    ListNode_t **tail = const_decls;
    while (*tail != NULL)
        tail = &(*tail)->next;

    ListBuilder nested_type_builder;
    list_builder_init(&nested_type_builder);
    if (class_spec != NULL)
        collect_record_nested_types(class_spec->child, &nested_type_builder);
    ListNode_t *nested_type_sections = list_builder_finish(&nested_type_builder);

    ListBuilder class_map_builder;
    list_builder_init(&class_map_builder);

    for (ast_t *cur = class_spec->child; cur != NULL; cur = cur->next)
    {
        ast_t *node = cur;
        ast_t *unwrapped = unwrap_pascal_node(cur);
        int is_const_section = 0;
        if (node != NULL && node->typ == PASCAL_T_CONST_SECTION)
            is_const_section = 1;
        else if (node != NULL && node->typ == PASCAL_T_NONE &&
                 node->sym != NULL && node->sym->name != NULL &&
                 strcasecmp(node->sym->name, "const") == 0)
            is_const_section = 1;
        else if (unwrapped != NULL && unwrapped->typ == PASCAL_T_CONST_SECTION)
            is_const_section = 1;
        if (!is_const_section)
            continue;

        if (kgpc_getenv("KGPC_DEBUG_CLASS_CONST") != NULL) {
            fprintf(stderr, "[KGPC] class const section in %s at line %d\n",
                class_id, node != NULL ? node->line : -1);
        }

        ListNode_t *class_consts = NULL;
        ListNode_t **var_tail_before = var_builder->tail_next;
        append_const_decls_from_section(node, &class_consts, var_builder, type_section);
        ListNode_t *new_var_nodes = (var_tail_before != NULL) ? *var_tail_before : NULL;

        ListNode_t *class_iter = class_consts;
        while (class_iter != NULL)
        {
            Tree_t *decl = (Tree_t *)class_iter->cur;
            if (decl != NULL && decl->type == TREE_CONST_DECL &&
                decl->tree_data.const_decl_data.id != NULL)
            {
                if (kgpc_getenv("KGPC_DEBUG_CLASS_CONST") != NULL) {
                    fprintf(stderr, "[KGPC]   class const %s.%s\n",
                        class_id, decl->tree_data.const_decl_data.id);
                }
                char *mangled = mangle_helper_const_name(class_id,
                    decl->tree_data.const_decl_data.id);
                if (mangled != NULL)
                {
                    add_class_const_map_entry(&class_map_builder,
                        decl->tree_data.const_decl_data.id, mangled);
                    free(mangled);
                }
            }
            class_iter = class_iter->next;
        }

        for (ListNode_t *iter = new_var_nodes; iter != NULL; iter = iter->next)
        {
            Tree_t *decl = (Tree_t *)iter->cur;
            if (decl == NULL)
                continue;
            if (decl->type == TREE_VAR_DECL)
            {
                for (ListNode_t *id_node = decl->tree_data.var_decl_data.ids;
                     id_node != NULL; id_node = id_node->next)
                {
                    if (id_node->type == LIST_STRING && id_node->cur != NULL)
                    {
                        char *old_id = (char *)id_node->cur;
                        char *mangled = mangle_helper_const_name(class_id, old_id);
                        if (mangled != NULL)
                        {
                            add_class_const_map_entry(&class_map_builder, old_id, mangled);
                            free(mangled);
                        }
                    }
                }
            }
            else if (decl->type == TREE_ARR_DECL)
            {
                for (ListNode_t *id_node = decl->tree_data.arr_decl_data.ids;
                     id_node != NULL; id_node = id_node->next)
                {
                    if (id_node->type == LIST_STRING && id_node->cur != NULL)
                    {
                        char *old_id = (char *)id_node->cur;
                        char *mangled = mangle_helper_const_name(class_id, old_id);
                        if (mangled != NULL)
                        {
                            add_class_const_map_entry(&class_map_builder, old_id, mangled);
                            free(mangled);
                        }
                    }
                }
            }
        }

        ListNode_t *map_list = list_builder_finish(&class_map_builder);

        ListNode_t *iter = class_consts;
        while (iter != NULL)
        {
            Tree_t *decl = (Tree_t *)iter->cur;
            if (decl != NULL && decl->type == TREE_CONST_DECL &&
                decl->tree_data.const_decl_data.id != NULL)
            {
                const char *mangled = lookup_class_const_map(map_list,
                    decl->tree_data.const_decl_data.id);
                if (mangled != NULL)
                {
                    free(decl->tree_data.const_decl_data.id);
                    decl->tree_data.const_decl_data.id = strdup(mangled);
                }
            }

            if (decl != NULL && decl->type == TREE_CONST_DECL)
            {
                rewrite_class_const_expr(decl->tree_data.const_decl_data.value, map_list);
                rewrite_class_const_expr_nested_types(decl->tree_data.const_decl_data.value,
                    class_id, nested_type_sections);
            }

            ListNode_t *next = iter->next;
            iter->next = NULL;
            *tail = iter;
            tail = &iter->next;
            iter = next;
        }

        for (ListNode_t *iter = new_var_nodes; iter != NULL; iter = iter->next)
        {
            Tree_t *decl = (Tree_t *)iter->cur;
            if (decl == NULL)
                continue;
            if (decl->type == TREE_VAR_DECL)
            {
                for (ListNode_t *id_node = decl->tree_data.var_decl_data.ids;
                     id_node != NULL; id_node = id_node->next)
                {
                    if (id_node->type == LIST_STRING && id_node->cur != NULL)
                    {
                        const char *mangled = lookup_class_const_map(map_list,
                            (char *)id_node->cur);
                        if (mangled != NULL)
                        {
                            free(id_node->cur);
                            id_node->cur = strdup(mangled);
                        }
                    }
                }
                rewrite_class_const_statement(decl->tree_data.var_decl_data.initializer, map_list);
                rewrite_class_const_statement_nested_types(decl->tree_data.var_decl_data.initializer,
                    class_id, nested_type_sections);
            }
            else if (decl->type == TREE_ARR_DECL)
            {
                for (ListNode_t *id_node = decl->tree_data.arr_decl_data.ids;
                     id_node != NULL; id_node = id_node->next)
                {
                    if (id_node->type == LIST_STRING && id_node->cur != NULL)
                    {
                        const char *mangled = lookup_class_const_map(map_list,
                            (char *)id_node->cur);
                        if (mangled != NULL)
                        {
                            free(id_node->cur);
                            id_node->cur = strdup(mangled);
                        }
                    }
                }
                rewrite_class_const_statement(decl->tree_data.arr_decl_data.initializer, map_list);
                rewrite_class_const_statement_nested_types(decl->tree_data.arr_decl_data.initializer,
                    class_id, nested_type_sections);
            }
        }

    }

    ListNode_t *map_list = list_builder_finish(&class_map_builder);
    for (ListNode_t *cur_map = map_list; cur_map != NULL; cur_map = cur_map->next)
    {
        ClassConstMap *entry = (ClassConstMap *)cur_map->cur;
        if (entry != NULL)
        {
            free(entry->original);
            free(entry->mangled);
            free(entry);
        }
    }
    destroy_list(map_list);
}

static void append_helper_const_decls_from_type_decl(ast_t *type_decl_node,
    const char *helper_id, ListNode_t **const_decls, ListBuilder *var_builder,
    ast_t *type_section)
{
    if (type_decl_node == NULL || helper_id == NULL || const_decls == NULL ||
        var_builder == NULL)
        return;

    ast_t *record_spec = find_helper_record_spec(type_decl_node);
    if (record_spec == NULL)
        return;

    ListNode_t **tail = const_decls;
    while (*tail != NULL)
        tail = &(*tail)->next;

    for (ast_t *cur = record_spec->child; cur != NULL; cur = cur->next)
    {
        ast_t *node = cur;
        ast_t *unwrapped = unwrap_pascal_node(cur);
        int is_const_section = 0;
        if (node != NULL && node->typ == PASCAL_T_CONST_SECTION)
            is_const_section = 1;
        else if (node != NULL && node->typ == PASCAL_T_NONE &&
                 node->sym != NULL && node->sym->name != NULL &&
                 strcasecmp(node->sym->name, "const") == 0)
            is_const_section = 1;
        else if (unwrapped != NULL && unwrapped->typ == PASCAL_T_CONST_SECTION)
            is_const_section = 1;
        if (!is_const_section)
            continue;

        ListNode_t *helper_consts = NULL;
        ListNode_t **var_tail_before = var_builder->tail_next;
        append_const_decls_from_section(node, &helper_consts, var_builder, type_section);
        ListNode_t *new_var_nodes = (var_tail_before != NULL) ? *var_tail_before : NULL;

        ListNode_t *const_cur = helper_consts;
        while (const_cur != NULL)
        {
            ListNode_t *next = const_cur->next;
            const_cur->next = NULL;

            Tree_t *decl = (Tree_t *)const_cur->cur;
            if (decl != NULL && decl->type == TREE_CONST_DECL &&
                decl->tree_data.const_decl_data.id != NULL)
            {
                char *mangled = mangle_helper_const_name(helper_id,
                    decl->tree_data.const_decl_data.id);
                if (mangled != NULL)
                {
                    free(decl->tree_data.const_decl_data.id);
                    decl->tree_data.const_decl_data.id = mangled;
                }
            }

            *tail = const_cur;
            tail = &const_cur->next;
            const_cur = next;
        }

        /* Also rename any typed var decls that were appended to var_builder
         * from this const section (e.g., MaxValue : Single = ...) */
        ListBuilder helper_map_builder;
        list_builder_init(&helper_map_builder);

        for (ListNode_t *iter = new_var_nodes; iter != NULL; iter = iter->next)
        {
            Tree_t *decl = (Tree_t *)iter->cur;
            if (decl == NULL)
                continue;
            if (decl->type == TREE_VAR_DECL)
            {
                for (ListNode_t *id_node = decl->tree_data.var_decl_data.ids;
                     id_node != NULL; id_node = id_node->next)
                {
                    if (id_node->type == LIST_STRING && id_node->cur != NULL)
                    {
                        char *old_id = (char *)id_node->cur;
                        char *mangled = mangle_helper_const_name(helper_id, old_id);
                        if (mangled != NULL)
                        {
                            add_class_const_map_entry(&helper_map_builder, old_id, mangled);
                            free(id_node->cur);
                            id_node->cur = strdup(mangled);
                            free(mangled);
                        }
                    }
                }
            }
            else if (decl->type == TREE_ARR_DECL)
            {
                for (ListNode_t *id_node = decl->tree_data.arr_decl_data.ids;
                     id_node != NULL; id_node = id_node->next)
                {
                    if (id_node->type == LIST_STRING && id_node->cur != NULL)
                    {
                        char *old_id = (char *)id_node->cur;
                        char *mangled = mangle_helper_const_name(helper_id, old_id);
                        if (mangled != NULL)
                        {
                            add_class_const_map_entry(&helper_map_builder, old_id, mangled);
                            free(id_node->cur);
                            id_node->cur = strdup(mangled);
                            free(mangled);
                        }
                    }
                }
            }
        }

        /* Rewrite initializer statements so LHS uses mangled names too */
        ListNode_t *helper_map = list_builder_finish(&helper_map_builder);
        if (helper_map != NULL)
        {
            for (ListNode_t *iter = new_var_nodes; iter != NULL; iter = iter->next)
            {
                Tree_t *decl = (Tree_t *)iter->cur;
                if (decl == NULL)
                    continue;
                if (decl->type == TREE_VAR_DECL)
                    rewrite_class_const_statement(decl->tree_data.var_decl_data.initializer, helper_map);
                else if (decl->type == TREE_ARR_DECL)
                    rewrite_class_const_statement(decl->tree_data.arr_decl_data.initializer, helper_map);
            }
            for (ListNode_t *cur_map = helper_map; cur_map != NULL; cur_map = cur_map->next)
            {
                ClassConstMap *entry = (ClassConstMap *)cur_map->cur;
                if (entry != NULL)
                {
                    free(entry->original);
                    free(entry->mangled);
                    free(entry);
                }
            }
            destroy_list(helper_map);
        }
    }

}

static int nested_type_section_has_id(ast_t *section, const char *id)
{
    if (section == NULL || id == NULL)
        return 0;
    for (ast_t *child = section->child; child != NULL; child = child->next)
    {
        ast_t *unwrapped = unwrap_pascal_node(child);
        if (unwrapped == NULL)
            unwrapped = child;
        if (unwrapped == NULL)
            continue;
        if (unwrapped->typ == PASCAL_T_TYPE_DECL ||
            unwrapped->typ == PASCAL_T_GENERIC_TYPE_DECL)
        {
            char *name = dup_first_identifier_in_node(unwrapped);
            int match = (name != NULL && pascal_identifier_equals(name, id));
            if (name != NULL)
                free(name);
            if (match)
                return 1;
        }
    }
    return 0;
}

static struct TypeRef *ensure_type_ref_from_id(char **type_id, struct TypeRef **type_ref)
{
    if (type_ref == NULL)
        return NULL;
    if (*type_ref != NULL)
        return *type_ref;
    if (type_id == NULL || *type_id == NULL)
        return NULL;

    QualifiedIdent *qid = qualified_ident_from_dotted(*type_id);
    if (qid == NULL)
        qid = qualified_ident_from_single(*type_id);
    if (qid == NULL)
        return NULL;
    *type_ref = type_ref_create(qid, NULL, 0);
    return *type_ref;
}

static void qualify_record_field_nested_types(struct RecordType *record,
    const char *parent_type_name, ast_t *type_section)
{
    if (record == NULL || parent_type_name == NULL || type_section == NULL)
        return;

    for (ListNode_t *cur = record->fields; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_RECORD_FIELD || cur->cur == NULL)
            continue;
        struct RecordField *field = (struct RecordField *)cur->cur;

        struct TypeRef *type_ref = ensure_type_ref_from_id(&field->type_id, &field->type_ref);
        if (type_ref != NULL && type_ref->name != NULL && type_ref->name->count == 1)
        {
            const char *base = qualified_ident_last(type_ref->name);
            if (base != NULL && nested_type_section_has_id(type_section, base))
            {
                qualify_type_ref_id(&field->type_id, &field->type_ref, parent_type_name, base);
            }
        }

        struct TypeRef *elem_ref = ensure_type_ref_from_id(&field->array_element_type_id,
            &field->array_element_type_ref);
        if (elem_ref != NULL && elem_ref->name != NULL && elem_ref->name->count == 1)
        {
            const char *base = qualified_ident_last(elem_ref->name);
            if (base != NULL && nested_type_section_has_id(type_section, base))
            {
                qualify_type_ref_id(&field->array_element_type_id, &field->array_element_type_ref,
                    parent_type_name, base);
            }
        }

        struct TypeRef *ptr_ref = ensure_type_ref_from_id(&field->pointer_type_id,
            &field->pointer_type_ref);
        if (ptr_ref != NULL && ptr_ref->name != NULL && ptr_ref->name->count == 1)
        {
            const char *base = qualified_ident_last(ptr_ref->name);
            if (base != NULL && nested_type_section_has_id(type_section, base))
            {
                qualify_type_ref_id(&field->pointer_type_id, &field->pointer_type_ref,
                    parent_type_name, base);
            }
        }
    }
}

void append_type_decls_from_section(ast_t *type_section, ListNode_t **dest,
    ListNode_t **subprograms, ListNode_t **const_decls, ListBuilder *var_builder,
    const char *parent_type_name) {
    if (type_section == NULL || dest == NULL)
        return;

    ListNode_t **tail = dest;
    while (*tail != NULL)
        tail = &(*tail)->next;

    if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
        fprintf(stderr, "[KGPC] append_type_decls_from_section: type_section->typ=%d child=%p line=%d parent=%s\n",
                type_section->typ, (void*)type_section->child, type_section->line,
                parent_type_name ? parent_type_name : "<none>");

        /* Count children and print info for each */
        int child_count = 0;
        for (ast_t *c = type_section->child; c != NULL; c = c->next) {
            char *c_id = dup_first_identifier_in_node(c);
            fprintf(stderr, "[KGPC]   child[%d] typ=%d (%s) line=%d id=%s\n",
                    child_count++, c->typ, pascal_tag_to_string(c->typ),
                    c->line, c_id != NULL ? c_id : "<none>");
            if (c_id != NULL) free(c_id);
        }
        fprintf(stderr, "[KGPC]   total children: %d\n", child_count);
    }

    ast_t *type_decl = type_section->child;
    while (type_decl != NULL) {
        ast_t *unwrapped = unwrap_pascal_node(type_decl);
        if (unwrapped == NULL)
            unwrapped = type_decl;

        if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL && unwrapped != NULL) {
            char *name = dup_first_identifier_in_node(unwrapped);
            fprintf(stderr, "[KGPC] type-section decl tag=%d (%s) name=%s\n",
                    unwrapped->typ,
                    pascal_tag_to_string(unwrapped->typ),
                    name != NULL ? name : "<none>");
            if (name != NULL)
                free(name);
        }

        int treat_as_generic = 0;
        if (unwrapped != NULL) {
            if (unwrapped->typ == PASCAL_T_GENERIC_TYPE_DECL) {
                treat_as_generic = 1;
            } else if (unwrapped->typ == PASCAL_T_TYPE_DECL) {
                ast_t *id_node = unwrapped->child;
                while (id_node != NULL && id_node->typ != PASCAL_T_IDENTIFIER)
                    id_node = id_node->next;
                ast_t *next = id_node != NULL ? id_node->next : NULL;
                if (next != NULL && next->typ == PASCAL_T_TYPE_PARAM_LIST)
                    treat_as_generic = 1;
            }
        }

        if (treat_as_generic && unwrapped != NULL) {
            if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL) {
                char *type_name = dup_first_identifier_in_node(unwrapped);
                if (type_name != NULL) {
                    fprintf(stderr, "[KGPC] append_type_decls_from_section saw generic-like %s (tag=%d)\n",
                            type_name, unwrapped->typ);
                    free(type_name);
                }
            }
            Tree_t *decl = convert_generic_type_decl(unwrapped);
            if (decl != NULL) {
                if (parent_type_name != NULL && decl->tree_data.type_decl_data.id != NULL) {
                    char *orig_id = decl->tree_data.type_decl_data.id;
                    size_t len = strlen(parent_type_name) + 1 + strlen(orig_id) + 1;
                    char *qualified_id = (char *)malloc(len);
                    if (qualified_id != NULL) {
                        snprintf(qualified_id, len, "%s.%s", parent_type_name, orig_id);
                        free(orig_id);
                        decl->tree_data.type_decl_data.id = qualified_id;
                        /* Set outer_type_id on record types for nested type qualification */
                        if (decl->tree_data.type_decl_data.kind == TYPE_DECL_RECORD &&
                            decl->tree_data.type_decl_data.info.record != NULL &&
                            decl->tree_data.type_decl_data.info.record->outer_type_id == NULL)
                            decl->tree_data.type_decl_data.info.record->outer_type_id = strdup(parent_type_name);
                    }
                    /* Also qualify pointer_type_id for pointer aliases referencing
                     * sibling types in the same nested scope */
                    if (decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
                        struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
                        if (alias->is_pointer) {
                            struct TypeRef *ptr_ref = ensure_type_ref_from_id(
                                &alias->pointer_type_id, &alias->pointer_type_ref);
                            if (ptr_ref != NULL && ptr_ref->name != NULL && ptr_ref->name->count == 1) {
                                const char *base = qualified_ident_last(ptr_ref->name);
                                if (base != NULL && nested_type_section_has_id(type_section, base)) {
                                    qualify_type_ref_id(&alias->pointer_type_id, &alias->pointer_type_ref,
                                        parent_type_name, base);
                                }
                            }
                        }
                    }
                }
                ListNode_t *node = CreateListNode(decl, LIST_TREE);
                *tail = node;
                tail = &node->next;
            }
        } else if (unwrapped != NULL && unwrapped->typ == PASCAL_T_TYPE_DECL) {
            ListNode_t *nested_type_decls = NULL;
            char *nested_parent_name = NULL;
            if (const_decls != NULL && var_builder != NULL) {
                char *helper_id = dup_first_identifier_in_node(unwrapped);
                if (helper_id != NULL) {
                    append_helper_const_decls_from_type_decl(unwrapped, helper_id,
                        const_decls, var_builder, type_section);
                    append_class_const_decls_from_type_decl(unwrapped, helper_id,
                        const_decls, var_builder, type_section);
                    if (parent_type_name != NULL) {
                        size_t len = strlen(parent_type_name) + 1 + strlen(helper_id) + 1;
                        nested_parent_name = (char *)malloc(len);
                        if (nested_parent_name != NULL) {
                            snprintf(nested_parent_name, len, "%s.%s", parent_type_name, helper_id);
                        }
                    } else {
                        nested_parent_name = strdup(helper_id);
                    }
                    free(helper_id);
                }
            } else if (parent_type_name != NULL) {
                char *type_id = dup_first_identifier_in_node(unwrapped);
                if (type_id != NULL) {
                    size_t len = strlen(parent_type_name) + 1 + strlen(type_id) + 1;
                    nested_parent_name = (char *)malloc(len);
                    if (nested_parent_name != NULL) {
                        snprintf(nested_parent_name, len, "%s.%s", parent_type_name, type_id);
                    }
                    free(type_id);
                }
            }
            Tree_t *decl = convert_type_decl_ex(unwrapped, subprograms, &nested_type_decls);
            if (decl != NULL && parent_type_name != NULL && decl->tree_data.type_decl_data.id != NULL) {
                char *orig_id = decl->tree_data.type_decl_data.id;
                size_t len = strlen(parent_type_name) + 1 + strlen(orig_id) + 1;
                char *qualified_id = (char *)malloc(len);
                if (qualified_id != NULL) {
                    snprintf(qualified_id, len, "%s.%s", parent_type_name, orig_id);
                    free(orig_id);
                    decl->tree_data.type_decl_data.id = qualified_id;
                    if (decl->tree_data.type_decl_data.kind == TYPE_DECL_RECORD &&
                        decl->tree_data.type_decl_data.info.record != NULL &&
                        decl->tree_data.type_decl_data.info.record->type_id != NULL)
                    {
                        free(decl->tree_data.type_decl_data.info.record->type_id);
                        decl->tree_data.type_decl_data.info.record->type_id = strdup(qualified_id);
                        if (decl->tree_data.type_decl_data.info.record->outer_type_id == NULL)
                            decl->tree_data.type_decl_data.info.record->outer_type_id = strdup(parent_type_name);
                        qualify_record_field_nested_types(decl->tree_data.type_decl_data.info.record,
                            parent_type_name, type_section);
                        if (decl->tree_data.type_decl_data.info.record->parent_class_name != NULL &&
                            nested_type_section_has_id(type_section,
                                decl->tree_data.type_decl_data.info.record->parent_class_name))
                        {
                            char *orig_parent = decl->tree_data.type_decl_data.info.record->parent_class_name;
                            size_t plen = strlen(parent_type_name) + 1 + strlen(orig_parent) + 1;
                            char *qualified_parent = (char *)malloc(plen);
                            if (qualified_parent != NULL)
                            {
                                snprintf(qualified_parent, plen, "%s.%s", parent_type_name, orig_parent);
                                free(orig_parent);
                                decl->tree_data.type_decl_data.info.record->parent_class_name = qualified_parent;
                            }
                        }
                    }
                    if (decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
                        struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
                        /* Qualify pointer_type_id so symbol table lookup resolves
                         * the sibling nested type correctly */
                        if (alias->is_pointer) {
                            struct TypeRef *ptr_ref = ensure_type_ref_from_id(
                                &alias->pointer_type_id, &alias->pointer_type_ref);
                            if (ptr_ref != NULL && ptr_ref->name != NULL && ptr_ref->name->count == 1) {
                                const char *base = qualified_ident_last(ptr_ref->name);
                                if (base != NULL && nested_type_section_has_id(type_section, base)) {
                                    qualify_type_ref_id(&alias->pointer_type_id, &alias->pointer_type_ref,
                                        parent_type_name, base);
                                }
                            }
                        }
                        if (alias->kgpc_type != NULL &&
                            alias->kgpc_type->kind == TYPE_KIND_POINTER &&
                            alias->kgpc_type->info.points_to != NULL &&
                            alias->kgpc_type->info.points_to->kind == TYPE_KIND_RECORD &&
                            alias->kgpc_type->info.points_to->info.record_info != NULL)
                        {
                            struct RecordType *points_to_record = alias->kgpc_type->info.points_to->info.record_info;
                            if (points_to_record->type_id != NULL)
                            {
                                free(points_to_record->type_id);
                                points_to_record->type_id = strdup(qualified_id);
                            }
                        }
                    }
                }
            }
            if (nested_type_decls != NULL) {
                ListNode_t *nested_cursor = nested_type_decls;
                while (nested_cursor != NULL) {
                    ListNode_t *next = nested_cursor->next;
                    nested_cursor->next = NULL;
                    *tail = nested_cursor;
                    tail = &nested_cursor->next;
                    nested_cursor = next;
                }
            }
            if (decl != NULL) {
                ListNode_t *node = CreateListNode(decl, LIST_TREE);
                *tail = node;
                tail = &node->next;
            }
            if (nested_parent_name != NULL)
                free(nested_parent_name);
        }
        type_decl = type_decl->next;
    }
}

