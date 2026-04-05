#include "../from_cparser_internal.h"

ListNode_t *convert_identifier_list(ast_t **cursor) {
    ListBuilder builder;
    list_builder_init(&builder);
    ast_t *cur = *cursor;
    while (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER) {
        char *dup = dup_symbol(cur);
        if (dup != NULL)
            list_builder_append(&builder, dup, LIST_STRING);
        cur = cur->next;
    }
    *cursor = cur;
    return list_builder_finish(&builder);
}

static ListNode_t *convert_field_decl(ast_t *field_decl_node);
static struct VariantPart *convert_variant_part(ast_t *variant_node, ListNode_t **out_tag_fields);
static struct VariantBranch *convert_variant_branch(ast_t *branch_node);

static ListNode_t *convert_class_field_decl(ast_t *field_decl_node) {
    if (field_decl_node == NULL || field_decl_node->typ != PASCAL_T_FIELD_DECL)
        return NULL;

    int is_class_var = 0;
    for (ast_t *scan = field_decl_node->child; scan != NULL; scan = scan->next)
    {
        ast_t *node = unwrap_pascal_node(scan);
        if (node != NULL && node->sym != NULL && node->sym->name != NULL &&
            strcasecmp(node->sym->name, "class") == 0)
        {
            is_class_var = 1;
            break;
        }
    }

    ast_t *cursor = field_decl_node->child;
    if (cursor != NULL && cursor->typ == PASCAL_T_IDENTIFIER &&
        cursor->sym != NULL && cursor->sym->name != NULL &&
        strcasecmp(cursor->sym->name, "class") == 0)
    {
        ast_t *next = cursor->next;
        if (next != NULL && next->typ == PASCAL_T_IDENTIFIER &&
            next->sym != NULL && next->sym->name != NULL &&
            strcasecmp(next->sym->name, "var") == 0)
        {
            cursor = next->next;
        }
    }
    ListNode_t *names = convert_identifier_list(&cursor);
    if (names == NULL) {
        return NULL;
    }

    /* Skip to the type specification */
    while (cursor != NULL && cursor->typ != PASCAL_T_TYPE_SPEC &&
           cursor->typ != PASCAL_T_RECORD_TYPE && cursor->typ != PASCAL_T_OBJECT_TYPE &&
           cursor->typ != PASCAL_T_IDENTIFIER && cursor->typ != PASCAL_T_ARRAY_TYPE &&
           cursor->typ != PASCAL_T_SET &&
           cursor->typ != PASCAL_T_POINTER_TYPE &&
           cursor->typ != PASCAL_T_ENUMERATED_TYPE &&
           cursor->typ != PASCAL_T_FILE_TYPE &&
           cursor->typ != PASCAL_T_PROCEDURE_TYPE &&
           cursor->typ != PASCAL_T_FUNCTION_TYPE &&
           cursor->typ != PASCAL_T_REFERENCE_TO_TYPE) {
        cursor = cursor->next;
    }

    char *field_type_id = NULL;
    struct RecordType *nested_record = NULL;
    TypeInfo field_info;
    memset(&field_info, 0, sizeof(TypeInfo));
    int field_type = UNKNOWN_TYPE;

    if (cursor != NULL) {
        /* Use convert_type_spec to properly handle all type forms including arrays */
        field_type = convert_type_spec(cursor, &field_type_id, &nested_record, &field_info);
    } else if (names != NULL) {
        /* Fallback: if no type spec, try to parse last name as type */
        char *candidate = pop_last_identifier(&names);
        if (candidate != NULL) {
            char *mapped_id = NULL;
            int mapped_type = map_type_name(candidate, &mapped_id);
            if (mapped_type != UNKNOWN_TYPE) {
                mapped_type = apply_shortstring_mode(mapped_type, candidate);
                field_type = mapped_type;
                field_type_id = mapped_id;
                free(candidate);
            } else {
                free(mapped_id);
                field_type_id = candidate;
            }
        }
    }

    ListBuilder result;
    list_builder_init(&result);

    ListNode_t *name_node = names;
    while (name_node != NULL) {
        char *field_name = (char *)name_node->cur;
        char *type_id_copy = NULL;
        if (field_type_id != NULL)
            type_id_copy = strdup(field_type_id);

        struct RecordType *nested_copy = NULL;
        if (nested_record != NULL) {
            if (name_node->next == NULL) {
                nested_copy = nested_record;
                nested_record = NULL;
            } else {
                nested_copy = clone_record_type(nested_record);
            }
        }

        struct RecordField *field_desc = (struct RecordField *)calloc(1, sizeof(struct RecordField));
        if (field_desc != NULL) {
            field_desc->name = field_name;
            field_desc->type = field_type;
            /* string[N] fields are shortstrings (array[0..N] of Char with length byte) */
            if (field_desc->type == UNKNOWN_TYPE && field_info.is_shortstring)
                field_desc->type = SHORTSTRING_TYPE;
            field_desc->type_id = type_id_copy;
            field_desc->type_ref = type_ref_from_info_or_id(&field_info, type_id_copy);
            field_desc->nested_record = nested_copy;
            field_desc->is_array = field_info.is_array;
            field_desc->array_start = field_info.start;
            field_desc->array_end = field_info.end;
            field_desc->array_element_type = field_info.element_type;
            field_desc->array_element_type_ref =
                type_ref_from_element_info(&field_info, field_info.element_type_id);
            field_desc->array_is_open = field_info.is_open_array;
            /* For multi-name fields (e.g. a, b: array[0..1] of T),
             * duplicate shared resources; transfer ownership only for the last name. */
            if (name_node->next == NULL)
            {
                field_desc->array_element_type_id = field_info.element_type_id;
                field_info.element_type_id = NULL;
                field_desc->array_element_record = field_info.record_type;
                field_info.record_type = NULL;
            }
            else
            {
                field_desc->array_element_type_id =
                    field_info.element_type_id ? strdup(field_info.element_type_id) : NULL;
                field_desc->array_element_record =
                    field_info.record_type ? clone_record_type(field_info.record_type) : NULL;
            }
            /* Transfer pre-built element KgpcType for nested arrays */
            if (field_info.element_kgpc_type != NULL)
            {
                field_desc->array_element_kgpc_type = field_info.element_kgpc_type;
                kgpc_type_retain(field_desc->array_element_kgpc_type);
            }
            field_desc->is_hidden = 0;
            field_desc->is_class_var = is_class_var;
            /* Copy pointer type info for inline pointer fields like ^Char */
            field_desc->is_pointer = field_info.is_pointer;
            field_desc->pointer_type = field_info.pointer_type;
            if (field_info.pointer_type_id != NULL)
                field_desc->pointer_type_id = strdup(field_info.pointer_type_id);
            else
                field_desc->pointer_type_id = NULL;
            field_desc->pointer_type_ref =
                type_ref_from_pointer_info(&field_info, field_info.pointer_type_id);
            /* Copy set element type info for proper set size computation */
            if (field_info.is_set && field_info.set_element_type_id != NULL)
                field_desc->set_element_type_id = strdup(field_info.set_element_type_id);
            list_builder_append(&result, field_desc, LIST_RECORD_FIELD);
        } else {
            if (field_name != NULL)
                free(field_name);
            if (type_id_copy != NULL)
                free(type_id_copy);
            destroy_record_type(nested_copy);
        }

        ListNode_t *next_name = name_node->next;
        free(name_node);
        name_node = next_name;
    }

    if (field_type_id != NULL)
        free(field_type_id);
    if (nested_record != NULL)
        destroy_record_type(nested_record);

    return list_builder_finish(&result);
}

static struct ClassProperty *convert_property_decl(ast_t *property_node)
{
    if (property_node == NULL)
        return NULL;

    char *property_name = NULL;
    ast_t *type_node = NULL;
    char *read_accessor = NULL;
    char *write_accessor = NULL;
    int has_indexer = 0;
    int is_default = 0;
    int next_accessor_is_write = 0; /* set when we see "write" keyword */
    int next_accessor_is_read = 0;  /* set when we see "read" keyword */

    ast_t *cursor = property_node->child;
    if (cursor != NULL && cursor->typ == PASCAL_T_NONE && cursor->child != NULL)
        cursor = cursor->child;
    if (kgpc_getenv("KGPC_DEBUG_PROPERTY") != NULL)
    {
        fprintf(stderr, "[KGPC] property decl child list:\n");
        for (ast_t *dbg = cursor; dbg != NULL; dbg = dbg->next)
        {
            fprintf(stderr, "  - typ=%d (%s)\n", dbg->typ, pascal_tag_to_string(dbg->typ));
        }
    }
    while (cursor != NULL)
    {
        if (cursor->typ == PASCAL_T_PARAM_LIST)
        {
            has_indexer = 1;
            cursor = cursor->next;
            continue;
        }
        if (cursor->typ == PASCAL_T_DEFAULT_PROPERTY)
        {
            is_default = 1;
            cursor = cursor->next;
            continue;
        }
        ast_t *unwrapped = unwrap_pascal_node(cursor);
        if (unwrapped == NULL)
        {
            cursor = cursor->next;
            continue;
        }

        if (unwrapped->typ == PASCAL_T_IDENTIFIER)
        {
            char *dup = dup_symbol(unwrapped);
            if (dup == NULL)
            {
                cursor = cursor->next;
                continue;
            }

            if (property_name == NULL &&
                (strcasecmp(dup, "class") == 0 || strcasecmp(dup, "generic") == 0))
            {
                free(dup);
                cursor = cursor->next;
                continue;
            }

            if (property_name != NULL && strcasecmp(dup, "default") == 0)
            {
                is_default = 1;
                free(dup);
                cursor = cursor->next;
                continue;
            }

            if (property_name == NULL)
            {
                property_name = dup;
            }
            else if (type_node == NULL)
            {
                type_node = unwrapped;
                free(dup);
            }
            else if (strcasecmp(dup, "read") == 0)
            {
                next_accessor_is_read = 1;
                next_accessor_is_write = 0;
                free(dup);
            }
            else if (strcasecmp(dup, "write") == 0)
            {
                next_accessor_is_write = 1;
                next_accessor_is_read = 0;
                free(dup);
            }
            else if (next_accessor_is_read && read_accessor == NULL)
            {
                read_accessor = dup;
                next_accessor_is_read = 0;
            }
            else if (next_accessor_is_write && write_accessor == NULL)
            {
                write_accessor = dup;
                next_accessor_is_write = 0;
            }
            else if (read_accessor == NULL && !next_accessor_is_write)
            {
                read_accessor = dup;
            }
            else if (write_accessor == NULL)
            {
                write_accessor = dup;
            }
            else
            {
                free(dup);
            }
        }
        else if (type_node == NULL &&
                 (unwrapped->typ == PASCAL_T_TYPE_SPEC ||
                  unwrapped->typ == PASCAL_T_ARRAY_TYPE ||
                  unwrapped->typ == PASCAL_T_POINTER_TYPE ||
                  unwrapped->typ == PASCAL_T_CONSTRUCTED_TYPE ||
                  unwrapped->typ == PASCAL_T_FUNCTION_TYPE ||
                  unwrapped->typ == PASCAL_T_PROCEDURE_TYPE))
        {
            type_node = unwrapped;
        }

        cursor = cursor->next;
    }

    if (property_name == NULL)
        return NULL;
    (void)has_indexer;

    int property_type = UNKNOWN_TYPE;
    char *property_type_id = NULL;
    struct RecordType *inline_record = NULL;
    TypeInfo type_info;
    memset(&type_info, 0, sizeof(TypeInfo));
    TypeRef *property_type_ref = NULL;

    if (type_node != NULL)
    {
        property_type = convert_type_spec(type_node, &property_type_id, &inline_record, &type_info);
        property_type_ref = type_ref_from_info_or_id(&type_info, property_type_id);
        destroy_type_info_contents(&type_info);
        if (inline_record != NULL)
            destroy_record_type(inline_record);
    }

    struct ClassProperty *property = (struct ClassProperty *)calloc(1, sizeof(struct ClassProperty));
    if (property == NULL)
    {
        free(property_name);
        free(property_type_id);
        free(read_accessor);
        free(write_accessor);
        return NULL;
    }

    property->name = property_name;
    property->type = property_type;
    property->type_id = property_type_id;
    property->type_ref = property_type_ref;
    if (property->type_ref == NULL)
        property->type_ref = type_ref_from_single_name(property_type_id);
    property->read_accessor = read_accessor;
    property->write_accessor = write_accessor;
    property->is_indexed = has_indexer;
    property->is_default = is_default;

    return property;
}

void append_module_property_wrappers(ListNode_t **subprograms, ast_t *property_node)
{
    if (subprograms == NULL || property_node == NULL)
        return;

    struct ClassProperty *prop = convert_property_decl(property_node);
    if (prop == NULL)
        return;

    if (kgpc_getenv("KGPC_DEBUG_PROPERTY") != NULL)
    {
        fprintf(stderr,
            "[KGPC] module property name=%s type=%d type_id=%s read=%s write=%s\n",
            prop->name ? prop->name : "<null>",
            prop->type,
            prop->type_id ? prop->type_id : "<null>",
            prop->read_accessor ? prop->read_accessor : "<null>",
            prop->write_accessor ? prop->write_accessor : "<null>");
    }

    if (prop->name != NULL && !prop->is_indexed)
    {
        if (prop->write_accessor != NULL)
        {
            char *proc_name = strdup(prop->name);
            if (proc_name != NULL)
            {
                char *param_name = strdup("Value");
                ListNode_t *param_ids = NULL;
                if (param_name != NULL)
                    param_ids = CreateListNode(param_name, LIST_STRING);

                char *param_type_id = prop->type_id != NULL ? strdup(prop->type_id) : NULL;
                Tree_t *param_decl = NULL;
                if (param_ids != NULL)
                    param_decl = mk_vardecl(0, param_ids, prop->type, param_type_id,
                        0, 0, NULL, NULL, NULL, NULL);
                if (param_decl == NULL && param_type_id != NULL)
                    free(param_type_id);
                if (param_decl != NULL)
                    param_decl->tree_data.var_decl_data.type_ref =
                        prop->type_ref != NULL ? type_ref_clone(prop->type_ref)
                                               : type_ref_from_single_name(param_type_id);

                ListNode_t *params = NULL;
                if (param_decl != NULL)
                    params = CreateListNode(param_decl, LIST_TREE);

                struct Expression *value_expr = mk_varid(0, strdup("Value"));
                ListNode_t *call_args = NULL;
                if (value_expr != NULL)
                    call_args = CreateListNode(value_expr, LIST_EXPR);
                struct Statement *call_stmt = NULL;
                if (call_args != NULL)
                    call_stmt = mk_procedurecall(0, strdup(prop->write_accessor), call_args);

                ListNode_t *body_list = NULL;
                if (call_stmt != NULL)
                    body_list = CreateListNode(call_stmt, LIST_STMT);
                struct Statement *body_stmt = NULL;
                if (body_list != NULL)
                    body_stmt = mk_compoundstatement(0, body_list);

                Tree_t *proc_tree = NULL;
                if (body_stmt != NULL)
                    proc_tree = mk_procedure(0, proc_name, params, NULL, NULL, NULL, NULL, NULL,
                        body_stmt, 0, 0);

                if (proc_tree != NULL)
                {
                    append_subprogram_node(subprograms, proc_tree);
                }
                else
                {
                    if (body_stmt != NULL)
                        destroy_stmt(body_stmt);
                    free(proc_name);
                }
            }
        }

        if (prop->read_accessor != NULL)
        {
            char *func_name = strdup(prop->name);
            if (func_name != NULL)
            {
                struct Expression *accessor_call = mk_functioncall(0, strdup(prop->read_accessor), NULL);
                if (accessor_call != NULL)
                {
                    struct Expression *result_var = mk_varid(0, strdup("Result"));
                    if (result_var != NULL)
                    {
                        struct Statement *assign_stmt = mk_varassign(0, 0, result_var, accessor_call);
                        if (assign_stmt != NULL)
                        {
                            ListNode_t *body_list = CreateListNode((void *)assign_stmt, LIST_STMT);
                            struct Statement *body_stmt = mk_compoundstatement(0, body_list);
                            if (body_stmt != NULL)
                            {
                                char *return_type_id = prop->type_id != NULL ? strdup(prop->type_id) : NULL;
                                Tree_t *func_tree = mk_function(0, func_name, NULL, NULL, NULL, NULL, NULL, NULL, body_stmt,
                                    prop->type, return_type_id, NULL, 0, 0);
                                if (func_tree != NULL)
                                {
                                    func_tree->tree_data.subprogram_data.return_type_ref =
                                        prop->type_ref != NULL ? type_ref_clone(prop->type_ref)
                                                               : type_ref_from_single_name(return_type_id);
                                    append_subprogram_node(subprograms, func_tree);
                                    if (kgpc_getenv("KGPC_DEBUG_PROPERTY") != NULL)
                                    {
                                        fprintf(stderr, "[KGPC] Created synthetic function '%s' for module property\n", prop->name);
                                    }
                                }
                                else
                                {
                                    destroy_stmt(body_stmt);
                                    free(func_name);
                                }
                            }
                            else
                            {
                                destroy_stmt(assign_stmt);
                                free(func_name);
                            }
                        }
                        else
                        {
                            destroy_expr(accessor_call);
                            destroy_expr(result_var);
                            free(func_name);
                        }
                    }
                    else
                    {
                        destroy_expr(accessor_call);
                        free(func_name);
                    }
                }
                else
                {
                    free(func_name);
                }
            }
        }
    }

    if (prop->name) free(prop->name);
    if (prop->type_id) free(prop->type_id);
    if (prop->read_accessor) free(prop->read_accessor);
    if (prop->write_accessor) free(prop->write_accessor);
    free(prop);
}

void annotate_method_template(struct MethodTemplate *method_template, ast_t *method_ast)
{
    if (method_template == NULL || method_ast == NULL)
        return;

    method_template->kind = METHOD_TEMPLATE_UNKNOWN;

    /* Check the node type for constructor/destructor declarations.
     * The parser uses PASCAL_T_CONSTRUCTOR_DECL / PASCAL_T_DESTRUCTOR_DECL
     * for methods declared with the 'constructor' / 'destructor' keyword,
     * so the kind must be set from the node type, not from child sym_names. */
    if (method_ast->typ == PASCAL_T_CONSTRUCTOR_DECL)
        method_template->kind = METHOD_TEMPLATE_CONSTRUCTOR;
    else if (method_ast->typ == PASCAL_T_DESTRUCTOR_DECL)
        method_template->kind = METHOD_TEMPLATE_DESTRUCTOR;
    
    /* First pass: check ALL children for "class" keyword before the method name.
     * The parser produces an IDENTIFIER child with sym->name="class" when
     * create_keyword_parser("class", PASCAL_T_IDENTIFIER) matches. */
    ast_t *cursor = method_ast->child;
    while (cursor != NULL)
    {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node == NULL)
            node = cursor;
        const char *sym_name = (node->sym != NULL) ? node->sym->name : NULL;

        /* Check for "class" keyword in any child node.
         * Note: "class function" has Self = VMT pointer; "class function ... static" has no Self.
         * Only set is_class_method here; is_static is set by the "static" directive. */
        if (sym_name != NULL && strcasecmp(sym_name, "class") == 0) {
            method_template->is_class_method = 1;
        }
        cursor = cursor->next;
    }
    
    /* Second pass: process all other attributes */
    cursor = method_ast->child;
    while (cursor != NULL)
    {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node == NULL)
            node = cursor;

        const char *sym_name = (node->sym != NULL) ? node->sym->name : NULL;
        switch (node->typ)
        {
            case PASCAL_T_IDENTIFIER:
                if (method_template->return_type_ast == NULL &&
                    sym_name != NULL &&
                    method_template->name != NULL &&
                    strcasecmp(sym_name, method_template->name) != 0 &&
                    !is_method_decl_keyword(sym_name))
                {
                    method_template->return_type_ast = node;
                    method_template->has_return_type = 1;
                }
                break;
            case PASCAL_T_PARAM_LIST:
            case PASCAL_T_PARAM:
                if (method_template->params_ast == NULL)
                    method_template->params_ast = node;
                break;
            case PASCAL_T_METHOD_DIRECTIVE:
                method_template->directives_ast = node;
                /* Check for directive keywords (virtual, override, static) in symbol name and children */
                if (sym_name != NULL)
                {
                    if (strcasecmp(sym_name, "virtual") == 0)
                        method_template->is_virtual = 1;
                    else if (strcasecmp(sym_name, "abstract") == 0)
                        method_template->is_virtual = 1;  /* Abstract methods are implicitly virtual */
                    else if (strcasecmp(sym_name, "override") == 0)
                    {
                        method_template->is_override = 1;
                        method_template->is_virtual = 1;
                    }
                    else if (strcasecmp(sym_name, "static") == 0)
                        method_template->is_static = 1;
                }
                /* Also check the child for all directive keywords */
                if (node->child != NULL) {
                    ast_t *dir_child = unwrap_pascal_node(node->child);
                    if (dir_child != NULL && dir_child->sym != NULL && dir_child->sym->name != NULL) {
                        const char *child_name = dir_child->sym->name;
                        if (strcasecmp(child_name, "static") == 0)
                            method_template->is_static = 1;
                        else if (strcasecmp(child_name, "abstract") == 0)
                            method_template->is_virtual = 1;
                        else if (strcasecmp(child_name, "virtual") == 0)
                            method_template->is_virtual = 1;
                        else if (strcasecmp(child_name, "override") == 0) {
                            method_template->is_override = 1;
                            method_template->is_virtual = 1;
                        }
                    }
                }
                /* Also recursively check all children of the directive to find directives */
                ast_t *dir_cur = node->child;
                while (dir_cur != NULL) {
                    ast_t *unwrapped_dir = unwrap_pascal_node(dir_cur);
                    if (unwrapped_dir != NULL && unwrapped_dir->sym != NULL &&
                        unwrapped_dir->sym->name != NULL) {
                        const char *dir_name = unwrapped_dir->sym->name;
                        if (strcasecmp(dir_name, "static") == 0)
                            method_template->is_static = 1;
                        else if (strcasecmp(dir_name, "abstract") == 0)
                            method_template->is_virtual = 1;
                        else if (strcasecmp(dir_name, "virtual") == 0)
                            method_template->is_virtual = 1;
                        else if (strcasecmp(dir_name, "override") == 0) {
                            method_template->is_override = 1;
                            method_template->is_virtual = 1;
                        }
                    }
                    /* Check identifier child of token */
                    if (unwrapped_dir != NULL && unwrapped_dir->typ == PASCAL_T_IDENTIFIER &&
                        unwrapped_dir->sym != NULL && unwrapped_dir->sym->name != NULL) {
                        const char *id_name = unwrapped_dir->sym->name;
                        if (strcasecmp(id_name, "static") == 0)
                            method_template->is_static = 1;
                        else if (strcasecmp(id_name, "abstract") == 0)
                            method_template->is_virtual = 1;
                        else if (strcasecmp(id_name, "virtual") == 0)
                            method_template->is_virtual = 1;
                        else if (strcasecmp(id_name, "override") == 0) {
                            method_template->is_override = 1;
                            method_template->is_virtual = 1;
                        }
                    }
                    dir_cur = dir_cur->next;
                }
                break;
            case PASCAL_T_RETURN_TYPE:
                method_template->return_type_ast = node;
                method_template->has_return_type = 1;
                break;
            default:
                if (sym_name != NULL)
                {
                    if (strcasecmp(sym_name, "class") == 0) {
                        method_template->is_class_method = 1;
                        method_template->is_static = 1;
                    }
                    else if (strcasecmp(sym_name, "static") == 0)
                        method_template->is_static = 1;
                    else if (strcasecmp(sym_name, "constructor") == 0)
                        method_template->kind = METHOD_TEMPLATE_CONSTRUCTOR;
                    else if (strcasecmp(sym_name, "destructor") == 0)
                        method_template->kind = METHOD_TEMPLATE_DESTRUCTOR;
                    else if (strcasecmp(sym_name, "function") == 0)
                        method_template->kind = METHOD_TEMPLATE_FUNCTION;
                    else if (strcasecmp(sym_name, "procedure") == 0)
                        method_template->kind = METHOD_TEMPLATE_PROCEDURE;
                    else if (strcasecmp(sym_name, "operator") == 0)
                        method_template->kind = METHOD_TEMPLATE_OPERATOR;
                }
                break;
        }
        cursor = cursor->next;
    }

    if (method_template->kind == METHOD_TEMPLATE_UNKNOWN)
    {
        method_template->kind = method_template->has_return_type ?
            METHOD_TEMPLATE_FUNCTION : METHOD_TEMPLATE_PROCEDURE;
    }
    if (method_template->return_type_ast == NULL)
    {
        method_template->return_type_ast = find_ast_node_type(method_ast, PASCAL_T_RETURN_TYPE);
        if (method_template->return_type_ast != NULL)
            method_template->has_return_type = 1;
    }
}

static int extract_interface_delegation_info(ast_t *method_decl_node,
    char **iface_name_out, char **iface_method_out, char **target_name_out)
{
    if (iface_name_out != NULL)
        *iface_name_out = NULL;
    if (iface_method_out != NULL)
        *iface_method_out = NULL;
    if (target_name_out != NULL)
        *target_name_out = NULL;
    if (method_decl_node == NULL)
        return 0;

    ast_t *significant[3] = {0};
    int count = 0;
    for (ast_t *cur = method_decl_node->child; cur != NULL; cur = cur->next)
    {
        ast_t *node = unwrap_pascal_node(cur);
        if (node == NULL)
            node = cur;
        if (node == NULL)
            continue;

        if (node->typ == PASCAL_T_PARAM_LIST || node->typ == PASCAL_T_PARAM ||
            node->typ == PASCAL_T_RETURN_TYPE || node->typ == PASCAL_T_METHOD_DIRECTIVE)
            return 0;

        if (node->typ == PASCAL_T_IDENTIFIER || node->typ == PASCAL_T_QUALIFIED_IDENTIFIER ||
            (node->typ == PASCAL_T_NONE && node->child != NULL))
        {
            if (count >= 3)
                return 0;
            significant[count++] = node;
        }
    }

    if (count != 3)
        return 0;

    QualifiedIdent *iface_qid = qualified_ident_from_ast(significant[0]);
    QualifiedIdent *target_qid = qualified_ident_from_ast(significant[2]);
    char *iface_name = iface_qid != NULL ? qualified_ident_join(iface_qid, ".") : dup_symbol(significant[0]);
    char *iface_method = dup_symbol(significant[1]);
    char *target_name = NULL;
    if (target_qid != NULL)
    {
        const char *last = qualified_ident_last(target_qid);
        if (last != NULL)
            target_name = strdup(last);
    }
    if (target_name == NULL)
        target_name = dup_symbol(significant[2]);
    if (iface_qid != NULL)
        qualified_ident_free(iface_qid);
    if (target_qid != NULL)
        qualified_ident_free(target_qid);

    if (iface_name == NULL || iface_method == NULL || target_name == NULL)
    {
        free(iface_name);
        free(iface_method);
        free(target_name);
        return 0;
    }

    if (iface_name_out != NULL)
        *iface_name_out = iface_name;
    else
        free(iface_name);
    if (iface_method_out != NULL)
        *iface_method_out = iface_method;
    else
        free(iface_method);
    if (target_name_out != NULL)
        *target_name_out = target_name;
    else
        free(target_name);
    return 1;
}

struct MethodTemplate *create_method_template(ast_t *method_decl_node)
{
    if (method_decl_node == NULL)
        return NULL;

    struct MethodTemplate *template = (struct MethodTemplate *)calloc(1, sizeof(struct MethodTemplate));
    if (template == NULL)
        return NULL;

    if (!extract_interface_delegation_info(method_decl_node,
            &template->delegated_interface_name, &template->name,
            &template->delegated_target_name))
    {
        ast_t *name_node = method_decl_node->child;
        while (name_node != NULL)
        {
            if (name_node->typ == PASCAL_T_IDENTIFIER)
            {
                const char *sym_name = name_node->sym != NULL ? name_node->sym->name : NULL;
                if (sym_name != NULL &&
                    (is_method_decl_keyword(sym_name) || strcasecmp(sym_name, "generic") == 0))
                {
                    name_node = name_node->next;
                    continue;
                }
                break;
            }
            name_node = name_node->next;
        }
        if (name_node == NULL || name_node->sym == NULL || name_node->sym->name == NULL)
        {
            free(template);
            return NULL;
        }
        template->name = strdup(name_node->sym->name);
        if (template->name == NULL)
        {
            free(template);
            return NULL;
        }
    }
    else
    {
        template->is_interface_delegation = 1;
    }

    template->method_ast = copy_ast_detached(method_decl_node);
    if (template->method_ast == NULL)
    {
        free(template->name);
        free(template->delegated_interface_name);
        free(template->delegated_target_name);
        free(template);
        return NULL;
    }

    annotate_method_template(template, template->method_ast);
    template->method_impl_ast = NULL;
    template->source_offset = g_source_offset;
    template->owns_ast = 1; /* Template owns its detached AST copy */
    return template;
}

void destroy_method_template_instance(struct MethodTemplate *template)
{
    if (template == NULL)
        return;
    free(template->name);
    free(template->delegated_interface_name);
    free(template->delegated_target_name);
    /* Free detached AST copies when this template owns them.
     * Templates created by create_method_template always have owns_ast=1. */
    if (template->owns_ast)
    {
        free_ast_detached(template->method_ast);
        free_ast_detached(template->method_impl_ast);
    }
    if (template->method_tree != NULL)
        destroy_tree(template->method_tree);
    free(template);
}

static void collect_class_members(ast_t *node, const char *class_name,
    ListBuilder *field_builder, ListBuilder *property_builder,
    ListBuilder *method_builder, ListBuilder *nested_type_builder) {
    if (node == NULL)
        return;

    ast_t *cursor = node;
    while (cursor != NULL) {
        ast_t *unwrapped = unwrap_pascal_node(cursor);
        if (unwrapped != NULL) {
            if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
                fprintf(stderr, "[KGPC] collect_class_members: node typ=%d (%s) raw_typ=%d sym=%s in %s\n",
                    unwrapped->typ, pascal_tag_to_string(unwrapped->typ),
                    cursor->typ,
                    (cursor->sym && cursor->sym->name) ? cursor->sym->name : "(null)",
                    class_name ? class_name : "<unknown>");
            }
            switch (unwrapped->typ) {
            case PASCAL_T_CLASS_MEMBER:
            {
                int saw_class = 0;
                int saw_var = 0;
                for (ast_t *scan = unwrapped->child; scan != NULL; scan = scan->next)
                {
                    ast_t *node = unwrap_pascal_node(scan);
                    if (node != NULL && node->sym != NULL && node->sym->name != NULL)
                    {
                        if (strcasecmp(node->sym->name, "class") == 0)
                            saw_class = 1;
                        else if (strcasecmp(node->sym->name, "var") == 0)
                            saw_var = 1;
                    }
                }
                if (saw_class && saw_var)
                {
                    for (ast_t *child = unwrapped->child; child != NULL; child = child->next)
                    {
                        ast_t *node = unwrap_pascal_node(child);
                        if (node != NULL && node->typ == PASCAL_T_FIELD_DECL)
                        {
                            ListNode_t *fields = convert_class_field_decl(node);
                            if (fields != NULL)
                            {
                                for (ListNode_t *fnode = fields; fnode != NULL; fnode = fnode->next)
                                {
                                    if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
                                    {
                                        struct RecordField *field = (struct RecordField *)fnode->cur;
                                        field->is_class_var = 1;
                                    }
                                }
                            }
                            list_builder_extend(field_builder, fields);
                        }
                    }
                }
                else if (saw_class)
                {
                    /* "class function/procedure" member: propagate is_class_method to templates */
                    for (ast_t *child = unwrapped->child; child != NULL; child = child->next)
                    {
                        ast_t *cn = unwrap_pascal_node(child);
                        if (cn == NULL) cn = child;
                        if (cn->typ == PASCAL_T_METHOD_DECL ||
                            cn->typ == PASCAL_T_CONSTRUCTOR_DECL ||
                            cn->typ == PASCAL_T_DESTRUCTOR_DECL)
                        {
                            struct MethodTemplate *template = create_method_template(cn);
                            if (template != NULL)
                            {
                                template->is_class_method = 1;
                                if (!template->is_interface_delegation) {
                                    int param_count = from_cparser_count_params_ast(template->params_ast);
                                    char *param_sig = param_type_signature_from_params_ast(template->params_ast);
                                    register_class_method_ex(class_name, template->name,
                                        template->is_virtual, template->is_override, template->is_static,
                                        template->is_class_method,
                                        param_count, param_sig);
                                }
                                if (method_builder != NULL)
                                    list_builder_append(method_builder, template, LIST_METHOD_TEMPLATE);
                                else
                                    destroy_method_template_instance(template);
                            }
                        }
                        else if (cn->typ == PASCAL_T_PROPERTY_DECL)
                        {
                            struct ClassProperty *property = convert_property_decl(cn);
                            if (property != NULL && property_builder != NULL)
                                list_builder_append(property_builder, property, LIST_CLASS_PROPERTY);
                        }
                    }
                }
                else
                {
                    collect_class_members(unwrapped->child, class_name, field_builder, property_builder,
                        method_builder, nested_type_builder);
                }
                break;
            }
            case PASCAL_T_FIELD_DECL: {
                ListNode_t *fields = convert_class_field_decl(unwrapped);
                list_builder_extend(field_builder, fields);
                break;
            }
            case PASCAL_T_VAR_SECTION: {
                /* Handle var / class var sections inside classes. */
                int is_class_var_section = 0;
                if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR_PARSE") != NULL)
                {
                    fprintf(stderr, "[KGPC] class var section nodes:");
                    for (ast_t *dbg = unwrapped->child; dbg != NULL; dbg = dbg->next)
                    {
                        ast_t *node = unwrap_pascal_node(dbg);
                        const char *name = (node != NULL && node->sym != NULL) ? node->sym->name : NULL;
                        fprintf(stderr, " (%s:%d)", name ? name : "<null>",
                            node != NULL ? node->typ : -1);
                    }
                    fprintf(stderr, "\n");
                }
                for (ast_t *scan = unwrapped->child; scan != NULL; scan = scan->next)
                {
                    ast_t *node = unwrap_pascal_node(scan);
                    if (node != NULL && node->sym != NULL && node->sym->name != NULL &&
                        strcasecmp(node->sym->name, "class") == 0)
                    {
                        is_class_var_section = 1;
                        break;
                    }
                }
                for (ast_t *child = unwrapped->child; child != NULL; child = child->next)
                {
                    ast_t *node = unwrap_pascal_node(child);
                    if (node != NULL && node->typ == PASCAL_T_FIELD_DECL)
                    {
                        ListNode_t *fields = convert_class_field_decl(node);
                        if (is_class_var_section && fields != NULL)
                        {
                            for (ListNode_t *fnode = fields; fnode != NULL; fnode = fnode->next)
                            {
                                if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
                                {
                                    struct RecordField *field = (struct RecordField *)fnode->cur;
                                    field->is_class_var = 1;
                                }
                            }
                        }
                        list_builder_extend(field_builder, fields);
                    }
                }
                break;
            }
            case PASCAL_T_METHOD_DECL:
            case PASCAL_T_CONSTRUCTOR_DECL:
            case PASCAL_T_DESTRUCTOR_DECL: {
                struct MethodTemplate *template = create_method_template(unwrapped);
                if (template == NULL)
                    break;

                if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL && template->name != NULL)
                    fprintf(stderr, "[KGPC] captured template %s.%s\n",
                        class_name != NULL ? class_name : "<unknown>", template->name);

                if (!template->is_interface_delegation) {
                    int param_count = from_cparser_count_params_ast(template->params_ast);
                    char *param_sig = param_type_signature_from_params_ast(template->params_ast);
                    register_class_method_ex(class_name, template->name,
                        template->is_virtual, template->is_override, template->is_static,
                        template->is_class_method,
                        param_count, param_sig);
                }

                if (method_builder != NULL)
                    list_builder_append(method_builder, template, LIST_METHOD_TEMPLATE);
                else
                    destroy_method_template_instance(template);
                break;
            }
            case PASCAL_T_PROPERTY_DECL: {
                struct ClassProperty *property = convert_property_decl(unwrapped);
                if (property != NULL && property_builder != NULL)
                    list_builder_append(property_builder, property, LIST_CLASS_PROPERTY);
                break;
            }
            case PASCAL_T_TYPE_SECTION:
            case PASCAL_T_NESTED_TYPE_SECTION: {
                /* Nested type declarations inside record/class (Delphi syntax: public type ...) */
                if (nested_type_builder != NULL) {
                    /* Store a pointer to the AST node for later processing */
                    list_builder_append(nested_type_builder, unwrapped, LIST_UNSPECIFIED);
                    if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL)
                        fprintf(stderr, "[KGPC] collect_class_members: found NESTED_TYPE_SECTION in %s at line %d\n",
                            class_name ? class_name : "<unknown>", unwrapped->line);
                }
                break;
            }
            case PASCAL_T_CLASS_BODY:
            case PASCAL_T_ACCESS_MODIFIER:
            case PASCAL_T_PRIVATE_SECTION:
            case PASCAL_T_PUBLIC_SECTION:
            case PASCAL_T_PROTECTED_SECTION:
            case PASCAL_T_PUBLISHED_SECTION:
                collect_class_members(unwrapped->child, class_name, field_builder,
                    property_builder, method_builder, nested_type_builder);
                break;
            default:
                break;
            }
        }
        cursor = cursor->next;
    }
}

struct RecordType *convert_class_type_ex(const char *class_name, ast_t *class_node, ListNode_t **nested_types_out) {
    if (class_node == NULL)
        return NULL;

    if (nested_types_out != NULL)
        *nested_types_out = NULL;

    if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL && class_name != NULL)
        fprintf(stderr, "[KGPC] convert_class_type %s\n", class_name);

    ListBuilder field_builder;
    ListBuilder property_builder;
    ListBuilder method_template_builder;
    ListBuilder nested_type_builder;
    list_builder_init(&field_builder);
    list_builder_init(&property_builder);
    list_builder_init(&method_template_builder);
    list_builder_init(&nested_type_builder);

    // Check if first child is a parent class identifier
    char *parent_class_name = NULL;
    ast_t *body_start = class_node->child;

    while (body_start != NULL && body_start->typ == PASCAL_T_NONE &&
        body_start->child != NULL && body_start->next == NULL)
    {
        body_start = body_start->child;
    }

    if (body_start != NULL && body_start->typ == PASCAL_T_IDENTIFIER) {
        // First child is parent class name, extract it
        if (body_start->sym != NULL && body_start->sym->name != NULL)
            parent_class_name = strdup(body_start->sym->name);
        // Move to the actual class body (next sibling)
        body_start = body_start->next;
    }

    while (body_start != NULL && body_start->typ == PASCAL_T_NONE &&
        body_start->child != NULL && body_start->next == NULL)
    {
        body_start = body_start->child;
    }

    /* Collect additional IDENTIFIER siblings as interface names */
    int iface_count = 0;
    int iface_cap = 0;
    char **iface_names = NULL;
    {
        ast_t *scan = body_start;
        while (scan != NULL && scan->typ == PASCAL_T_IDENTIFIER) {
            if (scan->sym != NULL && scan->sym->name != NULL) {
                if (iface_count >= iface_cap) {
                    iface_cap = (iface_cap == 0) ? 4 : iface_cap * 2;
                    iface_names = (char **)realloc(iface_names, iface_cap * sizeof(char *));
                }
                iface_names[iface_count++] = strdup(scan->sym->name);
            }
            scan = scan->next;
        }
        body_start = scan;
    }

    if (kgpc_getenv("KGPC_DEBUG_CLASS_METHODS") != NULL) {
        fprintf(stderr, "[KGPC] convert_class_type: processing class %s\n", class_name ? class_name : "<null>");
        if (body_start != NULL) {
            fprintf(stderr, "[KGPC]   body_start type: %d\n", body_start->typ);
        } else {
            fprintf(stderr, "[KGPC]   body_start is NULL\n");
        }
        if (class_name != NULL &&
            (strcasecmp(class_name, "TList") == 0 ||
             strcasecmp(class_name, "TStringList") == 0 ||
             strcasecmp(class_name, "TComponent") == 0 ||
             strcasecmp(class_name, "TInterfaceList") == 0)) {
            ast_t *raw = class_node->child;
            int idx = 0;
            while (raw != NULL && idx < 12) {
                fprintf(stderr, "[KGPC]   raw[%d] typ=%d name=%s child=%p next=%p\n",
                    idx, raw->typ,
                    (raw->sym && raw->sym->name) ? raw->sym->name : "<null>",
                    (void *)raw->child, (void *)raw->next);
                raw = raw->next;
                idx++;
            }
        }
        /* Debug: show additional parent identifiers (interfaces) */
        ast_t *dbg = body_start;
        while (dbg != NULL && dbg->typ == PASCAL_T_IDENTIFIER) {
            fprintf(stderr, "[KGPC]   additional parent/interface: %s (type=%d)\n",
                (dbg->sym && dbg->sym->name) ? dbg->sym->name : "<null>", dbg->typ);
            dbg = dbg->next;
        }
    }

    collect_class_members(body_start, class_name, &field_builder, &property_builder,
        &method_template_builder, &nested_type_builder);

    /* Output collected nested type sections */
    if (nested_types_out != NULL)
        *nested_types_out = list_builder_finish(&nested_type_builder);
    else
        destroy_list(nested_type_builder.head);

    struct RecordType *record = (struct RecordType *)malloc(sizeof(struct RecordType));
    if (record == NULL) {
        destroy_list(field_builder.head);
        destroy_list(property_builder.head);
        destroy_list(method_template_builder.head);
        free(parent_class_name);
        return NULL;
    }

    record->fields = list_builder_finish(&field_builder);
    record->properties = list_builder_finish(&property_builder);
    record->method_templates = list_builder_finish(&method_template_builder);
    
    record->parent_class_name = parent_class_name;
    record->methods = NULL;  /* Methods list will be populated during semantic checking */
    record->is_class = 1;
    record->is_interface = 0;
    record->is_packed = 0;
    record->is_type_helper = 0;
    record->helper_base_type_id = NULL;
    record->helper_parent_id = NULL;
    record->type_id = class_name != NULL ? strdup(class_name) : NULL;
    record->outer_type_id = NULL;
    record->has_cached_size = 0;
    record->cached_size = 0;
    record->generic_decl = NULL;
    record->generic_args = NULL;
    record->num_generic_args = 0;
    record->is_generic_specialization = 0;
    record->method_clones_emitted = 0;
    record->parent_fields_merged = 0;
    record->source_unit_index = 0;
    record->default_indexed_property = NULL;
    record->default_indexed_element_type = UNKNOWN_TYPE;
    record->default_indexed_element_type_id = NULL;
    record->record_properties = NULL;
    record->guid_string = NULL;
    record->has_guid = 0;
    record->guid_d1 = 0;
    record->guid_d2 = 0;
    record->guid_d3 = 0;
    memset(record->guid_d4, 0, sizeof(record->guid_d4));
    record->has_guid = 0;
    record->guid_d1 = 0;
    record->guid_d2 = 0;
    record->guid_d3 = 0;
    memset(record->guid_d4, 0, sizeof(record->guid_d4));
    record->interface_names = iface_names;
    record->num_interfaces = iface_count;

    if (parent_class_name == NULL)
    {
        struct RecordField *typeinfo_field = (struct RecordField *)calloc(1, sizeof(struct RecordField));
        if (typeinfo_field != NULL)
        {
            typeinfo_field->name = strdup("__kgpc_class_typeinfo");
            typeinfo_field->type = POINTER_TYPE;
            typeinfo_field->type_id = NULL;
            typeinfo_field->nested_record = NULL;
            typeinfo_field->is_array = 0;
            typeinfo_field->array_start = 0;
            typeinfo_field->array_end = 0;
            typeinfo_field->array_element_type = UNKNOWN_TYPE;
            typeinfo_field->array_element_type_id = NULL;
            typeinfo_field->array_is_open = 0;
            typeinfo_field->is_hidden = 1;
            ListNode_t *node = CreateListNode(typeinfo_field, LIST_RECORD_FIELD);
            if (node != NULL) {
                if (record->fields == NULL) {
                    record->fields = node;
                } else {
                    record->fields = PushListNodeFront(record->fields, node);
                }
            }
            else
            {
                free(typeinfo_field->name);
                free(typeinfo_field);
            }
        }
    }
    return record;
}

struct RecordType *convert_interface_type_ex(const char *interface_name, ast_t *interface_node, ListNode_t **nested_types_out)
{
    if (interface_node == NULL)
        return NULL;

    if (nested_types_out != NULL)
        *nested_types_out = NULL;

    ListBuilder field_builder;
    ListBuilder property_builder;
    ListBuilder method_template_builder;
    ListBuilder nested_type_builder;
    list_builder_init(&field_builder);
    list_builder_init(&property_builder);
    list_builder_init(&method_template_builder);
    list_builder_init(&nested_type_builder);

    /* Optional parent interface identifier is stored first if present. */
    char *parent_interface_name = NULL;
    char *guid_string = NULL;
    int has_guid = 0;
    uint32_t guid_d1 = 0;
    uint16_t guid_d2 = 0;
    uint16_t guid_d3 = 0;
    uint8_t guid_d4[8] = {0};
    ast_t *body_start = interface_node->child;
    /* Skip optional GUID attribute: ['{...}'] or [SGUIDConst].
     * With PASCAL_T_INTERFACE_GUID, the GUID is now wrapped in its own node. */
    if (body_start != NULL && body_start->typ == PASCAL_T_INTERFACE_GUID) {
        /* Extract GUID string from inside the GUID node if available */
        ast_t *guid_child = body_start->child;
        while (guid_child != NULL) {
            if ((guid_child->typ == PASCAL_T_STRING ||
                 guid_child->typ == PASCAL_T_IDENTIFIER) &&
                guid_child->sym != NULL && guid_child->sym->name != NULL) {
                guid_string = strdup(guid_child->sym->name);
                has_guid = parse_guid_literal(guid_child->sym->name, &guid_d1, &guid_d2, &guid_d3, guid_d4);
                break;
            }
            guid_child = guid_child->next;
        }
        body_start = body_start->next;
    }

    if (body_start != NULL && body_start->typ == PASCAL_T_IDENTIFIER) {
        if (body_start->sym != NULL && body_start->sym->name != NULL)
            parent_interface_name = strdup(body_start->sym->name);
        body_start = body_start->next;
    }

    /* Optional GUID string follows parent identifier (when GUID is a string literal) */
    if (body_start != NULL && body_start->typ == PASCAL_T_STRING) {
        if (body_start->sym != NULL && body_start->sym->name != NULL) {
            free(guid_string);
            guid_string = strdup(body_start->sym->name);
            has_guid = parse_guid_literal(body_start->sym->name, &guid_d1, &guid_d2, &guid_d3, guid_d4);
        }
        body_start = body_start->next;
    }

    /* Skip GUID attribute if it follows the parent identifier */
    if (body_start != NULL && body_start->typ == PASCAL_T_INTERFACE_GUID) {
        ast_t *guid_child = body_start->child;
        while (guid_child != NULL && !has_guid) {
            if ((guid_child->typ == PASCAL_T_STRING ||
                 guid_child->typ == PASCAL_T_IDENTIFIER) &&
                guid_child->sym != NULL && guid_child->sym->name != NULL) {
                free(guid_string);
                guid_string = strdup(guid_child->sym->name);
                has_guid = parse_guid_literal(guid_child->sym->name, &guid_d1, &guid_d2, &guid_d3, guid_d4);
                break;
            }
            guid_child = guid_child->next;
        }
        body_start = body_start->next;
    }

    collect_class_members(body_start, interface_name, &field_builder, &property_builder,
        &method_template_builder, &nested_type_builder);

    if (nested_types_out != NULL)
        *nested_types_out = list_builder_finish(&nested_type_builder);
    else
        destroy_list(nested_type_builder.head);

    struct RecordType *record = (struct RecordType *)calloc(1, sizeof(struct RecordType));
    if (record == NULL) {
        destroy_list(field_builder.head);
        destroy_list(property_builder.head);
        destroy_list(method_template_builder.head);
        free(parent_interface_name);
        return NULL;
    }

    record->fields = list_builder_finish(&field_builder);
    record->properties = list_builder_finish(&property_builder);
    record->method_templates = list_builder_finish(&method_template_builder);
    record->parent_class_name = parent_interface_name;
    /* In FPC, interfaces without an explicit parent implicitly inherit from
     * IInterface (a.k.a. IUnknown).  Set the default parent so that methods
     * like _AddRef, _Release, QueryInterface are found by the class-method
     * walker when resolving field access on derived interfaces. */
    if (record->parent_class_name == NULL && interface_name != NULL &&
        !pascal_identifier_equals(interface_name, "IInterface") &&
        !pascal_identifier_equals(interface_name, "IUnknown"))
    {
        record->parent_class_name = strdup("IInterface");
    }
    record->methods = NULL;
    record->is_class = 0;
    record->is_interface = 1;
    record->is_packed = 0;
    record->is_type_helper = 0;
    record->helper_base_type_id = NULL;
    record->helper_parent_id = NULL;
    record->type_id = interface_name != NULL ? strdup(interface_name) : NULL;
    record->has_cached_size = 0;
    record->cached_size = 0;
    record->generic_decl = NULL;
    record->generic_args = NULL;
    record->num_generic_args = 0;
    record->is_generic_specialization = 0;
    record->is_generic_specialization = 0;
    record->method_clones_emitted = 0;
    record->parent_fields_merged = 0;
    record->default_indexed_property = NULL;
    record->default_indexed_element_type = UNKNOWN_TYPE;
    record->default_indexed_element_type_id = NULL;
    record->record_properties = NULL;
    record->guid_string = guid_string;
    record->has_guid = has_guid;
    record->guid_d1 = guid_d1;
    record->guid_d2 = guid_d2;
    record->guid_d3 = guid_d3;
    memcpy(record->guid_d4, guid_d4, sizeof(record->guid_d4));
    record->interface_names = NULL;
    record->num_interfaces = 0;

    return record;
}

static ListNode_t *convert_field_decl(ast_t *field_decl_node) {
    if (field_decl_node == NULL || field_decl_node->typ != PASCAL_T_FIELD_DECL)
        return NULL;

    int is_class_var = 0;
    int saw_class = 0;
    int saw_var = 0;
    if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR_PARSE") != NULL)
    {
        fprintf(stderr, "[KGPC] field_decl identifiers:");
        for (ast_t *dbg = field_decl_node->child; dbg != NULL; dbg = dbg->next)
        {
            ast_t *node = unwrap_pascal_node(dbg);
            if (node != NULL && node->sym != NULL && node->sym->name != NULL)
                fprintf(stderr, " %s", node->sym->name);
        }
        fprintf(stderr, "\n");
    }
    for (ast_t *scan = field_decl_node->child; scan != NULL; scan = scan->next)
    {
        ast_t *node = unwrap_pascal_node(scan);
        if (node != NULL && node->sym != NULL && node->sym->name != NULL)
        {
            if (strcasecmp(node->sym->name, "class") == 0)
                saw_class = 1;
            else if (strcasecmp(node->sym->name, "var") == 0)
                saw_var = 1;
        }
    }
    if (saw_class && saw_var)
        is_class_var = 1;
    ast_t *cursor = field_decl_node->child;
    while (cursor != NULL)
    {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node == NULL)
        {
            cursor = cursor->next;
            continue;
        }
        if (node->typ == PASCAL_T_IDENTIFIER &&
            node->sym != NULL && node->sym->name != NULL &&
            (strcasecmp(node->sym->name, "class") == 0 ||
             strcasecmp(node->sym->name, "var") == 0))
        {
            cursor = cursor->next;
            continue;
        }
        if (node->typ != PASCAL_T_IDENTIFIER)
        {
            cursor = cursor->next;
            continue;
        }
        break;
    }
    ListNode_t *names = convert_identifier_list(&cursor);
    if (names == NULL) {
        fprintf(stderr, "ERROR: record field declaration missing identifier list.\n");
        return NULL;
    }

    while (cursor != NULL && cursor->typ != PASCAL_T_TYPE_SPEC &&
           cursor->typ != PASCAL_T_RECORD_TYPE && cursor->typ != PASCAL_T_OBJECT_TYPE &&
           cursor->typ != PASCAL_T_IDENTIFIER && cursor->typ != PASCAL_T_ARRAY_TYPE &&
           cursor->typ != PASCAL_T_SET &&
           cursor->typ != PASCAL_T_POINTER_TYPE &&
           cursor->typ != PASCAL_T_ENUMERATED_TYPE &&
           cursor->typ != PASCAL_T_FILE_TYPE &&
           cursor->typ != PASCAL_T_PROCEDURE_TYPE &&
           cursor->typ != PASCAL_T_FUNCTION_TYPE &&
           cursor->typ != PASCAL_T_REFERENCE_TO_TYPE) {
        cursor = cursor->next;
    }

    char *field_type_id = NULL;
    struct RecordType *nested_record = NULL;
    TypeInfo field_info;
    memset(&field_info, 0, sizeof(TypeInfo));
    int field_type = UNKNOWN_TYPE;
    KgpcType *inline_proc_type = NULL;

    if (cursor != NULL) {
        field_type = convert_type_spec(cursor, &field_type_id, &nested_record, &field_info);
        if (field_type == UNKNOWN_TYPE && field_info.is_range)
            field_type = select_range_primitive_tag(&field_info);
        /* Capture inline procedural type signatures for record fields */
        {
            ast_t *spec_node = cursor;
            if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
                spec_node = spec_node->child;
            spec_node = unwrap_pascal_node(spec_node);
            if (spec_node != NULL &&
                (spec_node->typ == PASCAL_T_PROCEDURE_TYPE ||
                 spec_node->typ == PASCAL_T_FUNCTION_TYPE ||
                 spec_node->typ == PASCAL_T_REFERENCE_TO_TYPE))
            {
                inline_proc_type = convert_type_spec_to_kgpctype(cursor, NULL);
            }
        }
    } else if (names != NULL) {
        char *candidate = pop_last_identifier(&names);
        if (candidate != NULL) {
            char *mapped_id = NULL;
            int mapped_type = map_type_name(candidate, &mapped_id);
            if (mapped_type != UNKNOWN_TYPE) {
                mapped_type = apply_shortstring_mode(mapped_type, candidate);
                field_type = mapped_type;
                field_type_id = mapped_id;
                free(candidate);
            } else {
                free(mapped_id);
                field_type_id = candidate;
            }
        }
    }

    ListBuilder result_builder;
    list_builder_init(&result_builder);

    ListNode_t *name_node = names;
    while (name_node != NULL) {
        char *field_name = (char *)name_node->cur;
        char *type_id_copy = NULL;
        if (field_type_id != NULL)
            type_id_copy = strdup(field_type_id);

        struct RecordType *nested_copy = NULL;
        if (nested_record != NULL) {
            if (name_node->next == NULL) {
                nested_copy = nested_record;
                nested_record = NULL;
            } else {
                nested_copy = clone_record_type(nested_record);
            }
        }

        struct RecordField *field_desc = (struct RecordField *)calloc(1, sizeof(struct RecordField));
        if (field_desc != NULL) {
            field_desc->name = field_name;
            field_desc->type = field_type;
            /* string[N] fields are shortstrings (array[0..N] of Char with length byte) */
            if (field_desc->type == UNKNOWN_TYPE && field_info.is_shortstring)
                field_desc->type = SHORTSTRING_TYPE;
            field_desc->type_id = type_id_copy;
            field_desc->type_ref = type_ref_from_info_or_id(&field_info, type_id_copy);
            field_desc->nested_record = nested_copy;
            field_desc->proc_type = inline_proc_type;
            if (field_desc->proc_type != NULL)
                kgpc_type_retain(field_desc->proc_type);
            field_desc->is_array = field_info.is_array;
            field_desc->array_start = field_info.start;
            field_desc->array_end = field_info.end;
            field_desc->array_element_type = field_info.element_type;
            field_desc->array_element_type_ref =
                type_ref_from_element_info(&field_info, field_info.element_type_id);
            field_desc->array_is_open = field_info.is_open_array;
            /* For multi-name fields (e.g. a, b: array[0..1] of T),
             * duplicate shared resources; transfer ownership only for the last name. */
            if (name_node->next == NULL)
            {
                field_desc->array_element_type_id = field_info.element_type_id;
                field_info.element_type_id = NULL;
                field_desc->array_element_record = field_info.record_type;
                field_info.record_type = NULL;
            }
            else
            {
                field_desc->array_element_type_id =
                    field_info.element_type_id ? strdup(field_info.element_type_id) : NULL;
                field_desc->array_element_record =
                    field_info.record_type ? clone_record_type(field_info.record_type) : NULL;
            }
            /* Transfer pre-built element KgpcType for nested arrays */
            if (field_info.element_kgpc_type != NULL)
            {
                field_desc->array_element_kgpc_type = field_info.element_kgpc_type;
                kgpc_type_retain(field_desc->array_element_kgpc_type);
            }
            field_desc->is_class_var = is_class_var;
            /* Copy pointer type info for inline pointer fields like ^Char */
            field_desc->is_pointer = field_info.is_pointer;
            field_desc->pointer_type = field_info.pointer_type;
            if (field_info.pointer_type_id != NULL)
                field_desc->pointer_type_id = strdup(field_info.pointer_type_id);
            else
                field_desc->pointer_type_id = NULL;
            field_desc->pointer_type_ref =
                type_ref_from_pointer_info(&field_info, field_info.pointer_type_id);
            /* Transfer anonymous enum values for fields like `kind: (a, b, c)` */
            if (field_info.enum_literals != NULL && name_node->next == NULL)
            {
                field_desc->enum_literals = field_info.enum_literals;
                field_info.enum_literals = NULL;
            }
            else if (field_info.enum_literals != NULL)
            {
                /* Clone for multi-name fields (e.g. a, b: (x, y, z)) */
                ListBuilder clone_builder;
                list_builder_init(&clone_builder);
                for (ListNode_t *en = field_info.enum_literals; en != NULL; en = en->next)
                    if (en->cur != NULL)
                        list_builder_append(&clone_builder, strdup((char *)en->cur), LIST_STRING);
                field_desc->enum_literals = list_builder_finish(&clone_builder);
            }
            /* Copy set element type info for proper set size computation */
            if (field_info.is_set && field_info.set_element_type_id != NULL)
                field_desc->set_element_type_id = strdup(field_info.set_element_type_id);
            list_builder_append(&result_builder, field_desc, LIST_RECORD_FIELD);
        } else {
            if (field_name != NULL)
                free(field_name);
            if (type_id_copy != NULL)
                free(type_id_copy);
            destroy_record_type(nested_copy);
        }

        ListNode_t *next_name = name_node->next;
        free(name_node);
        name_node = next_name;
    }

    if (field_type_id != NULL)
        free(field_type_id);
    if (nested_record != NULL)
        destroy_record_type(nested_record);
    if (inline_proc_type != NULL)
        kgpc_type_release(inline_proc_type);
    destroy_type_info_contents(&field_info);

    return list_builder_finish(&result_builder);
}

static ListNode_t *convert_variant_labels(ast_t *labels_node) {
    if (labels_node == NULL)
        return NULL;

    ListBuilder labels_builder;
    list_builder_init(&labels_builder);

    ast_t *label_cursor = labels_node;
    while (label_cursor != NULL) {
        append_case_label(&labels_builder, label_cursor);
        label_cursor = label_cursor->next;
    }

    return list_builder_finish(&labels_builder);
}

static struct VariantBranch *convert_variant_branch(ast_t *branch_node) {
    if (branch_node == NULL || branch_node->typ != PASCAL_T_VARIANT_BRANCH)
        return NULL;

    struct VariantBranch *branch = (struct VariantBranch *)calloc(1, sizeof(struct VariantBranch));
    if (branch == NULL)
        return NULL;

    ast_t *cursor = branch_node->child;
    if (cursor != NULL && cursor->typ == PASCAL_T_CASE_LABEL_LIST) {
        branch->labels = convert_variant_labels(cursor->child);
        cursor = cursor->next;
    } else if (cursor != NULL && cursor->typ == PASCAL_T_CASE_LABEL) {
        branch->labels = convert_variant_labels(cursor);
        cursor = cursor->next;
    }

    ListBuilder members_builder;
    list_builder_init(&members_builder);
    convert_record_members(cursor, &members_builder, NULL, NULL);
    branch->members = list_builder_finish(&members_builder);

    return branch;
}

static struct VariantPart *convert_variant_part(ast_t *variant_node, ListNode_t **out_tag_fields) {
    if (out_tag_fields != NULL)
        *out_tag_fields = NULL;

    if (variant_node == NULL || variant_node->typ != PASCAL_T_VARIANT_PART)
        return NULL;

    struct VariantPart *variant = (struct VariantPart *)calloc(1, sizeof(struct VariantPart));
    if (variant == NULL)
        return NULL;

    ast_t *cursor = variant_node->child;
    if (cursor != NULL && cursor->typ == PASCAL_T_VARIANT_TAG) {
        ast_t *tag_cursor = cursor->child;
        char *tag_name = NULL;
        ast_t *type_cursor = tag_cursor;
        if (tag_cursor != NULL && tag_cursor->typ == PASCAL_T_IDENTIFIER && tag_cursor->next != NULL) {
            tag_name = dup_symbol(tag_cursor);
            type_cursor = tag_cursor->next;
        }

        char *tag_type_id = NULL;
        struct RecordType *tag_record = NULL;
        int tag_type = UNKNOWN_TYPE;
        if (type_cursor != NULL)
            tag_type = convert_type_spec(type_cursor, &tag_type_id, &tag_record, NULL);

        if (tag_name != NULL) {
            struct RecordField *tag_field = (struct RecordField *)calloc(1, sizeof(struct RecordField));
            if (tag_field != NULL) {
                tag_field->name = tag_name;
                tag_field->type = tag_type;
                tag_field->type_id = (tag_type_id != NULL) ? strdup(tag_type_id) : NULL;
                tag_field->nested_record = tag_record;
                tag_field->is_array = 0;
                tag_field->array_start = 0;
                tag_field->array_end = 0;
                tag_field->array_element_type = UNKNOWN_TYPE;
                tag_field->array_element_type_id = NULL;
                tag_field->array_is_open = 0;

                ListNode_t *field_node = CreateListNode(tag_field, LIST_RECORD_FIELD);
                if (out_tag_fields != NULL)
                    *out_tag_fields = field_node;
                variant->tag_field = tag_field;
                variant->tag_type = tag_type;
                variant->tag_type_id = NULL;
                variant->tag_record = NULL;
                tag_record = NULL;
            } else {
                free(tag_name);
                destroy_record_type(tag_record);
            }
        } else {
            variant->tag_field = NULL;
            variant->tag_type = tag_type;
            variant->tag_type_id = tag_type_id;
            variant->tag_record = tag_record;
            tag_type_id = NULL;
            tag_record = NULL;
        }

        variant->tag_type = tag_type;

        if (tag_type_id != NULL)
            free(tag_type_id);
        if (tag_record != NULL)
            destroy_record_type(tag_record);

        cursor = cursor->next;
    }

    ListBuilder branches_builder;
    list_builder_init(&branches_builder);
    for (; cursor != NULL; cursor = cursor->next) {
        if (cursor->typ != PASCAL_T_VARIANT_BRANCH)
            continue;
        struct VariantBranch *branch = convert_variant_branch(cursor);
        if (branch != NULL)
            list_builder_append(&branches_builder, branch, LIST_VARIANT_BRANCH);
    }
    variant->branches = list_builder_finish(&branches_builder);
    variant->has_cached_size = 0;
    variant->cached_size = 0;

    return variant;
}

void convert_record_members(ast_t *node, ListBuilder *builder,
    ListBuilder *property_builder, ListBuilder *method_template_builder) {
    for (ast_t *cur = node; cur != NULL; cur = cur->next) {
        if (cur->typ == PASCAL_T_FIELD_DECL) {
            ListNode_t *fields = convert_field_decl(cur);
            list_builder_extend(builder, fields);
        } else if (cur->typ == PASCAL_T_VARIANT_PART) {
            ListNode_t *tag_fields = NULL;
            struct VariantPart *variant = convert_variant_part(cur, &tag_fields);
            list_builder_extend(builder, tag_fields);
            if (variant != NULL)
                list_builder_append(builder, variant, LIST_VARIANT_PART);
        } else if (cur->typ == PASCAL_T_METHOD_DECL ||
                   cur->typ == PASCAL_T_CONSTRUCTOR_DECL ||
                   cur->typ == PASCAL_T_DESTRUCTOR_DECL) {
            /* Store method declaration as a special marker node for operator overloading */
            /* We'll handle this during semantic check when we know the record type name */
            list_builder_append(builder, cur, LIST_UNSPECIFIED);
            if (method_template_builder != NULL) {
                struct MethodTemplate *template = create_method_template(cur);
                if (template != NULL)
                    list_builder_append(method_template_builder, template, LIST_METHOD_TEMPLATE);
            }
        } else if (cur->typ == PASCAL_T_PROPERTY_DECL && property_builder != NULL) {
            struct ClassProperty *property = convert_property_decl(cur);
            if (property != NULL)
                list_builder_append(property_builder, property, LIST_CLASS_PROPERTY);
        } else if (cur->typ == PASCAL_T_VAR_SECTION) {
            /* Handle var / class var / class threadvar sections inside objects.
             * The VAR_SECTION wraps keyword nodes and FIELD_DECL children. */
            int is_class_var_section = 0;
            if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR_PARSE") != NULL)
            {
                fprintf(stderr, "[KGPC] var section nodes:");
                for (ast_t *dbg = cur->child; dbg != NULL; dbg = dbg->next)
                {
                    ast_t *node = unwrap_pascal_node(dbg);
                    const char *name = (node != NULL && node->sym != NULL) ? node->sym->name : NULL;
                    fprintf(stderr, " (%s:%d)", name ? name : "<null>",
                        node != NULL ? node->typ : -1);
                }
                fprintf(stderr, "\n");
            }
            for (ast_t *scan = cur->child; scan != NULL; scan = scan->next) {
                ast_t *node = unwrap_pascal_node(scan);
                if (node != NULL && node->sym != NULL && node->sym->name != NULL &&
                    strcasecmp(node->sym->name, "class") == 0)
                {
                    is_class_var_section = 1;
                    break;
                }
            }
            for (ast_t *child = cur->child; child != NULL; child = child->next) {
                if (child->typ == PASCAL_T_FIELD_DECL) {
                    ListNode_t *fields = convert_field_decl(child);
                    if (is_class_var_section && fields != NULL)
                    {
                        for (ListNode_t *fnode = fields; fnode != NULL; fnode = fnode->next)
                        {
                            if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
                            {
                                struct RecordField *field = (struct RecordField *)fnode->cur;
                                field->is_class_var = 1;
                            }
                        }
                    }
                    list_builder_extend(builder, fields);
                }
            }
        } else if (cur->typ == PASCAL_T_CLASS_MEMBER) {
            /* CLASS_MEMBER may wrap either:
             * 1. Visibility sections (e.g. public/private) — recurse
             * 2. "class var" fields from object types — set is_class_var on each */
            int has_field_decls = 0;
            for (ast_t *chk = cur->child; chk != NULL; chk = chk->next) {
                if (chk->typ == PASCAL_T_FIELD_DECL) { has_field_decls = 1; break; }
            }
            if (has_field_decls) {
                /* This is a "class var" section — convert fields and mark is_class_var */
                for (ast_t *child = cur->child; child != NULL; child = child->next) {
                    if (child->typ == PASCAL_T_FIELD_DECL) {
                        ListNode_t *fields = convert_field_decl(child);
                        if (fields != NULL) {
                            for (ListNode_t *fnode = fields; fnode != NULL; fnode = fnode->next) {
                                if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL) {
                                    struct RecordField *field = (struct RecordField *)fnode->cur;
                                    field->is_class_var = 1;
                                }
                            }
                            list_builder_extend(builder, fields);
                        }
                    }
                }
            } else {
                convert_record_members(cur->child, builder, property_builder, method_template_builder);
            }
        }
    }
}

/* Helper: scan record AST for nested type sections (Delphi advanced records) */
void collect_record_nested_types(ast_t *node, ListBuilder *nested_type_builder) {
    for (ast_t *cur = node; cur != NULL; cur = cur->next) {
        ast_t *unwrapped = unwrap_pascal_node(cur);
        if (unwrapped == NULL)
            continue;

        if (unwrapped->typ == PASCAL_T_CLASS_MEMBER) {
            /* Recurse into CLASS_MEMBER nodes */
            collect_record_nested_types(unwrapped->child, nested_type_builder);
        } else if (unwrapped->typ == PASCAL_T_NESTED_TYPE_SECTION || unwrapped->typ == PASCAL_T_TYPE_SECTION) {
            /* Found a nested type section */
            if (nested_type_builder != NULL) {
                list_builder_append(nested_type_builder, unwrapped, LIST_UNSPECIFIED);
                if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL)
                    fprintf(stderr, "[KGPC] collect_record_nested_types: found NESTED_TYPE_SECTION at line %d\n",
                        unwrapped->line);
            }
        }
    }
}

struct RecordType *convert_record_type_ex(ast_t *record_node, ListNode_t **nested_types_out) {
    if (record_node == NULL)
        return NULL;

    if (nested_types_out != NULL)
        *nested_types_out = NULL;

    if (record_node->sym != NULL &&
        record_node->sym->name != NULL &&
        strcasecmp(record_node->sym->name, "helper") == 0)
    {
        struct RecordType *record = (struct RecordType *)calloc(1, sizeof(struct RecordType));
        if (record == NULL)
            return NULL;
        ListBuilder helper_fields_builder;
        list_builder_init(&helper_fields_builder);
        ListBuilder helper_properties_builder;
        list_builder_init(&helper_properties_builder);
        ListBuilder method_template_builder;
        list_builder_init(&method_template_builder);
        record->fields = NULL;
        record->properties = NULL;
        record->parent_class_name = NULL;
        record->methods = NULL;
        record->method_templates = NULL;
        record->is_class = 0;
        record->is_interface = 0;
        record->is_packed = 0;
        record->is_type_helper = 1;
        record->helper_base_type_id = NULL;
        record->helper_parent_id = NULL;
        record->is_generic_specialization = 0;
        record->type_id = NULL;
        record->has_cached_size = 0;
        record->cached_size = 0;
        record->generic_decl = NULL;
        record->generic_args = NULL;
        record->num_generic_args = 0;
        record->method_clones_emitted = 0;

        /* Parse helper children:
         * For "type helper(ParentHelper) for BaseType":
         *   - First identifier: ParentHelper
         *   - Second identifier: BaseType
         * For "type helper for BaseType":
         *   - Only one identifier: BaseType
         */
        ast_t *first_ident = NULL;
        ast_t *second_ident = NULL;
        ast_t *base_node = record_node->child;
        
        while (base_node != NULL) {
            ast_t *unwrapped = unwrap_pascal_node(base_node);
            ast_t *type_ident = unwrapped;
            while (type_ident != NULL && type_ident->typ == PASCAL_T_TYPE_SPEC &&
                type_ident->child != NULL)
            {
                type_ident = unwrap_pascal_node(type_ident->child);
            }
            if (type_ident != NULL &&
                (type_ident->typ == PASCAL_T_IDENTIFIER || type_ident->typ == PASCAL_T_QUALIFIED_IDENTIFIER) &&
                type_ident->sym != NULL && type_ident->sym->name != NULL)
            {
                if (first_ident == NULL) {
                    first_ident = type_ident;
                } else if (second_ident == NULL) {
                    second_ident = type_ident;
                }
                base_node = base_node->next;
                continue;
            }

            if (unwrapped != NULL && unwrapped->typ == PASCAL_T_CLASS_MEMBER)
            {
                ast_t *member = unwrapped->child;
                while (member != NULL)
                {
                    ast_t *member_unwrapped = unwrap_pascal_node(member);
                    if (member_unwrapped != NULL)
                    {
                        if (member_unwrapped->typ == PASCAL_T_METHOD_DECL ||
                            member_unwrapped->typ == PASCAL_T_CONSTRUCTOR_DECL ||
                            member_unwrapped->typ == PASCAL_T_DESTRUCTOR_DECL)
                        {
                            list_builder_append(&helper_fields_builder, member_unwrapped, LIST_UNSPECIFIED);
                            struct MethodTemplate *template = create_method_template(member_unwrapped);
                            if (template != NULL) {
                                list_builder_append(&method_template_builder, template, LIST_METHOD_TEMPLATE);
                            }
                        }
                        else if (member_unwrapped->typ == PASCAL_T_PROPERTY_DECL)
                        {
                            struct ClassProperty *property = convert_property_decl(member_unwrapped);
                            if (property != NULL)
                                list_builder_append(&helper_properties_builder, property, LIST_CLASS_PROPERTY);
                        }
                    }
                    member = member->next;
                }
                base_node = base_node->next;
                continue;
            }

            if (unwrapped != NULL &&
                (unwrapped->typ == PASCAL_T_METHOD_DECL ||
                 unwrapped->typ == PASCAL_T_CONSTRUCTOR_DECL ||
                 unwrapped->typ == PASCAL_T_DESTRUCTOR_DECL))
            {
                list_builder_append(&helper_fields_builder, unwrapped, LIST_UNSPECIFIED);
                /* Also create method template to preserve default parameter values */
                struct MethodTemplate *template = create_method_template(unwrapped);
                if (template != NULL) {
                    list_builder_append(&method_template_builder, template, LIST_METHOD_TEMPLATE);
                }
            }
            if (unwrapped != NULL && unwrapped->typ == PASCAL_T_PROPERTY_DECL)
            {
                struct ClassProperty *property = convert_property_decl(unwrapped);
                if (property != NULL)
                    list_builder_append(&helper_properties_builder, property, LIST_CLASS_PROPERTY);
            }
            base_node = base_node->next;
        }
        
        /* If we have two identifiers, first is parent helper, second is base type.
         * If we have one identifier, it's the base type (no parent helper). */
        if (second_ident != NULL) {
            record->helper_parent_id = strdup(first_ident->sym->name);
            record->helper_base_type_id = strdup(second_ident->sym->name);
        } else if (first_ident != NULL) {
            record->helper_base_type_id = strdup(first_ident->sym->name);
        }

        record->fields = list_builder_finish(&helper_fields_builder);
        record->properties = list_builder_finish(&helper_properties_builder);
        record->method_templates = list_builder_finish(&method_template_builder);
        return record;
    }

    /* Scan for nested type sections in the record */
    ListBuilder nested_type_builder;
    list_builder_init(&nested_type_builder);

    /* For OBJECT_TYPE, the first child may be the parent type identifier
     * (inserted by the parser for object(BaseType) syntax).
     * Extract it before processing members. */
    char *parent_class_name = NULL;
    ast_t *members_start = record_node->child;
    if (record_node->typ == PASCAL_T_OBJECT_TYPE &&
        members_start != NULL &&
        members_start->typ == PASCAL_T_IDENTIFIER &&
        members_start->sym != NULL &&
        members_start->sym->name != NULL)
    {
        parent_class_name = strdup(members_start->sym->name);
        members_start = members_start->next;
    }

    collect_record_nested_types(members_start, &nested_type_builder);

    if (nested_types_out != NULL)
        *nested_types_out = list_builder_finish(&nested_type_builder);
    else
        destroy_list(nested_type_builder.head);

    ListBuilder fields_builder;
    ListBuilder property_builder;
    ListBuilder method_template_builder;
    list_builder_init(&fields_builder);
    list_builder_init(&property_builder);
    list_builder_init(&method_template_builder);
    convert_record_members(members_start, &fields_builder, &property_builder, &method_template_builder);

    struct RecordType *record = (struct RecordType *)malloc(sizeof(struct RecordType));
    if (record == NULL) {
        destroy_list(fields_builder.head);
        destroy_list(property_builder.head);
        free(parent_class_name);
        return NULL;
    }
    record->fields = list_builder_finish(&fields_builder);
    record->properties = NULL;
    record->parent_class_name = parent_class_name;
    record->methods = NULL;  /* Regular records don't have methods */
    record->method_templates = list_builder_finish(&method_template_builder);
    record->is_class = 0;
    record->is_interface = 0;
    record->is_packed = (record_node->sym != NULL &&
        record_node->sym->name != NULL &&
        (strcasecmp(record_node->sym->name, "packed") == 0 ||
         strcasecmp(record_node->sym->name, "bitpacked") == 0));
    record->is_type_helper = 0;
    record->helper_base_type_id = NULL;
    record->helper_parent_id = NULL;
    record->type_id = NULL;
    record->outer_type_id = NULL;
    record->has_cached_size = 0;
    record->cached_size = 0;
    record->generic_decl = NULL;
    record->generic_args = NULL;
    record->num_generic_args = 0;
    record->is_generic_specialization = 0;
    record->method_clones_emitted = 0;
    record->parent_fields_merged = 0;
    record->source_unit_index = 0;
    record->default_indexed_property = NULL;
    record->default_indexed_element_type = UNKNOWN_TYPE;
    record->record_properties = list_builder_finish(&property_builder);
    record->default_indexed_element_type_id = NULL;
    record->guid_string = NULL;
    record->interface_names = NULL;
    record->num_interfaces = 0;
    return record;
}

struct RecordType *convert_record_type(ast_t *record_node) {
    return convert_record_type_ex(record_node, NULL);
}

char *pop_last_identifier(ListNode_t **ids) {
    if (ids == NULL || *ids == NULL)
        return NULL;

    ListNode_t *prev = NULL;
    ListNode_t *cur = *ids;
    while (cur->next != NULL) {
        prev = cur;
        cur = cur->next;
    }

    char *value = (char *)cur->cur;
    if (prev != NULL)
        prev->next = NULL;
    else
        *ids = NULL;

    free(cur);
    return value;
}

ListNode_t *convert_param(ast_t *param_node) {
    if (param_node == NULL || param_node->typ != PASCAL_T_PARAM)
        return NULL;

    ast_t *modifier_node = param_node->child;
    ast_t *ids_cursor = modifier_node != NULL ? modifier_node->next : NULL;

    int is_var_param = 0;
    int is_const_param = 0;
    if (modifier_node != NULL && modifier_node->sym != NULL && modifier_node->sym->name != NULL) {
        const char *modifier_name = modifier_node->sym->name;
        if (strcasecmp(modifier_name, "var") == 0 || strcasecmp(modifier_name, "out") == 0)
            is_var_param = 1;
        else if (strcasecmp(modifier_name, "const") == 0)
            is_const_param = 1;
        else if (strcasecmp(modifier_name, "constref") == 0)
        {
            /* constref is like var (pass by reference) but also const (not modifiable).
             * For codegen purposes, we treat it as is_var_param=1 to pass by reference.
             * The const aspect is enforced by semantic checking. */
            is_var_param = 1;
            is_const_param = 1;
        }
    }

    ast_t *cursor = ids_cursor;
    ListNode_t *ids = convert_identifier_list(&cursor);
    if (ids == NULL) {
        fprintf(stderr, "ERROR: parameter missing identifier list.\n");
        return NULL;
    }

    ast_t *type_node = cursor;
    char *type_id = NULL;
    int var_type = UNKNOWN_TYPE;
    TypeInfo type_info = {0};
    ast_t *default_value_node = NULL;

    if (type_node == NULL || type_node->typ != PASCAL_T_TYPE_SPEC) {
        if (!(is_var_param || is_const_param)) {
            fprintf(stderr, "ERROR: parameter missing type specification.\n");
            destroy_list(ids);
            return NULL;
        }
        var_type = UNKNOWN_TYPE;
        type_id = NULL;
    }
    else
    {
        /* ARCHITECTURAL FIX: Pass TypeInfo to preserve array information */
        var_type = convert_type_spec(type_node, &type_id, NULL, &type_info);
        /* Check for default value node after type spec.
         * Some parser shapes wrap optional/default nodes so `type_node->next`
         * may be NULL even when a default exists. */
        if (type_node->next != NULL && type_node->next->typ == PASCAL_T_DEFAULT_VALUE) {
            default_value_node = type_node->next;
        } else {
            default_value_node = find_ast_node_type(param_node, PASCAL_T_DEFAULT_VALUE);
        }
        if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
            fprintf(stderr, "[convert_param] type_node=%p type_node->next=%p next_typ=%d\n",
                (void*)type_node, 
                (void*)(type_node ? type_node->next : NULL),
                (type_node && type_node->next) ? type_node->next->typ : -1);
        }
    }

    /* Convert default value if present */
    struct Statement *default_init = NULL;
    if (default_value_node != NULL) {
        /* PASCAL_T_DEFAULT_VALUE's child IS the expression (no "=" token since match returns ast_nil) */
        ast_t *expr_node = default_value_node->child;
        if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
            fprintf(stderr, "[convert_param] default_value_node=%p expr_node=%p\n",
                (void*)default_value_node, (void*)expr_node);
        }
        if (expr_node != NULL) {
            struct Expression *default_expr = convert_expression(expr_node);
            if (default_expr != NULL) {
                /* Wrap expression in a var_assign statement with NULL var for storage */
                default_init = mk_varassign(default_value_node->line, default_value_node->col, 
                                            NULL, default_expr);
                if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
                    fprintf(stderr, "[convert_param] Created default_init=%p\n", (void*)default_init);
                }
            }
        }
    }

    ListBuilder result_builder;
    list_builder_init(&result_builder);
    ListNode_t *id_node = ids;

    while (id_node != NULL) {
        ListNode_t *next_id = id_node->next;
        id_node->next = NULL;
        char *type_id_copy = type_id != NULL ? strdup(type_id) : NULL;
        
        Tree_t *param_decl = NULL;
        struct TypeAlias *inline_alias = NULL;
        if (type_info.is_set)
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
        /* Create TREE_ARR_DECL for inline array parameters */
        if (type_info.is_array)
        {
            int element_type = type_info.element_type;
            char *element_type_id = type_info.element_type_id != NULL ? strdup(type_info.element_type_id) : NULL;
            char *range_str = NULL;
            struct RecordType *inline_record = type_info.record_type;
            if (inline_record != NULL)
                type_info.record_type = NULL;
            if (type_info.array_dimensions != NULL && type_info.array_dimensions->cur != NULL) {
                range_str = strdup((char *)type_info.array_dimensions->cur);
            }
            param_decl = mk_arraydecl(param_node->line, id_node, element_type, element_type_id,
                                      type_info.start, type_info.end, range_str, NULL, inline_record);
            if (param_decl != NULL)
                param_decl->tree_data.arr_decl_data.is_shortstring = type_info.is_shortstring;
            if (param_decl != NULL)
                param_decl->tree_data.arr_decl_data.type_ref =
                    type_ref_from_element_info(&type_info, element_type_id);
            /* Set var parameter flag on array declaration */
            if (is_var_param && param_decl != NULL)
                param_decl->tree_data.arr_decl_data.type = var_type; // Store this for compatibility
            if (kgpc_getenv("KGPC_DEBUG_ARRAY_PARAM") != NULL && param_decl != NULL)
            {
                fprintf(stderr,
                    "[KGPC] array param %s: element_type=%d element_id=%s is_array_of_const=%d\n",
                    id_node != NULL && id_node->cur != NULL ? (const char *)id_node->cur : "<unnamed>",
                    element_type,
                    element_type_id != NULL ? element_type_id : "<null>",
                    type_info.is_array_of_const);
            }
            /* Note: array parameters with default values are rare but could be supported */
        }
        else
        {
            param_decl = mk_vardecl(param_node->line, id_node, var_type, type_id_copy,
                is_var_param, 0, default_init, NULL, inline_alias, NULL);
            if (param_decl != NULL)
                param_decl->tree_data.var_decl_data.is_const_param = is_const_param;
            if (param_decl != NULL && (type_node == NULL || type_node->typ != PASCAL_T_TYPE_SPEC))
                param_decl->tree_data.var_decl_data.is_untyped_param = 1;
            if (param_decl != NULL)
                param_decl->tree_data.var_decl_data.type_ref =
                    type_ref_from_info_or_id(&type_info, type_id_copy);
        }
        
        list_builder_append(&result_builder, param_decl, LIST_TREE);
        id_node = next_id;
    }

    if (type_id != NULL)
        free(type_id);
    
    destroy_type_info_contents(&type_info);

    return list_builder_finish(&result_builder);
}

ListNode_t *convert_param_list(ast_t **cursor) {
    ListNode_t *params = NULL;
    ast_t *cur = *cursor;
    ast_t *slow = cur;
    ast_t *fast = cur;
    int guard = 0;
    const int guard_limit = 100000;

    while (cur != NULL && cur->typ == PASCAL_T_PARAM) {
        guard++;
        if (guard > guard_limit) {
            fprintf(stderr, "ERROR: convert_param_list exceeded guard limit (%d); possible cycle in param list (node=%p typ=%d).\n",
                guard_limit, (void*)cur, cur->typ);
            break;
        }
        if (fast != NULL && fast->next != NULL) {
            fast = fast->next->next;
            slow = slow ? slow->next : NULL;
            if (fast != NULL && slow == fast) {
                fprintf(stderr, "ERROR: Cycle detected in param list during conversion (node=%p typ=%d).\n",
                    (void*)cur, cur->typ);
                break;
            }
        }
        ListNode_t *param_nodes = convert_param(cur);
        extend_list(&params, param_nodes);
        cur = cur->next;
    }

    *cursor = cur;
    return params;
}

ListNode_t *from_cparser_convert_params_ast(ast_t *params_ast)
{
    if (params_ast == NULL)
        return NULL;

    ast_t *cursor = params_ast;
    if (cursor->typ == PASCAL_T_PARAM_LIST)
        cursor = cursor->child;

    return convert_param_list(&cursor);
}

KgpcType *from_cparser_method_template_to_proctype(struct MethodTemplate *method_template,
    struct RecordType *record, struct SymTab *symtab)
{
    if (method_template == NULL)
        return NULL;

    ListNode_t *params = NULL;
    ListBuilder params_builder;
    list_builder_init(&params_builder);

    /* Add implicit Self parameter for instance methods and non-static class methods.
     * For instance methods, Self = instance pointer.
     * For class methods (non-static), Self = VMT pointer (class reference). */
    if (!method_template->is_static) {
        ListNode_t *self_ids = CreateListNode(strdup("Self"), LIST_STRING);
        char *self_type_id = NULL;
        int self_type_tag = UNKNOWN_TYPE;
        struct TypeAlias *self_type_alias = NULL;
        int self_is_var = 1;
        if (record != NULL) {
            if (record->is_type_helper && record->helper_base_type_id != NULL) {
                self_type_id = strdup(record->helper_base_type_id);
                self_type_tag = map_type_name(record->helper_base_type_id, NULL);
                self_is_var = helper_self_param_is_var(record->helper_base_type_id, symtab);
                self_type_alias = helper_self_real_alias(record->helper_base_type_id);
            } else if (record->is_class || record->is_interface) {
                self_is_var = 0;
            } else if (record->type_id != NULL) {
                self_type_id = strdup(record->type_id);
            }
        }
        Tree_t *self_param = mk_vardecl(0, self_ids, self_type_tag, self_type_id,
            self_is_var, 0, NULL, NULL, self_type_alias, NULL);
        if (self_param != NULL)
            self_param->tree_data.var_decl_data.type_ref = type_ref_from_single_name(self_type_id);
        if (self_param != NULL)
            list_builder_append(&params_builder, self_param, LIST_TREE);
    }

    if (method_template->params_ast != NULL) {
        ListNode_t *extra_params = from_cparser_convert_params_ast(method_template->params_ast);
        if (extra_params != NULL)
            list_builder_extend(&params_builder, extra_params);
    }

    params = list_builder_finish(&params_builder);

    const char *owner_full = (record != NULL) ? record->type_id : NULL;
    char *owner_outer = NULL;
    if (owner_full != NULL) {
        const char *dot = strrchr(owner_full, '.');
        if (dot != NULL && dot[1] != '\0' && dot != owner_full)
            owner_outer = strndup(owner_full, (size_t)(dot - owner_full));
    }
    qualify_param_decl_types(params, owner_full, owner_outer, symtab);
    if (owner_outer != NULL)
        free(owner_outer);

    KgpcType *return_type = NULL;
    char *return_type_id = NULL;
    if (method_template->has_return_type && method_template->return_type_ast != NULL) {
        ast_t *ret_node = method_template->return_type_ast;
        if (ret_node->typ == PASCAL_T_RETURN_TYPE && ret_node->child != NULL)
            ret_node = ret_node->child;
        return_type = convert_type_spec_to_kgpctype(ret_node, symtab);
        if (return_type == NULL && ret_node != NULL && ret_node->typ == PASCAL_T_IDENTIFIER) {
            char *ret_name = dup_symbol(ret_node);
            if (ret_name != NULL) {
                int ret_tag = map_type_name(ret_name, NULL);
                ret_tag = apply_shortstring_mode(ret_tag, ret_name);
                if (ret_tag != UNKNOWN_TYPE)
                    return_type = create_primitive_type(ret_tag);
                return_type_id = strdup(ret_name);
                free(ret_name);
            }
        } else if (ret_node != NULL && ret_node->typ == PASCAL_T_IDENTIFIER) {
            return_type_id = dup_symbol(ret_node);
        }
    }

    KgpcType *proc_type = create_procedure_type(params, return_type);
    if (proc_type != NULL) {
        proc_type->info.proc_info.owns_params = 1;
        DestroyList(params);
        if (return_type_id != NULL)
            proc_type->info.proc_info.return_type_id = return_type_id;
    } else {
        destroy_list(params);
        if (return_type_id != NULL)
            free(return_type_id);
    }

    return proc_type;
}

bool is_var_hint_clause(ast_t *node) {
    if (node == NULL || node->typ != PASCAL_T_NONE)
        return false;
    ast_t *child = node->child;
    if (child == NULL || child->typ != PASCAL_T_IDENTIFIER || child->sym == NULL)
        return false;
    const char *name = child->sym->name;
    if (name == NULL)
        return false;
    return strcasecmp(name, "deprecated") == 0
        || strcasecmp(name, "platform") == 0
        || strcasecmp(name, "library") == 0;
}

ast_t *absolute_clause_target(ast_t *node) {
    if (node == NULL || node->typ != PASCAL_T_ABSOLUTE_CLAUSE)
        return NULL;
    /* The PASCAL_T_ABSOLUTE_CLAUSE node contains the target identifier as its child
     * (the "absolute" keyword itself is consumed by keyword_ci and not in the AST) */
    ast_t *child = node->child;
    if (child == NULL || child->typ != PASCAL_T_IDENTIFIER)
        return NULL;
    return child;
}

/* Check if a node should be skipped as an initializer.
 * This handles:
 * - PASCAL_T_ABSOLUTE_CLAUSE: absolute variable aliasing (e.g., "X: Integer absolute Y")
 * - PASCAL_T_IDENTIFIER: trailing identifiers that aren't initializers (legacy case)
 */
int is_node_to_skip_as_initializer(ast_t *node) {
    if (node == NULL)
        return 0;
    return (node->typ == PASCAL_T_IDENTIFIER || node->typ == PASCAL_T_ABSOLUTE_CLAUSE);
}


