#include "../from_cparser_internal.h"

char *dup_symbol(ast_t *node) {
    while (node != NULL) {
        if (node->sym != NULL && node->sym->name != NULL)
            return strdup(node->sym->name);
        node = node->child;
    }
    return NULL;
}

/* list_builder_* functions are now in List.h - no local definition needed */

char *dup_first_identifier_in_node(ast_t *node)
{
    if (node == NULL)
        return NULL;
    node = unwrap_pascal_node(node);
    if (node == NULL)
        return NULL;
    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
        fprintf(stderr, "[KGPC] inspect node typ=%d (%s) sym=%s\n",
            node->typ, pascal_tag_to_string(node->typ),
            (node->sym != NULL && node->sym->name != NULL) ? node->sym->name : "<null>");
    if ((node->typ == PASCAL_T_IDENTIFIER ||
         node->typ == PASCAL_T_TYPE_ARG) &&
        node->sym != NULL && node->sym->name != NULL)
        return dup_symbol(node);
    ast_t *child = node->child;
    while (child != NULL)
    {
        char *dup = dup_first_identifier_in_node(child);
        if (dup != NULL)
            return dup;
        child = child->next;
    }
    return NULL;
}

ListNode_t *collect_constructed_type_args(ast_t *args_node) {
    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
        fprintf(stderr, "[KGPC] collect_constructed_type_args start node=%p typ=%d (%s)\n",
            (void *)args_node,
            args_node != NULL ? args_node->typ : -1,
            args_node != NULL ? pascal_tag_to_string(args_node->typ) : "<null>");
    if (args_node == NULL)
        return NULL;

    ListBuilder builder;
    list_builder_init(&builder);

    ast_t *cursor = args_node;
    if (cursor->typ == PASCAL_T_TYPE_ARG_LIST)
        cursor = cursor->child;

    while (cursor != NULL) {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node == NULL)
            node = cursor;

        if (node != NULL &&
            (node->typ == PASCAL_T_TYPE_ARG ||
             node->typ == PASCAL_T_IDENTIFIER ||
             node->typ == PASCAL_T_QUALIFIED_IDENTIFIER))
        {
            char *dup = dup_first_identifier_in_node(node);
            if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
                fprintf(stderr, "[KGPC] collect args extracted=%s\n", dup != NULL ? dup : "<null>");
            if (dup != NULL)
            {
                ListNode_t *append_node = list_builder_append(&builder, dup, LIST_STRING);
                if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
                    fprintf(stderr, "[KGPC] appended type_arg node=%p type=%d\n",
                        (void *)append_node, append_node != NULL ? append_node->type : -1);
            }
        }

        cursor = cursor->next;
    }
    return list_builder_finish(&builder);
}

int extract_constructed_type_info(ast_t *spec_node, char **base_name_out, ListNode_t **type_args_out) {
    if (base_name_out != NULL)
        *base_name_out = NULL;
    if (type_args_out != NULL)
        *type_args_out = NULL;

    if (spec_node == NULL)
        return 0;

    ast_t *node = spec_node;
    if (node->typ == PASCAL_T_TYPE_SPEC && node->child != NULL)
        node = node->child;
    node = unwrap_pascal_node(node);
    if (node == NULL || node->typ != PASCAL_T_CONSTRUCTED_TYPE)
        return 0;

    ast_t *name_node = node->child;
    while (name_node != NULL && name_node->typ == PASCAL_T_NONE)
        name_node = name_node->child;
    if (name_node == NULL)
        return 0;
    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
    {
        fprintf(stderr, "[KGPC] constructed type base node typ=%d (%s) sym=%s next_typ=%d (%s)\n",
            name_node->typ,
            pascal_tag_to_string(name_node->typ),
            (name_node->sym != NULL && name_node->sym->name != NULL) ? name_node->sym->name : "<null>",
            name_node->next != NULL ? name_node->next->typ : -1,
            name_node->next != NULL ? pascal_tag_to_string(name_node->next->typ) : "<null>");
    }

    char *base_name = dup_symbol(name_node);
    if (base_name == NULL)
        return 0;

    ListNode_t *type_args = collect_constructed_type_args(name_node->next);
    if (type_args == NULL) {
        free(base_name);
        return 0;
    }

    if (base_name_out != NULL)
        *base_name_out = base_name;
    else
        free(base_name);

    if (type_args_out != NULL)
        *type_args_out = type_args;
    else
        destroy_list(type_args);

    return 1;
}

static char *mangle_specialized_type_name(const char *base_name, char **type_ids, int num_types) {
    if (base_name == NULL)
        return NULL;
    size_t length = strlen(base_name) + 1;
    for (int i = 0; i < num_types; ++i) {
        if (type_ids[i] != NULL)
            length += 1 + strlen(type_ids[i]);
    }
    char *result = (char *)malloc(length);
    if (result == NULL)
        return NULL;
    strcpy(result, base_name);
    for (int i = 0; i < num_types; ++i) {
        strcat(result, "$");
        if (type_ids[i] != NULL)
            strcat(result, type_ids[i]);
    }
    return result;
}

char *mangle_specialized_name_from_list(const char *base_name, ListNode_t *type_args) {
    if (base_name == NULL)
        return NULL;
    int count = ListLength(type_args);
    if (count <= 0)
        return strdup(base_name);
    char **arg_ids = (char **)calloc((size_t)count, sizeof(char *));
    if (arg_ids == NULL)
        return NULL;
    int idx = 0;
    for (ListNode_t *cur = type_args; cur != NULL && idx < count; cur = cur->next, ++idx) {
        arg_ids[idx] = (char *)(cur->cur);
    }
    char *result = mangle_specialized_type_name(base_name, arg_ids, count);
    free(arg_ids);
    return result;
}

static void substitute_identifier(char **type_id, GenericTypeDecl *generic_decl, char **arg_types) {
    if (type_id == NULL || *type_id == NULL || generic_decl == NULL || arg_types == NULL)
        return;

    for (int i = 0; i < generic_decl->num_type_params; ++i) {
        if (generic_decl->type_parameters[i] != NULL &&
            strcasecmp(*type_id, generic_decl->type_parameters[i]) == 0) {
            char *replacement = NULL;
            if (arg_types[i] != NULL)
                replacement = strdup(arg_types[i]);
            free(*type_id);
            *type_id = replacement;
            return;
        }
    }
}

static void substitute_type_ref(TypeRef **type_ref, GenericTypeDecl *generic_decl, char **arg_types)
{
    if (type_ref == NULL || *type_ref == NULL || generic_decl == NULL || arg_types == NULL)
        return;

    TypeRef *ref = *type_ref;
    if (ref->generic_args != NULL && ref->num_generic_args > 0)
    {
        for (int i = 0; i < ref->num_generic_args; ++i)
            substitute_type_ref(&ref->generic_args[i], generic_decl, arg_types);
    }

    if (ref->name == NULL || ref->name->count != 1)
        return;

    const char *base_name = type_ref_base_name(ref);
    if (base_name == NULL)
        return;

    if (ref->num_generic_args > 0)
        return;

    for (int i = 0; i < generic_decl->num_type_params; ++i)
    {
        if (generic_decl->type_parameters[i] != NULL &&
            pascal_identifier_equals(base_name, generic_decl->type_parameters[i]))
        {
            TypeRef *replacement = NULL;
            if (arg_types[i] != NULL)
                replacement = type_ref_from_single_name(arg_types[i]);
            type_ref_free(ref);
            *type_ref = replacement;
            return;
        }
    }
}

static void substitute_record_type_parameters(struct RecordType *record, GenericTypeDecl *generic_decl, char **arg_types);

static void substitute_record_field(struct RecordField *field, GenericTypeDecl *generic_decl, char **arg_types) {
    if (field == NULL)
        return;
    
    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
    if (debug_env != NULL && field->name != NULL)
    {
        fprintf(stderr, "[KGPC] substitute_record_field BEFORE: name=%s is_array=%d type_id=%s array_element_type_id=%s\n",
            field->name, field->is_array,
            field->type_id ? field->type_id : "<null>",
            field->array_element_type_id ? field->array_element_type_id : "<null>");
    }
    
    substitute_identifier(&field->type_id, generic_decl, arg_types);
    substitute_type_ref(&field->type_ref, generic_decl, arg_types);
    if (field->array_element_type_id != NULL)
        substitute_identifier(&field->array_element_type_id, generic_decl, arg_types);
    substitute_type_ref(&field->array_element_type_ref, generic_decl, arg_types);
    if (field->pointer_type_id != NULL)
        substitute_identifier(&field->pointer_type_id, generic_decl, arg_types);
    substitute_type_ref(&field->pointer_type_ref, generic_decl, arg_types);
    if (field->nested_record != NULL)
        substitute_record_type_parameters(field->nested_record, generic_decl, arg_types);
    
    if (debug_env != NULL && field->name != NULL)
    {
        fprintf(stderr, "[KGPC] substitute_record_field AFTER: name=%s is_array=%d type_id=%s array_element_type_id=%s\n",
            field->name, field->is_array,
            field->type_id ? field->type_id : "<null>",
            field->array_element_type_id ? field->array_element_type_id : "<null>");
    }
}

static void substitute_record_type_parameters(struct RecordType *record, GenericTypeDecl *generic_decl, char **arg_types) {
    if (record == NULL || generic_decl == NULL || arg_types == NULL)
        return;

    ListNode_t *field_node = record->fields;
    while (field_node != NULL) {
        if (field_node->type == LIST_RECORD_FIELD)
            substitute_record_field((struct RecordField *)field_node->cur, generic_decl, arg_types);
        field_node = field_node->next;
    }

    ListNode_t *prop_node = record->properties;
    while (prop_node != NULL) {
        if (prop_node->type == LIST_CLASS_PROPERTY) {
            struct ClassProperty *property = (struct ClassProperty *)prop_node->cur;
            substitute_identifier(&property->type_id, generic_decl, arg_types);
            substitute_type_ref(&property->type_ref, generic_decl, arg_types);
        }
        prop_node = prop_node->next;
    }
}

struct RecordType *instantiate_generic_record(const char *base_name, ListNode_t *type_args, char **specialized_name_out) {
    if (specialized_name_out != NULL)
        *specialized_name_out = NULL;
    if (base_name == NULL)
        return NULL;

    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
    GenericTypeDecl *generic = generic_registry_find_decl(base_name);
    if (generic == NULL || generic->record_template == NULL) {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] instantiate_generic_record missing decl for %s\n", base_name);
        return NULL;
    }

    int arg_count = ListLength(type_args);
    if (arg_count != generic->num_type_params || arg_count <= 0) {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] instantiate_generic_record wrong arg count for %s: got %d expected %d\n",
                    base_name, arg_count, generic->num_type_params);
        destroy_list(type_args);
        return NULL;
    }

    char **arg_types = (char **)calloc((size_t)arg_count, sizeof(char *));
    if (arg_types == NULL) {
        return NULL;
    }

    int idx = 0;
    ListNode_t *cur = type_args;
    while (cur != NULL && idx < arg_count) {
        if (cur->type == LIST_STRING && cur->cur != NULL) {
            arg_types[idx] = strdup((char *)cur->cur);
            if (arg_types[idx] == NULL)
                break;
            idx++;
        }
        cur = cur->next;
    }

    if (idx != arg_count) {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] instantiate_generic_record failed to copy args for %s\n", base_name);
        for (int i = 0; i < arg_count; ++i)
            free(arg_types[i]);
        free(arg_types);
        return NULL;
    }

    char *specialized_name = mangle_specialized_type_name(base_name, arg_types, arg_count);
    if (specialized_name == NULL) {
        for (int i = 0; i < arg_count; ++i)
            free(arg_types[i]);
        free(arg_types);
        return NULL;
    }

    struct RecordType *record = clone_record_type(generic->record_template);
    if (record == NULL) {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] instantiate_generic_record failed to clone template for %s\n", base_name);
        free(specialized_name);
        for (int i = 0; i < arg_count; ++i)
            free(arg_types[i]);
        free(arg_types);
        return NULL;
    }

    if (record->type_id != NULL)
        free(record->type_id);
    record->type_id = strdup(specialized_name);
    record->generic_decl = generic;
    record->num_generic_args = arg_count;
    record->generic_args = arg_types;
    arg_types = NULL;

    substitute_record_type_parameters(record, generic, record->generic_args);

    /* Rewrite references to nested types within the generic class.
     * E.g. field FOnCompare with type_id="TCompareFunc" needs to become
     * "TFPGList$TMyRecord.TCompareFunc" for proper resolution. */
    if (generic->nested_type_decls != NULL) {
        /* Collect the short names of nested types (without the prefix) */
        size_t gen_prefix_len = strlen(base_name);
        ListNode_t *field_node = record->fields;
        while (field_node != NULL) {
            if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL) {
                struct RecordField *field = (struct RecordField *)field_node->cur;
                if (field->type_id != NULL) {
                    /* Check if this type_id matches a nested type name */
                    ListNode_t *nt = generic->nested_type_decls;
                    while (nt != NULL) {
                        if (nt->type == LIST_TREE && nt->cur != NULL) {
                            Tree_t *nt_tree = (Tree_t *)nt->cur;
                            if (nt_tree->type == TREE_TYPE_DECL && nt_tree->tree_data.type_decl_data.id != NULL) {
                                const char *nt_id = nt_tree->tree_data.type_decl_data.id;
                                /* nested type id is like "TFPGList.TCompareFunc" */
                                if (strncmp(nt_id, base_name, gen_prefix_len) == 0 &&
                                    nt_id[gen_prefix_len] == '.') {
                                    const char *short_name = nt_id + gen_prefix_len + 1;
                                    if (strcasecmp(field->type_id, short_name) == 0) {
                                        /* Rewrite to specialized: "TFPGList$TMyRecord.TCompareFunc" */
                                        size_t new_len = strlen(specialized_name) + 1 + strlen(short_name) + 1;
                                        char *new_id = (char *)malloc(new_len);
                                        if (new_id != NULL) {
                                            snprintf(new_id, new_len, "%s.%s", specialized_name, short_name);
                                            free(field->type_id);
                                            field->type_id = new_id;
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                        nt = nt->next;
                    }
                }
            }
            field_node = field_node->next;
        }
    }

    generic_registry_add_specialization(base_name, record->generic_args, arg_count);

    if (specialized_name_out != NULL)
        *specialized_name_out = specialized_name;
    else
        free(specialized_name);

    if (debug_env != NULL && record->type_id != NULL)
        fprintf(stderr, "[KGPC] instantiated generic record %s\n", record->type_id);
    return record;
}

static int build_generic_arg_array(ListNode_t *type_args, char ***arg_types_out, int *arg_count_out)
{
    if (arg_types_out == NULL || arg_count_out == NULL)
        return 0;
    *arg_types_out = NULL;
    *arg_count_out = 0;

    int arg_count = 0;
    for (ListNode_t *cur = type_args; cur != NULL; cur = cur->next)
    {
        if (cur->type == LIST_STRING && cur->cur != NULL)
            arg_count++;
    }
    if (arg_count == 0)
        return 0;

    char **arg_types = (char **)calloc((size_t)arg_count, sizeof(char *));
    if (arg_types == NULL)
        return 0;

    int idx = 0;
    ListNode_t *cur = type_args;
    while (cur != NULL && idx < arg_count) {
        if (cur->type == LIST_STRING && cur->cur != NULL) {
            arg_types[idx] = strdup((char *)cur->cur);
            if (arg_types[idx] == NULL)
                break;
            idx++;
        }
        cur = cur->next;
    }

    if (idx != arg_count) {
        for (int i = 0; i < arg_count; ++i)
            free(arg_types[i]);
        free(arg_types);
        return 0;
    }

    *arg_types_out = arg_types;
    *arg_count_out = arg_count;
    return 1;
}

static int type_info_has_resolution(const TypeInfo *info)
{
    if (info == NULL)
        return 0;
    return (info->is_array || info->is_set || info->is_record || info->is_file ||
            info->is_enum || info->is_range || info->is_pointer ||
            info->is_class_reference || info->is_array_of_const);
}

int resolve_generic_alias_type(const char *base_name, ListNode_t *type_args,
    char **type_id_out, TypeInfo *type_info, int *result_out)
{
    if (base_name == NULL || type_info == NULL)
        return 0;
    if (result_out != NULL)
        *result_out = UNKNOWN_TYPE;

    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
    GenericTypeDecl *generic = generic_registry_find_decl(base_name);
    Tree_t *generic_decl_tree = generic != NULL ? generic->original_decl : NULL;
    ast_t *generic_ast = generic_decl_tree != NULL ?
        generic_decl_tree->tree_data.type_decl_data.info.generic.original_ast : NULL;
    if (generic == NULL || generic->record_template != NULL || generic_ast == NULL)
    {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] resolve_generic_alias_type skip base=%s generic=%p record_template=%p original_ast=%p\n",
                base_name, (void *)generic,
                generic != NULL ? (void *)generic->record_template : NULL,
                (void *)generic_ast);
        return 0;
    }

    char **arg_types = NULL;
    int arg_count = 0;
    if (!build_generic_arg_array(type_args, &arg_types, &arg_count))
        return 0;

    if (arg_count != generic->num_type_params) {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] resolve_generic_alias_type arg mismatch base=%s got=%d expected=%d\n",
                base_name, arg_count, generic->num_type_params);
        for (int i = 0; i < arg_count; ++i)
            free(arg_types[i]);
        free(arg_types);
        return 0;
    }

    ast_t *ast_copy = copy_ast(generic_ast);
    if (ast_copy == NULL) {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] resolve_generic_alias_type copy failed base=%s\n", base_name);
        for (int i = 0; i < arg_count; ++i)
            free(arg_types[i]);
        free(arg_types);
        return 0;
    }

    substitute_generic_identifiers(ast_copy, generic->type_parameters, arg_types, arg_count);
    int result = convert_type_spec(ast_copy, type_id_out, NULL, type_info);
    if (debug_env != NULL)
        fprintf(stderr, "[KGPC] resolve_generic_alias_type resolved base=%s result=%d is_array=%d\n",
            base_name, result, type_info->is_array);
    free_ast(ast_copy);

    for (int i = 0; i < arg_count; ++i)
        free(arg_types[i]);
    free(arg_types);

    if (result_out != NULL)
        *result_out = result;

    if (result != UNKNOWN_TYPE || type_info_has_resolution(type_info) ||
        (type_id_out != NULL && *type_id_out != NULL))
        return 1;

    return 0;
}

void record_generic_method_impl(const char *class_name, const char *method_name, ast_t *method_ast)
{
    if (class_name == NULL || method_name == NULL || method_ast == NULL)
        return;

    GenericTypeDecl *generic = generic_registry_find_decl(class_name);
    if (generic == NULL || generic->record_template == NULL) {
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL && class_name != NULL)
            fprintf(stderr, "[KGPC] record_generic_method_impl: no generic decl for %s\n", class_name);
        return;
    }

    ListNode_t *cur = generic->record_template->method_templates;
    while (cur != NULL)
    {
        if (cur->type == LIST_METHOD_TEMPLATE)
        {
            struct MethodTemplate *template = (struct MethodTemplate *)cur->cur;
            if (template != NULL && template->method_impl_ast == NULL &&
                strcasecmp(template->name, method_name) == 0)
            {
                template->method_impl_ast = copy_ast_detached(method_ast);
                if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL)
                    fprintf(stderr, "[KGPC] recorded method implementation for %s.%s\n", class_name, method_name);
                break;
            }
        }
        cur = cur->next;
    }
}

static void substitute_generic_identifier_nodes(ast_t *node, struct RecordType *record)
{
    if (node == NULL || record == NULL || record->generic_decl == NULL ||
        record->generic_args == NULL || record->num_generic_args <= 0)
        return;

    const char *generic_name = record->generic_decl->name;
    size_t gen_prefix_len = generic_name != NULL ? strlen(generic_name) : 0;

    ast_t *cursor = node;
    while (cursor != NULL)
    {
        if (cursor->sym != NULL && cursor->sym->name != NULL)
        {
            /* Substitute generic type parameters (e.g. T -> TMyRecord) */
            for (int i = 0; i < record->generic_decl->num_type_params && i < record->num_generic_args; ++i)
            {
                const char *param_name = record->generic_decl->type_parameters[i];
                const char *arg_name = record->generic_args[i];
                if (param_name != NULL && arg_name != NULL &&
                    strcasecmp(cursor->sym->name, param_name) == 0)
                {
                    free(cursor->sym->name);
                    cursor->sym->name = strdup(arg_name);
                    break;
                }
            }
            /* Substitute the generic class name itself with the specialized name
             * (e.g. TFPGList -> TFPGList$TMyRecord in method parameter types) */
            if (generic_name != NULL && record->type_id != NULL &&
                strcasecmp(cursor->sym->name, generic_name) == 0)
            {
                free(cursor->sym->name);
                cursor->sym->name = strdup(record->type_id);
            }
            /* Substitute nested type short names with their specialized full names.
             * E.g. TFPGListEnumeratorSpec -> TFPGList$TMyRecord.TFPGListEnumeratorSpec
             * inside method bodies of the specialized class. */
            if (record->generic_decl->nested_type_decls != NULL && record->type_id != NULL)
            {
                ListNode_t *nt = record->generic_decl->nested_type_decls;
                while (nt != NULL)
                {
                    if (nt->type == LIST_TREE && nt->cur != NULL)
                    {
                        Tree_t *nt_tree = (Tree_t *)nt->cur;
                        if (nt_tree->type == TREE_TYPE_DECL && nt_tree->tree_data.type_decl_data.id != NULL)
                        {
                            const char *nt_id = nt_tree->tree_data.type_decl_data.id;
                            /* nested type id is like "TFPGList.TFPGListEnumeratorSpec" */
                            if (gen_prefix_len > 0 &&
                                strncmp(nt_id, generic_name, gen_prefix_len) == 0 &&
                                nt_id[gen_prefix_len] == '.')
                            {
                                const char *short_name = nt_id + gen_prefix_len + 1;
                                if (strcasecmp(cursor->sym->name, short_name) == 0)
                                {
                                    /* Rewrite to "TFPGList$TMyRecord.TFPGListEnumeratorSpec" */
                                    size_t new_len = strlen(record->type_id) + 1 + strlen(short_name) + 1;
                                    char *new_name = (char *)malloc(new_len);
                                    if (new_name != NULL)
                                    {
                                        snprintf(new_name, new_len, "%s.%s", record->type_id, short_name);
                                        free(cursor->sym->name);
                                        cursor->sym->name = new_name;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                    nt = nt->next;
                }
            }
        }
        if (cursor->child != NULL)
            substitute_generic_identifier_nodes(cursor->child, record);
        cursor = cursor->next;
    }
}

static void rewrite_method_impl_ast(ast_t *method_ast, struct RecordType *record)
{
    if (method_ast == NULL || record == NULL)
        return;

    ast_t *qualified = unwrap_pascal_node(method_ast->child);
    if (qualified != NULL && qualified->typ == PASCAL_T_QUALIFIED_IDENTIFIER)
    {
        ast_t *class_node = qualified->child;
        while (class_node != NULL && class_node->typ != PASCAL_T_IDENTIFIER)
            class_node = class_node->next;
        if (class_node != NULL && class_node->sym != NULL && record->type_id != NULL)
        {
            free(class_node->sym->name);
            class_node->sym->name = strdup(record->type_id);
        }
    }

    substitute_generic_identifier_nodes(method_ast, record);
}



static Tree_t *instantiate_method_template(struct MethodTemplate *method_template, struct RecordType *record)
{
    if (method_template == NULL || method_template->method_impl_ast == NULL || record == NULL)
        return NULL;

    /* Register method bindings for the specialized class name so that
     * convert_method_impl can look up is_static / is_class_method correctly.
     * The original bindings are registered under the generic class name,
     * but the cloned method will have the specialized class name. */
    if (record->type_id != NULL && method_template->name != NULL)
    {
        int param_count = from_cparser_count_params_ast(method_template->params_ast);
        char *param_sig = param_type_signature_from_params_ast(method_template->params_ast);
        register_class_method_ex(record->type_id, method_template->name,
            method_template->is_virtual, method_template->is_override,
            method_template->is_static, method_template->is_class_method,
            param_count, param_sig);
    }

    ast_t *method_copy = copy_ast(method_template->method_impl_ast);
    if (method_copy == NULL)
        return NULL;

    rewrite_method_impl_ast(method_copy, record);

    /* Restore the source offset from when the template was parsed,
     * so source_index values map to the correct source buffer. */
    int saved_offset = g_source_offset;
    g_source_offset = method_template->source_offset;

    /* Set the current generic context so that generic_registry_is_type_param()
     * only checks the enclosing generic's type parameters, not all generics. */
    GenericTypeDecl *saved_context = generic_registry_current_context();
    generic_registry_set_context(record->generic_decl);

    /* Let type_name_is_class_like resolve via the already-converted record
     * instead of the raw parser AST (which may have been freed). */
    struct RecordType *saved_record = g_instantiate_record;
    g_instantiate_record = record;

    Tree_t *method_tree = convert_method_impl(method_copy);

    g_instantiate_record = saved_record;
    generic_registry_set_context(saved_context);

    g_source_offset = saved_offset;

    free_ast(method_copy);
    return method_tree;
}

void append_subprogram_node(ListNode_t **dest, Tree_t *tree)
{
    if (dest == NULL || tree == NULL)
        return;

    ListNode_t *node = CreateListNode(tree, LIST_TREE);
    if (node == NULL)
        return;

    ListNode_t **tail = dest;
    while (*tail != NULL)
        tail = &(*tail)->next;
    *tail = node;
}

static int subprogram_list_has_id(const ListNode_t *subprograms, const char *id)
{
    if (subprograms == NULL || id == NULL)
        return 0;

    const ListNode_t *cur = subprograms;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            const Tree_t *tree = (const Tree_t *)cur->cur;
            if (tree->type == TREE_SUBPROGRAM &&
                tree->tree_data.subprogram_data.id != NULL &&
                strcasecmp(tree->tree_data.subprogram_data.id, id) == 0)
            {
                return 1;
            }
        }
        cur = cur->next;
    }
    return 0;
}

static int parse_generic_mangled_id(const char *mangled, char **base_out,
    char ***args_out, int *argc_out)
{
    if (mangled == NULL || base_out == NULL || args_out == NULL || argc_out == NULL)
        return 0;

    const char *dollar = strchr(mangled, '$');
    if (dollar == NULL || dollar == mangled || dollar[1] == '\0')
        return 0;

    size_t base_len = (size_t)(dollar - mangled);
    char *base = (char *)malloc(base_len + 1);
    if (base == NULL)
        return 0;
    memcpy(base, mangled, base_len);
    base[base_len] = '\0';

    int count = 0;
    for (const char *p = dollar; p != NULL; p = strchr(p + 1, '$'))
        count++;
    if (count <= 0)
    {
        free(base);
        return 0;
    }

    char **args = (char **)calloc((size_t)count, sizeof(char *));
    if (args == NULL)
    {
        free(base);
        return 0;
    }

    int idx = 0;
    const char *seg = dollar + 1;
    while (seg != NULL && idx < count)
    {
        const char *next = strchr(seg, '$');
        size_t seg_len = next ? (size_t)(next - seg) : strlen(seg);
        args[idx] = (char *)malloc(seg_len + 1);
        if (args[idx] == NULL)
            break;
        memcpy(args[idx], seg, seg_len);
        args[idx][seg_len] = '\0';
        idx++;
        if (next == NULL)
            break;
        seg = next + 1;
    }

    if (idx != count)
    {
        for (int i = 0; i < count; ++i)
            free(args[i]);
        free(args);
        free(base);
        return 0;
    }

    *base_out = base;
    *args_out = args;
    *argc_out = count;
    return 1;
}

void substitute_generic_identifiers(ast_t *node, char **params, char **args, int count)
{
    if (node == NULL || params == NULL || args == NULL || count <= 0)
        return;

    ast_t *cursor = node;
    while (cursor != NULL)
    {
        if (cursor->sym != NULL && cursor->sym->name != NULL)
        {
            for (int i = 0; i < count; ++i)
            {
                if (params[i] != NULL && args[i] != NULL &&
                    strcasecmp(cursor->sym->name, params[i]) == 0)
                {
                    free(cursor->sym->name);
                    cursor->sym->name = strdup(args[i]);
                    break;
                }
            }
        }
        if (cursor->child != NULL)
            substitute_generic_identifiers(cursor->child, params, args, count);
        cursor = cursor->next;
    }
}

/* Collect local type names from TYPE_DECL nodes within a subprogram AST.
 * Only walks children of the root node (not siblings) to avoid picking up
 * global type declarations from the program tree.
 * Returns allocated arrays via out params; caller must free. */
static void collect_local_type_names(ast_t *node, char ***names_out, int *count_out)
{
    *names_out = NULL;
    *count_out = 0;
    if (node == NULL || node->child == NULL)
        return;

    int capacity = 4;
    int count = 0;
    char **names = (char **)malloc(capacity * sizeof(char *));
    if (names == NULL)
        return;

    /* Walk only children of the root (subprogram body), not siblings */
    ast_t *stack[256];
    int sp = 0;
    /* Push only children of root, not root->next */
    for (ast_t *c = node->child; c != NULL; c = c->next)
    {
        if (sp < 256)
            stack[sp++] = c;
    }

    while (sp > 0)
    {
        ast_t *cur = stack[--sp];
        if (cur->typ == PASCAL_T_TYPE_DECL)
        {
            ast_t *id_node = cur->child;
            if (id_node != NULL && id_node->typ == PASCAL_T_IDENTIFIER &&
                id_node->sym != NULL && id_node->sym->name != NULL)
            {
                if (count >= capacity)
                {
                    capacity *= 2;
                    char **tmp = (char **)realloc(names, capacity * sizeof(char *));
                    if (tmp == NULL) break;
                    names = tmp;
                }
                names[count++] = strdup(id_node->sym->name);
            }
            /* Don't recurse into type decl children */
            continue;
        }
        if (cur->child != NULL && sp < 256)
            stack[sp++] = cur->child;
        /* Follow next for non-root nodes */
        if (cur->next != NULL && sp < 256)
            stack[sp++] = cur->next;
    }

    *names_out = names;
    *count_out = count;
}

static void rewrite_generic_subprogram_ast(ast_t *subprogram_ast, const char *specialized_name,
    char **params, char **args, int count)
{
    if (subprogram_ast == NULL || specialized_name == NULL)
        return;

    ast_t *cursor = subprogram_ast->child;
    while (cursor != NULL && cursor->typ != PASCAL_T_IDENTIFIER)
        cursor = cursor->next;
    if (cursor != NULL && cursor->sym != NULL)
    {
        free(cursor->sym->name);
        cursor->sym->name = strdup(specialized_name);
    }

    substitute_generic_identifiers(subprogram_ast, params, args, count);

    /* Strip the generic type parameter list node (PASCAL_T_TYPE_PARAM_LIST)
     * from the rewritten AST.  The specialization is a concrete subprogram;
     * leaving the node would cause convert_procedure/convert_function to
     * re-detect it as a generic template and create a spurious
     * generic_template_ast (a use-after-free hazard, see issue #478). */
    {
        ast_t *prev = NULL;
        ast_t *node = subprogram_ast->child;
        while (node != NULL)
        {
            if (node->typ == PASCAL_T_TYPE_PARAM_LIST)
            {
                /* Unlink the node from the sibling chain. */
                if (prev == NULL)
                    subprogram_ast->child = node->next;
                else
                    prev->next = node->next;
                /* Sever the sibling link before freeing: free_ast() follows
                 * both child and next pointers, and we must not free the
                 * remaining AST siblings that follow this node. */
                node->next = NULL;
                free_ast(node);
                break;
            }
            prev = node;
            node = node->next;
        }
    }

    /* Rename local type declarations so each specialization gets unique names.
     * E.g., local type "RawT" in Swap<LongInt> becomes "Swap$LongInt.RawT". */
    char **local_names = NULL;
    int local_count = 0;
    collect_local_type_names(subprogram_ast, &local_names, &local_count);
    if (local_count > 0 && local_names != NULL)
    {
        char **renamed = (char **)malloc(local_count * sizeof(char *));
        if (renamed != NULL)
        {
            for (int i = 0; i < local_count; i++)
            {
                size_t len = strlen(specialized_name) + 1 + strlen(local_names[i]) + 1;
                renamed[i] = (char *)malloc(len);
                if (renamed[i] != NULL)
                    snprintf(renamed[i], len, "%s$%s", specialized_name, local_names[i]);
            }
            substitute_generic_identifiers(subprogram_ast, local_names, renamed, local_count);
            for (int i = 0; i < local_count; i++)
                free(renamed[i]);
            free(renamed);
        }
    }
    if (local_names != NULL)
    {
        for (int i = 0; i < local_count; i++)
            free(local_names[i]);
        free(local_names);
    }
}

static Tree_t *instantiate_generic_subprogram(Tree_t *template,
    char **arg_types, int arg_count, const char *specialized_name)
{
    if (template == NULL || arg_types == NULL || arg_count <= 0 ||
        template->tree_data.subprogram_data.generic_template_ast == NULL)
        return NULL;

    assert(template->tree_data.subprogram_data.is_generic_template);

    ast_t *ast_copy = copy_ast(template->tree_data.subprogram_data.generic_template_ast);
    if (ast_copy == NULL)
        return NULL;

    rewrite_generic_subprogram_ast(ast_copy, specialized_name,
        template->tree_data.subprogram_data.generic_type_params, arg_types, arg_count);

    /* Restore the source offset from the unit that defined the template,
     * so that source_index values in the specialized code map to the
     * correct source buffer (e.g., fgl.pp, not the main program). */
    int saved_offset = g_source_offset;
    g_source_offset = template->tree_data.subprogram_data.generic_template_source_offset;

    Tree_t *result = NULL;
    if (template->type == TREE_SUBPROGRAM &&
        template->tree_data.subprogram_data.sub_type == TREE_SUBPROGRAM_PROC)
        result = convert_procedure(ast_copy);
    else if (template->type == TREE_SUBPROGRAM &&
        template->tree_data.subprogram_data.sub_type == TREE_SUBPROGRAM_FUNC)
        result = convert_function(ast_copy);

    g_source_offset = saved_offset;
    free_ast(ast_copy);

    if (result != NULL)
    {
        /* Verify that the specialization has no generic metadata.
         * rewrite_generic_subprogram_ast() strips TYPE_PARAM_LIST so
         * convert_procedure/convert_function should not detect any
         * generic type parameters on the rewritten AST. */
        assert(result->tree_data.subprogram_data.num_generic_type_params == 0);
        assert(result->tree_data.subprogram_data.generic_type_params == NULL);
        assert(!result->tree_data.subprogram_data.is_generic_template);
        assert(result->tree_data.subprogram_data.generic_template_ast == NULL);
    }

    return result;
}

/* Check if a subprogram ID matches a generic base name.
 * Handles both exact match ("UnfixArray" == "UnfixArray") and
 * class-prefixed match ("TMarshal__UnfixArray" ends with "__UnfixArray").
 * This is needed because specialize calls inside generic record methods
 * use the short method name (e.g. "UnfixArray$T") while the subprogram
 * templates carry the class-qualified ID (e.g. "TMarshal__UnfixArray"). */
static int generic_base_name_matches(Tree_t *sub, const char *base)
{
    if (sub == NULL || base == NULL)
        return 0;
    /* For class methods, the bare method_name is the relevant identifier */
    if (sub->tree_data.subprogram_data.method_name != NULL &&
        strcasecmp(sub->tree_data.subprogram_data.method_name, base) == 0)
        return 1;
    /* For standalone subprograms, check the id directly */
    if (sub->tree_data.subprogram_data.id != NULL &&
        strcasecmp(sub->tree_data.subprogram_data.id, base) == 0)
        return 1;
    return 0;
}

static void collect_specialize_from_expr(struct Expression *expr, Tree_t *program_tree)
{
    if (expr == NULL || program_tree == NULL)
        return;

    switch (expr->type)
    {
        case EXPR_TYPECAST:
        {
            const char *target_id = expr->expr_data.typecast_data.target_type_id;
            char *base = NULL;
            char **args = NULL;
            int argc = 0;
            if (target_id != NULL &&
                parse_generic_mangled_id(target_id, &base, &args, &argc))
            {
                if (kgpc_getenv("KGPC_DEBUG_GENERIC_SUBPROGRAM") != NULL)
                    fprintf(stderr, "[KGPC] specialize expr target=%s base=%s argc=%d\n",
                        target_id, base ? base : "<null>", argc);
                /* Skip if any type arg looks like an unresolved type parameter */
                int has_unresolved_arg = 0;
                for (int i = 0; i < argc; ++i)
                {
                    if (args[i] != NULL && strlen(args[i]) == 1 && isupper((unsigned char)args[i][0]))
                    {
                        has_unresolved_arg = 1;
                        break;
                    }
                }
                if (!has_unresolved_arg)
                {
                    ListNode_t *cur = program_tree->tree_data.program_data.subprograms;
                    while (cur != NULL)
                {
                    if (cur->type == LIST_TREE && cur->cur != NULL)
                    {
                        Tree_t *sub = (Tree_t *)cur->cur;
                        if (sub->type == TREE_SUBPROGRAM &&
                            sub->tree_data.subprogram_data.id != NULL &&
                            sub->tree_data.subprogram_data.num_generic_type_params == argc &&
                            generic_base_name_matches(sub, base))
                        {
                            if (kgpc_getenv("KGPC_DEBUG_GENERIC_SUBPROGRAM") != NULL)
                                fprintf(stderr, "[KGPC] generic template match %s\n",
                                    sub->tree_data.subprogram_data.id);
                            /* Reconstruct proper specialized name with class prefix */
                            const char *sub_id = sub->tree_data.subprogram_data.id;
                            const char *spec_name = target_id;
                            char *constructed_name = NULL;
                            if (strcasecmp(sub_id, base) != 0)
                            {
                                size_t sub_len = strlen(sub_id);
                                size_t base_len = strlen(base);
                                size_t prefix_len = sub_len - base_len;
                                const char *dollar = strchr(target_id, '$');
                                if (dollar != NULL)
                                {
                                    size_t suffix_len = strlen(dollar);
                                    constructed_name = (char *)malloc(prefix_len + base_len + suffix_len + 1);
                                    if (constructed_name != NULL)
                                    {
                                        memcpy(constructed_name, sub_id, prefix_len);
                                        memcpy(constructed_name + prefix_len, base, base_len);
                                        memcpy(constructed_name + prefix_len + base_len, dollar, suffix_len);
                                        constructed_name[prefix_len + base_len + suffix_len] = '\0';
                                        spec_name = constructed_name;
                                    }
                                }
                            }
                            if (!subprogram_list_has_id(program_tree->tree_data.program_data.subprograms, spec_name))
                            {
                                Tree_t *specialized = instantiate_generic_subprogram(
                                    sub, args, argc, spec_name);
                                if (specialized != NULL)
                                    append_subprogram_node(&program_tree->tree_data.program_data.subprograms,
                                        specialized);
                            }
                            free(constructed_name);
                            break;
                        }
                    }
                    cur = cur->next;
                }
                }
            }
            if (args != NULL)
            {
                for (int i = 0; i < argc; ++i)
                    free(args[i]);
                free(args);
            }
            if (base != NULL)
                free(base);
            collect_specialize_from_expr(expr->expr_data.typecast_data.expr, program_tree);
            break;
        }
        case EXPR_FUNCTION_CALL:
        {
            const char *call_id = expr->expr_data.function_call_data.id;
            char *base = NULL;
            char **args = NULL;
            int argc = 0;
            if (call_id != NULL &&
                parse_generic_mangled_id(call_id, &base, &args, &argc))
            {
                /* Skip if any type arg looks like an unresolved type parameter
                 * (e.g. single uppercase letter "T").  These come from bodies
                 * of generic record methods that haven't been specialized yet. */
                int has_unresolved_arg = 0;
                for (int i = 0; i < argc; ++i)
                {
                    if (args[i] != NULL && strlen(args[i]) == 1 && isupper((unsigned char)args[i][0]))
                    {
                        has_unresolved_arg = 1;
                        break;
                    }
                }
                if (!has_unresolved_arg)
                {
                    ListNode_t *cur = program_tree->tree_data.program_data.subprograms;
                    while (cur != NULL)
                    {
                        if (cur->type == LIST_TREE && cur->cur != NULL)
                        {
                            Tree_t *sub = (Tree_t *)cur->cur;
                            if (sub->type == TREE_SUBPROGRAM &&
                                sub->tree_data.subprogram_data.id != NULL &&
                                sub->tree_data.subprogram_data.num_generic_type_params == argc &&
                                generic_base_name_matches(sub, base))
                            {
                                /* Build the proper specialized name: if the template has
                                 * a class prefix (e.g., "TMarshal__UnfixArray") and the
                                 * call_id is just "UnfixArray$TPtrWrapper", reconstruct
                                 * to "TMarshal__UnfixArray$TPtrWrapper". */
                                const char *sub_id = sub->tree_data.subprogram_data.id;
                                const char *spec_name = call_id;
                                char *constructed_name = NULL;
                                if (strcasecmp(sub_id, base) != 0)
                                {
                                    /* Template has a prefix: extract it */
                                    size_t sub_len = strlen(sub_id);
                                    size_t base_len = strlen(base);
                                    size_t prefix_len = sub_len - base_len;
                                    const char *dollar = strchr(call_id, '$');
                                    if (dollar != NULL)
                                    {
                                        size_t suffix_len = strlen(dollar);
                                        constructed_name = (char *)malloc(prefix_len + base_len + suffix_len + 1);
                                        if (constructed_name != NULL)
                                        {
                                            memcpy(constructed_name, sub_id, prefix_len);
                                            memcpy(constructed_name + prefix_len, base, base_len);
                                            memcpy(constructed_name + prefix_len + base_len, dollar, suffix_len);
                                            constructed_name[prefix_len + base_len + suffix_len] = '\0';
                                            spec_name = constructed_name;
                                        }
                                    }
                                }
                                if (!subprogram_list_has_id(program_tree->tree_data.program_data.subprograms, spec_name))
                                {
                                    Tree_t *specialized = instantiate_generic_subprogram(
                                        sub, args, argc, spec_name);
                                    if (specialized != NULL)
                                        append_subprogram_node(&program_tree->tree_data.program_data.subprograms,
                                            specialized);
                                }
                                free(constructed_name);
                                break;
                            }
                        }
                        cur = cur->next;
                    }
                }
            }
            if (args != NULL)
            {
                for (int i = 0; i < argc; ++i)
                    free(args[i]);
                free(args);
            }
            if (base != NULL)
                free(base);

            ListNode_t *arg = expr->expr_data.function_call_data.args_expr;
            while (arg != NULL)
            {
                if (arg->type == LIST_EXPR)
                    collect_specialize_from_expr((struct Expression *)arg->cur, program_tree);
                arg = arg->next;
            }
            break;
        }
        case EXPR_RECORD_ACCESS:
            collect_specialize_from_expr(expr->expr_data.record_access_data.record_expr, program_tree);
            break;
        case EXPR_ARRAY_ACCESS:
            collect_specialize_from_expr(expr->expr_data.array_access_data.array_expr, program_tree);
            collect_specialize_from_expr(expr->expr_data.array_access_data.index_expr, program_tree);
            break;
        case EXPR_RELOP:
            collect_specialize_from_expr(expr->expr_data.relop_data.left, program_tree);
            collect_specialize_from_expr(expr->expr_data.relop_data.right, program_tree);
            break;
        case EXPR_SIGN_TERM:
            collect_specialize_from_expr(expr->expr_data.sign_term, program_tree);
            break;
        case EXPR_ADDOP:
            collect_specialize_from_expr(expr->expr_data.addop_data.left_expr, program_tree);
            collect_specialize_from_expr(expr->expr_data.addop_data.right_term, program_tree);
            break;
        case EXPR_MULOP:
            collect_specialize_from_expr(expr->expr_data.mulop_data.left_term, program_tree);
            collect_specialize_from_expr(expr->expr_data.mulop_data.right_factor, program_tree);
            break;
        case EXPR_ADDR:
            collect_specialize_from_expr(expr->expr_data.addr_data.expr, program_tree);
            break;
        case EXPR_POINTER_DEREF:
            collect_specialize_from_expr(expr->expr_data.pointer_deref_data.pointer_expr, program_tree);
            break;
        default:
            break;
    }
}

static void collect_specialize_from_stmt(struct Statement *stmt, Tree_t *program_tree)
{
    if (stmt == NULL || program_tree == NULL)
        return;

    switch (stmt->type)
    {
        case STMT_VAR_ASSIGN:
            collect_specialize_from_expr(stmt->stmt_data.var_assign_data.var, program_tree);
            collect_specialize_from_expr(stmt->stmt_data.var_assign_data.expr, program_tree);
            break;
        case STMT_PROCEDURE_CALL:
        {
            ListNode_t *arg = stmt->stmt_data.procedure_call_data.expr_args;
            while (arg != NULL)
            {
                if (arg->type == LIST_EXPR)
                    collect_specialize_from_expr((struct Expression *)arg->cur, program_tree);
                arg = arg->next;
            }
            break;
        }
        case STMT_EXPR:
            collect_specialize_from_expr(stmt->stmt_data.expr_stmt_data.expr, program_tree);
            break;
        case STMT_COMPOUND_STATEMENT:
        {
            ListNode_t *cur = stmt->stmt_data.compound_statement;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT)
                    collect_specialize_from_stmt((struct Statement *)cur->cur, program_tree);
                cur = cur->next;
            }
            break;
        }
        case STMT_LABEL:
            collect_specialize_from_stmt(stmt->stmt_data.label_data.stmt, program_tree);
            break;
        case STMT_IF_THEN:
            collect_specialize_from_expr(stmt->stmt_data.if_then_data.relop_expr, program_tree);
            collect_specialize_from_stmt(stmt->stmt_data.if_then_data.if_stmt, program_tree);
            collect_specialize_from_stmt(stmt->stmt_data.if_then_data.else_stmt, program_tree);
            break;
        case STMT_WHILE:
            collect_specialize_from_expr(stmt->stmt_data.while_data.relop_expr, program_tree);
            collect_specialize_from_stmt(stmt->stmt_data.while_data.while_stmt, program_tree);
            break;
        case STMT_REPEAT:
        {
            ListNode_t *cur = stmt->stmt_data.repeat_data.body_list;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT)
                    collect_specialize_from_stmt((struct Statement *)cur->cur, program_tree);
                cur = cur->next;
            }
            collect_specialize_from_expr(stmt->stmt_data.repeat_data.until_expr, program_tree);
            break;
        }
        case STMT_FOR:
            collect_specialize_from_expr(stmt->stmt_data.for_data.to, program_tree);
            collect_specialize_from_stmt(stmt->stmt_data.for_data.do_for, program_tree);
            if (stmt->stmt_data.for_data.for_assign_type == STMT_VAR_ASSIGN)
                collect_specialize_from_stmt(stmt->stmt_data.for_data.for_assign_data.var_assign, program_tree);
            else
                collect_specialize_from_expr(stmt->stmt_data.for_data.for_assign_data.var, program_tree);
            break;
        case STMT_FOR_IN:
            collect_specialize_from_expr(stmt->stmt_data.for_in_data.loop_var, program_tree);
            collect_specialize_from_expr(stmt->stmt_data.for_in_data.collection, program_tree);
            collect_specialize_from_stmt(stmt->stmt_data.for_in_data.do_stmt, program_tree);
            break;
        case STMT_CASE:
        {
            collect_specialize_from_expr(stmt->stmt_data.case_data.selector_expr, program_tree);
            ListNode_t *branch = stmt->stmt_data.case_data.branches;
            while (branch != NULL)
            {
                if (branch->type == LIST_CASE_BRANCH && branch->cur != NULL)
                {
                    struct CaseBranch *cb = (struct CaseBranch *)branch->cur;
                    ListNode_t *label = cb->labels;
                    while (label != NULL)
                    {
                        if (label->type == LIST_EXPR)
                            collect_specialize_from_expr((struct Expression *)label->cur, program_tree);
                        label = label->next;
                    }
                    collect_specialize_from_stmt(cb->stmt, program_tree);
                }
                branch = branch->next;
            }
            collect_specialize_from_stmt(stmt->stmt_data.case_data.else_stmt, program_tree);
            break;
        }
        case STMT_WITH:
            collect_specialize_from_expr(stmt->stmt_data.with_data.context_expr, program_tree);
            collect_specialize_from_stmt(stmt->stmt_data.with_data.body_stmt, program_tree);
            break;
        case STMT_TRY_FINALLY:
        {
            ListNode_t *cur = stmt->stmt_data.try_finally_data.try_statements;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT)
                    collect_specialize_from_stmt((struct Statement *)cur->cur, program_tree);
                cur = cur->next;
            }
            cur = stmt->stmt_data.try_finally_data.finally_statements;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT)
                    collect_specialize_from_stmt((struct Statement *)cur->cur, program_tree);
                cur = cur->next;
            }
            break;
        }
        case STMT_TRY_EXCEPT:
        {
            ListNode_t *cur = stmt->stmt_data.try_except_data.try_statements;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT)
                    collect_specialize_from_stmt((struct Statement *)cur->cur, program_tree);
                cur = cur->next;
            }
            cur = stmt->stmt_data.try_except_data.except_statements;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT)
                    collect_specialize_from_stmt((struct Statement *)cur->cur, program_tree);
                cur = cur->next;
            }
            break;
        }
        case STMT_RAISE:
            collect_specialize_from_expr(stmt->stmt_data.raise_data.exception_expr, program_tree);
            break;
        case STMT_INHERITED:
            collect_specialize_from_expr(stmt->stmt_data.inherited_data.call_expr, program_tree);
            break;
        case STMT_EXIT:
            collect_specialize_from_expr(stmt->stmt_data.exit_data.return_expr, program_tree);
            break;
        default:
            break;
    }
}

void resolve_pending_generic_subprograms(Tree_t *program_tree)
{
    if (program_tree == NULL || program_tree->type != TREE_PROGRAM_TYPE)
        return;

    if (kgpc_getenv("KGPC_DEBUG_GENERIC_SUBPROGRAM") != NULL)
        fprintf(stderr, "[KGPC] resolve_pending_generic_subprograms\n");

    ListNode_t *sub = program_tree->tree_data.program_data.subprograms;
    if (kgpc_getenv("KGPC_DEBUG_GENERIC_SUBPROGRAM") != NULL && sub == NULL)
        fprintf(stderr, "[KGPC] program has no subprograms\n");
    while (sub != NULL)
    {
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_SUBPROGRAM") != NULL)
            fprintf(stderr, "[KGPC] subprogram node type=%d\n", sub->type);
        if (sub->type == LIST_TREE && sub->cur != NULL)
        {
            Tree_t *sub_tree = (Tree_t *)sub->cur;
            if (sub_tree->type == TREE_SUBPROGRAM)
            {
                if (kgpc_getenv("KGPC_DEBUG_GENERIC_SUBPROGRAM") != NULL &&
                    sub_tree->tree_data.subprogram_data.id != NULL)
                {
                    fprintf(stderr, "[KGPC] subprogram %s generic_params=%d\n",
                        sub_tree->tree_data.subprogram_data.id,
                        sub_tree->tree_data.subprogram_data.num_generic_type_params);
                }
                /* Skip bodies of unresolved generic templates — their expressions
                 * still contain raw type parameter names (e.g. "T") which would
                 * produce invalid specializations if matched. */
                if (sub_tree->tree_data.subprogram_data.generic_type_params == NULL &&
                    sub_tree->tree_data.subprogram_data.num_generic_type_params == 0)
                    collect_specialize_from_stmt(sub_tree->tree_data.subprogram_data.statement_list, program_tree);
            }
        }
        sub = sub->next;
    }

    collect_specialize_from_stmt(program_tree->tree_data.program_data.body_statement, program_tree);
    /* Scan unit init/final blocks for generic specializations */
    {
        CompilationContext *comp_ctx = compilation_context_get_active();
        if (comp_ctx != NULL) {
            for (int ui = 0; ui < comp_ctx->loaded_unit_count; ++ui) {
                Tree_t *unit_tree = comp_ctx->loaded_units[ui].unit_tree;
                if (unit_tree == NULL || unit_tree->type != TREE_UNIT)
                    continue;
                if (unit_tree->tree_data.unit_data.initialization != NULL)
                    collect_specialize_from_stmt(unit_tree->tree_data.unit_data.initialization, program_tree);
                if (unit_tree->tree_data.unit_data.finalization != NULL)
                    collect_specialize_from_stmt(unit_tree->tree_data.unit_data.finalization, program_tree);
            }
        }
    }
}

static int subprogram_list_has_decl(const ListNode_t *subprograms, const Tree_t *tree)
{
    if (subprograms == NULL || tree == NULL || tree->type != TREE_SUBPROGRAM)
        return 0;

    const char *id = tree->tree_data.subprogram_data.id;
    int line_num = tree->line_num;
    enum TreeType sub_type = tree->tree_data.subprogram_data.sub_type;

    const ListNode_t *cur = subprograms;
    while (cur != NULL)
    {
        Tree_t *existing = (Tree_t *)cur->cur;
        if (existing != NULL && existing->type == TREE_SUBPROGRAM)
        {
            if (existing->line_num == line_num &&
                existing->tree_data.subprogram_data.sub_type == sub_type)
            {
                const char *existing_id = existing->tree_data.subprogram_data.id;
                if (existing_id != NULL && id != NULL &&
                    pascal_identifier_equals(existing_id, id))
                {
                    return 1;
                }
            }
        }
        cur = cur->next;
    }

    return 0;
}

static void append_subprogram_if_unique(ListNode_t **dest, Tree_t *tree)
{
    if (dest == NULL || tree == NULL)
        return;

    if (subprogram_list_has_decl(*dest, tree))
    {
        destroy_tree(tree);
        return;
    }

    append_subprogram_node(dest, tree);
}

void append_subprograms_from_ast_recursive(ast_t *node, ListNode_t **subprograms,
    VisitedSet *visited)
{
    /* Iterate over siblings to avoid deep recursion on node->next chains */
    for (ast_t *cur = node; cur != NULL && cur != ast_nil; cur = cur->next)
    {
        if (subprograms == NULL || visited == NULL)
            return;

        if (!is_safe_to_continue(visited, cur))
            continue;

        switch (cur->typ)
        {
        case PASCAL_T_PROCEDURE_DECL: {
            Tree_t *proc = convert_procedure(cur);
            append_subprogram_if_unique(subprograms, proc);
            continue; /* skip child traversal for subprograms */
        }
        case PASCAL_T_FUNCTION_DECL: {
            Tree_t *func = convert_function(cur);
            append_subprogram_if_unique(subprograms, func);
            continue;
        }
        case PASCAL_T_METHOD_IMPL:
        case PASCAL_T_CONSTRUCTOR_DECL:
        case PASCAL_T_DESTRUCTOR_DECL: {
            Tree_t *method_tree = convert_method_impl(cur);
            append_subprogram_if_unique(subprograms, method_tree);
            continue;
        }
        default:
            break;
        }

        /* Recurse into children (tree depth is bounded) */
        append_subprograms_from_ast_recursive(cur->child, subprograms, visited);
    }
}

void append_top_level_subprograms_from_ast(ast_t *node, ListNode_t **subprograms,
    VisitedSet *visited, int in_subprogram)
{
    /* Iterate over siblings to avoid deep recursion on node->next chains */
    for (ast_t *cur = node; cur != NULL && cur != ast_nil; cur = cur->next)
    {
        if (subprograms == NULL || visited == NULL)
            return;

        if (visited_set_contains(visited, cur))
            continue;
        visited_set_add(visited, cur);

        int is_subprogram = (cur->typ == PASCAL_T_PROCEDURE_DECL ||
            cur->typ == PASCAL_T_FUNCTION_DECL ||
            cur->typ == PASCAL_T_METHOD_IMPL ||
            cur->typ == PASCAL_T_CONSTRUCTOR_DECL ||
            cur->typ == PASCAL_T_DESTRUCTOR_DECL);

        if (is_subprogram && !in_subprogram)
        {
            if (cur->typ == PASCAL_T_PROCEDURE_DECL)
            {
                Tree_t *proc = convert_procedure(cur);
                append_subprogram_if_unique(subprograms, proc);
            }
            else if (cur->typ == PASCAL_T_FUNCTION_DECL)
            {
                Tree_t *func = convert_function(cur);
                append_subprogram_if_unique(subprograms, func);
            }
            else
            {
                Tree_t *method_tree = convert_method_impl(cur);
                append_subprogram_if_unique(subprograms, method_tree);
            }
        }

        int child_in_subprogram = in_subprogram || is_subprogram;
        /* Recurse into children (tree depth is bounded) */
        append_top_level_subprograms_from_ast(cur->child, subprograms, visited, child_in_subprogram);
    }
}

static void sync_method_impls_from_generic_template(struct RecordType *record)
{
    if (record == NULL || record->generic_decl == NULL ||
        record->generic_decl->record_template == NULL ||
        record->method_templates == NULL)
        return;

    ListNode_t *source = record->generic_decl->record_template->method_templates;
    for (ListNode_t *cur = record->method_templates; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_METHOD_TEMPLATE)
            continue;
        struct MethodTemplate *tmpl = (struct MethodTemplate *)cur->cur;
        if (tmpl == NULL || tmpl->method_impl_ast != NULL || tmpl->name == NULL)
            continue;

        for (ListNode_t *src = source; src != NULL; src = src->next)
        {
            if (src->type != LIST_METHOD_TEMPLATE)
                continue;
            struct MethodTemplate *src_tmpl = (struct MethodTemplate *)src->cur;
            if (src_tmpl != NULL && src_tmpl->name != NULL &&
                strcasecmp(src_tmpl->name, tmpl->name) == 0 &&
                src_tmpl->method_impl_ast != NULL)
            {
                tmpl->method_impl_ast = copy_ast_detached(src_tmpl->method_impl_ast);
                break;
            }
        }
    }
}

static int subprogram_list_has_mangled(const ListNode_t *subprograms, const char *mangled_id)
{
    if (subprograms == NULL || mangled_id == NULL)
        return 0;

    const ListNode_t *cur = subprograms;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            const Tree_t *tree = (const Tree_t *)cur->cur;
            if ((tree->type == TREE_SUBPROGRAM_FUNC || tree->type == TREE_SUBPROGRAM_PROC) &&
                tree->tree_data.subprogram_data.mangled_id != NULL &&
                strcmp(tree->tree_data.subprogram_data.mangled_id, mangled_id) == 0)
            {
                return 1;
            }
        }
        cur = cur->next;
    }
    return 0;
}

void append_specialized_method_clones(Tree_t *decl, ListNode_t **subprograms)
{
    if (decl == NULL || subprograms == NULL)
        return;
    if (decl->type != TREE_TYPE_DECL)
        return;
    
    struct RecordType *record = NULL;
    
    // Get the record from either TYPE_DECL_RECORD or TYPE_DECL_ALIAS
    if (decl->tree_data.type_decl_data.kind == TYPE_DECL_RECORD) {
        record = decl->tree_data.type_decl_data.info.record;
    } else if (decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
        record = decl->tree_data.type_decl_data.info.alias.inline_record_type;
        if (record == NULL && decl->tree_data.type_decl_data.kgpc_type != NULL)
        {
            KgpcType *alias_type = decl->tree_data.type_decl_data.kgpc_type;
            if (kgpc_type_is_record(alias_type))
                record = kgpc_type_get_record(alias_type);
            else if (kgpc_type_is_pointer(alias_type) && alias_type->info.points_to != NULL &&
                     kgpc_type_is_record(alias_type->info.points_to))
                record = kgpc_type_get_record(alias_type->info.points_to);
        }
    } else {
        return;
    }

    sync_method_impls_from_generic_template(record);

    if (record == NULL || record->method_templates == NULL ||
        record->generic_decl == NULL || record->generic_args == NULL ||
        record->num_generic_args <= 0)
    {
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL &&
            record != NULL && record->type_id != NULL)
            fprintf(stderr, "[KGPC] skipping clone for %s (missing templates)\n", record->type_id);
        return;
    }

    /* Skip records whose generic_args are still unresolved type parameters.
     * E.g. TFoo$T with generic_args=["T"] matching generic_decl params=["T"].
     * Method clones from these would produce bodies with unresolved type refs. */
    if (record->generic_decl->type_parameters != NULL)
    {
        int all_unresolved = 1;
        for (int i = 0; i < record->num_generic_args && i < record->generic_decl->num_type_params; ++i)
        {
            const char *arg = record->generic_args[i];
            const char *param = record->generic_decl->type_parameters[i];
            if (arg == NULL || param == NULL || strcasecmp(arg, param) != 0)
            {
                all_unresolved = 0;
                break;
            }
        }
        if (all_unresolved)
        {
            if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL &&
                record->type_id != NULL)
                fprintf(stderr, "[KGPC] skipping clone for %s (unresolved type params)\n", record->type_id);
            return;
        }
    }
    if (record->method_clones_emitted)
        return;

    const char *debug_env = kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES");
    int appended_any = 0;
    ListNode_t *cur = record->method_templates;
    while (cur != NULL)
    {
        if (cur->type == LIST_METHOD_TEMPLATE)
        {
            struct MethodTemplate *template = (struct MethodTemplate *)cur->cur;
            if (template != NULL)
            {
                Tree_t *method_tree = instantiate_method_template(template, record);
                if (method_tree != NULL) {
                    if (method_tree->tree_data.subprogram_data.mangled_id != NULL &&
                        subprogram_list_has_mangled(*subprograms,
                            method_tree->tree_data.subprogram_data.mangled_id))
                    {
                        destroy_tree(method_tree);
                        method_tree = NULL;
                    }
                }
                if (method_tree != NULL) {
                    append_subprogram_node(subprograms, method_tree);
                    appended_any = 1;
                    if (debug_env != NULL && record->type_id != NULL && template->name != NULL)
                        fprintf(stderr, "[KGPC] cloned method %s.%s\n", record->type_id, template->name);
                } else if (debug_env != NULL && record->type_id != NULL && template->name != NULL) {
                    fprintf(stderr, "[KGPC] failed to clone method %s.%s (missing implementation)\n",
                            record->type_id, template->name);
                }
            }
        }
        cur = cur->next;
    }
    if (appended_any)
        record->method_clones_emitted = 1;
}

void flush_deferred_inline_specializations(Tree_t *program_tree)
{
    DeferredInlineSpec *cur = g_deferred_inline_specs;
    g_deferred_inline_specs = NULL;

    ListNode_t **type_list = NULL;
    ListNode_t **subprograms = NULL;
    if (program_tree != NULL && program_tree->type == TREE_PROGRAM_TYPE) {
        type_list = &program_tree->tree_data.program_data.type_declaration;
        subprograms = &program_tree->tree_data.program_data.subprograms;
    } else if (program_tree != NULL && program_tree->type == TREE_UNIT) {
        type_list = &program_tree->tree_data.unit_data.interface_type_decls;
        subprograms = &program_tree->tree_data.unit_data.subprograms;
    }

    if (type_list == NULL) {
        while (cur != NULL) {
            DeferredInlineSpec *next = cur->next;
            if (cur->type_decl != NULL)
                destroy_tree(cur->type_decl);
            free(cur);
            cur = next;
        }
        return;
    }

    while (cur != NULL) {
        DeferredInlineSpec *next = cur->next;
        if (cur->type_decl != NULL) {
            /* Emit method clones for this inline specialization */
            if (subprograms != NULL)
                append_specialized_method_clones(cur->type_decl, subprograms);
            ListNode_t *node = CreateListNode(cur->type_decl, LIST_TREE);
            if (node != NULL) {
                node->next = *type_list;
                *type_list = node;
            } else {
                destroy_tree(cur->type_decl);
            }
        }
        free(cur);
        cur = next;
    }
}

void append_generic_method_clones(Tree_t *program_tree)
{
    if (program_tree == NULL || program_tree->type != TREE_PROGRAM_TYPE)
        return;

    ListNode_t *type_cursor = program_tree->tree_data.program_data.type_declaration;
    while (type_cursor != NULL)
    {
        if (type_cursor->type == LIST_TREE && type_cursor->cur != NULL)
        {
            Tree_t *decl = (Tree_t *)type_cursor->cur;
            append_specialized_method_clones(decl,
                &program_tree->tree_data.program_data.subprograms);
        }
        type_cursor = type_cursor->next;
    }
}

/* Clone nested type declarations from a generic class for a specialization.
 * For example, if TFPGList has nested type TFPGList.PT = ^T, then specializing
 * TFPGList<TMyRecord> produces TFPGList$TMyRecord.PT = ^TMyRecord.
 * The cloned declarations are appended to the program's type declaration list. */
static void clone_nested_types_for_specialization(
    GenericTypeDecl *generic_decl,
    const char *specialized_name, /* e.g. "TFPGList$TMyRecord" */
    ListNode_t *type_args,        /* concrete type arguments */
    ListNode_t **type_list_out,   /* where to append new type decls */
    ListNode_t **subprograms_out) /* where to append method clones (may be NULL) */
{
    if (generic_decl == NULL || generic_decl->nested_type_decls == NULL ||
        specialized_name == NULL || type_list_out == NULL)
        return;

    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
    const char *generic_name = generic_decl->name;
    size_t prefix_len = generic_name != NULL ? strlen(generic_name) : 0;

    /* Build arg_types array for substitution */
    int arg_count = 0;
    ListNode_t *arg_cur = type_args;
    while (arg_cur != NULL) {
        if (arg_cur->type == LIST_STRING) arg_count++;
        arg_cur = arg_cur->next;
    }

    ListNode_t *cur = generic_decl->nested_type_decls;
    while (cur != NULL) {
        if (cur->type == LIST_TREE && cur->cur != NULL) {
            Tree_t *orig = (Tree_t *)cur->cur;
            if (orig->type == TREE_TYPE_DECL && orig->tree_data.type_decl_data.id != NULL) {
                const char *orig_id = orig->tree_data.type_decl_data.id;
                /* Check that orig_id starts with generic_name + "." */
                if (prefix_len > 0 && strncmp(orig_id, generic_name, prefix_len) == 0 &&
                    orig_id[prefix_len] == '.')
                {
                    const char *suffix = orig_id + prefix_len; /* e.g., ".PT" */
                    /* Build new id: specialized_name + suffix */
                    size_t new_id_len = strlen(specialized_name) + strlen(suffix) + 1;
                    char *new_id = (char *)malloc(new_id_len);
                    if (new_id == NULL) { cur = cur->next; continue; }
                    snprintf(new_id, new_id_len, "%s%s", specialized_name, suffix);

                    Tree_t *clone = mk_typedecl(orig->line_num, new_id, 0, 0);
                    if (clone == NULL) { free(new_id); cur = cur->next; continue; }

                    clone->source_index = orig->source_index;
                    clone->tree_data.type_decl_data.kind = orig->tree_data.type_decl_data.kind;
                    clone->tree_data.type_decl_data.defined_in_unit = orig->tree_data.type_decl_data.defined_in_unit;

                    if (orig->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
                        struct TypeAlias *src_alias = &orig->tree_data.type_decl_data.info.alias;
                        struct TypeAlias *dst_alias = &clone->tree_data.type_decl_data.info.alias;
                        *dst_alias = *src_alias; /* Shallow copy */
                        dst_alias->target_type_id = src_alias->target_type_id ?
                            strdup(src_alias->target_type_id) : NULL;
                        dst_alias->pointer_type_id = src_alias->pointer_type_id ?
                            strdup(src_alias->pointer_type_id) : NULL;
                        dst_alias->array_element_type_id = src_alias->array_element_type_id ?
                            strdup(src_alias->array_element_type_id) : NULL;
                        dst_alias->inline_record_type = NULL;
                        dst_alias->target_type_ref = src_alias->target_type_ref ?
                            type_ref_clone(src_alias->target_type_ref) : NULL;
                        dst_alias->array_element_type_ref = src_alias->array_element_type_ref ?
                            type_ref_clone(src_alias->array_element_type_ref) : NULL;

                        /* Substitute generic type parameters in type identifiers.
                         * For example, if T → TMyRecord, then "T" → "TMyRecord",
                         * and pointer_type_id "T" → "TMyRecord". */
                        for (int pi = 0; pi < generic_decl->num_type_params && pi < arg_count; pi++) {
                            const char *param = generic_decl->type_parameters[pi];
                            /* Get the concrete type for this parameter */
                            ListNode_t *a = type_args;
                            for (int ai = 0; ai < pi && a != NULL; ai++)
                                a = a->next;
                            if (a == NULL || a->type != LIST_STRING || a->cur == NULL)
                                continue;
                            const char *concrete = (const char *)a->cur;

                            if (dst_alias->target_type_id != NULL &&
                                strcasecmp(dst_alias->target_type_id, param) == 0) {
                                free(dst_alias->target_type_id);
                                dst_alias->target_type_id = strdup(concrete);
                            }
                            if (dst_alias->pointer_type_id != NULL &&
                                strcasecmp(dst_alias->pointer_type_id, param) == 0) {
                                free(dst_alias->pointer_type_id);
                                dst_alias->pointer_type_id = strdup(concrete);
                            }
                            if (dst_alias->array_element_type_id != NULL &&
                                strcasecmp(dst_alias->array_element_type_id, param) == 0) {
                                free(dst_alias->array_element_type_id);
                                dst_alias->array_element_type_id = strdup(concrete);
                            }
                        }

                        /* Substitute type parameters in mangled names like
                         * "TFPGListEnumerator$T" -> "TFPGListEnumerator$TMyRecord" */
                        if (dst_alias->target_type_id != NULL) {
                            for (int pi = 0; pi < generic_decl->num_type_params && pi < arg_count; pi++) {
                                const char *param = generic_decl->type_parameters[pi];
                                ListNode_t *a = type_args;
                                for (int ai = 0; ai < pi && a != NULL; ai++)
                                    a = a->next;
                                if (a == NULL || a->type != LIST_STRING || a->cur == NULL)
                                    continue;
                                const char *concrete = (const char *)a->cur;
                                if (param == NULL || concrete == NULL) continue;
                                size_t param_len = strlen(param);
                                /* Look for "$T" pattern at the end or "$T$" in the middle */
                                char *pos = dst_alias->target_type_id;
                                while ((pos = strchr(pos, '$')) != NULL) {
                                    pos++; /* skip '$' */
                                    if (strncasecmp(pos, param, param_len) == 0 &&
                                        (pos[param_len] == '\0' || pos[param_len] == '$')) {
                                        /* Found "$T" — rebuild the string with "$TMyRecord" */
                                        size_t prefix_len2 = (pos - dst_alias->target_type_id);
                                        size_t suffix_len = strlen(pos + param_len);
                                        size_t concrete_len = strlen(concrete);
                                        char *new_target = malloc(prefix_len2 + concrete_len + suffix_len + 1);
                                        if (new_target != NULL) {
                                            memcpy(new_target, dst_alias->target_type_id, prefix_len2);
                                            memcpy(new_target + prefix_len2, concrete, concrete_len);
                                            memcpy(new_target + prefix_len2 + concrete_len,
                                                   pos + param_len, suffix_len + 1);
                                            free(dst_alias->target_type_id);
                                            dst_alias->target_type_id = new_target;
                                        }
                                        break;
                                    }
                                }
                            }
                        }

                        /* Also substitute qualified names: if target_type_id refers
                         * to another nested type of the same generic (e.g., "TFPGList.PT"),
                         * rewrite to "TFPGList$TMyRecord.PT" */
                        if (dst_alias->target_type_id != NULL &&
                            strncmp(dst_alias->target_type_id, generic_name, prefix_len) == 0 &&
                            dst_alias->target_type_id[prefix_len] == '.') {
                            const char *inner_suffix = dst_alias->target_type_id + prefix_len;
                            size_t new_target_len = strlen(specialized_name) + strlen(inner_suffix) + 1;
                            char *new_target = (char *)malloc(new_target_len);
                            if (new_target != NULL) {
                                snprintf(new_target, new_target_len, "%s%s", specialized_name, inner_suffix);
                                free(dst_alias->target_type_id);
                                dst_alias->target_type_id = new_target;
                            }
                        }
                    }

                    /* If the target_type_id is a mangled generic name like
                     * "TFPGListEnumerator$TMyRecord", ensure that specialization
                     * actually exists by triggering instantiation. */
                    int alias_type_set = 0;
                    if (clone->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
                        struct TypeAlias *dst_alias = &clone->tree_data.type_decl_data.info.alias;
                        if (debug_env != NULL && dst_alias->target_type_id != NULL)
                            fprintf(stderr, "[KGPC] clone alias %s target_type_id=%s\n",
                                clone->tree_data.type_decl_data.id, dst_alias->target_type_id);
                        if (dst_alias->target_type_id != NULL) {
                            char *dollar = strchr(dst_alias->target_type_id, '$');
                            if (dollar != NULL) {
                                /* Extract base name and type args from mangled name */
                                size_t base_len = dollar - dst_alias->target_type_id;
                                char *inner_base = strndup(dst_alias->target_type_id, base_len);
                                GenericTypeDecl *inner_generic = generic_registry_find_decl(inner_base);
                                if (inner_generic != NULL) {
                                    /* Build type args from the suffix */
                                    const char *arg_str = dollar + 1;
                                    ListNode_t *inner_args = CreateListNode(strdup(arg_str), LIST_STRING);
                                    char *inner_spec_name = NULL;
                                    struct RecordType *inner_record =
                                        instantiate_generic_record(inner_base, inner_args, &inner_spec_name);
                                    if (inner_record != NULL) {
                                        /* Set the alias clone's inline_record_type to the
                                         * specialized record.  The semantic checker will
                                         * register both the mangled name (inner_record->type_id)
                                         * and the alias name in the symbol table (see line 7012+
                                         * in SemCheck.c).  This also enables constructor
                                         * resolution via record_info. */
                                        dst_alias->inline_record_type = inner_record;
                                        dst_alias->base_type = RECORD_TYPE;
                                        KgpcType *alias_type = create_record_type(inner_record);
                                        if (inner_record->is_class) {
                                            KgpcType *ptr = create_pointer_type(alias_type);
                                            kgpc_type_release(alias_type);
                                            alias_type = ptr;
                                        }
                                        clone->tree_data.type_decl_data.kgpc_type = alias_type;
                                        alias_type_set = 1;
                                        if (debug_env != NULL)
                                            fprintf(stderr, "[KGPC] triggered nested specialization %s\n",
                                                inner_spec_name);
                                    }
                                    if (inner_spec_name != NULL) free(inner_spec_name);
                                    destroy_list(inner_args);
                                }
                                free(inner_base);
                            }
                        }
                    }

                    /* Also handle procedure/function type declarations */
                    if (!alias_type_set && orig->tree_data.type_decl_data.kgpc_type != NULL) {
                        clone->tree_data.type_decl_data.kgpc_type = orig->tree_data.type_decl_data.kgpc_type;
                        kgpc_type_retain(clone->tree_data.type_decl_data.kgpc_type);
                    }

                    ListNode_t *node = CreateListNode(clone, LIST_TREE);
                    if (node != NULL) {
                        node->next = *type_list_out;
                        *type_list_out = node;
                    } else {
                        destroy_tree(clone);
                    }

                    /* If we triggered a nested specialization via inline_record_type,
                     * also emit method clones for the inner specialization. */
                    if (alias_type_set && subprograms_out != NULL)
                        append_specialized_method_clones(clone, subprograms_out);

                    if (debug_env != NULL)
                        fprintf(stderr, "[KGPC] cloned nested type %s -> %s\n", orig_id, clone->tree_data.type_decl_data.id);
                }
            }
        }
        cur = cur->next;
    }
}

void resolve_pending_generic_aliases(Tree_t *program_tree)
{
    PendingGenericAlias *cur = g_pending_generic_aliases;
    g_pending_generic_aliases = NULL;
    ListNode_t **clone_dest = NULL;
    ListNode_t **type_list = NULL;
    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
    if (program_tree != NULL && program_tree->type == TREE_PROGRAM_TYPE) {
        clone_dest = &program_tree->tree_data.program_data.subprograms;
        type_list = &program_tree->tree_data.program_data.type_declaration;
    }

    while (cur != NULL) {
        PendingGenericAlias *next = cur->next;
        char *specialized_name = NULL;
        struct RecordType *record = instantiate_generic_record(cur->base_name, cur->type_args, &specialized_name);
        if (record != NULL && cur->decl != NULL &&
            cur->decl->type == TREE_TYPE_DECL &&
            cur->decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
        {
            struct TypeAlias *alias = &cur->decl->tree_data.type_decl_data.info.alias;
            if (alias->inline_record_type != NULL)
                destroy_record_type(alias->inline_record_type);
            alias->inline_record_type = record;
            alias->base_type = RECORD_TYPE;
            if (cur->decl->tree_data.type_decl_data.kgpc_type == NULL) {
                KgpcType *inline_type = create_record_type(record);
                if (record->is_class) {
                    KgpcType *ptr = create_pointer_type(inline_type);
                    kgpc_type_release(inline_type);
                    inline_type = ptr;
                }
                cur->decl->tree_data.type_decl_data.kgpc_type = inline_type;
            }
            if (clone_dest != NULL)
                append_specialized_method_clones(cur->decl, clone_dest);

            /* Clone nested type declarations for this specialization */
            if (type_list != NULL && specialized_name != NULL && cur->base_name != NULL) {
                GenericTypeDecl *generic = generic_registry_find_decl(cur->base_name);
                if (generic != NULL && generic->nested_type_decls != NULL) {
                    clone_nested_types_for_specialization(generic, specialized_name,
                        cur->type_args, type_list, clone_dest);
                }
            }

            if (debug_env != NULL && cur->decl->tree_data.type_decl_data.id != NULL &&
                cur->base_name != NULL)
            {
                fprintf(stderr, "[KGPC] resolved generic alias %s for %s\n",
                        cur->decl->tree_data.type_decl_data.id, cur->base_name);
            }
        } else {
            fprintf(stderr, "ERROR: Failed to instantiate generic record %s for deferred alias.\n",
                    cur->base_name != NULL ? cur->base_name : "<unknown>");
            if (record != NULL)
                destroy_record_type(record);
        }
        if (specialized_name != NULL)
            free(specialized_name);
        if (cur->base_name != NULL)
            free(cur->base_name);
        if (cur->type_args != NULL)
            destroy_list(cur->type_args);
        free(cur);
        cur = next;
    }
}

static char *mangle_specialized_type_name(const char *base_name, char **type_ids, int num_types);
static void substitute_record_type_parameters(struct RecordType *record, GenericTypeDecl *generic_decl, char **arg_types);
static void substitute_record_field(struct RecordField *field, GenericTypeDecl *generic_decl, char **arg_types);
static Tree_t *instantiate_method_template(struct MethodTemplate *method_template, struct RecordType *record);

ast_t *find_ast_node_type(ast_t *node, int typ)
{
    if (node == NULL)
        return NULL;
    if (node->typ == typ)
        return node;
    for (ast_t *child = node->child; child != NULL; child = child->next)
    {
        ast_t *found = find_ast_node_type(child, typ);
        if (found != NULL)
            return found;
    }
    return NULL;
}

int is_method_decl_keyword(const char *sym_name)
{
    if (sym_name == NULL)
        return 0;
    return (strcasecmp(sym_name, "virtual") == 0 ||
            strcasecmp(sym_name, "override") == 0 ||
            strcasecmp(sym_name, "static") == 0 ||
            strcasecmp(sym_name, "abstract") == 0 ||
            strcasecmp(sym_name, "inline") == 0 ||
            strcasecmp(sym_name, "cdecl") == 0 ||
            strcasecmp(sym_name, "stdcall") == 0 ||
            strcasecmp(sym_name, "constructor") == 0 ||
            strcasecmp(sym_name, "destructor") == 0 ||
            strcasecmp(sym_name, "function") == 0 ||
            strcasecmp(sym_name, "procedure") == 0 ||
            strcasecmp(sym_name, "operator") == 0 ||
            strcasecmp(sym_name, "class") == 0);
}
static void rewrite_method_impl_ast(ast_t *method_ast, struct RecordType *record);
static void substitute_generic_identifier_nodes(ast_t *node, struct RecordType *record);
