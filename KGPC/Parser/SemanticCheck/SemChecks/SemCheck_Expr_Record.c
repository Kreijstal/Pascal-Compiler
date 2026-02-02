/*
    SemCheck_Expr_Record.c - Record/class lookup helpers

    This file contains helpers for resolving record/class metadata
    and walking inheritance for fields, methods, and properties.

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"

struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    if (node == NULL) return NULL;
    
    /* Use hashnode helper which handles NULL KgpcType */
    struct RecordType *record = hashnode_get_record_type(node);
    if (record != NULL)
        return record;
        
    /* If not a direct record, check if it's a pointer to a record (Class types are pointers) */
    if (node->type != NULL && kgpc_type_is_pointer(node->type))
    {
        KgpcType *pointed_to = node->type->info.points_to;
        if (pointed_to != NULL && kgpc_type_is_record(pointed_to))
        {
            return kgpc_type_get_record(pointed_to);
        }
    }
    
    return NULL;
}

struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    return hashnode_get_type_alias(node);
}

int node_is_record_type(HashNode_t *node)
{
    return hashnode_is_record(node);
}

struct RecordType *semcheck_lookup_parent_record(SymTab_t *symtab,
    struct RecordType *record_info)
{
    if (symtab == NULL || record_info == NULL ||
        record_info->parent_class_name == NULL)
        return NULL;

    HashNode_t *parent_node = NULL;
    if (FindIdent(&parent_node, symtab, record_info->parent_class_name) == -1 ||
        parent_node == NULL)
        return NULL;

    return get_record_type_from_node(parent_node);
}

struct ClassProperty *semcheck_find_class_property(SymTab_t *symtab,
    struct RecordType *record_info, const char *property_name,
    struct RecordType **owner_out)
{
    if (owner_out != NULL)
        *owner_out = NULL;
    if (symtab == NULL || record_info == NULL || property_name == NULL)
        return NULL;

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_find_class_property: searching for '%s' in record_info=%p\n",
            property_name, record_info);
        fprintf(stderr, "[SemCheck]   record_info->properties=%p\n", record_info->properties);
    }

    struct RecordType *current = record_info;
    while (current != NULL)
    {
        ListNode_t *node = current->properties;
        int prop_count = 0;
        while (node != NULL)
        {

            if (node->type == LIST_CLASS_PROPERTY && node->cur != NULL)
            {
                struct ClassProperty *property = (struct ClassProperty *)node->cur;

                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck]   Found property: '%s'\n",
                        property->name ? property->name : "<null>");
                }
                prop_count++;
                if (property->name != NULL &&
                    pascal_identifier_equals(property->name, property_name))
                {

                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck]   MATCHED property '%s'!\n", property->name);
                    }
                    if (owner_out != NULL)
                        *owner_out = current;
                    return property;
                }
            }
            node = node->next;
        }
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck]   Searched %d properties in this record, no match\n", prop_count);
        }
        current = semcheck_lookup_parent_record(symtab, current);
    }
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck]   Property '%s' NOT FOUND\n", property_name);
    }
    return NULL;
}

static struct RecordField *semcheck_find_class_field_impl(SymTab_t *symtab,
    struct RecordType *record_info, const char *field_name,
    struct RecordType **owner_out, int include_hidden)
{
    if (owner_out != NULL)
        *owner_out = NULL;
    if (symtab == NULL || record_info == NULL || field_name == NULL)
        return NULL;

    struct RecordType *current = record_info;
    while (current != NULL)
    {
        ListNode_t *field_node = current->fields;
        while (field_node != NULL)
        {
            if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
            {
                struct RecordField *field = (struct RecordField *)field_node->cur;
                if (field->name != NULL &&
                    (include_hidden || !record_field_is_hidden(field)) &&
                    pascal_identifier_equals(field->name, field_name))
                {
                    if (owner_out != NULL)
                        *owner_out = current;
                    return field;
                }
            }
            field_node = field_node->next;
        }
        current = semcheck_lookup_parent_record(symtab, current);
    }
    return NULL;
}

struct RecordField *semcheck_find_class_field(SymTab_t *symtab,
    struct RecordType *record_info, const char *field_name,
    struct RecordType **owner_out)
{
    return semcheck_find_class_field_impl(symtab, record_info, field_name,
        owner_out, 0);
}

struct RecordField *semcheck_find_class_field_including_hidden(SymTab_t *symtab,
    struct RecordType *record_info, const char *field_name,
    struct RecordType **owner_out)
{
    return semcheck_find_class_field_impl(symtab, record_info, field_name,
        owner_out, 1);
}

HashNode_t *semcheck_find_class_method(SymTab_t *symtab,
    struct RecordType *record_info, const char *method_name,
    struct RecordType **owner_out)
{
    if (owner_out != NULL)
        *owner_out = NULL;
    if (symtab == NULL || record_info == NULL || method_name == NULL)
        return NULL;

    struct RecordType *current = record_info;
    /* Limit iterations to prevent infinite loops from circular inheritance */
    int max_iterations = 100;
    int iterations = 0;
    while (current != NULL && iterations < max_iterations)
    {
        iterations++;
        if (current->type_id != NULL)
        {
            char mangled_name[256];
            snprintf(mangled_name, sizeof(mangled_name), "%s__%s", current->type_id, method_name);
            
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] semcheck_find_class_method: Searching for '%s' (mangled: '%s') in class '%s'\n", 
                    method_name, mangled_name, current->type_id);
            }

            HashNode_t *method_node = NULL;
            int find_result = FindIdent(&method_node, symtab, mangled_name);

            if (find_result != -1 && method_node != NULL)
            {
                if (method_node->hash_type == HASHTYPE_FUNCTION_RETURN)
                {
                    ListNode_t *all_methods = FindAllIdents(symtab, mangled_name);
                    ListNode_t *cur = all_methods;
                    while (cur != NULL)
                    {
                        HashNode_t *candidate = (HashNode_t *)cur->cur;
                        if (candidate != NULL &&
                            (candidate->hash_type == HASHTYPE_FUNCTION ||
                             candidate->hash_type == HASHTYPE_PROCEDURE))
                        {
                            method_node = candidate;
                            break;
                        }
                        cur = cur->next;
                    }
                    if (all_methods != NULL)
                        DestroyList(all_methods);
                }

                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_find_class_method: Found '%s' in class '%s'\n", 
                        method_name, current->type_id);
                }
                if (owner_out != NULL)
                    *owner_out = current;
                return method_node;
            }
        }
        
        /* For type helpers, walk up the helper parent chain */
        if (current->is_type_helper && current->helper_parent_id != NULL)
        {
            HashNode_t *parent_node = NULL;
            if (FindIdent(&parent_node, symtab, current->helper_parent_id) != -1 && parent_node != NULL)
            {
                struct RecordType *parent_helper = get_record_type_from_node(parent_node);
                if (parent_helper != NULL && parent_helper->is_type_helper)
                {
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck] semcheck_find_class_method: Walking up helper parent chain to '%s'\n", 
                            current->helper_parent_id);
                    }
                    current = parent_helper;
                    continue;
                }
            }
        }
        
        current = semcheck_lookup_parent_record(symtab, current);
    }
    return NULL;
}

