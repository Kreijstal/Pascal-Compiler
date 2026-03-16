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

    HashNode_t *parent_node = semcheck_find_preferred_type_node(symtab, record_info->parent_class_name);
    if (parent_node == NULL)
        return NULL;

    return get_record_type_from_node(parent_node);
}

static struct RecordField *semcheck_find_field_in_members(ListNode_t *members,
    const char *field_name, int include_hidden)
{
    if (members == NULL || field_name == NULL)
        return NULL;

    for (ListNode_t *cur = members; cur != NULL; cur = cur->next)
    {
        if (cur->type == LIST_RECORD_FIELD && cur->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            if (field->name != NULL &&
                (include_hidden || !record_field_is_hidden(field)) &&
                pascal_identifier_equals(field->name, field_name))
            {
                return field;
            }
        }
        else if (cur->type == LIST_VARIANT_PART && cur->cur != NULL)
        {
            struct VariantPart *variant = (struct VariantPart *)cur->cur;
            for (ListNode_t *b = variant->branches; b != NULL; b = b->next)
            {
                if (b->type != LIST_VARIANT_BRANCH || b->cur == NULL)
                    continue;
                struct VariantBranch *branch = (struct VariantBranch *)b->cur;
                struct RecordField *found =
                    semcheck_find_field_in_members(branch->members, field_name, include_hidden);
                if (found != NULL)
                    return found;
            }
        }
    }

    return NULL;
}

struct ClassProperty *semcheck_find_class_property(SymTab_t *symtab,
    struct RecordType *record_info, const char *property_name,
    struct RecordType **owner_out)
{
    if (owner_out != NULL)
        *owner_out = NULL;
    if (symtab == NULL || record_info == NULL || property_name == NULL)
        return NULL;

    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[SemCheck] semcheck_find_class_property: searching for '%s' in record_info=%p\n",
            property_name, record_info);
        fprintf(stderr, "[SemCheck]   record_info->properties=%p\n", record_info->properties);
    }

    struct RecordType *current = record_info;
    while (current != NULL)
    {
        if (kgpc_getenv("KGPC_DEBUG_SYMCREAT_PROPS") != NULL &&
            (pascal_identifier_equals(property_name, "is_registered") ||
             pascal_identifier_equals(property_name, "forwarddef") ||
             pascal_identifier_equals(property_name, "realname")))
        {
            int debug_prop_count = 0;
            for (ListNode_t *tmp = current->properties; tmp != NULL; tmp = tmp->next)
            {
                if (tmp->type == LIST_CLASS_PROPERTY && tmp->cur != NULL)
                    debug_prop_count++;
            }
            fprintf(stderr,
                "[KGPC_DEBUG_SYMCREAT_PROPS] lookup property=%s on class=%s parent=%s prop_count=%d\n",
                property_name,
                current->type_id != NULL ? current->type_id : "<null>",
                current->parent_class_name != NULL ? current->parent_class_name : "<null>",
                debug_prop_count);
        }
        /* Search both class properties and record_properties (plain advanced records) */
        for (int pass = 0; pass < 2; pass++)
        {
            ListNode_t *node = (pass == 0) ? current->properties : current->record_properties;
            int prop_count = 0;
            while (node != NULL)
            {

                if (node->type == LIST_CLASS_PROPERTY && node->cur != NULL)
                {
                    struct ClassProperty *property = (struct ClassProperty *)node->cur;
                    if (kgpc_getenv("KGPC_DEBUG_SYMCREAT_PROPS") != NULL &&
                        (pascal_identifier_equals(property_name, "is_registered") ||
                         pascal_identifier_equals(property_name, "forwarddef") ||
                         pascal_identifier_equals(property_name, "realname")))
                    {
                        fprintf(stderr,
                            "[KGPC_DEBUG_SYMCREAT_PROPS] candidate property=%s owner=%s read=%s write=%s\n",
                            property->name != NULL ? property->name : "<null>",
                            current->type_id != NULL ? current->type_id : "<null>",
                            property->read_accessor != NULL ? property->read_accessor : "<null>",
                            property->write_accessor != NULL ? property->write_accessor : "<null>");
                    }

                    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                        fprintf(stderr, "[SemCheck]   Found property: '%s'\n",
                            property->name ? property->name : "<null>");
                    }
                    prop_count++;
                    if (property->name != NULL &&
                        pascal_identifier_equals(property->name, property_name))
                    {
                        if (kgpc_getenv("KGPC_DEBUG_SYMCREAT_PROPS") != NULL &&
                            (pascal_identifier_equals(property_name, "is_registered") ||
                             pascal_identifier_equals(property_name, "forwarddef") ||
                             pascal_identifier_equals(property_name, "realname")))
                        {
                            fprintf(stderr,
                                "[KGPC_DEBUG_SYMCREAT_PROPS] found property=%s owner=%s read=%s write=%s\n",
                                property_name,
                                current->type_id != NULL ? current->type_id : "<null>",
                                property->read_accessor != NULL ? property->read_accessor : "<null>",
                                property->write_accessor != NULL ? property->write_accessor : "<null>");
                        }

                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                            fprintf(stderr, "[SemCheck]   MATCHED property '%s'!\n", property->name);
                        }
                        if (owner_out != NULL)
                            *owner_out = current;
                        return property;
                    }
                }
                node = node->next;
            }
            if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck]   Searched %d properties (pass %d) in this record, no match\n", prop_count, pass);
            }
        }
        current = semcheck_lookup_parent_record(symtab, current);
    }
    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
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
        if (kgpc_getenv("KGPC_DEBUG_SYMCREAT_FIELDS") != NULL &&
            field_name != NULL &&
            (pascal_identifier_equals(field_name, "deflist") ||
             pascal_identifier_equals(field_name, "forwarddef") ||
             pascal_identifier_equals(field_name, "parentfpstruct") ||
             pascal_identifier_equals(field_name, "realname")))
        {
            fprintf(stderr,
                "[KGPC_DEBUG_SYMCREAT_FIELDS] lookup field=%s on class=%s parent=%s\n",
                field_name,
                current->type_id != NULL ? current->type_id : "<null>",
                current->parent_class_name != NULL ? current->parent_class_name : "<null>");
        }
        struct RecordField *field = NULL;
        field = semcheck_find_field_in_members(current->fields, field_name, include_hidden);
        if (field != NULL)
        {
            if (kgpc_getenv("KGPC_DEBUG_SYMCREAT_FIELDS") != NULL &&
                field_name != NULL &&
                (pascal_identifier_equals(field_name, "deflist") ||
                 pascal_identifier_equals(field_name, "forwarddef") ||
                 pascal_identifier_equals(field_name, "parentfpstruct") ||
                 pascal_identifier_equals(field_name, "realname")))
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_SYMCREAT_FIELDS] found field=%s owner=%s\n",
                    field_name,
                    current->type_id != NULL ? current->type_id : "<null>");
            }
            if (owner_out != NULL)
                *owner_out = current;
            return field;
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
            /* Use the type_id as-is first. If that fails and the type has a
             * different canonical name in the hash table (e.g. record has
             * "timezone" but the registered class is "TTimeZone"), retry
             * with the hash table node's id. */
            const char *class_id = current->type_id;
            snprintf(mangled_name, sizeof(mangled_name), "%s__%s", class_id, method_name);

            if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] semcheck_find_class_method: Searching for '%s' (mangled: '%s') in class '%s'\n",
                    method_name, mangled_name, class_id);
            }

            HashNode_t *method_node = NULL;
            if (!FindSymbol(&method_node, symtab, mangled_name) || method_node == NULL)
            {
                /* If not found, try looking up the class type to get its registered name */
                HashNode_t *class_type_node = NULL;
                if (FindSymbol(&class_type_node, symtab, class_id) &&
                    class_type_node != NULL && class_type_node->id != NULL &&
                    !pascal_identifier_equals(class_type_node->id, class_id))
                {
                    snprintf(mangled_name, sizeof(mangled_name), "%s__%s",
                        class_type_node->id, method_name);
                    FindSymbol(&method_node, symtab, mangled_name);
                }
            }

            if (method_node != NULL)
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

                if (method_node->hash_type != HASHTYPE_FUNCTION &&
                    method_node->hash_type != HASHTYPE_PROCEDURE &&
                    method_node->hash_type != HASHTYPE_BUILTIN_PROCEDURE)
                {
                    method_node = NULL;
                }

                if (method_node == NULL)
                    goto next_class;

                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                    fprintf(stderr, "[SemCheck] semcheck_find_class_method: Found '%s' in class '%s'\n", 
                        method_name, current->type_id);
                }
                if (owner_out != NULL)
                    *owner_out = current;
                return method_node;
            }
        }
    next_class:
        /* For type helpers, walk up the helper parent chain */
        if (current->is_type_helper && current->helper_parent_id != NULL)
        {
            HashNode_t *parent_node = NULL;
            if (FindSymbol(&parent_node, symtab, current->helper_parent_id) != 0 && parent_node != NULL)
            {
                struct RecordType *parent_helper = get_record_type_from_node(parent_node);
                if (parent_helper != NULL && parent_helper->is_type_helper)
                {
                    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
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
