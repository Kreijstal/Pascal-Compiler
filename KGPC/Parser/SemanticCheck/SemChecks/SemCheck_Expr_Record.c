/*
    SemCheck_Expr_Record.c - Record/class lookup helpers

    This file contains helpers for resolving record/class metadata
    and walking inheritance for fields, methods, and properties.

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"

static const int SEMCHECK_MAX_HIERARCHY_DEPTH = 100;

typedef struct SemcheckPtrSet {
    void **slots;
    size_t capacity;
    size_t count;
} SemcheckPtrSet;

static size_t semcheck_ptrset_hash_ptr(const void *ptr)
{
    uintptr_t value = (uintptr_t)ptr;
    value >>= 4;
    value ^= value >> 33;
    value *= 0xff51afd7ed558ccdULL;
    value ^= value >> 33;
    return (size_t)value;
}

static int semcheck_ptrset_reserve(SemcheckPtrSet *set, size_t min_capacity)
{
    if (set == NULL)
        return 0;
    if (set->capacity >= min_capacity)
        return 1;

    size_t new_capacity = set->capacity > 0 ? set->capacity : 16;
    while (new_capacity < min_capacity)
        new_capacity <<= 1;

    void **new_slots = (void **)calloc(new_capacity, sizeof(void *));
    if (new_slots == NULL)
        return 0;

    if (set->slots != NULL)
    {
        for (size_t i = 0; i < set->capacity; ++i)
        {
            void *entry = set->slots[i];
            if (entry == NULL)
                continue;
            size_t idx = semcheck_ptrset_hash_ptr(entry) & (new_capacity - 1);
            while (new_slots[idx] != NULL)
                idx = (idx + 1) & (new_capacity - 1);
            new_slots[idx] = entry;
        }
        free(set->slots);
    }

    set->slots = new_slots;
    set->capacity = new_capacity;
    return 1;
}

static int semcheck_ptrset_insert(SemcheckPtrSet *set, void *ptr)
{
    if (set == NULL || ptr == NULL)
        return 0;

    if ((set->count + 1) * 4 >= set->capacity * 3)
    {
        size_t target_capacity = set->capacity > 0 ? set->capacity << 1 : 16;
        if (!semcheck_ptrset_reserve(set, target_capacity))
            return 0;
    }
    else if (set->capacity == 0)
    {
        if (!semcheck_ptrset_reserve(set, 16))
            return 0;
    }

    size_t idx = semcheck_ptrset_hash_ptr(ptr) & (set->capacity - 1);
    while (set->slots[idx] != NULL)
    {
        if (set->slots[idx] == ptr)
            return 0;
        idx = (idx + 1) & (set->capacity - 1);
    }

    set->slots[idx] = ptr;
    set->count++;
    return 1;
}

static int semcheck_ptrset_contains(const SemcheckPtrSet *set, const void *ptr)
{
    if (set == NULL || ptr == NULL || set->capacity == 0 || set->slots == NULL)
        return 0;

    size_t idx = semcheck_ptrset_hash_ptr(ptr) & (set->capacity - 1);
    while (set->slots[idx] != NULL)
    {
        if (set->slots[idx] == ptr)
            return 1;
        idx = (idx + 1) & (set->capacity - 1);
    }
    return 0;
}

static void semcheck_ptrset_destroy(SemcheckPtrSet *set)
{
    if (set == NULL)
        return;
    free(set->slots);
    set->slots = NULL;
    set->capacity = 0;
    set->count = 0;
}

static void semcheck_merge_candidate_lists_dedup_seen(ListNode_t **combined_head,
    ListNode_t **combined_tail, ListNode_t *new_candidates, SemcheckPtrSet *seen)
{
    ListNode_t *cur = new_candidates;
    while (cur != NULL)
    {
        ListNode_t *next = cur->next;
        cur->next = NULL;

        if (cur->cur != NULL && semcheck_ptrset_insert(seen, cur->cur))
        {
            if (*combined_tail == NULL)
            {
                *combined_head = cur;
                *combined_tail = cur;
            }
            else
            {
                (*combined_tail)->next = cur;
                *combined_tail = cur;
            }
        }
        else
        {
            free(cur);
        }

        cur = next;
    }
}

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

    /* Fallback: if dep_scope filtering hid the parent type, use FindSymbol
     * which walks the full scope tree. Parent classes must always be
     * reachable since the child class itself was accessible. */
    if (parent_node == NULL)
    {
        HashNode_t *fallback = NULL;
        if (FindSymbol(&fallback, symtab, record_info->parent_class_name) != 0 &&
            fallback != NULL && fallback->hash_type == HASHTYPE_TYPE)
        {
            parent_node = fallback;
        }
    }

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

            /* Fallback: try FindAllIdents which searches all reachable scopes. */
            if (method_node == NULL)
            {
                ListNode_t *all = FindAllIdents(symtab, mangled_name);
                if (all != NULL)
                {
                    ListNode_t *cur = all;
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
                    DestroyList(all);
                }
            }

            /* Class methods (including private getters/setters) must be
             * reachable when we already have access to the class type.
             * Search all unit scopes directly — the method may live in
             * a unit that is not a direct/transitive dependency but
             * whose class type was exposed through re-exports. */
            if (method_node == NULL)
            {
                for (int u = 0; u < SYMTAB_MAX_UNITS; u++)
                {
                    if (symtab->unit_scopes[u] == NULL)
                        continue;
                    HashNode_t *found = FindIdentInTable(
                        symtab->unit_scopes[u]->table, mangled_name);
                    if (found != NULL &&
                        (found->hash_type == HASHTYPE_FUNCTION ||
                         found->hash_type == HASHTYPE_PROCEDURE))
                    {
                        method_node = found;
                        break;
                    }
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

/* Merge new_candidates into *existing by pointer-identity dedup.
 * Nodes from new_candidates that are already in *existing (same HashNode_t*)
 * are freed; unique nodes are appended to *existing.
 * After the call, new_candidates is consumed and must not be used. */
void semcheck_merge_candidate_lists_dedup(ListNode_t **existing,
    ListNode_t *new_candidates)
{
    if (new_candidates == NULL)
        return;
    if (existing == NULL)
    {
        ListNode_t *cur = new_candidates;
        while (cur != NULL)
        {
            ListNode_t *next = cur->next;
            free(cur);
            cur = next;
        }
        return;
    }

    SemcheckPtrSet seen = {0};
    ListNode_t *tail = NULL;

    for (ListNode_t *cur = *existing; cur != NULL; cur = cur->next)
    {
        tail = cur;
        if (cur->cur != NULL)
            KGPC_SEMCHECK_HARD_ASSERT(
                semcheck_ptrset_contains(&seen, cur->cur) ||
                semcheck_ptrset_insert(&seen, cur->cur),
                "pointer set insert failed while indexing overload candidates");
    }

    semcheck_merge_candidate_lists_dedup_seen(existing, &tail, new_candidates, &seen);
    semcheck_ptrset_destroy(&seen);
}

/* Collect all method overloads across the full class hierarchy.
 * For each class from start_record up through all parent classes,
 * look up "ClassName__method_name" via FindAllIdents and merge
 * the results into a single list. Duplicate HashNode_t pointers
 * are skipped so that a method registered at several scope levels
 * is only included once. */
ListNode_t *semcheck_collect_hierarchy_method_overloads(SymTab_t *symtab,
    struct RecordType *start_record, const char *method_name)
{
    if (symtab == NULL || start_record == NULL || method_name == NULL)
        return NULL;

    ListNode_t *combined = NULL;
    ListNode_t *combined_tail = NULL;
    SemcheckPtrSet seen = {0};
    SemcheckPtrSet visited_records = {0};
    struct RecordType *current = start_record;
    int iterations = 0;

    while (current != NULL)
    {
        iterations++;
        KGPC_SEMCHECK_HARD_ASSERT(iterations <= SEMCHECK_MAX_HIERARCHY_DEPTH,
            "record hierarchy traversal exceeded %d while collecting overloads for %s",
            SEMCHECK_MAX_HIERARCHY_DEPTH, method_name != NULL ? method_name : "<unknown>");
        KGPC_SEMCHECK_HARD_ASSERT(semcheck_ptrset_insert(&visited_records, current),
            "cycle detected while collecting overloads for %s (record=%s)",
            method_name != NULL ? method_name : "<unknown>",
            current->type_id != NULL ? current->type_id : "<anonymous>");
        if (current->type_id != NULL)
        {
            char mangled[256];
            snprintf(mangled, sizeof(mangled), "%s__%s",
                current->type_id, method_name);

            ListNode_t *class_overloads = FindAllIdents(symtab, mangled);
            semcheck_merge_candidate_lists_dedup_seen(&combined, &combined_tail,
                class_overloads, &seen);
        }

        /* Walk type helper parent chain first, then class parent chain */
        if (current->is_type_helper && current->helper_parent_id != NULL)
        {
            HashNode_t *parent_node = NULL;
            if (FindSymbol(&parent_node, symtab, current->helper_parent_id) != 0 &&
                parent_node != NULL)
            {
                struct RecordType *parent_helper = get_record_type_from_node(parent_node);
                if (parent_helper != NULL && parent_helper->is_type_helper)
                {
                    current = parent_helper;
                    continue;
                }
            }
        }

        current = semcheck_lookup_parent_record(symtab, current);
    }

    semcheck_ptrset_destroy(&visited_records);
    semcheck_ptrset_destroy(&seen);
    return combined;
}
