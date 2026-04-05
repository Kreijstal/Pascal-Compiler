#ifndef CODEGEN_RECORD_HELPERS_H
#define CODEGEN_RECORD_HELPERS_H

/**
 * Shared helper functions for record and class field lookups.
 *
 * These functions consolidate common patterns for finding fields in records,
 * classes, and variant record types across the code generator modules.
 */

#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../identifier_utils.h"

/**
 * Check if a record field should be hidden from lookup.
 * Hidden fields are typically internal compiler-generated fields.
 */
static inline int record_field_is_hidden(const struct RecordField *field)
{
    /* Fields starting with $ are compiler-generated and hidden */
    return (field != NULL && field->name != NULL && field->name[0] == '$');
}

/**
 * Look up a record field by name in a list of field members.
 * Handles both regular fields and fields within variant record parts.
 * Uses case-insensitive Pascal identifier comparison.
 *
 * @param members List of record members (fields and variant parts)
 * @param field_name Name of the field to find
 * @param skip_hidden If non-zero, skip compiler-generated hidden fields
 * @return Pointer to the RecordField if found, NULL otherwise
 */
static inline struct RecordField *codegen_lookup_field_in_members(ListNode_t *members,
    const char *field_name, int skip_hidden)
{
    if (field_name == NULL)
        return NULL;

    for (ListNode_t *cur = members; cur != NULL; cur = cur->next)
    {
        if (cur->type == LIST_RECORD_FIELD && cur->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            if (skip_hidden && record_field_is_hidden(field))
                continue;
            if (field->name != NULL && pascal_identifier_equals(field->name, field_name))
                return field;
        }
        else if (cur->type == LIST_VARIANT_PART && cur->cur != NULL)
        {
            /* Recursively search variant record branches */
            struct VariantPart *variant = (struct VariantPart *)cur->cur;
            for (ListNode_t *branch_node = variant->branches; branch_node != NULL;
                 branch_node = branch_node->next)
            {
                if (branch_node->type != LIST_VARIANT_BRANCH || branch_node->cur == NULL)
                    continue;
                struct VariantBranch *branch = (struct VariantBranch *)branch_node->cur;
                struct RecordField *field =
                    codegen_lookup_field_in_members(branch->members, field_name, skip_hidden);
                if (field != NULL)
                    return field;
            }
        }
    }

    return NULL;
}

/**
 * Look up a record field by name in a RecordType.
 * Uses case-insensitive Pascal identifier comparison.
 *
 * @param record The record type to search
 * @param field_name Name of the field to find
 * @param skip_hidden If non-zero, skip compiler-generated hidden fields
 * @return Pointer to the RecordField if found, NULL otherwise
 */
static inline struct RecordField *codegen_lookup_field_in_record(const struct RecordType *record,
    const char *field_name, int skip_hidden)
{
    if (record == NULL || field_name == NULL)
        return NULL;
    return codegen_lookup_field_in_members(record->fields, field_name, skip_hidden);
}

/**
 * Find a class property by name in a RecordType (representing a class).
 * Uses case-insensitive Pascal identifier comparison.
 *
 * @param record The record/class type to search
 * @param property_name Name of the property to find
 * @return Pointer to the ClassProperty if found, NULL otherwise
 */
static inline struct ClassProperty *codegen_find_property_in_record(const struct RecordType *record,
    const char *property_name)
{
    if (record == NULL || property_name == NULL)
        return NULL;

    for (ListNode_t *cur = record->properties; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_CLASS_PROPERTY || cur->cur == NULL)
            continue;
        struct ClassProperty *prop = (struct ClassProperty *)cur->cur;
        if (prop->name != NULL && pascal_identifier_equals(prop->name, property_name))
            return prop;
    }

    return NULL;
}

#endif /* CODEGEN_RECORD_HELPERS_H */
