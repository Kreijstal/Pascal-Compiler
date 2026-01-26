/*
    SizeOf and field resolution utilities for semantic checking.

    This module contains functions for computing the size of types,
    resolving record fields and their offsets.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif

#include "SemCheck_sizeof.h"
#include "../SemCheck.h"
#include "../HashTable/HashTable.h"
#include "../SymTab/SymTab.h"
#include "../../List/List.h"
#include "../../ParseTree/tree_types.h"
#include "../../ParseTree/type_tags.h"
#include "../../ParseTree/KgpcType.h"
#include "../../../identifier_utils.h"

/* Forward declarations for internal functions */
static int sizeof_from_record_members(SymTab_t *symtab, ListNode_t *members,
    long long *size_out, int depth, int line_num);
static int sizeof_from_variant_part(SymTab_t *symtab, struct VariantPart *variant,
    long long *size_out, int depth, int line_num);
static int find_field_in_members(SymTab_t *symtab, ListNode_t *members,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int depth, int line_num, int *found);
static int find_field_in_variant(SymTab_t *symtab, struct VariantPart *variant,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int depth, int line_num, int *found);
static int compute_field_size(SymTab_t *symtab, struct RecordField *field,
    long long *size_out, int depth, int line_num);
static long long align_offset(long long offset, int alignment);
static int get_field_alignment(SymTab_t *symtab, struct RecordField *field, int depth, int line_num);

/* Helper function to check if a node is a record type */
static inline int node_is_record_type(HashNode_t *node)
{
    return hashnode_is_record(node);
}

/* Helper to get record type from node */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
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

/* Helper to get type alias from node */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    return hashnode_get_type_alias(node);
}

HashNode_t *semcheck_find_preferred_type_node(SymTab_t *symtab, const char *type_id)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, (char *)type_id);
    if (matches == NULL)
    {
        /* Try stripping unit prefix from qualified name like "baseunix.stat" */
        const char *dot = strrchr(type_id, '.');
        if (dot != NULL && dot[1] != '\0')
            matches = FindAllIdents(symtab, (char *)(dot + 1));
    }
    HashNode_t *best = NULL;
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            if (best == NULL)
                best = node;
            else if (best->defined_in_unit && !node->defined_in_unit)
                best = node;
        }
        cur = cur->next;
    }
    if (matches != NULL)
        DestroyList(matches);
    return best;
}

long long sizeof_from_type_tag(int type_tag)
{
    switch(type_tag)
    {
        case INT_TYPE:
            return 4;
        case LONGINT_TYPE:
            return 4;
        case REAL_TYPE:
            return 8;
        case STRING_TYPE:
            return POINTER_SIZE_BYTES;
        case CHAR_TYPE:
            return 1;
        case BOOL:
            /*
             * Standalone booleans occupy 4 bytes to keep stack accesses aligned,
             * but packed arrays of boolean elements are emitted as 1 byte each.
             * Document the distinction so sizeof(boolean) users are not surprised.
             */
            return 4;
        case POINTER_TYPE:
            return POINTER_SIZE_BYTES;
        case SET_TYPE:
        case ENUM_TYPE:
            return 4;
        case FILE_TYPE:
        case TEXT_TYPE:
            return POINTER_SIZE_BYTES;
        case PROCEDURE:
            return POINTER_SIZE_BYTES;
        default:
            return -1;
    }
}

int sizeof_from_type_ref(SymTab_t *symtab, int type_tag,
    const char *type_id, long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        semcheck_error_with_context("Error on line %d, SizeOf exceeded recursion depth while resolving type.\n",
            line_num);
        return 1;
    }

    if (type_id != NULL)
    {
        HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_id);
        if (type_node != NULL && type_node->type != NULL)
        {
            long long size = kgpc_type_sizeof(type_node->type);
            if (size > 0)
            {
                *size_out = size;
                return 0;
            }
        }
    }

    if (type_tag != UNKNOWN_TYPE)
    {
        long long base_size = sizeof_from_type_tag(type_tag);
        if (base_size >= 0)
        {
            *size_out = base_size;
            return 0;
        }
    }

    if (type_id != NULL)
    {
        HashNode_t *target_node = semcheck_find_preferred_type_node(symtab, type_id);
        if (target_node == NULL)
        {
            /* For generic type parameters or nested types that haven't been resolved yet,
             * treat as pointer-sized rather than hard error - this allows:
             * - generic templates to be processed without full instantiation
             * - FPC bootstrap where nested types (Public type ...) aren't supported yet
             * Nested procedural types and class references are typically pointer-sized */
            const char *debug_env = getenv("KGPC_DEBUG_TFPG");
            if (debug_env != NULL)
            {
                fprintf(stderr, "[KGPC] SizeOf: unknown type %s at line %d (may be unresolved generic parameter or nested type)\n",
                    type_id, line_num);
            }
            *size_out = POINTER_SIZE_BYTES;  /* Assume pointer-sized for unknown types */
            return 0;  /* Return success to allow processing to continue */
        }
        return sizeof_from_hashnode(symtab, target_node, size_out, depth + 1, line_num);
    }

    /* If both type_tag and type_id are unspecified, we can't compute size.
     * This might happen for generic type parameters - don't print error, just fail gracefully */
    const char *debug_env = getenv("KGPC_DEBUG_TFPG");
    if (debug_env != NULL)
    {
        fprintf(stderr, "[KGPC] SizeOf: unable to resolve type at line %d (unspecified type_tag and type_id)\n",
            line_num);
    }
    *size_out = 0;
    return 1;  /* Return error - caller should check if this is expected */
}

int sizeof_from_record(SymTab_t *symtab, struct RecordType *record,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (record == NULL)
    {
        *size_out = 0;
        return 0;
    }

    if (record->has_cached_size)
    {
        *size_out = record->cached_size;
        return 0;
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        semcheck_error_with_context("Error on line %d, SizeOf exceeded recursion depth while resolving record type.\n",
            line_num);
        return 1;
    }

    long long computed_size = 0;

    /* Classes have a VMT pointer at offset 0 */
    if (record->is_class)
        computed_size = 8; /* 64-bit pointer */

    if (sizeof_from_record_members(symtab, record->fields, &computed_size, depth + 1, line_num) != 0)
        return 1;

    record->cached_size = computed_size;
    record->has_cached_size = 1;
    *size_out = computed_size;
    return 0;
}

/* Align offset to the specified alignment boundary */
static long long align_offset(long long offset, int alignment)
{
    if (alignment <= 0)
        return offset;

    long long remainder = offset % alignment;
    if (remainder == 0)
        return offset;

    return offset + (alignment - remainder);
}

/* Get alignment requirement for a field type (64-bit ABI) */
static int get_field_alignment(SymTab_t *symtab, struct RecordField *field, int depth, int line_num)
{
    if (field == NULL)
        return 1;  /* Minimum alignment */

    /* Prevent infinite recursion */
    if (depth > SIZEOF_RECURSION_LIMIT)
        return 1;

    /* Dynamic arrays are descriptors (pointer + int64), aligned to 8 */
    if (field->is_array && field->array_is_open)
        return POINTER_SIZE_BYTES;

    /*
     * Compute a natural alignment based on the underlying element size.
     * We clamp the result to pointer size (8 on this target) but never
     * return less than 1 so that small scalar fields (Byte/Word) can sit
     * back-to-back without being promoted to 4-byte boundaries.
     */
    long long elem_size = 0;
    int status = 0;

    if (field->is_array)
    {
        /* Static arrays align to their element type, not total size */
        if (field->array_element_type == RECORD_TYPE && field->nested_record != NULL)
            status = sizeof_from_record(symtab, field->nested_record, &elem_size, depth + 1, line_num);
        else if (field->array_element_type != UNKNOWN_TYPE || field->array_element_type_id != NULL)
            status = sizeof_from_type_ref(symtab, field->array_element_type, field->array_element_type_id,
                &elem_size, depth + 1, line_num);
        else
            status = sizeof_from_type_ref(symtab, field->type, field->type_id, &elem_size, depth + 1, line_num);
    }
    else if (field->nested_record != NULL)
    {
        status = sizeof_from_record(symtab, field->nested_record, &elem_size, depth + 1, line_num);
    }
    else
    {
        status = sizeof_from_type_ref(symtab, field->type, field->type_id, &elem_size, depth + 1, line_num);
    }

    if (status != 0 || elem_size <= 0)
        return 1; /* fallback to minimal alignment */

    if (elem_size > POINTER_SIZE_BYTES)
        return POINTER_SIZE_BYTES;
    return (int)elem_size;
}

static int compute_field_size(SymTab_t *symtab, struct RecordField *field,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (field == NULL)
    {
        *size_out = 0;
        return 0;
    }

    const char *debug_env = getenv("KGPC_DEBUG_TFPG");
    if (debug_env != NULL && field->name != NULL) {
        fprintf(stderr, "[KGPC] compute_field_size: field=%s type=%d type_id=%s is_array=%d\n",
            field->name, field->type, field->type_id ? field->type_id : "<null>", field->is_array);
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        semcheck_error_with_context("Error on line %d, SizeOf exceeded recursion depth while resolving record field.\n",
            line_num);
        return 1;
    }

    if (field->is_array)
    {
        const char *debug_env = getenv("KGPC_DEBUG_TFPG");
        if (debug_env != NULL && field->name != NULL) {
            fprintf(stderr, "[KGPC] compute_field_size array: field=%s is_open=%d start=%d end=%d\n",
                field->name, field->array_is_open, field->array_start, field->array_end);
        }

        if (field->array_is_open || field->array_end < field->array_start)
        {
            /* Dynamic arrays are descriptors {void *data, int64_t length}, so 16 bytes */
            *size_out = POINTER_SIZE_BYTES * 2;
            return 0;
        }

        long long element_size = 0;
        int elem_status = 1;
        if (field->array_element_type == RECORD_TYPE && field->nested_record != NULL)
            elem_status = sizeof_from_record(symtab, field->nested_record,
                &element_size, depth + 1, line_num);
        else if (field->array_element_type != UNKNOWN_TYPE ||
            field->array_element_type_id != NULL)
            elem_status = sizeof_from_type_ref(symtab, field->array_element_type,
                field->array_element_type_id, &element_size, depth + 1, line_num);
        else
            elem_status = sizeof_from_type_ref(symtab, field->type, field->type_id,
                &element_size, depth + 1, line_num);

        if (elem_status != 0)
            return 1;

        long long count = (long long)field->array_end - (long long)field->array_start + 1;
        if (count < 0)
        {
            semcheck_error_with_context("Error on line %d, invalid bounds for array field %s.\n",
                line_num, field->name != NULL ? field->name : "");
            return 1;
        }

        *size_out = element_size * count;
        return 0;
    }

    if (field->nested_record != NULL)
        return sizeof_from_record(symtab, field->nested_record, size_out, depth + 1, line_num);

    return sizeof_from_type_ref(symtab, field->type, field->type_id, size_out, depth + 1, line_num);
}

static int sizeof_from_record_members(SymTab_t *symtab, ListNode_t *members,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    long long total = *size_out;
    int max_alignment = 1;  /* Track maximum alignment for struct padding */

    ListNode_t *cur = members;
    while (cur != NULL)
    {
        if (cur->type == LIST_RECORD_FIELD)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;

            /* Get field alignment and align current offset */
            int field_alignment = get_field_alignment(symtab, field, depth + 1, line_num);
            if (field_alignment > max_alignment)
                max_alignment = field_alignment;

            total = align_offset(total, field_alignment);

            /* Add field size */
            long long field_size = 0;
            int field_result = compute_field_size(symtab, field, &field_size, depth + 1, line_num);
            if (field_result != 0)
            {
#ifdef DEBUG
                fprintf(stderr, "DEBUG: compute_field_size FAILED for field %s: result=%d\n",
                    field->name ? field->name : "<null>", field_result);
#endif
                return 1;
            }
            total += field_size;
        }
        else if (cur->type == LIST_VARIANT_PART)
        {
            struct VariantPart *variant = (struct VariantPart *)cur->cur;
            long long variant_size = 0;
            if (sizeof_from_variant_part(symtab, variant, &variant_size, depth + 1, line_num) != 0)
                return 1;
            total += variant_size;
        }
        cur = cur->next;
    }

    /* Align total size to the maximum alignment (struct padding at end) */
    total = align_offset(total, max_alignment);

    *size_out = total;
    return 0;
}

static int sizeof_from_variant_part(SymTab_t *symtab, struct VariantPart *variant,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (variant == NULL)
    {
        *size_out = 0;
        return 0;
    }

    if (variant->has_cached_size)
    {
        *size_out = variant->cached_size;
        return 0;
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        semcheck_error_with_context("Error on line %d, SizeOf exceeded recursion depth while resolving variant part.\n",
            line_num);
        return 1;
    }

    long long max_size = 0;
    ListNode_t *cur = variant->branches;
    while (cur != NULL)
    {
        if (cur->type == LIST_VARIANT_BRANCH)
        {
            struct VariantBranch *branch = (struct VariantBranch *)cur->cur;
            long long branch_size = 0;
            if (sizeof_from_record_members(symtab, branch->members, &branch_size,
                    depth + 1, line_num) != 0)
                return 1;
            if (branch_size > max_size)
                max_size = branch_size;
        }
        cur = cur->next;
    }

    variant->cached_size = max_size;
    variant->has_cached_size = 1;
    *size_out = max_size;
    return 0;
}

static int find_field_in_variant(SymTab_t *symtab, struct VariantPart *variant,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int depth, int line_num, int *found)
{
    if (found != NULL)
        *found = 0;

    if (variant == NULL || field_name == NULL)
        return 0;

    ListNode_t *cur = variant->branches;
    while (cur != NULL)
    {
        if (cur->type == LIST_VARIANT_BRANCH)
        {
            struct VariantBranch *branch = (struct VariantBranch *)cur->cur;
            long long branch_offset = 0;
            int branch_found = 0;
            if (find_field_in_members(symtab, branch->members, field_name, out_field,
                    &branch_offset, depth + 1, line_num, &branch_found) != 0)
                return 1;
            if (branch_found)
            {
                if (offset_out != NULL)
                    *offset_out = branch_offset;
                if (found != NULL)
                    *found = 1;
                return 0;
            }
        }
        cur = cur->next;
    }

    return 0;
}

static int find_field_in_members(SymTab_t *symtab, ListNode_t *members,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int depth, int line_num, int *found)
{
    if (found != NULL)
        *found = 0;

    long long offset = 0;
    ListNode_t *cur = members;
    while (cur != NULL)
    {
        if (cur->type == LIST_RECORD_FIELD)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            if (field != NULL && !record_field_is_hidden(field))
            {
                /* Align offset to field's alignment requirement */
                int field_alignment = get_field_alignment(symtab, field, depth + 1, line_num);
                offset = align_offset(offset, field_alignment);

                /* Check if this is the target field */
                if (field->name != NULL &&
                    pascal_identifier_equals(field->name, field_name))
                {
                    if (out_field != NULL)
                        *out_field = field;
                    if (offset_out != NULL)
                        *offset_out = offset;
                    if (found != NULL)
                        *found = 1;
                    return 0;
                }
            }

            long long field_size = 0;
            if (compute_field_size(symtab, field, &field_size, depth + 1, line_num) != 0)
                return 1;
            offset += field_size;
        }
        else if (cur->type == LIST_VARIANT_PART)
        {
            struct VariantPart *variant = (struct VariantPart *)cur->cur;
            long long variant_field_offset = 0;
            int variant_found = 0;
            if (find_field_in_variant(symtab, variant, field_name, out_field,
                    &variant_field_offset, depth + 1, line_num, &variant_found) != 0)
                return 1;
            if (variant_found)
            {
                if (offset_out != NULL)
                    *offset_out = offset + variant_field_offset;
                if (found != NULL)
                    *found = 1;
                return 0;
            }

            long long variant_size = 0;
            if (sizeof_from_variant_part(symtab, variant, &variant_size, depth + 1, line_num) != 0)
                return 1;
            offset += variant_size;
        }
        cur = cur->next;
    }

    if (offset_out != NULL)
        *offset_out = offset;
    return 0;
}

int sizeof_from_alias(SymTab_t *symtab, struct TypeAlias *alias,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (alias == NULL)
    {
        semcheck_error_with_context("Error on line %d, SizeOf encountered incomplete type alias.\n",
            line_num);
        return 1;
    }

    if (alias->storage_size > 0 && !alias->is_array && !alias->is_set &&
        !alias->is_enum && !alias->is_file)
    {
        *size_out = alias->storage_size;
        return 0;
    }

    if (alias->is_pointer)
    {
        *size_out = POINTER_SIZE_BYTES;
        return 0;
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        semcheck_error_with_context("Error on line %d, SizeOf exceeded recursion depth while resolving type alias.\n",
            line_num);
        return 1;
    }

    if (alias->is_array)
    {
        if (alias->is_open_array || alias->array_end < alias->array_start)
        {
            semcheck_error_with_context("Error on line %d, SizeOf cannot determine size of open array type.\n",
                line_num);
            return 1;
        }

        long long element_size = 0;
        if (sizeof_from_type_ref(symtab, alias->array_element_type,
                alias->array_element_type_id, &element_size, depth + 1, line_num) != 0)
            return 1;

        long long count = (long long)alias->array_end - (long long)alias->array_start + 1;
        if (count < 0)
        {
            semcheck_error_with_context("Error on line %d, invalid bounds for array type in SizeOf.\n",
                line_num);
            return 1;
        }

        *size_out = element_size * count;
        return 0;
    }

    if (alias->base_type != UNKNOWN_TYPE)
    {
        return sizeof_from_type_ref(symtab, alias->base_type, NULL,
            size_out, depth + 1, line_num);
    }

    if (alias->target_type_id != NULL)
        return sizeof_from_type_ref(symtab, UNKNOWN_TYPE, alias->target_type_id,
            size_out, depth + 1, line_num);

    semcheck_error_with_context("Error on line %d, SizeOf encountered unresolved type alias.\n",
        line_num);
    return 1;
}

int sizeof_from_hashnode(SymTab_t *symtab, HashNode_t *node,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (node == NULL)
    {
        semcheck_error_with_context("Error on line %d, SizeOf encountered null symbol information.\n",
            line_num);
        return 1;
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        semcheck_error_with_context("Error on line %d, SizeOf exceeded recursion depth while resolving symbol.\n",
            line_num);
        return 1;
    }

    /* PREFERRED PATH: Try using KgpcType directly if available */
    if (node->type != NULL)
    {
        long long size = kgpc_type_sizeof(node->type);
        if (size > 0)
        {
            *size_out = size;
            return 0;
        }
        else if (size == 0)
        {
            /* Zero-sized type (e.g., empty record or zero-length array) */
            *size_out = 0;
            return 0;
        }
        /* else size < 0: kgpc_type_sizeof couldn't determine size, fall through to legacy path */
    }

    /* LEGACY PATH: KgpcType not available or couldn't determine size */

    if (node->hash_type == HASHTYPE_TYPE)
    {
        struct RecordType *record = get_record_type_from_node(node);
        if (record != NULL)
            return sizeof_from_record(symtab, record, size_out, depth + 1, line_num);

        struct TypeAlias *alias = get_type_alias_from_node(node);
        if (alias != NULL)
        {
            return sizeof_from_alias(symtab, alias, size_out, depth + 1, line_num);
        }

    }

    /* For array size calculation */
    int is_array = hashnode_is_array(node);

    if (is_array)
    {
        int is_dynamic = hashnode_is_dynamic_array(node);

        if (is_dynamic)
        {
            semcheck_error_with_context("Error on line %d, SizeOf cannot determine size of dynamic array %s.\n",
                line_num, node->id);
            return 1;
        }

        /* Get element size */
        long long element_size = hashnode_get_element_size(node);
        if (element_size <= 0)
        {
            semcheck_error_with_context("Error on line %d, cannot determine element size for array %s.\n",
                line_num, node->id);
            return 1;
        }

        /* Get array bounds */
        int array_start, array_end;
        hashnode_get_array_bounds(node, &array_start, &array_end);

        long long count = (long long)array_end - (long long)array_start + 1;
        if (count < 0)
        {
            semcheck_error_with_context("Error on line %d, invalid bounds for array %s in SizeOf.\n",
                line_num, node->id);
            return 1;
        }

        *size_out = element_size * count;
        return 0;
    }

    if (node_is_record_type(node))
    {
        struct RecordType *record = get_record_type_from_node(node);
        if (record != NULL)
            return sizeof_from_record(symtab, record, size_out, depth + 1, line_num);
    }

    struct TypeAlias *alias = get_type_alias_from_node(node);
    if (alias != NULL)
        return sizeof_from_alias(symtab, alias, size_out, depth + 1, line_num);

    semcheck_error_with_context("Error on line %d, SizeOf cannot determine size of %s.\n",
        line_num, node->id != NULL ? node->id : "symbol");
    return 1;
}

int resolve_record_field(SymTab_t *symtab, struct RecordType *record,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int line_num, int silent)
{
    if (record == NULL || field_name == NULL)
        return 1;

    long long offset = 0;
    int found = 0;
    if (find_field_in_members(symtab, record->fields, field_name, out_field,
            &offset, 0, line_num, &found) != 0)
        return 1;

    if (!found)
    {
        if (!silent)
            semcheck_error_with_context("Error on line %d, record field %s not found.\n", line_num, field_name);
        return 1;
    }

    if (offset_out != NULL)
        *offset_out = offset;
    return 0;
}

int semcheck_compute_record_size(SymTab_t *symtab, struct RecordType *record,
    long long *size_out, int line_num)
{
    return sizeof_from_record(symtab, record, size_out, 0, line_num);
}
