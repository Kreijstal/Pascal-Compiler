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
    long long *size_out, int is_packed, int depth, int line_num);
static int sizeof_from_variant_part(SymTab_t *symtab, struct VariantPart *variant,
    long long *size_out, int is_packed, int depth, int line_num);
static int find_field_in_members(SymTab_t *symtab, ListNode_t *members,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    long long start_offset, int is_packed, int depth, int line_num, int *found);
static int find_field_in_variant(SymTab_t *symtab, struct VariantPart *variant,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int is_packed, int depth, int line_num, int *found);
static int compute_field_size(SymTab_t *symtab, struct RecordField *field,
    long long *size_out, int depth, int line_num);
static long long align_offset(long long offset, int alignment);
static int get_field_alignment(SymTab_t *symtab, struct RecordField *field, int depth, int line_num);
static int get_type_alignment_from_ref(SymTab_t *symtab, int type_tag,
    const char *type_id, int *align_out, int depth, int line_num);

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

static int fpc_size_to_alignment(long long size)
{
    if (size > 16)
        return 32;
    if (size > 8)
        return 16;
    if (size > 4)
        return 8;
    if (size > 2)
        return 4;
    if (size > 1)
        return 2;
    return 1;
}

static int fpc_type_alignment_from_size(long long size, int type_tag)
{
    if (type_tag == EXTENDED_TYPE)
        return 16;
    return fpc_size_to_alignment(size);
}

static int list_length(ListNode_t *list)
{
    int count = 0;
    for (ListNode_t *cur = list; cur != NULL; cur = cur->next)
        ++count;
    return count;
}

static long long fpc_default_set_storage_size_for_high(long long high)
{
    if (high < 32)
        return 4;
    if (high < 256)
        return 32;
    return (high + 7) / 8;
}

static long long fpc_set_storage_size_from_alias(SymTab_t *symtab, struct TypeAlias *alias,
    int depth, int line_num)
{
    if (alias == NULL || !alias->is_set)
        return 4;

    if (alias->storage_size > 0)
        return alias->storage_size;

    if (alias->set_element_type == CHAR_TYPE ||
        (alias->set_element_type_id != NULL &&
         (pascal_identifier_equals(alias->set_element_type_id, "Char") ||
          pascal_identifier_equals(alias->set_element_type_id, "AnsiChar"))))
        return 32;

    if (alias->is_enum_set && alias->inline_enum_values != NULL)
    {
        int count = list_length(alias->inline_enum_values);
        if (count > 0)
            return fpc_default_set_storage_size_for_high((long long)count - 1);
    }

    if (alias->set_element_type == BOOL)
        return 4;

    if (alias->set_element_type_id != NULL && symtab != NULL && depth <= SIZEOF_RECURSION_LIMIT)
    {
        HashNode_t *elem_node = semcheck_find_preferred_type_node(symtab, alias->set_element_type_id);
        if (elem_node != NULL)
        {
            struct TypeAlias *elem_alias = get_type_alias_from_node(elem_node);
            if (elem_alias != NULL)
            {
                if (elem_alias->is_enum && elem_alias->enum_literals != NULL)
                {
                    int count = list_length(elem_alias->enum_literals);
                    if (count > 0)
                        return fpc_default_set_storage_size_for_high((long long)count - 1);
                }
                if (elem_alias->range_known)
                    return fpc_default_set_storage_size_for_high(elem_alias->range_end);
            }
        }
    }

    return 4;
}

static long long fpc_enum_storage_size_from_alias(const struct TypeAlias *alias)
{
    if (alias != NULL && alias->storage_size > 0)
        return alias->storage_size;
    if (alias != NULL && alias->enum_literals != NULL)
    {
        int count = list_length(alias->enum_literals);
        if (count <= 0)
            return 4;
        if (count <= 0x100)
            return 1;
        if (count <= 0x10000)
            return 2;
    }
    return 4;
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
        case EXTENDED_TYPE:
            return 10;
        case STRING_TYPE:
            return POINTER_SIZE_BYTES;
        case INT64_TYPE:
            return 8;
        case CHAR_TYPE:
            return 1;
        case BYTE_TYPE:
            return 1;
        case WORD_TYPE:
            return 2;
        case LONGWORD_TYPE:
            return 4;
        case QWORD_TYPE:
            return 8;
        case BOOL:
            return 1;
        case POINTER_TYPE:
            return POINTER_SIZE_BYTES;
        case SET_TYPE:
            return 4;
        case ENUM_TYPE:
            return 4;
        case FILE_TYPE:
            return 368;
        case TEXT_TYPE:
            return 632;
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

    /* For known primitive type tags, use the tag directly to avoid
     * misresolution when type_id aliases differ across units (e.g. FPC
     * system unit maps Integer to SmallInt while the compiler prelude
     * maps it to LongInt). */
    if (type_tag != UNKNOWN_TYPE && type_tag != RECORD_TYPE &&
        type_tag != ENUM_TYPE && type_tag != SET_TYPE)
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
            const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
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
    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
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

    if (record->has_cached_size && !record_type_is_class(record))
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

    /* Set sentinel to break self-referencing cycles (e.g., record with class var
     * fields of its own type).  If we re-enter for the same record, the cached
     * size (0) will be returned, preventing infinite recursion. */
    int was_cached = record->has_cached_size;
    if (!was_cached && !record_type_is_class(record))
    {
        record->has_cached_size = 1;
        record->cached_size = 0;
    }

    long long computed_size = 0;

    if (record->parent_class_name != NULL && !record->parent_fields_merged && symtab != NULL)
    {
        HashNode_t *parent_node = semcheck_find_preferred_type_node(symtab, record->parent_class_name);
        struct RecordType *parent_record = get_record_type_from_node(parent_node);
        if (parent_record != NULL)
        {
            if (sizeof_from_record(symtab, parent_record, &computed_size, depth + 1, line_num) != 0)
            {
                if (!was_cached) record->has_cached_size = 0;
                return 1;
            }
        }
    }

    if (sizeof_from_record_members(symtab, record->fields, &computed_size,
            record->is_packed, depth + 1, line_num) != 0)
    {
        if (!was_cached) record->has_cached_size = 0;
        return 1;
    }

    record->cached_size = computed_size;
    record->has_cached_size = 1;
    *size_out = computed_size;
    if (computed_size > 1000000)
    {
        const char *debug_env = kgpc_getenv("KGPC_DEBUG_SIZE");
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC_SIZE] Large class/record size: %lld for type_id=%s is_class=%d parent=%s\n",
                computed_size, record->type_id ? record->type_id : "<null>",
                record->is_class, record->parent_class_name ? record->parent_class_name : "<none>");
    }
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

static int get_record_alignment(SymTab_t *symtab, struct RecordType *record,
    int depth, int line_num)
{
    if (record == NULL)
        return 1;

    if (record->is_packed)
        return 1;

    if (depth > SIZEOF_RECURSION_LIMIT)
        return 1;

    /* If size is already cached, derive alignment from it to avoid
     * re-walking potentially cyclic type graphs. */
    if (record->has_cached_size)
    {
        int align = fpc_size_to_alignment(record->cached_size);
        if (align > POINTER_SIZE_BYTES)
            align = POINTER_SIZE_BYTES;
        return align;
    }

    int max_alignment = 1;
    if (record->parent_class_name != NULL && !record->parent_fields_merged && symtab != NULL)
    {
        HashNode_t *parent_node = semcheck_find_preferred_type_node(symtab, record->parent_class_name);
        struct RecordType *parent_record = get_record_type_from_node(parent_node);
        if (parent_record != NULL)
        {
            int parent_align = get_record_alignment(symtab, parent_record, depth + 1, line_num);
            if (parent_align > max_alignment)
                max_alignment = parent_align;
        }
    }

    for (ListNode_t *cur = record->fields; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_RECORD_FIELD)
            continue;
        struct RecordField *field = (struct RecordField *)cur->cur;
        /* Class variables are stored as globals, not in the instance layout */
        if (field != NULL && field->is_class_var)
            continue;
        int field_align = get_field_alignment(symtab, field, depth + 1, line_num);
        if (field_align > max_alignment)
            max_alignment = field_align;
    }

    return max_alignment;
}

/* Get alignment requirement for a field type using FPC-style size alignment. */
static int get_field_alignment(SymTab_t *symtab, struct RecordField *field, int depth, int line_num)
{
    if (field == NULL)
        return 1;  /* Minimum alignment */

    /* Prevent infinite recursion */
    if (depth > SIZEOF_RECURSION_LIMIT)
        return 1;

    /* Pointer fields (^Type) are always pointer-sized. */
    if (field->is_pointer)
        return POINTER_SIZE_BYTES;

    /* Dynamic/open arrays are references for layout purposes. */
    if (field->is_array && field->array_is_open)
        return POINTER_SIZE_BYTES;

    if (field->is_array)
    {
        if (field->array_element_type == RECORD_TYPE && field->array_element_record != NULL)
            return get_record_alignment(symtab, field->array_element_record, depth + 1, line_num);

        {
            int align = 1;
            int status = 0;
            if (field->array_element_type != UNKNOWN_TYPE || field->array_element_type_id != NULL)
            {
                status = get_type_alignment_from_ref(symtab, field->array_element_type,
                    field->array_element_type_id, &align, depth + 1, line_num);
            }
            else
            {
                status = get_type_alignment_from_ref(symtab, field->type, field->type_id,
                    &align, depth + 1, line_num);
            }
            if (status != 0 || align <= 0)
                return 1;  /* Unresolvable type — use minimum alignment */
            return align;
        }
    }

    if (field->nested_record != NULL)
        return get_record_alignment(symtab, field->nested_record, depth + 1, line_num);

    if (field->type == RECORD_TYPE && field->type_id == NULL)
        return 1;

    {
        int align = 1;
        int status = get_type_alignment_from_ref(symtab, field->type, field->type_id,
            &align, depth + 1, line_num);
        if (status != 0 || align <= 0)
            return 1;  /* Unresolvable type — use minimum alignment */
        return align;
    }
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

    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
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

    /* Pointer fields (^Type) are always pointer-sized. */
    if (field->is_pointer)
    {
        *size_out = POINTER_SIZE_BYTES;
        return 0;
    }

    if (field->is_array)
    {
        const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
        if (debug_env != NULL && field->name != NULL) {
            fprintf(stderr, "[KGPC] compute_field_size array: field=%s is_open=%d start=%d end=%d\n",
                field->name, field->array_is_open, field->array_start, field->array_end);
        }

        if (field->array_is_open || field->array_end < field->array_start)
        {
            *size_out = POINTER_SIZE_BYTES * 2;
            return 0;
        }

        long long element_size = 0;
        int elem_status = 1;
        if (field->array_element_type == RECORD_TYPE && field->array_element_record != NULL)
            elem_status = sizeof_from_record(symtab, field->array_element_record,
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

    if (field->type == RECORD_TYPE && field->type_id == NULL)
    {
        /* Explicit anonymous placeholder records such as `name: record end;`
         * intentionally carry no payload bytes. */
        *size_out = 0;
        return 0;
    }

    /* Class fields are references (pointers), not inline structs.
     * If the field's type resolves to a class, return pointer size. */
    if (field->type_id != NULL && symtab != NULL)
    {
        HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, field->type_id);
        if (type_node != NULL)
        {
            struct RecordType *record = get_record_type_from_node(type_node);
            if (record != NULL && record_type_is_class(record))
            {
                *size_out = POINTER_SIZE_BYTES;
                return 0;
            }
        }
    }

    return sizeof_from_type_ref(symtab, field->type, field->type_id, size_out, depth + 1, line_num);
}

static int sizeof_from_record_members(SymTab_t *symtab, ListNode_t *members,
    long long *size_out, int is_packed, int depth, int line_num)
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

            /* Class variables are stored as globals, not in the instance layout */
            if (field != NULL && field->is_class_var)
            {
                cur = cur->next;
                continue;
            }

            /* Get field alignment and align current offset */
            int field_alignment = get_field_alignment(symtab, field, depth + 1, line_num);
            if (field_alignment > max_alignment)
                max_alignment = field_alignment;

            if (!is_packed)
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
            if (sizeof_from_variant_part(symtab, variant, &variant_size,
                    is_packed, depth + 1, line_num) != 0)
                return 1;
            total += variant_size;
        }
        cur = cur->next;
    }

    /* Align total size to the maximum alignment (struct padding at end) */
    if (!is_packed)
        total = align_offset(total, max_alignment);

    *size_out = total;
    return 0;
}

static int sizeof_from_variant_part(SymTab_t *symtab, struct VariantPart *variant,
    long long *size_out, int is_packed, int depth, int line_num)
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
                    is_packed, depth + 1, line_num) != 0)
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
    int is_packed, int depth, int line_num, int *found)
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
                    &branch_offset, 0, is_packed, depth + 1, line_num, &branch_found) != 0)
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
    long long start_offset, int is_packed, int depth, int line_num, int *found)
{
    if (found != NULL)
        *found = 0;

    long long offset = start_offset;
    ListNode_t *cur = members;
    while (cur != NULL)
    {
        if (cur->type == LIST_RECORD_FIELD)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            if (field != NULL && !record_field_is_hidden(field))
            {
                /* Class variables are stored as globals, not in the instance
                 * layout.  They can still be looked up by name (for access
                 * resolution) but must not contribute to the running offset. */
                if (field->is_class_var)
                {
                    if (field->name != NULL &&
                        pascal_identifier_equals(field->name, field_name))
                    {
                        if (out_field != NULL)
                            *out_field = field;
                        if (offset_out != NULL)
                            *offset_out = -1; /* sentinel: not an instance offset */
                        if (found != NULL)
                            *found = 1;
                        return 0;
                    }
                    cur = cur->next;
                    continue;
                }

                /* Align offset to field's alignment requirement */
                int field_alignment = get_field_alignment(symtab, field, depth + 1, line_num);
                if (!is_packed)
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
                    &variant_field_offset, is_packed, depth + 1, line_num, &variant_found) != 0)
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
            if (sizeof_from_variant_part(symtab, variant, &variant_size,
                    is_packed, depth + 1, line_num) != 0)
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
            *size_out = POINTER_SIZE_BYTES;
            return 0;
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

    if (alias->is_set)
    {
        *size_out = fpc_set_storage_size_from_alias(symtab, alias, depth + 1, line_num);
        return 0;
    }

    if (alias->is_enum)
    {
        *size_out = fpc_enum_storage_size_from_alias(alias);
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

static int get_type_alignment_from_ref(SymTab_t *symtab, int type_tag,
    const char *type_id, int *align_out, int depth, int line_num)
{
    if (align_out == NULL)
        return 1;

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        *align_out = 1;
        return 0;
    }

    /* For known primitive type tags, use the tag directly rather than looking
     * up the type_id in the symbol table.  This avoids misresolution when the
     * same identifier (e.g. "Integer") is aliased to different widths by the
     * FPC system unit (SmallInt, 2 bytes) vs the compiler prelude (LongInt,
     * 4 bytes).  The field's type_tag is authoritative because it was set
     * during parsing based on the resolved type at the declaration site. */
    if (type_tag != UNKNOWN_TYPE && type_tag != RECORD_TYPE &&
        type_tag != ENUM_TYPE && type_tag != SET_TYPE)
    {
        long long size = sizeof_from_type_tag(type_tag);
        if (size > 0)
        {
            int align = fpc_type_alignment_from_size(size, type_tag);
            if (type_tag == SET_TYPE)
                align = (align > POINTER_SIZE_BYTES) ? POINTER_SIZE_BYTES : align;
            *align_out = align;
            return 0;
        }
    }

    if (type_id != NULL && symtab != NULL)
    {
        HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_id);
        if (type_node != NULL)
        {
            /* Pointer type aliases: resolve to POINTER_SIZE_BYTES directly */
            if (type_node->type != NULL && type_node->type->kind == TYPE_KIND_POINTER)
            {
                *align_out = POINTER_SIZE_BYTES;
                return 0;
            }

            struct RecordType *record = get_record_type_from_node(type_node);
            if (record != NULL)
            {
                if (record_type_is_class(record))
                {
                    *align_out = POINTER_SIZE_BYTES;
                    return 0;
                }

                *align_out = get_record_alignment(symtab, record, depth + 1, line_num);
                return 0;
            }

            struct TypeAlias *alias = get_type_alias_from_node(type_node);
            if (alias != NULL)
            {
                if (alias->is_set)
                {
                    long long set_size = fpc_set_storage_size_from_alias(symtab, alias, depth + 1, line_num);
                    int align = fpc_size_to_alignment(set_size);
                    *align_out = (align > POINTER_SIZE_BYTES) ? POINTER_SIZE_BYTES : align;
                    return 0;
                }

                if (alias->is_enum)
                {
                    *align_out = fpc_size_to_alignment(fpc_enum_storage_size_from_alias(alias));
                    return 0;
                }

                if (alias->storage_size > 0)
                {
                    *align_out = fpc_type_alignment_from_size(alias->storage_size,
                        alias->base_type);
                    return 0;
                }
            }

            if (type_node->type != NULL)
            {
                long long size = kgpc_type_sizeof(type_node->type);
                if (size > 0)
                {
                    *align_out = fpc_type_alignment_from_size(size,
                        kgpc_type_get_primitive_tag(type_node->type));
                    if (type_node->type->kind == TYPE_KIND_POINTER)
                        *align_out = POINTER_SIZE_BYTES;
                    return 0;
                }
            }
        }
    }

    if (type_tag != UNKNOWN_TYPE)
    {
        long long size = sizeof_from_type_tag(type_tag);
        if (size > 0)
        {
            int align = fpc_type_alignment_from_size(size, type_tag);
            if (type_tag == SET_TYPE)
                align = (align > POINTER_SIZE_BYTES) ? POINTER_SIZE_BYTES : align;
            *align_out = align;
            return 0;
        }
    }

    *align_out = POINTER_SIZE_BYTES;
    return 0;
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

    /* Walk the inheritance chain (object/class hierarchy) */
    struct RecordType *current = record;
    int depth = 0;
    while (current != NULL && depth < 32) /* guard against infinite loops */
    {
        long long offset = 0;
        int found = 0;
        long long start_offset = 0;
        if (current->parent_class_name != NULL && !current->parent_fields_merged && symtab != NULL)
        {
            HashNode_t *parent_node = semcheck_find_preferred_type_node(symtab, current->parent_class_name);
            struct RecordType *parent_record = get_record_type_from_node(parent_node);
            if (parent_record != NULL &&
                sizeof_from_record(symtab, parent_record, &start_offset, 0, line_num) != 0)
                return 1;
        }
        if (find_field_in_members(symtab, current->fields, field_name, out_field,
                &offset, start_offset, current->is_packed, 0, line_num, &found) != 0)
            return 1;

        if (found)
        {
            if (offset_out != NULL)
                *offset_out = offset;
            return 0;
        }

        /* Try parent type */
        if (current->parent_class_name != NULL)
        {
            HashNode_t *parent_node = semcheck_find_preferred_type_node(symtab, current->parent_class_name);
            if (parent_node != NULL)
            {
                struct RecordType *parent_record = get_record_type_from_node(parent_node);
                if (parent_record != NULL)
                {
                    current = parent_record;
                    depth++;
                    continue;
                }
            }
        }
        break;
    }

    if (!silent)
        semcheck_error_with_context("Error on line %d, record field %s not found.\n", line_num, field_name);
    return 1;
}

int semcheck_compute_record_size(SymTab_t *symtab, struct RecordType *record,
    long long *size_out, int line_num)
{
    return sizeof_from_record(symtab, record, size_out, 0, line_num);
}
