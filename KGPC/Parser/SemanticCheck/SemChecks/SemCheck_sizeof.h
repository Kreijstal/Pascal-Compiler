/*
    SizeOf and field resolution utilities for semantic checking.

    This module contains functions for computing the size of types,
    resolving record fields and their offsets.
*/

#ifndef SEM_CHECK_SIZEOF_H
#define SEM_CHECK_SIZEOF_H

#include "../SymTab/SymTab.h"
#include "../../ParseTree/tree_types.h"

/* Compute the size of a record type in bytes.
 * Returns 0 on success, 1 on error.
 * size_out is set to the computed size on success.
 */
int semcheck_compute_record_size(SymTab_t *symtab, struct RecordType *record,
    long long *size_out, int line_num);

/* Resolve a field within a record type.
 * Returns 0 on success, 1 on error.
 * out_field is set to the found field on success.
 * offset_out is set to the byte offset of the field on success.
 * If silent is non-zero, no error message is printed when field is not found.
 */
int resolve_record_field(SymTab_t *symtab, struct RecordType *record,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int line_num, int silent);

/* Internal sizeof functions - used by semcheck_builtin_sizeof in SemCheck_expr.c */

/* Get size from a primitive type tag */
long long sizeof_from_type_tag(int type_tag);

/* Get size from a type reference (tag and/or type_id) */
int sizeof_from_type_ref(SymTab_t *symtab, int type_tag,
    const char *type_id, long long *size_out, int depth, int line_num);

/* Get size from a hash node */
int sizeof_from_hashnode(SymTab_t *symtab, HashNode_t *node,
    long long *size_out, int depth, int line_num);

/* Get size from a record type */
int sizeof_from_record(SymTab_t *symtab, struct RecordType *record,
    long long *size_out, int depth, int line_num);

/* Get size from a type alias */
int sizeof_from_alias(SymTab_t *symtab, struct TypeAlias *alias,
    long long *size_out, int depth, int line_num);

/* Helper to find preferred type node for a type_id - needed by sizeof functions */
HashNode_t *semcheck_find_preferred_type_node(SymTab_t *symtab, const char *type_id);

/* Recursion limit and pointer size constants */
#define SIZEOF_RECURSION_LIMIT 64
#define POINTER_SIZE_BYTES 8

#endif /* SEM_CHECK_SIZEOF_H */
