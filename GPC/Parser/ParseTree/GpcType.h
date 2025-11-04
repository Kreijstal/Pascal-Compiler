/*
 * GpcType.h
 * First-class type system for the Gwinn Pascal Compiler
 * 
 * This module provides a unified type representation that replaces the
 * scattered type system (VarType enum, TypeAlias struct, various flags).
 * All type information is now contained in a single GpcType structure.
 */

#ifndef GPC_TYPE_H
#define GPC_TYPE_H

#include "../List/List.h" // For ListNode_t
#include "tree.h"         // For Tree_t

// Forward declaration to allow recursive type definitions (e.g., pointers).
typedef struct GpcType GpcType;

// Forward declaration for symbol table (avoid circular dependency)
struct SymTab;

// Forward declaration for VarType enum (defined in HashTable.h)
enum VarType;

// Defines what kind of type we are dealing with.
typedef enum {
    TYPE_KIND_PRIMITIVE, // Integer, Real, Char, Boolean, etc.
    TYPE_KIND_POINTER,
    TYPE_KIND_ARRAY,
    TYPE_KIND_RECORD,
    TYPE_KIND_PROCEDURE
} GpcTypeKind;

// --- Detailed Information for Complex Types ---

// For TYPE_KIND_PROCEDURE
typedef struct {
    // A list of Tree_t* nodes, where each node is a TREE_VAR_DECL representing a parameter.
    // This reuses the existing, well-understood structure for parameter declarations.
    ListNode_t *params;
    
    // For functions, this points to the return type.
    // For procedures, this is NULL.
    GpcType *return_type;
} ProcedureTypeInfo;

// For TYPE_KIND_ARRAY
typedef struct {
    GpcType *element_type;
    int start_index;
    int end_index;
    // Can be extended with more dimension info if needed.
} ArrayTypeInfo;

// The main, unified type structure.
struct GpcType {
    GpcTypeKind kind;
    int size_in_bytes;      // To be calculated and filled in by the semantic checker.
    int alignment_in_bytes; // For future architecture support.
    
    // Optional type alias metadata - points to TypeAlias if this type was declared via a type alias.
    // This is owned by the AST, not by GpcType, so should not be freed.
    struct TypeAlias *type_alias;

    // A union to hold the specific details of the type.
    union {
        int primitive_type_tag; // For TYPE_KIND_PRIMITIVE (e.g., INT_TYPE, REAL_TYPE from type_tags.h)
        GpcType *points_to;     // For TYPE_KIND_POINTER
        ArrayTypeInfo array_info;
        ProcedureTypeInfo proc_info;
        struct RecordType *record_info; // For TYPE_KIND_RECORD
    } info;
};

// --- Type System API ---

// Constructor functions
GpcType* create_primitive_type(int primitive_tag);
GpcType* create_pointer_type(GpcType *points_to);
GpcType* create_procedure_type(ListNode_t *params, GpcType *return_type);
GpcType* create_array_type(GpcType *element_type, int start_index, int end_index);
GpcType* create_record_type(struct RecordType *record_info);

/* Create GpcType from TypeAlias structure
 * Handles ALL TypeAlias cases: arrays, pointers, sets, enums, files, primitives
 * Returns NULL if conversion fails (e.g., unresolvable type reference)
 * symtab is used to resolve type references (target_type_id, element_type_id, etc.)
 */
GpcType* create_gpc_type_from_type_alias(struct TypeAlias *alias, struct SymTab *symtab);

// Destructor function (CRITICAL for preventing memory leaks)
void destroy_gpc_type(GpcType *type);

// Utility functions
int are_types_compatible_for_assignment(GpcType *lhs_type, GpcType *rhs_type, struct SymTab *symtab);
const char* gpc_type_to_string(GpcType *type); // For debugging

/* Helper function to resolve GpcType from a parameter Tree_t node (TREE_VAR_DECL)
 * Returns NULL if type cannot be resolved.
 * If owns_type is not NULL, it will be set to 1 if caller owns the returned type (must free it),
 * or 0 if the type is a reference (must not free it).
 */
GpcType* resolve_type_from_vardecl(Tree_t *var_decl, struct SymTab *symtab, int *owns_type);

// --- Helper Functions for Accessing Type Information ---

/* Get the size in bytes of a type.
 * Returns the size, or -1 if size cannot be determined.
 * For records and arrays, computes the full size including all fields/elements. */
long long gpc_type_sizeof(GpcType *type);

/* Check if a type is an array type. */
int gpc_type_is_array(GpcType *type);

/* Check if a type is a record type. */
int gpc_type_is_record(GpcType *type);

/* Check if a type is a procedure type. */
int gpc_type_is_procedure(GpcType *type);

/* Get array bounds. Returns 0 on success, -1 if not an array.
 * start_out and end_out may be NULL if not needed. */
int gpc_type_get_array_bounds(GpcType *type, int *start_out, int *end_out);

/* Get the record info from a record type.
 * Returns NULL if not a record type. */
struct RecordType* gpc_type_get_record(GpcType *type);

/* Get the primitive type tag from a primitive type.
 * Returns the tag, or -1 if not a primitive type. */
int gpc_type_get_primitive_tag(GpcType *type);

/* Get the element type of an array.
 * Returns NULL if not an array type. */
GpcType* gpc_type_get_array_element_type(GpcType *type);

/* Get formal parameters from a procedure/function type.
 * Returns NULL if not a procedure type. */
ListNode_t* gpc_type_get_procedure_params(GpcType *type);

/* Get return type from a function type.
 * Returns NULL if not a function or if it's a procedure (no return type). */
GpcType* gpc_type_get_return_type(GpcType *type);

/* Check if an array type is a dynamic/open array.
 * Returns 1 if it is a dynamic array, 0 otherwise. */
int gpc_type_is_dynamic_array(GpcType *type);

/* Get element size in bytes for an array type.
 * Returns the element size, or -1 if not an array or size cannot be determined. */
long long gpc_type_get_array_element_size(GpcType *type);

/* Create a GpcType from a VarType enum value.
 * This is a helper for migrating from the legacy type system.
 * Note: HASHVAR_ARRAY, HASHVAR_RECORD, HASHVAR_POINTER, HASHVAR_PROCEDURE require
 * additional information beyond VarType and will return NULL - caller must use
 * appropriate create_*_type() function instead.
 * Returns a new GpcType that caller owns, or NULL for complex types. */
GpcType* gpc_type_from_var_type(enum VarType var_type);

/* Get the type alias metadata from a GpcType.
 * Returns NULL if no type alias metadata is attached. */
struct TypeAlias* gpc_type_get_type_alias(GpcType *type);

/* Set the type alias metadata on a GpcType.
 * The TypeAlias is owned by the AST, not by GpcType. */
void gpc_type_set_type_alias(GpcType *type, struct TypeAlias *alias);

/* Get the legacy type tag from a GpcType for compatibility with codegen.
 * This is a migration helper that allows code to work with both old and new type systems.
 * Returns a type tag (INT_TYPE, REAL_TYPE, etc.) for primitives, or specialized tags
 * like RECORD_TYPE, POINTER_TYPE, PROCEDURE for complex types.
 * Returns UNKNOWN_TYPE if the type cannot be mapped. */
int gpc_type_get_legacy_tag(GpcType *type);

/* Check if a GpcType represents a pointer type.
 * Returns 1 if it's a pointer, 0 otherwise. */
int gpc_type_is_pointer(GpcType *type);

/* For pointer types, get the type tag of what it points to.
 * Returns UNKNOWN_TYPE if not a pointer or if the pointed-to type is complex. */
int gpc_type_get_pointer_subtype_tag(GpcType *type);

/* Check if a GpcType requires qword (64-bit) operations.
 * Returns 1 if the type uses qword, 0 otherwise.
 * This replaces codegen_type_uses_qword() for GpcType-based code. */
int gpc_type_uses_qword(GpcType *type);

/* Check if a GpcType represents a signed integer type.
 * Returns 1 if signed, 0 otherwise.
 * This replaces codegen_type_is_signed() for GpcType-based code. */
int gpc_type_is_signed(GpcType *type);

/* Check if a GpcType matches a specific legacy type tag.
 * This is a helper for transitioning code that compares types.
 * Returns 1 if the type matches the tag, 0 otherwise. */
int gpc_type_equals_tag(GpcType *type, int type_tag);

#endif // GPC_TYPE_H
