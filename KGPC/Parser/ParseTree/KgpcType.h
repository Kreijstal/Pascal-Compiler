/*
 * KgpcType.h
 * First-class type system for the Kreijstal Gwinn Pascal Compiler
 * 
 * This module provides a unified type representation that replaces the
 * scattered type system (VarType enum, TypeAlias struct, various flags).
 * All type information is now contained in a single KgpcType structure.
 */

#ifndef KGPC_TYPE_H
#define KGPC_TYPE_H

#include "../List/List.h" // For ListNode_t
#include "tree.h"         // For Tree_t

// Forward declaration to allow recursive type definitions (e.g., pointers).
typedef struct KgpcType KgpcType;

// Forward declaration for symbol table (avoid circular dependency)
struct SymTab;
struct HashNode;

// Forward declaration for VarType enum (defined in HashTable.h)
enum VarType;

// Defines what kind of type we are dealing with.
typedef enum {
    TYPE_KIND_PRIMITIVE, // Integer, Real, Char, Boolean, etc.
    TYPE_KIND_POINTER,
    TYPE_KIND_ARRAY,
    TYPE_KIND_RECORD,
    TYPE_KIND_PROCEDURE,
    TYPE_KIND_ARRAY_OF_CONST
} KgpcTypeKind;

// --- Detailed Information for Complex Types ---

// For TYPE_KIND_PROCEDURE
typedef struct {
    // A list of Tree_t* nodes, where each node is a TREE_VAR_DECL representing a parameter.
    // This reuses the existing, well-understood structure for parameter declarations.
    ListNode_t *params;
    
    // For functions, this points to the return type.
    // For procedures, this is NULL.
    KgpcType *return_type;
    struct Tree *definition;  /* AST node for procedure/function (if available) */
    char *return_type_id;     /* Cached identifier used for return type lookup */
} ProcedureTypeInfo;

// For TYPE_KIND_ARRAY
typedef struct {
    KgpcType *element_type;
    int start_index;
    int end_index;
    // Deferred resolution: store element type ID if element_type is NULL
    char *element_type_id;
} ArrayTypeInfo;

typedef struct {
    int element_size;
} ArrayOfConstTypeInfo;

// The main, unified type structure.
struct KgpcType {
    KgpcTypeKind kind;
    int size_in_bytes;      // To be calculated and filled in by the semantic checker.
    int alignment_in_bytes; // For future architecture support.
    int ref_count;
    
    // Optional type alias metadata - points to TypeAlias if this type was declared via a type alias.
    // This is owned by the AST, not by KgpcType, so should not be freed.
    struct TypeAlias *type_alias;

    // A union to hold the specific details of the type.
    union {
        int primitive_type_tag; // For TYPE_KIND_PRIMITIVE (e.g., INT_TYPE, REAL_TYPE from type_tags.h)
        KgpcType *points_to;     // For TYPE_KIND_POINTER
        ArrayTypeInfo array_info;
        ProcedureTypeInfo proc_info;
        struct RecordType *record_info; // For TYPE_KIND_RECORD
        ArrayOfConstTypeInfo array_of_const_info;
    } info;
};

// --- Type System API ---

// Constructor functions
KgpcType* create_primitive_type(int primitive_tag);
KgpcType* create_primitive_type_with_size(int primitive_tag, int storage_size);
KgpcType* create_pointer_type(KgpcType *points_to);
KgpcType* create_procedure_type(ListNode_t *params, KgpcType *return_type);
KgpcType* create_array_type(KgpcType *element_type, int start_index, int end_index);
KgpcType* create_array_of_const_type(void);
KgpcType* create_record_type(struct RecordType *record_info);

/* Create KgpcType from TypeAlias structure
 * Handles ALL TypeAlias cases: arrays, pointers, sets, enums, files, primitives
 * Returns NULL if conversion fails (e.g., unresolvable type reference)
 * symtab is used to resolve type references (target_type_id, element_type_id, etc.)
 */
KgpcType* create_kgpc_type_from_type_alias(struct TypeAlias *alias, struct SymTab *symtab);

// Destructor function (CRITICAL for preventing memory leaks)
void destroy_kgpc_type(KgpcType *type);
void kgpc_type_retain(KgpcType *type);
void kgpc_type_release(KgpcType *type);

// Utility functions
int are_types_compatible_for_assignment(KgpcType *lhs_type, KgpcType *rhs_type, struct SymTab *symtab);
const char* kgpc_type_to_string(KgpcType *type); // For debugging

/* Helper function to resolve KgpcType from a parameter Tree_t node (TREE_VAR_DECL)
 * Returns NULL if type cannot be resolved.
 * If owns_type is not NULL, it will be set to 1 if caller owns the returned type (must free it),
 * or 0 if the type is a reference (must not free it).
 */
KgpcType* resolve_type_from_vardecl(Tree_t *var_decl, struct SymTab *symtab, int *owns_type);

// --- Helper Functions for Accessing Type Information ---

/* Get the size in bytes of a type.
 * Returns the size, or -1 if size cannot be determined.
 * For records and arrays, computes the full size including all fields/elements. */
long long kgpc_type_sizeof(KgpcType *type);

/* Check if a type is an array type. */
int kgpc_type_is_array(KgpcType *type);
int kgpc_type_is_array_of_const(KgpcType *type);
int kgpc_type_is_pointer(KgpcType *type);
int kgpc_type_is_record(KgpcType *type);
int kgpc_type_is_procedure(KgpcType *type);

int kgpc_type_is_char(KgpcType *type);
int kgpc_type_is_string(KgpcType *type);
int kgpc_type_is_shortstring(KgpcType *type);
int kgpc_type_is_integer(KgpcType *type);
int kgpc_type_is_real(KgpcType *type);
int kgpc_type_is_numeric(KgpcType *type);
int kgpc_type_is_boolean(KgpcType *type);

/* Check if a type is a record type. */
int kgpc_type_is_record(KgpcType *type);

/* Check if a type is a procedure type. */
int kgpc_type_is_procedure(KgpcType *type);

/* Get array bounds. Returns 0 on success, -1 if not an array.
 * start_out and end_out may be NULL if not needed. */
int kgpc_type_get_array_bounds(KgpcType *type, int *start_out, int *end_out);

/* Get the record info from a record type.
 * Returns NULL if not a record type. */
struct RecordType* kgpc_type_get_record(KgpcType *type);

/* Get the primitive type tag from a primitive type.
 * Returns the tag, or -1 if not a primitive type. */
int kgpc_type_get_primitive_tag(KgpcType *type);

/* Get the element type of an array.
 * Returns NULL if not an array type. */
KgpcType* kgpc_type_get_array_element_type(KgpcType *type);

/* Get the element type of an array, resolving deferred types if needed.
 * If the element_type is NULL but element_type_id is set, tries to resolve it.
 * Returns NULL if not an array or if resolution fails. */
KgpcType* kgpc_type_get_array_element_type_resolved(KgpcType *type, struct SymTab *symtab);

/* Get formal parameters from a procedure/function type.
 * Returns NULL if not a procedure type. */
ListNode_t* kgpc_type_get_procedure_params(KgpcType *type);

/* Get return type from a function type.
 * Returns NULL if not a function or if it's a procedure (no return type). */
KgpcType* kgpc_type_get_return_type(KgpcType *type);

/* Check if an array type is a dynamic/open array.
 * Returns 1 if it is a dynamic array, 0 otherwise. */
int kgpc_type_is_dynamic_array(KgpcType *type);

/* Get element size in bytes for an array type.
 * Returns the element size, or -1 if not an array or size cannot be determined. */
long long kgpc_type_get_array_element_size(KgpcType *type);

/* Get element size for an array-of-const helper structure (TVarRec). */
long long kgpc_type_get_array_of_const_element_size(KgpcType *type);

/* Create a KgpcType from a VarType enum value.
 * This is a helper for migrating from the legacy type system.
 * Note: HASHVAR_ARRAY, HASHVAR_RECORD, HASHVAR_POINTER, HASHVAR_PROCEDURE require
 * additional information beyond VarType and will return NULL - caller must use
 * appropriate create_*_type() function instead.
 * Returns a new KgpcType that caller owns, or NULL for complex types. */
KgpcType* kgpc_type_from_var_type(enum VarType var_type);

/* Get the type alias metadata from a KgpcType.
 * Returns NULL if no type alias metadata is attached. */
struct TypeAlias* kgpc_type_get_type_alias(KgpcType *type);

/* Set the type alias metadata on a KgpcType.
 * The TypeAlias is owned by the AST, not by KgpcType. */
void kgpc_type_set_type_alias(KgpcType *type, struct TypeAlias *alias);

/* Check if a KgpcType represents a pointer type.
 * Returns 1 if it's a pointer, 0 otherwise. */
int kgpc_type_is_pointer(KgpcType *type);

/* For pointer types, get the type tag of what it points to.
 * Returns UNKNOWN_TYPE if not a pointer or if the pointed-to type is complex. */
int kgpc_type_get_pointer_subtype_tag(KgpcType *type);

/* Check if a KgpcType requires qword (64-bit) operations.
 * Returns 1 if the type uses qword, 0 otherwise.
 * This replaces codegen_type_uses_qword() for KgpcType-based code. */
int kgpc_type_uses_qword(KgpcType *type);

/* Check if a KgpcType represents a signed integer type.
 * Returns 1 if signed, 0 otherwise.
 * This replaces codegen_type_is_signed() for KgpcType-based code. */
int kgpc_type_is_signed(KgpcType *type);

/* Check if a KgpcType matches a specific legacy type tag.
 * This is a helper for transitioning code that compares types.
 * Returns 1 if the type matches the tag, 0 otherwise. */
int kgpc_type_equals_tag(KgpcType *type, int type_tag);

/* Compare two KgpcType instances for identity. */
int kgpc_type_equals(KgpcType *a, KgpcType *b);

/* Determine conversion rank from 'from' to 'to'.
 * Returns -1 if incompatible. */
int kgpc_type_conversion_rank(KgpcType *from, KgpcType *to);

/* Check if two pointer types are compatible. */
int kgpc_type_pointers_compatible(KgpcType *ptr_a, KgpcType *ptr_b);

/* Build the function return type from inline alias/type-id/primitive specification.
 * This consolidates the logic used by semantic checking for both forward declarations
 * and full definitions. */
KgpcType* kgpc_type_build_function_return(struct TypeAlias *inline_alias,
                                        struct HashNode *resolved_type_node,
                                        int primitive_tag,
                                        struct SymTab *symtab);

/* Check if a type identified by name uses 64-bit operations.
 * This resolves the type by name through the symbol table, then checks if it uses qword.
 * Returns 1 if the type uses 64-bit operations, 0 otherwise.
 * If symtab is NULL or type cannot be resolved, uses heuristics based on naming conventions
 * (e.g., names starting with 'P' followed by uppercase letter are likely pointers). */
int kgpc_type_id_uses_qword(const char *type_id, struct SymTab *symtab);

#endif // KGPC_TYPE_H
