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

#endif // GPC_TYPE_H
