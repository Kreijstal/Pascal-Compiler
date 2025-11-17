#ifndef GENERIC_TYPES_H
#define GENERIC_TYPES_H

#include "tree.h"
#include "GpcType.h"

// Forward declarations
typedef struct GenericTypeDecl GenericTypeDecl;
typedef struct GenericSpecialization GenericSpecialization;
typedef struct GenericRegistry GenericRegistry;

// Represents a generic type declaration (e.g., TFPGList<T>)
struct GenericTypeDecl {
    char* name;                    // Generic type name (e.g., "TFPGList")
    char** type_parameters;        // Array of type parameter names (e.g., ["T"])
    int num_type_params;           // Number of type parameters
    Tree_t* original_decl;         // Original AST node for the generic declaration
    GenericTypeDecl* next;         // Linked list of generic declarations
};

// Represents a specialized instance of a generic type (e.g., TFPGList<TMyRecord>)
struct GenericSpecialization {
    char* generic_name;            // Name of the generic type (e.g., "TFPGList")
    char** concrete_types;         // Array of concrete type names (e.g., ["TMyRecord"])
    int num_concrete_types;        // Number of concrete types
    char* specialized_name;        // Mangled name (e.g., "TFPGList_TMyRecord")
    GpcType* specialized_type;     // The specialized GpcType
    GenericSpecialization* next;   // Linked list of specializations
};

// Registry to track all generic types and their specializations
struct GenericRegistry {
    GenericTypeDecl* generic_decls;           // List of generic type declarations
    GenericSpecialization* specializations;   // List of specializations
};

// Initialize the generic type registry
void generic_registry_init(void);

// Register a new generic type declaration
GenericTypeDecl* generic_registry_add_decl(const char* name, char** type_params, int num_params, Tree_t* decl);

// Look up a generic type declaration by name
GenericTypeDecl* generic_registry_find_decl(const char* name);

// Create a new specialization of a generic type
GenericSpecialization* generic_registry_add_specialization(const char* generic_name, char** concrete_types, int num_types);

// Look up a specialization by generic name and concrete types
GenericSpecialization* generic_registry_find_specialization(const char* generic_name, char** concrete_types, int num_types);

// Generate a mangled name for a specialized type
char* generic_mangle_name(const char* generic_name, char** concrete_types, int num_types);

// Perform type parameter substitution in a GpcType
GpcType* generic_substitute_type_parameter(GpcType* type, const char* param_name, GpcType* concrete_type);

// Perform full substitution of all type parameters in a GpcType
GpcType* generic_substitute_all_parameters(GpcType* type, char** param_names, GpcType** concrete_types, int num_params);

// Clean up the generic registry
void generic_registry_cleanup(void);

#endif // GENERIC_TYPES_H
