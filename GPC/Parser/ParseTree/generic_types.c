#include "generic_types.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// Global generic type registry
static GenericRegistry g_generic_registry = {NULL, NULL};

void generic_registry_init(void) {
    g_generic_registry.generic_decls = NULL;
    g_generic_registry.specializations = NULL;
}

GenericTypeDecl* generic_registry_add_decl(const char* name, char** type_params, int num_params, Tree_t* decl) {
    GenericTypeDecl* generic = malloc(sizeof(GenericTypeDecl));
    generic->name = strdup(name);
    generic->num_type_params = num_params;
    generic->original_decl = decl;
    
    // Copy type parameter names
    generic->type_parameters = malloc(sizeof(char*) * num_params);
    for (int i = 0; i < num_params; i++) {
        generic->type_parameters[i] = strdup(type_params[i]);
    }
    
    // Add to front of list
    generic->next = g_generic_registry.generic_decls;
    g_generic_registry.generic_decls = generic;
    
    return generic;
}

GenericTypeDecl* generic_registry_find_decl(const char* name) {
    for (GenericTypeDecl* decl = g_generic_registry.generic_decls; decl != NULL; decl = decl->next) {
        if (strcmp(decl->name, name) == 0) {
            return decl;
        }
    }
    return NULL;
}

char* generic_mangle_name(const char* generic_name, char** concrete_types, int num_types) {
    // Calculate needed buffer size
    size_t len = strlen(generic_name) + 1; // +1 for null terminator
    for (int i = 0; i < num_types; i++) {
        len += strlen(concrete_types[i]) + 1; // +1 for underscore
    }
    
    char* mangled = malloc(len);
    strcpy(mangled, generic_name);
    
    for (int i = 0; i < num_types; i++) {
        strcat(mangled, "_");
        strcat(mangled, concrete_types[i]);
    }
    
    return mangled;
}

GenericSpecialization* generic_registry_add_specialization(const char* generic_name, char** concrete_types, int num_types) {
    // Check if this specialization already exists
    GenericSpecialization* existing = generic_registry_find_specialization(generic_name, concrete_types, num_types);
    if (existing != NULL) {
        return existing;
    }
    
    GenericSpecialization* spec = malloc(sizeof(GenericSpecialization));
    spec->generic_name = strdup(generic_name);
    spec->num_concrete_types = num_types;
    spec->specialized_name = generic_mangle_name(generic_name, concrete_types, num_types);
    spec->specialized_type = NULL; // To be filled in during semantic check
    
    // Copy concrete type names
    spec->concrete_types = malloc(sizeof(char*) * num_types);
    for (int i = 0; i < num_types; i++) {
        spec->concrete_types[i] = strdup(concrete_types[i]);
    }
    
    // Add to front of list
    spec->next = g_generic_registry.specializations;
    g_generic_registry.specializations = spec;
    
    return spec;
}

GenericSpecialization* generic_registry_find_specialization(const char* generic_name, char** concrete_types, int num_types) {
    for (GenericSpecialization* spec = g_generic_registry.specializations; spec != NULL; spec = spec->next) {
        if (strcmp(spec->generic_name, generic_name) != 0) {
            continue;
        }
        if (spec->num_concrete_types != num_types) {
            continue;
        }
        
        // Check if all concrete types match
        int match = 1;
        for (int i = 0; i < num_types; i++) {
            if (strcmp(spec->concrete_types[i], concrete_types[i]) != 0) {
                match = 0;
                break;
            }
        }
        
        if (match) {
            return spec;
        }
    }
    return NULL;
}

GpcType* generic_substitute_type_parameter(GpcType* type, const char* param_name, GpcType* concrete_type) {
    if (type == NULL || param_name == NULL || concrete_type == NULL) {
        return type;
    }

    /* Handle each type kind */
    switch (type->kind) {
        case TYPE_KIND_PRIMITIVE:
            /* For primitive types, check if this is a type parameter reference.
             * Type parameters in Pascal are represented as identifiers, not primitive types,
             * so we don't substitute here. Primitives are leaf nodes. */
            return type;
        
        case TYPE_KIND_POINTER:
        {
            /* Substitute in the pointed-to type */
            GpcType* points_to = type->info.points_to;
            GpcType* new_points_to = generic_substitute_type_parameter(points_to, param_name, concrete_type);
            
            if (new_points_to != points_to) {
                /* Need to create a new pointer type */
                GpcType* new_type = create_pointer_type(new_points_to);
                return new_type;
            }
            return type;
        }
        
        case TYPE_KIND_ARRAY:
        {
            /* Substitute in the element type */
            GpcType* element_type = type->info.array_info.element_type;
            GpcType* new_element_type = generic_substitute_type_parameter(element_type, param_name, concrete_type);
            
            if (new_element_type != element_type) {
                /* Need to create a new array type */
                GpcType* new_type = create_array_type(
                    new_element_type,
                    type->info.array_info.start_index,
                    type->info.array_info.end_index
                );
                return new_type;
            }
            return type;
        }
        
        case TYPE_KIND_RECORD:
        {
            /* For records, we would need to substitute in field types.
             * This is complex as it requires cloning the RecordType structure
             * and substituting in each field's type. For now, return as-is.
             * Full implementation requires deep cloning of RecordType. */
            return type;
        }
        
        case TYPE_KIND_PROCEDURE:
        {
            /* Substitute in parameter types and return type */
            int needs_new_type = 0;
            
            /* Check if return type needs substitution */
            GpcType* return_type = type->info.proc_info.return_type;
            GpcType* new_return_type = NULL;
            if (return_type != NULL) {
                new_return_type = generic_substitute_type_parameter(return_type, param_name, concrete_type);
                if (new_return_type != return_type) {
                    needs_new_type = 1;
                }
            }
            
            /* For parameters, we would need to iterate and substitute.
             * This is complex as parameters are stored as Tree_t* nodes.
             * For now, simplified implementation. */
            
            if (needs_new_type) {
                /* Would need to clone parameter list with substitutions */
                /* For now, return original type */
                return type;
            }
            return type;
        }
        
        case TYPE_KIND_ARRAY_OF_CONST:
            /* Array of const doesn't have type parameters */
            return type;
        
        default:
            return type;
    }
}

/* Perform full substitution of all type parameters in a GpcType */
GpcType* generic_substitute_all_parameters(GpcType* type, char** param_names, GpcType** concrete_types, int num_params) {
    if (type == NULL || param_names == NULL || concrete_types == NULL || num_params <= 0) {
        return type;
    }
    
    GpcType* result = type;
    for (int i = 0; i < num_params; i++) {
        result = generic_substitute_type_parameter(result, param_names[i], concrete_types[i]);
    }
    
    return result;
}

void generic_registry_cleanup(void) {
    // Clean up generic declarations
    GenericTypeDecl* decl = g_generic_registry.generic_decls;
    while (decl != NULL) {
        GenericTypeDecl* next = decl->next;
        free(decl->name);
        for (int i = 0; i < decl->num_type_params; i++) {
            free(decl->type_parameters[i]);
        }
        free(decl->type_parameters);
        free(decl);
        decl = next;
    }
    
    // Clean up specializations
    GenericSpecialization* spec = g_generic_registry.specializations;
    while (spec != NULL) {
        GenericSpecialization* next = spec->next;
        free(spec->generic_name);
        free(spec->specialized_name);
        for (int i = 0; i < spec->num_concrete_types; i++) {
            free(spec->concrete_types[i]);
        }
        free(spec->concrete_types);
        // Note: specialized_type is owned by the type system, don't free here
        free(spec);
        spec = next;
    }
    
    g_generic_registry.generic_decls = NULL;
    g_generic_registry.specializations = NULL;
}
