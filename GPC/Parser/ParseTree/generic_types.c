#include "generic_types.h"
#include "../SemanticCheck/SymTab/SymTab.h"
#include "../SemanticCheck/HashTable/HashTable.h"
#include "tree.h"
#include "tree_types.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Forward declarations */
extern struct RecordType *clone_record_type(const struct RecordType *record_type);
extern int PushTypeOntoScope_Typed(struct SymTab *symtab, char *id, GpcType *type);
extern void destroy_gpc_type(GpcType *type);

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

/* Helper function to substitute type parameters in a RecordType's fields */
static void substitute_type_params_in_record(struct RecordType* record, 
                                             char** type_params,
                                             char** concrete_types,
                                             int num_params) {
    if (record == NULL || type_params == NULL || concrete_types == NULL || num_params == 0) {
        return;
    }

    /* Iterate through fields and substitute type_id references */
    ListNode_t* field_node = record->fields;
    while (field_node != NULL) {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL) {
            struct RecordField* field = (struct RecordField*)field_node->cur;
            
            /* Check if field's type_id matches any type parameter */
            if (field->type_id != NULL) {
                for (int i = 0; i < num_params; i++) {
                    if (strcmp(field->type_id, type_params[i]) == 0) {
                        /* Substitute with concrete type */
                        free(field->type_id);
                        field->type_id = strdup(concrete_types[i]);
                        break;
                    }
                }
            }
            
            /* Also check array element type_id */
            if (field->is_array && field->array_element_type_id != NULL) {
                for (int i = 0; i < num_params; i++) {
                    if (strcmp(field->array_element_type_id, type_params[i]) == 0) {
                        /* Substitute with concrete type */
                        free(field->array_element_type_id);
                        field->array_element_type_id = strdup(concrete_types[i]);
                        break;
                    }
                }
            }
            
            /* Recursively substitute in nested records */
            if (field->nested_record != NULL) {
                substitute_type_params_in_record(field->nested_record, type_params, 
                                                concrete_types, num_params);
            }
        }
        field_node = field_node->next;
    }
}

/* Process all pending specializations and add them to the symbol table */
int generic_process_specializations(struct SymTab* symtab) {
    if (symtab == NULL) {
        return -1;
    }

    int errors = 0;
    GenericSpecialization* spec = g_generic_registry.specializations;
    
    while (spec != NULL) {
        /* Skip if already processed */
        if (spec->specialized_type != NULL) {
            spec = spec->next;
            continue;
        }

        /* Find the generic declaration */
        GenericTypeDecl* generic_decl = generic_registry_find_decl(spec->generic_name);
        if (generic_decl == NULL) {
            fprintf(stderr, "Error: Generic type '%s' not found\n", spec->generic_name);
            errors++;
            spec = spec->next;
            continue;
        }

        /* Verify parameter count matches */
        if (generic_decl->num_type_params != spec->num_concrete_types) {
            fprintf(stderr, "Error: Generic type '%s' expects %d type parameters but got %d\n",
                    spec->generic_name, generic_decl->num_type_params, spec->num_concrete_types);
            errors++;
            spec = spec->next;
            continue;
        }

        /* For now, we'll create a simple placeholder type.
         * Full implementation would require:
         * 1. Cloning the generic type's AST
         * 2. Substituting type parameters throughout
         * 3. Running semantic checking on the specialized version
         * 
         * This is a simplified version that just registers the specialized name
         * so the compiler knows it exists.
         */
        
        /* Look up concrete types in symbol table */
        GpcType** concrete_gpc_types = malloc(sizeof(GpcType*) * spec->num_concrete_types);
        if (concrete_gpc_types == NULL) {
            errors++;
            spec = spec->next;
            continue;
        }

        int all_types_found = 1;
        for (int i = 0; i < spec->num_concrete_types; i++) {
            struct HashNode* type_node = NULL;
            if (FindIdent(&type_node, symtab, spec->concrete_types[i]) == -1 || type_node == NULL) {
                fprintf(stderr, "Error: Type '%s' not found for specialization\n", 
                        spec->concrete_types[i]);
                all_types_found = 0;
                break;
            }
            
            if (type_node->type == NULL) {
                fprintf(stderr, "Error: Type '%s' has no GpcType\n", spec->concrete_types[i]);
                all_types_found = 0;
                break;
            }
            
            concrete_gpc_types[i] = type_node->type;
        }

        if (!all_types_found) {
            free(concrete_gpc_types);
            errors++;
            spec = spec->next;
            continue;
        }

        /* Actually perform specialization by creating a specialized type */
        Tree_t* generic_tree = generic_decl->original_decl;
        if (generic_tree == NULL || generic_tree->type != TREE_TYPE_DECL) {
            fprintf(stderr, "Error: Invalid generic declaration for '%s'\n", spec->generic_name);
            free(concrete_gpc_types);
            errors++;
            spec = spec->next;
            continue;
        }

        /* For now, handle only record/class generics */
        if (generic_tree->tree_data.type_decl_data.kind != TYPE_DECL_RECORD &&
            generic_tree->tree_data.type_decl_data.kind != TYPE_DECL_ALIAS) {
            /* Skip non-record/non-alias generics for now */
            free(concrete_gpc_types);
            spec = spec->next;
            continue;
        }

        /* Create a specialized type by cloning and substituting */
        GpcType* specialized_type = NULL;
        
        if (generic_tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD) {
            /* Clone the record type and substitute type parameters in field types */
            struct RecordType* generic_record = generic_tree->tree_data.type_decl_data.info.record;
            if (generic_record != NULL) {
                struct RecordType* specialized_record = clone_record_type(generic_record);
                if (specialized_record != NULL) {
                    /* Substitute type parameters in field type_ids */
                    substitute_type_params_in_record(specialized_record, 
                                                     generic_decl->type_parameters,
                                                     spec->concrete_types,
                                                     generic_decl->num_type_params);
                    
                    /* Update the specialized record's type_id */
                    if (specialized_record->type_id != NULL) {
                        free(specialized_record->type_id);
                    }
                    specialized_record->type_id = strdup(spec->specialized_name);
                    
                    /* Create GpcType for the specialized record */
                    specialized_type = create_record_type(specialized_record);
                }
            }
        }
        
        if (specialized_type != NULL) {
            /* Add the specialized type to the symbol table */
            if (PushTypeOntoScope_Typed(symtab, spec->specialized_name, specialized_type) == 0) {
                /* Success - store the GpcType in the specialization */
                spec->specialized_type = specialized_type;
            } else {
                /* Failed to add to symbol table */
                fprintf(stderr, "Error: Failed to add specialized type '%s' to symbol table\n",
                       spec->specialized_name);
                destroy_gpc_type(specialized_type);
                errors++;
            }
        }
        
        free(concrete_gpc_types);
        spec = spec->next;
    }

    return errors;
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
