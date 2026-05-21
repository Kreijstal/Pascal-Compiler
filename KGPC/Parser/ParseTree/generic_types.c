#include "generic_types.h"
#include "tree.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

// Global generic type registry
static GenericRegistry g_generic_registry = {NULL, NULL};

// Current generic context for scoped type-parameter checks
static GenericTypeDecl *g_current_generic_context = NULL;

void generic_registry_init(void) {
    g_generic_registry.generic_decls = NULL;
    g_generic_registry.specializations = NULL;
}

GenericTypeDecl* generic_registry_add_decl(const char* name, char** type_params, int num_params, Tree_t* decl) {
    GenericTypeDecl* generic = malloc(sizeof(GenericTypeDecl));
    generic->name = strdup(name);
    generic->num_type_params = num_params;
    generic->original_decl = decl;
    generic->record_template = NULL;
    generic->nested_type_decls = NULL;
    if (decl != NULL && decl->type == TREE_TYPE_DECL &&
        decl->tree_data.type_decl_data.kind == TYPE_DECL_GENERIC) {
        generic->record_template = decl->tree_data.type_decl_data.info.generic.record_template;
    }
    
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

KgpcType* generic_substitute_type_parameter(KgpcType* type, const char* param_name, KgpcType* concrete_type) {
    if (type == NULL || param_name == NULL || concrete_type == NULL) {
        return type != NULL ? kgpc_type_clone_shallow_owned(type) : NULL;
    }

    struct TypeAlias *alias = type->type_alias;
    if (alias != NULL) {
        if ((alias->alias_name != NULL && strcasecmp(alias->alias_name, param_name) == 0) ||
            (alias->target_type_id != NULL && strcasecmp(alias->target_type_id, param_name) == 0) ||
            (alias->pointer_type_id != NULL && strcasecmp(alias->pointer_type_id, param_name) == 0) ||
            (alias->array_element_type_id != NULL && strcasecmp(alias->array_element_type_id, param_name) == 0) ||
            (alias->set_element_type_id != NULL && strcasecmp(alias->set_element_type_id, param_name) == 0) ||
            (alias->file_type_id != NULL && strcasecmp(alias->file_type_id, param_name) == 0)) {
            return kgpc_type_clone_shallow_owned(concrete_type);
        }
    }

    if (type->kind == TYPE_KIND_RECORD &&
        type->info.record_info != NULL &&
        type->info.record_info->type_id != NULL &&
        strcasecmp(type->info.record_info->type_id, param_name) == 0) {
        return kgpc_type_clone_shallow_owned(concrete_type);
    }

    KgpcType *result = kgpc_type_clone_shallow_owned(type);
    if (result == NULL) {
        return NULL;
    }

    switch (type->kind) {
        case TYPE_KIND_POINTER:
            if (type->info.points_to != NULL) {
                KgpcType *substituted = generic_substitute_type_parameter(
                    type->info.points_to, param_name, concrete_type);
                if (substituted == NULL) {
                    destroy_kgpc_type(result);
                    return NULL;
                }
                destroy_kgpc_type(result->info.points_to);
                result->info.points_to = substituted;
            }
            break;
        case TYPE_KIND_ARRAY:
            if (type->info.array_info.element_type != NULL) {
                KgpcType *substituted = generic_substitute_type_parameter(
                    type->info.array_info.element_type, param_name, concrete_type);
                if (substituted == NULL) {
                    destroy_kgpc_type(result);
                    return NULL;
                }
                destroy_kgpc_type(result->info.array_info.element_type);
                result->info.array_info.element_type = substituted;
            } else if (type->info.array_info.element_type_id != NULL &&
                       strcasecmp(type->info.array_info.element_type_id, param_name) == 0) {
                KgpcType *substituted = kgpc_type_clone_shallow_owned(concrete_type);
                if (substituted == NULL) {
                    destroy_kgpc_type(result);
                    return NULL;
                }
                result->info.array_info.element_type = substituted;
            }
            break;
        case TYPE_KIND_PROCEDURE:
            if (type->info.proc_info.return_type != NULL) {
                KgpcType *substituted = generic_substitute_type_parameter(
                    type->info.proc_info.return_type, param_name, concrete_type);
                if (substituted == NULL) {
                    destroy_kgpc_type(result);
                    return NULL;
                }
                destroy_kgpc_type(result->info.proc_info.return_type);
                result->info.proc_info.return_type = substituted;
            } else if (type->info.proc_info.return_type_id != NULL &&
                       strcasecmp(type->info.proc_info.return_type_id, param_name) == 0) {
                KgpcType *substituted = kgpc_type_clone_shallow_owned(concrete_type);
                if (substituted == NULL) {
                    destroy_kgpc_type(result);
                    return NULL;
                }
                result->info.proc_info.return_type = substituted;
            }
            break;
        case TYPE_KIND_PRIMITIVE:
        case TYPE_KIND_RECORD:
        case TYPE_KIND_ARRAY_OF_CONST:
            break;
    }

    return result;
}

int generic_registry_is_type_param(const char *name) {
    if (name == NULL) return 0;
    assert(g_current_generic_context != NULL &&
           "generic_registry_is_type_param called without a generic context");
    for (int i = 0; i < g_current_generic_context->num_type_params; i++) {
        if (g_current_generic_context->type_parameters[i] != NULL &&
            strcasecmp(g_current_generic_context->type_parameters[i], name) == 0)
            return 1;
    }
    return 0;
}

void generic_registry_set_context(GenericTypeDecl *decl) {
    g_current_generic_context = decl;
}

GenericTypeDecl *generic_registry_current_context(void) {
    return g_current_generic_context;
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
        /* nested_type_decls are appended into the registry exclusively from
         * convert_generic_type_decl (line ~2868) for later specialization; no
         * other owner destroys them, so reclaim here. */
        if (decl->nested_type_decls != NULL) {
            destroy_list(decl->nested_type_decls);
            decl->nested_type_decls = NULL;
        }
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
