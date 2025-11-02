/*
 * GpcType.c
 * First-class type system implementation for the Gwinn Pascal Compiler
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "GpcType.h"
#include "type_tags.h"
#include "tree_types.h"

// --- Constructor Implementations ---

GpcType* create_primitive_type(int primitive_tag) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_PRIMITIVE;
    type->info.primitive_type_tag = primitive_tag;
    // Size will be determined later in semcheck
    return type;
}

GpcType* create_pointer_type(GpcType *points_to) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_POINTER;
    type->info.points_to = points_to; // Takes ownership of points_to
    return type;
}

GpcType* create_procedure_type(ListNode_t *params, GpcType *return_type) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_PROCEDURE;
    type->info.proc_info.params = params; // Takes ownership
    type->info.proc_info.return_type = return_type; // Takes ownership
    return type;
}

GpcType* create_array_type(GpcType *element_type, int start_index, int end_index) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_ARRAY;
    type->info.array_info.element_type = element_type; // Takes ownership
    type->info.array_info.start_index = start_index;
    type->info.array_info.end_index = end_index;
    return type;
}

GpcType* create_record_type(struct RecordType *record_info) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_RECORD;
    type->info.record_info = record_info; // Record is owned by the AST
    return type;
}

// --- Destructor Implementation ---

void destroy_gpc_type(GpcType *type) {
    if (type == NULL) return;

    switch (type->kind) {
        case TYPE_KIND_POINTER:
            destroy_gpc_type(type->info.points_to);
            break;
        case TYPE_KIND_PROCEDURE:
            // The list itself contains Tree_t*, which are owned by the AST
            // and should not be freed here. Just free the list nodes.
            DestroyList(type->info.proc_info.params);
            destroy_gpc_type(type->info.proc_info.return_type);
            break;
        case TYPE_KIND_ARRAY:
            destroy_gpc_type(type->info.array_info.element_type);
            break;
        case TYPE_KIND_PRIMITIVE:
            // Nothing to free
            break;
        case TYPE_KIND_RECORD:
            // record_info is owned by the AST, not by us
            break;
    }
    free(type);
}

// --- Utility Implementations ---

/* Helper function to check numeric type compatibility */
static int types_numeric_compatible(int lhs, int rhs) {
    /* Exact match */
    if (lhs == rhs)
        return 1;

    /* Integer and longint are compatible */
    if ((lhs == INT_TYPE && rhs == LONGINT_TYPE) || (lhs == LONGINT_TYPE && rhs == INT_TYPE))
        return 1;

    /* Real can accept integer or longint */
    if (lhs == REAL_TYPE && (rhs == INT_TYPE || rhs == LONGINT_TYPE))
        return 1;

    /* Integer can accept char (for compatibility) */
    if (lhs == INT_TYPE && rhs == CHAR_TYPE)
        return 1;

    return 0;
}

/* Helper function to resolve GpcType from a parameter Tree_t node 
 * This is needed for procedure type compatibility checking */
GpcType *resolve_type_from_vardecl(Tree_t *var_decl, struct SymTab *symtab, int *owns_type) {
    if (var_decl == NULL || var_decl->type != TREE_VAR_DECL)
        return NULL;

    if (owns_type != NULL)
        *owns_type = 0;

    /* For now, we create a primitive type from the type field
     * In a fully migrated system, the type would be stored directly */
    int var_type_tag = var_decl->tree_data.var_decl_data.type;

    /* Handle special cases */
    if (var_type_tag == UNKNOWN_TYPE && var_decl->tree_data.var_decl_data.type_id != NULL) {
        /* This is a named type reference - for now we can't fully resolve it without symtab */
        /* We'll do a simple check based on the type_id string matching */
        (void)symtab;
        return NULL;
    }

    if (owns_type != NULL)
        *owns_type = 1;
    return create_primitive_type(var_type_tag);
}

int are_types_compatible_for_assignment(GpcType *lhs_type, GpcType *rhs_type, struct SymTab *symtab) {
    /* NULL types are incompatible */
    if (lhs_type == NULL || rhs_type == NULL)
        return 0;

    /* If kinds are different, generally incompatible */
    /* Exception: we need to check for special cases */
    if (lhs_type->kind != rhs_type->kind) {
        /* Allow nil (represented as pointer) to be assigned to any pointer */
        /* This is a common Pascal idiom but requires special handling */
        return 0;
    }

    switch (lhs_type->kind) {
        case TYPE_KIND_PRIMITIVE:
            /* Use numeric compatibility for primitives */
            return types_numeric_compatible(
                lhs_type->info.primitive_type_tag,
                rhs_type->info.primitive_type_tag);

        case TYPE_KIND_POINTER:
            /* Pointers are compatible if they point to compatible types */
            /* Also allow nil assignment (both point to NULL) */
            if (lhs_type->info.points_to == NULL && rhs_type->info.points_to == NULL)
                return 1;
            if (lhs_type->info.points_to == NULL || rhs_type->info.points_to == NULL)
                return 1; /* nil can be assigned to any pointer */
            return are_types_compatible_for_assignment(
                lhs_type->info.points_to,
                rhs_type->info.points_to,
                symtab);

        case TYPE_KIND_ARRAY:
            /* Arrays are compatible if element types match and dimensions match */
            if (lhs_type->info.array_info.start_index != rhs_type->info.array_info.start_index)
                return 0;
            if (lhs_type->info.array_info.end_index != rhs_type->info.array_info.end_index)
                return 0;
            return are_types_compatible_for_assignment(
                lhs_type->info.array_info.element_type,
                rhs_type->info.array_info.element_type,
                symtab);

        case TYPE_KIND_RECORD:
            /* Records are compatible if they are the same record type 
             * For now, we do pointer equality on record_info */
            return lhs_type->info.record_info == rhs_type->info.record_info;

        case TYPE_KIND_PROCEDURE: {
            ProcedureTypeInfo *lhs_proc = &lhs_type->info.proc_info;
            ProcedureTypeInfo *rhs_proc = &rhs_type->info.proc_info;

            /* 1. Check function vs procedure compatibility 
             * A procedure variable can only hold a procedure, not a function, and vice versa */
            int lhs_is_function = (lhs_proc->return_type != NULL);
            int rhs_is_function = (rhs_proc->return_type != NULL);
            
            if (lhs_is_function != rhs_is_function)
                return 0; /* Cannot assign function to procedure var or vice versa */

            /* 2. If both are functions, check return types */
            if (lhs_is_function) {
                if (!are_types_compatible_for_assignment(
                        lhs_proc->return_type,
                        rhs_proc->return_type,
                        symtab))
                    return 0;
            }

            /* 3. Check parameter counts */
            int lhs_param_count = ListLength(lhs_proc->params);
            int rhs_param_count = ListLength(rhs_proc->params);
            
            if (lhs_param_count != rhs_param_count)
                return 0;

            /* 4. Check each parameter's type and var status */
            ListNode_t *lhs_p = lhs_proc->params;
            ListNode_t *rhs_p = rhs_proc->params;
            
            while (lhs_p != NULL && rhs_p != NULL) {
                if (lhs_p->type != LIST_TREE || rhs_p->type != LIST_TREE)
                    return 0; /* Invalid parameter node */

                Tree_t *lhs_decl = (Tree_t *)lhs_p->cur;
                Tree_t *rhs_decl = (Tree_t *)rhs_p->cur;

                if (lhs_decl == NULL || rhs_decl == NULL)
                    return 0;

                if (lhs_decl->type != TREE_VAR_DECL || rhs_decl->type != TREE_VAR_DECL)
                    return 0;

                /* Check var vs. value parameter 
                 * var parameters must match exactly */
                int lhs_is_var = lhs_decl->tree_data.var_decl_data.is_var_param;
                int rhs_is_var = rhs_decl->tree_data.var_decl_data.is_var_param;
                
                if (lhs_is_var != rhs_is_var)
                    return 0;

                /* Check parameter types */
                int lhs_param_owned = 0;
                int rhs_param_owned = 0;
                GpcType *lhs_param_type = resolve_type_from_vardecl(lhs_decl, symtab, &lhs_param_owned);
                GpcType *rhs_param_type = resolve_type_from_vardecl(rhs_decl, symtab, &rhs_param_owned);

                int param_compatible = 1;
                if (lhs_param_type != NULL && rhs_param_type != NULL) {
                    param_compatible = are_types_compatible_for_assignment(
                        lhs_param_type, rhs_param_type, symtab);
                } else if (lhs_param_type != NULL || rhs_param_type != NULL) {
                    /* One is NULL, other is not - check type_id strings as fallback */
                    const char *lhs_type_id = lhs_decl->tree_data.var_decl_data.type_id;
                    const char *rhs_type_id = rhs_decl->tree_data.var_decl_data.type_id;
                    
                    /* If both have type IDs, compare them (for named types) */
                    if (lhs_type_id != NULL && rhs_type_id != NULL) {
                        param_compatible = (strcmp(lhs_type_id, rhs_type_id) == 0);
                    } else {
                        /* Different parameter types (one named, one primitive) */
                        param_compatible = 0;
                    }
                } else {
                    /* Both are NULL - check if type tags match */
                    int lhs_tag = lhs_decl->tree_data.var_decl_data.type;
                    int rhs_tag = rhs_decl->tree_data.var_decl_data.type;
                    param_compatible = types_numeric_compatible(lhs_tag, rhs_tag);
                }

                /* Clean up temporary types */
                if (lhs_param_owned && lhs_param_type != NULL)
                    destroy_gpc_type(lhs_param_type);
                if (rhs_param_owned && rhs_param_type != NULL)
                    destroy_gpc_type(rhs_param_type);

                if (!param_compatible)
                    return 0;

                lhs_p = lhs_p->next;
                rhs_p = rhs_p->next;
            }

            /* All checks passed */
            return 1;
        }

        default:
            /* Unknown type kind */
            return 0;
    }
}

const char* gpc_type_to_string(GpcType *type) {
    static char buffer[256];
    if (type == NULL) {
        return "NULL";
    }
    
    switch (type->kind) {
        case TYPE_KIND_PRIMITIVE:
            switch (type->info.primitive_type_tag) {
                case INT_TYPE: return "integer";
                case REAL_TYPE: return "real";
                case LONGINT_TYPE: return "longint";
                case STRING_TYPE: return "string";
                case CHAR_TYPE: return "char";
                case BOOL: return "boolean";
                case POINTER_TYPE: return "pointer";
                case SET_TYPE: return "set";
                case ENUM_TYPE: return "enum";
                case FILE_TYPE: return "file";
                default:
                    snprintf(buffer, sizeof(buffer), "primitive(%d)", type->info.primitive_type_tag);
                    return buffer;
            }
        case TYPE_KIND_POINTER:
            snprintf(buffer, sizeof(buffer), "^%s", gpc_type_to_string(type->info.points_to));
            return buffer;
        case TYPE_KIND_ARRAY:
            snprintf(buffer, sizeof(buffer), "array[%d..%d] of %s",
                type->info.array_info.start_index,
                type->info.array_info.end_index,
                gpc_type_to_string(type->info.array_info.element_type));
            return buffer;
        case TYPE_KIND_RECORD:
            return "record";
        case TYPE_KIND_PROCEDURE:
            if (type->info.proc_info.return_type == NULL) {
                return "procedure";
            } else {
                snprintf(buffer, sizeof(buffer), "function: %s",
                    gpc_type_to_string(type->info.proc_info.return_type));
                return buffer;
            }
        default:
            return "unknown";
    }
}
