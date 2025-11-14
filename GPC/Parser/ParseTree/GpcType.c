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
#include "../../format_arg.h"

/* Include symbol table headers for type resolution */
#include "../SemanticCheck/HashTable/HashTable.h"
#include "../SemanticCheck/SymTab/SymTab.h"

// --- Constructor Implementations ---

GpcType* create_primitive_type(int primitive_tag) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_PRIMITIVE;
    type->info.primitive_type_tag = primitive_tag;
    type->ref_count = 1;
    // Size will be determined later in semcheck
    return type;
}

GpcType* create_pointer_type(GpcType *points_to) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_POINTER;
    type->info.points_to = points_to; // Takes ownership of points_to
    type->ref_count = 1;
    return type;
}

GpcType* create_procedure_type(ListNode_t *params, GpcType *return_type) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_PROCEDURE;
    type->info.proc_info.params = params; // Takes ownership
    type->info.proc_info.return_type = return_type; // Takes ownership
    type->info.proc_info.definition = NULL;
    type->info.proc_info.return_type_id = NULL;
    type->ref_count = 1;
    
    #ifdef DEBUG_GPC_TYPE_CREATION
    fprintf(stderr, "DEBUG: create_procedure_type: params=%p, return_type=%p (is_function=%d)\n",
            (void*)params, (void*)return_type, return_type != NULL);
    #endif
    
    return type;
}

GpcType* create_array_type(GpcType *element_type, int start_index, int end_index) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_ARRAY;
    type->info.array_info.element_type = element_type; // Takes ownership
    type->info.array_info.start_index = start_index;
    type->info.array_info.end_index = end_index;
    type->ref_count = 1;
    return type;
}

GpcType* create_array_of_const_type(void) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_ARRAY_OF_CONST;
    type->info.array_of_const_info.element_size = sizeof(gpc_tvarrec);
    type->ref_count = 1;
    return type;
}

GpcType* create_record_type(struct RecordType *record_info) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_RECORD;
    type->info.record_info = record_info; // Record is owned by the AST
    type->ref_count = 1;
    return type;
}

/* Create GpcType from TypeAlias structure
 * Handles ALL TypeAlias cases: arrays, pointers, sets, enums, files, primitives
 * Returns NULL if conversion fails (e.g., unresolvable type reference)
 */
GpcType* create_gpc_type_from_type_alias(struct TypeAlias *alias, struct SymTab *symtab) {
    if (alias == NULL) return NULL;
    
    /* If alias already has a GpcType (enums, sets), use it */
    if (alias->gpc_type != NULL) {
        return alias->gpc_type;
    }
    
    GpcType *result = NULL;
    
    /* Handle array type aliases: type TIntArray = array[1..10] of Integer */
    if (alias->is_array) {
        int start = alias->array_start;
        int end = alias->array_end;
        
        if (alias->is_open_array) {
            start = 0;
            end = -1;
        }
        
        /* Resolve element type */
        GpcType *element_type = NULL;
        int element_type_tag = alias->array_element_type;
        
        if (element_type_tag != UNKNOWN_TYPE) {
            /* Direct primitive type tag */
            element_type = create_primitive_type(element_type_tag);
        } else if (alias->array_element_type_id != NULL && symtab != NULL) {
            /* Type reference - try to resolve it */
            HashNode_t *element_node = NULL;
            if (FindIdent(&element_node, symtab, alias->array_element_type_id) >= 0 &&
                element_node != NULL && element_node->type != NULL) {
                /* Use the resolved type (don't clone, just reference) */
                element_type = element_node->type;
            } else {
                /* Forward reference - create NULL element type for now
                 * This will be resolved when the array is actually used */
                element_type = NULL;
            }
        }
        
        /* Create array type even if element type is NULL (forward reference) */
        result = create_array_type(element_type, start, end);
        if (result != NULL) {
            gpc_type_set_type_alias(result, alias);
        }
        return result;
    }
    
    /* Handle pointer type aliases: type PInteger = ^Integer */
    if (alias->is_pointer) {
        GpcType *pointee_type = NULL;
        int pointer_type_tag = alias->pointer_type;
        
        if (pointer_type_tag != UNKNOWN_TYPE) {
            /* Direct primitive type tag */
            pointee_type = create_primitive_type(pointer_type_tag);
        } else if (alias->pointer_type_id != NULL && symtab != NULL) {
            /* Type reference - try to resolve it */
            HashNode_t *pointee_node = NULL;
            if (FindIdent(&pointee_node, symtab, alias->pointer_type_id) >= 0 &&
                pointee_node != NULL && pointee_node->type != NULL) {
                /* Use the resolved type (don't clone, just reference) */
                pointee_type = pointee_node->type;
            } else {
                /* Forward reference or unresolved type
                 * Create a pointer to NULL - this is valid in Pascal
                 * The pointee type will be resolved later during usage */
                pointee_type = NULL;
            }
        }
        
        /* Create pointer type even if pointee is NULL (forward reference) */
        result = create_pointer_type(pointee_type);
        if (result != NULL) {
            gpc_type_set_type_alias(result, alias);
        }
        return result;
    }
    
    /* Handle set type aliases: type TCharSet = set of Char */
    if (alias->is_set) {
        /* For sets, we should ideally have alias->gpc_type already populated
         * But if not, we can create a primitive SET_TYPE */
        result = create_primitive_type(SET_TYPE);
        if (result != NULL) {
            gpc_type_set_type_alias(result, alias);
        }
        return result;
    }
    
    /* Handle enum type aliases: type TColor = (Red, Green, Blue) */
    if (alias->is_enum) {
        /* For enums, we should ideally have alias->gpc_type already populated
         * But if not, we can create a primitive ENUM_TYPE */
        result = create_primitive_type(ENUM_TYPE);
        if (result != NULL) {
            gpc_type_set_type_alias(result, alias);
        }
        return result;
    }
    
    /* Handle file type aliases: type TTextFile = file of Char */
    if (alias->is_file) {
        /* For files, create a primitive FILE_TYPE */
        result = create_primitive_type(FILE_TYPE);
        if (result != NULL) {
            gpc_type_set_type_alias(result, alias);
        }
        return result;
    }
    
    /* Handle simple type aliases: type MyInt = Integer */
    if (alias->base_type != UNKNOWN_TYPE && alias->target_type_id == NULL) {
        /* Simple primitive type alias */
        result = create_primitive_type(alias->base_type);
        if (result != NULL) {
            gpc_type_set_type_alias(result, alias);
        }
        return result;
    }
    
    /* Handle type reference aliases: type MyType = SomeOtherType */
    if (alias->target_type_id != NULL && symtab != NULL) {
        HashNode_t *target_node = NULL;
        if (FindIdent(&target_node, symtab, alias->target_type_id) >= 0 &&
            target_node != NULL && target_node->type != NULL) {
            /* Return the target's GpcType (reference, not clone) */
            gpc_type_retain(target_node->type);
            return target_node->type;
        }
    }
    
    /* If we couldn't resolve the type, return NULL */
    return NULL;
}

// --- Destructor Implementation ---

void gpc_type_retain(GpcType *type) {
    if (type == NULL)
        return;
    assert(type->ref_count > 0);
    type->ref_count++;
}

void destroy_gpc_type(GpcType *type) {
    if (type == NULL) return;
    assert(type->ref_count > 0);
    type->ref_count--;
    if (type->ref_count > 0)
        return;

    switch (type->kind) {
        case TYPE_KIND_POINTER:
            destroy_gpc_type(type->info.points_to);
            break;
        case TYPE_KIND_PROCEDURE:
            DestroyList(type->info.proc_info.params);
            destroy_gpc_type(type->info.proc_info.return_type);
            if (type->info.proc_info.return_type_id != NULL)
            {
                free(type->info.proc_info.return_type_id);
                type->info.proc_info.return_type_id = NULL;
            }
            break;
        case TYPE_KIND_ARRAY:
            destroy_gpc_type(type->info.array_info.element_type);
            break;
        case TYPE_KIND_ARRAY_OF_CONST:
            break;
        case TYPE_KIND_PRIMITIVE:
            break;
        case TYPE_KIND_RECORD:
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
    if (var_decl == NULL)
        return NULL;

    if (owns_type != NULL)
        *owns_type = 0;

    /* Handle inline array declarations: var x: array[1..20] of char */
    if (var_decl->type == TREE_ARR_DECL)
    {
        int start = var_decl->tree_data.arr_decl_data.s_range;
        int end = var_decl->tree_data.arr_decl_data.e_range;
        int elem_type_tag = var_decl->tree_data.arr_decl_data.type;
        char *elem_type_id = var_decl->tree_data.arr_decl_data.type_id;
        
        GpcType *elem_type = NULL;
        
        /* Resolve element type */
        if (elem_type_tag != UNKNOWN_TYPE && elem_type_tag != -1)
        {
            elem_type = create_primitive_type(elem_type_tag);
        }
        else if (elem_type_id != NULL && symtab != NULL)
        {
            /* Look up named element type in symbol table */
            struct HashNode *elem_node = NULL;
            if (FindIdent(&elem_node, symtab, elem_type_id) != -1 && 
                elem_node != NULL && elem_node->type != NULL)
            {
                elem_type = elem_node->type;
            }
        }
        
        if (elem_type != NULL)
        {
            /* Create a new array GpcType - caller owns this */
            if (owns_type != NULL)
                *owns_type = 1;
            return create_array_type(elem_type, start, end);
        }
        
        return NULL;
    }
    
    if (var_decl->type != TREE_VAR_DECL)
        return NULL;

    int var_type_tag = var_decl->tree_data.var_decl_data.type;

    /* Handle named type references using the symbol table */
    if ((var_type_tag == UNKNOWN_TYPE || var_type_tag == -1) && 
        var_decl->tree_data.var_decl_data.type_id != NULL && symtab != NULL) {
        /* Look up the named type in the symbol table */
        struct HashNode *type_node = NULL;
        if (FindIdent(&type_node, symtab, var_decl->tree_data.var_decl_data.type_id) != -1 && 
            type_node != NULL && type_node->type != NULL) {
            /* Return a shared reference from the symbol table - caller doesn't own it */
            if (owns_type != NULL)
                *owns_type = 0;
            gpc_type_retain(type_node->type);
            return type_node->type;
        }
        /* If we couldn't resolve the named type, return NULL */
        return NULL;
    }

    /* For primitive types, create a GpcType - caller owns this */
    if (var_type_tag != UNKNOWN_TYPE && var_type_tag != -1) {
        if (owns_type != NULL)
            *owns_type = 1;
        return create_primitive_type(var_type_tag);
    }

    return NULL;
}

/* Helper function to check if a record type is a subclass of another */
static int is_record_subclass(struct RecordType *subclass, struct RecordType *superclass, struct SymTab *symtab) {
    if (subclass == superclass)
        return 1;  /* Same type */

    /* Follow inheritance chain */
    struct RecordType *current = subclass;
    while (current != NULL && current->parent_class_name != NULL) {
        /* Look up parent class in symbol table */
        HashNode_t *parent_node = NULL;
        if (FindIdent(&parent_node, symtab, current->parent_class_name) != -1 && parent_node != NULL) {
            struct RecordType *parent_record = hashnode_get_record_type(parent_node);
            if (parent_record == superclass)
                return 1;
            current = parent_record;
        } else {
            break;
        }
    }

    return 0;
}

int are_types_compatible_for_assignment(GpcType *lhs_type, GpcType *rhs_type, struct SymTab *symtab) {
    /* NULL types are incompatible */
    if (lhs_type == NULL || rhs_type == NULL)
        return 0;

    /* Special case: Allow string (primitive) to be assigned to char array */
    /* This is a common Pascal idiom: var s: array[1..20] of char; begin s := 'hello'; end; */
    if (lhs_type->kind == TYPE_KIND_ARRAY &&
        lhs_type->info.array_info.element_type != NULL &&
        lhs_type->info.array_info.element_type->kind == TYPE_KIND_PRIMITIVE &&
        lhs_type->info.array_info.element_type->info.primitive_type_tag == CHAR_TYPE &&
        rhs_type->kind == TYPE_KIND_PRIMITIVE &&
        (rhs_type->info.primitive_type_tag == STRING_TYPE || rhs_type->info.primitive_type_tag == CHAR_TYPE))
    {
        /* String literals and single characters can be assigned to char arrays */
        /* Size checking should be done at a higher level where we have access to the actual string */
        return 1;
    }

    /* Special case: Allow char to be assigned to string */
    /* This is a common Pascal idiom: var s: string; begin s := 'a'; end; */
    if (lhs_type->kind == TYPE_KIND_PRIMITIVE &&
        lhs_type->info.primitive_type_tag == STRING_TYPE &&
        rhs_type->kind == TYPE_KIND_PRIMITIVE &&
        rhs_type->info.primitive_type_tag == CHAR_TYPE)
    {
        /* Single characters can be assigned to string variables */
        return 1;
    }

    /* Allow procedure variables to accept explicit @proc references */
    if (lhs_type->kind == TYPE_KIND_PROCEDURE && rhs_type->kind == TYPE_KIND_POINTER)
    {
        GpcType *rhs_proc = rhs_type->info.points_to;
        if (rhs_proc != NULL && rhs_proc->kind == TYPE_KIND_PROCEDURE)
            return are_types_compatible_for_assignment(lhs_type, rhs_proc, symtab);
        return 0;
    }
    if (lhs_type->kind == TYPE_KIND_POINTER && rhs_type->kind == TYPE_KIND_PROCEDURE)
    {
        GpcType *lhs_target = lhs_type->info.points_to;
        if (lhs_target == NULL)
            return 1; /* Generic Pointer can hold procedure addresses */
        if (lhs_target->kind == TYPE_KIND_PROCEDURE)
            return are_types_compatible_for_assignment(lhs_target, rhs_type, symtab);
        return 0;
    }

    /* If kinds are different, generally incompatible */
    /* Exception: we need to check for special cases */
    if (lhs_type->kind != rhs_type->kind) {
        /* Allow nil (represented as pointer) to be assigned to any pointer */
        /* This is a common Pascal idiom: var p: PNode; begin p := nil; end; */
        if (lhs_type->kind == TYPE_KIND_POINTER && rhs_type->kind == TYPE_KIND_POINTER) {
            /* Both are pointers, check if one is nil (points_to == NULL) */
            if (rhs_type->info.points_to == NULL) {
                /* rhs is nil, can be assigned to any pointer */
                return 1;
            }
            if (lhs_type->info.points_to == NULL) {
                /* lhs is nil, rhs pointer can be assigned */
                return 1;
            }
        }
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
        {
            int lhs_dynamic = lhs_type->info.array_info.end_index < lhs_type->info.array_info.start_index;
            int rhs_dynamic = rhs_type->info.array_info.end_index < rhs_type->info.array_info.start_index;
            if (!lhs_dynamic && !rhs_dynamic)
            {
                if (lhs_type->info.array_info.start_index != rhs_type->info.array_info.start_index)
                    return 0;
                if (lhs_type->info.array_info.end_index != rhs_type->info.array_info.end_index)
                    return 0;
            }
            return are_types_compatible_for_assignment(
                lhs_type->info.array_info.element_type,
                rhs_type->info.array_info.element_type,
                symtab);
        }

        case TYPE_KIND_RECORD:
            /* Records are compatible if they are the same record type 
             * or if one is a subclass of the other */
            if (lhs_type->info.record_info == rhs_type->info.record_info)
                return 1;

            /* Check inheritance: rhs_type should be assignable to lhs_type if
             * rhs_type is a subclass of lhs_type */
            if (is_record_subclass(rhs_type->info.record_info, lhs_type->info.record_info, symtab))
                return 1;

            return 0;

        case TYPE_KIND_PROCEDURE: {
            ProcedureTypeInfo *lhs_proc = &lhs_type->info.proc_info;
            ProcedureTypeInfo *rhs_proc = &rhs_type->info.proc_info;

            /* DEBUG: Log parameter counts */

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
            int param_position = 0;
            
            while (lhs_p != NULL && rhs_p != NULL) {
                ++param_position;
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

// --- Helper Function Implementations ---

long long gpc_type_sizeof(GpcType *type)
{
    if (type == NULL)
        return -1;
    
    switch (type->kind)
    {
        case TYPE_KIND_PRIMITIVE:
            switch (type->info.primitive_type_tag)
            {
                case INT_TYPE:
                case BOOL:
                case ENUM_TYPE:
                    return 4;
                case SET_TYPE:
                {
                    /* Check if this is a character set (set of char) */
                    if (type->type_alias != NULL && type->type_alias->is_set)
                    {
                        if (type->type_alias->set_element_type == CHAR_TYPE)
                        {
                            /* Character sets need 256 bits = 32 bytes */
                            return 32;
                        }
                    }
                    /* Regular sets (0-31 range) use 32 bits = 4 bytes */
                    return 4;
                }
                case LONGINT_TYPE:
                case REAL_TYPE:
                    return 8;
                case STRING_TYPE:
                case POINTER_TYPE:
                case PROCEDURE:
                case FILE_TYPE:
                    return 8; /* Pointers are 8 bytes on x86-64 */
                case CHAR_TYPE:
                    return 1;
                default:
                    return -1;
            }
        
        case TYPE_KIND_POINTER:
            return 8; /* Pointers are 8 bytes on x86-64 */
        
        case TYPE_KIND_ARRAY:
        {
            long long element_size = gpc_type_sizeof(type->info.array_info.element_type);
            if (element_size < 0)
                return -1;
            int count = type->info.array_info.end_index - type->info.array_info.start_index + 1;
            if (count < 0)
                return -1;
            return element_size * count;
        }
        
        case TYPE_KIND_ARRAY_OF_CONST:
            return type->info.array_of_const_info.element_size;
        
        case TYPE_KIND_RECORD:
            /* Record size would need to be computed by iterating fields
             * For now, we return -1 to indicate it needs special handling */
            return -1;
        
        case TYPE_KIND_PROCEDURE:
            return 8; /* Procedure pointers are 8 bytes */
        
        default:
            return -1;
    }
}

int gpc_type_is_array(GpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_ARRAY);
}

int gpc_type_is_array_of_const(GpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_ARRAY_OF_CONST);
}

int gpc_type_is_record(GpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_RECORD);
}

int gpc_type_is_procedure(GpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_PROCEDURE);
}

int gpc_type_get_array_bounds(GpcType *type, int *start_out, int *end_out)
{
    if (type == NULL || type->kind != TYPE_KIND_ARRAY)
        return -1;
    
    if (start_out != NULL)
        *start_out = type->info.array_info.start_index;
    if (end_out != NULL)
        *end_out = type->info.array_info.end_index;
    
    return 0;
}

struct RecordType* gpc_type_get_record(GpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_RECORD)
        return NULL;
    return type->info.record_info;
}

int gpc_type_get_primitive_tag(GpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_PRIMITIVE)
        return -1;
    return type->info.primitive_type_tag;
}

GpcType* gpc_type_get_array_element_type(GpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_ARRAY)
        return NULL;
    return type->info.array_info.element_type;
}

ListNode_t* gpc_type_get_procedure_params(GpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_PROCEDURE)
        return NULL;
    return type->info.proc_info.params;
}

GpcType* gpc_type_get_return_type(GpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_PROCEDURE)
        return NULL;
    return type->info.proc_info.return_type;
}

int gpc_type_is_dynamic_array(GpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_ARRAY)
        return 0;
    /* Dynamic/open arrays are represented with end < start (e.g., [0..-1]) */
    return (type->info.array_info.end_index < type->info.array_info.start_index);
}

long long gpc_type_get_array_element_size(GpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_ARRAY)
        return -1;
    
    GpcType *element_type = type->info.array_info.element_type;
    if (element_type == NULL)
        return -1;
    
    return gpc_type_sizeof(element_type);
}

/* Helper function to convert VarType enum to primitive type tag */
static int var_type_to_primitive_tag(enum VarType var_type)
{
    switch(var_type)
    {
        case HASHVAR_INTEGER:
            return INT_TYPE;
        case HASHVAR_LONGINT:
            return LONGINT_TYPE;
        case HASHVAR_REAL:
            return REAL_TYPE;
        case HASHVAR_BOOLEAN:
            return BOOL;
        case HASHVAR_CHAR:
            return CHAR_TYPE;
        case HASHVAR_PCHAR:
            return STRING_TYPE;
        case HASHVAR_SET:
            return SET_TYPE;
        case HASHVAR_FILE:
            return FILE_TYPE;
        case HASHVAR_ENUM:
            return ENUM_TYPE;
        default:
            return UNKNOWN_TYPE;
    }
}

long long gpc_type_get_array_of_const_element_size(GpcType *type)
{
    if (!gpc_type_is_array_of_const(type))
        return -1;
    return type->info.array_of_const_info.element_size;
}

/* Create a GpcType from a VarType enum value.
 * This is a helper for migrating from legacy type system.
 * Note: HASHVAR_ARRAY, HASHVAR_RECORD, HASHVAR_POINTER, HASHVAR_PROCEDURE
 * require additional information and cannot be created from VarType alone.
 * Returns NULL for these types - caller must use appropriate create_*_type() function.
 */
GpcType* gpc_type_from_var_type(enum VarType var_type)
{
    switch(var_type)
    {
        case HASHVAR_INTEGER:
        case HASHVAR_LONGINT:
        case HASHVAR_REAL:
        case HASHVAR_BOOLEAN:
        case HASHVAR_CHAR:
        case HASHVAR_PCHAR:
        case HASHVAR_SET:
        case HASHVAR_FILE:
        case HASHVAR_ENUM:
        {
            int tag = var_type_to_primitive_tag(var_type);
            return create_primitive_type(tag);
        }
        
        case HASHVAR_ARRAY:
        case HASHVAR_RECORD:
        case HASHVAR_POINTER:
        case HASHVAR_PROCEDURE:
        case HASHVAR_UNTYPED:
            /* These require additional information beyond VarType */
            return NULL;
            
        default:
            return NULL;
    }
}

/* Get the type alias metadata from a GpcType */
struct TypeAlias* gpc_type_get_type_alias(GpcType *type)
{
    if (type == NULL)
        return NULL;
    return type->type_alias;
}

/* Set the type alias metadata on a GpcType */
void gpc_type_set_type_alias(GpcType *type, struct TypeAlias *alias)
{
    assert(type != NULL && "Cannot set type_alias on NULL GpcType");
    type->type_alias = alias;
}

/* Get the legacy type tag from a GpcType */
int gpc_type_get_legacy_tag(GpcType *type)
{
    if (type == NULL)
        return UNKNOWN_TYPE;
    
    switch (type->kind) {
        case TYPE_KIND_PRIMITIVE:
            return type->info.primitive_type_tag;
        
        case TYPE_KIND_POINTER:
            return POINTER_TYPE;
        
        case TYPE_KIND_ARRAY:
            /* Arrays don't have a single legacy type tag in the old system.
             * They were identified by the is_array_expr flag on expressions.
             * Return UNKNOWN_TYPE to indicate this is an array type that needs
             * special handling via GpcType helpers like gpc_type_is_array(). */
            return UNKNOWN_TYPE;
        
        case TYPE_KIND_RECORD:
            return RECORD_TYPE;
        case TYPE_KIND_ARRAY_OF_CONST:
            return ARRAY_OF_CONST_TYPE;
        
        case TYPE_KIND_PROCEDURE:
            return PROCEDURE;
        
        default:
            return UNKNOWN_TYPE;
    }
}

/* Check if a GpcType represents a pointer type */
int gpc_type_is_pointer(GpcType *type)
{
    return type != NULL && type->kind == TYPE_KIND_POINTER;
}

/* For pointer types, get the type tag of what it points to */
int gpc_type_get_pointer_subtype_tag(GpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_POINTER)
        return UNKNOWN_TYPE;
    
    GpcType *points_to = type->info.points_to;
    if (points_to == NULL)
        return UNKNOWN_TYPE;
    
    /* Recursively get the type tag of what we point to */
    return gpc_type_get_legacy_tag(points_to);
}

/* Check if a GpcType requires qword (64-bit) operations */
int gpc_type_uses_qword(GpcType *type)
{
    if (type == NULL)
        return 0;
    
    switch (type->kind) {
        case TYPE_KIND_PRIMITIVE:
            switch (type->info.primitive_type_tag) {
                case LONGINT_TYPE:
                case REAL_TYPE:
                case STRING_TYPE:
                case FILE_TYPE:
                    return 1;
                default:
                    return 0;
            }
        
        case TYPE_KIND_POINTER:
            return 1;  /* Pointers are always 64-bit */
        
        case TYPE_KIND_PROCEDURE:
            return 1;  /* Procedure pointers are 64-bit */
        
        case TYPE_KIND_ARRAY:
            return gpc_type_is_dynamic_array(type);
        case TYPE_KIND_RECORD:
        default:
            return 0;
    }
}

/* Check if a GpcType represents a signed integer type */
int gpc_type_is_signed(GpcType *type)
{
    if (type == NULL)
        return 0;
    
    if (type->kind != TYPE_KIND_PRIMITIVE)
        return 0;
    
    switch (type->info.primitive_type_tag) {
        case INT_TYPE:
        case LONGINT_TYPE:
            return 1;
        default:
            return 0;
    }
}

/* Check if a GpcType matches a specific legacy type tag */
int gpc_type_equals_tag(GpcType *type, int type_tag)
{
    if (type == NULL)
        return (type_tag == UNKNOWN_TYPE);
    
    /* For primitives, pointers, records, and procedures, use legacy tag comparison */
    int legacy_tag = gpc_type_get_legacy_tag(type);
    return (legacy_tag == type_tag);
}

GpcType* gpc_type_build_function_return(struct TypeAlias *inline_alias,
                                        HashNode_t *resolved_type_node,
                                        int primitive_tag,
                                        SymTab_t *symtab)
{
    GpcType *result = NULL;

    if (inline_alias != NULL)
    {
        result = create_gpc_type_from_type_alias(inline_alias, symtab);
        if (result != NULL && result->type_alias == NULL)
            gpc_type_set_type_alias(result, inline_alias);
    }
    else if (resolved_type_node != NULL && resolved_type_node->type != NULL) {
        gpc_type_retain(resolved_type_node->type);
        result = resolved_type_node->type;
    }
    else if (resolved_type_node != NULL) {
        struct TypeAlias *alias = hashnode_get_type_alias(resolved_type_node);
        if (alias != NULL) {
            result = create_gpc_type_from_type_alias(alias, symtab);
        }
    }
    else if (primitive_tag != -1)
    {
        result = create_primitive_type(primitive_tag);
    }

    return result;
}
