/*
 * KgpcType.c
 * First-class type system implementation for the Kreijstal Gwinn Pascal Compiler
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#endif
#include "KgpcType.h"
#include "type_tags.h"
#include "tree_types.h"
#include "../../format_arg.h"
#include "../../identifier_utils.h"

/* Include symbol table headers for type resolution */
#include "../SemanticCheck/HashTable/HashTable.h"
#include "../SemanticCheck/SymTab/SymTab.h"

static HashNode_t *kgpc_find_type_node(SymTab_t *symtab, const char *type_id)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    /* Prefer type identifiers even if a variable with the same name exists. */
    ListNode_t *cur = symtab->stack_head;
    while (cur != NULL)
    {
        HashTable_t *table = (HashTable_t *)cur->cur;
        HashNode_t *node = FindIdentInTable(table, (char *)type_id);
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
            return node;
        cur = cur->next;
    }

    HashNode_t *builtin = FindIdentInTable(symtab->builtins, (char *)type_id);
    if (builtin != NULL && builtin->hash_type == HASHTYPE_TYPE)
        return builtin;

    return NULL;
}

/* Forward declarations for TypeAlias copy functions */
static struct TypeAlias* copy_type_alias(const struct TypeAlias *src);
static void free_copied_type_alias(struct TypeAlias *alias);

// --- Constructor Implementations ---

KgpcType* create_primitive_type(int primitive_tag) {
    KgpcType *type = (KgpcType *)calloc(1, sizeof(KgpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_PRIMITIVE;
    type->info.primitive_type_tag = primitive_tag;
    type->ref_count = 1;
    // Size will be determined later in semcheck
    return type;
}

KgpcType* create_primitive_type_with_size(int primitive_tag, int storage_size) {
    KgpcType *type = (KgpcType *)calloc(1, sizeof(KgpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_PRIMITIVE;
    type->info.primitive_type_tag = primitive_tag;
    type->ref_count = 1;
    
    /* Create a minimal type_alias just to hold the storage_size */
    struct TypeAlias *alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
    if (alias != NULL) {
        alias->storage_size = storage_size;
        /* These fields are required for proper cleanup */
        alias->base_type = primitive_tag;
        alias->is_array = 0;
        alias->is_pointer = 0;
        alias->is_set = 0;
        alias->is_enum = 0;
        alias->is_file = 0;
        alias->is_range = 0;
    }
    type->type_alias = alias;
    
    return type;
}

KgpcType* create_pointer_type(KgpcType *points_to) {
    KgpcType *type = (KgpcType *)calloc(1, sizeof(KgpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_POINTER;
    type->info.points_to = points_to; // Takes ownership of points_to
    type->ref_count = 1;
    return type;
}

KgpcType* create_procedure_type(ListNode_t *params, KgpcType *return_type) {
    KgpcType *type = (KgpcType *)calloc(1, sizeof(KgpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_PROCEDURE;
    type->info.proc_info.params = CopyListShallow(params); // Takes ownership of a copy
    type->info.proc_info.return_type = return_type; // Takes ownership
    type->info.proc_info.definition = NULL;
    type->info.proc_info.return_type_id = NULL;
    type->ref_count = 1;
    
    #ifdef DEBUG_KGPC_TYPE_CREATION
    fprintf(stderr, "DEBUG: create_procedure_type: params=%p, return_type=%p (is_function=%d)\n",
            (void*)params, (void*)return_type, return_type != NULL);
    #endif
    
    return type;
}

KgpcType* create_array_type(KgpcType *element_type, int start_index, int end_index) {
    KgpcType *type = (KgpcType *)calloc(1, sizeof(KgpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_ARRAY;
    type->info.array_info.element_type = element_type; // Takes ownership
    type->info.array_info.start_index = start_index;
    type->info.array_info.end_index = end_index;
    type->info.array_info.element_type_id = NULL; // Initialize deferred resolution field
    type->ref_count = 1;
    return type;
}

KgpcType* create_array_of_const_type(void) {
    KgpcType *type = (KgpcType *)calloc(1, sizeof(KgpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_ARRAY_OF_CONST;
    type->info.array_of_const_info.element_size = sizeof(kgpc_tvarrec);
    type->ref_count = 1;
    return type;
}

KgpcType* create_record_type(struct RecordType *record_info) {
    KgpcType *type = (KgpcType *)calloc(1, sizeof(KgpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_RECORD;
    type->info.record_info = record_info; // Record is owned by the AST
    type->ref_count = 1;
    return type;
}

/* Create KgpcType from TypeAlias structure
 * Handles ALL TypeAlias cases: arrays, pointers, sets, enums, files, primitives
 * Returns NULL if conversion fails (e.g., unresolvable type reference)
 */
KgpcType* create_kgpc_type_from_type_alias(struct TypeAlias *alias, struct SymTab *symtab) {
    if (alias == NULL) return NULL;
    
    /* If alias already has a KgpcType (enums, sets), use it */
    if (alias->kgpc_type != NULL) {
        /* If the KgpcType doesn't have type_alias set, set it now.
         * This is important for RawByteString/UnicodeString name mangling. */
        if (alias->kgpc_type->type_alias == NULL && alias->alias_name != NULL) {
            kgpc_type_set_type_alias(alias->kgpc_type, alias);
        }
        return alias->kgpc_type;
    }
    
    KgpcType *result = NULL;
    
    /* Handle array type aliases: type TIntArray = array[1..10] of Integer */
    if (alias->is_array) {
        int start = alias->array_start;
        int end = alias->array_end;

        if (alias->is_open_array) {
            start = 0;
            end = -1;
        }

        /* Resolve element type */
        KgpcType *element_type = NULL;
        int element_type_tag = alias->array_element_type;
        const char *deferred_element_id = NULL;

        if (element_type_tag != UNKNOWN_TYPE) {
            /* Direct primitive type tag */
            element_type = create_primitive_type(element_type_tag);
        } else if (alias->array_element_type_id != NULL && symtab != NULL) {
            /* Type reference - try to resolve it */
            HashNode_t *element_node = kgpc_find_type_node(symtab, alias->array_element_type_id);
            if (element_node != NULL && element_node->type != NULL) {
                /* Use the resolved type - MUST retain since it's borrowed from symbol table
                 * and create_array_type takes ownership. */
                element_type = element_node->type;
                kgpc_type_retain(element_type);
            } else {
                /* Forward reference - store element_type_id for deferred resolution */
                element_type = NULL;
                deferred_element_id = alias->array_element_type_id;
            }
        }

        /* Create array type even if element type is NULL (forward reference) */
        result = create_array_type(element_type, start, end);
        if (result != NULL) {
            kgpc_type_set_type_alias(result, alias);
            /* Store element type ID for deferred resolution if element_type is NULL */
            if (element_type == NULL && deferred_element_id != NULL) {
                result->info.array_info.element_type_id = strdup(deferred_element_id);
            }
        }
        return result;
    }
    
    /* Handle pointer type aliases: type PInteger = ^Integer */
    if (alias->is_pointer) {
        KgpcType *pointee_type = NULL;
        int pointer_type_tag = alias->pointer_type;

        if (pointer_type_tag != UNKNOWN_TYPE) {
            /* Direct primitive type tag */
            pointee_type = create_primitive_type(pointer_type_tag);
        } else if (alias->pointer_type_id != NULL && symtab != NULL) {
            /* Type reference - try to resolve it */
            HashNode_t *pointee_node = kgpc_find_type_node(symtab, alias->pointer_type_id);
            if (pointee_node != NULL && pointee_node->type != NULL) {
                /* Use the resolved type - MUST retain since it's borrowed from symbol table
                 * and create_pointer_type takes ownership. */
                pointee_type = pointee_node->type;
                kgpc_type_retain(pointee_type);
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
            kgpc_type_set_type_alias(result, alias);
        }
        return result;
    }
    
    /* Handle set type aliases: type TCharSet = set of Char */
    if (alias->is_set) {
        /* For sets, we should ideally have alias->kgpc_type already populated
         * But if not, we can create a primitive SET_TYPE */
        result = create_primitive_type(SET_TYPE);
        if (result != NULL) {
            kgpc_type_set_type_alias(result, alias);
        }
        return result;
    }
    
    /* Handle enum type aliases: type TColor = (Red, Green, Blue) */
    if (alias->is_enum) {
        /* For enums, we should ideally have alias->kgpc_type already populated
         * But if not, we can create a primitive ENUM_TYPE */
        result = create_primitive_type(ENUM_TYPE);
        if (result != NULL) {
            kgpc_type_set_type_alias(result, alias);
        }
        return result;
    }
    
    /* Handle file type aliases: type TTextFile = file of Char */
    if (alias->is_file) {
        /* For files, create a primitive FILE_TYPE */
        result = create_primitive_type(FILE_TYPE);
        if (result != NULL) {
            kgpc_type_set_type_alias(result, alias);
        }
        return result;
    }
    
    /* Handle simple type aliases: type MyInt = Integer */
    if (alias->base_type != UNKNOWN_TYPE && alias->target_type_id == NULL) {
        /* Simple primitive type alias */
        if (alias->storage_size > 0) {
            /* Preserve storage size if specified */
            result = create_primitive_type_with_size(alias->base_type, alias->storage_size);
        } else {
            result = create_primitive_type(alias->base_type);
        }
        if (result != NULL) {
            kgpc_type_set_type_alias(result, alias);
        }
        return result;
    }
    
    /* Handle type reference aliases: type MyType = SomeOtherType */
    if (alias->target_type_id != NULL && symtab != NULL) {
        HashNode_t *target_node = kgpc_find_type_node(symtab, alias->target_type_id);
        if (target_node != NULL && target_node->type != NULL) {
            /* Return the target's KgpcType (reference, not clone) */
            kgpc_type_retain(target_node->type);
            return target_node->type;
        }
    }
    
    /* If we couldn't resolve the type, return NULL */
    return NULL;
}

// --- Destructor Implementation ---

void kgpc_type_retain(KgpcType *type) {
    if (type == NULL)
        return;
    assert(type->ref_count > 0);
    type->ref_count++;
}

void kgpc_type_release(KgpcType *type) {
    destroy_kgpc_type(type);
}

void destroy_kgpc_type(KgpcType *type) {
    if (type == NULL) return;
    
    /* Defensive check: if ref_count is already 0, this indicates a double-free.
     * This can happen when a KgpcType is shared across multiple structures but
     * not properly retained, or when the same pointer is destroyed multiple times.
     * Instead of crashing, we log a warning and return safely. */
    if (type->ref_count <= 0) {
        static int warn_once = 0;
        if (getenv("KGPC_DEBUG_TYPE_FREE") != NULL) {
            fprintf(stderr,
                "[KgpcType] destroy_kgpc_type ref_count=%d type=%p kind=%d",
                type->ref_count, (void *)type, type->kind);
            if (type->type_alias != NULL) {
                if (type->type_alias->alias_name != NULL)
                    fprintf(stderr, " alias=%s", type->type_alias->alias_name);
                if (type->type_alias->target_type_id != NULL)
                    fprintf(stderr, " target=%s", type->type_alias->target_type_id);
            }
            if (type->kind == TYPE_KIND_RECORD &&
                type->info.record_info != NULL &&
                type->info.record_info->type_id != NULL)
                fprintf(stderr, " record=%s", type->info.record_info->type_id);
            fprintf(stderr, "\n");
        }
        if (!warn_once) {
            fprintf(stderr,
                "Warning: Attempting to destroy KgpcType with ref_count=%d (possible double-free)\n",
                type->ref_count);
            warn_once = 1;
        }
        return;
    }
    
    assert(type->ref_count > 0);
    type->ref_count--;
    if (type->ref_count > 0)
        return;

    switch (type->kind) {
        case TYPE_KIND_POINTER:
            destroy_kgpc_type(type->info.points_to);
            break;
        case TYPE_KIND_PROCEDURE:
            DestroyList(type->info.proc_info.params);
            destroy_kgpc_type(type->info.proc_info.return_type);
            if (type->info.proc_info.return_type_id != NULL)
            {
                free(type->info.proc_info.return_type_id);
                type->info.proc_info.return_type_id = NULL;
            }
            break;
        case TYPE_KIND_ARRAY:
            destroy_kgpc_type(type->info.array_info.element_type);
            if (type->info.array_info.element_type_id != NULL)
            {
                free(type->info.array_info.element_type_id);
                type->info.array_info.element_type_id = NULL;
            }
            break;
        case TYPE_KIND_ARRAY_OF_CONST:
            break;
        case TYPE_KIND_PRIMITIVE:
            break;
        case TYPE_KIND_RECORD:
            break;
    }
    
    /* Free the copied type_alias if it exists */
    if (type->type_alias != NULL) {
        if (type->type_alias->kgpc_type == type) {
            /* Avoid releasing self-referential aliases during teardown. */
            type->type_alias->kgpc_type = NULL;
        }
        free_copied_type_alias(type->type_alias);
        type->type_alias = NULL;
    }
    
    free(type);
}

// --- Utility Implementations ---

/* Helper function to check if a KgpcType is a char array (shortstring representation) */
static int is_char_array_type(KgpcType *type) {
    if (type == NULL || type->kind != TYPE_KIND_ARRAY)
        return 0;
    if (type->info.array_info.element_type == NULL)
        return 0;
    if (type->info.array_info.element_type->kind != TYPE_KIND_PRIMITIVE)
        return 0;
    return type->info.array_info.element_type->info.primitive_type_tag == CHAR_TYPE;
}

/* Helper function to check numeric type compatibility */
static int types_numeric_compatible(int lhs, int rhs) {
    /* Exact match */
    if (lhs == rhs)
        return 1;

    /* All integer types are compatible with each other */
    if ((lhs == INT_TYPE || lhs == LONGINT_TYPE || lhs == INT64_TYPE) &&
        (rhs == INT_TYPE || rhs == LONGINT_TYPE || rhs == INT64_TYPE))
        return 1;

    /* Real can accept any integer type */
    if (lhs == REAL_TYPE && (rhs == INT_TYPE || rhs == LONGINT_TYPE || rhs == INT64_TYPE))
        return 1;

    /* Integer can accept char (for compatibility) */
    if ((lhs == INT_TYPE || lhs == LONGINT_TYPE || lhs == INT64_TYPE) && rhs == CHAR_TYPE)
        return 1;

    return 0;
}

/* Helper function to resolve KgpcType from a parameter Tree_t node 
 * This is needed for procedure type compatibility checking */
KgpcType *resolve_type_from_vardecl(Tree_t *var_decl, struct SymTab *symtab, int *owns_type) {
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
        
        if (elem_type_tag == ARRAY_OF_CONST_TYPE)
        {
            if (owns_type != NULL)
                *owns_type = 1;
            return create_array_of_const_type();
        }

        KgpcType *elem_type = NULL;
        int elem_type_borrowed = 0;

        /* Resolve element type */
        if (elem_type_tag != UNKNOWN_TYPE && elem_type_tag != -1)
        {
            elem_type = create_primitive_type(elem_type_tag);
        }
        else if (elem_type_id != NULL && symtab != NULL)
        {
            /* Look up named element type in symbol table */
            struct HashNode *elem_node = kgpc_find_type_node(symtab, elem_type_id);
            if (elem_node != NULL && elem_node->type != NULL)
            {
                elem_type = elem_node->type;
                elem_type_borrowed = 1;
            }
        }

        if (elem_type != NULL)
        {
            /* CRITICAL: Retain elem_type if borrowed from symbol table
             * since create_array_type takes ownership. */
            if (elem_type_borrowed)
                kgpc_type_retain(elem_type);
            /* Create a new array KgpcType - caller owns this */
            if (owns_type != NULL)
                *owns_type = 1;
            return create_array_type(elem_type, start, end);
        }
        
        return NULL;
    }
    
    if (var_decl->type != TREE_VAR_DECL)
        return NULL;

    int var_type_tag = var_decl->tree_data.var_decl_data.type;
    const char *type_id = var_decl->tree_data.var_decl_data.type_id;

    if (var_type_tag == POINTER_TYPE && type_id != NULL)
    {
        KgpcType *pointee_type = NULL;
        int pointee_shared = 0;
        if (symtab != NULL)
        {
            struct HashNode *type_node = kgpc_find_type_node(symtab, type_id);
            if (type_node != NULL && type_node->type != NULL)
            {
                pointee_type = type_node->type;
                pointee_shared = 1;
            }
        }
        if (pointee_type == NULL)
        {
            int builtin_tag = UNKNOWN_TYPE;
            if (pascal_identifier_equals(type_id, "String") || pascal_identifier_equals(type_id, "AnsiString") ||
                pascal_identifier_equals(type_id, "RawByteString") ||
                pascal_identifier_equals(type_id, "UnicodeString") ||
                pascal_identifier_equals(type_id, "WideString"))
                builtin_tag = STRING_TYPE;
            else if (pascal_identifier_equals(type_id, "ShortString"))
                builtin_tag = SHORTSTRING_TYPE;
            else if (pascal_identifier_equals(type_id, "Integer"))
                builtin_tag = INT_TYPE;
            else if (pascal_identifier_equals(type_id, "LongInt"))
                builtin_tag = LONGINT_TYPE;
            else if (pascal_identifier_equals(type_id, "Int64") ||
                     pascal_identifier_equals(type_id, "SizeUInt") || pascal_identifier_equals(type_id, "QWord") ||
                     pascal_identifier_equals(type_id, "NativeUInt") || pascal_identifier_equals(type_id, "NativeInt") ||
                     pascal_identifier_equals(type_id, "PtrInt") || pascal_identifier_equals(type_id, "PtrUInt"))
                builtin_tag = INT64_TYPE;
            else if (pascal_identifier_equals(type_id, "Byte") || pascal_identifier_equals(type_id, "SmallInt") ||
                     pascal_identifier_equals(type_id, "Word"))
                builtin_tag = INT_TYPE;
            else if (pascal_identifier_equals(type_id, "Real") || pascal_identifier_equals(type_id, "Double"))
                builtin_tag = REAL_TYPE;
            else if (pascal_identifier_equals(type_id, "Char") || pascal_identifier_equals(type_id, "AnsiChar"))
                builtin_tag = CHAR_TYPE;
            else if (pascal_identifier_equals(type_id, "Boolean"))
                builtin_tag = BOOL;
            else if (pascal_identifier_equals(type_id, "Pointer"))
                builtin_tag = POINTER_TYPE;

            if (builtin_tag != UNKNOWN_TYPE)
                pointee_type = create_primitive_type(builtin_tag);
        }

        if (pointee_shared)
            kgpc_type_retain(pointee_type);
        if (owns_type != NULL)
            *owns_type = 1;
        return create_pointer_type(pointee_type);
    }

    /* Handle named type references using the symbol table */
    if (type_id != NULL && symtab != NULL) {
        /* Look up the named type in the symbol table */
        struct HashNode *type_node = kgpc_find_type_node(symtab, type_id);
        if (type_node != NULL && type_node->type != NULL) {
            /* Return a shared reference from the symbol table - caller doesn't own it */
            if (owns_type != NULL)
                *owns_type = 0;
            kgpc_type_retain(type_node->type);
            return type_node->type;
        }
        
        /* Fallback: Check for built-in type names not in symbol table */
        int builtin_tag = UNKNOWN_TYPE;
        
        /* String types */
        if (pascal_identifier_equals(type_id, "String") || pascal_identifier_equals(type_id, "AnsiString") ||
            pascal_identifier_equals(type_id, "RawByteString") ||
            pascal_identifier_equals(type_id, "UnicodeString") ||
            pascal_identifier_equals(type_id, "WideString")) {
            builtin_tag = STRING_TYPE;
        }
        else if (pascal_identifier_equals(type_id, "ShortString")) {
            builtin_tag = SHORTSTRING_TYPE;
        }
        /* Integer types */
        else if (pascal_identifier_equals(type_id, "Integer")) {
            builtin_tag = INT_TYPE;
        }
        else if (pascal_identifier_equals(type_id, "LongInt")) {
            builtin_tag = LONGINT_TYPE;  /* 32-bit for FPC compatibility */
        }
        else if (pascal_identifier_equals(type_id, "Int64") ||
                 pascal_identifier_equals(type_id, "SizeUInt") || pascal_identifier_equals(type_id, "QWord") ||
                 pascal_identifier_equals(type_id, "NativeUInt") || pascal_identifier_equals(type_id, "NativeInt") ||
                 pascal_identifier_equals(type_id, "PtrInt") || pascal_identifier_equals(type_id, "PtrUInt")) {
            builtin_tag = INT64_TYPE;  /* 64-bit integer types */
        }
        else if (pascal_identifier_equals(type_id, "Byte") || pascal_identifier_equals(type_id, "SmallInt") ||
                 pascal_identifier_equals(type_id, "Word")) {
            builtin_tag = INT_TYPE;
        }
        /* Other types */
        else if (pascal_identifier_equals(type_id, "Real") || pascal_identifier_equals(type_id, "Double")) {
            builtin_tag = REAL_TYPE;
        }
        else if (pascal_identifier_equals(type_id, "Char") || pascal_identifier_equals(type_id, "AnsiChar")) {
            builtin_tag = CHAR_TYPE;
        }
        else if (pascal_identifier_equals(type_id, "Boolean")) {
            builtin_tag = BOOL;
        }
        else if (pascal_identifier_equals(type_id, "Pointer")) {
            builtin_tag = POINTER_TYPE;
        }
        else if (pascal_identifier_equals(type_id, "File")) {
            builtin_tag = FILE_TYPE;
        }
        else if (pascal_identifier_equals(type_id, "Text") || pascal_identifier_equals(type_id, "TextFile")) {
            builtin_tag = TEXT_TYPE;
        }
        
        if (builtin_tag != UNKNOWN_TYPE) {
            if (owns_type != NULL)
                *owns_type = 1;
            return create_primitive_type(builtin_tag);
        }
        
        if (var_type_tag == UNKNOWN_TYPE || var_type_tag == -1)
        {
            /* If we couldn't resolve the named type, return NULL */
            return NULL;
        }
    }

    /* For primitive types, create a KgpcType - caller owns this */
    if (var_type_tag != UNKNOWN_TYPE && var_type_tag != -1) {
        if (owns_type != NULL)
            *owns_type = 1;
        return create_primitive_type(var_type_tag);
    }

    return NULL;
}

/* Helper function to check if a record type is a subclass of another */
/* Helper to get RecordType from HashNode, handling pointer types (classes) */
static struct RecordType* get_record_from_hashnode(HashNode_t *node) {
    if (node == NULL || node->type == NULL)
        return NULL;
    
    /* Direct record type */
    if (node->type->kind == TYPE_KIND_RECORD)
        return node->type->info.record_info;
    
    /* Pointer to record (class types) */
    if (node->type->kind == TYPE_KIND_POINTER) {
        KgpcType *pointed_to = node->type->info.points_to;
        if (pointed_to != NULL && pointed_to->kind == TYPE_KIND_RECORD)
            return pointed_to->info.record_info;
    }
    
    return NULL;
}

static int is_record_subclass(struct RecordType *subclass, struct RecordType *superclass, struct SymTab *symtab) {
    if (subclass == superclass)
        return 1;  /* Same type */

    /* Follow inheritance chain */
    struct RecordType *current = subclass;
    while (current != NULL && current->parent_class_name != NULL) {
        /* Look up parent class in symbol table */
        HashNode_t *parent_node = NULL;
        if (FindIdent(&parent_node, symtab, current->parent_class_name) != -1 && parent_node != NULL) {
            struct RecordType *parent_record = get_record_from_hashnode(parent_node);
            if (parent_record == superclass)
                return 1;
            current = parent_record;
        } else {
            break;
        }
    }

    return 0;
}

int are_types_compatible_for_assignment(KgpcType *lhs_type, KgpcType *rhs_type, struct SymTab *symtab) {
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

    /* Allow PChar <-> String/ShortString assignment */
    /* PChar is ^Char */
    int lhs_is_pchar = (lhs_type->kind == TYPE_KIND_POINTER && lhs_type->info.points_to != NULL &&
                        lhs_type->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
                        lhs_type->info.points_to->info.primitive_type_tag == CHAR_TYPE);
    int rhs_is_pchar = (rhs_type->kind == TYPE_KIND_POINTER && rhs_type->info.points_to != NULL &&
                        rhs_type->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
                        rhs_type->info.points_to->info.primitive_type_tag == CHAR_TYPE);
    int lhs_is_string = (lhs_type->kind == TYPE_KIND_PRIMITIVE && 
                         (lhs_type->info.primitive_type_tag == STRING_TYPE ||
                          lhs_type->info.primitive_type_tag == SHORTSTRING_TYPE));
    int rhs_is_string = (rhs_type->kind == TYPE_KIND_PRIMITIVE && 
                         (rhs_type->info.primitive_type_tag == STRING_TYPE ||
                          rhs_type->info.primitive_type_tag == SHORTSTRING_TYPE));

    if ((lhs_is_pchar && rhs_is_string) || (lhs_is_string && rhs_is_pchar)) {
        return 1;
    }

    /* Allow assigning procedure values to strings in helper conversions
     * (e.g., unresolved parameterless method references in array-of-const formatting). */
    if (lhs_is_string && rhs_type->kind == TYPE_KIND_PROCEDURE)
    {
        return 1;
    }
    if (lhs_is_string && rhs_type->kind == TYPE_KIND_PRIMITIVE &&
        rhs_type->info.primitive_type_tag == PROCEDURE)
    {
        return 1;
    }
    if (lhs_is_string && rhs_type->kind == TYPE_KIND_PRIMITIVE &&
        rhs_type->info.primitive_type_tag == POINTER_TYPE)
    {
        return 1;
    }

    /* Allow assigning generic pointers to strings to support PChar-style conversions
     * when the pointer subtype can't be resolved. */
    if (lhs_is_string && rhs_type->kind == TYPE_KIND_POINTER)
    {
        KgpcType *points_to = rhs_type->info.points_to;
        if (points_to == NULL)
            return 1;
        if (points_to->kind == TYPE_KIND_PRIMITIVE &&
            points_to->info.primitive_type_tag == CHAR_TYPE)
            return 1;
    }

    /* Allow String <-> ShortString assignment */
    if (lhs_is_string && rhs_is_string) {
        return 1;
    }

    /* Allow String/ShortString <-> array[0..255] of char assignment (ShortString compatibility) */
    /* Allow String/ShortString <-> array of char assignment (ShortString compatibility) */
    if (lhs_is_string && is_char_array_type(rhs_type))
        return 1;
    if (rhs_is_string && is_char_array_type(lhs_type))
        return 1;
    if (is_char_array_type(lhs_type) && rhs_is_pchar)
        return 1;
    if (is_char_array_type(rhs_type) && lhs_is_pchar)
        return 1;
    if (is_char_array_type(lhs_type) && is_char_array_type(rhs_type))
        return 1;
    if (is_char_array_type(lhs_type) && rhs_type->kind == TYPE_KIND_POINTER &&
        rhs_type->info.points_to == NULL)
        return 1;

    /* Allow procedure variables to accept explicit @proc references or NIL */
    if (lhs_type->kind == TYPE_KIND_PROCEDURE && rhs_type->kind == TYPE_KIND_POINTER)
    {
        KgpcType *rhs_proc = rhs_type->info.points_to;
        /* NIL can be assigned to any procedure type */
        if (rhs_proc == NULL)
            return 1;
        if (rhs_proc->kind == TYPE_KIND_PROCEDURE)
            return are_types_compatible_for_assignment(lhs_type, rhs_proc, symtab);
        return 0;
    }
    if (lhs_type->kind == TYPE_KIND_POINTER && rhs_type->kind == TYPE_KIND_PROCEDURE)
    {
        KgpcType *lhs_target = lhs_type->info.points_to;
        if (lhs_target == NULL)
            return 1; /* Generic Pointer can hold procedure addresses */
        if (lhs_target->kind == TYPE_KIND_PROCEDURE)
            return are_types_compatible_for_assignment(lhs_target, rhs_type, symtab);
        return 0;
    }

    /* Allow class instances to be compatible with untyped Pointer parameters 
     * This is needed for procedures like FreeAndNil(var Obj: Pointer) */
    if (lhs_type->kind == TYPE_KIND_POINTER && rhs_type->kind == TYPE_KIND_RECORD)
    {
        /* Check if rhs is a class (classes are always heap-allocated and pointer-compatible) */
        if (rhs_type->info.record_info != NULL && record_type_is_class(rhs_type->info.record_info))
        {
            /* Allow if lhs is untyped Pointer (points_to == NULL) */
            if (lhs_type->info.points_to == NULL)
                return 1;
        }
    }

    /* If kinds are different, generally incompatible */
    /* Exception: we need to check for special cases */
    if (lhs_type->kind != rhs_type->kind) {
        /* Allow array literals to be assigned to array of const parameters.
         * Any array (TYPE_KIND_ARRAY) can be passed to array of const (TYPE_KIND_ARRAY_OF_CONST). */
        if (lhs_type->kind == TYPE_KIND_ARRAY_OF_CONST && rhs_type->kind == TYPE_KIND_ARRAY) {
            return 1;  /* Array literal or array variable to array of const */
        }
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

        /* Allow nil (represented as pointer) to be assigned to dynamic arrays.
         * Dynamic arrays in Pascal can be assigned nil to clear/empty them.
         * This is a common Pascal idiom: var A: array of Integer; begin A := nil; end; */
        if (lhs_type->kind == TYPE_KIND_ARRAY && kgpc_type_is_dynamic_array(lhs_type) &&
            rhs_type->kind == TYPE_KIND_POINTER && rhs_type->info.points_to == NULL)
        {
            return 1;  /* nil can be assigned to any dynamic array */
        }
        
        /* Allow typed pointer (^T) to be assigned to untyped Pointer (primitive POINTER_TYPE) */
        /* This is needed for procedures like: procedure foo(p: Pointer); ... foo(@myInt64); */
        if (lhs_type->kind == TYPE_KIND_PRIMITIVE && 
            lhs_type->info.primitive_type_tag == POINTER_TYPE &&
            rhs_type->kind == TYPE_KIND_POINTER)
        {
            return 1;  /* Any typed pointer can be passed to an untyped Pointer parameter */
        }
        if (rhs_type->kind == TYPE_KIND_PRIMITIVE && 
            rhs_type->info.primitive_type_tag == POINTER_TYPE &&
            lhs_type->kind == TYPE_KIND_POINTER)
        {
            return 1;  /* Untyped Pointer can be assigned to any typed pointer */
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
            
            /* Special case for class references ("class of T"):
             * When both sides are pointers and at least one of them points to a pointer
             * to a record (indicating a class reference type), check if they point to
             * the same class or compatible classes.
             * 
             * Pattern: class of TClass = ^(^TClass_record)
             * Assigning: ClassRef := TClass, where TClass is ^TClass_record
             * 
             * We need to "unwrap" the extra indirection level for class references.
             */
            {
                KgpcType *lhs_inner = lhs_type->info.points_to;
                KgpcType *rhs_inner = rhs_type->info.points_to;
                
                /* Special case: LHS points to primitive(RECORD_TYPE) and RHS points to record.
                 * This happens with "class of T" types where the TypeInfo created a primitive
                 * type with RECORD_TYPE tag instead of an actual pointer to the class record.
                 * For class references, we need to verify the RHS is compatible with the target class. */
                if (lhs_inner != NULL && lhs_inner->kind == TYPE_KIND_PRIMITIVE &&
                    lhs_inner->info.primitive_type_tag == RECORD_TYPE &&
                    rhs_inner != NULL && rhs_inner->kind == TYPE_KIND_RECORD)
                {
                    /* LHS is "class of T" (represented as ^primitive(RECORD_TYPE))
                     * RHS is a class type (represented as ^record)
                     * Check if LHS has type_alias info to validate the target class */
                    struct TypeAlias *lhs_alias = lhs_type->type_alias;
                    if (lhs_alias != NULL && lhs_alias->pointer_type_id != NULL) {
                        /* LHS has target class info - validate compatibility */
                        struct RecordType *rhs_record = rhs_inner->info.record_info;
                        if (rhs_record != NULL && rhs_record->type_id != NULL) {
                            /* Check if RHS is the same as or subclass of target */
                            if (strcasecmp(lhs_alias->pointer_type_id, rhs_record->type_id) == 0) {
                                return 1;  /* Same class */
                            }
                            /* Check inheritance - if RHS has a parent, walk up to find if LHS target is an ancestor */
                            if (rhs_record->parent_class_name != NULL) {
                                /* Walk up parent chain to find if LHS target is an ancestor */
                                const char *parent = rhs_record->parent_class_name;
                                while (parent != NULL) {
                                    if (strcasecmp(lhs_alias->pointer_type_id, parent) == 0) {
                                        return 1;  /* RHS is subclass of LHS target */
                                    }
                                    /* Look up parent record to continue walking */
                                    struct HashNode *parent_node = NULL;
                                    if (symtab != NULL && FindIdent(&parent_node, symtab, (char*)parent) >= 0 &&
                                        parent_node != NULL && parent_node->type != NULL &&
                                        parent_node->type->kind == TYPE_KIND_POINTER &&
                                        parent_node->type->info.points_to != NULL &&
                                        parent_node->type->info.points_to->kind == TYPE_KIND_RECORD) {
                                        struct RecordType *parent_record = parent_node->type->info.points_to->info.record_info;
                                        if (parent_record != NULL) {
                                            parent = parent_record->parent_class_name;
                                        } else {
                                            parent = NULL;
                                        }
                                    } else {
                                        parent = NULL;
                                    }
                                }
                            }
                            return 0;  /* Incompatible classes */
                        }
                    }
                    /* No target class info or can't verify - allow (for backwards compatibility) */
                    return 1;
                }
                
                /* Symmetric case: both are ^primitive(RECORD_TYPE) - class ref to class ref */
                if (lhs_inner != NULL && lhs_inner->kind == TYPE_KIND_PRIMITIVE &&
                    lhs_inner->info.primitive_type_tag == RECORD_TYPE &&
                    rhs_inner != NULL && rhs_inner->kind == TYPE_KIND_PRIMITIVE &&
                    rhs_inner->info.primitive_type_tag == RECORD_TYPE)
                {
                    /* Both are class reference types - check type_alias for target compatibility */
                    struct TypeAlias *lhs_alias = lhs_type->type_alias;
                    struct TypeAlias *rhs_alias = rhs_type->type_alias;
                    if (lhs_alias != NULL && lhs_alias->pointer_type_id != NULL &&
                        rhs_alias != NULL && rhs_alias->pointer_type_id != NULL) {
                        /* Both have target class info - must be same or compatible */
                        if (strcasecmp(lhs_alias->pointer_type_id, rhs_alias->pointer_type_id) == 0) {
                            return 1;  /* Same target class */
                        }
                        /* Check subclass relationship for class references */
                        /* RHS must be a subclass of LHS (e.g., "class of TChild" assigned to "class of TParent" is allowed) */
                        if (symtab != NULL) {
                            struct HashNode *rhs_class_node = NULL;
                            if (FindIdent(&rhs_class_node, symtab, rhs_alias->pointer_type_id) >= 0 &&
                                rhs_class_node != NULL && rhs_class_node->type != NULL &&
                                rhs_class_node->type->kind == TYPE_KIND_POINTER &&
                                rhs_class_node->type->info.points_to != NULL &&
                                rhs_class_node->type->info.points_to->kind == TYPE_KIND_RECORD) {
                                struct RecordType *rhs_record = rhs_class_node->type->info.points_to->info.record_info;
                                if (rhs_record != NULL && rhs_record->parent_class_name != NULL) {
                                    /* Walk up parent chain to find if LHS target is an ancestor */
                                    const char *parent = rhs_record->parent_class_name;
                                    while (parent != NULL) {
                                        if (strcasecmp(lhs_alias->pointer_type_id, parent) == 0) {
                                            return 1;  /* RHS is class ref to subclass of LHS target */
                                        }
                                        /* Look up parent record to continue walking */
                                        struct HashNode *parent_node = NULL;
                                        if (FindIdent(&parent_node, symtab, (char*)parent) >= 0 &&
                                            parent_node != NULL && parent_node->type != NULL &&
                                            parent_node->type->kind == TYPE_KIND_POINTER &&
                                            parent_node->type->info.points_to != NULL &&
                                            parent_node->type->info.points_to->kind == TYPE_KIND_RECORD) {
                                            struct RecordType *parent_record = parent_node->type->info.points_to->info.record_info;
                                            if (parent_record != NULL) {
                                                parent = parent_record->parent_class_name;
                                            } else {
                                                parent = NULL;
                                            }
                                        } else {
                                            parent = NULL;
                                        }
                                    }
                                }
                            }
                        }
                        return 0;  /* Different target classes */
                    }
                    /* Fall through if no alias info */
                    return 1;
                }
                
                /* Check if LHS is ^(^record) and RHS is ^record */
                if (lhs_inner != NULL && lhs_inner->kind == TYPE_KIND_POINTER &&
                    rhs_inner != NULL && rhs_inner->kind == TYPE_KIND_RECORD)
                {
                    KgpcType *lhs_record_ptr = lhs_inner->info.points_to;
                    if (lhs_record_ptr != NULL && lhs_record_ptr->kind == TYPE_KIND_RECORD)
                    {
                        /* LHS is class of T (^(^record)), RHS is T (^record) */
                        /* Check if they point to the same or compatible records */
                        if (lhs_record_ptr->info.record_info == rhs_inner->info.record_info)
                            return 1;
                        if (is_record_subclass(rhs_inner->info.record_info, lhs_record_ptr->info.record_info, symtab))
                            return 1;
                    }
                }
                
                /* Check if both are ^(^record) - class reference to class reference */
                if (lhs_inner != NULL && lhs_inner->kind == TYPE_KIND_POINTER &&
                    rhs_inner != NULL && rhs_inner->kind == TYPE_KIND_POINTER)
                {
                    KgpcType *lhs_record = lhs_inner->info.points_to;
                    KgpcType *rhs_record = rhs_inner->info.points_to;
                    if (lhs_record != NULL && lhs_record->kind == TYPE_KIND_RECORD &&
                        rhs_record != NULL && rhs_record->kind == TYPE_KIND_RECORD)
                    {
                        /* Both are class references - check record compatibility */
                        if (lhs_record->info.record_info == rhs_record->info.record_info)
                            return 1;
                        if (is_record_subclass(rhs_record->info.record_info, lhs_record->info.record_info, symtab))
                            return 1;
                    }
                }
            }
            
            return are_types_compatible_for_assignment(
                lhs_type->info.points_to,
                rhs_type->info.points_to,
                symtab);

        case TYPE_KIND_ARRAY:
        {
            if (is_char_array_type(lhs_type) && is_char_array_type(rhs_type))
                return 1;
            int lhs_dynamic = lhs_type->info.array_info.end_index < lhs_type->info.array_info.start_index;
            int rhs_dynamic = rhs_type->info.array_info.end_index < rhs_type->info.array_info.start_index;
            if (!lhs_dynamic && !rhs_dynamic)
            {
                if (lhs_type->info.array_info.start_index != rhs_type->info.array_info.start_index)
                    return 0;
                if (lhs_type->info.array_info.end_index != rhs_type->info.array_info.end_index)
                    return 0;
            }
            
            /* Handle NULL element types - if both NULL, they're compatible (untyped arrays) */
            if (lhs_type->info.array_info.element_type == NULL && 
                rhs_type->info.array_info.element_type == NULL)
                return 1;
            
            /* If only one is NULL, check if indices match (for shortstring-like arrays) */
            if (lhs_type->info.array_info.element_type == NULL ||
                rhs_type->info.array_info.element_type == NULL)
            {
                /* Allow if array bounds match (for shortstring compatibility) */
                if (!lhs_dynamic && !rhs_dynamic)
                    return 1;
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
                KgpcType *lhs_param_type = resolve_type_from_vardecl(lhs_decl, symtab, &lhs_param_owned);
                KgpcType *rhs_param_type = resolve_type_from_vardecl(rhs_decl, symtab, &rhs_param_owned);

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
                    destroy_kgpc_type(lhs_param_type);
                if (rhs_param_owned && rhs_param_type != NULL)
                    destroy_kgpc_type(rhs_param_type);

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

const char* kgpc_type_to_string(KgpcType *type) {
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
            snprintf(buffer, sizeof(buffer), "^%s", kgpc_type_to_string(type->info.points_to));
            return buffer;
        case TYPE_KIND_ARRAY:
            snprintf(buffer, sizeof(buffer), "array[%d..%d] of %s",
                type->info.array_info.start_index,
                type->info.array_info.end_index,
                kgpc_type_to_string(type->info.array_info.element_type));
            return buffer;
        case TYPE_KIND_RECORD:
            return "record";
        case TYPE_KIND_PROCEDURE:
            if (type->info.proc_info.return_type == NULL) {
                return "procedure";
            } else {
                snprintf(buffer, sizeof(buffer), "function: %s",
                    kgpc_type_to_string(type->info.proc_info.return_type));
                return buffer;
            }
        default:
            return "unknown";
    }
}

// --- Helper Function Implementations ---

long long kgpc_type_sizeof(KgpcType *type)
{
    if (type == NULL)
        return -1;

    if (type->type_alias != NULL &&
        type->type_alias->storage_size > 0 &&
        type->kind == TYPE_KIND_PRIMITIVE)
    {
        return type->type_alias->storage_size;
    }
    
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
                        if (type->type_alias->set_element_type == CHAR_TYPE ||
                            (type->type_alias->set_element_type_id != NULL &&
                             (pascal_identifier_equals(type->type_alias->set_element_type_id, "Char") ||
                              pascal_identifier_equals(type->type_alias->set_element_type_id, "AnsiChar"))))
                        {
                            /* Character sets need 256 bits = 32 bytes */
                            return 32;
                        }
                    }
                    /* Regular sets (0-31 range) use 32 bits = 4 bytes */
                    return 4;
                }
                case LONGINT_TYPE:
                    return 4;  /* 32-bit for FPC-compatible LongInt */
                case INT64_TYPE:
                    return 8;  /* 64-bit Int64 */
                case REAL_TYPE:
                    return 8;
                case STRING_TYPE:
                case POINTER_TYPE:
                case PROCEDURE:
                    return 8; /* Pointers are 8 bytes on x86-64 */
                case FILE_TYPE:
                    if (type->size_in_bytes > 0)
                        return type->size_in_bytes;
                    return 368;
                case TEXT_TYPE:
                    if (type->size_in_bytes > 0)
                        return type->size_in_bytes;
                    return 632;
                case CHAR_TYPE:
                    return 1;
                default:
                    return -1;
            }
        
        case TYPE_KIND_POINTER:
            return 8; /* Pointers are 8 bytes on x86-64 */
        
        case TYPE_KIND_ARRAY:
        {
            long long element_size = kgpc_type_sizeof(type->info.array_info.element_type);
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
        {
            struct RecordType *record = type->info.record_info;
            if (record != NULL && record->has_cached_size)
                return record->cached_size;
            return -1;
        }
        
        case TYPE_KIND_PROCEDURE:
            return 8; /* Procedure pointers are 8 bytes */
        
        default:
            return -1;
    }
}

int kgpc_type_is_array(KgpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_ARRAY);
}

int kgpc_type_is_array_of_const(KgpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_ARRAY_OF_CONST);
}

int kgpc_type_is_record(KgpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_RECORD);
}

int kgpc_type_is_procedure(KgpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_PROCEDURE);
}

int kgpc_type_get_array_bounds(KgpcType *type, int *start_out, int *end_out)
{
    if (type == NULL || type->kind != TYPE_KIND_ARRAY)
        return -1;
    
    if (start_out != NULL)
        *start_out = type->info.array_info.start_index;
    if (end_out != NULL)
        *end_out = type->info.array_info.end_index;
    
    return 0;
}

struct RecordType* kgpc_type_get_record(KgpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_RECORD)
        return NULL;
    return type->info.record_info;
}

int kgpc_type_get_primitive_tag(KgpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_PRIMITIVE)
        return -1;
    return type->info.primitive_type_tag;
}

KgpcType* kgpc_type_get_array_element_type(KgpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_ARRAY)
        return NULL;
    return type->info.array_info.element_type;
}

KgpcType* kgpc_type_get_array_element_type_resolved(KgpcType *type, SymTab_t *symtab)
{
    if (type == NULL || type->kind != TYPE_KIND_ARRAY)
        return NULL;
    
    /* If element_type is already resolved, return it */
    if (type->info.array_info.element_type != NULL)
        return type->info.array_info.element_type;
    
    /* Try deferred resolution using element_type_id */
    if (type->info.array_info.element_type_id != NULL && symtab != NULL) {
        HashNode_t *element_node = kgpc_find_type_node(symtab, type->info.array_info.element_type_id);
        if (element_node != NULL && element_node->type != NULL) {
            /* Found the element type - update the array type and return it */
            type->info.array_info.element_type = element_node->type;
            kgpc_type_retain(element_node->type);
            /* Clear the deferred ID since we've resolved it */
            free(type->info.array_info.element_type_id);
            type->info.array_info.element_type_id = NULL;
            return type->info.array_info.element_type;
        }
    }
    
    return NULL;
}

ListNode_t* kgpc_type_get_procedure_params(KgpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_PROCEDURE)
        return NULL;
    return type->info.proc_info.params;
}

KgpcType* kgpc_type_get_return_type(KgpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_PROCEDURE)
        return NULL;
    return type->info.proc_info.return_type;
}

int kgpc_type_is_dynamic_array(KgpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_ARRAY)
        return 0;
    /* Dynamic/open arrays are represented with end < start (e.g., [0..-1]) */
    return (type->info.array_info.end_index < type->info.array_info.start_index);
}

long long kgpc_type_get_array_element_size(KgpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_ARRAY)
        return -1;
    
    KgpcType *element_type = type->info.array_info.element_type;
    if (element_type == NULL)
        return -1;
    
    return kgpc_type_sizeof(element_type);
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
        case HASHVAR_TEXT:
            return TEXT_TYPE;
        case HASHVAR_ENUM:
            return ENUM_TYPE;
        default:
            return UNKNOWN_TYPE;
    }
}

long long kgpc_type_get_array_of_const_element_size(KgpcType *type)
{
    if (!kgpc_type_is_array_of_const(type))
        return -1;
    return type->info.array_of_const_info.element_size;
}

/* Create a KgpcType from a VarType enum value.
 * This is a helper for migrating from legacy type system.
 * Note: HASHVAR_ARRAY, HASHVAR_RECORD, HASHVAR_POINTER, HASHVAR_PROCEDURE
 * require additional information and cannot be created from VarType alone.
 * Returns NULL for these types - caller must use appropriate create_*_type() function.
 */
KgpcType* kgpc_type_from_var_type(enum VarType var_type)
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
        case HASHVAR_TEXT:
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

/* Get the type alias metadata from a KgpcType */
/* Helper function to copy a list of strings */
static ListNode_t *copy_string_list(const ListNode_t *src)
{
    if (src == NULL)
        return NULL;
    
    ListNode_t *dst = NULL;
    ListNode_t *tail = NULL;
    const ListNode_t *cur = src;
    
    while (cur != NULL)
    {
        if (cur->type == LIST_STRING && cur->cur != NULL)
        {
            ListNode_t *new_node = (ListNode_t *)malloc(sizeof(ListNode_t));
            if (new_node == NULL)
            {
                /* Free already allocated nodes on failure */
                destroy_list(dst);
                return NULL;
            }
            new_node->type = LIST_STRING;
            new_node->cur = strdup((char *)cur->cur);
            new_node->next = NULL;
            
            if (dst == NULL)
            {
                dst = new_node;
                tail = new_node;
            }
            else
            {
                tail->next = new_node;
                tail = new_node;
            }
        }
        cur = cur->next;
    }
    
    return dst;
}

/* Copy a TypeAlias structure */
static struct TypeAlias* copy_type_alias(const struct TypeAlias *src)
{
    if (src == NULL)
        return NULL;
    
    struct TypeAlias *dst = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
    if (dst == NULL)
        return NULL;
    
    /* Copy simple fields */
    dst->base_type = src->base_type;
    dst->is_char_alias = src->is_char_alias;
    dst->is_array = src->is_array;
    dst->array_start = src->array_start;
    dst->array_end = src->array_end;
    dst->array_element_type = src->array_element_type;
    dst->is_shortstring = src->is_shortstring;
    dst->is_open_array = src->is_open_array;
    dst->is_pointer = src->is_pointer;
    dst->pointer_type = src->pointer_type;
    dst->is_set = src->is_set;
    dst->set_element_type = src->set_element_type;
    dst->is_enum = src->is_enum;
    dst->is_file = src->is_file;
    dst->file_type = src->file_type;
    dst->is_range = src->is_range;
    dst->range_known = src->range_known;
    dst->range_start = src->range_start;
    dst->range_end = src->range_end;
    dst->storage_size = src->storage_size;
    
    /* Copy string fields with duplication */
    if (src->alias_name != NULL)
        dst->alias_name = strdup(src->alias_name);
    if (src->target_type_id != NULL)
        dst->target_type_id = strdup(src->target_type_id);
    if (src->array_element_type_id != NULL)
        dst->array_element_type_id = strdup(src->array_element_type_id);
    if (src->pointer_type_id != NULL)
        dst->pointer_type_id = strdup(src->pointer_type_id);
    if (src->set_element_type_id != NULL)
        dst->set_element_type_id = strdup(src->set_element_type_id);
    if (src->file_type_id != NULL)
        dst->file_type_id = strdup(src->file_type_id);
    
    /* Deep copy lists */
    dst->array_dimensions = copy_string_list(src->array_dimensions);
    dst->enum_literals = copy_string_list(src->enum_literals);
    
    /* Copy inline_record_type - reference only for now (owned by AST) */
    dst->inline_record_type = src->inline_record_type;
    
    /* Copy kgpc_type with proper reference counting */
    if (src->kgpc_type != NULL) {
        kgpc_type_retain(src->kgpc_type);
        dst->kgpc_type = src->kgpc_type;
    }
    
    return dst;
}

/* Free a copied TypeAlias structure */
static void free_copied_type_alias(struct TypeAlias *alias)
{
    if (alias == NULL)
        return;
    
    free(alias->alias_name);
    free(alias->target_type_id);
    free(alias->array_element_type_id);
    free(alias->pointer_type_id);
    free(alias->set_element_type_id);
    free(alias->file_type_id);
    
    /* Free deep-copied lists */
    if (alias->array_dimensions != NULL)
        destroy_list(alias->array_dimensions);
    if (alias->enum_literals != NULL)
        destroy_list(alias->enum_literals);
    
    /* Note: We don't free inline_record_type as it's owned by AST */
    
    /* Save and NULL out kgpc_type before releasing to prevent infinite recursion
     * when the alias's kgpc_type points back to the type that owns this alias */
    if (alias->kgpc_type != NULL) {
        KgpcType *kgpc_type_to_release = alias->kgpc_type;
        alias->kgpc_type = NULL;  /* Break potential cycle before release */
        kgpc_type_release(kgpc_type_to_release);
    }
    
    free(alias);
}

struct TypeAlias* kgpc_type_get_type_alias(KgpcType *type)
{
    if (type == NULL)
        return NULL;
    return type->type_alias;
}

/* Set the type alias metadata on a KgpcType */
void kgpc_type_set_type_alias(KgpcType *type, struct TypeAlias *alias)
{
    assert(type != NULL && "Cannot set type_alias on NULL KgpcType");
    
    /* Free existing copied alias if any */
    if (type->type_alias != NULL) {
        free_copied_type_alias(type->type_alias);
        type->type_alias = NULL;
    }
    
    /* Copy the alias to make KgpcType own it, or set to NULL if alias is NULL */
    if (alias != NULL) {
        type->type_alias = copy_type_alias(alias);
    } else {
        type->type_alias = NULL;
    }
}

/* Get the legacy type tag from a KgpcType */
int kgpc_type_get_legacy_tag(KgpcType *type)
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
             * special handling via KgpcType helpers like kgpc_type_is_array(). */
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

/* Check if a KgpcType represents a pointer type */
int kgpc_type_is_pointer(KgpcType *type)
{
    return type != NULL && type->kind == TYPE_KIND_POINTER;
}

/* For pointer types, get the type tag of what it points to */
int kgpc_type_get_pointer_subtype_tag(KgpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_POINTER)
        return UNKNOWN_TYPE;
    
    KgpcType *points_to = type->info.points_to;
    if (points_to == NULL)
        return UNKNOWN_TYPE;
    
    /* Recursively get the type tag of what we point to */
    return kgpc_type_get_legacy_tag(points_to);
}

/* Check if a KgpcType requires qword (64-bit) operations */
int kgpc_type_uses_qword(KgpcType *type)
{
    if (type == NULL)
        return 0;

    if (type->type_alias != NULL && type->type_alias->storage_size > 0)
    {
        return (type->type_alias->storage_size > 4);
    }
    
    switch (type->kind) {
        case TYPE_KIND_PRIMITIVE:
            switch (type->info.primitive_type_tag) {
                case REAL_TYPE:
                case STRING_TYPE:
                case FILE_TYPE:
                case TEXT_TYPE:
                case POINTER_TYPE:
                case PROCEDURE:
                case INT64_TYPE:
                    return 1;
                default:
                    return 0;
            }
        
        case TYPE_KIND_POINTER:
            return 1;  /* Pointers are always 64-bit */
        
        case TYPE_KIND_PROCEDURE:
            return 1;  /* Procedure pointers are 64-bit */
        
        case TYPE_KIND_ARRAY:
            return kgpc_type_is_dynamic_array(type);
        case TYPE_KIND_RECORD:
        default:
            return 0;
    }
}

/* Check if a KgpcType represents a signed integer type */
int kgpc_type_is_signed(KgpcType *type)
{
    if (type == NULL)
        return 0;

    if (type->type_alias != NULL && type->type_alias->range_known)
        return (type->type_alias->range_start < 0);
    
    if (type->kind != TYPE_KIND_PRIMITIVE)
        return 0;
    
    switch (type->info.primitive_type_tag) {
        case INT_TYPE:
        case LONGINT_TYPE:
        case INT64_TYPE:
            return 1;
        default:
            return 0;
    }
}

/* Check if a KgpcType matches a specific legacy type tag */
int kgpc_type_equals_tag(KgpcType *type, int type_tag)
{
    if (type == NULL)
        return (type_tag == UNKNOWN_TYPE);
    
    /* For primitives, pointers, records, and procedures, use legacy tag comparison */
    int legacy_tag = kgpc_type_get_legacy_tag(type);
    return (legacy_tag == type_tag);
}

KgpcType* kgpc_type_build_function_return(struct TypeAlias *inline_alias,
                                        HashNode_t *resolved_type_node,
                                        int primitive_tag,
                                        SymTab_t *symtab)
{
    KgpcType *result = NULL;

    if (inline_alias != NULL)
    {
        result = create_kgpc_type_from_type_alias(inline_alias, symtab);
        if (result != NULL && result->type_alias == NULL)
            kgpc_type_set_type_alias(result, inline_alias);
    }
    else if (resolved_type_node != NULL && resolved_type_node->type != NULL) {
        kgpc_type_retain(resolved_type_node->type);
        result = resolved_type_node->type;
    }
    else if (resolved_type_node != NULL) {
        struct TypeAlias *alias = hashnode_get_type_alias(resolved_type_node);
        if (alias != NULL) {
            result = create_kgpc_type_from_type_alias(alias, symtab);
        }
    }
    else if (primitive_tag != -1)
    {
        result = create_primitive_type(primitive_tag);
    }

    return result;
}
