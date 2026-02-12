/*
 * KgpcType.c
 * First-class type system implementation for the Kreijstal Gwinn Pascal Compiler
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <errno.h>
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
        HashNode_t *node = FindIdentInTable(table, type_id);
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
            return node;
        cur = cur->next;
    }

    HashNode_t *builtin = FindIdentInTable(symtab->builtins, type_id);
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
    
    KgpcType *result = NULL;

    /* Treat WideChar/UnicodeChar aliases as 2-byte CHAR_TYPE, even if declared as Word. */
    if (alias->alias_name != NULL &&
        (pascal_identifier_equals(alias->alias_name, "WideChar") ||
         pascal_identifier_equals(alias->alias_name, "UnicodeChar")))
    {
        result = create_primitive_type_with_size(CHAR_TYPE, 2);
        if (result != NULL)
            kgpc_type_set_type_alias(result, alias);
        return result;
    }

    /* If alias already has a KgpcType (enums, sets), use it */
    if (alias->kgpc_type != NULL) {
        /* If the KgpcType doesn't have type_alias set, set it now.
         * This is important for RawByteString/UnicodeString name mangling. */
        if (alias->kgpc_type->type_alias == NULL && alias->alias_name != NULL) {
            kgpc_type_set_type_alias(alias->kgpc_type, alias);
        }
        return alias->kgpc_type;
    }
    
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

        if (pointer_type_tag == RECORD_TYPE && alias->pointer_type_id != NULL && symtab != NULL) {
            /* "class of T" or pointer-to-record: look up T's actual record type */
            HashNode_t *pointee_node = kgpc_find_type_node(symtab, alias->pointer_type_id);
            if (pointee_node != NULL && pointee_node->type != NULL) {
                pointee_type = pointee_node->type;
                kgpc_type_retain(pointee_type);
            } else {
                /* Target class not yet declared; create primitive placeholder */
                pointee_type = create_primitive_type(pointer_type_tag);
            }
        } else if (pointer_type_tag != UNKNOWN_TYPE) {
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
            if (type->info.array_info.element_type_id != NULL) {
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

/* ShortString bounds constants - Pascal ShortString is array[0..255] of Char */
#define SHORTSTRING_START_INDEX 0
#define SHORTSTRING_END_INDEX 255

/* Helper function to check if a KgpcType is a char array (shortstring representation) */
static int is_char_array_type(KgpcType *type) {
    if (type == NULL || type->kind != TYPE_KIND_ARRAY)
        return 0;
    if (type->info.array_info.element_type == NULL) {
        /* Check if this is a ShortString-like array (bounds 0..255) with NULL element_type */
        if (type->info.array_info.start_index == SHORTSTRING_START_INDEX && 
            type->info.array_info.end_index == SHORTSTRING_END_INDEX) {
            /* Treat arrays with 0..255 bounds and NULL element_type as char arrays */
            return 1;
        }
        return 0;
    }
    if (type->info.array_info.element_type->kind != TYPE_KIND_PRIMITIVE)
        return 0;
    int tag = type->info.array_info.element_type->info.primitive_type_tag;
    /* Accept CHAR_TYPE for regular char arrays */
    if (tag == CHAR_TYPE)
        return 1;
    /* Also accept STRING_TYPE and SHORTSTRING_TYPE for ShortString compatibility */
    /* ShortString is often represented as array[0..255] of String internally */
    if ((tag == STRING_TYPE || tag == SHORTSTRING_TYPE) &&
        type->info.array_info.start_index == SHORTSTRING_START_INDEX && 
        type->info.array_info.end_index == SHORTSTRING_END_INDEX) {
        return 1;
    }
    return 0;
}

/* Helper function to check numeric type compatibility */
static int types_numeric_compatible(int lhs, int rhs) {
    /* Exact match */
    if (lhs == rhs)
        return 1;

    /* All integer types are compatible with each other */
    if (is_integer_type(lhs) && is_integer_type(rhs))
        return 1;

    /* Real can accept any integer type */
    if (lhs == REAL_TYPE && is_integer_type(rhs))
        return 1;

    /* Integer can accept char (for compatibility) */
    if (is_integer_type(lhs) && rhs == CHAR_TYPE)
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

    if (var_type_tag == ARRAY_OF_CONST_TYPE)
    {
        if (owns_type != NULL)
            *owns_type = 1;
        return create_array_of_const_type();
    }

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
static struct RecordType* get_record_type_from_hashnode(HashNode_t *node) {
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
            struct RecordType *parent_record = get_record_type_from_hashnode(parent_node);
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

    /* Allow pointer-to-array to be assigned to pointer-to-element when element types match.
     * This supports idioms like: pchar := @char_array; */
    if (lhs_type->kind == TYPE_KIND_POINTER && rhs_type->kind == TYPE_KIND_POINTER)
    {
        KgpcType *lhs_points_to = lhs_type->info.points_to;
        KgpcType *rhs_points_to = rhs_type->info.points_to;
        if (lhs_points_to != NULL && rhs_points_to != NULL &&
            rhs_points_to->kind == TYPE_KIND_ARRAY)
        {
            KgpcType *rhs_elem = kgpc_type_get_array_element_type_resolved(rhs_points_to, symtab);
            if (rhs_elem != NULL && kgpc_type_equals(lhs_points_to, rhs_elem))
                return 1;
        }
    }

    /* Allow assigning procedure values to strings in helper conversions
     * (e.g., unresolved parameterless method references in array-of-const formatting). */
    if (lhs_is_string && rhs_type->kind == TYPE_KIND_PROCEDURE)
    {
        return 1;
    }

    /* Allow boolean to integer assignment (False=0, True=1) */
    if (lhs_type->kind == TYPE_KIND_PRIMITIVE && is_integer_type(lhs_type->info.primitive_type_tag) &&
        rhs_type->kind == TYPE_KIND_PRIMITIVE && rhs_type->info.primitive_type_tag == BOOL)
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
            /* Allow enum <-> integer compatibility for ordinal values. */
            if (lhs_type->info.primitive_type_tag == ENUM_TYPE &&
                rhs_type->info.primitive_type_tag != ENUM_TYPE &&
                is_integer_type(rhs_type->info.primitive_type_tag))
            {
                return 1;
            }
            if (rhs_type->info.primitive_type_tag == ENUM_TYPE &&
                lhs_type->info.primitive_type_tag != ENUM_TYPE &&
                is_integer_type(lhs_type->info.primitive_type_tag))
            {
                return 1;
            }
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

                /* Reverse case: LHS points to record and RHS points to primitive(RECORD_TYPE).
                 * This happens when one side was resolved to TYPE_KIND_RECORD by symtab lookup
                 * but the other side still has the unresolved primitive placeholder. */
                if (lhs_inner != NULL && lhs_inner->kind == TYPE_KIND_RECORD &&
                    rhs_inner != NULL && rhs_inner->kind == TYPE_KIND_PRIMITIVE &&
                    rhs_inner->info.primitive_type_tag == RECORD_TYPE)
                {
                    struct RecordType *lhs_record = lhs_inner->info.record_info;
                    struct TypeAlias *rhs_alias = rhs_type->type_alias;
                    if (lhs_record != NULL && lhs_record->type_id != NULL &&
                        rhs_alias != NULL && rhs_alias->pointer_type_id != NULL)
                    {
                        if (strcasecmp(lhs_record->type_id, rhs_alias->pointer_type_id) == 0)
                            return 1;  /* Same class */
                        /* Check if RHS target is a subclass of LHS record */
                        if (symtab != NULL) {
                            struct HashNode *rhs_class_node = NULL;
                            if (FindIdent(&rhs_class_node, symtab, rhs_alias->pointer_type_id) >= 0 &&
                                rhs_class_node != NULL && rhs_class_node->type != NULL &&
                                rhs_class_node->type->kind == TYPE_KIND_POINTER &&
                                rhs_class_node->type->info.points_to != NULL &&
                                rhs_class_node->type->info.points_to->kind == TYPE_KIND_RECORD) {
                                struct RecordType *rhs_record = rhs_class_node->type->info.points_to->info.record_info;
                                if (rhs_record != NULL && rhs_record->parent_class_name != NULL) {
                                    const char *parent = rhs_record->parent_class_name;
                                    while (parent != NULL) {
                                        if (strcasecmp(lhs_record->type_id, parent) == 0)
                                            return 1;
                                        struct HashNode *parent_node = NULL;
                                        if (FindIdent(&parent_node, symtab, (char*)parent) >= 0 &&
                                            parent_node != NULL && parent_node->type != NULL &&
                                            parent_node->type->kind == TYPE_KIND_POINTER &&
                                            parent_node->type->info.points_to != NULL &&
                                            parent_node->type->info.points_to->kind == TYPE_KIND_RECORD) {
                                            struct RecordType *parent_record = parent_node->type->info.points_to->info.record_info;
                                            parent = (parent_record != NULL) ? parent_record->parent_class_name : NULL;
                                        } else {
                                            parent = NULL;
                                        }
                                    }
                                }
                            }
                        }
                        /* Check if LHS record is a subclass of RHS target */
                        if (lhs_record->parent_class_name != NULL) {
                            const char *parent = lhs_record->parent_class_name;
                            while (parent != NULL) {
                                if (strcasecmp(rhs_alias->pointer_type_id, parent) == 0)
                                    return 1;
                                struct HashNode *parent_node = NULL;
                                if (symtab != NULL &&
                                    FindIdent(&parent_node, symtab, (char*)parent) >= 0 &&
                                    parent_node != NULL && parent_node->type != NULL &&
                                    parent_node->type->kind == TYPE_KIND_POINTER &&
                                    parent_node->type->info.points_to != NULL &&
                                    parent_node->type->info.points_to->kind == TYPE_KIND_RECORD) {
                                    struct RecordType *parent_record = parent_node->type->info.points_to->info.record_info;
                                    parent = (parent_record != NULL) ? parent_record->parent_class_name : NULL;
                                } else {
                                    parent = NULL;
                                }
                            }
                        }
                        return 0;  /* Incompatible classes */
                    }
                    /* No type info to compare - allow for backwards compatibility */
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
            
            /* Check element type compatibility */
            int elem_compatible = are_types_compatible_for_assignment(
                lhs_type->info.array_info.element_type,
                rhs_type->info.array_info.element_type,
                symtab);
            if (elem_compatible)
                return 1;
            
            /* Fallback: If element types have same string representation, treat as compatible */
            /* This handles cases where types are structurally identical but represented differently */
            const char *lhs_elem_str = kgpc_type_to_string(lhs_type->info.array_info.element_type);
            const char *rhs_elem_str = kgpc_type_to_string(rhs_type->info.array_info.element_type);
            if (lhs_elem_str != NULL && rhs_elem_str != NULL && 
                strcasecmp(lhs_elem_str, rhs_elem_str) == 0)
                return 1;
            
            return 0;
        }
        case TYPE_KIND_ARRAY_OF_CONST:
            return 1;

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

            /* 1. Check function vs procedure compatibility 
             * A procedure variable can only hold a procedure, not a function, and vice versa 
             * Note: A function has either return_type != NULL or return_type_id != NULL */
            int lhs_is_function = (lhs_proc->return_type != NULL || lhs_proc->return_type_id != NULL);
            int rhs_is_function = (rhs_proc->return_type != NULL || rhs_proc->return_type_id != NULL);
            
            if (lhs_is_function != rhs_is_function)
                return 0; /* Cannot assign function to procedure var or vice versa */

            /* 2. If both are functions, check return types */
            if (lhs_is_function) {
                /* Handle case where return_type is NULL but return_type_id is set */
                if (lhs_proc->return_type != NULL && rhs_proc->return_type != NULL) {
                    if (!are_types_compatible_for_assignment(
                            lhs_proc->return_type,
                            rhs_proc->return_type,
                            symtab))
                        return 0;
                } else if (lhs_proc->return_type_id != NULL && rhs_proc->return_type_id != NULL) {
                    /* Both have return_type_id but no resolved return_type - compare by name */
                    if (strcasecmp(lhs_proc->return_type_id, rhs_proc->return_type_id) != 0)
                        return 0;
                } else if (lhs_proc->return_type != NULL && rhs_proc->return_type_id != NULL) {
                    /* LHS has resolved type, RHS has type_id - compare by string representation */
                    const char *lhs_str = kgpc_type_to_string(lhs_proc->return_type);
                    if (lhs_str == NULL || strcasecmp(lhs_str, rhs_proc->return_type_id) != 0)
                        return 0;
                } else if (lhs_proc->return_type_id != NULL && rhs_proc->return_type != NULL) {
                    /* LHS has type_id, RHS has resolved type - compare by string representation */
                    const char *rhs_str = kgpc_type_to_string(rhs_proc->return_type);
                    if (rhs_str == NULL || strcasecmp(lhs_proc->return_type_id, rhs_str) != 0)
                        return 0;
                }
                /* If all above checks pass or fall through, types are compatible */
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
    /* Use multiple static buffers to handle recursive calls.
     * Each recursive level uses a different buffer to prevent
     * the inner call from overwriting the outer call's data. */
    #define TYPE_STRING_BUFFER_COUNT 8
    #define TYPE_STRING_BUFFER_SIZE 256
    static char buffers[TYPE_STRING_BUFFER_COUNT][TYPE_STRING_BUFFER_SIZE];
    static int buffer_index = 0;
    
    char *buffer = buffers[buffer_index];
    buffer_index = (buffer_index + 1) % TYPE_STRING_BUFFER_COUNT;
    
    if (type == NULL) {
        return "NULL";
    }
    
    switch (type->kind) {
        case TYPE_KIND_PRIMITIVE:
            switch (type->info.primitive_type_tag) {
                case INT_TYPE: return "Integer";
                case REAL_TYPE: return "Real";
                case LONGINT_TYPE: return "LongInt";
                case STRING_TYPE: return "String";
                case CHAR_TYPE: return "Char";
                case BOOL: return "Boolean";
                case POINTER_TYPE: return "Pointer";
                case SET_TYPE: return "Set";
                case ENUM_TYPE: return "Enum";
                case FILE_TYPE: return "File";
                case UNKNOWN_TYPE: return "Unknown";
                case BUILTIN_ANY_TYPE: return "Any";
                case INT64_TYPE: return "Int64";
                case RECORD_TYPE: return "Record";
                case ARRAY_OF_CONST_TYPE: return "array of const";
                case TEXT_TYPE: return "Text";
                case SHORTSTRING_TYPE: return "ShortString";
                case BYTE_TYPE: return "Byte";
                case WORD_TYPE: return "Word";
                case LONGWORD_TYPE: return "LongWord";
                case QWORD_TYPE: return "QWord";
                default:
                    snprintf(buffer, TYPE_STRING_BUFFER_SIZE, "primitive(%d)", type->info.primitive_type_tag);
                    return buffer;
            }
        case TYPE_KIND_POINTER:
            snprintf(buffer, TYPE_STRING_BUFFER_SIZE, "^%s", kgpc_type_to_string(type->info.points_to));
            return buffer;
        case TYPE_KIND_ARRAY:
            snprintf(buffer, TYPE_STRING_BUFFER_SIZE, "array[%d..%d] of %s",
                type->info.array_info.start_index,
                type->info.array_info.end_index,
                kgpc_type_to_string(type->info.array_info.element_type));
            return buffer;
        case TYPE_KIND_ARRAY_OF_CONST:
            return "array of const";
        case TYPE_KIND_RECORD:
            return "record";
        case TYPE_KIND_PROCEDURE:
            if (type->info.proc_info.return_type == NULL) {
                /* If return_type is NULL but return_type_id is set, show it as a function */
                if (type->info.proc_info.return_type_id != NULL) {
                    snprintf(buffer, TYPE_STRING_BUFFER_SIZE, "function: %s",
                        type->info.proc_info.return_type_id);
                    return buffer;
                }
                return "procedure";
            } else {
                snprintf(buffer, TYPE_STRING_BUFFER_SIZE, "function: %s",
                    kgpc_type_to_string(type->info.proc_info.return_type));
                return buffer;
            }
        default:
            return "unknown";
    }
    #undef TYPE_STRING_BUFFER_COUNT
    #undef TYPE_STRING_BUFFER_SIZE
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
                case SHORTSTRING_TYPE:
                    return 256; /* length byte + 255 chars */
                case FILE_TYPE:
                    if (type->size_in_bytes > 0)
                        return type->size_in_bytes;
                    return 368;
                case TEXT_TYPE:
                    if (type->size_in_bytes > 0)
                        return type->size_in_bytes;
                    return 632;
                case CHAR_TYPE:
                case BYTE_TYPE:
                    return 1;
                case WORD_TYPE:
                    return 2;
                case LONGWORD_TYPE:
                    return 4;
                case QWORD_TYPE:
                    return 8;
                default:
                    return -1;
            }
        
        case TYPE_KIND_POINTER:
            return 8; /* Pointers are 8 bytes on x86-64 */
        
        case TYPE_KIND_ARRAY:
        {
            if (kgpc_type_is_dynamic_array(type))
                return 8;
            /* Use the KgpcType's concrete start_index/end_index.
             * Note: For multi-dimensional arrays defined via TypeAlias, the size computation
             * might be incorrect here because element_type may not be nested arrays.
             * Callers with access to symtab should use kgpc_type_get_array_dimension_info
             * for accurate multi-dimensional array sizes. */
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

int kgpc_type_is_array(const KgpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_ARRAY);
}

int kgpc_type_is_array_of_const(const KgpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_ARRAY_OF_CONST);
}

int kgpc_type_is_pointer(const KgpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_POINTER);
}

int kgpc_type_is_record(const KgpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_RECORD);
}

int kgpc_type_is_procedure(const KgpcType *type)
{
    return (type != NULL && type->kind == TYPE_KIND_PROCEDURE);
}

int kgpc_type_is_char(const KgpcType *type)
{
    return (type != NULL &&
        type->kind == TYPE_KIND_PRIMITIVE &&
        type->info.primitive_type_tag == CHAR_TYPE);
}

int kgpc_type_is_string(const KgpcType *type)
{
    if (type == NULL)
        return 0;
    if (type->kind == TYPE_KIND_PRIMITIVE &&
        (type->info.primitive_type_tag == STRING_TYPE ||
         type->info.primitive_type_tag == SHORTSTRING_TYPE))
    {
        return 1;
    }
    /* Also treat ShortString arrays as strings */
    if (type->kind == TYPE_KIND_ARRAY && type->type_alias != NULL &&
        type->type_alias->is_shortstring)
    {
        return 1;
    }
    return 0;
}

int kgpc_type_is_shortstring(const KgpcType *type)
{
    if (type == NULL)
        return 0;
    if (type->kind == TYPE_KIND_PRIMITIVE &&
        type->info.primitive_type_tag == SHORTSTRING_TYPE)
    {
        return 1;
    }
    if (type->kind == TYPE_KIND_ARRAY && type->type_alias != NULL &&
        type->type_alias->is_shortstring)
    {
        return 1;
    }
    return 0;
}

int kgpc_type_is_integer(const KgpcType *type)
{
    return (type != NULL &&
        type->kind == TYPE_KIND_PRIMITIVE &&
        is_integer_type(type->info.primitive_type_tag));
}

int kgpc_type_is_real(const KgpcType *type)
{
    return (type != NULL &&
        type->kind == TYPE_KIND_PRIMITIVE &&
        type->info.primitive_type_tag == REAL_TYPE);
}

int kgpc_type_is_numeric(const KgpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_PRIMITIVE)
        return 0;
    if (is_integer_type(type->info.primitive_type_tag))
        return 1;
    return (type->info.primitive_type_tag == REAL_TYPE);
}

int kgpc_type_is_boolean(const KgpcType *type)
{
    return (type != NULL &&
        type->kind == TYPE_KIND_PRIMITIVE &&
        type->info.primitive_type_tag == BOOL);
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

static int kgpc_parse_array_bound(struct SymTab *symtab, const char *token, long long *out_value)
{
    if (out_value == NULL || token == NULL)
        return -1;
    while (*token && isspace((unsigned char)*token)) token++;
    if (*token == '\0')
        return -1;

    char *endptr = NULL;
    errno = 0;
    long long val = strtoll(token, &endptr, 10);
    if (endptr != token)
    {
        while (*endptr && isspace((unsigned char)*endptr)) endptr++;
        if (*endptr == '\0' && errno != ERANGE)
        {
            *out_value = val;
            return 0;
        }
    }

    if (symtab != NULL)
    {
        /* Trim the token for lookup */
        char *trimmed = strdup(token);
        if (trimmed == NULL)
            return -1;

        char *end = trimmed + strlen(trimmed) - 1;
        while (end >= trimmed && isspace((unsigned char)*end)) *end-- = '\0';

        HashNode_t *node = NULL;
        if (FindIdent(&node, symtab, trimmed) >= 0 && node != NULL &&
            (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
        {
            *out_value = node->const_int_value;
            free(trimmed);
            return 0;
        }
        free(trimmed);
    }
    return -1;
}

int kgpc_type_get_array_dimension_info(KgpcType *type, struct SymTab *symtab, KgpcArrayDimensionInfo *info)
{
    if (type == NULL || info == NULL || type->kind != TYPE_KIND_ARRAY)
        return -1;

    memset(info, 0, sizeof(KgpcArrayDimensionInfo));
    struct TypeAlias *alias = kgpc_type_get_type_alias(type);

    if (alias != NULL && alias->array_dimensions != NULL)
    {
        /* Use dimensions metadata from TypeAlias */
        ListNode_t *dim_node = alias->array_dimensions;
        while (dim_node != NULL && info->dim_count < 10)
        {
            const char *range_str = (const char *)dim_node->cur;
            int dim_parsed = 0;
            if (range_str != NULL)
            {
                const char *dotdot = strstr(range_str, "..");
                if (dotdot != NULL)
                {
                    size_t left_len = (size_t)(dotdot - range_str);
                    char *left = (char *)malloc(left_len + 1);
                    if (left != NULL)
                    {
                        memcpy(left, range_str, left_len);
                        left[left_len] = '\0';
                        long long lower = 0;
                        long long upper = 0;
                        if (kgpc_parse_array_bound(symtab, left, &lower) != 0)
                        {
                            free(left);
                            return -1;
                        }
                        free(left);
                        if (kgpc_parse_array_bound(symtab, dotdot + 2, &upper) != 0)
                            return -1;

                        long long size = upper - lower + 1;
                        if (size <= 0)
                            return -1;

                        info->dim_lowers[info->dim_count] = lower;
                        info->dim_uppers[info->dim_count] = upper;
                        info->dim_sizes[info->dim_count] = size;
                        dim_parsed = 1;
                    }
                    else
                    {
                        /* malloc failure: this is a critical error, but we return error from function */
                        return -1;
                    }
                }
                else if (pascal_identifier_equals(range_str, "Boolean"))
                {
                    info->dim_lowers[info->dim_count] = 0;
                    info->dim_uppers[info->dim_count] = 1;
                    info->dim_sizes[info->dim_count] = 2;
                    dim_parsed = 1;
                }
                else if (symtab != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, range_str) >= 0 &&
                        type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                    {
                        struct TypeAlias *range_alias = hashnode_get_type_alias(type_node);
                        if (range_alias != NULL)
                        {
                            if (range_alias->is_enum && range_alias->enum_literals != NULL)
                            {
                                info->dim_lowers[info->dim_count] = 0;
                                info->dim_sizes[info->dim_count] = (long long)ListLength(range_alias->enum_literals);
                                info->dim_uppers[info->dim_count] = info->dim_sizes[info->dim_count] - 1;
                                if (info->dim_sizes[info->dim_count] <= 0)
                                    return -1;
                                dim_parsed = 1;
                            }
                            else if (range_alias->is_range && range_alias->range_known)
                            {
                                long long lower = range_alias->range_start;
                                long long upper = range_alias->range_end;
                                long long size = upper - lower + 1;
                                if (size <= 0)
                                    return -1;

                                info->dim_lowers[info->dim_count] = lower;
                                info->dim_uppers[info->dim_count] = upper;
                                info->dim_sizes[info->dim_count] = size;
                                dim_parsed = 1;
                            }
                        }
                    }
                }
            }
            if (dim_parsed)
                info->dim_count++;
            dim_node = dim_node->next;
        }

        info->element_size = kgpc_type_get_array_element_size(type);
        if (info->element_size <= 0 && alias->array_element_type_id != NULL && symtab != NULL)
        {
            HashNode_t *node = kgpc_find_type_node(symtab, alias->array_element_type_id);
            if (node != NULL && node->type != NULL)
                info->element_size = kgpc_type_sizeof(node->type);
        }
    }
    else
    {
        /* Traverse nested KgpcType objects */
        KgpcType *curr = type;
        while (curr != NULL && curr->kind == TYPE_KIND_ARRAY && info->dim_count < 10)
        {
            info->dim_lowers[info->dim_count] = curr->info.array_info.start_index;
            info->dim_uppers[info->dim_count] = curr->info.array_info.end_index;
            info->dim_sizes[info->dim_count] = info->dim_uppers[info->dim_count] - info->dim_lowers[info->dim_count] + 1;
            info->dim_count++;
            curr = curr->info.array_info.element_type;
        }
        if (curr != NULL)
            info->element_size = kgpc_type_sizeof(curr);
    }

    if (info->dim_count == 0)
        return -1;

    if (info->element_size <= 0)
        info->element_size = 1;

    /* Compute strides and total size */
    info->total_size = info->element_size;
    for (int i = info->dim_count - 1; i >= 0; i--)
    {
        info->strides[i] = info->total_size;
        if (info->dim_sizes[i] > 0)
        {
            if (info->total_size > LLONG_MAX / info->dim_sizes[i])
                return -1;
            info->total_size *= info->dim_sizes[i];
        }
    }

    return 0;
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
            /* Free element_type_id since resolution is complete */
            if (type->info.array_info.element_type_id != NULL) {
                free(type->info.array_info.element_type_id);
                type->info.array_info.element_type_id = NULL;
            }
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

int kgpc_type_is_dynamic_array(const KgpcType *type)
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
        case HASHVAR_SHORTSTRING:
            return SHORTSTRING_TYPE;
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
        case HASHVAR_SHORTSTRING:
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

/* For pointer types, get the type tag of what it points to */
int kgpc_type_get_pointer_subtype_tag(KgpcType *type)
{
    if (type == NULL || type->kind != TYPE_KIND_POINTER)
        return UNKNOWN_TYPE;
    
    KgpcType *points_to = type->info.points_to;
    if (points_to == NULL)
        return UNKNOWN_TYPE;
    
    if (points_to->kind == TYPE_KIND_PRIMITIVE)
        return points_to->info.primitive_type_tag;
    if (points_to->kind == TYPE_KIND_RECORD)
        return RECORD_TYPE;
    if (points_to->kind == TYPE_KIND_POINTER)
        return POINTER_TYPE;
    if (points_to->kind == TYPE_KIND_PROCEDURE)
        return PROCEDURE;
    if (points_to->kind == TYPE_KIND_ARRAY_OF_CONST)
        return ARRAY_OF_CONST_TYPE;
    if (points_to->kind == TYPE_KIND_ARRAY &&
        points_to->type_alias != NULL &&
        points_to->type_alias->is_shortstring)
        return SHORTSTRING_TYPE;
    return UNKNOWN_TYPE;
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
    if (type->type_alias != NULL && type->type_alias->is_shortstring)
        return 1;
    
    switch (type->kind) {
        case TYPE_KIND_PRIMITIVE:
            switch (type->info.primitive_type_tag) {
                case REAL_TYPE:
                case STRING_TYPE:
                case SHORTSTRING_TYPE:
                case FILE_TYPE:
                case TEXT_TYPE:
                case POINTER_TYPE:
                case PROCEDURE:
                case INT64_TYPE:
                case QWORD_TYPE:
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
int kgpc_type_is_signed(const KgpcType *type)
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

    if (type->kind == TYPE_KIND_PRIMITIVE)
        return (type->info.primitive_type_tag == type_tag);
    if (type->kind == TYPE_KIND_POINTER)
        return (type_tag == POINTER_TYPE);
    if (type->kind == TYPE_KIND_RECORD)
        return (type_tag == RECORD_TYPE);
    if (type->kind == TYPE_KIND_PROCEDURE)
        return (type_tag == PROCEDURE);
    if (type->kind == TYPE_KIND_ARRAY_OF_CONST)
        return (type_tag == ARRAY_OF_CONST_TYPE);
    if (type->kind == TYPE_KIND_ARRAY &&
        type->type_alias != NULL &&
        type->type_alias->is_shortstring)
        return (type_tag == SHORTSTRING_TYPE);
    return 0;
}

/* Internal helper with depth tracking to prevent infinite recursion */
static int kgpc_type_equals_internal(KgpcType *a, KgpcType *b, int depth)
{
    /* Prevent infinite recursion - if we've recursed too deep, assume not equal */
    if (depth > 100)
    {
        fprintf(stderr, "Warning: kgpc_type_equals recursion depth exceeded\n");
        return 0;
    }

    if (a == b)
        return 1;
    if (a == NULL || b == NULL)
        return 0;
    if ((kgpc_type_is_shortstring(a) && is_char_array_type(b)) ||
        (kgpc_type_is_shortstring(b) && is_char_array_type(a)))
        return 1;
    if (a->kind != b->kind)
        return 0;

    switch (a->kind)
    {
        case TYPE_KIND_PRIMITIVE:
            return a->info.primitive_type_tag == b->info.primitive_type_tag;
        case TYPE_KIND_POINTER:
            return kgpc_type_equals_internal(a->info.points_to, b->info.points_to, depth + 1);
        case TYPE_KIND_ARRAY:
            if (a->info.array_info.start_index != b->info.array_info.start_index ||
                a->info.array_info.end_index != b->info.array_info.end_index)
                return 0;
            if (a->info.array_info.element_type != NULL &&
                b->info.array_info.element_type != NULL)
            {
                return kgpc_type_equals_internal(a->info.array_info.element_type,
                    b->info.array_info.element_type, depth + 1);
            }
            if (a->info.array_info.element_type_id != NULL ||
                b->info.array_info.element_type_id != NULL)
            {
                if (a->info.array_info.element_type_id == NULL ||
                    b->info.array_info.element_type_id == NULL)
                    return 0;
                return strcasecmp(a->info.array_info.element_type_id,
                    b->info.array_info.element_type_id) == 0;
            }
            return 0;
        case TYPE_KIND_RECORD:
            return a->info.record_info == b->info.record_info;
        case TYPE_KIND_PROCEDURE:
            return a->info.proc_info.definition == b->info.proc_info.definition;
        case TYPE_KIND_ARRAY_OF_CONST:
            return 1;
        default:
            return 0;
    }
}

int kgpc_type_equals(KgpcType *a, KgpcType *b)
{
    return kgpc_type_equals_internal(a, b, 0);
}

int kgpc_type_pointers_compatible(KgpcType *ptr_a, KgpcType *ptr_b)
{
    if (ptr_a == NULL || ptr_b == NULL)
        return 0;
    if (ptr_a->kind != TYPE_KIND_POINTER || ptr_b->kind != TYPE_KIND_POINTER)
        return 0;
    if (ptr_a->info.points_to == NULL || ptr_b->info.points_to == NULL)
        return 1;
    return kgpc_type_equals(ptr_a->info.points_to, ptr_b->info.points_to);
}

int kgpc_type_conversion_rank(KgpcType *from, KgpcType *to)
{
    if (from == NULL || to == NULL)
        return -1;
    if (kgpc_type_equals(from, to))
        return 0;

    if ((kgpc_type_is_shortstring(from) && is_char_array_type(to)) ||
        (kgpc_type_is_shortstring(to) && is_char_array_type(from)))
        return 0;

    if (to->kind == TYPE_KIND_ARRAY_OF_CONST)
        return 1;

    if (from->kind == TYPE_KIND_POINTER && to->kind == TYPE_KIND_POINTER)
    {
        if (!kgpc_type_pointers_compatible(from, to))
            return -1;
        if (from->info.points_to != NULL && to->info.points_to != NULL &&
            kgpc_type_equals(from->info.points_to, to->info.points_to))
            return 0;
        return 1;
    }

    if (from->kind == TYPE_KIND_POINTER && to->kind == TYPE_KIND_PRIMITIVE &&
        to->info.primitive_type_tag == POINTER_TYPE)
        return 1;
    if (to->kind == TYPE_KIND_POINTER && from->kind == TYPE_KIND_PRIMITIVE &&
        from->info.primitive_type_tag == POINTER_TYPE)
        return 1;

    /* Handle integer-to-POINTER_TYPE (untyped Pointer) conversions */
    if (to->kind == TYPE_KIND_PRIMITIVE && to->info.primitive_type_tag == POINTER_TYPE &&
        from->kind == TYPE_KIND_PRIMITIVE && is_integer_type(from->info.primitive_type_tag)) {
        return 3;  /* Integer to untyped Pointer conversion */
    }
    
    /* Handle POINTER_TYPE (untyped Pointer) to integer conversions */
    if (from->kind == TYPE_KIND_PRIMITIVE && from->info.primitive_type_tag == POINTER_TYPE &&
        to->kind == TYPE_KIND_PRIMITIVE && is_integer_type(to->info.primitive_type_tag)) {
        return 3;  /* Untyped Pointer to integer conversion */
    }

    /* Handle string-to-pointer conversions (e.g., string literal to PAnsiChar) */
    if (to->kind == TYPE_KIND_POINTER && from->kind == TYPE_KIND_PRIMITIVE &&
        is_string_type(from->info.primitive_type_tag)) {
        /* String to pointer conversion (e.g., 'text' to PAnsiChar) */
        /* Check if pointer points to char type */
        if (to->info.points_to != NULL &&
            to->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
            to->info.points_to->info.primitive_type_tag == CHAR_TYPE) {
            return 2;  /* String to PChar conversion is worse than string-to-string */
        }
    }

    if (from->kind == TYPE_KIND_PRIMITIVE && to->kind == TYPE_KIND_PRIMITIVE)
    {
        int from_tag = from->info.primitive_type_tag;
        int to_tag = to->info.primitive_type_tag;

        if (is_integer_type(from_tag) && is_integer_type(to_tag))
        {
            long long from_size = kgpc_type_sizeof(from);
            long long to_size = kgpc_type_sizeof(to);
            if (from_size > 0 && to_size > 0)
            {
                if (to_size > from_size)
                    return 2;
                if (to_size < from_size)
                    return 3;
                return 1;
            }
            return 1;
        }
        if (is_integer_type(from_tag) && to_tag == REAL_TYPE)
            return 2;
        if (from_tag == CHAR_TYPE && is_string_type(to_tag))
            return 1;
        if (is_string_type(from_tag) && is_string_type(to_tag))
            return 1;
        if (from_tag == ENUM_TYPE && is_integer_type(to_tag))
            return 1;
        if (is_integer_type(from_tag) && to_tag == ENUM_TYPE)
            return 1;
    }

    if (from->kind == TYPE_KIND_ARRAY && to->kind == TYPE_KIND_ARRAY)
    {
        KgpcType *from_elem = from->info.array_info.element_type;
        KgpcType *to_elem = to->info.array_info.element_type;
        if (from_elem != NULL && to_elem != NULL)
            return kgpc_type_conversion_rank(from_elem, to_elem);
    }

    if (from->kind == TYPE_KIND_PROCEDURE && to->kind == TYPE_KIND_PROCEDURE)
        return 1;

    return -1;
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

/* Check if a type identified by name uses 64-bit operations */
int kgpc_type_id_uses_qword(const char *type_id, struct SymTab *symtab)
{
    if (type_id == NULL)
        return 0;

    /* First, try to resolve the type through the symbol table */
    if (symtab != NULL)
    {
        HashNode_t *type_node = kgpc_find_type_node(symtab, type_id);
        if (type_node != NULL && type_node->type != NULL)
        {
            return kgpc_type_uses_qword(type_node->type);
        }
    }

    /* If we can't resolve through symbol table, use naming conventions.
     * In Pascal, pointer types typically:
     * - Start with 'P' followed by uppercase letter (e.g., PAnsiChar, PByte)
     * - Are named 'Pointer' (untyped pointer)
     * - Are named 'CodePointer' (code pointer)
     * - Are named '*string' types (dynamic strings are references)
     * All pointers are 64-bit on x86-64. */
    size_t len = strlen(type_id);
    
    /* Check for common 64-bit types by name */
    if (pascal_identifier_equals(type_id, "Pointer") ||
        pascal_identifier_equals(type_id, "CodePointer"))
        return 1;
    
    /* Check for pointer types: P followed by uppercase letter */
    if (len >= 2 && (type_id[0] == 'P' || type_id[0] == 'p'))
    {
        char second = type_id[1];
        if (second >= 'A' && second <= 'Z')
            return 1;  /* Likely a pointer type like PAnsiChar, PByte, PInteger, etc. */
    }
    
    /* Check for 64-bit primitive types */
    if (pascal_identifier_equals(type_id, "Int64") ||
        pascal_identifier_equals(type_id, "QWord") ||
        pascal_identifier_equals(type_id, "UInt64") ||
        pascal_identifier_equals(type_id, "Double") ||
        pascal_identifier_equals(type_id, "Real") ||
        pascal_identifier_equals(type_id, "Extended") ||
        pascal_identifier_equals(type_id, "Currency"))
        return 1;
    
    /* Check for string types (dynamic strings are references/pointers) */
    if (pascal_identifier_equals(type_id, "String") ||
        pascal_identifier_equals(type_id, "AnsiString") ||
        pascal_identifier_equals(type_id, "WideString") ||
        pascal_identifier_equals(type_id, "UnicodeString") ||
        pascal_identifier_equals(type_id, "RawByteString") ||
        pascal_identifier_equals(type_id, "ShortString"))
        return 1;
    
    /* Check for SizeInt/PtrInt/NativeInt which are pointer-sized */
    if (pascal_identifier_equals(type_id, "SizeInt") ||
        pascal_identifier_equals(type_id, "SizeUInt") ||
        pascal_identifier_equals(type_id, "PtrInt") ||
        pascal_identifier_equals(type_id, "PtrUInt") ||
        pascal_identifier_equals(type_id, "NativeInt") ||
        pascal_identifier_equals(type_id, "NativeUInt") ||
        pascal_identifier_equals(type_id, "IntPtr") ||
        pascal_identifier_equals(type_id, "UIntPtr"))
        return 1;
    
    /* Check for TObject and class types (classes are references) */
    if (len >= 2 && (type_id[0] == 'T' || type_id[0] == 't'))
    {
        /* TObject and T-prefixed classes are references */
        if (pascal_identifier_equals(type_id, "TObject") ||
            pascal_identifier_equals(type_id, "TClass"))
            return 1;
    }
    
    return 0;
}

const char* type_tag_to_string(int type_tag)
{
    switch (type_tag)
    {
        case UNKNOWN_TYPE: return "Unknown";
        case INT_TYPE: return "Integer";
        case REAL_TYPE: return "Real";
        case LONGINT_TYPE: return "LongInt";
        case STRING_TYPE: return "String";
        case BUILTIN_ANY_TYPE: return "Any";
        case INT64_TYPE: return "Int64";
        case RECORD_TYPE: return "Record";
        case ARRAY_OF_CONST_TYPE: return "array of const";
        case TEXT_TYPE: return "Text";
        case CHAR_TYPE: return "Char";
        case POINTER_TYPE: return "Pointer";
        case SET_TYPE: return "Set";
        case ENUM_TYPE: return "Enum";
        case FILE_TYPE: return "File";
        case SHORTSTRING_TYPE: return "ShortString";
        case BYTE_TYPE: return "Byte";
        case WORD_TYPE: return "Word";
        case LONGWORD_TYPE: return "LongWord";
        case QWORD_TYPE: return "QWord";
        default:
        {
            static char buf[32];
            snprintf(buf, sizeof(buf), "type(%d)", type_tag);
            return buf;
        }
    }
}
