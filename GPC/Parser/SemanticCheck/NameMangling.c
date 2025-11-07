#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "NameMangling.h"
#include "../ParseTree/tree.h"
#include "../List/List.h"
#include "../ParseTree/type_tags.h"
#include "SemChecks/SemCheck_expr.h"
#include "SymTab/SymTab.h"
#include "../ParseTree/GpcType.h"

// Helper to free a list of integers
static void DestroyIntList(ListNode_t* list) {
    assert(list != NULL);
    ListNode_t* cur = list;
    while (cur != NULL) {
        free(cur->cur);
        ListNode_t* next = cur->next;
        free(cur);
        cur = next;
    }
}

static enum VarType ConvertParserTypeToVarType(int parser_type)
{
    switch(parser_type)
    {
        case INT_TYPE:
            return HASHVAR_INTEGER;
        case LONGINT_TYPE:
            return HASHVAR_LONGINT;
        case REAL_TYPE:
            return HASHVAR_REAL;
        case STRING_TYPE:
            return HASHVAR_PCHAR;
        case BOOL:
            return HASHVAR_BOOLEAN;
        case CHAR_TYPE:
            return HASHVAR_CHAR;
        case POINTER_TYPE:
            return HASHVAR_POINTER;
        case SET_TYPE:
            return HASHVAR_SET;
        case ENUM_TYPE:
            return HASHVAR_ENUM;
        case FILE_TYPE:
            return HASHVAR_FILE;
        default:
            return HASHVAR_UNTYPED;
    }
}

// Helper to get VarType from a type HashNode, preferring GpcType
static enum VarType GetVarTypeFromTypeNode(HashNode_t* type_node) {
    if (type_node == NULL)
        return HASHVAR_UNTYPED;
    
    // If GpcType is available, extract VarType from it
    if (type_node->type != NULL) {
        if (type_node->type->kind == TYPE_KIND_PRIMITIVE) {
            int tag = gpc_type_get_primitive_tag(type_node->type);
            return ConvertParserTypeToVarType(tag);
        } else if (type_node->type->kind == TYPE_KIND_POINTER) {
            return HASHVAR_POINTER;
        } else if (type_node->type->kind == TYPE_KIND_RECORD) {
            return HASHVAR_RECORD;
        } else if (type_node->type->kind == TYPE_KIND_ARRAY) {
            return HASHVAR_ARRAY;
        } else if (type_node->type->kind == TYPE_KIND_PROCEDURE) {
            return HASHVAR_PROCEDURE;
        }
    }
    
    // Fallback to hashnode helper
    return hashnode_get_var_type(type_node);
}

// Helper function to flatten argument lists into a list of HASHVAR_ types.
static ListNode_t* GetFlatTypeListForMangling(ListNode_t *args, SymTab_t *symtab) { // <-- Add symtab
    assert(symtab != NULL);
    if (args == NULL) {
        return NULL;
    }

    ListNode_t* type_list = NULL;
    ListNode_t* arg_cur = args;
    while (arg_cur != NULL) {
        Tree_t* decl_tree = (Tree_t*)arg_cur->cur;
        enum VarType resolved_type = HASHVAR_UNTYPED; // Default to untyped
        ListNode_t* ids;

        if (decl_tree->type == TREE_VAR_DECL) {
            ids = decl_tree->tree_data.var_decl_data.ids;
            // --- THIS IS THE CORE LOGIC ---
            if (decl_tree->tree_data.var_decl_data.type_id != NULL) {
                // It's a custom type, look it up in the symbol table
                HashNode_t* type_node;
                if (FindIdent(&type_node, symtab, decl_tree->tree_data.var_decl_data.type_id) != -1 && type_node->hash_type == HASHTYPE_TYPE) {
                    resolved_type = GetVarTypeFromTypeNode(type_node);
                }
            } else {
                // It's a built-in type, convert from parser token to semantic type
                resolved_type = ConvertParserTypeToVarType(decl_tree->tree_data.var_decl_data.type);
            }
        } else { // Assume array or other type for now
            ids = decl_tree->tree_data.arr_decl_data.ids;
            resolved_type = -1; // Special marker for array
        }

        ListNode_t* id_cur = ids;
        while (id_cur != NULL) {
            int* type_ptr = malloc(sizeof(int));
            assert(type_ptr != NULL);
            *type_ptr = resolved_type;
            if (type_list == NULL) {
                type_list = CreateListNode(type_ptr, LIST_UNSPECIFIED);
            } else {
                PushListNodeBack(type_list, CreateListNode(type_ptr, LIST_UNSPECIFIED));
            }
            id_cur = id_cur->next;
        }
        arg_cur = arg_cur->next;
    }
    return type_list;
}

// Core mangling function
static char* MangleNameFromTypeList(const char* original_name, ListNode_t* type_list) {
    assert(original_name != NULL);
    if (type_list == NULL) {
        // No args, append _void
        const char* suffix = "_void";
        char* mangled_name = malloc(strlen(original_name) + strlen(suffix) + 1);
        assert(mangled_name != NULL);
        sprintf(mangled_name, "%s%s", original_name, suffix);
        return mangled_name;
    }

    // Calculate length
    size_t total_len = strlen(original_name);
    ListNode_t* cur = type_list;
    while (cur != NULL) {
        total_len += 4; // Max length of a type suffix, e.g., "_i" or "_r" plus some buffer
        cur = cur->next;
    }
    total_len += 1; // Null terminator

    char* mangled_name = malloc(total_len);
    assert(mangled_name != NULL);
    strcpy(mangled_name, original_name);

    cur = type_list;
    while (cur != NULL) {
        int type = *(int*)cur->cur;
        const char* type_suffix;
        switch (type) {
            case HASHVAR_INTEGER: type_suffix = "_i"; break;
            case HASHVAR_LONGINT: type_suffix = "_li"; break;
            case HASHVAR_REAL:    type_suffix = "_r"; break;
            case HASHVAR_PCHAR:   type_suffix = "_s"; break; // For string
case HASHVAR_BOOLEAN: type_suffix = "_b"; break;
        case HASHVAR_GENERIC_PARAM: type_suffix = "_g"; break;
        case HASHVAR_CHAR:    type_suffix = "_c"; break;
            case HASHVAR_POINTER: type_suffix = "_p"; break;
            case HASHVAR_SET:     type_suffix = "_set"; break;
            case HASHVAR_ENUM:    type_suffix = "_e"; break;
            case HASHVAR_FILE:    type_suffix = "_f"; break;
            case HASHVAR_RECORD:  type_suffix = "_u"; break; // Record types treated as unknown for mangling
            case HASHVAR_ARRAY:   type_suffix = "_a"; break; // Array
            default:              type_suffix = "_u"; break; // Unknown/unsupported
        }
        strcat(mangled_name, type_suffix);
        cur = cur->next;
    }

    DestroyIntList(type_list);
    return mangled_name;
}

// Update the main function signature and the call
char* MangleFunctionName(const char* original_name, ListNode_t* args, SymTab_t* symtab) { // <-- Add symtab
    assert(original_name != NULL);
    assert(symtab != NULL);
    ListNode_t* type_list = GetFlatTypeListForMangling(args, symtab); // <-- Pass symtab
    return MangleNameFromTypeList(original_name, type_list);
}

// Helper function to get a flat list of types from a function call's arguments
static ListNode_t* GetFlatTypeListFromCallSite(ListNode_t *args_expr, SymTab_t *symtab, int max_scope_lev) {
    assert(symtab != NULL);
    if (args_expr == NULL) {
        return NULL;
    }

    ListNode_t* type_list = NULL;
    ListNode_t* arg_cur = args_expr;
    while (arg_cur != NULL) {
        int type;
        semcheck_expr_main(&type, symtab, (struct Expression *)arg_cur->cur, max_scope_lev, NO_MUTATE);

        int* type_ptr = malloc(sizeof(int));
        assert(type_ptr != NULL);
        *type_ptr = ConvertParserTypeToVarType(type);
        if (type_list == NULL) {
            type_list = CreateListNode(type_ptr, LIST_UNSPECIFIED);
        } else {
            PushListNodeBack(type_list, CreateListNode(type_ptr, LIST_UNSPECIFIED));
        }
        arg_cur = arg_cur->next;
    }
    return type_list;
}

char* MangleFunctionNameFromCallSite(const char* original_name, ListNode_t* args_expr, SymTab_t *symtab, int max_scope_lev) {
    assert(original_name != NULL);
    assert(symtab != NULL);
    ListNode_t* type_list = GetFlatTypeListFromCallSite(args_expr, symtab, max_scope_lev);
    return MangleNameFromTypeList(original_name, type_list);
}
