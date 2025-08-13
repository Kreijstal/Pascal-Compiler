#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "NameMangling.h"
#include "../ParseTree/tree.h"
#include "../List/List.h"
#include "../LexAndYacc/Grammar.tab.h"
#include "SemChecks/SemCheck_expr.h"

// Helper to free a list of integers
static void DestroyIntList(ListNode_t* list) {
    ListNode_t* cur = list;
    while (cur != NULL) {
        free(cur->cur);
        ListNode_t* next = cur->next;
        free(cur);
        cur = next;
    }
}

// Helper function to flatten argument lists into a list of types for mangling.
static ListNode_t* GetFlatTypeListForMangling(ListNode_t *args) {
    if (args == NULL) {
        return NULL;
    }

    ListNode_t* type_list = NULL;
    ListNode_t* arg_cur = args;
    while (arg_cur != NULL) {
        Tree_t* decl_tree = (Tree_t*)arg_cur->cur;
        int type;
        ListNode_t* ids;

        if (decl_tree->type == TREE_VAR_DECL) {
            type = decl_tree->tree_data.var_decl_data.type;
            ids = decl_tree->tree_data.var_decl_data.ids;
        } else if (decl_tree->type == TREE_ARR_DECL) {
            type = -1; // Special type for arrays
            ids = decl_tree->tree_data.arr_decl_data.ids;
        } else {
            arg_cur = arg_cur->next;
            continue;
        }

        ListNode_t* id_cur = ids;
        while (id_cur != NULL) {
            int* type_ptr = malloc(sizeof(int));
            *type_ptr = type;
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
    if (type_list == NULL) {
        // No args, append _void
        const char* suffix = "_void";
        char* mangled_name = malloc(strlen(original_name) + strlen(suffix) + 1);
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
    strcpy(mangled_name, original_name);

    cur = type_list;
    while (cur != NULL) {
        int type = *(int*)cur->cur;
        const char* type_suffix;
        switch (type) {
            case INT_TYPE: type_suffix = "_i"; break;
            case LONGINT_TYPE: type_suffix = "_li"; break;
            case REAL_TYPE: type_suffix = "_r"; break;
            case -1: type_suffix = "_a"; break; // Array
            default: type_suffix = "_u"; break; // Unknown/unsupported
        }
        strcat(mangled_name, type_suffix);
        cur = cur->next;
    }

    DestroyIntList(type_list);
    return mangled_name;
}

char* MangleFunctionName(const char* original_name, ListNode_t* args) {
    ListNode_t* type_list = GetFlatTypeListForMangling(args);
    return MangleNameFromTypeList(original_name, type_list);
}

// Helper function to get a flat list of types from a function call's arguments
static ListNode_t* GetFlatTypeListFromCallSite(ListNode_t *args_expr, SymTab_t *symtab, int max_scope_lev) {
    if (args_expr == NULL) {
        return NULL;
    }

    ListNode_t* type_list = NULL;
    ListNode_t* arg_cur = args_expr;
    while (arg_cur != NULL) {
        int type;
        semcheck_expr_main(&type, symtab, (struct Expression *)arg_cur->cur, max_scope_lev, NO_MUTATE);

        int* type_ptr = malloc(sizeof(int));
        *type_ptr = type;
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
    ListNode_t* type_list = GetFlatTypeListFromCallSite(args_expr, symtab, max_scope_lev);
    return MangleNameFromTypeList(original_name, type_list);
}
