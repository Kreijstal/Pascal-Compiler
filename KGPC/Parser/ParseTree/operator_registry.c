/* Operator Overload Registry Implementation */

#include "operator_registry.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#endif

static ListNode_t *operator_overloads = NULL;

void operator_registry_init(void) {
    /* Registry is initialized lazily */
    operator_overloads = NULL;
}

void operator_registry_add(const char *type_name, const char *operator_symbol,
                           const char *mangled_method_name,
                           const char *left_param_type,
                           const char *right_param_type,
                           const char *return_type,
                           int param_count) {
    if (type_name == NULL || operator_symbol == NULL || mangled_method_name == NULL)
        return;
    
    OperatorOverload *overload = (OperatorOverload *)calloc(1, sizeof(OperatorOverload));
    if (overload == NULL)
        return;
    
    overload->type_name = strdup(type_name);
    overload->operator_symbol = strdup(operator_symbol);
    overload->mangled_method_name = strdup(mangled_method_name);
    overload->left_param_type = left_param_type ? strdup(left_param_type) : NULL;
    overload->right_param_type = right_param_type ? strdup(right_param_type) : NULL;
    overload->return_type = return_type ? strdup(return_type) : NULL;
    overload->param_count = param_count;
    
    /* Determine if this is a comparison operator */
    overload->is_comparison = (strcmp(operator_symbol, "=") == 0 ||
                              strcmp(operator_symbol, "<>") == 0 ||
                              strcmp(operator_symbol, "<") == 0 ||
                              strcmp(operator_symbol, ">") == 0 ||
                              strcmp(operator_symbol, "<=") == 0 ||
                              strcmp(operator_symbol, ">=") == 0);
    
    /* Add to registry */
    ListNode_t *node = CreateListNode(overload, LIST_UNSPECIFIED);
    if (node != NULL) {
        node->next = operator_overloads;
        operator_overloads = node;
    } else {
        free(overload->type_name);
        free(overload->operator_symbol);
        free(overload->mangled_method_name);
        free(overload->left_param_type);
        free(overload->right_param_type);
        free(overload->return_type);
        free(overload);
    }
}

/* Helper function to check if two types match (case-insensitive) */
static int types_match(const char *type1, const char *type2) {
    if (type1 == NULL && type2 == NULL)
        return 1;
    if (type1 == NULL || type2 == NULL)
        return 0;
    return strcasecmp(type1, type2) == 0;
}

OperatorOverload *operator_registry_lookup(const char *operator_symbol,
                                           const char *left_type,
                                           const char *right_type) {
    if (operator_symbol == NULL)
        return NULL;
    
    ListNode_t *cur = operator_overloads;
    while (cur != NULL) {
        OperatorOverload *overload = (OperatorOverload *)cur->cur;
        if (overload != NULL &&
            strcmp(overload->operator_symbol, operator_symbol) == 0) {
            
            /* For binary operators, check both parameter types */
            if (overload->param_count == 2 && right_type != NULL) {
                if (types_match(overload->left_param_type, left_type) &&
                    types_match(overload->right_param_type, right_type)) {
                    return overload;
                }
            }
            /* For unary operators, check only left parameter */
            else if (overload->param_count == 1 && right_type == NULL) {
                if (types_match(overload->left_param_type, left_type)) {
                    return overload;
                }
            }
        }
        cur = cur->next;
    }
    
    return NULL;
}

int operator_registry_has_overload(const char *type_name, const char *operator_symbol) {
    if (type_name == NULL || operator_symbol == NULL)
        return 0;
    
    ListNode_t *cur = operator_overloads;
    while (cur != NULL) {
        OperatorOverload *overload = (OperatorOverload *)cur->cur;
        if (overload != NULL &&
            strcmp(overload->operator_symbol, operator_symbol) == 0 &&
            types_match(overload->type_name, type_name)) {
            return 1;
        }
        cur = cur->next;
    }
    
    return 0;
}

void operator_registry_cleanup(void) {
    ListNode_t *cur = operator_overloads;
    while (cur != NULL) {
        ListNode_t *next = cur->next;
        OperatorOverload *overload = (OperatorOverload *)cur->cur;
        if (overload != NULL) {
            free(overload->type_name);
            free(overload->operator_symbol);
            free(overload->mangled_method_name);
            free(overload->left_param_type);
            free(overload->right_param_type);
            free(overload->return_type);
            free(overload);
        }
        free(cur);
        cur = next;
    }
    operator_overloads = NULL;
}
