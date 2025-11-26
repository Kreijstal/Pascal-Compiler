/* Operator Overload Registry
 * Tracks operator overloads defined for custom types (records/classes)
 */

#ifndef OPERATOR_REGISTRY_H
#define OPERATOR_REGISTRY_H

#include "../List/List.h"

/* Operator overload entry */
typedef struct OperatorOverload {
    char *type_name;           /* Name of the type that defines this operator */
    char *operator_symbol;     /* Operator symbol (+, -, *, /, =, etc.) */
    char *mangled_method_name; /* Mangled name of the implementing method */
    char *left_param_type;     /* Type of left parameter */
    char *right_param_type;    /* Type of right parameter (NULL for unary) */
    char *return_type;         /* Return type of the operator */
    int param_count;           /* Number of parameters (1 for unary, 2 for binary) */
    int is_comparison;         /* 1 if this is a comparison operator */
} OperatorOverload;

/* Initialize operator registry */
void operator_registry_init(void);

/* Register an operator overload */
void operator_registry_add(const char *type_name, const char *operator_symbol,
                           const char *mangled_method_name,
                           const char *left_param_type,
                           const char *right_param_type,
                           const char *return_type,
                           int param_count);

/* Look up an operator overload for given types */
OperatorOverload *operator_registry_lookup(const char *operator_symbol,
                                           const char *left_type,
                                           const char *right_type);

/* Check if a type has an operator overload */
int operator_registry_has_overload(const char *type_name, const char *operator_symbol);

/* Free operator registry */
void operator_registry_cleanup(void);

#endif /* OPERATOR_REGISTRY_H */
