#ifndef CODEGEN_EXPR_ARRAY_H
#define CODEGEN_EXPR_ARRAY_H

#include "codegen.h"

int codegen_get_indexable_element_size(struct Expression *array_expr,
                                       CodeGenContext *ctx,
                                       long long *out_size);
ListNode_t *codegen_array_element_address(struct Expression *expr,
                                          ListNode_t *inst_list,
                                          CodeGenContext *ctx,
                                          Register_t **out_reg);
ListNode_t *codegen_array_access(struct Expression *expr, ListNode_t *inst_list,
                                 CodeGenContext *ctx, Register_t *target_reg);
int expr_contains_function_call(const struct Expression *expr);
int codegen_dynarray_descriptor_size(const struct Expression *expr);

#endif // CODEGEN_EXPR_ARRAY_H
