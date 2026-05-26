#ifndef CODEGEN_EXPR_RELOP_H
#define CODEGEN_EXPR_RELOP_H

#include "codegen.h"

ListNode_t *codegen_simple_relop(struct Expression *expr, ListNode_t *inst_list,
                                 CodeGenContext *ctx, int *relop_type);
ListNode_t *codegen_relop_to_value(struct Expression *expr,
                                   ListNode_t *inst_list, CodeGenContext *ctx,
                                   Register_t **out_reg);

#endif // CODEGEN_EXPR_RELOP_H
