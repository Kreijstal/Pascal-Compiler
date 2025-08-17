#ifndef CODEGEN_EXPRESSION_H
#define CODEGEN_EXPRESSION_H

#include "codegen.h"

/*
    Expression-related code generation functions
*/

ListNode_t *codegen_pass_arguments(ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx, struct HashNode *proc_node);
ListNode_t *codegen_get_nonlocal(ListNode_t *, char *, int *);

ListNode_t *codegen_simple_relop(struct Expression *, ListNode_t *,
    CodeGenContext *ctx, int *);

ListNode_t *codegen_expr(struct Expression *, ListNode_t *, CodeGenContext *ctx);
ListNode_t *codegen_args(ListNode_t*, ListNode_t *, CodeGenContext *ctx);

/* (DEPRECATED) */
ListNode_t *codegen_expr_varid(struct Expression *, ListNode_t *, CodeGenContext *ctx);
ListNode_t *codegen_expr_inum(struct Expression *, ListNode_t *, CodeGenContext *ctx);


#endif // CODEGEN_EXPRESSION_H
