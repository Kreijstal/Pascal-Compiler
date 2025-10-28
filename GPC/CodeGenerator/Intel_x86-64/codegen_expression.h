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
ListNode_t *codegen_array_access(struct Expression *, ListNode_t *, CodeGenContext *, Register_t *);
ListNode_t *codegen_array_element_address(struct Expression *, ListNode_t *, CodeGenContext *, Register_t **);
ListNode_t *codegen_args(ListNode_t*, ListNode_t *, CodeGenContext *ctx);

ListNode_t *codegen_sign_extend32_to64(ListNode_t *inst_list, const char *src_reg32, const char *dst_reg64);

/* (DEPRECATED) */
ListNode_t *codegen_expr_varid(struct Expression *, ListNode_t *, CodeGenContext *ctx);
ListNode_t *codegen_expr_inum(struct Expression *, ListNode_t *, CodeGenContext *ctx);


#endif // CODEGEN_EXPRESSION_H
