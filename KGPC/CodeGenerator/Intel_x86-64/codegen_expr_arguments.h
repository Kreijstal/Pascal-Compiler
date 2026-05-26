#ifndef CODEGEN_EXPR_ARGUMENTS_H
#define CODEGEN_EXPR_ARGUMENTS_H

#include "codegen.h"

/* Argument-passing and nonlocal-access subsystem */

ListNode_t *codegen_get_nonlocal(ListNode_t *inst_list, char *var_id,
                                 int *offset, CodeGenContext *ctx);
ListNode_t *codegen_pass_arguments(
    ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx,
    struct KgpcType *proc_type, const char *procedure_name, int arg_start_index,
    const struct Expression *call_expr, int is_class_method_call_hint);
ListNode_t *codegen_cleanup_call_stack(ListNode_t *inst_list,
                                       CodeGenContext *ctx);
ListNode_t *codegen_goto_prev_scope(ListNode_t *inst_list,
                                    StackScope_t *cur_scope, char *base);

#endif /* CODEGEN_EXPR_ARGUMENTS_H */
