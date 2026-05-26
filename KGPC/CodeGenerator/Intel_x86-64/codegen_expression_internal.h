#ifndef CODEGEN_EXPRESSION_INTERNAL_H
#define CODEGEN_EXPRESSION_INTERNAL_H

#include "codegen_expression.h"

/* Helpers defined in codegen_expression.c, promoted from static for cross-TU
 * use */
struct RecordField *
codegen_expr_lookup_record_field(struct Expression *record_access_expr,
                                 CodeGenContext *ctx);
Register_t *codegen_try_get_reg(ListNode_t **inst_list, CodeGenContext *ctx,
                                const char *usage);

/* codegen_expr_tree_value: defined in codegen_expression.c, not yet in public
 * header */
ListNode_t *codegen_expr_tree_value(struct Expression *expr,
                                    ListNode_t *inst_list, CodeGenContext *ctx,
                                    Register_t **out_reg);

/* codegen_expr_record_type: defined in codegen_expression.c */
struct RecordType *codegen_expr_record_type(const struct Expression *expr,
                                            SymTab_t *symtab);

/* codegen_set_expr: defined in codegen_expr_access.c, called from
 * codegen_expression.c */
ListNode_t *codegen_set_expr(struct Expression *expr, ListNode_t *inst_list,
                             CodeGenContext *ctx, Register_t **out_reg);

#endif /* CODEGEN_EXPRESSION_INTERNAL_H */
