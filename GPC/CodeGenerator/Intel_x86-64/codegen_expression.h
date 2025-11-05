#ifndef CODEGEN_EXPRESSION_H
#define CODEGEN_EXPRESSION_H

#include "codegen.h"

/*
    Expression-related code generation functions
*/

ListNode_t *codegen_pass_arguments(ListNode_t *args, ListNode_t *inst_list,
    CodeGenContext *ctx, struct GpcType *proc_type, const char *procedure_name, int arg_start_index);
ListNode_t *codegen_get_nonlocal(ListNode_t *, char *, int *);

ListNode_t *codegen_simple_relop(struct Expression *, ListNode_t *,
    CodeGenContext *ctx, int *);
ListNode_t *codegen_condition_expr(struct Expression *, ListNode_t *,
    CodeGenContext *ctx, int *);

ListNode_t *codegen_expr(struct Expression *, ListNode_t *, CodeGenContext *ctx);
ListNode_t *codegen_expr_with_result(struct Expression *, ListNode_t *, CodeGenContext *ctx, Register_t **out_reg);
ListNode_t *codegen_array_access(struct Expression *, ListNode_t *, CodeGenContext *, Register_t *);
ListNode_t *codegen_array_element_address(struct Expression *, ListNode_t *, CodeGenContext *, Register_t **);
ListNode_t *codegen_record_access(struct Expression *, ListNode_t *, CodeGenContext *, Register_t *);
ListNode_t *codegen_record_field_address(struct Expression *, ListNode_t *, CodeGenContext *, Register_t **);
ListNode_t *codegen_address_for_expr(struct Expression *, ListNode_t *, CodeGenContext *, Register_t **);
ListNode_t *codegen_args(ListNode_t*, ListNode_t *, CodeGenContext *ctx);

int codegen_expr_is_addressable(const struct Expression *expr);

int codegen_get_record_size(CodeGenContext *ctx, struct Expression *expr, long long *size_out);
int codegen_sizeof_record_type(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out);
int codegen_sizeof_pointer_target(CodeGenContext *ctx, struct Expression *pointer_expr,
    long long *size_out);

ListNode_t *codegen_sign_extend32_to64(ListNode_t *inst_list, const char *src_reg32, const char *dst_reg64);
ListNode_t *codegen_zero_extend32_to64(ListNode_t *inst_list, const char *src_reg32, const char *dst_reg32);

int codegen_type_uses_qword(int type_tag);
int codegen_type_is_signed(int type_tag);
int codegen_expr_is_signed(const struct Expression *expr);

/* Helper to get type tag from expression, preferring resolved_gpc_type.
 * This is a migration helper for moving from legacy type fields to GpcType. */
int expr_get_type_tag(const struct Expression *expr);

/* Check if expression type matches a specific type tag, using GpcType when available.
 * Returns 1 if match, 0 otherwise. */
int expr_has_type_tag(const struct Expression *expr, int type_tag);

/* Check if expression uses qword (64-bit) operations, using GpcType when available.
 * Returns 1 if qword, 0 otherwise. */
int expr_uses_qword_gpctype(const struct Expression *expr);

/* Helper to get array lower bound from expression, preferring resolved_gpc_type.
 * Returns the lower bound, or 0 if not an array. */
int expr_get_array_lower_bound(const struct Expression *expr);

/* Helper to get array element size from expression, preferring resolved_gpc_type.
 * Returns the element size in bytes, or -1 if not determinable. */
long long expr_get_array_element_size(const struct Expression *expr, CodeGenContext *ctx);

/* Check if an expression represents a character set (set of char).
 * Returns 1 if it's a character set, 0 otherwise. */
int expr_is_char_set(const struct Expression *expr);

/* Check if an expression represents a character set with context for symbol lookup.
 * Returns 1 if it's a character set, 0 otherwise. */
int expr_is_char_set_ctx(const struct Expression *expr, CodeGenContext *ctx);

int codegen_sizeof_type_reference(CodeGenContext *ctx, int type_tag, const char *type_id,
    struct RecordType *record_type, long long *size_out);

ListNode_t *codegen_pointer_deref_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *codegen_addressof_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);

/* (DEPRECATED) */
ListNode_t *codegen_expr_varid(struct Expression *, ListNode_t *, CodeGenContext *ctx);
ListNode_t *codegen_expr_inum(struct Expression *, ListNode_t *, CodeGenContext *ctx);


#endif // CODEGEN_EXPRESSION_H
