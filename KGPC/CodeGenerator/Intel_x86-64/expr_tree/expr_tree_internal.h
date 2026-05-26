#ifndef EXPR_TREE_INTERNAL_H
#define EXPR_TREE_INTERNAL_H

#include "expr_tree.h"
#include "../codegen.h"

/* Describes the semantic kind of an assembly operand, so codegen helpers can
   select the correct instruction without resorting to string pattern matching.
 */
typedef enum {
  OPKIND_REGISTER,  /* Operand is a CPU register (e.g. %rbx)           */
  OPKIND_IMMEDIATE, /* Operand is an immediate value (e.g. $42)        */
  OPKIND_MEMORY,    /* Operand is a stack/memory reference (e.g. -8(%rbp)) */
  OPKIND_LABEL      /* Operand is a RIP-relative label (needs leaq)    */
} OperandKind;

/* Functions defined in expr_tree_op.c */
ListNode_t *gencode_leaf_var(struct Expression *, ListNode_t *,
                             CodeGenContext *, char *, int,
                             OperandKind *out_kind);
ListNode_t *gencode_op(struct Expression *expr, const char *left,
                       const Register_t *left_reg, const char *right,
                       const Register_t *right_reg, OperandKind left_kind,
                       OperandKind right_kind, ListNode_t *inst_list,
                       CodeGenContext *ctx);
ListNode_t *gencode_op_deprecated(struct Expression *expr,
                                  ListNode_t *inst_list, char *buffer,
                                  int buf_len, CodeGenContext *ctx);

ListNode_t *gencode_divide_const_no_optimize(char *left, char *right,
                                             ListNode_t *inst_list);
ListNode_t *gencode_divide_no_const(char *left, char *right,
                                    ListNode_t *inst_list);

/* Helpers defined in expr_tree.c, shared with expr_tree_op.c */
int expr_tree_is_tconstexprint_payload(const struct Expression *expr);
int expr_tree_symbol_matches_expr_type(const HashNode_t *node,
                                       const struct Expression *expr);
HashNode_t *expr_tree_find_preferred_symbol(CodeGenContext *ctx,
                                            const struct Expression *expr);
char *escape_string_for_assembly(const char *input);
const char *operand_as_reg64(const char *operand, const Register_t *reg);
const char *select_divisor_temp_reg(const Register_t *avoid_reg, int use_qword);
int expr_is_single_real_with_symtab(const struct Expression *expr,
                                    SymTab_t *symtab);
ListNode_t *emit_alu_op_with_large_imm(ListNode_t *inst_list,
                                       CodeGenContext *ctx,
                                       const char *mnemonic, char arith_suffix,
                                       const char *op_right,
                                       const char *op_left, int *error);
ListNode_t *
gencode_real_binary_op(CodeGenContext *ctx, struct Expression *left_expr,
                       const char *left_operand, const Register_t *left_reg,
                       struct Expression *right_expr, const char *right_operand,
                       const Register_t *right_reg, const char *dest,
                       const Register_t *dest_reg, ListNode_t *inst_list,
                       const char *sse_mnemonic);
const char *reg64_to_reg32(const char *reg_name, const Register_t *reg);
const char *reg_to_reg32(const char *reg_name, const Register_t *reg);
const char *reg_to_reg64(const char *reg_name, const Register_t *reg);
const char *reg32_to_reg8(const char *reg_name, const Register_t *reg);
const char *reg32_to_reg64(const char *reg_name, const Register_t *reg);
int operand_is_32bit_register(const char *operand, const Register_t *reg);
int type_tag_is_signed_32bit_int(int type_tag);
int expr_function_call_returns_ansistring(const struct Expression *expr,
                                          CodeGenContext *ctx);
int expr_is_shortstring_storage_ctx(const struct Expression *expr,
                                    CodeGenContext *ctx);
int expr_is_char_array_expr(const struct Expression *expr);
int expr_get_char_array_length_expr(const struct Expression *expr,
                                    CodeGenContext *ctx, long long *out_len);
ListNode_t *promote_shortstring_reg_operand(ListNode_t *inst_list,
                                            CodeGenContext *ctx,
                                            const char *value_operand,
                                            const Register_t *value_reg);
ListNode_t *
promote_shortstring_operand_ex(ListNode_t *inst_list, CodeGenContext *ctx,
                               const char **operand_ptr, Register_t **reg_ptr,
                               OperandKind *kind_ptr, const char *other_operand,
                               const Register_t *other_reg);
ListNode_t *promote_char_operand_to_string_ex(ListNode_t *inst_list,
                                              const char **operand_ptr,
                                              Register_t **reg_ptr,
                                              OperandKind *kind_ptr,
                                              const char *other_operand,
                                              const Register_t *other_reg);
ListNode_t *emit_move_ptr_operand_kind(ListNode_t *inst_list, const char *src,
                                       const Register_t *src_reg,
                                       OperandKind kind, const char *dst);

#endif /* EXPR_TREE_INTERNAL_H */
