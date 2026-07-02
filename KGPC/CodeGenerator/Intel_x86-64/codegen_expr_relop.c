/*
    Damon Gwinn
    Code generation for relational operators
*/

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/KgpcType.h"
#include "../../Parser/ParseTree/from_cparser.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../Parser/SemanticCheck/SemCheck.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_Expr_Internal.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_expr.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../../Parser/pascal_frontend.h"
#include "../../flags.h"
#include "../../identifier_utils.h"
#include "codegen.h"
#include "codegen_expr_relop.h"
#include "codegen_expression.h"
#include "expr_tree/expr_tree.h"
#include "register_types.h"
#include "stackmng/stackmng.h"

int codegen_expr_function_call_returns_ansistring(
    CodeGenContext *ctx, const struct Expression *expr);
int codegen_expr_is_char_array_like_ctx(const struct Expression *expr,
                                        CodeGenContext *ctx);
ListNode_t *codegen_promote_shortstring_reg(ListNode_t *inst_list,
                                            CodeGenContext *ctx,
                                            Register_t *value_reg);
int codegen_get_char_array_length(const struct Expression *expr,
                                  CodeGenContext *ctx, long long *out_len);
int expr_is_char_pointer(const struct Expression *expr);
ListNode_t *codegen_expr_tree_value(struct Expression *expr,
                                    ListNode_t *inst_list, CodeGenContext *ctx,
                                    Register_t **out_reg);
ListNode_t *codegen_spill_reg64_temp(ListNode_t *inst_list, CodeGenContext *ctx,
                                     const Register_t *reg,
                                     const char *temp_name,
                                     StackNode_t **spill_slot);
ListNode_t *codegen_restore_spilled_reg64(ListNode_t *inst_list,
                                          CodeGenContext *ctx,
                                          const Register_t *reg,
                                          StackNode_t *spill_slot);

static Register_t *codegen_try_get_reg(ListNode_t **inst_list,
                                       CodeGenContext *ctx, const char *usage) {
  Register_t *reg = get_free_reg(get_reg_stack(), inst_list);
  if (reg == NULL)
    reg = get_reg_with_spill(get_reg_stack(), inst_list);
  if (reg == NULL)
    codegen_report_error(ctx, "ERROR: Unable to allocate register for %s.",
                         usage);
  return reg;
}

static int codegen_infer_set_storage_bytes(const struct Expression *expr,
                                           CodeGenContext *ctx) {
  if (expr == NULL)
    return 0;

  int best = 0;

  KgpcType *expr_type = expr_get_kgpc_type(expr);
  if (expr_type != NULL && kgpc_type_is_set(expr_type)) {
    long long size = kgpc_type_sizeof(expr_type);
    if (size > best)
      best = (int)size;
  }

  if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL && ctx != NULL &&
      ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
        node != NULL) {
      if (node->const_set_value != NULL && node->const_set_size > best)
        best = node->const_set_size;

      if (node->type != NULL && kgpc_type_is_set(node->type)) {
        long long size = kgpc_type_sizeof(node->type);
        if (size > best)
          best = (int)size;
      }
    }
  }

  /* A set binary operation (union/intersection/difference) may carry a
   * KgpcType whose sizeof under-reports the true storage (the result type is
   * often computed as the narrow 4-byte default even when an operand spans
   * ordinals >= 32).  Its true storage width is at least the widest of its
   * operands, so recurse and take the maximum -- never trust the node's own
   * narrow type alone.  This lets the `in` test recognise e.g.
   * (setA + [hi]) as a large set and materialise it via
   * codegen_char_set_address instead of truncating it into a 32-bit
   * register. */
  if (expr->type == EXPR_ADDOP && expr_has_type_tag(expr, SET_TYPE)) {
    int l =
        codegen_infer_set_storage_bytes(expr->expr_data.addop_data.left_expr,
                                        ctx);
    int r =
        codegen_infer_set_storage_bytes(expr->expr_data.addop_data.right_term,
                                        ctx);
    if (l > best)
      best = l;
    if (r > best)
      best = r;
  }

  if (expr->type == EXPR_MULOP && expr_has_type_tag(expr, SET_TYPE)) {
    int l =
        codegen_infer_set_storage_bytes(expr->expr_data.mulop_data.left_term,
                                        ctx);
    int r = codegen_infer_set_storage_bytes(
        expr->expr_data.mulop_data.right_factor, ctx);
    if (l > best)
      best = l;
    if (r > best)
      best = r;
  }

  return best;
}

static int codegen_expr_is_large_set_ctx(const struct Expression *expr,
                                         CodeGenContext *ctx) {
  if (expr == NULL)
    return 0;

  if (expr_is_char_set_ctx(expr, ctx))
    return 1;

  if (codegen_infer_set_storage_bytes(expr, ctx) > 4)
    return 1;

  if (expr->type == EXPR_ADDOP && expr_has_type_tag(expr, SET_TYPE)) {
    return codegen_expr_is_large_set_ctx(expr->expr_data.addop_data.left_expr,
                                         ctx) ||
           codegen_expr_is_large_set_ctx(expr->expr_data.addop_data.right_term,
                                         ctx);
  }

  if (expr->type == EXPR_MULOP && expr_has_type_tag(expr, SET_TYPE)) {
    return codegen_expr_is_large_set_ctx(expr->expr_data.mulop_data.left_term,
                                         ctx) ||
           codegen_expr_is_large_set_ctx(
               expr->expr_data.mulop_data.right_factor, ctx);
  }

  return 0;
}

/* Clamp inferred set width so `(bytes * 8 - 1)` always stays within signed-int
 * range used by immediate compare emission below. */
enum { CODEGEN_MAX_SET_STORAGE_BYTES = INT_MAX / 8 };

static ListNode_t *codegen_promote_char_reg_to_string(ListNode_t *inst_list,
                                                      CodeGenContext *ctx,
                                                      Register_t *value_reg) {
  if (value_reg == NULL)
    return inst_list;

  const char *arg_reg32 = current_arg_reg32(0);
  if (arg_reg32 == NULL)
    return inst_list;

  {
    char buffer_tmpl[128];
    snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovl\t%%0, %s\n", arg_reg32);
    Register_t *u[] = {value_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
  }
  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
  {
    /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_VREG, BE_W64, {.vreg = value_reg}};
    BeOperand src = {OPK_PHYS, BE_W64, {.phys = "%rax"}};
    kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
    inst_list = em.list;
  }
  free_arg_regs();
  return inst_list;
}

static inline const char *register_name_for_type(const Register_t *reg,
                                                 int type_tag) {
  if (reg == NULL)
    return NULL;
  return codegen_type_uses_qword(type_tag) ? reg->bit_64 : reg->bit_32;
}

static inline const char *
register_name_for_expr(const Register_t *reg, const struct Expression *expr) {
  if (expr == NULL)
    return register_name_for_type(reg, UNKNOWN_TYPE);
  return expr_uses_qword_kgpctype(expr) ? reg->bit_64 : reg->bit_32;
}

static inline int expression_uses_qword(const struct Expression *expr) {
  return expr_uses_qword_kgpctype(expr);
}

static inline const char *codegen_register_id_to_8bit(RegisterId_t reg_id) {
  switch (reg_id) {
  case REG_RAX:
    return "%al";
  case REG_RBX:
    return "%bl";
  case REG_RCX:
    return "%cl";
  case REG_RDX:
    return "%dl";
  case REG_RSI:
    return "%sil";
  case REG_RDI:
    return "%dil";
  case REG_RBP:
    return "%bpl";
  case REG_RSP:
    return "%spl";
  case REG_R8:
    return "%r8b";
  case REG_R9:
    return "%r9b";
  case REG_R10:
    return "%r10b";
  case REG_R11:
    return "%r11b";
  case REG_R12:
    return "%r12b";
  case REG_R13:
    return "%r13b";
  case REG_R14:
    return "%r14b";
  case REG_R15:
    return "%r15b";
  default:
    return NULL;
  }
}

static const char *codegen_register_name8(const Register_t *reg) {
  if (reg == NULL || reg->bit_64 == NULL)
    return NULL;
  return codegen_register_id_to_8bit(reg->reg_id);
}

typedef struct {
  int relop;         /* EQ, NE, LT, … */
  int inverse;       /* the logical inverse relop */
  const char *setcc; /* x86 SETcc mnemonic */
} RelopEntry;

static const RelopEntry relop_table[] = {
    {EQ, NE, "sete"},      {NE, EQ, "setne"},     {LT, GE, "setl"},
    {LE, GT, "setle"},     {GT, LE, "setg"},      {GE, LT, "setge"},
    {LT_U, GE_U, "setnz"}, {LE_U, GT_U, "setnz"}, {GT_U, LE_U, "setnz"},
    {GE_U, LT_U, "setnz"},
};
#define RELOP_TABLE_SIZE ((int)(sizeof(relop_table) / sizeof(relop_table[0])))

static int invert_relop_type(int relop_kind) {
  for (int i = 0; i < RELOP_TABLE_SIZE; i++) {
    if (relop_table[i].relop == relop_kind)
      return relop_table[i].inverse;
  }
  return relop_kind;
}

/* Code generation for simple relops */
ListNode_t *codegen_simple_relop(struct Expression *expr, ListNode_t *inst_list,
                                 CodeGenContext *ctx, int *relop_type) {
#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
#endif
  assert(expr != NULL);
  assert(expr->type == EXPR_RELOP);
  assert(ctx != NULL);

  CODEGEN_DEBUG("DEBUG: Generating simple relop\n");

  struct Expression *left_expr = expr->expr_data.relop_data.left;
  struct Expression *right_expr = expr->expr_data.relop_data.right;
  int relop_kind = expr->expr_data.relop_data.type;

  if (relop_type != NULL)
    *relop_type = relop_kind;

  char buffer[128];
  if (relop_kind == NOT) {
    int inner_type = NE;
    inst_list = codegen_condition_expr(left_expr, inst_list, ctx, &inner_type);
    if (relop_type != NULL)
      *relop_type = invert_relop_type(inner_type);
    return inst_list;
  }

  if (relop_kind == IN && right_expr != NULL &&
      expr_is_char_set_ctx(right_expr, ctx)) {
    if (relop_type != NULL)
      *relop_type = NE;

    Register_t *left_reg = NULL;
    inst_list = codegen_expr_with_result(left_expr, inst_list, ctx, &left_reg);
    if (codegen_had_error(ctx) || left_reg == NULL)
      return inst_list;

    Register_t *set_addr_reg = NULL;
    inst_list =
        codegen_char_set_address(right_expr, inst_list, ctx, &set_addr_reg);
    if (codegen_had_error(ctx) || set_addr_reg == NULL) {
      free_reg(get_reg_stack(), left_reg);
      return inst_list;
    }

    /* btl with memory operand auto-computes byte offset for bit positions
       beyond 31, so we can test any bit 0..255 directly. Only 2 regs needed.

       A char set holds 256 bits (0..255).  Pascal requires that any element
       outside the set's domain test as false, so an element value > 255 (or
       < 0) must NOT be fed to btl: btl with such an index would walk past the
       32-byte set storage and read arbitrary memory, yielding a garbage
       result.  Bound-check the element first and force false when it is out
       of the 0..255 domain. */
    char cs_oob[24];
    char cs_done[24];
    gen_label(cs_oob, sizeof(cs_oob), ctx);
    gen_label(cs_done, sizeof(cs_done), ctx);
    {
      Register_t *u[] = {left_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$0, %0\n");
    }
    snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", cs_oob);
    inst_list = add_inst(inst_list, buffer);
    {
      Register_t *u[] = {left_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$255, %0\n");
    }
    snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", cs_oob);
    inst_list = add_inst(inst_list, buffer);
    {
      char buffer_tmpl[128];
      snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tbtl\t%%0, (%s)\n",
               set_addr_reg->bit_64);
      Register_t *u[] = {left_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
    }
    /* Convert CF to ZF: sbb sets reg to -1 if CF (bit set), 0 if not */
    {
      Register_t *u[] = {left_reg, left_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\tsbbl\t%0, %1\n");
    }
    snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", cs_done);
    inst_list = add_inst(inst_list, buffer);
    /* out-of-domain: force the element register to 0 (false) */
    snprintf(buffer, sizeof(buffer), "%s:\n", cs_oob);
    inst_list = add_inst(inst_list, buffer);
    {
      Register_t *d[] = {left_reg};
      Register_t *u[] = {left_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\txorl\t%1, %0\n");
    }
    snprintf(buffer, sizeof(buffer), "%s:\n", cs_done);
    inst_list = add_inst(inst_list, buffer);
    {
      Register_t *u[] = {left_reg, left_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\ttestl\t%0, %1\n");
    }

    free_reg(get_reg_stack(), set_addr_reg);
    free_reg(get_reg_stack(), left_reg);
    return inst_list;
  }

  /* IN against a register-resident (4-byte) set.  Pascal requires elements
   * outside the set's element domain to test as false, so we must NOT use
   * register-form btl unconditionally — that wraps the index mod 32 and
   * would falsely report e.g. (33 in [1..18]) = true because bit 33 mod 32
   * aliases bit 1.  Bound-check elem against [0..31] and short-circuit to
   * false when out of range; only then do the btl. */
  if (relop_kind == IN) {
    if (relop_type != NULL)
      *relop_type = NE;

    /* Evaluate the left operand (element value) */
    Register_t *elem_reg = NULL;
    inst_list = codegen_expr_with_result(left_expr, inst_list, ctx, &elem_reg);
    if (codegen_had_error(ctx) || elem_reg == NULL)
      return inst_list;

    /* Spill elem to stack across potential function calls in set eval */
    StackNode_t *elem_spill = add_l_t("in_elem_spill");
    if (elem_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n",
               elem_reg->bit_32, elem_spill->offset);
      inst_list = add_inst(inst_list, buffer);
      free_reg(get_reg_stack(), elem_reg);
      elem_reg = NULL;
    }

    /* Evaluate the right operand (set) as a 4-byte VALUE in a register. */
    Register_t *set_val_reg = NULL;
    inst_list =
        codegen_expr_with_result(right_expr, inst_list, ctx, &set_val_reg);
    if (codegen_had_error(ctx) || set_val_reg == NULL) {
      if (elem_reg != NULL)
        free_reg(get_reg_stack(), elem_reg);
      return inst_list;
    }

    /* If right_expr is a var-parameter VAR_ID, set_val_reg holds the
     * parameter's pointer value, not the dereferenced set.  Dereference: load
     * the first 4 bytes of the pointed-to set into set_val_reg. */
    if (right_expr != NULL && right_expr->type == EXPR_VAR_ID &&
        right_expr->expr_data.id != NULL && ctx != NULL &&
        ctx->symtab != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, ctx->symtab, right_expr->expr_data.id) &&
          node != NULL && node->is_var_parameter) {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovl\t(%s), %s\n",
                 set_val_reg->bit_64, set_val_reg->bit_32);
        inst_list = add_inst(inst_list, buffer_tmpl);
      }
    }

    int set_storage_bytes = codegen_infer_set_storage_bytes(right_expr, ctx);
    int set_max_bit = 31;
    Register_t *set_addr_reg = NULL;
    if (set_storage_bytes > 4) {
      if (set_storage_bytes > CODEGEN_MAX_SET_STORAGE_BYTES)
        set_storage_bytes = CODEGEN_MAX_SET_STORAGE_BYTES;
      set_max_bit = set_storage_bytes * 8 - 1;

      free_reg(get_reg_stack(), set_val_reg);
      set_val_reg = NULL;
      inst_list =
          codegen_char_set_address(right_expr, inst_list, ctx, &set_addr_reg);
      if (codegen_had_error(ctx) || set_addr_reg == NULL) {
        if (elem_reg != NULL)
          free_reg(get_reg_stack(), elem_reg);
        return inst_list;
      }
    }

    /* Reload elem if spilled */
    if (elem_spill != NULL) {
      elem_reg = codegen_try_get_reg(&inst_list, ctx, "in_elem_reload");
      if (elem_reg == NULL) {
        free_reg(get_reg_stack(), set_val_reg);
        return inst_list;
      }
      snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n",
               elem_spill->offset, elem_reg->bit_32);
      inst_list = add_inst(inst_list, buffer);
    }

    char in_oob_label[18];
    char in_done_label[18];
    gen_label(in_oob_label, sizeof(in_oob_label), ctx);
    gen_label(in_done_label, sizeof(in_done_label), ctx);

    /* Out-of-domain (elem < 0 or elem > 31) → result is false. */
    {
      Register_t *u[] = {elem_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$0, %0\n");
    }
    snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", in_oob_label);
    inst_list = add_inst(inst_list, buffer);
    {
      char cmp_template[128];
      snprintf(cmp_template, sizeof(cmp_template), "\tcmpl\t$%d, %%0\n",
               set_max_bit);
      Register_t *u[] = {elem_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, cmp_template);
    }
    snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", in_oob_label);
    inst_list = add_inst(inst_list, buffer);

    if (set_addr_reg != NULL) {
      char btl_template[128];
      snprintf(btl_template, sizeof(btl_template), "\tbtl\t%%0, (%s)\n",
               set_addr_reg->bit_64);
      Register_t *u[] = {elem_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, btl_template);
    } else {
      snprintf(buffer, sizeof(buffer), "\tbtl\t%s, %s\n", elem_reg->bit_32,
               set_val_reg->bit_32);
      inst_list = add_inst(inst_list, buffer);
    }
    snprintf(buffer, sizeof(buffer), "\tsbbl\t%s, %s\n", elem_reg->bit_32,
             elem_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", in_done_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "%s:\n", in_oob_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\txorl\t%s, %s\n", elem_reg->bit_32,
             elem_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", in_done_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", elem_reg->bit_32,
             elem_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    if (set_addr_reg != NULL)
      free_reg(get_reg_stack(), set_addr_reg);
    if (set_val_reg != NULL)
      free_reg(get_reg_stack(), set_val_reg);
    free_reg(get_reg_stack(), elem_reg);
    return inst_list;
  }

  if ((relop_kind == EQ || relop_kind == NE) &&
      ((left_expr != NULL && codegen_expr_is_large_set_ctx(left_expr, ctx)) ||
       (right_expr != NULL &&
        codegen_expr_is_large_set_ctx(right_expr, ctx)))) {
    Register_t *left_addr = NULL;
    Register_t *right_addr = NULL;
    Register_t *acc = NULL;
    Register_t *tmp = NULL;
    StackNode_t *left_addr_spill = NULL;

    inst_list = codegen_char_set_address(left_expr, inst_list, ctx, &left_addr);
    if (codegen_had_error(ctx) || left_addr == NULL)
      return inst_list;

    left_addr_spill = add_l_t("relop_set_laddr");
    if (left_addr_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
               left_addr->bit_64, left_addr_spill->offset);
      inst_list = add_inst(inst_list, buffer);
      free_reg(get_reg_stack(), left_addr);
      left_addr = NULL;
    }

    inst_list =
        codegen_char_set_address(right_expr, inst_list, ctx, &right_addr);
    if (codegen_had_error(ctx) || right_addr == NULL) {
      if (left_addr != NULL)
        free_reg(get_reg_stack(), left_addr);
      return inst_list;
    }

    if (left_addr_spill != NULL) {
      left_addr =
          codegen_try_get_reg(&inst_list, ctx, "relop_set_laddr_reload");
      if (left_addr == NULL) {
        free_reg(get_reg_stack(), right_addr);
        return inst_list;
      }
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
               left_addr_spill->offset, left_addr->bit_64);
      inst_list = add_inst(inst_list, buffer);
    }

    acc = codegen_try_get_reg(&inst_list, ctx, "relop_set_acc");
    tmp = codegen_try_get_reg(&inst_list, ctx, "relop_set_tmp");
    if (acc == NULL || tmp == NULL) {
      if (tmp != NULL)
        free_reg(get_reg_stack(), tmp);
      if (acc != NULL)
        free_reg(get_reg_stack(), acc);
      free_reg(get_reg_stack(), left_addr);
      free_reg(get_reg_stack(), right_addr);
      return inst_list;
    }

    snprintf(buffer, sizeof(buffer), "\txorq\t%s, %s\n", acc->bit_64,
             acc->bit_64);
    inst_list = add_inst(inst_list, buffer);
    for (int i = 0; i < 4; ++i) {
      int byte_off = i * 8;
      snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%s), %s\n", byte_off,
               left_addr->bit_64, tmp->bit_64);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\txorq\t%d(%s), %s\n", byte_off,
               right_addr->bit_64, tmp->bit_64);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\torq\t%s, %s\n", tmp->bit_64,
               acc->bit_64);
      inst_list = add_inst(inst_list, buffer);
    }
    snprintf(buffer, sizeof(buffer), "\ttestq\t%s, %s\n", acc->bit_64,
             acc->bit_64);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), tmp);
    free_reg(get_reg_stack(), acc);
    free_reg(get_reg_stack(), left_addr);
    free_reg(get_reg_stack(), right_addr);
    return inst_list;
  }

  /* A comparison against the nil literal is a pointer/reference test, never a
   * textual one.  A dynamic `array of char` (FPC's TAnsiCharDynArray, e.g.
   * ogcoff's FCoffStrs) is classified char-array-like further down and
   * materialised as its *address* (always nonzero), then routed through
   * kgpc_string_compare -- so `(FCoffStrs = nil)` could never be true, which
   * broke the win64 internal linker's COFF string-table reader.  Plain
   * pointers and non-char dynamic arrays already reach the generic
   * integer/pointer compare correctly; handle the char-array-like case here by
   * loading the reference's pointer value (the deref of its storage slot) and
   * comparing it to 0.  Flags are left set the same way the generic EQ/NE path
   * leaves them, so the caller's sete/setne conversion is unchanged. */
  if ((relop_kind == EQ || relop_kind == NE)) {
    struct Expression *ref_side = NULL;
    if (left_expr != NULL && left_expr->type == EXPR_NIL)
      ref_side = right_expr;
    else if (right_expr != NULL && right_expr->type == EXPR_NIL)
      ref_side = left_expr;
    if (ref_side != NULL && ref_side->type != EXPR_NIL &&
        codegen_expr_is_char_array_like_ctx(ref_side, ctx) &&
        codegen_expr_is_addressable(ref_side) &&
        ref_side->resolved_kgpc_type != NULL &&
        kgpc_type_is_dynamic_array(ref_side->resolved_kgpc_type)) {
      /* Restrict the slot-dereference compare to dynamic (reference-backed)
       * char arrays.  A fixed `array[0..N] of char` is char-array-like and
       * addressable too, but has no reference slot — dereferencing it would
       * compare its first qword of contents to 0, so a zero-filled buffer
       * would spuriously equal nil.  The analogous fast path in
       * expr_tree_op.c carries the same kgpc_type_is_dynamic_array guard. */
      Register_t *addr_reg = NULL;
      inst_list = codegen_address_for_expr(ref_side, inst_list, ctx, &addr_reg);
      if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;
      snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64,
               addr_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tcmpq\t$0, %s\n", addr_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      free_reg(get_reg_stack(), addr_reg);
      if (relop_type != NULL)
        *relop_type = relop_kind;
      return inst_list;
    }
  }

  /* For floating-point comparisons, use expr_tree-based evaluation with
   * spilling to preserve the left operand across function calls that may occur
   * when evaluating the right operand. Without this, caller-saved registers
   * like %rax would be clobbered by subsequent function calls.
   */
  int left_is_real =
      (left_expr != NULL && expr_has_type_tag(left_expr, REAL_TYPE));
  int right_is_real =
      (right_expr != NULL && expr_has_type_tag(right_expr, REAL_TYPE));

  Register_t *left_reg = NULL;
  Register_t *right_reg = NULL;
  StackNode_t *left_spill = NULL;

  if (left_is_real || right_is_real) {
    /* Evaluate left operand using expr_tree which properly handles results */
    inst_list = codegen_expr_with_result(left_expr, inst_list, ctx, &left_reg);
    if (codegen_had_error(ctx) || left_reg == NULL)
      return inst_list;

    /* Spill left result to stack to preserve it across the right operand
     * evaluation which may involve function calls that clobber caller-saved
     * registers */
    left_spill = add_l_t("relop_left_spill");
    if (left_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
               left_reg->bit_64, left_spill->offset);
      inst_list = add_inst(inst_list, buffer);

      /* Free left_reg temporarily; we will get a fresh register after right
       * eval */
      free_reg(get_reg_stack(), left_reg);
      left_reg = NULL;
    }

    /* Evaluate right operand */
    inst_list =
        codegen_expr_with_result(right_expr, inst_list, ctx, &right_reg);
    if (codegen_had_error(ctx) || right_reg == NULL) {
      if (left_reg != NULL)
        free_reg(get_reg_stack(), left_reg);
      return inst_list;
    }

    /* If we spilled the left operand, get a fresh register and reload it.
     * Otherwise, the original left_reg is still live. */
    if (left_spill != NULL) {
      left_reg = codegen_try_get_reg(&inst_list, ctx, "relop_left_reload");
      if (left_reg == NULL) {
        free_reg(get_reg_stack(), right_reg);
        return inst_list;
      }

      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
               left_spill->offset, left_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
    }
  } else {
    /* Non-floating-point comparisons: evaluating the right operand may need
     * scratch registers and force the spill allocator to reuse the left
     * operand's physical register.  Materialize the left value first and
     * reload it just before the cmp so the compare never observes a stale
     * register. */
    inst_list = codegen_expr_with_result(left_expr, inst_list, ctx, &left_reg);
    if (codegen_had_error(ctx) || left_reg == NULL)
      return inst_list;

    const int use_qword_spill =
        expression_uses_qword(left_expr) || expression_uses_qword(right_expr);
    StackNode_t *left_int_spill = add_l_t("relop_left_spill");
    if (left_int_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmov%c\t%s, -%d(%%rbp)\n",
               use_qword_spill ? 'q' : 'l',
               use_qword_spill ? left_reg->bit_64 : left_reg->bit_32,
               left_int_spill->offset);
      inst_list = add_inst(inst_list, buffer);
      free_reg(get_reg_stack(), left_reg);
      left_reg = NULL;
    }

    inst_list =
        codegen_expr_with_result(right_expr, inst_list, ctx, &right_reg);
    if (codegen_had_error(ctx) || right_reg == NULL) {
      if (left_reg != NULL)
        free_reg(get_reg_stack(), left_reg);
      return inst_list;
    }

    if (left_int_spill != NULL) {
      left_reg = codegen_try_get_reg(&inst_list, ctx, "relop_left_reload");
      if (left_reg == NULL) {
        free_reg(get_reg_stack(), right_reg);
        return inst_list;
      }
      snprintf(buffer, sizeof(buffer), "\tmov%c\t-%d(%%rbp), %s\n",
               use_qword_spill ? 'q' : 'l', left_int_spill->offset,
               use_qword_spill ? left_reg->bit_64 : left_reg->bit_32);
      inst_list = add_inst(inst_list, buffer);
    }
  }

  if (left_reg == NULL || right_reg == NULL)
    return inst_list;

  int left_is_shortstring =
      (left_expr != NULL &&
       codegen_expr_is_shortstring_value_ctx(left_expr, ctx));
  int right_is_shortstring =
      (right_expr != NULL &&
       codegen_expr_is_shortstring_value_ctx(right_expr, ctx));
  if (codegen_expr_function_call_returns_ansistring(ctx, left_expr))
    left_is_shortstring = 0;
  if (codegen_expr_function_call_returns_ansistring(ctx, right_expr))
    right_is_shortstring = 0;
  if ((left_is_shortstring || right_is_shortstring) &&
      pascal_frontend_default_shortstring()) {
    if (!left_is_shortstring && left_expr != NULL &&
        !codegen_expr_function_call_returns_ansistring(ctx, left_expr) &&
        expr_has_type_tag(left_expr, STRING_TYPE))
      left_is_shortstring = 1;
    if (!right_is_shortstring && right_expr != NULL &&
        !codegen_expr_function_call_returns_ansistring(ctx, right_expr) &&
        expr_has_type_tag(right_expr, STRING_TYPE))
      right_is_shortstring = 1;
  }
  int left_is_char_array =
      (left_expr != NULL &&
       codegen_expr_is_char_array_like_ctx(left_expr, ctx) &&
       !left_is_shortstring);
  int right_is_char_array =
      (right_expr != NULL &&
       codegen_expr_is_char_array_like_ctx(right_expr, ctx) &&
       !right_is_shortstring);

  int left_is_string =
      (left_expr != NULL && (expr_has_type_tag(left_expr, STRING_TYPE) ||
                             left_is_shortstring || left_is_char_array));
  int right_is_string =
      (right_expr != NULL && (expr_has_type_tag(right_expr, STRING_TYPE) ||
                              right_is_shortstring || right_is_char_array));
  if (left_is_char_array && left_expr != NULL &&
      left_expr->type == EXPR_ARRAY_ACCESS) {
    long long char_len = 0;
    if (codegen_get_char_array_length(left_expr, ctx, &char_len) &&
        char_len > 1 && char_len <= 256) {
      left_is_shortstring = 1;
      left_is_char_array = 0;
      left_is_string = 1;
    }
  }
  if (right_is_char_array && right_expr != NULL &&
      right_expr->type == EXPR_ARRAY_ACCESS) {
    long long char_len = 0;
    if (codegen_get_char_array_length(right_expr, ctx, &char_len) &&
        char_len > 1 && char_len <= 256) {
      right_is_shortstring = 1;
      right_is_char_array = 0;
      right_is_string = 1;
    }
  }
  int left_is_char_ptr = expr_is_char_pointer(left_expr);
  int right_is_char_ptr = expr_is_char_pointer(right_expr);

  /* When comparing a string to a char (EXPR_CHAR_CODE or single-char literal),
   * promote the char operand to a string via kgpc_char_to_string so the
   * comparison uses kgpc_string_compare.  This fixes segfaults from passing
   * raw char integers where string pointers are expected.
   *
   * The semantic checker may promote EXPR_CHAR_CODE to STRING_TYPE for equality
   * comparisons, so right_is_string can be TRUE even when the register holds
   * a raw char integer.  Detect this by checking the expression node type. */
  int right_needs_char_promo =
      (right_expr != NULL &&
       (right_expr->type == EXPR_CHAR_CODE ||
        expr_get_type_tag(right_expr) == CHAR_TYPE ||
        (right_expr->resolved_kgpc_type != NULL &&
         kgpc_type_is_char(right_expr->resolved_kgpc_type)) ||
        codegen_expr_is_string_char_index(right_expr)) &&
       !(right_expr != NULL &&
         codegen_expr_is_shortstring_value_ctx(right_expr, ctx) &&
         !codegen_expr_is_string_char_index(right_expr)));
  int left_needs_char_promo =
      (left_expr != NULL &&
       (left_expr->type == EXPR_CHAR_CODE ||
        expr_get_type_tag(left_expr) == CHAR_TYPE ||
        (left_expr->resolved_kgpc_type != NULL &&
         kgpc_type_is_char(left_expr->resolved_kgpc_type)) ||
        codegen_expr_is_string_char_index(left_expr)) &&
       !(left_expr != NULL &&
         codegen_expr_is_shortstring_value_ctx(left_expr, ctx) &&
         !codegen_expr_is_string_char_index(left_expr)));
  if ((left_is_string || right_is_string) && right_needs_char_promo) {
    StackNode_t *lhs_spill = add_l_t("relop_str_char_lhs");
    if (lhs_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
               left_reg->bit_64, lhs_spill->offset);
      inst_list = add_inst(inst_list, buffer);
    }
    const char *arg_reg32 = codegen_target_is_windows() ? "%ecx" : "%edi";
    {
      char buffer_tmpl[128];
      snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovl\t%%0, %s\n",
               arg_reg32);
      Register_t *u[] = {right_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
    }
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list =
        codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
    {
      /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_VREG, BE_W64, {.vreg = right_reg}};
      BeOperand src = {OPK_PHYS, BE_W64, {.phys = "%rax"}};
      kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
      inst_list = em.list;
    }
    free_arg_regs();
    if (lhs_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
               lhs_spill->offset, left_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
    }
    right_is_string = 1;
    right_needs_char_promo = 0;
  }
  if ((left_is_string || right_is_string) && left_needs_char_promo) {
    StackNode_t *rhs_spill = add_l_t("relop_str_char_rhs");
    if (rhs_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
               right_reg->bit_64, rhs_spill->offset);
      inst_list = add_inst(inst_list, buffer);
    }
    const char *arg_reg32 = codegen_target_is_windows() ? "%ecx" : "%edi";
    {
      char buffer_tmpl[128];
      snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovl\t%%0, %s\n",
               arg_reg32);
      Register_t *u[] = {left_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
    }
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list =
        codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
    {
      /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_VREG, BE_W64, {.vreg = left_reg}};
      BeOperand src = {OPK_PHYS, BE_W64, {.phys = "%rax"}};
      kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
      inst_list = em.list;
    }
    free_arg_regs();
    if (rhs_spill != NULL) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
               rhs_spill->offset, right_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
    }
    left_is_string = 1;
    left_needs_char_promo = 0;
  }

  if ((left_is_char_array || right_is_char_array) &&
      (left_is_string || right_is_string || left_is_char_ptr ||
       right_is_char_ptr)) {
    long long array_len = 0;
    long long rhs_array_len = 0;
    int invert_cmp = 0;
    const char *cmp_func = "kgpc_char_array_compare";
    int compare_full =
        ((left_is_char_array && right_is_string && !right_is_char_ptr) ||
         (right_is_char_array && left_is_string && !left_is_char_ptr));

    if (left_is_char_array && right_is_char_array) {
      if (!codegen_get_char_array_length(left_expr, ctx, &array_len) ||
          !codegen_get_char_array_length(right_expr, ctx, &rhs_array_len)) {
        free_reg(get_reg_stack(), right_reg);
        free_reg(get_reg_stack(), left_reg);
        if (relop_type != NULL)
          *relop_type = relop_kind;
        return inst_list;
      }
      cmp_func = "kgpc_char_array_compare_array";
    } else if (left_is_char_array) {
      if (!codegen_get_char_array_length(left_expr, ctx, &array_len)) {
        free_reg(get_reg_stack(), right_reg);
        free_reg(get_reg_stack(), left_reg);
        if (relop_type != NULL)
          *relop_type = relop_kind;
        return inst_list;
      }
    } else {
      if (!codegen_get_char_array_length(right_expr, ctx, &array_len)) {
        free_reg(get_reg_stack(), right_reg);
        free_reg(get_reg_stack(), left_reg);
        if (relop_type != NULL)
          *relop_type = relop_kind;
        return inst_list;
      }
      invert_cmp = 1;
    }

    if (left_is_shortstring) {
      StackNode_t *right_preserve = NULL;
      if (right_reg != NULL) {
        right_preserve = add_l_t("relop_right_preserve");
        if (right_preserve != NULL) {
          snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                   right_reg->bit_64, right_preserve->offset);
          inst_list = add_inst(inst_list, buffer);
        }
      }
      inst_list = codegen_promote_shortstring_reg(inst_list, ctx, left_reg);
      if (right_preserve != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 right_preserve->offset, right_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
      }
    }
    if (right_is_shortstring) {
      StackNode_t *left_preserve = NULL;
      if (left_reg != NULL) {
        left_preserve = add_l_t("relop_left_preserve");
        if (left_preserve != NULL) {
          snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                   left_reg->bit_64, left_preserve->offset);
          inst_list = add_inst(inst_list, buffer);
        }
      }
      inst_list = codegen_promote_shortstring_reg(inst_list, ctx, right_reg);
      if (left_preserve != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                 left_preserve->offset, left_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
      }
    }

    const char *arg0 = current_arg_reg64(0);
    const char *arg1 = current_arg_reg64(1);
    const char *arg2 = current_arg_reg64(2);
    const char *arg3 = current_arg_reg64(3);
    if (arg0 == NULL || arg1 == NULL ||
        (strcmp(cmp_func, "kgpc_char_array_compare_array") == 0 &&
         (arg2 == NULL || arg3 == NULL))) {
      free_reg(get_reg_stack(), right_reg);
      free_reg(get_reg_stack(), left_reg);
      if (relop_type != NULL)
        *relop_type = relop_kind;
      return inst_list;
    }

    if (strcmp(cmp_func, "kgpc_char_array_compare_array") == 0) {
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", arg0);
        Register_t *u[] = {left_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }
      snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", array_len, arg1);
      inst_list = add_inst(inst_list, buffer);
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", arg2);
        Register_t *u[] = {right_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }
      snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", rhs_array_len,
               arg3);
      inst_list = add_inst(inst_list, buffer);
    } else if (!invert_cmp) {
      if (compare_full)
        cmp_func = "kgpc_char_array_compare_full";
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", arg0);
        Register_t *u[] = {left_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }
      snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", array_len, arg1);
      inst_list = add_inst(inst_list, buffer);
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", arg2);
        Register_t *u[] = {right_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }
    } else {
      if (compare_full)
        cmp_func = "kgpc_char_array_compare_full";
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", arg0);
        Register_t *u[] = {right_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }
      snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", array_len, arg1);
      inst_list = add_inst(inst_list, buffer);
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", arg2);
        Register_t *u[] = {left_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", cmp_func);
    inst_list = add_inst(inst_list, buffer);
    if (invert_cmp)
      inst_list = add_inst(inst_list, "\tnegl\t%eax\n");
    {
      Register_t *d[] = {left_reg};
      inst_list =
          add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovl\t%eax, %0\n");
    }
    free_arg_regs();

    free_reg(get_reg_stack(), right_reg);
    {
      Register_t *u[] = {left_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$0, %0\n");
    }
    free_reg(get_reg_stack(), left_reg);

    if (relop_type != NULL)
      *relop_type = relop_kind;
    return inst_list;
  }

  if ((left_is_string && right_is_string) ||
      (left_is_string && right_is_char_ptr) ||
      (right_is_string && left_is_char_ptr)) {
    const char *lhs_arg = current_arg_reg64(0);
    const char *rhs_arg = current_arg_reg64(1);
    if (lhs_arg == NULL || rhs_arg == NULL) {
      free_reg(get_reg_stack(), right_reg);
      free_reg(get_reg_stack(), left_reg);
      if (relop_type != NULL)
        *relop_type = relop_kind;
      return inst_list;
    }

    if (left_is_shortstring) {
      StackNode_t *rhs_spill = NULL;
      inst_list = codegen_spill_reg64_temp(
          inst_list, ctx, right_reg, "relop_shortstr_rhs_spill", &rhs_spill);
      inst_list = codegen_promote_shortstring_reg(inst_list, ctx, left_reg);
      inst_list =
          codegen_restore_spilled_reg64(inst_list, ctx, right_reg, rhs_spill);
    }
    if (right_is_shortstring) {
      StackNode_t *lhs_spill = NULL;
      inst_list = codegen_spill_reg64_temp(
          inst_list, ctx, left_reg, "relop_shortstr_lhs_spill", &lhs_spill);
      inst_list = codegen_promote_shortstring_reg(inst_list, ctx, right_reg);
      inst_list =
          codegen_restore_spilled_reg64(inst_list, ctx, left_reg, lhs_spill);
    }

    /* Promote char-typed operands (EXPR_CHAR_CODE or single-char EXPR_STRING)
     * that hold raw integer values to string pointers before calling
     * kgpc_string_compare. The semantic checker may set resolved_kgpc_type
     * to STRING_TYPE but the expression evaluator still loads char integers. */
    if (left_needs_char_promo) {
      StackNode_t *rhs_spill = NULL;
      inst_list = codegen_spill_reg64_temp(inst_list, ctx, right_reg,
                                           "relop_rhs_char_promo", &rhs_spill);
      inst_list = codegen_promote_char_reg_to_string(inst_list, ctx, left_reg);
      inst_list =
          codegen_restore_spilled_reg64(inst_list, ctx, right_reg, rhs_spill);
    }
    if (right_needs_char_promo) {
      StackNode_t *lhs_spill = NULL;
      inst_list = codegen_spill_reg64_temp(inst_list, ctx, left_reg,
                                           "relop_lhs_char_promo", &lhs_spill);
      inst_list = codegen_promote_char_reg_to_string(inst_list, ctx, right_reg);
      inst_list =
          codegen_restore_spilled_reg64(inst_list, ctx, left_reg, lhs_spill);
    }

    {
      char buffer_tmpl[128];
      snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", rhs_arg);
      Register_t *u[] = {right_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
    }
    {
      char buffer_tmpl[128];
      snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", lhs_arg);
      Register_t *u[] = {left_reg};
      inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list =
        codegen_call_with_shadow_space(inst_list, "kgpc_string_compare");
    {
      Register_t *d[] = {left_reg};
      inst_list =
          add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovl\t%eax, %0\n");
    }
    free_arg_regs();

    free_reg(get_reg_stack(), right_reg);
    {
      Register_t *u[] = {left_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$0, %0\n");
    }
    free_reg(get_reg_stack(), left_reg);

    if (relop_type != NULL)
      *relop_type = relop_kind;
    return inst_list;
  }

  if (relop_kind == IN) {
    if (relop_type != NULL)
      *relop_type = NE;

    /* Check if this is a character set IN operation (32-byte set for values
     * 0..255) vs. a small integer set (4-byte bitmask for values 0..31).
     *
     * Character sets require memory-based indexing (32-byte array).
     * Small integer sets can use simple btl instruction on a 32-bit register.
     */
    HashNode_t *right_const_set = NULL;
    int right_is_char_set = 0; /* Only set to 1 if actually a char set */

    if (right_expr != NULL && right_expr->type == EXPR_VAR_ID && ctx != NULL &&
        ctx->symtab != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, ctx->symtab, right_expr->expr_data.id) &&
          node != NULL && node->hash_type == HASHTYPE_CONST &&
          node->const_set_value != NULL && node->const_set_size > 0) {
        right_const_set = node;
        /* Check if this is a char set (32 bytes) or small set (4 bytes) */
        right_is_char_set = (node->const_set_size > 4);
      }
    }

    /* For set expressions (literals and variables), check if they are char sets
     */
    if (!right_is_char_set && right_expr != NULL) {
      right_is_char_set = expr_is_char_set_ctx(right_expr, ctx);
    }

    if (right_const_set != NULL && right_const_set->const_set_value != NULL) {
      Register_t *set_addr_reg = NULL;

      inst_list =
          codegen_emit_const_set_rodata(right_const_set, inst_list, ctx);
      if (codegen_had_error(ctx) || right_const_set->const_set_label == NULL) {
        free_reg(get_reg_stack(), left_reg);
        free_reg(get_reg_stack(), right_reg);
        return inst_list;
      }

      set_addr_reg = codegen_try_get_reg(&inst_list, ctx, "const set addr");
      if (set_addr_reg == NULL) {
        free_reg(get_reg_stack(), left_reg);
        free_reg(get_reg_stack(), right_reg);
        return inst_list;
      }

      char buffer_addr[96];
      snprintf(buffer_addr, sizeof(buffer_addr), "\tleaq\t%s(%%rip), %s\n",
               right_const_set->const_set_label, set_addr_reg->bit_64);
      inst_list = add_inst(inst_list, buffer_addr);

      /* Bound-check elem against the actual storage size — bits beyond
       * 8 * const_set_size don't exist and reading them would walk past
       * the rodata.  Out-of-domain → result is false. */
      char cs_oob[18];
      char cs_done[18];
      gen_label(cs_oob, sizeof(cs_oob), ctx);
      gen_label(cs_done, sizeof(cs_done), ctx);
      int cs_max_bit = (int)(right_const_set->const_set_size) * 8 - 1;
      if (cs_max_bit < 0)
        cs_max_bit = 0;

      {
        Register_t *u[] = {left_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$0, %0\n");
      }
      snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", cs_oob);
      inst_list = add_inst(inst_list, buffer);
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tcmpl\t$%d, %%0\n",
                 cs_max_bit);
        Register_t *u[] = {left_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }
      snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", cs_oob);
      inst_list = add_inst(inst_list, buffer);

      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tbtl\t%%0, (%s)\n",
                 set_addr_reg->bit_64);
        Register_t *u[] = {left_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }
      {
        Register_t *u[] = {left_reg, left_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\tsbbl\t%0, %1\n");
      }
      snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", cs_done);
      inst_list = add_inst(inst_list, buffer);

      snprintf(buffer, sizeof(buffer), "%s:\n", cs_oob);
      inst_list = add_inst(inst_list, buffer);
      {
        Register_t *d[] = {left_reg};
        Register_t *u[] = {left_reg};
        inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\txorl\t%1, %0\n");
      }
      snprintf(buffer, sizeof(buffer), "%s:\n", cs_done);
      inst_list = add_inst(inst_list, buffer);

      {
        Register_t *u[] = {left_reg, left_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\ttestl\t%0, %1\n");
      }

      free_reg(get_reg_stack(), set_addr_reg);
      free_reg(get_reg_stack(), left_reg);
      free_reg(get_reg_stack(), right_reg);
      return inst_list;
    }

    if (right_is_char_set) {
      /* Char sets are 32 bytes = 256 bits.  The char value is already
       * naturally in [0..255] for AnsiChar; for wider element types the
       * bound check below short-circuits out-of-domain to false. */

      Register_t *set_addr_reg = NULL;
      inst_list =
          codegen_char_set_address(right_expr, inst_list, ctx, &set_addr_reg);
      if (codegen_had_error(ctx) || set_addr_reg == NULL) {
        if (set_addr_reg != NULL)
          free_reg(get_reg_stack(), set_addr_reg);
        free_reg(get_reg_stack(), left_reg);
        free_reg(get_reg_stack(), right_reg);
        return inst_list;
      }

      char ch_oob[18];
      char ch_done[18];
      gen_label(ch_oob, sizeof(ch_oob), ctx);
      gen_label(ch_done, sizeof(ch_done), ctx);

      {
        Register_t *u[] = {left_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$0, %0\n");
      }
      snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", ch_oob);
      inst_list = add_inst(inst_list, buffer);
      {
        Register_t *u[] = {left_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$255, %0\n");
      }
      snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", ch_oob);
      inst_list = add_inst(inst_list, buffer);

      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tbtl\t%%0, (%s)\n",
                 set_addr_reg->bit_64);
        Register_t *u[] = {left_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }
      {
        Register_t *u[] = {left_reg, left_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\tsbbl\t%0, %1\n");
      }
      snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", ch_done);
      inst_list = add_inst(inst_list, buffer);

      snprintf(buffer, sizeof(buffer), "%s:\n", ch_oob);
      inst_list = add_inst(inst_list, buffer);
      {
        Register_t *d[] = {left_reg};
        Register_t *u[] = {left_reg};
        inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\txorl\t%1, %0\n");
      }
      snprintf(buffer, sizeof(buffer), "%s:\n", ch_done);
      inst_list = add_inst(inst_list, buffer);

      {
        Register_t *u[] = {left_reg, left_reg};
        inst_list =
            add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\ttestl\t%0, %1\n");
      }

      free_reg(get_reg_stack(), set_addr_reg);
      free_reg(get_reg_stack(), left_reg);
      free_reg(get_reg_stack(), right_reg);
      return inst_list;
    }

    /* Small (4-byte register-resident) integer/enum set: bits beyond 31
     * don't exist in the storage.  Bound-check elem against [0..31] and
     * short-circuit out-of-domain to false; spill the set as qword so
     * btl(mem) can be used safely for 0..31. */
    {
      char es_oob[18];
      char es_done[18];
      gen_label(es_oob, sizeof(es_oob), ctx);
      gen_label(es_done, sizeof(es_done), ctx);

      StackNode_t *set_spill = add_l_t("in_set_spill");
      if (set_spill != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                 right_reg->bit_64, set_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), right_reg);

        {
          Register_t *u[] = {left_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$0, %0\n");
        }
        snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", es_oob);
        inst_list = add_inst(inst_list, buffer);
        {
          Register_t *u[] = {left_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$31, %0\n");
        }
        snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", es_oob);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "\tbtl\t%s, -%d(%%rbp)\n",
                 left_reg->bit_32, set_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        {
          Register_t *u[] = {left_reg, left_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\tsbbl\t%0, %1\n");
        }
        snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", es_done);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "%s:\n", es_oob);
        inst_list = add_inst(inst_list, buffer);
        {
          Register_t *d[] = {left_reg};
          Register_t *u[] = {left_reg};
          inst_list =
              add_inst_du(inst_list, ctx, d, 1, u, 1, "\txorl\t%1, %0\n");
        }
        snprintf(buffer, sizeof(buffer), "%s:\n", es_done);
        inst_list = add_inst(inst_list, buffer);
      } else {
        {
          Register_t *u[] = {left_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$0, %0\n");
        }
        snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", es_oob);
        inst_list = add_inst(inst_list, buffer);
        {
          Register_t *u[] = {left_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tcmpl\t$31, %0\n");
        }
        snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", es_oob);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "\tbtl\t%s, %s\n", left_reg->bit_32,
                 right_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), right_reg);
        {
          Register_t *u[] = {left_reg, left_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\tsbbl\t%0, %1\n");
        }
        snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", es_done);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "%s:\n", es_oob);
        inst_list = add_inst(inst_list, buffer);
        {
          Register_t *d[] = {left_reg};
          Register_t *u[] = {left_reg};
          inst_list =
              add_inst_du(inst_list, ctx, d, 1, u, 1, "\txorl\t%1, %0\n");
        }
        snprintf(buffer, sizeof(buffer), "%s:\n", es_done);
        inst_list = add_inst(inst_list, buffer);
      }
    }
    {
      Register_t *u[] = {left_reg, left_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\ttestl\t%0, %1\n");
    }

    free_reg(get_reg_stack(), left_reg);
    return inst_list;
  }

  if (left_expr != NULL && expr_has_type_tag(left_expr, REAL_TYPE)) {
    char true_label[32];
    char done_label[32];
    gen_label(true_label, sizeof(true_label), ctx);
    gen_label(done_label, sizeof(done_label), ctx);
    const char *left_name = register_name_for_type(left_reg, REAL_TYPE);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm1\n", left_name);
    inst_list = add_inst(inst_list, buffer);

    int right_declared_integer_like = 0;
    if (right_expr != NULL && right_expr->type == EXPR_VAR_ID &&
        right_expr->expr_data.id != NULL && ctx != NULL &&
        ctx->symtab != NULL) {
      HashNode_t *right_node = NULL;
      if (FindSymbol(&right_node, ctx->symtab, right_expr->expr_data.id) != 0 &&
          right_node != NULL && right_node->type != NULL) {
        int right_declared_tag = codegen_tag_from_kgpc(right_node->type);
        if (is_integer_type(right_declared_tag) || right_declared_tag == BOOL ||
            right_declared_tag == CHAR_TYPE ||
            right_declared_tag == ENUM_TYPE) {
          right_declared_integer_like = 1;
        }
      }
    }

    if (right_expr != NULL && expr_has_type_tag(right_expr, REAL_TYPE) &&
        !right_declared_integer_like) {
      /* Values in GP registers are always promoted to double (64-bit),
       * even when the declared type is Single. Use movq, not movd. */
      const char *right_name = register_name_for_type(right_reg, REAL_TYPE);
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", right_name);
      inst_list = add_inst(inst_list, buffer);
    } else {
      int right_tag =
          (right_expr != NULL) ? expr_get_type_tag(right_expr) : UNKNOWN_TYPE;
      {
        char buffer_tmpl[128];
        if (codegen_type_uses_qword(right_tag))
          snprintf(buffer_tmpl, sizeof(buffer_tmpl),
                   "\tcvtsi2sdq\t%%0, %%xmm0\n");
        else
          snprintf(buffer_tmpl, sizeof(buffer_tmpl),
                   "\tcvtsi2sdl\t%%0, %%xmm0\n");
        Register_t *u[] = {right_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }
    }
    {
      Register_t *d[] = {left_reg};
      Register_t *u[] = {left_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\txorl\t%1, %0\n");
    }
    inst_list = add_inst(inst_list, "\tucomisd\t%xmm0, %xmm1\n");

    int relop_kind = expr->expr_data.relop_data.type;
    switch (relop_kind) {
    case EQ:
      snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tje\t%s\n", true_label);
      inst_list = add_inst(inst_list, buffer);
      break;
    case NE:
      snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", true_label);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tjne\t%s\n", true_label);
      inst_list = add_inst(inst_list, buffer);
      break;
    case LT:
      snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tjb\t%s\n", true_label);
      inst_list = add_inst(inst_list, buffer);
      break;
    case LE:
      snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tjbe\t%s\n", true_label);
      inst_list = add_inst(inst_list, buffer);
      break;
    case GT:
      snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tja\t%s\n", true_label);
      inst_list = add_inst(inst_list, buffer);
      break;
    case GE:
      snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tjae\t%s\n", true_label);
      inst_list = add_inst(inst_list, buffer);
      break;
    default:
      break;
    }

    snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", true_label);
    inst_list = add_inst(inst_list, buffer);
    {
      Register_t *d[] = {left_reg};
      inst_list =
          add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovl\t$1, %0\n");
    }
    snprintf(buffer, sizeof(buffer), "%s:\n", done_label);
    inst_list = add_inst(inst_list, buffer);
    {
      Register_t *u[] = {left_reg, left_reg};
      inst_list =
          add_inst_du(inst_list, ctx, NULL, 0, u, 2, "\ttestl\t%0, %1\n");
    }

    if (relop_type != NULL)
      *relop_type = NE;

    free_reg(get_reg_stack(), right_reg);
    free_reg(get_reg_stack(), left_reg);
    return inst_list;
  }

  int use_qword =
      expression_uses_qword(left_expr) || expression_uses_qword(right_expr);
  /* When either operand is 64-bit (pointers, qword ints), make sure we also
   * use the 64-bit register names for the comparison. Using LONGINT_TYPE here
   * used to work when LongInt was 8 bytes, but now emits 32-bit names (rXXd)
   * which are invalid with a qword cmp suffix. */
  const char *left_name = use_qword
                              ? left_reg->bit_64
                              : register_name_for_expr(left_reg, left_expr);
  const char *right_name = use_qword
                               ? right_reg->bit_64
                               : register_name_for_expr(right_reg, right_expr);

  /* Sign/zero-extend 32-bit operands for 64-bit comparison.
   * When one side is qword and the other was computed as 32-bit,
   * the upper 32 bits may not match the intended value (e.g. negl
   * produces 32-bit -1 = 0x00000000FFFFFFFF instead of 64-bit -1). */
  if (use_qword) {
    if (left_reg != NULL && !expression_uses_qword(left_expr)) {
      int left_tag =
          (left_expr != NULL) ? expr_get_type_tag(left_expr) : UNKNOWN_TYPE;
      if (codegen_type_is_signed(left_tag))
        inst_list = codegen_sign_extend32_to64(inst_list, left_reg->bit_32,
                                               left_reg->bit_64);
      else
        inst_list = codegen_zero_extend32_to64(inst_list, left_reg->bit_32,
                                               left_reg->bit_32);
    }
    if (right_reg != NULL && !expression_uses_qword(right_expr)) {
      int right_tag =
          (right_expr != NULL) ? expr_get_type_tag(right_expr) : UNKNOWN_TYPE;
      if (codegen_type_is_signed(right_tag))
        inst_list = codegen_sign_extend32_to64(inst_list, right_reg->bit_32,
                                               right_reg->bit_64);
      else
        inst_list = codegen_zero_extend32_to64(inst_list, right_reg->bit_32,
                                               right_reg->bit_32);
    }
  }

  snprintf(buffer, sizeof(buffer), "\tcmp%c\t%s, %s\n", use_qword ? 'q' : 'l',
           right_name, left_name);
  inst_list = add_inst(inst_list, buffer);
  free_reg(get_reg_stack(), right_reg);
  free_reg(get_reg_stack(), left_reg);

  /* For unsigned integer types, convert relop to unsigned variant so
   * gencode_jmp emits jb/jbe/ja/jae instead of jl/jle/jg/jge. */
  if (relop_type != NULL) {
    int left_tag =
        (left_expr != NULL) ? expr_get_type_tag(left_expr) : UNKNOWN_TYPE;
    int right_tag =
        (right_expr != NULL) ? expr_get_type_tag(right_expr) : UNKNOWN_TYPE;
    int either_unsigned =
        (!codegen_type_is_signed(left_tag) && left_tag != UNKNOWN_TYPE &&
         left_tag != BOOL && left_tag != CHAR_TYPE && left_tag != ENUM_TYPE &&
         left_tag != REAL_TYPE && left_tag != STRING_TYPE &&
         left_tag != SHORTSTRING_TYPE && left_tag != POINTER_TYPE) ||
        (!codegen_type_is_signed(right_tag) && right_tag != UNKNOWN_TYPE &&
         right_tag != BOOL && right_tag != CHAR_TYPE &&
         right_tag != ENUM_TYPE && right_tag != REAL_TYPE &&
         right_tag != STRING_TYPE && right_tag != SHORTSTRING_TYPE &&
         right_tag != POINTER_TYPE);
    if (either_unsigned) {
      switch (*relop_type) {
      case LT:
        *relop_type = LT_U;
        break;
      case LE:
        *relop_type = LE_U;
        break;
      case GT:
        *relop_type = GT_U;
        break;
      case GE:
        *relop_type = GE_U;
        break;
      default:
        break; /* EQ/NE don't care about signedness */
      }
    }
  }

  CODEGEN_DEBUG("DEBUG: Simple relop generated\n");
#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
  return inst_list;
}

/* Code generation for relop expressions that need a value result (not just
 * flags). This is used by builtin write functions that need the boolean value
 * in a register. Returns the result in out_reg as 0 or 1.
 */
ListNode_t *codegen_relop_to_value(struct Expression *expr,
                                   ListNode_t *inst_list, CodeGenContext *ctx,
                                   Register_t **out_reg) {
  if (expr == NULL || ctx == NULL || out_reg == NULL) {
    if (out_reg != NULL)
      *out_reg = NULL;
    return inst_list;
  }

  if (expr->type != EXPR_RELOP) {
    /* Not a relop - fall back to normal expression evaluation */
    return codegen_expr_tree_value(expr, inst_list, ctx, out_reg);
  }

  int relop_kind = expr->expr_data.relop_data.type;
  struct Expression *right_expr = expr->expr_data.relop_data.right;

  /* Check if this is an IN operation on a char set - requires special handling
   */
  int is_char_set_in = 0;
  if (relop_kind == IN && right_expr != NULL) {
    is_char_set_in = expr_is_char_set_ctx(right_expr, ctx);
    /* Also check for char set constants via variable reference */
    if (!is_char_set_in && right_expr->type == EXPR_VAR_ID &&
        ctx->symtab != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, ctx->symtab, right_expr->expr_data.id) != 0 &&
          node != NULL && node->hash_type == HASHTYPE_CONST &&
          node->const_set_value != NULL &&
          node->const_set_size > 4) /* 32-byte char set */
      {
        is_char_set_in = 1;
      }
    }
  }

  if (!is_char_set_in && relop_kind != IN) {
    /* For non-set operations, the expression tree approach works */
    return codegen_expr_tree_value(expr, inst_list, ctx, out_reg);
  }

  /* For set IN operations (both char sets and enum sets), we need to use
   * codegen_simple_relop which properly handles memory-based btl for sets
   * that may exceed 32 elements, then convert flags to a value. */

  int relop_type = 0;
  inst_list = codegen_simple_relop(expr, inst_list, ctx, &relop_type);

  /* codegen_simple_relop sets CPU flags. We need to convert to a 0/1 value.
   * The relop_type tells us what condition was tested:
   * - NE means "not equal" - we want setnz to get 1 when flag is set
   */
  Register_t *result_reg = codegen_try_get_reg(&inst_list, ctx, "relop result");
  if (result_reg == NULL) {
    *out_reg = NULL;
    return inst_list;
  }

  char buffer[64];
  const char *set_instr = "setnz"; /* Default for set membership */

  /* Convert flags to value based on relop type */
  for (int i = 0; i < RELOP_TABLE_SIZE; i++) {
    if (relop_table[i].relop == relop_type) {
      set_instr = relop_table[i].setcc;
      break;
    }
  }

  const char *reg8 = codegen_register_name8(result_reg);
  if (reg8 != NULL) {
    snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", set_instr, reg8);
    inst_list = add_inst(inst_list, buffer);
    {
      char buffer_tmpl[128];
      snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovzbl\t%s, %%0\n", reg8);
      Register_t *d[] = {result_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, buffer_tmpl);
    }
  }

  *out_reg = result_reg;
  return inst_list;
}
