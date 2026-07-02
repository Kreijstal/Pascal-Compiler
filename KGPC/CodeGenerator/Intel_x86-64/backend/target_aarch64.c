/*
 * target_aarch64.c — AArch64 (AAPCS64) implementation of the Target vtable.
 *
 * M4: proves the target.h interface is genuinely ISA-neutral.  The SAME test
 * harness (build_binop / build_const) drives this backend, and the SAME shared
 * register allocator colors into this target's registers — enabled by the
 * register-file generalization (stackmng_set_register_pool + the vtable's
 * regpool()).  No x86 assumption remains in the allocation path.
 *
 * Scope: the minimal op set, emitted with 64-bit x-registers (correct for the
 * integer harness; the 32/64 width-selection in ir_emit_function's suffix
 * heuristic is x86-specific and is the documented next generalization — see
 * the plan's M4 note).  No local AArch64 toolchain/qemu here, so the harness
 * exercises this target at the golden-asm level (structure of the emitted
 * assembly), which is where interface neutrality is demonstrated.
 */
#include "target.h"

#include "../stackmng/stackmng.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

static int *vregp(BeEmitter *em) {
  return em->next_vreg_id;
}

static void aa_lit(const BeOperand *op, char *buf, size_t n) {
  switch (op->kind) {
  case OPK_IMM:
    snprintf(buf, n, "#%lld", op->u.imm);
    break;
  case OPK_PHYS:
    snprintf(buf, n, "%s", op->u.phys);
    break;
  default:
    snprintf(buf, n, "?");
    break;
  }
}

static void aa_emit(BeEmitter *em, BeOp op, BeWidth w, const BeOperand *dst,
                    const BeOperand *a, const BeOperand *b) {
  /* AArch64 encodes width in the register name (w0 vs x0), not the mnemonic,
   * so widths are passed explicitly to be_add_inst_du_w. */
  const int w32 = (w == BE_W32);
  char tmpl[160];
  char lit[48];
  Register_t *defs[2];
  Register_t *uses[4];
  int use32[6];

  switch (op) {
  case BE_MOV:
    if (dst->kind == OPK_VREG && a->kind == OPK_VREG) {
      defs[0] = dst->u.vreg;
      uses[0] = a->u.vreg;
      use32[0] = w32;
      use32[1] = w32;
      em->list = be_add_inst_du_w(em->list, vregp(em), defs, 1, uses, 1,
                                  "\tmov\t%0, %1\n", use32);
    } else if (dst->kind == OPK_VREG) {
      aa_lit(a, lit, sizeof(lit));
      snprintf(tmpl, sizeof(tmpl), "\tmov\t%%0, %s\n", lit);
      defs[0] = dst->u.vreg;
      use32[0] = w32;
      em->list =
          be_add_inst_du_w(em->list, vregp(em), defs, 1, NULL, 0, tmpl, use32);
    } else if (a->kind == OPK_VREG) {
      aa_lit(dst, lit, sizeof(lit));
      snprintf(tmpl, sizeof(tmpl), "\tmov\t%s, %%0\n", lit);
      uses[0] = a->u.vreg;
      use32[0] = w32;
      em->list =
          be_add_inst_du_w(em->list, vregp(em), NULL, 0, uses, 1, tmpl, use32);
    } else {
      char litd[48];
      aa_lit(a, lit, sizeof(lit));
      aa_lit(dst, litd, sizeof(litd));
      snprintf(tmpl, sizeof(tmpl), "\tmov\t%s, %s\n", litd, lit);
      em->list = add_inst(em->list, tmpl);
    }
    break;

  case BE_ADD:
  case BE_SUB:
  case BE_MUL:
  case BE_AND:
  case BE_OR:
  case BE_XOR: {
    /* AArch64 is 3-operand: <mn> dst, a, b */
    const char *mn = (op == BE_ADD)   ? "add"
                     : (op == BE_SUB) ? "sub"
                     : (op == BE_MUL) ? "mul"
                     : (op == BE_AND) ? "and"
                     : (op == BE_OR)  ? "orr"
                                      : "eor";
    assert(dst->kind == OPK_VREG && a->kind == OPK_VREG);
    if (b->kind == OPK_VREG) {
      snprintf(tmpl, sizeof(tmpl), "\t%s\t%%0, %%1, %%2\n", mn);
      defs[0] = dst->u.vreg;
      uses[0] = a->u.vreg;
      uses[1] = b->u.vreg;
      use32[0] = use32[1] = use32[2] = w32;
      em->list = be_add_inst_du_w(em->list, vregp(em), defs, 1, uses, 2, tmpl,
                                  use32);
    } else {
      /* add/sub accept an immediate; mul does not (harness uses reg mul). */
      assert(op != BE_MUL && "aarch64 mul immediate unsupported");
      aa_lit(b, lit, sizeof(lit));
      snprintf(tmpl, sizeof(tmpl), "\t%s\t%%0, %%1, %s\n", mn, lit);
      defs[0] = dst->u.vreg;
      uses[0] = a->u.vreg;
      use32[0] = use32[1] = w32;
      em->list = be_add_inst_du_w(em->list, vregp(em), defs, 1, uses, 1, tmpl,
                                  use32);
    }
    break;
  }

  case BE_NEG:
    /* AArch64 neg is 2-operand: neg dst, src */
    assert(dst->kind == OPK_VREG && a->kind == OPK_VREG);
    defs[0] = dst->u.vreg;
    uses[0] = a->u.vreg;
    use32[0] = use32[1] = w32;
    em->list = be_add_inst_du_w(em->list, vregp(em), defs, 1, uses, 1,
                                "\tneg\t%0, %1\n", use32);
    break;

  case BE_SHL:
  case BE_SHR:
  case BE_SAR: {
    const char *mn = (op == BE_SHL) ? "lsl" : (op == BE_SHR) ? "lsr" : "asr";
    assert(dst->kind == OPK_VREG && a->kind == OPK_VREG && b->kind == OPK_IMM);
    snprintf(tmpl, sizeof(tmpl), "\t%s\t%%0, %%1, #%lld\n", mn, b->u.imm);
    defs[0] = dst->u.vreg;
    uses[0] = a->u.vreg;
    use32[0] = use32[1] = w32;
    em->list =
        be_add_inst_du_w(em->list, vregp(em), defs, 1, uses, 1, tmpl, use32);
    break;
  }

  case BE_DIV:
    /* sdiv dst, a, b */
    assert(dst->kind == OPK_VREG && a->kind == OPK_VREG && b->kind == OPK_VREG);
    defs[0] = dst->u.vreg;
    uses[0] = a->u.vreg;
    uses[1] = b->u.vreg;
    use32[0] = use32[1] = use32[2] = w32;
    em->list = be_add_inst_du_w(em->list, vregp(em), defs, 1, uses, 2,
                                "\tsdiv\t%0, %1, %2\n", use32);
    break;

  case BE_MOD: {
    /* No direct remainder: q = a / b; dst = a - q*b  via  sdiv + msub. */
    assert(dst->kind == OPK_VREG && a->kind == OPK_VREG && b->kind == OPK_VREG);
    Register_t *scr = get_free_reg(get_reg_stack(), &em->list);
    assert(scr != NULL && "no scratch register for aarch64 mod");
    defs[0] = scr;
    uses[0] = a->u.vreg;
    uses[1] = b->u.vreg;
    use32[0] = use32[1] = use32[2] = w32;
    em->list = be_add_inst_du_w(em->list, vregp(em), defs, 1, uses, 2,
                                "\tsdiv\t%0, %1, %2\n", use32);
    /* msub dst, scr, b, a  →  dst = a - scr*b */
    defs[0] = dst->u.vreg;
    uses[0] = scr;
    uses[1] = b->u.vreg;
    uses[2] = a->u.vreg;
    use32[0] = use32[1] = use32[2] = use32[3] = w32;
    em->list = be_add_inst_du_w(em->list, vregp(em), defs, 1, uses, 3,
                                "\tmsub\t%0, %1, %2, %3\n", use32);
    free_reg(get_reg_stack(), scr);
    break;
  }

  case BE_CMP:
    if (a->kind == OPK_VREG && b->kind == OPK_VREG) {
      uses[0] = a->u.vreg;
      uses[1] = b->u.vreg;
      use32[0] = use32[1] = w32;
      em->list = be_add_inst_du_w(em->list, vregp(em), NULL, 0, uses, 2,
                                  "\tcmp\t%0, %1\n", use32);
    } else if (a->kind == OPK_VREG) {
      aa_lit(b, lit, sizeof(lit));
      snprintf(tmpl, sizeof(tmpl), "\tcmp\t%%0, %s\n", lit);
      uses[0] = a->u.vreg;
      use32[0] = w32;
      em->list =
          be_add_inst_du_w(em->list, vregp(em), NULL, 0, uses, 1, tmpl, use32);
    } else {
      assert(0 && "aarch64 cmp needs a register first operand");
    }
    break;

  case BE_LOAD:
    assert(dst->kind == OPK_VREG && a->kind == OPK_MEM_BD);
    snprintf(tmpl, sizeof(tmpl), "\tldr\t%%0, [%%1, #%d]\n", a->u.mem_bd.disp);
    defs[0] = dst->u.vreg;
    uses[0] = a->u.mem_bd.base;
    use32[0] = w32; /* loaded value width */
    use32[1] = 0;   /* address register is always 64-bit */
    em->list =
        be_add_inst_du_w(em->list, vregp(em), defs, 1, uses, 1, tmpl, use32);
    break;

  case BE_STORE:
    assert(dst->kind == OPK_MEM_BD && a->kind == OPK_VREG);
    snprintf(tmpl, sizeof(tmpl), "\tstr\t%%0, [%%1, #%d]\n", dst->u.mem_bd.disp);
    uses[0] = a->u.vreg;
    uses[1] = dst->u.mem_bd.base;
    use32[0] = w32; /* stored value width */
    use32[1] = 0;   /* address register is always 64-bit */
    em->list =
        be_add_inst_du_w(em->list, vregp(em), NULL, 0, uses, 2, tmpl, use32);
    break;

  case BE_LEA:
  default:
    assert(0 && "unsupported aarch64 op");
    break;
  }
}

static const char *aa_cc_mnemonic(BeCond cc) {
  switch (cc) {
  case BE_EQ:
    return "b.eq";
  case BE_NE:
    return "b.ne";
  case BE_LT:
    return "b.lt";
  case BE_LE:
    return "b.le";
  case BE_GT:
    return "b.gt";
  case BE_GE:
    return "b.ge";
  case BE_ULT:
    return "b.lo";
  case BE_ULE:
    return "b.ls";
  case BE_UGT:
    return "b.hi";
  case BE_UGE:
    return "b.hs";
  case BE_ALWAYS:
  default:
    return "b";
  }
}

static const char *aa_cset_cond(BeCond cc) {
  switch (cc) {
  case BE_EQ:
    return "eq";
  case BE_NE:
    return "ne";
  case BE_LT:
    return "lt";
  case BE_LE:
    return "le";
  case BE_GT:
    return "gt";
  case BE_GE:
    return "ge";
  case BE_ULT:
    return "lo";
  case BE_ULE:
    return "ls";
  case BE_UGT:
    return "hi";
  case BE_UGE:
    return "hs";
  default:
    return "eq";
  }
}

static void aa_emit_setcc(BeEmitter *em, BeCond cc, const BeOperand *dst) {
  /* cset dst, <cond> — materialize the boolean directly (no %al dance). */
  assert(dst->kind == OPK_VREG);
  char tmpl[64];
  snprintf(tmpl, sizeof(tmpl), "\tcset\t%%0, %s\n", aa_cset_cond(cc));
  Register_t *defs[1] = {dst->u.vreg};
  int use32[1] = {(dst->width == BE_W32)};
  em->list = be_add_inst_du_w(em->list, vregp(em), defs, 1, NULL, 0, tmpl,
                              use32);
}

static void aa_emit_branch(BeEmitter *em, BeCond cc, const char *label) {
  char buf[96];
  snprintf(buf, sizeof(buf), "\t%s\t%s\n", aa_cc_mnemonic(cc), label);
  em->list = add_inst(em->list, buf);
}

static void aa_emit_call(BeEmitter *em, const char *sym, Register_t *indirect) {
  char buf[96];
  if (sym != NULL) {
    snprintf(buf, sizeof(buf), "\tbl\t%s\n", sym);
    em->list = add_inst(em->list, buf);
  } else {
    Register_t *uses[1] = {indirect};
    em->list =
        be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, "\tblr\t%0\n");
  }
}

static void aa_emit_ret(BeEmitter *em) {
  em->list = add_inst(em->list, "\tret\n");
}

static void aa_emit_label(BeEmitter *em, const char *label) {
  char buf[128];
  snprintf(buf, sizeof(buf), "%s:\n", label);
  em->list = add_inst(em->list, buf);
}

static void aa_emit_prologue(BeEmitter *em, const BeFrame *f) {
  char buf[256];
  /* Save fp/lr and the 5 callee-saved regs (x19-x23) in a 64-byte frame. */
  snprintf(buf, sizeof(buf),
           "%s:\n"
           "\tstp\tx29, x30, [sp, #-64]!\n"
           "\tmov\tx29, sp\n"
           "\tstp\tx19, x20, [sp, #16]\n"
           "\tstp\tx21, x22, [sp, #32]\n"
           "\tstr\tx23, [sp, #48]\n",
           f->name);
  em->list = add_inst(em->list, buf);
  if (f->frame_size > 0) {
    snprintf(buf, sizeof(buf), "\tsub\tsp, sp, #%d\n", f->frame_size);
    em->list = add_inst(em->list, buf);
  }
}

static void aa_emit_epilogue(BeEmitter *em, const BeFrame *f) {
  char buf[256];
  if (f->frame_size > 0) {
    snprintf(buf, sizeof(buf), "\tadd\tsp, sp, #%d\n", f->frame_size);
    em->list = add_inst(em->list, buf);
  }
  em->list = add_inst(em->list,
                      "\tldr\tx23, [sp, #48]\n"
                      "\tldp\tx21, x22, [sp, #32]\n"
                      "\tldp\tx19, x20, [sp, #16]\n"
                      "\tldp\tx29, x30, [sp], #64\n"
                      "\tret\n");
}

/* AAPCS64 arg/return registers (w0-w7 for 32-bit, x0-x7 for 64-bit). */
static const char *aa_arg_reg(int idx, BeWidth w) {
  static const char *x[] = {"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"};
  static const char *wr[] = {"w0", "w1", "w2", "w3", "w4", "w5", "w6", "w7"};
  if (idx < 0 || idx >= 8)
    return NULL;
  return (w == BE_W32) ? wr[idx] : x[idx];
}

static int aa_num_int_arg_regs(void) { return 8; }

static const char *aa_return_reg(BeWidth w) {
  return (w == BE_W32) ? "w0" : "x0";
}

/* Allocatable pool: AArch64 callee-saved x19-x23.  The reg_id slots are reused
 * as opaque identifiers (the allocator only needs distinct ids + names). */
static const BackendRegSpec kAArch64Pool[] = {
    {REG_RBX, "x19", "w19"}, {REG_R12, "x20", "w20"}, {REG_R13, "x21", "w21"},
    {REG_R14, "x22", "w22"}, {REG_R15, "x23", "w23"},
};

static const BackendRegSpec *aa_regpool(int *n) {
  *n = (int)(sizeof(kAArch64Pool) / sizeof(kAArch64Pool[0]));
  return kAArch64Pool;
}

static const Target kAArch64 = {
    .name = "aarch64",
    .ptr_width = 8,
    .emit = aa_emit,
    .emit_setcc = aa_emit_setcc,
    .emit_branch = aa_emit_branch,
    .emit_call = aa_emit_call,
    .emit_ret = aa_emit_ret,
    .emit_label = aa_emit_label,
    .emit_prologue = aa_emit_prologue,
    .emit_epilogue = aa_emit_epilogue,
    .arg_reg = aa_arg_reg,
    .num_int_arg_regs = aa_num_int_arg_regs,
    .return_reg = aa_return_reg,
    .regpool = aa_regpool,
};

const Target *target_aarch64(void) { return &kAArch64; }
