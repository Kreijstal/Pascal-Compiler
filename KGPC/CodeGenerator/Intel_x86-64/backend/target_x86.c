/*
 * target_x86.c — x86-64 (System V) implementation of the Target vtable.
 *
 * M2: minimal op set (MOV/LOAD/STORE/ADD/SUB/MUL/CMP/LEA + branch/call/ret +
 * prologue/epilogue).  Each op formats an AT&T template and appends it via the
 * proven be_add_inst_du / add_inst emission core — this is a thin wrapper over
 * emission that already works, not a fresh instruction selector.
 *
 * The register pool (stackmng) hands out the 5 callee-saved registers
 * (%rbx,%r12-%r15), so the prologue saves all five and the epilogue restores
 * them: any function the harness builds is ABI-correct when called from C.
 * (A later refinement can save only the ones the allocator actually used.)
 */
#include "target.h"

#include "../stackmng/stackmng.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

/* 32- vs 64-bit mnemonic suffix.  The width heuristic in ir_emit_function
 * keys register-name width off this same suffix, so it must be correct. */
static char x86_suffix(BeWidth w) {
  switch (w) {
  case BE_W8:
    return 'b';
  case BE_W16:
    return 'w';
  case BE_W32:
    return 'l';
  case BE_W64:
  default:
    return 'q';
  }
}

static int *vregp(BeEmitter *em) {
  return em->next_vreg_id;
}

/* Render a non-register operand (imm / phys / rip-sym) to a literal token.
 * Register operands are handled by the callers via %N placeholders. */
static void x86_lit(const BeOperand *op, char *buf, size_t n) {
  switch (op->kind) {
  case OPK_IMM:
    snprintf(buf, n, "$%lld", op->u.imm);
    break;
  case OPK_PHYS:
    snprintf(buf, n, "%s", op->u.phys);
    break;
  case OPK_RIP_SYM:
    snprintf(buf, n, "%s(%%rip)", op->u.sym);
    break;
  default:
    snprintf(buf, n, "?");
    break;
  }
}

static void x86_emit(BeEmitter *em, BeOp op, BeWidth w, const BeOperand *dst,
                     const BeOperand *a, const BeOperand *b) {
  char c = x86_suffix(w);
  char tmpl[160];
  char lit[48];
  Register_t *defs[2];
  Register_t *uses[4];

  switch (op) {
  case BE_MOV: {
    /* dst := a */
    if (dst->kind == OPK_VREG && a->kind == OPK_VREG) {
      /* movX %1, %0 : def dst, use a */
      snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%%1, %%0\n", c);
      defs[0] = dst->u.vreg;
      uses[0] = a->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1, tmpl);
    } else if (dst->kind == OPK_VREG) {
      /* movX <lit>, %0 : def dst */
      x86_lit(a, lit, sizeof(lit));
      snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%s, %%0\n", c, lit);
      defs[0] = dst->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, NULL, 0, tmpl);
    } else if (a->kind == OPK_VREG) {
      /* movX %0, <phys dst> : use a */
      x86_lit(dst, lit, sizeof(lit));
      snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%%0, %s\n", c, lit);
      uses[0] = a->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, tmpl);
    } else {
      /* both literal/phys — no placeholders */
      char litd[48];
      x86_lit(a, lit, sizeof(lit));
      x86_lit(dst, litd, sizeof(litd));
      snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%s, %s\n", c, lit, litd);
      em->list = add_inst(em->list, tmpl);
    }
    break;
  }

  case BE_LOAD: {
    /* dst(vreg) := [a: MEM_BD(base vreg, disp)]  →  movX disp(%1), %0 */
    assert(dst->kind == OPK_VREG && a->kind == OPK_MEM_BD);
    snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%d(%%1), %%0\n", c, a->u.mem_bd.disp);
    defs[0] = dst->u.vreg;
    uses[0] = a->u.mem_bd.base;
    em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1, tmpl);
    break;
  }

  case BE_STORE: {
    /* [dst: MEM_BD(base vreg, disp)] := a(vreg)  →  movX %0, disp(%1) */
    assert(dst->kind == OPK_MEM_BD && a->kind == OPK_VREG);
    snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%%0, %d(%%1)\n", c,
             dst->u.mem_bd.disp);
    uses[0] = a->u.vreg;
    uses[1] = dst->u.mem_bd.base;
    em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 2, tmpl);
    break;
  }

  case BE_ADD:
  case BE_SUB:
  case BE_MUL:
  case BE_AND:
  case BE_OR:
  case BE_XOR: {
    /* dst := a <op> b, dst is a register.  Two-operand x86: fold a into dst
     * first if needed, then apply b. */
    const char *mn = (op == BE_ADD)   ? "add"
                     : (op == BE_SUB) ? "sub"
                     : (op == BE_MUL) ? "imul"
                     : (op == BE_AND) ? "and"
                     : (op == BE_OR)  ? "or"
                                      : "xor";
    assert(dst->kind == OPK_VREG);
    if (!(a->kind == OPK_VREG && a->u.vreg == dst->u.vreg)) {
      /* dst := a first */
      x86_emit(em, BE_MOV, w, dst, a, NULL);
    }
    /* dst := dst <op> b : dst is def+use */
    if (b->kind == OPK_VREG) {
      /* opX %2, %0   (def dst[%0], use dst[%1], use b[%2]) */
      snprintf(tmpl, sizeof(tmpl), "\t%s%c\t%%2, %%0\n", mn, c);
      defs[0] = dst->u.vreg;
      uses[0] = dst->u.vreg;
      uses[1] = b->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 2, tmpl);
    } else {
      /* opX <lit>, %0   (def dst[%0], use dst[%1]) */
      x86_lit(b, lit, sizeof(lit));
      snprintf(tmpl, sizeof(tmpl), "\t%s%c\t%s, %%0\n", mn, c, lit);
      defs[0] = dst->u.vreg;
      uses[0] = dst->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1, tmpl);
    }
    break;
  }

  case BE_NEG: {
    /* dst := -dst (RMW) */
    assert(dst->kind == OPK_VREG);
    if (!(a->kind == OPK_VREG && a->u.vreg == dst->u.vreg))
      x86_emit(em, BE_MOV, w, dst, a, NULL);
    snprintf(tmpl, sizeof(tmpl), "\tneg%c\t%%0\n", c);
    defs[0] = dst->u.vreg;
    uses[0] = dst->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1, tmpl);
    break;
  }

  case BE_SHL:
  case BE_SHR:
  case BE_SAR: {
    /* dst := dst <shift> imm.  x86: shl/shr/sar $imm, dst */
    const char *mn = (op == BE_SHL) ? "shl" : (op == BE_SHR) ? "shr" : "sar";
    assert(dst->kind == OPK_VREG && b->kind == OPK_IMM);
    if (!(a->kind == OPK_VREG && a->u.vreg == dst->u.vreg))
      x86_emit(em, BE_MOV, w, dst, a, NULL);
    snprintf(tmpl, sizeof(tmpl), "\t%s%c\t$%lld, %%0\n", mn, c, b->u.imm);
    defs[0] = dst->u.vreg;
    uses[0] = dst->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1, tmpl);
    break;
  }

  case BE_CMP: {
    /* compare a to b: cmpX <b>, <a>  (flags reflect a ? b) */
    if (a->kind == OPK_VREG && b->kind == OPK_VREG) {
      snprintf(tmpl, sizeof(tmpl), "\tcmp%c\t%%1, %%0\n", c);
      uses[0] = a->u.vreg;
      uses[1] = b->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 2, tmpl);
    } else if (a->kind == OPK_VREG) {
      x86_lit(b, lit, sizeof(lit));
      snprintf(tmpl, sizeof(tmpl), "\tcmp%c\t%s, %%0\n", c, lit);
      uses[0] = a->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, tmpl);
    } else {
      char lita[48];
      x86_lit(b, lit, sizeof(lit));
      x86_lit(a, lita, sizeof(lita));
      snprintf(tmpl, sizeof(tmpl), "\tcmp%c\t%s, %s\n", c, lit, lita);
      em->list = add_inst(em->list, tmpl);
    }
    break;
  }

  case BE_LEA: {
    /* dst(vreg) := &a  (RIP symbol or MEM_BD) */
    assert(dst->kind == OPK_VREG);
    if (a->kind == OPK_RIP_SYM) {
      snprintf(tmpl, sizeof(tmpl), "\tleaq\t%s(%%rip), %%0\n", a->u.sym);
      defs[0] = dst->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, NULL, 0, tmpl);
    } else if (a->kind == OPK_MEM_BD) {
      snprintf(tmpl, sizeof(tmpl), "\tleaq\t%d(%%1), %%0\n", a->u.mem_bd.disp);
      defs[0] = dst->u.vreg;
      uses[0] = a->u.mem_bd.base;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1, tmpl);
    } else {
      assert(0 && "unsupported LEA operand");
    }
    break;
  }

  default:
    assert(0 && "unsupported op");
    break;
  }
}

static const char *x86_cc_mnemonic(BeCond cc) {
  switch (cc) {
  case BE_EQ:
    return "je";
  case BE_NE:
    return "jne";
  case BE_LT:
    return "jl";
  case BE_LE:
    return "jle";
  case BE_GT:
    return "jg";
  case BE_GE:
    return "jge";
  case BE_ULT:
    return "jb";
  case BE_ULE:
    return "jbe";
  case BE_UGT:
    return "ja";
  case BE_UGE:
    return "jae";
  case BE_ALWAYS:
  default:
    return "jmp";
  }
}

static void x86_emit_branch(BeEmitter *em, BeCond cc, const char *label) {
  char buf[96];
  snprintf(buf, sizeof(buf), "\t%s\t%s\n", x86_cc_mnemonic(cc), label);
  em->list = add_inst(em->list, buf);
}

static void x86_emit_call(BeEmitter *em, const char *sym, Register_t *indirect) {
  char buf[96];
  if (sym != NULL) {
    snprintf(buf, sizeof(buf), "\tcall\t%s\n", sym);
    em->list = add_inst(em->list, buf);
  } else {
    Register_t *uses[1] = {indirect};
    em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1,
                              "\tcall\t*%0\n");
  }
}

static void x86_emit_ret(BeEmitter *em) {
  em->list = add_inst(em->list, "\tret\n");
}

static void x86_emit_label(BeEmitter *em, const char *label) {
  char buf[128];
  snprintf(buf, sizeof(buf), "%s:\n", label);
  em->list = add_inst(em->list, buf);
}

/* Callee-saved registers in the stackmng allocation pool. */
#define X86_CALLEE_SAVED_BYTES 40 /* 5 regs * 8 */

static void x86_emit_prologue(BeEmitter *em, const BeFrame *f) {
  char buf[192];
  snprintf(buf, sizeof(buf),
           "%s:\n"
           "\tpushq\t%%rbp\n"
           "\tmovq\t%%rsp, %%rbp\n"
           "\tpushq\t%%rbx\n"
           "\tpushq\t%%r12\n"
           "\tpushq\t%%r13\n"
           "\tpushq\t%%r14\n"
           "\tpushq\t%%r15\n",
           f->name);
  em->list = add_inst(em->list, buf);
  if (f->frame_size > 0) {
    snprintf(buf, sizeof(buf), "\tsubq\t$%d, %%rsp\n", f->frame_size);
    em->list = add_inst(em->list, buf);
  }
}

static void x86_emit_epilogue(BeEmitter *em, const BeFrame *f) {
  char buf[192];
  (void)f;
  /* Reposition %rsp to just below the saved %rbp (discards any locals frame),
   * restore callee-saved regs, restore %rbp, return. */
  snprintf(buf, sizeof(buf),
           "\tleaq\t-%d(%%rbp), %%rsp\n"
           "\tpopq\t%%r15\n"
           "\tpopq\t%%r14\n"
           "\tpopq\t%%r13\n"
           "\tpopq\t%%r12\n"
           "\tpopq\t%%rbx\n"
           "\tpopq\t%%rbp\n"
           "\tret\n",
           X86_CALLEE_SAVED_BYTES);
  em->list = add_inst(em->list, buf);
}

/* System V argument / return registers. */
static const char *x86_arg_reg(int idx, BeWidth w) {
  static const char *r64[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
  static const char *r32[] = {"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"};
  if (idx < 0 || idx >= 6)
    return NULL;
  return (w == BE_W32) ? r32[idx] : r64[idx];
}

static int x86_num_int_arg_regs(void) { return 6; }

static const char *x86_return_reg(BeWidth w) {
  return (w == BE_W32) ? "%eax" : "%rax";
}

/* Allocatable pool: x86-64 callee-saved registers (matches stackmng's default;
 * provided through the vtable so pool selection is uniform across targets). */
static const BackendRegSpec kX86Pool[] = {
    {REG_RBX, "%rbx", "%ebx"},  {REG_R12, "%r12", "%r12d"},
    {REG_R13, "%r13", "%r13d"}, {REG_R14, "%r14", "%r14d"},
    {REG_R15, "%r15", "%r15d"},
};

static const BackendRegSpec *x86_regpool(int *n) {
  *n = (int)(sizeof(kX86Pool) / sizeof(kX86Pool[0]));
  return kX86Pool;
}

static const Target kX86SysV = {
    .name = "x86_64-sysv",
    .ptr_width = 8,
    .emit = x86_emit,
    .emit_branch = x86_emit_branch,
    .emit_call = x86_emit_call,
    .emit_ret = x86_emit_ret,
    .emit_label = x86_emit_label,
    .emit_prologue = x86_emit_prologue,
    .emit_epilogue = x86_emit_epilogue,
    .arg_reg = x86_arg_reg,
    .num_int_arg_regs = x86_num_int_arg_regs,
    .return_reg = x86_return_reg,
    .regpool = x86_regpool,
};

const Target *target_x86_sysv(void) { return &kX86SysV; }

const Target *kgpc_backend_target(void) { return &kX86SysV; }
