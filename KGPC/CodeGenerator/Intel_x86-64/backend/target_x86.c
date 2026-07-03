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
#include <stdint.h>
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

/* Floating-point widths use the SSE scalar instructions (…sd for double, …ss
 * for single) and the xmm register file, not the GP mnemonic-suffix path. */
static int x86_is_float(BeWidth w) { return w == BE_WF32 || w == BE_WF64; }

/* SSE scalar suffix: "sd" for double, "ss" for single. */
static const char *x86_fsuffix(BeWidth w) { return (w == BE_WF32) ? "ss" : "sd"; }

/* Render a frame-relative memory operand to its AT&T literal: "disp(base)", or
 * "(base)" when disp == 0 (matching the compiler's zero-displacement spelling).
 * The base is the fixed frame/stack pointer, rendered here — it is never a pool
 * placeholder. */
static void x86_frame(const BeOperand *op, char *buf, size_t n) {
  const char *base = (op->u.mem_frame.base == BE_BASE_SP) ? "%rsp" : "%rbp";
  long long d = op->u.mem_frame.disp;
  /* x86-64 encodes displacements as signed 32-bit.  Real frame offsets always
   * fit; guard so a mis-sized offset asserts instead of silently truncating. */
  assert(d >= INT32_MIN && d <= INT32_MAX &&
         "frame displacement out of x86 signed-32 range");
  if (d == 0)
    snprintf(buf, n, "(%s)", base);
  else
    snprintf(buf, n, "%lld(%s)", d, base);
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

/* SSE scalar float ops (MOV/ADD/SUB/MUL/DIV/CMP) on the xmm register file.
 * xmm registers have a single name regardless of width, so the vreg's four
 * name buffers are all identical — whichever width the substitution heuristic
 * picks yields the same "%xmmN".  Conversions (int↔float) are handled by
 * x86_emit, not here, because they cross register classes. */
static void x86_emit_float(BeEmitter *em, BeOp op, BeWidth w,
                           const BeOperand *dst, const BeOperand *a,
                           const BeOperand *b) {
  const char *sfx = x86_fsuffix(w);
  char tmpl[160];
  char lit[48];
  Register_t *defs[2];
  Register_t *uses[4];

  switch (op) {
  case BE_MOV: {
    /* movsd/movss — dst := a, where operands are xmm vregs or xmm phys regs. */
    if (dst->kind == OPK_VREG && a->kind == OPK_VREG) {
      snprintf(tmpl, sizeof(tmpl), "\tmov%s\t%%1, %%0\n", sfx);
      defs[0] = dst->u.vreg;
      uses[0] = a->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1, tmpl);
    } else if (dst->kind == OPK_VREG) { /* dst := <phys xmm> */
      x86_lit(a, lit, sizeof(lit));
      snprintf(tmpl, sizeof(tmpl), "\tmov%s\t%s, %%0\n", sfx, lit);
      defs[0] = dst->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, NULL, 0, tmpl);
    } else if (a->kind == OPK_VREG) { /* <phys xmm> := a */
      x86_lit(dst, lit, sizeof(lit));
      snprintf(tmpl, sizeof(tmpl), "\tmov%s\t%%0, %s\n", sfx, lit);
      uses[0] = a->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, tmpl);
    } else {
      char litd[48];
      x86_lit(a, lit, sizeof(lit));
      x86_lit(dst, litd, sizeof(litd));
      snprintf(tmpl, sizeof(tmpl), "\tmov%s\t%s, %s\n", sfx, lit, litd);
      em->list = add_inst(em->list, tmpl);
    }
    break;
  }

  case BE_ADD:
  case BE_SUB:
  case BE_MUL:
  case BE_DIV: {
    /* addsd/subsd/mulsd/divsd — two-operand: dst := dst OP b.  Fold a into dst
     * first if a is not already dst. */
    const char *base = (op == BE_ADD)   ? "add"
                       : (op == BE_SUB) ? "sub"
                       : (op == BE_MUL) ? "mul"
                                        : "div";
    assert(dst->kind == OPK_VREG && b->kind == OPK_VREG);
    if (!(a->kind == OPK_VREG && a->u.vreg == dst->u.vreg))
      x86_emit_float(em, BE_MOV, w, dst, a, NULL);
    snprintf(tmpl, sizeof(tmpl), "\t%s%s\t%%2, %%0\n", base, sfx);
    defs[0] = dst->u.vreg;
    uses[0] = dst->u.vreg;
    uses[1] = b->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 2, tmpl);
    break;
  }

  case BE_CMP: {
    /* ucomisd/ucomiss — set flags from a ? b (unordered compare). */
    const char *mn = (w == BE_WF32) ? "ucomiss" : "ucomisd";
    assert(a->kind == OPK_VREG && b->kind == OPK_VREG);
    snprintf(tmpl, sizeof(tmpl), "\t%s\t%%1, %%0\n", mn);
    uses[0] = a->u.vreg;
    uses[1] = b->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 2, tmpl);
    break;
  }

  default:
    assert(0 && "unsupported float op");
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

  /* Float-width arithmetic/mov/cmp dispatch to the SSE path.  LOAD/STORE are
   * excluded: a float frame load/store is a plain movsd/movss to/from an xmm
   * register, handled by the frame branches of BE_LOAD/BE_STORE below (the SSE
   * arithmetic path does not model memory operands). */
  if (x86_is_float(w) && op != BE_CVT_I2F && op != BE_CVT_F2I &&
      op != BE_LOAD && op != BE_STORE) {
    x86_emit_float(em, op, w, dst, a, b);
    return;
  }

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
    if (a->kind == OPK_MEM_FRAME) {
      char fb[40];
      x86_frame(a, fb, sizeof(fb));
      if (dst->kind == OPK_PHYS) {
        /* <phys> := [frame]  →  movX <frame>, <phys>  (both operands literal:
         * no %N placeholder, no def/use — a fixed physical register is never
         * allocated, so this is a plain string emission).  A float width uses
         * the SSE scalar mnemonic (movsd/movss) into an xmm register. */
        if (x86_is_float(w))
          snprintf(tmpl, sizeof(tmpl), "\tmov%s\t%s, %s\n", x86_fsuffix(w), fb,
                   dst->u.phys);
        else
          snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%s, %s\n", c, fb, dst->u.phys);
        em->list = add_inst(em->list, tmpl);
        break;
      }
      /* dst(vreg) := [frame]  →  movX disp(%rbp), %0  (fixed base, no use) */
      assert(dst->kind == OPK_VREG);
      snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%s, %%0\n", c, fb);
      defs[0] = dst->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, NULL, 0, tmpl);
      break;
    }
    /* dst(vreg) := [a: MEM_BD(base vreg, disp)]  →  movX disp(%1), %0 */
    assert(dst->kind == OPK_VREG && a->kind == OPK_MEM_BD);
    snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%d(%%1), %%0\n", c, a->u.mem_bd.disp);
    defs[0] = dst->u.vreg;
    uses[0] = a->u.mem_bd.base;
    em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1, tmpl);
    break;
  }

  case BE_STORE: {
    if (dst->kind == OPK_MEM_FRAME) {
      char fb[40];
      x86_frame(dst, fb, sizeof(fb));
      if (a->kind == OPK_PHYS || a->kind == OPK_IMM) {
        /* [frame] := <phys/imm>  →  movX <phys/$imm>, <frame>  (both operands
         * literal: no placeholder, no def/use).  A float width uses the SSE
         * scalar mnemonic (movsd/movss) from an xmm register. */
        char lit[48];
        x86_lit(a, lit, sizeof(lit));
        if (x86_is_float(w))
          snprintf(tmpl, sizeof(tmpl), "\tmov%s\t%s, %s\n", x86_fsuffix(w), lit,
                   fb);
        else
          snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%s, %s\n", c, lit, fb);
        em->list = add_inst(em->list, tmpl);
        break;
      }
      /* [frame] := a(vreg)  →  movX %0, disp(%rbp)  (fixed base, no def) */
      assert(a->kind == OPK_VREG);
      snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%%0, %s\n", c, fb);
      uses[0] = a->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, tmpl);
      break;
    }
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

  case BE_DIV:
  case BE_MOD: {
    /* Signed division: dividend in (r/e)ax, sign-extended into (r/e)dx by
     * cltd/cqto, idiv by the divisor; quotient in (r/e)ax, remainder in
     * (r/e)dx.  rax/rdx are not in the allocatable pool, so clobbering them is
     * safe.  a, b, dst are pool vregs. */
    assert(dst->kind == OPK_VREG && a->kind == OPK_VREG && b->kind == OPK_VREG);
    const char *ax = (w == BE_W32) ? "%eax" : "%rax";
    const char *dx = (w == BE_W32) ? "%edx" : "%rdx";
    const char *cvt = (w == BE_W32) ? "\tcltd\n" : "\tcqto\n";
    const char *res = (op == BE_DIV) ? ax : dx;
    Register_t *u1[1];
    snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%%0, %s\n", c, ax);
    u1[0] = a->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, u1, 1, tmpl);
    em->list = add_inst(em->list, cvt);
    snprintf(tmpl, sizeof(tmpl), "\tidiv%c\t%%0\n", c);
    u1[0] = b->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, u1, 1, tmpl);
    snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%s, %%0\n", c, res);
    defs[0] = dst->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), defs, 1, NULL, 0, tmpl);
    break;
  }

  case BE_CMP: {
    /* compare a to b: cmpX <b>, <a>  (flags reflect a ? b) */
    if (a->kind == OPK_MEM_FRAME || b->kind == OPK_MEM_FRAME) {
      /* One operand is a frame slot (fixed base, literal — no placeholder).
       * cmp is a pure read: no def, and the only use is the other operand when
       * it is a vreg.  AT&T order is `cmp <b>, <a>`. */
      char fb[40];
      if (a->kind == OPK_MEM_FRAME && b->kind == OPK_VREG) {
        /* cmpX %0, <frameA> : use b */
        x86_frame(a, fb, sizeof(fb));
        snprintf(tmpl, sizeof(tmpl), "\tcmp%c\t%%0, %s\n", c, fb);
        uses[0] = b->u.vreg;
        em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, tmpl);
      } else if (a->kind == OPK_MEM_FRAME) {
        /* cmpX <b-lit>, <frameA> : b is imm/phys — both literal */
        x86_frame(a, fb, sizeof(fb));
        x86_lit(b, lit, sizeof(lit));
        snprintf(tmpl, sizeof(tmpl), "\tcmp%c\t%s, %s\n", c, lit, fb);
        em->list = add_inst(em->list, tmpl);
      } else if (a->kind == OPK_VREG) {
        /* cmpX <frameB>, %0 : use a */
        x86_frame(b, fb, sizeof(fb));
        snprintf(tmpl, sizeof(tmpl), "\tcmp%c\t%s, %%0\n", c, fb);
        uses[0] = a->u.vreg;
        em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, tmpl);
      } else {
        /* cmpX <frameB>, <a-lit> : a is imm/phys — both literal */
        x86_frame(b, fb, sizeof(fb));
        x86_lit(a, lit, sizeof(lit));
        snprintf(tmpl, sizeof(tmpl), "\tcmp%c\t%s, %s\n", c, fb, lit);
        em->list = add_inst(em->list, tmpl);
      }
      break;
    }
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
    if (a->kind == OPK_MEM_FRAME) {
      /* dst := &frame-slot  ->  leaq disp(%rbp), <dst>.  The address is always
       * 64-bit (leaq), independent of the op width.  A vreg dst uses the %0
       * placeholder (def, no use); a physical dst renders literally via
       * add_inst (no placeholder/def-use). */
      char fb[40];
      x86_frame(a, fb, sizeof(fb));
      if (dst->kind == OPK_PHYS) {
        snprintf(tmpl, sizeof(tmpl), "\tleaq\t%s, %s\n", fb, dst->u.phys);
        em->list = add_inst(em->list, tmpl);
      } else {
        assert(dst->kind == OPK_VREG);
        snprintf(tmpl, sizeof(tmpl), "\tleaq\t%s, %%0\n", fb);
        defs[0] = dst->u.vreg;
        em->list = be_add_inst_du(em->list, vregp(em), defs, 1, NULL, 0, tmpl);
      }
      break;
    }
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

  case BE_CVT_I2F: {
    /* dst(float vreg) := (float)src(int).  src is a physical integer register
     * (the "other class" operand — see fregpool's mixed-class limitation).
     * cvtsi2sd/cvtsi2ss; the source's register name (%edi vs %rdi) tells gas
     * the integer width, so no mnemonic suffix is needed. */
    assert(dst->kind == OPK_VREG && a->kind == OPK_PHYS);
    const char *mn = (dst->width == BE_WF32) ? "cvtsi2ss" : "cvtsi2sd";
    snprintf(tmpl, sizeof(tmpl), "\t%s\t%s, %%0\n", mn, a->u.phys);
    defs[0] = dst->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), defs, 1, NULL, 0, tmpl);
    break;
  }

  case BE_CVT_F2I: {
    /* dst(int) := (int)src(float vreg), truncating.  dst is a physical integer
     * register.  cvttsd2si/cvttss2si; the destination register name (%eax vs
     * %rax) tells gas the integer width. */
    assert(dst->kind == OPK_PHYS && a->kind == OPK_VREG);
    const char *mn = (a->width == BE_WF32) ? "cvttss2si" : "cvttsd2si";
    snprintf(tmpl, sizeof(tmpl), "\t%s\t%%0, %s\n", mn, dst->u.phys);
    uses[0] = a->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, tmpl);
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

static const char *x86_setcc_mnemonic(BeCond cc) {
  switch (cc) {
  case BE_EQ:
    return "sete";
  case BE_NE:
    return "setne";
  case BE_LT:
    return "setl";
  case BE_LE:
    return "setle";
  case BE_GT:
    return "setg";
  case BE_GE:
    return "setge";
  case BE_ULT:
    return "setb";
  case BE_ULE:
    return "setbe";
  case BE_UGT:
    return "seta";
  case BE_UGE:
    return "setae";
  default:
    return "sete";
  }
}

static void x86_emit_setcc(BeEmitter *em, BeCond cc, const BeOperand *dst) {
  /* setCC %al ; movzbl %al, <dst32> — materialize the boolean into dst. */
  assert(dst->kind == OPK_VREG);
  char buf[64];
  snprintf(buf, sizeof(buf), "\t%s\t%%al\n", x86_setcc_mnemonic(cc));
  em->list = add_inst(em->list, buf);
  Register_t *defs[1] = {dst->u.vreg};
  int use32[1] = {1};
  em->list = be_add_inst_du_w(em->list, vregp(em), defs, 1, NULL, 0,
                              "\tmovzbl\t%al, %0\n", use32);
}

/* Map a BeWidth to a reg_width_sel code (see ir_inst.h): 1=64,2=32,3=16,4=8. */
static int x86_width_sel(BeWidth w) {
  switch (w) {
  case BE_W8:
    return 4;
  case BE_W16:
    return 3;
  case BE_W32:
    return 2;
  case BE_W64:
  default:
    return 1;
  }
}

static void x86_emit_ext(BeEmitter *em, const BeOperand *dst,
                         const BeOperand *src, BeWidth from, BeWidth to,
                         int is_signed) {
  assert(from == BE_W8 || from == BE_W16 || from == BE_W32);
  assert(to == BE_W32 || to == BE_W64);

  const char *mn;
  BeWidth dstw; /* width the destination register name is rendered at */

  if (is_signed) {
    /* movsbl/movsbq, movswl/movswq, movslq — dst rendered at `to`. */
    if (from == BE_W8)
      mn = (to == BE_W64) ? "movsbq" : "movsbl";
    else if (from == BE_W16)
      mn = (to == BE_W64) ? "movswq" : "movswl";
    else { /* from == BE_W32 → to must be W64 */
      assert(to == BE_W64 && "signed 32→32 extend is a no-op");
      mn = "movslq";
    }
    dstw = to;
  } else {
    /* Zero-extend.  movzbl/movzwl write a 32-bit dest, which the CPU
     * zero-fills into the full 64-bit register — so the dest is always
     * rendered 32-bit even when `to` is W64.  Zero-extending W32→W64 is a
     * plain 32-bit mov (writing e-reg clears the upper half). */
    if (from == BE_W8)
      mn = "movzbl";
    else if (from == BE_W16)
      mn = "movzwl";
    else /* from == BE_W32 */
      mn = "movl";
    dstw = BE_W32;
  }

  /* Frame-source sign/zero-extend load into a fixed physical register:
   *   <mn> disp(%rbp), <phys>   (e.g. movslq -N(%rbp), %rax)
   * Both operands are literal — no %N placeholder, no def/use (a physical
   * register is never allocated).  The destination's register name (%rax vs
   * %eax) already carries `dstw`, so no width-sel is needed.  Matches the raw
   * add_inst templates the live compiler uses for spill-slot widening loads. */
  if (src->kind == OPK_MEM_FRAME) {
    assert(dst->kind == OPK_PHYS);
    char fb[40];
    x86_frame(src, fb, sizeof(fb));
    char ftmpl[80];
    snprintf(ftmpl, sizeof(ftmpl), "\t%s\t%s, %s\n", mn, fb, dst->u.phys);
    em->list = add_inst(em->list, ftmpl);
    return;
  }

  assert(dst->kind == OPK_VREG && src->kind == OPK_VREG);
  char tmpl[64];
  snprintf(tmpl, sizeof(tmpl), "\t%s\t%%1, %%0\n", mn);
  Register_t *defs[1] = {dst->u.vreg};
  Register_t *uses[1] = {src->u.vreg};
  int sel[2];
  sel[0] = x86_width_sel(dstw);  /* %0 = destination */
  sel[1] = x86_width_sel(from);  /* %1 = source */
  em->list = be_add_inst_du_wsel(em->list, vregp(em), defs, 1, uses, 1, tmpl,
                                 sel);
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
  static const char *xmm[] = {"%xmm0", "%xmm1", "%xmm2", "%xmm3",
                              "%xmm4", "%xmm5", "%xmm6", "%xmm7"};
  if (w == BE_WF32 || w == BE_WF64)
    return (idx < 0 || idx >= 8) ? NULL : xmm[idx];
  if (idx < 0 || idx >= 6)
    return NULL;
  return (w == BE_W32) ? r32[idx] : r64[idx];
}

static int x86_num_int_arg_regs(void) { return 6; }

static const char *x86_return_reg(BeWidth w) {
  if (w == BE_WF32 || w == BE_WF64)
    return "%xmm0";
  return (w == BE_W32) ? "%eax" : "%rax";
}

/* Allocatable pool: x86-64 callee-saved registers (matches stackmng's default;
 * provided through the vtable so pool selection is uniform across targets). */
static const BackendRegSpec kX86Pool[] = {
    {REG_RBX, "%rbx", "%ebx", "%bx", "%bl"},
    {REG_R12, "%r12", "%r12d", "%r12w", "%r12b"},
    {REG_R13, "%r13", "%r13d", "%r13w", "%r13b"},
    {REG_R14, "%r14", "%r14d", "%r14w", "%r14b"},
    {REG_R15, "%r15", "%r15d", "%r15w", "%r15b"},
};

static const BackendRegSpec *x86_regpool(int *n) {
  *n = (int)(sizeof(kX86Pool) / sizeof(kX86Pool[0]));
  return kX86Pool;
}

/* Allocatable float pool: SSE xmm8..xmm15.  Args/return use xmm0..xmm7, so
 * xmm8+ never alias the incoming/outgoing float values (no save needed in a
 * leaf).  xmm registers have a single width, so all four name columns are the
 * same "%xmmN" — the width-substitution heuristic is therefore a no-op.  The
 * reg_id column is an opaque distinct id (reusing the GP enum values). */
static const BackendRegSpec kX86FPool[] = {
    {REG_R8, "%xmm8", "%xmm8", "%xmm8", "%xmm8"},
    {REG_R9, "%xmm9", "%xmm9", "%xmm9", "%xmm9"},
    {REG_R10, "%xmm10", "%xmm10", "%xmm10", "%xmm10"},
    {REG_R11, "%xmm11", "%xmm11", "%xmm11", "%xmm11"},
    {REG_R12, "%xmm12", "%xmm12", "%xmm12", "%xmm12"},
    {REG_R13, "%xmm13", "%xmm13", "%xmm13", "%xmm13"},
    {REG_R14, "%xmm14", "%xmm14", "%xmm14", "%xmm14"},
    {REG_R15, "%xmm15", "%xmm15", "%xmm15", "%xmm15"},
};

static const BackendRegSpec *x86_fregpool(int *n) {
  *n = (int)(sizeof(kX86FPool) / sizeof(kX86FPool[0]));
  return kX86FPool;
}

/* ---- Directive / data channel (AT&T GAS) -------------------------------- */

static void x86_emit_section(BeEmitter *em, BeSection s) {
  const char *d = (s == BE_SEC_TEXT)   ? "\t.text\n"
                  : (s == BE_SEC_DATA) ? "\t.data\n"
                                       : "\t.section\t.rodata\n";
  em->list = add_inst(em->list, d);
}

static void x86_emit_global(BeEmitter *em, const char *sym) {
  char buf[128];
  snprintf(buf, sizeof(buf), "\t.globl\t%s\n", sym);
  em->list = add_inst(em->list, buf);
}

static void x86_emit_data_label(BeEmitter *em, const char *label) {
  char buf[128];
  snprintf(buf, sizeof(buf), "%s:\n", label);
  em->list = add_inst(em->list, buf);
}

static void x86_emit_data(BeEmitter *em, BeDataKind k, long long value) {
  const char *d = (k == BE_D8)    ? ".byte"
                  : (k == BE_D16) ? ".short"
                  : (k == BE_D32) ? ".long"
                                  : ".quad";
  char buf[64];
  snprintf(buf, sizeof(buf), "\t%s\t%lld\n", d, value);
  em->list = add_inst(em->list, buf);
}

/* Emit a .string, escaping the GAS-significant characters. */
static void be_string_escape(const char *s, char *out, size_t n) {
  size_t j = 0;
  for (; *s && j + 2 < n; ++s) {
    char c = *s;
    if (c == '\\' || c == '"') {
      out[j++] = '\\';
      out[j++] = c;
    } else if (c == '\n') {
      out[j++] = '\\';
      out[j++] = 'n';
    } else if (c == '\t') {
      out[j++] = '\\';
      out[j++] = 't';
    } else {
      out[j++] = c;
    }
  }
  out[j] = '\0';
}

static void x86_emit_string(BeEmitter *em, const char *s) {
  char esc[256], buf[300];
  be_string_escape(s, esc, sizeof(esc));
  snprintf(buf, sizeof(buf), "\t.string\t\"%s\"\n", esc);
  em->list = add_inst(em->list, buf);
}

static void x86_emit_zero(BeEmitter *em, int nbytes) {
  char buf[48];
  snprintf(buf, sizeof(buf), "\t.zero\t%d\n", nbytes);
  em->list = add_inst(em->list, buf);
}

static void x86_emit_align(BeEmitter *em, int nbytes) {
  char buf[48];
  snprintf(buf, sizeof(buf), "\t.align\t%d\n", nbytes);
  em->list = add_inst(em->list, buf);
}

static const Target kX86SysV = {
    .name = "x86_64-sysv",
    .ptr_width = 8,
    .emit = x86_emit,
    .emit_setcc = x86_emit_setcc,
    .emit_ext = x86_emit_ext,
    .emit_branch = x86_emit_branch,
    .emit_call = x86_emit_call,
    .emit_ret = x86_emit_ret,
    .emit_label = x86_emit_label,
    .emit_prologue = x86_emit_prologue,
    .emit_epilogue = x86_emit_epilogue,
    .emit_section = x86_emit_section,
    .emit_global = x86_emit_global,
    .emit_data_label = x86_emit_data_label,
    .emit_data = x86_emit_data,
    .emit_string = x86_emit_string,
    .emit_zero = x86_emit_zero,
    .emit_align = x86_emit_align,
    .arg_reg = x86_arg_reg,
    .num_int_arg_regs = x86_num_int_arg_regs,
    .return_reg = x86_return_reg,
    .regpool = x86_regpool,
    .fregpool = x86_fregpool,
};

const Target *target_x86_sysv(void) { return &kX86SysV; }

const Target *kgpc_backend_target(void) { return &kX86SysV; }
