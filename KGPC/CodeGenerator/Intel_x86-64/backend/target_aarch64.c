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

static int aa_is_float(BeWidth w) { return w == BE_WF32 || w == BE_WF64; }

/* Index scale (1/2/4/8) -> AArch64 lsl shift amount (0/1/2/3). */
static int aa_scale_shift(int scale) {
  switch (scale) {
  case 2:
    return 1;
  case 4:
    return 2;
  case 8:
    return 3;
  default:
    return 0;
  }
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

/* AArch64 scalar-FP ops (fadd/fsub/fmul/fdiv/fmov/fcmp) on the d/s register
 * file.  The float pool's register names carry the width (d8 vs s8), so — like
 * the integer path — no mnemonic suffix is used; the register name is the
 * width.  All four name columns of a float pool register are identical, so the
 * width-substitution heuristic (which keys off a trailing 'l', e.g. fmul) is a
 * harmless no-op.  Conversions cross register classes and are in aa_emit. */
static void aa_emit_float(BeEmitter *em, BeOp op, BeWidth w,
                          const BeOperand *dst, const BeOperand *a,
                          const BeOperand *b) {
  (void)w;
  char tmpl[160];
  char lit[48];
  Register_t *defs[2];
  Register_t *uses[4];

  switch (op) {
  case BE_MOV:
    if (dst->kind == OPK_VREG && a->kind == OPK_VREG) {
      defs[0] = dst->u.vreg;
      uses[0] = a->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1,
                                "\tfmov\t%0, %1\n");
    } else if (dst->kind == OPK_VREG) {
      aa_lit(a, lit, sizeof(lit));
      snprintf(tmpl, sizeof(tmpl), "\tfmov\t%%0, %s\n", lit);
      defs[0] = dst->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, NULL, 0, tmpl);
    } else if (a->kind == OPK_VREG) {
      aa_lit(dst, lit, sizeof(lit));
      snprintf(tmpl, sizeof(tmpl), "\tfmov\t%s, %%0\n", lit);
      uses[0] = a->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, tmpl);
    } else {
      char litd[48];
      aa_lit(a, lit, sizeof(lit));
      aa_lit(dst, litd, sizeof(litd));
      snprintf(tmpl, sizeof(tmpl), "\tfmov\t%s, %s\n", litd, lit);
      em->list = add_inst(em->list, tmpl);
    }
    break;

  case BE_ADD:
  case BE_SUB:
  case BE_MUL:
  case BE_DIV: {
    /* 3-operand: <mn> dst, a, b */
    const char *mn = (op == BE_ADD)   ? "fadd"
                     : (op == BE_SUB) ? "fsub"
                     : (op == BE_MUL) ? "fmul"
                                      : "fdiv";
    assert(dst->kind == OPK_VREG && a->kind == OPK_VREG && b->kind == OPK_VREG);
    snprintf(tmpl, sizeof(tmpl), "\t%s\t%%0, %%1, %%2\n", mn);
    defs[0] = dst->u.vreg;
    uses[0] = a->u.vreg;
    uses[1] = b->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 2, tmpl);
    break;
  }

  case BE_CMP:
    assert(a->kind == OPK_VREG && b->kind == OPK_VREG);
    uses[0] = a->u.vreg;
    uses[1] = b->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 2,
                              "\tfcmp\t%0, %1\n");
    break;

  default:
    assert(0 && "unsupported aarch64 float op");
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

  /* Float-width arithmetic/mov/cmp dispatch to the scalar-FP path.  LOAD/STORE
   * are excluded: an AArch64 float frame load/store is a plain ldr/str to/from
   * a d/s register (the register name carries the width), handled by the frame
   * branches of BE_LOAD/BE_STORE below. */
  if (aa_is_float(w) && op != BE_CVT_I2F && op != BE_CVT_F2I &&
      op != BE_LOAD && op != BE_STORE) {
    aa_emit_float(em, op, w, dst, a, b);
    return;
  }

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
    if (a->kind == OPK_MEM_FRAME || b->kind == OPK_MEM_FRAME) {
      /* AArch64 has no compare-with-memory: load the frame operand into a
       * scratch register, then compare register-to-{register,immediate}.  The
       * load-then-compare sequence is the neutral counterpart to x86's
       * single cmp <mem>,<reg>. */
      const BeOperand *frame = (a->kind == OPK_MEM_FRAME) ? a : b;
      const char *base = (frame->u.mem_frame.base == BE_BASE_SP) ? "sp" : "x29";
      Register_t *scr = get_free_reg(get_reg_stack(), &em->list);
      assert(scr != NULL && "no scratch for aarch64 cmp-with-frame");
      snprintf(tmpl, sizeof(tmpl), "\tldr\t%%0, [%s, #%lld]\n", base,
               frame->u.mem_frame.disp);
      defs[0] = scr;
      use32[0] = w32;
      em->list =
          be_add_inst_du_w(em->list, vregp(em), defs, 1, NULL, 0, tmpl, use32);
      BeOperand sreg = {OPK_VREG, w, {.vreg = scr}};
      const BeOperand *na = (a->kind == OPK_MEM_FRAME) ? &sreg : a;
      const BeOperand *nb = (b->kind == OPK_MEM_FRAME) ? &sreg : b;
      aa_emit(em, BE_CMP, w, NULL, na, nb); /* reg/reg or reg/imm compare */
      free_reg(get_reg_stack(), scr);
      break;
    }
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
    if (a->kind == OPK_MEM_FRAME) {
      const char *base = (a->u.mem_frame.base == BE_BASE_SP) ? "sp" : "x29";
      if (dst->kind == OPK_PHYS) {
        /* <phys> := [frame]  →  ldr <phys>, [<fp/sp>, #disp]  (both literal,
         * no placeholder/def-use — a physical register is never allocated). */
        snprintf(tmpl, sizeof(tmpl), "\tldr\t%s, [%s, #%lld]\n", dst->u.phys,
                 base, a->u.mem_frame.disp);
        em->list = add_inst(em->list, tmpl);
        break;
      }
      /* dst(vreg) := [frame]  →  ldr %0, [<fp/sp>, #disp]  (fixed base) */
      assert(dst->kind == OPK_VREG);
      snprintf(tmpl, sizeof(tmpl), "\tldr\t%%0, [%s, #%lld]\n", base,
               a->u.mem_frame.disp);
      defs[0] = dst->u.vreg;
      use32[0] = w32; /* loaded value width */
      em->list =
          be_add_inst_du_w(em->list, vregp(em), defs, 1, NULL, 0, tmpl, use32);
      break;
    }
    if (a->kind == OPK_MEM_BIS) {
      /* dst(vreg) := [base + index*scale]  →  ldr %0, [%1, %2, lsl #shift].
       * base(%1)/index(%2) are tracked vreg USES at 64-bit; value at width w. */
      assert(dst->kind == OPK_VREG);
      int shift = aa_scale_shift(a->u.mem_bis.scale);
      snprintf(tmpl, sizeof(tmpl), "\tldr\t%%0, [%%1, %%2, lsl #%d]\n", shift);
      defs[0] = dst->u.vreg;
      uses[0] = a->u.mem_bis.base;
      uses[1] = a->u.mem_bis.index;
      use32[0] = w32; /* value width */
      use32[1] = 0;   /* base = 64-bit */
      use32[2] = 0;   /* index = 64-bit */
      em->list =
          be_add_inst_du_w(em->list, vregp(em), defs, 1, uses, 2, tmpl, use32);
      break;
    }
    if (dst->kind == OPK_PHYS) {
      /* <phys> := [base(vreg)+disp]  →  ldr <phys>, [%0, #disp].  The
       * destination is a fixed physical register (literal); the base(%0) is a
       * tracked vreg USE at 64-bit, mirroring the MEM_FRAME phys-dst branch. */
      assert(a->kind == OPK_MEM_BD);
      snprintf(tmpl, sizeof(tmpl), "\tldr\t%s, [%%0, #%d]\n", dst->u.phys,
               a->u.mem_bd.disp);
      uses[0] = a->u.mem_bd.base;
      use32[0] = 0; /* address register is always 64-bit */
      em->list =
          be_add_inst_du_w(em->list, vregp(em), NULL, 0, uses, 1, tmpl, use32);
      break;
    }
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
    if (dst->kind == OPK_MEM_FRAME) {
      const char *base = (dst->u.mem_frame.base == BE_BASE_SP) ? "sp" : "x29";
      if (a->kind == OPK_PHYS) {
        /* [frame] := <phys>  →  str <phys>, [<fp/sp>, #disp]  (both literal). */
        snprintf(tmpl, sizeof(tmpl), "\tstr\t%s, [%s, #%lld]\n", a->u.phys, base,
                 dst->u.mem_frame.disp);
        em->list = add_inst(em->list, tmpl);
        break;
      }
      if (a->kind == OPK_IMM) {
        /* AArch64 has no store-immediate.  Zero stores the zero register
         * directly; any other value is materialized into a scratch register
         * first (the neutral counterpart to x86's single movX $imm,disp(%rbp)). */
        if (a->u.imm == 0) {
          snprintf(tmpl, sizeof(tmpl), "\tstr\t%s, [%s, #%lld]\n",
                   w32 ? "wzr" : "xzr", base, dst->u.mem_frame.disp);
          em->list = add_inst(em->list, tmpl);
        } else {
          Register_t *scr = get_free_reg(get_reg_stack(), &em->list);
          assert(scr != NULL && "no scratch register for aarch64 imm store");
          BeOperand sd = {OPK_VREG, w, {.vreg = scr}};
          BeOperand si = {OPK_IMM, w, {.imm = a->u.imm}};
          aa_emit(em, BE_MOV, w, &sd, &si, NULL); /* mov scr, #imm */
          snprintf(tmpl, sizeof(tmpl), "\tstr\t%%0, [%s, #%lld]\n", base,
                   dst->u.mem_frame.disp);
          uses[0] = scr;
          use32[0] = w32;
          em->list = be_add_inst_du_w(em->list, vregp(em), NULL, 0, uses, 1,
                                      tmpl, use32);
          free_reg(get_reg_stack(), scr);
        }
        break;
      }
      /* [frame] := a(vreg)  →  str %0, [<fp/sp>, #disp]  (fixed base) */
      assert(a->kind == OPK_VREG);
      snprintf(tmpl, sizeof(tmpl), "\tstr\t%%0, [%s, #%lld]\n", base,
               dst->u.mem_frame.disp);
      uses[0] = a->u.vreg;
      use32[0] = w32; /* stored value width */
      em->list =
          be_add_inst_du_w(em->list, vregp(em), NULL, 0, uses, 1, tmpl, use32);
      break;
    }
    if (dst->kind == OPK_MEM_BIS) {
      /* [base + index*scale] := a(vreg)  →  str %0, [%1, %2, lsl #shift]. */
      assert(a->kind == OPK_VREG);
      int shift = aa_scale_shift(dst->u.mem_bis.scale);
      snprintf(tmpl, sizeof(tmpl), "\tstr\t%%0, [%%1, %%2, lsl #%d]\n", shift);
      uses[0] = a->u.vreg;
      uses[1] = dst->u.mem_bis.base;
      uses[2] = dst->u.mem_bis.index;
      use32[0] = w32; /* value width */
      use32[1] = 0;   /* base = 64-bit */
      use32[2] = 0;   /* index = 64-bit */
      em->list =
          be_add_inst_du_w(em->list, vregp(em), NULL, 0, uses, 3, tmpl, use32);
      break;
    }
    assert(dst->kind == OPK_MEM_BD && a->kind == OPK_VREG);
    snprintf(tmpl, sizeof(tmpl), "\tstr\t%%0, [%%1, #%d]\n", dst->u.mem_bd.disp);
    uses[0] = a->u.vreg;
    uses[1] = dst->u.mem_bd.base;
    use32[0] = w32; /* stored value width */
    use32[1] = 0;   /* address register is always 64-bit */
    em->list =
        be_add_inst_du_w(em->list, vregp(em), NULL, 0, uses, 2, tmpl, use32);
    break;

  case BE_CVT_I2F: {
    /* scvtf <dst float vreg>, <src int phys>.  The int source's register name
     * (w0 vs x0) carries its width. */
    assert(dst->kind == OPK_VREG && a->kind == OPK_PHYS);
    snprintf(tmpl, sizeof(tmpl), "\tscvtf\t%%0, %s\n", a->u.phys);
    defs[0] = dst->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), defs, 1, NULL, 0, tmpl);
    break;
  }

  case BE_CVT_F2I: {
    /* fcvtzs <dst int phys>, <src float vreg> (truncating).  The int dest's
     * register name (w0 vs x0) carries its width. */
    assert(dst->kind == OPK_PHYS && a->kind == OPK_VREG);
    snprintf(tmpl, sizeof(tmpl), "\tfcvtzs\t%s, %%0\n", dst->u.phys);
    uses[0] = a->u.vreg;
    em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, tmpl);
    break;
  }

  case BE_LEA:
    if (a->kind == OPK_MEM_FRAME) {
      /* dst := &frame-slot.  AArch64 has no lea; a frame-slot address is
       * base +/- disp: add for a non-negative offset, sub for a negative one
       * (the immediate is unsigned).  Neutral counterpart to x86 leaq
       * disp(%rbp),<dst> — the divergent add/sub spelling is the proof. */
      const char *base = (a->u.mem_frame.base == BE_BASE_SP) ? "sp" : "x29";
      long long d = a->u.mem_frame.disp;
      const char *op_mn = (d < 0) ? "sub" : "add";
      long long mag = (d < 0) ? -d : d;
      if (dst->kind == OPK_PHYS) {
        snprintf(tmpl, sizeof(tmpl), "\t%s\t%s, %s, #%lld\n", op_mn,
                 dst->u.phys, base, mag);
        em->list = add_inst(em->list, tmpl);
      } else {
        assert(dst->kind == OPK_VREG);
        snprintf(tmpl, sizeof(tmpl), "\t%s\t%%0, %s, #%lld\n", op_mn, base, mag);
        defs[0] = dst->u.vreg;
        use32[0] = 0; /* an address is always 64-bit */
        em->list =
            be_add_inst_du_w(em->list, vregp(em), defs, 1, NULL, 0, tmpl, use32);
      }
      break;
    }
    /* fall through to unsupported */
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

/* Map a BeWidth to a reg_width_sel code (see ir_inst.h): 1=64,2=32,3=16,4=8.
 * AArch64 has no distinct 8/16-bit register views — the pool's 16/8 names are
 * the same "w" register — so codes 3/4 resolve to the w-name too. */
static int aa_width_sel(BeWidth w) {
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

static void aa_emit_ext(BeEmitter *em, const BeOperand *dst,
                        const BeOperand *src, BeWidth from, BeWidth to,
                        int is_signed) {
  assert(from == BE_W8 || from == BE_W16 || from == BE_W32);
  assert(to == BE_W32 || to == BE_W64);

  /* Frame-source extend load into a fixed physical register: the AArch64
   * load-with-extend family (ldrsb/ldrsh/ldrsw signed; ldrb/ldrh/ldr unsigned),
   * rendered literally.  Neutral counterpart to x86's movsX/movzX disp(%rbp),
   * <phys>.  The dest register name (w vs x) carries its width. */
  if (src->kind == OPK_MEM_FRAME) {
    assert(dst->kind == OPK_PHYS);
    const char *base = (src->u.mem_frame.base == BE_BASE_SP) ? "sp" : "x29";
    const char *lmn = is_signed
                          ? (from == BE_W8 ? "ldrsb"
                             : from == BE_W16 ? "ldrsh"
                                              : "ldrsw")
                          : (from == BE_W8 ? "ldrb"
                             : from == BE_W16 ? "ldrh"
                                              : "ldr");
    char ftmpl[80];
    snprintf(ftmpl, sizeof(ftmpl), "\t%s\t%s, [%s, #%lld]\n", lmn, dst->u.phys,
             base, src->u.mem_frame.disp);
    em->list = add_inst(em->list, ftmpl);
    return;
  }

  /* Memory-source extend load into a vreg: the load-with-extend family from a
   * tracked base(%1) [+ index(%2)] address.  base/index are 64-bit USES; the
   * dest register name (w vs x) carries its width. */
  if (src->kind == OPK_MEM_BD || src->kind == OPK_MEM_BIS) {
    assert(dst->kind == OPK_VREG);
    const char *lmn = is_signed
                          ? (from == BE_W8 ? "ldrsb"
                             : from == BE_W16 ? "ldrsh"
                                              : "ldrsw")
                          : (from == BE_W8 ? "ldrb"
                             : from == BE_W16 ? "ldrh"
                                              : "ldr");
    char mtmpl[80];
    Register_t *muses[2];
    int nu, sel[3];
    if (src->kind == OPK_MEM_BD) {
      snprintf(mtmpl, sizeof(mtmpl), "\t%s\t%%0, [%%1, #%d]\n", lmn,
               src->u.mem_bd.disp);
      muses[0] = src->u.mem_bd.base;
      nu = 1;
    } else {
      snprintf(mtmpl, sizeof(mtmpl), "\t%s\t%%0, [%%1, %%2, lsl #%d]\n", lmn,
               aa_scale_shift(src->u.mem_bis.scale));
      muses[0] = src->u.mem_bis.base;
      muses[1] = src->u.mem_bis.index;
      nu = 2;
    }
    Register_t *mdefs[1] = {dst->u.vreg};
    sel[0] = aa_width_sel(to == BE_W64 && is_signed ? BE_W64 : BE_W32);
    sel[1] = 1; /* base 64-bit */
    sel[2] = 1; /* index 64-bit */
    em->list = be_add_inst_du_wsel(em->list, vregp(em), mdefs, 1, muses, nu,
                                   mtmpl, sel);
    return;
  }

  assert(dst->kind == OPK_VREG && src->kind == OPK_VREG);

  const char *mn;
  BeWidth dstw;

  if (is_signed) {
    /* sxtb/sxth take a 32-bit source (Wn) and a W- or X-dest; sxtw is X<-W. */
    if (from == BE_W8)
      mn = "sxtb";
    else if (from == BE_W16)
      mn = "sxth";
    else { /* from == BE_W32 */
      assert(to == BE_W64 && "signed 32→32 extend is a no-op");
      mn = "sxtw";
    }
    dstw = to;
  } else {
    /* uxtb/uxth write a W-dest (upper 32 bits of the X-reg are zeroed).
     * Zero-extending W32→W64 is a plain `mov Wd, Wn` (writing W clears the
     * upper half). */
    if (from == BE_W8)
      mn = "uxtb";
    else if (from == BE_W16)
      mn = "uxth";
    else /* from == BE_W32 */
      mn = "mov";
    dstw = BE_W32;
  }

  char tmpl[64];
  snprintf(tmpl, sizeof(tmpl), "\t%s\t%%0, %%1\n", mn);
  Register_t *defs[1] = {dst->u.vreg};
  Register_t *uses[1] = {src->u.vreg};
  int sel[2];
  sel[0] = aa_width_sel(dstw); /* %0 = destination */
  sel[1] = 2;                  /* %1 = source is always the 32-bit w-view */
  em->list = be_add_inst_du_wsel(em->list, vregp(em), defs, 1, uses, 1, tmpl,
                                 sel);
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

/* AAPCS64 arg/return registers (w0-w7 for 32-bit, x0-x7 for 64-bit; d0-d7 for
 * double / s0-s7 for single float args). */
static const char *aa_arg_reg(int idx, BeWidth w) {
  static const char *x[] = {"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"};
  static const char *wr[] = {"w0", "w1", "w2", "w3", "w4", "w5", "w6", "w7"};
  static const char *d[] = {"d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7"};
  static const char *s[] = {"s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7"};
  if (idx < 0 || idx >= 8)
    return NULL;
  if (w == BE_WF64)
    return d[idx];
  if (w == BE_WF32)
    return s[idx];
  return (w == BE_W32) ? wr[idx] : x[idx];
}

static int aa_num_int_arg_regs(void) { return 8; }

static const char *aa_return_reg(BeWidth w) {
  if (w == BE_WF64)
    return "d0";
  if (w == BE_WF32)
    return "s0";
  return (w == BE_W32) ? "w0" : "x0";
}

/* Allocatable pool: AArch64 callee-saved x19-x23.  The reg_id slots are reused
 * as opaque identifiers (the allocator only needs distinct ids + names). */
static const BackendRegSpec kAArch64Pool[] = {
    {REG_RBX, "x19", "w19", "w19", "w19"},
    {REG_R12, "x20", "w20", "w20", "w20"},
    {REG_R13, "x21", "w21", "w21", "w21"},
    {REG_R14, "x22", "w22", "w22", "w22"},
    {REG_R15, "x23", "w23", "w23", "w23"},
};

static const BackendRegSpec *aa_regpool(int *n) {
  *n = (int)(sizeof(kAArch64Pool) / sizeof(kAArch64Pool[0]));
  return kAArch64Pool;
}

/* Allocatable float pool: callee-saved d8..d15 (args/return use d0..d7).  All
 * four name columns carry the double-precision d-name; the width heuristic is a
 * no-op.  reg_id is an opaque distinct id (reusing the GP enum values). */
static const BackendRegSpec kAArch64FPool[] = {
    {REG_R8, "d8", "d8", "d8", "d8"},
    {REG_R9, "d9", "d9", "d9", "d9"},
    {REG_R10, "d10", "d10", "d10", "d10"},
    {REG_R11, "d11", "d11", "d11", "d11"},
    {REG_R12, "d12", "d12", "d12", "d12"},
    {REG_R13, "d13", "d13", "d13", "d13"},
    {REG_R14, "d14", "d14", "d14", "d14"},
    {REG_R15, "d15", "d15", "d15", "d15"},
};

static const BackendRegSpec *aa_fregpool(int *n) {
  *n = (int)(sizeof(kAArch64FPool) / sizeof(kAArch64FPool[0]));
  return kAArch64FPool;
}

/* ---- Directive / data channel (GNU as) ---------------------------------
 * Section and symbol directives share GAS spelling with x86; the data-word
 * mnemonics use the AArch64-idiomatic names (.hword/.word/.xword) to make the
 * neutral API's per-target rendering explicit.  (GNU as for aarch64 also
 * accepts .short/.long/.quad as synonyms.) */

static void aa_emit_section(BeEmitter *em, BeSection s) {
  const char *d = (s == BE_SEC_TEXT)   ? "\t.text\n"
                  : (s == BE_SEC_DATA) ? "\t.data\n"
                                       : "\t.section\t.rodata\n";
  em->list = add_inst(em->list, d);
}

static void aa_emit_global(BeEmitter *em, const char *sym) {
  char buf[128];
  snprintf(buf, sizeof(buf), "\t.globl\t%s\n", sym);
  em->list = add_inst(em->list, buf);
}

static void aa_emit_data_label(BeEmitter *em, const char *label) {
  char buf[128];
  snprintf(buf, sizeof(buf), "%s:\n", label);
  em->list = add_inst(em->list, buf);
}

static void aa_emit_data(BeEmitter *em, BeDataKind k, long long value) {
  const char *d = (k == BE_D8)    ? ".byte"
                  : (k == BE_D16) ? ".hword"
                  : (k == BE_D32) ? ".word"
                                  : ".xword";
  char buf[64];
  snprintf(buf, sizeof(buf), "\t%s\t%lld\n", d, value);
  em->list = add_inst(em->list, buf);
}

static void aa_string_escape(const char *s, char *out, size_t n) {
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

static void aa_emit_string(BeEmitter *em, const char *s) {
  char esc[256], buf[300];
  aa_string_escape(s, esc, sizeof(esc));
  snprintf(buf, sizeof(buf), "\t.string\t\"%s\"\n", esc);
  em->list = add_inst(em->list, buf);
}

static void aa_emit_zero(BeEmitter *em, int nbytes) {
  char buf[48];
  snprintf(buf, sizeof(buf), "\t.zero\t%d\n", nbytes);
  em->list = add_inst(em->list, buf);
}

static void aa_emit_align(BeEmitter *em, int nbytes) {
  char buf[48];
  snprintf(buf, sizeof(buf), "\t.align\t%d\n", nbytes);
  em->list = add_inst(em->list, buf);
}

static const Target kAArch64 = {
    .name = "aarch64",
    .ptr_width = 8,
    .emit = aa_emit,
    .emit_setcc = aa_emit_setcc,
    .emit_ext = aa_emit_ext,
    .emit_branch = aa_emit_branch,
    .emit_call = aa_emit_call,
    .emit_ret = aa_emit_ret,
    .emit_label = aa_emit_label,
    .emit_prologue = aa_emit_prologue,
    .emit_epilogue = aa_emit_epilogue,
    .emit_section = aa_emit_section,
    .emit_global = aa_emit_global,
    .emit_data_label = aa_emit_data_label,
    .emit_data = aa_emit_data,
    .emit_string = aa_emit_string,
    .emit_zero = aa_emit_zero,
    .emit_align = aa_emit_align,
    .arg_reg = aa_arg_reg,
    .num_int_arg_regs = aa_num_int_arg_regs,
    .return_reg = aa_return_reg,
    .regpool = aa_regpool,
    .fregpool = aa_fregpool,
};

const Target *target_aarch64(void) { return &kAArch64; }
