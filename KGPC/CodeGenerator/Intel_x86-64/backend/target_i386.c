/*
 * target_i386.c -- Linux ELF i386 System V backend target.
 *
 * i386 passes arguments on the stack, unlike the register argument interfaces
 * used by the currently integrated x86-64 lowering.  Consequently arg_reg()
 * deliberately reports no register arguments; stack argument lowering is the
 * responsibility of a caller that targets this vtable.
 */
#include "target.h"

#include "../stackmng/stackmng.h"

#include <assert.h>
#include <stdio.h>

static int *vregp(BeEmitter *em) { return em->next_vreg_id; }

static char i386_suffix(BeWidth width) {
  switch (width) {
  case BE_W8:
    return 'b';
  case BE_W16:
    return 'w';
  case BE_W32:
    return 'l';
  case BE_W64:
  default:
    assert(0 && "i386 does not support 64-bit scalar values");
    return 'l';
  }
}

static int i386_width_sel(BeWidth width) {
  switch (width) {
  case BE_W8:
    return 4;
  case BE_W16:
    return 3;
  case BE_W32:
    return 2;
  case BE_W64:
  default:
    assert(0 && "i386 does not support 64-bit scalar values");
    return 2;
  }
}

static void i386_frame(const BeOperand *op, char *buffer, size_t size) {
  const char *base = op->u.mem_frame.base == BE_BASE_SP ? "%esp" : "%ebp";
  if (op->u.mem_frame.disp == 0)
    snprintf(buffer, size, "(%s)", base);
  else
    snprintf(buffer, size, "%lld(%s)", op->u.mem_frame.disp, base);
}

static void i386_literal(const BeOperand *op, char *buffer, size_t size) {
  switch (op->kind) {
  case OPK_IMM:
    snprintf(buffer, size, "$%lld", op->u.imm);
    break;
  case OPK_PHYS:
    snprintf(buffer, size, "%s", op->u.phys);
    break;
  case OPK_RIP_SYM:
  case OPK_LABEL:
    snprintf(buffer, size, "%s", op->u.sym);
    break;
  default:
    assert(0 && "invalid i386 literal operand");
  }
}

static void i386_emit(BeEmitter *em, BeOp op, BeWidth width,
                      const BeOperand *dst, const BeOperand *a,
                      const BeOperand *b) {
  char tmpl[256], literal[96], frame[48];
  Register_t *defs[1], *uses[3];
  char suffix = i386_suffix(width);

  assert(width != BE_WF32 && width != BE_WF64 &&
         "i386 floating-point lowering is not implemented");
  switch (op) {
  case BE_MOV:
    if (dst->kind == OPK_VREG && a->kind == OPK_VREG) {
      defs[0] = dst->u.vreg;
      uses[0] = a->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1,
                                "\tmovl\t%1, %0\n");
    } else if (dst->kind == OPK_VREG) {
      i386_literal(a, literal, sizeof(literal));
      snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%s, %%0\n", suffix, literal);
      defs[0] = dst->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, NULL, 0, tmpl);
    } else if (a->kind == OPK_VREG) {
      i386_literal(dst, literal, sizeof(literal));
      snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%%0, %s\n", suffix, literal);
      uses[0] = a->u.vreg;
      em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, tmpl);
    } else {
      char destination[96];
      i386_literal(a, literal, sizeof(literal));
      i386_literal(dst, destination, sizeof(destination));
      snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%s, %s\n", suffix, literal,
               destination);
      em->list = add_inst(em->list, tmpl);
    }
    break;

  case BE_LOAD:
    assert(dst->kind == OPK_VREG);
    defs[0] = dst->u.vreg;
    if (a->kind == OPK_MEM_FRAME) {
      i386_frame(a, frame, sizeof(frame));
      snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%s, %%0\n", suffix, frame);
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, NULL, 0, tmpl);
    } else {
      assert(a->kind == OPK_MEM_BD);
      snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%d(%%1), %%0\n", suffix,
               a->u.mem_bd.disp);
      uses[0] = a->u.mem_bd.base;
      int widths[] = {i386_width_sel(width), 2};
      em->list = be_add_inst_du_wsel(em->list, vregp(em), defs, 1, uses, 1,
                                     tmpl, widths);
    }
    break;

  case BE_STORE:
    if (dst->kind == OPK_MEM_FRAME) {
      i386_frame(dst, frame, sizeof(frame));
      if (a->kind == OPK_VREG) {
        snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%%0, %s\n", suffix, frame);
        uses[0] = a->u.vreg;
        em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, tmpl);
      } else {
        i386_literal(a, literal, sizeof(literal));
        snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%s, %s\n", suffix, literal,
                 frame);
        em->list = add_inst(em->list, tmpl);
      }
    } else {
      assert(dst->kind == OPK_MEM_BD && a->kind == OPK_VREG);
      snprintf(tmpl, sizeof(tmpl), "\tmov%c\t%%0, %d(%%1)\n", suffix,
               dst->u.mem_bd.disp);
      uses[0] = a->u.vreg;
      uses[1] = dst->u.mem_bd.base;
      int widths[] = {i386_width_sel(width), 2};
      em->list = be_add_inst_du_wsel(em->list, vregp(em), NULL, 0, uses, 2,
                                     tmpl, widths);
    }
    break;

  case BE_ADD:
  case BE_SUB:
  case BE_MUL:
  case BE_AND:
  case BE_OR:
  case BE_XOR: {
    const char *mnemonic =
        op == BE_ADD ? "add" : op == BE_SUB ? "sub" : op == BE_MUL ? "imul"
                      : op == BE_AND ? "and" : op == BE_OR ? "or" : "xor";
    assert(dst->kind == OPK_VREG);
    if (!(a->kind == OPK_VREG && a->u.vreg == dst->u.vreg))
      i386_emit(em, BE_MOV, width, dst, a, NULL);
    defs[0] = dst->u.vreg;
    uses[0] = dst->u.vreg;
    if (b->kind == OPK_VREG) {
      uses[1] = b->u.vreg;
      snprintf(tmpl, sizeof(tmpl), "\t%s%c\t%%2, %%0\n", mnemonic, suffix);
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 2, tmpl);
    } else {
      i386_literal(b, literal, sizeof(literal));
      snprintf(tmpl, sizeof(tmpl), "\t%s%c\t%s, %%0\n", mnemonic, suffix,
               literal);
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1, tmpl);
    }
    break;
  }

  case BE_NEG:
    assert(dst->kind == OPK_VREG);
    if (!(a->kind == OPK_VREG && a->u.vreg == dst->u.vreg))
      i386_emit(em, BE_MOV, width, dst, a, NULL);
    defs[0] = dst->u.vreg;
    uses[0] = dst->u.vreg;
    snprintf(tmpl, sizeof(tmpl), "\tneg%c\t%%0\n", suffix);
    em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1, tmpl);
    break;

  case BE_SHL:
  case BE_SHR:
  case BE_SAR: {
    const char *mnemonic = op == BE_SHL ? "shl" : op == BE_SHR ? "shr" : "sar";
    assert(dst->kind == OPK_VREG && b->kind == OPK_IMM);
    if (!(a->kind == OPK_VREG && a->u.vreg == dst->u.vreg))
      i386_emit(em, BE_MOV, width, dst, a, NULL);
    defs[0] = dst->u.vreg;
    uses[0] = dst->u.vreg;
    snprintf(tmpl, sizeof(tmpl), "\t%s%c\t$%lld, %%0\n", mnemonic, suffix,
             b->u.imm);
    em->list = be_add_inst_du(em->list, vregp(em), defs, 1, uses, 1, tmpl);
    break;
  }

  case BE_CMP:
    assert(a->kind == OPK_VREG);
    uses[0] = a->u.vreg;
    if (b->kind == OPK_VREG) {
      uses[1] = b->u.vreg;
      snprintf(tmpl, sizeof(tmpl), "\tcmp%c\t%%1, %%0\n", suffix);
      em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 2, tmpl);
    } else {
      i386_literal(b, literal, sizeof(literal));
      snprintf(tmpl, sizeof(tmpl), "\tcmp%c\t%s, %%0\n", suffix, literal);
      em->list = be_add_inst_du(em->list, vregp(em), NULL, 0, uses, 1, tmpl);
    }
    break;

  case BE_LEA:
    assert(dst->kind == OPK_VREG);
    defs[0] = dst->u.vreg;
    if (a->kind == OPK_MEM_FRAME) {
      i386_frame(a, frame, sizeof(frame));
      snprintf(tmpl, sizeof(tmpl), "\tleal\t%s, %%0\n", frame);
      em->list = be_add_inst_du(em->list, vregp(em), defs, 1, NULL, 0, tmpl);
    } else {
      assert(a->kind == OPK_MEM_BD);
      snprintf(tmpl, sizeof(tmpl), "\tleal\t%d(%%1), %%0\n", a->u.mem_bd.disp);
      uses[0] = a->u.mem_bd.base;
      int widths[] = {2, 2};
      em->list = be_add_inst_du_wsel(em->list, vregp(em), defs, 1, uses, 1,
                                     tmpl, widths);
    }
    break;

  default:
    assert(0 && "unsupported i386 backend operation");
  }
}

static const char *i386_cc(BeCond cc) {
  static const char *const names[] = {"je",  "jne", "jl",  "jle", "jg",
                                      "jge", "jb",  "jbe", "ja",  "jae"};
  return cc == BE_ALWAYS ? "jmp" : names[cc];
}

static void i386_emit_setcc(BeEmitter *em, BeCond cc, const BeOperand *dst) {
  assert(dst->kind == OPK_VREG);
  char tmpl[32];
  snprintf(tmpl, sizeof(tmpl), "\tset%s\t%%al\n", i386_cc(cc) + 1);
  em->list = add_inst(em->list, tmpl);
  Register_t *defs[] = {dst->u.vreg};
  int widths[] = {2};
  em->list = be_add_inst_du_w(em->list, vregp(em), defs, 1, NULL, 0,
                               "\tmovzbl\t%al, %0\n", widths);
}

static void i386_emit_ext(BeEmitter *em, const BeOperand *dst,
                          const BeOperand *src, BeWidth from, BeWidth to,
                          int is_signed) {
  assert(dst->kind == OPK_VREG && src->kind == OPK_VREG && to == BE_W32);
  const char *mnemonic = from == BE_W8 ? (is_signed ? "movsbl" : "movzbl")
                         : from == BE_W16 ? (is_signed ? "movswl" : "movzwl")
                                           : "movl";
  Register_t *defs[] = {dst->u.vreg};
  Register_t *uses[] = {src->u.vreg};
  int widths[] = {2, i386_width_sel(from)};
  char tmpl[32];
  snprintf(tmpl, sizeof(tmpl), "\t%s\t%%1, %%0\n", mnemonic);
  em->list = be_add_inst_du_wsel(em->list, vregp(em), defs, 1, uses, 1, tmpl,
                                 widths);
}

static void i386_emit_branch(BeEmitter *em, BeCond cc, const char *label) {
  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", i386_cc(cc), label);
  em->list = add_inst(em->list, buffer);
}

static void i386_emit_call(BeEmitter *em, const char *symbol,
                           Register_t *indirect) {
  if (symbol != NULL) {
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", symbol);
    em->list = add_inst(em->list, buffer);
  } else {
    Register_t *uses[] = {indirect};
    int widths[] = {2};
    em->list = be_add_inst_du_w(em->list, vregp(em), NULL, 0, uses, 1,
                                 "\tcall\t*%0\n", widths);
  }
}

static void i386_emit_ret(BeEmitter *em) { em->list = add_inst(em->list, "\tret\n"); }
static void i386_emit_label(BeEmitter *em, const char *label) {
  char buffer[128];
  snprintf(buffer, sizeof(buffer), "%s:\n", label);
  em->list = add_inst(em->list, buffer);
}

static void i386_emit_prologue(BeEmitter *em, const BeFrame *frame) {
  char buffer[192];
  snprintf(buffer, sizeof(buffer),
           "%s:\n\tpushl\t%%ebp\n\tmovl\t%%esp, %%ebp\n\tpushl\t%%ebx\n"
           "\tpushl\t%%esi\n\tpushl\t%%edi\n",
           frame->name);
  em->list = add_inst(em->list, buffer);
  if (frame->frame_size > 0) {
    snprintf(buffer, sizeof(buffer), "\tsubl\t$%d, %%esp\n", frame->frame_size);
    em->list = add_inst(em->list, buffer);
  }
}

static void i386_emit_epilogue(BeEmitter *em, const BeFrame *frame) {
  (void)frame;
  em->list = add_inst(em->list,
                      "\tleal\t-12(%ebp), %esp\n\tpopl\t%edi\n\tpopl\t%esi\n"
                      "\tpopl\t%ebx\n\tpopl\t%ebp\n\tret\n");
}

static const char *i386_arg_reg(int index, BeWidth width) {
  (void)index;
  (void)width;
  return NULL;
}
static int i386_num_int_arg_regs(void) { return 0; }
static const char *i386_return_reg(BeWidth width) {
  return width == BE_WF32 || width == BE_WF64 ? NULL : "%eax";
}

static const BackendRegSpec kI386Pool[] = {
    {REG_RBX, "%ebx", "%ebx", "%bx", "%bl"},
    {REG_RSI, "%esi", "%esi", "%si", "%sil"},
    {REG_RDI, "%edi", "%edi", "%di", "%dil"},
};
static const BackendRegSpec *i386_regpool(int *count) {
  *count = (int)(sizeof(kI386Pool) / sizeof(kI386Pool[0]));
  return kI386Pool;
}

static void i386_emit_section(BeEmitter *em, BeSection section) {
  em->list = add_inst(em->list, section == BE_SEC_TEXT ? "\t.text\n"
                                   : section == BE_SEC_DATA ? "\t.data\n"
                                                            : "\t.section\t.rodata\n");
}
static void i386_emit_global(BeEmitter *em, const char *symbol) {
  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\t.globl\t%s\n", symbol);
  em->list = add_inst(em->list, buffer);
}
static void i386_emit_data_label(BeEmitter *em, const char *label) {
  i386_emit_label(em, label);
}
static void i386_emit_data(BeEmitter *em, BeDataKind kind, long long value) {
  const char *directive = kind == BE_D8 ? ".byte" : kind == BE_D16 ? ".short"
                          : kind == BE_D32 ? ".long" : ".quad";
  char buffer[64];
  snprintf(buffer, sizeof(buffer), "\t%s\t%lld\n", directive, value);
  em->list = add_inst(em->list, buffer);
}
static void i386_emit_string(BeEmitter *em, const char *string) {
  char buffer[320];
  snprintf(buffer, sizeof(buffer), "\t.string\t\"%s\"\n", string);
  em->list = add_inst(em->list, buffer);
}
static void i386_emit_zero(BeEmitter *em, int nbytes) {
  char buffer[32];
  snprintf(buffer, sizeof(buffer), "\t.zero\t%d\n", nbytes);
  em->list = add_inst(em->list, buffer);
}
static void i386_emit_align(BeEmitter *em, int nbytes) {
  char buffer[32];
  snprintf(buffer, sizeof(buffer), "\t.align\t%d\n", nbytes);
  em->list = add_inst(em->list, buffer);
}

static const Target kI386SysV = {
    .name = "i386-sysv",
    .ptr_width = 4,
    .emit = i386_emit,
    .emit_setcc = i386_emit_setcc,
    .emit_ext = i386_emit_ext,
    .emit_branch = i386_emit_branch,
    .emit_call = i386_emit_call,
    .emit_ret = i386_emit_ret,
    .emit_label = i386_emit_label,
    .emit_prologue = i386_emit_prologue,
    .emit_epilogue = i386_emit_epilogue,
    .emit_section = i386_emit_section,
    .emit_global = i386_emit_global,
    .emit_data_label = i386_emit_data_label,
    .emit_data = i386_emit_data,
    .emit_string = i386_emit_string,
    .emit_zero = i386_emit_zero,
    .emit_align = i386_emit_align,
    .arg_reg = i386_arg_reg,
    .num_int_arg_regs = i386_num_int_arg_regs,
    .return_reg = i386_return_reg,
    .regpool = i386_regpool,
    .fregpool = NULL,
};

const Target *target_i386_sysv(void) { return &kI386SysV; }
