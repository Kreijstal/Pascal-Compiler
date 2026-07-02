/*
 * target.h — target-neutral code-generation backend interface.
 *
 * A `Target` is a vtable (struct of function pointers) that emits machine
 * instructions from an abstract op/operand vocabulary.  Swapping the target
 * (x86-64, AArch64, …) means selecting a different `const Target *` — the
 * caller speaks only the neutral vocabulary below, never raw assembly.
 *
 * Design rule: this header must stay ISA-neutral.  It is co-designed against
 * x86-64 (target_x86.c) and AArch64 (target_aarch64.c) so no x86-ism leaks in.
 * Anything x86-specific (AT&T syntax, relop→mnemonic mapping, register names)
 * lives inside a target's .c file, never here.
 */
#ifndef KGPC_BACKEND_TARGET_H
#define KGPC_BACKEND_TARGET_H

#include "backend_emit.h" /* BackendCtx, be_add_inst_du, ListNode_t, Register_t */

/* Full definition in stackmng.h; targets return a static table of these to
 * describe their allocatable register pool. */
struct BackendRegSpec;

/* ---- Operand widths (value = size in bytes for the integer widths) ------- */
typedef enum {
  BE_W8 = 1,
  BE_W16 = 2,
  BE_W32 = 4,
  BE_W64 = 8,
  BE_WF32 = 104, /* IEEE single  (distinct from the integer byte counts) */
  BE_WF64 = 108  /* IEEE double */
} BeWidth;

/* ---- Minimal, extensible op set ----------------------------------------- */
typedef enum {
  BE_MOV,   /* dst := src            (reg/imm/mem/phys) */
  BE_LOAD,  /* dst := [mem]          */
  BE_STORE, /* [mem] := src          */
  BE_ADD,   /* dst := a + b          */
  BE_SUB,   /* dst := a - b          */
  BE_MUL,   /* dst := a * b          (signed) */
  BE_AND,   /* dst := a & b          */
  BE_OR,    /* dst := a | b          */
  BE_XOR,   /* dst := a ^ b          */
  BE_NEG,   /* dst := -dst           (unary) */
  BE_SHL,   /* dst := dst << b       (b immediate) */
  BE_SHR,   /* dst := dst >> b       (logical, b immediate) */
  BE_SAR,   /* dst := dst >> b       (arithmetic, b immediate) */
  BE_CMP,   /* set condition from a ? b */
  BE_LEA    /* dst := &mem           */
} BeOp;

/* ---- Condition codes (mapped to mnemonics inside each target) ----------- */
typedef enum {
  BE_EQ,
  BE_NE,
  BE_LT,
  BE_LE,
  BE_GT,
  BE_GE,
  BE_ULT,
  BE_ULE,
  BE_UGT,
  BE_UGE,
  BE_ALWAYS /* unconditional */
} BeCond;

/* ---- Operands ----------------------------------------------------------- */
typedef enum {
  OPK_VREG,    /* virtual register (Register_t* from get_free_reg) */
  OPK_PHYS,    /* named physical register (arg/return regs: "%edi", "x0") */
  OPK_IMM,     /* immediate integer */
  OPK_MEM_BD,  /* base + displacement:      disp(base) */
  OPK_MEM_BIS, /* base + index*scale + disp: disp(base,index,scale) */
  OPK_RIP_SYM, /* pc-relative symbol:       sym(%rip) / sym */
  OPK_LABEL    /* branch/call target label */
} BeOperandKind;

typedef struct BeOperand {
  BeOperandKind kind;
  BeWidth width;
  union {
    Register_t *vreg;   /* OPK_VREG */
    const char *phys;   /* OPK_PHYS */
    long long imm;      /* OPK_IMM  */
    struct {
      Register_t *base;
      int disp;
    } mem_bd; /* OPK_MEM_BD */
    struct {
      Register_t *base;
      Register_t *index;
      int scale;
      int disp;
    } mem_bis; /* OPK_MEM_BIS */
    const char *sym;   /* OPK_RIP_SYM */
    const char *label; /* OPK_LABEL */
  } u;
} BeOperand;

/* ---- Frame description for prologue/epilogue ---------------------------- */
typedef struct BeFrame {
  const char *name; /* function symbol */
  int frame_size;   /* locals bytes to reserve (aligned by the target) */
  int is_leaf;      /* no calls → may skip frame setup where legal */
} BeFrame;

/* ---- Emitter: the growing instruction list + borrowed counters.
 * The counters are held by pointer so the same emitter type binds to either a
 * standalone BackendCtx (tests) or the live compiler's CodeGenContext fields
 * (integration).  Either pointer may be NULL. */
typedef struct BeEmitter {
  ListNode_t *list;
  int *next_vreg_id;
  int *label_counter;
} BeEmitter;

/* Bind an emitter to a standalone BackendCtx (test/harness convenience). */
static inline BeEmitter be_emitter_from_backendctx(ListNode_t *list,
                                                   BackendCtx *cx) {
  BeEmitter em;
  em.list = list;
  em.next_vreg_id = cx ? &cx->next_vreg_id : (int *)0;
  em.label_counter = cx ? &cx->label_counter : (int *)0;
  return em;
}

/* ---- The target vtable -------------------------------------------------- */
typedef struct Target {
  const char *name;    /* e.g. "x86_64-sysv", "aarch64" */
  int ptr_width;       /* pointer size in bytes (8) */

  /* Core instruction emission.  Appends to em->list.  `b` may be NULL for
   * unary/two-operand forms (MOV/LOAD/STORE/LEA/CMP dst,a). */
  void (*emit)(BeEmitter *em, BeOp op, BeWidth w, const BeOperand *dst,
               const BeOperand *a, const BeOperand *b);

  /* Control flow. */
  void (*emit_branch)(BeEmitter *em, BeCond cc, const char *label);
  void (*emit_call)(BeEmitter *em, const char *sym, Register_t *indirect);
  void (*emit_ret)(BeEmitter *em);
  void (*emit_label)(BeEmitter *em, const char *label);

  /* Structural: emit prologue/epilogue as LIST_STRING so a standalone
   * function is fully self-assembleable (no front-end frame logic). */
  void (*emit_prologue)(BeEmitter *em, const BeFrame *f);
  void (*emit_epilogue)(BeEmitter *em, const BeFrame *f);

  /* ABI queries (thin wrappers over the target's register file). */
  const char *(*arg_reg)(int idx, BeWidth w);
  int (*num_int_arg_regs)(void);
  const char *(*return_reg)(BeWidth w);

  /* The target's allocatable register pool.  Returns a static table and sets
   * *n to its length.  Feed to stackmng_set_register_pool() so the shared
   * allocator colors into this target's registers. */
  const struct BackendRegSpec *(*regpool)(int *n);
} Target;

/* Target factories (defined in target_x86.c / target_aarch64.c). */
const Target *target_x86_sysv(void);
const Target *target_aarch64(void);

/* The instruction-emitting target the live compiler lowers through.  x86 today
 * (SysV/Windows differ only in ABI register selection at the call site, not in
 * the instructions the vtable emits). */
const Target *kgpc_backend_target(void);

#endif /* KGPC_BACKEND_TARGET_H */
