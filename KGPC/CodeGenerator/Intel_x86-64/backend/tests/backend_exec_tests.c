/*
 * backend_exec_tests.c — standalone end-to-end tests for the backend library.
 *
 * Builds functions through the target-neutral Target vtable, runs the REAL
 * register allocator + emitter (ir_liveness_allocate → ir_emit_function →
 * ir_peephole), writes an assembly file, then shells out to $CC to assemble,
 * link a tiny C driver, run it, and assert the return value.  No dependency on
 * the Pascal front-end.
 *
 * Test tiers:
 *   1. golden-asm         — structural checks on the emitted text (no gcc)
 *   2. assemble-link-run  — the real proof: compiled function returns correctly
 */
#include "../target.h"

#include "../../register_types.h" /* g_current_codegen_abi, ABI enum */
#include "../../stackmng/stackmng.h"
#include "../../ir/ir_inst.h"
#include "../../ir/ir_peephole.h"
#include "../regalloc_driver.h"

#include "../../../../Parser/List/List.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !USE_GRAPH_COLORING_ALLOCATOR
#error "backend_exec_tests requires USE_GRAPH_COLORING_ALLOCATOR=1"
#endif

static int g_failures = 0;
static int g_tests = 0;
static int g_exec_skips = 0;

#define CHECK(cond, msg)                                                        \
  do {                                                                          \
    ++g_tests;                                                                  \
    if (!(cond)) {                                                              \
      ++g_failures;                                                             \
      fprintf(stderr, "FAIL: %s (%s:%d)\n", (msg), __FILE__, __LINE__);        \
    } else {                                                                    \
      fprintf(stderr, "ok:   %s\n", (msg));                                    \
    }                                                                          \
  } while (0)

/* The assemble-link-run tier executes the emitted x86_64 System V assembly
 * natively, so it needs a host whose C ABI is x86-64 SysV.  Windows hosts —
 * including MSYS/Cygwin, whose runtime is POSIX but whose calling convention
 * is MS x64 — can only run the golden-asm tier (the same constraint that
 * skips the AArch64 exec tier).  The POSIX wait-status decoding in
 * assemble_link_run is likewise only valid where this is set. */
#if defined(__x86_64__) && !defined(_WIN32) && !defined(__CYGWIN__)
#define HOST_CAN_EXEC_SYSV 1
#else
#define HOST_CAN_EXEC_SYSV 0
#endif

/* Sentinel returned by assemble_link_run when the host cannot execute the
 * emitted code; distinct from -1 (compile/link/run failure). */
#define EXEC_SKIP (-2)

#define CHECK_EXEC(rc, msg)                                                     \
  do {                                                                          \
    if ((rc) == EXEC_SKIP) {                                                    \
      ++g_exec_skips;                                                           \
      fprintf(stderr, "skip: %s (host C ABI is not x86-64 SysV)\n", (msg));    \
    } else {                                                                    \
      CHECK((rc) == 0, msg);                                                    \
    }                                                                           \
  } while (0)

/* Point the shared allocator's register pool at this target's registers.
 * This is the register-file generalization in action: the same allocator
 * colors into x86 or AArch64 registers depending only on the target. */
static void select_target_pool(const Target *T) {
  int n = 0;
  const BackendRegSpec *specs = T->regpool(&n);
  stackmng_set_register_pool(specs, n);
}

/* Point the shared allocator at this target's FLOAT register pool, so float
 * vregs color into xmm/d registers (float-only functions — see fregpool's
 * mixed-class limitation note in target.h). */
static void select_target_fpool(const Target *T) {
  int n = 0;
  const BackendRegSpec *specs = T->fregpool(&n);
  stackmng_set_register_pool(specs, n);
}

/* Run a completed instruction list through allocation + emission and serialize
 * it to `path` as an assembleable .globl'd function. */
static void finalize_and_write(const char *path, const char *sym,
                               ListNode_t *list) {
  ir_liveness_allocate(list);
  ir_emit_function(list);
  ir_peephole_remove_redundant_moves(&list);

  FILE *fp = fopen(path, "w");
  if (fp == NULL) {
    fprintf(stderr, "FAIL: cannot open %s\n", path);
    ++g_failures;
    return;
  }
  fprintf(fp, "\t.text\n\t.globl\t%s\n", sym);
  be_inst_list_write(fp, list);
  fclose(fp);
}

/* Assemble asm_path + a generated driver, link, run.  Returns the run's exit
 * code, or -1 on a compile/link failure. */
static int assemble_link_run(const char *tag, const char *asm_path,
                             const char *driver_src) {
#if !HOST_CAN_EXEC_SYSV
  (void)tag;
  (void)asm_path;
  (void)driver_src;
  return EXEC_SKIP;
#else
  char driver_path[256], exe_path[256], cmd[1024];
  snprintf(driver_path, sizeof(driver_path), "be_%s_driver.c", tag);
  snprintf(exe_path, sizeof(exe_path), "./be_%s_exe", tag);

  FILE *df = fopen(driver_path, "w");
  if (df == NULL)
    return -1;
  fputs(driver_src, df);
  fclose(df);

  const char *cc = getenv("CC");
  if (cc == NULL || cc[0] == '\0')
    cc = "cc";

  snprintf(cmd, sizeof(cmd), "%s -no-pie -o %s %s %s > %s.buildlog 2>&1", cc,
           exe_path, driver_path, asm_path, exe_path);
  int rc = system(cmd);
  if (rc != 0) {
    fprintf(stderr, "  (compile/link failed for %s; see %s.buildlog)\n", tag,
            exe_path);
    return -1;
  }
  int run = system(exe_path);
  /* WEXITSTATUS-style: system() returns the wait status; extract exit code. */
  if (run == -1)
    return -1;
  return (run >> 8) & 0xff;
#endif
}

/* Build `int f(int a, int b)` computing `a OP b` and return the finished list. */
static ListNode_t *build_binop(const Target *T, const char *sym, BeOp op) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 0, 1};
  T->emit_prologue(&em, &f);

  Register_t *va = get_free_reg(rs, &em.list);
  Register_t *vb = get_free_reg(rs, &em.list);

  BeOperand dva = {OPK_VREG, BE_W32, {.vreg = va}};
  BeOperand dvb = {OPK_VREG, BE_W32, {.vreg = vb}};
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand arg1 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(1, BE_W32)}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};

  T->emit(&em, BE_MOV, BE_W32, &dva, &arg0, NULL); /* va = a */
  T->emit(&em, BE_MOV, BE_W32, &dvb, &arg1, NULL); /* vb = b */
  T->emit(&em, op, BE_W32, &dva, &dva, &dvb);      /* va = va OP vb */
  T->emit(&em, BE_MOV, BE_W32, &ret, &dva, NULL);  /* return va */
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Build `int f(void)` returning an immediate constant. */
static ListNode_t *build_const(const Target *T, const char *sym, int value) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();

  BeFrame f = {sym, 0, 1};
  T->emit_prologue(&em, &f);
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  BeOperand imm = {OPK_IMM, BE_W32, {.imm = value}};
  T->emit(&em, BE_MOV, BE_W32, &ret, &imm, NULL);
  T->emit_epilogue(&em, &f);
  return em.list;
}

static void test_golden_add(const Target *T) {
  ListNode_t *list = build_binop(T, "gadd", BE_ADD);
  ir_liveness_allocate(list);
  ir_emit_function(list);
  /* Concatenate emitted text and check structure. */
  char all[4096];
  size_t used = 0;
  all[0] = '\0';
  for (ListNode_t *n = list; n != NULL; n = n->next) {
    const char *t = (n->type == LIST_IR_INST) ? ((IrInst_t *)n->cur)->text
                                              : (const char *)n->cur;
    if (t == NULL)
      continue;
    size_t l = strlen(t);
    if (used + l < sizeof(all)) {
      memcpy(all + used, t, l);
      used += l;
      all[used] = '\0';
    }
  }
  CHECK(strstr(all, "gadd:") != NULL, "golden: has function label");
  CHECK(strstr(all, "pushq\t%rbp") != NULL, "golden: has prologue");
  CHECK(strstr(all, "addl") != NULL, "golden: has addl");
  CHECK(strstr(all, "%eax") != NULL, "golden: writes return reg");
  CHECK(strstr(all, "ret") != NULL, "golden: has ret");
}

/* Build `int f(int a)` computing `a <shift-op> n` (n immediate). */
static ListNode_t *build_shift(const Target *T, const char *sym, BeOp op,
                               int n) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 0, 1};
  T->emit_prologue(&em, &f);
  Register_t *va = get_free_reg(rs, &em.list);
  BeOperand dva = {OPK_VREG, BE_W32, {.vreg = va}};
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  BeOperand imm = {OPK_IMM, BE_W32, {.imm = n}};
  T->emit(&em, BE_MOV, BE_W32, &dva, &arg0, NULL);
  T->emit(&em, op, BE_W32, &dva, &dva, &imm);
  T->emit(&em, BE_MOV, BE_W32, &ret, &dva, NULL);
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Build `int f(int a)` computing `-a`. */
static ListNode_t *build_neg(const Target *T, const char *sym) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 0, 1};
  T->emit_prologue(&em, &f);
  Register_t *va = get_free_reg(rs, &em.list);
  BeOperand dva = {OPK_VREG, BE_W32, {.vreg = va}};
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  T->emit(&em, BE_MOV, BE_W32, &dva, &arg0, NULL);
  T->emit(&em, BE_NEG, BE_W32, &dva, &dva, NULL);
  T->emit(&em, BE_MOV, BE_W32, &ret, &dva, NULL);
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Build `int f(int a, int b)` computing `a <cc> b` as 0/1 (CMP + SETcc). */
static ListNode_t *build_cmp(const Target *T, const char *sym, BeCond cc) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 0, 1};
  T->emit_prologue(&em, &f);
  Register_t *va = get_free_reg(rs, &em.list);
  Register_t *vb = get_free_reg(rs, &em.list);
  BeOperand dva = {OPK_VREG, BE_W32, {.vreg = va}};
  BeOperand dvb = {OPK_VREG, BE_W32, {.vreg = vb}};
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand arg1 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(1, BE_W32)}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  T->emit(&em, BE_MOV, BE_W32, &dva, &arg0, NULL);
  T->emit(&em, BE_MOV, BE_W32, &dvb, &arg1, NULL);
  T->emit(&em, BE_CMP, BE_W32, NULL, &dva, &dvb);
  T->emit_setcc(&em, cc, &dva);
  T->emit(&em, BE_MOV, BE_W32, &ret, &dva, NULL);
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Build `f(int a)` computing `(<narrow>)a`: load the arg, extend its low
 * `from` bits (signed or unsigned) into a `to`-wide result, return it.
 * Return type is int when to==BE_W32, long when to==BE_W64. */
static ListNode_t *build_ext(const Target *T, const char *sym, BeWidth from,
                             BeWidth to, int is_signed) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 0, 1};
  T->emit_prologue(&em, &f);

  Register_t *va = get_free_reg(rs, &em.list);
  Register_t *vr = get_free_reg(rs, &em.list);
  BeOperand dva = {OPK_VREG, BE_W32, {.vreg = va}};
  BeOperand dvr = {OPK_VREG, to, {.vreg = vr}};
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand ret = {OPK_PHYS, to, {.phys = T->return_reg(to)}};

  /* va = a (low 32 bits carry the byte/half we care about) */
  T->emit(&em, BE_MOV, BE_W32, &dva, &arg0, NULL);
  /* vr = extend(va) */
  T->emit_ext(&em, &dvr, &dva, from, to, is_signed);
  /* return vr */
  T->emit(&em, BE_MOV, to, &ret, &dvr, NULL);
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Build `double f(double a, double b)` computing `a OP b` (float-only fn). */
static ListNode_t *build_fbinop(const Target *T, const char *sym, BeOp op) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_fpool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 0, 1};
  T->emit_prologue(&em, &f);

  Register_t *va = get_free_reg(rs, &em.list);
  Register_t *vb = get_free_reg(rs, &em.list);
  BeOperand dva = {OPK_VREG, BE_WF64, {.vreg = va}};
  BeOperand dvb = {OPK_VREG, BE_WF64, {.vreg = vb}};
  BeOperand arg0 = {OPK_PHYS, BE_WF64, {.phys = T->arg_reg(0, BE_WF64)}};
  BeOperand arg1 = {OPK_PHYS, BE_WF64, {.phys = T->arg_reg(1, BE_WF64)}};
  BeOperand ret = {OPK_PHYS, BE_WF64, {.phys = T->return_reg(BE_WF64)}};

  T->emit(&em, BE_MOV, BE_WF64, &dva, &arg0, NULL); /* va = a */
  T->emit(&em, BE_MOV, BE_WF64, &dvb, &arg1, NULL); /* vb = b */
  T->emit(&em, op, BE_WF64, &dva, &dva, &dvb);      /* va = va OP vb */
  T->emit(&em, BE_MOV, BE_WF64, &ret, &dva, NULL);  /* return va */
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Build `int f(double a)` computing `(int)a` (truncating double→int32). */
static ListNode_t *build_f2i(const Target *T, const char *sym) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_fpool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 0, 1};
  T->emit_prologue(&em, &f);
  Register_t *va = get_free_reg(rs, &em.list);
  BeOperand dva = {OPK_VREG, BE_WF64, {.vreg = va}};
  BeOperand arg0 = {OPK_PHYS, BE_WF64, {.phys = T->arg_reg(0, BE_WF64)}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  T->emit(&em, BE_MOV, BE_WF64, &dva, &arg0, NULL);   /* va = a */
  T->emit(&em, BE_CVT_F2I, BE_W32, &ret, &dva, NULL); /* return (int)va */
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Build `double g(int a)` computing `(double)a` (int32→double). */
static ListNode_t *build_i2f(const Target *T, const char *sym) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_fpool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 0, 1};
  T->emit_prologue(&em, &f);
  Register_t *va = get_free_reg(rs, &em.list);
  BeOperand dva = {OPK_VREG, BE_WF64, {.vreg = va}};
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand ret = {OPK_PHYS, BE_WF64, {.phys = T->return_reg(BE_WF64)}};
  T->emit(&em, BE_CVT_I2F, BE_WF64, &dva, &arg0, NULL); /* va = (double)a */
  T->emit(&em, BE_MOV, BE_WF64, &ret, &dva, NULL);      /* return va */
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Concatenate the emitted text of a finished (allocated+emitted) list. */
static void concat_emitted(ListNode_t *list, char *out, size_t cap) {
  size_t used = 0;
  out[0] = '\0';
  for (ListNode_t *n = list; n != NULL; n = n->next) {
    const char *t = (n->type == LIST_IR_INST) ? ((IrInst_t *)n->cur)->text
                                              : (const char *)n->cur;
    if (t == NULL)
      continue;
    size_t l = strlen(t);
    if (used + l < cap) {
      memcpy(out + used, t, l);
      used += l;
      out[used] = '\0';
    }
  }
}

/* M4 neutrality proof: the SAME harness + SAME shared allocator, driven through
 * the AArch64 target, must emit valid AArch64 assembly with the allocator
 * coloring into AArch64 registers (x19..) — and no x86 register leakage. */
static void test_golden_aarch64(const Target *T) {
  ListNode_t *list = build_binop(T, "aaadd", BE_ADD);
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));

  CHECK(strstr(all, "aaadd:") != NULL, "aarch64: has function label");
  CHECK(strstr(all, "stp\tx29, x30") != NULL, "aarch64: AAPCS64 prologue");
  CHECK(strstr(all, "add\tw19") != NULL, "aarch64: 32-bit add into w19");
  CHECK(strstr(all, "w19") != NULL, "aarch64: allocator colored into w19");
  CHECK(strstr(all, "w0") != NULL, "aarch64: uses w0 arg/return (32-bit)");
  CHECK(strstr(all, "ret") != NULL, "aarch64: has ret");
  /* Neutrality: no x86 registers must appear in AArch64 output. */
  CHECK(strstr(all, "%rbx") == NULL && strstr(all, "%rax") == NULL &&
            strstr(all, "%rbp") == NULL,
        "aarch64: no x86 register leakage");
}

/* Golden check for the extend ops on AArch64: signed byte → sxtb (32-bit
 * dest w-reg), unsigned byte → uxtb, signed byte → 64-bit → sxtb into x-reg. */
static void test_golden_aarch64_ext(const Target *T) {
  /* signed byte → W32 */
  ListNode_t *l1 = build_ext(T, "aasxtb", BE_W8, BE_W32, 1);
  ir_liveness_allocate(l1);
  ir_emit_function(l1);
  char a1[4096];
  concat_emitted(l1, a1, sizeof(a1));
  CHECK(strstr(a1, "sxtb\tw") != NULL,
        "aarch64: signed byte ext = sxtb w<d>, w<s>");

  /* unsigned byte → W32 */
  ListNode_t *l2 = build_ext(T, "aauxtb", BE_W8, BE_W32, 0);
  ir_liveness_allocate(l2);
  ir_emit_function(l2);
  char a2[4096];
  concat_emitted(l2, a2, sizeof(a2));
  CHECK(strstr(a2, "uxtb\tw") != NULL,
        "aarch64: unsigned byte ext = uxtb w<d>, w<s>");

  /* signed byte → W64: dest is an x-register, source stays w. */
  ListNode_t *l3 = build_ext(T, "aasxtbq", BE_W8, BE_W64, 1);
  ir_liveness_allocate(l3);
  ir_emit_function(l3);
  char a3[4096];
  concat_emitted(l3, a3, sizeof(a3));
  CHECK(strstr(a3, "sxtb\tx") != NULL,
        "aarch64: byte->64 signed ext = sxtb x<d>, w<s>");
}

static void test_exec_binop(const Target *T, const char *sym, BeOp op, int a,
                            int b, int expected) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_binop(T, sym, op);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(int,int);\nint main(void){return %s(%d,%d)==%d?0:1;}\n",
           sym, sym, a, b, expected);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: %s(%d,%d)==%d", sym, a, b, expected);
  CHECK_EXEC(rc, msg);
}

static void test_exec_shift(const Target *T, const char *sym, BeOp op, int a,
                            int n, int expected) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_shift(T, sym, op, n);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(int);\nint main(void){return %s(%d)==%d?0:1;}\n", sym,
           sym, a, expected);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: %s(%d)==%d", sym, a, expected);
  CHECK_EXEC(rc, msg);
}

static void test_exec_neg(const Target *T, const char *sym, int a,
                          int expected) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_neg(T, sym);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(int);\nint main(void){return %s(%d)==%d?0:1;}\n", sym,
           sym, a, expected);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: %s(%d)==%d", sym, a, expected);
  CHECK_EXEC(rc, msg);
}

static void test_exec_cmp(const Target *T, const char *sym, BeCond cc, int a,
                          int b, int expected) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_cmp(T, sym, cc);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(int,int);\nint main(void){return %s(%d,%d)==%d?0:1;}\n",
           sym, sym, a, b, expected);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: %s(%d,%d)==%d", sym, a, b, expected);
  CHECK_EXEC(rc, msg);
}

/* Exec an extend into a 32-bit result (int f(int)). */
static void test_exec_ext(const Target *T, const char *sym, BeWidth from,
                          int is_signed, int a, int expected) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_ext(T, sym, from, BE_W32, is_signed);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(int);\nint main(void){return %s(%d)==%d?0:1;}\n", sym,
           sym, a, expected);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: %s(%d)==%d", sym, a, expected);
  CHECK_EXEC(rc, msg);
}

/* Exec an extend into a 64-bit result (long f(int)). */
static void test_exec_ext64(const Target *T, const char *sym, BeWidth from,
                            int is_signed, int a, long long expected) {
  char spath[256], driver[512], msg[160];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_ext(T, sym, from, BE_W64, is_signed);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern long %s(int);\nint main(void){return %s(%d)==%lldL?0:1;}\n",
           sym, sym, a, expected);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: %s(%d)==%lld", sym, a, expected);
  CHECK_EXEC(rc, msg);
}

static void test_exec_const(const Target *T, const char *sym, int value) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_const(T, sym, value);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(void);\nint main(void){return %s()==%d?0:1;}\n", sym,
           sym, value);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: %s()==%d", sym, value);
  CHECK_EXEC(rc, msg);
}

/* Exec a double-in/double-out binary float op (compares by exact ==, so pass
 * exactly-representable values). */
static void test_exec_fbinop(const Target *T, const char *sym, BeOp op, double a,
                             double b, double expected) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_fbinop(T, sym, op);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern double %s(double,double);\n"
           "int main(void){return %s(%.17g,%.17g)==%.17g?0:1;}\n",
           sym, sym, a, b, expected);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: %s(%g,%g)==%g", sym, a, b, expected);
  CHECK_EXEC(rc, msg);
}

/* Exec `int f(double)` = (int)a. */
static void test_exec_f2i(const Target *T, const char *sym, double a,
                          int expected) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_f2i(T, sym);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(double);\nint main(void){return %s(%.17g)==%d?0:1;}\n",
           sym, sym, a, expected);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: %s(%g)==%d", sym, a, expected);
  CHECK_EXEC(rc, msg);
}

/* Exec `double g(int)` = (double)a. */
static void test_exec_i2f(const Target *T, const char *sym, int a,
                          double expected) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_i2f(T, sym);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern double %s(int);\nint main(void){return %s(%d)==%.17g?0:1;}\n",
           sym, sym, a, expected);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: %s(%d)==%g", sym, a, expected);
  CHECK_EXEC(rc, msg);
}

/* Golden check for the float path on AArch64: fadd on d-registers, fmov moves,
 * scvtf (int→float) and fcvtzs (float→int), colored into d8+ with no x86/GP
 * float leakage. */
static void test_golden_aarch64_float(const Target *T) {
  ListNode_t *l1 = build_fbinop(T, "aafadd", BE_ADD);
  ir_liveness_allocate(l1);
  ir_emit_function(l1);
  char a1[4096];
  concat_emitted(l1, a1, sizeof(a1));
  CHECK(strstr(a1, "fadd\td") != NULL, "aarch64: fadd on d-register");
  CHECK(strstr(a1, "fmov") != NULL, "aarch64: fmov for float moves");
  CHECK(strstr(a1, "d8") != NULL, "aarch64: allocator colored into d8");
  CHECK(strstr(a1, "d0") != NULL, "aarch64: uses d0 arg/return");

  ListNode_t *l2 = build_i2f(T, "aascvtf");
  ir_liveness_allocate(l2);
  ir_emit_function(l2);
  char a2[4096];
  concat_emitted(l2, a2, sizeof(a2));
  CHECK(strstr(a2, "scvtf\td") != NULL, "aarch64: int->float = scvtf");

  ListNode_t *l3 = build_f2i(T, "aafcvtzs");
  ir_liveness_allocate(l3);
  ir_emit_function(l3);
  char a3[4096];
  concat_emitted(l3, a3, sizeof(a3));
  CHECK(strstr(a3, "fcvtzs\tw") != NULL, "aarch64: float->int = fcvtzs w<d>");
}

/* Run allocation + emission and serialize the list verbatim (no injected
 * .text/.globl header) — the directive/data channel emits its own sections and
 * globals into the list, so the whole translation unit is self-describing. */
static void finalize_and_write_raw(const char *path, ListNode_t *list) {
  ir_liveness_allocate(list);
  ir_emit_function(list);
  ir_peephole_remove_redundant_moves(&list);

  FILE *fp = fopen(path, "w");
  if (fp == NULL) {
    fprintf(stderr, "FAIL: cannot open %s\n", path);
    ++g_failures;
    return;
  }
  be_inst_list_write(fp, list);
  fclose(fp);
}

/* Build a .rodata 64-bit constant labeled `lbl`, plus a `.text` function `sym`
 * that loads it RIP-relative and returns it (long f(void)). */
static ListNode_t *build_data_const(const Target *T, const char *sym,
                                    const char *lbl, long long value) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();

  /* .rodata:  lbl:  .quad value */
  T->emit_section(&em, BE_SEC_RODATA);
  T->emit_data_label(&em, lbl);
  T->emit_data(&em, BE_D64, value);

  /* .text:  .globl sym ; sym: <prologue> movq lbl(%rip),%rax <epilogue> */
  T->emit_section(&em, BE_SEC_TEXT);
  T->emit_global(&em, sym);
  BeFrame f = {sym, 0, 1};
  T->emit_prologue(&em, &f);
  BeOperand ret = {OPK_PHYS, BE_W64, {.phys = T->return_reg(BE_W64)}};
  BeOperand src = {OPK_RIP_SYM, BE_W64, {.sym = lbl}};
  T->emit(&em, BE_MOV, BE_W64, &ret, &src, NULL); /* movq lbl(%rip), %rax */
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Build a `.data` array of three .long words, plus a `.text` function that
 * returns the middle element (arr[1]) via a RIP-relative load at lbl+4. */
static ListNode_t *build_data_array(const Target *T, const char *sym,
                                    const char *lbl) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();

  T->emit_section(&em, BE_SEC_DATA);
  T->emit_data_label(&em, lbl);
  T->emit_data(&em, BE_D32, 10);
  T->emit_data(&em, BE_D32, 20);
  T->emit_data(&em, BE_D32, 30);

  T->emit_section(&em, BE_SEC_TEXT);
  T->emit_global(&em, sym);
  BeFrame f = {sym, 0, 1};
  T->emit_prologue(&em, &f);
  char elt[80];
  snprintf(elt, sizeof(elt), "%s+4", lbl); /* &arr[1] (rendered synchronously) */
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  BeOperand src = {OPK_RIP_SYM, BE_W32, {.sym = elt}};
  T->emit(&em, BE_MOV, BE_W32, &ret, &src, NULL); /* movl lbl+4(%rip), %eax */
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Exec: .rodata .quad constant loaded and returned. */
static void test_exec_data_const(const Target *T) {
  ListNode_t *list = build_data_const(T, "getk", ".LCk", 42);
  finalize_and_write_raw("be_getk.s", list);
  const char *drv =
      "extern long getk(void);\nint main(void){return getk()==42?0:1;}\n";
  int rc = assemble_link_run("getk", "be_getk.s", drv);
  CHECK_EXEC(rc, "exec: .rodata .quad 42 loaded RIP-relative returns 42");
}

/* Exec: .data three-.long array; function returns arr[1]==20. */
static void test_exec_data_array(const Target *T) {
  ListNode_t *list = build_data_array(T, "getarr1", ".Larr");
  finalize_and_write_raw("be_getarr1.s", list);
  const char *drv =
      "extern int getarr1(void);\nint main(void){return getarr1()==20?0:1;}\n";
  int rc = assemble_link_run("getarr1", "be_getarr1.s", drv);
  CHECK_EXEC(rc, "exec: .data .long array returns arr[1]==20");
}

/* Golden: x86 renders the neutral data channel as AT&T GAS. */
static void test_golden_x86_data(const Target *T) {
  ListNode_t *list = build_data_const(T, "getk", ".LCk", 42);
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, ".section\t.rodata") != NULL, "x86 data: .section .rodata");
  CHECK(strstr(all, ".LCk:") != NULL, "x86 data: data label");
  CHECK(strstr(all, ".quad\t42") != NULL, "x86 data: .quad 42");
  CHECK(strstr(all, ".globl\tgetk") != NULL, "x86 data: .globl getk");
  CHECK(strstr(all, ".LCk(%rip)") != NULL, "x86 data: RIP-relative load");
}

/* Golden: the SAME neutral data channel rendered as AArch64 GNU as.  Proves the
 * directive/data API is ISA-neutral — the target renders concrete syntax (e.g.
 * .xword for a 64-bit word), with sections/globals/labels shared with x86. */
static void test_golden_aarch64_data(const Target *T) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  em.list = NULL;

  T->emit_section(&em, BE_SEC_RODATA);
  T->emit_global(&em, "kdata");
  T->emit_data_label(&em, ".LCk");
  T->emit_data(&em, BE_D64, 42);
  T->emit_data(&em, BE_D32, 7);
  T->emit_data(&em, BE_D16, 3);
  T->emit_string(&em, "hi");
  T->emit_align(&em, 8);
  T->emit_zero(&em, 16);

  char all[4096];
  concat_emitted(em.list, all, sizeof(all));
  CHECK(strstr(all, ".section\t.rodata") != NULL,
        "aarch64 data: .section .rodata");
  CHECK(strstr(all, ".globl\tkdata") != NULL, "aarch64 data: .globl");
  CHECK(strstr(all, ".LCk:") != NULL, "aarch64 data: data label");
  CHECK(strstr(all, ".xword\t42") != NULL, "aarch64 data: .xword (64-bit word)");
  CHECK(strstr(all, ".word\t7") != NULL, "aarch64 data: .word (32-bit word)");
  CHECK(strstr(all, ".hword\t3") != NULL, "aarch64 data: .hword (16-bit word)");
  CHECK(strstr(all, ".string\t\"hi\"") != NULL, "aarch64 data: .string");
  CHECK(strstr(all, ".align\t8") != NULL, "aarch64 data: .align");
  CHECK(strstr(all, ".zero\t16") != NULL, "aarch64 data: .zero");
  /* Neutrality: AArch64 must not carry x86's .quad spelling. */
  CHECK(strstr(all, ".quad") == NULL, "aarch64 data: no x86 .quad spelling");
}

/* Build `int f(int a)`: store the arg into a frame slot via BE_STORE, reload it
 * via BE_LOAD, and return it — exercising the OPK_MEM_FRAME operand end-to-end.
 * The value round-trips through memory, so a wrong base/offset/width fails at
 * run time, not just in a golden check. */
static ListNode_t *build_frame_roundtrip(const Target *T, const char *sym) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 16, 1}; /* reserve 16 bytes of locals below the saves */
  T->emit_prologue(&em, &f);

  Register_t *va = get_free_reg(rs, &em.list);
  BeOperand dva = {OPK_VREG, BE_W32, {.vreg = va}};
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  /* -48(%rbp): inside the 16-byte locals region (x86 saves occupy -8..-40). */
  BeOperand slot = {OPK_MEM_FRAME, BE_W32, {.mem_frame = {BE_BASE_FP, -48}}};

  T->emit(&em, BE_MOV, BE_W32, &dva, &arg0, NULL);   /* va = a */
  T->emit(&em, BE_STORE, BE_W32, &slot, &dva, NULL); /* [frame] = va */

  Register_t *vb = get_free_reg(rs, &em.list);
  BeOperand dvb = {OPK_VREG, BE_W32, {.vreg = vb}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  T->emit(&em, BE_LOAD, BE_W32, &dvb, &slot, NULL); /* vb = [frame] */
  T->emit(&em, BE_MOV, BE_W32, &ret, &dvb, NULL);   /* return vb */
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Build `int f(int a)` that stores the incoming argument register STRAIGHT to a
 * frame slot (BE_STORE with a PHYSICAL value operand) and reloads it into the
 * return register (BE_LOAD with a PHYSICAL dest) — no pool vreg involved.  Both
 * operands are literal, so these lower with no %N placeholder and no def/use. */
static ListNode_t *build_frame_phys(const Target *T, const char *sym) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();

  BeFrame f = {sym, 16, 1};
  T->emit_prologue(&em, &f);
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  BeOperand slot = {OPK_MEM_FRAME, BE_W32, {.mem_frame = {BE_BASE_FP, -48}}};
  T->emit(&em, BE_STORE, BE_W32, &slot, &arg0, NULL); /* [slot] = arg0 (phys) */
  T->emit(&em, BE_LOAD, BE_W32, &ret, &slot, NULL);   /* ret(phys) = [slot] */
  T->emit_epilogue(&em, &f);
  return em.list;
}

static void test_exec_frame_phys(const Target *T, const char *sym, int a) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_frame_phys(T, sym);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(int);\nint main(void){return %s(%d)==%d?0:1;}\n", sym,
           sym, a, a);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: phys frame spill/reload %s(%d)==%d", sym, a,
           a);
  CHECK_EXEC(rc, msg);
}

static void test_golden_x86_frame_phys(const Target *T) {
  ListNode_t *list = build_frame_phys(T, "x86framep");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "movl\t%edi, -48(%rbp)") != NULL,
        "x86 frame-phys: BE_STORE renders <arg phys>, -48(%rbp)");
  CHECK(strstr(all, "movl\t-48(%rbp), %eax") != NULL,
        "x86 frame-phys: BE_LOAD renders -48(%rbp), <ret phys>");
}

static void test_golden_aarch64_frame_phys(const Target *T) {
  ListNode_t *list = build_frame_phys(T, "aaframep");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "str\tw0, [x29, #-48]") != NULL,
        "aarch64 frame-phys: str <arg phys>, [x29, #-48]");
  CHECK(strstr(all, "ldr\tw0, [x29, #-48]") != NULL,
        "aarch64 frame-phys: ldr <ret phys>, [x29, #-48]");
}

/* Build `long f(int a)`: store the 32-bit arg to a frame slot, then sign-extend
 * it (movslq) straight into the 64-bit return register — exercising emit_ext
 * with an OPK_MEM_FRAME source and a physical destination. */
static ListNode_t *build_frame_ext(const Target *T, const char *sym) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();

  BeFrame f = {sym, 16, 1};
  T->emit_prologue(&em, &f);
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand retq = {OPK_PHYS, BE_W64, {.phys = T->return_reg(BE_W64)}};
  BeOperand slot = {OPK_MEM_FRAME, BE_W32, {.mem_frame = {BE_BASE_FP, -48}}};
  T->emit(&em, BE_STORE, BE_W32, &slot, &arg0, NULL); /* [slot] = (int)a */
  /* retq(64) = sign_extend((int)[slot])  via a frame-source extend load */
  T->emit_ext(&em, &retq, &slot, BE_W32, BE_W64, 1);
  T->emit_epilogue(&em, &f);
  return em.list;
}

static void test_exec_frame_ext(const Target *T, const char *sym, int a) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_frame_ext(T, sym);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern long %s(int);\nint main(void){return %s(%d)==%dL?0:1;}\n",
           sym, sym, a, a);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: frame sign-extend load %s(%d)==%d", sym, a,
           a);
  CHECK_EXEC(rc, msg);
}

static void test_golden_x86_frame_ext(const Target *T) {
  ListNode_t *list = build_frame_ext(T, "x86framex");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "movslq\t-48(%rbp), %rax") != NULL,
        "x86 frame-ext: emit_ext renders movslq -48(%rbp), <phys>");
}

static void test_golden_aarch64_frame_ext(const Target *T) {
  ListNode_t *list = build_frame_ext(T, "aaframex");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "ldrsw\tx0, [x29, #-48]") != NULL,
        "aarch64 frame-ext: emit_ext renders ldrsw <phys>, [x29, #-48]");
}

/* Build `int f(int a)`: store the arg to a frame slot, take the slot's ADDRESS
 * with BE_LEA (OPK_MEM_FRAME source), then load the value back THROUGH that
 * computed pointer (BE_LOAD via OPK_MEM_BD) and return it.  A wrong lea address
 * fails at run time. */
static ListNode_t *build_frame_lea(const Target *T, const char *sym) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 16, 1};
  T->emit_prologue(&em, &f);
  Register_t *vp = get_free_reg(rs, &em.list); /* pointer to the slot */
  Register_t *vv = get_free_reg(rs, &em.list); /* reloaded value */
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  BeOperand slot = {OPK_MEM_FRAME, BE_W32, {.mem_frame = {BE_BASE_FP, -48}}};
  BeOperand lea_src = {OPK_MEM_FRAME, BE_W64, {.mem_frame = {BE_BASE_FP, -48}}};
  BeOperand dvp = {OPK_VREG, BE_W64, {.vreg = vp}};
  BeOperand dvv = {OPK_VREG, BE_W32, {.vreg = vv}};
  BeOperand thru = {OPK_MEM_BD, BE_W32, {.mem_bd = {vp, 0}}};
  T->emit(&em, BE_STORE, BE_W32, &slot, &arg0, NULL); /* [slot] = a */
  T->emit(&em, BE_LEA, BE_W64, &dvp, &lea_src, NULL); /* vp = &slot */
  T->emit(&em, BE_LOAD, BE_W32, &dvv, &thru, NULL);   /* vv = [vp+0] */
  T->emit(&em, BE_MOV, BE_W32, &ret, &dvv, NULL);     /* return vv */
  T->emit_epilogue(&em, &f);
  return em.list;
}

static void test_exec_frame_lea(const Target *T, const char *sym, int a) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_frame_lea(T, sym);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(int);\nint main(void){return %s(%d)==%d?0:1;}\n", sym,
           sym, a, a);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: frame lea addr-of %s(%d)==%d", sym, a, a);
  CHECK_EXEC(rc, msg);
}

static void test_golden_x86_frame_lea(const Target *T) {
  ListNode_t *list = build_frame_lea(T, "x86framel");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "leaq\t-48(%rbp), %") != NULL,
        "x86 frame-lea: BE_LEA renders leaq -48(%rbp), <dst>");
}

static void test_golden_aarch64_frame_lea(const Target *T) {
  ListNode_t *list = build_frame_lea(T, "aaframel");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "sub\t") != NULL && strstr(all, ", x29, #48") != NULL,
        "aarch64 frame-lea: BE_LEA renders sub <dst>, x29, #48");
}

/* Build `int f(void)`: store an immediate straight to a frame slot (BE_STORE
 * with an OPK_IMM value), reload it, and return it — exercising the immediate
 * value operand. */
static ListNode_t *build_frame_imm(const Target *T, const char *sym, int imm) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 16, 1};
  T->emit_prologue(&em, &f);
  Register_t *vv = get_free_reg(rs, &em.list);
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  BeOperand slot = {OPK_MEM_FRAME, BE_W32, {.mem_frame = {BE_BASE_FP, -48}}};
  BeOperand ival = {OPK_IMM, BE_W32, {.imm = imm}};
  BeOperand dvv = {OPK_VREG, BE_W32, {.vreg = vv}};
  T->emit(&em, BE_STORE, BE_W32, &slot, &ival, NULL); /* [slot] = $imm */
  T->emit(&em, BE_LOAD, BE_W32, &dvv, &slot, NULL);   /* vv = [slot] */
  T->emit(&em, BE_MOV, BE_W32, &ret, &dvv, NULL);     /* return vv */
  T->emit_epilogue(&em, &f);
  return em.list;
}

static void test_exec_frame_imm(const Target *T, const char *sym, int imm) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_frame_imm(T, sym, imm);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(void);\nint main(void){return %s()==%d?0:1;}\n", sym,
           sym, imm);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: frame imm store %s()==%d", sym, imm);
  CHECK_EXEC(rc, msg);
}

static void test_golden_x86_frame_imm(const Target *T) {
  ListNode_t *list = build_frame_imm(T, "x86framei", 123);
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "movl\t$123, -48(%rbp)") != NULL,
        "x86 frame-imm: BE_STORE renders movl $123, -48(%rbp)");
}

static void test_i386_target(void) {
  const Target *T = target_i386_sysv();
  CHECK(strcmp(T->name, "i386-sysv") == 0, "i386: target name");
  CHECK(T->ptr_width == 4, "i386: 32-bit pointers");
  CHECK(T->num_int_arg_regs() == 0, "i386: arguments use the stack");
  CHECK(T->arg_reg(0, BE_W32) == NULL, "i386: no register argument");
  CHECK(strcmp(T->return_reg(BE_W32), "%eax") == 0,
        "i386: returns integers in %eax");

  ListNode_t *list = build_const(T, "i386const", 12345);
  finalize_and_write("be_i386_const.s", "i386const", list);
  char emitted[4096];
  concat_emitted(list, emitted, sizeof(emitted));
  CHECK(strstr(emitted, "pushl\t%ebp") != NULL,
        "i386: emits a 32-bit frame prologue");
  CHECK(strstr(emitted, "movl\t$12345, %eax") != NULL,
        "i386: emits a 32-bit return value");
  CHECK(strstr(emitted, "%r") == NULL && strstr(emitted, "q\t") == NULL,
        "i386: emitted code contains no x86-64 registers or instructions");
  CHECK(system("as --32 -o be_i386_const.o be_i386_const.s") == 0,
        "i386: generated assembly assembles as ELF32");
}

static void test_golden_aarch64_frame_imm(const Target *T) {
  ListNode_t *l0 = build_frame_imm(T, "aaframei0", 0);
  ir_liveness_allocate(l0);
  ir_emit_function(l0);
  char a0[4096];
  concat_emitted(l0, a0, sizeof(a0));
  CHECK(strstr(a0, "str\twzr, [x29, #-48]") != NULL,
        "aarch64 frame-imm: zero stores via the zero register (str wzr)");
  ListNode_t *l1 = build_frame_imm(T, "aaframei1", 7);
  ir_liveness_allocate(l1);
  ir_emit_function(l1);
  char a1[4096];
  concat_emitted(l1, a1, sizeof(a1));
  CHECK(strstr(a1, "mov\t") != NULL && strstr(a1, "#7") != NULL &&
            strstr(a1, "str\t") != NULL,
        "aarch64 frame-imm: non-zero materializes into a scratch then str");
}

/* Build `int f(int a)`: store a to a frame slot, load the constant 5 into a
 * vreg, compare the vreg against the frame slot (BE_CMP with an OPK_MEM_FRAME
 * operand), and return (5 > a) as 0/1.  Exercises a frame compare end-to-end. */
static ListNode_t *build_frame_cmp(const Target *T, const char *sym) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 16, 1};
  T->emit_prologue(&em, &f);
  Register_t *va = get_free_reg(rs, &em.list);
  Register_t *vc = get_free_reg(rs, &em.list);
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  BeOperand slot = {OPK_MEM_FRAME, BE_W32, {.mem_frame = {BE_BASE_FP, -48}}};
  BeOperand dva = {OPK_VREG, BE_W32, {.vreg = va}};
  BeOperand dvc = {OPK_VREG, BE_W32, {.vreg = vc}};
  BeOperand five = {OPK_IMM, BE_W32, {.imm = 5}};
  T->emit(&em, BE_MOV, BE_W32, &dva, &arg0, NULL);   /* va = a */
  T->emit(&em, BE_STORE, BE_W32, &slot, &dva, NULL); /* [slot] = a */
  T->emit(&em, BE_MOV, BE_W32, &dvc, &five, NULL);   /* vc = 5 */
  T->emit(&em, BE_CMP, BE_W32, NULL, &dvc, &slot);   /* flags: 5 ? [slot] */
  T->emit_setcc(&em, BE_GT, &dvc);                   /* vc = (5 > a) */
  T->emit(&em, BE_MOV, BE_W32, &ret, &dvc, NULL);    /* return vc */
  T->emit_epilogue(&em, &f);
  return em.list;
}

static void test_exec_frame_cmp(const Target *T, const char *sym, int a,
                                int expected) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_frame_cmp(T, sym);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(int);\nint main(void){return %s(%d)==%d?0:1;}\n", sym,
           sym, a, expected);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: frame cmp (5>%d)==%d via %s", a, expected,
           sym);
  CHECK_EXEC(rc, msg);
}

/* Golden: emit all four cmp-with-frame operand combinations and assert the
 * rendered AT&T form of each (frame as either operand; other = vreg/imm/phys). */
static void test_golden_x86_frame_cmp(const Target *T) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();
  Register_t *v = get_free_reg(rs, &em.list);
  BeOperand dv = {OPK_VREG, BE_W32, {.vreg = v}};
  BeOperand slot = {OPK_MEM_FRAME, BE_W32, {.mem_frame = {BE_BASE_FP, -48}}};
  BeOperand imm = {OPK_IMM, BE_W32, {.imm = 9}};
  BeOperand eax = {OPK_PHYS, BE_W32, {.phys = "%eax"}};
  T->emit(&em, BE_CMP, BE_W32, NULL, &dv, &slot);   /* D: cmpl -48(%rbp), %v */
  T->emit(&em, BE_CMP, BE_W32, NULL, &slot, &dv);   /* A: cmpl %v, -48(%rbp) */
  T->emit(&em, BE_CMP, BE_W32, NULL, &slot, &imm);  /* B: cmpl $9, -48(%rbp) */
  T->emit(&em, BE_CMP, BE_W32, NULL, &eax, &slot);  /* C: cmpl -48(%rbp), %eax */
  ir_liveness_allocate(em.list);
  ir_emit_function(em.list);
  char all[4096];
  concat_emitted(em.list, all, sizeof(all));
  CHECK(strstr(all, "-48(%rbp), %") != NULL,
        "x86 frame-cmp: frame as compared operand (cmp -48(%rbp), reg)");
  CHECK(strstr(all, ", -48(%rbp)") != NULL,
        "x86 frame-cmp: frame as destination operand (cmp x, -48(%rbp))");
  CHECK(strstr(all, "cmpl\t$9, -48(%rbp)") != NULL,
        "x86 frame-cmp: immediate vs frame (cmpl $9, -48(%rbp))");
  CHECK(strstr(all, "cmpl\t-48(%rbp), %eax") != NULL,
        "x86 frame-cmp: frame vs physical reg (cmpl -48(%rbp), %eax)");
}

static void test_golden_aarch64_frame_cmp(const Target *T) {
  ListNode_t *list = build_frame_cmp(T, "aacmp");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "ldr\t") != NULL && strstr(all, "[x29, #-48]") != NULL,
        "aarch64 frame-cmp: loads the frame operand into a scratch");
  CHECK(strstr(all, "cmp\t") != NULL,
        "aarch64 frame-cmp: compares register-to-register after the load");
}

/* Build `int f(int a, int b)`: spill both args to frame slots, then BE_CMP
 * with BOTH operands frame-relative and return (a == b) as 0/1.  Regression:
 * aliasing the two sides to one scratch yields cmp scr, scr — always equal. */
static ListNode_t *build_frame_frame_cmp(const Target *T, const char *sym) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();

  BeFrame f = {sym, 16, 1};
  T->emit_prologue(&em, &f);
  Register_t *va = get_free_reg(rs, &em.list);
  Register_t *vb = get_free_reg(rs, &em.list);
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand arg1 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(1, BE_W32)}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  BeOperand slot_a = {OPK_MEM_FRAME, BE_W32, {.mem_frame = {BE_BASE_FP, -48}}};
  BeOperand slot_b = {OPK_MEM_FRAME, BE_W32, {.mem_frame = {BE_BASE_FP, -56}}};
  BeOperand dva = {OPK_VREG, BE_W32, {.vreg = va}};
  BeOperand dvb = {OPK_VREG, BE_W32, {.vreg = vb}};
  T->emit(&em, BE_MOV, BE_W32, &dva, &arg0, NULL);     /* va = a */
  T->emit(&em, BE_STORE, BE_W32, &slot_a, &dva, NULL); /* [slotA] = a */
  T->emit(&em, BE_MOV, BE_W32, &dvb, &arg1, NULL);     /* vb = b */
  T->emit(&em, BE_STORE, BE_W32, &slot_b, &dvb, NULL); /* [slotB] = b */
  T->emit(&em, BE_CMP, BE_W32, NULL, &slot_a, &slot_b); /* [slotA] ? [slotB] */
  T->emit_setcc(&em, BE_EQ, &dva);                     /* va = (a == b) */
  T->emit(&em, BE_MOV, BE_W32, &ret, &dva, NULL);      /* return va */
  T->emit_epilogue(&em, &f);
  return em.list;
}

static void test_exec_frame_frame_cmp(const Target *T, const char *sym, int a,
                                      int b, int expected) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_frame_frame_cmp(T, sym);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(int,int);\nint main(void){return %s(%d,%d)==%d?0:1;}\n",
           sym, sym, a, b, expected);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: frame-frame cmp (%d==%d)==%d via %s", a, b,
           expected, sym);
  CHECK_EXEC(rc, msg);
}

static void test_golden_x86_frame_frame_cmp(const Target *T) {
  ListNode_t *list = build_frame_frame_cmp(T, "x86ffcmp");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "movl\t-56(%rbp), %") != NULL,
        "x86 frame-frame cmp: loads one side into a scratch");
  const char *cmp = strstr(all, "\tcmpl\t");
  CHECK(cmp != NULL && strstr(cmp, ", -48(%rbp)") != NULL,
        "x86 frame-frame cmp: compares scratch against the other frame slot");
}

static void test_golden_aarch64_frame_frame_cmp(const Target *T) {
  ListNode_t *list = build_frame_frame_cmp(T, "aaffcmp");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "[x29, #-48]") != NULL &&
            strstr(all, "[x29, #-56]") != NULL,
        "aarch64 frame-frame cmp: loads BOTH frame operands");
  const char *cmp = strstr(all, "\tcmp\t");
  CHECK(cmp != NULL, "aarch64 frame-frame cmp: has a cmp");
  if (cmp != NULL) {
    char r1[16] = {0}, r2[16] = {0};
    CHECK(sscanf(cmp, "\tcmp\t%15[^,], %15[^\n]", r1, r2) == 2 &&
              strcmp(r1, r2) != 0,
          "aarch64 frame-frame cmp: compares two DISTINCT scratches");
  }
}

/* Build `double f(double a)`: store the incoming xmm argument to a frame slot
 * (BE_STORE, float width, physical xmm operand) and reload it into the xmm
 * return register (BE_LOAD) — exercising the frame float mov path (movsd on
 * x86; ldr/str d-reg on AArch64). */
static ListNode_t *build_frame_float(const Target *T, const char *sym) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();

  BeFrame f = {sym, 16, 1};
  T->emit_prologue(&em, &f);
  BeOperand arg0 = {OPK_PHYS, BE_WF64, {.phys = T->arg_reg(0, BE_WF64)}};
  BeOperand ret = {OPK_PHYS, BE_WF64, {.phys = T->return_reg(BE_WF64)}};
  BeOperand slot = {OPK_MEM_FRAME, BE_WF64, {.mem_frame = {BE_BASE_FP, -48}}};
  T->emit(&em, BE_STORE, BE_WF64, &slot, &arg0, NULL); /* [slot] = xmm0 */
  T->emit(&em, BE_LOAD, BE_WF64, &ret, &slot, NULL);   /* xmm0 = [slot] */
  T->emit_epilogue(&em, &f);
  return em.list;
}

static void test_exec_frame_float(const Target *T, const char *sym) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_frame_float(T, sym);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern double %s(double);\n"
           "int main(void){return %s(3.5)==3.5?0:1;}\n",
           sym, sym);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: frame float spill/reload %s(3.5)==3.5", sym);
  CHECK_EXEC(rc, msg);
}

static void test_golden_x86_frame_float(const Target *T) {
  ListNode_t *list = build_frame_float(T, "x86framef");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "movsd\t%xmm0, -48(%rbp)") != NULL,
        "x86 frame-float: BE_STORE renders movsd %xmm0, -48(%rbp)");
  CHECK(strstr(all, "movsd\t-48(%rbp), %xmm0") != NULL,
        "x86 frame-float: BE_LOAD renders movsd -48(%rbp), %xmm0");
}

static void test_golden_aarch64_frame_float(const Target *T) {
  ListNode_t *list = build_frame_float(T, "aaframef");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "str\td0, [x29, #-48]") != NULL,
        "aarch64 frame-float: BE_STORE renders str d0, [x29, #-48]");
  CHECK(strstr(all, "ldr\td0, [x29, #-48]") != NULL,
        "aarch64 frame-float: BE_LOAD renders ldr d0, [x29, #-48]");
}

/* Build `int f(int a)`: take the address of a frame slot into a vreg pointer,
 * store a THROUGH that pointer (BE_STORE via OPK_MEM_BD, base=vreg), reload it
 * (BE_LOAD via OPK_MEM_BD) and return — base rendered as a 64-bit tracked USE. */
static ListNode_t *build_mem_bd(const Target *T, const char *sym) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();
  BeFrame f = {sym, 16, 1};
  T->emit_prologue(&em, &f);
  Register_t *va = get_free_reg(rs, &em.list);
  Register_t *vp = get_free_reg(rs, &em.list);
  Register_t *vr = get_free_reg(rs, &em.list);
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  BeOperand lea_src = {OPK_MEM_FRAME, BE_W64, {.mem_frame = {BE_BASE_FP, -48}}};
  BeOperand dva = {OPK_VREG, BE_W32, {.vreg = va}};
  BeOperand dvp = {OPK_VREG, BE_W64, {.vreg = vp}};
  BeOperand dvr = {OPK_VREG, BE_W32, {.vreg = vr}};
  BeOperand mem = {OPK_MEM_BD, BE_W32, {.mem_bd = {vp, 0}}};
  T->emit(&em, BE_MOV, BE_W32, &dva, &arg0, NULL);
  T->emit(&em, BE_LEA, BE_W64, &dvp, &lea_src, NULL); /* vp = &slot */
  T->emit(&em, BE_STORE, BE_W32, &mem, &dva, NULL);   /* [vp] = a */
  T->emit(&em, BE_LOAD, BE_W32, &dvr, &mem, NULL);    /* vr = [vp] */
  T->emit(&em, BE_MOV, BE_W32, &ret, &dvr, NULL);
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Build `int f(int a)`: store a to base[index] (index=1, scale=4) via
 * OPK_MEM_BIS and reload it — base+index rendered as 64-bit tracked USES. */
static ListNode_t *build_mem_bis(const Target *T, const char *sym) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();
  BeFrame f = {sym, 32, 1};
  T->emit_prologue(&em, &f);
  Register_t *va = get_free_reg(rs, &em.list);
  Register_t *vb = get_free_reg(rs, &em.list);
  Register_t *vi = get_free_reg(rs, &em.list);
  Register_t *vr = get_free_reg(rs, &em.list);
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  BeOperand lea_base = {OPK_MEM_FRAME, BE_W64, {.mem_frame = {BE_BASE_FP, -64}}};
  BeOperand dva = {OPK_VREG, BE_W32, {.vreg = va}};
  BeOperand dvb = {OPK_VREG, BE_W64, {.vreg = vb}};
  BeOperand dvi = {OPK_VREG, BE_W64, {.vreg = vi}};
  BeOperand dvr = {OPK_VREG, BE_W32, {.vreg = vr}};
  BeOperand one = {OPK_IMM, BE_W64, {.imm = 1}};
  BeOperand mem = {OPK_MEM_BIS, BE_W32, {.mem_bis = {vb, vi, 4, 0}}};
  T->emit(&em, BE_MOV, BE_W32, &dva, &arg0, NULL);
  T->emit(&em, BE_LEA, BE_W64, &dvb, &lea_base, NULL); /* vb = &array */
  T->emit(&em, BE_MOV, BE_W64, &dvi, &one, NULL);      /* vi = 1 */
  T->emit(&em, BE_STORE, BE_W32, &mem, &dva, NULL);    /* array[1] = a */
  T->emit(&em, BE_LOAD, BE_W32, &dvr, &mem, NULL);     /* vr = array[1] */
  T->emit(&em, BE_MOV, BE_W32, &ret, &dvr, NULL);
  T->emit_epilogue(&em, &f);
  return em.list;
}

/* Build `int f(int a)`: store the low byte of a through a pointer, then
 * sign/zero-extend it back via emit_ext with an OPK_MEM_BD source. */
static ListNode_t *build_ext_deref(const Target *T, const char *sym,
                                   int is_signed) {
  BackendCtx cx = {0, 0};
  BeEmitter em = be_emitter_from_backendctx(NULL, &cx);
  add_inst_invalidate_cache();
  select_target_pool(T);
  reset_reg_stack();
  RegStack_t *rs = get_reg_stack();
  BeFrame f = {sym, 16, 1};
  T->emit_prologue(&em, &f);
  Register_t *va = get_free_reg(rs, &em.list);
  Register_t *vp = get_free_reg(rs, &em.list);
  Register_t *vr = get_free_reg(rs, &em.list);
  BeOperand arg0 = {OPK_PHYS, BE_W32, {.phys = T->arg_reg(0, BE_W32)}};
  BeOperand ret = {OPK_PHYS, BE_W32, {.phys = T->return_reg(BE_W32)}};
  BeOperand lea_src = {OPK_MEM_FRAME, BE_W64, {.mem_frame = {BE_BASE_FP, -48}}};
  BeOperand dva = {OPK_VREG, BE_W32, {.vreg = va}};
  BeOperand dvp = {OPK_VREG, BE_W64, {.vreg = vp}};
  BeOperand dvr = {OPK_VREG, BE_W32, {.vreg = vr}};
  BeOperand mem = {OPK_MEM_BD, BE_W8, {.mem_bd = {vp, 0}}};
  BeOperand memr = {OPK_MEM_BD, BE_W8, {.mem_bd = {vp, 0}}};
  T->emit(&em, BE_MOV, BE_W32, &dva, &arg0, NULL);
  T->emit(&em, BE_LEA, BE_W64, &dvp, &lea_src, NULL);     /* vp = &slot */
  T->emit(&em, BE_STORE, BE_W8, &mem, &dva, NULL);        /* [vp] = (byte)a */
  T->emit_ext(&em, &dvr, &memr, BE_W8, BE_W32, is_signed); /* vr = ext([vp]) */
  T->emit(&em, BE_MOV, BE_W32, &ret, &dvr, NULL);
  T->emit_epilogue(&em, &f);
  return em.list;
}

static void run_exec(const char *tag, const char *sym, ListNode_t *list,
                     const char *proto_arg, int arg, int expected,
                     const char *what) {
  char spath[256], driver[512], msg[160];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(%s);\nint main(void){return %s(%d)==%d?0:1;}\n", sym,
           proto_arg, sym, arg, expected);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: %s %s(%d)==%d", what, sym, arg, expected);
  (void)tag;
  CHECK_EXEC(rc, msg);
}

static void test_mem_operands_x86(const Target *T) {
  run_exec("bd", "bemembd", build_mem_bd(T, "bemembd"), "int", 42, 42,
           "MEM_BD ptr deref roundtrip");
  run_exec("bis", "bemembis", build_mem_bis(T, "bemembis"), "int", 77, 77,
           "MEM_BIS array[index] roundtrip");
  run_exec("zx", "beextz", build_ext_deref(T, "beextz", 0), "int", 0xFF, 255,
           "emit_ext zero-extend from MEM_BD");
  run_exec("sx", "beexts", build_ext_deref(T, "beexts", 1), "int", 0xFF, -1,
           "emit_ext sign-extend from MEM_BD");
}

static void test_golden_x86_mem_operands(const Target *T) {
  ListNode_t *l = build_mem_bis(T, "x86bis");
  ir_liveness_allocate(l);
  ir_emit_function(l);
  char all[4096];
  concat_emitted(l, all, sizeof(all));
  CHECK(strstr(all, ",4), %") != NULL || strstr(all, ",4)") != NULL,
        "x86 MEM_BIS: renders (base,index,4) index form");
  ListNode_t *l2 = build_mem_bd(T, "x86bd");
  ir_liveness_allocate(l2);
  ir_emit_function(l2);
  char all2[4096];
  concat_emitted(l2, all2, sizeof(all2));
  CHECK(strstr(all2, "(%r") != NULL,
        "x86 MEM_BD: renders (%rbase) with a 64-bit base register");
}

static void test_golden_aarch64_mem_operands(const Target *T) {
  ListNode_t *l = build_mem_bis(T, "aabis");
  ir_liveness_allocate(l);
  ir_emit_function(l);
  char all[4096];
  concat_emitted(l, all, sizeof(all));
  CHECK(strstr(all, "lsl #2") != NULL,
        "aarch64 MEM_BIS: renders [base, index, lsl #2] for scale 4");
  CHECK(strstr(all, "%rbp") == NULL && strstr(all, "%rax") == NULL,
        "aarch64 MEM_BIS: no x86 register leakage");
}

static void test_exec_frame(const Target *T, const char *sym, int a) {
  char spath[256], driver[512], msg[128];
  snprintf(spath, sizeof(spath), "be_%s.s", sym);
  ListNode_t *list = build_frame_roundtrip(T, sym);
  finalize_and_write(spath, sym, list);
  snprintf(driver, sizeof(driver),
           "extern int %s(int);\nint main(void){return %s(%d)==%d?0:1;}\n", sym,
           sym, a, a);
  int rc = assemble_link_run(sym, spath, driver);
  snprintf(msg, sizeof(msg), "exec: frame spill/reload %s(%d)==%d", sym, a, a);
  CHECK_EXEC(rc, msg);
}

static void test_golden_x86_frame(const Target *T) {
  ListNode_t *list = build_frame_roundtrip(T, "x86frame");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, ", -48(%rbp)") != NULL,
        "x86 frame: BE_STORE renders -48(%rbp) dest");
  CHECK(strstr(all, "-48(%rbp), %") != NULL,
        "x86 frame: BE_LOAD renders -48(%rbp) source");
  CHECK(strstr(all, "movl") != NULL, "x86 frame: 32-bit width suffix");
}

static void test_golden_aarch64_frame(const Target *T) {
  ListNode_t *list = build_frame_roundtrip(T, "aaframe");
  ir_liveness_allocate(list);
  ir_emit_function(list);
  char all[4096];
  concat_emitted(list, all, sizeof(all));
  CHECK(strstr(all, "str\tw") != NULL, "aarch64 frame: str w-reg (32-bit store)");
  CHECK(strstr(all, "[x29, #-48]") != NULL,
        "aarch64 frame: fixed x29 base + disp");
  CHECK(strstr(all, "ldr\tw") != NULL, "aarch64 frame: ldr w-reg (32-bit load)");
  CHECK(strstr(all, "%rbp") == NULL && strstr(all, "%rsp") == NULL,
        "aarch64 frame: no x86 base leakage");
}

int main(void) {
  g_current_codegen_abi = KGPC_TARGET_ABI_SYSTEM_V;
  g_stack_home_space_bytes = 0;
  init_stackmng();

  const Target *T = target_x86_sysv();
  fprintf(stderr, "== backend exec tests (target=%s) ==\n", T->name);

  /* Tier 1: golden-asm */
  test_golden_add(T);

  /* Tier 2: assemble-link-run */
  test_exec_binop(T, "beadd", BE_ADD, 2, 3, 5);
  test_exec_binop(T, "besub", BE_SUB, 10, 3, 7);
  test_exec_binop(T, "bemul", BE_MUL, 6, 7, 42);
  test_exec_binop(T, "beaddneg", BE_ADD, -4, 9, 5);
  test_exec_binop(T, "beand", BE_AND, 12, 10, 8);
  test_exec_binop(T, "beor", BE_OR, 12, 10, 14);
  test_exec_binop(T, "bexor", BE_XOR, 12, 10, 6);
  test_exec_shift(T, "beshl", BE_SHL, 3, 2, 12);
  test_exec_shift(T, "beshr", BE_SHR, 20, 2, 5);
  test_exec_neg(T, "beneg", 7, -7);
  test_exec_binop(T, "bediv", BE_DIV, 20, 3, 6);
  test_exec_binop(T, "bemod", BE_MOD, 20, 3, 2);
  test_exec_binop(T, "bedivneg", BE_DIV, -20, 3, -6);
  test_exec_cmp(T, "belt_t", BE_LT, 3, 5, 1);
  test_exec_cmp(T, "belt_f", BE_LT, 5, 3, 0);
  test_exec_cmp(T, "beeq_t", BE_EQ, 4, 4, 1);
  test_exec_const(T, "beconst", 12345);

  /* Sign / zero extend (movsbl/movzbl/movswl/movzwl/movsbq/movslq/movl). */
  test_exec_ext(T, "besxtb", BE_W8, 1, 0xFF, -1);       /* (signed char)0xFF */
  test_exec_ext(T, "bezxtb", BE_W8, 0, 0xFF, 255);      /* (byte)0xFF */
  test_exec_ext(T, "besxth", BE_W16, 1, 0x8000, -32768);/* (int16)0x8000 */
  test_exec_ext(T, "bezxth", BE_W16, 0, 0x8000, 32768); /* (uint16)0x8000 */
  test_exec_ext64(T, "besxtbq", BE_W8, 1, 0xFF, -1LL);  /* byte→64 signed */
  test_exec_ext64(T, "bezxtbq", BE_W8, 0, 0xFF, 255LL); /* byte→64 zero */
  test_exec_ext64(T, "besxlq", BE_W32, 1, -5, -5LL);    /* movslq */
  test_exec_ext64(T, "bezxlq", BE_W32, 0, -1, 4294967295LL); /* movl zero-ext */

  /* Frame-relative memory operand (OPK_MEM_FRAME): BE_STORE/BE_LOAD spill a
   * value to a %rbp-relative slot and reload it. */
  fprintf(stderr, "-- frame-relative memory operand --\n");
  test_golden_x86_frame(T);
  test_exec_frame(T, "beframe", 42);
  test_golden_x86_frame_phys(T);
  test_exec_frame_phys(T, "beframep", 99);
  test_golden_x86_frame_ext(T);
  test_exec_frame_ext(T, "beframex", -5);
  test_golden_x86_frame_lea(T);
  test_exec_frame_lea(T, "beframel", 77);
  test_golden_x86_frame_imm(T);
  test_exec_frame_imm(T, "beframei", 123);
  test_exec_frame_imm(T, "beframei0", 0);
  test_i386_target();
  test_golden_x86_frame_cmp(T);
  test_exec_frame_cmp(T, "beframec1", 3, 1); /* 5 > 3 -> 1 */
  test_exec_frame_cmp(T, "beframec0", 7, 0); /* 5 > 7 -> 0 */
  test_golden_x86_frame_frame_cmp(T);
  test_exec_frame_frame_cmp(T, "beffceq", 4, 4, 1); /* 4 == 4 -> 1 */
  test_exec_frame_frame_cmp(T, "beffcne", 3, 5, 0); /* 3 == 5 -> 0 */
  test_golden_x86_frame_float(T);
  test_exec_frame_float(T, "beframef");
  test_golden_x86_mem_operands(T);
  test_mem_operands_x86(T);

  /* Floating-point (double / IEEE-754 64-bit): SSE scalar arithmetic + int↔
   * float conversions.  Values are exactly representable so `==` is exact. */
  fprintf(stderr, "-- floating-point (double) --\n");
  test_exec_fbinop(T, "befadd", BE_ADD, 2.5, 1.5, 4.0);
  test_exec_fbinop(T, "befsub", BE_SUB, 5.0, 1.5, 3.5);
  test_exec_fbinop(T, "befmul", BE_MUL, 3.0, 4.0, 12.0);
  test_exec_fbinop(T, "befdiv", BE_DIV, 9.0, 2.0, 4.5);
  test_exec_f2i(T, "bef2i", 3.9, 3);   /* (int)3.9 == 3 (truncate) */
  test_exec_i2f(T, "bei2f", 7, 7.0);   /* (double)7 == 7.0 */

  /* Directive / data channel: emit sections + data words through the neutral
   * API, then assemble-link-run to prove the emitted data is real. */
  fprintf(stderr, "-- directive / data channel --\n");
  test_golden_x86_data(T);
  test_exec_data_const(T);
  test_exec_data_array(T);

  /* M4: neutrality proof via a second backend (golden-asm; no local AArch64
   * toolchain/qemu here to run exec — that tier is intentionally skipped). */
  const Target *A = target_aarch64();
  fprintf(stderr, "-- neutrality: same harness through target=%s --\n", A->name);
  test_golden_aarch64(A);
  test_golden_aarch64_ext(A);
  test_golden_aarch64_float(A);
  test_golden_aarch64_data(A);
  test_golden_aarch64_frame(A);
  test_golden_aarch64_frame_phys(A);
  test_golden_aarch64_frame_ext(A);
  test_golden_aarch64_frame_lea(A);
  test_golden_aarch64_frame_imm(A);
  test_golden_aarch64_frame_cmp(A);
  test_golden_aarch64_frame_frame_cmp(A);
  test_golden_aarch64_frame_float(A);
  test_golden_aarch64_mem_operands(A);
  fprintf(stderr,
          "note: AArch64 assemble-link-run skipped (no aarch64 toolchain/qemu "
          "in this environment)\n");

  if (g_exec_skips > 0)
    fprintf(stderr,
            "note: assemble-link-run tier skipped (%d execs): host C ABI is "
            "not x86-64 System V\n",
            g_exec_skips);
  fprintf(stderr, "== %d/%d checks passed ==\n", g_tests - g_failures, g_tests);
  return g_failures == 0 ? 0 : 1;
}
