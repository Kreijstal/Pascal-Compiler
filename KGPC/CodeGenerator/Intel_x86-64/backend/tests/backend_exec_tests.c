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

/* Point the shared allocator's register pool at this target's registers.
 * This is the register-file generalization in action: the same allocator
 * colors into x86 or AArch64 registers depending only on the target. */
static void select_target_pool(const Target *T) {
  int n = 0;
  const BackendRegSpec *specs = T->regpool(&n);
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
  CHECK(rc == 0, msg);
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
  CHECK(rc == 0, msg);
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
  CHECK(rc == 0, msg);
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
  CHECK(rc == 0, msg);
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
  CHECK(rc == 0, msg);
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
  CHECK(rc == 0, msg);
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
  CHECK(rc == 0, msg);
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

  /* M4: neutrality proof via a second backend (golden-asm; no local AArch64
   * toolchain/qemu here to run exec — that tier is intentionally skipped). */
  const Target *A = target_aarch64();
  fprintf(stderr, "-- neutrality: same harness through target=%s --\n", A->name);
  test_golden_aarch64(A);
  test_golden_aarch64_ext(A);
  fprintf(stderr,
          "note: AArch64 assemble-link-run skipped (no aarch64 toolchain/qemu "
          "in this environment)\n");

  fprintf(stderr, "== %d/%d checks passed ==\n", g_tests - g_failures, g_tests);
  return g_failures == 0 ? 0 : 1;
}
