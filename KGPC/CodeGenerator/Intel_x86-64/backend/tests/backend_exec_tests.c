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
  CHECK(strstr(all, "add\t") != NULL, "aarch64: has add");
  CHECK(strstr(all, "x19") != NULL, "aarch64: allocator colored into x19");
  CHECK(strstr(all, "x0") != NULL, "aarch64: uses x0 arg/return");
  CHECK(strstr(all, "ret") != NULL, "aarch64: has ret");
  /* Neutrality: no x86 registers must appear in AArch64 output. */
  CHECK(strstr(all, "%rbx") == NULL && strstr(all, "%rax") == NULL &&
            strstr(all, "%rbp") == NULL,
        "aarch64: no x86 register leakage");
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
  test_exec_const(T, "beconst", 12345);

  /* M4: neutrality proof via a second backend (golden-asm; no local AArch64
   * toolchain/qemu here to run exec — that tier is intentionally skipped). */
  const Target *A = target_aarch64();
  fprintf(stderr, "-- neutrality: same harness through target=%s --\n", A->name);
  test_golden_aarch64(A);
  fprintf(stderr,
          "note: AArch64 assemble-link-run skipped (no aarch64 toolchain/qemu "
          "in this environment)\n");

  fprintf(stderr, "== %d/%d checks passed ==\n", g_tests - g_failures, g_tests);
  return g_failures == 0 ? 0 : 1;
}
