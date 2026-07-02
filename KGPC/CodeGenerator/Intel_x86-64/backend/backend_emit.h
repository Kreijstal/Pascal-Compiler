/*
 * backend_emit.h — target-independent instruction-list emission core.
 *
 * These functions build and serialize the backend's instruction list without
 * any dependency on the Pascal front-end.  They were moved out of codegen.c so
 * that the emission core (this file, the IR layer, the register allocator, and
 * stackmng) links as a standalone library (libkgpc_backend) with no AST/symbol
 * -table coupling.
 *
 * The front-end keeps its historical CodeGenContext-based entry points
 * (add_inst_du / gen_label / codegen_inst_list, declared in codegen.h) as thin
 * shims over the context-free cores below — so its ~1,900 call sites are
 * unchanged.
 */
#ifndef KGPC_BACKEND_EMIT_H
#define KGPC_BACKEND_EMIT_H

#include <stdio.h>

#include "backend_debug.h"

/* Forward declarations — full definitions live in List.h / stackmng.h.
 * Matches the pattern already used by ir/ir_inst.h and ir/ir_cfg.h. */
typedef struct List ListNode_t;
typedef struct Register Register_t;

/* Minimal backend-local counters that replace CodeGenContext for the
 * standalone emission core.  The main compiler does not use this struct; its
 * shims pass &ctx->next_vreg_id / &ctx->label_counter directly. */
typedef struct BackendCtx {
  int next_vreg_id;
  int label_counter;
} BackendCtx;

/* x86_64 ABI stack alignment: frame offsets are rounded up to this.
 * Backend constant (was in codegen.h); the front-end gets it by including
 * this header from codegen.h. */
#ifndef REQUIRED_OFFSET
#define REQUIRED_OFFSET 16
#endif

/* Append a plain instruction (LIST_STRING node).  Makes a copy of `inst`. */
ListNode_t *add_inst(ListNode_t *inst_list, const char *inst);

/* Reset the O(1)-append tail cache.  MUST be called before free_inst_list /
 * ConcatList on a list built by add_inst. */
void add_inst_invalidate_cache(void);

/* Context-free core of add_inst_du: emit a template with %0/%1 def/use
 * register placeholders (LIST_IR_INST node).  Registers with vreg_id == -1
 * receive a fresh ID from *next_vreg_id (nullable — pass NULL to skip ID
 * assignment). */
ListNode_t *be_add_inst_du(ListNode_t *inst_list, int *next_vreg_id,
                           Register_t **defs, int n_defs, Register_t **uses,
                           int n_uses, const char *fmt);

/* Context-free label generator: writes ".L<n>" using ++*label_counter. */
void be_gen_label(char *buf, int buf_len, int *label_counter);

/* Serialize an instruction list (LIST_STRING / LIST_IR_INST nodes) to out. */
void be_inst_list_write(FILE *out, ListNode_t *inst_list);

#endif /* KGPC_BACKEND_EMIT_H */
