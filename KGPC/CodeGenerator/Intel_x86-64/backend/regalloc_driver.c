/*
 * regalloc_driver.c — graph-coloring register allocation driver.
 *
 * ir_liveness_allocate moved verbatim from codegen.c (it had zero
 * CodeGenContext coupling — it drives cfg_build / liveness_compute / the
 * interference graph / the graph-coloring allocator over the current
 * reg_stack).  Lives in the standalone backend library.
 */
#include "regalloc_driver.h"

#include "../ir/ir_cfg.h"
#include "../ir/ir_inst.h"
#include "../ir/ir_liveness.h"
#include "../stackmng/stackmng.h"

#include "../../../Parser/List/List.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#if USE_GRAPH_COLORING_ALLOCATOR
#include "../graph_coloring_allocator.h"

/*
 * ir_liveness_allocate — graph-coloring register allocator driven by IR
 * liveness analysis.
 *
 * Uses stable integer vreg IDs (stored in IrInst_t.vreg_ids[] at
 * add_inst_du() time) rather than Register_t* pointers, so this is safe
 * to call after reset_reg_stack() has freed the original register objects.
 *
 * Algorithm:
 *  1. Build the CFG for the function's instruction list.
 *  2. Compute live-in / live-out sets via backward dataflow (liveness_compute).
 *  3. Discover the physical register pool from the current reg_stack.
 *  4. Scan the instruction list to:
 *       a. determine the maximum vreg_id used, and
 *       b. map each vreg_id to its current physical register name
 * (preferred_color).
 *  5. Build an interference graph: two vregs interfere if they appear
 *     simultaneously in any live_out[B] or live_in[B] set, or if they are
 *     both defs of the same instruction.
 *  6. Run the simplify/select/color loop (preferred_color keeps existing
 *     assignments stable when there is no conflict).
 *  7. If the coloring succeeds with no spills, apply it by updating
 *     IrInst_t.reg_names_64[] and reg_names_32[] in every instruction.
 *  8. Free all temporary data structures.
 */
void ir_liveness_allocate(ListNode_t *inst_list) {
  if (inst_list == NULL)
    return;

  /* ------------------------------------------------------------------ */
  /* Step 1 & 2: CFG + liveness                                          */
  /* ------------------------------------------------------------------ */
  Cfg_t *cfg = cfg_build(inst_list);
  if (cfg == NULL)
    return;

  LivenessInfo_t *liveness = liveness_compute(cfg);
  if (liveness == NULL) {
    cfg_free(cfg);
    return;
  }

  /* ------------------------------------------------------------------ */
  /* Step 3: physical register pool from reg_stack                       */
  /* The reg_stack always holds the same 5 callee-saved registers        */
  /* (%rbx, %r12-%r15).  We enumerate them to build a color→name table. */
  /* ------------------------------------------------------------------ */
#define LIVENESS_MAX_POOL 16
  const char *pool_names_64[LIVENESS_MAX_POOL];
  const char *pool_names_32[LIVENESS_MAX_POOL];
  int pool_size = 0;

  RegStack_t *rstack = get_reg_stack();
  if (rstack != NULL) {
    for (ListNode_t *n = rstack->registers_free;
         n != NULL && pool_size < LIVENESS_MAX_POOL; n = n->next) {
      Register_t *reg = (Register_t *)n->cur;
      if (reg != NULL && reg->bit_64 != NULL) {
        pool_names_64[pool_size] = reg->bit_64;
        pool_names_32[pool_size] = reg->bit_32;
        pool_size++;
      }
    }
    for (ListNode_t *n = rstack->registers_allocated;
         n != NULL && pool_size < LIVENESS_MAX_POOL; n = n->next) {
      Register_t *reg = (Register_t *)n->cur;
      if (reg != NULL && reg->bit_64 != NULL) {
        pool_names_64[pool_size] = reg->bit_64;
        pool_names_32[pool_size] = reg->bit_32;
        pool_size++;
      }
    }
  }

  if (pool_size == 0) {
    liveness_free(liveness);
    cfg_free(cfg);
    return;
  }

  /* ------------------------------------------------------------------ */
  /* Step 4a: find maximum vreg_id used in this function's instruction   */
  /* list.                                                               */
  /* ------------------------------------------------------------------ */
  int max_vreg = -1;
  for (ListNode_t *n = inst_list; n != NULL; n = n->next) {
    if (n->type == LIST_IR_INST) {
      const IrInst_t *inst = (const IrInst_t *)n->cur;
      if (inst == NULL)
        continue;
      int total = inst->n_defs + inst->n_uses;
      for (int i = 0; i < total && i < IR_MAX_DEFS + IR_MAX_USES; ++i) {
        if (inst->vreg_ids[i] > max_vreg)
          max_vreg = inst->vreg_ids[i];
      }
    }
  }

  if (max_vreg < 0) {
    /* No virtual registers — nothing to allocate. */
    liveness_free(liveness);
    cfg_free(cfg);
    return;
  }

  int n_vregs = max_vreg + 1;

  /* ------------------------------------------------------------------ */
  /* Step 4b: map vreg_id → preferred color (= current pool index)       */
  /* ------------------------------------------------------------------ */
  int *vreg_to_color = (int *)malloc((size_t)n_vregs * sizeof(int));
  if (vreg_to_color == NULL) {
    liveness_free(liveness);
    cfg_free(cfg);
    return;
  }
  for (int i = 0; i < n_vregs; ++i)
    vreg_to_color[i] = -1;

  /* Build vreg_id → preferred color (= pool index) map by scanning the
   * instruction list.  All occurrences of the same vreg_id must map to the
   * same physical register; any divergence is a compiler bug. */
  for (ListNode_t *n = inst_list; n != NULL; n = n->next) {
    if (n->type != LIST_IR_INST)
      continue;
    const IrInst_t *inst = (const IrInst_t *)n->cur;
    if (inst == NULL)
      continue;
    int total = inst->n_defs + inst->n_uses;
    for (int i = 0; i < total && i < IR_MAX_DEFS + IR_MAX_USES; ++i) {
      int v = inst->vreg_ids[i];
      if (v < 0 || v >= n_vregs)
        continue;
      if (inst->reg_names_64[i][0] == '\0')
        continue;
      /* Find this register's pool index. */
      int c = -1;
      for (int k = 0; k < pool_size; ++k) {
        if (pool_names_64[k] != NULL &&
            strcmp(inst->reg_names_64[i], pool_names_64[k]) == 0) {
          c = k;
          break;
        }
      }
      if (c < 0)
        continue; /* not a pooled register */
      if (vreg_to_color[v] < 0) {
        vreg_to_color[v] = c;
      } else {
        /* Same vreg_id must always map to the same physical register.
         * A mismatch indicates a compiler bug in vreg ID scoping — the
         * register state was not properly reset between nested function
         * compilations, causing stale IDs to leak into the outer
         * function's instruction list. */
        assert(
            vreg_to_color[v] == c &&
            "vreg ID collision: same vreg_id maps to two physical registers");
      }
    }
  }

  /* ------------------------------------------------------------------ */
  /* Step 5: build interference graph                                    */
  /* ------------------------------------------------------------------ */
  InterferenceGraph_t *graph = create_interference_graph(pool_size);
  if (graph == NULL) {
    free(vreg_to_color);
    liveness_free(liveness);
    cfg_free(cfg);
    return;
  }

  LiveRange_t **ranges =
      (LiveRange_t **)calloc((size_t)n_vregs, sizeof(LiveRange_t *));
  if (ranges == NULL) {
    free_interference_graph(graph);
    free(vreg_to_color);
    liveness_free(liveness);
    cfg_free(cfg);
    return;
  }

  /* Create one LiveRange per live vreg (those with a known pool mapping). */
  int ranges_ok = 1;
  for (int v = 0; v < n_vregs; ++v) {
    if (vreg_to_color[v] < 0)
      continue;
    ranges[v] = create_live_range(v, 0, 0);
    if (ranges[v] == NULL) {
      ranges_ok = 0;
      break;
    }
    ranges[v]->preferred_color = vreg_to_color[v];
  }
  if (!ranges_ok) {
    /* Ranges not yet added to graph — free them directly. */
    for (int v = 0; v < n_vregs; ++v)
      free(ranges[v]);
    free(ranges);
    free_interference_graph(graph);
    free(vreg_to_color);
    liveness_free(liveness);
    cfg_free(cfg);
    return;
  }

  /* Now add all successfully created ranges to the graph. */
  for (int v = 0; v < n_vregs; ++v) {
    if (ranges[v] != NULL)
      add_live_range(graph, ranges[v]);
  }

  /* Add interference edges for each block's live_out set (all pairs). */
  for (int b = 0; b < liveness->n_blocks; ++b) {
    const LiveSet_t *lout = &liveness->live_out[b];
    for (int i = 0; i < lout->n_regs; ++i) {
      int vi = lout->vreg_ids[i];
      if (vi < 0 || vi >= n_vregs || ranges[vi] == NULL)
        continue;
      for (int j = i + 1; j < lout->n_regs; ++j) {
        int vj = lout->vreg_ids[j];
        if (vj < 0 || vj >= n_vregs || vj == vi || ranges[vj] == NULL)
          continue;
        add_interference_edge(ranges[vi], ranges[vj]);
      }
    }
    /* Also add interference for live_in pairs (handles values live at
     * block entry that are never defined within the block, e.g. function
     * parameters that flow across multiple blocks). */
    const LiveSet_t *lin = &liveness->live_in[b];
    for (int i = 0; i < lin->n_regs; ++i) {
      int vi = lin->vreg_ids[i];
      if (vi < 0 || vi >= n_vregs || ranges[vi] == NULL)
        continue;
      for (int j = i + 1; j < lin->n_regs; ++j) {
        int vj = lin->vreg_ids[j];
        if (vj < 0 || vj >= n_vregs || vj == vi || ranges[vj] == NULL)
          continue;
        add_interference_edge(ranges[vi], ranges[vj]);
      }
    }
  }

  /* Add interference between co-defs of the same instruction. */
  for (ListNode_t *n = inst_list; n != NULL; n = n->next) {
    if (n->type != LIST_IR_INST)
      continue;
    const IrInst_t *inst = (const IrInst_t *)n->cur;
    if (inst == NULL)
      continue;
    for (int di = 0; di < inst->n_defs; ++di) {
      int d1 = inst->vreg_ids[di];
      if (d1 < 0 || d1 >= n_vregs || ranges[d1] == NULL)
        continue;
      for (int di2 = di + 1; di2 < inst->n_defs; ++di2) {
        int d2 = inst->vreg_ids[di2];
        if (d2 < 0 || d2 >= n_vregs || d2 == d1 || ranges[d2] == NULL)
          continue;
        add_interference_edge(ranges[d1], ranges[d2]);
      }
    }
  }

  /* ------------------------------------------------------------------ */
  /* Step 6: run graph coloring                                          */
  /* ------------------------------------------------------------------ */
  ListNode_t *spilled = allocate_registers_graph_coloring_prebuilt(graph);
  /* The register pool consists solely of the callee-saved registers that
   * the LRU allocator has already pre-assigned.  With preferred colors
   * locked in, graph coloring is effectively just a consistency check and
   * should never produce spills.  A spill here indicates a compiler bug. */
  assert(spilled == NULL && "graph coloring produced spills — impossible with "
                            "pre-allocated callee-saved registers");

  /* ------------------------------------------------------------------ */
  /* Step 7: apply coloring                                              */
  /* ------------------------------------------------------------------ */
  {
    for (ListNode_t *n = inst_list; n != NULL; n = n->next) {
      if (n->type != LIST_IR_INST)
        continue;
      IrInst_t *inst = (IrInst_t *)n->cur;
      if (inst == NULL)
        continue;
      int total = inst->n_defs + inst->n_uses;
      for (int i = 0; i < total && i < IR_MAX_DEFS + IR_MAX_USES; ++i) {
        int v = inst->vreg_ids[i];
        if (v < 0 || v >= n_vregs || ranges[v] == NULL)
          continue;
        int color = ranges[v]->assigned_reg_num;
        if (color < 0 || color >= pool_size)
          continue;
        if (pool_names_64[color] != NULL)
          snprintf(inst->reg_names_64[i], IR_REG_NAME_BUF, "%s",
                   pool_names_64[color]);
        if (pool_names_32[color] != NULL)
          snprintf(inst->reg_names_32[i], IR_REG_NAME_BUF, "%s",
                   pool_names_32[color]);
      }
    }
  }

  /* ------------------------------------------------------------------ */
  /* Step 8: cleanup                                                     */
  /* free_interference_graph frees neighbor lists (sets them to NULL);   */
  /* we then free the LiveRange_t objects via free_live_range.           */
  /* ------------------------------------------------------------------ */
  free_interference_graph(graph);
  for (int v = 0; v < n_vregs; ++v)
    free_live_range(ranges[v]);
  free(ranges);
  free(vreg_to_color);
  liveness_free(liveness);
  cfg_free(cfg);
}
#undef LIVENESS_MAX_POOL
#endif /* USE_GRAPH_COLORING_ALLOCATOR */
