/*
 * ir_liveness.h — Per-block liveness sets computed by backwards dataflow.
 *
 * After cfg_build() produces a Cfg_t, call liveness_compute() to compute
 * live-in / live-out virtual-register ID sets at every basic block.  This is
 * pure metadata: no effect on emitted assembly.
 *
 * Register identity uses the integer vreg_id field copied into IrInst_t at
 * add_inst_du() time.  Entries with vreg_id == -1 are ignored (no register).
 * LIST_STRING nodes in the instruction list carry no def/use metadata and are
 * treated as having empty def and use sets.
 */

#ifndef IR_LIVENESS_H
#define IR_LIVENESS_H

#include <stdio.h>

/* Forward declarations */
typedef struct Cfg Cfg_t;

/* Per-block set of live virtual-register IDs (integers). */
typedef struct LiveSet {
  int *vreg_ids; /* array of virtual register IDs (integers) */
  int n_regs;    /* number of elements currently in the set  */
  int cap;       /* allocated capacity of vreg_ids[]         */
} LiveSet_t;

/* Liveness information for an entire function. */
typedef struct LivenessInfo {
  LiveSet_t *live_in; /* one LiveSet_t per block, indexed same as cfg->blocks */
  LiveSet_t
      *live_out; /* one LiveSet_t per block, indexed same as cfg->blocks */
  int n_blocks;  /* must equal cfg->n_blocks                             */
} LivenessInfo_t;

/* Compute live-in / live-out sets for every block in cfg using the standard
 * backwards dataflow worklist algorithm.  Returns NULL only on allocation
 * failure.  A CFG with zero blocks returns a valid empty LivenessInfo_t. */
LivenessInfo_t *liveness_compute(const Cfg_t *cfg);

/* Free a LivenessInfo_t returned by liveness_compute(). */
void liveness_free(LivenessInfo_t *info);

/* Print live-in / live-out annotations for every block to out.
 * fn_name is used only for the header/footer comment line. */
void liveness_print(FILE *out, const Cfg_t *cfg, const LivenessInfo_t *info,
                    const char *fn_name);

#endif /* IR_LIVENESS_H */
