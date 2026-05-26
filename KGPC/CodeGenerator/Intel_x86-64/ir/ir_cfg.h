/*
 * ir_cfg.h — Control-flow graph (CFG) built from an instruction list.
 *
 * One pass over inst_list identifies basic-block boundaries:
 *   - Function entry (first instruction)
 *   - Any label definition  (pattern ^[A-Za-z_.][A-Za-z0-9_.]*:)
 *   - Any instruction that immediately follows a branch
 *
 * Branch classification:
 *   - jmp <target>              → ends block, 1 successor (target)
 *   - jcc <target>              → ends block, 2 successors (fall-through +
 * target)
 *   - ret                       → ends block, 0 successors
 *   - call / everything else    → fall-through only (not a block terminator)
 *
 * The inst_list nodes are NOT owned by the CFG; cfg_free() does not touch them.
 */

#ifndef IR_CFG_H
#define IR_CFG_H

#include <stdio.h>

/* Forward declaration — full definition lives in List.h */
typedef struct List ListNode_t;

#define CFG_MAX_SUCCS 2
/* Predecessors are tracked in a heap-allocated array that grows on demand,
 * so there is no fixed upper limit. */

typedef struct BasicBlock {
  char *label;            /* block label (no colon), NULL for entry */
  ListNode_t *first_inst; /* first ListNode_t of this block (borrowed) */
  ListNode_t *last_inst;  /* last  ListNode_t of this block (borrowed) */
  struct BasicBlock *succs[CFG_MAX_SUCCS];
  int n_succs;
  struct BasicBlock **preds; /* heap-allocated, grown on demand */
  int n_preds;
  int preds_cap;
  int index; /* position in cfg->blocks[] array */
} BasicBlock_t;

typedef struct Cfg {
  BasicBlock_t **blocks;
  int n_blocks;
} Cfg_t;

/* Build a CFG from the instruction list produced by a single function's
 * codegen pass.  Returns NULL only on allocation failure or empty input.
 * Must not crash on any input. */
Cfg_t *cfg_build(ListNode_t *inst_list);

/* Free a CFG.  Does NOT free the inst_list nodes (they are borrowed). */
void cfg_free(Cfg_t *cfg);

/* Print the CFG in human-readable form to out.
 * fn_name identifies the function (for the header comment). */
void cfg_print(FILE *out, const Cfg_t *cfg, const char *fn_name);

#endif /* IR_CFG_H */
