/*
 * ir_liveness.c — Liveness analysis over a CFG.
 *
 * See ir_liveness.h for the design overview.
 *
 * Algorithm: standard backwards dataflow worklist, iterated to fixpoint.
 *
 *   For each block B (iterated in reverse order):
 *     live_out[B] = union of live_in[succ]  for each successor
 *     live_in[B]  = use[B] union (live_out[B] \ def[B])
 *
 * use[B] = vreg IDs used before their first definition in B
 * def[B] = vreg IDs defined in B
 * Both are derived from IrInst_t.vreg_ids[] metadata on LIST_IR_INST nodes.
 * vreg_ids[0..n_defs-1] are defs; vreg_ids[n_defs..n_defs+n_uses-1] are uses.
 * A slot with vreg_id == -1 carries no register and is skipped.
 * LIST_STRING nodes carry no metadata and are skipped.
 */

#include "ir_liveness.h"
#include "ir_inst.h"
#include "ir_cfg.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

/* Pull in full type definitions. */
#include "List.h"

/* -----------------------------------------------------------------------
 * Internal: dynamic set of integer vreg IDs
 *
 * We reuse LiveSet_t directly (n_regs = count, cap = capacity).
 * ----------------------------------------------------------------------- */

#define RSET_INIT_CAP 4

static int rset_init(LiveSet_t *s) {
  s->n_regs = 0;
  s->cap = RSET_INIT_CAP;
  s->vreg_ids = (int *)malloc((size_t)s->cap * sizeof(int));
  return (s->vreg_ids != NULL) ? 0 : -1;
}

static void rset_fini(LiveSet_t *s) {
  free(s->vreg_ids);
  s->vreg_ids = NULL;
  s->n_regs = 0;
  s->cap = 0;
}

static int rset_contains(const LiveSet_t *s, int id) {
  for (int i = 0; i < s->n_regs; ++i)
    if (s->vreg_ids[i] == id)
      return 1;
  return 0;
}

/* Add id (must be >= 0) to s if not already present.  Returns 0 on success. */
static int rset_add(LiveSet_t *s, int id) {
  if (id < 0 || rset_contains(s, id))
    return 0;
  if (s->n_regs >= s->cap) {
    int nc = s->cap * 2;
    int *a = (int *)realloc(s->vreg_ids, (size_t)nc * sizeof(int));
    if (a == NULL)
      return -1;
    s->vreg_ids = a;
    s->cap = nc;
  }
  s->vreg_ids[s->n_regs++] = id;
  return 0;
}

/* dst |= src.  Returns 1 if at least one new element was added. */
static int rset_union_into(LiveSet_t *dst, const LiveSet_t *src) {
  int added = 0;
  for (int i = 0; i < src->n_regs; ++i) {
    if (!rset_contains(dst, src->vreg_ids[i])) {
      if (rset_add(dst, src->vreg_ids[i]) == 0)
        added = 1;
    }
  }
  return added;
}

/* Returns 1 iff a and b contain exactly the same integer IDs. */
static int rset_equal(const LiveSet_t *a, const LiveSet_t *b) {
  if (a->n_regs != b->n_regs)
    return 0;
  for (int i = 0; i < a->n_regs; ++i)
    if (!rset_contains(b, a->vreg_ids[i]))
      return 0;
  return 1;
}

/* Copy src into dst (overwrites dst's content; dst must already be
 * initialised). Returns 0 on success, -1 on allocation failure. */
static int rset_copy(LiveSet_t *dst, const LiveSet_t *src) {
  if (src->n_regs > dst->cap) {
    int *a = (int *)realloc(dst->vreg_ids, (size_t)src->n_regs * sizeof(int));
    if (a == NULL)
      return -1;
    dst->vreg_ids = a;
    dst->cap = src->n_regs;
  }
  dst->n_regs = src->n_regs;
  if (src->n_regs > 0)
    memcpy(dst->vreg_ids, src->vreg_ids, (size_t)src->n_regs * sizeof(int));
  return 0;
}

static void rset_clear(LiveSet_t *s) { s->n_regs = 0; }

/* -----------------------------------------------------------------------
 * Per-block def/use computation
 * ----------------------------------------------------------------------- */

/* Populate def_out and use_out for one basic block by scanning its
 * LIST_IR_INST nodes from first_inst to last_inst (inclusive).
 * Both sets must already be initialised (empty) by the caller.
 *
 * IrInst_t.vreg_ids layout (set by add_inst_du()):
 *   [0 .. n_defs-1]              → def slots
 *   [n_defs .. n_defs+n_uses-1]  → use slots
 * A slot with value -1 carries no register and is skipped. */
static void compute_def_use(const BasicBlock_t *block, LiveSet_t *def_out,
                            LiveSet_t *use_out) {
  for (ListNode_t *node = block->first_inst; node != NULL; node = node->next) {
    if (node->type == LIST_IR_INST) {
      const IrInst_t *inst = (const IrInst_t *)node->cur;
      if (inst != NULL) {
        /* Uses: vreg IDs read before they are defined in this block. */
        for (int i = 0; i < inst->n_uses; ++i) {
          int id = inst->vreg_ids[inst->n_defs + i];
          if (id >= 0 && !rset_contains(def_out, id))
            rset_add(use_out, id);
        }
        /* Defs: vreg IDs written in this block. */
        for (int i = 0; i < inst->n_defs; ++i) {
          int id = inst->vreg_ids[i];
          if (id >= 0)
            rset_add(def_out, id);
        }
      }
    }
    if (node == block->last_inst)
      break;
  }
}

/* -----------------------------------------------------------------------
 * liveness_compute
 * ----------------------------------------------------------------------- */

LivenessInfo_t *liveness_compute(const Cfg_t *cfg) {
  if (cfg == NULL)
    return NULL;

  LivenessInfo_t *info = (LivenessInfo_t *)calloc(1, sizeof(LivenessInfo_t));
  if (info == NULL)
    return NULL;

  if (cfg->n_blocks == 0) {
    info->n_blocks = 0;
    return info;
  }

  int n = cfg->n_blocks;
  info->n_blocks = n;

  info->live_in = (LiveSet_t *)calloc((size_t)n, sizeof(LiveSet_t));
  info->live_out = (LiveSet_t *)calloc((size_t)n, sizeof(LiveSet_t));
  if (info->live_in == NULL || info->live_out == NULL)
    goto oom_info;

  /* Temporary per-block def and use sets. */
  LiveSet_t *def_sets = (LiveSet_t *)calloc((size_t)n, sizeof(LiveSet_t));
  LiveSet_t *use_sets = (LiveSet_t *)calloc((size_t)n, sizeof(LiveSet_t));
  if (def_sets == NULL || use_sets == NULL) {
    free(def_sets);
    free(use_sets);
    goto oom_info;
  }

  /* Initialise all sets. */
  int ok = 1;
  for (int i = 0; i < n; ++i) {
    ok = ok & (rset_init(&info->live_in[i]) == 0);
    ok = ok & (rset_init(&info->live_out[i]) == 0);
    ok = ok & (rset_init(&def_sets[i]) == 0);
    ok = ok & (rset_init(&use_sets[i]) == 0);
  }
  if (!ok) {
    for (int i = 0; i < n; ++i) {
      rset_fini(&info->live_in[i]);
      rset_fini(&info->live_out[i]);
      rset_fini(&def_sets[i]);
      rset_fini(&use_sets[i]);
    }
    free(def_sets);
    free(use_sets);
    goto oom_info;
  }

  /* Compute per-block def/use from IrInst_t metadata. */
  for (int i = 0; i < n; ++i)
    compute_def_use(cfg->blocks[i], &def_sets[i], &use_sets[i]);

  /* Scratch sets for one iteration — zero-initialised so cleanup is safe
   * even when rset_init() fails part-way. */
  LiveSet_t new_out = {NULL, 0, 0};
  LiveSet_t new_in = {NULL, 0, 0};

  if (rset_init(&new_out) != 0 || rset_init(&new_in) != 0) {
    rset_fini(&new_out);
    rset_fini(&new_in);
    for (int i = 0; i < n; ++i) {
      rset_fini(&info->live_in[i]);
      rset_fini(&info->live_out[i]);
      rset_fini(&def_sets[i]);
      rset_fini(&use_sets[i]);
    }
    free(def_sets);
    free(use_sets);
    goto oom_info;
  }

  /* Iterate to fixpoint (reverse order = backward dataflow). */
  int changed = 1;
  while (changed) {
    changed = 0;
    for (int i = n - 1; i >= 0; --i) {
      const BasicBlock_t *b = cfg->blocks[i];

      /* new_out[B] = union of live_in[succ] for each successor of B. */
      rset_clear(&new_out);
      for (int s = 0; s < b->n_succs; ++s) {
        const BasicBlock_t *succ = b->succs[s];
        rset_union_into(&new_out, &info->live_in[succ->index]);
      }

      /* new_in[B] = use[B] | (new_out \ def[B]) */
      rset_clear(&new_in);
      rset_union_into(&new_in, &use_sets[i]);
      for (int k = 0; k < new_out.n_regs; ++k) {
        int id = new_out.vreg_ids[k];
        if (!rset_contains(&def_sets[i], id))
          rset_add(&new_in, id);
      }

      /* Detect change and update sets. */
      if (!rset_equal(&new_out, &info->live_out[i]) ||
          !rset_equal(&new_in, &info->live_in[i])) {
        rset_copy(&info->live_out[i], &new_out);
        rset_copy(&info->live_in[i], &new_in);
        changed = 1;
      }
    }
  }

  rset_fini(&new_out);
  rset_fini(&new_in);

  /* Free temporary def/use sets. */
  for (int i = 0; i < n; ++i) {
    rset_fini(&def_sets[i]);
    rset_fini(&use_sets[i]);
  }
  free(def_sets);
  free(use_sets);

  return info;

oom_info:
  free(info->live_in);
  free(info->live_out);
  free(info);
  return NULL;
}

/* -----------------------------------------------------------------------
 * liveness_free
 * ----------------------------------------------------------------------- */

void liveness_free(LivenessInfo_t *info) {
  if (info == NULL)
    return;
  if (info->live_in != NULL) {
    for (int i = 0; i < info->n_blocks; ++i)
      free(info->live_in[i].vreg_ids);
    free(info->live_in);
  }
  if (info->live_out != NULL) {
    for (int i = 0; i < info->n_blocks; ++i)
      free(info->live_out[i].vreg_ids);
    free(info->live_out);
  }
  free(info);
}

/* -----------------------------------------------------------------------
 * liveness_print
 * ----------------------------------------------------------------------- */

static void print_liveset(FILE *out, const LiveSet_t *ls) {
  fputc('[', out);
  for (int i = 0; i < ls->n_regs; ++i) {
    if (i > 0)
      fputc(' ', out);
    fprintf(out, "v%d", ls->vreg_ids[i]);
  }
  fputc(']', out);
}

void liveness_print(FILE *out, const Cfg_t *cfg, const LivenessInfo_t *info,
                    const char *fn_name) {
  assert(out != NULL);
  if (cfg == NULL || info == NULL)
    return;

  fprintf(out, "; --- Liveness: %s ---\n", fn_name ? fn_name : "<unknown>");

  int limit = cfg->n_blocks < info->n_blocks ? cfg->n_blocks : info->n_blocks;
  for (int i = 0; i < limit; ++i) {
    const BasicBlock_t *b = cfg->blocks[i];
    const LiveSet_t *lin = &info->live_in[i];
    const LiveSet_t *lout = &info->live_out[i];

    /* Block label. */
    if (b->label != NULL)
      fprintf(out, "; BB%d (%s)", i, b->label);
    else
      fprintf(out, "; BB%d (entry)", i);

    fputs("  live_in=", out);
    print_liveset(out, lin);
    fputs("  live_out=", out);
    print_liveset(out, lout);
    fputc('\n', out);
  }

  fprintf(out, "; --- End Liveness: %s ---\n", fn_name ? fn_name : "<unknown>");
}
