/*
 * ir_cfg.c — CFG construction from an instruction list.
 *
 * See ir_cfg.h for the design overview.
 */

#include "ir_cfg.h"
#include "ir_inst.h"

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "List.h"

/* Maximum length of an assembly mnemonic string (e.g. "jmpq" = 4 chars). */
#define CFG_MAX_MNEMONIC_LEN 16

/* Maximum length of a branch target label (generous upper bound). */
#define CFG_MAX_TARGET_LEN 256

/* -----------------------------------------------------------------------
 * Internal helpers
 * ----------------------------------------------------------------------- */

/* Return the assembly text of a list node, regardless of its type. */
static const char *get_node_text(const ListNode_t *node) {
  if (node == NULL)
    return NULL;
  if (node->type == LIST_IR_INST) {
    const IrInst_t *inst = (const IrInst_t *)node->cur;
    return inst ? inst->text : NULL;
  }
  return (const char *)node->cur;
}

/* Return non-zero if c is a valid first character of an assembly label
 * as described in the problem spec: [A-Za-z_.] */
static int is_label_first_char(char c) {
  return ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_' ||
          c == '.');
}

/* Return non-zero if c is a valid continuation character of an assembly label:
 * [A-Za-z0-9_.] */
static int is_label_cont_char(char c) {
  return (is_label_first_char(c) || (c >= '0' && c <= '9'));
}

/* Return non-zero if text is a label definition line.
 * Pattern: ^[A-Za-z_.][A-Za-z0-9_.]*: (colon at end of name, at line start). */
static int is_label_def(const char *text) {
  if (text == NULL || !is_label_first_char(text[0]))
    return 0;
  const char *p = text + 1;
  while (*p && is_label_cont_char(*p))
    ++p;
  return (*p == ':');
}

/* Extract the label name (without the trailing colon) from a label definition
 * line.  Returns a heap-allocated copy, or NULL on failure. */
static char *extract_label_name(const char *text) {
  if (text == NULL || !is_label_first_char(text[0]))
    return NULL;
  const char *p = text + 1;
  while (*p && is_label_cont_char(*p))
    ++p;
  if (*p != ':')
    return NULL;
  size_t len = (size_t)(p - text);
  char *name = (char *)malloc(len + 1);
  if (name == NULL)
    return NULL;
  memcpy(name, text, len);
  name[len] = '\0';
  return name;
}

/* Branch kinds used internally. */
typedef enum {
  BR_NONE, /* not a branch — fall-through continues into next block     */
  BR_JMP,  /* unconditional jump  — 1 successor (target)                */
  BR_JCC,  /* conditional  jump   — 2 successors (fall-through + target) */
  BR_RET   /* return              — 0 successors                         */
} BranchKind;

/* Check whether mnem matches any of the conditional branch mnemonics. */
static int is_cond_branch(const char *mnem) {
  static const char *const cond_mnems[] = {"je",  "jne", "jl",  "jle", "jg",
                                           "jge", "jb",  "jbe", "ja",  "jae",
                                           "jz",  "jnz", "jc",  "jnc", NULL};
  for (int i = 0; cond_mnems[i] != NULL; ++i)
    if (strcmp(mnem, cond_mnems[i]) == 0)
      return 1;
  return 0;
}

/* Classify the branch kind of an instruction and (optionally) extract the
 * jump target label into target_out[0..target_max-1].
 * target_out is set to an empty string when there is no label target. */
static BranchKind get_branch_kind(const char *text, char *target_out,
                                  int target_max) {
  if (target_out != NULL && target_max > 0)
    target_out[0] = '\0';

  if (text == NULL || *text == '\0')
    return BR_NONE;

  /* Skip leading whitespace (the assembly indent is a tab). */
  const char *p = text;
  while (*p == '\t' || *p == ' ')
    ++p;

  /* Extract the mnemonic (up to the next whitespace or end-of-string). */
  const char *mnem_start = p;
  while (*p && *p != '\t' && *p != ' ' && *p != '\n')
    ++p;
  size_t mnem_len = (size_t)(p - mnem_start);

  if (mnem_len == 0 || mnem_len >= CFG_MAX_MNEMONIC_LEN)
    return BR_NONE;

  char mnem[CFG_MAX_MNEMONIC_LEN];
  memcpy(mnem, mnem_start, mnem_len);
  mnem[mnem_len] = '\0';

  BranchKind bk;
  if (strcmp(mnem, "ret") == 0 || strcmp(mnem, "retq") == 0)
    return BR_RET;
  else if (strcmp(mnem, "jmp") == 0 || strcmp(mnem, "jmpq") == 0)
    bk = BR_JMP;
  else if (is_cond_branch(mnem))
    bk = BR_JCC;
  else
    return BR_NONE;

  /* Extract the target operand (first non-whitespace token after mnemonic). */
  if (target_out != NULL && target_max > 1) {
    while (*p == '\t' || *p == ' ')
      ++p;
    const char *tgt_start = p;
    while (*p && *p != '\n' && *p != '\t' && *p != ' ')
      ++p;
    size_t tgt_len = (size_t)(p - tgt_start);

    /* Only record the target if it looks like a direct label reference
     * (starts with a valid label first char).  Indirect targets like
     * *%rax are silently ignored (empty target_out). */
    if (tgt_len > 0 && tgt_len < (size_t)target_max &&
        is_label_first_char(tgt_start[0])) {
      memcpy(target_out, tgt_start, tgt_len);
      target_out[tgt_len] = '\0';
    }
  }

  return bk;
}

/* -----------------------------------------------------------------------
 * Block array management
 * ----------------------------------------------------------------------- */

#define INITIAL_BLOCK_CAPACITY 8

typedef struct {
  BasicBlock_t **blocks;
  int n_blocks;
  int capacity;
} BlockArray;

static int block_array_init(BlockArray *ba) {
  ba->n_blocks = 0;
  ba->capacity = INITIAL_BLOCK_CAPACITY;
  ba->blocks =
      (BasicBlock_t **)calloc((size_t)ba->capacity, sizeof(BasicBlock_t *));
  return (ba->blocks != NULL) ? 0 : -1;
}

static int block_array_push(BlockArray *ba, BasicBlock_t *block) {
  if (ba->n_blocks >= ba->capacity) {
    int new_cap = ba->capacity * 2;
    BasicBlock_t **new_arr = (BasicBlock_t **)realloc(
        ba->blocks, (size_t)new_cap * sizeof(BasicBlock_t *));
    if (new_arr == NULL)
      return -1;
    ba->blocks = new_arr;
    ba->capacity = new_cap;
  }
  block->index = ba->n_blocks;
  ba->blocks[ba->n_blocks++] = block;
  return 0;
}

/* Allocate a fresh, zeroed BasicBlock_t. */
static BasicBlock_t *new_block(void) {
  return (BasicBlock_t *)calloc(1, sizeof(BasicBlock_t));
}

/* -----------------------------------------------------------------------
 * Edge helpers
 * ----------------------------------------------------------------------- */

static void add_succ(BasicBlock_t *from, BasicBlock_t *to) {
  if (from == NULL || to == NULL)
    return;
  if (from->n_succs >= CFG_MAX_SUCCS)
    return;
  /* Avoid duplicates. */
  for (int i = 0; i < from->n_succs; ++i)
    if (from->succs[i] == to)
      return;
  from->succs[from->n_succs++] = to;
}

static void add_pred(BasicBlock_t *block, BasicBlock_t *pred) {
  if (block == NULL || pred == NULL)
    return;
  /* Avoid duplicates. */
  for (int i = 0; i < block->n_preds; ++i)
    if (block->preds[i] == pred)
      return;
  /* Grow the predecessors array on demand. */
  if (block->n_preds >= block->preds_cap) {
    int new_cap = (block->preds_cap == 0) ? 4 : block->preds_cap * 2;
    BasicBlock_t **new_preds = (BasicBlock_t **)realloc(
        block->preds, (size_t)new_cap * sizeof(BasicBlock_t *));
    if (new_preds == NULL) {
      /* Out of memory — the predecessor edge is lost, which may produce
       * a suboptimal coloring but is not a correctness error in the
       * emitted assembly (liveness is conservative). */
      return;
    }
    block->preds = new_preds;
    block->preds_cap = new_cap;
  }
  block->preds[block->n_preds++] = pred;
}

/* Find a block by its label.  Returns NULL if not found. */
static BasicBlock_t *find_block_by_label(const Cfg_t *cfg, const char *label) {
  if (cfg == NULL || label == NULL || label[0] == '\0')
    return NULL;
  for (int i = 0; i < cfg->n_blocks; ++i) {
    if (cfg->blocks[i]->label != NULL &&
        strcmp(cfg->blocks[i]->label, label) == 0)
      return cfg->blocks[i];
  }
  return NULL;
}

/* -----------------------------------------------------------------------
 * cfg_build
 * ----------------------------------------------------------------------- */

Cfg_t *cfg_build(ListNode_t *inst_list) {
  if (inst_list == NULL) {
    /* Empty function — return a valid empty CFG. */
    Cfg_t *cfg = (Cfg_t *)calloc(1, sizeof(Cfg_t));
    return cfg;
  }

  BlockArray ba;
  if (block_array_init(&ba) != 0)
    return NULL;

  BasicBlock_t *current = new_block();
  if (current == NULL) {
    free(ba.blocks);
    return NULL;
  }

  ListNode_t *prev_node = NULL;
  int after_branch = 0; /* next instruction starts a new block */

  for (ListNode_t *node = inst_list; node != NULL; node = node->next) {
    const char *text = get_node_text(node);

    /* Determine whether this instruction starts a new basic block. */
    int starts_new = 0;
    if (prev_node != NULL) {
      if (after_branch)
        starts_new = 1;
      else if (text != NULL && is_label_def(text))
        starts_new = 1;
    }

    if (starts_new) {
      /* Finalize the current block. */
      if (current->first_inst != NULL) {
        current->last_inst = prev_node;
        if (block_array_push(&ba, current) != 0) {
          /* Allocation failed — clean up and return what we have. */
          free(current->label);
          free(current->preds);
          free(current);
          goto finish;
        }
      } else {
        /* Empty block (shouldn't normally happen) — discard. */
        free(current->label);
        free(current->preds);
        free(current);
      }

      /* Start a new block. */
      current = new_block();
      if (current == NULL)
        goto finish;
    }

    /* Initialise first_inst on the current block. */
    if (current->first_inst == NULL) {
      current->first_inst = node;
      /* If the first instruction is a label, record it as the block label. */
      if (text != NULL && is_label_def(text))
        current->label = extract_label_name(text);
    }

    /* Classify the current instruction as a branch. */
    char target[CFG_MAX_TARGET_LEN];
    BranchKind bk = get_branch_kind(text, target, (int)sizeof(target));
    after_branch = (bk != BR_NONE);

    prev_node = node;
  }

  /* Finalise the last block. */
  if (current->first_inst != NULL) {
    current->last_inst = prev_node;
    if (block_array_push(&ba, current) != 0) {
      free(current->label);
      free(current->preds);
      free(current);
    }
  } else {
    free(current->label);
    free(current->preds);
    free(current);
  }

finish:;
  /* Build the Cfg_t. */
  Cfg_t *cfg = (Cfg_t *)calloc(1, sizeof(Cfg_t));
  if (cfg == NULL) {
    /* Free all allocated blocks. */
    for (int i = 0; i < ba.n_blocks; ++i) {
      free(ba.blocks[i]->label);
      free(ba.blocks[i]->preds);
      free(ba.blocks[i]);
    }
    free(ba.blocks);
    return NULL;
  }
  cfg->blocks = ba.blocks;
  cfg->n_blocks = ba.n_blocks;

  /* ---------------------------------------------------------------
   * Pass 2: wire successor / predecessor edges.
   * --------------------------------------------------------------- */
  for (int i = 0; i < cfg->n_blocks; ++i) {
    BasicBlock_t *b = cfg->blocks[i];
    if (b->last_inst == NULL)
      continue;

    const char *text = get_node_text(b->last_inst);
    char target[CFG_MAX_TARGET_LEN];
    BranchKind bk = get_branch_kind(text, target, (int)sizeof(target));

    BasicBlock_t *next_block =
        (i + 1 < cfg->n_blocks) ? cfg->blocks[i + 1] : NULL;

    switch (bk) {
    case BR_NONE:
      /* Fall-through to next block. */
      if (next_block != NULL) {
        add_succ(b, next_block);
        add_pred(next_block, b);
      }
      break;

    case BR_JMP: {
      /* Unconditional jump — one successor (the target). */
      BasicBlock_t *tgt = find_block_by_label(cfg, target);
      if (tgt != NULL) {
        add_succ(b, tgt);
        add_pred(tgt, b);
      }
      break;
    }

    case BR_JCC: {
      /* Conditional jump — two successors: fall-through + target. */
      if (next_block != NULL) {
        add_succ(b, next_block);
        add_pred(next_block, b);
      }
      BasicBlock_t *tgt = find_block_by_label(cfg, target);
      if (tgt != NULL) {
        add_succ(b, tgt);
        add_pred(tgt, b);
      }
      break;
    }

    case BR_RET:
      /* No successors. */
      break;
    }
  }

  return cfg;
}

/* -----------------------------------------------------------------------
 * cfg_free
 * ----------------------------------------------------------------------- */

void cfg_free(Cfg_t *cfg) {
  if (cfg == NULL)
    return;
  for (int i = 0; i < cfg->n_blocks; ++i) {
    if (cfg->blocks[i] != NULL) {
      free(cfg->blocks[i]->label);
      free(cfg->blocks[i]->preds);
      free(cfg->blocks[i]);
    }
  }
  free(cfg->blocks);
  free(cfg);
}

/* -----------------------------------------------------------------------
 * cfg_print
 * ----------------------------------------------------------------------- */

/* Return the index of block in cfg, or -1 if not found. */
static int block_index(const Cfg_t *cfg, const BasicBlock_t *block) {
  for (int i = 0; i < cfg->n_blocks; ++i)
    if (cfg->blocks[i] == block)
      return i;
  return -1;
}

void cfg_print(FILE *out, const Cfg_t *cfg, const char *fn_name) {
  assert(out != NULL);
  if (cfg == NULL)
    return;

  fprintf(out, "; --- CFG: %s ---\n", fn_name ? fn_name : "<unknown>");

  for (int i = 0; i < cfg->n_blocks; ++i) {
    const BasicBlock_t *b = cfg->blocks[i];

    /* Block header. */
    if (b->label != NULL)
      fprintf(out, "; BB%d (%s)", i, b->label);
    else
      fprintf(out, "; BB%d (entry)", i);

    /* Predecessors. */
    if (b->n_preds == 0) {
      fprintf(out, "  preds=[]");
    } else {
      fprintf(out, "  preds=[");
      for (int p = 0; p < b->n_preds; ++p) {
        int idx = block_index(cfg, b->preds[p]);
        fprintf(out, "%sBB%d", p > 0 ? " " : "", idx);
      }
      fprintf(out, "]");
    }

    /* Successors. */
    if (b->n_succs == 0) {
      fprintf(out, "  succs=[]\n");
    } else {
      fprintf(out, "  succs=[");
      for (int s = 0; s < b->n_succs; ++s) {
        int idx = block_index(cfg, b->succs[s]);
        fprintf(out, "%sBB%d", s > 0 ? " " : "", idx);
      }
      fprintf(out, "]\n");
    }

    /* Print instructions of this block (prefixed with "; | "). */
    for (ListNode_t *node = b->first_inst; node != NULL; node = node->next) {
      const char *text = get_node_text(node);
      if (text != NULL) {
        /* Strip trailing newline for cleaner output. */
        size_t len = strlen(text);
        if (len > 0 && text[len - 1] == '\n')
          fprintf(out, "; | %.*s\n", (int)(len - 1), text);
        else
          fprintf(out, "; | %s\n", text);
      }
      if (node == b->last_inst)
        break;
    }
  }

  fprintf(out, "; --- End CFG: %s ---\n", fn_name ? fn_name : "<unknown>");
}
