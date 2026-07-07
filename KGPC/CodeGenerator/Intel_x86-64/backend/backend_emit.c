/*
 * backend_emit.c — target-independent instruction-list emission core.
 *
 * Bodies moved verbatim from codegen.c (add_inst, add_inst_invalidate_cache),
 * or de-contextualized from it (be_add_inst_du ← add_inst_du, be_gen_label ←
 * gen_label, be_inst_list_write ← codegen_inst_list): every CodeGenContext
 * reference is replaced by an explicit int* / FILE* parameter.  Debug-only
 * CODEGEN_DEBUG tracing (compiled out unless DEBUG_CODEGEN) was dropped here to
 * keep the core free-standing.
 */
#include "backend_emit.h"

#include "../ir/ir_inst.h"
#include "../stackmng/stackmng.h"

#include "../../../Parser/List/List.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Tail pointer for O(1) add_inst append.
 * Tracks the (head, tail) of the last inst_list built by add_inst.
 * When the same head is passed and the cached tail's ->next is still NULL,
 * we append in O(1) instead of walking the entire list (O(n)).
 * MUST be invalidated before free_inst_list and ConcatList. */
static ListNode_t *g_inst_tail = NULL;
static ListNode_t *g_inst_head = NULL;

void add_inst_invalidate_cache(void) {
  g_inst_tail = NULL;
  g_inst_head = NULL;
}

ListNode_t *add_inst(ListNode_t *inst_list, const char *inst) {
  ListNode_t *new_node;

  assert(inst != NULL);
  new_node = CreateListNode(strdup(inst), LIST_STRING);
  if (inst_list == NULL) {
    inst_list = new_node;
  } else if (g_inst_head == inst_list && g_inst_tail != NULL &&
             g_inst_tail->next == NULL) {
    /* Fast path: cached tail is valid, O(1) append */
    g_inst_tail->next = new_node;
  } else {
    /* Slow path: walk to end */
    PushListNodeBack(inst_list, new_node);
  }
  g_inst_head = inst_list;
  g_inst_tail = new_node;

  return inst_list;
}

/* be_add_inst_du — emit an instruction template with def/use metadata.
 *
 * fmt is a template string where %0, %1, ... are placeholders for the
 * physical register names of the def/use registers.  Placeholders are
 * substituted by ir_emit_function() before code emission.
 *
 * defs[0..n_defs-1] are written by this instruction.
 * uses[0..n_uses-1] are read by this instruction.
 * vreg_ids[] is filled in defs-first order: [defs[0], ..., uses[0], ...].
 * Each register without an assigned vreg_id (vreg_id == -1) receives a fresh
 * ID from *next_vreg_id++ when next_vreg_id is non-NULL. */
/* Shared core (defined below); the public entry points are thin translators
 * over it that fix the `sel` interpretation. */
static ListNode_t *be_add_inst_du_impl(ListNode_t *inst_list, int *next_vreg_id,
                                       Register_t **defs, int n_defs,
                                       Register_t **uses, int n_uses,
                                       const char *fmt, const int *sel,
                                       int sel_is_use32);

ListNode_t *be_add_inst_du(ListNode_t *inst_list, int *next_vreg_id,
                           Register_t **defs, int n_defs, Register_t **uses,
                           int n_uses, const char *fmt) {
  return be_add_inst_du_impl(inst_list, next_vreg_id, defs, n_defs, uses,
                             n_uses, fmt, NULL, 1);
}

ListNode_t *be_add_inst_du_w(ListNode_t *inst_list, int *next_vreg_id,
                             Register_t **defs, int n_defs, Register_t **uses,
                             int n_uses, const char *fmt, const int *use32) {
  /* Legacy semantics: each entry is a boolean (non-zero → 32-bit name).
   * Interpreted by the impl as use32-mode. */
  return be_add_inst_du_impl(inst_list, next_vreg_id, defs, n_defs, uses,
                             n_uses, fmt, use32, 1);
}

ListNode_t *be_add_inst_du_wsel(ListNode_t *inst_list, int *next_vreg_id,
                                Register_t **defs, int n_defs, Register_t **uses,
                                int n_uses, const char *fmt,
                                const int *widthsel) {
  /* Each entry is a direct reg_width_sel code (0..4).  Interpreted by the impl
   * as select-mode. */
  return be_add_inst_du_impl(inst_list, next_vreg_id, defs, n_defs, uses,
                             n_uses, fmt, widthsel, 0);
}

/* Copy the four sub-register name views from `reg` into placeholder slot `ph`.
 * Missing names (NULL) become empty strings, distinguishable from a set name. */
static void be_copy_reg_names(IrInst_t *inst, int ph, Register_t *reg) {
  if (reg->bit_64)
    snprintf(inst->reg_names_64[ph], IR_REG_NAME_BUF, "%s", reg->bit_64);
  else
    inst->reg_names_64[ph][0] = '\0';
  if (reg->bit_32)
    snprintf(inst->reg_names_32[ph], IR_REG_NAME_BUF, "%s", reg->bit_32);
  else
    inst->reg_names_32[ph][0] = '\0';
  if (reg->bit_16)
    snprintf(inst->reg_names_16[ph], IR_REG_NAME_BUF, "%s", reg->bit_16);
  else
    inst->reg_names_16[ph][0] = '\0';
  if (reg->bit_8)
    snprintf(inst->reg_names_8[ph], IR_REG_NAME_BUF, "%s", reg->bit_8);
  else
    inst->reg_names_8[ph][0] = '\0';
}

/* Shared implementation of the def/use emission core.
 *
 * `sel` (nullable) has one entry per placeholder (defs first, then uses).
 * When `sel_is_use32` is non-zero each entry is a boolean (non-zero forces the
 * 32-bit name → width-sel code 2, zero forces 64-bit → code 1); this is the
 * historical be_add_inst_du_w contract.  When `sel_is_use32` is zero each entry
 * is a direct width-sel code (0=heuristic, 1=64, 2=32, 3=16, 4=8).  `sel` is
 * read at exactly the placeholder indices the loops visit, so a NULL `sel`
 * (be_add_inst_du) leaves the calloc-zeroed heuristic default untouched — the
 * compiler's ~1,900 be_add_inst_du call sites are byte-for-byte unchanged. */
static ListNode_t *be_add_inst_du_impl(ListNode_t *inst_list, int *next_vreg_id,
                                       Register_t **defs, int n_defs,
                                       Register_t **uses, int n_uses,
                                       const char *fmt, const int *sel,
                                       int sel_is_use32) {
  IrInst_t *inst = ir_inst_new(NULL, defs, n_defs, uses, n_uses);
  if (inst == NULL)
    return inst_list;

  /* Store template string */
  inst->tmpl = fmt ? strdup(fmt) : NULL;
  inst->text = NULL;

  /* Assign vreg_ids: defs first, then uses.
   * If next_vreg_id is available, assign fresh IDs to unassigned registers.
   * Also copy physical register names (bit_64/bit_32/bit_16/bit_8) into the
   * instruction so that ir_emit_function() can resolve placeholder names
   * without dereferencing the borrowed Register_t pointers (which may be freed
   * by reset_reg_stack() when nested subprograms are codegen'd before
   * ir_emit_function() is called on the outer function). */
  int placeholder = 0;
  for (int i = 0;
       i < n_defs && i < IR_MAX_DEFS &&
       placeholder < (int)(sizeof(inst->vreg_ids) / sizeof(inst->vreg_ids[0]));
       ++i, ++placeholder) {
    if (defs[i] != NULL) {
      if (next_vreg_id != NULL && defs[i]->vreg_id == -1)
        defs[i]->vreg_id = (*next_vreg_id)++;
      inst->vreg_ids[placeholder] = defs[i]->vreg_id;
      be_copy_reg_names(inst, placeholder, defs[i]);
      if (sel != NULL)
        inst->reg_width_sel[placeholder] =
            sel_is_use32 ? (sel[placeholder] ? 2 : 1)
                         : (unsigned char)sel[placeholder];
    }
  }
  for (int i = 0;
       i < n_uses && i < IR_MAX_USES &&
       placeholder < (int)(sizeof(inst->vreg_ids) / sizeof(inst->vreg_ids[0]));
       ++i, ++placeholder) {
    if (uses[i] != NULL) {
      if (next_vreg_id != NULL && uses[i]->vreg_id == -1)
        uses[i]->vreg_id = (*next_vreg_id)++;
      inst->vreg_ids[placeholder] = uses[i]->vreg_id;
      be_copy_reg_names(inst, placeholder, uses[i]);
      if (sel != NULL)
        inst->reg_width_sel[placeholder] =
            sel_is_use32 ? (sel[placeholder] ? 2 : 1)
                         : (unsigned char)sel[placeholder];
    }
  }
  inst->n_placeholders = placeholder;

  ListNode_t *new_node = CreateListNode(inst, LIST_IR_INST);
  if (new_node == NULL) {
    ir_inst_free(inst);
    return inst_list;
  }

  if (inst_list == NULL) {
    inst_list = new_node;
  } else if (g_inst_head == inst_list && g_inst_tail != NULL &&
             g_inst_tail->next == NULL) {
    g_inst_tail->next = new_node;
  } else {
    PushListNodeBack(inst_list, new_node);
  }
  g_inst_head = inst_list;
  g_inst_tail = new_node;

  return inst_list;
}

void be_gen_label(char *buf, int buf_len, int *label_counter) {
  assert(buf != NULL);
  assert(label_counter != NULL);
  snprintf(buf, buf_len, ".L%d", ++(*label_counter));
}

void be_inst_list_write(FILE *out, ListNode_t *inst_list) {
  assert(out != NULL);

  while (inst_list != NULL) {
    const char *text;
    if (inst_list->type == LIST_IR_INST) {
      IrInst_t *ir = (IrInst_t *)inst_list->cur;
      assert(ir != NULL);
      text = ir->text;
    } else {
      text = (const char *)inst_list->cur;
    }
    assert(text != NULL);

    fprintf(out, "%s", text);

    inst_list = inst_list->next;
  }
}
