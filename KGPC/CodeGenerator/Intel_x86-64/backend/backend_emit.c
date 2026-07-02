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
ListNode_t *be_add_inst_du(ListNode_t *inst_list, int *next_vreg_id,
                           Register_t **defs, int n_defs, Register_t **uses,
                           int n_uses, const char *fmt) {
  IrInst_t *inst = ir_inst_new(NULL, defs, n_defs, uses, n_uses);
  if (inst == NULL)
    return inst_list;

  /* Store template string */
  inst->tmpl = fmt ? strdup(fmt) : NULL;
  inst->text = NULL;

  /* Assign vreg_ids: defs first, then uses.
   * If next_vreg_id is available, assign fresh IDs to unassigned registers.
   * Also copy physical register names (bit_64/bit_32) into the instruction
   * so that ir_emit_function() can resolve placeholder names without
   * dereferencing the borrowed Register_t pointers (which may be freed by
   * reset_reg_stack() when nested subprograms are codegen'd before
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
      if (defs[i]->bit_64)
        snprintf(inst->reg_names_64[placeholder], IR_REG_NAME_BUF, "%s",
                 defs[i]->bit_64);
      else
        inst->reg_names_64[placeholder][0] = '\0';
      if (defs[i]->bit_32)
        snprintf(inst->reg_names_32[placeholder], IR_REG_NAME_BUF, "%s",
                 defs[i]->bit_32);
      else
        inst->reg_names_32[placeholder][0] = '\0';
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
      if (uses[i]->bit_64)
        snprintf(inst->reg_names_64[placeholder], IR_REG_NAME_BUF, "%s",
                 uses[i]->bit_64);
      else
        inst->reg_names_64[placeholder][0] = '\0';
      if (uses[i]->bit_32)
        snprintf(inst->reg_names_32[placeholder], IR_REG_NAME_BUF, "%s",
                 uses[i]->bit_32);
      else
        inst->reg_names_32[placeholder][0] = '\0';
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
