/*
 * ir_peephole.c — Post-allocation peephole optimization.
 *
 * See ir_peephole.h for the design overview.
 *
 * Pattern matched (after physical-register substitution):
 *     <tab>mov{q,l,w,b}<tab>%REG, %REG<newline>
 *
 * The mnemonic must be exactly one of mov, movq, movl, movw, movb (i.e. a
 * plain register move with no addressing modes).  Any operand containing
 * parentheses, displacements, or immediate prefixes is left untouched so
 * that loads/stores/leas are never mistaken for redundant moves.
 *
 * Implementation notes:
 *   - The list head may itself be a redundant move; the head pointer is
 *     updated through inst_list_ptr.
 *   - Per-node text is owned by IrInst_t (LIST_IR_INST) or is a plain
 *     char* (LIST_STRING).  Removal frees the payload via the type-
 *     appropriate destructor and then frees the list node itself.
 *   - The pass is intentionally single-pass; consecutive redundant moves
 *     are all removed because we keep `prev` rather than advancing it
 *     past a deleted node.
 */

#include "ir_peephole.h"
#include "ir_inst.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "List.h"

/* Return the assembly text of a list node, regardless of its type. */
static const char *peephole_node_text(const ListNode_t *node) {
  if (node == NULL)
    return NULL;
  if (node->type == LIST_IR_INST) {
    const IrInst_t *inst = (const IrInst_t *)node->cur;
    return inst != NULL ? inst->text : NULL;
  }
  return (const char *)node->cur;
}

/* Free the node's payload and the node itself. */
static void peephole_free_node(ListNode_t *node) {
  if (node == NULL)
    return;
  if (node->type == LIST_IR_INST)
    ir_inst_free((IrInst_t *)node->cur);
  else
    free(node->cur);
  free(node);
}

/* Skip past tabs/spaces.  Returns the new pointer (never NULL if input
 * was non-NULL — it may point at '\0'). */
static const char *skip_blanks(const char *p) {
  while (*p == ' ' || *p == '\t')
    ++p;
  return p;
}

/* Test whether two non-NULL register-operand spans are byte-for-byte equal.
 * A register operand here is `%name` (always starts with `%`). */
static int reg_spans_equal(const char *a, size_t alen, const char *b,
                           size_t blen) {
  return alen == blen && alen > 0 && memcmp(a, b, alen) == 0;
}

/* If text matches a redundant register-to-register mov, return 1; otherwise 0.
 *
 * The match is strict:
 *   start_of_line<tab>mov[qlwb]?<tab>%<reg>,<optional spaces>%<reg><newline>
 *
 * A register operand is exactly the literal byte '%' followed by one or
 * more alphanumeric characters; any other character (e.g. '(' for memory
 * addressing or '$' for immediates) disqualifies the operand. */
static int text_is_redundant_self_mov(const char *text) {
  if (text == NULL)
    return 0;

  /* Required leading tab — the assembler indent used by every emitter. */
  if (*text != '\t')
    return 0;
  const char *p = text + 1;

  /* Mnemonic must be one of "mov", "movq", "movl", "movw", "movb". */
  if (p[0] != 'm' || p[1] != 'o' || p[2] != 'v')
    return 0;
  p += 3;
  /* Optional size suffix. */
  if (*p == 'q' || *p == 'l' || *p == 'w' || *p == 'b')
    ++p;
  /* Must be followed by whitespace (tab or space). */
  if (*p != '\t' && *p != ' ')
    return 0;
  p = skip_blanks(p);

  /* First operand: %REG. */
  if (*p != '%')
    return 0;
  const char *src_start = p;
  ++p;
  while (isalnum((unsigned char)*p))
    ++p;
  size_t src_len = (size_t)(p - src_start);
  if (src_len < 2) /* must be at least "%X" */
    return 0;

  /* Separator: comma + optional whitespace. */
  if (*p != ',')
    return 0;
  ++p;
  p = skip_blanks(p);

  /* Second operand: %REG. */
  if (*p != '%')
    return 0;
  const char *dst_start = p;
  ++p;
  while (isalnum((unsigned char)*p))
    ++p;
  size_t dst_len = (size_t)(p - dst_start);
  if (dst_len < 2)
    return 0;

  /* End-of-line: optional whitespace then '\n' or '\0'. */
  p = skip_blanks(p);
  if (*p != '\n' && *p != '\0')
    return 0;

  return reg_spans_equal(src_start, src_len, dst_start, dst_len);
}

int ir_peephole_remove_redundant_moves(ListNode_t **inst_list_ptr) {
  if (inst_list_ptr == NULL || *inst_list_ptr == NULL)
    return 0;

  int removed = 0;
  ListNode_t *prev = NULL;
  ListNode_t *cur = *inst_list_ptr;

  while (cur != NULL) {
    const char *text = peephole_node_text(cur);
    if (text_is_redundant_self_mov(text)) {
      ListNode_t *to_free = cur;
      cur = cur->next;
      if (prev == NULL)
        *inst_list_ptr = cur;
      else
        prev->next = cur;
      peephole_free_node(to_free);
      ++removed;
      /* `prev` intentionally not advanced so back-to-back
       * redundant moves all get caught in one pass. */
    } else {
      prev = cur;
      cur = cur->next;
    }
  }

  return removed;
}
