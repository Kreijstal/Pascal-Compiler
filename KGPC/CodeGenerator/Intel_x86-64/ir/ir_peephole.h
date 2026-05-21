/*
 * ir_peephole.h — Post-allocation peephole optimization pass.
 *
 * Runs after ir_emit_function() has substituted physical register names
 * into each LIST_IR_INST node's text.  Walks the instruction list and
 * removes pure redundant register-to-register move instructions of the
 * form
 *     \tmov{q,l,w,b}\t%REG, %REG\n
 * (i.e. mov where the source and destination registers are identical).
 *
 * On x86-64 a mov rN, rN has no architectural effect: it does not modify
 * flags, and the destination value is unchanged.  Removing it is safe
 * regardless of surrounding code.
 *
 * The pass operates on both LIST_IR_INST and LIST_STRING nodes — it parses
 * the textual operands rather than inspecting metadata, so it catches
 * redundant moves emitted via add_inst() as well as add_inst_du().  Nodes
 * removed have their payload freed (ir_inst_free or free(), as
 * appropriate) so the rest of the codegen pipeline sees no dangling
 * pointers.
 *
 * The instruction list head is updated through *inst_list_ptr so that
 * callers can keep using their existing local pointer variable.
 */

#ifndef IR_PEEPHOLE_H
#define IR_PEEPHOLE_H

/* Forward declaration — full definition lives in List.h */
typedef struct List ListNode_t;

/* Scan *inst_list_ptr in place and remove every node whose text is an
 * x86-64 register-to-register move with identical source and destination.
 * Returns the number of instructions removed (>= 0).  Safe to call with a
 * NULL pointer or an empty list (returns 0). */
int ir_peephole_remove_redundant_moves(ListNode_t **inst_list_ptr);

#endif /* IR_PEEPHOLE_H */
