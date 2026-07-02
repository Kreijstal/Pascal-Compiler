/*
 * ir_inst.h — IR instruction with def/use metadata.
 *
 * Each emitted instruction is wrapped in IrInst_t, which carries the
 * pre-formatted assembly text (unchanged) plus arrays of the virtual
 * registers it writes (defs) and reads (uses).
 *
 * The defs/uses pointers are BORROWED from the live register allocator.
 * They are never freed by ir_inst_free().  Parsed instances that create
 * synthetic Register_t objects must set owns_regs = 1 so the free helper
 * will release them.
 */

#ifndef IR_INST_H
#define IR_INST_H

#include <stdio.h>

/* Forward-declare Register_t so callers that include this header without
 * stackmng.h can still compile.  The full definition lives in stackmng.h. */
typedef struct Register Register_t;

/* ListNode_t forward declaration */
typedef struct List ListNode_t;

#define IR_MAX_DEFS 2
#define IR_MAX_USES 4

typedef struct IrInst {
  /* pre-formatted assembly text — identical to the string stored by
   * add_inst().  Owned by this struct; freed by ir_inst_free(). */
  char *text;

  /* def/use metadata (pure annotation, no effect on code generation) */
  Register_t *defs[IR_MAX_DEFS];
  int n_defs;
  Register_t *uses[IR_MAX_USES];
  int n_uses;

  /* When non-zero, this instance owns its Register_t objects (used by
   * ir_parse() which creates synthetic register nodes). */
  int owns_regs;

  /* Template fields for future virtual-register substitution.
   * tmpl is a format string like "\tmovq\t%0, %1\n" where %N refers to
   * vreg_ids[N].  When tmpl is NULL the instruction is emitted verbatim
   * from text.  ir_emit_function() substitutes physical register names
   * and writes the result back into text. */
  char *tmpl; /* format template; NULL = no substitution */
  int vreg_ids[IR_MAX_DEFS +
               IR_MAX_USES]; /* which vreg_id maps to %0, %1, ... */
  int n_placeholders;

  /* Physical register name copies for each placeholder, in the same order
   * as vreg_ids[].  Copied from the Register_t at add_inst_du() time so
   * that ir_emit_function() does not need to dereference the borrowed
   * defs[]/uses[] pointers (which may have been freed by reset_reg_stack()
   * when nested subprograms are codegen'd before ir_emit_function runs).
   * Fixed-size inline buffers avoid heap allocation overhead for pp.pas-
   * scale compilations (hundreds of thousands of add_inst_du calls). */
#define IR_REG_NAME_BUF 12
  char reg_names_64[IR_MAX_DEFS + IR_MAX_USES][IR_REG_NAME_BUF];
  char reg_names_32[IR_MAX_DEFS + IR_MAX_USES][IR_REG_NAME_BUF];

  /* Per-placeholder register-width selector (0 = use the mnemonic-suffix
   * heuristic, the x86 default; 1 = force 64-bit name; 2 = force 32-bit name).
   * calloc-zeroed, so the legacy heuristic path is the default and unchanged.
   * A target that does not encode width in the mnemonic (e.g. AArch64, where
   * width is the register name w0 vs x0) sets this explicitly via
   * be_add_inst_du_w(). */
  unsigned char reg_width_sel[IR_MAX_DEFS + IR_MAX_USES];
} IrInst_t;

/* Allocate and initialise a new IrInst_t.
 * text is strdup'd.  The defs/uses arrays are filled from the supplied
 * pointers.  owns_regs is set to 0 (borrowed pointers). */
IrInst_t *ir_inst_new(const char *text, Register_t **defs, int n_defs,
                      Register_t **uses, int n_uses);

/* Free an IrInst_t.  If owns_regs is set, frees the Register_t objects
 * in defs[] and uses[] as well. */
void ir_inst_free(IrInst_t *inst);

/* -----------------------------------------------------------------------
 * Serialisation
 * ----------------------------------------------------------------------- */

/* Print a single annotated instruction to out.
 * Format:
 *   <asm text (no trailing newline)>  ; def: A B, use: C D\n
 * When there are no defs/uses, the annotation is omitted and the text is
 * printed verbatim (which already contains its trailing newline). */
void ir_print_inst(FILE *out, const IrInst_t *inst);

/* Print all instructions in inst_list for function fn_name to out.
 * inst_list nodes may be LIST_STRING (plain char*) or LIST_IR_INST
 * (IrInst_t*).  The function emits a header/footer comment block. */
void ir_print_function(FILE *out, const char *fn_name, ListNode_t *inst_list);

/* -----------------------------------------------------------------------
 * Deserialisation
 * ----------------------------------------------------------------------- */

/* Parse the serialised text (as produced by ir_print_function) back into
 * a linked list of LIST_IR_INST nodes.  Synthetic Register_t objects are
 * created for each named register in the def/use annotation; owns_regs is
 * set to 1 on such nodes so ir_inst_free releases them.
 *
 * Returns NULL on allocation failure or empty input.
 * The round-trip property ir_print(ir_parse(s)) == s holds for all
 * well-formed input produced by ir_print_function. */
ListNode_t *ir_parse(const char *text);

/* Free a list returned by ir_parse().  Handles LIST_IR_INST payloads. */
void ir_free_parsed_list(ListNode_t *list);

/* Substitute %0, %1, ... placeholders in tmpl with physical register names
 * and write the result into inst->text (freeing the old text).
 * Instructions without a tmpl are left unchanged.
 * Called once per function after all instructions have been emitted. */
void ir_emit_function(ListNode_t *inst_list);

#endif /* IR_INST_H */
