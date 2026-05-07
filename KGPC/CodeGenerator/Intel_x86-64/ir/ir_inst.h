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
    /* Final asm text written at emit time.  NULL until ir_emit_function()
     * substitutes the template; set directly by add_inst_du() until call
     * sites have been migrated to the template API. */
    char *text;

    /* Printf-style template with %N placeholders for virtual registers.
     * Example: "\tmovq\t%0, %1\n"  (%0 = first register, %1 = second).
     * NULL for instructions created by add_inst() with no def/use metadata.
     * When non-NULL, ir_emit_function() fills text by substituting %N. */
    char *tmpl;

    /* def/use metadata (pure annotation, no effect on code generation) */
    Register_t *defs[IR_MAX_DEFS];
    int         n_defs;
    Register_t *uses[IR_MAX_USES];
    int         n_uses;

    /* vreg_ids[i] is the vreg_id of the register referenced by %i in tmpl.
     * Ordering: defs[0..n_defs-1] then uses[0..n_uses-1].
     * -1 for slots not in use. */
    int vreg_ids[IR_MAX_DEFS + IR_MAX_USES];

    /* When non-zero, this instance owns its Register_t objects (used by
     * ir_parse() which creates synthetic register nodes). */
    int owns_regs;
} IrInst_t;

/* Allocate and initialise a new IrInst_t.
 * text is strdup'd (may be NULL when a template is used instead).
 * tmpl is strdup'd (may be NULL when text is pre-formatted).
 * The defs/uses arrays are filled from the supplied pointers.
 * vreg_ids[] is copied from the supplied array (may be NULL → filled with -1).
 * owns_regs is set to 0 (borrowed pointers). */
IrInst_t *ir_inst_new(const char *text,
                      const char *tmpl,
                      Register_t **defs, int n_defs,
                      Register_t **uses, int n_uses,
                      const int *vreg_ids, int n_vreg_ids);

/* Free an IrInst_t.  If owns_regs is set, frees the Register_t objects
 * in defs[] and uses[] as well. */
void ir_inst_free(IrInst_t *inst);

/* -----------------------------------------------------------------------
 * Template substitution
 * ----------------------------------------------------------------------- */

/* Walk inst_list and, for every LIST_IR_INST node whose text is NULL and
 * tmpl is non-NULL, substitute the %N placeholders with the physical register
 * names (bit_64) and store the result in inst->text.
 *
 * LIST_STRING nodes and nodes whose text is already set are left untouched.
 * This must be called before codegen_inst_list() so that every
 * LIST_IR_INST node has a non-NULL text field. */
void ir_emit_function(ListNode_t *inst_list);

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

/* Print virtual register ID assignments for each instruction in inst_list.
 * For each LIST_IR_INST node that has defs or uses with assigned vreg_ids,
 * emits a line of the form:
 *   vreg_N(regname) [def|use] ...  <instruction-text>
 * Instructions with no vreg metadata are printed verbatim. */
void ir_print_vregs(FILE *out, const char *fn_name, ListNode_t *inst_list);

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

#endif /* IR_INST_H */
