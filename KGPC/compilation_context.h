/*
 * CompilationContext: central structure owning all per-compilation state.
 *
 * Phase 2 of the non-merged-units refactoring.  This struct replaces the
 * scattered global arrays (g_loaded_units, etc.) with a single owner that
 * the pipeline passes around.
 *
 * Owned data:
 *   - loaded_units      parsed unit ASTs (in load order)
 *   - unit_count        how many units have been loaded
 *   - symtab            the shared symbol table / scope tree
 *   - program           the program AST (not owned, just referenced)
 */

#ifndef KGPC_COMPILATION_CONTEXT_H
#define KGPC_COMPILATION_CONTEXT_H

#include "Parser/ParseTree/tree.h"
#include "Parser/SemanticCheck/SymTab/SymTab.h"

/* A single loaded-unit record. */
typedef struct {
    Tree_t *unit_tree;   /* The parsed unit AST (owned by the context) */
    int     unit_idx;    /* Unit registry index (1-based) */
} LoadedUnit;

/* The compilation context owns all per-compilation state that is not
 * specific to a single pass (semantic analysis, code generation, etc.). */
typedef struct CompilationContext {
    /* --- Loaded units (in dependency / load order) --- */
    LoadedUnit *loaded_units;
    int         loaded_unit_count;
    int         loaded_unit_capacity;

    /* --- Symbol table / scope tree (created early, survives until cleanup) --- */
    SymTab_t   *symtab;

    /* --- Program AST (not owned; the caller manages its lifetime) --- */
    Tree_t     *program;
} CompilationContext;

/* Initialise a zero-filled context.  Does NOT allocate the symtab. */
void compilation_context_init(CompilationContext *ctx);

/* Destroy all owned data (loaded-unit trees, etc.).
 * Does NOT destroy ctx->symtab or ctx->program (caller owns those). */
void compilation_context_destroy(CompilationContext *ctx);

/* Append a unit to the loaded-units list.  The context takes ownership
 * of `unit_tree`. */
void compilation_context_add_unit(CompilationContext *ctx,
                                  Tree_t *unit_tree, int unit_idx);

/* Look up a loaded unit by its registry index.
 * Returns NULL if not found. */
LoadedUnit *compilation_context_find_unit(const CompilationContext *ctx,
                                          int unit_idx);

/* Set/get the active compilation context for passes that don't receive
 * it as a parameter (e.g. semcheck, mark_used).  The pointer is NOT owned. */
void compilation_context_set_active(CompilationContext *ctx);
CompilationContext *compilation_context_get_active(void);

#endif /* KGPC_COMPILATION_CONTEXT_H */
