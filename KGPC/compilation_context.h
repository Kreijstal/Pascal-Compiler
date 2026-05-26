/**
 * @file compilation_context.h
 * @brief CompilationContext: central structure owning all per-compilation
 * state.
 *
 * Replaces the scattered global arrays (g_loaded_units, etc.) with a
 * single owner that the pipeline passes around.  Owned data:
 *
 *   - loaded_units      parsed unit ASTs (in load order)
 *   - unit_count        how many units have been loaded
 *   - symtab            the shared symbol table / scope tree
 *   - program           the program AST (not owned, just referenced)
 */

#ifndef KGPC_COMPILATION_CONTEXT_H
#define KGPC_COMPILATION_CONTEXT_H

#include "Parser/ParseTree/tree.h"
#include "Parser/SemanticCheck/SymTab/SymTab.h"
#include <stdbool.h>

/** @brief One loaded-unit record (parsed AST plus bookkeeping). */
typedef struct {
  Tree_t *unit_tree; /**< The parsed unit AST (owned by the context). */
  int unit_idx;      /**< Unit registry index (1-based). */
  char *source_path; /**< Source file path (owned, may be NULL). */
} LoadedUnit;

/**
 * @brief Per-compilation state owned by one pipeline invocation.
 *
 * Holds everything that is not specific to a single pass (semantic
 * analysis, code generation, ...).  Pass-specific state lives in the
 * respective pass structures (e.g. @c CodeGenContext).
 */
typedef struct CompilationContext {
  /* --- Loaded units (in dependency / load order) --- */
  LoadedUnit *loaded_units;     /**< Dynamic array of loaded units. */
  int loaded_unit_count;        /**< Number of entries in @c loaded_units. */
  int loaded_unit_capacity;     /**< Allocated capacity of @c loaded_units. */

  /* --- Include files resolved during preprocessing (for cache keys) --- */
  char **include_files;         /**< Resolved include-file paths. */
  int include_file_count;       /**< Number of entries in @c include_files. */
  int include_file_capacity;    /**< Allocated capacity of @c include_files. */

  /** @brief Symbol table / scope tree (created early, lives until cleanup). */
  SymTab_t *symtab;

  /** @brief Program AST (not owned; the caller manages its lifetime). */
  Tree_t *program;
} CompilationContext;

/** @brief Initialise a zero-filled context.  Does NOT allocate the symtab. */
void compilation_context_init(CompilationContext *ctx);

/**
 * @brief Destroy all owned data (loaded-unit trees, etc.).
 *
 * Does NOT destroy @c ctx->symtab or @c ctx->program — the caller owns
 * those and is responsible for their lifetimes.
 */
void compilation_context_destroy(CompilationContext *ctx);

/**
 * @brief Append a unit to the loaded-units list.
 *
 * The context takes ownership of @p unit_tree on success.  Returns
 * true on success; false on allocation failure (in which case
 * @p unit_tree is NOT freed and the caller retains ownership).
 */
bool compilation_context_add_unit(CompilationContext *ctx, Tree_t *unit_tree,
                                  int unit_idx);

/**
 * @brief Record include files resolved during preprocessing (cache key input).
 *
 * Returns true if all @p count files were recorded; false if any
 * allocation failed (partial registration possible on failure).
 */
bool compilation_context_add_include_files(CompilationContext *ctx,
                                           const char *const *files, int count);

/** @brief Look up a loaded unit by its registry index; NULL if not found. */
LoadedUnit *compilation_context_find_unit(const CompilationContext *ctx,
                                          int unit_idx);

/**
 * @brief Install @p ctx as the active context.
 *
 * Passes that don't receive the context as a parameter (semcheck,
 * mark-used, ...) read it via @ref compilation_context_get_active.
 * The pointer is NOT owned.
 */
void compilation_context_set_active(CompilationContext *ctx);

/** @brief Get the context most recently installed via @ref compilation_context_set_active. */
CompilationContext *compilation_context_get_active(void);

#endif /* KGPC_COMPILATION_CONTEXT_H */
