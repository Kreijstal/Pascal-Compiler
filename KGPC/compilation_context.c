/*
 * CompilationContext implementation.
 * See compilation_context.h for documentation.
 */

#include "compilation_context.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/* Global active-context pointer.
 *
 * Reset contract: this pointer must be set to NULL (via
 * compilation_context_set_active(NULL)) at the end of every compilation before
 * the pointed-to CompilationContext is destroyed. compilation_context_destroy()
 * auto-clears the pointer if it currently refers to the context being
 * destroyed, ensuring passes that call compilation_context_get_active() cannot
 * reach stale memory after cleanup.
 */
static CompilationContext *g_active_comp_ctx = NULL;

void compilation_context_init(CompilationContext *ctx) {
  memset(ctx, 0, sizeof(*ctx));
}

void compilation_context_destroy(CompilationContext *ctx) {
  if (ctx == NULL)
    return;

  /* Auto-clear the global active pointer to prevent use-after-free if a
   * caller forgets to call compilation_context_set_active(NULL) before
   * destroying the context. */
  if (g_active_comp_ctx == ctx)
    g_active_comp_ctx = NULL;

  if (ctx->loaded_units != NULL) {
    for (int i = 0; i < ctx->loaded_unit_count; ++i) {
      if (ctx->loaded_units[i].unit_tree != NULL)
        destroy_tree(ctx->loaded_units[i].unit_tree);
      free(ctx->loaded_units[i].source_path);
    }
    free(ctx->loaded_units);
  }
  ctx->loaded_units = NULL;
  ctx->loaded_unit_count = 0;
  ctx->loaded_unit_capacity = 0;

  if (ctx->include_files != NULL) {
    for (int i = 0; i < ctx->include_file_count; ++i)
      free(ctx->include_files[i]);
    free(ctx->include_files);
  }
  ctx->include_files = NULL;
  ctx->include_file_count = 0;
  ctx->include_file_capacity = 0;

  /* symtab and program are NOT owned by the context. */
}

bool compilation_context_add_unit(CompilationContext *ctx, Tree_t *unit_tree,
                                  int unit_idx) {
  if (ctx->loaded_unit_count == ctx->loaded_unit_capacity) {
    int new_cap =
        ctx->loaded_unit_capacity == 0 ? 8 : ctx->loaded_unit_capacity * 2;
    LoadedUnit *new_arr = (LoadedUnit *)realloc(
        ctx->loaded_units, (size_t)new_cap * sizeof(LoadedUnit));
    if (new_arr == NULL) {
      fprintf(stderr, "ERROR: Out of memory in compilation_context_add_unit\n");
      return false;
    }
    ctx->loaded_units = new_arr;
    ctx->loaded_unit_capacity = new_cap;
  }
  ctx->loaded_units[ctx->loaded_unit_count].unit_tree = unit_tree;
  ctx->loaded_units[ctx->loaded_unit_count].unit_idx = unit_idx;
  ctx->loaded_units[ctx->loaded_unit_count].source_path = NULL;
  ctx->loaded_unit_count++;
  return true;
}

bool compilation_context_add_include_files(CompilationContext *ctx,
                                           const char *const *files,
                                           int count) {
  for (int i = 0; i < count; ++i) {
    if (ctx->include_file_count == ctx->include_file_capacity) {
      int new_cap =
          ctx->include_file_capacity == 0 ? 64 : ctx->include_file_capacity * 2;
      char **new_arr = (char **)realloc(ctx->include_files,
                                        (size_t)new_cap * sizeof(char *));
      if (new_arr == NULL) {
        fprintf(
            stderr,
            "ERROR: Out of memory in compilation_context_add_include_files\n");
        return false;
      }
      ctx->include_files = new_arr;
      ctx->include_file_capacity = new_cap;
    }
    char *copy = strdup(files[i]);
    if (copy == NULL) {
      fprintf(
          stderr,
          "ERROR: Out of memory in compilation_context_add_include_files\n");
      return false;
    }
    ctx->include_files[ctx->include_file_count++] = copy;
  }
  return true;
}

void compilation_context_set_active(CompilationContext *ctx) {
  g_active_comp_ctx = ctx;
}

CompilationContext *compilation_context_get_active(void) {
  return g_active_comp_ctx;
}

LoadedUnit *compilation_context_find_unit(const CompilationContext *ctx,
                                          int unit_idx) {
  if (ctx == NULL || ctx->loaded_units == NULL)
    return NULL;

  for (int i = 0; i < ctx->loaded_unit_count; ++i) {
    if (ctx->loaded_units[i].unit_idx == unit_idx)
      return &ctx->loaded_units[i];
  }
  return NULL;
}
