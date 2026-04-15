/*
 * Binary AST cache for parsed units.
 *
 * Allows saving a parsed ast_t tree to a binary file and reloading it,
 * skipping both preprocessing and combinator parsing on cache hits.
 * This dramatically speeds up FPC RTL test compilation where system.pp
 * and objpas.pp (36K+ lines after preprocessing) are re-parsed for every test.
 *
 * Binary format (all values little-endian):
 *   Header: "KGPC_AST\0" (9 bytes) + uint32 version
 *   Preprocessed source length (uint32) + preprocessed source bytes
 *   AST node count (uint32)
 *   For each node (pre-order traversal):
 *     tag (uint32), line (int32), col (int32), index (int32),
 *     has_sym (uint8), [sym_len (uint32) + sym_bytes if has_sym],
 *     has_child (uint8), has_next (uint8)
 */
#ifndef AST_CACHE_H
#define AST_CACHE_H

#include "parser.h"
#include <stddef.h>
#include <stdbool.h>

#define AST_CACHE_VERSION 2

/* Save a parsed AST tree and its preprocessed source buffer to a binary cache file.
 * Returns true on success. */
bool ast_cache_save(const char *cache_path, const ast_t *root,
                    const char *preprocessed_buf, size_t preprocessed_len);

/* Load a cached AST tree and preprocessed source buffer from a binary cache file.
 * On success, *out_root is set to the deserialized AST (caller must free),
 * *out_buf is set to a malloc'd copy of the preprocessed source,
 * *out_len is set to the preprocessed source length, and returns true.
 * On failure, returns false and sets outputs to NULL/0. */
bool ast_cache_load(const char *cache_path, ast_t **out_root,
                    char **out_buf, size_t *out_len);

#endif /* AST_CACHE_H */
