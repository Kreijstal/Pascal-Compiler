/**
 * @file ast_cache.h
 * @brief Binary AST cache for parsed Pascal units.
 *
 * Allows saving a parsed @ref ast_t tree to a binary file and reloading
 * it, skipping both preprocessing and combinator parsing on cache hits.
 * This dramatically speeds up FPC RTL test compilation where `system.pp`
 * and `objpas.pp` (36K+ lines after preprocessing) would otherwise be
 * re-parsed for every test in the suite.
 *
 * Binary format (all multi-byte integers little-endian).  Header
 * `"KGPC_AST\0"` (9 bytes) followed by `uint32 version`.  Then
 * `uint32 preprocessed_length` and that many bytes of preprocessed
 * source.  Then `uint32 ast_node_count` and, for each node in
 * pre-order traversal: `uint32 tag`, `int32 line`, `int32 col`,
 * `int32 index`, `uint8 has_sym`, optional `uint32 sym_len` and
 * `sym_bytes`, `uint8 has_child`, `uint8 has_next`.
 */
#ifndef AST_CACHE_H
#define AST_CACHE_H

#include "parser.h"
#include <stdbool.h>
#include <stddef.h>

/** @brief On-disk format version (bump on incompatible layout changes). */
#define AST_CACHE_VERSION 2

/**
 * @brief Save a parsed AST tree and its preprocessed source to @p cache_path.
 *
 * @p cache_path is overwritten if it exists.  @p root is the AST tree
 * to serialise.  @p preprocessed_buf is the preprocessor output that
 * produced @p root, and @p preprocessed_len is its length in bytes.
 *
 * Returns true on success; false on any I/O error.
 */
bool ast_cache_save(const char *cache_path, const ast_t *root,
                    const char *preprocessed_buf, size_t preprocessed_len);

/**
 * @brief Load a cached AST and preprocessed source from @p cache_path.
 *
 * On success the caller owns both the AST tree (must be freed with the
 * usual AST helpers) and the @p out_buf malloc'd buffer.  Output
 * parameters (@p out_root, @p out_buf, @p out_len) receive the
 * deserialised tree, a malloc'd copy of the preprocessed source, and
 * its length, respectively.
 *
 * Returns true on success; on failure returns false and sets outputs
 * to NULL / 0.
 */
bool ast_cache_load(const char *cache_path, ast_t **out_root, char **out_buf,
                    size_t *out_len);

#endif /* AST_CACHE_H */
