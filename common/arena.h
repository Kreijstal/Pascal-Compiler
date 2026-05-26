/**
 * @file arena.h
 * @brief Region-based memory allocator.
 *
 * Bump-pointer arena: all allocations from a given arena are freed in one
 * call to @ref arena_destroy or @ref arena_reset.  Used by the parser and
 * the compiler front end for AST and symbol-table nodes whose lifetimes
 * are tied to a single compilation unit.
 */
#ifndef ARENA_H
#define ARENA_H

#include <stddef.h>

typedef struct arena_block arena_block_t;

/**
 * @brief Region-of-memory allocator handle.
 *
 * Created by @ref arena_create.  Internally a linked list of blocks; the
 * fields are public so that callers can inspect block utilisation but
 * should not be mutated directly.
 */
typedef struct arena {
  arena_block_t *current;     /**< Block servicing the next allocation. */
  arena_block_t *head;        /**< Head of the block list (for reset/free). */
  size_t default_block_size;  /**< Block size used when growing the arena. */
} arena_t;

/**
 * @brief Construct a new arena.
 *
 * @p default_block_size is the block size used when growing the arena;
 * individual allocations larger than this still succeed (they get their
 * own dedicated block).
 *
 * @returns Newly allocated arena, or NULL on out-of-memory.
 */
arena_t *arena_create(size_t default_block_size);

/**
 * @brief Allocate @p size bytes from @p arena.
 *
 * Returned memory is uninitialised.  Lives until the arena is reset or
 * destroyed; do not free individually.
 * @returns Pointer to @p size bytes, or NULL on out-of-memory.
 */
void *arena_alloc(arena_t *arena, size_t size);

/**
 * @brief Release every block but the first; reuse the arena for fresh
 * allocations.  Existing pointers into the arena become invalid.
 */
void arena_reset(arena_t *arena);

/** @brief Free all blocks owned by @p arena and the arena itself. */
void arena_destroy(arena_t *arena);

/**
 * @brief Install @p arena as the process-wide default arena.
 *
 * Some helper APIs (notably the parser combinators) allocate from this
 * implicit arena when no explicit handle is in scope.
 */
void arena_set_global(arena_t *arena);

/** @brief Get the arena most recently installed via @ref arena_set_global. */
arena_t *arena_get_global(void);

#endif
