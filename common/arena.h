#ifndef ARENA_H
#define ARENA_H

#include <stddef.h>

typedef struct arena_block arena_block_t;

typedef struct arena {
    arena_block_t* current;
    arena_block_t* head;
    size_t default_block_size;
} arena_t;

/* Create a new arena with the given default block size */
arena_t* arena_create(size_t default_block_size);

/* Allocate memory from the arena */
void* arena_alloc(arena_t* arena, size_t size);

/* Reset the arena (free all blocks except one, reset pointers) */
void arena_reset(arena_t* arena);

/* Destroy the arena and free all memory */
void arena_destroy(arena_t* arena);

/* Global arena for the parser/compiler */
void arena_set_global(arena_t* arena);
arena_t* arena_get_global(void);

#endif
