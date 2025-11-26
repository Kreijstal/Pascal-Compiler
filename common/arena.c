#include "arena.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

/* 
 * Arena Block Structure
 * Memory layout: [header][data...]
 */
struct arena_block {
    struct arena_block* next;
    size_t capacity;
    size_t used;
    uint8_t data[];
};

static arena_t* g_global_arena = NULL;

void arena_set_global(arena_t* arena) {
    g_global_arena = arena;
}

arena_t* arena_get_global(void) {
    return g_global_arena;
}

static arena_block_t* arena_block_create(size_t size) {
    arena_block_t* block = (arena_block_t*)malloc(sizeof(arena_block_t) + size);
    if (!block) {
        fprintf(stderr, "FATAL: Out of memory in arena_block_create\n");
        abort();
    }
    block->next = NULL;
    block->capacity = size;
    block->used = 0;
    return block;
}

arena_t* arena_create(size_t default_block_size) {
    arena_t* arena = (arena_t*)malloc(sizeof(arena_t));
    if (!arena) {
        fprintf(stderr, "FATAL: Out of memory in arena_create\n");
        abort();
    }
    arena->default_block_size = default_block_size;
    arena->head = arena_block_create(default_block_size);
    arena->current = arena->head;
    return arena;
}

void* arena_alloc(arena_t* arena, size_t size) {
    if (!arena) return malloc(size); // Fallback if no arena provided

    /* Align size to 8 bytes */
    size = (size + 7) & ~7;

    if (arena->current->used + size <= arena->current->capacity) {
        void* ptr = arena->current->data + arena->current->used;
        arena->current->used += size;
        return ptr;
    }

    /* Need a new block */
    size_t new_size = (size > arena->default_block_size) ? size : arena->default_block_size;
    arena_block_t* new_block = arena_block_create(new_size);
    
    /* Add to list */
    new_block->next = arena->head; /* Insert at head for O(1) */
    arena->head = new_block;
    arena->current = new_block;

    void* ptr = new_block->data;
    new_block->used += size;
    return ptr;
}

void arena_reset(arena_t* arena) {
    if (!arena) return;

    /* Free all blocks except the first one (or keep them and reset 'used'?) 
       For simplicity, let's keep one block and free others, or just reset all.
       Actually, keeping them is better for performance if we reuse the arena.
       But for now, let's just free everything to be safe and allocate a fresh one.
    */
    
    arena_block_t* cur = arena->head;
    while (cur != NULL) {
        arena_block_t* next = cur->next;
        free(cur);
        cur = next;
    }

    arena->head = arena_block_create(arena->default_block_size);
    arena->current = arena->head;
}

void arena_destroy(arena_t* arena) {
    if (!arena) return;

    /* Clear global pointer if this is the global arena */
    if (g_global_arena == arena) {
        g_global_arena = NULL;
    }

    arena_block_t* cur = arena->head;
    while (cur != NULL) {
        arena_block_t* next = cur->next;
        free(cur);
        cur = next;
    }
    free(arena);
}
