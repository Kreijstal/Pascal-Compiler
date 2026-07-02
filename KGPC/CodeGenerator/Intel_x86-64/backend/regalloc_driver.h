/*
 * regalloc_driver.h — graph-coloring register allocation driver.
 *
 * Declares ir_liveness_allocate, moved out of codegen.c into the standalone
 * backend library.  The front-end also declares this in
 * codegen_subprograms_internal.h (identical signature); both are fine.
 */
#ifndef KGPC_REGALLOC_DRIVER_H
#define KGPC_REGALLOC_DRIVER_H

/* Forward declaration — full definition lives in List.h. */
typedef struct List ListNode_t;

/* Assign physical registers to the virtual registers referenced by the
 * LIST_IR_INST nodes of a single function's instruction list, in place.
 * No-op unless built with USE_GRAPH_COLORING_ALLOCATOR. */
void ir_liveness_allocate(ListNode_t *inst_list);

#endif /* KGPC_REGALLOC_DRIVER_H */
