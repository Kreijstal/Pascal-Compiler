/*
    Graph Coloring Register Allocator (Chaitin's Algorithm)
    
    This module implements a proper graph coloring register allocator
    as an alternative to the simple LRU-based spilling approach.
    
    Enable with -DUSE_GRAPH_COLORING_ALLOCATOR
*/

#ifndef GRAPH_COLORING_ALLOCATOR_H
#define GRAPH_COLORING_ALLOCATOR_H

#include <stdlib.h>
#include "stackmng/stackmng.h"
#include "../../Parser/List/List.h"

/* Forward declarations */
typedef struct LiveRange LiveRange_t;
typedef struct InterferenceGraph InterferenceGraph_t;

/* Live range represents a value's lifetime */
struct LiveRange
{
    int id;                          /* Unique identifier */
    Register_t *preferred_reg;       /* Preferred physical register */
    int assigned_reg_num;            /* Assigned register number (-1 if spilled) */
    StackNode_t *spill_location;     /* Stack location if spilled */
    
    /* Live range interval */
    int start_pos;                   /* Start position in instruction sequence */
    int end_pos;                     /* End position in instruction sequence */
    
    /* Interference information */
    ListNode_t *neighbors;           /* List of LiveRange_t* that interfere */
    int degree;                      /* Number of interfering neighbors */
    
    /* Allocation state */
    int simplified;                  /* Already processed in simplification */
    int is_spilled;                  /* Marked for spilling */
};

/* Interference graph for register allocation */
struct InterferenceGraph
{
    ListNode_t *live_ranges;         /* List of LiveRange_t* */
    int num_ranges;
    int num_physical_regs;           /* Number of available registers */
};

/* Create a new live range */
LiveRange_t *create_live_range(int id, int start, int end);

/* Free a live range */
void free_live_range(LiveRange_t *lr);

/* Create interference graph */
InterferenceGraph_t *create_interference_graph(int num_regs);

/* Add live range to graph */
void add_live_range(InterferenceGraph_t *graph, LiveRange_t *lr);

/* Build interference edges between overlapping ranges */
void build_interference_edges(InterferenceGraph_t *graph);

/* Check if two live ranges interfere */
int live_ranges_interfere(LiveRange_t *lr1, LiveRange_t *lr2);

/* Allocate registers using graph coloring (Chaitin's algorithm) */
/* Returns list of spilled LiveRange_t* */
ListNode_t *allocate_registers_graph_coloring(InterferenceGraph_t *graph);

/* Free interference graph */
void free_interference_graph(InterferenceGraph_t *graph);

/* Helper: Count active neighbors in a set */
int count_active_neighbors(LiveRange_t *lr, ListNode_t *active_set);

/* Helper: Find node with lowest degree in active set */
LiveRange_t *find_low_degree_node(InterferenceGraph_t *graph, 
                                   ListNode_t *active_set);

/* Helper: Find available color (register) for a node */
int find_available_color(LiveRange_t *lr, int num_colors);

#endif /* GRAPH_COLORING_ALLOCATOR_H */
