/*
    Graph Coloring Register Allocator Implementation (Chaitin's Algorithm)
*/

#include "graph_coloring_allocator.h"
#include "../../Parser/List/List.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

/* Create a new live range */
LiveRange_t *create_live_range(int id, int start, int end)
{
    LiveRange_t *lr = (LiveRange_t *)malloc(sizeof(LiveRange_t));
    if (lr == NULL)
        return NULL;
    
    lr->id = id;
    lr->start_pos = start;
    lr->end_pos = end;
    lr->preferred_reg = NULL;
    lr->assigned_reg_num = -1;
    lr->spill_location = NULL;
    lr->neighbors = NULL;
    lr->neighbors_tail = NULL;
    lr->degree = 0;
    lr->simplified = 0;
    lr->is_spilled = 0;
    lr->preferred_color = -1;
    
    return lr;
}

/* Free a live range */
void free_live_range(LiveRange_t *lr)
{
    if (lr == NULL)
        return;
    
    /* Free neighbors list (but not the LiveRange objects themselves) */
    if (lr->neighbors != NULL)
        DestroyList(lr->neighbors);
    
    free(lr);
}

/* Create interference graph */
InterferenceGraph_t *create_interference_graph(int num_regs)
{
    InterferenceGraph_t *graph = (InterferenceGraph_t *)malloc(sizeof(InterferenceGraph_t));
    if (graph == NULL)
        return NULL;
    
    graph->live_ranges = NULL;
    graph->live_ranges_tail = NULL;
    graph->num_ranges = 0;
    graph->num_physical_regs = num_regs;
    
    return graph;
}

/* Add live range to graph */
void add_live_range(InterferenceGraph_t *graph, LiveRange_t *lr)
{
    assert(graph != NULL);
    assert(lr != NULL);

    ListNode_t *node = CreateListNode(lr, LIST_UNSPECIFIED);
    if (graph->live_ranges_tail != NULL)
    {
        /* O(1) tail append */
        graph->live_ranges_tail->next = node;
    }
    else
    {
        /* First element */
        graph->live_ranges = node;
    }
    graph->live_ranges_tail = node;
    graph->num_ranges++;
}

/* Check if two live ranges interfere (overlap) */
int live_ranges_interfere(LiveRange_t *lr1, LiveRange_t *lr2)
{
    assert(lr1 != NULL);
    assert(lr2 != NULL);
    
    /* Two ranges interfere if they overlap */
    return !(lr1->end_pos < lr2->start_pos || lr2->end_pos < lr1->start_pos);
}

/* O(1) helper: append a neighbor node to a live range's neighbor list */
static inline void append_neighbor(LiveRange_t *lr, ListNode_t *node)
{
    if (lr->neighbors_tail != NULL)
        lr->neighbors_tail->next = node;
    else
        lr->neighbors = node;
    lr->neighbors_tail = node;
}

/* Build interference edges between overlapping ranges */
void build_interference_edges(InterferenceGraph_t *graph)
{
    assert(graph != NULL);
    
    ListNode_t *node1 = graph->live_ranges;
    
    while (node1 != NULL)
    {
        LiveRange_t *lr1 = (LiveRange_t *)node1->cur;
        ListNode_t *node2 = node1->next;
        
        while (node2 != NULL)
        {
            LiveRange_t *lr2 = (LiveRange_t *)node2->cur;
            
            if (live_ranges_interfere(lr1, lr2))
            {
                /* Add lr2 to lr1's neighbors (O(1) via tail pointer) */
                append_neighbor(lr1, CreateListNode(lr2, LIST_UNSPECIFIED));
                lr1->degree++;

                /* Add lr1 to lr2's neighbors (O(1) via tail pointer) */
                append_neighbor(lr2, CreateListNode(lr1, LIST_UNSPECIFIED));
                lr2->degree++;
            }
            
            node2 = node2->next;
        }
        
        node1 = node1->next;
    }
}

/* Check if a node is in a list */
static int is_in_list(ListNode_t *list, LiveRange_t *target)
{
    ListNode_t *cur = list;
    while (cur != NULL)
    {
        if (cur->cur == target)
            return 1;
        cur = cur->next;
    }
    return 0;
}

/* Count active neighbors in a set */
int count_active_neighbors(LiveRange_t *lr, ListNode_t *active_set)
{
    assert(lr != NULL);
    
    int count = 0;
    ListNode_t *neighbor = lr->neighbors;
    
    while (neighbor != NULL)
    {
        LiveRange_t *n = (LiveRange_t *)neighbor->cur;
        if (is_in_list(active_set, n))
            count++;
        neighbor = neighbor->next;
    }
    
    return count;
}

/* Find node with degree < K in active set */
LiveRange_t *find_low_degree_node(InterferenceGraph_t *graph, ListNode_t *active_set)
{
    assert(graph != NULL);
    
    ListNode_t *cur = active_set;
    
    while (cur != NULL)
    {
        LiveRange_t *lr = (LiveRange_t *)cur->cur;
        int active_degree = count_active_neighbors(lr, active_set);
        
        if (active_degree < graph->num_physical_regs)
            return lr;
        
        cur = cur->next;
    }
    
    return NULL;
}

/* Remove node from list and return new head */
static ListNode_t *remove_from_list(ListNode_t *list, LiveRange_t *target)
{
    if (list == NULL)
        return NULL;
    
    if (list->cur == target)
    {
        ListNode_t *new_head = list->next;
        list->next = NULL;
        DestroyList(list);
        return new_head;
    }
    
    ListNode_t *cur = list;
    while (cur->next != NULL)
    {
        if (cur->next->cur == target)
        {
            ListNode_t *to_remove = cur->next;
            cur->next = to_remove->next;
            to_remove->next = NULL;
            DestroyList(to_remove);
            return list;
        }
        cur = cur->next;
    }
    
    return list;
}

/* Find available color (register) for a node */
int find_available_color(LiveRange_t *lr, int num_colors)
{
    assert(lr != NULL);
    
    /* Build set of used colors by neighbors */
    int *used = (int *)calloc(num_colors, sizeof(int));
    if (used == NULL)
        return -1;
    
    ListNode_t *neighbor = lr->neighbors;
    while (neighbor != NULL)
    {
        LiveRange_t *n = (LiveRange_t *)neighbor->cur;
        if (n->assigned_reg_num >= 0 && n->assigned_reg_num < num_colors)
            used[n->assigned_reg_num] = 1;
        neighbor = neighbor->next;
    }
    
    /* Find first available color */
    int color = -1;
    for (int i = 0; i < num_colors; i++)
    {
        if (!used[i])
        {
            color = i;
            break;
        }
    }
    
    free(used);
    return color;
}

/* Allocate registers using Chaitin's graph coloring algorithm */
ListNode_t *allocate_registers_graph_coloring(InterferenceGraph_t *graph)
{
    assert(graph != NULL);

    /* Build interference edges */
    build_interference_edges(graph);

    /* Stack for simplification order (LIFO for coloring) - use ListBuilder for O(1) append */
    ListBuilder stack_builder;
    list_builder_init(&stack_builder);

    /* List of spilled nodes - use ListBuilder for O(1) append */
    ListBuilder spilled_builder;
    list_builder_init(&spilled_builder);

    /* Active set (nodes not yet processed) - use ListBuilder for O(1) initial build */
    ListBuilder active_builder;
    list_builder_init(&active_builder);
    {
        ListNode_t *cur = graph->live_ranges;
        while (cur != NULL)
        {
            list_builder_append(&active_builder, cur->cur, LIST_UNSPECIFIED);
            cur = cur->next;
        }
    }
    ListNode_t *active = list_builder_finish(&active_builder);

    /* Simplification phase */
    while (active != NULL)
    {
        /* Try to find a node with degree < K */
        LiveRange_t *low_degree = find_low_degree_node(graph, active);

        if (low_degree != NULL)
        {
            /* Push onto stack for later coloring (O(1) via ListBuilder) */
            list_builder_append(&stack_builder, low_degree, LIST_UNSPECIFIED);

            /* Remove from active set */
            active = remove_from_list(active, low_degree);
            low_degree->simplified = 1;
        }
        else
        {
            /* No low-degree node - must spill */
            /* Choose node with highest degree (most constrained) */
            LiveRange_t *spill_candidate = NULL;
            int max_degree = -1;

            ListNode_t *cur = active;
            while (cur != NULL)
            {
                LiveRange_t *lr = (LiveRange_t *)cur->cur;
                int deg = count_active_neighbors(lr, active);
                if (deg > max_degree)
                {
                    max_degree = deg;
                    spill_candidate = lr;
                }
                cur = cur->next;
            }

            if (spill_candidate != NULL)
            {
                /* Mark for spilling and remove from active (O(1) via ListBuilder) */
                spill_candidate->is_spilled = 1;
                list_builder_append(&spilled_builder, spill_candidate, LIST_UNSPECIFIED);
                active = remove_from_list(active, spill_candidate);
            }
            else
            {
                /* Should not happen - no nodes in active set */
                break;
            }
        }
    }

    /* Coloring phase (process stack in reverse order) */
    ListNode_t *stack = list_builder_finish(&stack_builder);
    while (stack != NULL)
    {
        /* Pop from stack (take last element for LIFO) */
        LiveRange_t *node = NULL;
        if (stack->next == NULL)
        {
            /* Only one element */
            node = (LiveRange_t *)stack->cur;
            DestroyList(stack);
            stack = NULL;
        }
        else
        {
            /* Find last element */
            ListNode_t *prev = stack;
            while (prev->next->next != NULL)
                prev = prev->next;

            node = (LiveRange_t *)prev->next->cur;
            DestroyList(prev->next);
            prev->next = NULL;
        }

        /* Try to assign a color */
        int color = find_available_color(node, graph->num_physical_regs);

        if (color >= 0)
        {
            node->assigned_reg_num = color;
        }
        else
        {
            /* Actual spill (couldn't color) - O(1) via ListBuilder */
            node->is_spilled = 1;
            list_builder_append(&spilled_builder, node, LIST_UNSPECIFIED);
        }
    }

    /* Clean up active list if any remains */
    if (active != NULL)
        DestroyList(active);

    return list_builder_finish(&spilled_builder);
}

/* Free interference graph */
void free_interference_graph(InterferenceGraph_t *graph)
{
    if (graph == NULL)
        return;
    
    /* NOTE: We do NOT free the LiveRange_t objects themselves because they are owned
     * by the register stack (reg_stack->active_live_ranges), not by the interference graph.
     * The graph only references them. The LiveRange_t objects will be freed when the
     * register stack is cleaned up.
     * 
     * We only need to free the list structure and the neighbor lists within each LiveRange.
     */
    
    /* Free the neighbor lists within each LiveRange */
    ListNode_t *cur = graph->live_ranges;
    while (cur != NULL)
    {
        LiveRange_t *lr = (LiveRange_t *)cur->cur;
        if (lr != NULL && lr->neighbors != NULL)
        {
            DestroyList(lr->neighbors);
            lr->neighbors = NULL;  /* Prevent double-free if same LiveRange is in multiple graphs */
        }
        cur = cur->next;
    }
    
    /* Free the list structure (but not the LiveRange_t objects) */
    if (graph->live_ranges != NULL)
        DestroyList(graph->live_ranges);
    
    free(graph);
}

/* =========================================================================
 * Phase 6: Wire IR liveness into the graph-coloring allocator
 * =========================================================================
 * Only compiled when USE_GRAPH_COLORING_ALLOCATOR is set.
 */
#if USE_GRAPH_COLORING_ALLOCATOR

#include "ir/ir_inst.h"
#include "ir/ir_cfg.h"
#include "ir/ir_liveness.h"

/* Map a RegisterId_t to its canonical pool index (0=rbx, 1=r12, …, 4=r15).
 * Returns -1 for registers that are not part of the callee-saved pool. */
static int pool_index_of(RegisterId_t reg_id)
{
    switch (reg_id)
    {
        case REG_RBX: return 0;
        case REG_R12: return 1;
        case REG_R13: return 2;
        case REG_R14: return 3;
        case REG_R15: return 4;
        default:      return -1;
    }
}

/* Maximum pool index returned by pool_index_of() + 1.
 * The callee-saved pool has exactly 5 slots: rbx(0), r12(1), r13(2), r14(3), r15(4). */
#define IR_PHYS_POOL_MAX 5

/* Populate phys_pool[0..pool_cap-1] from reg_stack (both free and allocated
 * registers), keyed by pool_index_of(reg->reg_id).
 * *n_pool is set to the highest valid index + 1. */
static void gather_phys_pool(RegStack_t    *reg_stack,
                             Register_t   **pool,
                             int            pool_cap,
                             int           *n_pool)
{
    for (int i = 0; i < pool_cap; i++)
        pool[i] = NULL;
    *n_pool = 0;

    ListNode_t *lists[2];
    lists[0] = reg_stack->registers_allocated;
    lists[1] = reg_stack->registers_free;

    for (int l = 0; l < 2; l++)
    {
        for (ListNode_t *cur = lists[l]; cur; cur = cur->next)
        {
            Register_t *r = (Register_t *)cur->cur;
            if (!r)
                continue;
            int idx = pool_index_of(r->reg_id);
            if (idx >= 0 && idx < pool_cap)
            {
                pool[idx] = r;
                if (idx + 1 > *n_pool)
                    *n_pool = idx + 1;
            }
        }
    }
}

/* Collect unique Register_t* from all LIST_IR_INST nodes in inst_list.
 * Returns a heap-allocated array; caller must free().  *n_out = count. */
static Register_t **collect_ir_vregs(ListNode_t *inst_list, int *n_out)
{
    int cap = 8, n = 0;
    Register_t **arr = (Register_t **)malloc(cap * sizeof(Register_t *));
    if (!arr)
    {
        *n_out = 0;
        return NULL;
    }

    for (ListNode_t *node = inst_list; node; node = node->next)
    {
        if (node->type != LIST_IR_INST)
            continue;
        IrInst_t *inst = (IrInst_t *)node->cur;
        if (!inst)
            continue;

        for (int pass = 0; pass < 2; pass++)
        {
            Register_t **rs  = (pass == 0) ? inst->defs : inst->uses;
            int          cnt = (pass == 0) ? inst->n_defs : inst->n_uses;
            for (int i = 0; i < cnt; i++)
            {
                Register_t *r = rs[i];
                if (!r)
                    continue;
                int found = 0;
                for (int j = 0; j < n && !found; j++)
                    if (arr[j] == r)
                        found = 1;
                if (!found)
                {
                    if (n >= cap)
                    {
                        cap *= 2;
                        Register_t **tmp = (Register_t **)realloc(arr,
                                            cap * sizeof(Register_t *));
                        if (!tmp)
                        {
                            free(arr);
                            *n_out = 0;
                            return NULL;
                        }
                        arr = tmp;
                    }
                    arr[n++] = r;
                }
            }
        }
    }

    *n_out = n;
    return arr;
}

/* Build interference edges from liveness sets.
 * Two registers interfere if they both appear in any block's live_out or
 * live_in set.
 *
 * vregs[0..n_vregs-1] and the parallel lr_table[0..n_vregs-1] provide an
 * O(n_vregs) lookup from Register_t* to LiveRange_t* that avoids rescanning
 * the full live_ranges list for every register pair. */
static void build_interference_from_liveness(Register_t          **vregs,
                                              LiveRange_t         **lr_table,
                                              int                   n_vregs,
                                              const Cfg_t          *cfg,
                                              const LivenessInfo_t *liveness)
{
    if (!vregs || !lr_table || !cfg || !liveness || n_vregs == 0)
        return;

    for (int b = 0; b < cfg->n_blocks && b < liveness->n_blocks; b++)
    {
        for (int set = 0; set < 2; set++)
        {
            LiveSet_t *ls = (set == 0) ? &liveness->live_out[b]
                                       : &liveness->live_in[b];
            for (int i = 0; i < ls->n_regs; i++)
            {
                for (int j = i + 1; j < ls->n_regs; j++)
                {
                    Register_t *r1 = ls->regs[i];
                    Register_t *r2 = ls->regs[j];
                    if (!r1 || !r2 || r1 == r2)
                        continue;

                    /* Look up LiveRange_t via the parallel table (O(n_vregs)) */
                    LiveRange_t *lr1 = NULL, *lr2 = NULL;
                    for (int k = 0; k < n_vregs && !(lr1 && lr2); k++)
                    {
                        if (vregs[k] == r1) lr1 = lr_table[k];
                        if (vregs[k] == r2) lr2 = lr_table[k];
                    }
                    if (!lr1 || !lr2)
                        continue;

                    /* Add bidirectional edge if not already present */
                    int already = 0;
                    for (ListNode_t *nb = lr1->neighbors; nb && !already; nb = nb->next)
                        if (nb->cur == lr2)
                            already = 1;
                    if (!already)
                    {
                        append_neighbor(lr1, CreateListNode(lr2, LIST_UNSPECIFIED));
                        lr1->degree++;
                        append_neighbor(lr2, CreateListNode(lr1, LIST_UNSPECIFIED));
                        lr2->degree++;
                    }
                }
            }
        }
    }
}

/* Like find_available_color() but tries lr->preferred_color first. */
static int find_available_color_pref(LiveRange_t *lr, int num_colors)
{
    assert(lr != NULL);

    int *used = (int *)calloc(num_colors, sizeof(int));
    if (!used)
        return -1;

    for (ListNode_t *nb = lr->neighbors; nb; nb = nb->next)
    {
        LiveRange_t *n = (LiveRange_t *)nb->cur;
        if (n->assigned_reg_num >= 0 && n->assigned_reg_num < num_colors)
            used[n->assigned_reg_num] = 1;
    }

    /* Try preferred color first */
    int color = -1;
    if (lr->preferred_color >= 0 && lr->preferred_color < num_colors
        && !used[lr->preferred_color])
    {
        color = lr->preferred_color;
    }
    else
    {
        /* Fall back to lowest available */
        for (int i = 0; i < num_colors && color < 0; i++)
            if (!used[i])
                color = i;
    }

    free(used);
    return color;
}

/* Wire IR liveness into the graph-coloring allocator.
 *
 * Intercepts between codegen and ir_emit_function():
 *   1. cfg_build()              — build the control-flow graph
 *   2. liveness_compute()       — backward-dataflow live-in / live-out
 *   3. collect unique Register_t* from IR def/use metadata
 *   4. build_interference_from_liveness() — replaces position-based edges
 *   5. simplify / select / spill loop (using preferred-color awareness)
 *   6. update each Register_t's bit_64/bit_32 from the assigned physical reg
 */
void ir_liveness_allocate(ListNode_t *inst_list)
{
    if (!inst_list)
        return;

    /* ---- 1. CFG ---- */
    Cfg_t *cfg = cfg_build(inst_list);
    if (!cfg)
        return;

    /* ---- 2. Liveness ---- */
    LivenessInfo_t *liveness = liveness_compute(cfg);
    if (!liveness)
    {
        cfg_free(cfg);
        return;
    }

    /* ---- 3. Collect unique virtual registers ---- */
    int n_vregs = 0;
    Register_t **vregs = collect_ir_vregs(inst_list, &n_vregs);
    if (n_vregs == 0)
    {
        free(vregs);
        liveness_free(liveness);
        cfg_free(cfg);
        return;
    }

    /* ---- Get physical register pool ---- */
    RegStack_t *reg_stack = get_reg_stack();
    Register_t *phys_pool[IR_PHYS_POOL_MAX];
    int n_phys = 0;
    gather_phys_pool(reg_stack, phys_pool, IR_PHYS_POOL_MAX, &n_phys);
    if (n_phys == 0)
        n_phys = reg_stack->num_registers; /* conservative fallback */

    /* ---- 4. Build interference graph ---- */
    InterferenceGraph_t *graph = create_interference_graph(n_phys);
    if (!graph)
    {
        free(vregs);
        liveness_free(liveness);
        cfg_free(cfg);
        return;
    }

    /* Parallel lookup table: lr_table[i] is the LiveRange_t for vregs[i].
     * Built incrementally as we add live ranges so FIND_LR is not needed. */
    LiveRange_t **lr_table = (LiveRange_t **)calloc(n_vregs, sizeof(LiveRange_t *));
    if (!lr_table)
    {
        free(graph);
        free(vregs);
        liveness_free(liveness);
        cfg_free(cfg);
        return;
    }

    for (int i = 0; i < n_vregs; i++)
    {
        LiveRange_t *lr = create_live_range(i, 0, 0);
        if (!lr)
            continue;
        lr->preferred_reg   = vregs[i];
        lr->preferred_color = pool_index_of(vregs[i]->reg_id);
        add_live_range(graph, lr);
        lr_table[i] = lr;
    }

    build_interference_from_liveness(vregs, lr_table, n_vregs, cfg, liveness);
    free(lr_table);

    /* ---- 5. Simplify / Select / Spill ---- */
    {
        ListBuilder stack_builder, spilled_builder, active_builder;
        list_builder_init(&stack_builder);
        list_builder_init(&spilled_builder);
        list_builder_init(&active_builder);

        /* Build active set */
        for (ListNode_t *c = graph->live_ranges; c; c = c->next)
            list_builder_append(&active_builder, c->cur, LIST_UNSPECIFIED);
        ListNode_t *active = list_builder_finish(&active_builder);

        /* Simplification phase */
        while (active != NULL)
        {
            LiveRange_t *low = find_low_degree_node(graph, active);
            if (low != NULL)
            {
                list_builder_append(&stack_builder, low, LIST_UNSPECIFIED);
                active        = remove_from_list(active, low);
                low->simplified = 1;
            }
            else
            {
                /* No low-degree node: must spill the highest-degree node */
                LiveRange_t *spill    = NULL;
                int          max_deg  = -1;
                for (ListNode_t *c = active; c; c = c->next)
                {
                    LiveRange_t *lr = (LiveRange_t *)c->cur;
                    int d = count_active_neighbors(lr, active);
                    if (d > max_deg)
                    {
                        max_deg = d;
                        spill   = lr;
                    }
                }
                if (spill)
                {
                    spill->is_spilled = 1;
                    list_builder_append(&spilled_builder, spill, LIST_UNSPECIFIED);
                    active = remove_from_list(active, spill);
                }
                else
                    break;
            }
        }
        if (active)
            DestroyList(active);

        /* Coloring phase (LIFO pop from stack) */
        ListNode_t *stack = list_builder_finish(&stack_builder);
        while (stack != NULL)
        {
            LiveRange_t *node;
            if (!stack->next)
            {
                node = (LiveRange_t *)stack->cur;
                DestroyList(stack);
                stack = NULL;
            }
            else
            {
                ListNode_t *prev = stack;
                while (prev->next->next)
                    prev = prev->next;
                node = (LiveRange_t *)prev->next->cur;
                DestroyList(prev->next);
                prev->next = NULL;
            }

            int color = find_available_color_pref(node, graph->num_physical_regs);
            if (color >= 0)
                node->assigned_reg_num = color;
            else
            {
                node->is_spilled = 1;
                list_builder_append(&spilled_builder, node, LIST_UNSPECIFIED);
            }
        }

        /* Discard spilled list — stackmng already handles spill code */
        ListNode_t *spilled = list_builder_finish(&spilled_builder);
        if (spilled)
            DestroyList(spilled);
    }

    /* ---- 6. Update Register_t assignments ---- */
    for (ListNode_t *c = graph->live_ranges; c; c = c->next)
    {
        LiveRange_t *lr = (LiveRange_t *)c->cur;
        if (!lr || !lr->preferred_reg || lr->is_spilled)
            continue;
        int color = lr->assigned_reg_num;
        if (color < 0 || color >= n_phys || !phys_pool[color])
            continue;

        Register_t *new_phys = phys_pool[color];
        Register_t *old_reg  = lr->preferred_reg;
        if (new_phys == old_reg)
            continue; /* same slot — no change needed */

        /* Reassign: old_reg now carries new_phys's name.
         * This is safe because the coloring only deviates from the current
         * assignment when preferred_color is unavailable (i.e., a genuine
         * conflict exists).  In that case the old assignment was wrong and
         * the template instructions need the corrected name. */
        free(old_reg->bit_64);
        free(old_reg->bit_32);
        old_reg->bit_64 = strdup(new_phys->bit_64);
        old_reg->bit_32 = strdup(new_phys->bit_32);
        old_reg->reg_id = new_phys->reg_id;
    }

    /* ---- Cleanup ---- */
    /* Manually free the LiveRange_t objects we created (they are NOT owned
     * by reg_stack->active_live_ranges, unlike the ones created during
     * normal codegen).  First free neighbors, then the LiveRange_t. */
    {
        ListNode_t *c = graph->live_ranges;
        while (c)
        {
            LiveRange_t *lr = (LiveRange_t *)c->cur;
            if (lr)
            {
                if (lr->neighbors)
                {
                    DestroyList(lr->neighbors);
                    lr->neighbors = NULL;
                }
                free(lr);
                c->cur = NULL; /* prevent free_interference_graph from touching it */
            }
            c = c->next;
        }
        /* Free the graph structure (live_ranges list, InterferenceGraph_t) */
        if (graph->live_ranges)
            DestroyList(graph->live_ranges);
        graph->live_ranges = NULL;
        free(graph);
    }

    free(vregs);
    liveness_free(liveness);
    cfg_free(cfg);
}

#endif /* USE_GRAPH_COLORING_ALLOCATOR */
