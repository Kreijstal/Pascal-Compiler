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
    lr->degree = 0;
    lr->simplified = 0;
    lr->is_spilled = 0;
    
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
    graph->num_ranges = 0;
    graph->num_physical_regs = num_regs;
    
    return graph;
}

/* Add live range to graph */
void add_live_range(InterferenceGraph_t *graph, LiveRange_t *lr)
{
    assert(graph != NULL);
    assert(lr != NULL);
    
    if (graph->live_ranges == NULL)
    {
        graph->live_ranges = CreateListNode(lr, LIST_UNSPECIFIED);
    }
    else
    {
        graph->live_ranges = PushListNodeBack(graph->live_ranges, 
                                               CreateListNode(lr, LIST_UNSPECIFIED));
    }
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
                /* Add lr2 to lr1's neighbors */
                if (lr1->neighbors == NULL)
                    lr1->neighbors = CreateListNode(lr2, LIST_UNSPECIFIED);
                else
                    lr1->neighbors = PushListNodeBack(lr1->neighbors, 
                                                      CreateListNode(lr2, LIST_UNSPECIFIED));
                lr1->degree++;
                
                /* Add lr1 to lr2's neighbors */
                if (lr2->neighbors == NULL)
                    lr2->neighbors = CreateListNode(lr1, LIST_UNSPECIFIED);
                else
                    lr2->neighbors = PushListNodeBack(lr2->neighbors,
                                                      CreateListNode(lr1, LIST_UNSPECIFIED));
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
    
    /* Stack for simplification order (LIFO for coloring) */
    ListNode_t *stack = NULL;
    
    /* List of spilled nodes */
    ListNode_t *spilled = NULL;
    
    /* Active set (nodes not yet processed) */
    ListNode_t *active = NULL;
    {
        /* Copy all live ranges to active set */
        ListNode_t *cur = graph->live_ranges;
        while (cur != NULL)
        {
            LiveRange_t *lr = (LiveRange_t *)cur->cur;
            if (active == NULL)
                active = CreateListNode(lr, LIST_UNSPECIFIED);
            else
                active = PushListNodeBack(active, CreateListNode(lr, LIST_UNSPECIFIED));
            cur = cur->next;
        }
    }
    
    /* Simplification phase */
    while (active != NULL)
    {
        /* Try to find a node with degree < K */
        LiveRange_t *low_degree = find_low_degree_node(graph, active);
        
        if (low_degree != NULL)
        {
            /* Push onto stack for later coloring */
            if (stack == NULL)
                stack = CreateListNode(low_degree, LIST_UNSPECIFIED);
            else
                stack = PushListNodeBack(stack, CreateListNode(low_degree, LIST_UNSPECIFIED));
            
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
                /* Mark for spilling and remove from active */
                spill_candidate->is_spilled = 1;
                if (spilled == NULL)
                    spilled = CreateListNode(spill_candidate, LIST_UNSPECIFIED);
                else
                    spilled = PushListNodeBack(spilled, 
                                               CreateListNode(spill_candidate, LIST_UNSPECIFIED));
                
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
            /* Actual spill (couldn't color) */
            node->is_spilled = 1;
            if (spilled == NULL)
                spilled = CreateListNode(node, LIST_UNSPECIFIED);
            else
                spilled = PushListNodeBack(spilled, CreateListNode(node, LIST_UNSPECIFIED));
        }
    }
    
    /* Clean up active list if any remains */
    if (active != NULL)
        DestroyList(active);
    
    return spilled;
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
