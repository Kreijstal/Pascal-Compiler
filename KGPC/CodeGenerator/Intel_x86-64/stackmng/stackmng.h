/*
    Damon Gwinn
    Stack Manager structure (see codegen.h for details)

    TODO:
        - Turns linked lists into hash table of linked lists
        - Support 16 and 8 bit registers
*/
#ifndef STACK_MNG_H
#define STACK_MNG_H

#include <stdlib.h>
#include <stdio.h>
#include "../../../Parser/List/List.h"

#ifndef KGPC_ENABLE_REG_DEBUG
#define KGPC_ENABLE_REG_DEBUG 0
#endif

/* Register allocation strategy selection */
#ifndef USE_GRAPH_COLORING_ALLOCATOR
#define USE_GRAPH_COLORING_ALLOCATOR 0
#endif

#define DOUBLEWORD 4

extern int g_stack_home_space_bytes;

static inline int current_stack_home_space(void)
{
    return g_stack_home_space_bytes;
}

typedef struct StackScope StackScope_t;
typedef struct StackNode StackNode_t;
typedef struct RegStack RegStack_t;
typedef struct Register Register_t;

typedef void (*RegisterSpillCallback)(Register_t *reg, StackNode_t *spill_slot, void *context);

#define NUM_CALLER_SAVED_REGISTERS 9

/* Helper for getting special registers */
extern int num_args_alloced;
void free_arg_regs(void);
const char *get_arg_reg64_num(int num);
const char *get_arg_reg32_num(int num);

/****** stackmng *******/
typedef struct stackmng
{
    /* Still in progress */
    StackScope_t *cur_scope;
    RegStack_t *reg_stack;
} stackmng_t;

void init_stackmng();
StackScope_t *get_cur_scope();
int get_full_stack_offset();
int get_needed_stack_space();
void push_stackscope();
void push_stackscope_inherited();
void pop_stackscope();
StackNode_t *add_l_t(char *);
StackNode_t *add_l_t_bytes(char *label, int size);
StackNode_t *add_l_x(char *, int size);
void remove_last_l_x(char *label);
StackNode_t *add_l_z(char *);
StackNode_t *add_q_z(char *);
void reserve_z_offset(int size);
void unreserve_z_offset(void);
StackNode_t *add_array(char *label, int total_size, int element_size, int lower_bound);
StackNode_t *add_dynamic_array(char *label, int element_size, int lower_bound,
    int use_static_storage, const char *static_label);
StackNode_t *add_static_var(char *label, int size, const char *static_label);
StackNode_t *find_in_temp(char *);
StackNode_t *find_label(char *);
/* Returns the scope depth (0 = current scope, 1 = parent, etc.) */
StackNode_t *find_label_with_depth(char *label, int *depth);
RegStack_t *get_reg_stack();
void free_stackmng();

/********* RegStack_t **********/

typedef struct RegStack
{
    ListNode_t *registers_free;
    ListNode_t *registers_allocated;
    int num_registers;
    /* Global sequence counter for LRU tracking */
    unsigned long long use_sequence;
    
#if USE_GRAPH_COLORING_ALLOCATOR
    /* Live range tracking for graph coloring */
    ListNode_t *active_live_ranges;  /* List of LiveRange_t* currently being tracked */
    int next_live_range_id;          /* ID counter for live ranges */
#endif
} RegStack_t;

RegStack_t *init_reg_stack();

/* NOTE: Getters return number greater than 1 if it had to kick a value out to temp */
/* The returned int is the temp offset to restore the value */
int get_register_64bit(RegStack_t *, char *reg_64, Register_t **);
int get_register_32bit(RegStack_t *, char *reg_32, Register_t **);
void restore_register_64bit(RegStack_t *, Register_t *, int temp_offset);
void restore_register_32bit(RegStack_t *, Register_t *, int temp_offset);
void free_reg(RegStack_t *, Register_t *);
void swap_reg_stack(RegStack_t *);
Register_t *front_reg_stack(RegStack_t *);
Register_t *get_free_reg(RegStack_t *, ListNode_t **);
/* Force register allocation by spilling LRU register if needed */
Register_t *get_reg_with_spill(RegStack_t *, ListNode_t **);
void register_set_spill_callback(Register_t *reg, RegisterSpillCallback callback, void *context);
void register_clear_spill_callback(Register_t *reg);
int get_num_registers_free(RegStack_t *);
int get_num_registers_alloced(RegStack_t *);

void free_reg_stack(RegStack_t *);

#if KGPC_ENABLE_REG_DEBUG
extern const char *g_reg_debug_context;
#endif

/********* Register_t **********/
typedef struct Register
{
    char *bit_64;
    char *bit_32;
    /* Spill tracking - if spilled, this points to the stack location */
    StackNode_t *spill_location;
    /* Sequence number for LRU tracking */
    unsigned long long last_use_seq;
    RegisterSpillCallback spill_callback;
    void *spill_context;
    
#if USE_GRAPH_COLORING_ALLOCATOR
    /* Forward declaration from graph_coloring_allocator.h */
    struct LiveRange *current_live_range;  /* Active live range for this register */
#endif
} Register_t;


/********* StackScope_t **********/

typedef struct StackScope
{
    int t_offset, x_offset, z_offset;
    ListNode_t *t, *x, *z;

    StackScope_t *prev_scope;
} StackScope_t;

StackScope_t *init_stackscope();
StackNode_t *stackscope_find_t(StackScope_t *, char *);
StackNode_t *stackscope_find_x(StackScope_t *, char *);
StackNode_t *stackscope_find_z(StackScope_t *, char *);
StackScope_t *free_stackscope(StackScope_t *);
void free_all_stackscopes(StackScope_t *);
void free_stackscope_list(ListNode_t *);

/********* StackNode_t **********/
/* Used to encapsulate offset, label, and size */
typedef struct StackNode
{
    int offset;
    char *label;
    int size;
    int is_array;
    int array_lower_bound;
    int element_size;
    int is_dynamic;
    int is_static;
    int is_reference;
    int is_alias;
    char *static_label;
} StackNode_t;

/* WARNING: init_stack_node makes copy of given label */
StackNode_t *init_stack_node(int offset, char *label, int size);
StackNode_t *add_static_array(char *label, int total_size, int element_size, int lower_bound, const char *static_label);
void destroy_stack_node(StackNode_t *);

#endif
