/*
    Damon Gwinn
    Stack Manager structure (see codegen.h for details)

    TODO:
        - Turns linked lists into hash table of linked lists
        - Support 16 and 8 bit registers
*/
#ifndef STACK_MNG_H
#define STACK_MNG_H

#include "../../../Parser/List/List.h"
#include <stdio.h>
#include <stdlib.h>

#ifndef KGPC_ENABLE_REG_DEBUG
#define KGPC_ENABLE_REG_DEBUG 0
#endif

/* Register allocation strategy selection */
#ifndef USE_GRAPH_COLORING_ALLOCATOR
#define USE_GRAPH_COLORING_ALLOCATOR 0
#endif

#define DOUBLEWORD 4

extern int g_stack_home_space_bytes;

static inline int current_stack_home_space(void) {
  return g_stack_home_space_bytes;
}

typedef struct StackScope StackScope_t;
typedef struct StackNode StackNode_t;
typedef struct RegStack RegStack_t;
typedef struct Register Register_t;
typedef enum RegisterId RegisterId_t;

typedef void (*RegisterSpillCallback)(Register_t *reg, StackNode_t *spill_slot,
                                      void *context);

#define NUM_CALLER_SAVED_REGISTERS 9

/* Helper for getting special registers */
extern int num_args_alloced;
void free_arg_regs(void);
const char *get_arg_reg64_num(int num);
const char *get_arg_reg32_num(int num);

/****** stackmng *******/
typedef struct stackmng {
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
StackNode_t *add_l_z_bytes(char *label, int size);
StackNode_t *add_q_z(char *);
void reserve_z_offset(int size);
void unreserve_z_offset(void);
StackNode_t *add_array(char *label, int total_size, int element_size,
                       int lower_bound);
StackNode_t *add_dynamic_array(char *label, int element_size, int lower_bound,
                               int use_static_storage,
                               const char *static_label);
StackNode_t *add_static_var(char *label, int size, const char *static_label);
StackNode_t *find_in_temp(char *);
StackNode_t *find_label(const char *);
/* Returns static-link depth (0 = current frame, 1 = parent frame, etc.). */
StackNode_t *find_label_with_depth(const char *label, int *depth);
RegStack_t *get_reg_stack();
void reset_reg_stack(void);
void free_stackmng();

/********* RegStack_t **********/

typedef struct RegStack {
  ListNode_t *registers_free;
  ListNode_t *registers_allocated;
  int num_registers;
  /* Global sequence counter for LRU tracking */
  unsigned long long use_sequence;

#if USE_GRAPH_COLORING_ALLOCATOR
  /* Live range tracking for graph coloring */
  ListNode_t
      *active_live_ranges; /* List of LiveRange_t* currently being tracked */
  ListNode_t *active_live_ranges_tail; /* Tail pointer for O(1) append */
  int next_live_range_id;              /* ID counter for live ranges */
#endif
} RegStack_t;

RegStack_t *init_reg_stack();

/* NOTE: Getters return number greater than 1 if it had to kick a value out to
 * temp */
/* The returned int is the temp offset to restore the value */
int get_register_by_id(RegStack_t *, RegisterId_t reg_id, Register_t **,
                       ListNode_t **inst_list);
void restore_register_64bit(RegStack_t *, Register_t *, int temp_offset);
void restore_register_32bit(RegStack_t *, Register_t *, int temp_offset);
void free_reg(RegStack_t *, Register_t *);
void swap_reg_stack(RegStack_t *);
Register_t *front_reg_stack(RegStack_t *);
Register_t *get_free_reg(RegStack_t *, ListNode_t **);
/* Force register allocation by spilling LRU register if needed */
Register_t *get_reg_with_spill(RegStack_t *, ListNode_t **);
void register_set_spill_callback(Register_t *reg,
                                 RegisterSpillCallback callback, void *context);
void register_clear_spill_callback(Register_t *reg);
int get_num_registers_free(RegStack_t *);
int get_num_registers_alloced(RegStack_t *);

/* Caller-save/restore around function calls.
 * All caller-saved registers are clobbered by `call` per the SysV ABI.
 * These helpers save allocated GP registers before a call and restore after. */
#define MAX_SAVED_CALLER_REGS 16
typedef struct {
  int count;
  struct {
    Register_t *reg;
    int spill_offset;
  } entries[MAX_SAVED_CALLER_REGS];
  int rax_was_saved;       /* was %rax among the saved regs? */
  int return_spill_offset; /* temp slot for saving the call return value */
} CallerSaveState;

/* Save all currently-allocated registers. Call BEFORE the `call` instruction.
 */
void regstack_caller_save(RegStack_t *reg_stack, ListNode_t **inst_list,
                          CallerSaveState *state);

/* Restore previously-saved registers. Call AFTER the `call` instruction.
 * Preserves %rax (the call's return value) even if %rax was saved. */
void regstack_caller_restore(RegStack_t *reg_stack, ListNode_t **inst_list,
                             CallerSaveState *state);

void free_reg_stack(RegStack_t *);

#if KGPC_ENABLE_REG_DEBUG
extern const char *g_reg_debug_context;
#endif

/********* Register_t **********/
enum RegisterId {
  REG_INVALID = 0,
  REG_RAX,
  REG_RBX,
  REG_RCX,
  REG_RDX,
  REG_RSI,
  REG_RDI,
  REG_RBP,
  REG_RSP,
  REG_R8,
  REG_R9,
  REG_R10,
  REG_R11,
  REG_R12,
  REG_R13,
  REG_R14,
  REG_R15
};

typedef struct Register {
  RegisterId_t reg_id;
  char *bit_64;
  char *bit_32;
  /* Spill tracking - if spilled, this points to the stack location */
  StackNode_t *spill_location;
  /* Sequence number for LRU tracking */
  unsigned long long last_use_seq;
  RegisterSpillCallback spill_callback;
  void *spill_context;
  int vreg_id; /* -1 = not yet assigned a virtual ID */

#if USE_GRAPH_COLORING_ALLOCATOR
  /* Forward declaration from graph_coloring_allocator.h */
  struct LiveRange
      *current_live_range; /* Active live range for this register */
#endif
} Register_t;

/* A target's allocatable register: an opaque slot id plus its 64/32-bit
 * assembly names.  Lets the register pool be target-provided instead of
 * hardcoded, so the shared allocator is target-neutral. */
typedef struct BackendRegSpec {
  RegisterId_t reg_id;
  const char *name64;
  const char *name32;
} BackendRegSpec;

/* Override the allocatable register pool used by init_reg_stack (and thus
 * reset_reg_stack).  Pass NULL/0 to restore the default x86-64 pool.  The
 * `specs` array must outlive all subsequent init/reset calls (targets pass
 * static tables). */
void stackmng_set_register_pool(const BackendRegSpec *specs, int n);

/********* StackScope_t **********/

/* Forward declaration; defined in stackmng.c. Used to accelerate
 * name lookups in stackscope_find_{t,x,z} from O(N) per-call to O(1). */
struct ScopeHashTable;

typedef struct StackScope {
  int t_offset, x_offset, z_offset;
  ListNode_t *t, *x, *z;
  ListNode_t *t_tail, *x_tail, *z_tail;

  /* Parallel hash indexes onto the t/x/z lists keyed by lowercased label.
   * The lists remain the source of truth (still iterated by callers in
   * codegen.c). All insertion and removal sites that touch t/x/z MUST
   * keep these indexes in sync via stackscope_index_{t,x,z}_insert /
   * stackscope_index_x_remove. */
  struct ScopeHashTable *t_index;
  struct ScopeHashTable *x_index;
  struct ScopeHashTable *z_index;

  StackScope_t *prev_scope;
} StackScope_t;

/* Hash-index maintenance helpers exposed for the few sites outside
 * stackmng.c that directly append to ->x (codegen_subprograms.c aliases). */
void stackscope_index_t_insert(StackScope_t *scope, ListNode_t *node);
void stackscope_index_x_insert(StackScope_t *scope, ListNode_t *node);
void stackscope_index_z_insert(StackScope_t *scope, ListNode_t *node);
void stackscope_index_x_remove(StackScope_t *scope, ListNode_t *node);

StackScope_t *init_stackscope();
StackNode_t *stackscope_find_t(StackScope_t *, const char *);
StackNode_t *stackscope_find_x(StackScope_t *, const char *);
StackNode_t *stackscope_find_z(StackScope_t *, const char *);
StackScope_t *free_stackscope(StackScope_t *);
void free_all_stackscopes(StackScope_t *);
void free_stackscope_list(ListNode_t *);

/********* StackNode_t **********/
/* Used to encapsulate offset, label, and size */
typedef struct StackNode {
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
StackNode_t *add_static_array(char *label, int total_size, int element_size,
                              int lower_bound, const char *static_label);
void destroy_stack_node(StackNode_t *);

#endif
