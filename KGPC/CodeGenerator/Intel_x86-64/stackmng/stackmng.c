/*
    Damon Gwinn
    Stack Manager structure (see codegen.h for details)
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>
#include "stackmng.h"
#include "../register_types.h"
#include "../codegen.h"
#include "../../../Parser/List/List.h"
#include "../../../identifier_utils.h"

#if USE_GRAPH_COLORING_ALLOCATOR
#include "../graph_coloring_allocator.h"
#endif

#if KGPC_ENABLE_REG_DEBUG
const char *g_reg_debug_context = "default";
#define REG_DEBUG_LOG(...) fprintf(stderr, __VA_ARGS__)
#else
#define REG_DEBUG_LOG(...) ((void)0)
#endif

/* Sets num_args_alloced to 0 */
void free_arg_regs(void)
{
    num_args_alloced = 0;
}

/* Helpers for getting special registers */
/* TODO: Support loading arguments into temp if needed */
const char *get_arg_reg64_num(int num)
{
    return current_arg_reg64(num);
}

const char *get_arg_reg32_num(int num)
{
    return current_arg_reg32(num);
}

/******** stackmng *********/
stackmng_t *global_stackmng = NULL;

void init_stackmng()
{
    assert(global_stackmng == NULL);

    num_args_alloced = 0;
    global_stackmng = (stackmng_t *)malloc(sizeof(stackmng_t));
    assert(global_stackmng != NULL);

    global_stackmng->cur_scope = NULL;
    global_stackmng->reg_stack = init_reg_stack();
}

/* For when you need low level access to the stack */
StackScope_t *get_cur_scope()
{
    return global_stackmng->cur_scope;
}

int get_full_stack_offset()
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    StackScope_t *scope;
    int actual_offset, div, rem;

    scope = global_stackmng->cur_scope;

    /* On Windows x64, we need to reserve shadow space at the top of the frame */
    /* The shadow space is reserved for the callee to spill register arguments */
    /* Locals should be placed below the shadow space */
    /* Total stack space needed = locals_size + shadow_space_size */
    actual_offset = scope->t_offset + scope->x_offset + scope->z_offset + current_stack_home_space();

    /* x86_64 requires stack offsets to avoid undefined behavior */
    div = actual_offset / REQUIRED_OFFSET;
    rem = actual_offset % REQUIRED_OFFSET;

    if(rem > 0)
        ++div;

    return div * REQUIRED_OFFSET;

}

void push_stackscope()
{
    assert(global_stackmng != NULL);

    StackScope_t *new_scope;
    new_scope = init_stackscope();

    new_scope->prev_scope = global_stackmng->cur_scope;
    global_stackmng->cur_scope = new_scope;
}

void push_stackscope_inherited()
{
    assert(global_stackmng != NULL);

    StackScope_t *parent = global_stackmng->cur_scope;
    StackScope_t *new_scope = init_stackscope();

    /* Inherit the parent's cumulative offset so that new variables
     * in this child scope are placed at non-overlapping stack slots. */
    if (parent != NULL) {
        new_scope->z_offset = parent->z_offset + parent->x_offset + parent->t_offset;
    }

    new_scope->prev_scope = parent;
    global_stackmng->cur_scope = new_scope;
}

void pop_stackscope()
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);

    global_stackmng->cur_scope = free_stackscope(global_stackmng->cur_scope);
}

static inline int align_up(int value, int alignment)
{
    if (alignment <= 0)
        return value;
    int remainder = value % alignment;
    if (remainder == 0)
        return value;
    return value + (alignment - remainder);
}

static inline int stack_slot_alignment(void)
{
    int alignment = (int)sizeof(void *);
    if (alignment < DOUBLEWORD)
        alignment = DOUBLEWORD;
    return alignment;
}

/* Adds temporary storage to t */
StackNode_t *add_l_t(char *label)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope;
    StackNode_t *new_node;
    int offset;

    /* Reserve space that can hold pointer-sized temporaries. */
    int temp_size = (int)sizeof(void *);
    if (temp_size < DOUBLEWORD)
        temp_size = DOUBLEWORD;

    cur_scope = global_stackmng->cur_scope;

    int alignment = stack_slot_alignment();
    cur_scope->t_offset = align_up(cur_scope->t_offset, alignment);
    cur_scope->t_offset = align_up(cur_scope->t_offset, temp_size);
    cur_scope->t_offset += temp_size;

    offset = cur_scope->z_offset + cur_scope->x_offset + cur_scope->t_offset;

    new_node = init_stack_node(offset, label, temp_size);

    if(cur_scope->t == NULL)
    {
        cur_scope->t = CreateListNode(new_node, LIST_UNSPECIFIED);
    }
    else
    {
        cur_scope->t = PushListNodeBack(cur_scope->t,
            CreateListNode(new_node, LIST_UNSPECIFIED));
    }

    #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: Added %s to t_offset %d\n", label, offset);
    #endif

    return new_node;
}

StackNode_t *add_l_t_bytes(char *label, int size)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope = global_stackmng->cur_scope;
    StackNode_t *new_node;

    if (size <= 0)
        size = DOUBLEWORD;
    int alignment = stack_slot_alignment();
    int aligned_size = align_up(size, alignment);

    cur_scope->t_offset = align_up(cur_scope->t_offset, alignment);
    cur_scope->t_offset += aligned_size;

    int offset = cur_scope->z_offset + cur_scope->x_offset + cur_scope->t_offset;

    new_node = init_stack_node(offset, label, aligned_size);
    new_node->element_size = size;

    if (cur_scope->t == NULL)
        cur_scope->t = CreateListNode(new_node, LIST_UNSPECIFIED);
    else
        cur_scope->t = PushListNodeBack(cur_scope->t,
            CreateListNode(new_node, LIST_UNSPECIFIED));

    #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: Added %s to t_offset %d (bytes=%d)\n",
            label, offset, aligned_size);
    #endif

    return new_node;
}

/* Adds storage to x */
StackNode_t *add_l_x(char *label, int size)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope;
    StackNode_t *new_node;
    int offset;

    cur_scope = global_stackmng->cur_scope;

    if (size <= 0)
        size = DOUBLEWORD;

    int alignment = stack_slot_alignment();
    int aligned_size = align_up(size, alignment);

    cur_scope->x_offset = align_up(cur_scope->x_offset, alignment);
    cur_scope->x_offset += aligned_size;

    /* Locals are placed below the shadow space and must not overlap temp slots.
     * After prologue: RSP = RBP - 8, then we subtract frame_size.
     * Shadow space is at [RSP .. RSP+31] = [RBP-8-frame_size .. RBP-8-frame_size+31].
     * Locals should be at offsets more negative than RBP-32. */
    offset = cur_scope->z_offset + cur_scope->x_offset + cur_scope->t_offset;

    new_node = init_stack_node(offset, label, aligned_size);
    new_node->element_size = size;

    if(cur_scope->x == NULL)
    {
        cur_scope->x = CreateListNode(new_node, LIST_UNSPECIFIED);
    }
    else
    {
        cur_scope->x = PushListNodeBack(cur_scope->x,
            CreateListNode(new_node, LIST_UNSPECIFIED));
    }

    #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: Added %s to x_offset %d\n", new_node->label, new_node->offset);
    #endif

    return new_node;
}

void remove_last_l_x(char *label)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope = global_stackmng->cur_scope;
    ListNode_t *cur_li = cur_scope->x;
    ListNode_t *prev_li = NULL;
    ListNode_t *last_match_prev = NULL;
    ListNode_t *last_match = NULL;

    /* Find the LAST node with this label */
    while (cur_li != NULL) {
        StackNode_t *node = (StackNode_t *)cur_li->cur;
        if (pascal_identifier_equals(node->label, label)) {
            last_match_prev = prev_li;
            last_match = cur_li;
        }
        prev_li = cur_li;
        cur_li = cur_li->next;
    }

    if (last_match == NULL)
        return;

    /* Remove from linked list */
    if (last_match_prev != NULL)
        last_match_prev->next = last_match->next;
    else
        cur_scope->x = last_match->next;

    /* Free the node */
    StackNode_t *node = (StackNode_t *)last_match->cur;
    if (node != NULL) {
        if (node->label != NULL)
            free(node->label);
        free(node);
    }
    free(last_match);
}

StackNode_t *add_array(char *label, int total_size, int element_size, int lower_bound)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope = global_stackmng->cur_scope;

    cur_scope->x_offset += total_size;

    /* Locals are placed below the shadow space and must not overlap temp slots. */
    int offset = cur_scope->z_offset + cur_scope->x_offset + cur_scope->t_offset;

    StackNode_t *new_node = init_stack_node(offset, label, total_size);
    new_node->is_array = 1;
    new_node->array_lower_bound = lower_bound;
    new_node->element_size = element_size;
    new_node->is_dynamic = 0;

    if(cur_scope->x == NULL)
    {
        cur_scope->x = CreateListNode(new_node, LIST_UNSPECIFIED);
    }
    else
    {
        cur_scope->x = PushListNodeBack(cur_scope->x,
            CreateListNode(new_node, LIST_UNSPECIFIED));
    }

    #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: Added array %s to x_offset %d\n", new_node->label, new_node->offset);
    #endif

    return new_node;
}

StackNode_t *add_dynamic_array(char *label, int element_size, int lower_bound,
    int use_static_storage, const char *static_label)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope = global_stackmng->cur_scope;

    int descriptor_size = 4 * DOUBLEWORD;
    if (descriptor_size < 2 * element_size)
        descriptor_size = 2 * element_size;

    if (!use_static_storage)
        cur_scope->x_offset += descriptor_size;

    /* Locals are placed below the shadow space and must not overlap temp slots. */
    int offset = cur_scope->z_offset + cur_scope->x_offset + cur_scope->t_offset;
    if (use_static_storage)
        offset = 0;

    StackNode_t *new_node = init_stack_node(offset, label, descriptor_size);
    new_node->is_array = 1;
    new_node->array_lower_bound = lower_bound;
    new_node->element_size = element_size;
    new_node->is_dynamic = 1;
    if (use_static_storage)
    {
        new_node->is_static = 1;
        if (static_label != NULL)
            new_node->static_label = strdup(static_label);
    }

    if(cur_scope->x == NULL)
    {
        cur_scope->x = CreateListNode(new_node, LIST_UNSPECIFIED);
    }
    else
    {
        cur_scope->x = PushListNodeBack(cur_scope->x,
            CreateListNode(new_node, LIST_UNSPECIFIED));
    }

    #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: Added dynamic array %s descriptor to x_offset %d\n", new_node->label, new_node->offset);
    #endif

    return new_node;
}

StackNode_t *add_static_var(char *label, int size, const char *static_label)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope = global_stackmng->cur_scope;
    StackNode_t *new_node = init_stack_node(0, label, size > 0 ? size : DOUBLEWORD);
    new_node->is_static = 1;
    if (static_label != NULL)
        new_node->static_label = strdup(static_label);

    ListNode_t *list_node = CreateListNode(new_node, LIST_UNSPECIFIED);
    if (cur_scope->x == NULL)
        cur_scope->x = list_node;
    else
        cur_scope->x = PushListNodeBack(cur_scope->x, list_node);
    return new_node;
}

/* Adds doubleword to z */
StackNode_t *add_l_z(char *label)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope;
    StackNode_t *new_node;
    int offset;

    cur_scope = global_stackmng->cur_scope;

    cur_scope->z_offset += DOUBLEWORD;

    /* Arguments are placed below the shadow space */
    /* After prologue: RSP = RBP - 8, then we subtract frame_size */
    /* Shadow space is at [RSP .. RSP+31] = [RBP-8-frame_size .. RBP-8-frame_size+31] */
    /* Arguments should be at offsets more negative than RBP-32 */
    offset = cur_scope->z_offset;

    new_node = init_stack_node(offset, label, DOUBLEWORD);

    if(cur_scope->z == NULL)
    {
        cur_scope->z = CreateListNode(new_node, LIST_UNSPECIFIED);
    }
    else
    {
        cur_scope->z = PushListNodeBack(cur_scope->z,
            CreateListNode(new_node, LIST_UNSPECIFIED));
    }

    #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: Added %s to z_offset %d\n", label, offset);
    #endif

    return new_node;
}

/* Adds quadword (8 bytes) to z */
StackNode_t *add_q_z(char *label)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope;
    StackNode_t *new_node;
    int offset;

    cur_scope = global_stackmng->cur_scope;

    cur_scope->z_offset += 8;  // quadword size

    /* Arguments are placed below the shadow space */
    /* After prologue: RSP = RBP - 8, then we subtract frame_size */
    /* Shadow space is at [RSP .. RSP+31] = [RBP-8-frame_size .. RBP-8-frame_size+31] */
    /* Arguments should be at offsets more negative than RBP-32 */
    offset = cur_scope->z_offset;

    new_node = init_stack_node(offset, label, 8);

    if(cur_scope->z == NULL)
    {
        cur_scope->z = CreateListNode(new_node, LIST_UNSPECIFIED);
    }
    else
    {
        cur_scope->z = PushListNodeBack(cur_scope->z,
            CreateListNode(new_node, LIST_UNSPECIFIED));
    }

    #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: Added %s to z_offset %d (quadword)\n", label, offset);
    #endif

    return new_node;
}

/* Reserved z_offset value for temporary reservation */
static int g_reserved_z_offset = 0;

/* Reserves z_offset space to prevent overlap between x pool (temp slots) and z pool (final storage).
 * This should be called before allocating temp slots so they are placed after the z pool region. */
void reserve_z_offset(int size)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    
    g_reserved_z_offset = global_stackmng->cur_scope->z_offset;
    global_stackmng->cur_scope->z_offset = size;
    
    #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: Reserved z_offset to %d (was %d)\n", size, g_reserved_z_offset);
    #endif
}

/* Unreserves z_offset by restoring the original value.
 * This should be called after allocating temp slots so z allocations start from the beginning. */
void unreserve_z_offset(void)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    
    #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: Unreserved z_offset from %d to %d\n",
            global_stackmng->cur_scope->z_offset, g_reserved_z_offset);
    #endif
    
    global_stackmng->cur_scope->z_offset = g_reserved_z_offset;
    g_reserved_z_offset = 0;
}

RegStack_t *get_reg_stack()
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->reg_stack != NULL);

    return global_stackmng->reg_stack;
}

/* Finds variable on temp in current stack */
/* Returns NULL if not found */
StackNode_t *find_in_temp(char *label)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope;
    StackNode_t *cur_node;

    cur_scope = global_stackmng->cur_scope;

    cur_node = stackscope_find_t(cur_scope, label);
    return cur_node;
}

/* TODO: Does not find variables outside the current scope */
StackNode_t *find_label(char *label)
{
    int depth = 0;
    return find_label_with_depth(label, &depth);
}

/* Returns the scope depth (0 = current scope, 1 = parent, etc.) */
StackNode_t *find_label_with_depth(char *label, int *depth)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);
    assert(depth != NULL);

    StackScope_t *cur_scope;
    StackNode_t *cur_node;
    int scope_depth = 0;

    cur_scope = global_stackmng->cur_scope;

    while (cur_scope != NULL)
    {
        cur_node = stackscope_find_z(cur_scope, label);
        if(cur_node != NULL)
        {
            *depth = scope_depth;
            return cur_node;
        }

        cur_node = stackscope_find_x(cur_scope, label);
        if(cur_node != NULL)
        {
            *depth = scope_depth;
            return cur_node;
        }

        cur_node = stackscope_find_t(cur_scope, label);
        if(cur_node != NULL)
        {
            *depth = scope_depth;
            return cur_node;
        }

        cur_scope = cur_scope->prev_scope;
        scope_depth++;
    }

    return NULL;
}

void free_stackmng()
{
    assert(global_stackmng != NULL);

    free_all_stackscopes(global_stackmng->cur_scope);
    free_reg_stack(global_stackmng->reg_stack);
    free(global_stackmng);
    global_stackmng = NULL;
}


/********* RegStack_t **********/

static void apply_register_limit(RegStack_t *reg_stack)
{
    if (reg_stack == NULL || reg_stack->registers_free == NULL)
        return;

    const char *env_limit = getenv("KGPC_FORCE_REGISTER_LIMIT");
    if (env_limit == NULL || env_limit[0] == '\0')
        return;

    char *endptr = NULL;
    long limit = strtol(env_limit, &endptr, 10);
    if (endptr == env_limit || limit <= 0)
        return;

    ListNode_t *cur = reg_stack->registers_free;
    ListNode_t *prev = NULL;
    int retained = 0;

    while (cur != NULL && retained < limit)
    {
        prev = cur;
        cur = cur->next;
        retained++;
    }

    if (cur == NULL)
    {
        reg_stack->num_registers = retained;
        return;
    }

    if (prev != NULL)
        prev->next = NULL;
    else
        reg_stack->registers_free = NULL;

    while (cur != NULL)
    {
        ListNode_t *next = cur->next;
        Register_t *reg = (Register_t *)cur->cur;
        if (reg != NULL)
        {
            free(reg->bit_64);
            free(reg->bit_32);
            free(reg);
        }
        free(cur);
        cur = next;
    }

    reg_stack->num_registers = retained;
}

RegStack_t *init_reg_stack()
{
    /*
     * Initialize register pool for expression evaluation.
     * 
     * Strategy: Reserve the MOST COMMONLY USED argument registers to minimize
     * conflicts with function calls, but keep enough registers available for
     * complex expressions.
     * 
     * Windows x64: First 4 args in %rcx, %rdx, %r8, %r9
     *   - Reserve: %rcx, %rdx (most common - used for 1-2 arg functions)
     *   - Available: %rax, %rsi, %rdi, %r8, %r9, %r10, %r11 (7 registers)
     * 
     * Linux x64 (SysV): First 6 args in %rdi, %rsi, %rdx, %rcx, %r8, %r9
     *   - Reserve: %rdi, %rsi, %rdx (most common - used for 1-3 arg functions)
     *   - Available: %rax, %rcx, %r8, %r9, %r10, %r11 (6 registers)
     * 
     * This approach:
     * 1. Prevents conflicts for the most common case (1-3 argument functions)
     * 2. Keeps enough registers for complex expressions
     * 3. The conflict handling code in codegen_statement.c handles cases
     *    where we need r8/r9 for 3-4 argument functions
     */
    ListNode_t *registers = NULL;
    RegStack_t *reg_stack;
    reg_stack = (RegStack_t *)malloc(sizeof(RegStack_t));
    assert(reg_stack != NULL);

    /* %rax - return register, always safe to use for expressions */
    Register_t *rax = (Register_t *)malloc(sizeof(Register_t));
    assert(rax != NULL);
    rax->bit_64 = strdup("%rax");
    rax->bit_32 = strdup("%eax");
    rax->spill_location = NULL;
    rax->last_use_seq = 0;
    rax->spill_callback = NULL;
    rax->spill_context = NULL;
#if USE_GRAPH_COLORING_ALLOCATOR
    rax->current_live_range = NULL;
#endif

    /* %r10, %r11 - caller-saved, never used for arguments */
    Register_t *r10 = (Register_t *)malloc(sizeof(Register_t));
    assert(r10 != NULL);
    r10->bit_64 = strdup("%r10");
    r10->bit_32 = strdup("%r10d");
    r10->spill_location = NULL;
    r10->last_use_seq = 0;
    r10->spill_callback = NULL;
    r10->spill_context = NULL;
#if USE_GRAPH_COLORING_ALLOCATOR
    r10->current_live_range = NULL;
#endif

    Register_t *r11 = (Register_t *)malloc(sizeof(Register_t));
    assert(r11 != NULL);
    r11->bit_64 = strdup("%r11");
    r11->bit_32 = strdup("%r11d");
    r11->spill_location = NULL;
    r11->last_use_seq = 0;
    r11->spill_callback = NULL;
    r11->spill_context = NULL;
#if USE_GRAPH_COLORING_ALLOCATOR
    r11->current_live_range = NULL;
#endif

    /* %r8, %r9 - argument registers 3 and 4 (both ABIs), less commonly used */
    Register_t *r8 = (Register_t *)malloc(sizeof(Register_t));
    assert(r8 != NULL);
    r8->bit_64 = strdup("%r8");
    r8->bit_32 = strdup("%r8d");
    r8->spill_location = NULL;
    r8->last_use_seq = 0;
    r8->spill_callback = NULL;
    r8->spill_context = NULL;
#if USE_GRAPH_COLORING_ALLOCATOR
    r8->current_live_range = NULL;
#endif

    Register_t *r9 = (Register_t *)malloc(sizeof(Register_t));
    assert(r9 != NULL);
    r9->bit_64 = strdup("%r9");
    r9->bit_32 = strdup("%r9d");
    r9->spill_location = NULL;
    r9->last_use_seq = 0;
    r9->spill_callback = NULL;
    r9->spill_context = NULL;
#if USE_GRAPH_COLORING_ALLOCATOR
    r9->current_live_range = NULL;
#endif

    /* Build base register list */
    registers = CreateListNode(rax, LIST_UNSPECIFIED);
    registers = PushListNodeBack(registers, CreateListNode(r10, LIST_UNSPECIFIED));
    registers = PushListNodeBack(registers, CreateListNode(r11, LIST_UNSPECIFIED));
    registers = PushListNodeBack(registers, CreateListNode(r8, LIST_UNSPECIFIED));
    registers = PushListNodeBack(registers, CreateListNode(r9, LIST_UNSPECIFIED));

    /* Add platform-specific registers */
    if (g_current_codegen_abi == KGPC_TARGET_ABI_WINDOWS)
    {
        /* Windows: %rsi and %rdi are not argument registers */
        Register_t *rsi = (Register_t *)malloc(sizeof(Register_t));
        assert(rsi != NULL);
        rsi->bit_64 = strdup("%rsi");
        rsi->bit_32 = strdup("%esi");
        rsi->spill_location = NULL;
        rsi->last_use_seq = 0;
        rsi->spill_callback = NULL;
        rsi->spill_context = NULL;
#if USE_GRAPH_COLORING_ALLOCATOR
        rsi->current_live_range = NULL;
#endif

        Register_t *rdi = (Register_t *)malloc(sizeof(Register_t));
        assert(rdi != NULL);
        rdi->bit_64 = strdup("%rdi");
        rdi->bit_32 = strdup("%edi");
        rdi->spill_location = NULL;
        rdi->last_use_seq = 0;
        rdi->spill_callback = NULL;
        rdi->spill_context = NULL;
#if USE_GRAPH_COLORING_ALLOCATOR
        rdi->current_live_range = NULL;
#endif

        registers = PushListNodeBack(registers, CreateListNode(rsi, LIST_UNSPECIFIED));
        registers = PushListNodeBack(registers, CreateListNode(rdi, LIST_UNSPECIFIED));
    }
    else
    {
        /* Linux/SysV: %rcx is argument register 4, less commonly used than rdi/rsi/rdx */
        Register_t *rcx = (Register_t *)malloc(sizeof(Register_t));
        assert(rcx != NULL);
        rcx->bit_64 = strdup("%rcx");
        rcx->bit_32 = strdup("%ecx");
        rcx->spill_location = NULL;
        rcx->last_use_seq = 0;
        rcx->spill_callback = NULL;
        rcx->spill_context = NULL;
#if USE_GRAPH_COLORING_ALLOCATOR
        rcx->current_live_range = NULL;
#endif

        registers = PushListNodeBack(registers, CreateListNode(rcx, LIST_UNSPECIFIED));
    }

    reg_stack->registers_allocated = NULL;
    reg_stack->registers_free = registers;
    reg_stack->num_registers = NUM_CALLER_SAVED_REGISTERS;
    reg_stack->use_sequence = 0;

#if USE_GRAPH_COLORING_ALLOCATOR
    reg_stack->active_live_ranges = NULL;
    reg_stack->next_live_range_id = 1;
#endif

    apply_register_limit(reg_stack);

    return reg_stack;
}

/* NOTE: Getters return number greater than 0 if it had to kick a value out to temp */
/* The returned int is the temp offset to restore the value */
/* TODO: Doesn't actually kick variable out to temp yet */
int get_register_64bit(RegStack_t *regstack, char *reg_64, Register_t **return_reg)
{
    assert(regstack != NULL);
    assert(reg_64 != NULL);

    ListNode_t *cur_reg, *prev_reg;
    Register_t *reg;

    cur_reg = regstack->registers_free;
    prev_reg = NULL;
    while(cur_reg != NULL)
    {
        reg = (Register_t *)cur_reg->cur;
        if(strcmp(reg->bit_64, reg_64) == 0)
        {
            if(prev_reg == NULL)
                regstack->registers_free = cur_reg->next;
            else
                prev_reg->next = cur_reg->next;

            cur_reg->next = regstack->registers_allocated;
            regstack->registers_allocated = cur_reg;
            *return_reg = reg;

            return 0;
        }

        prev_reg = cur_reg;
        cur_reg = cur_reg->next;
    }

    assert(0 && "Kicking out values in registers not currently supported!");
}

/* NOTE: Getters return number greater than 0 if it had to kick a value out to temp */
/* The returned int is the temp offset to restore the value */
/* TODO: Doesn't actually kick variable out to temp yet */
int get_register_32bit(RegStack_t *regstack, char *reg_32, Register_t **return_reg)
{
    assert(regstack != NULL);
    assert(reg_32 != NULL);

    ListNode_t *cur_reg, *prev_reg;
    Register_t *reg;

    cur_reg = regstack->registers_free;
    prev_reg = NULL;
    while(cur_reg != NULL)
    {
        reg = (Register_t *)cur_reg->cur;
        if(strcmp(reg->bit_32, reg_32) == 0)
        {
            if(prev_reg == NULL)
                regstack->registers_free = cur_reg->next;
            else
                prev_reg->next = cur_reg->next;

            cur_reg->next = regstack->registers_allocated;
            regstack->registers_allocated = cur_reg;
            *return_reg = reg;

            return 0;
        }

        prev_reg = cur_reg;
        cur_reg = cur_reg->next;
    }

    assert(0 && "Kicking out values in registers not currently supported!");
}

void free_reg(RegStack_t *reg_stack, Register_t *reg)
{
    assert(reg_stack != NULL);
    assert(reg != NULL);

    ListNode_t *cur, *prev;

    cur = reg_stack->registers_allocated;
    prev = NULL;
    while(cur != NULL)
    {
        if(cur->cur == reg)
        {
            if(prev == NULL)
                reg_stack->registers_allocated = cur->next;
            else
                prev->next = cur->next;

            register_clear_spill_callback(reg);
            cur->next = reg_stack->registers_free;
            reg_stack->registers_free = cur;
            
#if USE_GRAPH_COLORING_ALLOCATOR
            /* End the live range for this register */
            if (reg->current_live_range != NULL)
            {
                reg->current_live_range->end_pos = reg_stack->use_sequence;
                reg->current_live_range = NULL;
            }
#endif
            
#if KGPC_ENABLE_REG_DEBUG
            if (strcmp(reg->bit_64, "%rcx") == 0 && g_reg_debug_context != NULL)
                fprintf(stderr, "[reg-debug] free  %s (%s)\n", reg->bit_64, g_reg_debug_context);
#endif
            return;
        }
        prev = cur;
        cur = cur->next;
    }

    return;
}

void register_set_spill_callback(Register_t *reg, RegisterSpillCallback callback, void *context)
{
    if (reg == NULL)
        return;
    reg->spill_callback = callback;
    reg->spill_context = context;
}

void register_clear_spill_callback(Register_t *reg)
{
    if (reg == NULL)
        return;
    reg->spill_callback = NULL;
    reg->spill_context = NULL;
}

void swap_reg_stack(RegStack_t *reg_stack)
{
    /* Need at least two registers to do a swap */
    assert(reg_stack != NULL);
    assert(reg_stack->registers_free != NULL);
    assert(reg_stack->registers_free->next != NULL);

    ListNode_t *top, *next;
    top = reg_stack->registers_free;
    next = top->next;

    top->next = next->next;
    next->next = top;
    reg_stack->registers_free = next;
}

Register_t *front_reg_stack(RegStack_t *reg_stack)
{
    assert(reg_stack != NULL);

    return (Register_t *)reg_stack->registers_free->cur;
}

/* Register spilling: When out of registers, spill the least recently used one */
Register_t *get_free_reg(RegStack_t *reg_stack, ListNode_t **inst_list)
{
    assert(reg_stack != NULL);
    assert(inst_list != NULL);

    ListNode_t *register_node;
    Register_t *reg;

    if(reg_stack->registers_free != NULL)
    {
        register_node = reg_stack->registers_free;
        reg_stack->registers_free = reg_stack->registers_free->next;

        register_node->next = reg_stack->registers_allocated;
        reg_stack->registers_allocated = register_node;

        reg = (Register_t *)register_node->cur;
        reg->last_use_seq = ++reg_stack->use_sequence;
        
        /* Clear spill location when register is freshly allocated */
        reg->spill_location = NULL;
        
#if USE_GRAPH_COLORING_ALLOCATOR
        /* Create a new live range for this register allocation */
        LiveRange_t *lr = create_live_range(reg_stack->next_live_range_id++, 
                                            reg_stack->use_sequence, -1);
        if (lr != NULL)
        {
            lr->preferred_reg = reg;
            reg->current_live_range = lr;
            
            /* Add to active live ranges list */
            if (reg_stack->active_live_ranges == NULL)
                reg_stack->active_live_ranges = CreateListNode(lr, LIST_UNSPECIFIED);
            else
                reg_stack->active_live_ranges = PushListNodeBack(reg_stack->active_live_ranges,
                                                                 CreateListNode(lr, LIST_UNSPECIFIED));
        }
#endif
        
#if KGPC_ENABLE_REG_DEBUG
        if (strcmp(reg->bit_64, "%rcx") == 0 && g_reg_debug_context != NULL)
            fprintf(stderr, "[reg-debug] alloc %s (%s)\n", reg->bit_64, g_reg_debug_context);
#endif
        return reg;
    }
    else
    {
        /* No free registers - caller should use get_reg_with_spill */
        REG_DEBUG_LOG("[reg-spill] Out of registers - caller should handle this\n");
        return NULL;
    }
}

/* Force register allocation by spilling using graph coloring or LRU */
Register_t *get_reg_with_spill(RegStack_t *reg_stack, ListNode_t **inst_list)
{
    assert(reg_stack != NULL);
    assert(inst_list != NULL);

    /* First try normal allocation */
    Register_t *reg = get_free_reg(reg_stack, inst_list);
    if (reg != NULL)
        return reg;

    /* Out of registers - must spill */
    Register_t *spill_reg = NULL;

#if USE_GRAPH_COLORING_ALLOCATOR
    /* ACTUAL GRAPH COLORING IMPLEMENTATION */
    
    /* Build interference graph from active live ranges */
    InterferenceGraph_t *graph = create_interference_graph(reg_stack->num_registers);
    if (graph != NULL && reg_stack->active_live_ranges != NULL)
    {
        /* Copy active live ranges to the graph */
        ListNode_t *lr_node = reg_stack->active_live_ranges;
        while (lr_node != NULL)
        {
            LiveRange_t *lr = (LiveRange_t *)lr_node->cur;
            if (lr != NULL && lr->end_pos == -1)  /* Only active (not ended) ranges */
            {
                add_live_range(graph, lr);
            }
            lr_node = lr_node->next;
        }
        
        /* Run the graph coloring algorithm */
        ListNode_t *spilled_ranges = allocate_registers_graph_coloring(graph);
        
        /* Find which register to spill based on graph coloring result */
        if (spilled_ranges != NULL && spilled_ranges->cur != NULL)
        {
            LiveRange_t *spilled_lr = (LiveRange_t *)spilled_ranges->cur;
            spill_reg = spilled_lr->preferred_reg;
            REG_DEBUG_LOG("[reg-spill] Graph coloring selected %s for spilling\n",
                spill_reg != NULL ? spill_reg->bit_64 : "NULL");
            
            DestroyList(spilled_ranges);
        }
        
        free_interference_graph(graph);
    }
    
    /* Fallback to LRU if graph coloring didn't select a register */
    if (spill_reg == NULL)
    {
        REG_DEBUG_LOG("[reg-spill] Graph coloring failed, falling back to LRU\n");
        ListNode_t *cur_node = reg_stack->registers_allocated;
        unsigned long long oldest_seq = ULLONG_MAX;
        
        while (cur_node != NULL)
        {
            Register_t *cur_reg = (Register_t *)cur_node->cur;
            if (cur_reg->last_use_seq < oldest_seq)
            {
                oldest_seq = cur_reg->last_use_seq;
                spill_reg = cur_reg;
            }
            cur_node = cur_node->next;
        }
    }
    
#else
    /* LRU-based spilling: Find least recently used register */
    ListNode_t *cur_node = reg_stack->registers_allocated;
    unsigned long long oldest_seq = ULLONG_MAX;

    while (cur_node != NULL)
    {
        Register_t *cur_reg = (Register_t *)cur_node->cur;
        if (cur_reg->last_use_seq < oldest_seq)
        {
            oldest_seq = cur_reg->last_use_seq;
            spill_reg = cur_reg;
        }
        cur_node = cur_node->next;
    }
#endif

    if (spill_reg == NULL)
    {
        REG_DEBUG_LOG("[reg-spill] ERROR: No registers available to spill\n");
        return NULL;
    }

    /* Allocate stack slot for this spill */
    char spill_label[64];
    snprintf(spill_label, sizeof(spill_label), "spill_%s_%llu",
        spill_reg->bit_64 + 1, (unsigned long long)(reg_stack->use_sequence + 1));
    StackNode_t *spill_slot = add_l_t_bytes(spill_label, (int)sizeof(void *));
    if (spill_slot == NULL)
    {
        REG_DEBUG_LOG("[reg-spill] ERROR: Failed to allocate spill slot\n");
        return NULL;
    }
    spill_reg->spill_location = spill_slot;

    /* Generate spill code */
    char spill_code[128];
#if USE_GRAPH_COLORING_ALLOCATOR
    snprintf(spill_code, sizeof(spill_code), "\t# Spill %s (graph-coloring)\n\tmovq\t%s, -%d(%%rbp)\n",
        spill_reg->bit_64, spill_reg->bit_64, spill_slot->offset);
#else
    snprintf(spill_code, sizeof(spill_code), "\t# Spill %s (LRU)\n\tmovq\t%s, -%d(%%rbp)\n",
        spill_reg->bit_64, spill_reg->bit_64, spill_slot->offset);
#endif
    *inst_list = add_inst(*inst_list, spill_code);

    REG_DEBUG_LOG("[reg-spill] Spilled %s (seq %llu) to offset -%d\n",
        spill_reg->bit_64, spill_reg->last_use_seq, spill_slot->offset);

    if (spill_reg->spill_callback != NULL)
        spill_reg->spill_callback(spill_reg, spill_slot, spill_reg->spill_context);
    register_clear_spill_callback(spill_reg);

    /* Update timestamp */
    spill_reg->last_use_seq = ++reg_stack->use_sequence;

    return spill_reg;
}

int get_num_registers_free(RegStack_t *reg_stack)
{
    assert(reg_stack != NULL);
    return ListLength(reg_stack->registers_free);
}

int get_num_registers_alloced(RegStack_t *reg_stack)
{
    assert(reg_stack != NULL);
    return ListLength(reg_stack->registers_allocated);
}

void free_reg_stack(RegStack_t *reg_stack)
{
    assert(reg_stack != NULL);

    if(ListLength(reg_stack->registers_allocated) != 0)
    {
        fprintf(stderr, "WARNING: Not all registers freed, %d still remaining!\n",
            ListLength(reg_stack->registers_allocated));
    }

    ListNode_t *cur;
    Register_t *reg;
    while(reg_stack->registers_free != NULL)
    {
        cur = reg_stack->registers_free;
        reg_stack->registers_free = cur->next;

        reg = (Register_t *)cur->cur;

        free(reg->bit_64);
        free(reg->bit_32);
        free(reg);
        free(cur);
    }

    /* Just in case something was left allocated */
    while(reg_stack->registers_allocated != NULL)
    {
        cur = reg_stack->registers_allocated;
        reg_stack->registers_allocated = cur->next;

        reg = (Register_t *)cur->cur;

        free(reg->bit_64);
        free(reg->bit_32);
        free(reg);
        free(cur);
    }


    free(reg_stack);
}


/*********** StackScope *************/

StackScope_t *init_stackscope()
{
    StackScope_t *new_scope;
    new_scope = (StackScope_t *)malloc(sizeof(StackScope_t));

    new_scope->t_offset = 0;
    new_scope->x_offset = 0;
    new_scope->z_offset = 0;

    new_scope->t = NULL;
    new_scope->x = NULL;
    new_scope->z = NULL;

    new_scope->prev_scope = NULL;

    return new_scope;
}

StackNode_t *stackscope_find_t(StackScope_t *cur_scope, char *label)
{
    ListNode_t *cur_li;
    StackNode_t *cur_node;

    assert(cur_scope != NULL);
    assert(label != NULL);

    cur_li = cur_scope->t;
    while(cur_li != NULL)
    {
        cur_node = (StackNode_t *)cur_li->cur;
        if(pascal_identifier_equals(cur_node->label, label))
        {
            return cur_node;
        }

        cur_li = cur_li->next;
    }

    return NULL;
}

StackNode_t *stackscope_find_x(StackScope_t *cur_scope, char *label)
{
    ListNode_t *cur_li;
    StackNode_t *cur_node;
    StackNode_t *alias_match = NULL;

    assert(cur_scope != NULL);
    assert(label != NULL);

    cur_li = cur_scope->x;
    while(cur_li != NULL)
    {
        cur_node = (StackNode_t *)cur_li->cur;
        if(pascal_identifier_equals(cur_node->label, label))
        {
            if (!cur_node->is_alias)
                return cur_node;
            if (alias_match == NULL)
                alias_match = cur_node;
        }

        cur_li = cur_li->next;
    }

    return alias_match;
}

StackNode_t *stackscope_find_z(StackScope_t *cur_scope, char *label)
{
    ListNode_t *cur_li;
    StackNode_t *cur_node;

    assert(cur_scope != NULL);
    assert(label != NULL);

    cur_li = cur_scope->z;
    while(cur_li != NULL)
    {
        cur_node = (StackNode_t *)cur_li->cur;
        if(pascal_identifier_equals(cur_node->label, label))
        {
            return cur_node;
        }

        cur_li = cur_li->next;
    }

    return NULL;
}

/* Returns pointer to previous stack scope */
StackScope_t *free_stackscope(StackScope_t *stackscope)
{
    StackScope_t *prev_scope;

    prev_scope = NULL;
    if(stackscope != NULL)
    {
        prev_scope = stackscope->prev_scope;

        free_stackscope_list(stackscope->t);
        free_stackscope_list(stackscope->x);
        free_stackscope_list(stackscope->z);
        free(stackscope);
    }
    return prev_scope;
}

void free_all_stackscopes(StackScope_t *stackscope)
{
    StackScope_t *prev_scope;

    while(stackscope != NULL)
    {
        prev_scope = stackscope->prev_scope;

        free_stackscope_list(stackscope->t);
        free_stackscope_list(stackscope->x);
        free_stackscope_list(stackscope->z);
        free(stackscope);

        stackscope = prev_scope;
    }
}

void free_stackscope_list(ListNode_t *li)
{
    ListNode_t *cur;
    cur = li;

    if(li == NULL)
        return;

    while(cur != NULL)
    {
        destroy_stack_node((StackNode_t *)cur->cur);
        cur = cur->next;
    }
    DestroyList(li);
}

/*********** StackNode *************/

/* WARNING: Copy is made of label */
StackNode_t *init_stack_node(int offset, char *label, int size)
{
    assert(label != NULL);

    StackNode_t *new_node;
    new_node = (StackNode_t *)malloc(sizeof(StackNode_t));
    assert(new_node != NULL);

    new_node->offset = offset;
    new_node->label = strdup(label);
    new_node->size = size;
    new_node->is_array = 0;
    new_node->array_lower_bound = 0;
    new_node->element_size = size;
    new_node->is_dynamic = 0;
    new_node->is_static = 0;
    new_node->is_reference = 0;
    new_node->is_alias = 0;
    new_node->static_label = NULL;

    return new_node;
}

void destroy_stack_node(StackNode_t *node)
{
    assert(node != NULL);
    assert(node->label != NULL);

    free(node->label);
    if (node->static_label != NULL)
        free(node->static_label);
    free(node);
}

StackNode_t *add_static_array(char *label, int total_size, int element_size, int lower_bound, const char *static_label)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope = global_stackmng->cur_scope;
    StackNode_t *new_node = init_stack_node(0, label, total_size);
    new_node->is_array = 1;
    new_node->array_lower_bound = lower_bound;
    new_node->element_size = element_size;
    new_node->is_dynamic = 0;
    new_node->is_static = 1;
    if (static_label != NULL)
        new_node->static_label = strdup(static_label);

    ListNode_t *list_node = CreateListNode(new_node, LIST_UNSPECIFIED);
    if (cur_scope->x == NULL)
        cur_scope->x = list_node;
    else
        cur_scope->x = PushListNodeBack(cur_scope->x, list_node);

    return new_node;
}
#include <string.h>
