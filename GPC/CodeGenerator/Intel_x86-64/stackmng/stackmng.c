/*
    Damon Gwinn
    Stack Manager structure (see codegen.h for details)
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "stackmng.h"
#include "../register_types.h"
#include "../codegen.h"
#include "../../../Parser/List/List.h"
#include "../../../identifier_utils.h"

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

    actual_offset = current_stack_home_space() +
                        scope->t_offset + scope->x_offset + scope->z_offset;

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

void pop_stackscope()
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);

    global_stackmng->cur_scope = free_stackscope(global_stackmng->cur_scope);
}

static inline int align_up(int value, int alignment)
{
    return (value + alignment - 1) & ~(alignment - 1);
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

    cur_scope->t_offset = align_up(cur_scope->t_offset, temp_size);
    cur_scope->t_offset += temp_size;

    offset = current_stack_home_space() +
        cur_scope->z_offset + cur_scope->x_offset + cur_scope->t_offset;

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

StackNode_t *add_temp_bytes(char *label, int size)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    if (size <= 0)
        size = (int)sizeof(void *);

    StackScope_t *cur_scope = global_stackmng->cur_scope;
    int alignment = (int)sizeof(void *);
    if (alignment < DOUBLEWORD)
        alignment = DOUBLEWORD;

    cur_scope->t_offset = align_up(cur_scope->t_offset, alignment);
    cur_scope->t_offset = align_up(cur_scope->t_offset, size);
    cur_scope->t_offset += size;

    int offset = current_stack_home_space() +
        cur_scope->z_offset + cur_scope->x_offset + cur_scope->t_offset;

    StackNode_t *new_node = init_stack_node(offset, label, size);

    if (cur_scope->t == NULL)
        cur_scope->t = CreateListNode(new_node, LIST_UNSPECIFIED);
    else
        cur_scope->t = PushListNodeBack(cur_scope->t,
            CreateListNode(new_node, LIST_UNSPECIFIED));

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

    int aligned_size = size;
    if (aligned_size % DOUBLEWORD != 0)
        aligned_size = ((aligned_size + DOUBLEWORD - 1) / DOUBLEWORD) * DOUBLEWORD;

    cur_scope->x_offset += aligned_size;

    offset = current_stack_home_space() +
        cur_scope->z_offset + cur_scope->x_offset;

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

StackNode_t *add_array(char *label, int total_size, int element_size, int lower_bound)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope = global_stackmng->cur_scope;

    cur_scope->x_offset += total_size;

    int offset = current_stack_home_space() +
        cur_scope->z_offset + cur_scope->x_offset;

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

StackNode_t *add_dynamic_array(char *label, int element_size, int lower_bound)
{
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope = global_stackmng->cur_scope;

    int descriptor_size = 4 * DOUBLEWORD;
    if (descriptor_size < 2 * element_size)
        descriptor_size = 2 * element_size;

    cur_scope->x_offset += descriptor_size;

    int offset = current_stack_home_space() +
        cur_scope->z_offset + cur_scope->x_offset;

    StackNode_t *new_node = init_stack_node(offset, label, descriptor_size);
    new_node->is_array = 1;
    new_node->array_lower_bound = lower_bound;
    new_node->element_size = element_size;
    new_node->is_dynamic = 1;

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

    offset = current_stack_home_space() +
        cur_scope->z_offset;

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

    offset = current_stack_home_space() +
        cur_scope->z_offset;

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
    assert(global_stackmng != NULL);
    assert(global_stackmng->cur_scope != NULL);
    assert(label != NULL);

    StackScope_t *cur_scope;
    StackNode_t *cur_node;

    cur_scope = global_stackmng->cur_scope;

    while (cur_scope != NULL)
    {
        cur_node = stackscope_find_z(cur_scope, label);
        if(cur_node != NULL)
        {
            return cur_node;
        }

        cur_node = stackscope_find_x(cur_scope, label);
        if(cur_node != NULL)
        {
            return cur_node;
        }

        cur_node = stackscope_find_t(cur_scope, label);
        if(cur_node != NULL)
        {
            return cur_node;
        }

        cur_scope = cur_scope->prev_scope;
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

RegStack_t *init_reg_stack()
{
    /* See codegen.h for information on available general purpose registers */
    ListNode_t *registers;

    RegStack_t *reg_stack;
    reg_stack = (RegStack_t *)malloc(sizeof(RegStack_t));
    assert(reg_stack != NULL);

    /* Caller-saved general purpose registers available for expression evaluation */
    Register_t *rax = (Register_t *)malloc(sizeof(Register_t));
    assert(rax != NULL);
    rax->bit_64 = strdup("%rax");
    rax->bit_32 = strdup("%eax");

    Register_t *rcx = (Register_t *)malloc(sizeof(Register_t));
    assert(rcx != NULL);
    rcx->bit_64 = strdup("%rcx");
    rcx->bit_32 = strdup("%ecx");

    Register_t *rdx = (Register_t *)malloc(sizeof(Register_t));
    assert(rdx != NULL);
    rdx->bit_64 = strdup("%rdx");
    rdx->bit_32 = strdup("%edx");

    Register_t *rsi = (Register_t *)malloc(sizeof(Register_t));
    assert(rsi != NULL);
    rsi->bit_64 = strdup("%rsi");
    rsi->bit_32 = strdup("%esi");

    Register_t *rdi = (Register_t *)malloc(sizeof(Register_t));
    assert(rdi != NULL);
    rdi->bit_64 = strdup("%rdi");
    rdi->bit_32 = strdup("%edi");

    Register_t *r8 = (Register_t *)malloc(sizeof(Register_t));
    assert(r8 != NULL);
    r8->bit_64 = strdup("%r8");
    r8->bit_32 = strdup("%r8d");

    Register_t *r9 = (Register_t *)malloc(sizeof(Register_t));
    assert(r9 != NULL);
    r9->bit_64 = strdup("%r9");
    r9->bit_32 = strdup("%r9d");

    Register_t *r10 = (Register_t *)malloc(sizeof(Register_t));
    assert(r10 != NULL);
    r10->bit_64 = strdup("%r10");
    r10->bit_32 = strdup("%r10d");

    Register_t *r11 = (Register_t *)malloc(sizeof(Register_t));
    assert(r11 != NULL);
    r11->bit_64 = strdup("%r11");
    r11->bit_32 = strdup("%r11d");

    registers = CreateListNode(rax, LIST_UNSPECIFIED);
    registers = PushListNodeBack(registers, CreateListNode(rcx, LIST_UNSPECIFIED));
    registers = PushListNodeBack(registers, CreateListNode(rdx, LIST_UNSPECIFIED));
    registers = PushListNodeBack(registers, CreateListNode(rsi, LIST_UNSPECIFIED));
    registers = PushListNodeBack(registers, CreateListNode(rdi, LIST_UNSPECIFIED));
    registers = PushListNodeBack(registers, CreateListNode(r8, LIST_UNSPECIFIED));
    registers = PushListNodeBack(registers, CreateListNode(r9, LIST_UNSPECIFIED));
    registers = PushListNodeBack(registers, CreateListNode(r10, LIST_UNSPECIFIED));
    registers = PushListNodeBack(registers, CreateListNode(r11, LIST_UNSPECIFIED));

    /*
    registers = CreateListNode(rdi, LIST_UNSPECIFIED);
    registers = PushListNodeBack(registers, CreateListNode(rsi, LIST_UNSPECIFIED));
    registers = PushListNodeBack(registers, CreateListNode(rbx, LIST_UNSPECIFIED));
    */

    reg_stack->registers_allocated = NULL;
    reg_stack->registers_free = registers;
    reg_stack->num_registers = NUM_CALLER_SAVED_REGISTERS;

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

            cur->next = reg_stack->registers_free;
            reg_stack->registers_free = cur;
            return;
        }
        prev = cur;
        cur = cur->next;
    }

    return;
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

/* TODO: Spilling */
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

        return reg;
    }
    else
    {
        return NULL;
    }
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

    assert(cur_scope != NULL);
    assert(label != NULL);

    cur_li = cur_scope->x;
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

    return new_node;
}

void destroy_stack_node(StackNode_t *node)
{
    assert(node != NULL);
    assert(node->label != NULL);

    free(node->label);
    free(node);
}
