#include "register_allocator.h"
#include <stdlib.h>

static Register registers[] = {
    {"%eax", "%rax", 1},
    {"%ebx", "%rbx", 1},
    {"%ecx", "%rcx", 1},
    {"%edx", "%rdx", 1},
    {"%esi", "%rsi", 1},
    {"%edi", "%rdi", 1},
    // Add more registers as needed
};
static int num_registers = sizeof(registers) / sizeof(Register);

void new_init_register_allocator() {
    for (int i = 0; i < num_registers; i++) {
        registers[i].is_free = 1;
    }
}

Register *new_alloc_reg() {
    for (int i = 0; i < num_registers; i++) {
        if (registers[i].is_free) {
            registers[i].is_free = 0;
            return &registers[i];
        }
    }
    return NULL; // No free registers
}

void new_free_reg(Register *reg) {
    if (reg) {
        reg->is_free = 1;
    }
}

ListNode_t *new_get_allocated_regs() {
    ListNode_t *allocated_regs = NULL;
    for (int i = 0; i < num_registers; i++) {
        if (!registers[i].is_free) {
            if (allocated_regs == NULL) {
                allocated_regs = CreateListNode(&registers[i], LIST_UNSPECIFIED);
            } else {
                PushListNodeBack(allocated_regs, CreateListNode(&registers[i], LIST_UNSPECIFIED));
            }
        }
    }
    return allocated_regs;
}
