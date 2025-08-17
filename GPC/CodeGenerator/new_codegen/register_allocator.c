#include "register_allocator.h"
#include <stdlib.h>

static Register registers[] = {
    {"%eax", 1},
    {"%ebx", 1},
    {"%ecx", 1},
    {"%edx", 1},
    {"%esi", 1},
    {"%edi", 1},
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
