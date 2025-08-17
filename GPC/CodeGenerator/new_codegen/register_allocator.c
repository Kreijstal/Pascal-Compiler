#include <stdlib.h>
#include "register_allocator.h"

#define NUM_REGS 4
static Register regs[NUM_REGS];
static int next_reg = 0;

void new_init_register_allocator() {
    regs[0].name = "%eax";
    regs[1].name = "%ebx";
    regs[2].name = "%ecx";
    regs[3].name = "%edx";
    for (int i = 0; i < NUM_REGS; i++) {
        regs[i].is_free = 1;
    }
}

Register *new_alloc_reg() {
    // Super simple round-robin allocator for now
    Register *reg = &regs[next_reg];
    next_reg = (next_reg + 1) % NUM_REGS;
    return reg;
}

void new_free_reg(Register *reg) {
    // Do nothing for now
}
