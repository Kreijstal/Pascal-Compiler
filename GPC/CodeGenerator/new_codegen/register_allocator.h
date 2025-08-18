#ifndef REGISTER_ALLOCATOR_H
#define REGISTER_ALLOCATOR_H

struct Register {
    char *name;
    char *name64;
    int is_free;
};

typedef struct Register Register;

#include "../../Parser/List/List.h"

void new_init_register_allocator();
Register *new_alloc_reg();
void new_free_reg(Register *reg);
ListNode_t *new_get_allocated_regs();

#endif // REGISTER_ALLOCATOR_H
