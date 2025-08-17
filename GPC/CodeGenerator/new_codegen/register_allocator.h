#ifndef REGISTER_ALLOCATOR_H
#define REGISTER_ALLOCATOR_H

typedef struct {
    char *name;
    int is_free;
} Register;

void new_init_register_allocator();
Register *new_alloc_reg();
void new_free_reg(Register *reg);

#endif // REGISTER_ALLOCATOR_H
