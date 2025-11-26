#include "pass_manager.h"

#include "optimizer.h"
#include "../flags.h"

#include <stddef.h>

typedef struct
{
    const char *name;
    optimizer_pass_fn run;
    int minimum_level;
} optimizer_pass_descriptor;

static const optimizer_pass_descriptor default_passes[] = {
    { "constant-folding", optimizer_pass_constant_folding, 1 },
    { "dead-code-elimination", optimizer_pass_dead_code_elimination, 2 },
};

void optimizer_pass_manager_run(SymTab_t *symtab, Tree_t *tree)
{
    if (symtab == NULL || tree == NULL)
        return;

    int level = optimize_flag();
    for (size_t i = 0; i < sizeof(default_passes) / sizeof(default_passes[0]); ++i)
    {
        if (level >= default_passes[i].minimum_level && default_passes[i].run != NULL)
            default_passes[i].run(symtab, tree);
    }
}
