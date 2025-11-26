#ifndef KGPC_OPTIMIZER_PASS_MANAGER_H
#define KGPC_OPTIMIZER_PASS_MANAGER_H

#include "../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../Parser/ParseTree/tree.h"

typedef void (*optimizer_pass_fn)(SymTab_t *symtab, Tree_t *tree);

void optimizer_pass_manager_run(SymTab_t *symtab, Tree_t *tree);

#endif
