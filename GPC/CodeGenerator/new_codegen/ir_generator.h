#ifndef IR_GENERATOR_H
#define IR_GENERATOR_H

#include "../../ir.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"

ListNode_t *generate_ir(Tree_t *ast, SymTab_t *table);

#endif // IR_GENERATOR_H
