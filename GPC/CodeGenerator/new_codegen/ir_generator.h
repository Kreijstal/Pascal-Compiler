#ifndef IR_GENERATOR_H
#define IR_GENERATOR_H

#include "../../ir.h"
#include "../../Parser/ParseTree/tree.h"

ListNode_t *generate_ir(Tree_t *ast);

#endif // IR_GENERATOR_H
