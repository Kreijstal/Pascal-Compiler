#ifndef FLAT_IR_GENERATOR_H
#define FLAT_IR_GENERATOR_H

#include "../../ir.h"
#include "../../Parser/flat_ast.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"

ListNode_t *generate_ir_from_flat_ast(FlatNode *ast, SymTab_t *table);

#endif // FLAT_IR_GENERATOR_H
