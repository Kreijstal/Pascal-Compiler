#ifndef MANGLE_H
#define MANGLE_H

#include "../../Parser/flat_ast.h"
#include "../../Parser/List/List.h"

char* get_mangled_name(char* base_name, ListNode_t* params);

#endif // MANGLE_H
