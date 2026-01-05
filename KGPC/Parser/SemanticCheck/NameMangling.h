#ifndef NAME_MANGLING_H
#define NAME_MANGLING_H

#include "../List/List.h"
#include "SymTab/SymTab.h"
#include "HashTable/HashTable.h"

// Generates a mangled name for a function from its declaration.
// The caller is responsible for freeing the returned string.
char* MangleFunctionName(const char* original_name, ListNode_t* args, SymTab_t* symtab);

// Generates a mangled name for a function from a call site.
// The caller is responsible for freeing the returned string.
char* MangleFunctionNameFromCallSite(const char* original_name, ListNode_t* args_expr, SymTab_t *symtab, int max_scope_lev);

#endif
