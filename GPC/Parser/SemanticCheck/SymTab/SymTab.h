/*
    Damon Gwinn
    Creates a symbol table which is simply a stack of hash tables for identifiers
    Used to perform semantic checking on a ParseTree

    WARNING: Symbol table will NOT free given identifier strings or args when destroyed
        Remember to free given identifier strings manually
*/

#ifndef SYM_TAB_H
#define SYM_TAB_H

#include <stdio.h>
#include "../HashTable/HashTable.h"
#include "../../List/List.h"

/*enum VarType{HASHVAR_INTEGER, HASHVAR_REAL, HASHVAR_PROCEDURE, HASHVAR_UNTYPED};
  Defined in HashTable.h */

/* A stack of hash tables with built-ins */
typedef struct SymTab
{
    ListNode_t *stack_head;
    HashTable_t *builtins;
} SymTab_t;

/* Initializes the SymTab with stack_head pointing to NULL */
SymTab_t *InitSymTab();

/* Pushes a new scope onto the stack (FIFO) */
void PushScope(SymTab_t *symtab);

int PushConstOntoScope(SymTab_t *symtab, char *id, long long value);

/* Pushes a constant with explicit GpcType onto the current scope (head) */
int PushConstOntoScope_Typed(SymTab_t *symtab, char *id, long long value, GpcType *type);

/* Pushes a new type onto the current scope (head) */
int PushTypeOntoScope(SymTab_t *symtab, char *id, enum VarType var_type,
    struct RecordType *record_type, struct TypeAlias *type_alias);

/* Type system functions using GpcType */

/* Pushes a new variable with a GpcType onto the current scope */
int PushVarOntoScope_Typed(SymTab_t *symtab, char *id, GpcType *type);

/* Pushes a new array with a GpcType onto the current scope */
int PushArrayOntoScope_Typed(SymTab_t *symtab, char *id, GpcType *type);

/* Pushes a new procedure with a GpcType onto the current scope */
int PushProcedureOntoScope_Typed(SymTab_t *symtab, char *id, char *mangled_id, GpcType *type);

/* Pushes a new function with a GpcType onto the current scope */
int PushFunctionOntoScope_Typed(SymTab_t *symtab, char *id, char *mangled_id, GpcType *type);

/* Pushes a new function return value with a GpcType onto the current scope */
int PushFuncRetOntoScope_Typed(SymTab_t *symtab, char *id, GpcType *type);

/* Pushes a new type declaration with a GpcType onto the current scope */
int PushTypeOntoScope_Typed(SymTab_t *symtab, char *id, GpcType *type);

/* Builtin declarations using GpcType */

/* Adds a built-in type with a GpcType */
int AddBuiltinType_Typed(SymTab_t *symtab, char *id, GpcType *type);

/* Adds a built-in procedure with a GpcType */
int AddBuiltinProc_Typed(SymTab_t *symtab, char *id, GpcType *type);

/* Adds a built-in function with a GpcType */
int AddBuiltinFunction_Typed(SymTab_t *symtab, char *id, GpcType *type);

/* Searches for an identifier and sets the hash_return that contains the id and type information */
/* Returns -1 and sets hash_return to NULL if not found */
/* Returns >= 0 tells what scope level it was found at */
int FindIdent(HashNode_t ** hash_return, SymTab_t *symtab, char *id);

/* Searches for all instances of an identifier and returns a list of HashNode_t* */
/* Returns NULL if not found */
ListNode_t *FindAllIdents(SymTab_t *symtab, char *id);

/* Pops the current scope */
void PopScope(SymTab_t *symtab);

/* Destroys the SymTab and all associated hash tables */
/* WARNING: Does not free given identifier strings */
void DestroySymTab(SymTab_t *symtab);

/* Prints the table for debugging */
void PrintSymTab(SymTab_t *symtab, FILE *f, int num_indent);

#endif
