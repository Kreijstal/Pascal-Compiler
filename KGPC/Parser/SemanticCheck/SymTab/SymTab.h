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

/* Pushes a constant with explicit KgpcType onto the current scope (head) */
int PushConstOntoScope_Typed(SymTab_t *symtab, char *id, long long value, KgpcType *type);

/* Pushes a real constant onto the current scope (head) */
int PushRealConstOntoScope(SymTab_t *symtab, char *id, double value);

/* Pushes a string constant onto the current scope (head) */
int PushStringConstOntoScope(SymTab_t *symtab, char *id, const char *value);

/* Pushes a set constant (supports 4-byte small sets and 32-byte char sets) */
int PushSetConstOntoScope(SymTab_t *symtab, char *id, const unsigned char *data,
    int size_bytes, KgpcType *type);

/* Pushes a new type onto the current scope (head) */
int PushTypeOntoScope(SymTab_t *symtab, char *id, enum VarType var_type,
    struct RecordType *record_type, struct TypeAlias *type_alias);

/* Type system functions using KgpcType */

/* Pushes a new variable with a KgpcType onto the current scope */
int PushVarOntoScope_Typed(SymTab_t *symtab, char *id, KgpcType *type);

/* Pushes a new array with a KgpcType onto the current scope */
int PushArrayOntoScope_Typed(SymTab_t *symtab, char *id, KgpcType *type);

/* Pushes a new procedure with a KgpcType onto the current scope */
int PushProcedureOntoScope_Typed(SymTab_t *symtab, char *id, char *mangled_id, KgpcType *type);

/* Pushes a new function with a KgpcType onto the current scope */
int PushFunctionOntoScope_Typed(SymTab_t *symtab, char *id, char *mangled_id, KgpcType *type);

/* Pushes a new function return value with a KgpcType onto the current scope */
int PushFuncRetOntoScope_Typed(SymTab_t *symtab, char *id, KgpcType *type);

/* Pushes a new type declaration with a KgpcType onto the current scope */
int PushTypeOntoScope_Typed(SymTab_t *symtab, char *id, KgpcType *type);

/* Builtin declarations using KgpcType */

/* Adds a built-in type with a KgpcType */
int AddBuiltinType_Typed(SymTab_t *symtab, char *id, KgpcType *type);

/* Adds a built-in procedure with a KgpcType */
int AddBuiltinProc_Typed(SymTab_t *symtab, char *id, KgpcType *type);

/* Adds a built-in function with a KgpcType */
int AddBuiltinFunction_Typed(SymTab_t *symtab, char *id, KgpcType *type);

/* Adds a built-in real constant */
int AddBuiltinRealConst(SymTab_t *symtab, const char *id, double value);

/* Adds a built-in string constant */
int AddBuiltinStringConst(SymTab_t *symtab, const char *id, const char *value);

/* Adds a built-in integer constant */
int AddBuiltinIntConst(SymTab_t *symtab, const char *id, long long value);

/* Adds a built-in character constant */
int AddBuiltinCharConst(SymTab_t *symtab, const char *id, unsigned char value);

/* Searches for an identifier and sets the hash_return that contains the id and type information */
/* Returns -1 and sets hash_return to NULL if not found */
/* Returns >= 0 tells what scope level it was found at */
int FindIdent(HashNode_t ** hash_return, SymTab_t *symtab, const char *id);

/* Searches for all instances of an identifier and returns a list of HashNode_t* */
/* Returns NULL if not found */
ListNode_t *FindAllIdents(SymTab_t *symtab, const char *id);

/* Searches for all instances of an identifier in the nearest scope that defines it */
/* Returns NULL if not found */
ListNode_t *FindAllIdentsInNearestScope(SymTab_t *symtab, const char *id);

/* Pops the current scope */
void PopScope(SymTab_t *symtab);

/* Destroys the SymTab and all associated hash tables */
/* WARNING: Does not free given identifier strings */
void DestroySymTab(SymTab_t *symtab);

/* Prints the table for debugging */
void PrintSymTab(SymTab_t *symtab, FILE *f, int num_indent);

#endif
