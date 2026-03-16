/*
    Symbol table: a stack of hash tables (legacy) with a parallel parent-pointer
    scope tree being built for migration (see docs/SCOPE_TREE_REFACTORING.md).

    Phase 1: Both systems coexist. EnterScope/LeaveScope maintain the tree AND
    call PushScope/PopScope. All existing code continues to work unchanged.

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

#define SYMTAB_MAX_UNITS 256

/* ========================================================================
 * Scope tree types (Phase 1: coexists with flat stack)
 * ======================================================================== */

typedef enum ScopeKind {
    SCOPE_BUILTIN,      /* Root: compiler builtins */
    SCOPE_UNIT,         /* Unit-level scope (one per unit) */
    SCOPE_PROGRAM,      /* Program-level scope (globals) */
    SCOPE_SUBPROGRAM,   /* Procedure/function body */
    SCOPE_BLOCK,        /* Nested block: try-except, with, anonymous method */
} ScopeKind;

typedef struct ScopeNode {
    HashTable_t *table;          /* Symbols declared in this scope (NOT owned — points to
                                  * the same table as the flat stack or unit_tables entry) */
    struct ScopeNode *parent;    /* Walk this chain for FindIdent */
    ScopeKind kind;
    int unit_index;              /* Which unit this scope belongs to (0 = program) */

    /* For SCOPE_UNIT / SCOPE_PROGRAM: dependency edges from `uses` clause */
    struct ScopeNode **dep_scopes;
    int num_deps;
    int cap_deps;
} ScopeNode;

/* ========================================================================
 * Symbol table: flat stack (legacy) + scope tree (new)
 * ======================================================================== */

typedef struct SymTab
{
    /* --- Legacy flat stack (all existing code uses these) --- */
    ListNode_t *stack_head;
    HashTable_t *builtins;
    int unit_context;       /* Active unit index for unit-aware resolution (0 = program) */
    int push_target_unit;   /* When > 0, Push*OntoScope routes to unit_tables[this] */
    HashTable_t *unit_tables[SYMTAB_MAX_UNITS];

    /* --- Scope tree (Phase 1: maintained in parallel, not yet used for lookup) --- */
    ScopeNode *builtin_scope;                 /* Root of the tree (NULL until initialized) */
    ScopeNode *current_scope;                 /* Active scope node */
    ScopeNode *unit_scopes[SYMTAB_MAX_UNITS]; /* O(1) lookup by unit index */
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

/* Like FindIdent but uses unit-aware resolution.
 * Prefers symbols from caller_unit_index, then program-local, then any. */
int FindIdentInUnit(HashNode_t **hash_return, SymTab_t *symtab, const char *id, int caller_unit_index);

/* Searches for any identifier starting with the given prefix */
/* Returns -1 and sets hash_return to NULL if not found */
/* Returns >= 0 tells what scope level it was found at */
int FindIdentByPrefix(HashNode_t **hash_return, SymTab_t *symtab, const char *prefix);

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

/* Find an identifier in the current (top) scope only, bypassing unit-aware resolution.
 * Use after pushing an entry to get the just-added node reliably. */
HashNode_t *FindIdentInCurrentScope(SymTab_t *symtab, const char *id);

/* Move a hash node to the back of its bucket list in the nearest scope */
void SymTab_MoveHashNodeToBack(SymTab_t *symtab, HashNode_t *node);

/* ========================================================================
 * Scope tree API (Phase 1: infrastructure alongside legacy)
 * ======================================================================== */

/* Create a scope node.  The table pointer is NOT owned by the scope node —
 * it points into the flat stack or unit_tables.  DestroyScope frees the
 * ScopeNode itself and the dep_scopes array, but NOT the table. */
ScopeNode *CreateScope(ScopeKind kind, ScopeNode *parent, int unit_index, HashTable_t *table);
void DestroyScope(ScopeNode *scope);

/* Get (or lazily create) the unit scope for the given unit registry index.
 * The scope's table pointer is set to symtab->unit_tables[unit_index]
 * (lazily allocated if NULL). */
ScopeNode *GetOrCreateUnitScope(SymTab_t *symtab, int unit_index);

/* Add a dependency edge: scope can see dep_scope's symbols. */
void ScopeAddDependency(ScopeNode *scope, ScopeNode *dep_scope);

/* Push a new child scope under current_scope and make it current.
 * Also calls PushScope() to keep the flat stack in sync. */
void EnterScope(SymTab_t *symtab, ScopeKind kind, int unit_index);

/* Pop current_scope to its parent.
 * Also calls PopScope() to keep the flat stack in sync. */
void LeaveScope(SymTab_t *symtab);

#endif
