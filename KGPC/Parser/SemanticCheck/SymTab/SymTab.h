/*
    Symbol table: parent-pointer scope tree (primary).
    See docs/SCOPE_TREE_REFACTORING.md.

    Phase 5: All lookups and insertions use the scope tree directly.

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

#define SYMTAB_MAX_UNITS 1024

/* ========================================================================
 * Scope tree types
 * ======================================================================== */

typedef struct ScopeNode {
    HashTable_t *table;          /* Symbols declared in this scope (owned by this node, freed in DestroyScope) */
    struct ScopeNode *parent;    /* Walk this chain for FindIdent */
    int unit_index;              /* Which unit this scope belongs to (0 = program) */
    int codegen_unit_scope_registered; /* Codegen-only lazy population of persistent unit scopes */

    /* Dependency edges from `uses` clause (only on unit/program scopes) */
    struct ScopeNode **dep_scopes;
    int *dep_is_iface;               /* 1 = interface dep, 0 = implementation-only */
    int num_deps;
    int cap_deps;
} ScopeNode;

/* ========================================================================
 * Symbol table: scope tree (primary)
 * ======================================================================== */

typedef struct SymTab
{
    /* --- Scope tree (used for all lookups and insertions) --- */
    ScopeNode *builtin_scope;                 /* Root of the tree; owns its table (builtins) */
    ScopeNode *current_scope;                 /* Active scope node */
    ScopeNode *unit_scopes[SYMTAB_MAX_UNITS]; /* O(1) lookup by unit index; each scope OWNS its table */
    int current_unit_index;                   /* Unit being compiled (0 = main program); set by SemCheck */
    int skip_unit_filter;                     /* When 1, FindSymbol skips the unit_is_public filter (used by codegen) */
} SymTab_t;

/* Initializes the SymTab */
SymTab_t *InitSymTab();

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

/* Returns the hash table that Push* operations currently target */
HashTable_t *SymTab_GetTargetTable(SymTab_t *symtab);

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

/* Searches for a symbol and sets the hash_return that contains the id and type information */
/* Returns 0 (false) if not found, 1 (true) if found */
int FindSymbol(HashNode_t ** hash_return, SymTab_t *symtab, const char *id);

/* Searches for any identifier starting with the given prefix */
/* Returns 0 and sets hash_return to NULL if not found */
/* Returns 1 if found (hash_return set to the matching node) */
int FindIdentByPrefix(HashNode_t **hash_return, SymTab_t *symtab, const char *prefix);

/* Searches for all instances of an identifier and returns a list of HashNode_t* */
/* Returns NULL if not found */
ListNode_t *FindAllIdents(SymTab_t *symtab, const char *id);

/* Searches for all instances of an identifier in the nearest scope that defines it */
/* Returns NULL if not found */
ListNode_t *FindAllIdentsInNearestScope(SymTab_t *symtab, const char *id);

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
 * Scope tree API
 * ======================================================================== */

/* Create a scope node. DestroyScope always frees the table. */
ScopeNode *CreateScope(ScopeNode *parent, int unit_index, HashTable_t *table);
void DestroyScope(ScopeNode *scope);

/* Get (or lazily create) the unit scope for the given unit registry index.
 * Allocates a new HashTable for the scope if it does not exist yet.
 * The scope OWNS its table (freed in DestroyScope). */
ScopeNode *GetOrCreateUnitScope(SymTab_t *symtab, int unit_index);

/* Add a dependency edge: scope can see dep_scope's symbols.
 * is_interface: 1 = interface (public) dependency, 0 = implementation-only. */
void ScopeAddDependency(ScopeNode *scope, ScopeNode *dep_scope, int is_interface);

/* Add a dependency edge with transitive propagation for interface deps.
 * If is_interface is true, also pulls in dep_scope's own interface deps
 * (one level deep, which is sufficient since units are wired in topological order). */
void ScopeAddDependencyTransitive(ScopeNode *scope, ScopeNode *dep_scope, int is_interface);

/* Push a new child scope under current_scope and make it current. */
void EnterScope(SymTab_t *symtab, int unit_index);

/* Pop current_scope to its parent (never leaves the root). */
void LeaveScope(SymTab_t *symtab);

#endif
