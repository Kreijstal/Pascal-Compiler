/*
    Symbol table: a parent-pointer scope tree.
    Each ScopeNode owns a HashTable_t of symbols.  FindIdent walks
    current_scope → parent → … → builtin_scope, checking dependency
    edges at SCOPE_UNIT / SCOPE_PROGRAM boundaries.

    WARNING: Symbol table will NOT free given identifier strings or args when destroyed.
        Remember to free given identifier strings manually.
*/

#ifndef SYM_TAB_H
#define SYM_TAB_H

#include <stdio.h>
#include "../HashTable/HashTable.h"
#include "../../List/List.h"

#define SYMTAB_MAX_UNITS 256

/* ======================================================================== */
/* Scope tree types                                                         */
/* ======================================================================== */

typedef enum ScopeKind {
    SCOPE_BUILTIN,      /* Root: compiler builtins */
    SCOPE_UNIT,         /* Unit-level scope (one per unit) */
    SCOPE_PROGRAM,      /* Program-level scope (globals) */
    SCOPE_SUBPROGRAM,   /* Procedure/function body */
    SCOPE_BLOCK,        /* Nested block: try-except, with, anonymous method */
} ScopeKind;

typedef struct ScopeNode {
    HashTable_t *table;          /* Symbols declared in this scope */
    struct ScopeNode *parent;    /* Walk this chain for FindIdent */
    ScopeKind kind;
    int unit_index;              /* Which unit this scope belongs to (0 = program) */

    /* For SCOPE_UNIT / SCOPE_PROGRAM: dependency edges from `uses` clause */
    struct ScopeNode **dep_scopes;
    int num_deps;
    int cap_deps;
} ScopeNode;

/* ======================================================================== */
/* Symbol table: a scope tree                                               */
/* ======================================================================== */

typedef struct SymTab
{
    ScopeNode *builtin_scope;                 /* Root of the tree */
    ScopeNode *current_scope;                 /* Active scope — all lookups start here */
    ScopeNode *unit_scopes[SYMTAB_MAX_UNITS]; /* O(1) lookup by unit index */
} SymTab_t;

/* ======================================================================== */
/* Scope lifecycle                                                          */
/* ======================================================================== */

ScopeNode *CreateScope(ScopeKind kind, ScopeNode *parent, int unit_index);
void DestroyScope(ScopeNode *scope);

/* Get (or lazily create) the unit scope for the given unit registry index. */
ScopeNode *GetOrCreateUnitScope(SymTab_t *symtab, int unit_index);

/* Add a dependency edge: scope can see dep_scope's symbols. */
void ScopeAddDependency(ScopeNode *scope, ScopeNode *dep_scope);

/* Push a new child scope under current_scope and make it current. */
void EnterScope(SymTab_t *symtab, ScopeKind kind, int unit_index);

/* Destroy current_scope and move to its parent. */
void LeaveScope(SymTab_t *symtab);

/* ======================================================================== */
/* Symbol table lifecycle                                                   */
/* ======================================================================== */

SymTab_t *InitSymTab(void);
void DestroySymTab(SymTab_t *symtab);

/* ======================================================================== */
/* Push functions — add symbols to current_scope->table                     */
/* ======================================================================== */

int PushConstOntoScope(SymTab_t *symtab, char *id, long long value);
int PushConstOntoScope_Typed(SymTab_t *symtab, char *id, long long value, KgpcType *type);
int PushRealConstOntoScope(SymTab_t *symtab, char *id, double value);
int PushStringConstOntoScope(SymTab_t *symtab, char *id, const char *value);
int PushSetConstOntoScope(SymTab_t *symtab, char *id, const unsigned char *data,
    int size_bytes, KgpcType *type);
int PushTypeOntoScope(SymTab_t *symtab, char *id, enum VarType var_type,
    struct RecordType *record_type, struct TypeAlias *type_alias);
int PushVarOntoScope_Typed(SymTab_t *symtab, char *id, KgpcType *type);
int PushArrayOntoScope_Typed(SymTab_t *symtab, char *id, KgpcType *type);
int PushProcedureOntoScope_Typed(SymTab_t *symtab, char *id, char *mangled_id, KgpcType *type);
int PushFunctionOntoScope_Typed(SymTab_t *symtab, char *id, char *mangled_id, KgpcType *type);
int PushFuncRetOntoScope_Typed(SymTab_t *symtab, char *id, KgpcType *type);
int PushTypeOntoScope_Typed(SymTab_t *symtab, char *id, KgpcType *type);

/* ======================================================================== */
/* Builtin declarations — go to system unit scope                           */
/* ======================================================================== */

int AddBuiltinType_Typed(SymTab_t *symtab, char *id, KgpcType *type);
int AddBuiltinProc_Typed(SymTab_t *symtab, char *id, KgpcType *type);
int AddBuiltinFunction_Typed(SymTab_t *symtab, char *id, KgpcType *type);
int AddBuiltinRealConst(SymTab_t *symtab, const char *id, double value);
int AddBuiltinStringConst(SymTab_t *symtab, const char *id, const char *value);
int AddBuiltinIntConst(SymTab_t *symtab, const char *id, long long value);
int AddBuiltinCharConst(SymTab_t *symtab, const char *id, unsigned char value);

/* ======================================================================== */
/* Find functions                                                           */
/* ======================================================================== */

/* Walk current_scope → parent → … → builtin_scope.
 * Returns scope depth (0 = current or global, 1+ = enclosing local scopes).
 * Returns -1 and sets *hash_return to NULL if not found. */
int FindIdent(HashNode_t **hash_return, SymTab_t *symtab, const char *id);

int FindIdentByPrefix(HashNode_t **hash_return, SymTab_t *symtab, const char *prefix);
ListNode_t *FindAllIdents(SymTab_t *symtab, const char *id);
ListNode_t *FindAllIdentsInNearestScope(SymTab_t *symtab, const char *id);

/* Find an identifier in current_scope->table only. */
HashNode_t *FindIdentInCurrentScope(SymTab_t *symtab, const char *id);

/* ======================================================================== */
/* Debug / utilities                                                        */
/* ======================================================================== */

void PrintSymTab(SymTab_t *symtab, FILE *f, int num_indent);
void SymTab_MoveHashNodeToBack(SymTab_t *symtab, HashNode_t *node);

#endif
