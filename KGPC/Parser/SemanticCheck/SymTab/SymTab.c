/*
    Damon Gwinn
    Creates a symbol table which is simply a stack of hash tables for identifiers
    Used to perform semantic checking on a ParseTree

    WARNING: Symbol table will NOT free given identifier strings or args when destroyed
        Remember to free given identifier strings manually
*/

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdio.h>
#include <assert.h>
#include "SymTab.h"
#include "../HashTable/HashTable.h"
#include "../../List/List.h"
#include "../../ParseTree/KgpcType.h"
#include "../../ParseTree/type_tags.h"
#include "../../../unit_registry.h"

/* Forward declarations */
ScopeNode *GetOrCreateUnitScope(SymTab_t *symtab, int unit_index);  /* defined later */

/* Forward declarations for tree-walking lookups (defined at end of file) */
static int FindIdent_Tree(HashNode_t **hash_return, SymTab_t *symtab, const char *id);
static int FindIdentByPrefix_Tree(HashNode_t **hash_return, SymTab_t *symtab, const char *prefix);
static ListNode_t *FindAllIdents_Tree(SymTab_t *symtab, const char *id);
static ListNode_t *FindAllIdentsInNearestScope_Tree(SymTab_t *symtab, const char *id);
static HashNode_t *FindIdentInCurrentScope_Tree(SymTab_t *symtab, const char *id);

/* ========================================================================
 * Internal helper: get the target hash table for Push operations.
 * All insertions target the active scope tree node directly.
 * ======================================================================== */
HashTable_t *SymTab_GetTargetTable(SymTab_t *symtab)
{
    assert(symtab->current_scope != NULL);
    return symtab->current_scope->table;
}

/* Helper: append list b to end of list a */
static ListNode_t *append_list(ListNode_t *a, ListNode_t *b)
{
    if (a == NULL) return b;
    if (b == NULL) return a;
    ListNode_t *tail = a;
    while (tail->next != NULL)
        tail = tail->next;
    tail->next = b;
    return a;
}

/* ========================================================================
 * Init / Destroy
 * ======================================================================== */

SymTab_t *InitSymTab()
{
    SymTab_t *new_symtab;

    new_symtab = (SymTab_t *)malloc(sizeof(SymTab_t));
    assert(new_symtab != NULL);
    /* Initialize scope tree.
     * builtin_scope owns its table (the builtins hash table). */
    new_symtab->builtin_scope = CreateScope(NULL, 0, InitHashTable());
    new_symtab->current_scope = new_symtab->builtin_scope;
    new_symtab->current_unit_index = 0;
    new_symtab->skip_unit_filter = 0;
    memset(new_symtab->unit_scopes, 0, sizeof(new_symtab->unit_scopes));

    return new_symtab;
}

void DestroySymTab(SymTab_t *symtab)
{
    assert(symtab != NULL);

    /* Destroy scope tree nodes.
     * All scopes own their tables; DestroyScope frees them.
     * Walk from current_scope up to builtin_scope, destroying each node.
     * Clear unit_scopes[] entries as we go so we don't double-free. */
    {
        ScopeNode *scope = symtab->current_scope;
        while (scope != NULL && scope != symtab->builtin_scope)
        {
            ScopeNode *parent = scope->parent;
            if (scope->unit_index > 0 && scope->unit_index < SYMTAB_MAX_UNITS &&
                symtab->unit_scopes[scope->unit_index] == scope)
            {
                symtab->unit_scopes[scope->unit_index] = NULL;
            }
            DestroyScope(scope);
            scope = parent;
        }
        /* Destroy any remaining unit scopes not in the parent chain */
        for (int i = 0; i < SYMTAB_MAX_UNITS; i++)
        {
            if (symtab->unit_scopes[i] != NULL)
            {
                DestroyScope(symtab->unit_scopes[i]);
                symtab->unit_scopes[i] = NULL;
            }
        }
        /* Destroy builtin scope (owns its table — freed by DestroyScope) */
        if (symtab->builtin_scope != NULL)
            DestroyScope(symtab->builtin_scope);
    }

    free(symtab);
}

/* ========================================================================
 * Push functions — route via SymTab_GetTargetTable
 * ======================================================================== */

int PushConstOntoScope(SymTab_t *symtab, char *id, long long value)
{
    assert(symtab != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash = SymTab_GetTargetTable(symtab);

    int type_tag = INT_TYPE;
    if (value > INT_MAX || value < INT_MIN)
        type_tag = INT64_TYPE;

    KgpcType *kgpc_type = create_primitive_type(type_tag);
    if (kgpc_type == NULL)
        return 1;

    int result = AddIdentToTable(cur_hash, id, NULL, HASHTYPE_CONST, kgpc_type);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(cur_hash, id);
        if (node != NULL)
        {
            node->is_constant = 1;
            node->const_int_value = value;
        }
    }
    destroy_kgpc_type(kgpc_type);
    return result;
}

int PushConstOntoScope_Typed(SymTab_t *symtab, char *id, long long value, KgpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    assert(type != NULL && "KgpcType must be provided for typed constant");
    HashTable_t *cur_hash = SymTab_GetTargetTable(symtab);
    int result = AddIdentToTable(cur_hash, id, NULL, HASHTYPE_CONST, type);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(cur_hash, id);
        if (node != NULL)
        {
            node->is_constant = 1;
            node->const_int_value = value;
        }
    }
    return result;
}

int PushRealConstOntoScope(SymTab_t *symtab, char *id, double value)
{
    assert(symtab != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash = SymTab_GetTargetTable(symtab);

    KgpcType *kgpc_type = create_primitive_type(REAL_TYPE);
    if (kgpc_type == NULL)
        return 1;

    int result = AddIdentToTable(cur_hash, id, NULL, HASHTYPE_CONST, kgpc_type);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(cur_hash, id);
        if (node != NULL)
        {
            node->is_constant = 1;
            node->const_real_value = value;
        }
    }
    destroy_kgpc_type(kgpc_type);
    return result;
}

int PushStringConstOntoScope(SymTab_t *symtab, char *id, const char *value)
{
    assert(symtab != NULL);
    assert(id != NULL);
    assert(value != NULL);
    HashTable_t *cur_hash = SymTab_GetTargetTable(symtab);

    KgpcType *kgpc_type = create_primitive_type(STRING_TYPE);
    if (kgpc_type == NULL)
        return 1;

    int result = AddIdentToTable(cur_hash, id, NULL, HASHTYPE_CONST, kgpc_type);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(cur_hash, id);
        if (node != NULL)
        {
            node->is_constant = 1;
            node->const_string_value = strdup(value);
            if (node->const_string_value == NULL)
            {
                destroy_kgpc_type(kgpc_type);
                return 1;
            }
        }
    }
    destroy_kgpc_type(kgpc_type);
    return result;
}

int PushSetConstOntoScope(SymTab_t *symtab, char *id, const unsigned char *data,
    int size_bytes, KgpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    assert(data != NULL);
    assert(size_bytes > 0);

    HashTable_t *cur_hash = SymTab_GetTargetTable(symtab);
    int result = AddIdentToTable(cur_hash, id, NULL, HASHTYPE_CONST, type);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(cur_hash, id);
        if (node != NULL)
        {
            node->is_constant = 1;
            node->const_set_size = size_bytes;
            node->const_set_value = (unsigned char *)malloc((size_t)size_bytes);
            if (node->const_set_value == NULL)
            {
                return 1;
            }
            memcpy(node->const_set_value, data, (size_t)size_bytes);
            if (size_bytes <= (int)sizeof(long long))
            {
                long long mask = 0;
                for (int i = 0; i < size_bytes; ++i)
                {
                    mask |= ((long long)node->const_set_value[i]) << (i * 8);
                }
                node->const_int_value = mask;
            }
        }
    }
    return result;
}

int PushTypeOntoScope(SymTab_t *symtab, char *id, enum VarType var_type,
    struct RecordType *record_type, struct TypeAlias *type_alias)
{
    assert(symtab != NULL);
    assert(id != NULL);

    KgpcType *kgpc_type = NULL;

    if (record_type != NULL)
        kgpc_type = create_record_type(record_type);
    else if (type_alias != NULL)
        kgpc_type = create_kgpc_type_from_type_alias(type_alias, symtab, 0);
    else if (var_type != HASHVAR_UNTYPED)
        kgpc_type = kgpc_type_from_var_type(var_type);

    int result = PushTypeOntoScope_Typed(symtab, id, kgpc_type);
    if (kgpc_type != NULL)
        destroy_kgpc_type(kgpc_type);
    return result;
}

int PushVarOntoScope_Typed(SymTab_t *symtab, char *id, KgpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    return AddIdentToTable(SymTab_GetTargetTable(symtab), id, NULL, HASHTYPE_VAR, type);
}

int PushArrayOntoScope_Typed(SymTab_t *symtab, char *id, KgpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    return AddIdentToTable(SymTab_GetTargetTable(symtab), id, NULL, HASHTYPE_ARRAY, type);
}

int PushProcedureOntoScope_Typed(SymTab_t *symtab, char *id, char *mangled_id, KgpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    return AddIdentToTable(SymTab_GetTargetTable(symtab), id, mangled_id, HASHTYPE_PROCEDURE, type);
}

int PushFunctionOntoScope_Typed(SymTab_t *symtab, char *id, char *mangled_id, KgpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    return AddIdentToTable(SymTab_GetTargetTable(symtab), id, mangled_id, HASHTYPE_FUNCTION, type);
}

int PushFuncRetOntoScope_Typed(SymTab_t *symtab, char *id, KgpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    return AddIdentToTable(SymTab_GetTargetTable(symtab), id, NULL, HASHTYPE_FUNCTION_RETURN, type);
}

int PushTypeOntoScope_Typed(SymTab_t *symtab, char *id, KgpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    return AddIdentToTable(SymTab_GetTargetTable(symtab), id, NULL, HASHTYPE_TYPE, type);
}

/* ========================================================================
 * Builtin declarations — go to unit_scopes[System]->table.
 * Compiler-intrinsic types and procedures are injected here just like
 * FPC's psystem.pas does addtype('LongInt', s32inttype).
 * If system.p also declares the same name, its declaration updates
 * the existing entry (same table, collision allowed).
 * ======================================================================== */

static HashTable_t *get_or_create_system_unit_table(SymTab_t *symtab)
{
    int sys_idx = unit_registry_add("System");
    if (sys_idx <= 0 || sys_idx >= SYMTAB_MAX_UNITS)
        return symtab->builtin_scope->table; /* last resort if System not registered */
    return GetOrCreateUnitScope(symtab, sys_idx)->table;
}

int AddBuiltinType_Typed(SymTab_t *symtab, char *id, KgpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    assert(type != NULL);
    return AddIdentToTable(get_or_create_system_unit_table(symtab), id, NULL, HASHTYPE_TYPE, type);
}

int AddBuiltinProc_Typed(SymTab_t *symtab, char *id, KgpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    assert(type != NULL);
    assert(type->kind == TYPE_KIND_PROCEDURE && "Builtin proc must have procedure type");
    assert(type->info.proc_info.return_type == NULL && "Procedure must not have return type");
    type->info.proc_info.owns_params = 1;
    return AddIdentToTable(get_or_create_system_unit_table(symtab), id, NULL, HASHTYPE_BUILTIN_PROCEDURE, type);
}

int AddBuiltinFunction_Typed(SymTab_t *symtab, char *id, KgpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    assert(type != NULL);
    assert(type->kind == TYPE_KIND_PROCEDURE && "Builtin function must have procedure type");
    assert(type->info.proc_info.return_type != NULL && "Function must have return type");
    type->info.proc_info.owns_params = 1;
    return AddIdentToTable(get_or_create_system_unit_table(symtab), id, NULL, HASHTYPE_FUNCTION, type);
}

int AddBuiltinRealConst(SymTab_t *symtab, const char *id, double value)
{
    assert(symtab != NULL);
    assert(id != NULL);
    KgpcType *type = create_primitive_type(REAL_TYPE);
    if (type == NULL) return 1;
    HashTable_t *sys_tbl = get_or_create_system_unit_table(symtab);
    int result = AddIdentToTable(sys_tbl, (char *)id, NULL, HASHTYPE_CONST, type);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(sys_tbl, id);
        if (node != NULL) { node->is_constant = 1; node->const_real_value = value; }
    }
    destroy_kgpc_type(type);
    return result;
}

int AddBuiltinStringConst(SymTab_t *symtab, const char *id, const char *value)
{
    assert(symtab != NULL);
    assert(id != NULL);
    assert(value != NULL);
    KgpcType *type = create_primitive_type(STRING_TYPE);
    if (type == NULL) return 1;
    HashTable_t *sys_tbl = get_or_create_system_unit_table(symtab);
    int result = AddIdentToTable(sys_tbl, (char *)id, NULL, HASHTYPE_CONST, type);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(sys_tbl, id);
        if (node != NULL) { node->is_constant = 1; node->const_string_value = strdup(value); }
    }
    destroy_kgpc_type(type);
    return result;
}

int AddBuiltinIntConst(SymTab_t *symtab, const char *id, long long value)
{
    assert(symtab != NULL);
    assert(id != NULL);
    int type_tag = INT_TYPE;
    if (value > INT_MAX || value < INT_MIN) type_tag = LONGINT_TYPE;
    KgpcType *type = create_primitive_type(type_tag);
    if (type == NULL) return 1;
    HashTable_t *sys_tbl = get_or_create_system_unit_table(symtab);
    int result = AddIdentToTable(sys_tbl, (char *)id, NULL, HASHTYPE_CONST, type);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(sys_tbl, id);
        if (node != NULL) { node->is_constant = 1; node->const_int_value = value; }
    }
    destroy_kgpc_type(type);
    return result;
}

int AddBuiltinCharConst(SymTab_t *symtab, const char *id, unsigned char value)
{
    assert(symtab != NULL);
    assert(id != NULL);
    KgpcType *type = create_primitive_type(CHAR_TYPE);
    if (type == NULL) return 1;
    HashTable_t *sys_tbl = get_or_create_system_unit_table(symtab);
    int result = AddIdentToTable(sys_tbl, (char *)id, NULL, HASHTYPE_CONST, type);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(sys_tbl, id);
        if (node != NULL) { node->is_constant = 1; node->const_int_value = (long long)value; }
    }
    destroy_kgpc_type(type);
    return result;
}

/* ========================================================================
 * Find functions
 * ======================================================================== */

int FindSymbol(HashNode_t **hash_return, SymTab_t *symtab, const char *id)
{
    assert(symtab != NULL);
    assert(id != NULL);

    /* Tree-walking path is unconditional.
     * The tree walk handles unit-aware lookup via parent chain + dep_scopes. */
    if (symtab->current_scope != NULL)
        return FindIdent_Tree(hash_return, symtab, id);

    /* Fallback for edge case where scope tree is not initialized */
    *hash_return = NULL;
    return 0;
}

HashNode_t *FindIdentInCurrentScope(SymTab_t *symtab, const char *id)
{
    assert(symtab != NULL);
    assert(id != NULL);

    if (symtab->current_scope != NULL)
        return FindIdentInCurrentScope_Tree(symtab, id);

    return NULL;
}

int FindIdentByPrefix(HashNode_t **hash_return, SymTab_t *symtab, const char *prefix)
{
    assert(symtab != NULL);
    assert(prefix != NULL);

    if (symtab->current_scope != NULL)
        return FindIdentByPrefix_Tree(hash_return, symtab, prefix);

    *hash_return = NULL;
    return 0;
}

ListNode_t *FindAllIdents(SymTab_t *symtab, const char *id)
{
    assert(symtab != NULL);
    assert(id != NULL);

    if (symtab->current_scope != NULL)
        return FindAllIdents_Tree(symtab, id);

    return NULL;
}

ListNode_t *FindAllIdentsInNearestScope(SymTab_t *symtab, const char *id)
{
    assert(symtab != NULL);
    assert(id != NULL);

    if (symtab->current_scope != NULL)
        return FindAllIdentsInNearestScope_Tree(symtab, id);

    return NULL;
}

/* ========================================================================
 * Print / Debug
 * ======================================================================== */

void PrintSymTab(SymTab_t *symtab, FILE *f, int num_indent)
{
    assert(symtab != NULL);
    assert(f != NULL);

    int i;

    for(i = 0; i < num_indent; ++i)
        fprintf(f, "  ");
    fprintf(f, "[BUILT-INS]:\n");
    PrintHashTable(symtab->builtin_scope->table, f, num_indent+1);

    for (i = 1; i < SYMTAB_MAX_UNITS; i++)
    {
        if (symtab->unit_scopes[i] != NULL && symtab->unit_scopes[i]->table != NULL)
        {
            int j;
            for(j = 0; j < num_indent; ++j)
                fprintf(f, "  ");
            const char *uname = unit_registry_get(i);
            fprintf(f, "[UNIT:%d %s]:\n", i, uname ? uname : "?");
            PrintHashTable(symtab->unit_scopes[i]->table, f, num_indent+1);
            fprintf(f, "\n");
        }
    }
}

void SymTab_MoveHashNodeToBack(SymTab_t *symtab, HashNode_t *node)
{
    if (symtab == NULL || node == NULL)
        return;

    /* Move in per-unit scope tables */
    for (int i = 1; i < SYMTAB_MAX_UNITS; i++)
    {
        if (symtab->unit_scopes[i] != NULL && symtab->unit_scopes[i]->table != NULL)
            HashTable_MoveNodeToBack(symtab->unit_scopes[i]->table, node);
    }

    /* Move in the current scope chain (non-unit scopes) */
    ScopeNode *scope = symtab->current_scope;
    while (scope != NULL)
    {
        if (scope->unit_index == 0 && scope != symtab->builtin_scope)
            HashTable_MoveNodeToBack(scope->table, node);
        scope = scope->parent;
    }
}

/* ========================================================================
 * Scope tree infrastructure
 * ======================================================================== */

ScopeNode *CreateScope(ScopeNode *parent, int unit_index, HashTable_t *table)
{
    assert(table != NULL);
    ScopeNode *scope = (ScopeNode *)calloc(1, sizeof(ScopeNode));
    assert(scope != NULL);
    scope->table = table;
    scope->parent = parent;
    scope->unit_index = unit_index;
    scope->dep_scopes = NULL;
    scope->num_deps = 0;
    scope->cap_deps = 0;
    return scope;
}

void DestroyScope(ScopeNode *scope)
{
    if (scope == NULL) return;
    if (scope->table != NULL)
        DestroyHashTable(scope->table);
    free(scope->dep_scopes);
    free(scope);
}

ScopeNode *GetOrCreateUnitScope(SymTab_t *symtab, int unit_index)
{
    assert(symtab != NULL);
    assert(unit_index > 0 && unit_index < SYMTAB_MAX_UNITS);

    if (symtab->unit_scopes[unit_index] != NULL)
        return symtab->unit_scopes[unit_index];

    /* Allocate a new table owned by the scope */
    ScopeNode *scope = CreateScope(symtab->builtin_scope, unit_index, InitHashTable());
    symtab->unit_scopes[unit_index] = scope;
    return scope;
}

void ScopeAddDependency(ScopeNode *scope, ScopeNode *dep_scope)
{
    assert(scope != NULL);
    assert(dep_scope != NULL);

    /* Don't add duplicates */
    for (int i = 0; i < scope->num_deps; i++)
    {
        if (scope->dep_scopes[i] == dep_scope)
            return;
    }

    if (scope->num_deps >= scope->cap_deps)
    {
        int new_cap = (scope->cap_deps == 0) ? 8 : scope->cap_deps * 2;
        ScopeNode **new_arr = (ScopeNode **)realloc(scope->dep_scopes,
            (size_t)new_cap * sizeof(ScopeNode *));
        assert(new_arr != NULL);
        scope->dep_scopes = new_arr;
        scope->cap_deps = new_cap;
    }

    scope->dep_scopes[scope->num_deps++] = dep_scope;
}

void EnterScope(SymTab_t *symtab, int unit_index)
{
    assert(symtab != NULL);
    assert(symtab->current_scope != NULL);

    ScopeNode *new_scope = CreateScope(symtab->current_scope, unit_index, InitHashTable());
    symtab->current_scope = new_scope;
}

void LeaveScope(SymTab_t *symtab)
{
    assert(symtab != NULL);
    assert(symtab->current_scope != NULL);
    assert(symtab->current_scope->parent != NULL);  /* never leave the root */

    ScopeNode *old_scope = symtab->current_scope;
    symtab->current_scope = old_scope->parent;
    DestroyScope(old_scope);
}

/* ========================================================================
 * Tree-walking lookup (unconditional, deps wired)
 *
 * These walk current_scope -> parent -> ... -> builtin_scope, checking
 * dep_scopes at unit/program boundaries.
 * ======================================================================== */

/* Tree-walking FindIdent: walk current_scope -> parent -> ... -> NULL.
 * At unit/program scope, also check dep_scopes[].
 *
 * Return value: 0 = not found, 1 = found. */
static int FindIdent_Tree(HashNode_t **hash_return, SymTab_t *symtab, const char *id)
{
    ScopeNode *scope = symtab->current_scope;
    int trace_flush = (id != NULL && strcasecmp(id, "Flush") == 0 && getenv("KGPC_DEBUG_FLUSH") != NULL);

    while (scope != NULL)
    {
        HashNode_t *node = FindIdentInTable(scope->table, id);

        if (trace_flush)
            fprintf(stderr, "[FLUSH] scope=%p unit_idx=%d table=%p node=%p num_deps=%d parent=%p\n",
                (void*)scope, scope->unit_index, (void*)scope->table, (void*)node, scope->num_deps, (void*)scope->parent);

        if (node != NULL)
        {
            *hash_return = node;
            return 1;
        }

        /* At unit/program scope, also search dependency unit scopes.
         * Search in reverse order: last `uses` clause entry wins (Pascal semantics). */
        if (scope->num_deps > 0)
        {
            for (int i = scope->num_deps - 1; i >= 0; i--)
            {
                node = FindIdentInTable(scope->dep_scopes[i]->table, id);
                if (trace_flush)
                    fprintf(stderr, "[FLUSH]   dep[%d] unit_idx=%d table=%p node=%p\n",
                        i, scope->dep_scopes[i]->unit_index, (void*)scope->dep_scopes[i]->table, (void*)node);
                if (node != NULL)
                {
                    /* Skip implementation-only symbols from foreign units —
                     * they should not be visible outside their defining unit.
                     * Bypass this filter during codegen (skip_unit_filter=1)
                     * since codegen needs all types for size computation. */
                    if (!symtab->skip_unit_filter &&
                        node->defined_in_unit && !node->unit_is_public &&
                        node->source_unit_index != symtab->current_unit_index)
                        continue;
                    *hash_return = node;
                    return 1;
                }
            }
        }

        scope = scope->parent;
    }

    *hash_return = NULL;
    return 0;
}

/* Tree-walking FindIdentByPrefix.  Returns 0 = not found, 1 = found. */
static int FindIdentByPrefix_Tree(HashNode_t **hash_return, SymTab_t *symtab, const char *prefix)
{
    ScopeNode *scope = symtab->current_scope;

    while (scope != NULL)
    {
        HashNode_t *node = FindIdentByPrefixInTable(scope->table, prefix);
        if (node != NULL)
        {
            *hash_return = node;
            return 1;
        }

        if (scope->num_deps > 0)
        {
            for (int i = scope->num_deps - 1; i >= 0; i--)
            {
                node = FindIdentByPrefixInTable(scope->dep_scopes[i]->table, prefix);
                if (node != NULL)
                {
                    *hash_return = node;
                    return 1;
                }
            }
        }

        scope = scope->parent;
    }

    *hash_return = NULL;
    return 0;
}

/* Tree-walking FindAllIdents: collect all matches from all reachable scopes */
static ListNode_t *FindAllIdents_Tree(SymTab_t *symtab, const char *id)
{
    ListNode_t *all = NULL;
    ScopeNode *scope = symtab->current_scope;

    while (scope != NULL)
    {
        all = append_list(all, FindAllIdentsInTable(scope->table, id));

        if (scope->num_deps > 0)
        {
            for (int i = 0; i < scope->num_deps; i++)
                all = append_list(all, FindAllIdentsInTable(scope->dep_scopes[i]->table, id));
        }

        scope = scope->parent;
    }

    /* Transitive dep fallback: if nothing was found in the normal scope walk,
     * search ALL unit scopes.  Types from transitively-used units (A uses B,
     * B uses C — types from C visible in A) are needed for typecast resolution
     * and field type lookups.  This fallback only fires when the normal walk
     * found nothing, so it doesn't affect overload resolution. */
    if (all == NULL)
    {
        for (int u = 1; u < SYMTAB_MAX_UNITS; u++)
        {
            if (symtab->unit_scopes[u] == NULL)
                continue;
            all = append_list(all, FindAllIdentsInTable(symtab->unit_scopes[u]->table, id));
        }
    }

    return all;
}

/* Tree-walking FindAllIdentsInNearestScope: find closest scope with a match,
 * then collect all matches from that scope level (including dep_scopes if
 * at unit/program boundary). */
static ListNode_t *FindAllIdentsInNearestScope_Tree(SymTab_t *symtab, const char *id)
{
    ScopeNode *scope = symtab->current_scope;

    while (scope != NULL)
    {
        ListNode_t *matches = FindAllIdentsInTable(scope->table, id);
        if (matches != NULL)
        {
            /* Also collect from dep_scopes at this level for completeness */
            if (scope->num_deps > 0)
            {
                for (int i = 0; i < scope->num_deps; i++)
                    matches = append_list(matches,
                        FindAllIdentsInTable(scope->dep_scopes[i]->table, id));
            }
            return matches;
        }

        /* Check dep_scopes as part of this scope level */
        if (scope->num_deps > 0)
        {
            ListNode_t *dep_matches = NULL;
            for (int i = 0; i < scope->num_deps; i++)
                dep_matches = append_list(dep_matches,
                    FindAllIdentsInTable(scope->dep_scopes[i]->table, id));
            if (dep_matches != NULL)
                return dep_matches;
        }

        scope = scope->parent;
    }

    return NULL;
}

/* Tree-walking FindIdentInCurrentScope */
static HashNode_t *FindIdentInCurrentScope_Tree(SymTab_t *symtab, const char *id)
{
    if (symtab->current_scope == NULL)
        return NULL;

    return FindIdentInTable(symtab->current_scope->table, id);
}
