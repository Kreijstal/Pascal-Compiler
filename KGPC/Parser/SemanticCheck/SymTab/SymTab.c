/*
    Symbol table: a parent-pointer scope tree.
    Replaces the former flat stack + unit_tables[] + builtins architecture.
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

/* ========================================================================
 * Internal helper: get the target hash table for Push operations.
 * Always returns current_scope->table.
 * ======================================================================== */
static HashTable_t *SymTab_GetTargetTable(SymTab_t *symtab)
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
 * Scope lifecycle
 * ======================================================================== */

ScopeNode *CreateScope(ScopeKind kind, ScopeNode *parent, int unit_index)
{
    ScopeNode *scope = (ScopeNode *)malloc(sizeof(ScopeNode));
    assert(scope != NULL);
    scope->table = InitHashTable();
    scope->parent = parent;
    scope->kind = kind;
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

    ScopeNode *scope = CreateScope(SCOPE_UNIT, symtab->builtin_scope, unit_index);
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

void EnterScope(SymTab_t *symtab, ScopeKind kind, int unit_index)
{
    assert(symtab != NULL);
    assert(symtab->current_scope != NULL);

    ScopeNode *scope = CreateScope(kind, symtab->current_scope, unit_index);
    symtab->current_scope = scope;
}

void LeaveScope(SymTab_t *symtab)
{
    assert(symtab != NULL);
    assert(symtab->current_scope != NULL);
    assert(symtab->current_scope->parent != NULL);  /* never leave the root */

    ScopeNode *old = symtab->current_scope;
    symtab->current_scope = old->parent;
    DestroyScope(old);
}

/* ========================================================================
 * Init / Destroy
 * ======================================================================== */

SymTab_t *InitSymTab(void)
{
    SymTab_t *symtab = (SymTab_t *)malloc(sizeof(SymTab_t));
    assert(symtab != NULL);

    symtab->builtin_scope = CreateScope(SCOPE_BUILTIN, NULL, 0);
    symtab->current_scope = symtab->builtin_scope;
    memset(symtab->unit_scopes, 0, sizeof(symtab->unit_scopes));

    return symtab;
}

void DestroySymTab(SymTab_t *symtab)
{
    assert(symtab != NULL);

    /* Walk from current_scope up to (but not including) builtin_scope and destroy.
     * Clear unit_scopes[] entries as we go so we don't double-free. */
    ScopeNode *scope = symtab->current_scope;
    while (scope != NULL && scope != symtab->builtin_scope)
    {
        ScopeNode *parent = scope->parent;
        /* If this is a unit scope, clear its slot */
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

    /* Destroy the builtin scope (root) */
    DestroyScope(symtab->builtin_scope);

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
    /* Also register in builtin scope for later codegen lookup */
    if (result == 0 && symtab->builtin_scope != NULL)
    {
        int builtins_result = AddIdentToTable(symtab->builtin_scope->table, id, NULL, HASHTYPE_CONST, type);
        if (builtins_result == 0)
        {
            HashNode_t *builtin_node = FindIdentInTable(symtab->builtin_scope->table, id);
            if (builtin_node != NULL)
            {
                builtin_node->is_constant = 1;
                builtin_node->const_set_size = size_bytes;
                builtin_node->const_set_value = (unsigned char *)malloc((size_t)size_bytes);
                if (builtin_node->const_set_value != NULL)
                {
                    memcpy(builtin_node->const_set_value, data, (size_t)size_bytes);
                    if (size_bytes <= (int)sizeof(long long))
                    {
                        long long mask = 0;
                        for (int i = 0; i < size_bytes; ++i)
                            mask |= ((long long)builtin_node->const_set_value[i]) << (i * 8);
                        builtin_node->const_int_value = mask;
                    }
                }
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
 * Builtin declarations — go to the System unit scope.
 * ======================================================================== */

static HashTable_t *get_or_create_system_unit_table(SymTab_t *symtab)
{
    int sys_idx = unit_registry_add("System");
    assert(sys_idx > 0 && sys_idx < SYMTAB_MAX_UNITS);
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
 * Find functions — tree walk
 * ======================================================================== */

int FindIdent(HashNode_t **hash_return, SymTab_t *symtab, const char *id)
{
    assert(symtab != NULL);
    assert(id != NULL);

    int depth = 0;
    ScopeNode *scope = symtab->current_scope;

    while (scope != NULL)
    {
        HashNode_t *node = FindIdentInTable(scope->table, id);
        if (node != NULL)
        {
            *hash_return = node;
            /* Unit/program/builtin scopes report depth 0 (global level),
             * matching the old flat-stack behavior where unit_tables and
             * builtins always returned 0. */
            if (scope->kind == SCOPE_UNIT || scope->kind == SCOPE_PROGRAM ||
                scope->kind == SCOPE_BUILTIN)
                return 0;
            return depth;
        }

        /* At unit/program scope, also search dependency unit scopes */
        if (scope->kind == SCOPE_UNIT || scope->kind == SCOPE_PROGRAM)
        {
            for (int i = 0; i < scope->num_deps; i++)
            {
                node = FindIdentInTable(scope->dep_scopes[i]->table, id);
                if (node != NULL)
                {
                    *hash_return = node;
                    return 0;  /* dependency symbols are global level */
                }
            }
        }

        scope = scope->parent;
        depth++;
    }

    *hash_return = NULL;
    return -1;
}

HashNode_t *FindIdentInCurrentScope(SymTab_t *symtab, const char *id)
{
    assert(symtab != NULL);
    assert(id != NULL);
    assert(symtab->current_scope != NULL);
    return FindIdentInTable(symtab->current_scope->table, id);
}

int FindIdentByPrefix(HashNode_t **hash_return, SymTab_t *symtab, const char *prefix)
{
    int depth = 0;
    assert(symtab != NULL);
    assert(prefix != NULL);

    ScopeNode *scope = symtab->current_scope;
    while (scope != NULL)
    {
        HashNode_t *node = FindIdentByPrefixInTable(scope->table, prefix);
        if (node != NULL)
        {
            *hash_return = node;
            if (scope->kind == SCOPE_UNIT || scope->kind == SCOPE_PROGRAM ||
                scope->kind == SCOPE_BUILTIN)
                return 0;
            return depth;
        }

        /* At unit/program scope, search deps */
        if (scope->kind == SCOPE_UNIT || scope->kind == SCOPE_PROGRAM)
        {
            for (int i = 0; i < scope->num_deps; i++)
            {
                node = FindIdentByPrefixInTable(scope->dep_scopes[i]->table, prefix);
                if (node != NULL)
                {
                    *hash_return = node;
                    return 0;
                }
            }
        }

        scope = scope->parent;
        depth++;
    }

    *hash_return = NULL;
    return -1;
}

ListNode_t *FindAllIdents(SymTab_t *symtab, const char *id)
{
    assert(symtab != NULL);
    assert(id != NULL);

    ListNode_t *all = NULL;

    for (ScopeNode *scope = symtab->current_scope; scope != NULL; scope = scope->parent)
    {
        all = append_list(all, FindAllIdentsInTable(scope->table, id));

        /* At unit/program scope, also search deps */
        if (scope->kind == SCOPE_UNIT || scope->kind == SCOPE_PROGRAM)
        {
            for (int i = 0; i < scope->num_deps; i++)
                all = append_list(all, FindAllIdentsInTable(scope->dep_scopes[i]->table, id));
        }
    }

    return all;
}

ListNode_t *FindAllIdentsInNearestScope(SymTab_t *symtab, const char *id)
{
    assert(symtab != NULL);
    assert(id != NULL);

    for (ScopeNode *scope = symtab->current_scope; scope != NULL; scope = scope->parent)
    {
        ListNode_t *matches = FindAllIdentsInTable(scope->table, id);
        if (matches != NULL)
            return matches;

        /* At unit/program scope, also check deps — they're at the same "global" level */
        if (scope->kind == SCOPE_UNIT || scope->kind == SCOPE_PROGRAM)
        {
            ListNode_t *dep_all = NULL;
            for (int i = 0; i < scope->num_deps; i++)
                dep_all = append_list(dep_all,
                    FindAllIdentsInTable(scope->dep_scopes[i]->table, id));
            if (dep_all != NULL)
                return dep_all;
        }
    }

    return NULL;
}

/* ========================================================================
 * Print / Debug
 * ======================================================================== */

void PrintSymTab(SymTab_t *symtab, FILE *f, int num_indent)
{
    assert(symtab != NULL);
    assert(f != NULL);

    int i, scope_depth;

    /* Print scope chain from current up to root */
    ScopeNode *scope = symtab->current_scope;
    scope_depth = 0;
    while (scope != NULL)
    {
        for (i = 0; i < num_indent; ++i)
            fprintf(f, "  ");

        const char *kind_str = "UNKNOWN";
        switch (scope->kind)
        {
            case SCOPE_BUILTIN:    kind_str = "BUILTIN"; break;
            case SCOPE_UNIT:       kind_str = "UNIT"; break;
            case SCOPE_PROGRAM:    kind_str = "PROGRAM"; break;
            case SCOPE_SUBPROGRAM: kind_str = "SUBPROGRAM"; break;
            case SCOPE_BLOCK:      kind_str = "BLOCK"; break;
        }
        fprintf(f, "[SCOPE:%d %s unit=%d]:\n", scope_depth, kind_str, scope->unit_index);
        PrintHashTable(scope->table, f, num_indent + 1);
        fprintf(f, "\n");

        scope = scope->parent;
        ++scope_depth;
    }

    /* Print unit scopes */
    for (i = 1; i < SYMTAB_MAX_UNITS; i++)
    {
        if (symtab->unit_scopes[i] != NULL)
        {
            int j;
            for (j = 0; j < num_indent; ++j)
                fprintf(f, "  ");
            const char *uname = unit_registry_get(i);
            fprintf(f, "[UNIT_SCOPE:%d %s]:\n", i, uname ? uname : "?");
            PrintHashTable(symtab->unit_scopes[i]->table, f, num_indent + 1);
            fprintf(f, "\n");
        }
    }
}

void SymTab_MoveHashNodeToBack(SymTab_t *symtab, HashNode_t *node)
{
    if (symtab == NULL || node == NULL)
        return;

    for (ScopeNode *scope = symtab->current_scope; scope != NULL; scope = scope->parent)
    {
        if (scope->table != NULL)
            HashTable_MoveNodeToBack(scope->table, node);
    }

    /* Also move in unit scopes */
    for (int i = 1; i < SYMTAB_MAX_UNITS; i++)
    {
        if (symtab->unit_scopes[i] != NULL)
            HashTable_MoveNodeToBack(symtab->unit_scopes[i]->table, node);
    }
}
