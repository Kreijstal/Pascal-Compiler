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

/* ========================================================================
 * Internal helper: get the target hash table for Push operations.
 * When push_target_unit > 0, routes to the per-unit table (lazily created).
 * Otherwise, routes to the current scope stack head.
 * ======================================================================== */
static HashTable_t *SymTab_GetTargetTable(SymTab_t *symtab)
{
    int idx = symtab->push_target_unit;
    if (idx > 0 && idx < SYMTAB_MAX_UNITS)
    {
        if (symtab->unit_tables[idx] == NULL)
            symtab->unit_tables[idx] = InitHashTable();
        return symtab->unit_tables[idx];
    }
    assert(symtab->stack_head != NULL);
    return (HashTable_t *)symtab->stack_head->cur;
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
    new_symtab->stack_head = NULL;
    new_symtab->builtins = InitHashTable();
    new_symtab->unit_context = 0;
    new_symtab->push_target_unit = 0;
    memset(new_symtab->unit_tables, 0, sizeof(new_symtab->unit_tables));

    /* Phase 1: Initialize scope tree alongside flat stack.
     * builtin_scope->table points to the same builtins hash table. */
    new_symtab->builtin_scope = CreateScope(SCOPE_BUILTIN, NULL, 0, new_symtab->builtins);
    new_symtab->current_scope = new_symtab->builtin_scope;
    memset(new_symtab->unit_scopes, 0, sizeof(new_symtab->unit_scopes));

    return new_symtab;
}

void PushScope(SymTab_t *symtab)
{
    assert(symtab != NULL);

    HashTable_t *new_hash;
    new_hash = InitHashTable();

    if(symtab->stack_head == NULL)
        symtab->stack_head = CreateListNode(new_hash, LIST_UNSPECIFIED);
    else
        symtab->stack_head = PushListNodeFront(symtab->stack_head,
            CreateListNode(new_hash, LIST_UNSPECIFIED));
}

void PopScope(SymTab_t *symtab)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);

    ListNode_t *cur;
    cur = symtab->stack_head;
    symtab->stack_head = symtab->stack_head->next;

    DestroyHashTable((HashTable_t *)cur->cur);
    free(cur);
}

void DestroySymTab(SymTab_t *symtab)
{
    assert(symtab != NULL);

    /* Phase 1: Destroy scope tree nodes (they don't own their tables).
     * Walk from current_scope up to builtin_scope, destroying each node.
     * Clear unit_scopes[] entries as we go so we don't double-free. */
    {
        ScopeNode *scope = symtab->current_scope;
        while (scope != NULL && scope != symtab->builtin_scope)
        {
            ScopeNode *parent = scope->parent;
            /* If this is a unit scope, clear its slot to prevent double-free below */
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
        /* Destroy builtin scope */
        if (symtab->builtin_scope != NULL)
            DestroyScope(symtab->builtin_scope);
    }

    /* Destroy per-unit tables (they own their nodes) */
    for (int i = 0; i < SYMTAB_MAX_UNITS; i++)
    {
        if (symtab->unit_tables[i] != NULL)
            DestroyHashTable(symtab->unit_tables[i]);
    }

    ListNode_t *cur, *next;
    cur = symtab->stack_head;
    while(cur != NULL)
    {
        next = cur->next;
        DestroyHashTable((HashTable_t *)cur->cur);
        free(cur);
        cur = next;
    }

    DestroyHashTable(symtab->builtins);
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
    /* Also register in builtins for later codegen lookup */
    if (result == 0 && symtab->builtins != NULL)
    {
        int builtins_result = AddIdentToTable(symtab->builtins, id, NULL, HASHTYPE_CONST, type);
        if (builtins_result == 0)
        {
            HashNode_t *builtin_node = FindIdentInTable(symtab->builtins, id);
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
 * Builtin declarations — go to unit_tables[System].
 * Compiler-intrinsic types and procedures are injected here just like
 * FPC's psystem.pas does addtype('LongInt', s32inttype).
 * If system.p also declares the same name, its declaration updates
 * the existing entry (same table, collision allowed).
 * ======================================================================== */

static HashTable_t *get_or_create_system_unit_table(SymTab_t *symtab)
{
    int sys_idx = unit_registry_add("System");
    if (sys_idx <= 0 || sys_idx >= SYMTAB_MAX_UNITS)
        return symtab->builtins; /* last resort if System not registered */
    if (symtab->unit_tables[sys_idx] == NULL)
        symtab->unit_tables[sys_idx] = InitHashTable();
    return symtab->unit_tables[sys_idx];
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

/* Search all unit tables (for non-unit-aware path) */
static HashNode_t *FindIdentInAnyUnitTable(SymTab_t *symtab, const char *id)
{
    for (int i = 1; i < SYMTAB_MAX_UNITS; i++)
    {
        if (symtab->unit_tables[i] != NULL)
        {
            HashNode_t *node = FindIdentInTable(symtab->unit_tables[i], id);
            if (node != NULL)
                return node;
        }
    }
    return NULL;
}

/* Collect all matches from all unit tables */
static ListNode_t *FindAllIdentsInAnyUnitTable(SymTab_t *symtab, const char *id)
{
    ListNode_t *all = NULL;
    for (int i = 1; i < SYMTAB_MAX_UNITS; i++)
    {
        if (symtab->unit_tables[i] != NULL)
            all = append_list(all, FindAllIdentsInTable(symtab->unit_tables[i], id));
    }
    return all;
}

/* Count scope stack depth (= outermost scope level) */
static int scope_stack_depth(SymTab_t *symtab)
{
    int depth = 0;
    for (ListNode_t *cur = symtab->stack_head; cur != NULL; cur = cur->next)
        depth++;
    return depth;
}

int FindIdent(HashNode_t **hash_return, SymTab_t *symtab, const char *id)
{
    if (symtab->unit_context > 0)
        return FindIdentInUnit(hash_return, symtab, id, symtab->unit_context);

    int return_val = 0;
    assert(symtab != NULL);
    assert(id != NULL);

    HashNode_t *hash_node;

    /* 1. Scope stack (program-local + inner scopes) */
    ListNode_t *cur = symtab->stack_head;
    while (cur != NULL)
    {
        hash_node = FindIdentInTable((HashTable_t *)cur->cur, id);
        if (hash_node != NULL)
        {
            *hash_return = hash_node;
            return return_val;
        }
        ++return_val;
        cur = cur->next;
    }

    /* 2. Unit tables + builtins (global scope — same level as outermost) */
    {
        int global_level = 0;  /* unit/builtin symbols are always accessible */

        hash_node = FindIdentInAnyUnitTable(symtab, id);
        if (hash_node != NULL)
        {
            *hash_return = hash_node;
            return global_level;
        }

        hash_node = FindIdentInTable(symtab->builtins, id);
        if (hash_node != NULL)
        {
            *hash_return = hash_node;
            return global_level;
        }
    }

    *hash_return = NULL;
    return -1;
}

int FindIdentInUnit(HashNode_t **hash_return, SymTab_t *symtab, const char *id, int caller_unit_index)
{
    int return_val = 0;
    assert(symtab != NULL);
    assert(id != NULL);

    HashNode_t *hash_node;

    /* 1. Scope stack (inner scopes — local variables, parameters) */
    ListNode_t *cur = symtab->stack_head;
    while (cur != NULL)
    {
        hash_node = FindIdentInTable((HashTable_t *)cur->cur, id);
        if (hash_node != NULL)
        {
            *hash_return = hash_node;
            return return_val;
        }
        ++return_val;
        cur = cur->next;
    }

    /* 2. Caller's own unit table + deps + builtins (all global scope) */
    {
        int global_level = 0;  /* unit/builtin symbols are always accessible */

        if (caller_unit_index > 0 && caller_unit_index < SYMTAB_MAX_UNITS &&
            symtab->unit_tables[caller_unit_index] != NULL)
        {
            hash_node = FindIdentInTable(symtab->unit_tables[caller_unit_index], id);
            if (hash_node != NULL)
            {
                *hash_return = hash_node;
                return global_level;
            }
        }

        /* 3. Dependency unit tables */
        int num_units = unit_registry_count();
        for (int dep = 1; dep <= num_units; dep++)
        {
            if (dep == caller_unit_index)
                continue;
            if (!unit_registry_is_dep(caller_unit_index, dep))
                continue;
            if (symtab->unit_tables[dep] == NULL)
                continue;
            hash_node = FindIdentInTable(symtab->unit_tables[dep], id);
            if (hash_node != NULL)
            {
                *hash_return = hash_node;
                return global_level;
            }
        }

        /* 4. Builtins */
        hash_node = FindIdentInTable(symtab->builtins, id);
        if (hash_node != NULL)
        {
            *hash_return = hash_node;
            return global_level;
        }
    }

    *hash_return = NULL;
    return -1;
}

HashNode_t *FindIdentInCurrentScope(SymTab_t *symtab, const char *id)
{
    assert(symtab != NULL);
    assert(id != NULL);
    /* When push_target_unit is active, the "current scope" is the unit table */
    if (symtab->push_target_unit > 0 && symtab->push_target_unit < SYMTAB_MAX_UNITS &&
        symtab->unit_tables[symtab->push_target_unit] != NULL)
        return FindIdentInTable(symtab->unit_tables[symtab->push_target_unit], id);
    if (symtab->stack_head == NULL)
        return NULL;
    return FindIdentInTable((HashTable_t *)symtab->stack_head->cur, id);
}

int FindIdentByPrefix(HashNode_t **hash_return, SymTab_t *symtab, const char *prefix)
{
    int return_val = 0;
    assert(symtab != NULL);
    assert(prefix != NULL);

    ListNode_t *cur = symtab->stack_head;
    while (cur != NULL)
    {
        HashNode_t *node = FindIdentByPrefixInTable((HashTable_t *)cur->cur, prefix);
        if (node != NULL)
        {
            *hash_return = node;
            return return_val;
        }
        ++return_val;
        cur = cur->next;
    }

    /* Search unit tables */
    for (int i = 1; i < SYMTAB_MAX_UNITS; i++)
    {
        if (symtab->unit_tables[i] != NULL)
        {
            HashNode_t *node = FindIdentByPrefixInTable(symtab->unit_tables[i], prefix);
            if (node != NULL)
            {
                *hash_return = node;
                return return_val;
            }
        }
    }

    HashNode_t *node = FindIdentByPrefixInTable(symtab->builtins, prefix);
    if (node != NULL)
    {
        *hash_return = node;
        return return_val;
    }

    *hash_return = NULL;
    return -1;
}

ListNode_t *FindAllIdents(SymTab_t *symtab, const char *id)
{
    assert(symtab != NULL);
    assert(id != NULL);

    if (symtab->unit_context > 0)
    {
        /* Unit-aware: scope stack + caller's unit + deps + builtins */
        int caller = symtab->unit_context;
        ListNode_t *all = NULL;

        /* 1. Scope stack */
        for (ListNode_t *cur = symtab->stack_head; cur != NULL; cur = cur->next)
            all = append_list(all, FindAllIdentsInTable((HashTable_t *)cur->cur, id));

        /* 2. Caller's own unit table */
        if (caller > 0 && caller < SYMTAB_MAX_UNITS && symtab->unit_tables[caller] != NULL)
            all = append_list(all, FindAllIdentsInTable(symtab->unit_tables[caller], id));

        /* 3. Dependency unit tables */
        int num_units = unit_registry_count();
        for (int dep = 1; dep <= num_units; dep++)
        {
            if (dep == caller) continue;
            if (!unit_registry_is_dep(caller, dep)) continue;
            if (symtab->unit_tables[dep] == NULL) continue;
            all = append_list(all, FindAllIdentsInTable(symtab->unit_tables[dep], id));
        }

        /* 4. Builtins */
        all = append_list(all, FindAllIdentsInTable(symtab->builtins, id));
        return all;
    }

    /* Non-unit-aware: scope stack + all unit tables + builtins */
    ListNode_t *all = NULL;

    for (ListNode_t *cur = symtab->stack_head; cur != NULL; cur = cur->next)
        all = append_list(all, FindAllIdentsInTable((HashTable_t *)cur->cur, id));

    all = append_list(all, FindAllIdentsInAnyUnitTable(symtab, id));
    all = append_list(all, FindAllIdentsInTable(symtab->builtins, id));

    return all;
}

ListNode_t *FindAllIdentsInNearestScope(SymTab_t *symtab, const char *id)
{
    assert(symtab != NULL);
    assert(id != NULL);

    /* Check scope stack first — each scope is a distinct level */
    for (ListNode_t *cur = symtab->stack_head; cur != NULL; cur = cur->next)
    {
        ListNode_t *scope_matches = FindAllIdentsInTable((HashTable_t *)cur->cur, id);
        if (scope_matches != NULL)
            return scope_matches;
    }

    /* Unit tables + builtins are all at the same "global" level.
     * If any match is found, collect from all of them. */
    ListNode_t *all = NULL;
    all = append_list(all, FindAllIdentsInAnyUnitTable(symtab, id));
    all = append_list(all, FindAllIdentsInTable(symtab->builtins, id));
    return all;
}

/* ========================================================================
 * Print / Debug
 * ======================================================================== */

void PrintSymTab(SymTab_t *symtab, FILE *f, int num_indent)
{
    assert(symtab != NULL);
    assert(f != NULL);

    int i, scope;
    ListNode_t *cur;

    for(i = 0; i < num_indent; ++i)
        fprintf(f, "  ");
    fprintf(f, "[BUILT-INS]:\n");
    PrintHashTable(symtab->builtins, f, num_indent+1);

    cur = symtab->stack_head;
    scope = 0;
    while(cur != NULL)
    {
        for(i = 0; i < num_indent; ++i)
            fprintf(f, "  ");

        fprintf(f, "[SCOPE:%d]\n", scope);
        PrintHashTable((HashTable_t *)cur->cur, f, num_indent+1);
        fprintf(f, "\n");

        cur = cur->next;
        ++scope;
    }

    for (i = 1; i < SYMTAB_MAX_UNITS; i++)
    {
        if (symtab->unit_tables[i] != NULL)
        {
            int j;
            for(j = 0; j < num_indent; ++j)
                fprintf(f, "  ");
            const char *uname = unit_registry_get(i);
            fprintf(f, "[UNIT:%d %s]:\n", i, uname ? uname : "?");
            PrintHashTable(symtab->unit_tables[i], f, num_indent+1);
            fprintf(f, "\n");
        }
    }
}

void SymTab_MoveHashNodeToBack(SymTab_t *symtab, HashNode_t *node)
{
    if (symtab == NULL || node == NULL)
        return;

    for (ListNode_t *cur = symtab->stack_head; cur != NULL; cur = cur->next)
    {
        HashTable_t *table = (HashTable_t *)cur->cur;
        if (table != NULL)
            HashTable_MoveNodeToBack(table, node);
    }

    /* Also move in per-unit tables */
    for (int i = 1; i < SYMTAB_MAX_UNITS; i++)
    {
        if (symtab->unit_tables[i] != NULL)
            HashTable_MoveNodeToBack(symtab->unit_tables[i], node);
    }
}

/* ========================================================================
 * Scope tree infrastructure (Phase 1)
 *
 * These functions build/maintain the scope tree in parallel with the
 * flat stack.  During Phase 1, no lookup code uses the tree — it is
 * purely structural.  Phase 2 will shadow FindIdent; Phase 3+ will
 * remove the flat stack entirely.
 * ======================================================================== */

ScopeNode *CreateScope(ScopeKind kind, ScopeNode *parent, int unit_index, HashTable_t *table)
{
    assert(table != NULL);
    ScopeNode *scope = (ScopeNode *)calloc(1, sizeof(ScopeNode));
    assert(scope != NULL);
    scope->table = table;  /* NOT owned — shared with flat stack or unit_tables */
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
    /* table is NOT owned — it belongs to the flat stack or unit_tables */
    free(scope->dep_scopes);
    free(scope);
}

ScopeNode *GetOrCreateUnitScope(SymTab_t *symtab, int unit_index)
{
    assert(symtab != NULL);
    assert(unit_index > 0 && unit_index < SYMTAB_MAX_UNITS);

    if (symtab->unit_scopes[unit_index] != NULL)
        return symtab->unit_scopes[unit_index];

    /* Ensure unit_tables entry exists (flat stack owns the table) */
    if (symtab->unit_tables[unit_index] == NULL)
        symtab->unit_tables[unit_index] = InitHashTable();

    ScopeNode *scope = CreateScope(SCOPE_UNIT, symtab->builtin_scope, unit_index,
                                    symtab->unit_tables[unit_index]);
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

    /* Keep flat stack in sync */
    PushScope(symtab);

    /* Build tree node whose table points to the just-pushed flat-stack hash table */
    ScopeNode *scope = CreateScope(kind, symtab->current_scope, unit_index,
                                   (HashTable_t *)symtab->stack_head->cur);
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

    /* Keep flat stack in sync */
    PopScope(symtab);
}
