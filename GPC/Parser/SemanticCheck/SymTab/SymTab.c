/*
    Damon Gwinn
    Creates a symbol table which is simply a stack of hash tables for identifiers
    Used to perform semantic checking on a ParseTree

    WARNING: Symbol table will NOT free given identifier strings or args when destroyed
        Remember to free given identifier strings manually
*/

#include <stdlib.h>
#include <limits.h>
#include <stdio.h>
#include <assert.h>
#include "SymTab.h"
#include "../HashTable/HashTable.h"
#include "../../List/List.h"

/* Initializes the SymTab with stack_head pointing to NULL */
SymTab_t *InitSymTab()
{
    SymTab_t *new_symtab;

    new_symtab = (SymTab_t *)malloc(sizeof(SymTab_t));
    assert(new_symtab != NULL);
    new_symtab->stack_head = NULL;
    new_symtab->builtins = InitHashTable();

    return new_symtab;
}

/* Adds a built-in procedure call */
/* NOTE: Built-ins reflected on all scope levels */
/* Returns 1 if failed, 0 otherwise */
int AddBuiltinProc(SymTab_t *symtab, char *id, ListNode_t *args)
{
    assert(symtab != NULL);
    assert(id != NULL);

    return AddIdentToTable_Legacy(symtab->builtins, id, NULL, HASHVAR_PROCEDURE, HASHTYPE_BUILTIN_PROCEDURE, args, NULL, NULL);
}

int AddBuiltinFunction(SymTab_t *symtab, char *id, enum VarType return_type)
{
    assert(symtab != NULL);
    assert(id != NULL);

    return AddIdentToTable_Legacy(symtab->builtins, id, NULL, return_type, HASHTYPE_FUNCTION, NULL, NULL, NULL);
}

/* Adds a built-in type */
int AddBuiltinType(SymTab_t *symtab, char *id, enum VarType var_type)
{
    assert(symtab != NULL);
    assert(id != NULL);

    return AddIdentToTable_Legacy(symtab->builtins, id, NULL, var_type, HASHTYPE_TYPE, NULL, NULL, NULL);
}

/* Pushes a new scope onto the stack (FIFO) */
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

/* Pushes a new variable onto the current scope (head) */
int PushVarOntoScope(SymTab_t *symtab, enum VarType var_type, char *id)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;
    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    return AddIdentToTable_Legacy(cur_hash, id, NULL, var_type, HASHTYPE_VAR, NULL, NULL, NULL);
}

/* Pushes a new array onto the current scope (head) */
int PushArrayOntoScope(SymTab_t *symtab, enum VarType var_type, char *id, int start, int end, int element_size)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;

    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    int result = AddIdentToTable_Legacy(cur_hash, id, NULL, var_type, HASHTYPE_ARRAY, NULL, NULL, NULL);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(cur_hash, id);
        if (node != NULL)
        {
            node->is_array = 1;
            node->array_start = start;
            node->array_end = end;
            node->element_size = element_size;
            node->is_dynamic_array = (end < start);
        }
    }
    return result;
}

int PushConstOntoScope(SymTab_t *symtab, char *id, long long value)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;

    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    enum VarType stored_type = HASHVAR_INTEGER;
    if (value > INT_MAX || value < INT_MIN)
        stored_type = HASHVAR_LONGINT;
    int result = AddIdentToTable_Legacy(cur_hash, id, NULL, stored_type, HASHTYPE_CONST, NULL, NULL, NULL);
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

/* Pushes a new procedure onto the current scope (head) */
/* NOTE: args can be NULL to represent no args */
int PushProcedureOntoScope(SymTab_t *symtab, char *id, char *mangled_id, ListNode_t *args)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;

    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    return AddIdentToTable_Legacy(cur_hash, id, mangled_id, HASHVAR_PROCEDURE, HASHTYPE_PROCEDURE, args, NULL, NULL);
}

/* Pushes a new function onto the current scope (head) */
/* NOTE: args can be NULL to represent no args */
int PushFunctionOntoScope(SymTab_t *symtab, char *id, char *mangled_id, enum VarType var_type, ListNode_t *args)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;

    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    return AddIdentToTable_Legacy(cur_hash, id, mangled_id, var_type, HASHTYPE_FUNCTION, args, NULL, NULL);
}

/* Pushes a new function return type var onto the current scope (head) */
/* NOTE: args can be NULL to represent no args */
int PushFuncRetOntoScope(SymTab_t *symtab, char *id, enum VarType var_type, ListNode_t *args)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;

    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    return AddIdentToTable_Legacy(cur_hash, id, NULL, var_type, HASHTYPE_FUNCTION_RETURN, args, NULL, NULL);
}

/* Searches for an identifier and sets the hash_return that contains the id and type information */
/* Returns -1 and sets hash_return to NULL if not found */
/* Returns >= 0 tells what scope level it was found at */
/* Mutating tells whether it's being referenced in an assignment context */
int FindIdent(HashNode_t **hash_return, SymTab_t *symtab, char *id)
{
    int return_val = 0;
    assert(symtab != NULL);
    assert(id != NULL);

    ListNode_t *cur;
    HashNode_t *hash_node;

    /* First check user definitions */
    cur = symtab->stack_head;
    while(cur != NULL)
    {
        hash_node = FindIdentInTable((HashTable_t *)cur->cur, id);
        if(hash_node != NULL)
        {
            *hash_return = hash_node;
            return return_val;
        }

        ++return_val;
        cur = cur->next;
    }

    /* Then check built-ins */
    hash_node = FindIdentInTable(symtab->builtins, id);
    if(hash_node != NULL)
    {
        *hash_return = hash_node;
        return return_val;
    }

    *hash_return = NULL;
    return_val = -1;

    return return_val;
}

/* Searches for all instances of an identifier and returns a list of HashNode_t* */
/* Returns NULL if not found */
ListNode_t *FindAllIdents(SymTab_t *symtab, char *id)
{
    ListNode_t *cur_scope;
    ListNode_t *found_nodes = NULL;

    assert(symtab != NULL);
    assert(id != NULL);

    cur_scope = symtab->stack_head;

    /* Check scopes first */
    while(cur_scope != NULL)
    {
        found_nodes = FindAllIdentsInTable((HashTable_t *)cur_scope->cur, id);
        if(found_nodes != NULL)
            return found_nodes;

        cur_scope = cur_scope->next;
    }

    /* Check builtins if not found */
    found_nodes = FindAllIdentsInTable(symtab->builtins, id);
    if(found_nodes != NULL)
        return found_nodes;

    return NULL;
}

/* Pushes a new type onto the current scope (head) */
int PushTypeOntoScope(SymTab_t *symtab, char *id, enum VarType var_type,
    struct RecordType *record_type, struct TypeAlias *type_alias)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;

    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    return AddIdentToTable_Legacy(cur_hash, id, NULL, var_type, HASHTYPE_TYPE, NULL, record_type, type_alias);
}

/* ===== NEW TYPE SYSTEM FUNCTIONS USING GpcType ===== */

/* Pushes a new variable with a GpcType onto the current scope */
int PushVarOntoScope_Typed(SymTab_t *symtab, char *id, GpcType *type)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;
    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    return AddIdentToTable(cur_hash, id, NULL, HASHTYPE_VAR, NULL, type);
}

/* Pushes a new array with a GpcType onto the current scope */
int PushArrayOntoScope_Typed(SymTab_t *symtab, char *id, GpcType *type)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;
    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    return AddIdentToTable(cur_hash, id, NULL, HASHTYPE_ARRAY, NULL, type);
}

/* Pushes a new procedure with a GpcType onto the current scope */
int PushProcedureOntoScope_Typed(SymTab_t *symtab, char *id, char *mangled_id, GpcType *type)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;
    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    
    // Extract args from the procedure type if present
    ListNode_t *args = NULL;
    if (type != NULL && type->kind == TYPE_KIND_PROCEDURE) {
        args = type->info.proc_info.params;
    }
    
    return AddIdentToTable(cur_hash, id, mangled_id, HASHTYPE_PROCEDURE, args, type);
}

/* Pushes a new function with a GpcType onto the current scope */
int PushFunctionOntoScope_Typed(SymTab_t *symtab, char *id, char *mangled_id, GpcType *type)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;
    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    
    // Extract args from the procedure type if present
    ListNode_t *args = NULL;
    if (type != NULL && type->kind == TYPE_KIND_PROCEDURE) {
        args = type->info.proc_info.params;
    }
    
    return AddIdentToTable(cur_hash, id, mangled_id, HASHTYPE_FUNCTION, args, type);
}

/* Pushes a new type declaration with a GpcType onto the current scope */
int PushTypeOntoScope_Typed(SymTab_t *symtab, char *id, GpcType *type)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;
    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    return AddIdentToTable(cur_hash, id, NULL, HASHTYPE_TYPE, NULL, type);
}

/* Pops the current scope */
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

/* Destroys the SymTab and all associated hash tables */
/* WARNING: Does not free given identifier strings */
void DestroySymTab(SymTab_t *symtab)
{
    assert(symtab != NULL);

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

/* Prints the table for debugging */
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
}
