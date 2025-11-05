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
#include "../../ParseTree/GpcType.h"
#include "../../ParseTree/type_tags.h"

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

int PushConstOntoScope(SymTab_t *symtab, char *id, long long value)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;

    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    
    /* Determine type based on value range and create GpcType */
    int type_tag = INT_TYPE;
    if (value > INT_MAX || value < INT_MIN)
        type_tag = LONGINT_TYPE;
    
    GpcType *gpc_type = create_primitive_type(type_tag);
    if (gpc_type == NULL)
        return 1; /* Failed to create type */
    
    int result = AddIdentToTable(cur_hash, id, NULL, HASHTYPE_CONST, gpc_type);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(cur_hash, id);
        if (node != NULL)
        {
            node->is_constant = 1;
            node->const_int_value = value;
        }
    }
    else
    {
        /* Failed to add, clean up GpcType */
        destroy_gpc_type(gpc_type);
    }
    return result;
}

/* Pushes a constant with explicit GpcType onto the current scope (head) */
int PushConstOntoScope_Typed(SymTab_t *symtab, char *id, long long value, GpcType *type)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);
    assert(type != NULL && "GpcType must be provided for typed constant");

    HashTable_t *cur_hash = (HashTable_t *)symtab->stack_head->cur;
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

/* Pushes a real constant onto the current scope (head) */
int PushRealConstOntoScope(SymTab_t *symtab, char *id, double value)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash = (HashTable_t *)symtab->stack_head->cur;
    
    GpcType *gpc_type = create_primitive_type(REAL_TYPE);
    if (gpc_type == NULL)
        return 1; /* Failed to create type */
    
    int result = AddIdentToTable(cur_hash, id, NULL, HASHTYPE_CONST, gpc_type);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(cur_hash, id);
        if (node != NULL)
        {
            node->is_constant = 1;
            node->const_real_value = value;
        }
    }
    else
    {
        /* Failed to add, clean up GpcType */
        destroy_gpc_type(gpc_type);
    }
    return result;
}

/* Pushes a string constant onto the current scope (head) */
int PushStringConstOntoScope(SymTab_t *symtab, char *id, const char *value)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);
    assert(value != NULL);

    HashTable_t *cur_hash = (HashTable_t *)symtab->stack_head->cur;
    
    GpcType *gpc_type = create_primitive_type(STRING_TYPE);
    if (gpc_type == NULL)
        return 1; /* Failed to create type */
    
    int result = AddIdentToTable(cur_hash, id, NULL, HASHTYPE_CONST, gpc_type);
    if (result == 0)
    {
        HashNode_t *node = FindIdentInTable(cur_hash, id);
        if (node != NULL)
        {
            node->is_constant = 1;
            node->const_string_value = strdup(value);
            if (node->const_string_value == NULL)
            {
                /* Memory allocation failed, clean up */
                destroy_gpc_type(gpc_type);
                return 1;
            }
        }
    }
    else
    {
        /* Failed to add, clean up GpcType */
        destroy_gpc_type(gpc_type);
    }
    return result;
}

/* Pushes a new procedure onto the current scope (head) */
/* NOTE: args can be NULL to represent no args */
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

    /* Create GpcType from legacy parameters */
    GpcType *gpc_type = NULL;
    
    if (record_type != NULL)
    {
        /* Create record type */
        gpc_type = create_record_type(record_type);
    }
    else if (type_alias != NULL)
    {
        /* Use the comprehensive TypeAlias → GpcType converter
         * This handles ALL cases: arrays, pointers, sets, enums, files, primitives */
        gpc_type = create_gpc_type_from_type_alias(type_alias, symtab);
        
        /* If conversion failed (e.g., forward reference), we'll handle it below */
    }
    else if (var_type != HASHVAR_UNTYPED)
    {
        /* Create from var_type */
        gpc_type = gpc_type_from_var_type(var_type);
    }
    
    /* All cases should create a GpcType now. If gpc_type is NULL, it means:
     * - Truly UNTYPED (var_type == HASHVAR_UNTYPED)
     * - This is valid and we use NULL GpcType */
    return PushTypeOntoScope_Typed(symtab, id, gpc_type);
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
    
    return AddIdentToTable(cur_hash, id, NULL, HASHTYPE_VAR, type);
}

/* Pushes a new array with a GpcType onto the current scope */
int PushArrayOntoScope_Typed(SymTab_t *symtab, char *id, GpcType *type)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;
    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    return AddIdentToTable(cur_hash, id, NULL, HASHTYPE_ARRAY, type);
}

/* Pushes a new procedure with a GpcType onto the current scope */
int PushProcedureOntoScope_Typed(SymTab_t *symtab, char *id, char *mangled_id, GpcType *type)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;
    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    
    return AddIdentToTable(cur_hash, id, mangled_id, HASHTYPE_PROCEDURE, type);
}

/* Pushes a new function with a GpcType onto the current scope */
int PushFunctionOntoScope_Typed(SymTab_t *symtab, char *id, char *mangled_id, GpcType *type)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;
    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    
    return AddIdentToTable(cur_hash, id, mangled_id, HASHTYPE_FUNCTION, type);
}

/* Pushes a new function return value with a GpcType onto the current scope */
int PushFuncRetOntoScope_Typed(SymTab_t *symtab, char *id, GpcType *type)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;
    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    return AddIdentToTable(cur_hash, id, NULL, HASHTYPE_FUNCTION_RETURN, type);
}

/* Pushes a new type declaration with a GpcType onto the current scope */
int PushTypeOntoScope_Typed(SymTab_t *symtab, char *id, GpcType *type)
{
    assert(symtab != NULL);
    assert(symtab->stack_head != NULL);
    assert(id != NULL);

    HashTable_t *cur_hash;
    cur_hash = (HashTable_t *)symtab->stack_head->cur;
    return AddIdentToTable(cur_hash, id, NULL, HASHTYPE_TYPE, type);
}

/* Adds a built-in type with a GpcType */
int AddBuiltinType_Typed(SymTab_t *symtab, char *id, GpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    assert(type != NULL);

    return AddIdentToTable(symtab->builtins, id, NULL, HASHTYPE_TYPE, type);
}

/* Adds a built-in procedure with a GpcType */
int AddBuiltinProc_Typed(SymTab_t *symtab, char *id, GpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    assert(type != NULL);
    assert(type->kind == TYPE_KIND_PROCEDURE && "Builtin proc must have procedure type");
    assert(type->info.proc_info.return_type == NULL && "Procedure must not have return type");

    return AddIdentToTable(symtab->builtins, id, NULL, HASHTYPE_BUILTIN_PROCEDURE, type);
}

/* Adds a built-in function with a GpcType */
int AddBuiltinFunction_Typed(SymTab_t *symtab, char *id, GpcType *type)
{
    assert(symtab != NULL);
    assert(id != NULL);
    assert(type != NULL);
    assert(type->kind == TYPE_KIND_PROCEDURE && "Builtin function must have procedure type");
    assert(type->info.proc_info.return_type != NULL && "Function must have return type");

    return AddIdentToTable(symtab->builtins, id, NULL, HASHTYPE_FUNCTION, type);
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
