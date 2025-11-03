/*
    Damon Gwinn
    Hash table of identifiers
    Used for scoping (semantic checking)

    WARNING: Hash table will NOT free given identifier strings or args when destroyed
        Remember to free given identifier strings and args manually
*/

#include "../../../identifier_utils.h"
#include "../../ParseTree/GpcType.h"
#include "../../ParseTree/type_tags.h"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "../../List/List.h"
#include "HashTable.h"

/* Forward declarations for internal helper functions */
static HashNode_t* create_hash_node(char* id, char* mangled_id, 
                                   enum HashType hash_type,
                                   GpcType* type, enum VarType var_type,
                                   struct RecordType* record_type, 
                                   struct TypeAlias* type_alias);
static int is_procedure_or_function(enum HashType hash_type);
static int check_collision_allowance(HashNode_t* existing_node, enum HashType new_hash_type);

/* Internal parameter structure to unify both APIs */
typedef struct {
    char* id;
    char* mangled_id;
    enum HashType hash_type;
    GpcType* type;
    enum VarType var_type;
    struct RecordType* record_type;
    struct TypeAlias* type_alias;
} HashTableParams;

HashTable_t *InitHashTable()
{
    HashTable_t *table;
    int i;

    table = (HashTable_t *)malloc(sizeof(HashTable_t));
    assert(table != NULL);

    for(i = 0; i < TABLE_SIZE; i++)
        table->table[i] = NULL;

    return table;
}

enum VarType primitive_tag_to_var_type(int tag)
{
    switch(tag)
    {
        case INT_TYPE:
            return HASHVAR_INTEGER;
        case LONGINT_TYPE:
            return HASHVAR_LONGINT;
        case REAL_TYPE:
            return HASHVAR_REAL;
        case BOOL:
            return HASHVAR_BOOLEAN;
        case CHAR_TYPE:
            return HASHVAR_CHAR;
        case STRING_TYPE:
            return HASHVAR_PCHAR;
        case SET_TYPE:
            return HASHVAR_SET;
        case FILE_TYPE:
            return HASHVAR_FILE;
        default:
            return HASHVAR_UNTYPED;
    }
}

/* Unified internal function for adding identifiers to hash table */
static int add_ident_to_table_internal(HashTable_t *table, const HashTableParams* params)
{
    assert(table != NULL);
    assert(params != NULL);
    assert(params->id != NULL);

    char *canonical_id = pascal_identifier_lower_dup(params->id);
    if (canonical_id == NULL)
        return 1;

    int hash = hashpjw(canonical_id);
    ListNode_t *list = table->table[hash];
    
    if (list == NULL)
    {
        /* Empty bucket - create new entry */
        HashNode_t *hash_node = create_hash_node(params->id, params->mangled_id, 
                                               params->hash_type,
                                               params->type, params->var_type,
                                               params->record_type, params->type_alias);
        if (hash_node == NULL)
        {
            free(canonical_id);
            return 1;
        }
        
        hash_node->canonical_id = canonical_id;
        table->table[hash] = CreateListNode(hash_node, LIST_UNSPECIFIED);
        return 0;
    }
    else
    {
        /* Check for collisions */
        ListNode_t *cur = list;
        while (cur != NULL)
        {
            HashNode_t *existing_node = (HashNode_t *)cur->cur;
            if (strcmp(existing_node->canonical_id, canonical_id) == 0)
            {
                if (!check_collision_allowance(existing_node, params->hash_type))
                {
                    free(canonical_id);
                    return 1;
                }
            }
            cur = cur->next;
        }

        /* No collision or allowed collision - create new entry */
        HashNode_t *hash_node = create_hash_node(params->id, params->mangled_id, 
                                               params->hash_type,
                                               params->type, params->var_type,
                                               params->record_type, params->type_alias);
        if (hash_node == NULL)
        {
            free(canonical_id);
            return 1;
        }
        
        hash_node->canonical_id = canonical_id;
        table->table[hash] = PushListNodeFront(list, CreateListNode(hash_node, LIST_UNSPECIFIED));
        return 0;
    }
}

int AddIdentToTable(HashTable_t *table, char *id, char *mangled_id,
    enum HashType hash_type, GpcType *type)
{
    HashTableParams params = {
        .id = id,
        .mangled_id = mangled_id,
        .hash_type = hash_type,
        .type = type,
        .var_type = HASHVAR_UNTYPED,  // Will be set from GpcType
        .record_type = NULL,
        .type_alias = NULL
    };
    
    return add_ident_to_table_internal(table, &params);
}

int AddIdentToTable_Legacy(HashTable_t *table, char *id, char *mangled_id, enum VarType var_type,
    enum HashType hash_type, ListNode_t *args, struct RecordType *record_type,
    struct TypeAlias *type_alias)
{
    /* NOTE: args parameter is ignored - it's kept only for API compatibility.
     * The legacy system doesn't store args in HashNode anymore. */
    (void)args;  // Suppress unused parameter warning
    
    HashTableParams params = {
        .id = id,
        .mangled_id = mangled_id,
        .hash_type = hash_type,
        .type = NULL,  // Legacy function doesn't use GpcType
        .var_type = var_type,
        .record_type = record_type,
        .type_alias = type_alias
    };
    
    return add_ident_to_table_internal(table, &params);
}

HashNode_t *FindIdentInTable(HashTable_t *table, char *id)
{
    ListNode_t *list, *cur;
    HashNode_t *hash_node;
    int hash;

    assert(table != NULL);
    assert(id != NULL);

    char *canonical_id = pascal_identifier_lower_dup(id);
    if (canonical_id == NULL)
        return NULL;

    hash = hashpjw(canonical_id);
    list = table->table[hash];
    if(list == NULL)
    {
        free(canonical_id);
        return NULL;
    }

    cur = list;
    while(cur != NULL)
    {
        hash_node = (HashNode_t *)cur->cur;
        if(strcmp(hash_node->canonical_id, canonical_id) == 0)
        {
            free(canonical_id);
            return hash_node;
        }
        cur = cur->next;
    }

    free(canonical_id);
    return NULL;
}

ListNode_t *FindAllIdentsInTable(HashTable_t *table, char *id)
{
    ListNode_t *list, *cur;
    HashNode_t *hash_node;
    int hash;
    ListNode_t *found_list = NULL;

    assert(table != NULL);
    assert(id != NULL);

    char *canonical_id = pascal_identifier_lower_dup(id);
    if (canonical_id == NULL)
        return NULL;

    hash = hashpjw(canonical_id);
    list = table->table[hash];
    if(list == NULL)
    {
        free(canonical_id);
        return NULL;
    }

    cur = list;
    while(cur != NULL)
    {
        hash_node = (HashNode_t *)cur->cur;
        if(strcmp(hash_node->canonical_id, canonical_id) == 0)
        {
            if (found_list == NULL)
            {
                found_list = CreateListNode(hash_node, LIST_UNSPECIFIED);
            }
            else
            {
                PushListNodeBack(found_list, CreateListNode(hash_node, LIST_UNSPECIFIED));
            }
        }
        cur = cur->next;
    }

    free(canonical_id);
    return found_list;
}

void ResetHashNodeStatus(HashNode_t *hash_node)
{
    assert(hash_node != NULL);
    hash_node->referenced = 0;
    hash_node->mutated = 0;
}

void DestroyHashTable(HashTable_t *table)
{
    ListNode_t *cur, *temp;
    HashNode_t *hash_node;
    int i;

    assert(table != NULL);

    for(i = 0; i < TABLE_SIZE; i++)
    {
        cur = table->table[i];
        while(cur != NULL)
        {
            hash_node = (HashNode_t *)cur->cur;
            if (hash_node->id != NULL)
                free(hash_node->id);
            if (hash_node->canonical_id != NULL)
                free(hash_node->canonical_id);
            /* Builtin procedures are handled separately - do not call DestroyBuiltin here */
            /* to avoid double-free issues */

            free(cur->cur);
            temp = cur->next;
            free(cur);

            cur = temp;
        }
    }
    free(table);
}

void DestroyBuiltin(HashNode_t *hash_node)
{
    assert(hash_node != NULL);
    assert(hash_node->hash_type == HASHTYPE_BUILTIN_PROCEDURE);

    /* Builtin procedure args are owned by the parser and will be destroyed separately */
    /* Do not destroy args here to avoid double-free issues */
}

void PrintHashTable(HashTable_t *table, FILE *f, int num_indent)
{
    int i;
    ListNode_t *list, *cur;
    HashNode_t *hash_node;
    char indent_str[256];
    int j;

    assert(table != NULL);
    assert(f != NULL);

    for(j = 0; j < num_indent; j++)
        indent_str[j] = '\t';
    indent_str[num_indent] = '\0';

    for(i = 0; i < TABLE_SIZE; i++)
    {
        list = table->table[i];
        cur = list;
        while(cur != NULL)
        {
            hash_node = (HashNode_t *)cur->cur;
            fprintf(f, "%s%s\n", indent_str, hash_node->id);
            cur = cur->next;
        }
    }
}

/* The well-known symbol hash function
 * -----------------------------------------------------------------------------
 * hashpjw
 * Peter J. Weinberger's hash function
 * Source: Aho, Sethi, and Ullman, "Compilers", Addison-Wesley, 1986 (page 436).
 */
int hashpjw( char *s )
{
    char *p;
    unsigned h = 0, g;

    for(p = s; *p != '\0'; p++)
    {
        h = (h << 4) + (*p);
        if((g = h & 0xf0000000))
        {
            h = h ^ (g >> 24);
            h = h ^ g;
        }
    }

    return h % TABLE_SIZE;
}

/* =============================================================================
 * Internal Helper Functions
 * ============================================================================= */

/* Create and initialize a new hash node with default values */
static HashNode_t* create_hash_node(char* id, char* mangled_id, 
                                   enum HashType hash_type,
                                   GpcType* type, enum VarType var_type,
                                   struct RecordType* record_type, 
                                   struct TypeAlias* type_alias)
{
    HashNode_t *hash_node = (HashNode_t *)malloc(sizeof(HashNode_t));
    if (hash_node == NULL)
        return NULL;
        
    assert(hash_node != NULL);
    
    /* Set basic fields */
    hash_node->hash_type = hash_type;
    hash_node->type = type;
    hash_node->mangled_id = mangled_id;
    hash_node->referenced = 0;
    hash_node->mutated = 0;
    hash_node->is_constant = 0;
    hash_node->const_int_value = 0;
    hash_node->is_var_parameter = 0;
    
    /* Set identifier */
    hash_node->id = strdup(id);
    if (hash_node->id == NULL)
    {
        free(hash_node);
        return NULL;
    }
    
    /* Handle type information based on which API is being used */
    if (type != NULL)
    {
        /* New API with GpcType - Initialize legacy fields to safe defaults
         * All type information accessed through GpcType */
        
        /* Assert that we're not accidentally using both APIs */
        assert(var_type == HASHVAR_UNTYPED && "When GpcType provided, var_type should be HASHVAR_UNTYPED");
        assert(record_type == NULL && "When GpcType provided, record_type should be NULL");
        assert(type_alias == NULL && "When GpcType provided, type_alias should be NULL");
        
        /* Initialize legacy fields to safe defaults (0/NULL) */
        hash_node->var_type = HASHVAR_UNTYPED;
        hash_node->record_type = NULL;
        hash_node->type_alias = NULL;
        hash_node->is_array = 0;
        hash_node->array_start = 0;
        hash_node->array_end = 0;
        hash_node->element_size = 0;
        hash_node->is_dynamic_array = 0;
    }
    else
    {
        #ifdef ENABLE_LEGACY_FIELDS_PHASE6
        /* Legacy API - use provided values */
        hash_node->var_type = var_type;
        hash_node->record_type = record_type;
        hash_node->type_alias = type_alias;
        
        /* Set array-specific fields to defaults - will be set later by caller if needed */
        hash_node->is_array = (hash_type == HASHTYPE_ARRAY);
        hash_node->array_start = 0;
        hash_node->array_end = 0;
        hash_node->element_size = 0;
        hash_node->is_dynamic_array = 0;
        #else
        /* Phase 6: Legacy fields removed - this path should not be used */
        (void)var_type; (void)record_type; (void)type_alias; /* Suppress unused warnings */
        assert(0 && "Legacy API path - should use typed API with GpcType");
        #endif
    }
    
    return hash_node;
}

/* Check if a hash type represents a procedure or function */
static int is_procedure_or_function(enum HashType hash_type)
{
    return (hash_type == HASHTYPE_PROCEDURE || hash_type == HASHTYPE_FUNCTION);
}

/* Check if a collision between existing and new entries is allowed */
static int check_collision_allowance(HashNode_t* existing_node, enum HashType new_hash_type)
{
    int is_new_proc_func = is_procedure_or_function(new_hash_type);
    int is_existing_proc_func = is_procedure_or_function(existing_node->hash_type);

    /* Allow collision only if both are procedures/functions */
    return (is_new_proc_func && is_existing_proc_func);
}