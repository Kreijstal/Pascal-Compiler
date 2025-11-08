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
        case ENUM_TYPE:
            return HASHVAR_ENUM;
        case FILE_TYPE:
            return HASHVAR_FILE;
        case POINTER_TYPE:
            return HASHVAR_POINTER;
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
        
        if (params->type != NULL)
            gpc_type_retain(params->type);
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
        
        if (params->type != NULL)
            gpc_type_retain(params->type);
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
        .var_type = HASHVAR_UNTYPED,
        .record_type = NULL,
        .type_alias = NULL
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
            if (hash_node->const_string_value != NULL)
                free(hash_node->const_string_value);
            if (hash_node->type != NULL)
                destroy_gpc_type(hash_node->type);
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
    hash_node->const_string_value = NULL;
    hash_node->is_var_parameter = 0;
    hash_node->requires_static_link = 0;
    
    /* Set identifier */
    hash_node->id = strdup(id);
    if (hash_node->id == NULL)
    {
        free(hash_node);
        return NULL;
    }
    
    /* Legacy parameters are no longer used - assert they're not set */
    if (type != NULL) {
        /* When GpcType is provided, legacy parameters should not be set */
        assert(var_type == HASHVAR_UNTYPED && "When GpcType provided, var_type should be HASHVAR_UNTYPED");
        assert(record_type == NULL && "When GpcType provided, record_type should be NULL");
        assert(type_alias == NULL && "When GpcType provided, type_alias should be NULL");
    }
    /* If type is NULL, this is an UNTYPED node (valid for untyped procedure parameters) */
    /* No legacy fields to populate - they've been removed */
    
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

    /* Allow collision if both are procedures/functions (overloading) */
    if (is_new_proc_func && is_existing_proc_func) {
        return 1;
    }
    
    /* Allow user-declared variables/arrays to shadow the automatic FUNCTION_RETURN "Result" variable
     * This enables the common Pascal pattern of declaring a local "result" variable in functions.
     * The function name itself can still be used for the return value if the user doesn't shadow it. */
    if (existing_node->hash_type == HASHTYPE_FUNCTION_RETURN && 
        (new_hash_type == HASHTYPE_VAR || new_hash_type == HASHTYPE_ARRAY)) {
        return 1;
    }
    
    /* No other collisions allowed */
    return 0;
}

/* Get VarType equivalent from node (for legacy code compatibility) */
enum VarType hashnode_get_var_type(const HashNode_t *node)
{
    if (node == NULL)
        return HASHVAR_UNTYPED;
    
    /* Prefer GpcType when available */
    if (node->type != NULL)
    {
        switch (node->type->kind)
        {
            case TYPE_KIND_PRIMITIVE:
            {
                int tag = gpc_type_get_primitive_tag(node->type);
                return primitive_tag_to_var_type(tag);
            }
            case TYPE_KIND_POINTER:
                return HASHVAR_POINTER;
            case TYPE_KIND_ARRAY:
                return HASHVAR_ARRAY;
            case TYPE_KIND_RECORD:
                return HASHVAR_RECORD;
            case TYPE_KIND_PROCEDURE:
                return HASHVAR_PROCEDURE;
            default:
                return HASHVAR_UNTYPED;
        }
    }
    
    /* If type is NULL, this is an UNTYPED node */
    return HASHVAR_UNTYPED;
}
