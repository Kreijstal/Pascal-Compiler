/*
    Damon Gwinn
    Hash table of identifiers
    Used for scoping (semantic checking)

    WARNING: Hash table will NOT free given identifier strings or args when destroyed
        Remember to free given identifier strings and args manually
*/

#ifndef HASH_TABLE_H
#define HASH_TABLE_H
#define TABLE_SIZE	211

#include <stdio.h>
#include <assert.h>
#include "../../List/List.h"
#include "../../ParseTree/GpcType.h"

struct RecordType;
struct TypeAlias;

enum HashType{HASHTYPE_VAR, HASHTYPE_ARRAY, HASHTYPE_CONST, HASHTYPE_PROCEDURE, HASHTYPE_FUNCTION,
    HASHTYPE_FUNCTION_RETURN, HASHTYPE_BUILTIN_PROCEDURE, HASHTYPE_TYPE};
enum VarType{
    HASHVAR_INTEGER,
    HASHVAR_LONGINT,
    HASHVAR_REAL,
    HASHVAR_PROCEDURE,
    HASHVAR_UNTYPED,
    HASHVAR_PCHAR,
    HASHVAR_RECORD,
    HASHVAR_ARRAY,
    HASHVAR_BOOLEAN,
    HASHVAR_CHAR,
    HASHVAR_POINTER,
    HASHVAR_SET,
    HASHVAR_ENUM,
    HASHVAR_FILE
};

/* Items we put in the hash table */
typedef struct HashNode
{
    char *id;
    char *canonical_id;
    char *mangled_id;
    enum HashType hash_type;
    
    /* Unified type system - this one pointer replaces var_type, type_alias, 
     * record_type, is_array, array_start, array_end, and other scattered type info.
     * 
     * When type != NULL: GpcType contains all type information
     * When type == NULL: Node is UNTYPED (untyped procedure parameters in Pascal)
     * 
     * ALL type information queries MUST use helper functions:
     * - hashnode_get_record_type()
     * - hashnode_get_type_alias()
     * - hashnode_is_array()
     * - hashnode_get_array_bounds()
     * - hashnode_get_element_size()
     * - hashnode_is_dynamic_array()
     */
    GpcType *type;

    /* Symbol table resources */
    int referenced;
    int mutated;

    int is_constant;
    long long const_int_value;
    double const_real_value;
    char *const_string_value;  /* Owned by HashNode, must be freed */

    int is_var_parameter;
    int requires_static_link;
    int defined_in_unit;
    int unit_is_public;

} HashNode_t;

/* Our actual hash table */
typedef struct HashTable
{
    ListNode_t *table[TABLE_SIZE];

} HashTable_t;

/* Gives a new hash tables with NULL'd out list pointers */
HashTable_t *InitHashTable();

/* Adds an identifier to the table */
/* Returns 0 if successfully added, 1 if the identifier already exists */
int AddIdentToTable(HashTable_t *table, char *id, char *mangled_id,
    enum HashType hash_type, GpcType *type);

/* Searches for the given identifier in the table. Returns NULL if not found */
/* Mutating tells whether it's being referenced in an assignment context */
HashNode_t *FindIdentInTable(HashTable_t *table, char *id);

/* Searches for all instances of a given identifier in the table. Returns a list of HashNode_t* or NULL if not found */
ListNode_t *FindAllIdentsInTable(HashTable_t *table, char *id);

/* Resets hash node mutation and reference status */
void ResetHashNodeStatus(HashNode_t *hash_node);

/* Frees any and all allocated ListNode_t pointers */
void DestroyHashTable(HashTable_t *table);

/* Destroys special builtin procedure addons */
void DestroyBuiltin(HashNode_t *);

/* Prints all entries in the HashTable */
void PrintHashTable(HashTable_t *table, FILE *f, int num_indent);

/* Check if node represents an array */
static inline int hashnode_is_array(const HashNode_t *node)
{
    if (node == NULL) return 0;
    if (node->type != NULL) {
        return gpc_type_is_array(node->type);
    }
    /* UNTYPED nodes are not arrays */
    return 0;
}

/* Helper functions to query HashNode type information via GpcType */

/* Check if node represents a record */
static inline int hashnode_is_record(const HashNode_t *node)
{
    if (node == NULL) return 0;
    if (node->type != NULL) {
        return gpc_type_is_record(node->type);
    }
    /* UNTYPED nodes are not records */
    return 0;
}

/* Check if node represents a dynamic array */
static inline int hashnode_is_dynamic_array(const HashNode_t *node)
{
    if (node == NULL) return 0;
    if (node->type != NULL && gpc_type_is_array(node->type)) {
        return gpc_type_is_dynamic_array(node->type);
    }
    /* No fallback needed - is_dynamic_array field has been removed */
    /* Dynamic array info must come from GpcType */
    return 0;
}

/* Get array bounds from node */
static inline void hashnode_get_array_bounds(const HashNode_t *node, int *start, int *end)
{
    if (node == NULL) {
        if (start) *start = 0;
        if (end) *end = 0;
        return;
    }
    if (node->type != NULL && gpc_type_is_array(node->type)) {
        gpc_type_get_array_bounds(node->type, start, end);
        return;
    }
    /* No fallback - array_start/end fields have been removed */
    /* Array bounds must come from GpcType */
    if (start) *start = 0;
    if (end) *end = 0;
}

/* Get element size from array node */
static inline int hashnode_get_element_size(const HashNode_t *node)
{
    if (node == NULL) return 0;
    if (node->type != NULL && gpc_type_is_array(node->type)) {
        GpcType *element_type = gpc_type_get_array_element_type(node->type);
        if (element_type != NULL) {
            return gpc_type_sizeof(element_type);
        }
    }
    /* No fallback - element_size field has been removed */
    /* Element size must come from GpcType */
    return 0;
}

static inline int hashnode_requires_static_link(const HashNode_t *node)
{
    return (node != NULL) ? node->requires_static_link : 0;
}

/* Get record type from node */
static inline struct RecordType* hashnode_get_record_type(const HashNode_t *node)
{
    if (node == NULL) return NULL;
    if (node->type != NULL && gpc_type_is_record(node->type)) {
        return gpc_type_get_record(node->type);
    }
    /* UNTYPED nodes have no record type */
    return NULL;
}

/* Get type alias from node */
static inline struct TypeAlias* hashnode_get_type_alias(const HashNode_t *node)
{
    if (node == NULL) return NULL;
    if (node->type != NULL) {
        return gpc_type_get_type_alias(node->type);
    }
    /* UNTYPED nodes have no type alias */
    return NULL;
}

/* The well-known symbol hash function
 * -----------------------------------------------------------------------------
 * hashpjw
 * Peter J. Weinberger's hash function
 * Source: Aho, Sethi, and Ullman, "Compilers", Addison-Wesley, 1986 (page 436).
 */
int hashpjw( char *s );

#endif
