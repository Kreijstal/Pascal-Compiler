/*
    Damon Gwinn
    Hash table of identifiers
    Used for scoping (semantic checking)
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "../../List/List.h"
#include "../../ParseTree/tree.h"
#include "../../ParseTree/type_tags.h"
#include "../../../identifier_utils.h"
#include "HashTable.h"

/* Gives a new hash tables with NULL'd out list pointers */
HashTable_t *InitHashTable()
{
    HashTable_t *new_table = (HashTable_t *)malloc(sizeof(HashTable_t));
    assert(new_table != NULL);

    int i;
    for(i = 0; i < TABLE_SIZE; ++i)
        new_table->table[i] = NULL;

    return new_table;
}

/* Helper function to convert primitive type tag to VarType */
static enum VarType primitive_tag_to_var_type(int primitive_tag)
{
    switch (primitive_tag) {
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
        case POINTER_TYPE:
            return HASHVAR_POINTER;
        case SET_TYPE:
            return HASHVAR_SET;
        case ENUM_TYPE:
            return HASHVAR_ENUM;
        case FILE_TYPE:
            return HASHVAR_FILE;
        case RECORD_TYPE:
            return HASHVAR_RECORD;
        case PROCEDURE:
            return HASHVAR_PROCEDURE;
        default:
            return HASHVAR_UNTYPED;
    }
}

/* Adds an identifier to the table - NEW VERSION with GpcType */
/* Returns 0 if successfully added, 1 if the identifier already exists */
int AddIdentToTable(HashTable_t *table, char *id, char *mangled_id,
    enum HashType hash_type, ListNode_t *args, GpcType *type)
{
    ListNode_t *list, *cur;
    HashNode_t *hash_node;
    int hash;

    assert(table != NULL);
    assert(id != NULL);

    char *canonical_id = pascal_identifier_lower_dup(id);
    if (canonical_id == NULL)
        return 1;

    hash = hashpjw(canonical_id);
    list = table->table[hash];
    if(list == NULL)
    {
        hash_node = (HashNode_t *)malloc(sizeof(HashNode_t));
        assert(hash_node != NULL);
        hash_node->hash_type = hash_type;
        hash_node->type = type; // NEW: Use unified type system
        hash_node->id = strdup(id);
        if (hash_node->id == NULL)
        {
            free(hash_node);
            free(canonical_id);
            return 1;
        }
        hash_node->canonical_id = canonical_id;
        hash_node->mangled_id = mangled_id;
        hash_node->args = args;
        hash_node->referenced = 0;
        hash_node->mutated = 0;
        hash_node->is_constant = 0;
        hash_node->const_int_value = 0;
        hash_node->is_var_parameter = 0;
        
        // Initialize legacy fields to defaults for backward compatibility
        hash_node->var_type = HASHVAR_UNTYPED;
        
        // Set var_type based on GpcType if available
        if (type != NULL) {
            if (type->kind == TYPE_KIND_PROCEDURE) {
                if (type->info.proc_info.return_type == NULL) {
                    // It's a procedure (no return type)
                    hash_node->var_type = HASHVAR_PROCEDURE;
                } else {
                    // It's a function (has return type)
                    // Extract the return type and set var_type accordingly
                    GpcType *return_type = type->info.proc_info.return_type;
                    if (return_type->kind == TYPE_KIND_PRIMITIVE) {
                        hash_node->var_type = primitive_tag_to_var_type(return_type->info.primitive_type_tag);
                    } else if (return_type->kind == TYPE_KIND_POINTER) {
                        hash_node->var_type = HASHVAR_POINTER;
                    } else if (return_type->kind == TYPE_KIND_RECORD) {
                        hash_node->var_type = HASHVAR_RECORD;
                    } else if (return_type->kind == TYPE_KIND_ARRAY) {
                        hash_node->var_type = HASHVAR_ARRAY;
                    }
                    // Note: var_type remains HASHVAR_UNTYPED if we can't determine it
                }
            } else if (type->kind == TYPE_KIND_PRIMITIVE) {
                // For non-procedure types, set var_type directly
                hash_node->var_type = primitive_tag_to_var_type(type->info.primitive_type_tag);
            } else if (type->kind == TYPE_KIND_POINTER) {
                hash_node->var_type = HASHVAR_POINTER;
            } else if (type->kind == TYPE_KIND_RECORD) {
                hash_node->var_type = HASHVAR_RECORD;
            } else if (type->kind == TYPE_KIND_ARRAY) {
                hash_node->var_type = HASHVAR_ARRAY;
            }
        }
        
        hash_node->record_type = NULL;
        hash_node->is_array = (hash_type == HASHTYPE_ARRAY);
        hash_node->array_start = 0;
        hash_node->array_end = 0;
        hash_node->element_size = 0;
        hash_node->is_dynamic_array = 0;
        hash_node->type_alias = NULL;

        table->table[hash] = CreateListNode(hash_node, LIST_UNSPECIFIED);
        return 0;
    }
    else
    {
        cur = list;
        while(cur != NULL)
        {
            hash_node = (HashNode_t *)cur->cur;
            if(strcmp(hash_node->canonical_id, canonical_id) == 0)
            {
                int is_new_proc_func = (hash_type == HASHTYPE_PROCEDURE || hash_type == HASHTYPE_FUNCTION);
                int is_existing_proc_func = (hash_node->hash_type == HASHTYPE_PROCEDURE || hash_node->hash_type == HASHTYPE_FUNCTION);

                if (!is_new_proc_func || !is_existing_proc_func)
                {
                    // If either is not a proc/func, it's a redeclaration error.
                    free(canonical_id);
                    return 1;
                }
            }
            cur = cur->next;
        }

        /* Success if here */
        hash_node = (HashNode_t *)malloc(sizeof(HashNode_t));
        assert(hash_node != NULL);
        hash_node->hash_type = hash_type;
        hash_node->type = type; // NEW: Use unified type system
        hash_node->id = strdup(id);
        if (hash_node->id == NULL)
        {
            free(hash_node);
            free(canonical_id);
            return 1;
        }
        hash_node->canonical_id = canonical_id;
        hash_node->mangled_id = mangled_id;
        hash_node->args = args;
        hash_node->referenced = 0;
        hash_node->mutated = 0;
        hash_node->is_constant = 0;
        hash_node->const_int_value = 0;
        hash_node->is_var_parameter = 0;
        
        // Initialize legacy fields to defaults for backward compatibility
        hash_node->var_type = HASHVAR_UNTYPED;
        
        // Set var_type based on GpcType if available
        if (type != NULL) {
            if (type->kind == TYPE_KIND_PROCEDURE) {
                if (type->info.proc_info.return_type == NULL) {
                    // It's a procedure (no return type)
                    hash_node->var_type = HASHVAR_PROCEDURE;
                } else {
                    // It's a function (has return type)
                    // Extract the return type and set var_type accordingly
                    GpcType *return_type = type->info.proc_info.return_type;
                    if (return_type->kind == TYPE_KIND_PRIMITIVE) {
                        hash_node->var_type = primitive_tag_to_var_type(return_type->info.primitive_type_tag);
                    } else if (return_type->kind == TYPE_KIND_POINTER) {
                        hash_node->var_type = HASHVAR_POINTER;
                    } else if (return_type->kind == TYPE_KIND_RECORD) {
                        hash_node->var_type = HASHVAR_RECORD;
                    } else if (return_type->kind == TYPE_KIND_ARRAY) {
                        hash_node->var_type = HASHVAR_ARRAY;
                    }
                    // Note: var_type remains HASHVAR_UNTYPED if we can't determine it
                }
            } else if (type->kind == TYPE_KIND_PRIMITIVE) {
                // For non-procedure types, set var_type directly
                hash_node->var_type = primitive_tag_to_var_type(type->info.primitive_type_tag);
            } else if (type->kind == TYPE_KIND_POINTER) {
                hash_node->var_type = HASHVAR_POINTER;
            } else if (type->kind == TYPE_KIND_RECORD) {
                hash_node->var_type = HASHVAR_RECORD;
            } else if (type->kind == TYPE_KIND_ARRAY) {
                hash_node->var_type = HASHVAR_ARRAY;
            }
        }
        
        hash_node->record_type = NULL;
        hash_node->is_array = (hash_type == HASHTYPE_ARRAY);
        hash_node->array_start = 0;
        hash_node->array_end = 0;
        hash_node->element_size = 0;
        hash_node->is_dynamic_array = 0;
        hash_node->type_alias = NULL;

        table->table[hash] = PushListNodeFront(list, CreateListNode(hash_node, LIST_UNSPECIFIED));
        return 0;
    }
}

/* Adds an identifier to the table - LEGACY VERSION */
/* Returns 1 if successfully added, 0 if the identifier already exists */
int AddIdentToTable_Legacy(HashTable_t *table, char *id, char *mangled_id, enum VarType var_type,
    enum HashType hash_type, ListNode_t *args, struct RecordType *record_type,
    struct TypeAlias *type_alias)
{
    ListNode_t *list, *cur;
    HashNode_t *hash_node;
    int hash;

    assert(table != NULL);
    assert(id != NULL);

    char *canonical_id = pascal_identifier_lower_dup(id);
    if (canonical_id == NULL)
        return 1;

    hash = hashpjw(canonical_id);
    list = table->table[hash];
    if(list == NULL)
    {
        hash_node = (HashNode_t *)malloc(sizeof(HashNode_t));
        assert(hash_node != NULL);
        hash_node->hash_type = hash_type;
        hash_node->var_type = var_type;
        hash_node->type = NULL;
        hash_node->id = strdup(id);
        if (hash_node->id == NULL)
        {
            free(hash_node);
            free(canonical_id);
            return 1;
        }
        hash_node->canonical_id = canonical_id;
        hash_node->mangled_id = mangled_id;
        hash_node->args = args;
        hash_node->record_type = record_type;
        hash_node->referenced = 0;
        hash_node->mutated = 0;
        hash_node->is_constant = 0;
        hash_node->const_int_value = 0;
        hash_node->is_var_parameter = 0;
        hash_node->is_array = (hash_type == HASHTYPE_ARRAY);
        hash_node->array_start = 0;
        hash_node->array_end = 0;
        hash_node->element_size = 0;
        hash_node->is_dynamic_array = 0;
        hash_node->type_alias = type_alias;

        table->table[hash] = CreateListNode(hash_node, LIST_UNSPECIFIED);
        return 0;
    }
    else
    {
        cur = list;
        while(cur != NULL)
        {
            hash_node = (HashNode_t *)cur->cur;
            if(strcmp(hash_node->canonical_id, canonical_id) == 0)
            {
                int is_new_proc_func = (hash_type == HASHTYPE_PROCEDURE || hash_type == HASHTYPE_FUNCTION);
                int is_existing_proc_func = (hash_node->hash_type == HASHTYPE_PROCEDURE || hash_node->hash_type == HASHTYPE_FUNCTION);

                if (!is_new_proc_func || !is_existing_proc_func)
                {
                    // If either is not a proc/func, it's a redeclaration error.
                    free(canonical_id);
                    return 1;
                }
            }
            cur = cur->next;
        }

        /* Success if here */
        hash_node = (HashNode_t *)malloc(sizeof(HashNode_t));
        assert(hash_node != NULL);
        hash_node->hash_type = hash_type;
        hash_node->var_type = var_type;
        hash_node->type = NULL;
        hash_node->id = strdup(id);
        if (hash_node->id == NULL)
        {
            free(hash_node);
            free(canonical_id);
            return 1;
        }
        hash_node->canonical_id = canonical_id;
        hash_node->mangled_id = mangled_id;
        hash_node->args = args;
        hash_node->record_type = record_type;
        hash_node->referenced = 0;
        hash_node->mutated = 0;
        hash_node->is_constant = 0;
        hash_node->const_int_value = 0;
        hash_node->is_var_parameter = 0;
        hash_node->is_array = (hash_type == HASHTYPE_ARRAY);
        hash_node->array_start = 0;
        hash_node->array_end = 0;
        hash_node->element_size = 0;
        hash_node->is_dynamic_array = 0;
        hash_node->type_alias = type_alias;

        table->table[hash] = PushListNodeFront(list, CreateListNode(hash_node, LIST_UNSPECIFIED));
        return 0;
    }
}

/* Searches for the given identifier in the table. Returns NULL if not found */
/* Mutating tells whether it's being referenced in an assignment context */
HashNode_t *FindIdentInTable(HashTable_t *table, char *id)
{
    ListNode_t *list;
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
    else
    {
        while(list != NULL)
        {
            hash_node = (HashNode_t *)list->cur;
            if(strcmp(hash_node->canonical_id, canonical_id) == 0)
            {
                free(canonical_id);
                return hash_node;
            }

            list = list->next;
        }

        free(canonical_id);
        return NULL;
    }
}

/* Searches for all instances of a given identifier in the table. Returns a list of HashNode_t* or NULL if not found */
ListNode_t *FindAllIdentsInTable(HashTable_t *table, char *id)
{
    ListNode_t *list;
    ListNode_t *matches = NULL;
    HashNode_t *hash_node;
    int hash;

    assert(table != NULL);
    assert(id != NULL);

    char *canonical_id = pascal_identifier_lower_dup(id);
    if (canonical_id == NULL)
        return NULL;

    hash = hashpjw(canonical_id);
    list = table->table[hash];

    while(list != NULL)
    {
        hash_node = (HashNode_t *)list->cur;
        if(strcmp(hash_node->canonical_id, canonical_id) == 0)
        {
            if(matches == NULL)
                matches = CreateListNode(hash_node, LIST_UNSPECIFIED);
            else
                PushListNodeBack(matches, CreateListNode(hash_node, LIST_UNSPECIFIED));
        }
        list = list->next;
    }

    free(canonical_id);
    return matches;
}

/* Resets hash node mutation and reference status */
void ResetHashNodeStatus(HashNode_t *hash_node)
{
    assert(hash_node != NULL);
    hash_node->mutated = 0;
    hash_node->referenced = 0;
}

/* Frees any and all allocated ListNode_t pointers */
void DestroyHashTable(HashTable_t *table)
{
    ListNode_t *cur, *temp;
    HashNode_t *hash_node;

    assert(table != NULL);

    int i;
    for(i = 0; i < TABLE_SIZE; ++i)
    {
        cur = table->table[i];
        while(cur != NULL)
        {
            hash_node = (HashNode_t *)cur->cur;
            if (hash_node->id != NULL)
                free(hash_node->id);
            if (hash_node->canonical_id != NULL)
                free(hash_node->canonical_id);
            if(hash_node->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
                DestroyBuiltin(hash_node);

            free(cur->cur);
            temp = cur->next;
            free(cur);

            cur = temp;
        }
    }
    free(table);
}

/* Destroys special builtin procedure addons */
void DestroyBuiltin(HashNode_t *node)
{
    assert(node != NULL);
    assert(node->hash_type == HASHTYPE_BUILTIN_PROCEDURE);

    destroy_list(node->args);
}

/* Prints all entries in the HashTable */
void PrintHashTable(HashTable_t *table, FILE *f, int num_indent)
{
    int i, j;
    ListNode_t *list;
    HashNode_t *hash_node;

    assert(table != NULL);
    assert(f != NULL);

    for(i = 0; i < TABLE_SIZE; ++i)
    {
        list = table->table[i];
        while(list != NULL)
        {
            for(j = 0; j < num_indent; ++j)
                fprintf(f, "  ");
            hash_node = (HashNode_t *)list->cur;
            fprintf(f, "ID: %s  HASH_TYPE: %d  VAR_TYPE: %d  HASH: %d\n",
            hash_node->id, hash_node->hash_type, hash_node->var_type, i);

            if(hash_node->args != NULL)
            {
                for(j = 0; j < num_indent+1; ++j)
                    fprintf(f, "  ");
                fprintf(f, "[ARGS]: ");
                PrintList(hash_node->args, f, 0);
            }

            list = list->next;
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

    assert(s != NULL);

	for ( p = s; *p != '\0'; p++ )
	{
		h = (h << 4) + (*p);
		if ( (g = h & 0xf0000000) )
		{
			h = h ^ ( g >> 24 );
			h = h ^ g;
		}
	}
	return h % TABLE_SIZE;
}
