/*
    Damon Gwinn
    For unit testing the HashTable
*/

#include <stdlib.h>
#include <stdio.h>

#include "../../List/List.h"
#include "HashTable.h"

int main()
{
    HashTable_t *table;
    table = InitHashTable();

    fprintf(stderr, "INITIAL TABLE:\n");
    PrintHashTable(table, stderr, 0);

    /* Test basic variable additions */
    AddIdentToTable(table, "meow", NULL, HASHTYPE_VAR, NULL, NULL);
    AddIdentToTable(table, "lol", NULL, HASHTYPE_ARRAY, NULL, NULL);

    fprintf(stderr, "TABLE:\n");
    PrintHashTable(table, stderr, 0);

    /* Test find operations */
    fprintf(stderr, "%d\n", (FindIdentInTable(table, "meow") != NULL));
    fprintf(stderr, "%d\n", (FindIdentInTable(table, "meow1") != NULL));

    /* Test collision handling */
    fprintf(stderr, "%d\n", AddIdentToTable(table, "meow", NULL, HASHTYPE_ARRAY, NULL, NULL));
    AddIdentToTable(table, "meow1", NULL, HASHTYPE_ARRAY, NULL, NULL);
    fprintf(stderr, "TABLE:\n");
    PrintHashTable(table, stderr, 0);

    /* Test procedure with arguments */
    ListNode_t *args = PushListNodeBack(CreateListNode("arg1", LIST_STRING),
                                      CreateListNode("arg2", LIST_STRING));
    AddIdentToTable(table, "test_proc", NULL, HASHTYPE_PROCEDURE, args, NULL);

    PrintHashTable(table, stderr, 0);

    /* Test legacy API for backward compatibility */
    AddIdentToTable_Legacy(table, "legacy_var", NULL, HASHVAR_INTEGER, HASHTYPE_VAR, NULL, NULL, NULL);

    fprintf(stderr, "FINAL TABLE:\n");
    PrintHashTable(table, stderr, 0);

    DestroyHashTable(table);
    table = NULL;
    return 0;
}