/*
    Damon Gwinn
    Linked list for tree_t, Statement, and Expression structures

    WARNING: List will NOT free the contents of the void pointer cur.
        This must be freed by the user.
*/
#ifndef TREE_LIST_H
#define TREE_LIST_H

#include <stdio.h>

/* Careful with using LIST_UNSPECIFIED. Can cause errors on switch statements */
enum ListType{LIST_TREE, LIST_STMT, LIST_EXPR, LIST_STRING,
              LIST_RECORD_FIELD, LIST_CASE_BRANCH, LIST_SET_ELEMENT,
              LIST_VARIANT_PART, LIST_VARIANT_BRANCH, LIST_CLASS_MEMBER,
              LIST_UNSPECIFIED};

/* Our linked list of tree type nodes */
typedef struct List ListNode_t;
typedef struct List
{
    enum ListType type;
    void *cur;

    ListNode_t *next;
} ListNode_t;

/* Creates a list node */
ListNode_t *CreateListNode(void *new_obj, enum ListType type);

/* This is FIFO style */
/* Returns the new head node */
ListNode_t *PushListNodeFront(ListNode_t *head_node, ListNode_t *new_head);

/* This is a traditional array style */
/* TODO: Make more efficient */
/* Returns the head node */
ListNode_t *PushListNodeBack(ListNode_t *head_node, ListNode_t *new_node);

/* Concatenates two lists by appending back_list to the end of front_list. */
ListNode_t *ConcatList(ListNode_t *front_list, ListNode_t *back_list);

/* Inserting a node given the previous node */
void InsertListNode(ListNode_t *prev, ListNode_t *new_node);

/* Pops the top of the list */
ListNode_t *PopListHeadNode(ListNode_t *head);

/* Returns pointer to the next node */
ListNode_t *DeleteListNode(ListNode_t *node, ListNode_t *prev);

void DestroyList(ListNode_t *head_node);

void PrintList(ListNode_t *head_node, FILE *f, int num_indent);

/* Returns the length of the list */
int ListLength(ListNode_t *head_node);

#endif
