/*
    Damon Gwinn
    Linked list for tree_t, Statement, and Expression structures
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "List.h"

/* Creates a list node */
ListNode_t *CreateListNode(void *new_obj, enum ListType type)
{
    ListNode_t *new_node;

    new_node = (ListNode_t *)malloc(sizeof(ListNode_t));
    assert(new_node != NULL);

    new_node->type = type;
    new_node->cur = new_obj;
    new_node->next = NULL;

    return new_node;
}

/* Implemented FIFO style */
ListNode_t *PushListNodeFront(ListNode_t *head_node, ListNode_t *new_head)
{
    assert(head_node != NULL);
    assert(new_head != NULL);

    new_head->next = head_node;

    return new_head;
}
/* This is a traditional array style */
ListNode_t *PushListNodeBack(ListNode_t *head_node, ListNode_t *new_node)
{
    ListNode_t *cur;

    assert(new_node != NULL);
    assert(head_node != NULL);

    cur = head_node;
    while(cur->next != NULL)
        cur = cur->next;

    cur->next = new_node;
    return head_node;
}

void InsertListNode(ListNode_t *prev, ListNode_t *new_node)
{
    assert(prev != NULL);
    assert(new_node != NULL);

    new_node->next = prev->next;
    prev->next = new_node;
}

/* Pops the top of the list */
ListNode_t *PopListHeadNode(ListNode_t *head)
{
    return DeleteListNode(head, NULL);
}

ListNode_t *DeleteListNode(ListNode_t *node, ListNode_t *prev)
{
    ListNode_t *next;
    assert(node != NULL);

    next = node->next;
    if(prev != NULL)
        prev->next = next;

    free(node);

    return next;
}

void DestroyList(ListNode_t *head_node)
{
    ListNode_t *cur, *next;
    cur = head_node;
    while(cur != NULL)
    {
        cur = DeleteListNode(cur, NULL);
    }
}

void PrintList(ListNode_t *head_node, FILE *f, int num_indent)
{
    int i;
    ListNode_t *cur;

    for(i=0; i<num_indent; ++i)
        fprintf(f, "  ");

    cur = head_node;
    while(cur != NULL)
    {
        fprintf(f, "%p", cur->cur);
        cur = cur->next;
        if(cur != NULL)
            fprintf(f, " -> ");
    }
    fprintf(f, "\n");
}

/* Returns the length of the list */
int ListLength(ListNode_t *head_node)
{
    int length = 0;
    ListNode_t *cur = head_node;

    while(cur != NULL)
    {
        length++;
        cur = cur->next;
    }
    return length;
}

ListNode_t *ReverseList(ListNode_t *head) {
    ListNode_t *prev = NULL;
    ListNode_t *current = head;
    ListNode_t *next = NULL;
    while (current != NULL) {
        next = current->next;
        current->next = prev;
        prev = current;
        current = next;
    }
    return prev;
}

ListNode_t *Cons(void *data, ListNode_t *next_node)
{
    ListNode_t *new_node = CreateListNode(data, LIST_UNSPECIFIED);
    new_node->next = next_node;
    return new_node;
}

ListNode_t *Chain(ListNode_t *head, ListNode_t *tail)
{
    if (head == NULL) return tail;
    ListNode_t *cur = head;
    while (cur->next != NULL) {
        cur = cur->next;
    }
    cur->next = tail;
    return head;
}

void destroy_list(ListNode_t *head) {
    ListNode_t *current = head;
    ListNode_t *next;
    while (current != NULL) {
        next = current->next;
        // Note: This does not free the `cur` pointer content,
        // which is the responsibility of the caller.
        free(current);
        current = next;
    }
}
