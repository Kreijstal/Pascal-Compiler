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
              LIST_RECORD_FIELD, LIST_CLASS_PROPERTY, LIST_CASE_BRANCH, LIST_SET_ELEMENT,
              LIST_VARIANT_PART, LIST_VARIANT_BRANCH, LIST_METHOD_TEMPLATE,
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

/* Returns a shallow copy of the list (new nodes, same content pointers) */
ListNode_t *CopyListShallow(ListNode_t *head_node);

/**
 * ListBuilder - Efficient tail-pointer list construction pattern.
 *
 * This builder pattern eliminates the need for O(n) tail-finding operations
 * when appending to a list. It maintains a pointer to the next-pointer of
 * the last node, allowing O(1) append operations.
 *
 * Example usage:
 *   ListBuilder builder;
 *   list_builder_init(&builder);
 *   list_builder_append(&builder, some_value, LIST_EXPR);
 *   list_builder_append(&builder, another_value, LIST_EXPR);
 *   ListNode_t *result = list_builder_finish(&builder);
 */
typedef struct {
    ListNode_t *head;
    ListNode_t **tail_next;  /* Pointer to the next field of the last node */
} ListBuilder;

/**
 * Initialize a ListBuilder for efficient list construction.
 * Must be called before using list_builder_append.
 */
static inline void list_builder_init(ListBuilder *builder)
{
    if (builder == NULL)
        return;
    builder->head = NULL;
    builder->tail_next = &builder->head;
}

/**
 * Append a new element to the list being built.
 * Returns the created ListNode_t or NULL on allocation failure.
 */
static inline ListNode_t *list_builder_append(ListBuilder *builder, void *value, enum ListType type)
{
    if (builder == NULL)
        return NULL;

    ListNode_t *node = CreateListNode(value, type);
    if (node == NULL)
        return NULL;

    *builder->tail_next = node;
    builder->tail_next = &node->next;
    return node;
}

/**
 * Finish building and return the constructed list.
 * After calling this, the builder can be reused with list_builder_init.
 */
static inline ListNode_t *list_builder_finish(ListBuilder *builder)
{
    if (builder == NULL)
        return NULL;
    return builder->head;
}

/**
 * Extend the builder by appending an existing list.
 * The nodes list is NOT copied - the nodes themselves are appended.
 */
static inline void list_builder_extend(ListBuilder *builder, ListNode_t *nodes)
{
    if (builder == NULL || nodes == NULL)
        return;

    *builder->tail_next = nodes;
    /* Advance tail_next to the end of the appended list */
    while (*builder->tail_next != NULL)
        builder->tail_next = &(*builder->tail_next)->next;
}

#endif
