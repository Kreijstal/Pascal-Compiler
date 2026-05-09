#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "List.h"
#include "arena.h"

int main()
{
    ListNode_t *head;
    ListNode_t *heap_head;
    ListNode_t *arena_mid;
    ListNode_t *arena_tail;
    arena_t *arena;

    head = CreateListNode(NULL, LIST_TREE);
    head = PushListNodeFront(head, CreateListNode(NULL, LIST_STMT));
    head = PushListNodeFront(head, CreateListNode(NULL, LIST_EXPR));

    PrintList(head, stderr, 0);
    fprintf(stderr, "%d, %d, %d\n", head->type, head->next->type, head->next->next->type);

    DeleteListNode(head->next, head);
    PrintList(head, stderr, 0);

    head = PushListNodeFront(head, CreateListNode(NULL, LIST_TREE));
    PrintList(head, stderr, 0);
    fprintf(stderr, "%d, %d, %d\n", head->type, head->next->type, head->next->next->type);

    DestroyList(head);

    head = CreateListNode((void *)1, LIST_UNSPECIFIED);
    arena = arena_create(256);
    arena_set_global(arena);
    head = PushListNodeFront(head, CreateListNode((void *)2, LIST_UNSPECIFIED));
    DestroyList(head);

    arena_destroy(arena);
    arena = arena_create(256);
    heap_head = CreateListNode((void *)1, LIST_UNSPECIFIED);
    arena_set_global(arena);
    arena_mid = CreateListNode((void *)2, LIST_UNSPECIFIED);
    arena_tail = CreateListNode((void *)3, LIST_UNSPECIFIED);
    heap_head->next = arena_mid;
    arena_mid->next = arena_tail;
    assert(DeleteListNode(arena_mid, heap_head) == arena_tail);
    assert(heap_head->next == arena_tail);
    DestroyList(heap_head);

    arena_destroy(arena);
    return 0;
}
