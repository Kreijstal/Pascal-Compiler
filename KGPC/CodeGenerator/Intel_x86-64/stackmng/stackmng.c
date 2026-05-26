/*
    Damon Gwinn
    Stack Manager structure (see codegen.h for details)
*/

#include "stackmng.h"
#include "../../../Parser/List/List.h"
#include "../../../identifier_utils.h"
#include "../codegen.h"
#include "../register_types.h"
#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if USE_GRAPH_COLORING_ALLOCATOR
#include "../graph_coloring_allocator.h"
#endif

#if KGPC_ENABLE_REG_DEBUG
const char *g_reg_debug_context = "default";
#define REG_DEBUG_LOG(...) fprintf(stderr, __VA_ARGS__)
#else
#define REG_DEBUG_LOG(...) ((void)0)
#endif

/* Sets num_args_alloced to 0 */
void free_arg_regs(void) { num_args_alloced = 0; }

stackmng_t *global_stackmng = NULL;

/* Helpers for getting special registers */
static const char *arg_temp_operand(int num, int size) {
  enum {
    ARG_TEMP_LABEL_SIZE = 64,
    ARG_TEMP_OPERAND_SIZE = 64,
    ARG_TEMP_OPERAND_CACHE_LIMIT = 1024
  };

  if (num < 0 || global_stackmng == NULL || global_stackmng->cur_scope == NULL)
    return NULL;

  char label[ARG_TEMP_LABEL_SIZE];
  if (snprintf(label, sizeof(label), "__arg_temp_%d", num) >=
      (int)sizeof(label))
    return NULL;

  StackNode_t *slot = find_in_temp(label);
  if (slot == NULL)
    slot = add_l_t_bytes(label, size);
  if (slot == NULL)
    return NULL;

  static char operand_cache[ARG_TEMP_OPERAND_CACHE_LIMIT]
                           [ARG_TEMP_OPERAND_SIZE];
  static char overflow_operand[ARG_TEMP_OPERAND_SIZE];
  char *operand = overflow_operand;
  if (num < ARG_TEMP_OPERAND_CACHE_LIMIT)
    operand = operand_cache[num];
  snprintf(operand, sizeof(operand_cache[0]), "-%d(%%rbp)", slot->offset);

  if (num_args_alloced <= num)
    num_args_alloced = num + 1;

  return operand;
}

const char *get_arg_reg64_num(int num) {
  const char *reg = current_arg_reg64(num);
  if (reg != NULL) {
    if (num_args_alloced <= num)
      num_args_alloced = num + 1;
    return reg;
  }
  return arg_temp_operand(num, 8);
}

const char *get_arg_reg32_num(int num) {
  const char *reg = current_arg_reg32(num);
  if (reg != NULL) {
    if (num_args_alloced <= num)
      num_args_alloced = num + 1;
    return reg;
  }
  return arg_temp_operand(num, 4);
}

/******** stackmng *********/

void init_stackmng() {
  assert(global_stackmng == NULL);

  num_args_alloced = 0;
  global_stackmng = (stackmng_t *)malloc(sizeof(stackmng_t));
  assert(global_stackmng != NULL);

  global_stackmng->cur_scope = NULL;
  global_stackmng->reg_stack = init_reg_stack();
}

/* For when you need low level access to the stack */
StackScope_t *get_cur_scope() { return global_stackmng->cur_scope; }

int get_full_stack_offset() {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  StackScope_t *scope;
  int actual_offset, div, rem;

  scope = global_stackmng->cur_scope;

  /* On Windows x64, we need to reserve shadow space at the top of the frame */
  /* The shadow space is reserved for the callee to spill register arguments */
  /* Locals should be placed below the shadow space */
  /* Total stack space needed = locals_size + shadow_space_size */
  actual_offset = scope->t_offset + scope->x_offset + scope->z_offset +
                  current_stack_home_space();

  /* x86_64 requires stack offsets to avoid undefined behavior */
  div = actual_offset / REQUIRED_OFFSET;
  rem = actual_offset % REQUIRED_OFFSET;

  if (rem > 0)
    ++div;

  return div * REQUIRED_OFFSET;
}

void push_stackscope() {
  assert(global_stackmng != NULL);

  StackScope_t *new_scope;
  new_scope = init_stackscope();

  new_scope->prev_scope = global_stackmng->cur_scope;
  global_stackmng->cur_scope = new_scope;
}

void push_stackscope_inherited() {
  assert(global_stackmng != NULL);

  StackScope_t *parent = global_stackmng->cur_scope;
  StackScope_t *new_scope = init_stackscope();

  /* Inherit the parent's cumulative offset so that new variables
   * in this child scope are placed at non-overlapping stack slots.
   * z_offset: parameter area, x_offset: local variables, t_offset: temporaries.
   * Their sum is the total bytes used in the parent scope. */
  if (parent != NULL) {
    new_scope->z_offset =
        parent->z_offset + parent->x_offset + parent->t_offset;
  }

  new_scope->prev_scope = parent;
  global_stackmng->cur_scope = new_scope;
}

void pop_stackscope() {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);

  global_stackmng->cur_scope = free_stackscope(global_stackmng->cur_scope);
}

static inline int align_up(int value, int alignment) {
  if (alignment <= 0)
    return value;
  int remainder = value % alignment;
  if (remainder == 0)
    return value;
  return value + (alignment - remainder);
}

static inline int stack_slot_alignment_for_size(int size) {
  int alignment = (int)sizeof(void *);
  if (alignment < DOUBLEWORD)
    alignment = DOUBLEWORD;
  if ((size == 10 || size >= 16) && alignment < 16)
    alignment = 16;
  return alignment;
}

/* ------------------------------------------------------------------
 * Scope hash index
 *
 * The codegen previously did O(N) linear scans of cur_scope->{t,x,z}
 * for every identifier lookup. With ~13k entries on the x list during
 * pp.pas compilation (mostly record_ctor_N temps), this dominated
 * runtime (>75% inclusive in profiling).
 *
 * We add a parallel open-addressed hash table per list. Each bucket
 * head holds a chain of insertion-order entries for one key (lowercased
 * identifier name), so we preserve the exact semantics of the original
 * linear scans:
 *   - find_t / find_z: return first-inserted match -> head of chain
 *   - find_x:          prefer first non-alias; else first alias
 *     (matches the original `alias_match == NULL` first-wins rule)
 *
 * Hash keys are case-insensitive (lowercased) so collisions match the
 * pascal_identifier_equals comparator exactly. We always verify via
 * pascal_identifier_equals on probe to handle hash collisions.
 * ------------------------------------------------------------------ */

typedef struct ScopeHashEntry {
  ListNode_t *list_node; /* points to a cell in the t/x/z list */
  struct ScopeHashEntry *next_same_key;
} ScopeHashEntry;

typedef struct ScopeHashHead {
  char *key;             /* lowercased label, owned; NULL = empty bucket */
  ScopeHashEntry *first; /* insertion-order chain head */
  ScopeHashEntry *last;  /* insertion-order chain tail */
} ScopeHashHead;

typedef struct ScopeHashTable {
  ScopeHashHead *buckets;
  size_t bucket_count; /* always a power of two */
  size_t live_heads;   /* number of buckets with first != NULL */
} ScopeHashTable;

#define SCOPE_HASH_INITIAL_BUCKETS 16

static unsigned long scope_hash_lower(const char *s) {
  /* djb2 over lowercased bytes. */
  unsigned long h = 5381UL;
  while (*s != '\0') {
    unsigned char c = (unsigned char)*s++;
    h = ((h << 5) + h) + (unsigned long)tolower(c);
  }
  return h;
}

static ScopeHashTable *scope_hash_create(void) {
  ScopeHashTable *ht = (ScopeHashTable *)malloc(sizeof(ScopeHashTable));
  assert(ht != NULL);
  ht->bucket_count = SCOPE_HASH_INITIAL_BUCKETS;
  ht->live_heads = 0;
  ht->buckets =
      (ScopeHashHead *)calloc(ht->bucket_count, sizeof(ScopeHashHead));
  assert(ht->buckets != NULL);
  return ht;
}

static void scope_hash_destroy(ScopeHashTable *ht) {
  if (ht == NULL)
    return;

  for (size_t i = 0; i < ht->bucket_count; ++i) {
    ScopeHashHead *head = &ht->buckets[i];
    if (head->key != NULL)
      free(head->key);
    ScopeHashEntry *e = head->first;
    while (e != NULL) {
      ScopeHashEntry *next = e->next_same_key;
      free(e);
      e = next;
    }
  }
  free(ht->buckets);
  free(ht);
}

/* Returns the bucket head for `key` (case-insensitive), or NULL.
 * If `out_index` is non-NULL, *out_index is set to either the matching
 * bucket index or the first free bucket where a new head would go. */
static ScopeHashHead *scope_hash_probe(ScopeHashTable *ht, const char *key,
                                       size_t *out_index) {
  assert(ht->bucket_count > 0);
  assert((ht->bucket_count & (ht->bucket_count - 1)) == 0);

  unsigned long h = scope_hash_lower(key);
  size_t mask = ht->bucket_count - 1;
  size_t idx = (size_t)h & mask;
  size_t first_free = ht->bucket_count;

  for (size_t step = 0; step < ht->bucket_count; ++step) {
    ScopeHashHead *head = &ht->buckets[idx];
    if (head->key == NULL) {
      if (first_free == ht->bucket_count)
        first_free = idx;
      /* truly empty: no head ever inserted here. Stop probing. */
      if (out_index != NULL)
        *out_index = first_free;
      return NULL;
    }
    if (pascal_identifier_equals(head->key, key)) {
      if (out_index != NULL)
        *out_index = idx;
      return head;
    }
    idx = (idx + 1) & mask;
  }

  /* table full of distinct keys (shouldn't happen since we resize). */
  if (out_index != NULL)
    *out_index = first_free;
  return NULL;
}

static void scope_hash_grow(ScopeHashTable *ht) {
  size_t old_count = ht->bucket_count;
  ScopeHashHead *old_buckets = ht->buckets;

  ht->bucket_count = old_count * 2;
  ht->buckets =
      (ScopeHashHead *)calloc(ht->bucket_count, sizeof(ScopeHashHead));
  assert(ht->buckets != NULL);
  ht->live_heads = 0;

  for (size_t i = 0; i < old_count; ++i) {
    ScopeHashHead *src = &old_buckets[i];
    if (src->key == NULL)
      continue;
    if (src->first == NULL) {
      /* empty head left over from a remove; drop it. */
      free(src->key);
      continue;
    }
    /* Re-home this head. */
    size_t mask = ht->bucket_count - 1;
    unsigned long h = scope_hash_lower(src->key);
    size_t idx = (size_t)h & mask;
    while (ht->buckets[idx].key != NULL)
      idx = (idx + 1) & mask;
    ht->buckets[idx] = *src;
    ht->live_heads++;
  }
  free(old_buckets);
}

static void scope_hash_insert(ScopeHashTable *ht, const char *key,
                              ListNode_t *list_node) {
  if (ht == NULL || key == NULL || list_node == NULL)
    return;

  /* Resize at 75% load of live heads. */
  if ((ht->live_heads + 1) * 4 > ht->bucket_count * 3)
    scope_hash_grow(ht);

  size_t idx = 0;
  ScopeHashHead *head = scope_hash_probe(ht, key, &idx);

  ScopeHashEntry *e = (ScopeHashEntry *)malloc(sizeof(ScopeHashEntry));
  assert(e != NULL);
  e->list_node = list_node;
  e->next_same_key = NULL;

  if (head != NULL) {
    /* existing chain or empty head left from a remove */
    if (head->first == NULL) {
      head->first = e;
      head->last = e;
      ht->live_heads++;
    } else {
      head->last->next_same_key = e;
      head->last = e;
    }
    return;
  }

  /* fresh bucket */
  assert(idx < ht->bucket_count);
  ScopeHashHead *fresh = &ht->buckets[idx];
  assert(fresh->key == NULL);
  fresh->key = pascal_identifier_lower_dup(key);
  assert(fresh->key != NULL);
  fresh->first = e;
  fresh->last = e;
  ht->live_heads++;
}

/* Remove the *latest* (tail) entry for `key`. Returns 1 if something
 * was removed, 0 if no entry existed. Used by remove_last_l_x. */
static int scope_hash_remove_last(ScopeHashTable *ht, const char *key) {
  if (ht == NULL || key == NULL)
    return 0;

  ScopeHashHead *head = scope_hash_probe(ht, key, NULL);
  if (head == NULL || head->first == NULL)
    return 0;

  if (head->first == head->last) {
    free(head->first);
    head->first = NULL;
    head->last = NULL;
    /* keep head->key around as a tombstone-with-name so subsequent
     * inserts of the same key reuse this bucket and probing past it
     * still continues. live_heads decreases. */
    assert(ht->live_heads > 0);
    ht->live_heads--;
    return 1;
  }

  /* Multi-entry chain: walk to find predecessor of tail (rare). */
  ScopeHashEntry *prev = head->first;
  while (prev->next_same_key != head->last)
    prev = prev->next_same_key;
  free(head->last);
  prev->next_same_key = NULL;
  head->last = prev;
  return 1;
}

/* Returns the list-node pointer of the last (most recently inserted)
 * entry for `key`, or NULL. Used by remove_last_l_x. */
static ListNode_t *scope_hash_get_last_listnode(ScopeHashTable *ht,
                                                const char *key) {
  if (ht == NULL || key == NULL)
    return NULL;
  ScopeHashHead *head = scope_hash_probe(ht, key, NULL);
  if (head == NULL || head->last == NULL)
    return NULL;
  return head->last->list_node;
}

/* Find for _t / _z semantics: first-inserted match. */
static StackNode_t *scope_hash_find_first(ScopeHashTable *ht, const char *key) {
  if (ht == NULL || key == NULL)
    return NULL;
  ScopeHashHead *head = scope_hash_probe(ht, key, NULL);
  if (head == NULL || head->first == NULL)
    return NULL;
  return (StackNode_t *)head->first->list_node->cur;
}

/* Find for _x semantics: prefer first non-alias, else first alias.
 * Matches the original linear-scan behavior in stackscope_find_x. */
static StackNode_t *scope_hash_find_x(ScopeHashTable *ht, const char *key) {
  if (ht == NULL || key == NULL)
    return NULL;
  ScopeHashHead *head = scope_hash_probe(ht, key, NULL);
  if (head == NULL || head->first == NULL)
    return NULL;

  StackNode_t *alias_match = NULL;
  for (ScopeHashEntry *e = head->first; e != NULL; e = e->next_same_key) {
    StackNode_t *node = (StackNode_t *)e->list_node->cur;
    if (node == NULL || node->label == NULL)
      continue;
    if (!node->is_alias)
      return node;
    if (alias_match == NULL)
      alias_match = node;
  }
  return alias_match;
}

/* Public wrappers for insert/remove (used by code outside this file
 * that appends to ->x manually). */
void stackscope_index_t_insert(StackScope_t *scope, ListNode_t *node) {
  if (scope == NULL || node == NULL)
    return;
  if (scope->t_index == NULL)
    scope->t_index = scope_hash_create();
  StackNode_t *sn = (StackNode_t *)node->cur;
  if (sn == NULL || sn->label == NULL)
    return;
  scope_hash_insert(scope->t_index, sn->label, node);
}

void stackscope_index_x_insert(StackScope_t *scope, ListNode_t *node) {
  if (scope == NULL || node == NULL)
    return;
  if (scope->x_index == NULL)
    scope->x_index = scope_hash_create();
  StackNode_t *sn = (StackNode_t *)node->cur;
  if (sn == NULL || sn->label == NULL)
    return;
  scope_hash_insert(scope->x_index, sn->label, node);
}

void stackscope_index_z_insert(StackScope_t *scope, ListNode_t *node) {
  if (scope == NULL || node == NULL)
    return;
  if (scope->z_index == NULL)
    scope->z_index = scope_hash_create();
  StackNode_t *sn = (StackNode_t *)node->cur;
  if (sn == NULL || sn->label == NULL)
    return;
  scope_hash_insert(scope->z_index, sn->label, node);
}

void stackscope_index_x_remove(StackScope_t *scope, ListNode_t *node) {
  if (scope == NULL || node == NULL || scope->x_index == NULL)
    return;
  StackNode_t *sn = (StackNode_t *)node->cur;
  if (sn == NULL || sn->label == NULL)
    return;
  /* The remove_last_l_x caller removes the latest-inserted match,
   * which is exactly what scope_hash_remove_last does. */
  scope_hash_remove_last(scope->x_index, sn->label);
}

static void stackscope_append_node(ListNode_t **head, ListNode_t **tail,
                                   ListNode_t *node) {
  assert(head != NULL);
  assert(tail != NULL);
  assert(node != NULL);

  node->next = NULL;
  if (*head == NULL) {
    *head = node;
    *tail = node;
    return;
  }

  assert(*tail != NULL);
  (*tail)->next = node;
  *tail = node;
}

/* Combined append-to-list-and-index helpers. Use these for any new code;
 * they keep the hash index in sync. */
static inline void stackscope_append_t(StackScope_t *scope, ListNode_t *node) {
  stackscope_append_node(&scope->t, &scope->t_tail, node);
  stackscope_index_t_insert(scope, node);
}

static inline void stackscope_append_x(StackScope_t *scope, ListNode_t *node) {
  stackscope_append_node(&scope->x, &scope->x_tail, node);
  stackscope_index_x_insert(scope, node);
}

static inline void stackscope_append_z(StackScope_t *scope, ListNode_t *node) {
  stackscope_append_node(&scope->z, &scope->z_tail, node);
  stackscope_index_z_insert(scope, node);
}

/* Adds temporary storage to t */
StackNode_t *add_l_t(char *label) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);

  StackScope_t *cur_scope;
  StackNode_t *new_node;
  int offset;

  /* Reserve space that can hold pointer-sized temporaries. */
  int temp_size = (int)sizeof(void *);
  if (temp_size < DOUBLEWORD)
    temp_size = DOUBLEWORD;

  cur_scope = global_stackmng->cur_scope;

  int alignment = stack_slot_alignment_for_size(temp_size);
  cur_scope->t_offset = align_up(cur_scope->t_offset, alignment);
  cur_scope->t_offset = align_up(cur_scope->t_offset, temp_size);
  cur_scope->t_offset += temp_size;

  offset = cur_scope->z_offset + cur_scope->x_offset + cur_scope->t_offset;

  new_node = init_stack_node(offset, label, temp_size);

  stackscope_append_t(cur_scope, CreateListNode(new_node, LIST_UNSPECIFIED));

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: Added %s to t_offset %d\n", label, offset);
#endif

  return new_node;
}

StackNode_t *add_l_t_bytes(char *label, int size) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);

  StackScope_t *cur_scope = global_stackmng->cur_scope;
  StackNode_t *new_node;

  if (size <= 0)
    size = DOUBLEWORD;
  int alignment = stack_slot_alignment_for_size(size);
  int aligned_size = align_up(size, alignment);

  /* Align the cumulative offset (z + x + t) so the final stack slot
     is properly aligned.  Aligning t_offset alone is insufficient when
     x_offset is not a multiple of the required alignment. */
  int base = cur_scope->z_offset + cur_scope->x_offset;
  int total_before = base + cur_scope->t_offset;
  int total_aligned = align_up(total_before, alignment);
  cur_scope->t_offset = total_aligned - base;
  cur_scope->t_offset += aligned_size;

  int offset = base + cur_scope->t_offset;

  new_node = init_stack_node(offset, label, aligned_size);
  new_node->element_size = size;

  stackscope_append_t(cur_scope, CreateListNode(new_node, LIST_UNSPECIFIED));

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: Added %s to t_offset %d (bytes=%d)\n", label, offset,
                aligned_size);
#endif

  return new_node;
}

/* Adds storage to x */
StackNode_t *add_l_x(char *label, int size) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);

  StackScope_t *cur_scope;
  StackNode_t *new_node;
  int offset;

  cur_scope = global_stackmng->cur_scope;

  if (size <= 0)
    size = DOUBLEWORD;

  int alignment = stack_slot_alignment_for_size(size);
  int aligned_size = align_up(size, alignment);

  cur_scope->x_offset = align_up(cur_scope->x_offset, alignment);
  cur_scope->x_offset += aligned_size;

  /* Locals are placed below the shadow space and must not overlap temp slots.
   * After prologue: RSP = RBP - 8, then we subtract frame_size.
   * Shadow space is at [RSP .. RSP+31] = [RBP-8-frame_size ..
   * RBP-8-frame_size+31]. Locals should be at offsets more negative than
   * RBP-32. */
  offset = cur_scope->z_offset + cur_scope->x_offset + cur_scope->t_offset;

  new_node = init_stack_node(offset, label, aligned_size);
  new_node->element_size = size;

  stackscope_append_x(cur_scope, CreateListNode(new_node, LIST_UNSPECIFIED));

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: Added %s to x_offset %d\n", new_node->label,
                new_node->offset);
#endif

  return new_node;
}

void remove_last_l_x(char *label) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);

  StackScope_t *cur_scope = global_stackmng->cur_scope;

  /* Locate the latest matching listnode via the hash index. */
  ListNode_t *last_match =
      scope_hash_get_last_listnode(cur_scope->x_index, label);
  if (last_match == NULL)
    return;

  /* Find the predecessor in the linked list. We only need pointer
   * comparison, not strcmp, so this is fast even for long lists. */
  ListNode_t *prev_li = NULL;
  ListNode_t *cur_li = cur_scope->x;
  while (cur_li != NULL && cur_li != last_match) {
    prev_li = cur_li;
    cur_li = cur_li->next;
  }
  assert(cur_li == last_match);

  /* Unlink from linked list */
  if (prev_li != NULL)
    prev_li->next = last_match->next;
  else
    cur_scope->x = last_match->next;

  if (cur_scope->x == NULL)
    cur_scope->x_tail = NULL;
  else if (cur_scope->x_tail == last_match)
    cur_scope->x_tail = prev_li;

  /* Drop the index entry for this listnode before freeing it. */
  scope_hash_remove_last(cur_scope->x_index, label);

  /* Free the stack node and list cell */
  StackNode_t *node = (StackNode_t *)last_match->cur;
  if (node != NULL) {
    if (node->label != NULL)
      free(node->label);
    free(node);
  }
  free(last_match);
}

StackNode_t *add_array(char *label, int total_size, int element_size,
                       int lower_bound) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);

  StackScope_t *cur_scope = global_stackmng->cur_scope;

  cur_scope->x_offset += total_size;

  /* Locals are placed below the shadow space and must not overlap temp slots.
   */
  int offset = cur_scope->z_offset + cur_scope->x_offset + cur_scope->t_offset;

  StackNode_t *new_node = init_stack_node(offset, label, total_size);
  new_node->is_array = 1;
  new_node->array_lower_bound = lower_bound;
  new_node->element_size = element_size;
  new_node->is_dynamic = 0;

  stackscope_append_x(cur_scope, CreateListNode(new_node, LIST_UNSPECIFIED));

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: Added array %s to x_offset %d\n", new_node->label,
                new_node->offset);
#endif

  return new_node;
}

StackNode_t *add_dynamic_array(char *label, int element_size, int lower_bound,
                               int use_static_storage,
                               const char *static_label) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);

  StackScope_t *cur_scope = global_stackmng->cur_scope;

  int descriptor_size = 4 * DOUBLEWORD;
  if (descriptor_size < 2 * element_size)
    descriptor_size = 2 * element_size;

  if (!use_static_storage)
    cur_scope->x_offset += descriptor_size;

  /* Locals are placed below the shadow space and must not overlap temp slots.
   */
  int offset = cur_scope->z_offset + cur_scope->x_offset + cur_scope->t_offset;
  if (use_static_storage)
    offset = 0;

  StackNode_t *new_node = init_stack_node(offset, label, descriptor_size);
  new_node->is_array = 1;
  new_node->array_lower_bound = lower_bound;
  new_node->element_size = element_size;
  new_node->is_dynamic = 1;
  if (use_static_storage) {
    new_node->is_static = 1;
    if (static_label != NULL)
      new_node->static_label = strdup(static_label);
  }

  stackscope_append_x(cur_scope, CreateListNode(new_node, LIST_UNSPECIFIED));

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: Added dynamic array %s descriptor to x_offset %d\n",
                new_node->label, new_node->offset);
#endif

  return new_node;
}

StackNode_t *add_static_var(char *label, int size, const char *static_label) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);

  StackScope_t *cur_scope = global_stackmng->cur_scope;
  StackNode_t *new_node =
      init_stack_node(0, label, size > 0 ? size : DOUBLEWORD);
  new_node->is_static = 1;
  if (static_label != NULL)
    new_node->static_label = strdup(static_label);

  ListNode_t *list_node = CreateListNode(new_node, LIST_UNSPECIFIED);
  stackscope_append_x(cur_scope, list_node);
  return new_node;
}

/* Adds doubleword to z */
StackNode_t *add_l_z(char *label) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);

  StackScope_t *cur_scope;
  StackNode_t *new_node;
  int offset;

  cur_scope = global_stackmng->cur_scope;

  cur_scope->z_offset += DOUBLEWORD;

  /* Arguments are placed below the shadow space */
  /* After prologue: RSP = RBP - 8, then we subtract frame_size */
  /* Shadow space is at [RSP .. RSP+31] = [RBP-8-frame_size ..
   * RBP-8-frame_size+31] */
  /* Arguments should be at offsets more negative than RBP-32 */
  offset = cur_scope->z_offset;

  new_node = init_stack_node(offset, label, DOUBLEWORD);

  stackscope_append_z(cur_scope, CreateListNode(new_node, LIST_UNSPECIFIED));

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: Added %s to z_offset %d\n", label, offset);
#endif

  return new_node;
}

StackNode_t *add_l_z_bytes(char *label, int size) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);

  StackScope_t *cur_scope = global_stackmng->cur_scope;
  if (size <= 0)
    size = DOUBLEWORD;

  int alignment = stack_slot_alignment_for_size(size);
  int aligned_size = align_up(size, alignment);
  cur_scope->z_offset = align_up(cur_scope->z_offset, alignment);
  cur_scope->z_offset += aligned_size;

  int offset = cur_scope->z_offset;
  StackNode_t *new_node = init_stack_node(offset, label, aligned_size);
  new_node->element_size = size;

  stackscope_append_z(cur_scope, CreateListNode(new_node, LIST_UNSPECIFIED));

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: Added %s to z_offset %d (bytes=%d)\n", label, offset,
                aligned_size);
#endif

  return new_node;
}

/* Adds quadword (8 bytes) to z */
StackNode_t *add_q_z(char *label) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);

  StackScope_t *cur_scope;
  StackNode_t *new_node;
  int offset;

  cur_scope = global_stackmng->cur_scope;

  cur_scope->z_offset += 8; // quadword size

  /* Arguments are placed below the shadow space */
  /* After prologue: RSP = RBP - 8, then we subtract frame_size */
  /* Shadow space is at [RSP .. RSP+31] = [RBP-8-frame_size ..
   * RBP-8-frame_size+31] */
  /* Arguments should be at offsets more negative than RBP-32 */
  offset = cur_scope->z_offset;

  new_node = init_stack_node(offset, label, 8);

  stackscope_append_z(cur_scope, CreateListNode(new_node, LIST_UNSPECIFIED));

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: Added %s to z_offset %d (quadword)\n", label, offset);
#endif

  return new_node;
}

/* Reserved z_offset value for temporary reservation */
static int g_reserved_z_offset = 0;

/* Reserves z_offset space to prevent overlap between x pool (temp slots) and z
 * pool (final storage). This should be called before allocating temp slots so
 * they are placed after the z pool region. */
void reserve_z_offset(int size) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);

  g_reserved_z_offset = global_stackmng->cur_scope->z_offset;
  global_stackmng->cur_scope->z_offset = size;

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: Reserved z_offset to %d (was %d)\n", size,
                g_reserved_z_offset);
#endif
}

/* Unreserves z_offset by restoring the original value.
 * This should be called after allocating temp slots so z allocations start from
 * the beginning. */
void unreserve_z_offset(void) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: Unreserved z_offset from %d to %d\n",
                global_stackmng->cur_scope->z_offset, g_reserved_z_offset);
#endif

  global_stackmng->cur_scope->z_offset = g_reserved_z_offset;
  g_reserved_z_offset = 0;
}

RegStack_t *get_reg_stack() {
  assert(global_stackmng != NULL);
  assert(global_stackmng->reg_stack != NULL);

  return global_stackmng->reg_stack;
}

/* Reset register allocator to a fresh state (used to recover from codegen
 * errors that leak registers). */
void reset_reg_stack(void) {
  assert(global_stackmng != NULL);
  if (global_stackmng->reg_stack != NULL)
    free_reg_stack(global_stackmng->reg_stack);
  global_stackmng->reg_stack = init_reg_stack();
}

/* Finds variable on temp in current stack */
/* Returns NULL if not found */
StackNode_t *find_in_temp(char *label) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);

  StackScope_t *cur_scope;
  StackNode_t *cur_node;

  cur_scope = global_stackmng->cur_scope;

  cur_node = stackscope_find_t(cur_scope, label);
  return cur_node;
}

StackNode_t *find_label(const char *label) {
  int depth = 0;
  return find_label_with_depth(label, &depth);
}

/* Returns static-link depth (0 = current frame, 1 = parent frame, etc.) */
StackNode_t *find_label_with_depth(const char *label, int *depth) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);
  assert(depth != NULL);

  StackScope_t *cur_scope;
  StackNode_t *cur_node;
  int static_link_depth = 0;

  cur_scope = global_stackmng->cur_scope;

  while (cur_scope != NULL) {
    cur_node = stackscope_find_z(cur_scope, label);
    if (cur_node != NULL) {
      *depth = static_link_depth;
      return cur_node;
    }

    cur_node = stackscope_find_x(cur_scope, label);
    if (cur_node != NULL) {
      *depth = static_link_depth;
      return cur_node;
    }

    cur_node = stackscope_find_t(cur_scope, label);
    if (cur_node != NULL) {
      *depth = static_link_depth;
      return cur_node;
    }

    /* Crossing a scope that owns a static-link slot means moving to the
     * next lexical frame in the parent chain. */
    if (stackscope_find_x(cur_scope, "__static_link__") != NULL)
      static_link_depth++;

    cur_scope = cur_scope->prev_scope;
  }

  return NULL;
}

void free_stackmng() {
  assert(global_stackmng != NULL);

  free_all_stackscopes(global_stackmng->cur_scope);
  free_reg_stack(global_stackmng->reg_stack);
  free(global_stackmng);
  global_stackmng = NULL;
}

/********* RegStack_t **********/

static void apply_register_limit(RegStack_t *reg_stack) {
  if (reg_stack == NULL || reg_stack->registers_free == NULL)
    return;

  const char *env_limit = getenv("KGPC_FORCE_REGISTER_LIMIT");
  if (env_limit == NULL || env_limit[0] == '\0')
    return;

  char *endptr = NULL;
  long limit = strtol(env_limit, &endptr, 10);
  if (endptr == env_limit || limit <= 0)
    return;

  ListNode_t *cur = reg_stack->registers_free;
  ListNode_t *prev = NULL;
  int retained = 0;

  while (cur != NULL && retained < limit) {
    prev = cur;
    cur = cur->next;
    retained++;
  }

  if (cur == NULL) {
    reg_stack->num_registers = retained;
    return;
  }

  if (prev != NULL)
    prev->next = NULL;
  else
    reg_stack->registers_free = NULL;

  while (cur != NULL) {
    ListNode_t *next = cur->next;
    Register_t *reg = (Register_t *)cur->cur;
    if (reg != NULL) {
      free(reg->bit_64);
      free(reg->bit_32);
      free(reg);
    }
    free(cur);
    cur = next;
  }

  reg_stack->num_registers = retained;
}

RegStack_t *init_reg_stack() {
  /*
   * Initialize register pool for expression evaluation.
   *
   * Strategy: Reserve the MOST COMMONLY USED argument registers to minimize
   * conflicts with function calls, but keep enough registers available for
   * complex expressions.
   *
   * Windows x64: First 4 args in %rcx, %rdx, %r8, %r9
   *   - Reserve: %rcx, %rdx (most common - used for 1-2 arg functions)
   *   - Available: %rax, %rsi, %rdi, %r8, %r9, %r10, %r11 (7 registers)
   *
   * Linux x64 (SysV): First 6 args in %rdi, %rsi, %rdx, %rcx, %r8, %r9
   *   - Reserve: %rdi, %rsi, %rdx (most common - used for 1-3 arg functions)
   *   - Available: %rax, %rcx, %r8, %r9, %r10, %r11 (6 registers)
   *
   * This approach:
   * 1. Prevents conflicts for the most common case (1-3 argument functions)
   * 2. Keeps enough registers for complex expressions
   * 3. The conflict handling code in codegen_statement.c handles cases
   *    where we need r8/r9 for 3-4 argument functions
   */
  ListNode_t *registers = NULL;
  RegStack_t *reg_stack;
  reg_stack = (RegStack_t *)malloc(sizeof(RegStack_t));
  assert(reg_stack != NULL);

  /* %rax is caller-saved — not in the allocatable pool.
     It is used as return value and varargs indicator. */

  /* %r10, %r11 are caller-saved scratch registers.
     They are NOT in the allocatable pool — only used explicitly
     (VMT dispatch, division, etc.) and never live across calls. */

  /* %r8, %r9, %r10, %r11 are caller-saved — not added to the pool. */

  /* %rbx, %r12 - callee-saved, survive across function calls.
     Placed first in the list so get_free_reg prefers them for
     intermediate values that may live across call instructions. */
  Register_t *rbx = (Register_t *)malloc(sizeof(Register_t));
  assert(rbx != NULL);
  rbx->reg_id = REG_RBX;
  rbx->bit_64 = strdup("%rbx");
  rbx->bit_32 = strdup("%ebx");
  rbx->spill_location = NULL;
  rbx->last_use_seq = 0;
  rbx->spill_callback = NULL;
  rbx->spill_context = NULL;
  rbx->vreg_id = -1;
#if USE_GRAPH_COLORING_ALLOCATOR
  rbx->current_live_range = NULL;
#endif

  Register_t *r12 = (Register_t *)malloc(sizeof(Register_t));
  assert(r12 != NULL);
  r12->reg_id = REG_R12;
  r12->bit_64 = strdup("%r12");
  r12->bit_32 = strdup("%r12d");
  r12->spill_location = NULL;
  r12->last_use_seq = 0;
  r12->spill_callback = NULL;
  r12->spill_context = NULL;
  r12->vreg_id = -1;
#if USE_GRAPH_COLORING_ALLOCATOR
  r12->current_live_range = NULL;
#endif

  Register_t *r13 = (Register_t *)malloc(sizeof(Register_t));
  assert(r13 != NULL);
  r13->reg_id = REG_R13;
  r13->bit_64 = strdup("%r13");
  r13->bit_32 = strdup("%r13d");
  r13->spill_location = NULL;
  r13->last_use_seq = 0;
  r13->spill_callback = NULL;
  r13->spill_context = NULL;
  r13->vreg_id = -1;
#if USE_GRAPH_COLORING_ALLOCATOR
  r13->current_live_range = NULL;
#endif

  Register_t *r14 = (Register_t *)malloc(sizeof(Register_t));
  assert(r14 != NULL);
  r14->reg_id = REG_R14;
  r14->bit_64 = strdup("%r14");
  r14->bit_32 = strdup("%r14d");
  r14->spill_location = NULL;
  r14->last_use_seq = 0;
  r14->spill_callback = NULL;
  r14->spill_context = NULL;
  r14->vreg_id = -1;
#if USE_GRAPH_COLORING_ALLOCATOR
  r14->current_live_range = NULL;
#endif

  Register_t *r15 = (Register_t *)malloc(sizeof(Register_t));
  assert(r15 != NULL);
  r15->reg_id = REG_R15;
  r15->bit_64 = strdup("%r15");
  r15->bit_32 = strdup("%r15d");
  r15->spill_location = NULL;
  r15->last_use_seq = 0;
  r15->spill_callback = NULL;
  r15->spill_context = NULL;
  r15->vreg_id = -1;
#if USE_GRAPH_COLORING_ALLOCATOR
  r15->current_live_range = NULL;
#endif

  /* Build register list — only callee-saved registers are in the pool.
     These survive across function calls, so values are never lost.
     Caller-saved registers (%rax, %rcx, %rdx, %rsi, %rdi, %r8-%r11)
     are NOT in the pool — they are clobbered by calls. */
  registers = CreateListNode(rbx, LIST_UNSPECIFIED);
  registers =
      PushListNodeBack(registers, CreateListNode(r12, LIST_UNSPECIFIED));
  registers =
      PushListNodeBack(registers, CreateListNode(r13, LIST_UNSPECIFIED));
  registers =
      PushListNodeBack(registers, CreateListNode(r14, LIST_UNSPECIFIED));
  registers =
      PushListNodeBack(registers, CreateListNode(r15, LIST_UNSPECIFIED));

  reg_stack->registers_allocated = NULL;
  reg_stack->registers_free = registers;
  reg_stack->num_registers = 5; /* rbx, r12, r13, r14, r15 */
  reg_stack->use_sequence = 0;

#if USE_GRAPH_COLORING_ALLOCATOR
  reg_stack->active_live_ranges = NULL;
  reg_stack->active_live_ranges_tail = NULL;
  reg_stack->next_live_range_id = 1;
#endif

  apply_register_limit(reg_stack);

  return reg_stack;
}

/* Returns 0 and assigns return_reg when reg_id is in this allocator's pool.
 * If the requested register is currently allocated, its value is spilled to a
 * temp slot before returning it. Caller-saved fixed registers such as rax/rdx
 * are intentionally outside this pool and return -1. */
int get_register_by_id(RegStack_t *regstack, RegisterId_t reg_id,
                       Register_t **return_reg, ListNode_t **inst_list) {
  assert(regstack != NULL);
  assert(return_reg != NULL);
  assert(inst_list != NULL);

  ListNode_t *cur_reg, *prev_reg;
  Register_t *reg;

  cur_reg = regstack->registers_free;
  prev_reg = NULL;
  while (cur_reg != NULL) {
    reg = (Register_t *)cur_reg->cur;
    if (reg->reg_id == reg_id) {
      if (prev_reg == NULL)
        regstack->registers_free = cur_reg->next;
      else
        prev_reg->next = cur_reg->next;

      cur_reg->next = regstack->registers_allocated;
      regstack->registers_allocated = cur_reg;
      *return_reg = reg;

      return 0;
    }

    prev_reg = cur_reg;
    cur_reg = cur_reg->next;
  }

  cur_reg = regstack->registers_allocated;
  while (cur_reg != NULL) {
    reg = (Register_t *)cur_reg->cur;
    if (reg->reg_id == reg_id) {
      char spill_label[64];
      snprintf(spill_label, sizeof(spill_label), "spill_%s", reg->bit_64 + 1);
      StackNode_t *spill_slot = find_in_temp(spill_label);
      if (spill_slot == NULL)
        spill_slot = add_l_t_bytes(spill_label, (int)sizeof(void *));
      if (spill_slot == NULL) {
        *return_reg = NULL;
        return -1;
      }
      reg->spill_location = spill_slot;

      char spill_code[128];
      snprintf(spill_code, sizeof(spill_code),
               "\t# Spill %s (by-id)\n\tmovq\t%s, -%d(%%rbp)\n", reg->bit_64,
               reg->bit_64, spill_slot->offset);
      *inst_list = add_inst(*inst_list, spill_code);

      if (reg->spill_callback != NULL)
        reg->spill_callback(reg, spill_slot, reg->spill_context);
      register_clear_spill_callback(reg);

      reg->last_use_seq = ++regstack->use_sequence;
      *return_reg = reg;
      return 0;
    }
    cur_reg = cur_reg->next;
  }

  *return_reg = NULL;
  return -1;
}

void free_reg(RegStack_t *reg_stack, Register_t *reg) {
  assert(reg_stack != NULL);
  assert(reg != NULL);

  ListNode_t *cur, *prev;

  cur = reg_stack->registers_allocated;
  prev = NULL;
  while (cur != NULL) {
    if (cur->cur == reg) {
      if (prev == NULL)
        reg_stack->registers_allocated = cur->next;
      else
        prev->next = cur->next;

      register_clear_spill_callback(reg);
      cur->next = reg_stack->registers_free;
      reg_stack->registers_free = cur;

#if USE_GRAPH_COLORING_ALLOCATOR
      /* End the live range for this register */
      if (reg->current_live_range != NULL) {
        reg->current_live_range->end_pos = reg_stack->use_sequence;
        reg->current_live_range = NULL;
      }
#endif

#if KGPC_ENABLE_REG_DEBUG
      if (reg->reg_id == REG_RCX && g_reg_debug_context != NULL)
        fprintf(stderr, "[reg-debug] free  %s (%s)\n", reg->bit_64,
                g_reg_debug_context);
#endif
      return;
    }
    prev = cur;
    cur = cur->next;
  }

  return;
}

void register_set_spill_callback(Register_t *reg,
                                 RegisterSpillCallback callback,
                                 void *context) {
  if (reg == NULL)
    return;
  reg->spill_callback = callback;
  reg->spill_context = context;
}

void register_clear_spill_callback(Register_t *reg) {
  if (reg == NULL)
    return;
  reg->spill_callback = NULL;
  reg->spill_context = NULL;
}

void swap_reg_stack(RegStack_t *reg_stack) {
  /* Need at least two registers to do a swap */
  assert(reg_stack != NULL);
  assert(reg_stack->registers_free != NULL);
  assert(reg_stack->registers_free->next != NULL);

  ListNode_t *top, *next;
  top = reg_stack->registers_free;
  next = top->next;

  top->next = next->next;
  next->next = top;
  reg_stack->registers_free = next;
}

Register_t *front_reg_stack(RegStack_t *reg_stack) {
  assert(reg_stack != NULL);

  return (Register_t *)reg_stack->registers_free->cur;
}

/* Register spilling: When out of registers, spill the least recently used one
 */
Register_t *get_free_reg(RegStack_t *reg_stack, ListNode_t **inst_list) {
  assert(reg_stack != NULL);
  assert(inst_list != NULL);

  ListNode_t *register_node;
  Register_t *reg;

  if (reg_stack->registers_free != NULL) {
    register_node = reg_stack->registers_free;
    reg_stack->registers_free = reg_stack->registers_free->next;

    register_node->next = reg_stack->registers_allocated;
    reg_stack->registers_allocated = register_node;

    reg = (Register_t *)register_node->cur;
    reg->last_use_seq = ++reg_stack->use_sequence;

    /* Clear spill location when register is freshly allocated */
    reg->spill_location = NULL;

#if USE_GRAPH_COLORING_ALLOCATOR
    /* Create a new live range for this register allocation */
    LiveRange_t *lr = create_live_range(reg_stack->next_live_range_id++,
                                        reg_stack->use_sequence, -1);
    if (lr != NULL) {
      lr->preferred_reg = reg;
      reg->current_live_range = lr;

      /* Add to active live ranges list (O(1) via tail pointer) */
      {
        ListNode_t *lr_node = CreateListNode(lr, LIST_UNSPECIFIED);
        if (reg_stack->active_live_ranges_tail != NULL)
          reg_stack->active_live_ranges_tail->next = lr_node;
        else
          reg_stack->active_live_ranges = lr_node;
        reg_stack->active_live_ranges_tail = lr_node;
      }
    }
#endif

#if KGPC_ENABLE_REG_DEBUG
    if (reg->reg_id == REG_RCX && g_reg_debug_context != NULL)
      fprintf(stderr, "[reg-debug] alloc %s (%s)\n", reg->bit_64,
              g_reg_debug_context);
#endif
    return reg;
  } else {
    /* No free registers - caller should use get_reg_with_spill */
    REG_DEBUG_LOG("[reg-spill] Out of registers - caller should handle this\n");
    return NULL;
  }
}

/* Force register allocation by spilling using graph coloring or LRU */
Register_t *get_reg_with_spill(RegStack_t *reg_stack, ListNode_t **inst_list) {
  assert(reg_stack != NULL);
  assert(inst_list != NULL);

  /* First try normal allocation */
  Register_t *reg = get_free_reg(reg_stack, inst_list);
  if (reg != NULL)
    return reg;

  /* Out of registers - must spill */
  Register_t *spill_reg = NULL;

#if USE_GRAPH_COLORING_ALLOCATOR
  /* ACTUAL GRAPH COLORING IMPLEMENTATION */

  /* Build interference graph from active live ranges */
  InterferenceGraph_t *graph =
      create_interference_graph(reg_stack->num_registers);
  if (graph != NULL && reg_stack->active_live_ranges != NULL) {
    /* Copy active live ranges to the graph */
    ListNode_t *lr_node = reg_stack->active_live_ranges;
    while (lr_node != NULL) {
      LiveRange_t *lr = (LiveRange_t *)lr_node->cur;
      if (lr != NULL && lr->end_pos == -1) /* Only active (not ended) ranges */
      {
        add_live_range(graph, lr);
      }
      lr_node = lr_node->next;
    }

    /* Run the graph coloring algorithm */
    ListNode_t *spilled_ranges = allocate_registers_graph_coloring(graph);

    /* Find which register to spill based on graph coloring result */
    if (spilled_ranges != NULL && spilled_ranges->cur != NULL) {
      LiveRange_t *spilled_lr = (LiveRange_t *)spilled_ranges->cur;
      spill_reg = spilled_lr->preferred_reg;
      REG_DEBUG_LOG("[reg-spill] Graph coloring selected %s for spilling\n",
                    spill_reg != NULL ? spill_reg->bit_64 : "NULL");

      DestroyList(spilled_ranges);
    }

    free_interference_graph(graph);
  }

  /* Fallback to LRU if graph coloring didn't select a register */
  if (spill_reg == NULL) {
    REG_DEBUG_LOG("[reg-spill] Graph coloring failed, falling back to LRU\n");
    ListNode_t *cur_node = reg_stack->registers_allocated;
    unsigned long long oldest_seq = ULLONG_MAX;

    while (cur_node != NULL) {
      Register_t *cur_reg = (Register_t *)cur_node->cur;
      if (cur_reg->last_use_seq < oldest_seq) {
        oldest_seq = cur_reg->last_use_seq;
        spill_reg = cur_reg;
      }
      cur_node = cur_node->next;
    }
  }

#else
  /* LRU-based spilling: Find least recently used register */
  ListNode_t *cur_node = reg_stack->registers_allocated;
  unsigned long long oldest_seq = ULLONG_MAX;

  while (cur_node != NULL) {
    Register_t *cur_reg = (Register_t *)cur_node->cur;
    if (cur_reg->last_use_seq < oldest_seq) {
      oldest_seq = cur_reg->last_use_seq;
      spill_reg = cur_reg;
    }
    cur_node = cur_node->next;
  }
#endif

  if (spill_reg == NULL) {
    REG_DEBUG_LOG("[reg-spill] ERROR: No registers available to spill\n");
    return NULL;
  }

  /* Allocate stack slot for this spill */
  char spill_label[64];
  snprintf(spill_label, sizeof(spill_label), "spill_%s", spill_reg->bit_64 + 1);
  StackNode_t *spill_slot = find_in_temp(spill_label);
  if (spill_slot == NULL)
    spill_slot = add_l_t_bytes(spill_label, (int)sizeof(void *));
  if (spill_slot == NULL) {
    REG_DEBUG_LOG("[reg-spill] ERROR: Failed to allocate spill slot\n");
    return NULL;
  }
  spill_reg->spill_location = spill_slot;

  /* Generate spill code */
  char spill_code[128];
#if USE_GRAPH_COLORING_ALLOCATOR
  snprintf(spill_code, sizeof(spill_code),
           "\t# Spill %s (graph-coloring)\n\tmovq\t%s, -%d(%%rbp)\n",
           spill_reg->bit_64, spill_reg->bit_64, spill_slot->offset);
#else
  snprintf(spill_code, sizeof(spill_code),
           "\t# Spill %s (LRU)\n\tmovq\t%s, -%d(%%rbp)\n", spill_reg->bit_64,
           spill_reg->bit_64, spill_slot->offset);
#endif
  *inst_list = add_inst(*inst_list, spill_code);

  REG_DEBUG_LOG("[reg-spill] Spilled %s (seq %llu) to offset -%d\n",
                spill_reg->bit_64, spill_reg->last_use_seq, spill_slot->offset);

  if (spill_reg->spill_callback != NULL)
    spill_reg->spill_callback(spill_reg, spill_slot, spill_reg->spill_context);
  register_clear_spill_callback(spill_reg);

  /* Update timestamp */
  spill_reg->last_use_seq = ++reg_stack->use_sequence;

  return spill_reg;
}

int get_num_registers_free(RegStack_t *reg_stack) {
  assert(reg_stack != NULL);
  return ListLength(reg_stack->registers_free);
}

int get_num_registers_alloced(RegStack_t *reg_stack) {
  assert(reg_stack != NULL);
  return ListLength(reg_stack->registers_allocated);
}

void regstack_caller_save(RegStack_t *reg_stack, ListNode_t **inst_list,
                          CallerSaveState *state) {
  assert(reg_stack != NULL);
  assert(inst_list != NULL);
  assert(state != NULL);

  memset(state, 0, sizeof(*state));

  ListNode_t *cur = reg_stack->registers_allocated;
  while (cur != NULL && state->count < MAX_SAVED_CALLER_REGS) {
    Register_t *reg = (Register_t *)cur->cur;
    if (reg != NULL && reg->bit_64 != NULL) {
      /* Allocate a temp slot for this register's value */
      char spill_label[64];
      snprintf(spill_label, sizeof(spill_label), "__caller_save_%s",
               reg->bit_64 + 1);
      StackNode_t *slot = find_in_temp(spill_label);
      if (slot == NULL)
        slot = add_l_t_bytes(spill_label, 8);
      if (slot != NULL) {
        state->entries[state->count].reg = reg;
        state->entries[state->count].spill_offset = slot->offset;

        char buf[128];
        snprintf(buf, sizeof(buf), "\tmovq\t%s, -%d(%%rbp)\n", reg->bit_64,
                 slot->offset);
        *inst_list = add_inst(*inst_list, buf);

        if (reg->reg_id == REG_RAX)
          state->rax_was_saved = 1;

        state->count++;
      }
    }
    cur = cur->next;
  }

  /* If %rax was among the saved registers, we need a slot to save
     the call's return value before restoring the old %rax. */
  if (state->rax_was_saved) {
    char ret_label[64];
    snprintf(ret_label, sizeof(ret_label), "__call_retval");
    StackNode_t *ret_slot = find_in_temp(ret_label);
    if (ret_slot == NULL)
      ret_slot = add_l_t_bytes(ret_label, 8);
    if (ret_slot != NULL)
      state->return_spill_offset = ret_slot->offset;
  }
}

void regstack_caller_restore(RegStack_t *reg_stack, ListNode_t **inst_list,
                             CallerSaveState *state) {
  assert(reg_stack != NULL);
  assert(inst_list != NULL);
  assert(state != NULL);

  if (state->count == 0)
    return;

  char buf[128];

  /* If %rax was saved, stash the return value first so restore doesn't lose it
   */
  if (state->rax_was_saved && state->return_spill_offset > 0) {
    snprintf(buf, sizeof(buf), "\tmovq\t%%rax, -%d(%%rbp)\n",
             state->return_spill_offset);
    *inst_list = add_inst(*inst_list, buf);
  }

  /* Restore all saved registers */
  for (int i = 0; i < state->count; i++) {
    Register_t *reg = state->entries[i].reg;
    int offset = state->entries[i].spill_offset;
    snprintf(buf, sizeof(buf), "\tmovq\t-%d(%%rbp), %s\n", offset, reg->bit_64);
    *inst_list = add_inst(*inst_list, buf);
  }

  /* Reload the return value into %rax (after restoring old %rax) */
  if (state->rax_was_saved && state->return_spill_offset > 0) {
    snprintf(buf, sizeof(buf), "\tmovq\t-%d(%%rbp), %%rax\n",
             state->return_spill_offset);
    *inst_list = add_inst(*inst_list, buf);
  }

  (void)reg_stack; /* suppress unused warning */
}

void free_reg_stack(RegStack_t *reg_stack) {
  assert(reg_stack != NULL);

  if (ListLength(reg_stack->registers_allocated) != 0) {
    fprintf(stderr, "WARNING: Not all registers freed, %d still remaining!\n",
            ListLength(reg_stack->registers_allocated));
  }

  ListNode_t *cur;
  Register_t *reg;
  while (reg_stack->registers_free != NULL) {
    cur = reg_stack->registers_free;
    reg_stack->registers_free = cur->next;

    reg = (Register_t *)cur->cur;

    free(reg->bit_64);
    free(reg->bit_32);
    free(reg);
    free(cur);
  }

  /* Just in case something was left allocated */
  while (reg_stack->registers_allocated != NULL) {
    cur = reg_stack->registers_allocated;
    reg_stack->registers_allocated = cur->next;

    reg = (Register_t *)cur->cur;

    free(reg->bit_64);
    free(reg->bit_32);
    free(reg);
    free(cur);
  }

  /* Free live ranges tracked by the graph coloring allocator */
#if USE_GRAPH_COLORING_ALLOCATOR
  while (reg_stack->active_live_ranges != NULL) {
    cur = reg_stack->active_live_ranges;
    reg_stack->active_live_ranges = cur->next;
    free_live_range((LiveRange_t *)cur->cur);
    free(cur);
  }
#endif

  free(reg_stack);
}

/*********** StackScope *************/

StackScope_t *init_stackscope() {
  StackScope_t *new_scope;
  new_scope = (StackScope_t *)malloc(sizeof(StackScope_t));

  new_scope->t_offset = 0;
  new_scope->x_offset = 0;
  new_scope->z_offset = 0;

  new_scope->t = NULL;
  new_scope->x = NULL;
  new_scope->z = NULL;
  new_scope->t_tail = NULL;
  new_scope->x_tail = NULL;
  new_scope->z_tail = NULL;

  /* Lazily allocated on first insert to avoid paying setup cost
   * for scopes that never see any locals (early init paths). */
  new_scope->t_index = NULL;
  new_scope->x_index = NULL;
  new_scope->z_index = NULL;

  new_scope->prev_scope = NULL;

  return new_scope;
}

StackNode_t *stackscope_find_t(StackScope_t *cur_scope, const char *label) {
  assert(cur_scope != NULL);
  assert(label != NULL);
  /* Indexed lookup; preserves "first-inserted match" semantics of
   * the prior linear scan because the hash chain is in insertion
   * order. Empty scope (no index yet) returns NULL via the helper. */
  return scope_hash_find_first(cur_scope->t_index, label);
}

StackNode_t *stackscope_find_x(StackScope_t *cur_scope, const char *label) {
  assert(cur_scope != NULL);
  assert(label != NULL);
  /* The x list uses alias-aware lookup: prefer first non-alias
   * match, otherwise return the first alias seen. */
  return scope_hash_find_x(cur_scope->x_index, label);
}

StackNode_t *stackscope_find_z(StackScope_t *cur_scope, const char *label) {
  assert(cur_scope != NULL);
  assert(label != NULL);
  return scope_hash_find_first(cur_scope->z_index, label);
}

/* Returns pointer to previous stack scope */
StackScope_t *free_stackscope(StackScope_t *stackscope) {
  StackScope_t *prev_scope;

  prev_scope = NULL;
  if (stackscope != NULL) {
    prev_scope = stackscope->prev_scope;

    free_stackscope_list(stackscope->t);
    free_stackscope_list(stackscope->x);
    free_stackscope_list(stackscope->z);
    scope_hash_destroy(stackscope->t_index);
    scope_hash_destroy(stackscope->x_index);
    scope_hash_destroy(stackscope->z_index);
    free(stackscope);
  }
  return prev_scope;
}

void free_all_stackscopes(StackScope_t *stackscope) {
  StackScope_t *prev_scope;

  while (stackscope != NULL) {
    prev_scope = stackscope->prev_scope;

    free_stackscope_list(stackscope->t);
    free_stackscope_list(stackscope->x);
    free_stackscope_list(stackscope->z);
    scope_hash_destroy(stackscope->t_index);
    scope_hash_destroy(stackscope->x_index);
    scope_hash_destroy(stackscope->z_index);
    free(stackscope);

    stackscope = prev_scope;
  }
}

void free_stackscope_list(ListNode_t *li) {
  ListNode_t *cur;
  cur = li;

  if (li == NULL)
    return;

  while (cur != NULL) {
    destroy_stack_node((StackNode_t *)cur->cur);
    cur = cur->next;
  }
  DestroyList(li);
}

/*********** StackNode *************/

/* WARNING: Copy is made of label */
StackNode_t *init_stack_node(int offset, char *label, int size) {
  assert(label != NULL);

  StackNode_t *new_node;
  new_node = (StackNode_t *)malloc(sizeof(StackNode_t));
  assert(new_node != NULL);

  new_node->offset = offset;
  new_node->label = strdup(label);
  new_node->size = size;
  new_node->is_array = 0;
  new_node->array_lower_bound = 0;
  new_node->element_size = size;
  new_node->is_dynamic = 0;
  new_node->is_static = 0;
  new_node->is_reference = 0;
  new_node->is_alias = 0;
  new_node->static_label = NULL;

  return new_node;
}

void destroy_stack_node(StackNode_t *node) {
  assert(node != NULL);
  assert(node->label != NULL);

  free(node->label);
  if (node->static_label != NULL)
    free(node->static_label);
  free(node);
}

StackNode_t *add_static_array(char *label, int total_size, int element_size,
                              int lower_bound, const char *static_label) {
  assert(global_stackmng != NULL);
  assert(global_stackmng->cur_scope != NULL);
  assert(label != NULL);

  StackScope_t *cur_scope = global_stackmng->cur_scope;
  StackNode_t *new_node = init_stack_node(0, label, total_size);
  new_node->is_array = 1;
  new_node->array_lower_bound = lower_bound;
  new_node->element_size = element_size;
  new_node->is_dynamic = 0;
  new_node->is_static = 1;
  if (static_label != NULL)
    new_node->static_label = strdup(static_label);

  ListNode_t *list_node = CreateListNode(new_node, LIST_UNSPECIFIED);
  stackscope_append_x(cur_scope, list_node);

  return new_node;
}
#include <string.h>
