/*
 * codegen_string_set.c — Simple string hash set for O(1) label/name lookups
 */
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "codegen_string_set.h"

static unsigned codegen_hash(const char *s) {
  unsigned h = 0;
  for (; *s; s++)
    h = h * 31 + (unsigned char)*s;
  return h % CODEGEN_HASHSET_SIZE;
}

int codegen_set_contains(const CodeGenStringSet *set, const char *key) {
  unsigned idx = codegen_hash(key);
  for (CodeGenHashEntry *e = set->buckets[idx]; e != NULL; e = e->next)
    if (strcmp(e->key, key) == 0)
      return 1;
  return 0;
}

int codegen_set_contains_ci(const CodeGenStringSet *set, const char *key) {
  unsigned h = 0;
  for (const char *s = key; *s; s++)
    h = h * 31 + (unsigned char)tolower((unsigned char)*s);
  unsigned idx = h % CODEGEN_HASHSET_SIZE;
  for (CodeGenHashEntry *e = set->buckets[idx]; e != NULL; e = e->next)
    if (strcasecmp(e->key, key) == 0)
      return 1;
  return 0;
}

void codegen_set_insert(CodeGenStringSet *set, const char *key) {
  unsigned idx = codegen_hash(key);
  CodeGenHashEntry *entry = malloc(sizeof(CodeGenHashEntry));
  entry->key = key;
  entry->next = set->buckets[idx];
  set->buckets[idx] = entry;
}

void codegen_set_insert_ci(CodeGenStringSet *set, const char *key) {
  unsigned h = 0;
  for (const char *s = key; *s; s++)
    h = h * 31 + (unsigned char)tolower((unsigned char)*s);
  unsigned idx = h % CODEGEN_HASHSET_SIZE;
  CodeGenHashEntry *entry = malloc(sizeof(CodeGenHashEntry));
  entry->key = key;
  entry->next = set->buckets[idx];
  set->buckets[idx] = entry;
}

void codegen_set_destroy(CodeGenStringSet *set) {
  for (int i = 0; i < CODEGEN_HASHSET_SIZE; i++) {
    CodeGenHashEntry *e = set->buckets[i];
    while (e != NULL) {
      CodeGenHashEntry *next = e->next;
      free(e);
      e = next;
    }
    set->buckets[i] = NULL;
  }
}
