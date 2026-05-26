/*
 * codegen_string_set.h — Simple string hash set for O(1) label/name lookups
 */
#ifndef CODEGEN_STRING_SET_H
#define CODEGEN_STRING_SET_H

#define CODEGEN_HASHSET_SIZE 8191

typedef struct CodeGenHashEntry {
  const char *key;
  struct CodeGenHashEntry *next;
} CodeGenHashEntry;

typedef struct {
  CodeGenHashEntry *buckets[CODEGEN_HASHSET_SIZE];
} CodeGenStringSet;

int codegen_set_contains(const CodeGenStringSet *set, const char *key);
int codegen_set_contains_ci(const CodeGenStringSet *set, const char *key);
void codegen_set_insert(CodeGenStringSet *set, const char *key);
void codegen_set_insert_ci(CodeGenStringSet *set, const char *key);
void codegen_set_destroy(CodeGenStringSet *set);

#endif /* CODEGEN_STRING_SET_H */
