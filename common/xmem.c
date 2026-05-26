#include "xmem.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void kgpc_xmem_die(const char *what, size_t bytes) {
  fprintf(stderr, "KGPC: out of memory in %s (requested %zu bytes)\n", what,
          bytes);
  exit(EXIT_FAILURE);
}

void *kgpc_xmalloc(size_t size) {
  void *p = malloc(size);
  if (p == NULL && size != 0)
    kgpc_xmem_die("kgpc_xmalloc", size);
  return p;
}

void *kgpc_xcalloc(size_t nmemb, size_t size) {
  void *p = calloc(nmemb, size);
  if (p == NULL && nmemb != 0 && size != 0)
    kgpc_xmem_die("kgpc_xcalloc", nmemb * size);
  return p;
}

void *kgpc_xrealloc(void *ptr, size_t size) {
  void *p = realloc(ptr, size);
  if (p == NULL && size != 0)
    kgpc_xmem_die("kgpc_xrealloc", size);
  return p;
}

char *kgpc_xstrdup(const char *s) {
  if (s == NULL)
    return NULL;
  size_t n = strlen(s) + 1;
  char *p = (char *)kgpc_xmalloc(n);
  memcpy(p, s, n);
  return p;
}
