#ifndef COMMON_XMEM_H
#define COMMON_XMEM_H

/* Checked malloc/calloc/realloc/strdup wrappers: print an OOM diagnostic
 * and exit(EXIT_FAILURE) instead of returning NULL. Use these from sites
 * where every existing caller assumed success — KGPC has no meaningful
 * way to recover from heap exhaustion mid-compile. */

#include <stddef.h>

void *kgpc_xmalloc(size_t size);
void *kgpc_xcalloc(size_t nmemb, size_t size);
void *kgpc_xrealloc(void *ptr, size_t size);
char *kgpc_xstrdup(const char *s);

#endif /* COMMON_XMEM_H */
