#ifndef KGPC_COMMON_UTILS_H
#define KGPC_COMMON_UTILS_H

#include <stdlib.h>
#include <string.h>

/**
 * Common utility functions and macros to reduce code duplication
 * across the KGPC codebase.
 */

/* ===================================================================
 * Platform portability: POSIX string functions missing on Windows
 * =================================================================== */

#ifdef _WIN32
#ifndef strcasecmp
#define strcasecmp _stricmp
#endif
#ifndef strncasecmp
#define strncasecmp _strnicmp
#endif
static inline char* kgpc_strndup(const char* s, size_t n)
{
    size_t len = strnlen(s, n);
    char* buf = (char*)malloc(len + 1);
    if (buf == NULL)
        return NULL;
    memcpy(buf, s, len);
    buf[len] = '\0';
    return buf;
}
#define strndup kgpc_strndup
#endif /* _WIN32 */

/**
 * Guard clause helpers for NULL pointer validation.
 * These inline functions help reduce repetitive NULL checking patterns
 * by consolidating common guard clause logic.
 */

/**
 * Check if all 2 pointers are non-NULL.
 * @return 1 if all pointers are non-NULL, 0 otherwise
 */
static inline int ensure_non_null_2(const void *a, const void *b)
{
    return (a != NULL && b != NULL);
}

/**
 * Check if all 3 pointers are non-NULL.
 * @return 1 if all pointers are non-NULL, 0 otherwise
 */
static inline int ensure_non_null_3(const void *a, const void *b, const void *c)
{
    return (a != NULL && b != NULL && c != NULL);
}

/**
 * Check if all 4 pointers are non-NULL.
 * @return 1 if all pointers are non-NULL, 0 otherwise
 */
static inline int ensure_non_null_4(const void *a, const void *b, const void *c, const void *d)
{
    return (a != NULL && b != NULL && c != NULL && d != NULL);
}

/**
 * Safe memory cleanup macro that checks for NULL before freeing
 * and sets the pointer to NULL after freeing.
 *
 * Usage: SAFE_FREE(ptr);
 */
#define SAFE_FREE(ptr) do { \
    if ((ptr) != NULL) { \
        free(ptr); \
        (ptr) = NULL; \
    } \
} while(0)

/**
 * Safe memory cleanup with custom destructor.
 * Checks for NULL before calling destructor and sets pointer to NULL after.
 *
 * Usage: SAFE_FREE_WITH(ptr, custom_free_func);
 */
#define SAFE_FREE_WITH(ptr, destructor) do { \
    if ((ptr) != NULL) { \
        destructor(ptr); \
        (ptr) = NULL; \
    } \
} while(0)

#endif /* KGPC_COMMON_UTILS_H */
