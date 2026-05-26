/**
 * @file identifier_utils.h
 * @brief Case-insensitive Pascal-identifier helpers.
 *
 * Pascal is case-insensitive, so identifier comparisons and hash keys
 * need a fold to a canonical form.  This header is inline-only — each
 * helper is a `static inline` so it can be used freely without an
 * accompanying .c file.
 */
#ifndef KGPC_IDENTIFIER_UTILS_H
#define KGPC_IDENTIFIER_UTILS_H

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/**
 * @brief Allocate a lower-cased copy of @p src.
 *
 * Returns NULL on out-of-memory or if @p src is NULL.  Caller frees.
 */
static inline char *pascal_identifier_lower_dup(const char *src) {
  if (src == NULL)
    return NULL;

  size_t len = strlen(src);
  char *dst = (char *)malloc(len + 1);
  if (dst == NULL)
    return NULL;

  for (size_t i = 0; i < len; ++i)
    dst[i] = (char)tolower((unsigned char)src[i]);

  dst[len] = '\0';
  return dst;
}

/** @brief Default stack-buffer size for @ref pascal_identifier_lower_buf. */
#define PASCAL_ID_STACK_MAX 256

/**
 * @brief Lower an identifier into a caller-provided buffer.
 *
 * Returns @p buf on success, or a malloc'd string if the identifier is
 * too long for @p buf_size.  Caller must use
 * @ref pascal_identifier_lower_buf_free to release if the result was
 * heap-allocated.
 */
static inline char *pascal_identifier_lower_buf(const char *src, char *buf,
                                                size_t buf_size) {
  if (src == NULL)
    return NULL;

  size_t len = strlen(src);
  char *dst;
  if (len + 1 <= buf_size) {
    dst = buf;
  } else {
    dst = (char *)malloc(len + 1);
    if (dst == NULL)
      return NULL;
  }

  for (size_t i = 0; i < len; ++i)
    dst[i] = (char)tolower((unsigned char)src[i]);

  dst[len] = '\0';
  return dst;
}

/**
 * @brief Free a @ref pascal_identifier_lower_buf result if it was heap-allocated.
 *
 * No-op when @p result is NULL or aliases @p buf (the stack buffer).
 */
static inline void pascal_identifier_lower_buf_free(char *result, char *buf) {
  if (result != NULL && result != buf)
    free(result);
}

/**
 * @brief Case-insensitive identifier equality.
 *
 * Returns 1 if both strings compare equal under ASCII case-folding,
 * 0 otherwise.  Treats both arguments as identifiers (NULL only equals NULL).
 */
static inline int pascal_identifier_equals(const char *lhs, const char *rhs) {
  if (lhs == NULL || rhs == NULL)
    return lhs == rhs;

  while (*lhs != '\0' && *rhs != '\0') {
    unsigned char cl = (unsigned char)*lhs;
    unsigned char cr = (unsigned char)*rhs;
    if (tolower(cl) != tolower(cr))
      return 0;
    ++lhs;
    ++rhs;
  }

  return *lhs == '\0' && *rhs == '\0';
}

/**
 * @brief Case-insensitive `strstr`.
 *
 * Returns the first position in @p haystack where @p needle occurs
 * under ASCII case-folding, or NULL if not present.
 */
static inline const char *pascal_strcasestr(const char *haystack,
                                            const char *needle) {
  if (haystack == NULL || needle == NULL)
    return NULL;

  if (needle[0] == '\0')
    return haystack;

  for (; *haystack != '\0'; ++haystack) {
    if (tolower((unsigned char)*haystack) == tolower((unsigned char)*needle)) {
      /* Potential match, check the rest */
      const char *h = haystack;
      const char *n = needle;
      while (*n != '\0' && *h != '\0') {
        if (tolower((unsigned char)*h) != tolower((unsigned char)*n))
          break;
        ++h;
        ++n;
      }
      if (*n == '\0')
        return haystack;
    }
  }

  return NULL;
}

#endif /* KGPC_IDENTIFIER_UTILS_H */
