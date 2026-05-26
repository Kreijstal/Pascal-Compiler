/**
 * @file string_intern.h
 * @brief Case-insensitive string interning pool.
 *
 * Deduplicates strings so identical values share one canonical pointer.
 * The dedup is case-insensitive but preserves the first-seen casing.
 * Interned strings must NOT be individually freed; call
 * @ref string_intern_reset to release everything.
 */
#ifndef KGPC_STRING_INTERN_H
#define KGPC_STRING_INTERN_H

/**
 * @brief Intern @p str and return the canonical pointer for its value.
 *
 * Returns NULL if @p str is NULL.  Subsequent calls with the same
 * string (case-insensitive) return the same pointer.  The returned
 * pointer is valid until @ref string_intern_reset is called.
 */
const char *string_intern(const char *str);

/** @brief Free all interned strings (call between compilations or at exit). */
void string_intern_reset(void);

#endif /* KGPC_STRING_INTERN_H */
