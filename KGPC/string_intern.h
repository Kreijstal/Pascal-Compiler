/*
    String interning pool - deduplicates strings so identical values share
    one canonical pointer. Case-insensitive dedup, preserves first-seen case.
    Interned strings must NOT be individually freed.
*/
#ifndef KGPC_STRING_INTERN_H
#define KGPC_STRING_INTERN_H

/* Intern a string: returns canonical pointer for this value.
 * Returns NULL if str is NULL. Subsequent calls with the same string
 * (case-insensitive) return the same pointer.
 * The returned pointer is valid until string_intern_reset() is called. */
const char *string_intern(const char *str);

/* Free all interned strings (call between compilations or at exit). */
void string_intern_reset(void);

#endif /* KGPC_STRING_INTERN_H */
