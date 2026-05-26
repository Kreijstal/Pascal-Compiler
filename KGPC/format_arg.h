/**
 * @file format_arg.h
 * @brief Tagged-variant runtime representation passed to `Format` /
 * `Write*` etc.
 *
 * Mirrors FPC's `TVarRec` layout: each argument carries an integer
 * @c kind discriminator plus a payload union.  KGPC's runtime
 * `Format` family reads this layout when destructuring variadic
 * `array of const` arguments.
 */
#ifndef KGPC_FORMAT_ARG_H
#define KGPC_FORMAT_ARG_H

#include <stdint.h>

/**
 * @brief Discriminator for @ref kgpc_tvarrec.
 *
 * Values match FPC's `vtInteger`, `vtBoolean`, ... constants so the
 * same byte layout works for both compilers.
 */
typedef enum kgpc_tvar_kind {
  KGPC_TVAR_KIND_INT = 0,
  KGPC_TVAR_KIND_BOOL = 1,
  KGPC_TVAR_KIND_CHAR = 2,
  KGPC_TVAR_KIND_REAL = 3,
  KGPC_TVAR_KIND_STRING = 4,      /**< `vtString`: pointer to ShortString. */
  KGPC_TVAR_KIND_POINTER = 5,
  KGPC_TVAR_KIND_PCHAR = 6,       /**< `vtPChar`: pointer to NUL-terminated. */
  KGPC_TVAR_KIND_ANSISTRING = 11  /**< `vtAnsiString`: managed AnsiString. */
} kgpc_tvar_kind_t;

/**
 * @brief One element of an `array of const` argument.
 *
 * @c kind selects which member of @c data is live.  Lifetimes follow
 * the standard FPC rules: managed-string kinds keep a reference that
 * the runtime releases once the call returns.
 */
typedef struct kgpc_tvarrec {
  int32_t kind;       /**< @ref kgpc_tvar_kind_t value. */
  int32_t reserved;   /**< Padding for 8-byte alignment of @c data. */
  union {
    int64_t v_int;    /**< Active when @c kind is INT / BOOL / CHAR. */
    double v_real;    /**< Active when @c kind is REAL. */
    void *v_ptr;      /**< Active for STRING / POINTER / PCHAR / ANSISTRING. */
  } data;
} kgpc_tvarrec;

#endif /* KGPC_FORMAT_ARG_H */
