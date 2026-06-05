/**
 * @file runtime_strhdr.h
 * @brief Adaptive AnsiString/UnicodeString header geometry + accessors.
 *
 * Dependency-free (only stdint/stddef/string) so it can be included by the
 * runtime translation units that cannot pull in the full runtime_internal.h
 * (which depends on the Parser/SemanticCheck headers) — notably
 * runtime_fpc_assign.c.  runtime_internal.h includes this header too.
 *
 * Compiled FPC RTL code accesses the managed-string header directly via
 * `PAnsiRec(Pointer(S) - AnsiFirstOff)^`, so the runtime's on-heap header
 * MUST byte-match the RTL's `TAnsiRec` — and that record's layout differs
 * between FPC versions:
 *
 *   FPC trunk 3.3.1 (CPU64):     CodePage:Word; ElementSize:Word;
 *                                Ref:Longint;   Len:SizeInt   -> 16 bytes,
 *                                Ref(4) @ data-12, Len(8) @ data-8.
 *   FPC 3.2.2 release (VER3_2):  CodePage:Word; ElementSize:Word;
 *                                Dummy:DWord;   Ref:SizeInt;  Len:SizeInt
 *                                -> 24 bytes, Ref(8) @ data-16, Len(8) @ data-8.
 *
 * Rather than hardcode either size (which serves only one FPC version) or
 * branch on a version macro, KGPC reads `SizeOf(TAnsiRec)` and the `Ref`
 * field's offset/width from the RTL it is compiling and emits the three
 * globals below as program data; the runtime reads them so a single runtime
 * binary serves both layouts.  Invariants used directly by the accessors:
 *   - Len is a 64-bit field at data-8 in every layout.
 *   - CodePage(Word) @ data-H, ElementSize(Word) @ data-H+2 (H = header size).
 * The KGPC-emitted definitions live in `codegen_main` (one per program); see
 * `KGPC/CodeGenerator/Intel_x86-64/codegen.c`.
 */
#ifndef KGPC_RUNTIME_STRHDR_H
#define KGPC_RUNTIME_STRHDR_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

extern int64_t kgpc_ansistr_header_size; /**< H: header bytes; data = base + H */
extern int64_t kgpc_ansistr_ref_offset;  /**< Ref offset from data (negative) */
extern int64_t kgpc_ansistr_ref_width;   /**< Ref field width in bytes: 4 or 8 */

/** @brief Non-zero iff VALUE is a runtime-managed string (accessors valid). */
int kgpc_string_is_managed(const char *value);

/** @brief Header byte count (alloc = this + length + 1). */
static inline size_t kgpc_strhdr_size(void) {
  return (size_t)kgpc_ansistr_header_size;
}
/** @brief Allocation base pointer for a managed-string data pointer. */
static inline char *kgpc_strhdr_base(const char *data) {
  return (char *)data - kgpc_ansistr_header_size;
}
static inline int64_t kgpc_strhdr_get_length(const char *data) {
  int64_t v;
  memcpy(&v, data - 8, sizeof(v));
  return v;
}
static inline void kgpc_strhdr_set_length(char *data, int64_t n) {
  memcpy(data - 8, &n, sizeof(n));
}
static inline int64_t kgpc_strhdr_get_refcount(const char *data) {
  if (kgpc_ansistr_ref_width == 4) {
    int32_t v;
    memcpy(&v, data + kgpc_ansistr_ref_offset, sizeof(v));
    return v;
  }
  int64_t v;
  memcpy(&v, data + kgpc_ansistr_ref_offset, sizeof(v));
  return v;
}
static inline void kgpc_strhdr_set_refcount(char *data, int64_t r) {
  if (kgpc_ansistr_ref_width == 4) {
    int32_t v = (int32_t)r;
    memcpy(data + kgpc_ansistr_ref_offset, &v, sizeof(v));
  } else {
    memcpy(data + kgpc_ansistr_ref_offset, &r, sizeof(r));
  }
}
static inline uint16_t kgpc_strhdr_get_codepage(const char *data) {
  uint16_t v;
  memcpy(&v, data - kgpc_ansistr_header_size, sizeof(v));
  return v;
}
static inline void kgpc_strhdr_set_codepage(char *data, uint16_t cp) {
  memcpy(data - kgpc_ansistr_header_size, &cp, sizeof(cp));
}
static inline uint16_t kgpc_strhdr_get_elementsize(const char *data) {
  uint16_t v;
  memcpy(&v, data - kgpc_ansistr_header_size + 2, sizeof(v));
  return v;
}
static inline void kgpc_strhdr_set_elementsize(char *data, uint16_t es) {
  memcpy(data - kgpc_ansistr_header_size + 2, &es, sizeof(es));
}

#ifdef __cplusplus
}
#endif

#endif /* KGPC_RUNTIME_STRHDR_H */
