/*
 * textrec_layout.h — C mirror of KGPC's TextRec (KGPCTextRec) for use in
 * runtime modules that cannot include runtime_internal.h (which pulls in
 * Parser/SemanticCheck dependencies).
 *
 * This struct MUST stay in sync with KGPCTextRec in runtime_internal.h.
 * Any file that includes this header should add:
 *
 *   #include "runtime_internal.h"
 *   _Static_assert(sizeof(KgpcTextRecLayout) == sizeof(KGPCTextRec),
 *       "KgpcTextRecLayout / KGPCTextRec size mismatch");
 *   _Static_assert(offsetof(KgpcTextRecLayout, openfunc) ==
 *                  offsetof(KGPCTextRec,       openfunc),
 *       "KgpcTextRecLayout openfunc offset mismatch");
 *
 * (runtime_fpc_assign.c cannot include runtime_internal.h, so the static
 * asserts there compare against known numeric constants instead.)
 *
 * Layout for x86_64 Linux (KGPCTextRec, 640 bytes):
 *   0:   handle       (int32_t)
 *   4:   mode         (int32_t)
 *   8:   bufsize      (int64_t)
 *  16:   private_data (int64_t)  — FILE* pointer stored by KGPC runtime
 *  24:   bufpos       (int64_t)
 *  32:   bufend       (int64_t)
 *  40:   bufptr       (void*)
 *  48:   openfunc     (void*)   — called by opentext_t_li_li
 *  56:   inoutfunc    (void*)   — called by close_t for output flush
 *  64:   flushfunc    (void*)
 *  72:   closefunc    (void*)   — called by close_t
 *  80:   userdata[32] (uint8_t)
 * 112:   name[256]    (char)    — AnsiChar file name
 * 368:   line_end[4]  (char)
 * 372:   buffer[256]  (char)
 * 628:   codepage     (uint16_t)
 * 630:   _pad_fullname[2] (uint8_t)
 * 632:   fullname     (void*)
 * 640: end
 */

#ifndef KGPC_TEXTREC_LAYOUT_H
#define KGPC_TEXTREC_LAYOUT_H

#include <stddef.h>
#include <stdint.h>

/*
 * KgpcTextRecLayout mirrors KGPCTextRec from runtime_internal.h
 * field-for-field. Use offsetof(KgpcTextRecLayout, <field>) to get compile-time
 * field offsets.
 */
typedef struct KgpcTextRecLayout {
  int32_t handle;       /*   0: THandle (Longint on x86_64 Linux) */
  int32_t mode;         /*   4: Mode    (Longint) */
  int64_t bufsize;      /*   8: SizeInt */
  int64_t private_data; /*  16: SizeInt — KGPC stores FILE* here */
  int64_t bufpos;       /*  24: SizeInt */
  int64_t bufend;       /*  32: SizeInt */
  void *bufptr;         /*  40: ^TextBuf */
  void *openfunc;       /*  48: codepointer */
  void *inoutfunc;      /*  56: codepointer */
  void *flushfunc;      /*  64: codepointer */
  void *closefunc;      /*  72: codepointer */
  uint8_t userdata[32]; /*  80: UserData[1..32] */
  char name[256];       /* 112: name[0..255] of AnsiChar (TFileTextRecChar) */
  char line_end[4];     /* 368: TLineEndStr (len byte + 3 chars) */
  char buffer[256];     /* 372: TextBuf */
  uint16_t codepage;    /* 628: TSystemCodePage */
  uint8_t _pad_fullname[2]; /* 630: padding to align fullname */
  void *fullname; /* 632: FullName pointer (FPC_HAS_FEATURE_UNICODESTRINGS) */
} KgpcTextRecLayout;

/* Compile-time verification that the struct has the expected size. */
_Static_assert(sizeof(KgpcTextRecLayout) == 640,
               "KgpcTextRecLayout must be 640 bytes to match KGPCTextRec");

/* Compile-time verification of key field offsets. */
_Static_assert(offsetof(KgpcTextRecLayout, handle) == 0, "handle offset");
_Static_assert(offsetof(KgpcTextRecLayout, mode) == 4, "mode offset");
_Static_assert(offsetof(KgpcTextRecLayout, bufsize) == 8, "bufsize offset");
_Static_assert(offsetof(KgpcTextRecLayout, private_data) == 16,
               "private_data offset");
_Static_assert(offsetof(KgpcTextRecLayout, bufpos) == 24, "bufpos offset");
_Static_assert(offsetof(KgpcTextRecLayout, bufend) == 32, "bufend offset");
_Static_assert(offsetof(KgpcTextRecLayout, bufptr) == 40, "bufptr offset");
_Static_assert(offsetof(KgpcTextRecLayout, openfunc) == 48, "openfunc offset");
_Static_assert(offsetof(KgpcTextRecLayout, inoutfunc) == 56,
               "inoutfunc offset");
_Static_assert(offsetof(KgpcTextRecLayout, flushfunc) == 64,
               "flushfunc offset");
_Static_assert(offsetof(KgpcTextRecLayout, closefunc) == 72,
               "closefunc offset");
_Static_assert(offsetof(KgpcTextRecLayout, userdata) == 80, "userdata offset");
_Static_assert(offsetof(KgpcTextRecLayout, name) == 112, "name offset");
_Static_assert(offsetof(KgpcTextRecLayout, line_end) == 368, "line_end offset");
_Static_assert(offsetof(KgpcTextRecLayout, buffer) == 372, "buffer offset");
_Static_assert(offsetof(KgpcTextRecLayout, codepage) == 628, "codepage offset");
_Static_assert(offsetof(KgpcTextRecLayout, fullname) == 632, "fullname offset");

#endif /* KGPC_TEXTREC_LAYOUT_H */
