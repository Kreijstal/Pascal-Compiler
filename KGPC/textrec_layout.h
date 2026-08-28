/**
 * @file textrec_layout.h
 * @brief C mirror of KGPC's `TextRec` (`KGPCTextRec`) for runtime
 * modules that cannot include `runtime_internal.h`.
 *
 * `runtime_internal.h` pulls in `Parser/SemanticCheck` dependencies, so
 * leaf runtime translation units (notably `runtime_fpc_assign.c`)
 * include this minimal struct instead.  The layout MUST stay in sync
 * with `KGPCTextRec` in `runtime_internal.h`; the `_Static_assert`
 * block at the bottom verifies field sizes and offsets at compile time.
 *
 * Any file that includes this header should ideally also include
 * `runtime_internal.h` and add cross-check asserts of the form
 * `sizeof(KgpcTextRecLayout) == sizeof(KGPCTextRec)` and
 * `offsetof(KgpcTextRecLayout, openfunc) == offsetof(KGPCTextRec, openfunc)`.
 *
 * Layout (x86_64 Linux, 640 bytes total): `handle` (int32) at 0,
 * `mode` (int32) at 4, `bufsize` / `private_data` / `bufpos` / `bufend`
 * (int64) at 8..40, `bufptr` at 40, the four codepointers `openfunc`,
 * `inoutfunc`, `flushfunc`, `closefunc` at 48/56/64/72, `userdata[32]`
 * at 80, `name[256]` at 112, `line_end[4]` at 368, `buffer[256]` at 372,
 * `codepage` (uint16) at 628, and `fullname` (`void` pointer) at 632.
 */

#ifndef KGPC_TEXTREC_LAYOUT_H
#define KGPC_TEXTREC_LAYOUT_H

#include <stddef.h>
#include <stdint.h>

/**
 * @brief Field-for-field mirror of `KGPCTextRec` from `runtime_internal.h`.
 *
 * Use `offsetof(KgpcTextRecLayout, <field>)` to get compile-time field
 * offsets without including the heavier full definition.
 */
#if defined(_WIN64) || defined(_WIN32) || defined(KGPC_WIN64_ABI)
/* Win64 TextRec: THandle = QWord widens Handle to 8 bytes and inserts
 * 4 bytes of padding before BufSize.  See KGPCTextRec in
 * runtime_internal.h and SemCheck_sizeof.c TEXT_TYPE. */
typedef struct KgpcTextRecLayout {
  int64_t handle;         /*   0: THandle (QWord on Win64) */
  int32_t mode;           /*   8: Mode (Longint) */
  uint32_t _pad_bufsize;  /*  12: padding so BufSize is 8-byte aligned */
  int64_t bufsize;        /*  16: SizeInt */
  int64_t private_data;   /*  24: SizeInt — KGPC stores FILE* here */
  int64_t bufpos;         /*  32: SizeInt */
  int64_t bufend;         /*  40: SizeInt */
  void *bufptr;           /*  48: ^TextBuf */
  void *openfunc;         /*  56: codepointer */
  void *inoutfunc;        /*  64: codepointer */
  void *flushfunc;        /*  72: codepointer */
  void *closefunc;        /*  80: codepointer */
  uint8_t userdata[32];   /*  88: UserData[1..32] */
  char name[256];         /* 120: name[0..255] */
  char line_end[4];       /* 376: TLineEndStr */
  char buffer[256];       /* 380: TextBuf */
  uint16_t codepage;      /* 636: TSystemCodePage */
  uint8_t _pad_fullname[2]; /* 638: padding to align fullname */
  void *fullname;         /* 640: FullName pointer */
} KgpcTextRecLayout;

_Static_assert(sizeof(KgpcTextRecLayout) == 648 || sizeof(void *) != 8,
               "KgpcTextRecLayout must be 648 bytes on Win64 to match KGPCTextRec");

_Static_assert(offsetof(KgpcTextRecLayout, handle) == 0, "handle offset");
_Static_assert(offsetof(KgpcTextRecLayout, mode) == 8, "mode offset");
_Static_assert(offsetof(KgpcTextRecLayout, bufsize) == 16, "bufsize offset");
_Static_assert(offsetof(KgpcTextRecLayout, private_data) == 24,
               "private_data offset");
_Static_assert(offsetof(KgpcTextRecLayout, bufpos) == 32, "bufpos offset");
_Static_assert(offsetof(KgpcTextRecLayout, bufend) == 40, "bufend offset");
_Static_assert(offsetof(KgpcTextRecLayout, bufptr) == 48, "bufptr offset");
_Static_assert(offsetof(KgpcTextRecLayout, openfunc) == 56, "openfunc offset");
_Static_assert(offsetof(KgpcTextRecLayout, inoutfunc) == 64,
               "inoutfunc offset");
_Static_assert(offsetof(KgpcTextRecLayout, flushfunc) == 72,
               "flushfunc offset");
_Static_assert(offsetof(KgpcTextRecLayout, closefunc) == 80,
               "closefunc offset");
_Static_assert(offsetof(KgpcTextRecLayout, userdata) == 88, "userdata offset");
_Static_assert(offsetof(KgpcTextRecLayout, name) == 120, "name offset");
_Static_assert(offsetof(KgpcTextRecLayout, line_end) == 376, "line_end offset");
_Static_assert(offsetof(KgpcTextRecLayout, buffer) == 380, "buffer offset");
_Static_assert(offsetof(KgpcTextRecLayout, codepage) == 636, "codepage offset");
_Static_assert(offsetof(KgpcTextRecLayout, fullname) == 640, "fullname offset");
#else
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
#if __SIZEOF_POINTER__ == 8
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
#endif /* __SIZEOF_POINTER__ == 8 */
#endif

#endif /* KGPC_TEXTREC_LAYOUT_H */
