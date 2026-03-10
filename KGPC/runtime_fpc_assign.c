/*
 * runtime_fpc_assign.c — FPC RTL compatibility stubs for KGPC.
 *
 * Provides runtime symbols referenced by KGPC-compiled FPC RTL code that are
 * not emitted by the compiler or provided elsewhere in the runtime library.
 *
 * TextRec layout (as KGPC allocates: 632 bytes, AnsiChar name):
 *   0:  Handle (int32)
 *   4:  Mode   (int32)   — fmClosed=55216, fmInput=55217, fmOutput=55218, fmInOut=55219, fmAppend=55220
 *   8:  BufSize (int64)
 *  16:  _private / KGPC private_data (int64, stores FILE* pointer)
 *  24:  BufPos  (int64)
 *  32:  BufEnd  (int64)
 *  40:  BufPtr  (ptr)
 *  48:  KGPC openfunc (ptr) — not used by FPC RTL compiled code
 *  80:  UserData[32]
 * 112:  name[256] (AnsiChar)  — KGPC's name field
 *
 * FPC RTL compiled code accesses these offsets (empirically confirmed for the
 * current x86_64 Linux RTL build):
 * 320:  OpenFunc   (codepointer) — called by opentext_t_li_li
 * 328:  InOutFunc  (codepointer) — called by close_t for output flush
 * 336:  FlushFunc  (codepointer)
 * 344:  CloseFunc  (codepointer) — called by close_t
 *
 * FileRec layout (as KGPC allocates: 368 bytes):
 *   0:  Handle (int32)
 *   4:  Mode   (int32)
 *   8:  RecSize (int64)
 *  16:  private_data[64]
 *  80:  UserData[32]
 * 112:  name[256] (AnsiChar) — KGPC's name field
 *
 * FPC RTL compiled code accesses these FileRec offsets:
 *  52:  name[0..255] (AnsiChar) — read by do_open_u_pc_li_b
 * 568:  FullName (ptr, out of bounds, harmless BSS access)
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#ifndef _WIN32
#include <unistd.h>
#endif

/* ------------------------------------------------------------------ */
/* Forward declarations from KGPC runtime                              */
/* ------------------------------------------------------------------ */

/* Sets Pascal IOResult variable */
extern void kgpc_ioresult_set(int32_t value);
extern uint16_t InOutRes;

/* Initializes a TextRec to KGPC defaults (zeros + name copy) */
extern void kgpc_text_assign(void *file, const char *path);

/* Initializes a FileRec to KGPC defaults */
extern void kgpc_tfile_assign(void *file, const char *path);

/* ------------------------------------------------------------------ */
/* TextRec field offsets used by FPC RTL compiled code                 */
/* ------------------------------------------------------------------ */
#define TR_HANDLE       0
#define TR_MODE         4
#define TR_PRIV        16   /* KGPC private_data = FILE* */
#define TR_BUFPOS      24
#define TR_NAME       112   /* KGPC's AnsiChar name[] */
#define FPC_OPENFUNC  320
#define FPC_INOUTFUNC 328
#define FPC_FLUSHFUNC 336
#define FPC_CLOSEFUNC 344

#define KGPC_FM_CLOSED  0xD7B0   /* 55216 */
#define KGPC_FM_INPUT   0xD7B1   /* 55217 */
#define KGPC_FM_OUTPUT  0xD7B2   /* 55218 */
#define KGPC_FM_INOUT   0xD7B3   /* 55219 */
#define KGPC_FM_APPEND  0xD7B4   /* 55220 */

/* ------------------------------------------------------------------ */
/* Internal helpers for the TextRec I/O function table                 */
/* ------------------------------------------------------------------ */

static void kgpc_fpc_set_ioresult(int32_t value)
{
    kgpc_ioresult_set(value);
    InOutRes = (uint16_t)value;
}

/* No-op I/O stub used for InOutFunc and FlushFunc slots */
static void kgpc_fpc_noop_t(void *textrec)
{
    (void)textrec;
}

/* CloseFunc: fclose the FILE* stored in KGPC private_data, reset handle */
static void kgpc_fpc_closefunc(void *textrec)
{
    char *tr = (char *)textrec;
    int64_t priv;
    memcpy(&priv, tr + TR_PRIV, sizeof(priv));
    if (priv != 0) {
        FILE *stream = (FILE *)(uintptr_t)(uint64_t)priv;
        fclose(stream);
        priv = 0;
        memcpy(tr + TR_PRIV, &priv, sizeof(priv));
    }
    int32_t neg1 = -1;
    memcpy(tr + TR_HANDLE, &neg1, sizeof(neg1));
}

/* OpenFunc (set at FPC offset 296): opens the file named at TR_NAME,
 * stores FILE* in KGPC private_data, fd in Handle, sets I/O func table. */
static void kgpc_fpc_openfunc(void *textrec)
{
    char *tr = (char *)textrec;
    int32_t mode;
    memcpy(&mode, tr + TR_MODE, sizeof(mode));
    const char *name = tr + TR_NAME;

    FILE *stream = NULL;
    if (name[0] == '\0') {
        /* Empty filename = stdio */
        if (mode == (int32_t)KGPC_FM_INPUT)
            stream = stdin;
        else
            stream = stdout;
    } else {
        const char *fmode;
        if (mode == (int32_t)KGPC_FM_INPUT)
            fmode = "r";
        else if (mode == (int32_t)KGPC_FM_OUTPUT)
            fmode = "w";
        else if (mode == (int32_t)KGPC_FM_INOUT)
            fmode = "r+";
        else if (mode == (int32_t)KGPC_FM_APPEND)
            fmode = "a";
        else
            fmode = "w";
        stream = fopen(name, fmode);
    }

    if (stream != NULL) {
        int64_t priv = (int64_t)(uintptr_t)stream;
        memcpy(tr + TR_PRIV, &priv, sizeof(priv));
        int32_t fd = fileno(stream);
        memcpy(tr + TR_HANDLE, &fd, sizeof(fd));
        if (mode == (int32_t)KGPC_FM_APPEND) {
            int32_t output_mode = KGPC_FM_OUTPUT;
            memcpy(tr + TR_MODE, &output_mode, sizeof(output_mode));
        }
        kgpc_fpc_set_ioresult(0);
    } else {
        kgpc_fpc_set_ioresult(errno);
    }

    /* Set FPC I/O function table slots */
    void *noop  = (void *)kgpc_fpc_noop_t;
    void *close_fn = (void *)kgpc_fpc_closefunc;
    memcpy(tr + FPC_INOUTFUNC, &noop,     sizeof(noop));
    memcpy(tr + FPC_FLUSHFUNC, &noop,     sizeof(noop));
    memcpy(tr + FPC_CLOSEFUNC, &close_fn, sizeof(close_fn));
}

/* ------------------------------------------------------------------ */
/* assign_t_s: Assign(var t: Text; const s: string)                    */
/* Single implementation for both standard and FPC RTL mode.           */
/* Calls kgpc_text_assign (KGPC proper init), then sets FPC I/O        */
/* function pointers at offsets 296-320 (harmless in standard mode,    */
/* required for FPC RTL compiled code).                                 */
/* ------------------------------------------------------------------ */
void kgpc_assign_t_s(void *textrec, const char *path)
{
    if (textrec == NULL)
        return;

    kgpc_text_assign(textrec, path);

    /* Set FPC RTL function pointers at the offsets FPC RTL code expects */
    char *tr = (char *)textrec;
    void *open_fn  = (void *)kgpc_fpc_openfunc;
    void *noop     = (void *)kgpc_fpc_noop_t;
    void *close_fn = (void *)kgpc_fpc_closefunc;

    memcpy(tr + FPC_OPENFUNC,  &open_fn,  sizeof(open_fn));
    memcpy(tr + FPC_INOUTFUNC, &noop,     sizeof(noop));
    memcpy(tr + FPC_FLUSHFUNC, &noop,     sizeof(noop));
    memcpy(tr + FPC_CLOSEFUNC, &close_fn, sizeof(close_fn));
}

/* FPC RTL path — FPC system.pp mangles Assign(text,string) as assign_t_s. */
static void assign_t_s(void *textrec, const char *path) { kgpc_assign_t_s(textrec, path); }

/* ------------------------------------------------------------------ */
/* assign_t_c: Assign(var t: Text; c: AnsiChar)                        */
/* FPC mangles this as assign_t_c.  The char value arrives in %rsi as  */
/* an integer; we convert it to a 1-char string and delegate.          */
/* ------------------------------------------------------------------ */
static void assign_t_c(void *textrec, char c)
{
    char buf[2];
    buf[0] = c;
    buf[1] = '\0';
    kgpc_assign_t_s(textrec, buf);
}

/* ------------------------------------------------------------------ */
/* assign_f_s: Assign(var f: File; const s: string)                    */
/* Single implementation for both standard and FPC RTL mode.           */
/* Calls kgpc_tfile_assign (KGPC proper init), then copies filename    */
/* to FPC's expected offset 52 for FPC RTL compatibility.              */
/* ------------------------------------------------------------------ */

#define FR_NAME_FPC 52   /* AnsiChar name used by FPC RTL's do_open_u_pc_li_b */

void kgpc_assign_f_s(void *filerec, const char *path)
{
    if (filerec == NULL)
        return;

    kgpc_tfile_assign(filerec, path);

    /* Also copy filename to offset 52 where FPC RTL code reads it */
    if (path != NULL) {
        char *fr = (char *)filerec;
        size_t len = strlen(path);
        if (len > 255)
            len = 255;
        memcpy(fr + FR_NAME_FPC, path, len);
        fr[FR_NAME_FPC + len] = '\0';
    }
}

static void assign_f_s(void *filerec, const char *path) { kgpc_assign_f_s(filerec, path); }

/* RawByteString variants — kept as static stubs in case they're needed
 * internally.  The compiler now emits .globl (not .weak) for all unit
 * functions, so the compiler's own versions are used directly. */
static void assign_t_rbs(void *textrec, const char *path) { kgpc_assign_t_s(textrec, path); }
static void assign_f_rbs(void *filerec, const char *path) { kgpc_assign_f_s(filerec, path); }

/* ------------------------------------------------------------------ */
/* FileOpen / FileCreate — strong implementations for KGPC runtime.    */
/*                                                                     */
/* In KGPC, all strings are AnsiStrings (1-byte chars).  FPC RTL's     */
/* fileopen_us_i expects UnicodeString and converts via                */
/* widestringmanager, which garbles KGPC's AnsiString data.            */
/* These strong definitions bypass the conversion and call open()      */
/* directly with the AnsiString's char data.                           */
/* ------------------------------------------------------------------ */
#include <fcntl.h>
#include <sys/stat.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

typedef struct KgpcStringHeaderShim {
    uint16_t codepage;
    uint16_t elementsize;
    int32_t refcount;
    int64_t length;
} KgpcStringHeaderShim;

static const KgpcStringHeaderShim *kgpc_string_header_shim(const char *value)
{
    if (value == NULL)
        return NULL;
    return (const KgpcStringHeaderShim *)(value - (ptrdiff_t)sizeof(KgpcStringHeaderShim));
}

static char *kgpc_fpc_filename_to_ansi_dup(const char *filename)
{
    if (filename == NULL)
        return NULL;

    const KgpcStringHeaderShim *hdr = kgpc_string_header_shim(filename);
    if (hdr == NULL || hdr->elementsize != 2 || hdr->length <= 0)
        return strdup(filename);

    const uint16_t *src = (const uint16_t *)filename;
    size_t len = (size_t)hdr->length;
    char *ansi = (char *)malloc(len + 1);
    if (ansi == NULL)
        return NULL;

    for (size_t i = 0; i < len; ++i)
        ansi[i] = (src[i] < 256) ? (char)src[i] : '?';
    ansi[len] = '\0';
    return ansi;
}

/* FPC file mode constants (lower 2 bits) */
#define FPC_FM_OPENREAD      0
#define FPC_FM_OPENWRITE     1
#define FPC_FM_OPENREADWRITE 2

static int fpc_mode_to_posix_flags(int32_t mode)
{
    switch (mode & 3) {
        case FPC_FM_OPENREAD:      return O_RDONLY;
        case FPC_FM_OPENWRITE:     return O_WRONLY;
        case FPC_FM_OPENREADWRITE: return O_RDWR;
        default:                   return O_RDONLY;
    }
}

/* fileopen_us_i: FileOpen(const FileName: UnicodeString; Mode: Integer): THandle */
int32_t fileopen_us_i(const char *filename, int32_t mode)
{
    if (filename == NULL)
        return -1;
    char *ansi = kgpc_fpc_filename_to_ansi_dup(filename);
    if (ansi == NULL)
        return -1;
    int fd = open(ansi, fpc_mode_to_posix_flags(mode));
    free(ansi);
    return fd;
}

/* fileopen_rbs_i: Now provided by compiler-emitted FPC Pascal code.
 * Renamed to kgpc_ prefix to avoid duplicate symbol conflict. */
static int32_t kgpc_fileopen_rbs_i(const char *filename, int32_t mode)
{
    if (filename == NULL)
        return -1;
    return open(filename, fpc_mode_to_posix_flags(mode));
}

/* ------------------------------------------------------------------ */
/* FPC-mangled untyped file operations                                 */
/*                                                                     */
/* Strong overrides for FPC-compiled .weak file I/O functions.         */
/* The FPC-compiled versions have a codegen bug: each var→var pass     */
/* through do_read/do_write/fpread/fpwrite adds an extra level of      */
/* pointer indirection, causing read()/write() to operate on the       */
/* wrong buffer address.  These strong overrides delegate directly     */
/* to KGPC's C runtime which handles everything correctly.             */
/*                                                                     */
/* This object file is only pulled from the archive when FPC-specific  */
/* symbols are referenced (e.g. assign_t_s, assign_f_s).  In non-FPC  */
/* mode the compiler generates these symbols from system.p, and this   */
/* .o is never pulled in, avoiding multiple-definition errors.         */
/* Init functions (kgpc_fpc_init_os_params, kgpc_fpc_init_thread_mgr)  */
/* live in runtime_fpc_init.c (separate .o, always pulled in).         */
/* ------------------------------------------------------------------ */

/* Forward declarations from KGPC runtime (runtime.c) */
typedef struct KGPCFileRec KGPCFileRec;
extern void kgpc_tfile_rewrite(KGPCFileRec *file);
extern void kgpc_tfile_reset(KGPCFileRec *file);
extern void kgpc_tfile_close(KGPCFileRec *file);
extern int  kgpc_tfile_blockread(KGPCFileRec *file, void *buf, size_t count, long long *actual);
extern int  kgpc_tfile_blockwrite(KGPCFileRec *file, const void *buf, size_t count, long long *actual);
extern int  kgpc_tfile_seek(KGPCFileRec *file, long long index);
extern int  kgpc_tfile_filepos(KGPCFileRec *file, long long *pos);
extern int  kgpc_tfile_truncate_current(KGPCFileRec *file);

static void rewrite_f(void *filerec)
{
    kgpc_tfile_rewrite((KGPCFileRec *)filerec);
}

static void rewrite_f_li(void *filerec, int32_t recsize)
{
    KGPCFileRec *f = (KGPCFileRec *)filerec;
    kgpc_tfile_rewrite(f);
    if (recsize > 0) {
        /* Set RecSize at offset 8 in FileRec */
        int64_t rs = recsize;
        memcpy((char *)filerec + 8, &rs, sizeof(rs));
    }
}

static void reset_f(void *filerec)
{
    kgpc_tfile_reset((KGPCFileRec *)filerec);
}

static void reset_f_li(void *filerec, int32_t recsize)
{
    KGPCFileRec *f = (KGPCFileRec *)filerec;
    kgpc_tfile_reset(f);
    if (recsize > 0) {
        int64_t rs = recsize;
        memcpy((char *)filerec + 8, &rs, sizeof(rs));
    }
}

static void close_f(void *filerec)
{
    kgpc_tfile_close((KGPCFileRec *)filerec);
}

static void blockread_f_u_li_li(void *filerec, void *buf, int32_t count, int32_t *result)
{
    long long actual = 0;
    kgpc_tfile_blockread((KGPCFileRec *)filerec, buf, (size_t)count, &actual);
    if (result != NULL)
        *result = (int32_t)actual;
}

static void blockread_f_u_i64_i64(void *filerec, void *buf, int64_t count, int64_t *result)
{
    long long actual = 0;
    kgpc_tfile_blockread((KGPCFileRec *)filerec, buf, (size_t)count, &actual);
    if (result != NULL)
        *result = actual;
}

static void blockwrite_f_u_li_li(void *filerec, const void *buf, int32_t count, int32_t *result)
{
    long long actual = 0;
    kgpc_tfile_blockwrite((KGPCFileRec *)filerec, buf, (size_t)count, &actual);
    if (result != NULL)
        *result = (int32_t)actual;
}

static void blockwrite_f_u_i64_i64(void *filerec, const void *buf, int64_t count, int64_t *result)
{
    long long actual = 0;
    kgpc_tfile_blockwrite((KGPCFileRec *)filerec, buf, (size_t)count, &actual);
    if (result != NULL)
        *result = actual;
}

static void seek_f_i64(void *filerec, int64_t pos)
{
    kgpc_tfile_seek((KGPCFileRec *)filerec, pos);
}

static int64_t filepos_f(void *filerec)
{
    long long pos = 0;
    kgpc_tfile_filepos((KGPCFileRec *)filerec, &pos);
    return (int64_t)pos;
}

static void truncate_f(void *filerec)
{
    kgpc_tfile_truncate_current((KGPCFileRec *)filerec);
}

/* ------------------------------------------------------------------ */
/* Format function override                                            */
/*                                                                     */
/* FPC-compiled Format has a codegen bug: variant record field access   */
/* for vtChar dereferences the char value (e.g. 0x41) as a pointer.    */
/* Override with KGPC's C Format which handles TVarRec correctly.       */
/* ------------------------------------------------------------------ */
