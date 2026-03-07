/*
 * runtime_fpc_assign.c — FPC RTL compatibility stubs for KGPC.
 *
 * Provides runtime symbols referenced by KGPC-compiled FPC RTL code that are
 * not emitted by the compiler or provided elsewhere in the runtime library.
 *
 * TextRec layout (as KGPC allocates: 632 bytes, AnsiChar name):
 *   0:  Handle (int32)
 *   4:  Mode   (int32)   — fmClosed=55216, fmInput=55217, fmOutput=55218, fmInOut=55219
 *   8:  BufSize (int64)
 *  16:  _private / KGPC private_data (int64, stores FILE* pointer)
 *  24:  BufPos  (int64)
 *  32:  BufEnd  (int64)
 *  40:  BufPtr  (ptr)
 *  48:  KGPC openfunc (ptr) — not used by FPC RTL compiled code
 *  80:  UserData[32]
 * 112:  name[256] (AnsiChar)  — KGPC's name field
 *
 * FPC RTL compiled code accesses these offsets (empirically confirmed):
 * 296:  OpenFunc  (codepointer) — called by opentext_t_li_li
 * 304:  InOutFunc (codepointer) — called by close_t for output flush
 * 312:  FlushFunc (codepointer)
 * 320:  CloseFunc (codepointer) — called by close_t
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
#include <unistd.h>

/* ------------------------------------------------------------------ */
/* Forward declarations from KGPC runtime                              */
/* ------------------------------------------------------------------ */

/* Sets Pascal IOResult variable */
extern void kgpc_ioresult_set(int32_t value);

/* Initializes a TextRec to KGPC defaults (zeros + name copy) */
extern void kgpc_text_assign(void *file, const char *path);

/* ------------------------------------------------------------------ */
/* TextRec field offsets used by FPC RTL compiled code                 */
/* ------------------------------------------------------------------ */
#define TR_HANDLE       0
#define TR_MODE         4
#define TR_PRIV        16   /* KGPC private_data = FILE* */
#define TR_BUFPOS      24
#define TR_NAME       112   /* KGPC's AnsiChar name[] */
#define FPC_OPENFUNC  296
#define FPC_INOUTFUNC 304
#define FPC_FLUSHFUNC 312
#define FPC_CLOSEFUNC 320

#define KGPC_FM_CLOSED  0xD7B0   /* 55216 */
#define KGPC_FM_INPUT   0xD7B1   /* 55217 */
#define KGPC_FM_OUTPUT  0xD7B2   /* 55218 */
#define KGPC_FM_INOUT   0xD7B3   /* 55219 */

/* ------------------------------------------------------------------ */
/* Internal helpers for the TextRec I/O function table                 */
/* ------------------------------------------------------------------ */

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
        else
            fmode = "w";
        stream = fopen(name, fmode);
    }

    if (stream != NULL) {
        int64_t priv = (int64_t)(uintptr_t)stream;
        memcpy(tr + TR_PRIV, &priv, sizeof(priv));
        int32_t fd = fileno(stream);
        memcpy(tr + TR_HANDLE, &fd, sizeof(fd));
        kgpc_ioresult_set(0);
    } else {
        kgpc_ioresult_set(errno);
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
/* Called by KGPC-compiled code when Assign(TextFile, filename) is     */
/* compiled in FPC RTL mode (--no-stdlib).                             */
/* rdi = TextRec*, rsi = PChar filename                                */
/* ------------------------------------------------------------------ */
void assign_t_s(void *textrec, const char *path)
{
    if (textrec == NULL)
        return;

    /* Use KGPC's assign to initialize the TextRec (zeroes, copies name) */
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

/* ------------------------------------------------------------------ */
/* assign_f_s: Assign(var f: File; const s: string)                    */
/* FileRec layout (368 bytes, KGPC allocation).                        */
/* FPC RTL code reads name at offset 52 (AnsiChar).                    */
/* ------------------------------------------------------------------ */

#define FR_HANDLE   0
#define FR_MODE     4
#define FR_NAME_FPC 52   /* AnsiChar name used by do_open_u_pc_li_b */

void assign_f_s(void *filerec, const char *path)
{
    if (filerec == NULL)
        return;

    /* Zero the FileRec (368 bytes) */
    memset(filerec, 0, 368);

    char *fr = (char *)filerec;

    /* Handle = -1 (unassigned) */
    int32_t neg1 = -1;
    memcpy(fr + FR_HANDLE, &neg1, sizeof(neg1));

    /* Mode = fmClosed */
    int32_t mode = (int32_t)KGPC_FM_CLOSED;
    memcpy(fr + FR_MODE, &mode, sizeof(mode));

    /* Copy filename to offset 52 (used by FPC RTL's do_open_u_pc_li_b) */
    if (path != NULL) {
        size_t len = strlen(path);
        if (len > 255)
            len = 255;
        memcpy(fr + FR_NAME_FPC, path, len);
        fr[FR_NAME_FPC + len] = '\0';
    }
}

/* ------------------------------------------------------------------ */
/* MODE_OPEN: FPC constant S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH = 0666 octal = 438 */
/* Referenced as a global variable by FPC RTL compiled code.           */
/* ------------------------------------------------------------------ */
int32_t MODE_OPEN = 0x1B6;   /* 438 decimal, 0o666 octal */

/* ------------------------------------------------------------------ */
/* operatingsystem_parameter_* : FPC system unit global variables      */
/* Set by kgpc_fpc_init_os_params() called from kgpc_init_args.        */
/* ------------------------------------------------------------------ */
int32_t  operatingsystem_parameter_argc = 0;
void    *operatingsystem_parameter_argv = NULL;
void    *operatingsystem_parameter_envp = NULL;

/* Called from kgpc_init_args to populate FPC globals */
void kgpc_fpc_init_os_params(int argc, char **argv, char **envp)
{
    operatingsystem_parameter_argc = (int32_t)argc;
    operatingsystem_parameter_argv = (void *)argv;
    operatingsystem_parameter_envp = (void *)envp;
}

/* ------------------------------------------------------------------ */
/* FPC_THREADVARTABLES: Thread variable tables (TltvInitTablesTable)   */
/* count=0 means no thread variables — init/copy loops do nothing.     */
/* ------------------------------------------------------------------ */
struct {
    int64_t count;
    void   *tables[1];  /* placeholder, never accessed when count=0 */
} FPC_THREADVARTABLES = { 0, { NULL } };

/* ------------------------------------------------------------------ */
/* dest / Dest: operator result parameters leaked as global references */
/* These appear as dead code in .weak functions overridden by the      */
/* runtime's strong widechar__op_assign_olevariant_wc.  Providing      */
/* them as zero globals satisfies the linker.                          */
/* ------------------------------------------------------------------ */
void *dest = NULL;
void *Dest = NULL;
