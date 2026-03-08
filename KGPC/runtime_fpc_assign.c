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

void assign_t_s(void *textrec, const char *path)
{
    kgpc_assign_t_s(textrec, path);
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

void assign_f_s(void *filerec, const char *path)
{
    kgpc_assign_f_s(filerec, path);
}

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

/* Helper: if the string has elementsize==2 (real UTF-16 UnicodeString),
 * convert it to a temporary char* by truncating each uint16_t to a byte.
 * Returns a malloc'd buffer that the caller must free, or NULL if no
 * conversion was needed (in which case use the original pointer). */
static char *unicode_filename_to_ansi(const char *filename)
{
    if (filename == NULL)
        return NULL;
    /* KgpcStringHeader is at offset -24 from the data pointer.
     * elementsize is at offset 10 within the header (offset -14 from data). */
    uint16_t elementsize = *(const uint16_t *)((const char *)filename - 14);
    if (elementsize != 2)
        return NULL;  /* already AnsiString, no conversion needed */
    /* It's UTF-16 data — get length from header (int64 at offset -8 from data) */
    int64_t len = *(const int64_t *)((const char *)filename - 8);
    if (len <= 0)
        return NULL;
    char *buf = (char *)malloc(len + 1);
    const uint16_t *src = (const uint16_t *)filename;
    for (int64_t i = 0; i < len; i++)
        buf[i] = (src[i] < 256) ? (char)src[i] : '?';
    buf[len] = '\0';
    return buf;
}

/* fileopen_us_i: FileOpen(const FileName: UnicodeString; Mode: Integer): THandle */
int32_t fileopen_us_i(const char *filename, int32_t mode)
{
    if (filename == NULL)
        return -1;
    char *ansi = unicode_filename_to_ansi(filename);
    int32_t result = open(ansi ? ansi : filename, fpc_mode_to_posix_flags(mode));
    free(ansi);
    return result;
}

/* fileopen_rbs_i: FileOpen(const FileName: RawByteString; Mode: Integer): THandle */
int32_t fileopen_rbs_i(const char *filename, int32_t mode)
{
    if (filename == NULL)
        return -1;
    return open(filename, fpc_mode_to_posix_flags(mode));
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

/* ------------------------------------------------------------------ */
/* FPC TThreadManager implementation (single-threaded).                */
/* FPC RTL's commoninit calls through CurrentTM function pointers.    */
/* These are real implementations for a single-threaded runtime using  */
/* POSIX primitives where needed.                                      */
/*                                                                     */
/* TThreadManager layout (each field is a function pointer, 8 bytes): */
/*   0: InitManager       8: DoneManager      16: BeginThread         */
/*  24: EndThread         32: SuspendThread    40: ResumeThread        */
/*  48: KillThread        56: CloseThread      64: ThreadSwitch        */
/*  72: WaitForTerminate  80: SetPriority      88: GetPriority         */
/*  96: GetCurrentThreadId 104: SetDebugNameA  112: SetDebugNameU      */
/* 120: InitCritSection   128: DoneCritSection 136: EnterCritSection   */
/* 144: TryEnterCritSection 152: LeaveCritSection                      */
/* 160: InitThreadVar     168: RelocateThreadVar                       */
/* 176: AllocateThreadVars 184: ReleaseThreadVars                      */
/* 192: BasicEventCreate  200: BasicEventDestroy                       */
/* 208: BasicEventReset   216: BasicEventSet   224: BasicEventWaitFor  */
/* 232: RTLEventCreate    240: RTLEventDestroy  248: RTLEventSetEvent  */
/* 256: RTLEventResetEvent 264: RTLEventWaitFor 272: RTLEventWaitTimeout */
/* ------------------------------------------------------------------ */

#include <stdlib.h>
#include <pthread.h>

/* RTL events: malloc'd flag + mutex + condvar for proper wait semantics */
typedef struct {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    int signaled;
} KgpcRTLEvent;

static void *kgpc_rtlevent_create(void) {
    KgpcRTLEvent *ev = (KgpcRTLEvent *)calloc(1, sizeof(KgpcRTLEvent));
    if (ev == NULL) return NULL;
    pthread_mutex_init(&ev->mutex, NULL);
    pthread_cond_init(&ev->cond, NULL);
    return ev;
}

static void kgpc_rtlevent_destroy(void *event) {
    KgpcRTLEvent *ev = (KgpcRTLEvent *)event;
    if (ev == NULL) return;
    pthread_cond_destroy(&ev->cond);
    pthread_mutex_destroy(&ev->mutex);
    free(ev);
}

static void kgpc_rtlevent_set(void *event) {
    KgpcRTLEvent *ev = (KgpcRTLEvent *)event;
    if (ev == NULL) return;
    pthread_mutex_lock(&ev->mutex);
    ev->signaled = 1;
    pthread_cond_signal(&ev->cond);
    pthread_mutex_unlock(&ev->mutex);
}

static void kgpc_rtlevent_reset(void *event) {
    KgpcRTLEvent *ev = (KgpcRTLEvent *)event;
    if (ev == NULL) return;
    pthread_mutex_lock(&ev->mutex);
    ev->signaled = 0;
    pthread_mutex_unlock(&ev->mutex);
}

static void kgpc_rtlevent_wait(void *event) {
    KgpcRTLEvent *ev = (KgpcRTLEvent *)event;
    if (ev == NULL) return;
    pthread_mutex_lock(&ev->mutex);
    while (!ev->signaled)
        pthread_cond_wait(&ev->cond, &ev->mutex);
    ev->signaled = 0;
    pthread_mutex_unlock(&ev->mutex);
}

static void kgpc_rtlevent_wait_timeout(void *event, int timeout) {
    KgpcRTLEvent *ev = (KgpcRTLEvent *)event;
    if (ev == NULL) return;
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += timeout / 1000;
    ts.tv_nsec += (timeout % 1000) * 1000000L;
    if (ts.tv_nsec >= 1000000000L) {
        ts.tv_sec++;
        ts.tv_nsec -= 1000000000L;
    }
    pthread_mutex_lock(&ev->mutex);
    while (!ev->signaled)
        if (pthread_cond_timedwait(&ev->cond, &ev->mutex, &ts) != 0)
            break;
    ev->signaled = 0;
    pthread_mutex_unlock(&ev->mutex);
}

/* Critical sections backed by pthread_mutex */
static void kgpc_critsection_init(void *cs) {
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init((pthread_mutex_t *)cs, &attr);
    pthread_mutexattr_destroy(&attr);
}
static void kgpc_critsection_done(void *cs) {
    pthread_mutex_destroy((pthread_mutex_t *)cs);
}
static void kgpc_critsection_enter(void *cs) {
    pthread_mutex_lock((pthread_mutex_t *)cs);
}
static int64_t kgpc_critsection_tryenter(void *cs) {
    return pthread_mutex_trylock((pthread_mutex_t *)cs) == 0 ? 1 : 0;
}
static void kgpc_critsection_leave(void *cs) {
    pthread_mutex_unlock((pthread_mutex_t *)cs);
}

static int64_t kgpc_tm_init(void) { return 1; }
static int64_t kgpc_get_current_thread_id(void) {
    return (int64_t)pthread_self();
}

/* CurrentTM: FPC's TThreadManager record (35 function pointers).
 * Storage is provided by runtime_fpc_thread_symbols.S (.comm directive).
 * In FPC RTL mode, the compiler-generated .globl overrides the .comm. */
extern void *CurrentTM[];

/* Called from kgpc_init_args to populate CurrentTM */
void kgpc_fpc_init_thread_manager(void)
{
    if (CurrentTM[29] != NULL)  /* already initialized */
        return;

    CurrentTM[0]  = (void *)kgpc_tm_init;               /* InitManager */
    CurrentTM[1]  = (void *)kgpc_tm_init;               /* DoneManager */
    CurrentTM[12] = (void *)kgpc_get_current_thread_id;
    CurrentTM[15] = (void *)kgpc_critsection_init;
    CurrentTM[16] = (void *)kgpc_critsection_done;
    CurrentTM[17] = (void *)kgpc_critsection_enter;
    CurrentTM[18] = (void *)kgpc_critsection_tryenter;
    CurrentTM[19] = (void *)kgpc_critsection_leave;
    CurrentTM[24] = (void *)kgpc_rtlevent_create;       /* BasicEventCreate */
    CurrentTM[25] = (void *)kgpc_rtlevent_destroy;
    CurrentTM[26] = (void *)kgpc_rtlevent_reset;
    CurrentTM[27] = (void *)kgpc_rtlevent_set;
    CurrentTM[28] = (void *)kgpc_rtlevent_wait;
    CurrentTM[29] = (void *)kgpc_rtlevent_create;       /* RTLEventCreate */
    CurrentTM[30] = (void *)kgpc_rtlevent_destroy;
    CurrentTM[31] = (void *)kgpc_rtlevent_set;
    CurrentTM[32] = (void *)kgpc_rtlevent_reset;
    CurrentTM[33] = (void *)kgpc_rtlevent_wait;
    CurrentTM[34] = (void *)kgpc_rtlevent_wait_timeout;
}

/* ------------------------------------------------------------------ */
/* FPC-mangled untyped file operations: Rewrite, Reset, Close          */
/* These map FPC system unit declarations to KGPC runtime functions.   */
/* ------------------------------------------------------------------ */
extern void kgpc_tfile_rewrite(void *file);
extern void kgpc_tfile_reset(void *file);
extern void kgpc_tfile_close(void *file);
extern int  kgpc_tfile_blockread(void *file, void *buffer, size_t count, long long *actual);
extern int  kgpc_tfile_blockwrite(void *file, const void *buffer, size_t count, long long *actual);

/* FileRec field offsets */
#define FR_RECSIZE  8    /* int64 recsize at offset 8 */

void rewrite_f_li(void *filerec, int32_t recsize)
{
    if (filerec != NULL) {
        int64_t rs = recsize;
        memcpy((char *)filerec + FR_RECSIZE, &rs, sizeof(rs));
    }
    kgpc_tfile_rewrite(filerec);
}

void rewrite_f(void *filerec)
{
    kgpc_tfile_rewrite(filerec);
}

void reset_f_li(void *filerec, int32_t recsize)
{
    if (filerec != NULL) {
        int64_t rs = recsize;
        memcpy((char *)filerec + FR_RECSIZE, &rs, sizeof(rs));
    }
    kgpc_tfile_reset(filerec);
}

void reset_f(void *filerec)
{
    kgpc_tfile_reset(filerec);
}

void close_f(void *filerec)
{
    kgpc_tfile_close(filerec);
}

/* ------------------------------------------------------------------ */
/* FPC-mangled BlockRead/BlockWrite overloads                          */
/* ------------------------------------------------------------------ */
void blockread_f_u_i64_i64(void *file, void *buffer, int64_t count, int64_t *result)
{
    long long actual = 0;
    kgpc_tfile_blockread(file, buffer, (size_t)count, &actual);
    if (result != NULL)
        *result = actual;
}

void blockread_f_u_li_li(void *file, void *buffer, int32_t count, int32_t *result)
{
    long long actual = 0;
    kgpc_tfile_blockread(file, buffer, (size_t)count, &actual);
    if (result != NULL)
        *result = (int32_t)actual;
}

void blockread_f_u_li(void *file, void *buffer, int32_t count)
{
    kgpc_tfile_blockread(file, buffer, (size_t)count, NULL);
}

void blockwrite_f_u_i64_i64(void *file, const void *buffer, int64_t count, int64_t *result)
{
    long long actual = 0;
    kgpc_tfile_blockwrite(file, buffer, (size_t)count, &actual);
    if (result != NULL)
        *result = actual;
}

void blockwrite_f_u_li_li(void *file, const void *buffer, int32_t count, int32_t *result)
{
    long long actual = 0;
    kgpc_tfile_blockwrite(file, buffer, (size_t)count, &actual);
    if (result != NULL)
        *result = (int32_t)actual;
}

void blockwrite_f_u_li(void *file, const void *buffer, int32_t count)
{
    kgpc_tfile_blockwrite(file, buffer, (size_t)count, NULL);
}
