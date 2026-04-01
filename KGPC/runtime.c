#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stddef.h>
#include <math.h>
#include <errno.h>
#include <setjmp.h>

#include "runtime_internal.h"
#include <sys/stat.h>
#include <sys/time.h>
#include <limits.h>
#include "format_arg.h"

static const double KGPC_PI = 3.14159265358979323846264338327950288;

/* FPC RTL compatibility symbols for float-to-ASCII conversion (flt_core.inc) */
const unsigned int C_1_SHL_31 = 0x80000000u;
const unsigned long long C_MANT2_INTEGER = (1ULL << 52);
const int C_EXP2_SPECIAL = 2047;  /* C_EXP2_BIAS * 2 + 1 for double */
const int RT_NATIVE = 2;  /* RT_S64REAL for double */

/* FPC Grisu lookup table for float-to-string conversion.
 * This is a simplified placeholder - the full table is in FPC's flt_core.inc. */
static const char lookup_placeholder[1] = { 0 };
const char *lookup = lookup_placeholder;

/* FPC dynamic library handle placeholder */
#ifdef _WIN32
static void *Libdl_handle = NULL;
void *Libdl = NULL;
void *libdl = NULL;
#else
static void *Libdl_handle = NULL;
void *Libdl = &Libdl_handle;
void *libdl = &Libdl_handle;
#endif

/* FPC I/O check functions - weak stubs for real number I/O.
 * These are provided by FPC's RTL when linked, but we provide weak fallbacks
 * for cases where the RTL code isn't included. */
__attribute__((weak)) void checkread_t(void) { }
__attribute__((weak)) void readreal_t_ss(void) { }

/* FPC errno setter - weak stub */
__attribute__((weak)) void FPC_SYS_SETERRNO(int err) { errno = err; }

uint32_t kgpc_randseed = 0u;
static uint32_t kgpc_old_randseed = 0xFFFFFFFFu;
/* Native stdlib lowering currently materializes a qualifier symbol for
 * unit-qualified System.Error(...) calls. Provide an addressable symbol so
 * those references link without reviving the broad System.Error alias. */
int32_t System = 0;

/* Xoshiro128** state (matching FPC rtl/inc/system.inc). */
static uint32_t kgpc_xsr_state[4] = {
    0xAFF181C0u,
    0x73B13BA2u,
    0x1340D3B4u,
    0x61204305u
};

/* Forward decl for optional debug flag helper */
static int kgpc_env_flag(const char *name);
char *kgpc_float_to_string(double value, int precision);
static char *kgpc_apply_field_width(char *value, int64_t width);
uint16_t *kgpc_unicodestring_from_string(const char *value);

#ifdef _WIN32
#include <windows.h>
#include <time.h>
#include <errno.h>
#include <conio.h>
#include <io.h>
#include <fcntl.h>
#include <direct.h>
#else
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <sys/select.h>
#include <pthread.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <dlfcn.h>

/* Define W_EXITCODE and W_STOPCODE if not available */
#ifndef W_EXITCODE
#define W_EXITCODE(ret, sig) ((ret) << 8 | (sig))
#endif

#ifndef W_STOPCODE
#define W_STOPCODE(sig) ((sig) << 8 | 0x7f)
#endif
#endif

#ifdef __linux__
#ifdef __x86_64__
long FPC_SYSCALL0(long sysnr) {
    long ret;
    __asm__ volatile ("syscall" : "=a"(ret) : "a"(sysnr) : "rcx", "r11", "memory");
    if (ret < 0 && ret >= -4095) { errno = -ret; return -1; }
    return ret;
}
long FPC_SYSCALL1(long sysnr, long p1) {
    long ret;
    __asm__ volatile ("syscall" : "=a"(ret) : "a"(sysnr), "D"(p1) : "rcx", "r11", "memory");
    if (ret < 0 && ret >= -4095) { errno = -ret; return -1; }
    return ret;
}
long FPC_SYSCALL2(long sysnr, long p1, long p2) {
    long ret;
    __asm__ volatile ("syscall" : "=a"(ret) : "a"(sysnr), "D"(p1), "S"(p2) : "rcx", "r11", "memory");
    if (ret < 0 && ret >= -4095) { errno = -ret; return -1; }
    return ret;
}
long FPC_SYSCALL3(long sysnr, long p1, long p2, long p3) {
    long ret;
    __asm__ volatile ("syscall" : "=a"(ret) : "a"(sysnr), "D"(p1), "S"(p2), "d"(p3) : "rcx", "r11", "memory");
    if (ret < 0 && ret >= -4095) { errno = -ret; return -1; }
    return ret;
}
long FPC_SYSCALL4(long sysnr, long p1, long p2, long p3, long p4) {
    long ret;
    register long r10 __asm__("r10") = p4;
    __asm__ volatile ("syscall" : "=a"(ret) : "a"(sysnr), "D"(p1), "S"(p2), "d"(p3), "r"(r10) : "rcx", "r11", "memory");
    if (ret < 0 && ret >= -4095) { errno = -ret; return -1; }
    return ret;
}
long FPC_SYSCALL5(long sysnr, long p1, long p2, long p3, long p4, long p5) {
    long ret;
    register long r10 __asm__("r10") = p4;
    register long r8 __asm__("r8") = p5;
    __asm__ volatile ("syscall" : "=a"(ret) : "a"(sysnr), "D"(p1), "S"(p2), "d"(p3), "r"(r10), "r"(r8) : "rcx", "r11", "memory");
    if (ret < 0 && ret >= -4095) { errno = -ret; return -1; }
    return ret;
}
long FPC_SYSCALL6(long sysnr, long p1, long p2, long p3, long p4, long p5, long p6) {
    long ret;
    register long r10 __asm__("r10") = p4;
    register long r8 __asm__("r8") = p5;
    register long r9 __asm__("r9") = p6;
    __asm__ volatile ("syscall" : "=a"(ret) : "a"(sysnr), "D"(p1), "S"(p2), "d"(p3), "r"(r10), "r"(r8), "r"(r9) : "rcx", "r11", "memory");
    if (ret < 0 && ret >= -4095) { errno = -ret; return -1; }
    return ret;
}
#endif
#endif

int64_t kgpc_current_exception = 0;
static __thread int kgpc_ioresult = 0;
static int kgpc_threading_used = 0;

/* ────────── setjmp/longjmp exception frame stack ────────── */

typedef struct kgpc_except_frame {
    jmp_buf buf;
} kgpc_except_frame;

#define KGPC_EXCEPT_STACK_INIT 8

static kgpc_except_frame *kgpc_except_stack = NULL;
static int kgpc_except_stack_depth = 0;
static int kgpc_except_stack_capacity = 0;

jmp_buf *kgpc_push_except_frame(void)
{
    if (kgpc_except_stack_depth >= kgpc_except_stack_capacity) {
        int new_cap = (kgpc_except_stack_capacity > 0)
                      ? kgpc_except_stack_capacity * 2
                      : KGPC_EXCEPT_STACK_INIT;
        kgpc_except_frame *new_stack =
            (kgpc_except_frame *)realloc(kgpc_except_stack,
                                         sizeof(kgpc_except_frame) * (size_t)new_cap);
        if (new_stack == NULL) {
            fprintf(stderr, "KGPC runtime: except frame stack allocation failed\n");
            exit(EXIT_FAILURE);
        }
        kgpc_except_stack = new_stack;
        kgpc_except_stack_capacity = new_cap;
    }
    return &kgpc_except_stack[kgpc_except_stack_depth++].buf;
}

void kgpc_pop_except_frame(void)
{
    if (kgpc_except_stack_depth > 0)
        kgpc_except_stack_depth--;
}

/* ────────── end exception frame stack ────────── */

int kgpc_ioresult_get_and_clear(void)
{
    int value = kgpc_ioresult;
    kgpc_ioresult = 0;
    return value;
}

void kgpc_ioresult_set(int value)
{
    kgpc_ioresult = value;
}

int kgpc_ioresult_peek(void)
{
    return kgpc_ioresult;
}

void kgpc_cthreads_init(void)
{
#ifndef _WIN32
    (void)pthread_self();
#endif
}

int kgpc_threading_already_used(void)
{
    return kgpc_threading_used;
}

int threadingalreadyused_void(void)
{
    return kgpc_threading_already_used();
}

#define KGPC_ATOMIC_INC_RET(target, val) __sync_add_and_fetch(target, val)
#define KGPC_ATOMIC_DEC_RET(target, val) __sync_sub_and_fetch(target, val)
#define KGPC_ATOMIC_EXCHANGE(target, val) __atomic_exchange_n(target, val, __ATOMIC_SEQ_CST)
#define KGPC_ATOMIC_CMP_EXCHANGE(target, val, comp) __sync_val_compare_and_swap(target, comp, val)

int32_t kgpc_interlockedincrement(int32_t *target)
{
    return (int32_t)KGPC_ATOMIC_INC_RET(target, 1);
}

int32_t kgpc_interlockeddecrement(int32_t *target)
{
    return (int32_t)KGPC_ATOMIC_DEC_RET(target, 1);
}

int64_t kgpc_interlockedincrement64(int64_t *target)
{
    return (int64_t)KGPC_ATOMIC_INC_RET(target, 1);
}

int64_t kgpc_interlockeddecrement64(int64_t *target)
{
    return (int64_t)KGPC_ATOMIC_DEC_RET(target, 1);
}

void kgpc_interlocked_exchange_add_i32(int32_t *target, int32_t value, int32_t *result)
{
#if defined(__GNUC__) || defined(__clang__)
    int32_t old = __atomic_fetch_add(target, value, __ATOMIC_SEQ_CST);
#elif defined(_WIN32)
    int32_t old = (int32_t)InterlockedExchangeAdd((volatile LONG *)target, (LONG)value);
#else
    int32_t old = *target;
    *target += value;
#endif
    if (result != NULL)
        *result = old;
}

void kgpc_interlocked_exchange_add_i64(int64_t *target, int64_t value, int64_t *result)
{
#if defined(__GNUC__) || defined(__clang__)
    int64_t old = __atomic_fetch_add(target, value, __ATOMIC_SEQ_CST);
#elif defined(_WIN32) && defined(_WIN64)
    int64_t old = (int64_t)InterlockedExchangeAdd64((volatile LONGLONG *)target, (LONGLONG)value);
#else
    int64_t old = *target;
    *target += value;
#endif
    if (result != NULL)
        *result = old;
}

void kgpc_interlocked_exchange_add_ptr(intptr_t *target, intptr_t value, intptr_t *result)
{
#if defined(__GNUC__) || defined(__clang__)
    intptr_t old = __atomic_fetch_add(target, value, __ATOMIC_SEQ_CST);
#elif defined(_WIN32) && defined(_WIN64)
    intptr_t old = (intptr_t)InterlockedExchangeAdd64((volatile LONGLONG *)target, (LONGLONG)value);
#else
    intptr_t old = *target;
    *target += value;
#endif
    if (result != NULL)
        *result = old;
}


typedef enum {
    KGPC_BINARY_UNSPECIFIED = 0,
    KGPC_BINARY_INT32,
    KGPC_BINARY_CHAR,
    KGPC_BINARY_DOUBLE
} KGPCBinaryType;

typedef struct KGPCTextRec
{
    int32_t handle;         /* offset 0 */
    int32_t mode;           /* offset 4 */
    int64_t bufsize;        /* offset 8 */
    int64_t private_data;   /* offset 16 */
    int64_t bufpos;         /* offset 24 */
    int64_t bufend;         /* offset 32 */
    char *bufptr;           /* offset 40 */
    void *openfunc;         /* offset 48 */
    void *inoutfunc;        /* offset 56 */
    void *flushfunc;        /* offset 64 */
    void *closefunc;        /* offset 72 */
    unsigned char userdata[32]; /* offset 80 */
    char name[256];         /* offset 112 */
    char line_end[4];       /* offset 368 */
    char buffer[256];       /* offset 372 */
    uint16_t codepage;      /* offset 628 */
    unsigned char _pad_fullname[2]; /* padding: 630..631 */
    void *fullname;         /* offset 632 */
} KGPCTextRec;

/* Ensure TEXT_TYPE size in the compiler stays in sync with this struct.
 * sizeof_from_type_tag(TEXT_TYPE) returns 640; any layout change here
 * must be mirrored there. */
_Static_assert(sizeof(KGPCTextRec) == 640,
    "KGPCTextRec size must be 640 to match TEXT_TYPE in SemCheck_sizeof.c");

typedef struct KGPCFileRec
{
    int32_t handle;
    int32_t mode;
    int64_t recsize;
    unsigned char private_data[64];
    unsigned char userdata[32];
    char name[256];
    void *fullname;          /* USE_FILEREC_FULLNAME pointer */
} KGPCFileRec;

typedef struct KGPCFilePrivate
{
    FILE *handle;
    KGPCBinaryType element_type;
    size_t element_size;
} KGPCFilePrivate;

#define KGPC_FILE_PRIVATE_MAGIC 0x4B475046u
#define KGPC_FILE_PRIVATE_MAGIC_INV (~KGPC_FILE_PRIVATE_MAGIC)

static char *kgpc_string_alloc_with_length(size_t length);
char *kgpc_alloc_empty_string(void);

static int kgpc_file_private_magic_valid(const KGPCFileRec *file)
{
    if (file == NULL)
        return 0;
    uint32_t magic = 0;
    uint32_t inv = 0;
    size_t base = sizeof(file->private_data) - (2 * sizeof(uint32_t));
    memcpy(&magic, file->private_data + base, sizeof(magic));
    memcpy(&inv, file->private_data + base + sizeof(magic), sizeof(inv));
    return (magic == KGPC_FILE_PRIVATE_MAGIC && inv == KGPC_FILE_PRIVATE_MAGIC_INV);
}

#define KGPC_FM_CLOSED 0xD7B0
#define KGPC_FM_INPUT  0xD7B1
#define KGPC_FM_OUTPUT 0xD7B2
#define KGPC_FM_INOUT  0xD7B3

/* Global standard I/O file variables for Pascal programs */
/* These are initialized lazily in the output/input stream functions */
static KGPCTextRec kgpc_stdin_file = { 0 };
static KGPCTextRec kgpc_stdout_file = { 0 };
static KGPCTextRec kgpc_stderr_file = { 0 };
static int kgpc_argc = 0;
static char **kgpc_argv = NULL;

/* Global pointers exported for Pascal programs to reference */
KGPCTextRec *stdin_ptr = NULL;
KGPCTextRec *stdout_ptr = NULL;
KGPCTextRec *stderr_ptr = NULL;
KGPCTextRec *Input_ptr = NULL;   /* Standard Pascal Input */
KGPCTextRec *Output_ptr = NULL;  /* Standard Pascal Output */

/* Initialize standard I/O file handles (called once) */
static void kgpc_init_stdio(void)
{
    static int initialized = 0;
    if (initialized)
        return;
    initialized = 1;
    
    kgpc_stdin_file.handle = fileno(stdin);
    kgpc_stdout_file.handle = fileno(stdout);
    kgpc_stderr_file.handle = fileno(stderr);
    kgpc_stdin_file.mode = KGPC_FM_INPUT;
    kgpc_stdout_file.mode = KGPC_FM_OUTPUT;
    kgpc_stderr_file.mode = KGPC_FM_OUTPUT;
    kgpc_stdin_file.private_data = (int64_t)(uintptr_t)stdin;
    kgpc_stdout_file.private_data = (int64_t)(uintptr_t)stdout;
    kgpc_stderr_file.private_data = (int64_t)(uintptr_t)stderr;
    kgpc_stdin_file.bufptr = kgpc_stdin_file.buffer;
    kgpc_stdout_file.bufptr = kgpc_stdout_file.buffer;
    kgpc_stderr_file.bufptr = kgpc_stderr_file.buffer;
    kgpc_stdin_file.bufsize = (int64_t)sizeof(kgpc_stdin_file.buffer);
    kgpc_stdout_file.bufsize = (int64_t)sizeof(kgpc_stdout_file.buffer);
    kgpc_stderr_file.bufsize = (int64_t)sizeof(kgpc_stderr_file.buffer);
    
    stdin_ptr = &kgpc_stdin_file;
    stdout_ptr = &kgpc_stdout_file;
    stderr_ptr = &kgpc_stderr_file;
    Input_ptr = &kgpc_stdin_file;   /* Input = stdin */
    Output_ptr = &kgpc_stdout_file; /* Output = stdout */
}

static void kgpc_textrec_init_defaults(KGPCTextRec *file)
{
    if (file == NULL)
        return;
    if (file->bufptr == NULL)
        file->bufptr = file->buffer;
    if (file->bufsize == 0)
        file->bufsize = (int64_t)sizeof(file->buffer);
    if (file->mode == 0)
        file->mode = KGPC_FM_CLOSED;
}

/* Side-table tracking which handle+mode each cached FILE* was created for.
 * Detects stale caches after FPC RTL close+reopen cycles.
 * Also stores the original file's dev+ino to detect fd recycling
 * (same fd number reused for a different file). */
#define KGPC_STREAM_CACHE_SLOTS 16
static struct {
    KGPCTextRec *textrec;
    int32_t original_handle;
    int32_t original_mode;
    dev_t original_dev;
    ino_t original_ino;
    int has_stat;  /* 1 if original_dev/ino are valid */
} kgpc_stream_cache[KGPC_STREAM_CACHE_SLOTS];

static int kgpc_stream_cache_find(KGPCTextRec *file)
{
    for (int i = 0; i < KGPC_STREAM_CACHE_SLOTS; i++)
        if (kgpc_stream_cache[i].textrec == file)
            return i;
    return -1;
}

static void kgpc_stream_cache_set(KGPCTextRec *file, int32_t handle, int32_t mode)
{
    int idx = kgpc_stream_cache_find(file);
    if (idx < 0)
    {
        /* Find empty slot */
        for (int i = 0; i < KGPC_STREAM_CACHE_SLOTS; i++)
            if (kgpc_stream_cache[i].textrec == NULL)
            {
                idx = i;
                break;
            }
    }
    if (idx >= 0)
    {
        kgpc_stream_cache[idx].textrec = file;
        kgpc_stream_cache[idx].original_handle = handle;
        kgpc_stream_cache[idx].original_mode = mode;
        /* Record device+inode so we can detect fd recycling later. */
        struct stat st;
        if (handle >= 0 && fstat(handle, &st) == 0)
        {
            kgpc_stream_cache[idx].original_dev = st.st_dev;
            kgpc_stream_cache[idx].original_ino = st.st_ino;
            kgpc_stream_cache[idx].has_stat = 1;
        }
        else
        {
            kgpc_stream_cache[idx].original_dev = 0;
            kgpc_stream_cache[idx].original_ino = 0;
            kgpc_stream_cache[idx].has_stat = 0;
        }
    }
}

static void kgpc_stream_cache_clear(KGPCTextRec *file)
{
    int idx = kgpc_stream_cache_find(file);
    if (idx >= 0)
    {
        kgpc_stream_cache[idx].textrec = NULL;
        kgpc_stream_cache[idx].original_handle = -1;
        kgpc_stream_cache[idx].original_mode = 0;
        kgpc_stream_cache[idx].original_dev = 0;
        kgpc_stream_cache[idx].original_ino = 0;
        kgpc_stream_cache[idx].has_stat = 0;
    }
}

static FILE *kgpc_textrec_get_stream(KGPCTextRec *file, FILE *fallback)
{
    kgpc_init_stdio();
    if (file == NULL)
        return fallback;
    /* When FPC RTL opens a file, it sets TextRec.Handle but not
     * private_data.  Use the handle to create a FILE* stream.
     * FPC mode constants: fmClosed=0xD7B0, fmInput=0xD7B1,
     * fmOutput=0xD7B2, fmInOut=0xD7B3, fmAppend=0xD7B4 */
    int32_t h = file->handle;
    if (file->private_data != 0)
    {
        FILE *cached = (FILE *)(uintptr_t)file->private_data;
        int cached_fd = fileno(cached);
        if (cached_fd >= 0 && cached_fd == h)
            return cached;  /* KGPC RTL: direct ownership, always valid */
        /* FPC RTL path: the FILE* was created via fdopen(dup(h)), so
         * cached_fd != h.  Validate via the side-table: check that the
         * original handle and mode haven't changed (which would indicate
         * the FPC RTL closed and reopened the file).
         *
         * Critical invariant: when h and cached_fd share the same kernel
         * file description (dup'd from the same open), lseek on both
         * returns the same position.  After FPC RTL close+reopen, h is
         * a NEW file description (position 0) while cached_fd still
         * references the OLD one (at whatever position reading left it).
         * Comparing the two kernel positions detects this stale case. */
        int is_closed = (file->mode == (int32_t)0xD7B0 || file->mode == 0);
        int idx = kgpc_stream_cache_find(file);
        int cache_valid = 0;
        if (!is_closed && h >= 0 && idx >= 0 &&
            h == kgpc_stream_cache[idx].original_handle &&
            file->mode == kgpc_stream_cache[idx].original_mode)
        {
            /* Handle and mode match, but the fd could have been recycled
             * (closed and reopened to a different file).  Use fstat to
             * compare device+inode with the originals when available. */
            if (kgpc_stream_cache[idx].has_stat)
            {
                struct stat st;
                if (fstat(h, &st) == 0 &&
                    st.st_dev == kgpc_stream_cache[idx].original_dev &&
                    st.st_ino == kgpc_stream_cache[idx].original_ino)
                {
                    /* Same file on disk.  But h might be a freshly-opened
                     * fd (after FPC RTL close+reopen) while cached_fd
                     * still refers to the old file description.  When h
                     * and cached_fd share the same kernel file description
                     * (normal case), lseek returns the same position for
                     * both because offset is per-description, not per-fd.
                     * After close+reopen, they are different descriptions
                     * with independent positions — detect via divergence. */
                    off_t h_pos = lseek(h, 0, SEEK_CUR);
                    off_t c_pos = lseek(cached_fd, 0, SEEK_CUR);
                    if (h_pos != (off_t)-1 && c_pos != (off_t)-1
                        && h_pos != c_pos)
                    {
                        /* Positions differ: h and cached_fd are different
                         * file descriptions.  Cache is stale. */
                        cache_valid = 0;
                    }
                    else
                    {
                        cache_valid = 1;
                    }
                }
            }
            else
            {
                /* fstat was unavailable when the cache was created (e.g.
                 * special fd); fall back to handle+mode validation only. */
                cache_valid = 1;
            }
        }
        if (cache_valid)
            return cached;
        /* Stale cache: close the old stream and clear */
        fclose(cached);
        file->private_data = 0;
        kgpc_stream_cache_clear(file);
    }
    if (h >= 0 && file->mode != 0 && file->mode != (int32_t)0xD7B0)
    {
        if (h == STDOUT_FILENO)
            return stdout;
        if (h == STDERR_FILENO)
            return stderr;
        if (h == STDIN_FILENO)
            return stdin;
        /* Valid fd opened by FPC — wrap in FILE* and cache */
        const char *fmode = "w";
        if (file->mode == (int32_t)0xD7B1) /* fmInput */
            fmode = "r";
        else if (file->mode == (int32_t)0xD7B4) /* fmAppend */
            fmode = "a";
        FILE *stream = fdopen(dup(h), fmode);
        if (stream != NULL)
        {
            file->private_data = (int64_t)(uintptr_t)stream;
            kgpc_stream_cache_set(file, h, file->mode);
            return stream;
        }
    }
    return fallback;
}

static void kgpc_textrec_set_stream(KGPCTextRec *file, FILE *stream)
{
    if (file == NULL)
        return;
    file->private_data = (int64_t)(uintptr_t)stream;
    if (stream != NULL)
    {
        file->handle = fileno(stream);
        kgpc_stream_cache_set(file, file->handle, file->mode);
    }
    else
        kgpc_stream_cache_clear(file);
}

void kgpc_text_setbuf(KGPCTextRec *file, void *buffer, int32_t size)
{
    if (file == NULL)
        return;
    file->bufptr = (char *)buffer;
    file->bufsize = size > 0 ? (int64_t)size : 0;
}

static KGPCFilePrivate kgpc_file_private_get(const KGPCFileRec *file)
{
    KGPCFilePrivate priv;
    memset(&priv, 0, sizeof(priv));
    if (file == NULL)
        return priv;
    if (!kgpc_file_private_magic_valid(file))
        return priv;
    memcpy(&priv, file->private_data, sizeof(priv));
    return priv;
}

static void kgpc_file_private_set(KGPCFileRec *file, const KGPCFilePrivate *priv)
{
    if (file == NULL || priv == NULL)
        return;
    memset(file->private_data, 0, sizeof(file->private_data));
    memcpy(file->private_data, priv, sizeof(*priv));
    uint32_t magic = KGPC_FILE_PRIVATE_MAGIC;
    uint32_t inv = KGPC_FILE_PRIVATE_MAGIC_INV;
    size_t base = sizeof(file->private_data) - (2 * sizeof(uint32_t));
    memcpy(file->private_data + base, &magic, sizeof(magic));
    memcpy(file->private_data + base + sizeof(magic), &inv, sizeof(inv));
}

static void kgpc_copy_name(char *dest, size_t dest_size, const char *src);

int kgpc_file_is_text(void *slot)
{
    if (slot == NULL)
        return 1;
    return 1;
}

static FILE *kgpc_text_output_stream(KGPCTextRec *file)
{
    kgpc_init_stdio();  /* Ensure stdio is initialized */
    if (file != NULL)
    {
        FILE *stream = kgpc_textrec_get_stream(file, stdout);
        if (stream != NULL)
            return stream;
    }
#ifdef _WIN32
    static int kgpc_stdout_binary_set = 0;
    if (!kgpc_stdout_binary_set)
    {
        /* Avoid CRLF translation doubling when strings contain CRLF */
        _setmode(_fileno(stdout), _O_BINARY);
        kgpc_stdout_binary_set = 1;
    }
#endif
    return stdout;
}

static void kgpc_flush_text_output_stream(FILE *dest)
{
    if (dest == NULL)
        return;
#ifdef _WIN32
    if (_isatty(_fileno(dest)))
        fflush(dest);
#else
    if (isatty(fileno(dest)))
        fflush(dest);
#endif
}

/* Flush a Text file — Pascal's Flush(var f: Text) */
void kgpc_flush(KGPCTextRec *file)
{
    if (file == NULL)
        return;
    FILE *fp = kgpc_textrec_get_stream(file, stdout);
    if (fp != NULL)
        fflush(fp);
}

/* GetFPCHeapStatus — returns a TFPCHeapStatus record with heap usage info.
   Fields: MaxHeapSize, MaxHeapUsed, CurrHeapSize, CurrHeapUsed, CurrHeapFree (all PtrUInt). */
typedef struct {
    uint64_t MaxHeapSize;
    uint64_t MaxHeapUsed;
    uint64_t CurrHeapSize;
    uint64_t CurrHeapUsed;
    uint64_t CurrHeapFree;
} KGPCFPCHeapStatus;

KGPCFPCHeapStatus kgpc_get_fpc_heap_status(void)
{
    KGPCFPCHeapStatus status = {0};
    /* Provide approximate values — exact tracking would require wrapping malloc */
    status.MaxHeapSize = 0;
    status.MaxHeapUsed = 0;
    status.CurrHeapSize = 0;
    status.CurrHeapUsed = 0;
    status.CurrHeapFree = 0;
    return status;
}

/* GetLocalTime — fills a TSystemTime record with the current local time.
   TSystemTime layout: Year, Month, Day, DayOfWeek, Hour, Minute, Second, Millisecond (all uint16). */
typedef struct {
    uint16_t Year, Month, Day, DayOfWeek;
    uint16_t Hour, Minute, Second, Millisecond;
} KGPCSystemTime;

void kgpc_getlocaltime(KGPCSystemTime *st)
{
#ifdef _WIN32
    SYSTEMTIME sys_time;
    if (st == NULL)
        return;
    GetLocalTime(&sys_time);
    st->Year = (uint16_t)sys_time.wYear;
    st->Month = (uint16_t)sys_time.wMonth;
    st->Day = (uint16_t)sys_time.wDay;
    st->DayOfWeek = (uint16_t)sys_time.wDayOfWeek;
    st->Hour = (uint16_t)sys_time.wHour;
    st->Minute = (uint16_t)sys_time.wMinute;
    st->Second = (uint16_t)sys_time.wSecond;
    st->Millisecond = (uint16_t)sys_time.wMilliseconds;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    struct tm *tm = localtime(&tv.tv_sec);
    if (tm == NULL || st == NULL) return;
    st->Year = (uint16_t)(tm->tm_year + 1900);
    st->Month = (uint16_t)(tm->tm_mon + 1);
    st->Day = (uint16_t)tm->tm_mday;
    st->DayOfWeek = (uint16_t)tm->tm_wday;
    st->Hour = (uint16_t)tm->tm_hour;
    st->Minute = (uint16_t)tm->tm_min;
    st->Second = (uint16_t)tm->tm_sec;
    st->Millisecond = (uint16_t)(tv.tv_usec / 1000);
#endif
}

/* DecodeTime — extracts time components from a TDateTime value.
   TDateTime is a double: integer part = days since 1899-12-30, fractional = time of day. */
void kgpc_decodetime(double dt, uint16_t *hour, uint16_t *min, uint16_t *sec, uint16_t *msec)
{
    double frac = dt - (int64_t)dt;
    if (frac < 0) frac = -frac;
    double total_ms = frac * 86400000.0;
    int64_t ms = (int64_t)(total_ms + 0.5);
    if (hour) *hour = (uint16_t)((ms / 3600000) % 24);
    if (min) *min = (uint16_t)((ms / 60000) % 60);
    if (sec) *sec = (uint16_t)((ms / 1000) % 60);
    if (msec) *msec = (uint16_t)(ms % 1000);
}

/* DecodeDate — extracts date components from a TDateTime value. */
void kgpc_decodedate(double dt, uint16_t *year, uint16_t *month, uint16_t *day)
{
    /* Convert TDateTime to Unix-like: TDateTime epoch is 1899-12-30 */
    int64_t days = (int64_t)dt;
    /* Use a simple algorithm: convert to calendar date */
    /* TDateTime day 1 = 1899-12-31, day 2 = 1900-01-01 */
    /* We use the time.h approach: convert to seconds since epoch */
    /* TDateTime 25569 = 1970-01-01 (Unix epoch) */
    time_t t = (time_t)((days - 25569) * 86400);
    struct tm *tm = gmtime(&t);
    if (tm == NULL) {
        if (year) *year = 0;
        if (month) *month = 0;
        if (day) *day = 0;
        return;
    }
    if (year) *year = (uint16_t)(tm->tm_year + 1900);
    if (month) *month = (uint16_t)(tm->tm_mon + 1);
    if (day) *day = (uint16_t)tm->tm_mday;
}

static FILE *kgpc_text_input_stream(KGPCTextRec *file)
{
    kgpc_init_stdio();  /* Ensure stdio is initialized */
    return kgpc_textrec_get_stream(file, stdin);
}

/* ------------------------------------------------------------------
 * Typed file support (binary files: file of Integer/Char/Real).
 *
 * These helpers operate on FileRec-compatible records and store the
 * FILE* pointer in the record's private data buffer.
 * ------------------------------------------------------------------ */

static void kgpc_copy_name(char *dest, size_t dest_size, const char *src)
{
    if (dest == NULL || dest_size == 0)
        return;
    if (src == NULL)
    {
        dest[0] = '\0';
        return;
    }
    strncpy(dest, src, dest_size - 1);
    dest[dest_size - 1] = '\0';
}

void kgpc_tfile_assign(KGPCFileRec *file, const char *path)
{
    if (file == NULL)
        return;

    KGPCFilePrivate priv;
    memset(&priv, 0, sizeof(priv));
    if (file->mode == KGPC_FM_INPUT || file->mode == KGPC_FM_OUTPUT ||
        file->mode == KGPC_FM_INOUT)
    {
        priv = kgpc_file_private_get(file);
        if (priv.handle != NULL)
        {
            fclose(priv.handle);
            priv.handle = NULL;
        }
    }
    priv.element_type = KGPC_BINARY_UNSPECIFIED;
    priv.element_size = 0;
    kgpc_file_private_set(file, &priv);

    file->handle = -1;
    file->mode = KGPC_FM_CLOSED;
    file->fullname = NULL;
    kgpc_copy_name(file->name, sizeof(file->name), path);
}

void kgpc_tfile_configure(KGPCFileRec *file, size_t element_size, int element_tag)
{
    if (file == NULL)
        return;

    KGPCFilePrivate priv;
    memset(&priv, 0, sizeof(priv));
    if (file->mode == KGPC_FM_INPUT || file->mode == KGPC_FM_OUTPUT ||
        file->mode == KGPC_FM_INOUT)
        priv = kgpc_file_private_get(file);
    if (element_size > 0)
    {
        priv.element_size = element_size;
        file->recsize = (int64_t)element_size;
    }

    switch (element_tag)
    {
        case HASHVAR_CHAR:
            priv.element_type = KGPC_BINARY_CHAR;
            break;
        case HASHVAR_REAL:
            priv.element_type = KGPC_BINARY_DOUBLE;
            break;
        case HASHVAR_INTEGER:
        case HASHVAR_LONGINT:
        default:
            priv.element_type = KGPC_BINARY_INT32;
            break;
    }

    kgpc_file_private_set(file, &priv);
}

void kgpc_tfile_rewrite(KGPCFileRec *file)
{
    if (file == NULL || file->name[0] == '\0')
        return;

    KGPCFilePrivate priv;
    memset(&priv, 0, sizeof(priv));
    if (file->mode == KGPC_FM_INPUT || file->mode == KGPC_FM_OUTPUT ||
        file->mode == KGPC_FM_INOUT)
    {
        priv = kgpc_file_private_get(file);
        if (priv.handle != NULL)
        {
            fclose(priv.handle);
            priv.handle = NULL;
        }
    }

    priv.handle = fopen(file->name, "wb");
    if (priv.handle != NULL)
    {
        file->handle = fileno(priv.handle);
        file->mode = KGPC_FM_INOUT;
        kgpc_ioresult_set(0);
    }
    else
    {
        file->handle = -1;
        file->mode = KGPC_FM_CLOSED;
        kgpc_ioresult_set(errno);
    }

    kgpc_file_private_set(file, &priv);
}

void kgpc_tfile_reset(KGPCFileRec *file)
{
    if (file == NULL || file->name[0] == '\0')
        return;

    KGPCFilePrivate priv;
    memset(&priv, 0, sizeof(priv));
    if (file->mode == KGPC_FM_INPUT || file->mode == KGPC_FM_OUTPUT ||
        file->mode == KGPC_FM_INOUT)
    {
        priv = kgpc_file_private_get(file);
        if (priv.handle != NULL)
        {
            fclose(priv.handle);
            priv.handle = NULL;
        }
    }

    priv.handle = fopen(file->name, "r+b");
    if (priv.handle != NULL)
    {
        file->handle = fileno(priv.handle);
        file->mode = KGPC_FM_INOUT;
        kgpc_ioresult_set(0);
    }
    else
    {
        file->handle = -1;
        file->mode = KGPC_FM_CLOSED;
        kgpc_ioresult_set(errno);
    }

    kgpc_file_private_set(file, &priv);
}

void kgpc_tfile_close(KGPCFileRec *file)
{
    if (file == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return;
    }

    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        file->handle = -1;
        file->mode = KGPC_FM_CLOSED;
        kgpc_ioresult_set(0);
        return;
    }

    if (fclose(priv.handle) == 0)
    {
        kgpc_ioresult_set(0);
    }
    else
    {
        kgpc_ioresult_set(errno);
    }

    priv.handle = NULL;
    file->handle = -1;
    file->mode = KGPC_FM_CLOSED;
    kgpc_file_private_set(file, &priv);
}

static size_t kgpc_tfile_element_size(KGPCFileRec *file, KGPCFilePrivate *priv)
{
    size_t elem_size = 0;
    if (priv->element_size > 0)
        elem_size = priv->element_size;
    else if (file != NULL && file->recsize > 0)
        elem_size = (size_t)file->recsize;

    if (elem_size == 0)
    {
        switch (priv->element_type)
        {
            case KGPC_BINARY_CHAR: elem_size = sizeof(unsigned char); break;
            case KGPC_BINARY_DOUBLE: elem_size = sizeof(double); break;
            case KGPC_BINARY_INT32:
            case KGPC_BINARY_UNSPECIFIED:
            default:
                elem_size = sizeof(int32_t);
                break;
        }
    }
    return elem_size;
}

int kgpc_tfile_read_int(KGPCFileRec *file, int32_t *ptr)
{
    if (file == NULL || ptr == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }
    priv.element_type = KGPC_BINARY_INT32;
    priv.element_size = sizeof(int32_t);
    size_t n = fread(ptr, sizeof(int32_t), 1, priv.handle);
    kgpc_file_private_set(file, &priv);
    if (n == 1 || feof(priv.handle))
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(priv.handle) ? EIO : 0);
    return ferror(priv.handle) ? 1 : 0;
}

int kgpc_tfile_read_char(KGPCFileRec *file, char *ptr)
{
    if (file == NULL || ptr == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }
    unsigned char ch = 0;
    priv.element_type = KGPC_BINARY_CHAR;
    priv.element_size = sizeof(unsigned char);
    size_t n = fread(&ch, sizeof(unsigned char), 1, priv.handle);
    kgpc_file_private_set(file, &priv);
    if (n == 1)
    {
        *ptr = (char)ch;
        kgpc_ioresult_set(0);
        return 0;
    }
    if (feof(priv.handle))
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(priv.handle) ? EIO : 0);
    return ferror(priv.handle) ? 1 : 0;
}

int kgpc_tfile_read_real(KGPCFileRec *file, double *ptr)
{
    if (file == NULL || ptr == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }
    priv.element_type = KGPC_BINARY_DOUBLE;
    priv.element_size = sizeof(double);
    size_t n = fread(ptr, sizeof(double), 1, priv.handle);
    kgpc_file_private_set(file, &priv);
    if (n == 1 || feof(priv.handle))
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(priv.handle) ? EIO : 0);
    return ferror(priv.handle) ? 1 : 0;
}

int kgpc_tfile_write_int(KGPCFileRec *file, int32_t value)
{
    if (file == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }
    priv.element_type = KGPC_BINARY_INT32;
    priv.element_size = sizeof(int32_t);
    size_t n = fwrite(&value, sizeof(int32_t), 1, priv.handle);
    kgpc_file_private_set(file, &priv);
    if (n == 1)
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(priv.handle) ? EIO : 1);
    return 1;
}

int kgpc_tfile_write_char(KGPCFileRec *file, char value)
{
    if (file == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }
    unsigned char ch = (unsigned char)value;
    priv.element_type = KGPC_BINARY_CHAR;
    priv.element_size = sizeof(unsigned char);
    size_t n = fwrite(&ch, sizeof(unsigned char), 1, priv.handle);
    kgpc_file_private_set(file, &priv);
    if (n == 1)
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(priv.handle) ? EIO : 1);
    return 1;
}

int kgpc_tfile_write_real(KGPCFileRec *file, double value)
{
    if (file == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }
    priv.element_type = KGPC_BINARY_DOUBLE;
    priv.element_size = sizeof(double);
    size_t n = fwrite(&value, sizeof(double), 1, priv.handle);
    kgpc_file_private_set(file, &priv);
    if (n == 1)
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(priv.handle) ? EIO : 1);
    return 1;
}

int kgpc_tfile_blockread(KGPCFileRec *file, void *buffer, size_t count, long long *actual_read)
{
    if (file == NULL || buffer == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }

    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }

    size_t elem_size = kgpc_tfile_element_size(file, &priv);
    if (count == 0)
    {
        if (actual_read != NULL)
            *actual_read = 0;
        kgpc_ioresult_set(0);
        return 0;
    }

    size_t read_elems = fread(buffer, elem_size, count, priv.handle);
    if (actual_read != NULL)
        *actual_read = (long long)read_elems;

    kgpc_file_private_set(file, &priv);
    if (read_elems == count || feof(priv.handle))
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(priv.handle) ? EIO : 1);
    return 1;
}

int kgpc_tfile_blockwrite(KGPCFileRec *file, const void *buffer, size_t count, long long *actual_written)
{
    if (file == NULL || buffer == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }

    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }

    size_t elem_size = kgpc_tfile_element_size(file, &priv);
    if (count == 0)
    {
        if (actual_written != NULL)
            *actual_written = 0;
        kgpc_ioresult_set(0);
        return 0;
    }

    size_t written = fwrite(buffer, elem_size, count, priv.handle);
    if (actual_written != NULL)
        *actual_written = (long long)written;

    kgpc_file_private_set(file, &priv);
    if (written == count)
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(priv.handle) ? EIO : 1);
    return 1;
}

int kgpc_tfile_seek(KGPCFileRec *file, long long index)
{
    if (file == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }

    size_t elem_size = kgpc_tfile_element_size(file, &priv);
    if (fseeko(priv.handle, (off_t)(elem_size * index), SEEK_SET) != 0)
    {
        kgpc_ioresult_set(errno);
        return 1;
    }
    kgpc_ioresult_set(0);
    return 0;
}

int kgpc_tfile_filepos(KGPCFileRec *file, long long *position)
{
    if (file == NULL || position == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }

    size_t elem_size = kgpc_tfile_element_size(file, &priv);
    off_t offset = ftello(priv.handle);
    if (offset == (off_t)-1)
    {
        kgpc_ioresult_set(errno);
        return 1;
    }
    *position = (long long)(offset / (off_t)elem_size);
    kgpc_ioresult_set(0);
    return 0;
}

int kgpc_tfile_filesize(KGPCFileRec *file, long long *size_out)
{
    if (file == NULL || size_out == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }

    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }

    size_t elem_size = kgpc_tfile_element_size(file, &priv);
    off_t current = ftello(priv.handle);
    if (current == (off_t)-1)
    {
        kgpc_ioresult_set(errno);
        return 1;
    }
    if (fseeko(priv.handle, 0, SEEK_END) != 0)
    {
        kgpc_ioresult_set(errno);
        return 1;
    }

    off_t end_offset = ftello(priv.handle);
    if (end_offset == (off_t)-1)
    {
        kgpc_ioresult_set(errno);
        return 1;
    }
    if (fseeko(priv.handle, current, SEEK_SET) != 0)
    {
        kgpc_ioresult_set(errno);
        return 1;
    }

    *size_out = (long long)(end_offset / (off_t)elem_size);
    kgpc_ioresult_set(0);
    return 0;
}

int kgpc_tfile_truncate(KGPCFileRec *file, long long length)
{
    if (file == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCFilePrivate priv = kgpc_file_private_get(file);
    if (priv.handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }

    size_t elem_size = kgpc_tfile_element_size(file, &priv);
    off_t target = (off_t)(elem_size * length);
    if (fflush(priv.handle) != 0)
    {
        kgpc_ioresult_set(errno);
        return 1;
    }

#ifdef _WIN32
    {
        int fd = _fileno(priv.handle);
        if (fd == -1)
        {
            kgpc_ioresult_set(errno);
            return 1;
        }
        if (_chsize_s(fd, target) != 0)
        {
            kgpc_ioresult_set(errno);
            return 1;
        }
    }
#else
    {
        int fd = fileno(priv.handle);
        if (fd == -1)
        {
            kgpc_ioresult_set(errno);
            return 1;
        }
        if (ftruncate(fd, target) != 0)
        {
            kgpc_ioresult_set(errno);
            return 1;
        }
    }
#endif

    if (fseeko(priv.handle, target, SEEK_SET) != 0)
    {
        kgpc_ioresult_set(errno);
        return 1;
    }
    kgpc_ioresult_set(0);
    return 0;
}

int kgpc_tfile_truncate_current(KGPCFileRec *file)
{
    long long position = 0;
    if (kgpc_tfile_filepos(file, &position) != 0)
        return 1;
    return kgpc_tfile_truncate(file, position);
}
static int kgpc_vprintf_impl(const char *format, va_list args) {
    return vprintf(format, args);
}

static inline double kgpc_bits_to_double(int64_t bits)
{
    double value;
    memcpy(&value, &bits, sizeof(value));
    return value;
}

static inline int64_t kgpc_double_to_bits(double value)
{
    int64_t bits;
    memcpy(&bits, &value, sizeof(bits));
    return bits;
}

void kgpc_store_extended_from_bits(void *dest, int64_t bits)
{
    if (dest == NULL)
        return;

    double value = kgpc_bits_to_double(bits);
    long double ext = (long double)value;
    memcpy(dest, &ext, 10);
}

int64_t kgpc_load_extended_to_bits(const void *src)
{
    if (src == NULL)
        return kgpc_double_to_bits(0.0);

    long double ext = 0.0L;
    memcpy(&ext, src, 10);
    return kgpc_double_to_bits((double)ext);
}

void kgpc_store_extended_from_int64(void *dest, int64_t value)
{
    if (dest == NULL)
        return;

    long double ext = (long double)value;
    memcpy(dest, &ext, 10);
}

void kgpc_extended_neg(void *dest, const void *src)
{
    if (dest == NULL || src == NULL)
        return;

    long double value = 0.0L;
    memcpy(&value, src, 10);
    value = -value;
    memcpy(dest, &value, 10);
}

void kgpc_extended_add(void *dest, const void *lhs, const void *rhs)
{
    if (dest == NULL || lhs == NULL || rhs == NULL)
        return;

    long double left = 0.0L, right = 0.0L;
    memcpy(&left, lhs, 10);
    memcpy(&right, rhs, 10);
    left += right;
    memcpy(dest, &left, 10);
}

void kgpc_extended_sub(void *dest, const void *lhs, const void *rhs)
{
    if (dest == NULL || lhs == NULL || rhs == NULL)
        return;

    long double left = 0.0L, right = 0.0L;
    memcpy(&left, lhs, 10);
    memcpy(&right, rhs, 10);
    left -= right;
    memcpy(dest, &left, 10);
}

void kgpc_extended_mul(void *dest, const void *lhs, const void *rhs)
{
    if (dest == NULL || lhs == NULL || rhs == NULL)
        return;

    long double left = 0.0L, right = 0.0L;
    memcpy(&left, lhs, 10);
    memcpy(&right, rhs, 10);
    left *= right;
    memcpy(dest, &left, 10);
}

void kgpc_extended_div(void *dest, const void *lhs, const void *rhs)
{
    if (dest == NULL || lhs == NULL || rhs == NULL)
        return;

    long double left = 0.0L, right = 0.0L;
    memcpy(&left, lhs, 10);
    memcpy(&right, rhs, 10);
    left /= right;
    memcpy(dest, &left, 10);
}

/* Cache extended-precision parses so default Write can preserve StrToFloat precision.
 * This mirrors FPC's behavior where StrToFloat returns Extended on x86_64. */
#define KGPC_REAL_CACHE_SIZE 64
typedef struct
{
    int valid;
    int64_t bits;
    long double ext_value;
} kgpc_real_cache_entry;

static kgpc_real_cache_entry kgpc_real_cache[KGPC_REAL_CACHE_SIZE];
static unsigned kgpc_real_cache_pos = 0;

static void kgpc_real_cache_put(int64_t bits, long double ext_value)
{
    kgpc_real_cache[kgpc_real_cache_pos].valid = 1;
    kgpc_real_cache[kgpc_real_cache_pos].bits = bits;
    kgpc_real_cache[kgpc_real_cache_pos].ext_value = ext_value;
    kgpc_real_cache_pos = (kgpc_real_cache_pos + 1) % KGPC_REAL_CACHE_SIZE;
}

static const char *kgpc_rtti_type_name(const kgpc_class_typeinfo *info)
{
    if (info != NULL && info->class_name != NULL)
        return info->class_name;
    return "<unknown>";
}

int kgpc_rtti_is(const kgpc_class_typeinfo *value_type,
    const kgpc_class_typeinfo *target_type)
{
    if (value_type == NULL || target_type == NULL)
        return 0;

    const kgpc_class_typeinfo *current = value_type;
    while (current != NULL)
    {
        if (current == target_type)
            return 1;
        current = current->parent;
    }
    return 0;
}

void kgpc_rtti_check_cast(const kgpc_class_typeinfo *value_type,
    const kgpc_class_typeinfo *target_type)
{
    if (value_type == NULL)
    {
        fprintf(stderr, "Runtime error: invalid class reference in \"as\" operator.\n");
        abort();
    }
    if (target_type == NULL)
    {
        fprintf(stderr, "Runtime error: missing target class metadata in \"as\" operator.\n");
        abort();
    }
    if (kgpc_rtti_is(value_type, target_type))
        return;

    fprintf(stderr, "Runtime error: cannot cast class %s to %s.\n",
        kgpc_rtti_type_name(value_type), kgpc_rtti_type_name(target_type));
    abort();
}

/* Resolve an interface method for dispatch.  Handles both raw object pointers
 * and adjusted interface pointers (from kgpc_get_interface/Supports).
 * - If self points to an object (first qword is VMT), navigates VMT → interface
 *   table → vtable → method.
 * - If self is already adjusted (first qword is an interface vtable), uses it directly.
 * Returns the method function pointer, or __kgpc_abstract_method_error if not found. */
typedef void (*kgpc_method_ptr)(void);
extern void __kgpc_abstract_method_error(void);
kgpc_method_ptr __kgpc_resolve_intf_method(void *self,
    const void *interface_guid, int method_index)
{
    if (self == NULL)
        return (kgpc_method_ptr)__kgpc_abstract_method_error;

    /* self could be either:
     * a) Raw object pointer: *(void**)self = VMT, VMT[10] = interface table
     * b) Adjusted interface pointer: *(void**)self = interface vtable (flat array)
     * Distinguish by checking VMT signature: VMT has vInstanceSize at offset 0
     * (small positive int) and vInstanceSize2 = -vInstanceSize at offset 8. */
    const void *first_qword = *(const void **)self;
    if (first_qword == NULL)
        return (kgpc_method_ptr)__kgpc_abstract_method_error;

    /* Check if first_qword looks like a VMT (vInstanceSize / vInstanceSize2 pattern) */
    int64_t slot0 = *(const int64_t *)first_qword;
    int64_t slot1 = *((const int64_t *)first_qword + 1);
    int is_vmt = (slot0 > 0 && slot0 < 10000000 && slot1 == -slot0);

    if (is_vmt) {
        /* Case (a): raw object pointer — walk VMT chain looking for matching interface */
        const void *cur_vmt = first_qword;
        while (cur_vmt != NULL) {
            /* vIntfTable at VMT offset 80 */
            const kgpc_interface_table *intf_table =
                *(const kgpc_interface_table * const *)((const char *)cur_vmt + 80);
            if (intf_table != NULL && intf_table->entry_count > 0) {
                for (uint64_t i = 0; i < intf_table->entry_count; i++) {
                    const kgpc_interface_entry *entry = &intf_table->entries[i];
                    if (entry->iid_ref != NULL && interface_guid != NULL) {
                        const void *iid = *(entry->iid_ref);
                        if (iid != NULL && memcmp(iid, interface_guid, 16) == 0) {
                            if (entry->vtable != NULL) {
                                const void **vtable = (const void **)entry->vtable;
                                if (method_index >= 0)
                                    return (kgpc_method_ptr)vtable[method_index];
                            }
                            return (kgpc_method_ptr)__kgpc_abstract_method_error;
                        }
                    }
                }
            }
            /* Walk to parent via vParentRef (offset 16) */
            const void * const *parent_ref =
                *(const void * const * const *)((const char *)cur_vmt + 16);
            if (parent_ref != NULL)
                cur_vmt = *parent_ref;
            else
                cur_vmt = NULL;
        }
    } else {
        /* Case (b): adjusted interface pointer — first_qword is the vtable directly */
        const void **vtable = (const void **)first_qword;
        if (method_index >= 0)
            return (kgpc_method_ptr)vtable[method_index];
    }

    return (kgpc_method_ptr)__kgpc_abstract_method_error;
}

int kgpc_get_interface(const void *self, const void *guid, void **out_intf)
{
    if (self == NULL || guid == NULL || out_intf == NULL)
        return 0;

    /* Walk the VMT chain via vIntfTable (offset 80) and vParentRef (offset 16).
     * This reads the FPC-compatible tinterfacetable layout:
     *   uint64_t EntryCount, then EntryCount * tinterfaceentry (40 bytes each).
     * Each entry's IIDRef is ^pguid: double-deref to get the 16-byte GUID. */
    const void *vmt = *(const void * const *)self;
    if (vmt == NULL)
        return 0;

    const void *cur_vmt = vmt;
    while (cur_vmt != NULL) {
        /* vIntfTable at VMT offset 80 */
        const kgpc_interface_table *intf_table =
            *(const kgpc_interface_table * const *)((const char *)cur_vmt + 80);
        if (intf_table != NULL && intf_table->entry_count > 0) {
            for (uint64_t i = 0; i < intf_table->entry_count; i++) {
                const kgpc_interface_entry *entry = &intf_table->entries[i];
                if (entry->iid_ref != NULL) {
                    const void *iid = *(entry->iid_ref);  /* deref ^pguid to pguid */
                    if (iid != NULL && memcmp(iid, guid, 16) == 0) {
                        *out_intf = (void *)((const char *)self + entry->ioffset);
                        return 1;
                    }
                }
            }
        }
        /* Walk to parent via vParentRef (offset 16): PPVmt, deref to get parent VMT */
        const void * const *parent_ref =
            *(const void * const * const *)((const char *)cur_vmt + 16);
        if (parent_ref != NULL)
            cur_vmt = *parent_ref;
        else
            cur_vmt = NULL;
    }
    return 0;
}

const void *kgpc_class_parent(const void *self)
{
    if (self == NULL)
        return NULL;

    /* self may be a VMT pointer or an instance pointer.
     * TypeInfo is at VMT offset 56 (vTypeInfo slot in FPC VMT layout). */
    const kgpc_class_typeinfo *typeinfo = NULL;

    /* Try treating self as VMT pointer: read TypeInfo at offset 56 */
    const kgpc_class_typeinfo *candidate = *(const kgpc_class_typeinfo * const *)((const char *)self + 56);
    if (candidate != NULL && candidate->vmt == self)
    {
        typeinfo = candidate;
    }
    else
    {
        /* Try treating self as instance pointer: instance[0] = VMT, then VMT[56] = TypeInfo */
        const void *vmt = *(const void *const *)self;
        if (vmt != NULL)
        {
            const kgpc_class_typeinfo *candidate2 = *(const kgpc_class_typeinfo * const *)((const char *)vmt + 56);
            if (candidate2 != NULL && candidate2->vmt == vmt)
                typeinfo = candidate2;
        }
    }

    if (typeinfo != NULL && typeinfo->parent != NULL)
        return typeinfo->parent->vmt;
    return NULL;
}

const char *kgpc_class_name(const void *self)
{
    if (self == NULL)
        return "";

    /* self may be a VMT pointer or an instance pointer.
     * TypeInfo is at VMT offset 56 (vTypeInfo slot in FPC VMT layout). */
    const kgpc_class_typeinfo *typeinfo = NULL;

    /* Try treating self as VMT pointer: read TypeInfo at offset 56 */
    const kgpc_class_typeinfo *candidate = *(const kgpc_class_typeinfo * const *)((const char *)self + 56);
    if (candidate != NULL && candidate->vmt == self)
    {
        typeinfo = candidate;
    }
    else
    {
        /* Try treating self as instance pointer: instance[0] = VMT, then VMT[56] = TypeInfo */
        const void *vmt = *(const void *const *)self;
        if (vmt != NULL)
        {
            const kgpc_class_typeinfo *candidate2 = *(const kgpc_class_typeinfo * const *)((const char *)vmt + 56);
            if (candidate2 != NULL && candidate2->vmt == vmt)
                typeinfo = candidate2;
        }
    }

    if (typeinfo != NULL && typeinfo->class_name != NULL)
        return typeinfo->class_name;
    return "";
}

void kgpc_assert_failed(const char *msg, const char *filename, int line)
{
    if (msg != NULL && msg[0] != '\0')
        fprintf(stderr, "Assertion failed: %s", msg);
    else
        fprintf(stderr, "Assertion failed");
    if (filename != NULL && filename[0] != '\0')
        fprintf(stderr, " (%s, line %d)", filename, line);
    fprintf(stderr, "\n");
    fflush(stderr);
    exit(227);
}

int kgpc_printf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int result = kgpc_vprintf_impl(format, args);
    va_end(args);
    return result;
}

static int kgpc_vscanf_impl(const char *format, va_list args) {
#ifdef _WIN32
    return vscanf(format, args);
#else
    return vscanf(format, args);
#endif
}

int kgpc_scanf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int result = kgpc_vscanf_impl(format, args);
    va_end(args);
    return result;
}

#ifdef _WIN32
int __isoc99_scanf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int result = kgpc_vscanf_impl(format, args);
    va_end(args);
    return result;
}
#endif

/* Non-variadic read functions for proper Windows x64 calling convention */
/* These avoid the issue where variadic arguments must be on stack on Windows */

static inline int kgpc_scanf_result_to_ioresult(int scan_res)
{
    if (scan_res == EOF)
    {
        kgpc_ioresult_set(errno != 0 ? errno : EOF);
    }
    else
    {
        kgpc_ioresult_set(0);
    }
    return scan_res;
}

int kgpc_read_integer(KGPCTextRec *file, int32_t *ptr) {
    FILE *stream = kgpc_text_input_stream(file);
    int res = fscanf(stream, "%d", ptr);
    return kgpc_scanf_result_to_ioresult(res);
}

int kgpc_read_longint(KGPCTextRec *file, int32_t *ptr) {
    FILE *stream = kgpc_text_input_stream(file);
    int res = fscanf(stream, "%" PRId32, ptr);
    return kgpc_scanf_result_to_ioresult(res);
}

int kgpc_read_char(KGPCTextRec *file, char *ptr) {
    FILE *stream = kgpc_text_input_stream(file);
    int res = fscanf(stream, " %c", ptr);
    return kgpc_scanf_result_to_ioresult(res);
}

int kgpc_read_real(KGPCTextRec *file, double *ptr) {
    FILE *stream = kgpc_text_input_stream(file);
    int res = fscanf(stream, "%lf", ptr);
    return kgpc_scanf_result_to_ioresult(res);
}

void print_integer(int n) {
    printf("%d\n", n);
}

int64_t kgpc_real_add(int64_t a_bits, int64_t b_bits)
{
    double a = kgpc_bits_to_double(a_bits);
    double b = kgpc_bits_to_double(b_bits);
    double result = a + b;
    return kgpc_double_to_bits(result);
}

int64_t kgpc_real_sub(int64_t a_bits, int64_t b_bits)
{
    double a = kgpc_bits_to_double(a_bits);
    double b = kgpc_bits_to_double(b_bits);
    double result = a - b;
    return kgpc_double_to_bits(result);
}

int64_t kgpc_real_mul(int64_t a_bits, int64_t b_bits)
{
    double a = kgpc_bits_to_double(a_bits);
    double b = kgpc_bits_to_double(b_bits);
    double result = a * b;
    return kgpc_double_to_bits(result);
}

int64_t kgpc_real_div(int64_t a_bits, int64_t b_bits)
{
    double a = kgpc_bits_to_double(a_bits);
    double b = kgpc_bits_to_double(b_bits);
    double result = a / b;
    return kgpc_double_to_bits(result);
}

int64_t kgpc_real_neg(int64_t value_bits)
{
    double value = kgpc_bits_to_double(value_bits);
    return kgpc_double_to_bits(-value);
}

int kgpc_real_compare(int64_t a_bits, int64_t b_bits)
{
    double a = kgpc_bits_to_double(a_bits);
    double b = kgpc_bits_to_double(b_bits);
    if (a != a || b != b)
        return 0;
    if (a < b)
        return -1;
    if (a > b)
        return 1;
    if (a == b)
        return 0;
    return 1;
}

uint64_t kgpc_get_tick_count64(void) {
#ifdef _WIN32
    return (uint64_t) GetTickCount64();
#else
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
        return 0;
    }
    return (uint64_t)ts.tv_sec * 1000ULL + (uint64_t)(ts.tv_nsec / 1000000ULL);
#endif
}



void kgpc_sleep_ms(int milliseconds) {
    if (milliseconds <= 0)
        return;

#ifdef _WIN32
    Sleep((DWORD)milliseconds);
#else
    struct timespec req;
    req.tv_sec = milliseconds / 1000;
    req.tv_nsec = (long)(milliseconds % 1000) * 1000000L;

    while (nanosleep(&req, &req) == -1 && errno == EINTR) {
        /* Retry until sleep completes */
    }
#endif
}

void kgpc_sleep_ms_i(int milliseconds) {
    kgpc_sleep_ms(milliseconds);
}

/* High-resolution performance counter wrappers */
uint64_t kgpc_query_performance_counter(void) {
#ifdef _WIN32
    LARGE_INTEGER val;
    if (QueryPerformanceCounter(&val)) {
        return (uint64_t)val.QuadPart;
    }
    return 0ULL;
#else
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) return 0ULL;
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
#endif
}

uint64_t kgpc_query_performance_frequency(void) {
#ifdef _WIN32
    LARGE_INTEGER freq;
    if (QueryPerformanceFrequency(&freq)) {
        return (uint64_t)freq.QuadPart;
    }
    return 0ULL;
#else
    /* CLOCK_MONOTONIC is in nanoseconds on POSIX fallback */
    return 1000000000ULL;
#endif
}

int kgpc_is_debugger_present(void) {
#ifdef _WIN32
    return IsDebuggerPresent() ? 1 : 0;
#else
    return 0;
#endif
}

uint32_t kgpc_getpid(void) {
#ifdef _WIN32
    return (uint32_t)GetCurrentProcessId();
#else
    return (uint32_t)getpid();
#endif
}

int32_t kgpc_setenv(const char *name, const char *value) {
    if (name == NULL) return -1;
#ifdef _WIN32
    if (value == NULL)
        return _putenv_s(name, "") == 0 ? 0 : -1;
    return _putenv_s(name, value) == 0 ? 0 : -1;
#else
    if (value == NULL)
        return unsetenv(name);
    return setenv(name, value, 1);
#endif
}

static int kgpc_normalize_crt_color(int color)
{
    int normalized = color % 16;
    if (normalized < 0)
        normalized += 16;
    return normalized;
}

static const int kgpc_crt_ansi_fg[8] = {30, 34, 32, 36, 31, 35, 33, 37};
void kgpc_clrscr(void)
{
#ifdef _WIN32
    HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if (handle != INVALID_HANDLE_VALUE)
    {
        CONSOLE_SCREEN_BUFFER_INFO csbi;
        if (GetConsoleScreenBufferInfo(handle, &csbi))
        {
            DWORD cell_count = (DWORD)csbi.dwSize.X * (DWORD)csbi.dwSize.Y;
            DWORD written = 0;
            COORD origin = {0, 0};

            FillConsoleOutputCharacterA(handle, ' ', cell_count, origin, &written);
            FillConsoleOutputAttribute(handle, csbi.wAttributes, cell_count, origin, &written);
            SetConsoleCursorPosition(handle, origin);
            return;
        }
    }
#endif
    fputs("\033[H\033[m\033[H\033[2J", stdout);
    fflush(stdout);
}

void kgpc_textcolor(int color)
{
    int normalized = kgpc_normalize_crt_color(color);
    const int base_code = kgpc_crt_ansi_fg[normalized % 8];
    char buf[32];

#ifdef _WIN32
    HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if (handle != INVALID_HANDLE_VALUE)
    {
        CONSOLE_SCREEN_BUFFER_INFO csbi;
        WORD attributes = (WORD)normalized;
        if (GetConsoleScreenBufferInfo(handle, &csbi))
            attributes = (csbi.wAttributes & ~(WORD)0x000F) | (WORD)normalized;

        if (SetConsoleTextAttribute(handle, attributes))
            return;
    }
#endif

    if (normalized == 0)
        snprintf(buf, sizeof(buf), "\033[%dm", base_code);
    else if (normalized == 7)
        snprintf(buf, sizeof(buf), "\033[m");
    else if (normalized == 8)
        snprintf(buf, sizeof(buf), "\033[1;%dm", base_code);
    else if (normalized == 15)
        snprintf(buf, sizeof(buf), "\033[0;1m");
    else if (normalized >= 9 && normalized <= 14)
        snprintf(buf, sizeof(buf), "\033[0;1;%dm", base_code);
    else
        snprintf(buf, sizeof(buf), "\033[0;%dm", base_code);

    fputs(buf, stdout);
    fflush(stdout);
}

int kgpc_crt_screen_width(void)
{
#ifdef _WIN32
    HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if (handle != INVALID_HANDLE_VALUE)
    {
        CONSOLE_SCREEN_BUFFER_INFO csbi;
        if (GetConsoleScreenBufferInfo(handle, &csbi))
            return (int)(csbi.srWindow.Right - csbi.srWindow.Left + 1);
    }
    return 80;
#else
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0)
        return (int)ws.ws_col;
    return 80;
#endif
}

int kgpc_crt_screen_height(void)
{
#ifdef _WIN32
    HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if (handle != INVALID_HANDLE_VALUE)
    {
        CONSOLE_SCREEN_BUFFER_INFO csbi;
        if (GetConsoleScreenBufferInfo(handle, &csbi))
            return (int)(csbi.srWindow.Bottom - csbi.srWindow.Top + 1);
    }
    return 25;
#else
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_row > 0)
        return (int)ws.ws_row;
    return 25;
#endif
}

int kgpc_crt_stdout_is_tty(void)
{
#ifdef _WIN32
    return _isatty(_fileno(stdout)) ? 1 : 0;
#else
    return isatty(STDOUT_FILENO) ? 1 : 0;
#endif
}

#ifndef _WIN32
static int kgpc_keyboard_initialized = 0;
static struct termios kgpc_keyboard_saved_termios;

static void kgpc_keyboard_restore_terminal(void)
{
    if (!kgpc_keyboard_initialized)
        return;
    tcsetattr(STDIN_FILENO, TCSANOW, &kgpc_keyboard_saved_termios);
}

static void kgpc_keyboard_init_once(void)
{
    if (kgpc_keyboard_initialized)
        return;
    if (tcgetattr(STDIN_FILENO, &kgpc_keyboard_saved_termios) != 0)
        return;

    struct termios raw = kgpc_keyboard_saved_termios;
    /* Crt.ReadKey must receive control characters like Ctrl+C as input bytes
     * instead of letting the terminal driver turn them into signals. */
    raw.c_lflag &= (tcflag_t) ~(ICANON | ECHO | ISIG);
    raw.c_iflag &= (tcflag_t) ~(IXON | ICRNL);
    raw.c_cc[VMIN] = 1;
    raw.c_cc[VTIME] = 0;

    if (tcsetattr(STDIN_FILENO, TCSANOW, &raw) != 0)
        return;

    kgpc_keyboard_initialized = 1;
    atexit(kgpc_keyboard_restore_terminal);
}
#endif

static unsigned char kgpc_keybuf[64];
static int kgpc_keybuf_head = 0;
static int kgpc_keybuf_tail = 0;

static int kgpc_keybuf_is_empty(void)
{
    return kgpc_keybuf_head == kgpc_keybuf_tail;
}

static int kgpc_keybuf_is_full(void)
{
    return ((kgpc_keybuf_tail + 1) % (int)sizeof(kgpc_keybuf)) == kgpc_keybuf_head;
}

static int kgpc_keybuf_count(void)
{
    if (kgpc_keybuf_tail >= kgpc_keybuf_head)
        return kgpc_keybuf_tail - kgpc_keybuf_head;
    return (int)sizeof(kgpc_keybuf) - kgpc_keybuf_head + kgpc_keybuf_tail;
}

static int kgpc_keybuf_peek_at(int offset)
{
    if (kgpc_keybuf_is_empty())
        return 0;
    int count = kgpc_keybuf_count();
    if (offset < 0 || offset >= count)
        return 0;
    int idx = (kgpc_keybuf_head + offset) % (int)sizeof(kgpc_keybuf);
    return (int)kgpc_keybuf[idx];
}

static void kgpc_keybuf_push(unsigned char value)
{
    if (kgpc_keybuf_is_full())
        return;
    kgpc_keybuf[kgpc_keybuf_tail] = value;
    kgpc_keybuf_tail = (kgpc_keybuf_tail + 1) % (int)sizeof(kgpc_keybuf);
}

static int kgpc_keybuf_peek(void)
{
    if (kgpc_keybuf_is_empty())
        return 0;
    return (int)kgpc_keybuf[kgpc_keybuf_head];
}

static int kgpc_keybuf_pop(void)
{
    if (kgpc_keybuf_is_empty())
        return 0;
    int value = (int)kgpc_keybuf[kgpc_keybuf_head];
    kgpc_keybuf_head = (kgpc_keybuf_head + 1) % (int)sizeof(kgpc_keybuf);
    return value;
}

static void kgpc_keyboard_decode_escape_sequences(void)
{
    for (;;)
    {
        if (kgpc_keybuf_count() < 3)
            return;
        int a = kgpc_keybuf_peek_at(0);
        int b = kgpc_keybuf_peek_at(1);
        int c = kgpc_keybuf_peek_at(2);
        if (a != 27 || b != '[')
            return;

        int mapped = -1;
        switch (c)
        {
            case 'A': mapped = 72; break; /* Up */
            case 'B': mapped = 80; break; /* Down */
            case 'C': mapped = 77; break; /* Right */
            case 'D': mapped = 75; break; /* Left */
            default: break;
        }

        if (mapped == -1)
            /* Leave the raw ESC sequence intact when we don't recognize it. */
            return;

        /* Consume the escape sequence */
        (void)kgpc_keybuf_pop();
        (void)kgpc_keybuf_pop();
        (void)kgpc_keybuf_pop();

        /* Preserve original ordering: inject 0,<mapped> ahead of remaining bytes. */
        unsigned char remainder[sizeof(kgpc_keybuf)];
        int remainder_len = 0;
        while (!kgpc_keybuf_is_empty() && remainder_len < (int)sizeof(remainder))
        {
            remainder[remainder_len++] = (unsigned char)kgpc_keybuf_pop();
        }

        kgpc_keybuf_push(0);
        kgpc_keybuf_push((unsigned char)mapped);
        for (int i = 0; i < remainder_len; ++i)
            kgpc_keybuf_push(remainder[i]);
    }
}

static void kgpc_keyboard_fill_nonblocking(void)
{
#ifdef _WIN32
    while (!_kbhit())
        break;
    while (_kbhit())
    {
        int ch = _getch();
        if (ch >= 0 && ch <= 255)
            kgpc_keybuf_push((unsigned char)ch);
        else
            break;
    }
#else
    kgpc_keyboard_init_once();
    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(STDIN_FILENO, &rfds);
    struct timeval tv;
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    int ready = select(STDIN_FILENO + 1, &rfds, NULL, NULL, &tv);
    if (ready <= 0 || !FD_ISSET(STDIN_FILENO, &rfds))
        return;

    int available = 0;
    if (ioctl(STDIN_FILENO, FIONREAD, &available) == 0 && available > 0)
    {
        while (available > 0 && !kgpc_keybuf_is_full())
        {
            unsigned char c = 0;
            ssize_t got = read(STDIN_FILENO, &c, 1);
            if (got == 1)
                kgpc_keybuf_push(c);
            else
                break;
            --available;
        }
        return;
    }

    int saved_flags = fcntl(STDIN_FILENO, F_GETFL, 0);
    if (saved_flags != -1)
        (void)fcntl(STDIN_FILENO, F_SETFL, saved_flags | O_NONBLOCK);

    while (!kgpc_keybuf_is_full())
    {
        unsigned char c = 0;
        ssize_t got = read(STDIN_FILENO, &c, 1);
        if (got == 1)
        {
            kgpc_keybuf_push(c);
            continue;
        }
        if (got == -1 && (errno == EAGAIN || errno == EWOULDBLOCK))
            break;
        break;
    }

    if (saved_flags != -1)
        (void)fcntl(STDIN_FILENO, F_SETFL, saved_flags);
#endif
    kgpc_keyboard_decode_escape_sequences();
}

static void kgpc_keyboard_try_extend_escape(int allow_blocking)
{
#ifndef _WIN32
    if (kgpc_keybuf_count() == 1 && kgpc_keybuf_peek_at(0) == 27)
    {
        fd_set rfds;
        FD_ZERO(&rfds);
        FD_SET(STDIN_FILENO, &rfds);
        struct timeval tv;
        tv.tv_sec = 0;
        tv.tv_usec = allow_blocking ? 10000 : 0;
        int ready = select(STDIN_FILENO + 1, &rfds, NULL, NULL, &tv);
        if (ready > 0 && FD_ISSET(STDIN_FILENO, &rfds))
        {
            unsigned char extra[2];
            ssize_t got = read(STDIN_FILENO, extra, sizeof(extra));
            if (got > 0)
            {
                for (ssize_t i = 0; i < got; ++i)
                    kgpc_keybuf_push(extra[i]);
            }
        }
    }
#endif
}

int kgpc_keyboard_poll(void)
{
    kgpc_keyboard_fill_nonblocking();
    kgpc_keyboard_try_extend_escape(0);
    kgpc_keyboard_decode_escape_sequences();
    return kgpc_keybuf_peek();
}

int kgpc_keyboard_get(void)
{
    kgpc_keyboard_fill_nonblocking();
    kgpc_keyboard_try_extend_escape(0);
    kgpc_keyboard_decode_escape_sequences();
    return kgpc_keybuf_pop();
}

int kgpc_keyboard_read_char(void)
{
#ifdef _WIN32
    int ch = kgpc_keyboard_get();
    if (ch != 0)
        return ch;
    return _getch();
#else
    kgpc_keyboard_init_once();

    for (;;)
    {
        kgpc_keyboard_fill_nonblocking();
        kgpc_keyboard_try_extend_escape(1);
        kgpc_keyboard_decode_escape_sequences();

        int buffered = kgpc_keybuf_count();
        if (buffered == 0 || (kgpc_keybuf_peek_at(0) == 27 && buffered < 3))
        {
            unsigned char c = 0;
            ssize_t got = read(STDIN_FILENO, &c, 1);
            if (got == 1)
            {
                kgpc_keybuf_push(c);
                continue;
            }
            return -1;
        }

        int ch = kgpc_keybuf_pop();
        return ch;
    }
#endif
}

/* 
 * Dynamic Array ABI Design (Model A - Embedded Descriptor)
 * =========================================================
 * A dynarray variable (or field) stores a kgpc_dynarray_descriptor_t BY VALUE.
 * The descriptor contains:
 *   - data: pointer to the element buffer (allocated with malloc/realloc)
 *   - length: logical length/capacity of the array
 * 
 * The descriptor itself lives:
 *   - in .bss (global/static variables)
 *   - on the stack (local variables)
 *   - inside a record/class (fields)
 * 
 * Runtime functions receive a POINTER to the descriptor (not the descriptor itself).
 * 
 * TFPGList Layout:
 *   Offset 0-7:   TypeInfo pointer (class RTTI)
 *   Offset 8-23:  FItems (kgpc_dynarray_descriptor_t - data + length)
 *                   8-15:  FItems.data (pointer to elements)
 *                   16-23: FItems.length (capacity)
 *   Offset 24-31: FCount (Int64 - number of valid elements)
 */
typedef struct {
    void *data;
    int64_t length;
} kgpc_dynarray_descriptor_t;

/*
 * Get the length of a dynamic array.
 * descriptor_ptr: pointer to a kgpc_dynarray_descriptor_t
 * Returns: length field, or 0 if descriptor is NULL
 */
int64_t __kgpc_dynarray_length(void *descriptor_ptr)
{
    if (descriptor_ptr == NULL)
        return 0;
    
    kgpc_dynarray_descriptor_t *desc = (kgpc_dynarray_descriptor_t *)descriptor_ptr;
    return desc->length;
}

/*
 * Set the length of a dynamic array.
 * descriptor_ptr: pointer to a kgpc_dynarray_descriptor_t
 * new_length: desired new length
 * element_size: size of each element in bytes
 */
void kgpc_dynarray_setlength(void *descriptor_ptr, int64_t new_length, int64_t element_size)
{
    if (descriptor_ptr == NULL || element_size <= 0)
        return;

    if (new_length < 0)
        new_length = 0;

    kgpc_dynarray_descriptor_t *descriptor = (kgpc_dynarray_descriptor_t *)descriptor_ptr;
    size_t old_length = descriptor->length > 0 ? (size_t)descriptor->length : 0;
    size_t target_length = (size_t)new_length;

    size_t alloc_length = target_length;
    if (alloc_length < SIZE_MAX)
        alloc_length += 1; /* Provide a spare slot to tolerate off-by-one accesses. */

    size_t new_size = alloc_length * (size_t)element_size;
    if (new_size == 0)
    {
        free(descriptor->data);
        descriptor->data = NULL;
        descriptor->length = 0;
        return;
    }

    void *new_data = realloc(descriptor->data, new_size);
    if (new_data == NULL)
    {
        /* Allocation failure leaves the array unchanged. */
        return;
    }

    if (target_length > old_length)
    {
        size_t old_bytes = old_length * (size_t)element_size;
        size_t new_bytes = target_length * (size_t)element_size;
        if (new_bytes > old_bytes)
            memset((char *)new_data + old_bytes, 0, new_bytes - old_bytes);
    }

    descriptor->data = new_data;
    descriptor->length = new_length;
}

static FILE *kgpc_get_write_stream(KGPCTextRec *file, int *width)
{
    FILE *dest = kgpc_text_output_stream(file);
    if (dest == NULL)
        return NULL;

    if (*width > 1024 || *width < -1024)
        *width = 0;
    if (*width == -1)
        *width = 0;
    return dest;
}

static void kgpc_write_padded_buf(FILE *dest, const char *buf, size_t len, int width)
{
    if (width > 0)
    {
        size_t pad = ((size_t)width > len) ? (size_t)width - len : 0;
        for (size_t i = 0; i < pad; ++i)
            fputc(' ', dest);
        if (len > 0)
            fwrite(buf, 1, len, dest);
    }
    else if (width < 0)
    {
        size_t target = (size_t)(-width);
        if (len > 0)
            fwrite(buf, 1, len, dest);
        size_t pad = (target > len) ? target - len : 0;
        for (size_t i = 0; i < pad; ++i)
            fputc(' ', dest);
    }
    else if (len > 0)
    {
        fwrite(buf, 1, len, dest);
    }
}

void kgpc_write_integer(KGPCTextRec *file, int width, int64_t value)
{
    FILE *dest = kgpc_get_write_stream(file, &width);
    if (dest == NULL)
        return;

    if (width > 0)
        fprintf(dest, "%*lld", width, (long long)value);
    else if (width < 0)
        fprintf(dest, "%-*lld", -width, (long long)value);
    else
        fprintf(dest, "%lld", (long long)value);
    kgpc_flush_text_output_stream(dest);
}

void kgpc_write_unsigned(KGPCTextRec *file, int width, uint64_t value)
{
    FILE *dest = kgpc_get_write_stream(file, &width);
    if (dest == NULL)
        return;

    if (width > 0)
        fprintf(dest, "%*llu", width, (unsigned long long)value);
    else if (width < 0)
        fprintf(dest, "%-*llu", -width, (unsigned long long)value);
    else
        fprintf(dest, "%llu", (unsigned long long)value);
    kgpc_flush_text_output_stream(dest);
}

typedef struct KgpcStringHeader KgpcStringHeader;
static size_t kgpc_string_known_length(const char *value);
static KgpcStringHeader *kgpc_string_header(const char *value);
static void kgpc_string_set_insert(const void *value);
static void kgpc_string_release(char *value);
static void kgpc_default_unicode2ansi_move(const uint16_t *source, char **dest, int32_t cp, int64_t len);
static void kgpc_default_ansi2unicode_move(const char *source, int32_t cp, uint16_t **dest, int64_t len);
extern void *widestringmanager[25];
extern int32_t DefaultSystemCodePage;
int64_t kgpc_widechar_length(const uint16_t *value);

void kgpc_write_string(KGPCTextRec *file, int width, const char *value)
{
    FILE *dest = kgpc_get_write_stream(file, &width);
    if (dest == NULL)
        return;

    size_t len = kgpc_string_known_length(value ? value : "");
    kgpc_write_padded_buf(dest, value ? value : "", len, width);
    kgpc_flush_text_output_stream(dest);
}

/* Write a char array with specified maximum length (for Pascal char arrays) */
void kgpc_write_char_array(KGPCTextRec *file, int width, const char *value, size_t max_len)
{
    FILE *dest = kgpc_get_write_stream(file, &width);
    if (dest == NULL || value == NULL)
        return;

    /* Find the actual length: either max_len or until first null, whichever comes first */
    size_t actual_len = 0;
    while (actual_len < max_len && value[actual_len] != '\0')
        actual_len++;

    kgpc_write_padded_buf(dest, value, actual_len, width);
    kgpc_flush_text_output_stream(dest);
}

/* Write ShortString (Pascal string with length byte at index 0) */
void kgpc_write_shortstring(KGPCTextRec *file, int width, const char *value)
{
    FILE *dest = kgpc_get_write_stream(file, &width);
    if (dest == NULL || value == NULL)
        return;

    /* Read length from index 0 */
    unsigned char len = (unsigned char)value[0];
    /* String data starts at index 1 */
    kgpc_write_padded_buf(dest, value + 1, (size_t)len, width);
    kgpc_flush_text_output_stream(dest);
}

void kgpc_write_newline(KGPCTextRec *file)
{
    FILE *dest = kgpc_text_output_stream(file);
    if (dest == NULL)
        return;

    int fd = fileno(dest);
    if (fd >= 0 && isatty(fd))
    {
        fputc('\r', dest);
        fputc('\n', dest);
    }
    else
    {
        fputc('\n', dest);
    }
    fflush(dest);
}

void kgpc_write_char(KGPCTextRec *file, int width, int value)
{
    unsigned char ch = (unsigned char)value;
    char buffer[2];
    buffer[0] = (char)ch;
    buffer[1] = '\0';
    kgpc_write_string(file, width, buffer);
}

void kgpc_write_boolean(KGPCTextRec *file, int width, int value)
{
    FILE *dest = kgpc_get_write_stream(file, &width);
    if (dest == NULL)
        return;

    const char *text = value ? "TRUE" : "FALSE";
    kgpc_write_padded_buf(dest, text, strlen(text), width);
    kgpc_flush_text_output_stream(dest);
}

void kgpc_write_real(KGPCTextRec *file, int width, int precision, int64_t value_bits)
{
    FILE *dest = kgpc_get_write_stream(file, &width);
    if (dest == NULL)
        return;

    if (precision < -1)
        precision = -1;
    if (precision > 18)
        precision = 18;

    double value = kgpc_bits_to_double(value_bits);

    if (precision < 0)
    {
        const int default_precision = 6;
        if (width > 0)
            fprintf(dest, "%*.*g", width, default_precision, value);
        else if (width < 0)
            fprintf(dest, "%-*.*g", -width, default_precision, value);
        else
            fprintf(dest, "%.*g", default_precision, value);
    }
    else
    {
        if (width > 0)
            fprintf(dest, "%*.*f", width, precision, value);
        else if (width < 0)
            fprintf(dest, "%-*.*f", -width, precision, value);
        else
            fprintf(dest, "%.*f", precision, value);
    }
    kgpc_flush_text_output_stream(dest);
}

void kgpc_raise(int64_t value)
{
    kgpc_current_exception = value;

    /* If there is an active except frame, longjmp to it */
    if (kgpc_except_stack_depth > 0) {
        kgpc_except_stack_depth--;
        longjmp(kgpc_except_stack[kgpc_except_stack_depth].buf, 1);
    }

    /* No except frame — truly unhandled */
    if (value == 0)
        fprintf(stderr, "Unhandled exception raised.\n");
    else
        fprintf(stderr, "Unhandled exception raised with code %lld.\n", (long long)value);
    fflush(stderr);
    exit(EXIT_FAILURE);
}

void kgpc_new(void **target, size_t size)
{
    if (target == NULL)
        return;

    void *memory = calloc(1, size);
    if (memory == NULL)
    {
        fprintf(stderr, "KGPC runtime: failed to allocate %zu bytes.\n", size);
        exit(EXIT_FAILURE);
    }

    *target = memory;
}

void kgpc_dispose(void **target)
{
    if (target == NULL)
        return;

    if (*target != NULL)
    {
        free(*target);
        *target = NULL;
    }
}

void Finalize(void *value)
{
    (void)value;
}

void Initialize(void *value)
{
    (void)value;
}

/* Generic default constructor for classes without explicit constructors */
/* Initialize interface vtable pointer slots in a class instance.
 * For each interface the class implements, the instance has a slot at
 * (instance + ioffset) that must point to the interface vtable.
 * This is called after the VMT pointer is set at offset 0. */
void __kgpc_init_interface_vtables(void *instance)
{
    if (instance == NULL)
        return;
    const void *vmt = *(const void * const *)instance;
    if (vmt == NULL)
        return;
    /* Walk the VMT chain (including parent classes) to init all interface slots */
    const void *cur_vmt = vmt;
    while (cur_vmt != NULL) {
        /* vIntfTable at VMT offset 80 */
        const kgpc_interface_table *intf_table =
            *(const kgpc_interface_table * const *)((const char *)cur_vmt + 80);
        if (intf_table != NULL && intf_table->entry_count > 0) {
            for (uint64_t i = 0; i < intf_table->entry_count; i++) {
                const kgpc_interface_entry *entry = &intf_table->entries[i];
                if (entry->ioffset > 0 && entry->vtable != NULL) {
                    *(const void **)((char *)instance + entry->ioffset) = entry->vtable;
                }
            }
        }
        /* Walk to parent via vParentRef (offset 16) */
        const void * const *parent_ref =
            *(const void * const * const *)((const char *)cur_vmt + 16);
        if (parent_ref != NULL)
            cur_vmt = *parent_ref;
        else
            cur_vmt = NULL;
    }
}

void *__kgpc_default_create(size_t class_size, const void *vmt_ptr)
{
    /* Allocate and zero-initialize the class instance */
    void *instance = calloc(1, class_size);
    if (instance == NULL)
    {
        fprintf(stderr, "KGPC runtime: failed to allocate %zu bytes for class instance.\\n", class_size);
        exit(EXIT_FAILURE);
    }

    /* Set the VMT pointer (first field of the instance) */
    if (vmt_ptr != NULL)
    {
        *(const void **)instance = vmt_ptr;
    }

    /* Initialize interface vtable pointer slots */
    __kgpc_init_interface_vtables(instance);

    return instance;
}

/* FPC-compatible AnsiString header (TAnsiRec on x86-64).
   Compiled FPC RTL code accesses these fields directly via
   PAnsiRec(Pointer(S) - AnsiFirstOff), so the layout MUST match. */
typedef struct KgpcStringHeader
{
    uint16_t codepage;     /* offset -16 from data: TSystemCodePage */
    uint16_t elementsize;  /* offset -14 from data: element size (1 for AnsiChar) */
    int32_t  refcount;     /* offset -12 from data: reference count */
    int64_t  length;       /* offset  -8 from data: SizeInt length */
} KgpcStringHeader;

static void *const KGPC_STRING_TOMBSTONE = (void *)1;
static void **kgpc_string_set_slots = NULL;
static size_t kgpc_string_set_cap = 0;
static size_t kgpc_string_set_count = 0;

static size_t kgpc_hash_ptr(const void *value)
{
    uintptr_t v = (uintptr_t)value;
    v ^= v >> 33;
    v *= UINT64_C(0xff51afd7ed558ccd);
    v ^= v >> 33;
    v *= UINT64_C(0xc4ceb9fe1a85ec53);
    v ^= v >> 33;
    return (size_t)v;
}

static void kgpc_string_set_grow(size_t new_cap)
{
    void **old_slots = kgpc_string_set_slots;
    size_t old_cap = kgpc_string_set_cap;

    kgpc_string_set_slots = (void **)calloc(new_cap, sizeof(void *));
    if (kgpc_string_set_slots == NULL)
    {
        kgpc_string_set_slots = old_slots;
        return;
    }
    kgpc_string_set_cap = new_cap;
    kgpc_string_set_count = 0;

    if (old_slots != NULL)
    {
        size_t mask = new_cap - 1;
        for (size_t i = 0; i < old_cap; i++)
        {
            void *entry = old_slots[i];
            if (entry == NULL || entry == KGPC_STRING_TOMBSTONE)
                continue;
            size_t idx = kgpc_hash_ptr(entry) & mask;
            while (kgpc_string_set_slots[idx] != NULL)
                idx = (idx + 1) & mask;
            kgpc_string_set_slots[idx] = entry;
            kgpc_string_set_count += 1;
        }
        free(old_slots);
    }
}

static void kgpc_string_set_insert(const void *value)
{
    if (value == NULL || value == KGPC_STRING_TOMBSTONE)
        return;
    for (;;)
    {
        if (kgpc_string_set_cap == 0)
            kgpc_string_set_grow(1024);
        if (kgpc_string_set_cap == 0)
            return;
        if ((kgpc_string_set_count + 1) * 10 >= kgpc_string_set_cap * 7)
            kgpc_string_set_grow(kgpc_string_set_cap * 2);
        if (kgpc_string_set_cap == 0)
            return;

        size_t mask = kgpc_string_set_cap - 1;
        size_t idx = kgpc_hash_ptr(value) & mask;
        size_t first_tombstone = (size_t)-1;
        for (size_t probe = 0; probe < kgpc_string_set_cap; probe++)
        {
            void *entry = kgpc_string_set_slots[idx];
            if (entry == NULL)
            {
                if (first_tombstone != (size_t)-1)
                    idx = first_tombstone;
                kgpc_string_set_slots[idx] = (void *)value;
                kgpc_string_set_count += 1;
                return;
            }
            if (entry == value)
                return;
            if (entry == KGPC_STRING_TOMBSTONE && first_tombstone == (size_t)-1)
                first_tombstone = idx;
            idx = (idx + 1) & mask;
        }

        if (first_tombstone != (size_t)-1)
        {
            kgpc_string_set_slots[first_tombstone] = (void *)value;
            kgpc_string_set_count += 1;
            return;
        }

        kgpc_string_set_grow(kgpc_string_set_cap * 2);
    }
}

static void kgpc_string_set_remove(const void *value)
{
    if (value == NULL || kgpc_string_set_cap == 0)
        return;
    size_t mask = kgpc_string_set_cap - 1;
    size_t idx = kgpc_hash_ptr(value) & mask;
    for (size_t probe = 0; probe < kgpc_string_set_cap; probe++)
    {
        void *entry = kgpc_string_set_slots[idx];
        if (entry == NULL)
            return;
        if (entry == value)
        {
            kgpc_string_set_slots[idx] = KGPC_STRING_TOMBSTONE;
            if (kgpc_string_set_count > 0)
                kgpc_string_set_count -= 1;
            if (kgpc_string_set_count == 0)
                memset(kgpc_string_set_slots, 0, kgpc_string_set_cap * sizeof(void *));
            return;
        }
        idx = (idx + 1) & mask;
    }
}

static int kgpc_string_is_managed(const char *value)
{
    if (value == NULL)
        return 0;
    if (value == kgpc_alloc_empty_string())
        return 1;
    if (kgpc_string_set_cap == 0)
        return 0;
    size_t mask = kgpc_string_set_cap - 1;
    size_t idx = kgpc_hash_ptr(value) & mask;
    for (size_t probe = 0; probe < kgpc_string_set_cap; probe++)
    {
        void *entry = kgpc_string_set_slots[idx];
        if (entry == NULL)
            return 0;
        if (entry == value)
            return 1;
        idx = (idx + 1) & mask;
    }
    return 0;
}

static KgpcStringHeader *kgpc_string_header(const char *value)
{
    if (!kgpc_string_is_managed(value))
        return NULL;
    return (KgpcStringHeader *)((char *)value - (int64_t)sizeof(KgpcStringHeader));
}

static size_t kgpc_string_known_length(const char *value)
{
    if (value == NULL)
        return 0;
    KgpcStringHeader *hdr = kgpc_string_header(value);
    if (hdr != NULL)
        return (size_t)hdr->length;
    return strlen(value);
}

void kgpc_string_assign_take(char **target, char *value);

/* Robust SetCodePage wrapper: accepts either a var RawByteString (by-ref)
 * or a raw string pointer (by-value) to avoid crashes when call sites
 * accidentally pass the value. */
void kgpc_setcodepage_rbs_i_b(void *s_arg, int32_t codepage, int32_t convert)
{
    if (s_arg == NULL)
        return;

    char *str = NULL;
    int by_value = 0;

    /* If s_arg itself looks like a managed string, treat it as by-value. */
    if (kgpc_string_is_managed((char *)s_arg))
    {
        str = (char *)s_arg;
        by_value = 1;
    }
    else
    {
        char **s_ptr = (char **)s_arg;
        str = (s_ptr != NULL) ? *s_ptr : NULL;
    }

    if (str == NULL)
        return;
    if (kgpc_string_known_length(str) == 0)
        return;

    if (!convert || by_value)
    {
        KgpcStringHeader *hdr = kgpc_string_header(str);
        if (hdr != NULL)
            hdr->codepage = (uint16_t)codepage;
        return;
    }

    /* Portable fallback conversion path: keep bytes unchanged and
     * update codepage metadata. This matches FPC behavior for raw
     * byte strings when no concrete conversion backend is linked. */
    KgpcStringHeader *hdr = kgpc_string_header(str);
    if (hdr != NULL)
        hdr->codepage = (uint16_t)codepage;
}

/* 2-arg SetCodePage overload: Convert defaults to True in FPC. */
void kgpc_setcodepage_rbs_i(void *s_arg, int32_t codepage)
{
    kgpc_setcodepage_rbs_i_b(s_arg, codepage, 1);
}

/* FPC RTL compatibility: some bootstrap constants use WideChar literals
 * in PAnsiChar contexts, which KGPC currently lowers via the
 * widechar__op_assign_olevariant_wc symbol. Provide a real conversion
 * by returning a stable, null-terminated single-byte string. */
static char *kgpc_cached_widechar_pchar(uint16_t value)
{
    static char *cache[256] = {0};
    static char *fallback = NULL;

    if (value < 256)
    {
        if (cache[value] == NULL)
        {
            char *buf = (char *)malloc(2);
            if (buf == NULL)
                return kgpc_alloc_empty_string();
            buf[0] = (char)value;
            buf[1] = '\0';
            cache[value] = buf;
        }
        return cache[value];
    }

    if (fallback == NULL)
    {
        fallback = (char *)malloc(2);
        if (fallback == NULL)
            return kgpc_alloc_empty_string();
        fallback[0] = '?';
        fallback[1] = '\0';
    }
    return fallback;
}

char *widechar__op_assign_olevariant_wc(uint16_t value)
{
    return kgpc_cached_widechar_pchar(value);
}

/* olevariant__op_assign_widestring_u is in runtime_olevariant_assign.c
 * (separate .o so linker only pulls it when not defined by compiler code) */

static char *kgpc_string_alloc_with_length(size_t length)
{
    size_t total = sizeof(KgpcStringHeader) + length + 1;
    KgpcStringHeader *hdr = (KgpcStringHeader *)malloc(total);
    if (hdr == NULL)
        return NULL;
    hdr->codepage = 0;      /* CP_ACP / default */
    hdr->elementsize = 1;   /* AnsiChar = 1 byte */
    hdr->refcount = 1;
    hdr->length = (int64_t)length;
    char *data = (char *)(hdr + 1);
    data[length] = '\0';
    kgpc_string_set_insert(data);
    return data;
}

static void kgpc_string_retain(const char *value)
{
    KgpcStringHeader *hdr = kgpc_string_header(value);
    if (hdr == NULL)
        return;
    if (hdr->refcount >= 0)
        hdr->refcount += 1;
}

static void kgpc_string_release(char *value)
{
    KgpcStringHeader *hdr = kgpc_string_header(value);
    if (hdr == NULL)
        return;
    if (hdr->refcount < 0)
        return;
    hdr->refcount -= 1;
    if (hdr->refcount == 0)
    {
        kgpc_string_set_remove(value);
        free(hdr);
    }
}

void FPC_ANSISTR_UNIQUE(char **value)
{
    if (value == NULL || *value == NULL)
        return;
    KgpcStringHeader *hdr = kgpc_string_header(*value);
    if (hdr == NULL)
        return;
    if (hdr->refcount == 1 || hdr->refcount < 0)
        return;
    size_t len = (size_t)hdr->length;
    char *new_str = (char *)malloc(sizeof(KgpcStringHeader) + len + 1);
    if (new_str == NULL)
        return;
    KgpcStringHeader *new_hdr = (KgpcStringHeader *)new_str;
    new_hdr->codepage = hdr->codepage;
    new_hdr->elementsize = hdr->elementsize;
    new_hdr->refcount = 1;
    new_hdr->length = len;
    char *new_data = new_str + sizeof(KgpcStringHeader);
    memcpy(new_data, *value, len + 1);
    kgpc_string_release(*value);
    *value = new_data;
}

char *kgpc_alloc_empty_string(void)
{
    static struct {
        KgpcStringHeader header;
        char data[1];
    } empty = { { 0, 1, -1, 0 }, { '\0' } };
    return empty.data;
}

void kgpc_init_widestringmanager(void);

void kgpc_fpc_init_os_params(int argc, char **argv, char **envp);
void kgpc_fpc_init_thread_manager(void);
void kgpc_fpc_init_fpu(void);

void kgpc_init_args(int argc, char **argv, char **envp)
{
    kgpc_argc = (argc < 0) ? 0 : argc;
    kgpc_argv = argv;
    kgpc_fpc_init_os_params(argc, argv, envp);
    kgpc_fpc_init_thread_manager();
    kgpc_init_widestringmanager();
    kgpc_fpc_init_fpu();
    /* Note: FPC heap init (initthread_u64) is NOT called here.
     * The FPC system unit's own initialization section calls it
     * before any Pascal code that needs heap allocation runs. */
}

int kgpc_param_count(void)
{
    if (kgpc_argc <= 1)
        return 0;
    return kgpc_argc - 1;
}

char *kgpc_param_str(int index)
{
    if (index < 0 || index >= kgpc_argc || kgpc_argv == NULL)
        return kgpc_alloc_empty_string();
    return kgpc_string_duplicate(kgpc_argv[index]);
}

char *kgpc_string_duplicate(const char *value)
{
    if (value == NULL)
        return kgpc_alloc_empty_string();

    KgpcStringHeader *hdr = kgpc_string_header(value);
    if (hdr != NULL)
    {
        kgpc_string_retain(value);
        return (char *)value;
    }

    size_t len = strlen(value);
    char *copy = kgpc_string_alloc_with_length(len);
    if (copy == NULL)
        return kgpc_alloc_empty_string();
    if (len > 0)
        memcpy(copy, value, len);
    return copy;
}

static char *kgpc_string_duplicate_length(const char *value, size_t length)
{
    char *copy = kgpc_string_alloc_with_length(length);
    if (copy == NULL)
        return kgpc_alloc_empty_string();
    if (length > 0 && value != NULL)
        memcpy(copy, value, length);
    return copy;
}

char *kgpc_windows_get_hostname_string(void)
{
#ifdef _WIN32
    char buffer[256];
    DWORD size = (DWORD)sizeof(buffer);
    if (GetComputerNameA(buffer, &size))
    {
        buffer[sizeof(buffer) - 1] = '\0';
        /* Convert to lowercase for consistency with Unix behavior */
        for (size_t i = 0; buffer[i] != '\0'; i++)
        {
            buffer[i] = (char)tolower((unsigned char)buffer[i]);
        }
        return kgpc_string_duplicate(buffer);
    }
    return kgpc_alloc_empty_string();
#else
    return kgpc_alloc_empty_string();
#endif
}

char *kgpc_windows_get_domainname_string(void)
{
#ifdef _WIN32
    char fqdn[256];
    DWORD size = sizeof(fqdn);
    /* Try to get DNS domain name on Windows */
    if (GetComputerNameExA(ComputerNameDnsFullyQualified, fqdn, &size))
    {
        char *dot = strchr(fqdn, '.');
        if (dot != NULL && *(dot + 1) != '\0')
        {
            return kgpc_string_duplicate(dot + 1);
        }
    }
    /* Fallback: try DNS hostname */
    size = sizeof(fqdn);
    if (GetComputerNameExA(ComputerNameDnsHostname, fqdn, &size))
    {
        char *dot = strchr(fqdn, '.');
        if (dot != NULL && *(dot + 1) != '\0')
        {
            return kgpc_string_duplicate(dot + 1);
        }
    }
    return kgpc_alloc_empty_string();
#else
    return kgpc_alloc_empty_string();
#endif
}


void kgpc_string_assign(char **target, const char *value)
{
    if (target == NULL)
        return;

    char *existing = *target;
    if (existing != NULL && value == existing)
        return;

    if (existing != NULL)
        kgpc_string_release(existing);

    if (value == NULL)
    {
        *target = kgpc_alloc_empty_string();
        return;
    }

    KgpcStringHeader *hdr = kgpc_string_header(value);
    if (hdr != NULL)
    {
        kgpc_string_retain(value);
        *target = (char *)value;
        return;
    }

    *target = kgpc_string_duplicate(value);
}

void kgpc_string_assign_take(char **target, char *value)
{
    if (target == NULL)
    {
        if (value != NULL)
            free(value);
        return;
    }

    if (value == NULL)
    {
        if (*target != NULL)
            kgpc_string_release(*target);
        *target = kgpc_alloc_empty_string();
        return;
    }

    if (*target != NULL)
        kgpc_string_release(*target);

    KgpcStringHeader *hdr = kgpc_string_header(value);
    if (hdr != NULL)
    {
        *target = value;
        return;
    }

    size_t len = strlen(value);
    char *copy = kgpc_string_alloc_with_length(len);
    if (copy == NULL)
    {
        *target = kgpc_alloc_empty_string();
        free(value);
        return;
    }
    if (len > 0)
        memcpy(copy, value, len);
    free(value);
    *target = copy;
}

void kgpc_set_codepage_string(char **value, uint16_t codepage, int convert)
{
    (void)value;
    (void)codepage;
    (void)convert;
}

void kgpc_string_setlength(char **target, int64_t new_length)
{
    if (target == NULL)
        return;

    if (new_length < 0)
        new_length = 0;

    size_t requested = (size_t)new_length;
    char *current = *target;
    size_t current_len = kgpc_string_known_length(current);
    if (current_len > requested)
        current_len = requested;

    char *resized = kgpc_string_alloc_with_length(requested);
    if (resized == NULL)
    {
        fprintf(stderr, "KGPC runtime: failed to resize string to %lld bytes.\n",
            (long long)new_length);
        exit(EXIT_FAILURE);
    }

    if (current_len > 0)
        memcpy(resized, current, current_len);
    if (requested > current_len)
        memset(resized + current_len, 0, requested - current_len);

    if (current != NULL)
        kgpc_string_release(current);
    *target = resized;
}

void kgpc_unicodestring_setlength(uint16_t **target, int64_t new_length)
{
    if (target == NULL)
        return;

    if (new_length <= 0)
    {
        if (*target != NULL)
            kgpc_string_release((char *)*target);
        *target = NULL;
        return;
    }

    size_t data_bytes = (size_t)new_length * 2 + 2;
    KgpcStringHeader *hdr = (KgpcStringHeader *)malloc(sizeof(KgpcStringHeader) + data_bytes);
    if (hdr == NULL)
    {
        fprintf(stderr, "KGPC runtime: failed to resize unicode string to %lld chars.\n",
            (long long)new_length);
        exit(EXIT_FAILURE);
    }

    hdr->codepage = 1200;
    hdr->elementsize = 2;
    hdr->refcount = 1;
    hdr->length = new_length;

    uint16_t *data = (uint16_t *)(hdr + 1);
    memset(data, 0, data_bytes);
    kgpc_string_set_insert((char *)data);

    if (*target != NULL)
        kgpc_string_release((char *)*target);
    *target = data;
}

void kgpc_setstring(char **target, const char *buffer, int64_t length)
{
    if (target == NULL)
        return;

    if (buffer == NULL || length <= 0)
    {
        /* Set to empty string */
        char *empty = kgpc_alloc_empty_string();
        char *current = *target;
        if (current != NULL)
            kgpc_string_release(current);
        *target = empty;
        return;
    }

    size_t copy_len = (size_t)length;
    char *result = kgpc_string_alloc_with_length(copy_len);
    if (result == NULL)
    {
        fprintf(stderr, "KGPC runtime: failed to allocate string (%zu bytes including null).\n",
            copy_len + 1);
        exit(EXIT_FAILURE);
    }

    memcpy(result, buffer, copy_len);
    char *current = *target;
    if (current != NULL)
        kgpc_string_release(current);
    *target = result;
}

static int64_t kgpc_unicode_known_length(const uint16_t *value)
{
    if (value == NULL)
        return 0;
    KgpcStringHeader *hdr = kgpc_string_header((const char *)value);
    if (hdr != NULL)
        return hdr->length;
    return kgpc_widechar_length(value);
}

static uint16_t *kgpc_alloc_empty_unicodestring(void)
{
    static struct {
        KgpcStringHeader header;
        uint16_t data[1];
    } empty = { { 1200, 2, -1, 0 }, { 0 } };
    return empty.data;
}

void kgpc_setstring_unicode(uint16_t **target, const uint16_t *buffer, int64_t length)
{
    if (target == NULL)
        return;

    if (buffer == NULL || length <= 0)
    {
        uint16_t *empty = kgpc_alloc_empty_unicodestring();
        uint16_t *current = *target;
        if (current != NULL)
            kgpc_string_release((char *)current);
        *target = empty;
        return;
    }

    size_t data_bytes = (size_t)length * 2 + 2;
    KgpcStringHeader *hdr = (KgpcStringHeader *)malloc(sizeof(KgpcStringHeader) + data_bytes);
    if (hdr == NULL)
    {
        fprintf(stderr, "KGPC runtime: failed to allocate UnicodeString (%zu bytes).\n", data_bytes);
        exit(EXIT_FAILURE);
    }
    hdr->codepage = 1200;       /* UTF-16LE */
    hdr->elementsize = 2;       /* UnicodeChar = 2 bytes */
    hdr->refcount = 1;
    hdr->length = length;
    uint16_t *data = (uint16_t *)(hdr + 1);
    memcpy(data, buffer, (size_t)length * 2);
    data[length] = 0;

    kgpc_string_set_insert((char *)data);

    uint16_t *current = *target;
    if (current != NULL)
        kgpc_string_release((char *)current);
    *target = data;
}

void kgpc_write_unicodestring(KGPCTextRec *file, int width, const uint16_t *value)
{
    FILE *dest = kgpc_get_write_stream(file, &width);
    if (dest == NULL)
        return;

    if (value == NULL)
        value = kgpc_alloc_empty_unicodestring();

    /* KGPC may still carry "unicode" values in single-byte managed-string
     * storage (elementsize=1). In that case, print through string writer
     * instead of interpreting bytes as UTF-16 code units. */
    {
        KgpcStringHeader *hdr = kgpc_string_header((const char *)value);
        if (hdr != NULL && hdr->elementsize == 1)
        {
            kgpc_write_string(file, width, (const char *)value);
            return;
        }
    }

    int64_t len = kgpc_unicode_known_length(value);
    if (len <= 0 && width == 0)
        return;

    char *ansi = NULL;
    typedef void (*Unicode2AnsiProc)(const uint16_t *, char **, int32_t, int64_t);
    Unicode2AnsiProc conv = (Unicode2AnsiProc)widestringmanager[19];
    int32_t cp = DefaultSystemCodePage;
    if (conv != NULL)
        conv(value, &ansi, cp, len);
    else
        kgpc_default_unicode2ansi_move(value, &ansi, cp, len);

    kgpc_write_string(file, width, ansi);

    if (ansi != NULL)
        kgpc_string_release(ansi);
}

void kgpc_string_delete(char **target, int64_t index, int64_t count)
{
    if (target == NULL || index <= 0 || count <= 0)
        return;

    char *source = *target;
    size_t length = kgpc_string_known_length(source);
    if (length == 0)
        return;

    if (index > (int64_t)length)
        return;

    size_t start = (size_t)(index - 1);
    size_t remove = (size_t)count;
    if (remove > length - start)
        remove = length - start;

    size_t new_length = length - remove;
    char *result = kgpc_string_alloc_with_length(new_length);
    if (result == NULL)
    {
        fprintf(stderr, "KGPC runtime: failed to delete substring (%lld bytes).\n",
            (long long)remove);
        exit(EXIT_FAILURE);
    }

    if (start > 0)
        memcpy(result, source, start);
    size_t tail = length - start - remove;
    if (tail > 0)
        memcpy(result + start, source + start + remove, tail);
    if (source != NULL)
        kgpc_string_release(source);
    *target = result;
}

void kgpc_string_insert(const char *value, char **target, int64_t index)
{
    if (target == NULL || value == NULL)
        return;

    size_t insert_len = kgpc_string_known_length(value);
    if (insert_len == 0)
        return;

    char *dest = *target;
    size_t dest_len = kgpc_string_known_length(dest);

    if (index <= 0)
        index = 1;
    if (index > (int64_t)dest_len + 1)
        index = (int64_t)dest_len + 1;

    size_t pos = (size_t)(index - 1);
    size_t new_len = dest_len + insert_len;

    char *result = kgpc_string_alloc_with_length(new_len);
    if (result == NULL)
    {
        fprintf(stderr, "KGPC runtime: failed to insert substring (%zu bytes).\n",
            insert_len);
        exit(EXIT_FAILURE);
    }

    if (pos > 0 && dest != NULL)
        memcpy(result, dest, pos);
    if (insert_len > 0)
        memcpy(result + pos, value, insert_len);
    if (dest != NULL && pos < dest_len)
        memcpy(result + pos + insert_len, dest + pos, dest_len - pos);
    if (dest != NULL)
        kgpc_string_release(dest);
    *target = result;
}

void *kgpc_dynarray_clone_descriptor(const void *descriptor, size_t descriptor_size)
{
    if (descriptor_size == 0)
        descriptor_size = sizeof(kgpc_dynarray_descriptor_t);

    void *temp = malloc(descriptor_size);
    if (temp == NULL)
        return NULL;

    if (descriptor != NULL)
        memcpy(temp, descriptor, descriptor_size);
    else
        memset(temp, 0, descriptor_size);

    return temp;
}

void kgpc_dynarray_assign_descriptor(void *dest_descriptor, const void *src_descriptor,
    size_t descriptor_size)
{
    if (dest_descriptor == NULL || src_descriptor == NULL)
        return;
    
    if (descriptor_size == 0)
        descriptor_size = sizeof(kgpc_dynarray_descriptor_t);

    memcpy(dest_descriptor, src_descriptor, descriptor_size);
}

void kgpc_dynarray_assign_from_temp(void *dest_descriptor, void *temp_descriptor,
    size_t descriptor_size)
{
    if (dest_descriptor == NULL)
    {
        if (temp_descriptor != NULL)
            free(temp_descriptor);
        return;
    }

    if (descriptor_size == 0)
        descriptor_size = sizeof(kgpc_dynarray_descriptor_t);

    if (temp_descriptor == NULL)
    {
        memset(dest_descriptor, 0, descriptor_size);
        return;
    }

    memcpy(dest_descriptor, temp_descriptor, descriptor_size);
    free(temp_descriptor);
}

long long kgpc_dynarray_compute_high(const void *descriptor_ptr, long long lower_bound)
{
    if (descriptor_ptr == NULL)
        return lower_bound - 1;

    const kgpc_dynarray_descriptor_t *descriptor =
        (const kgpc_dynarray_descriptor_t *)descriptor_ptr;
    long long length = descriptor->length;
    if (length <= 0)
        return lower_bound - 1;

    return lower_bound + length - 1;
}

char **kgpc_array_string_to_ppchar(const void *descriptor_ptr, long long reserve_entries)
{
    if (descriptor_ptr == NULL)
        return NULL;

    const kgpc_dynarray_descriptor_t *descriptor =
        (const kgpc_dynarray_descriptor_t *)descriptor_ptr;

    long long length = descriptor->length;
    if (length <= 0)
        return NULL;

    char **data = (char **)descriptor->data;

    /* Allocate: reserve_entries + length + 1 (null terminator) */
    long long total = reserve_entries + length + 1;
    char **result = (char **)malloc(total * sizeof(char *));
    if (result == NULL)
        return NULL;

    /* Fill reserved slots with NULL */
    for (long long i = 0; i < reserve_entries; i++)
        result[i] = NULL;

    /* Copy string pointers from the dynamic array */
    for (long long i = 0; i < length; i++)
        result[reserve_entries + i] = data[i];

    /* Null-terminate */
    result[reserve_entries + length] = NULL;

    return result;
}

/* Copy a string literal to a char array (fixed-size buffer)
 * Fills the entire array. If the string is shorter, pads with nulls.
 * If the string is longer, truncates to fit.
 * Note: May NOT be null-terminated if string exactly fills the array!
 */
void kgpc_string_to_char_array(char *dest, const char *src, size_t dest_size)
{
    if (dest == NULL || src == NULL || dest_size == 0)
        return;
    
    size_t src_len = kgpc_string_known_length(src);
    size_t copy_len = (src_len < dest_size) ? src_len : dest_size;
    
    memcpy(dest, src, copy_len);
    
    /* Pad remaining space with zeros if string is shorter than array */
    if (copy_len < dest_size)
        memset(dest + copy_len, 0, dest_size - copy_len);
}

/* Copy string to ShortString (Pascal string with length byte at index 0) */
void kgpc_string_to_shortstring(char *dest, const char *src, size_t dest_size)
{
    if (dest == NULL || src == NULL || dest_size < 2)
        return;
    
    size_t src_len = kgpc_string_known_length(src);
    /* ShortString max capacity is 255 chars (indices 1..255) */
    size_t max_chars = (dest_size - 1 < 255) ? (dest_size - 1) : 255;
    size_t copy_len = (src_len < max_chars) ? src_len : max_chars;
    
    /* Set length byte at index 0 */
    dest[0] = (char)copy_len;
    
    /* Copy characters starting at index 1 */
    memcpy(dest + 1, src, copy_len);
    
    /* Pad remaining space with zeros */
    if (copy_len + 1 < dest_size)
        memset(dest + 1 + copy_len, 0, dest_size - 1 - copy_len);
}

void kgpc_char_array_to_shortstring(char *dest, const char *src, size_t src_len, size_t dest_size)
{
    if (dest == NULL || src == NULL || dest_size < 2)
        return;

    size_t max_chars = (dest_size - 1 < 255) ? (dest_size - 1) : 255;
    size_t copy_len = (src_len < max_chars) ? src_len : max_chars;

    dest[0] = (char)copy_len;
    if (copy_len > 0)
        memcpy(dest + 1, src, copy_len);
    if (copy_len + 1 < dest_size)
        memset(dest + 1 + copy_len, 0, dest_size - 1 - copy_len);
}

void kgpc_shortstring_to_char_array(char *dest, const char *src, size_t dest_size)
{
    if (dest == NULL || src == NULL || dest_size == 0)
        return;

    unsigned char src_len = (unsigned char)src[0];
    size_t copy_len = (src_len < dest_size) ? src_len : dest_size;

    if (copy_len > 0)
        memcpy(dest, src + 1, copy_len);
    if (copy_len < dest_size)
        memset(dest + copy_len, 0, dest_size - copy_len);
}

void kgpc_shortstring_to_shortstring(char *dest, size_t dest_size, const char *src)
{
    if (dest == NULL || src == NULL || dest_size < 2)
        return;

    unsigned char src_len = (unsigned char)src[0];
    size_t max_chars = (dest_size - 1 < 255) ? (dest_size - 1) : 255;
    size_t copy_len = (src_len < max_chars) ? src_len : max_chars;

    dest[0] = (char)copy_len;
    if (copy_len > 0)
        memcpy(dest + 1, src + 1, copy_len);
    if (copy_len + 1 < dest_size)
        memset(dest + 1 + copy_len, 0, dest_size - 1 - copy_len);
}

void kgpc_shortstring_setlength(char *target, int64_t new_length)
{
    if (target == NULL)
        return;

    if (new_length < 0)
        new_length = 0;
    if (new_length > 255)
        new_length = 255;

    unsigned char old_len = (unsigned char)target[0];
    unsigned char new_len = (unsigned char)new_length;
    target[0] = (char)new_len;

    if (new_len > old_len)
        memset(target + 1 + old_len, 0, (size_t)(new_len - old_len));
}

void kgpc_shortstring_setstring(char *target, const char *buffer, int64_t length)
{
    if (target == NULL)
        return;

    if (buffer == NULL || length <= 0)
    {
        target[0] = 0;
        return;
    }

    if (length > 255)
        length = 255;

    target[0] = (char)length;
    memcpy(target + 1, buffer, (size_t)length);
    if (length < 255)
        memset(target + 1 + length, 0, (size_t)(255 - length));
}

char *kgpc_shortstring_to_string(const char *value)
{
    if (value == NULL)
        return kgpc_string_duplicate("");

    unsigned char len = (unsigned char)value[0];
    return kgpc_string_duplicate_length(value + 1, len);
}

/* FPC_PCHAR_TO_SHORTSTR: Moved to runtime_fpc_pchar_to_shortstr.c
 * so it lives in its own .o file.  In FPC mode, the compiler provides its own
 * version and this .o is not pulled from the archive.  In non-FPC mode, this
 * .o is pulled to resolve the undefined reference. */

int64_t kgpc_shortstring_length(const char *value)
{
    if (value == NULL)
        return 0;
    return (unsigned char)value[0];
}

void kgpc_shortstring_delete(char *target, int64_t index, int64_t count)
{
    if (target == NULL || index <= 0 || count <= 0)
        return;

    size_t length = (unsigned char)target[0];
    if (length == 0)
        return;
    if (index > (int64_t)length)
        return;

    size_t start = (size_t)(index - 1);
    size_t remove = (size_t)count;
    if (remove > length - start)
        remove = length - start;

    size_t tail = length - start - remove;
    if (tail > 0)
        memmove(target + 1 + start, target + 1 + start + remove, tail);

    size_t new_len = length - remove;
    target[0] = (char)new_len;
    if (remove > 0)
        memset(target + 1 + new_len, 0, remove);
}

void kgpc_shortstring_insert(const char *value, char *target, int64_t index, int value_is_shortstring)
{
    if (target == NULL || value == NULL)
        return;

    const char *insert_ptr = value;
    size_t insert_len = 0;
    if (value_is_shortstring)
    {
        insert_len = (unsigned char)value[0];
        insert_ptr = value + 1;
    }
    else
    {
        insert_len = kgpc_string_known_length(value);
        insert_ptr = value;
    }

    if (insert_len == 0)
        return;

    size_t dest_len = (unsigned char)target[0];
    if (index <= 0)
        index = 1;
    if (index > (int64_t)dest_len + 1)
        index = (int64_t)dest_len + 1;

    size_t max_insert = (dest_len < 255) ? (255 - dest_len) : 0;
    if (insert_len > max_insert)
        insert_len = max_insert;
    if (insert_len == 0)
        return;

    size_t pos = (size_t)(index - 1);
    memmove(target + 1 + pos + insert_len, target + 1 + pos, dest_len - pos);
    memcpy(target + 1 + pos, insert_ptr, insert_len);

    target[0] = (char)(dest_len + insert_len);
}


static void kgpc_text_close_stream(KGPCTextRec *file)
{
    if (file == NULL)
        return;
    FILE *stream = kgpc_textrec_get_stream(file, NULL);
    if (stream == NULL)
        return;
    if (stream == stdin || stream == stdout || stream == stderr)
        return;
    fclose(stream);
    kgpc_textrec_set_stream(file, NULL);
    file->mode = KGPC_FM_CLOSED;
}

static char *kgpc_text_read_line_from_stream(FILE *stream)
{
    if (stream == NULL)
        return NULL;

    char chunk[4096];
    size_t capacity = 256;
    size_t length = 0;
    char *buffer = (char *)malloc(capacity);
    if (buffer == NULL)
        return NULL;

    int read_any = 0;
    while (fgets(chunk, sizeof(chunk), stream) != NULL)
    {
        read_any = 1;
        size_t chunk_len = strlen(chunk);
        char *line_end = strpbrk(chunk, "\r\n");
        size_t copy_len = (line_end != NULL) ? (size_t)(line_end - chunk) : chunk_len;

        if (length + copy_len + 1 > capacity)
        {
            size_t new_capacity = capacity;
            while (length + copy_len + 1 > new_capacity)
            {
                new_capacity = (new_capacity < 1024) ? new_capacity * 2 : new_capacity + 512;
                if (new_capacity <= capacity)
                    new_capacity = capacity + 512;
            }
            char *new_buffer = (char *)realloc(buffer, new_capacity);
            if (new_buffer == NULL)
            {
                free(buffer);
                return NULL;
            }
            buffer = new_buffer;
            capacity = new_capacity;
        }

        if (copy_len > 0)
        {
            memcpy(buffer + length, chunk, copy_len);
            length += copy_len;
        }

        if (line_end != NULL)
        {
            if (*line_end == '\r' && line_end[1] != '\n')
            {
                int next = fgetc(stream);
                if (next != '\n' && next != EOF)
                    ungetc(next, stream);
            }
            break;
        }
    }

    if (!read_any)
    {
        free(buffer);
        return NULL;
    }

    buffer[length] = '\0';
    return buffer;
}

void kgpc_text_assign(KGPCTextRec *file, const char *path)
{
    if (file == NULL)
        return;
    kgpc_text_close_stream(file);
    kgpc_copy_name(file->name, sizeof(file->name), path);
    kgpc_textrec_init_defaults(file);
}

void kgpc_text_rewrite(KGPCTextRec *file)
{
    if (file == NULL)
        return;
    kgpc_text_close_stream(file);
    kgpc_textrec_init_defaults(file);

    FILE *stream = NULL;
    if (file->name[0] == '\0')
        stream = stdout;
    else
        stream = fopen(file->name, "w");
    if (stream != NULL)
    {
        kgpc_textrec_set_stream(file, stream);
        if (file->bufptr != NULL && file->bufsize > 0)
            setvbuf(stream, file->bufptr, _IOFBF, (size_t)file->bufsize);
        file->mode = KGPC_FM_OUTPUT;
        kgpc_ioresult_set(0);
    }
    else
    {
        file->mode = KGPC_FM_CLOSED;
        kgpc_ioresult_set(errno);
    }
}

void kgpc_text_append(KGPCTextRec *file)
{
    if (file == NULL)
        return;
    kgpc_text_close_stream(file);
    kgpc_textrec_init_defaults(file);

    FILE *stream = NULL;
    if (file->name[0] == '\0')
        stream = stdout;
    else
        stream = fopen(file->name, "a");
    if (stream != NULL)
    {
        kgpc_textrec_set_stream(file, stream);
        if (file->bufptr != NULL && file->bufsize > 0)
            setvbuf(stream, file->bufptr, _IOFBF, (size_t)file->bufsize);
        file->mode = KGPC_FM_OUTPUT;
        fseek(stream, 0, SEEK_END);
        kgpc_ioresult_set(0);
    }
    else
    {
        file->mode = KGPC_FM_CLOSED;
        kgpc_ioresult_set(errno);
    }
}

void kgpc_text_app(KGPCTextRec *file)
{
    kgpc_text_append(file);
}

void kgpc_text_reset(KGPCTextRec *file)
{
    if (file == NULL)
        return;
    kgpc_text_close_stream(file);
    kgpc_textrec_init_defaults(file);

    FILE *stream = NULL;
    if (file->name[0] == '\0')
        stream = stdin;
    else
        stream = fopen(file->name, "r");
    if (stream != NULL)
    {
        kgpc_textrec_set_stream(file, stream);
        if (file->bufptr != NULL && file->bufsize > 0)
            setvbuf(stream, file->bufptr, _IOFBF, (size_t)file->bufsize);
        file->mode = KGPC_FM_INPUT;
        kgpc_ioresult_set(0);
    }
    else
    {
        file->mode = KGPC_FM_CLOSED;
        kgpc_ioresult_set(errno);
    }
}

void kgpc_text_close(KGPCTextRec *file)
{
    if (file == NULL)
        return;
    kgpc_text_close_stream(file);
}

int kgpc_text_eof(KGPCTextRec *file)
{
    FILE *stream = kgpc_text_input_stream(file);
    if (stream == NULL)
        return 1;

    int ch = fgetc(stream);
    if (ch == EOF)
        return 1;

    // Try to put the character back, but don't treat ungetc failure as EOF
    // ungetc can fail due to buffer limitations, but that doesn't mean we're at EOF
    ungetc(ch, stream);

    return 0;
}

int kgpc_text_eof_default(void)
{
    return kgpc_text_eof(NULL);
}

int kgpc_text_eoln(KGPCTextRec *file)
{
    FILE *stream = kgpc_text_input_stream(file);
    if (stream == NULL)
        return 1;

    int ch = fgetc(stream);
    if (ch == EOF)
        return 1;

    if (ch == '\r')
    {
        int next = fgetc(stream);
        if (next != EOF)
            ungetc(next, stream);
        ungetc('\r', stream);
        return 1;
    }

    if (ch == '\n')
    {
        ungetc(ch, stream);
        return 1;
    }

    ungetc(ch, stream);
    return 0;
}

int kgpc_text_eoln_default(void)
{
    return kgpc_text_eoln(NULL);
}

void kgpc_text_readln_into(KGPCTextRec *file, char **target)
{
    if (target == NULL)
        return;

    FILE *stream = kgpc_text_input_stream(file);
    if (stream == NULL)
    {
        kgpc_string_assign(target, "");
        return;
    }

    char *line = kgpc_text_read_line_from_stream(stream);
    if (line == NULL)
    {
        kgpc_string_assign(target, "");
        return;
    }

    kgpc_string_assign_take(target, line);
}

void kgpc_text_readln_into_char(KGPCTextRec *file, char *target)
{
    if (target == NULL)
        return;

    FILE *stream = kgpc_text_input_stream(file);
    if (stream == NULL)
    {
        *target = '\0';
        return;
    }

    char *line = kgpc_text_read_line_from_stream(stream);
    if (line == NULL)
    {
        *target = '\0';
        return;
    }

    if (line[0] != '\0')
        *target = line[0];
    else
        *target = '\0';

    free(line);
}

void kgpc_text_readln_discard(KGPCTextRec *file)
{
    FILE *stream = kgpc_text_input_stream(file);
    if (stream == NULL)
        return;

    int ch;
    while ((ch = fgetc(stream)) != EOF)
    {
        if (ch == '\r')
        {
            int next = fgetc(stream);
            if (next != '\n' && next != EOF)
                ungetc(next, stream);
            break;
        }
        if (ch == '\n')
            break;
    }
}


void kgpc_move(void *dest, const void *src, size_t count)
{
    if (dest == NULL || src == NULL || count == 0)
        return;

    memmove(dest, src, count);
}

void kgpc_fillchar(void *dest, size_t count, int value)
{
    if (dest == NULL || count == 0)
        return;

    unsigned char byte_value = (unsigned char)(value & 0xFF);
    memset(dest, byte_value, count);
}

void kgpc_fillword(void *dest, size_t count, unsigned short value)
{
    if (dest == NULL || count == 0)
        return;

    unsigned short *ptr = (unsigned short *)dest;
    for (size_t i = 0; i < count; ++i)
        ptr[i] = value;
}

void kgpc_getmem(void **target, size_t size)
{
    if (target == NULL)
        return;

    if (size == 0)
    {
        if (*target != NULL)
        {
            free(*target);
            *target = NULL;
        }
        return;
    }

    void *memory = malloc(size);
    if (memory == NULL)
    {
        fprintf(stderr, "KGPC runtime: failed to allocate %zu bytes via GetMem.\n", size);
        exit(EXIT_FAILURE);
    }

    *target = memory;
}

/* AllocMem: allocates memory and zero-initializes it (like calloc) */
void *kgpc_allocmem(size_t size)
{
    if (size == 0)
        return NULL;

    void *memory = calloc(1, size);
    if (memory == NULL)
    {
        fprintf(stderr, "KGPC runtime: failed to allocate %zu bytes via AllocMem.\n", size);
        exit(EXIT_FAILURE);
    }

    return memory;
}

void kgpc_freemem(void *ptr)
{
    if (ptr != NULL)
        free(ptr);
}

void kgpc_reallocmem(void **target, size_t new_size)
{
    if (target == NULL)
        return;

    if (new_size == 0)
    {
        if (*target != NULL)
        {
            free(*target);
            *target = NULL;
        }
        return;
    }

    void *original = *target;
    void *resized = NULL;
    if (original == NULL)
        resized = malloc(new_size);
    else
        resized = realloc(original, new_size);

    if (resized == NULL)
    {
        fprintf(stderr, "KGPC runtime: failed to (re)allocate %zu bytes via ReallocMem.\n", new_size);
        exit(EXIT_FAILURE);
    }

    *target = resized;
}

/* =====================================================================
 * FPC RTL heap-manager overrides.
 *
 * When compiling against the FPC RTL (--no-stdlib), the FPC system unit
 * emits its own HeapInc allocator with weak SysGetMem/SysFreeMem/etc.
 * symbols.  HeapInc requires InitHeap() to be called first, which KGPC
 * does not do.  Instead we provide strong symbols that forward straight
 * to libc, so the linker picks these over the weak HeapInc versions.
 *
 * FPC ABI: SysGetMem(size: PtrInt): Pointer
 *          SysFreeMem(p: Pointer): PtrInt  (returns 0 on success)
 *          SysReallocMem(p: Pointer; size: PtrInt): Pointer
 *          SysFreeMemSize(p: Pointer; size: PtrInt): PtrInt
 *          SysTryResizeMem(p: Pointer; size: PtrInt): Pointer
 * ===================================================================== */

void *SysGetMem(intptr_t size)
{
    if (size <= 0)
        return NULL;
    return malloc((size_t)size);
}

intptr_t SysFreeMem(void *p)
{
    free(p);
    return 0;
}

void *SysReallocMem(void **pp, intptr_t size)
{
    if (pp == NULL)
        return NULL;
    void *original = *pp;
    if (size <= 0)
    {
        free(original);
        *pp = NULL;
        return NULL;
    }
    void *result = realloc(original, (size_t)size);
    if (result != NULL)
        *pp = result;
    return result;
}

intptr_t SysFreeMemSize(void *p, intptr_t size)
{
    (void)size;
    free(p);
    return 0;
}

void *SysTryResizeMem(void *p, intptr_t size)
{
    if (size <= 0)
    {
        free(p);
        return NULL;
    }
    return realloc(p, (size_t)size);
}

/* ------------------------------------------------------------------ */
/* MemoryManager initialization                                        */
/*                                                                     */
/* FPC's heap functions (GetMem, FreeMem, etc.) call through function  */
/* pointers in the MemoryManager typed constant.  KGPC doesn't yet     */
/* support typed const initialization with @function values, so        */
/* MemoryManager is emitted as all-zeros (BSS).  We initialize it at   */
/* program startup with simple malloc/free wrappers.                   */
/*                                                                     */
/* TMemoryManager layout (x86-64):                                     */
/*   offset  0: NeedLock (boolean, padded to 8 bytes)                  */
/*   offset  8: GetMem   (function pointer)                            */
/*   offset 16: FreeMem  (function pointer)                            */
/*   offset 24: FreeMemSize (function pointer)                         */
/*   offset 32: AllocMem (function pointer)                            */
/*   offset 40: ReAllocMem (function pointer)                          */
/*   offset 48: MemSize  (function pointer)                            */
/*   offset 56: InitThread (procedure pointer)                         */
/*   offset 64: DoneThread (procedure pointer)                         */
/*   offset 72: RelocateHeap (procedure pointer)                       */
/*   offset 80: GetHeapStatus (function pointer)                       */
/*   offset 88: GetFPCHeapStatus (function pointer)                    */
/* ------------------------------------------------------------------ */

/* MemoryManager — the FPC heap manager dispatch table.
 * Declared in system.p (KGPC mode) or system.pp (FPC mode).
 * We populate the function pointers at startup. */
extern char MemoryManager[];

/* Simple heap wrappers matching FPC's TMemoryManager function signatures */
static void *kgpc_mm_getmem(uintptr_t size)
{
    if (size == 0)
        return NULL;
    return malloc((size_t)size);
}

static uintptr_t kgpc_mm_freemem(void *p)
{
    free(p);
    return 0;
}

static uintptr_t kgpc_mm_freememsize(void *p, uintptr_t size)
{
    (void)size;
    free(p);
    return 0;
}

static void *kgpc_mm_allocmem(uintptr_t size)
{
    if (size == 0)
        return NULL;
    return calloc(1, (size_t)size);
}

static void *kgpc_mm_reallocmem(void **pp, uintptr_t size)
{
    if (pp == NULL)
        return NULL;
    void *original = *pp;
    if (size == 0) {
        free(original);
        *pp = NULL;
        return NULL;
    }
    void *result = realloc(original, (size_t)size);
    if (result != NULL)
        *pp = result;
    return result;
}

static uintptr_t kgpc_mm_memsize(void *p)
{
    (void)p;
    return (uintptr_t)-1; /* unknown */
}

static void kgpc_mm_noop(void) {}

__attribute__((constructor))
static void kgpc_init_memory_manager(void)
{
    char *mm = (char *)MemoryManager;
    /* Skip NeedLock at offset 0 (leave as false/0) */
    void *ptrs[] = {
        kgpc_mm_getmem,       /*  8: GetMem */
        kgpc_mm_freemem,      /* 16: FreeMem */
        kgpc_mm_freememsize,   /* 24: FreeMemSize */
        kgpc_mm_allocmem,     /* 32: AllocMem */
        kgpc_mm_reallocmem,   /* 40: ReAllocMem */
        kgpc_mm_memsize,      /* 48: MemSize */
        kgpc_mm_noop,         /* 56: InitThread */
        kgpc_mm_noop,         /* 64: DoneThread */
        kgpc_mm_noop,         /* 72: RelocateHeap */
        NULL,                 /* 80: GetHeapStatus (unused) */
        NULL,                 /* 88: GetFPCHeapStatus (unused) */
    };
    for (int i = 0; i < 11; i++)
        memcpy(mm + 8 + i * 8, &ptrs[i], sizeof(void *));
}

char *kgpc_string_concat(const char *lhs, const char *rhs)
{
    if (lhs == NULL)
        lhs = "";
    if (rhs == NULL)
        rhs = "";

    KgpcStringHeader *lhs_hdr = kgpc_string_header(lhs);
    KgpcStringHeader *rhs_hdr = kgpc_string_header(rhs);
    int lhs_elem = (lhs_hdr != NULL && lhs_hdr->elementsize == 2) ? 2 : 1;
    int rhs_elem = (rhs_hdr != NULL && rhs_hdr->elementsize == 2) ? 2 : 1;

    size_t lhs_len = kgpc_string_known_length(lhs);
    size_t rhs_len = kgpc_string_known_length(rhs);
    size_t total = lhs_len + rhs_len;

    if (lhs_elem == 2 || rhs_elem == 2)
    {
        size_t data_bytes = total * 2 + 2;
        KgpcStringHeader *hdr = (KgpcStringHeader *)malloc(sizeof(KgpcStringHeader) + data_bytes);
        if (hdr == NULL)
            return kgpc_alloc_empty_string();

        hdr->codepage = 1200;
        hdr->elementsize = 2;
        hdr->refcount = 1;
        hdr->length = (int64_t)total;

        uint16_t *out = (uint16_t *)(hdr + 1);
        size_t pos = 0;

        if (lhs_len > 0)
        {
            if (lhs_elem == 2)
            {
                memcpy(out, lhs, lhs_len * 2);
                pos += lhs_len;
            }
            else
            {
                for (size_t i = 0; i < lhs_len; ++i)
                    out[pos++] = (uint16_t)(unsigned char)lhs[i];
            }
        }

        if (rhs_len > 0)
        {
            if (rhs_elem == 2)
            {
                memcpy(out + pos, rhs, rhs_len * 2);
                pos += rhs_len;
            }
            else
            {
                for (size_t i = 0; i < rhs_len; ++i)
                    out[pos++] = (uint16_t)(unsigned char)rhs[i];
            }
        }

        out[total] = 0;
        kgpc_string_set_insert((char *)out);
        return (char *)out;
    }

    char *result = kgpc_string_alloc_with_length(total);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    if (lhs_len > 0)
        memcpy(result, lhs, lhs_len);
    if (rhs_len > 0)
        memcpy(result + lhs_len, rhs, rhs_len);
    return result;
}

uint16_t *kgpc_unicodestring_concat(const uint16_t *lhs, const uint16_t *rhs)
{
    uint16_t *lhs_owned = NULL;
    uint16_t *rhs_owned = NULL;

    if (lhs == NULL)
        lhs = kgpc_alloc_empty_unicodestring();
    if (rhs == NULL)
        rhs = kgpc_alloc_empty_unicodestring();

    {
        KgpcStringHeader *hdr = kgpc_string_header((const char *)lhs);
        if (hdr != NULL && hdr->elementsize == 1)
        {
            lhs_owned = kgpc_unicodestring_from_string((const char *)lhs);
            lhs = lhs_owned;
        }
    }
    {
        KgpcStringHeader *hdr = kgpc_string_header((const char *)rhs);
        if (hdr != NULL && hdr->elementsize == 1)
        {
            rhs_owned = kgpc_unicodestring_from_string((const char *)rhs);
            rhs = rhs_owned;
        }
    }

    int64_t lhs_len = kgpc_unicode_known_length(lhs);
    int64_t rhs_len = kgpc_unicode_known_length(rhs);
    int64_t total_len = lhs_len + rhs_len;

    uint16_t *result = kgpc_alloc_empty_unicodestring();
    if (total_len > 0)
    {
        size_t data_bytes = (size_t)total_len * sizeof(uint16_t) + sizeof(uint16_t);
        KgpcStringHeader *hdr = (KgpcStringHeader *)malloc(sizeof(KgpcStringHeader) + data_bytes);
        if (hdr != NULL)
        {
            hdr->codepage = 1200;
            hdr->elementsize = 2;
            hdr->refcount = 1;
            hdr->length = total_len;
            result = (uint16_t *)(hdr + 1);
            if (lhs_len > 0)
                memcpy(result, lhs, (size_t)lhs_len * sizeof(uint16_t));
            if (rhs_len > 0)
                memcpy(result + lhs_len, rhs, (size_t)rhs_len * sizeof(uint16_t));
            result[total_len] = 0;
            kgpc_string_set_insert((char *)result);
        }
    }

    if (lhs_owned != NULL)
        kgpc_string_release((char *)lhs_owned);
    if (rhs_owned != NULL)
        kgpc_string_release((char *)rhs_owned);

    return result;
}

int64_t kgpc_string_length(const char *value)
{
    return (int64_t)kgpc_string_known_length(value);
}

int64_t kgpc_widechar_length(const uint16_t *value)
{
    if (value == NULL)
        return 0;
    const uint16_t *cursor = value;
    while (*cursor != 0)
        cursor++;
    return (int64_t)(cursor - value);
}

char *kgpc_string_copy(const char *value, int64_t index, int64_t count)
{
    if (value == NULL)
        value = "";

    size_t len = kgpc_string_known_length(value);
    if (index < 1 || index > (int64_t)len)
        return kgpc_alloc_empty_string();

    if (count < 0)
        count = 0;

    size_t start = (size_t)(index - 1);
    size_t available = len - start;
    size_t to_copy = (size_t)count;
    if (to_copy > available)
        to_copy = available;

    char *result = kgpc_string_alloc_with_length(to_copy);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    if (to_copy > 0)
        memcpy(result, value + start, to_copy);
    return result;
}

uint16_t *kgpc_unicodestring_copy(const uint16_t *value, int64_t index, int64_t count)
{
    if (value == NULL)
        return kgpc_alloc_empty_unicodestring();

    int64_t len = kgpc_unicode_known_length(value);
    if (index < 1 || index > len)
        return kgpc_alloc_empty_unicodestring();

    if (count < 0)
        count = 0;

    int64_t start = index - 1;
    int64_t available = len - start;
    int64_t to_copy = count;
    if (to_copy > available)
        to_copy = available;

    if (to_copy <= 0)
        return kgpc_alloc_empty_unicodestring();

    uint16_t *result = kgpc_alloc_empty_unicodestring();
    size_t data_bytes = (size_t)to_copy * sizeof(uint16_t) + sizeof(uint16_t);
    KgpcStringHeader *hdr = (KgpcStringHeader *)malloc(sizeof(KgpcStringHeader) + data_bytes);
    if (hdr == NULL)
        return result;

    hdr->codepage = 1200;
    hdr->elementsize = 2;
    hdr->refcount = 1;
    hdr->length = to_copy;
    result = (uint16_t *)(hdr + 1);
    memcpy(result, value + start, (size_t)to_copy * sizeof(uint16_t));
    result[to_copy] = 0;
    kgpc_string_set_insert((char *)result);
    return result;
}

/* Copy from ShortString (length byte at index 0, chars at 1..255) */
char *kgpc_shortstring_copy(const char *value, int64_t index, int64_t count)
{
    if (value == NULL)
        return kgpc_alloc_empty_string();

    /* ShortString has length byte at position 0 */
    size_t len = (unsigned char)value[0];
    const char *chars = value + 1;  /* Actual characters start at position 1 */

    if (index < 1 || index > (int64_t)len)
        return kgpc_alloc_empty_string();

    if (count < 0)
        count = 0;

    size_t start = (size_t)(index - 1);
    size_t available = len - start;
    size_t to_copy = (size_t)count;
    if (to_copy > available)
        to_copy = available;

    char *result = kgpc_string_alloc_with_length(to_copy);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    if (to_copy > 0)
        memcpy(result, chars + start, to_copy);
    return result;
}

int64_t kgpc_string_compare(const char *lhs, const char *rhs)
{
    if (lhs == NULL)
        lhs = "";
    if (rhs == NULL)
        rhs = "";
    if (kgpc_env_flag("KGPC_DEBUG_STRTOFLOAT"))
        fprintf(stderr, "[kgpc] strcmp lhs='%s' rhs='%s'\n", lhs, rhs);

    size_t lhs_len = kgpc_string_known_length(lhs);
    size_t rhs_len = kgpc_string_known_length(rhs);
    size_t min_len = (lhs_len < rhs_len) ? lhs_len : rhs_len;

    if (min_len > 0)
    {
        int cmp = memcmp(lhs, rhs, min_len);
        if (cmp != 0)
            return (int64_t)cmp;
    }

    if (lhs_len < rhs_len)
        return -1;
    if (lhs_len > rhs_len)
        return 1;
    return 0;
}

static size_t kgpc_char_array_bounded_length(const char *value, size_t max_len)
{
    if (value == NULL || max_len == 0)
        return 0;
    const void *pos = memchr(value, '\0', max_len);
    if (pos == NULL)
        return max_len;
    return (size_t)((const char *)pos - value);
}

int64_t kgpc_char_array_compare(const char *array_value, size_t array_len, const char *rhs)
{
    if (array_value == NULL)
        array_value = "";
    if (rhs == NULL)
        rhs = "";

    size_t lhs_len = kgpc_char_array_bounded_length(array_value, array_len);
    size_t rhs_len = kgpc_string_known_length(rhs);
    size_t min_len = (lhs_len < rhs_len) ? lhs_len : rhs_len;

    if (min_len > 0)
    {
        int cmp = memcmp(array_value, rhs, min_len);
        if (cmp != 0)
            return (int64_t)cmp;
    }

    if (lhs_len < rhs_len)
        return -1;
    if (lhs_len > rhs_len)
        return 1;
    return 0;
}

int64_t kgpc_char_array_compare_full(const char *array_value, size_t array_len, const char *rhs)
{
    if (array_value == NULL)
        array_value = "";
    if (rhs == NULL)
        rhs = "";

    size_t lhs_len = array_len;
    size_t rhs_len = kgpc_string_known_length(rhs);
    size_t min_len = (lhs_len < rhs_len) ? lhs_len : rhs_len;

    if (min_len > 0)
    {
        int cmp = memcmp(array_value, rhs, min_len);
        if (cmp != 0)
            return (int64_t)cmp;
    }

    if (lhs_len < rhs_len)
        return -1;
    if (lhs_len > rhs_len)
        return 1;
    return 0;
}

int64_t kgpc_char_array_compare_array(const char *lhs, size_t lhs_len,
    const char *rhs, size_t rhs_len)
{
    if (lhs == NULL)
        lhs = "";
    if (rhs == NULL)
        rhs = "";

    size_t lhs_eff = kgpc_char_array_bounded_length(lhs, lhs_len);
    size_t rhs_eff = kgpc_char_array_bounded_length(rhs, rhs_len);
    size_t min_len = (lhs_eff < rhs_eff) ? lhs_eff : rhs_eff;

    if (min_len > 0)
    {
        int cmp = memcmp(lhs, rhs, min_len);
        if (cmp != 0)
            return (int64_t)cmp;
    }

    if (lhs_eff < rhs_eff)
        return -1;
    if (lhs_eff > rhs_eff)
        return 1;
    return 0;
}

void kgpc_string_assign_from_shortstring(char **target, const char *ss)
{
    if (target == NULL)
        return;
    char *existing = *target;
    if (existing != NULL)
        kgpc_string_release(existing);
    if (ss == NULL)
    {
        *target = kgpc_alloc_empty_string();
        return;
    }
    unsigned char len = (unsigned char)ss[0];
    char *copy = kgpc_string_alloc_with_length(len);
    if (copy == NULL)
    {
        *target = kgpc_alloc_empty_string();
        return;
    }
    if (len > 0)
        memcpy(copy, ss + 1, len);
    *target = copy;
}

void kgpc_string_assign_from_char_array(char **target, const char *value, size_t max_len)
{
    if (target == NULL)
        return;

    char *existing = *target;
    if (existing != NULL)
        kgpc_string_release(existing);

    if (value == NULL || max_len == 0)
    {
        *target = kgpc_alloc_empty_string();
        return;
    }

    size_t len = kgpc_char_array_bounded_length(value, max_len);
    char *copy = kgpc_string_alloc_with_length(len);
    if (copy == NULL)
    {
        *target = kgpc_alloc_empty_string();
        return;
    }

    if (len > 0)
        memcpy(copy, value, len);
    *target = copy;
}

void kgpc_string_assign_from_unicodestring(char **target, const uint16_t *value)
{
    if (target == NULL)
        return;

    if (value == NULL)
    {
        kgpc_string_assign(target, "");
        return;
    }

    KgpcStringHeader *hdr = kgpc_string_header((const char *)value);
    if (hdr != NULL && hdr->elementsize == 1)
    {
        kgpc_string_assign(target, (const char *)value);
        return;
    }

    int64_t len = kgpc_unicode_known_length(value);
    char *ansi = NULL;
    typedef void (*Unicode2AnsiProc)(const uint16_t *, char **, int32_t, int64_t);
    Unicode2AnsiProc conv = (Unicode2AnsiProc)widestringmanager[19];
    int32_t cp = DefaultSystemCodePage;
    if (conv != NULL)
        conv(value, &ansi, cp, len);
    else
        kgpc_default_unicode2ansi_move(value, &ansi, cp, len);

    kgpc_string_assign_take(target, ansi);
}

char *kgpc_string_from_unicodestring(const uint16_t *value)
{
    char *result = NULL;
    kgpc_string_assign_from_unicodestring(&result, value);
    return result;
}

uint16_t *kgpc_unicodestring_from_string(const char *value)
{
    if (value == NULL)
        return kgpc_alloc_empty_unicodestring();

    KgpcStringHeader *hdr = kgpc_string_header(value);
    if (hdr != NULL && hdr->elementsize == 2)
    {
        kgpc_string_retain(value);
        return (uint16_t *)value;
    }

    uint16_t *result = NULL;
    int64_t len = kgpc_string_length(value);
    typedef void (*Ansi2UnicodeProc)(const char *, int32_t, uint16_t **, int64_t);
    Ansi2UnicodeProc conv = (Ansi2UnicodeProc)widestringmanager[20];
    int32_t cp = DefaultSystemCodePage;
    if (conv != NULL)
        conv(value, cp, &result, len);
    else
        kgpc_default_ansi2unicode_move(value, cp, &result, len);

    if (result == NULL)
        return kgpc_alloc_empty_unicodestring();
    return result;
}

void kgpc_unicodestring_assign_from_string(uint16_t **target, const char *value)
{
    if (target == NULL)
        return;

    uint16_t *converted = kgpc_unicodestring_from_string(value);
    uint16_t *current = *target;
    if (current != NULL)
        kgpc_string_release((char *)current);
    *target = converted;
}

void kgpc_unicodestring_assign(uint16_t **target, const uint16_t *value)
{
    if (target == NULL)
        return;

    uint16_t *current = *target;
    if (current == value)
        return;

    if (value == NULL)
    {
        if (current != NULL)
            kgpc_string_release((char *)current);
        *target = kgpc_alloc_empty_unicodestring();
        return;
    }

    KgpcStringHeader *hdr = kgpc_string_header((const char *)value);
    if (hdr == NULL || hdr->elementsize == 1)
    {
        kgpc_unicodestring_assign_from_string(target, (const char *)value);
        return;
    }

    kgpc_string_retain((const char *)value);
    if (current != NULL)
        kgpc_string_release((char *)current);
    *target = (uint16_t *)value;
}

char *kgpc_strpas(const char *p)
{
    if (p == NULL)
        return kgpc_alloc_empty_string();
    return kgpc_string_duplicate(p);
}

char *kgpc_strpas_len(const char *p, int64_t length)
{
    if (p == NULL || length <= 0)
        return kgpc_alloc_empty_string();
    /* Truncate at first NUL, like a C-string — StrPas copies until NUL */
    size_t actual = strnlen(p, (size_t)length);
    return kgpc_string_duplicate_length(p, actual);
}

static int64_t kgpc_pos_internal(const char *hay, size_t hay_len, const char *needle, size_t needle_len, int64_t start_index)
{
    if (needle_len == 0)
    {
        if (start_index < 1) start_index = 1;
        return (size_t)start_index > hay_len + 1 ? 0 : start_index;
    }
    if (needle_len > hay_len)
        return 0;

    if (start_index < 1) start_index = 1;
    size_t start = (size_t)(start_index - 1);
    if (start >= hay_len)
        return 0;

    for (size_t i = start; i + needle_len <= hay_len; ++i)
    {
        if (memcmp(hay + i, needle, needle_len) == 0)
            return (int64_t)(i + 1);
    }
    return 0;
}

int64_t kgpc_string_pos_ca(unsigned char ch, const char *value)
{
    char needle[1] = { (char)ch };
    return kgpc_pos_internal(value ? value : "", kgpc_string_known_length(value ? value : ""), needle, 1, 1);
}

int64_t kgpc_string_pos_cs(unsigned char ch, const char *value)
{
    char needle[1] = { (char)ch };
    size_t hay_len = value ? (size_t)(unsigned char)value[0] : 0;
    return kgpc_pos_internal(value ? value + 1 : "", hay_len, needle, 1, 1);
}

int64_t kgpc_string_pos_cc(unsigned char substr, unsigned char value)
{
    char needle[1] = { (char)substr };
    char hay[1] = { (char)value };
    return kgpc_pos_internal(hay, 1, needle, 1, 1);
}

int64_t kgpc_string_pos_ac(const char *substr, unsigned char value)
{
    char hay[1] = { (char)value };
    size_t needle_len = kgpc_string_known_length(substr ? substr : "");
    return kgpc_pos_internal(hay, 1, substr ? substr : "", needle_len, 1);
}

int64_t kgpc_string_pos_sc(const char *substr, unsigned char value)
{
    char hay[1] = { (char)value };
    size_t needle_len = substr ? (size_t)(unsigned char)substr[0] : 0;
    return kgpc_pos_internal(hay, 1, substr ? substr + 1 : "", needle_len, 1);
}

int64_t kgpc_string_pos_sa(const char *substr, const char *value)
{
    size_t hay_len = kgpc_string_known_length(value ? value : "");
    size_t needle_len = substr ? (size_t)(unsigned char)substr[0] : 0;
    return kgpc_pos_internal(value ? value : "", hay_len, substr ? substr + 1 : "", needle_len, 1);
}

int64_t kgpc_string_pos_as(const char *substr, const char *value)
{
    size_t hay_len = value ? (size_t)(unsigned char)value[0] : 0;
    size_t needle_len = kgpc_string_known_length(substr ? substr : "");
    return kgpc_pos_internal(value ? value + 1 : "", hay_len, substr ? substr : "", needle_len, 1);
}

int64_t kgpc_string_pos_ss(const char *substr, const char *value)
{
    size_t hay_len = value ? (size_t)(unsigned char)value[0] : 0;
    size_t needle_len = substr ? (size_t)(unsigned char)substr[0] : 0;
    return kgpc_pos_internal(value ? value + 1 : "", hay_len, substr ? substr + 1 : "", needle_len, 1);
}

int64_t kgpc_string_pos(const char *substr, const char *value)
{
    size_t hay_len = kgpc_string_known_length(value ? value : "");
    size_t needle_len = kgpc_string_known_length(substr ? substr : "");
    return kgpc_pos_internal(value ? value : "", hay_len, substr ? substr : "", needle_len, 1);
}

int64_t kgpc_string_pos_sa_from(const char *substr, const char *value, int64_t start_index)
{
    size_t hay_len = kgpc_string_known_length(value ? value : "");
    size_t needle_len = substr ? (size_t)(unsigned char)substr[0] : 0;
    return kgpc_pos_internal(value ? value : "", hay_len, substr ? substr + 1 : "", needle_len, start_index);
}

int64_t kgpc_string_pos_as_from(const char *substr, const char *value, int64_t start_index)
{
    size_t hay_len = value ? (size_t)(unsigned char)value[0] : 0;
    size_t needle_len = kgpc_string_known_length(substr ? substr : "");
    return kgpc_pos_internal(value ? value + 1 : "", hay_len, substr ? substr : "", needle_len, start_index);
}

int64_t kgpc_string_pos_ss_from(const char *substr, const char *value, int64_t start_index)
{
    size_t hay_len = value ? (size_t)(unsigned char)value[0] : 0;
    size_t needle_len = substr ? (size_t)(unsigned char)substr[0] : 0;
    return kgpc_pos_internal(value ? value + 1 : "", hay_len, substr ? substr + 1 : "", needle_len, start_index);
}

int64_t kgpc_string_pos_from(const char *substr, const char *value, int64_t start_index)
{
    size_t hay_len = kgpc_string_known_length(value ? value : "");
    size_t needle_len = kgpc_string_known_length(substr ? substr : "");
    return kgpc_pos_internal(value ? value : "", hay_len, substr ? substr : "", needle_len, start_index);
}

int64_t kgpc_string_pos_ca_from(unsigned char ch, const char *value, int64_t start_index)
{
    char needle[1] = { (char)ch };
    return kgpc_pos_internal(value ? value : "", kgpc_string_known_length(value ? value : ""), needle, 1, start_index);
}

int64_t kgpc_string_pos_cs_from(unsigned char ch, const char *value, int64_t start_index)
{
    char needle[1] = { (char)ch };
    size_t hay_len = value ? (size_t)(unsigned char)value[0] : 0;
    return kgpc_pos_internal(value ? value + 1 : "", hay_len, needle, 1, start_index);
}

int64_t kgpc_string_pos_cc_from(unsigned char substr, unsigned char value, int64_t start_index)
{
    char needle[1] = { (char)substr };
    char hay[1] = { (char)value };
    return kgpc_pos_internal(hay, 1, needle, 1, start_index);
}

int64_t kgpc_string_pos_ac_from(const char *substr, unsigned char value, int64_t start_index)
{
    char hay[1] = { (char)value };
    size_t needle_len = kgpc_string_known_length(substr ? substr : "");
    return kgpc_pos_internal(hay, 1, substr ? substr : "", needle_len, start_index);
}

int64_t kgpc_string_pos_sc_from(const char *substr, unsigned char value, int64_t start_index)
{
    char hay[1] = { (char)value };
    size_t needle_len = substr ? (size_t)(unsigned char)substr[0] : 0;
    return kgpc_pos_internal(hay, 1, substr ? substr + 1 : "", needle_len, start_index);
}

static int kgpc_is_path_delim_char(char ch)
{
    return ch == '/' || ch == '\\';
}

static const char *kgpc_find_last_path_delim(const char *path)
{
    if (path == NULL)
        return NULL;
    const char *last = NULL;
    for (const char *ptr = path; *ptr != '\0'; ++ptr)
    {
        if (kgpc_is_path_delim_char(*ptr))
            last = ptr;
    }
    return last;
}

char *kgpc_extract_file_path(const char *filename)
{
    if (filename == NULL)
        return kgpc_alloc_empty_string();
    const char *last = kgpc_find_last_path_delim(filename);
    if (last == NULL)
        return kgpc_alloc_empty_string();
    size_t length = (size_t)(last - filename) + 1;
    return kgpc_string_duplicate_length(filename, length);
}

char *kgpc_extract_file_name(const char *filename)
{
    if (filename == NULL)
        return kgpc_alloc_empty_string();
    const char *last = kgpc_find_last_path_delim(filename);
    if (last == NULL)
        return kgpc_string_duplicate(filename);
    return kgpc_string_duplicate(last + 1);
}

char *kgpc_extract_file_ext(const char *filename)
{
    if (filename == NULL)
        return kgpc_alloc_empty_string();
    size_t len = strlen(filename);
    const char *start = filename;
    const char *limit = kgpc_find_last_path_delim(filename);
    if (limit != NULL)
        start = limit + 1;
    const char *ptr = filename + len;
    while (ptr > start)
    {
        --ptr;
        if (kgpc_is_path_delim_char(*ptr))
            break;
        if (*ptr == '.')
            return kgpc_string_duplicate(ptr);
    }
    return kgpc_alloc_empty_string();
}

char *kgpc_change_file_ext(const char *filename, const char *extension)
{
    if (filename == NULL)
        return kgpc_alloc_empty_string();
    size_t len = strlen(filename);
    const char *start = filename;
    const char *limit = kgpc_find_last_path_delim(filename);
    if (limit != NULL)
        start = limit + 1;
    const char *ptr = filename + len;
    const char *dot = NULL;
    while (ptr > start)
    {
        --ptr;
        if (kgpc_is_path_delim_char(*ptr))
            break;
        if (*ptr == '.')
        {
            dot = ptr;
            break;
        }
    }
    size_t base_len = dot ? (size_t)(dot - filename) : len;
    size_t ext_len = (extension != NULL) ? strlen(extension) : 0;
    char *result = kgpc_string_alloc_with_length(base_len + ext_len);
    if (result == NULL)
        return kgpc_alloc_empty_string();
    if (base_len > 0)
        memcpy(result, filename, base_len);
    if (ext_len > 0 && extension != NULL)
        memcpy(result + base_len, extension, ext_len);
    return result;
}

char *kgpc_exclude_trailing_path_delim(const char *path)
{
    if (path == NULL)
        return kgpc_alloc_empty_string();
    size_t len = strlen(path);
    if (len == 0)
        return kgpc_alloc_empty_string();
    size_t end = len;
    while (end > 0 && kgpc_is_path_delim_char(path[end - 1]))
        --end;
    if (end == 0)
        return kgpc_alloc_empty_string();
    if (end == len)
        return kgpc_string_duplicate(path);
    return kgpc_string_duplicate_length(path, end);
}

static long long kgpc_val_error_position(const char *text, const char *error_ptr)
{
    if (text == NULL || error_ptr == NULL)
        return 1;
    return (long long)((error_ptr - text) + 1);
}

static const char *kgpc_val_skip_trailing_whitespace(const char *ptr)
{
    if (ptr == NULL)
        return NULL;
    while (*ptr != '\0' && isspace((unsigned char)*ptr))
        ++ptr;
    return ptr;
}

static long long kgpc_val_parse_integer(const char *text, long long min_value,
    long long max_value, long long *out_value)
{
    if (text == NULL)
        text = "";

    errno = 0;
    char *endptr = NULL;
    long long value = strtoll(text, &endptr, 10);
    if (endptr == text)
        return 1;

    if (errno == ERANGE || value < min_value || value > max_value)
        return kgpc_val_error_position(text, endptr);

    const char *rest = kgpc_val_skip_trailing_whitespace(endptr);
    if (rest != NULL && *rest != '\0')
        return kgpc_val_error_position(text, rest);

    if (out_value != NULL)
        *out_value = value;
    return 0;
}

static long long kgpc_val_parse_unsigned(const char *text, unsigned long long max_value,
    unsigned long long *out_value)
{
    if (text == NULL)
        text = "";

    const char *ptr = text;
    while (*ptr != '\0' && isspace((unsigned char)*ptr))
        ++ptr;

    if (*ptr == '-')
        return kgpc_val_error_position(text, ptr);

    errno = 0;
    char *endptr = NULL;
    unsigned long long value = strtoull(ptr, &endptr, 10);
    if (endptr == ptr)
        return 1;

    if (errno == ERANGE || value > max_value)
        return kgpc_val_error_position(text, endptr);

    const char *rest = kgpc_val_skip_trailing_whitespace(endptr);
    if (rest != NULL && *rest != '\0')
        return kgpc_val_error_position(text, rest);

    if (out_value != NULL)
        *out_value = value;
    return 0;
}

static long long kgpc_val_parse_real(const char *text, double *out_value)
{
    if (text == NULL)
        text = "";

    errno = 0;
    char *endptr = NULL;
    double value = strtod(text, &endptr);
    if (endptr == text)
        return 1;

    if (errno == ERANGE)
        return kgpc_val_error_position(text, endptr);

    const char *rest = kgpc_val_skip_trailing_whitespace(endptr);
    if (rest != NULL && *rest != '\0')
        return kgpc_val_error_position(text, rest);

    if (out_value != NULL)
        *out_value = value;
    return 0;
}

long long kgpc_val_integer(const char *text, int32_t *out_value)
{
    long long parsed = 0;
    long long code = kgpc_val_parse_integer(text, INT32_MIN, INT32_MAX, &parsed);
    if (out_value != NULL)
        *out_value = (int32_t)parsed;
    return code;
}

long long kgpc_val_longint(const char *text, int64_t *out_value)
{
    long long parsed = 0;
    long long code = kgpc_val_parse_integer(text, INT64_MIN, INT64_MAX, &parsed);
    if (out_value != NULL)
        *out_value = parsed;
    return code;
}

long long kgpc_val_qword(const char *text, uint64_t *out_value)
{
    unsigned long long parsed = 0;
    long long code = kgpc_val_parse_unsigned(text, ULLONG_MAX, &parsed);
    if (out_value != NULL)
        *out_value = (uint64_t)parsed;
    return code;
}

long long kgpc_val_real(const char *text, double *out_value)
{
    double parsed = 0.0;
    long long code = kgpc_val_parse_real(text, &parsed);
    if (out_value != NULL)
        *out_value = parsed;
    return code;
}

long long kgpc_val_extended(const char *text, void *out_value)
{
    double parsed = 0.0;
    long long code = kgpc_val_parse_real(text, &parsed);
    if (out_value != NULL)
    {
        long double ext = (long double)parsed;
        memcpy(out_value, &ext, 10);
    }
    return code;
}

/* ShortString versions of Val: take a ShortString pointer (length byte + chars) */
static const char *kgpc_shortstr_to_cstr(const unsigned char *ss, char *buf, int bufsize)
{
    if (ss == NULL)
        return "";
    int len = ss[0];
    if (len >= bufsize)
        len = bufsize - 1;
    memcpy(buf, ss + 1, len);
    buf[len] = '\0';
    return buf;
}

long long kgpc_val_integer_ss(const unsigned char *ss, int32_t *out_value)
{
    char buf[256];
    return kgpc_val_integer(kgpc_shortstr_to_cstr(ss, buf, sizeof(buf)), out_value);
}

long long kgpc_val_longint_ss(const unsigned char *ss, int64_t *out_value)
{
    char buf[256];
    return kgpc_val_longint(kgpc_shortstr_to_cstr(ss, buf, sizeof(buf)), out_value);
}

long long kgpc_val_qword_ss(const unsigned char *ss, uint64_t *out_value)
{
    char buf[256];
    return kgpc_val_qword(kgpc_shortstr_to_cstr(ss, buf, sizeof(buf)), out_value);
}

long long kgpc_val_real_ss(const unsigned char *ss, double *out_value)
{
    char buf[256];
    return kgpc_val_real(kgpc_shortstr_to_cstr(ss, buf, sizeof(buf)), out_value);
}

long long kgpc_val_extended_ss(const unsigned char *ss, void *out_value)
{
    char buf[256];
    return kgpc_val_extended(kgpc_shortstr_to_cstr(ss, buf, sizeof(buf)), out_value);
}

int64_t bsrqword_i64(uint64_t value)
{
    if (value == 0)
        return -1;
    return (int64_t)(63u - (uint64_t)__builtin_clzll(value));
}

/* bsrdword_li and bsfdword_li: FPC's system.pp provides Pascal
 * implementations compiled by KGPC.  These C versions are kept as
 * fallbacks with kgpc_ prefix to avoid duplicate symbol conflicts. */
int64_t kgpc_bsrdword_li(uint32_t value)
{
    if (value == 0)
        return -1;
    return (int64_t)(31u - (uint32_t)__builtin_clz(value));
}

int64_t bsfqword_i64(uint64_t value)
{
    if (value == 0)
        return -1;
    return (int64_t)__builtin_ctzll(value);
}

int64_t kgpc_bsfdword_li(uint32_t value)
{
    if (value == 0)
        return -1;
    return (int64_t)__builtin_ctz(value);
}

int64_t fpc_in_popcnt_x(uint64_t value)
{
    return (int64_t)__builtin_popcountll(value);
}

int32_t popcnt_i(int32_t value)
{
    return (int32_t)__builtin_popcount((uint32_t)value);
}

int32_t popcnt_li(int32_t value)
{
    return (int32_t)__builtin_popcount((uint32_t)value);
}

/* fpc_finalize: FPC runtime procedure for finalizing managed types
 * (strings, dynamic arrays, interfaces, variants).  The actual
 * finalization logic depends on TypeInfo, but the current compiler
 * doesn't emit full RTTI.  Provide a no-op stub so that generated
 * cleanup code links successfully. */
void fpc_finalize(void *data, void *typeinfo)
{
    (void)data;
    (void)typeinfo;
}

/* filecreate_rbs: FPC FileCreate(Filename: string): THandle
 * Creates a new file (or truncates existing), returns file descriptor.
 * The _i and _i_i suffixed variants (with Rights/Attributes params) redirect
 * here via runtime_fpc_rtl_compat.S; extra parameters are ignored. */
#include <fcntl.h>
#include <sys/stat.h>
int64_t kgpc_filecreate_rbs(const char *filename)
{
#ifdef _WIN32
    #include <io.h>
    return (int64_t)_open(filename, _O_CREAT | _O_TRUNC | _O_RDWR | _O_BINARY, _S_IREAD | _S_IWRITE);
#else
    return (int64_t)open(filename, O_CREAT | O_TRUNC | O_RDWR, 0666);
#endif
}

/* Chr function - returns a character value as an integer */
int64_t fpc_in_chr_byte(int64_t value)
{
    if (value < 0)
        return 0;
    if (value > 255)
        return 255;
    
    return value;
}

int64_t kgpc_chr(int64_t value)
{
    return fpc_in_chr_byte(value);
}

/* Convert a character value to a single-character string */
char *kgpc_char_to_string(int64_t value)
{
    /* Clamp value to valid character range [0, 255] */
    if (value < 0)
        value = 0;
    else if (value > 255)
        value = 255;

    char *result = kgpc_string_alloc_with_length(1);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    result[0] = (char)value;
    return result;
}

/* Alias for WriteStr compatibility */
char *kgpc_char_to_str(int64_t value)
{
    return kgpc_char_to_string(value);
}

char *kgpc_bool_to_str(int64_t value)
{
    return kgpc_string_duplicate(value ? "TRUE" : "FALSE");
}

char *kgpc_real_to_str(double value)
{
    char buffer[64];
    int written = snprintf(buffer, sizeof(buffer), "%g", value);
    if (written < 0 || written >= (int)sizeof(buffer))
        return kgpc_alloc_empty_string();

    char *result = kgpc_string_alloc_with_length((size_t)written);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    memcpy(result, buffer, (size_t)written + 1);
    return result;
}

int64_t kgpc_upcase_char(int64_t value)
{
    unsigned char ch = (unsigned char)(value & 0xFF);
    if (ch >= 'a' && ch <= 'z')
        ch = (unsigned char)(ch - ('a' - 'A'));
    return (int64_t)ch;
}

int64_t kgpc_lowercase_char(int64_t value)
{
    unsigned char ch = (unsigned char)(value & 0xFF);
    if (ch >= 'A' && ch <= 'Z')
        ch = (unsigned char)(ch + ('a' - 'A'));
    return (int64_t)ch;
}

int64_t kgpc_is_odd(int64_t value)
{
    return (value & 1) ? 1 : 0;
}

int64_t fpc_in_const_odd(int64_t value)
{
    return (value & 1) ? 1 : 0;
}

void *fpc_in_const_ptr(void *value)
{
    return value;
}

int32_t kgpc_sqr_int32(int32_t value)
{
    return value * value;
}

int64_t kgpc_sqr_int64(int64_t value)
{
    return value * value;
}

int64_t fpc_in_const_sqr(int64_t value)
{
    return value * value;
}

double fpc_in_sqr_real(double value)
{
    return value * value;
}

int64_t kgpc_ord_string(const char *value)
{
    if (value == NULL || value[0] == '\0')
        return 0;

    return (unsigned char)value[0];
}

int64_t kgpc_ord_longint(int64_t value)
{
    return value;
}

char *kgpc_int_to_str(int64_t value)
{
    char buffer[32];
    int written = snprintf(buffer, sizeof(buffer), "%lld", (long long)value);
    if (written < 0)
        return kgpc_alloc_empty_string();

    char *result = kgpc_string_alloc_with_length((size_t)written);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    memcpy(result, buffer, (size_t)written + 1);
    return result;
}

char *kgpc_octstr(int64_t value, int64_t digits)
{
    if (digits < 1)
        digits = 1;
    if (digits > 22)
        digits = 22;
    char buffer[32];
    int written = snprintf(buffer, sizeof(buffer), "%0*llo", (int)digits,
                           (unsigned long long)value);
    if (written < 0)
        return kgpc_alloc_empty_string();

    char *result = kgpc_string_alloc_with_length((size_t)written);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    memcpy(result, buffer, (size_t)written + 1);
    return result;
}

char *kgpc_binstr(int64_t value, int64_t digits)
{
    if (digits < 1)
        digits = 1;
    if (digits > 64)
        digits = 64;
    char buffer[72];
    uint64_t uval = (uint64_t)value;
    int len = (int)digits;
    buffer[len] = '\0';
    for (int i = len - 1; i >= 0; i--)
    {
        buffer[i] = '0' + (char)(uval & 1);
        uval >>= 1;
    }

    char *result = kgpc_string_alloc_with_length((size_t)len);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    memcpy(result, buffer, (size_t)len + 1);
    return result;
}

char *kgpc_hexstr(int64_t value, int64_t digits)
{
    if (digits < 1)
        digits = 1;
    if (digits > 16)
        digits = 16;
    char buffer[32];
    int written = snprintf(buffer, sizeof(buffer), "%0*llX", (int)digits,
                           (unsigned long long)value);
    if (written < 0)
        return kgpc_alloc_empty_string();

    char *result = kgpc_string_alloc_with_length((size_t)written);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    memcpy(result, buffer, (size_t)written + 1);
    return result;
}

static char *kgpc_apply_field_width(char *value, int64_t width)
{
    if (value == NULL)
        return NULL;

    if (width == -1 || width == 0)
        return value;

    int left_align = (width < 0);
    uint64_t abs_width = (width < 0) ? (uint64_t)(-width) : (uint64_t)width;
    size_t len = kgpc_string_known_length(value);
    if (abs_width <= len)
        return value;

    size_t pad = abs_width - len;
    char *result = kgpc_string_alloc_with_length(abs_width);
    if (result == NULL)
        return value;

    if (left_align)
    {
        memcpy(result, value, len);
        memset(result + len, ' ', pad);
    }
    else
    {
        memset(result, ' ', pad);
        memcpy(result + pad, value, len);
    }
    if (value != NULL)
        kgpc_string_release(value);
    return result;
}

void kgpc_str_int64(int64_t value, char **target)
{
    if (target == NULL)
        return;

    char *result = kgpc_int_to_str(value);
    if (result == NULL)
        return;

    char *existing = *target;
    if (existing != NULL)
        kgpc_string_release(existing);
    *target = result;
}

void kgpc_str_int64_fmt(int64_t value, int64_t width, char **target)
{
    if (target == NULL)
        return;

    char *result = kgpc_int_to_str(value);
    if (result == NULL)
        return;

    result = kgpc_apply_field_width(result, width);

    char *existing = *target;
    if (existing != NULL)
        kgpc_string_release(existing);
    *target = result;
}

void kgpc_str_real(double value, char **target)
{
    if (target == NULL)
        return;

    char *result = kgpc_float_to_string(value, -1);
    if (result == NULL)
        return;

    char *existing = *target;
    if (existing != NULL)
        kgpc_string_release(existing);
    *target = result;
}

void kgpc_str_real_fmt(double value, int64_t width, int64_t precision, char **target)
{
    if (target == NULL)
        return;

    char *result = kgpc_float_to_string(value, (int)precision);
    if (result == NULL)
        return;

    result = kgpc_apply_field_width(result, width);

    char *existing = *target;
    if (existing != NULL)
        kgpc_string_release(existing);
    *target = result;
}

/* Str for ShortString targets - copies result to a fixed-size Pascal ShortString array.
 * ShortString has format: first byte = length, followed by up to 255 characters. */
static void kgpc_str_to_shortstring_final(char *target, char *result, int64_t max_length)
{
    if (target == NULL || result == NULL)
        return;
    if (max_length < 1) max_length = 1;
    if (max_length > 256) max_length = 256;
    kgpc_string_to_shortstring(target, result, (size_t)max_length);
    kgpc_string_release(result);
}

void kgpc_str_int64_shortstring(int64_t value, char *target)
{
    kgpc_str_to_shortstring_final(target, kgpc_int_to_str(value), 256);
}

void kgpc_str_int64_bounded_shortstring(int64_t value, char *target, int64_t max_length)
{
    kgpc_str_to_shortstring_final(target, kgpc_int_to_str(value), max_length);
}

void kgpc_str_int64_fmt_shortstring(int64_t value, int64_t width, char *target)
{
    kgpc_str_to_shortstring_final(target, kgpc_apply_field_width(kgpc_int_to_str(value), width), 256);
}

void kgpc_str_int64_fmt_bounded_shortstring(int64_t value, int64_t width, char *target, int64_t max_length)
{
    kgpc_str_to_shortstring_final(target, kgpc_apply_field_width(kgpc_int_to_str(value), width), max_length);
}

void kgpc_str_real_shortstring(double value, char *target)
{
    kgpc_str_to_shortstring_final(target, kgpc_float_to_string(value, -1), 256);
}

void kgpc_str_real_bounded_shortstring(double value, char *target, int64_t max_length)
{
    kgpc_str_to_shortstring_final(target, kgpc_float_to_string(value, -1), max_length);
}

void kgpc_str_real_fmt_shortstring(double value, int64_t width, int64_t precision, char *target)
{
    kgpc_str_to_shortstring_final(target, kgpc_apply_field_width(kgpc_float_to_string(value, (int)precision), width), 256);
}

void kgpc_str_real_fmt_bounded_shortstring(double value, int64_t width, char *target, int64_t precision_and_max)
{
    int64_t precision = (int32_t)(precision_and_max & 0xffffffffu);
    int64_t max_length = (uint32_t)((uint64_t)precision_and_max >> 32);
    kgpc_str_to_shortstring_final(target, kgpc_apply_field_width(kgpc_float_to_string(value, (int)precision), width), max_length);
}

double kgpc_now(void)
{
#ifdef _WIN32
    FILETIME ft;
    ULARGE_INTEGER uli;
    GetSystemTimeAsFileTime(&ft);
    uli.LowPart = ft.dwLowDateTime;
    uli.HighPart = ft.dwHighDateTime;
    if (uli.QuadPart == 0)
    {
        time_t fallback = time(NULL);
        if (fallback < 0)
            return 0;
        double unix_seconds = (double)fallback;
        return (unix_seconds / 86400.0) + 25569.0;
    }
    if (uli.QuadPart <= 116444736000000000ULL)
    {
        time_t fallback = time(NULL);
        if (fallback < 0)
            return 0;
        double unix_seconds = (double)fallback;
        return (unix_seconds / 86400.0) + 25569.0;
    }
    /* FILETIME is in 100-nanosecond intervals since January 1, 1601. */
    uint64_t ticks = uli.QuadPart - 116444736000000000ULL;
    double unix_seconds = (double)ticks / 10000000.0;
    return (unix_seconds / 86400.0) + 25569.0;
#else
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
        return 0;
    double unix_seconds = (double)ts.tv_sec + ((double)ts.tv_nsec / 1000000000.0);
    return (unix_seconds / 86400.0) + 25569.0;
#endif
}

static int append_char(char **buffer, size_t *length, size_t *capacity, char ch)
{
    if (*length + 1 >= *capacity)
    {
        size_t new_capacity = (*capacity == 0) ? 32 : (*capacity * 2);
        char *new_buf = (char *)realloc(*buffer, new_capacity);
        if (new_buf == NULL)
            return 0;
        *buffer = new_buf;
        *capacity = new_capacity;
    }
    (*buffer)[(*length)++] = ch;
    (*buffer)[*length] = '\0';
    return 1;
}

static int append_text(char **buffer, size_t *length, size_t *capacity, const char *text)
{
    while (text != NULL && *text != '\0')
    {
        if (!append_char(buffer, length, capacity, *text++))
            return 0;
    }
    return 1;
}

static int match_token_ci(const char *cursor, const char *token)
{
    size_t len = strlen(token);
    for (size_t i = 0; i < len; ++i)
    {
        if (cursor[i] == '\0' || tolower((unsigned char)cursor[i]) != tolower((unsigned char)token[i]))
            return 0;
    }
    return 1;
}

static void kgpc_fill_tm_from_unix_seconds(struct tm *out, int64_t seconds)
{
    if (out == NULL)
        return;

    int64_t days = seconds / 86400;
    int64_t rem = seconds % 86400;
    if (rem < 0)
    {
        rem += 86400;
        days -= 1;
    }

    int hour = (int)(rem / 3600);
    rem %= 3600;
    int minute = (int)(rem / 60);
    int second = (int)(rem % 60);

    int64_t z = days + 719468;
    int64_t era = (z >= 0 ? z : z - 146096) / 146097;
    unsigned doe = (unsigned)(z - era * 146097);
    unsigned yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    int year = (int)(yoe) + (int)era * 400;
    unsigned doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    unsigned mp = (5 * doy + 2) / 153;
    unsigned day = doy - (153 * mp + 2) / 5 + 1;
    unsigned month = mp + (mp < 10 ? 3 : -9);
    year += (month <= 2);

    memset(out, 0, sizeof(*out));
    out->tm_year = year - 1900;
    out->tm_mon = (int)month - 1;
    out->tm_mday = (int)day;
    out->tm_hour = hour;
    out->tm_min = minute;
    out->tm_sec = second;
    out->tm_isdst = -1;
}

char *kgpc_format_datetime(const char *format, double datetime)
{
    if (format == NULL)
        format = "";

    double unix_seconds = (datetime - 25569.0) * 86400.0;
    double whole_seconds = floor(unix_seconds);
    double frac = unix_seconds - whole_seconds;
    if (frac < 0.0)
    {
        frac += 1.0;
        whole_seconds -= 1.0;
    }
    time_t seconds = (time_t)whole_seconds;
    int millis = (int)floor((frac * 1000.0) + 0.5);
    if (millis >= 1000)
    {
        millis = 0;
        seconds += 1;
    }

    struct tm tm_value;
#ifdef _WIN32
    errno_t err = localtime_s(&tm_value, &seconds);
    if (err != 0)
    {
        struct tm *fallback = localtime(&seconds);
        if (fallback != NULL)
        {
            tm_value = *fallback;
        }
        else
        {
            errno_t gmt_err = gmtime_s(&tm_value, &seconds);
            if (gmt_err != 0)
            {
                struct tm *gmt_fallback = gmtime(&seconds);
                if (gmt_fallback != NULL)
                {
                    tm_value = *gmt_fallback;
                }
                else
                {
                    kgpc_fill_tm_from_unix_seconds(&tm_value, (int64_t)seconds);
                }
            }
        }
    }
#else
    if (localtime_r(&seconds, &tm_value) == NULL)
    {
        kgpc_fill_tm_from_unix_seconds(&tm_value, (int64_t)seconds);
    }
#endif

    size_t capacity = 64;
    size_t length = 0;
    char *result = (char *)malloc(capacity);
    if (result == NULL)
        return kgpc_alloc_empty_string();
    result[0] = '\0';

    const char *cursor = format;
    while (*cursor != '\0')
    {
        if (*cursor == '\'')
        {
            ++cursor;
            while (*cursor != '\0')
            {
                if (*cursor == '\'' && *(cursor + 1) == '\'')
                {
                    if (!append_char(&result, &length, &capacity, '\''))
                    {
                        free(result);
                        return kgpc_alloc_empty_string();
                    }
                    cursor += 2;
                    continue;
                }
                if (*cursor == '\'')
                {
                    ++cursor;
                    break;
                }
                if (!append_char(&result, &length, &capacity, *cursor++))
                {
                    free(result);
                    return kgpc_alloc_empty_string();
                }
            }
            continue;
        }

        if (match_token_ci(cursor, "yyyy"))
        {
            char buffer[16];  // Increased from 8 to handle extreme values
            snprintf(buffer, sizeof(buffer), "%04d", tm_value.tm_year + 1900);
            if (!append_text(&result, &length, &capacity, buffer))
            {
                free(result);
                return kgpc_alloc_empty_string();
            }
            cursor += 4;
            continue;
        }
        if (match_token_ci(cursor, "yy"))
        {
            char buffer[4];
            snprintf(buffer, sizeof(buffer), "%02d", (tm_value.tm_year + 1900) % 100);
            if (!append_text(&result, &length, &capacity, buffer))
            {
                free(result);
                return kgpc_alloc_empty_string();
            }
            cursor += 2;
            continue;
        }
        if (match_token_ci(cursor, "mm"))
        {
            char buffer[16];  // Increased from 4 to handle extreme values
            snprintf(buffer, sizeof(buffer), "%02d", tm_value.tm_mon + 1);
            if (!append_text(&result, &length, &capacity, buffer))
            {
                free(result);
                return kgpc_alloc_empty_string();
            }
            cursor += 2;
            continue;
        }
        if (match_token_ci(cursor, "dd"))
        {
            char buffer[4];
            snprintf(buffer, sizeof(buffer), "%02d", tm_value.tm_mday);
            if (!append_text(&result, &length, &capacity, buffer))
            {
                free(result);
                return kgpc_alloc_empty_string();
            }
            cursor += 2;
            continue;
        }
        if (match_token_ci(cursor, "hh"))
        {
            char buffer[4];
            snprintf(buffer, sizeof(buffer), "%02d", tm_value.tm_hour);
            if (!append_text(&result, &length, &capacity, buffer))
            {
                free(result);
                return kgpc_alloc_empty_string();
            }
            cursor += 2;
            continue;
        }
        if (match_token_ci(cursor, "nn"))
        {
            char buffer[4];
            snprintf(buffer, sizeof(buffer), "%02d", tm_value.tm_min);
            if (!append_text(&result, &length, &capacity, buffer))
            {
                free(result);
                return kgpc_alloc_empty_string();
            }
            cursor += 2;
            continue;
        }
        if (match_token_ci(cursor, "ss"))
        {
            char buffer[4];
            snprintf(buffer, sizeof(buffer), "%02d", tm_value.tm_sec);
            if (!append_text(&result, &length, &capacity, buffer))
            {
                free(result);
                return kgpc_alloc_empty_string();
            }
            cursor += 2;
            continue;
        }
        if (match_token_ci(cursor, "zzz"))
        {
            char buffer[12];  // Large enough for any int value
            snprintf(buffer, sizeof(buffer), "%03d", millis);
            if (!append_text(&result, &length, &capacity, buffer))
            {
                free(result);
                return kgpc_alloc_empty_string();
            }
            cursor += 3;
            continue;
        }

        if (!append_char(&result, &length, &capacity, *cursor++))
        {
            free(result);
            return kgpc_alloc_empty_string();
        }
    }

    return result;
}

typedef struct
{
    char *data;
    size_t length;
    size_t capacity;
} kgpc_format_builder;

static void kgpc_format_builder_init(kgpc_format_builder *builder)
{
    builder->data = NULL;
    builder->length = 0;
    builder->capacity = 0;
}

static void kgpc_format_builder_free(kgpc_format_builder *builder)
{
    if (builder->data != NULL)
        free(builder->data);
    builder->data = NULL;
    builder->length = 0;
    builder->capacity = 0;
}

static int kgpc_format_builder_reserve(kgpc_format_builder *builder, size_t extra)
{
    size_t needed = builder->length + extra + 1;
    if (needed <= builder->capacity)
        return 1;
    size_t new_capacity = (builder->capacity == 0) ? 64 : builder->capacity;
    while (new_capacity < needed)
        new_capacity *= 2;
    char *new_data = (char *)realloc(builder->data, new_capacity);
    if (new_data == NULL)
        return 0;
    builder->data = new_data;
    builder->capacity = new_capacity;
    return 1;
}

static int kgpc_format_builder_append_buf(kgpc_format_builder *builder, const char *buf, size_t len)
{
    if (len == 0)
        return 1;
    if (!kgpc_format_builder_reserve(builder, len))
        return 0;
    memcpy(builder->data + builder->length, buf, len);
    builder->length += len;
    builder->data[builder->length] = '\0';
    return 1;
}

static int kgpc_format_builder_append_char(kgpc_format_builder *builder, char ch)
{
    return kgpc_format_builder_append_buf(builder, &ch, 1);
}

static int kgpc_format_builder_append_padding(kgpc_format_builder *builder, size_t count)
{
    if (count == 0)
        return 1;
    if (!kgpc_format_builder_reserve(builder, count))
        return 0;
    memset(builder->data + builder->length, ' ', count);
    builder->length += count;
    builder->data[builder->length] = '\0';
    return 1;
}

static int kgpc_format_builder_append_formatted(kgpc_format_builder *builder,
    const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    int needed = vsnprintf(NULL, 0, fmt, args);
    va_end(args);
    if (needed < 0)
        return 0;
    if (!kgpc_format_builder_reserve(builder, (size_t)needed))
        return 0;
    va_start(args, fmt);
    vsnprintf(builder->data + builder->length, builder->capacity - builder->length, fmt, args);
    va_end(args);
    builder->length += (size_t)needed;
    return 1;
}

static int kgpc_format_builder_append_string_with_width(kgpc_format_builder *builder,
    const char *value, size_t value_len, int width, int left_align)
{
    size_t padding = 0;
    if (width > 0 && (size_t)width > value_len)
        padding = (size_t)width - value_len;
    if (!left_align && padding > 0)
    {
        if (!kgpc_format_builder_append_padding(builder, padding))
            return 0;
    }
    if (!kgpc_format_builder_append_buf(builder, value, value_len))
        return 0;
    if (left_align && padding > 0)
    {
        if (!kgpc_format_builder_append_padding(builder, padding))
            return 0;
    }
    return 1;
}

char *kgpc_format(const char *fmt, const kgpc_tvarrec *args, size_t arg_count)
{
    if (fmt == NULL)
        fmt = "";

    kgpc_format_builder builder;
    kgpc_format_builder_init(&builder);

    size_t arg_index = 0;
    while (*fmt != '\0')
    {
        if (*fmt != '%')
        {
            if (!kgpc_format_builder_append_char(&builder, *fmt++))
            {
                kgpc_format_builder_free(&builder);
                return kgpc_alloc_empty_string();
            }
            continue;
        }

        ++fmt;
        if (*fmt == '%')
        {
            if (!kgpc_format_builder_append_char(&builder, '%'))
            {
                kgpc_format_builder_free(&builder);
                return kgpc_alloc_empty_string();
            }
            ++fmt;
            continue;
        }

        int left_align = 0;
        if (*fmt == '-')
        {
            left_align = 1;
            ++fmt;
        }

        int width = -1;
        if (isdigit((unsigned char)*fmt))
        {
            width = 0;
            while (isdigit((unsigned char)*fmt))
            {
                width = width * 10 + (*fmt - '0');
                ++fmt;
            }
        }

        int precision = -1;
        if (*fmt == '.')
        {
            ++fmt;
            precision = 0;
            while (isdigit((unsigned char)*fmt))
            {
                precision = precision * 10 + (*fmt - '0');
                ++fmt;
            }
        }

        char spec = *fmt ? *fmt++ : '\0';
        if (spec == '\0')
            break;

        if (arg_index >= arg_count || args == NULL)
        {
            kgpc_format_builder_append_buf(&builder, "[Invalid]", strlen("[Invalid]"));
            continue;
        }

        const kgpc_tvarrec *arg = &args[arg_index++];
        switch (spec)
        {
            case 'd':
            case 'i':
            case 'u':
            case 'x':
            case 'X':
            case 'p':
            case 'P':
            {
                long long value = arg->data.v_int;
                if (arg->kind == KGPC_TVAR_KIND_BOOL || arg->kind == KGPC_TVAR_KIND_CHAR)
                    value = arg->data.v_int;
                else if (arg->kind == KGPC_TVAR_KIND_POINTER || arg->kind == KGPC_TVAR_KIND_STRING || arg->kind == KGPC_TVAR_KIND_ANSISTRING)
                    value = (long long)(intptr_t)arg->data.v_ptr;
                else if (arg->kind != KGPC_TVAR_KIND_INT)
                {
                    kgpc_format_builder_append_buf(&builder, "[Invalid]", strlen("[Invalid]"));
                    break;
                }

                char fmtbuf[32];
                size_t pos = 0;
                fmtbuf[pos++] = '%';
                if (left_align)
                    fmtbuf[pos++] = '-';
                if (width >= 0)
                    pos += (size_t)snprintf(fmtbuf + pos, sizeof(fmtbuf) - pos, "%d", width);
                if (precision >= 0 && spec != 'p' && spec != 'P')
                {
                    fmtbuf[pos++] = '.';
                    pos += (size_t)snprintf(fmtbuf + pos, sizeof(fmtbuf) - pos, "%d", precision);
                }
                fmtbuf[pos++] = spec;
                fmtbuf[pos] = '\0';

                if (!kgpc_format_builder_append_formatted(&builder, fmtbuf, value))
                {
                    kgpc_format_builder_free(&builder);
                    return kgpc_alloc_empty_string();
                }
                break;
            }
            case 'f':
            case 'F':
            case 'g':
            case 'G':
            case 'e':
            case 'E':
            case 'n':
            case 'N':
            {
                double real_value = 0.0;
                if (arg->kind == KGPC_TVAR_KIND_REAL)
                    real_value = *(double *)arg->data.v_ptr;
                else if (arg->kind == KGPC_TVAR_KIND_INT || arg->kind == KGPC_TVAR_KIND_BOOL ||
                         arg->kind == KGPC_TVAR_KIND_CHAR)
                    real_value = (double)arg->data.v_int;
                else
                {
                    kgpc_format_builder_append_buf(&builder, "[Invalid]", strlen("[Invalid]"));
                    break;
                }

                char fmtbuf[32];
                size_t pos = 0;
                fmtbuf[pos++] = '%';
                if (left_align)
                    fmtbuf[pos++] = '-';
                if (width >= 0)
                    pos += (size_t)snprintf(fmtbuf + pos, sizeof(fmtbuf) - pos, "%d", width);
                if (precision >= 0)
                {
                    fmtbuf[pos++] = '.';
                    pos += (size_t)snprintf(fmtbuf + pos, sizeof(fmtbuf) - pos, "%d", precision);
                }
                fmtbuf[pos++] = spec;
                fmtbuf[pos] = '\0';

                if (!kgpc_format_builder_append_formatted(&builder, fmtbuf, real_value))
                {
                    kgpc_format_builder_free(&builder);
                    return kgpc_alloc_empty_string();
                }
                break;
            }
            case 's':
            case 'S':
            {
                const char *str_ptr = (const char *)arg->data.v_ptr;
                if (arg->kind == KGPC_TVAR_KIND_CHAR)
                {
                    char ch = (char)(arg->data.v_int & 0xFF);
                    char temp[2] = { ch, '\0' };
                    str_ptr = temp;
                    size_t len = 1;
                    if (!kgpc_format_builder_append_string_with_width(&builder, temp, len, width, left_align))
                    {
                        kgpc_format_builder_free(&builder);
                        return kgpc_alloc_empty_string();
                    }
                    break;
                }
                if (arg->kind != KGPC_TVAR_KIND_STRING && arg->kind != KGPC_TVAR_KIND_POINTER && arg->kind != KGPC_TVAR_KIND_ANSISTRING)
                {
                    kgpc_format_builder_append_buf(&builder, "[Invalid]", strlen("[Invalid]"));
                    break;
                }
                if (str_ptr == NULL)
                    str_ptr = "";
                size_t len = kgpc_string_known_length(str_ptr);
                if (precision >= 0 && (size_t)precision < len)
                    len = (size_t)precision;
                if (!kgpc_format_builder_append_string_with_width(&builder, str_ptr, len, width, left_align))
                {
                    kgpc_format_builder_free(&builder);
                    return kgpc_alloc_empty_string();
                }
                break;
            }
            case 'c':
            case 'C':
            {
                char ch = 0;
                if (arg->kind == KGPC_TVAR_KIND_CHAR || arg->kind == KGPC_TVAR_KIND_INT ||
                    arg->kind == KGPC_TVAR_KIND_BOOL)
                    ch = (char)(arg->data.v_int & 0xFF);
                else
                {
                    kgpc_format_builder_append_buf(&builder, "[Invalid]", strlen("[Invalid]"));
                    break;
                }
                if (!kgpc_format_builder_append_string_with_width(&builder, &ch, 1, width, left_align))
                {
                    kgpc_format_builder_free(&builder);
                    return kgpc_alloc_empty_string();
                }
                break;
            }
            default:
                kgpc_format_builder_append_char(&builder, spec);
                break;
        }
    }

    if (!kgpc_format_builder_reserve(&builder, 0))
    {
        kgpc_format_builder_free(&builder);
        return kgpc_alloc_empty_string();
    }

    char *result = kgpc_string_duplicate(builder.data != NULL ? builder.data : "");
    kgpc_format_builder_free(&builder);
    return result != NULL ? result : kgpc_alloc_empty_string();
}

char *kgpc_float_to_string(double value, int precision)
{
    if (precision < 0)
    {
        if (isnan(value))
            return kgpc_string_duplicate("-nan(ind)");
        if (isinf(value))
            return kgpc_string_duplicate(value > 0 ? "inf" : "-inf");

        char buffer[64];
        int len = snprintf(buffer, sizeof(buffer), "%.15g", value);
        if (len < 0)
            return kgpc_alloc_empty_string();
        return kgpc_string_duplicate(buffer);
    }
    if (precision > 18)
        precision = 18;

    double normalized = value;
    if (isfinite(value) && precision > 0)
    {
        double scale = 1.0;
        for (int i = 0; i < precision; ++i)
            scale *= 10.0;
        if (scale != 0.0 && isfinite(scale))
            normalized = round(value * scale) / scale;
    }

    if (isnan(normalized))
        return kgpc_string_duplicate("-nan(ind)");
    if (isinf(normalized))
        return kgpc_string_duplicate(value > 0 ? "inf" : "-inf");

    char fmt[16];
    snprintf(fmt, sizeof(fmt), "%%.%df", precision);
    char buffer[64];
    int len = snprintf(buffer, sizeof(buffer), fmt, normalized);
    if (len < 0)
        return kgpc_alloc_empty_string();
    return kgpc_string_duplicate(buffer);
}

int kgpc_string_to_int(const char *text, int *out_value)
{
    if (text == NULL)
    {
        if (out_value != NULL) *out_value = 0;
        return 0;
    }
    char *endptr;
    errno = 0;
    long long val = strtoll(text, &endptr, 10);
    if (endptr == text || *endptr != '\0' || errno == ERANGE)
    {
        if (out_value != NULL) *out_value = 0;
        return 0;
    }
    if (out_value != NULL)
        *out_value = (int)val;
    return 1;
}

static int kgpc_env_flag(const char *name)
{
    const char *v = getenv(name);
    return v && (*v == '1' || *v == 'y' || *v == 'Y' || *v == 't' || *v == 'T');
}

int kgpc_string_to_real(const char *text, double *out_value)
{
    if (text == NULL)
    {
        if (out_value != NULL) *out_value = 0.0;
        return 0;
    }
    const char *p = text;
    while (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n')
        p++;

    int sign = 1;
    if (*p == '+' || *p == '-')
    {
        if (*p == '-')
            sign = -1;
        p++;
    }

    double int_part = 0.0;
    int digits = 0;
    while (*p >= '0' && *p <= '9')
    {
        int_part = int_part * 10.0 + (double)(*p - '0');
        p++;
        digits++;
    }

    double frac_part = 0.0;
    double frac_div = 1.0;
    if (*p == '.')
    {
        p++;
        while (*p >= '0' && *p <= '9')
        {
            frac_part = frac_part * 10.0 + (double)(*p - '0');
            frac_div *= 10.0;
            p++;
            digits++;
        }
    }

    if (digits == 0)
    {
        if (out_value != NULL) *out_value = 0.0;
        return 0;
    }

    if (*p == 'e' || *p == 'E')
    {
        char *endptr;
        errno = 0;
        double val = strtod(text, &endptr);
        if (kgpc_env_flag("KGPC_DEBUG_STRTOFLOAT"))
            fprintf(stderr, "[kgpc] string_to_real('%s') -> val=%g end=%p (err=%d)\n", text ? text : "(null)", val, (void*)endptr, errno);
        if (endptr == text || errno == ERANGE)
        {
            if (out_value != NULL) *out_value = 0.0;
            return 0;
        }
        while (*endptr == ' ' || *endptr == '\t' || *endptr == '\r' || *endptr == '\n')
            endptr++;
        if (*endptr != '\0')
        {
            if (out_value != NULL) *out_value = 0.0;
            return 0;
        }
        if (out_value != NULL)
            *out_value = val;
        kgpc_real_cache_put(kgpc_double_to_bits(val), (long double)val);
        return 1;
    }

    while (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n')
        p++;
    if (*p != '\0')
    {
        if (out_value != NULL) *out_value = 0.0;
        return 0;
    }

    double val = (int_part + (frac_part / frac_div)) * (double)sign;
    if (out_value != NULL)
        *out_value = val;
    kgpc_real_cache_put(kgpc_double_to_bits(val), (long double)val);
    return 1;
}

/* Convenience wrapper: parse string to double and return as value */
double kgpc_str_to_float(const char *text)
{
    double val = 0.0;
    if (kgpc_env_flag("KGPC_DEBUG_STRTOFLOAT"))
        fprintf(stderr, "[kgpc] str_to_float('%s')\n", text ? text : "(null)");
    (void)kgpc_string_to_real(text, &val);
    if (kgpc_env_flag("KGPC_DEBUG_STRTOFLOAT"))
        fprintf(stderr, "[kgpc] str_to_float -> %g\n", val);
    return val;
}

char *kgpc_get_current_dir(void)
{
#ifdef _WIN32
    DWORD needed = GetCurrentDirectoryA(0, NULL);
    if (needed == 0)
    {
        kgpc_ioresult_set((int)GetLastError());
        return kgpc_alloc_empty_string();
    }
    char *buffer = (char *)malloc((size_t)needed + 1);
    if (buffer == NULL)
    {
        kgpc_ioresult_set(ENOMEM);
        return kgpc_alloc_empty_string();
    }
    if (GetCurrentDirectoryA(needed + 1, buffer) == 0)
    {
        kgpc_ioresult_set((int)GetLastError());
        free(buffer);
        return kgpc_alloc_empty_string();
    }
    char *result = kgpc_string_duplicate(buffer);
    kgpc_ioresult_set(0);
    free(buffer);
    return result;
#else
    size_t max_len = 0;
#ifdef PATH_MAX
    max_len = PATH_MAX;
#endif
    if (max_len == 0)
        max_len = 4096;
    char *buffer = (char *)malloc(max_len);
    if (buffer == NULL)
    {
        kgpc_ioresult_set(ENOMEM);
        return kgpc_alloc_empty_string();
    }
    if (getcwd(buffer, max_len) == NULL)
    {
        kgpc_ioresult_set(errno);
        free(buffer);
        return kgpc_alloc_empty_string();
    }
    char *result = kgpc_string_duplicate(buffer);
    kgpc_ioresult_set(0);
    free(buffer);
    return result;
#endif
}

int kgpc_set_current_dir(const char *path)
{
    if (path == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return EINVAL;
    }
#ifdef _WIN32
    int res = SetCurrentDirectoryA(path) ? 0 : (int)GetLastError();
    kgpc_ioresult_set(res);
    return res;
#else
    int res = (chdir(path) == 0) ? 0 : errno;
    kgpc_ioresult_set(res);
    return res;
#endif
}

char *kgpc_get_environment_variable(const char *name)
{
    if (name == NULL)
        return kgpc_alloc_empty_string();
#ifdef _WIN32
    DWORD needed = GetEnvironmentVariableA(name, NULL, 0);
    if (needed == 0)
        return kgpc_alloc_empty_string();
    char *buffer = (char *)malloc((size_t)needed + 1);
    if (buffer == NULL)
        return kgpc_alloc_empty_string();
    if (GetEnvironmentVariableA(name, buffer, needed + 1) == 0)
    {
        free(buffer);
        return kgpc_alloc_empty_string();
    }
    char *result = kgpc_string_duplicate(buffer);
    free(buffer);
    return result;
#else
    const char *value = getenv(name);
    if (value == NULL)
        return kgpc_alloc_empty_string();
    return kgpc_string_duplicate(value);
#endif
}

int kgpc_set_environment_variable(const char *name, const char *value)
{
    if (name == NULL)
        return EINVAL;
#ifdef _WIN32
    if (SetEnvironmentVariableA(name, value != NULL ? value : "") == 0)
        return (int)GetLastError();
    return 0;
#else
    if (value == NULL)
        value = "";
    return setenv(name, value, 1);
#endif
}

int kgpc_unset_environment_variable(const char *name)
{
    if (name == NULL)
        return EINVAL;
#ifdef _WIN32
    return SetEnvironmentVariableA(name, NULL) ? 0 : (int)GetLastError();
#else
    return unsetenv(name);
#endif
}

int64_t kgpc_get_process_id(void)
{
#ifdef _WIN32
    return (int64_t)GetCurrentProcessId();
#else
    return (int64_t)getpid();
#endif
}

uintptr_t kgpc_load_library(const char *path)
{
#ifdef _WIN32
    HMODULE handle = LoadLibraryA(path);
    return (uintptr_t)handle;
#else
    void *handle = dlopen(path != NULL ? path : NULL, RTLD_LAZY);
    if (handle == NULL && path == NULL)
        handle = dlopen(NULL, RTLD_LAZY);
    return (uintptr_t)handle;
#endif
}

uintptr_t kgpc_get_proc_address(uintptr_t handle, const char *symbol)
{
    if (handle == 0 || symbol == NULL)
        return 0;
#ifdef _WIN32
    return (uintptr_t)GetProcAddress((HMODULE)handle, symbol);
#else
    dlerror();
    void *addr = dlsym((void *)handle, symbol);
    return (uintptr_t)addr;
#endif
}

int kgpc_free_library(uintptr_t handle)
{
    if (handle == 0)
        return 0;
#ifdef _WIN32
    return FreeLibrary((HMODULE)handle) ? 1 : 0;
#else
    return (dlclose((void *)handle) == 0) ? 1 : 0;
#endif
}

typedef struct KgpcFpcDynLibsManager {
    uintptr_t (*LoadLibraryU)(const char *name);
    uintptr_t (*LoadLibraryA)(const char *name);
    void *(*GetProcAddress)(uintptr_t lib, const char *proc_name);
    void *(*GetProcAddressOrdinal)(uintptr_t lib, uintptr_t ordinal);
    int (*UnloadLibrary)(uintptr_t lib);
    char *(*GetLoadErrorStr)(void);
} KgpcFpcDynLibsManager;

typedef struct KgpcStringHeaderShim {
    uint16_t codepage;
    uint16_t elementsize;
    int32_t refcount;
    int64_t length;
} KgpcStringHeaderShim;

/* Defined in Pascal (system.p / system.pp) — referenced as extern here. */
extern KgpcFpcDynLibsManager CurrentDLM;

static const KgpcStringHeaderShim *kgpc_string_header_shim(const char *value)
{
    if (value == NULL)
        return NULL;
    return (const KgpcStringHeaderShim *)(value - (ptrdiff_t)sizeof(KgpcStringHeaderShim));
}

static char *kgpc_fpc_unicode_to_ansi_dup(const char *value)
{
    const KgpcStringHeaderShim *hdr = kgpc_string_header_shim(value);
    if (value == NULL)
        return NULL;
    if (hdr == NULL || hdr->elementsize != 2 || hdr->length <= 0)
        return strdup(value);

    size_t len = (size_t)hdr->length;
    char *ansi = (char *)malloc(len + 1);
    if (ansi == NULL)
        return NULL;
    const uint16_t *src = (const uint16_t *)value;
    for (size_t i = 0; i < len; ++i)
        ansi[i] = (src[i] < 256u) ? (char)src[i] : '?';
    ansi[len] = '\0';
    return ansi;
}

static uintptr_t kgpc_fpc_dynlib_load_u(const char *name)
{
    char *ansi = kgpc_fpc_unicode_to_ansi_dup(name);
    uintptr_t handle = kgpc_load_library(ansi);
    free(ansi);
    return handle;
}

static uintptr_t kgpc_fpc_dynlib_load_a(const char *name)
{
    return kgpc_load_library(name);
}

static void *kgpc_fpc_dynlib_get_proc(uintptr_t lib, const char *proc_name)
{
    return (void *)kgpc_get_proc_address(lib, proc_name);
}

static void *kgpc_fpc_dynlib_get_proc_ordinal(uintptr_t lib, uintptr_t ordinal)
{
#ifdef _WIN32
    return (void *)GetProcAddress((HMODULE)lib, (LPCSTR)(uintptr_t)ordinal);
#else
    (void)lib;
    (void)ordinal;
    return NULL;
#endif
}

static int kgpc_fpc_dynlib_unload(uintptr_t lib)
{
    return kgpc_free_library(lib);
}

static char *kgpc_fpc_dynlib_error(void)
{
#ifdef _WIN32
    DWORD err = GetLastError();
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "LoadLibrary error %lu", (unsigned long)err);
    return kgpc_string_duplicate(buffer);
#else
    const char *msg = dlerror();
    if (msg == NULL)
        msg = "";
    return kgpc_string_duplicate(msg);
#endif
}

__attribute__((constructor))
static void kgpc_fpc_init_dynlibs_manager(void)
{
    CurrentDLM.LoadLibraryU = kgpc_fpc_dynlib_load_u;
    CurrentDLM.LoadLibraryA = kgpc_fpc_dynlib_load_a;
    CurrentDLM.GetProcAddress = kgpc_fpc_dynlib_get_proc;
    CurrentDLM.GetProcAddressOrdinal = kgpc_fpc_dynlib_get_proc_ordinal;
    CurrentDLM.UnloadLibrary = kgpc_fpc_dynlib_unload;
    CurrentDLM.GetLoadErrorStr = kgpc_fpc_dynlib_error;
}

int kgpc_directory_create(const char *path)
{
    if (path == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return EINVAL;
    }
#ifdef _WIN32
    if (_mkdir(path) == 0)
#else
    if (mkdir(path, 0777) == 0)
#endif
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(errno);
    return errno;
}

int kgpc_directory_remove(const char *path)
{
    if (path == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return EINVAL;
    }
#ifdef _WIN32
    if (_rmdir(path) == 0)
#else
    if (rmdir(path) == 0)
#endif
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(errno);
    return errno;
}

int kgpc_file_rename(const char *old_path, const char *new_path)
{
    if (old_path == NULL || new_path == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return EINVAL;
    }
    if (rename(old_path, new_path) == 0)
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(errno);
    return errno;
}

int kgpc_file_exists(const char *path)
{
    if (path == NULL)
        return 0;
#if defined(_WIN32)
    struct _stat st;
    if (_stat(path, &st) != 0)
        return 0;
    return !(_S_IFDIR & st.st_mode);
#else
    struct stat st;
    if (stat(path, &st) != 0)
        return 0;
    return !S_ISDIR(st.st_mode);
#endif
}

int kgpc_directory_exists(const char *path)
{
    if (path == NULL)
        return 0;
#if defined(_WIN32)
    struct _stat st;
    if (_stat(path, &st) != 0)
        return 0;
    return (_S_IFDIR & st.st_mode);
#else
    struct stat st;
    if (stat(path, &st) != 0)
        return 0;
    return S_ISDIR(st.st_mode);
#endif
}

/* FileAge(filename): returns file modification time as DOS date-time stamp,
   or -1 if file doesn't exist. Matches FPC's SysUtils.FileAge. */
int64_t kgpc_fileage(const char *path)
{
    if (path == NULL)
        return -1;
    struct stat st;
    if (stat(path, &st) != 0)
        return -1;
    /* Convert Unix timestamp to DOS date-time format:
       Bits 0-4: seconds/2, 5-10: minutes, 11-15: hours,
       Bits 16-20: day, 21-24: month, 25-31: year-1980 */
    struct tm *tm = localtime(&st.st_mtime);
    if (tm == NULL)
        return -1;
    int dos_time = (tm->tm_sec / 2) | (tm->tm_min << 5) | (tm->tm_hour << 11);
    int dos_date = tm->tm_mday | ((tm->tm_mon + 1) << 5) | ((tm->tm_year - 80) << 9);
    return (int64_t)((dos_date << 16) | dos_time);
}

int kgpc_delete_file(const char *path)
{
    if (path == NULL || path[0] == '\0')
        return 0;
#if defined(_WIN32)
    int rc = _unlink(path);
#else
    int rc = remove(path);
#endif
    return (rc == 0) ? 1 : 0;
}

/* Wrapper for memcpy to be called from assembly code.
   NULL-safe: if src is NULL, zeroes dest (Pascal semantics for nil PChar to shortstring). */
void *kgpc_memcpy_wrapper(void *dest, const void *src, size_t n)
{
    if (src == NULL)
    {
        if (dest != NULL && n > 0)
            memset(dest, 0, n);
        return dest;
    }
    return memcpy(dest, src, n);
}

int64_t kgpc_assigned(const void *ptr)
{
    return (ptr != NULL) ? 1 : 0;
}

int64_t kgpc_aligned(const void *ptr, int64_t alignment)
{
    /* Validate alignment: must be positive */
    assert(alignment > 0);
    return (((uintptr_t)ptr % alignment) == 0) ? 1 : 0;
}

int32_t fpc_in_abs_long(int32_t value)
{
    return (value < 0) ? -value : value;
}

int64_t kgpc_abs_longint(int64_t value)
{
    return (value < 0) ? -value : value;
}

int64_t fpc_in_const_abs(int64_t value)
{
    return (value < 0) ? -value : value;
}

uint64_t kgpc_abs_unsigned(uint64_t value)
{
    return value;
}

double fpc_in_abs_real(double value)
{
    return fabs(value);
}

double fpc_in_sqrt_real(double value)
{
    return sqrt(value);
}

double fpc_in_sin_real(double value)
{
    return sin(value);
}

double kgpc_csc(double value)
{
    return 1.0 / sin(value);
}

double fpc_in_cos_real(double value)
{
    return cos(value);
}

double kgpc_sec(double value)
{
    return 1.0 / cos(value);
}

double kgpc_tan(double value)
{
    return tan(value);
}

double kgpc_cot(double value)
{
    return cos(value) / sin(value);
}

double kgpc_sinh(double value)
{
    return sinh(value);
}

double kgpc_csch(double value)
{
    return 1.0 / sinh(value);
}

double kgpc_cosh(double value)
{
    return cosh(value);
}

double kgpc_sech(double value)
{
    return 1.0 / cosh(value);
}

double kgpc_tanh(double value)
{
    return tanh(value);
}

double kgpc_coth(double value)
{
    double s = sinh(value);
    return s != 0.0 ? cosh(value) / s : (value >= 0.0 ? INFINITY : -INFINITY);
}

double fpc_in_arctan_real(double value)
{
    return atan(value);
}

double kgpc_arccot(double value)
{
    /* Match FPC's ArcCot: returns 0.5*pi for x=0, arctan(1/x) otherwise.
     * Note: for x < 0 this gives (-pi/2, 0) not (pi/2, pi). */
    if (value == 0.0)
        return KGPC_PI / 2.0;
    return atan(1.0 / value);
}

double kgpc_arctan2(double y, double x)
{
    return atan2(y, x);
}

double kgpc_arcsin(double value)
{
    return asin(value);
}

double kgpc_arccos(double value)
{
    return acos(value);
}

double kgpc_arcsinh(double value)
{
    return asinh(value);
}

double kgpc_arctanh(double value)
{
    return atanh(value);
}

double kgpc_arccosh(double value)
{
    return acosh(value);
}

double kgpc_arcsech(double value)
{
    if (value <= 0.0)
        return INFINITY;
    return acosh(1.0 / value);
}

double kgpc_arccsch(double value)
{
    if (value == 0.0)
        return (value >= 0.0) ? INFINITY : -INFINITY;
    return asinh(1.0 / value);
}

double kgpc_arccoth(double value)
{
    if (value == 0.0)
        return (value >= 0.0) ? INFINITY : -INFINITY;
    return atanh(1.0 / value);
}

double kgpc_deg_to_rad(double value)
{
    return value * (KGPC_PI / 180.0);
}

double kgpc_rad_to_deg(double value)
{
    return value * (180.0 / KGPC_PI);
}

double kgpc_deg_to_grad(double value)
{
    return value * (400.0 / 360.0);
}

double kgpc_grad_to_deg(double value)
{
    return value * (360.0 / 400.0);
}

double kgpc_grad_to_rad(double value)
{
    return (value / 200.0) * KGPC_PI;
}

double kgpc_rad_to_grad(double value)
{
    return value * (200.0 / KGPC_PI);
}

double kgpc_cycle_to_rad(double value)
{
    return value * (2.0 * KGPC_PI);
}

double kgpc_rad_to_cycle(double value)
{
    return value / (2.0 * KGPC_PI);
}

double fpc_in_ln_real(double value)
{
    return log(value);
}

double kgpc_logn(double base, double value)
{
    return log(value) / log(base);
}

double fpc_in_exp_real(double value)
{
    return exp(value);
}

double kgpc_power(double base, double exponent)
{
    return pow(base, exponent);
}

double kgpc_hypot(double x, double y)
{
    return hypot(x, y);
}

long long fpc_in_round_real(double value)
{
    double rounded;
    if (value >= 0.0)
        rounded = floor(value + 0.5);
    else
        rounded = ceil(value - 0.5);
    return (long long)rounded;
}

long long fpc_in_trunc_real(double value)
{
    /* Use trunc() to avoid conflict with FPC RTL's Pascal ceil/floor aliases.
     * FPC's math.pp generates ".set ceil, ceil_r" and ".set floor, floor_r"
     * which override C library's ceil/floor symbols when linked together,
     * causing infinite recursion if we called ceil() or floor() here. */
    return (long long)trunc(value);
}

/* Trunc for Currency type - Currency stores values scaled by 10000.
 * The value is passed as a signed 64-bit integer. */
long long kgpc_trunc_currency(long long currency_value)
{
    /* Currency is stored as value * 10000, so divide by 10000 to get actual value */
    /* Truncate towards zero */
    if (currency_value >= 0)
        return currency_value / 10000;
    return -((-currency_value) / 10000);
}

long long kgpc_int(double value)
{
    return fpc_in_trunc_real(value);
}

/* fpc_in_int_real: Int() intrinsic returning the integer part as a double.
   Used by FPC RTL [internproc:fpc_in_int_real]. */
double fpc_in_int_real(double value)
{
    return (double)fpc_in_trunc_real(value);
}

/* fpc_in_pi_real: Pi() intrinsic returning the mathematical constant.
   Used by FPC RTL [internproc:fpc_in_pi_real]. */
double fpc_in_pi_real(void)
{
    return 3.14159265358979323846;
}

double fpc_in_frac_real(double value)
{
    return value - (double)fpc_in_trunc_real(value);
}

long long kgpc_ceil(double value)
{
    /* Avoid C ceil() — FPC RTL aliases it to Pascal ceil_r. Use trunc+1. */
    long long t = (long long)trunc(value);
    return (value > (double)t) ? t + 1 : t;
}

long long kgpc_floor(double value)
{
    /* Avoid C floor() — FPC RTL aliases it to Pascal floor_r. Use trunc-1. */
    long long t = (long long)trunc(value);
    return (value < (double)t) ? t - 1 : t;
}

static uint32_t kgpc_rol32(uint32_t value, uint32_t shift)
{
    return (value << shift) | (value >> (32u - shift));
}

static uint64_t kgpc_splitmix64_next(uint64_t *state)
{
    uint64_t z = *state + 0x9e3779b97f4a7c15ULL;
    *state = z;
    z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
    z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
    return z ^ (z >> 31);
}

static void kgpc_xsr128_setup(uint32_t seed)
{
    uint64_t sm_state = (uint64_t)seed;
    uint64_t x = kgpc_splitmix64_next(&sm_state);
    kgpc_xsr_state[0] = (uint32_t)x;
    kgpc_xsr_state[1] = (uint32_t)(x >> 32);
    x = kgpc_splitmix64_next(&sm_state);
    kgpc_xsr_state[2] = (uint32_t)x;
    kgpc_xsr_state[3] = (uint32_t)(x >> 32);
}

static uint32_t kgpc_xsr128_next(void)
{
    uint32_t s0 = kgpc_xsr_state[0];
    uint32_t s1 = kgpc_xsr_state[1];
    uint32_t s2 = kgpc_xsr_state[2];
    uint32_t s3 = kgpc_xsr_state[3];

    uint32_t result = kgpc_rol32(s1 * 5u, 7) * 9u;
    uint32_t t = s1 << 9;

    s2 ^= s0;
    s3 ^= s1;
    s1 ^= s2;
    s0 ^= s3;
    s2 ^= t;
    s3 = kgpc_rol32(s3, 11);

    kgpc_xsr_state[0] = s0;
    kgpc_xsr_state[1] = s1;
    kgpc_xsr_state[2] = s2;
    kgpc_xsr_state[3] = s3;
    return result;
}

static uint32_t kgpc_xsr128_u32rand(void)
{
    if (kgpc_randseed != kgpc_old_randseed)
    {
        kgpc_xsr128_setup(kgpc_randseed);
        kgpc_randseed = ~kgpc_randseed;
        kgpc_old_randseed = kgpc_randseed;
    }
    return kgpc_xsr128_next();
}

void kgpc_randomize(void)
{
    kgpc_randseed = (uint32_t)time(NULL);
}

double kgpc_random_real(void)
{
    uint32_t r0 = kgpc_xsr128_u32rand();
    uint64_t mantissa = (uint64_t)(r0 & ((1u << 20) - 1u)) << 32;
    mantissa |= (uint64_t)kgpc_xsr128_u32rand();

    int32_t exponent = 1023 - 1 - 31;
    if ((r0 >> 20) == 0)
    {
        exponent += 20;
        do
        {
            exponent -= 32;
            r0 = kgpc_xsr128_u32rand();
        } while (r0 == 0);
    }

    uint32_t bsr = 31u - (uint32_t)__builtin_clz(r0);
    uint64_t bits = mantissa | ((uint64_t)(exponent + (int32_t)bsr) << 52);
    double result = 0.0;
    memcpy(&result, &bits, sizeof(result));
    return result;
}

double kgpc_random_real_upper(double upper)
{
    if (upper <= 0.0)
        return 0.0;
    return kgpc_random_real() * upper;
}

static uint64_t kgpc_random_u64_bounded(uint64_t bound)
{
    if (bound == 0)
        return 0;

    /* For 32-bit bounds, use a faster path with 64-bit multiplication. */
    if (bound <= 0xFFFFFFFFu)
    {
        uint32_t b32 = (uint32_t)bound;
        uint64_t m = (uint64_t)kgpc_xsr128_u32rand() * (uint64_t)b32;
        if ((uint32_t)m < b32)
        {
            uint32_t t = (uint32_t)(0u - b32) % b32;
            while ((uint32_t)m < t)
                m = (uint64_t)kgpc_xsr128_u32rand() * (uint64_t)b32;
        }
        return m >> 32;
    }

    /* For larger bounds, use rejection sampling with 128-bit multiplication (Lemire's method). */
    uint32_t a = kgpc_xsr128_u32rand();
    __uint128_t prod = ((__uint128_t)a << 32) | kgpc_xsr128_u32rand();
    __uint128_t full = prod * bound;
    uint64_t mLo = (uint64_t)full;
    uint64_t mHi = (uint64_t)(full >> 64);

    if (mLo < bound)
    {
        uint64_t t = (uint64_t)(0u - bound) % bound;
        while (mLo < t)
        {
            a = kgpc_xsr128_u32rand();
            prod = ((__uint128_t)a << 32) | kgpc_xsr128_u32rand();
            full = prod * bound;
            mLo = (uint64_t)full;
            mHi = (uint64_t)(full >> 64);
        }
    }
    return mHi;
}

int64_t kgpc_random_int(int64_t upper)
{
    uint64_t ul = (uint64_t)upper;
    if (upper < 0)
        ul = ~ul;

    uint64_t res = kgpc_random_u64_bounded(ul);

    int64_t result = (int64_t)res;
    if (upper < -1)
        result = -result - 1;
    return result;
}

int64_t kgpc_random_int64(int64_t upper)
{
    return kgpc_random_int(upper);
}

int64_t kgpc_random_range(int64_t low, int64_t high)
{
    if (high <= low)
        return low;
    return low + kgpc_random_int64(high - low);
}

uint32_t kgpc_get_randseed(void)
{
    return kgpc_randseed;
}

void kgpc_set_randseed(uint32_t seed)
{
    kgpc_randseed = seed;
}

#if defined(__GLIBC__) || defined(__linux__)
extern void sincos(double, double *, double *);
#define KGPC_HAVE_SINCOS 1
#endif

void kgpc_sincos_bits(int64_t angle_bits, double *sin_out, double *cos_out)
{
    double angle = kgpc_bits_to_double(angle_bits);
#ifdef KGPC_HAVE_SINCOS
    sincos(angle, sin_out, cos_out);
#else
    if (sin_out != NULL)
        *sin_out = sin(angle);
    if (cos_out != NULL)
        *cos_out = cos(angle);
#endif
}


void Halt(int64_t code)
{
    exit((int)code);
}

/* Called when an abstract method is invoked - should never happen in correct code */
void __kgpc_abstract_method_error(void)
{
    fprintf(stderr, "Runtime error: Abstract method called\n");
    abort();
}

/* FPC compiler intrinsic stubs: get_frame, get_pc_addr, etc.
 * These return the current stack frame pointer for exception handling.
 * The _void suffix variant is needed because KGPC's codegen mangles
 * zero-argument functions with _void. */
void *kgpc_get_frame(void)
{
    return __builtin_frame_address(0);
}

void *kgpc_get_frame_void(void)
{
    return __builtin_frame_address(0);
}

void *kgpc_get_frame_p(void *frame)
{
    (void)frame;
    return __builtin_frame_address(0);
}

/* FPC atomic intrinsics — these are [internproc] builtins in FPC that have
 * no Pascal body.  KGPC emits calls to the mangled names below. */
long atomicincrement_i_i(long *target, long value) { return (long)KGPC_ATOMIC_INC_RET(target, value); }
uint32_t atomicincrement_u32_u32(uint32_t *target, uint32_t value) { return (uint32_t)KGPC_ATOMIC_INC_RET(target, value); }

/* [internproc] AtomicIncrement(var Target): 1-param overload, increments by 1 */
long atomicincrement_i(long *target) { return (long)KGPC_ATOMIC_INC_RET(target, 1); }
uint32_t atomicincrement_u32(uint32_t *target) { return (uint32_t)KGPC_ATOMIC_INC_RET(target, 1); }

/* LongInt (32-bit signed) overloads for compiler intrinsics. */
int32_t atomicincrement_li(int32_t *target) { return (int32_t)KGPC_ATOMIC_INC_RET(target, 1); }
int32_t atomicincrement_li_li(int32_t *target, int32_t value) { return (int32_t)KGPC_ATOMIC_INC_RET(target, value); }

long atomicdecrement_i(long *target) { return (long)KGPC_ATOMIC_DEC_RET(target, 1); }
uint32_t atomicdecrement_u32(uint32_t *target) { return (uint32_t)KGPC_ATOMIC_DEC_RET(target, 1); }
int32_t atomicdecrement_li(int32_t *target) { return (int32_t)KGPC_ATOMIC_DEC_RET(target, 1); }
int32_t atomicdecrement_li_li(int32_t *target, int32_t value) { return (int32_t)KGPC_ATOMIC_DEC_RET(target, value); }

int32_t atomicexchange_li_li(int32_t *target, int32_t new_val) { return (int32_t)KGPC_ATOMIC_EXCHANGE(target, new_val); }
int32_t atomiccmpexchange_li_li_li(int32_t *target, int32_t new_val, int32_t comparand) { return (int32_t)KGPC_ATOMIC_CMP_EXCHANGE(target, new_val, comparand); }
void *atomiccmpexchange_p_p_p(void **target, void *new_val, void *comparand) { return (void *)KGPC_ATOMIC_CMP_EXCHANGE(target, new_val, comparand); }
void *atomicexchange_p_p(void **target, void *new_val) { return (void *)KGPC_ATOMIC_EXCHANGE(target, new_val); }

/* [internproc] AtomicExchange for integer types */
long atomicexchange_i_i(long *target, long new_val) { return (long)KGPC_ATOMIC_EXCHANGE(target, new_val); }
uint32_t atomicexchange_u32_u32(uint32_t *target, uint32_t new_val) { return (uint32_t)KGPC_ATOMIC_EXCHANGE(target, new_val); }

/* Int64 overloads of atomic intrinsics */
long long atomicincrement_i64(long long *target) { return (long long)KGPC_ATOMIC_INC_RET(target, 1); }
uint64_t atomicincrement_u64(uint64_t *target) { return (uint64_t)KGPC_ATOMIC_INC_RET(target, 1); }
long long atomicincrement_i64_i64(long long *target, long long value) { return (long long)KGPC_ATOMIC_INC_RET(target, value); }
uint64_t atomicincrement_u64_u64(uint64_t *target, uint64_t value) { return (uint64_t)KGPC_ATOMIC_INC_RET(target, value); }

long long atomicdecrement_i64(long long *target) { return (long long)KGPC_ATOMIC_DEC_RET(target, 1); }
uint64_t atomicdecrement_u64(uint64_t *target) { return (uint64_t)KGPC_ATOMIC_DEC_RET(target, 1); }

long long atomicexchange_i64_i64(long long *target, long long new_val) { return (long long)KGPC_ATOMIC_EXCHANGE(target, new_val); }
uint64_t atomicexchange_u64_u64(uint64_t *target, uint64_t new_val) { return (uint64_t)KGPC_ATOMIC_EXCHANGE(target, new_val); }

long long atomiccmpexchange_i64_i64_i64(long long *target, long long new_val, long long comparand) { return (long long)KGPC_ATOMIC_CMP_EXCHANGE(target, new_val, comparand); }
uint32_t atomiccmpexchange_u32_u32_u32(uint32_t *target, uint32_t new_val, uint32_t comparand) { return (uint32_t)KGPC_ATOMIC_CMP_EXCHANGE(target, new_val, comparand); }
uint64_t atomiccmpexchange_u64_u64_u64(uint64_t *target, uint64_t new_val, uint64_t comparand) { return (uint64_t)KGPC_ATOMIC_CMP_EXCHANGE(target, new_val, comparand); }

/* FPC_INTERLOCKEDEXCHANGEADD / FPC_INTERLOCKEDCOMPAREEXCHANGE64:
 * Now provided by the compiler-emitted FPC Pascal code (via [Public,Alias] in
 * system.pp).  Removed from runtime to avoid duplicate symbol conflicts. */

uint8_t fpc_in_lo_Word(uint16_t value)
{
    return (uint8_t)value;
}

uint8_t fpc_in_hi_Word(uint16_t value)
{
    return (uint8_t)(value >> 8);
}

uint16_t fpc_in_lo_long(int32_t value)
{
    return (uint16_t)value;
}

uint16_t fpc_in_hi_long(int32_t value)
{
    return (uint16_t)((uint32_t)value >> 16);
}

uint32_t fpc_in_lo_qword(uint64_t value)
{
    return (uint32_t)value;
}

uint32_t fpc_in_hi_qword(uint64_t value)
{
    return (uint32_t)(value >> 32);
}

/* Set operations for 256-bit sets (set of Char / set of AnsiChar).
   Each set is 32 bytes = 4 x uint64. */
void kgpc_set_union_256(void *dest, const void *a, const void *b)
{
    const uint64_t *sa = (const uint64_t *)a, *sb = (const uint64_t *)b;
    uint64_t *d = (uint64_t *)dest;
    d[0] = sa[0] | sb[0]; d[1] = sa[1] | sb[1];
    d[2] = sa[2] | sb[2]; d[3] = sa[3] | sb[3];
}

void kgpc_set_intersect_256(void *dest, const void *a, const void *b)
{
    const uint64_t *sa = (const uint64_t *)a, *sb = (const uint64_t *)b;
    uint64_t *d = (uint64_t *)dest;
    d[0] = sa[0] & sb[0]; d[1] = sa[1] & sb[1];
    d[2] = sa[2] & sb[2]; d[3] = sa[3] & sb[3];
}

void kgpc_set_diff_256(void *dest, const void *a, const void *b)
{
    const uint64_t *sa = (const uint64_t *)a, *sb = (const uint64_t *)b;
    uint64_t *d = (uint64_t *)dest;
    d[0] = sa[0] & ~sb[0]; d[1] = sa[1] & ~sb[1];
    d[2] = sa[2] & ~sb[2]; d[3] = sa[3] & ~sb[3];
}

void kgpc_set_symdiff_256(void *dest, const void *a, const void *b)
{
    const uint64_t *sa = (const uint64_t *)a, *sb = (const uint64_t *)b;
    uint64_t *d = (uint64_t *)dest;
    d[0] = sa[0] ^ sb[0]; d[1] = sa[1] ^ sb[1];
    d[2] = sa[2] ^ sb[2]; d[3] = sa[3] ^ sb[3];
}

/* _haltproc: asm-only startup procedure from si_prc.inc / si_c.inc.
   There is no Pascal body — only hand-written assembly in FPC RTL.
   Our runtime provides a C equivalent.
   The FPC RTL's system_exit passes the exit code (from operatingsystem_result)
   as the first argument, so we must accept and forward it. */
void _haltproc(int exitcode)
{
    exit(exitcode);
}

/* atomiccmpexchange_i_i_i: [internproc] AtomicCmpExchange intrinsic.
   No Pascal body exists — this is a compiler intrinsic. */
int atomiccmpexchange_i_i_i(int *target, int new_val, int comparand) { return (int)KGPC_ATOMIC_CMP_EXCHANGE(target, new_val, comparand); }

/* =====================================================================
 * TUnicodeStringManager (widestringmanager) initialization.
 *
 * FPC's system unit normally calls initunicodestringmanager() to fill
 * widestringmanager with default callbacks.  Since KGPC's --no-stdlib
 * build may not compile that init code, we provide C stubs and call
 * the init from kgpc_init_args (which runs before the program body).
 *
 * TUnicodeStringManager layout (25 function pointers, 200 bytes):
 *   [0]  Wide2AnsiMoveProc
 *   [1]  Ansi2WideMoveProc
 *   [2]  UpperWideStringProc
 *   [3]  LowerWideStringProc
 *   [4]  CompareWideStringProc
 *   [5]  CharLengthPCharProc
 *   [6]  CodePointLengthProc
 *   [7]  UpperAnsiStringProc
 *   [8]  LowerAnsiStringProc
 *   [9]  CompareStrAnsiStringProc
 *   [10] CompareTextAnsiStringProc
 *   [11] StrCompAnsiStringProc
 *   [12] StrICompAnsiStringProc
 *   [13] StrLCompAnsiStringProc
 *   [14] StrLICompAnsiStringProc
 *   [15] StrLowerAnsiStringProc
 *   [16] StrUpperAnsiStringProc
 *   [17] ThreadInitProc
 *   [18] ThreadFiniProc
 *   [19] Unicode2AnsiMoveProc
 *   [20] Ansi2UnicodeMoveProc
 *   [21] UpperUnicodeStringProc
 *   [22] LowerUnicodeStringProc
 *   [23] CompareUnicodeStringProc
 *   [24] GetStandardCodePageProc
 * ===================================================================== */

/* The widestringmanager global — array of 25 function pointers for
 * FPC's TWideStringManager record.  Defined by the compiler from
 * system.p (or FPC's system.pp), populated at startup. */
extern void *widestringmanager[25];

/* Default: convert wide/unicode chars to ansi by truncating to low byte.
   Signature: procedure(source:punicodechar; var dest:RawByteString;
                         cp:TSystemCodePage; len:SizeInt)
   SysV ABI: rdi=source, rsi=&dest, rdx=cp, rcx=len */
static void kgpc_default_unicode2ansi_move(const uint16_t *source,
    char **dest, int32_t cp, int64_t len)
{
    if (len <= 0 || source == NULL)
    {
        kgpc_string_setlength(dest, 0);
        return;
    }
    /* Check if source is actually an AnsiString (elementsize==1).
       KGPC aliases UnicodeString=AnsiString, so callers may pass
       1-byte char data through PUnicodeChar parameters. */
    int src_is_ansi = 0;
    if (kgpc_string_is_managed((const char *)source))
    {
        KgpcStringHeader *shdr = (KgpcStringHeader *)((const char *)source - (int64_t)sizeof(KgpcStringHeader));
        if (shdr->elementsize == 1)
            src_is_ansi = 1;
    }
    kgpc_string_setlength(dest, len);
    if (*dest == NULL)
        return;
    /* Set codepage in the header */
    KgpcStringHeader *hdr = kgpc_string_header(*dest);
    if (hdr != NULL)
        hdr->codepage = (uint16_t)cp;
    char *p = *dest;
    if (src_is_ansi)
    {
        const char *asrc = (const char *)source;
        for (int64_t i = 0; i < len; i++)
            p[i] = asrc[i];
    }
    else
    {
        for (int64_t i = 0; i < len; i++)
            p[i] = (source[i] < 256) ? (char)source[i] : '?';
    }
}

/* Default: convert ansi chars to unicode by zero-extending.
   Signature: procedure(source:pansichar; cp:TSystemCodePage;
                         var dest:unicodestring; len:SizeInt)
   SysV ABI: rdi=source, rsi=cp, rdx=&dest, rcx=len

   UnicodeString in FPC is a reference-counted array of uint16_t with the
   same TAnsiRec header (but elementsize=2).  We allocate the header+data
   manually since KGPC doesn't have full UnicodeString runtime support. */
static void kgpc_default_ansi2unicode_move(const char *source,
    int32_t cp, uint16_t **dest, int64_t len)
{
    /* Free existing value */
    if (*dest != NULL)
        kgpc_string_release((char *)*dest);

    if (len <= 0) {
        *dest = NULL;
        return;
    }

    /* Allocate: header + len*2 bytes + 2 byte null terminator */
    size_t data_bytes = (size_t)len * 2 + 2;
    KgpcStringHeader *hdr = (KgpcStringHeader *)malloc(sizeof(KgpcStringHeader) + data_bytes);
    if (hdr == NULL) {
        *dest = NULL;
        return;
    }
    hdr->codepage = 1200;       /* UTF-16LE */
    hdr->elementsize = 2;       /* UnicodeChar = 2 bytes */
    hdr->refcount = 1;
    hdr->length = len;
    uint16_t *data = (uint16_t *)(hdr + 1);
    for (int64_t i = 0; i < len; i++)
        data[i] = (uint16_t)(unsigned char)source[i];
    data[len] = 0;  /* null terminator */
    kgpc_string_set_insert((char *)data);
    *dest = data;
}

/* Default Wide2Ansi — same as Unicode2Ansi for our purposes */
static void kgpc_default_wide2ansi_move(const uint16_t *source,
    char **dest, int32_t cp, int64_t len)
{
    kgpc_default_unicode2ansi_move(source, dest, cp, len);
}

/* Default Ansi2Wide — same as Ansi2Unicode */
static void kgpc_default_ansi2wide_move(const char *source,
    int32_t cp, uint16_t **dest, int64_t len)
{
    kgpc_default_ansi2unicode_move(source, cp, dest, len);
}

/* Upper/LowerAnsiString: return a new string with case changed */
static char *kgpc_default_upper_ansistring(const char *s)
{
    size_t len = kgpc_string_known_length(s);
    char *result = kgpc_string_alloc_with_length(len);
    if (result == NULL) return kgpc_alloc_empty_string();
    for (size_t i = 0; i < len; i++)
        result[i] = (char)toupper((unsigned char)s[i]);
    return result;
}

static char *kgpc_default_lower_ansistring(const char *s)
{
    size_t len = kgpc_string_known_length(s);
    char *result = kgpc_string_alloc_with_length(len);
    if (result == NULL) return kgpc_alloc_empty_string();
    for (size_t i = 0; i < len; i++)
        result[i] = (char)tolower((unsigned char)s[i]);
    return result;
}

/* CompareStr: case-sensitive AnsiString comparison */
static int64_t kgpc_default_comparestr_ansistring(const char *s1, const char *s2)
{
    size_t len1 = kgpc_string_known_length(s1);
    size_t len2 = kgpc_string_known_length(s2);
    size_t minlen = (len1 < len2) ? len1 : len2;
    int r = memcmp(s1 ? s1 : "", s2 ? s2 : "", minlen);
    if (r != 0) return (int64_t)r;
    if (len1 < len2) return -1;
    if (len1 > len2) return 1;
    return 0;
}

/* CompareText: case-insensitive AnsiString comparison */
static int64_t kgpc_default_comparetext_ansistring(const char *s1, const char *s2)
{
    if (s1 == NULL) s1 = "";
    if (s2 == NULL) s2 = "";
    return (int64_t)strcasecmp(s1, s2);
}

/* PAnsiChar comparisons */
static int64_t kgpc_default_strcomp(const char *s1, const char *s2)
{
    return (int64_t)strcmp(s1 ? s1 : "", s2 ? s2 : "");
}

static int64_t kgpc_default_stricomp(const char *s1, const char *s2)
{
    return (int64_t)strcasecmp(s1 ? s1 : "", s2 ? s2 : "");
}

static int64_t kgpc_default_strlcomp(const char *s1, const char *s2, uint64_t maxlen)
{
    return (int64_t)strncmp(s1 ? s1 : "", s2 ? s2 : "", (size_t)maxlen);
}

static int64_t kgpc_default_strlicomp(const char *s1, const char *s2, uint64_t maxlen)
{
    return (int64_t)strncasecmp(s1 ? s1 : "", s2 ? s2 : "", (size_t)maxlen);
}

/* In-place PAnsiChar case conversion */
static char *kgpc_default_strlower(char *str)
{
    if (str == NULL) return str;
    for (char *p = str; *p; p++)
        *p = (char)tolower((unsigned char)*p);
    return str;
}

static char *kgpc_default_strupper(char *str)
{
    if (str == NULL) return str;
    for (char *p = str; *p; p++)
        *p = (char)toupper((unsigned char)*p);
    return str;
}

/* Stub wide/unicode case: just return the input unchanged */
static void *kgpc_stub_widecase(void *s) { return s; }

/* Stub wide/unicode compare: byte comparison */
static int64_t kgpc_stub_compare_wide(void *s1, void *s2, int64_t opts)
{
    (void)opts;
    if (s1 == s2) return 0;
    if (s1 == NULL) return -1;
    if (s2 == NULL) return 1;
    return 0;  /* stub: treat as equal */
}

/* CharLengthPChar: just strlen for single-byte encodings */
static int64_t kgpc_default_charlength_pchar(const char *str)
{
    return str ? (int64_t)strlen(str) : 0;
}

/* CodePointLength: 1 if non-null char, 0 otherwise */
static int64_t kgpc_default_codepoint_length(const char *str, int64_t maxlookahead)
{
    (void)maxlookahead;
    return (str != NULL && str[0] != '\0') ? 1 : 0;
}

/* Stub thread init/fini: no-ops */
static void kgpc_stub_thread_noop(void) {}

/* GetStandardCodePage: return 0 (system default) */
extern int32_t DefaultSystemCodePage;
extern int32_t DefaultFileSystemCodePage;
static int32_t kgpc_default_get_standard_codepage(int32_t stdcp)
{
    if (stdcp != 2)  /* scpFileSystemSingleByte = 2 */
        return DefaultSystemCodePage;
    return DefaultFileSystemCodePage;
}

/* Initialize widestringmanager with default C callbacks.
   Called from kgpc_init_args before the program body runs. */
void kgpc_init_widestringmanager(void)
{
    widestringmanager[0]  = (void *)kgpc_default_wide2ansi_move;
    widestringmanager[1]  = (void *)kgpc_default_ansi2wide_move;
    widestringmanager[2]  = (void *)kgpc_stub_widecase;             /* UpperWide */
    widestringmanager[3]  = (void *)kgpc_stub_widecase;             /* LowerWide */
    widestringmanager[4]  = (void *)kgpc_stub_compare_wide;         /* CompareWide */
    widestringmanager[5]  = (void *)kgpc_default_charlength_pchar;
    widestringmanager[6]  = (void *)kgpc_default_codepoint_length;
    widestringmanager[7]  = (void *)kgpc_default_upper_ansistring;
    widestringmanager[8]  = (void *)kgpc_default_lower_ansistring;
    widestringmanager[9]  = (void *)kgpc_default_comparestr_ansistring;
    widestringmanager[10] = (void *)kgpc_default_comparetext_ansistring;
    widestringmanager[11] = (void *)kgpc_default_strcomp;
    widestringmanager[12] = (void *)kgpc_default_stricomp;
    widestringmanager[13] = (void *)kgpc_default_strlcomp;
    widestringmanager[14] = (void *)kgpc_default_strlicomp;
    widestringmanager[15] = (void *)kgpc_default_strlower;
    widestringmanager[16] = (void *)kgpc_default_strupper;
    widestringmanager[17] = (void *)kgpc_stub_thread_noop;          /* ThreadInit */
    widestringmanager[18] = (void *)kgpc_stub_thread_noop;          /* ThreadFini */
    widestringmanager[19] = (void *)kgpc_default_unicode2ansi_move;
    widestringmanager[20] = (void *)kgpc_default_ansi2unicode_move;
    widestringmanager[21] = (void *)kgpc_stub_widecase;             /* UpperUnicode */
    widestringmanager[22] = (void *)kgpc_stub_widecase;             /* LowerUnicode */
    widestringmanager[23] = (void *)kgpc_stub_compare_wide;         /* CompareUnicode */
    widestringmanager[24] = (void *)kgpc_default_get_standard_codepage;
}

/* =========================================================================
   FPC intrinsic bit manipulation functions
   ========================================================================= */

int64_t kgpc_sar_int64(int64_t value, int32_t shift) {
    return value >> (shift & 63);
}

/* FPC internal intrinsic stub — the FPC compiler would inline SAR
 * instructions, but since we emit a regular call we need a runtime
 * symbol.  The generic version uses 64-bit arithmetic. */
int64_t fpc_in_sar_x_y(int64_t value, int64_t shift) {
    return value >> (shift & 63);
}

int32_t kgpc_sar_longint(int32_t value, int32_t shift) {
    return value >> (shift & 31);
}

int16_t kgpc_sar_smallint(int16_t value, int32_t shift) {
    return (int16_t)(((int32_t)value) >> (shift & 15));
}

int8_t kgpc_sar_shortint(int8_t value, int32_t shift) {
    return (int8_t)(((int32_t)value) >> (shift & 7));
}

uint32_t kgpc_rol_dword(uint32_t value, int32_t shift) {
    shift &= 31;
    return (value << shift) | (value >> (32 - shift));
}

uint32_t kgpc_ror_dword(uint32_t value, int32_t shift) {
    shift &= 31;
    return (value >> shift) | (value << (32 - shift));
}

uint64_t kgpc_rol_qword(uint64_t value, int32_t shift) {
    shift &= 63;
    return (value << shift) | (value >> (64 - shift));
}

uint64_t kgpc_ror_qword(uint64_t value, int32_t shift) {
    shift &= 63;
    return (value >> shift) | (value << (64 - shift));
}

uint16_t kgpc_rol_word(uint16_t value, int32_t shift) {
    shift &= 15;
    return (uint16_t)((value << shift) | (value >> (16 - shift)));
}

uint16_t kgpc_ror_word(uint16_t value, int32_t shift) {
    shift &= 15;
    return (uint16_t)((value >> shift) | (value << (16 - shift)));
}

uint8_t kgpc_rol_byte(uint8_t value, int32_t shift) {
    shift &= 7;
    return (uint8_t)((value << shift) | (value >> (8 - shift)));
}

uint8_t kgpc_ror_byte(uint8_t value, int32_t shift) {
    shift &= 7;
    return (uint8_t)((value >> shift) | (value << (8 - shift)));
}

int32_t kgpc_compare_mem(const void *p1, const void *p2, int64_t count) {
    if (count <= 0) return 1; /* true */
    return memcmp(p1, p2, (size_t)count) == 0 ? 1 : 0;
}

void kgpc_prefetch(const void *p) {
    (void)p;
    /* No-op; just a hint */
}

uint32_t kgpc_hi_qword(uint64_t value) { return (uint32_t)(value >> 32); }
uint16_t kgpc_hi_dword(uint32_t value) { return (uint16_t)(value >> 16); }
uint8_t  kgpc_hi_word(uint16_t value)  { return (uint8_t)(value >> 8); }
uint32_t kgpc_lo_qword(uint64_t value) { return (uint32_t)(value & 0xFFFFFFFFu); }
uint16_t kgpc_lo_dword(uint32_t value) { return (uint16_t)(value & 0xFFFF); }
uint8_t  kgpc_lo_word(uint16_t value)  { return (uint8_t)(value & 0xFF); }

void kgpc_runerror(int32_t code) {
    fprintf(stderr, "Runtime error %d\n", code);
    exit(code);
}

/* GetMemory/FreeMemory/ReallocMemory - C heap wrappers */
void *kgpc_getmem_ptr(size_t size) { return malloc(size); }
size_t kgpc_freemem_ptr(void *p) { free(p); return 0; }
void *kgpc_reallocmem_ptr(void *p, size_t size) { return realloc(p, size); }

/* Memory barriers - no-ops on x86 (strong memory model) */
void kgpc_readbarrier(void) {}
void kgpc_writebarrier(void) {}
void kgpc_readwritebarrier(void) {}

/* SysBeep — system procedure that produces a beep.
 * Not meaningful in a CLI/batch context; no-op. */
void SysBeep(void) {}

/* TMarshal.UnfixArray<TPtrWrapper> — generic method specialization.
 * The body is Finalize(TArray<TPtrWrapper>) which is a no-op for
 * TPtrWrapper (a simple record with no managed fields). */
void tmarshal__unfixarray_u_tptrwrapper(void) {}

/* FindComponentClass — FPC TReader class method referenced but not
 * exercised in test programs. Raises abstract method error if called. */
void FindComponentClass(void) { __kgpc_abstract_method_error(); }

/* ReadDeltaStream — FPC TReader class method referenced but not
 * exercised in test programs. Raises abstract method error if called. */
void ReadDeltaStream(void) { __kgpc_abstract_method_error(); }

/* Default IInterface implementations for non-reference-counted classes.
 * Classes that implement interfaces without inheriting from TInterfacedObject
 * (e.g. TList = class(TObject, IFPObserved)) need default QueryInterface,
 * _AddRef, and _Release methods. These behave like FPC's defaults:
 * no reference counting, QueryInterface delegates to GetInterface. */
int64_t kgpc_default_queryinterface(const void *self, const void *guid, void **out_intf)
{
    if (kgpc_get_interface(self, guid, out_intf))
        return 0;  /* S_OK */
    return (int64_t)0x80004002u;  /* E_NOINTERFACE */
}

int64_t kgpc_default_addref(const void *self)
{
    (void)self;
    return -1;
}

int64_t kgpc_default_release(const void *self)
{
    (void)self;
    return -1;
}
