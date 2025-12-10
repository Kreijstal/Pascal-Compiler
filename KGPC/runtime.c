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

#include "runtime_internal.h"
#include <sys/stat.h>
#include <limits.h>
#include "format_arg.h"

static const double KGPC_PI = 3.14159265358979323846264338327950288;
static uint64_t kgpc_rand_state = 0x9e3779b97f4a7c15ULL;

/* Forward decl for optional debug flag helper */
static int kgpc_env_flag(const char *name);

#ifdef _WIN32
#include <windows.h>
#include <time.h>
#include <errno.h>
#include <io.h>
#include <fcntl.h>
#include <direct.h>
#else
#include <time.h>
#include <errno.h>
#include <unistd.h>
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

int64_t kgpc_current_exception = 0;
static __thread int kgpc_ioresult = 0;

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


typedef enum {
    KGPC_FILE_KIND_TEXT = 0,
    KGPC_FILE_KIND_BINARY = 1
} KGPCFileKind;

typedef enum {
    KGPC_BINARY_UNSPECIFIED = 0,
    KGPC_BINARY_INT32,
    KGPC_BINARY_CHAR,
    KGPC_BINARY_DOUBLE
} KGPCBinaryType;

typedef struct KGPCTextFile
{
    FILE *handle;
    char *path;
    int mode;
    KGPCFileKind kind;
    KGPCBinaryType element_type;
    size_t element_size;
} KGPCTextFile;

int kgpc_file_is_text(void **slot)
{
    if (slot == NULL)
        return 1;
    KGPCTextFile *file = (KGPCTextFile *)(*slot);
    if (file == NULL)
        return 1;
    return (file->kind != KGPC_FILE_KIND_BINARY);
}

static FILE *kgpc_text_output_stream(KGPCTextFile *file)
{
    if (file != NULL && file->handle != NULL)
        return file->handle;
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

static FILE *kgpc_text_input_stream(KGPCTextFile *file)
{
    if (file != NULL)
        return file->handle;
    return stdin;
}

/* ------------------------------------------------------------------
 * Typed file support (binary files: file of Integer/Char/Real).
 *
 * These helpers share the same descriptor layout as text files but
 * open the underlying FILE* streams in binary mode and use fread/fwrite
 * to read/write fixed-size elements.
 * ------------------------------------------------------------------ */

void kgpc_tfile_assign(KGPCTextFile **slot, const char *path)
{
    /* For now, reuse the same assign logic as text files. */
    if (slot == NULL)
        return;

    KGPCTextFile *file = *slot;
    if (file == NULL)
    {
        file = (KGPCTextFile *)calloc(1, sizeof(KGPCTextFile));
        if (file == NULL)
        {
            kgpc_ioresult_set(EACCES);
            return;
        }
        *slot = file;
    }

    file->kind = KGPC_FILE_KIND_BINARY;
    file->element_type = KGPC_BINARY_UNSPECIFIED;
    file->element_size = 0;
    if (file->handle != NULL)
    {
        fclose(file->handle);
        file->handle = NULL;
    }
    if (file->path != NULL)
    {
        free(file->path);
        file->path = NULL;
    }

    if (path != NULL)
    {
        size_t len = strlen(path);
        file->path = (char *)malloc(len + 1);
        if (file->path != NULL)
            memcpy(file->path, path, len + 1);
    }

    if (file->element_type == KGPC_BINARY_UNSPECIFIED)
    {
        file->element_type = KGPC_BINARY_INT32;
        file->element_size = sizeof(int32_t);
    }
}

void kgpc_tfile_configure(KGPCTextFile **slot, size_t element_size, int element_tag)
{
    if (slot == NULL)
        return;
    KGPCTextFile *file = *slot;
    if (file == NULL)
        return;
    file->kind = KGPC_FILE_KIND_BINARY;
    if (element_size > 0)
        file->element_size = element_size;

    switch (element_tag)
    {
        case HASHVAR_CHAR:
            file->element_type = KGPC_BINARY_CHAR;
            break;
        case HASHVAR_REAL:
            file->element_type = KGPC_BINARY_DOUBLE;
            break;
        case HASHVAR_INTEGER:
        case HASHVAR_LONGINT:
        default:
            file->element_type = KGPC_BINARY_INT32;
            break;
    }
}

void kgpc_tfile_rewrite(KGPCTextFile **slot)
{
    if (slot == NULL)
        return;

    KGPCTextFile *file = *slot;
    if (file == NULL || file->path == NULL)
        return;

    if (file->handle != NULL)
    {
        fclose(file->handle);
        file->handle = NULL;
    }

    file->handle = fopen(file->path, "wb");
    if (file->handle != NULL)
    {
        file->mode = 1; /* write mode */
        file->kind = KGPC_FILE_KIND_BINARY;
        kgpc_ioresult_set(0);
    }
    else
    {
        file->mode = 0;
        kgpc_ioresult_set(errno);
    }
}

void kgpc_tfile_reset(KGPCTextFile **slot)
{
    if (slot == NULL)
        return;

    KGPCTextFile *file = *slot;
    if (file == NULL || file->path == NULL)
        return;

    if (file->handle != NULL)
    {
        fclose(file->handle);
        file->handle = NULL;
    }

    file->handle = fopen(file->path, "rb");
    if (file->handle != NULL)
    {
        file->mode = 2; /* read mode */
        file->kind = KGPC_FILE_KIND_BINARY;
        kgpc_ioresult_set(0);
    }
    else
    {
        file->mode = 0;
        kgpc_ioresult_set(errno);
    }
}

void kgpc_tfile_close(KGPCTextFile **slot)
{
    if (slot == NULL) {
        kgpc_ioresult_set(EINVAL);
        return;
    }

    KGPCTextFile *file = *slot;
    if (file == NULL) {
        kgpc_ioresult_set(EBADF);
        return;
    }
    if (file->handle == NULL) {
        kgpc_ioresult_set(0);
        return;
    }

    if (fclose(file->handle) == 0) {
        kgpc_ioresult_set(0);
    } else {
        kgpc_ioresult_set(errno);
    }
    file->handle = NULL;
}

int kgpc_tfile_read_int(KGPCTextFile **slot, int32_t *ptr)
{
    if (slot == NULL || ptr == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCTextFile *file = *slot;
    if (file == NULL || file->handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }
    file->element_type = KGPC_BINARY_INT32;
    file->element_size = sizeof(int32_t);
    size_t n = fread(ptr, sizeof(int32_t), 1, file->handle);
    if (n == 1)
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    if (feof(file->handle))
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(file->handle) ? EIO : 0);
    return ferror(file->handle) ? 1 : 0;
}

int kgpc_tfile_read_char(KGPCTextFile **slot, char *ptr)
{
    if (slot == NULL || ptr == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCTextFile *file = *slot;
    if (file == NULL || file->handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }
    unsigned char ch;
    file->element_type = KGPC_BINARY_CHAR;
    file->element_size = sizeof(unsigned char);
    size_t n = fread(&ch, sizeof(unsigned char), 1, file->handle);
    if (n == 1)
    {
        *ptr = (char)ch;
        kgpc_ioresult_set(0);
        return 0;
    }
    if (feof(file->handle))
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(file->handle) ? EIO : 0);
    return ferror(file->handle) ? 1 : 0;
}

int kgpc_tfile_read_real(KGPCTextFile **slot, double *ptr)
{
    if (slot == NULL || ptr == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCTextFile *file = *slot;
    if (file == NULL || file->handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }
    file->element_type = KGPC_BINARY_DOUBLE;
    file->element_size = sizeof(double);
    size_t n = fread(ptr, sizeof(double), 1, file->handle);
    if (n == 1)
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    if (feof(file->handle))
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(file->handle) ? EIO : 0);
    return ferror(file->handle) ? 1 : 0;
}

int kgpc_tfile_write_int(KGPCTextFile **slot, int32_t value)
{
    if (slot == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCTextFile *file = *slot;
    if (file == NULL || file->handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }
    file->element_type = KGPC_BINARY_INT32;
    file->element_size = sizeof(int32_t);
    size_t n = fwrite(&value, sizeof(int32_t), 1, file->handle);
    if (n == 1)
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(file->handle) ? EIO : 1);
    return 1;
}

int kgpc_tfile_write_char(KGPCTextFile **slot, char value)
{
    if (slot == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCTextFile *file = *slot;
    if (file == NULL || file->handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }
    unsigned char ch = (unsigned char)value;
    file->element_type = KGPC_BINARY_CHAR;
    file->element_size = sizeof(unsigned char);
    size_t n = fwrite(&ch, sizeof(unsigned char), 1, file->handle);
    if (n == 1)
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(file->handle) ? EIO : 1);
    return 1;
}

int kgpc_tfile_write_real(KGPCTextFile **slot, double value)
{
    if (slot == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCTextFile *file = *slot;
    if (file == NULL || file->handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }
    file->element_type = KGPC_BINARY_DOUBLE;
    file->element_size = sizeof(double);
    size_t n = fwrite(&value, sizeof(double), 1, file->handle);
    if (n == 1)
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(file->handle) ? EIO : 1);
    return 1;
}

int kgpc_tfile_blockread(KGPCTextFile **slot, void *buffer, size_t count, long long *actual_read)
{
    if (slot == NULL || buffer == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }

    KGPCTextFile *file = *slot;
    if (file == NULL || file->handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }

    size_t elem_size = file->element_size;
    if (elem_size == 0)
    {
        switch (file->element_type)
        {
            case KGPC_BINARY_CHAR: elem_size = sizeof(unsigned char); break;
            case KGPC_BINARY_DOUBLE: elem_size = sizeof(double); break;
            case KGPC_BINARY_INT32:
            case KGPC_BINARY_UNSPECIFIED:
            default:
                elem_size = sizeof(int32_t);
                break;
        }
        file->element_size = elem_size;
    }

    if (count == 0)
    {
        if (actual_read != NULL)
            *actual_read = 0;
        kgpc_ioresult_set(0);
        return 0;
    }

    size_t read_elems = fread(buffer, elem_size, count, file->handle);
    if (actual_read != NULL)
        *actual_read = (long long)read_elems;

    if (read_elems == count)
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    if (feof(file->handle))
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(file->handle) ? EIO : 1);
    return 1;
}

int kgpc_tfile_blockwrite(KGPCTextFile **slot, const void *buffer, size_t count, long long *actual_written)
{
    if (slot == NULL || buffer == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }

    KGPCTextFile *file = *slot;
    if (file == NULL || file->handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }

    size_t elem_size = file->element_size;
    if (elem_size == 0)
    {
        switch (file->element_type)
        {
            case KGPC_BINARY_CHAR: elem_size = sizeof(unsigned char); break;
            case KGPC_BINARY_DOUBLE: elem_size = sizeof(double); break;
            case KGPC_BINARY_INT32:
            case KGPC_BINARY_UNSPECIFIED:
            default:
                elem_size = sizeof(int32_t);
                break;
        }
        file->element_size = elem_size;
    }

    if (count == 0)
    {
        if (actual_written != NULL)
            *actual_written = 0;
        kgpc_ioresult_set(0);
        return 0;
    }

    size_t written = fwrite(buffer, elem_size, count, file->handle);
    if (actual_written != NULL)
        *actual_written = (long long)written;

    if (written == count)
    {
        kgpc_ioresult_set(0);
        return 0;
    }
    kgpc_ioresult_set(ferror(file->handle) ? EIO : 1);
    return 1;
}


int kgpc_tfile_seek(KGPCTextFile **slot, long long index)
{
    if (slot == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCTextFile *file = *slot;
    if (file == NULL || file->handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }

    size_t elem_size = file->element_size;
    if (elem_size == 0)
    {
        switch (file->element_type)
        {
            case KGPC_BINARY_CHAR: elem_size = sizeof(unsigned char); break;
            case KGPC_BINARY_DOUBLE: elem_size = sizeof(double); break;
            case KGPC_BINARY_INT32:
            case KGPC_BINARY_UNSPECIFIED:
            default:
                elem_size = sizeof(int32_t);
                break;
        }
        file->element_size = elem_size;
    }

    if (fseeko(file->handle, elem_size * index, SEEK_SET) != 0)
    {
        kgpc_ioresult_set(errno);
        return 1;
    }
    kgpc_ioresult_set(0);
    return 0;
}

int kgpc_tfile_filepos(KGPCTextFile **slot, long long *position)
{
    if (slot == NULL || position == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCTextFile *file = *slot;
    if (file == NULL || file->handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }

    size_t elem_size = file->element_size;
    if (elem_size == 0)
    {
        switch (file->element_type)
        {
            case KGPC_BINARY_CHAR: elem_size = sizeof(unsigned char); break;
            case KGPC_BINARY_DOUBLE: elem_size = sizeof(double); break;
            case KGPC_BINARY_INT32:
            case KGPC_BINARY_UNSPECIFIED:
            default:
                elem_size = sizeof(int32_t);
                break;
        }
        file->element_size = elem_size;
    }

    off_t offset = ftello(file->handle);
    if (offset == (off_t)-1)
    {
        kgpc_ioresult_set(errno);
        return 1;
    }
    *position = (long long)(offset / (off_t)elem_size);
    kgpc_ioresult_set(0);
    return 0;
}

int kgpc_tfile_truncate(KGPCTextFile **slot, long long length)
{
    if (slot == NULL)
    {
        kgpc_ioresult_set(EINVAL);
        return 1;
    }
    KGPCTextFile *file = *slot;
    if (file == NULL || file->handle == NULL)
    {
        kgpc_ioresult_set(EBADF);
        return 1;
    }

    size_t elem_size = file->element_size;
    if (elem_size == 0)
    {
        switch (file->element_type)
        {
            case KGPC_BINARY_CHAR: elem_size = sizeof(unsigned char); break;
            case KGPC_BINARY_DOUBLE: elem_size = sizeof(double); break;
            case KGPC_BINARY_INT32:
            case KGPC_BINARY_UNSPECIFIED:
            default:
                elem_size = sizeof(int32_t);
                break;
        }
        file->element_size = elem_size;
    }

    off_t target = (off_t)(elem_size * length);
    if (fflush(file->handle) != 0)
    {
        kgpc_ioresult_set(errno);
        return 1;
    }

#ifdef _WIN32
    int fd = _fileno(file->handle);
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
#else
    int fd = fileno(file->handle);
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
#endif

    if (fseeko(file->handle, target, SEEK_SET) != 0)
    {
        kgpc_ioresult_set(errno);
        return 1;
    }
    kgpc_ioresult_set(0);
    return 0;
}

int kgpc_tfile_truncate_current(KGPCTextFile **slot)
{
    long long position = 0;
    if (kgpc_tfile_filepos(slot, &position) != 0)
        return 1;
    return kgpc_tfile_truncate(slot, position);
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

int kgpc_read_integer(KGPCTextFile *file, int32_t *ptr) {
    FILE *stream = kgpc_text_input_stream(file);
    int res = fscanf(stream, "%d", ptr);
    return kgpc_scanf_result_to_ioresult(res);
}

int kgpc_read_longint(KGPCTextFile *file, int32_t *ptr) {
    FILE *stream = kgpc_text_input_stream(file);
    int res = fscanf(stream, "%" PRId32, ptr);
    return kgpc_scanf_result_to_ioresult(res);
}

int kgpc_read_char(KGPCTextFile *file, char *ptr) {
    FILE *stream = kgpc_text_input_stream(file);
    int res = fscanf(stream, " %c", ptr);
    return kgpc_scanf_result_to_ioresult(res);
}

int kgpc_read_real(KGPCTextFile *file, double *ptr) {
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

static int kgpc_normalize_crt_color(int color)
{
    int normalized = color % 16;
    if (normalized < 0)
        normalized += 16;
    return normalized;
}

static const char *const kgpc_crt_ansi_codes[16] = {
    "\033[0;30m", "\033[0;34m", "\033[0;32m", "\033[0;36m",
    "\033[0;31m", "\033[0;35m", "\033[0;33m", "\033[0;37m",
    "\033[0;90m", "\033[0;94m", "\033[0;92m", "\033[0;96m",
    "\033[0;91m", "\033[0;95m", "\033[0;93m", "\033[0;97m"
};

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
    fputs("\033[2J\033[H", stdout);
    fflush(stdout);
}

void kgpc_textcolor(int color)
{
    int normalized = kgpc_normalize_crt_color(color);

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
    fputs(kgpc_crt_ansi_codes[normalized], stdout);
    fflush(stdout);
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

void kgpc_write_integer(KGPCTextFile *file, int width, int64_t value)
{
    FILE *dest = kgpc_text_output_stream(file);
    if (dest == NULL)
        return;

    if (width > 1024 || width < -1024)
        width = 0;
    if (width == -1)
        width = 0;

    if (width > 0)
        fprintf(dest, "%*lld", width, (long long)value);
    else if (width < 0)
        fprintf(dest, "%-*lld", -width, (long long)value);
    else
        fprintf(dest, "%lld", (long long)value);
}

void kgpc_write_unsigned(KGPCTextFile *file, int width, uint64_t value)
{
    FILE *dest = kgpc_text_output_stream(file);
    if (dest == NULL)
        return;

    if (width > 1024 || width < -1024)
        width = 0;
    if (width == -1)
        width = 0;

    if (width > 0)
        fprintf(dest, "%*llu", width, (unsigned long long)value);
    else if (width < 0)
        fprintf(dest, "%-*llu", -width, (unsigned long long)value);
    else
        fprintf(dest, "%llu", (unsigned long long)value);
}

static size_t kgpc_string_known_length(const char *value);

void kgpc_write_string(KGPCTextFile *file, int width, const char *value)
{
    FILE *dest = kgpc_text_output_stream(file);
    if (dest == NULL)
        return;

    if (value == NULL)
        value = "";
    if (width > 1024 || width < -1024)
        width = 0;

    size_t len = kgpc_string_known_length(value);
    if (len == 0 && width <= 0)
        return;
    if (width > 0)
    {
        size_t pad = (width > (int)len) ? (size_t)width - len : 0;
        for (size_t i = 0; i < pad; ++i)
            fputc(' ', dest);
        if (len > 0)
            fwrite(value, 1, len, dest);
    }
    else if (width < 0)
    {
        if (width == -1)
        {
            if (len > 0)
                fwrite(value, 1, len, dest);
        }
        else
        {
            size_t target = (size_t)(-width);
            if (len > 0)
                fwrite(value, 1, len, dest);
            size_t pad = (target > len) ? target - len : 0;
            for (size_t i = 0; i < pad; ++i)
                fputc(' ', dest);
        }
    }
    else if (len > 0)
    {
        fwrite(value, 1, len, dest);
    }
}

/* Write a char array with specified maximum length (for Pascal char arrays) */
void kgpc_write_char_array(KGPCTextFile *file, int width, const char *value, size_t max_len)
{
    FILE *dest = kgpc_text_output_stream(file);
    if (dest == NULL)
        return;

    if (value == NULL)
        return;
    
    /* Find the actual length: either max_len or until first null, whichever comes first */
    size_t actual_len = 0;
    while (actual_len < max_len && value[actual_len] != '\0')
        actual_len++;
    
    /* Use precision specifier to limit output */
    if (width > 1024 || width < -1024)
        width = 0;
    if (width == -1)
        width = 0;
    if (width > 0)
        fprintf(dest, "%*.*s", width, (int)actual_len, value);
    else if (width < 0)
        fprintf(dest, "%-*.*s", -width, (int)actual_len, value);
    else
        fprintf(dest, "%.*s", (int)actual_len, value);
}

/* Write ShortString (Pascal string with length byte at index 0) */
void kgpc_write_shortstring(KGPCTextFile *file, int width, const char *value)
{
    FILE *dest = kgpc_text_output_stream(file);
    if (dest == NULL || value == NULL)
        return;
    
    /* Read length from index 0 */
    unsigned char len = (unsigned char)value[0];
    
    /* String data starts at index 1 */
    const char *str_data = value + 1;
    
    /* Use precision specifier to limit output to the stored length */
    if (width > 1024 || width < -1024)
        width = 0;
    if (width == -1)
        width = 0;
    if (width > 0)
        fprintf(dest, "%*.*s", width, (int)len, str_data);
    else if (width < 0)
        fprintf(dest, "%-*.*s", -width, (int)len, str_data);
    else
        fprintf(dest, "%.*s", (int)len, str_data);
}

void kgpc_write_newline(KGPCTextFile *file)
{
    FILE *dest = kgpc_text_output_stream(file);
    if (dest == NULL)
        return;

    fputc('\n', dest);
    fflush(dest);
}

void kgpc_write_char(KGPCTextFile *file, int width, int value)
{
    unsigned char ch = (unsigned char)value;
    char buffer[2];
    buffer[0] = (char)ch;
    buffer[1] = '\0';
    kgpc_write_string(file, width, buffer);
}

void kgpc_write_boolean(KGPCTextFile *file, int width, int value)
{
    FILE *dest = kgpc_text_output_stream(file);
    if (dest == NULL)
        return;

    const char *text = value ? "TRUE" : "FALSE";
    if (width > 1024 || width < -1024)
        width = 0;
    if (width > 0)
        fprintf(dest, "%*s", width, text);
    else if (width < 0)
        fprintf(dest, "%-*s", -width, text);
    else
        fprintf(dest, "%s", text);
}

void kgpc_write_real(KGPCTextFile *file, int width, int precision, int64_t value_bits)
{
    FILE *dest = kgpc_text_output_stream(file);
    if (dest == NULL)
        return;

    if (width < -1024 || width > 1024)
        width = 0;
    if (precision < -1)
        precision = -1;
    if (precision > 18)
        precision = 18;

    if (width == -1)
        width = 0;

    double value = kgpc_bits_to_double(value_bits);

    if (precision < 0)
    {
        if (width > 0)
            fprintf(dest, "%*g", width, value);
        else if (width < 0)
            fprintf(dest, "%-*g", -width, value);
        else
            fprintf(dest, "%g", value);
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
}

void kgpc_raise(int64_t value)
{
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

/* Generic default constructor for classes without explicit constructors */
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
    
    return instance;
}

typedef struct KgpcStringNode
{
    char *ptr;
    size_t length;
    struct KgpcStringNode *next;
} KgpcStringNode;

static KgpcStringNode *kgpc_string_allocations = NULL;

static void kgpc_string_register_allocation(char *ptr, size_t length)
{
    if (ptr == NULL)
        return;

    KgpcStringNode *node = (KgpcStringNode *)malloc(sizeof(KgpcStringNode));
    if (node == NULL)
        return;

    node->ptr = ptr;
    node->length = length;
    node->next = kgpc_string_allocations;
    kgpc_string_allocations = node;
}

static int kgpc_string_release_allocation(char *ptr)
{
    if (ptr == NULL)
        return 0;

    KgpcStringNode **link = &kgpc_string_allocations;
    while (*link != NULL)
    {
        if ((*link)->ptr == ptr)
        {
            KgpcStringNode *victim = *link;
            *link = victim->next;
            free(victim);
            return 1;
        }
        link = &(*link)->next;
    }

    return 0;
}

static KgpcStringNode *kgpc_string_find_allocation(const char *ptr)
{
    KgpcStringNode *node = kgpc_string_allocations;
    while (node != NULL)
    {
        if (node->ptr == ptr)
            return node;
        node = node->next;
    }
    return NULL;
}

static size_t kgpc_string_known_length(const char *value)
{
    if (value == NULL)
        return 0;

    KgpcStringNode *node = kgpc_string_find_allocation(value);
    if (node != NULL)
        return node->length;
    return strlen(value);
}

char *kgpc_alloc_empty_string(void)
{
    char *empty = (char *)malloc(1);
    if (empty != NULL)
    {
        empty[0] = '\0';
        kgpc_string_register_allocation(empty, 0);
    }
    return empty;
}

char *kgpc_string_duplicate(const char *value)
{
    if (value == NULL)
        return kgpc_alloc_empty_string();

    size_t len = kgpc_string_known_length(value);
    char *copy = (char *)malloc(len + 1);
    if (copy == NULL)
        return kgpc_alloc_empty_string();

    if (len > 0)
        memcpy(copy, value, len);
    copy[len] = '\0';
    kgpc_string_register_allocation(copy, len);
    return copy;
}

static char *kgpc_string_duplicate_length(const char *value, size_t length)
{
    char *copy = (char *)malloc(length + 1);
    if (copy == NULL)
        return kgpc_alloc_empty_string();
    if (length > 0 && value != NULL)
        memcpy(copy, value, length);
    copy[length] = '\0';
    kgpc_string_register_allocation(copy, length);
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
    if (kgpc_string_release_allocation(existing))
    {
        free(existing);
        *target = NULL;
    }

    char *copy = kgpc_string_duplicate(value);
    *target = copy;
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

    char *resized = (char *)malloc(requested + 1);
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
    resized[requested] = '\0';

    kgpc_string_register_allocation(resized, requested);
    if (current != NULL && kgpc_string_release_allocation(current))
        free(current);
    *target = resized;
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
    char *result = (char *)malloc(new_length + 1);
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
    result[new_length] = '\0';

    kgpc_string_register_allocation(result, new_length);
    if (source != NULL && kgpc_string_release_allocation(source))
        free(source);
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

    char *result = (char *)malloc(new_len + 1);
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
    result[new_len] = '\0';

    kgpc_string_register_allocation(result, new_len);
    if (dest != NULL && kgpc_string_release_allocation(dest))
        free(dest);
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


static char *kgpc_textfile_duplicate_path(const char *path)
{
    if (path == NULL)
        return NULL;

    size_t len = strlen(path);
    char *copy = (char *)malloc(len + 1);
    if (copy == NULL)
        return NULL;

    memcpy(copy, path, len + 1);
    return copy;
}

static void kgpc_textfile_free_path(KGPCTextFile *file)
{
    if (file == NULL || file->path == NULL)
        return;

    free(file->path);
    file->path = NULL;
}

static void kgpc_textfile_close_handle(KGPCTextFile *file)
{
    if (file == NULL)
        return;

    if (file->handle != NULL)
    {
        fclose(file->handle);
        file->handle = NULL;
    }
    file->mode = 0;
}

static KGPCTextFile *kgpc_textfile_prepare(KGPCTextFile **slot)
{
    if (slot == NULL)
        return NULL;

    KGPCTextFile *file = *slot;
    if (file == NULL)
    {
        file = (KGPCTextFile *)calloc(1, sizeof(KGPCTextFile));
        if (file == NULL)
            return NULL;
        *slot = file;
    }

    return file;
}

static char *kgpc_text_read_line_from_stream(FILE *stream)
{
    if (stream == NULL)
        return NULL;

    size_t capacity = 64;
    size_t length = 0;
    char *buffer = (char *)malloc(capacity);
    if (buffer == NULL)
        return NULL;

    int ch = 0;
    int read_any = 0;
    while (1)
    {
        ch = fgetc(stream);
        if (ch == EOF)
            break;

        read_any = 1;
        if (ch == '\r')
        {
            int next = fgetc(stream);
            if (next != '\n' && next != EOF)
                ungetc(next, stream);
            break;
        }
        if (ch == '\n')
            break;

        if (length + 1 >= capacity)
        {
            size_t new_capacity = capacity < 128 ? capacity * 2 : capacity + 64;
            if (new_capacity <= capacity)
                new_capacity = capacity + 64;
            char *new_buffer = (char *)realloc(buffer, new_capacity);
            if (new_buffer == NULL)
            {
                free(buffer);
                return NULL;
            }
            buffer = new_buffer;
            capacity = new_capacity;
        }

        buffer[length++] = (char)ch;
    }

    if (!read_any && ch == EOF)
    {
        free(buffer);
        return NULL;
    }

    buffer[length] = '\0';
    return buffer;
}

void kgpc_text_assign(KGPCTextFile **slot, const char *path)
{
    if (slot == NULL)
        return;

    KGPCTextFile *file = kgpc_textfile_prepare(slot);
    if (file == NULL)
        return;

    kgpc_textfile_close_handle(file);
    kgpc_textfile_free_path(file);

    if (path != NULL)
        file->path = kgpc_textfile_duplicate_path(path);
}

void kgpc_text_rewrite(KGPCTextFile **slot)
{
    KGPCTextFile *file = kgpc_textfile_prepare(slot);
    if (file == NULL || file->path == NULL)
        return;

    kgpc_textfile_close_handle(file);

    file->handle = fopen(file->path, "w");
    if (file->handle != NULL)
        file->mode = 1;
    else
        file->mode = 0;
}

void kgpc_text_append(KGPCTextFile **slot)
{
    KGPCTextFile *file = kgpc_textfile_prepare(slot);
    if (file == NULL || file->path == NULL)
        return;

    kgpc_textfile_close_handle(file);

    file->handle = fopen(file->path, "a");
    if (file->handle != NULL)
    {
        file->mode = 1;
        fseek(file->handle, 0, SEEK_END);
    }
    else
    {
        file->mode = 0;
    }
}

void kgpc_text_app(KGPCTextFile **slot)
{
    kgpc_text_append(slot);
}

void kgpc_text_reset(KGPCTextFile **slot)
{
    KGPCTextFile *file = kgpc_textfile_prepare(slot);
    if (file == NULL || file->path == NULL)
        return;

    kgpc_textfile_close_handle(file);

    file->handle = fopen(file->path, "r");
    if (file->handle != NULL)
        file->mode = 2;
    else
        file->mode = 0;
}

void kgpc_text_close(KGPCTextFile **slot)
{
    if (slot == NULL)
        return;

    KGPCTextFile *file = *slot;
    if (file == NULL)
        return;

    kgpc_textfile_close_handle(file);
}

int kgpc_text_eof(KGPCTextFile *file)
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

int kgpc_text_eoln(KGPCTextFile *file)
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

void kgpc_text_readln_into(KGPCTextFile *file, char **target)
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

    kgpc_string_assign(target, line);
    free(line);
}

void kgpc_text_readln_into_char(KGPCTextFile *file, char *target)
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

void kgpc_text_readln_discard(KGPCTextFile *file)
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

char *kgpc_string_concat(const char *lhs, const char *rhs)
{
    if (lhs == NULL)
        lhs = "";
    if (rhs == NULL)
        rhs = "";

    size_t lhs_len = kgpc_string_known_length(lhs);
    size_t rhs_len = kgpc_string_known_length(rhs);
    size_t total = lhs_len + rhs_len;

    char *result = (char *)malloc(total + 1);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    if (lhs_len > 0)
        memcpy(result, lhs, lhs_len);
    if (rhs_len > 0)
        memcpy(result + lhs_len, rhs, rhs_len);
    result[total] = '\0';
    kgpc_string_register_allocation(result, total);
    return result;
}

int64_t kgpc_string_length(const char *value)
{
    return (int64_t)kgpc_string_known_length(value);
}

char *kgpc_string_copy(const char *value, int64_t index, int64_t count)
{
    if (value == NULL)
        value = "";

    size_t len = strlen(value);
    if (index < 1 || index > (int64_t)len)
        return kgpc_alloc_empty_string();

    if (count < 0)
        count = 0;

    size_t start = (size_t)(index - 1);
    size_t available = len - start;
    size_t to_copy = (size_t)count;
    if (to_copy > available)
        to_copy = available;

    char *result = (char *)malloc(to_copy + 1);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    if (to_copy > 0)
        memcpy(result, value + start, to_copy);
    result[to_copy] = '\0';
    kgpc_string_register_allocation(result, to_copy);
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

char *kgpc_strpas(const char *p)
{
    if (p == NULL)
        return kgpc_alloc_empty_string();
    return kgpc_string_duplicate(p);
}

int64_t kgpc_string_pos(const char *substr, const char *value)
{
    if (value == NULL)
        value = "";
    if (substr == NULL)
        substr = "";

    size_t hay_len = kgpc_string_known_length(value);
    size_t needle_len = kgpc_string_known_length(substr);

    if (needle_len == 0)
        return 1;
    if (needle_len > hay_len)
        return 0;

    for (size_t i = 0; i + needle_len <= hay_len; ++i)
    {
        if (memcmp(value + i, substr, needle_len) == 0)
            return (int64_t)(i + 1);
    }

    return 0;
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
    char *result = (char *)malloc(base_len + ext_len + 1);
    if (result == NULL)
        return kgpc_alloc_empty_string();
    if (base_len > 0)
        memcpy(result, filename, base_len);
    if (ext_len > 0 && extension != NULL)
        memcpy(result + base_len, extension, ext_len);
    result[base_len + ext_len] = '\0';
    kgpc_string_register_allocation(result, base_len + ext_len);
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
    {
        if (end == 1)
            break;
        if (end == 3 && path[1] == ':' && kgpc_is_path_delim_char(path[2]))
            break;
        --end;
    }
    if (end == len)
        return kgpc_string_duplicate(path);
    if (end == 0)
        end = 1;
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
    if (code == 0 && out_value != NULL)
        *out_value = (int32_t)parsed;
    return code;
}

long long kgpc_val_longint(const char *text, int64_t *out_value)
{
    long long parsed = 0;
    long long code = kgpc_val_parse_integer(text, INT64_MIN, INT64_MAX, &parsed);
    if (code == 0 && out_value != NULL)
        *out_value = parsed;
    return code;
}

long long kgpc_val_real(const char *text, double *out_value)
{
    double parsed = 0.0;
    long long code = kgpc_val_parse_real(text, &parsed);
    if (code == 0 && out_value != NULL)
        *out_value = parsed;
    return code;
}

/* Chr function - returns a character value as an integer */
int64_t kgpc_chr(int64_t value)
{
    /* Clamp value to valid character range [0, 255] */
    if (value < 0)
        return 0;
    if (value > 255)
        return 255;
    
    return value;
}

/* Convert a character value to a single-character string */
char *kgpc_char_to_string(int64_t value)
{
    /* Clamp value to valid character range [0, 255] */
    if (value < 0)
        value = 0;
    else if (value > 255)
        value = 255;

    char *result = (char *)malloc(2);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    result[0] = (char)value;
    result[1] = '\0';
    kgpc_string_register_allocation(result, 1);
    return result;
}

int64_t kgpc_upcase_char(int64_t value)
{
    unsigned char ch = (unsigned char)(value & 0xFF);
    if (ch >= 'a' && ch <= 'z')
        ch = (unsigned char)(ch - ('a' - 'A'));
    return (int64_t)ch;
}

int64_t kgpc_is_odd(int64_t value)
{
    return (value & 1) ? 1 : 0;
}

int32_t kgpc_sqr_int32(int32_t value)
{
    return value * value;
}

int64_t kgpc_sqr_int64(int64_t value)
{
    return value * value;
}

double kgpc_sqr_real(double value)
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

    char *result = (char *)malloc((size_t)written + 1);
    if (result == NULL)
        return kgpc_alloc_empty_string();

    memcpy(result, buffer, (size_t)written + 1);
    kgpc_string_register_allocation(result, (size_t)written);
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
    if (existing != NULL && kgpc_string_release_allocation(existing))
        free(existing);
    *target = result;
}

int64_t kgpc_now(void)
{
#ifdef _WIN32
    FILETIME ft;
    ULARGE_INTEGER uli;
    GetSystemTimeAsFileTime(&ft);
    uli.LowPart = ft.dwLowDateTime;
    uli.HighPart = ft.dwHighDateTime;
    /* FILETIME is in 100-nanosecond intervals since January 1, 1601. */
    uint64_t ticks = uli.QuadPart - 116444736000000000ULL;
    return (int64_t)(ticks / 10000ULL);
#else
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
        return 0;
    return (int64_t)ts.tv_sec * 1000LL + (int64_t)(ts.tv_nsec / 1000000LL);
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

char *kgpc_format_datetime(const char *format, int64_t datetime_ms)
{
    if (format == NULL)
        format = "";

    time_t seconds = (time_t)(datetime_ms / 1000);
    int millis = (int)(datetime_ms % 1000);
    if (millis < 0)
    {
        millis += 1000;
        seconds -= 1;
    }

    struct tm tm_value;
#ifdef _WIN32
    errno_t err = localtime_s(&tm_value, &seconds);
    if (err != 0)
        return kgpc_alloc_empty_string();
#else
    if (localtime_r(&seconds, &tm_value) == NULL)
        return kgpc_alloc_empty_string();
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
                else if (arg->kind == KGPC_TVAR_KIND_POINTER || arg->kind == KGPC_TVAR_KIND_STRING)
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
                    real_value = arg->data.v_real;
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
                if (arg->kind != KGPC_TVAR_KIND_STRING && arg->kind != KGPC_TVAR_KIND_POINTER)
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
        precision = 6;
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
    char *endptr;
    errno = 0;
    double val = strtod(text, &endptr);
    if (kgpc_env_flag("KGPC_DEBUG_STRTOFLOAT"))
        fprintf(stderr, "[kgpc] string_to_real('%s') -> val=%g end=%p (err=%d)\n", text ? text : "(null)", val, (void*)endptr, errno);
    if (endptr == text || *endptr != '\0' || errno == ERANGE)
    {
        if (out_value != NULL) *out_value = 0.0;
        return 0;
    }
    if (out_value != NULL)
        *out_value = val;
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
        return kgpc_alloc_empty_string();
    char *buffer = (char *)malloc((size_t)needed + 1);
    if (buffer == NULL)
        return kgpc_alloc_empty_string();
    if (GetCurrentDirectoryA(needed + 1, buffer) == 0)
    {
        free(buffer);
        return kgpc_alloc_empty_string();
    }
    char *result = kgpc_string_duplicate(buffer);
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
        return kgpc_alloc_empty_string();
    if (getcwd(buffer, max_len) == NULL)
    {
        free(buffer);
        return kgpc_alloc_empty_string();
    }
    char *result = kgpc_string_duplicate(buffer);
    free(buffer);
    return result;
#endif
}

int kgpc_set_current_dir(const char *path)
{
    if (path == NULL)
        return EINVAL;
#ifdef _WIN32
    return SetCurrentDirectoryA(path) ? 0 : (int)GetLastError();
#else
    return (chdir(path) == 0) ? 0 : errno;
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

/* Provide shims for Pascal mangled runtime helpers so the runtime always
 * satisfies references emitted by generated assembly. The COFF alternatename
 * directives make these symbols resolve to our implementations when no user
 * definition is present, while avoiding duplicate-definition errors when user
 * code defines them. */
/* Default implementations that can be adopted via COFF alternatename so
 * user-emitted stubs override when present, while ELF uses weak aliases. */
uintptr_t kgpc_default_LoadLibrary_s(const char *path)
{
    return kgpc_load_library(path);
}

uintptr_t kgpc_default_GetProcedureAddress_li_s(uintptr_t handle, const char *symbol)
{
    return kgpc_get_proc_address(handle, symbol);
}

int kgpc_default_FreeLibrary_li(uintptr_t handle)
{
    return kgpc_free_library(handle);
}

#ifdef _WIN32
__asm__(".section .drectve,\"yn\"\n"
        ".ascii \" /alternatename:LoadLibrary_s=kgpc_default_LoadLibrary_s\\0\"\n"
        ".ascii \" /alternatename:GetProcedureAddress_li_s=kgpc_default_GetProcedureAddress_li_s\\0\"\n"
        ".ascii \" /alternatename:FreeLibrary_li=kgpc_default_FreeLibrary_li\\0\"\n"
        ".text");
#else
uintptr_t LoadLibrary_s(const char *path) __attribute__((weak, alias("kgpc_default_LoadLibrary_s")));
uintptr_t GetProcedureAddress_li_s(uintptr_t handle, const char *symbol) __attribute__((weak, alias("kgpc_default_GetProcedureAddress_li_s")));
int FreeLibrary_li(uintptr_t handle) __attribute__((weak, alias("kgpc_default_FreeLibrary_li")));
#endif

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
    return (_S_IFREG & st.st_mode) && (_S_IFMT & st.st_mode);
#else
    struct stat st;
    if (stat(path, &st) != 0)
        return 0;
    return S_ISREG(st.st_mode);
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

/* Wrapper for memcpy to be called from assembly code */
void *kgpc_memcpy_wrapper(void *dest, const void *src, size_t n)
{
    return memcpy(dest, src, n);
}

int64_t kgpc_assigned(const void *ptr)
{
    return (ptr != NULL) ? 1 : 0;
}

int32_t kgpc_abs_int(int32_t value)
{
    return (value < 0) ? -value : value;
}

int64_t kgpc_abs_longint(int64_t value)
{
    return (value < 0) ? -value : value;
}

double kgpc_abs_real(double value)
{
    return fabs(value);
}

double kgpc_sqrt(double value)
{
    return sqrt(value);
}

double kgpc_sin(double value)
{
    return sin(value);
}

double kgpc_csc(double value)
{
    return 1.0 / sin(value);
}

double kgpc_cos(double value)
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

double kgpc_arctan(double value)
{
    return atan(value);
}

double kgpc_arccot(double value)
{
    return (KGPC_PI / 2.0) - atan(value);
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

double kgpc_ln(double value)
{
    return log(value);
}

double kgpc_logn(double base, double value)
{
    return log(value) / log(base);
}

double kgpc_exp(double value)
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

long long kgpc_round(double value)
{
    double rounded;
    if (value >= 0.0)
        rounded = floor(value + 0.5);
    else
        rounded = ceil(value - 0.5);
    return (long long)rounded;
}

long long kgpc_trunc(double value)
{
    if (value >= 0.0)
        return (long long)floor(value);
    return (long long)ceil(value);
}

long long kgpc_int(double value)
{
    return kgpc_trunc(value);
}

double kgpc_frac(double value)
{
    return value - (double)kgpc_trunc(value);
}

long long kgpc_ceil(double value)
{
    return (long long)ceil(value);
}

long long kgpc_floor(double value)
{
    return (long long)floor(value);
}
static uint64_t kgpc_rand_next(void)
{
    kgpc_rand_state = kgpc_rand_state * 2862933555777941757ULL + 3037000493ULL;
    return kgpc_rand_state;
}

void kgpc_randomize(void)
{
    uint64_t seed = ((uint64_t)time(NULL) << 32) ^ (uint64_t)clock();
    if (seed == 0)
        seed = 0x9e3779b97f4a7c15ULL;
    kgpc_rand_state = seed;
}

double kgpc_random_real(void)
{
    uint64_t value = kgpc_rand_next();
    return (double)(value >> 11) * (1.0 / 9007199254740992.0);
}

double kgpc_random_real_upper(double upper)
{
    if (upper <= 0.0)
        return 0.0;
    return kgpc_random_real() * upper;
}

int64_t kgpc_random_int(int64_t upper)
{
    if (upper <= 0)
        return 0;
    uint64_t value = kgpc_rand_next();
    uint64_t range = (uint64_t)upper;
    return (int64_t)(value % range);
}

int64_t kgpc_random_range(int64_t low, int64_t high)
{
    if (high <= low)
        return low;
    uint64_t diff = (uint64_t)(high - low);
    uint64_t value = kgpc_rand_next();
    uint64_t offset = value % diff;
    return low + (int64_t)offset;
}

int64_t kgpc_get_randseed(void)
{
    return (int64_t)kgpc_rand_state;
}

void kgpc_set_randseed(int64_t seed)
{
    if ((uint64_t)seed == 0)
        kgpc_rand_state = 0x9e3779b97f4a7c15ULL;
    else
        kgpc_rand_state = (uint64_t)seed;
}

void kgpc_sincos_bits(int64_t angle_bits, double *sin_out, double *cos_out)
{
    double angle = kgpc_bits_to_double(angle_bits);
    if (sin_out != NULL)
        *sin_out = sin(angle);
    if (cos_out != NULL)
        *cos_out = cos(angle);
}
