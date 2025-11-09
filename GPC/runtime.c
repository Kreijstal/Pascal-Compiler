#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stddef.h>

#ifdef _WIN32
#include <windows.h>
#include <time.h>
#include <errno.h>
#else
#include <time.h>
#include <errno.h>
#include <unistd.h>
#endif

int64_t gpc_current_exception = 0;

typedef struct GPCTextFile
{
    FILE *handle;
    char *path;
    int mode;
} GPCTextFile;

static FILE *gpc_text_output_stream(GPCTextFile *file)
{
    if (file != NULL && file->handle != NULL)
        return file->handle;
    return stdout;
}

static FILE *gpc_text_input_stream(GPCTextFile *file)
{
    if (file != NULL)
        return file->handle;
    return stdin;
}

static int gpc_vprintf_impl(const char *format, va_list args) {
    return vprintf(format, args);
}

static inline double gpc_bits_to_double(int64_t bits)
{
    double value;
    memcpy(&value, &bits, sizeof(value));
    return value;
}

static inline int64_t gpc_double_to_bits(double value)
{
    int64_t bits;
    memcpy(&bits, &value, sizeof(bits));
    return bits;
}

int gpc_printf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int result = gpc_vprintf_impl(format, args);
    va_end(args);
    return result;
}

static int gpc_vscanf_impl(const char *format, va_list args) {
#ifdef _WIN32
    return vscanf(format, args);
#else
    return vscanf(format, args);
#endif
}

int gpc_scanf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int result = gpc_vscanf_impl(format, args);
    va_end(args);
    return result;
}

#ifdef _WIN32
int __isoc99_scanf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int result = gpc_vscanf_impl(format, args);
    va_end(args);
    return result;
}
#endif

void print_integer(int n) {
    printf("%d\n", n);
}

int64_t gpc_real_add(int64_t a_bits, int64_t b_bits)
{
    double a = gpc_bits_to_double(a_bits);
    double b = gpc_bits_to_double(b_bits);
    double result = a + b;
    return gpc_double_to_bits(result);
}

int64_t gpc_real_sub(int64_t a_bits, int64_t b_bits)
{
    double a = gpc_bits_to_double(a_bits);
    double b = gpc_bits_to_double(b_bits);
    double result = a - b;
    return gpc_double_to_bits(result);
}

int64_t gpc_real_mul(int64_t a_bits, int64_t b_bits)
{
    double a = gpc_bits_to_double(a_bits);
    double b = gpc_bits_to_double(b_bits);
    double result = a * b;
    return gpc_double_to_bits(result);
}

int64_t gpc_real_div(int64_t a_bits, int64_t b_bits)
{
    double a = gpc_bits_to_double(a_bits);
    double b = gpc_bits_to_double(b_bits);
    double result = a / b;
    return gpc_double_to_bits(result);
}

int64_t gpc_real_neg(int64_t value_bits)
{
    double value = gpc_bits_to_double(value_bits);
    return gpc_double_to_bits(-value);
}

int gpc_real_compare(int64_t a_bits, int64_t b_bits)
{
    double a = gpc_bits_to_double(a_bits);
    double b = gpc_bits_to_double(b_bits);
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

uint64_t gpc_get_tick_count64(void) {
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

void gpc_sleep_ms(int milliseconds) {
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

static int gpc_normalize_crt_color(int color)
{
    int normalized = color % 16;
    if (normalized < 0)
        normalized += 16;
    return normalized;
}

static const char *const gpc_crt_ansi_codes[16] = {
    "\033[0;30m", "\033[0;34m", "\033[0;32m", "\033[0;36m",
    "\033[0;31m", "\033[0;35m", "\033[0;33m", "\033[0;37m",
    "\033[0;90m", "\033[0;94m", "\033[0;92m", "\033[0;96m",
    "\033[0;91m", "\033[0;95m", "\033[0;93m", "\033[0;97m"
};

void gpc_clrscr(void)
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

void gpc_textcolor(int color)
{
    int normalized = gpc_normalize_crt_color(color);

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
    fputs(gpc_crt_ansi_codes[normalized], stdout);
    fflush(stdout);
}

typedef struct {
    void *data;
    int64_t length;
} gpc_dynarray_descriptor_t;

void gpc_dynarray_setlength(void *descriptor_ptr, int64_t new_length, int64_t element_size)
{
    if (descriptor_ptr == NULL || element_size <= 0)
        return;

    if (new_length < 0)
        new_length = 0;

    gpc_dynarray_descriptor_t *descriptor = (gpc_dynarray_descriptor_t *)descriptor_ptr;
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

void gpc_write_integer(GPCTextFile *file, int width, int64_t value)
{
    FILE *dest = gpc_text_output_stream(file);
    if (dest == NULL)
        return;

    if (width > 1024 || width < -1024)
        width = 0;

    if (width > 0)
        fprintf(dest, "%*lld", width, (long long)value);
    else if (width < 0)
        fprintf(dest, "%-*lld", -width, (long long)value);
    else
        fprintf(dest, "%lld", (long long)value);
}

void gpc_write_string(GPCTextFile *file, int width, const char *value)
{
    FILE *dest = gpc_text_output_stream(file);
    if (dest == NULL)
        return;

    if (value == NULL)
        value = "";
    if (width > 1024 || width < -1024)
        width = 0;
    if (width > 0)
        fprintf(dest, "%*s", width, value);
    else if (width < 0)
        fprintf(dest, "%-*s", -width, value);
    else
        fprintf(dest, "%s", value);
}

/* Write a char array with specified maximum length (for Pascal char arrays) */
void gpc_write_char_array(GPCTextFile *file, int width, const char *value, size_t max_len)
{
    FILE *dest = gpc_text_output_stream(file);
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
    if (width > 0)
        fprintf(dest, "%*.*s", width, (int)actual_len, value);
    else if (width < 0)
        fprintf(dest, "%-*.*s", -width, (int)actual_len, value);
    else
        fprintf(dest, "%.*s", (int)actual_len, value);
}

void gpc_write_newline(GPCTextFile *file)
{
    FILE *dest = gpc_text_output_stream(file);
    if (dest == NULL)
        return;

    fputc('\n', dest);
    fflush(dest);
}

void gpc_write_char(GPCTextFile *file, int width, int value)
{
    unsigned char ch = (unsigned char)value;
    char buffer[2];
    buffer[0] = (char)ch;
    buffer[1] = '\0';
    gpc_write_string(file, width, buffer);
}

void gpc_write_boolean(GPCTextFile *file, int width, int value)
{
    FILE *dest = gpc_text_output_stream(file);
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

void gpc_write_real(GPCTextFile *file, int width, int precision, int64_t value_bits)
{
    FILE *dest = gpc_text_output_stream(file);
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

    double value = gpc_bits_to_double(value_bits);

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

void gpc_raise(int64_t value)
{
    if (value == 0)
        fprintf(stderr, "Unhandled exception raised.\n");
    else
        fprintf(stderr, "Unhandled exception raised with code %lld.\n", (long long)value);
    fflush(stderr);
    exit(EXIT_FAILURE);
}

void gpc_new(void **target, size_t size)
{
    if (target == NULL)
        return;

    void *memory = calloc(1, size);
    if (memory == NULL)
    {
        fprintf(stderr, "GPC runtime: failed to allocate %zu bytes.\n", size);
        exit(EXIT_FAILURE);
    }

    *target = memory;
}

void gpc_dispose(void **target)
{
    if (target == NULL)
        return;

    if (*target != NULL)
    {
        free(*target);
        *target = NULL;
    }
}

typedef struct GpcStringNode
{
    char *ptr;
    struct GpcStringNode *next;
} GpcStringNode;

static GpcStringNode *gpc_string_allocations = NULL;

static void gpc_string_register_allocation(char *ptr)
{
    if (ptr == NULL)
        return;

    GpcStringNode *node = (GpcStringNode *)malloc(sizeof(GpcStringNode));
    if (node == NULL)
        return;

    node->ptr = ptr;
    node->next = gpc_string_allocations;
    gpc_string_allocations = node;
}

static int gpc_string_release_allocation(char *ptr)
{
    if (ptr == NULL)
        return 0;

    GpcStringNode **link = &gpc_string_allocations;
    while (*link != NULL)
    {
        if ((*link)->ptr == ptr)
        {
            GpcStringNode *victim = *link;
            *link = victim->next;
            free(victim);
            return 1;
        }
        link = &(*link)->next;
    }

    return 0;
}

static char *gpc_alloc_empty_string(void)
{
    char *empty = (char *)malloc(1);
    if (empty != NULL)
    {
        empty[0] = '\0';
        gpc_string_register_allocation(empty);
    }
    return empty;
}

static char *gpc_string_duplicate(const char *value)
{
    if (value == NULL)
        return gpc_alloc_empty_string();

    size_t len = strlen(value);
    char *copy = (char *)malloc(len + 1);
    if (copy == NULL)
        return gpc_alloc_empty_string();

    if (len > 0)
        memcpy(copy, value, len);
    copy[len] = '\0';
    gpc_string_register_allocation(copy);
    return copy;
}

void gpc_string_assign(char **target, const char *value)
{
    if (target == NULL)
        return;

    char *existing = *target;
    if (gpc_string_release_allocation(existing))
    {
        free(existing);
        *target = NULL;
    }

    char *copy = gpc_string_duplicate(value);
    *target = copy;
}

void *gpc_dynarray_clone_descriptor(const void *descriptor, size_t descriptor_size)
{
    if (descriptor_size == 0)
        descriptor_size = sizeof(gpc_dynarray_descriptor_t);

    void *temp = malloc(descriptor_size);
    if (temp == NULL)
        return NULL;

    if (descriptor != NULL)
        memcpy(temp, descriptor, descriptor_size);
    else
        memset(temp, 0, descriptor_size);

    return temp;
}

void gpc_dynarray_assign_descriptor(void *dest_descriptor, const void *src_descriptor,
    size_t descriptor_size)
{
    if (dest_descriptor == NULL || src_descriptor == NULL)
        return;
    
    if (descriptor_size == 0)
        descriptor_size = sizeof(gpc_dynarray_descriptor_t);

    memcpy(dest_descriptor, src_descriptor, descriptor_size);
}

void gpc_dynarray_assign_from_temp(void *dest_descriptor, void *temp_descriptor,
    size_t descriptor_size)
{
    if (dest_descriptor == NULL)
    {
        if (temp_descriptor != NULL)
            free(temp_descriptor);
        return;
    }

    if (descriptor_size == 0)
        descriptor_size = sizeof(gpc_dynarray_descriptor_t);

    if (temp_descriptor == NULL)
    {
        memset(dest_descriptor, 0, descriptor_size);
        return;
    }

    memcpy(dest_descriptor, temp_descriptor, descriptor_size);
    free(temp_descriptor);
}

/* Copy a string literal to a char array (fixed-size buffer)
 * Fills the entire array. If the string is shorter, pads with nulls.
 * If the string is longer, truncates to fit.
 * Note: May NOT be null-terminated if string exactly fills the array!
 */
void gpc_string_to_char_array(char *dest, const char *src, size_t dest_size)
{
    if (dest == NULL || src == NULL || dest_size == 0)
        return;
    
    size_t src_len = strlen(src);
    size_t copy_len = (src_len < dest_size) ? src_len : dest_size;
    
    memcpy(dest, src, copy_len);
    
    /* Pad remaining space with zeros if string is shorter than array */
    if (copy_len < dest_size)
        memset(dest + copy_len, 0, dest_size - copy_len);
}


static char *gpc_textfile_duplicate_path(const char *path)
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

static void gpc_textfile_free_path(GPCTextFile *file)
{
    if (file == NULL || file->path == NULL)
        return;

    free(file->path);
    file->path = NULL;
}

static void gpc_textfile_close_handle(GPCTextFile *file)
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

static GPCTextFile *gpc_textfile_prepare(GPCTextFile **slot)
{
    if (slot == NULL)
        return NULL;

    GPCTextFile *file = *slot;
    if (file == NULL)
    {
        file = (GPCTextFile *)calloc(1, sizeof(GPCTextFile));
        if (file == NULL)
            return NULL;
        *slot = file;
    }

    return file;
}

static char *gpc_text_read_line_from_stream(FILE *stream)
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

void gpc_text_assign(GPCTextFile **slot, const char *path)
{
    if (slot == NULL)
        return;

    GPCTextFile *file = gpc_textfile_prepare(slot);
    if (file == NULL)
        return;

    gpc_textfile_close_handle(file);
    gpc_textfile_free_path(file);

    if (path != NULL)
        file->path = gpc_textfile_duplicate_path(path);
}

void gpc_text_rewrite(GPCTextFile **slot)
{
    GPCTextFile *file = gpc_textfile_prepare(slot);
    if (file == NULL || file->path == NULL)
        return;

    gpc_textfile_close_handle(file);

    file->handle = fopen(file->path, "w");
    if (file->handle != NULL)
        file->mode = 1;
    else
        file->mode = 0;
}

void gpc_text_reset(GPCTextFile **slot)
{
    GPCTextFile *file = gpc_textfile_prepare(slot);
    if (file == NULL || file->path == NULL)
        return;

    gpc_textfile_close_handle(file);

    file->handle = fopen(file->path, "r");
    if (file->handle != NULL)
        file->mode = 2;
    else
        file->mode = 0;
}

void gpc_text_close(GPCTextFile **slot)
{
    if (slot == NULL)
        return;

    GPCTextFile *file = *slot;
    if (file == NULL)
        return;

    gpc_textfile_close_handle(file);
}

int gpc_text_eof(GPCTextFile *file)
{
    FILE *stream = gpc_text_input_stream(file);
    if (stream == NULL)
        return 1;

    int ch = fgetc(stream);
    if (ch == EOF)
        return 1;

    if (ungetc(ch, stream) == EOF)
        return 1;

    return 0;
}

int gpc_text_eof_default(void)
{
    return gpc_text_eof(NULL);
}

void gpc_text_readln_into(GPCTextFile *file, char **target)
{
    if (target == NULL)
        return;

    FILE *stream = gpc_text_input_stream(file);
    if (stream == NULL)
    {
        gpc_string_assign(target, "");
        return;
    }

    char *line = gpc_text_read_line_from_stream(stream);
    if (line == NULL)
    {
        gpc_string_assign(target, "");
        return;
    }

    gpc_string_assign(target, line);
    free(line);
}

void gpc_text_readln_into_char(GPCTextFile *file, char *target)
{
    if (target == NULL)
        return;

    FILE *stream = gpc_text_input_stream(file);
    if (stream == NULL)
    {
        *target = '\0';
        return;
    }

    char *line = gpc_text_read_line_from_stream(stream);
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

void gpc_text_readln_discard(GPCTextFile *file)
{
    FILE *stream = gpc_text_input_stream(file);
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


void gpc_move(void *dest, const void *src, size_t count)
{
    if (dest == NULL || src == NULL || count == 0)
        return;

    memmove(dest, src, count);
}

char *gpc_string_concat(const char *lhs, const char *rhs)
{
    if (lhs == NULL)
        lhs = "";
    if (rhs == NULL)
        rhs = "";

    size_t lhs_len = strlen(lhs);
    size_t rhs_len = strlen(rhs);
    size_t total = lhs_len + rhs_len;

    char *result = (char *)malloc(total + 1);
    if (result == NULL)
        return gpc_alloc_empty_string();

    if (lhs_len > 0)
        memcpy(result, lhs, lhs_len);
    if (rhs_len > 0)
        memcpy(result + lhs_len, rhs, rhs_len);
    result[total] = '\0';
    gpc_string_register_allocation(result);
    return result;
}

int64_t gpc_string_length(const char *value)
{
    if (value == NULL)
        return 0;

    size_t len = strlen(value);
    return (int64_t)len;
}

char *gpc_string_copy(const char *value, int64_t index, int64_t count)
{
    if (value == NULL)
        value = "";

    size_t len = strlen(value);
    if (index < 1 || index > (int64_t)len)
        return gpc_alloc_empty_string();

    if (count < 0)
        count = 0;

    size_t start = (size_t)(index - 1);
    size_t available = len - start;
    size_t to_copy = (size_t)count;
    if (to_copy > available)
        to_copy = available;

    char *result = (char *)malloc(to_copy + 1);
    if (result == NULL)
        return gpc_alloc_empty_string();

    if (to_copy > 0)
        memcpy(result, value + start, to_copy);
    result[to_copy] = '\0';
    gpc_string_register_allocation(result);
    return result;
}

int64_t gpc_string_compare(const char *lhs, const char *rhs)
{
    if (lhs == NULL)
        lhs = "";
    if (rhs == NULL)
        rhs = "";

    int cmp = strcmp(lhs, rhs);
    return (int64_t)cmp;
}

static long long gpc_val_error_position(const char *text, const char *error_ptr)
{
    if (text == NULL || error_ptr == NULL)
        return 1;
    return (long long)((error_ptr - text) + 1);
}

static const char *gpc_val_skip_trailing_whitespace(const char *ptr)
{
    if (ptr == NULL)
        return NULL;
    while (*ptr != '\0' && isspace((unsigned char)*ptr))
        ++ptr;
    return ptr;
}

static long long gpc_val_parse_integer(const char *text, long long min_value,
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
        return gpc_val_error_position(text, endptr);

    const char *rest = gpc_val_skip_trailing_whitespace(endptr);
    if (rest != NULL && *rest != '\0')
        return gpc_val_error_position(text, rest);

    if (out_value != NULL)
        *out_value = value;
    return 0;
}

static long long gpc_val_parse_real(const char *text, double *out_value)
{
    if (text == NULL)
        text = "";

    errno = 0;
    char *endptr = NULL;
    double value = strtod(text, &endptr);
    if (endptr == text)
        return 1;

    if (errno == ERANGE)
        return gpc_val_error_position(text, endptr);

    const char *rest = gpc_val_skip_trailing_whitespace(endptr);
    if (rest != NULL && *rest != '\0')
        return gpc_val_error_position(text, rest);

    if (out_value != NULL)
        *out_value = value;
    return 0;
}

long long gpc_val_integer(const char *text, int32_t *out_value)
{
    long long parsed = 0;
    long long code = gpc_val_parse_integer(text, INT32_MIN, INT32_MAX, &parsed);
    if (code == 0 && out_value != NULL)
        *out_value = (int32_t)parsed;
    return code;
}

long long gpc_val_longint(const char *text, int64_t *out_value)
{
    long long parsed = 0;
    long long code = gpc_val_parse_integer(text, INT64_MIN, INT64_MAX, &parsed);
    if (code == 0 && out_value != NULL)
        *out_value = parsed;
    return code;
}

long long gpc_val_real(const char *text, double *out_value)
{
    double parsed = 0.0;
    long long code = gpc_val_parse_real(text, &parsed);
    if (code == 0 && out_value != NULL)
        *out_value = parsed;
    return code;
}

/* Chr function - returns a character value as an integer */
int64_t gpc_chr(int64_t value)
{
    /* Clamp value to valid character range [0, 255] */
    if (value < 0)
        return 0;
    if (value > 255)
        return 255;
    
    return value;
}

/* Convert a character value to a single-character string */
char *gpc_char_to_string(int64_t value)
{
    /* Clamp value to valid character range [0, 255] */
    if (value < 0)
        value = 0;
    else if (value > 255)
        value = 255;

    char *result = (char *)malloc(2);
    if (result == NULL)
        return gpc_alloc_empty_string();

    result[0] = (char)value;
    result[1] = '\0';
    gpc_string_register_allocation(result);
    return result;
}

int64_t gpc_ord_string(const char *value)
{
    if (value == NULL || value[0] == '\0')
        return 0;

    return (unsigned char)value[0];
}

int64_t gpc_ord_longint(int64_t value)
{
    return value;
}

char *gpc_int_to_str(int64_t value)
{
    char buffer[32];
    int written = snprintf(buffer, sizeof(buffer), "%lld", (long long)value);
    if (written < 0)
        return gpc_alloc_empty_string();

    char *result = (char *)malloc((size_t)written + 1);
    if (result == NULL)
        return gpc_alloc_empty_string();

    memcpy(result, buffer, (size_t)written + 1);
    gpc_string_register_allocation(result);
    return result;
}

int64_t gpc_now(void)
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

char *gpc_format_datetime(const char *format, int64_t datetime_ms)
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
        return gpc_alloc_empty_string();
#else
    if (localtime_r(&seconds, &tm_value) == NULL)
        return gpc_alloc_empty_string();
#endif

    size_t capacity = 64;
    size_t length = 0;
    char *result = (char *)malloc(capacity);
    if (result == NULL)
        return gpc_alloc_empty_string();
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
                        return gpc_alloc_empty_string();
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
                    return gpc_alloc_empty_string();
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
                return gpc_alloc_empty_string();
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
                return gpc_alloc_empty_string();
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
                return gpc_alloc_empty_string();
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
                return gpc_alloc_empty_string();
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
                return gpc_alloc_empty_string();
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
                return gpc_alloc_empty_string();
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
                return gpc_alloc_empty_string();
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
                return gpc_alloc_empty_string();
            }
            cursor += 3;
            continue;
        }

        if (!append_char(&result, &length, &capacity, *cursor++))
        {
            free(result);
            return gpc_alloc_empty_string();
        }
    }

    return result;
}
