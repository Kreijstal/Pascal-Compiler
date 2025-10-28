#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#include <errno.h>
#include <unistd.h>
#endif

static char *gpc_alloc_empty_string(void);

static int gpc_vprintf_impl(const char *format, va_list args) {
    return vprintf(format, args);
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

typedef struct {
    void *data;
    int64_t length;
} gpc_dynarray_descriptor_t;

int64_t gpc_dynarray_length(void *descriptor_ptr)
{
    if (descriptor_ptr == NULL)
        return 0;

    gpc_dynarray_descriptor_t *descriptor = (gpc_dynarray_descriptor_t *)descriptor_ptr;
    return descriptor->length;
}

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

int64_t gpc_length_string(const char *value)
{
    if (value == NULL)
        return 0;
    return (int64_t)strlen(value);
}

char *gpc_copy_string(const char *source, int64_t index, int64_t count)
{
    if (source == NULL)
        source = "";

    if (count <= 0)
        return gpc_alloc_empty_string();

    if (index < 1)
        index = 1;

    size_t src_len = strlen(source);
    size_t start = (size_t)((index > 0) ? (index - 1) : 0);
    if (start >= src_len)
        return gpc_alloc_empty_string();

    size_t max_count = (size_t)count;
    if (start + max_count > src_len)
        max_count = src_len - start;

    char *result = (char *)malloc(max_count + 1);
    if (result == NULL)
        return gpc_alloc_empty_string();

    memcpy(result, source + start, max_count);
    result[max_count] = '\0';
    return result;
}

char *gpc_int_to_str(int64_t value)
{
    char buffer[32];
    int written = snprintf(buffer, sizeof(buffer), "%lld", (long long)value);
    if (written < 0)
        return gpc_alloc_empty_string();

    size_t length = (size_t)written;
    char *result = (char *)malloc(length + 1);
    if (result == NULL)
        return gpc_alloc_empty_string();

    memcpy(result, buffer, length + 1);
    return result;
}

int64_t gpc_now(void)
{
#ifdef _WIN32
    FILETIME ft;
    GetSystemTimeAsFileTime(&ft);
    ULARGE_INTEGER ui;
    ui.LowPart = ft.dwLowDateTime;
    ui.HighPart = ft.dwHighDateTime;
    return (int64_t)(ui.QuadPart / 10000ULL);
#else
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
        return 0;
    return (int64_t)ts.tv_sec * 1000LL + (int64_t)(ts.tv_nsec / 1000000LL);
#endif
}

static void append_char(char **buffer, size_t *length, size_t *capacity, char ch)
{
    if (*length + 1 >= *capacity)
    {
        size_t new_capacity = (*capacity < 32) ? 64 : (*capacity * 2);
        char *new_buffer = (char *)realloc(*buffer, new_capacity);
        if (new_buffer == NULL)
            return;
        *buffer = new_buffer;
        *capacity = new_capacity;
    }
    (*buffer)[(*length)++] = ch;
    (*buffer)[*length] = '\0';
}

static void append_string(char **buffer, size_t *length, size_t *capacity, const char *text)
{
    if (text == NULL)
        return;
    while (*text != '\0')
        append_char(buffer, length, capacity, *text++);
}

char *gpc_format_datetime(const char *format, int64_t datetime_ms)
{
    if (format == NULL)
        format = "";

    int64_t total_ms = datetime_ms;
    if (total_ms < 0)
        total_ms = -total_ms;

    int64_t hours = (total_ms / 3600000LL) % 24LL;
    int64_t minutes = (total_ms / 60000LL) % 60LL;
    int64_t seconds = (total_ms / 1000LL) % 60LL;
    int64_t millis = total_ms % 1000LL;

    size_t capacity = strlen(format) + 32;
    char *result = (char *)malloc(capacity);
    if (result == NULL)
        return gpc_alloc_empty_string();
    size_t length = 0;
    result[0] = '\0';

    for (size_t i = 0; format[i] != '\0'; )
    {
        char ch = format[i];
        size_t run = 1;
        while (format[i + run] != '\0' && toupper((unsigned char)format[i + run]) == toupper((unsigned char)ch))
            ++run;

        char upper = (char)toupper((unsigned char)ch);
        char temp[32];
        if (upper == 'H' || upper == 'N' || upper == 'S' || upper == 'Z')
        {
            int width = (int)run;
            long long value = 0;
            if (upper == 'H')
                value = hours;
            else if (upper == 'N')
                value = minutes;
            else if (upper == 'S')
                value = seconds;
            else if (upper == 'Z')
            {
                if (width < 3)
                    width = 3;
                value = millis;
            }

            if (upper == 'Z' && width > 6)
                width = 6;

            snprintf(temp, sizeof(temp), "%0*lld", width, value);
            append_string(&result, &length, &capacity, temp);
        }
        else
        {
            for (size_t j = 0; j < run; ++j)
                append_char(&result, &length, &capacity, format[i + j]);
        }

        i += run;
    }

    return result;
}
void gpc_write_integer(int width, int64_t value)
{
    if (width > 0)
        printf("%*lld", width, (long long)value);
    else
        printf("%lld", (long long)value);
}

void gpc_write_string(int width, const char *value)
{
    if (value == NULL)
        value = "";
    if (width > 0)
        printf("%*s", width, value);
    else
        printf("%s", value);
}

void gpc_write_newline(void)
{
    putchar('\n');
}

void gpc_write_boolean(int width, int value)
{
    const char *text = value ? "TRUE" : "FALSE";
    if (width > 0)
        printf("%*s", width, text);
    else
        printf("%s", text);
}

static char *gpc_alloc_empty_string(void)
{
    char *empty = (char *)malloc(1);
    if (empty != NULL)
        empty[0] = '\0';
    return empty;
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
    return result;
}

char *gpc_chr(int64_t value)
{
    if (value < 0)
        value = 0;
    else if (value > 255)
        value = 255;

    char *result = (char *)malloc(2);
    if (result == NULL)
        return gpc_alloc_empty_string();

    result[0] = (char)(value & 0xFF);
    result[1] = '\0';
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
