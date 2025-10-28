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

int64_t gpc_string_length(const char *value)
{
    if (value == NULL)
        return 0;
    return (int64_t)strlen(value);
}

char *gpc_string_copy(const char *source, int64_t index, int64_t count)
{
    if (source == NULL || count <= 0)
        return gpc_alloc_empty_string();

    size_t length = strlen(source);
    if (index <= 0)
        index = 1;

    size_t start = (size_t)(index - 1);
    if (start >= length)
        return gpc_alloc_empty_string();

    size_t available = length - start;
    size_t copy_len = (size_t)count;
    if (copy_len > available)
        copy_len = available;

    char *result = (char *)malloc(copy_len + 1);
    if (result == NULL)
        return gpc_alloc_empty_string();

    memcpy(result, source + start, copy_len);
    result[copy_len] = '\0';
    return result;
}

char *gpc_int_to_string(int64_t value)
{
    char buffer[32];
    int written = snprintf(buffer, sizeof(buffer), "%lld", (long long)value);
    if (written < 0)
        return gpc_alloc_empty_string();
    char *result = strdup(buffer);
    if (result == NULL)
        return gpc_alloc_empty_string();
    return result;
}

int64_t gpc_now(void)
{
    time_t current = time(NULL);
    if (current < 0)
        return 0;
    return (int64_t)current;
}

static char *gpc_translate_datetime_format(const char *format)
{
    if (format == NULL)
        return strdup("%Y-%m-%d %H:%M:%S");

    size_t len = strlen(format);
    size_t capacity = len * 4 + 16;
    char *buffer = (char *)malloc(capacity);
    if (buffer == NULL)
        return strdup("%Y-%m-%d %H:%M:%S");

    size_t out = 0;
    for (size_t i = 0; format[i] != '\0'; )
    {
        char ch = format[i];
        if (ch == '\'')
        {
            ++i;
            while (format[i] != '\0' && format[i] != '\'')
            {
                if (out + 2 >= capacity)
                {
                    capacity *= 2;
                    char *temp = (char *)realloc(buffer, capacity);
                    if (temp == NULL)
                    {
                        free(buffer);
                        return strdup("%Y-%m-%d %H:%M:%S");
                    }
                    buffer = temp;
                }
                if (format[i] == '%')
                    buffer[out++] = '%';
                buffer[out++] = format[i++];
            }
            if (format[i] == '\'')
                ++i;
            continue;
        }

        size_t count = 1;
        while (format[i + count] == ch)
            ++count;

        char lower = (char)tolower((unsigned char)ch);
        const char *replacement = NULL;
        switch (lower)
        {
            case 'y':
                replacement = (count >= 4) ? "%Y" : "%y";
                break;
            case 'm':
                replacement = "%m";
                break;
            case 'd':
                replacement = "%d";
                break;
            case 'h':
                replacement = "%H";
                break;
            case 'n':
                replacement = "%M";
                break;
            case 's':
                replacement = "%S";
                break;
            case 't':
                if (count >= 2)
                    replacement = "%p";
                break;
            default:
                break;
        }

        if (replacement != NULL)
        {
            size_t repl_len = strlen(replacement);
            if (out + repl_len + 1 >= capacity)
            {
                capacity = (out + repl_len + 1) * 2;
                char *temp = (char *)realloc(buffer, capacity);
                if (temp == NULL)
                {
                    free(buffer);
                    return strdup("%Y-%m-%d %H:%M:%S");
                }
                buffer = temp;
            }
            memcpy(buffer + out, replacement, repl_len);
            out += repl_len;
        }
        else
        {
            if (out + 2 >= capacity)
            {
                capacity *= 2;
                char *temp = (char *)realloc(buffer, capacity);
                if (temp == NULL)
                {
                    free(buffer);
                    return strdup("%Y-%m-%d %H:%M:%S");
                }
                buffer = temp;
            }
            if (ch == '%')
                buffer[out++] = '%';
            buffer[out++] = ch;
        }

        i += count;
    }

    if (out >= capacity)
    {
        char *temp = (char *)realloc(buffer, out + 1);
        if (temp != NULL)
            buffer = temp;
    }
    buffer[out] = '\0';
    return buffer;
}

char *gpc_format_datetime(const char *format, int64_t datetime)
{
    char *strftime_format = gpc_translate_datetime_format(format);
    if (strftime_format == NULL)
        return gpc_alloc_empty_string();

    time_t raw_time = (time_t)datetime;
    struct tm tm_value;
    struct tm *tm_ptr = localtime(&raw_time);
    if (tm_ptr == NULL)
    {
        free(strftime_format);
        return gpc_alloc_empty_string();
    }
    tm_value = *tm_ptr;

    size_t buffer_size = 64;
    char *result = NULL;
    for (int attempt = 0; attempt < 5; ++attempt)
    {
        char *buffer = (char *)malloc(buffer_size);
        if (buffer == NULL)
            break;
        size_t written = strftime(buffer, buffer_size, strftime_format, &tm_value);
        if (written > 0)
        {
            result = buffer;
            break;
        }
        free(buffer);
        buffer_size *= 2;
    }

    free(strftime_format);
    if (result == NULL)
        return gpc_alloc_empty_string();
    return result;
}

int64_t gpc_dynarray_length(const void *descriptor_ptr)
{
    if (descriptor_ptr == NULL)
        return 0;
    const gpc_dynarray_descriptor_t *descriptor = (const gpc_dynarray_descriptor_t *)descriptor_ptr;
    return (descriptor->length < 0) ? 0 : descriptor->length;
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
