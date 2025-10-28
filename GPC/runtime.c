#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
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

void gpc_move(void *dest, const void *src, size_t count)
{
    if (dest == NULL || src == NULL || count == 0)
        return;

    memmove(dest, src, count);
}

int64_t gpc_now_ms(void)
{
#ifdef _WIN32
    FILETIME ft;
    GetSystemTimeAsFileTime(&ft);
    ULARGE_INTEGER uli;
    uli.LowPart = ft.dwLowDateTime;
    uli.HighPart = ft.dwHighDateTime;
    const uint64_t EPOCH_DIFF = 116444736000000000ULL; /* Difference between 1601 and 1970 */
    if (uli.QuadPart < EPOCH_DIFF)
        return 0;
    uint64_t unix_100ns = uli.QuadPart - EPOCH_DIFF;
    return (int64_t)(unix_100ns / 10000ULL);
#else
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
        return 0;
    return (int64_t)ts.tv_sec * 1000LL + (int64_t)(ts.tv_nsec / 1000000LL);
#endif
}

typedef struct
{
    char *data;
    size_t length;
    size_t capacity;
} gpc_string_builder_t;

static bool gpc_sb_init(gpc_string_builder_t *sb, size_t initial_capacity)
{
    if (initial_capacity == 0)
        initial_capacity = 32;

    sb->data = (char *)malloc(initial_capacity);
    if (sb->data == NULL)
        return false;

    sb->length = 0;
    sb->capacity = initial_capacity;
    sb->data[0] = '\0';
    return true;
}

static bool gpc_sb_reserve(gpc_string_builder_t *sb, size_t extra)
{
    if (sb == NULL)
        return false;

    size_t required = sb->length + extra + 1;
    if (required <= sb->capacity)
        return true;

    size_t new_capacity = sb->capacity;
    while (new_capacity < required)
        new_capacity *= 2;

    char *new_data = (char *)realloc(sb->data, new_capacity);
    if (new_data == NULL)
        return false;

    sb->data = new_data;
    sb->capacity = new_capacity;
    return true;
}

static bool gpc_sb_append_char(gpc_string_builder_t *sb, char ch)
{
    if (!gpc_sb_reserve(sb, 1))
        return false;

    sb->data[sb->length++] = ch;
    sb->data[sb->length] = '\0';
    return true;
}

static bool gpc_sb_append_str(gpc_string_builder_t *sb, const char *text)
{
    if (text == NULL)
        return true;

    size_t len = strlen(text);
    if (len == 0)
        return true;

    if (!gpc_sb_reserve(sb, len))
        return false;

    memcpy(sb->data + sb->length, text, len);
    sb->length += len;
    sb->data[sb->length] = '\0';
    return true;
}

static bool gpc_emit_negative_prefix(gpc_string_builder_t *sb, bool *negative_pending)
{
    if (negative_pending == NULL || !*negative_pending)
        return true;

    if (!gpc_sb_append_char(sb, '-'))
        return false;

    *negative_pending = false;
    return true;
}

static bool gpc_append_component(gpc_string_builder_t *sb, int value, size_t run_length, bool *negative_pending)
{
    if (!gpc_emit_negative_prefix(sb, negative_pending))
        return false;

    char buffer[32];
    if (run_length <= 1)
        snprintf(buffer, sizeof(buffer), "%d", value);
    else
    {
        int width = (int)run_length;
        if (width < 2)
            width = 2;
        snprintf(buffer, sizeof(buffer), "%0*d", width, value);
    }

    return gpc_sb_append_str(sb, buffer);
}

char *gpc_format_datetime(const char *format, int64_t datetime_ms)
{
    if (format == NULL)
        format = "";

    bool negative_pending = datetime_ms < 0;
    uint64_t abs_value;
    if (datetime_ms < 0)
        abs_value = (uint64_t)(-(datetime_ms + 1)) + 1;
    else
        abs_value = (uint64_t)datetime_ms;

    int millisecond = (int)(abs_value % 1000ULL);
    uint64_t total_seconds = abs_value / 1000ULL;
    int second = (int)(total_seconds % 60ULL);
    uint64_t total_minutes = total_seconds / 60ULL;
    int minute = (int)(total_minutes % 60ULL);
    uint64_t total_hours = total_minutes / 60ULL;
    int hour = (int)(total_hours % 24ULL);

    gpc_string_builder_t sb;
    if (!gpc_sb_init(&sb, 64))
        return NULL;

    bool in_quote = false;

    for (size_t i = 0; format[i] != '\0';)
    {
        char ch = format[i];

        if (ch == '\'')
        {
            if (format[i + 1] == '\'')
            {
                if (!gpc_emit_negative_prefix(&sb, &negative_pending))
                    goto error;
                if (!gpc_sb_append_char(&sb, '\''))
                    goto error;
                i += 2;
                continue;
            }

            in_quote = !in_quote;
            ++i;
            continue;
        }

        size_t run = 1;
        while (format[i + run] == ch)
            ++run;

        if (in_quote)
        {
            if (!gpc_emit_negative_prefix(&sb, &negative_pending))
                goto error;
            for (size_t j = 0; j < run; ++j)
            {
                if (!gpc_sb_append_char(&sb, ch))
                    goto error;
            }
            i += run;
            continue;
        }

        char lowered = (char)tolower((unsigned char)ch);
        switch (lowered)
        {
            case 'h':
                if (!gpc_append_component(&sb, hour, run, &negative_pending))
                    goto error;
                break;
            case 'n':
                if (!gpc_append_component(&sb, minute, run, &negative_pending))
                    goto error;
                break;
            case 's':
                if (!gpc_append_component(&sb, second, run, &negative_pending))
                    goto error;
                break;
            case 'z':
            {
                if (!gpc_emit_negative_prefix(&sb, &negative_pending))
                    goto error;
                int width = (int)run;
                if (width < 1)
                    width = 1;
                if (width > 3)
                    width = 3;

                char buffer[16];
                if (width == 1)
                    snprintf(buffer, sizeof(buffer), "%d", millisecond);
                else
                    snprintf(buffer, sizeof(buffer), "%0*d", width, millisecond);

                if (!gpc_sb_append_str(&sb, buffer))
                    goto error;
                break;
            }
            default:
                if (!gpc_emit_negative_prefix(&sb, &negative_pending))
                    goto error;
                for (size_t j = 0; j < run; ++j)
                {
                    if (!gpc_sb_append_char(&sb, ch))
                        goto error;
                }
                break;
        }

        i += run;
    }

    if (negative_pending)
    {
        if (!gpc_sb_append_char(&sb, '-'))
            goto error;
    }

    return sb.data;

error:
    free(sb.data);
    return NULL;
}

char *gpc_int_to_str(int64_t value)
{
    char buffer[32];
    int length = snprintf(buffer, sizeof(buffer), "%lld", (long long)value);
    if (length < 0)
        return NULL;

    char *result = (char *)malloc((size_t)length + 1);
    if (result == NULL)
        return NULL;

    memcpy(result, buffer, (size_t)length + 1);
    return result;
}

char *FormatDateTime(const char *format, int64_t datetime_ms)
{
    return gpc_format_datetime(format, datetime_ms);
}

char *IntToStr(long long value)
{
    int64_t signed_value = (int64_t)(int32_t)value;
    return gpc_int_to_str(signed_value);
}
