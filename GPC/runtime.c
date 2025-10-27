#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

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

static void gpc_dynarray_set_length_impl(void **array_ptr, long long length, size_t element_size) {
    if (array_ptr == NULL)
        return;

    if (length < 0)
        length = 0;

    size_t header_size = sizeof(size_t);
    size_t requested_count = (length > 0) ? (size_t)length + 1 : 0;

    unsigned char *data_ptr = (unsigned char *)(*array_ptr);
    unsigned char *base_ptr = data_ptr != NULL ? data_ptr - header_size : NULL;
    size_t old_count = base_ptr != NULL ? *((size_t *)base_ptr) : 0;

    if (requested_count == 0) {
        if (base_ptr != NULL)
            free(base_ptr);
        *array_ptr = NULL;
        return;
    }

    size_t total_size = header_size + requested_count * element_size;
    unsigned char *new_base = (unsigned char *)realloc(base_ptr, total_size);
    if (new_base == NULL)
        return;

    *((size_t *)new_base) = requested_count;
    unsigned char *new_data = new_base + header_size;

    if (requested_count > old_count) {
        size_t grow_bytes = (requested_count - old_count) * element_size;
        memset(new_data + old_count * element_size, 0, grow_bytes);
    }

    /* Always keep the sentinel slot zeroed. */
    memset(new_data + (requested_count - 1) * element_size, 0, element_size);

    *array_ptr = new_data;
}

void gpc_dynarray_set_length_integer(int32_t **array_ptr, long long length) {
    gpc_dynarray_set_length_impl((void **)array_ptr, length, sizeof(int32_t));
}

void gpc_dynarray_set_length_longint(int64_t **array_ptr, long long length) {
    gpc_dynarray_set_length_impl((void **)array_ptr, length, sizeof(int64_t));
}

void gpc_runtime_halt(void) {
    exit(0);
}
