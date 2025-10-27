#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#include <errno.h>
#include <unistd.h>
#endif

#if defined(_WIN32) && defined(__GNUC__)
#define GPC_SYSV_ABI __attribute__((sysv_abi))
#else
#define GPC_SYSV_ABI
#endif

static int gpc_vprintf_impl(const char *format, va_list args) {
    return vprintf(format, args);
}

int GPC_SYSV_ABI gpc_printf(const char *format, ...) {
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

int GPC_SYSV_ABI gpc_scanf(const char *format, ...) {
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
