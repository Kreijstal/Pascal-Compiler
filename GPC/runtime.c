#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#ifdef _WIN32
#include <stdarg.h>

int __isoc99_scanf(const char *format, ...) {
    int result;
    va_list args;
    va_start(args, format);
    result = vscanf(format, args);
    va_end(args);
    return result;
}
#endif

void print_integer(int n) {
    printf("%d\n", n);
}

uint64_t gpc_get_tick_count64(void) {
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
        return 0;
    }
    return (uint64_t)ts.tv_sec * 1000ULL + (uint64_t)(ts.tv_nsec / 1000000ULL);
}

void gpc_sleep_ms(int milliseconds) {
    if (milliseconds <= 0)
        return;

    struct timespec req;
    req.tv_sec = milliseconds / 1000;
    req.tv_nsec = (long)(milliseconds % 1000) * 1000000L;

    while (nanosleep(&req, &req) == -1 && errno == EINTR) {
        /* Retry until sleep completes */
    }
}
