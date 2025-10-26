#define _GNU_SOURCE
#if defined(__has_include)
#  if __has_include(<libunwind.h>)
#    define HAVE_LIBUNWIND 1
#  endif
#endif

#ifdef HAVE_LIBUNWIND
#include <libunwind.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

#include "stacktrace.h"

static void stack_trace_handler(int sig) {
    unw_context_t context;
    unw_cursor_t cursor;
    unw_word_t ip, off;
    char proc_name[256];

    fprintf(stderr, "Caught signal %d\n", sig);

    unw_getcontext(&context);
    unw_init_local(&cursor, &context);

    fprintf(stderr, "Stack trace:\n");

    while (unw_step(&cursor) > 0) {
        if (unw_get_reg(&cursor, UNW_REG_IP, &ip)) {
            fprintf(stderr, "  (unable to get instruction pointer)\n");
            continue;
        }

        if (unw_get_proc_name(&cursor, proc_name, sizeof(proc_name), &off) == 0) {
            fprintf(stderr, "  0x%lx: %s + 0x%lx\n", (long)ip, proc_name, (long)off);
        } else {
            fprintf(stderr, "  0x%lx: (unable to get procedure name)\n", (long)ip);
        }
    }

    exit(sig);
}

void install_stack_trace_handler(void) {
    struct sigaction sa;
    sa.sa_handler = stack_trace_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;

    sigaction(SIGSEGV, &sa, NULL);
    sigaction(SIGFPE, &sa, NULL);
    sigaction(SIGABRT, &sa, NULL);
}

#else

#include <signal.h>

#include "stacktrace.h"

void install_stack_trace_handler(void) {
    /* libunwind not available; no-op handler */
    signal(SIGSEGV, SIG_DFL);
    signal(SIGFPE, SIG_DFL);
    signal(SIGABRT, SIG_DFL);
}

#endif
