/**
 * @file stacktrace.h
 * @brief Install a signal handler that prints a libunwind-based stack
 * trace on `SIGSEGV` / `SIGABRT`.
 *
 * Called once at startup from @c main_cparser.c.  Best-effort: returns
 * silently if libunwind support was disabled at build time.
 */
#ifndef STACKTRACE_H
#define STACKTRACE_H

/** @brief Register signal handlers that print a stack trace on crash. */
void install_stack_trace_handler(void);

#endif // STACKTRACE_H
