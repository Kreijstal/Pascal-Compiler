/**
 * @file file_lock.h
 * @brief Cooperative inter-process advisory file lock.
 *
 * Used to serialise concurrent KGPC invocations that touch shared
 * caches (pp-cache, AST cache, etc.) in CI matrices.  Re-entrant within
 * a single process: nested acquires of the same path on the same PID
 * increment a refcount and only the matching release tears down.
 */
#ifndef KGPC_FILE_LOCK_H
#define KGPC_FILE_LOCK_H

#include <stdbool.h>

/**
 * @brief Acquire an advisory exclusive lock on @p path.
 *
 * Creates `${path}.lock` (best-effort) and blocks via @c fcntl until the
 * lock is held or @p timeout_secs elapses.  Re-entrant on the same path
 * within the same process.  @p timeout_secs is the maximum number of
 * seconds to wait; pass a negative value to block indefinitely.
 *
 * @returns true on success; false on timeout, error, or @p path NULL/empty.
 */
bool file_lock_acquire(const char *path, int timeout_secs);

/**
 * @brief Release a lock previously acquired via @ref file_lock_acquire.
 *
 * Decrements the in-process refcount; the underlying fcntl lock is only
 * released when the refcount reaches zero.
 */
void file_lock_release(const char *path);

#endif /* KGPC_FILE_LOCK_H */
