#ifndef KGPC_FILE_LOCK_H
#define KGPC_FILE_LOCK_H

#include <stdbool.h>

/**
 * Acquire a file-based lock for the given resource path.
 *
 * Uses `<path>.lock`. If the lock is held by another process, it waits/polls
 * with exponential backoff until it can be acquired or `timeout_secs` is reached.
 * Stale locks (held by dead processes) are detected and removed.
 *
 * Returns true if the lock was acquired, false on timeout or error.
 */
bool file_lock_acquire(const char *path, int timeout_secs);

/**
 * Release a file-based lock previously acquired by file_lock_acquire.
 */
void file_lock_release(const char *path);

#endif /* KGPC_FILE_LOCK_H */
