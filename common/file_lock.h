#ifndef KGPC_FILE_LOCK_H
#define KGPC_FILE_LOCK_H

#include <stdbool.h>

bool file_lock_acquire(const char *path, int timeout_secs);
void file_lock_release(const char *path);

#endif /* KGPC_FILE_LOCK_H */
