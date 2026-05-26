/**
 * @file runtime_baseunix_internal.h
 * @brief Private Windows-portability shims for `runtime_baseunix.c`.
 *
 * Included ONLY by `runtime_baseunix.c` — path translation and Unix→Win32
 * open-flag translation that are not part of the public runtime ABI.
 * Do not include from other files.
 */

#ifndef KGPC_RUNTIME_BASEUNIX_INTERNAL_H
#define KGPC_RUNTIME_BASEUNIX_INTERNAL_H

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#include <string.h>

/** @brief Linux `O_CREAT` constant for cross-platform flag translation. */
#define LINUX_O_CREAT 0x40
/** @brief Linux `O_TRUNC` constant for cross-platform flag translation. */
#define LINUX_O_TRUNC 0x200

/**
 * @brief Translate Unix-style paths to their Windows equivalents.
 *
 * Currently maps `/dev/null` → `NUL`; passes everything else through.
 */
static inline const char *translate_unix_path(const char *path) {
  /* Map /dev/null to NUL */
  if (path != NULL && strcmp(path, "/dev/null") == 0)
    return "NUL";
  return path;
}

/**
 * @brief Translate Unix `O_*` flags into Windows `_open` flags.
 *
 * Always sets `_O_BINARY` so KGPC files behave the same as on Unix.
 */
static inline int translate_flags(int flags) {
  int wflags = _O_BINARY; /* Always use binary mode on Windows */

  /* O_RDONLY = 0, O_WRONLY = 1, O_RDWR = 2 */
  int accmode = flags & 3;
  if (accmode == 0) /* O_RDONLY */
    wflags |= _O_RDONLY;
  else if (accmode == 1) /* O_WRONLY */
    wflags |= _O_WRONLY;
  else if (accmode == 2) /* O_RDWR */
    wflags |= _O_RDWR;

  if (flags & LINUX_O_CREAT)
    wflags |= _O_CREAT;

  if (flags & LINUX_O_TRUNC)
    wflags |= _O_TRUNC;

  return wflags;
}
#endif

#endif /* KGPC_RUNTIME_BASEUNIX_INTERNAL_H */
