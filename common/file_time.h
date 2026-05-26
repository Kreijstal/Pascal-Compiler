/**
 * @file file_time.h
 * @brief Cross-platform stat / mkdir helpers.
 *
 * Papers over @c st_mtim vs @c st_mtimespec vs Windows-style FILETIME
 * fields so callers can use a single nanosecond-resolution @c timespec
 * representation, and exposes a portable @c mkdir wrapper that drops
 * the mode argument on Windows.  Header-only.
 */
#ifndef KGPC_FILE_TIME_H
#define KGPC_FILE_TIME_H

#include <sys/stat.h>
#include <time.h>

#ifdef _WIN32
#include <direct.h>
#endif

/**
 * @brief Extract the modification time of @p st as a @c timespec.
 *
 * Returns the zero @c timespec when @p st is NULL.  On Linux this uses
 * @c st_mtim; on macOS @c st_mtimespec; on Windows the second-resolution
 * @c st_mtime with @c tv_nsec=0.
 */
static inline struct timespec kgpc_stat_mtime(const struct stat *st) {
  struct timespec ts;
  if (st == NULL) {
    ts.tv_sec = 0;
    ts.tv_nsec = 0;
    return ts;
  }

#ifdef _WIN32
  ts.tv_sec = st->st_mtime;
  ts.tv_nsec = 0;
#elif defined(__APPLE__) && defined(_DARWIN_FEATURE_64_BIT_INODE)
  ts = st->st_mtimespec;
#else
  ts = st->st_mtim;
#endif
  return ts;
}

/**
 * @brief Portable @c mkdir wrapper.
 *
 * On POSIX, @p mode is the directory permission bits as passed to
 * @c mkdir(2).  On Windows the parameter is ignored.
 * @returns 0 on success; -1 (and @c errno set) on failure.
 */
static inline int kgpc_mkdir(const char *path, int mode) {
#ifdef _WIN32
  (void)mode;
  return _mkdir(path);
#else
  return mkdir(path, (mode_t)mode);
#endif
}

#endif /* KGPC_FILE_TIME_H */
