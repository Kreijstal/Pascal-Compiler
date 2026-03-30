#include "runtime_baseunix_internal.h"
#include <stddef.h>
#include <sys/types.h>

#ifdef _WIN32
#include <io.h>
#include <direct.h>
#include <limits.h>
typedef long long ssize_t;
#else
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#endif

/* File opening */
int fpOpen(const char *path, int flags)
{
#ifdef _WIN32
    const char *wpath = translate_unix_path(path);
    int wflags = translate_flags(flags);
    return _open(wpath, wflags);
#else
    return open(path, flags);
#endif
}

int fpOpen_i_i_i(const char *path, int flags, int mode)
{
#ifdef _WIN32
    const char *wpath = translate_unix_path(path);
    int wflags = translate_flags(flags);
    return _open(wpath, wflags, mode);
#else
    return open(path, flags, (mode_t)mode);
#endif
}

/* File closing */
int fpClose(int fd)
{
#ifdef _WIN32
    return _close(fd);
#else
    return close(fd);
#endif
}

int fpClose_i(int fd)
{
#ifdef _WIN32
    return _close(fd);
#else
    return close(fd);
#endif
}

/* Reading and writing */
ssize_t fpRead(int fd, void *buf, size_t count)
{
#ifdef _WIN32
    unsigned int safe_count = (count > UINT_MAX) ? UINT_MAX : (unsigned int)count;
    return (ssize_t)_read(fd, buf, safe_count);
#else
    return read(fd, buf, count);
#endif
}

ssize_t fpWrite(int fd, const void *buf, size_t count)
{
#ifdef _WIN32
    unsigned int safe_count = (count > UINT_MAX) ? UINT_MAX : (unsigned int)count;
    return (ssize_t)_write(fd, buf, safe_count);
#else
    return write(fd, buf, count);
#endif
}

/* Directory and seek */
char *fpGetCwd(char *path, size_t len)
{
    if (path == NULL || len == 0)
        return NULL;
#ifdef _WIN32
    return _getcwd(path, (int)len);
#else
    return getcwd(path, len);
#endif
}

off_t fplSeek(int fd, off_t offset, int whence)
{
#ifdef _WIN32
    return (off_t)_lseeki64(fd, (__int64)offset, whence);
#else
    return lseek(fd, offset, whence);
#endif
}

/* Permissions */
int fpchmod(const char *path, int mode)
{
#ifdef _WIN32
    return _chmod(path, mode);
#else
    return chmod(path, (mode_t)mode);
#endif
}
