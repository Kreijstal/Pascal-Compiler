/*
 * runtime_baseunix.c — KGPC runtime support for FPC's BaseUnix unit
 *
 * CONTRACT: This file provides raw POSIX file-descriptor and filesystem
 * syscall wrappers that mirror FPC's BaseUnix unit
 * (FPCSource/rtl/unix/baseunix.pp).  Every exported symbol uses FPC's "fp"
 * naming convention (fpOpen, fpClose, fpRead, fpWrite, fplSeek, fpGetCwd,
 * fpchmod) and is declared as "external" inside KGPC/Units/baseunix.p.
 *
 * Belongs here:
 *   - File-descriptor operations: fpOpen / fpClose / fpRead / fpWrite /
 *     fplSeek / fpchmod
 *   - Directory helpers: fpGetCwd
 *   - Overload variants required by the KGPC name-mangling scheme:
 *     fpOpen_i_i_i  (three-argument overload: path, flags, mode)
 *     fpClose_i     (one-argument overload: fd; mangled name for Pascal call
 *                    site fpClose(fd: cint) when cint maps to "_i")
 *
 * Does NOT belong here:
 *   - Higher-level helpers with the "kgpc_unix_" prefix — those live in
 *     runtime_unix.c because they back FPC's Unix unit.
 *   - Stat, fork, exec, signal — those symbols are declared external in
 *     baseunix.p but resolved directly against the system C library at link
 *     time (stat, fork, execve, etc.); no wrapper layer is needed here.
 *
 * Overload naming: the KGPC name-mangler appends a type suffix per parameter
 * (e.g. "_i" for Integer/cint, "_pc" for PAnsiChar).  fpOpen and fpClose
 * each have exactly one Pascal declaration in baseunix.p but Pascal overload
 * resolution always uses the mangled name at the call site.  Therefore BOTH
 * the bare C name (fpOpen, fpClose) and the mangled C name (fpOpen_i_i_i,
 * fpClose_i) are provided so that any variation in call-site mangling finds
 * a defined symbol.
 *
 * Windows portability: the #ifdef _WIN32 guards translate Linux open-flag
 * constants and paths (/dev/null → NUL) via helpers in
 * runtime_baseunix_internal.h.
 */

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
