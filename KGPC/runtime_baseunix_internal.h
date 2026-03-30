#ifndef KGPC_RUNTIME_BASEUNIX_INTERNAL_H
#define KGPC_RUNTIME_BASEUNIX_INTERNAL_H

#ifdef _WIN32
#include <string.h>
#include <fcntl.h>
#include <io.h>

/* Linux open flag constants for cross-platform translation */
#define LINUX_O_CREAT  0x40
#define LINUX_O_TRUNC  0x200

/* Translate Unix paths to Windows equivalents */
static inline const char* translate_unix_path(const char *path)
{
    /* Map /dev/null to NUL */
    if (path != NULL && strcmp(path, "/dev/null") == 0)
        return "NUL";
    return path;
}

/* Translate Unix open flags to Windows _open flags */
static inline int translate_flags(int flags)
{
    int wflags = _O_BINARY;  /* Always use binary mode on Windows */

    /* O_RDONLY = 0, O_WRONLY = 1, O_RDWR = 2 */
    int accmode = flags & 3;
    if (accmode == 0)       /* O_RDONLY */
        wflags |= _O_RDONLY;
    else if (accmode == 1)  /* O_WRONLY */
        wflags |= _O_WRONLY;
    else if (accmode == 2)  /* O_RDWR */
        wflags |= _O_RDWR;

    if (flags & LINUX_O_CREAT)
        wflags |= _O_CREAT;

    if (flags & LINUX_O_TRUNC)
        wflags |= _O_TRUNC;

    return wflags;
}
#endif

#endif /* KGPC_RUNTIME_BASEUNIX_INTERNAL_H */
