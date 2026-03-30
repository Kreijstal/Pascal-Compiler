#include "runtime_baseunix_internal.h"
#ifndef _WIN32
#include <fcntl.h>
#include <unistd.h>
#endif

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
