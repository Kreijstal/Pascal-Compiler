#include "runtime_baseunix_internal.h"
#ifndef _WIN32
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#endif

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
