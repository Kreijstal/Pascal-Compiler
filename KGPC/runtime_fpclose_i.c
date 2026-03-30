#ifndef _WIN32
#include <unistd.h>
#else
#include <io.h>
#endif

int fpClose_i(int fd)
{
#ifdef _WIN32
    return _close(fd);
#else
    return close(fd);
#endif
}
