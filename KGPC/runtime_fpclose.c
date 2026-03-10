#ifdef _WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

int fpClose(int fd)
{
#ifdef _WIN32
    return _close(fd);
#else
    return close(fd);
#endif
}
