#ifdef _WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

int fpClose_i(int fd)
{
#ifdef _WIN32
    return _close(fd);
#else
    return close(fd);
#endif
}
