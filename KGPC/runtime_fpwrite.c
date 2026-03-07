#include <stddef.h>
#ifdef _WIN32
#include <io.h>
#include <limits.h>
typedef long long ssize_t;
#else
#include <unistd.h>
#endif

ssize_t fpWrite(int fd, const void *buf, size_t count)
{
#ifdef _WIN32
    unsigned int safe_count = (count > UINT_MAX) ? UINT_MAX : (unsigned int)count;
    return (ssize_t)_write(fd, buf, safe_count);
#else
    return write(fd, buf, count);
#endif
}
