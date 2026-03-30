#include <sys/types.h>
#ifdef _WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

off_t fplSeek(int fd, off_t offset, int whence)
{
#ifdef _WIN32
    return (off_t)_lseeki64(fd, (__int64)offset, whence);
#else
    return lseek(fd, offset, whence);
#endif
}
