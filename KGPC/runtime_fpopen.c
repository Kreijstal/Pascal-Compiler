#include <fcntl.h>
#ifdef _WIN32
#include <io.h>
#endif

int fpOpen(const char *path, int flags)
{
#ifdef _WIN32
    return _open(path, flags);
#else
    return open(path, flags);
#endif
}
