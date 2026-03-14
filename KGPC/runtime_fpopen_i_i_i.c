#include <fcntl.h>
#include <sys/types.h>
#ifdef _WIN32
#include <io.h>
#endif

int fpOpen_i_i_i(const char *path, int flags, int mode)
{
#ifdef _WIN32
    return _open(path, flags, mode);
#else
    return open(path, flags, (mode_t)mode);
#endif
}
