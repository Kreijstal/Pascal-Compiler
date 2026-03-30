#ifdef _WIN32
#include <io.h>
#else
#include <sys/stat.h>
#endif

int fpchmod(const char *path, int mode)
{
#ifdef _WIN32
    return _chmod(path, mode);
#else
    return chmod(path, (mode_t)mode);
#endif
}
