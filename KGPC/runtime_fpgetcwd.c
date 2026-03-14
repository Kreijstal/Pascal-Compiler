#include <stddef.h>
#ifdef _WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

char *fpGetCwd(char *path, size_t len)
{
#ifdef _WIN32
    if (path == NULL || len == 0)
        return NULL;
    return _getcwd(path, (int)len);
#else
    return getcwd(path, len);
#endif
}
