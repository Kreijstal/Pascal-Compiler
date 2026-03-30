#include <stddef.h>
#ifdef _WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

char *fpGetCwd(char *path, size_t len)
{
    if (path == NULL || len == 0)
        return NULL;
#ifdef _WIN32
    return _getcwd(path, (int)len);
#else
    return getcwd(path, len);
#endif
}
