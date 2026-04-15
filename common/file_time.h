#ifndef KGPC_FILE_TIME_H
#define KGPC_FILE_TIME_H

#include <sys/stat.h>
#include <time.h>

#ifdef _WIN32
#include <direct.h>
#endif

static inline struct timespec kgpc_stat_mtime(const struct stat *st)
{
    struct timespec ts;
    if (st == NULL)
    {
        ts.tv_sec = 0;
        ts.tv_nsec = 0;
        return ts;
    }

#ifdef _WIN32
    ts.tv_sec = st->st_mtime;
    ts.tv_nsec = 0;
#elif defined(__APPLE__) && defined(_DARWIN_FEATURE_64_BIT_INODE)
    ts = st->st_mtimespec;
#else
    ts = st->st_mtim;
#endif
    return ts;
}

static inline int kgpc_mkdir(const char *path, int mode)
{
#ifdef _WIN32
    (void)mode;
    return _mkdir(path);
#else
    return mkdir(path, (mode_t)mode);
#endif
}

#endif /* KGPC_FILE_TIME_H */
