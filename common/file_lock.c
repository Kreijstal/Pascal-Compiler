#include "file_lock.h"

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <windows.h>
#include <io.h>
#define open _open
#define read _read
#define write _write
#define close _close
#define unlink _unlink
#define getpid GetCurrentProcessId
#else
#include <unistd.h>
#endif

typedef struct HeldLock {
    char *path;
    int count;
    struct HeldLock *next;
} HeldLock;

static HeldLock *g_held_locks = NULL;

static bool is_pid_alive(int pid)
{
    if (pid <= 0) return false;
#ifdef _WIN32
    HANDLE h = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, (DWORD)pid);
    if (h != NULL)
    {
        DWORD exitCode;
        if (GetExitCodeProcess(h, &exitCode) && exitCode == STILL_ACTIVE)
        {
            CloseHandle(h);
            return true;
        }
        CloseHandle(h);
    }
    return false;
#else
    return kill(pid, 0) == 0 || errno == EPERM;
#endif
}

static bool try_acquire(const char *lock_path)
{
#ifdef _WIN32
    int fd = open(lock_path, _O_CREAT | _O_EXCL | _O_RDWR | _O_BINARY, _S_IREAD | _S_IWRITE);
#else
    int fd = open(lock_path, O_CREAT | O_EXCL | O_RDWR, 0666);
#endif
    if (fd >= 0)
    {
        char pid_buf[32];
        int len = snprintf(pid_buf, sizeof(pid_buf), "%d\n", (int)getpid());
        if (write(fd, pid_buf, (size_t)len) != len)
        {
            close(fd);
            unlink(lock_path);
            return false;
        }
        close(fd);
        return true;
    }

    if (errno == EEXIST)
    {
        /* Check if the existing lock is stale */
        FILE *f = fopen(lock_path, "r");
        if (f != NULL)
        {
            int pid = 0;
            if (fscanf(f, "%d", &pid) == 1)
            {
                fclose(f);
                if (!is_pid_alive(pid))
                {
                    /* Lock is stale, remove it and try once more */
                    unlink(lock_path);
                    return try_acquire(lock_path);
                }
            }
            else
            {
                fclose(f);
                /* Lock file is empty or corrupted, assume stale */
                unlink(lock_path);
                return try_acquire(lock_path);
            }
        }
    }

    return false;
}

bool file_lock_acquire(const char *path, int timeout_secs)
{
    if (path == NULL || path[0] == '\0')
        return false;

    /* Check if we already hold this lock in this process (re-entrancy) */
    for (HeldLock *hl = g_held_locks; hl != NULL; hl = hl->next)
    {
        if (strcmp(hl->path, path) == 0)
        {
            hl->count++;
            return true;
        }
    }

    size_t lock_len = strlen(path) + 8;
    char *lock_path = (char *)malloc(lock_len);
    if (lock_path == NULL)
        return false;
    snprintf(lock_path, lock_len, "%s.lock", path);

#ifdef _WIN32
    DWORD start_ms = GetTickCount();
#else
    long long start_ns = 0;
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0)
        start_ns = (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;
#endif

    long wait_ms = 10;
    while (!try_acquire(lock_path))
    {
        if (timeout_secs > 0)
        {
#ifdef _WIN32
            DWORD now_ms = GetTickCount();
            if (now_ms - start_ms >= (DWORD)timeout_secs * 1000)
            {
                free(lock_path);
                return false;
            }
#else
            long long now_ns = 0;
            if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0)
                now_ns = (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;

            if (now_ns - start_ns >= (long long)timeout_secs * 1000000000LL)
            {
                free(lock_path);
                return false;
            }
#endif
        }

        /* Exponential backoff */
#ifdef _WIN32
        Sleep(wait_ms);
#else
        struct timespec sleep_ts;
        sleep_ts.tv_sec = wait_ms / 1000;
        sleep_ts.tv_nsec = (wait_ms % 1000) * 1000000L;
        nanosleep(&sleep_ts, NULL);
#endif

        wait_ms *= 2;
        if (wait_ms > 1000) wait_ms = 1000;
    }

    free(lock_path);

    /* Record that we now hold this lock */
    HeldLock *hl = (HeldLock *)malloc(sizeof(HeldLock));
    if (hl != NULL)
    {
        hl->path = strdup(path);
        hl->count = 1;
        hl->next = g_held_locks;
        g_held_locks = hl;
    }

    return true;
}

void file_lock_release(const char *path)
{
    if (path == NULL || path[0] == '\0')
        return;

    HeldLock *prev = NULL;
    HeldLock *hl = g_held_locks;
    while (hl != NULL)
    {
        if (strcmp(hl->path, path) == 0)
        {
            hl->count--;
            if (hl->count > 0)
                return; /* Still held (nested) */

            /* Truly release it */
            size_t lock_len = strlen(path) + 8;
            char *lock_path = (char *)malloc(lock_len);
            if (lock_path != NULL)
            {
                snprintf(lock_path, lock_len, "%s.lock", path);
                unlink(lock_path);
                free(lock_path);
            }

            if (prev != NULL)
                prev->next = hl->next;
            else
                g_held_locks = hl->next;

            free(hl->path);
            free(hl);
            return;
        }
        prev = hl;
        hl = hl->next;
    }
}
