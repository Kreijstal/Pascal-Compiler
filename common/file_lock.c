#include "file_lock.h"

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>

#ifdef _WIN32
#include <io.h>
#include <windows.h>
#define close _close
#define getpid GetCurrentProcessId
#define open _open
#define unlink _unlink
#define write _write
#else
#include <unistd.h>
#endif

typedef struct HeldLock {
    char *path;
    char *lock_path;
    int count;
    struct HeldLock *next;
} HeldLock;

static HeldLock *g_held_locks = NULL;

static bool is_pid_alive(int pid)
{
    if (pid <= 0)
        return false;
#ifdef _WIN32
    HANDLE h = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, (DWORD)pid);
    if (h == NULL)
        return false;
    DWORD exit_code = 0;
    bool alive = GetExitCodeProcess(h, &exit_code) && exit_code == STILL_ACTIVE;
    CloseHandle(h);
    return alive;
#else
    return kill(pid, 0) == 0 || errno == EPERM;
#endif
}

static bool try_acquire(const char *lock_path)
{
    for (;;)
    {
#ifdef _WIN32
        int fd = open(lock_path, _O_CREAT | _O_EXCL | _O_RDWR | _O_BINARY, 0666);
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
            FILE *f = fopen(lock_path, "r");
            if (f != NULL)
            {
                int pid = 0;
                int parsed = fscanf(f, "%d", &pid);
                fclose(f);
                if ((parsed == 1 && !is_pid_alive(pid)) || parsed != 1)
                {
                    unlink(lock_path);
                    continue;
                }
            }
        }

        return false;
    }
}

bool file_lock_acquire(const char *path, int timeout_secs)
{
    if (path == NULL || path[0] == '\0')
        return false;

    for (HeldLock *held = g_held_locks; held != NULL; held = held->next)
    {
        if (strcmp(held->path, path) == 0)
        {
            held->count++;
            return true;
        }
    }

    size_t lock_len = strlen(path) + 8;
    char *lock_path = malloc(lock_len);
    if (lock_path == NULL)
        return false;
    snprintf(lock_path, lock_len, "%s.lock", path);

#ifdef _WIN32
    DWORD start_ms = GetTickCount();
#else
    struct timespec start_ts;
    clock_gettime(CLOCK_MONOTONIC, &start_ts);
#endif

    long wait_ms = 10;
    while (!try_acquire(lock_path))
    {
        if (timeout_secs > 0)
        {
#ifdef _WIN32
            if (GetTickCount() - start_ms >= (DWORD)timeout_secs * 1000)
#else
            struct timespec now_ts;
            clock_gettime(CLOCK_MONOTONIC, &now_ts);
            long long elapsed_ns =
                ((long long)now_ts.tv_sec - (long long)start_ts.tv_sec) * 1000000000LL +
                ((long long)now_ts.tv_nsec - (long long)start_ts.tv_nsec);
            if (elapsed_ns >= (long long)timeout_secs * 1000000000LL)
#endif
            {
                free(lock_path);
                return false;
            }
        }

#ifdef _WIN32
        Sleep((DWORD)wait_ms);
#else
        struct timespec sleep_ts;
        sleep_ts.tv_sec = wait_ms / 1000;
        sleep_ts.tv_nsec = (wait_ms % 1000) * 1000000L;
        nanosleep(&sleep_ts, NULL);
#endif
        wait_ms *= 2;
        if (wait_ms > 1000)
            wait_ms = 1000;
    }

    HeldLock *held = malloc(sizeof(*held));
    if (held == NULL)
    {
        unlink(lock_path);
        free(lock_path);
        return false;
    }
    held->path = strdup(path);
    if (held->path == NULL)
    {
        unlink(lock_path);
        free(lock_path);
        free(held);
        return false;
    }
    held->lock_path = lock_path;
    held->count = 1;
    held->next = g_held_locks;
    g_held_locks = held;

    return true;
}

void file_lock_release(const char *path)
{
    if (path == NULL || path[0] == '\0')
        return;

    HeldLock *prev = NULL;
    HeldLock *held = g_held_locks;
    while (held != NULL)
    {
        if (strcmp(held->path, path) == 0)
        {
            held->count--;
            if (held->count > 0)
                return;

            if (held->lock_path != NULL)
                unlink(held->lock_path);

            if (prev != NULL)
                prev->next = held->next;
            else
                g_held_locks = held->next;

            free(held->path);
            free(held->lock_path);
            free(held);
            return;
        }
        prev = held;
        held = held->next;
    }
}
