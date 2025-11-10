#include "runtime_internal.h"

#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifndef _WIN32
#include <netdb.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <unistd.h>

/* Define W_EXITCODE and W_STOPCODE if not available */
#ifndef W_EXITCODE
#define W_EXITCODE(ret, sig) ((ret) << 8 | (sig))
#endif

#ifndef W_STOPCODE
#define W_STOPCODE(sig) ((sig) << 8 | 0x7f)
#endif
#endif

int gpc_unix_get_hostname(char *buffer, size_t buffer_size)
{
    if (buffer == NULL || buffer_size == 0)
        return -1;
#ifdef _WIN32
    (void)buffer;
    (void)buffer_size;
    return -1;
#else
    if (gethostname(buffer, buffer_size) != 0)
        return -1;
    buffer[buffer_size - 1] = '\0';
    return 0;
#endif
}

int gpc_unix_get_domainname(char *buffer, size_t buffer_size)
{
    if (buffer == NULL || buffer_size == 0)
        return -1;
#ifdef _WIN32
    (void)buffer;
    (void)buffer_size;
    return -1;
#else
    memset(buffer, 0, buffer_size);
#if defined(GPC_HAVE_GETDOMAINNAME)
    if (getdomainname(buffer, buffer_size) == 0)
    {
        buffer[buffer_size - 1] = '\0';
        if (buffer[0] != '\0' && strcmp(buffer, "(none)") != 0)
            return 0;
    }
#endif
    struct utsname uts;
    if (uname(&uts) == 0)
    {
        if (uts.nodename[0] != '\0')
        {
            char temp[256];
            strncpy(temp, uts.nodename, sizeof(temp) - 1);
            temp[sizeof(temp) - 1] = '\0';
            char *dot = strchr(temp, '.');
            if (dot != NULL && *(dot + 1) != '\0')
            {
                strncpy(buffer, dot + 1, buffer_size - 1);
                buffer[buffer_size - 1] = '\0';
                return 0;
            }
        }
    }
    char hostname[256];
    if (gethostname(hostname, sizeof(hostname)) == 0)
    {
        char *dot = strchr(hostname, '.');
        if (dot != NULL && *(dot + 1) != '\0')
        {
            strncpy(buffer, dot + 1, buffer_size - 1);
            buffer[buffer_size - 1] = '\0';
            return 0;
        }
        /* Try DNS lookup for FQDN */
        struct addrinfo hints, *info = NULL;
        memset(&hints, 0, sizeof(hints));
        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_flags = AI_CANONNAME;

        if (getaddrinfo(hostname, NULL, &hints, &info) == 0)
        {
            if (info != NULL && info->ai_canonname != NULL)
            {
                dot = strchr(info->ai_canonname, '.');
                if (dot != NULL && *(dot + 1) != '\0')
                {
                    strncpy(buffer, dot + 1, buffer_size - 1);
                    buffer[buffer_size - 1] = '\0';
                    freeaddrinfo(info);
                    return 0;
                }
            }
            if (info != NULL)
                freeaddrinfo(info);
        }
    }
    return -1;
#endif
}

int gpc_unix_wait_process(int pid)
{
#ifdef _WIN32
    (void)pid;
    return -1;
#else
    int status = 0;
    pid_t waited = 0;
    do
    {
        waited = waitpid((pid_t)pid, &status, 0);
    } while (waited == -1 && errno == EINTR);

    if (waited == -1)
        return -1;
    if (WIFEXITED(status))
        return WEXITSTATUS(status);
    return -status;
#endif
}

int gpc_unix_w_exitcode(int return_code, int signal_code)
{
#ifdef _WIN32
    (void)return_code;
    (void)signal_code;
    return -1;
#else
    return W_EXITCODE(return_code, signal_code);
#endif
}

int gpc_unix_w_stopcode(int signal_code)
{
#ifdef _WIN32
    (void)signal_code;
    return -1;
#else
    return W_STOPCODE(signal_code);
#endif
}

int gpc_unix_wifstopped(int status)
{
#ifdef _WIN32
    (void)status;
    return 0;
#else
    return WIFSTOPPED(status) ? 1 : 0;
#endif
}

char *gpc_unix_get_hostname_string(void)
{
    char buffer[256];
    if (gpc_unix_get_hostname(buffer, sizeof(buffer)) != 0)
        return gpc_alloc_empty_string();
    return gpc_string_duplicate(buffer);
}

char *gpc_unix_get_domainname_string(void)
{
    char buffer[256];
    if (gpc_unix_get_domainname(buffer, sizeof(buffer)) != 0)
        return gpc_alloc_empty_string();
    return gpc_string_duplicate(buffer);
}
