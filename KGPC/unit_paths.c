#include "unit_paths.h"
#include "Parser/pascal_frontend.h"

#include <ctype.h>
#include <limits.h>
#include <libgen.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <unistd.h>
#else
#include <io.h>
#ifndef R_OK
#define R_OK 4
#endif
#define access _access
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

static int file_is_readable(const char *path)
{
    return path != NULL && access(path, R_OK) == 0;
}

static char *duplicate_path(const char *path)
{
    if (path == NULL)
        return NULL;

    char *dup = strdup(path);
    if (dup == NULL)
        fprintf(stderr, "Error: Memory allocation failed while duplicating path '%s'\n", path);

    return dup;
}

static char *build_candidate_unit_path(const char *dir, const char *unit_name)
{
    if (dir == NULL || unit_name == NULL)
        return NULL;

    char candidate[PATH_MAX];
    
    /* Try .p extension first (kgpc convention) */
    int written = snprintf(candidate, sizeof(candidate), "%s/%s.p", dir, unit_name);
    if (written > 0 && written < (int)sizeof(candidate) && file_is_readable(candidate))
        return duplicate_path(candidate);
    
    /* Try .pp extension (FPC convention) */
    written = snprintf(candidate, sizeof(candidate), "%s/%s.pp", dir, unit_name);
    if (written > 0 && written < (int)sizeof(candidate) && file_is_readable(candidate))
        return duplicate_path(candidate);
    
    /* Try .pas extension (common convention) */
    written = snprintf(candidate, sizeof(candidate), "%s/%s.pas", dir, unit_name);
    if (written > 0 && written < (int)sizeof(candidate) && file_is_readable(candidate))
        return duplicate_path(candidate);

    return NULL;
}

void unit_search_paths_init(UnitSearchPaths *paths)
{
    if (paths == NULL)
        return;

    paths->vendor_dir = NULL;
    paths->user_dir = NULL;
    paths->unit_path_count = 0;
    paths->disable_vendor_units = false;
    for (int i = 0; i < MAX_UNIT_SEARCH_PATHS; ++i)
        paths->unit_paths[i] = NULL;
}

void unit_search_paths_destroy(UnitSearchPaths *paths)
{
    if (paths == NULL)
        return;

    free(paths->vendor_dir);
    free(paths->user_dir);
    paths->vendor_dir = NULL;
    paths->user_dir = NULL;
    
    for (int i = 0; i < paths->unit_path_count; ++i)
    {
        free(paths->unit_paths[i]);
        paths->unit_paths[i] = NULL;
    }
    paths->unit_path_count = 0;
}

bool unit_search_paths_set_vendor(UnitSearchPaths *paths, const char *stdlib_path)
{
    if (paths == NULL || stdlib_path == NULL)
        return false;

    char *stdlib_copy = strdup(stdlib_path);
    if (stdlib_copy == NULL)
        return false;

    bool success = false;
    char *dir = dirname(stdlib_copy);
    if (dir != NULL)
    {
        char buffer[PATH_MAX];
        int written = snprintf(buffer, sizeof(buffer), "%s/Units", dir);
        if (written > 0 && written < (int)sizeof(buffer))
        {
            char *dup = duplicate_path(buffer);
            if (dup != NULL)
            {
                free(paths->vendor_dir);
                paths->vendor_dir = dup;
                success = true;
            }
        }
    }

    free(stdlib_copy);
    return success;
}

bool unit_search_paths_set_user(UnitSearchPaths *paths, const char *input_path)
{
    if (paths == NULL || input_path == NULL)
        return false;

    char *input_copy = strdup(input_path);
    if (input_copy == NULL)
        return false;

    bool success = false;
    char *dir = dirname(input_copy);
    if (dir != NULL)
    {
        char *dup = duplicate_path(dir);
        if (dup != NULL)
        {
            free(paths->user_dir);
            paths->user_dir = dup;
            success = true;
        }
    }

    free(input_copy);
    return success;
}

bool unit_search_paths_add_unit_path(UnitSearchPaths *paths, const char *path)
{
    if (paths == NULL || path == NULL)
        return false;
    
    if (paths->unit_path_count >= MAX_UNIT_SEARCH_PATHS)
    {
        fprintf(stderr, "Warning: Maximum unit search paths (%d) exceeded\n", MAX_UNIT_SEARCH_PATHS);
        return false;
    }
    
    char *dup = duplicate_path(path);
    if (dup == NULL)
        return false;
    
    paths->unit_paths[paths->unit_path_count++] = dup;
    return true;
}

void unit_search_paths_disable_vendor(UnitSearchPaths *paths)
{
    if (paths != NULL)
        paths->disable_vendor_units = true;
}

char *unit_search_paths_resolve(const UnitSearchPaths *paths, const char *unit_name)
{
    if (paths == NULL || unit_name == NULL)
        return NULL;

    char *path = NULL;

    /* 1. First search in -Fu paths (highest priority for FPC compatibility) */
    for (int i = 0; i < paths->unit_path_count; ++i)
    {
        path = build_candidate_unit_path(paths->unit_paths[i], unit_name);
        if (path != NULL)
            return path;
    }

    /* 2. Search in user directory (same directory as input file) */
    path = build_candidate_unit_path(paths->user_dir, unit_name);
    if (path != NULL)
        return path;

    /* 3. Search in preprocessor include paths (from -I flags) */
    int include_count = 0;
    const char * const *include_paths = pascal_frontend_get_include_paths(&include_count);
    for (int i = 0; i < include_count; ++i)
    {
        path = build_candidate_unit_path(include_paths[i], unit_name);
        if (path != NULL)
            return path;
    }

    /* 4. Search in vendor directories (KGPC built-in units) unless disabled */
    if (!paths->disable_vendor_units)
    {
        path = build_candidate_unit_path(paths->vendor_dir, unit_name);
        if (path != NULL)
            return path;

        path = build_candidate_unit_path("KGPC/Units", unit_name);
        if (path != NULL)
            return path;

        const char *source_root = getenv("MESON_SOURCE_ROOT");
        if (source_root != NULL)
        {
            char buffer[PATH_MAX];
            int written = snprintf(buffer, sizeof(buffer), "%s/KGPC/Units", source_root);
            if (written > 0 && written < (int)sizeof(buffer))
            {
                path = build_candidate_unit_path(buffer, unit_name);
                if (path != NULL)
                    return path;
            }
        }
    }

    /* 5. Search in current directory (fallback) */
    return build_candidate_unit_path(".", unit_name);
}

char *unit_search_paths_normalize_name(const char *name)
{
    if (name == NULL)
        return NULL;

    size_t len = strlen(name);
    char *copy = (char *)malloc(len + 1);
    if (copy == NULL)
        return NULL;

    for (size_t i = 0; i < len; ++i)
        copy[i] = (char)tolower((unsigned char)name[i]);
    copy[len] = '\0';
    return copy;
}
