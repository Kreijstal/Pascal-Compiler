#ifndef UNIT_PATHS_H
#define UNIT_PATHS_H

#include <stdbool.h>

#define MAX_UNIT_SEARCH_PATHS 64

typedef struct UnitSearchPaths {
    char *vendor_dir;
    char *user_dir;
    char *unit_paths[MAX_UNIT_SEARCH_PATHS];  /* -Fu paths */
    int unit_path_count;
    bool disable_vendor_units;  /* --no-vendor-units flag */
} UnitSearchPaths;

void unit_search_paths_init(UnitSearchPaths *paths);
void unit_search_paths_destroy(UnitSearchPaths *paths);
bool unit_search_paths_set_vendor(UnitSearchPaths *paths, const char *stdlib_path);
bool unit_search_paths_set_user(UnitSearchPaths *paths, const char *input_path);
bool unit_search_paths_add_unit_path(UnitSearchPaths *paths, const char *path);
void unit_search_paths_disable_vendor(UnitSearchPaths *paths);
char *unit_search_paths_resolve(const UnitSearchPaths *paths, const char *unit_name);
char *unit_search_paths_normalize_name(const char *name);

#endif /* UNIT_PATHS_H */
