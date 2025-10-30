#ifndef UNIT_PATHS_H
#define UNIT_PATHS_H

#include <stdbool.h>

typedef struct UnitSearchPaths {
    char *vendor_dir;
    char *user_dir;
} UnitSearchPaths;

void unit_search_paths_init(UnitSearchPaths *paths);
void unit_search_paths_destroy(UnitSearchPaths *paths);
bool unit_search_paths_set_vendor(UnitSearchPaths *paths, const char *stdlib_path);
bool unit_search_paths_set_user(UnitSearchPaths *paths, const char *input_path);
char *unit_search_paths_resolve(const UnitSearchPaths *paths, const char *unit_name);
char *unit_search_paths_normalize_name(const char *name);

#endif /* UNIT_PATHS_H */
