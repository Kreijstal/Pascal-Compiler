/**
 * @file unit_paths.h
 * @brief Pascal unit search-path resolution.
 *
 * Mirrors the FPC `-Fu` flag plus a couple of KGPC-specific roots:
 *   - the "vendor" directory (bundled stdlib units),
 *   - the user input file's directory (implicit sibling lookup),
 *   - any number of explicit `-Fu` paths.
 *
 * @ref unit_search_paths_resolve walks the list in priority order
 * (`-Fu` first, then user dir, then vendor) and returns the first match.
 */
#ifndef UNIT_PATHS_H
#define UNIT_PATHS_H

#include <stdbool.h>

/** @brief Hard upper bound on the number of `-Fu` search paths. */
#define MAX_UNIT_SEARCH_PATHS 64

/** @brief Collection of unit search roots used by @ref unit_search_paths_resolve. */
typedef struct UnitSearchPaths {
  char *vendor_dir;                              /**< Bundled-stdlib root (owned). */
  char *user_dir;                                /**< Input file's directory (owned). */
  char *unit_paths[MAX_UNIT_SEARCH_PATHS];       /**< Explicit `-Fu` paths (owned). */
  int unit_path_count;                           /**< Entries used in @c unit_paths. */
  bool disable_vendor_units;                     /**< `--no-vendor-units` flag. */
} UnitSearchPaths;

/** @brief Zero-initialise @p paths.  Allocates nothing. */
void unit_search_paths_init(UnitSearchPaths *paths);

/** @brief Free every owned string and reset @p paths to zero state. */
void unit_search_paths_destroy(UnitSearchPaths *paths);

/**
 * @brief Set the vendor directory.
 *
 * @p stdlib_path is the path to the bundled stdlib (typically
 * `<install>/share/kgpc/lib`).  Returns true on success.
 */
bool unit_search_paths_set_vendor(UnitSearchPaths *paths,
                                  const char *stdlib_path);

/**
 * @brief Set the user directory from the input file.
 *
 * Strips the basename of @p input_path and stores the directory.
 * Returns true on success.
 */
bool unit_search_paths_set_user(UnitSearchPaths *paths, const char *input_path);

/**
 * @brief Append @p path to the `-Fu` list.
 *
 * Returns false on capacity overflow (`MAX_UNIT_SEARCH_PATHS`) or
 * allocation failure.
 */
bool unit_search_paths_add_unit_path(UnitSearchPaths *paths, const char *path);

/** @brief Mark the vendor root as disabled (e.g. for `--no-vendor-units`). */
void unit_search_paths_disable_vendor(UnitSearchPaths *paths);

/**
 * @brief Resolve a unit name to a source path.
 *
 * Searches `-Fu` paths first, then the user dir, then vendor (unless
 * disabled).  Returns a freshly malloc'd string on success; NULL if
 * the unit could not be found.
 */
char *unit_search_paths_resolve(const UnitSearchPaths *paths,
                                const char *unit_name);

/**
 * @brief Lower-case and strip the `.pas`/`.pp` extension from @p name.
 *
 * Returns a freshly malloc'd canonical form for hash-table keys.
 */
char *unit_search_paths_normalize_name(const char *name);

#endif /* UNIT_PATHS_H */
