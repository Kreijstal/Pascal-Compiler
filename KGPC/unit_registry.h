/**
 * @file unit_registry.h
 * @brief Unit-name registry: maps Pascal unit names to integer indices.
 *
 * Index 0 means "no unit" (local / unknown).  Indices `1..N` identify
 * registered unit names.  The registry also records inter-unit
 * dependency edges so cycles and interface dependencies can be queried.
 */
#ifndef KGPC_UNIT_REGISTRY_H
#define KGPC_UNIT_REGISTRY_H

/**
 * @brief Register a unit name and return its 1-based index.
 *
 * Returns the existing index if @p name is already registered.
 * Returns 0 on failure (NULL @p name or registry full).
 */
int unit_registry_add(const char *name);

/** @brief Look up a unit name by index; NULL for index 0 or out-of-range. */
const char *unit_registry_get(int index);

/** @brief Returns the 1-based index of @p name, or 0 if not registered (case-insensitive). */
int unit_registry_contains(const char *name);

/** @brief Record that unit @p unit_idx uses (depends on) unit @p dep_idx. */
void unit_registry_add_dep(int unit_idx, int dep_idx);

/**
 * @brief Record an interface (public) dependency from @p unit_idx on @p dep_idx.
 *
 * Also records as a general dependency (calls @ref unit_registry_add_dep
 * internally).
 */
void unit_registry_add_iface_dep(int unit_idx, int dep_idx);

/** @brief Returns non-zero iff @p dep_idx is a direct dependency of @p unit_idx. */
int unit_registry_is_dep(int unit_idx, int dep_idx);

/** @brief Returns non-zero iff @p dep_idx is an interface dependency of @p unit_idx. */
int unit_registry_is_iface_dep(int unit_idx, int dep_idx);

/** @brief Number of registered units; indices `1..count` are valid. */
int unit_registry_count(void);

/** @brief Reset the registry (call between compilations if needed). */
void unit_registry_reset(void);

#endif /* KGPC_UNIT_REGISTRY_H */
