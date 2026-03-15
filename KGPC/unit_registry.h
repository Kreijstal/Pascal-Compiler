/*
    Unit name registry - maps unit names to integer indices.
    Index 0 means "no unit" (local/unknown).
    Indices 1..N are registered unit names.
*/
#ifndef KGPC_UNIT_REGISTRY_H
#define KGPC_UNIT_REGISTRY_H

/* Register a unit name and return its index (1-based). Returns existing index if already registered.
 * Returns 0 on failure (NULL name or registry full). */
int unit_registry_add(const char *name);

/* Look up a unit name by index. Returns NULL for index 0 or out-of-range. */
const char *unit_registry_get(int index);

/* Check if a unit name exists in the registry (case-insensitive). */
int unit_registry_contains(const char *name);

/* Record that unit_idx uses (depends on) dep_idx. */
void unit_registry_add_dep(int unit_idx, int dep_idx);

/* Check if dep_idx is a direct dependency of unit_idx. */
int unit_registry_is_dep(int unit_idx, int dep_idx);

/* Reset the registry (call between compilations if needed). */
void unit_registry_reset(void);

#endif /* KGPC_UNIT_REGISTRY_H */
