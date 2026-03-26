/*
    Unit name registry - maps unit names to integer indices.
*/
#include "unit_registry.h"
#include "identifier_utils.h"
#include <stdlib.h>
#include <string.h>

#define MAX_UNIT_NAMES 256

static const char *registry[MAX_UNIT_NAMES]; /* index 0 unused (means "no unit") */
static int count = 0;

/* Dependency tracking: deps[i] is a bitmask of unit indices that unit i uses.
 * With MAX_UNIT_NAMES=256, we need 256/64=4 uint64_t per unit. */
#define DEP_WORDS ((MAX_UNIT_NAMES + 63) / 64)
static unsigned long long deps[MAX_UNIT_NAMES][DEP_WORDS];
static unsigned long long iface_deps[MAX_UNIT_NAMES][DEP_WORDS];

int unit_registry_add(const char *name)
{
    if (name == NULL || name[0] == '\0')
        return 0;

    for (int i = 1; i <= count; i++)
    {
        if (pascal_identifier_equals(registry[i], name))
            return i;
    }

    if (count >= MAX_UNIT_NAMES - 1)
        return 0;

    count++;
    registry[count] = strdup(name);
    return count;
}

const char *unit_registry_get(int index)
{
    if (index <= 0 || index > count)
        return NULL;
    return registry[index];
}

int unit_registry_contains(const char *name)
{
    if (name == NULL || name[0] == '\0')
        return 0;
    for (int i = 1; i <= count; i++)
    {
        if (pascal_identifier_equals(registry[i], name))
            return 1;
    }
    return 0;
}

void unit_registry_add_dep(int unit_idx, int dep_idx)
{
    if (unit_idx <= 0 || unit_idx >= MAX_UNIT_NAMES) return;
    if (dep_idx <= 0 || dep_idx >= MAX_UNIT_NAMES) return;
    deps[unit_idx][dep_idx / 64] |= (1ULL << (dep_idx % 64));
}

void unit_registry_add_iface_dep(int unit_idx, int dep_idx)
{
    if (unit_idx <= 0 || unit_idx >= MAX_UNIT_NAMES) return;
    if (dep_idx <= 0 || dep_idx >= MAX_UNIT_NAMES) return;
    iface_deps[unit_idx][dep_idx / 64] |= (1ULL << (dep_idx % 64));
    unit_registry_add_dep(unit_idx, dep_idx);
}

int unit_registry_is_dep(int unit_idx, int dep_idx)
{
    if (unit_idx <= 0 || unit_idx >= MAX_UNIT_NAMES) return 0;
    if (dep_idx <= 0 || dep_idx >= MAX_UNIT_NAMES) return 0;
    return (deps[unit_idx][dep_idx / 64] & (1ULL << (dep_idx % 64))) != 0;
}

int unit_registry_is_iface_dep(int unit_idx, int dep_idx)
{
    if (unit_idx <= 0 || unit_idx >= MAX_UNIT_NAMES) return 0;
    if (dep_idx <= 0 || dep_idx >= MAX_UNIT_NAMES) return 0;
    return (iface_deps[unit_idx][dep_idx / 64] & (1ULL << (dep_idx % 64))) != 0;
}

int unit_registry_count(void)
{
    return count;
}

void unit_registry_reset(void)
{
    for (int i = 1; i <= count; i++)
    {
        free((char *)registry[i]);
        registry[i] = NULL;
    }
    memset(deps, 0, sizeof(deps));
    memset(iface_deps, 0, sizeof(iface_deps));
    count = 0;
}
