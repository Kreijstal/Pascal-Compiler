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

void unit_registry_reset(void)
{
    for (int i = 1; i <= count; i++)
    {
        free((char *)registry[i]);
        registry[i] = NULL;
    }
    count = 0;
}
