/*
    String interning pool - deduplicates strings so identical values share
    one canonical pointer. Case-insensitive dedup, preserves first-seen case.
*/
#include "string_intern.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define INTERN_TABLE_SIZE 512

typedef struct InternEntry {
    char *str;
    struct InternEntry *next;
} InternEntry;

static InternEntry *table[INTERN_TABLE_SIZE];

static unsigned int intern_hash(const char *s)
{
    unsigned int h = 2166136261u;
    for (; *s; s++)
        h = (h ^ (unsigned char)tolower((unsigned char)*s)) * 16777619u;
    return h;
}

const char *string_intern(const char *str)
{
    if (str == NULL)
        return NULL;

    unsigned int h = intern_hash(str) % INTERN_TABLE_SIZE;
    for (InternEntry *e = table[h]; e != NULL; e = e->next)
    {
        if (strcasecmp(e->str, str) == 0)
            return e->str;
    }

    InternEntry *e = (InternEntry *)malloc(sizeof(InternEntry));
    if (e == NULL)
        return NULL;
    e->str = strdup(str);
    if (e->str == NULL)
    {
        free(e);
        return NULL;
    }
    e->next = table[h];
    table[h] = e;
    return e->str;
}

void string_intern_reset(void)
{
    for (int i = 0; i < INTERN_TABLE_SIZE; i++)
    {
        InternEntry *e = table[i];
        while (e != NULL)
        {
            InternEntry *next = e->next;
            free(e->str);
            free(e);
            e = next;
        }
        table[i] = NULL;
    }
}
