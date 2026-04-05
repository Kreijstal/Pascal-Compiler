#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <stdarg.h>
#include <limits.h>
#include <errno.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
static char* strndup(const char* s, size_t n)
{
    size_t len = strnlen(s, n);
    char* buf = (char*)malloc(len + 1);
    if (buf == NULL)
        return NULL;
    memcpy(buf, s, len);
    buf[len] = '\0';
    return buf;
}
#endif

#include "from_cparser.h"
#include "../../string_intern.h"
#include "../../unit_registry.h"
#include "../../compilation_context.h"
#include "../pascal_frontend.h"

/* Cached getenv() — defined in SemCheck.c */
extern const char *kgpc_getenv(const char *name);

#include "../List/List.h"
#include "tree.h"
#include "tree_types.h"
#include "ident_ref.h"
#include "type_tags.h"
#include "pascal_parser.h"
#include "KgpcType.h"
#include "generic_types.h"
#include "../SemanticCheck/SymTab/SymTab.h"
#include "../../identifier_utils.h"
#include "../pascal_frontend.h"
#include "../ErrVars.h"

#include "from_cparser_parts/from_cparser_part1.inc"
#include "from_cparser_parts/from_cparser_part2.inc"
#include "from_cparser_parts/from_cparser_part3.inc"
#include "from_cparser_parts/from_cparser_part4.inc"
#include "from_cparser_parts/from_cparser_part5.inc"
#include "from_cparser_parts/from_cparser_part6.inc"

