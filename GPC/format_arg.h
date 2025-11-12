#ifndef GPC_FORMAT_ARG_H
#define GPC_FORMAT_ARG_H

#include <stdint.h>

typedef enum gpc_tvar_kind
{
    GPC_TVAR_KIND_INT = 0,
    GPC_TVAR_KIND_BOOL = 1,
    GPC_TVAR_KIND_CHAR = 2,
    GPC_TVAR_KIND_REAL = 3,
    GPC_TVAR_KIND_STRING = 4,
    GPC_TVAR_KIND_POINTER = 5
} gpc_tvar_kind_t;

typedef struct gpc_tvarrec
{
    int32_t kind;
    int32_t reserved;
    union
    {
        int64_t v_int;
        double v_real;
        void *v_ptr;
    } data;
} gpc_tvarrec;

#endif /* GPC_FORMAT_ARG_H */

