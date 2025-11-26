#ifndef KGPC_FORMAT_ARG_H
#define KGPC_FORMAT_ARG_H

#include <stdint.h>

typedef enum kgpc_tvar_kind
{
    KGPC_TVAR_KIND_INT = 0,
    KGPC_TVAR_KIND_BOOL = 1,
    KGPC_TVAR_KIND_CHAR = 2,
    KGPC_TVAR_KIND_REAL = 3,
    KGPC_TVAR_KIND_STRING = 4,
    KGPC_TVAR_KIND_POINTER = 5
} kgpc_tvar_kind_t;

typedef struct kgpc_tvarrec
{
    int32_t kind;
    int32_t reserved;
    union
    {
        int64_t v_int;
        double v_real;
        void *v_ptr;
    } data;
} kgpc_tvarrec;

#endif /* KGPC_FORMAT_ARG_H */

