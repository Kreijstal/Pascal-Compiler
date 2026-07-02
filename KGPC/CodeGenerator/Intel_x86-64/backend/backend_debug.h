/*
 * backend_debug.h — backend debug-tracing macro.
 *
 * Extracted verbatim from codegen.h so the standalone backend emission core
 * (backend_emit.c, stackmng.c, …) can use CODEGEN_DEBUG without pulling in the
 * front-end-coupled codegen.h.  codegen.h includes this header, so the macro's
 * definition is shared (single source of truth).
 */
#ifndef KGPC_BACKEND_DEBUG_H
#define KGPC_BACKEND_DEBUG_H

#include <stdio.h>

#ifdef KGPC_DEBUG_CODEGEN
#define DEBUG_CODEGEN
#endif
#ifdef DEBUG_CODEGEN
#define CODEGEN_DEBUG(...) fprintf(stderr, __VA_ARGS__)
#else
#define CODEGEN_DEBUG(...) ((void)0)
#endif

#endif /* KGPC_BACKEND_DEBUG_H */
