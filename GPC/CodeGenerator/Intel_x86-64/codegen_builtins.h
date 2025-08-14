#ifndef CODEGEN_BUILTINS_H
#define CODEGEN_BUILTINS_H

#include "codegen.h"

/*
    Builtin-related code generation functions
*/

void escape_string(char *dest, const char *src, size_t dest_size);
ListNode_t *codegen_builtin_write(ListNode_t *, ListNode_t *, CodeGenContext *ctx);
ListNode_t *codegen_builtin_writeln(ListNode_t *, ListNode_t *, CodeGenContext *ctx);
ListNode_t *codegen_builtin_read(ListNode_t *, ListNode_t *, CodeGenContext *ctx);

#endif // CODEGEN_BUILTINS_H
