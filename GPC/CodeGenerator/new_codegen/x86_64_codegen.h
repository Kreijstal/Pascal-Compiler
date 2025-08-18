#ifndef X86_64_CODEGEN_H
#define X86_64_CODEGEN_H

#include "../../ir.h"
#include <stdio.h>

void codegen_x86_64(ListNode_t *ir_list, FILE *out);

#endif // X86_64_CODEGEN_H
