#ifndef CODEGEN_STATEMENT_H
#define CODEGEN_STATEMENT_H

#include "codegen.h"

ListNode_t *codegen_stmt(struct Statement *, ListNode_t *, CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_compound_stmt(struct Statement *, ListNode_t *, CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_var_assignment(struct Statement *, ListNode_t *, CodeGenContext *ctx);
ListNode_t *codegen_proc_call(struct Statement *, ListNode_t *, CodeGenContext *, SymTab_t *symtab);
ListNode_t *codegen_if_then(struct Statement *, ListNode_t *, CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_while(struct Statement *, ListNode_t *, CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_for(struct Statement *, ListNode_t *, CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_builtin_proc(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx);

#endif
