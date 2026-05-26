#ifndef CODEGEN_EXPR_SIZEOF_H
#define CODEGEN_EXPR_SIZEOF_H

#include "codegen.h"

int codegen_sizeof_record(CodeGenContext *ctx, struct RecordType *record,
                          long long *size_out, int depth);
int codegen_sizeof_record_type(CodeGenContext *ctx, struct RecordType *record,
                               long long *size_out);
int codegen_sizeof_alias(CodeGenContext *ctx, struct TypeAlias *alias,
                         long long *size_out, int depth);
int codegen_sizeof_hashnode(CodeGenContext *ctx, HashNode_t *node,
                            long long *size_out, int depth);
int codegen_get_record_size(CodeGenContext *ctx, struct Expression *expr,
                            long long *size_out);
int codegen_sizeof_pointer_target(CodeGenContext *ctx,
                                  struct Expression *pointer_expr,
                                  long long *size_out);
long long codegen_record_field_effective_size(struct Expression *expr,
                                              CodeGenContext *ctx);

#endif /* CODEGEN_EXPR_SIZEOF_H */
