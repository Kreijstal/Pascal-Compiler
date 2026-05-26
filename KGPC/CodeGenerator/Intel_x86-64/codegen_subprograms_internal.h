#ifndef CODEGEN_SUBPROGRAMS_INTERNAL_H
#define CODEGEN_SUBPROGRAMS_INTERNAL_H

#include "codegen.h"
#include "codegen_string_set.h"

extern CodeGenStringSet g_emitted_set;
extern ListNode_t *g_emitted_tail;

void ir_liveness_allocate(ListNode_t *inst_list);
int node_is_file_type(HashNode_t *node);
int node_is_class_type(HashNode_t *node);
void codegen_add_class_vars_for_method(const char *owner_class,
                                       const char *method_name_arg,
                                       SymTab_t *symtab, CodeGenContext *ctx);
int codegen_real_param_storage_size(Tree_t *arg_decl,
                                    HashNode_t *resolved_type_node,
                                    KgpcType *cached_arg_type);
int codegen_shortstring_storage_size(KgpcType *type);
void codegen_register_local_types(ListNode_t *type_decls, SymTab_t *symtab);
void codegen_register_const_decls(ListNode_t *const_decls, SymTab_t *symtab);
void codegen_register_decl_list(CodeGenContext *ctx, ListNode_t *decls,
                                SymTab_t *symtab, int is_param);
void codegen_emit_local_const_equivs(CodeGenContext *ctx, SymTab_t *symtab);
void codegen_emit_const_decl_equivs_from_list(CodeGenContext *ctx,
                                              ListNode_t *const_decls);
void codegen_register_owner_unit_scope(CodeGenContext *ctx, SymTab_t *symtab,
                                       int source_unit_index);
KgpcType *codegen_canonical_shortstring_type(void);
void free_inst_list(ListNode_t *inst_list);
void codegen_function_header_ex_alias_vis(char *func_name, CodeGenContext *ctx,
                                          int nostackframe,
                                          const char *cname_override,
                                          int emit_weak);
void codegen_stack_space_for_inst_list(ListNode_t *inst_list,
                                       CodeGenContext *ctx);
void codegen_function_footer_ex(char *func_name, CodeGenContext *ctx,
                                int nostackframe);
void codegen_function_header(char *func_name, CodeGenContext *ctx);
void codegen_function_footer(char *func_name, CodeGenContext *ctx);

int codegen_dynamic_array_descriptor_bytes(int element_size);
int add_absolute_var_alias(const char *alias_label, const char *target_label);
int add_absolute_var_alias_with_offset(const char *alias_label,
                                       const char *target_label,
                                       int field_offset, int alias_size);
int add_absolute_static_symbol_alias(const char *alias_label,
                                     const char *target_symbol, int alias_size);

/* Attempt to emit a typed-const record's storage as static .data so the
 * loader populates the record at program start.  Returns 0 if the record's
 * full layout was emitted (the caller must then skip the .bss/.comm
 * directive for the same storage label).  Sets
 * decl->tree_data.var_decl_data.static_init_emitted on success.
 * `emit_bare_alias` is non-zero when the bare Pascal name should be
 * `.set` to the storage label for cross-unit visibility. */
int codegen_try_emit_typed_const_record_static_alias(
    CodeGenContext *ctx, SymTab_t *symtab, Tree_t *decl, const char *var_name,
    const char *static_label, long long total_size, int emit_bare_alias);

#endif
