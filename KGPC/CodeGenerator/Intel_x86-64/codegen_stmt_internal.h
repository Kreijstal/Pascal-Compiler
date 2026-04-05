/*
 * codegen_stmt_internal.h — Internal header for codegen_statement module.
 *
 * This header declares functions shared between the split implementation
 * files (codegen_stmt_*.c) but NOT exposed to external code.
 *
 * External code should only include codegen_statement.h for the public API.
 */

#ifndef CODEGEN_STMT_INTERNAL_H
#define CODEGEN_STMT_INTERNAL_H

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include "register_types.h"
#include "codegen.h"
#include "codegen_statement.h"
#include "stackmng/stackmng.h"
#include "asm_emit.h"
#include "expr_tree/expr_tree.h"
#include "codegen_expression.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/ident_ref.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/KgpcType.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../identifier_utils.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_expr.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_stmt.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_sizeof.h"
#include "../../Parser/ParseTree/from_cparser.h"

/* Shared macro - pointer size for x86-64 codegen */
#ifndef CODEGEN_POINTER_SIZE_BYTES
#define CODEGEN_POINTER_SIZE_BYTES 8
#endif

/* Forward declarations from expr_tree module */
struct KgpcType *expr_get_kgpc_type(const struct Expression *expr);

/* ===================================================================
 * Cross-module function declarations
 *
 * These functions are implemented in one codegen_stmt_*.c file but
 * called from another. They are NOT part of the public API.
 * =================================================================== */

/* --- From codegen_stmt_assignment.c --- */

int codegen_array_access_targets_shortstring(const struct Expression *expr, CodeGenContext *ctx);
ListNode_t *codegen_call_char_array_to_shortstring(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg, int src_len, int dest_size);
ListNode_t *codegen_call_shortstring_copy(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *dest_reg, int dest_size, Register_t *src_reg);
ListNode_t *codegen_call_shortstring_to_char_array(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg, int array_size);
ListNode_t *codegen_call_string_assign(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg);
ListNode_t *codegen_call_string_assign_from_char_array(ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *addr_reg, Register_t *value_reg, int src_len);
ListNode_t *codegen_call_string_assign_func(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg, const char *func_name);
ListNode_t *codegen_call_string_to_char_array(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg, int array_size);
ListNode_t *codegen_call_string_to_shortstring(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg, int array_size);
int codegen_expr_is_mp_integer(struct Expression *expr);
int codegen_expr_is_shortstring_array(const struct Expression *expr);
int codegen_expr_is_shortstring_rhs(const struct Expression *expr, CodeGenContext *ctx);
int codegen_expr_is_shortstring_value_local(const struct Expression *expr);
int codegen_expr_is_wide_string_value(const struct Expression *expr);
int codegen_get_char_array_bounds(const struct Expression *expr, CodeGenContext *ctx,
    int *lower_out, int *upper_out, int *is_shortstring_out);
int codegen_get_shortstring_capacity(const struct Expression *expr, CodeGenContext *ctx);
int codegen_is_current_return_var_id(const struct Expression *expr, CodeGenContext *ctx);
int codegen_shortstring_capacity_from_type_local(KgpcType *type);
int record_type_is_mp_integer(const struct RecordType *record_type);

/* --- From codegen_stmt_calls_and_control.c --- */

ListNode_t *codegen_continue_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
int codegen_expr_is_extended_storage(const struct Expression *expr);
ListNode_t *codegen_for_in(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_inherited(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_on_exception(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_raise(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_try_except(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_try_finally(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_with(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);

/* --- From codegen_stmt_dispatch.c --- */

int codegen_current_except_finally_depth(const CodeGenContext *ctx);
const char *codegen_current_except_label(const CodeGenContext *ctx);
const char *codegen_current_loop_continue(const CodeGenContext *ctx);
const char *codegen_current_loop_exit(const CodeGenContext *ctx);
int codegen_current_loop_finally_depth(const CodeGenContext *ctx);
int codegen_dynamic_array_element_size(CodeGenContext *ctx, StackNode_t *array_node,
    struct Expression *array_expr);
int codegen_get_current_return_shortstring_capacity(CodeGenContext *ctx, SymTab_t *symtab);
int codegen_has_finally(const CodeGenContext *ctx);
void codegen_pop_except(CodeGenContext *ctx);
void codegen_pop_finally(CodeGenContext *ctx);
void codegen_pop_loop(CodeGenContext *ctx);
int codegen_push_except(CodeGenContext *ctx, const char *label);
int codegen_push_finally(CodeGenContext *ctx, ListNode_t *statements);
int codegen_push_loop(CodeGenContext *ctx, const char *exit_label, const char *continue_label);
struct RecordType *codegen_resolve_with_record_type(struct Expression *context_expr,
    SymTab_t *symtab);
void codegen_with_pop(CodeGenContext *ctx);

/* --- From codegen_stmt_infrastructure.c --- */

StackNode_t *codegen_alloc_incdec_temp(int size);
StackNode_t *codegen_alloc_temp_slot(const char *prefix);
RegisterId_t codegen_arg_reg_id_num(int num);
ListNode_t *codegen_assign_dynamic_array(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx);
ListNode_t *codegen_assign_extended_value(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx);
ListNode_t *codegen_assign_record_value(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx);
ListNode_t *codegen_assign_static_array(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx);
ListNode_t *codegen_branch_through_finally(CodeGenContext *ctx, ListNode_t *inst_list,
    SymTab_t *symtab, const char *target_label, int limit_depth);
ListNode_t *codegen_break_stmt(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_builtin_delete(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
ListNode_t *codegen_builtin_insert(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
ListNode_t *codegen_builtin_prefetch(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
ListNode_t *codegen_builtin_setlength_shortstring(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx);
ListNode_t *codegen_builtin_setlength_string(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx);
ListNode_t *codegen_builtin_setlength_unicodestring(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx);
ListNode_t *codegen_builtin_setstring(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx);
ListNode_t *codegen_builtin_str(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
ListNode_t *codegen_builtin_val(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
ListNode_t *codegen_builtin_writestr(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx);
ListNode_t *codegen_call_mpint_assign(ListNode_t *inst_list, Register_t *addr_reg,
    Register_t *value_reg);
ListNode_t *codegen_emit_cmp_spill_immediate(ListNode_t *inst_list,
    CodeGenContext *ctx, int compare_as_qword, long long imm_value,
    int spill_offset);
ListNode_t *codegen_emit_finally_block(CodeGenContext *ctx, ListNode_t *inst_list,
    SymTab_t *symtab, const char *entry_label, const char *target_label);
ListNode_t *codegen_emit_new_dispose_method_fallback(struct Statement *stmt,
    ListNode_t *inst_list, CodeGenContext *ctx, struct Expression *target_expr,
    struct Expression *method_expr);
int codegen_enum_type_literal_count(KgpcType *type);
ListNode_t *codegen_evaluate_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg);
int codegen_expr_is_string_like(const struct Expression *expr);
ListNode_t *codegen_fail_register(CodeGenContext *ctx, ListNode_t *inst_list,
    Register_t **out_reg, const char *message);
HashNode_t *codegen_find_zero_arg_method_node(SymTab_t *symtab,
    const struct RecordType *record, const char *method_name);
void codegen_get_current_return_info(CodeGenContext *ctx, SymTab_t *symtab,
    int *out_is_real, int *out_size);
void codegen_get_current_return_slot_info(CodeGenContext *ctx,
    int *out_is_real, int *out_size);
int codegen_get_enumerator_current_info(SymTab_t *symtab, struct RecordType *enum_record,
    HashNode_t **out_current_node, KgpcType **out_current_type);
void codegen_hydrate_array_literal_from_lhs(struct Expression *lhs_expr,
    struct Expression *rhs_expr, CodeGenContext *ctx);
struct RecordField *codegen_lookup_record_field(struct Expression *record_access_expr);
struct RecordField *codegen_lookup_record_field_expr(struct Expression *record_access_expr,
    CodeGenContext *ctx);
struct TypeAlias *codegen_lookup_type_alias(CodeGenContext *ctx, const char *type_id);
ListNode_t *codegen_maybe_convert_int_like_to_real(int target_type,
    struct Expression *source_expr, Register_t *value_reg, ListNode_t *inst_list,
    int *coerced_to_real);
ListNode_t *codegen_promote_char_reg_to_string(ListNode_t *inst_list, Register_t *value_reg);
long long codegen_record_field_effective_size(struct Expression *expr, CodeGenContext *ctx);
ListNode_t *codegen_restore_call_arg_regs_stmt(ListNode_t *inst_list,
    const int *int_offsets, const int *xmm_offsets);
int codegen_set_iteration_upper_bound(CodeGenContext *ctx, KgpcType *set_type);
ListNode_t *codegen_spill_call_arg_regs_stmt(ListNode_t *inst_list,
    int *int_offsets, int *xmm_offsets);
ListNode_t *codegen_statement_list(ListNode_t *stmts, ListNode_t *inst_list,
    CodeGenContext *ctx, SymTab_t *symtab);
int codegen_statement_return_storage_size(KgpcType *return_type);
ListNode_t *codegen_store_exception_value(ListNode_t *inst_list,
    CodeGenContext *ctx, struct Expression *exc_expr, Register_t *value_reg);
int codegen_with_push(CodeGenContext *ctx, struct Expression *context_expr,
    struct RecordType *record_type);
int expr_integer_store_size(const struct Expression *expr);
int expr_is_dynamic_array(const struct Expression *expr);
int expr_is_static_array_like(const struct Expression *expr, CodeGenContext *ctx);
int expr_is_unsigned_type(const struct Expression *expr);
int expr_value_requires_64bit(const struct Expression *expr, CodeGenContext *ctx);
void format_pascal_label(char *buffer, size_t size, const CodeGenContext *ctx, const char *label_text);
int is_single_float_type(int type_tag, long long storage_size);
int lookup_record_field_type(struct RecordType *record_type, const char *field_name);
const char *register_name8(const Register_t *reg);

/*
 * get_record_type_from_node — extract RecordType from a HashNode.
 * Defined as static inline here because an identically-named function
 * exists in other compilation units (SemCheck_Expr_Record.c, codegen.c).
 */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    return hashnode_get_record_type(node);
}

/* ===================================================================
 * Cross-module variable declarations
 * =================================================================== */


#endif /* CODEGEN_STMT_INTERNAL_H */
