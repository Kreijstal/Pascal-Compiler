/*
    Damon Gwinn
    Code generation for expressions
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>

#include "codegen_expression.h"
#include "register_types.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../Parser/ParseTree/KgpcType.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../../identifier_utils.h"
#include "../../format_arg.h"

#define CODEGEN_POINTER_SIZE_BYTES 8
#define CODEGEN_SIZEOF_RECURSION_LIMIT 32

/* Helper functions for transitioning from legacy type fields to KgpcType */

/* Helper function to check if a node is a record type */
static inline int node_is_record_type(HashNode_t *node)
{
    return hashnode_is_record(node);
}

/* Helper function to get RecordType from HashNode */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    return hashnode_get_record_type(node);
}

static int codegen_self_param_is_class(Tree_t *formal_arg_decl, CodeGenContext *ctx)
{
    if (formal_arg_decl == NULL || formal_arg_decl->type != TREE_VAR_DECL)
        return 0;

    KgpcType *type = formal_arg_decl->tree_data.var_decl_data.cached_kgpc_type;
    const char *type_id = formal_arg_decl->tree_data.var_decl_data.type_id;
    if (type == NULL && ctx != NULL && ctx->symtab != NULL && type_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, ctx->symtab, type_id) == 0 &&
            type_node != NULL && type_node->type != NULL)
            type = type_node->type;
    }

    if (type == NULL)
        return 0;

    if (kgpc_type_is_pointer(type) &&
        type->info.points_to != NULL &&
        type->info.points_to->kind == TYPE_KIND_RECORD &&
        type->info.points_to->info.record_info != NULL)
        return record_type_is_class(type->info.points_to->info.record_info);

    if (type->kind == TYPE_KIND_RECORD && type->info.record_info != NULL)
        return record_type_is_class(type->info.record_info);

    return 0;
}

static StackNode_t *codegen_alloc_temp_bytes(const char *prefix, int size);
static const char *codegen_register_name8(const Register_t *reg);
const char *codegen_register_name16(const Register_t *reg);
static ListNode_t *codegen_store_value_to_stack(ListNode_t *inst_list, Register_t *value_reg,
    int offset, int element_size);
static ListNode_t *codegen_materialize_array_literal(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg);
static ListNode_t *codegen_materialize_array_of_const(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg);
static ListNode_t *codegen_set_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg);
static int formal_decl_is_open_array(Tree_t *decl);
static long long codegen_static_array_length(const struct Expression *expr);
static Register_t *codegen_try_get_reg(ListNode_t **inst_list, CodeGenContext *ctx, const char *usage);

typedef struct ArgInfo
{
    Register_t *reg;
    StackNode_t *spill;
    struct Expression *expr;
    int expected_type;
    int is_pointer_like;
    int assigned_class;
    int assigned_index;
    int pass_via_stack;
    int stack_slot;
    int stack_offset;
} ArgInfo;

static void arginfo_register_spill_handler(Register_t *reg, StackNode_t *spill_slot, void *context)
{
    ArgInfo *info = (ArgInfo *)context;
    if (info == NULL || spill_slot == NULL)
        return;
    info->reg = NULL;
    info->spill = spill_slot;
}

static void arginfo_assign_register(ArgInfo *info, Register_t *reg, struct Expression *expr)
{
    if (info == NULL)
        return;
    info->reg = reg;
    info->spill = NULL;
    info->expr = expr;
    if (reg != NULL)
        register_set_spill_callback(reg, arginfo_register_spill_handler, info);
}

static const char *codegen_class_typeinfo_label(struct RecordType *record,
    const char *fallback_id)
{
    if (record != NULL && record->type_id != NULL)
        return record->type_id;
    return fallback_id;
}

static ListNode_t *codegen_load_class_typeinfo(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg)
{
    if (out_reg != NULL)
        *out_reg = NULL;

    if (expr == NULL || ctx == NULL)
        return inst_list;

    if (!codegen_expr_is_addressable(expr))
    {
        codegen_report_error(ctx,
            "ERROR: RTTI operations currently require addressable class expressions.");
        return inst_list;
    }

    /* For class variables (which are pointers), we need to:
     * 1. Load the pointer value from the variable
     * 2. Dereference the pointer to get the typeinfo (first field of the instance)
     *
     * For non-class types, we only need:
     * 1. Get address and dereference to get typeinfo
     */
    int is_class_var = (expr->record_type != NULL && record_type_is_class(expr->record_type));

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    Register_t *typeinfo_reg = codegen_try_get_reg(&inst_list, ctx, "class RTTI");
    if (typeinfo_reg == NULL)
    {
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    char buffer[128];
    if (is_class_var)
    {
        /* Class variables are pointers: first load the pointer, then dereference it */
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, typeinfo_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        /* Now typeinfo_reg contains the pointer to the instance, dereference to get VMT */
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", typeinfo_reg->bit_64, typeinfo_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        /* Now typeinfo_reg contains the VMT pointer, dereference to get TypeInfo (offset 0) */
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", typeinfo_reg->bit_64, typeinfo_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        /* Non-class: addr_reg already points to the instance, just load typeinfo */
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, typeinfo_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    free_reg(get_reg_stack(), addr_reg);

    if (out_reg != NULL)
        *out_reg = typeinfo_reg;
    else
        free_reg(get_reg_stack(), typeinfo_reg);
    return inst_list;
}

static void codegen_move_rtti_args(ListNode_t **inst_list,
    const Register_t *value_reg, const char *target_label)
{
    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", value_reg->bit_64);
        *inst_list = add_inst(*inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s_TYPEINFO(%%rip), %%rdx\n", target_label);
        *inst_list = add_inst(*inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", value_reg->bit_64);
        *inst_list = add_inst(*inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s_TYPEINFO(%%rip), %%rsi\n", target_label);
        *inst_list = add_inst(*inst_list, buffer);
    }
}
/* Helper to check if a formal parameter declaration expects a string type. */
static int formal_decl_expects_string(Tree_t *decl)
{
    if (decl == NULL)
        return 0;

    if (decl->type != TREE_VAR_DECL)
        return 0;

    if (decl->tree_data.var_decl_data.type == STRING_TYPE)
        return 1;

    if (decl->tree_data.var_decl_data.type_id != NULL)
    {
        const char *type_id = decl->tree_data.var_decl_data.type_id;
        if (pascal_identifier_equals(type_id, "string") ||
            pascal_identifier_equals(type_id, "ansistring"))
            return 1;
    }

    return 0;
}

static int builtin_arg_expects_string(const char *procedure_name, int arg_index)
{
    if (procedure_name == NULL || arg_index != 0)
        return 0;
    if (pascal_identifier_equals(procedure_name, "Pos") ||
        pascal_identifier_equals(procedure_name, "AnsiPos") ||
        pascal_identifier_equals(procedure_name, "kgpc_string_pos"))
    {
        return 1;
    }
    return 0;
}

static int codegen_param_expected_type(Tree_t *decl, SymTab_t *symtab)
{
    if (decl == NULL)
        return UNKNOWN_TYPE;

    HashNode_t *type_node = NULL;
    char *type_id = NULL;

    if (decl->type == TREE_VAR_DECL)
    {
        type_id = decl->tree_data.var_decl_data.type_id;
        if (decl->tree_data.var_decl_data.type != UNKNOWN_TYPE)
            return decl->tree_data.var_decl_data.type;
    }
    if (decl->type == TREE_ARR_DECL)
    {
        type_id = decl->tree_data.arr_decl_data.type_id;
        if (decl->tree_data.arr_decl_data.type != UNKNOWN_TYPE)
            return decl->tree_data.arr_decl_data.type;
    }

    /* Special case: ShortString type identifier */
    if (type_id != NULL && pascal_identifier_equals(type_id, "ShortString"))
        return SHORTSTRING_TYPE;

    if (type_id != NULL && symtab != NULL &&
        FindIdent(&type_node, symtab, type_id) >= 0 && type_node != NULL &&
        type_node->type != NULL)
    {
        int resolved = kgpc_type_get_legacy_tag(type_node->type);
        if (resolved != UNKNOWN_TYPE)
            return resolved;
    }

    return UNKNOWN_TYPE;
}

static int codegen_expected_type_for_builtin(const char *name)
{
    if (name == NULL)
        return UNKNOWN_TYPE;

    if (pascal_identifier_equals(name, "Trunc") ||
        pascal_identifier_equals(name, "Int") ||
        pascal_identifier_equals(name, "Round") ||
        pascal_identifier_equals(name, "Frac") ||
        pascal_identifier_equals(name, "Ln") ||
        pascal_identifier_equals(name, "Exp") ||
        pascal_identifier_equals(name, "Sqrt") ||
        pascal_identifier_equals(name, "Sin") ||
        pascal_identifier_equals(name, "Csc") ||
        pascal_identifier_equals(name, "Sinh") ||
        pascal_identifier_equals(name, "Csch") ||
        pascal_identifier_equals(name, "Cos") ||
        pascal_identifier_equals(name, "Sec") ||
        pascal_identifier_equals(name, "Cosh") ||
        pascal_identifier_equals(name, "Sech") ||
        pascal_identifier_equals(name, "Tan") ||
        pascal_identifier_equals(name, "Cot") ||
        pascal_identifier_equals(name, "Tanh") ||
        pascal_identifier_equals(name, "Coth") ||
        pascal_identifier_equals(name, "ArcSin") ||
        pascal_identifier_equals(name, "ArcCos") ||
        pascal_identifier_equals(name, "ArcCosh") ||
        pascal_identifier_equals(name, "ArcSech") ||
        pascal_identifier_equals(name, "ArcCsch") ||
        pascal_identifier_equals(name, "ArcTan2") ||
        pascal_identifier_equals(name, "Hypot") ||
        pascal_identifier_equals(name, "ArcSinh") ||
        pascal_identifier_equals(name, "ArcTanh") ||
        pascal_identifier_equals(name, "ArcCot") ||
        pascal_identifier_equals(name, "ArcCoth") ||
        pascal_identifier_equals(name, "ArcTan") ||
        pascal_identifier_equals(name, "DegToRad") ||
        pascal_identifier_equals(name, "RadToDeg") ||
        pascal_identifier_equals(name, "DegToGrad") ||
        pascal_identifier_equals(name, "GradToDeg") ||
        pascal_identifier_equals(name, "GradToRad") ||
        pascal_identifier_equals(name, "RadToGrad") ||
        pascal_identifier_equals(name, "CycleToRad") ||
        pascal_identifier_equals(name, "RadToCycle") ||
        pascal_identifier_equals(name, "Ln") ||
        pascal_identifier_equals(name, "Log10") ||
        pascal_identifier_equals(name, "Log2") ||
        pascal_identifier_equals(name, "LogN") ||
        pascal_identifier_equals(name, "Exp"))
    {
        return REAL_TYPE;
    }

    if (pascal_identifier_equals(name, "Random"))
        return LONGINT_TYPE;
    if (pascal_identifier_equals(name, "RandomRange"))
        return LONGINT_TYPE;
    if (pascal_identifier_equals(name, "Power"))
        return REAL_TYPE;
    if (pascal_identifier_equals(name, "Ceil") ||
        pascal_identifier_equals(name, "Floor"))
        return REAL_TYPE;

    return UNKNOWN_TYPE;
}

static ListNode_t *codegen_expr_convert_int_like_to_real(ListNode_t *inst_list,
    Register_t *value_reg, int source_type)
{
    if (inst_list == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (source_type == LONGINT_TYPE)
        snprintf(buffer, sizeof(buffer), "\tcvtsi2sdq\t%s, %%xmm0\n", value_reg->bit_64);
    else
        snprintf(buffer, sizeof(buffer), "\tcvtsi2sdl\t%s, %%xmm0\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n", value_reg->bit_64);
    return add_inst(inst_list, buffer);
}

static ListNode_t *codegen_expr_maybe_convert_int_like_to_real(int target_type,
    struct Expression *source_expr, Register_t *value_reg, ListNode_t *inst_list)
{
    if (inst_list == NULL || source_expr == NULL || value_reg == NULL)
        return inst_list;

    int source_type = expr_get_type_tag(source_expr);
    if (target_type == REAL_TYPE &&
        (source_type == INT_TYPE || source_type == LONGINT_TYPE))
    {
        inst_list = codegen_expr_convert_int_like_to_real(inst_list, value_reg, source_type);
    }

    return inst_list;
}

static unsigned long codegen_expr_next_temp_suffix(void)
{
    static unsigned long counter = 0;
    return ++counter;
}

static int codegen_expr_align_to(int value, int alignment)
{
    if (alignment <= 0)
        return value;
    int remainder = value % alignment;
    if (remainder == 0)
        return value;
    return value + (alignment - remainder);
}

static StackNode_t *codegen_alloc_temp_bytes(const char *prefix, int size)
{
    if (size <= 0)
        size = DOUBLEWORD;
    char label[32];
    snprintf(label, sizeof(label), "%s_%lu", prefix != NULL ? prefix : "temp",
        codegen_expr_next_temp_suffix());
    return add_l_t_bytes(label, size);
}

static int formal_decl_is_open_array(Tree_t *decl)
{
    if (decl == NULL || decl->type != TREE_ARR_DECL)
        return 0;

    struct Array *arr = &decl->tree_data.arr_decl_data;
    return (arr->e_range < arr->s_range);
}

static long long codegen_static_array_length(const struct Expression *expr)
{
    if (expr == NULL || !expr->is_array_expr || expr->array_is_dynamic)
        return -1;

    long long lower = expr->array_lower_bound;
    long long upper = expr->array_upper_bound;
    if (upper < lower)
        return -1;

    return (upper - lower) + 1;
}

ListNode_t *codegen_emit_is_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (out_reg != NULL)
        *out_reg = NULL;

    if (expr == NULL)
        return inst_list;

    const char *target_label = codegen_class_typeinfo_label(
        expr->expr_data.is_data.target_record_type,
        expr->expr_data.is_data.target_type_id);
    if (target_label == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to resolve class type for \"is\" operator.");
        return inst_list;
    }

    Register_t *value_reg = NULL;
    inst_list = codegen_load_class_typeinfo(expr->expr_data.is_data.expr, inst_list, ctx, &value_reg);
    if (value_reg == NULL)
        return inst_list;

    codegen_move_rtti_args(&inst_list, value_reg, target_label);
    free_reg(get_reg_stack(), value_reg);

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_rtti_is\n");
    free_arg_regs();

    if (out_reg != NULL)
    {
        Register_t *result_reg = codegen_try_get_reg(&inst_list, ctx, "is result");
        if (result_reg == NULL)
            return inst_list;

        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tmovl\t%%eax, %s\n", result_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        *out_reg = result_reg;
    }

    return inst_list;
}

ListNode_t *codegen_emit_class_cast_check_from_address(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t *addr_reg)
{
    if (expr == NULL || addr_reg == NULL)
        return inst_list;

    const char *target_label = codegen_class_typeinfo_label(
        expr->expr_data.as_data.target_record_type,
        expr->expr_data.as_data.target_type_id);
    if (target_label == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to resolve class type for \"as\" operator.");
        return inst_list;
    }

    /* Check if the source expression is a class variable (pointer) */
    struct Expression *source_expr = expr->expr_data.as_data.expr;
    int is_class_var = (source_expr != NULL && source_expr->record_type != NULL && 
                        record_type_is_class(source_expr->record_type));

    Register_t *instance_ptr_reg = addr_reg;
    
    /* For class variables, addr_reg points to the variable holding the pointer.
     * We need to load the pointer value to get the address of the instance. */
    if (is_class_var)
    {
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        /* Now addr_reg contains the pointer to the instance */
    }

    Register_t *typeinfo_reg = codegen_try_get_reg(&inst_list, ctx, "class RTTI");
    if (typeinfo_reg == NULL)
        return inst_list;

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", instance_ptr_reg->bit_64, typeinfo_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    /* Preserve the instance pointer across the runtime call (caller-saved registers may be clobbered).
     * Reserve the 32-byte Windows shadow space as well so the saved pointer is not overwritten. */
    inst_list = add_inst(inst_list, "\tsubq\t$48, %rsp\n");
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, 32(%%rsp)\n", instance_ptr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    codegen_move_rtti_args(&inst_list, typeinfo_reg, target_label);
    free_reg(get_reg_stack(), typeinfo_reg);

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tkgpc_rtti_check_cast\n");
    free_arg_regs();

    snprintf(buffer, sizeof(buffer), "\tmovq\t32(%%rsp), %s\n", instance_ptr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    inst_list = add_inst(inst_list, "\taddq\t$48, %rsp\n");
    return inst_list;
}

ListNode_t *codegen_emit_class_cast_check(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (expr == NULL || expr->expr_data.as_data.expr == NULL)
        return inst_list;

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(expr->expr_data.as_data.expr, inst_list, ctx, &addr_reg);
    if (addr_reg == NULL)
        return inst_list;

    inst_list = codegen_emit_class_cast_check_from_address(expr, inst_list, ctx, addr_reg);
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}

static const char *codegen_register_name8(const Register_t *reg)
{
    if (reg == NULL || reg->bit_64 == NULL)
        return NULL;

    static const struct
    {
        const char *wide;
        const char *byte;
    } register_map[] = {
        { "%rax", "%al" },
        { "%rbx", "%bl" },
        { "%rcx", "%cl" },
        { "%rdx", "%dl" },
        { "%rsi", "%sil" },
        { "%rdi", "%dil" },
        { "%rbp", "%bpl" },
        { "%rsp", "%spl" },
        { "%r8", "%r8b" },
        { "%r9", "%r9b" },
        { "%r10", "%r10b" },
        { "%r11", "%r11b" },
        { "%r12", "%r12b" },
        { "%r13", "%r13b" },
        { "%r14", "%r14b" },
        { "%r15", "%r15b" },
    };

    size_t count = sizeof(register_map) / sizeof(register_map[0]);
    for (size_t i = 0; i < count; ++i)
    {
        if (strcmp(reg->bit_64, register_map[i].wide) == 0)
            return register_map[i].byte;
    }

    return NULL;
}

const char *codegen_register_name16(const Register_t *reg)
{
    if (reg == NULL || reg->bit_64 == NULL)
        return NULL;

    static const struct
    {
        const char *wide;
        const char *word;
    } register_map[] = {
        { "%rax", "%ax" },
        { "%rbx", "%bx" },
        { "%rcx", "%cx" },
        { "%rdx", "%dx" },
        { "%rsi", "%si" },
        { "%rdi", "%di" },
        { "%rbp", "%bp" },
        { "%rsp", "%sp" },
        { "%r8", "%r8w" },
        { "%r9", "%r9w" },
        { "%r10", "%r10w" },
        { "%r11", "%r11w" },
        { "%r12", "%r12w" },
        { "%r13", "%r13w" },
        { "%r14", "%r14w" },
        { "%r15", "%r15w" },
    };

    size_t count = sizeof(register_map) / sizeof(register_map[0]);
    for (size_t i = 0; i < count; ++i)
    {
        if (strcmp(reg->bit_64, register_map[i].wide) == 0)
            return register_map[i].word;
    }

    return NULL;
}

static ListNode_t *codegen_store_value_to_stack(ListNode_t *inst_list, Register_t *value_reg,
    int offset, int element_size)
{
    if (value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (element_size == 1)
    {
        const char *reg8 = codegen_register_name8(value_reg);
        assert(reg8 != NULL && "8-bit register name not found for store operation");
        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", reg8, offset);
        return add_inst(inst_list, buffer);
    }
    else if (element_size == 2)
    {
        const char *reg16 = codegen_register_name16(value_reg);
        assert(reg16 != NULL && "16-bit register name not found for store operation");
        snprintf(buffer, sizeof(buffer), "\tmovw\t%s, -%d(%%rbp)\n", reg16, offset);
        return add_inst(inst_list, buffer);
    }
    else if (element_size == 4)
    {
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", value_reg->bit_32, offset);
        return add_inst(inst_list, buffer);
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", value_reg->bit_64, offset);
    return add_inst(inst_list, buffer);
}

static ListNode_t *codegen_materialize_array_literal(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || expr->type != EXPR_ARRAY_LITERAL || out_reg == NULL)
        return inst_list;

    if (expr->array_element_type == ARRAY_OF_CONST_TYPE)
        return codegen_materialize_array_of_const(expr, inst_list, ctx, out_reg);

    int element_size = expr_get_array_element_size(expr, ctx);
    if (element_size <= 0)
        element_size = DOUBLEWORD;

    int element_count = expr->expr_data.array_literal_data.element_count;
    int data_size = codegen_expr_align_to(element_count * element_size, DOUBLEWORD);
    StackNode_t *data_slot = codegen_alloc_temp_bytes("arr_lit_data", data_size);
    if (data_slot == NULL)
        return inst_list;

    ListNode_t *cur = expr->expr_data.array_literal_data.elements;
    int index = 0;
    while (cur != NULL)
    {
        struct Expression *element_expr = (struct Expression *)cur->cur;
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(element_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        int element_offset = data_slot->offset - index * element_size;
        inst_list = codegen_store_value_to_stack(inst_list, value_reg, element_offset, element_size);
        free_reg(get_reg_stack(), value_reg);

        cur = cur->next;
        ++index;
    }

    const int pointer_bytes = CODEGEN_POINTER_SIZE_BYTES;
    int descriptor_size = codegen_expr_align_to(2 * pointer_bytes, pointer_bytes);
    if (expr->array_element_size > 0)
    {
        int candidate = expr->array_element_size * 2;
        if (descriptor_size < candidate)
            descriptor_size = codegen_expr_align_to(candidate, pointer_bytes);
    }
    StackNode_t *desc_slot = codegen_alloc_temp_bytes("arr_lit_desc", descriptor_size);
    if (desc_slot == NULL)
        return inst_list;

    Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (addr_reg == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to allocate register for array literal descriptor.");
        return inst_list;
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", data_slot->offset, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, desc_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %s\n", element_count, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64,
        desc_slot->offset - pointer_bytes);
    inst_list = add_inst(inst_list, buffer);

    int field_count = descriptor_size / pointer_bytes;
    for (int field = 2; field < field_count; ++field)
    {
        int field_offset = desc_slot->offset - field * pointer_bytes;
        snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", field_offset);
        inst_list = add_inst(inst_list, buffer);
    }

    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", desc_slot->offset, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    *out_reg = addr_reg;
    return inst_list;
}

static int codegen_format_arg_kind_for_expr(struct Expression *expr)
{
    int type_tag = expr_get_type_tag(expr);
    switch (type_tag)
    {
        case INT_TYPE:
        case LONGINT_TYPE:
            return KGPC_TVAR_KIND_INT;
        case BOOL:
            return KGPC_TVAR_KIND_BOOL;
        case CHAR_TYPE:
            return KGPC_TVAR_KIND_CHAR;
        case REAL_TYPE:
            return KGPC_TVAR_KIND_REAL;
        case STRING_TYPE:
            return KGPC_TVAR_KIND_STRING;
        case POINTER_TYPE:
            return KGPC_TVAR_KIND_POINTER;
        default:
            return -1;
    }
}

static ListNode_t *codegen_materialize_array_of_const(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || expr->type != EXPR_ARRAY_LITERAL || out_reg == NULL)
        return inst_list;

    const int element_size = (int)sizeof(kgpc_tvarrec);
    int element_count = expr->expr_data.array_literal_data.element_count;
    int data_size = codegen_expr_align_to(element_count * element_size, DOUBLEWORD);
    StackNode_t *data_slot = codegen_alloc_temp_bytes("arr_const_data", data_size);
    if (data_slot == NULL)
        return inst_list;

    ListNode_t *cur = expr->expr_data.array_literal_data.elements;
    int index = 0;
    char buffer[128];

    while (cur != NULL)
    {
        struct Expression *element_expr = (struct Expression *)cur->cur;
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(element_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        int kind = codegen_format_arg_kind_for_expr(element_expr);
        if (kind < 0)
        {
            codegen_report_error(ctx,
                "ERROR: Unsupported argument type in array of const literal.");
            free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        int element_offset = data_slot->offset - index * element_size;
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, -%d(%%rbp)\n",
            kind, element_offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$0, -%d(%%rbp)\n",
            element_offset - 4);
        inst_list = add_inst(inst_list, buffer);

        int data_offset = element_offset - 8;
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
            value_reg->bit_64, data_offset);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), value_reg);
        cur = cur->next;
        ++index;
    }

    const int pointer_bytes = CODEGEN_POINTER_SIZE_BYTES;
    StackNode_t *desc_slot = codegen_alloc_temp_bytes("arr_const_desc",
        codegen_expr_align_to(2 * pointer_bytes, pointer_bytes));
    if (desc_slot == NULL)
        return inst_list;

    Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (addr_reg == NULL)
    {
        codegen_report_error(ctx,
            "ERROR: Unable to allocate register for array of const descriptor.");
        return inst_list;
    }

    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
        data_slot->offset, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
        addr_reg->bit_64, desc_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %s\n",
        element_count, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
        addr_reg->bit_64, desc_slot->offset - pointer_bytes);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
        desc_slot->offset, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    *out_reg = addr_reg;
    return inst_list;
}

/* Helper function to get TypeAlias from HashNode */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    return hashnode_get_type_alias(node);
}

static unsigned long codegen_next_record_temp_id(void)
{
    static unsigned long counter = 0;
    return ++counter;
}

static StackNode_t *codegen_alloc_record_temp(long long size)
{
    if (size <= 0 || size > INT_MAX)
        return NULL;

    char label[32];
    snprintf(label, sizeof(label), "record_arg_%lu", codegen_next_record_temp_id());
    return add_l_x(label, (int)size);
}


static inline int type_is_file_like(int type_tag)
{
    return type_tag == FILE_TYPE || type_tag == TEXT_TYPE;
}

int codegen_type_uses_qword(int type_tag)
{
    return (type_tag == REAL_TYPE || type_tag == INT64_TYPE ||
        type_tag == POINTER_TYPE || type_tag == STRING_TYPE ||
        type_is_file_like(type_tag) || type_tag == PROCEDURE);
}

int codegen_type_is_signed(int type_tag)
{
    switch (type_tag)
    {
        case INT_TYPE:
        case LONGINT_TYPE:
            return 1;
        default:
            return 0;
    }
}

/* Helper to get KgpcType from expression, preferring resolved_kgpc_type.
 * Returns the KgpcType if available, or creates a temporary one from legacy fields.
 * Returns NULL if type cannot be determined.
 * Note: The returned KgpcType should NOT be freed - it's either owned by the expression
 * or is a static/temporary type. */
KgpcType* expr_get_kgpc_type(const struct Expression *expr)
{
    if (expr == NULL)
        return NULL;
    
    /* Prefer KgpcType if available */
    if (expr->resolved_kgpc_type != NULL)
        return expr->resolved_kgpc_type;
    
    /* For legacy compatibility, create KgpcType from resolved_type tag.
     * resolved_type is a type tag (INT_TYPE, REAL_TYPE, etc.), not VarType.
     * For simple types, we can create a primitive KgpcType. */
    int type_tag = expr->resolved_type;
    
    /* Handle primitive types */
    switch (type_tag) {
        case INT_TYPE:
        case LONGINT_TYPE:
        case REAL_TYPE:
        case CHAR_TYPE:
        case BOOL:
        case STRING_TYPE:
        case SET_TYPE:
        case ENUM_TYPE:
        case FILE_TYPE:
        case TEXT_TYPE:
            /* These can be represented as primitive KgpcTypes, but we can't
             * create them here without memory management issues.
             * Better to just return NULL and let callers fall back to legacy logic */
            return NULL;
        
        case POINTER_TYPE:
        case RECORD_TYPE:
        case PROCEDURE:
        case UNKNOWN_TYPE:
        default:
            /* Complex types or unknown - can't create KgpcType */
            return NULL;
    }
}

long long expr_effective_size_bytes(const struct Expression *expr)
{
    KgpcType *type = expr_get_kgpc_type(expr);
    if (type != NULL)
    {
        long long size = kgpc_type_sizeof(type);
        if (size > 0)
            return size;
    }

    /* For pointer dereference, try to get size from the pointer's subtype info.
     * This handles cases like PByte^ where Byte is a subrange type that maps to
     * INT_TYPE but should have size 1. */
    if (expr != NULL && expr->type == EXPR_POINTER_DEREF)
    {
        struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
        if (pointer_expr != NULL && pointer_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_pointer(pointer_expr->resolved_kgpc_type))
        {
            KgpcType *points_to = pointer_expr->resolved_kgpc_type->info.points_to;
            if (points_to != NULL)
            {
                long long size = kgpc_type_sizeof(points_to);
                if (size > 0)
                    return size;
            }
        }
    }

    int tag = expr_get_type_tag(expr);
    switch (tag)
    {
        case CHAR_TYPE:
            return 1;
        case INT_TYPE:
        case BOOL:
        case SET_TYPE:
        case ENUM_TYPE:
            return 4;
        case FILE_TYPE:
            return 368;
        case TEXT_TYPE:
            return 632;
        case STRING_TYPE:
        case POINTER_TYPE:
        case REAL_TYPE:
            return 8;
        case LONGINT_TYPE:
            return 4;  // Match FPC's 32-bit LongInt
        default:
            return 0;
    }
}

/* Helper to get type tag from expression, preferring resolved_kgpc_type */
int expr_get_type_tag(const struct Expression *expr)
{
    if (expr == NULL)
        return UNKNOWN_TYPE;

    if (expr->type == EXPR_MULOP &&
        expr->expr_data.mulop_data.mulop_type == SLASH)
        return REAL_TYPE;
    
    /* Prefer KgpcType if available */
    if (expr->resolved_kgpc_type != NULL)
    {
        int tag = kgpc_type_get_legacy_tag(expr->resolved_kgpc_type);
        if (tag != UNKNOWN_TYPE)
            return tag;
    }
    
    /* Fall back to legacy field */
    return expr->resolved_type;
}

/* Helper to get array lower bound from expression, preferring resolved_kgpc_type */
int expr_get_array_lower_bound(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    /* Prefer KgpcType if available */
    if (expr->resolved_kgpc_type != NULL && kgpc_type_is_array(expr->resolved_kgpc_type))
    {
        int start = 0;
        if (kgpc_type_get_array_bounds(expr->resolved_kgpc_type, &start, NULL) == 0)
            return start;
    }
    
    /* Fall back to legacy field */
    return expr->array_lower_bound;
}

/* Helper to get array upper bound from expression, preferring resolved_kgpc_type */
int expr_get_array_upper_bound(const struct Expression *expr)
{
    if (expr == NULL)
        return -1;

    if (expr->resolved_kgpc_type != NULL && kgpc_type_is_array(expr->resolved_kgpc_type))
    {
        int end = -1;
        if (kgpc_type_get_array_bounds(expr->resolved_kgpc_type, NULL, &end) == 0)
            return end;
    }

    return expr->array_upper_bound;
}

/* Check if an expression represents a character set (set of char) */
int expr_is_char_set_ctx(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL)
        return 0;
    
    /* Check if expression has a KgpcType with type_alias */
    if (expr->resolved_kgpc_type != NULL)
    {
        struct TypeAlias *alias = expr->resolved_kgpc_type->type_alias;
        if (alias != NULL && alias->is_set && alias->set_element_type == CHAR_TYPE)
            return 1;
    }
    
    /* For variable references, look up the type in the symbol table */
    if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, ctx->symtab, expr->expr_data.id) >= 0 && node != NULL)
        {
            if (node->type != NULL)
            {
                struct TypeAlias *alias = node->type->type_alias;
                if (alias != NULL && alias->is_set && alias->set_element_type == CHAR_TYPE)
                    return 1;
            }
            if (node->hash_type == HASHTYPE_CONST &&
                node->const_set_value != NULL &&
                node->const_set_size > 0)
            {
                return 1;
            }
        }
    }
    
    /* For set literals, check if elements are characters or single-char strings */
    if (expr->type == EXPR_SET && expr->expr_data.set_data.elements != NULL)
    {
        ListNode_t *node = expr->expr_data.set_data.elements;
        while (node != NULL)
        {
            struct SetElement *element = (struct SetElement *)node->cur;
            if (element->lower != NULL)
            {
                int elem_type = element->lower->resolved_type;
                /* Character sets can have CHAR_TYPE or STRING_TYPE (single char) elements */
                if (elem_type == CHAR_TYPE)
                    return 1;
                if (elem_type == STRING_TYPE && element->lower->type == EXPR_STRING)
                {
                    /* Single-character string literal */
                    if (element->lower->expr_data.string != NULL &&
                        strlen(element->lower->expr_data.string) == 1)
                        return 1;
                }
                if (element->lower->type == EXPR_CHAR_CODE)
                    return 1;
                if (element->lower->type == EXPR_STRING &&
                    element->lower->expr_data.string != NULL &&
                    strlen(element->lower->expr_data.string) == 1)
                    return 1;
            }
            node = node->next;
        }
    }
    
    return 0;
}

/* Wrapper that doesn't need context - for backward compatibility */
int expr_is_char_set(const struct Expression *expr)
{
    return expr_is_char_set_ctx(expr, NULL);
}

/* Helper to get array element size from expression, preferring resolved_kgpc_type
 * ctx parameter reserved for future use in computing complex type sizes */
long long expr_get_array_element_size(const struct Expression *expr, CodeGenContext *ctx)
{
    (void)ctx; /* Reserved for future use */
    if (expr == NULL)
        return -1;

    if (expr->array_element_type == ARRAY_OF_CONST_TYPE)
        return (long long)sizeof(kgpc_tvarrec);
    
    /* Prefer KgpcType if available */
    if (expr->resolved_kgpc_type != NULL && kgpc_type_is_array(expr->resolved_kgpc_type))
    {
        long long size = kgpc_type_get_array_element_size(expr->resolved_kgpc_type);
        if (size > 0)
            return size;
    }
    
    /* Fall back to legacy field */
    return expr->array_element_size;
}

/* Check if expression is signed, working with KgpcType */
static int expr_is_signed_kgpctype(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    KgpcType *type = expr_get_kgpc_type(expr);
    if (type != NULL)
        return kgpc_type_is_signed(type);
    
    /* Ultimate fallback for legacy compatibility */
    return codegen_type_is_signed(expr->resolved_type);
}

/* Check if expression uses qword, working with KgpcType */
int expr_uses_qword_kgpctype(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    KgpcType *type = expr_get_kgpc_type(expr);
    if (type != NULL)
        return kgpc_type_uses_qword(type);
    
    /* Ultimate fallback for legacy compatibility */
    return codegen_type_uses_qword(expr->resolved_type);
}

/* Check if expression has a specific type tag, working with KgpcType */
int expr_has_type_tag(const struct Expression *expr, int type_tag)
{
    if (expr == NULL)
        return (type_tag == UNKNOWN_TYPE);
    
    KgpcType *type = expr_get_kgpc_type(expr);
    if (type != NULL)
        return kgpc_type_equals_tag(type, type_tag);
    
    /* Ultimate fallback for legacy compatibility */
    return (expr->resolved_type == type_tag);
}

static int expr_is_char_pointer(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr->resolved_type != POINTER_TYPE)
    {
        KgpcType *type = expr_get_kgpc_type(expr);
        if (type == NULL || !kgpc_type_is_pointer(type))
            return 0;
    }

    if (expr->pointer_subtype == CHAR_TYPE)
        return 1;
    if (expr->pointer_subtype_id != NULL)
    {
        if (pascal_identifier_equals(expr->pointer_subtype_id, "AnsiChar") ||
            pascal_identifier_equals(expr->pointer_subtype_id, "WideChar") ||
            pascal_identifier_equals(expr->pointer_subtype_id, "Char"))
            return 1;
    }

    KgpcType *type = expr_get_kgpc_type(expr);
    if (type != NULL && kgpc_type_is_pointer(type))
    {
        KgpcType *pointee = type->info.points_to;
        if (pointee != NULL &&
            pointee->kind == TYPE_KIND_PRIMITIVE &&
            pointee->info.primitive_type_tag == CHAR_TYPE)
            return 1;
    }

    return 0;
}

int expr_returns_sret(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr_has_type_tag(expr, RECORD_TYPE))
        return 1;

    /* ShortStrings are passed via SRET because they're small fixed-size arrays */
    if (expr_has_type_tag(expr, SHORTSTRING_TYPE))
        return 1;

    KgpcType *type = expr_get_kgpc_type(expr);
    if (type != NULL && type->kind == TYPE_KIND_ARRAY &&
        !kgpc_type_is_dynamic_array(type))
        return 1;

    /* Also check for shortstring type aliases */
    if (type != NULL && type->type_alias != NULL && type->type_alias->is_shortstring)
        return 1;

    return 0;
}

void codegen_release_function_call_mangled_id(struct Expression *expr)
{
    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return;

    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
}

int codegen_expr_is_signed(const struct Expression *expr)
{
    return expr_is_signed_kgpctype(expr);
}

static inline const char *register_name_for_type(const Register_t *reg, int type_tag)
{
    if (reg == NULL)
        return NULL;
    return codegen_type_uses_qword(type_tag) ? reg->bit_64 : reg->bit_32;
}

static inline const char *register_name_for_expr(const Register_t *reg, const struct Expression *expr)
{
    if (expr == NULL)
        return register_name_for_type(reg, UNKNOWN_TYPE);
    /* Use KgpcType-based helper instead of converting to tag */
    return expr_uses_qword_kgpctype(expr) ? reg->bit_64 : reg->bit_32;
}

static inline int expression_uses_qword(const struct Expression *expr)
{
    return expr_uses_qword_kgpctype(expr);
}

static int codegen_sizeof_type(CodeGenContext *ctx, int type_tag, const char *type_id,
    struct RecordType *record_type, long long *size_out, int depth);

static int codegen_sizeof_record(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out, int depth);
int codegen_sizeof_record_type(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out);

static int codegen_sizeof_alias(CodeGenContext *ctx, struct TypeAlias *alias,
    long long *size_out, int depth);

static int codegen_formal_is_dynamic_array(Tree_t *formal, SymTab_t *symtab)
{
    if (formal == NULL || formal->type != TREE_VAR_DECL)
        return 0;

    KgpcType *cached = formal->tree_data.var_decl_data.cached_kgpc_type;
    if (cached != NULL && cached->kind == TYPE_KIND_ARRAY &&
        kgpc_type_is_dynamic_array(cached))
    {
        return 1;
    }

    if (symtab != NULL && formal->tree_data.var_decl_data.type_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, formal->tree_data.var_decl_data.type_id) != -1 &&
            type_node != NULL && type_node->type != NULL &&
            type_node->type->kind == TYPE_KIND_ARRAY &&
            kgpc_type_is_dynamic_array(type_node->type))
        {
            return 1;
        }
    }

    return 0;
}

static int codegen_sizeof_hashnode(CodeGenContext *ctx, HashNode_t *node,
    long long *size_out, int depth);

int codegen_expr_is_addressable(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    switch (expr->type)
    {
        case EXPR_VAR_ID:
        case EXPR_ARRAY_ACCESS:
        case EXPR_RECORD_ACCESS:
        case EXPR_POINTER_DEREF:
        case EXPR_RECORD_CONSTRUCTOR:
            return 1;
        case EXPR_TYPECAST:
            if (expr->expr_data.typecast_data.expr != NULL)
                return codegen_expr_is_addressable(expr->expr_data.typecast_data.expr);
            return 0;
        case EXPR_AS:
            if (expr->expr_data.as_data.expr != NULL)
                return codegen_expr_is_addressable(expr->expr_data.as_data.expr);
            return 0;
        default:
            return 0;
    }
}

static int codegen_sizeof_array_node(CodeGenContext *ctx, HashNode_t *node,
    long long *size_out, int depth)
{
    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx,
            "ERROR: Type resolution exceeded supported recursion depth.");
        return 1;
    }

    /* Check if array is dynamic */
    int is_dynamic = hashnode_is_dynamic_array(node);
    
    if (is_dynamic)
    {
        codegen_report_error(ctx,
            "ERROR: Unable to determine size of dynamic array %s.",
            node->id != NULL ? node->id : "");
        return 1;
    }

    /* Get element size from KgpcType */
    long long element_size = hashnode_get_element_size(node);
    
    if (element_size <= 0)
    {
        struct TypeAlias *alias = get_type_alias_from_node(node);
        if (alias != NULL && alias->is_array)
        {
            if (codegen_sizeof_type(ctx, alias->array_element_type,
                    alias->array_element_type_id, NULL,
                    &element_size, depth + 1) != 0)
                return 1;
        }
        else if (node_is_record_type(node))
        {
            struct RecordType *record_type = get_record_type_from_node(node);
            if (record_type != NULL && codegen_sizeof_record(ctx, record_type, &element_size,
                    depth + 1) != 0)
                return 1;
        }
        else
        {
            if (node->type == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to determine element size for array %s (missing type info).",
                    node->id != NULL ? node->id : "");
                return 1;
            }

            long long base = kgpc_type_sizeof(node->type);
            if (base < 0)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to determine element size for array %s.",
                    node->id != NULL ? node->id : "");
                return 1;
            }
            element_size = base;
        }
    }

    /* Get array bounds from KgpcType if available */
    int array_start, array_end;
    hashnode_get_array_bounds(node, &array_start, &array_end);
    
    long long count = (long long)array_end - (long long)array_start + 1;
    if (count < 0)
    {
        codegen_report_error(ctx,
            "ERROR: Invalid bounds for array %s during size computation.",
            node->id != NULL ? node->id : "");
        return 1;
    }

    *size_out = element_size * count;
    return 0;
}

static long long codegen_sizeof_type_tag(int type_tag)
{
    switch (type_tag)
    {
        case INT_TYPE:
        case BOOL:
        case SET_TYPE:
        case ENUM_TYPE:
            return 4;
        case LONGINT_TYPE:
            return 4;  // Match FPC's 32-bit LongInt
        case REAL_TYPE:
            return 8;
        case STRING_TYPE:
        case POINTER_TYPE:
        case FILE_TYPE:
        case TEXT_TYPE:
        case PROCEDURE:
            return CODEGEN_POINTER_SIZE_BYTES;
        case CHAR_TYPE:
            return 1;
        case RECORD_TYPE:
            return -1;
        default:
            return -1;
    }
}

static int codegen_sizeof_type(CodeGenContext *ctx, int type_tag, const char *type_id,
    struct RecordType *record_type, long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Unable to determine type size due to excessive nesting.");
        return 1;
    }

    if (record_type != NULL)
        return codegen_sizeof_record(ctx, record_type, size_out, depth + 1);

    if (type_tag == RECORD_TYPE && type_id == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to resolve anonymous record type for size computation.");
        return 1;
    }

    if (type_tag != UNKNOWN_TYPE)
    {
        long long base = codegen_sizeof_type_tag(type_tag);
        if (base >= 0)
        {
            *size_out = base;
            return 0;
        }
    }

    if (type_id != NULL && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, ctx->symtab, (char *)type_id) >= 0 && node != NULL)
            return codegen_sizeof_hashnode(ctx, node, size_out, depth + 1);

        codegen_report_error(ctx, "ERROR: Unable to resolve type %s for size computation.", type_id);
        return 1;
    }

    codegen_report_error(ctx, "ERROR: Unable to determine size for expression type %d.", type_tag);
    return 1;
}

static int codegen_sizeof_variant_part(CodeGenContext *ctx, struct VariantPart *variant,
    long long *size_out, int depth);

static int codegen_sizeof_record_members(CodeGenContext *ctx, ListNode_t *members,
    long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    long long total = 0;
    ListNode_t *cur = members;
    while (cur != NULL)
    {
        if (cur->cur == NULL)
        {
            cur = cur->next;
            continue;
        }

        if (cur->type == LIST_RECORD_FIELD)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            long long field_size = 0;

            if (field->is_array)
            {
                if (field->array_is_open || field->array_end < field->array_start)
                {
                    field_size = CODEGEN_POINTER_SIZE_BYTES;
                }
                else
                {
                    long long element_size = 0;
                    if (codegen_sizeof_type(ctx, field->array_element_type,
                            field->array_element_type_id, NULL,
                            &element_size, depth + 1) != 0)
                        return 1;

                    long long count = (long long)field->array_end - (long long)field->array_start + 1;
                    if (count < 0)
                    {
                        codegen_report_error(ctx,
                            "ERROR: Invalid bounds for array field %s.",
                            field->name != NULL ? field->name : "");
                        return 1;
                    }

                    field_size = element_size * count;
                }

                total += field_size;
                cur = cur->next;
                continue;
            }

            if (field->nested_record != NULL)
            {
                if (codegen_sizeof_record(ctx, field->nested_record, &field_size, depth + 1) != 0)
                    return 1;
            }
            else
            {
                if (codegen_sizeof_type(ctx, field->type, field->type_id, NULL,
                        &field_size, depth + 1) != 0)
                    return 1;
            }

            total += field_size;
        }
        else if (cur->type == LIST_VARIANT_PART)
        {
            struct VariantPart *variant = (struct VariantPart *)cur->cur;
            long long variant_size = 0;
            if (codegen_sizeof_variant_part(ctx, variant, &variant_size, depth + 1) != 0)
                return 1;
            total += variant_size;
        }

        cur = cur->next;
    }

    *size_out = total;
    return 0;
}

static int codegen_sizeof_variant_part(CodeGenContext *ctx, struct VariantPart *variant,
    long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    if (variant == NULL)
    {
        *size_out = 0;
        return 0;
    }

    if (variant->has_cached_size)
    {
        *size_out = variant->cached_size;
        return 0;
    }

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Variant part nesting exceeds supported depth.");
        return 1;
    }

    long long max_size = 0;
    ListNode_t *cur = variant->branches;
    while (cur != NULL)
    {
        if (cur->type == LIST_VARIANT_BRANCH && cur->cur != NULL)
        {
            struct VariantBranch *branch = (struct VariantBranch *)cur->cur;
            long long branch_size = 0;
            if (codegen_sizeof_record_members(ctx, branch->members, &branch_size, depth + 1) != 0)
                return 1;
            if (branch_size > max_size)
                max_size = branch_size;
        }
        cur = cur->next;
    }

    variant->cached_size = max_size;
    variant->has_cached_size = 1;
    *size_out = max_size;
    return 0;
}

static int codegen_sizeof_record(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    if (record == NULL)
    {
        *size_out = 0;
        return 0;
    }

    if (record->has_cached_size && record->cached_size > 0)
    {
        *size_out = record->cached_size;
        return 0;
    }

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Record type nesting exceeds supported depth.");
        return 1;
    }

    long long members_size = 0;
    int result = codegen_sizeof_record_members(ctx, record->fields, &members_size, depth);
    if (result != 0)
        return result;
    
    /* For classes, add 8 bytes for the VMT pointer at the beginning */
    if (record_type_is_class(record))
        *size_out = 8 + members_size;
    else
        *size_out = members_size;
    
    return 0;
}

int codegen_sizeof_record_type(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out)
{
    return codegen_sizeof_record(ctx, record, size_out, 0);
}

static int codegen_sizeof_alias(CodeGenContext *ctx, struct TypeAlias *alias,
    long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    if (alias == NULL)
    {
        codegen_report_error(ctx, "ERROR: Incomplete type alias encountered during size computation.");
        return 1;
    }

    if (alias->storage_size > 0 && !alias->is_array && !alias->is_set &&
        !alias->is_enum && !alias->is_file)
    {
        *size_out = alias->storage_size;
        return 0;
    }

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Type alias nesting exceeds supported depth.");
        return 1;
    }

    if (alias->is_array)
    {
        if (alias->is_open_array || alias->array_end < alias->array_start)
        {
            codegen_report_error(ctx, "ERROR: Unable to determine size of open array type.");
            return 1;
        }

        long long element_size = 0;
        if (codegen_sizeof_type(ctx, alias->array_element_type, alias->array_element_type_id,
                NULL, &element_size, depth + 1) != 0)
            return 1;

        long long count = (long long)alias->array_end - (long long)alias->array_start + 1;
        if (count < 0)
        {
            codegen_report_error(ctx, "ERROR: Invalid bounds for array type during size computation.");
            return 1;
        }

        *size_out = element_size * count;
        return 0;
    }

    if (alias->base_type != UNKNOWN_TYPE)
        return codegen_sizeof_type(ctx, alias->base_type, NULL, NULL, size_out, depth + 1);

    if (alias->target_type_id != NULL)
        return codegen_sizeof_type(ctx, UNKNOWN_TYPE, alias->target_type_id, NULL,
            size_out, depth + 1);

    codegen_report_error(ctx, "ERROR: Unable to resolve type alias size.");
    return 1;
}

static int codegen_sizeof_hashnode(CodeGenContext *ctx, HashNode_t *node,
    long long *size_out, int depth)
{
    if (size_out == NULL || node == NULL)
        return 1;

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Type resolution exceeded supported recursion depth.");
        return 1;
    }

    /* PREFERRED PATH: Try using KgpcType directly if available */
    if (node->type != NULL)
    {
        long long size = kgpc_type_sizeof(node->type);
        if (size > 0)
        {
            *size_out = size;
            return 0;
        }
        else if (size == 0)
        {
            /* Zero-sized type */
            *size_out = 0;
            return 0;
        }
        /* else size < 0: kgpc_type_sizeof couldn't determine size, fall through to legacy path */
    }

    /* LEGACY PATH: KgpcType not available or couldn't determine size */

    /* Check if this is an array */
    int is_array = hashnode_is_array(node);
    
    if (is_array)
        return codegen_sizeof_array_node(ctx, node, size_out, depth);

    if (node->hash_type == HASHTYPE_TYPE)
    {
        struct RecordType *record = get_record_type_from_node(node);
        if (record != NULL)
            return codegen_sizeof_record(ctx, record, size_out, depth + 1);
        struct TypeAlias *alias = get_type_alias_from_node(node);
        if (alias != NULL)
            return codegen_sizeof_alias(ctx, alias, size_out, depth + 1);
    }

    if (node_is_record_type(node))
    {
        struct RecordType *record_type = get_record_type_from_node(node);
        if (record_type != NULL)
            return codegen_sizeof_record(ctx, record_type, size_out, depth + 1);
    }

    struct TypeAlias *alias = get_type_alias_from_node(node);
    if (alias != NULL)
        return codegen_sizeof_alias(ctx, alias, size_out, depth + 1);

    if (node->type != NULL)
    {
        long long base = kgpc_type_sizeof(node->type);
        if (base >= 0)
        {
            *size_out = base;
            return 0;
        }
    }
    else
    {
        codegen_report_error(ctx, "ERROR: Symbol %s has no type information.",
            node->id != NULL ? node->id : "");
        return 1;
    }

    codegen_report_error(ctx, "ERROR: Unable to determine size for symbol %s.",
        node->id != NULL ? node->id : "");
    return 1;
}

int codegen_get_record_size(CodeGenContext *ctx, struct Expression *expr,
    long long *size_out)
{
    if (expr == NULL || size_out == NULL)
        return 1;

    if (expr->record_type != NULL)
        return codegen_sizeof_record(ctx, expr->record_type, size_out, 0);

    if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, ctx->symtab, expr->expr_data.id) >= 0 && node != NULL)
            return codegen_sizeof_hashnode(ctx, node, size_out, 0);
    }

    if (expr->type == EXPR_POINTER_DEREF)
    {
        if (expr->pointer_subtype_id != NULL && ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *node = NULL;
            if (FindIdent(&node, ctx->symtab, expr->pointer_subtype_id) >= 0 && node != NULL)
                return codegen_sizeof_hashnode(ctx, node, size_out, 0);
        }

        struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
        while (pointer_expr != NULL && pointer_expr->type == EXPR_TYPECAST &&
            pointer_expr->expr_data.typecast_data.expr != NULL)
        {
            pointer_expr = pointer_expr->expr_data.typecast_data.expr;
        }

        if (pointer_expr != NULL)
        {
            if (pointer_expr->record_type != NULL)
                return codegen_sizeof_record(ctx, pointer_expr->record_type, size_out, 0);

            if (pointer_expr->pointer_subtype_id != NULL && ctx != NULL && ctx->symtab != NULL)
            {
                HashNode_t *node = NULL;
                if (FindIdent(&node, ctx->symtab, pointer_expr->pointer_subtype_id) >= 0 && node != NULL)
                    return codegen_sizeof_hashnode(ctx, node, size_out, 0);
            }
        }
    }

    if (expr->type == EXPR_TYPECAST && expr->expr_data.typecast_data.expr != NULL)
        return codegen_get_record_size(ctx, expr->expr_data.typecast_data.expr, size_out);

    codegen_report_error(ctx, "ERROR: Unable to determine size for record expression.");
    return 1;
}

int codegen_sizeof_pointer_target(CodeGenContext *ctx, struct Expression *pointer_expr,
    long long *size_out)
{
    if (pointer_expr == NULL || size_out == NULL)
        return 1;

    KgpcType *pointer_type = expr_get_kgpc_type(pointer_expr);
    if (pointer_type != NULL && kgpc_type_is_pointer(pointer_type))
    {
        KgpcType *points_to = pointer_type->info.points_to;
        if (points_to != NULL)
        {
            long long pointee_size = kgpc_type_sizeof(points_to);
            if (pointee_size > 0)
            {
                *size_out = pointee_size;
                return 0;
            }
        }
    }

    int subtype = pointer_expr->pointer_subtype;
    const char *type_id = pointer_expr->pointer_subtype_id;
    struct RecordType *record_type = pointer_expr->record_type;

    if (record_type == NULL && type_id != NULL && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, ctx->symtab, (char *)type_id) >= 0 && node != NULL)
            record_type = get_record_type_from_node(node);
    }

    if (record_type == NULL && subtype == RECORD_TYPE && type_id == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to determine record size for pointer target.");
        return 1;
    }

    return codegen_sizeof_type(ctx, subtype, type_id, record_type, size_out, 0);
}

ListNode_t *codegen_pointer_deref_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *codegen_addressof_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *codegen_record_access(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *codegen_record_field_address(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg);

/* Lookup the RecordField metadata for a record access expression */
static struct RecordField *codegen_lookup_record_field_expr(struct Expression *record_access_expr)
{
    if (record_access_expr == NULL ||
        record_access_expr->type != EXPR_RECORD_ACCESS ||
        record_access_expr->expr_data.record_access_data.field_id == NULL)
        return NULL;

    const char *field_id = record_access_expr->expr_data.record_access_data.field_id;
    struct RecordType *record = record_access_expr->record_type;
    if (record == NULL && record_access_expr->expr_data.record_access_data.record_expr != NULL)
        record = record_access_expr->expr_data.record_access_data.record_expr->record_type;
    if (record == NULL)
        return NULL;

    ListNode_t *cur = record->fields;
    while (cur != NULL)
    {
        if (cur->type == LIST_RECORD_FIELD && cur->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            if (field->name != NULL && strcmp(field->name, field_id) == 0)
                return field;
        }
        cur = cur->next;
    }
    return NULL;
}

/* Best-effort size for a record field, respecting packed/range aliases */
static long long codegen_record_field_effective_size(struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL || ctx == NULL)
        return expr_effective_size_bytes(expr);

    long long size = expr_effective_size_bytes(expr);
    struct RecordField *field = codegen_lookup_record_field_expr(expr);
    long long field_size = 0;
    if (field != NULL && !field->is_array)
    {
        struct RecordType *nested = field->nested_record;
        if (codegen_sizeof_type_reference(ctx, field->type, field->type_id, nested, &field_size) == 0 &&
            field_size > 0)
            return field_size;
    }

    if (size > 0)
        return size;
    return field_size;
}


/* Code generation for expressions */
static const char *describe_expression_kind(const struct Expression *expr)
{
    if (expr == NULL)
        return "unknown";

    switch (expr->type)
    {
        case EXPR_VAR_ID:
            return "variable reference";
        case EXPR_ARRAY_ACCESS:
            return "array access";
        case EXPR_FUNCTION_CALL:
            return "function call";
        case EXPR_ADDOP:
            return "additive expression";
        case EXPR_MULOP:
            return "multiplicative expression";
        case EXPR_SIGN_TERM:
            return "signed term";
        case EXPR_RELOP:
            return "relational expression";
        case EXPR_INUM:
            return "integer literal";
        case EXPR_RNUM:
            return "real literal";
        default:
            return "expression";
    }
}

static Register_t *codegen_try_get_reg(ListNode_t **inst_list, CodeGenContext *ctx, const char *usage)
{
    Register_t *reg = get_free_reg(get_reg_stack(), inst_list);
    if (reg == NULL)
        reg = get_reg_with_spill(get_reg_stack(), inst_list);
    if (reg == NULL)
        codegen_report_error(ctx, "ERROR: Unable to allocate register for %s.", usage);
    return reg;
}

static ListNode_t *codegen_expr_tree_value(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{

    
    if (expr != NULL)
    {
        if (expr->type == EXPR_IS)
            return codegen_emit_is_expr(expr, inst_list, ctx, out_reg);
        if (expr->type == EXPR_ARRAY_LITERAL)
        {
            Register_t *tmp_reg = NULL;
            inst_list = codegen_materialize_array_literal(expr, inst_list, ctx, &tmp_reg);
            if (out_reg != NULL)
                *out_reg = tmp_reg;
            else if (tmp_reg != NULL)
                free_reg(get_reg_stack(), tmp_reg);
            return inst_list;
        }
        if (expr->type == EXPR_AS)
        {
            Register_t *addr_reg = NULL;
            if (expr->expr_data.as_data.expr == NULL)
                return inst_list;

            inst_list = codegen_address_for_expr(expr->expr_data.as_data.expr, inst_list, ctx, &addr_reg);
            if (addr_reg == NULL)
                return inst_list;

            inst_list = codegen_emit_class_cast_check_from_address(expr, inst_list, ctx, addr_reg);

            if (out_reg != NULL)
                *out_reg = addr_reg;
            else
                free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
    }


    codegen_begin_expression(ctx);

    expr_node_t *expr_tree = build_expr_tree(expr);

    Register_t *target_reg = codegen_try_get_reg(&inst_list, ctx, describe_expression_kind(expr));
    if (target_reg == NULL)
    {

        free_expr_tree(expr_tree);

        if (out_reg != NULL)
            *out_reg = NULL;
        codegen_end_expression(ctx);
        return inst_list;
    }


    inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);

    free_expr_tree(expr_tree);


    if (out_reg != NULL)
    {
        *out_reg = target_reg;
        codegen_end_expression(ctx);
    }
    else
    {
        codegen_end_expression(ctx);
        free_reg(get_reg_stack(), target_reg);
    }

    return inst_list;
}

static ListNode_t *codegen_expr_via_tree(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    return codegen_expr_tree_value(expr, inst_list, ctx, NULL);
}

ListNode_t *codegen_sign_extend32_to64(ListNode_t *inst_list, const char *src_reg32, const char *dst_reg64)
{
    assert(src_reg32 != NULL);
    assert(dst_reg64 != NULL);

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", src_reg32, dst_reg64);
    return add_inst(inst_list, buffer);
}

ListNode_t *codegen_zero_extend32_to64(ListNode_t *inst_list, const char *src_reg32, const char *dst_reg32)
{
    assert(src_reg32 != NULL);
    assert(dst_reg32 != NULL);

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", src_reg32, dst_reg32);
    return add_inst(inst_list, buffer);
}

int codegen_sizeof_type_reference(CodeGenContext *ctx, int type_tag, const char *type_id,
    struct RecordType *record_type, long long *size_out)
{
    return codegen_sizeof_type(ctx, type_tag, type_id, record_type, size_out, 0);
}

ListNode_t *codegen_pointer_deref_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg)
{
    if (expr == NULL || ctx == NULL || target_reg == NULL)
        return inst_list;

    struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr == NULL)
    {
        codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
        return inst_list;
    }

    expr_node_t *pointer_tree = build_expr_tree(pointer_expr);
    Register_t *addr_reg = codegen_try_get_reg(&inst_list, ctx, "pointer dereference");
    if (addr_reg == NULL)
    {
        free_expr_tree(pointer_tree);
        return inst_list;
    }

    inst_list = gencode_expr_tree(pointer_tree, inst_list, ctx, addr_reg);
    free_expr_tree(pointer_tree);

    long long load_size = expr_effective_size_bytes(expr);
    char buffer[64];
    if (load_size == 1)
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    else if (load_size == 2)
    {
        const int is_signed = expr_is_signed_kgpctype(expr);
        snprintf(buffer, sizeof(buffer), "\t%s\t(%s), %s\n",
            is_signed ? "movswl" : "movzwl", addr_reg->bit_64, target_reg->bit_32);
    }
    else if (expr_uses_qword_kgpctype(expr))
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_64);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}

ListNode_t *codegen_addressof_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg)
{
    if (expr == NULL || ctx == NULL || target_reg == NULL)
        return inst_list;

    struct Expression *inner = expr->expr_data.addr_data.expr;
    if (inner == NULL)
    {
        codegen_report_error(ctx, "ERROR: Address-of operator missing operand.");
        return inst_list;
    }

    char buffer[64];
    if (inner->type == EXPR_VAR_ID)
    {
        StackNode_t *var_node = find_label(inner->expr_data.id);
        if (var_node != NULL)
        {
            if (var_node->is_static)
            {
                const char *label = (var_node->static_label != NULL) ?
                    var_node->static_label : var_node->label;
                snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                    label, target_reg->bit_64);
            }
            else if (var_node->is_reference)
            {
                /* For var/out/constref parameters, the stack slot already contains
                 * a pointer to the actual variable. So @param returns the VALUE
                 * stored in the slot (the pointer), not the address of the slot. */
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    var_node->offset, target_reg->bit_64);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    var_node->offset, target_reg->bit_64);
            }
            return add_inst(inst_list, buffer);
        }
        else if (nonlocal_flag() == 1)
        {
            int offset = 0;
            inst_list = codegen_get_nonlocal(inst_list, inner->expr_data.id, &offset);
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%s), %s\n", offset, current_non_local_reg64(), target_reg->bit_64);
            return add_inst(inst_list, buffer);
        }

        codegen_report_error(ctx,
            "ERROR: Address-of non-local variables is unsupported without -non-local flag.");
        return inst_list;
    }
    else if (inner->type == EXPR_ARRAY_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_array_element_address(inner, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }
    else if (inner->type == EXPR_RECORD_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_record_field_address(inner, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }
    else if (inner->type == EXPR_POINTER_DEREF)
    {
        struct Expression *pointer_expr = inner->expr_data.pointer_deref_data.pointer_expr;
        if (pointer_expr == NULL)
        {
            codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
            return inst_list;
        }

        Register_t *addr_reg = NULL;
        inst_list = codegen_expr_tree_value(pointer_expr, inst_list, ctx, &addr_reg);
        if (addr_reg == NULL)
            return inst_list;

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    codegen_report_error(ctx, "ERROR: Unsupported operand for address-of operator.");
    return inst_list;
}

ListNode_t *codegen_record_field_address(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || ctx == NULL || out_reg == NULL)
        return inst_list;

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    if (record_expr == NULL)
        return inst_list;

    /* Check if this is a class field access. Classes are pointers, so we need an extra dereference.
     * However, parameters are already passed as pointers, so we shouldn't dereference them. */
    int is_class_field = (record_expr->record_type != NULL && 
                          record_type_is_class(record_expr->record_type));

    int is_class_type_ref = 0;
    const char *class_type_label = NULL;
    if (is_class_field && record_expr->type == EXPR_VAR_ID &&
        record_expr->expr_data.id != NULL && ctx->symtab != NULL)
    {
        HashNode_t *symbol = NULL;
        if (FindIdent(&symbol, ctx->symtab, record_expr->expr_data.id) >= 0 &&
            symbol != NULL && symbol->hash_type == HASHTYPE_TYPE)
        {
            is_class_type_ref = 1;
            if (record_expr->record_type != NULL && record_expr->record_type->type_id != NULL)
                class_type_label = record_expr->record_type->type_id;
            else
                class_type_label = record_expr->expr_data.id;
        }
    }
    
    /* Check if the record expression is a parameter (already a pointer) */
    int is_parameter = 0;
    if (is_class_field && record_expr->type == EXPR_VAR_ID)
    {
        StackNode_t *var_node = find_label(record_expr->expr_data.id);
        if (var_node != NULL && var_node->is_reference)
            is_parameter = 1;
        
        /* Also check symbol table */
        if (!is_parameter && ctx->symtab != NULL)
        {
            HashNode_t *symbol = NULL;
            if (FindIdent(&symbol, ctx->symtab, record_expr->expr_data.id) >= 0 && 
                symbol != NULL && symbol->is_var_parameter)
                is_parameter = 1;
        }
    }

    Register_t *addr_reg = NULL;
    if (is_class_type_ref && class_type_label != NULL)
    {
        addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
            addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
        {
            codegen_report_error(ctx,
                "ERROR: Unable to allocate register for class var access.");
            return inst_list;
        }

        char buffer[96];
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s_CLASSVAR(%%rip), %s\n",
            class_type_label, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        inst_list = codegen_address_for_expr(record_expr, inst_list, ctx, &addr_reg);
        if (addr_reg == NULL)
            return inst_list;
    }

    /* For class types that are NOT parameters, addr_reg points to the variable holding the pointer.
     * We need to load the pointer value to get the address of the instance.
     * Parameters are already pointers, so we don't need the extra dereference. */
    if (is_class_field && !is_parameter && !is_class_type_ref)
    {
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    long long offset = expr->expr_data.record_access_data.field_offset;
    if (offset != 0)
    {
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\taddq\t$%lld, %s\n", offset, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    *out_reg = addr_reg;
    return inst_list;
}

ListNode_t *codegen_record_access(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg)
{
    if (expr == NULL || ctx == NULL || target_reg == NULL)
        return inst_list;

    if (expr_has_type_tag(expr, RECORD_TYPE))
    {
        codegen_report_error(ctx, "ERROR: Record-valued expressions are unsupported in this context.");
        return inst_list;
    }

    Register_t *addr_reg = NULL;
    inst_list = codegen_record_field_address(expr, inst_list, ctx, &addr_reg);
    if (addr_reg == NULL)
        return inst_list;

    char buffer[64];
    long long field_size = codegen_record_field_effective_size(expr, ctx);
    if (expr_uses_qword_kgpctype(expr) || field_size == 8)
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_64);
    else if (field_size == 1 || expr_has_type_tag(expr, CHAR_TYPE))
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    else if (field_size == 2)
        snprintf(buffer, sizeof(buffer), "\tmovzwl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}

static ListNode_t *codegen_set_emit_single(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *dest_reg, Register_t *value_reg)
{
    if (dest_reg == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    char skip_label[18];
    gen_label(skip_label, sizeof(skip_label), ctx);

    snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", skip_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tcmpl\t$31, %s\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", skip_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tshll\t%%cl, %s\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\torl\t%s, %s\n", value_reg->bit_32, dest_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", skip_label);
    inst_list = add_inst(inst_list, buffer);
    return inst_list;
}

static ListNode_t *codegen_set_emit_range(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *dest_reg, Register_t *start_reg, Register_t *end_reg)
{
    if (dest_reg == NULL || start_reg == NULL || end_reg == NULL)
        return inst_list;

    Register_t *temp_reg = codegen_try_get_reg(&inst_list, ctx, "set range temp");
    if (temp_reg == NULL)
        return inst_list;

    char order_label[18];
    char loop_label[18];
    char done_label[18];
    char start_floor_label[18];
    char end_cap_label[18];
    gen_label(order_label, sizeof(order_label), ctx);
    gen_label(loop_label, sizeof(loop_label), ctx);
    gen_label(done_label, sizeof(done_label), ctx);
    gen_label(start_floor_label, sizeof(start_floor_label), ctx);
    gen_label(end_cap_label, sizeof(end_cap_label), ctx);

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, %s\n", end_reg->bit_32, start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", order_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", start_reg->bit_32, temp_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", end_reg->bit_32, start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", temp_reg->bit_32, end_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", order_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", end_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tcmpl\t$31, %s\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjge\t%s\n", start_floor_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t$0, %s\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", start_floor_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tcmpl\t$31, %s\n", end_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", end_cap_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t$31, %s\n", end_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", end_cap_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "%s:\n", loop_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, %s\n", end_reg->bit_32, start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", temp_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tshll\t%%cl, %s\n", temp_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\torl\t%s, %s\n", temp_reg->bit_32, dest_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, %s\n", end_reg->bit_32, start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tje\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tincl\t%s\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", loop_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", done_label);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), temp_reg);
    return inst_list;
}

ListNode_t *codegen_set_literal(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg, int force_char_set)
{
    if (expr == NULL)
        return inst_list;

    /* Check if this is a character set literal */
    int is_char_set = force_char_set || expr_is_char_set_ctx(expr, ctx);
    
    if (is_char_set)
    {
        /* Character sets need 32 bytes in memory, not a register */
        /* Allocate a temporary 32-byte buffer on the stack */
        StackNode_t *char_set_temp = codegen_alloc_record_temp(32);
        if (char_set_temp == NULL)
        {
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }
        
        /* Zero-initialize all 32 bytes */
        char buffer[128];
        for (int i = 0; i < 8; i++)
        {
            int offset = char_set_temp->offset - (i * 4);
            snprintf(buffer, sizeof(buffer), "\tmovl\t$0, -%d(%%rbp)\n", offset);
            inst_list = add_inst(inst_list, buffer);
        }
        
        /* Get address register for the set buffer */
        Register_t *addr_reg = codegen_try_get_reg(&inst_list, ctx, "char set addr");
        if (addr_reg == NULL)
        {
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }
        
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", 
            char_set_temp->offset, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        
        /* Now set each element in the character set */
        for (ListNode_t *cur = expr->expr_data.set_data.elements; cur != NULL; cur = cur->next)
        {
            struct SetElement *element = (struct SetElement *)cur->cur;
            if (element == NULL)
                continue;

            Register_t *value_reg = NULL;
            inst_list = codegen_expr_tree_value(element->lower, inst_list, ctx, &value_reg);
            if (codegen_had_error(ctx) || value_reg == NULL)
            {
                if (value_reg != NULL)
                    free_reg(get_reg_stack(), value_reg);
                free_reg(get_reg_stack(), addr_reg);
                if (out_reg != NULL)
                    *out_reg = NULL;
                return inst_list;
            }
            
            /* For character sets: Calculate dword index and bit index */
            Register_t *bit_reg = codegen_try_get_reg(&inst_list, ctx, "char set bit");
            Register_t *dword_reg = codegen_try_get_reg(&inst_list, ctx, "char set dword");
            if (bit_reg == NULL || dword_reg == NULL)
            {
                if (bit_reg != NULL)
                    free_reg(get_reg_stack(), bit_reg);
                if (dword_reg != NULL)
                    free_reg(get_reg_stack(), dword_reg);
                free_reg(get_reg_stack(), value_reg);
                free_reg(get_reg_stack(), addr_reg);
                if (out_reg != NULL)
                    *out_reg = NULL;
                return inst_list;
            }
            
            /* Save value for bit calculation */
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", value_reg->bit_32, bit_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            
            /* Calculate bit index: value & 31 */
            snprintf(buffer, sizeof(buffer), "\tandl\t$31, %s\n", bit_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            
            /* Calculate dword offset: (value >> 5) * 4 */
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", value_reg->bit_32, dword_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tshrl\t$5, %s\n", dword_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tshll\t$2, %s\n", dword_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            
            /* Load current dword value */
            snprintf(buffer, sizeof(buffer), "\tmovl\t(%s,%s,1), %s\n", 
                addr_reg->bit_64, dword_reg->bit_64, value_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            
            /* Create bit mask: 1 << bit_index */
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", bit_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", bit_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tshll\t%%cl, %s\n", bit_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            
            /* OR the bit into the dword */
            snprintf(buffer, sizeof(buffer), "\torl\t%s, %s\n", bit_reg->bit_32, value_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            
            /* Store back to memory */
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s,%s,1)\n", 
                value_reg->bit_32, addr_reg->bit_64, dword_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            free_reg(get_reg_stack(), dword_reg);
            free_reg(get_reg_stack(), bit_reg);
            free_reg(get_reg_stack(), value_reg);
            
            /* TODO: Handle ranges (element->upper != NULL) */
            if (element->upper != NULL)
            {
                codegen_report_error(ctx, "ERROR: Character set ranges not yet implemented.");
                free_reg(get_reg_stack(), addr_reg);
                if (out_reg != NULL)
                    *out_reg = NULL;
                return inst_list;
            }
        }
        
        /* Return the address register */
        if (out_reg != NULL)
            *out_reg = addr_reg;
        else
            free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    /* Regular 32-bit sets */
    if (expr->expr_data.set_data.is_constant)
    {
        Register_t *dest_reg = codegen_try_get_reg(&inst_list, ctx, "set literal");
        if (dest_reg == NULL)
        {
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }

        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%u, %s\n", expr->expr_data.set_data.bitmask,
            dest_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        if (out_reg != NULL)
            *out_reg = dest_reg;
        else
            free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    Register_t *dest_reg = codegen_try_get_reg(&inst_list, ctx, "set literal");
    if (dest_reg == NULL)
    {
        if (out_reg != NULL)
            *out_reg = NULL;
        return inst_list;
    }

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovl\t$0, %s\n", dest_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    for (ListNode_t *cur = expr->expr_data.set_data.elements; cur != NULL; cur = cur->next)
    {
        struct SetElement *element = (struct SetElement *)cur->cur;
        if (element == NULL)
            continue;

        Register_t *lower_reg = NULL;
        inst_list = codegen_expr_tree_value(element->lower, inst_list, ctx, &lower_reg);
        if (codegen_had_error(ctx) || lower_reg == NULL)
        {
            if (lower_reg != NULL)
                free_reg(get_reg_stack(), lower_reg);
            free_reg(get_reg_stack(), dest_reg);
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }

        Register_t *upper_reg = NULL;
        if (element->upper != NULL)
        {
            inst_list = codegen_expr_tree_value(element->upper, inst_list, ctx, &upper_reg);
            if (codegen_had_error(ctx) || upper_reg == NULL)
            {
                if (upper_reg != NULL)
                    free_reg(get_reg_stack(), upper_reg);
                free_reg(get_reg_stack(), lower_reg);
                free_reg(get_reg_stack(), dest_reg);
                if (out_reg != NULL)
                    *out_reg = NULL;
                return inst_list;
            }
        }

        if (element->upper == NULL)
            inst_list = codegen_set_emit_single(inst_list, ctx, dest_reg, lower_reg);
        else
            inst_list = codegen_set_emit_range(inst_list, ctx, dest_reg, lower_reg, upper_reg);

        if (codegen_had_error(ctx))
        {
            if (upper_reg != NULL)
                free_reg(get_reg_stack(), upper_reg);
            free_reg(get_reg_stack(), lower_reg);
            free_reg(get_reg_stack(), dest_reg);
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }

        if (upper_reg != NULL)
            free_reg(get_reg_stack(), upper_reg);
        free_reg(get_reg_stack(), lower_reg);
    }

    if (out_reg != NULL)
        *out_reg = dest_reg;
    else
        free_reg(get_reg_stack(), dest_reg);
    return inst_list;
}

static ListNode_t *codegen_set_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL)
    {
        if (out_reg != NULL)
            *out_reg = NULL;
        return inst_list;
    }

    if (expr->type == EXPR_SET)
        return codegen_set_literal(expr, inst_list, ctx, out_reg, 0);

    return codegen_expr_tree_value(expr, inst_list, ctx, out_reg);
}

static Register_t *codegen_clone_register_if_rcx(ListNode_t **inst_list, CodeGenContext *ctx,
    Register_t *reg, const char *label)
{
    if (reg == NULL || inst_list == NULL)
        return reg;

    if (strcmp(reg->bit_64, "%rcx") != 0)
        return reg;

    static const char *preferred_regs[] = { "%rax", "%r10", "%r11", "%r8", "%r9" };
    Register_t *replacement = NULL;
    for (size_t i = 0; i < sizeof(preferred_regs) / sizeof(preferred_regs[0]); ++i)
    {
        if (get_register_64bit(get_reg_stack(), (char *)preferred_regs[i], &replacement) == 0 && replacement != NULL)
            break;
        replacement = NULL;
    }

    if (replacement == NULL)
        replacement = codegen_try_get_reg(inst_list, ctx, label);

    if (replacement == NULL)
        return reg;

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", reg->bit_64, replacement->bit_64);
    *inst_list = add_inst(*inst_list, buffer);
    free_reg(get_reg_stack(), reg);
    return replacement;
}

static ListNode_t *codegen_char_set_address(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || out_reg == NULL)
        return inst_list;

    Register_t *addr_reg = NULL;
    if (codegen_expr_is_addressable(expr))
        inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
    else
        inst_list = codegen_set_expr(expr, inst_list, ctx, &addr_reg);

    if (codegen_had_error(ctx) || addr_reg == NULL)
    {
        *out_reg = NULL;
        return inst_list;
    }

    addr_reg = codegen_clone_register_if_rcx(&inst_list, ctx, addr_reg, "char set addr spill");
    *out_reg = addr_reg;
    return inst_list;
}

ListNode_t *codegen_expr(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(expr != NULL);
    assert(ctx != NULL);
    CODEGEN_DEBUG("DEBUG: Generating code for expression type %d\n", expr->type);

    if (expr_has_type_tag(expr, SET_TYPE))
    {
        Register_t *set_reg = NULL;
        inst_list = codegen_set_expr(expr, inst_list, ctx, &set_reg);
        if (codegen_had_error(ctx))
            return inst_list;
        if (set_reg != NULL)
            free_reg(get_reg_stack(), set_reg);
        return inst_list;
    }

    switch(expr->type) {
        case EXPR_VAR_ID:
            CODEGEN_DEBUG("DEBUG: Processing variable ID expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_RECORD_ACCESS:
            CODEGEN_DEBUG("DEBUG: Processing record access expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_ARRAY_ACCESS:
            CODEGEN_DEBUG("DEBUG: Processing array access expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_MULOP:
            CODEGEN_DEBUG("DEBUG: Processing mulop expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_INUM:
            CODEGEN_DEBUG("DEBUG: Processing integer constant expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_RNUM:
            CODEGEN_DEBUG("DEBUG: Processing real constant expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_BOOL:
            CODEGEN_DEBUG("DEBUG: Processing boolean constant expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_NIL:
            CODEGEN_DEBUG("DEBUG: Processing nil literal expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_SET:
            CODEGEN_DEBUG("DEBUG: Processing set literal expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_STRING:
            CODEGEN_DEBUG("DEBUG: Processing string literal expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_CHAR_CODE:
            CODEGEN_DEBUG("DEBUG: Processing character code literal expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_POINTER_DEREF:
            CODEGEN_DEBUG("DEBUG: Processing pointer dereference expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_RECORD_CONSTRUCTOR:
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
            if (addr_reg != NULL)
                free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
        case EXPR_ADDR:
            CODEGEN_DEBUG("DEBUG: Processing address-of expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_ADDR_OF_PROC:
            CODEGEN_DEBUG("DEBUG: Processing address-of-procedure expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_ARRAY_LITERAL:
            CODEGEN_DEBUG("DEBUG: Processing array literal expression\n");
            {
                Register_t *tmp_reg = NULL;
                inst_list = codegen_materialize_array_literal(expr, inst_list, ctx, &tmp_reg);
                if (tmp_reg != NULL)
                    free_reg(get_reg_stack(), tmp_reg);
            }
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_RELOP:
            CODEGEN_DEBUG("DEBUG: Processing relational operator expression\n");
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return codegen_simple_relop(expr, inst_list, ctx, NULL);
        case EXPR_ADDOP:
            CODEGEN_DEBUG("DEBUG: Processing addop expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_SIGN_TERM:
            CODEGEN_DEBUG("DEBUG: Processing sign term expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_FUNCTION_CALL:
            CODEGEN_DEBUG("DEBUG: Processing function call expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_TYPECAST:
            CODEGEN_DEBUG("DEBUG: Processing typecast expression\n");
            if (expr->expr_data.typecast_data.expr != NULL)
                inst_list = codegen_expr(expr->expr_data.typecast_data.expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_IS:
            CODEGEN_DEBUG("DEBUG: Processing RTTI is expression\n");
            inst_list = codegen_emit_is_expr(expr, inst_list, ctx, NULL);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_AS:
            CODEGEN_DEBUG("DEBUG: Processing RTTI as expression\n");
            if (expr->expr_data.as_data.expr != NULL)
            {
                Register_t *addr_reg = NULL;
                inst_list = codegen_address_for_expr(expr->expr_data.as_data.expr, inst_list, ctx, &addr_reg);
                if (addr_reg != NULL)
                {
                    inst_list = codegen_emit_class_cast_check_from_address(expr, inst_list, ctx, addr_reg);
                    free_reg(get_reg_stack(), addr_reg);
                }
            }
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        default:
            assert(0 && "Unsupported expression type");
            break;
    }
}

ListNode_t *codegen_expr_with_result(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(out_reg != NULL);
    
    /* Handle set expressions specially - they need codegen_set_expr for proper bitmask generation */
    if (expr != NULL && expr_has_type_tag(expr, SET_TYPE))
    {
        inst_list = codegen_set_expr(expr, inst_list, ctx, out_reg);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s (SET_TYPE path)\n", __func__);
        #endif
        return inst_list;
    }

    inst_list = codegen_expr_tree_value(expr, inst_list, ctx, out_reg);

    
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

static int codegen_resolve_is_array(struct Expression *array_expr, CodeGenContext *ctx,
    StackNode_t **out_stack_node)
{
    if (out_stack_node != NULL)
        *out_stack_node = NULL;
    if (array_expr == NULL || ctx == NULL)
        return 0;

    KgpcType *base_type = expr_get_kgpc_type(array_expr);
    int base_is_array = (array_expr->is_array_expr ||
        (base_type != NULL && kgpc_type_is_array(base_type)));
    if (!base_is_array && ctx->symtab != NULL && array_expr->type == EXPR_VAR_ID)
    {
        HashNode_t *array_node = NULL;
        if (FindIdent(&array_node, ctx->symtab, array_expr->expr_data.id) >= 0 &&
            array_node != NULL && hashnode_is_array(array_node))
        {
            base_is_array = 1;
        }
    }
    if (array_expr->type == EXPR_VAR_ID)
    {
        StackNode_t *stack_node = find_label(array_expr->expr_data.id);
        if (out_stack_node != NULL)
            *out_stack_node = stack_node;
        if (!base_is_array && stack_node != NULL && stack_node->is_array)
            base_is_array = 1;
    }

    return base_is_array;
}

static int codegen_get_indexable_element_size(struct Expression *array_expr,
    CodeGenContext *ctx, long long *out_size)
{
    assert(array_expr != NULL);
    assert(out_size != NULL);

    StackNode_t *array_stack_node = NULL;
    int base_is_array = codegen_resolve_is_array(array_expr, ctx, &array_stack_node);
    int base_is_string = (expr_has_type_tag(array_expr, STRING_TYPE) && !base_is_array);
    int base_is_pointer = (expr_has_type_tag(array_expr, POINTER_TYPE) && !base_is_array);
    long long element_size_ll = 1;

    if (base_is_string)
    {
        *out_size = 1;
        return 1;
    }

    if (base_is_pointer)
    {
        /* For pointers, get the size of what the pointer points to */
        if (codegen_sizeof_type(ctx, array_expr->pointer_subtype,
                array_expr->pointer_subtype_id,
                array_expr->record_type,
                &element_size_ll, 0) != 0 || element_size_ll <= 0)
        {
            /* Default to pointer size if we can't determine */
            element_size_ll = 8;
        }
        *out_size = element_size_ll;
        return 1;
    }

    element_size_ll = expr_get_array_element_size(array_expr, ctx);

    int need_element_size = 0;
    if (element_size_ll <= 0)
        need_element_size = 1;
    else if (array_expr->array_element_record_type != NULL)
        need_element_size = 1;
    else if (array_expr->array_element_type == RECORD_TYPE)
        need_element_size = 1;
    else if (array_expr->array_element_type == UNKNOWN_TYPE &&
        array_expr->array_element_type_id != NULL)
        need_element_size = 1;

    if (need_element_size)
    {
        if (codegen_sizeof_type(ctx, array_expr->array_element_type,
                array_expr->array_element_type_id,
                array_expr->array_element_record_type,
                &element_size_ll, 0) != 0 || element_size_ll <= 0)
        {
            codegen_report_error(ctx, "ERROR: Unable to determine element size for array access.");
            return 0;
        }
    }

    *out_size = element_size_ll;
    return 1;
}


ListNode_t *codegen_array_element_address(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg)
{
    assert(expr != NULL);
    assert(expr->type == EXPR_ARRAY_ACCESS);
    assert(ctx != NULL);
    assert(out_reg != NULL);

    struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
    struct Expression *index_expr = expr->expr_data.array_access_data.index_expr;

    if (array_expr == NULL)
    {
        codegen_report_error(ctx, "ERROR: Array access missing base expression.");
        return inst_list;
    }

    StackNode_t *array_stack_node = NULL;
    int base_is_array = codegen_resolve_is_array(array_expr, ctx, &array_stack_node);
    int base_is_string = (expr_has_type_tag(array_expr, STRING_TYPE) && !base_is_array);
    int base_is_pointer = (expr_has_type_tag(array_expr, POINTER_TYPE) && !base_is_array);

    if (!base_is_array && !base_is_string && !base_is_pointer)
    {
        codegen_report_error(ctx, "ERROR: Expression is not indexable as an array.");
        return inst_list;
    }

    Register_t *index_reg = NULL;
    inst_list = codegen_expr_with_result(index_expr, inst_list, ctx, &index_reg);
    if (codegen_had_error(ctx) || index_reg == NULL)
        return inst_list;

    Register_t *base_reg = NULL;
    if (base_is_string || base_is_pointer)
    {
        inst_list = codegen_expr_with_result(array_expr, inst_list, ctx, &base_reg);
        if (codegen_had_error(ctx) || base_reg == NULL)
        {
            free_reg(get_reg_stack(), index_reg);
            return inst_list;
        }
    }
    else
    {
        inst_list = codegen_address_for_expr(array_expr, inst_list, ctx, &base_reg);
        if (codegen_had_error(ctx) || base_reg == NULL)
        {
            free_reg(get_reg_stack(), index_reg);
            return inst_list;
        }
    }

    char buffer[128];

    if (!base_is_string && !base_is_pointer && array_expr->array_is_dynamic &&
        !(array_stack_node != NULL && array_stack_node->is_array && !array_stack_node->is_dynamic))
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", base_reg->bit_64, base_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    int lower_bound = base_is_pointer ? 0 : (base_is_string ? 1 : expr_get_array_lower_bound(array_expr));
    if (lower_bound > 0)
    {
        snprintf(buffer, sizeof(buffer), "\tsubl\t$%d, %s\n", lower_bound, index_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }
    else if (lower_bound < 0)
    {
        snprintf(buffer, sizeof(buffer), "\taddl\t$%d, %s\n", -lower_bound, index_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32, index_reg->bit_64);

    long long element_size_ll = 1;
    if (!codegen_get_indexable_element_size(array_expr, ctx, &element_size_ll))
    {
        free_reg(get_reg_stack(), base_reg);
        free_reg(get_reg_stack(), index_reg);
        return inst_list;
    }

    int element_size = (int)element_size_ll;
    static const int scaled_sizes[] = {1, 2, 4, 8};
    int can_scale = 0;
    for (size_t i = 0; i < sizeof(scaled_sizes) / sizeof(scaled_sizes[0]); ++i)
    {
        if (element_size == scaled_sizes[i])
        {
            can_scale = 1;
            break;
        }
    }

    if (can_scale)
    {
        snprintf(buffer, sizeof(buffer), "\tleaq\t(%s,%s,%d), %s\n",
            base_reg->bit_64, index_reg->bit_64, element_size, index_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        if (element_size != 1)
        {
            snprintf(buffer, sizeof(buffer), "\timulq\t$%d, %s\n", element_size, index_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }

        snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n", base_reg->bit_64, index_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    free_reg(get_reg_stack(), base_reg);
    *out_reg = index_reg;
    return inst_list;
}

ListNode_t *codegen_array_access(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg)
{
    assert(expr != NULL);
    assert(target_reg != NULL);

    Register_t *addr_reg = NULL;
    inst_list = codegen_array_element_address(expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
    long long element_size_ll = 4; /* default to 4-byte integer */
    if (array_expr != NULL &&
        !codegen_get_indexable_element_size(array_expr, ctx, &element_size_ll))
    {
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }
    int element_size = (int)element_size_ll;

    char buffer[100];
    if (expr_uses_qword_kgpctype(expr))
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        if (expr_has_type_tag(expr, CHAR_TYPE) || element_size == 1)
        {
            /* Byte-sized element - use zero-extend byte to long */
            snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        }
        else if (element_size == 2)
        {
            /* Word-sized element - use zero-extend word to long */
            snprintf(buffer, sizeof(buffer), "\tmovzwl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        }
        inst_list = add_inst(inst_list, buffer);
        if (expr_has_type_tag(expr, LONGINT_TYPE))
            inst_list = codegen_sign_extend32_to64(inst_list, target_reg->bit_32, target_reg->bit_64);
    }

    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}


static int invert_relop_type(int relop_kind)
{
    switch (relop_kind)
    {
        case EQ:
            return NE;
        case NE:
            return EQ;
        case LT:
            return GE;
        case LE:
            return GT;
        case GT:
            return LE;
        case GE:
            return LT;
        default:
            return relop_kind;
    }
}

/* Code generation for simple relops */
ListNode_t *codegen_simple_relop(struct Expression *expr, ListNode_t *inst_list,
                                CodeGenContext *ctx, int *relop_type)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(expr != NULL);
    assert(expr->type == EXPR_RELOP);
    assert(ctx != NULL);

    CODEGEN_DEBUG("DEBUG: Generating simple relop\n");

    struct Expression *left_expr = expr->expr_data.relop_data.left;
    struct Expression *right_expr = expr->expr_data.relop_data.right;
    int relop_kind = expr->expr_data.relop_data.type;

    if (relop_type != NULL)
        *relop_type = relop_kind;

    char buffer[128];
    if (relop_kind == NOT)
    {
        int inner_type = NE;
        inst_list = codegen_condition_expr(left_expr, inst_list, ctx, &inner_type);
        if (relop_type != NULL)
            *relop_type = invert_relop_type(inner_type);
        return inst_list;
    }

    if (relop_kind == IN && right_expr != NULL && expr_is_char_set_ctx(right_expr, ctx))
    {
        if (relop_type != NULL)
            *relop_type = NE;

        Register_t *left_reg = NULL;
        inst_list = codegen_expr_with_result(left_expr, inst_list, ctx, &left_reg);
        if (codegen_had_error(ctx) || left_reg == NULL)
            return inst_list;

        Register_t *set_addr_reg = NULL;
        inst_list = codegen_char_set_address(right_expr, inst_list, ctx, &set_addr_reg);
        if (codegen_had_error(ctx) || set_addr_reg == NULL)
        {
            free_reg(get_reg_stack(), left_reg);
            return inst_list;
        }

        Register_t *dword_reg = codegen_try_get_reg(&inst_list, ctx, "char set dword");
        Register_t *bit_mask_reg = codegen_try_get_reg(&inst_list, ctx, "char set bit mask");
        if (dword_reg == NULL || bit_mask_reg == NULL)
        {
            if (dword_reg != NULL) free_reg(get_reg_stack(), dword_reg);
            if (bit_mask_reg != NULL) free_reg(get_reg_stack(), bit_mask_reg);
            free_reg(get_reg_stack(), set_addr_reg);
            free_reg(get_reg_stack(), left_reg);
            return inst_list;
        }

        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", left_reg->bit_32, bit_mask_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tshrl\t$5, %s\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tshll\t$2, %s\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_sign_extend32_to64(inst_list, left_reg->bit_32, left_reg->bit_64);
        snprintf(buffer, sizeof(buffer), "\tmovl\t(%s,%s,1), %s\n",
            set_addr_reg->bit_64, left_reg->bit_64, dword_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tandl\t$31, %s\n", bit_mask_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", bit_mask_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", bit_mask_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tshll\t%%cl, %s\n", bit_mask_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", bit_mask_reg->bit_32, dword_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), bit_mask_reg);
        free_reg(get_reg_stack(), dword_reg);
        free_reg(get_reg_stack(), set_addr_reg);
        free_reg(get_reg_stack(), left_reg);
        return inst_list;
    }

    /* For floating-point comparisons, use expr_tree-based evaluation with spilling
     * to preserve the left operand across function calls that may occur when
     * evaluating the right operand. Without this, caller-saved registers like
     * %rax would be clobbered by subsequent function calls.
     */
    int left_is_real = (left_expr != NULL && expr_has_type_tag(left_expr, REAL_TYPE));
    int right_is_real = (right_expr != NULL && expr_has_type_tag(right_expr, REAL_TYPE));
    
    Register_t *left_reg = NULL;
    Register_t *right_reg = NULL;
    StackNode_t *left_spill = NULL;
    
    if (left_is_real || right_is_real)
    {
        /* Evaluate left operand using expr_tree which properly handles results */
        inst_list = codegen_expr_with_result(left_expr, inst_list, ctx, &left_reg);
        if (codegen_had_error(ctx) || left_reg == NULL)
            return inst_list;
        
        /* Spill left result to stack to preserve it across the right operand evaluation
         * which may involve function calls that clobber caller-saved registers */
        left_spill = add_l_t("relop_left_spill");
        if (left_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", 
                     left_reg->bit_64, left_spill->offset);
            inst_list = add_inst(inst_list, buffer);

            /* Free left_reg temporarily; we will get a fresh register after right eval */
            free_reg(get_reg_stack(), left_reg);
            left_reg = NULL;
        }
        
        /* Evaluate right operand */
        inst_list = codegen_expr_with_result(right_expr, inst_list, ctx, &right_reg);
        if (codegen_had_error(ctx) || right_reg == NULL)
        {
            if (left_reg != NULL)
                free_reg(get_reg_stack(), left_reg);
            return inst_list;
        }
        
        /* If we spilled the left operand, get a fresh register and reload it.
         * Otherwise, the original left_reg is still live. */
        if (left_spill != NULL)
        {
            left_reg = codegen_try_get_reg(&inst_list, ctx, "relop_left_reload");
            if (left_reg == NULL)
            {
                free_reg(get_reg_stack(), right_reg);
                return inst_list;
            }

            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", 
                     left_spill->offset, left_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    else
    {
        /* Non-floating-point comparisons: evaluate both operands into registers.
         * The right operand may contain function calls (e.g. Length(s)) that clobber
         * caller-saved registers, so preserve the left operand via a spill slot. */
        inst_list = codegen_expr_with_result(left_expr, inst_list, ctx, &left_reg);
        if (codegen_had_error(ctx) || left_reg == NULL)
            return inst_list;

        const int use_qword_spill = expression_uses_qword(left_expr) || expression_uses_qword(right_expr);
        StackNode_t *left_int_spill = add_l_t("relop_left_spill");
        if (left_int_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmov%c\t%s, -%d(%%rbp)\n",
                use_qword_spill ? 'q' : 'l',
                use_qword_spill ? left_reg->bit_64 : left_reg->bit_32,
                left_int_spill->offset);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), left_reg);
            left_reg = NULL;
        }

        inst_list = codegen_expr_with_result(right_expr, inst_list, ctx, &right_reg);
        if (codegen_had_error(ctx) || right_reg == NULL)
        {
            if (left_reg != NULL)
                free_reg(get_reg_stack(), left_reg);
            return inst_list;
        }

        if (left_int_spill != NULL)
        {
            left_reg = codegen_try_get_reg(&inst_list, ctx, "relop_left_reload");
            if (left_reg == NULL)
            {
                free_reg(get_reg_stack(), right_reg);
                return inst_list;
            }
            snprintf(buffer, sizeof(buffer), "\tmov%c\t-%d(%%rbp), %s\n",
                use_qword_spill ? 'q' : 'l',
                left_int_spill->offset,
                use_qword_spill ? left_reg->bit_64 : left_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    if (left_reg == NULL || right_reg == NULL)
        return inst_list;

    int left_is_string = (left_expr != NULL && expr_has_type_tag(left_expr, STRING_TYPE));
    int right_is_string = (right_expr != NULL && expr_has_type_tag(right_expr, STRING_TYPE));
    int left_is_char_ptr = expr_is_char_pointer(left_expr);
    int right_is_char_ptr = expr_is_char_pointer(right_expr);
    if ((left_is_string && right_is_string) ||
        (left_is_string && right_is_char_ptr) ||
        (right_is_string && left_is_char_ptr))
    {
        const char *lhs_arg = current_arg_reg64(0);
        const char *rhs_arg = current_arg_reg64(1);
        if (lhs_arg == NULL || rhs_arg == NULL)
        {
            free_reg(get_reg_stack(), right_reg);
            free_reg(get_reg_stack(), left_reg);
            if (relop_type != NULL)
                *relop_type = relop_kind;
            return inst_list;
        }

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", right_reg->bit_64, rhs_arg);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", left_reg->bit_64, lhs_arg);
        inst_list = add_inst(inst_list, buffer);

        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = add_inst(inst_list, "\tcall\tkgpc_string_compare\n");
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", RETURN_REG_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        free_arg_regs();

        free_reg(get_reg_stack(), right_reg);
        snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), left_reg);

        if (relop_type != NULL)
            *relop_type = relop_kind;
        return inst_list;
    }

    if (relop_kind == IN)
    {
        if (relop_type != NULL)
            *relop_type = NE;

        /* Check if this is a character set IN operation (32-byte set for values 0..255)
         * vs. a small integer set (4-byte bitmask for values 0..31).
         * 
         * Character sets require memory-based indexing (32-byte array).
         * Small integer sets can use simple btl instruction on a 32-bit register.
         */
        HashNode_t *right_const_set = NULL;
        int right_is_char_set = 0; /* Only set to 1 if actually a char set */
        
        if (right_expr != NULL && right_expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *node = NULL;
            if (FindIdent(&node, ctx->symtab, right_expr->expr_data.id) >= 0 && node != NULL &&
                node->hash_type == HASHTYPE_CONST && node->const_set_value != NULL &&
                node->const_set_size > 0)
            {
                right_const_set = node;
                /* Check if this is a char set (32 bytes) or small set (4 bytes) */
                right_is_char_set = (node->const_set_size > 4);
            }
        }
        
        /* For set expressions (literals and variables), check if they are char sets */
        if (!right_is_char_set && right_expr != NULL)
        {
            right_is_char_set = expr_is_char_set_ctx(right_expr, ctx);
        }

        if (right_is_char_set)
        {
            /* For character sets: right operand is 32-byte array, left is char value (0-255)
             * Algorithm:
             * 1. dword_index = value / 32  (which of 8 dwords)
             * 2. bit_index = value % 32    (which bit in that dword)
             * 3. Load the appropriate dword from set variable
             * 4. Test the appropriate bit
             */

            /* Get address of the set variable */
            Register_t *set_addr_reg = NULL;
            if (right_const_set != NULL && right_const_set->const_set_value != NULL)
            {
                inst_list = codegen_emit_const_set_rodata(right_const_set, inst_list, ctx);
                if (codegen_had_error(ctx) || right_const_set->const_set_label == NULL)
                {
                    free_reg(get_reg_stack(), left_reg);
                    free_reg(get_reg_stack(), right_reg);
                    return inst_list;
                }
                set_addr_reg = codegen_try_get_reg(&inst_list, ctx, "char set const addr");
                if (set_addr_reg == NULL)
                {
                    free_reg(get_reg_stack(), left_reg);
                    free_reg(get_reg_stack(), right_reg);
                    return inst_list;
                }
                char buffer_addr[96];
                snprintf(buffer_addr, sizeof(buffer_addr), "\tleaq\t%s(%%rip), %s\n",
                    right_const_set->const_set_label, set_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer_addr);
            }
            else
            {
                inst_list = codegen_char_set_address(right_expr, inst_list, ctx, &set_addr_reg);
                if (codegen_had_error(ctx) || set_addr_reg == NULL)
                {
                    if (set_addr_reg != NULL)
                        free_reg(get_reg_stack(), set_addr_reg);
                    free_reg(get_reg_stack(), left_reg);
                    free_reg(get_reg_stack(), right_reg);
                    return inst_list;
                }
            }

            /* Calculate dword index: value / 32 (shift right by 5) */
            /* Calculate bit index: value % 32 (mask with 31) */
            Register_t *bit_mask_reg = codegen_try_get_reg(&inst_list, ctx, "char set bit mask");
            if (bit_mask_reg == NULL)
            {
                free_reg(get_reg_stack(), set_addr_reg);
                free_reg(get_reg_stack(), left_reg);
                free_reg(get_reg_stack(), right_reg);
                return inst_list;
            }

            /* Save the value for bit index calculation */
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", left_reg->bit_32, bit_mask_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);

            /* Calculate dword index in left_reg: value >> 5 */
            snprintf(buffer, sizeof(buffer), "\tshrl\t$5, %s\n", left_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);

            /* Multiply by 4 to get byte offset */
            snprintf(buffer, sizeof(buffer), "\tshll\t$2, %s\n", left_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);

            /* Load the appropriate dword: right_reg = [set_addr + offset] */
            snprintf(buffer, sizeof(buffer), "\tmovl\t(%s,%s,1), %s\n",
                set_addr_reg->bit_64, left_reg->bit_64, right_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);

            /* Calculate bit index: value & 31 */
            snprintf(buffer, sizeof(buffer), "\tandl\t$31, %s\n", bit_mask_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);

            /* Create bit mask: 1 << bit_index */
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", bit_mask_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", bit_mask_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tshll\t%%cl, %s\n", bit_mask_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);

            /* Test the bit */
            snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", bit_mask_reg->bit_32, right_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);

            free_reg(get_reg_stack(), bit_mask_reg);
            free_reg(get_reg_stack(), set_addr_reg);
            free_reg(get_reg_stack(), left_reg);
            free_reg(get_reg_stack(), right_reg);
            return inst_list;
        }

        /* Regular 32-bit sets */
        StackNode_t *set_spill = add_l_t("set_relop");
        if (set_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", right_reg->bit_32, set_spill->offset);
            inst_list = add_inst(inst_list, buffer);
        }

        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%ecx\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tshll\t%%cl, %s\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        if (set_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", set_spill->offset, right_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }

        snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", left_reg->bit_32, right_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), left_reg);
        return inst_list;
    }

    if (left_expr != NULL && expr_has_type_tag(left_expr, REAL_TYPE))
    {
        const char *left_name = register_name_for_type(left_reg, REAL_TYPE);
        const char *right_name = register_name_for_type(right_reg, REAL_TYPE);
        char true_label[32];
        char done_label[32];
        gen_label(true_label, sizeof(true_label), ctx);
        gen_label(done_label, sizeof(done_label), ctx);

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm1\n", left_name);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", right_name);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\txorl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        inst_list = add_inst(inst_list, "\tucomisd\t%xmm0, %xmm1\n");

        int relop_kind = expr->expr_data.relop_data.type;
        switch (relop_kind)
        {
            case EQ:
                snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tje\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                break;
            case NE:
                snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tjne\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                break;
            case LT:
                snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tjb\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                break;
            case LE:
                snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tjbe\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                break;
            case GT:
                snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tja\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                break;
            case GE:
                snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tjae\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                break;
            default:
                break;
        }

        snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", done_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "%s:\n", true_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "%s:\n", done_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        if (relop_type != NULL)
            *relop_type = NE;

        free_reg(get_reg_stack(), left_reg);
        return inst_list;
    }

    int use_qword = expression_uses_qword(left_expr) || expression_uses_qword(right_expr);
    /* When either operand is 64-bit (pointers, qword ints), make sure we also
     * use the 64-bit register names for the comparison. Using LONGINT_TYPE here
     * used to work when LongInt was 8 bytes, but now emits 32-bit names (rXXd)
     * which are invalid with a qword cmp suffix. */
    const char *left_name = use_qword
        ? left_reg->bit_64
        : register_name_for_expr(left_reg, left_expr);
    const char *right_name = use_qword
        ? right_reg->bit_64
        : register_name_for_expr(right_reg, right_expr);
    snprintf(buffer, sizeof(buffer), "\tcmp%c\t%s, %s\n", use_qword ? 'q' : 'l', right_name, left_name);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), right_reg);
    free_reg(get_reg_stack(), left_reg);

    CODEGEN_DEBUG("DEBUG: Simple relop generated\n");
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for relop expressions that need a value result (not just flags).
 * This is used by builtin write functions that need the boolean value in a register.
 * Returns the result in out_reg as 0 or 1.
 */
ListNode_t *codegen_relop_to_value(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || ctx == NULL || out_reg == NULL)
    {
        if (out_reg != NULL)
            *out_reg = NULL;
        return inst_list;
    }

    if (expr->type != EXPR_RELOP)
    {
        /* Not a relop - fall back to normal expression evaluation */
        return codegen_expr_tree_value(expr, inst_list, ctx, out_reg);
    }

    int relop_kind = expr->expr_data.relop_data.type;
    struct Expression *right_expr = expr->expr_data.relop_data.right;

    /* Check if this is an IN operation on a char set - requires special handling */
    int is_char_set_in = 0;
    if (relop_kind == IN && right_expr != NULL)
    {
        is_char_set_in = expr_is_char_set_ctx(right_expr, ctx);
        /* Also check for char set constants via variable reference */
        if (!is_char_set_in && right_expr->type == EXPR_VAR_ID && ctx->symtab != NULL)
        {
            HashNode_t *node = NULL;
            if (FindIdent(&node, ctx->symtab, right_expr->expr_data.id) >= 0 && node != NULL &&
                node->hash_type == HASHTYPE_CONST && node->const_set_value != NULL &&
                node->const_set_size > 4) /* 32-byte char set */
            {
                is_char_set_in = 1;
            }
        }
    }

    if (!is_char_set_in)
    {
        /* For non-char-set operations, the expression tree approach works */
        return codegen_expr_tree_value(expr, inst_list, ctx, out_reg);
    }

    /* For char set IN operations, we need to use codegen_simple_relop
     * which properly handles the 32-byte set, then convert flags to value */
    
    int relop_type = 0;
    inst_list = codegen_simple_relop(expr, inst_list, ctx, &relop_type);
    
    /* codegen_simple_relop sets CPU flags. We need to convert to a 0/1 value.
     * The relop_type tells us what condition was tested:
     * - NE means "not equal" - we want setnz to get 1 when flag is set
     */
    Register_t *result_reg = codegen_try_get_reg(&inst_list, ctx, "relop result");
    if (result_reg == NULL)
    {
        *out_reg = NULL;
        return inst_list;
    }

    char buffer[64];
    const char *set_instr = "setnz"; /* Default for set membership */
    
    /* Convert flags to value based on relop type */
    switch (relop_type)
    {
        case EQ:
            set_instr = "sete";
            break;
        case NE:
            set_instr = "setne";
            break;
        case LT:
            set_instr = "setl";
            break;
        case LE:
            set_instr = "setle";
            break;
        case GT:
            set_instr = "setg";
            break;
        case GE:
            set_instr = "setge";
            break;
        default:
            set_instr = "setnz";
            break;
    }

    const char *reg8 = codegen_register_name8(result_reg);
    if (reg8 != NULL)
    {
        snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", set_instr, reg8);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", reg8, result_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }

    *out_reg = result_reg;
    return inst_list;
}

/* Code generation for non-local variable access */
ListNode_t *codegen_get_nonlocal(ListNode_t *inst_list, char *var_id, int *offset)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    CODEGEN_DEBUG("DEBUG: Generating non-local access for %s\n", var_id);

    assert(inst_list != NULL);
    assert(var_id != NULL);
    assert(offset != NULL);

    char buffer[100];
    StackNode_t *var = find_label(var_id);

    if(var == NULL) {
        fprintf(stderr, "ERROR: Could not find non-local variable %s\n", var_id);
        exit(1);
    }

    *offset = var->offset;
    snprintf(buffer, 100, "\tmovq\t-8(%%rbp), %s\n", current_non_local_reg64());
    inst_list = add_inst(inst_list, buffer);

    CODEGEN_DEBUG("DEBUG: Non-local access generated\n");
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for passing arguments */
ListNode_t *codegen_pass_arguments(ListNode_t *args, ListNode_t *inst_list,
    CodeGenContext *ctx, struct KgpcType *proc_type, const char *procedure_name, int arg_start_index)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    int arg_num;
    Register_t *top_reg;
    char buffer[128];
    const char *arg_reg_char;
    expr_node_t *expr_tree;

    assert(ctx != NULL);

    ListNode_t *formal_args = NULL;
    if(proc_type != NULL && proc_type->kind == TYPE_KIND_PROCEDURE)
    {
        /* Get formal parameters from the KgpcType.
         * This avoids use-after-free bugs by not relying on HashNode pointers
         * that may point to freed memory after PopScope. */
        formal_args = proc_type->info.proc_info.params;
        CODEGEN_DEBUG("DEBUG: Using formal_args from KgpcType: %p\n", formal_args);
    }
    if (formal_args != NULL)
    {
        int formal_count = ListLength(formal_args);
        int actual_count = ListLength(args);
        if (formal_count == actual_count + 1)
        {
            Tree_t *first_decl = (Tree_t *)formal_args->cur;
            if (first_decl != NULL && first_decl->type == TREE_VAR_DECL)
            {
                ListNode_t *ids = first_decl->tree_data.var_decl_data.ids;
                const char *first_id = (ids != NULL) ? (const char *)ids->cur : NULL;
                if (first_id != NULL && pascal_identifier_equals(first_id, "Self"))
                    formal_args = formal_args->next;
            }
        }
    }
    
    /* CRITICAL VALIDATION: Ensure formal_args is either NULL or properly structured.
     * This catches any remaining cases of corrupted list pointers. */
    if (formal_args != NULL)
    {
        /* Basic sanity check: formal_args should have a valid list type.
         * This catches cases where formal_args contains garbage data. */
        if (formal_args->type != LIST_TREE && formal_args->type != LIST_UNSPECIFIED)
        {
            codegen_report_error(ctx,
                "FATAL: Internal compiler error - corrupted formal_args list (invalid type %d). "
                "This may indicate a bug in the semantic checker or memory corruption.",
                formal_args->type);
            return inst_list;
        }
    }

    enum {
        ARG_CLASS_INT = 0,
        ARG_CLASS_SSE = 1
    };


    int total_args = 0;
    for (ListNode_t *cur = args; cur != NULL; cur = cur->next)
        ++total_args;

    ArgInfo *arg_infos = NULL;
    const int max_int_regs = kgpc_max_int_arg_regs();
    const int max_sse_regs = kgpc_max_sse_arg_regs();
    int stack_slot_count = 0;
    int is_external_c_function = 0;
    if (total_args > 0)
    {
        arg_infos = (ArgInfo *)calloc((size_t)total_args, sizeof(ArgInfo));
        if (arg_infos == NULL)
        {
            fprintf(stderr, "ERROR: Failed to allocate argument metadata.\n");
            exit(1);
        }
    }

    if (ctx != NULL)
        ctx->pending_stack_arg_bytes = 0;

    if (arg_start_index < 0)
        arg_start_index = 0;

    if (proc_type != NULL && proc_type->kind == TYPE_KIND_PROCEDURE &&
        proc_type->info.proc_info.definition != NULL)
    {
        Tree_t *def = proc_type->info.proc_info.definition;
        if (def->type == TREE_SUBPROGRAM || def->type == TREE_SUBPROGRAM_PROC || def->type == TREE_SUBPROGRAM_FUNC)
            is_external_c_function = def->tree_data.subprogram_data.cname_flag;
    }

    arg_num = 0;
    while(args != NULL)
    {
        CODEGEN_DEBUG("DEBUG: In codegen_pass_arguments loop, arg_num = %d\n", arg_num);
        struct Expression *arg_expr = (struct Expression *)args->cur;
        
        /* Validate argument expression */
        if (arg_expr == NULL)
        {
            const char *proc_name = procedure_name ? procedure_name : "(unknown)";
            codegen_report_error(ctx,
                "ERROR: NULL argument expression in call to %s at argument position %d",
                proc_name, arg_num);
            if (arg_infos != NULL)
                free(arg_infos);
            return inst_list;
        }
        
        CODEGEN_DEBUG("DEBUG: arg_expr at %p, type %d\n", arg_expr, arg_expr->type);

        Tree_t *formal_arg_decl = NULL;
        if(formal_args != NULL)
        {
            /* CRITICAL VALIDATION: Before dereferencing formal_args, verify it's not corrupted.
             * On Cygwin/MSYS, corrupted list nodes can cause segfaults when accessing ->cur.
             * We check the list type to detect garbage values early. */
            if (formal_args->type != LIST_TREE && formal_args->type != LIST_UNSPECIFIED)
            {
                const char *proc_name = "(unknown)";
                codegen_report_error(ctx,
                    "FATAL: Internal compiler error - corrupted formal_args list node (type=%d) at argument %d for procedure %s. "
                    "This indicates memory corruption or an improperly initialized list.",
                    formal_args->type, arg_num, proc_name);
                if (arg_infos != NULL)
                    free(arg_infos);
                return inst_list;
            }
            formal_arg_decl = (Tree_t *)formal_args->cur;
        }

        int is_self_param = 0;
        if (formal_arg_decl != NULL && formal_arg_decl->type == TREE_VAR_DECL)
        {
            ListNode_t *ids = formal_arg_decl->tree_data.var_decl_data.ids;
            const char *formal_id = (ids != NULL) ? (const char *)ids->cur : NULL;
            if (formal_id != NULL && pascal_identifier_equals(formal_id, "Self"))
                is_self_param = 1;
        }

        int is_var_param = (formal_arg_decl != NULL &&
            formal_arg_decl->tree_data.var_decl_data.is_var_param);
        if (is_self_param && codegen_self_param_is_class(formal_arg_decl, ctx))
            is_var_param = 0;
        int is_array_param = (formal_arg_decl != NULL && formal_arg_decl->type == TREE_ARR_DECL);
        int formal_is_open_array = formal_decl_is_open_array(formal_arg_decl);
        int formal_is_dynarray = codegen_formal_is_dynamic_array(formal_arg_decl, ctx->symtab);
        
        /* Also check if we're passing a static array argument (even if not declared as var param) */
        int is_array_arg = (arg_expr != NULL && arg_expr->is_array_expr && !arg_expr->array_is_dynamic);
        int treat_self_by_value = 0;

        int expected_type = codegen_param_expected_type(formal_arg_decl, ctx->symtab);
        if (expected_type == UNKNOWN_TYPE && procedure_name != NULL)
            expected_type = codegen_expected_type_for_builtin(procedure_name);
        if (expected_type == UNKNOWN_TYPE && procedure_name != NULL &&
            pascal_identifier_equals(procedure_name, "Sqr"))
        {
            int arg_type = expr_get_type_tag(arg_expr);
            if (arg_type == REAL_TYPE)
                expected_type = REAL_TYPE;
            else if (arg_type == LONGINT_TYPE)
                expected_type = LONGINT_TYPE;
            else if (arg_type == INT_TYPE)
                expected_type = INT_TYPE;
        }
        if (procedure_name != NULL &&
            pascal_identifier_equals(procedure_name, "Random"))
        {
            int arg_type = expr_get_type_tag(arg_expr);
            if (arg_type == REAL_TYPE)
                expected_type = REAL_TYPE;
            else if (arg_type == LONGINT_TYPE)
                expected_type = LONGINT_TYPE;
            else if (arg_type == INT_TYPE)
                expected_type = INT_TYPE;
            else if (expected_type == UNKNOWN_TYPE)
                expected_type = LONGINT_TYPE;
        }
        if (is_var_param && arg_num == 0 && arg_expr != NULL &&
            !codegen_expr_is_addressable(arg_expr))
        {
            const char *formal_id = NULL;
            if (formal_arg_decl != NULL && formal_arg_decl->type == TREE_VAR_DECL)
            {
                ListNode_t *ids = formal_arg_decl->tree_data.var_decl_data.ids;
                formal_id = (ids != NULL) ? (const char *)ids->cur : NULL;
            }
            if (formal_id != NULL && pascal_identifier_equals(formal_id, "Self"))
            {
                treat_self_by_value = 1;
                is_var_param = 0;
            }
        }

        int is_pointer_like = (is_var_param || is_array_param || is_array_arg || treat_self_by_value || formal_is_dynarray);

        if (arg_infos != NULL)
        {
            arg_infos[arg_num].expected_type = expected_type;
            arg_infos[arg_num].is_pointer_like = is_pointer_like;
            arg_infos[arg_num].assigned_class = ARG_CLASS_INT;
            arg_infos[arg_num].assigned_index = -1;
        }

        if (formal_is_open_array && is_array_arg)
        {
            long long element_count = codegen_static_array_length(arg_expr);
            if (element_count < 0)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to determine length for open array argument.");
                return inst_list;
            }

            StackNode_t *desc_slot = codegen_alloc_temp_bytes("openarr_desc",
                2 * CODEGEN_POINTER_SIZE_BYTES);
            if (desc_slot == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Failed to allocate descriptor storage for open array argument.");
                return inst_list;
            }

            if (!codegen_expr_is_addressable(arg_expr))
            {
                codegen_report_error(ctx,
                    "ERROR: Unsupported expression type for open array argument.");
                return inst_list;
            }

            Register_t *data_addr_reg = NULL;
            inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &data_addr_reg);
            if (codegen_had_error(ctx) || data_addr_reg == NULL)
                return inst_list;

            Register_t *desc_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (desc_addr_reg == NULL)
            {
                free_reg(get_reg_stack(), data_addr_reg);
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for open array descriptor.");
                return inst_list;
            }

            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                desc_slot->offset, desc_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                data_addr_reg->bit_64, desc_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), data_addr_reg);

            snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, 8(%s)\n",
                element_count, desc_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            StackNode_t *arg_spill = add_l_t("arg_eval");
            if (arg_spill != NULL && arg_infos != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                    desc_addr_reg->bit_64, arg_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), desc_addr_reg);
                
                arg_infos[arg_num].reg = NULL;
                arg_infos[arg_num].spill = arg_spill;
                arg_infos[arg_num].expr = arg_expr;
            }
            else if (arg_infos != NULL)
            {
                arginfo_assign_register(&arg_infos[arg_num], desc_addr_reg, arg_expr);
            }
        }
        else if (formal_is_open_array && arg_expr != NULL && arg_expr->type == EXPR_STRING)
        {
            /* Handle string literal passed to open array of Char parameter.
             * Create a descriptor: (pointer to string data, element count).
             * The string is placed in read-only section. */
            const char *str_data = arg_expr->expr_data.string;
            int str_len = (str_data != NULL) ? (int)strlen(str_data) : 0;

            const char *readonly_section = codegen_readonly_section_directive();
            char label[64];
            snprintf(label, sizeof(label), ".LC%d", ctx->write_label_counter++);

            char escaped_str[CODEGEN_MAX_INST_BUF];
            escape_string(escaped_str, str_data ? str_data : "", sizeof(escaped_str));
            snprintf(buffer, sizeof(buffer), "%s\n%s:\n\t.string \"%s\"\n\t.text\n",
                     readonly_section, label, escaped_str);
            inst_list = add_inst(inst_list, buffer);

            StackNode_t *desc_slot = codegen_alloc_temp_bytes("str_arr_desc",
                2 * CODEGEN_POINTER_SIZE_BYTES);
            if (desc_slot == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate descriptor for string literal to open array.");
                return inst_list;
            }

            /* Get a register to hold the string address temporarily */
            Register_t *data_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (data_addr_reg == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for string literal address.");
                return inst_list;
            }

            /* Get descriptor address register */
            Register_t *desc_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (desc_addr_reg == NULL)
            {
                free_reg(get_reg_stack(), data_addr_reg);
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for open array descriptor.");
                return inst_list;
            }

            /* Load descriptor slot address */
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                desc_slot->offset, desc_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            /* Load string address */
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                     label, data_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            /* Store string pointer at descriptor[0] */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                data_addr_reg->bit_64, desc_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), data_addr_reg);

            /* Store element count at descriptor[1] (offset 8) */
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, 8(%s)\n",
                str_len, desc_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            StackNode_t *arg_spill = add_l_t("arg_eval");
            if (arg_spill != NULL && arg_infos != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                    desc_addr_reg->bit_64, arg_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), desc_addr_reg);

                arg_infos[arg_num].reg = NULL;
                arg_infos[arg_num].spill = arg_spill;
                arg_infos[arg_num].expr = arg_expr;
            }
            else if (arg_infos != NULL)
            {
                arginfo_assign_register(&arg_infos[arg_num], desc_addr_reg, arg_expr);
            }
        }
        /* Handle string literal passed to ShortString parameter.
         * ShortStrings use Pascal format: length byte at index 0, followed by string data.
         * We need to convert the C string literal to a ShortString in a stack buffer. */
        else if (expected_type == SHORTSTRING_TYPE && arg_expr != NULL && arg_expr->type == EXPR_STRING)
        {
            const char *str_data = arg_expr->expr_data.string;
            int str_len = (str_data != NULL) ? (int)strlen(str_data) : 0;
            if (str_len > 255) str_len = 255;  /* ShortString max length */
            
            /* Allocate 256 bytes on stack for ShortString buffer */
            StackNode_t *shortstr_buf = codegen_alloc_temp_bytes("shortstr_arg", 256);
            if (shortstr_buf == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate ShortString buffer for argument.");
                if (arg_infos != NULL) free(arg_infos);
                return inst_list;
            }
            
            /* Put string literal in rodata section */
            const char *readonly_section = codegen_readonly_section_directive();
            char label[64];
            snprintf(label, sizeof(label), ".LC%d", ctx->write_label_counter++);
            
            char escaped_str[CODEGEN_MAX_INST_BUF];
            escape_string(escaped_str, str_data ? str_data : "", sizeof(escaped_str));
            snprintf(buffer, sizeof(buffer), "%s\n%s:\n\t.string \"%s\"\n\t.text\n",
                     readonly_section, label, escaped_str);
            inst_list = add_inst(inst_list, buffer);
            
            /* Get register for buffer address */
            Register_t *buf_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (buf_addr_reg == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for ShortString buffer.");
                if (arg_infos != NULL) free(arg_infos);
                return inst_list;
            }
            
            /* Get register for string literal address */
            Register_t *str_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (str_addr_reg == NULL)
            {
                free_reg(get_reg_stack(), buf_addr_reg);
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for string literal address.");
                if (arg_infos != NULL) free(arg_infos);
                return inst_list;
            }
            
            /* Load buffer address */
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                shortstr_buf->offset, buf_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            /* Load string literal address */
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                label, str_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            /* Call kgpc_string_to_shortstring(dest, src, max_len) */
            if (codegen_target_is_windows())
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", buf_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", str_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                inst_list = add_inst(inst_list, "\tmovl\t$256, %r8d\n");
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", buf_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", str_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                inst_list = add_inst(inst_list, "\tmovl\t$256, %edx\n");
            }
            
            free_reg(get_reg_stack(), str_addr_reg);
            
            inst_list = add_inst(inst_list, "\tmovl\t$0, %eax\n");
            inst_list = add_inst(inst_list, "\tcall\tkgpc_string_to_shortstring\n");
            
            /* Reload buffer address after the call (it may have been clobbered) */
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                shortstr_buf->offset, buf_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            /* Save buffer address to arg_infos for later use in argument passing */
            StackNode_t *arg_spill = add_l_t("arg_eval");
            if (arg_spill != NULL && arg_infos != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                    buf_addr_reg->bit_64, arg_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), buf_addr_reg);
                
                arg_infos[arg_num].reg = NULL;
                arg_infos[arg_num].spill = arg_spill;
                arg_infos[arg_num].expr = arg_expr;
                arg_infos[arg_num].is_pointer_like = 1;  /* Pass as pointer */
            }
            else if (arg_infos != NULL)
            {
                arginfo_assign_register(&arg_infos[arg_num], buf_addr_reg, arg_expr);
                arg_infos[arg_num].is_pointer_like = 1;  /* Pass as pointer */
            }
        }
        else if(is_var_param || is_array_param || is_array_arg)
        {
            Register_t *addr_reg = NULL;
            if (arg_expr != NULL && arg_expr->type == EXPR_ARRAY_LITERAL)
            {
                inst_list = codegen_materialize_array_literal(arg_expr, inst_list, ctx, &addr_reg);
            }
            else
            {
                if (!codegen_expr_is_addressable(arg_expr))
                {
                    codegen_report_error(ctx,
                        "ERROR: Unsupported expression type for var parameter.");
                    return inst_list;
                }
                inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &addr_reg);
                
                /* BUGFIX: For TRUE var parameters of class types, we pass the ADDRESS of the variable itself,
                 * not the value it contains. This allows the callee to update the variable (e.g., FreeAndNil).
                 * 
                 * However, for class methods, Self (first parameter) needs to be dereferenced to pass the
                 * instance pointer, even though it's technically a var parameter internally. */
                 
                if (getenv("KGPC_DEBUG_CODEGEN") != NULL) {
                    fprintf(stderr, "[CodeGen] Checking var param arg %d: expr=%p, type=%d, record_type=%p\n", 
                        arg_num, arg_expr, arg_expr ? arg_expr->type : -1, arg_expr ? arg_expr->record_type : NULL);
                }

                if (addr_reg != NULL && arg_expr != NULL && arg_expr->type != EXPR_AS &&
                    arg_expr->record_type != NULL && record_type_is_class(arg_expr->record_type))
                {
                    int is_class_method = 0;
                    const char *mangled_name_hint = (procedure_name != NULL) ? procedure_name : "";

                    /* Detect if this is a class method by checking for __ in the mangled name.
                     * Class methods have mangled names like TClassName__MethodName. */
                    if (strstr(mangled_name_hint, "__") != NULL)
                        is_class_method = 1;

                    /* Check if the argument expression is itself a var parameter variable.
                     * If so, codegen_address_for_expr already loaded the instance pointer via movq,
                     * so we should NOT dereference again. */
                    int arg_is_var_param = 0;
                    if (arg_expr->type == EXPR_VAR_ID && ctx->symtab != NULL)
                    {
                        HashNode_t *arg_symbol = NULL;
                        if (FindIdent(&arg_symbol, ctx->symtab, arg_expr->expr_data.id) >= 0 &&
                            arg_symbol != NULL && arg_symbol->is_var_parameter)
                        {
                            arg_is_var_param = 1;
                        }
                    }

                    /* For class methods, dereference the first argument (Self) to get instance pointer,
                     * BUT only if Self was not already loaded by value (i.e., not a var param).
                     * For non-methods with var parameters, don't dereference. */
                    int should_dereference = 0;
                    if (is_class_method && arg_num == 0 && !arg_is_var_param)
                    {
                        /* Class method Self from local variable: dereference to get instance pointer */
                        should_dereference = 1;
                    }
                    else if (!is_var_param && !arg_is_var_param)
                    {
                        /* Non-var class parameter from local variable: dereference to get instance pointer */
                        should_dereference = 1;
                    }
                    /* else: var parameter of class type OR argument is already a var param:
                     * codegen_address_for_expr already loaded the value, don't dereference again */

                    if (should_dereference)
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                            addr_reg->bit_64, addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }
            }
            if (codegen_had_error(ctx) || addr_reg == NULL)
                return inst_list;

            /* ARCHITECTURAL FIX: Spill address to stack to prevent clobbering by nested calls */
            StackNode_t *arg_spill = add_l_t("arg_eval");
            if (arg_spill != NULL && arg_infos != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", 
                    addr_reg->bit_64, arg_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), addr_reg);
                
                arg_infos[arg_num].reg = NULL;
                arg_infos[arg_num].spill = arg_spill;
                arg_infos[arg_num].expr = arg_expr;
            }
            else if (arg_infos != NULL)
            {
                arginfo_assign_register(&arg_infos[arg_num], addr_reg, arg_expr);
            }
        }
        else if (formal_is_dynarray && arg_expr != NULL &&
            arg_expr->is_array_expr && arg_expr->array_is_dynamic)
        {
            Register_t *addr_reg = NULL;
            if (!codegen_expr_is_addressable(arg_expr))
            {
                codegen_report_error(ctx,
                    "ERROR: Unsupported expression type for dynamic array argument.");
                return inst_list;
            }
            inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
                return inst_list;

            StackNode_t *arg_spill = add_l_t("arg_eval");
            if (arg_spill != NULL && arg_infos != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                    addr_reg->bit_64, arg_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), addr_reg);
                
                arg_infos[arg_num].reg = NULL;
                arg_infos[arg_num].spill = arg_spill;
                arg_infos[arg_num].expr = arg_expr;
            }
            else if (arg_infos != NULL)
            {
                arginfo_assign_register(&arg_infos[arg_num], addr_reg, arg_expr);
            }
        }
        else if (arg_expr != NULL && expr_has_type_tag(arg_expr, RECORD_TYPE))
        {
            if (!codegen_expr_is_addressable(arg_expr))
            {
                codegen_report_error(ctx,
                    "ERROR: Unsupported record argument expression.");
                return inst_list;
            }

            long long record_size = 0;
            if (codegen_get_record_size(ctx, arg_expr, &record_size) != 0 || record_size <= 0)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to determine record size for argument.");
                return inst_list;
            }

            if (record_size > INT_MAX)
            {
                codegen_report_error(ctx,
                    "ERROR: Record argument size exceeds supported limits.");
                return inst_list;
            }

            StackNode_t *temp_slot = codegen_alloc_record_temp(record_size);
            if (temp_slot == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Failed to allocate temporary storage for record argument.");
                return inst_list;
            }

            Register_t *src_reg = NULL;
            inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &src_reg);
            if (codegen_had_error(ctx) || src_reg == NULL)
                return inst_list;

            Register_t *size_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (size_reg == NULL)
            {
                free_reg(get_reg_stack(), src_reg);
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for record copy size.");
                return inst_list;
            }

            char copy_buffer[128];
            snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t$%lld, %s\n", record_size, size_reg->bit_64);
            inst_list = add_inst(inst_list, copy_buffer);

            if (codegen_target_is_windows())
            {
                snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
                inst_list = add_inst(inst_list, copy_buffer);
                snprintf(copy_buffer, sizeof(copy_buffer), "\tleaq\t-%d(%%rbp), %%rcx\n", temp_slot->offset);
                inst_list = add_inst(inst_list, copy_buffer);
                snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t%s, %%r8\n", size_reg->bit_64);
                inst_list = add_inst(inst_list, copy_buffer);
            }
            else
            {
                snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
                inst_list = add_inst(inst_list, copy_buffer);
                snprintf(copy_buffer, sizeof(copy_buffer), "\tleaq\t-%d(%%rbp), %%rdi\n", temp_slot->offset);
                inst_list = add_inst(inst_list, copy_buffer);
                snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t%s, %%rdx\n", size_reg->bit_64);
                inst_list = add_inst(inst_list, copy_buffer);
            }

            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = add_inst(inst_list, "\tcall\tkgpc_move\n");
            free_arg_regs();

            free_reg(get_reg_stack(), src_reg);
            free_reg(get_reg_stack(), size_reg);

            Register_t *result_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (result_reg == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for record argument pointer.");
                return inst_list;
            }

            /* For external C functions (cdecl), small structs (≤8 bytes) are passed by VALUE,
             * but Pascal passes them by reference (pointer). We automatically dereference
             * the pointer here so Pascal code doesn't need to change.
             * 
             * Example: inet_ntoa(in_addr: TInAddr) where TInAddr is 4 bytes
             * - Pascal passes pointer to TInAddr
             * - C expects TInAddr value in register
             * - We dereference: load the 4-byte value from the pointer
             * 
             * CRITICAL FIX: Check the KgpcType's procedure definition for cname_flag
             * instead of checking if the procedure name contains "cdecl" or "external".
             * The procedure name is just "inet_ntoa", not "inet_ntoa_cdecl_external".
             */
            if (is_external_c_function && record_size <= 8)
            {
                /* Load address of the record copy */
                snprintf(copy_buffer, sizeof(copy_buffer), "\tleaq\t-%d(%%rbp), %s\n", 
                    temp_slot->offset, result_reg->bit_64);
                inst_list = add_inst(inst_list, copy_buffer);
                
                /* Dereference: load the value from the address */
                if (record_size == 1)
                {
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovzbl\t(%s), %s\n", 
                        result_reg->bit_64, result_reg->bit_32);
                }
                else if (record_size == 2)
                {
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovzwl\t(%s), %s\n", 
                        result_reg->bit_64, result_reg->bit_32);
                }
                else if (record_size <= 4)
                {
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovl\t(%s), %s\n", 
                        result_reg->bit_64, result_reg->bit_32);
                }
                else /* record_size <= 8 */
                {
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t(%s), %s\n", 
                        result_reg->bit_64, result_reg->bit_64);
                }
                inst_list = add_inst(inst_list, copy_buffer);
            }
            else
            {
                /* Normal case: pass pointer to struct */
                snprintf(copy_buffer, sizeof(copy_buffer), "\tleaq\t-%d(%%rbp), %s\n", temp_slot->offset, result_reg->bit_64);
                inst_list = add_inst(inst_list, copy_buffer);
            }

            /* ARCHITECTURAL FIX: Spill address to stack to prevent clobbering by nested calls */
            StackNode_t *arg_spill = add_l_t("arg_eval");
            if (arg_spill != NULL && arg_infos != NULL)
            {
                snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                    result_reg->bit_64, arg_spill->offset);
                inst_list = add_inst(inst_list, copy_buffer);
                free_reg(get_reg_stack(), result_reg);
                
                arg_infos[arg_num].reg = NULL;
                arg_infos[arg_num].spill = arg_spill;
                arg_infos[arg_num].expr = arg_expr;
            }
            else if (arg_infos != NULL)
            {
                arginfo_assign_register(&arg_infos[arg_num], result_reg, arg_expr);
            }
        }
        else
        {
            // Pass by value
            if (arg_expr->type == EXPR_AS || arg_expr->type == EXPR_IS ||
                arg_expr->type == EXPR_ARRAY_LITERAL ||
                expr_has_type_tag(arg_expr, SET_TYPE))
            {
                Register_t *value_reg = NULL;
                inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &value_reg);
                if (codegen_had_error(ctx) || value_reg == NULL)
                {
                    if (arg_infos != NULL)
                        free(arg_infos);
                    return inst_list;
                }
                top_reg = value_reg;
            }
            else
            {
                expr_tree = build_expr_tree(arg_expr);
                top_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (top_reg == NULL)
                {
                    /* Try spilling to get a register */
                    top_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                }
                CODEGEN_DEBUG("DEBUG: top_reg at %p\n", top_reg);
                if (top_reg == NULL)
                {
                    free_expr_tree(expr_tree);
                    codegen_report_error(ctx,
                        "ERROR: Unable to allocate register for argument evaluation. "
                        "Expression may be too complex for available registers.");
                    if (arg_infos != NULL)
                        free(arg_infos);
                    return inst_list;
                }
                inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, top_reg);
                free_expr_tree(expr_tree);
            }

            if (expected_type == REAL_TYPE)
                inst_list = codegen_expr_maybe_convert_int_like_to_real(expected_type,
                    arg_expr, top_reg, inst_list);

            /* Promote char arguments to strings when the formal parameter expects string. */
            if ((formal_decl_expects_string(formal_arg_decl) ||
                 builtin_arg_expects_string(procedure_name, arg_num)) &&
                expr_has_type_tag(arg_expr, CHAR_TYPE))
            {
                const char *arg_reg32 = codegen_target_is_windows() ? "%ecx" : "%edi";
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", top_reg->bit_32, arg_reg32);
                inst_list = add_inst(inst_list, buffer);
                inst_list = add_inst(inst_list, "\tcall\tkgpc_char_to_string\n");
                snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", top_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }

            /* ARCHITECTURAL FIX: Immediately spill argument to stack to prevent
             * nested function calls from clobbering this value. This ensures that
             * even if subsequent argument evaluations (which may include nested
             * function calls) reuse registers, we can restore the correct value. */
            StackNode_t *arg_spill = add_l_t("arg_eval");
            if (arg_spill != NULL && arg_infos != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", 
                    top_reg->bit_64, arg_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), top_reg);
                
                arg_infos[arg_num].reg = NULL;
                arg_infos[arg_num].spill = arg_spill;
                arg_infos[arg_num].expr = arg_expr;
            }
            else if (arg_infos != NULL)
            {
                arginfo_assign_register(&arg_infos[arg_num], top_reg, arg_expr);
            }
        }

        args = args->next;
        if(formal_args != NULL)
        {
            formal_args = formal_args->next;
            
            /* CRITICAL VALIDATION: After advancing formal_args, check if the new node is valid.
             * On some platforms, corrupted list nodes may have garbage in their 'next' pointer.
             * We validate the next node before the next iteration to prevent segfaults. */
            if (formal_args != NULL && formal_args->type != LIST_TREE && formal_args->type != LIST_UNSPECIFIED)
            {
                const char *proc_name = procedure_name ? procedure_name : "(unknown)";
                codegen_report_error(ctx,
                    "FATAL: Internal compiler error - corrupted formal_args->next (type=%d) at argument %d for procedure %s. "
                    "This indicates the formal arguments list is not properly NULL-terminated or contains corrupted nodes.",
                    formal_args->type, arg_num, proc_name);
                if (arg_infos != NULL)
                    free(arg_infos);
                return inst_list;
            }
        }
        ++arg_num;
    }

    int next_gpr = arg_start_index;
    int next_sse = 0;
    if (arg_infos != NULL)
    {
        for (int i = 0; i < arg_num; ++i)
        {
            int use_sse = (arg_infos[i].expected_type == REAL_TYPE &&
                !arg_infos[i].is_pointer_like);
            if (g_current_codegen_abi == KGPC_TARGET_ABI_WINDOWS && is_external_c_function)
            {
                /* Windows x64 C ABI: argument slots are positional across classes.
                 * The Nth argument uses RCX/RDX/R8/R9 or XMM0-3 based on its type. */
                int reg_slot = arg_start_index + i;
                if (use_sse)
                {
                    arg_infos[i].assigned_class = ARG_CLASS_SSE;
                    if (reg_slot < max_sse_regs)
                        arg_infos[i].assigned_index = reg_slot;
                    else
                    {
                        arg_infos[i].assigned_index = -1;
                        arg_infos[i].pass_via_stack = 1;
                        arg_infos[i].stack_slot = stack_slot_count++;
                    }
                }
                else
                {
                    arg_infos[i].assigned_class = ARG_CLASS_INT;
                    if (reg_slot < max_int_regs)
                        arg_infos[i].assigned_index = reg_slot;
                    else
                    {
                        arg_infos[i].assigned_index = -1;
                        arg_infos[i].pass_via_stack = 1;
                        arg_infos[i].stack_slot = stack_slot_count++;
                    }
                }
            }
            else
            {
                /* SysV: separate register files for SSE and INT */
                if (use_sse)
                {
                    arg_infos[i].assigned_class = ARG_CLASS_SSE;
                    if (next_sse < max_sse_regs)
                    {
                        arg_infos[i].assigned_index = next_sse++;
                    }
                    else
                    {
                        arg_infos[i].assigned_index = -1;
                        arg_infos[i].pass_via_stack = 1;
                        arg_infos[i].stack_slot = stack_slot_count++;
                    }
                }
                else
                {
                    arg_infos[i].assigned_class = ARG_CLASS_INT;
                    if (next_gpr < max_int_regs)
                    {
                        arg_infos[i].assigned_index = next_gpr++;
                    }
                    else
                    {
                        arg_infos[i].assigned_index = -1;
                        arg_infos[i].pass_via_stack = 1;
                        arg_infos[i].stack_slot = stack_slot_count++;
                    }
                }
            }
        }
    }

    if (stack_slot_count > 0 || codegen_target_is_windows())
    {
        /* Windows x64 requires the caller to reserve 32 bytes of shadow space for
         * *every* call, even when all args fit in registers. Stack-passed args are
         * placed after that shadow space. */
        int shadow_space = codegen_target_is_windows() ? 32 : 0;
        int stack_bytes = stack_slot_count * CODEGEN_POINTER_SIZE_BYTES;
        /* Alignment padding must be placed AFTER the stack arguments.
         * Stack arguments start immediately after the Windows shadow space (if any),
         * i.e. at offset 32(%rsp) for the 5th argument. */
        int padding = codegen_expr_align_to(stack_bytes, REQUIRED_OFFSET) - stack_bytes;
        int total_stack_area = shadow_space + stack_bytes + padding;
        if (total_stack_area > 0)
        {
            snprintf(buffer, sizeof(buffer), "\tsubq\t$%d, %%rsp\n", total_stack_area);
            inst_list = add_inst(inst_list, buffer);
            if (ctx != NULL)
                ctx->pending_stack_arg_bytes += total_stack_area;
            for (int i = 0; i < arg_num; ++i)
            {
                if (arg_infos[i].pass_via_stack)
                    arg_infos[i].stack_offset =
                        shadow_space + arg_infos[i].stack_slot * CODEGEN_POINTER_SIZE_BYTES;
            }
        }
    }

    for (int i = arg_num - 1; i >= 0; --i)
    {
        int expected_type = (arg_infos != NULL) ? arg_infos[i].expected_type : UNKNOWN_TYPE;
        int actual_type = (arg_infos != NULL && arg_infos[i].expr != NULL)
            ? expr_get_type_tag(arg_infos[i].expr) : UNKNOWN_TYPE;
        int needs_int_to_long = (expected_type == LONGINT_TYPE && actual_type == INT_TYPE);
        int pass_on_stack = (arg_infos != NULL && arg_infos[i].pass_via_stack);

        int reg_index = arg_start_index + i;
        if (!pass_on_stack && arg_infos != NULL && arg_infos[i].assigned_index >= 0)
            reg_index = arg_infos[i].assigned_index;

        if (!pass_on_stack)
        {
            if (arg_infos != NULL && arg_infos[i].assigned_class == ARG_CLASS_SSE)
                arg_reg_char = current_arg_reg_xmm(reg_index);
            else
                arg_reg_char = get_arg_reg64_num(reg_index);
            if (arg_reg_char == NULL)
            {
                fprintf(stderr, "ERROR: Could not get arg register: %d\n", i);
                exit(1);
            }

            if (arg_infos != NULL)
            {
                for (int j = 0; j < i; ++j)
                {
                    const char *check_reg = arg_reg_char;
                    if (arg_infos[j].reg != NULL &&
                        strcmp(arg_infos[j].reg->bit_64, check_reg) == 0)
                    {
                        StackNode_t *spill = add_l_t("arg_spill");
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                            arg_infos[j].reg->bit_64, spill->offset);
                        inst_list = add_inst(inst_list, buffer);
                        free_reg(get_reg_stack(), arg_infos[j].reg);
                        arg_infos[j].reg = NULL;
                        arg_infos[j].spill = spill;
                    }
                }
            }
        }

        Register_t *stored_reg = arg_infos != NULL ? arg_infos[i].reg : NULL;
        struct Expression *source_expr = arg_infos != NULL ? arg_infos[i].expr : NULL;
        if (stored_reg != NULL)
        {
            if (needs_int_to_long && arg_infos != NULL &&
                arg_infos[i].assigned_class == ARG_CLASS_INT)
            {
                inst_list = codegen_sign_extend32_to64(inst_list,
                    stored_reg->bit_32, stored_reg->bit_64);
            }
            if (pass_on_stack)
            {
                char stack_dest[64];
                snprintf(stack_dest, sizeof(stack_dest), "%d(%%rsp)", arg_infos[i].stack_offset);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", stored_reg->bit_64, stack_dest);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", stored_reg->bit_64, arg_reg_char);
                inst_list = add_inst(inst_list, buffer);
            }
            free_reg(get_reg_stack(), stored_reg);
        }
        else if (arg_infos != NULL && arg_infos[i].spill != NULL)
        {
            Register_t *temp_reg = NULL;
            if (needs_int_to_long && arg_infos[i].assigned_class == ARG_CLASS_INT)
            {
                temp_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (temp_reg == NULL)
                    return inst_list;
                snprintf(buffer, sizeof(buffer), "\tmovslq\t-%d(%%rbp), %s\n",
                    arg_infos[i].spill->offset, temp_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                temp_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (temp_reg == NULL)
                    return inst_list;
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    arg_infos[i].spill->offset, temp_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }

            if (pass_on_stack)
            {
                char stack_dest[64];
                snprintf(stack_dest, sizeof(stack_dest), "%d(%%rsp)", arg_infos[i].stack_offset);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", temp_reg->bit_64, stack_dest);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", temp_reg->bit_64, arg_reg_char);
                inst_list = add_inst(inst_list, buffer);
            }
            free_reg(get_reg_stack(), temp_reg);
        }
        else
        {
            const char *proc_name = procedure_name ? procedure_name : "(unknown)";
            fprintf(stderr,
                    "ERROR: Missing evaluated value for argument %d in call to %s (%s).\n",
                    i,
                    proc_name,
                    describe_expression_kind(source_expr));
            exit(1);
        }
    }

    free(arg_infos);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

ListNode_t *codegen_cleanup_call_stack(ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (ctx != NULL && ctx->pending_stack_arg_bytes > 0)
    {
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\n\taddq\t$%d, %%rsp\n", ctx->pending_stack_arg_bytes);
        inst_list = add_inst(inst_list, buffer);
        ctx->pending_stack_arg_bytes = 0;
    }
    free_arg_regs();
    return inst_list;
}

/* Helper for codegen_get_nonlocal */
ListNode_t * codegen_goto_prev_scope(ListNode_t *inst_list, StackScope_t *cur_scope, char *base)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    char buffer[50];

    assert(inst_list != NULL);
    assert(cur_scope != NULL);
    assert(base != NULL);

    snprintf(buffer, 50, "\tmovq\t(%s), %s\n", base, current_non_local_reg64());
    inst_list = add_inst(inst_list, buffer);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}
