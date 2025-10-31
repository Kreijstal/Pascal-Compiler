/*
    Damon Gwinn
    Performs semantic checking on a given statement

    NOTE: Max scope level refers to the highest level scope we can reference a variable at
        - 0 is the current scope, 1 is the first above and so on
        - Functions can't have side effects, but they can contain procedures so this is a
            general way to define the maximum scope level

    All functions set the pointer type_return to the type the expression will return
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>
#include "SemCheck_expr.h"
#include "../NameMangling.h"
#include "../HashTable/HashTable.h"
#include "../SymTab/SymTab.h"
#include "../../ParseTree/tree.h"
#include "../../ParseTree/tree_types.h"
#include "../../ParseTree/type_tags.h"
#include "../../../identifier_utils.h"

int is_type_ir(int *type);
static int types_numeric_compatible(int lhs, int rhs);
int is_and_or(int *type);
int set_type_from_hashtype(int *type, HashNode_t *hash_node);
int semcheck_arrayaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_funccall(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
static int semcheck_typecast(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
static void semcheck_clear_pointer_info(struct Expression *expr);
static void semcheck_set_pointer_info(struct Expression *expr, int subtype, const char *type_id);
static int semcheck_pointer_deref(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
static int semcheck_recordaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
static int semcheck_addressof(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);

typedef struct WithContextEntry {
    struct Expression *context_expr;
    struct RecordType *record_type;
} WithContextEntry;

static WithContextEntry *with_context_stack = NULL;
static size_t with_context_count = 0;
static size_t with_context_capacity = 0;

static int ensure_with_capacity(void);
static struct Expression *clone_expression(const struct Expression *expr);
static struct RecordType *resolve_record_type_for_with(SymTab_t *symtab,
    struct Expression *context_expr, int expr_type, int line_num);

#define SIZEOF_RECURSION_LIMIT 64
#define POINTER_SIZE_BYTES 8

static int sizeof_from_hashnode(SymTab_t *symtab, HashNode_t *node,
    long long *size_out, int depth, int line_num);
static int sizeof_from_alias(SymTab_t *symtab, struct TypeAlias *alias,
    long long *size_out, int depth, int line_num);
static int sizeof_from_record(SymTab_t *symtab, struct RecordType *record,
    long long *size_out, int depth, int line_num);
static int sizeof_from_type_ref(SymTab_t *symtab, int type_tag,
    const char *type_id, long long *size_out, int depth, int line_num);
static int resolve_record_field(SymTab_t *symtab, struct RecordType *record,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int line_num);
static void semcheck_clear_pointer_info(struct Expression *expr)
{
    if (expr == NULL)
        return;

    expr->pointer_subtype = UNKNOWN_TYPE;
    if (expr->pointer_subtype_id != NULL)
    {
        free(expr->pointer_subtype_id);
        expr->pointer_subtype_id = NULL;
    }
}

static void semcheck_set_pointer_info(struct Expression *expr, int subtype, const char *type_id)
{
    if (expr == NULL)
        return;

    semcheck_clear_pointer_info(expr);
    expr->pointer_subtype = subtype;
    if (type_id != NULL)
    {
        expr->pointer_subtype_id = strdup(type_id);
        if (expr->pointer_subtype_id == NULL)
            fprintf(stderr, "Error: failed to allocate pointer type identifier.\n");
    }
}

static int ensure_with_capacity(void)
{
    if (with_context_count < with_context_capacity)
        return 0;

    size_t new_capacity = with_context_capacity == 0 ? 8 : with_context_capacity * 2;
    WithContextEntry *new_stack = realloc(with_context_stack,
        new_capacity * sizeof(*with_context_stack));
    if (new_stack == NULL)
        return 1;

    with_context_stack = new_stack;
    with_context_capacity = new_capacity;
    return 0;
}

static struct Expression *clone_expression(const struct Expression *expr)
{
    if (expr == NULL)
        return NULL;

    struct Expression *clone = (struct Expression *)calloc(1, sizeof(struct Expression));
    if (clone == NULL)
        return NULL;

    clone->line_num = expr->line_num;
    clone->type = expr->type;
    clone->resolved_type = expr->resolved_type;
    clone->pointer_subtype = expr->pointer_subtype;
    clone->record_type = expr->record_type;
    if (expr->pointer_subtype_id != NULL)
    {
        clone->pointer_subtype_id = strdup(expr->pointer_subtype_id);
        if (clone->pointer_subtype_id == NULL)
        {
            free(clone);
            return NULL;
        }
    }

    if (expr->field_width != NULL)
        clone->field_width = clone_expression(expr->field_width);
    if (expr->field_precision != NULL)
        clone->field_precision = clone_expression(expr->field_precision);

    switch (expr->type)
    {
        case EXPR_VAR_ID:
            clone->expr_data.id = expr->expr_data.id != NULL ? strdup(expr->expr_data.id) : NULL;
            if (expr->expr_data.id != NULL && clone->expr_data.id == NULL)
            {
                destroy_expr(clone);
                return NULL;
            }
            break;
        case EXPR_RECORD_ACCESS:
            clone->expr_data.record_access_data.record_expr =
                clone_expression(expr->expr_data.record_access_data.record_expr);
            clone->expr_data.record_access_data.field_id =
                expr->expr_data.record_access_data.field_id != NULL ?
                    strdup(expr->expr_data.record_access_data.field_id) : NULL;
            clone->expr_data.record_access_data.field_offset =
                expr->expr_data.record_access_data.field_offset;
            if (expr->expr_data.record_access_data.field_id != NULL &&
                clone->expr_data.record_access_data.field_id == NULL)
            {
                destroy_expr(clone);
                return NULL;
            }
            break;
        case EXPR_POINTER_DEREF:
            clone->expr_data.pointer_deref_data.pointer_expr =
                clone_expression(expr->expr_data.pointer_deref_data.pointer_expr);
            break;
        case EXPR_ADDR:
            clone->expr_data.addr_data.expr =
                clone_expression(expr->expr_data.addr_data.expr);
            break;
        case EXPR_SIGN_TERM:
            clone->expr_data.sign_term = clone_expression(expr->expr_data.sign_term);
            break;
        case EXPR_ADDOP:
            clone->expr_data.addop_data.addop_type = expr->expr_data.addop_data.addop_type;
            clone->expr_data.addop_data.left_expr =
                clone_expression(expr->expr_data.addop_data.left_expr);
            clone->expr_data.addop_data.right_term =
                clone_expression(expr->expr_data.addop_data.right_term);
            break;
        case EXPR_MULOP:
            clone->expr_data.mulop_data.mulop_type = expr->expr_data.mulop_data.mulop_type;
            clone->expr_data.mulop_data.left_term =
                clone_expression(expr->expr_data.mulop_data.left_term);
            clone->expr_data.mulop_data.right_factor =
                clone_expression(expr->expr_data.mulop_data.right_factor);
            break;
        case EXPR_ARRAY_ACCESS:
            clone->expr_data.array_access_data.id =
                expr->expr_data.array_access_data.id != NULL ?
                    strdup(expr->expr_data.array_access_data.id) : NULL;
            clone->expr_data.array_access_data.array_expr =
                clone_expression(expr->expr_data.array_access_data.array_expr);
            if (expr->expr_data.array_access_data.id != NULL &&
                clone->expr_data.array_access_data.id == NULL)
            {
                destroy_expr(clone);
                return NULL;
            }
            break;
        case EXPR_TYPECAST:
            clone->expr_data.typecast_data.target_type = expr->expr_data.typecast_data.target_type;
            clone->expr_data.typecast_data.expr =
                clone_expression(expr->expr_data.typecast_data.expr);
            if (expr->expr_data.typecast_data.target_type_id != NULL)
            {
                clone->expr_data.typecast_data.target_type_id =
                    strdup(expr->expr_data.typecast_data.target_type_id);
                if (clone->expr_data.typecast_data.target_type_id == NULL)
                {
                    destroy_expr(clone);
                    return NULL;
                }
            }
            break;
        case EXPR_RELOP:
            clone->expr_data.relop_data.type = expr->expr_data.relop_data.type;
            clone->expr_data.relop_data.left =
                clone_expression(expr->expr_data.relop_data.left);
            clone->expr_data.relop_data.right =
                clone_expression(expr->expr_data.relop_data.right);
            break;
        case EXPR_INUM:
            clone->expr_data.i_num = expr->expr_data.i_num;
            break;
        case EXPR_RNUM:
            clone->expr_data.r_num = expr->expr_data.r_num;
            break;
        case EXPR_STRING:
            clone->expr_data.string = expr->expr_data.string != NULL ?
                strdup(expr->expr_data.string) : NULL;
            if (expr->expr_data.string != NULL && clone->expr_data.string == NULL)
            {
                destroy_expr(clone);
                return NULL;
            }
            break;
        case EXPR_BOOL:
            clone->expr_data.bool_value = expr->expr_data.bool_value;
            break;
        case EXPR_NIL:
            break;
        default:
            destroy_expr(clone);
            return NULL;
    }

    return clone;
}

int semcheck_with_push(struct Expression *context_expr, struct RecordType *record_type)
{
    if (context_expr == NULL || record_type == NULL)
        return 1;
    if (ensure_with_capacity() != 0)
    {
        fprintf(stderr,
            "Error: failed to expand WITH context stack for semantic analysis.\n");
        return -1;
    }

    with_context_stack[with_context_count].context_expr = context_expr;
    with_context_stack[with_context_count].record_type = record_type;
    ++with_context_count;
    return 0;
}

void semcheck_with_pop(void)
{
    if (with_context_count > 0)
        --with_context_count;
}

static struct RecordType *resolve_record_type_for_with(SymTab_t *symtab,
    struct Expression *context_expr, int expr_type, int line_num)
{
    (void)line_num;
    if (context_expr == NULL)
        return NULL;

    if (expr_type == RECORD_TYPE)
        return context_expr->record_type;

    if (expr_type == POINTER_TYPE)
    {
        struct RecordType *record_info = context_expr->record_type;
        if (record_info == NULL && context_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, context_expr->pointer_subtype_id) != -1 &&
                target_node != NULL)
                record_info = target_node->record_type;
        }
        return record_info;
    }

    return NULL;
}

struct RecordType *semcheck_with_resolve_record_type(SymTab_t *symtab,
    struct Expression *context_expr, int expr_type, int line_num)
{
    return resolve_record_type_for_with(symtab, context_expr, expr_type, line_num);
}

int semcheck_with_try_resolve(const char *field_id, SymTab_t *symtab,
    struct Expression **out_record_expr, int line_num)
{
    if (field_id == NULL || out_record_expr == NULL)
        return 1;

    for (size_t index = with_context_count; index > 0; --index)
    {
        WithContextEntry *entry = &with_context_stack[index - 1];
        if (entry->record_type == NULL)
            continue;

        struct RecordField *field_desc = NULL;
        long long offset = 0;
        if (resolve_record_field(symtab, entry->record_type, field_id,
                &field_desc, &offset, line_num) == 0 && field_desc != NULL)
        {
            struct Expression *clone = clone_expression(entry->context_expr);
            if (clone == NULL)
                return -1;
            *out_record_expr = clone;
            return 0;
        }
    }

    return 1;
}

static int resolve_record_field(SymTab_t *symtab, struct RecordType *record,
    const char *field_name, struct RecordField **out_field, long long *offset_out,
    int line_num)
{
    if (record == NULL || field_name == NULL)
        return 1;

    long long offset = 0;
    ListNode_t *cur = record->fields;
    while (cur != NULL)
    {
        assert(cur->type == LIST_RECORD_FIELD);
        struct RecordField *field = (struct RecordField *)cur->cur;
        if (field != NULL && field->name != NULL &&
            pascal_identifier_equals(field->name, field_name))
        {
            if (out_field != NULL)
                *out_field = field;
            if (offset_out != NULL)
                *offset_out = offset;
            return 0;
        }

        long long field_size = 0;
        if (field != NULL)
        {
            if (field->nested_record != NULL)
            {
                if (sizeof_from_record(symtab, field->nested_record, &field_size, 0, line_num) != 0)
                    return 1;
            }
            else
            {
                if (sizeof_from_type_ref(symtab, field->type, field->type_id,
                        &field_size, 0, line_num) != 0)
                    return 1;
            }
            offset += field_size;
        }

        cur = cur->next;
    }

    fprintf(stderr, "Error on line %d, record field %s not found.\n", line_num, field_name);
    return 1;
}
static int semcheck_builtin_chr(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Chr expects exactly one argument.\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr,
        max_scope_lev, NO_MUTATE);
    if (error_count == 0 && arg_type != INT_TYPE && arg_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Chr expects an integer argument.\\n",
            expr->line_num);
        error_count++;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }

        expr->expr_data.function_call_data.mangled_id = strdup("gpc_chr");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Chr.\\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

        expr->expr_data.function_call_data.resolved_func = NULL;
        *type_return = STRING_TYPE;
        expr->resolved_type = STRING_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_ord(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Ord expects exactly one argument.\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr,
        max_scope_lev, NO_MUTATE);
    if (error_count != 0)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    const char *mangled_name = NULL;
    if (arg_type == STRING_TYPE)
    {
        if (arg_expr != NULL && arg_expr->type == EXPR_STRING)
        {
            char *literal = arg_expr->expr_data.string;
            if (literal == NULL || literal[0] == '\0')
            {
                fprintf(stderr, "Error on line %d, Ord expects a non-empty character literal.\\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }
            if (literal[1] != '\0')
            {
                fprintf(stderr, "Error on line %d, Ord expects a single character literal.\\n",
                    expr->line_num);
                *type_return = UNKNOWN_TYPE;
                return 1;
            }

            unsigned char ordinal_value = (unsigned char)literal[0];

            destroy_list(expr->expr_data.function_call_data.args_expr);
            expr->expr_data.function_call_data.args_expr = NULL;
            if (expr->expr_data.function_call_data.id != NULL)
            {
                free(expr->expr_data.function_call_data.id);
                expr->expr_data.function_call_data.id = NULL;
            }
            if (expr->expr_data.function_call_data.mangled_id != NULL)
            {
                free(expr->expr_data.function_call_data.mangled_id);
                expr->expr_data.function_call_data.mangled_id = NULL;
            }
            expr->expr_data.function_call_data.resolved_func = NULL;

            expr->type = EXPR_INUM;
            expr->expr_data.i_num = ordinal_value;
            expr->resolved_type = LONGINT_TYPE;
            *type_return = LONGINT_TYPE;
            return 0;
        }

        mangled_name = "gpc_ord_string";
    }
    else if (arg_type == INT_TYPE || arg_type == LONGINT_TYPE)
    {
        mangled_name = "gpc_ord_longint";
    }

    if (mangled_name != NULL)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }

        expr->expr_data.function_call_data.mangled_id = strdup(mangled_name);
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Ord.\\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }

        expr->expr_data.function_call_data.resolved_func = NULL;
        *type_return = LONGINT_TYPE;
        expr->resolved_type = LONGINT_TYPE;
        return 0;
    }

    fprintf(stderr, "Error on line %d, Ord expects an integer or character argument.\\n",
        expr->line_num);
    *type_return = UNKNOWN_TYPE;
    return 1;
}

static int semcheck_builtin_length(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Length expects exactly one argument.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg_expr = (struct Expression *)args->cur;
    int arg_type = UNKNOWN_TYPE;
    int error_count = semcheck_expr_main(&arg_type, symtab, arg_expr, max_scope_lev, NO_MUTATE);

    const char *mangled_name = NULL;
    if (error_count == 0 && arg_type == STRING_TYPE)
        mangled_name = "gpc_string_length";
    else if (error_count == 0)
    {
        fprintf(stderr, "Error on line %d, Length currently supports only string arguments.\n", expr->line_num);
        error_count++;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup(mangled_name);
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Length.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        expr->expr_data.function_call_data.resolved_func = NULL;
        expr->resolved_type = LONGINT_TYPE;
        *type_return = LONGINT_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_copy(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next == NULL || args->next->next == NULL || args->next->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Copy expects exactly three arguments.\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *source_expr = (struct Expression *)args->cur;
    struct Expression *index_expr = (struct Expression *)args->next->cur;
    struct Expression *count_expr = (struct Expression *)args->next->next->cur;

    int error_count = 0;
    int source_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&source_type, symtab, source_expr, max_scope_lev, NO_MUTATE);
    if (error_count == 0 && source_type != STRING_TYPE)
    {
        fprintf(stderr, "Error on line %d, Copy expects its first argument to be a string.\n", expr->line_num);
        error_count++;
    }

    int index_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&index_type, symtab, index_expr, max_scope_lev, NO_MUTATE);
    if (error_count == 0 && index_type != INT_TYPE && index_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Copy index must be an integer.\n", expr->line_num);
        error_count++;
    }

    int count_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&count_type, symtab, count_expr, max_scope_lev, NO_MUTATE);
    if (error_count == 0 && count_type != INT_TYPE && count_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Copy count must be an integer.\n", expr->line_num);
        error_count++;
    }

    if (error_count == 0)
    {
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.mangled_id = strdup("gpc_string_copy");
        if (expr->expr_data.function_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for Copy.\n");
            *type_return = UNKNOWN_TYPE;
            return 1;
        }
        expr->expr_data.function_call_data.resolved_func = NULL;
        expr->resolved_type = STRING_TYPE;
        *type_return = STRING_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

static int semcheck_builtin_sizeof(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev);

static long long sizeof_from_type_tag(int type_tag)
{
    switch(type_tag)
    {
        case INT_TYPE:
            return 4;
        case LONGINT_TYPE:
            return 8;
        case REAL_TYPE:
            return 8;
        case STRING_TYPE:
            return POINTER_SIZE_BYTES;
        case CHAR_TYPE:
            return 1;
        case BOOL:
            /*
             * Standalone booleans occupy 4 bytes to keep stack accesses aligned,
             * but packed arrays of boolean elements are emitted as 1 byte each.
             * Document the distinction so sizeof(boolean) users are not surprised.
             */
            return 4;
        case POINTER_TYPE:
            return POINTER_SIZE_BYTES;
        case SET_TYPE:
        case ENUM_TYPE:
            return 4;
        case FILE_TYPE:
            return POINTER_SIZE_BYTES;
        case PROCEDURE:
            return POINTER_SIZE_BYTES;
        default:
            return -1;
    }
}

static long long sizeof_from_var_type(enum VarType var_type)
{
    switch(var_type)
    {
        case HASHVAR_INTEGER:
            return 4;
        case HASHVAR_LONGINT:
            return 8;
        case HASHVAR_REAL:
            return 8;
        case HASHVAR_PCHAR:
            return POINTER_SIZE_BYTES;
        case HASHVAR_BOOLEAN:
            return 4;
        case HASHVAR_PROCEDURE:
            return POINTER_SIZE_BYTES;
        case HASHVAR_CHAR:
            return 1;
        case HASHVAR_POINTER:
            return POINTER_SIZE_BYTES;
        case HASHVAR_SET:
            return 4;
        case HASHVAR_ENUM:
            return 4;
        case HASHVAR_FILE:
            return POINTER_SIZE_BYTES;
        default:
            return -1;
    }
}

static int sizeof_from_type_ref(SymTab_t *symtab, int type_tag,
    const char *type_id, long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        fprintf(stderr, "Error on line %d, SizeOf exceeded recursion depth while resolving type.\n",
            line_num);
        return 1;
    }

    if (type_tag != UNKNOWN_TYPE)
    {
        long long base_size = sizeof_from_type_tag(type_tag);
        if (base_size >= 0)
        {
            *size_out = base_size;
            return 0;
        }
    }

    if (type_id != NULL)
    {
        HashNode_t *target_node = NULL;
        if (FindIdent(&target_node, symtab, (char *)type_id) == -1 || target_node == NULL)
        {
            fprintf(stderr, "Error on line %d, SizeOf references unknown type %s.\n",
                line_num, type_id);
            return 1;
        }
        return sizeof_from_hashnode(symtab, target_node, size_out, depth + 1, line_num);
    }

    fprintf(stderr, "Error on line %d, unable to resolve type information for SizeOf.\n",
        line_num);
    return 1;
}

static int sizeof_from_record(SymTab_t *symtab, struct RecordType *record,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (record == NULL)
    {
        *size_out = 0;
        return 0;
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        fprintf(stderr, "Error on line %d, SizeOf exceeded recursion depth while resolving record type.\n",
            line_num);
        return 1;
    }

    long long total = 0;
    ListNode_t *cur = record->fields;
    while (cur != NULL)
    {
        assert(cur->type == LIST_RECORD_FIELD);
        struct RecordField *field = (struct RecordField *)cur->cur;
        long long field_size = 0;

        if (field->nested_record != NULL)
        {
            if (sizeof_from_record(symtab, field->nested_record, &field_size,
                    depth + 1, line_num) != 0)
                return 1;
        }
        else
        {
            if (sizeof_from_type_ref(symtab, field->type, field->type_id,
                    &field_size, depth + 1, line_num) != 0)
                return 1;
        }

        total += field_size;
        cur = cur->next;
    }

    *size_out = total;
    return 0;
}

static int sizeof_from_alias(SymTab_t *symtab, struct TypeAlias *alias,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (alias == NULL)
    {
        fprintf(stderr, "Error on line %d, SizeOf encountered incomplete type alias.\n",
            line_num);
        return 1;
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        fprintf(stderr, "Error on line %d, SizeOf exceeded recursion depth while resolving type alias.\n",
            line_num);
        return 1;
    }

    if (alias->is_array)
    {
        if (alias->is_open_array || alias->array_end < alias->array_start)
        {
            fprintf(stderr, "Error on line %d, SizeOf cannot determine size of open array type.\n",
                line_num);
            return 1;
        }

        long long element_size = 0;
        if (sizeof_from_type_ref(symtab, alias->array_element_type,
                alias->array_element_type_id, &element_size, depth + 1, line_num) != 0)
            return 1;

        long long count = (long long)alias->array_end - (long long)alias->array_start + 1;
        if (count < 0)
        {
            fprintf(stderr, "Error on line %d, invalid bounds for array type in SizeOf.\n",
                line_num);
            return 1;
        }

        *size_out = element_size * count;
        return 0;
    }

    if (alias->base_type != UNKNOWN_TYPE)
    {
        return sizeof_from_type_ref(symtab, alias->base_type, NULL,
            size_out, depth + 1, line_num);
    }

    if (alias->target_type_id != NULL)
        return sizeof_from_type_ref(symtab, UNKNOWN_TYPE, alias->target_type_id,
            size_out, depth + 1, line_num);

    fprintf(stderr, "Error on line %d, SizeOf encountered unresolved type alias.\n",
        line_num);
    return 1;
}

static int sizeof_from_hashnode(SymTab_t *symtab, HashNode_t *node,
    long long *size_out, int depth, int line_num)
{
    if (size_out == NULL)
        return 1;

    if (node == NULL)
    {
        fprintf(stderr, "Error on line %d, SizeOf encountered null symbol information.\n",
            line_num);
        return 1;
    }

    if (depth > SIZEOF_RECURSION_LIMIT)
    {
        fprintf(stderr, "Error on line %d, SizeOf exceeded recursion depth while resolving symbol.\n",
            line_num);
        return 1;
    }

    if (node->hash_type == HASHTYPE_TYPE)
    {
        if (node->record_type != NULL)
            return sizeof_from_record(symtab, node->record_type, size_out,
                depth + 1, line_num);
        if (node->type_alias != NULL)
            return sizeof_from_alias(symtab, node->type_alias, size_out,
                depth + 1, line_num);

        long long base = sizeof_from_var_type(node->var_type);
        if (base >= 0)
        {
            *size_out = base;
            return 0;
        }
    }

    if (node->is_array)
    {
        if (node->is_dynamic_array || node->array_end < node->array_start)
        {
            fprintf(stderr, "Error on line %d, SizeOf cannot determine size of dynamic array %s.\n",
                line_num, node->id);
            return 1;
        }

        long long element_size = node->element_size;
        if (element_size <= 0)
        {
            long long base = sizeof_from_var_type(node->var_type);
            if (base < 0)
            {
                fprintf(stderr, "Error on line %d, SizeOf cannot determine element size for array %s.\n",
                    line_num, node->id);
                return 1;
            }
            element_size = base;
        }

        long long count = (long long)node->array_end - (long long)node->array_start + 1;
        if (count < 0)
        {
            fprintf(stderr, "Error on line %d, invalid bounds for array %s in SizeOf.\n",
                line_num, node->id);
            return 1;
        }

        *size_out = element_size * count;
        return 0;
    }

    if (node->var_type == HASHVAR_RECORD && node->record_type != NULL)
        return sizeof_from_record(symtab, node->record_type, size_out,
            depth + 1, line_num);

    if (node->type_alias != NULL)
        return sizeof_from_alias(symtab, node->type_alias, size_out,
            depth + 1, line_num);

    long long base = sizeof_from_var_type(node->var_type);
    if (base >= 0)
    {
        *size_out = base;
        return 0;
    }

    fprintf(stderr, "Error on line %d, SizeOf cannot determine size of %s.\n",
        line_num, node->id != NULL ? node->id : "symbol");
    return 1;
}

static int semcheck_builtin_sizeof(int *type_return, SymTab_t *symtab,
    struct Expression *expr, int max_scope_lev)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, SizeOf expects exactly one argument.\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    struct Expression *arg = (struct Expression *)args->cur;
    int error_count = 0;
    long long computed_size = 0;

    if (arg != NULL && arg->type == EXPR_VAR_ID)
    {
        char *arg_id = arg->expr_data.id;
        HashNode_t *node = NULL;
        int scope = FindIdent(&node, symtab, arg_id);
        if (scope == -1 || node == NULL)
        {
            fprintf(stderr, "Error on line %d, SizeOf references undeclared identifier %s.\n",
                expr->line_num, arg_id);
            error_count++;
        }
        else
        {
            if (node->hash_type != HASHTYPE_TYPE && scope > max_scope_lev)
            {
                fprintf(stderr, "Error on line %d, SizeOf cannot access %s due to scope restrictions.\n",
                    expr->line_num, arg_id);
                error_count++;
            }

            if (error_count == 0)
            {
                if (node->hash_type == HASHTYPE_VAR || node->hash_type == HASHTYPE_ARRAY ||
                    node->hash_type == HASHTYPE_CONST || node->hash_type == HASHTYPE_FUNCTION_RETURN)
                {
                    set_hash_meta(node, NO_MUTATE);
                }
                else if (node->hash_type != HASHTYPE_TYPE)
                {
                    fprintf(stderr, "Error on line %d, SizeOf argument %s is not a data object.\n",
                        expr->line_num, arg_id);
                    error_count++;
                }
            }

            if (error_count == 0)
            {
                if (node->hash_type != HASHTYPE_TYPE && node->hash_type != HASHTYPE_ARRAY)
                {
                    int dummy_type = UNKNOWN_TYPE;
                    error_count += semcheck_expr_main(&dummy_type, symtab, arg,
                        max_scope_lev, NO_MUTATE);
                }

                if (error_count == 0)
                    error_count += sizeof_from_hashnode(symtab, node, &computed_size,
                        0, expr->line_num);
            }
        }
    }
    else
    {
        int arg_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_main(&arg_type, symtab, arg, max_scope_lev, NO_MUTATE);
        if (error_count == 0)
            error_count += sizeof_from_type_ref(symtab, arg_type, NULL, &computed_size,
                0, expr->line_num);
    }

    if (error_count == 0)
    {
        if (computed_size < 0)
        {
            fprintf(stderr, "Error on line %d, SizeOf produced an invalid result.\n",
                expr->line_num);
            error_count++;
        }
        /* No upper-bound clamp: computed_size already stored in a long long literal. */
    }

    if (error_count == 0)
    {
        destroy_list(expr->expr_data.function_call_data.args_expr);
        expr->expr_data.function_call_data.args_expr = NULL;
        if (expr->expr_data.function_call_data.id != NULL)
        {
            free(expr->expr_data.function_call_data.id);
            expr->expr_data.function_call_data.id = NULL;
        }
        if (expr->expr_data.function_call_data.mangled_id != NULL)
        {
            free(expr->expr_data.function_call_data.mangled_id);
            expr->expr_data.function_call_data.mangled_id = NULL;
        }
        expr->expr_data.function_call_data.resolved_func = NULL;

        expr->type = EXPR_INUM;
        expr->expr_data.i_num = computed_size;
        expr->resolved_type = LONGINT_TYPE;
        *type_return = LONGINT_TYPE;
        return 0;
    }

    *type_return = UNKNOWN_TYPE;
    return error_count;
}

int semcheck_relop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_signterm(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_addop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_mulop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_varid(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
    /* Sets hash meta based on given mutating flag */
void set_hash_meta(HashNode_t *node, int mutating)
{
    assert(node != NULL);
    if(mutating == BOTH_MUTATE_REFERENCE)
    {
        node->referenced += 1;
        node->mutated += 1;
    }
    else
    {
        node->referenced += 1-mutating;
        node->mutated += mutating;
    }
}

/* Verifies a type is an INT_TYPE or REAL_TYPE */
int is_type_ir(int *type)
{
    assert(type != NULL);
    return (*type == INT_TYPE || *type == REAL_TYPE || *type == LONGINT_TYPE);
}

static int types_numeric_compatible(int lhs, int rhs)
{
    if (lhs == rhs)
        return 1;
    if ((lhs == INT_TYPE && rhs == LONGINT_TYPE) || (lhs == LONGINT_TYPE && rhs == INT_TYPE))
        return 1;
    if ((lhs == REAL_TYPE && (rhs == INT_TYPE || rhs == LONGINT_TYPE)) ||
        (rhs == REAL_TYPE && (lhs == INT_TYPE || lhs == LONGINT_TYPE)))
        return 1;
    return 0;
}

static int resolve_type_identifier(int *out_type, SymTab_t *symtab,
    const char *type_id, int line_num)
{
    if (type_id == NULL)
        return 0;

    HashNode_t *type_node = NULL;
    if (FindIdent(&type_node, symtab, (char *)type_id) == -1 || type_node == NULL)
    {
        fprintf(stderr, "Error on line %d, typecast references unknown type %s!\n\n",
            line_num, type_id);
        return 1;
    }

    if (type_node->hash_type != HASHTYPE_TYPE)
    {
        fprintf(stderr, "Error on line %d, %s is not a type identifier.\n\n",
            line_num, type_id);
        return 1;
    }

    set_type_from_hashtype(out_type, type_node);

    if (type_node->type_alias != NULL)
    {
        struct TypeAlias *alias = type_node->type_alias;
        if (alias->base_type != UNKNOWN_TYPE)
            *out_type = alias->base_type;

        if (alias->target_type_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, alias->target_type_id) != -1 &&
                target_node != NULL)
            {
                set_type_from_hashtype(out_type, target_node);
            }
        }

        if (alias->is_pointer)
            *out_type = POINTER_TYPE;
        else if (alias->is_set)
            *out_type = SET_TYPE;
        else if (alias->is_enum)
            *out_type = ENUM_TYPE;
        else if (alias->is_file)
            *out_type = FILE_TYPE;
    }

    return 0;
}
/* Checks if a type is a relational AND or OR */
int is_and_or(int *type)
{
    assert(type != NULL);
    return (*type == AND || *type == OR);
}

static int semcheck_typecast(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;

    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_TYPECAST);

    int error_count = 0;
    int inner_type = UNKNOWN_TYPE;

    if (expr->expr_data.typecast_data.expr != NULL)
        error_count += semcheck_expr_main(&inner_type, symtab,
            expr->expr_data.typecast_data.expr, max_scope_lev, NO_MUTATE);

    int target_type = expr->expr_data.typecast_data.target_type;
    error_count += resolve_type_identifier(&target_type, symtab,
        expr->expr_data.typecast_data.target_type_id, expr->line_num);

    if (target_type == UNKNOWN_TYPE &&
        expr->expr_data.typecast_data.target_type_id == NULL)
    {
        fprintf(stderr, "Error on line %d, typecast requires a target type.\n\n",
            expr->line_num);
        ++error_count;
    }

    *type_return = target_type;

    (void)inner_type;
    return error_count;
}

static int semcheck_pointer_deref(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;

    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_POINTER_DEREF);

    semcheck_clear_pointer_info(expr);
    expr->record_type = NULL;

    struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr == NULL)
    {
        fprintf(stderr, "Error on line %d, dereference operator requires an operand.\\n\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int error_count = 0;
    int pointer_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&pointer_type, symtab, pointer_expr,
        max_scope_lev, NO_MUTATE);

    if (pointer_type != POINTER_TYPE)
    {
        fprintf(stderr, "Error on line %d, dereference operator requires a pointer expression.\\n\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return ++error_count;
    }

    int target_type = pointer_expr->pointer_subtype;
    if (target_type == UNKNOWN_TYPE && pointer_expr->pointer_subtype_id != NULL)
    {
        HashNode_t *target_node = NULL;
        if (FindIdent(&target_node, symtab, pointer_expr->pointer_subtype_id) != -1 &&
            target_node != NULL)
        {
            set_type_from_hashtype(&target_type, target_node);
            if (target_node->type_alias != NULL)
            {
                struct TypeAlias *alias = target_node->type_alias;
                if (alias->base_type != UNKNOWN_TYPE)
                    target_type = alias->base_type;
                else if (alias->is_pointer)
                    target_type = POINTER_TYPE;
                else if (alias->is_set)
                    target_type = SET_TYPE;
                else if (alias->is_enum)
                    target_type = ENUM_TYPE;
                else if (alias->is_file)
                    target_type = FILE_TYPE;
            }
        }
    }

    if (target_type == UNKNOWN_TYPE)
        target_type = LONGINT_TYPE;

    if (target_type == POINTER_TYPE)
        semcheck_set_pointer_info(expr, POINTER_TYPE, pointer_expr->pointer_subtype_id);
    else if (target_type == RECORD_TYPE)
    {
        expr->record_type = pointer_expr->record_type;
        if (expr->record_type == NULL && pointer_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, pointer_expr->pointer_subtype_id) != -1 && target_node != NULL)
                expr->record_type = target_node->record_type;
        }
    }

    *type_return = target_type;
    return error_count;
}

static int semcheck_recordaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_RECORD_ACCESS);

    expr->record_type = NULL;

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    const char *field_id = expr->expr_data.record_access_data.field_id;
    if (record_expr == NULL || field_id == NULL)
    {
        fprintf(stderr, "Error on line %d, malformed record field access.\n\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int error_count = 0;
    int record_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&record_type, symtab, record_expr, max_scope_lev, mutating);

    struct RecordType *record_info = NULL;
    if (record_type == RECORD_TYPE)
    {
        record_info = record_expr->record_type;
    }
    else if (record_type == POINTER_TYPE)
    {
        record_info = record_expr->record_type;
        if (record_info == NULL && record_expr->pointer_subtype_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, record_expr->pointer_subtype_id) != -1 &&
                target_node != NULL)
            {
                record_info = target_node->record_type;
            }
        }

        if (record_info == NULL)
        {
            fprintf(stderr, "Error on line %d, pointer does not reference a record type.\n\n",
                expr->line_num);
            *type_return = UNKNOWN_TYPE;
            return error_count + 1;
        }
    }
    else
    {
        fprintf(stderr, "Error on line %d, field access requires a record value.\n\n", expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    if (record_info == NULL)
    {
        fprintf(stderr, "Error on line %d, unable to resolve record type for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    struct RecordField *field_desc = NULL;
    long long field_offset = 0;
    if (resolve_record_field(symtab, record_info, field_id, &field_desc,
            &field_offset, expr->line_num) != 0 || field_desc == NULL)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    expr->expr_data.record_access_data.field_offset = field_offset;

    int field_type = field_desc->type;
    struct RecordType *field_record = field_desc->nested_record;
    if (field_record != NULL)
        field_type = RECORD_TYPE;

    if (field_desc->type_id != NULL)
    {
        int resolved_type = field_type;
        if (resolve_type_identifier(&resolved_type, symtab, field_desc->type_id, expr->line_num) != 0)
            ++error_count;
        field_type = resolved_type;

        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, field_desc->type_id) != -1 && type_node != NULL)
        {
            if (type_node->record_type != NULL)
                field_record = type_node->record_type;
            else if (type_node->type_alias != NULL && type_node->type_alias->target_type_id != NULL)
            {
                HashNode_t *target_node = NULL;
                if (FindIdent(&target_node, symtab, type_node->type_alias->target_type_id) != -1 &&
                    target_node != NULL && target_node->record_type != NULL)
                {
                    field_record = target_node->record_type;
                }
            }
        }

        if (field_record != NULL && field_type == UNKNOWN_TYPE)
            field_type = RECORD_TYPE;
    }

    if (field_type == UNKNOWN_TYPE && field_record == NULL)
    {
        fprintf(stderr, "Error on line %d, unable to resolve type for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    if (field_type == RECORD_TYPE && field_record == NULL)
    {
        fprintf(stderr, "Error on line %d, missing record definition for field %s.\n\n",
            expr->line_num, field_id);
        *type_return = UNKNOWN_TYPE;
        return error_count + 1;
    }

    expr->record_type = (field_type == RECORD_TYPE) ? field_record : NULL;
    *type_return = field_type;
    return error_count;
}

static int semcheck_addressof(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    (void)mutating;

    assert(type_return != NULL);
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_ADDR);

    semcheck_clear_pointer_info(expr);

    struct Expression *inner = expr->expr_data.addr_data.expr;
    if (inner == NULL)
    {
        fprintf(stderr, "Error on line %d, address-of operator requires an operand.\\n\\n",
            expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
    }

    int error_count = 0;
    int inner_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&inner_type, symtab, inner, max_scope_lev, NO_MUTATE);

    if (inner_type == UNKNOWN_TYPE)
    {
        *type_return = UNKNOWN_TYPE;
        return error_count;
    }

    const char *type_id = NULL;
    if (inner_type == POINTER_TYPE && inner->pointer_subtype_id != NULL)
        type_id = inner->pointer_subtype_id;

    struct RecordType *record_info = NULL;
    if (inner_type == RECORD_TYPE)
        record_info = inner->record_type;
    else if (inner_type == POINTER_TYPE && inner->record_type != NULL)
        record_info = inner->record_type;

    semcheck_set_pointer_info(expr, inner_type, type_id);
    expr->record_type = record_info;
    *type_return = POINTER_TYPE;
    return error_count;
}
/* Sets a type based on a hash_type */
int set_type_from_hashtype(int *type, HashNode_t *hash_node)
{
    assert(type != NULL);
    assert(hash_node != NULL);

    switch(hash_node->var_type)
    {
        case HASHVAR_INTEGER:
            *type = INT_TYPE;
            break;
        case HASHVAR_LONGINT:
            *type = LONGINT_TYPE;
            break;
        case HASHVAR_REAL:
            *type = REAL_TYPE;
            break;
        case HASHVAR_PROCEDURE:
            *type = PROCEDURE;
            break;
        case HASHVAR_PCHAR:
             *type = STRING_TYPE;
             break;
        case HASHVAR_BOOLEAN:
            *type = BOOL;
            break;
        case HASHVAR_CHAR:
            *type = CHAR_TYPE;
            break;
        case HASHVAR_POINTER:
            *type = POINTER_TYPE;
            break;
        case HASHVAR_SET:
            *type = SET_TYPE;
            break;
        case HASHVAR_ENUM:
            *type = ENUM_TYPE;
            break;
        case HASHVAR_FILE:
            *type = FILE_TYPE;
            break;
        case HASHVAR_UNTYPED:
            *type = UNKNOWN_TYPE;
            break;
        case HASHVAR_RECORD:
            *type = RECORD_TYPE;
            break;
        default:
            assert(0 && "Bad type in set_type_from_hashtype!");
            break;
    }
    return 0;
}

/* Semantic check on a normal expression */
int semcheck_expr(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    assert(type_return != NULL);
    return semcheck_expr_main(type_return, symtab, expr, max_scope_lev, mutating);
}

/* Semantic check on a function expression (no side effects allowed) */
int semcheck_expr_func(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int mutating)
{
    assert(type_return != NULL);
    return semcheck_expr_main(type_return, symtab, expr, 0, mutating);
}

/* Main semantic checking */
int semcheck_expr_main(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(type_return != NULL);

    return_val = 0;
    expr->resolved_type = UNKNOWN_TYPE;
    switch(expr->type)
    {
        case EXPR_RELOP:
            return_val += semcheck_relop(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_SIGN_TERM:
            return_val += semcheck_signterm(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_ADDOP:
            return_val += semcheck_addop(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_MULOP:
            return_val += semcheck_mulop(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_VAR_ID:
            return_val += semcheck_varid(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_ARRAY_ACCESS:
            return_val += semcheck_arrayaccess(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_RECORD_ACCESS:
            return_val += semcheck_recordaccess(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        case EXPR_FUNCTION_CALL:
            return_val += semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_POINTER_DEREF:
            return_val += semcheck_pointer_deref(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_ADDR:
            return_val += semcheck_addressof(type_return, symtab, expr, max_scope_lev, mutating);
            break;
        case EXPR_TYPECAST:
            return_val += semcheck_typecast(type_return, symtab, expr, max_scope_lev, mutating);
            break;

        /*** BASE CASES ***/
        case EXPR_INUM:
            if (expr->expr_data.i_num > INT_MAX || expr->expr_data.i_num < INT_MIN)
                *type_return = LONGINT_TYPE;
            else
                *type_return = INT_TYPE;
            break;

        case EXPR_RNUM:
            *type_return = REAL_TYPE;
            break;
        
        case EXPR_STRING:
            *type_return = STRING_TYPE;
            break;

        case EXPR_BOOL:
            *type_return = BOOL;
            break;
        case EXPR_NIL:
            *type_return = POINTER_TYPE;
            semcheck_clear_pointer_info(expr);
            break;
        case EXPR_SET:
            *type_return = SET_TYPE;
            break;

        default:
            assert(0 && "Bad type in semcheck_expr_main!");
            break;
    }

    expr->resolved_type = *type_return;
    return return_val;
}

/****** EXPR SEMCHECKS *******/

/** RELOP **/
int semcheck_relop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val;
    int type_first, type_second;
    struct Expression *expr1, *expr2;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_RELOP);

    return_val = 0;
    expr1 = expr->expr_data.relop_data.left;
    expr2 = expr->expr_data.relop_data.right;

    return_val += semcheck_expr_main(&type_first, symtab, expr1, max_scope_lev, mutating);
    if(expr2 != NULL)
        return_val += semcheck_expr_main(&type_second, symtab, expr2, max_scope_lev, mutating);

    /* Verifying types */

    /* Type must be a bool (this only happens with a NOT operator) */
    if(expr2 == NULL && type_first != BOOL)
    {
        fprintf(stderr, "Error on line %d, expected relational type after \"NOT\"!\n\n",
            expr->line_num);
        ++return_val;
    }
    else
    {
        if(is_and_or(&expr->expr_data.relop_data.type))
        {
            if(type_first != BOOL || type_second != BOOL)
            {
                fprintf(stderr,
                    "Error on line %d, expected two relational types between AND/OR!\n\n",
                    expr->line_num);
                ++return_val;
            }
        }
        else
        {
            int relop_type = expr->expr_data.relop_data.type;
            if (relop_type == IN)
            {
                if (type_second != SET_TYPE)
                {
                    fprintf(stderr, "Error on line %d, expected set operand on right side of IN expression!\n\n",
                        expr->line_num);
                    ++return_val;
                }
                if (type_first != INT_TYPE && type_first != LONGINT_TYPE &&
                    type_first != ENUM_TYPE && type_first != CHAR_TYPE && type_first != BOOL)
                {
                    fprintf(stderr, "Error on line %d, expected integer operand on left side of IN expression!\n\n",
                        expr->line_num);
                    ++return_val;
                }
            }
            else if (relop_type == EQ || relop_type == NE)
            {
                int numeric_ok = types_numeric_compatible(type_first, type_second) &&
                                 is_type_ir(&type_first) && is_type_ir(&type_second);
                int boolean_ok = (type_first == BOOL && type_second == BOOL);
                if (!numeric_ok && !boolean_ok)
                {
                    fprintf(stderr, "Error on line %d, equality comparison requires matching numeric or boolean types!\n\n",
                        expr->line_num);
                    ++return_val;
                }
            }
            else
            {
                if(!types_numeric_compatible(type_first, type_second) ||
                   !is_type_ir(&type_first) || !is_type_ir(&type_second))
                {
                    fprintf(stderr, "Error on line %d, expected compatible numeric types between relational op!\n\n",
                        expr->line_num);
                    ++return_val;
                }
            }
        }
    }

    *type_return = BOOL;
    return return_val;
}

/** SIGN_TERM **/
int semcheck_signterm(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val;
    struct Expression *sign_expr;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_SIGN_TERM);

    return_val = 0;
    sign_expr = expr->expr_data.sign_term;

    return_val += semcheck_expr_main(type_return, symtab, sign_expr, max_scope_lev, mutating);

    /* Checking types */
    if(!is_type_ir(type_return))
    {
        fprintf(stderr, "Error on line %d, expected int or real after \"-\"!\n\n",
            expr->line_num);
        ++return_val;
    }

    return return_val;
}

/** ADDOP **/
int semcheck_addop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val;
    int type_first, type_second;
    struct Expression *expr1, *expr2;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_ADDOP);

    return_val = 0;
    expr1 = expr->expr_data.addop_data.left_expr;
    expr2 = expr->expr_data.addop_data.right_term;

    return_val += semcheck_expr_main(&type_first, symtab, expr1, max_scope_lev, mutating);
    return_val += semcheck_expr_main(&type_second, symtab, expr2, max_scope_lev, mutating);

    int op_type = expr->expr_data.addop_data.addop_type;
    if (op_type == OR)
    {
        if (type_first != BOOL || type_second != BOOL)
        {
            fprintf(stderr, "Error on line %d, expected boolean operands for OR expression!\n\n",
                expr->line_num);
            ++return_val;
        }
        *type_return = BOOL;
        return return_val;
    }

    if (type_first == SET_TYPE && type_second == SET_TYPE)
    {
        if (op_type == PLUS)
        {
            *type_return = SET_TYPE;
        }
        else
        {
            fprintf(stderr, "Error on line %d, unsupported set additive operator.\n\n",
                expr->line_num);
            ++return_val;
            *type_return = SET_TYPE;
        }
        return return_val;
    }

    if (op_type == PLUS && type_first == STRING_TYPE && type_second == STRING_TYPE)
    {
        *type_return = STRING_TYPE;
        return return_val;
    }

    /* Checking numeric types */
    if(!types_numeric_compatible(type_first, type_second))
    {
        fprintf(stderr, "Error on line %d, type mismatch on addop!\n\n",
            expr->line_num);
        ++return_val;
    }
    if(!is_type_ir(&type_first) || !is_type_ir(&type_second))
    {
        fprintf(stderr, "Error on line %d, expected int/real on both sides of addop!\n\n",
            expr->line_num);
        ++return_val;
    }

    if (type_first == REAL_TYPE || type_second == REAL_TYPE)
        *type_return = REAL_TYPE;
    else if (type_first == LONGINT_TYPE || type_second == LONGINT_TYPE)
        *type_return = LONGINT_TYPE;
    else
        *type_return = type_first;
    return return_val;
}

/** MULOP **/
int semcheck_mulop(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val;
    int type_first, type_second;
    struct Expression *expr1, *expr2;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_MULOP);

    return_val = 0;
    expr1 = expr->expr_data.mulop_data.left_term;
    expr2 = expr->expr_data.mulop_data.right_factor;

    return_val += semcheck_expr_main(&type_first, symtab, expr1, max_scope_lev, mutating);
    return_val += semcheck_expr_main(&type_second, symtab, expr2, max_scope_lev, mutating);

    int op_type = expr->expr_data.mulop_data.mulop_type;
    if (op_type == AND)
    {
        if (type_first != BOOL || type_second != BOOL)
        {
            fprintf(stderr, "Error on line %d, expected boolean operands for AND expression!\n\n",
                expr->line_num);
            ++return_val;
        }
        *type_return = BOOL;
        return return_val;
    }

    if (type_first == SET_TYPE && type_second == SET_TYPE)
    {
        if (op_type == STAR)
        {
            *type_return = SET_TYPE;
        }
        else
        {
            fprintf(stderr, "Error on line %d, unsupported set multiplicative operator.\n\n",
                expr->line_num);
            ++return_val;
            *type_return = SET_TYPE;
        }
        return return_val;
    }

    /* Checking numeric types */
    if(!types_numeric_compatible(type_first, type_second))
    {
        fprintf(stderr, "Error on line %d, type mismatch on mulop!\n\n",
            expr->line_num);
        ++return_val;
    }
    if(!is_type_ir(&type_first) || !is_type_ir(&type_second))
    {
        fprintf(stderr, "Error on line %d, expected int/real on both sides of mulop!\n\n",
            expr->line_num);
        ++return_val;
    }

    if (type_first == REAL_TYPE || type_second == REAL_TYPE)
        *type_return = REAL_TYPE;
    else if (type_first == LONGINT_TYPE || type_second == LONGINT_TYPE)
        *type_return = LONGINT_TYPE;
    else
        *type_return = type_first;
    return return_val;
}

/** VAR_ID **/
int semcheck_varid(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val, scope_return;
    char *id;
    HashNode_t *hash_return;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_VAR_ID);

    return_val = 0;
    id = expr->expr_data.id;
    semcheck_clear_pointer_info(expr);

    scope_return = FindIdent(&hash_return, symtab, id);
    if(scope_return == -1)
    {
        struct Expression *with_expr = NULL;
        int with_status = semcheck_with_try_resolve(id, symtab, &with_expr, expr->line_num);
        if (with_status == 0 && with_expr != NULL)
        {
            char *field_id = expr->expr_data.id;
            expr->type = EXPR_RECORD_ACCESS;
            expr->expr_data.record_access_data.record_expr = with_expr;
            expr->expr_data.record_access_data.field_id = field_id;
            expr->expr_data.record_access_data.field_offset = 0;
            return semcheck_recordaccess(type_return, symtab, expr, max_scope_lev, mutating);
        }

        if (with_status == -1)
        {
            fprintf(stderr,
                "Error on line %d, unable to resolve WITH context for field \"%s\".\n\n",
                expr->line_num, id);
            ++return_val;
        }
        else
        {
            fprintf(stderr, "Error on line %d, undeclared identifier \"%s\"!\n\n", expr->line_num, id);
            ++return_val;
        }

        *type_return = UNKNOWN_TYPE;
    }
    else
    {
        /* If this is a function being used in an expression context (not being assigned to),
           convert it to a function call with no arguments.
           
           When mutating == NO_MUTATE, we're reading the function's return value.
           When mutating != NO_MUTATE, we're inside the function assigning to its return value,
           which should remain as HASHTYPE_FUNCTION_RETURN access. */
        if(hash_return->hash_type == HASHTYPE_FUNCTION && mutating == NO_MUTATE)
        {
            char *func_id = expr->expr_data.id;
            /* Set to NULL to transfer ownership to function_call_data.id and avoid double-free */
            expr->expr_data.id = NULL;
            
            expr->type = EXPR_FUNCTION_CALL;
            expr->expr_data.function_call_data.id = func_id;
            expr->expr_data.function_call_data.args_expr = NULL;
            expr->expr_data.function_call_data.mangled_id = NULL;
            expr->expr_data.function_call_data.resolved_func = NULL;
            
            return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
        }
        
        set_hash_meta(hash_return, mutating);
        if(scope_return > max_scope_lev)
        {
            fprintf(stderr, "Error on line %d, cannot change \"%s\", invalid scope!\n",
                expr->line_num, id);
            fprintf(stderr, "[Was it defined above a function declaration?]\n\n");
            ++return_val;
        }
        if(hash_return->hash_type != HASHTYPE_VAR &&
            hash_return->hash_type != HASHTYPE_FUNCTION_RETURN)
        {
            if(hash_return->hash_type == HASHTYPE_CONST && mutating == 0)
            {
                /* Constants are readable values. */
            }
            else
            {
                fprintf(stderr, "Error on line %d, cannot assign \"%s\", is not a scalar variable!\n\n",
                    expr->line_num, id);
                ++return_val;
            }
        }
        set_type_from_hashtype(type_return, hash_return);
        if (*type_return == POINTER_TYPE)
        {
            int subtype = UNKNOWN_TYPE;
            const char *type_id = NULL;
            if (hash_return->type_alias != NULL)
            {
                struct TypeAlias *alias = hash_return->type_alias;
                subtype = alias->pointer_type;
                type_id = alias->pointer_type_id;
            }
            semcheck_set_pointer_info(expr, subtype, type_id);
            expr->record_type = NULL;
            if (expr->pointer_subtype_id != NULL)
            {
                HashNode_t *target_node = NULL;
                if (FindIdent(&target_node, symtab, expr->pointer_subtype_id) != -1 && target_node != NULL)
                    expr->record_type = target_node->record_type;
            }
        }
        if (*type_return == RECORD_TYPE)
            expr->record_type = hash_return->record_type;
        else
            expr->record_type = NULL;
    }

    return return_val;
}

/** ARRAY_ACCESS **/
int semcheck_arrayaccess(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val, scope_return;
    char *id;
    int expr_type;
    struct Expression *access_expr;
    HashNode_t *hash_return;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_ARRAY_ACCESS);

    return_val = 0;
    id = expr->expr_data.array_access_data.id;
    access_expr = expr->expr_data.array_access_data.array_expr;

    /***** FIRST VERIFY ARRAY IDENTIFIER *****/

    scope_return = FindIdent(&hash_return, symtab, id);
    if(scope_return == -1)
    {
        fprintf(stderr, "Error on line %d, undeclared identifier \"%s\"!\n\n", expr->line_num, id);
        ++return_val;

        *type_return = UNKNOWN_TYPE;
    }
    else
    {
        set_hash_meta(hash_return, mutating);
        if(scope_return > max_scope_lev)
        {
            fprintf(stderr, "Error on line %d, cannot change \"%s\", invalid scope!\n",
                expr->line_num, id);
            fprintf(stderr, "[Was it defined above a function declaration?]\n\n");
            ++return_val;
        }
        if(hash_return->hash_type != HASHTYPE_ARRAY)
        {
            fprintf(stderr, "Error on line %d, \"%s\" is not an array variable!\n\n",
                expr->line_num, id);
            ++return_val;
        }

        set_type_from_hashtype(type_return, hash_return);
    }

    /***** THEN VERIFY EXPRESSION INSIDE *****/
    return_val += semcheck_expr_main(&expr_type, symtab, access_expr, max_scope_lev, NO_MUTATE);
    if(expr_type != INT_TYPE && expr_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, expected int in array index expression!\n\n",
            expr->line_num);
        ++return_val;
    }

    return return_val;
}

/** FUNC_CALL **/
int semcheck_funccall(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating)
{
    int return_val, scope_return;
    char *id;
    char *mangled_name;
    int arg_type, cur_arg;
    ListNode_t *true_args, *true_arg_ids, *args_given;
    HashNode_t *hash_return;
    Tree_t *arg_decl;
    assert(symtab != NULL);
    assert(expr != NULL);
    assert(expr->type == EXPR_FUNCTION_CALL);

    return_val = 0;
    id = expr->expr_data.function_call_data.id;
    args_given = expr->expr_data.function_call_data.args_expr;

    if (id != NULL && pascal_identifier_equals(id, "SizeOf"))
        return semcheck_builtin_sizeof(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Chr"))
        return semcheck_builtin_chr(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Ord"))
        return semcheck_builtin_ord(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Length"))
        return semcheck_builtin_length(type_return, symtab, expr, max_scope_lev);

    if (id != NULL && pascal_identifier_equals(id, "Copy"))
        return semcheck_builtin_copy(type_return, symtab, expr, max_scope_lev);

    /***** FIRST VERIFY FUNCTION IDENTIFIER *****/

    ListNode_t *overload_candidates = FindAllIdents(symtab, id);
    mangled_name = MangleFunctionNameFromCallSite(id, args_given, symtab, max_scope_lev);

    int match_count = 0;

    if (overload_candidates != NULL)
    {
        ListNode_t *cur = overload_candidates;
        while(cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate->mangled_id != NULL && strcmp(candidate->mangled_id, mangled_name) == 0)
                match_count++;
            cur = cur->next;
        }
    }

    HashNode_t *best_match = NULL;
    int best_score = 9999;
    int num_best_matches = 0;

    if (overload_candidates != NULL)
    {
        ListNode_t *cur = overload_candidates;
        while(cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;

            if (ListLength(candidate->args) == ListLength(args_given))
            {
                int current_score = 0;
                ListNode_t *formal_args = candidate->args;
                ListNode_t *call_args = args_given;

                while(formal_args != NULL)
                {
                    Tree_t *formal_decl = (Tree_t *)formal_args->cur;
                    int formal_type = formal_decl->tree_data.var_decl_data.type;

                    int call_type;
                    semcheck_expr_main(&call_type, symtab, (struct Expression *)call_args->cur, max_scope_lev, NO_MUTATE);

                    if(formal_type == call_type)
                        current_score += 0;
                    else if (formal_type == LONGINT_TYPE && call_type == INT_TYPE)
                        current_score += 1;
                    else
                        current_score += 1000; // Mismatch

                    formal_args = formal_args->next;
                    call_args = call_args->next;
                }

                if(current_score < best_score)
                {
                    best_score = current_score;
                    best_match = candidate;
                    num_best_matches = 1;
                }
                else if (current_score == best_score)
                {
                    num_best_matches++;
                }
            }
            cur = cur->next;
        }
    }

    if (num_best_matches == 1)
    {
        expr->expr_data.function_call_data.mangled_id = strdup(best_match->mangled_id);
        expr->expr_data.function_call_data.resolved_func = best_match;
        hash_return = best_match;
        scope_return = 0; // FIXME
    }
    else if (num_best_matches == 0)
    {
        fprintf(stderr, "Error on line %d, call to function %s does not match any available overload\n", expr->line_num, id);
        *type_return = UNKNOWN_TYPE;
        return ++return_val;
    }
    else
    {
        fprintf(stderr, "Error on line %d, call to function %s is ambiguous\n", expr->line_num, id);
        *type_return = UNKNOWN_TYPE;
        return ++return_val;
    }


    if(scope_return == -1) // Should not happen if match_count > 0
    {
        fprintf(stderr, "Error on line %d, undeclared function %s (mangled to %s)!\n\n", expr->line_num, id, mangled_name);
        ++return_val;

        *type_return = UNKNOWN_TYPE;
    }
    else
    {
        set_hash_meta(hash_return, mutating);
        if(scope_return > max_scope_lev)
        {
            fprintf(stderr, "Error on line %d, cannot change \"%s\", invalid scope!\n\n",
                expr->line_num, id);
            fprintf(stderr, "[Was it defined above a function declaration?]\n\n");
            ++return_val;
        }
        if(hash_return->hash_type != HASHTYPE_FUNCTION &&
            hash_return->hash_type != HASHTYPE_FUNCTION_RETURN)
        {
            fprintf(stderr, "Error on line %d, \"%s\" is not a function!\n\n",
                expr->line_num, id);
            ++return_val;
        }

        set_type_from_hashtype(type_return, hash_return);

        /***** THEN VERIFY ARGS INSIDE *****/
        cur_arg = 0;
        true_args = hash_return->args;
        while(args_given != NULL && true_args != NULL)
        {
            ++cur_arg;
            assert(args_given->type == LIST_EXPR);
            assert(true_args->type == LIST_TREE);
            return_val += semcheck_expr_main(&arg_type,
                symtab, (struct Expression *)args_given->cur, max_scope_lev, NO_MUTATE);

            arg_decl = (Tree_t *)true_args->cur;
            assert(arg_decl->type == TREE_VAR_DECL);
            true_arg_ids = arg_decl->tree_data.var_decl_data.ids;

            while(true_arg_ids != NULL && args_given != NULL)
            {
                int expected_type = arg_decl->tree_data.var_decl_data.type;
                if(arg_type != expected_type && expected_type != BUILTIN_ANY_TYPE)
                {
                    if (!((arg_type == INT_TYPE && expected_type == LONGINT_TYPE) ||
                          (arg_type == LONGINT_TYPE && expected_type == INT_TYPE)))
                    {
                        fprintf(stderr, "Error on line %d, on function call %s, argument %d: Type mismatch!\n\n",
                            expr->line_num, id, cur_arg);
                        ++return_val;
                    }
                }

                true_arg_ids = true_arg_ids->next;
                args_given = args_given->next;
            }

            true_args = true_args->next;
        }
        if(true_args == NULL && args_given != NULL)
        {
            fprintf(stderr, "Error on line %d, on function call %s, too many arguments given!\n\n",
                expr->line_num, id);
            ++return_val;
        }
        else if(true_args != NULL && args_given == NULL)
        {
            fprintf(stderr, "Error on line %d, on function call %s, not enough arguments given!\n\n",
                expr->line_num, id);
            ++return_val;
        }
    }

    return return_val;
}
