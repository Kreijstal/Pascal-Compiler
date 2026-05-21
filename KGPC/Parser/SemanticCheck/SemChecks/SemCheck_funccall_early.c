#include "SemCheck_Expr_Internal.h"
#include "SemCheck_stmt.h"
#include <time.h>
#include <ctype.h>
#include <limits.h>
#include "SemCheck_funccall_internal.h"

/*
 * BUILTIN_FUNC_MAP — table-driven dispatch for Pascal builtin functions
 * that take no special argument-type pre-inspection.
 *
 * Each entry maps a Pascal builtin name (matched case-insensitively) to its
 * semcheck handler.  Two handler slots handle the two calling conventions:
 *
 *   fn          — standard handler: (int*, SymTab_t*, Expression*, int)
 *   fn_with_arg — extended handler: (int*, SymTab_t*, Expression*, int, int)
 *                 used for Pred/Succ (is_succ) and Low/High (is_high)
 *
 * Exactly one of fn / fn_with_arg is non-NULL per entry; int_arg is only
 * meaningful when fn_with_arg is set.
 *
 * Entries NOT in this table (UpCase, UpperCase, LowerCase, Trunc) require
 * argument-type inspection before dispatching and are handled inline.
 */
typedef int (*SemcheckBuiltinFn)(int *, SymTab_t *, struct Expression *, int);
typedef int (*SemcheckBuiltinFnWithArg)(int *, SymTab_t *, struct Expression *, int, int);

typedef struct {
    const char         *name;        /* Pascal builtin name */
    SemcheckBuiltinFn   fn;          /* standard handler (NULL if fn_with_arg used) */
    SemcheckBuiltinFnWithArg fn_with_arg; /* extended handler (NULL if fn used) */
    int                 int_arg;     /* extra int for fn_with_arg */
} BuiltinFuncEntry;

static const BuiltinFuncEntry BUILTIN_FUNC_MAP[] = {
    /* name       fn                          fn_with_arg                  int_arg */
    { "Chr",      semcheck_builtin_chr,        NULL,                        0 },
    { "Ord",      semcheck_builtin_ord,        NULL,                        0 },
    { "Pred",     NULL,                        semcheck_builtin_predsucc,   0 },
    { "Succ",     NULL,                        semcheck_builtin_predsucc,   1 },
    { "Length",   semcheck_builtin_length,     NULL,                        0 },
    { "Copy",     semcheck_builtin_copy,       NULL,                        0 },
    { "Concat",   semcheck_builtin_concat,     NULL,                        0 },
    { "Pos",      semcheck_builtin_pos,        NULL,                        0 },
    { "StrPas",   semcheck_builtin_strpas,     NULL,                        0 },
    { "EOF",      semcheck_builtin_eof,        NULL,                        0 },
    { "EOLN",     semcheck_builtin_eoln,       NULL,                        0 },
    { "Low",      NULL,                        semcheck_builtin_lowhigh,    0 },
    { "High",     NULL,                        semcheck_builtin_lowhigh,    1 },
    { "Default",  semcheck_builtin_default,    NULL,                        0 },
    { "New",      semcheck_builtin_new_func,   NULL,                        0 },
    { "Power",    semcheck_builtin_power,      NULL,                        0 },
    { "Aligned",  semcheck_builtin_aligned,    NULL,                        0 },
};

#define BUILTIN_FUNC_MAP_COUNT \
    ((int)(sizeof(BUILTIN_FUNC_MAP) / sizeof(BUILTIN_FUNC_MAP[0])))

/*
 * semcheck_dispatch_builtin_func — look up and invoke a table entry by name.
 *
 * Returns 1 and writes the handler's return value into *status_out if a
 * matching entry is found; returns 0 if the name is not in the table.
 */
int semcheck_dispatch_builtin_func(
    const char *id,
    int *type_return,
    SymTab_t *symtab,
    struct Expression *expr,
    int max_scope_lev,
    int *status_out)
{
    if (id == NULL)
        return 0;
    for (int i = 0; i < BUILTIN_FUNC_MAP_COUNT; ++i)
    {
        if (pascal_identifier_equals(id, BUILTIN_FUNC_MAP[i].name))
        {
            if (BUILTIN_FUNC_MAP[i].fn != NULL)
                *status_out = BUILTIN_FUNC_MAP[i].fn(type_return, symtab, expr, max_scope_lev);
            else
                *status_out = BUILTIN_FUNC_MAP[i].fn_with_arg(
                    type_return, symtab, expr, max_scope_lev,
                    BUILTIN_FUNC_MAP[i].int_arg);
            return 1;
        }
    }
    return 0;
}

FunccallState funccall_state_normalize(FunccallCtx *ctx)
{
/* In objfpc mode, a bare method/function name inside its own body refers
 * to the result variable when used as a value expression. Normalize such
 * argument expressions before overload resolution so they are not mistaken
 * for recursive zero-argument calls. */
if (ctx->args_given != NULL)
{
    const char *cur_sub_id = semcheck_get_current_subprogram_id();
    const char *result_var = semcheck_get_current_subprogram_result_var_name();
    const char *method_name = semcheck_get_current_subprogram_method_name();
    const char *replacement = (result_var != NULL && result_var[0] != '\0')
        ? result_var : "Result";
    for (ListNode_t *arg_cur = ctx->args_given; arg_cur != NULL; arg_cur = arg_cur->next)
    {
        if (arg_cur->type != LIST_EXPR || arg_cur->cur == NULL)
            continue;
        struct Expression *arg_expr = (struct Expression *)arg_cur->cur;
        if (kgpc_getenv("KGPC_DEBUG_RESULT_NAME") != NULL &&
            arg_expr->type == EXPR_VAR_ID && arg_expr->expr_data.id != NULL &&
            pascal_identifier_equals(arg_expr->expr_data.id, "correct_fpuregister"))
        {
            fprintf(stderr,
                "[KGPC_DEBUG_RESULT_NAME] funccall arg id=%s cur=%s result=%s method=%s\n",
                arg_expr->expr_data.id,
                cur_sub_id != NULL ? cur_sub_id : "<null>",
                result_var != NULL ? result_var : "<null>",
                method_name != NULL ? method_name : "<null>");
        }
        if (arg_expr->type != EXPR_VAR_ID || arg_expr->expr_data.id == NULL)
            continue;
        const char *arg_id = arg_expr->expr_data.id;
        int is_result_name =
            (cur_sub_id != NULL && pascal_identifier_equals(arg_id, cur_sub_id)) ||
            (result_var != NULL && pascal_identifier_equals(arg_id, result_var)) ||
            (method_name != NULL && pascal_identifier_equals(arg_id, method_name));
        if (!is_result_name)
            continue;

        if (!pascal_identifier_equals(arg_id, replacement))
        {
            /* Don't rename if the original name is a local variable or
             * parameter (it takes precedence over the result variable). */
            HashNode_t *orig_check = NULL;
            if (ctx->symtab != NULL && ctx->symtab->current_scope != NULL &&
                ctx->symtab->current_scope->table != NULL)
            {
                orig_check = FindIdentInTable(ctx->symtab->current_scope->table, arg_id);
                if (orig_check != NULL &&
                    (orig_check->hash_type == HASHTYPE_VAR ||
                     orig_check->hash_type == HASHTYPE_ARRAY))
                    continue;
            }
            /* Don't rename if a user-declared local variable with the
             * replacement name would shadow the function return slot. */
            HashNode_t *local_check = NULL;
            if (ctx->symtab != NULL && ctx->symtab->current_scope != NULL &&
                ctx->symtab->current_scope->table != NULL)
            {
                local_check = FindIdentInTable(ctx->symtab->current_scope->table, replacement);
                if (local_check != NULL &&
                    local_check->hash_type == HASHTYPE_FUNCTION_RETURN)
                    local_check = NULL;
            }
            if (local_check != NULL)
                continue;
            char *dup = strdup(replacement);
            if (dup == NULL)
                do { ctx->final_status = -1; return FC_CLEANUP; } while (0);
            free(arg_expr->expr_data.id);
            arg_expr->expr_data.id = dup;
        }
    }
}

    return FC_INHERITED;
}

FunccallState funccall_state_inherited(FunccallCtx *ctx)
{
/* Handle inherited calls in expression context: resolve to parent class method.
 * E.g., T(inherited Get(Index)^) should call TFPSList.Get, not TFPGList.Get. */
if (ctx->expr->expr_data.function_call_data.is_inherited_call && ctx->id != NULL)
{
    if (ctx->args_given == NULL && ctx->expr->expr_data.function_call_data.is_bare_inherited)
    {
        /* Only auto-forward enclosing method's args for bare "inherited"
         * (no explicit method name).  "inherited MethodName" with no args
         * is a zero-argument call and must NOT inherit the outer params. */
        ListNode_t *forwarded_args = semcheck_clone_current_subprogram_actual_args(0);
        if (forwarded_args != NULL)
        {
            ctx->expr->expr_data.function_call_data.args_expr = forwarded_args;
            ctx->args_given = forwarded_args;
        }
    }

    HashNode_t *self_node = NULL;
    if (FindSymbol(&self_node, ctx->symtab, "Self") != 0 && self_node != NULL)
    {
        struct RecordType *current_class = NULL;
        if (self_node->type != NULL && self_node->type->kind == TYPE_KIND_RECORD &&
            self_node->type->info.record_info != NULL)
            current_class = self_node->type->info.record_info;
        else if (self_node->type != NULL && self_node->type->kind == TYPE_KIND_POINTER &&
                 self_node->type->info.points_to != NULL &&
                 self_node->type->info.points_to->kind == TYPE_KIND_RECORD &&
                 self_node->type->info.points_to->info.record_info != NULL)
            current_class = self_node->type->info.points_to->info.record_info;

        if (current_class != NULL && current_class->parent_class_name != NULL)
        {
            /* Look up method in parent class; walk up ancestors if needed.
             * Use FindAllIdents to handle overloaded methods whose
             * mangled names have parameter-type suffixes (e.g.
             * tcgraisenode__pass_1_p instead of tcgraisenode__pass_1). */
            const char *search_parent = current_class->parent_class_name;
            while (search_parent != NULL)
            {
                char parent_mangled[512];
                snprintf(parent_mangled, sizeof(parent_mangled), "%s__%s",
                    search_parent, ctx->id);
                HashNode_t *parent_method = NULL;
                /* First try exact match */
                if (FindSymbol(&parent_method, ctx->symtab, parent_mangled) != 0 &&
                    parent_method != NULL)
                {
                    /* found */
                }
                else
                {
                    /* Try overload resolution via FindAllIdents */
                    parent_method = NULL;
                    ListNode_t *candidates = FindAllIdents(ctx->symtab, parent_mangled);
                    if (candidates != NULL)
                    {
                        /* Build temp args including Self to match method signatures */
                        struct Expression *tmp_self = mk_varid(ctx->expr->line_num, strdup("Self"));
                        ListNode_t *tmp_self_arg = CreateListNode(tmp_self, LIST_EXPR);
                        tmp_self_arg->next = ctx->args_given;

                        char *call_mangled = MangleFunctionNameFromCallSite(
                            parent_mangled, tmp_self_arg, ctx->symtab, INT_MAX);
                        if (call_mangled != NULL)
                        {
                            for (ListNode_t *c = candidates; c != NULL; c = c->next)
                            {
                                HashNode_t *cand = (HashNode_t *)c->cur;
                                if (cand != NULL && cand->mangled_id != NULL &&
                                    strcmp(cand->mangled_id, call_mangled) == 0)
                                {
                                    parent_method = cand;
                                    break;
                                }
                            }
                            free(call_mangled);
                        }
                        if (parent_method == NULL)
                        {
                            /* Fallback: use first candidate */
                            HashNode_t *first = (HashNode_t *)candidates->cur;
                            if (first != NULL)
                                parent_method = first;
                        }
                        tmp_self_arg->next = NULL;
                        destroy_expr(tmp_self);
                        free(tmp_self_arg);
                        DestroyList(candidates);
                    }
                }
                if (parent_method != NULL)
                {
                    /* Prepend Self as the first argument (method receiver) */
                    struct Expression *self_expr = mk_varid(ctx->expr->line_num, strdup("Self"));
                    ListNode_t *self_arg = CreateListNode(self_expr, LIST_EXPR);
                    self_arg->next = ctx->args_given;
                    ctx->expr->expr_data.function_call_data.args_expr = self_arg;
                    ctx->args_given = self_arg;

                    /* Rewrite the call id to the parent's base name (without
                     * parameter-type suffixes) so downstream symbol table lookups
                     * still work.  Set mangled_id to the fully resolved name
                     * (with suffixes) so codegen emits the correct call target.
                     * Keep is_inherited_call set so downstream code skips
                     * VMT dispatch — inherited calls must be direct. */
                    free(ctx->expr->expr_data.function_call_data.id);
                    ctx->expr->expr_data.function_call_data.id = strdup(parent_mangled);
                    ctx->id = ctx->expr->expr_data.function_call_data.id;
                    /* Set mangled_id to the resolved name from the symbol table */
                    if (parent_method->mangled_id != NULL)
                    {
                        if (ctx->expr->expr_data.function_call_data.mangled_id != NULL)
                            free(ctx->expr->expr_data.function_call_data.mangled_id);
                        ctx->expr->expr_data.function_call_data.mangled_id =
                            strdup(parent_method->mangled_id);
                    }
                    break;
                }
                /* Walk up to grandparent */
                HashNode_t *parent_node = NULL;
                if (FindSymbol(&parent_node, ctx->symtab, (char *)search_parent) != 0 &&
                    parent_node != NULL)
                {
                    struct RecordType *parent_record =
                        get_record_type_from_node(parent_node);
                    search_parent = parent_record ? parent_record->parent_class_name : NULL;
                }
                else
                    search_parent = NULL;
            }
        }
    }
}
    return FC_INTRINSICS;
}

FunccallState funccall_state_intrinsics(FunccallCtx *ctx)
{
/* Parse recovery: Supports(intf, IInterfaceType, ref) can be parsed as
 * Supports(intf, IInterfaceType(ref)) due no-parens call rules.
 * Rewrite the second argument back into a type identifier and restore the
 * third argument when the shape is unambiguous. */
if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "supports") &&
    ctx->args_given != NULL && ctx->args_given->next != NULL && ctx->args_given->next->next == NULL)
{
    struct Expression *arg2 = (struct Expression *)ctx->args_given->next->cur;
    if (arg2 != NULL &&
        arg2->type == EXPR_FUNCTION_CALL &&
        arg2->expr_data.function_call_data.id != NULL &&
        arg2->expr_data.function_call_data.args_expr != NULL &&
        arg2->expr_data.function_call_data.args_expr->next == NULL)
    {
        HashNode_t *type_node = semcheck_find_preferred_type_node(ctx->symtab,
            arg2->expr_data.function_call_data.id);
        if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            ListNode_t *captured_arg_node = arg2->expr_data.function_call_data.args_expr;
            struct Expression *captured_arg = (struct Expression *)captured_arg_node->cur;
            captured_arg_node->cur = NULL;
            free(captured_arg_node);
            arg2->expr_data.function_call_data.args_expr = NULL;

            if (arg2->expr_data.function_call_data.mangled_id != NULL)
                free(arg2->expr_data.function_call_data.mangled_id);
            if (arg2->expr_data.function_call_data.placeholder_method_name != NULL)
                free(arg2->expr_data.function_call_data.placeholder_method_name);
            if (arg2->expr_data.function_call_data.call_qualifier != NULL)
                free(arg2->expr_data.function_call_data.call_qualifier);
            if (arg2->expr_data.function_call_data.call_kgpc_type != NULL)
                destroy_kgpc_type(arg2->expr_data.function_call_data.call_kgpc_type);
            arg2->expr_data.function_call_data.mangled_id = NULL;
            arg2->expr_data.function_call_data.placeholder_method_name = NULL;
            arg2->expr_data.function_call_data.call_qualifier = NULL;
            arg2->expr_data.function_call_data.call_kgpc_type = NULL;

            char *type_id = arg2->expr_data.function_call_data.id;
            arg2->type = EXPR_VAR_ID;
            arg2->expr_data.id = type_id;

            ListNode_t *third_node = CreateListNode(captured_arg, LIST_EXPR);
            if (third_node != NULL)
            {
                ctx->args_given->next->next = third_node;
                ctx->expr->expr_data.function_call_data.args_expr = ctx->args_given;
            }
        }
    }
}
if (kgpc_getenv("KGPC_DEBUG_EOF") != NULL && ctx->id != NULL &&
    pascal_identifier_equals(ctx->id, "EOF"))
{
    fprintf(stderr,
        "[KGPC_DEBUG_EOF] semcheck_funccall enter id=%s placeholder=%d args=%d procvar=%d call_valid=%d\n",
        ctx->id,
        ctx->expr->expr_data.function_call_data.is_method_call_placeholder,
        ListLength(ctx->expr->expr_data.function_call_data.args_expr),
        ctx->expr->expr_data.function_call_data.is_procedural_var_call,
        ctx->expr->expr_data.function_call_data.is_call_info_valid);
}

/* FPC compiler intrinsics: get_frame, get_pc_addr, get_caller_addr, get_caller_frame
 * These return Pointer and are used for exception handling / stack trace support. */
if (ctx->id != NULL && (pascal_identifier_equals(ctx->id, "get_frame") ||
                   pascal_identifier_equals(ctx->id, "get_pc_addr") ||
                   pascal_identifier_equals(ctx->id, "get_caller_addr") ||
                   pascal_identifier_equals(ctx->id, "get_caller_frame") ||
                   pascal_identifier_equals(ctx->id, "Get_Frame") ||
                   pascal_identifier_equals(ctx->id, "Get_Caller_Addr") ||
                   pascal_identifier_equals(ctx->id, "Get_Caller_Frame")))
{
    /* Type-check any arguments (they should be Pointer) */
    ListNode_t *args = ctx->expr->expr_data.function_call_data.args_expr;
    while (args != NULL)
    {
        struct Expression *arg_expr = (struct Expression *)args->cur;
        if (arg_expr != NULL)
        {
            int arg_type_local = UNKNOWN_TYPE;
            ctx->return_val += semcheck_expr_legacy_tag(&arg_type_local, ctx->symtab, arg_expr, ctx->max_scope_lev, NO_MUTATE);
        }
        args = args->next;
    }
    /* Rewrite to runtime stub */
    free(ctx->expr->expr_data.function_call_data.id);
    ctx->expr->expr_data.function_call_data.id = strdup("kgpc_get_frame");
    if (ctx->expr->expr_data.function_call_data.mangled_id != NULL)
        free(ctx->expr->expr_data.function_call_data.mangled_id);
    ctx->expr->expr_data.function_call_data.mangled_id = strdup("kgpc_get_frame");
    semcheck_reset_function_call_cache(ctx->expr);
    *ctx->type_return = POINTER_TYPE;
    if (ctx->expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(ctx->expr->resolved_kgpc_type);
        ctx->expr->resolved_kgpc_type = NULL;
    }
    ctx->expr->resolved_kgpc_type = create_primitive_type(POINTER_TYPE);
    do { ctx->final_status = ctx->return_val; return FC_CLEANUP; } while (0);
}

if (semcheck_try_rewrite_internconst_call(ctx->type_return, ctx->symtab, ctx->expr))
    do { ctx->final_status = ctx->return_val; return FC_CLEANUP; } while (0);

/* Normalize unit-qualified builtins parsed as extra-arg calls:
 * System.Length(x) may be parsed as Length(System, x). Strip the unit
 * qualifier only when the target identifier is a builtin. */
if (ctx->id != NULL && ctx->args_given != NULL)
{
    struct Expression *first_arg = (struct Expression *)ctx->args_given->cur;
    if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
        first_arg->expr_data.id != NULL &&
        semcheck_is_unit_name(first_arg->expr_data.id))
    {
        HashNode_t *builtin_node = FindIdentInTable(ctx->symtab->builtin_scope->table, ctx->id);
        if (builtin_node != NULL)
        {
            ListNode_t *remaining_args = ctx->args_given->next;
            destroy_expr(first_arg);
            ctx->args_given->cur = NULL;
            free(ctx->args_given);
            ctx->expr->expr_data.function_call_data.args_expr = remaining_args;
            ctx->args_given = remaining_args;
            ctx->was_unit_qualified = 1;
            /* Unit-qualified builtins (System.Length) are NOT method calls */
            ctx->expr->expr_data.function_call_data.is_method_call_placeholder = 0;
        }
    }
}

/* Builtins should resolve before any method/overload handling to avoid
 * accidental mangling into user-defined symbols (e.g., EOF -> eof_void).
 * If this is a method call placeholder (obj.Method()), never intercept as
 * a builtin — the method name may collide with a builtin (e.g., Concat)
 * but method resolution must take priority since methods are stored under
 * mangled names and FindIdent won't find them.
 * Also skip builtins if an active WITH context can resolve the name as a
 * method — e.g., `with LinkScript do Concat(...)` must call the method,
 * not the built-in string Concat. */
int allow_early_builtins = 0;
if (ctx->id != NULL && !ctx->expr->expr_data.function_call_data.is_method_call_placeholder)
{
    HashNode_t *builtin_node = FindIdentInTable(ctx->symtab->builtin_scope->table, ctx->id);
    /* Also check the System unit table — compiler intrinsics (Ord, Chr,
     * Length, High, etc.) live there since per-unit scoping was added. */
    if (builtin_node == NULL)
    {
        int sys_idx = unit_registry_add("System");
        if (sys_idx > 0 && sys_idx < SYMTAB_MAX_UNITS &&
            ctx->symtab->unit_scopes[sys_idx] != NULL)
        {
            HashNode_t *sys_node = FindIdentInTable(
                ctx->symtab->unit_scopes[sys_idx]->table, ctx->id);
            if (sys_node != NULL &&
                (sys_node->hash_type == HASHTYPE_FUNCTION ||
                 sys_node->hash_type == HASHTYPE_PROCEDURE ||
                 sys_node->hash_type == HASHTYPE_BUILTIN_PROCEDURE))
                builtin_node = sys_node;
        }
    }
    if (builtin_node != NULL)
    {
        allow_early_builtins = 1;
        /* Check if an active WITH context provides a method with this name.
         * If so, the WITH method takes priority over the builtin. */
        if (with_context_count > 0)
        {
            struct Expression *with_check = NULL;
            int wm = semcheck_with_try_resolve_method(ctx->id, ctx->symtab, &with_check, ctx->expr->line_num);
            if ((wm == 0 || wm == 2) && with_check != NULL)
            {
                allow_early_builtins = 0;
                destroy_expr(with_check);
            }
        }
    }
}

if (allow_early_builtins && ctx->id != NULL)
{
    /* Table-driven dispatch for builtins that need no argument-type inspection */
    if (semcheck_dispatch_builtin_func(ctx->id, ctx->type_return, ctx->symtab,
                                       ctx->expr, ctx->max_scope_lev, &ctx->final_status))
        return FC_CLEANUP;

    /* UpCase/UpperCase/LowerCase(char) must be intercepted here (in the
     * early builtins section) because the later interception at the end of
     * the builtin block is never reached when overload resolution diverts
     * to a unit-scope overload (e.g. the shortstring UpCase from FPC RTL).
     * Only intercept when the argument is char-like; string overloads must
     * still go through normal overload resolution. */
    if (pascal_identifier_equals(ctx->id, "UpCase") ||
        pascal_identifier_equals(ctx->id, "UpperCase") ||
        pascal_identifier_equals(ctx->id, "LowerCase"))
    {
        ListNode_t *uc_args = ctx->expr->expr_data.function_call_data.args_expr;
        if (uc_args != NULL && uc_args->next == NULL)
        {
            struct Expression *uc_arg = (struct Expression *)uc_args->cur;
            KgpcType *uc_kgpc_type = NULL;
            int uc_cast_type = UNKNOWN_TYPE;
            if (uc_arg != NULL && uc_arg->type == EXPR_FUNCTION_CALL &&
                semcheck_expr_is_char_typecast_call_for_call_local(uc_arg))
                semcheck_try_reinterpret_as_typecast(&uc_cast_type, ctx->symtab, uc_arg, ctx->max_scope_lev);
            int uc_err = semcheck_expr_with_type(&uc_kgpc_type, ctx->symtab, uc_arg, ctx->max_scope_lev, NO_MUTATE);
            int uc_tag = semcheck_tag_from_kgpc(uc_kgpc_type);
            if (uc_err == 0 &&
                (uc_tag == CHAR_TYPE ||
                 semcheck_expr_is_char_like(uc_arg) ||
                 semcheck_kgpc_type_is_char_like_for_call_local(uc_kgpc_type) ||
                 semcheck_expr_is_explicit_char_typecast_for_call_local(uc_arg)))
            {
                if (pascal_identifier_equals(ctx->id, "LowerCase"))
                {
                    if (ctx->expr->expr_data.function_call_data.mangled_id != NULL)
                        free(ctx->expr->expr_data.function_call_data.mangled_id);
                    if (ctx->expr->expr_data.function_call_data.id != NULL)
                        free(ctx->expr->expr_data.function_call_data.id);
                    ctx->expr->expr_data.function_call_data.id = strdup("kgpc_lowercase_char");
                    ctx->expr->expr_data.function_call_data.mangled_id = strdup("kgpc_lowercase_char");
                    semcheck_reset_function_call_cache(ctx->expr);
                    if (ctx->expr->resolved_kgpc_type != NULL)
                    {
                        destroy_kgpc_type(ctx->expr->resolved_kgpc_type);
                        ctx->expr->resolved_kgpc_type = NULL;
                    }
                    ctx->expr->resolved_kgpc_type = create_primitive_type(CHAR_TYPE);
                    semcheck_expr_set_resolved_type(ctx->expr, CHAR_TYPE);
                    *ctx->type_return = CHAR_TYPE;
                    do { ctx->final_status = 0; return FC_CLEANUP; } while (0);
                }
                else
                {
                    do { ctx->final_status = semcheck_builtin_upcase(ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev); return FC_CLEANUP; } while (0);
                }
            }
            if (uc_err == 0 && uc_tag == STRING_TYPE &&
                uc_arg != NULL && uc_arg->type == EXPR_STRING &&
                uc_arg->expr_data.string != NULL &&
                strlen(uc_arg->expr_data.string) == 1)
            {
                do { ctx->final_status = semcheck_builtin_upcase(ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev); return FC_CLEANUP; } while (0);
            }
        }
    }
}

/* Trunc with Currency argument needs special handling — Currency is Int64 internally,
   not a double, so it must call kgpc_trunc_currency instead of kgpc_trunc.
   Must intercept before normal overload resolution (which would coerce Currency to Real). */
if (ctx->id != NULL && pascal_identifier_equals(ctx->id, "Trunc") &&
    !ctx->expr->expr_data.function_call_data.is_method_call_placeholder)
{
    ListNode_t *trunc_args = ctx->expr->expr_data.function_call_data.args_expr;
    if (trunc_args != NULL && trunc_args->next == NULL)
    {
        struct Expression *trunc_arg = (struct Expression *)trunc_args->cur;
        KgpcType *trunc_arg_type = NULL;
        int trunc_err = semcheck_expr_with_type(&trunc_arg_type, ctx->symtab, trunc_arg, ctx->max_scope_lev, NO_MUTATE);
        if (trunc_err == 0 && trunc_arg_type != NULL &&
            semcheck_is_currency_kgpc_type(trunc_arg_type))
            do { ctx->final_status = semcheck_builtin_trunc(ctx->type_return, ctx->symtab, ctx->expr, ctx->max_scope_lev); return FC_CLEANUP; } while (0);
    }
}
    return FC_EARLY_BUILTINS;
}
