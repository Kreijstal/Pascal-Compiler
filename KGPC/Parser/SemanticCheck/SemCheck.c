/*
    Damon Gwinn
    Performs semantic checking on a given parse tree

    NOTE: Max scope level refers to the highest level scope we can reference a variable at
        - 0 is the current scope, 1 is the first above and so on
        - Functions can't have side effects, but they can contain procedures so this is a
            general way to define the maximum scope level

    TODO: CHECK FOR RETURN IN FUNCTIONS (Add a "referenced" flag to symbol table elements. Need it for optimizations anyway...)
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#endif
#include <math.h>
#include "SemCheck.h"
#include "../../flags.h"
#include "../../identifier_utils.h"
#include "../../Optimizer/optimizer.h"
#include "../pascal_frontend.h"
#include "../ParseTree/tree.h"
#include "../ParseTree/tree_types.h"
#include "../ParseTree/KgpcType.h"
#include "../ParseTree/type_tags.h"
#include "../ParseTree/KgpcType.h"
#include "../ParseTree/from_cparser.h"
#include "../ParseTree/operator_registry.h"
#include "../ParseTree/generic_types.h"
#include "../parser_error.h"
#include "../ErrVars.h"
#include "./SymTab/SymTab.h"
#include "./HashTable/HashTable.h"
#include "SemChecks/SemCheck_stmt.h"
#include "SemChecks/SemCheck_expr.h"
#include "NameMangling.h"
#include <stdarg.h>

static ListNode_t *g_semcheck_unit_names = NULL;
static char *g_semcheck_current_unit_name = NULL;
static char *g_semcheck_source_path = NULL;
static char *g_semcheck_source_buffer = NULL;
static size_t g_semcheck_source_length = 0;

void semcheck_set_source_path(const char *path)
{
    if (g_semcheck_source_path != NULL)
    {
        free(g_semcheck_source_path);
        g_semcheck_source_path = NULL;
    }
    if (path != NULL)
        g_semcheck_source_path = strdup(path);
}

void semcheck_set_source_buffer(const char *buffer, size_t length)
{
    if (g_semcheck_source_buffer != NULL)
    {
        free(g_semcheck_source_buffer);
        g_semcheck_source_buffer = NULL;
    }
    g_semcheck_source_length = 0;

    if (buffer == NULL || length == 0)
        return;

    g_semcheck_source_buffer = (char *)malloc(length + 1);
    if (g_semcheck_source_buffer == NULL)
        return;

    memcpy(g_semcheck_source_buffer, buffer, length);
    g_semcheck_source_buffer[length] = '\0';
    g_semcheck_source_length = length;
}

static int semcheck_print_context_from_file(const char *file_path, int line_num, int col_num, int context_lines)
{
    if (file_path == NULL || line_num <= 0)
        return 0;

    FILE *fp = fopen(file_path, "rb");
    if (fp == NULL)
        return 0;

    if (fseek(fp, 0, SEEK_END) != 0)
    {
        fclose(fp);
        return 0;
    }

    long size = ftell(fp);
    if (size < 0)
    {
        fclose(fp);
        return 0;
    }
    if (fseek(fp, 0, SEEK_SET) != 0)
    {
        fclose(fp);
        return 0;
    }

    char *buffer = (char *)malloc((size_t)size + 1);
    if (buffer == NULL)
    {
        fclose(fp);
        return 0;
    }

    size_t read_len = fread(buffer, 1, (size_t)size, fp);
    fclose(fp);
    buffer[read_len] = '\0';

    int printed = print_source_context_from_buffer(buffer, read_len, line_num, col_num, context_lines);
    free(buffer);
    return printed;
}

static void semcheck_unit_names_reset(void)
{
    ListNode_t *cur = g_semcheck_unit_names;
    while (cur != NULL)
    {
        if (cur->cur != NULL)
            free(cur->cur);
        cur = cur->next;
    }
    DestroyList(g_semcheck_unit_names);
    g_semcheck_unit_names = NULL;
    if (g_semcheck_current_unit_name != NULL)
    {
        free(g_semcheck_current_unit_name);
        g_semcheck_current_unit_name = NULL;
    }
}

static void semcheck_unit_name_add(const char *name)
{
    if (name == NULL || name[0] == '\0')
        return;

    ListNode_t *cur = g_semcheck_unit_names;
    while (cur != NULL)
    {
        const char *existing = (const char *)cur->cur;
        if (existing != NULL && pascal_identifier_equals(existing, name))
            return;
        cur = cur->next;
    }

    char *dup = strdup(name);
    if (dup == NULL)
        return;

    ListNode_t *node = CreateListNode(dup, LIST_STRING);
    if (node == NULL)
    {
        free(dup);
        return;
    }

    if (g_semcheck_unit_names == NULL)
        g_semcheck_unit_names = node;
    else
    {
        ListNode_t *tail = g_semcheck_unit_names;
        while (tail->next != NULL)
            tail = tail->next;
        tail->next = node;
    }
}

static void semcheck_unit_names_add_list(ListNode_t *units)
{
    ListNode_t *cur = units;
    while (cur != NULL)
    {
        if (cur->type == LIST_STRING && cur->cur != NULL)
            semcheck_unit_name_add((const char *)cur->cur);
        cur = cur->next;
    }
}

int semcheck_is_unit_name(const char *name)
{
    if (name == NULL || name[0] == '\0')
        return 0;
    if (pascal_identifier_equals(name, "System"))
        return 1;
    if (g_semcheck_current_unit_name != NULL &&
        pascal_identifier_equals(name, g_semcheck_current_unit_name))
        return 1;

    ListNode_t *cur = g_semcheck_unit_names;
    while (cur != NULL)
    {
        const char *existing = (const char *)cur->cur;
        if (existing != NULL && pascal_identifier_equals(existing, name))
            return 1;
        cur = cur->next;
    }
    return 0;
}

/* Helper declared in SemCheck_expr.c */
static const char *semcheck_base_type_name(const char *id)
{
    if (id == NULL)
        return NULL;
    const char *dot = strrchr(id, '.');
    return (dot != NULL && dot[1] != '\0') ? (dot + 1) : id;
}

static int semcheck_map_builtin_type_name_local(const char *id)
{
    if (id == NULL)
        return UNKNOWN_TYPE;
    id = semcheck_base_type_name(id);

    if (pascal_identifier_equals(id, "Integer"))
        return INT_TYPE;
    if (pascal_identifier_equals(id, "LongInt"))
        return LONGINT_TYPE;
    if (pascal_identifier_equals(id, "Int64"))
        return INT64_TYPE;
    if (pascal_identifier_equals(id, "Real"))
        return REAL_TYPE;
    if (pascal_identifier_equals(id, "Single") ||
        pascal_identifier_equals(id, "Double") ||
        pascal_identifier_equals(id, "Extended"))
        return REAL_TYPE;
    if (pascal_identifier_equals(id, "Byte") ||
        pascal_identifier_equals(id, "ShortInt") ||
        pascal_identifier_equals(id, "SmallInt") ||
        pascal_identifier_equals(id, "Word") ||
        pascal_identifier_equals(id, "LongWord") ||
        pascal_identifier_equals(id, "Cardinal") ||
        pascal_identifier_equals(id, "DWord") ||
        pascal_identifier_equals(id, "Int8") ||
        pascal_identifier_equals(id, "UInt8") ||
        pascal_identifier_equals(id, "Int16") ||
        pascal_identifier_equals(id, "UInt16") ||
        pascal_identifier_equals(id, "Int32") ||
        pascal_identifier_equals(id, "UInt32"))
        return INT_TYPE;
    if (pascal_identifier_equals(id, "QWord") ||
        pascal_identifier_equals(id, "UInt64"))
        return INT64_TYPE;
    if (pascal_identifier_equals(id, "String") ||
        pascal_identifier_equals(id, "AnsiString") ||
        pascal_identifier_equals(id, "RawByteString") ||
        pascal_identifier_equals(id, "UnicodeString") ||
        pascal_identifier_equals(id, "WideString"))
        return STRING_TYPE;
    if (pascal_identifier_equals(id, "ShortString"))
        return SHORTSTRING_TYPE;
    if (pascal_identifier_equals(id, "Char") ||
        pascal_identifier_equals(id, "AnsiChar") ||
        pascal_identifier_equals(id, "WideChar") ||
        pascal_identifier_equals(id, "UnicodeChar"))
        return CHAR_TYPE;
    if (pascal_identifier_equals(id, "Boolean"))
        return BOOL;
    if (pascal_identifier_equals(id, "Pointer"))
        return POINTER_TYPE;
    if (pascal_identifier_equals(id, "CodePointer"))
        return POINTER_TYPE;
    if (pascal_identifier_equals(id, "TClass"))
        return POINTER_TYPE;
    if (pascal_identifier_equals(id, "file"))
        return FILE_TYPE;
    return UNKNOWN_TYPE;
}

static char *semcheck_dup_type_id_from_ast(ast_t *node)
{
    if (node == NULL)
        return NULL;
    if (node->sym != NULL && node->sym->name != NULL)
        return strdup(node->sym->name);
    for (ast_t *child = node->child; child != NULL; child = child->next)
    {
        char *found = semcheck_dup_type_id_from_ast(child);
        if (found != NULL)
            return found;
    }
    return NULL;
}

static int semcheck_is_char_alias_name(const char *id)
{
    return (id != NULL &&
        (pascal_identifier_equals(id, "Char") ||
            pascal_identifier_equals(id, "AnsiChar") ||
            pascal_identifier_equals(id, "WideChar") ||
            pascal_identifier_equals(id, "UnicodeChar")));
}

static int semcheck_alias_should_be_char_like(const char *alias_id, const char *target_id)
{
    return (semcheck_is_char_alias_name(alias_id) ||
        semcheck_is_char_alias_name(target_id));
}

static int semcheck_is_currency_type_id(const char *type_id)
{
    const char *base = semcheck_base_type_name(type_id);
    return (base != NULL && pascal_identifier_equals(base, "Currency"));
}

static int map_var_type_to_type_tag(enum VarType var_type)
{
    switch (var_type)
    {
        case HASHVAR_INTEGER:
            return INT_TYPE;
        case HASHVAR_LONGINT:
            return LONGINT_TYPE;
        case HASHVAR_INT64:
            return INT64_TYPE;
        case HASHVAR_REAL:
            return REAL_TYPE;
        case HASHVAR_BOOLEAN:
            return BOOL;
        case HASHVAR_CHAR:
            return CHAR_TYPE;
        case HASHVAR_PCHAR:
            return STRING_TYPE;
        default:
    return UNKNOWN_TYPE;
}

}

static int semcheck_find_ident_with_qualified_fallback(HashNode_t **out, SymTab_t *symtab,
    const char *id)
{
    if (out == NULL || symtab == NULL || id == NULL)
        return -1;

    int found = FindIdent(out, symtab, (char *)id);
    if (found >= 0 && out != NULL && *out != NULL)
        return found;

    const char *dot = strrchr(id, '.');
    if (dot != NULL && dot[1] != '\0')
        return FindIdent(out, symtab, (char *)(dot + 1));

    return found;
}

static HashNode_t *semcheck_find_type_excluding_alias(SymTab_t *symtab, const char *type_id,
    struct TypeAlias *exclude_alias)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, (char *)type_id);
    if (matches == NULL)
    {
        const char *base = semcheck_base_type_name(type_id);
        if (base != NULL && base != type_id)
            matches = FindAllIdents(symtab, (char *)base);
    }
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate != NULL && candidate->hash_type == HASHTYPE_TYPE &&
            candidate->type != NULL)
        {
            struct TypeAlias *candidate_alias = kgpc_type_get_type_alias(candidate->type);
            if (exclude_alias == NULL || candidate_alias != exclude_alias)
            {
                DestroyList(matches);
                return candidate;
            }
        }
        cur = cur->next;
    }

    DestroyList(matches);
    return NULL;
}

static int semcheck_param_decl_equivalent(const Tree_t *lhs, const Tree_t *rhs)
{
    if (lhs == NULL || rhs == NULL)
        return 0;
    if (lhs->type != rhs->type)
        return 0;

    if (lhs->type == TREE_VAR_DECL)
    {
        const char *lhs_id = lhs->tree_data.var_decl_data.type_id;
        const char *rhs_id = rhs->tree_data.var_decl_data.type_id;
        int lhs_tag = lhs->tree_data.var_decl_data.type;
        int rhs_tag = rhs->tree_data.var_decl_data.type;
        int lhs_var = lhs->tree_data.var_decl_data.is_var_param;
        int rhs_var = rhs->tree_data.var_decl_data.is_var_param;

        if (lhs_var != rhs_var)
            return 0;
        if (lhs_id != NULL || rhs_id != NULL)
            return (lhs_id != NULL && rhs_id != NULL && pascal_identifier_equals(lhs_id, rhs_id));
        return lhs_tag == rhs_tag;
    }

    if (lhs->type == TREE_ARR_DECL)
    {
        const char *lhs_id = lhs->tree_data.arr_decl_data.type_id;
        const char *rhs_id = rhs->tree_data.arr_decl_data.type_id;
        int lhs_tag = lhs->tree_data.arr_decl_data.type;
        int rhs_tag = rhs->tree_data.arr_decl_data.type;
        int lhs_start = lhs->tree_data.arr_decl_data.s_range;
        int rhs_start = rhs->tree_data.arr_decl_data.s_range;
        int lhs_end = lhs->tree_data.arr_decl_data.e_range;
        int rhs_end = rhs->tree_data.arr_decl_data.e_range;

        if ((lhs_id != NULL || rhs_id != NULL) &&
            !(lhs_id != NULL && rhs_id != NULL && pascal_identifier_equals(lhs_id, rhs_id)))
            return 0;
        if (lhs_tag != rhs_tag)
            return 0;
        return (lhs_start == rhs_start && lhs_end == rhs_end);
    }

    return 0;
}

static int semcheck_subprogram_signatures_equivalent(const Tree_t *lhs, const Tree_t *rhs)
{
    if (lhs == NULL || rhs == NULL)
        return 0;
    if (lhs->type != TREE_SUBPROGRAM || rhs->type != TREE_SUBPROGRAM)
        return 0;

    ListNode_t *lhs_args = lhs->tree_data.subprogram_data.args_var;
    ListNode_t *rhs_args = rhs->tree_data.subprogram_data.args_var;

    while (lhs_args != NULL && rhs_args != NULL)
    {
        Tree_t *lhs_decl = (Tree_t *)lhs_args->cur;
        Tree_t *rhs_decl = (Tree_t *)rhs_args->cur;
        if (!semcheck_param_decl_equivalent(lhs_decl, rhs_decl))
            return 0;
        lhs_args = lhs_args->next;
        rhs_args = rhs_args->next;
    }

    return (lhs_args == NULL && rhs_args == NULL);
}

static ListNode_t *semcheck_create_builtin_param(const char *name, int type_tag)
{
    char *param_name = strdup(name);
    if (param_name == NULL)
        return NULL;

    ListNode_t *ids = CreateListNode(param_name, LIST_STRING);
    if (ids == NULL)
        return NULL;

    Tree_t *decl = mk_vardecl(0, ids, type_tag, NULL, 0, 0, NULL, NULL, NULL, NULL);
    if (decl == NULL)
        return NULL;

    return CreateListNode(decl, LIST_TREE);
}

static ListNode_t *semcheck_create_builtin_param_var(const char *name, int type_tag)
{
    char *param_name = strdup(name);
    if (param_name == NULL)
        return NULL;

    ListNode_t *ids = CreateListNode(param_name, LIST_STRING);
    if (ids == NULL)
        return NULL;

    Tree_t *decl = mk_vardecl(0, ids, type_tag, NULL, 1, 0, NULL, NULL, NULL, NULL);
    if (decl == NULL)
        return NULL;

    return CreateListNode(decl, LIST_TREE);
}

/* Adds built-in functions */
void semcheck_add_builtins(SymTab_t *symtab)
{
    /* Disabled: rely on FPC RTL definitions instead of injecting builtins. */
    (void)symtab;
}

/* Semantic check for a program */
int semcheck_program(SymTab_t *symtab, Tree_t *tree)
{
    int return_val;
    assert(tree != NULL);
    assert(symtab != NULL);
    assert(tree->type == TREE_PROGRAM_TYPE);

    return_val = 0;

    PushScope(symtab);

    semcheck_unit_names_reset();
    semcheck_unit_name_add("System");
    semcheck_unit_names_add_list(tree->tree_data.program_data.uses_units);

    return_val += semcheck_id_not_main(tree->tree_data.program_data.program_id);

    /* TODO: Push program name onto scope */

    /* TODO: Fix line number bug here */
    return_val += semcheck_args(symtab, tree->tree_data.program_data.args_char,
      tree->line_num);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after args: %d\n", return_val);
#endif

    return_val += predeclare_enum_literals(symtab, tree->tree_data.program_data.type_declaration);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    return_val += predeclare_types(symtab, tree->tree_data.program_data.type_declaration);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after type predeclare: %d\n", return_val);
#endif

    /* Predeclare subprograms so they can be referenced in const initializers */
    return_val += predeclare_subprograms(symtab, tree->tree_data.program_data.subprograms, 0, NULL);
    if (getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
    {
        ListNode_t *debug_cur = tree->tree_data.program_data.type_declaration;
        while (debug_cur != NULL)
        {
            if (debug_cur->type == LIST_TREE && debug_cur->cur != NULL)
            {
                Tree_t *debug_tree = (Tree_t *)debug_cur->cur;
                if (debug_tree->type == TREE_TYPE_DECL && debug_tree->tree_data.type_decl_data.id != NULL)
                {
                    fprintf(stderr, "[KGPC] program type decl: %s kind=%d\n",
                        debug_tree->tree_data.type_decl_data.id,
                        debug_tree->tree_data.type_decl_data.kind);
                }
            }
            debug_cur = debug_cur->next;
        }
    }
    
    /* Four-pass processing for constants to handle all reference patterns:
     *
     * Pass 1: Imported unit untyped constants.
     * Pass 2: Imported unit typed constants (e.g., DirectorySeparator from system.p).
     * Pass 3: Local untyped constants.
     * Pass 4: Local typed constants.
     */

    /* Pass 1: Imported unit untyped constants */
    return_val += semcheck_const_decls_imported(symtab, tree->tree_data.program_data.const_declaration);

    /* Pass 2: Imported unit typed constants */
    ListNode_t *unit_typed_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.program_data.var_declaration, 1);  /* from_unit_only=true */
    if (unit_typed_consts != NULL)
    {
        return_val += semcheck_decls(symtab, unit_typed_consts);
        DestroyList(unit_typed_consts);
    }

    /* Pass 3: Local untyped constants */
    return_val += semcheck_const_decls_local(symtab, tree->tree_data.program_data.const_declaration);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after consts: %d\n", return_val);
#endif

    /* Pass 4: Local typed constants (can reference both unit typed consts and local untyped consts) */
    ListNode_t *local_typed_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.program_data.var_declaration, 0);  /* from_unit_only=false */
    if (local_typed_consts != NULL)
    {
        return_val += semcheck_decls(symtab, local_typed_consts);
        DestroyList(local_typed_consts);
    }

    return_val += semcheck_type_decls(symtab, tree->tree_data.program_data.type_declaration);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after types: %d\n", return_val);
#endif

    return_val += semcheck_decls(symtab, tree->tree_data.program_data.var_declaration);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after vars: %d\n", return_val);
#endif

    return_val += semcheck_subprograms(symtab, tree->tree_data.program_data.subprograms, 0, NULL);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after subprograms: %d\n", return_val);
#endif

    return_val += semcheck_stmt(symtab, tree->tree_data.program_data.body_statement, INT_MAX);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after body: %d\n", return_val);
#endif

    // Semantic check finalization statements from units
    if (tree->tree_data.program_data.finalization_statements != NULL) {
        ListNode_t *final_node = tree->tree_data.program_data.finalization_statements;
        while (final_node != NULL) {
            if (final_node->type == LIST_STMT && final_node->cur != NULL) {
                struct Statement *final_stmt = (struct Statement *)final_node->cur;
                return_val += semcheck_stmt(symtab, final_stmt, INT_MAX);
            }
            final_node = final_node->next;
        }
    }

    if(optimize_flag() > 0 && return_val == 0)
    {
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] Before optimize: body_statement = %p\n", 
                    (void*)tree->tree_data.program_data.body_statement);
            if (tree->tree_data.program_data.body_statement != NULL) {
                fprintf(stderr, "[KGPC] Body statement type: %d\n", 
                        tree->tree_data.program_data.body_statement->type);
                if (tree->tree_data.program_data.body_statement->type == STMT_COMPOUND_STATEMENT) {
                    fprintf(stderr, "[KGPC] Compound statement list: %p\n",
                            (void*)tree->tree_data.program_data.body_statement->stmt_data.compound_statement);
                }
            }
        }
        optimize(symtab, tree);
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] After optimize: body_statement = %p\n", 
                    (void*)tree->tree_data.program_data.body_statement);
        }
    }

    /* Keep the outermost scope alive for code generation. DestroySymTab will clean it up. */
    semcheck_unit_names_reset();
    return return_val;
}

/* Semantic check for a unit */
int semcheck_unit(SymTab_t *symtab, Tree_t *tree)
{
    int return_val;
    const char *debug_steps = getenv("KGPC_DEBUG_SEMSTEPS");
    assert(tree != NULL);
    assert(symtab != NULL);
    assert(tree->type == TREE_UNIT);

    return_val = 0;

    PushScope(symtab);

    semcheck_unit_names_reset();
    semcheck_unit_name_add("System");
    semcheck_unit_name_add(tree->tree_data.unit_data.unit_id);
    if (tree->tree_data.unit_data.unit_id != NULL)
        g_semcheck_current_unit_name = strdup(tree->tree_data.unit_data.unit_id);
    semcheck_unit_names_add_list(tree->tree_data.unit_data.interface_uses);
    semcheck_unit_names_add_list(tree->tree_data.unit_data.implementation_uses);

    /* Check interface section */
    int before = return_val;
    return_val += predeclare_enum_literals(symtab, tree->tree_data.unit_data.interface_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface enum predeclare +%d (total %d)\n",
                return_val - before, return_val);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    before = return_val;
    return_val += predeclare_types(symtab, tree->tree_data.unit_data.interface_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface type predeclare +%d (total %d)\n",
                return_val - before, return_val);
    
    /* Check implementation section - predeclare types BEFORE subprograms */
    before = return_val;
    return_val += predeclare_enum_literals(symtab, tree->tree_data.unit_data.implementation_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl enum predeclare +%d (total %d)\n",
                return_val - before, return_val);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    before = return_val;
    return_val += predeclare_types(symtab, tree->tree_data.unit_data.implementation_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl type predeclare +%d (total %d)\n",
                return_val - before, return_val);
    
    /* Now predeclare subprograms AFTER all types (interface + implementation) are predeclared.
     * This ensures function return types referencing implementation types can be resolved.
     * The same subprograms list is shared by interface and implementation. */
    before = return_val;

    /* Debug: dump subprograms list before predeclaration */
    if (getenv("KGPC_DEBUG_SUBPROGRAMS_LIST") != NULL)
    {
        fprintf(stderr, "[SUBPROGRAMS_LIST] Dumping subprograms list for unit:\n");
        ListNode_t *debug_cur = tree->tree_data.unit_data.subprograms;
        int count = 0;
        while (debug_cur != NULL)
        {
            count++;
            if (debug_cur->type == LIST_TREE && debug_cur->cur != NULL)
            {
                Tree_t *sub = (Tree_t *)debug_cur->cur;
                if (sub->type == TREE_SUBPROGRAM)
                {
                    fprintf(stderr, "[SUBPROGRAMS_LIST] %d: %s (line %d)\n",
                            count, sub->tree_data.subprogram_data.id, sub->line_num);
                }
            }
            debug_cur = debug_cur->next;
        }
        fprintf(stderr, "[SUBPROGRAMS_LIST] Total: %d subprograms\n", count);
    }

    return_val += predeclare_subprograms(symtab, tree->tree_data.unit_data.subprograms, 0, NULL);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] subprogram predeclare +%d (total %d)\n",
                return_val - before, return_val);
    
    /* Continue interface section processing */
    /* Pass 1: Imported unit untyped constants. */
    before = return_val;
    return_val += semcheck_const_decls_imported(symtab, tree->tree_data.unit_data.interface_const_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 2: Imported unit typed constants (e.g., prelude/system typed consts). */
    before = return_val;
    ListNode_t *typed_iface_unit_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.interface_var_decls, 1);
    if (typed_iface_unit_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_iface_unit_consts);
        DestroyList(typed_iface_unit_consts);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface typed unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 3: Local untyped const declarations. */
    before = return_val;
    return_val += semcheck_const_decls_local(symtab, tree->tree_data.unit_data.interface_const_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface consts +%d (total %d)\n",
                return_val - before, return_val);
                
    /* Pass 4: Local interface typed constants - they can reference regular constants */
    before = return_val;
    ListNode_t *typed_iface_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.interface_var_decls, 0);
    if (typed_iface_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_iface_consts);
        DestroyList(typed_iface_consts);
    }
    before = return_val;
    return_val += semcheck_type_decls(symtab, tree->tree_data.unit_data.interface_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface types +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    return_val += semcheck_decls(symtab, tree->tree_data.unit_data.interface_var_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface vars +%d (total %d)\n",
                return_val - before, return_val);

    /* Continue implementation section processing */
    /* Pass 1: Imported unit untyped constants from implementation section. */
    before = return_val;
    return_val += semcheck_const_decls_imported(symtab, tree->tree_data.unit_data.implementation_const_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 2: Imported unit typed constants from implementation section. */
    before = return_val;
    ListNode_t *typed_impl_unit_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.implementation_var_decls, 1);
    if (typed_impl_unit_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_impl_unit_consts);
        DestroyList(typed_impl_unit_consts);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl typed unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 3: Local untyped const declarations. */
    before = return_val;
    return_val += semcheck_const_decls_local(symtab, tree->tree_data.unit_data.implementation_const_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl consts +%d (total %d)\n",
                return_val - before, return_val);
                
    /* Pass 4: Local implementation typed constants */
    before = return_val;
    ListNode_t *typed_impl_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.implementation_var_decls, 0);
    if (typed_impl_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_impl_consts);
        DestroyList(typed_impl_consts);
    }
    before = return_val;
    return_val += semcheck_type_decls(symtab, tree->tree_data.unit_data.implementation_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl types +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    return_val += semcheck_decls(symtab, tree->tree_data.unit_data.implementation_var_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl vars +%d (total %d)\n",
                return_val - before, return_val);

    /* Check subprograms */
    before = return_val;
    return_val += semcheck_subprograms(symtab, tree->tree_data.unit_data.subprograms, 0, NULL);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] subprograms +%d (total %d)\n",
                return_val - before, return_val);

    /* Check initialization section if present */
    if (tree->tree_data.unit_data.initialization != NULL) {
        before = return_val;
        return_val += semcheck_stmt(symtab, tree->tree_data.unit_data.initialization, INT_MAX);
        if (debug_steps != NULL && return_val != before)
            fprintf(stderr, "[SemCheck] initialization +%d (total %d)\n",
                    return_val - before, return_val);
    }

    /* Check finalization section if present */
    if (tree->tree_data.unit_data.finalization != NULL) {
        before = return_val;
        return_val += semcheck_stmt(symtab, tree->tree_data.unit_data.finalization, INT_MAX);
        if (debug_steps != NULL && return_val != before)
            fprintf(stderr, "[SemCheck] finalization +%d (total %d)\n",
                    return_val - before, return_val);
    }

    semcheck_unit_names_reset();
    return return_val;
}


/* Adds arguments to the symbol table */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num)
{
    ListNode_t *cur;
    int return_val, func_return;
    assert(symtab != NULL);

    return_val = 0;

    cur = args;

    /* Checking if they are declarations
        NOTE: Mismatching arg types is an error */
    if(cur != NULL)
        if(cur->type == LIST_TREE)
            return semcheck_decls(symtab, args);

    while(cur != NULL)
    {
        /* If not a list of declarations, must be a list of strings */
        assert(cur->type == LIST_STRING);

        /* UNTYPED procedure parameters - use NULL KgpcType */
        func_return = PushVarOntoScope_Typed(symtab, (char *)cur->cur, NULL);

        /* Greater than 0 signifies an error */
        if(func_return > 0)
        {
            semcheck_error_with_context("Error on line %d, redeclaration of name %s!\n",
                line_num, (char *)cur->cur);
            return_val += func_return;
        }

        cur = cur->next;
    }

    return return_val;
}

/* Pushes a bunch of declarations onto the current scope */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls)
{
    ListNode_t *cur, *ids, *ids_head;
    Tree_t *tree;
    int return_val, func_return;

    enum VarType var_type;

    assert(symtab != NULL);

    return_val = 0;
    cur = decls;
    while(cur != NULL)
    {
        /* Any declaration is always a tree */
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_VAR_DECL || tree->type == TREE_ARR_DECL);

        if (tree->type == TREE_VAR_DECL)
            ids_head = tree->tree_data.var_decl_data.ids;
        else
            ids_head = tree->tree_data.arr_decl_data.ids;

        ids = ids_head;

        HashNode_t *resolved_type = NULL;
        if (tree->type == TREE_VAR_DECL && tree->tree_data.var_decl_data.type_id != NULL)
            resolved_type = semcheck_find_preferred_type_node(symtab,
                tree->tree_data.var_decl_data.type_id);
        if (tree->type == TREE_VAR_DECL)
        {
            if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
            {
                destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
            }
            if (resolved_type != NULL && resolved_type->type != NULL)
            {
                kgpc_type_retain(resolved_type->type);
                tree->tree_data.var_decl_data.cached_kgpc_type = resolved_type->type;
            }
            else if (resolved_type == NULL && tree->tree_data.var_decl_data.type_id != NULL)
            {
                /* Fallback: Create KgpcType for built-in type names not in symbol table */
                const char *type_id = tree->tree_data.var_decl_data.type_id;
                int builtin_tag = semcheck_map_builtin_type_name_local(type_id);
                if (builtin_tag != UNKNOWN_TYPE)
                {
                    tree->tree_data.var_decl_data.cached_kgpc_type = create_primitive_type(builtin_tag);
                }
            }
        }
        int skip_initializer = 0;
        while(ids != NULL)
        {
            assert(ids->cur != NULL);
            assert(ids->type == LIST_STRING);

            /* Variable declarations */
            if(tree->type == TREE_VAR_DECL)
            {
                if (tree->tree_data.var_decl_data.is_typed_const)
                {
                    HashNode_t *existing_node = NULL;
                    if (FindIdent(&existing_node, symtab, (char *)ids->cur) >= 0 &&
                        existing_node != NULL && existing_node->is_typed_const)
                    {
                        mark_hashnode_unit_info(existing_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                        skip_initializer = 1;
                        goto next_identifier;
                    }
                }
                if (ids->cur != NULL && strcmp((char *)ids->cur, "Sock") == 0) {
#ifdef DEBUG
                     fprintf(stderr, "DEBUG: semcheck_decls processing Sock. type_id=%s resolved_type=%p\n",
                         tree->tree_data.var_decl_data.type_id ? tree->tree_data.var_decl_data.type_id : "<null>",
                         resolved_type);
#endif
                }

                if (tree->tree_data.var_decl_data.type_id != NULL)
                {
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && tree->tree_data.var_decl_data.is_typed_const)
                        fprintf(stderr, "[SemCheck] Typed const with type_id: %s, var: %s\n",
                            tree->tree_data.var_decl_data.type_id,
                            ids && ids->cur ? (char*)ids->cur : "<null>");
                    HashNode_t *type_node = resolved_type;
                    const char *type_id = tree->tree_data.var_decl_data.type_id;
                    int declared_type = tree->tree_data.var_decl_data.type;
                    if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur &&
                        pascal_identifier_equals((char*)ids->cur, "Self"))
                    {
                        fprintf(stderr, "[KGPC] semcheck_decls Self: type_id=%s declared_type=%d type_node=%p type_node->type=%p kind=%d\n",
                            type_id ? type_id : "<null>", declared_type, (void*)type_node,
                            type_node && type_node->type ? (void*)type_node->type : NULL,
                            type_node && type_node->type ? type_node->type->kind : -1);
                    }
                    
                    if (declared_type == SET_TYPE)
                    {
                        KgpcType *set_type = create_primitive_type(SET_TYPE);
                        if (set_type != NULL &&
                            tree->tree_data.var_decl_data.inline_type_alias != NULL)
                        {
                            kgpc_type_set_type_alias(set_type,
                                tree->tree_data.var_decl_data.inline_type_alias);
                        }
                        if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                        {
                            destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                            tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
                        }
                        if (set_type != NULL)
                        {
                            kgpc_type_retain(set_type);
                            tree->tree_data.var_decl_data.cached_kgpc_type = set_type;
                        }
                        func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, set_type);
                        if (func_return == 0)
                        {
                            HashNode_t *var_node = NULL;
                            if (FindIdent(&var_node, symtab, (char *)ids->cur) != -1 && var_node != NULL)
                            {
                                var_node->is_var_parameter = tree->tree_data.var_decl_data.is_var_param ? 1 : 0;
                                mark_hashnode_unit_info(var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                            }
                        }
                        goto next_identifier;
                    }

                    /* If declared as pointer type (^TypeName), handle inline pointer */
                    if (declared_type == POINTER_TYPE)
                    {
                        /* This is ^TypeName where TypeName is stored in type_id.
                         * We need to create a pointer type pointing to TypeName. */
                        KgpcType *points_to = NULL;
                        
                        /* Try to resolve the target type */
                        if (type_node != NULL && type_node->type != NULL)
                        {
                            kgpc_type_retain(type_node->type);
                            points_to = type_node->type;
                        }
                        else
                        {
                            /* Check builtin types for the pointer target */
                            int target_tag = semcheck_map_builtin_type_name_local(type_id);
                            if (target_tag != UNKNOWN_TYPE)
                            {
                                points_to = create_primitive_type(target_tag);
                            }
                            else
                            {
                                /* Unknown target type - create a placeholder */
                                points_to = NULL;
                            }
                        }
                        
                        KgpcType *pointer_type = create_pointer_type(points_to);
                        func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, pointer_type);
                        if (func_return == 0)
                        {
                            HashNode_t *var_node = NULL;
                            if (FindIdent(&var_node, symtab, (char *)ids->cur) != -1 && var_node != NULL)
                            {
                                var_node->is_var_parameter = tree->tree_data.var_decl_data.is_var_param ? 1 : 0;
                                mark_hashnode_unit_info(var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                            }
                        }
                        goto next_identifier;
                    }
                    
                    /* Check if it's a builtin type even if not in symbol table */
                    if (type_node == NULL)
                    {
                        if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur)
                            fprintf(stderr, "[KGPC] semcheck_decls: %s has type_id=%s but type_node is NULL\n",
                                (char*)ids->cur, type_id);
                        /* Check for builtin types */
                        if (pascal_identifier_equals(type_id, "Integer"))
                            var_type = HASHVAR_INTEGER;
                        else if (pascal_identifier_equals(type_id, "LongInt"))
                            var_type = HASHVAR_LONGINT;
                        else if (pascal_identifier_equals(type_id, "SizeUInt") || pascal_identifier_equals(type_id, "QWord") || 
                                 pascal_identifier_equals(type_id, "NativeUInt"))
                            var_type = HASHVAR_LONGINT;  /* Unsigned 64-bit on x86-64 */
                        else if (pascal_identifier_equals(type_id, "Real") || pascal_identifier_equals(type_id, "Double"))
                            var_type = HASHVAR_REAL;
                        else if (pascal_identifier_equals(type_id, "String") || pascal_identifier_equals(type_id, "AnsiString") ||
                                 pascal_identifier_equals(type_id, "RawByteString") ||
                                 pascal_identifier_equals(type_id, "UnicodeString") ||
                                 pascal_identifier_equals(type_id, "WideString"))
                            var_type = HASHVAR_PCHAR;
                        else if (pascal_identifier_equals(type_id, "ShortString"))
                        {
                            /* ShortString is array[0..255] of Char with length at index 0 */
                            var_type = HASHVAR_ARRAY;
                        }
                        else if (pascal_identifier_equals(type_id, "Char") ||
                                 pascal_identifier_equals(type_id, "AnsiChar"))
                            var_type = HASHVAR_CHAR;
                        else if (pascal_identifier_equals(type_id, "Boolean"))
                            var_type = HASHVAR_BOOLEAN;
                        else if (pascal_identifier_equals(type_id, "Pointer"))
                            var_type = HASHVAR_POINTER;
                        else if (pascal_identifier_equals(type_id, "Byte") || pascal_identifier_equals(type_id, "Word"))
                            var_type = HASHVAR_INTEGER;
                        /* Handle FPC system pointer types (PInt64, PByte, etc.) */
                        else if (pascal_identifier_equals(type_id, "PInt64") ||
                                 pascal_identifier_equals(type_id, "PByte") ||
                                 pascal_identifier_equals(type_id, "PWord") ||
                                 pascal_identifier_equals(type_id, "PLongInt") ||
                                 pascal_identifier_equals(type_id, "PLongWord") ||
                                 pascal_identifier_equals(type_id, "PInteger") ||
                                 pascal_identifier_equals(type_id, "PCardinal") ||
                                 pascal_identifier_equals(type_id, "PQWord") ||
                                 pascal_identifier_equals(type_id, "PPointer") ||
                                 pascal_identifier_equals(type_id, "PBoolean"))
                            var_type = HASHVAR_POINTER;
                        else
                        {
                            semantic_error(tree->line_num, 0, "undefined type %s", type_id);
                            return_val++;
                            var_type = HASHVAR_UNTYPED;
                        }
                    }
                    else
                    {
                        if (type_node->type != NULL && type_node->type->kind == TYPE_KIND_PROCEDURE)
                        {
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, type_node->type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = NULL;
                                if (FindIdent(&var_node, symtab, (char *)ids->cur) != -1 && var_node != NULL)
                                {
                                    var_node->is_var_parameter = tree->tree_data.var_decl_data.is_var_param ? 1 : 0;
                                    mark_hashnode_unit_info(var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                }
                            }
                            goto next_identifier;
                        }

                        int declared_type_tag = tree->tree_data.var_decl_data.type;
                        int needs_inline_pointer = (declared_type_tag == POINTER_TYPE &&
                            (type_node->type == NULL || type_node->type->kind != TYPE_KIND_POINTER));

                        if (needs_inline_pointer)
                        {
                            KgpcType *points_to = NULL;
                            if (type_node->type != NULL)
                            {
                                kgpc_type_retain(type_node->type);
                                points_to = type_node->type;
                            }
                            else
                            {
                                struct RecordType *target_record = get_record_type_from_node(type_node);
                                if (target_record != NULL)
                                {
                                    points_to = create_record_type(target_record);
                                }
                                else
                                {
                                    enum VarType target_var_type = get_var_type_from_node(type_node);
                                    int primitive_tag = map_var_type_to_type_tag(target_var_type);
                                    if (primitive_tag != UNKNOWN_TYPE)
                                        points_to = create_primitive_type(primitive_tag);
                                }
                            }

                            KgpcType *pointer_type = create_pointer_type(points_to);
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, pointer_type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = NULL;
                                if (FindIdent(&var_node, symtab, (char *)ids->cur) != -1 && var_node != NULL)
                                {
                                    var_node->is_var_parameter = tree->tree_data.var_decl_data.is_var_param ? 1 : 0;
                                    mark_hashnode_unit_info(var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                }
                            }
                            goto next_identifier;
                        }

                        var_type = get_var_type_from_node(type_node);
                        int resolved_tag = map_var_type_to_type_tag(var_type);
                        if (resolved_tag != UNKNOWN_TYPE)
                            tree->tree_data.var_decl_data.type = resolved_tag;
                        struct TypeAlias *alias = get_type_alias_from_node(type_node);
                        if (alias != NULL && alias->is_array)
                        {
                            int start = alias->array_start;
                            int end = alias->array_end;
                            if (alias->is_open_array)
                            {
                                start = 0;
                                end = -1;
                            }

                            /* Get element type - it might be a primitive type or a type reference */
                            KgpcType *element_type = NULL;
                            int element_type_tag = alias->array_element_type;

                            /* If element type is a type reference, resolve it */
                            int element_type_owned = 0;  /* Track if we own element_type */
                            if (element_type_tag == UNKNOWN_TYPE && alias->array_element_type_id != NULL)
                            {
                                HashNode_t *element_type_node = NULL;
                                if (FindIdent(&element_type_node, symtab, alias->array_element_type_id) >= 0 &&
                                    element_type_node != NULL && element_type_node->type != NULL)
                                {
                                    element_type = element_type_node->type;
                                    /* CRITICAL: Retain element_type since it's borrowed from symbol table
                                     * and create_array_type takes ownership. Without this, both the symbol
                                     * table and array_type would try to free the same type, causing double-free. */
                                    kgpc_type_retain(element_type);
                                    element_type_owned = 1;
                                }
                            }
                            else if (element_type_tag != UNKNOWN_TYPE)
                            {
                                /* Direct primitive type tag - use create_primitive_type */
                                element_type = create_primitive_type(element_type_tag);
                                element_type_owned = 1;
                            }

                            /* If element type is still NULL, create an unknown type to avoid crash */
                            if (element_type == NULL)
                            {
                                element_type = create_primitive_type(UNKNOWN_TYPE);
                                element_type_owned = 1;
                            }

                            /* Create array KgpcType - takes ownership of element_type */
                            KgpcType *array_type = create_array_type(element_type, start, end);
                            assert(array_type != NULL && "Failed to create array type");
                            
                            /* Set type_alias on KgpcType so it's properly propagated */
                            kgpc_type_set_type_alias(array_type, alias);
                            
                            func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, array_type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = NULL;
                                if (FindIdent(&var_node, symtab, (char *)ids->cur) != -1 && var_node != NULL)
                                {
                                    mark_hashnode_unit_info(var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                }
                            }

                            goto next_identifier;
                        }
                        
                        /* For non-array type references (e.g., enum, set, file, record), create KgpcType from type_node */
                        KgpcType *var_kgpc_type = NULL;
                        if (type_node->type != NULL)
                        {
                            /* Type node already has a KgpcType - reference it (don't clone) */
                            var_kgpc_type = type_node->type;
                            if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur &&
                                pascal_identifier_equals((char*)ids->cur, "Self"))
                            {
                                fprintf(stderr, "[KGPC] semcheck_decls: Pushing Self with type_node->type kind=%d\n",
                                    var_kgpc_type ? var_kgpc_type->kind : -1);
                            }
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                        }
                        else
                        {
                            /* Fallback: create KgpcType from legacy fields using helpers */
                            struct RecordType *record_type = get_record_type_from_node(type_node);
                            if (record_type != NULL)
                            {
                                /* Use the canonical RecordType, not a clone */
                                var_kgpc_type = create_record_type(record_type);
                            }
                            else if (var_type == HASHVAR_POINTER)
                            {
                                /* For pointer types, we need to create a pointer KgpcType */
                                /* Get the TypeAlias to find what the pointer points to */
                                struct TypeAlias *type_alias = get_type_alias_from_node(type_node);
                                if (type_alias != NULL && type_alias->is_pointer)
                                {
                                    KgpcType *points_to = NULL;
                                    
                                    /* Try to resolve the target type */
                                    if (type_alias->pointer_type_id != NULL)
                                    {
                                        HashNode_t *target_node = NULL;
                                        if (FindIdent(&target_node, symtab, type_alias->pointer_type_id) >= 0 &&
                                            target_node != NULL && target_node->type != NULL)
                                        {
                                            points_to = target_node->type;
                                        }
                                    }
                                    
                                    /* If we couldn't resolve it, create a placeholder based on pointer_type */
                                    if (points_to == NULL && type_alias->pointer_type != UNKNOWN_TYPE)
                                    {
                                        points_to = create_primitive_type(type_alias->pointer_type);
                                    }
                                    
                                    if (points_to != NULL)
                                    {
                                        var_kgpc_type = create_pointer_type(points_to);
                                        kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                                    }
                                }
                            }
                            else
                            {
                                var_kgpc_type = kgpc_type_from_var_type(var_type);
                            }
                            
                            struct TypeAlias *type_alias = get_type_alias_from_node(type_node);
                            if (var_kgpc_type != NULL && type_alias != NULL && var_type != HASHVAR_POINTER)
                            {
                                kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                            }
                            
                            /* Always use _Typed variant, even if KgpcType is NULL (UNTYPED) */
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                            if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur &&
                                pascal_identifier_equals((char*)ids->cur, "Self"))
                                fprintf(stderr, "[KGPC] semcheck_decls: PushVarOntoScope_Typed for Self returned %d\n", func_return);
                        }

                        if (func_return == 0)
                        {
                            HashNode_t *var_node = NULL;
                            if (FindIdent(&var_node, symtab, (char *)ids->cur) != -1 && var_node != NULL)
                            {
                                var_node->is_var_parameter = tree->tree_data.var_decl_data.is_var_param ? 1 : 0;
                                mark_hashnode_unit_info(var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                            }
                        }
                        goto next_identifier;
                    }
                }
                else if (tree->tree_data.var_decl_data.inferred_type)
                {
                    /* For type inference, use INTEGER as placeholder - will be replaced later */
                    var_type = HASHVAR_INTEGER;  /* Placeholder */
                }
                else if (tree->tree_data.var_decl_data.type == UNKNOWN_TYPE &&
                    tree->tree_data.var_decl_data.type_id == NULL &&
                    tree->tree_data.var_decl_data.is_untyped_param)
                {
                    var_type = HASHVAR_UNTYPED;
                }
                else if(tree->tree_data.var_decl_data.type == INT_TYPE)
                    var_type = HASHVAR_INTEGER;
                else if(tree->tree_data.var_decl_data.type == LONGINT_TYPE)
                    var_type = HASHVAR_LONGINT;
                else if(tree->tree_data.var_decl_data.type == BOOL)
                    var_type = HASHVAR_BOOLEAN;
                else if(tree->tree_data.var_decl_data.type == SET_TYPE)
                    var_type = HASHVAR_SET;
                else if(tree->tree_data.var_decl_data.type == ENUM_TYPE)
                    var_type = HASHVAR_ENUM;
                else if(tree->tree_data.var_decl_data.type == STRING_TYPE)
                    var_type = HASHVAR_PCHAR;
                else if(tree->tree_data.var_decl_data.type == RECORD_TYPE)
                    var_type = HASHVAR_RECORD;
                else if(tree->tree_data.var_decl_data.type == FILE_TYPE)
                    var_type = HASHVAR_FILE;
                else if(tree->tree_data.var_decl_data.type == TEXT_TYPE)
                    var_type = HASHVAR_TEXT;
                else
                    var_type = HASHVAR_REAL;
                
                /* Create KgpcType for typed variables */
                KgpcType *var_kgpc_type = NULL;
                if (resolved_type != NULL && resolved_type->type != NULL)
                {
                    /* Use KgpcType from resolved type if available */
                    var_kgpc_type = resolved_type->type;
                }
                else if (tree->tree_data.var_decl_data.inline_record_type != NULL)
                {
                    /* Handle inline record type declarations */
                    var_kgpc_type = create_record_type(clone_record_type(tree->tree_data.var_decl_data.inline_record_type));
                }
                else
                {
                    /* Special handling for ShortString - create as array[0..255] of Char */
                    if (var_type == HASHVAR_ARRAY &&
                        tree->tree_data.var_decl_data.type_id != NULL &&
                        pascal_identifier_equals(tree->tree_data.var_decl_data.type_id, "ShortString"))
                    {
                        /* Create ShortString as array[0..255] of Char */
                        KgpcType *char_type = create_primitive_type(CHAR_TYPE);
                        var_kgpc_type = create_array_type(char_type, 0, 255);
                        if (var_kgpc_type != NULL)
                        {
                            struct TypeAlias alias = {0};
                            alias.is_array = 1;
                            alias.array_start = 0;
                            alias.array_end = 255;
                            alias.array_element_type = CHAR_TYPE;
                            alias.array_element_type_id = "char";  /* Will be duplicated by copy_type_alias */
                            alias.is_shortstring = 1;
                            kgpc_type_set_type_alias(var_kgpc_type, &alias);
                        }
                    }
                    /* Handle inline array types (e.g., array[0..2] of PChar) */
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL &&
                             tree->tree_data.var_decl_data.inline_type_alias->is_array)
                    {
                        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                            fprintf(stderr, "[SemCheck] Processing inline array for var: %s\n",
                                ids && ids->cur ? (char*)ids->cur : "<null>");
                        struct TypeAlias *alias = tree->tree_data.var_decl_data.inline_type_alias;
                        int start = alias->array_start;
                        int end = alias->array_end;
                        if (alias->is_open_array)
                        {
                            start = 0;
                            end = -1;
                        }

                        /* Get element type */
                        KgpcType *element_type = NULL;
                        int element_type_tag = alias->array_element_type;
                        int element_type_borrowed = 0;  /* Track if borrowed from symbol table */

                        if (element_type_tag == UNKNOWN_TYPE && alias->array_element_type_id != NULL)
                        {
                            HashNode_t *element_type_node = NULL;
                            if (FindIdent(&element_type_node, symtab, alias->array_element_type_id) >= 0 &&
                                element_type_node != NULL && element_type_node->type != NULL)
                            {
                                element_type = element_type_node->type;
                                element_type_borrowed = 1;
                            }
                            else
                            {
                                /* Check for builtin type */
                                int builtin_tag = semcheck_map_builtin_type_name_local(alias->array_element_type_id);
                                if (builtin_tag != UNKNOWN_TYPE)
                                    element_type = create_primitive_type(builtin_tag);
                            }
                        }
                        else if (element_type_tag != UNKNOWN_TYPE)
                        {
                            element_type = create_primitive_type(element_type_tag);
                        }

                        if (element_type != NULL)
                        {
                            /* CRITICAL: Retain element_type if borrowed from symbol table
                             * since create_array_type takes ownership. */
                            if (element_type_borrowed)
                                kgpc_type_retain(element_type);
                            var_kgpc_type = create_array_type(element_type, start, end);
                            kgpc_type_set_type_alias(var_kgpc_type, alias);
                        }
                        else
                        {
                            /* Fallback to var_type if element type unresolved */
                            var_kgpc_type = kgpc_type_from_var_type(var_type);
                        }
                    }
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL)
                    {
                        struct TypeAlias *alias = tree->tree_data.var_decl_data.inline_type_alias;
                        if (alias->storage_size > 0)
                        {
                            var_kgpc_type = create_primitive_type_with_size(alias->base_type,
                                (int)alias->storage_size);
                        }
                        else
                        {
                            var_kgpc_type = create_primitive_type(alias->base_type);
                        }
                        if (var_kgpc_type != NULL)
                            kgpc_type_set_type_alias(var_kgpc_type, alias);
                    }
                    else
                    {
                        /* Create KgpcType from var_type */
                        var_kgpc_type = kgpc_type_from_var_type(var_type);
                    }
                    
                    if (var_kgpc_type != NULL)
                    {
                        if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                        {
                            destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                            tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
                        }
                        kgpc_type_retain(var_kgpc_type);
                        tree->tree_data.var_decl_data.cached_kgpc_type = var_kgpc_type;
                    }

                    /* Add metadata from resolved_type if present */
                    if (var_kgpc_type != NULL && resolved_type != NULL)
                    {
                        struct TypeAlias *type_alias = get_type_alias_from_node(resolved_type);
                        if (type_alias != NULL)
                        {
                            kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                        }
                        struct RecordType *record_type = get_record_type_from_node(resolved_type);
                        if (record_type != NULL && var_kgpc_type->kind == TYPE_KIND_RECORD)
                        {
                            /* Use the canonical RecordType from the symbol table,
                             * not a clone. This ensures type identity checks work correctly. */
                            var_kgpc_type->info.record_info = record_type;
                        }
                    }
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL &&
                        var_kgpc_type != NULL)
                    {
                        kgpc_type_set_type_alias(var_kgpc_type,
                            tree->tree_data.var_decl_data.inline_type_alias);
                    }
                }
                
                if (var_kgpc_type != NULL && kgpc_type_is_record(var_kgpc_type))
                {
                    struct RecordType *var_record = kgpc_type_get_record(var_kgpc_type);
                    if (var_record != NULL)
                    {
                        /* Skip size computation for generic templates (not yet specialized)
                         * Size can only be computed after type parameters are substituted */
                        int is_unspecialized_generic = (var_record->generic_decl != NULL && 
                                                         var_record->num_generic_args == 0);
                        
                        if (!is_unspecialized_generic)
                        {
                            long long record_size = 0;
                            if (semcheck_compute_record_size(symtab, var_record, &record_size,
                                    tree->line_num) != 0)
                            {
                                return_val += 1;
                            }
                        }
                    }
                }

                /* Always use _Typed variant, even if KgpcType is NULL (UNTYPED) */
                /* Use PushArrayOntoScope_Typed for ShortString (which is array[0..255] of Char) */
                int is_shortstring = (var_type == HASHVAR_ARRAY && 
                                     tree->tree_data.var_decl_data.type_id != NULL &&
                                     pascal_identifier_equals(tree->tree_data.var_decl_data.type_id, "ShortString"));
                
                if (is_shortstring)
                    func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                else
                    func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                
                if (func_return == 0)
                {
                    HashNode_t *var_node = NULL;
                    if (FindIdent(&var_node, symtab, (char *)ids->cur) != -1 && var_node != NULL)
                    {
                        var_node->is_var_parameter = tree->tree_data.var_decl_data.is_var_param ? 1 : 0;
                        mark_hashnode_unit_info(var_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                    }
                }
            }
            /* Array declarations */
            else
            {
                assert(tree->type == TREE_ARR_DECL);
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] Processing TREE_ARR_DECL: %s is_typed_const=%d\n",
                        ids && ids->cur ? (char*)ids->cur : "<null>",
                        tree->tree_data.arr_decl_data.is_typed_const);

                KgpcType *element_type = NULL;
                int element_type_borrowed = 0;  /* Track if borrowed from symbol table */
                int is_array_of_const = (tree->tree_data.arr_decl_data.type == ARRAY_OF_CONST_TYPE);

                /* If type_id is specified, resolve it to get the element type */
                if (!is_array_of_const && tree->tree_data.arr_decl_data.type_id != NULL)
                {
                    HashNode_t *element_type_node = NULL;
                    element_type_node = semcheck_find_preferred_type_node(symtab,
                        tree->tree_data.arr_decl_data.type_id);
                    if (element_type_node != NULL)
                    {
                        /* Use the KgpcType from the resolved type node */
                        element_type = element_type_node->type;
                        element_type_borrowed = 1;  /* Mark as borrowed */
                        if (element_type == NULL)
                        {
                            /* Fallback for migration: some nodes may not have KgpcType populated yet.
                             * Try to construct KgpcType from legacy record type information. */
                            struct RecordType *record_type = get_record_type_from_node(element_type_node);
                            if (record_type != NULL)
                            {
                                /* Use the canonical RecordType, not a clone */
                                element_type = create_record_type(record_type);
                                element_type_borrowed = 0;  /* New type, not borrowed */
                            }
                        }
                    }
                    else
                    {
                        /* Fallback: check for builtin types not in symbol table */
                        const char *type_id = tree->tree_data.arr_decl_data.type_id;
                        int builtin_type = semcheck_map_builtin_type_name_local(type_id);
                        if (builtin_type != UNKNOWN_TYPE)
                        {
                            element_type = create_primitive_type(builtin_type);
                        }
                        else
                        {
                            semcheck_error_with_context("Error on line %d: undefined type %s\n",
                                tree->line_num, tree->tree_data.arr_decl_data.type_id);
                            return_val++;
                        }
                    }
                }
                
                /* If element type not resolved from type_id, use primitive type */
                if (element_type == NULL && !is_array_of_const)
                {
                    if(tree->tree_data.arr_decl_data.type == INT_TYPE)
                        var_type = HASHVAR_INTEGER;
                    else if(tree->tree_data.arr_decl_data.type == LONGINT_TYPE)
                        var_type = HASHVAR_LONGINT;
                    else if(tree->tree_data.arr_decl_data.type == BOOL)
                        var_type = HASHVAR_BOOLEAN;
                    else if(tree->tree_data.arr_decl_data.type == STRING_TYPE)
                        var_type = HASHVAR_PCHAR;
                    else if(tree->tree_data.arr_decl_data.type == SHORTSTRING_TYPE)
                        var_type = HASHVAR_PCHAR;  /* ShortString is array of char */
                    else if(tree->tree_data.arr_decl_data.type == CHAR_TYPE)
                        var_type = HASHVAR_CHAR;
                    else if(tree->tree_data.arr_decl_data.type == REAL_TYPE)
                        var_type = HASHVAR_REAL;
                    else {
                        /* Unknown type - report error and default to real */
                        fprintf(stderr, "Warning: Unknown array element type %d for %s, defaulting to real\n",
                                tree->tree_data.arr_decl_data.type,
                                ids && ids->cur ? (char*)ids->cur : "<unknown>");
                        var_type = HASHVAR_REAL;
                    }
                    
                    element_type = kgpc_type_from_var_type(var_type);
                    assert(element_type != NULL && "Array element type must be createable from VarType");
                }
                
                /* Resolve array bounds from constant identifiers if necessary.
                 * This handles inline array declarations like: var arr: array[1..N] of integer
                 * where N is a const. The parser stores the original range string (e.g., "1..N")
                 * in range_str, which we parse and resolve here. */
                int start_bound = tree->tree_data.arr_decl_data.s_range;
                int end_bound = tree->tree_data.arr_decl_data.e_range;
                
                if (tree->tree_data.arr_decl_data.range_str != NULL)
                {
                    char *range_str = tree->tree_data.arr_decl_data.range_str;
                    char *sep = strstr(range_str, "..");
                    
                    if (sep != NULL)
                    {
                        /* Parse "start..end" format */
                        size_t start_len = sep - range_str;
                        char *start_str = (char *)malloc(start_len + 1);
                        char *end_str = strdup(sep + 2);
                        
                        if (start_str != NULL && end_str != NULL)
                        {
                            strncpy(start_str, range_str, start_len);
                            start_str[start_len] = '\0';
                            
                            /* Trim whitespace */
                            char *s = start_str;
                            while (*s == ' ' || *s == '\t') s++;
                            char *e = end_str;
                            while (*e == ' ' || *e == '\t') e++;
                            char *p = s + strlen(s) - 1;
                            while (p > s && (*p == ' ' || *p == '\t')) *p-- = '\0';
                            p = e + strlen(e) - 1;
                            while (p > e && (*p == ' ' || *p == '\t')) *p-- = '\0';
                            
                            /* Try to resolve start bound as constant */
                            long long start_val = 0;
                            if (resolve_const_identifier(symtab, s, &start_val) == 0)
                            {
                                start_bound = (int)start_val;
                            }
                            else
                            {
                                /* Try parsing as integer literal */
                                char *endptr;
                                long num = strtol(s, &endptr, 10);
                                if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                                    start_bound = (int)num;
                            }
                            
                            /* Try to resolve end bound as constant */
                            long long end_val = 0;
                            if (resolve_const_identifier(symtab, e, &end_val) == 0)
                            {
                                end_bound = (int)end_val;
                            }
                            else
                            {
                                /* Try parsing as integer literal */
                                char *endptr;
                                long num = strtol(e, &endptr, 10);
                                if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                                    end_bound = (int)num;
                            }
                            
                            free(start_str);
                            free(end_str);
                        }
                    }
                }
                
                /* CRITICAL FIX: Update the tree's s_range and e_range fields with the resolved bounds.
                 * Downstream code (especially the x86-64 code generator) checks arr->e_range < arr->s_range
                 * to determine if an array is dynamic. Without updating these fields, arrays declared with
                 * constant expressions (e.g., array[1..N] where N is a const) would be incorrectly treated
                 * as dynamic arrays, leading to segmentation faults. */
                tree->tree_data.arr_decl_data.s_range = start_bound;
                tree->tree_data.arr_decl_data.e_range = end_bound;
                
                KgpcType *array_type = NULL;
                if (is_array_of_const)
                {
                    array_type = create_array_of_const_type();
                }
                else
                {
                    /* CRITICAL: Retain element_type if borrowed from symbol table
                     * since create_array_type takes ownership. */
                    if (element_type_borrowed && element_type != NULL)
                        kgpc_type_retain(element_type);
                    array_type = create_array_type(
                        element_type,
                        start_bound,
                        end_bound
                    );
                }
                assert(array_type != NULL && "Failed to create array type");

                /* If the element type was specified by a type_id (like TAlfa), preserve that information
                 * by creating a minimal TypeAlias and attaching it to the array_type. This allows
                 * nested array indexing to work correctly (e.g., Keywords[1][1] where Keywords is
                 * array[1..5] of TAlfa and TAlfa is array[1..10] of char). */
                if (!is_array_of_const)
                {
                    struct TypeAlias temp_alias = {0};
                    int has_alias = 0;
                    
                    if (tree->tree_data.arr_decl_data.type_id != NULL)
                    {
                        temp_alias.is_array = 1;
                        temp_alias.array_start = start_bound;
                        temp_alias.array_end = end_bound;
                        temp_alias.array_element_type_id = tree->tree_data.arr_decl_data.type_id;  /* Will be duplicated by copy_type_alias */
                        temp_alias.array_element_type = tree->tree_data.arr_decl_data.type;
                        has_alias = 1;
                    }

                    if (tree->tree_data.arr_decl_data.is_shortstring)
                    {
                        if (!has_alias)
                        {
                            temp_alias.is_array = 1;
                            temp_alias.array_start = start_bound;
                            temp_alias.array_end = end_bound;
                            temp_alias.array_element_type = CHAR_TYPE;
                            temp_alias.array_element_type_id = "char";  /* Will be duplicated by copy_type_alias */
                            has_alias = 1;
                        }
                        temp_alias.is_shortstring = 1;
                    }

                    if (has_alias)
                        kgpc_type_set_type_alias(array_type, &temp_alias);
                }
                
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] Pushing array: %s, array_type=%p kind=%d elem_kind=%d\n",
                        ids && ids->cur ? (char*)ids->cur : "<null>",
                        (void*)array_type, array_type ? array_type->kind : -1,
                        (array_type && array_type->kind == TYPE_KIND_ARRAY && array_type->info.array_info.element_type) ?
                            array_type->info.array_info.element_type->kind : -1);
                func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, array_type);
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] PushArrayOntoScope_Typed returned: %d\n", func_return);
            }

            /* Greater than 0 signifies an error */
            if(func_return > 0)
            {
                semantic_error(tree->line_num, 0, "redeclaration of name %s",
                    (char *)ids->cur);
                return_val += func_return;
            }
            else
            {
                HashNode_t *decl_node = NULL;
                if (FindIdent(&decl_node, symtab, (char *)ids->cur) != -1 && decl_node != NULL)
                {
                    if (tree->type == TREE_VAR_DECL)
                    {
                        decl_node->is_var_parameter = tree->tree_data.var_decl_data.is_var_param ? 1 : 0;
                        mark_hashnode_unit_info(decl_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                    }
                    else
                    {
                        mark_hashnode_unit_info(decl_node,
                            tree->tree_data.arr_decl_data.defined_in_unit,
                            tree->tree_data.arr_decl_data.unit_is_public);
                    }
                }
            }

next_identifier:
            ids = ids->next;
        }

        cur = cur->next;
        if (skip_initializer)
            continue;

        if (tree->type == TREE_VAR_DECL && tree->tree_data.var_decl_data.initializer != NULL)
        {
            struct Statement *init_stmt = tree->tree_data.var_decl_data.initializer;
            
            /* Handle COMPOUND_STATEMENT initializers (from record const lowering) separately */
            if (init_stmt->type == STMT_COMPOUND_STATEMENT)
            {
                /* This is a lowered record const - just semantic check the compound statement */
                return_val += semcheck_stmt(symtab, init_stmt, INT_MAX);
            }
            else if (ids_head == NULL || ids_head->next != NULL)
            {
                semcheck_error_with_context("Error on line %d, type inference initializers must declare a single identifier.\n",
                    tree->line_num);
                ++return_val;
            }
            else
            {
                char *var_name = (char *)ids_head->cur;
                HashNode_t *var_node = NULL;
                if (FindIdent(&var_node, symtab, var_name) == -1 || var_node == NULL)
                {
                    semcheck_error_with_context("Error on line %d, failed to resolve variable %s for initializer.\n",
                        tree->line_num, var_name);
                    ++return_val;
                }
                else
                {
                    struct Statement *init_stmt = tree->tree_data.var_decl_data.initializer;
                    struct Expression *init_expr = init_stmt->stmt_data.var_assign_data.expr;
                    int is_default_param = (init_stmt->type == STMT_VAR_ASSIGN &&
                        init_stmt->stmt_data.var_assign_data.var == NULL);
                    if (init_expr == NULL)
                    {
                        semcheck_error_with_context("Error on line %d, initializer expression is NULL for %s.\n",
                            tree->line_num, var_name);
                        ++return_val;
                        /* Skip remaining processing for this variable but continue with the loop */
                    }
                    else
                    {
                        if (tree->tree_data.var_decl_data.is_typed_const && var_node != NULL)
                            var_node->is_typed_const = 1;

                        if (tree->tree_data.var_decl_data.is_typed_const &&
                            !tree->tree_data.var_decl_data.currency_scaled &&
                            semcheck_is_currency_type_id(tree->tree_data.var_decl_data.type_id))
                        {
                            double real_value = 0.0;
                            if (evaluate_real_const_expr(symtab, init_expr, &real_value) == 0)
                            {
                                long long scaled = llround(real_value * 10000.0);
                                destroy_expr(init_expr);
                                init_expr = mk_inum(tree->line_num, scaled);
                                init_stmt->stmt_data.var_assign_data.expr = init_expr;
                                tree->tree_data.var_decl_data.currency_scaled = 1;
                            }
                        }

                        int expr_type = UNKNOWN_TYPE;
                        if (init_expr->type == EXPR_RECORD_CONSTRUCTOR && init_expr->record_type == NULL)
                        {
                            struct RecordType *record_type = NULL;
                            if (var_node->type != NULL && kgpc_type_is_record(var_node->type))
                                record_type = kgpc_type_get_record(var_node->type);
                            else if (var_node->type != NULL && kgpc_type_is_pointer(var_node->type) &&
                                var_node->type->info.points_to != NULL &&
                                kgpc_type_is_record(var_node->type->info.points_to))
                                record_type = kgpc_type_get_record(var_node->type->info.points_to);
                            else if (tree->tree_data.var_decl_data.type_id != NULL)
                            {
                                HashNode_t *type_node = NULL;
                                if (FindIdent(&type_node, symtab,
                                        tree->tree_data.var_decl_data.type_id) >= 0 &&
                                    type_node != NULL)
                                    record_type = hashnode_get_record_type(type_node);
                            }
                            init_expr->record_type = record_type;
                        }
                        return_val += semcheck_expr_main(&expr_type, symtab, init_expr, INT_MAX, NO_MUTATE);

                        if (tree->tree_data.var_decl_data.is_typed_const && var_node != NULL &&
                            init_expr->type != EXPR_ADDR && init_expr->type != EXPR_ADDR_OF_PROC &&
                            init_expr->type != EXPR_RECORD_CONSTRUCTOR &&
                            init_expr->type != EXPR_ARRAY_LITERAL)
                        {
                        if (expression_is_string(symtab, init_expr))
                            {
                                char *string_value = NULL;
                                if (evaluate_string_const_expr(symtab, init_expr, &string_value) == 0)
                                {
                                    if (var_node->const_string_value != NULL)
                                        free(var_node->const_string_value);
                                    var_node->const_string_value = string_value;
                                    if (string_value != NULL &&
                                        string_value[0] != '\0' && string_value[1] == '\0')
                                    {
                                        var_node->const_int_value = (unsigned char)string_value[0];
                                    }
                                }
                            }
                            else if (expression_contains_real_literal_impl(symtab, init_expr))
                            {
                                double real_value = 0.0;
                                if (evaluate_real_const_expr(symtab, init_expr, &real_value) == 0)
                                    var_node->const_real_value = real_value;
                            }
                            else if (expression_is_set_const_expr(symtab, init_expr))
                            {
                                unsigned char set_bytes[32];
                                size_t set_size = 0;
                                long long mask = 0;
                                int is_char_set = 0;
                                if (evaluate_set_const_bytes(symtab, init_expr, set_bytes, sizeof(set_bytes),
                                        &set_size, &mask, &is_char_set) == 0)
                                {
                                    unsigned char *copy = (unsigned char *)malloc(set_size);
                                    if (copy != NULL)
                                    {
                                        memcpy(copy, set_bytes, set_size);
                                        if (var_node->const_set_value != NULL)
                                            free(var_node->const_set_value);
                                        var_node->const_set_value = copy;
                                        var_node->const_set_size = (int)set_size;
                                    }
                                }
                            }
                            else
                            {
                                long long value = 0;
                                if (evaluate_const_expr(symtab, init_expr, &value) == 0)
                                    var_node->const_int_value = value;
                            }
                        }

                    if (expr_type == UNKNOWN_TYPE)
                    {
                        if (!is_default_param)
                        {
                            semcheck_error_with_context("Error on line %d, unable to infer type for %s.\n", tree->line_num, var_name);
                            ++return_val;
                        }
                    }
                    else
                    {
                        enum VarType inferred_var_type = HASHVAR_UNTYPED;
                        int normalized_type = expr_type;

                        switch(expr_type)
                        {
                            case INT_TYPE:
                            case LONGINT_TYPE:
                                inferred_var_type = HASHVAR_INTEGER;
                                normalized_type = (expr_type == LONGINT_TYPE) ? LONGINT_TYPE : INT_TYPE;
                                break;
                            case INT64_TYPE:
                                inferred_var_type = HASHVAR_INT64;
                                normalized_type = INT64_TYPE;
                                break;
                            case BOOL:
                                inferred_var_type = HASHVAR_BOOLEAN;
                                normalized_type = BOOL;
                                break;
                            case REAL_TYPE:
                                inferred_var_type = HASHVAR_REAL;
                                normalized_type = REAL_TYPE;
                                break;
                            case CHAR_TYPE:
                                inferred_var_type = HASHVAR_CHAR;
                                normalized_type = CHAR_TYPE;
                                if (tree->tree_data.var_decl_data.type_id == NULL)
                                    tree->tree_data.var_decl_data.type_id = strdup("char");
                                break;
                            case STRING_TYPE:
                                inferred_var_type = HASHVAR_PCHAR;
                                normalized_type = STRING_TYPE;
                                if (tree->tree_data.var_decl_data.type_id == NULL)
                                    tree->tree_data.var_decl_data.type_id = strdup("string");
                                break;
                            case SET_TYPE:
                                inferred_var_type = HASHVAR_SET;
                                normalized_type = SET_TYPE;
                                break;
                            case ENUM_TYPE:
                                inferred_var_type = HASHVAR_ENUM;
                                normalized_type = ENUM_TYPE;
                                break;
                            case POINTER_TYPE:
                                inferred_var_type = HASHVAR_POINTER;
                                normalized_type = POINTER_TYPE;
                                break;
                            case RECORD_TYPE:
                                inferred_var_type = HASHVAR_RECORD;
                                normalized_type = RECORD_TYPE;
                                break;
                            default:
                                semcheck_error_with_context("Error on line %d, unsupported inferred type for %s.\n",
                                    tree->line_num, var_name);
                                ++return_val;
                                inferred_var_type = HASHVAR_UNTYPED;
                                break;
                        }

                        if (tree->tree_data.var_decl_data.inferred_type)
                        {
                            if (inferred_var_type != HASHVAR_UNTYPED)
                            {
                                tree->tree_data.var_decl_data.type = normalized_type;
                                /* Replace or create KgpcType with inferred type */
                                KgpcType *inferred_kgpc_type = create_primitive_type(normalized_type);
                                if (inferred_kgpc_type != NULL)
                                {
                                    if (var_node->type != NULL)
                                    {
                                        /* Free old type and replace */
                                        destroy_kgpc_type(var_node->type);
                                    }
                                    var_node->type = inferred_kgpc_type;
                                    /* Legacy field will be populated by helper if needed */
                                }
                            }
                        }
                        else if (!is_default_param)
                        {
                            enum VarType current_var_type = get_var_type_from_node(var_node);
                            int compatible = (inferred_var_type == current_var_type);
                            
                            /* Allow mixing Integer and LongInt for initializers */
                            if (!compatible)
                            {
                                if ((inferred_var_type == HASHVAR_INTEGER || inferred_var_type == HASHVAR_LONGINT) &&
                                    (current_var_type == HASHVAR_INTEGER || current_var_type == HASHVAR_LONGINT))
                                {
                                    compatible = 1;
                                }
                            }
                            if (!compatible)
                            {
                                if (current_var_type == HASHVAR_INT64 &&
                                    (inferred_var_type == HASHVAR_INTEGER || inferred_var_type == HASHVAR_LONGINT ||
                                     inferred_var_type == HASHVAR_INT64))
                                {
                                    compatible = 1;
                                }
                                else if (inferred_var_type == HASHVAR_INT64 &&
                                         (current_var_type == HASHVAR_INTEGER || current_var_type == HASHVAR_LONGINT))
                                {
                                    compatible = 1;
                                }
                            }

                            if (!compatible && current_var_type == HASHVAR_PCHAR && expr_type == CHAR_TYPE)
                            {
                                compatible = 1;
                            }
                            
                            /* Allow pointer type initializers (including nil) for pointer variables.
                             * Both the initializer expression type and the declared variable type must
                             * be pointer types. This enables patterns like: var my_ptr: PChar = nil; */
                            if (!compatible)
                            {
                                int inferred_is_pointer = (inferred_var_type == HASHVAR_POINTER || expr_type == POINTER_TYPE);
                                int current_is_pointer = (current_var_type == HASHVAR_POINTER);
                                int current_is_proc = (current_var_type == HASHVAR_PROCEDURE);
                                if (var_node->type != NULL)
                                {
                                    current_is_pointer |= kgpc_type_is_pointer(var_node->type);
                                    if (kgpc_type_get_legacy_tag(var_node->type) == POINTER_TYPE)
                                        current_is_pointer = 1;
                                    if (var_node->type->kind == TYPE_KIND_PROCEDURE)
                                        current_is_proc = 1;
                                }
                                if (tree->tree_data.var_decl_data.type == POINTER_TYPE)
                                    current_is_pointer = 1;
                                if (tree->tree_data.var_decl_data.type == PROCEDURE)
                                    current_is_proc = 1;
                                if (inferred_is_pointer && current_is_pointer)
                                    compatible = 1;
                                if (!compatible && inferred_is_pointer && current_is_proc)
                                    compatible = 1;
                            }

                            if (!compatible && current_var_type == HASHVAR_RECORD && expr_type == STRING_TYPE)
                            {
                                const char *record_id = NULL;
                                if (var_node->type != NULL)
                                {
                                    struct RecordType *record = kgpc_type_get_record(var_node->type);
                                    if (record != NULL && record->type_id != NULL)
                                        record_id = record->type_id;
                                }
                                if (record_id == NULL)
                                    record_id = tree->tree_data.var_decl_data.type_id;
                                if (record_id != NULL &&
                                    (strcasecmp(record_id, "TGuid") == 0 || strcasecmp(record_id, "GUID") == 0))
                                {
                                    compatible = 1;
                                }
                            }

                            if (!compatible)
                            {
                                semcheck_error_with_context("Error on line %d, initializer type mismatch for %s.\n",
                                    tree->line_num, var_name);
                                ++return_val;
                            }
                        }
                    }
                    }  /* Close else at line 2054 */
                }  /* Close else at line 2043 */
            }  /* Close else at line 2033 */
        }  /* Close if at line 2017 */
        else if (tree->type == TREE_ARR_DECL && tree->tree_data.arr_decl_data.initializer != NULL)
        {
            return_val += semcheck_stmt(symtab, tree->tree_data.arr_decl_data.initializer, INT_MAX);
        }

    }

    return return_val;
}

/* Semantic check on an entire subprogram */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev)
{
    int return_val, func_return;
    int new_max_scope;
    enum TreeType sub_type;
    struct Statement *body;
    HashNode_t *hash_return;

    assert(symtab != NULL);
    assert(subprogram != NULL);
    
#ifdef DEBUG
    fprintf(stderr, "DEBUG: semcheck_subprogram called for %s\n", subprogram->tree_data.subprogram_data.id);
#endif

    assert(subprogram->type == TREE_SUBPROGRAM);

    const char *prev_owner = semcheck_get_current_method_owner();
    char *owner_copy = NULL;
    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        const char *mangled = subprogram->tree_data.subprogram_data.mangled_id;
        const char *sep = strstr(mangled, "__");
        if (sep != NULL && sep != mangled)
        {
            size_t len = (size_t)(sep - mangled);
            owner_copy = (char *)malloc(len + 1);
            if (owner_copy != NULL)
            {
                memcpy(owner_copy, mangled, len);
                owner_copy[len] = '\0';
                semcheck_set_current_method_owner(owner_copy);
            }
        }
    }

    /* For class methods, copy default parameter values from the class declaration
     * to the implementation. This is needed because Pascal allows defaults only in
     * the declaration, not in the implementation. */
    copy_method_decl_defaults_to_impl(symtab, subprogram);

    /* Record lexical nesting depth so codegen can reason about static links accurately.
     * Store depth as parent depth + 1 so the top-level program has depth 1 and
     * nested subprograms continue to increase. */
    subprogram->tree_data.subprogram_data.nesting_level = max_scope_lev + 1;
    int default_requires = (subprogram->tree_data.subprogram_data.nesting_level > 1 &&
        !subprogram->tree_data.subprogram_data.defined_in_unit);
    subprogram->tree_data.subprogram_data.requires_static_link = default_requires ? 1 : 0;

    char *id_to_use_for_lookup;

    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val = 0;
    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    // --- Name Mangling Logic ---
    // Only set mangled_id if not already set by predeclare_subprogram (which handles
    // nested functions with unique parent$child naming)
    if (subprogram->tree_data.subprogram_data.mangled_id == NULL) {
        static int debug_external = -1;
        if (debug_external == -1)
            debug_external = (getenv("KGPC_DEBUG_EXTERNAL") != NULL);
        const char *explicit_name = subprogram->tree_data.subprogram_data.cname_override;
        if (explicit_name != NULL) {
            char *overload_mangled = MangleFunctionName(
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.args_var,
                symtab);
            if (overload_mangled != NULL)
                subprogram->tree_data.subprogram_data.mangled_id = overload_mangled;
            else
                subprogram->tree_data.subprogram_data.mangled_id = strdup(explicit_name);
        } else if (subprogram->tree_data.subprogram_data.cname_flag) {
            const char *export_name = subprogram->tree_data.subprogram_data.id;
            if (debug_external) {
                fprintf(stderr, "[SemCheck] cname_flag id=%s alias=%s\n",
                    subprogram->tree_data.subprogram_data.id,
                    export_name != NULL ? export_name : "(null)");
            }
            char *overload_mangled = MangleFunctionName(
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.args_var,
                symtab);
            if (overload_mangled != NULL)
                subprogram->tree_data.subprogram_data.mangled_id = overload_mangled;
            else if (export_name != NULL)
                subprogram->tree_data.subprogram_data.mangled_id = strdup(export_name);
            else
                subprogram->tree_data.subprogram_data.mangled_id = NULL;
        } else {
            // Pass the symbol table to the mangler
            subprogram->tree_data.subprogram_data.mangled_id = MangleFunctionName(
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.args_var,
                symtab);
        }
    }
    id_to_use_for_lookup = subprogram->tree_data.subprogram_data.id;

    /* Check if already declared (e.g., by predeclare_subprogram in two-pass approach) */
    HashNode_t *existing_decl = NULL;
    int already_declared = 0;
    
    /* For overloaded functions, find the correct overload by matching mangled name */
    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
        ListNode_t *cur = all_matches;
        
        while (cur != NULL && existing_decl == NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL && candidate->mangled_id != NULL &&
                strcmp(candidate->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
            {
                existing_decl = candidate;
                already_declared = 1;
            }
            cur = cur->next;
        }
        
        if (all_matches != NULL)
            DestroyList(all_matches);
    }
    
    /* Fallback to simple lookup if no mangled name or no match found */
    if (!already_declared)
        already_declared = (FindIdent(&existing_decl, symtab, id_to_use_for_lookup) == 0);

    if (already_declared && existing_decl != NULL &&
        subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        if (existing_decl->mangled_id != NULL)
            free(existing_decl->mangled_id);
        existing_decl->mangled_id = strdup(subprogram->tree_data.subprogram_data.mangled_id);
    }

    /**** FIRST PLACING SUBPROGRAM ON THE CURRENT SCOPE ****/
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        /* Create KgpcType for the procedure */
        /* Create KgpcType for the procedure */
        KgpcType *proc_type = NULL;
        int created_new_type = 0;

        if (already_declared && existing_decl != NULL && existing_decl->type != NULL)
        {
            proc_type = existing_decl->type;
            Tree_t *prev_def = proc_type->info.proc_info.definition;
            if (subprogram->tree_data.subprogram_data.statement_list != NULL &&
                (prev_def == NULL || prev_def->tree_data.subprogram_data.statement_list == NULL))
            {
                proc_type->info.proc_info.definition = subprogram;
            }
            /* Copy default parameter values from forward declaration to implementation */
            copy_default_values_to_impl_params(
                proc_type->info.proc_info.params,
                subprogram->tree_data.subprogram_data.args_var);
        }
        else
        {
            proc_type = create_procedure_type(
                subprogram->tree_data.subprogram_data.args_var,
                NULL  /* procedures have no return type */
            );
            if (proc_type != NULL)
            {
                proc_type->info.proc_info.definition = subprogram;
                if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                    proc_type->info.proc_info.return_type_id =
                        strdup(subprogram->tree_data.subprogram_data.return_type_id);
            }
            created_new_type = 1;
        }
        
        /* ARCHITECTURAL FIX: Resolve array bounds in parameter types now that constants are in scope */
        if (proc_type != NULL && proc_type->info.proc_info.params != NULL)
        {
            ListNode_t *param = proc_type->info.proc_info.params;
            while (param != NULL)
            {
                if (param->type == LIST_TREE && param->cur != NULL)
                {
                    Tree_t *param_tree = (Tree_t *)param->cur;
                    /* For array parameters, resolve the bounds */
                    if (param_tree->type == TREE_ARR_DECL)
                    {
                        /* If element type is a named type, look it up to get proper KgpcType */
                        if (param_tree->tree_data.arr_decl_data.type_id != NULL)
                        {
                            HashNode_t *elem_type_node = NULL;
                            if (FindIdent(&elem_type_node, symtab, param_tree->tree_data.arr_decl_data.type_id) != -1 &&
                                elem_type_node != NULL && elem_type_node->type != NULL)
                            {
                                /* Element type resolved - bounds should be in the tree node */
                                /* Nothing to do here */
                            }
                        }
                    }
                }
                param = param->next;
            }
        }
        
        // Use the typed version to properly set the KgpcType
        // Skip if already declared
        if (!already_declared)
        {
            func_return = PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                            subprogram->tree_data.subprogram_data.mangled_id,
                            proc_type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            FindIdent(&existing_decl, symtab, id_to_use_for_lookup);
        }
        else
        {
            func_return = 0;  /* No error since it's expected to be already declared */
            
            /* If we created a new type but it was already declared (e.g. existing had no type), update it */
            if (created_new_type && existing_decl != NULL && existing_decl->type == NULL)
            {
                existing_decl->type = proc_type;
            }
            else if (created_new_type)
            {
                /* We created a type but didn't use it (shouldn't happen if logic is correct, but for safety) */
                destroy_kgpc_type(proc_type);
            }
        }

        PushScope(symtab);
        
        /* For method implementations, add class vars to scope */
        add_class_vars_to_method_scope(symtab, subprogram->tree_data.subprogram_data.id);
        
        if (existing_decl != NULL && existing_decl->type != NULL)
        {
            kgpc_type_retain(existing_decl->type);
            PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id,
                existing_decl->type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            destroy_kgpc_type(existing_decl->type);
        }

        new_max_scope = max_scope_lev+1;
    }
    else // Function
    {
        KgpcType *return_kgpc_type = NULL;

        /* Reuse the type created during predeclaration when possible. */
        if (already_declared && existing_decl != NULL &&
            existing_decl->type != NULL &&
            existing_decl->type->kind == TYPE_KIND_PROCEDURE)
        {
            return_kgpc_type = kgpc_type_get_return_type(existing_decl->type);
            if (subprogram->tree_data.subprogram_data.statement_list != NULL)
            {
                Tree_t *prev_def = existing_decl->type->info.proc_info.definition;
                if (prev_def == NULL || prev_def->tree_data.subprogram_data.statement_list == NULL)
                {
                    existing_decl->type->info.proc_info.definition = subprogram;
                }
            }
            /* Copy default parameter values from forward declaration to implementation */
            copy_default_values_to_impl_params(
                existing_decl->type->info.proc_info.params,
                subprogram->tree_data.subprogram_data.args_var);
        }

        /* If the predeclare step could not resolve the type (e.g., inline array),
         * build it now and update the existing declaration. */
        if (return_kgpc_type == NULL)
        {
            return_kgpc_type = build_function_return_type(subprogram, symtab, &return_val, 0);
#ifdef DEBUG
            if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after build_function_return_type: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
        }

        KgpcType *func_type = NULL;
        if (!already_declared)
        {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: semcheck_subprogram %s NOT already declared in Pass 2!\n", subprogram->tree_data.subprogram_data.id);
#endif
            func_type = create_procedure_type(
                subprogram->tree_data.subprogram_data.args_var,
                return_kgpc_type
            );
            if (func_type != NULL)
            {
                func_type->info.proc_info.definition = subprogram;
                if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                    func_type->info.proc_info.return_type_id =
                        strdup(subprogram->tree_data.subprogram_data.return_type_id);
            }
            func_return = PushFunctionOntoScope_Typed(symtab, id_to_use_for_lookup,
                            subprogram->tree_data.subprogram_data.mangled_id,
                            func_type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
        }
        else
        {
            func_return = 0;
            if (existing_decl != NULL)
            {
                if (existing_decl->type == NULL)
                {
                    func_type = create_procedure_type(
                        subprogram->tree_data.subprogram_data.args_var,
                        return_kgpc_type
                    );
                    if (func_type != NULL)
                    {
                        func_type->info.proc_info.definition = subprogram;
                        if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                            func_type->info.proc_info.return_type_id =
                                strdup(subprogram->tree_data.subprogram_data.return_type_id);
                    }
                    existing_decl->type = func_type;
                }
                else if (return_kgpc_type != NULL)
                {
                    existing_decl->type->info.proc_info.return_type = return_kgpc_type;
                }
            }
        }

        PushScope(symtab);
        if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL)
            fprintf(stderr, "[KGPC] semcheck_subprogram (func): PushScope for %s\n",
                subprogram->tree_data.subprogram_data.id);

        /* For method implementations, add class vars to scope */
        add_class_vars_to_method_scope(symtab, subprogram->tree_data.subprogram_data.id);

        // **THIS IS THE FIX FOR THE RETURN VALUE**:
        // Use the ORIGINAL name for the internal return variable with KgpcType
        // Always use _Typed variant, even if KgpcType is NULL
        PushFuncRetOntoScope_Typed(symtab, id_to_use_for_lookup, return_kgpc_type);
        
/* Also add "Result" as an alias for the return variable for Pascal compatibility */
        /* Check if "Result" is already used in the current scope */
        HashNode_t *result_check = NULL;
        HashTable_t *cur_hash = (HashTable_t *)symtab->stack_head->cur;
        result_check = (cur_hash != NULL) ? FindIdentInTable(cur_hash, "Result") : NULL;
        if (result_check == NULL)
        {
            /* "Result" is not already declared in this scope, so add it as an alias */
            PushFuncRetOntoScope_Typed(symtab, "Result", return_kgpc_type);
        }

        /* For class methods, also add an alias using the unmangled method name (suffix after __) */
        const char *alias_suffix = NULL;
        if (subprogram->tree_data.subprogram_data.id != NULL)
        {
            const char *sep = strstr(subprogram->tree_data.subprogram_data.id, "__");
            if (sep != NULL && sep[2] != '\0')
                alias_suffix = sep + 2;
        }
        if (alias_suffix != NULL && alias_suffix[0] != '\0')
        {
            size_t alias_len = strcspn(alias_suffix, "_");
            if (alias_len > 0 && alias_len < 128)
            {
                char alias_buf[128];
                memcpy(alias_buf, alias_suffix, alias_len);
                alias_buf[alias_len] = '\0';

                HashNode_t *suffix_check = NULL;
                if (FindIdent(&suffix_check, symtab, alias_buf) == -1)
                    PushFuncRetOntoScope_Typed(symtab, alias_buf, return_kgpc_type);
            }
        }
        /* Note: We don't check for "result" anymore since it conflicts with built-in Result alias */

        /* Note: Type metadata now in KgpcType, no post-creation writes needed */

        new_max_scope = max_scope_lev+1;
    }

    /**** Check the subprogram internals now ****/

    /* Greater than 0 signifies an error */
    if (func_return > 0)
    {
        int current_has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
        HashNode_t *existing_decl = NULL;
        if (FindIdent(&existing_decl, symtab, id_to_use_for_lookup) == 0 &&
            existing_decl != NULL &&
            existing_decl->mangled_id != NULL &&
            subprogram->tree_data.subprogram_data.mangled_id != NULL &&
            strcmp(existing_decl->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
        {
            func_return = 0;
        }
        else
        {
            ListNode_t *candidates = FindAllIdents(symtab, id_to_use_for_lookup);
            ListNode_t *iter = candidates;
            while (iter != NULL)
            {
                HashNode_t *candidate = (HashNode_t *)iter->cur;
                if (candidate != NULL && candidate->type != NULL &&
                    candidate->type->kind == TYPE_KIND_PROCEDURE)
                {
                    Tree_t *def = candidate->type->info.proc_info.definition;
                    int existing_has_body = (def != NULL &&
                        def->tree_data.subprogram_data.statement_list != NULL);
                    if (existing_has_body != current_has_body)
                    {
                        func_return = 0;
                        break;
                    }
                }
                iter = iter->next;
            }
            if (candidates != NULL)
                DestroyList(candidates);
        }
    }
    if(func_return > 0)
    {
        fprintf(stderr, "On line %d: redeclaration of name %s!\n",
            subprogram->line_num, subprogram->tree_data.subprogram_data.id);

        return_val += func_return;
    }

    /* These arguments are themselves like declarations */
    if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL) {
        ListNode_t *arg_debug = subprogram->tree_data.subprogram_data.args_var;
        fprintf(stderr, "[KGPC] semcheck_subprogram: %s args:\n", subprogram->tree_data.subprogram_data.id);
        while (arg_debug != NULL) {
            if (arg_debug->type == LIST_TREE && arg_debug->cur != NULL) {
                Tree_t *arg_tree = (Tree_t *)arg_debug->cur;
                if (arg_tree->type == TREE_VAR_DECL) {
                    ListNode_t *ids = arg_tree->tree_data.var_decl_data.ids;
                    if (ids != NULL && ids->cur != NULL)
                        fprintf(stderr, "  - %s (type_id=%s)\n", (char*)ids->cur,
                            arg_tree->tree_data.var_decl_data.type_id ? arg_tree->tree_data.var_decl_data.type_id : "<null>");
                }
            }
            arg_debug = arg_debug->next;
        }
    }
    return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.args_var);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after args: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    /* Ensure helper methods always have an implicit Self in scope even if the
     * parameter list was parsed without it (e.g., macro-expanded helper types). */
    {
        HashNode_t *self_node = NULL;
        if (FindIdent(&self_node, symtab, "Self") == -1)
        {
            const char *owner_id = semcheck_get_current_method_owner();
            struct RecordType *owner_record = NULL;
            if (owner_id != NULL)
            {
                HashNode_t *owner_node = semcheck_find_preferred_type_node(symtab, owner_id);
                if (owner_node != NULL)
                    owner_record = get_record_type_from_node(owner_node);
            }
            if (owner_record != NULL && owner_record->is_type_helper &&
                owner_record->helper_base_type_id != NULL)
            {
                KgpcType *self_type = NULL;
                HashNode_t *type_node = semcheck_find_preferred_type_node(symtab,
                    owner_record->helper_base_type_id);
                if (type_node != NULL && type_node->type != NULL)
                {
                    kgpc_type_retain(type_node->type);
                    self_type = type_node->type;
                }
                else
                {
                    int builtin_tag = semcheck_map_builtin_type_name_local(
                        owner_record->helper_base_type_id);
                    if (builtin_tag != UNKNOWN_TYPE)
                        self_type = create_primitive_type(builtin_tag);
                }

                if (self_type != NULL)
                {
                    PushVarOntoScope_Typed(symtab, "Self", self_type);
                    destroy_kgpc_type(self_type);
                }
            }
        }
    }

    return_val += predeclare_enum_literals(symtab, subprogram->tree_data.subprogram_data.type_declarations);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    return_val += predeclare_types(symtab, subprogram->tree_data.subprogram_data.type_declarations);
    return_val += semcheck_const_decls(symtab, subprogram->tree_data.subprogram_data.const_declarations);
    return_val += semcheck_type_decls(symtab, subprogram->tree_data.subprogram_data.type_declarations);
    return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.declarations);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after decls: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    return_val += semcheck_subprograms(symtab, subprogram->tree_data.subprogram_data.subprograms,
                    new_max_scope, subprogram);

    Tree_t *prev_current_subprogram = g_semcheck_current_subprogram;
    g_semcheck_current_subprogram = subprogram;

    body = subprogram->tree_data.subprogram_data.statement_list;
    if (body == NULL)
    {
        g_semcheck_current_subprogram = prev_current_subprogram;
        PopScope(symtab);
#ifdef DEBUG
        fprintf(stderr, "DEBUG: semcheck_subprogram %s returning (no body): %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
        return return_val;
    }

    /* Functions cannot have side effects, so need to call a special function in that case */
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        return_val += semcheck_stmt(symtab,
                body,
                new_max_scope);
    }
    else
    {
        assert(FindIdent(&hash_return, symtab, subprogram->tree_data.subprogram_data.id)
                    == 0);

        ResetHashNodeStatus(hash_return);
        int func_stmt_ret = semcheck_func_stmt(symtab,
                body, new_max_scope);
        return_val += func_stmt_ret;
#ifdef DEBUG
        if (func_stmt_ret > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after semcheck_func_stmt: %d\n", subprogram->tree_data.subprogram_data.id, func_stmt_ret);
#endif

        /* Allow functions with asm blocks to skip explicit return assignment */
        int has_asm = statement_contains_asm_block(body);
        
        /* Constructors implicitly yield the constructed instance, so do not
         * require an explicit assignment to the return variable. */
        int is_constructor = 0;
        if (subprogram->tree_data.subprogram_data.id != NULL) {
            const char *suffix = strstr(subprogram->tree_data.subprogram_data.id, "__");
            const char *name = (suffix != NULL && suffix[2] != '\0') ? suffix + 2
                                                                     : subprogram->tree_data.subprogram_data.id;
            if (strcasecmp(name, "create") == 0) {
                is_constructor = 1;
            }
        }

        /* Check if either the function name or "Result" was assigned to */
        int return_was_assigned = is_constructor ? 1 : (hash_return->mutated != NO_MUTATE);
        if (!return_was_assigned)
        {
            /* Also check if "Result" was mutated */
            HashNode_t *result_node = NULL;
            if (FindIdent(&result_node, symtab, "Result") == 0 && result_node != NULL)
            {
                return_was_assigned = (result_node->mutated != NO_MUTATE);
            }
        }

        /* Methods use mangled identifiers like Class__Func; allow assignments to the
         * unmangled function name (after the final '__') to satisfy the return check. */
        if (!return_was_assigned && subprogram->tree_data.subprogram_data.id != NULL) {
            const char *id = subprogram->tree_data.subprogram_data.id;
            const char *sep = strstr(id, "__");
            if (sep != NULL && sep[2] != '\0') {
                HashNode_t *suffix_node = NULL;
                if (FindIdent(&suffix_node, symtab, (char *)(sep + 2)) == 0 && suffix_node != NULL) {
                    return_was_assigned = (suffix_node->mutated != NO_MUTATE);
                }
            }
        }
        
        if(!return_was_assigned && !has_asm)
        {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: Checking return for %s. cname_override=%s cname_flag=%d return_was_assigned=%d\n",
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.cname_override ? subprogram->tree_data.subprogram_data.cname_override : "<null>",
                subprogram->tree_data.subprogram_data.cname_flag,
                return_was_assigned);
#endif

            /* Skip check for external functions (cname_flag or cname_override) or declarations without bodies */
            int is_external = (subprogram->tree_data.subprogram_data.cname_override != NULL) ||
                              (subprogram->tree_data.subprogram_data.cname_flag != 0);
            int has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
            if (is_external || !has_body)
            {
                /* External function, no body expected */
            }
            else
            {
                /* FPC treats this as a warning, not an error - function result may be uninitialized */
                fprintf(stderr,
                    "Warning in function %s: function result does not seem to be set\n\n",
                    subprogram->tree_data.subprogram_data.id);
                /* Note: Not incrementing return_val - this is a warning, not an error */
            }
        }
    }

    if(optimize_flag() > 0 && return_val == 0)
    {
        optimize(symtab, subprogram);
    }

    if (subprogram->tree_data.subprogram_data.id != NULL)
    {
        ListNode_t *defs = FindAllIdents(symtab, subprogram->tree_data.subprogram_data.id);
        ListNode_t *iter = defs;
        while (iter != NULL)
        {
            if (iter->cur != NULL)
            {
                HashNode_t *node = (HashNode_t *)iter->cur;
                if (node != NULL &&
                    node->mangled_id != NULL &&
                    subprogram->tree_data.subprogram_data.mangled_id != NULL &&
                    strcmp(node->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
                {
                    node->requires_static_link =
                        subprogram->tree_data.subprogram_data.requires_static_link ? 1 : 0;
                    node->has_nested_requiring_link =
                        subprogram->tree_data.subprogram_data.has_nested_requiring_link ? 1 : 0;
                }
            }
            iter = iter->next;
        }
        DestroyList(defs);
    }

    g_semcheck_current_subprogram = prev_current_subprogram;
    PopScope(symtab);

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s returning at end: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
    if (owner_copy != NULL)
    {
        semcheck_set_current_method_owner(prev_owner);
        free(owner_copy);
    }

    return return_val;
}


/* Pre-declare a subprogram (add to symbol table without processing body)
 * This is used for forward declarations so all procedures are visible
 * before any bodies are processed.
 */
static int predeclare_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev, Tree_t *parent_subprogram)
{
    int return_val = 0;
    int func_return;
    enum TreeType sub_type;

    assert(symtab != NULL);
    assert(subprogram != NULL);
    assert(subprogram->type == TREE_SUBPROGRAM);

    char *id_to_use_for_lookup;

    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    /* Debug output for procedure predeclaration */
    if (getenv("KGPC_DEBUG_PREDECLARE_PROC") != NULL)
    {
        fprintf(stderr, "[PREDECLARE_PROC] %s (line %d) parent=%s\n",
                subprogram->tree_data.subprogram_data.id,
                subprogram->line_num,
                parent_subprogram != NULL ? parent_subprogram->tree_data.subprogram_data.id : "(null)");
    }

    // --- Name Mangling Logic ---
    const char *predeclare_name = subprogram->tree_data.subprogram_data.cname_override;
    if (predeclare_name != NULL) {
        subprogram->tree_data.subprogram_data.mangled_id = strdup(predeclare_name);
    } else if (subprogram->tree_data.subprogram_data.cname_flag) {
        subprogram->tree_data.subprogram_data.mangled_id = strdup(subprogram->tree_data.subprogram_data.id);
    } else {
        // Pass the symbol table to the mangler
        char *base_mangled = MangleFunctionName(
            subprogram->tree_data.subprogram_data.id,
            subprogram->tree_data.subprogram_data.args_var,
            symtab);

        // For nested functions, prepend the parent's mangled_id to make the name unique
        if (parent_subprogram != NULL && parent_subprogram->tree_data.subprogram_data.mangled_id != NULL) {
            const char *parent_mangled = parent_subprogram->tree_data.subprogram_data.mangled_id;
            size_t len = strlen(parent_mangled) + 2 + strlen(base_mangled) + 1; // parent$nested\0
            char *nested_mangled = malloc(len);
            if (nested_mangled != NULL) {
                snprintf(nested_mangled, len, "%s$%s", parent_mangled, base_mangled);
                free(base_mangled);
                subprogram->tree_data.subprogram_data.mangled_id = nested_mangled;
            } else {
                subprogram->tree_data.subprogram_data.mangled_id = base_mangled;
            }
        } else {
            subprogram->tree_data.subprogram_data.mangled_id = base_mangled;
        }
    }

    if (getenv("KGPC_DEBUG_PREDECLARE_PROC") != NULL)
    {
        fprintf(stderr, "[PREDECLARE_PROC]   -> mangled_id=%s\n",
                subprogram->tree_data.subprogram_data.mangled_id);
    }

    id_to_use_for_lookup = subprogram->tree_data.subprogram_data.id;
    
    /* Check if this specific overload is already declared (by matching mangled name) */
    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
        ListNode_t *cur = all_matches;
        int already_exists = 0;
        while (cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL && candidate->mangled_id != NULL &&
                strcmp(candidate->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
            {
                int same_signature = 1;
                if (candidate->type != NULL && candidate->type->kind == TYPE_KIND_PROCEDURE)
                {
                    Tree_t *candidate_def = candidate->type->info.proc_info.definition;
                    if (candidate_def != NULL &&
                        !semcheck_subprogram_signatures_equivalent(subprogram, candidate_def))
                    {
                        same_signature = 0;
                    }
                }
                if (same_signature)
                {
                    already_exists = 1;
                    break;
                }
            }
            /* If this exact subprogram was already registered (even with a different
             * mangled name), reuse that declaration instead of creating a duplicate. */
            if (candidate != NULL && candidate->type != NULL &&
                candidate->type->kind == TYPE_KIND_PROCEDURE &&
                candidate->type->info.proc_info.definition == subprogram)
            {
                if (candidate->mangled_id != NULL)
                    free(candidate->mangled_id);
                candidate->mangled_id = strdup(subprogram->tree_data.subprogram_data.mangled_id);
                already_exists = 1;
                break;
            }
            cur = cur->next;
        }
        if (all_matches != NULL)
            DestroyList(all_matches);
        
        if (already_exists)
            return 0;  /* Already declared - skip to avoid duplicates */
    }

    /* If a declaration already exists for this name/signature, skip predeclaring
     * the matching implementation body. */
    {
        int current_has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
        ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
        ListNode_t *cur = all_matches;
        while (cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate == NULL || candidate->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
            {
                cur = cur->next;
                continue;
            }
            if (candidate->type != NULL && candidate->type->kind == TYPE_KIND_PROCEDURE)
            {
                Tree_t *def = candidate->type->info.proc_info.definition;
                int existing_has_body = (def != NULL &&
                    def->tree_data.subprogram_data.statement_list != NULL);
                int same_signature = 0;

                if (def != NULL &&
                    semcheck_subprogram_signatures_equivalent(subprogram, def))
                {
                    same_signature = 1;
                }
                else if (candidate->mangled_id != NULL &&
                         subprogram->tree_data.subprogram_data.mangled_id != NULL &&
                         strcmp(candidate->mangled_id,
                                subprogram->tree_data.subprogram_data.mangled_id) == 0)
                {
                    same_signature = 1;
                }

                if (same_signature && existing_has_body != current_has_body)
                {
                    if (all_matches != NULL)
                        DestroyList(all_matches);
                    return 0;
                }
            }
            cur = cur->next;
        }
        if (all_matches != NULL)
            DestroyList(all_matches);
    }
    
    /**** PLACE SUBPROGRAM ON THE CURRENT SCOPE ****/
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        /* Create KgpcType for the procedure */
        KgpcType *proc_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            NULL  /* procedures have no return type */
        );
        if (proc_type != NULL) {
            proc_type->info.proc_info.definition = subprogram;
            if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                proc_type->info.proc_info.return_type_id =
                    strdup(subprogram->tree_data.subprogram_data.return_type_id);
        }
        
        // Add to current scope
        func_return = PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        proc_type);
        
        if(func_return > 0)
        {
            fprintf(stderr, "On line %d: redeclaration of name %s!\n",
                subprogram->line_num, subprogram->tree_data.subprogram_data.id);
            return_val += func_return;
        }
    }
    else // Function
    {
        KgpcType *return_kgpc_type = build_function_return_type(subprogram, symtab, &return_val, 1);
#ifdef DEBUG
        if (return_val > 0) fprintf(stderr, "DEBUG: predeclare_subprogram %s error after build_function_return_type: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
        
        /* Create function KgpcType */
        KgpcType *func_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            return_kgpc_type  /* functions have a return type */
        );
        if (func_type != NULL) {
            func_type->info.proc_info.definition = subprogram;
            if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                func_type->info.proc_info.return_type_id =
                    strdup(subprogram->tree_data.subprogram_data.return_type_id);
        }
        
        // Add to current scope
        func_return = PushFunctionOntoScope_Typed(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        func_type);
        
        if(func_return > 0)
        {
            fprintf(stderr, "On line %d: redeclaration of name %s!\n",
                subprogram->line_num, subprogram->tree_data.subprogram_data.id);
            return_val += func_return;
        }
    }
    
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: predeclare_subprogram %s returning error: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    return return_val;
}


/* Semantic check on multiple subprograms */
/* A return value greater than 0 indicates how many errors occurred */
/* Forward declaration - we'll define this after semcheck_subprogram */
static int predeclare_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev, Tree_t *parent_subprogram);
static void semcheck_update_symbol_alias(SymTab_t *symtab, const char *id, const char *alias);

/* Predeclare a list of subprograms without processing bodies.
 * Safe to call multiple times thanks to duplicate checks in predeclare_subprogram. */
static int predeclare_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram)
{
    int return_val = 0;
    ListNode_t *cur = subprograms;
    while (cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        Tree_t *child = (Tree_t *)cur->cur;
        return_val += predeclare_subprogram(symtab, child, max_scope_lev, parent_subprogram);
        /* Note: We intentionally do NOT predeclare nested subprograms here globally.
         * Nested subprograms are predeclared within their parent's scope when
         * semcheck_subprogram calls semcheck_subprograms (which has its own Pass 1).
         * This ensures nested functions with the same name in different parents
         * don't shadow each other. */
        cur = cur->next;
    }
    return return_val;
}

int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram)
{
    ListNode_t *cur;
    int return_val;
    assert(symtab != NULL);

#ifdef DEBUG
    fprintf(stderr, "DEBUG: semcheck_subprograms called. subprograms=%p\n", subprograms);
#endif

    return_val = 0;
    
    /* ARCHITECTURAL FIX: Two-pass approach to ensure all procedure declarations
     * are visible before processing any bodies. This fixes the issue where unit
     * procedures were not visible in nested user procedures because they came
     * later in the subprograms list.
     * 
     * Pass 1: Declare all procedures (add to symbol table)
     * Pass 2: Process bodies (which may reference procedures declared in pass 1)
     */
    
    /* Pass 1: Pre-declare all procedures at this level */
    cur = subprograms;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        return_val += predeclare_subprogram(symtab, (Tree_t *)cur->cur, max_scope_lev, parent_subprogram);
        cur = cur->next;
    }

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprograms error after Pass 1: %d\n", return_val);
#endif

    /* Pass 2: Process full semantic checking including bodies */
    cur = subprograms;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        Tree_t *child = (Tree_t *)cur->cur;
        if (child != NULL &&
            child->tree_data.subprogram_data.statement_list == NULL &&
            child->tree_data.subprogram_data.cname_flag == 0 &&
            child->tree_data.subprogram_data.cname_override == NULL)
        {
            /* Interface/forward declaration with no body. */
            cur = cur->next;
            continue;
        }
        return_val += semcheck_subprogram(symtab, child, max_scope_lev);
        /* If child needs a static link, mark parent as having nested children that need links.
         * This is used by codegen to know when to PASS a static link when calling nested functions.
         * We do NOT propagate requires_static_link to parent - the parent only needs to RECEIVE
         * a static link if it's nested itself or accesses outer scope variables. */
        if (parent_subprogram != NULL &&
            child != NULL &&
            child->tree_data.subprogram_data.requires_static_link)
        {
            parent_subprogram->tree_data.subprogram_data.has_nested_requiring_link = 1;
        }
        cur = cur->next;
    }

    return return_val;
}
