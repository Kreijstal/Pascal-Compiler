#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif

#include "from_cparser.h"

#include "../List/List.h"
#include "tree.h"
#include "tree_types.h"
#include "type_tags.h"
#include "pascal_parser.h"

typedef struct {
    int is_array;
    int start;
    int end;
    int element_type;
    char *element_type_id;
} ArrayTypeInfo;

static char *dup_symbol(ast_t *node) {
    while (node != NULL) {
        if (node->sym != NULL && node->sym->name != NULL)
            return strdup(node->sym->name);
        node = node->child;
    }
    return NULL;
}

static ListNode_t *append_node(ListNode_t **head, void *value, enum ListType type) {
    ListNode_t *node = CreateListNode(value, type);
    if (*head == NULL) {
        *head = node;
    } else {
        PushListNodeBack(*head, node);
    }
    return node;
}

static void extend_list(ListNode_t **dest, ListNode_t *src) {
    if (src == NULL) {
        return;
    }
    if (*dest == NULL) {
        *dest = src;
        return;
    }
    ListNode_t *cur = *dest;
    while (cur->next != NULL) {
        cur = cur->next;
    }
    cur->next = src;
}

static ast_t *unwrap_pascal_node(ast_t *node) {
    ast_t *cur = node;
    while (cur != NULL) {
        switch (cur->typ) {
        case PASCAL_T_NONE:
        case PASCAL_T_THEN:
        case PASCAL_T_ELSE:
        case PASCAL_T_DO:
            if (cur->child != NULL) {
                cur = cur->child;
                continue;
            }
            break;
        default:
            break;
        }
        break;
    }
    return cur;
}

static int map_type_name(const char *name, char **type_id_out) {
    if (name == NULL) {
        return UNKNOWN_TYPE;
    }
    if (strcasecmp(name, "integer") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("integer");
        return INT_TYPE;
    }
    if (strcasecmp(name, "longint") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("longint");
        return LONGINT_TYPE;
    }
    if (strcasecmp(name, "real") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("real");
        return REAL_TYPE;
    }
    if (strcasecmp(name, "string") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("string");
        return STRING_TYPE;
    }
    if (strcasecmp(name, "single") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("single");
        return REAL_TYPE;
    }
    if (strcasecmp(name, "boolean") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("boolean");
        return BOOL;
    }
    if (type_id_out != NULL) {
        *type_id_out = strdup(name);
    }
    return UNKNOWN_TYPE;
}

static struct RecordType *convert_record_type(ast_t *record_node);
static struct Expression *convert_expression(ast_t *expr_node);
static struct Expression *convert_field_width_expr(ast_t *field_width_node);
static ListNode_t *convert_expression_list(ast_t *arg_node);
static ListNode_t *convert_statement_list(ast_t *stmt_list_node);

static int convert_type_spec(ast_t *type_spec, char **type_id_out, struct RecordType **record_out, ArrayTypeInfo *array_info) {
    if (type_id_out != NULL)
        *type_id_out = NULL;
    if (record_out != NULL)
        *record_out = NULL;
    if (array_info != NULL) {
        array_info->is_array = 0;
        array_info->start = 0;
        array_info->end = 0;
        array_info->element_type = UNKNOWN_TYPE;
        array_info->element_type_id = NULL;
    }

    if (type_spec == NULL)
        return UNKNOWN_TYPE;

    ast_t *spec_node = type_spec;
    if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
        spec_node = spec_node->child;

    if (spec_node == NULL)
        return UNKNOWN_TYPE;

    if (spec_node->typ == PASCAL_T_IDENTIFIER) {
        char *dup = dup_symbol(spec_node);
        int result = map_type_name(dup, type_id_out);
        if (result == UNKNOWN_TYPE && type_id_out != NULL && *type_id_out == NULL) {
            *type_id_out = dup;
        } else {
            free(dup);
        }
        return result;
    }
    if (spec_node->typ == PASCAL_T_ARRAY_TYPE) {
        if (array_info != NULL) {
            array_info->is_array = 1;
            ast_t *range_node = spec_node->child;
            if (range_node != NULL && range_node->typ == PASCAL_T_RANGE_TYPE) {
                ast_t *lower = range_node->child;
                ast_t *upper = (lower != NULL) ? lower->next : NULL;
                if (lower != NULL && upper != NULL &&
                    lower->typ == PASCAL_T_INTEGER && upper->typ == PASCAL_T_INTEGER &&
                    lower->sym != NULL && upper->sym != NULL) {
                    array_info->start = atoi(lower->sym->name);
                    array_info->end = atoi(upper->sym->name);
                }
                ast_t *element_node = range_node;
                while (element_node != NULL && element_node->next != NULL)
                    element_node = element_node->next;
                if (element_node != NULL && element_node->typ == PASCAL_T_IDENTIFIER) {
                    char *dup = dup_symbol(element_node);
                    int mapped = map_type_name(dup, &array_info->element_type_id);
                    array_info->element_type = mapped;
                    if (mapped != UNKNOWN_TYPE && array_info->element_type_id != NULL) {
                        free(array_info->element_type_id);
                        array_info->element_type_id = NULL;
                    }
                    if (mapped == UNKNOWN_TYPE && array_info->element_type_id == NULL)
                        array_info->element_type_id = dup;
                    else if (mapped != UNKNOWN_TYPE)
                        free(dup);
                }
            } else {
                array_info->start = 0;
                array_info->end = -1;
                ast_t *element_node = range_node;
                while (element_node != NULL && element_node->next != NULL)
                    element_node = element_node->next;
                if (element_node != NULL && element_node->typ == PASCAL_T_IDENTIFIER) {
                    char *dup = dup_symbol(element_node);
                    int mapped = map_type_name(dup, &array_info->element_type_id);
                    array_info->element_type = mapped;
                    if (mapped != UNKNOWN_TYPE && array_info->element_type_id != NULL) {
                        free(array_info->element_type_id);
                        array_info->element_type_id = NULL;
                    }
                    if (mapped == UNKNOWN_TYPE && array_info->element_type_id == NULL)
                        array_info->element_type_id = dup;
                    else if (mapped != UNKNOWN_TYPE)
                        free(dup);
                }
            }
        }
        return UNKNOWN_TYPE;
    }

    if (spec_node->typ == PASCAL_T_RECORD_TYPE) {
        struct RecordType *record = convert_record_type(spec_node);
        if (record_out != NULL) {
            *record_out = record;
        } else {
            destroy_record_type(record);
        }
        return UNKNOWN_TYPE;
    }

    return UNKNOWN_TYPE;
}

static ListNode_t *convert_identifier_list(ast_t **cursor) {
    ListNode_t *ids = NULL;
    ast_t *cur = *cursor;
    while (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER) {
        char *dup = dup_symbol(cur);
        append_node(&ids, dup, LIST_STRING);
        cur = cur->next;
    }
    *cursor = cur;
    return ids;
}

static struct RecordType *convert_record_type(ast_t *record_node) {
    if (record_node == NULL)
        return NULL;

    ListNode_t *fields = NULL;

    for (ast_t *field_decl = record_node->child; field_decl != NULL; field_decl = field_decl->next) {
        if (field_decl->typ != PASCAL_T_FIELD_DECL)
            continue;

        ast_t *cursor = field_decl->child;
        ListNode_t *names = convert_identifier_list(&cursor);
        if (names == NULL)
            continue;

        while (cursor != NULL && cursor->typ != PASCAL_T_TYPE_SPEC &&
               cursor->typ != PASCAL_T_RECORD_TYPE && cursor->typ != PASCAL_T_IDENTIFIER) {
            cursor = cursor->next;
        }

        char *field_type_id = NULL;
        struct RecordType *nested_record = NULL;
        int field_type = UNKNOWN_TYPE;
        if (cursor != NULL)
            field_type = convert_type_spec(cursor, &field_type_id, &nested_record, NULL);

        ListNode_t *name_node = names;
        while (name_node != NULL) {
            char *field_name = (char *)name_node->cur;
            char *type_id_copy = NULL;
            if (field_type_id != NULL)
                type_id_copy = strdup(field_type_id);

            struct RecordType *nested_copy = NULL;
            if (nested_record != NULL) {
                if (name_node->next == NULL) {
                    nested_copy = nested_record;
                    nested_record = NULL;
                } else {
                    nested_copy = clone_record_type(nested_record);
                }
            }

            struct RecordField *field_desc = (struct RecordField *)malloc(sizeof(struct RecordField));
            if (field_desc != NULL) {
                field_desc->name = field_name;
                field_desc->type = field_type;
                field_desc->type_id = type_id_copy;
                field_desc->nested_record = nested_copy;
                append_node(&fields, field_desc, LIST_RECORD_FIELD);
            } else {
                if (field_name != NULL)
                    free(field_name);
                if (type_id_copy != NULL)
                    free(type_id_copy);
                destroy_record_type(nested_copy);
            }

            ListNode_t *next_name = name_node->next;
            free(name_node);
            name_node = next_name;
        }

        if (field_type_id != NULL)
            free(field_type_id);
        if (nested_record != NULL)
            destroy_record_type(nested_record);
    }

    struct RecordType *record = (struct RecordType *)malloc(sizeof(struct RecordType));
    if (record == NULL) {
        destroy_list(fields);
        return NULL;
    }
    record->fields = fields;
    return record;
}

static char *pop_last_identifier(ListNode_t **ids) {
    if (ids == NULL || *ids == NULL)
        return NULL;

    ListNode_t *prev = NULL;
    ListNode_t *cur = *ids;
    while (cur->next != NULL) {
        prev = cur;
        cur = cur->next;
    }

    char *value = (char *)cur->cur;
    if (prev != NULL)
        prev->next = NULL;
    else
        *ids = NULL;

    free(cur);
    return value;
}

static ListNode_t *convert_param(ast_t *param_node) {
    ast_t *cur = param_node->child;
    int is_var_param = 0;

    if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER && cur->sym != NULL) {
        if (strcasecmp(cur->sym->name, "var") == 0) {
            is_var_param = 1;
            cur = cur->next;
        } else if (strcasecmp(cur->sym->name, "const") == 0) {
            cur = cur->next;
        }
    }

    ListNode_t *ids = convert_identifier_list(&cur);
    if (ids == NULL) {
        fprintf(stderr, "ERROR: parameter missing identifier list.\n");
        return NULL;
    }

    char *type_id = NULL;
    int var_type = UNKNOWN_TYPE;
    if (cur != NULL && cur->typ == PASCAL_T_TYPE_SPEC) {
        var_type = convert_type_spec(cur, &type_id, NULL, NULL);
        cur = cur->next;
    } else {
        char *type_name = pop_last_identifier(&ids);
        if (type_name != NULL) {
            var_type = map_type_name(type_name, &type_id);
            if (var_type == UNKNOWN_TYPE && type_id == NULL) {
                type_id = type_name;
            } else {
                free(type_name);
            }
        }
    }

    ListNode_t *result = NULL;
    ListNode_t *id_node = ids;

    while (id_node != NULL) {
        ListNode_t *next_id = id_node->next;
        id_node->next = NULL;
        char *type_id_copy = type_id != NULL ? strdup(type_id) : NULL;
        Tree_t *param_decl = mk_vardecl(param_node->line, id_node, var_type, type_id_copy, is_var_param, 0, NULL);
        append_node(&result, param_decl, LIST_TREE);
        id_node = next_id;
    }

    if (type_id != NULL)
        free(type_id);

    return result;
}

static ListNode_t *convert_param_list(ast_t **cursor) {
    ListNode_t *params = NULL;
    ast_t *cur = *cursor;

    while (cur != NULL && cur->typ == PASCAL_T_PARAM) {
        ListNode_t *param_nodes = convert_param(cur);
        extend_list(&params, param_nodes);
        cur = cur->next;
    }

    *cursor = cur;
    return params;
}

static Tree_t *convert_var_decl(ast_t *decl_node) {
    ast_t *cur = decl_node->child;
    ast_t *first_non_identifier = cur;
    while (first_non_identifier != NULL && first_non_identifier->typ == PASCAL_T_IDENTIFIER)
        first_non_identifier = first_non_identifier->next;

    ListNode_t *ids = convert_identifier_list(&cur);
    if (ids == NULL) {
        fprintf(stderr, "ERROR: variable declaration missing identifier list.\n");
        return NULL;
    }
    char *type_id = NULL;
    int var_type = UNKNOWN_TYPE;
    ArrayTypeInfo array_info = {0};
    ast_t *type_node = first_non_identifier;
    if (type_node != NULL && type_node->typ == PASCAL_T_TYPE_SPEC) {
        var_type = convert_type_spec(type_node, &type_id, NULL, &array_info);
        cur = type_node->next;
    } else if (type_node != NULL && type_node->typ == PASCAL_T_IDENTIFIER && type_node->next == NULL) {
        char *type_name = dup_symbol(type_node);
        if (type_name != NULL) {
            var_type = map_type_name(type_name, &type_id);
            if (var_type == UNKNOWN_TYPE && type_id == NULL)
                type_id = type_name;
            else
                free(type_name);
        }
        cur = type_node->next;
    } else {
        cur = type_node;
    }

    if (var_type == UNKNOWN_TYPE && (type_node == NULL || var_type == UNKNOWN_TYPE)) {
        ast_t *search = decl_node->child;
        while (search != NULL && search->typ == PASCAL_T_IDENTIFIER)
            search = search->next;
        if (search != NULL && search->typ == PASCAL_T_TYPE_SPEC) {
            var_type = convert_type_spec(search, &type_id, NULL, &array_info);
        } else if (search != NULL && search->typ == PASCAL_T_IDENTIFIER) {
            char *type_name = dup_symbol(search);
            if (type_name != NULL) {
                int mapped = map_type_name(type_name, &type_id);
                if (mapped == UNKNOWN_TYPE && type_id == NULL)
                    type_id = type_name;
                else
                    free(type_name);
                var_type = mapped;
            }
        }
    }

    if (array_info.is_array) {
        int element_type = array_info.element_type;
        char *element_type_id = array_info.element_type_id;
        Tree_t *decl = mk_arraydecl(decl_node->line, ids, element_type, element_type_id,
                                    array_info.start, array_info.end);
        return decl;
    }

    int inferred = 0;
    struct Statement *initializer_stmt = NULL;
    if (cur != NULL) {
        struct Expression *init_expr = convert_expression(cur);
        if (init_expr != NULL) {
            if (ids != NULL && ids->next == NULL) {
                inferred = (var_type == UNKNOWN_TYPE && type_id == NULL) ? 1 : 0;
                char *var_name = (char *)ids->cur;
                struct Expression *lhs = mk_varid(decl_node->line, strdup(var_name));
                initializer_stmt = mk_varassign(decl_node->line, lhs, init_expr);
            } else {
                destroy_expr(init_expr);
            }
        }
    }

    if (var_type == UNKNOWN_TYPE && ids != NULL && ids->next != NULL && type_node == NULL) {
        ListNode_t *prev = NULL;
        ListNode_t *iter = ids;
        while (iter->next != NULL) {
            prev = iter;
            iter = iter->next;
        }
        if (prev != NULL && iter != NULL && iter->type == LIST_STRING) {
            char *type_name = strdup((char *)iter->cur);
            if (type_name != NULL) {
                int mapped = map_type_name(type_name, &type_id);
                if (mapped == UNKNOWN_TYPE && type_id == NULL)
                    type_id = type_name;
                else
                    free(type_name);
                var_type = mapped;
            }
            prev->next = NULL;
            free(iter->cur);
            free(iter);
        }
    }

    Tree_t *decl = mk_vardecl(decl_node->line, ids, var_type, type_id, 0, inferred, initializer_stmt);
    return decl;
}

static ListNode_t *convert_var_section(ast_t *section_node) {
    ListNode_t *decls = NULL;
    ast_t *cur = section_node->child;

    while (cur != NULL && cur->typ == PASCAL_T_VAR_DECL) {
        Tree_t *decl = convert_var_decl(cur);
        if (decl != NULL)
            append_node(&decls, decl, LIST_TREE);
        cur = cur->next;
    }

    return decls;
}

static Tree_t *convert_const_decl(ast_t *const_decl_node) {
    if (const_decl_node == NULL)
        return NULL;

    ast_t *cur = const_decl_node->child;
    if (cur == NULL)
        return NULL;

    char *id = dup_symbol(cur);
    if (id == NULL)
        return NULL;

    cur = cur->next;
    char *type_id = NULL;
    ArrayTypeInfo array_info = {0};

    if (cur != NULL && cur->typ == PASCAL_T_TYPE_SPEC) {
        convert_type_spec(cur, &type_id, NULL, &array_info);
        cur = cur->next;
    }

    ast_t *value_node = unwrap_pascal_node(cur);
    if (value_node == NULL || array_info.is_array) {
        fprintf(stderr, "ERROR: Unsupported const declaration for %s.\n", id);
        if (type_id != NULL)
            free(type_id);
        free(id);
        if (array_info.element_type_id != NULL)
            free(array_info.element_type_id);
        return NULL;
    }

    struct Expression *value_expr = convert_expression(value_node);
    if (value_expr == NULL) {
        fprintf(stderr, "ERROR: Unsupported const expression for %s.\n", id);
        if (type_id != NULL)
            free(type_id);
        free(id);
        return NULL;
    }

    Tree_t *decl = mk_constdecl(const_decl_node->line, id, type_id, value_expr);
    return decl;
}

static void append_const_decls_from_section(ast_t *const_section, ListNode_t **dest) {
    if (const_section == NULL || dest == NULL)
        return;

    ast_t *const_decl = const_section->child;
    while (const_decl != NULL) {
        if (const_decl->typ == PASCAL_T_CONST_DECL) {
            Tree_t *decl = convert_const_decl(const_decl);
            if (decl != NULL)
                append_node(dest, decl, LIST_TREE);
        }
        const_decl = const_decl->next;
    }
}

static Tree_t *convert_type_decl(ast_t *type_decl_node) {
    if (type_decl_node == NULL)
        return NULL;

    ast_t *id_node = type_decl_node->child;
    if (id_node == NULL)
        return NULL;

    char *id = dup_symbol(id_node);
    if (id == NULL)
        return NULL;

    ast_t *spec_node = id_node->next;
    while (spec_node != NULL && spec_node->typ != PASCAL_T_TYPE_SPEC &&
           spec_node->typ != PASCAL_T_RECORD_TYPE) {
        spec_node = spec_node->next;
    }

    char *type_id = NULL;
    struct RecordType *record_type = NULL;
    ArrayTypeInfo array_info = {0};
    int mapped_type = UNKNOWN_TYPE;
    if (spec_node != NULL)
        mapped_type = convert_type_spec(spec_node, &type_id, &record_type, &array_info);

    Tree_t *decl = NULL;
    if (record_type != NULL) {
        decl = mk_record_type(type_decl_node->line, id, record_type);
    } else if (array_info.is_array) {
        decl = mk_typealiasdecl(type_decl_node->line, id, 1, array_info.element_type,
                                 array_info.element_type_id, array_info.start, array_info.end);
        array_info.element_type_id = NULL;
    } else if (mapped_type != UNKNOWN_TYPE || type_id != NULL) {
        decl = mk_typealiasdecl(type_decl_node->line, id, 0, mapped_type, type_id, 0, 0);
        type_id = NULL;
    } else {
        decl = mk_typedecl(type_decl_node->line, id, 0, 0);
    }

    if (type_id != NULL)
        free(type_id);
    if (array_info.element_type_id != NULL)
        free(array_info.element_type_id);

    if (decl == NULL) {
        free(id);
        destroy_record_type(record_type);
    }

    return decl;
}

static struct Statement *convert_statement(ast_t *stmt_node);
static struct Statement *convert_block(ast_t *block_node);
static Tree_t *convert_procedure(ast_t *proc_node);
static Tree_t *convert_function(ast_t *func_node);

static void convert_routine_body(ast_t *body_node, ListNode_t **const_decls,
                                 ListNode_t **var_decls,
                                 ListNode_t **nested_subs,
                                 struct Statement **body_out) {
    if (body_node == NULL)
        return;

    ast_t *cursor = body_node->child;
    while (cursor != NULL) {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node != NULL) {
            switch (node->typ) {
            case PASCAL_T_CONST_SECTION:
                append_const_decls_from_section(node, const_decls);
                break;
            case PASCAL_T_VAR_SECTION:
                extend_list(var_decls, convert_var_section(node));
                break;
            case PASCAL_T_PROCEDURE_DECL: {
                if (nested_subs != NULL) {
                    Tree_t *proc = convert_procedure(node);
                    if (proc != NULL)
                        append_node(nested_subs, proc, LIST_TREE);
                }
                break;
            }
            case PASCAL_T_FUNCTION_DECL: {
                if (nested_subs != NULL) {
                    Tree_t *func = convert_function(node);
                    if (func != NULL)
                        append_node(nested_subs, func, LIST_TREE);
                }
                break;
            }
            case PASCAL_T_BEGIN_BLOCK:
            case PASCAL_T_MAIN_BLOCK:
                if (body_out != NULL)
                    *body_out = convert_block(node);
                break;
            case PASCAL_T_ASM_BLOCK: {
                if (body_out != NULL) {
                    struct Statement *stmt = convert_statement(node);
                    ListNode_t *stmts = NULL;
                    if (stmt != NULL)
                        append_node(&stmts, stmt, LIST_STMT);
                    *body_out = mk_compoundstatement(node->line, stmts);
                }
                break;
            }
            default:
                break;
            }
        }
        cursor = cursor->next;
    }
}

static void append_uses_from_section(ast_t *uses_node, ListNode_t **dest) {
    if (uses_node == NULL || dest == NULL)
        return;

    ast_t *unit = uses_node->child;
    while (unit != NULL) {
        if (unit->typ == PASCAL_T_USES_UNIT) {
            char *dup = dup_symbol(unit);
            if (dup != NULL)
                append_node(dest, dup, LIST_STRING);
        }
        unit = unit->next;
    }
}

static void append_type_decls_from_section(ast_t *type_section, ListNode_t **dest) {
    if (type_section == NULL || dest == NULL)
        return;

    ast_t *type_decl = type_section->child;
    while (type_decl != NULL) {
        if (type_decl->typ == PASCAL_T_TYPE_DECL) {
            Tree_t *decl = convert_type_decl(type_decl);
            if (decl != NULL)
                append_node(dest, decl, LIST_TREE);
        }
        type_decl = type_decl->next;
    }
}

static int map_relop_tag(int tag) {
    switch (tag) {
    case PASCAL_T_EQ: return EQ;
    case PASCAL_T_NE: return NE;
    case PASCAL_T_LT: return LT;
    case PASCAL_T_LE: return LE;
    case PASCAL_T_GT: return GT;
    case PASCAL_T_GE: return GE;
    case PASCAL_T_AND: return AND;
    case PASCAL_T_OR:  return OR;
    default: return UNKNOWN_TYPE;
    }
}

static int map_addop_tag(int tag) {
    switch (tag) {
    case PASCAL_T_ADD: return PLUS;
    case PASCAL_T_SUB: return MINUS;
    case PASCAL_T_OR:  return OR;
    default: return UNKNOWN_TYPE;
    }
}

static int map_mulop_tag(int tag) {
    switch (tag) {
    case PASCAL_T_MUL: return STAR;
    case PASCAL_T_DIV: return SLASH;
    case PASCAL_T_INTDIV: return DIV;
    case PASCAL_T_MOD: return MOD;
    case PASCAL_T_AND: return AND;
    case PASCAL_T_XOR: return XOR;
    case PASCAL_T_SHL: return SHL;
    case PASCAL_T_SHR: return SHR;
    default: return UNKNOWN_TYPE;
    }
}

static void append_asm_line(char **buffer, size_t *length, const char *line) {
    if (line == NULL)
        return;
    size_t line_len = strlen(line);
    size_t new_len = *length + line_len + 1; /* newline */
    char *tmp = realloc(*buffer, new_len + 1);
    if (tmp == NULL)
        return;
    memcpy(tmp + *length, line, line_len);
    tmp[*length + line_len] = '\n';
    tmp[new_len] = '\0';
    *buffer = tmp;
    *length = new_len;
}

static void collect_asm_lines(ast_t *node, char **buffer, size_t *length) {
    for (ast_t *cur = node; cur != NULL && cur != ast_nil; cur = cur->next) {
        if (cur->sym != NULL && cur->sym->name != NULL)
            append_asm_line(buffer, length, cur->sym->name);
        if (cur->child != NULL)
            collect_asm_lines(cur->child, buffer, length);
    }
}

static char *collect_asm_text(ast_t *block_node) {
    char *buffer = NULL;
    size_t length = 0;
    collect_asm_lines(block_node, &buffer, &length);
    return buffer;
}

static struct Expression *convert_factor(ast_t *expr_node) {
    expr_node = unwrap_pascal_node(expr_node);
    if (expr_node == NULL) {
        return NULL;
    }

    switch (expr_node->typ) {
    case PASCAL_T_INTEGER:
        return mk_inum(expr_node->line, strtoll(expr_node->sym->name, NULL, 10));
    case PASCAL_T_REAL:
        return mk_rnum(expr_node->line, strtof(expr_node->sym->name, NULL));
    case PASCAL_T_STRING:
    case PASCAL_T_CHAR:
        return mk_string(expr_node->line, dup_symbol(expr_node));
    case PASCAL_T_BOOLEAN:
        if (expr_node->sym != NULL && expr_node->sym->name != NULL) {
            const char *value = expr_node->sym->name;
            int bool_value = (strcasecmp(value, "true") == 0);
            return mk_bool(expr_node->line, bool_value);
        }
        return mk_bool(expr_node->line, 0);
    case PASCAL_T_IDENTIFIER:
        return mk_varid(expr_node->line, dup_symbol(expr_node));
    case PASCAL_T_FUNC_CALL: {
        ast_t *child = expr_node->child;
        char *id = NULL;
        if (child != NULL) {
            if (child->typ == PASCAL_T_IDENTIFIER) {
                id = dup_symbol(child);
            } else if (child->child != NULL && child->child->typ == PASCAL_T_IDENTIFIER) {
                id = dup_symbol(child->child);
            }
            child = child->next;
        }
        ListNode_t *args = convert_expression_list(child);
        return mk_functioncall(expr_node->line, id, args);
    }
    case PASCAL_T_ARRAY_ACCESS: {
        ast_t *array_id = expr_node->child;
        ast_t *index_expr = array_id != NULL ? array_id->next : NULL;
        struct Expression *index = convert_expression(index_expr);
        return mk_arrayaccess(expr_node->line, dup_symbol(array_id), index);
    }
    default:
        return NULL;
    }
}

static struct Expression *convert_binary_expr(ast_t *node, int type) {
    ast_t *left_node = node->child;
    ast_t *right_node = left_node != NULL ? left_node->next : NULL;
    struct Expression *left = convert_expression(left_node);
    struct Expression *right = convert_expression(right_node);

    switch (type) {
    case PASCAL_T_ADD:
    case PASCAL_T_SUB:
    case PASCAL_T_OR:
        return mk_addop(node->line, map_addop_tag(type), left, right);
    case PASCAL_T_MUL:
    case PASCAL_T_DIV:
    case PASCAL_T_INTDIV:
    case PASCAL_T_MOD:
    case PASCAL_T_AND:
    case PASCAL_T_XOR:
    case PASCAL_T_SHL:
    case PASCAL_T_SHR:
        return mk_mulop(node->line, map_mulop_tag(type), left, right);
    case PASCAL_T_EQ:
    case PASCAL_T_NE:
    case PASCAL_T_LT:
    case PASCAL_T_LE:
    case PASCAL_T_GT:
    case PASCAL_T_GE:
        return mk_relop(node->line, map_relop_tag(type), left, right);
    default:
        break;
    }
    return NULL;
}

static struct Expression *convert_unary_expr(ast_t *node) {
    struct Expression *inner = convert_expression(node->child);
    // Only negate for PASCAL_T_NEG, unary plus (PASCAL_T_POS) returns the inner expression as-is
    if (node->typ == PASCAL_T_NEG) {
        return mk_signterm(node->line, inner);
    }
    return inner;
}

static const char *tag_name(tag_t tag) {
    const char *name = pascal_tag_to_string(tag);
    return (name != NULL) ? name : "UNKNOWN";
}

static struct Expression *convert_expression(ast_t *expr_node) {
    expr_node = unwrap_pascal_node(expr_node);
    if (expr_node == NULL)
        return NULL;

    switch (expr_node->typ) {
    case PASCAL_T_INTEGER:
    case PASCAL_T_REAL:
    case PASCAL_T_STRING:
    case PASCAL_T_CHAR:
    case PASCAL_T_BOOLEAN:
    case PASCAL_T_IDENTIFIER:
    case PASCAL_T_FUNC_CALL:
    case PASCAL_T_ARRAY_ACCESS:
        return convert_factor(expr_node);
    case PASCAL_T_ADD:
    case PASCAL_T_SUB:
    case PASCAL_T_MUL:
    case PASCAL_T_DIV:
    case PASCAL_T_INTDIV:
    case PASCAL_T_MOD:
    case PASCAL_T_EQ:
    case PASCAL_T_NE:
    case PASCAL_T_LT:
    case PASCAL_T_LE:
    case PASCAL_T_GT:
    case PASCAL_T_GE:
    case PASCAL_T_AND:
    case PASCAL_T_OR:
    case PASCAL_T_XOR:
    case PASCAL_T_SHL:
    case PASCAL_T_SHR:
        return convert_binary_expr(expr_node, expr_node->typ);
    case PASCAL_T_NEG:
    case PASCAL_T_POS:
        return convert_unary_expr(expr_node);
    case PASCAL_T_NOT:
        return mk_relop(expr_node->line, NOT, convert_expression(expr_node->child), NULL);
    case PASCAL_T_TUPLE:
        return convert_expression(expr_node->child);
    case PASCAL_T_FIELD_WIDTH:
        return convert_field_width_expr(expr_node);
    default: {
        const char *name = tag_name(expr_node->typ);
        fprintf(stderr, "ERROR: unsupported expression tag %d (%s) at line %d.",
                expr_node->typ, name, expr_node->line);
        if (expr_node->sym != NULL && expr_node->sym->name != NULL)
            fprintf(stderr, " (symbol: %s)", expr_node->sym->name);
        fprintf(stderr, "\n");
        break;
    }
    }

    return NULL;
}

static ListNode_t *convert_expression_list(ast_t *arg_node) {
    ListNode_t *args = NULL;
    ast_t *cur = arg_node;

    /*
     * cparser often wraps Pascal arguments in synthetic tuple/statement nodes. Unwrap each
     * layer so we only convert real expressions and preserve field-width wrappers explicitly.
     */
    while (cur != NULL && cur != ast_nil) {
        ast_t *unwrapped = unwrap_pascal_node(cur);
        if (unwrapped != NULL && unwrapped->typ == PASCAL_T_FIELD_WIDTH) {
            struct Expression *expr = convert_field_width_expr(unwrapped);
            if (expr != NULL)
                append_node(&args, expr, LIST_EXPR);
        } else if (unwrapped != NULL) {
            struct Expression *expr = convert_expression(unwrapped);
            if (expr != NULL)
                append_node(&args, expr, LIST_EXPR);
        }
        cur = cur->next;
    }

    return args;
}

static struct Expression *convert_field_width_expr(ast_t *field_width_node) {
    if (field_width_node == NULL)
        return NULL;

    ast_t *base_node = field_width_node->child;
    if (base_node == NULL)
        return NULL;

    struct Expression *base_expr = convert_expression(base_node);
    if (base_expr == NULL)
        return NULL;

    ast_t *width_node = base_node->next;
    if (width_node != NULL && width_node != ast_nil) {
        struct Expression *width_expr = convert_expression(width_node);
        base_expr->field_width = width_expr;
    }

    ast_t *precision_node = NULL;
    if (width_node != NULL)
        precision_node = width_node->next;
    if (precision_node != NULL && precision_node != ast_nil) {
        struct Expression *precision_expr = convert_expression(precision_node);
        base_expr->field_precision = precision_expr;
    }

    return base_expr;
}

static struct Statement *convert_assignment(ast_t *assign_node) {
    ast_t *lhs = assign_node->child;
    ast_t *rhs = lhs != NULL ? lhs->next : NULL;

    struct Expression *left = convert_expression(lhs);
    struct Expression *right = convert_expression(rhs);
    return mk_varassign(assign_node->line, left, right);
}

static struct Statement *convert_proc_call(ast_t *call_node, bool implicit_identifier) {
    ast_t *child = call_node->child;
    ast_t *args_start = NULL;
    char *id = NULL;

    if (call_node->typ == PASCAL_T_IDENTIFIER) {
        id = dup_symbol(call_node);
        args_start = call_node->next;
    } else if (child != NULL) {
        if (implicit_identifier && child->typ == PASCAL_T_IDENTIFIER) {
            id = dup_symbol(child);
            args_start = child->next;
        } else {
            if (child->typ == PASCAL_T_IDENTIFIER) {
                id = dup_symbol(child);
                args_start = child->next;
            } else if (child->child != NULL && child->child->typ == PASCAL_T_IDENTIFIER) {
                id = dup_symbol(child->child);
                args_start = child->next;
            }
        }
    }

    ListNode_t *args = convert_expression_list(args_start);
    struct Statement *call = mk_procedurecall(call_node->line, id, args);
    return call;
}

static struct Statement *convert_statement(ast_t *stmt_node) {
    stmt_node = unwrap_pascal_node(stmt_node);
    if (stmt_node == NULL)
        return NULL;

    switch (stmt_node->typ) {
    case PASCAL_T_ASSIGNMENT:
        return convert_assignment(stmt_node);
    case PASCAL_T_STATEMENT: {
        ast_t *inner = unwrap_pascal_node(stmt_node->child);
        if (inner == NULL)
            return NULL;
        if (inner->typ == PASCAL_T_IDENTIFIER) {
            char *name = dup_symbol(inner);
            if (name != NULL) {
                bool is_assembler = strcasecmp(name, "assembler") == 0;
                free(name);
                if (is_assembler)
                    return NULL;
            }
        }
        if (inner->typ == PASCAL_T_IDENTIFIER || inner->typ == PASCAL_T_FUNC_CALL)
            return convert_proc_call(inner, inner->typ == PASCAL_T_IDENTIFIER);
        return convert_statement(inner);
    }
    case PASCAL_T_FUNC_CALL:
        return convert_proc_call(stmt_node, true);
    case PASCAL_T_ASM_BLOCK: {
        char *code = collect_asm_text(stmt_node->child);
        return mk_asmblock(stmt_node->line, code);
    }
    case PASCAL_T_BEGIN_BLOCK:
        return convert_block(stmt_node);
    case PASCAL_T_IF_STMT: {
        ast_t *cond = stmt_node->child;
        ast_t *then_wrapper = cond != NULL ? cond->next : NULL;
        ast_t *else_wrapper = then_wrapper != NULL ? then_wrapper->next : NULL;

        struct Expression *condition = convert_expression(cond);
        struct Statement *then_stmt = convert_statement(unwrap_pascal_node(then_wrapper));
        struct Statement *else_stmt = convert_statement(unwrap_pascal_node(else_wrapper));
        return mk_ifthen(stmt_node->line, condition, then_stmt, else_stmt);
    }
    case PASCAL_T_WHILE_STMT: {
        ast_t *cond = stmt_node->child;
        ast_t *body = unwrap_pascal_node(cond != NULL ? cond->next : NULL);
        struct Expression *condition = convert_expression(cond);
        struct Statement *body_stmt = convert_statement(body);
        return mk_while(stmt_node->line, condition, body_stmt);
    }
    case PASCAL_T_REPEAT_STMT: {
        ast_t *body_wrapper = stmt_node->child;
        ast_t *cond_node = body_wrapper != NULL ? body_wrapper->next : NULL;

        ListNode_t *body_list = NULL;
        if (body_wrapper != NULL) {
            ast_t *body_nodes = body_wrapper->child != NULL ? body_wrapper->child : body_wrapper;
            body_list = convert_statement_list(body_nodes);
        }

        struct Expression *condition = convert_expression(cond_node);
        return mk_repeat(stmt_node->line, body_list, condition);
    }
    case PASCAL_T_FOR_STMT: {
        ast_t *init_node = unwrap_pascal_node(stmt_node->child);
        ast_t *limit_wrapper = init_node != NULL ? init_node->next : NULL;
        ast_t *end_node = unwrap_pascal_node(limit_wrapper);
        ast_t *body_node = unwrap_pascal_node(limit_wrapper != NULL ? limit_wrapper->next : NULL);

        struct Expression *end_expr = convert_expression(end_node);
        struct Statement *body_stmt = convert_statement(body_node);

        if (init_node != NULL && init_node->typ == PASCAL_T_ASSIGNMENT) {
            struct Statement *assign_stmt = convert_assignment(init_node);
            return mk_forassign(stmt_node->line, assign_stmt, end_expr, body_stmt);
        }

        struct Expression *var_expr = convert_expression(init_node);
        if (var_expr == NULL)
            return NULL;
        return mk_forvar(stmt_node->line, var_expr, end_expr, body_stmt);
    }
    default: {
        const char *name = tag_name(stmt_node->typ);
        fprintf(stderr, "ERROR: unsupported statement tag %d (%s) at line %d.",
                stmt_node->typ, name, stmt_node->line);
        if (stmt_node->sym != NULL && stmt_node->sym->name != NULL)
            fprintf(stderr, " (symbol: %s)", stmt_node->sym->name);
        fprintf(stderr, "\n");
        break;
    }
    }

    return NULL;
}

static ListNode_t *convert_statement_list(ast_t *stmt_list_node) {
    ListNode_t *stmts = NULL;
    ast_t *cur = stmt_list_node;

    while (cur != NULL && cur != ast_nil) {
        ast_t *unwrapped = unwrap_pascal_node(cur);
        struct Statement *stmt = convert_statement(unwrapped);
        if (stmt != NULL)
            append_node(&stmts, stmt, LIST_STMT);
        cur = cur->next;
    }

    return stmts;
}

static struct Statement *convert_block(ast_t *block_node) {
    if (block_node == NULL)
        return NULL;

    ast_t *stmts = block_node->child;
    ListNode_t *list = convert_statement_list(stmts);
    return mk_compoundstatement(block_node->line, list);
}

static Tree_t *convert_procedure(ast_t *proc_node) {
    ast_t *cur = proc_node->child;
    char *id = NULL;

    if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER)
        id = dup_symbol(cur);

    if (cur != NULL)
        cur = cur->next;

    ListNode_t *params = NULL;
    if (cur != NULL && cur->typ == PASCAL_T_PARAM_LIST) {
        ast_t *param_cursor = cur->child;
        params = convert_param_list(&param_cursor);
        cur = cur->next;
    } else {
        while (cur != NULL && cur->typ == PASCAL_T_PARAM) {
            ListNode_t *param_nodes = convert_param(cur);
            extend_list(&params, param_nodes);
            cur = cur->next;
        }
    }

    ListNode_t *const_decls = NULL;
    ListNode_t *var_decls = NULL;
    ListNode_t *nested_subs = NULL;
    struct Statement *body = NULL;

    while (cur != NULL) {
        switch (cur->typ) {
        case PASCAL_T_CONST_SECTION:
            append_const_decls_from_section(cur, &const_decls);
            break;
        case PASCAL_T_VAR_SECTION:
            extend_list(&var_decls, convert_var_section(cur));
            break;
        case PASCAL_T_PROCEDURE_DECL:
        case PASCAL_T_FUNCTION_DECL: {
            Tree_t *sub = (cur->typ == PASCAL_T_PROCEDURE_DECL)
                              ? convert_procedure(cur)
                              : convert_function(cur);
            if (sub != NULL)
                append_node(&nested_subs, sub, LIST_TREE);
            break;
        }
        case PASCAL_T_FUNCTION_BODY:
            convert_routine_body(cur, &const_decls, &var_decls, &nested_subs, &body);
            break;
        case PASCAL_T_BEGIN_BLOCK:
            body = convert_block(cur);
            break;
        case PASCAL_T_ASM_BLOCK: {
            struct Statement *stmt = convert_statement(cur);
            ListNode_t *stmts = NULL;
            if (stmt != NULL)
                append_node(&stmts, stmt, LIST_STMT);
            body = mk_compoundstatement(cur->line, stmts);
            break;
        }
        default:
            break;
        }
        cur = cur->next;
    }

    Tree_t *tree = mk_procedure(proc_node->line, id, params, const_decls, var_decls, nested_subs, body, 0, 0);
    return tree;
}

static Tree_t *convert_function(ast_t *func_node) {
    ast_t *cur = func_node->child;
    char *id = NULL;

    if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER)
        id = dup_symbol(cur);

    if (cur != NULL)
        cur = cur->next;

    ListNode_t *params = NULL;
    if (cur != NULL && cur->typ == PASCAL_T_PARAM_LIST) {
        ast_t *param_cursor = cur->child;
        params = convert_param_list(&param_cursor);
        cur = cur->next;
    } else {
        while (cur != NULL && cur->typ == PASCAL_T_PARAM) {
            ListNode_t *param_nodes = convert_param(cur);
            extend_list(&params, param_nodes);
            cur = cur->next;
        }
    }

    char *return_type_id = NULL;
    int return_type = UNKNOWN_TYPE;

    if (cur != NULL && cur->typ == PASCAL_T_RETURN_TYPE) {
        return_type = convert_type_spec(cur->child, &return_type_id, NULL, NULL);
        cur = cur->next;
    }

    ListNode_t *const_decls = NULL;
    ListNode_t *var_decls = NULL;
    ListNode_t *nested_subs = NULL;
    struct Statement *body = NULL;

    while (cur != NULL) {
        switch (cur->typ) {
        case PASCAL_T_CONST_SECTION:
            append_const_decls_from_section(cur, &const_decls);
            break;
        case PASCAL_T_VAR_SECTION:
            extend_list(&var_decls, convert_var_section(cur));
            break;
        case PASCAL_T_PROCEDURE_DECL:
        case PASCAL_T_FUNCTION_DECL: {
            Tree_t *sub = (cur->typ == PASCAL_T_PROCEDURE_DECL)
                              ? convert_procedure(cur)
                              : convert_function(cur);
            if (sub != NULL)
                append_node(&nested_subs, sub, LIST_TREE);
            break;
        }
        case PASCAL_T_FUNCTION_BODY:
            convert_routine_body(cur, &const_decls, &var_decls, &nested_subs, &body);
            break;
        case PASCAL_T_BEGIN_BLOCK:
            body = convert_block(cur);
            break;
        case PASCAL_T_ASM_BLOCK: {
            struct Statement *stmt = convert_statement(cur);
            ListNode_t *stmts = NULL;
            if (stmt != NULL)
                append_node(&stmts, stmt, LIST_STMT);
            body = mk_compoundstatement(cur->line, stmts);
            break;
        }
        default:
            break;
        }
        cur = cur->next;
    }

    Tree_t *tree = mk_function(func_node->line, id, params, const_decls, var_decls, nested_subs, body,
                               return_type, return_type_id, 0, 0);
    return tree;
}

Tree_t *tree_from_pascal_ast(ast_t *program_ast) {
    if (program_ast == NULL)
        return NULL;

    ast_t *cur = program_ast;
    if (cur->typ == PASCAL_T_NONE)
        cur = cur->child;

    if (cur == NULL) {
        fprintf(stderr, "ERROR: Empty Pascal AST.\n");
        return NULL;
    }

    if (cur->typ == PASCAL_T_PROGRAM_DECL) {
        ast_t *program_name_node = cur->child;
        char *program_id = program_name_node != NULL ? dup_symbol(program_name_node) : strdup("main");

        ListNode_t *args = NULL;
        ListNode_t *uses = NULL;
        ListNode_t *const_decls = NULL;
        ListNode_t *var_decls = NULL;
        ListNode_t *type_decls = NULL;
        ListNode_t *subprograms = NULL;
        struct Statement *body = NULL;

        ast_t *section = program_name_node != NULL ? program_name_node->next : NULL;
        while (section != NULL) {
            switch (section->typ) {
            case PASCAL_T_CONST_SECTION:
                append_const_decls_from_section(section, &const_decls);
                break;
            case PASCAL_T_VAR_SECTION:
                extend_list(&var_decls, convert_var_section(section));
                break;
            case PASCAL_T_TYPE_SECTION:
                append_type_decls_from_section(section, &type_decls);
                break;
            case PASCAL_T_USES_SECTION:
                append_uses_from_section(section, &uses);
                break;
            case PASCAL_T_PROCEDURE_DECL:
            case PASCAL_T_FUNCTION_DECL: {
                Tree_t *sub = (section->typ == PASCAL_T_PROCEDURE_DECL)
                                  ? convert_procedure(section)
                                  : convert_function(section);
                if (sub != NULL)
                    append_node(&subprograms, sub, LIST_TREE);
                break;
            }
            case PASCAL_T_BEGIN_BLOCK:
            case PASCAL_T_MAIN_BLOCK:
                body = convert_block(section);
                break;
            default:
                break;
            }
            section = section->next;
        }

        Tree_t *tree = mk_program(cur->line, program_id, args, uses, const_decls, var_decls, type_decls, subprograms, body);
        return tree;
    }

    if (cur->typ == PASCAL_T_UNIT_DECL) {
        ast_t *unit_name_node = cur->child;
        char *unit_id = unit_name_node != NULL ? dup_symbol(unit_name_node) : strdup("unit");

        ListNode_t *interface_uses = NULL;
        ListNode_t *interface_type_decls = NULL;
        ListNode_t *interface_var_decls = NULL;
        ListNode_t *implementation_uses = NULL;
        ListNode_t *implementation_type_decls = NULL;
        ListNode_t *implementation_var_decls = NULL;
        ListNode_t *subprograms = NULL;
        struct Statement *initialization = NULL;

        ast_t *interface_node = unit_name_node != NULL ? unit_name_node->next : NULL;
        ast_t *implementation_node = interface_node != NULL ? interface_node->next : NULL;
        ast_t *initialization_node = implementation_node != NULL ? implementation_node->next : NULL;

        if (interface_node != NULL && interface_node->typ == PASCAL_T_INTERFACE_SECTION) {
            ast_t *section = interface_node->child;
            while (section != NULL) {
                ast_t *node = unwrap_pascal_node(section);
                if (node != NULL) {
                    switch (node->typ) {
                    case PASCAL_T_USES_SECTION:
                        append_uses_from_section(node, &interface_uses);
                        break;
                    case PASCAL_T_TYPE_SECTION:
                        append_type_decls_from_section(node, &interface_type_decls);
                        break;
                    case PASCAL_T_VAR_SECTION:
                        extend_list(&interface_var_decls, convert_var_section(node));
                        break;
                    default:
                        break;
                    }
                }
                section = section->next;
            }
        }

        if (implementation_node != NULL && implementation_node->typ == PASCAL_T_IMPLEMENTATION_SECTION) {
            ast_t *definition = implementation_node->child;
            while (definition != NULL) {
                ast_t *node = unwrap_pascal_node(definition);
                if (node != NULL) {
                    switch (node->typ) {
                    case PASCAL_T_USES_SECTION:
                        append_uses_from_section(node, &implementation_uses);
                        break;
                    case PASCAL_T_TYPE_SECTION:
                        append_type_decls_from_section(node, &implementation_type_decls);
                        break;
                    case PASCAL_T_VAR_SECTION:
                        extend_list(&implementation_var_decls, convert_var_section(node));
                        break;
                    case PASCAL_T_PROCEDURE_DECL: {
                        Tree_t *proc = convert_procedure(node);
                        if (proc != NULL)
                            append_node(&subprograms, proc, LIST_TREE);
                        break;
                    }
                    case PASCAL_T_FUNCTION_DECL: {
                        Tree_t *func = convert_function(node);
                        if (func != NULL)
                            append_node(&subprograms, func, LIST_TREE);
                        break;
                    }
                    default:
                        break;
                    }
                }
                definition = definition->next;
            }
        }

        if (initialization_node != NULL) {
            ast_t *init_block = unwrap_pascal_node(initialization_node);
            if (init_block != NULL)
                initialization = convert_block(init_block);
        }

        Tree_t *tree = mk_unit(cur->line, unit_id, interface_uses, interface_type_decls,
                               interface_var_decls, implementation_uses,
                               implementation_type_decls, implementation_var_decls,
                               subprograms, initialization);
        return tree;
    }

    fprintf(stderr, "ERROR: Unsupported Pascal AST root type %d.\n", cur->typ);
    return NULL;
}
