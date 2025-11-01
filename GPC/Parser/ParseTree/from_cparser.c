#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
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
    int is_open_array;
    ListNode_t *array_dimensions;
    int is_pointer;
    int pointer_type;
    char *pointer_type_id;
    int is_set;
    int set_element_type;
    char *set_element_type_id;
    int is_enum;
    ListNode_t *enum_literals;
    int is_file;
    int file_type;
    char *file_type_id;
} TypeInfo;

static void destroy_type_info_contents(TypeInfo *info) {
    if (info == NULL)
        return;

    if (info->element_type_id != NULL) {
        free(info->element_type_id);
        info->element_type_id = NULL;
    }
    if (info->pointer_type_id != NULL) {
        free(info->pointer_type_id);
        info->pointer_type_id = NULL;
    }
    if (info->set_element_type_id != NULL) {
        free(info->set_element_type_id);
        info->set_element_type_id = NULL;
    }
    if (info->file_type_id != NULL) {
        free(info->file_type_id);
        info->file_type_id = NULL;
    }
    if (info->enum_literals != NULL) {
        destroy_list(info->enum_literals);
        info->enum_literals = NULL;
    }
    if (info->array_dimensions != NULL) {
        destroy_list(info->array_dimensions);
        info->array_dimensions = NULL;
    }
}

static int extract_constant_int(struct Expression *expr, long long *out_value);
static struct Expression *convert_set_literal(ast_t *set_node);
static char *pop_last_identifier(ListNode_t **ids);

static int typed_const_counter = 0;

static char *dup_symbol(ast_t *node) {
    while (node != NULL) {
        if (node->sym != NULL && node->sym->name != NULL)
            return strdup(node->sym->name);
        node = node->child;
    }
    return NULL;
}

typedef struct {
    ListNode_t *head;
    ListNode_t **tail_next;
} ListBuilder;

static void list_builder_init(ListBuilder *builder) {
    if (builder == NULL)
        return;
    builder->head = NULL;
    builder->tail_next = &builder->head;
}

static ListNode_t *list_builder_append(ListBuilder *builder, void *value, enum ListType type) {
    if (builder == NULL)
        return NULL;

    ListNode_t *node = CreateListNode(value, type);
    *builder->tail_next = node;
    builder->tail_next = &node->next;
    return node;
}

static ListNode_t *list_builder_finish(ListBuilder *builder) {
    if (builder == NULL)
        return NULL;
    return builder->head;
}

static void list_builder_extend(ListBuilder *builder, ListNode_t *nodes) {
    if (builder == NULL || nodes == NULL)
        return;

    *builder->tail_next = nodes;
    while (*builder->tail_next != NULL)
        builder->tail_next = &(*builder->tail_next)->next;
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
    if (strcasecmp(name, "char") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("char");
        return CHAR_TYPE;
    }
    if (strcasecmp(name, "file") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("file");
        return FILE_TYPE;
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
static struct Expression *convert_member_access(ast_t *node);
static struct Expression *convert_field_width_expr(ast_t *field_width_node);
static ListNode_t *convert_expression_list(ast_t *arg_node);
static ListNode_t *convert_statement_list(ast_t *stmt_list_node);
static struct Statement *build_nested_with_statements(int line,
                                                      ast_t *context_node,
                                                      struct Statement *body_stmt);

static int convert_type_spec(ast_t *type_spec, char **type_id_out,
                             struct RecordType **record_out, TypeInfo *type_info) {
    if (type_id_out != NULL)
        *type_id_out = NULL;
    if (record_out != NULL)
        *record_out = NULL;
    if (type_info != NULL) {
        type_info->is_array = 0;
        type_info->start = 0;
        type_info->end = 0;
        type_info->element_type = UNKNOWN_TYPE;
        type_info->element_type_id = NULL;
        type_info->is_open_array = 0;
        type_info->array_dimensions = NULL;
        type_info->is_pointer = 0;
        type_info->pointer_type = UNKNOWN_TYPE;
        type_info->pointer_type_id = NULL;
        type_info->is_set = 0;
        type_info->set_element_type = UNKNOWN_TYPE;
        type_info->set_element_type_id = NULL;
        type_info->is_enum = 0;
        type_info->enum_literals = NULL;
        type_info->is_file = 0;
        type_info->file_type = UNKNOWN_TYPE;
        type_info->file_type_id = NULL;
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
        if (type_info != NULL && result == FILE_TYPE) {
            type_info->is_file = 1;
            type_info->file_type = FILE_TYPE;
            if (type_id_out != NULL && *type_id_out != NULL)
                type_info->file_type_id = strdup(*type_id_out);
        }
        if (result == UNKNOWN_TYPE && type_id_out != NULL && *type_id_out == NULL) {
            *type_id_out = dup;
        } else {
            free(dup);
        }
        return result;
    }
    if (spec_node->typ == PASCAL_T_ARRAY_TYPE) {
        if (type_info != NULL) {
            type_info->is_array = 1;
            type_info->start = 0;
            type_info->end = -1;
            ListBuilder dims_builder;
            list_builder_init(&dims_builder);

            ast_t *child = spec_node->child;
            ast_t *element_node = child;
            while (element_node != NULL && element_node->next != NULL)
                element_node = element_node->next;

            for (ast_t *dim = child; dim != NULL && dim != element_node; dim = dim->next) {
                if (dim->typ == PASCAL_T_RANGE_TYPE) {
                    ast_t *lower = dim->child;
                    ast_t *upper = (lower != NULL) ? lower->next : NULL;
                    if (lower != NULL && upper != NULL && lower->sym != NULL && upper->sym != NULL) {
                        if (dims_builder.head == NULL) {
                            type_info->start = atoi(lower->sym->name);
                            type_info->end = atoi(upper->sym->name);
                        }
                        char buffer[128];
                        snprintf(buffer, sizeof(buffer), "%s..%s", lower->sym->name, upper->sym->name);
                        list_builder_append(&dims_builder, strdup(buffer), LIST_STRING);
                    } else {
                        type_info->is_open_array = 1;
                    }
                } else if (dim->typ == PASCAL_T_IDENTIFIER) {
                    type_info->is_open_array = 1;
                    list_builder_append(&dims_builder, dup_symbol(dim), LIST_STRING);
                } else {
                    type_info->is_open_array = 1;
                }
            }

            type_info->array_dimensions = list_builder_finish(&dims_builder);

            if (element_node != NULL) {
                if (element_node->typ == PASCAL_T_IDENTIFIER) {
                    char *dup = dup_symbol(element_node);
                    int mapped = map_type_name(dup, &type_info->element_type_id);
                    type_info->element_type = mapped;
                    if (mapped == UNKNOWN_TYPE && type_info->element_type_id == NULL)
                        type_info->element_type_id = dup;
                    else if (mapped != UNKNOWN_TYPE)
                        free(dup);
                } else if (element_node->typ == PASCAL_T_TYPE_SPEC) {
                    char *nested_id = NULL;
                    struct RecordType *nested_record = NULL;
                    TypeInfo nested_info = {0};
                    int mapped = convert_type_spec(element_node, &nested_id, &nested_record, &nested_info);
                    type_info->element_type = mapped;
                    if (type_info->element_type_id == NULL)
                        type_info->element_type_id = nested_id;
                    else if (nested_id != NULL)
                        free(nested_id);
                    if (nested_record != NULL)
                        destroy_record_type(nested_record);
                    destroy_type_info_contents(&nested_info);
                }
            }
        }
        return UNKNOWN_TYPE;
    }

    if (spec_node->typ == PASCAL_T_POINTER_TYPE) {
        if (type_info != NULL) {
            type_info->is_pointer = 1;
            ast_t *target = spec_node->child;
            while (target != NULL && target->typ != PASCAL_T_IDENTIFIER)
                target = target->next;
            if (target != NULL) {
                char *dup = dup_symbol(target);
                int mapped = map_type_name(dup, &type_info->pointer_type_id);
                type_info->pointer_type = mapped;
                if (mapped == UNKNOWN_TYPE && type_info->pointer_type_id == NULL)
                    type_info->pointer_type_id = dup;
                else if (mapped != UNKNOWN_TYPE)
                    free(dup);
                if (type_id_out != NULL && *type_id_out == NULL && type_info->pointer_type_id != NULL)
                    *type_id_out = strdup(type_info->pointer_type_id);
            }
        }
        return POINTER_TYPE;
    }

    if (spec_node->typ == PASCAL_T_SET) {
        if (type_info != NULL) {
            type_info->is_set = 1;
            ast_t *elem = spec_node->child;
            while (elem != NULL && elem->typ == PASCAL_T_NONE)
                elem = elem->child;
            if (elem != NULL) {
                if (elem->typ == PASCAL_T_IDENTIFIER) {
                    char *dup = dup_symbol(elem);
                    int mapped = map_type_name(dup, &type_info->set_element_type_id);
                    type_info->set_element_type = mapped;
                    if (mapped == UNKNOWN_TYPE && type_info->set_element_type_id == NULL)
                        type_info->set_element_type_id = dup;
                    else if (mapped != UNKNOWN_TYPE)
                        free(dup);
                }
            }
        }
        return SET_TYPE;
    }

    if (spec_node->typ == PASCAL_T_ENUMERATED_TYPE) {
        if (type_info != NULL) {
            type_info->is_enum = 1;
            ListBuilder enum_builder;
            list_builder_init(&enum_builder);
            ast_t *value = spec_node->child;
            while (value != NULL) {
                if (value->typ == PASCAL_T_IDENTIFIER)
                    list_builder_append(&enum_builder, dup_symbol(value), LIST_STRING);
                value = value->next;
            }
            type_info->enum_literals = list_builder_finish(&enum_builder);
        }
        return ENUM_TYPE;
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
    ListBuilder builder;
    list_builder_init(&builder);
    ast_t *cur = *cursor;
    while (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER) {
        char *dup = dup_symbol(cur);
        list_builder_append(&builder, dup, LIST_STRING);
        cur = cur->next;
    }
    *cursor = cur;
    return list_builder_finish(&builder);
}

static ListNode_t *convert_field_decl(ast_t *field_decl_node);
static void convert_record_members(ast_t *node, ListBuilder *builder);
static struct VariantPart *convert_variant_part(ast_t *variant_node, ListNode_t **out_tag_fields);
static struct VariantBranch *convert_variant_branch(ast_t *branch_node);

static ListNode_t *convert_field_decl(ast_t *field_decl_node) {
    if (field_decl_node == NULL || field_decl_node->typ != PASCAL_T_FIELD_DECL)
        return NULL;

    ast_t *cursor = field_decl_node->child;
    ListNode_t *names = convert_identifier_list(&cursor);
    if (names == NULL) {
        fprintf(stderr, "ERROR: record field declaration missing identifier list.\n");
        return NULL;
    }

    while (cursor != NULL && cursor->typ != PASCAL_T_TYPE_SPEC &&
           cursor->typ != PASCAL_T_RECORD_TYPE && cursor->typ != PASCAL_T_IDENTIFIER) {
        cursor = cursor->next;
    }

    char *field_type_id = NULL;
    struct RecordType *nested_record = NULL;
    TypeInfo field_info;
    memset(&field_info, 0, sizeof(TypeInfo));
    int field_type = UNKNOWN_TYPE;

    if (cursor != NULL) {
        field_type = convert_type_spec(cursor, &field_type_id, &nested_record, &field_info);
    } else if (names != NULL) {
        char *candidate = pop_last_identifier(&names);
        if (candidate != NULL) {
            char *mapped_id = NULL;
            int mapped_type = map_type_name(candidate, &mapped_id);
            if (mapped_type != UNKNOWN_TYPE) {
                field_type = mapped_type;
                field_type_id = mapped_id;
                free(candidate);
            } else {
                field_type_id = candidate;
            }
        }
    }

    ListBuilder result_builder;
    list_builder_init(&result_builder);

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
            field_desc->is_array = field_info.is_array;
            field_desc->array_start = field_info.start;
            field_desc->array_end = field_info.end;
            field_desc->array_element_type = field_info.element_type;
            field_desc->array_element_type_id = field_info.element_type_id;
            field_desc->array_is_open = field_info.is_open_array;
            field_info.element_type_id = NULL;
            list_builder_append(&result_builder, field_desc, LIST_RECORD_FIELD);
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
    destroy_type_info_contents(&field_info);

    return list_builder_finish(&result_builder);
}

static ListNode_t *convert_variant_labels(ast_t *labels_node) {
    if (labels_node == NULL)
        return NULL;

    ListBuilder labels_builder;
    list_builder_init(&labels_builder);

    ast_t *label_cursor = labels_node;
    while (label_cursor != NULL) {
        if (label_cursor->typ == PASCAL_T_CASE_LABEL && label_cursor->child != NULL) {
            struct Expression *label_expr = convert_expression(label_cursor->child);
            if (label_expr != NULL)
                list_builder_append(&labels_builder, label_expr, LIST_EXPR);
        } else if (label_cursor->typ == PASCAL_T_INTEGER ||
                   label_cursor->typ == PASCAL_T_IDENTIFIER ||
                   label_cursor->typ == PASCAL_T_CHAR ||
                   label_cursor->typ == PASCAL_T_CHAR_CODE ||
                   label_cursor->typ == PASCAL_T_STRING) {
            struct Expression *label_expr = convert_expression(label_cursor);
            if (label_expr != NULL)
                list_builder_append(&labels_builder, label_expr, LIST_EXPR);
        }
        label_cursor = label_cursor->next;
    }

    return list_builder_finish(&labels_builder);
}

static struct VariantBranch *convert_variant_branch(ast_t *branch_node) {
    if (branch_node == NULL || branch_node->typ != PASCAL_T_VARIANT_BRANCH)
        return NULL;

    struct VariantBranch *branch = (struct VariantBranch *)calloc(1, sizeof(struct VariantBranch));
    if (branch == NULL)
        return NULL;

    ast_t *cursor = branch_node->child;
    if (cursor != NULL && cursor->typ == PASCAL_T_CASE_LABEL_LIST) {
        branch->labels = convert_variant_labels(cursor->child);
        cursor = cursor->next;
    } else if (cursor != NULL && cursor->typ == PASCAL_T_CASE_LABEL) {
        branch->labels = convert_variant_labels(cursor);
        cursor = cursor->next;
    }

    ListBuilder members_builder;
    list_builder_init(&members_builder);
    convert_record_members(cursor, &members_builder);
    branch->members = list_builder_finish(&members_builder);

    return branch;
}

static struct VariantPart *convert_variant_part(ast_t *variant_node, ListNode_t **out_tag_fields) {
    if (out_tag_fields != NULL)
        *out_tag_fields = NULL;

    if (variant_node == NULL || variant_node->typ != PASCAL_T_VARIANT_PART)
        return NULL;

    struct VariantPart *variant = (struct VariantPart *)calloc(1, sizeof(struct VariantPart));
    if (variant == NULL)
        return NULL;

    ast_t *cursor = variant_node->child;
    if (cursor != NULL && cursor->typ == PASCAL_T_VARIANT_TAG) {
        ast_t *tag_cursor = cursor->child;
        char *tag_name = NULL;
        ast_t *type_cursor = tag_cursor;
        if (tag_cursor != NULL && tag_cursor->typ == PASCAL_T_IDENTIFIER && tag_cursor->next != NULL) {
            tag_name = dup_symbol(tag_cursor);
            type_cursor = tag_cursor->next;
        }

        char *tag_type_id = NULL;
        struct RecordType *tag_record = NULL;
        int tag_type = UNKNOWN_TYPE;
        if (type_cursor != NULL)
            tag_type = convert_type_spec(type_cursor, &tag_type_id, &tag_record, NULL);

        if (tag_name != NULL) {
            struct RecordField *tag_field = (struct RecordField *)calloc(1, sizeof(struct RecordField));
            if (tag_field != NULL) {
                tag_field->name = tag_name;
                tag_field->type = tag_type;
                tag_field->type_id = (tag_type_id != NULL) ? strdup(tag_type_id) : NULL;
                tag_field->nested_record = tag_record;
                tag_field->is_array = 0;
                tag_field->array_start = 0;
                tag_field->array_end = 0;
                tag_field->array_element_type = UNKNOWN_TYPE;
                tag_field->array_element_type_id = NULL;
                tag_field->array_is_open = 0;

                ListNode_t *field_node = CreateListNode(tag_field, LIST_RECORD_FIELD);
                if (out_tag_fields != NULL)
                    *out_tag_fields = field_node;
                variant->tag_field = tag_field;
                variant->tag_type = tag_type;
                variant->tag_type_id = NULL;
                variant->tag_record = NULL;
                tag_record = NULL;
            } else {
                free(tag_name);
                destroy_record_type(tag_record);
            }
        } else {
            variant->tag_field = NULL;
            variant->tag_type = tag_type;
            variant->tag_type_id = tag_type_id;
            variant->tag_record = tag_record;
            tag_type_id = NULL;
            tag_record = NULL;
        }

        variant->tag_type = tag_type;

        if (tag_type_id != NULL)
            free(tag_type_id);
        if (tag_record != NULL)
            destroy_record_type(tag_record);

        cursor = cursor->next;
    }

    ListBuilder branches_builder;
    list_builder_init(&branches_builder);
    for (; cursor != NULL; cursor = cursor->next) {
        if (cursor->typ != PASCAL_T_VARIANT_BRANCH)
            continue;
        struct VariantBranch *branch = convert_variant_branch(cursor);
        if (branch != NULL)
            list_builder_append(&branches_builder, branch, LIST_VARIANT_BRANCH);
    }
    variant->branches = list_builder_finish(&branches_builder);
    variant->has_cached_size = 0;
    variant->cached_size = 0;

    return variant;
}

static void convert_record_members(ast_t *node, ListBuilder *builder) {
    for (ast_t *cur = node; cur != NULL; cur = cur->next) {
        if (cur->typ == PASCAL_T_FIELD_DECL) {
            ListNode_t *fields = convert_field_decl(cur);
            list_builder_extend(builder, fields);
        } else if (cur->typ == PASCAL_T_VARIANT_PART) {
            ListNode_t *tag_fields = NULL;
            struct VariantPart *variant = convert_variant_part(cur, &tag_fields);
            list_builder_extend(builder, tag_fields);
            if (variant != NULL)
                list_builder_append(builder, variant, LIST_VARIANT_PART);
        }
    }
}

static struct RecordType *convert_record_type(ast_t *record_node) {
    if (record_node == NULL)
        return NULL;

    ListBuilder fields_builder;
    list_builder_init(&fields_builder);
    convert_record_members(record_node->child, &fields_builder);

    struct RecordType *record = (struct RecordType *)malloc(sizeof(struct RecordType));
    if (record == NULL) {
        destroy_list(fields_builder.head);
        return NULL;
    }
    record->fields = list_builder_finish(&fields_builder);
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
    if (param_node == NULL || param_node->typ != PASCAL_T_PARAM)
        return NULL;

    ast_t *modifier_node = param_node->child;
    ast_t *ids_cursor = modifier_node != NULL ? modifier_node->next : NULL;

    int is_var_param = 0;
    if (modifier_node != NULL && modifier_node->sym != NULL && modifier_node->sym->name != NULL) {
        const char *modifier_name = modifier_node->sym->name;
        if (strcasecmp(modifier_name, "var") == 0 || strcasecmp(modifier_name, "out") == 0)
            is_var_param = 1;
    }

    ast_t *cursor = ids_cursor;
    ListNode_t *ids = convert_identifier_list(&cursor);
    if (ids == NULL) {
        fprintf(stderr, "ERROR: parameter missing identifier list.\n");
        return NULL;
    }

    ast_t *type_node = cursor;
    char *type_id = NULL;
    int var_type = UNKNOWN_TYPE;

    if (type_node == NULL || type_node->typ != PASCAL_T_TYPE_SPEC) {
        fprintf(stderr, "ERROR: parameter missing type specification.\n");
        destroy_list(ids);
        return NULL;
    }

    var_type = convert_type_spec(type_node, &type_id, NULL, NULL);

    ListBuilder result_builder;
    list_builder_init(&result_builder);
    ListNode_t *id_node = ids;

    while (id_node != NULL) {
        ListNode_t *next_id = id_node->next;
        id_node->next = NULL;
        char *type_id_copy = type_id != NULL ? strdup(type_id) : NULL;
        Tree_t *param_decl = mk_vardecl(param_node->line, id_node, var_type, type_id_copy, is_var_param, 0, NULL);
        list_builder_append(&result_builder, param_decl, LIST_TREE);
        id_node = next_id;
    }

    if (type_id != NULL)
        free(type_id);

    return list_builder_finish(&result_builder);
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
    TypeInfo type_info = {0};
    ast_t *type_node = first_non_identifier;
    if (type_node != NULL && type_node->typ == PASCAL_T_TYPE_SPEC) {
        var_type = convert_type_spec(type_node, &type_id, NULL, &type_info);
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
            var_type = convert_type_spec(search, &type_id, NULL, &type_info);
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

    if (type_info.is_array) {
        int element_type = type_info.element_type;
        char *element_type_id = type_info.element_type_id;
        Tree_t *decl = mk_arraydecl(decl_node->line, ids, element_type, element_type_id,
                                    type_info.start, type_info.end, NULL);
        type_info.element_type_id = NULL;
        destroy_type_info_contents(&type_info);
        return decl;
    }

    if (type_info.is_pointer) {
        if (type_info.pointer_type_id != NULL && type_id == NULL)
            type_id = strdup(type_info.pointer_type_id);
        var_type = POINTER_TYPE;
    } else if (type_info.is_set) {
        if (type_info.set_element_type_id != NULL && type_id == NULL)
            type_id = strdup(type_info.set_element_type_id);
        var_type = SET_TYPE;
    } else if (type_info.is_enum) {
        var_type = ENUM_TYPE;
    } else if (type_info.is_file) {
        var_type = FILE_TYPE;
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

    destroy_type_info_contents(&type_info);

    return decl;
}

static ListNode_t *convert_var_section(ast_t *section_node) {
    ListBuilder decls_builder;
    list_builder_init(&decls_builder);
    ast_t *cur = section_node->child;

    while (cur != NULL && cur->typ == PASCAL_T_VAR_DECL) {
        Tree_t *decl = convert_var_decl(cur);
        if (decl != NULL)
            list_builder_append(&decls_builder, decl, LIST_TREE);
        cur = cur->next;
    }

    return list_builder_finish(&decls_builder);
}

static int lower_const_array(ast_t *const_decl_node, char **id_ptr, TypeInfo *type_info,
                             ast_t *value_node, ListBuilder *var_builder) {
    if (id_ptr == NULL || *id_ptr == NULL || type_info == NULL)
        return -1;

    if (var_builder == NULL) {
        fprintf(stderr,
                "ERROR: Cannot lower const array %s without a variable declaration list.\n",
                *id_ptr);
        return -1;
    }

    if (type_info->array_dimensions != NULL && type_info->array_dimensions->next != NULL) {
        fprintf(stderr, "ERROR: Unsupported multi-dimensional const array %s.\n", *id_ptr);
        return -1;
    }

    if (type_info->is_open_array) {
        fprintf(stderr, "ERROR: Open array typed const %s is not supported.\n", *id_ptr);
        return -1;
    }

    ast_t *tuple_node = value_node;
    if (tuple_node == NULL || tuple_node->typ != PASCAL_T_TUPLE) {
        fprintf(stderr, "ERROR: Const array %s must use tuple syntax for its initializer.\n",
                *id_ptr);
        return -1;
    }

    int start = type_info->start;
    int end = type_info->end;
    int expected_count = -1;
    if (end >= start)
        expected_count = end - start + 1;

    int actual_count = 0;
    for (ast_t *elem = tuple_node->child; elem != NULL; elem = elem->next)
        ++actual_count;

    if (expected_count >= 0 && actual_count != expected_count) {
        fprintf(stderr,
                "ERROR: Const array %s initializer count %d does not match declared range %d..%d.\n",
                *id_ptr, actual_count, start, end);
        return -1;
    }

    if (expected_count < 0)
        end = start + actual_count - 1;

    ListBuilder stmt_builder;
    list_builder_init(&stmt_builder);

    int index = start;
    ast_t *element = tuple_node->child;
    while (element != NULL) {
        ast_t *unwrapped = unwrap_pascal_node(element);
        struct Expression *rhs = convert_expression(unwrapped);
        if (rhs == NULL) {
            fprintf(stderr, "ERROR: Unsupported const array element in %s.\n", *id_ptr);
            destroy_list(stmt_builder.head);
            return -1;
        }

        struct Expression *index_expr = mk_inum(element->line, index);
        struct Expression *base_expr = mk_varid(element->line, strdup(*id_ptr));
        struct Expression *lhs = mk_arrayaccess(element->line, base_expr, index_expr);
        struct Statement *assign = mk_varassign(element->line, lhs, rhs);
        list_builder_append(&stmt_builder, assign, LIST_STMT);

        ++index;
        element = element->next;
    }

    ListNode_t *assignments = list_builder_finish(&stmt_builder);
    struct Statement *initializer = NULL;
    if (assignments != NULL)
        initializer = mk_compoundstatement(const_decl_node->line, assignments);

    ListNode_t *ids = CreateListNode(*id_ptr, LIST_STRING);
    Tree_t *array_decl = mk_arraydecl(const_decl_node->line, ids, type_info->element_type,
                                      type_info->element_type_id, start, end, initializer);
    type_info->element_type_id = NULL;

    if (type_info->array_dimensions != NULL) {
        destroy_list(type_info->array_dimensions);
        type_info->array_dimensions = NULL;
    }

    array_decl->tree_data.arr_decl_data.is_typed_const = 1;
    array_decl->tree_data.arr_decl_data.has_static_storage = 1;

    char label_buffer[64];
    snprintf(label_buffer, sizeof(label_buffer), "__gpc_tconst_array_%d", typed_const_counter);
    array_decl->tree_data.arr_decl_data.static_label = strdup(label_buffer);
    snprintf(label_buffer, sizeof(label_buffer), "__gpc_tconst_guard_%d", typed_const_counter);
    array_decl->tree_data.arr_decl_data.init_guard_label = strdup(label_buffer);
    ++typed_const_counter;

    list_builder_append(var_builder, array_decl, LIST_TREE);

    *id_ptr = NULL;
    return 0;
}

static Tree_t *convert_const_decl(ast_t *const_decl_node, ListBuilder *var_builder) {
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
    TypeInfo type_info = {0};

    if (cur != NULL && cur->typ == PASCAL_T_TYPE_SPEC) {
        convert_type_spec(cur, &type_id, NULL, &type_info);
        cur = cur->next;
    }

    ast_t *value_node = unwrap_pascal_node(cur);
    if (value_node == NULL) {
        fprintf(stderr, "ERROR: Unsupported const declaration for %s.\n", id);
        if (type_id != NULL)
            free(type_id);
        free(id);
        destroy_type_info_contents(&type_info);
        return NULL;
    }

    if (type_info.is_array) {
        if (lower_const_array(const_decl_node, &id, &type_info, value_node, var_builder) != 0)
            free(id);

        if (type_id != NULL)
            free(type_id);
        destroy_type_info_contents(&type_info);
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

    destroy_type_info_contents(&type_info);

    return decl;
}

static void append_const_decls_from_section(ast_t *const_section, ListNode_t **dest,
                                            ListBuilder *var_builder) {
    if (const_section == NULL || dest == NULL)
        return;

    ListNode_t **tail = dest;
    while (*tail != NULL)
        tail = &(*tail)->next;

    ast_t *const_decl = const_section->child;
    while (const_decl != NULL) {
        if (const_decl->typ == PASCAL_T_CONST_DECL) {
            Tree_t *decl = convert_const_decl(const_decl, var_builder);
            if (decl != NULL) {
                ListNode_t *node = CreateListNode(decl, LIST_TREE);
                *tail = node;
                tail = &node->next;
            }
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
    TypeInfo type_info = {0};
    int mapped_type = UNKNOWN_TYPE;
    if (spec_node != NULL)
        mapped_type = convert_type_spec(spec_node, &type_id, &record_type, &type_info);

    Tree_t *decl = NULL;
    if (record_type != NULL) {
        decl = mk_record_type(type_decl_node->line, id, record_type);
    } else if (type_info.is_array) {
        decl = mk_typealiasdecl(type_decl_node->line, id, 1, type_info.element_type,
                                 type_info.element_type_id, type_info.start, type_info.end);
        type_info.element_type_id = NULL;
    } else if (mapped_type != UNKNOWN_TYPE || type_id != NULL) {
        decl = mk_typealiasdecl(type_decl_node->line, id, 0, mapped_type, type_id, 0, 0);
        type_id = NULL;
    } else {
        decl = mk_typedecl(type_decl_node->line, id, 0, 0);
    }

    if (decl != NULL && decl->type == TREE_TYPE_DECL &&
        decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
        struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
        if (type_info.array_dimensions != NULL) {
            alias->array_dimensions = type_info.array_dimensions;
            type_info.array_dimensions = NULL;
        }
        alias->is_pointer = type_info.is_pointer;
        alias->pointer_type = type_info.pointer_type;
        if (type_info.pointer_type_id != NULL) {
            alias->pointer_type_id = type_info.pointer_type_id;
            type_info.pointer_type_id = NULL;
        }
        alias->is_set = type_info.is_set;
        alias->set_element_type = type_info.set_element_type;
        if (type_info.set_element_type_id != NULL) {
            alias->set_element_type_id = type_info.set_element_type_id;
            type_info.set_element_type_id = NULL;
        }
        alias->is_enum = type_info.is_enum;
        if (type_info.enum_literals != NULL) {
            alias->enum_literals = type_info.enum_literals;
            type_info.enum_literals = NULL;
        }
        alias->is_file = type_info.is_file;
        alias->file_type = type_info.file_type;
        if (type_info.file_type_id != NULL) {
            alias->file_type_id = type_info.file_type_id;
            type_info.file_type_id = NULL;
        }
    }

    if (type_id != NULL)
        free(type_id);
    destroy_type_info_contents(&type_info);

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
                                 ListBuilder *var_builder,
                                 ListNode_t **nested_subs,
                                 struct Statement **body_out) {
    if (body_node == NULL)
        return;

    ListNode_t **nested_tail = NULL;
    if (nested_subs != NULL) {
        nested_tail = nested_subs;
        while (*nested_tail != NULL)
            nested_tail = &(*nested_tail)->next;
    }

    ast_t *cursor = body_node->child;
    while (cursor != NULL) {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node != NULL) {
            switch (node->typ) {
            case PASCAL_T_CONST_SECTION:
                append_const_decls_from_section(node, const_decls, var_builder);
                break;
            case PASCAL_T_VAR_SECTION:
                list_builder_extend(var_builder, convert_var_section(node));
                break;
            case PASCAL_T_PROCEDURE_DECL: {
                if (nested_tail != NULL) {
                    Tree_t *proc = convert_procedure(node);
                    if (proc != NULL) {
                        ListNode_t *list_node = CreateListNode(proc, LIST_TREE);
                        *nested_tail = list_node;
                        nested_tail = &list_node->next;
                    }
                }
                break;
            }
            case PASCAL_T_FUNCTION_DECL: {
                if (nested_tail != NULL) {
                    Tree_t *func = convert_function(node);
                    if (func != NULL) {
                        ListNode_t *list_node = CreateListNode(func, LIST_TREE);
                        *nested_tail = list_node;
                        nested_tail = &list_node->next;
                    }
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
                    ListBuilder stmts_builder;
                    list_builder_init(&stmts_builder);
                    if (stmt != NULL)
                        list_builder_append(&stmts_builder, stmt, LIST_STMT);
                    *body_out = mk_compoundstatement(node->line, list_builder_finish(&stmts_builder));
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

    ListNode_t **tail = dest;
    while (*tail != NULL)
        tail = &(*tail)->next;

    ast_t *unit = uses_node->child;
    while (unit != NULL) {
        if (unit->typ == PASCAL_T_USES_UNIT) {
            char *dup = dup_symbol(unit);
            if (dup != NULL) {
                ListNode_t *node = CreateListNode(dup, LIST_STRING);
                *tail = node;
                tail = &node->next;
            }
        }
        unit = unit->next;
    }
}

static void append_type_decls_from_section(ast_t *type_section, ListNode_t **dest) {
    if (type_section == NULL || dest == NULL)
        return;

    ListNode_t **tail = dest;
    while (*tail != NULL)
        tail = &(*tail)->next;

    ast_t *type_decl = type_section->child;
    while (type_decl != NULL) {
        if (type_decl->typ == PASCAL_T_TYPE_DECL) {
            Tree_t *decl = convert_type_decl(type_decl);
            if (decl != NULL) {
                ListNode_t *node = CreateListNode(decl, LIST_TREE);
                *tail = node;
                tail = &node->next;
            }
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
    case PASCAL_T_IN: return IN;
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
    case PASCAL_T_ROL: return ROL;
    case PASCAL_T_ROR: return ROR;
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

static int extract_constant_int(struct Expression *expr, long long *out_value) {
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type) {
    case EXPR_INUM:
        *out_value = expr->expr_data.i_num;
        return 0;
    case EXPR_SIGN_TERM:
        if (extract_constant_int(expr->expr_data.sign_term, out_value) != 0)
            return 1;
        *out_value = -*out_value;
        return 0;
    case EXPR_ADDOP: {
        long long left_value = 0;
        long long right_value = 0;
        if (extract_constant_int(expr->expr_data.addop_data.left_expr, &left_value) != 0)
            return 1;
        if (extract_constant_int(expr->expr_data.addop_data.right_term, &right_value) != 0)
            return 1;
        if (expr->expr_data.addop_data.addop_type == PLUS)
            *out_value = left_value + right_value;
        else if (expr->expr_data.addop_data.addop_type == MINUS)
            *out_value = left_value - right_value;
        else
            return 1;
        return 0;
    }
    case EXPR_MULOP: {
        long long left_value = 0;
        long long right_value = 0;
        if (extract_constant_int(expr->expr_data.mulop_data.left_term, &left_value) != 0)
            return 1;
        if (extract_constant_int(expr->expr_data.mulop_data.right_factor, &right_value) != 0)
            return 1;
        if (expr->expr_data.mulop_data.mulop_type == STAR) {
            *out_value = left_value * right_value;
            return 0;
        }
        return 1;
    }
    default:
        break;
    }

    return 1;
}

static ListNode_t *append_set_element(ListNode_t *elements, struct SetElement *element)
{
    if (element == NULL)
        return elements;

    ListNode_t *node = CreateListNode(element, LIST_SET_ELEMENT);
    if (elements == NULL)
        return node;
    return PushListNodeBack(elements, node);
}

static struct Expression *convert_set_literal(ast_t *set_node) {
    if (set_node == NULL)
        return mk_set(0, 0, NULL, 1);

    uint32_t mask = 0;
    int all_constant = 1;
    ListNode_t *elements = NULL;

    for (ast_t *element = set_node->child; element != NULL && element != ast_nil; element = element->next) {
        ast_t *unwrapped = unwrap_pascal_node(element);
        if (unwrapped == NULL)
            continue;

        if (unwrapped->typ == PASCAL_T_RANGE) {
            ast_t *start_node = unwrap_pascal_node(unwrapped->child);
            ast_t *end_node = NULL;
            if (start_node != NULL)
                end_node = unwrap_pascal_node(start_node->next);

            struct Expression *start_expr = convert_expression(start_node);
            struct Expression *end_expr = convert_expression(end_node);
            elements = append_set_element(elements, mk_set_element(start_expr, end_expr));

            long long start_value = 0;
            long long end_value = 0;
            int have_start = (start_expr != NULL) && extract_constant_int(start_expr, &start_value) == 0;
            int have_end = (end_expr != NULL) && extract_constant_int(end_expr, &end_value) == 0;

            if (!have_start || !have_end)
            {
                all_constant = 0;
                continue;
            }

            if (start_value > end_value) {
                long long tmp = start_value;
                start_value = end_value;
                end_value = tmp;
            }
            for (long long value = start_value; value <= end_value; ++value) {
                if (value >= 0 && value < 32)
                    mask |= (uint32_t)1u << value;
            }
        } else {
            struct Expression *value_expr = convert_expression(unwrapped);
            elements = append_set_element(elements, mk_set_element(value_expr, NULL));

            long long value = 0;
            if (value_expr == NULL || extract_constant_int(value_expr, &value) != 0) {
                all_constant = 0;
                continue;
            }

            if (value >= 0 && value < 32)
                mask |= (uint32_t)1u << value;
        }
    }

    return mk_set(set_node->line, mask, elements, all_constant);
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
    case PASCAL_T_SET:
        return convert_set_literal(expr_node);
    case PASCAL_T_NIL:
        return mk_nil(expr_node->line);
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
        ast_t *array_node = expr_node->child;
        ast_t *index_expr_node = array_node != NULL ? array_node->next : NULL;
        struct Expression *base = convert_expression(array_node);
        struct Expression *index = convert_expression(index_expr_node);
        return mk_arrayaccess(expr_node->line, base, index);
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
    case PASCAL_T_ROL:
    case PASCAL_T_ROR:
        return mk_mulop(node->line, map_mulop_tag(type), left, right);
    case PASCAL_T_EQ:
    case PASCAL_T_NE:
    case PASCAL_T_LT:
    case PASCAL_T_LE:
    case PASCAL_T_GT:
    case PASCAL_T_GE:
    case PASCAL_T_IN:
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
    if (expr_node == NULL || expr_node == ast_nil)
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
    case PASCAL_T_SET:
    case PASCAL_T_NIL:
        return convert_factor(expr_node);
    case PASCAL_T_MEMBER_ACCESS:
        return convert_member_access(expr_node);
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
    case PASCAL_T_IN:
    case PASCAL_T_AND:
    case PASCAL_T_OR:
    case PASCAL_T_XOR:
    case PASCAL_T_SHL:
    case PASCAL_T_SHR:
    case PASCAL_T_ROL:
    case PASCAL_T_ROR:
        return convert_binary_expr(expr_node, expr_node->typ);
    case PASCAL_T_SET_UNION:
        return convert_binary_expr(expr_node, PASCAL_T_ADD);
    case PASCAL_T_SET_INTERSECT:
        return convert_binary_expr(expr_node, PASCAL_T_MUL);
    case PASCAL_T_SET_DIFF:
        return convert_binary_expr(expr_node, PASCAL_T_SUB);
    case PASCAL_T_SET_SYM_DIFF:
        return convert_binary_expr(expr_node, PASCAL_T_XOR);
    case PASCAL_T_NEG:
    case PASCAL_T_POS:
        return convert_unary_expr(expr_node);
    case PASCAL_T_NOT:
        return mk_relop(expr_node->line, NOT, convert_expression(expr_node->child), NULL);
    case PASCAL_T_TUPLE:
        return convert_expression(expr_node->child);
    case PASCAL_T_FIELD_WIDTH:
        return convert_field_width_expr(expr_node);
    case PASCAL_T_TYPECAST:
    {
        ast_t *type_node = expr_node->child;
        ast_t *value_node = (type_node != NULL) ? type_node->next : NULL;

        int target_type = UNKNOWN_TYPE;
        char *target_type_id = NULL;
        struct RecordType *record_type = NULL;
        TypeInfo type_info;
        memset(&type_info, 0, sizeof(TypeInfo));

        ast_t *unwrapped_type = unwrap_pascal_node(type_node);
        if (unwrapped_type != NULL)
        {
            target_type = convert_type_spec(unwrapped_type, &target_type_id,
                &record_type, &type_info);
            destroy_type_info_contents(&type_info);
            if (record_type != NULL)
            {
                destroy_record_type(record_type);
                record_type = NULL;
            }

            if (target_type == UNKNOWN_TYPE && target_type_id == NULL &&
                unwrapped_type->typ == PASCAL_T_IDENTIFIER)
            {
                target_type_id = dup_symbol(unwrapped_type);
            }
        }

        struct Expression *inner_expr = convert_expression(value_node);
        return mk_typecast(expr_node->line, target_type, target_type_id, inner_expr);
    }
    case PASCAL_T_DEREF:
        return mk_pointer_deref(expr_node->line, convert_expression(expr_node->child));
    case PASCAL_T_ADDR:
        return mk_addressof(expr_node->line, convert_expression(expr_node->child));
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
    ListBuilder builder;
    list_builder_init(&builder);
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
                list_builder_append(&builder, expr, LIST_EXPR);
        } else if (unwrapped != NULL) {
            struct Expression *expr = convert_expression(unwrapped);
            if (expr != NULL)
                list_builder_append(&builder, expr, LIST_EXPR);
        }
        cur = cur->next;
    }

    return list_builder_finish(&builder);
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

static struct Expression *convert_member_access(ast_t *node) {
    if (node == NULL)
        return NULL;

    ast_t *base_node = node->child;
    ast_t *field_node = (base_node != NULL) ? base_node->next : NULL;

    struct Expression *record_expr = convert_expression(base_node);

    char *field_id = NULL;
    ast_t *unwrapped = unwrap_pascal_node(field_node);
    if (unwrapped == NULL)
        unwrapped = field_node;

    if (unwrapped != NULL) {
        if (unwrapped->typ == PASCAL_T_IDENTIFIER) {
            field_id = dup_symbol(unwrapped);
        } else if (unwrapped->child != NULL && unwrapped->child->typ == PASCAL_T_IDENTIFIER) {
            field_id = dup_symbol(unwrapped->child);
        }
    }

    return mk_recordaccess(node->line, record_expr, field_id);
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

static struct Statement *build_nested_with_statements(int line,
                                                      ast_t *context_node,
                                                      struct Statement *body_stmt) {
    if (context_node == NULL || context_node == ast_nil)
        return NULL;

    ast_t *next_context = context_node->next;
    struct Statement *inner_stmt = body_stmt;
    if (next_context != NULL && next_context != ast_nil) {
        inner_stmt = build_nested_with_statements(line, next_context, body_stmt);
        if (inner_stmt == NULL)
            return NULL;
    }

    ast_t *unwrapped = unwrap_pascal_node(context_node);
    struct Expression *expr = convert_expression(unwrapped);
    if (expr == NULL)
        return NULL;

    return mk_with(line, expr, inner_stmt);
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
    case PASCAL_T_BREAK_STMT:
        return mk_break(stmt_node->line);
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
    case PASCAL_T_EXIT_STMT:
        return mk_exit(stmt_node->line);
    case PASCAL_T_WITH_STMT: {
        ast_t *contexts_wrapper = stmt_node->child;
        ast_t *body_node = unwrap_pascal_node(contexts_wrapper != NULL ? contexts_wrapper->next : NULL);

        struct Statement *body_stmt = convert_statement(body_node);
        if (body_stmt == NULL)
            return NULL;

        if (contexts_wrapper == NULL)
            return NULL;

        if (contexts_wrapper->typ != PASCAL_T_WITH_CONTEXTS) {
            ast_t *single_context = unwrap_pascal_node(contexts_wrapper);
            struct Expression *context_expr = convert_expression(single_context);
            if (context_expr == NULL)
                return NULL;
            return mk_with(stmt_node->line, context_expr, body_stmt);
        }

        ast_t *contexts_start = contexts_wrapper->child;
        if (contexts_start == NULL || contexts_start == ast_nil)
            return NULL;

        return build_nested_with_statements(stmt_node->line, contexts_start, body_stmt);
    }
    case PASCAL_T_TRY_BLOCK: {
        ListBuilder try_builder;
        ListBuilder finally_builder;
        ListBuilder except_builder;
        list_builder_init(&try_builder);
        list_builder_init(&finally_builder);
        list_builder_init(&except_builder);

        ast_t *cur = stmt_node->child;
        while (cur != NULL) {
            if (cur->typ == PASCAL_T_FINALLY_BLOCK || cur->typ == PASCAL_T_EXCEPT_BLOCK) {
                ListBuilder *target = (cur->typ == PASCAL_T_FINALLY_BLOCK) ? &finally_builder : &except_builder;
                ast_t *inner = cur->child;
                while (inner != NULL) {
                    struct Statement *inner_stmt = convert_statement(unwrap_pascal_node(inner));
                    if (inner_stmt != NULL)
                        list_builder_append(target, inner_stmt, LIST_STMT);
                    inner = inner->next;
                }
            } else {
                struct Statement *try_stmt = convert_statement(unwrap_pascal_node(cur));
                if (try_stmt != NULL)
                    list_builder_append(&try_builder, try_stmt, LIST_STMT);
            }
            cur = cur->next;
        }

        ListNode_t *try_stmts = list_builder_finish(&try_builder);
        ListNode_t *finally_stmts = list_builder_finish(&finally_builder);
        ListNode_t *except_stmts = list_builder_finish(&except_builder);

        if (finally_stmts != NULL)
            return mk_tryfinally(stmt_node->line, try_stmts, finally_stmts);
        return mk_tryexcept(stmt_node->line, try_stmts, except_stmts);
    }
    case PASCAL_T_RAISE_STMT: {
        struct Expression *exc_expr = convert_expression(unwrap_pascal_node(stmt_node->child));
        return mk_raise(stmt_node->line, exc_expr);
    }
    case PASCAL_T_INHERITED_STMT: {
        struct Expression *call_expr = convert_expression(unwrap_pascal_node(stmt_node->child));
        return mk_inherited(stmt_node->line, call_expr);
    }
    case PASCAL_T_CASE_STMT: {
        ast_t *selector = stmt_node->child;
        ast_t *branches_start = selector != NULL ? selector->next : NULL;

        struct Expression *selector_expr = convert_expression(selector);
        ListBuilder branches_builder;
        list_builder_init(&branches_builder);
        struct Statement *else_stmt = NULL;
        
        /* Walk through all siblings after the selector */
        ast_t *cur = branches_start;
        while (cur != NULL) {
            if (cur->typ == PASCAL_T_CASE_BRANCH) {
                /* Process this branch */
                ListBuilder labels_builder;
                list_builder_init(&labels_builder);
                struct Statement *branch_stmt = NULL;
                
                /* Walk through children of the branch */
                ast_t *child = cur->child;
                while (child != NULL) {
                    if (child->typ == PASCAL_T_CASE_LABEL_LIST) {
                        /* CASE_LABEL_LIST contains the labels */
                        ast_t *label_node = child->child;
                        while (label_node != NULL) {
                            if (label_node->typ == PASCAL_T_CASE_LABEL && label_node->child != NULL) {
                                struct Expression *label_expr = convert_expression(label_node->child);
                                if (label_expr != NULL) {
                                    list_builder_append(&labels_builder, label_expr, LIST_EXPR);
                                }
                            } else if (label_node->typ == PASCAL_T_INTEGER || 
                                       label_node->typ == PASCAL_T_IDENTIFIER) {
                                /* Direct value */
                                struct Expression *label_expr = convert_expression(label_node);
                                if (label_expr != NULL) {
                                    list_builder_append(&labels_builder, label_expr, LIST_EXPR);
                                }
                            }
                            label_node = label_node->next;
                        }
                    } else if (child->typ == PASCAL_T_CASE_LABEL) {
                        /* Single CASE_LABEL (not in a list) */
                        if (child->child != NULL) {
                            struct Expression *label_expr = convert_expression(child->child);
                            if (label_expr != NULL) {
                                list_builder_append(&labels_builder, label_expr, LIST_EXPR);
                            }
                        }
                    } else if (child->typ == PASCAL_T_STATEMENT || 
                               child->typ == PASCAL_T_FUNC_CALL ||
                               child->typ == PASCAL_T_BEGIN_BLOCK) {
                        /* This is the statement for this branch */
                        branch_stmt = convert_statement(child);
                        break; /* Statement is last */
                    }
                    child = child->next;
                }
                
                /* Create the branch */
                struct CaseBranch *branch = (struct CaseBranch *)malloc(sizeof(struct CaseBranch));
                if (branch != NULL) {
                    branch->labels = list_builder_finish(&labels_builder);
                    branch->stmt = branch_stmt;
                    list_builder_append(&branches_builder, branch, LIST_CASE_BRANCH);
                }
            } else if (cur->typ == PASCAL_T_ELSE) {
                /* Optional else clause */
                if (cur->child != NULL) {
                    else_stmt = convert_statement(cur->child);
                }
            }
            cur = cur->next;
        }
        
        return mk_case(stmt_node->line, selector_expr, list_builder_finish(&branches_builder), else_stmt);
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
    ListBuilder builder;
    list_builder_init(&builder);
    ast_t *cur = stmt_list_node;

    while (cur != NULL && cur != ast_nil) {
        ast_t *unwrapped = unwrap_pascal_node(cur);
        if (unwrapped == NULL) {
            cur = cur->next;
            continue;
        }

        if (unwrapped->typ == PASCAL_T_NONE && unwrapped->child == NULL) {
            cur = cur->next;
            continue;
        }

        struct Statement *stmt = convert_statement(unwrapped);
        if (stmt != NULL)
            list_builder_append(&builder, stmt, LIST_STMT);
        cur = cur->next;
    }

    return list_builder_finish(&builder);
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
    ListBuilder var_decls_builder;
    list_builder_init(&var_decls_builder);
    ListNode_t *nested_subs = NULL;
    ListNode_t **nested_tail = &nested_subs;
    struct Statement *body = NULL;

    while (cur != NULL) {
        switch (cur->typ) {
        case PASCAL_T_CONST_SECTION:
            append_const_decls_from_section(cur, &const_decls, &var_decls_builder);
            break;
        case PASCAL_T_VAR_SECTION:
            list_builder_extend(&var_decls_builder, convert_var_section(cur));
            break;
        case PASCAL_T_PROCEDURE_DECL:
        case PASCAL_T_FUNCTION_DECL: {
            Tree_t *sub = (cur->typ == PASCAL_T_PROCEDURE_DECL)
                              ? convert_procedure(cur)
                              : convert_function(cur);
            if (sub != NULL) {
                ListNode_t *node = CreateListNode(sub, LIST_TREE);
                *nested_tail = node;
                nested_tail = &node->next;
            }
            break;
        }
        case PASCAL_T_FUNCTION_BODY:
            convert_routine_body(cur, &const_decls, &var_decls_builder, &nested_subs, &body);
            nested_tail = &nested_subs;
            while (*nested_tail != NULL)
                nested_tail = &(*nested_tail)->next;
            break;
        case PASCAL_T_BEGIN_BLOCK:
            body = convert_block(cur);
            break;
        case PASCAL_T_ASM_BLOCK: {
            struct Statement *stmt = convert_statement(cur);
            ListBuilder stmts_builder;
            list_builder_init(&stmts_builder);
            if (stmt != NULL)
                list_builder_append(&stmts_builder, stmt, LIST_STMT);
            body = mk_compoundstatement(cur->line, list_builder_finish(&stmts_builder));
            break;
        }
        default:
            break;
        }
        cur = cur->next;
    }

    Tree_t *tree = mk_procedure(proc_node->line, id, params, const_decls,
                                list_builder_finish(&var_decls_builder), nested_subs, body, 0, 0);
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
    ListBuilder var_decls_builder;
    list_builder_init(&var_decls_builder);
    ListNode_t *nested_subs = NULL;
    ListNode_t **nested_tail = &nested_subs;
    struct Statement *body = NULL;

    while (cur != NULL) {
        switch (cur->typ) {
        case PASCAL_T_CONST_SECTION:
            append_const_decls_from_section(cur, &const_decls, &var_decls_builder);
            break;
        case PASCAL_T_VAR_SECTION:
            list_builder_extend(&var_decls_builder, convert_var_section(cur));
            break;
        case PASCAL_T_PROCEDURE_DECL:
        case PASCAL_T_FUNCTION_DECL: {
            Tree_t *sub = (cur->typ == PASCAL_T_PROCEDURE_DECL)
                              ? convert_procedure(cur)
                              : convert_function(cur);
            if (sub != NULL) {
                ListNode_t *node = CreateListNode(sub, LIST_TREE);
                *nested_tail = node;
                nested_tail = &node->next;
            }
            break;
        }
        case PASCAL_T_FUNCTION_BODY:
            convert_routine_body(cur, &const_decls, &var_decls_builder, &nested_subs, &body);
            nested_tail = &nested_subs;
            while (*nested_tail != NULL)
                nested_tail = &(*nested_tail)->next;
            break;
        case PASCAL_T_BEGIN_BLOCK:
            body = convert_block(cur);
            break;
        case PASCAL_T_ASM_BLOCK: {
            struct Statement *stmt = convert_statement(cur);
            ListBuilder stmts_builder;
            list_builder_init(&stmts_builder);
            if (stmt != NULL)
                list_builder_append(&stmts_builder, stmt, LIST_STMT);
            body = mk_compoundstatement(cur->line, list_builder_finish(&stmts_builder));
            break;
        }
        default:
            break;
        }
        cur = cur->next;
    }

    Tree_t *tree = mk_function(func_node->line, id, params, const_decls,
                               list_builder_finish(&var_decls_builder), nested_subs, body,
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
        ListBuilder var_decls_builder;
        list_builder_init(&var_decls_builder);
        ListNode_t *type_decls = NULL;
        ListNode_t *subprograms = NULL;
        ListNode_t **subprograms_tail = &subprograms;
        struct Statement *body = NULL;

        ast_t *section = program_name_node != NULL ? program_name_node->next : NULL;
        while (section != NULL) {
            switch (section->typ) {
            case PASCAL_T_CONST_SECTION:
                append_const_decls_from_section(section, &const_decls, &var_decls_builder);
                break;
            case PASCAL_T_VAR_SECTION:
                list_builder_extend(&var_decls_builder, convert_var_section(section));
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
                if (sub != NULL) {
                    ListNode_t *node = CreateListNode(sub, LIST_TREE);
                    *subprograms_tail = node;
                    subprograms_tail = &node->next;
                }
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

        Tree_t *tree = mk_program(cur->line, program_id, args, uses, const_decls,
                                  list_builder_finish(&var_decls_builder), type_decls, subprograms, body);
        return tree;
    }

    if (cur->typ == PASCAL_T_UNIT_DECL) {
        ast_t *unit_name_node = cur->child;
        char *unit_id = unit_name_node != NULL ? dup_symbol(unit_name_node) : strdup("unit");

        ListNode_t *interface_uses = NULL;
        ListNode_t *interface_const_decls = NULL;
        ListNode_t *interface_type_decls = NULL;
        ListBuilder interface_var_builder;
        list_builder_init(&interface_var_builder);
        ListNode_t *implementation_uses = NULL;
        ListNode_t *implementation_const_decls = NULL;
        ListNode_t *implementation_type_decls = NULL;
        ListBuilder implementation_var_builder;
        list_builder_init(&implementation_var_builder);
        ListNode_t *subprograms = NULL;
        ListNode_t **subprograms_tail = &subprograms;
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
                    case PASCAL_T_CONST_SECTION:
                        append_const_decls_from_section(node, &interface_const_decls,
                                                        &interface_var_builder);
                        break;
                    case PASCAL_T_TYPE_SECTION:
                        append_type_decls_from_section(node, &interface_type_decls);
                        break;
                    case PASCAL_T_VAR_SECTION:
                        list_builder_extend(&interface_var_builder, convert_var_section(node));
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
                    case PASCAL_T_CONST_SECTION:
                        append_const_decls_from_section(node, &implementation_const_decls,
                                                        &implementation_var_builder);
                        break;
                    case PASCAL_T_TYPE_SECTION:
                        append_type_decls_from_section(node, &implementation_type_decls);
                        break;
                    case PASCAL_T_VAR_SECTION:
                        list_builder_extend(&implementation_var_builder, convert_var_section(node));
                        break;
                    case PASCAL_T_PROCEDURE_DECL: {
                        Tree_t *proc = convert_procedure(node);
                        if (proc != NULL) {
                            ListNode_t *list_node = CreateListNode(proc, LIST_TREE);
                            *subprograms_tail = list_node;
                            subprograms_tail = &list_node->next;
                        }
                        break;
                    }
                    case PASCAL_T_FUNCTION_DECL: {
                        Tree_t *func = convert_function(node);
                        if (func != NULL) {
                            ListNode_t *list_node = CreateListNode(func, LIST_TREE);
                            *subprograms_tail = list_node;
                            subprograms_tail = &list_node->next;
                        }
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

        Tree_t *tree = mk_unit(cur->line, unit_id, interface_uses,
                               interface_const_decls, interface_type_decls,
                               list_builder_finish(&interface_var_builder), implementation_uses,
                               implementation_const_decls,
                               implementation_type_decls, list_builder_finish(&implementation_var_builder),
                               subprograms, initialization);
        return tree;
    }

    fprintf(stderr, "ERROR: Unsupported Pascal AST root type %d.\n", cur->typ);
    return NULL;
}
