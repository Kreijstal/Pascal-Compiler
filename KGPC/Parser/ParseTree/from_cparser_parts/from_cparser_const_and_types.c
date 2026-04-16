#include "../from_cparser_internal.h"




/* Collect type argument identifiers from a \"specialize\" type argument list.
 * The argument subtree is simpler than CONSTRUCTED_TYPE: it consists of identifiers
 * separated by punctuation inside a PASCAL_T_NONE wrapper created by specialize_args. */
static ListNode_t *collect_specialize_type_args(ast_t *args_node) {
    if (args_node == NULL)
        return NULL;

    ListBuilder builder;
    list_builder_init(&builder);

    ast_t *cursor = args_node;
    while (cursor != NULL) {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node == NULL)
            node = cursor;
        if (node != NULL && node->typ == PASCAL_T_IDENTIFIER) {
            char *dup = dup_symbol(node);
            if (dup != NULL)
                list_builder_append(&builder, dup, LIST_STRING);
        }
        cursor = cursor->next;
    }

    ListNode_t *result = list_builder_finish(&builder);
    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
        fprintf(stderr, "[KGPC] collect_constructed_type_args result=%p\n", (void *)result);
    return result;
}

/* Extract generic base name and type arguments from a \"specialize\" type
 * specification, such as:
 *   specialize TFPGList<TMyRecord>
 * The input node may be either the PASCAL_T_TYPE_SPEC wrapper or its child. */
int extract_specialize_type_info(ast_t *spec_node, char **base_name_out, ListNode_t **type_args_out) {
    if (base_name_out != NULL)
        *base_name_out = NULL;
    if (type_args_out != NULL)
        *type_args_out = NULL;

    if (spec_node == NULL)
        return 0;

    ast_t *node = spec_node;
    if (node->typ == PASCAL_T_TYPE_SPEC && node->child != NULL)
        node = node->child;
    node = unwrap_pascal_node(node);
    if (node == NULL)
        return 0;

    /* Expect first child to be the "specialize" keyword */
    if (node->sym == NULL || node->sym->name == NULL ||
        strcasecmp(node->sym->name, "specialize") != 0)
        return 0;

    /* Next sibling should be the generic type identifier (possibly qualified) */
    ast_t *base_node = node->next;
    while (base_node != NULL && base_node->typ == PASCAL_T_NONE)
        base_node = base_node->child;
    if (base_node == NULL)
        return 0;

    char *base_name = dup_symbol(base_node);
    if (base_name == NULL)
        return 0;

    /* Optional argument list node (created by specialize_args) */
    ListNode_t *type_args = NULL;
    ast_t *args_node = base_node->next;
    if (args_node != NULL) {
        type_args = collect_specialize_type_args(args_node);
        if (type_args == NULL) {
            free(base_name);
            return 0;
        }
    }

    if (base_name_out != NULL)
        *base_name_out = base_name;
    else
        free(base_name);

    if (type_args_out != NULL)
        *type_args_out = type_args;
    else
        destroy_list(type_args);

    return 1;
}

static struct Expression *make_varid_from_qualified_ast(ast_t *node)
{
    if (node == NULL)
        return NULL;
    QualifiedIdent *qid = qualified_ident_from_ast(node);
    if (qid == NULL)
        return NULL;
    char *joined = qualified_ident_join(qid, ".");
    struct Expression *expr = mk_varid(node->line, joined);
    if (expr != NULL)
    {
        if (expr->id_ref != NULL)
            qualified_ident_free(expr->id_ref);
        expr->id_ref = qid;
        qid = NULL;
    }
    if (qid != NULL)
        qualified_ident_free(qid);
    return expr;
}

static struct Expression *convert_case_label_expression(ast_t *node) {
    ast_t *unwrapped = unwrap_pascal_node(node);
    if (unwrapped != NULL &&
        (unwrapped->typ == PASCAL_T_QUALIFIED_IDENTIFIER ||
         unwrapped->typ == PASCAL_T_MEMBER_ACCESS ||
         unwrapped->typ == PASCAL_T_IDENTIFIER))
    {
        struct Expression *qualified = make_varid_from_qualified_ast(unwrapped);
        if (qualified != NULL)
            return qualified;
    }

    struct Expression *expr = convert_expression(node);
    if (expr != NULL && expr->type == EXPR_STRING && expr->expr_data.string != NULL) {
        const char *value = expr->expr_data.string;
        if (value[0] != '\0' && value[1] == '\0') {
            unsigned int code = (unsigned char)value[0];
            int line = (node != NULL) ? node->line : 0;
            destroy_expr(expr);
            return mk_charcode(line, code);
        }
    }
    return expr;
}

void append_case_label(ListBuilder *builder, ast_t *label_node) {
    if (builder == NULL || label_node == NULL)
        return;

    ast_t *unwrapped = unwrap_pascal_node(label_node);
    if (unwrapped == NULL)
        return;

    if (unwrapped->typ == PASCAL_T_CASE_LABEL) {
        append_case_label(builder, unwrapped->child);
        return;
    }

    if (unwrapped->typ == PASCAL_T_RANGE) {
        ast_t *lower_node = unwrap_pascal_node(unwrapped->child);
        ast_t *upper_node = NULL;
        if (lower_node != NULL)
            upper_node = unwrap_pascal_node(lower_node->next);

        struct Expression *lower_expr = convert_case_label_expression(lower_node);
        struct Expression *upper_expr = convert_case_label_expression(upper_node);

        if (lower_expr != NULL && upper_expr != NULL) {
            struct SetElement *range = mk_set_element(lower_expr, upper_expr);
            list_builder_append(builder, range, LIST_SET_ELEMENT);
        } else {
            if (lower_expr != NULL)
                destroy_expr(lower_expr);
            if (upper_expr != NULL)
                destroy_expr(upper_expr);
        }
        return;
    }

    struct Expression *label_expr = convert_case_label_expression(unwrapped);
    if (label_expr != NULL)
        list_builder_append(builder, label_expr, LIST_EXPR);
}

void extend_list(ListNode_t **dest, ListNode_t *src) {
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

ast_t *unwrap_pascal_node(ast_t *node) {
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

/* Under {$H-}, bare 'string' mapped to STRING_TYPE should become
 * SHORTSTRING_TYPE.  Only remap when the name is literally "string"
 * (case-insensitive) so that explicit AnsiString/UnicodeString are
 * left untouched. */
int apply_shortstring_mode(int type_tag, const char *name) {
    if (type_tag == STRING_TYPE && pascal_frontend_default_shortstring() &&
        name != NULL && strcasecmp(name, "string") == 0)
        return SHORTSTRING_TYPE;
    return type_tag;
}

int map_type_name(const char *name, char **type_id_out) {
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
    if (strcasecmp(name, "int64") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("int64");
        return INT64_TYPE;
    }
    if (strcasecmp(name, "int32") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("int32");
        return LONGINT_TYPE;
    }
    if (strcasecmp(name, "int16") == 0 || strcasecmp(name, "int8") == 0 ||
        strcasecmp(name, "shortint") == 0 || strcasecmp(name, "smallint") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return INT_TYPE;
    }
    if (strcasecmp(name, "byte") == 0 || strcasecmp(name, "uint8") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return BYTE_TYPE;
    }
    if (strcasecmp(name, "word") == 0 || strcasecmp(name, "uint16") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return WORD_TYPE;
    }
    if (strcasecmp(name, "longword") == 0 || strcasecmp(name, "dword") == 0 ||
        strcasecmp(name, "cardinal") == 0 || strcasecmp(name, "uint32") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return LONGWORD_TYPE;
    }
    if (strcasecmp(name, "qword") == 0 || strcasecmp(name, "uint64") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return QWORD_TYPE;
    }
    if (strcasecmp(name, "nativeint") == 0 ||
        strcasecmp(name, "sizeint") == 0 ||
        strcasecmp(name, "ptrint") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return INT64_TYPE;
    }
    if (strcasecmp(name, "nativeuint") == 0 ||
        strcasecmp(name, "sizeuint") == 0 ||
        strcasecmp(name, "ptruint") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return INT64_TYPE;
    }
    if (strcasecmp(name, "real") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("real");
        return REAL_TYPE;
    }
    if (strcasecmp(name, "float") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("Float");
        return REAL_TYPE;
    }
    if (strcasecmp(name, "double") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("double");
        return REAL_TYPE;
    }
    if (strcasecmp(name, "extended") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("Extended");
        return EXTENDED_TYPE;
    }
    if (strcasecmp(name, "valreal") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("ValReal");
        return REAL_TYPE;
    }
    if (strcasecmp(name, "openstring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("shortstring");
        return SHORTSTRING_TYPE;
    }
    if (strcasecmp(name, "string") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("string");
        return STRING_TYPE;
    }
    if (strcasecmp(name, "ansistring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("AnsiString");
        return STRING_TYPE;
    }
    if (strcasecmp(name, "widestring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("WideString");
        return STRING_TYPE;
    }
    if (strcasecmp(name, "shortstring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("shortstring");
        return SHORTSTRING_TYPE;
    }
    /* RawByteString and UnicodeString need to preserve their original names
     * for correct name mangling of overloaded procedures */
    if (strcasecmp(name, "rawbytestring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("RawByteString");
        return STRING_TYPE;
    }
    if (strcasecmp(name, "unicodestring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("UnicodeString");
        return STRING_TYPE;
    }
    if (strcasecmp(name, "text") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("text");
        return TEXT_TYPE;
    }
    /* C-style char aliases from ctypes/sysutils */
    if (strcasecmp(name, "cchar") == 0 || strcasecmp(name, "cschar") == 0 ||
        strcasecmp(name, "cuchar") == 0 || strcasecmp(name, "ansichar") == 0)
    {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return CHAR_TYPE;
    }
    if (strcasecmp(name, "char") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("char");
        return CHAR_TYPE;
    }
    /* WideChar is a 2-byte character type; resolve via symbol table for correct size. */
    if (strcasecmp(name, "widechar") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("widechar");
        return UNKNOWN_TYPE;
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
    /* Procedure and Function as bare type names (no parameters) - procedure pointers */
    if (strcasecmp(name, "procedure") == 0 || strcasecmp(name, "function") == 0) {
        /* Don't set type_id_out - PROCEDURE type tag is sufficient */
        return PROCEDURE;
    }
    if (type_id_out != NULL) {
        *type_id_out = strdup(name);
    }
    return UNKNOWN_TYPE;
}

int helper_self_param_is_var(const char *base_type_id, struct SymTab *symtab)
{
    if (base_type_id == NULL)
        return 0;
    /* Real/Single/Double/Extended: helper Self is passed by value. */
    int type_tag = map_type_name(base_type_id, NULL);
    if (is_real_family_type(type_tag))
        return 0;
    /* String types are heap-allocated pointers — by value is correct. */
    if (type_tag == STRING_TYPE || type_tag == SHORTSTRING_TYPE ||
        type_tag == CHAR_TYPE)
        return 0;
    /* Class and pointer types: Self is already a pointer. */
    if (symtab != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, base_type_id) != 0 && type_node != NULL)
        {
            if (type_node->type != NULL && type_node->type->kind == TYPE_KIND_RECORD)
            {
                struct RecordType *rec = type_node->type->info.record_info;
                if (rec != NULL && rec->is_class)
                    return 0;
            }
            if (type_node->type != NULL && type_node->type->kind == TYPE_KIND_POINTER)
                return 0;
        }
    }
    /* Integer/ordinal value types: Self must be passed by reference
     * so that mutations (Self := Self or ...) persist at the call site. */
    return 1;
}

struct TypeAlias *helper_self_real_alias(const char *base_type_id)
{
    if (base_type_id == NULL)
        return NULL;

    int type_tag = map_type_name(base_type_id, NULL);
    if (!is_real_family_type(type_tag))
        return NULL;

    struct TypeAlias *alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
    if (alias == NULL)
        return NULL;

    alias->base_type = type_tag;
    if (pascal_identifier_equals(base_type_id, "Single"))
        alias->storage_size = 4;
    else if (pascal_identifier_equals(base_type_id, "Extended"))
        alias->storage_size = 10;
    else
        alias->storage_size = 8;

    return alias;
}


ast_t *unwrap_record_constructor_elem(ast_t *elem)
{
    ast_t *unwrapped = unwrap_pascal_node(elem);
    if (unwrapped != NULL && unwrapped->typ == PASCAL_T_STATEMENT &&
        unwrapped->child != NULL)
        unwrapped = unwrap_pascal_node(unwrapped->child);
    return unwrapped;
}

int tuple_is_record_constructor(ast_t *tuple_node)
{
    if (tuple_node == NULL || tuple_node->typ != PASCAL_T_TUPLE)
        return 0;

    int has_fields = 0;
    int has_record_like = 0;
    int debug_tuple = (kgpc_getenv("KGPC_DEBUG_RECORD_TUPLE") != NULL);
    int debug_this = debug_tuple && tuple_node->line >= 2165 && tuple_node->line <= 2175;
    if (debug_this)
    {
        fprintf(stderr, "[KGPC] tuple_is_record_constructor line=%d\n", tuple_node->line);
    }
    for (ast_t *elem = tuple_node->child; elem != NULL; elem = elem->next)
    {
        ast_t *unwrapped = unwrap_record_constructor_elem(elem);
        if (unwrapped == NULL)
            continue;
        if (unwrapped->typ == PASCAL_T_NONE && unwrapped->child == NULL)
            continue;
        if (debug_this)
        {
            fprintf(stderr, "  elem typ=%d\n", unwrapped->typ);
        }
        if (unwrapped->typ == PASCAL_T_ASSIGNMENT || unwrapped->typ == PASCAL_T_FIELD_WIDTH)
        {
            has_fields = 1;
            has_record_like = 1;
            continue;
        }
        if (unwrapped->typ == PASCAL_T_ASSIGNMENT || unwrapped->typ == PASCAL_T_FIELD_WIDTH)
            has_record_like = 1;
        return 0;
    }
    if (debug_this)
        fprintf(stderr, "  has_fields=%d\n", has_fields);
    if (debug_tuple && !has_fields && has_record_like)
    {
        fprintf(stderr, "[KGPC] tuple skipped record constructor at line=%d\n", tuple_node->line);
    }
    return has_fields;
}

struct Expression *convert_record_constructor_expr(ast_t *expr_node)
{
    if (expr_node == NULL)
        return NULL;

    ListNode_t *fields = NULL;
    ListNode_t *fields_tail = NULL;
    int field_count = 0;

    for (ast_t *field_assignment = expr_node->child;
         field_assignment != NULL;
         field_assignment = field_assignment->next)
    {
        ast_t *assignment_node = unwrap_record_constructor_elem(field_assignment);
        if (assignment_node == NULL)
            continue;

        ast_t *field_name_node = NULL;
        ast_t *field_value_node = NULL;

        if (assignment_node->typ == PASCAL_T_ASSIGNMENT)
        {
            field_name_node = assignment_node->child;
            field_value_node = (field_name_node != NULL) ? field_name_node->next : NULL;
        }
        else if (assignment_node->typ == PASCAL_T_FIELD_WIDTH)
        {
            field_name_node = assignment_node->child;
            field_value_node = (field_name_node != NULL) ? field_name_node->next : NULL;
        }
        else
        {
            continue;
        }
        if (field_name_node == NULL || field_value_node == NULL ||
            field_name_node->sym == NULL || field_name_node->sym->name == NULL)
        {
            fprintf(stderr, "ERROR: Malformed record constructor field at line %d.\n",
                expr_node->line);
            goto record_ctor_cleanup;
        }

        struct Expression *field_value = NULL;
        if (field_value_node != NULL && field_value_node->typ == PASCAL_T_TUPLE &&
            tuple_is_record_constructor(field_value_node))
        {
            field_value = convert_record_constructor_expr(field_value_node);
        }
        else
        {
            field_value = convert_expression(field_value_node);
        }
        if (field_value == NULL)
        {
            fprintf(stderr, "ERROR: Failed to convert record constructor field value at line %d.\n",
                expr_node->line);
            goto record_ctor_cleanup;
        }

        struct RecordConstructorField *field =
            (struct RecordConstructorField *)calloc(1, sizeof(struct RecordConstructorField));
        if (field == NULL)
            goto record_ctor_cleanup;
        field->field_id = strdup(field_name_node->sym->name);
        field->value = field_value;

        ListNode_t *node = CreateListNode(field, LIST_UNSPECIFIED);
        if (node == NULL)
            goto record_ctor_cleanup;
        if (fields == NULL)
        {
            fields = node;
            fields_tail = node;
        }
        else
        {
            fields_tail->next = node;
            fields_tail = node;
        }
        ++field_count;
    }

    return mk_record_constructor(expr_node->line, fields, field_count);

record_ctor_cleanup:
    if (fields != NULL)
    {
        ListNode_t *cur = fields;
        while (cur != NULL)
        {
            struct RecordConstructorField *field = (struct RecordConstructorField *)cur->cur;
            if (field != NULL)
            {
                if (field->value != NULL)
                    destroy_expr(field->value);
                free(field->field_id);
                free(field->field_type_id);
                free(field->array_element_type_id);
                free(field);
            }
            ListNode_t *next = cur->next;
            free(cur);
            cur = next;
        }
    }
    return NULL;
}

/* Helper function to resolve enum literal identifier to its ordinal value
 * by searching through AST type section.
 * Returns the ordinal value if found (>= 0), -1 if not found.
 */
int resolve_enum_ordinal_from_ast(const char *identifier, ast_t *type_section) {
    if (identifier == NULL || type_section == NULL)
        return -1;
    
    /* Iterate through type declarations */
    ast_t *type_decl = type_section->child;
    while (type_decl != NULL) {
        if (type_decl->typ == PASCAL_T_TYPE_DECL) {
            /* Find the type spec within the type declaration */
            ast_t *type_spec_node = type_decl->child;
            if (type_spec_node != NULL)
                type_spec_node = type_spec_node->next; /* Skip the identifier */
            
            while (type_spec_node != NULL && 
                   type_spec_node->typ != PASCAL_T_TYPE_SPEC &&
                   type_spec_node->typ != PASCAL_T_ENUMERATED_TYPE) {
                type_spec_node = type_spec_node->next;
            }
            
            /* Unwrap TYPE_SPEC if needed */
            ast_t *spec = type_spec_node;
            if (spec != NULL && spec->typ == PASCAL_T_TYPE_SPEC && spec->child != NULL)
                spec = spec->child;
            
            /* Check if it's an enumerated type */
            if (spec != NULL && spec->typ == PASCAL_T_ENUMERATED_TYPE) {
                int ordinal = 0;
                ast_t *literal = spec->child;
                while (literal != NULL) {
                    ast_t *literal_id = literal;
                    if (literal_id != NULL && literal_id->typ == PASCAL_T_ASSIGNMENT)
                        literal_id = literal_id->child;
                    if (literal_id != NULL && literal_id->typ == PASCAL_T_IDENTIFIER &&
                        literal_id->sym != NULL) {
                        if (strcmp(literal_id->sym->name, identifier) == 0) {
                            return ordinal; /* Found it! Return the ordinal value */
                        }
                    }
                    ordinal++;
                    literal = literal->next;
                }
            }
        }
        type_decl = type_decl->next;
    }
    
    return -1; /* Not found */
}

/* Helper to resolve an enum literal within a specific enumerated type.
 * Returns ordinal value if found, -1 otherwise. */
int resolve_enum_literal_in_type(const char *type_name, const char *literal, ast_t *type_section) {
    if (type_name == NULL || literal == NULL || type_section == NULL)
        return -1;

    ast_t *type_decl = type_section->child;
    while (type_decl != NULL) {
        if (type_decl->typ == PASCAL_T_TYPE_DECL) {
            ast_t *id_node = type_decl->child;
            if (id_node != NULL && id_node->typ == PASCAL_T_IDENTIFIER &&
                id_node->sym != NULL && id_node->sym->name != NULL &&
                strcasecmp(id_node->sym->name, type_name) == 0) {
                ast_t *type_spec_node = id_node->next;
                while (type_spec_node != NULL &&
                       type_spec_node->typ != PASCAL_T_TYPE_SPEC &&
                       type_spec_node->typ != PASCAL_T_ENUMERATED_TYPE) {
                    type_spec_node = type_spec_node->next;
                }
                ast_t *spec = type_spec_node;
                if (spec != NULL && spec->typ == PASCAL_T_TYPE_SPEC && spec->child != NULL)
                    spec = spec->child;
                if (spec != NULL && spec->typ == PASCAL_T_ENUMERATED_TYPE) {
                    int ordinal = 0;
                    for (ast_t *lit = spec->child; lit != NULL; lit = lit->next) {
                        ast_t *lit_id = lit;
                        if (lit_id != NULL && lit_id->typ == PASCAL_T_ASSIGNMENT)
                            lit_id = lit_id->child;
                        if (lit_id != NULL && lit_id->typ == PASCAL_T_IDENTIFIER &&
                            lit_id->sym != NULL &&
                            lit_id->sym->name != NULL &&
                            strcasecmp(lit_id->sym->name, literal) == 0) {
                            return ordinal;
                        }
                        ordinal++;
                    }
                }
                return -1;
            }
        }
        type_decl = type_decl->next;
    }

    return -1;
}

/* Helper function to resolve the range of an enumerated type by type name.
 * For example, if color = (red, blue, yellow), then resolve_enum_type_range_from_ast("color", ...)
 * will set out_start=0 and out_end=2 (for 3 values: red, blue, yellow).
 * Returns 0 on success, -1 if the type is not found or is not an enum type.
 */
int resolve_enum_type_range_from_ast(const char *type_name, ast_t *type_section, int *out_start, int *out_end) {
    if (type_name == NULL || type_section == NULL || out_start == NULL || out_end == NULL)
        return -1;
    
    /* Iterate through type declarations to find the matching type name */
    ast_t *type_decl = type_section->child;
    while (type_decl != NULL) {
        if (type_decl->typ == PASCAL_T_TYPE_DECL) {
            /* Get the type name (first child) */
            ast_t *type_id = type_decl->child;
            if (type_id != NULL && type_id->typ == PASCAL_T_IDENTIFIER && type_id->sym != NULL) {
                if (strcasecmp(type_id->sym->name, type_name) == 0) {
                    /* Found the type - now check if it's an enumerated type */
                    ast_t *type_spec_node = type_id->next;
                    
                    while (type_spec_node != NULL &&
                           type_spec_node->typ != PASCAL_T_TYPE_SPEC &&
                           type_spec_node->typ != PASCAL_T_ENUMERATED_TYPE &&
                           type_spec_node->typ != PASCAL_T_RANGE_TYPE) {
                        type_spec_node = type_spec_node->next;
                    }

                    /* Unwrap TYPE_SPEC if needed */
                    ast_t *spec = type_spec_node;
                    if (spec != NULL && spec->typ == PASCAL_T_TYPE_SPEC && spec->child != NULL)
                        spec = spec->child;

                    /* Check if it's an enumerated type */
                    if (spec != NULL && spec->typ == PASCAL_T_ENUMERATED_TYPE) {
                        /* Count the enum values */
                        int count = 0;
                        ast_t *literal = spec->child;
                        while (literal != NULL) {
                            if (literal->typ == PASCAL_T_IDENTIFIER ||
                                literal->typ == PASCAL_T_ASSIGNMENT)
                                count++;
                            literal = literal->next;
                        }

                        if (count > 0) {
                            *out_start = 0;
                            *out_end = count - 1;
                            enum_registry_add(type_name, 0, count - 1);
                            return 0; /* Success */
                        }
                    }

                    /* Check if it's a subrange type (e.g., 0..NUM_REGS - 1) */
                    if (spec != NULL && spec->typ == PASCAL_T_RANGE_TYPE) {
                        ast_t *lower = spec->child;
                        ast_t *upper = (lower != NULL) ? lower->next : NULL;
                        int low_val = 0, high_val = 0;
                        if (evaluate_const_int_expr(lower, &low_val, 0) == 0 &&
                            evaluate_const_int_expr(upper, &high_val, 0) == 0) {
                            *out_start = low_val;
                            *out_end = high_val;
                            enum_registry_add(type_name, low_val, high_val);
                            return 0; /* Success */
                        }
                    }

                    /* Found the type but it's not an enum or resolved subrange */
                    return -1;
                }
            }
        }
        type_decl = type_decl->next;
    }
    
    return -1; /* Type not found */
}

/* Helper function to resolve a const integer identifier from the same const section.
 * This is needed for array ranges like [C_Low .. C_High].
 * Returns the integer value if found, otherwise returns fallback_value.
 * Note: Only resolves simple integer consts; does not handle forward references
 * or complex const expressions.
 */
static int resolve_const_int_from_ast_internal(const char *identifier, ast_t *const_section,
                                               int fallback_value, int depth);
static int resolve_const_int_in_node(const char *identifier, ast_t *node,
                                     ast_t *const_section, int *out_value, int depth) {
    if (node == NULL)
        return -1;
    if (depth > 32)
        return -1;

    if (node->typ == PASCAL_T_CONST_DECL) {
        ast_t *id_node = node->child;
        if (id_node != NULL && id_node->sym != NULL) {
            if (strcasecmp(id_node->sym->name, identifier) == 0) {
                ast_t *value_node = id_node->next;
                if (value_node != NULL && value_node->typ == PASCAL_T_TYPE_SPEC)
                    value_node = value_node->next;
                value_node = unwrap_pascal_node(value_node);

                if (value_node != NULL && value_node->sym != NULL) {
                    char *endptr;
                    long val = strtol(value_node->sym->name, &endptr, 10);
                    if (*endptr == '\0') {
                        if (val >= INT_MIN && val <= INT_MAX) {
                            *out_value = (int)val;
                            return 0;
                        }
                    }

                    if (value_node->typ == PASCAL_T_IDENTIFIER &&
                        strcasecmp(value_node->sym->name, identifier) != 0) {
                        int resolved = resolve_const_int_from_ast_internal(value_node->sym->name,
                                                                           const_section,
                                                                           INT_MIN,
                                                                           depth + 1);
                        if (resolved != INT_MIN) {
                            *out_value = resolved;
                            return 0;
                        }
                    }
                } else if (value_node != NULL && value_node->typ == PASCAL_T_NEG &&
                           value_node->child != NULL) {
                    ast_t *inner = unwrap_pascal_node(value_node->child);
                    if (inner != NULL && inner->sym != NULL) {
                        int resolved = resolve_const_int_from_ast_internal(inner->sym->name,
                                                                           const_section,
                                                                           INT_MIN,
                                                                           depth + 1);
                        if (resolved != INT_MIN) {
                            *out_value = -resolved;
                            return 0;
                        }
                    }
                }
            }
        }
    }

    if (resolve_const_int_in_node(identifier, node->child, const_section, out_value, depth) == 0)
        return 0;
    if (resolve_const_int_in_node(identifier, node->next, const_section, out_value, depth) == 0)
        return 0;
    return -1;
}

static int resolve_const_string_in_node(const char *identifier, ast_t *node,
                                        ast_t *const_section, const char **out_value, int depth);

/* AstStringValue typedef moved to from_cparser_internal.h */

void ast_string_value_reset(AstStringValue *value)
{
    if (value == NULL)
        return;
    free(value->data);
    value->data = NULL;
    value->len = 0;
}

static int ast_string_value_assign_bytes(AstStringValue *value, const char *data, size_t len)
{
    char *copy = NULL;
    if (value == NULL)
        return -1;
    if (len > 0)
    {
        copy = (char *)malloc(len);
        if (copy == NULL)
            return -1;
        memcpy(copy, data, len);
    }
    ast_string_value_reset(value);
    value->data = copy;
    value->len = len;
    return 0;
}

static int ast_string_value_append(AstStringValue *dst, const AstStringValue *src)
{
    char *combined = NULL;
    if (dst == NULL || src == NULL)
        return -1;
    if (src->len == 0)
        return 0;
    combined = (char *)malloc(dst->len + src->len);
    if (combined == NULL)
        return -1;
    if (dst->len > 0 && dst->data != NULL)
        memcpy(combined, dst->data, dst->len);
    memcpy(combined + dst->len, src->data, src->len);
    free(dst->data);
    dst->data = combined;
    dst->len += src->len;
    return 0;
}

static int parse_ast_char_code(ast_t *node, unsigned int *out_value)
{
    const char *literal;
    const char *digits;
    int base = 10;

    if (node == NULL || out_value == NULL)
        return -1;

    node = unwrap_pascal_node(node);
    if (node == NULL || node->typ != PASCAL_T_CHAR_CODE)
        return -1;

    literal = (node->sym != NULL) ? node->sym->name : NULL;
    if (literal == NULL)
        return -1;

    digits = literal;
    if (*digits == '#')
        ++digits;
    if (*digits == '$')
    {
        base = 16;
        ++digits;
    }
    if (*digits == '\0')
        return -1;

    {
        char *endptr = NULL;
        long parsed = strtol(digits, &endptr, base);
        if (endptr == NULL || *endptr != '\0' || parsed < 0)
            return -1;
        *out_value = (unsigned int)parsed;
    }
    return 0;
}

int evaluate_const_string_ast(ast_t *node, ast_t *const_section,
                                     AstStringValue *out_value, int depth)
{
    ast_t *unwrapped;
    if (node == NULL || out_value == NULL || depth > 32)
        return -1;

    unwrapped = unwrap_pascal_node(node);
    if (unwrapped == NULL)
        return -1;

    switch (unwrapped->typ)
    {
        case PASCAL_T_STRING:
        case PASCAL_T_CHAR:
        {
            const char *value = (unwrapped->sym != NULL) ? unwrapped->sym->name : NULL;
            if (value == NULL)
                return -1;
            return ast_string_value_assign_bytes(out_value, value, strlen(value));
        }

        case PASCAL_T_CHAR_CODE:
        {
            unsigned int ch = 0;
            unsigned char byte;
            if (parse_ast_char_code(unwrapped, &ch) != 0)
                return -1;
            byte = (unsigned char)(ch & 0xffu);
            return ast_string_value_assign_bytes(out_value, (const char *)&byte, 1);
        }

        case PASCAL_T_IDENTIFIER:
        {
            const char *resolved = NULL;
            if (unwrapped->sym != NULL && unwrapped->sym->name != NULL &&
                resolve_const_string_from_ast_internal(unwrapped->sym->name, const_section,
                                                       &resolved, depth + 1) == 0 &&
                resolved != NULL)
            {
                return ast_string_value_assign_bytes(out_value, resolved, strlen(resolved));
            }
            return -1;
        }

        case PASCAL_T_ADD:
        {
            AstStringValue left = {0};
            AstStringValue right = {0};
            ast_t *lhs = unwrapped->child;
            ast_t *rhs = (lhs != NULL) ? lhs->next : NULL;
            int result = -1;

            if (evaluate_const_string_ast(lhs, const_section, &left, depth + 1) != 0)
                goto concat_cleanup;
            if (evaluate_const_string_ast(rhs, const_section, &right, depth + 1) != 0)
                goto concat_cleanup;
            if (ast_string_value_assign_bytes(out_value, left.data, left.len) != 0)
                goto concat_cleanup;
            if (ast_string_value_append(out_value, &right) != 0)
            {
                ast_string_value_reset(out_value);
                goto concat_cleanup;
            }
            result = 0;

concat_cleanup:
            ast_string_value_reset(&left);
            ast_string_value_reset(&right);
            return result;
        }

        default:
            return -1;
    }
}

int type_info_targets_char_array(const TypeInfo *type_info, int *is_widechar_out)
{
    int is_char_array_target = 0;
    int is_widechar_array_target = 0;

    if (is_widechar_out != NULL)
        *is_widechar_out = 0;
    if (type_info == NULL)
        return 0;

    is_char_array_target = (type_info->element_type == CHAR_TYPE);
    is_widechar_array_target = (type_info->element_type_id != NULL &&
                                strcasecmp(type_info->element_type_id, "widechar") == 0);
    if (!is_char_array_target && type_info->element_type_id != NULL &&
        (strcasecmp(type_info->element_type_id, "char") == 0 ||
         strcasecmp(type_info->element_type_id, "ansichar") == 0))
    {
        is_char_array_target = 1;
    }

    if (is_widechar_out != NULL)
        *is_widechar_out = is_widechar_array_target;
    return is_char_array_target || is_widechar_array_target;
}

struct Expression *mk_const_array_element_lhs(int line_num, const char *array_name,
    int outer_index, int inner_index, int is_multidim)
{
    struct Expression *base_expr = mk_varid(line_num, strdup(array_name));
    struct Expression *outer_index_expr = mk_inum(line_num, outer_index);
    struct Expression *inner_index_expr = mk_inum(line_num, inner_index);

    if (is_multidim)
    {
        struct Expression *lhs = mk_arrayaccess(line_num, base_expr, outer_index_expr);
        lhs->expr_data.array_access_data.extra_indices =
            CreateListNode(inner_index_expr, LIST_EXPR);
        return lhs;
    }

    {
        struct Expression *outer_access = mk_arrayaccess(line_num, base_expr, outer_index_expr);
        return mk_arrayaccess(line_num, outer_access, inner_index_expr);
    }
}

static int resolve_const_string_in_section(const char *identifier, ast_t *const_section,
                                           const char **out_value, int depth) {
    if (identifier == NULL || const_section == NULL || out_value == NULL)
        return -1;
    return resolve_const_string_in_node(identifier, const_section->child, const_section, out_value, depth);
}

int resolve_const_string_from_ast_internal(const char *identifier, ast_t *const_section,
                                                  const char **out_value, int depth) {
    if (identifier == NULL || const_section == NULL || out_value == NULL)
        return -1;
    if (depth > 32)
        return -1;

    int found = 0;
    if (g_const_sections != NULL) {
        for (ListNode_t *cur = g_const_sections; cur != NULL; cur = cur->next) {
            if (resolve_const_string_in_section(identifier, (ast_t *)cur->cur, out_value, depth) == 0) {
                found = 1;
                break;
            }
        }
    } else {
        if (resolve_const_string_in_section(identifier, const_section, out_value, depth) == 0)
            found = 1;
    }

    return found ? 0 : -1;
}

static int resolve_const_string_in_node(const char *identifier, ast_t *node,
                                        ast_t *const_section, const char **out_value, int depth) {
    if (node == NULL)
        return -1;
    if (depth > 32)
        return -1;

    node = unwrap_pascal_node(node);
    if (node == NULL)
        return -1;

    if (node->typ == PASCAL_T_CONST_DECL) {
        ast_t *id_node = node->child;
        if (id_node != NULL && id_node->sym != NULL) {
            if (strcasecmp(id_node->sym->name, identifier) == 0) {
                ast_t *value_node = id_node->next;
                if (value_node != NULL && value_node->typ == PASCAL_T_TYPE_SPEC)
                    value_node = value_node->next;
                value_node = unwrap_pascal_node(value_node);

                if (value_node != NULL && value_node->typ == PASCAL_T_STRING &&
                    value_node->sym != NULL && value_node->sym->name != NULL) {
                    *out_value = value_node->sym->name;
                    return 0;
                }

                if (value_node != NULL && value_node->typ == PASCAL_T_IDENTIFIER &&
                    value_node->sym != NULL &&
                    strcasecmp(value_node->sym->name, identifier) != 0) {
                    if (resolve_const_string_from_ast_internal(value_node->sym->name, const_section,
                                                               out_value, depth + 1) == 0)
                        return 0;
                }
            }
        }
    }

    if (resolve_const_string_in_node(identifier, node->child, const_section, out_value, depth) == 0)
        return 0;
    if (resolve_const_string_in_node(identifier, node->next, const_section, out_value, depth) == 0)
        return 0;
    return -1;
}

static int resolve_enum_type_range_in_section_chain(const char *type_name, ast_t *section_root,
                                                    int *out_start, int *out_end) {
    if (type_name == NULL || section_root == NULL || out_start == NULL || out_end == NULL)
        return -1;

    ast_t *section = section_root->child;
    while (section != NULL) {
        ast_t *node = unwrap_pascal_node(section);
        for (ast_t *cursor = node; cursor != NULL;
             cursor = (section->typ == PASCAL_T_NONE) ? cursor->next : NULL) {
            if (cursor->typ == PASCAL_T_TYPE_SECTION) {
                if (resolve_enum_type_range_from_ast(type_name, cursor, out_start, out_end) == 0)
                    return 0;
            }
        }
        section = section->next;
    }

    return -1;
}

int resolve_enum_type_range_with_fallback(const char *type_name, ast_t *type_section,
                                                 int *out_start, int *out_end) {
    if (resolve_enum_type_range_from_ast(type_name, type_section, out_start, out_end) == 0)
        return 0;
    if (resolve_enum_type_range_in_section_chain(type_name, g_interface_section_ast, out_start, out_end) == 0)
        return 0;
    if (resolve_enum_type_range_in_section_chain(type_name, g_implementation_section_ast, out_start, out_end) == 0)
        return 0;
    /* Fallback: check the cross-unit enum registry */
    if (enum_registry_lookup(type_name, out_start, out_end) == 0)
        return 0;
    return -1;
}

ast_t *find_type_decl_in_section(ast_t *type_section, const char *type_name) {
    if (type_section == NULL || type_name == NULL)
        return NULL;

    for (ast_t *type_decl = type_section->child; type_decl != NULL; type_decl = type_decl->next) {
        if (type_decl->typ != PASCAL_T_TYPE_DECL)
            continue;
        ast_t *id_node = type_decl->child;
        if (id_node != NULL && id_node->typ == PASCAL_T_IDENTIFIER && id_node->sym != NULL &&
            id_node->sym->name != NULL && strcasecmp(id_node->sym->name, type_name) == 0) {
            return type_decl;
        }
    }
    return NULL;
}

int type_name_is_class_like(const char *type_name) {
    if (type_name == NULL)
        return 0;

    /* When called during generic method instantiation the raw parser AST
     * may already have been freed.  Use the converted RecordType instead. */
    if (g_instantiate_record != NULL) {
        if (g_instantiate_record->type_id != NULL &&
            strcasecmp(g_instantiate_record->type_id, type_name) == 0)
            return g_instantiate_record->is_class;
    }

    ast_t *type_decl = find_type_decl_in_section(g_interface_type_section_ast, type_name);
    if (type_decl == NULL)
        type_decl = find_type_decl_in_section(g_implementation_type_section_ast, type_name);
    if (type_decl == NULL)
        return 0;

    ast_t *spec_node = type_decl->child;
    while (spec_node != NULL &&
           spec_node->typ != PASCAL_T_TYPE_SPEC &&
           spec_node->typ != PASCAL_T_CLASS_TYPE &&
           spec_node->typ != PASCAL_T_INTERFACE_TYPE)
    {
        spec_node = spec_node->next;
    }

    if (spec_node == NULL)
        return 0;

    if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
        spec_node = spec_node->child;

    return (spec_node->typ == PASCAL_T_CLASS_TYPE ||
            spec_node->typ == PASCAL_T_INTERFACE_TYPE);
}

int resolve_array_type_info_from_ast(const char *type_name, ast_t *type_section, TypeInfo *out_info, int depth) {
    if (type_name == NULL || type_section == NULL || out_info == NULL)
        return -1;
    if (depth > 16)
        return -1;

    ast_t *type_decl = find_type_decl_in_section(type_section, type_name);
    if (type_decl == NULL)
        return -1;

    ast_t *spec_node = type_decl->child;
    while (spec_node != NULL && spec_node->typ != PASCAL_T_TYPE_SPEC)
        spec_node = spec_node->next;
    if (spec_node == NULL)
        return -1;

    TypeInfo tmp_info = {0};
    char *tmp_id = NULL;
    struct RecordType *tmp_record = NULL;
    int mapped = convert_type_spec(spec_node, &tmp_id, &tmp_record, &tmp_info);
    if (tmp_record != NULL)
        destroy_record_type(tmp_record);

    if (tmp_info.is_array) {
        *out_info = tmp_info;
        if (tmp_id != NULL)
            free(tmp_id);
        return 0;
    }

    destroy_type_info_contents(&tmp_info);
    if (tmp_id != NULL)
        free(tmp_id);

    ast_t *spec_child = spec_node->child;
    spec_child = unwrap_pascal_node(spec_child);
    if (spec_child != NULL && spec_child->typ == PASCAL_T_IDENTIFIER && spec_child->sym != NULL &&
        spec_child->sym->name != NULL) {
        return resolve_array_type_info_from_ast(spec_child->sym->name, type_section, out_info, depth + 1);
    }

    (void)mapped;
    return -1;
}

void resolve_array_bounds(TypeInfo *info, ast_t *type_section, ast_t *const_section, const char *id_for_error) {
    if (info == NULL)
        return;
    if (info->start != 0 || info->end != 0)
        return;
    if (info->array_dimensions == NULL || info->array_dimensions->cur == NULL)
        return;

    char *range_str = (char *)info->array_dimensions->cur;
    char *range_copy = strdup(range_str);
    if (range_copy == NULL)
        return;

    char *sep = strstr(range_copy, "..");
    if (sep != NULL) {
        *sep = '\0';
        char *start_id = range_copy;
        char *end_id = sep + 2;

        while (*start_id == ' ' || *start_id == '\t') start_id++;
        while (*end_id == ' ' || *end_id == '\t') end_id++;
        char *p = start_id + strlen(start_id) - 1;
        while (p > start_id && (*p == ' ' || *p == '\t')) *p-- = '\0';
        p = end_id + strlen(end_id) - 1;
        while (p > end_id && (*p == ' ' || *p == '\t')) *p-- = '\0';

        int start_ordinal = resolve_enum_ordinal_from_ast(start_id, type_section);
        int end_ordinal = resolve_enum_ordinal_from_ast(end_id, type_section);
        if (start_ordinal >= 0 && end_ordinal >= 0) {
            info->start = start_ordinal;
            info->end = end_ordinal;
        } else {
            int start_val;
            if (evaluate_simple_const_expr(start_id, const_section, &start_val) == 0) {
                info->start = start_val;
            } else {
                int resolved_start = resolve_const_int_from_ast(start_id, const_section, 0);
                if (resolved_start != 0 || strcmp(start_id, "0") == 0) {
                    info->start = resolved_start;
                } else {
                    char *endptr;
                    long num = strtol(start_id, &endptr, 10);
                    if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                        info->start = (int)num;
                }
            }

            int end_val;
            if (evaluate_simple_const_expr(end_id, const_section, &end_val) == 0) {
                info->end = end_val;
            } else {
                int resolved_end = resolve_const_int_from_ast(end_id, const_section, 0);
                if (resolved_end != 0 || strcmp(end_id, "0") == 0) {
                    info->end = resolved_end;
                } else {
                    char *endptr;
                    long num = strtol(end_id, &endptr, 10);
                    if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                        info->end = (int)num;
                }
            }
        }
    } else {
        int enum_start, enum_end;
        if (resolve_enum_type_range_with_fallback(range_str, type_section, &enum_start, &enum_end) == 0) {
            info->start = enum_start;
            info->end = enum_end;
        } else {
            /* Defer resolution — store the type name for a post-load fixup pass */
            info->unresolved_index_type = strdup(range_str);
        }
    }

    free(range_copy);
}

static int resolve_const_int_in_section(const char *identifier, ast_t *const_section,
                                        int *out_value, int depth) {
    if (identifier == NULL || const_section == NULL || out_value == NULL)
        return -1;
    return resolve_const_int_in_node(identifier, const_section->child, const_section, out_value, depth);
}

int type_name_exists_in_section(const char *name, ast_t *type_section) {
    if (name == NULL || type_section == NULL)
        return 0;
    ast_t *type_decl = type_section->child;
    while (type_decl != NULL) {
        if (type_decl->typ == PASCAL_T_TYPE_DECL) {
            ast_t *id_node = type_decl->child;
            if (id_node != NULL && id_node->typ == PASCAL_T_IDENTIFIER &&
                id_node->sym != NULL && id_node->sym->name != NULL &&
                strcasecmp(id_node->sym->name, name) == 0) {
                return 1;
            }
        }
        type_decl = type_decl->next;
    }
    return 0;
}

int evaluate_const_int_expr(ast_t *expr, int *out_value, int depth) {
    if (expr == NULL || out_value == NULL)
        return -1;
    if (depth > 32)
        return -1;

    expr = unwrap_pascal_node(expr);
    if (expr == NULL)
        return -1;

    switch (expr->typ) {
    case PASCAL_T_INTEGER:
    {
        const char *num_str = (expr->sym != NULL) ? expr->sym->name : "0";
        int base = 10;
        if (num_str[0] == '$') {
            base = 16;
            num_str++;
        } else if (num_str[0] == '%') {
            base = 2;
            num_str++;
        } else if (num_str[0] == '&') {
            base = 8;
            num_str++;
        }
        char *endptr = NULL;
        long val = strtol(num_str, &endptr, base);
        if (endptr != NULL && *endptr == '\0' && val >= INT_MIN && val <= INT_MAX) {
            *out_value = (int)val;
            return 0;
        }
        return -1;
    }
    case PASCAL_T_IDENTIFIER:
        return lookup_const_int(expr->sym != NULL ? expr->sym->name : NULL, out_value);
    case PASCAL_T_NEG:
    {
        int inner = 0;
        if (evaluate_const_int_expr(expr->child, &inner, depth + 1) == 0) {
            *out_value = -inner;
            return 0;
        }
        return -1;
    }
    case PASCAL_T_ADD:
    case PASCAL_T_SUB:
    {
        int left = 0;
        int right = 0;
        if (expr->child == NULL || expr->child->next == NULL)
            return -1;
        if (evaluate_const_int_expr(expr->child, &left, depth + 1) != 0)
            return -1;
        if (evaluate_const_int_expr(expr->child->next, &right, depth + 1) != 0)
            return -1;
        if (expr->typ == PASCAL_T_ADD)
            *out_value = left + right;
        else
            *out_value = left - right;
        return 0;
    }
    case PASCAL_T_MUL:
    case PASCAL_T_DIV:
    case PASCAL_T_INTDIV:
    case PASCAL_T_MOD:
    case PASCAL_T_SHL:
    case PASCAL_T_SHR:
    {
        int left = 0;
        int right = 0;
        if (expr->child == NULL || expr->child->next == NULL)
            return -1;
        if (evaluate_const_int_expr(expr->child, &left, depth + 1) != 0)
            return -1;
        if (evaluate_const_int_expr(expr->child->next, &right, depth + 1) != 0)
            return -1;
        switch (expr->typ) {
            case PASCAL_T_MUL:
                *out_value = left * right;
                return 0;
            case PASCAL_T_DIV:
            case PASCAL_T_INTDIV:
                if (right == 0)
                    return -1;
                *out_value = left / right;
                return 0;
            case PASCAL_T_MOD:
                if (right == 0)
                    return -1;
                *out_value = left % right;
                return 0;
            case PASCAL_T_SHL:
                *out_value = left << right;
                return 0;
            case PASCAL_T_SHR:
                *out_value = (unsigned int)left >> right;
                return 0;
            default:
                break;
        }
        return -1;
    }
    case PASCAL_T_NOT:
    {
        int inner = 0;
        if (evaluate_const_int_expr(expr->child, &inner, depth + 1) == 0) {
            *out_value = ~inner;
            return 0;
        }
        return -1;
    }
    case PASCAL_T_AND:
    case PASCAL_T_OR:
    case PASCAL_T_XOR:
    {
        int left = 0, right = 0;
        if (expr->child == NULL || expr->child->next == NULL)
            return -1;
        if (evaluate_const_int_expr(expr->child, &left, depth + 1) != 0)
            return -1;
        if (evaluate_const_int_expr(expr->child->next, &right, depth + 1) != 0)
            return -1;
        if (expr->typ == PASCAL_T_AND)
            *out_value = left & right;
        else if (expr->typ == PASCAL_T_OR)
            *out_value = left | right;
        else
            *out_value = left ^ right;
        return 0;
    }
    case PASCAL_T_FUNC_CALL:
    {
        /* Typecast expressions like tregister($05000000) are parsed as
         * FUNC_CALL where child is the type name and child->next is the
         * argument.  Evaluate the argument as an integer constant. */
        if (expr->child != NULL && expr->child->next != NULL)
            return evaluate_const_int_expr(expr->child->next, out_value, depth + 1);
        return -1;
    }
    default:
        return -1;
    }
}

static int resolve_const_int_from_ast_internal(const char *identifier, ast_t *const_section,
                                               int fallback_value, int depth) {
    if (identifier == NULL || const_section == NULL)
        return fallback_value;
    if (depth > 32)
        return fallback_value;

    int cached = 0;
    if (lookup_const_int(identifier, &cached) == 0)
        return cached;

    /* Fast path: use the indexed const-decl map for O(1) lookup.
     * This replaces the recursive resolve_const_int_in_section walk. */
    ast_t *indexed_value = const_decl_index_lookup(identifier);
    if (indexed_value != NULL) {
        ast_t *value_node = unwrap_pascal_node(indexed_value);
        if (value_node != NULL && value_node->sym != NULL) {
            char *endptr;
            long val = strtol(value_node->sym->name, &endptr, 10);
            if (*endptr == '\0' && val >= INT_MIN && val <= INT_MAX) {
                register_const_int(identifier, (int)val);
                return (int)val;
            }
            /* Value is another identifier — resolve recursively */
            if (value_node->typ == PASCAL_T_IDENTIFIER &&
                strcasecmp(value_node->sym->name, identifier) != 0) {
                int resolved = resolve_const_int_from_ast_internal(
                    value_node->sym->name, const_section, INT_MIN, depth + 1);
                if (resolved != INT_MIN) {
                    register_const_int(identifier, resolved);
                    return resolved;
                }
            }
        } else if (value_node != NULL && value_node->typ == PASCAL_T_NEG &&
                   value_node->child != NULL) {
            ast_t *inner = unwrap_pascal_node(value_node->child);
            if (inner != NULL && inner->sym != NULL) {
                int resolved = resolve_const_int_from_ast_internal(
                    inner->sym->name, const_section, INT_MIN, depth + 1);
                if (resolved != INT_MIN) {
                    register_const_int(identifier, -resolved);
                    return -resolved;
                }
            }
        } else if (value_node != NULL) {
            /* Try evaluating as a const expression (handles +, -, *, etc.) */
            int eval_result = 0;
            if (evaluate_const_int_expr(value_node, &eval_result, depth + 1) == 0) {
                register_const_int(identifier, eval_result);
                return eval_result;
            }
        }
    }

    /* Slow fallback: recursive AST walk (for const sections not yet indexed) */
    int resolved = INT_MIN;
    int found = 0;
    if (g_const_sections != NULL) {
        for (ListNode_t *cur = g_const_sections; cur != NULL; cur = cur->next) {
            if (resolve_const_int_in_section(identifier, (ast_t *)cur->cur, &resolved, depth) == 0) {
                found = 1;
                break;
            }
        }
    } else {
        /* Index this const_section for future O(1) lookups */
        const_decl_index_scan_section(const_section);
        if (resolve_const_int_in_section(identifier, const_section, &resolved, depth) == 0)
            found = 1;
    }

    if (found) {
        /* Cache the resolved value so future lookups are O(1) */
        register_const_int(identifier, resolved);
        return resolved;
    }
    return fallback_value; /* Not found */
}

int resolve_const_int_from_ast(const char *identifier, ast_t *const_section, int fallback_value) {
    return resolve_const_int_from_ast_internal(identifier, const_section, fallback_value, 0);
}

/* Evaluate simple const expression like "NUM-1" or "NUM+1" */
typedef struct ConstExprScanner {
    const char *input;
    size_t pos;
    ast_t *const_section;
} ConstExprScanner;

static void const_expr_skip_ws(ConstExprScanner *scanner) {
    while (scanner->input[scanner->pos] != '\0' &&
           isspace((unsigned char)scanner->input[scanner->pos])) {
        scanner->pos++;
    }
}

static int const_expr_match_char(ConstExprScanner *scanner, char ch) {
    const_expr_skip_ws(scanner);
    if (scanner->input[scanner->pos] == ch) {
        scanner->pos++;
        return 1;
    }
    return 0;
}

static int const_expr_match_keyword(ConstExprScanner *scanner, const char *kw) {
    const_expr_skip_ws(scanner);
    size_t len = strlen(kw);
    if (strncasecmp(scanner->input + scanner->pos, kw, len) != 0)
        return 0;
    char next = scanner->input[scanner->pos + len];
    if (isalnum((unsigned char)next) || next == '_')
        return 0;
    scanner->pos += len;
    return 1;
}

int parse_integer_literal(const char *num_str, int base, long long *out_value, char **out_endptr)
{
    if (num_str == NULL || out_value == NULL)
        return -1;

    errno = 0;
    char *endptr = NULL;
    unsigned long long value = strtoull(num_str, &endptr, base);
    if (endptr == num_str)
        return -1;
    if (errno == ERANGE)
        errno = 0;

    *out_value = (long long)value;
    if (out_endptr != NULL)
        *out_endptr = endptr;
    return 0;
}

static int const_expr_parse_number(ConstExprScanner *scanner, long long *out_value) {
    const_expr_skip_ws(scanner);
    const char *start = scanner->input + scanner->pos;
    if (*start == '\0')
        return -1;

    int base = 10;
    const char *num_start = start;
    if (*start == '$') {
        base = 16;
        num_start = start + 1;
    } else if (*start == '%') {
        base = 2;
        num_start = start + 1;
    } else if (*start == '&') {
        base = 8;
        num_start = start + 1;
    } else if (!isdigit((unsigned char)*start)) {
        return -1;
    }

    char *endptr = NULL;
    long long value = 0;
    if (parse_integer_literal(num_start, base, &value, &endptr) != 0)
        return -1;

    scanner->pos = (size_t)(endptr - scanner->input);
    *out_value = value;
    return 0;
}

/* Resolve SizeOf(typename) to a compile-time constant for known types.
   Returns the size in bytes on x86_64, or -1 if the type is unknown. */
static int resolve_sizeof_type(const char *type_name) {
    if (type_name == NULL)
        return -1;
    /* Pointer-sized types (8 bytes on x86_64) */
    if (strcasecmp(type_name, "Pointer") == 0 ||
        strcasecmp(type_name, "CodePointer") == 0 ||
        strcasecmp(type_name, "SizeInt") == 0 ||
        strcasecmp(type_name, "SizeUInt") == 0 ||
        strcasecmp(type_name, "PtrInt") == 0 ||
        strcasecmp(type_name, "PtrUInt") == 0 ||
        strcasecmp(type_name, "Int64") == 0 ||
        strcasecmp(type_name, "QWord") == 0 ||
        strcasecmp(type_name, "NativeInt") == 0 ||
        strcasecmp(type_name, "NativeUInt") == 0 ||
        strcasecmp(type_name, "ValSInt") == 0 ||
        strcasecmp(type_name, "ValUInt") == 0 ||
        strcasecmp(type_name, "CodePtrInt") == 0 ||
        strcasecmp(type_name, "CodePtrUInt") == 0)
        return 8;
    /* 4-byte types */
    if (strcasecmp(type_name, "LongInt") == 0 ||
        strcasecmp(type_name, "LongWord") == 0 ||
        strcasecmp(type_name, "DWord") == 0 ||
        strcasecmp(type_name, "Cardinal") == 0 ||
        strcasecmp(type_name, "Integer") == 0 ||
        strcasecmp(type_name, "Single") == 0)
        return 4;
    /* 2-byte types */
    if (strcasecmp(type_name, "SmallInt") == 0 ||
        strcasecmp(type_name, "Word") == 0 ||
        strcasecmp(type_name, "WideChar") == 0)
        return 2;
    /* 1-byte types */
    if (strcasecmp(type_name, "Byte") == 0 ||
        strcasecmp(type_name, "ShortInt") == 0 ||
        strcasecmp(type_name, "Boolean") == 0 ||
        strcasecmp(type_name, "Char") == 0 ||
        strcasecmp(type_name, "AnsiChar") == 0)
        return 1;
    /* 8-byte float */
    if (strcasecmp(type_name, "Double") == 0 ||
        strcasecmp(type_name, "Real") == 0)
        return 8;
    /* 10-byte extended (padded to 16 on x86_64 in some ABIs, but SizeOf returns 10) */
    if (strcasecmp(type_name, "Extended") == 0)
        return 10;
    return -1;
}

static int const_expr_parse_identifier(ConstExprScanner *scanner, long long *out_value) {
    const_expr_skip_ws(scanner);
    size_t start = scanner->pos;
    char ch = scanner->input[start];
    if (!(isalpha((unsigned char)ch) || ch == '_'))
        return -1;
    scanner->pos++;
    while (scanner->input[scanner->pos] != '\0') {
        char c = scanner->input[scanner->pos];
        if (!(isalnum((unsigned char)c) || c == '_'))
            break;
        scanner->pos++;
    }

    size_t len = scanner->pos - start;
    char *ident = (char *)malloc(len + 1);
    if (ident == NULL)
        return -1;
    memcpy(ident, scanner->input + start, len);
    ident[len] = '\0';

    /* Handle SizeOf(typename) as a compile-time builtin */
    if (strcasecmp(ident, "sizeof") == 0) {
        free(ident);
        const_expr_skip_ws(scanner);
        if (scanner->input[scanner->pos] != '(')
            return -1;
        scanner->pos++;  /* skip '(' */
        const_expr_skip_ws(scanner);
        /* Read the type name argument */
        size_t arg_start = scanner->pos;
        while (scanner->input[scanner->pos] != '\0' &&
               scanner->input[scanner->pos] != ')' &&
               (isalnum((unsigned char)scanner->input[scanner->pos]) || scanner->input[scanner->pos] == '_'))
            scanner->pos++;
        size_t arg_len = scanner->pos - arg_start;
        if (arg_len == 0)
            return -1;
        char *type_name = (char *)malloc(arg_len + 1);
        if (type_name == NULL)
            return -1;
        memcpy(type_name, scanner->input + arg_start, arg_len);
        type_name[arg_len] = '\0';
        const_expr_skip_ws(scanner);
        if (scanner->input[scanner->pos] != ')') {
            free(type_name);
            return -1;
        }
        scanner->pos++;  /* skip ')' */
        int size = resolve_sizeof_type(type_name);
        free(type_name);
        if (size < 0)
            return -1;
        *out_value = size;
        return 0;
    }

    /* Handle boolean literals */
    if (strcasecmp(ident, "false") == 0) {
        free(ident);
        *out_value = 0;
        return 0;
    }
    if (strcasecmp(ident, "true") == 0) {
        free(ident);
        *out_value = 1;
        return 0;
    }

    int val = resolve_const_int_from_ast(ident, scanner->const_section, INT_MIN);
    free(ident);
    if (val == INT_MIN)
        return -1;
    *out_value = val;
    return 0;
}

static int const_expr_parse_expression(ConstExprScanner *scanner, long long *out_value);

static int const_expr_parse_primary(ConstExprScanner *scanner, long long *out_value) {
    const_expr_skip_ws(scanner);
    if (const_expr_match_char(scanner, '(')) {
        if (const_expr_parse_expression(scanner, out_value) != 0)
            return -1;
        if (!const_expr_match_char(scanner, ')'))
            return -1;
        return 0;
    }
    if (const_expr_parse_number(scanner, out_value) == 0)
        return 0;
    return const_expr_parse_identifier(scanner, out_value);
}

static int const_expr_parse_unary(ConstExprScanner *scanner, long long *out_value) {
    if (const_expr_match_char(scanner, '-')) {
        long long inner = 0;
        if (const_expr_parse_unary(scanner, &inner) != 0)
            return -1;
        *out_value = -inner;
        return 0;
    }
    if (const_expr_match_keyword(scanner, "not")) {
        long long inner = 0;
        if (const_expr_parse_unary(scanner, &inner) != 0)
            return -1;
        *out_value = ~inner;
        return 0;
    }
    return const_expr_parse_primary(scanner, out_value);
}

static int const_expr_parse_mul(ConstExprScanner *scanner, long long *out_value) {
    long long lhs = 0;
    if (const_expr_parse_unary(scanner, &lhs) != 0)
        return -1;

    while (1) {
        if (const_expr_match_char(scanner, '*')) {
            long long rhs = 0;
            if (const_expr_parse_unary(scanner, &rhs) != 0)
                return -1;
            lhs *= rhs;
            continue;
        }
        if (const_expr_match_char(scanner, '/')) {
            long long rhs = 0;
            if (const_expr_parse_unary(scanner, &rhs) != 0)
                return -1;
            if (rhs == 0)
                return -1;
            lhs /= rhs;
            continue;
        }
        if (const_expr_match_keyword(scanner, "div")) {
            long long rhs = 0;
            if (const_expr_parse_unary(scanner, &rhs) != 0)
                return -1;
            if (rhs == 0)
                return -1;
            lhs /= rhs;
            continue;
        }
        if (const_expr_match_keyword(scanner, "mod")) {
            long long rhs = 0;
            if (const_expr_parse_unary(scanner, &rhs) != 0)
                return -1;
            if (rhs == 0)
                return -1;
            lhs %= rhs;
            continue;
        }
        if (const_expr_match_keyword(scanner, "shl")) {
            long long rhs = 0;
            if (const_expr_parse_unary(scanner, &rhs) != 0)
                return -1;
            lhs = lhs << rhs;
            continue;
        }
        if (const_expr_match_keyword(scanner, "shr")) {
            long long rhs = 0;
            if (const_expr_parse_unary(scanner, &rhs) != 0)
                return -1;
            lhs = (unsigned long long)lhs >> rhs;
            continue;
        }
        break;
    }

    *out_value = lhs;
    return 0;
}

static int const_expr_parse_add(ConstExprScanner *scanner, long long *out_value) {
    long long lhs = 0;
    if (const_expr_parse_mul(scanner, &lhs) != 0)
        return -1;

    while (1) {
        if (const_expr_match_char(scanner, '+')) {
            long long rhs = 0;
            if (const_expr_parse_mul(scanner, &rhs) != 0)
                return -1;
            lhs += rhs;
            continue;
        }
        if (const_expr_match_char(scanner, '-')) {
            long long rhs = 0;
            if (const_expr_parse_mul(scanner, &rhs) != 0)
                return -1;
            lhs -= rhs;
            continue;
        }
        break;
    }

    *out_value = lhs;
    return 0;
}

static int const_expr_parse_expression(ConstExprScanner *scanner, long long *out_value) {
    long long lhs = 0;
    if (const_expr_parse_add(scanner, &lhs) != 0)
        return -1;

    while (1) {
        if (const_expr_match_keyword(scanner, "and")) {
            long long rhs = 0;
            if (const_expr_parse_add(scanner, &rhs) != 0)
                return -1;
            lhs = lhs & rhs;
            continue;
        }
        if (const_expr_match_keyword(scanner, "or")) {
            long long rhs = 0;
            if (const_expr_parse_add(scanner, &rhs) != 0)
                return -1;
            lhs = lhs | rhs;
            continue;
        }
        if (const_expr_match_keyword(scanner, "xor")) {
            long long rhs = 0;
            if (const_expr_parse_add(scanner, &rhs) != 0)
                return -1;
            lhs = lhs ^ rhs;
            continue;
        }
        break;
    }

    *out_value = lhs;
    return 0;
}

int evaluate_simple_const_expr(const char *expr, ast_t *const_section, int *result) {
    if (expr == NULL || result == NULL)
        return -1;

    ConstExprScanner scanner = {expr, 0, const_section};
    long long value = 0;
    if (const_expr_parse_expression(&scanner, &value) != 0)
        return -1;
    const_expr_skip_ws(&scanner);
    if (scanner.input[scanner.pos] != '\0')
        return -1;
    if (value < INT_MIN || value > INT_MAX)
        return -1;
    *result = (int)value;
    return 0;
}

/* Serialize an expression AST node to a string representation.
 * Handles simple identifiers, literals, and binary operations (+, -).
 * Returns a malloc'd string or NULL on failure. */
static int const_expr_precedence(ast_t *expr) {
    if (expr == NULL)
        return 0;
    switch (expr->typ) {
        case PASCAL_T_ADD:
        case PASCAL_T_SUB:
            return 1;
        case PASCAL_T_MUL:
        case PASCAL_T_DIV:
        case PASCAL_T_INTDIV:
        case PASCAL_T_MOD:
        case PASCAL_T_SHL:
        case PASCAL_T_SHR:
            return 2;
        case PASCAL_T_NEG:
            return 3;
        default:
            return 4;
    }
}

static char *serialize_expr_to_string_internal(ast_t *expr, int parent_prec) {
    if (expr == NULL)
        return NULL;
    expr = unwrap_pascal_node(expr);
    if (expr == NULL)
        return NULL;

    /* Simple identifier or literal with a symbol */
    if (expr->sym != NULL && expr->sym->name != NULL) {
        if (strcasecmp(expr->sym->name, "sizeof") == 0 &&
            kgpc_getenv("KGPC_DEBUG_ARRAY_BOUNDS") != NULL)
        {
            fprintf(stderr,
                "[KGPC] sizeof node typ=%d child=%p child_typ=%d child_sym=%s next_typ=%d\n",
                expr->typ,
                (void *)expr->child,
                expr->child ? expr->child->typ : -1,
                (expr->child && expr->child->sym && expr->child->sym->name) ? expr->child->sym->name : "<null>",
                expr->next ? expr->next->typ : -1);
        }
        if (strcasecmp(expr->sym->name, "sizeof") == 0 && expr->child != NULL) {
            char *inner = serialize_expr_to_string_internal(expr->child, 0);
            if (inner == NULL)
                return NULL;
            size_t len = strlen(inner) + 9;  /* "sizeof(" + inner + ")" + '\0' */
            char *result = (char *)malloc(len);
            if (result == NULL) {
                free(inner);
                return NULL;
            }
            snprintf(result, len, "sizeof(%s)", inner);
            free(inner);
            return result;
        }
        return strdup(expr->sym->name);
    }

    /* Unary minus: -X */
    if (expr->typ == PASCAL_T_NEG && expr->child != NULL) {
        int prec = const_expr_precedence(expr);
        char *inner = serialize_expr_to_string_internal(expr->child, prec);
        if (inner == NULL)
            return NULL;
        size_t len = strlen(inner) + 2;
        char *result = (char *)malloc(len + 2);
        if (result == NULL) {
            free(inner);
            return NULL;
        }
        if (const_expr_precedence(expr->child) < prec)
            snprintf(result, len + 2, "-(%s)", inner);
        else
            snprintf(result, len + 2, "-%s", inner);
        free(inner);
        if (prec < parent_prec) {
            size_t wrap_len = strlen(result) + 3;
            char *wrapped = (char *)malloc(wrap_len);
            if (wrapped == NULL) {
                free(result);
                return NULL;
            }
            snprintf(wrapped, wrap_len, "(%s)", result);
            free(result);
            return wrapped;
        }
        return result;
    }

    if ((expr->typ == PASCAL_T_ADD || expr->typ == PASCAL_T_SUB ||
         expr->typ == PASCAL_T_MUL || expr->typ == PASCAL_T_DIV ||
         expr->typ == PASCAL_T_INTDIV || expr->typ == PASCAL_T_MOD || expr->typ == PASCAL_T_SHL ||
         expr->typ == PASCAL_T_SHR) &&
        expr->child != NULL && expr->child->next != NULL) {
        int prec = const_expr_precedence(expr);
        char *left = serialize_expr_to_string_internal(expr->child, prec);
        char *right = serialize_expr_to_string_internal(expr->child->next, prec);
        if (left == NULL || right == NULL) {
            if (left) free(left);
            if (right) free(right);
            return NULL;
        }
        const char *op = NULL;
        switch (expr->typ) {
            case PASCAL_T_ADD: op = "+"; break;
            case PASCAL_T_SUB: op = "-"; break;
            case PASCAL_T_MUL: op = "*"; break;
            case PASCAL_T_DIV: op = "div"; break;
            case PASCAL_T_INTDIV: op = "div"; break;
            case PASCAL_T_MOD: op = "mod"; break;
            case PASCAL_T_SHL: op = "shl"; break;
            case PASCAL_T_SHR: op = "shr"; break;
            default: break;
        }
        size_t len = strlen(left) + strlen(right) + strlen(op) + 3;
        char *result = (char *)malloc(len + 2);
        if (result == NULL) {
            free(left);
            free(right);
            return NULL;
        }
        snprintf(result, len + 2, "%s %s %s", left, op, right);
        free(left);
        free(right);
        if (prec < parent_prec) {
            size_t wrap_len = strlen(result) + 3;
            char *wrapped = (char *)malloc(wrap_len);
            if (wrapped == NULL) {
                free(result);
                return NULL;
            }
            snprintf(wrapped, wrap_len, "(%s)", result);
            free(result);
            return wrapped;
        }
        return result;
    }

    /* If we have a child with a symbol (wrapped expression) */
    if (expr->child != NULL && expr->child->sym != NULL && expr->child->sym->name != NULL) {
        if (strcasecmp(expr->child->sym->name, "sizeof") == 0) {
            ast_t *arg_node = expr->child->child;
            if (arg_node == NULL)
                arg_node = expr->child->next;
            if (kgpc_getenv("KGPC_DEBUG_ARRAY_BOUNDS") != NULL)
            {
                fprintf(stderr,
                    "[KGPC] sizeof wrapper typ=%d arg_typ=%d arg_sym=%s\n",
                    expr->child->typ,
                    arg_node ? arg_node->typ : -1,
                    (arg_node && arg_node->sym && arg_node->sym->name) ? arg_node->sym->name : "<null>");
            }
            if (arg_node != NULL)
            {
                char *inner = serialize_expr_to_string_internal(arg_node, 0);
                if (inner != NULL)
                {
                    size_t len = strlen(inner) + 9;
                    char *result = (char *)malloc(len);
                    if (result == NULL) {
                        free(inner);
                        return NULL;
                    }
                    snprintf(result, len, "sizeof(%s)", inner);
                    free(inner);
                    return result;
                }
            }
        }
        return strdup(expr->child->sym->name);
    }

    return NULL;
}

static char *serialize_expr_to_string(ast_t *expr) {
    return serialize_expr_to_string_internal(expr, 0);
}

static int resolve_const_expr_to_int(ast_t *expr, int *out_value)
{
    if (out_value == NULL)
        return -1;
    *out_value = 0;
    if (expr == NULL)
        return -1;

    int direct_value = 0;
    if (evaluate_const_int_expr(expr, &direct_value, 0) == 0) {
        *out_value = direct_value;
        return 0;
    }

    char *expr_str = serialize_expr_to_string(expr);
    if (expr_str == NULL)
        return -1;

    int value = 0;
    if (resolve_const_expr_from_sections(expr_str, &value) != 0)
        value = atoi(expr_str);
    free(expr_str);

    *out_value = value;
    return 0;
}

struct RecordField *find_record_field_by_name(const struct RecordType *record,
    const char *field_name)
{
    if (record == NULL || field_name == NULL)
        return NULL;
    for (ListNode_t *cur = record->fields; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_RECORD_FIELD)
            continue;
        struct RecordField *field = (struct RecordField *)cur->cur;
        if (field != NULL && field->name != NULL &&
            pascal_identifier_equals(field->name, field_name))
        {
            return field;
        }
    }
    return NULL;
}

int convert_type_spec(ast_t *type_spec, char **type_id_out,
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
        type_info->element_type_ref = NULL;
        type_info->is_open_array = 0;
        type_info->array_dimensions = NULL;
        type_info->is_pointer = 0;
        type_info->pointer_type = UNKNOWN_TYPE;
        type_info->pointer_type_id = NULL;
        type_info->pointer_type_ref = NULL;
        type_info->is_set = 0;
        type_info->set_element_type = UNKNOWN_TYPE;
        type_info->set_element_type_id = NULL;
        type_info->set_element_type_ref = NULL;
        type_info->is_enum = 0;
        type_info->enum_has_explicit_values = 0;
        type_info->enum_literals = NULL;
        type_info->is_file = 0;
        type_info->file_type = UNKNOWN_TYPE;
        type_info->file_type_id = NULL;
        type_info->file_type_ref = NULL;
        type_info->is_record = 0;
        type_info->record_type = NULL;
        type_info->is_generic_specialization = 0;
        type_info->generic_base_name = NULL;
        type_info->generic_type_args = NULL;
        type_info->type_ref = NULL;
        type_info->is_range = 0;
        type_info->range_known = 0;
        type_info->range_start = 0;
        type_info->range_end = 0;
    }

    if (type_spec == NULL)
        return UNKNOWN_TYPE;

    ast_t *spec_node = type_spec;
    if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
        spec_node = spec_node->child;
    spec_node = unwrap_pascal_node(spec_node);

    if (spec_node == NULL)
        return UNKNOWN_TYPE;

    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
        fprintf(stderr, "[KGPC] convert_type_spec node typ=%d (%s) sym=%s\n",
            spec_node->typ,
            pascal_tag_to_string(spec_node->typ),
            (spec_node->sym != NULL && spec_node->sym->name != NULL) ? spec_node->sym->name : "<null>");

    if (spec_node->typ == PASCAL_T_QUALIFIED_IDENTIFIER) {
        QualifiedIdent *qid = qualified_ident_from_ast(spec_node);
        const char *last = qualified_ident_last(qid);
        char *last_dup = last ? strdup(last) : NULL;
        char *qualified_name = qualified_ident_join(qid, ".");
        int result = map_type_name(last_dup, type_id_out);
        if (type_info != NULL && qid != NULL)
            type_info->type_ref = type_ref_create(qid, NULL, 0);
        else if (qid != NULL)
            qualified_ident_free(qid);
        if (type_info != NULL && result == FILE_TYPE) {
            type_info->is_file = 1;
            type_info->file_type = FILE_TYPE;
            if (type_id_out != NULL && *type_id_out != NULL)
                type_info->file_type_id = strdup(*type_id_out);
            if (type_info->file_type_id != NULL)
                type_info->file_type_ref = type_ref_from_single_name(type_info->file_type_id);
        }
        if (result == UNKNOWN_TYPE && type_id_out != NULL) {
            /* map_type_name sets *type_id_out to the bare last component even
             * for UNKNOWN_TYPE.  Override with the full qualified name so that
             * unit-qualified references like baseunix.stat are preserved. */
            if (qualified_name != NULL) {
                free(*type_id_out);
                *type_id_out = qualified_name;
                qualified_name = NULL;
            } else if (*type_id_out == NULL) {
                *type_id_out = last_dup;
                last_dup = NULL;
            }
        }
        free(qualified_name);
        free(last_dup);
        return result;
    }
    if (spec_node->typ == PASCAL_T_MEMBER_ACCESS) {
        QualifiedIdent *qid = qualified_ident_from_ast(spec_node);
        const char *last = qualified_ident_last(qid);
        char *last_dup = last ? strdup(last) : NULL;
        char *qualified_name = qualified_ident_join(qid, ".");
        int result = map_type_name(last_dup, type_id_out);
        if (type_info != NULL && qid != NULL)
            type_info->type_ref = type_ref_create(qid, NULL, 0);
        else if (qid != NULL)
            qualified_ident_free(qid);
        if (type_info != NULL && result == FILE_TYPE) {
            type_info->is_file = 1;
            type_info->file_type = FILE_TYPE;
            if (type_id_out != NULL && *type_id_out != NULL)
                type_info->file_type_id = strdup(*type_id_out);
            if (type_info->file_type_id != NULL)
                type_info->file_type_ref = type_ref_from_single_name(type_info->file_type_id);
        }
        if (result == UNKNOWN_TYPE && type_id_out != NULL) {
            if (qualified_name != NULL) {
                free(*type_id_out);
                *type_id_out = qualified_name;
                qualified_name = NULL;
            } else if (*type_id_out == NULL) {
                *type_id_out = last_dup;
                last_dup = NULL;
            }
        }
        free(qualified_name);
        free(last_dup);
        return result;
    }
    if (spec_node->typ == PASCAL_T_IDENTIFIER) {
        char *dup = dup_symbol(spec_node);
        if (dup != NULL && strchr(dup, '.') != NULL)
        {
            QualifiedIdent *qid = qualified_ident_from_ast(spec_node);
            const char *last = qualified_ident_last(qid);
            char *last_dup = last ? strdup(last) : NULL;
            char *qualified_name = qualified_ident_join(qid, ".");
            int result = map_type_name(last_dup, type_id_out);
            if (type_info != NULL && qid != NULL)
                type_info->type_ref = type_ref_create(qid, NULL, 0);
            else if (qid != NULL)
                qualified_ident_free(qid);
            if (type_info != NULL && result == FILE_TYPE) {
                type_info->is_file = 1;
                type_info->file_type = FILE_TYPE;
                if (type_id_out != NULL && *type_id_out != NULL)
                    type_info->file_type_id = strdup(*type_id_out);
                if (type_info->file_type_id != NULL)
                    type_info->file_type_ref = type_ref_from_single_name(type_info->file_type_id);
            }
            if (result == UNKNOWN_TYPE && type_id_out != NULL) {
                if (qualified_name != NULL) {
                    free(*type_id_out);
                    *type_id_out = qualified_name;
                    qualified_name = NULL;
                } else if (*type_id_out == NULL) {
                    if (dup != NULL)
                        *type_id_out = strdup(dup);
                    else {
                        *type_id_out = last_dup;
                        last_dup = NULL;
                    }
                }
            }
            free(qualified_name);
            free(last_dup);
            free(dup);
            return result;
        }

        if (dup != NULL && spec_node->child != NULL &&
            strcasecmp(dup, "string") == 0)
        {
            int size_val = 0;
            if (resolve_const_expr_to_int(spec_node->child, &size_val) != 0)
            {
                frontend_error("Error on line %d: string bound must be constant",
                    spec_node->line);
                size_val = 0;
            }
            if (size_val < 0)
            {
                frontend_error("Error on line %d: string bound must be >= 0",
                    spec_node->line);
                size_val = 0;
            }

            if (type_info != NULL)
            {
                type_info->is_array = 1;
                type_info->start = 0;
                type_info->end = size_val;
                type_info->element_type = CHAR_TYPE;
                type_info->element_type_id = strdup("char");
                type_info->element_type_ref = type_ref_from_single_name("char");
                type_info->is_shortstring = 1;

                ListBuilder dims_builder;
                list_builder_init(&dims_builder);
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "0..%d", size_val);
                list_builder_append(&dims_builder, strdup(buffer), LIST_STRING);
                type_info->array_dimensions = list_builder_finish(&dims_builder);
                /* Numeric bounds are in type_info->start/end; no symbolic strings needed */
                type_info->array_dims_parsed = 1;
            }

            free(dup);
            return UNKNOWN_TYPE;
        }

        int result = map_type_name(dup, type_id_out);
        result = apply_shortstring_mode(result, dup);
        if (type_info != NULL && result == FILE_TYPE) {
            type_info->is_file = 1;
            type_info->file_type = FILE_TYPE;
            if (type_id_out != NULL && *type_id_out != NULL)
                type_info->file_type_id = strdup(*type_id_out);
            if (type_info->file_type_id != NULL)
                type_info->file_type_ref = type_ref_from_single_name(type_info->file_type_id);
        }
        if (type_info != NULL && type_info->type_ref == NULL && dup != NULL)
            type_info->type_ref = type_ref_from_single_name(dup);
        if (result == UNKNOWN_TYPE && type_id_out != NULL && *type_id_out == NULL) {
            *type_id_out = dup;
        } else {
            free(dup);
        }
        return result;
    }
    /* Generic type specializations: either constructed syntax TFoo<T> or
     * FPC-style \"specialize TFoo<T>\". Both map to generic instantiation
     * using the same mangling and RecordType template. */
    {
        char *base_name = NULL;
        ListNode_t *type_args = NULL;
        int is_generic = 0;

        if (spec_node->typ == PASCAL_T_CONSTRUCTED_TYPE) {
            is_generic = extract_constructed_type_info(type_spec, &base_name, &type_args);
        } else {
            is_generic = extract_specialize_type_info(type_spec, &base_name, &type_args);
        }

        if (is_generic) {
            char *specialized_name = NULL;
            struct RecordType *record = instantiate_generic_record(base_name, type_args, &specialized_name);

            if (type_info != NULL && type_info->type_ref == NULL)
                type_info->type_ref = type_ref_from_name_and_args(base_name, type_args);

            if (record != NULL) {
                if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL && specialized_name != NULL)
                {
                    fprintf(stderr, "[KGPC] convert_type_spec generic %s -> record=%p type_info_ptr=%p record_out_ptr=%p\n",
                        specialized_name, (void *)record, (void *)type_info, (void *)record_out);
                }
                if (type_info != NULL) {
                    type_info->is_record = 1;
                    type_info->record_type = record;
                    type_info->is_generic_specialization = 1;
                    record = NULL;
                } else if (record_out != NULL) {
                    *record_out = record;
                    record = NULL;
                }
                if (record != NULL)
                    destroy_record_type(record);
                if (type_id_out != NULL && specialized_name != NULL)
                    *type_id_out = specialized_name;
                else
                    free(specialized_name);
                free(base_name);
                if (type_args != NULL)
                    destroy_list(type_args);
                return RECORD_TYPE;
            }

            if (type_info != NULL) {
                int alias_result = UNKNOWN_TYPE;
                if (resolve_generic_alias_type(base_name, type_args, type_id_out, type_info,
                        &alias_result)) {
                    if (specialized_name != NULL)
                        free(specialized_name);
                    free(base_name);
                    if (type_args != NULL)
                        destroy_list(type_args);
                    return alias_result;
                }
            }

            int can_defer = (g_allow_pending_specializations && type_info != NULL);
            if (can_defer) {
                if (type_info != NULL) {
                    type_info->is_record = 1;
                    type_info->is_generic_specialization = 1;
                    type_info->generic_base_name = base_name;
                    type_info->generic_type_args = type_args;
                }
                if (type_id_out != NULL) {
                    if (*type_id_out != NULL) {
                        free(*type_id_out);
                    }
                    if (specialized_name == NULL)
                        specialized_name = mangle_specialized_name_from_list(base_name, type_args);
                    *type_id_out = specialized_name;
                    specialized_name = NULL;
                }
                if (specialized_name != NULL)
                    free(specialized_name);
                return RECORD_TYPE;
            }

            if (specialized_name != NULL)
                free(specialized_name);
            free(base_name);
            if (type_args != NULL)
                destroy_list(type_args);
        }
    }
    if (spec_node->typ == PASCAL_T_RANGE_TYPE) {
        ast_t *lower = unwrap_pascal_node(spec_node->child);
        ast_t *upper = (lower != NULL) ? unwrap_pascal_node(lower->next) : NULL;
        long long start_value = 0;
        long long end_value = 0;
        int have_start = 0;
        int have_end = 0;
        struct Expression *lower_expr = convert_expression(lower);
        struct Expression *upper_expr = convert_expression(upper);

        if (lower_expr != NULL) {
            have_start = (extract_constant_int(lower_expr, &start_value) == 0);
            destroy_expr(lower_expr);
        }
        if (upper_expr != NULL) {
            have_end = (extract_constant_int(upper_expr, &end_value) == 0);
            destroy_expr(upper_expr);
        }

        /* Fallback: try evaluating bounds from the AST using const int registry
         * (handles cases like 0..NUM_REGS - 1 where NUM_REGS is a named constant) */
        if (!have_start) {
            int val = 0;
            if (evaluate_const_int_expr(spec_node->child, &val, 0) == 0) {
                start_value = val;
                have_start = 1;
            }
        }
        if (!have_end && spec_node->child != NULL) {
            int val = 0;
            if (evaluate_const_int_expr(spec_node->child->next, &val, 0) == 0) {
                end_value = val;
                have_end = 1;
            }
        }

        if (type_info != NULL) {
            char *lower_str = serialize_expr_to_string(spec_node->child);
            char *upper_str = serialize_expr_to_string(spec_node->child != NULL ?
                spec_node->child->next : NULL);
            type_info->is_range = 1;
            type_info->range_start = start_value;
            type_info->range_end = end_value;
            type_info->range_known = (have_start && have_end);
            type_info->range_start_str = lower_str;
            type_info->range_end_str = upper_str;
        }

        return UNKNOWN_TYPE;
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

            if (kgpc_getenv("KGPC_DEBUG_ARRAY_BOUNDS") != NULL) {
                fprintf(stderr, "[KGPC] array type children:");
                for (ast_t *dim = child; dim != NULL; dim = dim->next) {
                    fprintf(stderr, " %d(%s)", dim->typ, pascal_tag_to_string(dim->typ));
                }
                fprintf(stderr, "\n");
            }

            // Check if there are any dimensions specified
            int has_dimensions = 0;
            for (ast_t *dim = child; dim != NULL && dim != element_node; dim = dim->next) {
                has_dimensions = 1;
                break;
            }
            
            // If no dimensions specified, it's an open array (e.g., "array of string")
            if (!has_dimensions) {
                type_info->is_open_array = 1;
            }

            for (ast_t *dim = child; dim != NULL && dim != element_node; dim = dim->next) {
                if (dim->typ == PASCAL_T_RANGE_TYPE) {
                    ast_t *lower = dim->child;
                    ast_t *upper = (lower != NULL) ? lower->next : NULL;
                    
                    /* Extract lower and upper bound strings, handling various AST structures:
                     * - Simple literals/identifiers (e.g., "1", "N") have sym->name set
                     * - Unary expressions (e.g., "-1") are PASCAL_T_NEG
                     * - Binary expressions (e.g., "N-1") are PASCAL_T_SUB
                     * Use serialize_expr_to_string to handle all cases uniformly */
                    char *lower_str = serialize_expr_to_string(lower);
                    char *upper_str = serialize_expr_to_string(upper);
                    int lower_val = 0;
                    int upper_val = 0;
                    int lower_ok = (evaluate_const_int_expr(lower, &lower_val, 0) == 0);
                    int upper_ok = (evaluate_const_int_expr(upper, &upper_val, 0) == 0);

                    if (lower_str == NULL && lower_ok) {
                        char buffer[64];
                        snprintf(buffer, sizeof(buffer), "%d", lower_val);
                        lower_str = strdup(buffer);
                    }
                    if (upper_str == NULL && upper_ok) {
                        char buffer[64];
                        snprintf(buffer, sizeof(buffer), "%d", upper_val);
                        upper_str = strdup(buffer);
                    }

                    if ((lower_str == NULL || upper_str == NULL) &&
                        kgpc_getenv("KGPC_DEBUG_ARRAY_BOUNDS") != NULL)
                    {
                        fprintf(stderr,
                            "[KGPC] array bounds serialize failed: lower typ=%d (%s) upper typ=%d (%s)\n",
                            lower ? lower->typ : -1,
                            lower ? pascal_tag_to_string(lower->typ) : "<null>",
                            upper ? upper->typ : -1,
                            upper ? pascal_tag_to_string(upper->typ) : "<null>");
                    }

                    if (lower_str != NULL && upper_str != NULL) {
                        if (dims_builder.head == NULL) {
                            int start_val = 0;
                            int end_val = 0;
                            if (lower_ok)
                                start_val = lower_val;
                            else if (resolve_const_expr_from_sections(lower_str, &start_val) != 0)
                                start_val = atoi(lower_str);
                            if (upper_ok)
                                end_val = upper_val;
                            else if (resolve_const_expr_from_sections(upper_str, &end_val) != 0)
                                end_val = atoi(upper_str);
                            type_info->start = start_val;
                            type_info->end = end_val;
                            /* Store structured bounds for semcheck */
                            type_info->array_dim_start_str = strdup(lower_str);
                            type_info->array_dim_end_str = strdup(upper_str);
                            type_info->array_dims_parsed = 1;
                        }
                        char buffer[128];
                        snprintf(buffer, sizeof(buffer), "%s..%s", lower_str, upper_str);
                        list_builder_append(&dims_builder, strdup(buffer), LIST_STRING);
                        free(lower_str);
                        free(upper_str);
                    } else {
                        if (lower_str != NULL) free(lower_str);
                        if (upper_str != NULL) free(upper_str);
                        type_info->is_open_array = 1;
                    }
                } else if (dim->typ == PASCAL_T_IDENTIFIER) {
                    // Single identifier as array dimension (e.g., array[color], array[boolean])
                    // This is NOT an open array - it's an array indexed by an enum or subrange type
                    char *dim_name = dup_symbol(dim);
                    if (dim_name != NULL) {
                        // Check if this is the first dimension (no nodes appended yet)
                        int is_first_dim = (dims_builder.head == NULL);
                        list_builder_append(&dims_builder, dim_name, LIST_STRING);
                        // Set bounds based on the dimension type
                        // For boolean: 0..1 (False=0, True=1)
                        if (is_first_dim) {
                            // This is the first dimension - set bounds
                            if (strcasecmp(dim_name, "boolean") == 0) {
                                type_info->start = 0;
                                type_info->end = 1;
                            } else if (strcasecmp(dim_name, "char") == 0) {
                                type_info->start = 0;
                                type_info->end = 255;
                            } else if (strcasecmp(dim_name, "byte") == 0) {
                                type_info->start = 0;
                                type_info->end = 255;
                            } else if (strcasecmp(dim_name, "shortint") == 0) {
                                type_info->start = -128;
                                type_info->end = 127;
                            } else {
                                // For other types (enums, etc.), we'll need to resolve them later
                                // For now, mark as needing resolution but set safe bounds
                                // to avoid being marked as open array
                                type_info->start = 0;
                                type_info->end = 0;  // Will be resolved in semantic checking
                            }
                        }
                    }
                    // Note: is_open_array is NOT set to 1 here
                } else {
                    type_info->is_open_array = 1;
                }
            }

            type_info->array_dimensions = list_builder_finish(&dims_builder);

            if (element_node != NULL) {
                if (element_node->typ == PASCAL_T_IDENTIFIER) {
                    char *dup = dup_symbol(element_node);
                    if (dup != NULL && strcasecmp(dup, "const") == 0) {
                        type_info->is_array_of_const = 1;
                        type_info->element_type = ARRAY_OF_CONST_TYPE;
                        type_info->is_open_array = 1;
                        free(dup);
                    } else {
                        int mapped = map_type_name(dup, &type_info->element_type_id);
                        type_info->element_type = mapped;
                        if (mapped == UNKNOWN_TYPE && type_info->element_type_id == NULL)
                            type_info->element_type_id = dup;
                        else
                            free(dup);
                    }
                } else if (element_node->typ == PASCAL_T_TYPE_SPEC) {
                    char *nested_id = NULL;
                    struct RecordType *nested_record = NULL;
                    TypeInfo nested_info = {0};
                    int mapped = convert_type_spec(element_node, &nested_id, &nested_record, &nested_info);
                    /* Handle shortstring element types (e.g., string[10]) */
                    if (nested_info.is_shortstring) {
                        type_info->element_type = SHORTSTRING_TYPE;
                        if (type_info->element_type_id == NULL)
                            type_info->element_type_id = strdup("ShortString");
                        /* Preserve the shortstring capacity so codegen uses
                           the correct element size (N+1 bytes for string[N])
                           instead of defaulting to 256. */
                        if (nested_info.end > 0 && type_info->element_kgpc_type == NULL) {
                            type_info->element_kgpc_type =
                                create_primitive_type_with_size(SHORTSTRING_TYPE,
                                                               nested_info.end + 1);
                        }
                        if (nested_id != NULL)
                            free(nested_id);
                    } else if (nested_info.is_range && mapped == UNKNOWN_TYPE) {
                        /* Subrange element type (e.g., array[0..15] of 0..15).
                         * The element is a range type wrapped in TYPE_SPEC —
                         * treat as integer. */
                        type_info->element_type = INT_TYPE;
                        if (nested_id != NULL)
                            free(nested_id);
                    } else {
                        type_info->element_type = mapped;
                        if (type_info->element_type_id == NULL)
                            type_info->element_type_id = nested_id;
                        else if (nested_id != NULL)
                            free(nested_id);
                    }
                    if (nested_record != NULL)
                    {
                        type_info->record_type = nested_record;
                        type_info->is_record = 1;
                        type_info->element_type = RECORD_TYPE;
                    }
                    /* Build KgpcType for nested array elements (array of array of ...) */
                    if (nested_info.is_array)
                    {
                        KgpcType *inner_elem = NULL;
                        if (nested_info.element_kgpc_type != NULL)
                        {
                            /* Deeper nesting: element is already a pre-built KgpcType */
                            inner_elem = nested_info.element_kgpc_type;
                            kgpc_type_retain(inner_elem);
                        }
                        else if (nested_info.record_type != NULL)
                        {
                            inner_elem = create_record_type(clone_record_type(nested_info.record_type));
                        }
                        else if (nested_info.element_type != UNKNOWN_TYPE)
                        {
                            inner_elem = create_primitive_type(nested_info.element_type);
                        }
                        /* Create the inner array KgpcType.
                         * If inner_elem is NULL but element_type_id is set, create a
                         * deferred-resolution array type */
                        if (inner_elem != NULL)
                        {
                            type_info->element_kgpc_type = create_array_type(
                                inner_elem, nested_info.start, nested_info.end);
                        }
                        else if (nested_info.element_type_id != NULL)
                        {
                            /* Named element type: create array with deferred element resolution */
                            type_info->element_kgpc_type = create_array_type(
                                NULL, nested_info.start, nested_info.end);
                            if (type_info->element_kgpc_type != NULL)
                                type_info->element_kgpc_type->info.array_info.element_type_id =
                                    strdup(nested_info.element_type_id);
                        }
                    }
                    destroy_type_info_contents(&nested_info);
                }
                if (type_info->element_type_id != NULL &&
                    strcasecmp(type_info->element_type_id, "const") == 0)
                {
                    free(type_info->element_type_id);
                    type_info->element_type_id = NULL;
                    type_info->is_array_of_const = 1;
                    type_info->element_type = ARRAY_OF_CONST_TYPE;
                    type_info->is_open_array = 1;
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
                else
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
                    else
                        free(dup);
                }
                /* Handle subrange as set element type: set of 0..31 */
                else if (elem->typ == PASCAL_T_RANGE_TYPE) {
                    ast_t *lower = unwrap_pascal_node(elem->child);
                    ast_t *upper = (lower != NULL) ? unwrap_pascal_node(lower->next) : NULL;
                    long long start_value = 0;
                    long long end_value = 0;
                    int have_start = 0;
                    int have_end = 0;
                    struct Expression *lower_expr = convert_expression(lower);
                    struct Expression *upper_expr = convert_expression(upper);
                    if (lower_expr != NULL) {
                        have_start = (extract_constant_int(lower_expr, &start_value) == 0);
                        destroy_expr(lower_expr);
                    }
                    if (upper_expr != NULL) {
                        have_end = (extract_constant_int(upper_expr, &end_value) == 0);
                        destroy_expr(upper_expr);
                    }
                    if (!have_start && elem->child != NULL) {
                        int val = 0;
                        if (evaluate_const_int_expr(elem->child, &val, 0) == 0) {
                            start_value = val;
                            have_start = 1;
                        }
                    }
                    if (!have_end && elem->child != NULL) {
                        int val = 0;
                        if (evaluate_const_int_expr(elem->child->next, &val, 0) == 0) {
                            end_value = val;
                            have_end = 1;
                        }
                    }
                    if (have_start && have_end) {
                        type_info->range_known = 1;
                        type_info->range_start = start_value;
                        type_info->range_end = end_value;
                    }
                    type_info->set_element_type = INT_TYPE;
                }
                /* Handle anonymous enum as set element type: set of (val1, val2, ...) */
                else if (elem->typ == PASCAL_T_ENUMERATED_TYPE) {
                    type_info->is_enum_set = 1;
                    type_info->set_element_type = ENUM_TYPE;
                    ListBuilder enum_builder;
                    list_builder_init(&enum_builder);
                    ast_t *value = elem->child;
                    while (value != NULL) {
                        if (value->typ == PASCAL_T_IDENTIFIER)
                            list_builder_append(&enum_builder, dup_symbol(value), LIST_STRING);
                        value = value->next;
                    }
                    type_info->inline_enum_values = list_builder_finish(&enum_builder);
                }
            }
        }
        return SET_TYPE;
    }

    if (spec_node->typ == PASCAL_T_FILE_TYPE) {
        if (type_info != NULL) {
            type_info->is_file = 1;
            ast_t *elem = spec_node->child;
            while (elem != NULL && (elem->typ == PASCAL_T_NONE || elem->typ == PASCAL_T_TYPE_SPEC))
                elem = elem->child;
            if (elem != NULL && elem->typ == PASCAL_T_IDENTIFIER) {
                char *dup = dup_symbol(elem);
                int mapped = map_type_name(dup, &type_info->file_type_id);
                type_info->file_type = mapped;
                if (mapped == UNKNOWN_TYPE && type_info->file_type_id == NULL)
                    type_info->file_type_id = dup;
                else
                    free(dup);
            }
        }
        return FILE_TYPE;
    }

    if (spec_node->typ == PASCAL_T_ENUMERATED_TYPE) {
        if (type_info != NULL) {
            type_info->is_enum = 1;
            type_info->enum_is_scoped = from_cparser_scopedenums_enabled_at_line(spec_node->line);
            ListBuilder enum_builder;
            list_builder_init(&enum_builder);
            ast_t *value = spec_node->child;
            while (value != NULL) {
                if (value->typ == PASCAL_T_IDENTIFIER)
                    list_builder_append(&enum_builder, dup_symbol(value), LIST_STRING);
                else if (value->typ == PASCAL_T_ASSIGNMENT &&
                         value->child != NULL && value->child->typ == PASCAL_T_IDENTIFIER) {
                    list_builder_append(&enum_builder, dup_symbol(value->child), LIST_STRING);
                    type_info->enum_has_explicit_values = 1;
                }
                value = value->next;
            }
            type_info->enum_literals = list_builder_finish(&enum_builder);
        }
        return ENUM_TYPE;
    }

    if (spec_node->typ == PASCAL_T_PROCEDURE_TYPE || spec_node->typ == PASCAL_T_FUNCTION_TYPE) {
        // Note: For now, we return PROCEDURE as the type tag for both procedures and functions
        // The actual KgpcType object will be created later in the semantic checker
        // This is a temporary bridge solution until full migration to KgpcType
        return PROCEDURE;
    }

    if (spec_node->typ == PASCAL_T_REFERENCE_TO_TYPE) {
        // Reference to type wraps a procedure/function type
        // For now, treat it the same as PROCEDURE_TYPE/FUNCTION_TYPE
        return PROCEDURE;
    }

    if (spec_node->typ == PASCAL_T_RECORD_TYPE || spec_node->typ == PASCAL_T_OBJECT_TYPE) {
        struct RecordType *record = convert_record_type(spec_node);
        if (type_info != NULL) {
            type_info->is_record = 1;
            type_info->record_type = clone_record_type(record);
        }
        if (record_out != NULL) {
            *record_out = record;
        } else {
            destroy_record_type(record);
        }
        return RECORD_TYPE;
    }

    /* Distinct type: type Real = Double - creates a strong alias.
     * The target may be a full type expression (e.g. "type ^TFoo"),
     * not just an identifier, so recurse through convert_type_spec. */
    if (spec_node->typ == PASCAL_T_DISTINCT_TYPE) {
        /* The child is the target type spec */
        ast_t *target = spec_node->child;
        if (target != NULL) {
            target = unwrap_pascal_node(target);
            if (target != NULL) {
                TypeInfo nested_info = {0};
                struct RecordType *nested_record = NULL;
                char *nested_type_id = NULL;
                int result = convert_type_spec(target, &nested_type_id, &nested_record, &nested_info);

                if (type_info != NULL) {
                    /* Preserve full structural metadata from the distinct target.
                     * Distinct aliases remain strong aliases semantically, but they
                     * must still carry pointer/array/etc. shape for semcheck/codegen. */
                    destroy_type_info_contents(type_info);
                    *type_info = nested_info;
                    memset(&nested_info, 0, sizeof(nested_info));
                }

                if (record_out != NULL && nested_record != NULL) {
                    *record_out = nested_record;
                    nested_record = NULL;
                }

                if (type_id_out != NULL && *type_id_out == NULL && nested_type_id != NULL) {
                    *type_id_out = nested_type_id;
                    nested_type_id = NULL;
                }

                if (nested_type_id != NULL)
                    free(nested_type_id);
                if (nested_record != NULL)
                    destroy_record_type(nested_record);
                destroy_type_info_contents(&nested_info);
                return result;
            }
        }
        /* Malformed distinct type target */
        return UNKNOWN_TYPE;
    }

    /* Handle "class of T" - class reference type */
    if (spec_node->typ == PASCAL_T_CLASS_OF_TYPE) {
        /* Class reference type (class of TMyClass).
         * In Pascal, when T is a class type, T is already a pointer to the class record.
         * "class of T" is semantically a reference to the class itself (metaclass),
         * but structurally it's the same as T - a pointer to the class record.
         * 
         * We treat "class of T" the same as T, which allows:
         *   var ClassRef: class of TMyClass;
         *   ClassRef := TMyClass;  // Works because both are ^record
         */
        if (type_info != NULL) {
            type_info->is_class_reference = 1;
            ast_t *target = spec_node->child;
            while (target != NULL && target->typ != PASCAL_T_IDENTIFIER)
                target = target->next;
            if (target != NULL) {
                char *dup = dup_symbol(target);
                /* Check if this is a builtin type (non-class) - reject it */
                int mapped = map_type_name(dup, NULL);
                if (mapped != UNKNOWN_TYPE) {
                    /* Builtin types like Integer, String, etc. are not class types */
                    frontend_error("Error: 'class of' requires a class type, but got %s", dup);
                    free(dup);
                    return UNKNOWN_TYPE;
                }
                /* Not a builtin type - assume it's a class type.
                 * Further validation will happen during semantic checking. */
                type_info->pointer_type_id = dup;
                if (type_id_out != NULL && *type_id_out == NULL)
                    *type_id_out = strdup(dup);
                type_info->is_pointer = 1;
                type_info->pointer_type = RECORD_TYPE;
            }
        }
        return POINTER_TYPE;
    }

    return UNKNOWN_TYPE;
}

/* Forward declare functions we need */

/* Convert an AST type specification to a KgpcType object.
 * This is the Phase 2 bridge function that creates KgpcType objects from AST nodes.
 * Returns NULL if the type cannot be converted or if symtab is needed but not provided.
 */
KgpcType *convert_type_spec_to_kgpctype(ast_t *type_spec, struct SymTab *symtab) {
    if (type_spec == NULL)
        return NULL;

    ast_t *spec_node = type_spec;
    if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
        spec_node = spec_node->child;
    spec_node = unwrap_pascal_node(spec_node);

    if (spec_node == NULL)
        return NULL;

    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
        fprintf(stderr, "[KGPC] convert_type_spec_to_kgpctype node typ=%d sym=%s\n",
            spec_node->typ,
            (spec_node->sym != NULL && spec_node->sym->name != NULL) ? spec_node->sym->name : "<null>");
    /* Generic type specializations: constructed TFoo<T> or \"specialize TFoo<T>\" */
    {
        char *base_name = NULL;
        ListNode_t *type_args = NULL;
        int is_generic = 0;

        if (spec_node->typ == PASCAL_T_CONSTRUCTED_TYPE) {
            is_generic = extract_constructed_type_info(type_spec, &base_name, &type_args);
        } else {
            is_generic = extract_specialize_type_info(type_spec, &base_name, &type_args);
        }

        if (is_generic) {
            char *specialized_name = NULL;
            struct RecordType *record = instantiate_generic_record(base_name, type_args, &specialized_name);
            free(base_name);
            if (type_args != NULL)
                destroy_list(type_args);
            if (specialized_name != NULL)
                free(specialized_name);
            if (record != NULL) {
                return create_record_type(record);
            }
        }
    }

    /* Handle primitive types by identifier */
    if (spec_node->typ == PASCAL_T_IDENTIFIER) {
        char *type_name = dup_symbol(spec_node);
        if (type_name == NULL)
            return NULL;

        /* Get the preserved type name for RawByteString/UnicodeString */
        char *preserved_type_id = NULL;
        int type_tag = map_type_name(type_name, &preserved_type_id);
        type_tag = apply_shortstring_mode(type_tag, type_name);
        if (type_tag != UNKNOWN_TYPE) {
            KgpcType *type = create_primitive_type(type_tag);
            /* Preserve distinct string-family aliases needed for helper lookup and overloads. */
            if (type != NULL && preserved_type_id != NULL &&
                (strcasecmp(preserved_type_id, "AnsiString") == 0 ||
                 strcasecmp(preserved_type_id, "RawByteString") == 0 ||
                 strcasecmp(preserved_type_id, "UnicodeString") == 0 ||
                 strcasecmp(preserved_type_id, "WideString") == 0)) {
                /* Create a TypeAlias to preserve the original type name */
                struct TypeAlias *alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
                if (alias != NULL) {
                    alias->alias_name = strdup(preserved_type_id);
                    alias->base_type = type_tag;
                    /* Set type_alias on the KgpcType */
                    kgpc_type_set_type_alias(type, alias);
                    /* Free the temporary alias after copying */
                    free(alias->alias_name);
                    free(alias);
                }
            }
            free(type_name);
            free(preserved_type_id);
            return type;
        }
        
        free(type_name);
        free(preserved_type_id);

        /* If unknown type and we have a symbol table, try to look it up */
        if (symtab != NULL) {
            /* This will be resolved later during semantic checking */
            /* For now, we need to look up the type in the symbol table */
            /* But that requires SymTab.h which creates circular dependency */
            /* So we return NULL and let the caller handle named type lookup */
            return NULL;
        }

        return NULL;
    }
    if (spec_node->typ == PASCAL_T_CONSTRUCTED_TYPE) {
        char *base_name = NULL;
        ListNode_t *type_args = NULL;
        if (extract_constructed_type_info(spec_node, &base_name, &type_args)) {
            char *specialized_name = NULL;
            struct RecordType *record = instantiate_generic_record(base_name, type_args, &specialized_name);
            free(base_name);
            if (record != NULL) {
                free(specialized_name);
                return create_record_type(record);
            }
            if (specialized_name != NULL)
                free(specialized_name);
        }
    }

    /* Handle array types */
    if (spec_node->typ == PASCAL_T_ARRAY_TYPE) {
        ast_t *child = spec_node->child;
        ast_t *element_node = child;
        
        /* Find the element type node (last child) */
        while (element_node != NULL && element_node->next != NULL)
            element_node = element_node->next;

        /* Get range if available (first dimension only for now) */
        int start = 0, end = -1;
        if (child != NULL && child->typ == PASCAL_T_RANGE_TYPE) {
            ast_t *lower = child->child;
            ast_t *upper = (lower != NULL) ? lower->next : NULL;
            if (lower != NULL && upper != NULL && lower->sym != NULL && upper->sym != NULL) {
                start = atoi(lower->sym->name);
                end = atoi(upper->sym->name);
            }
        }

        /* Recursively convert element type */
        KgpcType *element_type = convert_type_spec_to_kgpctype(element_node, symtab);
        if (element_type == NULL) {
            /* Try to get a primitive type from identifier */
            if (element_node != NULL && element_node->typ == PASCAL_T_IDENTIFIER) {
                char *elem_type_name = dup_symbol(element_node);
                if (elem_type_name != NULL) {
                    int elem_tag = map_type_name(elem_type_name, NULL);
                    free(elem_type_name);
                    if (elem_tag != UNKNOWN_TYPE) {
                        element_type = create_primitive_type(elem_tag);
                    }
                }
            }
        }

        if (element_type == NULL)
            return NULL;

        {
            KgpcType *arr = create_array_type(element_type, start, end);
            kgpc_type_release(element_type);
            return arr;
        }
    }

    /* Handle file types */
    if (spec_node->typ == PASCAL_T_FILE_TYPE) {
        ast_t *elem = spec_node->child;
        while (elem != NULL && (elem->typ == PASCAL_T_NONE || elem->typ == PASCAL_T_TYPE_SPEC))
            elem = elem->child;

        if (elem == NULL) {
            /* Untyped file - represented as primitive FILE_TYPE */
            return create_primitive_type(FILE_TYPE);
        }

        /* Typed file: attempt to convert element type */
        KgpcType *element_type = convert_type_spec_to_kgpctype(elem, symtab);
        if (element_type == NULL && elem->typ == PASCAL_T_IDENTIFIER) {
            char *elem_name = dup_symbol(elem);
            if (elem_name != NULL) {
                int elem_tag = map_type_name(elem_name, NULL);
                free(elem_name);
                if (elem_tag != UNKNOWN_TYPE) {
                    element_type = create_primitive_type(elem_tag);
                }
            }
        }

        if (element_type == NULL) {
            /* Fallback: treat as generic file */
            return create_primitive_type(FILE_TYPE);
        }

        /* For now, represent file types as primitive FILE_TYPE with alias metadata.
         * The detailed element type is tracked in the TypeAlias/TypeInfo structures. */
        destroy_kgpc_type(element_type);
        return create_primitive_type(FILE_TYPE);
    }

    /* Handle pointer types */
    if (spec_node->typ == PASCAL_T_POINTER_TYPE) {
        ast_t *target = spec_node->child;
        while (target != NULL && target->typ != PASCAL_T_IDENTIFIER && target->typ != PASCAL_T_TYPE_SPEC)
            target = target->next;

        if (target != NULL) {
            KgpcType *points_to = convert_type_spec_to_kgpctype(target, symtab);
            if (points_to == NULL) {
                /* Try primitive type lookup */
                if (target->typ == PASCAL_T_IDENTIFIER) {
                    char *target_name = dup_symbol(target);
                    if (target_name != NULL) {
                        int target_tag = map_type_name(target_name, NULL);
                        free(target_name);
                        if (target_tag != UNKNOWN_TYPE) {
                            points_to = create_primitive_type(target_tag);
                        }
                    }
                }
            }

            KgpcType *ptr = create_pointer_type(points_to);
            if (points_to != NULL) kgpc_type_release(points_to);
            return ptr;
        }
        return create_pointer_type(NULL);
    }

    /* Handle procedure and function types */
    if (spec_node->typ == PASCAL_T_PROCEDURE_TYPE || spec_node->typ == PASCAL_T_FUNCTION_TYPE) {
        int is_function = (spec_node->typ == PASCAL_T_FUNCTION_TYPE);
        
        #ifdef DEBUG_KGPC_TYPE_CREATION
        fprintf(stderr, "DEBUG: convert_type_spec_to_kgpctype: handling %s\n",
                is_function ? "FUNCTION_TYPE" : "PROCEDURE_TYPE");
        #endif
        
        /* Parse parameters */
        ast_t *cursor = spec_node->child;
        ListNode_t *params = NULL;
        
        /* Check if first child is a PARAM_LIST */
        if (cursor != NULL && cursor->typ == PASCAL_T_PARAM_LIST) {
            #ifdef DEBUG_KGPC_TYPE_CREATION
            fprintf(stderr, "DEBUG: Found PARAM_LIST node\n");
            #endif
            /* The PARAM_LIST node should contain PARAM children */
            ast_t *param_cursor = cursor->child;
            params = convert_param_list(&param_cursor);
            /* Move cursor to the next sibling after PARAM_LIST */
            cursor = cursor->next;
            #ifdef DEBUG_KGPC_TYPE_CREATION
            fprintf(stderr, "DEBUG: After PARAM_LIST, cursor=%p, cursor->typ=%d\n",
                    (void*)cursor, cursor ? cursor->typ : -1);
            #endif
        } else {
            /* Skip to parameter list if present, but stop at return type (for parameterless functions) */
            while (cursor != NULL && 
                   cursor->typ != PASCAL_T_PARAM && 
                   cursor->typ != PASCAL_T_TYPE_SPEC &&
                   cursor->typ != PASCAL_T_RETURN_TYPE)
                cursor = cursor->next;
            
            if (cursor != NULL && cursor->typ == PASCAL_T_PARAM) {
                params = convert_param_list(&cursor);
            }
        }
        
        /* For functions, get return type */
        KgpcType *return_type = NULL;
        char *return_type_id = NULL;
        int owns_return_type = 0;
        if (is_function) {
            #ifdef DEBUG_KGPC_TYPE_CREATION
            fprintf(stderr, "DEBUG: Looking for return type, cursor=%p, cursor->typ=%d, cursor->sym=%s, cursor->child=%p\n",
                    (void*)cursor, cursor ? cursor->typ : -1,
                    (cursor && cursor->sym && cursor->sym->name) ? cursor->sym->name : "NULL",
                    cursor ? (void*)cursor->child : NULL);
            if (cursor && cursor->child) {
                fprintf(stderr, "DEBUG: cursor->child->typ=%d, cursor->child->sym=%s\n",
                        cursor->child->typ,
                        (cursor->child->sym && cursor->child->sym->name) ? cursor->child->sym->name : "NULL");
            }
            #endif
            /* Look for return type specification */
            /* The return type might be:
             * 1. A direct sibling (PASCAL_T_TYPE_SPEC or PASCAL_T_IDENTIFIER)
             * 2. A child of an intermediate wrapper node (check child->typ)
             * 3. Wrapped in PASCAL_T_RETURN_TYPE node
             */
            while (cursor != NULL && cursor->typ != PASCAL_T_TYPE_SPEC && cursor->typ != PASCAL_T_IDENTIFIER)
            {
                /* Check for RETURN_TYPE wrapper */
                if (cursor->typ == PASCAL_T_RETURN_TYPE) {
                    if (cursor->child != NULL &&
                        (cursor->child->typ == PASCAL_T_TYPE_SPEC || cursor->child->typ == PASCAL_T_IDENTIFIER))
                    {
                        cursor = cursor->child;
                        break;
                    }
                }
                /* Check if the child is a TYPE_SPEC or IDENTIFIER */
                if (cursor->child != NULL && 
                    (cursor->child->typ == PASCAL_T_TYPE_SPEC || cursor->child->typ == PASCAL_T_IDENTIFIER))
                {
                    cursor = cursor->child;
                    break;
                }
                #ifdef DEBUG_KGPC_TYPE_CREATION
                fprintf(stderr, "DEBUG: Skipping node type %d (PASCAL_T_TYPE_SPEC=%d, PASCAL_T_IDENTIFIER=%d)\n", 
                        cursor->typ, PASCAL_T_TYPE_SPEC, PASCAL_T_IDENTIFIER);
                #endif
                cursor = cursor->next;
            }

            if (cursor == NULL)
            {
                /* Fallback: scan the function type node for a return type */
                ast_t *scan = spec_node->child;
                while (scan != NULL)
                {
                    ast_t *node = unwrap_pascal_node(scan);
                    if (node != NULL)
                    {
                        if (node->typ == PASCAL_T_RETURN_TYPE)
                        {
                            if (node->child != NULL)
                            {
                                cursor = node->child;
                                break;
                            }
                        }
                        else if (node->typ == PASCAL_T_TYPE_SPEC || node->typ == PASCAL_T_IDENTIFIER)
                        {
                            cursor = node;
                            break;
                        }
                    }
                    scan = scan->next;
                }
            }
                
            if (cursor != NULL) {
                #ifdef DEBUG_KGPC_TYPE_CREATION
                fprintf(stderr, "DEBUG: Found return type node, typ=%d\n", cursor->typ);
                #endif
                if (cursor->typ == PASCAL_T_TYPE_SPEC) {
                    return_type = convert_type_spec_to_kgpctype(cursor, symtab);
                    owns_return_type = (return_type != NULL);
                    if (return_type_id == NULL && cursor->child != NULL &&
                        cursor->child->sym != NULL && cursor->child->sym->name != NULL)
                    {
                        return_type_id = strdup(cursor->child->sym->name);
                    }
                } else if (cursor->typ == PASCAL_T_IDENTIFIER) {
                    char *ret_type_name = dup_symbol(cursor);
                    if (ret_type_name != NULL) {
                        // First check if it's a primitive type
                        int ret_tag = map_type_name(ret_type_name, NULL);
                        ret_tag = apply_shortstring_mode(ret_tag, ret_type_name);
                        if (ret_tag != UNKNOWN_TYPE) {
                            return_type = create_primitive_type(ret_tag);
                            owns_return_type = 1;
                        } else {
                            // Check if it's a user-defined type in the symbol table
                            HashNode_t *type_node = NULL;
                            if (symtab != NULL && FindSymbol(&type_node, symtab, ret_type_name) != 0 &&
                                type_node != NULL && type_node->type != NULL) {
                                return_type = type_node->type;
                            }
                        }
                        if (return_type_id == NULL)
                            return_type_id = strdup(ret_type_name);
                        free(ret_type_name);
                    }
                }
            }
            #ifdef DEBUG_KGPC_TYPE_CREATION
            else {
                fprintf(stderr, "DEBUG: No return type node found!\n");
            }
            #endif
        }
        
        KgpcType *proc_type = create_procedure_type(params, return_type);
        /* create_procedure_type retains return_type; release our ref if we own it */
        if (owns_return_type && return_type != NULL)
            kgpc_type_release(return_type);
        if (proc_type != NULL) {
            /* create_procedure_type makes a shallow copy of params; tell the
             * type to own (deeply free) its copy so the TREE_VAR_DECL param
             * nodes are eventually freed.  Then free the original list nodes. */
            proc_type->info.proc_info.owns_params = 1;
            DestroyList(params);
            if (return_type_id != NULL)
                proc_type->info.proc_info.return_type_id = return_type_id;
        } else {
            destroy_list(params);
            if (return_type_id != NULL)
                free(return_type_id);
        }
        return proc_type;
    }

    /* Handle reference to types (wraps procedure/function types) */
    if (spec_node->typ == PASCAL_T_REFERENCE_TO_TYPE) {
        // The child should be a PROCEDURE_TYPE or FUNCTION_TYPE
        ast_t *wrapped_type = spec_node->child;
        if (wrapped_type != NULL) {
            return convert_type_spec_to_kgpctype(wrapped_type, symtab);
        }
        return NULL;
    }

    /* Handle record types and legacy object types */
    if (spec_node->typ == PASCAL_T_RECORD_TYPE || spec_node->typ == PASCAL_T_OBJECT_TYPE) {
        struct RecordType *record = convert_record_type(spec_node);
        if (record != NULL) {
            return create_record_type(record);
        }
        return NULL;
    }

    /* Handle interface types */
    if (spec_node->typ == PASCAL_T_INTERFACE_TYPE) {
        struct RecordType *record = convert_interface_type_ex(NULL, spec_node, NULL);
        if (record != NULL) {
            KgpcType *rec_type = create_record_type(record);
            if (rec_type != NULL) {
                KgpcType *ptr = create_pointer_type(rec_type);
                kgpc_type_release(rec_type);
                return ptr;
            }
        }
        return NULL;
    }

    /* Handle set types */
    if (spec_node->typ == PASCAL_T_SET) {
        /* For sets, we currently just return a primitive SET_TYPE */
        /* In the future, we could extend KgpcType to include set element type info */
        return create_primitive_type(SET_TYPE);
    }

    /* Handle enum types */
    if (spec_node->typ == PASCAL_T_ENUMERATED_TYPE) {
        /* For enums, we currently just return a primitive ENUM_TYPE */
        return create_primitive_type(ENUM_TYPE);
    }

    /* Handle class types */
    if (spec_node->typ == PASCAL_T_CLASS_TYPE) {
        /* Note: We can't get the class name here because we're inside the type spec, not the type decl.
         * For now, we return NULL to let the class be handled by the legacy path.
         * The class's RecordType will be properly populated during semantic checking. */
        return NULL;
    }

    /* Handle "class of T" - class reference type */
    if (spec_node->typ == PASCAL_T_CLASS_OF_TYPE) {
        /* Class reference type (class of TMyClass).
         * This is structurally the same as the target class type - a pointer to the class record.
         * Return NULL here to let it be handled by the convert_type_spec path which creates
         * the appropriate pointer type. */
        return NULL;
    }

    return NULL;
}

