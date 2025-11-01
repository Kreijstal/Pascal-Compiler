#include "pascal_type.h"
#include "pascal_parser.h"
#include "pascal_keywords.h"
#include "pascal_expression.h"
#include "pascal_declaration.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

static void set_combinator_name(combinator_t* comb, const char* name) {
    if (comb == NULL)
        return;

    if (comb->name != NULL) {
        free(comb->name);
    }
    comb->name = strdup(name);
}

static inline void discard_failure(ParseResult result) {
    if (!result.is_success) {
        free_error(result.value.error);
    }
}

static ParseResult fail_with_message(const char* message, input_t* in, InputState* state, char* parser_name) {
    restore_input_state(in, state);
    return make_failure_v2(in, parser_name, strdup(message), NULL);
}

// Range type parser: reuse expression parser and re-tag range AST nodes
static ParseResult range_type_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    combinator_t* expr_parser = new_combinator();
    init_pascal_expression_parser(&expr_parser);

    ParseResult expr_result = parse(in, expr_parser);
    free_combinator(expr_parser);

    if (!expr_result.is_success) {
        discard_failure(expr_result);
        return fail_with_message("Expected range expression", in, &state, parser_name);
    }

    ast_t* expr_ast = expr_result.value.ast;
    if (expr_ast == NULL || expr_ast->typ != PASCAL_T_RANGE || expr_ast->child == NULL || expr_ast->child->next == NULL) {
        free_ast(expr_ast);
        return fail_with_message("Invalid range expression", in, &state, parser_name);
    }

    expr_ast->typ = pargs->tag;
    set_ast_position(expr_ast, in);
    return make_success(expr_ast);
}

combinator_t* range_type(tag_t tag) {
    combinator_t* comb = new_combinator();
    prim_args* args = safe_malloc(sizeof(prim_args));
    args->tag = tag;
    comb->args = args;
    comb->fn = range_type_fn;
    return comb;
}

// Array type parser: ARRAY[range1,range2,...] OF element_type
static ParseResult array_type_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    bool is_packed = false;
    combinator_t* packed_keyword = token(keyword_ci("packed"));
    ParseResult packed_res = parse(in, packed_keyword);
    if (packed_res.is_success) {
        is_packed = true;
        free_ast(packed_res.value.ast);
    } else {
        discard_failure(packed_res);
    }
    free_combinator(packed_keyword);

    // Parse "ARRAY" keyword (case insensitive)
    combinator_t* array_keyword = token(keyword_ci("array"));
    ParseResult array_res = parse(in, array_keyword);
    if (!array_res.is_success) {
        discard_failure(array_res);
        free_combinator(array_keyword);
        return fail_with_message("Expected 'array'", in, &state, parser_name);
    }
    free_ast(array_res.value.ast);
    free_combinator(array_keyword);

    bool has_indices = false;
    ast_t* indices_ast = NULL;

    // Parse optional [ ... ] section for static array bounds.
    combinator_t* open_bracket = token(match("["));
    ParseResult open_res = parse(in, open_bracket);
    if (open_res.is_success) {
        has_indices = true;
        free_ast(open_res.value.ast);
    } else {
        discard_failure(open_res);
    }
    free_combinator(open_bracket);

    if (has_indices) {
        // Parse ranges/indices (simplified - just accept any identifiers/ranges for now)
        combinator_t* array_index = multi(new_combinator(), PASCAL_T_NONE,
            range_type(PASCAL_T_RANGE_TYPE),
            token(cident(PASCAL_T_IDENTIFIER)),
            NULL
        );
        combinator_t* index_list = sep_by(array_index, token(match(",")));
        ParseResult indices_res = parse(in, index_list);
        if (indices_res.is_success) {
            indices_ast = indices_res.value.ast;
        } else {
            discard_failure(indices_res);
            free_combinator(index_list);
            return fail_with_message("Expected array indices", in, &state, parser_name);
        }
        free_combinator(index_list);

        // Parse ]
        combinator_t* close_bracket = token(match("]"));
        ParseResult close_res = parse(in, close_bracket);
        if (!close_res.is_success) {
            discard_failure(close_res);
            free_ast(indices_ast);
            free_combinator(close_bracket);
            return fail_with_message("Expected ']'", in, &state, parser_name);
        }
        free_ast(close_res.value.ast);
        free_combinator(close_bracket);
    }

    // Parse OF
    combinator_t* of_keyword = token(keyword_ci("of"));
    ParseResult of_res = parse(in, of_keyword);
    if (!of_res.is_success) {
        discard_failure(of_res);
        free_ast(indices_ast);
        free_combinator(of_keyword);
        return fail_with_message("Expected 'OF' after array indices", in, &state, parser_name);
    }
    free_ast(of_res.value.ast);
    free_combinator(of_keyword);

    // Parse element type (simplified)
    combinator_t* element_type = token(cident(PASCAL_T_IDENTIFIER));
    ParseResult elem_res = parse(in, element_type);
    ast_t* element_ast = NULL;
    if (elem_res.is_success) {
        element_ast = elem_res.value.ast;
    } else {
        discard_failure(elem_res);
        free_ast(indices_ast);
        free_combinator(element_type);
        return fail_with_message("Expected element type after 'OF'", in, &state, parser_name);
    }
    free_combinator(element_type);

    // Build AST
    ast_t* array_ast = new_ast();
    array_ast->typ = pargs->tag;
    array_ast->sym = is_packed ? sym_lookup("packed") : NULL;
    array_ast->child = indices_ast;
    if (indices_ast) {
        // Link element type as the last child
        ast_t* current = indices_ast;
        while (current->next) current = current->next;
        current->next = element_ast;
    } else {
        array_ast->child = element_ast;
    }
    array_ast->next = NULL;

    set_ast_position(array_ast, in);
    return make_success(array_ast);
}

combinator_t* array_type(tag_t tag) {
    combinator_t* comb = new_combinator();
    prim_args* args = safe_malloc(sizeof(prim_args));
    args->tag = tag;
    comb->args = args;
    comb->fn = array_type_fn;
    return comb;
}

static ast_t* build_class_ast(ast_t* ast) {
    // ast is the result of the seq. The first child is the class_body.
    ast_t* class_body = ast->child;

    ast_t* class_node = new_ast();
    class_node->typ = ast->typ;
    class_node->child = copy_ast(class_body);
    class_node->line = ast->line;
    class_node->col = ast->col;

    free_ast(ast);
    return class_node;
}

combinator_t* class_type(tag_t tag) {
    // Field declaration: field_name: Type;
    combinator_t* field_name = token(cident(PASCAL_T_IDENTIFIER));
    combinator_t* field_type = token(cident(PASCAL_T_IDENTIFIER)); // simplified type for now
    combinator_t* field_decl = seq(new_combinator(), PASCAL_T_FIELD_DECL,
        field_name,
        token(match(":")),
        field_type,
        token(match(";")),
        NULL
    );

    // Method declarations (simplified - just headers for now)
    combinator_t* constructor_decl = seq(new_combinator(), PASCAL_T_CONSTRUCTOR_DECL,
        token(keyword_ci("constructor")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        NULL
    );

    combinator_t* destructor_decl = seq(new_combinator(), PASCAL_T_DESTRUCTOR_DECL,
        token(keyword_ci("destructor")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("override")),
            optional(token(match(";"))),
            NULL
        )),
        NULL
    );

    combinator_t* procedure_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("procedure")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("override")),
            optional(token(match(";"))),
            NULL
        )),
        NULL
    );

    combinator_t* function_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("function")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(":")),
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match(";")),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("override")),
            optional(token(match(";"))),
            NULL
        )),
        NULL
    );

    // Class operator declaration: operator Name; [override];
    combinator_t* class_operator_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("operator")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(":")),
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match(";")),
        NULL
    );

    // Property declaration: property Name: Type read ReadField write WriteField;
    combinator_t* property_decl = seq(new_combinator(), PASCAL_T_PROPERTY_DECL,
        token(keyword_ci("property")),
        token(cident(PASCAL_T_IDENTIFIER)), // property name
        token(match(":")),
        token(cident(PASCAL_T_IDENTIFIER)), // property type
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("read")),
            token(cident(PASCAL_T_IDENTIFIER)), // read field/method
            NULL
        )),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("write")),
            token(cident(PASCAL_T_IDENTIFIER)), // write field/method
            NULL
        )),
        token(match(";")),
        NULL
    );

    combinator_t* class_member = multi(new_combinator(), PASCAL_T_CLASS_MEMBER,
        constructor_decl,
        destructor_decl,
        procedure_decl,
        function_decl,
        class_operator_decl,
        property_decl,
        field_decl,
        NULL
    );
    set_combinator_name(class_member, "class_member");

    // Skip comments and whitespace in class body
    combinator_t* class_element = class_member;

    // Access sections: private, public, protected, published
    combinator_t* access_keyword = multi(new_combinator(), PASCAL_T_ACCESS_MODIFIER,
        token(keyword_ci("private")),
        token(keyword_ci("public")),
        token(keyword_ci("protected")),
        token(keyword_ci("published")),
        NULL
    );

    // Access section: just the access keyword (members will be parsed individually)
    combinator_t* access_section = access_keyword;

    // Class body: mix of access modifiers and class members  
    combinator_t* class_body_parser = many(multi(new_combinator(), PASCAL_T_NONE,
        access_section,      // access modifiers like private/public
        class_element,       // individual class members
        NULL
    ));

    // Optional parent class specification: (ParentClassName)
    combinator_t* parent_class = optional(between(
        token(match("(")),
        token(match(")")),
        token(cident(PASCAL_T_IDENTIFIER))
    ));

    combinator_t* class_parser = seq(new_combinator(), tag,
        token(keyword_ci("class")),
        parent_class,  // optional parent class
        class_body_parser,
        token(keyword_ci("end")),
        NULL
    );

    return map(class_parser, build_class_ast);
}

combinator_t* type_name(tag_t tag) {
    return multi(new_combinator(), PASCAL_T_NONE,
        token(create_keyword_parser("integer", tag)),
        token(create_keyword_parser("real", tag)),
        token(create_keyword_parser("boolean", tag)),
        token(create_keyword_parser("char", tag)),
        token(create_keyword_parser("string", tag)),
        token(create_keyword_parser("byte", tag)),
        token(create_keyword_parser("word", tag)),
        token(create_keyword_parser("longint", tag)),
        NULL
    );
}

static combinator_t* create_record_field_type_spec(void) {
    return multi(new_combinator(), PASCAL_T_TYPE_SPEC,
        array_type(PASCAL_T_ARRAY_TYPE),
        set_type(PASCAL_T_SET),
        pointer_type(PASCAL_T_POINTER_TYPE),
        enumerated_type(PASCAL_T_ENUMERATED_TYPE),
        range_type(PASCAL_T_RANGE_TYPE),
        record_type(PASCAL_T_RECORD_TYPE),
        type_name(PASCAL_T_IDENTIFIER),
        token(pascal_identifier(PASCAL_T_IDENTIFIER)),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );
}

typedef struct {
    combinator_t* type_parser;
} variant_tag_args;

static ParseResult variant_tag_fn(input_t* in, void* args, char* parser_name) {
    variant_tag_args* vargs = (variant_tag_args*)args;
    InputState start_state; save_input_state(in, &start_state);

    ast_t* name_ast = NULL;
    ast_t* type_ast = NULL;

    combinator_t* identifier = token(cident(PASCAL_T_IDENTIFIER));
    ParseResult ident_res = parse(in, identifier);
    free_combinator(identifier);

    if (ident_res.is_success) {
        ast_t* ident_ast = ident_res.value.ast;
        InputState colon_state; save_input_state(in, &colon_state);
        combinator_t* colon = token(match(":"));
        ParseResult colon_res = parse(in, colon);
        free_combinator(colon);
        if (colon_res.is_success) {
            free_ast(colon_res.value.ast);
            ParseResult type_res = parse(in, vargs->type_parser);
            if (!type_res.is_success) {
                discard_failure(type_res);
                free_ast(ident_ast);
                restore_input_state(in, &start_state);
                return fail_with_message("Expected variant tag type", in, &start_state, parser_name);
            }
            name_ast = ident_ast;
            type_ast = type_res.value.ast;
        } else {
            discard_failure(colon_res);
            restore_input_state(in, &colon_state);
            type_ast = ident_ast;
        }
    } else {
        discard_failure(ident_res);
        restore_input_state(in, &start_state);
        ParseResult type_res = parse(in, vargs->type_parser);
        if (!type_res.is_success) {
            return type_res;
        }
        type_ast = type_res.value.ast;
    }

    ast_t* tag_ast = new_ast();
    tag_ast->typ = PASCAL_T_VARIANT_TAG;
    tag_ast->sym = NULL;
    tag_ast->child = NULL;
    tag_ast->next = NULL;

    ast_t* tail = NULL;
    if (name_ast != NULL) {
        tag_ast->child = name_ast;
        tail = name_ast;
        while (tail->next != NULL)
            tail = tail->next;
    }
    if (type_ast != NULL) {
        if (tag_ast->child == NULL)
            tag_ast->child = type_ast;
        else if (tail != NULL)
            tail->next = type_ast;
    }

    set_ast_position(tag_ast, in);
    return make_success(tag_ast);
}

static combinator_t* create_variant_tag_parser(combinator_t* type_parser) {
    combinator_t* comb = new_combinator();
    variant_tag_args* args = safe_malloc(sizeof(variant_tag_args));
    args->type_parser = type_parser;
    comb->fn = variant_tag_fn;
    comb->args = args;
    comb->type = COMB_VARIANT_TAG;
    set_combinator_name(comb, "variant_tag");
    return comb;
}

static combinator_t* create_variant_branch_parser(combinator_t** record_item_ref) {
    combinator_t* label_atom = multi(new_combinator(), PASCAL_T_CASE_LABEL,
        token(integer(PASCAL_T_INTEGER)),
        token(char_literal(PASCAL_T_CHAR)),
        token(char_code_literal(PASCAL_T_CHAR_CODE)),
        token(pascal_string(PASCAL_T_STRING)),
        token(pascal_identifier(PASCAL_T_IDENTIFIER)),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );

    combinator_t* label_list = seq(new_combinator(), PASCAL_T_CASE_LABEL_LIST,
        sep_by(label_atom, token(match(","))),
        NULL
    );

    combinator_t* inner_items = sep_end_by(lazy(record_item_ref), token(match(";")));
    combinator_t* branch_fields = between(
        token(match("(")),
        token(match(")")),
        optional(inner_items)
    );

    combinator_t* branch = seq(new_combinator(), PASCAL_T_VARIANT_BRANCH,
        label_list,
        token(match(":")),
        optional(branch_fields),
        NULL
    );
    set_combinator_name(branch, "variant_branch");
    return branch;
}

typedef struct {
    tag_t tag;
    combinator_t* tag_parser;
    combinator_t* branch_parser;
} variant_part_args;

static ParseResult variant_part_fn(input_t* in, void* args, char* parser_name) {
    variant_part_args* pargs = (variant_part_args*)args;
    InputState start_state; save_input_state(in, &start_state);

    combinator_t* case_keyword = token(keyword_ci("case"));
    ParseResult case_res = parse(in, case_keyword);
    free_combinator(case_keyword);
    if (!case_res.is_success) {
        discard_failure(case_res);
        restore_input_state(in, &start_state);
        return make_failure_v2(in, parser_name, strdup("variant part"), NULL);
    }
    free_ast(case_res.value.ast);

    ParseResult tag_res = parse(in, pargs->tag_parser);
    if (!tag_res.is_success) {
        discard_failure(tag_res);
        restore_input_state(in, &start_state);
        return fail_with_message("Expected variant selector", in, &start_state, parser_name);
    }
    ast_t* tag_ast = tag_res.value.ast;

    combinator_t* of_keyword = token(keyword_ci("of"));
    ParseResult of_res = parse(in, of_keyword);
    free_combinator(of_keyword);
    if (!of_res.is_success) {
        discard_failure(of_res);
        free_ast(tag_ast);
        restore_input_state(in, &start_state);
        return fail_with_message("Expected 'of' in variant part", in, &start_state, parser_name);
    }
    free_ast(of_res.value.ast);

    combinator_t* branch_list = sep_end_by(lazy(&pargs->branch_parser), token(match(";")));
    ParseResult branches_res = parse(in, branch_list);
    free_combinator(branch_list);
    if (!branches_res.is_success) {
        discard_failure(branches_res);
        free_ast(tag_ast);
        restore_input_state(in, &start_state);
        return fail_with_message("Expected variant branch", in, &start_state, parser_name);
    }
    ast_t* branches_ast = branches_res.value.ast;
    if (branches_ast == ast_nil) {
        free_ast(tag_ast);
        restore_input_state(in, &start_state);
        return fail_with_message("Variant part requires at least one branch", in, &start_state, parser_name);
    }

    ast_t* variant_ast = new_ast();
    variant_ast->typ = pargs->tag;
    variant_ast->sym = NULL;
    variant_ast->child = NULL;
    variant_ast->next = NULL;

    ast_t* tail = NULL;
    if (tag_ast != NULL && tag_ast != ast_nil) {
        variant_ast->child = tag_ast;
        tail = tag_ast;
        while (tail->next != NULL)
            tail = tail->next;
    }

    if (branches_ast == ast_nil)
        branches_ast = NULL;

    if (branches_ast != NULL) {
        if (variant_ast->child == NULL)
            variant_ast->child = branches_ast;
        else if (tail != NULL)
            tail->next = branches_ast;
    }

    set_ast_position(variant_ast, in);
    return make_success(variant_ast);
}

// Record type parser: RECORD field1: type1; field2: type2; ... END
static ParseResult record_type_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state; save_input_state(in, &state);

    bool is_packed = false;
    combinator_t* packed_keyword = token(keyword_ci("packed"));
    ParseResult packed_res = parse(in, packed_keyword);
    if (packed_res.is_success) {
        is_packed = true;
        free_ast(packed_res.value.ast);
    } else {
        discard_failure(packed_res);
    }
    free_combinator(packed_keyword);

    combinator_t* record_keyword = token(keyword_ci("record"));
    ParseResult record_res = parse(in, record_keyword);
    if (!record_res.is_success) {
        discard_failure(record_res);
        free_combinator(record_keyword);
        return fail_with_message("Expected 'record'", in, &state, parser_name);
    }
    free_ast(record_res.value.ast);
    free_combinator(record_keyword);

    combinator_t* field_name_list = sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(",")));
    combinator_t* field_type = create_record_field_type_spec();
    combinator_t* field_decl = seq(new_combinator(), PASCAL_T_FIELD_DECL,
        field_name_list,
        token(match(":")),
        field_type,
        NULL
    );
    set_combinator_name(field_decl, "record_field_decl");

    combinator_t** record_item_ref = safe_malloc(sizeof(combinator_t*));
    *record_item_ref = new_combinator();

    combinator_t* variant_tag = create_variant_tag_parser(field_type);
    combinator_t* variant_branch = create_variant_branch_parser(record_item_ref);

    variant_part_args* variant_args = safe_malloc(sizeof(variant_part_args));
    variant_args->tag = PASCAL_T_VARIANT_PART;
    variant_args->tag_parser = variant_tag;
    variant_args->branch_parser = variant_branch;

    combinator_t* variant_part = new_combinator();
    variant_part->fn = variant_part_fn;
    variant_part->args = variant_args;
    variant_part->type = COMB_VARIANT_PART;
    set_combinator_name(variant_part, "variant_part");

    multi(*record_item_ref, PASCAL_T_NONE,
        variant_part,
        field_decl,
        NULL
    );

    combinator_t* record_items = sep_end_by(lazy_owned(record_item_ref), token(match(";")));
    ParseResult items_res = parse(in, record_items);
    ast_t* fields_ast = NULL;
    if (items_res.is_success) {
        fields_ast = (items_res.value.ast == ast_nil) ? NULL : items_res.value.ast;
    } else {
        discard_failure(items_res);
    }

    free_combinator(record_items);
    free(record_item_ref);

    // Skip additional content until END (methods, attributes, etc.)
    combinator_t* end_keyword = token(keyword_ci("end"));
    combinator_t* skip_end_delimiter = token(keyword_ci("end"));
    combinator_t* skip_to_end = until(skip_end_delimiter, PASCAL_T_NONE);
    ParseResult skip_res = parse(in, skip_to_end);
    if (skip_res.is_success) {
        if (skip_res.value.ast != ast_nil)
            free_ast(skip_res.value.ast);
    } else {
        discard_failure(skip_res);
        if (fields_ast != NULL)
            free_ast(fields_ast);
        free_combinator(skip_to_end);
        free_combinator(end_keyword);
        return fail_with_message("Failed to parse record body", in, &state, parser_name);
    }
    free_combinator(skip_to_end);

    ParseResult end_res = parse(in, end_keyword);
    if (!end_res.is_success) {
        discard_failure(end_res);
        if (fields_ast != NULL)
            free_ast(fields_ast);
        free_combinator(end_keyword);
        return fail_with_message("Expected 'end' after record fields", in, &state, parser_name);
    }
    free_ast(end_res.value.ast);
    free_combinator(end_keyword);

    InputState directive_state; save_input_state(in, &directive_state);
    combinator_t* skip_semicolon_delim = token(match(";"));
    combinator_t* skip_to_semicolon = until(skip_semicolon_delim, PASCAL_T_NONE);
    ParseResult directive_res = parse(in, skip_to_semicolon);
    if (directive_res.is_success) {
        if (directive_res.value.ast != ast_nil)
            free_ast(directive_res.value.ast);
    } else {
        restore_input_state(in, &directive_state);
        discard_failure(directive_res);
    }
    free_combinator(skip_to_semicolon);

    ast_t* record_ast = new_ast();
    record_ast->typ = pargs->tag;
    record_ast->sym = is_packed ? sym_lookup("packed") : NULL;
    record_ast->child = fields_ast;
    record_ast->next = NULL;

    set_ast_position(record_ast, in);
    return make_success(record_ast);
}

combinator_t* record_type(tag_t tag) {
    combinator_t* comb = new_combinator();
    prim_args* args = safe_malloc(sizeof(prim_args));
    args->tag = tag;
    comb->args = args;
    comb->fn = record_type_fn;
    return comb;
}

// Pointer type parser: ^TypeName
combinator_t* pointer_type(tag_t tag) {
    combinator_t* pointed_type = multi(new_combinator(), PASCAL_T_NONE,
        type_name(PASCAL_T_IDENTIFIER),             // built-in Pascal types (integer, string, ...)
        token(pascal_identifier(PASCAL_T_IDENTIFIER)), // user-defined type identifiers
        NULL
    );

    return seq(new_combinator(), tag,
        token(match("^")),
        pointed_type,
        NULL
    );
}

// Enumerated type parser: (Value1, Value2, Value3)
static ParseResult enumerated_type_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    // Parse opening parenthesis
    combinator_t* open_paren = token(match("("));
    ParseResult open_res = parse(in, open_paren);
    if (!open_res.is_success) {
        discard_failure(open_res);
        free_combinator(open_paren);
        return fail_with_message("Expected '(' for enumerated type", in, &state, parser_name);
    }
    free_ast(open_res.value.ast);
    free_combinator(open_paren);

    // Parse enumerated values: identifier, identifier, ...
    combinator_t* enum_value = token(cident(PASCAL_T_IDENTIFIER));
    combinator_t* value_list = sep_by(enum_value, token(match(",")));
    ParseResult values_res = parse(in, value_list);
    ast_t* values_ast = NULL;
    if (values_res.is_success) {
        values_ast = values_res.value.ast;
    } else {
        discard_failure(values_res);
        free_combinator(value_list);
        return fail_with_message("Expected enumerated values", in, &state, parser_name);
    }
    free_combinator(value_list);

    // Parse closing parenthesis
    combinator_t* close_paren = token(match(")"));
    ParseResult close_res = parse(in, close_paren);
    if (!close_res.is_success) {
        discard_failure(close_res);
        free_ast(values_ast);
        free_combinator(close_paren);
        return fail_with_message("Expected ')' after enumerated values", in, &state, parser_name);
    }
    free_ast(close_res.value.ast);
    free_combinator(close_paren);

    // Build AST
    ast_t* enum_ast = new_ast();
    enum_ast->typ = pargs->tag;
    enum_ast->sym = NULL;
    enum_ast->child = values_ast;
    enum_ast->next = NULL;

    set_ast_position(enum_ast, in);
    return make_success(enum_ast);
}

combinator_t* enumerated_type(tag_t tag) {
    combinator_t* comb = new_combinator();
    prim_args* args = safe_malloc(sizeof(prim_args));
    args->tag = tag;
    comb->args = args;
    comb->fn = enumerated_type_fn;
    return comb;
}

// Set type parser: set of TypeName (e.g., set of TAsmSehDirective)
static ParseResult set_type_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    // Parse "set"
    combinator_t* set_keyword = token(keyword_ci("set"));
    ParseResult set_result = parse(in, set_keyword);
    if (!set_result.is_success) {
        discard_failure(set_result);
        free_combinator(set_keyword);
        return fail_with_message("Expected 'set'", in, &state, parser_name);
    }
    free_combinator(set_keyword);
    free_ast(set_result.value.ast);

    // Parse "of"
    combinator_t* of_keyword = token(keyword_ci("of"));
    ParseResult of_result = parse(in, of_keyword);
    if (!of_result.is_success) {
        discard_failure(of_result);
        free_combinator(of_keyword);
        return fail_with_message("Expected 'of' after 'set'", in, &state, parser_name);
    }
    free_combinator(of_keyword);
    free_ast(of_result.value.ast);

    // Parse element type (identifier or subrange like 1..10)
    combinator_t* element_type = multi(new_combinator(), PASCAL_T_NONE,
        token(range_type(PASCAL_T_RANGE_TYPE)),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );
    ParseResult element_result = parse(in, element_type);
    if (!element_result.is_success) {
        discard_failure(element_result);
        free_combinator(element_type);
        return fail_with_message("Expected element type after 'of'", in, &state, parser_name);
    }
    free_combinator(element_type);

    // Create set type AST node
    ast_t* set_ast = new_ast();
    set_ast->typ = pargs->tag;
    set_ast->child = element_result.value.ast;
    set_ast_position(set_ast, in);

    return make_success(set_ast);
}

combinator_t* set_type(tag_t tag) {
    combinator_t* comb = new_combinator();
    prim_args* args = safe_malloc(sizeof(prim_args));
    args->tag = tag;
    comb->args = args;
    comb->fn = set_type_fn;
    return comb;
}

// --- Procedure and Function Type Parsers ---

// Helper to wrap parameter list in a PASCAL_T_PARAM_LIST node
static ast_t* wrap_in_param_list_node(ast_t* params) {
    if (params == NULL || params == ast_nil)
        return ast_nil;
    
    ast_t* param_list_node = new_ast();
    param_list_node->typ = PASCAL_T_PARAM_LIST;
    param_list_node->child = params;
    return param_list_node;
}

// Arguments for subroutine type parser
typedef struct {
    tag_t tag;
    int is_function;  // 1 for function, 0 for procedure
} SubroutineTypeArgs;

// Core parser function for both procedure and function types
static ParseResult subroutine_type_fn(input_t* in, void* args, char* parser_name) {
    SubroutineTypeArgs* pargs = (SubroutineTypeArgs*)args;
    InputState state;
    save_input_state(in, &state);

    // 1. Parse 'procedure' or 'function' keyword
    const char* keyword = pargs->is_function ? "function" : "procedure";
    combinator_t* keyword_parser = token(keyword_ci(keyword));
    ParseResult keyword_res = parse(in, keyword_parser);
    free_combinator(keyword_parser);
    
    if (!keyword_res.is_success) {
        discard_failure(keyword_res);
        return fail_with_message(pargs->is_function ? "Expected 'function'" : "Expected 'procedure'", 
                                in, &state, parser_name);
    }
    free_ast(keyword_res.value.ast);

    // 2. Parse optional parameter list
    combinator_t* params_parser = create_pascal_param_parser();
    ParseResult params_res = parse(in, params_parser);
    free_combinator(params_parser);
    
    ast_t* params_ast = NULL;
    if (params_res.is_success) {
        ast_t* raw_params = params_res.value.ast;
        if (raw_params != NULL && raw_params != ast_nil) {
            params_ast = wrap_in_param_list_node(raw_params);
        }
    } else {
        discard_failure(params_res);
    }

    // 3. If it's a function, parse the required return type
    ast_t* return_ast = NULL;
    if (pargs->is_function) {
        // Parse ': TypeSpec'
        combinator_t* colon_parser = token(match(":"));
        ParseResult colon_res = parse(in, colon_parser);
        free_combinator(colon_parser);
        
        if (!colon_res.is_success) {
            if (params_ast != NULL) free_ast(params_ast);
            discard_failure(colon_res);
            return fail_with_message("Expected ':' for function return type", in, &state, parser_name);
        }
        free_ast(colon_res.value.ast);

        // Parse the return type
        combinator_t* type_spec = multi(new_combinator(), PASCAL_T_TYPE_SPEC,
            array_type(PASCAL_T_ARRAY_TYPE),
            set_type(PASCAL_T_SET),
            range_type(PASCAL_T_RANGE_TYPE),
            pointer_type(PASCAL_T_POINTER_TYPE),
            enumerated_type(PASCAL_T_ENUMERATED_TYPE),
            record_type(PASCAL_T_RECORD_TYPE),
            token(cident(PASCAL_T_IDENTIFIER)),
            NULL
        );
        
        ParseResult type_res = parse(in, type_spec);
        free_combinator(type_spec);
        
        if (!type_res.is_success) {
            if (params_ast != NULL) free_ast(params_ast);
            discard_failure(type_res);
            return fail_with_message("Expected return type specification", in, &state, parser_name);
        }
        
        // Wrap return type in PASCAL_T_RETURN_TYPE node
        return_ast = new_ast();
        return_ast->typ = PASCAL_T_RETURN_TYPE;
        return_ast->child = type_res.value.ast;
        set_ast_position(return_ast, in);
    }

    // 4. Assemble the final AST node
    ast_t* type_ast = new_ast();
    type_ast->typ = pargs->tag;
    type_ast->child = params_ast;
    
    if (params_ast != NULL) {
        params_ast->next = return_ast;
    } else {
        type_ast->child = return_ast;
    }
    
    set_ast_position(type_ast, in);
    return make_success(type_ast);
}

// Public constructor for the procedure type parser
combinator_t* procedure_type(tag_t tag) {
    combinator_t* comb = new_combinator();
    SubroutineTypeArgs* args = safe_malloc(sizeof(SubroutineTypeArgs));
    args->tag = tag;
    args->is_function = 0;
    comb->args = args;
    comb->fn = subroutine_type_fn;
    return comb;
}

// Public constructor for the function type parser
combinator_t* function_type(tag_t tag) {
    combinator_t* comb = new_combinator();
    SubroutineTypeArgs* args = safe_malloc(sizeof(SubroutineTypeArgs));
    args->tag = tag;
    args->is_function = 1;
    comb->args = args;
    comb->fn = subroutine_type_fn;
    return comb;
}
