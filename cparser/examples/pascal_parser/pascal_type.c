#include "pascal_type.h"
#include "pascal_parser.h"
#include "pascal_keywords.h"
#include "pascal_declaration.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Range type parser: start..end (e.g., -1..1)
static ParseResult range_type_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    // Try parsing integer (including negative) or identifier for start value
    combinator_t* start_parser = multi(new_combinator(), PASCAL_T_NONE,
        integer(PASCAL_T_INTEGER),
        seq(new_combinator(), PASCAL_T_INTEGER,
            match("-"),
            integer(PASCAL_T_INTEGER),
            NULL),
        cident(PASCAL_T_IDENTIFIER),
        NULL);

    ParseResult start_result = parse(in, start_parser);
    if (!start_result.is_success) {
        free_combinator(start_parser);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected range start value"), NULL);
    }

    // Parse the ".." separator with whitespace handling
    combinator_t* range_sep = token(match(".."));
    ParseResult sep_result = parse(in, range_sep);
    if (!sep_result.is_success) {
        free_ast(start_result.value.ast);
        free_combinator(start_parser);
        free_combinator(range_sep);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected '..' in range type"), NULL);
    }
    free_combinator(range_sep);
    free_ast(sep_result.value.ast);

    // Parse end value using the same parser
    ParseResult end_result = parse(in, start_parser);
    if (!end_result.is_success) {
        free_ast(start_result.value.ast);
        free_combinator(start_parser);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected range end value"), NULL);
    }

    // Create range AST
    ast_t* range_ast = new_ast();
    range_ast->typ = pargs->tag;
    range_ast->child = start_result.value.ast;
    start_result.value.ast->next = end_result.value.ast;
    range_ast->sym = NULL;
    range_ast->next = NULL;

    free_combinator(start_parser);
    set_ast_position(range_ast, in);
    return make_success(range_ast);
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

    // Parse "ARRAY" keyword (case insensitive)
    combinator_t* array_keyword = token(keyword_ci("array"));
    ParseResult array_res = parse(in, array_keyword);
    if (!array_res.is_success) {
        free_combinator(array_keyword);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected 'array'"), NULL);
    }
    free_ast(array_res.value.ast);
    free_combinator(array_keyword);

    // Parse [
    combinator_t* open_bracket = token(match("["));
    ParseResult open_res = parse(in, open_bracket);
    if (!open_res.is_success) {
        free_combinator(open_bracket);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected '[' after 'array'"), NULL);
    }
    free_ast(open_res.value.ast);
    free_combinator(open_bracket);

    // Parse ranges/indices (simplified - just accept any identifiers/ranges for now)
    combinator_t* array_index = multi(new_combinator(), PASCAL_T_NONE,
        range_type(PASCAL_T_RANGE_TYPE),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );
    combinator_t* index_list = sep_by(array_index, token(match(",")));
    ParseResult indices_res = parse(in, index_list);
    ast_t* indices_ast = NULL;
    if (indices_res.is_success) {
        indices_ast = indices_res.value.ast;
    } else {
        free_combinator(index_list);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected array indices"), NULL);
    }
    free_combinator(index_list);

    // Parse ]
    combinator_t* close_bracket = token(match("]"));
    ParseResult close_res = parse(in, close_bracket);
    if (!close_res.is_success) {
        free_ast(indices_ast);
        free_combinator(close_bracket);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected ']'"), NULL);
    }
    free_ast(close_res.value.ast);
    free_combinator(close_bracket);

    // Parse OF
    combinator_t* of_keyword = token(keyword_ci("of"));
    ParseResult of_res = parse(in, of_keyword);
    if (!of_res.is_success) {
        free_ast(indices_ast);
        free_combinator(of_keyword);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected 'OF' after array indices"), NULL);
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
        free_ast(indices_ast);
        free_combinator(element_type);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected element type after 'OF'"), NULL);
    }
    free_combinator(element_type);

    // Build AST
    ast_t* array_ast = new_ast();
    array_ast->typ = pargs->tag;
    array_ast->sym = NULL;
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
    // ast is the result of the seq.
    // children are: class_keyword, class_body, end_keyword
    ast_t* class_body = ast->child->next;

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
    combinator_t* method_name = token(cident(PASCAL_T_IDENTIFIER));
    
    // Use the shared parameter parser
    combinator_t* param_list = create_pascal_param_parser();

    // Constructor declaration: constructor Name;
    combinator_t* constructor_decl = seq(new_combinator(), PASCAL_T_CONSTRUCTOR_DECL,
        token(keyword_ci("constructor")),
        method_name,
        param_list,
        token(match(";")),
        NULL
    );

    // Destructor declaration: destructor Name; override;
    combinator_t* destructor_decl = seq(new_combinator(), PASCAL_T_DESTRUCTOR_DECL,
        token(keyword_ci("destructor")),
        method_name,
        param_list,
        token(match(";")),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("override")),       // override keyword
            optional(token(match(";"))),         // optional semicolon after override
            NULL
        )),
        NULL
    );

    // Procedure declaration: procedure Name; [override];
    combinator_t* procedure_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        token(keyword_ci("procedure")),
        method_name,
        param_list,
        token(match(";")),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("override")),       // override keyword
            optional(token(match(";"))),         // optional semicolon after override
            NULL
        )),
        NULL
    );

    // Function declaration: function Name: ReturnType; [override];
    combinator_t* function_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        token(keyword_ci("function")),
        method_name,
        param_list,
        token(match(":")),
        token(cident(PASCAL_T_IDENTIFIER)), // return type
        token(match(";")),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("override")),       // override keyword
            optional(token(match(";"))),         // optional semicolon after override
            NULL
        )),
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

    // Class member: field, method, constructor, destructor, or property
    combinator_t* class_member = multi(new_combinator(), PASCAL_T_CLASS_MEMBER,
        constructor_decl,
        destructor_decl,
        procedure_decl,
        function_decl,
        property_decl,
        field_decl,
        NULL
    );

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

// Record type parser: RECORD field1: type1; field2: type2; ... END
static ParseResult record_type_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    // Parse "RECORD" keyword (case insensitive)
    combinator_t* record_keyword = token(keyword_ci("record"));
    ParseResult record_res = parse(in, record_keyword);
    if (!record_res.is_success) {
        free_combinator(record_keyword);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected 'record'"), NULL);
    }
    free_ast(record_res.value.ast);
    free_combinator(record_keyword);

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

    // Parse field list - many field declarations
    combinator_t* field_list = many(field_decl);
    ParseResult fields_res = parse(in, field_list);
    ast_t* fields_ast = NULL;
    if (fields_res.is_success) {
        fields_ast = fields_res.value.ast;
    }
    // Note: Empty record is allowed in Pascal, so we don't require fields
    free_combinator(field_list);

    // Parse "END" keyword
    combinator_t* end_keyword = token(keyword_ci("end"));
    ParseResult end_res = parse(in, end_keyword);
    if (!end_res.is_success) {
        if (fields_ast) free_ast(fields_ast);
        free_combinator(end_keyword);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected 'end' after record fields"), NULL);
    }
    free_ast(end_res.value.ast);
    free_combinator(end_keyword);

    // Build AST
    ast_t* record_ast = new_ast();
    record_ast->typ = pargs->tag;
    record_ast->sym = NULL;
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
    return seq(new_combinator(), tag,
        token(match("^")),
        pascal_identifier(PASCAL_T_IDENTIFIER),
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
        free_combinator(open_paren);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected '(' for enumerated type"), NULL);
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
        free_combinator(value_list);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected enumerated values"), NULL);
    }
    free_combinator(value_list);

    // Parse closing parenthesis
    combinator_t* close_paren = token(match(")"));
    ParseResult close_res = parse(in, close_paren);
    if (!close_res.is_success) {
        free_ast(values_ast);
        free_combinator(close_paren);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected ')' after enumerated values"), NULL);
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
        free_combinator(set_keyword);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected 'set'"), NULL);
    }
    free_combinator(set_keyword);
    free_ast(set_result.value.ast);

    // Parse "of"
    combinator_t* of_keyword = token(keyword_ci("of"));
    ParseResult of_result = parse(in, of_keyword);
    if (!of_result.is_success) {
        free_combinator(of_keyword);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected 'of' after 'set'"), NULL);
    }
    free_combinator(of_keyword);
    free_ast(of_result.value.ast);

    // Parse element type (usually an identifier)
    combinator_t* element_type = token(cident(PASCAL_T_IDENTIFIER));
    ParseResult element_result = parse(in, element_type);
    if (!element_result.is_success) {
        free_combinator(element_type);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected element type after 'of'"), NULL);
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
