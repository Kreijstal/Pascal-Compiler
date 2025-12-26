#include "pascal_type.h"
#include "pascal_parser.h"
#include "pascal_keywords.h"
#include "pascal_peek.h"
#include "pascal_expression.h"
#include "pascal_declaration.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
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

static combinator_t* make_generic_type_prefix(void) {
    return seq(new_combinator(), PASCAL_T_NONE,
        optional(token(keyword_ci("generic"))),
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match("<")),
        NULL
    );
}

static combinator_t* create_record_field_type_spec(void);
static combinator_t* create_record_method_directives(void);
static combinator_t* create_class_method_directives(void);

static combinator_t* create_nested_method_directives(void) {
    combinator_t* directive = multi(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("override")),
        token(keyword_ci("virtual")),
        token(keyword_ci("overload")),
        token(keyword_ci("inline")),
        token(keyword_ci("external")),
        token(keyword_ci("cdecl")),
        token(keyword_ci("stdcall")),
        token(keyword_ci("register")),
        NULL
    );

    return many(seq(new_combinator(), PASCAL_T_NONE,
        directive,
        token(match(";")),
        NULL
    ));
}

static bool read_identifier_slice(const char* buffer, int length, int pos, int* out_start, size_t* out_len) {
    if (pos >= length) {
        return false;
    }
    unsigned char ch = (unsigned char)buffer[pos];
    if (!(isalpha(ch) || ch == '_')) {
        return false;
    }
    int start = pos;
    pos++;
    while (pos < length) {
        unsigned char next = (unsigned char)buffer[pos];
        if (!(isalnum(next) || next == '_')) {
            break;
        }
        pos++;
    }
    *out_start = start;
    *out_len = (size_t)(pos - start);
    return true;
}

static bool slice_equals_keyword_ci(const char* start, size_t len, const char* keyword) {
    size_t keyword_len = strlen(keyword);
    if (len != keyword_len) {
        return false;
    }
    return strncasecmp(start, keyword, len) == 0;
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

static ParseResult class_member_dispatch_fn(input_t* in, void* args, char* parser_name) {
    class_member_dispatch_args_t* dispatch = (class_member_dispatch_args_t*)args;
    if (dispatch == NULL) {
        return make_failure(in, strdup("class member dispatcher misconfigured"));
    }
    if (in == NULL || in->buffer == NULL) {
        return make_failure(in, strdup("class member dispatcher missing input"));
    }
    const char* buffer = in->buffer;
    int length = in->length > 0 ? in->length : (int)strlen(buffer);
    int pos = skip_pascal_layout_preview(in, in->start);
    if (pos >= length) {
        return make_failure_v2(in, parser_name, strdup("Unexpected end of input inside class member"), NULL);
    }

    int word_start = 0;
    size_t word_len = 0;
    if (!read_identifier_slice(buffer, length, pos, &word_start, &word_len)) {
        if (dispatch->field_parser != NULL) {
            return parse(in, dispatch->field_parser);
        }
        return make_failure_v2(in, parser_name, strdup("Unable to determine class member kind"), NULL);
    }

    pos = skip_pascal_layout_preview(in, word_start + (int)word_len);
    const char* keyword_start = buffer + word_start;
    size_t keyword_len = word_len;

    if (slice_equals_keyword_ci(keyword_start, keyword_len, "generic")) {
        if (!read_identifier_slice(buffer, length, pos, &word_start, &word_len)) {
            if (dispatch->field_parser != NULL) {
                return parse(in, dispatch->field_parser);
            }
            return make_failure_v2(in, parser_name, strdup("Expected keyword after 'generic'"), NULL);
        }
        pos = skip_pascal_layout_preview(in, word_start + (int)word_len);
        keyword_start = buffer + word_start;
        keyword_len = word_len;
    }

    if (slice_equals_keyword_ci(keyword_start, keyword_len, "class")) {
        if (!read_identifier_slice(buffer, length, pos, &word_start, &word_len)) {
            if (dispatch->field_parser != NULL) {
                return parse(in, dispatch->field_parser);
            }
            return make_failure_v2(in, parser_name, strdup("Expected keyword after 'class'"), NULL);
        }
        keyword_start = buffer + word_start;
        keyword_len = word_len;
    }

    if (slice_equals_keyword_ci(keyword_start, keyword_len, "constructor") && dispatch->constructor_parser != NULL) {
        return parse(in, dispatch->constructor_parser);
    }
    if (slice_equals_keyword_ci(keyword_start, keyword_len, "destructor") && dispatch->destructor_parser != NULL) {
        return parse(in, dispatch->destructor_parser);
    }
    if (slice_equals_keyword_ci(keyword_start, keyword_len, "procedure") && dispatch->procedure_parser != NULL) {
        return parse(in, dispatch->procedure_parser);
    }
    if (slice_equals_keyword_ci(keyword_start, keyword_len, "function") && dispatch->function_parser != NULL) {
        return parse(in, dispatch->function_parser);
    }
    if (slice_equals_keyword_ci(keyword_start, keyword_len, "operator") && dispatch->operator_parser != NULL) {
        return parse(in, dispatch->operator_parser);
    }
    if (slice_equals_keyword_ci(keyword_start, keyword_len, "property") && dispatch->property_parser != NULL) {
        return parse(in, dispatch->property_parser);
    }

    if (dispatch->field_parser != NULL) {
        return parse(in, dispatch->field_parser);
    }

    return make_failure_v2(in, parser_name, strdup("No matching class member parser"), NULL);
}

// Range type parser: reuse expression parser and re-tag range AST nodes
static ParseResult range_type_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    combinator_t* expr_parser = new_combinator();
    init_pascal_expression_parser(&expr_parser, NULL);

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

    const char* pack_sym = NULL;
    combinator_t* bitpacked_keyword = token(keyword_ci("bitpacked"));
    ParseResult bitpacked_res = parse(in, bitpacked_keyword);
    if (bitpacked_res.is_success) {
        pack_sym = "bitpacked";
        free_ast(bitpacked_res.value.ast);
    } else {
        discard_failure(bitpacked_res);
    }
    free_combinator(bitpacked_keyword);

    if (pack_sym == NULL) {
        combinator_t* packed_keyword = token(keyword_ci("packed"));
        ParseResult packed_res = parse(in, packed_keyword);
        if (packed_res.is_success) {
            pack_sym = "packed";
            free_ast(packed_res.value.ast);
        } else {
            discard_failure(packed_res);
        }
        free_combinator(packed_keyword);
    }

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

    // Parse element type - support full type specs (record, set, pointer, identifier with optional [size], etc.)
    ast_t* element_ast = NULL;
    {
        // Try a rich element type first: record/class/interface/proc/func/set/file/pointer/range
        combinator_t* packed_record = seq(new_combinator(), PASCAL_T_RECORD_TYPE,
            token(keyword_ci("packed")),
            record_type(PASCAL_T_RECORD_TYPE),
            NULL
        );
        combinator_t* rich_element_type = multi(new_combinator(), PASCAL_T_TYPE_SPEC,
            array_type(PASCAL_T_ARRAY_TYPE),
            packed_record,
            record_type(PASCAL_T_RECORD_TYPE),
            interface_type(PASCAL_T_INTERFACE_TYPE),
            class_type(PASCAL_T_CLASS_TYPE),
            procedure_type(PASCAL_T_PROCEDURE_TYPE),
            function_type(PASCAL_T_FUNCTION_TYPE),
            set_type(PASCAL_T_SET),
            file_type(PASCAL_T_FILE_TYPE),
            pointer_type(PASCAL_T_POINTER_TYPE),
            range_type(PASCAL_T_RANGE_TYPE),
            token(pascal_identifier_with_subscript(PASCAL_T_IDENTIFIER)),
            token(cident(PASCAL_T_IDENTIFIER)),
            NULL
        );
        ParseResult el_res = parse(in, rich_element_type);
        free_combinator(rich_element_type);
        if (!el_res.is_success) {
            discard_failure(el_res);
            free_ast(indices_ast);
            return fail_with_message("Expected element type after 'OF'", in, &state, parser_name);
        }
        element_ast = el_res.value.ast;
    }

    // Build AST
    ast_t* array_ast = new_ast();
    array_ast->typ = pargs->tag;
    array_ast->sym = pack_sym ? sym_lookup(pack_sym) : NULL;
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

// Helper function to create a type reference parser that supports both
// simple identifiers and constructed types (like TFoo<Integer> or specialize TFoo<Integer>)
static combinator_t* create_type_ref_parser(void) {
    combinator_t* type_arg = token(cident(PASCAL_T_TYPE_ARG));
    combinator_t* type_arg_list = seq(new_combinator(), PASCAL_T_TYPE_ARG_LIST,
        token(match("<")),
        sep_by(type_arg, token(match(","))),
        token(match(">")),
        NULL
    );
    combinator_t* constructed_type = seq(new_combinator(), PASCAL_T_CONSTRUCTED_TYPE,
        optional(token(keyword_ci("specialize"))),
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
        type_arg_list,
        NULL
    );
    // Allow qualified identifiers like BU.stat and System.THandle as simple type refs.
    combinator_t* simple_type = token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER));
    
    // Include array types, records, and other complex type specs
    // This allows class fields to have types like "array of T"
    return multi(new_combinator(), PASCAL_T_NONE,
        constructed_type,
        array_type(PASCAL_T_ARRAY_TYPE),
        set_type(PASCAL_T_SET),
        pointer_type(PASCAL_T_POINTER_TYPE),
        file_type(PASCAL_T_FILE_TYPE),
        simple_type,
        NULL
    );
}

// Helper function to create a method type parameter list parser
// This supports generic methods like: procedure Foo<T>(X: T);
static combinator_t* create_method_type_param_list(void) {
    combinator_t* constraint_keyword = multi(new_combinator(), PASCAL_T_TYPE_CONSTRAINT,
        token(keyword_ci("class")),
        token(keyword_ci("record")),
        token(keyword_ci("constructor")),
        token(keyword_ci("interface")),
        NULL
    );
    
    combinator_t* type_constraint = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(match(":")),
        constraint_keyword,
        NULL
    ));
    
    combinator_t* type_param_with_constraint = seq(new_combinator(), PASCAL_T_TYPE_PARAM,
        token(cident(PASCAL_T_IDENTIFIER)),
        type_constraint,
        NULL
    );
    
    return optional(seq(new_combinator(), PASCAL_T_TYPE_PARAM_LIST,
        token(match("<")),
        sep_by(type_param_with_constraint, token(match(","))),
        token(match(">")),
        NULL
    ));
}

combinator_t* class_type(tag_t tag) {
    // Type reference for field declarations, method return types, etc.
    combinator_t* type_ref = create_type_ref_parser();

    // Field declaration: field_name: Type;
    combinator_t* field_name = token(cident(PASCAL_T_IDENTIFIER));
    combinator_t* field_decl = seq(new_combinator(), PASCAL_T_FIELD_DECL,
        field_name,
        token(match(":")),
        type_ref,
        token(match(";")),
        NULL
    );

    // Method declarations (simplified - just headers for now)
    combinator_t* constructor_decl = seq(new_combinator(), PASCAL_T_CONSTRUCTOR_DECL,
        optional(token(keyword_ci("class"))),  // Support class constructor
        token(keyword_ci("constructor")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        create_class_method_directives(),  // Support virtual, override, etc.
        NULL
    );

    combinator_t* destructor_decl = seq(new_combinator(), PASCAL_T_DESTRUCTOR_DECL,
        optional(token(keyword_ci("class"))),  // Support class destructor
        token(keyword_ci("destructor")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        create_class_method_directives(),  // Support virtual, override, etc.
        NULL
    );

    combinator_t* procedure_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        optional(token(keyword_ci("generic"))),
        optional(token(keyword_ci("class"))),
        token(keyword_ci("procedure")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_method_type_param_list(),  // Optional type parameters for generic methods
        create_pascal_param_parser(),
        token(match(";")),
        create_class_method_directives(),  // Support virtual, override, reintroduce, etc.
        NULL
    );

    combinator_t* function_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        optional(token(keyword_ci("generic"))),
        optional(token(keyword_ci("class"))),
        token(keyword_ci("function")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_method_type_param_list(),  // Optional type parameters for generic methods
        create_pascal_param_parser(),
        token(match(":")),
        create_type_ref_parser(),  // Support both simple and constructed types
        token(match(";")),
        create_class_method_directives(),  // Support virtual, override, reintroduce, etc.
        NULL
    );

    // Class operator declaration: operator Name/Symbol(params): ReturnType; [override];
    combinator_t* class_operator_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("operator")),
        token(operator_name(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(":")),
        create_type_ref_parser(),  // Support both simple and constructed types
        token(match(";")),
        NULL
    );

    // Property declaration: property Name: Type read ReadField write WriteField; [default;]
    combinator_t* property_indexer = optional(between(
        token(match("[")),
        token(match("]")),
        sep_by(seq(new_combinator(), PASCAL_T_NONE,
            sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
            token(match(":")),
            create_record_field_type_spec(),
            NULL
        ), token(match(";")))
    ));

    combinator_t* property_decl = seq(new_combinator(), PASCAL_T_PROPERTY_DECL,
        optional(token(keyword_ci("class"))),  // Support class properties
        token(keyword_ci("property")),
        token(cident(PASCAL_T_IDENTIFIER)), // property name
        property_indexer,
        token(match(":")),
        create_type_ref_parser(),  // Support both simple and constructed types
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
        // Optional trailing "default;" directive used in FGL-like properties
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("default")),
            token(match(";")),
            NULL
        )),
        NULL
    );

    class_member_dispatch_args_t* class_dispatch = (class_member_dispatch_args_t*)safe_malloc(sizeof(class_member_dispatch_args_t));
    class_dispatch->constructor_parser = constructor_decl;
    class_dispatch->destructor_parser = destructor_decl;
    class_dispatch->procedure_parser = procedure_decl;
    class_dispatch->function_parser = function_decl;
    class_dispatch->operator_parser = class_operator_decl;
    class_dispatch->property_parser = property_decl;
    class_dispatch->field_parser = field_decl;

    combinator_t* class_member = new_combinator();
    class_member->type = COMB_CLASS_MEMBER_DISPATCH;
    class_member->fn = class_member_dispatch_fn;
    class_member->args = class_dispatch;
    set_combinator_name(class_member, "class_member");

    // Access sections: private, public, protected, published, strict private, strict protected
    // Handle strict modifiers first (two-word) then single-word modifiers
    combinator_t* strict_access = seq(new_combinator(), PASCAL_T_ACCESS_MODIFIER,
        token(keyword_ci("strict")),
        multi(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("private")),
            token(keyword_ci("protected")),
            NULL
        ),
        NULL
    );
    combinator_t* simple_access = multi(new_combinator(), PASCAL_T_ACCESS_MODIFIER,
        token(keyword_ci("private")),
        token(keyword_ci("public")),
        token(keyword_ci("protected")),
        token(keyword_ci("published")),
        NULL
    );
    combinator_t* access_keyword = multi(new_combinator(), PASCAL_T_NONE,
        strict_access,
        simple_access,
        NULL
    );

    // Access section: just the access keyword (members will be parsed individually)
    combinator_t* access_section = access_keyword;

    // Nested type section in class body: type TBar = class ... end;
    // Type parameter with constraint for nested types
    combinator_t* nested_constraint_kw = multi(new_combinator(), PASCAL_T_TYPE_CONSTRAINT,
        token(keyword_ci("class")),
        token(keyword_ci("record")),
        token(keyword_ci("constructor")),
        token(keyword_ci("interface")),
        NULL
    );
    
    combinator_t* nested_type_constraint = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(match(":")),
        nested_constraint_kw,
        NULL
    ));
    
    combinator_t* nested_type_param_with_constraint = seq(new_combinator(), PASCAL_T_TYPE_PARAM,
        token(cident(PASCAL_T_IDENTIFIER)),
        nested_type_constraint,
        NULL
    );
    
    combinator_t* nested_type_param_list_required = seq(new_combinator(), PASCAL_T_TYPE_PARAM_LIST,
        token(match("<")),
        sep_by(nested_type_param_with_constraint, token(match(","))),
        token(match(">")),
        NULL
    );
    
    // Nested class type (simplified - just empty class for now)
    combinator_t* nested_field_decl = seq(new_combinator(), PASCAL_T_NONE,
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
        token(match(":")),
        create_type_ref_parser(),
        token(match(";")),
        NULL
    );

    // Nested method declarations inside nested classes
    combinator_t* nested_proc_decl = seq(new_combinator(), PASCAL_T_NONE,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("procedure")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        create_nested_method_directives(),
        NULL
    );

    combinator_t* nested_func_decl = seq(new_combinator(), PASCAL_T_NONE,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("function")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(":")),
        create_type_ref_parser(),
        token(match(";")),
        create_nested_method_directives(),
        NULL
    );

    combinator_t* nested_constructor_decl = seq(new_combinator(), PASCAL_T_NONE,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("constructor")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        create_nested_method_directives(),
        NULL
    );

    combinator_t* nested_destructor_decl = seq(new_combinator(), PASCAL_T_NONE,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("destructor")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        create_nested_method_directives(),
        NULL
    );

    combinator_t* nested_property_indexer = optional(between(
        token(match("[")),
        token(match("]")),
        sep_by(seq(new_combinator(), PASCAL_T_NONE,
            sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
            token(match(":")),
            create_record_field_type_spec(),
            NULL
        ), token(match(";")))
    ));

    combinator_t* nested_property_decl = seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("property")),
        token(cident(PASCAL_T_IDENTIFIER)),
        nested_property_indexer,
        token(match(":")),
        create_type_ref_parser(),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("read")),
            token(cident(PASCAL_T_IDENTIFIER)),
            NULL
        )),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("write")),
            token(cident(PASCAL_T_IDENTIFIER)),
            NULL
        )),
        token(match(";")),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("default")),
            token(match(";")),
            NULL
        )),
        NULL
    );

    combinator_t* nested_class_member = multi(new_combinator(), PASCAL_T_NONE,
        access_section,
        nested_field_decl,
        nested_constructor_decl,
        nested_destructor_decl,
        nested_proc_decl,
        nested_func_decl,
        nested_property_decl,
        NULL
    );

    combinator_t* nested_class_body = many(nested_class_member);

    combinator_t* nested_class_type = seq(new_combinator(), PASCAL_T_CLASS_TYPE,
        token(keyword_ci("class")),
        nested_class_body,
        token(keyword_ci("end")),
        NULL
    );
    
    combinator_t* nested_type_spec = multi(new_combinator(), PASCAL_T_TYPE_SPEC,
        nested_class_type,
        record_type(PASCAL_T_RECORD_TYPE),          // Support nested record types
        enumerated_type(PASCAL_T_ENUMERATED_TYPE),  // Support nested enumeration types
        pointer_type(PASCAL_T_POINTER_TYPE),        // Support nested pointer types
        array_type(PASCAL_T_ARRAY_TYPE),
        set_type(PASCAL_T_SET),
        range_type(PASCAL_T_RANGE_TYPE),
        file_type(PASCAL_T_FILE_TYPE),
        function_type(PASCAL_T_FUNCTION_TYPE),      // Support nested function types
        procedure_type(PASCAL_T_PROCEDURE_TYPE),    // Support nested procedure types
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
        NULL
    );
    
    combinator_t* nested_generic_type_decl = seq(new_combinator(), PASCAL_T_GENERIC_TYPE_DECL,
        optional(token(keyword_ci("generic"))),
        token(cident(PASCAL_T_IDENTIFIER)),
        nested_type_param_list_required,
        token(match("=")),
        nested_type_spec,
        optional(token(match(";"))),
        NULL
    );
    nested_generic_type_decl = right(peek(make_generic_type_prefix()), nested_generic_type_decl);
    
    combinator_t* nested_regular_type_decl = seq(new_combinator(), PASCAL_T_TYPE_DECL,
        optional(token(keyword_ci("generic"))),
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match("=")),
        nested_type_spec,
        optional(token(match(";"))),
        NULL
    );
    
    combinator_t* nested_type_decl = multi(new_combinator(), PASCAL_T_NONE,
        nested_generic_type_decl,
        nested_regular_type_decl,
        NULL
    );
    
    combinator_t* nested_type_section = seq(new_combinator(), PASCAL_T_NESTED_TYPE_SECTION,
        token(keyword_ci("type")),
        many(nested_type_decl),
        NULL
    );

    // Nested const section in class body: const DefaultCapacity = 64;
    // Initialize expression parser for class const expressions
    combinator_t** class_const_expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *class_const_expr_parser = new_combinator();
    init_pascal_expression_parser(class_const_expr_parser, NULL);

    combinator_t* nested_const_decl = seq(new_combinator(), PASCAL_T_CONST_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(match(":")),
            create_type_ref_parser(),
            NULL
        )),
        token(match("=")),
        lazy(class_const_expr_parser),
        optional(token(match(";"))),
        NULL
    );

    combinator_t* nested_const_section = seq(new_combinator(), PASCAL_T_CONST_SECTION,
        token(keyword_ci("const")),
        many(nested_const_decl),
        NULL
    );

    // Class var section: class var Field1, Field2: Type;
    combinator_t* class_var_decl = seq(new_combinator(), PASCAL_T_FIELD_DECL,
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
        token(match(":")),
        create_type_ref_parser(),
        token(match(";")),
        NULL
    );
    combinator_t* class_var_section = seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("class")),
        token(keyword_ci("var")),
        many(class_var_decl),
        NULL
    );

    // Plain var section inside class: var FField: Type;
    combinator_t* plain_var_section = seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("var")),
        many(class_var_decl),
        NULL
    );

    // Skip comments and whitespace in class body
    combinator_t* class_element = multi(new_combinator(), PASCAL_T_NONE,
        nested_type_section,   // Nested type declarations
        nested_const_section,  // Nested const declarations
        class_var_section,     // Class var declarations (class var)
        plain_var_section,     // Plain var declarations (instance var)
        class_member,          // Regular class members
        NULL
    );

    // Class body: mix of access modifiers and class members  
    combinator_t* class_body_parser = many(multi(new_combinator(), PASCAL_T_NONE,
        access_section,      // access modifiers like private/public
        class_element,       // individual class members
        NULL
    ));

    // Optional parent class/interface specification: (Parent) or (Parent1, Parent2, ...)
    // Supports both single parent and multiple parents (class + interfaces)
    combinator_t* parent_class = optional(between(
        token(match("(")),
        token(match(")")),
        sep_by(create_type_ref_parser(), token(match(",")))
    ));

    combinator_t* class_modifier = multi(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("sealed")),
        token(keyword_ci("abstract")),
        token(keyword_ci("helper")),
        NULL
    );
    combinator_t* class_modifiers = many(class_modifier);

    combinator_t* class_full = seq(new_combinator(), tag,
        token(keyword_ci("class")),
        class_modifiers,
        parent_class,  // optional parent class
        class_body_parser,
        token(keyword_ci("end")),
        NULL
    );

    combinator_t* class_forward = seq(new_combinator(), tag,
        token(keyword_ci("class")),
        class_modifiers,
        parent_class,
        peek(token(match(";"))),
        NULL
    );

    combinator_t* class_choice = multi(new_combinator(), 0,
        class_forward,
        class_full,
        NULL
    );

    return map(class_choice, build_class_ast);
}

combinator_t* interface_type(tag_t tag) {
    // Interface method declarations (similar to class but no implementation)
    combinator_t* procedure_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        token(keyword_ci("procedure")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_method_type_param_list(),  // Optional type parameters for generic methods
        create_pascal_param_parser(),
        token(match(";")),
        create_class_method_directives(),
        NULL
    );

    combinator_t* function_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        token(keyword_ci("function")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_method_type_param_list(),  // Optional type parameters for generic methods
        create_pascal_param_parser(),
        token(match(":")),
        create_type_ref_parser(),  // Support both simple and constructed types
        token(match(";")),
        create_class_method_directives(),
        NULL
    );

    // Property declaration: property Name: Type read ReadField write WriteField;
    combinator_t* property_decl = seq(new_combinator(), PASCAL_T_PROPERTY_DECL,
        token(keyword_ci("property")),
        token(cident(PASCAL_T_IDENTIFIER)), // property name
        token(match(":")),
        create_type_ref_parser(),  // Support both simple and constructed types
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

    combinator_t* interface_member = multi(new_combinator(), PASCAL_T_CLASS_MEMBER,
        procedure_decl,
        function_decl,
        property_decl,
        NULL
    );

    // Interface body: method declarations
    combinator_t* interface_body_parser = many(interface_member);

    // Optional parent interface specification: (ParentInterface) or (ParentInterface<T>)
    // Can also support multiple parent interfaces if needed
    combinator_t* parent_interface = optional(between(
        token(match("(")),
        token(match(")")),
        sep_by(create_type_ref_parser(), token(match(",")))
    ));

    // Optional GUID attribute list: ['{...}']
    combinator_t* interface_guid = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(match("[")),
        sep_by(token(pascal_string(PASCAL_T_STRING)), token(match(","))),
        token(match("]")),
        NULL
    ));

    combinator_t* interface_full = seq(new_combinator(), tag,
        token(keyword_ci("interface")),
        parent_interface,  // optional parent interface
        interface_guid,    // optional GUID attribute
        interface_body_parser,
        token(keyword_ci("end")),
        NULL
    );

    combinator_t* interface_forward = seq(new_combinator(), tag,
        token(keyword_ci("interface")),
        parent_interface,
        interface_guid,
        peek(token(match(";"))),
        NULL
    );

    combinator_t* interface_choice = multi(new_combinator(), 0,
        interface_forward,
        interface_full,
        NULL
    );

    return map(interface_choice, build_class_ast);  // Reuse same AST builder
}

// Parser function for type_name with optional [size] subscript for string types
static ParseResult type_name_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    tag_t tag = (pargs != NULL) ? pargs->tag : PASCAL_T_IDENTIFIER;
    
    InputState state;
    save_input_state(in, &state);
    
    // Try each built-in type
    const char* type_keywords[] = {
        "integer", "real", "boolean", "char", "string", "shortstring", "ansistring",
        "unicodestring", "widestring", "rawbytestring", "byte", "word", "longint",
        "pointer", NULL
    };
    
    for (int i = 0; type_keywords[i] != NULL; i++) {
        combinator_t* keyword_parser = create_keyword_parser(type_keywords[i], tag);
        ParseResult keyword_res = parse(in, keyword_parser);
        free_combinator(keyword_parser);
        
        if (keyword_res.is_success) {
            ast_t* type_ast = keyword_res.value.ast;
            
            // Only check for subscript if this is "string"
            if (strcasecmp(type_keywords[i], "string") == 0) {
                InputState subscript_state;
                save_input_state(in, &subscript_state);
                
                combinator_t* open_bracket = match("[");
                ParseResult open_res = parse(in, open_bracket);
                free_combinator(open_bracket);
                
                if (open_res.is_success) {
                    free_ast(open_res.value.ast);
                    
                    // Parse the size expression
                    combinator_t* size_expr = new_combinator();
                    init_pascal_expression_parser(&size_expr, NULL);
                    ParseResult size_res = parse(in, size_expr);
                    free_combinator(size_expr);
                    
                    if (size_res.is_success) {
                        // Parse closing ]
                        combinator_t* close_bracket = match("]");
                        ParseResult close_res = parse(in, close_bracket);
                        free_combinator(close_bracket);
                        
                        if (close_res.is_success) {
                            free_ast(close_res.value.ast);
                            // Discard the size - treat string[n] as string for now
                            free_ast(size_res.value.ast);
                        } else {
                            // Failed to parse closing bracket, restore and ignore subscript
                            discard_failure(close_res);
                            free_ast(size_res.value.ast);
                            restore_input_state(in, &subscript_state);
                        }
                    } else {
                        // Failed to parse size, restore and ignore subscript
                        discard_failure(size_res);
                        restore_input_state(in, &subscript_state);
                    }
                } else {
                    discard_failure(open_res);
                    restore_input_state(in, &subscript_state);
                }
            }
            
            ParseResult result;
            result.is_success = true;
            result.value.ast = type_ast;
            return result;
        } else {
            discard_failure(keyword_res);
            restore_input_state(in, &state);
        }
    }
    
    return make_failure(in, strdup("Expected built-in type name"));
}

combinator_t* type_name(tag_t tag) {
    combinator_t* comb = new_combinator();
    prim_args* args = safe_malloc(sizeof(prim_args));
    args->tag = tag;
    comb->args = args;
    comb->fn = type_name_fn;
    return comb;
}

static combinator_t* create_record_field_type_spec(void) {
    return multi(new_combinator(), PASCAL_T_TYPE_SPEC,
        array_type(PASCAL_T_ARRAY_TYPE),
        set_type(PASCAL_T_SET),
        file_type(PASCAL_T_FILE_TYPE),
        pointer_type(PASCAL_T_POINTER_TYPE),
        enumerated_type(PASCAL_T_ENUMERATED_TYPE),
        range_type(PASCAL_T_RANGE_TYPE),
        record_type(PASCAL_T_RECORD_TYPE),
        procedure_type(PASCAL_T_PROCEDURE_TYPE),  /* Allow procedure/function fields with calling conventions */
        function_type(PASCAL_T_FUNCTION_TYPE),
        type_name(PASCAL_T_IDENTIFIER),
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
        token(pascal_identifier(PASCAL_T_IDENTIFIER)),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );
}

static combinator_t* create_record_method_directives(void) {
    combinator_t* record_method_directive_keyword = multi(new_combinator(), PASCAL_T_NONE,
        token(create_keyword_parser("static", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("overload", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("inline", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("cdecl", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("stdcall", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("register", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("safecall", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("pascal", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("export", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("external", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("deprecated", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("unimplemented", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("platform", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("library", PASCAL_T_IDENTIFIER)),
        NULL
    );
    combinator_t* record_method_directive = seq(new_combinator(), PASCAL_T_METHOD_DIRECTIVE,
        record_method_directive_keyword,
        optional(token(pascal_string(PASCAL_T_STRING))),
        token(match(";")),
        NULL
    );
    return many(record_method_directive);
}

// Method directives for class methods - includes virtual, override, reintroduce, etc.
static combinator_t* create_class_method_directives(void) {
    combinator_t* class_method_directive_keyword = multi(new_combinator(), PASCAL_T_NONE,
        token(create_keyword_parser("virtual", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("override", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("reintroduce", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("overload", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("static", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("inline", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("abstract", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("cdecl", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("stdcall", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("register", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("safecall", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("pascal", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("export", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("external", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("deprecated", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("unimplemented", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("platform", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("library", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("dynamic", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("message", PASCAL_T_IDENTIFIER)),
        NULL
    );
    combinator_t* class_method_directive = seq(new_combinator(), PASCAL_T_METHOD_DIRECTIVE,
        class_method_directive_keyword,
        optional(token(pascal_string(PASCAL_T_STRING))),  // Some directives have string args
        token(match(";")),
        NULL
    );
    return many(class_method_directive);
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
        token(control_char_literal(PASCAL_T_CHAR)),
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

    /* Optional record helper clause: "helper for <Type>" */
    bool is_helper = false;
    pascal_word_slice_t helper_word;
    if (pascal_peek_word_after(in, in->start, &helper_word) &&
        pascal_word_equals_ci(&helper_word, "helper")) {
        is_helper = true;
        combinator_t* helper_kw = token(keyword_ci("helper"));
        ParseResult helper_res = parse(in, helper_kw);
        if (helper_res.is_success) {
            free_ast(helper_res.value.ast);
        } else {
            discard_failure(helper_res);
        }
        free_combinator(helper_kw);

        combinator_t* for_kw = token(keyword_ci("for"));
        ParseResult for_res = parse(in, for_kw);
        if (for_res.is_success) {
            free_ast(for_res.value.ast);
            combinator_t* helper_base = create_type_ref_parser();
            ParseResult base_res = parse(in, helper_base);
            if (base_res.is_success) {
                free_ast(base_res.value.ast);
            } else {
                discard_failure(base_res);
            }
            free_combinator(helper_base);
        } else {
            discard_failure(for_res);
        }
        free_combinator(for_kw);
    }

    /* For record helpers, be permissive and skip the helper body.
     * This avoids failing on complex helper method declarations. */
    if (is_helper) {
        combinator_t* helper_body = optional(until(token(keyword_ci("end")), PASCAL_T_NONE));
        ParseResult helper_body_res = parse(in, helper_body);
        if (helper_body_res.is_success && helper_body_res.value.ast != ast_nil) {
            free_ast(helper_body_res.value.ast);
        } else if (!helper_body_res.is_success) {
            discard_failure(helper_body_res);
        }
        free_combinator(helper_body);

        combinator_t* end_keyword = token(keyword_ci("end"));
        ParseResult end_res = parse(in, end_keyword);
        if (!end_res.is_success) {
            discard_failure(end_res);
            free_combinator(end_keyword);
            return fail_with_message("Expected 'end' after record helper body", in, &state, parser_name);
        }
        free_ast(end_res.value.ast);
        free_combinator(end_keyword);

        ast_t* record_ast = new_ast();
        record_ast->typ = pargs->tag;
        record_ast->sym = is_packed ? sym_lookup("packed") : NULL;
        record_ast->child = NULL;
        record_ast->next = NULL;
        set_ast_position(record_ast, in);
        return make_success(record_ast);
    }

    combinator_t* field_name_list = sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(",")));
    combinator_t* field_type = create_record_field_type_spec();
    
    // Hint directives for record fields: deprecated, platform, library, experimental
    // FPC allows these after the field type without requiring a semicolon
    combinator_t* field_hint_directive = optional(seq(new_combinator(), PASCAL_T_NONE,
        multi(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("deprecated")),
            token(keyword_ci("platform")),
            token(keyword_ci("library")),
            token(keyword_ci("experimental")),
            NULL
        ),
        optional(token(pascal_string(PASCAL_T_STRING))),  // optional message for deprecated
        NULL
    ));
    
    combinator_t* field_decl = seq(new_combinator(), PASCAL_T_FIELD_DECL,
        field_name_list,
        token(match(":")),
        field_type,
        field_hint_directive,                        // optional hint directive
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

    ast_t* fields_ast = NULL;
    if (!is_helper) {
        combinator_t* record_items = sep_end_by(lazy(record_item_ref), token(match(";")));
        ParseResult items_res = parse(in, record_items);
        if (items_res.is_success) {
            fields_ast = (items_res.value.ast == ast_nil) ? NULL : items_res.value.ast;
        } else {
            discard_failure(items_res);
        }
        free_combinator(record_items);
    }
    if (!is_helper) {
        // Parse method declarations (class operators) for advanced records
        // Create a parser for class operator declarations in records
        combinator_t* record_method_directives_for_operator = create_record_method_directives();
        combinator_t* record_operator_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
            optional(token(keyword_ci("class"))),
            token(keyword_ci("operator")),
            token(operator_name(PASCAL_T_IDENTIFIER)),
            create_pascal_param_parser(),
            token(match(":")),
            create_type_ref_parser(),
            token(match(";")),
            record_method_directives_for_operator,
            NULL
        );
        set_combinator_name(record_operator_decl, "record_operator_decl");
        
        // Parse zero or more method declarations
        combinator_t* method_list = many(record_operator_decl);
        ParseResult methods_res = parse(in, method_list);
        ast_t* methods_ast = NULL;
        if (methods_res.is_success) {
            methods_ast = (methods_res.value.ast == ast_nil) ? NULL : methods_res.value.ast;
        } else {
            discard_failure(methods_res);
        }
        free_combinator(method_list);
        
        // Attach methods to fields list if any exist
        if (methods_ast != NULL) {
            if (fields_ast == NULL) {
                fields_ast = methods_ast;
            } else {
                // Append methods to end of fields list
                ast_t* last_field = fields_ast;
                while (last_field->next != NULL)
                    last_field = last_field->next;
                last_field->next = methods_ast;
            }
        }
    }

    // Advanced record members (as in FPC "advanced records"):
    // allow visibility sections and simple method headers, so constructs like
    //   private FCount: NativeInt; public function MoveNext: Boolean;
    // are consumed instead of causing the record parser to fail.
    combinator_t* access_modifier = multi(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("private")),
        token(keyword_ci("public")),
        token(keyword_ci("protected")),
        token(keyword_ci("published")),
        NULL
    );

    // Field directive keywords like platform, deprecated
    combinator_t* field_directive = multi(new_combinator(), PASCAL_T_NONE,
        token(create_keyword_parser("platform", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("deprecated", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("experimental", PASCAL_T_IDENTIFIER)),
        NULL
    );

    // Simple field declaration with trailing ';' and optional directives
    combinator_t* adv_field_decl = seq(new_combinator(), PASCAL_T_FIELD_DECL,
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
        token(match(":")),
        create_record_field_type_spec(),
        many(field_directive),  // Optional field directives like platform
        token(match(";")),
        NULL
    );

    // Method directives for advanced record members (e.g., overload; static; inline;)
    combinator_t* record_method_directives = create_record_method_directives();

    // Simple procedure header inside a record (with optional class prefix and method directives)
    combinator_t* adv_proc_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("procedure")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        record_method_directives,
        NULL
    );

    // Simple function header inside a record (with optional class prefix and method directives)
    combinator_t* adv_func_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("function")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(":")),
        create_type_ref_parser(),
        token(match(";")),
        record_method_directives,
        NULL
    );

    // Class operator header inside a record (with optional class prefix and method directives)
    combinator_t* record_method_directives_for_operator_member = create_record_method_directives();
    combinator_t* adv_operator_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("operator")),
        token(operator_name(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(":")),
        create_type_ref_parser(),
        token(match(";")),
        record_method_directives_for_operator_member,
        NULL
    );

    // Simple property declaration inside a record (e.g., property Current: T read GetCurrent;)
    combinator_t* adv_property_decl = seq(new_combinator(), PASCAL_T_PROPERTY_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("property")),
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match(":")),
        create_type_ref_parser(),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("read")),
            token(cident(PASCAL_T_IDENTIFIER)),
            NULL
        )),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("write")),
            token(cident(PASCAL_T_IDENTIFIER)),
            NULL
        )),
        token(match(";")),
        NULL
    );

    // Constructor/destructor headers inside a record (with optional class prefix and method directives)
    combinator_t* adv_ctor_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("constructor")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        record_method_directives,
        NULL
    );

    combinator_t* adv_dtor_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("destructor")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        record_method_directives,
        NULL
    );

    // Nested type declaration inside advanced record (e.g., type TNestedType = Integer;)
    combinator_t* adv_nested_type_decl = seq(new_combinator(), PASCAL_T_TYPE_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match("=")),
        multi(new_combinator(), PASCAL_T_TYPE_SPEC,
            record_type(PASCAL_T_RECORD_TYPE),
            enumerated_type(PASCAL_T_ENUMERATED_TYPE),
            pointer_type(PASCAL_T_POINTER_TYPE),
            procedure_type(PASCAL_T_PROCEDURE_TYPE),
            function_type(PASCAL_T_FUNCTION_TYPE),
            range_type(PASCAL_T_RANGE_TYPE),
            token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
            NULL
        ),
        optional(token(match(";"))),
        NULL
    );

    // Nested type section inside advanced record (e.g., public type TNestedType = Integer;)
    combinator_t* adv_nested_type_section = seq(new_combinator(), PASCAL_T_NESTED_TYPE_SECTION,
        token(keyword_ci("type")),
        many(adv_nested_type_decl),
        NULL
    );
    set_combinator_name(adv_nested_type_section, "adv_nested_type_section");

    // Nested const declaration inside advanced record (e.g., MyConst = 42;)
    combinator_t** adv_const_expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *adv_const_expr_parser = new_combinator();
    init_pascal_expression_parser(adv_const_expr_parser, NULL);

    combinator_t* adv_nested_const_decl = seq(new_combinator(), PASCAL_T_CONST_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(match(":")),
            create_type_ref_parser(),
            NULL
        )),
        token(match("=")),
        lazy(adv_const_expr_parser),
        optional(token(match(";"))),
        NULL
    );

    // Nested const section inside advanced record (e.g., public const MyConst = 42;)
    combinator_t* adv_nested_const_section = seq(new_combinator(), PASCAL_T_CONST_SECTION,
        token(keyword_ci("const")),
        many(adv_nested_const_decl),
        NULL
    );

    // Class var declaration inside advanced record (e.g., class var FInstance: TObject;)
    combinator_t* adv_class_var_decl = seq(new_combinator(), PASCAL_T_FIELD_DECL,
        token(keyword_ci("class")),
        token(keyword_ci("var")),
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
        token(match(":")),
        create_record_field_type_spec(),
        token(match(";")),
        NULL
    );

    combinator_t* adv_member = multi(new_combinator(), PASCAL_T_NONE,
        access_modifier,
        adv_nested_type_section,    // Support nested type sections
        adv_nested_const_section,   // Support nested const sections
        adv_class_var_decl,         // Support class var declarations
        variant_part,               // Allow variant sections after visibility blocks
        adv_field_decl,
        adv_proc_decl,
        adv_func_decl,
        adv_ctor_decl,
        adv_dtor_decl,
        adv_operator_decl,
        adv_property_decl,
        NULL
    );

    combinator_t* adv_members = many(adv_member);
    if (getenv("KGPC_DEBUG_RECORD") != NULL) {
        const char* next_input = in->buffer + in->start;
        int preview_len = 60;
        if (in->length - in->start < preview_len) preview_len = in->length - in->start;
        fprintf(stderr, "[RECORD] before adv_members, pos=%d line=%d next='%.*s'\n",
                in->start, in->line, preview_len, next_input);
    }
    ParseResult adv_res = parse(in, adv_members);
    if (getenv("KGPC_DEBUG_RECORD") != NULL) {
        fprintf(stderr, "[RECORD] adv_res.is_success=%d\n", adv_res.is_success);
        if (adv_res.is_success) {
            fprintf(stderr, "[RECORD] adv_res.value.ast=%p (ast_nil=%p)\n",
                    (void*)adv_res.value.ast, (void*)ast_nil);
            // Print AST chain
            ast_t* node = adv_res.value.ast;
            int idx = 0;
            while (node != NULL && node != ast_nil) {
                fprintf(stderr, "[RECORD]   node[%d] type=%d\n", idx++, node->typ);
                node = node->next;
            }
        }
    }
    ast_t* adv_members_ast = NULL;
    if (adv_res.is_success) {
        if (adv_res.value.ast != ast_nil)
            adv_members_ast = adv_res.value.ast;
    } else {
        discard_failure(adv_res);
    }
    free_combinator(adv_members);
    
    /* Add advanced member declarations to fields list */
    if (adv_members_ast != NULL) {
        /* Keep meaningful AST nodes, discard access modifiers and ast_nil */
        ast_t *adv_cur = adv_members_ast;
        ast_t *last_kept = NULL;
        ast_t *first_kept = NULL;
        while (adv_cur != NULL) {
            ast_t *next_node = adv_cur->next;
            if (adv_cur->typ == PASCAL_T_METHOD_DECL ||
                adv_cur->typ == PASCAL_T_FIELD_DECL ||
                adv_cur->typ == PASCAL_T_PROPERTY_DECL ||
                adv_cur->typ == PASCAL_T_NESTED_TYPE_SECTION ||
                adv_cur->typ == PASCAL_T_CONST_SECTION) {
                adv_cur->next = NULL;
                if (first_kept == NULL) {
                    first_kept = adv_cur;
                    last_kept = adv_cur;
                } else {
                    last_kept->next = adv_cur;
                    last_kept = adv_cur;
                }
            } else {
                /* Discard other nodes (e.g., access modifiers) */
                adv_cur->next = NULL;
                free_ast(adv_cur);
            }
            adv_cur = next_node;
        }
        
        /* Append to fields list */
        if (first_kept != NULL) {
            if (fields_ast == NULL) {
                fields_ast = first_kept;
            } else {
                ast_t* last_field = fields_ast;
                while (last_field->next != NULL)
                    last_field = last_field->next;
                last_field->next = first_kept;
            }
        }
    }

    /* Allow trailing field/variant declarations after advanced members (FPC allows interleaving). */
    pascal_word_slice_t tail_word;
    bool parse_tail_items = false;
    if (pascal_peek_word(in, &tail_word)) {
        if (!pascal_word_equals_ci(&tail_word, "end"))
            parse_tail_items = true;
    }

    if (parse_tail_items) {
        combinator_t* tail_field_name_list = sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(",")));
        combinator_t* tail_field_type = create_record_field_type_spec();
        combinator_t* tail_field_hint_directive = optional(seq(new_combinator(), PASCAL_T_NONE,
            multi(new_combinator(), PASCAL_T_NONE,
                token(keyword_ci("deprecated")),
                token(keyword_ci("platform")),
                token(keyword_ci("library")),
                token(keyword_ci("experimental")),
                NULL
            ),
            optional(token(pascal_string(PASCAL_T_STRING))),
            NULL
        ));

        combinator_t* tail_field_decl = seq(new_combinator(), PASCAL_T_FIELD_DECL,
            tail_field_name_list,
            token(match(":")),
            tail_field_type,
            tail_field_hint_directive,
            NULL
        );
        set_combinator_name(tail_field_decl, "record_field_decl_tail");

        combinator_t** tail_record_item_ref = safe_malloc(sizeof(combinator_t*));
        *tail_record_item_ref = new_combinator();

        combinator_t* tail_variant_tag = create_variant_tag_parser(tail_field_type);
        combinator_t* tail_variant_branch = create_variant_branch_parser(tail_record_item_ref);

        variant_part_args* tail_variant_args = safe_malloc(sizeof(variant_part_args));
        tail_variant_args->tag = PASCAL_T_VARIANT_PART;
        tail_variant_args->tag_parser = tail_variant_tag;
        tail_variant_args->branch_parser = tail_variant_branch;

        combinator_t* tail_variant_part = new_combinator();
        tail_variant_part->fn = variant_part_fn;
        tail_variant_part->args = tail_variant_args;
        tail_variant_part->type = COMB_VARIANT_PART;
        set_combinator_name(tail_variant_part, "variant_part_tail");

        multi(*tail_record_item_ref, PASCAL_T_NONE,
            tail_variant_part,
            tail_field_decl,
            NULL
        );

        combinator_t* tail_record_items = sep_end_by(lazy_owned(tail_record_item_ref), token(match(";")));
        ParseResult tail_items_res = parse(in, tail_record_items);
        ast_t* tail_fields_ast = NULL;
        if (tail_items_res.is_success) {
            tail_fields_ast = (tail_items_res.value.ast == ast_nil) ? NULL : tail_items_res.value.ast;
        } else {
            discard_failure(tail_items_res);
        }

        free_combinator(tail_record_items);
        free(tail_record_item_ref);

        if (tail_fields_ast != NULL) {
            if (fields_ast == NULL) {
                fields_ast = tail_fields_ast;
            } else {
                ast_t* last_field = fields_ast;
                while (last_field->next != NULL)
                    last_field = last_field->next;
                last_field->next = tail_fields_ast;
            }
        }
    }
    free(record_item_ref);

    combinator_t* end_keyword = token(keyword_ci("end"));
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

    // Do not consume beyond 'end' here; higher-level parsers will handle any following tokens

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

// Object type parser: OBJECT field1: type1; [procedure/function declarations] ... END
// Legacy Pascal object type - similar to record but with methods
static ParseResult object_type_fn(input_t* in, void* args, char* parser_name) {
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

    combinator_t* object_keyword = token(keyword_ci("object"));
    ParseResult object_res = parse(in, object_keyword);
    if (!object_res.is_success) {
        discard_failure(object_res);
        free_combinator(object_keyword);
        return fail_with_message("Expected 'object'", in, &state, parser_name);
    }
    free_ast(object_res.value.ast);
    free_combinator(object_keyword);

    // Field declaration: identifier_list : type
    combinator_t* field_name_list = sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(",")));
    combinator_t* field_type = create_record_field_type_spec();
    combinator_t* field_decl = seq(new_combinator(), PASCAL_T_FIELD_DECL,
        field_name_list,
        token(match(":")),
        field_type,
        token(match(";")),
        NULL
    );
    set_combinator_name(field_decl, "object_field_decl");

    // Method declarations (procedure/function headers in object definition)
    combinator_t* method_procedure_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        token(keyword_ci("procedure")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        optional(seq(new_combinator(), PASCAL_T_METHOD_DIRECTIVE,
            multi(new_combinator(), PASCAL_T_NONE,
                token(keyword_ci("virtual")),
                token(keyword_ci("override")),
                token(keyword_ci("static")),
                NULL
            ),
            optional(token(match(";"))),
            NULL
        )),
        NULL
    );

    combinator_t* method_function_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        token(keyword_ci("function")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(":")),
        create_type_ref_parser(),
        token(match(";")),
        optional(seq(new_combinator(), PASCAL_T_METHOD_DIRECTIVE,
            multi(new_combinator(), PASCAL_T_NONE,
                token(keyword_ci("virtual")),
                token(keyword_ci("override")),
                token(keyword_ci("static")),
                NULL
            ),
            optional(token(match(";"))),
            NULL
        )),
        NULL
    );

    combinator_t* constructor_decl = seq(new_combinator(), PASCAL_T_CONSTRUCTOR_DECL,
        token(keyword_ci("constructor")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        optional(seq(new_combinator(), PASCAL_T_METHOD_DIRECTIVE,
            multi(new_combinator(), PASCAL_T_NONE,
                token(keyword_ci("virtual")),
                NULL
            ),
            optional(token(match(";"))),
            NULL
        )),
        NULL
    );

    combinator_t* destructor_decl = seq(new_combinator(), PASCAL_T_DESTRUCTOR_DECL,
        token(keyword_ci("destructor")),
        token(cident(PASCAL_T_IDENTIFIER)),
        create_pascal_param_parser(),
        token(match(";")),
        optional(seq(new_combinator(), PASCAL_T_METHOD_DIRECTIVE,
            multi(new_combinator(), PASCAL_T_NONE,
                token(keyword_ci("virtual")),
                NULL
            ),
            optional(token(match(";"))),
            NULL
        )),
        NULL
    );

    // Visibility sections (public/private/protected)
    combinator_t* visibility_keyword = multi(new_combinator(), PASCAL_T_ACCESS_MODIFIER,
        token(keyword_ci("private")),
        token(keyword_ci("public")),
        token(keyword_ci("protected")),
        NULL
    );

    // Object member can be field, method, or visibility section
    combinator_t* object_member = multi(new_combinator(), PASCAL_T_NONE,
        visibility_keyword,
        constructor_decl,
        destructor_decl,
        method_procedure_decl,
        method_function_decl,
        field_decl,
        NULL
    );

    combinator_t* object_members = many(object_member);
    ParseResult members_res = parse(in, object_members);
    ast_t* fields_ast = NULL;
    if (members_res.is_success) {
        fields_ast = (members_res.value.ast == ast_nil) ? NULL : members_res.value.ast;
    } else {
        discard_failure(members_res);
    }
    free_combinator(object_members);

    combinator_t* end_keyword = token(keyword_ci("end"));
    ParseResult end_res = parse(in, end_keyword);
    if (!end_res.is_success) {
        discard_failure(end_res);
        if (fields_ast != NULL)
            free_ast(fields_ast);
        free_combinator(end_keyword);
        return fail_with_message("Expected 'end' after object members", in, &state, parser_name);
    }
    free_ast(end_res.value.ast);
    free_combinator(end_keyword);

    ast_t* object_ast = new_ast();
    object_ast->typ = pargs->tag;
    object_ast->sym = is_packed ? sym_lookup("packed") : NULL;
    object_ast->child = fields_ast;
    object_ast->next = NULL;

    set_ast_position(object_ast, in);
    return make_success(object_ast);
}

combinator_t* object_type(tag_t tag) {
    combinator_t* comb = new_combinator();
    prim_args* args = safe_malloc(sizeof(prim_args));
    args->tag = tag;
    comb->args = args;
    comb->fn = object_type_fn;
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

// Class reference type parser: class of TObject
combinator_t* class_of_type(tag_t tag) {
    combinator_t* base_class = multi(new_combinator(), PASCAL_T_NONE,
        type_name(PASCAL_T_IDENTIFIER),             // built-in types (TObject, etc.)
        token(pascal_identifier(PASCAL_T_IDENTIFIER)), // user-defined type identifiers
        NULL
    );

    return seq(new_combinator(), tag,
        token(keyword_ci("class")),
        token(keyword_ci("of")),
        base_class,
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
    // Use pascal_qualified_identifier to support nested types like TRecord.TNestedType
    combinator_t* element_type = multi(new_combinator(), PASCAL_T_NONE,
        enumerated_type(PASCAL_T_ENUMERATED_TYPE),
        token(range_type(PASCAL_T_RANGE_TYPE)),
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
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

// File type parser: file or file of TypeName
static ParseResult file_type_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    (void)parser_name;

    // Parse "file"
    combinator_t* file_keyword = token(keyword_ci("file"));
    ParseResult file_result = parse(in, file_keyword);
    if (!file_result.is_success) {
        discard_failure(file_result);
        free_combinator(file_keyword);
        return fail_with_message("Expected 'file'", in, &state, parser_name);
    }
    free_ast(file_result.value.ast);
    free_combinator(file_keyword);

    // Optional "of" and element type
    ast_t* element_type_ast = NULL;

    InputState after_file_state;
    save_input_state(in, &after_file_state);

    combinator_t* of_keyword = token(keyword_ci("of"));
    ParseResult of_result = parse(in, of_keyword);
    if (of_result.is_success) {
        free_ast(of_result.value.ast);
        free_combinator(of_keyword);

        combinator_t* element_type = multi(new_combinator(), PASCAL_T_TYPE_SPEC,
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
        ParseResult element_result = parse(in, element_type);
        free_combinator(element_type);
        if (!element_result.is_success) {
            discard_failure(element_result);
            return fail_with_message("Expected file element type after 'of'", in, &state, parser_name);
        }
        element_type_ast = element_result.value.ast;
    } else {
        discard_failure(of_result);
        free_combinator(of_keyword);
        restore_input_state(in, &after_file_state);
    }

    ast_t* file_ast = new_ast();
    file_ast->typ = pargs ? pargs->tag : PASCAL_T_FILE_TYPE;
    file_ast->child = element_type_ast;
    file_ast->next = NULL;
    set_ast_position(file_ast, in);

    return make_success(file_ast);
}

combinator_t* file_type(tag_t tag) {
    combinator_t* comb = new_combinator();
    prim_args* args = safe_malloc(sizeof(prim_args));
    args->tag = tag;
    comb->args = args;
    comb->fn = file_type_fn;
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
            file_type(PASCAL_T_FILE_TYPE),
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

    // 3.5 Optional "of object" for method pointer types (procedure of object)
    {
        combinator_t* of_object = optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("of")),
            token(keyword_ci("object")),
            NULL
        ));
        ParseResult of_object_res = parse(in, of_object);
        if (of_object_res.is_success && of_object_res.value.ast != ast_nil) {
            free_ast(of_object_res.value.ast);
        } else if (!of_object_res.is_success) {
            discard_failure(of_object_res);
        }
        free_combinator(of_object);
    }

    // 4. Optionally consume calling convention/directive keywords (e.g., stdcall, cdecl)
    // that can appear after procedure/function type declarations in Delphi/FPC.
    // We ignore their semantics here and only consume the tokens to avoid syntax errors.
    {
        combinator_t* directive_keyword = multi(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("stdcall")),
            token(keyword_ci("cdecl")),
            token(keyword_ci("register")),
            token(keyword_ci("safecall")),
            token(keyword_ci("pascal")),
            token(keyword_ci("export")),
            token(keyword_ci("external")),
            token(keyword_ci("inline")),
            token(keyword_ci("overload")),
            NULL
        );
        combinator_t* external_name_clause = seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("name")),
            token(pascal_string(PASCAL_T_STRING)),
            NULL
        );
        combinator_t* directive_argument = optional(multi(new_combinator(), PASCAL_T_NONE,
            external_name_clause,
            token(pascal_string(PASCAL_T_STRING)),
            token(cident(PASCAL_T_IDENTIFIER)),
            NULL
        ));
        // Some dialects require a semicolon between return type and directives in type declarations
        // Example: function(...): Integer; stdcall;  -- consume the first ';' and the directive list here,
        // leaving the final ';' for the enclosing type declaration to consume.
        // Only consume the ';' if a directive keyword follows immediately after.
        InputState s0; save_input_state(in, &s0);
        combinator_t* directive_semicolon = token(match(";"));
        ParseResult sc = parse(in, directive_semicolon);
        free_combinator(directive_semicolon);
        if (sc.is_success) {
            // Check if a directive follows; if not, roll back and keep ';' for outer parser
            InputState s1; save_input_state(in, &s1);
            ParseResult pk = parse(in, directive_keyword);
            if (!pk.is_success) {
                // No directive, restore to before ';'
                restore_input_state(in, &s0);
                if (sc.value.ast) {
                    free_ast(sc.value.ast);
                }
                discard_failure(pk);
            } else {
                // We have a directive: keep ';', consume directives and optional args
                if (sc.value.ast) free_ast(sc.value.ast);
                if (pk.value.ast) free_ast(pk.value.ast);
                while (1) {
                    ParseResult arg = parse(in, directive_argument);
                    if (arg.is_success) {
                        if (arg.value.ast) free_ast(arg.value.ast);
                    } else {
                        discard_failure(arg);
                    }
                    // Allow multiple directive keywords
                    InputState dn; save_input_state(in, &dn);
                    ParseResult nk = parse(in, directive_keyword);
                    if (nk.is_success) {
                        if (nk.value.ast) free_ast(nk.value.ast);
                        continue;
                    }
                    restore_input_state(in, &dn);
                    discard_failure(nk);
                    break;
                }
            }
        } else {
            discard_failure(sc);
        }
        free_combinator(directive_argument);
        free_combinator(directive_keyword);
    }

    // 5. Assemble the final AST node
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

// Reference to type parser: "reference to procedure/function"
static ParseResult reference_to_type_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    // Parse "reference" keyword
    combinator_t* reference_keyword = token(keyword_ci("reference"));
    ParseResult reference_res = parse(in, reference_keyword);
    free_combinator(reference_keyword);
    
    if (!reference_res.is_success) {
        discard_failure(reference_res);
        return fail_with_message("Expected 'reference'", in, &state, parser_name);
    }
    free_ast(reference_res.value.ast);

    // Parse "to" keyword
    combinator_t* to_keyword = token(keyword_ci("to"));
    ParseResult to_res = parse(in, to_keyword);
    free_combinator(to_keyword);
    
    if (!to_res.is_success) {
        discard_failure(to_res);
        return fail_with_message("Expected 'to' after 'reference'", in, &state, parser_name);
    }
    free_ast(to_res.value.ast);

    // Parse the procedure or function type
    combinator_t* subroutine_parser = multi(new_combinator(), PASCAL_T_NONE,
        procedure_type(PASCAL_T_PROCEDURE_TYPE),
        function_type(PASCAL_T_FUNCTION_TYPE),
        NULL
    );
    
    ParseResult subroutine_res = parse(in, subroutine_parser);
    free_combinator(subroutine_parser);
    
    if (!subroutine_res.is_success) {
        discard_failure(subroutine_res);
        return fail_with_message("Expected procedure or function type after 'reference to'", 
                                in, &state, parser_name);
    }

    // Create reference to type AST node wrapping the subroutine type
    ast_t* reference_ast = new_ast();
    reference_ast->typ = pargs->tag;
    reference_ast->child = subroutine_res.value.ast;
    reference_ast->next = NULL;
    set_ast_position(reference_ast, in);

    return make_success(reference_ast);
}

combinator_t* reference_to_type(tag_t tag) {
    combinator_t* comb = new_combinator();
    prim_args* args = safe_malloc(sizeof(prim_args));
    args->tag = tag;
    comb->args = args;
    comb->fn = reference_to_type_fn;
    return comb;
}

// Parser function for identifier with optional [size] subscript (e.g., string[20])
static ParseResult pascal_identifier_with_subscript_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    tag_t tag = (pargs != NULL) ? pargs->tag : PASCAL_T_IDENTIFIER;
    
    InputState state;
    save_input_state(in, &state);
    
    // Parse the base identifier
    combinator_t* base_id = cident(tag);
    ParseResult base_res = parse(in, base_id);
    free_combinator(base_id);
    
    if (!base_res.is_success) {
        return base_res;
    }
    
    ast_t* identifier_ast = base_res.value.ast;
    
    // Check for optional [size] subscript
    InputState subscript_state;
    save_input_state(in, &subscript_state);
    
    combinator_t* open_bracket = match("[");
    ParseResult open_res = parse(in, open_bracket);
    free_combinator(open_bracket);
    
    if (open_res.is_success) {
        free_ast(open_res.value.ast);
        
        // Parse the size expression
        combinator_t* size_expr = new_combinator();
        init_pascal_expression_parser(&size_expr, NULL);
        ParseResult size_res = parse(in, size_expr);
        free_combinator(size_expr);
        
        if (!size_res.is_success) {
            free_ast(identifier_ast);
            restore_input_state(in, &subscript_state);
            return size_res;
        }
        
        // Parse closing ]
        combinator_t* close_bracket = match("]");
        ParseResult close_res = parse(in, close_bracket);
        free_combinator(close_bracket);
        
        if (!close_res.is_success) {
            free_ast(size_res.value.ast);
            free_ast(identifier_ast);
            restore_input_state(in, &subscript_state);
            return close_res;
        }
        free_ast(close_res.value.ast);
        
        // Discard the size - treat string[n] as string for now
        // Full shortstring support would require changes throughout the type system
        free_ast(size_res.value.ast);
    } else {
        discard_failure(open_res);
        restore_input_state(in, &subscript_state);
    }
    
    ParseResult result;
    result.is_success = true;
    result.value.ast = identifier_ast;
    return result;
}

combinator_t* pascal_identifier_with_subscript(tag_t tag) {
    combinator_t* comb = new_combinator();
    prim_args* args = safe_malloc(sizeof(prim_args));
    args->tag = tag;
    comb->args = args;
    comb->fn = pascal_identifier_with_subscript_fn;
    return comb;
}
