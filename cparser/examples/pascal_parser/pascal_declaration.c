#include "pascal_declaration.h"
#include "pascal_parser.h"
#include "pascal_statement.h"
#include "pascal_expression.h"
#include "pascal_type.h"
#include "pascal_keywords.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Helper to create simple keyword AST nodes for modifiers
static void set_combinator_name(combinator_t* comb, const char* name) {
    if (comb == NULL)
        return;

    if (comb->name != NULL) {
        free(comb->name);
    }
    comb->name = strdup(name);
}

static ast_t* make_modifier_node(ast_t* original, const char* keyword) {
    if (original == NULL)
        return NULL;

    original->typ = PASCAL_T_IDENTIFIER;
    original->sym = sym_lookup(keyword);
    original->child = NULL;
    original->next = NULL;
    return original;
}

static ast_t* map_const_modifier(ast_t* ast) {
    return make_modifier_node(ast, "const");
}

static ast_t* map_var_modifier(ast_t* ast) {
    return make_modifier_node(ast, "var");
}

// Maps the 'out' keyword to a modifier node
static ast_t* map_out_modifier(ast_t* ast) {
    return make_modifier_node(ast, "out");
}

static ast_t* identity_map(ast_t* ast) {
    return ast;
}

static ast_t* discard_ast(ast_t* ast) {
    if (ast != NULL && ast != ast_nil) {
        free_ast(ast);
    }
    return ast_nil;
}

static combinator_t* create_param_name_list(void) {
    return sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(",")));
}

static combinator_t* create_param_type_spec(void) {
    combinator_t* type_reference = multi(new_combinator(), PASCAL_T_TYPE_SPEC,
        array_type(PASCAL_T_ARRAY_TYPE),
        set_type(PASCAL_T_SET),
        range_type(PASCAL_T_RANGE_TYPE),
        pointer_type(PASCAL_T_POINTER_TYPE),
        enumerated_type(PASCAL_T_ENUMERATED_TYPE),
        record_type(PASCAL_T_RECORD_TYPE),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );

    return seq(new_combinator(), PASCAL_T_NONE,
        token(match(":")),
        type_reference,
        NULL
    );
}

static combinator_t* create_modifier_param(const char* keyword, ast_t* (*mapper)(ast_t*), const char* name) {
    map_func transform = mapper != NULL ? mapper : identity_map;
    combinator_t* param = seq(new_combinator(), PASCAL_T_PARAM,
        map(token(match((char*)keyword)), transform),
        create_param_name_list(),
        create_param_type_spec(),
        NULL
    );
    set_combinator_name(param, name);
    return param;
}

static combinator_t* create_value_param(void) {
    combinator_t* param = seq(new_combinator(), PASCAL_T_PARAM,
        create_param_name_list(),
        create_param_type_spec(),
        NULL
    );
    set_combinator_name(param, "value_param");
    return param;
}

// Helper function to create parameter parser (reduces code duplication)
combinator_t* create_pascal_param_parser(void) {
    combinator_t* param = multi(new_combinator(), PASCAL_T_NONE,
        create_modifier_param("var", map_var_modifier, "var_param"),
        create_modifier_param("const", map_const_modifier, "const_param"),
        create_modifier_param("out", map_out_modifier, "out_param"),
        create_value_param(),
        NULL
    );
    set_combinator_name(param, "param");

    return optional(between(
        token(match("(")), token(match(")")), sep_by(param, token(match(";")))));
}

// Bring in the global sentinel value for an empty AST node
extern ast_t* ast_nil;

static ast_t* wrap_program_params(ast_t* params) {
    ast_t* params_node = new_ast();
    params_node->typ = PASCAL_T_PROGRAM_PARAMS;
    params_node->child = (params == ast_nil) ? NULL : params;
    return params_node;
}

// Custom parser for main block content that parses statements properly
static ParseResult main_block_content_fn(input_t* in, void* args, char* parser_name) {
    // Parse statements until encountering the END keyword handled by the caller.
    // Reuse the statement parser so the main program block supports the same
    // constructs as regular compound statements (case, loops, nested blocks, etc.).

    // `lazy` needs a stable pointer-to-pointer so recursive constructs like CASE
    // branches can reuse the same statement parser instance.  Wrap the parser in a
    // heap-allocated pointer so the lifetime matches the combinator graph.
    combinator_t** stmt_parser_ref = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *stmt_parser_ref = new_combinator();
    init_pascal_statement_parser(stmt_parser_ref);

    // Statements in a BEGIN..END block follow the same semicolon rules as any
    // compound statement: statements are separated by semicolons with an optional
    // trailing semicolon.  Use sep_by/optional to mirror the begin-end handling in
    // the statement parser so complex statements (like CASE) remain available.
    combinator_t* stmt_sequence = seq(new_combinator(), PASCAL_T_NONE,
        sep_by(lazy_owned(stmt_parser_ref), token(match(";"))),
        optional(token(match(";"))),
        NULL
    );

    ParseResult stmt_result = parse(in, stmt_sequence);

    free_combinator(stmt_sequence);

    return stmt_result;
}

static combinator_t* main_block_content(tag_t tag) {
    combinator_t* comb = new_combinator();
    prim_args* args = safe_malloc(sizeof(prim_args));
    args->tag = tag;
    comb->args = args;
    comb->fn = main_block_content_fn;
    return comb;
}

// Helper function to wrap the content of a begin-end block in a PASCAL_T_MAIN_BLOCK node
static ast_t* build_main_block_ast(ast_t* ast) {
    ast_t* block_node = new_ast();
    block_node->typ = PASCAL_T_MAIN_BLOCK;
    // If the parsed content is the nil sentinel, the block is empty.
    block_node->child = (ast == ast_nil) ? NULL : ast;
    return block_node;
}


// Pascal Program/Terminated Statement Parser - for standalone statements with semicolons
void init_pascal_program_parser(combinator_t** p) {
    // Create the base statement parser
    combinator_t** base_stmt = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *base_stmt = new_combinator();
    init_pascal_statement_parser(base_stmt);

    // Terminated statement: statement followed by semicolon
    seq(*p, PASCAL_T_NONE,
        lazy(base_stmt),                       // any statement
        token(match(";")),                     // followed by semicolon
        NULL
    );

    (*p)->extra_to_free = base_stmt;
}

// Pascal Unit Parser
void init_pascal_unit_parser(combinator_t** p) {
    combinator_t** stmt_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *stmt_parser = new_combinator();
    init_pascal_statement_parser(stmt_parser);

    // Uses section: uses unit1, unit2, unit3;
    combinator_t* uses_unit = token(pascal_qualified_identifier(PASCAL_T_USES_UNIT));
    combinator_t* uses_section = seq(new_combinator(), PASCAL_T_USES_SECTION,
        token(keyword_ci("uses")),                      // uses keyword (with word boundary check)
        sep_by(uses_unit, token(match(","))),        // unit names separated by commas
        token(match(";")),                           // semicolon
        NULL
    );

    // Type section: type name = TypeDefinition; ...
    combinator_t* specialize_args = seq(new_combinator(), PASCAL_T_NONE,
        token(match("<")),
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
        token(match(">")),
        NULL
    );

    combinator_t* specialize_type = seq(new_combinator(), PASCAL_T_TYPE_SPEC,
        token(keyword_ci("specialize")),
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
        optional(specialize_args),
        NULL
    );

    combinator_t* helper_param_list = create_pascal_param_parser();

    combinator_t* helper_procedure_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        token(keyword_ci("procedure")),
        token(cident(PASCAL_T_IDENTIFIER)),
        helper_param_list,
        token(match(";")),
        NULL
    );

    combinator_t* helper_function_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        token(keyword_ci("function")),
        token(cident(PASCAL_T_IDENTIFIER)),
        helper_param_list,
        token(match(":")),
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match(";")),
        NULL
    );

    combinator_t* helper_member = many(multi(new_combinator(), PASCAL_T_NONE,
        helper_procedure_decl,
        helper_function_decl,
        NULL
    ));

    combinator_t* type_helper_type = seq(new_combinator(), PASCAL_T_CLASS_TYPE,
        token(keyword_ci("type")),
        token(keyword_ci("helper")),
        token(keyword_ci("for")),
        token(cident(PASCAL_T_IDENTIFIER)),
        helper_member,
        token(keyword_ci("end")),
        NULL
    );

    combinator_t* type_definition = multi(new_combinator(), PASCAL_T_TYPE_SPEC,
        type_helper_type,
        class_type(PASCAL_T_CLASS_TYPE),                // class types like class ... end (try first)
        record_type(PASCAL_T_RECORD_TYPE),              // record types like record ... end
        enumerated_type(PASCAL_T_ENUMERATED_TYPE),      // enumerated types like (Value1, Value2, Value3)
        array_type(PASCAL_T_ARRAY_TYPE),                // array types like ARRAY[0..9] OF integer
        set_type(PASCAL_T_SET),                         // set types like set of TAsmSehDirective
        range_type(PASCAL_T_RANGE_TYPE),                // range types like 1..100
        pointer_type(PASCAL_T_POINTER_TYPE),            // pointer types like ^integer
        specialize_type,
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),             // simple aliases like Foo = integer
        NULL
    );

    // Const section: const name : type = value; ...
    // Use the full expression parser so typed constants support the same
    // constructs as regular expressions (arrays, arithmetic, sets, etc.).
    combinator_t** const_expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *const_expr_parser = new_combinator();
    init_pascal_expression_parser(const_expr_parser);

    combinator_t* const_value = lazy(const_expr_parser);

    combinator_t* const_decl = seq(new_combinator(), PASCAL_T_CONST_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),          // constant name
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(match(":")),                       // optional type specification
            type_definition,                         // full type definition (not just simple identifier)
            NULL
        )),
        token(match("=")),                           // equals sign
        const_value,                                 // constant value (simplified for now)
        token(match(";")),                           // semicolon
        NULL
    );

    combinator_t* const_section = seq(new_combinator(), PASCAL_T_CONST_SECTION,
        token(keyword_ci("const")),                     // const keyword (with word boundary check)
        many(const_decl),                            // multiple const declarations
        NULL
    );
    const_section->extra_to_free = const_expr_parser;

    combinator_t* resourcestring_value = multi(new_combinator(), PASCAL_T_NONE,
        token(pascal_string(PASCAL_T_STRING)),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );

    combinator_t* resourcestring_decl = seq(new_combinator(), PASCAL_T_CONST_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match("=")),
        resourcestring_value,
        token(match(";")),
        NULL
    );

    combinator_t* resourcestring_section = seq(new_combinator(), PASCAL_T_CONST_SECTION,
        token(keyword_ci("resourcestring")),
        many(resourcestring_decl),
        NULL
    );
    
    combinator_t* type_param_list = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(match("<")),
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
        token(match(">")),
        NULL
    ));

    combinator_t* type_decl = seq(new_combinator(), PASCAL_T_TYPE_DECL,
        optional(token(keyword_ci("generic"))),      // optional generic keyword
        token(cident(PASCAL_T_IDENTIFIER)),           // type name
        type_param_list,                              // optional type parameters
        token(match("=")),                           // equals sign
        type_definition,                              // type definition
        optional(token(match(";"))),                 // semicolon (optional for last decl)
        NULL
    );

    combinator_t* type_section = seq(new_combinator(), PASCAL_T_TYPE_SECTION,
        token(keyword_ci("type")),                      // type keyword (with word boundary check)
        many(type_decl),                             // multiple type declarations
        NULL
    );

    combinator_t* param_list = create_pascal_param_parser();

    // Variable declaration for function/procedure local variables
    combinator_t** var_expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *var_expr_parser = new_combinator();
    init_pascal_expression_parser(var_expr_parser);

    combinator_t* typed_var_decl = seq(new_combinator(), PASCAL_T_VAR_DECL,
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))), // variable name(s)
        token(match(":")),                          // colon
        type_definition,                             // full type definitions (array, record, etc.)
        optional(token(match(";"))),                // optional semicolon
        NULL
    );

    combinator_t* inferred_var_decl = seq(new_combinator(), PASCAL_T_VAR_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),          // single variable name
        token(match(":=")),                         // assignment for inference
        lazy(var_expr_parser),                      // initializer expression
        optional(token(match(";"))),               // optional semicolon
        NULL
    );

    combinator_t* var_decl = multi(new_combinator(), PASCAL_T_NONE,
        inferred_var_decl,
        typed_var_decl,
        NULL
    );

    combinator_t* var_section = seq(new_combinator(), PASCAL_T_VAR_SECTION,
        token(keyword_ci("var")),                   // var keyword
        many(var_decl),                            // multiple variable declarations
        NULL
    );
    var_section->extra_to_free = var_expr_parser;

    combinator_t* threadvar_section = seq(new_combinator(), PASCAL_T_VAR_SECTION,
        token(keyword_ci("threadvar")),
        many(typed_var_decl),
        NULL
    );

    // Function/procedure body that can contain local declarations
    combinator_t* function_body = seq(new_combinator(), PASCAL_T_FUNCTION_BODY,
        many(multi(new_combinator(), PASCAL_T_NONE,    // Optional local declarations
            var_section,                               // local variables
            const_section,                             // local constants
            type_section,                              // local types
            NULL
        )),
        lazy(stmt_parser),                         // main statement block
        NULL
    );
    set_combinator_name(function_body, "function_body");

    // Routine directives like inline; overload; etc.
    combinator_t* directive_keyword = multi(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("inline")),
        token(keyword_ci("overload")),
        token(keyword_ci("cdecl")),
        token(keyword_ci("stdcall")),
        token(keyword_ci("register")),
        token(keyword_ci("export")),
        token(keyword_ci("external")),
        token(keyword_ci("assembler")),
        token(keyword_ci("far")),
        token(keyword_ci("near")),
        token(keyword_ci("platform")),
        token(keyword_ci("deprecated")),
        token(keyword_ci("library")),
        token(keyword_ci("local")),
        token(keyword_ci("forward")),
        NULL
    );

    combinator_t* directive_argument = optional(multi(new_combinator(), PASCAL_T_NONE,
        token(string(PASCAL_T_STRING)),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    ));

    combinator_t* routine_directive = seq(new_combinator(), PASCAL_T_NONE,
        directive_keyword,
        directive_argument,
        token(match(";")),
        NULL
    );

    combinator_t* routine_directives = many(routine_directive);

    combinator_t* procedure_header = seq(new_combinator(), PASCAL_T_PROCEDURE_DECL,
        token(keyword_ci("procedure")),
        token(cident(PASCAL_T_IDENTIFIER)),
        param_list,
        token(match(";")),
        routine_directives,
        NULL);

    combinator_t* function_header = seq(new_combinator(), PASCAL_T_FUNCTION_DECL,
        token(keyword_ci("function")),
        token(cident(PASCAL_T_IDENTIFIER)),
        param_list,
        token(match(":")),
        token(cident(PASCAL_T_RETURN_TYPE)),
        token(match(";")),
        routine_directives,
        NULL);

    // Simple procedure implementation for unit
    combinator_t* procedure_impl = seq(new_combinator(), PASCAL_T_PROCEDURE_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("procedure")), token(cident(PASCAL_T_IDENTIFIER)), optional(param_list), token(match(";")),
        routine_directives,
        function_body, optional(token(match(";"))), NULL);
    set_combinator_name(procedure_impl, "procedure_impl");

    // Method implementations with qualified names (Class.Method)
    combinator_t* method_name_with_class = seq(new_combinator(), PASCAL_T_QUALIFIED_IDENTIFIER,
        token(cident(PASCAL_T_IDENTIFIER)),          // class name
        token(match(".")),                           // dot
        token(cident(PASCAL_T_IDENTIFIER)),          // method name
        NULL
    );

    // Return type for functions: : type
    combinator_t* return_type = seq(new_combinator(), PASCAL_T_RETURN_TYPE,
        token(match(":")),                           // colon
        token(cident(PASCAL_T_IDENTIFIER)),          // return type
        NULL
    );

    // Constructor implementation: constructor ClassName.MethodName[(params)]; body
    combinator_t* constructor_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        token(keyword_ci("constructor")),            // constructor keyword
        method_name_with_class,                      // ClassName.MethodName
        optional(param_list),                        // optional parameter list
        token(match(";")),                           // semicolon
        routine_directives,
        function_body,                               // method body with local declarations
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );
    set_combinator_name(constructor_impl, "constructor_impl");

    // Destructor implementation: destructor ClassName.MethodName[(params)]; body
    combinator_t* destructor_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        token(keyword_ci("destructor")),             // destructor keyword
        method_name_with_class,                      // ClassName.MethodName
        optional(param_list),                        // optional parameter list
        token(match(";")),                           // semicolon
        routine_directives,
        function_body,                               // method body with local declarations
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );
    set_combinator_name(destructor_impl, "destructor_impl");

    // Method procedure implementation: procedure ClassName.MethodName[(params)]; body
    combinator_t* method_procedure_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        optional(token(keyword_ci("class"))),        // optional class modifier
        token(keyword_ci("procedure")),              // procedure keyword
        method_name_with_class,                      // ClassName.MethodName
        optional(param_list),                        // optional parameter list
        token(match(";")),                           // semicolon
        routine_directives,
        function_body,                               // method body with local declarations
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );
    set_combinator_name(method_procedure_impl, "method_procedure_impl");

    // Method function implementation: function ClassName.MethodName[(params)]: ReturnType; body
    combinator_t* method_function_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        optional(token(keyword_ci("class"))),        // optional class modifier
        token(keyword_ci("function")),               // function keyword
        method_name_with_class,                      // ClassName.MethodName
        optional(param_list),                        // optional parameter list
        return_type,                                 // return type
        token(match(";")),                           // semicolon
        routine_directives,
        function_body,                               // method body with local declarations
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );
    set_combinator_name(method_function_impl, "method_function_impl");

    // Simple function implementation for unit
    combinator_t* function_impl = seq(new_combinator(), PASCAL_T_FUNCTION_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("function")), token(cident(PASCAL_T_IDENTIFIER)), optional(param_list),
        return_type, token(match(";")),
        routine_directives,
        function_body, optional(token(match(";"))), NULL);
    set_combinator_name(function_impl, "function_impl");

    combinator_t* class_operator_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("operator")),
        method_name_with_class,
        optional(param_list),
        return_type,
        token(match(";")),
        routine_directives,
        function_body,
        optional(token(match(";"))),
        NULL
    );
    set_combinator_name(class_operator_impl, "class_operator_impl");

    // Interface section declarations: uses, const, type, procedure/function headers
    combinator_t* interface_declaration = multi(new_combinator(), PASCAL_T_NONE,
        uses_section,
        const_section,
        resourcestring_section,
        type_section,
        threadvar_section,
        var_section,
        procedure_header,
        function_header,
        NULL
    );
    
    combinator_t* interface_declarations = many(interface_declaration);
    
    // Implementation section can contain both simple implementations and method implementations
    // as well as uses, const, type, and var sections
    combinator_t* implementation_definition = multi(new_combinator(), PASCAL_T_NONE,
        uses_section,                                // uses clauses in implementation
        const_section,                               // const declarations in implementation
        resourcestring_section,
        type_section,                                // type declarations in implementation
        threadvar_section,
        var_section,                                 // var declarations in implementation
        constructor_impl,                            // constructor Class.Method implementations
        destructor_impl,                             // destructor Class.Method implementations
        method_procedure_impl,                       // procedure Class.Method implementations
        class_operator_impl,                         // class operator implementations
        method_function_impl,                        // function Class.Method implementations
        procedure_impl,                              // simple procedure implementations
        function_impl,                               // simple function implementations
        NULL
    );
    set_combinator_name(implementation_definition, "implementation_definition");

    combinator_t* implementation_definitions = many(implementation_definition);
    set_combinator_name(implementation_definitions, "implementation_definitions");

    combinator_t* interface_impl_keyword = token(keyword_ci("implementation"));
    combinator_t* interface_fallback = optional(seq(new_combinator(), PASCAL_T_NONE,
        until(interface_impl_keyword, PASCAL_T_NONE),
        NULL
    ));

    combinator_t* interface_section = seq(new_combinator(), PASCAL_T_INTERFACE_SECTION,
        token(keyword_ci("interface")),
        interface_declarations,
        interface_fallback,
        NULL);
    set_combinator_name(interface_section, "interface_section");

    combinator_t* unit_end_marker = seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("end")),
        token(match(".")),
        NULL
    );

    combinator_t* implementation_end_marker = multi(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("initialization")),
        token(keyword_ci("finalization")),
        token(keyword_ci("exports")),
        unit_end_marker,
        NULL
    );

    combinator_t* implementation_fallback = optional(seq(new_combinator(), PASCAL_T_NONE,
        until(implementation_end_marker, PASCAL_T_NONE),
        NULL
    ));

    combinator_t* implementation_section = seq(new_combinator(), PASCAL_T_IMPLEMENTATION_SECTION,
        token(keyword_ci("implementation")),
        implementation_definitions,
        implementation_fallback,
        NULL);
    set_combinator_name(implementation_section, "implementation_section");

    // Extend the sequence with optional exports/initialization/finalization sections and the final end.
    combinator_t* exports_end_delim = token(match(";"));
    combinator_t* exports_body = until(exports_end_delim, PASCAL_T_NONE);
    combinator_t* exports_section = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("exports")),
        exports_body,
        exports_end_delim,
        NULL
    ));

    combinator_t* initialization_end_marker = multi(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("finalization")),
        unit_end_marker,
        NULL
    );

    combinator_t* initialization_section = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("initialization")),
        map(until(initialization_end_marker, PASCAL_T_NONE), discard_ast),
        NULL
    ));

    combinator_t* finalization_section = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("finalization")),
        map(until(unit_end_marker, PASCAL_T_NONE), discard_ast),
        NULL
    ));

    combinator_t* stmt_list_for_init = sep_end_by(lazy(stmt_parser), token(match(";")));
    combinator_t* legacy_initialization_block = optional(map(seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("begin")),
        stmt_list_for_init,
        NULL
    ), discard_ast));

    combinator_t* unit_semicolon_delim = token(match(";"));
    combinator_t* unit_directives = map(until(unit_semicolon_delim, PASCAL_T_NONE), discard_ast);

    seq(*p, PASCAL_T_UNIT_DECL,
        token(keyword_ci("unit")),
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
        unit_directives,
        token(match(";")),
        interface_section,
        implementation_section,
        exports_section,
        initialization_section,
        finalization_section,
        legacy_initialization_block,
        token(keyword_ci("end")),
        token(match(".")),
        NULL
    );

    (*p)->extra_to_free = stmt_parser;
}

// Pascal Procedure/Function Declaration Parser
void init_pascal_procedure_parser(combinator_t** p) {
    // Create statement parser for procedure/function bodies
    combinator_t** stmt_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *stmt_parser = new_combinator();
    init_pascal_statement_parser(stmt_parser);

    // Parameter: [const|var] identifier1,identifier2,... : type
    combinator_t* param_name_list = sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(",")));
    combinator_t* param = seq(new_combinator(), PASCAL_T_PARAM,
        optional(token(keyword_ci("const"))),        // optional const modifier
        optional(token(keyword_ci("var"))),          // optional var modifier
        param_name_list,                             // parameter name(s) - can be multiple comma-separated
        token(match(":")),                           // colon
        token(cident(PASCAL_T_IDENTIFIER)),          // type name (simplified)
        NULL
    );

    // Parameter list: optional ( param ; param ; ... )
    combinator_t* param_list = optional(between(
        token(match("(")),
        token(match(")")),
        sep_by(param, token(match(";")))
    ));

    // Return type: : type (for functions)
    combinator_t* return_type = seq(new_combinator(), PASCAL_T_RETURN_TYPE,
        token(match(":")),                       // colon
        token(cident(PASCAL_T_IDENTIFIER)),      // return type (simplified)
        NULL
    );

    // Procedure declaration: procedure name [(params)] ; body
    combinator_t* procedure_decl = seq(new_combinator(), PASCAL_T_PROCEDURE_DECL,
        token(match("procedure")),               // procedure keyword
        token(cident(PASCAL_T_IDENTIFIER)),      // procedure name
        param_list,                              // optional parameter list
        token(match(";")),                       // semicolon
        lazy(stmt_parser),                       // procedure body
        NULL
    );

    // Function declaration: function name [(params)] : return_type ; body
    combinator_t* function_decl = seq(new_combinator(), PASCAL_T_FUNCTION_DECL,
        token(match("function")),                // function keyword
        token(cident(PASCAL_T_IDENTIFIER)),      // function name
        param_list,                              // optional parameter list
        return_type,                             // return type
        token(match(";")),                       // semicolon
        lazy(stmt_parser),                       // function body
        NULL
    );

    // Main procedure parser: try function or procedure declaration
    multi(*p, PASCAL_T_NONE,
        function_decl,                           // function declarations first
        procedure_decl,                          // procedure declarations second
        NULL
    );

    (*p)->extra_to_free = stmt_parser;
}

// Pascal Method Implementation Parser - for constructor/destructor/procedure implementations
void init_pascal_method_implementation_parser(combinator_t** p) {
    // Create statement parser for method bodies
    combinator_t** stmt_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *stmt_parser = new_combinator();
    init_pascal_statement_parser(stmt_parser);

    // Parameter: [const|var] identifier1,identifier2,... : type
    combinator_t* param_name_list = sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(",")));
    combinator_t* param = seq(new_combinator(), PASCAL_T_PARAM,
        optional(token(keyword_ci("const"))),        // optional const modifier
        optional(token(keyword_ci("var"))),          // optional var modifier
        param_name_list,                             // parameter name(s) - can be multiple comma-separated
        token(match(":")),                           // colon
        token(cident(PASCAL_T_IDENTIFIER)),          // type name (simplified)
        NULL
    );

    // Parameter list: optional ( param ; param ; ... )
    combinator_t* param_list = optional(between(
        token(match("(")),
        token(match(")")),
        sep_by(param, token(match(";")))
    ));

    // Method name with class: ClassName.MethodName
    combinator_t* method_name_with_class = seq(new_combinator(), PASCAL_T_QUALIFIED_IDENTIFIER,
        token(cident(PASCAL_T_IDENTIFIER)),      // class name
        token(match(".")),                       // dot
        token(cident(PASCAL_T_IDENTIFIER)),      // method name
        NULL
    );

    // Constructor implementation: constructor ClassName.MethodName[(params)]; body
    combinator_t* constructor_impl = seq(new_combinator(), PASCAL_T_CONSTRUCTOR_DECL,
        token(keyword_ci("constructor")),        // constructor keyword
        method_name_with_class,                  // ClassName.MethodName
        param_list,                              // optional parameter list
        token(match(";")),                       // semicolon
        lazy(stmt_parser),                       // method body
        optional(token(match(";"))),             // optional terminating semicolon
        NULL
    );

    // Destructor implementation: destructor ClassName.MethodName[(params)]; body
    combinator_t* destructor_impl = seq(new_combinator(), PASCAL_T_DESTRUCTOR_DECL,
        token(keyword_ci("destructor")),         // destructor keyword
        method_name_with_class,                  // ClassName.MethodName
        param_list,                              // optional parameter list
        token(match(";")),                       // semicolon
        lazy(stmt_parser),                       // method body
        optional(token(match(";"))),             // optional terminating semicolon
        NULL
    );

    // Procedure implementation: procedure ClassName.MethodName[(params)]; body
    combinator_t* procedure_impl = seq(new_combinator(), PASCAL_T_PROCEDURE_DECL,
        token(keyword_ci("procedure")),          // procedure keyword
        method_name_with_class,                  // ClassName.MethodName
        param_list,                              // optional parameter list
        token(match(";")),                       // semicolon
        lazy(stmt_parser),                       // method body
        optional(token(match(";"))),             // optional terminating semicolon
        NULL
    );

    // Method implementation parser: constructor, destructor, or procedure implementation
    multi(*p, PASCAL_T_NONE,
        constructor_impl,
        destructor_impl,
        procedure_impl,
        NULL
    );

    (*p)->extra_to_free = stmt_parser;
}

// Pascal Complete Program Parser - for full Pascal programs
void init_pascal_complete_program_parser(combinator_t** p) {
    // Use `between` to parse the content inside `begin` and `end`, then `map` to wrap it.
    combinator_t* main_block_content_parser = main_block_content(PASCAL_T_NONE);
    combinator_t* main_block_body = between(
        token(keyword_ci("begin")),
        token(keyword_ci("end")),
        main_block_content_parser
    );
    combinator_t* main_block = map(main_block_body, build_main_block_ast);

    // Program parameter list: (identifier, identifier, ...)
    combinator_t* program_param = token(cident(PASCAL_T_IDENTIFIER));
    combinator_t* program_param_list = optional(map(
        between(
            token(match("(")),
            token(match(")")),
            sep_by(program_param, token(match(",")))
        ),
        wrap_program_params
    ));

    // Enhanced Variable declaration: var1, var2, var3 : type;
    combinator_t* var_identifier_list = sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(",")));
    combinator_t* type_spec = multi(new_combinator(), PASCAL_T_TYPE_SPEC,
        class_type(PASCAL_T_CLASS_TYPE),                // class types like class ... end
        record_type(PASCAL_T_RECORD_TYPE),              // record types like record ... end
        enumerated_type(PASCAL_T_ENUMERATED_TYPE),      // enumerated types like (Value1, Value2, Value3)
        array_type(PASCAL_T_ARRAY_TYPE),                // array types like ARRAY[0..9] OF integer
        set_type(PASCAL_T_SET),                         // set types like set of TAsmSehDirective
        pointer_type(PASCAL_T_POINTER_TYPE),            // pointer types like ^TMyObject
        range_type(PASCAL_T_RANGE_TYPE),                // range types like -1..1
        type_name(PASCAL_T_IDENTIFIER),                 // built-in types
        token(cident(PASCAL_T_IDENTIFIER)),             // custom types
        NULL
    );

    combinator_t** program_expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *program_expr_parser = new_combinator();
    init_pascal_expression_parser(program_expr_parser);

    combinator_t* typed_program_var_decl = seq(new_combinator(), PASCAL_T_VAR_DECL,
        var_identifier_list,                            // multiple variable names
        token(match(":")),                              // colon
        type_spec,                                      // type specification
        token(match(";")),                              // semicolon
        NULL
    );

    combinator_t* inferred_program_var_decl = seq(new_combinator(), PASCAL_T_VAR_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),             // single variable name
        token(match(":=")),                            // assignment for inference
        lazy(program_expr_parser),                      // initializer expression
        token(match(";")),                              // semicolon
        NULL
    );

    combinator_t* var_decl = multi(new_combinator(), PASCAL_T_NONE,
        inferred_program_var_decl,
        typed_program_var_decl,
        NULL
    );

    // Var section: var var_decl var_decl ...
    combinator_t* var_section = seq(new_combinator(), PASCAL_T_VAR_SECTION,
        token(keyword_ci("var")),                       // var keyword (with word boundary check)
        many(var_decl),                              // multiple variable declarations
        NULL
    );
    var_section->extra_to_free = program_expr_parser;

    // Type declaration: TypeName = TypeSpec;
    combinator_t* type_decl = seq(new_combinator(), PASCAL_T_TYPE_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),          // type name
        token(match("=")),                           // equals sign
        type_spec,                                   // type specification
        token(match(";")),                           // semicolon
        NULL
    );

    // Type section: type type_decl type_decl ...
    combinator_t* type_section = seq(new_combinator(), PASCAL_T_TYPE_SECTION,
        token(keyword_ci("type")),                      // type keyword (with word boundary check)
        many(type_decl),                             // multiple type declarations
        NULL
    );

    // Uses section: uses unit1, unit2, unit3;
    combinator_t* uses_unit = token(pascal_qualified_identifier(PASCAL_T_USES_UNIT));
    combinator_t* uses_section = seq(new_combinator(), PASCAL_T_USES_SECTION,
        token(keyword_ci("uses")),                      // uses keyword (with word boundary check)
        sep_by(uses_unit, token(match(","))),        // unit names separated by commas
        token(match(";")),                           // semicolon
        NULL
    );

    // Const section: const name : type = value; ...
    // Reuse the expression parser so program-level constants support the
    // complete expression grammar instead of a limited subset.
    combinator_t** program_const_expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *program_const_expr_parser = new_combinator();
    init_pascal_expression_parser(program_const_expr_parser);

    combinator_t* const_value = lazy(program_const_expr_parser);

    combinator_t* const_decl = seq(new_combinator(), PASCAL_T_CONST_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),          // constant name
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(match(":")),                       // colon
            type_spec,                               // full type specification (not just simple identifier)
            NULL
        )),
        token(match("=")),                           // equals
        const_value,                                 // constant value (simplified for now)
        token(match(";")),                           // semicolon
        NULL
    );

    combinator_t* const_section = seq(new_combinator(), PASCAL_T_CONST_SECTION,
        token(keyword_ci("const")),                     // const keyword (with word boundary check)
        many(const_decl),                            // multiple const declarations
        NULL
    );
    const_section->extra_to_free = program_const_expr_parser;

    // Create procedure/function parsers for use in complete program
    // Need to create a modified procedure parser that supports var parameters
    combinator_t** stmt_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *stmt_parser = new_combinator();
    init_pascal_statement_parser(stmt_parser);

    // Enhanced parameter: [const|var] identifier1,identifier2,... : type
    combinator_t* param_name_list = sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(",")));
    combinator_t* param = seq(new_combinator(), PASCAL_T_PARAM,
        optional(token(keyword_ci("const"))),        // optional const modifier
        optional(token(keyword_ci("var"))),          // optional var modifier
        param_name_list,                             // parameter name(s) - can be multiple comma-separated
        token(match(":")),                           // colon
        token(cident(PASCAL_T_IDENTIFIER)),          // type name (simplified)
        NULL
    );

    // Parameter list: optional ( param ; param ; ... )
    combinator_t* param_list = optional(between(
        token(match("(")),
        token(match(")")),
        sep_by(param, token(match(";")))
    ));

    // Return type: : type (for functions)
    combinator_t* return_type = seq(new_combinator(), PASCAL_T_RETURN_TYPE,
        token(match(":")),                           // colon
        token(cident(PASCAL_T_IDENTIFIER)),          // return type (simplified)
        NULL
    );

    // Function body parser: handles local sections followed by begin-end block
    // This is different from statement parsing - functions can have local declarations

    // Local VAR section - reuse the existing var_section parser
    combinator_t* local_var_section = var_section;

    // Allow local CONST/TYPE/VAR sections before the statement body.
    combinator_t* local_decl_sections = many(multi(new_combinator(), PASCAL_T_NONE,
        const_section,
        type_section,
        local_var_section,
        NULL));

    // Create a specialized function body parser that avoids circular references
    // This parser handles the most common function body patterns without full recursive complexity

    // Function body for standalone parsing (no terminating semicolon)
    // Function body that can contain nested function/procedure declarations
    combinator_t** nested_proc_or_func = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *nested_proc_or_func = new_combinator();
    (*nested_proc_or_func)->extra_to_free = nested_proc_or_func;

    // Forward declaration for nested functions - these will refer to working_function and working_procedure below
    combinator_t* nested_function_decl = lazy_owned(nested_proc_or_func);

    combinator_t* nested_function_body = seq(new_combinator(), PASCAL_T_NONE,
        local_decl_sections,                        // zero or more local const/type/var sections
        many(nested_function_decl),                 // zero or more nested function/procedure declarations
        lazy(stmt_parser),                          // begin-end block handled by statement parser
        NULL
    );

    // Use the nested function body parser for complete programs to support nested functions
    combinator_t* program_function_body = nested_function_body;

    // Create simple working function and procedure parsers based on the nested version
    // These work because they use the recursive statement parser for bodies

    // Working function parser: function name [(params)] : return_type ; body ;
    combinator_t* working_function = seq(new_combinator(), PASCAL_T_FUNCTION_DECL,
        token(keyword_ci("function")),               // function keyword (with word boundary check)
        token(cident(PASCAL_T_IDENTIFIER)),          // function name
        param_list,                                  // optional parameter list
        return_type,                                 // return type
        token(match(";")),                           // semicolon after signature
        program_function_body,                       // function body with VAR section support
        optional(token(match(";"))),                 // optional terminating semicolon after function body
        NULL
    );

    // Working procedure parser: procedure name [(params)] ; body ;
    combinator_t* working_procedure = seq(new_combinator(), PASCAL_T_PROCEDURE_DECL,
        token(keyword_ci("procedure")),                // procedure keyword (case-insensitive)
        token(cident(PASCAL_T_IDENTIFIER)),          // procedure name
        param_list,                                  // optional parameter list
        token(match(";")),                           // semicolon after signature
        program_function_body,                       // procedure body with VAR section support
        optional(token(match(";"))),                 // optional terminating semicolon after procedure body
        NULL
    );

    // Object Pascal method implementations (constructor/destructor/procedure with class.method syntax)
    // These are Object Pascal extensions, not standard Pascal
    combinator_t* method_name_with_class = seq(new_combinator(), PASCAL_T_QUALIFIED_IDENTIFIER,
        token(cident(PASCAL_T_IDENTIFIER)),          // class name
        token(match(".")),                           // dot
        token(cident(PASCAL_T_IDENTIFIER)),          // method name
        NULL
    );

    combinator_t* constructor_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        token(keyword_ci("constructor")),              // constructor keyword (with word boundary check)
        method_name_with_class,                      // ClassName.MethodName
        param_list,                                  // optional parameter list
        token(match(";")),                           // semicolon
        lazy(stmt_parser),                           // use statement parser for method body
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );

    combinator_t* destructor_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        token(keyword_ci("destructor")),               // destructor keyword (with word boundary check)
        method_name_with_class,                      // ClassName.MethodName
        param_list,                                  // optional parameter list
        token(match(";")),                           // semicolon
        lazy(stmt_parser),                           // use statement parser for method body
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );

    combinator_t* procedure_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        token(keyword_ci("procedure")),              // procedure keyword (with word boundary check)
        method_name_with_class,                      // ClassName.MethodName
        param_list,                                  // optional parameter list
        token(match(";")),                           // semicolon
        lazy(stmt_parser),                           // use statement parser for method body
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );

    // Object Pascal method implementations - separate from standard Pascal proc_or_func
    combinator_t* method_impl = multi(new_combinator(), PASCAL_T_NONE,
        constructor_impl,
        destructor_impl,
        procedure_impl,
        NULL
    );

    // Standard Pascal procedure or function definitions
    combinator_t* proc_or_func = multi(new_combinator(), PASCAL_T_NONE,
        working_function,                            // working function parser
        working_procedure,                           // working procedure parser
        NULL
    );

    // Combined Pascal and Object Pascal declarations
    combinator_t* all_declarations = multi(new_combinator(), PASCAL_T_NONE,
        proc_or_func,                                // standard Pascal procedures/functions
        method_impl,                                 // Object Pascal method implementations
        NULL
    );

    // Set up the nested function parser to point to the working function/procedure parsers
    // This allows nested function/procedure declarations within function bodies
    multi(*nested_proc_or_func, PASCAL_T_NONE,
        working_function,                            // nested functions
        working_procedure,                           // nested procedures
        NULL
    );

    combinator_t* pre_subprogram_sections = many(multi(new_combinator(), PASCAL_T_NONE,
        const_section,
        type_section,
        var_section,
        NULL));

    combinator_t* post_subprogram_var_sections = many(var_section);

    // Support optional "program" header so unit-less Pascal files can be parsed.
    combinator_t* program_header = seq(new_combinator(), PASCAL_T_PROGRAM_HEADER,
        token(keyword_ci("program")),                   // program keyword (with word boundary check)
        token(cident(PASCAL_T_IDENTIFIER)),          // program name
        program_param_list,                          // optional parameter list
        token(match(";")),                           // semicolon
        NULL
    );

    // Complete program: optional header; optional uses clause; declaration sections; subprograms; optional trailing vars; optional main block.
    seq(*p, PASCAL_T_PROGRAM_DECL,
        optional(program_header),                    // optional "program" header
        optional(uses_section),                      // optional uses clause
        pre_subprogram_sections,                     // const/type/var sections in any order
        many(all_declarations),                      // zero or more procedure/function/method declarations
        post_subprogram_var_sections,                // additional var sections allowed after subprograms
        optional(main_block),                        // optional main program block
        token(match(".")),                           // final period
        NULL
    );

    (*p)->extra_to_free = stmt_parser;
}
