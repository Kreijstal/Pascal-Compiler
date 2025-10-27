#include "pascal_parser.h"
#include "pascal_keywords.h"
#include "pascal_type.h"
#include "pascal_expression.h"
#include "pascal_statement.h"
#include "pascal_declaration.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// --- Forward Declarations ---
extern ast_t* ast_nil;  // From parser.c

// --- Helper Functions ---
static bool is_whitespace_char(char c) {
    return isspace((unsigned char)c);
}

// Pascal-style comment parser using proper combinators: { comment content }
combinator_t* pascal_comment() {
    return seq(new_combinator(), PASCAL_T_NONE,
        match("{"),
        until(match("}"), PASCAL_T_NONE),
        match("}"),
        NULL);
}

// Pascal-style parentheses comment parser: (* comment content *)
combinator_t* pascal_paren_comment() {
    return seq(new_combinator(), PASCAL_T_NONE,
        match("(*"),
        until(match("*)"), PASCAL_T_NONE),
        match("*)"),
        NULL);
}

// C++-style comment parser: // comment content until end of line
combinator_t* cpp_comment() {
    return seq(new_combinator(), PASCAL_T_NONE,
        match("//"),
        until(match("\n"), PASCAL_T_NONE),
        optional(match("\n")),  // consume the newline if present
        NULL);
}

// Compiler directive parser: {$directive parameter}
combinator_t* compiler_directive(tag_t tag) {
    return right(
        match("{$"),
        left(
            until(match("}"), tag),
            match("}")
        )
    );
}

// Enhanced whitespace parser that handles whitespace, Pascal comments, C++ comments, and compiler directives
combinator_t* pascal_whitespace() {
    combinator_t* ws_char = satisfy(is_whitespace_char, PASCAL_T_NONE);
    combinator_t* pascal_comment_parser = pascal_comment();
    combinator_t* pascal_paren_comment_parser = pascal_paren_comment();
    combinator_t* cpp_comment_parser = cpp_comment();
    combinator_t* directive = compiler_directive(PASCAL_T_NONE);  // Treat directives as ignorable whitespace
    combinator_t* ws_or_comment = multi(new_combinator(), PASCAL_T_NONE,
        ws_char,
        pascal_comment_parser,
        pascal_paren_comment_parser,
        cpp_comment_parser,
        directive,  // Include compiler directives in whitespace
        NULL
    );
    return many(ws_or_comment);
}

// Renamed token parser with better Pascal-aware whitespace handling
combinator_t* pascal_token(combinator_t* p) {
    combinator_t* ws = pascal_whitespace();
    return right(ws, left(p, pascal_whitespace()));
}

// Backward compatibility: keep old token() function name for existing code
combinator_t* token(combinator_t* p) {
    return pascal_token(p);
}



const char* pascal_tag_to_string(tag_t tag) {
    switch (tag) {
        case PASCAL_T_NONE: return "NONE";
        case PASCAL_T_INTEGER: return "INTEGER";
        case PASCAL_T_REAL: return "REAL";
        case PASCAL_T_IDENTIFIER: return "IDENTIFIER";
        case PASCAL_T_STRING: return "STRING";
        case PASCAL_T_CHAR: return "CHAR";
        case PASCAL_T_BOOLEAN: return "BOOLEAN";
        case PASCAL_T_ADD: return "ADD";
        case PASCAL_T_SUB: return "SUB";
        case PASCAL_T_MUL: return "MUL";
        case PASCAL_T_DIV: return "DIV";
        case PASCAL_T_INTDIV: return "INTDIV";
        case PASCAL_T_MOD: return "MOD";
        case PASCAL_T_NEG: return "NEG";
        case PASCAL_T_POS: return "POS";
        case PASCAL_T_EQ: return "EQ";
        case PASCAL_T_NE: return "NE";
        case PASCAL_T_LT: return "LT";
        case PASCAL_T_GT: return "GT";
        case PASCAL_T_LE: return "LE";
        case PASCAL_T_GE: return "GE";
        case PASCAL_T_AND: return "AND";
        case PASCAL_T_OR: return "OR";
        case PASCAL_T_NOT: return "NOT";
        case PASCAL_T_XOR: return "XOR";
        case PASCAL_T_SHL: return "SHL";
        case PASCAL_T_SHR: return "SHR";
        case PASCAL_T_ADDR: return "ADDR";
        case PASCAL_T_DEREF: return "DEREF";
        case PASCAL_T_RANGE: return "RANGE";
        case PASCAL_T_SET: return "SET";
        case PASCAL_T_IN: return "IN";
        case PASCAL_T_SET_UNION: return "SET_UNION";
        case PASCAL_T_SET_INTERSECT: return "SET_INTERSECT";
        case PASCAL_T_SET_DIFF: return "SET_DIFF";
        case PASCAL_T_SET_SYM_DIFF: return "SET_SYM_DIFF";
        case PASCAL_T_IS: return "IS";
        case PASCAL_T_AS: return "AS";
        case PASCAL_T_TYPECAST: return "TYPECAST";
        case PASCAL_T_FUNC_CALL: return "FUNC_CALL";
        case PASCAL_T_ARRAY_ACCESS: return "ARRAY_ACCESS";
        case PASCAL_T_MEMBER_ACCESS: return "MEMBER_ACCESS";
        case PASCAL_T_ARG_LIST: return "ARG_LIST";
        case PASCAL_T_TUPLE: return "TUPLE";
        case PASCAL_T_PROCEDURE_DECL: return "PROCEDURE_DECL";
        case PASCAL_T_FUNCTION_DECL: return "FUNCTION_DECL";
        case PASCAL_T_FUNCTION_BODY: return "FUNCTION_BODY";
        case PASCAL_T_PARAM_LIST: return "PARAM_LIST";
        case PASCAL_T_PARAM: return "PARAM";
        case PASCAL_T_RETURN_TYPE: return "RETURN_TYPE";
        case PASCAL_T_ASSIGNMENT: return "ASSIGNMENT";
        case PASCAL_T_STATEMENT: return "STATEMENT";
        case PASCAL_T_STATEMENT_LIST: return "STATEMENT_LIST";
        case PASCAL_T_IF_STMT: return "IF_STMT";
        case PASCAL_T_THEN: return "THEN";
        case PASCAL_T_ELSE: return "ELSE";
        case PASCAL_T_BEGIN_BLOCK: return "BEGIN_BLOCK";
        case PASCAL_T_END_BLOCK: return "END_BLOCK";
        case PASCAL_T_FOR_STMT: return "FOR_STMT";
        case PASCAL_T_WHILE_STMT: return "WHILE_STMT";
        case PASCAL_T_WITH_STMT: return "WITH_STMT";
        case PASCAL_T_DO: return "DO";
        case PASCAL_T_TO: return "TO";
        case PASCAL_T_DOWNTO: return "DOWNTO";
        case PASCAL_T_CASE_STMT: return "CASE_STMT";
        case PASCAL_T_CASE_BRANCH: return "CASE_BRANCH";
        case PASCAL_T_CASE_LABEL: return "CASE_LABEL";
        case PASCAL_T_CASE_LABEL_LIST: return "CASE_LABEL_LIST";
        case PASCAL_T_OF: return "OF";
        case PASCAL_T_ASM_BLOCK: return "ASM_BLOCK";
        // Exception handling types
        case PASCAL_T_TRY_BLOCK: return "TRY_BLOCK";
        case PASCAL_T_FINALLY_BLOCK: return "FINALLY_BLOCK";
        case PASCAL_T_EXCEPT_BLOCK: return "EXCEPT_BLOCK";
        case PASCAL_T_RAISE_STMT: return "RAISE_STMT";
        case PASCAL_T_INHERITED_STMT: return "INHERITED_STMT";
        case PASCAL_T_EXIT_STMT: return "EXIT_STMT";
        case PASCAL_T_ON_CLAUSE: return "ON_CLAUSE";
        case PASCAL_T_PROGRAM_DECL: return "PROGRAM_DECL";
        case PASCAL_T_PROGRAM_HEADER: return "PROGRAM_HEADER";
        case PASCAL_T_PROGRAM_PARAMS: return "PROGRAM_PARAMS";
        case PASCAL_T_VAR_SECTION: return "VAR_SECTION";
        case PASCAL_T_VAR_DECL: return "VAR_DECL";
        case PASCAL_T_TYPE_SPEC: return "TYPE_SPEC";
        case PASCAL_T_MAIN_BLOCK: return "MAIN_BLOCK";
        // New enhanced parser types
        case PASCAL_T_COMPILER_DIRECTIVE: return "COMPILER_DIRECTIVE";
        case PASCAL_T_COMMENT: return "COMMENT";
        case PASCAL_T_TYPE_SECTION: return "TYPE_SECTION";
        case PASCAL_T_TYPE_DECL: return "TYPE_DECL";
        case PASCAL_T_RANGE_TYPE: return "RANGE_TYPE";
        case PASCAL_T_POINTER_TYPE: return "POINTER_TYPE";
        case PASCAL_T_ARRAY_TYPE: return "ARRAY_TYPE";
        case PASCAL_T_RECORD_TYPE: return "RECORD_TYPE";
        case PASCAL_T_ENUMERATED_TYPE: return "ENUMERATED_TYPE";
        case PASCAL_T_CLASS_TYPE: return "CLASS_TYPE";
        case PASCAL_T_CLASS_MEMBER: return "CLASS_MEMBER";
        case PASCAL_T_ACCESS_MODIFIER: return "ACCESS_MODIFIER";
        case PASCAL_T_CLASS_BODY: return "CLASS_BODY";
        case PASCAL_T_PRIVATE_SECTION: return "PRIVATE_SECTION";
        case PASCAL_T_PUBLIC_SECTION: return "PUBLIC_SECTION";
        case PASCAL_T_PROTECTED_SECTION: return "PROTECTED_SECTION";
        case PASCAL_T_PUBLISHED_SECTION: return "PUBLISHED_SECTION";
        case PASCAL_T_FIELD_DECL: return "FIELD_DECL";
        case PASCAL_T_METHOD_DECL: return "METHOD_DECL";
        case PASCAL_T_METHOD_IMPL: return "METHOD_IMPL";
        case PASCAL_T_QUALIFIED_IDENTIFIER: return "QUALIFIED_IDENTIFIER";
        case PASCAL_T_PROPERTY_DECL: return "PROPERTY_DECL";
        case PASCAL_T_CONSTRUCTOR_DECL: return "CONSTRUCTOR_DECL";
        case PASCAL_T_DESTRUCTOR_DECL: return "DESTRUCTOR_DECL";
        // Uses clause types
        case PASCAL_T_USES_SECTION: return "USES_SECTION";
        case PASCAL_T_USES_UNIT: return "USES_UNIT";
        // Const section types
        case PASCAL_T_CONST_SECTION: return "CONST_SECTION";
        case PASCAL_T_CONST_DECL: return "CONST_DECL";
        // Unit-related types
        case PASCAL_T_UNIT_DECL: return "UNIT_DECL";
        case PASCAL_T_INTERFACE_SECTION: return "INTERFACE_SECTION";
        case PASCAL_T_IMPLEMENTATION_SECTION: return "IMPLEMENTATION_SECTION";
        // Field width specifier
        case PASCAL_T_FIELD_WIDTH: return "FIELD_WIDTH";
        default:
            fprintf(stderr, "FATAL: Unknown Pascal AST node type: %d in %s at %s:%d\n", tag, __func__, __FILE__, __LINE__);
            abort();
    }
}

static void print_ast_recursive(ast_t* ast, int depth) {
    if (ast == NULL || ast == ast_nil) return;
    for (int i = 0; i < depth; i++) printf("  ");
    printf("(%s", pascal_tag_to_string(ast->typ));
    if (ast->sym) printf(" %s", ast->sym->name);

    ast_t* child = ast->child;
    if (child) {
        printf("\n");
        print_ast_recursive(child, depth + 1);
    }
    printf(")");

    if (ast->next) {
        printf("\n");
        print_ast_recursive(ast->next, depth);
    }
}

void print_pascal_ast(ast_t* ast) {
    print_ast_recursive(ast, 0);
    printf("\n");
}


