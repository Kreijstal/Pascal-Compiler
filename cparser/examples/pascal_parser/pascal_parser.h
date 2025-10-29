#ifndef PASCAL_PARSER_H
#define PASCAL_PARSER_H

#include "parser.h"
#include "combinators.h"
#include "pascal_expression.h"
#include "pascal_statement.h"
#include "pascal_declaration.h"
#include "pascal_type.h"

// --- Custom Tags for Pascal Expressions ---
typedef enum {
    PASCAL_T_NONE,
    PASCAL_T_INTEGER,
    PASCAL_T_REAL,
    PASCAL_T_IDENTIFIER,
    PASCAL_T_STRING,
    PASCAL_T_CHAR,
    PASCAL_T_BOOLEAN,
    PASCAL_T_ADD,
    PASCAL_T_SUB,
    PASCAL_T_MUL,
    PASCAL_T_DIV,
    PASCAL_T_INTDIV,
    PASCAL_T_MOD,
    PASCAL_T_NEG,
    PASCAL_T_POS,
    PASCAL_T_EQ,
    PASCAL_T_NE,
    PASCAL_T_LT,
    PASCAL_T_GT,
    PASCAL_T_LE,
    PASCAL_T_GE,
    PASCAL_T_AND,
    PASCAL_T_OR,
    PASCAL_T_NOT,
    PASCAL_T_XOR,
    PASCAL_T_SHL,
    PASCAL_T_SHR,
    PASCAL_T_ROL,
    PASCAL_T_ROR,
    PASCAL_T_ADDR,
    PASCAL_T_DEREF,
    PASCAL_T_RANGE,
    PASCAL_T_SET,
    PASCAL_T_IN,
    PASCAL_T_SET_UNION,
    PASCAL_T_SET_INTERSECT,
    PASCAL_T_SET_DIFF,
    PASCAL_T_SET_SYM_DIFF,
    PASCAL_T_IS,
    PASCAL_T_AS,
    PASCAL_T_TYPECAST,
    PASCAL_T_FUNC_CALL,
    PASCAL_T_ARRAY_ACCESS,  // Array access: table[i,j]
    PASCAL_T_MEMBER_ACCESS, // Member access: obj.field
    PASCAL_T_ARG_LIST,
    PASCAL_T_TUPLE,         // Tuple/Array constant: (1,2,3) or ((1,2),(3,4))
    // Procedure/Function Declaration types
    PASCAL_T_PROCEDURE_DECL,
    PASCAL_T_FUNCTION_DECL,
    PASCAL_T_FUNCTION_BODY,
    PASCAL_T_PARAM_LIST,
    PASCAL_T_PARAM,
    PASCAL_T_RETURN_TYPE,
    // Statement types
    PASCAL_T_ASSIGNMENT,
    PASCAL_T_STATEMENT,
    PASCAL_T_STATEMENT_LIST,
    PASCAL_T_IF_STMT,
    PASCAL_T_THEN,
    PASCAL_T_ELSE,
    PASCAL_T_BEGIN_BLOCK,
    PASCAL_T_END_BLOCK,
    PASCAL_T_FOR_STMT,
    PASCAL_T_WHILE_STMT,
    PASCAL_T_REPEAT_STMT,
    PASCAL_T_WITH_STMT,
    PASCAL_T_DO,
    PASCAL_T_TO,
    PASCAL_T_DOWNTO,
    PASCAL_T_CASE_STMT,
    PASCAL_T_CASE_BRANCH,
    PASCAL_T_CASE_LABEL,
    PASCAL_T_CASE_LABEL_LIST,
    PASCAL_T_OF,
    PASCAL_T_ASM_BLOCK,
    // Exception handling types
    PASCAL_T_TRY_BLOCK,
    PASCAL_T_FINALLY_BLOCK,
    PASCAL_T_EXCEPT_BLOCK,
    PASCAL_T_RAISE_STMT,
    PASCAL_T_INHERITED_STMT,
    PASCAL_T_EXIT_STMT,
    PASCAL_T_BREAK_STMT,
    PASCAL_T_ON_CLAUSE,
    // Program structure types
    PASCAL_T_PROGRAM_DECL,
    PASCAL_T_PROGRAM_HEADER,
    PASCAL_T_PROGRAM_PARAMS,
    PASCAL_T_VAR_SECTION,
    PASCAL_T_VAR_DECL,
    PASCAL_T_TYPE_SPEC,
    PASCAL_T_MAIN_BLOCK,
    // Compiler directive types
    PASCAL_T_COMPILER_DIRECTIVE,
    PASCAL_T_COMMENT,
    // Type definition types
    PASCAL_T_TYPE_SECTION,
    PASCAL_T_TYPE_DECL,
    PASCAL_T_RANGE_TYPE,
    PASCAL_T_POINTER_TYPE,
    PASCAL_T_ARRAY_TYPE,
    PASCAL_T_RECORD_TYPE,
    PASCAL_T_ENUMERATED_TYPE,
    PASCAL_T_CLASS_TYPE,
    PASCAL_T_CLASS_MEMBER,
    PASCAL_T_ACCESS_MODIFIER,
    PASCAL_T_CLASS_BODY,
    PASCAL_T_PRIVATE_SECTION,
    PASCAL_T_PUBLIC_SECTION,
    PASCAL_T_PROTECTED_SECTION,
    PASCAL_T_PUBLISHED_SECTION,
    PASCAL_T_FIELD_DECL,
    PASCAL_T_METHOD_DECL,
    PASCAL_T_METHOD_IMPL,
    PASCAL_T_QUALIFIED_IDENTIFIER,
    PASCAL_T_PROPERTY_DECL,
    PASCAL_T_CONSTRUCTOR_DECL,
    PASCAL_T_DESTRUCTOR_DECL,
    // Uses clause types
    PASCAL_T_USES_SECTION,
    PASCAL_T_USES_UNIT,
    // Const section types
    PASCAL_T_CONST_SECTION,
    PASCAL_T_CONST_DECL,
    // Unit-related types
    PASCAL_T_UNIT_DECL,
    PASCAL_T_INTERFACE_SECTION,
    PASCAL_T_IMPLEMENTATION_SECTION,
    // Field width specifier for formatted output
    PASCAL_T_FIELD_WIDTH
} pascal_tag_t;

// --- Function Declarations ---
void init_pascal_program_parser(combinator_t** p);
void init_pascal_unit_parser(combinator_t** p);
void print_pascal_ast(ast_t* ast);
const char* pascal_tag_to_string(tag_t tag);

// --- New Enhanced Functions ---
combinator_t* pascal_comment();
combinator_t* pascal_whitespace();  
combinator_t* pascal_token(combinator_t* p);
combinator_t* token(combinator_t* p);  // Backward compatibility wrapper
combinator_t* pascal_identifier(tag_t tag);  // Pascal identifier that excludes reserved keywords
combinator_t* compiler_directive(tag_t tag);

#endif // PASCAL_PARSER_H
