#include "pascal_statement.h"
#include "pascal_parser.h"
#include "pascal_expression.h"
#include "pascal_keywords.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// ASM block body parser - uses proper until() combinator instead of manual scanning
static combinator_t* asm_body(tag_t tag) {
    return until(match("end"), tag);  // Use raw match instead of token to preserve whitespace
}

// Transform "x += y" style compound assignments into a regular assignment
// with the addition expression on the right-hand side.  This keeps the rest
// of the compiler unaware of the compound assignment syntax while still
// allowing us to accept programs that use it.
static ast_t* transform_plus_assignment(ast_t* assignment_ast) {
    if (assignment_ast == NULL || assignment_ast == ast_nil)
        return assignment_ast;

    ast_t* lhs = assignment_ast->child;
    if (lhs == NULL)
        return assignment_ast;

    ast_t* rhs = lhs->next;
    if (rhs == NULL)
        return assignment_ast;

    // Detach the right-hand side so copying the lhs does not duplicate it.
    lhs->next = NULL;

    ast_t* lhs_copy = copy_ast(lhs);
    if (lhs_copy == NULL || lhs_copy == ast_nil) {
        lhs->next = rhs;
        return assignment_ast;
    }

    lhs_copy->line = lhs->line;
    lhs_copy->col = lhs->col;

    ast_t* add_node = new_ast();
    add_node->typ = PASCAL_T_ADD;
    add_node->child = lhs_copy;
    add_node->next = NULL;
    add_node->line = assignment_ast->line;
    add_node->col = assignment_ast->col;

    lhs_copy->next = rhs;
    lhs->next = add_node;

    return assignment_ast;
}

// --- Pascal Statement Parser Implementation ---
void init_pascal_statement_parser(combinator_t** p) {
    // First create the expression parser to use within statements
    combinator_t** expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *expr_parser = new_combinator();
    init_pascal_expression_parser(expr_parser);

    // Create the main statement parser pointer for recursive references
    combinator_t** stmt_parser = p;

    // Left-value parser: accepts identifiers, member access expressions, and array access
    combinator_t* simple_identifier = token(pascal_expression_identifier(PASCAL_T_IDENTIFIER));
    combinator_t* member_access_lval = seq(new_combinator(), PASCAL_T_MEMBER_ACCESS,
        token(pascal_expression_identifier(PASCAL_T_IDENTIFIER)),     // object name
        token(match(".")),                                            // dot
        token(pascal_expression_identifier(PASCAL_T_IDENTIFIER)),     // field/property name
        NULL
    );
    // Array access for lvalue: identifier[index, ...]
    combinator_t* array_access_lval = seq(new_combinator(), PASCAL_T_ARRAY_ACCESS,
        token(pascal_expression_identifier(PASCAL_T_IDENTIFIER)),     // array name
        between(token(match("[")), token(match("]")),
            sep_by(lazy(expr_parser), token(match(",")))),             // indices
        NULL
    );
    combinator_t* lvalue = multi(new_combinator(), PASCAL_T_NONE,
        array_access_lval,       // try array access first
        member_access_lval,      // try member access second
        simple_identifier,       // then simple identifier
        NULL
    );

    // Assignment statement: support both ":=" and "+=" compound assignments
    combinator_t* simple_assignment = seq(new_combinator(), PASCAL_T_ASSIGNMENT,
        lvalue,                                // left-hand side (identifier or member access)
        token(match(":=")),                    // assignment operator
        lazy(expr_parser),                     // expression
        NULL
    );

    combinator_t* plus_assignment_seq = seq(new_combinator(), PASCAL_T_ASSIGNMENT,
        lvalue,
        token(match("+=")),
        lazy(expr_parser),
        NULL
    );
    combinator_t* plus_assignment = map(plus_assignment_seq, transform_plus_assignment);

    combinator_t* assignment = multi(new_combinator(), PASCAL_T_NONE,
        plus_assignment,
        simple_assignment,
        NULL
    );

    // Simple expression statement: expression (no semicolon here)
    combinator_t* expr_stmt = seq(new_combinator(), PASCAL_T_STATEMENT,
        lazy(expr_parser),                     // expression
        NULL
    );

    // Begin-end block: begin [statement_list] end
    // Handle empty begin-end blocks explicitly to avoid recursive parsing issues
    combinator_t* empty_begin_end = seq(new_combinator(), PASCAL_T_BEGIN_BLOCK,
        token(keyword_ci("begin")),              // begin keyword
        token(keyword_ci("end")),                // end keyword (immediately after begin)
        NULL
    );

    // Standard BEGIN-END block for main statement parser (uses full recursive parsing)
    // Create custom statement list parser to properly handle Pascal semicolon rules
    // Pascal semicolons are separators, but there can be an optional trailing semicolon

    // Simplified statement list parser - just use sep_by with optional trailing semicolon
    combinator_t* stmt_list = seq(new_combinator(), PASCAL_T_NONE,
        sep_by(lazy(stmt_parser), token(match(";"))),     // statements separated by semicolons
        optional(token(match(";"))),                      // optional trailing semicolon
        NULL
    );

    combinator_t* non_empty_begin_end = seq(new_combinator(), PASCAL_T_BEGIN_BLOCK,
        token(keyword_ci("begin")),              // begin keyword
        stmt_list,                             // statements each terminated by semicolon
        token(keyword_ci("end")),                // end keyword
        NULL
    );

    // Try empty block first, then non-empty
    combinator_t* begin_end_block = multi(new_combinator(), PASCAL_T_NONE,
        empty_begin_end,                       // try empty block first
        non_empty_begin_end,                   // then try non-empty block
        NULL
    );

    // If statement: if expression then statement [else statement]
    combinator_t* if_stmt = seq(new_combinator(), PASCAL_T_IF_STMT,
        token(keyword_ci("if")),                     // if keyword (case-insensitive)
        lazy(expr_parser),                         // condition
        token(keyword_ci("then")),                   // then keyword (case-insensitive)
        lazy(stmt_parser),                         // then statement
        optional(seq(new_combinator(), PASCAL_T_ELSE,    // optional else part
            token(keyword_ci("else")),               // else keyword (case-insensitive)
            lazy(stmt_parser),
            NULL
        )),
        NULL
    );

    // For statement: for [identifier := expression | identifier] (to|downto) expression do statement
    combinator_t* for_direction = multi(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("to")),                 // to keyword (case-insensitive)
        token(keyword_ci("downto")),             // downto keyword (case-insensitive)
        NULL
    );
    combinator_t* for_initializer = multi(new_combinator(), PASCAL_T_NONE,
        assignment,                             // try explicit initializer first (y := 1)
        simple_identifier,                      // fall back to existing variable (for y to ...)
        NULL
    );
    combinator_t* for_stmt = seq(new_combinator(), PASCAL_T_FOR_STMT,
        token(keyword_ci("for")),                // for keyword (case-insensitive)
        for_initializer,                        // loop initializer (assignment or bare identifier)
        for_direction,                          // to or downto
        lazy(expr_parser),                      // end expression
        token(keyword_ci("do")),                 // do keyword (case-insensitive)
        lazy(stmt_parser),                      // loop body statement
        NULL
    );

    // While statement: while expression do statement
    combinator_t* while_stmt = seq(new_combinator(), PASCAL_T_WHILE_STMT,
        token(keyword_ci("while")),              // while keyword (case-insensitive)
        lazy(expr_parser),                     // condition
        token(keyword_ci("do")),                 // do keyword (case-insensitive)
        lazy(stmt_parser),                     // body statement
        NULL
    );

    // Repeat statement: repeat statement_list until expression
    combinator_t* repeat_stmt_list = seq(new_combinator(), PASCAL_T_STATEMENT_LIST,
        sep_by(lazy(stmt_parser), token(match(";"))),    // statements separated by semicolons
        optional(token(match(";"))),                     // optional trailing semicolon
        NULL
    );

    combinator_t* repeat_stmt = seq(new_combinator(), PASCAL_T_REPEAT_STMT,
        token(keyword_ci("repeat")),           // repeat keyword (case-insensitive)
        repeat_stmt_list,                      // repeated statements
        token(keyword_ci("until")),           // until keyword (case-insensitive)
        lazy(expr_parser),                     // termination expression
        NULL
    );

    // With statement: with expression do statement
    combinator_t* with_stmt = seq(new_combinator(), PASCAL_T_WITH_STMT,
        token(keyword_ci("with")),               // with keyword (case-insensitive)
        lazy(expr_parser),                     // expression
        token(keyword_ci("do")),                 // do keyword (case-insensitive)
        lazy(stmt_parser),                     // body statement
        NULL
    );

    // ASM block: asm ... end
    combinator_t* asm_stmt = seq(new_combinator(), PASCAL_T_ASM_BLOCK,
        token(match("asm")),                   // asm keyword
        asm_body(PASCAL_T_NONE),               // asm body content
        token(match("end")),                   // end keyword
        NULL
    );

    // Try-finally block: try statements finally statements end
    // Use more lenient statement parsing that doesn't require semicolons
    combinator_t* try_finally = seq(new_combinator(), PASCAL_T_TRY_BLOCK,
        token(keyword_ci("try")),              // try keyword (case-insensitive)
        many(seq(new_combinator(), PASCAL_T_NONE,
            lazy(stmt_parser),
            optional(token(match(";"))),       // optional semicolon after each statement
            NULL
        )),                                    // statements in try block
        token(keyword_ci("finally")),          // finally keyword (case-insensitive)
        many(seq(new_combinator(), PASCAL_T_NONE,
            lazy(stmt_parser),
            optional(token(match(";"))),       // optional semicolon after each statement
            NULL
        )),                                    // statements in finally block
        token(keyword_ci("end")),              // end keyword (case-insensitive)
        NULL
    );

    // Try-except block: try statements except statements end
    combinator_t* try_except = seq(new_combinator(), PASCAL_T_TRY_BLOCK,
        token(keyword_ci("try")),              // try keyword (case-insensitive)
        many(seq(new_combinator(), PASCAL_T_NONE,
            lazy(stmt_parser),
            optional(token(match(";"))),       // optional semicolon after each statement
            NULL
        )),                                    // statements in try block
        token(keyword_ci("except")),           // except keyword (case-insensitive)
        many(seq(new_combinator(), PASCAL_T_NONE,
            lazy(stmt_parser),
            optional(token(match(";"))),       // optional semicolon after each statement
            NULL
        )),                                    // statements in except block
        token(keyword_ci("end")),              // end keyword (case-insensitive)
        NULL
    );

    // Raise statement: raise [expression]
    combinator_t* raise_stmt = seq(new_combinator(), PASCAL_T_RAISE_STMT,
        token(keyword_ci("raise")),            // raise keyword (case-insensitive)
        optional(lazy(expr_parser)),           // optional exception expression
        NULL
    );

    // Inherited statement: inherited [method_call]
    combinator_t* inherited_stmt = seq(new_combinator(), PASCAL_T_INHERITED_STMT,
        token(keyword_ci("inherited")),        // inherited keyword (case-insensitive)
        optional(lazy(expr_parser)),           // optional method call expression (e.g., inherited Destroy)
        NULL
    );

    // Exit statement: exit
    combinator_t* exit_stmt = token(create_keyword_parser("exit", PASCAL_T_EXIT_STMT));
    combinator_t* break_stmt = token(create_keyword_parser("break", PASCAL_T_BREAK_STMT));

    // Case statement: case expression of label1: stmt1; label2: stmt2; [else stmt;] end
    // Case labels should handle constant expressions, not just simple values
    
    // Constant expressions for case labels - more flexible than simple values
    // but restricted to avoid conflicts with statement parsing
    combinator_t* const_expr_factor = multi(new_combinator(), PASCAL_T_NONE,
        integer(PASCAL_T_INTEGER),
        char_literal(PASCAL_T_CHAR), 
        cident(PASCAL_T_IDENTIFIER),
        between(token(match("(")), token(match(")")), lazy(expr_parser)), // parenthesized expressions
        NULL);
    
    // Allow simple arithmetic in case labels like (CONST + 1) or -5
    combinator_t* case_expression = multi(new_combinator(), PASCAL_T_NONE,
        seq(new_combinator(), PASCAL_T_NEG,
            token(match("-")),
            const_expr_factor,
            NULL),
        seq(new_combinator(), PASCAL_T_POS,
            token(match("+")),
            const_expr_factor,
            NULL),
        const_expr_factor,
        NULL);
    
    // Range case label: expression..expression
    combinator_t* range_case_label = seq(new_combinator(), PASCAL_T_RANGE,
        case_expression,
        token(match("..")),
        case_expression,
        NULL);
    
    combinator_t* case_label = multi(new_combinator(), PASCAL_T_CASE_LABEL,
        token(range_case_label),    // Try range first
        token(case_expression),     // Then single expressions
        NULL
    );
    
    combinator_t* case_label_list = seq(new_combinator(), PASCAL_T_CASE_LABEL_LIST,
        sep_by(case_label, token(match(","))), // labels separated by commas
        NULL
    );
    
    combinator_t* case_branch = seq(new_combinator(), PASCAL_T_CASE_BRANCH,
        case_label_list,                       // case labels
        token(match(":")),                     // colon
        lazy(stmt_parser),                     // statement
        NULL
    );
    
    combinator_t* case_stmt = seq(new_combinator(), PASCAL_T_CASE_STMT,
        token(keyword_ci("case")),             // case keyword
        lazy(expr_parser),                     // case expression
        token(keyword_ci("of")),               // of keyword
        sep_end_by(case_branch, token(match(";"))), // case branches with optional trailing semicolon
        optional(seq(new_combinator(), PASCAL_T_ELSE, // optional else clause
            token(keyword_ci("else")),         // else keyword
            lazy(stmt_parser),                 // else statement
            NULL
        )),
        token(keyword_ci("end")),              // end keyword
        NULL
    );

    // Main statement parser: try different types of statements (order matters!)
    // Note: VAR sections are handled by the complete program parser context
    multi(*stmt_parser, PASCAL_T_NONE,
        begin_end_block,                      // compound statements (must come before expr_stmt)
        try_finally,                          // try-finally blocks
        try_except,                           // try-except blocks
        case_stmt,                            // case statements (before other keyword statements)
        raise_stmt,                           // raise statements
        inherited_stmt,                       // inherited statements
        exit_stmt,                            // exit statements
        break_stmt,                           // break statements
        asm_stmt,                             // inline assembly blocks
        if_stmt,                              // if statements
        for_stmt,                             // for statements
        repeat_stmt,                          // repeat statements
        while_stmt,                           // while statements
        with_stmt,                            // with statements
        assignment,                            // assignment statements
        expr_stmt,                            // expression statements (must be last)
        NULL
    );

    (*p)->extra_to_free = expr_parser;
}
