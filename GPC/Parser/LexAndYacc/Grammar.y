/* The Pascal grammar! */
/* If byacc panics from conflicts, look in y.output */

%{
    #include <stdio.h>
    #include <stdlib.h>
    #include "../ErrVars.h"
#include <string.h>
#include <ctype.h>
    void yyerror(const char *s); /* Forward declaration for const-correctness */
    #include "../ParseTree/tree.h"
    #include "../List/List.h"
extern int yylex(void);
extern char *yytext;
extern int yyleng;

    /*extern FILE *yyin;*/
    extern int yylex();
%}

%define parse.error verbose
%union{
    /* Numbers */
    int i_val;
    float f_val;

    /* Strings */
    char *str;

    /* Operators */
    int op_val;

    /* Simple id without a line number */
    char *id;

    /* Identifier with line number */
    struct ident
    {
        char *id;
        int line_num;
    } ident;

    /* Ident list with line numbers */
    struct ident_list
    {
        ListNode_t *list;
        int line_num;
    } ident_list;


    /* For Types */
    struct Type
    {
        int type;

        /* For arrays */
        int actual_type;
        int start;
        int end;
    } type_s;

    /* For Subprogram Headers */
    struct SubprogramHead
    {
        int sub_type;

        char *id;
        ListNode_t *args;
        int line_num;
        int return_type; /* -1 if procedure */
    } subprogram_head_s;

    /* For the for_assign rule */
    struct ForAssign
    {
        int assign_type;
        union for_assign_bison
        {
            struct Statement *stmt;
            struct Expression *expr;
        } for_assign_bison_union;
    } for_assign_bison;

    /* Tree pointers */
    Tree_t *tree;
    struct Statement *stmt;
    struct Expression *expr;

    /* List */
    ListNode_t *list;
}

/* Token keywords */
%token PROGRAM
%token VARIABLE
%token PROCEDURE
%token FUNCTION
%token BBEGIN
%token END

/* A NUM is the actual number, a type is the type declaration (ex: real) */
%token INT_NUM
%token REAL_NUM
%token INT_TYPE
%token REAL_TYPE
%token <str> STRING

%token ID
%token ARRAY
%token SINGLE
%token OF
%token DOTDOT

%token IF
%token THEN
%token ELSE

%token WHILE
%token DO
%token NOT

%token FOR
%token TO

%token END_OF_FILE

/* Expression Tokens */
%token ASSIGNOP
%token RELOP
%token<op_val> EQ NE LT LE GT GE
%token ADDOP
%token<op_val> PLUS MINUS OR
%token MULOP
%token<op_val> STAR SLASH AND
%token PAREN

/* Extra tokens, DO NOT USE IN GRAMMAR RULES! */
%token VAR_ASSIGN
%token VAR

/* THIS SHOULD ONLY BE USED FOR BUILTINS! */
%token BUILTIN_ANY_TYPE

/* THIS SHOULD ONLY BE USED FOR RELOPS. THIS IS NOT A VALID TYPE DECL */
%token BOOL

/* SHOULD ONLY BE USED FOR SEMANTIC CHECKING */
%token UNKNOWN_TYPE

/* Easy fix for the dangling else (borrowed from "lex and yacc" [Levine et al.]) */
%nonassoc THEN
%nonassoc ELSE

%start program /* Explicitly set the start symbol */
/* TYPES FOR THE GRAMMAR */
%type<ident_list> identifier_list
%type<list> optional_program_parameters
%type<list> declarations
%type<list> subprogram_declarations
%type<stmt> compound_statement

%type<type_s> type
%type<i_val> standard_type

%type<tree> subprogram_declaration
%type<subprogram_head_s> subprogram_head
%type<list> arguments
%type<list> parameter_list

%type<list> optional_statements
%type<list> statement_list
%type<stmt> statement
%type<stmt> variable_assignment
%type<stmt> procedure_statement
%type<stmt> if_statement
%type<for_assign_bison> for_assign

%type<expr> variable;
%type<expr> relop_expression
%type<expr> relop_and
%type<expr> relop_not
%type<expr> relop_paren
%type<expr> relop_expression_single

%type<list> expression_list
%type<expr> expression
%type<expr> term
%type<expr> factor
%type<str> string_literal
%type<op_val> sign

/* Rules to extract union values */
%type<ident> ident
%type<i_val> int_num
%type<f_val> real_num
%type<op_val> relop
%type<op_val> addop
%type<op_val> mulop

%%

optional_program_parameters
    : '(' identifier_list ')'
        { $$ = $2.list; }
    | /* empty */
        { $$ = NULL; }
    ;

program
    : PROGRAM ident optional_program_parameters ';'
     declarations
     subprogram_declarations
     compound_statement
     '.'
     END_OF_FILE
     {
         // $1: PROGRAM token
         // $2: ident (type 'ident', provides $2.id and $2.line_num)
         // $3: optional_program_parameters (type 'list', provides ListNode_t* for args)
         // $4: ';' token
         // $5: declarations
         // $6: subprogram_declarations
         // $7: compound_statement
         parse_tree = mk_program($2.line_num, $2.id, $3, $5, $6, $7);
         return -1;
     }
    ;

ident
    : ID
        {
            $$.id = yylval.id;
            $$.line_num = line_num;
        }
    ;

int_num
    : INT_NUM {$$ = yylval.i_val;}
    ;

real_num
    : REAL_NUM {$$ = yylval.f_val;}
    ;

string_literal
    : STRING {$$ = yylval.str;}
    ;

relop
    : RELOP { $$ = yylval.op_val;}
    ;

addop
    : ADDOP {$$ = yylval.op_val;}
    ;

mulop
    : MULOP {$$ = yylval.op_val;}

identifier_list
    : ident
        {
            $$.list = CreateListNode($1.id, LIST_STRING);
            $$.line_num = $1.line_num; /* TODO: List of line nums */
        }
    | identifier_list ',' ident
        {
            $$.list = PushListNodeBack($1.list, CreateListNode($3.id, LIST_STRING));
            $$.line_num = $1.line_num;
        }
    ;

declarations
    : declarations VARIABLE identifier_list ':' type ';'
        {
            Tree_t *tree;
            if($5.type == ARRAY)
                tree = mk_arraydecl($3.line_num, $3.list, $5.actual_type, $5.start, $5.end);
            else
                tree = mk_vardecl($3.line_num, $3.list, $5.actual_type);

            if($1 == NULL)
                $$ = CreateListNode(tree, LIST_TREE);
            else
                $$ = PushListNodeBack($1, CreateListNode(tree, LIST_TREE));
        }
    | /* empty */ {$$ = NULL;}
    ;

type
    : standard_type
        {
            $$.type = SINGLE;
            $$.actual_type = $1;
        }
    | ARRAY '[' int_num DOTDOT int_num ']' OF standard_type
        {
            $$.type = ARRAY;
            $$.actual_type = $8;
            $$.start = $3;
            $$.end = $5;
        }

    ;

standard_type
    : INT_TYPE {$$ = INT_TYPE;}
    | REAL_TYPE {$$ = REAL_TYPE;}
    ;

subprogram_declarations
    : subprogram_declarations subprogram_declaration ';'
        {
            if($1 == NULL)
                $$ = CreateListNode($2, LIST_TREE);
            else
                $$ = PushListNodeBack($1, CreateListNode($2, LIST_TREE));
        }
    | /* empty */ {$$ = NULL;}
    ;

subprogram_declaration
    : subprogram_head
    declarations
    subprogram_declarations
    compound_statement
        {
            if($1.sub_type == PROCEDURE)
                $$ = mk_procedure($1.line_num, $1.id, $1.args, $2, $3, $4);
            else
                $$ = mk_function($1.line_num, $1.id, $1.args, $2, $3, $4, $1.return_type);
        }
    ;

subprogram_head
    : FUNCTION ident arguments ':' standard_type ';'
        {
            $$.sub_type = FUNCTION;
            $$.args = $3;
            $$.return_type = $5;

            $$.id = $2.id;
            $$.line_num = $2.line_num;
        }
    | PROCEDURE ident arguments ';'
        {
            $$.sub_type = PROCEDURE;
            $$.args = $3;
            $$.return_type = -1;

            $$.id = $2.id;
            $$.line_num = $2.line_num;
        }
    ;

arguments
    : '(' parameter_list ')' {$$ = $2;}
    | /* empty */ {$$ = NULL;}
    ;

parameter_list
    : identifier_list ':' type
        {
            Tree_t *tree;
            if($3.type == ARRAY)
                tree = mk_arraydecl($1.line_num, $1.list, $3.actual_type, $3.start, $3.end);
            else
                tree = mk_vardecl($1.line_num, $1.list, $3.actual_type);

            $$ = CreateListNode(tree, LIST_TREE);
        }
    | parameter_list ';' identifier_list ':' type
        {
            Tree_t *tree;
            if($5.type == ARRAY)
                tree = mk_arraydecl($3.line_num, $3.list, $5.actual_type, $5.start, $5.end);
            else
                tree = mk_vardecl($3.line_num, $3.list, $5.actual_type);

            $$ = PushListNodeBack($1, CreateListNode(tree, LIST_TREE));
        }
    ;

compound_statement
    : BBEGIN optional_statements optional_trailing_semicolon END
        {
            $$ = mk_compoundstatement(line_num, $2);
        }
    ;

optional_trailing_semicolon
    : ';'
    | /* empty */
    ;

optional_statements
    : statement_list {$$ = $1;}
    | /* empty */ {$$ = NULL;}
    ;

statement_list
    : statement
        {
            $$ = CreateListNode($1, LIST_STMT);
        }
    | statement_list ';' statement
        {
            $$ = PushListNodeBack($1, CreateListNode($3, LIST_STMT));
        }
    ;

statement
    : variable_assignment
        {
            $$ = $1;
        }
    | procedure_statement
        {
            $$ = $1;
        }
    | compound_statement
        {
            $$ = $1;
        }
    | if_statement
        {
            $$ = $1;
        }
    | WHILE relop_expression DO statement
        {
            $$ = mk_while(line_num, $2, $4);
        }
    | FOR for_assign TO expression DO statement
        {
            if($2.assign_type == VAR_ASSIGN)
                $$ = mk_forassign(line_num, $2.for_assign_bison_union.stmt, $4, $6);
            else
                $$ = mk_forvar(line_num, $2.for_assign_bison_union.expr, $4, $6);
        }
    ;

/* Dangling else solution is in the token section */
if_statement
    : IF relop_expression THEN statement
        {
            $$ = mk_ifthen(line_num, $2, $4, NULL);
        }
    | IF relop_expression THEN statement ELSE statement
        {
            $$ = mk_ifthen(line_num, $2, $4, $6);
        }

variable_assignment
    : variable ASSIGNOP expression
        {
            $$ = mk_varassign(line_num, $1, $3);
        }

for_assign
    : variable_assignment
        {
            $$.assign_type = VAR_ASSIGN;
            $$.for_assign_bison_union.stmt = $1;
        }
    | variable
        {
            $$.assign_type = VAR;
            $$.for_assign_bison_union.expr = $1;
        }

variable
    : ident
        {
            $$ = mk_varid($1.line_num, $1.id);
        }
    | ident '[' expression ']'
        {
            $$ = mk_arrayaccess($1.line_num, $1.id, $3);
        }
    ;

procedure_statement
    : ident
        {
            $$ = mk_procedurecall($1.line_num, $1.id, NULL);
        }
    | ident '(' expression_list ')'
        {
            $$ = mk_procedurecall($1.line_num, $1.id, $3);
        }
    ;

/* RELATIONAL_EXPRESSIONS */

relop_expression
    : relop_expression OR relop_and
        {
            $$ = mk_relop(line_num, OR, $1, $3);
        }
    | relop_and {$$ = $1;}
    ;

relop_and
    : relop_and AND relop_not
        {
            $$ = mk_relop(line_num, AND, $1, $3);
        }
    | relop_not {$$ = $1;}
    ;

relop_not
    : NOT relop_not
        {
            $$ = mk_relop(line_num, NOT, $2, NULL);
        }
    | relop_paren {$$ = $1;}
    ;

relop_paren
    : '(' relop_expression ')' {$$ = $2;}
    | relop_expression_single {$$ = $1;}
    ;

relop_expression_single
    : expression relop expression
        {
            $$ = mk_relop(line_num, $2, $1, $3);
        }
    ;

/* END RELATIONAL_EXPRESSIONS */

expression_list
    : expression
        {
            $$ = CreateListNode($1, LIST_EXPR);
        }
    | expression_list ',' expression
        {
            $$ = PushListNodeBack($1, CreateListNode($3, LIST_EXPR));
        }
    ;

expression
    : term {$$ = $1;}
    | expression addop term
        {
            $$ = mk_addop(line_num, $2, $1, $3);
        }
    ;

term
    : factor {$$ = $1;}
    | term mulop factor
        {
            $$ = mk_mulop(line_num, $2, $1, $3);
        }
    ;

factor
    : ident
        {
            $$ = mk_varid($1.line_num, $1.id);
        }
    | ident '[' expression ']'
        {
            $$ = mk_arrayaccess($1.line_num, $1.id, $3);
        }
    | ident '(' expression_list ')'
        {
            $$ = mk_functioncall($1.line_num, $1.id, $3);
        }
    | int_num
        {
            $$ = mk_inum(line_num, $1);
        }
    | real_num
        {
            $$ = mk_rnum(line_num, $1);
        }
    | string_literal
        {
            $$ = mk_string(line_num, $1);
        }
    | sign factor
        {
            if($1 == MINUS)
                $$ = mk_signterm(line_num, $2);
            else
                $$ = $2;
        }
    | '(' expression ')'
        {
            $$ = $2;
        }
    ;

sign
    : ADDOP
        {
            $$ = yylval.op_val;
        }
    ;

%%

// In Grammar.y's C code section (after %%)
void yyerror(const char *s) { // s is the message from Bison
    fprintf(stderr, "Error");
    if (file_to_parse != NULL && *file_to_parse != '\0') {
        fprintf(stderr, " in '%s'", file_to_parse);
    }

    // Ensure yytext and yyleng are declared extern in %{...%}
    int current_yyleng = (yytext ? yyleng : 0); // Use yyleng if yytext is valid
    int start_col = col_num > current_yyleng ? col_num - current_yyleng + 1 : 1;

    fprintf(stderr, " (line %d, column %d)", line_num, start_col);

    // Bison's error message 's'. With %error-verbose, this might be:
    // "syntax error, unexpected T_SEMICOLON, expecting T_LPAREN or T_IDENTIFIER"
    // or on older Bisons / simpler errors, just "syntax error"
    fprintf(stderr, ": %s\n", s);

    // If 's' is generic, but we have yytext (the unexpected token)
    if (yytext != NULL && *yytext != '\0') {
        char sanitized_yytext[100];
        strncpy(sanitized_yytext, yytext, sizeof(sanitized_yytext) - 1);
        sanitized_yytext[sizeof(sanitized_yytext) - 1] = '\0';
        for (char *p = sanitized_yytext; *p; ++p) {
            if (!isprint((unsigned char)*p) && *p != '\n' && *p != '\t') *p = '?';
        }
        // Only print if 's' doesn't already prominently feature the unexpected token.
        // This check is a bit heuristic.
        if (strstr(s, "unexpected") == NULL || strstr(s, sanitized_yytext) == NULL) {
             fprintf(stderr, "  Unexpected token: \"%s\"\n", sanitized_yytext);
        }
    }
    
    // No direct way to get a list of expected tokens from yyparse's default yyerror(char *s)
    // beyond what 's' provides with %error-verbose.
    // The 'expecting ...' part *is* what Bison puts in 's'.
    // If it's not there, Bison didn't generate it for that specific error condition.

    // Context line printing
    if (file_to_parse != NULL) {
        FILE* f_ctx = fopen(file_to_parse, "r");
        if (f_ctx) {
            char line_buf[256];
            int current_line = 1;
            while (current_line < line_num && fgets(line_buf, sizeof(line_buf), f_ctx)) {
                current_line++;
            }
            if (current_line == line_num && fgets(line_buf, sizeof(line_buf), f_ctx)) {
                line_buf[strcspn(line_buf, "\n\r")] = 0; 
                fprintf(stderr, "  Context: %s\n", line_buf);
                if (start_col > 0) {
                    fprintf(stderr, "           "); 
                    for (int i = 1; i < start_col; i++) fprintf(stderr, " ");
                    fprintf(stderr, "^\n");
                }
            }
            fclose(f_ctx);
        }
    }
    fprintf(stderr, "\n");
}
