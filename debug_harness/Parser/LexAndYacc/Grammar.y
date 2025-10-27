/* The Pascal grammar! */
/* If byacc panics from conflicts, look in y.output */

%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h> // For strstr, strncpy, strcspn
    #include <ctype.h>  // For isprint
    #include "ErrVars.h"
#include <string.h>
#include <ctype.h>
    void yyerror(const char *s); /* Forward declaration for const-correctness */
    #include "tree.h"
    #include "List.h"

    extern int yylex(void);
    extern char *yytext;
    extern int yyleng;

    /*extern FILE *yyin;*/
    extern int yylex(void); // Standard declaration for yylex
    extern char *yytext;    // Declare Flex's global variable
    extern int yyleng;
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
        char *id;
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
        char *return_type_id;
        int cname_flag;
        int overload_flag;
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

    struct UnitInterface
    {
        ListNode_t *uses;
        ListNode_t *type_decls;
        ListNode_t *var_decls;
    } unit_interface;

    struct UnitImplementation
    {
        ListNode_t *uses;
        ListNode_t *type_decls;
        ListNode_t *var_decls;
        ListNode_t *subprograms;
    } unit_implementation;

    /* Tree pointers */
    Tree_t *tree;
    struct Statement *stmt;
    struct Expression *expr;

    /* List */
    ListNode_t *list;
}

/* Token keywords */
%token PROGRAM
%token UNIT
%token INTERFACE
%token IMPLEMENTATION
%token USES
%token INITIALIZATION
%token FINALIZATION
%token VARIABLE
%token PROCEDURE
%token FUNCTION
%token OVERLOAD
%token BBEGIN
%token END

/* A NUM is the actual number, a type is the type declaration (ex: real) */
%token INT_NUM
%token REAL_NUM
%token INT_TYPE
%token REAL_TYPE
%token LONGINT_TYPE
%token STRING_TYPE
%token <str> STRING
%token <str> ENDASM

%token ID
%token TYPE
%token ARRAY
%token ASSEMBLER
%token ASM
%token CONST
%token ASMMODE
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

%token CNAME

%token END_OF_FILE

/* Expression Tokens */
%token ASSIGNOP
%token RELOP
%token<op_val> EQ NE LT LE GT GE
%token ADDOP
%token<op_val> PLUS MINUS OR
%token MULOP
%token<op_val> STAR SLASH AND MOD DIV
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

%start translation_unit /* Explicitly set the start symbol */
/* TYPES FOR THE GRAMMAR */
%type<ident_list> identifier_list
%type<list> optional_program_parameters
%type<list> declarations
%type<list> declaration_list
%type<list> type_declarations_opt
%type<list> type_declaration_list
%type<list> subprogram_declarations
%type<stmt> compound_statement
%type<list> statement_seq_opt  /* New: For zero or more statements */
%type<list> statement_seq      /* New: For one or more statements */

%type<type_s> type
%type<i_val> standard_type
%type<tree> type_declaration

%type<tree> subprogram_declaration
%type<subprogram_head_s> subprogram_head
%type<list> arguments
%type<list> parameter_list
%type<tree> parameter_item

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
%type<i_val> signed_int
%type<f_val> real_num
%type<op_val> relop
%type<op_val> addop
%type<op_val> mulop

%type<list> uses_clause
%type<list> uses_clause_opt
%type<list> uses_list
%type<unit_interface> interface_section
%type<unit_implementation> implementation_section
%type<stmt> unit_initialization

%%

translation_unit
    : program
    | unit
    ;

optional_program_parameters
    : '(' identifier_list ')'
        { $$ = $2.list; }
    | /* empty */
        { $$ = NULL; }
    ;

program
    : PROGRAM ident optional_program_parameters ';'
     uses_clause_opt
     type_declarations_opt
     declarations
     subprogram_declarations
     compound_statement
     '.'
     END_OF_FILE
     {
         parse_tree = mk_program($2.line_num, $2.id, $3, $5, $7, $6, $8, $9);
         return -1;
     }
    ;

uses_clause
    : USES uses_list ';'
        { $$ = $2; }
    ;

uses_clause_opt
    : uses_clause
        { $$ = $1; }
    | /* empty */
        { $$ = NULL; }
    ;

uses_list
    : ident
        {
            $$ = CreateListNode($1.id, LIST_STRING);
        }
    | uses_list ',' ident
        {
            $$ = PushListNodeBack($1, CreateListNode($3.id, LIST_STRING));
        }
    ;

unit
    : UNIT ident ';'
      interface_section
      implementation_section
      unit_initialization
      END
      '.'
      END_OF_FILE
      {
          parse_tree = mk_unit($2.line_num, $2.id,
              $4.uses, $4.type_decls, $4.var_decls,
              $5.uses, $5.type_decls, $5.var_decls, $5.subprograms,
              $6);
          return -1;
      }
    ;

interface_section
    : INTERFACE uses_clause_opt type_declarations_opt declarations
        {
            $$.uses = $2;
            $$.type_decls = $3;
            $$.var_decls = $4;
        }
    | /* empty */
        {
            $$.uses = NULL;
            $$.type_decls = NULL;
            $$.var_decls = NULL;
        }
    ;

implementation_section
    : IMPLEMENTATION uses_clause_opt type_declarations_opt declarations subprogram_declarations
        {
            $$.uses = $2;
            $$.type_decls = $3;
            $$.var_decls = $4;
            $$.subprograms = $5;
        }
    | IMPLEMENTATION uses_clause_opt type_declarations_opt declarations
        {
            $$.uses = $2;
            $$.type_decls = $3;
            $$.var_decls = $4;
            $$.subprograms = NULL;
        }
    ;

unit_initialization
    : INITIALIZATION compound_statement
        { $$ = $2; }
    | /* empty */
        { $$ = NULL; }
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
    : VARIABLE declaration_list
        { $$ = $2; }
    | /* empty */ {$$ = NULL;}
    ;

declaration_list
    : declaration_list identifier_list ':' type ';'
        {
            Tree_t *tree;
            if($4.type == ARRAY)
                tree = mk_arraydecl($2.line_num, $2.list, $4.actual_type, $4.start, $4.end);
            else
                tree = mk_vardecl($2.line_num, $2.list, $4.actual_type, $4.id, 0);

            if($1 == NULL)
                $$ = CreateListNode(tree, LIST_TREE);
            else
                $$ = PushListNodeBack($1, CreateListNode(tree, LIST_TREE));
        }
    | identifier_list ':' type ';'
        {
            Tree_t *tree;
            if($3.type == ARRAY)
                tree = mk_arraydecl($1.line_num, $1.list, $3.actual_type, $3.start, $3.end);
            else
                tree = mk_vardecl($1.line_num, $1.list, $3.actual_type, $3.id, 0);

            $$ = CreateListNode(tree, LIST_TREE);
        }
    ;

type_declarations_opt
    : TYPE type_declaration_list { $$ = $2; }
    | /* empty */ { $$ = NULL; }
    ;

type_declaration_list
    : type_declaration
        { $$ = CreateListNode($1, LIST_TREE); }
    | type_declaration_list type_declaration
        { $$ = PushListNodeBack($1, CreateListNode($2, LIST_TREE)); }
    ;

type_declaration
    : ident relop signed_int DOTDOT signed_int ';'
    {
        if ($2 != EQ) {
            yyerror("Expected '=' in type declaration");
        }
        $$ = mk_typedecl($1.line_num, $1.id, $3, $5);
    }
    ;

signed_int
    : int_num { $$ = $1; }
    | addop int_num
    {
        if ($1 == MINUS) {
            $$ = -$2;
        } else {
            $$ = $2;
        }
    }
    ;

type
    : standard_type
        {
            $$.type = SINGLE;
            $$.actual_type = $1;
            $$.id = NULL;
        }
    | ARRAY '[' int_num DOTDOT int_num ']' OF standard_type
        {
            $$.type = ARRAY;
            $$.actual_type = $8;
            $$.id = NULL;
            $$.start = $3;
            $$.end = $5;
        }
    | ident
        {
            $$.type = ID;
            $$.id = $1.id;
            $$.actual_type = -1;
        }
    ;

standard_type
    : INT_TYPE {$$ = INT_TYPE;}
    | REAL_TYPE {$$ = REAL_TYPE;}
    | LONGINT_TYPE {$$ = LONGINT_TYPE;}
    | STRING_TYPE {$$ = STRING_TYPE;}
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
    : subprogram_head declarations subprogram_declarations compound_statement
        {
            if($1.sub_type == PROCEDURE)
                $$ = mk_procedure($1.line_num, $1.id, $1.args, $2, $3, $4, $1.cname_flag, $1.overload_flag);
            else
                $$ = mk_function($1.line_num, $1.id, $1.args, $2, $3, $4, $1.return_type, $1.return_type_id, $1.cname_flag, $1.overload_flag);
        }
    ;

subprogram_head
    : FUNCTION ident arguments ':' type ';'
        {
            $$.sub_type = FUNCTION;
            $$.args = $3;
            if ($5.type == ID) {
                $$.return_type = -1;
                $$.return_type_id = $5.id;
            } else {
                $$.return_type = $5.actual_type;
                $$.return_type_id = NULL;
            }

            $$.id = $2.id;
            $$.line_num = $2.line_num;
            $$.cname_flag = 0;
            $$.overload_flag = 0;
        }
    | FUNCTION ident arguments ':' type OVERLOAD ';'
        {
            $$.sub_type = FUNCTION;
            $$.args = $3;
            if ($5.type == ID) {
                $$.return_type = -1;
                $$.return_type_id = $5.id;
            } else {
                $$.return_type = $5.actual_type;
                $$.return_type_id = NULL;
            }

            $$.id = $2.id;
            $$.line_num = $2.line_num;
            $$.cname_flag = 0;
            $$.overload_flag = 1;
        }
    | FUNCTION ident arguments ':' type CNAME ';'
        {
            $$.sub_type = FUNCTION;
            $$.args = $3;
            if ($5.type == ID) {
                $$.return_type = -1;
                $$.return_type_id = $5.id;
            } else {
                $$.return_type = $5.actual_type;
                $$.return_type_id = NULL;
            }

            $$.id = $2.id;
            $$.line_num = $2.line_num;
            $$.cname_flag = 1;
            $$.overload_flag = 0;
        }
    | FUNCTION ident arguments ':' type CNAME OVERLOAD ';'
        {
            $$.sub_type = FUNCTION;
            $$.args = $3;
            if ($5.type == ID) {
                $$.return_type = -1;
                $$.return_type_id = $5.id;
            } else {
                $$.return_type = $5.actual_type;
                $$.return_type_id = NULL;
            }

            $$.id = $2.id;
            $$.line_num = $2.line_num;
            $$.cname_flag = 1;
            $$.overload_flag = 1;
        }
    | PROCEDURE ident arguments ';'
        {
            $$.sub_type = PROCEDURE;
            $$.args = $3;
            $$.return_type = -1;
            $$.return_type_id = NULL;

            $$.id = $2.id;
            $$.line_num = $2.line_num;
            $$.cname_flag = 0;
            $$.overload_flag = 0;
        }
    | PROCEDURE ident arguments OVERLOAD ';'
        {
            $$.sub_type = PROCEDURE;
            $$.args = $3;
            $$.return_type = -1;
            $$.return_type_id = NULL;

            $$.id = $2.id;
            $$.line_num = $2.line_num;
            $$.cname_flag = 0;
            $$.overload_flag = 1;
        }
    | PROCEDURE ident arguments CNAME ';'
        {
            $$.sub_type = PROCEDURE;
            $$.args = $3;
            $$.return_type = -1;
            $$.return_type_id = NULL;

            $$.id = $2.id;
            $$.line_num = $2.line_num;
            $$.cname_flag = 1;
            $$.overload_flag = 0;
        }
    | PROCEDURE ident arguments CNAME OVERLOAD ';'
        {
            $$.sub_type = PROCEDURE;
            $$.args = $3;
            $$.return_type = -1;
            $$.return_type_id = NULL;

            $$.id = $2.id;
            $$.line_num = $2.line_num;
            $$.cname_flag = 1;
            $$.overload_flag = 1;
        }
    ;

arguments
    : '(' parameter_list ')' {$$ = $2;}
    | /* empty */ {$$ = NULL;}
    ;

optional_const
    : CONST
    | /* empty */
    ;

parameter_list
    : parameter_list ';' parameter_item
        { $$ = PushListNodeBack($1, CreateListNode($3, LIST_TREE)); }
    | parameter_item
        { $$ = CreateListNode($1, LIST_TREE); }
    ;

parameter_item
    : optional_const identifier_list ':' type
        {
            Tree_t *tree;
            if($4.type == ARRAY)
                tree = mk_arraydecl($2.line_num, $2.list, $4.actual_type, $4.start, $4.end);
            else
                tree = mk_vardecl($2.line_num, $2.list, $4.actual_type, $4.id, 0);
            $$ = tree;
        }
    | VARIABLE optional_const identifier_list ':' type
        {
            Tree_t *tree;
            if($5.type == ARRAY)
                tree = mk_arraydecl($3.line_num, $3.list, $5.actual_type, $5.start, $5.end);
            else
                tree = mk_vardecl($3.line_num, $3.list, $5.actual_type, $5.id, 1);
            $$ = tree;
        }
    ;

compound_statement
    : BBEGIN statement_seq_opt END { $$ = mk_compoundstatement(line_num, $2); }
    | ASSEMBLER ';' ASM ENDASM { $$ = mk_asmblock(line_num, $4); }
    ;

statement_seq_opt  /* Zero or more statements, handles optional trailing semicolon for the sequence */
    : /* empty */
        { $$ = NULL; }
    | statement_seq
        { $$ = $1; }
    | statement_seq ';' /* A sequence of statements can end with a semicolon */
        { $$ = $1; } /* The list itself is $1, semicolon is consumed */
    ;

statement_seq      /* One or more statements, separated by semicolons */
    : statement
        { $$ = CreateListNode($1, LIST_STMT); }
    | statement_seq ';' statement
        { $$ = PushListNodeBack($1, CreateListNode($3, LIST_STMT)); }
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

void yyerror(const char *s) {
    // Debug output
    fprintf(stderr, "Debug yyerror: s = \"%s\", yytext = \"%s\", yyleng = %d, line_num = %d, col_num = %d, file_to_parse = %s\n",
        s, (yytext ? yytext : "null"), yyleng, line_num, col_num, (file_to_parse ? file_to_parse : "null"));

    // Main error message
    fprintf(stderr, "Error");
    if (file_to_parse != NULL && *file_to_parse != '\0') {
        fprintf(stderr, " in '%s'", file_to_parse);
    }

    // Calculate column position
    int current_yyleng = (yytext ? yyleng : 0);
    int start_col = col_num > current_yyleng ? col_num - current_yyleng + 1 : 1;
    fprintf(stderr, " (line %d, column %d)", line_num, start_col);
    fprintf(stderr, ": %s\n", s);

    // Handle unexpected token display
    if (yytext != NULL && *yytext != '\0') {
        char sanitized_yytext[100];
        strncpy(sanitized_yytext, yytext, sizeof(sanitized_yytext) - 1);
        sanitized_yytext[sizeof(sanitized_yytext) - 1] = '\0';

        // Sanitize non-printable characters
        for (char *p = sanitized_yytext; *p; ++p) {
            if (!isprint((unsigned char)*p) && *p != '\n' && *p != '\t') {
                *p = '?';
            }
        }

        // Only print if 's' doesn't already feature the token
        if (strstr(s, "unexpected") == NULL || strstr(s, sanitized_yytext) == NULL) {
            fprintf(stderr, "  Unexpected token: \"%s\"\n", sanitized_yytext);
        }
    }

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
                    for (int i = 1; i < start_col; i++) {
                        fprintf(stderr, " ");
                    }
                    fprintf(stderr, "^\n");
                }
            }
            fclose(f_ctx);
        }
    }
    fprintf(stderr, "\n");
}
