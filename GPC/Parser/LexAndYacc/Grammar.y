%code requires {
#define YYERROR_VERBOSE 1
#include "Parser/ParseTree/tree.h"
#include "Parser/ParsePascal.h"
#include "Parser/flat_ast.h"
#include "Parser/List/List.h"
#include "Parser/ParseTree/tree_types.h"
}

%{
#include <stdio.h>
#include <string.h>

extern int yylex(void);
extern int line_num;
void yyerror(const char *s);

%}

%define parse.trace

%union {
    int line_num;
    int token;
    int inum;
    float rnum;
    char *str;
    IdBisonUnion id_bison_union;
    FlatNode *flat_node;
    VarDecl_t *var_decl;
    ListNode_t *list_node;
    Param_t *param;
    Type_t *type;
    TypeDecl_t *type_decl;
}

%token <id_bison_union> ID
%token <str> STRING CNAME OVERLOAD
%token <inum> INUM
%token <rnum> RNUM
%token <token> PROGRAM VAR TYPE PROCEDURE FUNCTION BBEGIN END ARRAY OF
%token <token> IF THEN ELSE WHILE DO FOR TO DOWNTO
%token <token> INTEGER REAL PCHAR BOOLEAN STRING_TYPE LONGINT
%token <token> ASSIGNOP COLON SEMI DOT COMMA LPAREN RPAREN LBRACKET RBRACKET
%token <token> EQUAL NOTEQUAL LESSTHAN GREATERTHAN LESSEQUAL GREATEREQUAL
%token <token> PLUS MINUS OR STAR SLASH DIV MOD AND NOT
%token <token> FORWARD ASSEMBLER
%token <str> ASM_BLOCK_STR

%type <id_bison_union> program_heading id_with_options
%type <var_decl> declarations variable_declaration_part variable_declaration_list variable_declaration
%type <list_node> subprogram_declarations identifier_list expression_list optional_parameters parameter_group_list statement_list
%type <param> parameter_specification parameter_group
%type <type> type standard_type
%type <type_decl> type_definition_part type_definition_list type_definition
%type <flat_node> subprogram_declaration subprogram_head statement_seq_opt statement compound_statement for_statement if_statement while_statement assignment_statement procedure_statement variable_access expression simple_expression term factor

%start program

%%

program:
      program_heading SEMI declarations subprogram_declarations compound_statement DOT
        { parse_tree = mk_flat_program($1.line_num, $1.id, $1.args, $3, $4, $5); }
    | program_heading SEMI declarations compound_statement DOT
        { parse_tree = mk_flat_program($1.line_num, $1.id, $1.args, $3, NULL, $4); }
    | program_heading SEMI subprogram_declarations compound_statement DOT
        { parse_tree = mk_flat_program($1.line_num, $1.id, $1.args, NULL, $3, $4); }
    | program_heading SEMI compound_statement DOT
        { parse_tree = mk_flat_program($1.line_num, $1.id, $1.args, NULL, NULL, $3); }
    ;

program_heading:
      PROGRAM ID { $$.id = $2.id; $$.line_num = $2.line_num; $$.args = NULL; }
    | PROGRAM ID LPAREN identifier_list RPAREN { $$.id = $2.id; $$.line_num = $2.line_num; $$.args = $4; }
    ;

identifier_list:
      ID
        { $$ = Cons(strdup($1.id), NULL); }
    | identifier_list COMMA ID
        { $$ = Cons(strdup($3.id), $1); }
    ;

declarations:
      variable_declaration_part
        { $$ = $1; }
    | type_definition_part
        { $$ = NULL; /* TODO: Handle type defs */ }
    | variable_declaration_part type_definition_part
        { $$ = $1; /* TODO: Handle type defs */ }
    | empty
        { $$ = NULL; }
    ;

variable_declaration_part:
      VAR variable_declaration_list
        { $$ = $2; }
    ;

variable_declaration_list:
      variable_declaration
        { $$ = $1; }
    | variable_declaration_list variable_declaration
        { $$ = ChainVarDecl($1, $2); }
    ;

variable_declaration:
      identifier_list COLON type SEMI
        { $$ = mk_var_decl($1, $3); }
    ;

type_definition_part:
      TYPE type_definition_list
        { /* TODO: Handle type defs */ }
    ;

type_definition_list:
      type_definition SEMI
    | type_definition_list type_definition SEMI
    ;

type_definition:
      ID EQUAL type
        { $$ = mk_type_decl($1.id, $3); }
    ;

type:
      standard_type
        { $$ = $1; }
    | ID
        { $$ = mk_type(TYPE_ID, 0, 0, NULL); $$->id = $1.id; }
    | ARRAY LBRACKET INUM DOT DOT INUM RBRACKET OF standard_type
        { $$ = mk_type(TYPE_ARRAY, $3, $6, $9); }
    ;

standard_type:
      INTEGER { $$ = mk_type(TYPE_INTEGER, 0, 0, NULL); }
    | REAL { $$ = mk_type(TYPE_REAL, 0, 0, NULL); }
    | PCHAR { $$ = mk_type(TYPE_PCHAR, 0, 0, NULL); }
    | BOOLEAN { $$ = mk_type(TYPE_BOOLEAN, 0, 0, NULL); }
    | STRING_TYPE { $$ = mk_type(TYPE_STRING, 0, 0, NULL); }
    | LONGINT { $$ = mk_type(TYPE_LONGINT, 0, 0, NULL); }
    ;

subprogram_declarations:
      subprogram_declarations subprogram_declaration SEMI
        { $$ = Cons($2, $1); }
    | empty
        { $$ = NULL; }
    ;

subprogram_declaration:
      subprogram_head declarations compound_statement
        {
            if ($1->node_type == FL_PROCEDURE) {
                $1->data.procedure.local_vars = $2;
                $1->data.procedure.subprograms = NULL;
                $1->data.procedure.compound_statement = $3;
                $$ = $1;
            } else { /* FL_FUNCTION */
                $1->data.function.local_vars = $2;
                $1->data.function.subprograms = NULL;
                $1->data.function.compound_statement = $3;
                $$ = $1;
            }
        }
    | subprogram_head declarations subprogram_declarations compound_statement
        {
            if ($1->node_type == FL_PROCEDURE) {
                $1->data.procedure.local_vars = $2;
                $1->data.procedure.subprograms = $3;
                $1->data.procedure.compound_statement = $4;
                $$ = $1;
            } else { /* FL_FUNCTION */
                $1->data.function.local_vars = $2;
                $1->data.function.subprograms = $3;
                $1->data.function.compound_statement = $4;
                $$ = $1;
            }
        }
    ;

subprogram_head:
      PROCEDURE id_with_options optional_parameters SEMI
        {
            $$ = new_flat_node($2.line_num, FL_PROCEDURE);
            $$->data.procedure.id = $2.id;
            $$->data.procedure.params = $3;
            $$->data.procedure.cname_flag = $2.cname_flag;
            $$->data.procedure.overload_flag = $2.overload_flag;
        }
    | FUNCTION id_with_options optional_parameters COLON standard_type SEMI
        {
            $$ = new_flat_node($2.line_num, FL_FUNCTION);
            $$->data.function.id = $2.id;
            $$->data.function.params = $3;
            $$->data.function.return_type = $5->base_type;
            $$->data.function.return_type_id = NULL;
            $$->data.function.cname_flag = $2.cname_flag;
            $$->data.function.overload_flag = $2.overload_flag;
        }
    | FUNCTION id_with_options optional_parameters COLON ID SEMI
        {
            $$ = new_flat_node($2.line_num, FL_FUNCTION);
            $$->data.function.id = $2.id;
            $$->data.function.params = $3;
            $$->data.function.return_type = TYPE_ID;
            $$->data.function.return_type_id = $5.id;
            $$->data.function.cname_flag = $2.cname_flag;
            $$->data.function.overload_flag = $2.overload_flag;
        }
    ;

id_with_options:
    ID { $$.cname_flag = 0; $$.overload_flag = 0; }
    | ID CNAME { $$.cname_flag = 1; $$.overload_flag = 0; }
    | ID OVERLOAD { $$.cname_flag = 0; $$.overload_flag = 1; }
    | ID CNAME OVERLOAD { $$.cname_flag = 1; $$.overload_flag = 1; }
    | ID OVERLOAD CNAME { $$.cname_flag = 1; $$.overload_flag = 1; }
    ;

optional_parameters:
      LPAREN parameter_group_list RPAREN { $$ = $2; }
    | LPAREN RPAREN { $$ = NULL; }
    | empty { $$ = NULL; }
    ;

parameter_group_list:
      parameter_group
        { $$ = Cons($1, NULL); }
    | parameter_group_list SEMI parameter_group
        { $$ = Cons($3, $1); }
    ;

parameter_group:
      parameter_specification
        { $$ = $1; }
    ;

parameter_specification:
      identifier_list COLON standard_type
        { $$ = mk_param($1, $3->base_type, 0); }
    | VAR identifier_list COLON standard_type
        { $$ = mk_param($2, $4->base_type, 1); }
    ;

statement_seq_opt:
      statement_list
        { $$ = mk_flat_compoundstatement(line_num, $1); }
    | empty
        { $$ = NULL; }
    ;

statement_list:
      statement
        { $$ = Cons($1, NULL); }
    | statement_list SEMI statement
        { $$ = Cons($3, $1); }
    ;

statement:
      compound_statement
    | assignment_statement
    | procedure_statement
    | if_statement
    | while_statement
    | for_statement
    | ASSEMBLER SEMI ASM_BLOCK_STR { $$ = mk_flat_asm_block(line_num, $3); }
    | empty { $$ = NULL; }
    ;

compound_statement:
    BBEGIN statement_seq_opt END { $$ = $2; }
    ;

assignment_statement:
      variable_access ASSIGNOP expression
        { $$ = mk_flat_varassign(line_num, $1, $3); }
    ;

procedure_statement:
      ID
        { $$ = mk_flat_procedurecall($1.line_num, $1.id, NULL); }
    | ID LPAREN RPAREN
        { $$ = mk_flat_procedurecall($1.line_num, $1.id, NULL); }
    | ID LPAREN expression_list RPAREN
        { $$ = mk_flat_procedurecall($1.line_num, $1.id, $3); }
    ;

if_statement:
      IF expression THEN statement
        { $$ = mk_flat_ifthen(line_num, $2, $4, NULL); }
    | IF expression THEN statement ELSE statement
        { $$ = mk_flat_ifthen(line_num, $2, $4, $6); }
    ;

while_statement:
      WHILE expression DO statement
        { $$ = mk_flat_while(line_num, $2, $4); }
    ;

for_statement:
      FOR variable_access ASSIGNOP expression TO expression DO statement
        { $$ = mk_flat_for(line_num, mk_flat_varassign(line_num, $2, $4), $6, NULL, $8); }
    | FOR variable_access ASSIGNOP expression DOWNTO expression DO statement
        { $$ = mk_flat_for(line_num, mk_flat_varassign(line_num, $2, $4), $6, NULL, $8); /* TODO: Handle downto */ }
    ;

variable_access:
      ID
        { $$ = mk_flat_varid($1.line_num, $1.id); }
    | ID LBRACKET expression RBRACKET
        { $$ = mk_flat_arrayaccess($1.line_num, $1.id, $3); }
    ;

expression_list:
      expression
        { $$ = Cons($1, NULL); }
    | expression_list COMMA expression
        { $$ = Cons($3, $1); }
    ;

expression:
      simple_expression
    | simple_expression EQUAL simple_expression { $$ = mk_flat_relop(line_num, EQUAL, $1, $3); }
    | simple_expression NOTEQUAL simple_expression { $$ = mk_flat_relop(line_num, NOTEQUAL, $1, $3); }
    | simple_expression LESSTHAN simple_expression { $$ = mk_flat_relop(line_num, LESSTHAN, $1, $3); }
    | simple_expression GREATERTHAN simple_expression { $$ = mk_flat_relop(line_num, GREATERTHAN, $1, $3); }
    | simple_expression LESSEQUAL simple_expression { $$ = mk_flat_relop(line_num, LESSEQUAL, $1, $3); }
    | simple_expression GREATEREQUAL simple_expression { $$ = mk_flat_relop(line_num, GREATEREQUAL, $1, $3); }
    ;

simple_expression:
      term
    | MINUS term { $$ = mk_flat_unop(line_num, OP_U_MINUS, $2); }
    | PLUS term { $$ = $2; }
    | simple_expression PLUS term { $$ = mk_flat_addop(line_num, PLUS, $1, $3); }
    | simple_expression MINUS term { $$ = mk_flat_addop(line_num, MINUS, $1, $3); }
    | simple_expression OR term { $$ = mk_flat_addop(line_num, OR, $1, $3); }
    ;

term:
      factor
    | term STAR factor { $$ = mk_flat_mulop(line_num, STAR, $1, $3); }
    | term SLASH factor { $$ = mk_flat_mulop(line_num, SLASH, $1, $3); }
    | term DIV factor { $$ = mk_flat_mulop(line_num, DIV, $1, $3); }
    | term MOD factor { $$ = mk_flat_mulop(line_num, MOD, $1, $3); }
    | term AND factor { $$ = mk_flat_mulop(line_num, AND, $1, $3); }
    ;

factor:
      variable_access
        { $$ = $1; }
    | ID LPAREN expression_list RPAREN
        { $$ = mk_flat_functioncall($1.line_num, $1.id, $3); }
    | ID LPAREN RPAREN
        { $$ = mk_flat_functioncall($1.line_num, $1.id, NULL); }
    | INUM
        { $$ = mk_flat_inum(line_num, $1); }
    | RNUM
        { $$ = mk_flat_rnum(line_num, $1); }
    | STRING
        { $$ = mk_flat_string(line_num, $1); }
    | LPAREN expression RPAREN
        { $$ = $2; }
    | NOT factor
        { $$ = mk_flat_unop(line_num, OP_NOT, $2); }
    ;

empty:
    ;

%%

void yyerror(const char *s) {
    fprintf(stderr, "Parse error on line %d: %s\n", line_num, s);
}
