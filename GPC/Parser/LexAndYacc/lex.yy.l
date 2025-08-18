%option noyywrap
%x ASM_MODE
%x COMMENT_MODE
%x IGNORE_MODE

%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "ErrVars.h"
    #include "../flat_ast.h"
    #include "Parser/ParseTree/tree_types.h"
    #include "Grammar.tab.h"

    int if_stack[100];
    int if_stack_ptr = 0;

    // Hardcoded defined symbols for now
    const char* defined_symbols[] = {"CPUx86_64", NULL};

    int is_defined(const char* symbol) {
        for (int i = 0; defined_symbols[i] != NULL; i++) {
            if (strcmp(defined_symbols[i], symbol) == 0) {
                return 1;
            }
        }
        return 0;
    }
%}

whitespace [ \t]+
integer  [0-9]+
real [0-9]*[.][0-9]+
id   [A-Za-z][A-Za-z0-9_]*
comment [(][*][^*]*[*]+([^*)][^*]*[*]+)*[)]
pascal_string \'([^\']|\'\')*\' 
%%

{whitespace} ;

"{"/[^$] {
    BEGIN(COMMENT_MODE);
}

<COMMENT_MODE>"}" {
    BEGIN(INITIAL);
}

<COMMENT_MODE>(.|\n) {
    if (yytext[0] == '\n') {
        line_num++;
    }
}

"{$ifDef"([ \t]+[A-Za-z0-9_]+)?"}" {
    char symbol[100];
    sscanf(yytext, "{$ifDef %99s}", symbol);
    if (if_stack_ptr > 0 && if_stack[if_stack_ptr-1] == 0) {
        if_stack[if_stack_ptr++] = 0; // nested if in false branch
    } else {
        if_stack[if_stack_ptr++] = is_defined(symbol);
    }
    if (if_stack[if_stack_ptr-1] == 0) {
        BEGIN(IGNORE_MODE);
    }
}

"{$ifNDef"([ \t]+[A-Za-z0-9_]+)?"}" {
    char symbol[100];
    sscanf(yytext, "{$ifNDef %99s}", symbol);
    if (if_stack_ptr > 0 && if_stack[if_stack_ptr-1] == 0) {
        if_stack[if_stack_ptr++] = 0; // nested if in false branch
    } else {
        if_stack[if_stack_ptr++] = !is_defined(symbol);
    }
    if (if_stack[if_stack_ptr-1] == 0) {
        BEGIN(IGNORE_MODE);
    }
}

"{$else}" {
    if (if_stack_ptr > 0) {
        if (if_stack[if_stack_ptr-1] == 1) { // if part was true, ignore else
            BEGIN(IGNORE_MODE);
        } else { // if part was false, parse else
            if (if_stack_ptr > 1 && if_stack[if_stack_ptr-2] == 0) {
                // nested in false branch, still ignore
            } else {
                BEGIN(INITIAL);
            }
        }
    }
}

"{$endIf}" {
    if (if_stack_ptr > 0) {
        if_stack_ptr--;
        if (if_stack_ptr == 0 || if_stack[if_stack_ptr-1] == 1) {
            BEGIN(INITIAL);
        }
    }
}

<IGNORE_MODE>"{$ifDef"([ \t]+[A-Za-z0-9_]+)?"}" { if_stack_ptr++; }
<IGNORE_MODE>"{$ifNDef"([ \t]+[A-Za-z0-9_]+)?"}" { if_stack_ptr++; }
<IGNORE_MODE>"{$else}" {
    if (if_stack_ptr > 0) {
        if (if_stack[if_stack_ptr-1] == 0) { // was in false if
            if (if_stack_ptr > 1 && if_stack[if_stack_ptr-2] == 0) {
                // still ignoring
            } else {
                BEGIN(INITIAL);
            }
        }
    }
}
<IGNORE_MODE>"{$endIf}" { if (if_stack_ptr > 0) if_stack_ptr--; if(if_stack_ptr == 0 || if_stack[if_stack_ptr-1]==1) BEGIN(INITIAL); }
<IGNORE_MODE>(.|\n) {}

"}" { /* might be needed for asmMode */ return '}'; }


{comment}|"//".*   { /* Well documented way to ignore comments */
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[COMMENT] ");
    #endif

    int i;
    for(i = 0; i < strlen(yytext); ++i)
    {
        if(yytext[i] == '\n')
            ++line_num;
    }
}

"\n" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "\n");
    #endif
    ++line_num;
}



<<EOF>> {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[EOF]\n");
    #endif
    return 0;
}

"program"   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[PROGRAM] ");
    #endif
    return PROGRAM;
}

"procedure"   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[PROCEDURE] ");
    #endif
    return PROCEDURE;
}

"function"   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[FUNCTION] ");
    #endif
    return FUNCTION;
}

"begin"   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[BEGIN] ");
    #endif
    return BBEGIN;
}

"end"   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[END] ");
    #endif
    return END;
}


"type" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[TYPE] ");
    #endif
    return TYPE;
}

"var"   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[VAR] ");
    #endif
    return VAR;
}

"array" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[ARRAY] ");
    #endif
    return ARRAY;
}

"of" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[OF] ");
    #endif
    return OF;
}

"overload" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[OVERLOAD] ");
    #endif
    return OVERLOAD;
}

"if" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[IF] ");
    #endif
    return IF;
}

"then" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[THEN] ");
    #endif
    return THEN;
}

"else" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[ELSE] ");
    #endif
    return ELSE;
}

"for" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[FOR] ");
    #endif
    return FOR;
}

"to" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[TO] ");
    #endif
    return TO;
}

"do" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[DO] ");
    #endif
    return DO;
}

"cname" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[CNAME] ");
    #endif
    return CNAME;
}

"while" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[WHILE] ");
    #endif
    return WHILE;
}

";" { return SEMI; }
":" { return COLON; }
"." { return DOT; }
"," { return COMMA; }
"(" { return LPAREN; }
")" { return RPAREN; }
"[" { return LBRACKET; }
"]" { return RBRACKET; }

":="   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[ASSIGNOP] ");
    #endif
    return ASSIGNOP;
}

">"   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[GREATERTHAN] ");
    #endif
    return GREATERTHAN;
}

">="   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[GREATEREQUAL] ");
    #endif
    return GREATEREQUAL;
}

"<"   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[LESSTHAN] ");
    #endif
    return LESSTHAN;
}

"<="   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[LESSEQUAL] ");
    #endif
    return LESSEQUAL;
}

"="   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[EQUAL] ");
    #endif
    return EQUAL;
}

"!="   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[NOTEQUAL] ");
    #endif
    return NOTEQUAL;
}

"and"   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[AND] ");
    #endif
    return AND;
}

"or"   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[OR] ");
    #endif
    return OR;
}

"*" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[STAR] ");
    #endif
    return STAR;
}

"div" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[DIV] ");
    #endif
    return DIV;
}

"/" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[SLASH] ");
    #endif
    return SLASH;
}

"mod" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[MOD] ");
    #endif
    return MOD;
}

"+" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[PLUS] ");
    #endif
    return PLUS;
}

"-" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[MINUS] ");
    #endif
    return MINUS;
}

"integer" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[INTEGER] ");
    #endif
    return INTEGER;
}

"real" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[REAL] ");
    #endif
    return REAL;
}


"string" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[STRING_TYPE] ");
    #endif
    return STRING_TYPE;
}


{integer}   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[INUM:%d] ", atoi(yytext));
    #endif
    yylval.inum = atoi(yytext);
    return INUM;
}

{real}   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[RNUM:%f] ", atof(yytext));
    #endif
    yylval.rnum = atof(yytext);
    return RNUM;
}

{id}   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[ID:%s] ", yytext);
    #endif
    yylval.id_bison_union.id = strdup(yytext);
    yylval.id_bison_union.line_num = line_num;
    yylval.id_bison_union.cname_flag = 0;
    yylval.id_bison_union.overload_flag = 0;
    return ID;
}

{pascal_string} {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[STRING:%s] ", yytext);
    #endif
    /* Remove surrounding quotes and handle escape sequences */
    char *str = strdup(yytext+1); // Skip opening quote
    str[strlen(str)-1] = '\0'; // Remove closing quote
    
    /* Handle escape sequences */
    char *src = str;
    char *dst = str;
    while(*src) {
        if(*src == '\\') {
            src++;
            switch(*src) {
                case 'n': *dst++ = '\n'; break;
                case 't': *dst++ = '\t'; break;
                case '\\': *dst++ = '\\'; break;
                case '"': *dst++ = '"'; break;
                default: *dst++ = *src; break;
            }
            src++;
        } else {
            *dst++ = *src++;
        }
    }
    *dst = '\0';
    
    yylval.str = str;
    return STRING;
}

.   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "{%s} ", yytext);
    #endif
    return yytext[0];
}

%%
