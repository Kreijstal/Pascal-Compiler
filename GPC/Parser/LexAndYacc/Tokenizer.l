%option noyywrap
%x ASM_MODE
%x COMMENT_MODE
%x IGNORE_MODE

%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "../ErrVars.h"
    #include "../ParseTree/tree.h"
    #include "../List/List.h"
    #include "Grammar.tab.h"

    char asm_buffer[1024];
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
    return END_OF_FILE;
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

"asm" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[ASM] ");
    #endif
    asm_buffer[0] = '\0';
    BEGIN(ASM_MODE);
    return ASM;
}

<ASM_MODE>end {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[ENDASM] ");
    #endif
    yylval.str = strdup(asm_buffer);
    BEGIN(INITIAL);
    return ENDASM;
}

<ASM_MODE>(.|\n) {
    if (strlen(asm_buffer) + strlen(yytext) < sizeof(asm_buffer)) {
        strcat(asm_buffer, yytext);
    } else {
        fprintf(stderr, "ASM buffer overflow\n");
        exit(1);
    }
}


"assembler" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[ASSEMBLER] ");
    #endif
    return ASSEMBLER;
}

"const" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[CONST] ");
    #endif
    return CONST;
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
    return VARIABLE;
}

"array" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[ARRAY] ");
    #endif
    return ARRAY;
}

".." {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[DOTDOT] ");
    #endif
    return DOTDOT;
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

":="   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[ASSIGNOP] ");
    #endif
    return ASSIGNOP;
}

">"   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[RELOP:%s] ", yytext);
    #endif
    yylval.op_val = GT;
    return RELOP;
}

">="   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[RELOP:%s] ", yytext);
    #endif
    yylval.op_val = GE;
    return RELOP;
}

"<"   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[RELOP:%s] ", yytext);
    #endif
    yylval.op_val = LT;
    return RELOP;
}

"<="   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[RELOP:%s] ", yytext);
    #endif
    yylval.op_val = LE;
    return RELOP;
}

"="   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[RELOP:%s] ", yytext);
    #endif
    yylval.op_val = EQ;
    return RELOP;
}

"!="   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[RELOP:%s] ", yytext);
    #endif
    yylval.op_val = NE;
    return RELOP;
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
        fprintf(stderr, "[MULOP:%s] ", yytext);
    #endif
    yylval.op_val = STAR;
    return MULOP;
}

"div"|"/" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[MULOP:%s] ", yytext);
    #endif
    yylval.op_val = SLASH;
    return MULOP;
}

"mod" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[MULOP:%s] ", yytext);
    #endif
    yylval.op_val = MOD;
    return MULOP;
}

"+" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[ADDOP:%s] ", yytext);
    #endif
    yylval.op_val = PLUS;
    return ADDOP;
}

"-" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[ADDOP:%s] ", yytext);
    #endif
    yylval.op_val = MINUS;
    return ADDOP;
}

"integer" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[INT_TYPE] ");
    #endif
    return INT_TYPE;
}

"real" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[REAL_TYPE] ");
    #endif
    return REAL_TYPE;
}

"longint" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[LONGINT_TYPE] ");
    #endif
    return LONGINT_TYPE;
}

"string" {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[STRING_TYPE] ");
    #endif
    return STRING_TYPE;
}


{integer}   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[INT_NUM:%d] ", atoi(yytext));
    #endif
    yylval.i_val = atoi(yytext);

    return INT_NUM;
}

{real}   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[REAL_NUM:%f] ", atof(yytext));
    #endif
    yylval.f_val = atof(yytext);
    return REAL_NUM;
}

{id}   {
    #ifdef DEBUG_FLEX
        fprintf(stderr, "[ID:%s] ", yytext);
    #endif
    yylval.id = strdup(yytext);
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
