/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_GRAMMAR_TAB_H_INCLUDED
# define YY_YY_GRAMMAR_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    PROGRAM = 258,                 /* PROGRAM  */
    VARIABLE = 259,                /* VARIABLE  */
    PROCEDURE = 260,               /* PROCEDURE  */
    FUNCTION = 261,                /* FUNCTION  */
    OVERLOAD = 262,                /* OVERLOAD  */
    BBEGIN = 263,                  /* BBEGIN  */
    END = 264,                     /* END  */
    INT_NUM = 265,                 /* INT_NUM  */
    REAL_NUM = 266,                /* REAL_NUM  */
    INT_TYPE = 267,                /* INT_TYPE  */
    REAL_TYPE = 268,               /* REAL_TYPE  */
    LONGINT_TYPE = 269,            /* LONGINT_TYPE  */
    STRING_TYPE = 270,             /* STRING_TYPE  */
    STRING = 271,                  /* STRING  */
    ENDASM = 272,                  /* ENDASM  */
    ID = 273,                      /* ID  */
    TYPE = 274,                    /* TYPE  */
    ARRAY = 275,                   /* ARRAY  */
    ASSEMBLER = 276,               /* ASSEMBLER  */
    ASM = 277,                     /* ASM  */
    CONST = 278,                   /* CONST  */
    ASMMODE = 279,                 /* ASMMODE  */
    SINGLE = 280,                  /* SINGLE  */
    OF = 281,                      /* OF  */
    DOTDOT = 282,                  /* DOTDOT  */
    IF = 283,                      /* IF  */
    THEN = 284,                    /* THEN  */
    ELSE = 285,                    /* ELSE  */
    WHILE = 286,                   /* WHILE  */
    DO = 287,                      /* DO  */
    NOT = 288,                     /* NOT  */
    FOR = 289,                     /* FOR  */
    TO = 290,                      /* TO  */
    CNAME = 291,                   /* CNAME  */
    END_OF_FILE = 292,             /* END_OF_FILE  */
    ASSIGNOP = 293,                /* ASSIGNOP  */
    RELOP = 294,                   /* RELOP  */
    EQ = 295,                      /* EQ  */
    NE = 296,                      /* NE  */
    LT = 297,                      /* LT  */
    LE = 298,                      /* LE  */
    GT = 299,                      /* GT  */
    GE = 300,                      /* GE  */
    ADDOP = 301,                   /* ADDOP  */
    PLUS = 302,                    /* PLUS  */
    MINUS = 303,                   /* MINUS  */
    OR = 304,                      /* OR  */
    MULOP = 305,                   /* MULOP  */
    STAR = 306,                    /* STAR  */
    SLASH = 307,                   /* SLASH  */
    AND = 308,                     /* AND  */
    MOD = 309,                     /* MOD  */
    DIV = 310,                     /* DIV  */
    PAREN = 311,                   /* PAREN  */
    VAR_ASSIGN = 312,              /* VAR_ASSIGN  */
    VAR = 313,                     /* VAR  */
    BUILTIN_ANY_TYPE = 314,        /* BUILTIN_ANY_TYPE  */
    BOOL = 315,                    /* BOOL  */
    UNKNOWN_TYPE = 316             /* UNKNOWN_TYPE  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 27 "Grammar.y"

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

    /* Tree pointers */
    Tree_t *tree;
    struct Statement *stmt;
    struct Expression *expr;

    /* List */
    ListNode_t *list;

#line 200 "Grammar.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_GRAMMAR_TAB_H_INCLUDED  */
