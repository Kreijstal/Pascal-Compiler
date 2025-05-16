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
    BBEGIN = 262,                  /* BBEGIN  */
    END = 263,                     /* END  */
    INT_NUM = 264,                 /* INT_NUM  */
    REAL_NUM = 265,                /* REAL_NUM  */
    INT_TYPE = 266,                /* INT_TYPE  */
    REAL_TYPE = 267,               /* REAL_TYPE  */
    STRING = 268,                  /* STRING  */
    ID = 269,                      /* ID  */
    ARRAY = 270,                   /* ARRAY  */
    SINGLE = 271,                  /* SINGLE  */
    OF = 272,                      /* OF  */
    DOTDOT = 273,                  /* DOTDOT  */
    IF = 274,                      /* IF  */
    THEN = 275,                    /* THEN  */
    ELSE = 276,                    /* ELSE  */
    WHILE = 277,                   /* WHILE  */
    DO = 278,                      /* DO  */
    NOT = 279,                     /* NOT  */
    FOR = 280,                     /* FOR  */
    TO = 281,                      /* TO  */
    END_OF_FILE = 282,             /* END_OF_FILE  */
    ASSIGNOP = 283,                /* ASSIGNOP  */
    RELOP = 284,                   /* RELOP  */
    EQ = 285,                      /* EQ  */
    NE = 286,                      /* NE  */
    LT = 287,                      /* LT  */
    LE = 288,                      /* LE  */
    GT = 289,                      /* GT  */
    GE = 290,                      /* GE  */
    ADDOP = 291,                   /* ADDOP  */
    PLUS = 292,                    /* PLUS  */
    MINUS = 293,                   /* MINUS  */
    OR = 294,                      /* OR  */
    MULOP = 295,                   /* MULOP  */
    STAR = 296,                    /* STAR  */
    SLASH = 297,                   /* SLASH  */
    AND = 298,                     /* AND  */
    PAREN = 299,                   /* PAREN  */
    VAR_ASSIGN = 300,              /* VAR_ASSIGN  */
    VAR = 301,                     /* VAR  */
    BUILTIN_ANY_TYPE = 302,        /* BUILTIN_ANY_TYPE  */
    BOOL = 303,                    /* BOOL  */
    UNKNOWN_TYPE = 304             /* UNKNOWN_TYPE  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 18 "Grammar.y"

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

#line 184 "Grammar.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_GRAMMAR_TAB_H_INCLUDED  */
