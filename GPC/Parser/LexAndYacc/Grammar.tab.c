/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 4 "Grammar.y"

    #define YYDEBUG 1
    #include <stdio.h>
    #include <stdlib.h>
    #include "../ErrVars.h"
    void yyerror(char *s); /* Forward declaration */
    #include "../ParseTree/tree.h"
    #include "../List/List.h"
    #include "Grammar.tab.h"

    /*extern FILE *yyin;*/
    extern int yylex();

#line 85 "Grammar.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "Grammar.tab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_PROGRAM = 3,                    /* PROGRAM  */
  YYSYMBOL_VARIABLE = 4,                   /* VARIABLE  */
  YYSYMBOL_PROCEDURE = 5,                  /* PROCEDURE  */
  YYSYMBOL_FUNCTION = 6,                   /* FUNCTION  */
  YYSYMBOL_BBEGIN = 7,                     /* BBEGIN  */
  YYSYMBOL_END = 8,                        /* END  */
  YYSYMBOL_INT_NUM = 9,                    /* INT_NUM  */
  YYSYMBOL_REAL_NUM = 10,                  /* REAL_NUM  */
  YYSYMBOL_INT_TYPE = 11,                  /* INT_TYPE  */
  YYSYMBOL_REAL_TYPE = 12,                 /* REAL_TYPE  */
  YYSYMBOL_STRING = 13,                    /* STRING  */
  YYSYMBOL_ID = 14,                        /* ID  */
  YYSYMBOL_ARRAY = 15,                     /* ARRAY  */
  YYSYMBOL_SINGLE = 16,                    /* SINGLE  */
  YYSYMBOL_OF = 17,                        /* OF  */
  YYSYMBOL_DOTDOT = 18,                    /* DOTDOT  */
  YYSYMBOL_IF = 19,                        /* IF  */
  YYSYMBOL_THEN = 20,                      /* THEN  */
  YYSYMBOL_ELSE = 21,                      /* ELSE  */
  YYSYMBOL_WHILE = 22,                     /* WHILE  */
  YYSYMBOL_DO = 23,                        /* DO  */
  YYSYMBOL_NOT = 24,                       /* NOT  */
  YYSYMBOL_FOR = 25,                       /* FOR  */
  YYSYMBOL_TO = 26,                        /* TO  */
  YYSYMBOL_END_OF_FILE = 27,               /* END_OF_FILE  */
  YYSYMBOL_ASSIGNOP = 28,                  /* ASSIGNOP  */
  YYSYMBOL_RELOP = 29,                     /* RELOP  */
  YYSYMBOL_EQ = 30,                        /* EQ  */
  YYSYMBOL_NE = 31,                        /* NE  */
  YYSYMBOL_LT = 32,                        /* LT  */
  YYSYMBOL_LE = 33,                        /* LE  */
  YYSYMBOL_GT = 34,                        /* GT  */
  YYSYMBOL_GE = 35,                        /* GE  */
  YYSYMBOL_ADDOP = 36,                     /* ADDOP  */
  YYSYMBOL_PLUS = 37,                      /* PLUS  */
  YYSYMBOL_MINUS = 38,                     /* MINUS  */
  YYSYMBOL_OR = 39,                        /* OR  */
  YYSYMBOL_MULOP = 40,                     /* MULOP  */
  YYSYMBOL_STAR = 41,                      /* STAR  */
  YYSYMBOL_SLASH = 42,                     /* SLASH  */
  YYSYMBOL_AND = 43,                       /* AND  */
  YYSYMBOL_PAREN = 44,                     /* PAREN  */
  YYSYMBOL_VAR_ASSIGN = 45,                /* VAR_ASSIGN  */
  YYSYMBOL_VAR = 46,                       /* VAR  */
  YYSYMBOL_BUILTIN_ANY_TYPE = 47,          /* BUILTIN_ANY_TYPE  */
  YYSYMBOL_BOOL = 48,                      /* BOOL  */
  YYSYMBOL_UNKNOWN_TYPE = 49,              /* UNKNOWN_TYPE  */
  YYSYMBOL_50_ = 50,                       /* '('  */
  YYSYMBOL_51_ = 51,                       /* ')'  */
  YYSYMBOL_52_ = 52,                       /* ';'  */
  YYSYMBOL_53_ = 53,                       /* '.'  */
  YYSYMBOL_54_ = 54,                       /* ','  */
  YYSYMBOL_55_ = 55,                       /* ':'  */
  YYSYMBOL_56_ = 56,                       /* '['  */
  YYSYMBOL_57_ = 57,                       /* ']'  */
  YYSYMBOL_YYACCEPT = 58,                  /* $accept  */
  YYSYMBOL_program = 59,                   /* program  */
  YYSYMBOL_ident = 60,                     /* ident  */
  YYSYMBOL_int_num = 61,                   /* int_num  */
  YYSYMBOL_real_num = 62,                  /* real_num  */
  YYSYMBOL_string_literal = 63,            /* string_literal  */
  YYSYMBOL_relop = 64,                     /* relop  */
  YYSYMBOL_addop = 65,                     /* addop  */
  YYSYMBOL_mulop = 66,                     /* mulop  */
  YYSYMBOL_identifier_list = 67,           /* identifier_list  */
  YYSYMBOL_declarations = 68,              /* declarations  */
  YYSYMBOL_type = 69,                      /* type  */
  YYSYMBOL_standard_type = 70,             /* standard_type  */
  YYSYMBOL_subprogram_declarations = 71,   /* subprogram_declarations  */
  YYSYMBOL_subprogram_declaration = 72,    /* subprogram_declaration  */
  YYSYMBOL_subprogram_head = 73,           /* subprogram_head  */
  YYSYMBOL_arguments = 74,                 /* arguments  */
  YYSYMBOL_parameter_list = 75,            /* parameter_list  */
  YYSYMBOL_compound_statement = 76,        /* compound_statement  */
  YYSYMBOL_optional_statements = 77,       /* optional_statements  */
  YYSYMBOL_statement_list = 78,            /* statement_list  */
  YYSYMBOL_statement = 79,                 /* statement  */
  YYSYMBOL_if_statement = 80,              /* if_statement  */
  YYSYMBOL_variable_assignment = 81,       /* variable_assignment  */
  YYSYMBOL_for_assign = 82,                /* for_assign  */
  YYSYMBOL_variable = 83,                  /* variable  */
  YYSYMBOL_procedure_statement = 84,       /* procedure_statement  */
  YYSYMBOL_relop_expression = 85,          /* relop_expression  */
  YYSYMBOL_relop_and = 86,                 /* relop_and  */
  YYSYMBOL_relop_not = 87,                 /* relop_not  */
  YYSYMBOL_relop_paren = 88,               /* relop_paren  */
  YYSYMBOL_relop_expression_single = 89,   /* relop_expression_single  */
  YYSYMBOL_expression_list = 90,           /* expression_list  */
  YYSYMBOL_expression = 91,                /* expression  */
  YYSYMBOL_term = 92,                      /* term  */
  YYSYMBOL_factor = 93,                    /* factor  */
  YYSYMBOL_sign = 94                       /* sign  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  5
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   146

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  58
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  37
/* YYNRULES -- Number of rules.  */
#define YYNRULES  72
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  148

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   304


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      50,    51,     2,     2,    54,     2,    53,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    55,    52,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    56,     2,    57,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   197,   197,   210,   218,   222,   226,   230,   234,   238,
     241,   246,   254,   267,   271,   276,   287,   288,   292,   299,
     303,   316,   325,   337,   338,   342,   352,   365,   372,   373,
     377,   381,   385,   392,   396,   400,   404,   408,   412,   423,
     427,   433,   439,   444,   451,   455,   462,   466,   475,   479,
     483,   487,   491,   495,   499,   500,   504,   513,   517,   524,
     525,   532,   533,   540,   546,   550,   554,   558,   562,   566,
     570,   577,   584
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "PROGRAM", "VARIABLE",
  "PROCEDURE", "FUNCTION", "BBEGIN", "END", "INT_NUM", "REAL_NUM",
  "INT_TYPE", "REAL_TYPE", "STRING", "ID", "ARRAY", "SINGLE", "OF",
  "DOTDOT", "IF", "THEN", "ELSE", "WHILE", "DO", "NOT", "FOR", "TO",
  "END_OF_FILE", "ASSIGNOP", "RELOP", "EQ", "NE", "LT", "LE", "GT", "GE",
  "ADDOP", "PLUS", "MINUS", "OR", "MULOP", "STAR", "SLASH", "AND", "PAREN",
  "VAR_ASSIGN", "VAR", "BUILTIN_ANY_TYPE", "BOOL", "UNKNOWN_TYPE", "'('",
  "')'", "';'", "'.'", "','", "':'", "'['", "']'", "$accept", "program",
  "ident", "int_num", "real_num", "string_literal", "relop", "addop",
  "mulop", "identifier_list", "declarations", "type", "standard_type",
  "subprogram_declarations", "subprogram_declaration", "subprogram_head",
  "arguments", "parameter_list", "compound_statement",
  "optional_statements", "statement_list", "statement", "if_statement",
  "variable_assignment", "for_assign", "variable", "procedure_statement",
  "relop_expression", "relop_and", "relop_not", "relop_paren",
  "relop_expression_single", "expression_list", "expression", "term",
  "factor", "sign", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-102)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-45)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int8 yypact[] =
{
      17,    60,    62,  -102,    58,  -102,    60,  -102,    41,    29,
      60,  -102,  -102,    89,    60,    98,    52,    60,    60,    54,
      59,  -102,    45,    76,    67,    67,    22,    22,    60,    -9,
    -102,   110,    68,  -102,  -102,  -102,    91,  -102,  -102,    89,
      94,  -102,  -102,    66,    71,  -102,    60,    72,    70,  -102,
    -102,  -102,    22,  -102,    22,    33,  -102,  -102,  -102,     4,
      83,  -102,  -102,  -102,    42,    87,  -102,    46,   -11,    73,
    -102,   104,    91,    46,    46,  -102,    54,    46,    98,  -102,
     119,  -102,    55,    34,  -102,   102,  -102,   -25,    13,    46,
      46,    54,    22,    22,  -102,  -102,    46,    46,  -102,    46,
      46,  -102,    54,    46,    43,    95,    -7,  -102,    95,  -102,
     114,    76,  -102,    60,    81,  -102,  -102,    48,    27,   113,
      83,  -102,    95,    87,  -102,   -21,  -102,    31,  -102,    46,
    -102,   119,  -102,    61,  -102,  -102,  -102,    54,    54,    95,
      78,    76,  -102,  -102,   121,  -102,   102,  -102
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,     0,     3,     0,     1,     0,    10,     0,     0,
       0,    13,    11,    19,     0,     0,     0,     0,     0,    29,
       0,    13,     0,     0,    24,    24,     0,     0,     0,    46,
      35,     0,    28,    30,    36,    33,     0,    34,    18,    19,
       0,    16,    17,     0,     0,    14,     0,     0,     0,     4,
       5,     6,     0,    72,     0,    64,    67,    68,    63,     0,
      49,    51,    53,    55,     0,    59,    61,     0,     0,    44,
      42,     0,    43,     0,     0,    27,    32,     0,     0,     2,
       0,    12,     0,     0,    22,     0,    52,     0,     0,     0,
       0,     0,     0,     0,     7,     8,     0,     0,     9,     0,
       0,    70,     0,     0,     0,    57,     0,    31,    41,    20,
       0,     0,    23,     0,     0,    54,    71,     0,     0,    39,
      48,    50,    56,    60,    62,     0,    37,     0,    47,     0,
      45,     0,    25,     0,    21,    66,    65,     0,     0,    58,
       0,     0,    40,    38,     0,    26,     0,    15
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
    -102,  -102,    -1,   -74,  -102,  -102,  -102,  -102,  -102,   -13,
     118,  -101,   -81,   101,  -102,  -102,   116,  -102,   -12,  -102,
    -102,   -68,  -102,   115,  -102,   117,  -102,    26,    50,   -41,
    -102,  -102,    57,   -52,    47,   -60,  -102
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
       0,     2,    55,    56,    57,    58,    96,    97,    99,     8,
      13,    44,    45,    15,    20,    21,    47,    83,    30,    31,
      32,    33,    34,    35,    71,    36,    37,    59,    60,    61,
      62,    63,   104,    64,    65,    66,    67
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
       4,    16,    88,    22,   114,     7,   110,   101,   107,    12,
     132,    86,   102,     7,    92,    95,    24,    25,    29,   -44,
       1,   105,   106,   119,    91,   108,   115,    69,    92,    95,
     116,    49,    50,    82,   126,    51,     3,   105,   118,   124,
     145,    73,    94,    92,   122,     7,    52,    74,   125,    95,
     130,   127,   121,    68,   138,    49,    50,   140,    53,    51,
       3,    19,     5,    95,   116,   147,   109,    95,     3,   142,
     143,    94,    54,    26,     3,    29,    27,   139,    95,    28,
      87,    11,    53,    89,   136,   112,   113,    41,    42,    90,
      29,    43,     9,    14,   128,    10,   100,   129,    40,   135,
     133,    29,   129,    17,    18,    19,    10,    23,     6,    10,
     111,    38,     7,    41,    42,    10,   141,    46,    75,    77,
      76,    79,    80,    81,    84,    85,    93,    98,    49,    74,
     103,    95,   131,   134,   137,   144,    29,    29,   146,    39,
      78,    48,   120,    70,   123,    72,   117
};

static const yytype_uint8 yycheck[] =
{
       1,    14,    54,    15,    85,     6,    80,    67,    76,    10,
     111,    52,    23,    14,    39,    36,    17,    18,    19,    28,
       3,    73,    74,    91,    20,    77,    51,    28,    39,    36,
      51,     9,    10,    46,   102,    13,    14,    89,    90,    99,
     141,    50,    29,    39,    96,    46,    24,    56,   100,    36,
      57,   103,    93,    27,    23,     9,    10,   131,    36,    13,
      14,     7,     0,    36,    51,   146,    78,    36,    14,   137,
     138,    29,    50,    19,    14,    76,    22,   129,    36,    25,
      54,    52,    36,    50,    57,    51,    52,    11,    12,    56,
      91,    15,    51,     4,    51,    54,    50,    54,    53,    51,
     113,   102,    54,     5,     6,     7,    54,    55,    50,    54,
      55,    52,   113,    11,    12,    54,    55,    50,     8,    28,
      52,    27,    56,    52,    52,    55,    43,    40,     9,    56,
      26,    36,    18,    52,    21,    57,   137,   138,    17,    21,
      39,    25,    92,    28,    97,    28,    89
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     3,    59,    14,    60,     0,    50,    60,    67,    51,
      54,    52,    60,    68,     4,    71,    67,     5,     6,     7,
      72,    73,    76,    55,    60,    60,    19,    22,    25,    60,
      76,    77,    78,    79,    80,    81,    83,    84,    52,    68,
      53,    11,    12,    15,    69,    70,    50,    74,    74,     9,
      10,    13,    24,    36,    50,    60,    61,    62,    63,    85,
      86,    87,    88,    89,    91,    92,    93,    94,    85,    60,
      81,    82,    83,    50,    56,     8,    52,    28,    71,    27,
      56,    52,    67,    75,    52,    55,    87,    85,    91,    50,
      56,    20,    39,    43,    29,    36,    64,    65,    40,    66,
      50,    93,    23,    26,    90,    91,    91,    79,    91,    76,
      61,    55,    51,    52,    70,    51,    51,    90,    91,    79,
      86,    87,    91,    92,    93,    91,    79,    91,    51,    54,
      57,    18,    69,    67,    52,    51,    57,    21,    23,    91,
      61,    55,    79,    79,    57,    69,    17,    70
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    67,    68,    68,    69,    69,    70,    70,    71,    71,
      72,    73,    73,    74,    74,    75,    75,    76,    77,    77,
      78,    78,    78,    79,    79,    79,    79,    79,    79,    80,
      80,    81,    82,    82,    83,    83,    84,    84,    85,    85,
      86,    86,    87,    87,    88,    88,    89,    90,    90,    91,
      91,    92,    92,    93,    93,    93,    93,    93,    93,    93,
      93,    93,    94
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,    11,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     6,     0,     1,     8,     1,     1,     3,     0,
       4,     6,     4,     3,     0,     3,     5,     3,     1,     0,
       1,     3,     2,     1,     1,     1,     1,     4,     6,     4,
       6,     3,     1,     1,     1,     4,     1,     4,     3,     1,
       3,     1,     2,     1,     3,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     1,     4,     4,     1,     1,     1,
       2,     3,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* program: PROGRAM ident '(' identifier_list ')' ';' declarations subprogram_declarations compound_statement '.' END_OF_FILE  */
#line 203 "Grammar.y"
     {
         parse_tree = mk_program((yyvsp[-9].ident).line_num, (yyvsp[-9].ident).id, (yyvsp[-7].ident_list).list, (yyvsp[-4].list), (yyvsp[-3].list), (yyvsp[-2].stmt));
         return -1;
     }
#line 1272 "Grammar.tab.c"
    break;

  case 3: /* ident: ID  */
#line 211 "Grammar.y"
        {
            (yyval.ident).id = yylval.id;
            (yyval.ident).line_num = line_num;
        }
#line 1281 "Grammar.tab.c"
    break;

  case 4: /* int_num: INT_NUM  */
#line 218 "Grammar.y"
              {(yyval.i_val) = yylval.i_val;}
#line 1287 "Grammar.tab.c"
    break;

  case 5: /* real_num: REAL_NUM  */
#line 222 "Grammar.y"
               {(yyval.f_val) = yylval.f_val;}
#line 1293 "Grammar.tab.c"
    break;

  case 6: /* string_literal: STRING  */
#line 226 "Grammar.y"
             {(yyval.str) = yylval.str;}
#line 1299 "Grammar.tab.c"
    break;

  case 7: /* relop: RELOP  */
#line 230 "Grammar.y"
            { (yyval.op_val) = yylval.op_val;}
#line 1305 "Grammar.tab.c"
    break;

  case 8: /* addop: ADDOP  */
#line 234 "Grammar.y"
            {(yyval.op_val) = yylval.op_val;}
#line 1311 "Grammar.tab.c"
    break;

  case 9: /* mulop: MULOP  */
#line 238 "Grammar.y"
            {(yyval.op_val) = yylval.op_val;}
#line 1317 "Grammar.tab.c"
    break;

  case 10: /* identifier_list: ident  */
#line 242 "Grammar.y"
        {
            (yyval.ident_list).list = CreateListNode((yyvsp[0].ident).id, LIST_STRING);
            (yyval.ident_list).line_num = (yyvsp[0].ident).line_num; /* TODO: List of line nums */
        }
#line 1326 "Grammar.tab.c"
    break;

  case 11: /* identifier_list: identifier_list ',' ident  */
#line 247 "Grammar.y"
        {
            (yyval.ident_list).list = PushListNodeBack((yyvsp[-2].ident_list).list, CreateListNode((yyvsp[0].ident).id, LIST_STRING));
            (yyval.ident_list).line_num = (yyvsp[-2].ident_list).line_num;
        }
#line 1335 "Grammar.tab.c"
    break;

  case 12: /* declarations: declarations VARIABLE identifier_list ':' type ';'  */
#line 255 "Grammar.y"
        {
            Tree_t *tree;
            if((yyvsp[-1].type_s).type == ARRAY)
                tree = mk_arraydecl((yyvsp[-3].ident_list).line_num, (yyvsp[-3].ident_list).list, (yyvsp[-1].type_s).actual_type, (yyvsp[-1].type_s).start, (yyvsp[-1].type_s).end);
            else
                tree = mk_vardecl((yyvsp[-3].ident_list).line_num, (yyvsp[-3].ident_list).list, (yyvsp[-1].type_s).actual_type);

            if((yyvsp[-5].list) == NULL)
                (yyval.list) = CreateListNode(tree, LIST_TREE);
            else
                (yyval.list) = PushListNodeBack((yyvsp[-5].list), CreateListNode(tree, LIST_TREE));
        }
#line 1352 "Grammar.tab.c"
    break;

  case 13: /* declarations: %empty  */
#line 267 "Grammar.y"
                  {(yyval.list) = NULL;}
#line 1358 "Grammar.tab.c"
    break;

  case 14: /* type: standard_type  */
#line 272 "Grammar.y"
        {
            (yyval.type_s).type = SINGLE;
            (yyval.type_s).actual_type = (yyvsp[0].i_val);
        }
#line 1367 "Grammar.tab.c"
    break;

  case 15: /* type: ARRAY '[' int_num DOTDOT int_num ']' OF standard_type  */
#line 277 "Grammar.y"
        {
            (yyval.type_s).type = ARRAY;
            (yyval.type_s).actual_type = (yyvsp[0].i_val);
            (yyval.type_s).start = (yyvsp[-5].i_val);
            (yyval.type_s).end = (yyvsp[-3].i_val);
        }
#line 1378 "Grammar.tab.c"
    break;

  case 16: /* standard_type: INT_TYPE  */
#line 287 "Grammar.y"
               {(yyval.i_val) = INT_TYPE;}
#line 1384 "Grammar.tab.c"
    break;

  case 17: /* standard_type: REAL_TYPE  */
#line 288 "Grammar.y"
                {(yyval.i_val) = REAL_TYPE;}
#line 1390 "Grammar.tab.c"
    break;

  case 18: /* subprogram_declarations: subprogram_declarations subprogram_declaration ';'  */
#line 293 "Grammar.y"
        {
            if((yyvsp[-2].list) == NULL)
                (yyval.list) = CreateListNode((yyvsp[-1].tree), LIST_TREE);
            else
                (yyval.list) = PushListNodeBack((yyvsp[-2].list), CreateListNode((yyvsp[-1].tree), LIST_TREE));
        }
#line 1401 "Grammar.tab.c"
    break;

  case 19: /* subprogram_declarations: %empty  */
#line 299 "Grammar.y"
                  {(yyval.list) = NULL;}
#line 1407 "Grammar.tab.c"
    break;

  case 20: /* subprogram_declaration: subprogram_head declarations subprogram_declarations compound_statement  */
#line 307 "Grammar.y"
        {
            if((yyvsp[-3].subprogram_head_s).sub_type == PROCEDURE)
                (yyval.tree) = mk_procedure((yyvsp[-3].subprogram_head_s).line_num, (yyvsp[-3].subprogram_head_s).id, (yyvsp[-3].subprogram_head_s).args, (yyvsp[-2].list), (yyvsp[-1].list), (yyvsp[0].stmt));
            else
                (yyval.tree) = mk_function((yyvsp[-3].subprogram_head_s).line_num, (yyvsp[-3].subprogram_head_s).id, (yyvsp[-3].subprogram_head_s).args, (yyvsp[-2].list), (yyvsp[-1].list), (yyvsp[0].stmt), (yyvsp[-3].subprogram_head_s).return_type);
        }
#line 1418 "Grammar.tab.c"
    break;

  case 21: /* subprogram_head: FUNCTION ident arguments ':' standard_type ';'  */
#line 317 "Grammar.y"
        {
            (yyval.subprogram_head_s).sub_type = FUNCTION;
            (yyval.subprogram_head_s).args = (yyvsp[-3].list);
            (yyval.subprogram_head_s).return_type = (yyvsp[-1].i_val);

            (yyval.subprogram_head_s).id = (yyvsp[-4].ident).id;
            (yyval.subprogram_head_s).line_num = (yyvsp[-4].ident).line_num;
        }
#line 1431 "Grammar.tab.c"
    break;

  case 22: /* subprogram_head: PROCEDURE ident arguments ';'  */
#line 326 "Grammar.y"
        {
            (yyval.subprogram_head_s).sub_type = PROCEDURE;
            (yyval.subprogram_head_s).args = (yyvsp[-1].list);
            (yyval.subprogram_head_s).return_type = -1;

            (yyval.subprogram_head_s).id = (yyvsp[-2].ident).id;
            (yyval.subprogram_head_s).line_num = (yyvsp[-2].ident).line_num;
        }
#line 1444 "Grammar.tab.c"
    break;

  case 23: /* arguments: '(' parameter_list ')'  */
#line 337 "Grammar.y"
                             {(yyval.list) = (yyvsp[-1].list);}
#line 1450 "Grammar.tab.c"
    break;

  case 24: /* arguments: %empty  */
#line 338 "Grammar.y"
                  {(yyval.list) = NULL;}
#line 1456 "Grammar.tab.c"
    break;

  case 25: /* parameter_list: identifier_list ':' type  */
#line 343 "Grammar.y"
        {
            Tree_t *tree;
            if((yyvsp[0].type_s).type == ARRAY)
                tree = mk_arraydecl((yyvsp[-2].ident_list).line_num, (yyvsp[-2].ident_list).list, (yyvsp[0].type_s).actual_type, (yyvsp[0].type_s).start, (yyvsp[0].type_s).end);
            else
                tree = mk_vardecl((yyvsp[-2].ident_list).line_num, (yyvsp[-2].ident_list).list, (yyvsp[0].type_s).actual_type);

            (yyval.list) = CreateListNode(tree, LIST_TREE);
        }
#line 1470 "Grammar.tab.c"
    break;

  case 26: /* parameter_list: parameter_list ';' identifier_list ':' type  */
#line 353 "Grammar.y"
        {
            Tree_t *tree;
            if((yyvsp[0].type_s).type == ARRAY)
                tree = mk_arraydecl((yyvsp[-2].ident_list).line_num, (yyvsp[-2].ident_list).list, (yyvsp[0].type_s).actual_type, (yyvsp[0].type_s).start, (yyvsp[0].type_s).end);
            else
                tree = mk_vardecl((yyvsp[-2].ident_list).line_num, (yyvsp[-2].ident_list).list, (yyvsp[0].type_s).actual_type);

            (yyval.list) = PushListNodeBack((yyvsp[-4].list), CreateListNode(tree, LIST_TREE));
        }
#line 1484 "Grammar.tab.c"
    break;

  case 27: /* compound_statement: BBEGIN optional_statements END  */
#line 366 "Grammar.y"
        {
            (yyval.stmt) = mk_compoundstatement(line_num, (yyvsp[-1].list));
        }
#line 1492 "Grammar.tab.c"
    break;

  case 28: /* optional_statements: statement_list  */
#line 372 "Grammar.y"
                     {(yyval.list) = (yyvsp[0].list);}
#line 1498 "Grammar.tab.c"
    break;

  case 29: /* optional_statements: %empty  */
#line 373 "Grammar.y"
                  {(yyval.list) = NULL;}
#line 1504 "Grammar.tab.c"
    break;

  case 30: /* statement_list: statement  */
#line 378 "Grammar.y"
        {
            (yyval.list) = CreateListNode((yyvsp[0].stmt), LIST_STMT);
        }
#line 1512 "Grammar.tab.c"
    break;

  case 31: /* statement_list: statement_list ';' statement  */
#line 382 "Grammar.y"
        {
            (yyval.list) = PushListNodeBack((yyvsp[-2].list), CreateListNode((yyvsp[0].stmt), LIST_STMT));
        }
#line 1520 "Grammar.tab.c"
    break;

  case 32: /* statement_list: statement_list ';'  */
#line 386 "Grammar.y"
        {
            (yyval.list) = (yyvsp[-1].list);
        }
#line 1528 "Grammar.tab.c"
    break;

  case 33: /* statement: variable_assignment  */
#line 393 "Grammar.y"
        {
            (yyval.stmt) = (yyvsp[0].stmt);
        }
#line 1536 "Grammar.tab.c"
    break;

  case 34: /* statement: procedure_statement  */
#line 397 "Grammar.y"
        {
            (yyval.stmt) = (yyvsp[0].stmt);
        }
#line 1544 "Grammar.tab.c"
    break;

  case 35: /* statement: compound_statement  */
#line 401 "Grammar.y"
        {
            (yyval.stmt) = (yyvsp[0].stmt);
        }
#line 1552 "Grammar.tab.c"
    break;

  case 36: /* statement: if_statement  */
#line 405 "Grammar.y"
        {
            (yyval.stmt) = (yyvsp[0].stmt);
        }
#line 1560 "Grammar.tab.c"
    break;

  case 37: /* statement: WHILE relop_expression DO statement  */
#line 409 "Grammar.y"
        {
            (yyval.stmt) = mk_while(line_num, (yyvsp[-2].expr), (yyvsp[0].stmt));
        }
#line 1568 "Grammar.tab.c"
    break;

  case 38: /* statement: FOR for_assign TO expression DO statement  */
#line 413 "Grammar.y"
        {
            if((yyvsp[-4].for_assign_bison).assign_type == VAR_ASSIGN)
                (yyval.stmt) = mk_forassign(line_num, (yyvsp[-4].for_assign_bison).for_assign_bison_union.stmt, (yyvsp[-2].expr), (yyvsp[0].stmt));
            else
                (yyval.stmt) = mk_forvar(line_num, (yyvsp[-4].for_assign_bison).for_assign_bison_union.expr, (yyvsp[-2].expr), (yyvsp[0].stmt));
        }
#line 1579 "Grammar.tab.c"
    break;

  case 39: /* if_statement: IF relop_expression THEN statement  */
#line 424 "Grammar.y"
        {
            (yyval.stmt) = mk_ifthen(line_num, (yyvsp[-2].expr), (yyvsp[0].stmt), NULL);
        }
#line 1587 "Grammar.tab.c"
    break;

  case 40: /* if_statement: IF relop_expression THEN statement ELSE statement  */
#line 428 "Grammar.y"
        {
            (yyval.stmt) = mk_ifthen(line_num, (yyvsp[-4].expr), (yyvsp[-2].stmt), (yyvsp[0].stmt));
        }
#line 1595 "Grammar.tab.c"
    break;

  case 41: /* variable_assignment: variable ASSIGNOP expression  */
#line 434 "Grammar.y"
        {
            (yyval.stmt) = mk_varassign(line_num, (yyvsp[-2].expr), (yyvsp[0].expr));
        }
#line 1603 "Grammar.tab.c"
    break;

  case 42: /* for_assign: variable_assignment  */
#line 440 "Grammar.y"
        {
            (yyval.for_assign_bison).assign_type = VAR_ASSIGN;
            (yyval.for_assign_bison).for_assign_bison_union.stmt = (yyvsp[0].stmt);
        }
#line 1612 "Grammar.tab.c"
    break;

  case 43: /* for_assign: variable  */
#line 445 "Grammar.y"
        {
            (yyval.for_assign_bison).assign_type = VAR;
            (yyval.for_assign_bison).for_assign_bison_union.expr = (yyvsp[0].expr);
        }
#line 1621 "Grammar.tab.c"
    break;

  case 44: /* variable: ident  */
#line 452 "Grammar.y"
        {
            (yyval.expr) = mk_varid((yyvsp[0].ident).line_num, (yyvsp[0].ident).id);
        }
#line 1629 "Grammar.tab.c"
    break;

  case 45: /* variable: ident '[' expression ']'  */
#line 456 "Grammar.y"
        {
            (yyval.expr) = mk_arrayaccess((yyvsp[-3].ident).line_num, (yyvsp[-3].ident).id, (yyvsp[-1].expr));
        }
#line 1637 "Grammar.tab.c"
    break;

  case 46: /* procedure_statement: ident  */
#line 463 "Grammar.y"
        {
            (yyval.stmt) = mk_procedurecall((yyvsp[0].ident).line_num, (yyvsp[0].ident).id, NULL);
        }
#line 1645 "Grammar.tab.c"
    break;

  case 47: /* procedure_statement: ident '(' expression_list ')'  */
#line 467 "Grammar.y"
        {
            (yyval.stmt) = mk_procedurecall((yyvsp[-3].ident).line_num, (yyvsp[-3].ident).id, (yyvsp[-1].list));
        }
#line 1653 "Grammar.tab.c"
    break;

  case 48: /* relop_expression: relop_expression OR relop_and  */
#line 476 "Grammar.y"
        {
            (yyval.expr) = mk_relop(line_num, OR, (yyvsp[-2].expr), (yyvsp[0].expr));
        }
#line 1661 "Grammar.tab.c"
    break;

  case 49: /* relop_expression: relop_and  */
#line 479 "Grammar.y"
                {(yyval.expr) = (yyvsp[0].expr);}
#line 1667 "Grammar.tab.c"
    break;

  case 50: /* relop_and: relop_and AND relop_not  */
#line 484 "Grammar.y"
        {
            (yyval.expr) = mk_relop(line_num, AND, (yyvsp[-2].expr), (yyvsp[0].expr));
        }
#line 1675 "Grammar.tab.c"
    break;

  case 51: /* relop_and: relop_not  */
#line 487 "Grammar.y"
                {(yyval.expr) = (yyvsp[0].expr);}
#line 1681 "Grammar.tab.c"
    break;

  case 52: /* relop_not: NOT relop_not  */
#line 492 "Grammar.y"
        {
            (yyval.expr) = mk_relop(line_num, NOT, (yyvsp[0].expr), NULL);
        }
#line 1689 "Grammar.tab.c"
    break;

  case 53: /* relop_not: relop_paren  */
#line 495 "Grammar.y"
                  {(yyval.expr) = (yyvsp[0].expr);}
#line 1695 "Grammar.tab.c"
    break;

  case 54: /* relop_paren: '(' relop_expression ')'  */
#line 499 "Grammar.y"
                               {(yyval.expr) = (yyvsp[-1].expr);}
#line 1701 "Grammar.tab.c"
    break;

  case 55: /* relop_paren: relop_expression_single  */
#line 500 "Grammar.y"
                              {(yyval.expr) = (yyvsp[0].expr);}
#line 1707 "Grammar.tab.c"
    break;

  case 56: /* relop_expression_single: expression relop expression  */
#line 505 "Grammar.y"
        {
            (yyval.expr) = mk_relop(line_num, (yyvsp[-1].op_val), (yyvsp[-2].expr), (yyvsp[0].expr));
        }
#line 1715 "Grammar.tab.c"
    break;

  case 57: /* expression_list: expression  */
#line 514 "Grammar.y"
        {
            (yyval.list) = CreateListNode((yyvsp[0].expr), LIST_EXPR);
        }
#line 1723 "Grammar.tab.c"
    break;

  case 58: /* expression_list: expression_list ',' expression  */
#line 518 "Grammar.y"
        {
            (yyval.list) = PushListNodeBack((yyvsp[-2].list), CreateListNode((yyvsp[0].expr), LIST_EXPR));
        }
#line 1731 "Grammar.tab.c"
    break;

  case 59: /* expression: term  */
#line 524 "Grammar.y"
           {(yyval.expr) = (yyvsp[0].expr);}
#line 1737 "Grammar.tab.c"
    break;

  case 60: /* expression: expression addop term  */
#line 526 "Grammar.y"
        {
            (yyval.expr) = mk_addop(line_num, (yyvsp[-1].op_val), (yyvsp[-2].expr), (yyvsp[0].expr));
        }
#line 1745 "Grammar.tab.c"
    break;

  case 61: /* term: factor  */
#line 532 "Grammar.y"
             {(yyval.expr) = (yyvsp[0].expr);}
#line 1751 "Grammar.tab.c"
    break;

  case 62: /* term: term mulop factor  */
#line 534 "Grammar.y"
        {
            (yyval.expr) = mk_mulop(line_num, (yyvsp[-1].op_val), (yyvsp[-2].expr), (yyvsp[0].expr));
        }
#line 1759 "Grammar.tab.c"
    break;

  case 63: /* factor: string_literal  */
#line 541 "Grammar.y"
        {
            printf("DEBUG: Creating string literal expression at line %d\n", line_num);
            (yyval.expr) = mk_string(line_num, (yyvsp[0].str));
            printf("DEBUG: Created string literal: %s\n", (yyvsp[0].str));
        }
#line 1769 "Grammar.tab.c"
    break;

  case 64: /* factor: ident  */
#line 547 "Grammar.y"
        {
            (yyval.expr) = mk_varid((yyvsp[0].ident).line_num, (yyvsp[0].ident).id);
        }
#line 1777 "Grammar.tab.c"
    break;

  case 65: /* factor: ident '[' expression ']'  */
#line 551 "Grammar.y"
        {
            (yyval.expr) = mk_arrayaccess((yyvsp[-3].ident).line_num, (yyvsp[-3].ident).id, (yyvsp[-1].expr));
        }
#line 1785 "Grammar.tab.c"
    break;

  case 66: /* factor: ident '(' expression_list ')'  */
#line 555 "Grammar.y"
        {
            (yyval.expr) = mk_functioncall((yyvsp[-3].ident).line_num, (yyvsp[-3].ident).id, (yyvsp[-1].list));
        }
#line 1793 "Grammar.tab.c"
    break;

  case 67: /* factor: int_num  */
#line 559 "Grammar.y"
        {
            (yyval.expr) = mk_inum(line_num, (yyvsp[0].i_val));
        }
#line 1801 "Grammar.tab.c"
    break;

  case 68: /* factor: real_num  */
#line 563 "Grammar.y"
        {
            (yyval.expr) = mk_rnum(line_num, (yyvsp[0].f_val));
        }
#line 1809 "Grammar.tab.c"
    break;

  case 69: /* factor: string_literal  */
#line 567 "Grammar.y"
        {
            (yyval.expr) = mk_string(line_num, (yyvsp[0].str));
        }
#line 1817 "Grammar.tab.c"
    break;

  case 70: /* factor: sign factor  */
#line 571 "Grammar.y"
        {
            if((yyvsp[-1].op_val) == MINUS)
                (yyval.expr) = mk_signterm(line_num, (yyvsp[0].expr));
            else
                (yyval.expr) = (yyvsp[0].expr);
        }
#line 1828 "Grammar.tab.c"
    break;

  case 71: /* factor: '(' expression ')'  */
#line 578 "Grammar.y"
        {
            (yyval.expr) = (yyvsp[-1].expr);
        }
#line 1836 "Grammar.tab.c"
    break;

  case 72: /* sign: ADDOP  */
#line 585 "Grammar.y"
        {
            (yyval.op_val) = yylval.op_val;
        }
#line 1844 "Grammar.tab.c"
    break;


#line 1848 "Grammar.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 590 "Grammar.y"


void yyerror(char *s)
{
    FILE *fp = NULL;
    char line[256] = {0};
    extern int yychar; // Current lookahead token
    
    if(file_to_parse != NULL)
    {
        fprintf(stderr, "\nError in %s, line %d:\n", file_to_parse, line_num);
        fp = fopen(file_to_parse, "r");
    }
    else
    {
        fprintf(stderr, "\nError on line %d:\n", line_num);
    }

    // Print the problematic line if possible
    if(fp != NULL)
    {
        int current_line = 1;
        while(fgets(line, sizeof(line), fp) != NULL && current_line < line_num)
        {
            current_line++;
        }
        
        if(current_line == line_num)
        {
            fprintf(stderr, ">>> %s", line);
            
            // Add specific error hints
            if(yychar == END) {
                fprintf(stderr, "Possible missing semicolon before 'end'\n");
            }
        }
            
        fclose(fp);
    }

    fprintf(stderr, "Syntax error: %s\n", s);
    fprintf(stderr, "Current token: %d\n", yychar);
}

