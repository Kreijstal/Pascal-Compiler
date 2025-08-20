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

#line 93 "Grammar.tab.c"

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
  YYSYMBOL_OVERLOAD = 7,                   /* OVERLOAD  */
  YYSYMBOL_BBEGIN = 8,                     /* BBEGIN  */
  YYSYMBOL_END = 9,                        /* END  */
  YYSYMBOL_INT_NUM = 10,                   /* INT_NUM  */
  YYSYMBOL_REAL_NUM = 11,                  /* REAL_NUM  */
  YYSYMBOL_INT_TYPE = 12,                  /* INT_TYPE  */
  YYSYMBOL_REAL_TYPE = 13,                 /* REAL_TYPE  */
  YYSYMBOL_LONGINT_TYPE = 14,              /* LONGINT_TYPE  */
  YYSYMBOL_STRING_TYPE = 15,               /* STRING_TYPE  */
  YYSYMBOL_STRING = 16,                    /* STRING  */
  YYSYMBOL_ENDASM = 17,                    /* ENDASM  */
  YYSYMBOL_ID = 18,                        /* ID  */
  YYSYMBOL_TYPE = 19,                      /* TYPE  */
  YYSYMBOL_ARRAY = 20,                     /* ARRAY  */
  YYSYMBOL_ASSEMBLER = 21,                 /* ASSEMBLER  */
  YYSYMBOL_ASM = 22,                       /* ASM  */
  YYSYMBOL_CONST = 23,                     /* CONST  */
  YYSYMBOL_ASMMODE = 24,                   /* ASMMODE  */
  YYSYMBOL_SINGLE = 25,                    /* SINGLE  */
  YYSYMBOL_OF = 26,                        /* OF  */
  YYSYMBOL_DOTDOT = 27,                    /* DOTDOT  */
  YYSYMBOL_IF = 28,                        /* IF  */
  YYSYMBOL_THEN = 29,                      /* THEN  */
  YYSYMBOL_ELSE = 30,                      /* ELSE  */
  YYSYMBOL_WHILE = 31,                     /* WHILE  */
  YYSYMBOL_DO = 32,                        /* DO  */
  YYSYMBOL_NOT = 33,                       /* NOT  */
  YYSYMBOL_FOR = 34,                       /* FOR  */
  YYSYMBOL_TO = 35,                        /* TO  */
  YYSYMBOL_CNAME = 36,                     /* CNAME  */
  YYSYMBOL_END_OF_FILE = 37,               /* END_OF_FILE  */
  YYSYMBOL_ASSIGNOP = 38,                  /* ASSIGNOP  */
  YYSYMBOL_RELOP = 39,                     /* RELOP  */
  YYSYMBOL_EQ = 40,                        /* EQ  */
  YYSYMBOL_NE = 41,                        /* NE  */
  YYSYMBOL_LT = 42,                        /* LT  */
  YYSYMBOL_LE = 43,                        /* LE  */
  YYSYMBOL_GT = 44,                        /* GT  */
  YYSYMBOL_GE = 45,                        /* GE  */
  YYSYMBOL_ADDOP = 46,                     /* ADDOP  */
  YYSYMBOL_PLUS = 47,                      /* PLUS  */
  YYSYMBOL_MINUS = 48,                     /* MINUS  */
  YYSYMBOL_OR = 49,                        /* OR  */
  YYSYMBOL_MULOP = 50,                     /* MULOP  */
  YYSYMBOL_STAR = 51,                      /* STAR  */
  YYSYMBOL_SLASH = 52,                     /* SLASH  */
  YYSYMBOL_AND = 53,                       /* AND  */
  YYSYMBOL_MOD = 54,                       /* MOD  */
  YYSYMBOL_DIV = 55,                       /* DIV  */
  YYSYMBOL_PAREN = 56,                     /* PAREN  */
  YYSYMBOL_VAR_ASSIGN = 57,                /* VAR_ASSIGN  */
  YYSYMBOL_VAR = 58,                       /* VAR  */
  YYSYMBOL_BUILTIN_ANY_TYPE = 59,          /* BUILTIN_ANY_TYPE  */
  YYSYMBOL_BOOL = 60,                      /* BOOL  */
  YYSYMBOL_UNKNOWN_TYPE = 61,              /* UNKNOWN_TYPE  */
  YYSYMBOL_62_ = 62,                       /* '('  */
  YYSYMBOL_63_ = 63,                       /* ')'  */
  YYSYMBOL_64_ = 64,                       /* ';'  */
  YYSYMBOL_65_ = 65,                       /* '.'  */
  YYSYMBOL_66_ = 66,                       /* ','  */
  YYSYMBOL_67_ = 67,                       /* ':'  */
  YYSYMBOL_68_ = 68,                       /* '['  */
  YYSYMBOL_69_ = 69,                       /* ']'  */
  YYSYMBOL_YYACCEPT = 70,                  /* $accept  */
  YYSYMBOL_optional_program_parameters = 71, /* optional_program_parameters  */
  YYSYMBOL_program = 72,                   /* program  */
  YYSYMBOL_ident = 73,                     /* ident  */
  YYSYMBOL_int_num = 74,                   /* int_num  */
  YYSYMBOL_real_num = 75,                  /* real_num  */
  YYSYMBOL_string_literal = 76,            /* string_literal  */
  YYSYMBOL_relop = 77,                     /* relop  */
  YYSYMBOL_addop = 78,                     /* addop  */
  YYSYMBOL_mulop = 79,                     /* mulop  */
  YYSYMBOL_identifier_list = 80,           /* identifier_list  */
  YYSYMBOL_declarations = 81,              /* declarations  */
  YYSYMBOL_declaration_list = 82,          /* declaration_list  */
  YYSYMBOL_type_declarations_opt = 83,     /* type_declarations_opt  */
  YYSYMBOL_type_declaration_list = 84,     /* type_declaration_list  */
  YYSYMBOL_type_declaration = 85,          /* type_declaration  */
  YYSYMBOL_signed_int = 86,                /* signed_int  */
  YYSYMBOL_type = 87,                      /* type  */
  YYSYMBOL_standard_type = 88,             /* standard_type  */
  YYSYMBOL_subprogram_declarations = 89,   /* subprogram_declarations  */
  YYSYMBOL_subprogram_declaration = 90,    /* subprogram_declaration  */
  YYSYMBOL_subprogram_head = 91,           /* subprogram_head  */
  YYSYMBOL_arguments = 92,                 /* arguments  */
  YYSYMBOL_optional_const = 93,            /* optional_const  */
  YYSYMBOL_parameter_list = 94,            /* parameter_list  */
  YYSYMBOL_parameter_item = 95,            /* parameter_item  */
  YYSYMBOL_compound_statement = 96,        /* compound_statement  */
  YYSYMBOL_statement_seq_opt = 97,         /* statement_seq_opt  */
  YYSYMBOL_statement_seq = 98,             /* statement_seq  */
  YYSYMBOL_statement = 99,                 /* statement  */
  YYSYMBOL_if_statement = 100,             /* if_statement  */
  YYSYMBOL_variable_assignment = 101,      /* variable_assignment  */
  YYSYMBOL_for_assign = 102,               /* for_assign  */
  YYSYMBOL_variable = 103,                 /* variable  */
  YYSYMBOL_procedure_statement = 104,      /* procedure_statement  */
  YYSYMBOL_relop_expression = 105,         /* relop_expression  */
  YYSYMBOL_relop_and = 106,                /* relop_and  */
  YYSYMBOL_relop_not = 107,                /* relop_not  */
  YYSYMBOL_relop_paren = 108,              /* relop_paren  */
  YYSYMBOL_relop_expression_single = 109,  /* relop_expression_single  */
  YYSYMBOL_expression_list = 110,          /* expression_list  */
  YYSYMBOL_expression = 111,               /* expression  */
  YYSYMBOL_term = 112,                     /* term  */
  YYSYMBOL_factor = 113,                   /* factor  */
  YYSYMBOL_sign = 114                      /* sign  */
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

#if 1

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
#endif /* 1 */

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
#define YYLAST   201

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  70
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  45
/* YYNRULES -- Number of rules.  */
#define YYNRULES  96
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  193

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   316


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
      62,    63,     2,     2,    66,     2,    65,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    67,    64,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    68,     2,    69,     2,     2,     2,     2,     2,     2,
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
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   229,   229,   232,   236,   250,   258,   262,   266,   270,
     274,   278,   281,   286,   294,   296,   300,   313,   326,   327,
     331,   333,   338,   348,   349,   360,   366,   374,   383,   384,
     385,   386,   390,   397,   401,   411,   428,   445,   462,   479,
     491,   503,   515,   530,   531,   535,   536,   540,   542,   547,
     556,   568,   569,   574,   575,   577,   582,   584,   589,   593,
     597,   601,   605,   609,   620,   624,   630,   636,   641,   648,
     652,   659,   663,   672,   676,   680,   684,   688,   692,   696,
     697,   701,   710,   714,   721,   722,   729,   730,   737,   741,
     745,   749,   753,   757,   761,   768,   775
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "PROGRAM", "VARIABLE",
  "PROCEDURE", "FUNCTION", "OVERLOAD", "BBEGIN", "END", "INT_NUM",
  "REAL_NUM", "INT_TYPE", "REAL_TYPE", "LONGINT_TYPE", "STRING_TYPE",
  "STRING", "ENDASM", "ID", "TYPE", "ARRAY", "ASSEMBLER", "ASM", "CONST",
  "ASMMODE", "SINGLE", "OF", "DOTDOT", "IF", "THEN", "ELSE", "WHILE", "DO",
  "NOT", "FOR", "TO", "CNAME", "END_OF_FILE", "ASSIGNOP", "RELOP", "EQ",
  "NE", "LT", "LE", "GT", "GE", "ADDOP", "PLUS", "MINUS", "OR", "MULOP",
  "STAR", "SLASH", "AND", "MOD", "DIV", "PAREN", "VAR_ASSIGN", "VAR",
  "BUILTIN_ANY_TYPE", "BOOL", "UNKNOWN_TYPE", "'('", "')'", "';'", "'.'",
  "','", "':'", "'['", "']'", "$accept", "optional_program_parameters",
  "program", "ident", "int_num", "real_num", "string_literal", "relop",
  "addop", "mulop", "identifier_list", "declarations", "declaration_list",
  "type_declarations_opt", "type_declaration_list", "type_declaration",
  "signed_int", "type", "standard_type", "subprogram_declarations",
  "subprogram_declaration", "subprogram_head", "arguments",
  "optional_const", "parameter_list", "parameter_item",
  "compound_statement", "statement_seq_opt", "statement_seq", "statement",
  "if_statement", "variable_assignment", "for_assign", "variable",
  "procedure_statement", "relop_expression", "relop_and", "relop_not",
  "relop_paren", "relop_expression_single", "expression_list",
  "expression", "term", "factor", "sign", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-94)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-70)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      16,    19,    40,   -94,     5,   -94,    19,    26,   -94,    37,
      61,   -94,    19,    19,   107,   -94,    74,    19,   -94,    19,
     -94,   -94,    29,   -94,    82,    19,   109,   -94,   -94,   -94,
     111,    98,   122,    84,    19,    19,    76,    67,    92,   107,
      93,   -94,    29,   -94,   -94,   -94,   -94,    73,   -94,    95,
     -94,   122,    99,    99,    12,    12,    19,    25,   -94,   148,
      96,   -94,   -94,   -94,   124,   -94,   141,   -94,   -94,   127,
     101,   111,   -94,   102,    72,     2,   100,   -94,   -94,    12,
     -94,    12,     0,   -94,   -94,   -94,    30,   115,   -94,   -94,
     -94,    62,   120,   -94,    36,    57,   103,   -94,   137,   124,
      36,    36,   -94,    76,    36,   156,   109,   -94,   -94,   147,
     -94,   152,   -94,    19,    75,   -94,   112,    -3,   -94,   122,
     -94,    15,    42,    36,    36,    76,    12,    12,    36,    36,
     -94,    36,    36,   -94,    76,    36,    56,   133,   -26,   -94,
     133,   -94,   -94,   111,    19,    86,   -94,    72,   -94,   116,
     -94,     6,   -94,   -94,    63,   -25,   153,   115,   -94,   133,
     120,   -94,    53,   -94,    77,   -94,    36,   -94,   113,    88,
     122,   -94,   -94,   121,     7,   -94,   -94,   -94,    76,    76,
     133,   158,   122,   -94,   -94,   123,   -94,   -94,   -94,   132,
     -94,   -94,   -94
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,     0,     5,     3,     1,     0,     0,    12,     0,
      19,     2,     0,     0,    15,    13,     0,    18,    20,     0,
      33,     9,     0,    21,     0,    14,     0,     6,    10,    23,
       0,     0,     0,     0,     0,     0,    53,     0,     0,    15,
       0,    24,     0,    28,    29,    30,    31,     0,    27,     0,
      25,     0,    44,    44,     0,     0,     0,    71,    60,     0,
      54,    56,    61,    58,     0,    59,     0,    32,    33,     0,
       0,     0,    17,     0,    46,     0,     0,     7,     8,     0,
      96,     0,    88,    91,    92,    93,     0,    74,    76,    78,
      80,     0,    84,    86,     0,     0,    69,    67,     0,    68,
       0,     0,    51,    55,     0,     0,     0,     4,    22,     0,
      16,    46,    45,     0,     0,    48,     0,     0,    39,     0,
      77,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      11,     0,     0,    94,     0,     0,     0,    82,     0,    57,
      66,    52,    34,     0,     0,     0,    43,    46,    40,     0,
      41,     0,    79,    95,     0,     0,    64,    73,    75,    81,
      85,    87,     0,    62,     0,    72,     0,    70,     0,     0,
       0,    47,    42,     0,     0,    35,    90,    89,     0,     0,
      83,     0,     0,    49,    36,     0,    37,    65,    63,     0,
      50,    38,    26
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -94,   -94,   -94,    -1,   -15,   -94,   -94,   170,    50,   -94,
     -17,   149,   -94,   -94,   -94,   172,   150,   -50,     1,   125,
     -94,   -94,   138,    83,   -94,    48,   -23,   -94,   -94,   -93,
     -94,   140,   -94,   142,   -94,    -4,    71,   -62,   -94,   -94,
      78,   -75,    70,   -58,   -94
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     7,     2,    82,    83,    84,    85,   128,   129,   131,
       9,    20,    25,    14,    17,    18,    31,    49,    50,    26,
      38,    39,    75,   113,   114,   115,    58,    59,    60,    61,
      62,    63,    98,    64,    65,    86,    87,    88,    89,    90,
     136,    91,    92,    93,    94
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
       4,    73,    24,    40,   149,     8,   122,    29,    33,   116,
     139,    15,    16,   173,   185,    41,    16,   120,     8,     1,
      28,    28,    27,    77,     8,   137,   138,    29,    78,   140,
       3,    48,   156,    52,    53,    57,   133,     3,   117,    27,
       5,   163,   174,   167,   177,    79,    27,    77,   137,   155,
      48,    95,    78,   159,     3,    96,   109,   162,    80,   125,
     164,   150,   123,   -69,   126,   158,   118,     6,   124,   151,
     175,   186,    30,   161,    81,    28,   111,   121,   152,   126,
      13,    21,    80,   142,    36,   187,   188,   100,    28,   134,
      10,   180,    30,   101,     3,   112,   145,    37,   132,    28,
      11,    21,    57,    12,    54,   153,   126,    55,    28,   179,
      56,    19,     8,    21,    34,    35,   153,    36,    48,   165,
     183,    27,   166,    28,    57,    42,   176,   169,   168,   166,
      37,    66,   190,    57,    43,    44,    45,    46,   146,   147,
       3,    71,    47,     8,    43,    44,    45,    46,    12,    32,
      12,    51,    12,   170,    12,   182,    67,   102,    69,    72,
     103,    74,   104,   105,   107,   108,   110,   119,   127,    48,
     130,   101,   135,   141,   143,   112,   148,    57,    57,    28,
     172,    48,   181,   178,   189,   184,    22,   191,    68,    23,
     192,    76,    70,   106,   144,   171,    97,   157,    99,   160,
       0,   154
};

static const yytype_int16 yycheck[] =
{
       1,    51,    19,    26,     7,     6,    81,    22,    25,     7,
     103,    12,    13,     7,     7,    30,    17,    79,    19,     3,
      46,    46,    10,    11,    25,   100,   101,    42,    16,   104,
      18,    32,   125,    34,    35,    36,    94,    18,    36,    10,
       0,   134,    36,    69,    69,    33,    10,    11,   123,   124,
      51,    55,    16,   128,    18,    56,    71,   132,    46,    29,
     135,    64,    62,    38,    49,   127,    64,    62,    68,   119,
      64,    64,    22,   131,    62,    46,     4,    81,    63,    49,
      19,    39,    46,   106,     8,   178,   179,    62,    46,    32,
      64,   166,    42,    68,    18,    23,   113,    21,    62,    46,
      63,    39,   103,    66,    28,    63,    49,    31,    46,    32,
      34,     4,   113,    39,     5,     6,    63,     8,   119,    63,
     170,    10,    66,    46,   125,    27,    63,   144,   143,    66,
      21,    64,   182,   134,    12,    13,    14,    15,    63,    64,
      18,    68,    20,   144,    12,    13,    14,    15,    66,    67,
      66,    67,    66,    67,    66,    67,    64,     9,    65,    64,
      64,    62,    38,    22,    37,    64,    64,    67,    53,   170,
      50,    68,    35,    17,    27,    23,    64,   178,   179,    46,
      64,   182,    69,    30,    26,    64,    16,    64,    39,    17,
     189,    53,    42,    68,   111,   147,    56,   126,    56,   129,
      -1,   123
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     3,    72,    18,    73,     0,    62,    71,    73,    80,
      64,    63,    66,    19,    83,    73,    73,    84,    85,     4,
      81,    39,    77,    85,    80,    82,    89,    10,    46,    74,
      78,    86,    67,    80,     5,     6,     8,    21,    90,    91,
      96,    74,    27,    12,    13,    14,    15,    20,    73,    87,
      88,    67,    73,    73,    28,    31,    34,    73,    96,    97,
      98,    99,   100,   101,   103,   104,    64,    64,    81,    65,
      86,    68,    64,    87,    62,    92,    92,    11,    16,    33,
      46,    62,    73,    74,    75,    76,   105,   106,   107,   108,
     109,   111,   112,   113,   114,   105,    73,   101,   102,   103,
      62,    68,     9,    64,    38,    22,    89,    37,    64,    74,
      64,     4,    23,    93,    94,    95,     7,    36,    64,    67,
     107,   105,   111,    62,    68,    29,    49,    53,    77,    78,
      50,    79,    62,   113,    32,    35,   110,   111,   111,    99,
     111,    17,    96,    27,    93,    80,    63,    64,    64,     7,
      64,    87,    63,    63,   110,   111,    99,   106,   107,   111,
     112,   113,   111,    99,   111,    63,    66,    69,    74,    80,
      67,    95,    64,     7,    36,    64,    63,    69,    30,    32,
     111,    69,    67,    87,    64,     7,    64,    99,    99,    26,
      87,    64,    88
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    70,    71,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    80,    81,    81,    82,    82,    83,    83,
      84,    84,    85,    86,    86,    87,    87,    87,    88,    88,
      88,    88,    89,    89,    90,    91,    91,    91,    91,    91,
      91,    91,    91,    92,    92,    93,    93,    94,    94,    95,
      95,    96,    96,    97,    97,    97,    98,    98,    99,    99,
      99,    99,    99,    99,   100,   100,   101,   102,   102,   103,
     103,   104,   104,   105,   105,   106,   106,   107,   107,   108,
     108,   109,   110,   110,   111,   111,   112,   112,   113,   113,
     113,   113,   113,   113,   113,   113,   114
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     3,     0,    10,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     2,     0,     5,     4,     2,     0,
       1,     2,     6,     1,     2,     1,     8,     1,     1,     1,
       1,     1,     3,     0,     4,     6,     7,     7,     8,     4,
       5,     5,     6,     3,     0,     1,     0,     3,     1,     4,
       5,     3,     4,     0,     1,     2,     1,     3,     1,     1,
       1,     1,     4,     6,     4,     6,     3,     1,     1,     1,
       4,     1,     4,     3,     1,     3,     1,     2,     1,     3,
       1,     3,     1,     3,     1,     3,     1,     3,     1,     4,
       4,     1,     1,     1,     2,     3,     1
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


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
  if (!yypact_value_is_default (yyn))
    {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;
      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;
      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


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

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

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
  case 2: /* optional_program_parameters: '(' identifier_list ')'  */
#line 230 "Grammar.y"
        { (yyval.list) = (yyvsp[-1].ident_list).list; }
#line 1607 "Grammar.tab.c"
    break;

  case 3: /* optional_program_parameters: %empty  */
#line 232 "Grammar.y"
        { (yyval.list) = NULL; }
#line 1613 "Grammar.tab.c"
    break;

  case 4: /* program: PROGRAM ident optional_program_parameters ';' type_declarations_opt declarations subprogram_declarations compound_statement '.' END_OF_FILE  */
#line 243 "Grammar.y"
     {
         parse_tree = mk_program((yyvsp[-8].ident).line_num, (yyvsp[-8].ident).id, (yyvsp[-7].list), (yyvsp[-4].list), (yyvsp[-5].list), (yyvsp[-3].list), (yyvsp[-2].stmt));
         return -1;
     }
#line 1622 "Grammar.tab.c"
    break;

  case 5: /* ident: ID  */
#line 251 "Grammar.y"
        {
            (yyval.ident).id = yylval.id;
            (yyval.ident).line_num = line_num;
        }
#line 1631 "Grammar.tab.c"
    break;

  case 6: /* int_num: INT_NUM  */
#line 258 "Grammar.y"
              {(yyval.i_val) = yylval.i_val;}
#line 1637 "Grammar.tab.c"
    break;

  case 7: /* real_num: REAL_NUM  */
#line 262 "Grammar.y"
               {(yyval.f_val) = yylval.f_val;}
#line 1643 "Grammar.tab.c"
    break;

  case 8: /* string_literal: STRING  */
#line 266 "Grammar.y"
             {(yyval.str) = yylval.str;}
#line 1649 "Grammar.tab.c"
    break;

  case 9: /* relop: RELOP  */
#line 270 "Grammar.y"
            { (yyval.op_val) = yylval.op_val;}
#line 1655 "Grammar.tab.c"
    break;

  case 10: /* addop: ADDOP  */
#line 274 "Grammar.y"
            {(yyval.op_val) = yylval.op_val;}
#line 1661 "Grammar.tab.c"
    break;

  case 11: /* mulop: MULOP  */
#line 278 "Grammar.y"
            {(yyval.op_val) = yylval.op_val;}
#line 1667 "Grammar.tab.c"
    break;

  case 12: /* identifier_list: ident  */
#line 282 "Grammar.y"
        {
            (yyval.ident_list).list = CreateListNode((yyvsp[0].ident).id, LIST_STRING);
            (yyval.ident_list).line_num = (yyvsp[0].ident).line_num; /* TODO: List of line nums */
        }
#line 1676 "Grammar.tab.c"
    break;

  case 13: /* identifier_list: identifier_list ',' ident  */
#line 287 "Grammar.y"
        {
            (yyval.ident_list).list = PushListNodeBack((yyvsp[-2].ident_list).list, CreateListNode((yyvsp[0].ident).id, LIST_STRING));
            (yyval.ident_list).line_num = (yyvsp[-2].ident_list).line_num;
        }
#line 1685 "Grammar.tab.c"
    break;

  case 14: /* declarations: VARIABLE declaration_list  */
#line 295 "Grammar.y"
        { (yyval.list) = (yyvsp[0].list); }
#line 1691 "Grammar.tab.c"
    break;

  case 15: /* declarations: %empty  */
#line 296 "Grammar.y"
                  {(yyval.list) = NULL;}
#line 1697 "Grammar.tab.c"
    break;

  case 16: /* declaration_list: declaration_list identifier_list ':' type ';'  */
#line 301 "Grammar.y"
        {
            Tree_t *tree;
            if((yyvsp[-1].type_s).type == ARRAY)
                tree = mk_arraydecl((yyvsp[-3].ident_list).line_num, (yyvsp[-3].ident_list).list, (yyvsp[-1].type_s).actual_type, (yyvsp[-1].type_s).start, (yyvsp[-1].type_s).end);
            else
                tree = mk_vardecl((yyvsp[-3].ident_list).line_num, (yyvsp[-3].ident_list).list, (yyvsp[-1].type_s).actual_type, (yyvsp[-1].type_s).id, 0);

            if((yyvsp[-4].list) == NULL)
                (yyval.list) = CreateListNode(tree, LIST_TREE);
            else
                (yyval.list) = PushListNodeBack((yyvsp[-4].list), CreateListNode(tree, LIST_TREE));
        }
#line 1714 "Grammar.tab.c"
    break;

  case 17: /* declaration_list: identifier_list ':' type ';'  */
#line 314 "Grammar.y"
        {
            Tree_t *tree;
            if((yyvsp[-1].type_s).type == ARRAY)
                tree = mk_arraydecl((yyvsp[-3].ident_list).line_num, (yyvsp[-3].ident_list).list, (yyvsp[-1].type_s).actual_type, (yyvsp[-1].type_s).start, (yyvsp[-1].type_s).end);
            else
                tree = mk_vardecl((yyvsp[-3].ident_list).line_num, (yyvsp[-3].ident_list).list, (yyvsp[-1].type_s).actual_type, (yyvsp[-1].type_s).id, 0);

            (yyval.list) = CreateListNode(tree, LIST_TREE);
        }
#line 1728 "Grammar.tab.c"
    break;

  case 18: /* type_declarations_opt: TYPE type_declaration_list  */
#line 326 "Grammar.y"
                                 { (yyval.list) = (yyvsp[0].list); }
#line 1734 "Grammar.tab.c"
    break;

  case 19: /* type_declarations_opt: %empty  */
#line 327 "Grammar.y"
                  { (yyval.list) = NULL; }
#line 1740 "Grammar.tab.c"
    break;

  case 20: /* type_declaration_list: type_declaration  */
#line 332 "Grammar.y"
        { (yyval.list) = CreateListNode((yyvsp[0].tree), LIST_TREE); }
#line 1746 "Grammar.tab.c"
    break;

  case 21: /* type_declaration_list: type_declaration_list type_declaration  */
#line 334 "Grammar.y"
        { (yyval.list) = PushListNodeBack((yyvsp[-1].list), CreateListNode((yyvsp[0].tree), LIST_TREE)); }
#line 1752 "Grammar.tab.c"
    break;

  case 22: /* type_declaration: ident relop signed_int DOTDOT signed_int ';'  */
#line 339 "Grammar.y"
    {
        if ((yyvsp[-4].op_val) != EQ) {
            yyerror("Expected '=' in type declaration");
        }
        (yyval.tree) = mk_typedecl((yyvsp[-5].ident).line_num, (yyvsp[-5].ident).id, (yyvsp[-3].i_val), (yyvsp[-1].i_val));
    }
#line 1763 "Grammar.tab.c"
    break;

  case 23: /* signed_int: int_num  */
#line 348 "Grammar.y"
              { (yyval.i_val) = (yyvsp[0].i_val); }
#line 1769 "Grammar.tab.c"
    break;

  case 24: /* signed_int: addop int_num  */
#line 350 "Grammar.y"
    {
        if ((yyvsp[-1].op_val) == MINUS) {
            (yyval.i_val) = -(yyvsp[0].i_val);
        } else {
            (yyval.i_val) = (yyvsp[0].i_val);
        }
    }
#line 1781 "Grammar.tab.c"
    break;

  case 25: /* type: standard_type  */
#line 361 "Grammar.y"
        {
            (yyval.type_s).type = SINGLE;
            (yyval.type_s).actual_type = (yyvsp[0].i_val);
            (yyval.type_s).id = NULL;
        }
#line 1791 "Grammar.tab.c"
    break;

  case 26: /* type: ARRAY '[' int_num DOTDOT int_num ']' OF standard_type  */
#line 367 "Grammar.y"
        {
            (yyval.type_s).type = ARRAY;
            (yyval.type_s).actual_type = (yyvsp[0].i_val);
            (yyval.type_s).id = NULL;
            (yyval.type_s).start = (yyvsp[-5].i_val);
            (yyval.type_s).end = (yyvsp[-3].i_val);
        }
#line 1803 "Grammar.tab.c"
    break;

  case 27: /* type: ident  */
#line 375 "Grammar.y"
        {
            (yyval.type_s).type = ID;
            (yyval.type_s).id = (yyvsp[0].ident).id;
            (yyval.type_s).actual_type = -1;
        }
#line 1813 "Grammar.tab.c"
    break;

  case 28: /* standard_type: INT_TYPE  */
#line 383 "Grammar.y"
               {(yyval.i_val) = INT_TYPE;}
#line 1819 "Grammar.tab.c"
    break;

  case 29: /* standard_type: REAL_TYPE  */
#line 384 "Grammar.y"
                {(yyval.i_val) = REAL_TYPE;}
#line 1825 "Grammar.tab.c"
    break;

  case 30: /* standard_type: LONGINT_TYPE  */
#line 385 "Grammar.y"
                   {(yyval.i_val) = LONGINT_TYPE;}
#line 1831 "Grammar.tab.c"
    break;

  case 31: /* standard_type: STRING_TYPE  */
#line 386 "Grammar.y"
                  {(yyval.i_val) = STRING_TYPE;}
#line 1837 "Grammar.tab.c"
    break;

  case 32: /* subprogram_declarations: subprogram_declarations subprogram_declaration ';'  */
#line 391 "Grammar.y"
        {
            if((yyvsp[-2].list) == NULL)
                (yyval.list) = CreateListNode((yyvsp[-1].tree), LIST_TREE);
            else
                (yyval.list) = PushListNodeBack((yyvsp[-2].list), CreateListNode((yyvsp[-1].tree), LIST_TREE));
        }
#line 1848 "Grammar.tab.c"
    break;

  case 33: /* subprogram_declarations: %empty  */
#line 397 "Grammar.y"
                  {(yyval.list) = NULL;}
#line 1854 "Grammar.tab.c"
    break;

  case 34: /* subprogram_declaration: subprogram_head declarations subprogram_declarations compound_statement  */
#line 402 "Grammar.y"
        {
            if((yyvsp[-3].subprogram_head_s).sub_type == PROCEDURE)
                (yyval.tree) = mk_procedure((yyvsp[-3].subprogram_head_s).line_num, (yyvsp[-3].subprogram_head_s).id, (yyvsp[-3].subprogram_head_s).args, (yyvsp[-2].list), (yyvsp[-1].list), (yyvsp[0].stmt), (yyvsp[-3].subprogram_head_s).cname_flag, (yyvsp[-3].subprogram_head_s).overload_flag);
            else
                (yyval.tree) = mk_function((yyvsp[-3].subprogram_head_s).line_num, (yyvsp[-3].subprogram_head_s).id, (yyvsp[-3].subprogram_head_s).args, (yyvsp[-2].list), (yyvsp[-1].list), (yyvsp[0].stmt), (yyvsp[-3].subprogram_head_s).return_type, (yyvsp[-3].subprogram_head_s).return_type_id, (yyvsp[-3].subprogram_head_s).cname_flag, (yyvsp[-3].subprogram_head_s).overload_flag);
        }
#line 1865 "Grammar.tab.c"
    break;

  case 35: /* subprogram_head: FUNCTION ident arguments ':' type ';'  */
#line 412 "Grammar.y"
        {
            (yyval.subprogram_head_s).sub_type = FUNCTION;
            (yyval.subprogram_head_s).args = (yyvsp[-3].list);
            if ((yyvsp[-1].type_s).type == ID) {
                (yyval.subprogram_head_s).return_type = -1;
                (yyval.subprogram_head_s).return_type_id = (yyvsp[-1].type_s).id;
            } else {
                (yyval.subprogram_head_s).return_type = (yyvsp[-1].type_s).actual_type;
                (yyval.subprogram_head_s).return_type_id = NULL;
            }

            (yyval.subprogram_head_s).id = (yyvsp[-4].ident).id;
            (yyval.subprogram_head_s).line_num = (yyvsp[-4].ident).line_num;
            (yyval.subprogram_head_s).cname_flag = 0;
            (yyval.subprogram_head_s).overload_flag = 0;
        }
#line 1886 "Grammar.tab.c"
    break;

  case 36: /* subprogram_head: FUNCTION ident arguments ':' type OVERLOAD ';'  */
#line 429 "Grammar.y"
        {
            (yyval.subprogram_head_s).sub_type = FUNCTION;
            (yyval.subprogram_head_s).args = (yyvsp[-4].list);
            if ((yyvsp[-2].type_s).type == ID) {
                (yyval.subprogram_head_s).return_type = -1;
                (yyval.subprogram_head_s).return_type_id = (yyvsp[-2].type_s).id;
            } else {
                (yyval.subprogram_head_s).return_type = (yyvsp[-2].type_s).actual_type;
                (yyval.subprogram_head_s).return_type_id = NULL;
            }

            (yyval.subprogram_head_s).id = (yyvsp[-5].ident).id;
            (yyval.subprogram_head_s).line_num = (yyvsp[-5].ident).line_num;
            (yyval.subprogram_head_s).cname_flag = 0;
            (yyval.subprogram_head_s).overload_flag = 1;
        }
#line 1907 "Grammar.tab.c"
    break;

  case 37: /* subprogram_head: FUNCTION ident arguments ':' type CNAME ';'  */
#line 446 "Grammar.y"
        {
            (yyval.subprogram_head_s).sub_type = FUNCTION;
            (yyval.subprogram_head_s).args = (yyvsp[-4].list);
            if ((yyvsp[-2].type_s).type == ID) {
                (yyval.subprogram_head_s).return_type = -1;
                (yyval.subprogram_head_s).return_type_id = (yyvsp[-2].type_s).id;
            } else {
                (yyval.subprogram_head_s).return_type = (yyvsp[-2].type_s).actual_type;
                (yyval.subprogram_head_s).return_type_id = NULL;
            }

            (yyval.subprogram_head_s).id = (yyvsp[-5].ident).id;
            (yyval.subprogram_head_s).line_num = (yyvsp[-5].ident).line_num;
            (yyval.subprogram_head_s).cname_flag = 1;
            (yyval.subprogram_head_s).overload_flag = 0;
        }
#line 1928 "Grammar.tab.c"
    break;

  case 38: /* subprogram_head: FUNCTION ident arguments ':' type CNAME OVERLOAD ';'  */
#line 463 "Grammar.y"
        {
            (yyval.subprogram_head_s).sub_type = FUNCTION;
            (yyval.subprogram_head_s).args = (yyvsp[-5].list);
            if ((yyvsp[-3].type_s).type == ID) {
                (yyval.subprogram_head_s).return_type = -1;
                (yyval.subprogram_head_s).return_type_id = (yyvsp[-3].type_s).id;
            } else {
                (yyval.subprogram_head_s).return_type = (yyvsp[-3].type_s).actual_type;
                (yyval.subprogram_head_s).return_type_id = NULL;
            }

            (yyval.subprogram_head_s).id = (yyvsp[-6].ident).id;
            (yyval.subprogram_head_s).line_num = (yyvsp[-6].ident).line_num;
            (yyval.subprogram_head_s).cname_flag = 1;
            (yyval.subprogram_head_s).overload_flag = 1;
        }
#line 1949 "Grammar.tab.c"
    break;

  case 39: /* subprogram_head: PROCEDURE ident arguments ';'  */
#line 480 "Grammar.y"
        {
            (yyval.subprogram_head_s).sub_type = PROCEDURE;
            (yyval.subprogram_head_s).args = (yyvsp[-1].list);
            (yyval.subprogram_head_s).return_type = -1;
            (yyval.subprogram_head_s).return_type_id = NULL;

            (yyval.subprogram_head_s).id = (yyvsp[-2].ident).id;
            (yyval.subprogram_head_s).line_num = (yyvsp[-2].ident).line_num;
            (yyval.subprogram_head_s).cname_flag = 0;
            (yyval.subprogram_head_s).overload_flag = 0;
        }
#line 1965 "Grammar.tab.c"
    break;

  case 40: /* subprogram_head: PROCEDURE ident arguments OVERLOAD ';'  */
#line 492 "Grammar.y"
        {
            (yyval.subprogram_head_s).sub_type = PROCEDURE;
            (yyval.subprogram_head_s).args = (yyvsp[-2].list);
            (yyval.subprogram_head_s).return_type = -1;
            (yyval.subprogram_head_s).return_type_id = NULL;

            (yyval.subprogram_head_s).id = (yyvsp[-3].ident).id;
            (yyval.subprogram_head_s).line_num = (yyvsp[-3].ident).line_num;
            (yyval.subprogram_head_s).cname_flag = 0;
            (yyval.subprogram_head_s).overload_flag = 1;
        }
#line 1981 "Grammar.tab.c"
    break;

  case 41: /* subprogram_head: PROCEDURE ident arguments CNAME ';'  */
#line 504 "Grammar.y"
        {
            (yyval.subprogram_head_s).sub_type = PROCEDURE;
            (yyval.subprogram_head_s).args = (yyvsp[-2].list);
            (yyval.subprogram_head_s).return_type = -1;
            (yyval.subprogram_head_s).return_type_id = NULL;

            (yyval.subprogram_head_s).id = (yyvsp[-3].ident).id;
            (yyval.subprogram_head_s).line_num = (yyvsp[-3].ident).line_num;
            (yyval.subprogram_head_s).cname_flag = 1;
            (yyval.subprogram_head_s).overload_flag = 0;
        }
#line 1997 "Grammar.tab.c"
    break;

  case 42: /* subprogram_head: PROCEDURE ident arguments CNAME OVERLOAD ';'  */
#line 516 "Grammar.y"
        {
            (yyval.subprogram_head_s).sub_type = PROCEDURE;
            (yyval.subprogram_head_s).args = (yyvsp[-3].list);
            (yyval.subprogram_head_s).return_type = -1;
            (yyval.subprogram_head_s).return_type_id = NULL;

            (yyval.subprogram_head_s).id = (yyvsp[-4].ident).id;
            (yyval.subprogram_head_s).line_num = (yyvsp[-4].ident).line_num;
            (yyval.subprogram_head_s).cname_flag = 1;
            (yyval.subprogram_head_s).overload_flag = 1;
        }
#line 2013 "Grammar.tab.c"
    break;

  case 43: /* arguments: '(' parameter_list ')'  */
#line 530 "Grammar.y"
                             {(yyval.list) = (yyvsp[-1].list);}
#line 2019 "Grammar.tab.c"
    break;

  case 44: /* arguments: %empty  */
#line 531 "Grammar.y"
                  {(yyval.list) = NULL;}
#line 2025 "Grammar.tab.c"
    break;

  case 47: /* parameter_list: parameter_list ';' parameter_item  */
#line 541 "Grammar.y"
        { (yyval.list) = PushListNodeBack((yyvsp[-2].list), CreateListNode((yyvsp[0].tree), LIST_TREE)); }
#line 2031 "Grammar.tab.c"
    break;

  case 48: /* parameter_list: parameter_item  */
#line 543 "Grammar.y"
        { (yyval.list) = CreateListNode((yyvsp[0].tree), LIST_TREE); }
#line 2037 "Grammar.tab.c"
    break;

  case 49: /* parameter_item: optional_const identifier_list ':' type  */
#line 548 "Grammar.y"
        {
            Tree_t *tree;
            if((yyvsp[0].type_s).type == ARRAY)
                tree = mk_arraydecl((yyvsp[-2].ident_list).line_num, (yyvsp[-2].ident_list).list, (yyvsp[0].type_s).actual_type, (yyvsp[0].type_s).start, (yyvsp[0].type_s).end);
            else
                tree = mk_vardecl((yyvsp[-2].ident_list).line_num, (yyvsp[-2].ident_list).list, (yyvsp[0].type_s).actual_type, (yyvsp[0].type_s).id, 0);
            (yyval.tree) = tree;
        }
#line 2050 "Grammar.tab.c"
    break;

  case 50: /* parameter_item: VARIABLE optional_const identifier_list ':' type  */
#line 557 "Grammar.y"
        {
            Tree_t *tree;
            if((yyvsp[0].type_s).type == ARRAY)
                tree = mk_arraydecl((yyvsp[-2].ident_list).line_num, (yyvsp[-2].ident_list).list, (yyvsp[0].type_s).actual_type, (yyvsp[0].type_s).start, (yyvsp[0].type_s).end);
            else
                tree = mk_vardecl((yyvsp[-2].ident_list).line_num, (yyvsp[-2].ident_list).list, (yyvsp[0].type_s).actual_type, (yyvsp[0].type_s).id, 1);
            (yyval.tree) = tree;
        }
#line 2063 "Grammar.tab.c"
    break;

  case 51: /* compound_statement: BBEGIN statement_seq_opt END  */
#line 568 "Grammar.y"
                                   { (yyval.stmt) = mk_compoundstatement(line_num, (yyvsp[-1].list)); }
#line 2069 "Grammar.tab.c"
    break;

  case 52: /* compound_statement: ASSEMBLER ';' ASM ENDASM  */
#line 569 "Grammar.y"
                               { (yyval.stmt) = mk_asmblock(line_num, (yyvsp[0].str)); }
#line 2075 "Grammar.tab.c"
    break;

  case 53: /* statement_seq_opt: %empty  */
#line 574 "Grammar.y"
        { (yyval.list) = NULL; }
#line 2081 "Grammar.tab.c"
    break;

  case 54: /* statement_seq_opt: statement_seq  */
#line 576 "Grammar.y"
        { (yyval.list) = (yyvsp[0].list); }
#line 2087 "Grammar.tab.c"
    break;

  case 55: /* statement_seq_opt: statement_seq ';'  */
#line 578 "Grammar.y"
        { (yyval.list) = (yyvsp[-1].list); }
#line 2093 "Grammar.tab.c"
    break;

  case 56: /* statement_seq: statement  */
#line 583 "Grammar.y"
        { (yyval.list) = CreateListNode((yyvsp[0].stmt), LIST_STMT); }
#line 2099 "Grammar.tab.c"
    break;

  case 57: /* statement_seq: statement_seq ';' statement  */
#line 585 "Grammar.y"
        { (yyval.list) = PushListNodeBack((yyvsp[-2].list), CreateListNode((yyvsp[0].stmt), LIST_STMT)); }
#line 2105 "Grammar.tab.c"
    break;

  case 58: /* statement: variable_assignment  */
#line 590 "Grammar.y"
        {
            (yyval.stmt) = (yyvsp[0].stmt);
        }
#line 2113 "Grammar.tab.c"
    break;

  case 59: /* statement: procedure_statement  */
#line 594 "Grammar.y"
        {
            (yyval.stmt) = (yyvsp[0].stmt);
        }
#line 2121 "Grammar.tab.c"
    break;

  case 60: /* statement: compound_statement  */
#line 598 "Grammar.y"
        {
            (yyval.stmt) = (yyvsp[0].stmt);
        }
#line 2129 "Grammar.tab.c"
    break;

  case 61: /* statement: if_statement  */
#line 602 "Grammar.y"
        {
            (yyval.stmt) = (yyvsp[0].stmt);
        }
#line 2137 "Grammar.tab.c"
    break;

  case 62: /* statement: WHILE relop_expression DO statement  */
#line 606 "Grammar.y"
        {
            (yyval.stmt) = mk_while(line_num, (yyvsp[-2].expr), (yyvsp[0].stmt));
        }
#line 2145 "Grammar.tab.c"
    break;

  case 63: /* statement: FOR for_assign TO expression DO statement  */
#line 610 "Grammar.y"
        {
            if((yyvsp[-4].for_assign_bison).assign_type == VAR_ASSIGN)
                (yyval.stmt) = mk_forassign(line_num, (yyvsp[-4].for_assign_bison).for_assign_bison_union.stmt, (yyvsp[-2].expr), (yyvsp[0].stmt));
            else
                (yyval.stmt) = mk_forvar(line_num, (yyvsp[-4].for_assign_bison).for_assign_bison_union.expr, (yyvsp[-2].expr), (yyvsp[0].stmt));
        }
#line 2156 "Grammar.tab.c"
    break;

  case 64: /* if_statement: IF relop_expression THEN statement  */
#line 621 "Grammar.y"
        {
            (yyval.stmt) = mk_ifthen(line_num, (yyvsp[-2].expr), (yyvsp[0].stmt), NULL);
        }
#line 2164 "Grammar.tab.c"
    break;

  case 65: /* if_statement: IF relop_expression THEN statement ELSE statement  */
#line 625 "Grammar.y"
        {
            (yyval.stmt) = mk_ifthen(line_num, (yyvsp[-4].expr), (yyvsp[-2].stmt), (yyvsp[0].stmt));
        }
#line 2172 "Grammar.tab.c"
    break;

  case 66: /* variable_assignment: variable ASSIGNOP expression  */
#line 631 "Grammar.y"
        {
            (yyval.stmt) = mk_varassign(line_num, (yyvsp[-2].expr), (yyvsp[0].expr));
        }
#line 2180 "Grammar.tab.c"
    break;

  case 67: /* for_assign: variable_assignment  */
#line 637 "Grammar.y"
        {
            (yyval.for_assign_bison).assign_type = VAR_ASSIGN;
            (yyval.for_assign_bison).for_assign_bison_union.stmt = (yyvsp[0].stmt);
        }
#line 2189 "Grammar.tab.c"
    break;

  case 68: /* for_assign: variable  */
#line 642 "Grammar.y"
        {
            (yyval.for_assign_bison).assign_type = VAR;
            (yyval.for_assign_bison).for_assign_bison_union.expr = (yyvsp[0].expr);
        }
#line 2198 "Grammar.tab.c"
    break;

  case 69: /* variable: ident  */
#line 649 "Grammar.y"
        {
            (yyval.expr) = mk_varid((yyvsp[0].ident).line_num, (yyvsp[0].ident).id);
        }
#line 2206 "Grammar.tab.c"
    break;

  case 70: /* variable: ident '[' expression ']'  */
#line 653 "Grammar.y"
        {
            (yyval.expr) = mk_arrayaccess((yyvsp[-3].ident).line_num, (yyvsp[-3].ident).id, (yyvsp[-1].expr));
        }
#line 2214 "Grammar.tab.c"
    break;

  case 71: /* procedure_statement: ident  */
#line 660 "Grammar.y"
        {
            (yyval.stmt) = mk_procedurecall((yyvsp[0].ident).line_num, (yyvsp[0].ident).id, NULL);
        }
#line 2222 "Grammar.tab.c"
    break;

  case 72: /* procedure_statement: ident '(' expression_list ')'  */
#line 664 "Grammar.y"
        {
            (yyval.stmt) = mk_procedurecall((yyvsp[-3].ident).line_num, (yyvsp[-3].ident).id, (yyvsp[-1].list));
        }
#line 2230 "Grammar.tab.c"
    break;

  case 73: /* relop_expression: relop_expression OR relop_and  */
#line 673 "Grammar.y"
        {
            (yyval.expr) = mk_relop(line_num, OR, (yyvsp[-2].expr), (yyvsp[0].expr));
        }
#line 2238 "Grammar.tab.c"
    break;

  case 74: /* relop_expression: relop_and  */
#line 676 "Grammar.y"
                {(yyval.expr) = (yyvsp[0].expr);}
#line 2244 "Grammar.tab.c"
    break;

  case 75: /* relop_and: relop_and AND relop_not  */
#line 681 "Grammar.y"
        {
            (yyval.expr) = mk_relop(line_num, AND, (yyvsp[-2].expr), (yyvsp[0].expr));
        }
#line 2252 "Grammar.tab.c"
    break;

  case 76: /* relop_and: relop_not  */
#line 684 "Grammar.y"
                {(yyval.expr) = (yyvsp[0].expr);}
#line 2258 "Grammar.tab.c"
    break;

  case 77: /* relop_not: NOT relop_not  */
#line 689 "Grammar.y"
        {
            (yyval.expr) = mk_relop(line_num, NOT, (yyvsp[0].expr), NULL);
        }
#line 2266 "Grammar.tab.c"
    break;

  case 78: /* relop_not: relop_paren  */
#line 692 "Grammar.y"
                  {(yyval.expr) = (yyvsp[0].expr);}
#line 2272 "Grammar.tab.c"
    break;

  case 79: /* relop_paren: '(' relop_expression ')'  */
#line 696 "Grammar.y"
                               {(yyval.expr) = (yyvsp[-1].expr);}
#line 2278 "Grammar.tab.c"
    break;

  case 80: /* relop_paren: relop_expression_single  */
#line 697 "Grammar.y"
                              {(yyval.expr) = (yyvsp[0].expr);}
#line 2284 "Grammar.tab.c"
    break;

  case 81: /* relop_expression_single: expression relop expression  */
#line 702 "Grammar.y"
        {
            (yyval.expr) = mk_relop(line_num, (yyvsp[-1].op_val), (yyvsp[-2].expr), (yyvsp[0].expr));
        }
#line 2292 "Grammar.tab.c"
    break;

  case 82: /* expression_list: expression  */
#line 711 "Grammar.y"
        {
            (yyval.list) = CreateListNode((yyvsp[0].expr), LIST_EXPR);
        }
#line 2300 "Grammar.tab.c"
    break;

  case 83: /* expression_list: expression_list ',' expression  */
#line 715 "Grammar.y"
        {
            (yyval.list) = PushListNodeBack((yyvsp[-2].list), CreateListNode((yyvsp[0].expr), LIST_EXPR));
        }
#line 2308 "Grammar.tab.c"
    break;

  case 84: /* expression: term  */
#line 721 "Grammar.y"
           {(yyval.expr) = (yyvsp[0].expr);}
#line 2314 "Grammar.tab.c"
    break;

  case 85: /* expression: expression addop term  */
#line 723 "Grammar.y"
        {
            (yyval.expr) = mk_addop(line_num, (yyvsp[-1].op_val), (yyvsp[-2].expr), (yyvsp[0].expr));
        }
#line 2322 "Grammar.tab.c"
    break;

  case 86: /* term: factor  */
#line 729 "Grammar.y"
             {(yyval.expr) = (yyvsp[0].expr);}
#line 2328 "Grammar.tab.c"
    break;

  case 87: /* term: term mulop factor  */
#line 731 "Grammar.y"
        {
            (yyval.expr) = mk_mulop(line_num, (yyvsp[-1].op_val), (yyvsp[-2].expr), (yyvsp[0].expr));
        }
#line 2336 "Grammar.tab.c"
    break;

  case 88: /* factor: ident  */
#line 738 "Grammar.y"
        {
            (yyval.expr) = mk_varid((yyvsp[0].ident).line_num, (yyvsp[0].ident).id);
        }
#line 2344 "Grammar.tab.c"
    break;

  case 89: /* factor: ident '[' expression ']'  */
#line 742 "Grammar.y"
        {
            (yyval.expr) = mk_arrayaccess((yyvsp[-3].ident).line_num, (yyvsp[-3].ident).id, (yyvsp[-1].expr));
        }
#line 2352 "Grammar.tab.c"
    break;

  case 90: /* factor: ident '(' expression_list ')'  */
#line 746 "Grammar.y"
        {
            (yyval.expr) = mk_functioncall((yyvsp[-3].ident).line_num, (yyvsp[-3].ident).id, (yyvsp[-1].list));
        }
#line 2360 "Grammar.tab.c"
    break;

  case 91: /* factor: int_num  */
#line 750 "Grammar.y"
        {
            (yyval.expr) = mk_inum(line_num, (yyvsp[0].i_val));
        }
#line 2368 "Grammar.tab.c"
    break;

  case 92: /* factor: real_num  */
#line 754 "Grammar.y"
        {
            (yyval.expr) = mk_rnum(line_num, (yyvsp[0].f_val));
        }
#line 2376 "Grammar.tab.c"
    break;

  case 93: /* factor: string_literal  */
#line 758 "Grammar.y"
        {
            (yyval.expr) = mk_string(line_num, (yyvsp[0].str));
        }
#line 2384 "Grammar.tab.c"
    break;

  case 94: /* factor: sign factor  */
#line 762 "Grammar.y"
        {
            if((yyvsp[-1].op_val) == MINUS)
                (yyval.expr) = mk_signterm(line_num, (yyvsp[0].expr));
            else
                (yyval.expr) = (yyvsp[0].expr);
        }
#line 2395 "Grammar.tab.c"
    break;

  case 95: /* factor: '(' expression ')'  */
#line 769 "Grammar.y"
        {
            (yyval.expr) = (yyvsp[-1].expr);
        }
#line 2403 "Grammar.tab.c"
    break;

  case 96: /* sign: ADDOP  */
#line 776 "Grammar.y"
        {
            (yyval.op_val) = yylval.op_val;
        }
#line 2411 "Grammar.tab.c"
    break;


#line 2415 "Grammar.tab.c"

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
      {
        yypcontext_t yyctx
          = {yyssp, yytoken};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
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
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 781 "Grammar.y"


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
