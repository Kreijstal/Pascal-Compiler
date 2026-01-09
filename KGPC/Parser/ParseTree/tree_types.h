/*
    Damon Gwinn
    Helper types for tree.h
*/

#ifndef TREE_TYPES_H
#define TREE_TYPES_H

#include "../List/List.h"

/* Forward declarations to avoid circular dependencies */
struct KgpcType;
struct HashNode;  /* Forward declare HashNode to avoid circular dependency */
struct Tree;      /* Forward declare Tree so MethodTemplate can reference it */
struct ast_t;     /* Forward-declare AST type without including parser headers */
struct GenericTypeDecl;
struct RecordType;

/* Enums for readability with types */
enum StmtType{STMT_VAR_ASSIGN, STMT_PROCEDURE_CALL, STMT_COMPOUND_STATEMENT,
    STMT_LABEL, STMT_GOTO, STMT_IF_THEN, STMT_WHILE, STMT_REPEAT, STMT_FOR, STMT_FOR_VAR,
    STMT_FOR_ASSIGN_VAR, STMT_FOR_IN, STMT_ASM_BLOCK, STMT_EXIT, STMT_BREAK, STMT_CONTINUE, STMT_CASE, STMT_WITH,
    STMT_TRY_FINALLY, STMT_TRY_EXCEPT, STMT_RAISE, STMT_INHERITED};

enum TypeDeclKind { TYPE_DECL_RANGE, TYPE_DECL_RECORD, TYPE_DECL_ALIAS, TYPE_DECL_GENERIC };

struct TypeAlias
{
    char *alias_name; /* The name of this alias (e.g. "RawByteString") */
    int base_type;
    int is_char_alias;
    char *target_type_id;
    struct RecordType *inline_record_type;
    int is_array;
    int array_start;
    int array_end;
    int array_element_type;
    char *array_element_type_id;
    int is_shortstring;
    int is_open_array;
    ListNode_t *array_dimensions;
    int is_pointer;
    int pointer_type;
    char *pointer_type_id;
    int is_set;
    int set_element_type;
    char *set_element_type_id;
    int is_enum;
    ListNode_t *enum_literals;
    int is_file;
    int file_type;
    char *file_type_id;
    /* Range/size metadata for scalar aliases */
    int is_range;
    int range_known;
    long long range_start;
    long long range_end;
    long long storage_size;
    
    /* KgpcType for this type alias - used for enums and sets to provide a shared
     * type that all literals/members reference. Owned by this structure. */
    struct KgpcType *kgpc_type;
};

/* Generic type declaration information */
struct GenericDecl
{
    char **type_parameters;     /* Array of type parameter names (e.g., ["T"]) */
    int num_type_params;        /* Number of type parameters */
    struct ast_t *original_ast; /* Pointer to original AST node for substitution */
    struct RecordType *record_template; /* Optional record/class template for generics */
};

struct RecordField
{
    char *name;
    int type;
    char *type_id;
    struct RecordType *nested_record;
    int is_array;
    int array_start;
    int array_end;
    int array_element_type;
    char *array_element_type_id;
    int array_is_open;
    int is_hidden;
};

struct ClassProperty
{
    char *name;
    int type;
    char *type_id;
    char *read_accessor;
    char *write_accessor;
    int is_indexed;
};

/* Method information for classes */
struct MethodInfo
{
    char *name;               /* Method name (unmangled) */
    char *mangled_name;       /* Mangled name (ClassName__MethodName) */
    int is_virtual;           /* 1 if declared virtual */
    int is_override;          /* 1 if declared override */
    int vmt_index;            /* Index in VMT (-1 if not virtual) */
};

enum MethodTemplateKind
{
    METHOD_TEMPLATE_UNKNOWN = 0,
    METHOD_TEMPLATE_PROCEDURE,
    METHOD_TEMPLATE_FUNCTION,
    METHOD_TEMPLATE_CONSTRUCTOR,
    METHOD_TEMPLATE_DESTRUCTOR,
    METHOD_TEMPLATE_OPERATOR
};

/* Template copy of a method declaration for future instantiation */
struct MethodTemplate
{
    char *name;               /* Simple method name */
    struct ast_t *method_ast; /* Cloned AST for the original declaration */
    struct Tree *method_tree; /* Converted Tree_t template built on-demand */
    enum MethodTemplateKind kind;  /* Method classification */
    int is_class_method;      /* 1 if declared with CLASS */
    int is_static;            /* 1 if directive static found (no Self parameter) */
    int is_virtual;           /* 1 if directive virtual found */
    int is_override;          /* 1 if directive override found */
    int has_return_type;      /* 1 if function with explicit return type */
    struct ast_t *params_ast;       /* Pointer inside method_ast for parameter list */
    struct ast_t *return_type_ast;  /* Pointer inside method_ast for return type */
    struct ast_t *directives_ast;   /* Pointer inside method_ast for directives */
    struct ast_t *method_impl_ast;  /* Cloned AST for the implementation */
};

struct RecordType
{
    ListNode_t *fields;
    ListNode_t *properties;
    char *parent_class_name;  /* For class inheritance */
    ListNode_t *methods;      /* List of MethodInfo for virtual/override methods */
    ListNode_t *method_templates; /* Template methods captured from declarations */
    int is_class;             /* 1 if this record represents a class */
    int is_type_helper;       /* 1 if this record represents a type helper */
    char *helper_base_type_id; /* Base type name for helpers */
    char *type_id;            /* Canonical type name if available */
    int has_cached_size;      /* 1 if cached_size has been computed */
    long long cached_size;    /* Cached byte size for kgpc_type_sizeof */
    struct GenericTypeDecl *generic_decl; /* Owning generic declaration, if any */
    char **generic_args;      /* Concrete type arguments for specialization */
    int num_generic_args;
    int method_clones_emitted; /* 1 if generic method clones have been appended */
};

static inline int record_type_is_class(const struct RecordType *record)
{
    if (record == NULL)
        return 0;
    if (record->is_class)
        return 1;
    return (record->properties != NULL);
}

static inline int record_field_is_hidden(const struct RecordField *field)
{
    return (field != NULL) ? field->is_hidden : 0;
}

struct VariantBranch
{
    ListNode_t *labels; /* List of Expression */
    ListNode_t *members; /* List of RecordField / VariantPart */
};

struct VariantPart
{
    struct RecordField *tag_field; /* Optional discriminant field */
    int tag_type;
    char *tag_type_id;
    struct RecordType *tag_record;
    ListNode_t *branches; /* List of VariantBranch */
    int has_cached_size;
    long long cached_size;
};

/* Case branch structure */
struct CaseBranch
{
    ListNode_t *labels;  /* List of expressions or range elements representing case labels */
    struct Statement *stmt;
};

/* A statement subtree */
struct Statement
{
    int line_num;
    int col_num;
    enum StmtType type;
    union stmt_data
    {
        /* Variable assignment */
        struct VarAssign
        {
            struct Expression *var;
            struct Expression *expr;
        } var_assign_data;

        /* Asm block */
        struct AsmBlock
        {
            char *code;
        } asm_block_data;

        /* Procedure call */
        struct ProcedureCall
        {
            char *id;
            char *mangled_id;
            ListNode_t *expr_args;
            struct HashNode *resolved_proc;  /* DEPRECATED: May point to freed memory! Use fields below instead. */
            
            /* Cached information from HashNode to avoid use-after-free.
             * These are populated during semantic checking and remain valid for codegen. */
            int call_hash_type;              /* HashType enum value (HASHTYPE_VAR, HASHTYPE_PROCEDURE, etc.) */
            struct KgpcType *call_kgpc_type;   /* KgpcType for getting formal parameters */
            int is_call_info_valid;          /* 1 if the above fields are valid, 0 otherwise */
            int is_procedural_var_call;      /* 1 if calling through a procedural variable/expression */
            struct HashNode *procedural_var_symbol; /* Symbol for procedural var (if any) */
            struct Expression *procedural_var_expr; /* Expression yielding procedure pointer */
        } procedure_call_data;

        /* Compound Statements */
        ListNode_t *compound_statement;

        /* Labelled statement */
        struct LabelStatement
        {
            char *label;
            struct Statement *stmt;
        } label_data;

        /* Goto statement */
        struct GotoStatement
        {
            char *label;
        } goto_data;

        /* IF THEN ELSE */
        struct IfThenElse
        {
            struct Expression *relop_expr;
            struct Statement *if_stmt;
            struct Statement *else_stmt; /* NOTE: can be null */
        } if_then_data;

        /* WHILE */
        struct While
        {
            struct Expression *relop_expr;
            struct Statement *while_stmt;
        } while_data;

        /* REPEAT */
        struct Repeat
        {
            ListNode_t *body_list;
            struct Expression *until_expr;
        } repeat_data;

        /* FOR */
        struct For
        {
            enum StmtType for_assign_type;
            struct Expression *to;
            struct Statement *do_for;
            int is_downto;  /* 0 for TO, 1 for DOWNTO */

            union for_assign
            {
                struct Statement *var_assign;
                struct Expression *var; /* Grammar will validate the correctness */
            } for_assign_data;
        } for_data;

        /* FOR-IN */
        struct ForIn
        {
            struct Expression *loop_var;     /* Loop variable (identifier expression) */
            struct Expression *collection;   /* Collection to iterate over (array, set, etc.) */
            struct Statement *do_stmt;       /* Body statement */
        } for_in_data;

        /* CASE */
        struct Case
        {
            struct Expression *selector_expr;
            ListNode_t *branches;  /* List of CaseBranch */
            struct Statement *else_stmt;  /* Optional else branch */
        } case_data;

        /* WITH */
        struct With
        {
            struct Expression *context_expr;
            struct Statement *body_stmt;
        } with_data;

        /* TRY..FINALLY */
        struct TryFinally
        {
            ListNode_t *try_statements;   /* List of Statement */
            ListNode_t *finally_statements; /* List of Statement */
        } try_finally_data;

        /* TRY..EXCEPT */
        struct TryExcept
        {
            ListNode_t *try_statements;   /* List of Statement */
            ListNode_t *except_statements; /* List of Statement */
            char *exception_var_name;     /* Variable name from 'on E: Exception do' (optional) */
            char *exception_type_name;    /* Type name from 'on E: Exception do' (optional) */
            int has_on_clause;            /* 1 if 'on' clause present, 0 otherwise */
        } try_except_data;

        /* RAISE */
        struct Raise
        {
            struct Expression *exception_expr; /* Optional */
        } raise_data;

        /* INHERITED */
        struct Inherited
        {
            struct Expression *call_expr; /* Optional */
        } inherited_data;

        /* EXIT with optional return value (FPC extension) */
        struct Exit
        {
            struct Expression *return_expr; /* Optional return value for functions */
        } exit_data;
    } stmt_data;
};

/* Expression types */
enum ExprType {
    EXPR_RELOP,
    EXPR_SIGN_TERM,
    EXPR_ADDOP,
    EXPR_MULOP,
    EXPR_VAR_ID,
    EXPR_ARRAY_ACCESS,
    EXPR_RECORD_ACCESS,
    EXPR_FUNCTION_CALL,
    EXPR_INUM,
    EXPR_RNUM,
    EXPR_STRING,
    EXPR_CHAR_CODE,
    EXPR_BOOL,
    EXPR_NIL,
    EXPR_SET,
    EXPR_ARRAY_LITERAL,
    EXPR_RECORD_CONSTRUCTOR,
    EXPR_POINTER_DEREF,
    EXPR_ADDR,
    EXPR_TYPECAST,
    EXPR_IS,
    EXPR_AS,
    EXPR_ADDR_OF_PROC,
    EXPR_ANONYMOUS_FUNCTION,
    EXPR_ANONYMOUS_PROCEDURE
};

/* An expression subtree */
struct Expression
{
    int line_num;
    int col_num;
    struct RecordType *record_type; /* MOVED HERE */
    enum ExprType type;
    union expr_data
    {
        /* Relational expression */
        struct Relop
        {
            int type;

            /* NOTE: right subtree will be NULL when type is NOT or PAREN */
            struct Expression *left;
            struct Expression *right;
        } relop_data;

        /* Sign term */
        struct Expression *sign_term;

        /* ADDOP */
        struct Addop
        {
            int addop_type;

            struct Expression *left_expr;
            struct Expression *right_term;
        } addop_data;

        /* MULOP */
        struct Mulop
        {
            int mulop_type;

            struct Expression *left_term;
            struct Expression *right_factor;
        } mulop_data;

        /* A variable ID */
        char *id;

        /* An indexed array */
        struct ArrayAccess
        {
            struct Expression *array_expr;
            struct Expression *index_expr;
        } array_access_data;

        /* Record field access */
        struct RecordAccess
        {
            struct Expression *record_expr;
            char *field_id;
            long long field_offset;
        } record_access_data;

        /* Function call */
        struct FunctionCall
        {
            char *id;
            char *mangled_id;
            ListNode_t *args_expr;
            struct HashNode *resolved_func;  /* DEPRECATED: may be invalid after PopScope. */
            
            /* Cached information copied during semantic checking so codegen
             * no longer depends on HashNode lifetime. */
            int call_hash_type;              /* HashType enum value */
            struct KgpcType *call_kgpc_type;   /* Procedure/function signature */
            int is_call_info_valid;          /* 1 if cached info is usable */
            
            /* Support for calling through procedural variables */
            int is_procedural_var_call;      /* 1 if calling through a procedural variable */
            struct HashNode *procedural_var_symbol;  /* Symbol for the procedural variable */
            struct Expression *procedural_var_expr;  /* Expression yielding a function pointer (for record fields, etc.) */
            int is_method_call_placeholder;          /* 1 if created from member access and needs method resolution */
            int is_virtual_call;                     /* 1 if this is a virtual method call (needs VMT dispatch) */
            int vmt_index;                           /* VMT index for virtual calls (-1 if not set) */
            char *self_class_name;                   /* Class name for VMT lookup in virtual calls */
        } function_call_data;

        /* Integer number */
        long long i_num;

        /* Real number */
        float r_num;

        /* String literal */
        char *string;

        /* Character code literal */
        unsigned int char_code;

        /* Boolean literal */
        int bool_value;

        /* Set literal represented as a bitmask */
        struct SetLiteral
        {
            unsigned int bitmask;
            ListNode_t *elements;
            int is_constant;
        } set_data;

        struct ArrayLiteral
        {
            ListNode_t *elements; /* LIST_EXPR nodes pointing to Expression* */
            int element_count;
            int elements_semchecked;
        } array_literal_data;

        struct RecordConstructor
        {
            ListNode_t *fields; /* LIST_UNSPECIFIED nodes pointing to RecordConstructorField* */
            int field_count;
            int fields_semchecked;
        } record_constructor_data;

        /* Pointer dereference */
        struct PointerDeref
        {
            struct Expression *pointer_expr;
        } pointer_deref_data;

        /* Address-of operator */
        struct AddressOf
        {
            struct Expression *expr;
        } addr_data;

        /* Type cast */
        struct TypeCast
        {
            int target_type;
            char *target_type_id;
            struct Expression *expr;
        } typecast_data;

        /* RTTI is operator */
        struct IsExpr
        {
            struct Expression *expr;
            int target_type;
            char *target_type_id;
            struct RecordType *target_record_type;
        } is_data;

        /* as operator (checked class cast) */
        struct AsExpr
        {
            struct Expression *expr;
            int target_type;
            char *target_type_id;
            struct RecordType *target_record_type;
        } as_data;

        /* Address of procedure */
        struct AddrOfProc
        {
            struct HashNode *procedure_symbol;
        } addr_of_proc_data;

        /* Anonymous function/procedure */
        struct AnonymousMethod
        {
            char *generated_name;           /* Auto-generated unique name */
            ListNode_t *parameters;         /* Parameter list */
            int return_type;                /* Return type (for functions, -1 for procedures) */
            char *return_type_id;           /* Return type identifier */
            struct Statement *body;         /* Body statement */
            int is_function;                /* 1 for function, 0 for procedure */
        } anonymous_method_data;
    } expr_data;
    struct Expression *field_width;
    struct Expression *field_precision;
    int resolved_type;
    
    /* NEW: Unified type system - resolved KgpcType for this expression */
    struct KgpcType *resolved_kgpc_type;
    
    int pointer_subtype;
    char *pointer_subtype_id;
    /* struct RecordType *record_type; MOVED */
    int is_pointer_diff; /* Flag for pointer-pointer subtraction */
    int is_array_expr;
    int array_element_type;
    char *array_element_type_id;
    int array_lower_bound;
    int array_upper_bound;
    int array_element_size;
    int array_is_dynamic;
    struct RecordType *array_element_record_type;
    int is_default_initializer;
};

struct SetElement
{
    struct Expression *lower;
    struct Expression *upper;
};

struct RecordConstructorField
{
    char *field_id;
    struct Expression *value;
    long long field_offset;
    int field_type;
    char *field_type_id;
    struct RecordType *field_record_type;
    int field_is_array;
    int array_start;
    int array_end;
    int array_element_type;
    char *array_element_type_id;
    int array_is_open;
    struct RecordType *array_element_record_type;
};


#endif
