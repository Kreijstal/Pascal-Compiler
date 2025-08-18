#ifndef TREE_TYPES_H
#define TREE_TYPES_H

#include "List/List.h"

typedef enum {
    TYPE_INTEGER,
    TYPE_REAL,
    TYPE_PCHAR,
    TYPE_BOOLEAN,
    TYPE_STRING,
    TYPE_ARRAY,
    TYPE_ID
} BaseType;

// For %union in Grammar.y
typedef struct IdBisonUnion {
    char *id;
    int line_num;
    int cname_flag;
    int overload_flag;
    ListNode_t *args;
} IdBisonUnion;

// For type declarations
typedef struct Type {
    int base_type;
    int array_start;
    int array_end;
    char* id; // For custom types
} Type_t;

typedef struct TypeDecl {
    char *id;
    Type_t *type;
    struct TypeDecl *next;
} TypeDecl_t;

// For variable declarations
typedef struct VarDecl {
    ListNode_t *id_list;
    Type_t *type;
    struct VarDecl *next;
} VarDecl_t;

// For function/procedure parameters
typedef struct Param {
    ListNode_t *id_list;
    int type;
    int pass_by_ref;
} Param_t;




#endif // TREE_TYPES_H
