#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include "NameMangling.h"
#include "../ParseTree/tree.h"
#include "../List/List.h"
#include "../ParseTree/type_tags.h"
#include "SemChecks/SemCheck_expr.h"
#include "SemCheck.h"
#include "SymTab/SymTab.h"
#include "../ParseTree/KgpcType.h"

// Helper to create a lowercase copy of a string (for case-insensitive mangling)
static char* str_tolower_dup(const char* src) {
    if (src == NULL)
        return NULL;
    
    size_t len = strlen(src);
    char* dst = (char*)malloc(len + 1);
    if (dst == NULL)
        return NULL;
    
    for (size_t i = 0; i < len; ++i)
        dst[i] = (char)tolower((unsigned char)src[i]);
    
    dst[len] = '\0';
    return dst;
}

typedef struct MangleType {
    int kind;
    char *type_id;
} MangleType;

static MangleType *create_mangle_type(int kind, const char *type_id)
{
    MangleType *mt = (MangleType *)malloc(sizeof(MangleType));
    if (mt == NULL)
        return NULL;
    mt->kind = kind;
    mt->type_id = type_id != NULL ? strdup(type_id) : NULL;
    return mt;
}

static char *sanitize_type_id(const char *type_id)
{
    if (type_id == NULL)
        return NULL;
    size_t len = strlen(type_id);
    char *out = (char *)malloc(len + 1);
    if (out == NULL)
        return NULL;
    for (size_t i = 0; i < len; ++i)
    {
        unsigned char c = (unsigned char)type_id[i];
        if (isalnum(c) || c == '_')
            out[i] = (char)tolower(c);
        else
            out[i] = '_';
    }
    out[len] = '\0';
    return out;
}

// Helper to free a list of mangle types
static void DestroyMangleTypeList(ListNode_t* list) {
    assert(list != NULL);
    ListNode_t* cur = list;
    while (cur != NULL) {
        MangleType *mt = (MangleType *)cur->cur;
        if (mt != NULL)
        {
            free(mt->type_id);
            free(mt);
        }
        ListNode_t* next = cur->next;
        free(cur);
        cur = next;
    }
}

static enum VarType ConvertParserTypeToVarType(int parser_type)
{
    switch(parser_type)
    {
        case INT_TYPE:
            return HASHVAR_INTEGER;
        case LONGINT_TYPE:
            return HASHVAR_LONGINT;
        case INT64_TYPE:
            return HASHVAR_INT64;
        case BYTE_TYPE:
        case WORD_TYPE:
            return HASHVAR_INTEGER;
        case LONGWORD_TYPE:
            return HASHVAR_LONGINT;
        case QWORD_TYPE:
            return HASHVAR_INT64;
        case REAL_TYPE:
            return HASHVAR_REAL;
        case STRING_TYPE:
            return HASHVAR_PCHAR;
        case SHORTSTRING_TYPE:
            return HASHVAR_SHORTSTRING;
        case BOOL:
            return HASHVAR_BOOLEAN;
        case CHAR_TYPE:
            return HASHVAR_CHAR;
        case POINTER_TYPE:
            return HASHVAR_POINTER;
        case SET_TYPE:
            return HASHVAR_SET;
        case ENUM_TYPE:
            return HASHVAR_ENUM;
        case FILE_TYPE:
            return HASHVAR_FILE;
        case TEXT_TYPE:
            return HASHVAR_TEXT;
        case ARRAY_OF_CONST_TYPE:
            return HASHVAR_ARRAY;
        default:
            return HASHVAR_UNTYPED;
    }
}

// Helper to get VarType from a type HashNode, preferring KgpcType
static enum VarType GetVarTypeFromTypeNode(HashNode_t* type_node) {
    if (type_node == NULL)
        return HASHVAR_UNTYPED;
    
    if (type_node->id != NULL && strcasecmp(type_node->id, "text") == 0)
        return HASHVAR_TEXT;

    // If KgpcType is available, extract VarType from it
    if (type_node->type != NULL) {
        if (kgpc_type_is_shortstring(type_node->type))
            return HASHVAR_SHORTSTRING;
        if (type_node->type->kind == TYPE_KIND_PRIMITIVE) {
            int tag = kgpc_type_get_primitive_tag(type_node->type);
            return ConvertParserTypeToVarType(tag);
        } else if (type_node->type->kind == TYPE_KIND_POINTER) {
            return HASHVAR_POINTER;
        } else if (type_node->type->kind == TYPE_KIND_RECORD) {
            return HASHVAR_RECORD;
        } else if (type_node->type->kind == TYPE_KIND_ARRAY) {
            return HASHVAR_ARRAY;
        } else if (type_node->type->kind == TYPE_KIND_PROCEDURE) {
            return HASHVAR_PROCEDURE;
        }
    }
    
    return HASHVAR_UNTYPED;
}

// Helper to map built-in type names to VarType (case-insensitive)
static enum VarType MapBuiltinTypeNameToVarType(const char *type_name) {
    if (type_name == NULL)
        return HASHVAR_UNTYPED;
    
    // Character types
    if (strcasecmp(type_name, "Char") == 0 || strcasecmp(type_name, "AnsiChar") == 0)
        return HASHVAR_CHAR;
    
    // String types
    if (strcasecmp(type_name, "String") == 0 || strcasecmp(type_name, "AnsiString") == 0 ||
        strcasecmp(type_name, "WideString") == 0)
        return HASHVAR_PCHAR;
    if (strcasecmp(type_name, "ShortString") == 0)
        return HASHVAR_SHORTSTRING;
    if (strcasecmp(type_name, "RawByteString") == 0)
        return HASHVAR_RAWBYTESTRING;
    if (strcasecmp(type_name, "UnicodeString") == 0)
        return HASHVAR_UNICODESTRING;
    
    // Integer types
    if (strcasecmp(type_name, "Integer") == 0 || strcasecmp(type_name, "Byte") == 0 ||
        strcasecmp(type_name, "Word") == 0 || strcasecmp(type_name, "TSystemCodePage") == 0)
        return HASHVAR_INTEGER;
    
    if (strcasecmp(type_name, "LongInt") == 0)
        return HASHVAR_LONGINT;
    
    if (strcasecmp(type_name, "Int64") == 0 ||
        strcasecmp(type_name, "QWord") == 0 || strcasecmp(type_name, "SizeInt") == 0 ||
        strcasecmp(type_name, "SizeUInt") == 0)
        return HASHVAR_INT64;
    
    // Real types
    if (strcasecmp(type_name, "Real") == 0 || strcasecmp(type_name, "Double") == 0)
        return HASHVAR_REAL;
    
    // Boolean type
    if (strcasecmp(type_name, "Boolean") == 0)
        return HASHVAR_BOOLEAN;

    if (strcasecmp(type_name, "CodePointer") == 0)
        return HASHVAR_POINTER;
    
    // File types
    if (strcasecmp(type_name, "Text") == 0)
        return HASHVAR_TEXT;
    if (strcasecmp(type_name, "File") == 0)
        return HASHVAR_FILE;
    
    // Pointer type
    if (strcasecmp(type_name, "Pointer") == 0)
        return HASHVAR_POINTER;
    
    // PAnsiChar/PChar types (distinct from String for name mangling)
    if (strcasecmp(type_name, "PChar") == 0 || strcasecmp(type_name, "PAnsiChar") == 0)
        return HASHVAR_PANSICHAR;
    if (strcasecmp(type_name, "PWideChar") == 0 || strcasecmp(type_name, "PWChar") == 0)
        return HASHVAR_PWIDECHAR;
    
    return HASHVAR_UNTYPED;
}

static HashNode_t *find_type_node_for_mangling(SymTab_t *symtab, const char *type_id)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    HashNode_t *type_node = NULL;
    if (FindIdent(&type_node, symtab, type_id) >= 0 && type_node != NULL)
    {
        if (type_node->hash_type == HASHTYPE_TYPE)
            return type_node;
    }

    ListNode_t *matches = FindAllIdents(symtab, type_id);
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate != NULL && candidate->hash_type == HASHTYPE_TYPE)
        {
            DestroyList(matches);
            return candidate;
        }
        cur = cur->next;
    }
    DestroyList(matches);

    const char *dot = strrchr(type_id, '.');
    if (dot != NULL && dot[1] != '\0')
        return find_type_node_for_mangling(symtab, dot + 1);

    return NULL;
}

// Helper function to flatten argument lists into a list of HASHVAR_ types.
static ListNode_t* GetFlatTypeListForMangling(ListNode_t *args, SymTab_t *symtab) { // <-- Add symtab
    assert(symtab != NULL);
    if (args == NULL) {
        return NULL;
    }

    ListNode_t* type_list = NULL;
    ListNode_t* arg_cur = args;
    while (arg_cur != NULL) {
        Tree_t* decl_tree = (Tree_t*)arg_cur->cur;
        enum VarType resolved_type = HASHVAR_UNTYPED; // Default to untyped
        const char *record_type_id = NULL;
        ListNode_t* ids;

        if (decl_tree->type == TREE_VAR_DECL) {
            ids = decl_tree->tree_data.var_decl_data.ids;
            // --- THIS IS THE CORE LOGIC ---
            struct TypeAlias *inline_alias = decl_tree->tree_data.var_decl_data.inline_type_alias;
            if (inline_alias != NULL && inline_alias->is_array)
            {
                int element_type = inline_alias->array_element_type;
                const char *element_type_id = inline_alias->array_element_type_id;

                if (element_type == ARRAY_OF_CONST_TYPE ||
                    (element_type_id != NULL && strcasecmp(element_type_id, "const") == 0))
                {
                    resolved_type = HASHVAR_ARRAY;
                }
                else if (element_type == UNKNOWN_TYPE && element_type_id != NULL)
                {
                    resolved_type = MapBuiltinTypeNameToVarType(element_type_id);
                    if (resolved_type == HASHVAR_UNTYPED)
                    {
                        HashNode_t *type_node = find_type_node_for_mangling(symtab, element_type_id);
                        if (type_node != NULL)
                            resolved_type = GetVarTypeFromTypeNode(type_node);
                    }
                    if (resolved_type != HASHVAR_UNTYPED)
                        resolved_type = resolved_type + 100;
                    else
                        resolved_type = HASHVAR_ARRAY;
                }
                else if (element_type != UNKNOWN_TYPE)
                {
                    resolved_type = ConvertParserTypeToVarType(element_type);
                    if (resolved_type != HASHVAR_UNTYPED)
                        resolved_type = resolved_type + 100;
                    else
                        resolved_type = HASHVAR_ARRAY;
                }
                else
                {
                    resolved_type = HASHVAR_ARRAY;
                }
            }

            if (resolved_type == HASHVAR_UNTYPED &&
                decl_tree->tree_data.var_decl_data.type_id != NULL) {
                const char *type_id = decl_tree->tree_data.var_decl_data.type_id;
                
                // First try to map built-in type names directly
                resolved_type = MapBuiltinTypeNameToVarType(type_id);
                
                // If not a built-in type, look it up in the symbol table
                if (resolved_type == HASHVAR_UNTYPED) {
                    HashNode_t* type_node = find_type_node_for_mangling(symtab, type_id);
                    if (type_node != NULL) {
                        resolved_type = GetVarTypeFromTypeNode(type_node);
                        if (resolved_type == HASHVAR_RECORD &&
                            type_node->type != NULL &&
                            type_node->type->kind == TYPE_KIND_RECORD &&
                            type_node->type->info.record_info != NULL &&
                            type_node->type->info.record_info->type_id != NULL)
                        {
                            record_type_id = type_node->type->info.record_info->type_id;
                        }
                        /* For class/interface types (pointer to record), include the
                         * record type_id so overloads with different class params
                         * get distinct mangled names. Skip for "Self" parameters
                         * since methods are already distinguished by ClassName__ prefix
                         * and including Self's class type breaks inherited calls. */
                        {
                            int is_self_param = 0;
                            if (ids != NULL && ids->cur != NULL)
                                is_self_param = (strcasecmp((const char *)ids->cur, "Self") == 0);
                            if (!is_self_param &&
                                resolved_type == HASHVAR_POINTER &&
                                type_node->type != NULL &&
                                type_node->type->kind == TYPE_KIND_POINTER &&
                                type_node->type->info.points_to != NULL &&
                                type_node->type->info.points_to->kind == TYPE_KIND_RECORD &&
                                type_node->type->info.points_to->info.record_info != NULL &&
                                type_node->type->info.points_to->info.record_info->type_id != NULL)
                            {
                                record_type_id = type_node->type->info.points_to->info.record_info->type_id;
                            }
                        }
                    }
                }
            } else {
                if (resolved_type == HASHVAR_UNTYPED)
                {
                    // It's a built-in type, convert from parser token to semantic type
                    resolved_type = ConvertParserTypeToVarType(decl_tree->tree_data.var_decl_data.type);
                }
            }

            if (resolved_type == HASHVAR_RECORD && record_type_id == NULL &&
                decl_tree->tree_data.var_decl_data.inline_record_type != NULL &&
                decl_tree->tree_data.var_decl_data.inline_record_type->type_id != NULL)
            {
                record_type_id = decl_tree->tree_data.var_decl_data.inline_record_type->type_id;
            }
        } else { // Assume array or other type for now
            ids = decl_tree->tree_data.arr_decl_data.ids;
            /* For open array parameters, include the element type in mangling
             * to distinguish between array of Char and array of Integer.
             * But array of const uses generic HASHVAR_ARRAY. */
            int element_type = decl_tree->tree_data.arr_decl_data.type;
            const char *element_type_id = decl_tree->tree_data.arr_decl_data.type_id;
            
            /* Special case: array of const uses generic array mangling */
            if (element_type == ARRAY_OF_CONST_TYPE || 
                (element_type_id != NULL && strcasecmp(element_type_id, "const") == 0))
            {
                resolved_type = HASHVAR_ARRAY;
            }
            else if (element_type == UNKNOWN_TYPE && element_type_id != NULL)
            {
                /* Try to map element type from type_id */
                resolved_type = MapBuiltinTypeNameToVarType(element_type_id);
                if (resolved_type == HASHVAR_UNTYPED)
                {
                    /* Look up in symbol table */
                    HashNode_t *type_node = find_type_node_for_mangling(symtab, element_type_id);
                    if (type_node != NULL)
                        resolved_type = GetVarTypeFromTypeNode(type_node);
                }
                /* Prepend 'a' to indicate array with this element type */
                if (resolved_type != HASHVAR_UNTYPED)
                    resolved_type = resolved_type + 100;  /* Use 100+ range for array element types */
                else
                    resolved_type = HASHVAR_ARRAY;
            }
            else if (element_type != UNKNOWN_TYPE)
            {
                /* Use ConvertParserTypeToVarType and add 100 to indicate array */
                resolved_type = ConvertParserTypeToVarType(element_type);
                if (resolved_type != HASHVAR_UNTYPED)
                    resolved_type = resolved_type + 100;  /* Use 100+ range for array element types */
                else
                    resolved_type = HASHVAR_ARRAY;
            }
            else
            {
                resolved_type = HASHVAR_ARRAY; /* Fallback for unknown element type */
            }
        }

        ListNode_t* id_cur = ids;
        while (id_cur != NULL) {
            MangleType *mt = create_mangle_type(resolved_type, record_type_id);
            assert(mt != NULL);
            if (type_list == NULL) {
                type_list = CreateListNode(mt, LIST_UNSPECIFIED);
            } else {
                PushListNodeBack(type_list, CreateListNode(mt, LIST_UNSPECIFIED));
            }
            id_cur = id_cur->next;
        }
        arg_cur = arg_cur->next;
    }
    return type_list;
}

// Core mangling function
static char* MangleNameFromTypeList(const char* original_name, ListNode_t* type_list) {
    assert(original_name != NULL);
    
    // Normalize function name to lowercase for case-insensitive matching
    // (Pascal is case-insensitive, so Lowercase and LowerCase should produce the same mangled name)
    char* lower_name = str_tolower_dup(original_name);
    if (lower_name == NULL) {
        lower_name = strdup(original_name); // fallback
    }
    
    if (type_list == NULL) {
        // No args, append _void
        const char* suffix = "_void";
        char* mangled_name = malloc(strlen(lower_name) + strlen(suffix) + 1);
        assert(mangled_name != NULL);
        sprintf(mangled_name, "%s%s", lower_name, suffix);
        free(lower_name);
        return mangled_name;
    }

    // Calculate length - max suffix is "_ai64" (5 chars), use 6 for safety
    size_t total_len = strlen(lower_name);
    ListNode_t* cur = type_list;
    while (cur != NULL) {
        MangleType *mt = (MangleType *)cur->cur;
        if (mt != NULL && (mt->kind == HASHVAR_RECORD || mt->kind == HASHVAR_POINTER) && mt->type_id != NULL)
        {
            char *sanitized = sanitize_type_id(mt->type_id);
            size_t extra = sanitized != NULL ? strlen(sanitized) : 0;
            if (sanitized != NULL)
                free(sanitized);
            total_len += 3 + extra; // "_u_" + type id
        }
        else
        {
            total_len += 6; // Max length of a type suffix (e.g., "_ai64" is 5 chars)
        }
        cur = cur->next;
    }
    total_len += 1; // Null terminator

    char* mangled_name = malloc(total_len);
    assert(mangled_name != NULL);
    strcpy(mangled_name, lower_name);
    free(lower_name);

    cur = type_list;
    while (cur != NULL) {
        MangleType *mt = (MangleType *)cur->cur;
        int type = mt != NULL ? mt->kind : HASHVAR_UNTYPED;
        const char* type_suffix = NULL;
        char *type_suffix_dynamic = NULL;
        /* Handle array element types (100+ range) */
        if (type >= 100) {
            int elem_type = type - 100;
            switch (elem_type) {
                case HASHVAR_INTEGER: type_suffix = "_ai"; break;   /* array of Integer */
                case HASHVAR_LONGINT: type_suffix = "_ali"; break;  /* array of LongInt */
                case HASHVAR_INT64:   type_suffix = "_ai64"; break; /* array of Int64 */
                case HASHVAR_REAL:    type_suffix = "_ar"; break;   /* array of Real */
                case HASHVAR_PCHAR:   type_suffix = "_as"; break;   /* array of String */
                case HASHVAR_SHORTSTRING: type_suffix = "_ass"; break; /* array of ShortString */
                case HASHVAR_BOOLEAN: type_suffix = "_ab"; break;   /* array of Boolean */
                case HASHVAR_CHAR:    type_suffix = "_ac"; break;   /* array of Char */
                case HASHVAR_POINTER: type_suffix = "_ap"; break;   /* array of Pointer */
                case HASHVAR_RECORD:  type_suffix = "_au"; break;   /* array of Record */
                default:              type_suffix = "_a"; break;    /* array of unknown */
            }
        } else if ((type == HASHVAR_RECORD || type == HASHVAR_POINTER) && mt != NULL && mt->type_id != NULL) {
            char *sanitized = sanitize_type_id(mt->type_id);
            if (sanitized != NULL)
            {
                size_t len = strlen(sanitized) + 4;
                type_suffix_dynamic = (char *)malloc(len);
                if (type_suffix_dynamic != NULL)
                    snprintf(type_suffix_dynamic, len, "_u_%s", sanitized);
                free(sanitized);
            }
            if (type_suffix_dynamic == NULL)
                type_suffix = "_u";
        } else {
            switch (type) {
                case HASHVAR_INTEGER: type_suffix = "_i"; break;
                case HASHVAR_LONGINT: type_suffix = "_li"; break;
                case HASHVAR_INT64:   type_suffix = "_i64"; break;
                case HASHVAR_REAL:    type_suffix = "_r"; break;
                case HASHVAR_PCHAR:   type_suffix = "_s"; break; // For String (keep backwards compat)
                case HASHVAR_SHORTSTRING: type_suffix = "_ss"; break; // ShortString
                case HASHVAR_PANSICHAR: type_suffix = "_pc"; break; // For PAnsiChar/PChar
                case HASHVAR_PWIDECHAR: type_suffix = "_pw"; break; // For PWideChar
                case HASHVAR_BOOLEAN: type_suffix = "_b"; break;
                case HASHVAR_CHAR:    type_suffix = "_c"; break;
                case HASHVAR_POINTER: type_suffix = "_p"; break;
                case HASHVAR_SET:     type_suffix = "_set"; break;
                case HASHVAR_ENUM:    type_suffix = "_e"; break;
                case HASHVAR_FILE:    type_suffix = "_f"; break;
                case HASHVAR_TEXT:    type_suffix = "_t"; break; // For text files
                case HASHVAR_RECORD:  type_suffix = "_u"; break; // Record types treated as unknown for mangling
                case HASHVAR_ARRAY:   type_suffix = "_a"; break; // Array
                case HASHVAR_RAWBYTESTRING: type_suffix = "_rbs"; break; // RawByteString
                case HASHVAR_UNICODESTRING: type_suffix = "_us"; break;  // UnicodeString
                default:              type_suffix = "_u"; break; // Unknown/unsupported
            }
        }
        if (type_suffix_dynamic != NULL) {
            strcat(mangled_name, type_suffix_dynamic);
            free(type_suffix_dynamic);
        } else if (type_suffix != NULL) {
            strcat(mangled_name, type_suffix);
        }
        cur = cur->next;
    }

    DestroyMangleTypeList(type_list);
    return mangled_name;
}

// Update the main function signature and the call
char* MangleFunctionName(const char* original_name, ListNode_t* args, SymTab_t* symtab) { // <-- Add symtab
    assert(original_name != NULL);
    assert(symtab != NULL);
    ListNode_t* type_list = GetFlatTypeListForMangling(args, symtab); // <-- Pass symtab
    return MangleNameFromTypeList(original_name, type_list);
}

// Helper function to get a flat list of types from a function call's arguments
static ListNode_t* GetFlatTypeListFromCallSite(ListNode_t *args_expr, SymTab_t *symtab, int max_scope_lev) {
    assert(symtab != NULL);
    if (args_expr == NULL) {
        return NULL;
    }

    ListNode_t* type_list = NULL;
    ListNode_t* arg_cur = args_expr;
    while (arg_cur != NULL) {
        struct Expression *arg_expr = (struct Expression *)arg_cur->cur;
        enum VarType resolved_type = HASHVAR_UNTYPED;
        const char *record_type_id = NULL;
        if (arg_expr != NULL && arg_expr->type == EXPR_RECORD_CONSTRUCTOR)
        {
            resolved_type = HASHVAR_RECORD;
            if (arg_expr->record_type != NULL && arg_expr->record_type->type_id != NULL)
                record_type_id = arg_expr->record_type->type_id;
        }
        else
        {
            KgpcType *arg_type = NULL;
            semcheck_expr_main(symtab, arg_expr, max_scope_lev, NO_MUTATE, &arg_type);
            int type_tag = arg_type != NULL ? semcheck_tag_from_kgpc(arg_type) : UNKNOWN_TYPE;
            resolved_type = ConvertParserTypeToVarType(type_tag);
            if (arg_expr != NULL && arg_expr->resolved_kgpc_type != NULL)
            {
                KgpcType *kgpc_type = arg_expr->resolved_kgpc_type;
                if (kgpc_type_is_array(kgpc_type) || kgpc_type_is_array_of_const(kgpc_type))
                {
                    /* For arrays, include element type in mangling (100+ range) */
                    KgpcType *elem_type = kgpc_type_is_array(kgpc_type) ?
                        kgpc_type->info.array_info.element_type : NULL;
                    if (elem_type != NULL && elem_type->kind == TYPE_KIND_PRIMITIVE)
                    {
                        int elem_tag = elem_type->info.primitive_type_tag;
                        enum VarType elem_var = ConvertParserTypeToVarType(elem_tag);
                        if (elem_var != HASHVAR_UNTYPED)
                            resolved_type = elem_var + 100;  /* Use 100+ range for array element types */
                        else
                            resolved_type = HASHVAR_ARRAY;
                    }
                    else
                    {
                        resolved_type = HASHVAR_ARRAY;
                    }
                }
                else if (kgpc_type->kind == TYPE_KIND_RECORD)
                {
                    resolved_type = HASHVAR_RECORD;
                    if (kgpc_type->info.record_info != NULL &&
                        kgpc_type->info.record_info->type_id != NULL)
                        record_type_id = kgpc_type->info.record_info->type_id;
                }
                else if (kgpc_type->kind == TYPE_KIND_POINTER)
                {
                    resolved_type = HASHVAR_POINTER;
                    /* For class/interface types (pointer to record), include the
                     * record type_id in mangling to distinguish overloads like
                     * Describe(TAnimal) vs Describe(TCat).
                     * Skip for "Self" arguments since methods are distinguished
                     * by ClassName__ prefix and including Self's class type
                     * breaks inherited calls. */
                    int is_self_arg = 0;
                    if (arg_expr != NULL && arg_expr->type == EXPR_VAR_ID &&
                        arg_expr->expr_data.id != NULL &&
                        strcasecmp(arg_expr->expr_data.id, "Self") == 0)
                        is_self_arg = 1;
                    if (!is_self_arg)
                    {
                        KgpcType *points_to = kgpc_type->info.points_to;
                        if (points_to != NULL && points_to->kind == TYPE_KIND_RECORD &&
                            points_to->info.record_info != NULL &&
                            points_to->info.record_info->type_id != NULL)
                            record_type_id = points_to->info.record_info->type_id;
                    }
                }
                else if (kgpc_type->kind == TYPE_KIND_PROCEDURE)
                    resolved_type = HASHVAR_PROCEDURE;
                /* Check type_alias for STRING_TYPE to distinguish between
                 * RawByteString and UnicodeString. With the fix in commit 868406b,
                 * type_alias is now owned by KgpcType and should be valid. */
                else if (type_tag == STRING_TYPE && kgpc_type->type_alias != NULL)
                {
                    struct TypeAlias *alias = kgpc_type->type_alias;
                    if (alias->alias_name != NULL)
                    {
                        if (strcasecmp(alias->alias_name, "RawByteString") == 0)
                            resolved_type = HASHVAR_RAWBYTESTRING;
                        else if (strcasecmp(alias->alias_name, "UnicodeString") == 0)
                            resolved_type = HASHVAR_UNICODESTRING;
                    }
                }
            }
            /* Also handle array literals that don't have resolved_kgpc_type yet */
            else if (arg_expr != NULL && (arg_expr->type == EXPR_ARRAY_LITERAL || arg_expr->type == EXPR_SET))
            {
                /* Get element type from first element of array literal */
                ListNode_t *first_elem = NULL;
                if (arg_expr->type == EXPR_ARRAY_LITERAL)
                    first_elem = arg_expr->expr_data.array_literal_data.elements;
                else if (arg_expr->type == EXPR_SET)
                    first_elem = arg_expr->expr_data.set_data.elements;
                
                if (first_elem != NULL && first_elem->cur != NULL)
                {
                    struct Expression *elem_expr = (struct Expression *)first_elem->cur;
                    /* Infer element type directly from expression type without calling semcheck_expr_main
                     * since the expression may not be fully ready for semantic checking yet */
                    enum VarType elem_var = HASHVAR_UNTYPED;
                    if (elem_expr->type == EXPR_INUM)
                        elem_var = HASHVAR_INTEGER;
                    else if (elem_expr->type == EXPR_CHAR_CODE)
                        elem_var = HASHVAR_CHAR;
                    else if (elem_expr->type == EXPR_STRING)
                        elem_var = HASHVAR_PCHAR;
                    else if (elem_expr->type == EXPR_BOOL)
                        elem_var = HASHVAR_BOOLEAN;
                    else if (elem_expr->type == EXPR_RNUM)
                        elem_var = HASHVAR_REAL;
                    
                    if (elem_var != HASHVAR_UNTYPED)
                        resolved_type = elem_var + 100;  /* Use 100+ range for array element types */
                    else
                        resolved_type = HASHVAR_ARRAY;
                }
                else
                {
                    resolved_type = HASHVAR_ARRAY;
                }
            }
        }

        MangleType *mt = create_mangle_type(resolved_type, record_type_id);
        assert(mt != NULL);
        if (type_list == NULL) {
            type_list = CreateListNode(mt, LIST_UNSPECIFIED);
        } else {
            PushListNodeBack(type_list, CreateListNode(mt, LIST_UNSPECIFIED));
        }
        arg_cur = arg_cur->next;
    }
    return type_list;
}

char* MangleFunctionNameFromCallSite(const char* original_name, ListNode_t* args_expr, SymTab_t *symtab, int max_scope_lev) {
    assert(original_name != NULL);
    assert(symtab != NULL);
    ListNode_t* type_list = GetFlatTypeListFromCallSite(args_expr, symtab, max_scope_lev);
    return MangleNameFromTypeList(original_name, type_list);
}
