/*
    SemCheck_Expr_Resolve.c - Type resolution helpers

    This file contains shared helpers for resolving type identifiers
    and mapping builtin type names.

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"

const char *semcheck_base_type_name(const char *id)
{
    if (id == NULL)
        return NULL;
    const char *dot = strrchr(id, '.');
    return (dot != NULL && dot[1] != '\0') ? (dot + 1) : id;
}

const char *semcheck_type_tag_name(int type_tag)
{
    switch (type_tag)
    {
        case INT_TYPE: return "int";
        case LONGINT_TYPE: return "longint";
        case INT64_TYPE: return "int64";
        case REAL_TYPE: return "real";
        case BOOL: return "bool";
        case CHAR_TYPE: return "char";
        case STRING_TYPE: return "string";
        case SHORTSTRING_TYPE: return "shortstring";
        case POINTER_TYPE: return "pointer";
        case SET_TYPE: return "set";
        case RECORD_TYPE: return "record";
        case PROCEDURE: return "procedure";
        case ENUM_TYPE: return "enum";
        case FILE_TYPE: return "file";
        default: return "unknown";
    }
}

int semcheck_map_builtin_type_name(SymTab_t *symtab, const char *id)
{
    if (id == NULL)
        return UNKNOWN_TYPE;

    /* Prefer looking up the identifier in the current symbol table so aliases
     * (e.g., SizeInt, NativeInt) use their declared storage sizes. */
    if (symtab != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, id) == 0 &&
            type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            int mapped = UNKNOWN_TYPE;
            set_type_from_hashtype(&mapped, type_node);
            if (mapped != UNKNOWN_TYPE)
                return mapped;
        }
    }

    /* Minimal fallback for core primitives */
    if (pascal_identifier_equals(id, "Integer"))
        return INT_TYPE;
    if (pascal_identifier_equals(id, "LongInt"))
        return LONGINT_TYPE;
    if (pascal_identifier_equals(id, "Real") || pascal_identifier_equals(id, "Double"))
        return REAL_TYPE;
    if (pascal_identifier_equals(id, "String") ||
        pascal_identifier_equals(id, "AnsiString") ||
        pascal_identifier_equals(id, "RawByteString") ||
        pascal_identifier_equals(id, "UnicodeString") ||
        pascal_identifier_equals(id, "WideString"))
        return STRING_TYPE;
    if (pascal_identifier_equals(id, "ShortString"))
        return SHORTSTRING_TYPE;
    if (pascal_identifier_equals(id, "Char") ||
        pascal_identifier_equals(id, "AnsiChar") ||
        pascal_identifier_equals(id, "WideChar") ||
        pascal_identifier_equals(id, "UnicodeChar"))
        return CHAR_TYPE;
    if (pascal_identifier_equals(id, "Boolean"))
        return BOOL;
    if (pascal_identifier_equals(id, "Pointer"))
        return POINTER_TYPE;
    if (pascal_identifier_equals(id, "CodePointer"))
        return POINTER_TYPE;
    if (pascal_identifier_equals(id, "Byte"))
        return BYTE_TYPE;
    if (pascal_identifier_equals(id, "Word"))
        return WORD_TYPE;
    if (pascal_identifier_equals(id, "LongWord") ||
        pascal_identifier_equals(id, "Cardinal") ||
        pascal_identifier_equals(id, "DWord"))
        return LONGWORD_TYPE;
    if (pascal_identifier_equals(id, "QWord") ||
        pascal_identifier_equals(id, "UInt64"))
        return QWORD_TYPE;

    return UNKNOWN_TYPE;
}

const char *semcheck_normalize_char_type_id(const char *id)
{
    if (id == NULL)
        return NULL;
    if (pascal_identifier_equals(id, "UnicodeChar") ||
        pascal_identifier_equals(id, "WideChar"))
        return "WideChar";
    if (pascal_identifier_equals(id, "Char") ||
        pascal_identifier_equals(id, "AnsiChar"))
        return "AnsiChar";
    return id;
}

int resolve_type_identifier(int *out_type, SymTab_t *symtab,
    const char *type_id, int line_num)
{
    if (type_id == NULL)
        return 0;

    HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_id);
    if (type_node == NULL)
    {
        semcheck_error_with_context("Error on line %d, typecast references unknown type %s!\n\n",
            line_num, type_id);
        return 1;
    }

    if (type_node->hash_type != HASHTYPE_TYPE)
    {
        semcheck_error_with_context("Error on line %d, %s is not a type identifier.\n\n",
            line_num, type_id);
        return 1;
    }

    set_type_from_hashtype(out_type, type_node);

    struct TypeAlias *alias = get_type_alias_from_node(type_node);
    if (alias != NULL)
    {
        if (alias->base_type != UNKNOWN_TYPE)
            *out_type = alias->base_type;

        if (alias->target_type_id != NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindIdent(&target_node, symtab, alias->target_type_id) != -1 &&
                target_node != NULL)
            {
                set_type_from_hashtype(out_type, target_node);
            }
        }

        if (alias->is_pointer)
            *out_type = POINTER_TYPE;
        else if (alias->is_set)
            *out_type = SET_TYPE;
        else if (alias->is_enum)
            *out_type = ENUM_TYPE;
        else if (alias->is_file)
            *out_type = FILE_TYPE;
    }

    if (getenv("KGPC_DEBUG_TYPEID") != NULL &&
        (pascal_identifier_equals(type_id, "qword") ||
         pascal_identifier_equals(type_id, "int64") ||
         pascal_identifier_equals(type_id, "cint64")))
    {
        fprintf(stderr, "[KGPC_DEBUG_TYPEID] type_id=%s resolved=%d\n", type_id, *out_type);
    }

    return 0;
}

int set_type_from_hashtype(int *type, HashNode_t *hash_node)
{
    assert(type != NULL);
    assert(hash_node != NULL);

    /* Try KgpcType first if available */
    if (hash_node->type != NULL)
    {
        if (hash_node->type->type_alias != NULL)
        {
            int base = hash_node->type->type_alias->base_type;
            if (base == STRING_TYPE || base == SHORTSTRING_TYPE)
            {
                *type = base;
                return 0;
            }
            if (hash_node->type->type_alias->is_set)
            {
                *type = SET_TYPE;
                return 0;
            }
            if (hash_node->type->type_alias->is_enum)
            {
                *type = ENUM_TYPE;
                return 0;
            }
            if (hash_node->type->type_alias->is_pointer)
            {
                *type = POINTER_TYPE;
                return 0;
            }
            if (hash_node->type->type_alias->is_file)
            {
                *type = FILE_TYPE;
                return 0;
            }
        }
        if (hash_node->type->kind == TYPE_KIND_PRIMITIVE)
        {
            *type = kgpc_type_get_primitive_tag(hash_node->type);
            return 0;
        }
        else if (hash_node->type->kind == TYPE_KIND_POINTER)
        {
            *type = POINTER_TYPE;
            return 0;
        }
        else if (hash_node->type->kind == TYPE_KIND_RECORD)
        {
            *type = RECORD_TYPE;
            return 0;
        }
        else if (hash_node->type->kind == TYPE_KIND_PROCEDURE)
        {
            /* When this is a variable/const with a procedural type (not a function/procedure definition),
             * we return PROCEDURE to indicate it's a procedure variable.
             * Only for actual function/procedure definitions (HASHTYPE_FUNCTION/HASHTYPE_PROCEDURE)
             * do we return the function's return type. */
            if (hash_node->hash_type == HASHTYPE_FUNCTION || hash_node->hash_type == HASHTYPE_PROCEDURE)
            {
                /* This is an actual function/procedure definition - return its return type */
                KgpcType *return_type = kgpc_type_get_return_type(hash_node->type);
                if (return_type != NULL)
                {
                    if (return_type->kind == TYPE_KIND_PRIMITIVE)
                    {
                        *type = kgpc_type_get_primitive_tag(return_type);
                        return 0;
                    }
                    else if (return_type->kind == TYPE_KIND_RECORD)
                    {
                        *type = RECORD_TYPE;
                        return 0;
                    }
                    else if (return_type->kind == TYPE_KIND_POINTER)
                    {
                        *type = POINTER_TYPE;
                        return 0;
                    }
                    else if (return_type->kind == TYPE_KIND_ARRAY)
                    {
                        KgpcType *elem_type = return_type->info.array_info.element_type;
                        if (elem_type != NULL && elem_type->kind == TYPE_KIND_PRIMITIVE &&
                            elem_type->info.primitive_type_tag == CHAR_TYPE)
                        {
                            *type = SHORTSTRING_TYPE;
                            return 0;
                        }
                        *type = UNKNOWN_TYPE;
                        return 0;
                    }
                    /* Add other return type kinds as needed */
                    *type = UNKNOWN_TYPE;
                    return 0;
                }
            }
            /* For procedure variables/constants or procedures with no return type */
            *type = PROCEDURE;
            return 0;
        }
        else if (hash_node->type->kind == TYPE_KIND_ARRAY)
        {
            /* Check if this is a ShortString (array[0..255] of Char with is_shortstring flag) */
            if (hash_node->type->type_alias != NULL && hash_node->type->type_alias->is_shortstring)
            {
                *type = SHORTSTRING_TYPE;
                return 0;
            }
            
            /* For arrays, return the element type's primitive tag if available */
            KgpcType *elem_type = hash_node->type->info.array_info.element_type;
            if (elem_type != NULL && elem_type->kind == TYPE_KIND_PRIMITIVE)
            {
                *type = kgpc_type_get_primitive_tag(elem_type);
                return 0;
            }
            if (elem_type != NULL && elem_type->kind == TYPE_KIND_POINTER)
            {
                *type = POINTER_TYPE;
                return 0;
            }
            if (elem_type != NULL && elem_type->kind == TYPE_KIND_RECORD)
            {
                *type = RECORD_TYPE;
                return 0;
            }
            *type = UNKNOWN_TYPE;
            return 0;
        }
        else if (hash_node->type->kind == TYPE_KIND_ARRAY_OF_CONST)
        {
            *type = ARRAY_OF_CONST_TYPE;
            return 0;
        }
        *type = UNKNOWN_TYPE;
        return 0;
    }

    *type = UNKNOWN_TYPE;
    return 0;
}
