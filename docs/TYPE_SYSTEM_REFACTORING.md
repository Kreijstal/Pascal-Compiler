# Type System Refactoring: Migrating from Legacy Type Tags to KgpcType

## Overview

The Kreijstal Gwinn Pascal Compiler (KGPC) is in the process of migrating from a legacy type system based on integer type tags (`INT_TYPE`, `CHAR_TYPE`, etc.) to a first-class type system based on the `KgpcType` structure.

This document describes the refactoring effort, migration patterns, and guidelines for future development.

## Background

### Legacy Type System

The original type system used integer constants defined in `type_tags.h`:

```c
#define UNKNOWN_TYPE    0
#define INT_TYPE        1
#define REAL_TYPE       2
#define CHAR_TYPE       28
#define STRING_TYPE     4
// ... etc
```

These tags were used throughout the semantic checker with patterns like:

```c
int type = UNKNOWN_TYPE;
semcheck_expr_legacy_tag(&type, symtab, expr, scope, mutating);
if (type == INT_TYPE || type == LONGINT_TYPE) {
    // handle integer type
}
```

### New KgpcType System

The `KgpcType` structure (defined in `KgpcType.h`) provides a rich, structured type representation:

```c
typedef struct KgpcType {
    KgpcTypeKind kind;  // TYPE_KIND_PRIMITIVE, TYPE_KIND_POINTER, etc.
    int size_in_bytes;
    int ref_count;
    union {
        int primitive_type_tag;
        KgpcType *points_to;
        ArrayTypeInfo array_info;
        ProcedureTypeInfo proc_info;
        struct RecordType *record_info;
    } info;
} KgpcType;
```

The new system provides predicate functions for type checking:

```c
int kgpc_type_is_integer(const KgpcType *type);
int kgpc_type_is_char(const KgpcType *type);
int kgpc_type_is_string(const KgpcType *type);
int kgpc_type_is_boolean(const KgpcType *type);
int kgpc_type_is_array(const KgpcType *type);
int kgpc_type_is_record(const KgpcType *type);
int kgpc_type_is_pointer(const KgpcType *type);
// ... etc
```

## Migration Status

### Completed

1. **Default Indexed Property Refactoring**: Removed hardcoded `TStringList` checks and replaced with a general-purpose `default_indexed_property` field on `RecordType`. This allows any class with an open array field to support array-like indexing.

2. **API for New Code**: Added `semcheck_expr_with_type()` function as the preferred API for new code. This returns a `KgpcType*` instead of an integer tag.

3. **Example Migrations**: Migrated `semcheck_builtin_chr` and `semcheck_builtin_ord` to use the new API.

### In Progress

The legacy wrapper function `semcheck_expr_legacy_tag()` still exists and is used in approximately 81 callsites across 6 files:

| File | Count |
|------|-------|
| SemCheck_Expr_Builtins.c | 41 |
| SemCheck_Expr_Access.c | 19 |
| SemCheck_Expr_Types.c | 8 |
| SemCheck_Expr_Ops.c | 7 |
| SemCheck_Expr_Constructors.c | 4 |
| SemCheck_overload.c | 2 |

## Migration Pattern

### Before (Legacy)

```c
int arg_type = UNKNOWN_TYPE;
int error_count = semcheck_expr_legacy_tag(&arg_type, symtab, arg_expr,
    max_scope_lev, NO_MUTATE);
if (error_count == 0 && arg_type != INT_TYPE && arg_type != LONGINT_TYPE) {
    // error: expected integer
}
```

### After (KgpcType)

```c
KgpcType *arg_kgpc_type = NULL;
int error_count = semcheck_expr_with_type(&arg_kgpc_type, symtab, arg_expr,
    max_scope_lev, NO_MUTATE);
if (error_count == 0 && !kgpc_type_is_integer(arg_kgpc_type)) {
    // error: expected integer
}
```

### Mapping from Tags to Predicates

| Legacy Tag Check | KgpcType Predicate |
|-----------------|-------------------|
| `type == INT_TYPE \|\| type == LONGINT_TYPE` | `kgpc_type_is_integer(type)` |
| `type == CHAR_TYPE` | `kgpc_type_is_char(type)` |
| `type == STRING_TYPE` | `kgpc_type_is_string(type)` |
| `type == BOOL` | `kgpc_type_is_boolean(type)` |
| `type == REAL_TYPE` | `kgpc_type_is_real(type)` |
| `type == POINTER_TYPE` | `kgpc_type_is_pointer(type)` |
| `type == RECORD_TYPE` | `kgpc_type_is_record(type)` |
| `is_integer_type(type)` | `kgpc_type_is_integer(type)` |
| `is_string_type(type)` | `kgpc_type_is_string(type)` (includes shortstring) |

### Fallback for Unusual Cases

For type tags that don't have a direct predicate (like `ENUM_TYPE`, `SET_TYPE`, `FILE_TYPE`), use:

```c
if (kgpc_type_equals_tag(arg_kgpc_type, ENUM_TYPE)) {
    // handle enum type
}
```

## Guidelines for New Code

1. **Always use `semcheck_expr_with_type()`** for new code. The legacy function is deprecated.

2. **Use KgpcType predicates** instead of comparing integer tags.

3. **Access the resolved type** via `expr->resolved_kgpc_type` after semantic checking.

4. **Don't free the returned KgpcType** - it's owned by the expression and will be freed when the expression is destroyed.

## Benefits of Migration

1. **Better Type Information**: `KgpcType` captures full type details including array bounds, pointer targets, and record structures.

2. **Cleaner Code**: Predicate functions like `kgpc_type_is_integer()` are more readable than tag comparisons.

3. **Extensibility**: Adding new type checks is easier with the structured approach.

4. **Eliminates Lossy Conversion**: The legacy tags lose information (e.g., all arrays become just `ARRAY_TYPE`). KgpcType preserves element types, bounds, etc.

## Files Changed

- `tree_types.h`: Added `default_indexed_property`, `default_indexed_element_type`, and `default_indexed_element_type_id` to `RecordType`.
- `tree.c`: Updated `destroy_record_type()` and `clone_record_type()` to handle new fields.
- `from_cparser.c`: Initialize new fields when creating `RecordType`.
- `SemCheck.c`: Added `detect_default_indexed_property()` function.
- `SemCheck_Expr_Access.c`: Replaced TStringList hardcoding with `default_indexed_property` lookup.
- `SemCheck_stmt.c`: Replaced TStringList hardcoding in for-in loop handling.
- `codegen_statement.c`: Replaced TStringList hardcoding in for-in codegen.
- `SemCheck_Expr_Internal.h`: Added `semcheck_expr_with_type()` function and documentation.
- `SemCheck_Expr_Builtins.c`: Migrated `semcheck_builtin_chr` and `semcheck_builtin_ord` to use new API.
