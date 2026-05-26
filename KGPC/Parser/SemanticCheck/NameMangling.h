#ifndef NAME_MANGLING_H
#define NAME_MANGLING_H

#include "../List/List.h"
#include "HashTable/HashTable.h"
#include "SymTab/SymTab.h"

/*
 * KgpcBuiltinTypeInfo — the canonical metadata for a single Pascal builtin
 * type.
 *
 * One entry per name in the BUILTIN_TYPE_MAP table (defined in NameMangling.c).
 * Future call sites in semcheck/codegen can call kgpc_lookup_builtin_type() to
 * replace scattered strcasecmp chains with a single table lookup.
 */
typedef struct KgpcBuiltinTypeInfo {
  /*
   * Pascal type name exactly as it appears in source (case-insensitive match).
   * E.g. "AnsiString", "WideChar", "LongInt".
   */
  const char *name;

  /*
   * The VarType used for name-mangling.  This is the canonical enum value that
   * identifies the type for overload-resolution purposes.
   */
  enum VarType mangle_var_type;

  /*
   * The primitive type tag (INT_TYPE, CHAR_TYPE, …) used by the semantic
   * checker and code generator, or UNKNOWN_TYPE (0) when the type has no
   * primitive tag (e.g. compound / pointer / string types that are handled
   * specially).
   */
  int primitive_type_tag;

  /*
   * Non-zero if this entry is a type alias that resolves to another canonical
   * type (e.g. AnsiChar → Char, WideString → UnicodeString).  The VarType
   * in mangle_var_type is the *canonical* one.
   */
  int is_alias;
} KgpcBuiltinTypeInfo;

/*
 * Look up a Pascal builtin type name in the compiler's canonical type table.
 *
 * name   — case-insensitive Pascal identifier (e.g. "AnsiString", "integer").
 * out    — if non-NULL and the name is found, filled with the entry's metadata.
 *
 * Returns 1 if the name was found, 0 otherwise.
 *
 * Callers in semcheck/codegen can use this to replace inline strcasecmp chains
 * against builtin type names.  The existing MapBuiltinTypeNameToVarType() is
 * now implemented in terms of this helper; future callers may adopt it
 * directly.
 */
int kgpc_lookup_builtin_type(const char *name, KgpcBuiltinTypeInfo *out);

// Generates a mangled name for a function from its declaration.
// The caller is responsible for freeing the returned string.
char *MangleFunctionName(const char *original_name, ListNode_t *args,
                         SymTab_t *symtab);

// Generates a mangled name for a function from a call site.
// The caller is responsible for freeing the returned string.
char *MangleFunctionNameFromCallSite(const char *original_name,
                                     ListNode_t *args_expr, SymTab_t *symtab,
                                     int max_scope_lev);

#endif
