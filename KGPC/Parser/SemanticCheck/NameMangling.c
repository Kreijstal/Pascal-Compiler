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
#include "../ParseTree/ident_ref.h"
#include "../pascal_frontend.h"

/* Cached getenv() — defined in SemCheck.c */
extern const char *kgpc_getenv(const char *name);
static const char *g_mangle_caller_name = NULL; /* debug only */

/*
 * BUILTIN_TYPE_MAP — single source of truth for all Pascal builtin type names.
 *
 * Each entry maps one Pascal type name to its canonical VarType (used for name
 * mangling) and primitive type tag (used by semcheck/codegen).  Aliases (e.g.
 * AnsiChar → Char) are included with is_alias = 1 but point to the same
 * canonical VarType as their primary type.
 *
 * Lookup is done via kgpc_lookup_builtin_type() which uses strcasecmp, matching
 * the case-insensitive semantics of Pascal.
 *
 * NOTE: "Integer" is deliberately absent.  In ObjFPC/Delphi mode ObjPas
 * redefines Integer = LongInt; hardcoding it here would bypass the live
 * symbol-table lookup that reflects that override.  Callers that hit
 * HASHVAR_UNTYPED fall through to find_type_node_for_mangling().
 */
static const KgpcBuiltinTypeInfo BUILTIN_TYPE_MAP[] = {
    /* name                 mangle_var_type         primitive_type_tag  is_alias
     */

    /* --- character types --- */
    {"Char", HASHVAR_CHAR, CHAR_TYPE, 0},
    {"AnsiChar", HASHVAR_CHAR, CHAR_TYPE, 1},
    {"WideChar", HASHVAR_WIDECHAR, CHAR_TYPE, 0},
    {"UnicodeChar", HASHVAR_WIDECHAR, CHAR_TYPE, 1},

    /* --- string types --- */
    {"String", HASHVAR_PCHAR, STRING_TYPE, 0},
    {"AnsiString", HASHVAR_PCHAR, STRING_TYPE, 0},
    {"RawByteString", HASHVAR_RAWBYTESTRING, STRING_TYPE, 0},
    {"UnicodeString", HASHVAR_UNICODESTRING, STRING_TYPE, 0},
    {"WideString", HASHVAR_UNICODESTRING, STRING_TYPE, 1},
    {"ShortString", HASHVAR_SHORTSTRING, SHORTSTRING_TYPE, 0},

    /* --- integer types --- */
    {"Byte", HASHVAR_INTEGER, BYTE_TYPE, 0},
    {"Word", HASHVAR_INTEGER, WORD_TYPE, 0},
    {"TSystemCodePage", HASHVAR_INTEGER, WORD_TYPE, 1},
    {"LongInt", HASHVAR_LONGINT, LONGINT_TYPE, 0},
    {"Int64", HASHVAR_INT64, INT64_TYPE, 0},
    {"SizeInt", HASHVAR_INT64, INT64_TYPE, 1},
    {"QWord", HASHVAR_QWORD, QWORD_TYPE, 0},
    {"SizeUInt", HASHVAR_QWORD, QWORD_TYPE, 1},

    /* --- real types --- */
    {"Real", HASHVAR_REAL, REAL_TYPE, 0},
    {"Double", HASHVAR_REAL, REAL_TYPE, 1},

    /* --- boolean --- */
    {"Boolean", HASHVAR_BOOLEAN, BOOL, 0},

    /* --- pointer types --- */
    {"Pointer", HASHVAR_POINTER, POINTER_TYPE, 0},
    {"CodePointer", HASHVAR_POINTER, POINTER_TYPE, 1},
    {"PChar", HASHVAR_PANSICHAR, POINTER_TYPE, 0},
    {"PAnsiChar", HASHVAR_PANSICHAR, POINTER_TYPE, 1},
    {"PWideChar", HASHVAR_PWIDECHAR, POINTER_TYPE, 0},
    {"PWChar", HASHVAR_PWIDECHAR, POINTER_TYPE, 1},

    /* --- file types --- */
    {"Text", HASHVAR_TEXT, TEXT_TYPE, 0},
    {"TypedFile", HASHVAR_TYPEDFILE, FILE_TYPE, 0},
    {"File", HASHVAR_FILE, FILE_TYPE, 0},
};

#define BUILTIN_TYPE_MAP_COUNT                                                 \
  ((int)(sizeof(BUILTIN_TYPE_MAP) / sizeof(BUILTIN_TYPE_MAP[0])))

int kgpc_lookup_builtin_type(const char *name, KgpcBuiltinTypeInfo *out) {
  if (name == NULL)
    return 0;
  for (int i = 0; i < BUILTIN_TYPE_MAP_COUNT; ++i) {
    if (strcasecmp(name, BUILTIN_TYPE_MAP[i].name) == 0) {
      if (out != NULL)
        *out = BUILTIN_TYPE_MAP[i];
      return 1;
    }
  }
  return 0;
}

/*
 * resolve_string_alias_to_vartype — map a string alias name to its mangling
 * VarType.
 *
 * Given an alias_name that is known to belong to a STRING_TYPE primitive (e.g.
 * the alias_name field of a TypeAlias or return_type_id of a function), return
 * the VarType that distinguishes RawByteString, UnicodeString, and generic
 * AnsiString/String from each other.
 *
 * Returns HASHVAR_UNTYPED if alias_name is NULL or not a recognized string
 * alias, so callers can detect "no override needed" without a separate null
 * check.
 */
static enum VarType resolve_string_alias_to_vartype(const char *alias_name) {
  if (alias_name == NULL)
    return HASHVAR_UNTYPED;
  KgpcBuiltinTypeInfo info;
  if (kgpc_lookup_builtin_type(alias_name, &info) &&
      (info.primitive_type_tag == STRING_TYPE ||
       info.primitive_type_tag == SHORTSTRING_TYPE))
    return info.mangle_var_type;
  return HASHVAR_UNTYPED;
}

// Helper to create a lowercase copy of a string (for case-insensitive mangling)
static char *str_tolower_dup(const char *src) {
  if (src == NULL)
    return NULL;

  size_t len = strlen(src);
  char *dst = (char *)malloc(len + 1);
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

static MangleType *create_mangle_type(int kind, const char *type_id) {
  MangleType *mt = (MangleType *)malloc(sizeof(MangleType));
  if (mt == NULL)
    return NULL;
  mt->kind = kind;
  mt->type_id = type_id != NULL ? strdup(type_id) : NULL;
  return mt;
}

static char *sanitize_type_id(const char *type_id) {
  if (type_id == NULL)
    return NULL;
  size_t len = strlen(type_id);
  char *out = (char *)malloc(len + 1);
  if (out == NULL)
    return NULL;
  for (size_t i = 0; i < len; ++i) {
    unsigned char c = (unsigned char)type_id[i];
    if (isalnum(c) || c == '_')
      out[i] = (char)tolower(c);
    else
      out[i] = '_';
  }
  out[len] = '\0';
  return out;
}

/* Return the base name from type_ref when available, else the raw type_id
 * string. If type_ref is non-NULL its base_name must be non-NULL (TypeRef
 * invariant). */
static const char *type_ref_resolved_name(const TypeRef *ref,
                                          const char *raw_type_id) {
  if (ref == NULL)
    return raw_type_id;
  const char *base = type_ref_base_name(ref);
  assert(base != NULL &&
         "TypeRef.base_name must be populated by type resolution");
  return base;
}

static int mangle_type_id_is_extended(const char *type_id) {
  return (type_id != NULL && strcasecmp(type_id, "Extended") == 0);
}

static int mangle_kgpc_type_is_extended_real(const KgpcType *type) {
  if (type == NULL || !kgpc_type_is_real(type))
    return 0;
  if (kgpc_type_is_extended(type))
    return 1;
  return kgpc_type_real_storage_size(type) > 8;
}

static char *type_ref_render_mangled_unqualified(const TypeRef *ref) {
  if (ref == NULL)
    return NULL;
  const char *base = type_ref_base_name(ref);
  if (base == NULL)
    return NULL;
  if (ref->num_generic_args <= 0)
    return strdup(base);

  size_t total = strlen(base) + 1;
  for (int i = 0; i < ref->num_generic_args; ++i) {
    char *arg = type_ref_render_mangled(ref->generic_args[i]);
    if (arg != NULL) {
      total += strlen(arg);
      free(arg);
    }
    if (i + 1 < ref->num_generic_args)
      total += 1;
  }

  char *out = (char *)malloc(total + 1);
  if (out == NULL)
    return NULL;
  out[0] = '\0';
  strcat(out, base);
  strcat(out, "$");
  for (int i = 0; i < ref->num_generic_args; ++i) {
    char *arg = type_ref_render_mangled(ref->generic_args[i]);
    if (arg != NULL) {
      strcat(out, arg);
      free(arg);
    }
    if (i + 1 < ref->num_generic_args)
      strcat(out, "$");
  }
  return out;
}

// Helper to free a list of mangle types
static void DestroyMangleTypeList(ListNode_t *list) {
  assert(list != NULL);
  ListNode_t *cur = list;
  while (cur != NULL) {
    MangleType *mt = (MangleType *)cur->cur;
    if (mt != NULL) {
      free(mt->type_id);
      free(mt);
    }
    ListNode_t *next = cur->next;
    free(cur);
    cur = next;
  }
}

static enum VarType ConvertParserTypeToVarType(int parser_type) {
  switch (parser_type) {
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
    return HASHVAR_QWORD;
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
static enum VarType GetVarTypeFromTypeNode(HashNode_t *type_node) {
  if (type_node == NULL)
    return HASHVAR_UNTYPED;

  if (type_node->id != NULL && strcasecmp(type_node->id, "text") == 0)
    return HASHVAR_TEXT;
  if (type_node->id != NULL && strcasecmp(type_node->id, "TypedFile") == 0)
    return HASHVAR_TYPEDFILE;

  // If KgpcType is available, extract VarType from it
  if (type_node->type != NULL) {
    if (kgpc_type_is_shortstring(type_node->type))
      return HASHVAR_SHORTSTRING;
    if (type_node->type->kind == TYPE_KIND_PRIMITIVE) {
      int tag = kgpc_type_get_primitive_tag(type_node->type);
      /* Distinguish WideChar/UnicodeChar (2 bytes) from AnsiChar (1 byte) */
      if (tag == CHAR_TYPE && type_node->type->type_alias != NULL &&
          type_node->type->type_alias->storage_size > 1)
        return HASHVAR_WIDECHAR;
      return ConvertParserTypeToVarType(tag);
    } else if (type_node->type->kind == TYPE_KIND_POINTER) {
      return HASHVAR_POINTER;
    } else if (type_node->type->kind == TYPE_KIND_RECORD) {
      return HASHVAR_RECORD;
    } else if (type_node->type->kind == TYPE_KIND_ARRAY) {
      return HASHVAR_ARRAY;
    } else if (type_node->type->kind == TYPE_KIND_PROCEDURE) {
      if (kgpc_type_is_method_pointer(type_node->type))
        return HASHVAR_METHODPROCEDURE;
      return HASHVAR_PROCEDURE;
    }
  }

  return HASHVAR_UNTYPED;
}

/*
 * MapBuiltinTypeNameToVarType — map a Pascal builtin type name to its VarType.
 *
 * Implemented via BUILTIN_TYPE_MAP; see the table comment for the "Integer"
 * exclusion rationale (symbol-table override must win).
 */
static enum VarType MapBuiltinTypeNameToVarType(const char *type_name) {
  KgpcBuiltinTypeInfo info;
  if (kgpc_lookup_builtin_type(type_name, &info))
    return info.mangle_var_type;
  return HASHVAR_UNTYPED;
}

static HashNode_t *find_type_node_for_mangling(SymTab_t *symtab,
                                               const TypeRef *type_ref,
                                               const char *type_id) {
  if (symtab == NULL || (type_id == NULL && type_ref == NULL))
    return NULL;

  HashNode_t *type_node = NULL;
  char *qualified = NULL;
  const char *lookup_name = type_id;
  if (type_ref != NULL) {
    qualified = type_ref_render_mangled(type_ref);
    if (qualified != NULL)
      lookup_name = qualified;
  }
  if (lookup_name != NULL && FindSymbol(&type_node, symtab, lookup_name) != 0 &&
      type_node != NULL) {
    if (type_node->hash_type == HASHTYPE_TYPE) {
      free(qualified);
      return type_node;
    }
  }

  if (lookup_name == NULL) {
    free(qualified);
    return NULL;
  }

  ListNode_t *matches = FindAllIdents(symtab, lookup_name);
  ListNode_t *cur = matches;
  while (cur != NULL) {
    HashNode_t *candidate = (HashNode_t *)cur->cur;
    if (candidate != NULL && candidate->hash_type == HASHTYPE_TYPE) {
      DestroyList(matches);
      free(qualified);
      return candidate;
    }
    cur = cur->next;
  }
  DestroyList(matches);

  if (type_ref != NULL && type_ref->name != NULL && type_ref->name->count > 1) {
    char *unqualified = type_ref_render_mangled_unqualified(type_ref);
    if (unqualified != NULL) {
      HashNode_t *unqualified_node = NULL;
      if (FindSymbol(&unqualified_node, symtab, unqualified) != 0 &&
          unqualified_node != NULL &&
          unqualified_node->hash_type == HASHTYPE_TYPE) {
        free(unqualified);
        free(qualified);
        return unqualified_node;
      }
      ListNode_t *matches_unqualified = FindAllIdents(symtab, unqualified);
      ListNode_t *cur_unqualified = matches_unqualified;
      while (cur_unqualified != NULL) {
        HashNode_t *candidate = (HashNode_t *)cur_unqualified->cur;
        if (candidate != NULL && candidate->hash_type == HASHTYPE_TYPE) {
          DestroyList(matches_unqualified);
          free(unqualified);
          free(qualified);
          return candidate;
        }
        cur_unqualified = cur_unqualified->next;
      }
      DestroyList(matches_unqualified);
      free(unqualified);
    }
  }

  free(qualified);
  return NULL;
}

// Helper function to flatten argument lists into a list of HASHVAR_ types.
static ListNode_t *
GetFlatTypeListForMangling(ListNode_t *args,
                           SymTab_t *symtab) { // <-- Add symtab
  assert(symtab != NULL);
  if (args == NULL) {
    return NULL;
  }

  ListNode_t *type_list = NULL;
  ListNode_t *arg_cur = args;
  while (arg_cur != NULL) {
    Tree_t *decl_tree = (Tree_t *)arg_cur->cur;
    enum VarType resolved_type = HASHVAR_UNTYPED; // Default to untyped
    const char *record_type_id = NULL;
    ListNode_t *ids;

    if (decl_tree->type == TREE_VAR_DECL) {
      ids = decl_tree->tree_data.var_decl_data.ids;
      // --- THIS IS THE CORE LOGIC ---
      struct TypeAlias *inline_alias =
          decl_tree->tree_data.var_decl_data.inline_type_alias;
      if (inline_alias != NULL && inline_alias->is_array) {
        int element_type = inline_alias->array_element_type;
        const char *element_type_id = inline_alias->array_element_type_id;

        if (element_type == ARRAY_OF_CONST_TYPE ||
            (element_type_id != NULL &&
             strcasecmp(element_type_id, "const") == 0)) {
          resolved_type = HASHVAR_ARRAY;
        } else if (element_type == UNKNOWN_TYPE && element_type_id != NULL) {
          resolved_type = MapBuiltinTypeNameToVarType(type_ref_resolved_name(
              inline_alias->array_element_type_ref, element_type_id));
          if (resolved_type == HASHVAR_UNTYPED) {
            HashNode_t *type_node = find_type_node_for_mangling(
                symtab, inline_alias->array_element_type_ref, element_type_id);
            if (type_node != NULL)
              resolved_type = GetVarTypeFromTypeNode(type_node);
          }
          if (resolved_type != HASHVAR_UNTYPED)
            resolved_type = resolved_type + 100;
          else
            resolved_type = HASHVAR_ARRAY;
        } else if (element_type != UNKNOWN_TYPE) {
          resolved_type = ConvertParserTypeToVarType(element_type);
          if (resolved_type != HASHVAR_UNTYPED)
            resolved_type = resolved_type + 100;
          else
            resolved_type = HASHVAR_ARRAY;
        } else {
          resolved_type = HASHVAR_ARRAY;
        }
      }

      if (resolved_type == HASHVAR_UNTYPED &&
          (decl_tree->tree_data.var_decl_data.type_ref != NULL ||
           decl_tree->tree_data.var_decl_data.type_id != NULL)) {
        const TypeRef *type_ref = decl_tree->tree_data.var_decl_data.type_ref;
        const char *type_id = decl_tree->tree_data.var_decl_data.type_id;

        // First try to map built-in type names directly
        resolved_type = MapBuiltinTypeNameToVarType(
            type_ref_resolved_name(type_ref, type_id));
        if (resolved_type == HASHVAR_REAL &&
            mangle_type_id_is_extended(
                type_ref_resolved_name(type_ref, type_id)))
          record_type_id = "Extended";
        if (resolved_type == HASHVAR_REAL && record_type_id == NULL) {
          const char *base = type_ref_resolved_name(type_ref, type_id);
          if (base != NULL && strcasecmp(base, "Single") == 0)
            record_type_id = "Single";
        }

        // If not a built-in type, look it up in the symbol table
        if (resolved_type == HASHVAR_UNTYPED) {
          HashNode_t *type_node =
              find_type_node_for_mangling(symtab, type_ref, type_id);
          if (type_node != NULL) {
            resolved_type = GetVarTypeFromTypeNode(type_node);
            if (resolved_type == HASHVAR_RECORD && type_node->type != NULL &&
                type_node->type->kind == TYPE_KIND_RECORD &&
                type_node->type->info.record_info != NULL &&
                type_node->type->info.record_info->type_id != NULL) {
              record_type_id = type_node->type->info.record_info->type_id;
            }
            /* For pointer types, include a distinguishing name so overloads
             * with different pointer type params get distinct mangled names.
             * Skip for "Self" parameters since methods are already
             * distinguished by ClassName__ prefix and including Self's class
             * type breaks inherited calls. */
            {
              int is_self_param = 0;
              if (ids != NULL && ids->cur != NULL)
                is_self_param =
                    (strcasecmp((const char *)ids->cur, "Self") == 0);
              if (!is_self_param && resolved_type == HASHVAR_POINTER &&
                  type_node->type != NULL &&
                  type_node->type->kind == TYPE_KIND_POINTER) {
                /* For class/interface pointer types, use the pointed-to
                 * record's type_id (e.g. TObject, IInterface). */
                if (type_node->type->info.points_to != NULL &&
                    type_node->type->info.points_to->kind == TYPE_KIND_RECORD &&
                    type_node->type->info.points_to->info.record_info != NULL &&
                    type_node->type->info.points_to->info.record_info
                            ->type_id != NULL &&
                    record_type_is_class(
                        type_node->type->info.points_to->info.record_info)) {
                  record_type_id = type_node->type->info.points_to->info
                                       .record_info->type_id;
                }
                /* For all other named pointer type aliases (e.g. PAnsi =
                 * ^AnsiString, PShort = ^ShortString, PDirRec = ^TDirRec), use
                 * the alias name itself. This ensures overloads with different
                 * pointer-type parameters get distinct mangled names.  Do NOT
                 * use the pointed-to type's name here (which was the original
                 * concern) — we use the alias name (type_id) instead, which is
                 * always distinct from the pointed-to type's name. */
                else if (record_type_id == NULL && type_id != NULL) {
                  record_type_id = type_id;
                }
              }
            }
            if (resolved_type == HASHVAR_REAL &&
                mangle_kgpc_type_is_extended_real(type_node->type))
              record_type_id = "Extended";
            if (resolved_type == HASHVAR_REAL && record_type_id == NULL &&
                type_node->type != NULL &&
                kgpc_type_real_storage_size(type_node->type) == 4)
              record_type_id = "Single";
          }
        }
      } else {
        if (resolved_type == HASHVAR_UNTYPED) {
          // It's a built-in type, convert from parser token to semantic type
          resolved_type = ConvertParserTypeToVarType(
              decl_tree->tree_data.var_decl_data.type);
          if (resolved_type == HASHVAR_REAL &&
              decl_tree->tree_data.var_decl_data.type == EXTENDED_TYPE)
            record_type_id = "Extended";
        }
      }

      if (resolved_type == HASHVAR_RECORD && record_type_id == NULL &&
          decl_tree->tree_data.var_decl_data.inline_record_type != NULL &&
          decl_tree->tree_data.var_decl_data.inline_record_type->type_id !=
              NULL) {
        record_type_id =
            decl_tree->tree_data.var_decl_data.inline_record_type->type_id;
      }
      /* Honour the parser's {$H-} remap.  When the parser converted
       * `s: string` to SHORTSTRING_TYPE on the var_decl (because
       * {$H-} was active or the type was textually `ShortString`),
       * MapBuiltinTypeNameToVarType("string") still returns
       * HASHVAR_PCHAR (AnsiString).  The type tag on the decl is the
       * authoritative signal — prefer SHORTSTRING when set.
       *
       * Forward (interface) declarations of class methods can reach
       * this path with var_decl.type == STRING_TYPE because the
       * parser path that builds them (convert_param_list ->
       * convert_type_spec) does not invoke apply_shortstring_mode.
       * The implementation's args_var, in contrast, IS remapped to
       * SHORTSTRING_TYPE via other paths.  When the call-resolution
       * path re-mangles from the candidate's proc_info.params
       * (typically the interface's params), it gets the un-remapped
       * STRING_TYPE form — disagreeing with the def-side label
       * which uses the impl's args_var.  Resolve the asymmetry by
       * consulting the live symbol table: if the user's "string"
       * type-id resolves to a ShortString-aware KgpcType, treat it
       * as ShortString here too. */
      int parser_marked_shortstring =
          (decl_tree->tree_data.var_decl_data.type == SHORTSTRING_TYPE);
      if (!parser_marked_shortstring &&
          decl_tree->tree_data.var_decl_data.type == STRING_TYPE &&
          decl_tree->tree_data.var_decl_data.type_id != NULL &&
          strcasecmp(decl_tree->tree_data.var_decl_data.type_id, "string") ==
              0 &&
          pascal_frontend_default_shortstring()) {
        /* Belt-and-braces: even if the upstream parameter-build
         * paths missed the {$H-} remap, honour the currently-
         * active mode here too. */
        parser_marked_shortstring = 1;
      }
      if (parser_marked_shortstring &&
          (resolved_type == HASHVAR_PCHAR ||
           resolved_type == HASHVAR_RAWBYTESTRING ||
           resolved_type == HASHVAR_UNICODESTRING ||
           resolved_type == HASHVAR_UNTYPED)) {
        resolved_type = HASHVAR_SHORTSTRING;
        record_type_id = NULL;
      }
      if (resolved_type == HASHVAR_REAL &&
          mangle_kgpc_type_is_extended_real(
              decl_tree->tree_data.var_decl_data.cached_kgpc_type)) {
        record_type_id = "Extended";
      }
      if (resolved_type == HASHVAR_REAL && record_type_id == NULL &&
          decl_tree->tree_data.var_decl_data.cached_kgpc_type != NULL &&
          kgpc_type_real_storage_size(
              decl_tree->tree_data.var_decl_data.cached_kgpc_type) == 4) {
        record_type_id = "Single";
      }
      if (resolved_type == HASHVAR_REAL && record_type_id == NULL &&
          inline_alias != NULL && inline_alias->storage_size > 8) {
        record_type_id = "Extended";
      }
      if (resolved_type == HASHVAR_REAL && record_type_id == NULL &&
          inline_alias != NULL && inline_alias->storage_size == 4) {
        record_type_id = "Single";
      }
    } else if (decl_tree->type == TREE_ARR_DECL) {
      ids = decl_tree->tree_data.arr_decl_data.ids;
      /* For open array parameters, include the element type in mangling
       * to distinguish between array of Char and array of Integer.
       * But array of const uses generic HASHVAR_ARRAY. */
      int element_type = decl_tree->tree_data.arr_decl_data.type;
      const char *element_type_id = decl_tree->tree_data.arr_decl_data.type_id;
      const TypeRef *element_type_ref =
          decl_tree->tree_data.arr_decl_data.type_ref;

      /* Special case: array of const uses generic array mangling */
      if (element_type == ARRAY_OF_CONST_TYPE ||
          (element_type_id != NULL &&
           strcasecmp(element_type_id, "const") == 0)) {
        resolved_type = HASHVAR_ARRAY;
      } else if (element_type == UNKNOWN_TYPE &&
                 (element_type_ref != NULL || element_type_id != NULL)) {
        /* Try to map element type from type_id */
        resolved_type = MapBuiltinTypeNameToVarType(
            type_ref_resolved_name(element_type_ref, element_type_id));
        if (resolved_type == HASHVAR_UNTYPED) {
          /* Look up in symbol table */
          HashNode_t *type_node = find_type_node_for_mangling(
              symtab, element_type_ref, element_type_id);
          if (type_node != NULL)
            resolved_type = GetVarTypeFromTypeNode(type_node);
        }
        /* Prepend 'a' to indicate array with this element type */
        if (resolved_type != HASHVAR_UNTYPED)
          resolved_type =
              resolved_type + 100; /* Use 100+ range for array element types */
        else
          resolved_type = HASHVAR_ARRAY;
      } else if (element_type != UNKNOWN_TYPE) {
        /* Use ConvertParserTypeToVarType and add 100 to indicate array */
        resolved_type = ConvertParserTypeToVarType(element_type);
        if (resolved_type != HASHVAR_UNTYPED)
          resolved_type =
              resolved_type + 100; /* Use 100+ range for array element types */
        else
          resolved_type = HASHVAR_ARRAY;
      } else {
        resolved_type = HASHVAR_ARRAY; /* Fallback for unknown element type */
      }
    } else {
      KGPC_SEMCHECK_HARD_ASSERT(
          0,
          "GetFlatTypeListForMangling: unsupported parameter decl tree type %d",
          decl_tree->type);
    }

    if (resolved_type < 100) {
      switch (resolved_type) {
      case HASHVAR_INTEGER:
      case HASHVAR_LONGINT:
      case HASHVAR_INT64:
      case HASHVAR_REAL:
      case HASHVAR_PROCEDURE:
      case HASHVAR_UNTYPED:
      case HASHVAR_PCHAR:
      case HASHVAR_PWIDECHAR:
      case HASHVAR_PANSICHAR:
      case HASHVAR_RECORD:
      case HASHVAR_ARRAY:
      case HASHVAR_BOOLEAN:
      case HASHVAR_CHAR:
      case HASHVAR_POINTER:
      case HASHVAR_SET:
      case HASHVAR_ENUM:
      case HASHVAR_FILE:
      case HASHVAR_TYPEDFILE:
      case HASHVAR_TEXT:
      case HASHVAR_RAWBYTESTRING:
      case HASHVAR_UNICODESTRING:
      case HASHVAR_SHORTSTRING:
      case HASHVAR_WIDECHAR:
      case HASHVAR_QWORD:
      case HASHVAR_METHODPROCEDURE:
        break;
      default:
        KGPC_SEMCHECK_HARD_ASSERT(
            0, "GetFlatTypeListForMangling: unsupported resolved VarType %d",
            resolved_type);
      }
    }

    ListNode_t *id_cur = ids;
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
static char *MangleNameFromTypeList(const char *original_name,
                                    ListNode_t *type_list) {
  assert(original_name != NULL);

  // Normalize function name to lowercase for case-insensitive matching
  // (Pascal is case-insensitive, so Lowercase and LowerCase should produce the
  // same mangled name)
  char *lower_name = str_tolower_dup(original_name);
  KGPC_SEMCHECK_HARD_ASSERT(lower_name != NULL,
                            "failed to normalize identifier '%s' for mangling",
                            original_name);

  if (type_list == NULL) {
    // No args, append _void
    const char *suffix = "_void";
    char *mangled_name = malloc(strlen(lower_name) + strlen(suffix) + 1);
    assert(mangled_name != NULL);
    sprintf(mangled_name, "%s%s", lower_name, suffix);
    free(lower_name);
    return mangled_name;
  }

  // Calculate length - max suffix is "_ai64" (5 chars), use 6 for safety
  size_t total_len = strlen(lower_name);
  ListNode_t *cur = type_list;
  while (cur != NULL) {
    MangleType *mt = (MangleType *)cur->cur;
    if (mt != NULL &&
        (mt->kind == HASHVAR_RECORD || mt->kind == HASHVAR_POINTER) &&
        mt->type_id != NULL) {
      char *sanitized = sanitize_type_id(mt->type_id);
      size_t extra = sanitized != NULL ? strlen(sanitized) : 0;
      if (sanitized != NULL)
        free(sanitized);
      total_len += 3 + extra; // "_u_" + type id
    } else {
      total_len += 6; // Max length of a type suffix (e.g., "_ai64" is 5 chars)
    }
    cur = cur->next;
  }
  total_len += 1; // Null terminator

  char *mangled_name = malloc(total_len);
  assert(mangled_name != NULL);
  strcpy(mangled_name, lower_name);
  free(lower_name);

  cur = type_list;
  while (cur != NULL) {
    MangleType *mt = (MangleType *)cur->cur;
    int type = mt != NULL ? mt->kind : HASHVAR_UNTYPED;
    const char *type_suffix = NULL;
    char *type_suffix_dynamic = NULL;
    /* Handle array element types (100+ range) */
    if (type >= 100) {
      int elem_type = type - 100;
      switch (elem_type) {
      case HASHVAR_INTEGER:
        type_suffix = "_ai";
        break; /* array of Integer */
      case HASHVAR_LONGINT:
        type_suffix = "_ali";
        break; /* array of LongInt */
      case HASHVAR_INT64:
        type_suffix = "_ai64";
        break; /* array of Int64 */
      case HASHVAR_QWORD:
        type_suffix = "_au64";
        break; /* array of QWord */
      case HASHVAR_REAL:
        if (mt != NULL && mangle_type_id_is_extended(mt->type_id))
          type_suffix = "_ax";
        else if (mt != NULL && mt->type_id != NULL &&
                 strcasecmp(mt->type_id, "Single") == 0)
          type_suffix = "_af";
        else
          type_suffix = "_ar";
        break;
      case HASHVAR_PCHAR:
        type_suffix = "_as";
        break; /* array of String */
      case HASHVAR_SHORTSTRING:
        type_suffix = "_ass";
        break; /* array of ShortString */
      case HASHVAR_BOOLEAN:
        type_suffix = "_ab";
        break; /* array of Boolean */
      case HASHVAR_CHAR:
        type_suffix = "_ac";
        break; /* array of Char */
      case HASHVAR_WIDECHAR:
        type_suffix = "_awc";
        break; /* array of WideChar */
      case HASHVAR_POINTER:
        type_suffix = "_ap";
        break; /* array of Pointer */
      case HASHVAR_PROCEDURE:
        type_suffix = "_apr";
        break; /* array of procedure */
      case HASHVAR_METHODPROCEDURE:
        type_suffix = "_amp";
        break; /* array of method procedure */
      case HASHVAR_RECORD:
        type_suffix = "_au";
        break; /* array of Record */
      default:
        type_suffix = "_a";
        break; /* array of unknown */
      }
    } else if ((type == HASHVAR_RECORD || type == HASHVAR_POINTER) &&
               mt != NULL && mt->type_id != NULL) {
      char *sanitized = sanitize_type_id(mt->type_id);
      if (sanitized != NULL) {
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
      case HASHVAR_INTEGER:
        type_suffix = "_i";
        break;
      case HASHVAR_LONGINT:
        type_suffix = "_li";
        break;
      case HASHVAR_INT64:
        type_suffix = "_i64";
        break;
      case HASHVAR_QWORD:
        type_suffix = "_u64";
        break;
      case HASHVAR_REAL:
        if (mt != NULL && mangle_type_id_is_extended(mt->type_id))
          type_suffix = "_x";
        else if (mt != NULL && mt->type_id != NULL &&
                 strcasecmp(mt->type_id, "Single") == 0)
          type_suffix = "_f";
        else
          type_suffix = "_r";
        break;
      case HASHVAR_PCHAR:
        type_suffix = "_s";
        break; // For String (keep backwards compat)
      case HASHVAR_SHORTSTRING:
        type_suffix = "_ss";
        break; // ShortString
      case HASHVAR_PANSICHAR:
        type_suffix = "_pc";
        break; // For PAnsiChar/PChar
      case HASHVAR_PWIDECHAR:
        type_suffix = "_pw";
        break; // For PWideChar
      case HASHVAR_BOOLEAN:
        type_suffix = "_b";
        break;
      case HASHVAR_CHAR:
        type_suffix = "_c";
        break;
      case HASHVAR_WIDECHAR:
        type_suffix = "_wc";
        break;
      case HASHVAR_POINTER:
        type_suffix = "_p";
        break;
      case HASHVAR_PROCEDURE:
        type_suffix = "_pr";
        break;
      case HASHVAR_METHODPROCEDURE:
        type_suffix = "_mp";
        break;
      case HASHVAR_SET:
        type_suffix = "_set";
        break;
      case HASHVAR_ENUM:
        type_suffix = "_e";
        break;
      case HASHVAR_FILE:
        type_suffix = "_f";
        break;
      case HASHVAR_TYPEDFILE:
        type_suffix = "_tf";
        break; // TypedFile (distinct from File)
      case HASHVAR_TEXT:
        type_suffix = "_t";
        break; // For text files
      case HASHVAR_RECORD:
        type_suffix = "_u";
        break; // Record types treated as unknown for mangling
      case HASHVAR_ARRAY:
        type_suffix = "_a";
        break; // Array
      case HASHVAR_RAWBYTESTRING:
        type_suffix = "_rbs";
        break; // RawByteString
      case HASHVAR_UNICODESTRING:
        type_suffix = "_us";
        break; // UnicodeString
      default:
        type_suffix = "_u";
        break; // Unknown/unsupported
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
char *MangleFunctionName(const char *original_name, ListNode_t *args,
                         SymTab_t *symtab) { // <-- Add symtab
  assert(original_name != NULL);
  assert(symtab != NULL);
  ListNode_t *type_list =
      GetFlatTypeListForMangling(args, symtab); // <-- Pass symtab
  return MangleNameFromTypeList(original_name, type_list);
}

// Helper function to get a flat list of types from a function call's arguments
static ListNode_t *GetFlatTypeListFromCallSite(ListNode_t *args_expr,
                                               SymTab_t *symtab,
                                               int max_scope_lev) {
  assert(symtab != NULL);
  if (args_expr == NULL) {
    return NULL;
  }

  ListNode_t *type_list = NULL;
  ListNode_t *arg_cur = args_expr;
  while (arg_cur != NULL) {
    struct Expression *arg_expr = (struct Expression *)arg_cur->cur;
    enum VarType resolved_type = HASHVAR_UNTYPED;
    const char *record_type_id = NULL;
    if (arg_expr != NULL && arg_expr->type == EXPR_RECORD_CONSTRUCTOR) {
      resolved_type = HASHVAR_RECORD;
      if (arg_expr->resolved_kgpc_type != NULL &&
          kgpc_type_is_record(arg_expr->resolved_kgpc_type)) {
        struct RecordType *record_type =
            kgpc_type_get_record(arg_expr->resolved_kgpc_type);
        if (record_type != NULL && record_type->type_id != NULL)
          record_type_id = record_type->type_id;
      }
    } else {
      KgpcType *arg_type = NULL;
      semcheck_expr_main(symtab, arg_expr, max_scope_lev, NO_MUTATE, &arg_type);
      int type_tag =
          arg_type != NULL ? semcheck_tag_from_kgpc(arg_type) : UNKNOWN_TYPE;
      resolved_type = ConvertParserTypeToVarType(type_tag);
      if (arg_type != NULL && arg_type->kind == TYPE_KIND_PROCEDURE)
        resolved_type = kgpc_type_is_method_pointer(arg_type)
                            ? HASHVAR_METHODPROCEDURE
                            : HASHVAR_PROCEDURE;
      /* Distinguish WideChar/UnicodeChar (2 bytes) from AnsiChar (1 byte) */
      if (resolved_type == HASHVAR_CHAR && arg_type != NULL &&
          arg_type->kind == TYPE_KIND_PRIMITIVE &&
          arg_type->type_alias != NULL &&
          arg_type->type_alias->storage_size > 1)
        resolved_type = HASHVAR_WIDECHAR;
      if (arg_expr != NULL && arg_expr->resolved_kgpc_type != NULL) {
        KgpcType *kgpc_type = arg_expr->resolved_kgpc_type;
        if (kgpc_type_is_array(kgpc_type) ||
            kgpc_type_is_array_of_const(kgpc_type)) {
          /* For arrays, include element type in mangling (100+ range) */
          KgpcType *elem_type = kgpc_type_is_array(kgpc_type)
                                    ? kgpc_type->info.array_info.element_type
                                    : NULL;
          if (elem_type != NULL && elem_type->kind == TYPE_KIND_PRIMITIVE) {
            int elem_tag = elem_type->info.primitive_type_tag;
            enum VarType elem_var = ConvertParserTypeToVarType(elem_tag);
            if (elem_var != HASHVAR_UNTYPED)
              resolved_type =
                  elem_var + 100; /* Use 100+ range for array element types */
            else
              resolved_type = HASHVAR_ARRAY;
          } else {
            resolved_type = HASHVAR_ARRAY;
          }
        } else if (kgpc_type->kind == TYPE_KIND_RECORD) {
          resolved_type = HASHVAR_RECORD;
          if (kgpc_type->info.record_info != NULL &&
              kgpc_type->info.record_info->type_id != NULL)
            record_type_id = kgpc_type->info.record_info->type_id;
        } else if (kgpc_type->kind == TYPE_KIND_POINTER) {
          resolved_type = HASHVAR_POINTER;
          /* For class/interface types (pointer to record), include the
           * record type_id in mangling to distinguish overloads like
           * Describe(TAnimal) vs Describe(TCat).
           * Skip for "Self" arguments since methods are distinguished
           * by ClassName__ prefix and including Self's class type
           * breaks inherited calls.
           * For non-class pointer types, do NOT use the pointed-to
           * record's type_id to avoid collision with the record type. */
          int is_self_arg = 0;
          if (arg_expr != NULL && arg_expr->type == EXPR_VAR_ID &&
              arg_expr->expr_data.id != NULL &&
              strcasecmp(arg_expr->expr_data.id, "Self") == 0)
            is_self_arg = 1;
          if (!is_self_arg) {
            KgpcType *points_to = kgpc_type->info.points_to;
            if (points_to != NULL && points_to->kind == TYPE_KIND_RECORD &&
                points_to->info.record_info != NULL &&
                points_to->info.record_info->type_id != NULL &&
                record_type_is_class(points_to->info.record_info))
              record_type_id = points_to->info.record_info->type_id;
          }
        } else if (kgpc_type->kind == TYPE_KIND_PROCEDURE)
          resolved_type = kgpc_type_is_method_pointer(kgpc_type)
                              ? HASHVAR_METHODPROCEDURE
                              : HASHVAR_PROCEDURE;
        if (resolved_type == HASHVAR_REAL &&
            mangle_kgpc_type_is_extended_real(kgpc_type))
          record_type_id = "Extended";
        /* Check type_alias for STRING_TYPE to distinguish between
         * RawByteString and UnicodeString. With the fix in commit 868406b,
         * type_alias is now owned by KgpcType and should be valid. */
        if (type_tag == STRING_TYPE && kgpc_getenv("KGPC_DEBUG_MANGLE")) {
          fprintf(stderr,
                  "[MANGLE] STRING_TYPE arg for %s: type_alias=%s, kind=%d, "
                  "expr_type=%d, line=%d",
                  g_mangle_caller_name ? g_mangle_caller_name : "?",
                  kgpc_type->type_alias
                      ? (kgpc_type->type_alias->alias_name
                             ? kgpc_type->type_alias->alias_name
                             : "<null_name>")
                      : "<null>",
                  kgpc_type->kind, arg_expr ? arg_expr->type : -1,
                  arg_expr ? arg_expr->line_num : -1);
          if (arg_expr != NULL &&
              arg_expr->type == 7 /* EXPR_FUNCTION_CALL */) {
            const char *call_id = arg_expr->expr_data.function_call_data.id;
            KgpcType *call_type =
                arg_expr->expr_data.function_call_data.call_kgpc_type;
            const char *ret_id = NULL;
            if (call_type != NULL && call_type->kind == TYPE_KIND_PROCEDURE)
              ret_id = call_type->info.proc_info.return_type_id;
            fprintf(stderr, ", call_id=%s, call_kgpc_type=%p, ret_id=%s",
                    call_id ? call_id : "<null>", (void *)call_type,
                    ret_id ? ret_id : "<null>");
          }
          fprintf(stderr, "\n");
        }
        if (type_tag == STRING_TYPE && kgpc_type->type_alias != NULL) {
          struct TypeAlias *alias = kgpc_type->type_alias;
          enum VarType alias_vt =
              resolve_string_alias_to_vartype(alias->alias_name);
          if (alias_vt != HASHVAR_UNTYPED)
            resolved_type = alias_vt;
        }
        /* Fallback: if type_alias is missing on a function call result,
         * look up the called function's return_type_id to distinguish
         * RawByteString from UnicodeString. */
        if (type_tag == STRING_TYPE && resolved_type == HASHVAR_PCHAR &&
            arg_expr != NULL && arg_expr->type == EXPR_FUNCTION_CALL) {
          const char *ret_id = NULL;
          /* Try call_kgpc_type first */
          KgpcType *call_type =
              arg_expr->expr_data.function_call_data.call_kgpc_type;
          if (call_type != NULL && call_type->kind == TYPE_KIND_PROCEDURE)
            ret_id = call_type->info.proc_info.return_type_id;
          /* If call_kgpc_type wasn't set, look up function in symbol table */
          if (ret_id == NULL &&
              arg_expr->expr_data.function_call_data.id != NULL) {
            HashNode_t *func_node = NULL;
            int find_result = FindSymbol(
                &func_node, symtab, arg_expr->expr_data.function_call_data.id);
            if (kgpc_getenv("KGPC_DEBUG_MANGLE"))
              fprintf(stderr,
                      "[MANGLE] fallback lookup '%s': find=%d node=%p "
                      "hash_type=%d type=%p kind=%d ret_id=%s\n",
                      arg_expr->expr_data.function_call_data.id, find_result,
                      (void *)func_node, func_node ? func_node->hash_type : -1,
                      func_node ? (void *)func_node->type : NULL,
                      (func_node && func_node->type) ? func_node->type->kind
                                                     : -1,
                      (func_node && func_node->type &&
                       func_node->type->kind == TYPE_KIND_PROCEDURE &&
                       func_node->type->info.proc_info.return_type_id)
                          ? func_node->type->info.proc_info.return_type_id
                          : "<null>");
            if (find_result && func_node != NULL && func_node->type != NULL &&
                func_node->type->kind == TYPE_KIND_PROCEDURE)
              ret_id = func_node->type->info.proc_info.return_type_id;
          }
          if (ret_id != NULL) {
            enum VarType alias_vt = resolve_string_alias_to_vartype(ret_id);
            if (alias_vt != HASHVAR_UNTYPED)
              resolved_type = alias_vt;
          }
        }
      }
      /* Also handle array literals that don't have resolved_kgpc_type yet */
      else if (arg_expr != NULL && (arg_expr->type == EXPR_ARRAY_LITERAL ||
                                    arg_expr->type == EXPR_SET)) {
        /* Get element type from first element of array literal */
        ListNode_t *first_elem = NULL;
        if (arg_expr->type == EXPR_ARRAY_LITERAL)
          first_elem = arg_expr->expr_data.array_literal_data.elements;
        else if (arg_expr->type == EXPR_SET)
          first_elem = arg_expr->expr_data.set_data.elements;

        if (first_elem != NULL && first_elem->cur != NULL) {
          struct Expression *elem_expr = (struct Expression *)first_elem->cur;
          /* Infer element type directly from expression type without calling
           * semcheck_expr_main since the expression may not be fully ready for
           * semantic checking yet */
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
            resolved_type =
                elem_var + 100; /* Use 100+ range for array element types */
          else
            resolved_type = HASHVAR_ARRAY;
        } else {
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

char *MangleFunctionNameFromCallSite(const char *original_name,
                                     ListNode_t *args_expr, SymTab_t *symtab,
                                     int max_scope_lev) {
  assert(original_name != NULL);
  assert(symtab != NULL);
  g_mangle_caller_name = original_name;
  ListNode_t *type_list =
      GetFlatTypeListFromCallSite(args_expr, symtab, max_scope_lev);
  g_mangle_caller_name = NULL;
  char *result = MangleNameFromTypeList(original_name, type_list);
  if (kgpc_getenv("KGPC_DEBUG_MANGLE") &&
      (strcasecmp(original_name, "FileExists") == 0 ||
       strcasecmp(original_name, "DirectoryExists") == 0))
    fprintf(stderr, "[MANGLE] MangleFunctionNameFromCallSite('%s') => '%s'\n",
            original_name, result ? result : "<null>");
  return result;
}
