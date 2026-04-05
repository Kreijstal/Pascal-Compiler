/*
 * from_cparser.c — Parse tree construction from cparser AST (public API wrapper).
 *
 * The implementation is split across separate compilation units under
 * from_cparser_parts/ for maintainability:
 *   - from_cparser_init_and_registry.c       — globals, enum/const registries
 *   - from_cparser_generics.c                — generic type substitution, specialization
 *   - from_cparser_const_and_types.c         — const evaluation, type name mapping
 *   - from_cparser_records.c                 — record/class member conversion
 *   - from_cparser_declarations.c            — var/const declarations, routine body
 *   - from_cparser_expressions.c             — expression conversion, operator mapping
 *   - from_cparser_statements_and_programs.c — statements, subprograms, tree_from_pascal_ast
 *
 * This file is intentionally empty; the public API declared in
 * from_cparser.h is implemented across the above files.
 */
