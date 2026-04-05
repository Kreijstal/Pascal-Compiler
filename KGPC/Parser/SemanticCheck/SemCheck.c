/*
 * SemCheck.c — Semantic checking (public API wrapper).
 *
 * The implementation is split across separate compilation units under
 * SemCheck_parts/ for maintainability:
 *   - SemCheck_env_types_errors.c        — env cache, unit management, error reporting
 *   - SemCheck_symbols_and_class.c       — symbol table, class vars, method identity
 *   - SemCheck_const_eval.c              — constant expression evaluation, type resolution
 *   - SemCheck_init_and_inheritance.c    — init symtab, predeclare types, class inheritance
 *   - SemCheck_vmt_and_type_decls.c      — VMT building, type declaration checking
 *   - SemCheck_const_decls_and_builtins.c — const declarations, builtin registration
 *   - SemCheck_program_and_vars.c        — program/unit checking, variable declarations
 *   - SemCheck_subprograms.c             — subprogram checking
 *
 * This file is intentionally empty; the public API declared in
 * SemCheck.h is implemented across the above files.
 */
