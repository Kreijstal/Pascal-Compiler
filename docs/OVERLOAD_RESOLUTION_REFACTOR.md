# Type Resolution Consolidation Plan

Goal: remove legacy type tags and make KgpcType the only source of truth across parsing,
semantic checks, overload resolution, codegen, and diagnostics.

## Phase 0: Inventory + Guardrails

- Audit all legacy type usage (`resolved_type`, `type_return`, `*_TYPE` tag checks,
  `kgpc_type_get_legacy_tag`).
- Identify bridge points that still depend on tags: overload resolution, builtins,
  assignment checks, and codegen.
- Add a temporary lint list (rg-based) to track remaining tag usage.

## Phase 1: KgpcType Everywhere

- Ensure every expression and statement has a valid `resolved_kgpc_type` (no NULL except error nodes).
- Add helper predicates so callers never need legacy tags:
  - `kgpc_type_is_char/string/shortstring/array/record/procedure/pointer`
  - `kgpc_type_is_numeric`, `kgpc_type_is_integer`, `kgpc_type_is_float`
- Replace `resolved_type` writes with `resolved_kgpc_type` creation/update.

## Phase 2: Semantic Checks on KgpcType Only

- Change `semcheck_expr_main` to return `KgpcType *` (or fill out a `KgpcType **`).
- Remove `type_return` from helper call chains; pass KgpcType instead.
- Update overload resolution to use KgpcType comparisons only.
- Update assignment compatibility to operate purely on KgpcType (no tag fallbacks).

## Phase 3: Codegen Without Legacy Tags

- Remove legacy-tag-driven codegen branches.
- Replace `switch(tag)` with `switch(type->kind)` plus primitive tag only for
  `TYPE_KIND_PRIMITIVE`.
- Ensure all type-tag-only AST nodes are upgraded during semantic analysis.

## Phase 4: Diagnostics and Error Formatting

- Make error messages use `kgpc_type_to_string` consistently.
- Remove legacy tag formatting in errors.
- Fix line/column file mapping based on `source_index` for exact error locations.

## Phase 5: Remove Legacy Storage

- Drop `resolved_type` and `type_return` where possible.
- Remove `kgpc_type_get_legacy_tag` and most `*_TYPE` usage in semantic/codegen
  (keep primitive tags only inside KgpcType).
- Delete VarType fallback usage where KgpcType exists.

## Phase 6: Tests + Cleanup

- Add regression tests that assert behavior with KgpcType-only flow.
- Run full test suite and sysutils compile to confirm zero regressions.
- Remove dead helpers and tag-only code paths.
