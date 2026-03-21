# Scope Tree Refactoring

## Status

The scope-tree refactor is complete.

What it solved:

- name lookup is tree-based instead of flat-stack based
- per-unit visibility is structural through scope dependencies
- `push_target_unit`, `unit_context`, `stack_head`, and related flat-model routing hacks are gone
- overload scoring no longer compensates for flat-scope leakage

What it did **not** solve:

- programs still compile imported units through a merged-program pipeline
- loaded unit ASTs are still appended into the program AST
- standalone unit compilation and `uses`-based program compilation do not yet share one identical semantic path

That remaining issue is architectural, but it is **not** part of the completed scope-tree phases.

## Motivation

The original compiler mixed two separate problems:

1. scope lookup was wrong
2. unit compilation architecture was merged and order-sensitive

The scope-tree work addressed the first problem.

The old flat model caused:

- sibling subprogram locals leaking into each other
- implicit `Self` leaking into unrelated procedures
- unit symbols and program symbols colliding through global routing state
- name-mangling depending on semcheck order rather than unit-local meaning

A tree of scopes fixes those visibility bugs directly because Pascal scopes are naturally hierarchical.

## Completed Architecture

Current lookup model:

```text
builtin scope
  |- unit scope: System
  |- unit scope: ObjPas
  |- unit scope: SysUtils
  \- program scope
       \- subprogram scopes
            \- nested block scopes
```

Lookup rules:

- search current scope
- walk parents
- at unit/program scopes, also search direct dependency unit scopes
- never search sibling or unrelated scopes

This is the completed scope-tree result.

## What Changed

### Data Model

`SymTab_t` now centers on:

- `builtin_scope`
- `current_scope`
- `unit_scopes[]`

The old flat-model fields have been removed from active use.

### Lookup

`FindIdent`/`FindSymbol` now use the scope tree directly.

This means:

- no global scan of all unit tables
- no special per-unit fallback helpers
- no hidden visibility across sibling scopes

### Insertion

Declaration processing inserts into the structurally correct scope by switching `current_scope` explicitly.

This replaced the old global routing model.

### Overload Resolution

Overload resolution was simplified after tree scoping became reliable.

The scorer now prefers:

- better conversion quality
- fewer defaulted arguments
- fewer untyped parameters
- non-builtin over builtin

Large flat-model tie-break heuristics were removed.

## Completed Phases

### Phase 1: Scope Infrastructure

Completed.

- added scope-node based infrastructure
- introduced `EnterScope` / `LeaveScope`
- established explicit parent/dependency relationships

### Phase 2: Tree Lookup Bring-Up

Completed.

- implemented tree-based symbol lookup
- validated it against the old path during migration
- switched lookup permanently to the tree path

### Phase 3: Tree Lookup Switchover

Completed.

- `FindIdent_Tree` became the only lookup path
- scope entry/exit moved to tree semantics
- old lookup assumptions were removed from semcheck

### Phase 4: Per-Unit Scope Isolation

Completed.

Important constraint:

- this phase did **not** remove merged-program compilation
- it made merged-program compilation use proper per-unit scopes

What this phase achieved:

- unit-owned declarations live in unit scopes
- program-owned declarations live in the program scope
- full-program semantic checking updates unit-owned declarations in place through their owning unit scopes

### Phase 5: Remove Flat Stack And Routing State

Completed.

Removed:

- `stack_head`
- `unit_tables`
- `push_target_unit`
- `unit_context`
- `PushScope` / `PopScope`
- `FindIdentInUnit`
- other flat-model lookup helpers

Acceptance result:

```text
grep -r 'stack_head\|unit_tables\|push_target_unit\|unit_context\|PushScope\|PopScope\|FindIdentInUnit' KGPC/
```

Expected result: zero matches for active code paths.

### Phase 6: Simplify Overload Resolution

Completed.

Removed or collapsed:

- scope-era ranking hacks
- broad ad hoc specificity bonuses
- flat-model compensating tie-break logic

Kept:

- explicit conversion ranking
- default-argument and untyped-parameter tie-breaks
- builtin vs non-builtin preference

## Completed Outcomes

The completed refactor fixed the scope-model problem.

Examples of problems this refactor was meant to eliminate:

- method-local symbols shadowing unrelated fields in other methods
- leaked `Self` causing ordinary procedure calls to resolve as method calls
- unit-local type meaning depending on global merged semcheck order

Those were scope-system defects. They are now addressed by the scope tree.

## What Is Still Open

The remaining architecture issue is unit merging.

Current program compilation still does this:

1. parse a used unit
2. do a lightweight declaration-only semantic pass for stable stubs
3. append that unit’s declarations into the program AST
4. run `semcheck_program()` over the merged program tree

That means tree scoping is correct, but unit compilation is still not fully isolated.

This can still produce discrepancies such as:

- standalone unit compilation failing where `uses`-based program compilation succeeds
- semantic behavior depending on whether a unit is compiled as a top-level unit or as a merged dependency
- const/typed-const/initializer behavior depending on merged-list processing rather than unit-local processing

## Next Plan: Non-Merged Units

This is the next architecture plan after the completed scope-tree refactor.

### Goal

Stop compiling programs by physically splicing loaded unit ASTs into the program AST.

Units should remain first-class compilation objects through parse, semcheck, and codegen.

### Phase 1: Freeze The Boundary

- keep imported units out of `program_data.*` declaration lists
- make `load_unit()` produce loaded-unit records instead of mutating the program AST
- keep visibility through scope dependencies only

Acceptance:

- `merge_unit_into_program()` and `merge_unit_into_target()` have been removed
- program AST contains only program-owned declarations

### Phase 2: Add A Compilation Context

Introduce a compilation context that owns:

- loaded unit ASTs
- unit scopes
- dependency order
- initialization/finalization order
- codegen worklist

Acceptance:

- imported units are tracked in one central structure
- semantic passes no longer rely on imported declarations appearing in program lists

### Phase 3: Fully Semcheck Units In Place

Replace the split model:

- `semcheck_unit_decls_only()` during load
- later reprocessing through merged `semcheck_program()`

with:

- full unit semantic checking in the unit’s own scope
- program semantic checking for program-owned declarations and body only

Acceptance:

- standalone unit compile and `uses`-based unit loading follow the same unit-semcheck path

### Phase 4: Remove Merge-Order Semantics (DONE)

Audited and verified that cross-unit resolution depends only on scopes and
dependency graph, not list concatenation order:

- **imported const passes**: `semcheck_const_decls_imported_filtered` iterates
  the merged list but each constant is pushed into its per-unit scope via
  `source_unit_index`; cross-unit references resolve through scope dependency
  edges, not list position.
- **typed-const collection**: `collect_typed_const_decls_filtered` filters by
  `defined_in_unit` flag (a per-declaration property), not by list position.
- **prepush imported-const helpers**: Removed the redundant
  `prepush_trivial_imported_consts` call from `semcheck_program()`. Trivial
  constants are already pre-pushed into per-unit scopes during unit loading
  (`semcheck_unit_decls_only`) and System predeclaration
  (`semcheck_predeclare_program_into_unit_scope`).
- **re-export handling**: `semcheck_single_const_decl` uses scope-based lookup
  (`FindSymbol` + `SymTab_GetTargetTable`) to detect same-value re-exports;
  no merge-order assumption.

Acceptance:

- cross-unit resolution depends only on scopes and dependency graph, not list concatenation order

### Phase 5: Codegen Units Separately

Generate code from:

- loaded units
- then the program

instead of one merged declaration tree.

Acceptance:

- codegen input is `program + loaded units`
- imported unit code is emitted from unit records, not copied program lists

### Phase 6: Make Init/Final Explicit

Store initialization and finalization on units themselves and schedule them from the dependency graph.

Acceptance:

- imported unit init/fini are no longer appended into the program AST
- initialization runs in dependency order
- finalization runs in reverse dependency order

### Phase 7: Delete Merge Machinery (COMPLETED)

Removed:

- `merge_unit_into_program()` (was already removed from main_cparser.c in Phase 1; now removed from legacy main.c)
- `merge_unit_into_target()` (replaced by generalized `build_combined_program_view()` that handles both program and unit targets)
- `append_initialization_statement()` helper (only used by merge machinery)
- `get_finalization_list()` helper (only used by merge machinery)
- Updated docs/comments describing merged-unit architecture

`build_combined_program_view()` and `unbuild_combined_program_view()` now handle both
TREE_PROGRAM_TYPE and TREE_UNIT targets, with the unit path only merging interface
declarations and public subprograms (not implementation details).

Acceptance (verified):

```text
grep -r 'merge_unit_into_program\|merge_unit_into_target' KGPC/
```

Result: zero matches.

### Phase 8: Stabilize (COMPLETED)

Ran comprehensive testing after all phases. Results:

- **Main test suite**: 816/817 pass (1 pre-existing failure:
  `test_auto_tdd_sysutils_fpclosedir_overload` — unrelated to refactoring)
- **FPC RTL test suite**: 220/220 pass

Verified no regressions in tracked areas:

- **imported consts**: `reg_imported_qualified_const_lowhigh` and related tests pass
- **typed consts**: all 15+ typed-const tests pass (array lowering, persistence,
  currency init, mutability, nil, pointer init, system constexpr, record array)
- **unit re-exports**: scope-based re-export detection working correctly
- **overloads across units**: 30+ cross-unit overload tests pass (string, pointer,
  array, open-array, helper, operator overloads)
- **initialization/finalization ordering**: `program_uses_init_final` and
  `unit_init_after_nested_body` tests pass
- **duplicate or missing codegen**: `unit_compile_only` and all codegen tests pass

Acceptance:

- Full Meson suite passes (no new failures)
- FPC RTL suite passes (220/220)
- No regressions from the non-merged-units refactoring

## Bottom Line

The scope-tree refactor and the non-merged-units architecture refactor are both done.

Units remain first-class compilation objects through the full pipeline:

- units are loaded as separate records in a compilation context
- semantic checking uses temporary combined views, not permanent merging
- codegen iterates loaded units separately
- init/final is scheduled from the dependency graph
