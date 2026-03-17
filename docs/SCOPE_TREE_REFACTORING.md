# Scope Tree Refactoring: Flat Stack to Parent-Pointer Tree

## Motivation

The current scope system uses a flat stack (`stack_head` linked list) shared by all method bodies, plus a bolt-on `unit_tables[256]` array for per-unit symbols. This causes:

- **Scope leaking**: Local variables, parameters, and implicit variables from one subprogram are visible inside sibling subprograms. Examples:
  - `x: uint64` (local in `Xoshiro128ss_32.Setup` in system.inc) shadows `TPointF.x` (field) in types.pp methods — **fixed with band-aid at `add_class_vars_to_method_scope_impl`**
  - `Output: TStream` (parameter of `ObjectBinaryToText` in classes.inc) shadows `Output: Text` (system global) inside `fpc_get_output` in text.inc — causes `format_function` compilation failure, **unfixable without tree scopes**
  - `Self` from a previously semchecked class method body leaks into standalone procedures like `Assign(t:Text, s:ShortString)` in text.inc. `semcheck_proccall` finds `Self` in scope, thinks the `Assign(t, AnsiString(s))` call is a method call on Self's class, and bypasses the normal overload resolution entirely. This causes the call to resolve to `assign_t_ss` (itself) instead of `assign_t_rbs` (the RawByteString overload), producing **infinite recursion at runtime**. Affects `crt_colour_list`, `fpc_bootstrap_assign_textrec_cast`, `fpc_bootstrap_system_qualified_proccall`, `fpc_settextcodepage`, `tdd_cp_acp_paramstr_ioresult` — all 5 crash with stack overflow in `assign_t_ss`.
- **Name mangling ordering dependency**: In the flat merge model, all units are merged then semchecked sequentially. ObjPas's `Integer = LongInt` override happens partway through. Call sites processed BEFORE the override mangle `Integer` parameters as `_i` (INT_TYPE = SmallInt). Function bodies processed AFTER mangle as `_li` (LONGINT_TYPE = LongInt). The symbols don't match → **link errors**. Example: `FileCreate(Filename: RawByteString; Rights: Integer)` is codegenned as `filecreate_rbs_li` but callers emit `call filecreate_rbs_i`. Affects `adfgvxcipher`, `class_field_offset_tstringlist`, `tdd_varparam_tstringlist_filter`. With tree scopes, each unit resolves types in its own scope — a unit that `uses ObjPas` sees `Integer = LongInt` from the start, no ordering dependency.
- **Workaround proliferation**: `source_unit_index` checks, `unit_context` routing, `push_target_unit` save/restore (~40 sites), `add_class_vars` FindIdent hacks
- **Unnatural complexity**: The flat stack fights against Pascal's inherently tree-structured scoping

A tree just mirrors the source code structure. It simplifies the codebase by removing workarounds, not adding complexity.

**Priority**: FPC RTL tests take priority over normal tests. The goal is all 208 FPC RTL tests passing.

## Current Architecture

```
SymTab_t:
  stack_head → [scope0] → [scope1] → [scope2] → ...   (flat linked list)
  unit_tables[256]                                       (bolt-on per-unit tables)
  builtins                                               (single hash table)
  unit_context                                           (which unit we're checking)
  push_target_unit                                       (routing hack)
```

`FindIdent` walks: stack → all accessible unit_tables → builtins. Everything sees everything.

## Target Architecture

```
builtins (root)
  ├── system.pp unit scope
  │     ├── Xoshiro128ss_32.Setup scope (parent → system)
  │     └── ... other system methods
  ├── types.pp unit scope (deps: [system])
  │     ├── TPointF.MidPoint scope (parent → types)
  │     │     └── nested block (parent → MidPoint)
  │     └── TRectF.CenterAt scope (parent → types)
  ├── sysutils.pp unit scope (deps: [system, types])
  └── program scope (deps: [system, sysutils, ...])
        └── main block scope
```

`FindIdent` walks: current → parent → parent → ... → builtins. At unit scopes, also checks `dep_scopes[]`. Never sees siblings or cousins.

## A. Data Structure Changes

### New `ScopeNode` struct (SymTab.h)

```c
typedef enum ScopeKind {
    SCOPE_BUILTIN,      /* Root: compiler builtins */
    SCOPE_UNIT,         /* Unit-level scope (one per unit) */
    SCOPE_PROGRAM,      /* Program-level scope (globals) */
    SCOPE_SUBPROGRAM,   /* Procedure/function body */
    SCOPE_BLOCK,        /* Nested block: try-except, with, anonymous method */
} ScopeKind;

typedef struct ScopeNode {
    HashTable_t *table;          /* Symbols declared in this scope */
    struct ScopeNode *parent;    /* Walk this chain for FindIdent */
    ScopeKind kind;
    int unit_index;              /* Which unit this scope belongs to (0 = program) */

    /* For SCOPE_UNIT: dependency edges from `uses` clause */
    struct ScopeNode **dep_scopes;
    int num_deps;
    int cap_deps;
} ScopeNode;
```

### Modified `SymTab_t`

```c
typedef struct SymTab
{
    ScopeNode *builtin_scope;                /* Root of the tree */
    ScopeNode *current_scope;                /* Active scope — all lookups start here */
    ScopeNode *unit_scopes[SYMTAB_MAX_UNITS]; /* O(1) lookup by unit index */

    /* DEPRECATED (kept during migration, removed at end) */
    ListNode_t *stack_head;
    HashTable_t *builtins;
    int unit_context;
    int push_target_unit;
    HashTable_t *unit_tables[SYMTAB_MAX_UNITS];
} SymTab_t;
```

## B. API Changes

### New functions

```c
ScopeNode *CreateScope(ScopeKind kind, ScopeNode *parent, int unit_index);
void DestroyScope(ScopeNode *scope);

ScopeNode *GetOrCreateUnitScope(SymTab_t *symtab, int unit_index, ScopeNode *parent);
void ScopeAddDependency(ScopeNode *unit_scope, ScopeNode *dep_scope);

void EnterScope(SymTab_t *symtab, ScopeKind kind, int unit_index);
void LeaveScope(SymTab_t *symtab);
```

### Simplified existing functions

```c
/* FindIdent — same signature, walks parent chain internally */
int FindIdent(HashNode_t **hash_return, SymTab_t *symtab, const char *id);

/* SymTab_GetTargetTable — becomes trivial */
static HashTable_t *SymTab_GetTargetTable(SymTab_t *symtab)
{
    return symtab->current_scope->table;
}
```

### Functions removed

- `FindIdentInUnit` — absorbed into FindIdent via tree walk
- `FindIdentInAnyUnitTable` — no longer needed
- `FindAllIdentsInAnyUnitTable` — no longer needed
- `PushScope` / `PopScope` — replaced by EnterScope / LeaveScope
- `get_or_create_system_unit_table` — replaced by GetOrCreateUnitScope

## C. FindIdent Rewrite

```c
/* Returns true (1) if found, false (0) if not.
 * Symbol is returned via *hash_return.
 * Optional found_kind reports which scope kind contained the symbol. */
bool FindIdent(HashNode_t **hash_return, SymTab_t *symtab, const char *id)
{
    ScopeNode *scope = symtab->current_scope;

    while (scope != NULL)
    {
        HashNode_t *node = FindIdentInTable(scope->table, id);
        if (node != NULL)
        {
            *hash_return = node;
            return 1;
        }

        /* At unit scope, also search dependency unit scopes */
        if (scope->kind == SCOPE_UNIT)
        {
            for (int i = 0; i < scope->num_deps; i++)
            {
                node = FindIdentInTable(scope->dep_scopes[i]->table, id);
                if (node != NULL)
                {
                    *hash_return = node;
                    return 1;
                }
            }
        }

        scope = scope->parent;
    }

    *hash_return = NULL;
    return 0;
}
```

Unit dependencies are flat (not transitive) — Pascal's `uses` gives direct visibility only.

## D. Migration Plan

### Phase 1: Add infrastructure alongside existing code (no behavioral change)

1. Add `ScopeNode` struct and `CreateScope`/`DestroyScope` to SymTab.h/c
2. Add `builtin_scope`, `current_scope`, `unit_scopes[]` to `SymTab_t`, initialize to NULL
3. Implement `EnterScope`/`LeaveScope` as wrappers that also call `PushScope`/`PopScope`

Tests pass — no behavioral change.

### Phase 2: Shadow FindIdent with tree-walking path

4. Implement `FindIdent_Tree` as a parallel implementation
5. Gate with `KGPC_SCOPE_TREE=1` env var — run all tests with both paths, diff results
6. Fix discrepancies until both paths agree

### Phase 3: Switch over

7. Make FindIdent use tree path unconditionally
8. Replace 28 PushScope/PopScope sites with EnterScope/LeaveScope
9. Remove ~40 `unit_context`/`push_target_unit` save/restore patterns

### Phase 4: Cleanup — zero legacy flat scoping

The goal is **complete removal** of the flat scope system. No fallbacks, no parallel paths, no "just in case" code. The tree is the only scope mechanism.

10. **Remove `stack_head` from `SymTab_t`.** This is the flat linked list. Delete the field, then fix every compile error — each one is a site that was still using the flat path. If something breaks, the fix is to use `current_scope`, not to keep `stack_head`.

11. **Remove `unit_tables[256]` from `SymTab_t`.** Replace all references with `unit_scopes[i]->table`. The `unit_scopes[]` array already provides the same O(1) lookup.

12. **Remove `builtins` field from `SymTab_t`.** It becomes `builtin_scope->table`. All `FindIdentInTable(symtab->builtins, ...)` calls become unnecessary since the tree walk already reaches `builtin_scope`.

13. **Remove `unit_context` and `push_target_unit` fields.** These are routing hacks for the flat model. With the tree, `current_scope` already knows which unit it belongs to (`current_scope->unit_index` or walk to nearest `SCOPE_UNIT` ancestor).

14. **Remove `PushScope` and `PopScope` functions entirely.** Not deprecated, not hidden — deleted. `EnterScope`/`LeaveScope` are the only scope manipulation API.

15. **Remove `FindIdentInUnit`, `FindIdentInAnyUnitTable`, `FindAllIdentsInAnyUnitTable`.** These flat-model helpers have no purpose in the tree.

16. **Remove `SymTab_GetTargetTable` routing logic.** It becomes `return current_scope->table;` — no conditionals, no `push_target_unit` check.

17. **Remove all workarounds** listed in section E (add_class_vars hack, source_unit_index priority system, error suppression flags, etc.)

18. **Change `FindIdent` return type to `bool`.** "Depth" is meaningless in a tree — a symbol in a sibling unit has no depth relative to the current scope. The return value should be:
    - `0` (false): not found
    - `1` (true): found — the symbol is in `*hash_return`

    The current Phase 2 shim returns 0 for unit/builtin symbols to keep 43 `== 0` checks working. This is a hack. The real fix:
    - Change signature: `bool FindIdent(HashNode_t **hash_return, SymTab_t *symtab, const char *id)`
    - Change all 43 `== 0` checks to `!= 0` or just use the bool directly
    - Change all `== -1` checks to `== 0` or `!FindIdent(...)`
    - Change all `>= 0` checks to `!= 0`
    - Remove the depth shim entirely

    If a caller genuinely needs to know WHERE the symbol was found (e.g. "is this in my local scope?"), add an optional `ScopeKind *found_in` output parameter. This is cleaner than inferring scope from a numeric depth that has no stable meaning.

19. **Compile-time guarantee**: After this phase, `grep -r 'stack_head\|unit_tables\|push_target_unit\|unit_context\|PushScope\|PopScope\|FindIdentInUnit' KGPC/` returns **zero results**. This is the acceptance criterion — if any of these strings exist in the codebase, Phase 4 is not complete.

19. **Delete `UNIT_SCOPING_PLAN.md`** — it documents the old bolt-on unit_tables approach which is now fully superseded by the tree.

20. **Remove overload scoring heuristics.** `semcheck_resolve_overload` uses `char_promo_rank`, `MATCH_PROMOTION`, `MATCH_CONVERSION` and string-type-name-comparison heuristics to pick between overloads. These exist because the flat scope model couldn't structurally distinguish which overload belongs to which unit. With tree scoping, overloads resolve by scope membership — the correct overload is the one visible in the current scope chain, not the one with the best "score". Remove:
    - `char_promo_rank` field and all uses
    - String-type-name scoring in `semcheck_resolve_overload`
    - `MatchQualityKind` ranking beyond EXACT/INCOMPATIBLE
    - Any `strcasecmp(alias_name, "RawByteString")` type-name comparisons in scoring

## E. What Gets Removed

These workarounds become unnecessary with tree scoping:

| Workaround | Location | Why it exists |
|-----------|----------|---------------|
| `push_target_unit` save/restore | SemCheck.c (~40 sites) | Routes symbols to correct unit table |
| `unit_context` save/restore | SemCheck.c | Tells FindIdentInUnit which unit we're in |
| `FindIdentInUnit` | SymTab.c | Special-cases unit-aware lookup |
| `FindIdentInAnyUnitTable` | SymTab.c | Searches all unit tables as fallback |
| `add_class_vars` FindIdent hack | SemCheck.c:2762 | Prevents field injection when unit symbol collides |
| `source_unit_index` priority system | HashTable.c | Ranks matches by unit origin |
| `g_semcheck_error_suppress_source_index` | SemCheck.c | Suppresses errors for wrong-unit symbols |

## F. Risk Area: FindIdent Return Value

`FindIdent` returns scope depth (0 = current, 1 = parent, etc.). ~504 call sites. Most only check `-1` vs `>= 0`, which is safe. The risky pattern is depth comparison for `max_scope_lev`:

```c
FindIdent(&node, symtab, id) == 0    /* current scope — safe */
FindIdent(&node, symtab, id) >= 0    /* found somewhere — safe */
FindIdent(&node, symtab, id) > X     /* deeper than X — MEANING CHANGES */
```

~5-10 sites use numeric depth for `max_scope_lev` checks. These need audit — may need to switch to scope-kind checks instead of numeric depth.

## G. Scope Kind Mapping

| Scope kind | Created when | Parent |
|-----------|-------------|--------|
| `SCOPE_BUILTIN` | `InitSymTab()` | NULL |
| `SCOPE_UNIT` | Unit loaded | `builtin_scope` |
| `SCOPE_PROGRAM` | `semcheck_program` entry | `builtin_scope` |
| `SCOPE_SUBPROGRAM` | `semcheck_subprogram` entry | Unit or program scope |
| `SCOPE_BLOCK` | try-except, with, etc. | Enclosing subprogram/block |
