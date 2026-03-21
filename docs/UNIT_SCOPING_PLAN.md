# Unit Scoping Refactoring Plan

## 1. Current Architecture Overview

### 1.1 Unit Loading and Merging (AST Level)

Units are parsed into `Tree_t` with `type == TREE_UNIT`, then stored in the `CompilationContext` loaded-units array. Before semcheck, `build_combined_program_view()` temporarily moves unit declarations into the target tree's flat lists. Every moved node gets `defined_in_unit = 1` and `source_unit_index` set. After semcheck, `unbuild_combined_program_view()` restores declarations back to their unit records.

### 1.2 Semantic Analysis (Symbol Table Level)

`semcheck_program()` (`SemCheck.c:12151`) pushes ONE scope and processes ALL declarations from the merged flat lists into this single scope. `FindIdent()` (`SymTab.c:242`) walks scopes from innermost to outermost, returning the first match. Since everything is in one scope, insertion order within the hash bucket determines which same-named symbol wins.

### 1.3 Collision Handling

`check_collision_allowance()` (`HashTable.c:529`) allows overloading, type+proc sharing, const-shadows-const, etc. But var-shadows-var across units is NOT allowed, causing compilation failures when program vars collide with unit vars.

When collision IS allowed, `PushListNodeFront()` (`HashTable.c:151`) puts the newest entry at the front of the hash bucket, so the last-inserted entry wins lookups.

## 2. The Problem

All declarations land in a single scope. When a program declares `var Input: String`, it collides with System's `var Input: Text`. Even if the collision were allowed, unit implementation code (e.g., `EOF(Input)` inside the System unit) would also pick up the program's `Input: String` instead of the unit's `Input: Text`, because there's no scope separation.

```pascal
program MyProg;
var
  Input: String;  // Collides with System.Input: Text
begin
  Input := 'hello';  // ERROR: incompatible types (lhs: Text, rhs: String)
end.
```

In FPC, the program's `Input` shadows System's `Input` only within the program's own code. Each unit's implementation code always resolves to its own unit's declarations.

## 3. Proposed Solution: Unit-Tagged Symbol Resolution

### 3.1 Core Idea

Rather than restructuring the scope stack (which would require major changes to the scope push/pop model and codegen), **tag every symbol with its `source_unit_index`** and use unit-aware resolution:

- When resolving a name inside a **unit subprogram** (known `source_unit_index = U`), prefer symbols from the same unit `U`, then symbols from units in U's `uses` chain, then fall back to any match.
- When resolving a name inside **program code** (`source_unit_index = 0`), prefer program-local symbols (`source_unit_index = 0`), then unit symbols.
- This avoids restructuring the scope stack while achieving correct scoping semantics.

### 3.2 Why Unit Tagging Over Scope Levels

- **Minimal disruption**: No changes to `PushScope`/`PopScope` or the scope stack model
- **Codegen compatibility**: Codegen uses `FindIdent` extensively (181 call sites) and may assume scope level semantics; tagging doesn't change scope levels
- **Handles unit-to-unit shadowing**: Later `uses` units can shadow earlier ones via insertion order, with unit index as tiebreaker
- **Already partially implemented**: `source_unit_index` and `defined_in_unit` are already set on AST nodes and `HashNode_t`

## 4. Implementation Phases

### Phase 1: Ensure All HashNodes Have `source_unit_index`

**Goal**: Every `HashNode_t` in the symbol table carries the `source_unit_index` of its origin.

**Current state**: `HashNode_t` has `defined_in_unit` (bool) and `source_unit_index` fields. Check that `source_unit_index` is reliably set during `AddIdentToTable` / `semcheck_decls`.

**Files**:
- `KGPC/Parser/SemanticCheck/HashTable/HashTable.h` — verify `source_unit_index` field exists
- `KGPC/Parser/SemanticCheck/SemCheck.c` — in `semcheck_decls()`, after `AddIdent*`, propagate `source_unit_index` from the AST node to the `HashNode_t`

**Risk**: Low. Read-only audit + targeted field propagation.

### Phase 2: Add Unit-Aware FindIdent

**Goal**: Add `FindIdentInUnit(symtab, id, caller_unit_index)` that resolves names with unit-aware priority.

**Algorithm**:
```
FindIdentInUnit(symtab, id, caller_unit_index):
  1. Search all scopes (same as FindIdent)
  2. Collect ALL matches with the same canonical name
  3. Priority order:
     a. Same source_unit_index as caller (exact unit match)
     b. source_unit_index = 0 (program-local) — only if caller is program code
     c. Any other match (cross-unit, ordered by uses clause / insertion order)
  4. Return highest-priority match
```

**Files**:
- `KGPC/Parser/SemanticCheck/SymTab/SymTab.c` — add `FindIdentInUnit()`
- `KGPC/Parser/SemanticCheck/SymTab/SymTab.h` — declare it

**Risk**: Low. New function, doesn't modify existing `FindIdent`.

### Phase 3: Allow Var-Var Collisions in Hash Table

**Goal**: Allow multiple vars with the same name to coexist in the same hash bucket (like overloaded functions already do).

**Changes**:
- `KGPC/Parser/SemanticCheck/HashTable/HashTable.c` — in `check_collision_allowance()`, add:
  ```c
  /* Allow variables from different units to coexist.
   * Unit-aware FindIdent resolves which one is visible. */
  if ((existing_node->hash_type == HASHTYPE_VAR || existing_node->hash_type == HASHTYPE_ARRAY) &&
      (new_hash_type == HASHTYPE_VAR || new_hash_type == HASHTYPE_ARRAY) &&
      existing_node->source_unit_index != new_source_unit_index) {
      return 1;
  }
  ```

**Challenge**: `check_collision_allowance` currently doesn't receive the new entry's `source_unit_index`. Options:
  - Pass it as an additional parameter (requires API change)
  - Simply allow all var-var collisions (safe because `FindIdentInUnit` handles priority)

**Recommended**: Allow all var-var collisions. With unit-aware resolution, the correct var is always picked. Two program vars with the same name would still be caught by semcheck's duplicate-declaration check before reaching the hash table.

**Risk**: Medium. Needs thorough testing to ensure no spurious duplicate vars.

### Phase 4: Use `FindIdentInUnit` in Semcheck

**Goal**: Replace `FindIdent` calls in semcheck with `FindIdentInUnit` where the caller's unit context is known.

**Key locations**:
- `semcheck_subprogram()` (`SemCheck.c:14585+`) — knows `subprogram->tree_data.subprogram_data.source_unit_index`
- `semcheck_decls()` — knows `defined_in_unit` from the AST node
- `SemCheck_Expr_Access.c` — expression resolution during subprogram body checking
- `SemCheck_Expr_Resolve.c` — identifier resolution

**Approach**: Thread a `current_unit_index` through the semcheck context (or use a global, like the existing `g_semcheck_*` globals). When entering a unit subprogram body, set `current_unit_index = sub->source_unit_index`. When in program code, set it to 0.

**Files**:
- `KGPC/Parser/SemanticCheck/SemCheck.c` — set context before subprogram body check
- `KGPC/Parser/SemanticCheck/SemChecks/SemCheck_Expr_Access.c` — use `FindIdentInUnit` for identifier resolution
- `KGPC/Parser/SemanticCheck/SemChecks/SemCheck_Expr_Resolve.c` — same

**Risk**: Medium-high. Many call sites; each needs to correctly determine the caller's unit context.

### Phase 5: Use `FindIdentInUnit` in Codegen (if needed)

**Goal**: Ensure codegen resolves symbols correctly with unit tagging.

**Assessment**: Codegen runs AFTER semcheck. By semcheck time, each AST node's type has been resolved and the `mangled_id` set. Codegen primarily uses `FindIdent` for:
- Looking up record/class field layouts (`SemCheck_sizeof.c` functions)
- Resolving type info for casts and conversions
- Finding subprogram signatures for call generation

Most of these lookups are for types and subprograms, not vars. Variable access in codegen uses the mangled name directly from the AST, not from the symbol table.

**Likely outcome**: Codegen doesn't need changes. But audit `FindIdent` calls in `codegen.c`, `codegen_expression.c`, `codegen_statement.c` to verify.

**Risk**: Low if assessment is correct; medium if codegen does var lookups by name.

## 5. Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Scope stack vs unit tagging | Unit tagging | Less disruptive; codegen unchanged; `source_unit_index` already exists |
| Per-unit scopes vs shared unit scope | Shared scope + tagging | Per-unit scopes need N scope levels and complex visibility rules |
| Collision handling | Allow all var-var collisions | `FindIdentInUnit` resolves priority; simpler than threading unit index through collision check |
| Codegen changes | None expected | Codegen uses mangled names from AST, not symbol table var lookups |
| `uses` clause ordering | Preserved by insertion order | Later-inserted entries win in hash bucket; same as current behavior |
| Implementation-only symbols | No change (future work) | Unit `unit_is_public=0` symbols visible to program is a pre-existing issue |

## 6. Risk Areas

1. **Multi-pass const processing**: `semcheck_program()` has a 4-pass const scheme (`SemCheck.c:12170-12211`). The `_imported`/`_local` split already handles unit vs program consts. Unit tagging should be compatible.

2. **Generic specialization**: Cloned generic types/subprograms may need to inherit the `source_unit_index` of their instantiation site, not their template's origin.

3. **Type helpers and operator overloads**: These scan all overloads regardless of origin. `FindAllIdents` (used for overload resolution) should NOT be unit-filtered — all overloads must be visible.

4. **`FindAllIdents` vs `FindIdentInUnit`**: Overload resolution via `FindAllIdents` must return ALL candidates, then the overload resolver picks the best match. Only single-result lookups (`FindIdent`) need unit-aware filtering.

5. **Unit init/finalization code**: Merged into program but should resolve names in unit context. Need to tag these statement blocks with their `source_unit_index`.

## 7. Testing Strategy

### New Test Cases

1. **`tdd_unit_scope_var_shadow.p`**: Program `var Input: String` + System `Input: Text`. Verifies no collision error and correct type.
2. **`tdd_unit_scope_isolation.p`**: Unit procedure references its own var. Program declares var with same name. Unit procedure sees its own var.
3. **`case_char_range.p`** (existing): Should pass with FPC RTL after this refactoring.

### Regression Testing

```bash
# After each phase:
meson test -C build 'Compiler tests'           # 758+ tests
meson test -C build-fpc 'FPC RTL tests'         # Check for improvements
```

### Key Existing Tests to Watch

- `tdd_unit_scope_leak` — already tests unit scope isolation
- `tdd_types_self_field_shadow_import` — type shadowing across units
- All `fpc_bootstrap_*` tests — heavy unit interop

## 8. Implementation Order Summary

```
Phase 1: Audit/fix source_unit_index propagation to HashNode_t    [Low risk]
Phase 2: Add FindIdentInUnit() function                           [Low risk]
Phase 3: Allow var-var collisions in hash table                    [Medium risk]
Phase 4: Thread unit context through semcheck, use FindIdentInUnit [Medium-high risk]
Phase 5: Audit codegen (likely no changes needed)                  [Low risk]
```

Estimated scope: ~200-400 lines of new/modified code across 6-8 files. The bulk of the work is in Phase 4 (threading the unit context through semcheck's expression resolution).
