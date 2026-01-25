# Overload Resolution Refactoring Plan

## Current State: Technical Debt Analysis

### The Problem

The overload resolution in `SemCheck_expr.c` (lines ~13170-14180) uses a **scoring system with magic penalty numbers** instead of proper type-based resolution:

```c
current_score += 1000;  // "Mismatch"
current_score += 10;    // "Untyped params"
current_score += 5;     // "UnicodeString penalty"
current_score += 3;     // "String type penalty"
current_score += 2;     // "Integer widening"
current_score += 1;     // "Char to string"
pointer_penalty = 1000; // "Pointer subtype mismatch"
```

This is **not how Pascal overload resolution works**. These are ad-hoc heuristics compensating for an incomplete type system.

### Root Cause

The codebase has **two parallel type systems**:

| System | Location | Purpose |
|--------|----------|---------|
| Legacy type tags | `type_tags.h` | Integers like `INT_TYPE=1`, `POINTER_TYPE=29` |
| KgpcType | `KgpcType.h` | Proper type structures with `kind`, `points_to`, etc. |

The overload resolution mixes both, falling back to string comparisons ("PAnsiChar" vs "PWideChar") when KgpcType is unavailable.

### Symptoms

1. **strlen ambiguity**: `strlen(p)` where `p: PWideChar` can't distinguish between `strlen(PAnsiChar)` and `strlen(PWideChar)` overloads
2. **Parameter type resolution**: Function parameters sometimes resolve to `BUILTIN_ANY_TYPE` (5) instead of their declared type
3. **Inconsistent pointer handling**: 200+ lines of code trying to extract and compare pointer subtypes via string IDs

## How Pascal Overload Resolution Should Work

### FPC/Delphi Rules (Simplified)

1. **Exact match wins**: If argument type exactly matches parameter type, that overload wins
2. **Implicit conversions ranked**: Integer→Int64 < Integer→Real < Char→String
3. **No match = error**: If no overload accepts the arguments, it's a compile error
4. **Ambiguous = error**: If multiple overloads match equally well, it's a compile error

### Type Compatibility Categories

```
EXACT_MATCH     = 0   // Same type identity
IMPLICIT_SAFE   = 1   // Byte→Integer, Char→String (no data loss)
IMPLICIT_WIDEN  = 2   // Integer→Int64, Single→Double
IMPLICIT_NARROW = 3   // Int64→Integer (potential data loss, warning)
INCOMPATIBLE    = -1  // Cannot convert
```

## Refactoring Plan

### Phase 1: Audit KgpcType Coverage (1-2 hours)

**Goal**: Identify where KgpcType is NULL or incomplete.

#### Step 1.1: Add diagnostic logging

```c
// In semcheck_decls() when processing parameters
if (getenv("KGPC_AUDIT_TYPES") != NULL) {
    fprintf(stderr, "[TYPE_AUDIT] var=%s type_id=%s kgpc_type=%p\n",
        id, type_id, type_node ? type_node->type : NULL);
    if (type_node && type_node->type)
        fprintf(stderr, "  kind=%d\n", type_node->type->kind);
}
```

#### Step 1.2: Run audit on sysutils

```bash
KGPC_AUDIT_TYPES=1 ./build/KGPC/kgpc ./FPCSource/rtl/unix/sysutils.pp ... 2>&1 | grep "kgpc_type=\(nil\|0x0\)"
```

#### Step 1.3: Document gaps

Create a list of type aliases that don't have proper KgpcType:
- [ ] PAnsiChar
- [ ] PWideChar
- [ ] PChar
- [ ] etc.

### Phase 2: Fix Type Alias Registration (2-3 hours)

**Goal**: Ensure all type aliases have complete KgpcType.

#### Step 2.1: Fix pointer type aliases

In `semcheck_type_decls()` or wherever type aliases are processed:

```c
// When processing: type PWideChar = ^WideChar;
if (alias->is_pointer && alias->pointer_type_id != NULL) {
    KgpcType *points_to = resolve_type_by_id(symtab, alias->pointer_type_id);
    KgpcType *ptr_type = create_pointer_type(points_to);
    // Store in symbol table entry
    type_node->type = ptr_type;
}
```

#### Step 2.2: Verify with test

```pascal
program test_pointer_types;
var
  p: PWideChar;
  q: PAnsiChar;
begin
  p := nil;
  q := nil;
  writeln(strlen(p));  // Should resolve to PWideChar overload
  writeln(strlen(q));  // Should resolve to PAnsiChar overload
end.
```

### Phase 3: Add Type Comparison API (2 hours)

**Goal**: Centralize type comparison in KgpcType module.

#### Step 3.1: Add to KgpcType.h

```c
/**
 * Compare two types for identity.
 * Returns 1 if types are identical, 0 otherwise.
 */
int kgpc_type_equals(KgpcType *a, KgpcType *b);

/**
 * Check if 'from' type can be converted to 'to' type.
 * Returns:
 *   -1: Incompatible (no conversion possible)
 *    0: Exact match (same type)
 *    1: Implicit safe conversion (no data loss)
 *    2: Implicit widening conversion
 *    3: Implicit narrowing conversion (warning)
 */
int kgpc_type_conversion_rank(KgpcType *from, KgpcType *to);

/**
 * Check if two pointer types are compatible.
 * Pointers are compatible if they point to the same type,
 * or one is a generic pointer (Pointer type).
 */
int kgpc_type_pointers_compatible(KgpcType *ptr_a, KgpcType *ptr_b);
```

#### Step 3.2: Implement in KgpcType.c

```c
int kgpc_type_equals(KgpcType *a, KgpcType *b) {
    if (a == b) return 1;  // Same pointer
    if (a == NULL || b == NULL) return 0;
    if (a->kind != b->kind) return 0;

    switch (a->kind) {
        case TYPE_KIND_PRIMITIVE:
            return a->info.primitive_type_tag == b->info.primitive_type_tag;

        case TYPE_KIND_POINTER:
            return kgpc_type_equals(a->info.points_to, b->info.points_to);

        case TYPE_KIND_ARRAY:
            return kgpc_type_equals(a->info.array_info.element_type,
                                   b->info.array_info.element_type) &&
                   a->info.array_info.start_index == b->info.array_info.start_index &&
                   a->info.array_info.end_index == b->info.array_info.end_index;

        case TYPE_KIND_RECORD:
            // Records are equal only if they're the same record type
            return a->info.record_info == b->info.record_info;

        case TYPE_KIND_PROCEDURE:
            // Compare return type and parameter types
            return kgpc_type_procedure_signatures_equal(a, b);

        default:
            return 0;
    }
}

int kgpc_type_conversion_rank(KgpcType *from, KgpcType *to) {
    if (kgpc_type_equals(from, to)) return 0;  // Exact match

    if (from == NULL || to == NULL) return -1;

    // Primitive conversions
    if (from->kind == TYPE_KIND_PRIMITIVE && to->kind == TYPE_KIND_PRIMITIVE) {
        int from_tag = from->info.primitive_type_tag;
        int to_tag = to->info.primitive_type_tag;

        // Integer promotions
        if (is_integer_type(from_tag) && is_integer_type(to_tag)) {
            int from_size = kgpc_type_sizeof(from);
            int to_size = kgpc_type_sizeof(to);
            if (to_size > from_size) return 2;  // Widening
            if (to_size < from_size) return 3;  // Narrowing
            return 1;  // Same size, different signedness
        }

        // Char to String
        if (from_tag == CHAR_TYPE && is_string_type(to_tag))
            return 1;

        // Integer to Real
        if (is_integer_type(from_tag) && to_tag == REAL_TYPE)
            return 2;
    }

    // Pointer conversions
    if (from->kind == TYPE_KIND_POINTER && to->kind == TYPE_KIND_POINTER) {
        // Generic pointer (Pointer type) accepts any pointer
        if (to->info.points_to == NULL) return 1;
        // Exact pointer match
        if (kgpc_type_equals(from->info.points_to, to->info.points_to))
            return 0;
        return -1;  // Different pointer types are incompatible
    }

    return -1;  // Incompatible
}
```

### Phase 4: Replace Scoring Logic (3-4 hours)

**Goal**: Replace magic numbers with type-based resolution.

#### Step 4.1: Define overload candidate structure

```c
typedef struct {
    HashNode_t *function;
    int valid;           // 1 if this overload can accept the arguments
    int total_rank;      // Sum of conversion ranks for all arguments
    int exact_matches;   // Count of exact type matches
} OverloadCandidate;
```

#### Step 4.2: New resolution algorithm

```c
static HashNode_t *resolve_overload(
    ListNode_t *candidates,
    ListNode_t *arguments,
    SymTab_t *symtab,
    int *error_code)  // 0=ok, 1=no match, 2=ambiguous
{
    OverloadCandidate *results = NULL;
    int num_candidates = ListLength(candidates);
    int num_valid = 0;

    results = calloc(num_candidates, sizeof(OverloadCandidate));

    // Score each candidate
    int i = 0;
    for (ListNode_t *c = candidates; c != NULL; c = c->next, i++) {
        HashNode_t *func = (HashNode_t *)c->cur;
        results[i].function = func;
        results[i].valid = 1;
        results[i].total_rank = 0;
        results[i].exact_matches = 0;

        ListNode_t *params = kgpc_type_get_procedure_params(func->type);
        ListNode_t *args = arguments;

        while (params != NULL && args != NULL) {
            KgpcType *param_type = get_param_kgpc_type(params->cur, symtab);
            KgpcType *arg_type = resolve_expr_kgpc_type(args->cur, symtab);

            int rank = kgpc_type_conversion_rank(arg_type, param_type);

            if (rank < 0) {
                results[i].valid = 0;
                break;
            }

            results[i].total_rank += rank;
            if (rank == 0) results[i].exact_matches++;

            params = params->next;
            args = args->next;
        }

        // Check argument count
        if (params != NULL && !has_default_value(params))
            results[i].valid = 0;  // Too few arguments
        if (args != NULL)
            results[i].valid = 0;  // Too many arguments

        if (results[i].valid) num_valid++;
    }

    // Find best match
    if (num_valid == 0) {
        *error_code = 1;  // No matching overload
        free(results);
        return NULL;
    }

    // Sort by: most exact matches, then lowest total rank
    OverloadCandidate *best = NULL;
    int num_best = 0;

    for (int j = 0; j < num_candidates; j++) {
        if (!results[j].valid) continue;

        if (best == NULL) {
            best = &results[j];
            num_best = 1;
        } else if (results[j].exact_matches > best->exact_matches ||
                   (results[j].exact_matches == best->exact_matches &&
                    results[j].total_rank < best->total_rank)) {
            best = &results[j];
            num_best = 1;
        } else if (results[j].exact_matches == best->exact_matches &&
                   results[j].total_rank == best->total_rank) {
            num_best++;
        }
    }

    if (num_best > 1) {
        *error_code = 2;  // Ambiguous
        free(results);
        return NULL;
    }

    HashNode_t *result = best->function;
    free(results);
    *error_code = 0;
    return result;
}
```

#### Step 4.3: Replace existing code

In `semcheck_funccall()`:
- Remove all `current_score +=` lines
- Remove `pointer_penalty` logic
- Replace with call to `resolve_overload()`

### Phase 5: Testing and Validation (2 hours)

#### Step 5.1: Create overload resolution test suite

```pascal
// tests/test_cases/overload_resolution.p
program test_overload;

// Integer overloads
procedure Foo(x: Byte); begin writeln('byte'); end;
procedure Foo(x: Integer); begin writeln('integer'); end;
procedure Foo(x: Int64); begin writeln('int64'); end;

// Pointer overloads
procedure Bar(p: PAnsiChar); begin writeln('ansi'); end;
procedure Bar(p: PWideChar); begin writeln('wide'); end;

var
  b: Byte;
  i: Integer;
  l: Int64;
  pa: PAnsiChar;
  pw: PWideChar;
begin
  Foo(b);   // Should call Byte version
  Foo(i);   // Should call Integer version
  Foo(l);   // Should call Int64 version
  Foo(42);  // Should call Integer version (literal)

  Bar(pa);  // Should call PAnsiChar version
  Bar(pw);  // Should call PWideChar version
end.
```

#### Step 5.2: Run regression tests

```bash
meson test -C build
```

Ensure all 507 existing tests pass.

#### Step 5.3: Test sysutils error count

```bash
./build/KGPC/kgpc ./FPCSource/rtl/unix/sysutils.pp ... 2>&1 | grep -c "Error on"
```

Should be ≤23 (current count), ideally fewer.

## Files to Modify

| File | Changes |
|------|---------|
| `KgpcType.h` | Add `kgpc_type_equals`, `kgpc_type_conversion_rank` |
| `KgpcType.c` | Implement type comparison functions |
| `SemCheck_expr.c` | Replace scoring logic (~300 lines removed, ~150 added) |
| `SemCheck.c` | Fix type alias registration for pointer types |

## Risk Assessment

| Risk | Mitigation |
|------|------------|
| Regression in overload resolution | Incremental testing after each phase |
| Missing KgpcType in edge cases | Phase 1 audit will identify gaps |
| Performance impact | Type comparison is O(depth), should be negligible |

## Success Criteria

1. No magic penalty numbers in overload resolution
2. All 507 existing tests pass
3. sysutils errors ≤23 (preferably reduced)
4. `strlen(PWideChar)` correctly resolves to PWideChar overload
5. Clean, auditable type comparison code

## Estimated Total Effort

| Phase | Hours |
|-------|-------|
| Phase 1: Audit | 1-2 |
| Phase 2: Fix type aliases | 2-3 |
| Phase 3: Type comparison API | 2 |
| Phase 4: Replace scoring | 3-4 |
| Phase 5: Testing | 2 |
| **Total** | **10-13 hours** |

## Future Improvements

After this refactoring:
- Generic type support will be easier to implement
- Operator overloading resolution can use the same infrastructure
- Type inference for `var` declarations becomes straightforward
