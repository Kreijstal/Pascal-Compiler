# Complete Legacy Type System Migration Plan

## Objective
Remove all legacy type fields from HashNode and related structures, ensuring GpcType is the single source of truth for all type information.

## User Requirements
1. **NO pointer heuristics** - assertions MUST crash the compiler
2. **NO fallbacks or defensive programming** - if a fallback seems needed, it's actually a case that needs assertions
3. **Fix all unrelated test failures** - it's our responsibility
4. **Be excellent, not minimal** - find the BEST solution

## Current State Analysis

### Legacy Fields to Remove from HashNode
- `enum VarType var_type` (46 usages: 18 writers, 28 readers)
- `struct RecordType *record_type` (93 usages: 28 writers, 65 readers)
- `struct TypeAlias *type_alias` (62 usages: 32 writers, 30 readers)
- `int is_array` (multiple usages across HashNode)
- `int array_start` 
- `int array_end`
- `int element_size`
- `int is_dynamic_array`

### Related Structures (NOT removing these - they serve different purposes)
- `struct TypeAlias` - represents Pascal type aliases, keep as-is
- `struct RecordField` - has its own array fields, keep as-is
- `struct Expression` - has type resolution fields, may need GpcType support

### Fallback Patterns to Eliminate
Examples of defensive code to remove:
```c
// BEFORE (defensive):
int is_array = (node->type != NULL) ? gpc_type_is_array(node->type) : node->is_array;

// AFTER (assertive):
assert(node->type != NULL && "GpcType must be populated for this node");
int is_array = gpc_type_is_array(node->type);
```

## Migration Strategy

### Phase 1: Ensure GpcType Population (Days 1-2)
**Goal:** Every HashNode that needs type info MUST have a valid GpcType

#### 1.1: Audit GpcType creation points
- [x] Identify all places where HashNodes are created
- [ ] Verify GpcType is created for each case
- [ ] Document cases where GpcType cannot be created (if any)

#### 1.2: Add GpcType creation for missing cases
- [ ] Check all legacy API calls (AddIdentToTable_Legacy, PushVarOntoScope, etc.)
- [ ] Convert to _Typed APIs or create GpcType on the spot
- [ ] Special cases: builtins, constants, enums

#### 1.3: Stop populating legacy fields from GpcType
- [ ] Modify set_var_type_from_gpctype() to NOT set legacy fields
- [ ] Remove array field initialization in AddIdentToTable
- [ ] Add assertions that GpcType is present

### Phase 2: Migrate Type Query Operations (Days 3-4)
**Goal:** All type queries use GpcType exclusively

#### 2.1: Remove fallback patterns in SemCheck_expr.c
- [ ] Find all `(node->type != NULL) ? gpc_X(node->type) : node->legacy_field` patterns
- [ ] Replace with assertion + GpcType query
- [ ] List:
  - Line 1384: is_array fallback
  - Line 1388: is_dynamic fallback
  - Line 1400+: element_size fallback
  - Line 525: codegen_expression.c is_array fallback

#### 2.2: Migrate var_type readers (28 locations)
- [ ] sizeof_from_var_type() calls → gpc_type_sizeof()
- [ ] var_type == HASHVAR_X checks → gpc_type_get_primitive_tag() or kind checks
- [ ] Switch statements on var_type → equivalent GpcType logic
- [ ] Special handling for HASHVAR_ENUM, HASHVAR_SET, etc.

#### 2.3: Migrate record_type readers (65 locations)
- [ ] record_type != NULL checks → gpc_type_is_record() + gpc_type_get_record()
- [ ] sizeof_from_record() calls → gpc_type_sizeof()
- [ ] record field access → use gpc_type_get_record()

#### 2.4: Migrate type_alias readers (30 locations)
- [ ] Design: Should TypeAlias be integrated into GpcType or remain separate?
  - Option A: Add TYPE_KIND_ALIAS to GpcTypeKind
  - Option B: Keep TypeAlias separate, store pointer in GpcType union
  - **Decision: Option B** - TypeAlias is complex, adding a pointer is simpler

- [ ] Add type_alias pointer to GpcType union
- [ ] Migrate all type_alias accesses to query through GpcType

### Phase 3: Handle Special Cases (Day 5)
**Goal:** Address edge cases and complex scenarios

#### 3.1: Expression type resolution
- [ ] Review Expression struct fields (resolved_type, record_type, etc.)
- [ ] Consider adding GpcType *resolved_gpc_type field
- [ ] Migrate expression type checking to use GpcType

#### 3.2: TypeAlias and RecordField array info
- [ ] These structures have their own is_array, array_start fields
- [ ] They represent type DEFINITIONS, not symbol table entries
- [ ] Keep as-is (they're part of the type system, not migration targets)

#### 3.3: Builtins and constants
- [ ] Create GpcTypes for builtin procedures/functions
- [ ] Create GpcTypes for builtin types (Integer, Real, etc.)
- [ ] Handle constant type inference

### Phase 4: Migrate Writers (Days 6-7)
**Goal:** Stop writing to legacy fields

#### 4.1: Remove legacy field writes in HashTable.c
- [ ] set_var_type_from_gpctype() - remove all legacy field assignments
- [ ] AddIdentToTable() - remove array field initialization
- [ ] Add assertions that fields should not be used

#### 4.2: Remove legacy field writes in SemCheck.c
- [ ] Convert enum value assignments to GpcType operations
- [ ] Convert record_type assignments to GpcType
- [ ] Convert type_alias assignments to GpcType

#### 4.3: Remove legacy field writes in SymTab.c
- [ ] Update PushArrayOntoScope to NOT set legacy array fields
- [ ] Ensure all operations create proper GpcType

### Phase 5: Code Generator Migration (Days 8-9)
**Goal:** Codegen uses only GpcType

#### 5.1: Migrate codegen.c
- [ ] var_type checks → GpcType queries
- [ ] record_type accesses → gpc_type_get_record()
- [ ] type_alias accesses → GpcType queries

#### 5.2: Migrate codegen_expression.c
- [ ] Remove fallback patterns
- [ ] sizeof calculations → gpc_type_sizeof()
- [ ] Type checks → GpcType kind checks

#### 5.3: Migrate codegen_statement.c
- [ ] Any var_type or record_type usage → GpcType

### Phase 6: Remove Legacy Code (Day 10)
**Goal:** Delete all legacy type system code

#### 6.1: Remove legacy fields from HashNode
- [ ] Delete var_type field
- [ ] Delete record_type field
- [ ] Delete type_alias field
- [ ] Delete is_array, array_start, array_end, element_size, is_dynamic_array

#### 6.2: Remove legacy API functions
- [ ] Delete AddIdentToTable_Legacy()
- [ ] Delete set_var_type_from_gpctype()
- [ ] Delete primitive_tag_to_var_type() if no longer needed
- [ ] Delete PushVarOntoScope() (non-_Typed version)
- [ ] Delete PushArrayOntoScope() (non-_Typed version)
- [ ] Delete PushFunctionOntoScope() (non-_Typed version)
- [ ] Delete PushProcedureOntoScope() (non-_Typed version)
- [ ] Delete PushTypeOntoScope() (non-_Typed version)

#### 6.3: Remove enum VarType if completely unused
- [ ] Check if VarType enum is still needed anywhere
- [ ] If only used for translation, keep minimal subset
- [ ] Otherwise, delete entire enum

### Phase 7: Testing and Validation (Day 11)
**Goal:** Ensure everything works perfectly

#### 7.1: Build and test iteratively
- [ ] Build after each major change
- [ ] Run test suite after each phase
- [ ] Fix any regressions immediately

#### 7.2: Memory leak check
- [ ] Run valgrind on test suite
- [ ] Ensure no new leaks introduced
- [ ] Verify proper GpcType cleanup

#### 7.3: Performance validation
- [ ] Compare compilation times before/after
- [ ] Check for any performance regressions
- [ ] Profile if needed

### Phase 8: Documentation (Day 12)
**Goal:** Update all documentation

#### 8.1: Update inline comments
- [ ] Remove comments about legacy fields
- [ ] Add comments about GpcType requirements
- [ ] Document assertion expectations

#### 8.2: Update documentation files
- [ ] Update LEGACY_MIGRATION_STATUS.md
- [ ] Create GPCTYPE_GUIDE.md explaining type system
- [ ] Update main README if needed

## Implementation Notes

### When to Use Assertions
```c
// Use assertion when GpcType MUST be available
assert(node->type != NULL && "GpcType required for type query");

// Use assertion to detect programmer errors
assert(type->kind == TYPE_KIND_ARRAY && "Expected array type");
```

### GpcType Helper Function Additions Needed
May need to add:
- `gpc_type_from_type_alias()` - create GpcType from TypeAlias
- `gpc_type_get_type_alias()` - get TypeAlias from GpcType (if we go with Option B)
- `gpc_type_is_dynamic()` - check if type is dynamic/incomplete
- Additional helpers as identified during migration

### Error Handling Strategy
- Compilation errors (user code issues) → print error, return failure
- Programmer errors (missing GpcType) → assert and crash
- Never silently fallback to legacy fields

## Risk Assessment

### High Risk Areas
1. **Builtins** - may not have GpcType, need to create
2. **Type Aliases** - complex nested types, need careful handling
3. **Expressions** - may need new GpcType field
4. **Code Generator** - lots of type queries, high impact if wrong

### Mitigation Strategies
1. **Incremental testing** - test after each file/function
2. **Comprehensive assertions** - crash early when assumptions violated
3. **Clear commit messages** - easy to bisect if issues arise
4. **Keep legacy code temporarily** - don't delete until migration complete

## Success Criteria
- ✅ All 79+ compiler tests passing
- ✅ No memory leaks (valgrind clean)
- ✅ No fallback patterns remaining
- ✅ All legacy fields removed from HashNode
- ✅ Build with no warnings
- ✅ Documentation updated
- ✅ Code is cleaner and more maintainable
- ✅ Single source of truth: GpcType

## Timeline Estimate
- **Total: 12 days** of focused work
- Can be parallelized in some areas
- May need adjustment based on discoveries during migration
