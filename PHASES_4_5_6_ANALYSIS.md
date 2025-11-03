# Phases 4-6 Analysis and Planning

## Executive Summary

Phases 4-6 represent the final stages of the legacy type system migration. They are significantly more complex and risky than Phases 2-3 because they involve:
- Stopping all writes to legacy fields (breaking backward compatibility)
- Removing fallback logic (requiring GpcType to ALWAYS be present)
- Actually deleting legacy fields and APIs

**Estimated effort:** 5-7 days of careful, incremental work with extensive testing

## Prerequisites

Before starting Phases 4-6, we MUST complete:

### 1. Remaining Phase 2 Migrations (~85 locations)
- Migrate remaining type_alias accesses to use `get_type_alias_from_node()`
- Migrate remaining record_type accesses to use `get_record_type_from_node()`
- These are mechanical replacements but must be done carefully

### 2. Ensure 100% GpcType Coverage
Current gaps:
- Line 421 in SemCheck.c: Type declarations without GpcType
- Line 777 in SemCheck.c: HASHVAR_UNTYPED procedure parameters (legitimately can't have GpcType)
- Line 896, 939 in SemCheck.c: Non-array type references preserving type_alias metadata

**Action needed:** Audit ALL HashNode creation sites and ensure GpcType is populated where possible.

### 3. Convert Fallback Logic to Assertions
Currently, helper functions try GpcType then fall back to legacy fields:
```c
if (node->type != NULL) {
    return gpc_type_is_array(node->type);
} else {
    return node->is_array;
}
```

**Must become:**
```c
assert(node->type != NULL && "GpcType must be populated");
return gpc_type_is_array(node->type);
```

## Phase 4: Migrate Writers - Detailed Plan

### 4.1: Remove Legacy Field Writes in HashTable.c

**Current state:** HashTable.c populates BOTH GpcType and legacy fields

**Goal:** Only populate GpcType; legacy fields left at default values

**Steps:**
1. Audit all places where legacy fields are set:
   - `hash_node->var_type = ...` (19 writes in HashTable.c)
   - `hash_node->record_type = ...` (2 writes in HashTable.c)  
   - `hash_node->type_alias = ...` (1 write in HashTable.c)
   - Array fields: is_array, array_start, array_end, element_size, is_dynamic_array

2. Modify `set_var_type_from_gpctype()`:
   - Remove ALL legacy field assignments
   - Add assertions that these fields should NOT be written
   
3. Modify `create_hash_node()`:
   - When GpcType is provided, do NOT populate legacy fields
   - Leave them at default values (0/NULL)
   
4. Add debug assertions:
   ```c
   // After GpcType population
   assert(hash_node->var_type == HASHVAR_UNTYPED && "Should not write to legacy var_type");
   ```

**Risk:** HIGH - This breaks backward compatibility. Must ensure ALL readers use GpcType first.

**Testing:** Run ALL tests after each sub-step. Expect failures that reveal missing GpcType usage.

### 4.2: Remove Legacy Field Writes in SemCheck.c

**Current state:** SemCheck.c writes to legacy fields in multiple places:
- Line 263, 275, 411, 437, 448, 497: var_type writes for enums, sets
- Line 1087: Inferred type assignment
- Line 413, 415: type_alias and record_type for type declarations
- Lines 856, 888, 904, 950: Copying type metadata between nodes
- Lines 909, 955, 1285, 1308: Cloning record_type for function returns

**Goal:** All type information flows through GpcType

**Steps:**
1. For enum/set assignments (lines 263, 275, 437, 448, 497):
   - Create GpcType for enum/set
   - Use PushConstOntoScope_Typed() or equivalent
   
2. For type inference (line 1087):
   - Create GpcType for inferred type
   - Populate node->type instead of node->var_type
   
3. For type metadata copying:
   - Instead of `var_node->type_alias = type_node->type_alias`
   - Do: `gpc_type_set_type_alias(var_node->type, get_type_alias_from_node(type_node))`
   
4. For record_type cloning:
   - Instead of cloning to legacy field
   - Create new GpcType with cloned record

**Risk:** VERY HIGH - This touches semantic analysis core logic

**Testing:** Incremental - change one pattern, test, repeat

### 4.3: Remove Legacy Field Writes in SymTab.c

**Current state:** PushArrayOntoScope() sets legacy array fields

**Goal:** Only GpcType-based array creation

**Steps:**
1. Modify PushArrayOntoScope() to create GpcType only
2. Remove all legacy array field assignments
3. Ensure PushArrayOntoScope_Typed() is used everywhere

**Risk:** MEDIUM - Array handling is complex but localized

## Phase 5: Code Generator Migration

### 5.1: Migrate codegen.c (9 var_type usages)

**Locations:**
- Lines 778, 807, 811, 862: Type checks
- Lines 1158, 1193-1196: Function return type checks  
- Line 1541: File type check

**Strategy:**
- Replace `node->var_type == HASHVAR_X` with `gpc_type_is_X(node->type)`
- For primitive type checks, use `gpc_type_get_primitive_tag()`
- Add assertions that node->type != NULL

### 5.2: Migrate codegen_expression.c (4 usages)

**Already partially migrated** - sizeof functions prefer GpcType

**Remaining:**
- Lines 169, 548, 554: var_type checks
- Convert to use GpcType kind checks

### 5.3: Migrate codegen_statement.c

**Need to audit** - check for any var_type/record_type usage

## Phase 6: Remove Legacy Code

### 6.1: Remove Legacy Fields from HashNode

**ONLY after Phases 4-5 complete and ALL tests pass**

**Steps:**
1. Comment out fields in HashTable.h:
   ```c
   // DEPRECATED - use type->kind
   // enum VarType var_type;
   ```
2. Rebuild - expect compile errors
3. Fix each error by using GpcType instead
4. Once build succeeds, DELETE commented fields

**Fields to remove:**
- `enum VarType var_type`
- `struct RecordType *record_type`
- `struct TypeAlias *type_alias`
- `int is_array`
- `int array_start, array_end`
- `int element_size`
- `int is_dynamic_array`

### 6.2: Remove Legacy API Functions

**Safe to delete once no callers remain:**
- `AddIdentToTable_Legacy()`
- `set_var_type_from_gpctype()`
- `primitive_tag_to_var_type()` (if unused)
- Non-_Typed versions of Push functions

**Process:**
1. Search for each function name
2. Verify no callers exist
3. Delete function definition and declaration
4. Rebuild to confirm

### 6.3: Evaluate VarType Enum

**Options:**
1. Keep for translation (primitive_tag_to_var_type mapping)
2. Delete if completely unused
3. Minimal subset for special cases

**Decision criteria:**
- Is VarType used anywhere after Phase 5?
- Is there external code that depends on it?
- Can primitive_type_tag replace it completely?

## Risk Mitigation Strategies

### 1. Incremental Changes
- Change ONE file at a time
- Build and test after EACH change
- Commit working states frequently

### 2. Comprehensive Testing
- Run full test suite after every change
- Add new tests for edge cases
- Use valgrind to check for memory issues

### 3. Assertions Everywhere
```c
// Before reading legacy field
assert(node->type != NULL && "GpcType must be populated for this operation");

// After Phase 4 changes
assert(node->var_type == HASHVAR_UNTYPED && "Should not write to legacy var_type");
```

### 4. Feature Flags (Optional)
Consider adding a compile-time flag to enable/disable legacy system during transition:
```c
#ifdef USE_LEGACY_TYPE_SYSTEM
    return node->var_type;
#else
    assert(node->type != NULL);
    return gpc_type_get_primitive_tag(node->type);
#endif
```

### 5. Rollback Plan
- Each commit should be atomic and revertible
- Keep detailed notes of changes
- If tests fail, revert immediately and analyze

## Success Criteria

### Phase 4 Complete:
- [ ] No writes to var_type, record_type, type_alias in new code
- [ ] All HashNodes created with GpcType (except HASHVAR_UNTYPED)
- [ ] All tests passing
- [ ] Zero memory leaks (valgrind clean)

### Phase 5 Complete:
- [ ] Codegen uses ONLY GpcType queries
- [ ] No var_type comparisons in codegen
- [ ] All tests passing
- [ ] Performance unchanged (benchmark)

### Phase 6 Complete:
- [ ] Legacy fields deleted from HashNode
- [ ] Legacy APIs deleted
- [ ] VarType enum minimal or deleted
- [ ] All tests passing
- [ ] Documentation updated
- [ ] Code is cleaner and more maintainable

## Timeline Estimate

- **Prerequisites:** 1-2 days (complete Phase 2 migrations, ensure GpcType coverage)
- **Phase 4:** 2-3 days (careful incremental changes)
- **Phase 5:** 1-2 days (codegen is smaller than semantic checker)
- **Phase 6:** 1 day (mostly deletion once Phases 4-5 done)
- **Testing & Validation:** Throughout + 1 day final validation

**Total:** 5-9 days of focused, careful work

## Recommendation

**DO NOT attempt Phases 4-6 until:**
1. All Phase 2-3 mechanical migrations complete
2. GpcType coverage audited and maximized
3. Helper function fallbacks converted to assertions
4. Comprehensive test coverage verified

**Then proceed incrementally:**
- One file at a time
- One function at a time
- Test after every change
- Commit frequently

**Alternative approach if timeline is critical:**
- Focus on completing Phase 2-3 migrations (lower risk)
- Leave Phases 4-6 for future work
- Current state is functional and maintainable

## Files Requiring Changes

### Phase 4:
- GPC/Parser/SemanticCheck/HashTable/HashTable.c
- GPC/Parser/SemanticCheck/SemCheck.c  
- GPC/Parser/SemanticCheck/SymTab/SymTab.c

### Phase 5:
- GPC/CodeGenerator/Intel_x86-64/codegen.c
- GPC/CodeGenerator/Intel_x86-64/codegen_expression.c
- GPC/CodeGenerator/Intel_x86-64/codegen_statement.c

### Phase 6:
- GPC/Parser/SemanticCheck/HashTable/HashTable.h (delete fields)
- GPC/Parser/SemanticCheck/HashTable/HashTable.c (delete functions)
- GPC/Parser/SemanticCheck/SymTab/SymTab.c (delete functions)
- GPC/Parser/SemanticCheck/SymTab/SymTab.h (delete declarations)
