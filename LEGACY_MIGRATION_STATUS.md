# Legacy Type System Migration - Status Report

## Summary

Successfully removed the `args` field from `HashNode` structure, fixing a critical
use-after-free bug and taking the first step toward complete GpcType migration.

## Completed Work

### Phase 1: Infrastructure
✅ Added GpcType helper functions to `GpcType.h/.c`:
- `gpc_type_sizeof()` - Calculate size of any type
- `gpc_type_is_array/record/procedure()` - Type checking
- `gpc_type_get_array_bounds()` - Extract array bounds
- `gpc_type_get_record()` - Get RecordType from record types
- `gpc_type_get_primitive_tag()` - Get primitive type tag
- `gpc_type_get_array_element_type()` - Get array element type
- `gpc_type_get_procedure_params()` - Get procedure formal parameters
- `gpc_type_get_return_type()` - Get function return type

### Phase 2: Remove args Field
✅ Removed `ListNode_t *args` from `HashNode` structure
✅ Updated `AddIdentToTable()` API to remove args parameter
✅ Updated all `_Typed` functions in `SymTab.c`:
  - `PushVarOntoScope_Typed()`
  - `PushArrayOntoScope_Typed()`
  - `PushProcedureOntoScope_Typed()`
  - `PushFunctionOntoScope_Typed()`
  - `PushTypeOntoScope_Typed()`

✅ Fixed all references to removed args field:
  - `GPC/Parser/SemanticCheck/SemChecks/SemCheck_expr.c` (2 locations)
  - `GPC/Parser/SemanticCheck/SemChecks/SemCheck_stmt.c` (1 location)
  - `GPC/CodeGenerator/Intel_x86-64/codegen_expression.c` (1 location)

✅ Updated unit tests to use new API

### Phase 3: Verification
✅ Build succeeds with no errors or warnings
✅ All 79 compiler tests pass
✅ No regressions introduced

## Benefits Achieved

1. **Fixed Use-After-Free Bug**: The args field was causing crashes on Cygwin/MSYS
   because it shared a pointer with GpcType->info.proc_info.params, which was freed
   when the GpcType was destroyed.

2. **Simplified API**: Removed redundant parameter from `AddIdentToTable()`, making
   the API cleaner and less error-prone.

3. **Single Source of Truth**: Formal parameters are now always accessed through
   GpcType, ensuring consistency and preventing pointer aliasing issues.

4. **Better Encapsulation**: GpcType helper functions provide a clean interface for
   accessing type information, hiding implementation details.

## Remaining Work

The next phases will migrate the remaining legacy fields from HashNode:

### Priority 1: Array Fields (COMPLETED ✅)
- [x] `int is_array` (2 reader usages) → Use `gpc_type_is_array()`
- [x] `int array_start, array_end` (4 reader usages) → Use `gpc_type_get_array_bounds()`
- [x] `int element_size` (3 reader usages) → Use `gpc_type_get_array_element_size()`
- [x] `int is_dynamic_array` (3 reader usages) → Use `gpc_type_is_dynamic_array()`

### Priority 2: High Usage Fields (IN PROGRESS)
- [ ] `enum VarType var_type` (~46 usages) → Use `gpc_type_get_primitive_tag()`
- [ ] `struct RecordType *record_type` (~93 usages) → Use `gpc_type_get_record()`
- [ ] `struct TypeAlias *type_alias` (~62 usages) → Use GpcType queries

### Priority 3: Cleanup
- [ ] Remove all legacy fields from `HashNode` structure
- [ ] Remove `AddIdentToTable_Legacy()` function
- [ ] Remove legacy `PushVarOntoScope()`, `PushFunctionOntoScope()`, etc.
- [ ] Update any remaining code using `enum VarType`

## Migration Strategy

### For Each Legacy Field:
1. Search for all usages of the field
2. Replace with appropriate GpcType helper function call
3. Test that build succeeds and tests pass
4. Commit the changes

### Challenges:
- **Volume**: ~201 reader usages remaining (var_type + record_type + type_alias)
- **Complexity**: Some legacy code may not have GpcType available
- **Testing**: Need to ensure no behavioral changes during migration

### Approach:
- Migrate field-by-field to keep changes manageable
- Use GpcType helpers extensively to simplify migration
- Maintain backward compatibility during transition via fallback to legacy fields
- Remove legacy code only after all usages are migrated

## Timeline Estimate

- ✅ Phase 1-2: args field removal (COMPLETE)
- ✅ Phase 3: array fields migration (COMPLETE - 12 reader usages migrated)
- 📅 Phase 4: var_type migration (~46 usages, estimated 3-4 hours)
- 📅 Phase 5: record_type migration (~93 usages, estimated 4-5 hours)
- 📅 Phase 6: type_alias migration (~62 usages, estimated 3-4 hours)
- 📅 Phase 7: Final cleanup (estimated 2-3 hours)

**Total estimated: 12-16 hours of focused work remaining**

## Recent Progress (This PR)

### Commits in Sub-PR #190:
1. **Initial plan** - Created comprehensive migration strategy
2. **gpc_type_is_dynamic_array** - Added helper and migrated 3 reader usages
3. **is_array migration** - Migrated 2 reader usages to use `gpc_type_is_array()`
4. **array_start/end migration** - Migrated 4 reader usages to use `gpc_type_get_array_bounds()`
5. **element_size migration** - Added `gpc_type_get_array_element_size()` and migrated 3 reader usages

### Summary:
- **12 reader usages** successfully migrated across 4 array-related fields
- All readers now prefer GpcType when available, with graceful fallback to legacy fields
- Zero regressions - all 79 compiler tests still passing
- Build clean with no new warnings

## Files Modified So Far

### Phase 1-2 (Complete):
1. `GPC/Parser/ParseTree/GpcType.h` - Added helper function declarations
2. `GPC/Parser/ParseTree/GpcType.c` - Implemented helper functions
3. `GPC/Parser/SemanticCheck/HashTable/HashTable.h` - Removed args from HashNode and API
4. `GPC/Parser/SemanticCheck/HashTable/HashTable.c` - Updated implementation
5. `GPC/Parser/SemanticCheck/HashTable/UnitTest.c` - Updated test code
6. `GPC/Parser/SemanticCheck/SymTab/SymTab.c` - Updated _Typed functions
7. `GPC/Parser/SemanticCheck/SemChecks/SemCheck_expr.c` - Use GpcType helpers
8. `GPC/Parser/SemanticCheck/SemChecks/SemCheck_stmt.c` - Use GpcType helpers
9. `GPC/CodeGenerator/Intel_x86-64/codegen_expression.c` - Use GpcType helpers

### Phase 3 (Complete - Array Fields):
1. `GPC/Parser/ParseTree/GpcType.h` - Added array helper declarations
2. `GPC/Parser/ParseTree/GpcType.c` - Implemented array helpers:
   - `gpc_type_is_dynamic_array()`
   - `gpc_type_get_array_element_size()`
3. `GPC/Parser/SemanticCheck/SemChecks/SemCheck_expr.c` - Migrated array field readers
4. `GPC/CodeGenerator/Intel_x86-64/codegen_expression.c` - Migrated array field readers

## Testing Strategy

After each phase:
1. Run `meson compile -C builddir` to ensure no compilation errors
2. Run `meson test -C builddir` to ensure all tests pass
3. Manually test with procedure_callback_demo.p to verify the Cygwin fix
4. Check for any new warnings or errors

## Conclusion

The first phase of legacy type system removal is complete and successful. The args
field has been completely eliminated, fixing a critical bug and paving the way for
removing the remaining legacy fields. The codebase is now cleaner, safer, and ready
for the next phase of migration.
