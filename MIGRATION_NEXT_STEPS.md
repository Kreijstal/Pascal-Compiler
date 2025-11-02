# Legacy Type System Migration - Next Steps

## Completed Work (Phase 3)

Successfully migrated all **array-related HashNode field readers** to use GpcType:
- ✅ `is_dynamic_array` (3 usages) → `gpc_type_is_dynamic_array()`
- ✅ `is_array` (2 usages) → `gpc_type_is_array()`
- ✅ `array_start/end` (4 usages) → `gpc_type_get_array_bounds()`
- ✅ `element_size` (3 usages) → `gpc_type_get_array_element_size()`

**Total: 12 reader usages migrated, 0 regressions, all 79 tests passing**

## Established Migration Pattern

```c
/* Pattern for migrating field readers */
if (node->type != NULL && gpc_type_check_function(node->type)) {
    /* Use GpcType - modern, clean, correct */
    value = gpc_type_get_value(node->type);
} else {
    /* Fall back to legacy field during transition */
    value = node->legacy_field;
}
```

This pattern:
- Prefers GpcType when available
- Gracefully falls back to legacy fields
- Allows incremental migration
- Maintains backward compatibility
- Enables testing at each step

## Remaining Work

### Phase 4: Migrate `var_type` field (~46 usages)

**Challenge**: Used in many type-checking operations throughout semantic checker and codegen.

**Approach**:
1. Ensure all primitive types create GpcType with correct tag
2. Migrate readers to use `gpc_type_get_primitive_tag()`
3. Update type comparison functions to use GpcType
4. Test thoroughly with primitive type test cases

**Key Files**:
- `SemCheck_expr.c` - Type checking and conversions
- `SemCheck.c` - Variable declarations
- `codegen.c` - Type size calculations
- `codegen_expression.c` - Expression type handling

### Phase 5: Migrate `record_type` field (~93 usages)

**Challenge**: Highest usage count, used for record field access and sizeof calculations.

**Approach**:
1. Ensure all record types create GpcType with RecordType pointer
2. Migrate readers to use `gpc_type_get_record()`
3. Update record field access code
4. Test with record-heavy test cases

**Key Files**:
- `SemCheck_expr.c` - Record field access
- `tree.c` - Record type operations
- `codegen.c` - Record layout
- `codegen_expression.c` - Field offset calculations

### Phase 6: Migrate `type_alias` field (~62 usages)

**Challenge**: TypeAlias contains complex nested type information.

**Approach**:
1. Ensure TypeAlias types create proper GpcType
2. Migrate readers to query GpcType directly
3. Handle enum types specially
4. Test with typedef-heavy code

**Key Files**:
- `SemCheck_expr.c` - Type resolution
- `SemCheck.c` - Type declarations
- `codegen_expression.c` - Alias type handling

### Phase 7: Remove Legacy Fields

Once all readers are migrated:
1. Remove legacy fields from `HashNode` structure:
   - `enum VarType var_type`
   - `struct RecordType *record_type`
   - `struct TypeAlias *type_alias`
   - `int is_array`
   - `int array_start, array_end`
   - `int element_size, is_dynamic_array`

2. Remove legacy API functions:
   - `AddIdentToTable_Legacy()`
   - `PushVarOntoScope()` (non-_Typed version)
   - `PushArrayOntoScope()` (non-_Typed version)
   - `PushFunctionOntoScope()` (non-_Typed version)

3. Update all writers to use only `_Typed` APIs

### Phase 8: Final Cleanup

1. Remove `enum VarType` if no longer needed
2. Update documentation
3. Run full test suite including stress tests
4. Performance validation
5. Memory leak check with valgrind

## Implementation Strategy

### For Each Field Migration:

1. **Audit**: Find all usages of the field
   ```bash
   grep -rn '\->field_name' GPC/ --include="*.c" | grep -v builddir
   ```

2. **Analyze**: Categorize usages as:
   - **Readers**: Code that reads the field value
   - **Writers**: Code that sets the field value (skip for now)
   - **Initialization**: Code that initializes the field (skip for now)

3. **Migrate Readers**: Update each reader location
   - Add GpcType check
   - Use GpcType helper if available
   - Fall back to legacy field
   - Test

4. **Commit**: After each file or logical group
   - Build and test
   - Commit with descriptive message
   - Report progress

5. **Verify**: After all readers migrated for a field
   - Run full test suite
   - Check for performance regressions
   - Verify memory usage

## Time Estimates

Based on Phase 3 completion:

- Phase 4 (var_type): **2-3 hours** (46 usages, simpler than records)
- Phase 5 (record_type): **4-5 hours** (93 usages, most complex)
- Phase 6 (type_alias): **2-3 hours** (62 usages, moderate complexity)
- Phase 7 (removal): **2-3 hours** (careful testing required)
- Phase 8 (cleanup): **1-2 hours**

**Total: 11-16 hours** of focused work remaining

## Success Criteria

- ✅ All legacy field readers migrated to use GpcType
- ✅ All tests passing (79+ test cases)
- ✅ No memory leaks (valgrind clean)
- ✅ No performance regressions
- ✅ Code cleaner and more maintainable
- ✅ Single source of truth for type information

## Risk Mitigation

1. **Incremental Changes**: Migrate one field at a time
2. **Continuous Testing**: Test after every change
3. **Fallback Support**: Keep legacy fields during transition
4. **Documentation**: Track progress in LEGACY_MIGRATION_STATUS.md
5. **Reversibility**: Each commit is a working state

## Notes for Future Work

- Consider creating conversion functions between VarType and type tags
- May need helper for checking if HashNode has valid GpcType
- Some code paths may need refactoring to populate GpcType earlier
- Performance profiling recommended after completion
