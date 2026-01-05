# Architectural Analysis: Program-Level Variable Access

## Problem Statement

Top-level procedures (at lexical depth 1) cannot access program-level variables. This causes compilation failures or runtime crashes when a top-level procedure tries to read or write variables declared at the program level.

### Example of Failing Code

```pascal
program Example;
var
  GlobalCounter: integer;

procedure IncrementCounter;  { Top-level procedure }
begin
  GlobalCounter := GlobalCounter + 1;  { FAILS }
end;

begin
  GlobalCounter := 0;
  IncrementCounter;
  WriteLn(GlobalCounter);
end.
```

**Error**: `Failed to acquire static link for variable GlobalCounter`

## Root Cause Analysis

### Current Architecture

1. **Program variables are stack-allocated**: Variables declared at program level are allocated on the program's stack frame (not in static .data section)

2. **No program frame preservation**: When the program calls procedures, the program's frame pointer is not preserved or passed to procedures

3. **Static link mechanism**: The compiler uses static links to access outer scope variables:
   - Nested procedures (depth > 1) receive a static link in the first parameter register
   - Top-level procedures (depth == 1) do NOT receive static links
   - Program variables are at scope depth 0 (or 1 depending on perspective)

4. **The mismatch**: When a top-level procedure tries to access a program variable:
   - `find_label_with_depth()` returns `scope_depth = 1` (parent scope)
   - Codegen tries to use static link to access the variable
   - But top-level procedures don't have a static link
   - Result: Compilation error or crash

### Why It Happens

The compiler architecture assumes a clear distinction between:
- **Local variables**: Accessed via `%rbp` offset (scope_depth == 0)
- **Outer scope variables**: Accessed via static link chain (scope_depth > 0)
- **Global/static variables**: Accessed via RIP-relative addressing (`is_static` flag)

Program variables fall into a gap: they're not local to procedures, they need outer scope access, but static links aren't available at the top level.

## Architectural Solutions

### Solution 1: Static Storage for Program Variables (Recommended)

**Approach**: Allocate program-level variables in the `.data` section instead of on the stack.

**Implementation**:
1. In `codegen_function_locals()`, check if we're generating code for the program (depth == 0)
2. For program-level variables, emit them to `.data` section with global labels
3. Set `is_static = 1` on their stack nodes
4. Access them via RIP-relative addressing from any procedure

**Advantages**:
- Clean separation: program variables are truly global
- No runtime overhead
- Simplest implementation
- Matches how most compilers handle global variables

**Disadvantages**:
- Changes program semantics slightly (variables persist beyond program scope)
- Requires refactoring variable allocation code

**Code Changes Required**:
```c
// In codegen.c:codegen_function_locals()
void codegen_function_locals(ListNode_t *local_decl, CodeGenContext *ctx, SymTab_t *symtab) {
    int is_program_scope = (ctx->current_subprogram_lexical_depth == 0);
    
    if (is_program_scope) {
        // Emit program variables to .data section
        codegen_program_variables_to_data(local_decl, ctx, symtab);
    } else {
        // Stack allocate procedure-local variables (existing logic)
        // ... existing code ...
    }
}
```

### Solution 2: Program Frame Pointer in Global Register

**Approach**: Reserve a callee-saved register (e.g., `%r12`) to hold the program's frame pointer.

**Implementation**:
1. At program start, save `%rbp` to `%r12`
2. Mark `%r12` as reserved (never allocate for general use)
3. When accessing program variables from procedures, use `%r12` instead of static link
4. Distinguish program variables by checking if `scope_depth == 1 && current_depth == 1`

**Advantages**:
- Preserves stack semantics for program variables
- Minimal changes to existing code
- Works with current allocation model

**Disadvantages**:
- Reduces available registers by one
- Requires careful register allocation changes
- More complex to implement correctly

**Code Changes Required**:
```c
// In codegen.c:codegen_program()
void codegen_program(...) {
    // ... setup code ...
    fprintf(ctx->output_file, "\tmovq\t%%rbp, %%r12\n");  // Save program frame
    
    // Mark r12 as reserved
    ctx->program_frame_reg = R12;
    // ... rest of program ...
}

// In codegen_statement.c:codegen_address_for_expr()
if (scope_depth == 1 && codegen_get_lexical_depth(ctx) == 1) {
    // Access via program frame register
    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%r12), %s\n",
             var_node->offset, addr_reg->bit_64);
}
```

### Solution 3: Static Link for Top-Level Procedures

**Approach**: Pass the program frame pointer as a "static link" to top-level procedures.

**Implementation**:
1. Modify `codegen_procedure()` to accept static link even at depth == 1
2. When calling top-level procedures from program, pass program's `%rbp`
3. When calling top-level procedures from nested procedures, pass program frame (stored in caller's static link slot at offset 0)

**Advantages**:
- Uses existing static link mechanism
- No register pressure
- Conceptually consistent with nested scopes

**Disadvantages**:
- Increases calling convention complexity
- Extra parameter for all top-level procedure calls
- May break existing assumptions about depth == 1

**Code Changes Required**:
```c
// In codegen.c:codegen_procedure()
int will_need_static_link = (!is_class_method &&
    (proc->requires_static_link || lexical_depth == 1));  // Changed: include depth 1
    
// In codegen_statement.c:codegen_proc_call()
if (callee_depth == 1) {
    // Pass program frame as static link
    if (current_depth == 0) {
        // Calling from program
        inst_list = add_inst(inst_list, "\tmovq\t%rbp, %rdi\n");
    } else {
        // Calling from nested procedure - pass base static link
        // (program frame is at the base of static link chain)
    }
}
```

### Solution 4: Hybrid - Program Variables as Static Members

**Approach**: Emit program variables as static data but initialize them at program start.

**Implementation**:
1. Declare program variables in `.bss` section (uninitialized global)
2. At program start, initialize them from constants/initializers
3. Access them via RIP-relative addressing

**Advantages**:
- Supports variable initialization
- Clean global access
- No runtime frame overhead

**Disadvantages**:
- Most complex implementation
- Requires initialization code generation
- May not support all initializer expressions

## Recommended Approach

**Solution 1 (Static Storage)** is recommended because:

1. **Simplicity**: Clearest implementation with fewest edge cases
2. **Performance**: No runtime overhead, direct RIP-relative access
3. **Semantics**: Program variables ARE effectively global in Pascal
4. **Compatibility**: Matches how Free Pascal and other compilers work
5. **Maintainability**: Easiest to understand and debug

## Implementation Plan for Solution 1

### Phase 1: Infrastructure (1-2 days)
1. Add `codegen_emit_program_variable_to_data()` function
2. Modify `codegen_function_locals()` to detect program scope
3. Update stack management to mark program variables as static

### Phase 2: Code Generation (2-3 days)
1. Generate `.data` section entries for program variables
2. Update variable access code to handle program-static variables
3. Ensure proper label generation and linkage

### Phase 3: Testing (1-2 days)
1. Test program variable access from top-level procedures
2. Test program variable access from nested procedures
3. Test with arrays, records, and complex types
4. Test with variable initializers
5. Regression test existing test suite

### Phase 4: Documentation (1 day)
1. Update architecture documentation
2. Add comments explaining program variable handling
3. Document any behavioral changes

## Affected Files

### Core Changes
- `KGPC/CodeGenerator/Intel_x86-64/codegen.c` (codegen_program, codegen_function_locals)
- `KGPC/CodeGenerator/Intel_x86-64/codegen_statement.c` (variable access)
- `KGPC/CodeGenerator/Intel_x86-64/stackmng.c` (stack management)

### Testing
- Add test cases for program variable access patterns
- Update existing tests if behavior changes

## Migration Notes

If implementing Solution 1, existing programs may see behavior changes:

1. **Memory lifetime**: Program variables now persist in static storage (not stack)
2. **Address stability**: Program variable addresses are now fixed (good for pointers)
3. **Initialization**: May need to handle variable initialization differently

These changes align with how most Pascal compilers work and should be transparent to users.

## Conclusion

The current architecture has a fundamental gap in handling program-level variables. Solution 1 (static storage) provides the cleanest path forward with good performance characteristics and maintainability. The implementation is straightforward and aligns with industry-standard Pascal compiler design.
