# VMT Implementation - Detailed Technical Design

## Current Status
**Phase 1 COMPLETE**: Infrastructure in place
- MethodInfo structure defined
- RecordType.methods list added
- ClassMethodBinding tracks virtual/override
- Parser extracts virtual/override from AST
- get_class_methods() API for querying methods

## Phase 2: VMT Construction (In Progress)

### Key Algorithm: Building the VMT

For each class during semantic checking:

1. **Collect Own Methods**
   ```c
   get_class_methods(class_name, &methods, &count);
   ```

2. **Inherit Parent VMT** (if parent exists)
   - Copy parent's VMT entries
   - Mark inherited methods with parent's mangled name

3. **Process Override Methods**
   - Find matching parent VMT entry by method name
   - Replace parent's mangled name with child's
   - Keep same VMT index

4. **Add New Virtual Methods**
   - Assign next available VMT index
   - Add to VMT with child's mangled name

5. **Store VMT in RecordType.methods**
   - Each MethodInfo has:
     - name: unmangled method name
     - mangled_name: ClassName__MethodName for dispatch
     - is_virtual/is_override: from parser
     - vmt_index: position in VMT (-1 if not virtual)

### Example:

```pascal
type
  TBase = class
    procedure Show; virtual;     // VMT[0] = TBase__Show
    procedure Print; virtual;    // VMT[1] = TBase__Print
  end;

  TDerived = class(TBase)
    procedure Show; override;    // VMT[0] = TDerived__Show (replaces)
    procedure Draw; virtual;     // VMT[2] = TDerived__Draw (new)
  end;
```

TDerived's VMT:
- [0]: TDerived__Show (overrides TBase__Show)
- [1]: TBase__Print (inherited)
- [2]: TDerived__Draw (new)

## Phase 3: Code Generation

### Object Layout
```
Offset 0:  VMT pointer (8 bytes on x86-64)
Offset 8:  Field1
Offset N:  Field2
...
```

### VMT Data Structure
```assembly
.data
TBase_VMT:
    .quad TBase__Show
    .quad TBase__Print

TDerived_VMT:
    .quad TDerived__Show
    .quad TBase__Print
    .quad TDerived__Draw
```

### Virtual Method Call
```pascal
Obj.Show();  // If Show is virtual
```

Generates:
```assembly
; Load object address
mov rax, [rbp - obj_offset]
; Load VMT pointer (first field)
mov rax, [rax]
; Call method at VMT[index]
call [rax + index*8]
```

### Non-Virtual Method Call
```pascal
Obj.Show();  // If Show is NOT virtual
```

Generates (current behavior):
```assembly
call ClassName__Show
```

## Phase 4: Method Resolution Fix

Current problem: When looking up `Obj.Method()`, we use `find_class_for_method()` which returns first match.

Solution:
1. Determine type of `Obj` (already available in `object_expr`)
2. Look up method in that class's VMT
3. Use the mangled_name from VMT entry
4. This resolves ambiguity for override methods

## Implementation Files

### Parser (from_cparser.c)
- [x] ClassMethodBinding with is_virtual/is_override
- [x] extract directive from AST
- [x] get_class_methods() API

### Semantic Checker (SemCheck.c)
- [ ] build_class_vmt() function
- [ ] Populate RecordType.methods during TYPE_DECL processing
- [ ] Method override resolution

### Code Generator (codegen.c, codegen_statement.c)
- [ ] generate_vmt_data() - emit VMT globals
- [ ] modify_class_layout() - add VMT pointer field
- [ ] codegen_virtual_call() - indirect dispatch
- [ ] Update convert_method_call_statement() to use VMT

## Testing Strategy

1. **Test virtual without override**
   - Single class with virtual method
   - Verify VMT is generated
   - Verify call works

2. **Test override**
   - Parent + child with override
   - Verify VMT has correct entries
   - Verify derived method is called

3. **Test polymorphism** (future - requires object creation)
   - Base pointer to derived object
   - Call virtual method
   - Verify derived implementation called

## Estimated Remaining Work
- build_class_vmt(): ~150 LOC
- VMT data generation: ~100 LOC
- Virtual call codegen: ~80 LOC
- Method lookup fix: ~60 LOC
- Testing and debugging: significant

Total: ~400 LOC + testing
