# VMT and Dynamic Dispatch Implementation Plan

## Problem
Currently, when a derived class declares a method with the same name as a parent class method (using `override`), the compiler generates an "ambiguous" error because it finds multiple overload candidates.

## Root Cause
The current method resolution system uses `find_class_for_method()` which returns the FIRST class that registered a given method name, without considering:
1. The actual type of the object being called
2. The inheritance relationship between classes
3. Whether a method is virtual or override

## Required Changes for Full VMT Implementation

### 1. Parser/AST Changes ✓ (Partially Done)
- [x] Accept `virtual` and `override` keywords
- [x] Add PASCAL_T_METHOD_DIRECTIVE AST tag
- [ ] Store virtual/override flags in method declarations
- [ ] Propagate this information to semantic checker

### 2. Semantic Checker Changes (Major Work Required)
- [ ] Track virtual/override status for each method
- [ ] Build Virtual Method Table (VMT) for each class
- [ ] Resolve method override chains (which method overrides which)
- [ ] Generate VMT layout with correct method pointers
- [ ] Handle method lookup considering inheritance and overrides

### 3. Object Layout Changes
- [ ] Add VMT pointer as first field of each object instance
- [ ] Initialize VMT pointer in object creation
- [ ] Ensure field offsets account for VMT pointer

### 4. Code Generator Changes (Major Work Required)
- [ ] Generate VMT data structures as global constants
- [ ] For virtual method calls: generate indirect call through VMT
- [ ] For non-virtual method calls: keep current direct call
- [ ] Handle method calls through base class pointers

### 5. Runtime Support
- [ ] May need runtime library support for object creation
- [ ] VMT initialization code

## Estimated Scope
- **Lines of code**: 800-1200 LOC across parser, semantic checker, and codegen
- **Complexity**: High - requires understanding of:
  - Virtual method dispatch mechanisms
  - Object layout and ABI
  - x86-64 calling conventions for indirect calls
  - Symbol resolution and mangling

## Alternative: Simplified Override Resolution (Intermediate Step)
Instead of full VMT, implement static dispatch with override support:
1. When resolving `Obj.Method()`:
   - Determine declared type of `Obj`
   - Look for `Method` in that class
   - If found with `override`, replace parent's version
   - Use static dispatch (no runtime polymorphism)

This fixes the ambiguity error and allows override test to compile, but doesn't provide true polymorphism (calling through base class pointer won't use derived class method).

## Current Status
- Parser accepts virtual/override: ✓
- Field inheritance works: ✓
- Method override resolution: ✗ (causes ambiguity)
- VMT generation: ✗ (not started)
- Dynamic dispatch: ✗ (not started)
