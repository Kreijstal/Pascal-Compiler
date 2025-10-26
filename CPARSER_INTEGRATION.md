# cparser Integration for Pascal Compiler

## Overview

This document describes the integration of the cparser combinator library into the Pascal Compiler (GPC).

## What is cparser?

cparser is a parser combinator library written in C that provides a modern, composable approach to building parsers. It already includes a comprehensive Pascal grammar implementation in its examples.

Repository: https://github.com/Kreijstal/cparser

## Integration Architecture

The integration uses a clean, direct approach:

```
Pascal Source File
       ↓
   cparser (lexing + parsing)
       ↓
   cparser AST
       ↓
   New Semantic Checker (works directly with cparser AST)
       ↓
   New Code Generator (works directly with cparser AST)
       ↓
   x86-64 Assembly
```

## Components

### 1. cparser Library (vendored in `/cparser`)
- Core parser combinator implementation
- Pascal grammar in `/cparser/examples/pascal_parser/`
- Built using Meson build system

### 2. New Semantic Checker
- **GPC/NewSemanticChecker/**: Rewritten to work directly with cparser AST
- Eliminates the need for AST conversion
- Cleaner, more maintainable code

### 3. New Code Generator  
- **GPC/NewCodeGenerator/**: Rewritten to work directly with cparser AST
- Generates x86-64 assembly from cparser AST nodes
- No intermediate parse tree conversion needed

## Usage

The compiler now uses cparser by default:

```bash
./gpc input.p output.s
```

## Build System - Meson

The project uses Meson as its build system. To build:

```bash
meson setup builddir
meson compile -C builddir
```

The Meson build configuration:
- Automatically compiles cparser library
- Builds the new semantic checker and code generator
- Integrates flex/bison for legacy support (if needed)
- Runs test suite

## Current Status

### Implemented
- ✅ Meson build integration for cparser
- ✅ cparser library compilation

### In Progress
- 🔄 New semantic checker working directly with cparser AST
- 🔄 New code generator working directly with cparser AST

### Benefits of This Approach

1. **No Conversion Overhead**: Direct use of cparser AST eliminates conversion step
2. **Cleaner Code**: No adapter layer means less code to maintain
3. **Better Error Messages**: Direct access to cparser's error information
4. **Type Safety**: Work with actual cparser AST types instead of converted structures
5. **Proper Build System**: Uses existing Meson infrastructure

## Architecture Details

### cparser AST Structure

The cparser produces an AST with nodes of type `ast_t`:
- `typ`: Node type (from `pascal_tag_t` enum)
- `sym`: Symbol information (identifier, value, etc.)
- `child`: First child node
- `next`: Next sibling node

### New Semantic Checker

Will implement symbol table and type checking directly on cparser AST:
- Symbol table management
- Type checking
- Scope resolution
- Function/procedure validation

### New Code Generator

Will generate assembly directly from cparser AST:
- Expression code generation
- Statement code generation
- Function/procedure code generation
- Register allocation

## Future Work

1. **Complete Implementation**:
   - Finish semantic checker for all Pascal constructs
   - Finish code generator for all Pascal constructs
   - Implement all operators and statements

2. **Testing**:
   - Comprehensive test suite
   - Compare with reference implementations
   - Performance benchmarks

3. **Optimization**:
   - AST-level optimizations
   - Register allocation improvements
   - Code generation optimizations

4. **Documentation**:
   - API documentation
   - Usage examples
   - Architecture guides

## Development Notes

- The build uses Meson, not makefiles
- Original flex/bison parser can be kept for reference
- New implementation is cleaner and more maintainable
- Direct AST usage is more efficient

## Building

Build with Meson:

```bash
meson setup builddir
meson compile -C builddir
```

Run tests:

```bash
meson test -C builddir
```

This will:
1. Build cparser library
2. Build new semantic checker
3. Build new code generator
4. Link everything into the `gpc` executable
5. Run test suite
