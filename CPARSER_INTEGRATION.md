# cparser Integration for Pascal Compiler

## Overview

This document describes the integration of the cparser combinator library into the Pascal Compiler (GPC).

## What is cparser?

cparser is a parser combinator library written in C that provides a modern, composable approach to building parsers. It already includes a comprehensive Pascal grammar implementation in its examples.

Repository: https://github.com/Kreijstal/cparser

## Integration Architecture

The integration follows a layered architecture:

```
Pascal Source File
       ↓
   cparser (lexing + parsing)
       ↓
   cparser AST
       ↓
   Adapter Layer (cparser_adapter.c)
       ↓
   GPC Parse Tree
       ↓
   Semantic Checker (unchanged)
       ↓
   Code Generator (unchanged)
       ↓
   x86-64 Assembly
```

## Components

### 1. cparser Library (vendored in `/cparser`)
- Core parser combinator implementation
- Pascal grammar in `/cparser/examples/pascal_parser/`
- Built as static library `libcparser.a`

### 2. Adapter Layer
- **cparser_adapter.c/h**: Converts cparser AST to GPC parse tree format
- **cparser_frontend.c/h**: High-level interface for parsing Pascal files with cparser

### 3. Modified GPC Files
- **main.c**: Added `-cparser` flag to enable cparser frontend
- **flags.h**: Added `use_cparser` global flag
- **makefile**: Integrated cparser library build

## Usage

To compile Pascal code using cparser instead of the traditional flex/bison parser:

```bash
./gpc input.p output.s -cparser
```

Without the `-cparser` flag, the compiler uses the original flex/bison parser.

## Current Status

### Working
- ✅ Build system integration
- ✅ Basic AST conversion for simple expressions
- ✅ Basic statement conversion (assignments, compound statements)
- ✅ Basic program structure parsing

### Partially Implemented
- ⚠️ Variable declarations
- ⚠️ Procedure/function declarations
- ⚠️ Control flow statements (if, while, for)

### Not Yet Implemented
- ❌ Complete type system conversion
- ❌ Array declarations and access
- ❌ Record types
- ❌ Comprehensive error handling
- ❌ All Pascal language constructs

## Architecture Details

### AST Conversion

The cparser produces an AST with nodes of type `ast_t` that have:
- `typ`: Node type (from `pascal_tag_t` enum)
- `sym`: Symbol information (identifier, value, etc.)
- `child`: First child node
- `next`: Next sibling node

The GPC compiler expects a parse tree with nodes of type `Tree_t` that represent:
- Program declarations
- Variable declarations
- Subprogram (procedure/function) declarations
- Statements and expressions

The adapter converts between these two representations by:
1. Recursively traversing the cparser AST
2. Mapping cparser node types to GPC node types
3. Converting expressions, statements, and declarations
4. Building appropriate GPC list structures

### Key Functions

- `cparser_ast_to_tree(ast_t* root)`: Main conversion entry point
- `convert_expression(ast_t* ast)`: Converts expressions
- `convert_statement(ast_t* ast)`: Converts statements
- `convert_var_declaration(ast_t* ast)`: Converts variable declarations
- `convert_subprogram(ast_t* ast)`: Converts procedure/function declarations

## Benefits of cparser

1. **Modern Parser Design**: Parser combinators are more composable and maintainable than traditional yacc/bison grammars
2. **Better Error Messages**: Parser combinators can provide more detailed error information
3. **Flexible Grammar**: Easier to extend and modify the Pascal grammar
4. **No External Dependencies**: Pure C implementation, no need for flex/bison tools

## Future Work

To complete the integration:

1. **Complete AST Conversion**: Implement all Pascal language constructs
   - Array types and access
   - Record types
   - Pointer types
   - All expression operators
   - All statement types

2. **Improve Error Handling**: 
   - Better error messages from cparser
   - Proper line/column number tracking
   - Error recovery

3. **Testing**:
   - Create comprehensive test suite
   - Test all Pascal language features
   - Compare output with flex/bison parser

4. **Optimization**:
   - The semantic checker and code generator remain unchanged
   - Could potentially optimize for the new AST structure

5. **Documentation**:
   - Document all supported Pascal constructs
   - Add examples
   - Update user guide

## Development Notes

- The original flex/bison parser remains functional and is used by default
- The `-cparser` flag enables the new parser for testing
- Both parsers can coexist, allowing gradual migration
- The semantic checker and code generator are parser-agnostic

## Building

The cparser library is automatically built as part of the GPC build process:

```bash
cd GPC
make clean
make release
```

This will:
1. Build cparser static library
2. Build flex/bison parser
3. Build code generator
4. Link everything into the `gpc` executable
