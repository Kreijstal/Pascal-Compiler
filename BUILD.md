# Building GPC with cparser

## Prerequisites

- Meson build system (`meson`)
- Ninja build tool (`ninja`)
- C compiler (gcc or clang)
- flex and bison (for legacy build, if enabled)

### Installing on Ubuntu/Debian:
```bash
sudo apt-get install meson ninja-build gcc flex bison
```

## Building

### Quick Start
```bash
meson setup builddir
meson compile -C builddir
```

The executable will be at `builddir/GPC/gpc`

### Running Tests
```bash
meson test -C builddir
```

### Clean Build
```bash
rm -rf builddir
meson setup builddir
meson compile -C builddir
```

## Usage

```bash
./builddir/GPC/gpc input.p output.s
```

## Current Status

### Working
- ✅ Meson build integration
- ✅ cparser library compilation
- ✅ Pascal parsing with cparser
- ✅ Basic executable structure

### To Do
- 🔄 Semantic checker (needs implementation)
- 🔄 Code generator (needs implementation)
- 🔄 Symbol table management
- 🔄 Type checking
- 🔄 Assembly output

## Architecture

The new architecture is straightforward:

1. **Parse**: cparser reads Pascal source and produces AST
2. **Semantic Check**: (TODO) Validate semantics directly on cparser AST
3. **Code Generate**: (TODO) Generate assembly directly from cparser AST

No intermediate conversion or adapter layers. Direct use of cparser's AST throughout.

## Development

### Build System

The project uses Meson:
- `meson.build` - Top level, builds cparser library
- `GPC/meson.build` - Builds GPC executable and runs tests

### Source Files

- `GPC/main_cparser.c` - Main entry point
- `cparser/` - cparser library (vendored)
- `cparser/examples/pascal_parser/` - Pascal grammar implementation

### Legacy Code

The original flex/bison parser is preserved but currently disabled in the build.
It can be re-enabled in `GPC/meson.build` if needed for comparison.

## Contributing

When implementing semantic checking or code generation:

1. Work directly with cparser's `ast_t` structure
2. Use `ast->typ` to determine node type (from `pascal_tag_t` enum)
3. Use `ast->sym` for symbol information (identifiers, values, etc.)
4. Use `ast->child` and `ast->next` to traverse the tree
5. See `cparser/examples/pascal_parser/pascal_parser.h` for AST node types

## More Information

See `CPARSER_INTEGRATION.md` for detailed architecture and design documentation.
