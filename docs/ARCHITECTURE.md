# KGPC Architecture

This document describes how the Kreijstal Gwinn Pascal Compiler is organised,
from source file to emitted assembly.  It is intentionally lightweight —
deeper notes live in the topic-specific docs under `docs/` and in the
`KGPC/README.txt` source-layout reference.

For the parser-combinator library that backs the front end, see
[`cparser/ARCHITECTURE.md`](../cparser/ARCHITECTURE.md).


## Pipeline

```
  input.p ──► parse ──► load units ──► semcheck ──► mark-used ──► codegen ──► output.s
                │           │             │             │             │
                ▼           ▼             ▼             ▼             ▼
            Tree_t      Tree_t per   SymTab_t     used-flag bits   .s file
            (AST)       used-unit                  in subprogram
                                                   nodes
```

Driver: [`KGPC/main_cparser.c`](../KGPC/main_cparser.c).  The call sequence is straightforward:

1. `ParsePascalOnly("KGPC/Units/system.p")` — every program is compiled
   against a system prelude.  The prelude AST is merged into the user AST
   before semantic analysis.
2. `ParsePascalOnly(argv[1])` — the user's source file.
3. `load_units_from_list(...)` — transitive `uses`-clause resolution with
   cycle detection (`UnitSet` in `main.c`).  Each unit is parsed and merged
   into the same `Tree_t`.
4. `start_semcheck(user_tree, &sem_result)` — type checking, scope
   resolution, symbol-table construction.  Produces a `SymTab_t`.
5. `mark_used_functions(...)` — dead-code elimination at the subprogram
   level.  Sets a used-flag on every reachable definition.
6. `codegen(user_tree, ..., &ctx, symtab, NULL)` — emits x86-64 assembly to
   `ctx.output_file`.  If codegen reports an error the partial `.s` is
   deleted.

Optional flag `--parse-only` short-circuits after step 3 and writes an empty
`.s` placeholder.


## Source layout

```
KGPC/
├── main_cparser.c               Driver (entry point + pipeline glue)
├── flags.c, flags.h             Command-line flag state
├── compilation_context.c/.h     Per-translation-unit context
├── unit_paths.c/.h              -Fu / vendor / user search paths
├── unit_registry.c/.h           Loaded-unit dedup, name lowercasing
├── stacktrace.c/.h              SIGSEGV → backtrace dump
├── string_intern.c/.h           Identifier interning
├── debug_serializer.{c,h}       AST snapshot dump (--asm-debug)
├── debug_deserializer.{c,h}     AST snapshot load (currently for testing)
│
├── Parser/
│   ├── pascal_frontend.c/.h     Top-level parser entry point
│   ├── ParsePascal.c/.h         Combinator wiring for Pascal grammar
│   ├── parser_error.c/.h        Diagnostic formatting
│   ├── ErrVars.h                Globals for error reporting
│   ├── List/                    Simple linked-list utilities
│   │
│   ├── ParseTree/               AST + type system
│   │   ├── tree.c/.h            Tree_t node constructors / destructors
│   │   ├── tree_types.h         TREE_* enum (PROGRAM / UNIT / STMT / EXPR ...)
│   │   ├── KgpcType.c/.h        First-class type system (GpcType_t)
│   │   ├── type_tags.h          Type-kind enum
│   │   ├── ident_ref.c/.h       Interned identifier reference
│   │   ├── generic_types.c/.h   Generic-type instantiation
│   │   ├── operator_registry.c/.h  Built-in operator dispatch
│   │   ├── from_cparser.c/.h    Bridge: cparser CST → Tree_t AST
│   │   └── from_cparser_parts/  Per-construct bridge fragments
│   │
│   └── SemanticCheck/
│       ├── SemCheck.c/.h        Top-level traversal entry
│       ├── SemCheck_parts/      Decomposed semantic-check passes
│       ├── SemChecks/           Per-construct checkers (Expr, Stmt,
│       │                        Builtins, Generics, Sets, Records, ...)
│       ├── SymTab/              Symbol-table backing store
│       └── HashTable/           Name → symbol map
│
├── CodeGenerator/Intel_x86-64/  Only target back-end at present
│   ├── codegen.c                Top-level codegen entry (7k LOC)
│   ├── codegen_expression.c     Expression lowering
│   ├── codegen_expr_*.c         Arguments, arrays, relops, sizeof, ...
│   ├── codegen_subprograms.c    Function / procedure prologue & epilogue
│   ├── codegen_symbol_resolution.c  Name lookup at codegen time
│   ├── codegen_vmt.c            Virtual method tables (classes)
│   ├── codegen_string_set.c     String-set literals and tests
│   ├── codegen_statement.c      Statement codegen entry
│   ├── codegen_statement_parts/ Statement codegen split by category
│   ├── codegen_builtins.c       Built-in routines (Writeln, SetLength, ...)
│   ├── graph_coloring_allocator.{c,h}  Chaitin register allocator
│   ├── register_types.h         Register set + spill bookkeeping
│   ├── asm_emit.h               Instruction emission helpers
│   ├── abi_constants.h          SysV / Windows ABI constants
│   ├── expr_tree/               Expression-tree IR (used during lowering)
│   ├── stackmng/                Stack-frame layout
│   └── ir/                      Per-callsite IR
│
├── Optimizer/
│   ├── optimizer.c/.h           Pass driver
│   ├── pass_manager.c/.h        Pass ordering
│   └── mark_used.c/.h           Reachability / DCE pass
│
├── Units/                       Pascal-side RTL units
│   ├── system.p                 Always-included prelude
│   ├── sysutils.p, classes.p, math.p, dateutils.p, typinfo.p, ...
│   ├── baseunix.p, unix.p, unixtype.p, linux.p, windows.p
│   ├── crt.p, keyboard.p, termio.p, dynlibs.p
│   ├── sortbase.p, charset.p, stringutils.p, sysconst.p, cthreads.p
│   ├── ctypes.p, fgl.p, gmp.p, errors.p, objpas.p, prelude.p
│
├── runtime.c                    Core RTL (Writeln, ReadLn, strings, ...)
├── runtime_baseunix.c           POSIX syscall wrappers (fpOpen, fpsigaction)
├── runtime_unix.c               Higher-level Unix helpers
├── runtime_fpc_assign.c         FPC-compatible managed assignment
├── runtime_fpc_init.c           FPC-compatible RTL init / finalisation
├── runtime_fpc_pchar_to_shortstr*.c  String conversions
├── runtime_string.c             ShortString / AnsiString helpers
├── runtime_olevariant_assign.c  OleVariant assignment semantics
├── runtime_widechar_assign_olevariant.c
├── runtime_gmp.c                Optional GMP integration
├── runtime_fpc_abstract_stubs.S Abstract-method trampolines (asm)
└── runtime_fpc_rtl_compat.S     FPC RTL link compatibility shims
```


## Type system

`GpcType_t` (defined in `Parser/ParseTree/KgpcType.h`) is the single source
of truth for type representation used by both the semantic checker and the
code generator.  It supersedes the older `var_type_*` enums still visible in
parts of the codebase.

`type_tags.h` enumerates the kinds (integer / real / pointer / record /
class / array / set / file / generic-parameter / …).  Composite types carry
references to other `GpcType_t` instances; ownership lives in a
type registry.


## Symbol tables and scoping

Symbol resolution uses a parent-pointer **scope tree** rooted at a
builtin scope, with one child per loaded unit and further children for
procedures, classes, and `with`-statements.  `SymTab_t`
(`KGPC/Parser/SemanticCheck/SymTab/`) owns the scope nodes; lookups walk
parent pointers and the current unit's `uses`-chain.  Unit registration
lives in `unit_registry.c`.


## Calling conventions

Two ABIs are supported at codegen time:

| ABI       | Default on          | Selected via                       |
|-----------|---------------------|------------------------------------|
| System V  | Linux, macOS        | (default), `--target=sysv`         |
| Windows   | `_WIN32`, `__CYGWIN__` | `--target=windows` |

`CodeGenContext.target_abi` is consulted by every callsite emitter; see
`abi_constants.h` for argument-register order, shadow-space rules, and the
red-zone treatment.


## Linking

KGPC only emits `.s`.  Assembling and linking is delegated to `gcc` (or
`clang`) by the Meson build, which compiles the C runtime objects and links
them against the user program.  The runtime split (one C file per concern,
under `KGPC/`) keeps each translation unit small and lets the build prune
unused runtime support via the linker's `--gc-sections`.


## FPC compatibility

The stated project goal is to bootstrap the Free Pascal Compiler without
needing FPC or a proprietary Delphi-compatible compiler.  Progress and
specific blockers are tracked in [`FPC_BOOTSTRAP.md`](FPC_BOOTSTRAP.md).
The KGPC RTL under `KGPC/Units/` aims for source-level compatibility with
the corresponding FPC units, modulo the language features KGPC does not yet
support.


## Tests

`tests/test_cases/` contains 2000+ `.p` files; each one with a matching
`.expected` becomes a test automatically.  See
[`tests/README_TEST_AUTODISCOVERY.md`](../tests/README_TEST_AUTODISCOVERY.md)
for how the harness discovers, compiles, links, runs, and diffs them.

A separate FPC-RTL mode (`-Drun_fpc_rtl_tests=true`) compiles a subset of
the same cases against the real FPC RTL under `FPCSource/`.  This is the
test surface used to keep KGPC ABI-compatible with FPC.


## Where to look first

| Task                           | File(s) to read first                                  |
|--------------------------------|--------------------------------------------------------|
| New language construct         | `Parser/ParseTree/from_cparser.c`, then SemCheck       |
| New built-in                   | `Parser/SemanticCheck/SemChecks/SemCheck_Expr_Builtins.c` and `CodeGenerator/.../codegen_builtins.c` |
| Calling-convention bug         | `CodeGenerator/Intel_x86-64/codegen_subprograms.c` and `abi_constants.h` |
| Register-spill issue           | `CodeGenerator/Intel_x86-64/graph_coloring_allocator.c` |
| RTL routine                    | one of the `KGPC/runtime_*.c` files (split by concern) |
| Unit not found                 | `unit_paths.c`, `unit_registry.c`                      |
| Crash with no diagnostic       | `stacktrace.c` (already installed at startup)          |
