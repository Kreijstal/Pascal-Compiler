Kreijstal Gwinn Pascal Compiler (KGPC)
======================================

KGPC is the compiler sources for this repository.  It is a fork of Damon
Gwinn's Pascal-Compiler that has been extended to compile a substantial
subset of Free Pascal (FPC) and to host portions of the FPC RTL.

For build, test, and usage instructions, see the README.md at the repository
root.  This file only documents source-tree layout and command-line flags.


Source layout
-------------

  Parser/                 Parsing and semantic analysis
    ParseTree/            AST nodes and the GpcType type system
    SemanticCheck/        Type checking, scope resolution, symbol tables
    List/                 Internal list utilities

  CodeGenerator/
    Intel_x86-64/         x86-64 code generator (Intel syntax)
      expr_tree/          Expression tree lowering
      stackmng/           Stack frame management
      ir/                 Per-call-site IR layer
      codegen_statement_parts/  Statement codegen, split by category

  Optimizer/              Dead-code elimination and pass manager

  Units/                  Pascal runtime units (system, sysutils, baseunix,
                          unix, classes, sysutils, dateutils, math, ...)

  runtime_*.c             C-level runtime support (baseunix, fpc_init,
                          fpc_assign, string, gmp, olevariant, widechar)


Invocation
----------

  kgpc <input.p> <output.s> [flags]

KGPC emits gcc-style x86-64 assembly.  Use gcc or clang to assemble and link
the resulting .s file with the runtime objects under KGPC/.


Flags
-----

  -O1                          Constant folding / expression simplification
  -O2                          Dead-code elimination

  --parse-only                 Stop after parsing and semantic checking;
  -parse-only                  do not emit code.

  --target=<abi>               Select target ABI.  Recognised values:
  --target <abi>                  windows | win64
                                  sysv    | systemv | linux
  -target=<abi>                Defaults to System V on Linux/macOS and
                               Windows x64 on _WIN32 / __CYGWIN__.

  --target-windows  /  --windows-abi   Shorthand for --target=windows
  --target-sysv     /  --sysv-abi      Shorthand for --target=sysv

  --asm-debug                  Emit comments tracking codegen state into
  --asm-debug-comments         the generated .s file.

  --disable-dce  /  --no-dce   Disable dead-code elimination during codegen
                               (emit every subprogram regardless of use).

  -non-local                   Enable non-local variable references from
                               nested procedures.  On by default — required
                               by the RTL and bootstrap units.


Front end
---------

KGPC uses the parser-combinator library under cparser/ as its front end.
An older lex/yacc-based parser used to live under Parser/LexAndYacc/ but
has been retired and removed.
