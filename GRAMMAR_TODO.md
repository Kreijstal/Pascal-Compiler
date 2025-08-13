# Grammar TODO: Semicolon Handling in `asm` blocks

This document outlines a required future change to the GPC grammar to correctly handle semicolons at the end of `asm` blocks within conditional compilation directives.

## Current State (Temporary Workaround)

To get the end-to-end test for the `signum` function working, a temporary change was made to the expected syntax of `asm` blocks. The parser currently expects `asm` blocks to be written as `asm ... end`, without a terminating semicolon. The semicolon that terminates the function or procedure declaration is then applied to the `asm` block, similar to how `begin...end` blocks are handled.

The test case `GPC/TestPrograms/sign_test.p` has been modified to reflect this temporary syntax.

## Desired Behavior

The intended syntax for an `asm` block, as provided in the original problem description, is `asm ... end;`. The semicolon is part of the `asm` block itself.

## The Problem

The grammar has difficulty distinguishing the internal semicolon of the `asm` block from the external semicolon of the subprogram declaration, especially when `asm` blocks are used inside conditional compilation directives (`{$if...}`). This leads to parsing ambiguities and errors.

For example, in the following code:
```pascal
{$ifDef CPUx86_64}
  assembler;
  asm
    ...
  end;
{$else}
  begin
    ...
  end;
{$endIf}
```

- The `asm` branch has an internal semicolon.
- The `begin...end` branch has an external semicolon.

The parser needs to be able to handle both cases within the same conditional compilation rule.

## Proposed Solution

A robust solution would likely involve refactoring the grammar to better distinguish between different kinds of statement blocks. Here are a few potential approaches:

1.  **Lexer-based state management:** The lexer could enter a special state when it sees `assembler;` and handle the entire `asm...end;` block as a single token. This would hide the complexity from the parser, but it makes the lexer more complex.

2.  **Grammar refactoring with lookahead:** Advanced grammars might use lookahead assertions (`yacc`'s `%glr-parser` or other parser generators) to resolve the ambiguity.

3.  **Separate rules for `asm` bodies:** The grammar could have completely separate rules for subprograms with `asm` bodies, but this could lead to code duplication in the grammar file.

A careful analysis is needed to choose the best approach. The current workaround is sufficient for the proof of concept, but this issue should be addressed to make the parser more robust and compliant with the intended language syntax.
