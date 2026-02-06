# FPC Bootstrap Analysis

## Status: BLOCKED - Compiler Bugs/Limitations

The FPC RTL cannot be fully compiled because kgpc has bugs and missing features that need to be fixed.

## Prerequisites

Clone the FPC source code:
```bash
git clone https://github.com/fpc/FPCSource
```

## Build Commands

### baseunix.pp (0 errors) ✓
```bash
./build/KGPC/kgpc ./FPCSource/rtl/unix/baseunix.pp /tmp/baseunix.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

### sysutils.pp (9 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/unix/sysutils.pp /tmp/sysutils.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/objpas/sysutils \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64 \
  -I./FPCSource/packages/rtl-objpas/src/inc
```

## Error Reduction with C-Vise (Flatten-Only Preprocessor)

When minimizing sysutils failures, use the preprocessor's `--flatten-only` mode to expand `{$i ...}` includes into a single file while keeping compiler directives intact for FPC to evaluate. This avoids corrupting conditional branches during reduction.

### Flatten sysutils.pp
```bash
./build/kgpc-preprocess --flatten-only \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/objpas/sysutils \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64 \
  ./FPCSource/rtl/unix/sysutils.pp sysutils_flat.pp
cp -f sysutils_flat.pp sysutils_indexofany.pp
```

### Interestingness test and cvise (example)
Create a small interestingness script locally (example below), then run cvise against the flattened file.
Do not use the `--timeout` flag with `cvise`.

```bash
cat > /tmp/cvise_indexofany.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

input="sysutils_indexofany.pp"
root="/home/kreijstal/git/Pascal-Compiler"

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
cp -f "$input" "$tmpdir/sysutils.pp"

(
  cd "$tmpdir"
  fpc \
    "-Fi$root/FPCSource/rtl/inc" \
    "-Fi$root/FPCSource/rtl/unix" \
    "-Fi$root/FPCSource/rtl/linux" \
    "-Fi$root/FPCSource/rtl/linux/x86_64" \
    "-Fi$root/FPCSource/rtl/objpas/sysutils" \
    "-Fi$root/FPCSource/rtl/x86_64" \
    "-Fu$root/FPCSource/rtl/unix" \
    "-Fu$root/FPCSource/rtl/objpas" \
    "-Fu$root/FPCSource/rtl/objpas/sysutils" \
    "-Fu$root/FPCSource/rtl/inc" \
    "-Fu$root/FPCSource/rtl/linux" \
    "-Fu$root/FPCSource/rtl/linux/x86_64" \
    "-Fu$root/FPCSource/rtl/x86_64" \
    "-Fu$root/FPCSource/packages/rtl-objpas/src/inc" \
    sysutils.pp >/dev/null 2>&1 || exit 1
)

output="$("$root/build/KGPC/kgpc" "$tmpdir/sysutils.pp" /tmp/sysutils_indexofany.s \
  --no-stdlib \
  "-I$root/FPCSource/rtl/unix" \
  "-I$root/FPCSource/rtl/objpas" \
  "-I$root/FPCSource/rtl/objpas/sysutils" \
  "-I$root/FPCSource/rtl/inc" \
  "-I$root/FPCSource/rtl/linux" \
  "-I$root/FPCSource/rtl/linux/x86_64" \
  "-I$root/FPCSource/rtl/x86_64" \
  "-I$root/FPCSource/packages/rtl-objpas/src/inc" 2>&1 || true)"

if echo "$output" | rg -q "IndexOfAnyUnQuoted does not match any available overload|IndexOfAny does not match any available overload|GetAnsiString, not enough arguments"; then
  exit 0
fi
exit 1
EOF
chmod +x /tmp/cvise_indexofany.sh

cvise --timeout 7200 /tmp/cvise_indexofany.sh sysutils_indexofany.pp
```

### Error categories (9 errors + 1 warning):
| Count | Error | Root Cause |
|-------|-------|------------|
| 5 | InitExceptions/InitInternational/FreeDriveStr/etc. not declared | Procedures in conditional compilation blocks not parsed |
| 1 | Join argument type mismatch | array of ShortString vs array of const |
| 1 | LowerCase result type (Char vs String) | Type alias resolution in FPC source |
| 1 | TryStrToDWord overload not found | Integer vs DWord parameter mismatch |
| 1 | OnBeep procedure assignment | Procedure variable type mismatch |

**Fixed issues (this PR):**
- ShortString to String parameter compatibility (31 errors fixed)
- Class constructor Self parameter matching (18+ errors fixed)
- VAR parameter pointer type compatibility
- Open array parameters (e.g., `array of ShortString`) now forward descriptor pointers correctly when passed to other overloads
- Open array arguments now spill descriptor pointers before nested calls, and dynamic arrays pass descriptor addresses (fixes nested helper overloads and `Length` on open arrays)

**Note**: The "LowerCase" function result warning is caused by FPC source (sysutils/sysstr.inc) using `{ELSE}` instead of `{$ELSE}`. This is now treated as a warning (matching FPC's behavior) rather than an error.

### Error Message Improvements (ba7dcf7)

Error context now shows the source file name from `{#line}` directives:
```
Error on line 250, incompatible types in assignment for S ...
  In ./FPCSource/rtl/objpas/sysutils/sysstrh.inc:
     248 | ...
  >  250 | Function ByteType(...
```

**Known issue**: Some error messages show wrong locations because multiple include files have overlapping logical line numbers. The AST stores line numbers but not filenames at the node level.

## Until Combinator "Swallowing" Content (No Fallbacks)

When compiling `sysutils.pp` with `KGPC_DEBUG_UNTIL=1`, you may see huge `[UNTIL]` spans such as:
- `fmtflt.inc:138` (`Function GetSections...`) with `len=283883`
- `system.inc:63` (`public name 'FPC_ERRORBASE'`) with `len=871527`

Historically, these were **not** the `until` combinator failing to find its delimiter. The delimiter shown in the debug log corresponded to the `implementation_end_marker` (initialization/finalization/exports/end.), meaning the parser had already fallen back to **implementation recovery** and was skipping forward to the next section boundary.

**Current status:** all fallback paths around declarations and function bodies have been removed (implementation recovery, interface-level fallback, and program-function body fallbacks). That means large `[UNTIL]` spans now indicate a *real* parse that is skipping to a delimiter inside the same declaration (or a different explicit recovery path if reintroduced later).

**Conclusion:** if you see a giant `[UNTIL]` span in `sysutils.pp`, treat it as evidence of a *real* parse error earlier in that declaration. The `until` combinator is doing what it was asked to do; the fix is to resolve the underlying parse mismatch.

## Case Labels With Char-Code Literals (sysutils FNMatch)

`sysutils.pp` uses `case` labels with char-code literals in `FNMatch`:
```
case Pattern[i] of
  #128..#255: ...
```

The case-label parser only accepted integer/char literals and identifiers, so `#NNN` was rejected. That caused the entire `FNMatch` implementation to fail parsing, which then made the unit parser stop and report `Expected keyword 'end'` at the next top-level declaration.

**Fix:** allow `char_code_literal` (`#NNN` / `#$NN`) as a valid `case` label expression.

## Case Labels Must Be Constant Expressions (No Calls)

FPC rejects function calls as `case` labels (they are not constant expressions):
```
case x of
  func(): ...  // invalid
end;
```

The parser previously accepted this form because labels were parsed as generic expressions.
That caused statement parsing to succeed when it should fail and masked real errors.

**Fix:** guard case-label parsing so ordinary function calls reject the label, but
allow compile-time builtins (`Low`, `High`, `Ord`, `Chr`, `Succ`, `Pred`, `Length`).
This matches FPC and keeps `case` parsing deterministic while still permitting
common constant expressions like `Low(A)..High(A)`.

## Expression Statements Must Parse (FNMatch)

`sysutils.pp` contains standalone expression statements that are not procedure calls. Example in `FNMatch`:
```
{UTF8:=StringCodePage}(Name)=CP_UTF8;
```
After comment stripping, this is a bare relational expression statement. FPC accepts it, but the parser
had been tightened to only allow expression statements that were procedure calls. That made the parser
reject this line and misreport a later `Expected keyword 'end'` at the next top-level declaration.

**Fix:** accept full expression statements again and represent them as a distinct statement kind.
Expression statements now typecheck and codegen by evaluating the expression and discarding the result.

## Nested Routine Bodies Misparsed (fmtflt.inc)

`fmtflt.inc` contains nested procedures/functions inside `IntFloatToTextFmt`, followed by the **real** function `begin` at line 318. The parse error shows up at line 138 (`Function GetSections`) because the parser is effectively treating an **inner** `begin` (from a nested routine) as the **outer** function body. That causes the outer function to end early and leaves the remaining nested routines as “unexpected content before final '.'”.

Repro (isolated wrapper):
```bash
cat > /tmp/wrap_fmtflt.p <<'EOF'
program t;
{$define FPChar:=PAnsiChar}
{$define FChar:=AnsiChar}
{$define FString:=AnsiString}
{$I /home/kreijstal/git/Pascal-Compiler/FPCSource/rtl/objpas/sysutils/fmtflt.inc}
begin
end.
EOF

./build/KGPC/kgpc /tmp/wrap_fmtflt.p /tmp/wrap_fmtflt.s --no-stdlib
```

Observed failure:
```
Line 138: Function GetSections...
Unexpected content before final '.'
```

**Root cause:** the parser originally treated the **first** `begin` it saw as the outer function body, even when nested routine headers appeared first. In `fmtflt.inc`, the first `begin` belongs to `procedure InitVars`, so the outer function body started too early and the remaining nested routines were left unparsed.

**Implication:** This is not an `until` bug. It was a function-body recovery bug in `program_function_body` that has now been removed; nested routine headers are consumed as declarations, and the parser hard-errors instead of skipping to the next `begin`.

## Proc-Type Cast Call Chaining (objpas.inc)

`objpas.inc` uses a common FPC pattern to invoke a procedure variable after casting:
```
TDispProc(DispCallByIDProc)(Result,IDispatch(Dispatch),DispDesc,Params);
```

The expression parser only allowed function calls on identifiers (e.g. `foo(x)`), not **call chaining** where the result of a cast/call is invoked immediately (`expr(...)(...)`). That caused the `begin...end` block inside `fpc_dispatch_by_id` to fail, and the parser then reported `Expected keyword 'end'` at the next top‑level declaration.

**Fix:** allow `call` as a postfix suffix in the expression parser, so any expression (including typecasts) can be followed by an argument list.

## Inherited Used As Expression (objpas.inc)

`objpas.inc` assigns the result of an inherited method call:
```
NewInstance := inherited NewInstance;
```

The expression parser previously rejected `inherited` as a valid expression starter (only statements supported it), so the assignment RHS failed to parse and the method body never closed.

**Fix:** allow `inherited <identifier>` in expression contexts by parsing it as a function call expression.

## Preprocessor Macros Default On (Compiler Tests)

FPC treats macro expansion as enabled by default; `{$macro off}` explicitly disables it.
The compiler test `preprocessor_macro_default_on.p` relies on this behavior:
```
{$define MYTYPE:=ShortString}
type
  TMy = MYTYPE;
```

**Fix:** set `macro_enabled` default to `true` in the preprocessor to match FPC.

## `specialize` Typecasts in Assignments (objpas.inc)

`TMarshal.FixArray` uses `specialize` as a typecast on the LHS:
```
specialize TArray<T>(Result) := Arr;
```

The lvalue parser did not understand `specialize` typecasts, so the assignment failed to parse and the method body never closed.

**Fix:** support `specialize <generic-type>(expr)` as a typecast in both expression and lvalue parsing.

## Units with Compilation Errors

- `baseunix.pp` - **0 errors** ✓
- `sysutils.pp` - **9 errors, 1 warning** (with `--no-stdlib`)
- `math.pp` - Depends on sysutils
