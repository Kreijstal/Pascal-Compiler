# FPC Bootstrap Analysis

## Status: BLOCKED - Compiler Bugs/Limitations

The FPC RTL cannot be fully compiled because kgpc has bugs and missing features that need to be fixed.

## Known Issues to Fix

1. **Type Helper Issues**
   - Undeclared `Self` in some type helper methods
   - Initializer type mismatch with default enum parameters

2. **Type Resolution**
   - Incompatible type assignments (char vs string, string vs pointer)
   - Type mismatches in function arguments

3. **Function Overloads**
   - IndexOfAny/IndexOfAnyUnQuoted overload resolution
   - Format/CurrToStr argument type mismatch
   - Some procedure overloads not matched

4. **Missing Built-ins**
   - `strlen` - String length for PAnsiChar
   - `flush` - Text file flush procedure

## Build Command

### sysutils.pp (67 errors)
```bash
./builddir/KGPC/kgpc ./FPCSource/rtl/unix/sysutils.pp /tmp/sysutils.s \
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

### Error categories (67 total):
| Count | Error | Root Cause |
|-------|-------|------------|
| 14 | incompatible types in assignment for S | String/pointer conversion |
| 12 | undeclared identifier "Self" | Type helper cascading errors |
| 12 | initializer type mismatch | Default parameter issues |
| 7 | incompatible types in assignment for Result | Int64 return type issues |
| 7 | IndexOfAny* overload issues | Overload resolution |
| 5 | procedure overload issues | Missing overloads |
| 4 | Format/CurrToStr argument mismatch | Type conversion |
| 6 | Other | Various |

## Compiles Successfully (RTL Units)

- `system.pp` - Core system unit
- `linux.pp` - Linux unit
- `unix.pp` - Unix unit
- `baseunix.pp` - Base Unix functions
- `objpas.pp` - Object Pascal RTL
- `strings.pp` - String handling
- `ctypes.pp` - C types
- `sysconst.pp` - System constants
- `types.pp` - Type helpers
- `cpu.pp` - CPU types (x86_64)
- `errors.pp` - Unix errors
- `rtlconsts.pp` - RTL constants
- `dl.pp` - Dynamic loading
- `fpintres.pp`, `si_prc.pp`, `si_c.pp`, `si_g.pp`, `si_dll.pp` - Startup units

## Units with Compilation Errors

- `sysutils.pp` - **67 errors** (with `--no-stdlib`)
- `math.pp` - Depends on sysutils
- `cthreads.pp` - Missing ThreadingAlreadyUsed
- `charset.pp` - Type incompatibilities
- `unixcp.pp` - Missing CP_* constants
- `intrinsics.pp` - Missing fpc_in_cpu_first
- `character.pas` - Needs unicodedata unit
- `getopts.pp` - Missing argv from system
- `ports.pp` - x86-specific, not x86_64
- `cmem.pp` - Needs system unit types
- `si_uc.pp` - Missing si_uc.inc for x86_64
