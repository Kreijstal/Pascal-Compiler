# FPC Bootstrap Analysis

## Status: BLOCKED - Compiler Bugs/Limitations

The FPC RTL cannot be fully compiled because kgpc has bugs and missing features that need to be fixed.

## Known Issues to Fix

1. **Type Helper Issues**
   - IndexOfAny/IndexOfAnyUnQuoted overload resolution in type helpers
   - TGUIDHelper.Create type mismatch with type casts

2. **Forward References**
   - SysBeep used before declaration (forward reference support needed)
   - Some procedure overloads not found due to forward reference issues

3. **Overload Resolution**
   - StringReplace ambiguous when ShortString can convert to both AnsiString and UnicodeString
   - Some procedure overloads not matched (InitInternational, InitExceptions, etc.)

## Build Command

### sysutils.pp (28 errors)
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

### Error categories (28 total):
| Count | Error | Root Cause |
|-------|-------|------------|
| 6 | IndexOfAny/IndexOfAnyUnQuoted overload not found | Missing overloads in FPC sysutils |
| 6 | Result type incompatible | Cascading from overload errors |
| 5 | procedure overload not found | Forward reference issues |
| 3 | Create argument type mismatch | Type cast resolution |
| 3 | ShortString assignment | Cascading from earlier errors |
| 3 | SysBeep/OnBeep undeclared | Forward reference support needed |
| 1 | StringReplace ambiguous | Overload resolution |
| 1 | Result real type mismatch | Cascading |

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

- `sysutils.pp` - **28 errors** (with `--no-stdlib`)
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
