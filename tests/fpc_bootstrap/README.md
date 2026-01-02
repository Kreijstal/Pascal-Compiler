# FPC Bootstrap Tests

This directory contains tests for bootstrapping the Free Pascal Compiler (FPC) RTL with KGPC.

## Goal

The long-term goal is to compile FPC's Runtime Library (RTL) using KGPC as a stepping stone
toward full FPC bootstrap capability.

## FPC RTL Compilation Order (x86_64-linux)

The following units need to be compiled in order:

1. `system.pp` - Core system unit (with `-Us -Sg` flags)
2. `fpintres.pp` - Internal resource handling
3. `si_prc.pp` - Startup code (PascalMain)
4. `si_c.pp` - C-compatible startup
5. `si_g.pp` - GNU-compatible startup
6. `si_dll.pp` - DLL startup
7. `uuchar.pp` - Unicode character support
8. `unixtype.pp` - Unix type definitions
9. `ctypes.pp` - C type definitions
10. `baseunix.pp` - Base Unix syscalls
... and many more

## Running Tests

```bash
# Run FPC bootstrap tests via meson
meson test -C builddir "FPC bootstrap tests"
```

## Status

Track progress by checking which units compile successfully with KGPC.
