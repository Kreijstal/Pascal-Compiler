# MSYS2 / mingw-w64 package: FPC bootstrapped from source via KGPC

`PKGBUILD` builds a Free Pascal Compiler package for MSYS2 **without any existing
FPC**. The whole compiler is produced from source by KGPC (this repo's Pascal
compiler, written in C):

1. **meson** builds the native `kgpc.exe` (C).
2. `scripts/native_build_pp_win.sh`: KGPC compiles FPC's own compiler source
   (`FPCSource/compiler/pp.pas`) into a native Win64 **`ppcx64.exe`**.
3. `ppcx64.exe` self-builds the entire Win64 RTL → ~90 `.ppu` + `.o` in
   `rtl/units/x86_64-win64/`. (No host FPC; the units are genuinely self-built.)
4. `ppcx64.exe` builds the **`fpc.exe`** driver from `compiler/utils/fpc.pp`.
   This step must pass `-dx86_64`: the driver selects its compiler binary
   (`ppcx64`) from `{$ifdef x86_64}`, and FPC's own Makefiles supply that bare
   define as `-d$(CPU_TARGET)` — it is *not* a compiler builtin, so without it
   `ppcbin` comes out empty and `fpc` cannot find the compiler.
5. The result is laid out under `$MINGW_PREFIX`:

   ```
   $MINGW_PREFIX/bin/ppcx64.exe
   $MINGW_PREFIX/bin/fpc.exe
   $MINGW_PREFIX/bin/fpc.cfg
   $MINGW_PREFIX/lib/fpc/3.3.1/units/x86_64-win64/rtl/*.ppu *.o
   ```

   `ppcx64` auto-reads `fpc.cfg` from its own directory; `-FD` lets the `fpc`
   driver locate `ppcx64`, and `-Fu` points at the installed units.

## Build

From a full checkout (with the `FPCSource` tree present), in a **UCRT64 /
MINGW64** MSYS2 shell (the `msys` POSIX gcc is a Cygwin fork and does **not**
define `_WIN64`; the produced `ppcx64` would fault at startup):

```sh
cd packaging/msys2
makepkg-mingw -sCf
pacman -U mingw-w64-*-fpc-kgpc-*.pkg.tar.zst
```

Then, from anywhere:

```sh
fpc yourprogram.pas
```

## Status (verified on win11, MSYS_NT UCRT64)

The full chain runs green: KGPC → `ppcx64.exe` → self-built RTL → `fpc.exe`
driver → assembled package. A built `mingw-w64-ucrt-x86_64-fpc-kgpc-3.3.1-1`
package installs via `pacman -U` and the system `fpc` compiles & runs programs
using `sysutils`/`classes`/`math` with no extra flags.

## Notes

- `makepkg` emits one cosmetic warning — *"Package contains reference to
  `$(cygpath -w /)`"* — because `fpc.cfg` carries the absolute install prefix
  (e.g. `E:\msys64\ucrt64`). This is expected for a fixed-prefix mingw-w64
  package whose config bakes in `$MINGW_PREFIX`.
- Only the RTL is packaged here; FPC's bundled packages (fcl-*, etc.) are a
  follow-on — they build the same way (`ppcx64` against the self-built RTL).
