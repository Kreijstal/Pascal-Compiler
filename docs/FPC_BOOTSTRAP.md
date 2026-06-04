# FPC Bootstrap Analysis

## Status

- **Linux:** Stage3 bootstrap verified (see below). KGPC→FPC 3-stage fixpoint completes.
- **Native Windows (win64/UCRT64):** Self-host **fixpoint reached** — `pp_win.exe` (KGPC-built FPC)
  compiles `pp.pas` into a working `pp_win2.exe` that boots and runs (`-iV` → `3.3.1`, RC=0), and the
  chain converges to a **byte-identical stage4==stage5 fixpoint**. One residual KGPC sign-extension
  bug remains (delays canonical convergence from stage3 to stage4 but does not break the bootstrap).
  See [Native Windows self-host fixpoint](#native-windows-self-host-fixpoint) below.

## Status: Stage3 Bootstrap Verified (Linux)

As of PR #732 merged into `master`, the FPC bootstrap path is green:

1. KGPC compiles and links `FPCSource/compiler/pp.pas` as `pp_bootstrap`.
2. `pp_bootstrap` compiles `FPCSource/compiler/pp.pas` as `pp_stage2`.
3. `pp_stage2 -h` starts successfully and prints the FPC compiler help banner.
4. The generated stage3 compiler can compile and run a hello-world program.
5. CI `fpc-rtl-tests` passes, along with the Linux, MSYS2, and Windows cross-compile checks.

The previous stage3 startup failure was caused by an AnsiString record field
receiving a ShortString local through the wrong assignment helper. That copied
the ShortString length byte into the managed string payload, producing bad paths
such as `!./FPCSource/compiler/pgenutil.pas`. Codegen now emits the
ShortString-to-AnsiString helper for that typed assignment case.

## Prerequisites

Clone the FPC source code:
```bash
git clone https://github.com/fpc/FPCSource
```

Regenerate the compiler message includes before attempting `pp.pas`:
```bash
make -B -C ./FPCSource/compiler msg
```

Without this, a stale `FPCSource/compiler/msgtxt.inc` can be a documentation-text
file instead of the generated Pascal include, which makes `verbose.pas` fail in
preprocessing with a malformed compiler directive error.

## Build Commands

### Recommended (match make -n order)
```bash
make -n -B -C ./FPCSource/rtl/linux units
```

Then compile each unit in that order with KGPC using `--no-stdlib` and the include
paths listed below. This matches the FPC RTL bootstrap sequence and avoids
ordering issues.

### system.pp (0 errors, 158 warnings)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/linux/system.pp /tmp/system.s \
  --no-stdlib \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/x86_64 \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64
```

### fpintres.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/inc/fpintres.pp /tmp/fpintres.s \
  --no-stdlib \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/x86_64 \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/objpas/classes \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64
```

### unixtype.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/unix/unixtype.pp /tmp/unixtype.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

### ctypes.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/inc/ctypes.pp /tmp/ctypes.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

### baseunix.pp (0 errors)
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

### objpas.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/objpas/objpas.pp /tmp/objpas.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

### sysconst.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/objpas/sysconst.pp /tmp/sysconst.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/objpas/sysutils \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

### unix.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/unix/unix.pp /tmp/unix.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

### sysutils.pp (0 errors)
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

### classes.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/unix/classes.pp /tmp/classes.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/objpas/sysutils \
  -I./FPCSource/rtl/objpas/classes \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64 \
  -I./FPCSource/packages/rtl-objpas/src/inc
```

### types.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/objpas/types.pp /tmp/types.s \
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

### math.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/objpas/math.pp /tmp/math.s \
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

### pp.pas (direct KGPC invocation)
```bash
./build-fpc/KGPC/kgpc FPCSource/compiler/pp.pas tests/output/pp_bootstrap.s --no-stdlib \
  -DCPU64 -DCPUX86_64 -Dx86_64 -DFPC -DLINUX -DUNIX -DFPC_HAS_TYPE_EXTENDED -DSUPPORT_EXTENDED -DFPC_BOOTSTRAP_INDIRECT_ENTRY -Sg \
  -IFPCSource/rtl/objpas \
  -IFPCSource/rtl/objpas/sysutils \
  -IFPCSource/rtl/objpas/classes \
  -IFPCSource/rtl/linux \
  -IFPCSource/rtl/unix \
  -IFPCSource/rtl/inc \
  -IFPCSource/rtl/x86_64 \
  -IFPCSource/rtl/linux/x86_64 \
  -IFPCSource/rtl/unix/x86_64 \
  -IFPCSource/compiler \
  -IFPCSource/compiler/x86 \
  -IFPCSource/compiler/x86_64 \
  -FuFPCSource/rtl/unix \
  -FuFPCSource/rtl/linux \
  -FuFPCSource/rtl/objpas \
  -FuFPCSource/rtl/inc \
  -FuFPCSource/rtl/objpas/sysutils \
  -FuFPCSource/rtl/objpas/classes \
  -FuFPCSource/compiler \
  -FuFPCSource/compiler/x86 \
  -FuFPCSource/compiler/x86_64 \
  -FuFPCSource/compiler/systems
```

Then link the generated assembly with the KGPC runtime:
```bash
cc -O2 -no-pie \
  -o tests/output/pp_bootstrap \
  tests/output/pp_bootstrap.s \
  build-fpc/KGPC/libkgpc_runtime.a
```

The Meson test harness is the preferred way to run the complete bootstrap flow,
because it also ensures `msgtxt.inc` and `msgidx.inc` exist by compiling and
running `compiler/utils/msg2inc.pp` when needed:
```bash
meson setup build-fpc -Drun_fpc_rtl_tests=true
ninja -C build-fpc KGPC/kgpc KGPC/libkgpc_runtime.a
KGPC_FPC_RTL=1 \
KGPC_FPC_RTL_DIR=FPCSource \
KGPC_RUNTIME_LIB="$PWD/build-fpc/KGPC/libkgpc_runtime.a" \
MESON_BUILD_ROOT="$PWD/build-fpc" \
CC=cc \
python3 tests/do_not_run_me_directly_but_through_meson.py \
  TestCompiler.test_fpcrtl_pp_pas_bootstrap
```

This writes:
```text
tests/output/pp_bootstrap.s
tests/output/pp_bootstrap
tests/output/pp_stage2/pp_stage2
```

The test verifies the full bootstrap chain:

1. KGPC compiles and links `FPCSource/compiler/pp.pas` as `pp_bootstrap`
2. `pp_bootstrap` compiles and runs `tests/test_cases/helloworld.p`
3. `pp_bootstrap` recompiles `FPCSource/compiler/pp.pas` as `pp_stage2`
4. `pp_stage2 -h` matches the expected bootstrap banner

Note: `-Dx86_64` is required (FPC's Makefile passes `-dx86_64` for x86_64 targets).
The x86/x86_64/systems subdirectories match FPC's `-Fux86 -Fix86 -Fux86_64 -Fix86_64 -Fusystems`.
`-DFPC_HAS_TYPE_EXTENDED -DSUPPORT_EXTENDED` are needed so `systemh.inc` defines `TExtended80Rec`
(used by `math.pp`'s `Frexp`/`Ldexp`). These flags propagate to all unit preprocessing.

`-DFPC_BOOTSTRAP_INDIRECT_ENTRY` makes the system unit declare `operatingsystem_parameter_argc`,
`operatingsystem_parameter_argv`, and `operatingsystem_parameter_envp` as `public name` globals
(emitted as `.comm` by codegen). Without it, they're declared `external name` and expected to come
from ASM startup object files (`si_c.inc` via `{$L}`), which KGPC doesn't support.

### Compile `pp.pas` with the generated `pp_bootstrap`

Build same-source RTL units first so the generated compiler consumes matching
`.ppu` files instead of any host-installed cache:

```bash
make -C FPCSource/compiler ppcx64 FPC="$(command -v fpc)"
make -C FPCSource/rtl/linux all FPC="$PWD/FPCSource/compiler/ppcx64"
```

Then rebuild the compiler with explicit unit/include/output paths:

```bash
mkdir -p tests/output/pp_stage2/units

./tests/output/pp_bootstrap -n \
  -FEtests/output/pp_stage2 \
  -FUtests/output/pp_stage2/units \
  -opp_stage2 \
  -FuFPCSource/rtl/units/x86_64-linux \
  -FiFPCSource/compiler \
  -FiFPCSource/compiler/x86 \
  -FiFPCSource/compiler/x86_64 \
  -FuFPCSource/compiler \
  -FuFPCSource/compiler/x86 \
  -FuFPCSource/compiler/x86_64 \
  -FuFPCSource/compiler/systems \
  FPCSource/compiler/pp.pas

./tests/output/pp_stage2/pp_stage2 -h
```

### Compile hello world with the generated `pp_bootstrap`

Pass the same-source FPC RTL unit path explicitly. Do not rely on guessed
search paths or host-generated `.ppu` files.
```bash
./tests/output/pp_bootstrap -n \
  -FEtests/output \
  -ohelloworld \
  -FuFPCSource/rtl/units/x86_64-linux \
  tests/test_cases/helloworld.p

./tests/output/helloworld
```

Expected output:
```text
Hello, World!
```

### Compile hello world with stage3

After `TestCompiler.test_fpcrtl_pp_pas_bootstrap` has produced
`tests/output/pp_bootstrap`, it can compile a standalone program directly:

```bash
tmpdir=$(mktemp -d /tmp/kgpc-hello.XXXXXX)
cat > "$tmpdir/hello.pas" <<'EOF'
program hello;
begin
  writeln('hello world');
end.
EOF

tests/output/pp_bootstrap \
  -Fu"$PWD/FPCSource/rtl/units/x86_64-linux" \
  -FE"$tmpdir" \
  -o"$tmpdir/hello" \
  "$tmpdir/hello.pas"

"$tmpdir/hello"
```

Expected output:
```text
hello world
```

The generated compiler also supports a quick startup check:
```bash
./tests/output/pp_bootstrap -h
```

## Native Windows (MSYS2 / win11) build of `pp.pas`

The win-target `pp.pas` is compiled to assembly by KGPC (run natively on the
Windows host) via `scripts/cross_compile_pp_win.py`, which emits
`/tmp/pp_win.s`. That assembly is then assembled and linked into `pp_win.exe`
by `scripts/native_build_pp_win.sh`.

**Toolchain — use `/mingw64`, never `/usr/bin`.** On MSYS2, `/usr/bin/gcc` is
the MSYS *POSIX* compiler (a Cygwin fork): it defines `__MSYS__` / `__CYGWIN__`
/ `__unix__` and does **not** define `_WIN32` / `_WIN64` / `__MINGW64__`.
Building KGPC's C runtime with it is fatal in two ways:

1. `KGPC/runtime_fpc_init.c`'s Win64 indirect-entry-information shim
   (`kgpc_fpc_init_win_entry_info`, which points `_FPC_SysInstance`,
   `_FPC_TlsKey`, and `WStrInitTablesTable` at backing storage before any
   Pascal init runs) is guarded on `_WIN32`/`_WIN64`/`__MINGW*`, so under the
   MSYS gcc it compiles as a **no-op**. `_FPC_SysInstance` then stays NULL when
   `rtl/win64/system.pp`'s initialization runs, and the very first
   `FPCSysInstance^ := getmodulehandle(nil)` (system.pp:436) **SIGSEGVs**
   writing to address 0 — before any output.
2. The runtime objects are Cygwin-ABI, mismatched against the Win64-PE
   `pp_win.o`; the resulting memory corruption garbles command-line filenames
   (`hi.pas` → `??.pas`) and the startup banner. This was long mistaken for a
   KGPC codegen "UAF"; it is purely a build-toolchain mismatch.

`scripts/native_build_pp_win.sh` hard-codes `/mingw64` (override with
`MINGW_PREFIX`) and refuses to run if the selected gcc does not define
`_WIN64`. Build and smoke-test:

```bash
# on the Windows host, after cross_compile_pp_win.py has produced /tmp/pp_win.s
bash scripts/native_build_pp_win.sh          # -> /tmp/pp_win.exe
/tmp/pp_win.exe                              # prints clean banner, exit 0
/tmp/pp_win.exe doesnotexist.pas             # Fatal: Cannot open file "doesnotexist.pas"
```

The cross-host counterpart (linking from Linux with the
`x86_64-w64-mingw32` toolchain) is `scripts/cross_build_pp_win.sh`; it is
correct for the same reason — that prefix also defines `__MINGW64__`.

## Native Windows self-host fixpoint

Once `pp_win.exe` exists (KGPC compiles `pp.pas` → asm → linked exe, per the section
above), it can self-host: compile `pp.pas` into `pp_win2.exe`, which compiles `pp_win3.exe`,
and so on until a byte-identical fixpoint. This is the win64 analogue of the Linux 3-stage
bootstrap and is the strongest correctness check on win64 codegen.

### ⚠️ The single most important lesson: delete the `/tmp` unit cache first

**FPC caches compiled units. `-FE/tmp` leaves ~1500 `.ppu`/`.o` files in `/tmp`, and FPC
*relinks* any unit whose source is unchanged instead of regenerating it.** This means a
fixpoint build can silently relink stale object files produced by an *older* compiler — so
bugs that were fixed long ago reappear, frozen, in the output, and look exactly like live
codegen bugs.

This caused a multi-day red herring: `pp_win2.exe -iV` SIGSEGV'd at startup (RC=139) on a
garbage stack-dealloc `lea -0x44b675e0(%rsp),%rsp; pop %rbp` in win64 `_fin$NNNN`
finalization-helper epilogues. The displacement was deterministic across rebuilds, which
*looked* like a reproducible `final_localsize`/`tg.lasttemp` codegen bug. It was not: gdb on
the running build showed `tcgx86_64.g_proc_exit` **never** computed a garbage `final_localsize`
(the breakpoint fired zero times across full builds), because the build was **link-only** —
only `pp.pas`'s own main proc was compiled fresh; every other unit, including the ones owning
the garbage `_fin` funclets (`SYSTEM`, `LINK`, `COMPILER`), was relinked from frozen `.o` built
by a *pre-fix* `pp_win.exe`. Deleting the cache and rebuilding produced a `pp_win2` with **zero**
garbage leas that boots and runs.

**Always run this before any fixpoint build or before debugging "broken" win64 output:**
```bash
rm -f /tmp/*.ppu /tmp/*.o
```

### The fixpoint build command (win64, run on the Windows host)

```bash
# on the Windows host (UCRT64 shell), with /tmp/pp_win.exe already built
export MSYS2_ARG_CONV_EXCL="*"          # RUN steps: stop MSYS mangling the FPC switches
cd ~/git/Pascal-Compiler/FPCSource/compiler
rm -f /tmp/*.ppu /tmp/*.o               # <-- mandatory; see lesson above
R=$(cygpath -m ~/git/Pascal-Compiler); F=$R/FPCSource; C=$F/compiler
PPU=$F/rtl/units/x86_64-win64           # prebuilt win64 RTL .ppu (90 units)

/tmp/pp_win.exe -Twin64 -Px86_64 -dx86_64 -dGDB -Sg \
  -Fi$C -Fi$C/x86 -Fi$C/x86_64 -Fi$F/rtl/inc -Fi$F/rtl/x86_64 -Fi$F/rtl/win -Fi$F/rtl/win/wininc \
  -Fu$PPU -Fu$C -Fu$C/x86 -Fu$C/x86_64 -Fu$C/systems \
  -FE/tmp -o$(cygpath -m /tmp/pp_win2.exe) pp.pas

export MSYS2_ARG_CONV_EXCL="*"; /tmp/pp_win2.exe -iV   # -> 3.3.1, exit 0 (no SIGSEGV)
```

Repeat with `pp_win2.exe` → `pp_win3.exe`, `pp_win3.exe` → `pp_win4.exe`, deleting the cache
each time, then `cmp` consecutive stages. Path notes: build/run steps that invoke the FPC
binaries pass Windows paths via `cygpath -m`; the `-FE` exe-output dir doubles as the unit
cache (hence the `/tmp` cleanup).

### Convergence result and how to read it

| stage | built by | garbage `_fin` leas | `-iV` | vs previous |
|---|---|---|---|---|
| 1 `pp_win.exe` | KGPC | — | — | — |
| 2 `pp_win2` | stage1 | 0 | `3.3.1` RC=0 | — |
| 3 `pp_win3` | stage2 | 0 | `3.3.1` RC=0 | ≠ stage2 (~176k bytes) |
| 4 `pp_win4` | stage3 | 0 | `3.3.1` RC=0 | ≠ stage3 (**4 bytes**) |
| 5 `pp_win5` | stage4 | 0 | `3.3.1` RC=0 | **== stage4 (FIXPOINT)** |

- **stage2 vs stage3 (~176k bytes):** uniform ±0x10 address-layout shifts (RIP-relative and
  absolute references to symbols that moved by 0x10). **Benign** — expected because stage1 is
  KGPC-built and makes slightly different instruction-selection/layout choices than self-built FPC.
- **stage3 vs stage4 (4 bytes):** a **value** difference (see residual bug below). Converges by
  stage4; stage4==stage5 byte-identical → true fixpoint.

### Residual KGPC bug: negative SmallInt constant zero-extended in the self-host path

A correct bootstrap reaches the fixpoint at **stage3** (stage2==stage3, since everything from
stage2 on is identical FPC source built by FPC logic). Ours needs **stage4**, and the extra
generation is a genuine KGPC bug, not cosmetic.

The 4 diverging bytes are at `.text` file offset `0xAD202`, in proc
`SYMDEF.TABSTRACTPROCDEF.TYPENAME_PARAS`, at a call to FPC's `Str(real)` helper
`SYSTEM_$$_STR_REAL$SMALLINT$SMALLINT$DOUBLE...` with the default-format sentinels
`width=-1`, `precision=-32767`:

| reg / param | stage3 (emitted by stage2 = KGPC-built FPC) | stage4 (emitted by stage3 = self-built FPC) |
|---|---|---|
| `edx` = SmallInt `-1` | `mov $0xffff,%edx` (zero-ext = 65535, **wrong**) | `mov $0xffffffff,%edx` (sign-ext = −1, right) |
| `ecx` = SmallInt `-32767` | `mov $0x8001,%ecx` (32769, **wrong**) | `mov $0xffff8001,%ecx` (−32767, right) |

**The bug is indirect.** KGPC's *surface* handling of negative SmallInt constants is correct —
minimal `foo(-1,-32767)`, typed `const A: smallint = -1`, and `-O2` all sign-extend properly
(`negl`/`movslq`, or `movswl` for a typed const) on both `--target=sysv` and `--target=windows`.
The defect is that KGPC **miscompiled FPC's own constant-emitter** inside `pp_win2`, so stage2's
FPC-logic emits that `str_real` sentinel path zero-extended; FPC's correct logic re-asserts at
stage3 (hence self-healing). Because the trigger lives in the self-host path, **no minimal `.p`
reproduces it** — chasing it means bisecting which FPC compiler routine (the constant →
32-bit-immediate parameter materialization used when lowering `Str(real)`) KGPC mis-translated.
This is the same sign/zero-extension family as the earlier SmallInt/var-narrow fixes.

### Debugging the running compiler on the Windows host (gdb gotchas)

When you do need to gdb the running `pp_win.exe` during a build, several Windows/MSYS-specific
traps cost real time:

- **Use gdbserver + remote gdb client, and pass Windows paths.** Native gdb/gdbserver can't
  resolve POSIX `/tmp/...` paths for the executable, the `-x` script, or `file`; translate with
  `cygpath -m`.
- **`strip --strip-debug` a copy of the binary first.** gdb 17.2 chokes on the FPC-emitted DWARF
  (`-dGDB` → `-g`) with "cannot get C stack". Stripping debug info does **not** move `.text`
  addresses, so symbol resolution still works.
- **Move `sitecustomize.py` aside.** `/ucrt64/lib/python3.12/sitecustomize.py`'s faulthandler
  crashes gdb; restore it after (a shell `trap` works well).
- **Disable the NT debug heap: `export _NO_DEBUG_HEAP=1`.** A process launched under a debugger
  otherwise uses the debug heap, which fills allocations with marker patterns and guard bytes —
  this *perturbs* uninitialized-memory bugs and makes them manifest differently (e.g. a garbage
  value that was baked into an instruction instead becomes a garbage filename string). With the
  debug heap off, gdbserver reproduces the exact standalone behavior (byte-identical output).
- **`set breakpoint condition-evaluation host`.** gdbserver evaluates breakpoint *conditions*
  target-side by default and silently no-ops on expressions it can't handle, so a conditional
  breakpoint never fires even though it's correctly placed. Forcing host-side evaluation is
  reliable (but slower, since every hit round-trips to the client).
- **Remember it's usually a link-only build.** If a breakpoint in codegen (`g_proc_exit`, etc.)
  never fires, the units are cached — delete `/tmp/*.ppu /tmp/*.o` to force recompilation before
  concluding the function "isn't called".

### Cleaning generated bootstrap outputs

Bootstrap and FPC RTL runs generate large local artifacts under `tests/output/`.
Use the cleanup helper to inspect or remove them:

```bash
scripts/clean_test_outputs.sh
scripts/clean_test_outputs.sh --yes
```

To keep the most useful generated compiler artifacts while deleting other test
outputs:

```bash
scripts/clean_test_outputs.sh --yes --keep-bootstrap
```

### FPC RTL (56 units)

All 56 FPC RTL units now compile with 0 semantic errors via `meson test -C build-fpc "FPC RTL tests"`.
The build-fpc directory uses `meson setup build-fpc -Drun_fpc_rtl_tests=true`.

### FPC Compiler Units (99 units scanned standalone)

All 99 compiler `.pas` files fail when compiled individually. Total: 257 errors.

Top error categories:
| Category | Count | Root Cause |
|---|---|---|
| Enum/Tconstexprint relational ops | 42 | `Tconstexprint` is a record with operator overloading (not yet supported) |
| Overload resolution | 27 | Parameter type mismatches (`^record` subtype compatibility) |
| Field not found / field access | 27 | Class field resolution, nested types, property getters |
| Assignment type mismatch | 26 | Various type incompatibilities |
| Type mismatch on arithmetic ops | 25 | `Tconstexprint` operator overloading |
| Array literal element mismatch | 11 | Constant array type coercion |
| `^record` vs `procedure` | 11 | Class instances typed as procedure (constructor resolution) |
| String concat method shadowing | 8 | Built-in `Concat` shadows `TAsmList.Concat()` method |
| Char/String mismatch | 7 | Implicit Char↔String conversion |

### pp.pas (full compiler entry point)

Compiling `pp.pas` loads ~200+ units together and produces ~16,000+ errors due to
cascading from the root causes above.

## Performance

### Hash Table Size

`TABLE_SIZE` was increased from 211 to 4099 (prime). With 267 units merged into
a flat scope, the old 211-bucket hash table had severe collisions (~hundreds of
entries per bucket), making every identifier lookup O(n).

### Semantic Analysis of Imported Implementation Bodies

`pp.pas` now completes with full semantic analysis and code generation on the
normal path. Recent profiling on this checkout is approximately:
- parse user units: `3.4s`
- semantic analysis: `130s`
- code generation: `74s`
- total pipeline: `212s`

The main remaining performance cost is full semantic analysis across the merged
compiler unit graph, followed by code generation. Optimization work should
target those hot paths directly rather than relying on a reduced-analysis mode.
### Parser Cache

The AST parser cache (`kgpc_ast_cache_<hash>`) is keyed on the binary hash.
Every recompile invalidates the cache, requiring a full re-parse of all 267
units (~44 seconds uncached vs ~3-5 seconds cached).

## Remaining Blockers (0 errors in pp.pas)

All semantic errors resolved. pp.pas compiles and generates code successfully.

Previous blockers that were resolved:
- TExtended80Rec typecast errors (15) — fixed by passing `-DFPC_HAS_TYPE_EXTENDED -DSUPPORT_EXTENDED`
- Operator overloading, type helper resolution, overload resolution, etc. — all resolved in earlier iterations

### References
- Keep using the FPC-declared order from `make -n -B -C ./FPCSource/rtl/linux units`
  for RTL bootstrap work.
- For compiler bootstrap, `make -n -C ./FPCSource/compiler ppcx64` shows that
  FPC expects the RTL to be prebuilt into `../rtl/units/x86_64-linux` and then
  compiles `pp.pas` in one top-level invocation using:
  - `-Fu../rtl/units/x86_64-linux`
  - `-Fux86_64`
  - `-Fux86`
  - `-Fusystems`

## Flags

- `--no-stdlib` loads the minimal KGPC prelude and then compiles the FPC RTL
  unit directly (required for `system.pp` and the bootstrap sequence so FPC’s
  `system.pp` is used instead of the KGPC stdlib).
  - When compiling RTL units, always include `-I./FPCSource/rtl/linux/x86_64`
    so `stat.inc` and other arch-specific includes resolve.
  - For `sysutils.pp`, include `-I./FPCSource/rtl/objpas/sysutils` to resolve
    `sysutilh.inc` and other sysutils include files.

## Meson Test Suite

Failing compiler invocations dropped from **58 to 7** after fixing:
1. `codegen_sizeof_type_tag` missing `BYTE_TYPE` (1 byte), `WORD_TYPE` (2 bytes), `LONGWORD_TYPE` (4 bytes), `QWORD_TYPE` (8 bytes)
2. `PASCAL_T_NONE` empty statements reaching `convert_statement` and hitting the unsupported default case
3. Plain record properties (Delphi advanced records) triggering `record_type_is_class` heuristic

Additionally, `kgpc_type_sizeof` was missing cases for `BYTE_TYPE`, `WORD_TYPE`, `LONGWORD_TYPE`, and `QWORD_TYPE`, causing SizeOf to fail for arrays of these types (e.g., `array[0..3] of Byte`).

All 3 Pos(char, string) test failures fixed by:
1. Removing Pos from `builtin_arg_expects_string()` (semcheck already dispatches to typed overloads)
2. Adding `mangled_call_expects_char()` to suppress spurious char-to-string promotion
3. Swapping `_ca`/`_cs` runtime signatures to consistent `(ch, value)` argument order

The `**` (power/dot-product) operator is now supported in the expression parser,
unblocking types.pp up to the qualified-default-parameter issue.

Qualified identifiers in case labels (`THorzRectAlign.Left:`) now parse and
resolve correctly. Fixed by using `pascal_qualified_identifier()` in the
case expression parser and adding dot-split resolution in `semcheck_varid()`
for scoped enum values and unit-qualified constants.

Generic class procedures and functions inside advanced records (`generic class
procedure Foo<T>(...)`) now parse correctly by adding `optional(generic)` and
`create_method_type_param_list()` to the record member parsers.

The `specialize` expression now supports multiple comma-separated arguments
(`TFoo.specialize A<T>(X, Dest)`) instead of just a single argument, unblocking
the TBitConverter methods in types.pp.

## Error Reduction with C-Vise (Flatten-Only Preprocessor)

When minimizing failures, use the preprocessor's `--flatten-only` mode to expand `{$i ...}` includes into a single file while keeping compiler directives intact for FPC to evaluate. This avoids corrupting conditional branches during reduction.

### Flatten a unit
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
```

## FPC RTL Build Order (from make -n)

The FPC RTL builds from `FPCSource/rtl/linux/` with these flags:
```
ppcx64 -Fi../inc -Fi../x86_64 -Fi../unix -Fix86_64 -FE. -FU../../rtl/units/x86_64-linux
```

Build order (relevant units):
1. system.pp
2. fpintres.pp
3. si_prc.pp
4. si_c.pp
5. si_g.pp
6. si_dll.pp
7. uuchar.pp
8. unixtype.pp
9. ctypes.pp
10. baseunix.pp
11. strings.pp
12. objpas.pp
13. sysconst.pp
14. unixutil.pp
15. syscall.pp
16. unix.pp
17. errors.pp
18. initc.pp
19. linux.pp
20. sysutils.pp (with `-I./FPCSource/rtl/objpas/sysutils`)
