#!/usr/bin/env python3
"""One-shot win-target pp.pas compile.  Forces windows RTL dirs regardless of host."""
import os, sys, subprocess

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir))
os.chdir(REPO)

FPC = "FPCSource"

# Defines for KGPC's compile-time symbol table (KGPC uses -D prefix).
defines = [
    "-DCPU64", "-DCPUX86_64", "-Dx86_64", "-DFPC",
    "-DWINDOWS", "-DWIN64", "-DMSWINDOWS",
    "-DFPC_HAS_INDIRECT_ENTRY_INFORMATION",
    "-DFPC_HAS_TYPE_EXTENDED", "-DSUPPORT_EXTENDED",
    "-DFPC_BOOTSTRAP_INDIRECT_ENTRY",
    "-Sg",
]

# RTL include dirs - windows layout
inc_dirs = [
    os.path.join(FPC, "rtl", "objpas"),
    os.path.join(FPC, "rtl", "objpas", "sysutils"),
    os.path.join(FPC, "rtl", "objpas", "classes"),
    os.path.join(FPC, "rtl", "inc"),
    os.path.join(FPC, "rtl", "x86_64"),
    os.path.join(FPC, "rtl", "win"),
    os.path.join(FPC, "rtl", "win", "wininc"),
    os.path.join(FPC, "rtl", "win64"),
    os.path.join(FPC, "rtl", "win64", "x86_64"),
]
# Compiler include dirs
inc_dirs += [
    os.path.join(FPC, "compiler"),
    os.path.join(FPC, "compiler", "x86"),
    os.path.join(FPC, "compiler", "x86_64"),
]

# RTL unit dirs - windows layout
unit_dirs = [
    os.path.join(FPC, "rtl", "win"),
    os.path.join(FPC, "rtl", "win64"),
    os.path.join(FPC, "rtl", "objpas"),
    os.path.join(FPC, "rtl", "inc"),
    os.path.join(FPC, "rtl", "objpas", "sysutils"),
    os.path.join(FPC, "rtl", "objpas", "classes"),
    os.path.join(FPC, "rtl", "charmaps"),
    os.path.join(FPC, "compiler"),
    os.path.join(FPC, "compiler", "x86"),
    os.path.join(FPC, "compiler", "x86_64"),
    os.path.join(FPC, "compiler", "systems"),
]

flags = (
    ["--no-stdlib", "--target-windows"]
    + defines
    + ["-I" + p for p in inc_dirs]
    + ["-Fu" + p for p in unit_dirs]
    + ["--pp-cache-dir=/tmp/win_cache_pp3"]
)

cmd = [os.path.join(REPO, "build/KGPC/kgpc"),
       os.path.join(FPC, "compiler", "pp.pas"),
       "/tmp/pp_win.s", "--emit-link-args"] + flags

print("CMD prefix:", " ".join(cmd[:4]), "... (+%d flags)" % len(flags), flush=True)

r = subprocess.run(cmd, capture_output=True, text=True, timeout=900)
print("kgpc_exit=", r.returncode)
err = r.stderr.splitlines()
out = r.stdout.splitlines()
print("--- last 40 stderr lines ---")
for l in err[-40:]:
    print(l)
errcount = sum(1 for l in (out + err) if "is not declared" in l)
print("not_declared_count=", errcount)
