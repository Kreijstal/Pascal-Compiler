# Test discovery and FPC RTL unit detection for the KGPC test harness.
import os
import re
from pathlib import Path

from .env import FPC_RTL_DIR, IS_NATIVE_WINDOWS

# ---------------------------------------------------------------------------
# FPC RTL known-unit cache
# ---------------------------------------------------------------------------

_FPC_RTL_KNOWN_UNITS = None


def _collect_pascal_unit_names(root_dir):
    names = set()
    if not os.path.isdir(root_dir):
        return names

    for dirpath, _, filenames in os.walk(root_dir):
        for filename in filenames:
            lower = filename.lower()
            if lower.endswith((".pp", ".p", ".pas")):
                names.add(os.path.splitext(filename)[0].lower())
    return names


def _get_fpc_rtl_known_units():
    global _FPC_RTL_KNOWN_UNITS
    if _FPC_RTL_KNOWN_UNITS is not None:
        return _FPC_RTL_KNOWN_UNITS

    roots = [
        FPC_RTL_DIR,
        os.path.join(os.environ.get("KGPC_FPC_RTL_DIR", "FPCSource"), "packages", "rtl-objpas", "src"),
        os.path.join(os.environ.get("KGPC_FPC_RTL_DIR", "FPCSource"), "packages", "rtl-console", "src"),
    ]
    unit_names = set()
    for root in roots:
        unit_names.update(_collect_pascal_unit_names(root))
    _FPC_RTL_KNOWN_UNITS = unit_names
    return _FPC_RTL_KNOWN_UNITS


def _strip_pascal_comments(text):
    result = []
    i = 0
    n = len(text)
    while i < n:
        if text.startswith("//", i):
            j = text.find("\n", i)
            if j == -1:
                break
            result.append("\n")
            i = j + 1
            continue
        if text.startswith("{", i):
            j = text.find("}", i + 1)
            if j == -1:
                break
            i = j + 1
            continue
        if text.startswith("(*", i):
            j = text.find("*)", i + 2)
            if j == -1:
                break
            i = j + 2
            continue
        result.append(text[i])
        i += 1
    return "".join(result)


def _extract_used_units(source_text):
    cleaned = _strip_pascal_comments(source_text)
    lower = cleaned.lower()
    units = set()
    i = 0
    while True:
        idx = lower.find("uses", i)
        if idx == -1:
            break

        before_ok = idx == 0 or not (lower[idx - 1].isalnum() or lower[idx - 1] == "_")
        after_idx = idx + 4
        after_ok = after_idx >= len(lower) or lower[after_idx].isspace()
        if not before_ok or not after_ok:
            i = idx + 4
            continue

        semi = cleaned.find(";", after_idx)
        if semi == -1:
            break
        clause = cleaned[after_idx:semi]
        for raw_unit in clause.split(","):
            token = raw_unit.strip()
            if not token:
                continue
            token = token.split("in", 1)[0].strip()
            if not token:
                continue
            unit_name = token.split(".")[-1].strip().lower()
            if unit_name:
                units.add(unit_name)
        i = semi + 1
    return units


def should_include_in_fpcrtl(base_name, pascal_file, fpc_rtl_implicit_unit_tests):
    if base_name in fpc_rtl_implicit_unit_tests:
        return True

    try:
        source_text = Path(pascal_file).read_text(encoding="utf-8", errors="replace")
    except OSError:
        return True

    used_units = _extract_used_units(source_text)
    if not used_units:
        return False

    known_units = _get_fpc_rtl_known_units()
    for unit_name in used_units:
        if unit_name in known_units:
            return True
    return False


# ---------------------------------------------------------------------------
# Bootstrap / FPC compiler path helpers
# ---------------------------------------------------------------------------

def _bootstrap_arch_defines(prefix):
    """Architecture defines (target-independent)."""
    return [
        f"-{prefix}CPU64",
        f"-{prefix}CPUX86_64",
        f"-{prefix}x86_64",
        f"-{prefix}FPC",
    ]


def _bootstrap_target_defines(prefix):
    """Target-OS defines.  Windows targets get -DWINDOWS -DWIN64; everything
    else assumes Linux/POSIX, which is the only other supported bootstrap
    target today."""
    if IS_NATIVE_WINDOWS:
        return [f"-{prefix}WINDOWS", f"-{prefix}WIN64", f"-{prefix}MSWINDOWS"]
    return [f"-{prefix}LINUX", f"-{prefix}UNIX"]


def _bootstrap_define_flags():
    """Returns KGPC-style define flags for invoking the KGPC compiler itself."""
    return [
        *_bootstrap_arch_defines("D"),
        *_bootstrap_target_defines("D"),
        "-DFPC_HAS_TYPE_EXTENDED",
        "-DSUPPORT_EXTENDED",
        "-DFPC_BOOTSTRAP_INDIRECT_ENTRY",
        "-Sg",
    ]


def _fpc_bootstrap_define_flags():
    """Returns FPC-style define flags for invoking a compiled FPC binary (pp_bootstrap etc.)."""
    return [
        *_bootstrap_arch_defines("d"),
        *_bootstrap_target_defines("d"),
        "-dFPC_HAS_TYPE_EXTENDED",
        "-dSUPPORT_EXTENDED",
        "-dFPC_BOOTSTRAP_INDIRECT_ENTRY",
        "-Sg",
    ]


def _bootstrap_rtl_include_dirs(fpc_src):
    shared = [
        os.path.join(fpc_src, "rtl", "objpas"),
        os.path.join(fpc_src, "rtl", "objpas", "sysutils"),
        os.path.join(fpc_src, "rtl", "objpas", "classes"),
        os.path.join(fpc_src, "rtl", "inc"),
        os.path.join(fpc_src, "rtl", "x86_64"),
    ]
    if IS_NATIVE_WINDOWS:
        # rtl/win is the shared-Windows source layer (sysutils.pp, dos.pp,
        # windirs.pp, wininc/); rtl/win64 is the Windows+x86_64 layer
        # (system.pp, classes.pp, signals.pp, windows.pp).
        return shared + [
            os.path.join(fpc_src, "rtl", "win"),
            os.path.join(fpc_src, "rtl", "win64"),
            os.path.join(fpc_src, "rtl", "win64", "x86_64"),
        ]
    return shared + [
        os.path.join(fpc_src, "rtl", "linux"),
        os.path.join(fpc_src, "rtl", "unix"),
        os.path.join(fpc_src, "rtl", "linux", "x86_64"),
        os.path.join(fpc_src, "rtl", "unix", "x86_64"),
    ]


def _bootstrap_rtl_unit_dirs(fpc_src):
    shared = [
        os.path.join(fpc_src, "rtl", "objpas"),
        os.path.join(fpc_src, "rtl", "inc"),
        os.path.join(fpc_src, "rtl", "objpas", "sysutils"),
        os.path.join(fpc_src, "rtl", "objpas", "classes"),
        # FPC charmap units (cp1250.pas, cp1251.pas, …, cp8859_1.pas) each
        # declare a same-named `unicodemap` typed-const that registers itself
        # via `registermapping(@unicodemap)` in initialization.  pp_bootstrap
        # consults this chain via getmap(); if KGPC's per-unit storage keys
        # are wrong the chain becomes self-referential and runtime lookups
        # (e.g. widestr.pas:391) crash.
        os.path.join(fpc_src, "rtl", "charmaps"),
    ]
    if IS_NATIVE_WINDOWS:
        return [
            os.path.join(fpc_src, "rtl", "win"),
            os.path.join(fpc_src, "rtl", "win64"),
        ] + shared
    return [
        os.path.join(fpc_src, "rtl", "unix"),
        os.path.join(fpc_src, "rtl", "linux"),
    ] + shared


def _bootstrap_compiler_include_dirs(fpc_src):
    return [
        os.path.join(fpc_src, "compiler"),
        os.path.join(fpc_src, "compiler", "x86"),
        os.path.join(fpc_src, "compiler", "x86_64"),
    ]


def _bootstrap_compiler_unit_dirs(fpc_src):
    return [
        os.path.join(fpc_src, "compiler"),
        os.path.join(fpc_src, "compiler", "x86"),
        os.path.join(fpc_src, "compiler", "x86_64"),
        os.path.join(fpc_src, "compiler", "systems"),
    ]


def kgpc_bootstrap_flags(fpc_src, *, include_compiler_dirs,
                         fpc_rtl_ast_cache_dir=None,
                         fpc_rtl_codegen_cache_dir=None):
    flags = ["--no-stdlib"]
    flags.extend(_bootstrap_define_flags())
    flags.extend("-I" + path for path in _bootstrap_rtl_include_dirs(fpc_src))
    if include_compiler_dirs:
        flags.extend("-I" + path for path in _bootstrap_compiler_include_dirs(fpc_src))
    flags.extend("-Fu" + path for path in _bootstrap_rtl_unit_dirs(fpc_src))
    if include_compiler_dirs:
        flags.extend("-Fu" + path for path in _bootstrap_compiler_unit_dirs(fpc_src))
    if fpc_rtl_ast_cache_dir is not None:
        flags.append("--pp-cache-dir=" + fpc_rtl_ast_cache_dir)
    if fpc_rtl_codegen_cache_dir is not None:
        flags.append("--codegen-cache-dir=" + fpc_rtl_codegen_cache_dir)
    return flags


def pp_bootstrap_compiler_flags(
    fpc_src, *, rtl_units_dir, output_dir, unit_output_dir, executable_name
):
    absolute_fpc_src = os.path.abspath(fpc_src)
    flags = [
        "-n",
        "-FE" + output_dir,
        "-FU" + unit_output_dir,
        "-o" + executable_name,
        "-Fu" + rtl_units_dir,
    ]
    flags.extend(_fpc_bootstrap_define_flags())
    # pp_bootstrap rebuilds pp.pas with cwd inside FPCSource/compiler, so these
    # paths must be absolute rather than repository-root-relative.
    flags.extend(
        "-Fi" + path for path in _bootstrap_compiler_include_dirs(absolute_fpc_src)
    )
    flags.extend(
        "-Fu" + path for path in _bootstrap_compiler_unit_dirs(absolute_fpc_src)
    )
    return flags


def pp_bootstrap_program_flags(*, rtl_units_dir, output_dir, executable_name):
    return [
        "-n",
        "-FE" + output_dir,
        "-o" + executable_name,
        "-Fu" + rtl_units_dir,
    ]


def _tree_contains_newer_file(root_dir, reference_file, fpc_rtl_generated_units_dirname):
    if not os.path.isdir(root_dir) or not os.path.isfile(reference_file):
        return False
    reference_mtime = os.path.getmtime(reference_file)
    for current_root, dirnames, filenames in os.walk(root_dir):
        # Skip generated RTL output so only source-tree changes invalidate the
        # same-source unit cache. This must rewrite dirnames in place so
        # os.walk does not descend into the filtered directories.
        dirnames[:] = [
            dirname for dirname in dirnames
            if dirname != fpc_rtl_generated_units_dirname
        ]
        for filename in filenames:
            path = os.path.join(current_root, filename)
            if os.path.getmtime(path) > reference_mtime:
                return True
    return False
