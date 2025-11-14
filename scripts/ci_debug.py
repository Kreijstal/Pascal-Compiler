#!/usr/bin/env python3
"""
Utility invoked from CI to run bespoke debug cases that are not part of the
official Meson test suite.  Each case is a standalone Pascal source file that
exercises typed-file behaviour with extra verbosity so we can inspect the
resulting stdout/stderr when diagnosing clang64-only problems.

Usage (from the project root):
    python scripts/ci_debug.py --mode typedfiles

The script discovers *.p files under scripts/clang64_debug, compiles them
with the already-built gpc compiler, links the resulting assembly into an
executable, and finally runs the executable.  Output for each case is stored
under builddir/debug/clang64/logs/<case>.log and also echoed to stdout.
"""

import argparse
import os
import shutil
import subprocess
import sys
from pathlib import Path


def build_dir_from_env() -> Path:
    env_build = os.environ.get("MESON_BUILD_ROOT")
    if env_build:
        return Path(env_build)
    return Path("build")


def run(cmd, **kwargs):
    print(f"[DEBUG] running: {' '.join(cmd)}")
    return subprocess.run(cmd, check=True, **kwargs)


def compile_case(gpc_path: Path, pas_path: Path, asm_path: Path):
    asm_path.parent.mkdir(parents=True, exist_ok=True)
    cmd = [str(gpc_path), str(pas_path), str(asm_path)]
    run(cmd)


def link_case(cc: str, asm_path: Path, exe_path: Path, runtime_lib: Path):
    exe_path.parent.mkdir(parents=True, exist_ok=True)
    cmd = [
        cc,
        "-O2",
        "-no-pie",
        "-o",
        str(exe_path),
        str(asm_path),
        str(runtime_lib),
        "-lm",
    ]
    run(cmd)


def execute_case(exe_path: Path, log_path: Path):
    log_path.parent.mkdir(parents=True, exist_ok=True)
    with log_path.open("w", encoding="utf-8") as log_file:
        result = subprocess.run(
            [str(exe_path)],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            check=False,
        )
        log_file.write(result.stdout)
    return result.returncode, log_path.read_text(encoding="utf-8")


def run_typedfile_suite():
    base_dir = Path("scripts/clang64_debug")
    pas_cases = sorted(base_dir.glob("case*.p"))
    if not pas_cases:
        print("No debug cases found under scripts/clang64_debug", file=sys.stderr)
        sys.exit(1)

    build_root = build_dir_from_env()
    gpc_path = build_root / ("GPC/gpc.exe" if os.name == "nt" else "GPC/gpc")
    if not gpc_path.exists():
        print(f"Compiler not found at {gpc_path}", file=sys.stderr)
        sys.exit(1)

    runtime_lib = build_root / "GPC/libgpc_runtime.a"
    if not runtime_lib.exists():
        print(f"Runtime library not found at {runtime_lib}", file=sys.stderr)
        sys.exit(1)

    cc = os.environ.get("CC", "cc")
    tmp_build = build_root / "debug" / "clang64"
    logs_dir = tmp_build / "logs"
    if logs_dir.exists():
        shutil.rmtree(logs_dir)

    for pas_file in pas_cases:
        case_name = pas_file.stem
        print(f"\n===== Debug case {case_name} =====")
        asm_path = tmp_build / f"{case_name}.s"
        exe_path = tmp_build / f"{case_name}.exe"
        log_path = logs_dir / f"{case_name}.log"

        compile_case(gpc_path, pas_file, asm_path)
        link_case(cc, asm_path, exe_path, runtime_lib)
        returncode, output = execute_case(exe_path, log_path)

        print(output.strip())
        print(f"[{case_name}] exit code: {returncode}")


def main():
    parser = argparse.ArgumentParser(description="CI debug harness")
    parser.add_argument(
        "--mode",
        choices=["typedfiles"],
        required=True,
        help="Select which debug suite to run.",
    )
    args = parser.parse_args()
    if args.mode == "typedfiles":
        run_typedfile_suite()
    else:
        print(f"Unsupported mode: {args.mode}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
