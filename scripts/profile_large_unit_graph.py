#!/usr/bin/env python3
import argparse
import json
import os
import re
import subprocess
import sys
from pathlib import Path


TIME_PATTERNS = {
    "parse_stdlib": re.compile(r"^\[time\] parse stdlib: .* in ([0-9.]+)s$"),
    "parse_units": re.compile(r"^\[time\] parse user units: .* in ([0-9.]+)s$"),
    "parse_user": re.compile(r"^\[time\] parse user program: .* in ([0-9.]+)s$"),
    "semantic": re.compile(r"^\[time\] semantic analysis: ([0-9.]+)s$"),
    "codegen": re.compile(r"^\[time\] code generation: ([0-9.]+)s$"),
    "total": re.compile(r"^\[time\] total \(tracked stages\): ([0-9.]+)s$"),
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Profile large-unit-graph compilation using the #555 pp_readargs repro."
    )
    parser.add_argument("--compiler", required=True, help="Path to kgpc executable")
    parser.add_argument("--fpc-source", required=True, help="Path to FPCSource tree")
    parser.add_argument("--source", default="tests/perf/test_pp_readargs.pas",
                        help="Pascal source file for the perf repro")
    parser.add_argument("--output-dir", default="builddir/perf",
                        help="Directory for generated assembly and logs")
    parser.add_argument("--log-file", default=None,
                        help="Optional path to write the full compiler log")
    parser.add_argument("--max-semantic", type=float, default=None,
                        help="Fail if semantic-analysis time exceeds this many seconds")
    parser.add_argument("--max-codegen", type=float, default=None,
                        help="Fail if code-generation time exceeds this many seconds")
    parser.add_argument("--max-total", type=float, default=None,
                        help="Fail if tracked total time exceeds this many seconds")
    return parser.parse_args()


def build_command(args: argparse.Namespace, asm_path: Path) -> list[str]:
    fpc = Path(args.fpc_source)
    return [
        str(Path(args.compiler)),
        str(Path(args.source)),
        str(asm_path),
        "--no-stdlib",
        "--time-passes",
        "-DCPU64",
        "-DCPUX86_64",
        "-Dx86_64",
        "-DFPC",
        "-DLINUX",
        "-DUNIX",
        "-DFPC_HAS_TYPE_EXTENDED",
        "-DSUPPORT_EXTENDED",
        "-DFPC_BOOTSTRAP_INDIRECT_ENTRY",
        "-Sg",
        f"-I{fpc / 'rtl' / 'objpas'}",
        f"-I{fpc / 'rtl' / 'objpas' / 'sysutils'}",
        f"-I{fpc / 'rtl' / 'objpas' / 'classes'}",
        f"-I{fpc / 'rtl' / 'linux'}",
        f"-I{fpc / 'rtl' / 'unix'}",
        f"-I{fpc / 'rtl' / 'inc'}",
        f"-I{fpc / 'rtl' / 'x86_64'}",
        f"-I{fpc / 'rtl' / 'linux' / 'x86_64'}",
        f"-I{fpc / 'rtl' / 'unix' / 'x86_64'}",
        f"-I{fpc / 'compiler'}",
        f"-I{fpc / 'compiler' / 'x86'}",
        f"-I{fpc / 'compiler' / 'x86_64'}",
        f"-Fu{fpc / 'rtl' / 'unix'}",
        f"-Fu{fpc / 'rtl' / 'linux'}",
        f"-Fu{fpc / 'rtl' / 'objpas'}",
        f"-Fu{fpc / 'rtl' / 'inc'}",
        f"-Fu{fpc / 'rtl' / 'objpas' / 'sysutils'}",
        f"-Fu{fpc / 'rtl' / 'objpas' / 'classes'}",
        f"-Fu{fpc / 'compiler'}",
        f"-Fu{fpc / 'compiler' / 'x86'}",
        f"-Fu{fpc / 'compiler' / 'x86_64'}",
        f"-Fu{fpc / 'compiler' / 'systems'}",
    ]


def build_msg2inc_command(compiler: Path, fpc_source: Path, asm_path: Path) -> list[str]:
    return [
        str(compiler),
        str(fpc_source / "compiler" / "utils" / "msg2inc.pp"),
        str(asm_path),
        "--no-stdlib",
        "-DCPU64",
        "-DCPUX86_64",
        "-Dx86_64",
        "-DFPC",
        "-DLINUX",
        "-DUNIX",
        "-DFPC_HAS_TYPE_EXTENDED",
        "-DSUPPORT_EXTENDED",
        "-Sg",
        f"-I{fpc_source / 'rtl' / 'objpas'}",
        f"-I{fpc_source / 'rtl' / 'objpas' / 'sysutils'}",
        f"-I{fpc_source / 'rtl' / 'objpas' / 'classes'}",
        f"-I{fpc_source / 'rtl' / 'linux'}",
        f"-I{fpc_source / 'rtl' / 'unix'}",
        f"-I{fpc_source / 'rtl' / 'inc'}",
        f"-I{fpc_source / 'rtl' / 'x86_64'}",
        f"-I{fpc_source / 'rtl' / 'linux' / 'x86_64'}",
        f"-I{fpc_source / 'rtl' / 'unix' / 'x86_64'}",
        f"-Fu{fpc_source / 'rtl' / 'unix'}",
        f"-Fu{fpc_source / 'rtl' / 'linux'}",
        f"-Fu{fpc_source / 'rtl' / 'objpas'}",
        f"-Fu{fpc_source / 'rtl' / 'inc'}",
        f"-Fu{fpc_source / 'rtl' / 'objpas' / 'sysutils'}",
        f"-Fu{fpc_source / 'rtl' / 'objpas' / 'classes'}",
    ]


def ensure_bootstrap_prereqs(compiler: Path, fpc_source: Path, output_dir: Path) -> None:
    msgtxt = fpc_source / "compiler" / "msgtxt.inc"
    if msgtxt.exists():
        return

    runtime_lib = compiler.parent / "libkgpc_runtime.a"
    if not runtime_lib.exists():
        raise RuntimeError(f"missing KGPC runtime library: {runtime_lib}")

    asm_path = output_dir / "msg2inc.s"
    exe_path = output_dir / "msg2inc"
    compile_cmd = build_msg2inc_command(compiler, fpc_source, asm_path)
    link_cmd = [
        "cc", "-O2", "-no-pie", "-o", str(exe_path), str(asm_path), str(runtime_lib),
        "-lm", "-lpthread",
    ]
    run_cmd = [str(exe_path.resolve()), "msg/errore.msg", "msg", "msg"]

    print("Preparing FPC bootstrap prerequisites with KGPC:")
    print(" ".join(compile_cmd))
    sys.stdout.flush()
    proc = subprocess.run(compile_cmd, capture_output=True, text=True)
    sys.stdout.write(proc.stdout)
    sys.stderr.write(proc.stderr)
    if proc.returncode != 0:
        raise RuntimeError(f"failed to compile msg2inc with KGPC (exit {proc.returncode})")

    print(" ".join(link_cmd))
    sys.stdout.flush()
    proc = subprocess.run(link_cmd, capture_output=True, text=True)
    sys.stdout.write(proc.stdout)
    sys.stderr.write(proc.stderr)
    if proc.returncode != 0:
        raise RuntimeError(f"failed to link msg2inc (exit {proc.returncode})")

    print(" ".join(run_cmd))
    sys.stdout.flush()
    proc = subprocess.run(
        run_cmd,
        cwd=fpc_source / "compiler",
        capture_output=True,
        text=True,
    )
    sys.stdout.write(proc.stdout)
    sys.stderr.write(proc.stderr)
    if proc.returncode != 0:
        raise RuntimeError(f"failed to generate {msgtxt} with KGPC msg2inc (exit {proc.returncode})")


def parse_timings(output: str) -> dict[str, float]:
    timings: dict[str, float] = {}
    for line in output.splitlines():
        stripped = line.strip()
        for key, pattern in TIME_PATTERNS.items():
            match = pattern.match(stripped)
            if match:
                timings[key] = float(match.group(1))
    return timings


def write_github_summary(summary: str) -> None:
    summary_path = os.environ.get("GITHUB_STEP_SUMMARY")
    if not summary_path:
        return
    with open(summary_path, "a", encoding="utf-8") as f:
        f.write(summary)
        if not summary.endswith("\n"):
            f.write("\n")


def main() -> int:
    args = parse_args()
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    asm_path = output_dir / "test_pp_readargs.s"
    fpc_source = Path(args.fpc_source)
    compiler = Path(args.compiler)
    ensure_bootstrap_prereqs(compiler, fpc_source, output_dir)
    cmd = build_command(args, asm_path)

    print("Running perf probe command:")
    print(" ".join(cmd))
    sys.stdout.flush()
    proc = subprocess.run(cmd, capture_output=True, text=True)
    combined = proc.stdout + proc.stderr

    if args.log_file is not None:
        Path(args.log_file).write_text(combined, encoding="utf-8")

    sys.stdout.write(proc.stdout)
    sys.stderr.write(proc.stderr)

    timings = parse_timings(combined)
    if not timings:
        print("ERROR: Failed to parse timing summary from compiler output.", file=sys.stderr)
        return 2

    summary = {
        "compiler": str(Path(args.compiler)),
        "source": str(Path(args.source)),
        "asm": str(asm_path),
        "compiler_exit_code": proc.returncode,
        "timings": timings,
    }
    print("PROFILE_SUMMARY=" + json.dumps(summary, sort_keys=True))

    gh_summary = ["## Large Unit Graph Perf Probe", "", "```json", json.dumps(summary, indent=2, sort_keys=True), "```", ""]
    write_github_summary("\n".join(gh_summary))

    failures: list[str] = []
    if args.max_semantic is not None and timings.get("semantic", 0.0) > args.max_semantic:
        failures.append(f"semantic {timings['semantic']:.3f}s > budget {args.max_semantic:.3f}s")
    if args.max_codegen is not None and timings.get("codegen", 0.0) > args.max_codegen:
        failures.append(f"codegen {timings['codegen']:.3f}s > budget {args.max_codegen:.3f}s")
    if args.max_total is not None and timings.get("total", 0.0) > args.max_total:
        failures.append(f"total {timings['total']:.3f}s > budget {args.max_total:.3f}s")

    if proc.returncode != 0:
        print(f"WARNING: Compiler exited with {proc.returncode}; retaining timings for perf tracking.",
              file=sys.stderr)
        write_github_summary(
            f"Warning: compiler exited with `{proc.returncode}` during perf probe, "
            "but parsed timing data was still captured.\n"
        )

    if failures:
        for failure in failures:
            print(f"PERF_REGRESSION: {failure}", file=sys.stderr)
        return 3

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
