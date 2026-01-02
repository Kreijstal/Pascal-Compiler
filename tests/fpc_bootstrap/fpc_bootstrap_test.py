#!/usr/bin/env python3
"""
FPC Bootstrap Test Runner

This script tests KGPC's ability to compile FPC's RTL units.
It clones the FPC source (if not present) and attempts to compile
each unit in the correct order, tracking which succeed and which fail.

Usage:
    python fpc_bootstrap_test.py [--tap]
"""

import os
import sys
import subprocess
import tempfile
import shutil
from pathlib import Path
from typing import Optional, List, Tuple

# FPC source repository
FPC_REPO_URL = "https://github.com/fpc/FPCSource.git"

# Units to compile in order (from make -n output for x86_64-linux)
# Format: (unit_path_relative_to_rtl_linux, extra_include_dirs, extra_flags)
RTL_UNITS = [
    ("system.pp", [], ["-Us", "-Sg"]),
    ("../inc/fpintres.pp", [], []),
    ("si_prc.pp", [], []),
    ("si_c.pp", [], []),
    ("si_g.pp", [], []),
    ("si_dll.pp", [], []),
    ("../inc/uuchar.pp", [], []),
    ("../unix/unixtype.pp", [], []),
    ("../inc/ctypes.pp", [], []),
    ("../unix/baseunix.pp", [], []),
    ("../inc/strings.pp", [], []),
    ("../objpas/objpas.pp", ["../objpas"], []),
    ("../objpas/sysconst.pp", [], []),
    ("../unix/unixutil.pp", [], []),
    ("../unix/syscall.pp", [], []),
    ("../unix/unix.pp", [], []),
    ("../unix/errors.pp", [], []),
    ("../unix/initc.pp", [], []),
    ("linux.pp", [], []),
    ("../unix/sysutils.pp", ["../objpas/sysutils"], []),
]


class FPCBootstrapTester:
    def __init__(self, kgpc_path: str, fpc_source_dir: str, work_dir: str):
        self.kgpc_path = kgpc_path
        self.fpc_source_dir = Path(fpc_source_dir)
        self.work_dir = Path(work_dir)
        self.results: List[Tuple[str, bool, str]] = []  # (unit_name, success, error_msg)
    
    def ensure_fpc_source(self) -> bool:
        """Clone FPC source if not present."""
        if not self.fpc_source_dir.exists():
            print(f"Cloning FPC source to {self.fpc_source_dir}...", file=sys.stderr)
            result = subprocess.run(
                ["git", "clone", "--depth=1", FPC_REPO_URL, str(self.fpc_source_dir)],
                capture_output=True,
                text=True
            )
            if result.returncode != 0:
                print(f"Failed to clone FPC source: {result.stderr}", file=sys.stderr)
                return False
        return True
    
    def get_include_paths(self, extra_includes: List[str]) -> List[str]:
        """Get the standard include paths for FPC RTL compilation."""
        base_paths = [
            ".",
            "../inc",
            "../x86_64",
            "../unix",
            "x86_64",
        ]
        all_paths = base_paths + extra_includes
        return [f"-I{p}" for p in all_paths]
    
    def compile_unit(self, unit_path: str, extra_includes: List[str], extra_flags: List[str]) -> Tuple[bool, str]:
        """Attempt to compile a single unit with KGPC."""
        rtl_linux_dir = self.fpc_source_dir / "rtl" / "linux"
        
        # Create output file path
        unit_name = Path(unit_path).stem
        output_file = self.work_dir / f"{unit_name}.s"
        
        # Build command
        cmd = [
            self.kgpc_path,
            unit_path,
            str(output_file),
            "--no-stdlib",  # Don't use KGPC's stdlib
        ] + self.get_include_paths(extra_includes) + extra_flags
        
        # Add define for x86_64
        cmd.append("-Dx86_64")
        cmd.append("-DCPU64")
        cmd.append("-DCPUX86_64")
        cmd.append("-DLINUX")
        cmd.append("-DUNIX")
        
        try:
            result = subprocess.run(
                cmd,
                cwd=str(rtl_linux_dir),
                capture_output=True,
                text=True,
                timeout=60
            )
            
            if result.returncode == 0:
                return True, ""
            else:
                error_msg = result.stderr.strip() or result.stdout.strip()
                return False, error_msg
        except subprocess.TimeoutExpired:
            return False, "Compilation timeout (60s)"
        except Exception as e:
            return False, str(e)
    
    def run_tests(self, tap_output: bool = False) -> int:
        """Run all unit compilation tests."""
        if not self.ensure_fpc_source():
            if tap_output:
                print("Bail out! Failed to get FPC source")
            return 1
        
        if tap_output:
            print(f"1..{len(RTL_UNITS)}")
        
        passed = 0
        failed = 0
        
        for i, (unit_path, extra_includes, extra_flags) in enumerate(RTL_UNITS, 1):
            unit_name = Path(unit_path).stem
            success, error_msg = self.compile_unit(unit_path, extra_includes, extra_flags)
            
            self.results.append((unit_name, success, error_msg))
            
            if success:
                passed += 1
                if tap_output:
                    print(f"ok {i} - {unit_name}")
                else:
                    print(f"✓ {unit_name}")
            else:
                failed += 1
                if tap_output:
                    # Escape error message for TAP
                    safe_error = error_msg.replace("\n", " ").replace("#", "")[:200]
                    print(f"not ok {i} - {unit_name}")
                    print(f"# Error: {safe_error}")
                else:
                    print(f"✗ {unit_name}: {error_msg[:200]}")
        
        if not tap_output:
            print(f"\nResults: {passed} passed, {failed} failed out of {len(RTL_UNITS)} units")
        
        return 0 if failed == 0 else 1


def find_kgpc() -> Optional[str]:
    """Find the KGPC compiler executable."""
    # Try common locations
    candidates = [
        os.environ.get("KGPC_PATH"),
        os.path.join(os.environ.get("MESON_BUILD_ROOT", ""), "KGPC", "kgpc"),
        "./builddir/KGPC/kgpc",
        "../builddir/KGPC/kgpc",
    ]
    
    for candidate in candidates:
        if candidate and os.path.isfile(candidate) and os.access(candidate, os.X_OK):
            return candidate
    
    return None


def main():
    import argparse
    parser = argparse.ArgumentParser(description="FPC Bootstrap Test Runner")
    parser.add_argument("--tap", action="store_true", help="Output in TAP format")
    parser.add_argument("--kgpc", type=str, help="Path to KGPC compiler")
    parser.add_argument("--fpc-source", type=str, help="Path to FPC source directory")
    args = parser.parse_args()
    
    # Find KGPC
    kgpc_path = args.kgpc or find_kgpc()
    if not kgpc_path:
        print("Error: Could not find KGPC compiler. Set KGPC_PATH or use --kgpc", file=sys.stderr)
        if args.tap:
            print("Bail out! KGPC not found")
        return 1
    
    # Setup directories
    fpc_source_dir = args.fpc_source or "/tmp/FPCSource"
    work_dir = tempfile.mkdtemp(prefix="fpc_bootstrap_")
    
    try:
        tester = FPCBootstrapTester(kgpc_path, fpc_source_dir, work_dir)
        return tester.run_tests(tap_output=args.tap)
    finally:
        # Cleanup temp directory
        shutil.rmtree(work_dir, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
