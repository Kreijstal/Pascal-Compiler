# THIS PROGRAM WILL NOT WORK IF YOU DO NOT COMPILE SOURCES FIRST WITH MESON
#
# This file is the entry point kept for meson compatibility.
# See tests/harness/ for all implementation:
#   env, cache, sanitizers, runner, reporters, parallel, artifacts, discovery,
#   test_compiler, auto_discovery.

# Ensure the project root is in sys.path so that `tests.harness` is importable
# regardless of how this script is invoked (by meson, pytest, or directly).
import sys
import os as _os
_project_root = _os.path.dirname(_os.path.dirname(_os.path.abspath(__file__)))
if _project_root not in sys.path:
    sys.path.insert(0, _project_root)

import argparse
import os
import unittest

from tests.harness.env import (
    FPC_RTL_MODE,
    PARALLEL_WORKERS,
    TEST_CASE_TIMEOUT,
)
from tests.harness.reporters import (
    TAPTestRunner,
    TimingTestResult,
)
from tests.harness.parallel import (
    TAPParallelTestRunner,
    ParallelTestRunner,
)

# Import TestCompiler into this module's namespace so that
# unittest.defaultTestLoader.loadTestsFromModule(sys.modules[__name__])
# discovers it.  Importing auto_discovery triggers dynamic method attachment.
from tests.harness.test_compiler import TestCompiler  # noqa: F401
import tests.harness.auto_discovery  # noqa: F401  (side-effect: populates TestCompiler)


def _load_suite():
    if FPC_RTL_MODE:
        suite = unittest.TestSuite()
        for name in sorted(dir(TestCompiler)):
            if name.startswith('test_fpcrtl_'):
                suite.addTest(TestCompiler(name))
        return suite
    return unittest.defaultTestLoader.loadTestsFromModule(sys.modules[__name__])


def main():
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument("--tap", action="store_true")
    parser.add_argument("--timings-out", dest="timings_out")
    parser.add_argument("--parallel", "-j", type=int, default=None,
                        help="Run tests in parallel with N workers")
    parser.add_argument("--test-timeout", type=int, default=None,
                        help="Per-test timeout in seconds")
    parser.add_argument("--failfast", "-f", action="store_true",
                        help="Stop on first failure/error (TAP runners only)")
    args, remaining = parser.parse_known_args()

    parallel_workers = args.parallel if args.parallel is not None else PARALLEL_WORKERS
    test_timeout = args.test_timeout if args.test_timeout is not None else TEST_CASE_TIMEOUT

    tap_enabled = args.tap or os.environ.get("KGPC_TEST_PROTOCOL", "").lower() == "tap"
    if tap_enabled and args.timings_out:
        print("ERROR: --timings-out is not supported with TAP output.", file=sys.stderr)
        sys.exit(2)

    if args.tap or os.environ.get("KGPC_TEST_PROTOCOL", "").lower() == "tap":
        if remaining:
            suite = unittest.defaultTestLoader.loadTestsFromNames(
                remaining, sys.modules[__name__]
            )
        else:
            suite = _load_suite()
        if parallel_workers > 0:
            runner = TAPParallelTestRunner(workers=parallel_workers, timeout=test_timeout,
                                           failfast=args.failfast)
        else:
            runner = TAPTestRunner(failfast=args.failfast)
        result = runner.run(suite)
        sys.exit(0 if result.wasSuccessful() else 1)

    if parallel_workers > 0:
        if remaining:
            suite = unittest.defaultTestLoader.loadTestsFromNames(
                remaining, sys.modules[__name__]
            )
        else:
            suite = _load_suite()
        runner = ParallelTestRunner(workers=parallel_workers, timeout=test_timeout, stream=sys.stderr)
        result = runner.run(suite)
        slow_tests = [(tid, t, s) for tid, t, s in result.test_timings if t > 30]
        if slow_tests:
            print("\n[PARALLEL] Slow tests (>30s):", file=sys.stderr)
            for test_id, elapsed, status in sorted(slow_tests, key=lambda x: -x[1]):
                print(f"  {elapsed:8.1f}s  {status:8s}  {test_id}", file=sys.stderr)
        sys.exit(0 if result.wasSuccessful() else 1)

    if args.timings_out:
        if remaining:
            suite = unittest.defaultTestLoader.loadTestsFromNames(
                remaining, sys.modules[__name__]
            )
        else:
            suite = _load_suite()
        runner = unittest.TextTestRunner(resultclass=TimingTestResult)
        result = runner.run(suite)
        with open(args.timings_out, "w", encoding="utf-8") as handle:
            for test_id, elapsed, status in sorted(
                result.timings, key=lambda x: x[1], reverse=True
            ):
                handle.write(f"{elapsed:8.3f}s\t{status}\t{test_id}\n")
        sys.exit(0 if result.wasSuccessful() else 1)
    print(f"DEBUG: argv={[sys.argv[0]] + remaining}")

    unittest.main(argv=[sys.argv[0]] + remaining)


if __name__ == "__main__":
    main()
