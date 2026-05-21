# Parallel test execution infrastructure for the KGPC test harness.
import concurrent.futures
import sys
import threading
import time
import traceback
import unittest

from .env import FPC_RTL_MODE, TAP_MAX_WORKERS


def _flatten_tests(test):
    """Recursively flatten a TestSuite into individual test cases."""
    if isinstance(test, unittest.TestSuite):
        for t in test:
            yield from _flatten_tests(t)
    else:
        yield test


def _prepare_parallel_class_fixtures(tests, log_stream=None):
    """Run setUpClass once per class; return (class_errors, setup_ok_classes)."""
    class_order = []
    seen = set()
    for test in tests:
        cls = test.__class__
        if cls not in seen:
            seen.add(cls)
            class_order.append(cls)

    class_errors = {}
    setup_ok_classes = []
    for cls in class_order:
        try:
            cls.setUpClass()
            setup_ok_classes.append(cls)
        except Exception:
            class_errors[cls] = traceback.format_exc()
            if log_stream is not None:
                log_stream.write(f"[PARALLEL] setUpClass failed for {cls.__name__}\n")
                log_stream.flush()
    return class_errors, setup_ok_classes


def _cleanup_parallel_class_fixtures(setup_ok_classes, log_stream=None):
    """Run tearDownClass for classes whose setUpClass succeeded."""
    for cls in reversed(setup_ok_classes):
        try:
            cls.tearDownClass()
        except Exception:
            if log_stream is not None:
                log_stream.write(f"[PARALLEL] tearDownClass failed for {cls.__name__}\n")
                log_stream.flush()


def _run_single_test_with_timeout(test, timeout, log_stream=None):
    """Run a single test with a timeout. Returns (test, result_type, err_info)."""
    test_id = test.id()
    start_time = time.monotonic()
    if log_stream is not None:
        log_stream.write(f"[PARALLEL] Starting: {test_id}\n")
        log_stream.flush()

    result = unittest.TestResult()
    exception_info = None
    result_type = "success"

    def run_test():
        nonlocal result_type, exception_info
        try:
            test(result)
            if result.failures:
                result_type = "failure"
                exception_info = result.failures[0][1]
            elif result.errors:
                result_type = "error"
                exception_info = result.errors[0][1]
            elif result.skipped:
                result_type = "skipped"
                exception_info = result.skipped[0][1]
        except Exception as e:
            result_type = "error"
            exception_info = traceback.format_exc()

    thread = threading.Thread(target=run_test, daemon=True)
    thread.start()
    thread.join(timeout=timeout)

    elapsed = time.monotonic() - start_time

    if thread.is_alive():
        # Test timed out
        result_type = "timeout"
        exception_info = f"Test timed out after {timeout} seconds"
        if log_stream is not None:
            log_stream.write(f"[PARALLEL] TIMEOUT: {test_id} (after {elapsed:.1f}s)\n")
            log_stream.flush()
        # Note: We cannot forcibly kill the thread, but we can continue
        # The thread will be left as a daemon and cleaned up on exit
    else:
        status = result_type.upper()
        if log_stream is not None:
            log_stream.write(f"[PARALLEL] Finished: {test_id} [{status}] ({elapsed:.1f}s)\n")
            log_stream.flush()

    return test, result_type, exception_info, elapsed


class ParallelTestResult(unittest.TestResult):
    """Thread-safe test result that aggregates results from parallel execution."""

    def __init__(self, stream=None):
        super().__init__()
        self.stream = stream or sys.stderr
        self.lock = threading.Lock()
        self.test_timings = []

    def add_result(self, test, result_type, err_info, elapsed):
        with self.lock:
            self.test_timings.append((test.id(), elapsed, result_type.upper()))
            if result_type == "success":
                self.successes = getattr(self, "successes", 0) + 1
            elif result_type == "failure":
                self.failures.append((test, err_info))
            elif result_type == "error":
                self.errors.append((test, err_info))
            elif result_type == "skipped":
                self.skipped.append((test, err_info))
            elif result_type == "timeout":
                self.errors.append((test, err_info))


class ParallelTestRunner:
    """Run tests in parallel with per-test timeouts."""

    def __init__(self, workers=4, timeout=300, stream=None):
        self.workers = workers
        self.timeout = timeout
        self.stream = stream or sys.stderr

    def run(self, test):
        tests = list(_flatten_tests(test))
        result = ParallelTestResult(self.stream)
        result.testsRun = 0

        self.stream.write(f"[PARALLEL] Running {len(tests)} tests with {self.workers} workers, timeout={self.timeout}s\n")
        self.stream.flush()

        result.startTestRun()
        setup_ok_classes = []
        try:
            class_errors, setup_ok_classes = _prepare_parallel_class_fixtures(tests, self.stream)
            runnable_tests = [t for t in tests if t.__class__ not in class_errors]
            if FPC_RTL_MODE and self.workers > 1:
                assert runnable_tests, (
                    "FPC RTL mode enabled but no runnable tests found. "
                    "Check that KGPC_FPC_RTL=1, FPCSource is present, and test discovery found .expected files."
                )
                # Use longest-job-first scheduling: move pp_pas_bootstrap to
                # the front so it starts early and overlaps with other tests.
                # pp_pas_bootstrap compiles the entire FPC compiler (~150s) and
                # is structurally the longest test — update this if renamed.
                #
                # No warm-up phase needed: the AST cache uses atomic
                # write-to-temp-then-rename (PID-unique temp files), so
                # parallel compiler processes can safely race to populate
                # the same cache entries without corruption.  The first
                # writer wins and subsequent processes read the cached
                # result on the next unit load.  This is faster than
                # blocking all workers on a sequential warm-up test.
                reordered = list(runnable_tests)
                pp_idx = next(
                    (i for i, t in enumerate(reordered)
                     if 'pp_pas_bootstrap' in t._testMethodName),
                    None,
                )
                if pp_idx is not None:
                    reordered.insert(0, reordered.pop(pp_idx))
                self.stream.write(
                    f"[PARALLEL] FPC RTL full parallel: "
                    f"{len(reordered)} tests with {self.workers} workers\n"
                )
                self.stream.flush()
                with concurrent.futures.ThreadPoolExecutor(max_workers=self.workers) as executor:
                    futures = {
                        executor.submit(
                            _run_single_test_with_timeout, t,
                            getattr(getattr(t, t._testMethodName, None), '_timeout', self.timeout),
                            self.stream,
                        ): t
                        for t in reordered
                    }
                    for future in concurrent.futures.as_completed(futures):
                        try:
                            test_case, result_type, err_info, elapsed = future.result()
                            result.add_result(test_case, result_type, err_info, elapsed)
                            result.testsRun += 1
                        except Exception:
                            test_case = futures[future]
                            result.errors.append((test_case, traceback.format_exc()))
                            result.testsRun += 1
            else:
                with concurrent.futures.ThreadPoolExecutor(max_workers=self.workers) as executor:
                    futures = {
                        executor.submit(
                            _run_single_test_with_timeout, t,
                            getattr(getattr(t, t._testMethodName, None), '_timeout', self.timeout),
                            self.stream,
                        ): t
                        for t in runnable_tests
                    }
                    for future in concurrent.futures.as_completed(futures):
                        try:
                            test_case, result_type, err_info, elapsed = future.result()
                            result.add_result(test_case, result_type, err_info, elapsed)
                            result.testsRun += 1
                        except Exception:
                            test_case = futures[future]
                            result.errors.append((test_case, traceback.format_exc()))
                            result.testsRun += 1
            for t in tests:
                if t.__class__ in class_errors:
                    result.add_result(t, "error", class_errors[t.__class__], 0.0)
        finally:
            _cleanup_parallel_class_fixtures(setup_ok_classes, self.stream)
            result.stopTestRun()

        # Print summary
        self.stream.write(f"\n[PARALLEL] Completed: {result.testsRun} tests\n")
        self.stream.write(f"[PARALLEL] Failures: {len(result.failures)}, Errors: {len(result.errors)}, Skipped: {len(result.skipped)}\n")
        self.stream.flush()

        return result


class TAPParallelTestResult(unittest.TestResult):
    """TAP-compatible aggregate result for parallel execution."""

    def __init__(self):
        super().__init__()
        self.test_timings = []

    def add_result(self, test, result_type, err_info, elapsed):
        self.testsRun += 1
        self.test_timings.append((test.id(), elapsed, result_type.upper()))
        if result_type == "success":
            return
        if result_type == "failure":
            self.failures.append((test, err_info))
            return
        if result_type == "error" or result_type == "timeout":
            self.errors.append((test, err_info))
            return
        if result_type == "skipped":
            self.skipped.append((test, err_info))


class TAPParallelTestRunner:
    """Run tests in parallel and emit TAP output deterministically."""

    def __init__(self, workers=4, timeout=300, stream=None, failfast=False):
        self.workers = workers
        self.timeout = timeout
        self.stream = stream or sys.stdout
        self.failfast = failfast

    def _emit(self, line):
        self.stream.write(f"{line}\n")
        self.stream.flush()

    def _emit_diagnostic(self, text):
        for raw_line in str(text).rstrip().splitlines():
            self._emit(f"# {raw_line}")

    def _emit_tap_result(self, tap_index, test_obj, result_type, err_info, result):
        result.add_result(test_obj, result_type, err_info, 0.0)
        test_name = test_obj.id()
        if result_type == "success":
            self._emit(f"ok {tap_index} - {test_name}")
        elif result_type == "skipped":
            self._emit(f"ok {tap_index} - {test_name} # SKIP {err_info}")
        else:
            self._emit(f"not ok {tap_index} - {test_name}")
            label = "Failure" if result_type == "failure" else "Error"
            self._emit_diagnostic(f"{label}:")
            if err_info is not None:
                self._emit_diagnostic(err_info)

    def run(self, test):
        tests = list(_flatten_tests(test))
        result = TAPParallelTestResult()
        result.startTestRun()
        self._emit(f"1..{len(tests)}")
        effective_workers = max(1, min(self.workers, TAP_MAX_WORKERS))

        completed = {}
        next_to_emit = 0  # next index to emit in order
        setup_ok_classes = []
        try:
            class_errors, setup_ok_classes = _prepare_parallel_class_fixtures(tests)

            # Pre-populate class errors
            for idx, t in enumerate(tests):
                if t.__class__ in class_errors:
                    completed[idx] = (t, "error", class_errors[t.__class__], 0.0)

            runnable = [(idx, t) for idx, t in enumerate(tests) if t.__class__ not in class_errors]
            if FPC_RTL_MODE and effective_workers > 1:
                assert runnable, (
                    "FPC RTL mode enabled but no runnable tests found. "
                    "Check that KGPC_FPC_RTL=1, FPCSource is present, and test discovery found .expected files."
                )
                # Use longest-job-first scheduling: move pp_pas_bootstrap to
                # the front so it starts early and overlaps with other tests.
                # pp_pas_bootstrap compiles the entire FPC compiler (~150s) and
                # is structurally the longest test — update this if renamed.
                #
                # No warm-up phase needed: the AST cache uses atomic
                # write-to-temp-then-rename (PID-unique temp files), so
                # parallel compiler processes can safely race to populate
                # the same cache entries without corruption.  The first
                # writer wins and subsequent processes read the cached
                # result on the next unit load.  This is faster than
                # blocking all workers on a sequential warm-up test.
                reordered = list(runnable)
                pp_idx = next(
                    (i for i, (_, t) in enumerate(reordered)
                     if 'pp_pas_bootstrap' in t._testMethodName),
                    None,
                )
                if pp_idx is not None:
                    reordered.insert(0, reordered.pop(pp_idx))
                self._emit(
                    f"# FPC RTL full parallel: "
                    f"{len(reordered)} tests with {effective_workers} workers"
                )
                with concurrent.futures.ThreadPoolExecutor(max_workers=effective_workers) as executor:
                    future_to_index = {
                        executor.submit(
                            _run_single_test_with_timeout, t,
                            getattr(getattr(t, t._testMethodName, None), '_timeout', self.timeout),
                        ): idx
                        for idx, t in reordered
                    }
                    for future in concurrent.futures.as_completed(future_to_index):
                        idx = future_to_index[future]
                        test_case = tests[idx]
                        try:
                            completed[idx] = future.result()
                        except Exception:
                            completed[idx] = (
                                test_case,
                                "error",
                                traceback.format_exc(),
                                0.0,
                            )
                        stop_now = False
                        while next_to_emit in completed:
                            test_obj, result_type, err_info, elapsed = completed[next_to_emit]
                            self._emit_tap_result(next_to_emit + 1, test_obj, result_type, err_info, result)
                            del completed[next_to_emit]
                            next_to_emit += 1
                            if self.failfast and result_type not in ("success", "skipped"):
                                stop_now = True
                                break
                        if stop_now:
                            for f in future_to_index:
                                f.cancel()
                            break
            else:
                with concurrent.futures.ThreadPoolExecutor(max_workers=effective_workers) as executor:
                    future_to_index = {
                        executor.submit(
                            _run_single_test_with_timeout, t,
                            getattr(getattr(t, t._testMethodName, None), '_timeout', self.timeout),
                        ): idx
                        for idx, t in runnable
                    }
                    for future in concurrent.futures.as_completed(future_to_index):
                        idx = future_to_index[future]
                        test_case = tests[idx]
                        try:
                            completed[idx] = future.result()
                        except Exception:
                            completed[idx] = (
                                test_case,
                                "error",
                                traceback.format_exc(),
                                0.0,
                            )
                        # Stream TAP lines for all consecutive completed tests
                        while next_to_emit in completed:
                            test_obj, result_type, err_info, elapsed = completed[next_to_emit]
                            self._emit_tap_result(next_to_emit + 1, test_obj, result_type, err_info, result)
                            del completed[next_to_emit]
                            next_to_emit += 1

            # Emit any remaining results (class errors that were never submitted)
            while next_to_emit < len(tests):
                if next_to_emit in completed:
                    test_obj, result_type, err_info, elapsed = completed[next_to_emit]
                else:
                    test_obj = tests[next_to_emit]
                    result_type = "error"
                    err_info = "Internal error: missing parallel test result"
                self._emit_tap_result(next_to_emit + 1, test_obj, result_type, err_info, result)
                next_to_emit += 1
        finally:
            _cleanup_parallel_class_fixtures(setup_ok_classes)
            result.stopTestRun()
        return result
