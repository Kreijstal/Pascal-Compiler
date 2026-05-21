# TAP and timing reporters for the KGPC test harness.
import signal
import sys
import time
import traceback
import unittest

from .env import IS_WINDOWS_ABI, TEST_CASE_TIMEOUT


class TAPTestResult(unittest.TestResult):
    # Enable verbose logging to stderr (controlled by env var for CI debugging)
    VERBOSE_LOG = __import__("os").environ.get("KGPC_TEST_VERBOSE", "false").lower() in ("1", "true", "yes")

    def __init__(self, stream):
        super().__init__()
        self.stream = stream
        self._test_index = 0
        self._test_states = {}
        self._test_start_times = {}

    def _emit(self, line):
        text = f"{line}\n"
        encoding = getattr(self.stream, "encoding", None) or "utf-8"
        safe_text = text.encode(
            encoding, errors="backslashreplace"
        ).decode(encoding, errors="replace")
        self.stream.write(safe_text)
        self.stream.flush()

    def _emit_diagnostic(self, text):
        for raw_line in text.rstrip().splitlines():
            self._emit(f"# {raw_line}")

    def _test_name(self, test):
        try:
            return test.id()
        except AttributeError:
            return str(test)

    def _subtest_name(self, subtest):
        if subtest is None:
            return ""
        params = getattr(subtest, "params", None) or {}
        msg = getattr(subtest, "message", None)
        parts = []
        if params:
            formatted = ", ".join(
                f"{key}={value!r}" for key, value in sorted(params.items())
            )
            parts.append(formatted)
        if msg:
            parts.append(str(msg))
        if parts:
            return " | ".join(parts)
        return str(subtest)

    def _get_state(self, test):
        return self._test_states.setdefault(
            test, {"reported": False, "had_failure": False}
        )

    def _emit_failure_header(self, test):
        state = self._get_state(test)
        if not state["reported"]:
            self._emit(f"not ok {self._test_index} - {self._test_name(test)}")
            state["reported"] = True
        return state

    def _mark_failure(self, test):
        state = self._get_state(test)
        state["had_failure"] = True
        return state

    def startTest(self, test):
        super().startTest(test)
        # Enforce per-test timeout in TAP mode to avoid hangs.
        if not IS_WINDOWS_ABI and hasattr(signal, "SIGALRM"):
            method = getattr(test, test._testMethodName, None)
            per_test_timeout = getattr(method, '_timeout', TEST_CASE_TIMEOUT)

            def _timeout_handler(_signum, _frame):
                raise TimeoutError(f"Test timed out after {per_test_timeout} seconds")

            self._prev_alarm_handler = signal.getsignal(signal.SIGALRM)
            signal.signal(signal.SIGALRM, _timeout_handler)
            signal.alarm(per_test_timeout)
        self._test_index += 1
        self._test_states[test] = {"reported": False, "had_failure": False}
        self._test_start_times[test] = time.monotonic()
        # Always emit starting message so we can see which test is running
        self._emit(f"# [STARTING] {self._test_name(test)}")

    def stopTest(self, test):
        elapsed = 0.0
        start_time = self._test_start_times.pop(test, None)
        if start_time is not None:
            elapsed = time.monotonic() - start_time
        if not IS_WINDOWS_ABI and hasattr(signal, "SIGALRM"):
            signal.alarm(0)
            prev = getattr(self, "_prev_alarm_handler", None)
            if prev is not None:
                signal.signal(signal.SIGALRM, prev)
        # Note: stopTest is called after addSuccess/addFailure, so the result line is already emitted
        super().stopTest(test)
        self._test_states.pop(test, None)

    def addSuccess(self, test):
        super().addSuccess(test)
        state = self._test_states.get(test)
        if state and state.get("had_failure"):
            return
        self._emit(f"ok {self._test_index} - {self._test_name(test)}")
        if state:
            state["reported"] = True

    def addSkip(self, test, reason):
        super().addSkip(test, reason)
        self._emit(f"ok {self._test_index} - {self._test_name(test)} # SKIP {reason}")
        state = self._test_states.get(test)
        if state:
            state["reported"] = True

    def addExpectedFailure(self, test, err):
        super().addExpectedFailure(test, err)
        self._emit(
            f"ok {self._test_index} - {self._test_name(test)} # TODO expected failure"
        )
        state = self._test_states.get(test)
        if state:
            state["reported"] = True

    def addUnexpectedSuccess(self, test):
        super().addUnexpectedSuccess(test)
        self._emit(
            f"not ok {self._test_index} - {self._test_name(test)} # Unexpected success"
        )
        self._mark_failure(test)

    def addFailure(self, test, err):
        super().addFailure(test, err)
        self._emit_failure_header(test)
        self._emit_diagnostic("Failure:")
        self._emit_diagnostic("".join(traceback.format_exception(*err)))
        self._mark_failure(test)

    def addError(self, test, err):
        super().addError(test, err)
        self._emit_failure_header(test)
        self._emit_diagnostic("Error:")
        self._emit_diagnostic("".join(traceback.format_exception(*err)))
        self._mark_failure(test)

    def addSubTest(self, test, subtest, err):
        super().addSubTest(test, subtest, err)
        if err is None:
            return
        self._emit_failure_header(test)
        description = self._subtest_name(subtest)
        if description:
            self._emit_diagnostic(f"Subtest failed: {description}")
        else:
            self._emit_diagnostic("Subtest failed")
        self._emit_diagnostic("".join(traceback.format_exception(*err)))
        self._mark_failure(test)


class TAPTestRunner:
    def __init__(self, stream=None, failfast=False):
        self.stream = stream or sys.stdout
        self.failfast = failfast

    def run(self, test):
        result = TAPTestResult(self.stream)
        test_count = test.countTestCases()
        result.startTestRun()
        try:
            result.stream.write(f"1..{test_count}\n")
            result.stream.flush()
            test(result)
        finally:
            result.stopTestRun()
        return result


class TimingTestResult(unittest.TextTestResult):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.timings = []
        self._start_times = {}
        self._status = {}

    def startTest(self, test):
        self._start_times[test] = time.perf_counter()
        super().startTest(test)

    def addSuccess(self, test):
        self._status[test] = "OK"
        super().addSuccess(test)

    def addFailure(self, test, err):
        self._status[test] = "FAIL"
        super().addFailure(test, err)

    def addError(self, test, err):
        self._status[test] = "ERROR"
        super().addError(test, err)

    def addSkip(self, test, reason):
        self._status[test] = "SKIP"
        super().addSkip(test, reason)

    def stopTest(self, test):
        start = self._start_times.pop(test, None)
        elapsed = time.perf_counter() - start if start else 0.0
        self.timings.append((test.id(), elapsed, self._status.get(test, "UNKNOWN")))
        super().stopTest(test)
