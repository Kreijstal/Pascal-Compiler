# Sanitizer / instrumented-run wrappers for the KGPC test harness.
# Covers: valgrind, ASAN/MSAN (via KGPC_SANITIZE), PTY capture, and Wine
# subprocess workarounds.
import locale
import os
import select
import shutil
import subprocess
import sys
import tempfile
import time

from .env import (
    EXEC_TIMEOUT,
    IS_WINE,
    IS_WINDOWS_ABI,
    VALGRIND_MODE,
)

try:
    import pty  # type: ignore
    import termios  # type: ignore
    import fcntl  # type: ignore

    HAS_PTY = True
except ImportError:  # Windows/POSIX-less runtimes
    HAS_PTY = False


def run_executable_with_valgrind(executable_args, **kwargs):
    """Run an executable, optionally with valgrind in CI mode."""
    command = list(executable_args)
    explicit_pty_capture = kwargs.pop("use_pty_capture", None)

    # Use valgrind when CI mode is enabled and valgrind is available
    if VALGRIND_MODE and shutil.which("valgrind") is not None:
        valgrind_cmd = [
            "valgrind",
            "--tool=memcheck",
            "--track-origins=yes",
            "--num-callers=50",
            "--error-exitcode=1",
        ]
        command = valgrind_cmd + command
        print(f"--- Running executable with valgrind: {' '.join(command)} ---", file=sys.stderr)

        if "timeout" not in kwargs:
            kwargs["timeout"] = EXEC_TIMEOUT
        return subprocess.run(command, **kwargs)

    if IS_WINE:
        text_mode = bool(kwargs.get("text"))
        input_data = kwargs.get("input")
        check = bool(kwargs.get("check"))
        capture_output = bool(kwargs.get("capture_output"))
        timeout = kwargs.get("timeout", EXEC_TIMEOUT)

        if timeout is not None:
            # Wine's Windows-Python subprocess waits can fail with WinError 6
            # for short-lived child processes. Use shell redirection instead.
            timeout = None

        with tempfile.TemporaryDirectory() as tmpdir:
            stdout_path = os.path.join(tmpdir, "stdout.txt")
            stderr_path = os.path.join(tmpdir, "stderr.txt")
            stdin_path = os.path.join(tmpdir, "stdin.txt")

            if input_data is not None:
                mode = "w" if text_mode else "wb"
                with open(stdin_path, mode) as handle:
                    handle.write(input_data)

            command_line = subprocess.list2cmdline(command)
            redirect_bits = []
            if input_data is not None:
                redirect_bits.append(f'< "{stdin_path}"')
            redirect_bits.append(f'> "{stdout_path}"')
            redirect_bits.append(f'2> "{stderr_path}"')
            shell_command = f"{command_line} {' '.join(redirect_bits)}"

            returncode = os.system(shell_command)

            stdout_data = None
            stderr_data = None
            if capture_output:
                read_mode = "r" if text_mode else "rb"
                read_kwargs = (
                    {
                        "encoding": locale.getpreferredencoding(False) or "cp1252",
                        "errors": "replace",
                    }
                    if text_mode
                    else {}
                )
                with open(stdout_path, read_mode, **read_kwargs) as handle:
                    stdout_data = handle.read()
                with open(stderr_path, read_mode, **read_kwargs) as handle:
                    stderr_data = handle.read()

            completed = subprocess.CompletedProcess(
                args=command,
                returncode=returncode,
                stdout=stdout_data,
                stderr=stderr_data,
            )
            if check and returncode != 0:
                raise subprocess.CalledProcessError(
                    returncode, command, output=completed.stdout, stderr=completed.stderr
                )
            return completed

    # PTY capture is opt-in. Most tests, and validation against real FPC invoked via
    # subprocess pipes, should observe redirected-output behaviour rather than a fake TTY.
    # Enable KGPC_USE_PTY_CAPTURE=1 when a test genuinely needs terminal semantics.
    #
    # Important: keep stdin as a pipe when input=... is provided, so test input is
    # not echoed by terminal line discipline.
    if explicit_pty_capture is None:
        use_pty_capture = os.environ.get("KGPC_USE_PTY_CAPTURE", "").lower() in ("1", "true", "yes")
    else:
        use_pty_capture = bool(explicit_pty_capture)
    if use_pty_capture and HAS_PTY and kwargs.get("capture_output") and "stdout" not in kwargs and "stderr" not in kwargs:
        text_mode = bool(kwargs.get("text"))
        timeout = kwargs.get("timeout", EXEC_TIMEOUT)
        input_data = kwargs.get("input")
        check = bool(kwargs.get("check"))

        master_fd, slave_fd = pty.openpty()
        try:
            try:
                os.set_blocking(master_fd, False)
            except AttributeError:
                flags = fcntl.fcntl(master_fd, fcntl.F_GETFL)
                fcntl.fcntl(master_fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)
            attrs = termios.tcgetattr(slave_fd)
            attrs[3] &= ~termios.ECHO
            termios.tcsetattr(slave_fd, termios.TCSANOW, attrs)

            proc = subprocess.Popen(
                command,
                stdin=subprocess.PIPE if input_data is not None else None,
                stdout=slave_fd,
                stderr=slave_fd,
                text=text_mode,
                close_fds=True,
            )

            os.close(slave_fd)
            slave_fd = None

            if input_data is not None:
                proc.stdin.write(input_data)
                proc.stdin.close()

            output_bytes = bytearray()
            start = time.monotonic()
            while True:
                if timeout is not None and (time.monotonic() - start) > timeout:
                    proc.kill()
                    break

                r, _, _ = select.select([master_fd], [], [], 0.1)
                if r:
                    try:
                        chunk = os.read(master_fd, 4096)
                    except BlockingIOError:
                        chunk = b""
                    except OSError:
                        break
                    if chunk:
                        output_bytes.extend(chunk)

                if proc.poll() is not None and not r:
                    break

            # Drain remaining bytes
            while True:
                try:
                    chunk = os.read(master_fd, 4096)
                except BlockingIOError:
                    break
                except OSError:
                    break
                if not chunk:
                    break
                output_bytes.extend(chunk)

            returncode = proc.wait()

            if text_mode:
                stdout_text = output_bytes.decode("utf-8", errors="replace")
                while "\r\r\n" in stdout_text:
                    stdout_text = stdout_text.replace("\r\r\n", "\r\n")
                stdout_text = stdout_text.replace("\r\n", "\n").replace("\r", "")
                stdout_val = stdout_text
                stderr_val = ""
            else:
                stdout_val = bytes(output_bytes)
                stderr_val = b""

            completed = subprocess.CompletedProcess(
                args=command,
                returncode=returncode,
                stdout=stdout_val,
                stderr=stderr_val,
            )
            if check and returncode != 0:
                raise subprocess.CalledProcessError(
                    returncode, command, output=completed.stdout, stderr=completed.stderr
                )
            return completed
        finally:
            if slave_fd is not None:
                os.close(slave_fd)
            os.close(master_fd)

    if "timeout" not in kwargs:
        kwargs["timeout"] = EXEC_TIMEOUT
    return subprocess.run(command, **kwargs)


def _write_helper_script(tmpdir, body):
    """Write a small Python helper script used to exercise PTY/non-PTY paths."""
    import textwrap
    script = os.path.join(tmpdir, "pty_helper.py")
    with open(script, "w", encoding="utf-8") as handle:
        handle.write(
            textwrap.dedent(
                f"""\
                #!/usr/bin/env {sys.executable}
                import sys
                import time

                def main():
            """
            )
        )
        for line in textwrap.dedent(body).splitlines():
            handle.write(f"    {line}\n")
        handle.write(
            textwrap.dedent(
                """\
                if __name__ == "__main__":
                    main()
                """
            )
        )
    os.chmod(script, 0o755)
    return script


def _run_helper_with_valgrind(command, *, timeout=None, text=True, input_data=None,
                              capture_output=True, check=False, **kwargs):
    """Wrapper to keep PTY helper tests consistent."""
    runner_kwargs = {
        "timeout": timeout if timeout is not None else EXEC_TIMEOUT,
        "text": text,
        "capture_output": capture_output,
        "check": check,
    }
    if input_data is not None:
        runner_kwargs["input"] = input_data
    runner_kwargs.update(kwargs)
    return run_executable_with_valgrind(command, **runner_kwargs)
