# Failure artifact storage and miscellaneous helpers for the KGPC test harness.
import os
import shutil
import signal
from pathlib import Path

from .env import FAILURE_ARTIFACT_DIR


def _signal_name_suffix(returncode):
    if returncode >= 0:
        return ""
    try:
        return f" ({signal.Signals(-returncode).name})"
    except (ValueError, AttributeError):
        return ""


def _sanitize_test_identifier(name):
    return (
        name.replace("/", "_")
        .replace("\\", "_")
        .replace(":", "_")
        .replace(" ", "_")
    )


def _copy_artifact(path, dest_dir):
    if not path:
        return
    src = Path(path)
    if not src.exists():
        return
    try:
        shutil.copy2(src, dest_dir / src.name)
    except OSError:
        pass


def _write_artifact_text(dest_dir, filename, content):
    if content is None:
        return
    target = dest_dir / filename
    target.write_text(str(content), encoding="utf-8", errors="ignore")


def store_failure_artifacts(
    test_id,
    base_name=None,
    *,
    input_file=None,
    asm_file=None,
    executable_file=None,
    expected_file=None,
    compiler_output=None,
    normalized_output=None,
    raw_stdout=None,
    raw_stderr=None,
    expected_output=None,
    expected_stderr=None,
    returncode=None,
    exception_text=None,
):
    if FAILURE_ARTIFACT_DIR is None:
        return

    case_name = base_name or test_id.split(".")[-1]
    dest = FAILURE_ARTIFACT_DIR / _sanitize_test_identifier(case_name)
    dest.mkdir(parents=True, exist_ok=True)

    info_lines = [
        f"test_id={test_id}",
        f"case={case_name}",
    ]
    if returncode is not None:
        info_lines.append(f"returncode={returncode}")
    (dest / "info.txt").write_text(
        "\n".join(info_lines), encoding="utf-8", errors="ignore"
    )

    for candidate in (input_file, asm_file, executable_file, expected_file):
        _copy_artifact(candidate, dest)

    _write_artifact_text(dest, "compiler-stderr.txt", compiler_output)
    _write_artifact_text(dest, "raw-stdout.txt", raw_stdout)
    _write_artifact_text(dest, "raw-stderr.txt", raw_stderr)
    _write_artifact_text(dest, "normalized-output.txt", normalized_output)
    _write_artifact_text(dest, "expected-output.txt", expected_output)
    _write_artifact_text(dest, "expected-stderr.txt", expected_stderr)
    if exception_text:
        _write_artifact_text(dest, "exception.txt", exception_text)


def read_file_content(filepath):
    """Reads and returns the content of a file."""
    with open(filepath, "r") as f:
        return f.read()
