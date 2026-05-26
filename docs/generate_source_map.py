#!/usr/bin/env python3
"""
Generate docs/source_map.rst by walking KGPC/, cparser/, and common/.

For every .c / .h file, extract the leading file-comment (first /* ... */
or contiguous //-block) and write it as a one-line description grouped by
directory.  Files without a leading comment are still listed (no blurb).

Run manually:
    python3 docs/generate_source_map.py
or let conf.py run it via the builder-inited hook on every Sphinx build.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OUTPUT = Path(__file__).resolve().parent / "source_map.rst"

TARGETS = ["KGPC", "cparser", "common"]
EXTS = {".c", ".h", ".S"}

BLOCK_RE = re.compile(r"/\*+(.*?)\*+/", re.DOTALL)
LINE_RE = re.compile(r"^\s*//\s?(.*)$")


def leading_doc(path: Path) -> str:
    """Return a one-line summary extracted from the file's leading comment."""
    try:
        text = path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return ""

    stripped = text.lstrip()
    if stripped.startswith("/*"):
        match = BLOCK_RE.match(stripped)
        if match:
            body = match.group(1)
            return _flatten(body)

    # Contiguous // lines at the top.
    lines = []
    for raw in text.splitlines():
        if not raw.strip():
            if lines:
                break
            continue
        m = LINE_RE.match(raw)
        if not m:
            break
        lines.append(m.group(1))
    if lines:
        return _flatten(" ".join(lines))
    return ""


def _flatten(body: str) -> str:
    cleaned: list[str] = []
    for line in body.splitlines():
        line = line.strip().lstrip("*").strip()
        if not line:
            if cleaned:
                break  # stop at the first blank line — first paragraph only
            continue
        cleaned.append(line)
    summary = " ".join(cleaned).strip()
    # Keep it short — first sentence or first 120 chars.
    if "." in summary:
        summary = summary.split(".", 1)[0].strip()
    if len(summary) > 120:
        summary = summary[:117].rstrip() + "..."
    return summary


def collect() -> dict[str, list[tuple[Path, str]]]:
    groups: dict[str, list[tuple[Path, str]]] = {}
    for top in TARGETS:
        base = ROOT / top
        if not base.exists():
            continue
        for path in sorted(base.rglob("*")):
            if path.suffix not in EXTS:
                continue
            if not path.is_file():
                continue
            rel = path.relative_to(ROOT)
            group_key = str(rel.parent)
            groups.setdefault(group_key, []).append((rel, leading_doc(path)))
    return groups


def render(groups: dict[str, list[tuple[Path, str]]]) -> str:
    out = ["Source Map", "==========", ""]
    out += [
        "Every C and assembly source file under ``KGPC/``, ``cparser/``, and",
        "``common/`` is listed here with the first paragraph of its leading",
        "comment (when present).  Regenerated on every Sphinx build by",
        "``docs/generate_source_map.py``.",
        "",
    ]
    for group, entries in sorted(groups.items()):
        out.append(f"``{group}/``")
        out.append("-" * (len(group) + 5))
        out.append("")
        out.append(".. list-table::")
        out.append("   :header-rows: 1")
        out.append("   :widths: 40 60")
        out.append("")
        out.append("   * - File")
        out.append("     - Summary")
        for rel, blurb in entries:
            out.append(f"   * - ``{rel.name}``")
            out.append(f"     - {blurb or '—'}")
        out.append("")
    return "\n".join(out) + "\n"


def main() -> int:
    groups = collect()
    OUTPUT.write_text(render(groups), encoding="utf-8")
    print(f"wrote {OUTPUT.relative_to(ROOT)} "
          f"({sum(len(v) for v in groups.values())} files in "
          f"{len(groups)} dirs)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
