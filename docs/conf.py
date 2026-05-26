"""
Sphinx configuration for KGPC.

Doc comments in the C source are surfaced via Hawkmoth (libclang-based).
Hand-written guides under docs/*.md and docs/*.rst are pulled in by the
toctree in docs/index.rst.
"""
from __future__ import annotations

import os
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent

# ----------------------------------------------------------------------------
# Project metadata
# ----------------------------------------------------------------------------
project = "KGPC"
author = "Kreijstal"
copyright = "2026, Kreijstal"


def _project_version() -> str:
    env = os.environ.get("KGPC_VERSION")
    if env:
        return env
    # Parse the version literal out of meson.build so docs stay in sync.
    meson_build = (ROOT / "meson.build").read_text(encoding="utf-8")
    for line in meson_build.splitlines():
        stripped = line.strip()
        if stripped.startswith("version"):
            # e.g.   version : '0.0.1',
            try:
                return stripped.split("'")[1]
            except IndexError:
                continue
    return "0.0.0"


version = _project_version()
release = version

# ----------------------------------------------------------------------------
# Extensions
# ----------------------------------------------------------------------------
extensions = [
    "hawkmoth",            # libclang-driven C autodoc
    "myst_parser",         # Markdown via MyST
    "sphinx.ext.todo",
]

# Source paths for Hawkmoth. Each entry maps a Sphinx-side basename to a
# real path; .. c:autodoc:: <path/to/file.h> resolves against these roots.
hawkmoth_root = str(ROOT)
hawkmoth_clang = [
    f"-I{ROOT}",
    f"-I{ROOT / 'KGPC'}",
    f"-I{ROOT / 'cparser'}",
    f"-I{ROOT / 'common'}",
    "-DKGPC_DOC_BUILD=1",
]

# Treat Markdown files as first-class sources alongside .rst.
source_suffix = {
    ".rst": "restructuredtext",
    ".md": "markdown",
}

# MyST settings: enable a few useful extensions without going overboard.
myst_enable_extensions = [
    "colon_fence",
    "deflist",
]
myst_heading_anchors = 3

# ----------------------------------------------------------------------------
# HTML output
# ----------------------------------------------------------------------------
html_theme = "furo"
html_title = f"KGPC {release}"
html_static_path: list[str] = []

exclude_patterns = [
    "_build",
    "Thumbs.db",
    ".DS_Store",
    "README.md",  # the docs/ README is for humans, not the rendered site
]

# Hawkmoth on libclang 22 occasionally warns about unknown attributes in
# system headers; surface them as info, not errors, so the build stays green.
nitpicky = False
todo_include_todos = True


# ----------------------------------------------------------------------------
# Build hooks
# ----------------------------------------------------------------------------
def _regenerate_source_map(app):  # noqa: ARG001 — sphinx callback signature
    """Refresh docs/source_map.rst from the live C source tree."""
    here = Path(__file__).resolve().parent
    subprocess.check_call(
        ["python3", str(here / "generate_source_map.py")],
        cwd=here.parent,
    )


def setup(app):
    app.connect("builder-inited", _regenerate_source_map)
    return {"parallel_read_safe": True, "parallel_write_safe": True}
