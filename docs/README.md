# KGPC documentation

KGPC's docs are built with [Sphinx](https://www.sphinx-doc.org/) and
[Hawkmoth](https://hawkmoth.readthedocs.io/) (a libclang-driven
autodoc extension for C).

## Build

```bash
# One-time setup
python3 -m venv .docs-venv
.docs-venv/bin/pip install -r requirements-docs.txt

# Build the HTML site
.docs-venv/bin/sphinx-build -b html docs docs/_build/html

# Open the result
xdg-open docs/_build/html/index.html
```

Sphinx's `conf.py` regenerates [`source_map.rst`](source_map.rst) on every
build from `generate_source_map.py`, so the per-file inventory always
matches the live source tree.

## What's where

| File / dir | Purpose |
|---|---|
| `index.rst` | Top-level TOC |
| `conf.py` | Sphinx configuration |
| `generate_source_map.py` | Walks the C tree and emits `source_map.rst` |
| `ARCHITECTURE.md` | Pipeline + source layout overview |
| `AST_OWNERSHIP_RULE.md` | AST node ownership invariant |
| `FPC_BOOTSTRAP.md` | Self-host bootstrap reference |
| `api/*.rst` | Per-module Hawkmoth pages (only doc-commented decls) |

## Adding API documentation

Put a `/** ... */` block immediately above the declaration in the
relevant `.h` file:

```c
/**
 * Compute the length of `s` up to the first NUL.
 *
 * :param s: NUL-terminated C string.
 * :returns: Byte length excluding the terminator.
 */
size_t kgpc_strlen(const char *s);
```

Then either add a `.. c:autodoc:: path/to/header.h` directive to the
appropriate `api/*.rst` file (if the header isn't listed yet), or
rebuild — existing autodoc directives pick it up automatically.

## CI

The doc build is not yet wired into CI; until it is, run `sphinx-build`
locally before committing changes to `docs/` so you catch broken
cross-references and bad reST syntax.
