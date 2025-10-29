# sysrpl/fpc Pascal sweep

We evaluated the vendored `pascal_parser_cli` against the entire `sysrpl/fpc` repository using the helper script [`scripts/run_pascal_parser_over_fpc.py`](../scripts/run_pascal_parser_over_fpc.py). The script parses every `.pas` file and caches results so the run can be resumed. A single sweep on the current checkout covered 2,930 files.

## Summary results

| Metric | Count |
| --- | --- |
| Pascal files analysed | 2,930 |
| Files parsed successfully | 0 |
| Files that failed to parse | 2,930 |

Top failure reasons reported by `pascal_parser_cli`:

| Parser error | Files |
| --- | --- |
| Expected keyword `implementation` | 1,717 |
| Expected keyword `end` | 518 |
| Expected keyword `unit` | 328 |
| Parser timed out (8s budget) | 320 |
| Expected keyword `interface` | 13 |
| Expected `;` after a unit name containing `.` | 10 |
| `deprecated` qualifier rejected after unit name | 4 |

The full JSON summary is stored in [`analysis/fpc_pascal_parser_summary.json`](fpc_pascal_parser_summary.json) and individual per-file records live in [`analysis/fpc_pascal_parser_results.jsonl`](fpc_pascal_parser_results.jsonl).【F:analysis/fpc_pascal_parser_summary.json†L1-L40】

## Major gaps surfaced by the sweep

### Interface features that block `implementation`

The majority of units fail immediately after their `interface` section because constructs such as `resourcestring`, compiler directives, or dynamic-array declarations are not recognised. Example: `packages/pasjpeg/src/jconsts.pas` fails on the `resourcestring` block even before reaching the `implementation` keyword.【F:analysis/fpc_pascal_parser_results.jsonl†L55-L59】

### Units built from include files

Several RTL units only contain an `interface` followed by include directives and a terminating `end.` (no `implementation`). The parser still demands an `implementation` keyword, which breaks files such as `packages/odbc/src/odbcsql.pas`.【F:analysis/fpc_pascal_parser_results.jsonl†L17-L22】

### Dotted identifiers in unit names and uses

Object Pascal code frequently scopes units by namespace. Files like `packages/rtl-objpas/tests/tests.rtti.pas` (`unit tests.rtti;`) and the `Generics.*` family all fail because the grammar rejects dots in unit identifiers or `uses` clauses, producing errors such as “expected `;` but found `.rtti;`/`.Collections;`”.【F:analysis/fpc_pascal_parser_results.jsonl†L210-L215】【F:analysis/fpc_pascal_parser_results.jsonl†L2719-L2724】

### Unit qualifiers (`deprecated`, platform modes, etc.)

FPC allows qualifiers after the unit header. The parser currently stops as soon as it sees `deprecated`, as shown by `tests/webtbf/tw21466.pas`.【F:analysis/fpc_pascal_parser_results.jsonl†L51-L54】

### Stand-alone programs are unsupported

Every file that begins with `program` rather than `unit` fails at the very first token. `packages/numlib/tests/turte.pas` is a representative example.【F:analysis/fpc_pascal_parser_results.jsonl†L223-L226】

### Performance: numerous 8-second timeouts

Even relatively small Cocoa/Objective-C bridge units time out because the parser backtracks heavily (32 worker threads, 8s per file). Examples include `packages/cocoaint/src/CoreGraphics.pas` and `packages/os2units/src/buildall.pas`. These appear in the results with `[TIMEOUT]` outputs.【F:analysis/fpc_pascal_parser_results.jsonl†L96-L104】

## Follow-up

New cparser unit tests have been added to codify these gaps (resourcestrings, dotted identifiers, include-only units, deprecated qualifiers, dotted `uses` clauses, and program files). They currently fail, providing concrete targets for grammar work.
