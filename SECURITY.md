# Security Policy

## Supported versions

KGPC is pre-1.0 alpha software.  Only the latest `0.0.x` release receives
security fixes; older alpha tags are not maintained.

| Version | Supported          |
|---------|--------------------|
| 0.0.x (latest)  | ✓ |
| anything earlier | ✗ |


## What counts as a security issue

KGPC is a compiler, so the realistic security surface is:

- **Compiler crashes / arbitrary-write bugs triggered by attacker-controlled
  Pascal source.**  If a malicious `.p` file makes `kgpc` execute attacker
  data, that's in scope.
- **Generated code that violates expected isolation** — for example, miscompiles
  that defeat array-bounds checks the source program requested, or that
  introduce executable-stack / W^X violations.
- **Vulnerable behaviour in the shipped runtime** (`KGPC/runtime_*.c`,
  `KGPC/Units/*.p`) that affects programs *correctly written against* those
  units — buffer overflows in `Writeln` formatting, signal-handler races
  in `baseunix`, and similar.
- **Build-system level injections** in `meson.build`, `meson_options.txt`,
  or the test harness that execute attacker code when a developer runs the
  documented build/test commands.

Out of scope: bugs in third-party tools KGPC delegates to (`gcc`, `clang`,
`ld`, FPC itself), and undefined-behaviour issues that only manifest when
deliberately writing malicious Pascal code intended to crash the compiler.


## Reporting a vulnerability

For non-public reports, email **elektrischrainbow@gmail.com** with:

- A description of the issue and its expected impact.
- A minimal `.p` (or other input) that reproduces it.
- The KGPC version (`kgpc --version`) and host details.
- Whether you intend to disclose publicly, and your preferred timeline.

You should expect:

- An acknowledgement within **7 days**.
- A status update within **30 days**, including a fix ETA or a public
  disclosure plan.

Because KGPC is alpha software with a single primary maintainer, response
times are best-effort.  Public GitHub issues are also acceptable for any
report you would be comfortable disclosing immediately.


## Disclosure

Once a fix is released, the relevant CHANGELOG entry will describe the
issue at a level appropriate for the reporter's wishes.  Reporters who
prefer to remain anonymous will be credited as "anonymous reporter."
