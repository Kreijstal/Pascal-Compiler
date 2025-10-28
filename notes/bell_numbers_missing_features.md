# Bell Numbers Sample Status

Running the Bell Numbers regression program with the current compiler toolchain fails during unit loading:

```
$ ./build/GPC/gpc tests/test_cases/bell_numbers.p /tmp/bell.s
ERROR: Failed to open GPC/Units/gmp.p
ERROR: Failed to load unit 'gmp'.
```

The failure occurs because we do not ship a `gmp` unit alongside the compiler. The sample depends on several pieces of functionality that our bundled runtime currently lacks:

- A `gmp` unit that exposes the `MPInteger` type and the `z_*` helpers used in the sample.
- Date/Time and string routines in `SysUtils`, including `TDateTime`, `Now`, `FormatDateTime`, and `IntToStr`.
- Memory helper `Move` with the `array of` overload used in the sample.

Until we provide these runtime features (or adapt the sample to avoid them), the compiler cannot emit runnable code for the Bell Numbers example.
