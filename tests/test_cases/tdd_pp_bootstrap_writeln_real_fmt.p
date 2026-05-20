program tdd_pp_bootstrap_writeln_real_fmt;
{ Regression test for: pp_bootstrap (FPC compiled by KGPC) printed `0.0`
  instead of `3.5` for `writeln(3.5:0:1)`.

  Root cause: KGPC classified an Extended (`bestreal`) value parameter as
  an SSE register argument because `codegen_param_real_storage_size` did
  not recognise the type-alias chain `bestreal = extended` and returned
  10 (the raw sizeof) rather than 16.  The caller therefore passed the
  argument in `xmm0` while the callee read it from `[%rbp+16]`, leaving
  `trealconstnode.create` with value=0.

  Companion bug: extended class fields read via `record_access` (e.g.
  `Self.value_real`) were loaded with a plain `movq (%addr), %reg` that
  truncated 10-byte extended to its low 8 bytes (mantissa).  This made
  FPC's `value_real = 0.0` peephole misfire (low 8 bytes of 1.0 extended
  equal -0.0 as double → equality matched), so `a := 1.0` emitted
  `xorpd %xmm0,%xmm0`.

  Both code paths must keep extended values numerically faithful so the
  generated pp_bootstrap prints `3.5` here. }

begin
  writeln(3.5:0:1);
end.
