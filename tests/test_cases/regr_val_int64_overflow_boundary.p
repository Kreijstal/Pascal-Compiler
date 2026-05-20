{ Regression test for KGPC bug where `val(s, i:int64, code)` accepted strings
  whose numeric value overflowed the signed range and silently wrapped, e.g.
  '9223372036854775808' (one above high(int64)) became low(int64) with code=0.

  This broke pp_bootstrap (FPC self-host).  pexpr.pas / _INTCONST does:

      val(pattern, ic, code);          { try int64 first }
      if code = 0 then int_to_type(ic, hdef)
                  else val(pattern, qc, code);

  With the bug, FPC source like `type R = 0..9223372036854775808;` (or
  `0..18446744073709551615` as in alias_array_typedef_sizeof) made KGPC-built
  pp_bootstrap take the int64 branch and store the high bound as low(int64),
  which is_negative()=true, so the subrange's high-bound was deemed less than
  its low bound, raising "High range limit < low range limit".

  The fix bounds-checks the strtoull result against the requested signed range
  before converting to long long (KGPC/runtime_string.c:kgpc_val_parse_integer).
}
program regr_val_int64_overflow_boundary;

var
  i: int64;
  q: qword;
  c: integer;
begin
  { Just above high(int64): must reject. }
  val('9223372036854775808', i, c);
  if c = 0 then writeln('BAD: int64 wrapped 9223372036854775808 to ', i)
           else writeln('ok: int64 rejected 9223372036854775808');

  { Exactly high(int64): must accept. }
  val('9223372036854775807', i, c);
  if (c = 0) and (i = 9223372036854775807) then writeln('ok: int64 max')
                                           else writeln('BAD: int64 max c=', c, ' i=', i);

  { Exactly low(int64): must accept. }
  val('-9223372036854775808', i, c);
  if (c = 0) and (i = low(int64)) then writeln('ok: int64 min')
                                  else writeln('BAD: int64 min c=', c, ' i=', i);

  { Just below low(int64): must reject. }
  val('-9223372036854775809', i, c);
  if c = 0 then writeln('BAD: int64 wrapped -9223372036854775809 to ', i)
           else writeln('ok: int64 rejected -9223372036854775809');

  { qword can hold 9223372036854775808: must accept. }
  val('9223372036854775808', q, c);
  if (c = 0) and (q = qword(9223372036854775808)) then writeln('ok: qword 2^63')
                                                  else writeln('BAD: qword 2^63 c=', c, ' q=', q);

  { qword max: must accept. }
  val('18446744073709551615', q, c);
  if (c = 0) and (q = high(qword)) then writeln('ok: qword max')
                                   else writeln('BAD: qword max c=', c, ' q=', q);

  { qword overflow: must reject. }
  val('18446744073709551616', q, c);
  if c = 0 then writeln('BAD: qword wrapped 18446744073709551616 to ', q)
           else writeln('ok: qword rejected 18446744073709551616');
end.
