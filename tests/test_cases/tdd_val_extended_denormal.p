program tdd_val_extended_denormal;
{ Val of an 80-bit Extended must accept underflowing (denormal) magnitudes with
  code 0 -- e.g. the sysutils/syshelph.inc Epsilon constant
  3.64519953188247460253e-4951 -- while still rejecting genuine overflow that
  saturates to +Inf. strtold reports ERANGE for both cases, so the runtime must
  distinguish them by whether the parsed result is infinite. The denormal is
  scaled back into normal range with 80-bit arithmetic to prove val stored a
  real nonzero value rather than silently flushing it to zero. }
var
  e, scaled: extended;
  code, i: integer;
begin
  val('3.64519953188247460253e-4951', e, code);
  writeln('denormal code=', code);
  scaled := e;
  for i := 1 to 4951 do
    scaled := scaled * 10.0;
  writeln('denormal recovered >1=', scaled > 1.0);

  val('1e400', e, code);
  writeln('1e400 code=', code);

  val('1e5000', e, code);
  writeln('overflow code<>0=', code <> 0);

  val('1.5', e, code);
  writeln('normal code=', code, ' val=', e:0:1);

  val('bogus', e, code);
  writeln('bad code<>0=', code <> 0);
end.
