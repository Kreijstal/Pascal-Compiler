{ Regression: Val() of a non-decimal ($hex) literal is an unsigned bit pattern
  reinterpreted into the destination's signed width.  $FFFFFFFFFFFFFFF0 is -16
  as Int64 and $8000000000000000 is Low(Int64); both previously reported a
  conversion error because the magnitude was checked against the signed max. }
var
  x: int64;
  c: integer;
begin
  val('$fffffffffffffff0', x, c);
  writeln(c, ' ', x);
  val('$8000000000000000', x, c);
  writeln(c, ' ', x);
  val('$7fffffffffffffff', x, c);
  writeln(c, ' ', x);
end.
