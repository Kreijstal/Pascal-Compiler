program ln_exp_function;
var
  val: real;
begin
  val := Ln(1.0);
  writeln('LnOne=', val:0:2);
  val := Ln(Exp(1.5));
  writeln('LnExp=', val:0:2);
  val := Exp(0.0);
  writeln('ExpZero=', val:0:2);
  val := Exp(1.0);
  writeln('ExpOne=', val:0:2);
end.
