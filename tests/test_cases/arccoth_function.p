program arccoth_function;
var
  r: real;
begin
  r := ArcCoth(2.0);
  writeln('ArcCothTwo=', r:0:4);
  r := ArcCoth(-2.0);
  writeln('ArcCothMinusTwo=', r:0:4);
end.
