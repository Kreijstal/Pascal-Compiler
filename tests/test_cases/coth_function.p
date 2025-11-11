program coth_function;
var
  r: real;
begin
  r := Coth(1.0);
  writeln('CothOne=', r:0:4);
  r := Coth(0.5);
  writeln('CothHalf=', r:0:4);
  r := Coth(2.0);
  writeln('CothTwo=', r:0:4);
end.
