program csch_function;
var
  r: real;
begin
  r := Csch(1.0);
  writeln('CschOne=', r:0:4);
  r := Csch(0.5);
  writeln('CschHalf=', r:0:4);
  r := Csch(2.0);
  writeln('CschTwo=', r:0:4);
end.
