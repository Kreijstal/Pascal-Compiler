program csc_function;
var
  r: real;
begin
  r := Csc(Pi / 6);
  writeln('CscPi6=', r:0:4);
  r := Csc(Pi / 4);
  writeln('CscPi4=', r:0:4);
  r := Csc(Pi / 3);
  writeln('CscPi3=', r:0:4);
end.
