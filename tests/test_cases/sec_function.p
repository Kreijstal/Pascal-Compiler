program sec_function;
var
  r: real;
begin
  r := Sec(Pi / 3);
  writeln('SecPi3=', r:0:4);
  r := Sec(Pi / 4);
  writeln('SecPi4=', r:0:4);
  r := Sec(Pi / 6);
  writeln('SecPi6=', r:0:4);
end.
