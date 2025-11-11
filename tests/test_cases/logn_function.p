program logn_function;
var
  r: real;
begin
  r := LogN(10.0, 1000.0);
  writeln('LogBase10_1000=', r:0:1);
  r := LogN(2.0, 256.0);
  writeln('LogBase2_256=', r:0:1);
  r := LogN(5.0, 25.0);
  writeln('LogBase5_25=', r:0:1);
end.
