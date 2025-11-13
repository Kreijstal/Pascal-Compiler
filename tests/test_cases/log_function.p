program log_function;

uses Math;

var
  r: real;
begin
  r := Log10(1000.0);
  writeln('Log10_1000=', r:0:4);
  r := Log2(32.0);
  writeln('Log2_32=', r:0:4);
end.
