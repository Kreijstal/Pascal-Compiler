program odd_function;
var
  i: integer;
begin
  i := -3;
  writeln('OddNeg=', Odd(i));
  writeln('OddZero=', Odd(0));
  writeln('OddOne=', Odd(1));
  writeln('OddEven=', Odd(24));
end.
