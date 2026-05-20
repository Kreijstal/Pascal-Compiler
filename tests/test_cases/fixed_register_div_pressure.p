program fixed_register_div_pressure;

var
  a, b, c, d, e, f, g, h: LongInt;
  q, r: LongInt;

begin
  a := 73;
  b := 18;
  c := 41;
  d := 9;
  e := 27;
  f := 66;
  g := 14;
  h := 5;

  q := ((a + b + c + d + e + f) div ((g mod 5) + 2)) + ((h * 9) mod 7);
  r := ((a * b) mod (c + d)) + ((f + e) div (g - 2));

  writeln(q);
  writeln(r);
end.
