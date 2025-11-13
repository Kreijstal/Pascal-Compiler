program hypot_function;
var
  r: real;
begin
  r := Hypot(3.0, 4.0);
  writeln('Hypot3_4=', r:0:1);
  r := Hypot(5.0, 12.0);
  writeln('Hypot5_12=', r:0:1);
  r := Hypot(1.5, 2.5);
  writeln('Hypot1_5_2_5=', r:0:4);
end.
