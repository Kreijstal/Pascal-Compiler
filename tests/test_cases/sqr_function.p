program sqr_function;
var
  i_pos, i_neg: integer;
  l_val: longint;
  r_val: real;
begin
  i_pos := Sqr(5);
  i_neg := Sqr(-4);
  l_val := Sqr(20000);
  r_val := Sqr(1.5);
  writeln('SqrPos=', i_pos);
  writeln('SqrNeg=', i_neg);
  writeln('SqrLong=', l_val);
  writeln('SqrReal=', r_val:0:2);
end.
