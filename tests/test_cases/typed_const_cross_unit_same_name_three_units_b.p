unit typed_const_cross_unit_same_name_three_units_b;
interface
procedure write_b;
implementation
const
  msg : array[0..3] of string[10] = ('AA','BBB','CCCC','DDDDD');
procedure write_b;
begin
  writeln('b0=', msg[0]);
  writeln('b1=', msg[1]);
  writeln('b2=', msg[2]);
  writeln('b3=', msg[3]);
end;
end.
