unit typed_const_cross_unit_same_name_three_units_a;
interface
procedure write_a;
implementation
const
  msg : array[0..2] of string[5] = ('alpha','beta','gamma');
procedure write_a;
begin
  writeln('a0=', msg[0]);
  writeln('a1=', msg[1]);
  writeln('a2=', msg[2]);
end;
end.
