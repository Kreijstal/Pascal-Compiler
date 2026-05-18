unit typed_const_cross_unit_same_name_three_units_c;
interface
procedure write_c;
implementation
const
  msg : array[1..3] of string[8] = ('one','two','three');
procedure write_c;
begin
  writeln('c1=', msg[1]);
  writeln('c2=', msg[2]);
  writeln('c3=', msg[3]);
end;
end.
