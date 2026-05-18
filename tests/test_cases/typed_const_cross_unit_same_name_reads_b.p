unit typed_const_cross_unit_same_name_reads_b;
interface
procedure write_b;
implementation
const
  msg : array[0..1] of string[6] = ('delta','epsiln');
procedure write_b;
begin
  writeln('b0=', msg[0]);
  writeln('b1=', msg[1]);
end;
end.
