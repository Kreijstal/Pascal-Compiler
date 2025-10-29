program BitShiftDemo;
var
  value: LongInt;
begin
  value := 252645135;
  writeln(value shl 3);
  writeln(value shr 2);
  writeln(value rol 4);
  writeln(value ror 4);
end.
