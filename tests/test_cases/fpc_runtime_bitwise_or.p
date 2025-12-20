program fpc_runtime_bitwise_or;
var
  a, b, c: LongInt;
begin
  a := 1;
  b := 2;
  c := (a shl 8) or b;
  writeln(c);
  writeln(16 or 4);
end.
