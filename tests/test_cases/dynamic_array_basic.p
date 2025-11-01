program DynArrayTest;
var
  data: array of longint;
begin
  SetLength(data, 3);
  data[0] := 1;
  data[1] := 2;
  data[2] := 3;
  writeln(data[0]);
end.
