program PointerOperators;
type
  PInteger = ^Integer;
var
  value, copied: Integer;
  ptr: PInteger;
begin
  value := 41;
  ptr := @value;
  ptr^ := ptr^ + 1;
  copied := ptr^;
  writeln(value);
  writeln(copied);
end.
