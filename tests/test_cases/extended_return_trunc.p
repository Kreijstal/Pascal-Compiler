program ExtendedReturnTrunc;

function GetHalf: Extended;
begin
  Result := 1.5;
end;

var
  Value: Extended;
begin
  Value := 2.75;
  WriteLn(Trunc(Value));
  WriteLn(Trunc(GetHalf));
end.
