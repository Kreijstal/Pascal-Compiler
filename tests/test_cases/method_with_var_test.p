program MethodWithVarTest;

type
  TTest = record
    X: Integer;
  end;

procedure TTest.SetX(Value: Integer);
var
  Temp: Integer;
begin
  Temp := Value;
  X := Temp;
end;

begin
  WriteLn('Method with var section parsed successfully!');
end.
