program RegressionConvertClassTypeGeneric;
{$mode objfpc}

type
  generic TBox<T> = class
  private
    FValue: T;
  public
    constructor Create(AValue: T);
    function GetValue: T;
  end;

  TBoxInt = specialize TBox<Integer>;

constructor TBox.Create(AValue: T);
begin
  FValue := AValue;
end;

function TBox.GetValue: T;
begin
  Result := FValue;
end;

var
  Box: TBoxInt;
begin
  Box := TBoxInt.Create(42);
  WriteLn(Box.GetValue);
end.
