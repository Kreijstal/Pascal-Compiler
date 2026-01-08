program fpc_gap_class_property_getter;
{$mode objfpc}
{ Test: Class property with class function getter
  FPC allows class properties with class function getters
  Expected output: 42 }

type
  TMyClass = class
  private
    class var FValue: Integer;
    class function GetValue: Integer; static;
    class procedure SetValue(AValue: Integer); static;
  public
    class property Value: Integer read GetValue write SetValue;
  end;

class function TMyClass.GetValue: Integer;
begin
  Result := FValue;
end;

class procedure TMyClass.SetValue(AValue: Integer);
begin
  FValue := AValue;
end;

begin
  TMyClass.Value := 42;
  writeln(TMyClass.Value);
end.
