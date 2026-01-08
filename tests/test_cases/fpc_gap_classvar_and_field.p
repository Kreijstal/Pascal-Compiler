program fpc_gap_classvar_and_field;
{$mode objfpc}
{ Test: Class with both class var and instance field
  FPC resolves FValue from instance methods when both class var and field exist
  Expected output: 42 }

type
  TMyClass = class
  public
    class var ClassField: Integer;
    FValue: Integer;
    function GetValue: Integer;
  end;

function TMyClass.GetValue: Integer;
begin
  Result := FValue;  { Should resolve to instance field, not class var }
end;

var
  obj: TMyClass;
begin
  TMyClass.ClassField := 100;
  obj := TMyClass.Create;
  obj.FValue := 42;
  writeln(obj.GetValue);
  obj.Free;
end.
