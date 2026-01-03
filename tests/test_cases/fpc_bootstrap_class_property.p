program fpc_bootstrap_class_property;
{$mode objfpc}
{ Test: Class property with getter }
{ FPC: Should compile and output 42 }
{ KGPC: Fails with "getter not found" }
{ This pattern is used in FPC's TEncoding.SystemEncoding }

type
  TMyClass = class
  private
    class var FValue: Integer;
    class function GetValue: Integer; static;
  public
    class property Value: Integer read GetValue;
  end;

class function TMyClass.GetValue: Integer;
begin
  Result := 42;
end;

begin
  TMyClass.FValue := 100;  { Initialize class var }
  writeln(TMyClass.Value);
end.
