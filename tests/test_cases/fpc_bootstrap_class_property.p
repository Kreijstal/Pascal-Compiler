program fpc_bootstrap_class_property;
{$mode objfpc}
{ Test: Class property with getter }
{ FPC: Should compile and output 42 }
{ KGPC: Was failing with "call to function does not match any available overload" }
{ This pattern is used in FPC's TEncoding.SystemEncoding }

type
  TMyClass = class
  private
    class function GetValue: Integer; static;
  public
    class property Value: Integer read GetValue;
  end;

class function TMyClass.GetValue: Integer;
begin
  Result := 42;
end;

begin
  writeln(TMyClass.Value);
end.
