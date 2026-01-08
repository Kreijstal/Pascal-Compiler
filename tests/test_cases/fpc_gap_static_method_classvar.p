program fpc_gap_static_method_classvar;
{$mode objfpc}
{ Test: Static class method accessing class var
  FPC allows static methods to access class vars without prefix
  Expected output: 42 }

type
  TMyClass = class
  public
    class var FCounter: Integer;
    class procedure SetCounter(AValue: Integer); static;
    class function GetCounter: Integer; static;
  end;

class procedure TMyClass.SetCounter(AValue: Integer);
begin
  FCounter := AValue;  { Static method accessing class var }
end;

class function TMyClass.GetCounter: Integer;
begin
  Result := FCounter;  { Static method accessing class var }
end;

begin
  TMyClass.SetCounter(42);
  writeln(TMyClass.GetCounter);
end.
