{$mode objfpc}
program test_inherited;
type
  TBase = class
    function GetFirst: Integer;
  end;
  TDerived = class(TBase)
    function Get(var m: Cardinal): Integer;
  end;

function TBase.GetFirst: Integer;
begin
  Result := 42;
end;

function TDerived.Get(var m: Cardinal): Integer;
var
  v: Integer;
begin
  v := inherited GetFirst;
  if v = 0 then
    begin Result := 0; m := 0; end
  else
    begin Result := v; m := 1; end;
end;

var
  d: TDerived;
  m: Cardinal;
begin
  d := TDerived.Create;
  WriteLn(d.Get(m), ' ', m);
  d.Free;
end.
