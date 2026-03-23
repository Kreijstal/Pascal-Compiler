{$mode objfpc}
program test_inherited_else;
type
  TBase = class
    function Pass1: Integer; virtual;
  end;
  TDerived = class(TBase)
    x: Boolean;
    function Pass1: Integer; override;
  end;

function TBase.Pass1: Integer;
begin
  Result := 42;
end;

function TDerived.Pass1: Integer;
begin
  if x then
    Result := inherited
  else
    Result := 99;
end;

var
  d: TDerived;
begin
  d := TDerived.Create;
  d.x := true;
  WriteLn(d.Pass1);
  d.x := false;
  WriteLn(d.Pass1);
  d.Free;
end.
