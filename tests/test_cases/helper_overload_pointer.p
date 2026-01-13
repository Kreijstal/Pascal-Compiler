{$mode objfpc}
{$modeswitch typehelpers}
program helper_overload_pointer;

type
  TPtrHelper = type helper for Pointer
    function Foo: Integer;
    function Foo(x: Integer): Integer;
  end;

function TPtrHelper.Foo: Integer;
begin
  Result := Foo(9);
end;

function TPtrHelper.Foo(x: Integer): Integer;
begin
  if Self = nil then
    Result := x
  else
    Result := x + 1;
end;

var
  p: Pointer;

begin
  p := nil;
  Writeln(p.Foo);
end.
