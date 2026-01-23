{$mode objfpc}
{$modeswitch typehelpers}
program helper_overload_single;

type
  TSingleHelper = type helper for Single
    function Foo: Integer;
    function Foo(x: Integer): Integer;
  end;

function TSingleHelper.Foo: Integer;
begin
  Result := Foo(2);
end;

function TSingleHelper.Foo(x: Integer): Integer;
begin
  Result := x + Trunc(Self);
end;

var
  s: Single;

begin
  s := 3.0;
  Writeln(s.Foo);
end.
