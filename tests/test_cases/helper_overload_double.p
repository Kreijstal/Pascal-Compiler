{$mode objfpc}
{$modeswitch typehelpers}
program helper_overload_double;

type
  TDoubleHelper = type helper for Double
    function Foo: Integer;
    function Foo(x: Integer): Integer;
  end;

function TDoubleHelper.Foo: Integer;
begin
  Result := Foo(7);
end;

function TDoubleHelper.Foo(x: Integer): Integer;
begin
  Result := x + Trunc(Self);
end;

var
  d: Double;

begin
  d := 1.0;
  Writeln(d.Foo);
end.
