{$mode objfpc}
{$modeswitch typehelpers}
program helper_overload_sizeint;

type
  TIntHelper = type helper for Integer
    function Foo: SizeInt;
    function Foo(x: SizeInt): SizeInt;
  end;

function TIntHelper.Foo: SizeInt;
begin
  Result := Foo(20);
end;

function TIntHelper.Foo(x: SizeInt): SizeInt;
begin
  Result := x - 3;
end;

var
  i: Integer;

begin
  i := 1;
  Writeln(i.Foo);
end.
