{$mode objfpc}
{$modeswitch typehelpers}
program helper_overload_nativeint;

type
  TIntHelper = type helper for Integer
    function Foo: NativeInt;
    function Foo(x: NativeInt): NativeInt;
  end;

function TIntHelper.Foo: NativeInt;
begin
  Result := Foo(100);
end;

function TIntHelper.Foo(x: NativeInt): NativeInt;
begin
  Result := x - 4;
end;

var
  i: Integer;

begin
  i := 0;
  Writeln(i.Foo);
end.
