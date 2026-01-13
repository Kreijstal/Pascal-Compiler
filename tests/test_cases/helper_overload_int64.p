{$mode objfpc}
{$modeswitch typehelpers}
program helper_overload_int64;

type
  TIntHelper = type helper for Integer
    function Foo: Int64;
    function Foo(x: Integer): Int64;
  end;

function TIntHelper.Foo: Int64;
begin
  Result := Foo(10);
end;

function TIntHelper.Foo(x: Integer): Int64;
begin
  Result := x + 5;
end;

var
  i: Integer;

begin
  i := 7;
  Writeln(i.Foo);
end.
