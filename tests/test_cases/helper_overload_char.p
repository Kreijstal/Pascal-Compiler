{$mode objfpc}
{$modeswitch typehelpers}
program helper_overload_char;

type
  TCharHelper = type helper for Char
    function Foo: Integer;
    function Foo(x: Integer): Integer;
  end;

function TCharHelper.Foo: Integer;
begin
  Result := Foo(1);
end;

function TCharHelper.Foo(x: Integer): Integer;
begin
  Result := x + Ord(Self);
end;

var
  c: Char;

begin
  c := 'A';
  Writeln(c.Foo);
end.
