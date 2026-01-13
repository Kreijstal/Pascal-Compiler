{$mode objfpc}
{$modeswitch typehelpers}
program helper_overload_shortstring;

type
  TShortHelper = type helper for ShortString
    function Foo: Integer;
    function Foo(x: Integer): Integer;
  end;

function TShortHelper.Foo: Integer;
begin
  Result := Foo(Length(Self));
end;

function TShortHelper.Foo(x: Integer): Integer;
begin
  Result := x * 2;
end;

var
  s: ShortString;

begin
  s := 'hi';
  Writeln(s.Foo);
end.
