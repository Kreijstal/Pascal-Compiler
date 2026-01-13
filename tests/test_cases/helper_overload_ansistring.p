{$mode objfpc}
{$modeswitch typehelpers}
program helper_overload_ansistring;

type
  TStringHelper = type helper for AnsiString
    function Foo: Integer;
    function Foo(x: Integer): Integer;
  end;

function TStringHelper.Foo: Integer;
begin
  Result := Foo(Length(Self));
end;

function TStringHelper.Foo(x: Integer): Integer;
begin
  Result := x + 1;
end;

var
  s: AnsiString;

begin
  s := 'abc';
  Writeln(s.Foo);
end.
