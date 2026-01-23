program reg_helper_open_array_overload;

{$mode objfpc}
{$H+}
{$modeswitch typehelpers}

type
  TStringHelper = type helper for AnsiString
    function IndexOfAny(const AnyOf: array of Char): Integer; overload;
    function IndexOfAny(const AnyOf: array of String): Integer; overload;
    function Pick(const AnyOf: array of String): Integer;
  end;

function TStringHelper.IndexOfAny(const AnyOf: array of Char): Integer;
begin
  Result := 1;
end;

function TStringHelper.IndexOfAny(const AnyOf: array of String): Integer;
begin
  Result := 2;
end;

function TStringHelper.Pick(const AnyOf: array of String): Integer;
begin
  Result := Self.IndexOfAny(AnyOf);
end;

begin
  writeln(AnsiString('x').Pick(['aa', 'bb']));
end.
