{$mode objfpc}
{$H+}
{$modeswitch typehelpers}
program reg_indexofanyunquoted_default;

type
  TStringHelper = type helper for AnsiString
    function IndexOfAnyUnquoted(const AnyOf: array of Char; StartIndex: SizeInt = 0; ACount: SizeInt = -1): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of Char; StartIndex: SizeInt; ACount: SizeInt; out AMatch: SizeInt): SizeInt; overload;
  end;

function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of Char; StartIndex: SizeInt; ACount: SizeInt): SizeInt;
begin
  Result := -1;
end;

function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of Char; StartIndex: SizeInt; ACount: SizeInt; out AMatch: SizeInt): SizeInt;
begin
  AMatch := -1;
  Result := -1;
end;

var
  s: AnsiString;

begin
  s := 'abc';
  WriteLn(s.IndexOfAnyUnquoted(['x']));
end.
