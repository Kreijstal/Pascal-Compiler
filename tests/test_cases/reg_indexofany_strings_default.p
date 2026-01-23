{$mode objfpc}
{$H+}
{$modeswitch typehelpers}
program reg_indexofany_strings_default;

type
  TStringHelper = type helper for AnsiString
    function IndexOfAny(const AnyOf: array of AnsiString; StartIndex: SizeInt = 0; ACount: SizeInt = -1): SizeInt; overload;
    function IndexOfAny(const AnyOf: array of AnsiString; StartIndex: SizeInt; ACount: SizeInt; out AMatch: SizeInt): SizeInt; overload;
  end;

function TStringHelper.IndexOfAny(const AnyOf: array of AnsiString; StartIndex: SizeInt; ACount: SizeInt): SizeInt;
begin
  Result := -1;
end;

function TStringHelper.IndexOfAny(const AnyOf: array of AnsiString; StartIndex: SizeInt; ACount: SizeInt; out AMatch: SizeInt): SizeInt;
begin
  AMatch := -1;
  Result := -1;
end;

var
  s: AnsiString;

begin
  s := 'abc';
  WriteLn(s.IndexOfAny(['a','b']));
end.
