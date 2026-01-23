{$mode objfpc}
{$H+}
{$modeswitch typehelpers}
program reg_indexof_default;

type
  TStringHelper = type helper for AnsiString
    function IndexOf(const AValue: Char; AStartIndex: SizeInt = 0; ACount: SizeInt = -1): SizeInt; overload;
    function IndexOf(const AValue: Char; AStartIndex: SizeInt; ACount: SizeInt; out AMatch: SizeInt): SizeInt; overload;
  end;

function TStringHelper.IndexOf(const AValue: Char; AStartIndex: SizeInt; ACount: SizeInt): SizeInt;
begin
  Result := -1;
end;

function TStringHelper.IndexOf(const AValue: Char; AStartIndex: SizeInt; ACount: SizeInt; out AMatch: SizeInt): SizeInt;
begin
  AMatch := -1;
  Result := -1;
end;

var
  s: AnsiString;

begin
  s := 'abc';
  WriteLn(s.IndexOf('a'));
end.
