{$mode objfpc}
{$H+}
{$modeswitch typehelpers}
program reg_split_default_opts;

type
  TStringSplitOption = (soIgnoreEmpty);
  TStringSplitOptions = set of TStringSplitOption;

  TStringHelper = type helper for AnsiString
    function Split(const Separators: array of Char; ACount: SizeInt; Options: TStringSplitOptions = []): SizeInt; overload;
  end;

function TStringHelper.Split(const Separators: array of Char; ACount: SizeInt; Options: TStringSplitOptions): SizeInt;
begin
  Result := ACount + System.Length(Separators);
end;

var
  s: AnsiString;

begin
  s := 'a;b;c';
  WriteLn(s.Split([';'], 2));
end.
