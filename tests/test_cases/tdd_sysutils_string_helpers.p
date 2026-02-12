{$codepage utf8}
program tdd_sysutils_string_helpers;

{$mode objfpc}
{$H+}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

uses
  SysUtils;

procedure DumpAnsiArray(const LabelName: string; const Values: TStringArray);
var
  I: Integer;
begin
  Write(LabelName, '=');
  for I := 0 to High(Values) do
  begin
    if I > 0 then
      Write('|');
    Write(Values[I]);
  end;
  Writeln;
end;

procedure TestAnsiString;
var
  A: AnsiString;
  Parts: TStringArray;
  Filtered: TStringArray;
  Trimmed: AnsiString;
  Idx: SizeInt;
  Replaced: AnsiString;
  I: Integer;
  FilterCount: Integer;
begin
  A := AnsiString('  one::two::::three::  ');
  Trimmed := A.Trim;
  Writeln('ansi_trim=', Trimmed);
  Parts := Trimmed.Split([':'], 0);
  SetLength(Filtered, 0);
  FilterCount := 0;
  for I := 0 to High(Parts) do
    if Parts[I] <> '' then
    begin
      SetLength(Filtered, FilterCount + 1);
      Filtered[FilterCount] := Parts[I];
      Inc(FilterCount);
    end;
  DumpAnsiArray('ansi_split', Filtered);
  Idx := Trimmed.LastIndexOfAny(['o', 'e'], Trimmed.Length - 1, Trimmed.Length);
  Writeln('ansi_last=', Idx);
  Writeln('ansi_pad=', Trimmed.PadRight(20, '.'));
  Replaced := Trimmed.Replace('::', '--', [rfReplaceAll]);
  Writeln('ansi_repl=', Replaced);
end;

procedure TestLongIntHelper;
var
  V: LongInt;
begin
  V := 0;
  V := V.SetBit(3);
  V := V.SetBit(1);
  V := V.ToggleBit(1);
  Writeln('longint_bin=', V.ToBinString);
  Writeln('longint_hex=', V.ToHexString(4));
end;

begin
  TestAnsiString;
  TestLongIntHelper;
end.
