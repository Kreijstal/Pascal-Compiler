{$codepage utf8}
program tdd_sysutils_string_helper_padleft;

{$mode objfpc}
{$modeswitch typehelpers}
{$H+}

uses
  SysUtils;

procedure Emit(const LabelName: AnsiString; const Value: AnsiString);
begin
  Writeln(LabelName, '=', Value);
end;

var
  Source: AnsiString;
  Trimmed: AnsiString;
  Padded: AnsiString;
  Quoted: AnsiString;
  Starts: Boolean;
  Ends: Boolean;
  Idx: SizeInt;
begin
  Source := '  alpha::beta::gamma::  ';
  Trimmed := Source.Trim;
  Padded := Trimmed.PadLeft(25, '*');
  Quoted := Trimmed.QuotedString('"');
  Starts := Trimmed.StartsWith('alpha');
  Ends := Trimmed.EndsWith('::');
  Idx := Trimmed.LastIndexOfAny(['a', 'm'], Trimmed.Length - 1, Trimmed.Length);

  Emit('pad', Padded);
  Emit('quoted', Quoted);
  Emit('starts', BoolToStr(Starts, True));
  Emit('ends', BoolToStr(Ends, True));
  Writeln('last=', Idx);
end.
