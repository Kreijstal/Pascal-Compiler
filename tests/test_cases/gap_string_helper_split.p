{$mode objfpc}
{$H+}
{$modeswitch typehelpers}
program gap_string_helper_split;

uses
  SysUtils;

var
  parts: TStringArray;
  s: AnsiString;

begin
  s := 'a;b;c';
  parts := s.Split([';'], 2);
  WriteLn(Length(parts));
end.
