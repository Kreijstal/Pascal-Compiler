{$mode objfpc}
{$H+}
{$modeswitch typehelpers}
program gap_string_helper_lastindexof;

uses
  SysUtils;

var
  s: AnsiString;

begin
  s := 'abcabc';
  WriteLn(s.LastIndexOf('b', 6, 6));
end.
