{$mode objfpc}
{$H+}
{$modeswitch typehelpers}
program gap_string_helper_trim;

uses
  SysUtils;

var
  s: AnsiString;

begin
  s := '  a  ';
  WriteLn(s.Trim([' ']));
end.
