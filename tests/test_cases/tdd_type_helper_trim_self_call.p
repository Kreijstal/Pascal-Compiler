program tdd_type_helper_trim_self_call;

{$mode objfpc}
{$modeswitch typehelpers}
{$H+}

uses
  SysUtils;

var
  S: AnsiString;
begin
  S := '  hello  ';
  WriteLn('[' + S.Trim + ']');
  S := #9 + 'world' + #10;
  WriteLn('[' + S.Trim + ']');
end.
