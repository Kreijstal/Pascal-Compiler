program tdd_type_helper_format_self;

{$mode objfpc}
{$modeswitch typehelpers}
{$H+}

uses
  SysUtils;

var
  S: AnsiString;
begin
  S := Format('Hello %d', [42]);
  WriteLn(S);
  S := Format('%s=%d', ['x', 10]);
  WriteLn(S);
end.
