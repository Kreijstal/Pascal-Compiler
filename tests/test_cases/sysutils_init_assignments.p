program sysutils_init_assignments;

{$mode objfpc}
{$J+}

type
  TBeepHandler = procedure;

const
  SysConfigDir: string = '';

var
  OnBeep: TBeepHandler = nil;

procedure SysBeep;
begin
  writeln('beep');
end;

begin
  SysConfigDir := '/etc';
  OnBeep := @SysBeep;
  writeln(SysConfigDir);
  if Assigned(OnBeep) then
    OnBeep;
end.
