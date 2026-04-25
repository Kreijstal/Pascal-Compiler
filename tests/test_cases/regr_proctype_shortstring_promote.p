program regr_proctype_shortstring_promote;

{$mode objfpc}

uses sysutils;

type
  TStrProc = procedure(const s: AnsiString);

procedure PrintStr(const s: AnsiString);
begin
  writeln(s);
end;

var
  proc: TStrProc;
  short: ShortString;
begin
  proc := @PrintStr;
  short := 'Hello';
  proc(short);
end.
