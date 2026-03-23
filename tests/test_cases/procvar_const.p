{$mode objfpc}
program test_procvar_const;
type
  TStopProc = procedure;
  TCheckFunc = function: Boolean;

procedure DefStop;
begin
  WriteLn('stop');
end;

function DefCheck: Boolean;
begin
  Result := True;
end;

const
  do_stop: TStopProc = DefStop;
  do_check: TCheckFunc = DefCheck;

begin
  do_stop();
  WriteLn(do_check());
end.
