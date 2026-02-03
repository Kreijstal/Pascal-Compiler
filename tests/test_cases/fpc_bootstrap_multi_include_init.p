{ TDD test: Unit with multiple includes and procedures defined between/after them. }
unit fpc_bootstrap_multi_include_init;

interface

implementation

procedure BeforeInclude;
begin
  WriteLn('BeforeInclude');
end;

{$i includes/sysutils_minimal.inc}

procedure AfterInclude;
begin
  WriteLn('AfterInclude');
end;

{$i includes/extra_procs.inc}

procedure AfterSecondInclude;
begin
  WriteLn('AfterSecondInclude');
end;

initialization
  BeforeInclude;
  InitExceptions;
  AfterInclude;
  ExtraProc;
  AfterSecondInclude;
finalization
  DoneExceptions;
end.
