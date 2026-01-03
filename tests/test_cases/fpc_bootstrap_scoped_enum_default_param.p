program fpc_bootstrap_scoped_enum_default_param;
{$mode objfpc}
{$scopedenums on}
{ Test: Scoped enum value as default parameter }
{ FPC: Should compile and output Using_Default,Using_Explicit }
{ KGPC: Fails with "equality comparison requires matching types" }
{ This pattern is used throughout FPC's sysutils for boolean helpers }

type
  TUseBoolStrs = (False, True);

procedure TestProc(mode: TUseBoolStrs = TUseBoolStrs.False);
begin
  if mode = TUseBoolStrs.False then
    writeln('Using_Default')
  else
    writeln('Using_Explicit');
end;

begin
  TestProc;  { Should use default TUseBoolStrs.False }
  TestProc(TUseBoolStrs.True);  { Explicit value }
end.
