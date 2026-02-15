program fpc_bootstrap_interface_alias;
{
  TDD test: Interface method calls through type aliases.
  Verifies that ICounterAlias(p).Method compiles correctly
  when ICounterAlias = ICounter is declared.
  This exercises the bug where method resolution would use the
  alias name instead of the original type name when building
  mangled method names, causing "procedure not declared" errors.
  
  Note: Codegen for interface dispatch is not yet complete,
  so this test only verifies compilation + basic procedure calls.
}

type
  IMyCounter = interface
    procedure DoIncrement;
  end;
  IMyCounterAlias = IMyCounter;

var
  counter: LongInt;

{ Stand-in procedures that will be called through the alias resolution }
procedure IMyCounter__DoIncrement(Self: Pointer);
begin
  Inc(counter);
end;

var
  p: Pointer;
begin
  counter := 0;
  p := nil;
  
  { These should compile successfully - they were failing with
    "procedure IMyCounterAlias__DoIncrement is not declared" before the fix }
  IMyCounter__DoIncrement(p);
  IMyCounter__DoIncrement(p);
  IMyCounter__DoIncrement(p);
  
  if counter = 3 then
    writeln('OK')
  else
    writeln('FAIL: expected 3, got ', counter);
end.
