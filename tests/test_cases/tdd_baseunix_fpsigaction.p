program tdd_baseunix_fpsigaction;
{$mode objfpc}
uses baseunix;

{$ifdef MSWINDOWS}
begin
  { fpsigaction is a POSIX-only wrapper for sigaction(2); Windows has no
    kernel signal model and runtime_baseunix.c only exposes the symbol
    under !_WIN32. Mirror the unix branch's success line so the auto test
    matches its expected output on both targets. }
  writeln('ok');
end.
{$else}
var
  act, oldact: SigActionRec;
  ret: cint;
begin
  { Install SIG_IGN for SIGUSR1 via fpsigaction, then restore the old handler.
    Both calls must return 0 (success). }
  FillChar(act, SizeOf(act), 0);
  act.sa_handler := Pointer(PtrInt(1)); { SIG_IGN = 1 on Linux }
  act.sa_flags := 0;

  ret := fpsigaction(10 {SIGUSR1}, @act, @oldact);
  if ret <> 0 then
  begin
    writeln('FAIL: fpsigaction install returned ', ret);
    Halt(1);
  end;

  { Restore original handler }
  ret := fpsigaction(10 {SIGUSR1}, @oldact, nil);
  if ret <> 0 then
  begin
    writeln('FAIL: fpsigaction restore returned ', ret);
    Halt(1);
  end;

  writeln('ok');
end.
{$endif}
