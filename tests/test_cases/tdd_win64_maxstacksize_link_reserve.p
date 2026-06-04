{ Regression: KGPC must honor {$MAXSTACKSIZE}/{$MINSTACKSIZE} on Windows
  targets by emitting a PE stack reserve (-Wl,--stack reserve,commit).

  Unlike POSIX, a Win64 thread's stack cannot grow past the reserve baked into
  the PE header.  FPC's pp.pas asks for a 512 MB stack precisely for this
  reason; when KGPC dropped the directive the recursion-heavy compiler
  overflowed the linker's default ~2 MB reserve and crashed at startup with
  STATUS_STACK_OVERFLOW (0xC00000FD).  On POSIX the directive is irrelevant
  (the stack auto-grows), so no --stack must be emitted there. }
program win64_maxstacksize;
{$MAXSTACKSIZE 64000000}
{$MINSTACKSIZE 2000000}
begin
  writeln('ok');
end.
