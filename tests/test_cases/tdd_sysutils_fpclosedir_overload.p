{$mode objfpc}
program tdd_sysutils_fpclosedir_overload;

uses
  tdd_sysutils_fpclosedir_unit_a,
  tdd_sysutils_fpclosedir_unit_b;

var
  raw: PDirRec;
  handle: Pointer;
  total: longint;
  step: longint;
begin
  New(raw);
  InitDir(raw^);
  handle := raw;

  step := FpReaddir(PDirRec(handle)^);
  FpClosedir(PDirRec(handle)^);

  total := step + ComputeChecksum(raw^);
  writeln('Total=', total);
  Dispose(raw);
end.
