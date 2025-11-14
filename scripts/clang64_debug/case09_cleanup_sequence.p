program DebugCase09CleanupSequence;

uses SysUtils;

var
  F1, F2: file of Longint;
  V: Longint;
begin
  Writeln('[CASE09] begin');
  Assign(F1, 'dbg_case09_a.bin');
  Assign(F2, 'dbg_case09_b.bin');
  {$I-}
  Rewrite(F1);
  Rewrite(F2);
  Writeln('[CASE09] rewrites IORes=', IOResult);
  V := 123;
  BlockWrite(F1, V, 1);
  BlockWrite(F2, V, 1);
  Writeln('[CASE09] wrote to both files IORes=', IOResult);
  Close(F1);
  Writeln('[CASE09] close F1 IORes=', IOResult);
  Close(F2);
  Writeln('[CASE09] close F2 IORes=', IOResult);
  {$I+}
  DeleteFile('dbg_case09_a.bin');
  DeleteFile('dbg_case09_b.bin');
  Writeln('[CASE09] done');
end.
