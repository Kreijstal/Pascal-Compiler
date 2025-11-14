program DebugCase05ResetMissing;

uses SysUtils;

var
  F: file of Longint;
begin
  Writeln('[CASE05] begin');
  Assign(F, 'dbg_case05.bin');
  DeleteFile('dbg_case05.bin');
  {$I-}
  Reset(F);
  Writeln('[CASE05] reset nonexistent file IORes=', IOResult);
  Rewrite(F);
  Writeln('[CASE05] rewrite IORes=', IOResult);
  Close(F);
  {$I+}
  DeleteFile('dbg_case05.bin');
  Writeln('[CASE05] done');
end.
