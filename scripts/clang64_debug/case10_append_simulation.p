program DebugCase10AppendSim;

uses SysUtils;

var
  F: file of Longint;
  V: Longint;
begin
  Writeln('[CASE10] begin');
  Assign(F, 'dbg_case10.bin');
  {$I-}
  Rewrite(F);
  V := 5; BlockWrite(F, V, 1);
  Close(F);
  Reset(F);
  Seek(F, FileSize(F));
  V := 99;
  BlockWrite(F, V, 1);
  Writeln('[CASE10] append style write IORes=', IOResult);
  Close(F);
  Reset(F);
  while not EOF(F) do
  begin
    BlockRead(F, V, 1);
    Writeln('[CASE10] value=', V, ' IORes=', IOResult);
  end;
  Close(F);
  {$I+}
  DeleteFile('dbg_case10.bin');
  Writeln('[CASE10] done');
end.
