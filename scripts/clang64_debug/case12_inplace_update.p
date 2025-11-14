program DebugCase12InplaceUpdate;

uses SysUtils;

var
  F: file of Longint;
  V: Longint;
begin
  Writeln('[CASE12] begin');
  Assign(F, 'dbg_case12.bin');
  {$I-}
  Rewrite(F);
  V := 7; BlockWrite(F, V, 1);
  V := 8; BlockWrite(F, V, 1);
  Close(F);
  Reset(F);
  Seek(F, 0);
  V := 777;
  BlockWrite(F, V, 1);
  Writeln('[CASE12] inplace update IORes=', IOResult);
  Close(F);
  Reset(F);
  while not EOF(F) do
  begin
    BlockRead(F, V, 1);
    Writeln('[CASE12] value=', V, ' IORes=', IOResult);
  end;
  Close(F);
  {$I+}
  DeleteFile('dbg_case12.bin');
  Writeln('[CASE12] done');
end.
