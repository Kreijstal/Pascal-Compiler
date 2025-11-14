program DebugCase07TruncateShorter;

uses SysUtils;

var
  F: file of Longint;
  V: Longint;
begin
  Writeln('[CASE07] begin');
  Assign(F, 'dbg_case07.bin');
  {$I-}
  Rewrite(F);
  V := 10; BlockWrite(F, V, 1);
  V := 20; BlockWrite(F, V, 1);
  V := 30; BlockWrite(F, V, 1);
  Writeln('[CASE07] wrote 3 entries IORes=', IOResult);
  Seek(F, 1);
  Writeln('[CASE07] seek(1) IORes=', IOResult);
  Truncate(F);
  Writeln('[CASE07] truncate IORes=', IOResult);
  Close(F);
  Reset(F);
  Writeln('[CASE07] reset IORes=', IOResult);
  while not EOF(F) do
  begin
    BlockRead(F, V, 1);
    Writeln('[CASE07] value=', V, ' IORes=', IOResult);
  end;
  Close(F);
  {$I+}
  DeleteFile('dbg_case07.bin');
  Writeln('[CASE07] done');
end.
