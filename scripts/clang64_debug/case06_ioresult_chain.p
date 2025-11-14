program DebugCase06IOResult;

uses SysUtils;

var
  F: file of Longint;
  V: Longint;
  Count: Longint;
begin
  Writeln('[CASE06] begin');
  Assign(F, 'dbg_case06.bin');
  {$I-}
  Rewrite(F);
  Writeln('[CASE06] rewrite IORes=', IOResult);
  for V := 0 to 4 do
  begin
    BlockWrite(F, V, 1, Count);
    Writeln('[CASE06] write value=', V, ' count=', Count, ' IORes=', IOResult);
  end;
  Close(F);
  Writeln('[CASE06] close IORes=', IOResult);
  Reset(F);
  Writeln('[CASE06] reset IORes=', IOResult);
  while True do
  begin
    BlockRead(F, V, 1, Count);
    if Count = 0 then
      Break;
    Writeln('[CASE06] read value=', V, ' count=', Count, ' IORes=', IOResult);
  end;
  Close(F);
  {$I+}
  DeleteFile('dbg_case06.bin');
  Writeln('[CASE06] done');
end.
