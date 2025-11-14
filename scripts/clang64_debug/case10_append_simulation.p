program DebugCase10AppendSim;

uses SysUtils;

var
  F: file of Longint;
  V: Longint;
  Count: Longint;
begin
  Writeln('[CASE10] begin');
  Assign(F, 'dbg_case10.bin');
  {$I-}
  Rewrite(F);
  V := 5; BlockWrite(F, V, 1);
  Close(F);
  Reset(F);
  V := 0;
  Count := 0;
  while True do
  begin
    BlockRead(F, V, 1, Count);
    if Count = 0 then
      Break;
  end;
  Seek(F, FilePos(F));
  V := 99;
  BlockWrite(F, V, 1);
  Writeln('[CASE10] append style write IORes=', IOResult);
  Close(F);
  Reset(F);
  while True do
  begin
    BlockRead(F, V, 1, Count);
    if Count = 0 then
      Break;
    Writeln('[CASE10] value=', V, ' IORes=', IOResult);
  end;
  Close(F);
  {$I+}
  DeleteFile('dbg_case10.bin');
  Writeln('[CASE10] done');
end.
