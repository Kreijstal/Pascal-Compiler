program DebugCase04RewriteTwice;

uses SysUtils;

var
  F: file of Longint;
  Value: Longint;
begin
  Writeln('[CASE04] begin');
  Assign(F, 'dbg_case04.bin');
  {$I-}
  Rewrite(F);
  Writeln('[CASE04] first rewrite IORes=', IOResult);
  Value := 1;
  BlockWrite(F, Value, 1);
  Writeln('[CASE04] first blockwrite IORes=', IOResult);
  Close(F);
  Writeln('[CASE04] close1 IORes=', IOResult);
  Rewrite(F);
  Writeln('[CASE04] second rewrite IORes=', IOResult);
  Value := 2;
  BlockWrite(F, Value, 1);
  Writeln('[CASE04] second blockwrite IORes=', IOResult);
  Close(F);
  Writeln('[CASE04] close2 IORes=', IOResult);
  {$I+}
  DeleteFile('dbg_case04.bin');
  Writeln('[CASE04] done');
end.
