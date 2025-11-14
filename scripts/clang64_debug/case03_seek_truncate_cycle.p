program DebugCase03SeekTruncate;

uses SysUtils;

var
  F: file of Longint;
  Value: Longint;
begin
  Writeln('[CASE03] begin');
  Assign(F, 'dbg_case03.bin');
  {$I-}
  Rewrite(F);
  Writeln('[CASE03] rewrite IORes=', IOResult);
  Value := 11; BlockWrite(F, Value, 1);
  Value := 22; BlockWrite(F, Value, 1);
  Value := 33; BlockWrite(F, Value, 1);
  Writeln('[CASE03] wrote entries IORes=', IOResult);
  Seek(F, 1);
  Writeln('[CASE03] seek(1) IORes=', IOResult);
  Value := 99;
  BlockWrite(F, Value, 1);
  Writeln('[CASE03] overwrite second entry IORes=', IOResult);
  Seek(F, 0);
  Writeln('[CASE03] FilePos after rewind=', FilePos(F), ' IORes=', IOResult);
  Truncate(F);
  Writeln('[CASE03] truncate IORes=', IOResult);
  Close(F);
  {$I+}
  DeleteFile('dbg_case03.bin');
  Writeln('[CASE03] done');
end.
