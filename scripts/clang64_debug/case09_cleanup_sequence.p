program DebugCase09TruncateEffect;

uses SysUtils;

const
  FileName = 'dbg_case09.bin';

var
  F: file of Longint;
  V, Count: Longint;
begin
  Writeln('[CASE09] begin');
  Assign(F, FileName);
  {$I-}
  Rewrite(F);
  V := 5; BlockWrite(F, V, 1);
  V := 6; BlockWrite(F, V, 1);
  V := 7; BlockWrite(F, V, 1);
  Close(F);
  Reset(F);
  Seek(F, 1);
  Writeln('[CASE09] seek1 IORes=', IOResult, ' filepos=', FilePos(F));
  Truncate(F);
  Writeln('[CASE09] truncate_current IORes=', IOResult);
  Seek(F, 0);
  repeat
    BlockRead(F, V, 1, Count);
    Writeln('[CASE09] read count=', Count, ' value=', V,
      ' filepos=', FilePos(F), ' IORes=', IOResult);
  until Count = 0;
  Close(F);
  DeleteFile(FileName);
  {$I+}
  Writeln('[CASE09] done');
end.
