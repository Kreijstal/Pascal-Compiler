program DebugCase08SeekAndAppend;

uses SysUtils;

const
  FileName = 'dbg_case08.bin';

var
  F: file of Longint;
  V, Count: Longint;
begin
  Writeln('[CASE08] begin');
  Assign(F, FileName);
  {$I-}
  Rewrite(F);
  V := 1; BlockWrite(F, V, 1);
  V := 2; BlockWrite(F, V, 1);
  V := 3; BlockWrite(F, V, 1);
  Close(F);
  Reset(F);
  Writeln('[CASE08] reset IORes=', IOResult, ' filepos=', FilePos(F));
  Seek(F, FilePos(F));
  Writeln('[CASE08] seek(filepos) IORes=', IOResult, ' filepos=', FilePos(F));
  V := 99;
  BlockWrite(F, V, 1);
  Writeln('[CASE08] append write IORes=', IOResult, ' filepos=', FilePos(F));
  Seek(F, 0);
  Writeln('[CASE08] seek0 IORes=', IOResult);
  repeat
    BlockRead(F, V, 1, Count);
    Writeln('[CASE08] read count=', Count, ' value=', V,
      ' filepos=', FilePos(F), ' IORes=', IOResult);
  until Count = 0;
  Close(F);
  DeleteFile(FileName);
  {$I+}
  Writeln('[CASE08] done');
end.
