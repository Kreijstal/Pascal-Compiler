program DebugCase03SeekPositions;

uses SysUtils;

const
  FileName = 'dbg_case03.bin';

var
  F: file of Longint;
  V, Count: Longint;
begin
  Writeln('[CASE03] begin');
  Assign(F, FileName);
  {$I-}
  Rewrite(F);
  Writeln('[CASE03] rewrite IORes=', IOResult);
  V := 11; BlockWrite(F, V, 1);
  V := 22; BlockWrite(F, V, 1);
  V := 33; BlockWrite(F, V, 1);
  Writeln('[CASE03] wrote 3 entries IORes=', IOResult);
  Close(F);
  Reset(F);
  Writeln('[CASE03] reset IORes=', IOResult, ' filepos=', FilePos(F));
  Seek(F, 1);
  Writeln('[CASE03] seek1 IORes=', IOResult, ' filepos=', FilePos(F));
  BlockRead(F, V, 1, Count);
  Writeln('[CASE03] read_after_seek value=', V, ' count=', Count,
    ' filepos=', FilePos(F), ' IORes=', IOResult);
  Seek(F, 2);
  Writeln('[CASE03] seek2 IORes=', IOResult, ' filepos=', FilePos(F));
  BlockRead(F, V, 1, Count);
  Writeln('[CASE03] read2 value=', V, ' count=', Count,
    ' filepos=', FilePos(F), ' IORes=', IOResult);
  Seek(F, 3);
  Writeln('[CASE03] seek3 IORes=', IOResult, ' filepos=', FilePos(F));
  BlockRead(F, V, 1, Count);
  Writeln('[CASE03] eof_read count=', Count, ' filepos=', FilePos(F),
    ' IORes=', IOResult);
  Close(F);
  DeleteFile(FileName);
  {$I+}
  Writeln('[CASE03] done');
end.
