program DebugCase04ReadUntilEOF;

uses SysUtils;

const
  FileName = 'dbg_case04.bin';

var
  F: file of Longint;
  V, Count: Longint;
  LoopPos: Longint;
begin
  Writeln('[CASE04] begin');
  Assign(F, FileName);
  {$I-}
  Rewrite(F);
  Writeln('[CASE04] rewrite IORes=', IOResult);
  V := 7; BlockWrite(F, V, 1);
  V := 9; BlockWrite(F, V, 1);
  V := 12; BlockWrite(F, V, 1);
  Close(F);
  Reset(F);
  Writeln('[CASE04] reset IORes=', IOResult);
  repeat
    LoopPos := FilePos(F);
    BlockRead(F, V, 1, Count);
    Writeln('[CASE04] loop filepos_before=', LoopPos,
      ' count=', Count, ' value=', V, ' IORes=', IOResult);
  until Count = 0;
  Writeln('[CASE04] final filepos=', FilePos(F), ' IORes=', IOResult);
  Seek(F, FilePos(F));
  Writeln('[CASE04] seek_to_eof IORes=', IOResult, ' filepos=', FilePos(F));
  Close(F);
  DeleteFile(FileName);
  {$I+}
  Writeln('[CASE04] done');
end.
