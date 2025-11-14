program DebugCase10AppendSimulation;

uses SysUtils;

const
  FileName = 'dbg_case10.bin';

var
  F: file of Longint;
  V, Count, PosBefore: Longint;
  EndPos: Longint;
begin
  Writeln('[CASE10] begin');
  Assign(F, FileName);
  {$I-}
  Rewrite(F);
  Writeln('[CASE10] rewrite IORes=', IOResult);
  V := 5;
  BlockWrite(F, V, 1);
  Writeln('[CASE10] initial write IORes=', IOResult);
  Close(F);
  Reset(F);
  Writeln('[CASE10] reset IORes=', IOResult, ' filepos=', FilePos(F));
  V := 0;
  Count := 0;
  repeat
    PosBefore := FilePos(F);
    BlockRead(F, V, 1, Count);
    Writeln('[CASE10] scan pos_before=', PosBefore, ' count=', Count,
      ' value=', V, ' IORes=', IOResult);
  until Count = 0;
  EndPos := FilePos(F);
  Writeln('[CASE10] reached eof filepos=', EndPos, ' IORes=', IOResult);
  Seek(F, EndPos);
  Writeln('[CASE10] seek_to_end IORes=', IOResult, ' filepos=', FilePos(F));
  V := 99;
  BlockWrite(F, V, 1);
  Writeln('[CASE10] append write IORes=', IOResult, ' filepos=', FilePos(F));
  Close(F);
  Reset(F);
  Writeln('[CASE10] reset_after_append IORes=', IOResult);
  repeat
    BlockRead(F, V, 1, Count);
    Writeln('[CASE10] readback count=', Count, ' value=', V,
      ' filepos=', FilePos(F), ' IORes=', IOResult);
  until Count = 0;
  Close(F);
  DeleteFile(FileName);
  {$I+}
  Writeln('[CASE10] done');
end.
