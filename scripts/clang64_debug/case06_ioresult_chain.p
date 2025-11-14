program DebugCase06MixedReads;

uses SysUtils;

const
  FileName = 'dbg_case06.bin';

var
  F: file of Longint;
  Buf: array[0..3] of Longint;
  Count, I: Longint;
begin
  Writeln('[CASE06] begin');
  Assign(F, FileName);
  {$I-}
  Rewrite(F);
  for I := 0 to 3 do
  begin
    Buf[I] := I + 1;
    BlockWrite(F, Buf[I], 1);
  end;
  Writeln('[CASE06] wrote values IORes=', IOResult);
  Close(F);
  Reset(F);
  Writeln('[CASE06] reset IORes=', IOResult);
  BlockRead(F, Buf, 2, Count);
  Writeln('[CASE06] blockread_two count=', Count, ' filepos=', FilePos(F),
    ' IORes=', IOResult);
  BlockRead(F, Buf, 1, Count);
  Writeln('[CASE06] blockread_one count=', Count, ' filepos=', FilePos(F),
    ' IORes=', IOResult);
  BlockRead(F, Buf, 5, Count);
  Writeln('[CASE06] blockread_overflow count=', Count, ' filepos=', FilePos(F),
    ' IORes=', IOResult);
  Close(F);
  DeleteFile(FileName);
  {$I+}
  Writeln('[CASE06] done');
end.
