program DebugCase02BlockReadAdvances;

uses SysUtils;

const
  FileName = 'dbg_case02.bin';

var
  F: file of Longint;
  V, Count: Longint;
  I: Integer;
begin
  Writeln('[CASE02] begin');
  Assign(F, FileName);
  {$I-}
  Rewrite(F);
  Writeln('[CASE02] rewrite IORes=', IOResult);
  for I := 0 to 4 do
  begin
    V := I * 10;
    BlockWrite(F, V, 1);
    Writeln('[CASE02] write value=', V, ' IORes=', IOResult);
  end;
  Close(F);
  Writeln('[CASE02] close_after_write IORes=', IOResult);
  Reset(F);
  Writeln('[CASE02] reset IORes=', IOResult, ' filepos=', FilePos(F));
  for I := 0 to 4 do
  begin
    BlockRead(F, V, 1, Count);
    Writeln('[CASE02] read idx=', I, ' value=', V, ' count=', Count,
      ' filepos=', FilePos(F), ' IORes=', IOResult);
  end;
  BlockRead(F, V, 1, Count);
  Writeln('[CASE02] eof_read count=', Count, ' filepos=', FilePos(F),
    ' IORes=', IOResult);
  Close(F);
  Writeln('[CASE02] final_close IORes=', IOResult);
  DeleteFile(FileName);
  {$I+}
  Writeln('[CASE02] done');
end.
