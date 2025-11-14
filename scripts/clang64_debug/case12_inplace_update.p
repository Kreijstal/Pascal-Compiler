program DebugCase12CrossFilePos;

uses SysUtils;

const
  FileA = 'dbg_case12a.bin';
  FileB = 'dbg_case12b.bin';

var
  FA, FB: file of Longint;
  V, Count: Longint;

procedure InitFile(var F: file of Longint; const Name: string; Seed: Longint);
var
  I: Integer;
begin
  Assign(F, Name);
  Rewrite(F);
  for I := 0 to 2 do
  begin
    V := Seed + I;
    BlockWrite(F, V, 1);
  end;
  Close(F);
end;

begin
  Writeln('[CASE12] begin');
  {$I-}
  InitFile(FA, FileA, 100);
  InitFile(FB, FileB, 200);
  Reset(FA);
  Reset(FB);
  Writeln('[CASE12] resetA IORes=', IOResult, ' filepos=', FilePos(FA));
  Writeln('[CASE12] resetB IORes=', IOResult, ' filepos=', FilePos(FB));
  BlockRead(FA, V, 1, Count);
  Writeln('[CASE12] readA value=', V, ' count=', Count,
    ' filepos=', FilePos(FA), ' IORes=', IOResult);
  BlockRead(FB, V, 1, Count);
  Writeln('[CASE12] readB value=', V, ' count=', Count,
    ' filepos=', FilePos(FB), ' IORes=', IOResult);
  Seek(FB, FilePos(FA));
  Writeln('[CASE12] seekB_to_fileposA IORes=', IOResult,
    ' fileposA=', FilePos(FA), ' fileposB=', FilePos(FB));
  BlockWrite(FB, 999, 1);
  Writeln('[CASE12] writeB IORes=', IOResult, ' fileposB=', FilePos(FB));
  Close(FA);
  Close(FB);
  DeleteFile(FileA);
  DeleteFile(FileB);
  {$I+}
  Writeln('[CASE12] done');
end.
