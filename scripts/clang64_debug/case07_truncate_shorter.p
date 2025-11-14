program DebugCase07ZeroCountRead;

uses SysUtils;

const
  FileName = 'dbg_case07.bin';

var
  F: file of Longint;
  V, Count: Longint;
begin
  Writeln('[CASE07] begin');
  Assign(F, FileName);
  {$I-}
  Rewrite(F);
  V := 123;
  BlockWrite(F, V, 1);
  Close(F);
  Reset(F);
  Writeln('[CASE07] reset IORes=', IOResult, ' filepos=', FilePos(F));
  BlockRead(F, V, 0, Count);
  Writeln('[CASE07] blockread_zero count=', Count, ' filepos=', FilePos(F),
    ' IORes=', IOResult);
  BlockRead(F, V, 1, Count);
  Writeln('[CASE07] blockread_one count=', Count, ' value=', V,
    ' filepos=', FilePos(F), ' IORes=', IOResult);
  Close(F);
  DeleteFile(FileName);
  {$I+}
  Writeln('[CASE07] done');
end.
