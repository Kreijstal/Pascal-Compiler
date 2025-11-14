program DebugCase02ResetRead;

uses SysUtils;

type
  TValues = array[0..2] of Longint;

var
  F: file of Longint;
  Values, ReadBack: TValues;
  ReadCount: Longint;
begin
  Writeln('[CASE02] begin');
  Assign(F, 'dbg_case02.bin');
  {$I-}
  Rewrite(F);
  Writeln('[CASE02] rewrite IORes=', IOResult);
  Values[0] := 101;
  Values[1] := 202;
  Values[2] := 303;
  BlockWrite(F, Values, Length(Values));
  Writeln('[CASE02] blockwrite IORes=', IOResult);
  Close(F);
  Writeln('[CASE02] close after write IORes=', IOResult);
  Reset(F);
  Writeln('[CASE02] reset IORes=', IOResult);
  BlockRead(F, ReadBack, Length(ReadBack), ReadCount);
  Writeln('[CASE02] blockread count=', ReadCount, ' IORes=', IOResult);
  Close(F);
  Writeln('[CASE02] close after read IORes=', IOResult);
  {$I+}
  DeleteFile('dbg_case02.bin');
  Writeln('[CASE02] done values=', ReadBack[0], ',', ReadBack[1], ',', ReadBack[2]);
end.
