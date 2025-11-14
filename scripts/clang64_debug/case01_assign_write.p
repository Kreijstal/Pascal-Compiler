program DebugCase01AssignWrite;

uses SysUtils;

type
  TValues = array[0..2] of Longint;

var
  F: file of Longint;
  Values: TValues;
  Written: Longint;
begin
  Writeln('[CASE01] begin');
  Assign(F, 'dbg_case01.bin');
  {$I-}
  Rewrite(F);
  Writeln('[CASE01] rewrite IORes=', IOResult);
  Values[0] := 11;
  Values[1] := 22;
  Values[2] := 33;
  BlockWrite(F, Values, Length(Values), Written);
  Writeln('[CASE01] blockwrite count=', Written, ' IORes=', IOResult);
  Close(F);
  Writeln('[CASE01] close IORes=', IOResult);
  {$I+}
  DeleteFile('dbg_case01.bin');
  Writeln('[CASE01] done');
end.
