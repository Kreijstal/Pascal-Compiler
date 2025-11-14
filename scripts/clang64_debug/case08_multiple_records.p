program DebugCase08MultipleRecords;

uses SysUtils;

type
  TValues = array[0..5] of Longint;

var
  F: file of Longint;
  Data: TValues;
  I: Integer;
  Count: Longint;
begin
  Writeln('[CASE08] begin');
  Assign(F, 'dbg_case08.bin');
  {$I-}
  Rewrite(F);
  Writeln('[CASE08] rewrite IORes=', IOResult);
  for I := 0 to High(Data) do
    Data[I] := I * 5;
  BlockWrite(F, Data, Length(Data), Count);
  Writeln('[CASE08] blockwrite count=', Count, ' IORes=', IOResult);
  Close(F);
  Reset(F);
  Writeln('[CASE08] reset IORes=', IOResult);
  for I := 0 to High(Data) do
  begin
    BlockRead(F, Data[I], 1, Count);
    Writeln('[CASE08] read idx=', I, ' val=', Data[I], ' count=', Count, ' IORes=', IOResult);
  end;
  Close(F);
  {$I+}
  DeleteFile('dbg_case08.bin');
  Writeln('[CASE08] done');
end.
