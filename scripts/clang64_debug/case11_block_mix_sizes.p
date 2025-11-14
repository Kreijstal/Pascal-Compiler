program DebugCase11BlockMix;

uses SysUtils;

type
  TSmall = array[0..1] of Longint;
  TLarge = array[0..7] of Longint;

var
  F: file of Longint;
  Small: TSmall;
  Large: TLarge;
  Count: Longint;
  I: Integer;
begin
  Writeln('[CASE11] begin');
  Assign(F, 'dbg_case11.bin');
  {$I-}
  Rewrite(F);
  Small[0] := 1; Small[1] := 2;
  BlockWrite(F, Small, Length(Small), Count);
  Writeln('[CASE11] wrote small block count=', Count, ' IORes=', IOResult);
  for I := 0 to High(Large) do
    Large[I] := I * 2;
  BlockWrite(F, Large, Length(Large), Count);
  Writeln('[CASE11] wrote large block count=', Count, ' IORes=', IOResult);
  Close(F);
  {$I+}
  DeleteFile('dbg_case11.bin');
  Writeln('[CASE11] done');
end.
