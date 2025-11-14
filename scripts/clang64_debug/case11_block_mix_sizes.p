program DebugCase11AppendSequences;

uses SysUtils;

const
  FileName = 'dbg_case11.bin';

var
  F: file of Longint;
  V, Count: Longint;

procedure AppendValue(Value: Longint; const Tag: string);
var
  PosBefore: Longint;
begin
  Assign(F, FileName);
  Reset(F);
  PosBefore := FilePos(F);
  Seek(F, PosBefore);
  Writeln(Tag, ' filepos_before=', PosBefore);
  V := Value;
  BlockWrite(F, V, 1);
  Writeln(Tag, ' append IORes=', IOResult, ' filepos=', FilePos(F));
  Close(F);
end;

begin
  Writeln('[CASE11] begin');
  Assign(F, FileName);
  {$I-}
  Rewrite(F);
  Writeln('[CASE11] rewrite IORes=', IOResult);
  V := 1; BlockWrite(F, V, 1);
  V := 2; BlockWrite(F, V, 1);
  Close(F);

  AppendValue(10, '[CASE11] pass1');
  AppendValue(20, '[CASE11] pass2');
  AppendValue(30, '[CASE11] pass3');

  Assign(F, FileName);
  Reset(F);
  BlockRead(F, V, 1, Count);
  Writeln('[CASE11] read1 count=', Count, ' value=', V,
    ' filepos=', FilePos(F), ' IORes=', IOResult);
  BlockRead(F, V, 1, Count);
  Writeln('[CASE11] read2 count=', Count, ' value=', V,
    ' filepos=', FilePos(F), ' IORes=', IOResult);
  BlockRead(F, V, 1, Count);
  Writeln('[CASE11] read3 count=', Count, ' value=', V,
    ' filepos=', FilePos(F), ' IORes=', IOResult);
  BlockRead(F, V, 1, Count);
  Writeln('[CASE11] read4 count=', Count, ' value=', V,
    ' filepos=', FilePos(F), ' IORes=', IOResult);
  BlockRead(F, V, 1, Count);
  Writeln('[CASE11] read5 count=', Count, ' value=', V,
    ' filepos=', FilePos(F), ' IORes=', IOResult);
  Close(F);
  DeleteFile(FileName);
  {$I+}
  Writeln('[CASE11] done');
end.
