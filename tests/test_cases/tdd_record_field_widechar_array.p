{$mode objfpc}
program tdd_record_field_widechar_array;
uses sysutils;
{ Regression: AnsiString assigned into a `record.field: array[..] of widechar`
  must widen each byte to a UTF-16 unit.  Mirrors FPC's
  `Assign(out f:File; const Name: RawByteString)` writing
  `FileRec(f).Name := Name` on Win64 — without the dedicated dispatch,
  the AnsiString payload was memcpy'd raw and CreateFileW received a
  corrupt PWideChar. }
type
  TRec = record
    name: array[0..15] of widechar;
  end;
var
  r: TRec;
  s: ansistring;
  i: longint;
  ok: boolean;
begin
  s := 'hi.pas';
  r.name := s;
  ok := true;
  if Ord(r.name[0]) <> $0068 then ok := false;
  if Ord(r.name[1]) <> $0069 then ok := false;
  if Ord(r.name[2]) <> $002E then ok := false;
  if Ord(r.name[6]) <> $0000 then ok := false;
  if ok then WriteLn('OK')
  else begin
    Write('FAIL r.name=');
    for i := 0 to 7 do
      Write(IntToHex(Ord(r.name[i]), 4), ' ');
    WriteLn;
  end;
end.
