{ Test: Complex record helper with const default parameter }
{ Verifies parser handles const enum value as default param }
program impl_guidhelper_methods;

type
  TEndian = (Little, Big);
  TBytes = array of Byte;
  TGUID = record
    D1: LongWord;
    D2: Word;
    D3: Word;
    D4: array[0..7] of Byte;
  end;

const
  CPUEndian = TEndian.Little;

type
  TGUIDHelper = record helper for TGUID
    function ToByteArray(DataEndian: TEndian = CPUEndian): TBytes;
    function ToString(SkipBrackets: Boolean = False): string;
  end;

function TGUIDHelper.ToByteArray(DataEndian: TEndian = CPUEndian): TBytes;
begin
  SetLength(Result, 16);
  if DataEndian <> CPUEndian then
  begin
    PCardinal(@Result[0])^ := D1;
    PWord(@Result[4])^ := D2;
    PWord(@Result[6])^ := D3;
    Move(D4, Result[8], 8);
  end
  else
    Move(D1, Result[0], SizeOf(Self));
end;

function TGUIDHelper.ToString(SkipBrackets: Boolean = False): string;
begin
  Result := '{GUID}';
  if SkipBrackets then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

var
  G: TGUID;
  B: TBytes;
begin
  G.D1 := $12345678;
  G.D2 := $ABCD;
  G.D3 := $EF01;
  WriteLn(G.ToString);
  WriteLn(G.ToString(True));
  B := G.ToByteArray;
  WriteLn(Length(B));
end.
