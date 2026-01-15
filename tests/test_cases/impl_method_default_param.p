{ Test: Record helper method with default parameter }
{ Verifies parser handles default params in helper methods }
program impl_method_default_param;

type
  TEndian = (Little, Big);
  TBytes = array of Byte;
  TGUID = record
    D1: LongWord;
  end;

  TGUIDHelper = record helper for TGUID
    function ToByteArray(DataEndian: TEndian = Little): TBytes;
  end;

function TGUIDHelper.ToByteArray(DataEndian: TEndian = Little): TBytes;
begin
  SetLength(Result, 4);
  if DataEndian = Little then
    Result[0] := D1 and $FF
  else
    Result[0] := (D1 shr 24) and $FF;
end;

var
  G: TGUID;
  B: TBytes;
begin
  G.D1 := $12345678;
  B := G.ToByteArray;
  WriteLn(B[0]);
  B := G.ToByteArray(Big);
  WriteLn(B[0]);
end.
