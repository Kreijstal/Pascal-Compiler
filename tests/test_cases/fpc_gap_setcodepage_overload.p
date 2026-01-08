program fpc_gap_setcodepage_overload;
{$mode objfpc}
{ Test: SetCodePage with RawByteString
  FPC System unit has SetCodePage(var s: RawByteString; CodePage: TSystemCodePage; Convert: Boolean = True)
  Expected output: 65001 }

type
  TSystemCodePage = Word;

var
  S: RawByteString;
  CP: TSystemCodePage;
begin
  S := 'test';
  CP := 65001;  { UTF-8 }
  SetCodePage(S, CP);
  writeln(CP);
end.
