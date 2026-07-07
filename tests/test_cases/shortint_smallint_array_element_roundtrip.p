program shortint_smallint_array_element_roundtrip;

{ Regression: array-of-ShortInt/SmallInt elements were laid out with a
  promoted 4-byte stride while element stores wrote 1/2 bytes, so a
  negative element write followed by a read returned garbage (e.g. 156
  instead of -100).  Elements must be sized by the element type's real
  storage size (ShortInt=1, SmallInt=2) with sign-extending reads. }

type
  TShortArr = array[0..3] of ShortInt;
  TSmallArr = array[0..3] of SmallInt;

var
  gsb: array[0..3] of ShortInt;
  gsm: array[0..3] of SmallInt;
  gnb: TShortArr;
  gnm: TSmallArr;

procedure LocalRoundtrip;
var
  lsb: array[0..3] of ShortInt;
  lsm: array[0..3] of SmallInt;
begin
  lsb[0] := -128; lsb[2] := -100; lsb[3] := 127;
  lsm[0] := -32768; lsm[2] := -1000; lsm[3] := 32767;
  writeln(lsb[0]); writeln(lsb[2]); writeln(lsb[3]);
  writeln(lsm[0]); writeln(lsm[2]); writeln(lsm[3]);
end;

begin
  { layout: elements sized by real storage size }
  writeln(SizeOf(gsb));
  writeln(SizeOf(gsm));
  writeln(SizeOf(TShortArr));
  writeln(SizeOf(TSmallArr));

  { global inline-typed arrays }
  gsb[0] := -128; gsb[2] := -100; gsb[3] := 127;
  gsm[0] := -32768; gsm[2] := -1000; gsm[3] := 32767;
  writeln(gsb[0]); writeln(gsb[2]); writeln(gsb[3]);
  writeln(gsm[0]); writeln(gsm[2]); writeln(gsm[3]);

  { global named-type arrays }
  gnb[1] := -77;
  gnm[1] := -7777;
  writeln(gnb[1]);
  writeln(gnm[1]);

  { local arrays }
  LocalRoundtrip;
end.
