program small_integer_alias_sizes;
{$mode objfpc}

{ Test: SizeOf, High, and Low with small integer type aliases }
{ This validates that type aliases preserve correct size/bounds information }
{ Issue: predeclare_types was mapping all integer aliases to LONGINT_TYPE }

type
  MyWord = Word;           { 16-bit unsigned: 2 bytes, 0..65535 }
  MyByte = Byte;           { 8-bit unsigned: 1 byte, 0..255 }
  MyCardinal = Cardinal;   { 32-bit unsigned: 4 bytes, 0..4294967295 }
  MySmallInt = SmallInt;   { 16-bit signed: 2 bytes, -32768..32767 }
  MyShortInt = ShortInt;   { 8-bit signed: 1 byte, -128..127 }

const
  { SizeOf tests }
  WordSize = SizeOf(MyWord);
  ByteSize = SizeOf(MyByte);
  CardinalSize = SizeOf(MyCardinal);
  SmallIntSize = SizeOf(MySmallInt);
  ShortIntSize = SizeOf(MyShortInt);

  { High tests }
  MaxWord = High(MyWord);
  MaxByte = High(MyByte);
  MaxCardinal = High(MyCardinal);
  MaxSmallInt = High(MySmallInt);
  MaxShortInt = High(MyShortInt);

  { Low tests }
  MinWord = Low(MyWord);
  MinByte = Low(MyByte);
  MinCardinal = Low(MyCardinal);
  MinSmallInt = Low(MySmallInt);
  MinShortInt = Low(MyShortInt);

begin
  { Output SizeOf values }
  writeln('SizeOf(MyWord) = ', WordSize);
  writeln('SizeOf(MyByte) = ', ByteSize);
  writeln('SizeOf(MyCardinal) = ', CardinalSize);
  writeln('SizeOf(MySmallInt) = ', SmallIntSize);
  writeln('SizeOf(MyShortInt) = ', ShortIntSize);

  { Output High values }
  writeln('High(MyWord) = ', MaxWord);
  writeln('High(MyByte) = ', MaxByte);
  writeln('High(MyCardinal) = ', MaxCardinal);
  writeln('High(MySmallInt) = ', MaxSmallInt);
  writeln('High(MyShortInt) = ', MaxShortInt);

  { Output Low values }
  writeln('Low(MyWord) = ', MinWord);
  writeln('Low(MyByte) = ', MinByte);
  writeln('Low(MyCardinal) = ', MinCardinal);
  writeln('Low(MySmallInt) = ', MinSmallInt);
  writeln('Low(MyShortInt) = ', MinShortInt);
end.
