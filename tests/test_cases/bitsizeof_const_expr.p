{$mode objfpc}
program bitsizeof_const_expr;

{ Test bitsizeof() in constant expressions and at runtime.
  This exercises the pattern from classes.pp:
    TBitsBase = PtrInt;
    MaxBitFlags = High(SizeInt) - (bitsizeof(TBitsBase) - 1);
  BitSizeOf(T) must be evaluable in const context, returning SizeOf(T)*8. }

type
  TMyInt = LongInt;        { 4 bytes = 32 bits }
  TMyByte = Byte;          { 1 byte = 8 bits }
  TMyWord = Word;          { 2 bytes = 16 bits }
  TMyInt64 = Int64;        { 8 bytes = 64 bits }
  TBitsBase = PtrInt;      { 8 bytes on x86-64 = 64 bits }

const
  { These must all be evaluated at compile time as constant expressions }
  BitsOfInt    = bitsizeof(TMyInt);         { expect 32 }
  BitsOfByte   = bitsizeof(TMyByte);        { expect 8 }
  BitsOfWord   = bitsizeof(TMyWord);        { expect 16 }
  BitsOfInt64  = bitsizeof(TMyInt64);       { expect 64 }
  BitsOfBase   = bitsizeof(TBitsBase);      { expect 64 on x86-64 }

  { Compound constant expressions using bitsizeof - mirrors classes.pp pattern }
  TBITS_SHIFT = 6;  { log2(64) for 64-bit PtrInt }
  TBITS_MASK  = (1 shl TBITS_SHIFT) - 1;   { = 63 }
  MaxBitFlags = High(SizeInt) - (bitsizeof(TBitsBase) - 1);

  { Arithmetic in const context }
  HalfBitsOfInt = bitsizeof(TMyInt) div 2;  { expect 16 }
  DoubleBitsOfByte = bitsizeof(TMyByte) * 2; { expect 16 }

begin
  { Verify compile-time constants }
  WriteLn('BitsOfInt = ', BitsOfInt);
  WriteLn('BitsOfByte = ', BitsOfByte);
  WriteLn('BitsOfWord = ', BitsOfWord);
  WriteLn('BitsOfInt64 = ', BitsOfInt64);
  WriteLn('BitsOfBase = ', BitsOfBase);
  WriteLn('MaxBitFlags = ', MaxBitFlags);
  WriteLn('HalfBitsOfInt = ', HalfBitsOfInt);
  WriteLn('DoubleBitsOfByte = ', DoubleBitsOfByte);
  WriteLn('TBITS_MASK = ', TBITS_MASK);

  { Runtime bitsizeof }
  WriteLn('bitsizeof(Byte) = ', bitsizeof(Byte));
  WriteLn('bitsizeof(Integer) = ', bitsizeof(Integer));
end.
