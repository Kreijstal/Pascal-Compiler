{ Test pointer-pointer subtraction - required for FPC system.pp bootstrap }
{ KGPC GAP: This test documents a feature not yet supported by KGPC. }
{ When two pointers of the same type are subtracted, the result is the }
{ difference in elements (not bytes) between the two addresses. }
{ This is essential for string length calculations like strlen. }
{ See docs/FPC_BOOTSTRAP_GAPS.md for details. }
program TestPointerSubtraction;

type
  PByte = ^Byte;

var
  arr: array[0..9] of Byte;
  p, q: PByte;
  diff: LongInt;

begin
  { Initialize array }
  arr[0] := 0;
  arr[9] := 9;
  
  { Set pointers to different locations }
  p := @arr[7];
  q := @arr[2];
  
  { Pointer subtraction should give the difference in elements }
  diff := p - q;  { Should be 5 }
  WriteLn(diff);
  
  { Test reverse subtraction }
  diff := q - p;  { Should be -5 }
  WriteLn(diff);
end.
