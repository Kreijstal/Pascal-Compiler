program test_ptr_arith;
{$mode objfpc}
{$POINTERMATH ON}

var
  buf: array[0..15] of Byte;
  p, p2: PByte;
  i: Integer;
  diff: PtrInt;
begin
  { Fill buffer with known values }
  for i := 0 to 15 do
    buf[i] := i * 10;

  { Get pointer to start }
  p := @buf[0];
  
  { Pointer + integer }
  p2 := p + 5;
  WriteLn('p+5: ', p2^);  { Should be 50 }
  
  { Pointer - integer }
  p2 := p + 10;
  p2 := p2 - 3;
  WriteLn('p+10-3: ', p2^);  { Should be 70 }
  
  { Pointer difference }
  p := @buf[0];
  p2 := @buf[8];
  diff := p2 - p;
  WriteLn('diff: ', diff);  { Should be 8 }
  
  { Pointer comparison }
  if p2 > p then
    WriteLn('p2 > p: OK')
  else
    WriteLn('p2 > p: FAIL');
end.
