program ArrayAssignmentTest;
{ Test case to verify array assignment copies all elements, not just the first one.
  This is a regression test for the bug where array assignment only copied the first element.
  See: https://github.com/Kreijstal/Pascal-Compiler/issues/XXX }

type
  TSmallArray = array[1..5] of integer;
  TCharArray = array[1..10] of char;

var
  intArr1, intArr2: TSmallArray;
  charArr1, charArr2: TCharArray;
  i: integer;
  allMatch: boolean;

begin
  { Test 1: Integer array assignment }
  for i := 1 to 5 do
    intArr1[i] := i * 10;
  
  { This assignment should copy all elements }
  intArr2 := intArr1;
  
  { Verify all elements were copied }
  allMatch := true;
  for i := 1 to 5 do begin
    if intArr2[i] <> intArr1[i] then begin
      WriteLn('FAIL: intArr2[', i:1, '] = ', intArr2[i]:1, ', expected ', intArr1[i]:1);
      allMatch := false;
    end;
  end;
  
  if allMatch then
    WriteLn('PASS: Integer array assignment')
  else
    WriteLn('FAIL: Integer array assignment');
  
  { Test 2: Character array assignment }
  for i := 1 to 10 do
    charArr1[i] := chr(ord('A') + i - 1);
  
  { This assignment should copy all elements }
  charArr2 := charArr1;
  
  { Verify all elements were copied }
  allMatch := true;
  for i := 1 to 10 do begin
    if charArr2[i] <> charArr1[i] then begin
      WriteLn('FAIL: charArr2[', i:1, '] = ', ord(charArr2[i]):1, ', expected ', ord(charArr1[i]):1);
      allMatch := false;
    end;
  end;
  
  if allMatch then
    WriteLn('PASS: Character array assignment')
  else
    WriteLn('FAIL: Character array assignment');
end.
