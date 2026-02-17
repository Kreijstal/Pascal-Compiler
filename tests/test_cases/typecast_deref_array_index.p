program TypecastDerefArrayIndex;
{$mode objfpc}

{ Test that PArrayType(ptr)^[index] works correctly.
  The parser must handle the chain: typecast -> deref -> array index,
  propagating array type info through the pointer dereference.
  This pattern is used extensively in FPC's system unit for
  dynamic array and variant record operations. }

type
  TIntArr = array[0..3] of Integer;
  PIntArr = ^TIntArr;

  TPointerArray = array[0..7] of Pointer;
  PPointerArray = ^TPointerArray;

var
  intArr: TIntArr;
  ptrArr: TPointerArray;
  p: Pointer;
  pp: Pointer;
  r: Integer;
  rp: Pointer;
begin
  { Test 1: PIntArr(p)^[i] }
  intArr[0] := 42;
  intArr[1] := 99;
  intArr[2] := -7;
  intArr[3] := 1000;
  p := @intArr;

  r := PIntArr(p)^[0];
  WriteLn(r);
  r := PIntArr(p)^[1];
  WriteLn(r);
  r := PIntArr(p)^[2];
  WriteLn(r);
  r := PIntArr(p)^[3];
  WriteLn(r);

  { Test 2: PPointerArray(p)^[i] }
  ptrArr[0] := Pointer(100);
  ptrArr[1] := Pointer(200);
  ptrArr[2] := Pointer(300);
  pp := @ptrArr;

  rp := PPointerArray(pp)^[0];
  WriteLn(PtrUInt(rp));
  rp := PPointerArray(pp)^[1];
  WriteLn(PtrUInt(rp));
  rp := PPointerArray(pp)^[2];
  WriteLn(PtrUInt(rp));

  { Test 3: Separate variable then deref+index (sanity check) }
  r := PIntArr(@intArr)^[0];
  WriteLn(r);
end.
