program NonLocalArrayAccess;

{ Test non-local access to arrays with constant bounds }

const
  ArraySize = 10;
  MinIndex = -1;

type
  TData = record
    value: integer;
    flag: boolean;
  end;

var
  GlobalArray: array[MinIndex..ArraySize] of integer;
  GlobalRecordArray: array[0..5] of TData;

procedure TestNestedAccess;
var
  local: integer;
begin
  { Access global array from nested procedure }
  GlobalArray[0] := 42;
  GlobalArray[MinIndex] := -100;
  GlobalArray[ArraySize] := 999;
  
  { Access global record array }
  GlobalRecordArray[0].value := 123;
  GlobalRecordArray[0].flag := true;
  
  local := GlobalArray[0];
  if local = 42 then
    WriteLn('Nested access OK');
end;

procedure TestDeeplyNested;
  procedure Inner;
  begin
    { Deep nesting - access global from inner nested procedure }
    GlobalArray[1] := 555;
    GlobalRecordArray[1].value := 777;
  end;
begin
  Inner;
  if GlobalArray[1] = 555 then
    WriteLn('Deep nesting OK');
end;

function GetArrayValue(index: integer): integer;
begin
  GetArrayValue := GlobalArray[index];
end;

begin
  { Initialize }
  GlobalArray[0] := 0;
  GlobalArray[MinIndex] := 0;
  
  { Test nested access }
  TestNestedAccess;
  
  { Verify values }
  if GlobalArray[0] = 42 then
    WriteLn('Value test 1 passed');
  if GlobalArray[MinIndex] = -100 then
    WriteLn('Value test 2 passed');
  if GlobalArray[ArraySize] = 999 then
    WriteLn('Value test 3 passed');
  if GlobalRecordArray[0].value = 123 then
    WriteLn('Record test passed');
    
  { Test deep nesting }
  TestDeeplyNested;
  if GlobalRecordArray[1].value = 777 then
    WriteLn('Deep record test passed');
    
  { Test function access }
  if GetArrayValue(0) = 42 then
    WriteLn('Function test passed');
    
  WriteLn('All non-local array tests completed');
end.
