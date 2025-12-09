program FPCProceduralTypeBasic;
{
  Test basic procedural types without "reference to" keyword.
  This is the traditional Pascal function pointer syntax used in FPC RTL.
  
  Based on FPC RTL's sortbase.pp which defines:
    TListSortComparer_NoContext = function(Item1, Item2: Pointer): Integer;
}

type
  { Basic function type - returns integer }
  TComparer = function(Item1, Item2: Pointer): Integer;
  
  { Basic procedure type - no return value }
  TProcessor = procedure(Item: Pointer);

{ A sample function that matches TComparer signature }
function SimpleComparer(Item1, Item2: Pointer): Integer;
begin
  { Simple comparison - not meaningful for pointers, just for testing }
  SimpleComparer := 0;
end;

{ A sample procedure that matches TProcessor signature }
procedure SimpleProcessor(Item: Pointer);
begin
  WriteLn('Processing item');
end;

var
  comp: TComparer;
  proc: TProcessor;

begin
  { Assign function pointers }
  comp := @SimpleComparer;
  proc := @SimpleProcessor;
  
  { Call through function pointers }
  WriteLn('Comparer result: ', comp(nil, nil));
  proc(nil);
  
  WriteLn('Test completed');
end.
