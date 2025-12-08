{
  Test: Procedural types (function/procedure types as variables)
  
  FPC Behavior: Supports declaring types that are function/procedure signatures
  Example: type TComparer = function(a, b: Pointer): Integer;
  
  This is used in sortbase.pp for pluggable sorting algorithms.
  
  CRITICAL for FPC bootstrap: sortbase.pp uses this extensively.
}
program ProceduralTypes;

type
  TComparer = function(Item1, Item2: Pointer): Integer;
  TSorter = procedure(Items: Pointer; Count: SizeUInt; Comparer: TComparer);
  
var
  MyComparer: TComparer;
  MySorter: TSorter;

function SimpleCompare(Item1, Item2: Pointer): Integer;
begin
  SimpleCompare := 0;
end;

procedure SimpleSortProcedure(Items: Pointer; Count: SizeUInt; Comparer: TComparer);
begin
  WriteLn('Sorting ', Count, ' items');
end;

begin
  MyComparer := @SimpleCompare;
  MySorter := @SimpleSortProcedure;
  
  if Assigned(MyComparer) then
    WriteLn('Comparer assigned');
  
  if Assigned(MySorter) then
    WriteLn('Sorter assigned');
    
  MySorter(nil, 5, MyComparer);
  
  WriteLn('Procedural types test passed');
end.
