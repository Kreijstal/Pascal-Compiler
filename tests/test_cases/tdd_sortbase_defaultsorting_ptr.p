program TDDSortbaseDefaultSortingPtr;

uses SortBase;

function DummyCompare(Item1, Item2: Pointer): Integer;
begin
  DummyCompare := 0;
end;

begin
  if PtrUInt(DefaultSortingAlgorithm^.PtrListSorter_NoContextComparer) =
     PtrUInt(@QuickSort_PtrList_NoContext) then
    WriteLn('OK')
  else
    WriteLn('BAD');
end.
