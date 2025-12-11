program FPCSortbaseMinimal;
{
  Minimal reproduction of FPC RTL sortbase.pp pattern.
  This demonstrates the procedural type pattern that blocks sortbase.pp compilation.
  
  Based on:
  - rtl/inc/sortbase.pp lines 26-44
}

type
  { Procedural types without "reference to" - traditional Pascal syntax }
  TComparer = function(Item1, Item2: Pointer): Integer;
  TSorter = procedure(Items: Pointer; Count: Integer; Comp: TComparer);
  
  { Record containing procedural type fields }
  TSortingAlgorithm = record
    Sorter: TSorter;
    Comparer: TComparer;
  end;

{ Sample comparer function }
function DefaultComparer(Item1, Item2: Pointer): Integer;
begin
  DefaultComparer := 0;
end;

{ Sample sorter procedure }
procedure DefaultSorter(Items: Pointer; Count: Integer; Comp: TComparer);
begin
  WriteLn('Sorting ', Count, ' items');
end;

const
  { Initialize const record with function pointers - FPC pattern }
  QuickSort: TSortingAlgorithm = (
    Sorter: @DefaultSorter;
    Comparer: @DefaultComparer;
  );

var
  algo: TSortingAlgorithm;

begin
  { Use the const-initialized algorithm }
  algo := QuickSort;
  
  { Call through the procedural fields }
  algo.Sorter(nil, 10, algo.Comparer);
  WriteLn('Comparer result: ', algo.Comparer(nil, nil));
  
  WriteLn('Test completed');
end.
