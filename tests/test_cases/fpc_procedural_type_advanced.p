program FPCProceduralTypeAdvanced;
{
  Test more advanced procedural type patterns from FPC RTL sortbase.pp:
  - Procedural types as record fields
  - Taking address of procedures/functions
  - Initializing records with procedure addresses
}

type
  { Procedural types }
  TComparer = function(Item1, Item2: Pointer): Integer;
  TProcessor = procedure(Item: Pointer);
  
  { Record containing procedural type fields }
  TAlgorithm = record
    Compare: TComparer;
    Process: TProcessor;
  end;

{ Sample functions matching the types }
function MyComparer(Item1, Item2: Pointer): Integer;
begin
  MyComparer := 0;  { Just return 0 for testing }
end;

procedure MyProcessor(Item: Pointer);
begin
  WriteLn('Processing');
end;

var
  algo: TAlgorithm;

begin
  { Assign procedure addresses to record fields }
  algo.Compare := @MyComparer;
  algo.Process := @MyProcessor;
  
  { Call through the procedural fields }
  WriteLn('Calling comparer: ', algo.Compare(nil, nil));
  algo.Process(nil);
  
  WriteLn('Test completed');
end.
