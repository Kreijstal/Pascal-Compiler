program nested_same_level_call;
{ Test calling a sibling nested procedure (same nesting level) with array argument }
type
  IntArray = array[1..5] of Integer;
var
  GlobalResult: Integer;

procedure Outer;
var
  OuterVar: IntArray;
  
  procedure Sibling1(arr: IntArray);
  begin
    GlobalResult := arr[1] + arr[2];
  end;
  
  procedure Sibling2;
  begin
    { Sibling2 calls Sibling1, both at the same nesting level }
    Sibling1(OuterVar);
  end;
  
begin
  OuterVar[1] := 10;
  OuterVar[2] := 20;
  Sibling2;
end;

begin
  Outer;
  writeln('Result: ', GlobalResult);
end.
