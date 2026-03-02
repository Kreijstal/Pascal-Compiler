program test_with_proc_field_call;
{$mode objfpc}
{ Test: Procedural field calls through WITH context.
  Exercises the pattern:
    with PMyContext(ptr)^ do
      Result := CompareFn(List, Index1, Index2);
  where CompareFn is a procedural-type field of the record accessed via WITH.
  This tests:
  1. Procedural field (function pointer) in a record
  2. WITH context on a dereferenced typed pointer
  3. Calling the procedural field with arguments through WITH
  4. Accessing regular fields alongside procedural fields in WITH
  This pattern is used extensively in TStringList.CustomSort in classes.pp. }

type
  TCompareFunc = function(Index1, Index2: Integer): Integer;

  TCustomSortContext = record
    ItemCount: Integer;
    CompareFn: TCompareFunc;
    Tag: Integer;
  end;
  PCustomSortContext = ^TCustomSortContext;

var
  Items: array[0..4] of Integer;

function MyCompare(Index1, Index2: Integer): Integer;
begin
  if Items[Index1] < Items[Index2] then
    Result := -1
  else if Items[Index1] > Items[Index2] then
    Result := 1
  else
    Result := 0;
end;

function DoCompareViaWith(ctx: PCustomSortContext; i, j: Integer): Integer;
begin
  { This is the key pattern from classes.pp:
    with ctx^ do
      Result := CompareFn(i, j);
    CompareFn must resolve as the procedural field, not a standalone function. }
  with ctx^ do
    Result := CompareFn(i, j);
end;

procedure BubbleSort(ctx: PCustomSortContext);
var
  i, j, tmp: Integer;
  cmpResult: Integer;
begin
  with ctx^ do
  begin
    for i := 0 to ItemCount - 2 do
      for j := 0 to ItemCount - 2 - i do
      begin
        cmpResult := CompareFn(j, j + 1);
        if cmpResult > 0 then
        begin
          tmp := Items[j];
          Items[j] := Items[j + 1];
          Items[j + 1] := tmp;
        end;
      end;
  end;
end;

function GetTagViaWith(ctx: PCustomSortContext): Integer;
begin
  { Test accessing regular field alongside procedural field in WITH }
  with ctx^ do
    Result := Tag;
end;

var
  ctx: TCustomSortContext;
  i: Integer;
begin
  Items[0] := 42;
  Items[1] := 17;
  Items[2] := 99;
  Items[3] := 3;
  Items[4] := 55;

  ctx.ItemCount := 5;
  ctx.CompareFn := @MyCompare;
  ctx.Tag := 777;

  { Test single comparison via WITH }
  WriteLn(DoCompareViaWith(@ctx, 0, 1));  { 42 vs 17 => 1 }
  WriteLn(DoCompareViaWith(@ctx, 3, 2));  { 3 vs 99 => -1 }
  WriteLn(DoCompareViaWith(@ctx, 0, 0));  { 42 vs 42 => 0 }
  WriteLn(GetTagViaWith(@ctx));           { 777 }

  { Sort and print }
  BubbleSort(@ctx);
  for i := 0 to 4 do
    WriteLn(Items[i]);
end.
