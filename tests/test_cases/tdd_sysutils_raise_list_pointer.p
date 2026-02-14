{$mode objfpc}
program tdd_sysutils_raise_list_pointer;

type
  PNode = ^TNode;
  TNode = record
    Value: LongInt;
    Next: PNode;
  end;

var
  Head: PNode;

function MakeNode(Start, Count: Integer): PNode;
var
  i: Integer;
  cur, first: PNode;
begin
  first := nil;
  cur := nil;
  for i := 0 to Count - 1 do
  begin
    New(cur);
    cur^.Value := Start + i * 3;
    cur^.Next := first;
    first := cur;
  end;
  Result := first;
end;

function GetHead: PNode;
begin
  Result := Head;
end;

function SumList(A: PNode): Integer;
var
  total: Integer;
  cur: PNode;
begin
  total := 0;
  cur := A;
  while cur <> nil do
  begin
    total := total + cur^.Value;
    cur := cur^.Next;
  end;
  Result := total;
end;

procedure Cleanup(var A: PNode);
var
  cur, next: PNode;
begin
  cur := A;
  while cur <> nil do
  begin
    next := cur^.Next;
    Dispose(cur);
    cur := next;
  end;
  A := nil;
end;

var
  firstValue, total: Integer;
  second: PNode;

begin
  Head := MakeNode(5, 4);
  second := MakeNode(2, 3);
  firstValue := GetHead^.Value;
  total := SumList(GetHead) + SumList(second);
  WriteLn('first=', firstValue);
  WriteLn('total=', total);
  Cleanup(Head);
  Cleanup(second);
end.
