program tdd_nested_type_in_unit_method;

{ Test that nested types defined inside objects within a unit can be resolved
  in method implementations without full qualification. This tests the pattern
  from FPC RTL's heap.inc where HeapInc.pFreeVarChunk is used as just
  pFreeVarChunk in HeapInc.VarFreeMap.Add method body. }

type
  TContainer = object
  type
    pItem = ^TItem;
    TItem = record
      value: Integer;
      next: pItem;
    end;
    TList = object
      head: pItem;
      procedure Add(item: pItem);
      function GetFirst: pItem;
    end;
  end;

procedure TContainer.TList.Add(item: pItem);
begin
  item^.next := head;
  head := item;
end;

function TContainer.TList.GetFirst: pItem;
begin
  Result := head;
end;

var
  list: TContainer.TList;
  p, q: TContainer.pItem;
begin
  list.head := nil;
  New(p);
  p^.value := 42;
  p^.next := nil;
  list.Add(p);
  q := list.GetFirst;
  writeln(q^.value);
  Dispose(p);
end.
