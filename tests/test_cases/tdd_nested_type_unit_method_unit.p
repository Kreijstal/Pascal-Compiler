unit tdd_nested_type_unit_method_unit;

interface

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
      count: Integer;
      procedure Add(item: pItem);
    end;
  end;

function MakeItem(v: Integer): TContainer.pItem;

implementation

procedure TContainer.TList.Add(item: pItem);
begin
  item^.next := head;
  head := item;
  Inc(count);
end;

function MakeItem(v: Integer): TContainer.pItem;
begin
  New(Result);
  Result^.value := v;
  Result^.next := nil;
end;

end.
