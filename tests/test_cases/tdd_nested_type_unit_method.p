program tdd_nested_type_unit_method;
uses tdd_nested_type_unit_method_unit;
var
  list: TContainer.TList;
  p: TContainer.pItem;
begin
  list.head := nil;
  list.count := 0;
  p := MakeItem(42);
  list.Add(p);
  writeln(list.head^.value);
  writeln(list.count);
  Dispose(p);
end.
