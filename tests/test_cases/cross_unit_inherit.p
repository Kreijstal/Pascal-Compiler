program cross_unit_inherit;

{ Test that a program can use a unit that inherits from a class
  defined in another unit. The parent class (TBaseItem) is in
  cross_unit_inherit_base, and TChildItem inherits from it in
  cross_unit_inherit_child. The program only uses the child unit,
  so the parent must be resolved transitively. }

uses cross_unit_inherit_child;

var
  item: TChildItem;
begin
  item := TChildItem.Create;
  writeln(item.FBaseData);
  writeln(item.FChildData);
end.
