{ Regression test: two units that each declare a same-named typed-const
  `mymap` and register it into a shared linked list `mappings` (also
  unit-defined).  Before the per-unit storage-key fix in codegen.c, the
  FLAT stack manager aliased every unit's `mymap` to the first-registered
  one, so `mappings` ended up self-referential and the program printed the
  same entry twice (or hung in an infinite loop).  After the fix each
  unit's `mymap` lives at a distinct stack node so the linked list
  contains both entries. }
program multi_unit_same_typed_const_name;
uses
  multi_unit_same_typed_const_name_unit_a,
  multi_unit_same_typed_const_name_unit_b;

var
  p : pmymap;

begin
  p := mappings;
  while p <> nil do
  begin
    writeln(p^.name, ' cp=', p^.cp);
    p := p^.next;
  end;
end.
