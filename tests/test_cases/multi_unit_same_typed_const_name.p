program multi_unit_same_typed_const_name;

{ Regression test for cross-unit typed-const name collision.
  Two units declare a typed-const named `mapentry` and register a
  pointer to it via the global `mappings` linked list.  Before the
  fix codegen's flat name lookup resolved BOTH @mapentry references
  to unit_a's storage, so unit_b's register call linked the list to
  itself and walking the chain printed the same entry forever.

  After the fix the two units' entries are distinct and the walk
  terminates after two iterations. }

uses
  multi_unit_same_typed_const_name_unit_a,
  multi_unit_same_typed_const_name_unit_b;

var
  p : pmapentry;
begin
  p := mappings;
  while p <> nil do
  begin
    writeln(p^.name, ' ', p^.code);
    p := p^.next;
  end;
end.
