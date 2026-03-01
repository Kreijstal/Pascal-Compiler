program tdd_type_shadow;

{ TSize from unit_b (record with cx/cy) should shadow TSize from unit_a (NativeUInt).
  In Pascal, the last unit in the uses clause wins for name conflicts. }

uses tdd_type_shadow_unit_a, tdd_type_shadow_unit_b;

var
  s: TSize;
begin
  s.cx := 10;
  s.cy := 20;
  writeln(s.cx);
  writeln(s.cy);
end.
