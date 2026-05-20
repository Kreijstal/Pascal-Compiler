{ Three-unit regression for the cross-unit same-named typed-const stride/bounds
  bug.  Each of units _a, _b, _c declares a private (implementation-section)
  typed-const named `msg` with a different element type (string[5], string[10],
  string[8]) and a different lower bound (0..2, 0..3, 1..3).  Before the
  FindIdent_Tree same-unit-preference fix, codegen — which runs with
  skip_unit_filter=1 — would resolve every read of `msg` to whichever unit's
  HashNode happened to sit first in the dep_scopes walk, so write_a (in unit_a)
  would index into unit_c's array with stride 9 and lower bound 1, printing
  garbage for msg[0].  The structural fix at the lookup level ensures each
  call site's `msg` reference resolves to the read site's own unit's HashNode. }
program typed_const_cross_unit_same_name_three_units;

uses
  typed_const_cross_unit_same_name_three_units_a,
  typed_const_cross_unit_same_name_three_units_b,
  typed_const_cross_unit_same_name_three_units_c;

begin
  write_a;
  write_b;
  write_c;
end.
