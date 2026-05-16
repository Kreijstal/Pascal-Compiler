program const_array_explicit_enum_ordinals;
{$mode objfpc}
{ Regression test: const arrays whose index type is an enum subrange where the
  enum literals carry explicit ordinal values (e.g. `foo = 35`). The lower
  bound of the array must be the real ordinal (35), not the literal's
  position in the enum declaration. Mismatching write- and read-paths used to
  cause inc()/dec() codegen in pp_bootstrap to emit A_NONE for OP_ADD/OP_SUB
  because addsubop: array[in_inc_x..in_dec_x] of TOpCG read garbage. }
type
  tinline = (in_none = -1, in_dummy1 = 30, in_dummy2 = 31,
             in_inc_x = 35, in_dec_x = 36);
  topcg = (OP_NONE, OP_MOVE, OP_ADD, OP_AND, OP_SUB);

procedure show(n: tinline);
const
  addsubop: array[in_inc_x..in_dec_x] of topcg = (OP_ADD, OP_SUB);
var
  op: topcg;
begin
  op := addsubop[n];
  writeln('n=', ord(n), ' op=', ord(op));
end;

begin
  show(in_inc_x);
  show(in_dec_x);
end.
