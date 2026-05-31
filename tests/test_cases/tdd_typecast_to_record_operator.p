program tdd_typecast_to_record_operator;
{ Regression: an explicit typecast `RecordType(scalar)` where RecordType has an
  `operator :=(scalar): RecordType` must *invoke that operator* (FPC
  value-conversion semantics).  KGPC was instead doing a reinterpret bitcast,
  copying the 8-byte scalar over the 16-byte record and leaving the rest as
  garbage — so `o` came out TRUE and `u` came out 0 instead of o=FALSE, u=8.

  Fixed in semcheck_typecast by morphing the INNER typecast expression into the
  operator call while preserving the outer EXPR_TYPECAST node (mirroring the
  existing record->primitive typecast-operator path).

  This was the FPC pp.pas bootstrap blocker at heap.inc(552) "Overflow in
  arithmetic operation": `TConstExprInt(tpointerconstnode(left).value)` bitcast
  an 8-byte TConstPtrUInt into the 16-byte Tconstexprint variant record, giving
  a bogus overflow flag and value. }
{$mode objfpc}
type
  TR = record
    o: boolean;
    case s: boolean of
      false: (u: qword);
      true:  (v: int64);
  end;

operator := (const x: qword): TR;
begin
  result.o := false;
  result.s := false;
  result.u := x;
end;

var
  r: TR;
  q: qword;
begin
  q := 8;
  r := TR(q);   { explicit typecast must invoke operator :=, not bitcast }
  writeln('o=', r.o, ' s=', r.s, ' u=', r.u);
end.
