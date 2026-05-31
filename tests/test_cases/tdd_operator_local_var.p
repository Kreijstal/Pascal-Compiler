program tdd_operator_local_var;
{ Regression: local variables in a standalone operator overload were dropped
  during AST conversion.  The grammar parses an operator body with `method_body`
  (a PASCAL_T_NONE sequence) that splices its local var/const/type sections in
  as *siblings* of the begin-block rather than wrapping them in a
  PASCAL_T_FUNCTION_BODY.  The operator-conversion path only grabbed the
  begin-block and skipped those sibling sections, so locals never reached
  codegen and surfaced as "Unresolved non-local symbol", producing a broken
  operator body.

  This was the root cause of FPC pp.pas bootstrap failing at heap.inc(552)
  "Overflow in arithmetic operation": Tconstexprint's `+` operator has a local
  `aneg`, so the KGPC-built compiler returned garbage from it, overflowing the
  @FixedArena(nil^).field offset constant. }
{$mode objfpc}
type
  TNum = record v: int64; end;

operator + (const a, b: TNum): TNum;
var
  neg: boolean;
begin
  neg := a.v < 0;          { local var — must resolve as a real local }
  if neg then
    result.v := b.v - a.v
  else
    result.v := a.v + b.v;
end;

var
  x, y, z: TNum;
begin
  x.v := 8;
  y.v := 4;
  z := x + y;
  writeln(z.v);
end.
