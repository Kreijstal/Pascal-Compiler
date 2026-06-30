{ Regression test: pointer-dereference assignment whose RHS is a function/
  constructor call.

  The general POINTER_DEREF branch of codegen_var_assignment evaluated the RHS
  with codegen_expr() (which discards/frees the result register) and then
  grabbed a fresh get_free_reg(), assuming the just-freed result register would
  be handed back.  That holds only by coincidence: under register pressure
  get_free_reg() can return a DIFFERENT free register still holding a stale
  value.  In FPC's optcse.pas searchcsedomain
  (pnode(lists.locationlist[i])^ := cderefnode.Create(ctemprefnode.create(...)))
  that stale value was the constructor receiver's class reference (a read-only
  VMT pointer); storing it into the node slot crashed pp_bootstrap at the next
  field write.  The fix captures the RHS result register explicitly via
  codegen_expr_with_result().

  Expected result: v = 13 + 1000 = 1013.
  inner.Create(1+2+3+4=10).v = 10; index = 10 + 1*2 - 3 + 4 = 13;
  TDerived.Create(13) sets v := 13 + 1000. }
program pointer_deref_assign_funccall_result;
{$mode objfpc}
type
  TBase = class
    v: longint;
    constructor Create(a: longint); virtual;
  end;
  TBaseClass = class of TBase;
  TDerived = class(TBase)
    constructor Create(a: longint); override;
  end;
  PBase = ^TBase;
var
  cls: TBaseClass;
  inner: TBaseClass;
  slots: array[0..3] of TBase;
  i, a, b, c, d: longint;
  p: PBase;
constructor TBase.Create(a: longint); begin v := a; end;
constructor TDerived.Create(a: longint); begin inherited Create(a); v := a + 1000; end;
begin
  cls := TDerived;
  inner := TBase;
  for i := 0 to 3 do slots[i] := nil;
  a := 1; b := 2; c := 3; d := 4;
  i := 2;
  p := @slots[i];
  p^ := cls.Create(inner.Create(a + b + c + d).v + a*b - c + d);
  writeln('v=', slots[2].v);
end.
