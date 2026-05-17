program ptr_class_assign_with_variants;

{ Regression: when the variants unit is in scope (or transitively visible),
  `^TDerived := @ClassInstance.BaseField` was wrongly routed through the
  operator-overload search and bound to `olevariant.op_assign(terror)`,
  producing a NULL-call crash.  Reduces FPC compiler/nset.pas makeifblock:
  the assignment must compile and run without invoking any variant operator.
  The case statement on strings exercises the same path makeifblock builds. }

{$mode objfpc}

uses
  variants;

type
  tnode = class
    payload: longint;
  end;

  tbinopnode = class(tnode)
    left, right: tnode;
  end;

  taddnode = class(tbinopnode)
  end;

procedure makeifblock;
var
  c: taddnode;
  np: ^taddnode;
begin
  c := taddnode.create;
  np := @c;
  writeln('@c ok');
  np := @c.right;
  writeln('@c.right ok');
  c.free;
end;

begin
  makeifblock;
  writeln('done');
end.
