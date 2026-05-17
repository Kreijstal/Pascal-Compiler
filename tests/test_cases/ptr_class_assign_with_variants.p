program ptr_class_assign_with_variants;

{ Regression for 15c0620c "semcheck: never route pointer-to-pointer
  assignments through op-overload search".

  Reduces FPC compiler/nset.pas makeifblock pattern
    newcheck : ^taddnode;
    newcheck := @check.right;     // right : tnode (base of taddnode)

  Before the fix, KGPC rejected (^taddnode vs ^tnode) in
  are_types_compatible_for_assignment, then fell through to
  semcheck_try_record_conversion_expression which gated the operator-
  overload search on target_is_pointer alone. Pointer-to-pointer
  assignments entered the search and bound `op_assign(terror)` whose
  NULL handler dereferenced and aborted compilation (this is why the
  original repro `uses variants` — it loaded olevariant's op_assign).

  The fix is purely about pointer-to-pointer typing rules: no
  `uses variants` needed to drive it. The test must compile, link, and
  run regardless of whether the stdlib or FPC RTL is in use. }

{$mode objfpc}

type
  TNode = class
    payload: longint;
  end;

  TBinopNode = class(TNode)
    left, right: TNode;
  end;

  TAddNode = class(TBinopNode)
  end;

procedure makeifblock;
var
  c: TAddNode;
  np: ^TAddNode;
begin
  c := TAddNode.create;
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
