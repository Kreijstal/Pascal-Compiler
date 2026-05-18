program tdd_nested_outer_varwrite;
{$mode objfpc}

{ Regression test: nested proc writing to outer function's var parameter.
  Bug: codegen used %rbp (nested frame) instead of static link to load
  the var-param pointer slot, writing to wrong address. }

procedure outer_set_42(var x: integer);
  procedure inner_set;
  begin
    x := 42;
  end;
begin
  inner_set;
end;

procedure outer_add(var x: integer; y: integer);
  procedure inner_add;
  begin
    x := x + y;
  end;
begin
  inner_add;
end;

var
  a, b: integer;
begin
  a := 0;
  outer_set_42(a);
  b := 10;
  outer_add(b, 5);
  writeln(a, ' ', b);
end.
