program nested_same_name(output);

{ Test that nested procedures with the same name in different parent
  procedures generate unique assembly labels }

procedure Outer1;
  procedure Helper;
  begin
    WriteLn('Helper in Outer1')
  end;
begin
  Helper
end;

procedure Outer2;
  procedure Helper;
  begin
    WriteLn('Helper in Outer2')
  end;
begin
  Helper
end;

function Compute1(x: integer): integer;
  function Inner(n: integer): integer;
  begin
    Inner := n * 2
  end;
begin
  Compute1 := Inner(x)
end;

function Compute2(x: integer): integer;
  function Inner(n: integer): integer;
  begin
    Inner := n + 10
  end;
begin
  Compute2 := Inner(x)
end;

begin
  Outer1;
  Outer2;
  WriteLn(Compute1(5));
  WriteLn(Compute2(5))
end.
