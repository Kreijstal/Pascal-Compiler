program tdd_case_multistmt_branch;

var
  x, y: Integer;

begin
  x := 2;
  y := 0;
  case x of
    1: y := 1;
  else
    y := 2;
    y := y + 3;
  end;
  writeln(y);
end.
