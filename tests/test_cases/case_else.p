program CaseElseTest;
var
  x: integer;
begin
  x := 5;
  case x of
    1: writeln(1);
    2: writeln(2);
    3: writeln(3)
  else
    writeln(999)
  end;
end.
