program CaseCompound;
var
  x: integer;
begin
  x := 2;
  case x of
    1: writeln(100);
    2: begin
         writeln(201);
         writeln(202)
       end;
    3: writeln(300)
  end;
end.
