program CaseComprehensive;
var
  x: integer;
begin
  { Test 1: match first case }
  x := 1;
  case x of
    1: writeln(100);
    2: writeln(200);
    3: writeln(300)
  end;
  
  { Test 2: match middle case }
  x := 2;
  case x of
    1: writeln(100);
    2: writeln(200);
    3: writeln(300)
  end;
  
  { Test 3: match last case }
  x := 3;
  case x of
    1: writeln(100);
    2: writeln(200);
    3: writeln(300)
  end;
  
  { Test 4: no match, with else }
  x := 5;
  case x of
    1: writeln(100);
    2: writeln(200);
    3: writeln(300)
  else
    writeln(999)
  end;
  
  { Test 5: match with compound statement }
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
