{ Test for the bug where procedure call in THEN followed by assignment in ELSE
  was incorrectly parsed. The peek_assignment_operator function was scanning
  past the 'else' keyword and finding ':=' in the else clause, causing the
  parser to incorrectly try to parse the procedure call as an assignment. }
program if_else_assignment_bug;

var
  d, i: Integer;

begin
  i := 5;
  
  { This pattern was failing: procedure call in THEN, assignment in ELSE }
  if i < 0 then
    writeln('Negative')
  else
    d := 1;
  
  { Also test with begin-end blocks }
  if i < 0 then
    writeln('Still negative')
  else begin
    d := 2;
    writeln('In else block');
  end;
  
  { Test the complex fakultat-like pattern }
  if i < 0 then
    writeln('Error')
  else begin
    d := 1;
    if d < 0 then begin
      writeln('Overflow');
    end else
      writeln('Result: ', d);
  end;
  
  writeln('Final d: ', d);
end.
