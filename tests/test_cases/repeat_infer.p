program repeat_infer;
var
  counter := 0;
begin
  repeat
    counter := counter + 1;
  until counter = 5;
  writeln(counter);
end.
