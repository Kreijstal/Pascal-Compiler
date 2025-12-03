{ Test that integer division with / operator produces Real result }
program integer_to_real_division;
var
  x: Integer;
  y: Real;
begin
  { Basic integer / integer = real }
  x := 100;
  y := x / 2;
  writeln('100/2=', y:0:2);
  
  { Literal division }
  y := 7 / 4;
  writeln('7/4=', y:0:2);
  
  { More complex expression }
  x := 10;
  y := (x * 2 + 5) / 3;
  writeln('(10*2+5)/3=', y:0:4);
  
  { Chain of operations }
  y := ((100 - 50) * 2) / 4;
  writeln('((100-50)*2)/4=', y:0:2);
  
  { Division by variable }
  x := 5;
  y := 100 / x;
  writeln('100/5=', y:0:2);
end.
