{ Test for register leak bug with multiple read statements before relational expression }
{ This test validates that read() doesn't leak registers, allowing subsequent expression
  evaluation to succeed. Many read calls before an if statement with addition would crash. }
var zahl1, zahl2, sum, dffU, prdktU, quoU : LongInt;
Begin 
  read(zahl1, zahl2);
  read(dffU);
  read(prdktU);
  read(quoU);
  sum := zahl1 + zahl2;
  if sum = 8 then 
    writeln('Sum is 8')
  else
    writeln('Sum is not 8');
end.
