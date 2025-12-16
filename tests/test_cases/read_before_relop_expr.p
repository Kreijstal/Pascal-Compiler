{ Test for register leak bug with multiple read statements before relational expression }
var zahl1, zahl2, addU, dffU, prdktU, quoU : LongInt;
Begin 
  read(zahl1, zahl2);
  read(dffU);
  read(prdktU);
  read(quoU);
  if addU = zahl1 + zahl2 then 
    writeln('Equal')
  else
    writeln('Not equal');
end.
