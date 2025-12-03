Program Geburtsjahr;
Var
gj2:Integer;
diesesJahr:Integer=2018;
bla:Real;
begin
writeln('Gibt es deine gebuhrtsjahr');
read(gj2);
bla:=(((gj2*2)+5)*50+(diesesJahr-gj2)-250)/100;
writeln(bla:0:2);
end.