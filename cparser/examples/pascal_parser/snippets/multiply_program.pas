program multiply_test;

var a,b:integer;

function multiply(x,y:integer):integer;
begin;
  multiply := x*y;
end;

begin;
  writeln('Enter the numbers you want to multiply.');
  readln(a,b);
  writeln(a,' x ',b,' = ',multiply(a,b));
  readln;
end.
