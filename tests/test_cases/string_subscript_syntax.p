program StringSubscriptTest;
{*
  Test support for string[N] type syntax (shortstring).
  Pascal supports fixed-length strings declared as string[size].
  This tests that type aliases and variable declarations work with this syntax.
*}
type
  TShortString = string[20];
  TName = string[50];
var
  name1: string[10];
  name2: TShortString;
  name3: TName;
begin
  name1 := 'Alice';
  name2 := 'Bob';
  name3 := 'Charlie';
  
  writeln('name1 = ', name1);
  writeln('name2 = ', name2);
  writeln('name3 = ', name3);
  writeln('PASS');
end.
