program StringToCharArray;

type
  TAlfa = array[1..20] of char;
  TShortString = array[1..5] of char;

var
  str1: TAlfa;
  str2: TShortString;

begin
  str1 := 'hello';
  writeln('The string is: ', str1);
  
  str2 := 'world';
  writeln('Short string: ', str2);
end.
