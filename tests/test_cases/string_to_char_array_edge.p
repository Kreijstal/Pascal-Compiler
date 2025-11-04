program StringToCharArrayEdgeCases;

type
  TExact = array[1..5] of char;  { Exactly 5 chars }
  TLarge = array[1..100] of char; { Much larger than any test string }
  
var
  exact: TExact;
  large: TLarge;
  
begin
  { Test exact fit }
  exact := 'hello';
  writeln('Exact fit: ', exact);
  
  { Test short string in large array }
  large := 'hi';
  writeln('Short in large: ', large);
  
  { Test empty string }
  large := '';
  writeln('Empty: <', large, '>');
  
  { Test longer string in large array }
  large := 'This is a longer test string';
  writeln('Longer: ', large);
end.
