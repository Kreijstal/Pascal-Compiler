{ Passing a computed char-set expression (union/intersection/difference) by
  value to a `set of char` parameter. Regression for "Unsupported expression
  type for set parameter" when KGPC compiled the FPC optimizer. }
program tdd_computed_charset_value_param;
type TCharSet = set of char;
function CountIn(s: TCharSet): Integer;
var c: char; n: Integer;
begin
  n := 0;
  for c := 'a' to 'z' do if c in s then Inc(n);
  for c := '0' to '9' do if c in s then Inc(n);
  CountIn := n;
end;
var s1, s2: TCharSet;
begin
  s1 := ['a'..'f'];   { 6 }
  s2 := ['0'..'9'];   { 10 }
  writeln(CountIn(s1 + s2));   { union = 16 }
  writeln(CountIn(s1 * ['c'..'z']));  { intersect a..f with c..z = c,d,e,f = 4 }
  writeln(CountIn(s1 - ['a'..'c'])); { diff = d,e,f = 3 }
end.
