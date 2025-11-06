program set_symmetric_diff;
type
  TDigit = set of 0..9;
var
  s1, s2, result: TDigit;
begin
  s1 := [1, 2, 3, 4];
  s2 := [3, 4, 5, 6];
  result := s1 >< s2;
  
  if 1 in result then writeln('has-1');
  if 2 in result then writeln('has-2');
  if 3 in result then writeln('has-3');
  if 4 in result then writeln('has-4');
  if 5 in result then writeln('has-5');
  if 6 in result then writeln('has-6');
end.
