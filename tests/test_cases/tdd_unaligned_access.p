program tdd_unaligned_access;

var
  value: longword;
  p: ^longword;

begin
  value := 0;
  p := @value;
  unaligned(p^) := $DEADBEEF;
  if unaligned(p^) = $DEADBEEF then
    writeln('ok')
  else
    writeln('bad');
end.
