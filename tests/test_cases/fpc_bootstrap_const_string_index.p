program fpc_bootstrap_const_string_index;

function FirstIsStar(const Pattern, Name: string): Boolean;
begin
  FirstIsStar := (Length(Pattern) = 1) and (Pattern[1] = '*') and
                 (Name[1] = 's');
end;

begin
  if FirstIsStar('*', 'symbian') then
    Writeln('OK')
  else
    Writeln('FAIL');
end.
