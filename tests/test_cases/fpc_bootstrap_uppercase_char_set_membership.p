program fpc_bootstrap_uppercase_char_set_membership;

var
  C: Char;
  More: String;

begin
  C := 'T';
  if C in ['a', 'c', 'f', 'i', 'o', 'r', 't', 'u', 'w'] then
    Writeln('FAIL')
  else
    Writeln('OK');

  C := 't';
  if C in ['a', 'c', 'f', 'i', 'o', 'r', 't', 'u', 'w'] then
    Writeln('OK_t')
  else
    Writeln('FAIL_t');

  More := 'TP';
  if (More = '') or (More[1] in ['a', 'c', 'f', 'i', 'o', 'r', 't', 'u', 'w']) then
    Writeln('FAIL_more')
  else
    Writeln('OK_more');
end.
