{ Test short-circuit AND/OR with single-char string literal comparisons.
  Regression test: single-char EXPR_STRING like '.' was passed as raw
  integer to kgpc_string_compare when inside AND/OR short-circuit
  evaluation, causing SIGSEGV. }
program shortcircuit_string_char_cmp;
var
  s: string;
begin
  s := '.';
  if (s <> 'x') and (s <> 'y') then
    writeln('pass1')
  else
    writeln('FAIL1');

  s := 'hello';
  if (s <> '.') and (s <> '..') then
    writeln('pass2')
  else
    writeln('FAIL2');

  s := '..';
  if (s = '.') or (s = '..') then
    writeln('pass3')
  else
    writeln('FAIL3');

  s := 'a';
  if (s <> 'b') and (s <> 'c') and (s <> 'd') then
    writeln('pass4')
  else
    writeln('FAIL4');
end.
