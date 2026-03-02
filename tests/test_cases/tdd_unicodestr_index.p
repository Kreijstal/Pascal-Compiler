{$mode objfpc}
program tdd_unicodestr_index;

{ Test that indexing a UnicodeString returns a character type (WideChar),
  not Word. This was broken because semcheck_arrayaccess() hardcoded
  WORD_TYPE for UnicodeString elements instead of CHAR_TYPE. }

var
  w: UnicodeString;
  wc: WideChar;
begin
  w := 'Hello';
  wc := w[1];
  if wc = 'H' then
    writeln('PASS')
  else
    writeln('FAIL');
end.
