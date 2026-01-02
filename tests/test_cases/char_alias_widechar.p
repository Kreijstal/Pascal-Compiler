program char_alias_widechar;

type
  Char = WideChar;

var
  c: Char;

begin
  c := 'a';
  if c >= 'a' then
    WriteLn('ok')
  else
    WriteLn('bad');
end.
