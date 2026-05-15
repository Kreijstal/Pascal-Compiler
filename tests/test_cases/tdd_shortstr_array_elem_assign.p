{ Regression: assigning a string[N] element (is_big, CHAR_TYPE tag) to a string[M]
  variable must use shortstring copy, not char-to-SS path. }
program test;
type
  TName = string[16];
  TNames = array[0..2] of TName;
const
  names: TNames = ('hello', 'world', 'foo');
var
  s: string[127];
  i: integer;
begin
  i := 1;
  s := names[i];
  if s = 'world' then
    writeln('OK')
  else
    writeln('FAIL: got ', s);
end.
