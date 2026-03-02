{$mode objfpc}
program tdd_codegen_char_shortstring_promotions;

type
  TShort = string[255];

var
  ss: TShort;
  s: string;
  c: char;
begin
  ss := 'hello';
  s := 'hello';
  if ss = s then
    WriteLn('shortstring-compare-ok')
  else
    WriteLn('shortstring-compare-fail');

  c := 'x';
  s := c;
  if s = 'x' then
    WriteLn('char-assign-ok')
  else
    WriteLn('char-assign-fail');

end.
