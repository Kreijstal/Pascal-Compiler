{$mode objfpc}
program tdd_builtin_type_word_expr;

var
  w: Word;
  b: Byte;
  lw: LongWord;
  total: LongInt;

begin
  w := Word($DBFF);
  b := Byte($7F);
  lw := LongWord($DEADBEEF);

  if w > $DB00 then
    WriteLn('w_high=true')
  else
    WriteLn('w_high=false');

  if b < $80 then
    WriteLn('b_low=true')
  else
    WriteLn('b_low=false');

  total := w + b + LongInt(lw and $FFFF);
  WriteLn('w=', w);
  WriteLn('b=', b);
  WriteLn('total=', total);
end.
