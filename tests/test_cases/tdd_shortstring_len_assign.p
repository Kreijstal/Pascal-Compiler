program tdd_shortstring_len_assign;

{$mode objfpc}

var
  res: shortstring;
  size: byte;

begin
  res := 'abc';
  size := 3;
  byte(res[0]) := byte(size);
  writeln(ord(res[0]));
end.
