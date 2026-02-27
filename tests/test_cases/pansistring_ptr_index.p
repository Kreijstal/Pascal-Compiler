program pansistring_ptr_index;

var
  s: AnsiString;
  p: PAnsiString;
  cptr: PChar;

begin
  s := 'abc';
  p := @s;
  cptr := @p^[1];
  writeln(cptr^);
end.
