program missing_pos_char;
var
  s: string;
  p: integer;
begin
  s := 'hello world';
  p := Pos(' ', s);
  writeln('Pos: ', p);
end.
