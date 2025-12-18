program keyboard_arrow;

uses crt;

var
  i: integer;
  c: char;

begin
  clrscr;
  for i := 1 to 4 do
  begin
    c := readkey;
    writeln(ord(c));
  end;
end.
