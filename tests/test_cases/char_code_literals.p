program char_code_literals;

var
  ch: char;

begin
  ch := #9;
  writeln('Tab character assigned');

  ch := #10;
  writeln('Line feed character assigned');

  ch := #13;
  writeln('Carriage return character assigned');

  writeln('All character codes assigned successfully');
end.
