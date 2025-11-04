program OrdEnumTest;

type
  TColor = (Red, Green, Blue, Yellow);
  TSize = (Small, Medium, Large);

var
  color: TColor;
  size: TSize;
  ordValue: integer;

begin
  { Test Ord with enum constants }
  writeln('Testing Ord with enum constants:');
  writeln('Ord(Red) = ', Ord(Red));
  writeln('Ord(Green) = ', Ord(Green));
  writeln('Ord(Blue) = ', Ord(Blue));
  writeln('Ord(Yellow) = ', Ord(Yellow));

  writeln('Ord(Small) = ', Ord(Small));
  writeln('Ord(Medium) = ', Ord(Medium));
  writeln('Ord(Large) = ', Ord(Large));

  { Test Ord with enum variables }
  color := Green;
  size := Large;

  writeln('Testing Ord with enum variables:');
  writeln('Ord(color) = ', Ord(color));
  writeln('Ord(size) = ', Ord(size));

  { Test Ord in expressions }
  ordValue := Ord(Blue) + Ord(Medium);
  writeln('Ord(Blue) + Ord(Medium) = ', ordValue);

  { Test Ord with different enum types }
  if Ord(Red) = 0 then
    writeln('Red is first enum value (ordinal 0)');

  if Ord(Large) = 2 then
    writeln('Large is third enum value (ordinal 2)');

  writeln('All Ord tests completed successfully!');
end.
