{ Test ShortString type - Pascal string with length byte at index 0 }
{ NOTE: This test is automatically SKIPPED by the test runner because }
{ KGPC doesn't yet support the ShortString type. }
{ It serves as documentation of the gap and a target for future implementation. }
{ See docs/FPC_BOOTSTRAP_GAPS.md for details. }
program TestShortString;

var
  s: ShortString;
  i: Integer;

begin
  { Assign string literal }
  s := 'Hello';
  writeln(s);
  
  { Access length byte - position 0 stores the length }
  writeln(ord(s[0]));
  
  { Access individual characters - 1-indexed }
  for i := 1 to ord(s[0]) do
    writeln(s[i]);
end.
