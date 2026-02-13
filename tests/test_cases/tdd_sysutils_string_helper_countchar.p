program tdd_sysutils_string_helper_countchar;
{$mode delphi}

uses SysUtils;

var
  s: string;
  chars: TCharArray;
  i: Integer;
  checksum: LongInt;
  reversed: string;
begin
  s := 'AabAaaBBA';
  writeln('Count a: ', s.CountChar('a'));
  writeln('Count A: ', s.CountChar('A'));
  writeln('IndexOfAny b/B: ', s.IndexOfAny(['b','B']));

  chars := s.ToCharArray;
  checksum := 0;
  for i := 0 to Length(chars) - 1 do
    checksum := checksum + Ord(chars[i]) * (i + 1);
  writeln('Checksum: ', checksum);

  reversed := '';
  for i := High(chars) downto 0 do
    reversed := reversed + chars[i];
  writeln('Reverse: ', reversed);

  s := '  spaced  ';
  writeln('Count space: ', s.CountChar(' '));
  writeln('Trimmed count space: ', s.Trim.CountChar(' '));
end.
