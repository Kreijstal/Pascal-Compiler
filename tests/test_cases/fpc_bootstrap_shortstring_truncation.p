program fpc_bootstrap_shortstring_truncation;
{$mode objfpc}
{ Bootstrap blocker: ShortString truncation semantics (used by system line endings, etc.) }

var
  s: ShortString;
  long_str: AnsiString;
  i: Integer;

begin
  long_str := '';
  for i := 1 to 300 do
    long_str := long_str + 'A';
  s := long_str;
  writeln(Length(s));
  writeln(s[1], s[Length(s)]);
end.
