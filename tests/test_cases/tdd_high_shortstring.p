program tdd_high_shortstring;
{ High()/Low() of a ShortString are compile-time constants of the declared
  type: High = capacity (255 for plain ShortString, N for string[N]), Low = 0.
  Regression: semcheck used to fold High(ShortString) to Length(s), so the
  common RTL idiom `if length(s) < high(s) then {append}` (text.inc NextChar)
  never appended — breaking Read(text, integer/real). }
var
  s : shortstring;
  t : string[20];
  i : integer;
begin
  writeln(high(s));
  writeln(low(s));
  writeln(high(t));
  s := '';
  for i := 1 to 3 do
    if length(s) < high(s) then
    begin
      SetLength(s, length(s) + 1);
      s[length(s)] := chr(ord('A') + i - 1);
    end;
  writeln('s=', s);
end.
