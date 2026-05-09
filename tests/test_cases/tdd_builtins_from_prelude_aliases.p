program tdd_builtins_from_prelude_aliases;

function FirstChar(const s: OpenString): AnsiChar;
begin
  if Length(s) > 0 then
    FirstChar := s[1]
  else
    FirstChar := #0;
end;

var
  c: AnsiChar;
  p: PAnsiChar;
  b8: Boolean8;
  b16: Boolean16;
  b32: Boolean32;
  b64: Boolean64;
begin
  c := FirstChar('abc');
  p := @c;
  b8 := True;
  b16 := True;
  b32 := False;
  b64 := True;

  if (p^ = 'a') and b8 and b16 and (not b32) and b64 then
    writeln('ok')
  else
    writeln('bad');
end.
