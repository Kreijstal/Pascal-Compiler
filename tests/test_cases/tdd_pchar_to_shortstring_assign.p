{$mode objfpc}
{ Regression: assignment of PChar to a ShortString variable must convert
  via strlen+memcpy (length byte + content), not store the raw 8-byte pointer
  value into the ShortString header.  Mirrors FPC Win RTL paramstr_li's
  `Result := argv[l]` lowering, which silently truncated argv strings to
  1 byte because the pointer's low byte was being mis-read as the length
  byte and the next 7 bytes of the pointer were copied as content. }
program tdd_pchar_to_shortstring_assign;
var
  buf: array[0..15] of Char;
  p: PChar;
  s: shortstring;
  i: longint;
begin
  buf[0] := 'h';
  buf[1] := 'i';
  buf[2] := '.';
  buf[3] := 'p';
  buf[4] := 'a';
  buf[5] := 's';
  buf[6] := #0;
  p := @buf[0];
  s := p;
  WriteLn('len=', Length(s));
  Write('chars=');
  for i := 1 to Length(s) do
    Write(Ord(s[i]), ' ');
  WriteLn;
end.
