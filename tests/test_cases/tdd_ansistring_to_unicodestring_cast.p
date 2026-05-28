{$mode objfpc}
{ Regression: `UnicodeString(AnsiStringExpr)` typecast must encode the
  ANSI bytes as UTF-16 widechars, not pass the raw AnsiString pointer
  through unchanged.  Mirrors FPC RTL `FileExists(RawByteString)` which
  reduces to `FileExists(UnicodeString(FileName), FollowLink)`; without
  the conversion, `GetFileAttributesW(PWideChar(FileName))` reads each
  pair of ANSI bytes as one widechar (e.g. "hi.pas" → 0x6968, 0x702e,
  0x7361) so pp_win.exe under wine cannot locate "hi.pas" on disk. }
program tdd_ansistring_to_unicodestring_cast;
var
  s: AnsiString;
  ws: UnicodeString;
  i: longint;
begin
  s := 'hi.pas';
  ws := UnicodeString(s);
  WriteLn('len=', Length(ws));
  Write('words=');
  for i := 1 to Length(ws) do
    Write(Ord(ws[i]), ' ');
  WriteLn;
end.
