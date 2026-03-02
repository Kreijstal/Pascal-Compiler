{$mode objfpc}
program tdd_widechar_is_char;

{ WideChar should be treated as a Char type, not as Word.
  Tests that WideChar variables can be assigned to Char,
  passed to overloaded functions, and compared. }

function UpCase(c: AnsiChar): AnsiChar; overload;
begin
  if (c >= 'a') and (c <= 'z') then
    Result := Chr(Ord(c) - 32)
  else
    Result := c;
end;

function UpCase(c: WideChar): WideChar; overload;
begin
  if (Ord(c) >= Ord('a')) and (Ord(c) <= Ord('z')) then
    Result := WideChar(Ord(c) - 32)
  else
    Result := c;
end;

var
  wc: WideChar;
  ac: AnsiChar;
  ch: Char;
begin
  wc := 'A';

  { WideChar should be assignable to Char }
  ch := wc;
  writeln(ch);

  { WideChar should work with UpCase overload }
  wc := 'b';
  wc := UpCase(wc);
  if wc = 'B' then
    writeln('PASS')
  else
    writeln('FAIL: UpCase did not work');

  { AnsiChar to Char assignment should still work }
  ac := 'X';
  ch := ac;
  writeln(ch);
end.
