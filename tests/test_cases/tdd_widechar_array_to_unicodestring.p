{$mode objfpc}
program tdd_widechar_array_to_unicodestring;
{ Regression: a fixed `array[..] of WideChar` assigned wholesale to a
  UnicodeString must copy the packed UTF-16 prefix up to the NUL terminator,
  not dispatch to the ShortString/AnsiString path.  Mirrors FPC's Win RTL
  FindMatch doing `Name := f.FindData.cFileName` (cFileName is
  array[0..MAX_PATH-1] of WideChar): the misdispatch read the first low byte
  ('i'=$69) as a ShortString length (105) or byte-copied the array as ANSI and
  stopped at the first embedded NUL, truncating every enumerated filename to a
  single character.  That broke the compiler's TDirectoryCache so no include
  directory (and thus systemh.inc) could be found on Win64. }
var
  small: array[0..5] of WideChar;
  big: array[0..259] of WideChar;
  u: UnicodeString;
  s: ansistring;
  ok: boolean;
begin
  ok := true;

  { small array — exercises the shortstring-sized path }
  small[0] := 'i'; small[1] := 'n'; small[2] := 'c'; small[3] := #0;
  u := small;
  if Length(u) <> 3 then ok := false;
  s := u;
  if s <> 'inc' then ok := false;

  { MAX_PATH-sized array — exercises the char-array path (cFileName layout) }
  big[0] := 'w'; big[1] := 'i'; big[2] := 'n'; big[3] := '6'; big[4] := '4';
  big[5] := #0;
  u := big;
  if Length(u) <> 5 then ok := false;
  s := u;
  if s <> 'win64' then ok := false;

  if ok then WriteLn('OK')
  else WriteLn('FAIL');
end.
